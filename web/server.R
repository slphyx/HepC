#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(Rcpp)
library(deSolve)
library(reshape2)
library(ggplot2)
library(tidyverse)
library(xlsx)
library(shinyjs)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {

  v <- reactiveValues(doPlot = FALSE)
  options(scipen=6)
  setwd("C:/hepc/web")
  sourceCpp('p1.cpp')
  
  observe({
    toggle(id = "A_G", condition = input$Anin_genotype)
  })
  
  observe({
    toggle(id = "A_nonG", condition = !input$Anin_genotype)
  })
  
  observeEvent(input$Treatment, {
    v$doPlot <- FALSE
  })  
  

  
  observeEvent(input$button, {
    if( v$doPlot == TRUE) v$doPlot <- FALSE
    v$doPlot <- TRUE
    
  })
  parms <- reactive({
    list(
      P0=input$P0,       #popstat(YEAR=1999)
      K= input$K,        #Maximum population (carrying capacity)
      r =input$r,        #Population growth rate (logistic growth curve)
      S0=0.0305853,
      standard_start = 2004,
      new_start = 2015,
      
      
      #cost = treated1+treated2
      total_HCC=0,
      total_HCV=631000,
      
      FI= input$Fi,     #Influx rate of the population to become susceptible per year
      
      #Progression of fibrosis
      f0f1=0.117,       #Fibrosis stage F0 to F1
      f1f2=0.085,       #Fibrosis stage F1 to F2
      f2f3=0.12,        #Fibrosis stage F2 to F3
      f3c1=0.116,       #Fibrosis stage F3 to C1
      
      #Progression of cirrhosis
      c1c2=0.044,       #Fibrosis stage C1 to C2
      c2c3=0.044,       #Fibrosis stage C2 to C3
      c3c4=0.076,       #Fibrosis stage C3 to C4
      
      #Incidence of developing HCC
      c1bA=0.0068,      #Fibrosis stage C1 to bA
      c1bB=0.0099,      #Fibrosis stage C1 to bB
      c1bC=0.0029,      #Fibrosis stage C1 to bC
      c1bD=0.0068,      #Fibrosis stage C1 to bD
      
      c2bA=0.0068,      #Fibrosis stage C2 to bA
      c2bB=0.0099,      #Fibrosis stage C2 to bB
      c2bC=0.0029,      #Fibrosis stage C2 to bC
      c2bD=0.0068,      #Fibrosis stage C2 to bD
      
      c3bD=0.0664,      #Fibrosis stage C3 to bD
      c4bD=0.0664,      #Fibrosis stage C4 to bD
      
      #Death rate from cirrhosis and HCC
      deathc1=0.01,         #Death rate for Cirrhosis Child-Pugh class A
      deathc2=0.01,         #Death rate for Cirrhosis Child-Pugh class B
      deathc3=0.2,          #Death rate for Cirrhosis Child-Pugh class C
      deathc4=0.57,         #Death rate for Cirrhosis Child-Pugh class D
      
      deathbA=1/(36/12),    #Death rate for HCC_BCLC_A
      deathbB=1/(16/12),    #Death rate for HCC_BCLC_B
      deathbC=1/(6/12),     #Death rate for HCC_BCLC_C
      deathbD=1/(3/12),     #Death rate for HCC_BCLC_D
      
      deathtrn=1/(240/12),
      
      #Transplantation
      tranc4=0.0015, #Transplantation rate in cirrhosis stage C4
      tranbA=0.0015, #Transplantation rate in HCC_BCLC_A
      tranbB=0.0015, #Transplantation rate in HCC_BCLC_B
      #;newcase=17000,
      cover = 5,
      #Natural rate of death
      natdeath=0.0424, #Unrelated to cirrhosis and hepatitis C
      
      beta= 0.327,              #Transmission coefficient
      #Treatment efficacy
      #Standard treatment response based on genotype (weighted average)
      std_cureF0=0.7,
      std_cureF1 =0.7,
      std_cureF2 =0.7,
      std_cureF3 =0.7,
      std_cureC1 =0.7,
      std_cureC2=0.7,
      #Novel treatment response based on genotype (weighted average)
      new_cureF0=0.8,
      new_cureF1=0.8,
      new_cureF2 =0.8,
      new_cureF3 =0.8,
      new_cureC1 =0.8,
      new_cureC2 =0.8,
      new_cureC3 =0.8,
      new_cureC4 =0.8,
      
      std_dist = c(0.05,0.05,0.3,0.3,0.3)
    )
  })
  

    S <-reactive({
      parms()$S0 * parms()$P0
    })

  
  inits <- reactive({ 
      c(
      S = S(), #;0.01*#pop_since1960(TIME=1)
      F0=0.2825*S(),#;genotype1
      F1=0.2825*S(),
      F2=0.184*S(),
      F3=0.124*S(),
      C1=0.03175*S(),# ;CirA
      C2=0.03175*S(),#; CirA
      C3=0.03175*S(),#; CirB
      C4=0.03175*S(),#;CirC
      
      HCC_A=0,
      HCC_B=0,
      HCC_C=0,
      HCC_D=0,
      
      C1std_cured=0,
      C1new_cured=0,
      C2new_cured=0,
      C3new_cured=0,
      C4new_cured=0,
      
      death = 0,
      deathHCC = 0,
      deathC14 = 0
      
    )
  })
  
  table_parmeter <- eventReactive(input$button,{
    
    #Initial status of model
    
    data_p <- cbind(as.data.frame(parms()),as.data.frame(rbind(inits() )))
    data_p
  })
  
  out_df <- reactive({
  
    times <- seq(1999, 2020, by = 0.01)
    
    out <- ode( y = inits(),times =  times, func = PanHepC, parms = parms(), method = "rk4")

    out_df <- as.data.frame(out)
    colnames(out_df)[23:29] <- c("prev","incHCC","pop","infect","total_HCV","total_HCC","New death")
    out_df

  })
  
  out_df_g1 <- reactive({
    
    times <- seq(1999, 2020, by = 0.01)
    parms <- 
      list(
        P0=input$P0,       #popstat(YEAR=1999)
        K= input$K,        #Maximum population (carrying capacity)
        r =input$r,        #Population growth rate (logistic growth curve)
        S0=0.0305853,
        standard_start = 2004,
        new_start = 2015,
        
        
        #cost = treated1+treated2
        total_HCC=0,
        total_HCV=631000,
        
        FI= input$Fi,     #Influx rate of the population to become susceptible per year
        
        #Progression of fibrosis
        f0f1=0.117,       #Fibrosis stage F0 to F1
        f1f2=0.085,       #Fibrosis stage F1 to F2
        f2f3=0.12,        #Fibrosis stage F2 to F3
        f3c1=0.116,       #Fibrosis stage F3 to C1
        
        #Progression of cirrhosis
        c1c2=0.044,       #Fibrosis stage C1 to C2
        c2c3=0.044,       #Fibrosis stage C2 to C3
        c3c4=0.076,       #Fibrosis stage C3 to C4
        
        #Incidence of developing HCC
        c1bA=0.0068,      #Fibrosis stage C1 to bA
        c1bB=0.0099,      #Fibrosis stage C1 to bB
        c1bC=0.0029,      #Fibrosis stage C1 to bC
        c1bD=0.0068,      #Fibrosis stage C1 to bD
        
        c2bA=0.0068,      #Fibrosis stage C2 to bA
        c2bB=0.0099,      #Fibrosis stage C2 to bB
        c2bC=0.0029,      #Fibrosis stage C2 to bC
        c2bD=0.0068,      #Fibrosis stage C2 to bD
        
        c3bD=0.0664,      #Fibrosis stage C3 to bD
        c4bD=0.0664,      #Fibrosis stage C4 to bD
        
        #Death rate from cirrhosis and HCC
        deathc1=0.01,         #Death rate for Cirrhosis Child-Pugh class A
        deathc2=0.01,         #Death rate for Cirrhosis Child-Pugh class B
        deathc3=0.2,          #Death rate for Cirrhosis Child-Pugh class C
        deathc4=0.57,         #Death rate for Cirrhosis Child-Pugh class D
        
        deathbA=1/(36/12),    #Death rate for HCC_BCLC_A
        deathbB=1/(16/12),    #Death rate for HCC_BCLC_B
        deathbC=1/(6/12),     #Death rate for HCC_BCLC_C
        deathbD=1/(3/12),     #Death rate for HCC_BCLC_D
        
        deathtrn=1/(240/12),
        
        #Transplantation
        tranc4=0.0015, #Transplantation rate in cirrhosis stage C4
        tranbA=0.0015, #Transplantation rate in HCC_BCLC_A
        tranbB=0.0015, #Transplantation rate in HCC_BCLC_B
        #;newcase=17000,
        cover = 5,
        #Natural rate of death
        natdeath=0.0424, #Unrelated to cirrhosis and hepatitis C
        
        beta= 0.327,              #Transmission coefficient
        #Treatment efficacy
        #Standard treatment response based on genotype (weighted average)
        std_cureF0=0.7,
        std_cureF1 =0.7,
        std_cureF2 =0.7,
        std_cureF3 =0.7,
        std_cureC1 =0.7,
        std_cureC2=0.7,
        #Novel treatment response based on genotype (weighted average)
        new_cureF0=0,
        new_cureF1=0,
        new_cureF2 =0,
        new_cureF3 =0,
        new_cureC1 =0,
        new_cureC2 =0,
        new_cureC3 =0,
        new_cureC4 =0,
        
        std_dist = c(0.05,0.05,0.3,0.3,0.3)
      )
    
    out <- ode( y = inits(),times =  times, func = PanHepC, parms = parms, method = "rk4")
    
    out_df <- as.data.frame(out)
    colnames(out_df)[23:29] <- c("prev_G1","incHCC_G1","pop_G1","infect_G1","total_HCV_G1","total_HCC_G1","New death_G1")
    colnames(out_df)[15:19] <- c("C1std_cured_G1","C1new_cured_G1","C2new_cured_G1","C3new_cured_G1","C4new_cured_G1")
    out_df
    
  })
  
  out_df_g2 <- reactive({
    
    times <- seq(1999, 2020, by = 0.01)
    parms <- 
      list(
        P0=input$P0,       #popstat(YEAR=1999)
        K= input$K,        #Maximum population (carrying capacity)
        r =input$r,        #Population growth rate (logistic growth curve)
        S0=0.0305853,
        standard_start = 2004,
        new_start = 2015,
        
        
        #cost = treated1+treated2
        total_HCC=0,
        total_HCV=631000,
        
        FI= input$Fi,     #Influx rate of the population to become susceptible per year
        
        #Progression of fibrosis
        f0f1=0.117,       #Fibrosis stage F0 to F1
        f1f2=0.085,       #Fibrosis stage F1 to F2
        f2f3=0.12,        #Fibrosis stage F2 to F3
        f3c1=0.116,       #Fibrosis stage F3 to C1
        
        #Progression of cirrhosis
        c1c2=0.044,       #Fibrosis stage C1 to C2
        c2c3=0.044,       #Fibrosis stage C2 to C3
        c3c4=0.076,       #Fibrosis stage C3 to C4
        
        #Incidence of developing HCC
        c1bA=0.0068,      #Fibrosis stage C1 to bA
        c1bB=0.0099,      #Fibrosis stage C1 to bB
        c1bC=0.0029,      #Fibrosis stage C1 to bC
        c1bD=0.0068,      #Fibrosis stage C1 to bD
        
        c2bA=0.0068,      #Fibrosis stage C2 to bA
        c2bB=0.0099,      #Fibrosis stage C2 to bB
        c2bC=0.0029,      #Fibrosis stage C2 to bC
        c2bD=0.0068,      #Fibrosis stage C2 to bD
        
        c3bD=0.0664,      #Fibrosis stage C3 to bD
        c4bD=0.0664,      #Fibrosis stage C4 to bD
        
        #Death rate from cirrhosis and HCC
        deathc1=0.01,         #Death rate for Cirrhosis Child-Pugh class A
        deathc2=0.01,         #Death rate for Cirrhosis Child-Pugh class B
        deathc3=0.2,          #Death rate for Cirrhosis Child-Pugh class C
        deathc4=0.57,         #Death rate for Cirrhosis Child-Pugh class D
        
        deathbA=1/(36/12),    #Death rate for HCC_BCLC_A
        deathbB=1/(16/12),    #Death rate for HCC_BCLC_B
        deathbC=1/(6/12),     #Death rate for HCC_BCLC_C
        deathbD=1/(3/12),     #Death rate for HCC_BCLC_D
        
        deathtrn=1/(240/12),
        
        #Transplantation
        tranc4=0.0015, #Transplantation rate in cirrhosis stage C4
        tranbA=0.0015, #Transplantation rate in HCC_BCLC_A
        tranbB=0.0015, #Transplantation rate in HCC_BCLC_B
        #;newcase=17000,
        cover = 5,
        #Natural rate of death
        natdeath=0.0424, #Unrelated to cirrhosis and hepatitis C
        
        beta= 0.327,              #Transmission coefficient
        #Treatment efficacy
        #Standard treatment response based on genotype (weighted average)
        std_cureF0=0.7,
        std_cureF1 =0.7,
        std_cureF2 =0.7,
        std_cureF3 =0.7,
        std_cureC1 =0.7,
        std_cureC2=0.7,
        #Novel treatment response based on genotype (weighted average)
        new_cureF0=0.9,
        new_cureF1=0.9,
        new_cureF2 =0.9,
        new_cureF3 =0.9,
        new_cureC1 =0.9,
        new_cureC2 =0.9,
        new_cureC3 =0.9,
        new_cureC4 =0.9,
        
        std_dist = c(0.05,0.05,0.3,0.3,0.3)
      )
    out <- ode( y = inits(),times =  times, func = PanHepC, parms = parms, method = "rk4")
    
    out_df <- as.data.frame(out)
    colnames(out_df)[23:29] <- c("prev_G2","incHCC_G2","pop_G2","infect_G2","total_HCV_G2","total_HCC_G2","New death_G2")
    colnames(out_df)[15:19] <- c("C1std_cured_G2","C1new_cured_G2","C2new_cured_G2","C3new_cured_G2","C4new_cured_G2")
    out_df
    
  })
  
  out_df_g3 <- reactive({
    
    times <- seq(1999, 2020, by = 0.01)
    parms <- 
      list(
        P0=input$P0,       #popstat(YEAR=1999)
        K= input$K,        #Maximum population (carrying capacity)
        r =input$r,        #Population growth rate (logistic growth curve)
        S0=0.0305853,
        standard_start = 2004,
        new_start = 2015,
        
        
        #cost = treated1+treated2
        total_HCC=0,
        total_HCV=631000,
        
        FI= input$Fi,     #Influx rate of the population to become susceptible per year
        
        #Progression of fibrosis
        f0f1=0.117,       #Fibrosis stage F0 to F1
        f1f2=0.085,       #Fibrosis stage F1 to F2
        f2f3=0.12,        #Fibrosis stage F2 to F3
        f3c1=0.116,       #Fibrosis stage F3 to C1
        
        #Progression of cirrhosis
        c1c2=0.044,       #Fibrosis stage C1 to C2
        c2c3=0.044,       #Fibrosis stage C2 to C3
        c3c4=0.076,       #Fibrosis stage C3 to C4
        
        #Incidence of developing HCC
        c1bA=0.0068,      #Fibrosis stage C1 to bA
        c1bB=0.0099,      #Fibrosis stage C1 to bB
        c1bC=0.0029,      #Fibrosis stage C1 to bC
        c1bD=0.0068,      #Fibrosis stage C1 to bD
        
        c2bA=0.0068,      #Fibrosis stage C2 to bA
        c2bB=0.0099,      #Fibrosis stage C2 to bB
        c2bC=0.0029,      #Fibrosis stage C2 to bC
        c2bD=0.0068,      #Fibrosis stage C2 to bD
        
        c3bD=0.0664,      #Fibrosis stage C3 to bD
        c4bD=0.0664,      #Fibrosis stage C4 to bD
        
        #Death rate from cirrhosis and HCC
        deathc1=0.01,         #Death rate for Cirrhosis Child-Pugh class A
        deathc2=0.01,         #Death rate for Cirrhosis Child-Pugh class B
        deathc3=0.2,          #Death rate for Cirrhosis Child-Pugh class C
        deathc4=0.57,         #Death rate for Cirrhosis Child-Pugh class D
        
        deathbA=1/(36/12),    #Death rate for HCC_BCLC_A
        deathbB=1/(16/12),    #Death rate for HCC_BCLC_B
        deathbC=1/(6/12),     #Death rate for HCC_BCLC_C
        deathbD=1/(3/12),     #Death rate for HCC_BCLC_D
        
        deathtrn=1/(240/12),
        
        #Transplantation
        tranc4=0.0015, #Transplantation rate in cirrhosis stage C4
        tranbA=0.0015, #Transplantation rate in HCC_BCLC_A
        tranbB=0.0015, #Transplantation rate in HCC_BCLC_B
        #;newcase=17000,
        cover = 5,
        #Natural rate of death
        natdeath=0.0424, #Unrelated to cirrhosis and hepatitis C
        
        beta= 0.327,              #Transmission coefficient
        #Treatment efficacy
        #Standard treatment response based on genotype (weighted average)
        std_cureF0=0.7,
        std_cureF1 =0.7,
        std_cureF2 =0.7,
        std_cureF3 =0.7,
        std_cureC1 =0.7,
        std_cureC2=0.7,
        #Novel treatment response based on genotype (weighted average)
        new_cureF0=0.95,
        new_cureF1=0.95,
        new_cureF2 =0.95,
        new_cureF3 =0.95,
        new_cureC1 =0.95,
        new_cureC2 =0.95,
        new_cureC3 =0.95,
        new_cureC4 =0.95,
        
        std_dist = c(0.05,0.05,0.3,0.3,0.3)
      )
    out <- ode( y = inits(),times =  times, func = PanHepC, parms = parms, method = "rk4")
    
    out_df <- as.data.frame(out)
    colnames(out_df)[23:29] <- c("prev_G3","incHCC_G3","pop_G3","infect_G3","total_HCV_G3","total_HCC_G3","New death_G3")
    colnames(out_df)[15:19] <- c("C1std_cured_G3","C1new_cured_G3","C2new_cured_G3","C3new_cured_G3","C4new_cured_G3")
    out_df
    
  })
  
  out_df_g4 <- reactive({
    
    times <- seq(1999, 2020, by = 0.01)
    parms <- 
      list(
        P0=input$P0,       #popstat(YEAR=1999)
        K= input$K,        #Maximum population (carrying capacity)
        r =input$r,        #Population growth rate (logistic growth curve)
        S0=0.0305853,
        standard_start = 2004,
        new_start = 2015,
        
        
        #cost = treated1+treated2
        total_HCC=0,
        total_HCV=631000,
        
        FI= input$Fi,     #Influx rate of the population to become susceptible per year
        
        #Progression of fibrosis
        f0f1=0.117,       #Fibrosis stage F0 to F1
        f1f2=0.085,       #Fibrosis stage F1 to F2
        f2f3=0.12,        #Fibrosis stage F2 to F3
        f3c1=0.116,       #Fibrosis stage F3 to C1
        
        #Progression of cirrhosis
        c1c2=0.044,       #Fibrosis stage C1 to C2
        c2c3=0.044,       #Fibrosis stage C2 to C3
        c3c4=0.076,       #Fibrosis stage C3 to C4
        
        #Incidence of developing HCC
        c1bA=0.0068,      #Fibrosis stage C1 to bA
        c1bB=0.0099,      #Fibrosis stage C1 to bB
        c1bC=0.0029,      #Fibrosis stage C1 to bC
        c1bD=0.0068,      #Fibrosis stage C1 to bD
        
        c2bA=0.0068,      #Fibrosis stage C2 to bA
        c2bB=0.0099,      #Fibrosis stage C2 to bB
        c2bC=0.0029,      #Fibrosis stage C2 to bC
        c2bD=0.0068,      #Fibrosis stage C2 to bD
        
        c3bD=0.0664,      #Fibrosis stage C3 to bD
        c4bD=0.0664,      #Fibrosis stage C4 to bD
        
        #Death rate from cirrhosis and HCC
        deathc1=0.01,         #Death rate for Cirrhosis Child-Pugh class A
        deathc2=0.01,         #Death rate for Cirrhosis Child-Pugh class B
        deathc3=0.2,          #Death rate for Cirrhosis Child-Pugh class C
        deathc4=0.57,         #Death rate for Cirrhosis Child-Pugh class D
        
        deathbA=1/(36/12),    #Death rate for HCC_BCLC_A
        deathbB=1/(16/12),    #Death rate for HCC_BCLC_B
        deathbC=1/(6/12),     #Death rate for HCC_BCLC_C
        deathbD=1/(3/12),     #Death rate for HCC_BCLC_D
        
        deathtrn=1/(240/12),
        
        #Transplantation
        tranc4=0.0015, #Transplantation rate in cirrhosis stage C4
        tranbA=0.0015, #Transplantation rate in HCC_BCLC_A
        tranbB=0.0015, #Transplantation rate in HCC_BCLC_B
        #;newcase=17000,
        cover = 5,
        #Natural rate of death
        natdeath=0.0424, #Unrelated to cirrhosis and hepatitis C
        
        beta= 0.327,              #Transmission coefficient
        #Treatment efficacy
        #Standard treatment response based on genotype (weighted average)
        std_cureF0=0.7,
        std_cureF1 =0.7,
        std_cureF2 =0.7,
        std_cureF3 =0.7,
        std_cureC1 =0.7,
        std_cureC2=0.7,
        #Novel treatment response based on genotype (weighted average)
        new_cureF0=0.65,
        new_cureF1=0.65,
        new_cureF2 =0.65,
        new_cureF3 =0.65,
        new_cureC1 =0.65,
        new_cureC2 =0.65,
        new_cureC3 =0.65,
        new_cureC4 =0.65,
        
        std_dist = c(0.05,0.05,0.3,0.3,0.3)
      )
    out <- ode( y = inits(),times =  times, func = PanHepC, parms = parms, method = "rk4")
    
    out_df <- as.data.frame(out)
    colnames(out_df)[23:29] <- c("prev_G4","incHCC_G4","pop_G4","infect_G4","total_HCV_G4","total_HCC_G4","New death_G4")
    colnames(out_df)[15:19] <- c("C1std_cured_G4","C1new_cured_G4","C2new_cured_G4","C3new_cured_G4","C4new_cured_G4")
    out_df
    
  })
  
  out_df_g5 <- reactive({
    
    times <- seq(1999, 2020, by = 0.01)
    parms <- 
      list(
        P0=input$P0,       #popstat(YEAR=1999)
        K= input$K,        #Maximum population (carrying capacity)
        r =input$r,        #Population growth rate (logistic growth curve)
        S0=0.0305853,
        standard_start = 2004,
        new_start = 2015,
        
        
        #cost = treated1+treated2
        total_HCC=0,
        total_HCV=631000,
        
        FI= input$Fi,     #Influx rate of the population to become susceptible per year
        
        #Progression of fibrosis
        f0f1=0.117,       #Fibrosis stage F0 to F1
        f1f2=0.085,       #Fibrosis stage F1 to F2
        f2f3=0.12,        #Fibrosis stage F2 to F3
        f3c1=0.116,       #Fibrosis stage F3 to C1
        
        #Progression of cirrhosis
        c1c2=0.044,       #Fibrosis stage C1 to C2
        c2c3=0.044,       #Fibrosis stage C2 to C3
        c3c4=0.076,       #Fibrosis stage C3 to C4
        
        #Incidence of developing HCC
        c1bA=0.0068,      #Fibrosis stage C1 to bA
        c1bB=0.0099,      #Fibrosis stage C1 to bB
        c1bC=0.0029,      #Fibrosis stage C1 to bC
        c1bD=0.0068,      #Fibrosis stage C1 to bD
        
        c2bA=0.0068,      #Fibrosis stage C2 to bA
        c2bB=0.0099,      #Fibrosis stage C2 to bB
        c2bC=0.0029,      #Fibrosis stage C2 to bC
        c2bD=0.0068,      #Fibrosis stage C2 to bD
        
        c3bD=0.0664,      #Fibrosis stage C3 to bD
        c4bD=0.0664,      #Fibrosis stage C4 to bD
        
        #Death rate from cirrhosis and HCC
        deathc1=0.01,         #Death rate for Cirrhosis Child-Pugh class A
        deathc2=0.01,         #Death rate for Cirrhosis Child-Pugh class B
        deathc3=0.2,          #Death rate for Cirrhosis Child-Pugh class C
        deathc4=0.57,         #Death rate for Cirrhosis Child-Pugh class D
        
        deathbA=1/(36/12),    #Death rate for HCC_BCLC_A
        deathbB=1/(16/12),    #Death rate for HCC_BCLC_B
        deathbC=1/(6/12),     #Death rate for HCC_BCLC_C
        deathbD=1/(3/12),     #Death rate for HCC_BCLC_D
        
        deathtrn=1/(240/12),
        
        #Transplantation
        tranc4=0.0015, #Transplantation rate in cirrhosis stage C4
        tranbA=0.0015, #Transplantation rate in HCC_BCLC_A
        tranbB=0.0015, #Transplantation rate in HCC_BCLC_B
        #;newcase=17000,
        cover = 5,
        #Natural rate of death
        natdeath=0.0424, #Unrelated to cirrhosis and hepatitis C
        
        beta= 0.327,              #Transmission coefficient
        #Treatment efficacy
        #Standard treatment response based on genotype (weighted average)
        std_cureF0=0.7,
        std_cureF1 =0.7,
        std_cureF2 =0.7,
        std_cureF3 =0.7,
        std_cureC1 =0.7,
        std_cureC2=0.7,
        #Novel treatment response based on genotype (weighted average)
        new_cureF0=1,
        new_cureF1=1,
        new_cureF2 =1,
        new_cureF3 =1,
        new_cureC1 =1,
        new_cureC2 =1,
        new_cureC3 =1,
        new_cureC4 =1,
        
        std_dist = c(0.05,0.05,0.3,0.3,0.3)
      )
    out <- ode( y = inits(),times =  times, func = PanHepC, parms = parms, method = "rk4")
    
    out_df <- as.data.frame(out)
    colnames(out_df)[23:29] <- c("prev_G5","incHCC_G5","pop_G5","infect_G5","total_HCV_G5","total_HCC_G5","New death_G5")
    colnames(out_df)[15:19] <- c("C1std_cured_G5","C1new_cured_G5","C2new_cured_G5","C3new_cured_G5","C4new_cured_G5")
    out_df
    
  })
  
  out_df_g6 <- reactive({
    
    times <- seq(1999, 2020, by = 0.01)
    parms <- 
      list(
        P0=input$P0,       #popstat(YEAR=1999)
        K= input$K,        #Maximum population (carrying capacity)
        r =input$r,        #Population growth rate (logistic growth curve)
        S0=0.0305853,
        standard_start = 2004,
        new_start = 2015,
        
        
        #cost = treated1+treated2
        total_HCC=0,
        total_HCV=631000,
        
        FI= input$Fi,     #Influx rate of the population to become susceptible per year
        
        #Progression of fibrosis
        f0f1=0.117,       #Fibrosis stage F0 to F1
        f1f2=0.085,       #Fibrosis stage F1 to F2
        f2f3=0.12,        #Fibrosis stage F2 to F3
        f3c1=0.116,       #Fibrosis stage F3 to C1
        
        #Progression of cirrhosis
        c1c2=0.044,       #Fibrosis stage C1 to C2
        c2c3=0.044,       #Fibrosis stage C2 to C3
        c3c4=0.076,       #Fibrosis stage C3 to C4
        
        #Incidence of developing HCC
        c1bA=0.0068,      #Fibrosis stage C1 to bA
        c1bB=0.0099,      #Fibrosis stage C1 to bB
        c1bC=0.0029,      #Fibrosis stage C1 to bC
        c1bD=0.0068,      #Fibrosis stage C1 to bD
        
        c2bA=0.0068,      #Fibrosis stage C2 to bA
        c2bB=0.0099,      #Fibrosis stage C2 to bB
        c2bC=0.0029,      #Fibrosis stage C2 to bC
        c2bD=0.0068,      #Fibrosis stage C2 to bD
        
        c3bD=0.0664,      #Fibrosis stage C3 to bD
        c4bD=0.0664,      #Fibrosis stage C4 to bD
        
        #Death rate from cirrhosis and HCC
        deathc1=0.01,         #Death rate for Cirrhosis Child-Pugh class A
        deathc2=0.01,         #Death rate for Cirrhosis Child-Pugh class B
        deathc3=0.2,          #Death rate for Cirrhosis Child-Pugh class C
        deathc4=0.57,         #Death rate for Cirrhosis Child-Pugh class D
        
        deathbA=1/(36/12),    #Death rate for HCC_BCLC_A
        deathbB=1/(16/12),    #Death rate for HCC_BCLC_B
        deathbC=1/(6/12),     #Death rate for HCC_BCLC_C
        deathbD=1/(3/12),     #Death rate for HCC_BCLC_D
        
        deathtrn=1/(240/12),
        
        #Transplantation
        tranc4=0.0015, #Transplantation rate in cirrhosis stage C4
        tranbA=0.0015, #Transplantation rate in HCC_BCLC_A
        tranbB=0.0015, #Transplantation rate in HCC_BCLC_B
        #;newcase=17000,
        cover = 5,
        #Natural rate of death
        natdeath=0.0424, #Unrelated to cirrhosis and hepatitis C
        
        beta= 0.327,              #Transmission coefficient
        #Treatment efficacy
        #Standard treatment response based on genotype (weighted average)
        std_cureF0=0.7,
        std_cureF1 =0.7,
        std_cureF2 =0.7,
        std_cureF3 =0.7,
        std_cureC1 =0.7,
        std_cureC2=0.7,
        #Novel treatment response based on genotype (weighted average)
        new_cureF0=0.6,
        new_cureF1=0.6,
        new_cureF2 =0.6,
        new_cureF3 =0.6,
        new_cureC1 =0.6,
        new_cureC2 =0.6,
        new_cureC3 =0.6,
        new_cureC4 =0.6,
        
        std_dist = c(0.05,0.05,0.3,0.3,0.3)
      )
    out <- ode( y = inits(),times =  times, func = PanHepC, parms = parms, method = "rk4")
    
    out_df <- as.data.frame(out)
    colnames(out_df)[23:29] <- c("prev_G6","incHCC_G6","pop_G6","infect_G6","total_HCV_G6","total_HCC_G6","New death_G6")
    colnames(out_df)[15:19] <- c("C1std_cured_G6","C1new_cured_G6","C2new_cured_G6","C3new_cured_G6","C4new_cured_G6")
    out_df
    
  })
  


    #output 1
    output$distPlot <- renderPlot({
      if (v$doPlot == FALSE) return()
      x <- out_df()[,c(1,23)]
      if(input$bygenotype){
        #x <- out_df[,c(1,7:14)]
        x <- cbind(out_df()[,c(1,23)],out_df_g1()[23],out_df_g2()[23],out_df_g3()[23],out_df_g4()[23],out_df_g5()[23],out_df_g6()[23])
      }
      isolate({

      #time year >= input$year[1] , year <= input$year[2]
      x_time <- out_df()["time"] >= input$year[1] & out_df()["time"] <= input$year[2]

      x <- x[x_time,]
      x_melt <-melt(x, id="time")
      ggplot(data = x_melt) + 
        labs( x = "time", y = "Prevalence")+
        geom_line(mapping = aes(x = time, y = value,color = variable),size = 1.5)+
        scale_x_continuous(breaks=seq(input$year[1], input$year[2], 1))
        
      })
    })
    
    #output 2
    output$distPlot2 <- renderPlot({
      if (v$doPlot == FALSE) return()
      x <- out_df()[,c(1,21,22)]
      
      if(input$showgenotype){
        
        x <- out_df()[,c(1,21,22,29)]
      }
      isolate({

        
        #time year >= input$year[1] , year <= input$year[2]
        x_time <- out_df()["time"] >= input$year[1] & out_df()["time"] <= input$year[2]
        
        x <- x[x_time,]
        x_melt <-melt(x, id="time")

        ggplot(data = x_melt) + 
          
          geom_line(mapping = aes(x = time, y = value,color = variable),size = 1.5)
      })
    })
    
    #output 3
    output$Anin_Plot <- renderPlot({
      if (v$doPlot == FALSE) return()
      x <- out_df()[,c(1,15:19)]
      
      isolate({
        
        
        #time year >= input$year[1] , year <= input$year[2]
       x_time <- out_df()["time"] >= input$year[1] & out_df()["time"] <= input$year[2]
        
        x <- x[x_time,]

      x_melt <-melt(x, id="time")
      

          
       
          ggplot(data = x_melt) + 
            geom_line(mapping = aes(x = time, y = value,color = variable),size = 1.5)+
            scale_x_continuous(breaks=seq(input$year[1], input$year[2], 1))
        
      })
    })
    
    #output 3 with genotype C1
    output$Anin_G_C1_Plot <- renderPlot({
      if (v$doPlot == FALSE) return()
      x <- cbind(out_df()[,c(1,15,16)],out_df_g1()[15:16],out_df_g2()[15:16],out_df_g3()[15:16],out_df_g4()[15:16],out_df_g5()[15:16],out_df_g6()[15:16])
      
      isolate({
        ggplot(data = x,aes(time)) + 
          #C1new
          geom_line(aes(y = C1new_cured,color="C1"),size = 1.5)+
          geom_line(aes(y = C1new_cured_G1,color="C1_G1"),size = 1.5)+
          geom_line(aes(y = C1new_cured_G2,color="C1_G2"),size = 1.5)+
          geom_line(aes(y = C1new_cured_G3,color="C1_G3"),size = 1.5)+
          geom_line(aes(y = C1new_cured_G4,color="C1_G4"),size = 1.5)+
          geom_line(aes(y = C1new_cured_G5,color="C1_G5"),size = 1.5)+
          geom_line(aes(y = C1new_cured_G6,color="C1_G6"),size = 1.5)+
          #C1std
          geom_line(aes(y = C1std_cured,color="C1"),linetype=2,size = 1.5)+
          geom_line(aes(y = C1std_cured_G1 ,color="C1_G1"),linetype=2,size = 1.5)+
          geom_line(aes(y = C1std_cured_G2 ,color="C1_G2"),linetype=2,size = 1.5)+
          geom_line(aes(y = C1std_cured_G3 ,color="C1_G3"),linetype=2,size = 1.5)+
          geom_line(aes(y = C1std_cured_G4 ,color="C1_G4"),linetype=2,size = 1.5)+
          geom_line(aes(y = C1std_cured_G5 ,color="C1_G5"),linetype=2,size = 1.5)+
          geom_line(aes(y = C1std_cured_G6 ,color="C1_G6"),linetype=2,size = 1.5)+
          ylab("value")+
          scale_x_continuous(breaks=seq(input$year[1], input$year[2], 1),limits = c(input$year[1],input$year[2]))
      })
      
    })
    
    #output 3 with genotype C2
    output$Anin_G_C2_Plot <- renderPlot({
      if (v$doPlot == FALSE) return()
      x <- cbind(out_df()[,c(1,17)],out_df_g1()[17],out_df_g2()[17],out_df_g3()[17],out_df_g4()[17],out_df_g5()[17],out_df_g6()[17])
      
      isolate({
        ggplot(data = x,aes(time)) + 
          #C1new
          geom_line(aes(y = C2new_cured,color="C2"),size = 1.5)+
          geom_line(aes(y = C2new_cured_G1,color="C2_G1"),size = 1.5)+
          geom_line(aes(y = C2new_cured_G2,color="C2_G2"),size = 1.5)+
          geom_line(aes(y = C2new_cured_G3,color="C2_G3"),size = 1.5)+
          geom_line(aes(y = C2new_cured_G4,color="C2_G4"),size = 1.5)+
          geom_line(aes(y = C2new_cured_G5,color="C2_G5"),size = 1.5)+
          geom_line(aes(y = C2new_cured_G6,color="C2_G6"),size = 1.5)+
          ylab("value")+
          scale_x_continuous(breaks=seq(input$year[1], input$year[2], 1),limits = c(input$year[1],input$year[2]))
      })
      
    })
    
    #output 3 with genotype C3
    output$Anin_G_C3_Plot <- renderPlot({
      if (v$doPlot == FALSE) return()
      x <- cbind(out_df()[,c(1,18)],out_df_g1()[18],out_df_g2()[18],out_df_g3()[18],out_df_g4()[18],out_df_g5()[18],out_df_g6()[18])
      
      isolate({
        ggplot(data = x,aes(time)) + 
          #C1new
          geom_line(aes(y = C3new_cured,color="C3"),size = 1.5)+
          geom_line(aes(y = C3new_cured_G1,color="C3_G1"),size = 1.5)+
          geom_line(aes(y = C3new_cured_G2,color="C3_G2"),size = 1.5)+
          geom_line(aes(y = C3new_cured_G3,color="C3_G3"),size = 1.5)+
          geom_line(aes(y = C3new_cured_G4,color="C3_G4"),size = 1.5)+
          geom_line(aes(y = C3new_cured_G5,color="C3_G5"),size = 1.5)+
          geom_line(aes(y = C3new_cured_G6,color="C3_G6"),size = 1.5)+
          ylab("value")+
          scale_x_continuous(breaks=seq(input$year[1], input$year[2], 1),limits = c(input$year[1],input$year[2]))
      })
      
    })
    
    #output 3 with genotype C4
    output$Anin_G_C4_Plot <- renderPlot({
      if (v$doPlot == FALSE) return()
      x <- cbind(out_df()[,c(1,19)],out_df_g1()[19],out_df_g2()[19],out_df_g3()[19],out_df_g4()[19],out_df_g5()[19],out_df_g6()[19])
      
      isolate({
        ggplot(data = x,aes(time)) + 
          #C1new
          geom_line(aes(y = C4new_cured,color="C4"),size = 1.5)+
          geom_line(aes(y = C4new_cured_G1,color="C4_G1"),size = 1.5)+
          geom_line(aes(y = C4new_cured_G2,color="C4_G2"),size = 1.5)+
          geom_line(aes(y = C4new_cured_G3,color="C4_G3"),size = 1.5)+
          geom_line(aes(y = C4new_cured_G4,color="C4_G4"),size = 1.5)+
          geom_line(aes(y = C4new_cured_G5,color="C4_G5"),size = 1.5)+
          geom_line(aes(y = C4new_cured_G6,color="C4_G6"),size = 1.5)+
          ylab("value")+
          scale_x_continuous(breaks=seq(input$year[1], input$year[2], 1),limits = c(input$year[1],input$year[2]))
      })
      
    })
    
    #output 4
    output$distPlot4 <- renderPlot({
      
      if (v$doPlot == FALSE) return()
      x <- out_df()[,c(1,15:17)]
      isolate({
        
        
        #time year >= input$year[1] , year <= input$year[2]
        x_time <- out_df()["time"] >= input$year[1] & out_df()["time"] <= input$year[2]
        
        x <- x[x_time,]
      
      x_melt <-melt(x, id="time")
      
      ggplot(data = x_melt) + 
        geom_line(mapping = aes(x = time, y = value,color = variable),size = 1.5)+
        scale_x_continuous(breaks=seq(input$year[1], input$year[2], 1))
      })
    })
    #textoutput
    output$text1 <- renderText({
      if(input$Treatment == 1){
        "Treatment efficacy : 0.7"
      } else if (input$Treatment == 2){
        "Treatment efficacy : 0.9"
      } else if (input$Treatment == 3){
        "Treatment efficacy : 0.8"
      } else if (input$Treatment == 4){
        "Treatment efficacy : 0.9"
      } else if (input$Treatment == 5){
        "Treatment efficacy : 0.9 "
      }
    })
    
    output$downloadData <- 
      
      downloadHandler(
        
        filename = "result.xlsx",
        content = function(file) {

          write.xlsx(out_df(), file,sheetName="Result" ,row.names = FALSE)
          
        },
        contentType = "text/xlsx"
      )
    #G1
    output$downloadDataG1 <- 
      
      downloadHandler(
        
        filename = "result_g1.xlsx",
        content = function(file) {
          
          write.xlsx(out_df_g1(), file, sheetName="Result_G1", append=TRUE, row.names=FALSE)
          
        },
        contentType = "text/xlsx"
      )
    #G2
    output$downloadDataG2 <- 
      
      downloadHandler(
        
        filename = "result_g2.xlsx",
        content = function(file) {
          
          write.xlsx(out_df_g2(), file, sheetName="Result_G2", append=TRUE, row.names=FALSE)
          
        },
        contentType = "text/xlsx"
      )
    
    #G3
    output$downloadDataG3 <- 
      
      downloadHandler(
        
        filename = "result_g3.xlsx",
        content = function(file) {
          
          write.xlsx(out_df_g3(), file, sheetName="Result_G3", append=TRUE, row.names=FALSE)
          
        },
        contentType = "text/xlsx"
      )
    output$downloadDataG4 <- 
      
      downloadHandler(
        
        filename = "result_g4.xlsx",
        content = function(file) {
          
          write.xlsx(out_df_g4(), file, sheetName="Result_G4", append=TRUE, row.names=FALSE)
          
        },
        contentType = "text/xlsx"
      )
    
    output$downloadDataG5 <- 
      
      downloadHandler(
        
        filename = "result_g5.xlsx",
        content = function(file) {
          
          write.xlsx(out_df_g5(), file, sheetName="Result_G5", append=TRUE, row.names=FALSE)
          
        },
        contentType = "text/xlsx"
      )
    output$downloadDataG6 <- 
      
      downloadHandler(
        
        filename = "result_g6.xlsx",
        content = function(file) {
          
          write.xlsx(out_df_g6(), file, sheetName="Result_G6", append=TRUE, row.names=FALSE)
          
        },
        contentType = "text/xlsx"
      )

    output$downloadparameter <-

      downloadHandler(
        filename = "parameter.xlsx",
        content = function(file) {
          
          write.xlsx(table_parmeter(), file, sheetName="Sheet1",row.names = FALSE)
          
        },
        contentType = "text/xlsx"
      )
    

})
