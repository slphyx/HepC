#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#    setwd("D:/HepC-betaweb/HepC-betaweb/web")
#C:\Hep-c

library(shiny)
library(Rcpp)
library(deSolve)
library(reshape2)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(erer)
library(xlsx)
library(scales)
library(plyr) 
library(shinyjs)
library(readxl)
library(openxlsx) 

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  

  v <- reactiveValues(doPlot = FALSE)
  options(scipen=6)
  
  #Treatment input checkbox
  Treatment <- reactiveValues(new_cureF0 = 0.1,
                              new_cureF1 = 0.1,
                              new_cureF2 = 0.1,
                              new_cureF3 = 0.1,
                              new_cureC1 = 0.1,
                              new_cureC2 = 0.1,
                              new_cureC3 = 0.1,
                              new_cureC4 = 0.1,
                              cost = 10)
  #do when checkbox of Treatment change.
  observeEvent(input$Treatment, {
    #drug 1
    if(input$Treatment == 1){
      Treatment$new_cureF0 <- 0.6
      Treatment$new_cureF1 <- 0.6
      Treatment$new_cureF2 <- 0.6
      Treatment$new_cureF3 <- 0.6
      Treatment$new_cureC1 <- 0.6
      Treatment$new_cureC2 <- 0.6
      Treatment$new_cureC3 <- 0.6
      Treatment$new_cureC4 <- 0.4
      Treatment$cost <- 10
      #drug 2
    }else if(input$Treatment == 2){
      Treatment$new_cureF0 <- 0.7
      Treatment$new_cureF1 <- 0.7
      Treatment$new_cureF2 <- 0.8
      Treatment$new_cureF3 <- 0.7
      Treatment$new_cureC1 <- 0.7
      Treatment$new_cureC2 <- 0.7
      Treatment$new_cureC3 <- 0.7
      Treatment$new_cureC4 <- 0.4
      Treatment$cost <- 15
      #drug 3
    }else if(input$Treatment == 3){
      Treatment$new_cureF0 <- 0.8
      Treatment$new_cureF1 <- 0.8
      Treatment$new_cureF2 <- 0.8
      Treatment$new_cureF3 <- 0.8
      Treatment$new_cureC1 <- 0.8
      Treatment$new_cureC2 <- 0.8
      Treatment$new_cureC3 <- 0.8
      Treatment$new_cureC4 <- 0.5
      Treatment$cost <- 20
      #drug 4
    }else if(input$Treatment == 4){
      Treatment$new_cureF0 <- 0.9
      Treatment$new_cureF1 <- 0.9
      Treatment$new_cureF2 <- 0.9
      Treatment$new_cureF3 <- 0.9
      Treatment$new_cureC1 <- 0.9
      Treatment$new_cureC2 <- 0.9
      Treatment$new_cureC3 <- 0.9
      Treatment$new_cureC4 <- 0.5
      Treatment$cost <- 25
      #drug 5
    }else if(input$Treatment == 5){
      Treatment$new_cureF0 <- 0.9
      Treatment$new_cureF1 <- 0.9
      Treatment$new_cureF2 <- 0.9
      Treatment$new_cureF3 <- 0.9
      Treatment$new_cureC1 <- 0.9
      Treatment$new_cureC2 <- 0.9
      Treatment$new_cureC3 <- 0.9
      Treatment$new_cureC4 <- 0.5
      Treatment$cost <- 20
      #Another drug
    }else{
      Treatment$new_cureF0 <- input$Input_F0
      Treatment$new_cureF1 <- input$Input_F1
      Treatment$new_cureF2 <- input$Input_F2
      Treatment$new_cureF3 <- input$Input_F3
      Treatment$new_cureC1 <- input$Input_C1
      Treatment$new_cureC2 <- input$Input_C2
      Treatment$new_cureC3 <- input$Input_C3
      Treatment$new_cureC4 <- input$Input_C4
      Treatment$cost <- input$Input_Cost
    }
    
  })
  
  #comfirm
  observeEvent(input$Comfirm, {
    if(input$Treatment == 1){
      Treatment$new_cureF0 <- 0.6
      Treatment$new_cureF1 <- 0.6
      Treatment$new_cureF2 <- 0.6
      Treatment$new_cureF3 <- 0.6
      Treatment$new_cureC1 <- 0.6
      Treatment$new_cureC2 <- 0.6
      Treatment$new_cureC3 <- 0.6
      Treatment$new_cureC4 <- 0.4
      Treatment$cost <- 10
      #drug 2
    }else if(input$Treatment == 2){
      Treatment$new_cureF0 <- 0.7
      Treatment$new_cureF1 <- 0.7
      Treatment$new_cureF2 <- 0.8
      Treatment$new_cureF3 <- 0.7
      Treatment$new_cureC1 <- 0.7
      Treatment$new_cureC2 <- 0.7
      Treatment$new_cureC3 <- 0.7
      Treatment$new_cureC4 <- 0.4
      Treatment$cost <- 15
      #drug 3
    }else if(input$Treatment == 3){
      Treatment$new_cureF0 <- 0.8
      Treatment$new_cureF1 <- 0.8
      Treatment$new_cureF2 <- 0.8
      Treatment$new_cureF3 <- 0.8
      Treatment$new_cureC1 <- 0.8
      Treatment$new_cureC2 <- 0.8
      Treatment$new_cureC3 <- 0.8
      Treatment$new_cureC4 <- 0.5
      Treatment$cost <- 20
      #drug 4
    }else if(input$Treatment == 4){
      Treatment$new_cureF0 <- 0.9
      Treatment$new_cureF1 <- 0.9
      Treatment$new_cureF2 <- 0.9
      Treatment$new_cureF3 <- 0.9
      Treatment$new_cureC1 <- 0.9
      Treatment$new_cureC2 <- 0.9
      Treatment$new_cureC3 <- 0.9
      Treatment$new_cureC4 <- 0.5
      Treatment$cost <- 25
      #drug 5
    }else if(input$Treatment == 5){
      Treatment$new_cureF0 <- 0.9
      Treatment$new_cureF1 <- 0.9
      Treatment$new_cureF2 <- 0.9
      Treatment$new_cureF3 <- 0.9
      Treatment$new_cureC1 <- 0.9
      Treatment$new_cureC2 <- 0.9
      Treatment$new_cureC3 <- 0.9
      Treatment$new_cureC4 <- 0.5
      Treatment$cost <- 20
      #Another drug
    }else{
      Treatment$new_cureF0 <- input$Input_F0
      Treatment$new_cureF1 <- input$Input_F1
      Treatment$new_cureF2 <- input$Input_F2
      Treatment$new_cureF3 <- input$Input_F3
      Treatment$new_cureC1 <- input$Input_C1
      Treatment$new_cureC2 <- input$Input_C2
      Treatment$new_cureC3 <- input$Input_C3
      Treatment$new_cureC4 <- input$Input_C4
      Treatment$cost <- input$Input_Cost
    }
    
  }
  )
  observe({
    if (input$Treatment != 6) {
      disable("Input_F0")
      disable("Input_F1")
      disable("Input_F2")
      disable("Input_F3")
      disable("Input_C1")
      disable("Input_C2")
      disable("Input_C3")
      disable("Input_C4")
      disable("Input_Cost")
      
    }else{
      enable("Input_F0")
      enable("Input_F1")
      enable("Input_F2")
      enable("Input_F3")
      enable("Input_C1")
      enable("Input_C2")
      enable("Input_C3")
      enable("Input_C4")
      enable("Input_Cost")

    }
  })
  
  observeEvent(input$go, {
    # 0 will be coerced to FALSE
    # 1+ will be coerced to TRUE
    v$doPlot <- input$go
  })
  
  observeEvent(input$reset, {
    v$doPlot <- FALSE
  })  
    
  observeEvent(input$screening, {
    if(input$screening==1){
      disable("risk_g")
      enable("age_s")
    }
    if(input$screening==2){
      disable("age_s")
      enable("risk_g")
    }
  }
  )
  
  
  
  #button to reset changed values back to the default values (from ui)
  #section 1
  observeEvent(input$resetSect1, {
    #reset("P0")
    updateSliderInput(session, inputId = "P0",value = 60000000)
    updateSliderInput(session, inputId = "K", value = 70000000)
    updateSliderInput(session, inputId = "r", value = 0.16)
    updateSliderInput(session, inputId = "beta", value = 0.32)
    updateSliderInput(session, inputId = "Fi", value = 0.0001)
  })
  
  #section 2
  observeEvent(input$resetSect2, {
    updateSliderInput(session, inputId = "f0f1", value = 0.117)
    updateSliderInput(session, inputId = "f1f2", value = 0.085)
    updateSliderInput(session, inputId = "f2f3", value = 0.12)
    updateSliderInput(session, inputId = "f3cA", value = 0.116)
  })
  
  #section3
  observeEvent(input$resetSect3, {
    updateSliderInput(session, inputId = "cAcB", value = 0.044)
    updateSliderInput(session, inputId = "cBcC", value = 0.076)
  })
  
  #section4
  observeEvent(input$resetSect4, {
    updateSliderInput(session, inputId = "c1bA",value = 0.0068)
    updateSliderInput(session, inputId = "c2bA", value = 0.0068)
    updateSliderInput(session, inputId = "c1bB", value = 0.0099)
    updateSliderInput(session, inputId = "c2bB", value = 0.0099)
    updateSliderInput(session, inputId = "c1bC", value = 0.0029)
    updateSliderInput(session, inputId = "c2bC", value = 0.0029)
    updateSliderInput(session, inputId = "c1bD", value = 0.0068)
    updateSliderInput(session, inputId = "c2bD", value = 0.0068)
    updateSliderInput(session, inputId = "c3bD", value = 0.066)
    updateSliderInput(session, inputId = "c4bD", value = 0.066)
  })

    sourceCpp('p1_scr_SS.cpp')
    
    parms <- reactive({
      list(
        P0=input$P0,       #popstat(YEAR=1999)
        K= input$K,        #Maximum population (carrying capacity)
        r = input$r,        #Population growth rate (logistic growth curve)
        flowin = input$Fi,
        caset0 =  input$P0*0.03,      #0.03*P0,
        standard_start = 5,
        new_start = 20,
        nscr = 0.05,
        scr_yr = 10,
        scr_cov = 0.09,      #0.9/scr.yr,
        sens = 0.985,
        pF0scr = 0.07,
        pF1scr = 0.03,
        pF2scr=0.49,
        pF3scr=0.12,
        pC1scr=0.0012,
        pC2scr=0.0012,
        pC3scr=0.0099,
        pC4scr=0.15,
        
        #Natural rate of death
        natdeath=0.0424, #Unrelated to cirrhosis and hepatitis C
        
        beta= 0.02,              #Transmission coefficient
        
        #standard treatment allocation
        F0std = 0.05,
        F1std = 0.05,
        F2std = 0.3,
        F3std = 0.3,
        C1std = 0.3,
        
        std_cureF0=0.7,
        std_cureF1=0.7,
        std_cureF2=0.7,
        std_cureF3=0.7,
        std_cureC1=0.7,
        new_cureF0=0.985,
        new_cureF1=0.985,
        new_cureF2=0.985,
        new_cureF3=0.985,
        new_cureC1=0.985,
        new_cureC2=0.985,
        new_cureC3=0.985,
        new_cureC4=0.985,
        
        #Progression of fibrosis
        f0f1=input$f0f1,       #Fibrosis stage F0 to F1
        f1f2=input$f1f2,       #Fibrosis stage F1 to F2
        f2f3=input$f2f3,        #Fibrosis stage F2 to F3
        f3c1=input$f3c1,       #Fibrosis stage F3 to C1
        
        #Progression of cirrhosis
        c1c2=0.044,       #Fibrosis stage C1 to C2
        c2c3=0.044,       #Fibrosis stage C2 to C3
        c3c4=0.076,       #Fibrosis stage C3 to C4
        
        #Incidence of developing HCC
        c1bA=input$c1bA,      #Fibrosis stage C1 to bA
        c1bB=input$c1bB,      #Fibrosis stage C1 to bB
        c1bC=input$c1bC,      #Fibrosis stage C1 to bC
        c1bD=input$c1bD,      #Fibrosis stage C1 to bD
        
        c2bA=input$c2bA,      #Fibrosis stage C2 to bA
        c2bB=input$c2bB,      #Fibrosis stage C2 to bB
        c2bC=input$c2bC,      #Fibrosis stage C2 to bC
        c2bD=input$c2bD,      #Fibrosis stage C2 to bD
        
        c3bD=input$c3bD,      #Fibrosis stage C3 to bD
        c4bD=input$c4bD,      #Fibrosis stage C4 to bD
        
        #Death rate from cirrhosis and HCC
        deathc1=0.01,         #Death rate for Cirrhosis Child-Pugh class A
        deathc2=0.01,         #Death rate for Cirrhosis Child-Pugh class B
        deathc3=0.2,          #Death rate for Cirrhosis Child-Pugh class C
        deathc4=0.57,         #Death rate for Cirrhosis Child-Pugh class D
        
        deathbA=1/(36/12),    #Death rate for HCC_BCLC_A
        deathbB=1/(16/12),    #Death rate for HCC_BCLC_B
        deathbC=1/(6/12),     #Death rate for HCC_BCLC_C
        deathbD=1/(3/12),     #Death rate for HCC_BCLC_D
        
        deathtran=1/(240/12),
        
        #Transplantation
        tranc4=0.0015, #Transplantation rate in cirrhosis stage C4
        tranbA=0.0015, #Transplantation rate in HCC_BCLC_A
        tranbB=0.0015, #Transplantation rate in HCC_BCLC_B
        
        #Treatment efficacy
        #Standard treatment response based on genotype (weighted average)
        std_cureF0=0.72,
        std_cureF1 =0.72,
        std_cureF2 =0.72,
        std_cureF3 =0.72,
        std_cureC1 =0.72,
        std_cureC2=0.72,
        #Novel treatment response based on genotype (weighted average)
        new_cureF0= 0.985,
        new_cureF1= 0.985,
        new_cureF2  = 0.985,
        new_cureF3 = 0.985,
        new_cureC1 = 0.985,
        new_cureC2 = 0.985,
        new_cureC3 = 0.985,
        new_cureC4 = 0.985
        
      )
    })
    
    S <-reactive({
      1*(parms()$P0 - parms()$caset0) 
    })
    
    
    inits <- reactive({ 
      c(
        S=S(),
        F0=0.2825*parms()$caset0,
        F1=0.2825*parms()$caset0,
        F2=0.184*parms()$caset0,
        F3=0.124*parms()$caset0,
        C1=0.03175*parms()$caset0,
        C2=0.03175*parms()$caset0,
        C3=0.03175*parms()$caset0,
        C4=0.03174*parms()$caset0,
        HCC_A=0,
        HCC_B=0,
        HCC_C=0,
        HCC_D=0,
        D=0,
        dthC14=0
        ,dthHCC=0
        ,C1std_cured=0,
        C1new_cured=0,
        C2new_cured=0,
        C3new_cured=0,
        C4new_cured=0
        #set up initial death
        
        
      )
    })
    
    #parameter 
    p_t <- reactiveValues(S_screening = 0,
                          Pos_T = 0 , #Positive True
                          Pos_F = 0 , #Positive False
                          Neg_T = 0 , #Negative True
                          Neg_T = 0   #Negative False
    )
    load("popscreen_list.RData")
    
    #table for "Screening" panel, age groups. involves importing data from excel
    screeningdesc <- read_excel("treatmentdesc.xlsx")
    observe({
      output$screeningtbl <- render_tableHTML(
        tableHTML(screeningdesc, rownames = FALSE,
                  border = 3, 
                  collapse = c("collapse", "separate","separate_shiny"), 
                  spacing = "5px 2px"
        ) %>%
          add_theme("rshiny-blue"))
    })
    
    #table for "Screening" panel, risk groups. involves importing data from excel
    riskdesc <- read_excel("riskdesc.xlsx")
    observe({
      output$risktbl <- render_tableHTML(
        tableHTML(riskdesc, rownames = FALSE,
                  border = 3, 
                  collapse = c("collapse", "separate","separate_shiny"), 
                  spacing = "5px 2px"
        ) %>%
          add_theme("rshiny-blue"))
    })
    
    #table for "Diagnosis" panel. involves importing data from excel
    
    testdesc <- read_excel("testdesc.xlsx")
    observe({
      output$testtbl <- render_tableHTML(
        tableHTML(testdesc, rownames = FALSE,
                  row_groups = list(c(2,3), c("Test 1", "Test 2")),
                  border = 3, 
                  collapse = c("collapse", "separate","separate_shiny"), 
                  spacing = "5px 2px"
        ) %>%
          add_theme("rshiny-blue")) 
    })
    
    #table for "Treatment" panel. involves importing data from excel
    
    treatmentdesc <- read_excel("treatmentdesc.xlsx")
    observe({
      output$treatmenttbl <- render_tableHTML(
        tableHTML(treatmentdesc, rownames = FALSE,
                  second_headers = list(c(1,2,1), c("","Sustained Virological Response (%)", "")),
                  border = 3, 
                  collapse = c("collapse", "separate","separate_shiny"), 
                  spacing = "5px 2px"
        ) %>%
          add_theme("rshiny-blue"))
    })
    
    
    out_df <- reactive({
      
      times <- seq(0, 40,by=1)
      
      out <- ode( y = inits(),times =  times, func = PanHepC, parms = parms())
      
      out_df <- as.data.frame(out)
      out_df
    })
    
    #output per year
    out_df_year <- reactive({
      
      time_year <- out_df()["time"] %% 1 ==0
      out_df_year <- out_df()[time_year,]

    })
    
    #cost plot
    cost_plot <- reactive({
      cost_times <- seq(1999, 2020, by = 1)
      cost_per_year <- 0
      cost_func <- function(time,state,parms){
        list(parms)
      }
      cost <- as.numeric(Treatment$cost)
      out <- ode( y = cost_per_year,times =  cost_times, func = cost_func, parms = cost, method = "rk4")
      cost_plot <- as.data.frame(out)
      colnames(cost_plot)[2] <- "Total_Cost"
      cost_plot
    })
    
    #output 1
    output$distPlot <- renderPlot({
      if (v$doPlot == FALSE) return()

      isolate({

      x <- out_df()[,c(1,23)]
      
    
      x_melt <-melt(x, id="time")
      ggplot(data = x_melt) + 
        labs( x = "time", y = "Prevalence")+
        geom_line(mapping = aes(x = time, y = value,color = variable),size = 1.5)
        
      })
    })
    
    #output 2
    output$distPlot2 <- renderPlot({
      if (v$doPlot == FALSE) return()
      x <- out_df()[,c(1,15,16,17)]
      
      if(input$showNewDeath){
        
        x <- out_df()[,c(1,15,16,17,26)]
      }
      isolate({

        x_melt <-melt(x, id="time")

        ggplot(data = x_melt) + 
          
          geom_line(mapping = aes(x = time, y = value,color = variable),size = 1.5)
      })
    })
    
    #output 3
    output$distPlot3 <- renderPlot({
      
      if (v$doPlot == FALSE) return()
      x <- out_df()[,c(1,27)]
      
      isolate({


      x_melt <-melt(x, id="time")
      
      ggplot(data = x_melt) + 
        geom_line(mapping = aes(x = time, y = value,color = variable),size = 1.5)
      })
    })
    
    #output 4
    output$distPlot4 <- renderPlot({
      
      if (v$doPlot == FALSE) return()
      x <- cost_plot()
      isolate({
        
      
      ggplot(data = x) + 
        geom_line(mapping = aes(x = time, y =Total_Cost ),size = 1.5)
      })
    })
    
    #textoutput
    output$text1 <- renderText({
        paste("Treatment efficacy :" , mean(c(Treatment$new_cureF0
                                               ,Treatment$new_cureF2
                                               ,Treatment$new_cureF3
                                               ,Treatment$new_cureF4
                                               ,Treatment$new_cureC1
                                               ,Treatment$new_cureC2
                                               ,Treatment$new_cureC3
                                               ,Treatment$new_cureC4))
              )
      

    })
    output$text2 <-renderText({
      paste("Treatment cost :" , Treatment$cost)
    })
    
    output$screening_p <- renderText({
      paste("screening people :" , (p_t$S_screening) )
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("parms", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(parms(), file, row.names = FALSE)
      },contentType = "text/csv"
    )

    output$downloadData2 <-       

    downloadHandler(

      filename = "result.xlsx",
      content = function(file) {
        
        write.xlsx(out_df_year(), file, sheetName="Sheet1")
        
      }, contentType = "text/xlsx"
      
    )

})
