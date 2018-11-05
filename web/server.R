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
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  v <- reactiveValues(doPlot = FALSE)
  options(scipen=6)
  
  observeEvent(input$go, {
    # 0 will be coerced to FALSE
    # 1+ will be coerced to TRUE
    v$doPlot <- input$go
  })
  
  observeEvent(input$reset, {
    v$doPlot <- FALSE
  })  
    
    setwd("D:/Users/HepC-betaweb/HepC-betaweb/web")
    sourceCpp('p1.cpp')
    parameter <-function(){
      parms <- list(
        K= input$K,        #Maximum population (carrying capacity)
        P0=input$P0,       #popstat(YEAR=1999)
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
        new_cureF0=0.985,
        new_cureF1=0.985,
        new_cureF2 =0.985,
        new_cureF3 =0.985,
        new_cureC1 =0.985,
        new_cureC2 =0.985,
        new_cureC3 =0.985,
        new_cureC4 =0.985,
        
        std_dist = c(0.05,0.05,0.3,0.3,0.3)
        
      )
    }
    
    f_init <- function(S){
      inits <- c(
        S = S, #;0.01*#pop_since1960(TIME=1)
        F0=0.2825*S,#;genotype1
        F1=0.2825*S,
        F2=0.184*S,
        F3=0.124*S,
        C1=0.03175*S,# ;CirA
        C2=0.03175*S,#; CirA
        C3=0.03175*S,#; CirB
        C4=0.03175*S,#;CirC
        
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
    }
    
    FS <-  function(S0,P0){
      S <- S0 * P0;
    }

    
    f_data <- function(){
      parms <- parameter()
      
      S <- parms$S0* parms$P0;
      
      #Initial status of model
      inits <- f_init(S)
        
      
      times <- seq(1999, 2020, by = 0.01)
      
      
      
      #out <-ode(yini3,times= times3,func = mysystem3,parms = pars3,method = "rk4")
      #plot(out,select = c("S"))
      out <- ode( y = inits,times =  times, func = PanHepC, parms = parms, method = "rk4")
      
      #    output$distPlot <- renderPlot({
      #    plot(out,select = input$checkGroup,new=TRUE)
      #     
      #    })
      
      out_df <- as.data.frame(out)
      colnames(out_df)[23:28] <- c("prev","incHCC","pop","infect","total_HCV","total_HCC")
      out_df
    }
    isolate({
    p <- parameter()
    s <- FS(p$P0,p$S0)
    i <- f_init(s)
    data_p <- cbind(as.data.frame(p),as.data.frame(rbind(i)))
    })
    #output 1
    output$distPlot <- renderPlot({
      if (v$doPlot == FALSE) return()
      out_df <- f_data()

      isolate({
        p <- parameter()
        s <- FS(p$P0,p$S0)
        i <- f_init(s)
        data_p <- cbind(as.data.frame(p),as.data.frame(rbind(i)))
      x <- out_df[,c(1,23)]
      
      
      #time year >= input$year[1] , year <= input$year[2]
      x_time <- out_df["time"] >= input$year[1] & out_df["time"] <= input$year[2]

      x <- x[x_time,]
      x_melt <-melt(x, id="time")
      ggplot(data = x_melt) + 
        labs( x = "time", y = "Prevalence")+
        geom_line(mapping = aes(x = time, y = value,color = variable),size = 1.5)
        
      })
    })
    
    #output 2
    output$distPlot2 <- renderPlot({
      if (v$doPlot == FALSE) return()
      out_df <- f_data()
      x <- out_df[,c(1,21,22)]
      
      if(input$showgenotype){
        
        x <- out_df[,c(1,21,22,29)]
      }
      isolate({

        
        #time year >= input$year[1] , year <= input$year[2]
        x_time <- out_df["time"] >= input$year[1] & out_df["time"] <= input$year[2]
        
        x <- x[x_time,]
        x_melt <-melt(x, id="time")

        ggplot(data = x_melt) + 
          
          geom_line(mapping = aes(x = time, y = value,color = variable),size = 1.5)
      })
    })
    
    #output 3
    output$distPlot3 <- renderPlot({
      
      if (v$doPlot == FALSE) return()
      out_df <- f_data()
      x <- out_df[,c(1,27,28)]
      
      if(input$showgenotype2){
        #x <- out_df[,c(1,7:14)]
        x <- out_df[,c(1:21)]
      }
      isolate({
        
        
        #time year >= input$year[1] , year <= input$year[2]
        x_time <- out_df["time"] >= input$year[1] & out_df["time"] <= input$year[2]
        
        x <- x[x_time,]

      x_melt <-melt(x, id="time")
      
      ggplot(data = x_melt) + 
        geom_line(mapping = aes(x = time, y = value,color = variable),size = 1.5)
      })
    })
    
    #output 4
    output$distPlot4 <- renderPlot({
      
      if (v$doPlot == FALSE) return()
      out_df <- f_data()
      x <- out_df[,c(1,15:17)]
      isolate({
        
        
        #time year >= input$year[1] , year <= input$year[2]
        x_time <- out_df["time"] >= input$year[1] & out_df["time"] <= input$year[2]
        
        x <- x[x_time,]
      
      x_melt <-melt(x, id="time")
      
      ggplot(data = x_melt) + 
        geom_line(mapping = aes(x = time, y = value,color = variable),size = 1.5)
      })
    })
    #textoutput
    output$text1 <- renderText({
      if(input$Treatment == 1){
        "Treatment efficacy : 0.7 \n
         Treatment cost : 10 $ per week"
      } else if (input$Treatment == 2){
        "Treatment efficacy : 0.9 \n
         Treatment cost : 100 $ per week"
      } else if (input$Treatment == 3){
        "Treatment efficacy : 0.8 \n
         Treatment cost : 50 $ per week"
      } else if (input$Treatment == 4){
        "Treatment efficacy : 0.9 \n
         Treatment cost : 50 $ per week"
      } else if (input$Treatment == 5){
        "Treatment efficacy : 0.9 \n
         Treatment cost : 50 $ per week"
      }
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("prev", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(f_data(), file, row.names = FALSE)
      },contentType = "text/csv"
    )

    output$downloadData2 <-       

    downloadHandler(

      filename = "result.xlsx",
      content = function(file) {
        
        write.xlsx(data_p, file, sheetName="Sheet1")
        
      }, contentType = "text/xlsx"
      
    )

})
