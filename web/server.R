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
library(dplyr)
library(erer)
library(xlsx)
library(scales)
library(plyr) 
library(shinyjs)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {

  v <- reactiveValues(doPlot = FALSE)
  Treatment <- reactiveValues(new_cureF0= c(0.1,0.1,0.1,0.1,0.1,0.1),
                              new_cureF1= c(0.1,0.1,0.1,0.1,0.1,0.1),
                              new_cureF2 = c(0.1,0.1,0.1,0.1,0.1,0.1),
                              new_cureF3 = c(0.1,0.1,0.1,0.1,0.1,0.1),
                              new_cureC1 = c(0.1,0.1,0.1,0.1,0.1,0.1),
                              new_cureC2 = c(0.1,0.1,0.1,0.1,0.1,0.1),
                              new_cureC3 = c(0.1,0.1,0.1,0.1,0.1,0.1),
                              new_cureC4 = c(0.1,0.1,0.1,0.1,0.1,0.1))
  
  observeEvent(input$Treatment, {
    if(input$Treatment == 1){
      Treatment$new_cureF0 <- c(0.1,0.2,0.3,0.4,0.5,0.6)
      Treatment$new_cureF1 <- c(0.3,0.2,0.3,0.4,0.5,0.6)
      Treatment$new_cureF2 <- c(0.1,0.2,0.3,0.4,0.5,0.6)
      Treatment$new_cureF3 <- c(0.1,0.2,0.5,0.4,0.5,0.6)
      Treatment$new_cureC1 <- c(0.1,0.2,0.3,0.4,0.5,0.6)
      Treatment$new_cureC2 <- c(0.1,0.2,0.3,0.8,0.5,0.6)
      Treatment$new_cureC3 <- c(0.1,0.2,0.3,0.4,0.2,0.6)
      Treatment$new_cureC4 <- c(0.1,0.2,0.3,0.4,0.5,0.4)
      
    }else if(input$Treatment == 2){
      Treatment$new_cureF0 <- c(0.2,0.2,0.3,0.4,0.5,0.6)
      Treatment$new_cureF1 <- c(0.3,0.2,0.3,0.4,0.5,0.6)
      Treatment$new_cureF2 <- c(0.2,0.2,0.3,0.4,0.5,0.6)
      Treatment$new_cureF3 <- c(0.2,0.2,0.5,0.4,0.5,0.6)
      Treatment$new_cureC1 <- c(0.2,0.2,0.3,0.4,0.5,0.6)
      Treatment$new_cureC2 <- c(0.2,0.2,0.3,0.8,0.5,0.6)
      Treatment$new_cureC3 <- c(0.2,0.2,0.3,0.4,0.2,0.6)
      Treatment$new_cureC4 <- c(0.2,0.2,0.3,0.4,0.5,0.4)
    }else if(input$Treatment == 3){
      Treatment$new_cureF0 <- c(0.3,0.3,0.3,0.4,0.5,0.6)
      Treatment$new_cureF1 <- c(0.3,0.3,0.3,0.4,0.5,0.6)
      Treatment$new_cureF2 <- c(0.3,0.3,0.3,0.4,0.5,0.6)
      Treatment$new_cureF3 <- c(0.3,0.3,0.5,0.4,0.5,0.6)
      Treatment$new_cureC1 <- c(0.3,0.3,0.3,0.4,0.5,0.6)
      Treatment$new_cureC2 <- c(0.3,0.3,0.3,0.8,0.5,0.6)
      Treatment$new_cureC3 <- c(0.3,0.3,0.3,0.4,0.3,0.6)
      Treatment$new_cureC4 <- c(0.3,0.3,0.3,0.4,0.5,0.4)
    }else if(input$Treatment == 4){
      Treatment$new_cureF0 <- c(0.4,0.4,0.4,0.4,0.5,0.6)
      Treatment$new_cureF1 <- c(0.4,0.4,0.4,0.4,0.5,0.6)
      Treatment$new_cureF2 <- c(0.4,0.4,0.4,0.4,0.5,0.6)
      Treatment$new_cureF3 <- c(0.4,0.4,0.5,0.4,0.5,0.6)
      Treatment$new_cureC1 <- c(0.4,0.4,0.4,0.4,0.5,0.6)
      Treatment$new_cureC2 <- c(0.4,0.4,0.4,0.8,0.5,0.6)
      Treatment$new_cureC3 <- c(0.4,0.4,0.4,0.4,0.4,0.6)
      Treatment$new_cureC4 <- c(0.4,0.4,0.4,0.4,0.5,0.4)
    }else if(input$Treatment == 5){
      Treatment$new_cureF0 <- c(0.5,0.5,0.5,0.5,0.5,0.6)
      Treatment$new_cureF1 <- c(0.5,0.5,0.5,0.5,0.5,0.6)
      Treatment$new_cureF2 <- c(0.5,0.5,0.5,0.5,0.5,0.6)
      Treatment$new_cureF3 <- c(0.5,0.5,0.5,0.5,0.5,0.6)
      Treatment$new_cureC1 <- c(0.5,0.5,0.5,0.5,0.5,0.6)
      Treatment$new_cureC2 <- c(0.5,0.5,0.5,0.8,0.5,0.6)
      Treatment$new_cureC3 <- c(0.5,0.5,0.5,0.5,0.5,0.6)
      Treatment$new_cureC4 <- c(0.5,0.5,0.5,0.5,0.5,0.5)
    }
    else{
      Treatment$new_cureF0 <- c(0,0,0,0,0,0)
      Treatment$new_cureF1 <- c(0,0,0,0,0,0)
      Treatment$new_cureF2 <- c(0,0,0,0,0,0)
      Treatment$new_cureF3 <- c(0,0,0,0,0,0)
      Treatment$new_cureC1 <- c(0,0,0,0,0,0)
      Treatment$new_cureC2 <- c(0,0,0,0,0,0)
      Treatment$new_cureC3 <- c(0,0,0,0,0,0)
      Treatment$new_cureC4 <- c(0,0,0,0,0,0)
    }
    
  })
  
  options(scipen=6)
  setwd("C:/Hep-c/main/web")
  sourceCpp('p1_scenario.cpp')
  
  #Show and hide
  observe({
    toggle(id = "A_G", condition = input$Anin_genotype)
  })
  
  observe({
    toggle(id = "A_nonG", condition = !input$Anin_genotype)
  })
  
  observe({
    toggle(id = "Pie_G", condition = input$showgenotype_pie)
  })
  
  observe({
    toggle(id = "Pie_nonG", condition = !input$showgenotype_pie)
  })
  
  observe({
    toggle(id = "Age_screen", condition = (input$screening == 1))
  })
  
  observe({
    toggle(id = "Risk_group", condition = (input$screening == 2))
  })
  
  observe({
    toggle(id = "HCVDes", condition = (input$care == 1))
  })
  
  observe({
    toggle(id = "FibDes", condition = (input$care == 2))
  })
  observe({
    toggle(id = "drugImg1", condition = (input$Treatment == 1))
  })
  observe({
    toggle(id = "drugImg2", condition = (input$Treatment == 2))
  })
  observe({
    toggle(id = "drugImg3", condition = (input$Treatment == 3))
  })
  observe({
    toggle(id = "drugImg4", condition = (input$Treatment == 4))
  })
  observe({
    toggle(id = "drugImg5", condition = (input$Treatment == 5))
  })


  
  
  observeEvent(input$Treatment, {
    v$doPlot <- FALSE
  })  
  
  #Enable/Disable radio
  observeEvent(input$test1, {
    toggleState("test2", input$test1 == 1)
  })
  
  observeEvent(input$button, {
    if( v$doPlot == TRUE) v$doPlot <- FALSE
    v$doPlot <- TRUE
    
  })
  
  
  
  #parameter 
  p_t <- reactiveValues(S_screening = 0,
                        Pos_T = 0 , #Positive True
                        Pos_F = 0 , #Positive False
                        Neg_T = 0 , #Negative True
                        Neg_T = 0   #Negative False
                        )
  load("popscreen_list.RData")
  
  #Choosing groups (when Choosing age , risk group)
  observeEvent(c(input$screening,input$age_s,input$risk_g ),{
      
      if(input$screening == 1){ #By age
        
          if(input$age_s == 1){# 40-50 Years
            
            p_t$S_screening <- popscreen_list$ag1
            
          }else if(input$age_s == 2){# 50-60 Years
            
            p_t$S_screening  <- popscreen_list$ag2
            
          }else if(input$age_s == 3){# 40-60 Years
            
            p_t$S_screening  <- popscreen_list$ag3
            
          }
        
      }else if(input$screening == 2){ #risk group
        
        if(input$risk_g == 1){# HIV
          
          p_t$S_screening  <- popscreen_list$risk1
          
        }else if(input$risk_g == 2){# IDU
          
          p_t$S_screening  <- popscreen_list$risk2
          
        }else if(input$risk_g == 3){# MSM
          
          p_t$S_screening  <- popscreen_list$risk3
          
        }else if(input$risk_g == 4){# Blood donate
          
          p_t$S_screening  <- popscreen_list$risk4
          
        }else if(input$risk_g == 5){# Prisoner
          
          p_t$S_screening  <- popscreen_list$risk5
          
        }
      }

    }
  )
  
  #calculate Test + Test 2 + Link to care
  load("Test_list.RData") # load list Test_cost_sen_spe.RData ("Cost per test","Sensitivity","Specificity")
  
  
  #Choosing test (when Choosing test 1st ,test 2nd, link to care)
  observeEvent(c(input$test1,input$test2,input$care), {
    if(input$test1 == 1){#Test 1st(Ant HCV)
      
      if(input$test2 == 1){ #Test 2nd(RNA)
        
        if(input$care == 1){ #Link to care (HCV genotype testing)
          
          p_t$Pos_T <- p_t$S_screening * Test_list$Ant_HCV[2] * Test_list$RNA[2] * Test_list$Genotype[2]                #Sensitivity
          
          p_t$Pos_F <- p_t$S_screening * (1-Test_list$Ant_HCV[2]) * (1-Test_list$RNA[2]) * (1-Test_list$Genotype[2])    #Sensitivity
          
          p_t$Neg_T <- p_t$S_screening * Test_list$Ant_HCV[3] * Test_list$RNA[3] * Test_list$Genotype[3]                #Specificity
          
          p_t$Neg_F <- p_t$S_screening * (1-Test_list$Ant_HCV[3]) * (1-Test_list$RNA[3]) * (1-Test_list$Genotype[3])    #Specificity
          
        }else if(input$care == 2){ #Link to care (fibroscan stiffness score)
          
          p_t$Pos_T <- p_t$S_screening * Test_list$Ant_HCV[2] * Test_list$RNA[2] * Test_list$Fiboscan[2]                #Sensitivity
          
          p_t$Pos_F <- p_t$S_screening * (1-Test_list$Ant_HCV[2]) * (1-Test_list$RNA[2]) * (1-Test_list$Fiboscan[2])    #Sensitivity
          
          p_t$Neg_T <- p_t$S_screening * Test_list$Ant_HCV[3] * Test_list$RNA[3] * Test_list$Fiboscan[3]                #Specificity
          
          p_t$Neg_F <- p_t$S_screening * (1-Test_list$Ant_HCV[3]) * (1-Test_list$RNA[3]) * (1-Test_list$Fiboscan[3])    #Specificity
          
        }
        
        
      }else if (input$test2 == 2){ #Test 2nd (CORE Antigen)
        
        if(input$care == 1){ #Link to care (HCV genotype testing)
          
          p_t$Pos_T <- p_t$S_screening * Test_list$Ant_HCV[2] * Test_list$CORE_Antigen[2] * Test_list$Genotype[2]                #Sensitivity
          
          p_t$Pos_F <- p_t$S_screening * (1-Test_list$Ant_HCV[2]) * (1-Test_list$CORE_Antigen[2]) * (1-Test_list$Genotype[2])    #Sensitivity
          
          p_t$Neg_T <- p_t$S_screening * Test_list$Ant_HCV[3] * Test_list$CORE_Antigen[3] * Test_list$Genotype[3]                #Specificity
          
          p_t$Neg_F <- p_t$S_screening * (1-Test_list$Ant_HCV[3]) * (1-Test_list$CORE_Antigen[3]) * (1-Test_list$Genotype[3])    #Specificity
          
        }else if(input$care == 2){ #Link to care (fibroscan stiffness score)
          
          p_t$Pos_T <- p_t$S_screening * Test_list$Ant_HCV[2] * Test_list$CORE_Antigen[2] * Test_list$Fiboscan[2]                #Sensitivity
          
          p_t$Pos_F <- p_t$S_screening * (1-Test_list$Ant_HCV[2]) * (1-Test_list$CORE_Antigen[2]) * (1-Test_list$Fiboscan[2])    #Sensitivity
          
          p_t$Neg_T <- p_t$S_screening * Test_list$Ant_HCV[3] * Test_list$CORE_Antigen[3] * Test_list$Fiboscan[3]                #Specificity
          
          p_t$Neg_F <- p_t$S_screening * (1-Test_list$Ant_HCV[3]) * (1-Test_list$CORE_Antigen[3]) * (1-Test_list$Fiboscan[3])    #Specificity
          
        }
      }
      else if(input$test2 == 3){ #Test 2nd (Rapid HCV RNA)
        
        if(input$care == 1){ #Link to care (HCV genotype testing)
          
          p_t$Pos_T <- p_t$S_screening * Test_list$Ant_HCV[2] * Test_list$Rapid_HCV_RNA[2] * Test_list$Genotype[2]                #Sensitivity
          
          p_t$Pos_F <- p_t$S_screening * (1-Test_list$Ant_HCV[2]) * (1-Test_list$Rapid_HCV_RNA[2]) * (1-Test_list$Genotype[2])    #Sensitivity
          
          p_t$Neg_T <- p_t$S_screening * Test_list$Ant_HCV[3] * Test_list$Rapid_HCV_RNA[3] * Test_list$Genotype[3]                #Specificity
          
          p_t$Neg_F <- p_t$S_screening * (1-Test_list$Ant_HCV[3]) * (1-Test_list$Rapid_HCV_RNA[3]) * (1-Test_list$Genotype[3])    #Specificity
          
        }else if(input$care == 2){ #Link to care (fibroscan stiffness score)
          
          p_t$Pos_T <- p_t$S_screening * Test_list$Ant_HCV[2] * Test_list$Rapid_HCV_RNA[2] * Test_list$Fiboscan[2]                #Sensitivity
          
          p_t$Pos_F <- p_t$S_screening * (1-Test_list$Ant_HCV[2]) * (1-Test_list$Rapid_HCV_RNA[2]) * (1-Test_list$Fiboscan[2])    #Sensitivity
          
          p_t$Neg_T <- p_t$S_screening * Test_list$Ant_HCV[3] * Test_list$Rapid_HCV_RNA[3] * Test_list$Fiboscan[3]                #Specificity
          
          p_t$Neg_F <- p_t$S_screening * (1-Test_list$Ant_HCV[3]) * (1-Test_list$Rapid_HCV_RNA[3]) * (1-Test_list$Fiboscan[3])    #Specificity
          
        }
      }
      
    }else if(input$test1 == 2){ #Test 1st (Rapid HCV RNA)
      
        if(input$care == 1){ #Link to care (HCV genotype testing)
          
          p_t$Pos_T <- p_t$S_screening * Test_list$Rapid_HCV_RNA[2] * Test_list$Genotype[2]            #Sensitivity
            
          p_t$Pos_F <- p_t$S_screening * (1-Test_list$Rapid_HCV_RNA[2]) * (1-Test_list$Genotype[2])    #Sensitivity
          
          p_t$Neg_T <- p_t$S_screening * Test_list$Rapid_HCV_RNA[3] * Test_list$Genotype[3]            #Specificity
          
          p_t$Neg_F <- p_t$S_screening * (1-Test_list$Rapid_HCV_RNA[3]) * (1-Test_list$Genotype[3])    #Specificity
            
        }else if(input$care == 2){#Link to care (fibroscan stiffness score)
          
          p_t$Pos_T <- p_t$S_screening * Test_list$Rapid_HCV_RNA[2] * Test_list$Fiboscan[2]            #Sensitivity
          
          p_t$Pos_F <- p_t$S_screening * (1-Test_list$Rapid_HCV_RNA[2]) * (1-Test_list$Fiboscan[2])    #Sensitivity
          
          p_t$Neg_T <- p_t$S_screening * Test_list$Rapid_HCV_RNA[3] * Test_list$Fiboscan[3]            #Specificity
          
          p_t$Neg_F <- p_t$S_screening * (1-Test_list$Rapid_HCV_RNA[3]) * (1-Test_list$Fiboscan[3])    #Specificity
          
        }
    }
  }
  )
  
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
      std_cureF0=c(0,0,0,0,0,0),
      std_cureF1 =c(0,0,0,0,0,0),
      std_cureF2 =c(0,0,0,0,0,0),
      std_cureF3 =c(0,0,0,0,0,0),
      std_cureC1 =c(0,0,0,0,0,0),
      std_cureC2=c(0,0,0,0,0,0),
      #Novel treatment response based on genotype (weighted average)
      new_cureF0= c(0.985,0.985,0.985,0.985,0.985,0.985),
      new_cureF1= c(0.985,0.985,0.985,0.985,0.985,0.985),
      new_cureF2 = c(0.985,0.985,0.985,0.985,0.985,0.985),
      new_cureF3 = c(0.985,0.985,0.985,0.985,0.985,0.985),
      new_cureC1 = c(0.985,0.985,0.985,0.985,0.985,0.985),
      new_cureC2 = c(0.985,0.985,0.985,0.985,0.985,0.985),
      new_cureC3 = c(0.985,0.985,0.985,0.985,0.985,0.985),
      new_cureC4 = c(0.985,0.985,0.985,0.985,0.985,0.985),
      
      std_dist = c(0.05,0.05,0.3,0.3,0.3)
    )
  })
  


    S <-reactive({
      parms()$S0 * parms()$P0
    })

  
  inits <- reactive({ 
      c(
        S_g1 = S(), #;0.01*#pop_since1960(TIME=1)
        S_g2 = S(),
        S_g3 = S(),
        S_g4 = S(),
        S_g5 = S(),
        S_g6 = S(),
        
        F0_g1 = 0.2825*S(),
        F0_g2 = 0.2825*S(),
        F0_g3 = 0.2825*S(),
        F0_g4 = 0.2825*S(),
        F0_g5 = 0.2825*S(),
        F0_g6 = 0.2825*S(),
        
        F1_g1 = 0.2825*S(),
        F1_g2 = 0.2825*S(),
        F1_g3 = 0.2825*S(),
        F1_g4 = 0.2825*S(),
        F1_g5 = 0.2825*S(),
        F1_g6 = 0.2825*S(),
        
        F2_g1 = 0.184*S(),
        F2_g2 = 0.184*S(),
        F2_g3 = 0.184*S(),
        F2_g4 = 0.184*S(),
        F2_g5 = 0.184*S(),
        F2_g6 = 0.184*S(),
        
        F3_g1 = 0.124*S(),
        F3_g2 = 0.124*S(),
        F3_g3 = 0.124*S(),
        F3_g4 = 0.124*S(),
        F3_g5 = 0.124*S(),
        F3_g6 = 0.124*S(),
        
        # ;CirA
        C1_g1 = 0.03175*S(),
        C1_g2 = 0.03175*S(),
        C1_g3 = 0.03175*S(),
        C1_g4 = 0.03175*S(),
        C1_g5 = 0.03175*S(),
        C1_g6 = 0.03175*S(),
        
        # ;CirA
        C2_g1 = 0.03175*S(),
        C2_g2 = 0.03175*S(),
        C2_g3 = 0.03175*S(),
        C2_g4 = 0.03175*S(),
        C2_g5 = 0.03175*S(),
        C2_g6 = 0.03175*S(),
        
        # ;CirB
        C3_g1 = 0.03175*S(),
        C3_g2 = 0.03175*S(),
        C3_g3 = 0.03175*S(),
        C3_g4 = 0.03175*S(),
        C3_g5 = 0.03175*S(),
        C3_g6 = 0.03175*S(),
        
        # ;CirC
        C4_g1 = 0.03175*S(),
        C4_g2 = 0.03175*S(),
        C4_g3 = 0.03175*S(),
        C4_g4 = 0.03175*S(),
        C4_g5 = 0.03175*S(),
        C4_g6 = 0.03175*S(),
        
        HCC_A_g1 = 0,
        HCC_A_g2 = 0,
        HCC_A_g3 = 0,
        HCC_A_g4 = 0,
        HCC_A_g5 = 0,
        HCC_A_g6 = 0,
        
        HCC_B_g1 = 0,
        HCC_B_g2 = 0,
        HCC_B_g3 = 0,
        HCC_B_g4 = 0,
        HCC_B_g5 = 0,
        HCC_B_g6 = 0,
        
        HCC_C_g1 = 0,
        HCC_C_g2 = 0,
        HCC_C_g3 = 0,
        HCC_C_g4 = 0,
        HCC_C_g5 = 0,
        HCC_C_g6 = 0,
        
        HCC_D_g1 = 0,
        HCC_D_g2 = 0,
        HCC_D_g3 = 0,
        HCC_D_g4 = 0,
        HCC_D_g5 = 0,
        HCC_D_g6 = 0,
        
        C1std_cured_g1 = 0,
        C1std_cured_g2 = 0,
        C1std_cured_g3 = 0,
        C1std_cured_g4 = 0,
        C1std_cured_g5 = 0,
        C1std_cured_g6 = 0,
        
        C1new_cured_g1 = 0,
        C1new_cured_g2 = 0,
        C1new_cured_g3 = 0,
        C1new_cured_g4 = 0,
        C1new_cured_g5 = 0,
        C1new_cured_g6 = 0,
        
        C2new_cured_g1 = 0,
        C2new_cured_g2 = 0,
        C2new_cured_g3 = 0,
        C2new_cured_g4 = 0,
        C2new_cured_g5 = 0,
        C2new_cured_g6 = 0,
        
        C3new_cured_g1 = 0,
        C3new_cured_g2 = 0,
        C3new_cured_g3 = 0,
        C3new_cured_g4 = 0,
        C3new_cured_g5 = 0,
        C3new_cured_g6 = 0,
        
        C4new_cured_g1 = 0,
        C4new_cured_g2 = 0,
        C4new_cured_g3 = 0,
        C4new_cured_g4 = 0,
        C4new_cured_g5 = 0,
        C4new_cured_g6 = 0,
        
        death_g1 = 0,
        death_g2 = 0,
        death_g3 = 0,
        death_g4 = 0,
        death_g5 = 0,
        death_g6 = 0,
        
        deathHCC_g1 = 0,
        deathHCC_g2 = 0,
        deathHCC_g3 = 0,
        deathHCC_g4 = 0,
        deathHCC_g5 = 0,
        deathHCC_g6 = 0,
        
        deathC14_g1 = 0,
        deathC14_g2 = 0,
        deathC14_g3 = 0,
        deathC14_g4 = 0,
        deathC14_g5 = 0,
        deathC14_g6 = 0
      
    )
  })
  
  
  
  out_df <- reactive({

    times_bf <- seq(1999, 2018, by = 0.01)
    
    
    out_bf <- ode( y = inits(),times =  times_bf, func = PanHepC, parms = parms(), method = "rk4")
    #update inits
    out_bf_df <-as.data.frame(out_bf)
    out_bf_lastRow <- tail(out_bf_df,1)
    
    Pos_T_div6 <- p_t$Pos_T/6
    #continuous inits
    inits_con <- reactive({ 
      c(
        S_g1 = out_bf_lastRow$S_g1 +Pos_T_div6, #;0.01*#pop_since1960(TIME=1)
        S_g2 = out_bf_lastRow$S_g2 +Pos_T_div6,
        S_g3 = out_bf_lastRow$S_g3 +Pos_T_div6,
        S_g4 = out_bf_lastRow$S_g4 +Pos_T_div6,
        S_g5 = out_bf_lastRow$S_g5 +Pos_T_div6,
        S_g6 = out_bf_lastRow$S_g6 +Pos_T_div6,
        
        F0_g1 = out_bf_lastRow$F0_g1,
        F0_g2 = out_bf_lastRow$F0_g2,
        F0_g3 = out_bf_lastRow$F0_g3,
        F0_g4 = out_bf_lastRow$F0_g4,
        F0_g5 = out_bf_lastRow$F0_g5,
        F0_g6 = out_bf_lastRow$F0_g6,
        
        F1_g1 = out_bf_lastRow$F1_g1,
        F1_g2 = out_bf_lastRow$F1_g2,
        F1_g3 = out_bf_lastRow$F1_g3,
        F1_g4 = out_bf_lastRow$F1_g4,
        F1_g5 = out_bf_lastRow$F1_g5,
        F1_g6 = out_bf_lastRow$F1_g6,
        
        F2_g1 = out_bf_lastRow$F2_g1,
        F2_g2 = out_bf_lastRow$F2_g2,
        F2_g3 = out_bf_lastRow$F2_g3,
        F2_g4 = out_bf_lastRow$F2_g4,
        F2_g5 = out_bf_lastRow$F2_g5,
        F2_g6 = out_bf_lastRow$F2_g6,
        
        F3_g1 = out_bf_lastRow$F3_g1,
        F3_g2 = out_bf_lastRow$F3_g2,
        F3_g3 = out_bf_lastRow$F3_g3,
        F3_g4 = out_bf_lastRow$F3_g4,
        F3_g5 = out_bf_lastRow$F3_g5,
        F3_g6 = out_bf_lastRow$F3_g6,
        
        # ;CirA
        C1_g1 = out_bf_lastRow$C1_g1,
        C1_g2 = out_bf_lastRow$C1_g2,
        C1_g3 = out_bf_lastRow$C1_g3,
        C1_g4 = out_bf_lastRow$C1_g4,
        C1_g5 = out_bf_lastRow$C1_g5,
        C1_g6 = out_bf_lastRow$C1_g6,
        
        # ;CirA
        C2_g1 = out_bf_lastRow$C2_g1,
        C2_g2 = out_bf_lastRow$C2_g2,
        C2_g3 = out_bf_lastRow$C2_g3,
        C2_g4 = out_bf_lastRow$C2_g4,
        C2_g5 = out_bf_lastRow$C2_g5,
        C2_g6 = out_bf_lastRow$C2_g6,
        
        # ;CirB
        C3_g1 = out_bf_lastRow$C3_g1,
        C3_g2 = out_bf_lastRow$C3_g2,
        C3_g3 = out_bf_lastRow$C3_g3,
        C3_g4 = out_bf_lastRow$C3_g4,
        C3_g5 = out_bf_lastRow$C3_g5,
        C3_g6 = out_bf_lastRow$C3_g6,
        
        # ;CirC
        C4_g1 = out_bf_lastRow$C4_g1,
        C4_g2 = out_bf_lastRow$C4_g2,
        C4_g3 = out_bf_lastRow$C4_g3,
        C4_g4 = out_bf_lastRow$C4_g4,
        C4_g5 = out_bf_lastRow$C4_g5,
        C4_g6 = out_bf_lastRow$C4_g6,
        
        HCC_A_g1 = out_bf_lastRow$HCC_A_g1,
        HCC_A_g2 = out_bf_lastRow$HCC_A_g2,
        HCC_A_g3 = out_bf_lastRow$HCC_A_g3,
        HCC_A_g4 = out_bf_lastRow$HCC_A_g4,
        HCC_A_g5 = out_bf_lastRow$HCC_A_g5,
        HCC_A_g6 = out_bf_lastRow$HCC_A_g6,
        
        HCC_B_g1 = out_bf_lastRow$HCC_B_g1,
        HCC_B_g2 = out_bf_lastRow$HCC_B_g2,
        HCC_B_g3 = out_bf_lastRow$HCC_B_g3,
        HCC_B_g4 = out_bf_lastRow$HCC_B_g4,
        HCC_B_g5 = out_bf_lastRow$HCC_B_g5,
        HCC_B_g6 = out_bf_lastRow$HCC_B_g6,
        
        HCC_C_g1 = out_bf_lastRow$HCC_C_g1,
        HCC_C_g2 = out_bf_lastRow$HCC_C_g2,
        HCC_C_g3 = out_bf_lastRow$HCC_C_g3,
        HCC_C_g4 = out_bf_lastRow$HCC_C_g4,
        HCC_C_g5 = out_bf_lastRow$HCC_C_g5,
        HCC_C_g6 = out_bf_lastRow$HCC_C_g6,
        
        HCC_D_g1 = out_bf_lastRow$HCC_D_g1,
        HCC_D_g2 = out_bf_lastRow$HCC_D_g2,
        HCC_D_g3 = out_bf_lastRow$HCC_D_g3,
        HCC_D_g4 = out_bf_lastRow$HCC_D_g4,
        HCC_D_g5 = out_bf_lastRow$HCC_D_g5,
        HCC_D_g6 = out_bf_lastRow$HCC_D_g6,
        
        C1std_cured_g1 = out_bf_lastRow$C1std_cured_g1,
        C1std_cured_g2 = out_bf_lastRow$C1std_cured_g2,
        C1std_cured_g3 = out_bf_lastRow$C1std_cured_g3,
        C1std_cured_g4 = out_bf_lastRow$C1std_cured_g4,
        C1std_cured_g5 = out_bf_lastRow$C1std_cured_g5,
        C1std_cured_g6 = out_bf_lastRow$C1std_cured_g6,
        
        C1new_cured_g1 = out_bf_lastRow$C1new_cured_g1,
        C1new_cured_g2 = out_bf_lastRow$C1new_cured_g2,
        C1new_cured_g3 = out_bf_lastRow$C1new_cured_g3,
        C1new_cured_g4 = out_bf_lastRow$C1new_cured_g4,
        C1new_cured_g5 = out_bf_lastRow$C1new_cured_g5,
        C1new_cured_g6 = out_bf_lastRow$C1new_cured_g6,
        
        C2new_cured_g1 = out_bf_lastRow$C2new_cured_g1,
        C2new_cured_g2 = out_bf_lastRow$C2new_cured_g2,
        C2new_cured_g3 = out_bf_lastRow$C2new_cured_g3,
        C2new_cured_g4 = out_bf_lastRow$C2new_cured_g4,
        C2new_cured_g5 = out_bf_lastRow$C2new_cured_g5,
        C2new_cured_g6 = out_bf_lastRow$C2new_cured_g6,
        
        C3new_cured_g1 = out_bf_lastRow$C3new_cured_g1,
        C3new_cured_g2 = out_bf_lastRow$C3new_cured_g2,
        C3new_cured_g3 = out_bf_lastRow$C3new_cured_g3,
        C3new_cured_g4 = out_bf_lastRow$C3new_cured_g4,
        C3new_cured_g5 = out_bf_lastRow$C3new_cured_g5,
        C3new_cured_g6 = out_bf_lastRow$C3new_cured_g6,
        
        C4new_cured_g1 = out_bf_lastRow$C4new_cured_g1,
        C4new_cured_g2 = out_bf_lastRow$C4new_cured_g2,
        C4new_cured_g3 = out_bf_lastRow$C4new_cured_g3,
        C4new_cured_g4 = out_bf_lastRow$C4new_cured_g4,
        C4new_cured_g5 = out_bf_lastRow$C4new_cured_g5,
        C4new_cured_g6 = out_bf_lastRow$C4new_cured_g6,
        
        death_g1 = out_bf_lastRow$death_g1,
        death_g2 = out_bf_lastRow$death_g2,
        death_g3 = out_bf_lastRow$death_g3,
        death_g4 = out_bf_lastRow$death_g4,
        death_g5 = out_bf_lastRow$death_g5,
        death_g6 = out_bf_lastRow$death_g6,
        
        deathHCC_g1 = out_bf_lastRow$deathHCC_g1,
        deathHCC_g2 = out_bf_lastRow$deathHCC_g2,
        deathHCC_g3 = out_bf_lastRow$deathHCC_g3,
        deathHCC_g4 = out_bf_lastRow$deathHCC_g4,
        deathHCC_g5 = out_bf_lastRow$deathHCC_g5,
        deathHCC_g6 = out_bf_lastRow$deathHCC_g6,
        
        deathC14_g1 = out_bf_lastRow$deathC14_g1,
        deathC14_g2 = out_bf_lastRow$deathC14_g2,
        deathC14_g3 = out_bf_lastRow$deathC14_g3,
        deathC14_g4 = out_bf_lastRow$deathC14_g4,
        deathC14_g5 = out_bf_lastRow$deathC14_g5,
        deathC14_g6 = out_bf_lastRow$deathC14_g6
        
      )
    })
    #out_at
    times_at <- seq(2018.01, 2035, by = 0.01)
    out_at <- ode( y = inits_con(),times =  times_at, func = PanHepC, parms = parms(), method = "rk4")
    
    out_at_df <-as.data.frame(out_at)
    
    out_df <- rbind(out_bf_df,out_at_df)
    
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
    out <- ode( y = cost_per_year,times =  cost_times, func = cost_func, parms = Treatment$cost, method = "rk4")
    cost_plot <- as.data.frame(out)
    colnames(cost_plot)[2] <- "Total_Cost"
    cost_plot
  })
  


    #output 1
    output$distPlot <- renderPlot({
      if (v$doPlot == FALSE) return()
      x <- select(out_df(), time,total_prev)
      if(input$bygenotype){
        x <- select(out_df(),time,contains("prev"))
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
      x <- select(out_df(), time , total_new_death)
      
      if(input$showgenotype){
        
        x <- select(out_df(), time,contains("death"))
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
      x <- select(out_df(), time , contains("std_cured"),contains("new_cured")) %>%
              transmute( time =time,
                       C1std_cured_total = C1std_cured_g1+C1std_cured_g2+C1std_cured_g3+C1std_cured_g4+C1std_cured_g5+C1std_cured_g6,
                       C1new_cured_total = C1new_cured_g1+C1new_cured_g2+C1new_cured_g3+C1new_cured_g4+C1new_cured_g5+C1new_cured_g6,
                       C2new_cured_total = C2new_cured_g1+C2new_cured_g2+C2new_cured_g3+C2new_cured_g4+C2new_cured_g5+C2new_cured_g6,
                       C3new_cured_total = C3new_cured_g1+C3new_cured_g2+C3new_cured_g3+C3new_cured_g4+C3new_cured_g5+C3new_cured_g6,
                       C4new_cured_total = C4new_cured_g1+C4new_cured_g2+C4new_cured_g3+C4new_cured_g4+C4new_cured_g5+C4new_cured_g6
                       )
      
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
      x <- select(out_df(), time , contains("C1std_cured"),contains("C1new_cured")) %>%
              mutate(
                C1std_cured_total = C1std_cured_g1+C1std_cured_g2+C1std_cured_g3+C1std_cured_g4+C1std_cured_g5+C1std_cured_g6,
                C1new_cured_total = C1new_cured_g1+C1new_cured_g2+C1new_cured_g3+C1new_cured_g4+C1new_cured_g5+C1new_cured_g6
              )
      
      isolate({
        ggplot(data = x,aes(time)) + 
          #C1new
          geom_line(aes(y = C1new_cured_total,color="C1"),size = 1.5)+
          geom_line(aes(y = C1new_cured_g1,color="C1_G1"),size = 1.5)+
          geom_line(aes(y = C1new_cured_g2,color="C1_G2"),size = 1.5)+
          geom_line(aes(y = C1new_cured_g3,color="C1_G3"),size = 1.5)+
          geom_line(aes(y = C1new_cured_g4,color="C1_G4"),size = 1.5)+
          geom_line(aes(y = C1new_cured_g5,color="C1_G5"),size = 1.5)+
          geom_line(aes(y = C1new_cured_g6,color="C1_G6"),size = 1.5)+
          #C1std
          geom_line(aes(y = C1std_cured_total,color="C1"),linetype=2,size = 1.5)+
          geom_line(aes(y = C1std_cured_g1 ,color="C1_G1"),linetype=2,size = 1.5)+
          geom_line(aes(y = C1std_cured_g2 ,color="C1_G2"),linetype=2,size = 1.5)+
          geom_line(aes(y = C1std_cured_g3 ,color="C1_G3"),linetype=2,size = 1.5)+
          geom_line(aes(y = C1std_cured_g4 ,color="C1_G4"),linetype=2,size = 1.5)+
          geom_line(aes(y = C1std_cured_g5 ,color="C1_G5"),linetype=2,size = 1.5)+
          geom_line(aes(y = C1std_cured_g6 ,color="C1_G6"),linetype=2,size = 1.5)+
          ylab("value")+
          scale_x_continuous(breaks=seq(input$year[1], input$year[2], 1),limits = c(input$year[1],input$year[2]))
      })
      
    })
    
    #output 3 with genotype C2
    output$Anin_G_C2_Plot <- renderPlot({
      if (v$doPlot == FALSE) return()
      x <- select(out_df(), time ,contains("C2new_cured")) %>%
        mutate(
          C2new_cured_total = C2new_cured_g1+C2new_cured_g2+C2new_cured_g3+C2new_cured_g4+C2new_cured_g5+C2new_cured_g6
        )
      
      isolate({
        ggplot(data = x,aes(time)) + 
          #C1new
          geom_line(aes(y = C2new_cured_total,color="C2"),size = 1.5)+
          geom_line(aes(y = C2new_cured_g1,color="C2_G1"),size = 1.5)+
          geom_line(aes(y = C2new_cured_g2,color="C2_G2"),size = 1.5)+
          geom_line(aes(y = C2new_cured_g3,color="C2_G3"),size = 1.5)+
          geom_line(aes(y = C2new_cured_g4,color="C2_G4"),size = 1.5)+
          geom_line(aes(y = C2new_cured_g5,color="C2_G5"),size = 1.5)+
          geom_line(aes(y = C2new_cured_g6,color="C2_G6"),size = 1.5)+
          ylab("value")+
          scale_x_continuous(breaks=seq(input$year[1], input$year[2], 1),limits = c(input$year[1],input$year[2]))
      })
      
    })
    
    #output 3 with genotype C3
    output$Anin_G_C3_Plot <- renderPlot({
      if (v$doPlot == FALSE) return()
      x <- select(out_df(), time ,contains("C3new_cured")) %>%
        mutate(
          C3new_cured_total = C3new_cured_g1+C3new_cured_g2+C3new_cured_g3+C3new_cured_g4+C3new_cured_g5+C3new_cured_g6
        )
      
      isolate({
        ggplot(data = x,aes(time)) + 
          #C1new
          geom_line(aes(y = C3new_cured_total,color="C3"),size = 1.5)+
          geom_line(aes(y = C3new_cured_g1,color="C3_G1"),size = 1.5)+
          geom_line(aes(y = C3new_cured_g2,color="C3_G2"),size = 1.5)+
          geom_line(aes(y = C3new_cured_g3,color="C3_G3"),size = 1.5)+
          geom_line(aes(y = C3new_cured_g4,color="C3_G4"),size = 1.5)+
          geom_line(aes(y = C3new_cured_g5,color="C3_G5"),size = 1.5)+
          geom_line(aes(y = C3new_cured_g6,color="C3_G6"),size = 1.5)+
          ylab("value")+
          scale_x_continuous(breaks=seq(input$year[1], input$year[2], 1),limits = c(input$year[1],input$year[2]))
      })
      
    })
    
    #output 3 with genotype C4
    output$Anin_G_C4_Plot <- renderPlot({
      if (v$doPlot == FALSE) return()
      x <- select(out_df(), time ,contains("C4new_cured"))%>%
        mutate(
          C4new_cured_total = C4new_cured_g1+C4new_cured_g2+C4new_cured_g3+C4new_cured_g4+C4new_cured_g5+C4new_cured_g6
        )
      
      isolate({
        
        ggplot(data = x,aes(time)) + 
          #C1new
          geom_line(aes(y = C4new_cured_total,color="C4"),size = 1.5)+
          geom_line(aes(y = C4new_cured_g1,color="C4_G1"),size = 1.5)+
          geom_line(aes(y = C4new_cured_g2,color="C4_G2"),size = 1.5)+
          geom_line(aes(y = C4new_cured_g3,color="C4_G3"),size = 1.5)+
          geom_line(aes(y = C4new_cured_g4,color="C4_G4"),size = 1.5)+
          geom_line(aes(y = C4new_cured_g5,color="C4_G5"),size = 1.5)+
          geom_line(aes(y = C4new_cured_g6,color="C4_G6"),size = 1.5)+
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
    
    #piePlot
    output$piePlot <- renderPlot({
      
      if (v$doPlot == FALSE) return()
      x <-  select(out_df(), time ,propF0,propF1,propF2,propF3)
      isolate({
        
        
        #time year == input$year[2]
        x_time <- out_df()["time"] == input$year[2]
        x <- x[x_time,]
        
        x_melt <-melt(x, id="time")
        
        ggplot(x_melt, aes(x="", y=value, fill=variable))+
          geom_bar(width = 1, stat = "identity") + 
          coord_polar("y", start=0) +  scale_fill_brewer(palette="Blues") + theme_minimal() +  
          theme(axis.text.x=element_blank()) +
          geom_text(aes(label = percent(value)), size=3, position = position_stack(vjust = 0.5))
      })
    })
    
    #piePlotF0
    output$piePlot_F0 <- renderPlot({
      
      if (v$doPlot == FALSE) return()
      x <-  select(out_df(), time ,contains("propF0_genotype"))
      isolate({
        
        
        #time year == input$year[2]
        x_time <- out_df()["time"] == input$year[2]
        x <- x[x_time,]
        
        x_melt <-melt(x, id="time")
        
        ggplot(x_melt, aes(x="", y=value, fill=variable))+
          geom_bar(width = 1, stat = "identity") + 
          coord_polar("y", start=0) +  scale_fill_brewer(palette="Blues") + theme_minimal() + 
          theme(axis.text.x=element_blank()) +
          geom_text(aes(label = percent(value)), size=3, position = position_stack(vjust = 0.5))
      })
    })
    #piePlotF1
    output$piePlot_F1 <- renderPlot({
      
      if (v$doPlot == FALSE) return()
      x <-  select(out_df(), time ,contains("propF1_genotype"))
      isolate({
        
        
        #time year == input$year[2]
        x_time <- out_df()["time"] == input$year[2]
        x <- x[x_time,]
        
        x_melt <-melt(x, id="time")
        
        ggplot(x_melt, aes(x="", y=value, fill=variable))+
          geom_bar(width = 1, stat = "identity") + 
          coord_polar("y", start=0) +  scale_fill_brewer(palette="Blues") + theme_minimal() +  
          theme(axis.text.x=element_blank()) +
          geom_text(aes(label = percent(value)), size=3, position = position_stack(vjust = 0.5))
      })
    })
    
    #piePlotF2
    output$piePlot_F2 <- renderPlot({
      
      if (v$doPlot == FALSE) return()
      x <-  select(out_df(), time ,contains("propF2_genotype"))
      isolate({
        
        
        #time year == input$year[2]
        x_time <- out_df()["time"] == input$year[2]
        x <- x[x_time,]
        
        x_melt <-melt(x, id="time")
        
        ggplot(x_melt, aes(x="", y=value, fill=variable))+
          geom_bar(width = 1, stat = "identity") + 
          coord_polar("y", start=0) +  scale_fill_brewer(palette="Blues") + theme_minimal() +  
          theme(axis.text.x=element_blank()) +
          geom_text(aes(label = percent(value)), size=3, position = position_stack(vjust = 0.5))
      })
    })
    
    
    #piePlotF3
    output$piePlot_F3 <- renderPlot({
      
      if (v$doPlot == FALSE) return()
      x <-  select(out_df(), time ,contains("propF3_genotype"))
      isolate({
        
        
        #time year == input$year[2]
        x_time <- out_df()["time"] == input$year[2]
        x <- x[x_time,]
        
        x_melt <-melt(x, id="time")
        
        ggplot(x_melt, aes(x="", y=value, fill=variable))+
          geom_bar(width = 1, stat = "identity") + 
          coord_polar("y", start=0) +  scale_fill_brewer(palette="Blues") + theme_minimal() +  
          theme(axis.text.x=element_blank()) +
          geom_text(aes(label = percent(value)), size=3, position = position_stack(vjust = 0.5))
      })
    })
    
    
    #textoutput
    output$text1 <- renderText({
        paste("Treatment efficacy F0 :" , round(mean(Treatment$new_cureF0),2))
    })
    
    output$text2 <- renderText({
        paste("Treatment efficacy F1 :" , round(mean(Treatment$new_cureF1),2))
    })
    
    output$text3 <- renderText({
       paste("Treatment efficacy F2 :" , round(mean(Treatment$new_cureF2),2))
    })
    
    output$text4 <- renderText({
      paste("Treatment efficacy F3 :" , round(mean(Treatment$new_cureF3),2))
    })
    
    output$text5 <- renderText({
      paste("Treatment efficacy C1 :" , round(mean(Treatment$new_cureC1),2))
    })
    
    output$text6 <- renderText({
      paste("Treatment efficacy C2 :" , round(mean(Treatment$new_cureC2),2))
    })
    
    output$text7 <- renderText({
      paste("Treatment efficacy C3 :" , round(mean(Treatment$new_cureC3),2))
    })
    
    output$text8 <- renderText({
      paste("Treatment efficacy C4 :" , round(mean(Treatment$new_cureC4),2))
    })
    
    output$S_list <- renderPrint({
      print(p_t$Pos_F)
      print(p_t$Neg_T)
      print(p_t$Neg_F)
    })
    output$screening_p <- renderText({
      paste("screening people :" , (p_t$S_screening) )
    })
    
    output$Pos_T_Text <- renderText({
      paste("Positive True :" , (p_t$Pos_T) )
    })
    
    output$Pos_F_Text <- renderText({
      paste("Positive False :" , (p_t$Pos_F) )
    })
    
    output$Neg_T_Text <- renderText({
      paste("Negative True :" , (p_t$Neg_T) )
    })
    
    output$Neg_F_Text <- renderText({
      paste("Negative False :" , (p_t$Neg_F) )
    })
    
    output$downloadData <- 
      
      
      downloadHandler(
        
        filename = "result.xlsx",
        content = function(file) {

          write.xlsx(out_df_year(), file,sheetName="Result" ,row.names = FALSE)
          
        },
        contentType = "text/xlsx"
      )

    output$downloadparameter <-

      downloadHandler(
        filename = "parameter.csv",
        content = function(file) {
          
          write.list(parms(), file )
          
        },
        contentType = "text/csv"
      )
    

})
