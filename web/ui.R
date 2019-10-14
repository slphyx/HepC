#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyBS)
library(shinyjs)
library(tableHTML)

# Define UI for application that draws a histogram
shinyUI(fluidPage(  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),

  
  # Application title
  tags$div(class="header bg-primary",
  tags$h1(class="text-center","Hepatitis C Virus Modelling")
            ),
  tabsetPanel(
      #tab1
    tabPanel("Introduction",
             tags$h2("Introduction"),
             
             
             tags$p("The aim of this interactive web application is to help decision makers design the screening programme for treatment of chronic Hepatitis C infection.																						
                        The development of the model is based mainly on the Thai setting and Hepatitis C situations in Thai context. However, small adjustments of some parameters will allow the users to adapt the model to other settings.																						
                        This is the first version of the web application. The research team is hoping to further devlope this application to serve more purposes for strategic planning to prevent and control chronic hepatitis C infections nationally and internationally."),
             tags$h2("Rationale"),
             tags$p("Hepatitis C virus (HCV) infection is an important worldwide public health problem, and most
                          of the global HCV burden is in low- to middle-income countries. Mathematical model approach is used  
                          to study the disease dynamics and estimate the cost-effectiveness, given different types of screening methods, diagnosis and treatment strategies. 
                          In particular, we evaluate the benefits of using the new screening scheme, new diagnostic tests and 
                          novel direct-acting antiviral agents for treatment against the current standards."),
             tags$p("These advances have led to the potential of HCV treatment delivery to national public
                          health programs and decreased overall HCV-related morbidity and mortality [1]."),
             
             tags$h2("Study Objectives"),
             tags$p("1) To asess impacts and costs of using different screening and diagnosis schemes on morbidity and mortality of HCV infections in Thai setting"),
             tags$p("2) To assess impacts and costs of treating HCV patients (by measuring the incidence of CHC - related decompensated
                         cirrhosis and hepatocellular carcinoma (HCC) and mortality) wih different regimens including "),
             tags$ol(type = "a",
                     tags$li("Sofosbuvir with Peginterferon alfa type 2a or 2b and ribavirin (National List of Essential Medicines)"),
                     tags$li("Sofosbuvir with Ledipasvir (National List of Essential Medicines)"),
                     tags$li("Sofosbuvir with Daclatasvir (pan-genotypic treatments)"),
                     tags$li("Sofosbuvir with Velpatasvir (pan-genotypic treatments)"),
                     tags$li("Sofosbuvir with Ravidasvir (pan-genotypic treatments, on-going clinical trial)")
             ),
             
             tags$h2("Mathematical Modelling Approach"),
             tags$p("Based on our previous study looking at the coverage of novel direct-acting antiviral agents and allocation strategies in Poovorawan",tags$i(class = "italic","et. al."), 
                    "2016 [2], we extend the model to consider the process of screening HCV patients from the general population and applying different diagnostic tests."),
             tags$div(
               tags$img(src="image/disease progression model.png" 
                        ,alt="Study design of the transmission and disease progression model."),
               
               tags$p(class = "italic",tags$b("Fig 1. Study design of the transmission and disease progression model."), "The fibrosis stage of progression develops gradually, and cirrhosis and
                      decompensation develop over time. The risk of developing HCC starts after cirrhosis. Survival rates depend on the severity of disease at each stage. The
                      current standard treatment in Thailand was compared with new direct-acting antivirals with different treatment coverage and allocations. *HCV-related
                      mortality.")
             ),
             tags$hr(),
             tags$h2("Collaborators"),
             tags$h2("Funder"),
             tags$h2("Software"),
             tags$p("This App is built on the following open-source, free software:																									
                        R Core Team (2014). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. ; Winston Chang (2015). shiny: Web Application Framework for R. ; Leaflet, a JavaScript library for interactive maps ; Datable table plug-in for jQuery."),
             tags$h2("Contact"),
             tags$p("For questions on the interactive web application, please contact wirichada.pan@mahidol.ac.th"),
             tags$p("Further feedback and suggestion on this web application is greatly appreciated. Click here"),
             tags$hr(),
             tags$p("These advances have led to the potential of HCV treatment delivery to national public
                          health programs and decreased overall HCV-related morbidity and mortality [1]."),
             
             tags$hr(),
             tags$p("1.Kohli A, Shaffer A, Sherman A, Kottilil S. Treatment of hepatitis C: a systematic review. Jama. 2014;
                        312(6):631-40.  doi:", tags$a (href="http://dx.doi.org/10.1001/jama.2014.7085","10.1001/jama.2014.7085"), "PMID: 25117132."),
             tags$p("2.Poovorawan K, Pan-Ngum W, White LJ" , tags$i(class = "italic","et. al."),"Estimating the impact of expanding treatment coverage and allocation strategies for chronic Hepatitis C in a Direct Antiviral Agent Era.
                        PLoS One. 2016 Sep 15;11(9):e0163095. doi:doi: 10.1371/journal.pone.0163095. eCollection 2016" )
    ),
      
      #tab2
      tabPanel("Natural History Of Disease",
               tags$div(class = "sliderDisplay col-sm-12",
                        tags$h3("Populations and Transmission Coefficient"),
                        tags$hr(),
                        tags$div(class = "col-sm-4",
                                 sliderInput("P0",
                                             "Total population at the beginning",
                                             min = 10000000,
                                             max = 100000000,
                                             step = 1000000,
                                             value = 60000000
                                 )
                        ),
                        tags$div(class = "col-sm-4",
                                 sliderInput("K",
                                             "Maximum population",
                                             min = 10000000,
                                             max = 200000000,
                                             step = 1000000,
                                             value = 70000000
                                 )
                        ),
                        tags$div(class = "col-sm-4",
                                 sliderInput("r",
                                             "Population growth rate",
                                             min = 0.01,
                                             max = 0.5,
                                             step = 0.01,
                                             value = 0.16
                                 )
                        ),
                        tags$div(class = "col-sm-4",
                                 sliderInput("beta",
                                             "Transmission coefficient",
                                             min = 0.2,
                                             max = 0.6,
                                             step = 0.01,
                                             value = 0.32
                                 )
                        ),
                        tags$div(class = "col-sm-8",
                                 sliderInput("Fi",
                                             "Influx rate of the population to become susceptible per year",
                                             min = 0.001,
                                             max = 1,
                                             step = 0.001,
                                             value = 0.001
                                 )
                        ),

                        tags$h3("Progression of fibrosis"),
                        tags$hr(),
                        tags$div(class = "col-sm-4",
                                 sliderInput("f0f1",
                                             "Fibrosis stage F0 to F1",
                                             min = 0.05,
                                             max = 0.2,
                                             step = 0.001,
                                             value = 0.117
                                 )
                        ),
                        tags$div(class = "col-sm-4",
                                 sliderInput("f1f2",
                                             "Fibrosis stage F1 to F2",
                                             min = 0.05,
                                             max = 0.2,
                                             step = 0.001,
                                             value = 0.085
                                 )
                        ),
                        tags$div(class = "col-sm-4",
                                 sliderInput("f2f3",
                                             "Fibrosis stage F2 to F3",
                                             min = 0.05,
                                             max = 0.2,
                                             step = 0.001,
                                             value = 0.12
                                 )
                        ),
                        tags$div(class = "col-sm-12",
                                 sliderInput("f3c1",
                                             "Fibrosis stage F3 to Cirrhosis Child-Pugh class A",
                                             min = 0.05,
                                             max = 0.2,
                                             step = 0.001,
                                             value = 0.116
                                 )
                                 
                        ),
                        tags$h3("Progression of cirrhosis"),
                        tags$hr(),
                        tags$div(class = "col-sm-6",
                                 sliderInput("c1c2",
                                             "Cirrhosis Child-Pugh class A to B",
                                             min = 0.03,
                                             max = 0.1,
                                             step = 0.001,
                                             value = 0.044
                                 )
                        ),
                        tags$div(class = "col-sm-6",
                                 sliderInput("c2c3",
                                             "Cirrhosis Child-Pugh class B to C",
                                             min = 0.03,
                                             max = 0.1,
                                             step = 0.001,
                                             value = 0.076
                                 )
                        ),
                        tags$h3("Incidence of developing HCC"),
                        tags$hr(),
                        tags$div(class = "col-sm-4",
                                 sliderInput("c1bA",
                                             "Cirrhosis stage C1 to HCC_BCLC_A",
                                             min = 0.005,
                                             max = 0.01,
                                             step = 0.0001,
                                             value = 0.0068
                                 )
                        ),
                        tags$div(class = "col-sm-4",
                                 sliderInput("c2bA",
                                             "Cirrhosis stage C2 to HCC_BCLC_A",
                                             min = 0.005,
                                             max = 0.01,
                                             step = 0.0001,
                                             value = 0.0068
                                 )
                        ),
                        tags$div(class = "col-sm-4",
                                 sliderInput("c1bB",
                                             "Cirrhosis stage C1 to HCC_BCLC_B",
                                             min = 0.005,
                                             max = 0.01,
                                             step = 0.0001,
                                             value = 0.0099
                                 )
                        ),
                        tags$div(class = "col-sm-4",
                                 sliderInput("c2bB",
                                             "Cirrhosis stage C2 to HCC_BCLC_B",
                                             min = 0.005,
                                             max = 0.01,
                                             step = 0.0001,
                                             value = 0.0099
                                 )
                        ),
                        tags$div(class = "col-sm-4",
                                 sliderInput("c1bC",
                                             "Cirrhosis stage C1 to HCC_BCLC_C",
                                             min = 0.005,
                                             max = 0.01,
                                             step = 0.0001,
                                             value = 0.0029
                                 )
                        ),
                        tags$div(class = "col-sm-4",
                                 sliderInput("c2bC",
                                             "Cirrhosis stage C2 to HCC_BCLC_C",
                                             min = 0.005,
                                             max = 0.01,
                                             step = 0.0001,
                                             value = 0.0029
                                 )
                        ),
                        tags$div(class = "col-sm-4",
                                 sliderInput("c1bD",
                                             "Cirrhosis stage C1 to HCC_BCLC_D",
                                             min = 0.005,
                                             max = 0.01,
                                             step = 0.0001,
                                             value = 0.0068
                                 )
                        ),
                        tags$div(class = "col-sm-4",
                                 sliderInput("c2bD",
                                             "Cirrhosis stage C2 to HCC_BCLC_D",
                                             min = 0.005,
                                             max = 0.01,
                                             step = 0.0001,
                                             value = 0.0068
                                 )
                        ),
                        tags$div(class = "col-sm-4",
                                 sliderInput("c3bD",
                                             "Cirrhosis stage C3 to HCC_BCLC_D",
                                             min = 0.05,
                                             max = 0.1,
                                             step = 0.001,
                                             value = 0.066
                                 )
                        ),
                        tags$div(class = "col-sm-4",
                                 sliderInput("c4bD",
                                             "Cirrhosis stage C4 to HCC_BCLC_D",
                                             min = 0.05,
                                             max = 0.1,
                                             step = 0.001,
                                             value = 0.066
                                 )
                        )
               )
               ), 
      #tab3
      tabPanel("Screening",
               tags$div(class = "sliderDisplay col-sm-12",
                        tags$div(
                          radioButtons("screening", "Screening type:",
                                       c("By age" = 1,
                                         "By risk groups" = 2),
                                       inline = T)
                        ),
                        tags$div(id = "Age_screen",
                                 radioButtons("age_s", "Age screening range (years):",
                                              c("40 to 50" = 1,
                                                "50 to 60" = 2,
                                                "40 to 60" = 3)),
                                 tags$img(src="image/population2019.png" 
                                          ,alt="Study design of the transmission and disease progression model."),
                                 
                                 #imported excel file and now in r as "screeningdesc"
                                 tags$div(class = "screeningdesc col-sm-6",
                                          fluidRow(
                                            
                                            br(),
                                            column(width = 1),
                                            tableHTML_output("screeningtbl")), style = "position:absolute; right:1em; top:200px")),
                        
                        tags$div(id = "Risk_group",
                                 radioButtons("risk_g", "Risk groups:",
                                              c("Human Immunodeficiency Virus (HIV)" = 1,
                                                "Injection Drug User (IDU)" = 2,
                                                "Men who have Sex with Men (MSM)" = 3,
                                                "Blood donors" = 4,
                                                "Prisoners" = 5
                                              )),
                                 
                                 #imported excel file and now in r as "riskdesc"
                                 tags$div(class = "riskdesc col-sm-6",
                                          fluidRow(
                                            
                                            br(),
                                            column(width = 1),
                                            tableHTML_output("risktbl")), style = "position:absolute; right:5em; top:30px")),
                        
                        textOutput("screening_p")
                        
                        
               ) 
      ), 
      #tab4
      tabPanel("Diagnosis",
               tags$div(class = "sliderDisplay col-sm-12",
                        tags$div(class = "col-sm-6",
                                 radioButtons("test1", "First test:",
                                              c("HCV Antibody" = 1,
                                                "Rapid HCV RNA" = 2))
                        ),
                        tags$div(id = "test2",class = "col-sm-6",
                                 radioButtons("test2", "Second test:",
                                              c("HCV RNA" = 1,
                                                "CORE Antigen" = 2,
                                                "Rapid HCV RNA" = 3))
                        ),
                        #imported excel file and now in r as "testdesc"
                        tags$div(class = "testdesc col-sm-6",
                                 fluidRow(
                                   
                                   br(),
                                   column(width = 1),
                                   tableHTML_output("testtbl"), style = "position:absolute; right:-40em; top:1px")
                        ),
                        
                        tags$div(
                          
                          tags$img(src="image/hcv testing sequence identifying hcv infection.jpg" 
                                   ,alt="Study design of the transmission and disease progression model."),
                          tags$p("source : https://www.hepatitisc.uw.edu")
                        ))),
      #tab5
    tabPanel("Treatment",
             
             tags$div(class = "sliderDisplay col-sm-12",
                      tags$h3("Treatment"),
                      tags$hr(),
                      tags$div(class = "col-sm-12",
                               radioButtons("Treatment", "Novel Treatment type:",width = '100%',
                                            c("No novel treatment" = 0,
                                              "Sofosbuvir with Peginterferon alfa type 2a or 2b and ribavirin (National List of Essential Medicines)" =1,
                                              "Sofosbuvir with Ledipasvir (National List of Essential Medicines)" = 2,
                                              "Sofosbuvir with Daclatasvir (pan-genotypic treatments)" = 3,
                                              "Sofosbuvir with Velpatasvir (pan-genotypic treatments)" = 4,
                                              "Sofosbuvir with Ravidasvir (pan-genotypic treatments, on-going clinical trial)" = 5,
                                              "Another durg" = 6),),
                               shinyjs::useShinyjs(),
                               numericInput("Input_F0", "F0", 0.5, min = 0, max = 1,step =0.1),
                               numericInput("Input_F1", "F1", 0.5, min = 0, max = 1,step =0.1),
                               numericInput("Input_F2", "F2", 0.5, min = 0, max = 1,step =0.1),
                               numericInput("Input_F3", "F3", 0.5, min = 0, max = 1,step =0.1),
                               numericInput("Input_C1", "C1", 0.5, min = 0, max = 1,step =0.1),
                               numericInput("Input_C2", "C2", 0.5, min = 0, max = 1,step =0.1),
                               numericInput("Input_C3", "C3", 0.5, min = 0, max = 1,step =0.1),
                               numericInput("Input_C4", "C4", 0.5, min = 0, max = 1,step =0.1),
                               numericInput("Input_Cost", "Cost", 20)
                               
                      ),
                      tags$div(class = "row",
                               tags$div(class = "col-sm-11",id ="TreatmentOutput",
                                        textOutput("text1"),
                                        textOutput("text2"),
                                        textOutput("text3"),
                                        textOutput("text4"),
                                        textOutput("text5"),
                                        textOutput("text6"),
                                        textOutput("text7"),
                                        textOutput("text8")
                                        
                                        # tags$h3("Treatment Cost"),
                                        # tags$hr(),
                                        # tags$div(class = "col-sm-6",
                                        #          sliderInput("Te1",
                                        #                      "Treatment Cost ($)",
                                        #                      min = 0.05,
                                        #                      max = 0.1,
                                        #                      step = 0.001,
                                        #                      value = 0.066
                               )
                      ),
                      tags$div(class = "col-sm-6"),
                      
                      tags$div(id = "drugImg1",
                               tags$img(src="image/drug1.png" 
                                        ,alt="drug1")
                      ),
                      tags$div(id = "drugImg2",
                               tags$img(src="image/drug2.png" 
                                        ,alt="drug2")
                      ),
                      tags$div(id = "drugImg3",
                               tags$img(src="image/drug3.png" 
                                        ,alt="drug3")
                      ),
                      tags$div(id = "drugImg4",
                               tags$img(src="image/drug4.png" 
                                        ,alt="drug4")
                      ),
                      tags$div(id = "drugImg5",
                               tags$img(src="image/drug5.png" 
                                        ,alt="drug5")
                      )
                      
             ),
             #imported excel file and now in r as "treatmentdesc"
             tags$div(class = "treatmentdesc col-sm-6",
                      fluidRow(
                        
                        br(),
                        column(width = 1),
                        tableHTML_output("treatmenttbl"))
             )), 

      #tab7
      tabPanel("Model prediction",

                  actionButton("go", "Run model",class = "button btn btn-primary"),
                  downloadButton("downloadData", "Download Parms"),
                  downloadButton("downloadData2", "Download Result Table"),
                 tabsetPanel(
                    #output 1
                    tabPanel("Prevalence Of CHC",
                             plotOutput("distPlot")
                             ),
                    #output 2
                    tabPanel("Cumulative Death",
                             checkboxInput("showNewDeath", "New death", FALSE),

                             plotOutput("distPlot2")
                             
                             ),
                    #output 3
                    tabPanel("Annual Incidence",
                             plotOutput("distPlot3")
                            ),
                    #output 4
                    tabPanel("Estimated cost",
                      
                             plotOutput("distPlot4")
                    )
                  )
               )
    )
 
  
  )
)
