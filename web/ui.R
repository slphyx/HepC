#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(bootstrap)

# Define UI for application that draws a histogram
shinyUI(fluidPage(  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),

  
  # Application title
  tags$div(class="header bg-primary",
  tags$h1(class="text-center","Hep C Model",class = "font-family-sans-serif")
            ),
  tabsetPanel(
      #tab1
      tabPanel("Introduction",
                  tags$h2("Abstract"),
                  tags$p("Hepatitis C virus (HCV) infection is an important worldwide public health problem, and most
                          of the global HCV burden is in low- to middle-income countries.A mathematical model of CHC transmission
                          dynamics was constructed to examine the disease burden over the next 20 years using different
                          treatment strategies. We compared and evaluated the current treatment (PEGylated
                          interferon and ribavirin) with new treatments using novel direct-acting antiviral agents
                          among various treatment policies."),
                  
                  tags$h2("Introduction"),
                  tags$p("These advances have led to the potential of HCV treatment delivery to national public
                          health programs and decreased overall HCV-related morbidity and mortality [1]."),
                tags$div(
                tags$img(src="image/disease progression model.png" 
                          ,alt="Study design of the transmission and disease progression model."),
               
               tags$p(class = "italic",tags$b("Fig 1. Study design of the transmission and disease progression model."), "The fibrosis stage of progression develops gradually, and cirrhosis and
                      decompensation develop over time. The risk of developing HCC starts after cirrhosis. Survival rates depend on the severity of disease at each stage. The
                      current standard treatment in Thailand was compared with new direct-acting antivirals with different treatment coverage and allocations. *HCV-related
                      mortality.")
                ),
               tags$hr(),
               tags$p("
                      1.	Kohli A, Shaffer A, Sherman A, Kottilil S. Treatment of hepatitis C: a systematic review. Jama. 2014;
                          312(6):631-40.  doi:", tags$a (href="http://dx.doi.org/10.1001/jama.2014.7085","10.1001/jama.2014.7085"), "PMID: 25117132.")
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
                                             max = 100000000,
                                             step = 1000000,
                                             value = 70000000
                                 )
                        ),
                        tags$div(class = "col-sm-4",
                                 sliderInput("r",
                                             "Population growth rate",
                                             min = 0,
                                             max = 0.5,
                                             step = 0.01,
                                             value = 0.16
                                 )
                        ),
                        tags$div(class = "col-sm-4",
                                 sliderInput("F0",
                                             "persons at fibrosis stage F0",
                                             min = 0.1,
                                             max = 0.3,
                                             step = 0.001,
                                             value = 0.282
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
                        tags$div(class = "col-sm-4",
                                 sliderInput("S",
                                             "proportion of the susceptible population",
                                             min = 0.001,
                                             max = 0.05,
                                             step = 0.001,
                                             value = 0.032
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
                                 sliderInput("f3cA",
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
                                 sliderInput("cAcB",
                                             "Cirrhosis Child-Pugh class A to B",
                                             min = 0.03,
                                             max = 0.1,
                                             step = 0.001,
                                             value = 0.044
                                 )
                        ),
                        tags$div(class = "col-sm-6",
                                 sliderInput("cBcC",
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
      tabPanel("Treatment",
               tags$div(class = "sliderDisplay col-sm-12",
                    
                    tags$h3("Treatment efficacy"),
                    tags$hr(),
                    
                    tags$div(class = "col-sm-6",
                    sliderInput("stdC_E",
                                "Standard Treatment Efficacy",
                                min = 0,
                                max = 1,
                                step = 0.05,
                                value = 0.7
                                )
                    ),
                    
                    tags$div(class = "col-sm-6",
                    sliderInput("newC_E",
                                "Novel Treatment Efficacy",
                                min = 0,
                                max = 1,
                                step = 0.05,
                                value = 0.9
                                )
                    ),
                    
                    

                    tags$h3("Costs of treatments"),
                    tags$hr(),
                    
                    tags$div(class = "col-sm-6",
                    sliderInput("stdC_cost",
                                "Standard Treatment Costs",
                                min = 1,
                                max = 100,
                                step = 1,
                                value = 10
                    )
                    ),
                    
                    tags$div(class = "col-sm-6",
                    sliderInput("newC_cost",
                                "Novel Treatment Costs",
                                min = 1,
                                max = 100,
                                step = 1,
                                value = 10
                    )
                    )
                  
               )
               ), 
      #tab4
      tabPanel("Display Seting",
               tags$div(class = "sliderDisplay col-sm-12",
                        sliderInput("time",
                                    "Time (Year)",
                                    min = 1999, 
                                    max = 2020,
                                    value = c(2000,2010)
                        )
               )
               ),
      #tab5
      tabPanel("Output",

                  actionButton("go", "Plot",class = "button btn btn-primary"),
                 tabsetPanel(
                    #output 1
                    tabPanel("Prevalence Of CHC",
                             checkboxInput("bygenotype", "By Genotype", FALSE),
                             plotOutput("distPlot")
                             ),
                    #output 2
                    tabPanel("Cumulative Death",
                             checkboxInput("showgenotype", "total death", FALSE),

                             plotOutput("distPlot2")
                             
                             ),
                    #output 3
                    tabPanel("Annual Incidence",
                             checkboxInput("showgenotype2", "By Genotype", FALSE),
                             plotOutput("distPlot3")
                            )
                  )
               )
    )
 
  
  )
)
