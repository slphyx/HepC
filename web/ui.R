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
                  tags$p("Lorem ipsum dolor, sit amet consectetur 
                         adipisicing elit. Sapiente, ex esse. Esse, 
                         vel quas corporis labore atque facilis ad nobis aut. 
                         Cumque debitis, eum optio neque ipsa saepe. Minima, fugiat?")
               ),
      #tab2
      tabPanel("Natural History Of Disease",
                  tags$img(src="image/disease progression model.png" 
                           ,alt="Study design of the transmission and disease progression model.")
               ), 
      #tab3
      tabPanel("Treatment",
                  sidebarPanel(
                    
                    tags$h3("Treatment efficacy"),
                    tags$hr(),
                    
                    sliderInput("stdC_E",
                                "Standard Treatment Efficacy",
                                min = 0,
                                max = 1,
                                step = 0.05,
                                value = 0.7
                                ),
                    
                    sliderInput("newC_E",
                                "Novel Treatment Efficacy",
                                min = 0,
                                max = 1,
                                step = 0.05,
                                value = 0.9
                                ),
                    
                    tags$h3("Costs of treatments"),
                    tags$hr(),
                    
                    sliderInput("stdC_cost",
                                "Standard Treatment Costs",
                                min = 1,
                                max = 100,
                                step = 1,
                                value = 10
                    ),
                    
                    sliderInput("newC_cost",
                                "Novel Treatment Costs",
                                min = 1,
                                max = 100,
                                step = 1,
                                value = 10
                    )
                  )
               ), 
      #tab4
      tabPanel("Display Seting",
               tags$div(class = "sliderDisplay col-sm-12",
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
                    tags$div(class = "col-sm-4",
                    sliderInput("f3cA",
                                "Fibrosis stage F3 to Cirrhosis Child-Pugh class A",
                                min = 0.05,
                                max = 0.2,
                                step = 0.001,
                                value = 0.116
                      )
                    )
                  )
               ),
      #tab5
      tabPanel("Output",

                  actionButton("go", "Plot",class = "button btn btn-primary"),
                 tabsetPanel(
                    #output 1
                    tabPanel("Prevalence Of CHC",
                             plotOutput("distPlot")
                             ),
                    #output 2
                    tabPanel("Cumulative Death",
                             checkboxInput("showgenotype", "show genotype", FALSE),

                             plotOutput("distPlot2")
                             
                             ),
                    #output 3
                    tabPanel("Annual Incidence",
                             checkboxInput("showgenotype2", "show genotype", FALSE),
                             plotOutput("distPlot3")
                            )
                  )
               )
    )
 
  
  )
)
