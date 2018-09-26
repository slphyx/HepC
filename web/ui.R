#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

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
    tabPanel("Plot",
  # Sidebar with a slider input for number of bins 
      tags$div(id = "sidebarinput",
        sidebarLayout(
          sidebarPanel(
            sliderInput("R",
                  "Number of Max People:",
                  min = 10000,
                  max = 500000,
                  value = 100000),
            sliderInput("FI",
                  "Number of FI:",
                  min = 0.0001,
                  max = 0.0020,
                  value = 0.001,
                  step = 0.0001),
            sliderInput("S0",
                  "Number of S0:",
                  min = 0.01,
                  max = 0.2,
                  value = 0.05,
                  step = 0.01),
            sliderInput("gamar",
                  "Number of gamar:",
                  min = 0,
                  max = 1,
                  value = 0.05),
          checkboxGroupInput("checkGroup", label = h3("Graph"), 
                         choices = list("S" = "S",
                                        "F0" = "F0", 
                                        "F1" = "F1",
                                        "F2" = "F2",
                                        "F3" = "F3",
                                        "C1" = "C1",
                                        "C2" = "C2",
                                        "C3" = "C3",
                                        "C4" = "C4",
                                        "HCC_A" ="HCC_A",
                                        "HCC_B" ="HCC_B",
                                        "HCC_C" ="HCC_C",
                                        "HCC_D" ="HCC_D",
                                        "c1std_cured" = "c1std_cured",
                                        "c1new_cured" = "c1new_cured",
                                        "c2new_cured" = "c2new_cured",
                                        "c3new_cured" = "c3new_cured",
                                        "c4new_cured" = "c4new_cured",
                                        "death" = "death",
                                        "deathHCC" = "deathHCC",
                                        "deathC14" = "deathC14"
                                        )
                                )
                             ),
    
    # Show a plot of the generated distribution
      mainPanel(
                  plotOutput("distPlot")
                )
              ),
      tags$div(class="center col-sm-4", 
               actionButton("go", "Plot",class = "button btn btn-primary")
               )
            ) 
          ),
      tabPanel("point"), tabPanel("point"), tabPanel("point")
    )
 
  
))
