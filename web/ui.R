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

# Define UI for application that draws a histogram
shinyUI(fluidPage(  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),

  
  # Application title
  tags$div(class="header bg-primary",
  tags$h1(class="text-center","Hepatitis C Virus Modelling",class = "font-family-sans-serif")
            ),
  tabsetPanel(
      #tab1
      tabPanel("Introduction",
                  tags$h2("Rationale"),
                  tags$p("Hepatitis C virus (HCV) infection is an important worldwide public health problem, and most
                          of the global HCV burden is in low- to middle-income countries.A mathematical model of CHC transmission
                          dynamics was constructed to examine the disease burden over the next 20 years using different
                          treatment strategies. We compared and evaluated the current treatment (PEGylated
                          interferon and ribavirin) with new treatments using novel direct-acting antiviral agents
                          among various treatment policies."),
                  tags$p("These advances have led to the potential of HCV treatment delivery to national public
                          health programs and decreased overall HCV-related morbidity and mortality [1]."),
               
                  tags$h2("Study Objectives"),
                  tags$p("1) To assess impact of treating HCV patients (by measuring the incidence of CHC - related decompensated
                         cirrhosis and hepatocellular carcinoma (HCC) and mortality) wih different regimens including "),
                  tags$ol(type = "a",
                    tags$li("sofosbuvir with peginterferon alfa type 2a or 2b and ribavirin (National List of Essential Medicines)"),
                    tags$li("sofosbuvir with ledipasvir (National List of Essential Medicines)"),
                    tags$li("sofosbuvir with daclatasvir (pan-genotypic treatments)"),
                    tags$li("sofosbuvir with velpatasvir (pan-genotypic treatments)"),
                    tags$li("sofosbuvir with ravidasvir (pan-genotypic treatments, on-going clinical trial)")
                  ),
               
                  tags$p("2) To estimate the cost of treatment for each regimen and compare with the standard treatment"),
               
                  tags$h2("Mathematical Modelling Approach"),
                  tags$p("We further develop the model from our previous study looking at the 
                         treatment coverage and allocation strategies (Poovorawan",tags$i(class = "italic","et. al."), 
                         "2016)."),
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
                                             min = 0.0001,
                                             max = 0.1,
                                             step = 0.0001,
                                             value = 0.0001
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
      tabPanel("Screening",
               tags$div(class = "sliderDisplay col-sm-12",
                        tags$div(
                          radioButtons("screening", "screening type:",
                                       c("By age" = 1,
                                         "By Risks" = 2),
                                         inline = T)
                        ),
                          tags$div(id = "Age_screen",
                          radioButtons("age_s", "Age screening range (Years):",
                                       c("40-50" = 1,
                                         "50-60" = 2,
                                         "40-60" = 3))
                          ),
                          tags$div(id = "Risk_group",
                          radioButtons("risk_g", "Risk groups:",
                                       c("Human Immunodeficiency Virus (HIV)" = 1,
                                         "Injection Drug User (IDU)" = 2,
                                         "Men who have Sex with Men (MSM)" = 3,
                                         "Blood donate" = 4,
                                         "Prisoner" = 5
                                         ))
                          ),
                        textOutput("screening_p")

                        
               ) 
               ), 
      #tab4
      tabPanel("diagnosis",
               tags$div(class = "sliderDisplay col-sm-12",
                 tags$div(class = "col-sm-6",
                   radioButtons("test1", "Test 1st:",
                                c("Anti HCV" = 1,
                                  "Rapid HCV RNA" = 2))
                 ),
                 tags$div(id = "test2",class = "col-sm-6",
                            radioButtons("test2", "Test 2nd:",
                                         c("RNA" = 1,
                                           "CORE Antigen" = 2,
                                           "Rapid HCV RNA" = 3))
                 ),
                 tags$img(src="image/hcv testing sequence identifying hcv infection.jpg" 
                          ,alt="Study design of the transmission and disease progression model."),
                 tags$p("source : https://www.hepatitisc.uw.edu")
               )
               ),
      #tab5
      tabPanel("Link to care",
               tags$div(class = "sliderDisplay col-sm-12",
                        tags$div(class = "col-sm-6",
                          radioButtons("care", "Link to care :",
                                       c("HCV genotype testing" = 1,
                                         "fibroscan stiffness score" = 2
                                         )
                          )
                        ),
               
               tags$div(class= "linkToCareDes col-sm-6",
                        tags$div( id = "HCVDes",
                          "HCV genotype testing is recommended to guide selection of the most appropriate antiviral regimen."
                          ),
                        tags$div( id = "FibDes",
                                  " FibroScan scores can be used to validate advanced fibrosis/cirrhosis for insurance companies, health care plans, or medical centers that require a significant amount of fibrosis before approving or allowing the start of HCV treatment."
                        )
               )
               ),
               tags$div(class = "sliderDisplay col-sm-12",
                 textOutput("Pos_T_Text"),
                 textOutput("Pos_F_Text"),
                 textOutput("Neg_T_Text"),
                 textOutput("Neg_F_Text")
               )
               ),        
      #tab6
      tabPanel("Treatment",
               
               tags$div(class = "sliderDisplay col-sm-12",
                        tags$h3("Treatment"),
                        tags$hr(),
                        tags$div(class = "col-sm-12",
                        radioButtons("Treatment", "Novel Treatment type:",width = '100%',
                                     c("no novel treatment" = 0,
                                       "sofosbuvir with peginterferon alfa type 2a or 2b and ribavirin (National List of Essential Medicines)" =1,
                                       "sofosbuvir with ledipasvir (National List of Essential Medicines)" = 2,
                                       "sofosbuvir with daclatasvir (pan-genotypic treatments)" = 3,
                                       "sofosbuvir with velpatasvir (pan-genotypic treatments)" = 4,
                                       "sofosbuvir with ravidasvir (pan-genotypic treatments, on-going clinical trial)" = 5))
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
                        )),
                        tags$h3("treatment Cost"),
                        tags$hr(),
                        tags$div(class = "col-sm-6",
                                 sliderInput("Te1",
                                             "treatment Cost ($)",
                                             min = 0.05,
                                             max = 0.1,
                                             step = 0.001,
                                             value = 0.066
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
                  
               )
               ), 
      #tab7
      tabPanel("Display Seting",
               tags$div(class = "sliderDisplay col-sm-12",
                        sliderInput("year",
                                    "Time (Year)",
                                    min = 1999, 
                                    max = 2035,
                                    value = c(2000,2020)
                        )
               )
               ),
      #tab8
      tabPanel("Model prediction",

                  actionButton("button", "Run model",class = "button btn btn-primary"),
               downloadButton("downloadData", "Download Table"),
               downloadButton("downloadparameter", "Download Parameter"),
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
                             useShinyjs(),
                             checkboxInput("Anin_genotype", "By Genotype", FALSE),
                             div(id="A_nonG",
                               plotOutput("Anin_Plot")
                               ),
                             
                             div(id="A_G",
                                tags$h3("C1 Patient Cured"),
                                hr(),
                                plotOutput("Anin_G_C1_Plot"),
                                tags$h3("C2 Patient Cured"),
                                hr(),
                                plotOutput("Anin_G_C2_Plot"),
                                tags$h3("C3 Patient Cured"),
                                hr(),
                                plotOutput("Anin_G_C3_Plot"),
                                tags$h3("C4 Patient Cured"),
                                hr(),
                                plotOutput("Anin_G_C4_Plot")
                                )
                            ),
                    #output 4
                    tabPanel("Estimated cost",
                             checkboxInput("showgenotype3", "By Genotype", FALSE),
                             plotOutput("distPlot4")
                    ),
                    #output 5
                    tabPanel("Pie",
                             checkboxInput("showgenotype_pie", "By Genotype", FALSE),
                             div(id="Pie_nonG",
                                plotOutput("piePlot")
                                ),
                             div(id="Pie_G",
                                 tags$h3("propF0_genotype"),
                                 hr(),
                                 plotOutput("piePlot_F0"),
                                 tags$h3("propF1_genotype"),
                                 hr(),
                                 plotOutput("piePlot_F1"),
                                 tags$h3("propF2_genotype"),
                                 hr(),
                                 plotOutput("piePlot_F2"),
                                 tags$h3("propF3_genotype"),
                                 hr(),
                                 plotOutput("piePlot_F3")
                                 )
                    )
                  )
               ),
      #tab9
      tabPanel("Appendix",
                  div(
                      tags$p("1. progress 2nd _ 2018 10 03"),
                      tags$a(
                        #link
                        href= "https://docs.google.com/presentation/d/1iHAfhqpBh5MpYjf4Sethcq5E3OhoYcyK_nugy2QXnik/edit?usp=sharing"
                        ,"Google Drive" #text
                      )
                      )
              
               )
    )
 
  #https://docs.google.com/presentation/d/1iHAfhqpBh5MpYjf4Sethcq5E3OhoYcyK_nugy2QXnik/edit?usp=sharing
  )
)

