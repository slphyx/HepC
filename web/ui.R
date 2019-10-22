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
             tags$h2(class = "center2","Collaborators"),
             tags$div(class = "col-sm-12 Mbottom",
             tags$div(class = "col-sm-12 col-lg-4 center",tags$a(href="https://mahidol.ac.th/",
                                                tags$img(class = "logo",src="image/mahidol.png",
                                                alt="Mahidol University"))),
             
             tags$div(class = "col-sm-12 col-lg-4 center",tags$a(href="http://www.tropmedres.ac/home",
                                                tags$img(class = "logo2",src="image/Moru.webp",
                                                alt="Mahidol Oxford Tropical Medicine Research Unit"))),
             
             tags$div(class = "col-sm-12 col-lg-4 center",tags$a(href="http://www.hitap.net/en/",
                                                tags$img(class = "logo3",src="image/Hitap.jpg", 
                                                alt="Health Intervention and Technology Assessment Program")))
              ),
             tags$h2(class = "center2","Funder"),
             tags$div(class = "center",tags$a(href="https://www.nstda.or.th/",
                                              tags$img(class = "logo2",src="image/nstda.png"  
                                              ,alt="National Science and Technology Development Agency"))),
             tags$h2("Software"),
             tags$p("This App is built on the following open-source, free software:																									
                        R Core Team (2014). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. ; Winston Chang (2015). shiny: Web Application Framework for R. ; Leaflet, a JavaScript library for interactive maps ; Datable table plug-in for jQuery."),
             tags$h2("Contact"),
             tags$p("Further feedback and suggestion on this web application is greatly appreciated, please contact",tags$a(href = "mailto: wirichada.pan@mahidol.ac.th","wirichada.pan@mahidol.ac.th")),
             tags$hr(),
             tags$h3("Reference"),
             tags$p("1.Kohli A, Shaffer A, Sherman A, Kottilil S. Treatment of hepatitis C: a systematic review. Jama. 2014;
                        312(6):631-40.  doi:", tags$a (href="http://dx.doi.org/10.1001/jama.2014.7085","10.1001/jama.2014.7085"), "PMID: 25117132."),
             tags$p("2.Poovorawan K, Pan-Ngum W, White LJ" , tags$i(class = "italic","et. al."),"Estimating the impact of expanding treatment coverage and allocation strategies for chronic Hepatitis C in a Direct Antiviral Agent Era.
                        PLoS One. 2016 Sep 15;11(9):e0163095. doi:doi: 10.1371/journal.pone.0163095. eCollection 2016" )
    ),
      
      #tab2
      tabPanel("Natural History Of Disease",
               tags$div(class = "sliderDisplay col-sm-12",
                        
                        #Populations and Transmission Coefficient
                        tags$div(class = "col-sm-12",
                        tags$div(class = "col-sm-8",
                        tags$h3("Populations and Transmission Coefficient"),
                        ),tags$div(class = "col-sm-4 right",
                        actionButton("resetSect1","RESET", class="resetbutton"),
                        ),
                        ),
                        tags$div(class = "col-sm-12",
                        tags$b(class="warning",
                               "*Changing the value of any parameters could result in different baseline disease characteristics. Model validation against real data may be required."),
                        ),
                        tags$div(class = "col-sm-12",
                        tags$hr(),
                        ),
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
                        
                        #Progression of fibrosis
                        tags$div(class = "col-sm-12",
                                 tags$div(class = "col-sm-8",
                                          tags$h3("Progression of fibrosis"),
                                 ),tags$div(class = "col-sm-4 right",
                                            actionButton("resetSect2","RESET", class="resetbutton"),
                                 ),
                        ),
                        tags$div(class = "col-sm-12",
                                 tags$b(class="warning",
                                        "*Changing the value of any parameters could result in different baseline disease characteristics. Model validation against real data may be required."),
                        ),
                        tags$div(class = "col-sm-12",
                                 tags$hr(),
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
                        tags$div(class = "col-sm-12",
                                 sliderInput("f3c1",
                                             "Fibrosis stage F3 to Cirrhosis Child-Pugh class A",
                                             min = 0.05,
                                             max = 0.2,
                                             step = 0.001,
                                             value = 0.116
                                 )
                                 
                        ),
                        
                        #Progression of cirrhosis
                        tags$div(class = "col-sm-12",
                                 tags$div(class = "col-sm-8",
                                    tags$h3("Progression of cirrhosis"),
                                 ),tags$div(class = "col-sm-4 right",
                                    actionButton("resetSect3","RESET", class="resetbutton"),
                                 ),
                        ),
                        tags$div(class = "col-sm-12",
                                 tags$b(class="warning",
                                        "*Changing the value of any parameters could result in different baseline disease characteristics. Model validation against real data may be required."),
                        ),
                        tags$div(class = "col-sm-12",
                                 tags$hr(),
                        ),
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
                        
                        #Incidence of developing HCC
                        tags$div(class = "col-sm-12",
                                 tags$div(class = "col-sm-8",
                                          tags$h3("Incidence of developing HCC"),
                                 ),tags$div(class = "col-sm-4 right",
                                            actionButton("resetSect4","RESET", class="resetbutton"),
                                 ),
                        ),
                        tags$div(class = "col-sm-12",
                                 tags$b(class="warning",
                                        "*Changing the value of any parameters could result in different baseline disease characteristics. Model validation against real data may be required."),
                        ),
                        tags$div(class = "col-sm-12",
                                 tags$hr(),
                        ),
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
                        shinyjs::useShinyjs(),
                        tags$div(class = " col-sm-6",
                        tags$div(
                          radioButtons("screening", "Screening type:",
                                       c("By age" = 1,
                                         "By risk groups" = 2),
                                       inline = T)
                        ),
                        
                        tags$div(

                                 radioButtons("age_s", "Age screening range (years):",
                                              c("40 to 50" = 1,
                                                "50 to 60" = 2,
                                                "40 to 60" = 3))


                                 ),
                        tags$table(
                          tags$tr(
                                  tags$th("Screening option"),
                                  tags$th("Age to screen (Years)"),
                                  tags$th("HCV screening positive rate"),
                                  tags$th("HCV confirming positive rate"),
                          ),
                          tags$tr(
                            tags$td("A"),
                            tags$td("41-50"),
                            tags$td("2.72"),
                            tags$td("1.69"),
                          ),
                          tags$tr(
                            tags$td("B"),
                            tags$td("51-60"),
                            tags$td("1.46"),
                            tags$td("0.93"),
                          ),
                          tags$tr(
                            tags$td("C"),
                            tags$td("41-60"),
                            tags$td("4.18"),
                            tags$td("2.62"),
                          ),
                        ),
                        tags$div(id = "Risk_group",
                                 checkboxGroupInput("risk_g", "Risk groups:",
                                                    c("Human Immunodeficiency Virus (HIV)" = 1,
                                                      "Injection Drug User (IDU)" = 2,
                                                      "Men who have Sex with Men (MSM)" = 3,
                                                      "Received blood transfusion before HCV screening" = 4,
                                                      "Blood donors" = 5,
                                                      "Prisoners" = 6,
                                                      "Chronic kidney disease" = 7
                                                    )),
                        ),

                        ),
                        tags$div(class = " col-sm-6",
                                 tags$img(src="image/population2019.png" 
                                          ,alt="Study design of the transmission and disease progression model."),
                        ),
                        shinyjs::useShinyjs(),
                        
                        tags$table(id = "Scr_table",
                         hidden(
                          tags$tr(id = "Scr_th",
                            tags$th("Risk group"),
                            tags$th("Population size"),
                            tags$th("Positive Anti HCV (%)"),
                            tags$th("Screen Positive(%)"),
                            tags$th("Confirm Positive (%)"),
                          ),
                          #risk HIV
                          tags$tr(id = "Scr_td1",
                            tags$td("HIV"),
                            tags$td("444,000 (2018)" ,tags$sup("[1]")),
                            tags$td("11 (1995)" ,tags$sup("[7]")," 
                                    ,8.4 (2010)" ,tags$sup("[8]") 
                                    ," ,7.7 (2015)" , tags$sup("[9]")
                                    ),
                            #slider
                            tags$td(sliderInput("HIV_Scr",
                                                "",
                                                min = 5,
                                                max = 15,
                                                step = 0.1,
                                                value = 9
                                                )
                                    
                                    ),
                            tags$td(sliderInput("HIV_Con",
                                                "",
                                                min = 50,
                                                max = 100,
                                                step = 1,
                                                value = 70
                                                )
                                    ),
                          ),
                          #risk IDU
                          tags$tr(id = "Scr_td2",
                                 tags$td("IDU"),
                                 tags$td("260,305 (2019)" ,tags$sup("[2]")),
                                 tags$td("44.3 (2019)" ,tags$sup("[10]")),
                                 #slider
                                 tags$td(sliderInput("IDU_Scr",
                                                     "",
                                                     min = 30,
                                                     max = 60,
                                                     step = 1,
                                                     value = 44
                                 )
                                 
                                 ),
                                 tags$td(sliderInput("IDU_Con",
                                                     "",
                                                     min = 50,
                                                     max = 100,
                                                     step = 1,
                                                     value = 70
                                 )
                                 ),
                          ),
                          #risk MSM
                          tags$tr(id = "Scr_td3",
                            tags$td("MSM"),
                            tags$td("590,000 (2016)" ,tags$sup("[3]")),
                            tags$td("4.9 (2019)" ,tags$sup("[11]")),
                            #slider
                            tags$td(sliderInput("MSM_Scr",
                                                "",
                                                min = 1,
                                                max = 10,
                                                step = 0.1,
                                                value = 4.9
                            )
                            
                            ),
                            tags$td(sliderInput("MSM_Con",
                                                "",
                                                min = 50,
                                                max = 100,
                                                step = 1,
                                                value = 70
                            )
                            ),
                          ),
                          #risk Received blood
                          tags$tr(id = "Scr_td4",
                            tags$td("Received blood"),
                            tags$td("390,000(Estimate (1% age 30 or above)"),
                            tags$td("Unknown"),
                            #slider
                            tags$td(sliderInput("Rb_Scr",
                                                "",
                                                min = 1,
                                                max = 10,
                                                step = 0.1,
                                                value = 5
                            )
                            
                            ),
                            tags$td(sliderInput("Rb_Con",
                                                "",
                                                min = 50,
                                                max = 100,
                                                step = 1,
                                                value = 70
                            )
                            ),
                          ),
                          #risk Blood donor
                          tags$tr(id = "Scr_td5",
                            tags$td("Blood donor"),
                            tags$td("89,311 (2009)" ,tags$sup("[4]")),
                            tags$td("2.9 (2004)" ,tags$sup("[12]")),
                            #slider
                            tags$td(sliderInput("Rd_Scr",
                                                "",
                                                min = 1,
                                                max = 5,
                                                step = 0.1,
                                                value = 2.9
                            )
                            
                            ),
                            tags$td(sliderInput("Rd_Con",
                                                "",
                                                min = 50,
                                                max = 100,
                                                step = 1,
                                                value = 70
                            )
                            ),
                          ),
                          #Prisoner
                         tags$tr(id = "Scr_td6",
                                 tags$td("Prisoner"),
                                 tags$td("372,979 (2019)" ,tags$sup("[5]")),
                                 tags$td("7 (2019)" ,tags$sup("[13]")),
                                 #slider
                                 tags$td(sliderInput("Pri_Scr",
                                                     "",
                                                     min = 1,
                                                     max = 10,
                                                     step = 0.1,
                                                     value = 7
                                 )
                                 
                                 ),
                                 tags$td(sliderInput("Pri_Con",
                                                     "",
                                                     min = 50,
                                                     max = 100,
                                                     step = 1,
                                                     value = 70
                                 )
                                 ),
                         ),
                         #Chronic kidney disease
                         tags$tr(id = "Scr_td7",
                                 tags$td("CKD"),
                                 tags$td("128,338(2016)" ,tags$sup("[6]")),
                                 tags$td("5.6 (2016)" ,tags$sup("[14]")) , 
                                 #slider
                                 tags$td(sliderInput("CKD_Scr",
                                                     "",
                                                     min = 1,
                                                     max = 10,
                                                     step = 0.1,
                                                     value = 5.6
                                 )
                                 
                                 ),
                                 tags$td(sliderInput("CKD_Con",
                                                     "",
                                                     min = 50,
                                                     max = 100,
                                                     step = 1,
                                                     value = 70
                                 )
                                 ),
                         )
                        )
                        ),
                        
                        textOutput("screening_p")
                        
                        
               ) ,
               tags$div(class = "col-sm-12",
                        tags$hr(),
               ),
               tags$h3("Reference"),
               tags$p("1 HIV AIDS Asia Pacific Research Statistical Data Information Resources AIDS Data Hub. HIV in Thailand. 2018 [Available from: ",
                      tags$a (href="https://www.aidsdatahub.org/Country-Profiles/Thailand","https://www.aidsdatahub.org/Country-Profiles/Thailand"), "]"
                      ),
               tags$p("2 Office of the Narcotics Control Board. Anual report. Office of the Narcotics Control Board, Ministry of Justice 2019 [Available from: ",
                      tags$a (href="https://www.oncb.go.th/Pages/main_old.aspx","https://www.oncb.go.th/Pages/main_old.aspx"), "]"
               ),
               tags$p("3 United Nations. World Population Prospects 2019. Office of the Director, Population Division, United Nations. 2019 [Available from: ",
                      tags$a (href="https://population.un.org/wpp/","https://population.un.org/wpp/"), "]"
               ),
               tags$p("4 Chimparlee N, Oota S, Phikulsod S, Tangkijvanich P, Poovorawan Y. Hepatitis B and hepatitis C virus in Thai blood donors. The Southeast Asian journal of tropical medicine and public health. 2011;42(3):609-15."
                      ),
               tags$p("5 Department of Corrections. Corrections statistics. Department of Corrections, Ministry of Justice. 2019 [Available from: ",
                      tags$a (href="http://www.correct.go.th/stat102/display/select_type.php","http://www.correct.go.th/stat102/display/select_type.php"), "]"
               ),
               tags$p("6 Anutrakulchai S, Mairiang P, Pongskul C, Thepsuthammarat K, Chan-On C, Thinkhamrop B. Mortality and treatment costs of hospitalized chronic kidney disease patients between the three major health insurance schemes in Thailand. BMC Health Serv Res. 2016;16(1):528-."
               ),
               tags$p("7 Suwanagool S, Tieangrim S, Ratanasuwan W, Mekanantagosol S, Luengrojanakul P, Kunasol P. Seroprevalence of anti-HCV among HIV-infected persons and general population. Journal of the Medical Association of Thailand = Chotmaihet thangphaet. 1995;78(11):611-7."
               ),
               tags$p("8 Jatapai A, Nelson KE, Chuenchitra T, Kana K, Eiumtrakul S, Sunantarod E, et al. Prevalence and risk factors for hepatitis C virus infection among young Thai men. The American journal of tropical medicine and hygiene. 2010;83(2):433-9."
               ),
               tags$p("9 Phuangchoei P, Chotiyaputta W, Chayakulkeeree M. Clinical characteristics of hepatitis B and C virus infections in HIV-infected patients. J Med Assoc Thai. 2015;98(3):226-31.)"
               ),
               tags$p("10 Martin M, Vanichseni S, Leelawiwat W, Anekvorapong R, Raengsakulrach B, Cherdtrakulkiat T, et al. Hepatitis C virus infection among people who inject drugs in Bangkok, Thailand, 2005-2010. WHO South-East Asia journal of public health. 2019;8(1):50-5."
               ),
               tags$p("11 Thai Red CRoss AIDS Research Centre. 2019  [Available from: ",
                      tags$a (href="http://en.trcarc.org/Homepage/?page_id=14","http://en.trcarc.org/Homepage/?page_id=14"), "]"
               ),
               tags$p("12 Luksamijarulkul P, Thammata N, Sujirarat D, Tiloklurs M. Hepatitis C virus infection among Thai blood donors: antibody prevalence, risk factors and development of risk screening form. The Southeast Asian journal of tropical medicine and public health. 2004;35(1):147-54."
               ),
               tags$p("13 Bureau AIDS, TB, and STI. Heppatitis C. Bureau AIDS, TB, and STI, Department of Disease control, Ministry of Public health 2019 [Available from: ",
                      tags$a (href="https://ddc.moph.go.th/aidssti/","https://ddc.moph.go.th/aidssti/"), "]"
               ),
               tags$p("14 Jha V, Prasad N. CKD and Infectious Diseases in Asia Pacific: Challenges and Opportunities. American journal of kidney diseases : the official journal of the National Kidney Foundation. 2016;68(1):148-60."
               ),

      ), 
      #tab4
      tabPanel("Diagnosis",
               tags$div(class = "sliderDisplay col-sm-12",
                        tags$div(class = "col-sm-12 Mbottom",
                        tags$div(class = "col-sm-6",
                                 radioButtons("Dia_Scr", "Screening:",
                                              c("Rapid strip test ANT HCV" =1,
                                                "HCV Antibody" = 2,
                                                "Rapid HCV RNA" = 3,
                                                "Other test" = 4)),
                                 hidden(
                                   tags$div(id="NewDiagnosis",
                                            numericInput("Input_Dia_Sen", "Sensitivity", 50, min = 0, max = 100,step =0.1),
                                            numericInput("Input_Dia_Cost", "Cost (THB)", 6000),
                                            actionButton("Comfirm_dia", "Comfirm"),
                                   )       
                                   ),
                        ),
                        tags$div(id = "test2",class = "col-sm-6",
                                 radioButtons("Dia_Con", "Confirming:",
                                              c("HCV RNA" = 1,
                                                "CORE Antigen" = 2,
                                                "Rapid HCV RNA" = 3))
                        ),
                        ),
                        
                        tags$div(class = "col-sm-12 col-lg-6 Mbottom",

                          tags$table(
                            tags$tr(
                              tags$th("Test"),
                              tags$th("Sensitivity/Specificity"),
                              tags$th("Cost (USD)"),
                              tags$th("Cost (THB)"),
                            ),
                            tags$tr(
                              tags$td(colspan="4","Screening"),
                            ),
                            tags$tr(
                              tags$td("Rapid strip test ANT HCV"),
                              tags$td("94/98",tags$sup("[1]")),
                              tags$td("60",tags$sup("[1]")),
                              tags$td(
                                sliderInput("Scr1_Dia_Cost",
                                                  "",
                                                  min = 50,
                                                  max = 1000,
                                                  step = 10,
                                                  value = 70
                                           )
                                
                              ),
                            ),
                            tags$tr(
                              tags$td("Antibody HCV"),
                              tags$td("99.5/99.8",tags$sup("[2]")),
                              tags$td("10"),
                              tags$td(
                                sliderInput("Scr2_Dia_Cost",
                                            "",
                                            min = 200,
                                            max = 500,
                                            step = 10,
                                            value = 300
                                )
                              ),
                            ),
                            tags$tr(
                              tags$td(colspan="4","Confirming"),
                            ),
                            tags$tr(
                              tags$td("HCV RNA"),
                              tags$td("81.2/96.15" ,tags$sup("[3]")),
                              tags$td("50",tags$sup("[6],[7]")),
                              tags$td(
                                sliderInput("Con1_Dia_Cost",
                                            "",
                                            min = 300,
                                            max = 4000,
                                            step = 10,
                                            value = 1500
                                )
                              ),
                            ),
                            tags$tr(
                              tags$td("Rapid HCV RNA test by Gene expert"),
                              tags$td("100/100" ,tags$sup("[4]")),
                              tags$td("10"),
                              tags$td(
                                sliderInput("Con1_Dia_Cost",
                                            "",
                                            min = 200,
                                            max = 500,
                                            step = 10,
                                            value = 300
                                )
                              ),
                            ),
                            tags$tr(
                              tags$td("HCV core antigen"),
                              tags$td("94/98" ,tags$sup("[5]")),
                              tags$td("30",tags$sup("[7]")),
                              tags$td(
                                sliderInput("Con1_Dia_Cost",
                                            "",
                                            min = 200,
                                            max = 500,
                                            step = 10,
                                            value = 300
                                )
                              ),
                            )
                            
                          ),
                        ),
                        
                        tags$div(class="col-sm-12 col-lg-6",
                          
                          tags$img(src="image/hcv testing sequence identifying hcv infection.jpg" 
                                   ,alt="Study design of the transmission and disease progression model."),
                          tags$p("source : https://www.hepatitisc.uw.edu")
                        )),
               tags$div(class = "col-sm-12",
               tags$hr(),
               ),
               tags$h3("Reference"),
               tags$p("1 Loei Provincial Health Office. Price lists of medical devices report. Loei Provincial Health Office, Ministry of Public health. 2018 [Available from: ",
                      tags$a (href="https://www.lo.moph.go.th/moph/moph-judesue-all.php","https://www.lo.moph.go.th/moph/moph-judesue-all.php"), "]"
               ),
               tags$p("2 Cadieux G, Campbell J, Dendukuri N. Systematic review of the accuracy of antibody tests used to screen asymptomatic adults for hepatitis C infection. CMAJ Open. 2016;4(4):E737-E45."
               ),
               tags$p("3 Gao Q, Liu D, Zhang S, Tong L. [Analyses of anti-hCV detected by ELISA and HCV RNA detected by RT-nPCR in chronic hepatitis C virus infectors]. Wei sheng yan jiu = Journal of hygiene research. 2007;36(1):69-71."
               ),
               tags$p("4 World Health Organizatio.WHO PQ Public Report. 2017 [Available from: ",
                      tags$a (href="https://www.who.int/diagnostics_laboratory/evaluations/pq-list/hcv/170404_final_pq_report_pqdx_0260-070-00.pdf","https://www.who.int/diagnostics_laboratory/evaluations/pq-list/hcv/170404_final_pq_report_pqdx_0260-070-00.pdf"), "]"
               ),
               tags$p("5 Lamoury FMJ, Soker A, Martinez D, Hajarizadeh B, Cunningham EB, Cunningham P, et al. Hepatitis C virus core antigen: A simplified treatment monitoring tool, including for post-treatment relapse. Journal of Clinical Virology. 2017;92:32-8."
               ),
               tags$p("6 National Health Security Office (NHSO). Guideline for screening and confirmation of Hepatitis C virus. 2018 [Available from: ",
                      tags$a (href="https://bit.ly/2J2cMLT","https://r8way.moph.go.th/"), "]"
               ),
               tags$p("7 Alcorn K. Hepatitis C antigen testing could eliminate need for two-step HCV testing, reduce cost of access. 2016 [Available from: ",
                      tags$a (href="ttps://www.aidsmap.com/news/jun-2016/hepatitis-c-antigen-testing-could-eliminate-need-two-step-hcv-testing-reduce-cost","ttps://www.aidsmap.com/news/jun-2016/hepatitis-c-antigen-testing-could-eliminate-need-two-step-hcv-testing-reduce-cost"), "]"
               ),
               ),
      #tab5
    tabPanel("Treatment",
             
             tags$div(class = "sliderDisplay col-sm-12",
                      tags$h3("Treatment"),
                      tags$hr(),
                      tags$div(class = "col-sm-12",
                               tags$div(class = "col-sm-12 col-lg-5",
                               radioButtons("Treatment", "Novel Treatment type:",width = '100%',
                                            c("No novel treatment" = 0,
                                              "Sofosbuvir with Velpatasvir (pan-genotypic treatments)" =1,
                                              "Sofosbuvir with Ledipasvir (National List of Essential Medicines)" = 2,
                                              "Sofosbuvir with Ravidasvir (pan-genotypic treatments, on-going clinical trial)" = 3,
                                              "Another durg" = 4),),
                               shinyjs::useShinyjs(),
                               hidden(
                               tags$div(id="Newdurg",
                               numericInput("Input_Tre_Eff", "Treatment Efficacy", 0.5, min = 0, max = 1,step =0.1),
                               numericInput("Input_Tre_Cost", "Cost (THB)", 24000, min = 0,step =100),
                               actionButton("Comfirm_Tre", "Comfirm"),
                               )
                        )
                      ),
                      tags$div(class = "col-sm-12 col-lg-7",
                                tags$table(
                                  tags$tr(
                                    tags$th("Regimen"),
                                    tags$th("HCV genotype 3 (40%)" ,tags$sup("[1]")),
                                    tags$th("HCV genotyp 1 & 6 (60%)" ,tags$sup("[1]")),
                                    tags$th("weighted avg efficay (%)"),
                                    tags$th("Cost (USD)"),
                                    tags$th("cost (THB)"),
                                  ),
                                  tags$tr(
                                    tags$td("Sofosbuvir + velpatasvir"),
                                    tags$td("98 (96-99)"),
                                    tags$td("98 (96-99)"),
                                    tags$td("98"),
                                    tags$td("800" ,tags$sup("[2]")),
                                    tags$td(
                                      sliderInput("Tre1_Cost",
                                                  "",
                                                  min = 10000,
                                                  max = 40000,
                                                  step = 1000,
                                                  value = 24000
                                      )
                                    ),
                                  ),
                                  tags$tr(
                                    tags$td("Sofosbuvir + Ledipasvir"),
                                    tags$td("64 (42 - 82)"),
                                    tags$td("95 (93 - 98)"),
                                    tags$td("82.6"),
                                    tags$td("560" ,tags$sup("[3]")),
                                    tags$td(
                                      sliderInput("Tre2_Cost",
                                                  "",
                                                  min = 10000,
                                                  max = 35000,
                                                  step = 100,
                                                  value = 16800
                                      )
                                    ),
                                  ),
                                  tags$tr(
                                    tags$td("Sofosbuvir + ravidasvir"),
                                    tags$td("97"),
                                    tags$td("81"),
                                    tags$td("87.4"),
                                    tags$td("320" ,tags$sup("[4]")),
                                    tags$td(
                                      sliderInput("Tre3_Cost",
                                                  "",
                                                  min = 5000,
                                                  max = 20000,
                                                  step = 100,
                                                  value = 9600
                                      )
                                    ),
                                  ),
                                ),
                        )
                      ),
                      
                      tags$div(class = "row",
                               tags$div(class = "col-sm-11",id ="TreatmentOutput",
                                        textOutput("text1"),
                                        textOutput("text2"),
                                        textOutput("text3"),
                                        tags$p("1 USD = 30.41 THB")
                                        
                               )
                      ),
                  
             ),
             tags$div(class = "col-sm-12",
                      tags$hr(),
             ),
             tags$h3("Reference"),
             tags$p("1 Wasitthankasem R, Vongpunsawad S, Siripon N, Suya C, Chulothok P, Chaiear K, et al. Genotypic distribution of hepatitis C virus in Thailand and Southeast Asia. PLoS One. 2015;10(5):e0126764."
             ),
             tags$p("2 Andrieux-Meyer I, Tan SS, Salvadori N, Simon F, Cressey TR, Said HRHM, et al. Safety and efficacy of ravidasvir plus sofosbuvir 12 weeks in noncirrhotic and 24 weeks in cirrhotic patients with hepatitis C virus genotypes 1, 2, 3 and 6: The STORM-C-1 pha"
                    ),
             tags$p("3 Rattanavipapong W, Anothaisintawee T, Teerawattananon Y. Revisiting policy on chronic HCV treatment under the Thai Universal Health Coverage: An economic evaluation and budget impact analysis. PLoS One. 2018;13(2):e0193112-e."
                    ),
             tags$p("4 Andrieux-Meyer I, Tan SS, Salvadori N, Simon F, Cressey TR, Said HRHM, et al. Safety and efficacy of ravidasvir plus sofosbuvir 12 weeks in noncirrhotic and 24 weeks in cirrhotic patients with hepatitis C virus genotypes 1, 2, 3 and 6: The STORM-C-1 phase II/III trial. Journal of Hepatology. 2018;68:S123-S4."
                    ),
    ),
    #tab6new
    tabPanel("Extra",
             tags$div(class = "sliderDisplay col-sm-12",
                      tags$div(class = "col-sm-12",
                               checkboxGroupInput("care", "Link to care:",
                                                  c("HCV genotype testing" = 1,
                                                    "Fibroscan stiffness testing" = 2,
                                                    "Relevant and safety lab" = 3,
                                                    "Others" = 4
                                                  ))
                      ),
                      tags$table(
                        tags$tr(
                          tags$th("Diagnostic Test"),
                          tags$th("Test Description"),
                          tags$th("Cost (THB)"),
                        ),
                        tags$tr(
                          tags$td("HCV genotype testing"
                                  ),
                          tags$td("HCV genotype testing is recommended to guide selection of the most appropriate antiviral regimen."
                                  ),
                          tags$td(
                            sliderInput("Extra1_Cost",
                                        "",
                                        min = 300,
                                        max = 6000,
                                        step = 100,
                                        value = 4000
                                      )
                                  ),
                          
                        ),
                        tags$tr(
                          tags$td("Fibroscan stiffness testing"
                          ),
                          tags$td("FibroScan scores can be used to validate advanced fibrosis/cirrhosis for insurance companies, health care plans, or medical centers that require a significant amount of fibrosis before approving or allowing the start of HCV treatment."
                          ),
                          tags$td(
                            sliderInput("Extra2_Cost",
                                        "",
                                        min = 1500,
                                        max = 3000,
                                        step = 100,
                                        value = 2500
                            )
                          ),
                        ),
                        tags$tr(
                          tags$td("Relevant and safety lab"
                          ),
                          tags$td("Relevant and safety lab involves the monitoring of CBC, LFT, and Creatinine levels. APRI, FIB-4 can then be calculated."
                          ),
                          tags$td(
                            sliderInput("Extra3_Cost",
                                        "",
                                        min = 1000,
                                        max = 2000,
                                        step = 100,
                                        value = 1500
                            )
                          ),
                        ),
                        tags$tr(
                          tags$td("Others"
                          ),
                          tags$td("Others"
                          ),
                          tags$td(
                            sliderInput("Extra4_Cost",
                                        "",
                                        min = 1000,
                                        max = 2000,
                                        step = 100,
                                        value = 1500
                            )
                          ),
                        ),
                      )

                ),
                  tags$div(class = "col-sm-12",
                      tags$hr(),
                  ),
                  tags$h3("Reference"),
                  tags$p("1 Rattanavipapong W, Anothaisintawee T, Teerawattananon Y. Revisiting policy on chronic HCV treatment under the Thai Universal Health Coverage: An economic evaluation and budget impact analysis. PLoS One. 2018;13(2):e0193112-e"
                  ),
             ),


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
               ),
    #tab8
    tabPanel("Appendix",
             div(
               tags$p("1. progress 2nd _ 2018 10 03"),
               tags$a(
                 #link
                 href= "https://docs.google.com/presentation/d/1iHAfhqpBh5MpYjf4Sethcq5E3OhoYcyK_nugy2QXnik/edit?usp=sharing"
                 ,"Google Drive" #text
               )
             )
             
    ),
    #tab9
    tabPanel("Cost & Untility",
             div(
               tags$p("Cost"),
               tags$table(
                 tags$tr(
                   tags$th("Screeninng cost"),
                   tags$th("THB"),
                   tags$th(),
                   tags$th(2019),
                   tags$th(2020),
                   tags$th(2021),
                   tags$th(2022),
                   tags$th(2023),
                   tags$th(2024),
                   tags$th(2025),
                   tags$th(2026),
                   tags$th(2027),
                   tags$th(2028),
                   tags$th(2029),
                   tags$th(2030),
                   tags$th(2031),
                   tags$th(2032),
                   tags$th(2033),
                   tags$th(2034),
                   tags$th(2035),
                   tags$th(2036),
                   tags$th(2037),
                   tags$th(2038),
                   tags$th(2039),
                   tags$th(2040),
                 ),
                 tags$tr(
                   tags$td(),
                   tags$td("SCREENING COST"),
                   tags$td("number of screening"),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                 ),
                 tags$tr(
                   tags$td(),
                   tags$td(),
                   tags$td("total cost"),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                 ),
               ),
               
               tags$br(),
               tags$table(
                 tags$tr(
                   tags$th("Direct Treatment cost"),
                   tags$th("THB"),
                   tags$th(),
                   tags$th(2019),
                   tags$th(2020),
                   tags$th(2021),
                   tags$th(2022),
                   tags$th(2023),
                   tags$th(2024),
                   tags$th(2025),
                   tags$th(2026),
                   tags$th(2027),
                   tags$th(2028),
                   tags$th(2029),
                   tags$th(2030),
                   tags$th(2031),
                   tags$th(2032),
                   tags$th(2033),
                   tags$th(2034),
                   tags$th(2035),
                   tags$th(2036),
                   tags$th(2037),
                   tags$th(2038),
                   tags$th(2039),
                   tags$th(2040),
                 ),
                 tags$tr(
                   tags$td(),
                   tags$td("Drug COST"),
                   tags$td("number of treatment"),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                 ),
                 tags$tr(
                   tags$td(),
                   tags$td(),
                   tags$td("total cost"),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                 ),
               ),
               
               tags$br(),
               tags$table(
                 tags$tr(
                   tags$th("Direct medical and non-medical cost"),
                   tags$th("THB"),
                   tags$th(),
                   tags$th(2019),
                   tags$th(2020),
                   tags$th(2021),
                   tags$th(2022),
                   tags$th(2023),
                   tags$th(2024),
                   tags$th(2025),
                   tags$th(2026),
                   tags$th(2027),
                   tags$th(2028),
                   tags$th(2029),
                   tags$th(2030),
                   tags$th(2031),
                   tags$th(2032),
                   tags$th(2033),
                   tags$th(2034),
                   tags$th(2035),
                   tags$th(2036),
                   tags$th(2037),
                   tags$th(2038),
                   tags$th(2039),
                   tags$th(2040),
                 ),
                 tags$tr(
                   tags$td(),
                   tags$td(),
                   tags$td("number of F0-3"),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                 ),
                 tags$tr(
                   tags$td(),
                   tags$td(78151.99),
                   tags$td("total cost Fibrosis"),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                 ),
                 tags$tr(
                   tags$td(),
                   tags$td(),
                   tags$td("number of C1-2"),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                 ),
                 tags$tr(
                   tags$td(),
                   tags$td(86235.97),
                   tags$td("total cost of compensated cirrhosis"),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                 ),
                 tags$tr(
                   tags$td(),
                   tags$td(),
                   tags$td("number of C3-4"),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                 ),
                 tags$tr(
                   tags$td(),
                   tags$td(157755.21),
                   tags$td("total cost ofdecompensated cirrhosis"),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                 ),
                 tags$tr(
                   tags$td(),
                   tags$td(),
                   tags$td("number of HCC"),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                 ),
                 tags$tr(
                   tags$td(),
                   tags$td(197960.51),
                   tags$td("total cost of HCC"),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                 ),
               ),
               
               tags$br(),
               tags$p("Untility"),
               tags$table(
                 tags$tr(
                   tags$th("Health states"),
                   tags$th("Utility"),
                 ),
                 tags$tr(
                   tags$td("Chronic HCV infection"),
                   tags$td("0.73"),
                 ),
                 tags$tr(
                   tags$td("Compensated cirrhosis"),
                   tags$td("0.7"),
                 ),
                 tags$tr(
                   tags$td("Decompensated cirrhosis"),
                   tags$td("0.58"),
                 ),
                 tags$tr(
                   tags$td("Hepatocellular carcinoma"),
                   tags$td("0.58"),
                 ),
               ),
               
               tags$br(),
               tags$table(
                 tags$tr(
                   tags$th(),
                   tags$th(2019),
                   tags$th(2020),
                   tags$th(2021),
                   tags$th(2022),
                   tags$th(2023),
                   tags$th(2024),
                   tags$th(2025),
                   tags$th(2026),
                   tags$th(2027),
                   tags$th(2028),
                   tags$th(2029),
                   tags$th(2030),
                   tags$th(2031),
                   tags$th(2032),
                   tags$th(2033),
                   tags$th(2034),
                   tags$th(2035),
                   tags$th(2036),
                   tags$th(2037),
                   tags$th(2038),
                   tags$th(2039),
                   tags$th(2040),
                 ),
                 tags$tr(
                   tags$td("number of HCV"),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                 ),
                 tags$tr(
                   tags$td("Total utility of HCV"),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                 ),
                 tags$tr(
                   tags$td("number of C1-2"),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                 ),
                 tags$tr(
                   tags$td("total utility of compensated cirrhosis"),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                 ),
                 tags$tr(
                   tags$td("number of C3-4"),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                 ),
                 tags$tr(
                   tags$td("total utility ofdecompensated cirrhosis"),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                 ),
                 tags$tr(
                   tags$td("number of HCC"),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                 ),
                 tags$tr(
                   tags$td("total utility of HCC"),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                   tags$td(),
                 ),
               ),

             ),
             tags$br()
    )
    )
  )
)
