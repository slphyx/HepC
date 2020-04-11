setwd("H:/OTHERS/MawYim/HepC_model")
setwd("D:/Data-Work/Pan/HepC/7Oct19")
setwd("D:/HepC-Betaweb_2/HepC-Betaweb_2")

library(deSolve)
library(Rcpp)
library(reshape2)
sourceCpp('p1_scr_SS.cpp')


# //get the value of each parameter from parms
# //population parameter
# double K = parms["K"];
# double P0 = parms["P0"];
# double r = parms["r"];
# double flowin = parms["flowin"];
# double caset0 = parms["caset0"];
# // screen and treat parameters
# //standard treatment from 2004, DAAs from 2019
# double std_start = parms["std_start"];
# double new_start = parms["new_start"];
# double nscr = parms["nscr"];
# double scr_yr = parms["scr_yr"];
# double scr_cov = parms["scr_cov"];
# double sens = parms["sens"];
# double pF0scr = parms["pF0scr"];
# double pF1scr = parms["pF1scr"];
# double pF2scr = parms["pF2scr"];
# double pF3scr = parms["pF3scr"];
# double pC1scr = parms["pC1scr"];
# double pC2scr = parms["pC2scr"];
# double pC3scr = parms["pC3scr"];
# double pC4scr = parms["pC4scr"];
# //standard treatment allocation
# double F0std = parms["F0std"];
# double F1std = parms["F1std"];
# double F2std = parms["F2std"];
# double F3std = parms["F3std"];
# double C1std = parms["C1std"];
# //disease progression  
# double f0f1 = parms["f0f1"];
# double f1f2 = parms["f1f2"];
# double f2f3 = parms["f2f3"];
# double f3c1 = parms["f3c1"];
# double c1c2 = parms["c1c2"];
# double c2c3 = parms["c2c3"];
# double c3c4 = parms["c3c4"];
# double c1bA = parms["c1bA"];
# double c1bB = parms["c1bB"];
# double c1bC = parms["c1bC"];
# double c1bD = parms["c1bD"];
# double c2bA = parms["c2bA"];
# double c2bB = parms["c2bB"];
# double c2bC = parms["c2bC"];
# double c2bD = parms["c2bD"];
# double c3bD = parms["c3bD"];
# double c4bD = parms["c4bD"];
# double dthc1 = parms["deathc1"];
# double dthc2 = parms["deathc2"];
# double dthc3 = parms["deathc3"];
# double dthc4 = parms["deathc4"];
# double dthbA = parms["deathbA"];
# double dthbB = parms["deathbB"];
# double dthbC = parms["deathbC"];
# double dthbD = parms["deathbD"];
# double dthtrn = parms["deathtrn"];
# double tranc4 = parms["tranc4"];
# double tranbA = parms["tranbA"];
# double tranbB = parms["tranbB"];
# //natural cause of death
# double natdeath = parms["natdeath"];

# double beta = parms["beta"];

# double std_cureF0 = parms["std_cureF0"];
# double std_cureF1 = parms["std_cureF1"];
# double std_cureF2 = parms["std_cureF2"];
# double std_cureF3 = parms["std_cureF3"];
# double std_cureC1 = parms["std_cureC1"];

# double new_cureF0 = parms["new_cureF0"];
# double new_cureF1 = parms["new_cureF1"];
# double new_cureF2 = parms["new_cureF2"];
# double new_cureF3 = parms["new_cureF3"];
# double new_cureC1 = parms["new_cureC1"];
# double new_cureC2 = parms["new_cureC2"];
# double new_cureC3 = parms["new_cureC3"];
# double new_cureC4 = parms["new_cureC4"];

scr_yr = 10
parms <-
  list(
    P0=61623143,       #popstat(YEAR=1999)
    K= 68508515,        #Maximum population (carrying capacity)
    r = 0.16,        #Population growth rate (logistic growth curve)
    flowin = 1.15*(10^-8),
    caset0 =  0.03*61623143,      #0.03*P0,
    standard_start = 5,
    new_start = 20,
    nscr = 0.05,
    scr_yr = 10,
    scr_cov = (23590+143320)/scr_yr,      #0.9/scr.yr,
    sens = 0.985,
    pF0scr = 0.1439,
    pF1scr = 0.2969,
    pF2scr=0.1098,
    pF3scr=0.1466,
    pC1scr=0.1998,
    pC2scr=0.0819,
    pC3scr=0.0096,
    pC4scr=0.0011,
    
    #Natural rate of death
    natdeath=0.04, #Unrelated to cirrhosis and hepatitis C
    
    beta= 0.02,              #Transmission coefficient
    
    #standard treatment allocation
    F0std = 0.05,
    F1std = 0.05,
    F2std = 0.3,
    F3std = 0.3,
    C1std = 0.3,
    
    
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
    
    deathtran=1/(240/12),
    
    #Transplantation
    tranc4=0.0015, #Transplantation rate in cirrhosis stage C4
    tranbA=0.0015, #Transplantation rate in HCC_BCLC_A
    tranbB=0.0015, #Transplantation rate in HCC_BCLC_B
    
    #Treatment efficacy
    #Standard treatment response based on genotype (weighted average)
    std_cureF0=0.7,
    std_cureF1 =0.7,
    std_cureF2 =0.7,
    std_cureF3 =0.7,
    std_cureC1 =0.7,
    std_cureC2=0.7,
    #Novel treatment response based on genotype (weighted average)
    new_cureF0= 0.98,
    new_cureF1= 0.98,
    new_cureF2  = 0.98,
    new_cureF3 = 0.98,
    new_cureC1 = 0.98,
    new_cureC2 = 0.98,
    new_cureC3 = 0.98,
    new_cureC4 = 0.98
    
  )


# inits
  P0 <- 61623143 #1999
  caset0 <- 0.03*P0 #From BM code
  #set up initial S compartment
  initS<- 1*(P0-caset0) #0.001
  #set up ininitSitial I compartments (values from BM code)
  initF0 <- 0.2825*caset0
  initF1 <- 0.2825*caset0
  initF2 <- 0.184*caset0
  initF3 <- 0.124*caset0
  initC1 <- 0.03175*caset0
  initC2 <- 0.03175*caset0
  initC3 <- 0.03175*caset0
  initC4 <- 0.03174*caset0
  initHCCA <- 0
  initHCCB <- 0
  initHCCC <- 0
  initHCCD <- 0

  initC1std_cured<-0  
  initC1new_cured<-0
  initC2new_cured<-0
  initC3new_cured<-0
  initC4new_cured<-0
  #set up initial death
  initD <- 0
  initdthC14<-0
  initdthHCC<-0
  initDHCC <- 0
  initDC14 <- 0
  
  initS<- 1*(P0-caset0)
  
#set up initial  
init <- c(S=initS,
          F0=initF0,
          F1=initF1,
          F2=initF2,
          F3=initF3,
          C1=initC1,
          C2=initC2,
          C3=initC3,
          C4=initC4,
          HCC_A=initHCCA,
          HCC_B=initHCCB,
          HCC_C=initHCCC,
          HCC_D=initHCCD,
          D=initD,
          dthC14=initdthC14
          ,dthHCC=initdthHCC
          ,C1std_cured=initC1std_cured,
          C1new_cured=initC1new_cured,
          C2new_cured=initC2new_cured,
          C3new_cured=initC3new_cured,
          C4new_cured=initC4new_cured)

#set up time
simu.time <- seq(0, 60,by=1) 
setwd("D:/PAN/")
# solve ode
out <- ode(y = init, times = simu.time, func=PanHepC, parms = parms)
write.csv(out,"output_cpp_2.csv")

#plotting
plot(out, select=c("prevalence","incidenceHCC"))

out_df <- as.data.frame(out)

out_df

#Choose column
x <- out_df[colnames(out_df) %in% c("time","S")]
x <- out_df[colnames(out_df) %in% c("time",c("F0","F1","F2","F3"))]
x <- out_df[colnames(out_df) %in% c("time",c("C1","C2","C3","C4"))]
x <- out_df[colnames(out_df) %in% c("time",c("HHC_A","HHC_B","HHC_C","HHC_D"))]
x <- out_df[colnames(out_df) %in% c("time",c("C1std_cured","C1new_cured"
                                                     ,"C2new_cured","C3new_cured","C4new_cured"))]
x <- out_df[colnames(out_df) %in% c("time",c("death","deathHCC","deathC14"))]
x <- out_df[colnames(out_df) %in% c("time","prevalence")]

ggplot(data = x_melt) + 
  geom_line(mapping = aes(x = time, y = prevalence),size = 1.5)

x_melt <-melt(x, id="time")

ggplot(data = x_melt) + 
  geom_line(mapping = aes(x = time, y = value,color = variable),size = 1.5)

times <- seq(0, 60,by=1) 

out <- ode( y = inits(),times =  times, func = PanHepC, parms = parms(), method = "rk4")

out_df <- as.data.frame(out)

out_df

x <- out_df[,c(1,23)]

outSS <- as.data.frame(ode(y = init, times = times, func=PanHepC, parms = parms))


library(ggplot2)
A = data.frame(x = rnorm(10),y=rnorm(10))
B = data.frame(x = rnorm(10),y=rnorm(10))
ggplot(A,aes(x,y)) +geom_line() +geom_line(data=B,colour='red',linetype = "dashed") + xlim(0, 3) 
