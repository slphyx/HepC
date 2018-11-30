library(Rcpp)
library(deSolve)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(scales)
library(plyr)
require(deSolve)

# encounter error
library.dynam.unload("deSolve", libpath=paste(.libPaths()[1], "//deSolve", sep=""))
library.dynam("deSolve", package="deSolve", lib.loc=.libPaths()[1])

#set Working Directory
setwd("C:/Hep-c/main")
getwd()

#parses the specified C++ file
sourceCpp('p1_scenario_backup.cpp')


#Parameters
parms <- list(
  K=66785001,
  P0=61623140, #popstat(YEAR=1999)
  r =0.16,
  S0= 0.0305853,
  standard_start = 2004,
  new_start = 2015,
  
  
  #cost = treated1+treated2
  total_HCC=0,
  total_HCV=631000,
  
  FI=10^-8,
  
  f0f1=0.117,
  f1f2=0.085,
  f2f3=0.12,
  f3c1=0.116,
  c1c2=0.044,
  c2c3=0.076,
  c3c4=0.076,
  c1bA=0.0068,
  c1bB=0.0099,
  c1bC=0.0029,
  c1bD=0.0068,
  
  c2bA=0.0068,
  c2bB=0.0099,
  c2bC=0.0029,
  c2bD=0.0068,
  
  c3bD=0.0664,
  c4bD=0.0664,
  deathc1=0.01,
  deathc2=0.2,
  deathc3=0.57,
  deathc4=0.57,
  deathbA=1/(36/12),
  deathbB=1/(16/12),
  deathbC=1/(6/12),
  deathbD=1/(3/12),
  deathtrn=1/(240/12),
  tranc4=0.0015,
  tranbA=0.0015,
  tranbB=0.0015,
  #;newcase=17000,
  cover = 5,
  natdeath=0.0424,
  
  beta=0.327,
  std_cureF0=c(0,0,0,0,0,0),
  std_cureF1 =c(0,0,0,0,0,0),
  std_cureF2 =c(0,0,0,0,0,0),
  std_cureF3 =c(0,0,0,0,0,0),
  std_cureC1 =c(0,0,0,0,0,0),
  std_cureC2=c(0,0,0,0,0,0),
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

S <- parms$S0* parms$P0;

#Initial
inits <- c(
  S_g1 = S, #;0.01*#pop_since1960(TIME=1)
  S_g2 = S,
  S_g3 = S,
  S_g4 = S,
  S_g5 = S,
  S_g6 = S,
  
  F0_g1 = 0.2825*S,
  F0_g2 = 0.2825*S,
  F0_g3 = 0.2825*S,
  F0_g4 = 0.2825*S,
  F0_g5 = 0.2825*S,
  F0_g6 = 0.2825*S,
  
  F1_g1 = 0.2825*S,
  F1_g2 = 0.2825*S,
  F1_g3 = 0.2825*S,
  F1_g4 = 0.2825*S,
  F1_g5 = 0.2825*S,
  F1_g6 = 0.2825*S,
  
  F2_g1 = 0.184*S,
  F2_g2 = 0.184*S,
  F2_g3 = 0.184*S,
  F2_g4 = 0.184*S,
  F2_g5 = 0.184*S,
  F2_g6 = 0.184*S,
  
  F3_g1 = 0.124*S,
  F3_g2 = 0.124*S,
  F3_g3 = 0.124*S,
  F3_g4 = 0.124*S,
  F3_g5 = 0.124*S,
  F3_g6 = 0.124*S,
  
  # ;CirA
  C1_g1 = 0.03175*S,
  C1_g2 = 0.03175*S,
  C1_g3 = 0.03175*S,
  C1_g4 = 0.03175*S,
  C1_g5 = 0.03175*S,
  C1_g6 = 0.03175*S,
  
  # ;CirA
  C2_g1 = 0.03175*S,
  C2_g2 = 0.03175*S,
  C2_g3 = 0.03175*S,
  C2_g4 = 0.03175*S,
  C2_g5 = 0.03175*S,
  C2_g6 = 0.03175*S,
  
  # ;CirB
  C3_g1 = 0.03175*S,
  C3_g2 = 0.03175*S,
  C3_g3 = 0.03175*S,
  C3_g4 = 0.03175*S,
  C3_g5 = 0.03175*S,
  C3_g6 = 0.03175*S,
  
  # ;CirC
  C4_g1 = 0.03175*S,
  C4_g2 = 0.03175*S,
  C4_g3 = 0.03175*S,
  C4_g4 = 0.03175*S,
  C4_g5 = 0.03175*S,
  C4_g6 = 0.03175*S,
  
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

year <- c(1999,2016)

times <- seq(year[1], year[2], by = 0.01)

## model
outputs <- ode( y = inits,times =  times, func = PanHepC, parms = parms, method = "rk4")

outputs

plot(outputs)

#convert to data frame
outputs_df <- as.data.frame(outputs)

#pie chart
#propF0_genotype
x_pie <- outputs_df[colnames(outputs_df) %in% c("time",c("propF0_genotype_g1","propF0_genotype_g2","propF0_genotype_g3","propF0_genotype_g4","propF0_genotype_g5","propF0_genotype_g6"))]
x_time <- outputs_df["time"] == 2000
x_pie <- x_pie[x_time,]
x_pie <- x_pie[colnames(x_pie)  %in% c("propF0_genotype_g1","propF0_genotype_g2","propF0_genotype_g3","propF0_genotype_g4","propF0_genotype_g5","propF0_genotype_g6")]
pie(unlist(x_pie))

x<- unlist(x_pie)
piepercent<- round(100*x/sum(x), 1)
pie(unlist(x_pie),labels = piepercent)

#pie chart(ggplot2)
x_pie <- outputs_df[colnames(outputs_df) %in% c("time",c("propF0_genotype_g1","propF0_genotype_g2","propF0_genotype_g3","propF0_genotype_g4","propF0_genotype_g5","propF0_genotype_g6"))]
x_melt_pie <-melt(x_pie, id="time")
x_time <- outputs_df["time"] == 2000
x_melt_pie <- x_melt_pie[x_time,]

gl2_pie <- ggplot(x_melt_pie, aes(x="", y=value, fill=variable))+
              geom_bar(width = 1, stat = "identity") + 
              coord_polar("y", start=0)+  scale_fill_brewer(palette="Blues") + theme_minimal() +  
              theme(axis.text.x=element_blank()) +
              geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
              label = percent(value)), size=3)
              
gl2_pie

#end pie chart


#Choose column
x <- outputs_df[colnames(outputs_df) %in% c("time","S")]
x <- outputs_df[colnames(outputs_df) %in% c("time",c("prev_g1","prev_g2","prev_g3","prev_g4","prev_g5","prev_g6"))]
x <- outputs_df[colnames(outputs_df) %in% c("time",c("C1","C2","C3","C4"))]
x <- outputs_df[colnames(outputs_df) %in% c("time",c("HHC_A","HHC_B","HHC_C","HHC_D"))]
x <- outputs_df[colnames(outputs_df) %in% c("time",c("C1std_cured","C1new_cured"
                                            ,"C2new_cured","C3new_cured","C4new_cured"))]
x <- outputs_df[colnames(outputs_df) %in% c("time",c("death","deathHCC","deathC14"))]

#The melt function takes data in wide format 
#and stacks a set of columns into a single column of data.
x_melt <-melt(x, id="time")

#plot graph(ggplot2) 
ggplot(data = x_melt) + 
  geom_line(mapping = aes(x = time, y = value,color = variable),size = 1.5)

#test read csv
heisenberg <- read.csv(file="simple.csv",head=TRUE,sep=",")
heisenberg[1,3]

tree <- read.csv(file="parameter.csv",header=TRUE,sep=",")
tree_df <- as.data.frame(tree)
tree <- read.csv(file="Input1.csv",header=TRUE,sep=",")

tree_l <- dlply(tree,1,c)

test_xlsx <- read_xlsx("Input.xlsx")

parms_xlsx <- list(
  K= test_xlsx$K,
  P0= test_xlsx$P0, #popstat(YEAR=1999)
  r = test_xlsx$r,
  S0= test_xlsx$S0,
  standard_start = test_xlsx$standard_start,
  new_start = test_xlsx$new_start,
  
  
  #cost = treated1+treated2
  total_HCC= test_xlsx$total_HCC,
  total_HCV= test_xlsx$total_HCV,
  
  FI= test_xlsx$FI,
  
  f0f1= test_xlsx$f0f1,
  f1f2= test_xlsx$f1f2,
  f2f3= test_xlsx$f2f3,
  f3c1= test_xlsx$f3c1,
  c1c2= test_xlsx$c1c2,
  c2c3= test_xlsx$c2c3,
  c3c4= test_xlsx$c3c4,
  c1bA= test_xlsx$c1bA,
  c1bB= test_xlsx$c1bB,
  c1bC= test_xlsx$c1bC,
  c1bD= test_xlsx$c1bD,
  
  c2bA= test_xlsx$c2bA,
  c2bB= test_xlsx$c2bB,
  c2bC= test_xlsx$c2bC,
  c2bD= test_xlsx$c2bD,
  
  c3bD= test_xlsx$c3bD,
  c4bD= test_xlsx$c4bD,
  deathc1=test_xlsx$deathc1,
  deathc2=test_xlsx$deathc2,
  deathc3=test_xlsx$deathc3,
  deathc4=test_xlsx$deathc4,
  deathbA=test_xlsx$deathbA,
  deathbB=test_xlsx$deathbB,
  deathbC=test_xlsx$deathbC,
  deathbD=test_xlsx$deathbD,
  deathtrn=test_xlsx$deathtrn,
  tranc4=test_xlsx$tranc4,
  tranbA=test_xlsx$tranbA,
  tranbB=test_xlsx$tranbB,
  #;newcase=17000,
  cover = test_xlsx$cover,
  natdeath=test_xlsx$natdeath,
  
  beta=test_xlsx$beta,
  std_cureF0 = c(test_xlsx$std_cureF0_G1 , test_xlsx$std_cureF0_G2 , test_xlsx$std_cureF0_G3 , test_xlsx$std_cureF0_G4 , test_xlsx$std_cureF0_G5 , test_xlsx$std_cureF0_G6 ),
  std_cureF1 = c(test_xlsx$std_cureF1_G1 , test_xlsx$std_cureF1_G2 , test_xlsx$std_cureF1_G3 , test_xlsx$std_cureF1_G4 , test_xlsx$std_cureF1_G5 , test_xlsx$std_cureF1_G6 ),
  std_cureF2 = c(test_xlsx$std_cureF2_G1 , test_xlsx$std_cureF2_G2 , test_xlsx$std_cureF2_G3 , test_xlsx$std_cureF2_G4 , test_xlsx$std_cureF2_G5 , test_xlsx$std_cureF2_G6 ),
  std_cureF3 = c(test_xlsx$std_cureF3_G1 , test_xlsx$std_cureF3_G2 , test_xlsx$std_cureF3_G3 , test_xlsx$std_cureF3_G4 , test_xlsx$std_cureF3_G5 , test_xlsx$std_cureF3_G6 ),
  std_cureC1 = c(test_xlsx$std_cureC1_G1 , test_xlsx$std_cureC1_G2 , test_xlsx$std_cureC1_G3 , test_xlsx$std_cureC1_G4 , test_xlsx$std_cureC1_G5 , test_xlsx$std_cureC1_G6 ),
  std_cureC2 = c(test_xlsx$std_cureC2_G1 , test_xlsx$std_cureC2_G2 , test_xlsx$std_cureC2_G3 , test_xlsx$std_cureC2_G4 , test_xlsx$std_cureC2_G5 , test_xlsx$std_cureC2_G6 ),
  
  new_cureF0 = c(test_xlsx$new_cureF0_G1 , test_xlsx$new_cureF0_G2 , test_xlsx$new_cureF0_G3 , test_xlsx$new_cureF0_G4 , test_xlsx$new_cureF0_G5 , test_xlsx$new_cureF0_G6 ),
  new_cureF1 = c(test_xlsx$new_cureF1_G1 , test_xlsx$new_cureF1_G2 , test_xlsx$new_cureF1_G3 , test_xlsx$new_cureF1_G4 , test_xlsx$new_cureF1_G5 , test_xlsx$new_cureF1_G6 ),
  new_cureF2 = c(test_xlsx$new_cureF2_G1 , test_xlsx$new_cureF2_G2 , test_xlsx$new_cureF2_G3 , test_xlsx$new_cureF2_G4 , test_xlsx$new_cureF2_G5 , test_xlsx$new_cureF2_G6 ),
  new_cureF3 = c(test_xlsx$new_cureF3_G1 , test_xlsx$new_cureF3_G2 , test_xlsx$new_cureF3_G3 , test_xlsx$new_cureF3_G4 , test_xlsx$new_cureF3_G5 , test_xlsx$new_cureF3_G6 ),
  new_cureC1 = c(test_xlsx$new_cureC1_G1 , test_xlsx$new_cureC1_G2 , test_xlsx$new_cureC1_G3 , test_xlsx$new_cureC1_G4 , test_xlsx$new_cureC1_G5 , test_xlsx$new_cureC1_G6 ),
  new_cureC2 = c(test_xlsx$new_cureC2_G1 , test_xlsx$new_cureC2_G2 , test_xlsx$new_cureC2_G3 , test_xlsx$new_cureC2_G4 , test_xlsx$new_cureC2_G5 , test_xlsx$new_cureC2_G6 ),
  new_cureC3 = c(test_xlsx$new_cureC3_G1 , test_xlsx$new_cureC3_G2 , test_xlsx$new_cureC3_G3 , test_xlsx$new_cureC3_G4 , test_xlsx$new_cureC3_G5 , test_xlsx$new_cureC3_G6 ),
  new_cureC4 = c(test_xlsx$new_cureC4_G1 , test_xlsx$new_cureC4_G2 , test_xlsx$new_cureC4_G3 , test_xlsx$new_cureC4_G4 , test_xlsx$new_cureC4_G5 , test_xlsx$new_cureC4_G6 ),
  
  std_dist = c(test_xlsx$std_dist1 , test_xlsx$std_dist2 , test_xlsx$std_dist3 , test_xlsx$std_dist4 , test_xlsx$std_dist5 )
  
)

data <- as.data.frame(parms_xlsx)
