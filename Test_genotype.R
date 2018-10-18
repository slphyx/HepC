library(Rcpp)
library(deSolve)

setwd("C:/hepc")
sourceCpp('p1_genotype.cpp')

require(deSolve)

# encounter error
library.dynam.unload("deSolve", libpath=paste(.libPaths()[1], "//deSolve", sep=""))
library.dynam("deSolve", package="deSolve", lib.loc=.libPaths()[1])

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

inits <- as.data.frame(inits)
year <- c(1999,2016)

times <- seq(year[1], year[2], by = 0.01)

## model
outputs <- ode( y = inits,times =  times, func = PanHepC, parms = parms, method = "rk4")

outputs

plot(outputs)

ggplot(data = outputs_df) + 
  geom_line(mapping = aes(x = 0, y =0)) 

outputs_df_gtime <-outputs_df %>% group_by(time)


ncol(outputs_df)
x <- outputs_df["time"] >= 2000 & outputs_df["time"] <= 2010

outputs_df["time"] >= 2000 
outputs_df[x,]
ggplot(data = outputs_df) + 
    geom_line(mapping = aes(x = time, y = S),size = 1.5)

 
for (x in 1:10) {
 print(x) 
}
col(x)

test_data_long <- melt(outputs_df, id="time")

outputs <- ode( y = inits,times =  times, func = PanHepC, parms = parms, method = "rk4")
outputs_df <- as.data.frame(outputs)
x <- outputs_df[colnames(outputs_df) %in% c("time","S")]
x <- outputs_df[colnames(outputs_df) %in% c("time",c("F0","F1","F2","F3"))]
x <- outputs_df[colnames(outputs_df) %in% c("time",c("C1","C2","C3","C4"))]
x <- outputs_df[colnames(outputs_df) %in% c("time",c("HHC_A","HHC_B","HHC_C","HHC_D"))]
x <- outputs_df[colnames(outputs_df) %in% c("time",c("C1std_cured","C1new_cured"
                                            ,"C2new_cured","C3new_cured","C4new_cured"))]
x <- outputs_df[colnames(outputs_df) %in% c("time",c("death","deathHCC","deathC14"))]
x_melt <-melt(x, id="time")

ggplot(data = x_melt) + 
  geom_line(mapping = aes(x = time, y = value,color = variable),size = 1.5)



