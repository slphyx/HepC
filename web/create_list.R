
setwd("C:/Hep-c/main/web")
getwd()


#name <- c("Cost per test","Sensitivity","Specificity")

Test_list <- list(
                          Ant_HCV = c(20,0.95,0.95),
                          Rapid_HCV_RNA = c(20,0.95,0.95),
                          CORE_Antigen = c(20,0.95,0.95),
                          RNA = c(20,0.95,0.95),
                          Genotype = c(20,0.95,0.95),
                          Fiboscan = c(20,0.95,0.95)
                          )

#for(t in seq_along(Test_list)){

 # names(Test_list[[t]]) <- name
#}

Test_list
save(Test_list, file="Test_list.RData")

load("Test_list.RData")
Test_list$RNA[2] *5  *Test_list$RNA[3]

popscreen_list <- list(
  ag1 =  30000,
  ag2 = 32000,
  ag3 = 62000,
  risk1 =10000,
  risk2 = 20000,
  risk3 =10000,
  risk4 = 20000,
  risk5 =10000
)

save(popscreen_list, file="popscreen_list.RData")

popscreen_list
