
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
