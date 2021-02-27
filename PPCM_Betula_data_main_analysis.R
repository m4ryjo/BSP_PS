library(BART)
library(doParallel)
source("data.R")
source("functions_PPCM_Betula_data.R")

Ndpost <- 28 

k <- 1 # k = 2 in the sensitivity analysis

cl <- makeCluster(14) # number of clusters for parallel computations using the doparallel package
registerDoParallel(cl)

Pop_predictions_mnar_pe <- foreach(icount(72), .combine = rbind,.packages =c("BART")) %dopar% pop_pred_mnar_pe(Ndpost, k)

write.table(Pop_predictions_mnar_pe, "Pop_predictions_main_analysis") 

stopCluster(cl)
registerDoSEQ()

