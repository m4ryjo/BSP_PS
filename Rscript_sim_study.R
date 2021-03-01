library(MASS)
library(doParallel)
library(BART)
library(MCMCpack)
library(sn)

source("functions_sim_study.R")

###############################################3
# N posterior samples
N <- 1000
# Detect and set number of clusters to run
N_clus <- detectCores()
cl <- makeCluster(N_clus)
registerDoParallel(cl)

#### BART scenario 1
results_TTF_BART_sparse <- foreach(icount(N), .packages =c("MASS","BART","MCMCpack")) %dopar% sim_BART(10000,1000,1000,"TTF_norm","BART_TTF_sparse")

#### BART scenario 2
results_TFF_BART_sparse <- foreach(icount(N), .packages =c("MASS","BART","MCMCpack")) %dopar% sim_BART(10000,1000,1000,"TFF_norm","BART_TFF_sparse")

#### BART scenario 3
results_FFF_BART_sparse <- foreach(icount(N), .packages =c("MASS","BART","MCMCpack","sn")) %dopar% sim_BART(10000,1000,1000,"FFF_sn","BART_FFF_sparse")

#### BART scenario 4
# Set upper bound (and mode) for the prior
pe <- 0
results_FFF_pe_BART_sparse <- foreach(icount(N), .packages =c("MASS","BART","MCMCpack","sn")) %dopar% sim_BART_pe(10000,1000,1000, pe,"BART_FFF_sparse_pe")

#### BART scenario 5
results_FFF_BART_sparse_death <- foreach(icount(N), .packages =c("MASS","BART","MCMCpack","sn")) %dopar% sim_BART(10000,1000,1000,"FFF_sn_death","BART_FFF_sparse_deaths")

stopCluster(cl)
registerDoSEQ()
