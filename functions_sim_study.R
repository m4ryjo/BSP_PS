##############################################
## functions used in Rscript_sim_study_PS.R ##
##############################################

expit <- function(p){
    exp(p)/(1+exp(p))
}

###################################
## no output from functions
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
}

#####################################################33
sim_data_TTF_norm <- function(n_pop,n_surv){
  X <- data.frame(matrix(c(runif(n = n_pop,-1,1),
                           runif(n = n_pop,-1,1),
                           rbinom(n = n_pop, size = 1, prob = 0.5),
                           rbinom(n = n_pop, size = 1, prob = 0.5),
                           runif(n = n_pop,-1,1),
                           runif(n = n_pop,-1,1),
                           runif(n = n_pop,-1,1),
                           runif(n = n_pop,-1,1)),
                         nrow = n_pop, ncol = 8))
  p_c <- expit(-2.358 + 0.15*X[,1] + 0.15*X[,2] + 0.15*X[,3]+ 0.15*X[,4])
  samp <- sample(1:n_pop, size=n_surv, prob = p_c)
  survsamp_T1 <- X[samp,]
  
  y1_hat_pop <- - 1 + X[,1] + X[,2] + X[,3] + X[,4]
  y1_pop <- rnorm(n=n_pop,mean=y1_hat_pop,sd=1)
  y1 <- y1_pop[samp]
  
  p_dropout <- expit(0.785 + 0.35*survsamp_T1[,1] + 0.35*survsamp_T1[,2] + 0.35*survsamp_T1[,3]+ 0.35*survsamp_T1[,4] + 0.3*y1)
  dropout <- rbinom(size = 1, n = n_surv, p = p_dropout)
  survsamp_T2 <- survsamp_T1[dropout==1,]
  
  y2_hat_pop <- -0.8 - 0.8*X[,1] + 0.8*X[,2] + 0.8*X[,3] + 0.8*X[,4] + 0.2*y1_pop
  y2_pop <- rnorm(n=n_pop,mean=y2_hat_pop,sd=1)
  y2 <- y2_pop[samp[dropout==1]]
  
  survsamp_T2 <- cbind(survsamp_T2, y1[dropout==1])
  colnames(survsamp_T2) <- c("X1", "X2", "X3", "X4","X5", "X6", "X7", "X8", "y1")
  
  return(list("X"=X,  "survsamp_T1"=survsamp_T1, "survsamp_T2"=survsamp_T2, "dropout"=dropout, "y1"=y1, "y2"=y2, "y2_pop"=y2_pop))
}

sim_data_TFF_norm <- function(n_pop,n_surv){
  X <- data.frame(matrix(c(runif(n = n_pop,-1,1),
                           runif(n = n_pop,-1,1),
                           rbinom(n = n_pop, size = 1, prob = 0.5),
                           rbinom(n = n_pop, size = 1, prob = 0.5),
                           runif(n = n_pop,-1,1),
                           runif(n = n_pop,-1,1),
                           runif(n = n_pop,-1,1),
                           runif(n = n_pop,-1,1)),
                         nrow = n_pop, ncol = 8))
  p_c <- expit(-2.358 + 0.15*X[,1] + 0.15*X[,2] + 0.15*X[,3]+ 0.15*X[,4])
  samp <- sample(1:n_pop, size=n_surv, prob = p_c)
  survsamp_T1 <- X[samp,]
  
  y1_hat_pop <- - 1 + X[,1] + X[,2] + X[,3] + X[,4]
  y1_pop <- rnorm(n=n_pop,mean=y1_hat_pop,sd=1)
  y1 <- y1_pop[samp]
  
  p_dropout <- expit(0.785 +  0.5*survsamp_T1[,1] + 0.5*survsamp_T1[,2] + 0.5*survsamp_T1[,3] + 
                       0.5*survsamp_T1[,1]*survsamp_T1[,2]+ 0.5*survsamp_T1[,1]*survsamp_T1[,3] + 
                       0.5*survsamp_T1[,4] + 0.5*y1 - 0.5*y1*survsamp_T1[,3])
  dropout <- rbinom(size = 1, n = n_surv, p = p_dropout)
  survsamp_T2 <- survsamp_T1[dropout==1,]
  
  y2_hat_pop <- - 0.8 - 0.8*X[,1] + 0.8*X[,2] + 0.8*X[,3] + 0.8*X[,4] + 0.2*y1_pop
  y2_pop <- rnorm(n=n_pop,mean=y2_hat_pop,sd=1)
  y2 <- y2_pop[samp[dropout==1]]
  survsamp_T2 <- cbind(survsamp_T2, y1[dropout==1])
  colnames(survsamp_T2) <- c("X1", "X2", "X3", "X4","X5", "X6", "X7", "X8", "y1")
  
  return(list("X"=X,  "survsamp_T1"=survsamp_T1, "survsamp_T2"=survsamp_T2, "dropout"=dropout, "y1"=y1, "y2"=y2, "y2_pop"=y2_pop))
  
}

sim_data_FFF_sn <- function(n_pop,n_surv){
  X <- data.frame(matrix(c(runif(n = n_pop,-1,1),
                           runif(n = n_pop,-1,1),
                           rbinom(n = n_pop, size = 1, prob = 0.5),
                           rbinom(n = n_pop, size = 1, prob = 0.5),
                           runif(n = n_pop,-1,1),
                           runif(n = n_pop,-1,1),
                           runif(n = n_pop,-1,1),
                           runif(n = n_pop,-1,1)),
                         nrow = n_pop, ncol = 8))
  p_c <- expit(-2.358 + 0.15*X[,1] + 0.15*X[,2] + 0.15*X[,3]+ 0.15*X[,4])
  samp <- sample(1:n_pop, size=n_surv, prob = p_c)
  survsamp_T1 <- X[samp,]
  
  y1_hat_pop <- - 1 + X[,1] + X[,2] + X[,3] + X[,4]
  #y1_pop <- rnorm(n=n_pop,mean=y1_hat_pop,sd=SD)
  y1_pop <- y1_hat_pop + rsn(n=n_pop,xi=-1.6*(5/sqrt(1+25))*sqrt(2/pi), omega=1.6, alpha=5)
  y1 <- y1_pop[samp]
  
  p_dropout <- expit(0.785 +  0.5*survsamp_T1[,1] + 0.5*survsamp_T1[,2] + 0.5*survsamp_T1[,3] + 
                       0.5*survsamp_T1[,1]*survsamp_T1[,2]+ 0.5*survsamp_T1[,1]*survsamp_T1[,3] + 
                       0.5*survsamp_T1[,4] + 0.5*y1 - 0.5*y1*survsamp_T1[,3])
  dropout <- rbinom(size = 1, n = n_surv, p = p_dropout)
  survsamp_T2 <- survsamp_T1[dropout==1,]
  
  #y2_hat_pop <- -0.87 - 0.4*X[,1] + 0.8*(X[,1])^2 - 0.8*(X[,1])^3 + 0.4*X[,2] + 0.8*X[,3] + 0.8*X[,4] + 0.4*y1_pop - 0.8 * X[,3]*y1_pop
  y2_hat_pop <- -0.42 + 0.5*X[,1] - 1*(X[,1])^2 + 1*(X[,1])^3 + 1*X[,2] + 1*X[,3] + 1*X[,4] + 0.5*y1_pop - 1 * X[,3]*y1_pop
  #y2_pop <- rnorm(n=n_pop,mean=y2_hat_pop,sd=SD)
  y2_pop <- y2_hat_pop + rsn(n=n_pop,xi=-1.6*(5/sqrt(1+25))*sqrt(2/pi), omega=1.6, alpha=5)
  y2 <- y2_pop[samp[dropout==1]]
  survsamp_T2 <- cbind(survsamp_T2, y1[dropout==1])
  colnames(survsamp_T2) <- c("X1", "X2", "X3", "X4","X5", "X6", "X7", "X8", "y1")
  
  return(list("X"=X, "survsamp_T1"=survsamp_T1, "survsamp_T2"=survsamp_T2, "dropout"=dropout, "y1"=y1, "y2"=y2, "y2_pop"=y2_pop))
  }

sim_data_FFF_pe <- function(n_pop,n_surv){
  X <- data.frame(matrix(c(runif(n = n_pop,-1,1),
                           runif(n = n_pop,-1,1),
                           rbinom(n = n_pop, size = 1, prob = 0.5),
                           rbinom(n = n_pop, size = 1, prob = 0.5),
                           runif(n = n_pop,-1,1),
                           runif(n = n_pop,-1,1),
                           runif(n = n_pop,-1,1),
                           runif(n = n_pop,-1,1)),
                         nrow = n_pop, ncol = 8))
  p_c <- expit(-2.358 + 0.15*X[,1] + 0.15*X[,2] + 0.15*X[,3]+ 0.15*X[,4])
  samp <- sample(1:n_pop, size=n_surv, prob = p_c)
  survsamp_T1 <- X[samp,]
  
  y1_hat_pop <- - 1 + X[,1] + X[,2] + X[,3] + X[,4]
  y1_pop <- y1_hat_pop + rsn(n=n_pop,xi=-1.6*(5/sqrt(1+25))*sqrt(2/pi), omega=1.6, alpha=5)
  y1 <- y1_pop[samp]
  
  p_dropout <- expit(0.785 +  0.5*survsamp_T1[,1] + 0.5*survsamp_T1[,2] + 0.5*survsamp_T1[,3] + 
                       0.5*survsamp_T1[,1]*survsamp_T1[,2]+ 0.5*survsamp_T1[,1]*survsamp_T1[,3] + 
                       0.5*survsamp_T1[,4] + 0.5*y1 - 0.5*y1*survsamp_T1[,3])
  dropout <- rbinom(size = 1, n = n_surv, p = p_dropout)
   
  y2_hat_pop <- -0.32 + 0.5*X[,1] - 1*(X[,1])^2 + 1*(X[,1])^3 + 1*X[,2] + 1*X[,3] + 1*X[,4] + 0.5*y1_pop - 1 * X[,3]*y1_pop
  y2_pop <- y2_hat_pop + rsn(n=n_pop,xi=-1.6*(5/sqrt(1+25))*sqrt(2/pi), omega=1.6, alpha=5)
  y2 <- y2_pop[samp[dropout==1]]
  #pe <- mean(y2-y1_pop[samp[dropout==1]])
  survsamp_T2 <- cbind(survsamp_T1, y1)[dropout==1,]
  colnames(survsamp_T2) <- c("X1", "X2", "X3", "X4","X5", "X6", "X7", "X8", "y1")
  
  return(list("X"=X, "survsamp_T1"=survsamp_T1, "survsamp_T2"=survsamp_T2, "dropout"=dropout, "y1"=y1, "y2"=y2, "y2_pop"=y2_pop))# , "pe"=pe))
}

sim_data_FFF_sn_death <- function(n_pop,n_surv){
  X <- data.frame(matrix(c(runif(n = n_pop,-1,1),
                           runif(n = n_pop,-1,1),
                           rbinom(n = n_pop, size = 1, prob = 0.5),
                           rbinom(n = n_pop, size = 1, prob = 0.5),
                           runif(n = n_pop,-1,1),
                           runif(n = n_pop,-1,1),
                           runif(n = n_pop,-1,1),
                           runif(n = n_pop,-1,1)),
                         nrow = n_pop, ncol = 8))
  
  p_survival <- expit(1.7 + 0.35*X[,1] + 0.35*X[,2] + 0.35*X[,3]+ 0.35*X[,4])
  survival <- rbinom(size = 1, n = n_pop, p = p_survival)
  X_T2 <- X[survival==1,]
  
  p_c <- expit(-2.358 + 0.15*X_T2[,1] + 0.15*X_T2[,2] + 0.15*X_T2[,3]+ 0.15*X_T2[,4])
  samp <- sample(1:nrow(X_T2), size=n_surv, prob = p_c)
  survsamp_T1 <- X_T2[samp,]
  
  y1_hat_pop <- - 1 + X_T2[,1] + X_T2[,2] + X_T2[,3] + X_T2[,4]
  y1_pop <- y1_hat_pop + rsn(n=nrow(X_T2),xi=-1.6*(5/sqrt(1+25))*sqrt(2/pi), omega=1.6, alpha=5)
  y1 <- y1_pop[samp]
  
  p_dropout <- expit(0.785 +  0.5*survsamp_T1[,1] + 0.5*survsamp_T1[,2] + 0.5*survsamp_T1[,3] + 
                       0.5*survsamp_T1[,1]*survsamp_T1[,2]+ 0.5*survsamp_T1[,1]*survsamp_T1[,3] + 
                       0.5*survsamp_T1[,4] + 0.5*y1 - 0.5*y1*survsamp_T1[,3])
  dropout <- rbinom(size = 1, n = n_surv, p = p_dropout)
  survsamp_T2 <- survsamp_T1[dropout==1,]
  
  y2_hat_pop <- -0.42 + 0.5*X_T2[,1] - 1*(X_T2[,1])^2 + 1*(X_T2[,1])^3 + 1*X_T2[,2] + 1*X_T2[,3] + 1*X_T2[,4] + 0.5*y1_pop - 1 * X_T2[,3]*y1_pop
  y2_pop <- y2_hat_pop + rsn(n=nrow(X_T2),xi=-1.6*(5/sqrt(1+25))*sqrt(2/pi), omega=1.6, alpha=5)
  y2 <- y2_pop[samp[dropout==1]]
  survsamp_T2 <- cbind(survsamp_T2, y1[dropout==1])
  colnames(survsamp_T2) <- c("X1", "X2", "X3", "X4","X5", "X6", "X7", "X8", "y1")
  
  return(list("X"=X_T2, "survsamp_T1"=survsamp_T1, "survsamp_T2"=survsamp_T2, "dropout"=dropout, "y1"=y1, "y2"=y2, "y2_hat_pop"=y2_hat_pop))
}

#####################################################
## sample inference
sim_sample <- function(n_pop, n_surv, scenario){    
  if(scenario=="TTF_norm"){
    dat <- sim_data_TTF_norm(n_pop,n_surv)
  } else if(scenario=="TFF_norm"){
    dat <- sim_data_TFF_norm(n_pop,n_surv)
  } else if(scenario=="FFF_sn"){
    dat <- sim_data_FFF_sn(n_pop,n_surv)
  } else if(scenario=="FFF_pe"){
    dat <- sim_data_FFF_pe(n_pop,n_surv)
  } 
  
  
  CI <- c(mean(dat$y2) + qt(0.025,length(dat$y2)-1)*sd(dat$y2)/sqrt(length(dat$y2)),mean(dat$y2) + qt(0.975,length(dat$y2)-1)*sd(dat$y2)/sqrt(length(dat$y2)))
  Cover_zero <- ifelse(all(CI>mean(dat$y2_pop))|all(CI<mean(dat$y2_pop)),0,1)
  out <- c(mean(dat$y2_pop), mean(dat$y2), Cover_zero)
  return(out)
}

#####################################################
sim_BART <- function(n_pop, n_surv, Npost, scenario, FILE){    
  if(scenario=="TTF_norm"){
    dat <- sim_data_TTF_norm(n_pop,n_surv)
  } else if(scenario=="TFF_norm"){
    dat <- sim_data_TFF_norm(n_pop,n_surv)
  } else if(scenario=="FFF_sn"){
    dat <- sim_data_FFF_sn(n_pop,n_surv)
  } else if(scenario=="FFF_sn_death"){
    dat <- sim_data_FFF_sn_death(n_pop,n_surv)
  }
  
  attach(dat)
  quiet(bart_machine_1 <- gbart(x.train=survsamp_T1, y.train=y1, sparse=TRUE))
  quiet(y_hat_T1_bart <- predict(bart_machine_1, X))
  
  BART <- numeric(Npost)
  for(j in 1:(Npost/10)){
    quiet(bart_machine_2 <- gbart(x.train=survsamp_T2, y.train=y2, ndpost=10, sparse=TRUE))
    for(i in 1:10){
      y_T1_bart <- y_hat_T1_bart[i,] + rnorm(n=length(y_hat_T1_bart[i,]), mean=0, sd=bart_machine_1$sigma[1])
      X_T2 <- data.frame(cbind(X, y_T1_bart)) 
      quiet(pred_pop_2 <- predict(bart_machine_2, X_T2)[i,])
      
      BART[(j-1)*10+i] <- mean(pred_pop_2)
    }
    rm(bart_machine_2)
  }
  
  Cover_zero_BART <- ifelse(all(quantile(BART, c(0.025,0.975))>0)|all(quantile(BART, c(0.025,0.975))<0),0,1)
  
  out <- c(mean(y2_pop),mean(y2),mean(BART),Cover_zero_BART)
  
  write(out, file=FILE, ncol=4, append = T)
  detach(dat)
}

#####################################################
sim_BART_pe <- function(n_pop, n_surv, Npost, pe, FILE){    
  dat <- sim_data_FFF_pe(n_pop,n_surv)
  
  attach(dat)
  quiet(bart_machine_1 <- gbart(x.train=survsamp_T1, y.train=y1, sparse=TRUE))
  quiet(y_hat_T1_bart <- predict(bart_machine_1, X))
  
  BART <- numeric(Npost)
  BART_pe <- numeric(Npost)
  for(j in 1:(Npost/10)){
    quiet(bart_machine_2 <- gbart(x.train=survsamp_T2, y.train=y2, ndpost=10, sparse=TRUE))
    for(i in 1:10){
      y_T1_bart <- y_hat_T1_bart[i,] + rnorm(n=length(y_hat_T1_bart[i,]), mean=0, sd=bart_machine_1$sigma[1])
      X_T2 <- data.frame(cbind(X, y_T1_bart)) 
      quiet(pred_pop_2 <- predict(bart_machine_2, X_T2)[i,])
      
      BART[(j-1)*10+i] <- mean(pred_pop_2)
      BART_pe[(j-1)*10+i] <- mean(pred_pop_2 - sqrt(runif(length(pred_pop_2)))*pe) 
    }
    rm(bart_machine_2)
  }
  
  Cover_zero_BART <- ifelse(all(quantile(BART, c(0.025,0.975))>0)|all(quantile(BART, c(0.025,0.975))<0),0,1)
  Cover_zero_BART_pe <- ifelse(all(quantile(BART_pe, c(0.025,0.975))>0)|all(quantile(BART_pe, c(0.025,0.975))<0),0,1)
  
  out <- c(mean(y2_pop),mean(y2),mean(BART),Cover_zero_BART,mean(BART_pe),Cover_zero_BART_pe)
  write(out, file=FILE, ncol=6, append = T)
  detach(dat)
}
