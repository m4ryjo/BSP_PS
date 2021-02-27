quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
}

seq_predictions_mnar_pe <- function(i, k, fit_Y1, fit_Y2, fit_Y3, fit_Y4, fit_drop2, fit_drop3, fit_drop4){
  quiet(yhat1 <- predict(fit_Y1, Pop_preddata)[i,])
  y1 = yhat1 + rnorm(length(yhat1), mean=0, sd=fit_Y1$sigma.mean)
  
  quiet(pred_drop_2 <- predict(fit_drop2,cbind(y1,Pop_preddata))$prob.test[i,])
  p2 <- rbinom(length(pred_drop_2),1,pred_drop_2)
  
  quiet(pred_pop_2 <- predict(fit_Y2, cbind(y1[which(id1%in%id2)], Pop_preddata_T2))[i,])
  yhat2 <- pred_pop_2 + p2[which(id1%in%id2)]*k*sqrt(runif(n=length(pred_pop_2)))*Pop_ume$Drop_ef[which(id1%in%id2)]# - rnorm(length(pe_t2), pe_t2, sd=1)
  y2 = yhat2  + rnorm(length(yhat2), mean=0, sd=fit_Y2$sigma.mean)
  
  quiet(pred_drop_3 <- predict(fit_drop3,cbind(y2,y1[which(id1%in%id2)], Pop_preddata_T2))$prob.test[i,])
  p3 <- rbinom(length(pred_drop_3),1,pred_drop_3)
  
  quiet(pred_pop_3 <- predict(fit_Y3, cbind(y2[which(id2%in%id3)], y1[which(id1%in%id3)], Pop_preddata_T3))[i,])
  yhat3 <- pred_pop_3 + p3[which(id2%in%id3)]*k*sqrt(runif(n=length(pred_pop_3)))*Pop_ume$Drop_ef[which(id1%in%id3)]# - rnorm(length(pe_t3), pe_t3, sd=1)
  y3 = yhat3  + rnorm(length(yhat3), mean=0, sd=fit_Y3$sigma.mean)
  
  quiet(pred_drop_4 <- predict(fit_drop4,cbind(y3, y2[which(id2%in%id3)], y1[which(id1%in%id3)], Pop_preddata_T3))$prob.test[i,])
  p4 <- rbinom(length(pred_drop_4),1,pred_drop_4)
  
  quiet(pred_pop_4 <- predict(fit_Y4, cbind(y3[which(id3%in%id4)], y2[which(id2%in%id4)], y1[which(id1%in%id4)], Pop_preddata_T4))[i,])
  yhat4 <- pred_pop_4 + p4[which(id3%in%id4)]*k*sqrt(runif(n=length(pred_pop_4)))*Pop_ume$Drop_ef[which(id1%in%id4)]
  
  pred_pop <- tapply(c(yhat1,
                       yhat2 - k*sqrt(runif(n=length(pe_t2)))*pe_t2,
                       yhat3 - k*sqrt(runif(n=length(pe_t3)))*pe_t3,
                       yhat4 - k*sqrt(runif(n=length(pe_t4)))*pe_t4),
                     c(Pop_preddata$age_T1,
                       Pop_preddata_T2$age_T1+5,
                       Pop_preddata_T3$age_T1+10,
                       Pop_preddata_T4$age_T1+15), mean)
  
  return(pred_pop)
}

pop_pred_mnar_pe <- function(Ndpost, k){
  Pop_predictions <- matrix(NA, ncol=13, nrow=Ndpost)
  quiet(fit_Y1 <- gbart(x.train=Betula_T1[,-1], y.train=Betula_T1$EMS_5_comp_90, rm.const=FALSE, ndpost=Ndpost, nskip=500, keepevery=1, sparse=TRUE))
  quiet(fit_Y2 <- gbart(x.train=Betula_T2[,-1], y.train=Betula_T2$EMS_5_comp_95, rm.const=FALSE, ndpost=Ndpost, nskip=500, keepevery=1, sparse=TRUE))
  quiet(fit_Y3 <- gbart(x.train=Betula_T3[,-1], y.train=Betula_T3$EMS_5_comp_00, rm.const=FALSE, ndpost=Ndpost, nskip=500, keepevery=1, sparse=TRUE))
  quiet(fit_Y4 <- gbart(x.train=Betula_T4[,-1], y.train=Betula_T4$EMS_5_comp_05, rm.const=FALSE, ndpost=Ndpost, nskip=500, keepevery=1, sparse=TRUE))
  
  quiet(fit_drop2 <- pbart(x.train=subset(BetulaT2,select = names(BetulaT1)), y.train=BetulaT2$dropT2, rm.const=FALSE, ndpost=Ndpost, nskip=500, keepevery=1, sparse=TRUE))
  quiet(fit_drop3 <- pbart(x.train=subset(BetulaT3,select = names(BetulaT2[,-c(28)])), y.train=BetulaT3$dropT3, rm.const=FALSE, ndpost=Ndpost, nskip=500, keepevery=1, sparse=TRUE))
  quiet(fit_drop4 <- pbart(x.train=subset(BetulaT4,select = names(BetulaT3[,-c(38)])), y.train=BetulaT4$dropT4, rm.const=FALSE, ndpost=Ndpost, nskip=500, keepevery=1, sparse=TRUE))
  for(i in 1:Ndpost) {
    Pop_predictions[i,] <- seq_predictions_mnar_pe(i, k, fit_Y1, fit_Y2, fit_Y3, fit_Y4, fit_drop2, fit_drop3, fit_drop4)
  }
  return(Pop_predictions)
}
