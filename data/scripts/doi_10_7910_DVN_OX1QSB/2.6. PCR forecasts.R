##############################################################
################# Out-of-sample PCR forecasts ################
##############################################################

## Function for forecast calculation ------

pcr.forecast <- 
  function(erp,
           predictor,
           start,
           end,
           freq,
           train,
           period=1,
           h=1,
           CT = FALSE){
  
    dat <- merge(erp,predictor)
  
    predictor.wholesubset <- predictor
    
    colnames(predictor.wholesubset)[1] <- 
      "firstcol"

    data <- 
      window(dat,
             index(dat$erp)[(start-(train*freq)):end])
  
    predictor.subset <- 
      window(predictor.wholesubset,
             index(predictor.wholesubset$firstcol)[(start-(train*freq)):end])
    
    ### Initialize vectors ------
    
    historical.mean <- NULL; actual.erp <- NULL
    predicted <- NULL; predicted.wls <- NULL
    predicted.rsquare <- NULL; predicted.rsquare.wls <- NULL
    components.number <- NULL; components.number.wls <- NULL

    n <- length(data$erp)
    
    ### WLS preliminaries --------
    
    rv.longhorizon <- NULL
    l <- length(forecast.data$svar)
    svar <- forecast.data$svar 
    
      for (t in 1:(l-h)){ 
        
        rv.longhorizon[t] <- 
          sum(svar[t:(t+(h-1))])/h
        
      }
      
      rv <- 
        rv.longhorizon[(start-(train*freq)):end]
      rv.short <- 
        stats::lag(svar,-1)
      rv.short <- 
        rv.short[(start-(train*freq)-1):(end-1)]
      rv.long <- 
        rollmean(svar,
                 freq,
                 align = "right")
      rv.long <- 
        rv.long[(start-(train*freq)-freq):(end-freq)] 
      rv.long <- 
        stats::lag(rv.long,-1)
    
      ### Main loop for OOS forecasts -----
      
  for (m in (train*freq+period):(n-h+1)){
    
    #### Regression weights -----
    rv.pred <- rv[2:(m-h)] 
    rv.s <- rv.short[2:(m-h)] 
    rv.l <- rv.long[2:(m-h)]
    
    rv.reg <- 
      lm(rv.pred ~ rv.s+rv.l)
    
	rv.exante <-
        sqrt(rv.reg$fitted.values)
      
      sigma.weights <- 1 / rv.exante
    
    
  ### PCA -------
    
    predictor.pca <- 
      window(predictor.subset,
             index(predictor.subset$firstcol))[1:(m-1)]
   
    factors <- 
      princomp(predictor.pca,
               cor = TRUE,
               scores = TRUE)

    pc1 <- factors$scores[,1]
    pc2 <- factors$scores[,2]
    pc3 <- factors$scores[,3]

    ### Premium calculations ------
    historical.avg <- 
      mean(data$erp[2:(m-1)])
    historical.mean <- 
      rbind(historical.mean,
            zoo(historical.avg,index(data$erp)[m]))
    
    actual.erp.t <- 
      sum(data$erp[m:(m+h-1)])*(1/h)
    actual.erp <- 
      rbind(actual.erp,
            zoo(actual.erp.t,index(data$erp)[m]))
    
    spe.mean <- (historical.mean-actual.erp)^2
    
    premium.longhorizon <- NULL
      
      for (t in 1:(m-h)){ 
        
        premium.longhorizon[t] <- 
          sum(data$erp[t:(t+(h-1))])/h
        
      }
      
      premium <- 
        premium.longhorizon[2:(m-h)]
      
      factor1 <- pc1[1:(m-h-1)]
      factor2 <- pc2[1:(m-h-1)]
      factor3 <- pc3[1:(m-h-1)]
    
      ### PCR regressions -----      
      reg1 <- lm(premium ~ factor1)
      reg1.wls <- lm(premium ~ factor1,
                     weights = sigma.weights^2)
      reg2 <- lm(premium ~ factor1+factor2) 
      reg2.wls <- lm(premium ~ factor1+factor2,
                     weights = sigma.weights^2)
      reg3 <- lm(premium ~ factor1+factor2+factor3)
      reg3.wls <- lm(premium ~ factor1+factor2+factor3,
                     weights = sigma.weights^2)
    
      k1 <- summary(reg1)$adj.r.squared 
      k1.wls <- summary(reg1.wls)$adj.r.squared
      k2 <- summary(reg2)$adj.r.squared 
      k2.wls <- summary(reg2.wls)$adj.r.squared
      k3 <- summary(reg3)$adj.r.squared 
      k3.wls <- summary(reg3.wls)$adj.r.squared

      maximum <- max(k1,k2,k3)
      maximum.wls <- max(k1.wls,k2.wls,k3.wls)
    
      ### One component -----
      #### Prediction ------
      x.pred <- 
        data.frame(factor1 <- pc1[m-1])
      
      prediction <- 
        predict.lm(reg1,x.pred) 
      prediction.wls <- 
        predict.lm(reg1.wls,x.pred)
     
      if(CT == T){
      #### Non-Negativity ------
      prediction <- 
        ifelse(prediction > 0, 
               prediction, 
               0)
      prediction.wls <- 
        ifelse(prediction.wls > 0, 
               prediction.wls, 
               0)
      }
      predicted <- 
        rbind(predicted,
              zoo(prediction,index(data$erp)[m]))
      predicted.wls <- 
        rbind(predicted.wls,
              zoo(prediction.wls,index(data$erp)[m]))
      
      ### R2 selection -----
      
      if(k1 == maximum){
        
        prediction.rsquare <- prediction
        nr <- 1
        
      }
      
      if(k2 == maximum){
        
        x.pred <- 
          data.frame(factor1 <- pc1[m-1],factor2 <- pc2[m-1])
        
        prediction.rsquare <- 
          predict.lm(reg2,x.pred)
        nr <- 2
        
      }
      
      if(k3 == maximum){
        
        x.pred <- 
          data.frame(factor1 <- pc1[m-1],factor2 <- pc2[m-1],
                     factor3 <- pc3[m-1])
        
        prediction.rsquare <- 
          predict.lm(reg3,x.pred)
        nr<-3
        
        }
    
      if(k1.wls == maximum.wls){
        
        prediction.rsquare.wls <- 
          prediction.wls
        nr.wls<-1
        
      }
      
      if(k2.wls == maximum.wls){
        
        x.pred <- 
          data.frame(factor1 <- pc1[m-1],factor2 <- pc2[m-1])
        
        prediction.rsquare.wls <- 
          predict.lm(reg2.wls,x.pred)
        nr.wls<-2
        
      }
      
      if(k3.wls == maximum.wls){
        
        x.pred <- data.frame(factor1 <- pc1[m-1],factor2 <- pc2[m-1],
                             factor3 <- pc3[m-1])
        
        prediction.rsquare.wls <- 
          predict.lm(reg3.wls,x.pred)
        nr.wls<-3
        
      }
      
      if(CT==T){
      #### Non-negativity ----
      prediction.rsquare <- 
        ifelse(prediction.rsquare > 0, 
               prediction.rsquare, 
               0)
      prediction.rsquare.wls <- 
        ifelse(prediction.wls > 0, 
               prediction.rsquare.wls, 
               0)
      }
      predicted.rsquare <- 
        rbind(predicted.rsquare,
              zoo(prediction.rsquare,index(data$erp)[m]))
      predicted.rsquare.wls <- 
        rbind(predicted.rsquare.wls,
              zoo(prediction.rsquare.wls,index(data$erp)[m]))
      
      #### Components number -----
      components.number <- 
        rbind(components.number,
              zoo(nr,index(data$erp)[m]))
      components.number.wls <- 
        rbind(components.number.wls,
              zoo(nr.wls,index(data$erp)[m]))
      
      ### Squared prediction error----- 
      spe.pred <- (actual.erp-predicted)^2
      spe.pred.wls <- (actual.erp-predicted.wls)^2
      spe.pred.rsquare <- (actual.erp-predicted.rsquare)^2
      spe.pred.rsquare.wls <- (actual.erp-predicted.rsquare.wls)^2
      
  }
  
    ## CSPE differences -----
    cspe.diff <- cumsum(spe.mean)-cumsum(spe.pred)
    cspe.diff.wls <- cumsum(spe.mean)-cumsum(spe.pred.wls)
    cspe.diff.rsquare <- cumsum(spe.mean)-cumsum(spe.pred.rsquare)
    cspe.diff.rsquare.wls <- cumsum(spe.mean)-cumsum(spe.pred.rsquare.wls)
    
    ## Output preparation -----
    firstcomp <- 
      list(predicted = predicted,predicted.wls = predicted.wls,
           spe.pred = spe.pred,spe.pred.wls = spe.pred.wls,
           cspe.diff = cspe.diff,cspe.diff.wls = cspe.diff.wls)
    
    rsquare <- 
      list(predicted = predicted.rsquare,predicted.wls = predicted.rsquare.wls,
           spe.pred = spe.pred.rsquare,spe.pred.wls = spe.pred.rsquare.wls,
           cspe.diff = cspe.diff.rsquare,cspe.diff.wls = cspe.diff.rsquare.wls,
           components.number = components.number,
           components.number.wls = components.number.wls)
    
    ## Output ----
  
  return(list(
    firstcomp = firstcomp,rsquare = rsquare,
    spe.mean = spe.mean,
    historical.mean = historical.mean,
    actual.erp = actual.erp))
}
