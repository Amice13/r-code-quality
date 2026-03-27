#################################################################
################ Out-of-sample C-ENet forecasts #################
#################################################################

## Function for C-ENet forecast calculation ------

cenet.forecast <- 
  function(erp, # unless noted, see 2.1. for definitions
           predictor,
           start,
           end,
           freq,
           train,
           holdout=10,
           k,
           period=1,
           h=1,
           alpha=0.5,
           CT = FALSE){
  
      data <- 
        merge(erp,predictor,all=FALSE)
  
      data <- 
        window(data,index(data$erp)[(start-(train*freq)):end])
  
    ## Univariate regressions -----

      forecastvector_lhs <- 
        paste("predicted",1:k,sep = "")
      forecastvector_rhs <- 
        paste("NULL",sep = "")
      eq_forecastvector <- 
        paste(paste(
          forecastvector_lhs, forecastvector_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_forecastvector))
      
      forecastvector.wls_lhs <- 
        paste("predicted.wls",1:k,sep = "")
      forecastvector.wls_rhs <- 
        paste("NULL",sep = "")
      eq_forecastvector.wls <- 
        paste(paste(
          forecastvector.wls_lhs,forecastvector.wls_rhs, sep = "<-"), 
          collapse = ";")
      eval(parse(text = eq_forecastvector.wls))
      
      n <- length(data$erp)
  
      m1 <- freq*(train-holdout)+period
  
      forecasts <- list(); forecasts.wls <- list()
      
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
      
        ### Main loop ----
  for (m in m1:(n-h+1)){
    
    #### WLS weights -------------
    
    rv.current <- rv[2:(m-h)]
    
    rv.s <- rv.short[2:(m-h)]
    
    rv.l <- rv.long[2:(m-h)]
    
    rv.reg <- lm(rv.current ~ rv.s+rv.l)
    rv.exante <-
        sqrt(rv.reg$fitted.values)
      
      sigma.weights <- 1 / rv.exante
      
    premium.longhorizon <- NULL
    
    for (t in 1:(m-h)){ 
      
      premium.longhorizon[t] <- 
        sum(data$erp[t:(t+(h-1))])/h
      
    }
    
      premium <- 
        premium.longhorizon[2:(m-h)]
      
      historical.avg <- 
        mean(data$erp[2:(m-1)])
      
      predictordata_lhs <- 
        paste("pred",1:k,sep = "")
      predictordata_rhs <- 
        paste("data[1:(m-h-1),",2:(k+1),"]",sep = "")
      eq_predictordata <- 
        paste(paste(
          predictordata_lhs, predictordata_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_predictordata))
        
      ### Regressions ----
      
      reg_lhs <- 
        paste("reg",1:k,sep = "")
      reg_rhs <- 
        paste("lm(premium ~ pred",1:k,")",sep = "")
      eq_reg <- 
        paste(paste(
          reg_lhs, reg_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_reg))
      
      reg.wls_lhs <- 
        paste("reg.wls",1:k,sep = "")
      reg.wls_rhs <- 
        paste("lm(premium ~ pred",1:k,",weights = sigma.weights^2)",sep = "")
      eq_reg.wls <- 
        paste(paste(
          reg.wls_lhs, reg.wls_rhs, sep = "<-"), collapse =";")
      eval(parse(text = eq_reg.wls))
    
      xpred_lhs <- 
        paste("x.pred",1:k,sep = "")
      xpred_rhs <- 
        paste("data.frame(pred",1:k,"<-data[(m-1),",2:(k+1),"])",sep = "")
      eq_xpred <- 
        paste(paste(
          xpred_lhs,xpred_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_xpred))
    
      prediction_lhs <- 
        paste("prediction",1:k,sep = "")
      prediction_rhs <- 
        paste("(predict(reg",1:k,",x.pred",1:k,"))",sep = "")
      eq_prediction <- 
        paste(paste(
          prediction_lhs,prediction_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_prediction))
    
      prediction.wls_lhs <- 
        paste("prediction.wls",1:k,sep = "")
      prediction.wls_rhs <- 
        paste("(predict(reg.wls",1:k,",x.pred",1:k,"))",sep = "")
      eq_prediction.wls <- 
        paste(paste(
          prediction.wls_lhs,prediction.wls_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_prediction.wls))
      
        
      ### CT restricions ----        
      predictionlist <- 
        vector(mode = "list",length = k)
      predictionlist.wls <- 
        vector(mode = "list",length = k)
      
      if(CT == FALSE){
      coefsign_lhs <- 
        paste("predictionlist[[",1:k,"]]",sep = "")
      coefsign_rhs <- 
        paste("prediction",1:k,
              sep = "")
      eq_coefsign <- 
        paste(paste(
          coefsign_lhs, coefsign_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_coefsign))
      
      coefsign.wls_lhs <- 
        paste("predictionlist.wls[[",1:k,"]]",sep = "")
      coefsign.wls_rhs <- 
        paste("prediction.wls",1:k,sep="")
      eq_coefsign.wls <- 
        paste(paste(
          coefsign.wls_lhs, coefsign.wls_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_coefsign.wls))
      }
      
      if(CT == T){
        
      #### Positive coefficients ----
      for (s in pos){
        
        coefsign_lhs <- 
          paste("predictionlist[[",s,"]]",sep = "")
        coefsign_rhs <- 
          paste("ifelse(coef(reg",s,")[2]>0,prediction",s,",historical.avg)",
                sep = "")
        eq_coefsign <- 
          paste(paste(
            coefsign_lhs, coefsign_rhs, sep = "<-"), collapse = ";")
        eval(parse(text = eq_coefsign))
        
        coefsign.wls_lhs <- 
          paste("predictionlist.wls[[",s,"]]",sep = "")
        coefsign.wls_rhs <- 
          paste("ifelse(coef(reg.wls",s,")[2]>0,
                prediction.wls",s,",historical.avg)",sep="")
        eq_coefsign.wls <- 
          paste(paste(
            coefsign.wls_lhs, coefsign.wls_rhs, sep = "<-"), collapse = ";")
        eval(parse(text = eq_coefsign.wls))
        
      } 
      
      #### Negative coefficients ----
      for (s in neg){
        
        coefsign_lhs <- 
          paste("predictionlist[[",s,"]]",sep = "")
        coefsign_rhs <- 
          paste("ifelse(coef(reg",s,")[2]<0,
                prediction",s,",historical.avg)",sep = "")
        eq_coefsign <- 
          paste(paste(
            coefsign_lhs, coefsign_rhs, sep = "<-"), collapse = ";")
        eval(parse(text = eq_coefsign))
        
        coefsign.wls_lhs <- 
          paste("predictionlist.wls[[",s,"]]",sep = "")
        coefsign.wls_rhs <- 
          paste("ifelse(coef(reg.wls",s,")[2]<0,
                prediction",s,",historical.avg)",sep = "")
        eq_coefsign.wls <- 
          paste(paste(
            coefsign.wls_lhs, coefsign.wls_rhs, sep = "<-"), collapse = ";")
        eval(parse(text = eq_coefsign.wls))
        
      }
        
      #### Non-negativity restrictions ----
      nonneg_lhs <- 
        paste("predictionlist[[",1:k,"]]",sep = "")
      nonneg_rhs <- 
        paste("ifelse(predictionlist[[",1:k,"]]>0,
              predictionlist[[",1:k,"]],0)",sep = "")
      eq_nonneg <- 
        paste(paste(
          nonneg_lhs,nonneg_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_nonneg))
      
      nonneg.wls_lhs <- 
        paste("predictionlist.wls[[",1:k,"]]",sep = "")
      nonneg.wls_rhs <- paste("ifelse(predictionlist.wls[[",1:k,"]]>0,
                              predictionlist.wls[[",1:k,"]],0)",sep = "")
      eq_nonneg.wls <- 
        paste(paste(
          nonneg.wls_lhs,nonneg.wls_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_nonneg.wls))
      }
      ### Forecasts ----
      forecast_lhs <- 
        paste("predicted",1:k,sep = "")
      forecast_rhs <- 
        paste("rbind(predicted",1:k,",zoo(predictionlist[[",1:k,"]],
              index(data$erp)[m]))",sep = "")
      eq_forecast <- 
        paste(paste(
          forecast_lhs, forecast_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_forecast))
    
      forecast.wls_lhs <- 
        paste("predicted.wls",1:k,sep = "")
      forecast.wls_rhs <- 
        paste("rbind(predicted.wls",1:k,",zoo(predictionlist.wls[[",1:k,"]],
              index(data$erp)[m]))",sep = "")
      eq_forecast.wls <- 
        paste(paste(
          forecast.wls_lhs, forecast.wls_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_forecast.wls))
        
  }
        
      list_lhs <- 
        paste("forecasts[[",1:k,"]]",sep = "")
      list_rhs <- 
        paste("predicted",1:k,sep = "")
      eq_list <- 
        paste(paste(
          list_lhs, list_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_list))  
  
      list.wls_lhs <- 
        paste("forecasts.wls[[",1:k,"]]",sep = "")
      list.wls_rhs <- 
        paste("predicted.wls",1:k,sep = "")
      eq_list.wls <- 
        paste(paste(
          list.wls_lhs, list.wls_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_list.wls)) 
      
      for(i in 1:k){
        names(forecasts)[i] <- 
          colnames(data)[i+1]
        names(forecasts.wls)[i] <- 
          colnames(data)[i+1]
      }

      allforecasts <- 
        do.call(merge,forecasts)
      allforecasts.wls <- 
        do.call(merge,forecasts.wls)
      
      ## Granger and Ramanathan regression ----  
      data <- 
        merge(erp,allforecasts,all=TRUE)
      data.wls <- 
        merge(erp,allforecasts.wls,all=TRUE)
      
      data <- 
        window(data,index(data$erp)[(start-(train*freq)):end])
      data.wls <- 
        window(data.wls,index(data.wls$erp)[(start-(train*freq)):end])
      
      ### Initialize vectors ----
      coefvector_lhs <- 
        paste("coefficients",1:k,sep = "")
      coefvector_rhs <- 
        paste("NULL",sep = "")
      eq_coefvector <- 
        paste(paste(
          coefvector_lhs, coefvector_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_coefvector))
      
      coefvector.wls_lhs <- 
        paste("coefficients.wls",1:k,sep = "")
      coefvector.wls_rhs <- 
        paste("NULL",sep = "")
      eq_coefvector.wls <- 
        paste(paste(
          coefvector.wls_lhs, coefvector.wls_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_coefvector.wls))
      
      predicted <- NULL;  predicted.wls <- NULL
      historical.mean <- NULL; actual.erp <- NULL
      cardinality <- NULL; cardinality.wls <- NULL
      lambda <- NULL; lambda.wls <- NULL

      n <- length(data$erp)
      
    for (t in (train*freq+period):(n-h+1)){
    
        historical.avg <- 
          mean(data$erp[2:(t-1)])
        historical.mean <- 
          rbind(historical.mean,
                zoo(historical.avg,index(data$erp)[t]))
    
        actual.erp.t <- 
          sum(data$erp[t:(t+h-1)])*(1/h)
        actual.erp <- 
          rbind(actual.erp,
                zoo(actual.erp.t,index(data$erp)[t]))

        spe.mean <- (historical.mean-actual.erp)^2
        
        premium <- 
          as.matrix(premium.longhorizon[m1:(t-h)])

        pred <- 
          as.matrix(data[m1:(t-h),2:(k+1)])
        pred.wls <- 
          as.matrix(data.wls[m1:(t-h),2:(k+1)])
        
    ### ENet regression ----
    # The value of lambda is selected via the AICC.
    # The coefficients are constrained to be non-negative.
    
        enet <- 
          ic.glmnet(pred,
                    premium,
                    crit = "aicc",
                    alpha = alpha,
                    lower.limits = 0)
        enet.wls <- 
          ic.glmnet(pred.wls,
                    premium,
                    crit = "aicc",
                    alpha = alpha,
                    lower.limits = 0)
        
    # The indicator corresponds to 1 if coefficient is positive.
        
        coefficientlist <- list()
        coefficientlist.wls <- list()
        
        indicatorlist <- list()
        indicatorlist.wls <- list()
        
        for (i in 2:(k+1)){
          
          theta <- coef(enet)[i]
          theta.wls <- coef(enet.wls)[i]
          
          indicator <- ifelse(theta>0,1,0)
          indicator.wls <- ifelse(theta.wls>0,1,0)
          
          coefficientlist[[i-1]]  <- theta 
          coefficientlist.wls[[i-1]] <- theta.wls
          
          indicatorlist[[i-1]] <- indicator
          indicatorlist.wls[[i-1]] <- indicator.wls
          
        } 

    ## Prediction ----        
      cenet.sum <- 0; cenet.sum.wls <- 0
        
      for (i in 1:k){
        
          current <- 
            as.numeric(indicatorlist[[i]])*data[t,i+1]
          current.wls <- 
            as.numeric(indicatorlist.wls[[i]])*data.wls[t,i+1]
          
          cenet.sum <- 
            cenet.sum+current
          cenet.sum.wls <- 
            cenet.sum.wls+current.wls
          
     }
    
        allcoef <- 
          as.matrix(unlist(
            coefficientlist),ncol = k)
        allcoef.wls <- 
          as.matrix(unlist(
            coefficientlist.wls),ncol=k)
        
        card <- sum(allcoef !=0)
        card.wls <- sum(allcoef.wls !=0)
        
        forecast.mean <- (1/card)*cenet.sum
        forecast.mean.wls <- (1/card.wls)*cenet.sum.wls
        
        forecast <- 
          ifelse(card == 0,historical.avg,forecast.mean)
        forecast.wls<-
          ifelse(card.wls == 0,historical.avg,forecast.mean.wls)
        predicted <- 
          rbind(predicted,
                zoo(forecast,index(data$erp)[t]))   
        predicted.wls <- 
          rbind(predicted.wls,
                zoo(forecast.wls,index(data.wls$erp)[t]))
        
        lambda.enet <- enet$lambda
        lambda.enet.wls <- enet.wls$lambda
        
        lambda <- 
          rbind(lambda,
                zoo(lambda.enet,index(data$erp)[t]))
        lambda.wls <- 
          rbind(lambda.wls,
                zoo(lambda.enet.wls,index(data.wls$erp)[t]))

        cardinality <- 
          rbind(cardinality,zoo(card,index(data$erp)[t]))
        cardinality.wls <- 
          rbind(cardinality.wls,zoo(card.wls,index(data.wls$erp)[t]))
        
        enetcoef_lhs <- 
          paste("coefficients",1:k,sep = "")
        enetcoef_rhs <- paste("rbind(coefficients",1:k,
                              ",zoo(coefficientlist[[",1:k,
                              "]],index(data$erp)[t]))",sep = "")
        eq_enetcoef <- 
          paste(paste(
            enetcoef_lhs, enetcoef_rhs, sep = "<-"), collapse = ";")
        eval(parse(text = eq_enetcoef))
        
        enetcoef.wls_lhs <- 
          paste("coefficients.wls",1:k,sep = "")
        enetcoef.wls_rhs <- 
          paste("rbind(coefficients.wls",1:k,
                ",zoo(coefficientlist.wls[[",1:k,"]],index(data$erp)[t]))",
                sep = "")
        eq_enetcoef.wls <- 
          paste(paste(
            enetcoef.wls_lhs, enetcoef.wls_rhs, sep = "<-"), collapse = ";")
        eval(parse(text = eq_enetcoef.wls))
        
        #### SPE ----
        spe.pred <- (actual.erp-predicted)^2
        spe.pred.wls <- (actual.erp-predicted.wls)^2 
        
    }
      
      enetcoeflist <- list()
      enetcoeflist.wls <- list()

      enetlist_lhs <- 
        paste("enetcoeflist[[",1:k,"]]",sep = "")
      enetlist_rhs <- 
        paste("coefficients",1:k,sep = "")
      eq_enetlist <- 
        paste(paste(
          enetlist_lhs, enetlist_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_enetlist))
      
      enetlist.wls_lhs <- 
        paste("enetcoeflist.wls[[",1:k,"]]",sep = "")
      enetlist.wls_rhs <- 
        paste("coefficients.wls",1:k,sep = "")
      eq_enetlist.wls <- 
        paste(paste(
          enetlist.wls_lhs, enetlist.wls_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_enetlist.wls))
      
      cspe.diff <- 
        cumsum(spe.mean)-cumsum(spe.pred)
      cspe.diff.wls <- 
        cumsum(spe.mean)-cumsum(spe.pred.wls)
      
      for(i in 1:k){
        
        names(enetcoeflist)[i] <- 
          colnames(data)[i+1]
        names(enetcoeflist.wls)[i] <- 
          colnames(data.wls)[i+1]
        
      }
      
      ## Output ----
      return(list(
        historical.mean = historical.mean,
        predicted = predicted,predicted.wls = predicted.wls,
        actual.erp = actual.erp,
        spe.mean = spe.mean,
        lambda = lambda,cardinality = cardinality,
        lambda.wls = lambda.wls,cardinality.wls = cardinality.wls,
        enetcoeflist = enetcoeflist,enetcoeflist.wls = enetcoeflist.wls,
        spe.pred = spe.pred,spe.pred.wls = spe.pred.wls,
        cspe.diff = cspe.diff,cspe.diff.wls = cspe.diff.wls))
}