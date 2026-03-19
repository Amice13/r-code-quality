################################################################
################ Out-of-sample 3PRFm forecasts #################
################################################################


modified.threepass <- 
  function(erp,
           predictor,
           start,
           end,
           freq,
           train,
           h=1,
           k,
           period=1,
           CT=FALSE){
  
    data <- 
      merge(erp,predictor)
  
    data <- 
      window(data,
             index(data$erp)[(start-(train*freq)):end])
  
    ### Initialize vectors ------
    
    slopevector_lhs <- 
      paste("slope",1:k,sep="")
    slopevector_rhs <- 
      paste("NULL",sep="")
    eq_slopevector <- 
      paste(paste(
        slopevector_lhs, slopevector_rhs, sep="<-"), collapse=";")
    eval(parse(text=eq_slopevector))
    
    slopevector.wls_lhs <- 
      paste("slope.wls",1:k,sep="")
    slopevector.wls_rhs <- 
      paste("NULL",sep="")
    eq_slopevector.wls <- 
      paste(paste(
        slopevector.wls_lhs, slopevector.wls_rhs, sep="<-"), collapse=";")
    eval(parse(text=eq_slopevector.wls))
    
    historical.mean <- NULL ; actual.erp <- NULL
    predicted <- NULL ; predicted.wls <- NULL
    coefficient <- NULL ; coefficient.wls <- NULL
    
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
    
    ### Main loop for OOS forecasts -------
    
    for (m in (train*freq+period):(n-h+1)){
      
      #### WLS weights -------------
      
      rv.pred <- rv[2:(m-h)]
      
      rv.s <- rv.short[2:(m-h)]
      
      rv.l <- rv.long[2:(m-h)]
      
      rv.reg <- lm(rv.pred ~ rv.s+rv.l)
      
      rv.exante <-
        sqrt(rv.reg$fitted.values)
      
      sigma.weights <- 1 / rv.exante
    
      
      ### Univariate regressions -----
      
      slopelist <- list() ; slopelist.wls <- list()

      premium.longhorizon <- NULL
      
      for (t in 1:(m-h)){ 
        
        premium.longhorizon[t] <- 
          sum(data$erp[t:(t+(h-1))])/h
        
      }
      
      premium <- 
        premium.longhorizon[2:(m-h)]
      
      signal_lhs <- 
        paste("signal",1:k,sep = "")
      signal_rhs <- 
        paste("data[1:(m-h-1),",2:(k+1),"]",
              sep = "")
      eq_signal <- 
        paste(paste(
          signal_lhs, signal_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_signal))

      reg_lhs <- 
        paste("reg",1:k,sep = "")
      reg_rhs <- 
        paste("lm(premium ~ signal",1:k,")",
              sep = "")
      eq_reg <- 
        paste(paste(
          reg_lhs, reg_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_reg))
      
      reg.wls_lhs <- 
        paste("reg.wls",1:k,sep = "")
      reg.wls_rhs <- 
        paste("lm(premium ~ signal",1:k,",weights = sigma.weights^2)",
              sep = "")
      eq_reg.wls <- 
        paste(paste(
          reg.wls_lhs, reg.wls_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_reg.wls))
      
      #### Univariate forecasts --------
      xpred_lhs <- 
        paste("x_pred",1:k,sep = "")
      xpred_rhs <- 
        paste("data.frame(signal",1:k,"<-data[1:(m-1),",2:(k+1),"])",
              sep = "")
      eq_xpred <- 
        paste(paste(
          xpred_lhs,xpred_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_xpred))
      
      prediction_lhs <- 
        paste("prediction",1:k,sep = "")
      prediction_rhs <- 
        paste("scale(predict(reg",1:k,",x_pred",1:k,"))",
              sep = "")
      eq_prediction <- 
        paste(paste(
          prediction_lhs,prediction_rhs, sep = "<-"), collapse=";")
      eval(parse(text = eq_prediction))

      prediction.wls_lhs <- 
        paste("prediction.wls",1:k,sep = "")
      prediction.wls_rhs <- 
        paste("scale(predict(reg.wls",1:k,",x_pred",1:k,"))",
              sep = "")
      eq_prediction.wls <- 
        paste(paste(
          prediction.wls_lhs,prediction.wls_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_prediction.wls))
      
      forecast_lhs <- 
        paste("forecast",1:k,sep = "")
      forecast_rhs <- 
        paste("prediction",1:k,"[1:(m-h-1)]",
              sep = "")
      eq_forecast <- 
        paste(paste(
          forecast_lhs,forecast_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_forecast))
      
      forecast.wls_lhs <- 
        paste("forecast.wls",1:k,sep = "")
      forecast.wls_rhs <- 
        paste("prediction.wls",1:k,"[1:(m-h-1)]",
              sep = "")
      eq_forecast.wls <- 
        paste(paste(
          forecast.wls_lhs,forecast.wls_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_forecast.wls))
      
      #### Regress forecasts on returns ------      
      secondreg_lhs <- 
        paste("secondreg",1:k,sep = "")
      secondreg_rhs <- 
        paste("lm(forecast",1:k," ~ premium)",
              sep = "")
      eq_secondreg <- 
        paste(paste(
          secondreg_lhs,secondreg_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_secondreg))
      
      secondreg.wls_lhs <- 
        paste("secondreg.wls",1:k,sep = "")
      secondreg.wls_rhs <- 
        paste("lm(forecast.wls",1:k," ~ premium,weights = sigma.weights^2)",sep = "")
      eq_secondreg.wls <- 
        paste(paste(
          secondreg.wls_lhs,secondreg.wls_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_secondreg.wls))
      
      #### Slopes retained -------
      beta_lhs <- 
        paste("beta",1:k,sep = "")
      beta_rhs <- 
        paste("coef(secondreg",1:k,")[2]",
              sep = "")
      eq_beta <- 
        paste(paste(
          beta_lhs, beta_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_beta))
      
      beta.wls_lhs <- 
        paste("beta.wls",1:k,sep = "")
      beta.wls_rhs <- 
        paste("coef(secondreg.wls",1:k,")[2]",sep = "")
      eq_beta.wls <- 
        paste(paste(beta.wls_lhs, beta.wls_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_beta.wls))
      
      coef.wls_lhs <- 
        paste("slope.wls",1:k,sep = "")
      coef.wls_rhs <- 
        paste("rbind(slope.wls",1:k,
              ",zoo(beta.wls",1:k,",index(data$erp)[m-1]))",sep = "")
      eq_coef.wls <- 
        paste(paste(
          coef.wls_lhs, coef.wls_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_coef.wls))
      
      coef_lhs <- 
        paste("slope",1:k,sep = "")
      coef_rhs <- 
        paste("rbind(slope",1:k,
              ",zoo(beta",1:k,",index(data$erp)[m-1]))",sep = "")
      eq_coef <- 
        paste(paste(
          coef_lhs, coef_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_coef))
      
      coeflist_lhs <- 
        paste("slopelist[[",1:k,"]]",sep = "")
      coeflist_rhs <- 
        paste("slope",1:k,sep = "")
      eq_coeflist <- 
        paste(paste(
          coeflist_lhs, coeflist_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_coeflist))
      
      coeflist.wls_lhs <- 
        paste("slopelist.wls[[",1:k,"]]",sep = "")
      coeflist.wls_rhs <- 
        paste("slope.wls",1:k,sep = "")
      eq_coeflist.wls <- 
        paste(paste(
          coeflist.wls_lhs, coeflist.wls_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_coeflist.wls))
      
      allslopes <- 
        do.call(merge,slopelist)
      allslopes.wls <- 
        do.call(merge,slopelist.wls)

      dat <- 
        merge(data,allslopes,all=T)
      dat.wls <- 
        merge(data,allslopes.wls,all=T)
      
      signals <- dat[,2:(k+1)]
      
      slopes <- 
        dat[,(k+2):(ncol(dat))]
      slopes.wls <- 
        dat.wls[,(k+2):(ncol(dat))]
      
      #### Fitted values -------
      fittedlist <- list()
      fittedlist.wls <- list()
      
      fitted_lhs <- 
        paste("fittedlist[[",1:k,"]]",sep = "")
      fitted_rhs <- 
        paste("prediction",1:k,sep = "")
      eq_fitted <- 
        paste(paste(
          fitted_lhs, fitted_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_fitted))
      
      fitted.wls_lhs <- 
        paste("fittedlist.wls[[",1:k,"]]",sep = "")
      fitted.wls_rhs <-
        paste("prediction.wls",1:k,
              sep = "")
      eq_fitted.wls <- 
        paste(paste(
          fitted.wls_lhs, fitted.wls_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_fitted.wls))
      
      l <- length(fittedlist[[1]])
      
      prediction <- 
        data.frame(matrix(
          unlist(fittedlist),
          nrow=l,
          byrow=F))
      prediction.wls <- 
        data.frame(matrix(
          unlist(fittedlist.wls),
          nrow=l,
          byrow=F))
      
      ### Cross-sectional regression ------
      
      q <- nrow(prediction)
      
      factor <- NULL ; factor.wls<-NULL
      
      for (t in 1:q){
        
        forecast <- 
          as.numeric(prediction[t,])
        forecast.wls <- 
          as.numeric(prediction.wls[t,])
        
        estslope <- 
          as.numeric(slopes[(m-1),])
        estslope.wls <- 
          as.numeric(slopes.wls[(m-1),])
        
        cross.sect.reg <- lm(forecast ~ estslope)
        cross.sect.reg.wls <- lm(forecast.wls ~ estslope.wls)
        
        beta <- coef(cross.sect.reg)[2]
        beta.wls <- coef(cross.sect.reg)[2]
        
        colnames(signals)[1] <- "firstcol"

        factor <- 
          rbind(factor,
                zoo(beta,index(signals$firstcol)[t]))
        factor.wls <- 
          rbind(factor.wls,
                zoo(beta.wls,index(signals$firstcol)[t]))
        
      }
      
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
      
      premium <- premium.longhorizon[2:(m-h)]

      pred <- factor[1:(m-h-1)]
      pred.wls <- factor.wls[1:(m-h-1)]
      
      ### Return predictive regression -----
      
      reg <- lm(premium ~ pred)
      reg.wls <- 
        lm(premium ~ pred.wls,
           weights = sigma.weights^2)
      
      beta <- coef(reg)[2]
      beta.wls <- coef(reg.wls)[2]
      
      coefficient <- 
        rbind(coefficient,
              zoo(beta,index(data$erp)[m]))
      coefficient.wls <- 
        rbind(coefficient.wls,
              zoo(beta.wls,index(data$erp)[m]))
      
      #### Print time period ------
      l <- length(coefficient)
      getindex <- as.yearmon(index(coefficient[l]))
      print(getindex)
      
      #### Predict returns ------
      x.pred <- 
        data.frame(pred <- factor[m-1])
      x.pred.wls <- 
        data.frame(pred.wls <- factor.wls[m-1])
      
      prediction <- predict(reg,x.pred)
      prediction.wls <- predict(reg.wls,x.pred.wls)

      if(CT == T){
      #### Non-negativity ------
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
      
      #### Squared prediction error ------
      spe.pred <- (actual.erp-predicted)^2
      spe.pred.wls <- (actual.erp-predicted.wls)^2
      
    }
    
    ## CSPE difference ------
    
    cspe.diff <- cumsum(spe.mean)-cumsum(spe.pred)
    cspe.diff.wls <- cumsum(spe.mean)-cumsum(spe.pred.wls)
    
    ## Output ------
    return(list(historical.mean = historical.mean,
                actual.erp = actual.erp,
                coefficient = coefficient,coefficient.wls = coefficient.wls,
                spe.mean = spe.mean,
                spe.pred = spe.pred,spe.pred.wls = spe.pred.wls,
                predicted = predicted,predicted.wls = predicted.wls,
                cspe.diff = cspe.diff,cspe.diff.wls = cspe.diff.wls))
    
}