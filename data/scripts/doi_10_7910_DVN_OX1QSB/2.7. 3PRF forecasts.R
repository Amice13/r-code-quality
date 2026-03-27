################################################################
################## Out-of-sample 3PRF forecasts ################
################################################################

## Function for forecast calculation ------

threepass.forecast <- 
  function(erp,
           predictor,
           start,
           end,
           freq,
           train,
           h=1,
           k,
           period=1,
           CT = FALSE){
  
      data <- 
        merge(erp,predictor)
  
      data <- 
        window(data,
               index(data$erp)[(start-(train*freq)):end])
  
      ### Initialize vectors ------
      
      slopevector_lhs <- 
        paste("slope",1:k,sep = "")
      slopevector_rhs <- 
        paste("NULL",sep = "")
      eq_slopevector <- 
        paste(paste(
          slopevector_lhs, slopevector_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_slopevector))
    
      slopevector.wls_lhs <- 
        paste("slope.wls",1:k,sep = "")
      slopevector.wls_rhs <- 
        paste("NULL",sep = "")
      eq_slopevector.wls <- 
        paste(paste(
          slopevector.wls_lhs, slopevector.wls_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_slopevector.wls))
      
      slopelist <- list() ; slopelist.wls <- list()
      
      n<-length(data$erp)
      
      ### WLS preliminaries --------
      
      rv.longhorizon<-NULL

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
      
  for (m in (train*freq+1):(n-h+1)){

    ### Regression weights ----
    rv.pred <- rv[2:(m-h)]
    
    rv.s <- rv.short[2:(m-h)]
    
    rv.l <- rv.long[2:(m-h)]
    
    rv.reg <- lm(rv.pred ~ rv.s+rv.l)
    
    rv.exante <-
        sqrt(rv.reg$fitted.values)
    sigma.weights <- 1 / rv.exante
    
    
    ### Equity premium ------
    premium.longhorizon <- NULL
    
      for (t in 1:(m-h)){ 
      
        premium.longhorizon[t] <- 
          sum(data$erp[t:(t+(h-1))])/h
      
      }
    
      premium <- 
        premium.longhorizon[2:(m-h)]
    
    ### Univariate time series regression -----
    #### Scale predictors ------
      
      unscaled_lhs <- 
        paste("signal_unscaled",1:k,sep = "")
      unscaled_rhs <- 
        paste("data[1:(m-1),",2:(k+1),"]",sep = "")
      eq_unscaled <- 
        paste(paste(
          unscaled_lhs, unscaled_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_unscaled))
    
      scaling_lhs <- 
        paste("signal",1:k,sep = "")
      scaling_rhs <- 
        paste("as.numeric(scale(signal_unscaled",1:k,")[1:(m-h-1)])",sep = "")
      eq_scaling <- 
        paste(paste(
          scaling_lhs, scaling_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_scaling))
    
      #### Univariate regression -----
      reg_lhs <- 
        paste("reg",1:k,sep = "")
      reg_rhs <- 
        paste("lm(signal",1:k,"~premium)",sep = "")
      eq_reg <- 
        paste(paste(
          reg_lhs, reg_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_reg))

      reg.wls_lhs <- 
        paste("reg.wls",1:k,sep = "")
      reg.wls_rhs <- 
        paste("lm(signal",1:k," ~ premium,weights = sigma.weights^2)",
              sep="")
      eq_reg.wls <- 
        paste(paste(
          reg.wls_lhs, reg.wls_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_reg.wls))
      
      coef_lhs <- 
        paste("slope",1:k,sep = "")
      coef_rhs <- 
        paste("rbind(slope",1:k,
              ",zoo(coef(reg",1:k,")[2],index(data$erp)[m-1]))",
              sep="")
      eq_coef <- 
        paste(paste(
          coef_lhs, coef_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_coef))
    
      coef.wls_lhs <- 
        paste("slope.wls",1:k,sep = "")
      coef.wls_rhs <- 
        paste("rbind(slope.wls",1:k,
              ",zoo(coef(reg.wls",1:k,")[2],index(data$erp)[m-1]))",
              sep = "")
      eq_coef.wls <- 
        paste(paste(
          coef.wls_lhs, coef.wls_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_coef.wls))
      
    # (note that the index is for m-1 to facilitate calculations later)
      
  }
    
    #### Slope list ------
    slopelist_lhs <- 
          paste("slopelist[[",1:k,"]]",sep = "")
    slopelist_rhs <- 
      paste("slope",1:k,sep = "")
    eq_slopelist <- 
      paste(paste(
        slopelist_lhs, slopelist_rhs, sep = "<-"), collapse = ";")
    eval(parse(text = eq_slopelist))
    
    slopelist.wls_lhs <- 
      paste("slopelist.wls[[",1:k,"]]",sep = "")
    slopelist.wls_rhs <- 
      paste("slope.wls",1:k,sep = "")
    eq_slopelist.wls <- 
      paste(paste(
        slopelist.wls_lhs, slopelist.wls_rhs, sep = "<-"), collapse = ";")
    eval(parse(text = eq_slopelist.wls))
    
    allslopes <- 
      do.call(merge,slopelist)
    allslopes.wls <- 
      do.call(merge,slopelist.wls)
    
    dat <- 
      merge(data,allslopes,all = T)
    dat.wls <- 
      merge(data,allslopes.wls,all = T)
    
    signals <- dat[,2:(k+1)]
    
    slopes <- 
      dat[,(k+2):(ncol(dat))]
    slopes.wls <- 
      dat.wls[,(k+2):(ncol(dat))]
    
    colnames(slopes) <- colnames(signals)
    colnames(slopes.wls)<-colnames(signals)
    
    ### Initialize vectors --------
    historical.mean <- NULL; actual.erp <- NULL
    predicted <- NULL; predicted.wls <- NULL
    coefficient <- NULL; coefficient.wls <- NULL

    ### Main loop for OOS forecasts -------

        for (m in (train*freq+period):(n-h+1)){
      
      ##### WLS weights -------------
      
      rv.pred <- rv[2:(m-h)]
      
      rv.s <- rv.short[2:(m-h)]
      
      rv.l <- rv.long[2:(m-h)]
      
      rv.reg <- lm(rv.pred ~ rv.s+rv.l)
      
      rv.exante <-
        sqrt(rv.reg$fitted.values)
      
      sigma.weights <- 1 / rv.exante
      
      #### Cross-sectional regressions ------ 
      
      factor <- NULL ; factor.wls <- NULL
      
      for (t in 1:(m-1)){
        
        unscaled_signal <- 
          signals[1:(m-1),]

        scaled_signal <- 
          scale(unscaled_signal)
        
        signal <- 
          as.numeric(scaled_signal[t,])
        
        estslope <- 
          as.numeric(slopes[(m-1),])
        estslope.wls <-
          as.numeric(slopes.wls[(m-1),])
        
        cross.sect.reg <- 
          lm(signal ~ estslope)
        cross.sect.reg.wls <- 
          lm(signal ~ estslope.wls)
        
        beta <- coef(cross.sect.reg)[2]
        beta.wls <- coef(cross.sect.reg.wls)[2]
        
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

      ### Return prediction ------
      
      premium <- 
        premium.longhorizon[2:(m-h)]
      
      pred <- factor[1:(m-h-1)]
      pred.wls <- factor.wls[1:(m-h-1)]
      
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
      
      ##### Print time period ------
      l <- length(coefficient)
      getindex <- as.yearmon(index(coefficient[l]))
      print(getindex)
      
      #### Prediction ------
      x.pred <- data.frame(pred <- factor[m-1])
      x.pred.wls <- 
        data.frame(pred.wls <- factor.wls[m-1])
      
      prediction <- 
        predict(reg,x.pred)
      prediction.wls <- 
        predict(reg.wls,x.pred.wls)

      if(CT == T){
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

    ## CSPE differences -----
    cspe.diff <- 
      cumsum(spe.mean)-cumsum(spe.pred)
    cspe.diff.wls <- 
      cumsum(spe.mean)-cumsum(spe.pred.wls)
    
    ## Output ------
    return(list(historical.mean = historical.mean,
                predicted = predicted,predicted.wls = predicted.wls,
                actual.erp = actual.erp,
                spe.mean = spe.mean,
                coefficient = coefficient,coefficient.wls = coefficient.wls,
                spe.pred = spe.pred,spe.pred.wls = spe.pred.wls,
                cspe.diff = cspe.diff,cspe.diff.wls = cspe.diff.wls))
    
}