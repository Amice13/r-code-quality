################################################################
################# Out-of-sample ENet forecasts #################
################################################################

## Function for forecast calculation ------

enet.forecast <- 
  function(erp,
           predictor,
           freq,
           k,
           h=1,
           start,
           end,
           train,
           period=1,
           alpha=0.5,
           CT = FALSE){
  
    data <- 
      merge(erp,predictor,all=FALSE)
  
    data <- 
      window(data,index(data$erp)[(start-(train*freq)):end])

    ### Initialize vectors ----
  for (i in 1:k){
    
    name <- paste("coefficients",i,sep = "")
    name.wls <- paste("coefficients.wls",i,sep ="")
    
    assign(name,NULL); assign(name.wls,NULL)
    
  }  
  
      predicted <- NULL; predicted.wls <- NULL
      historical.mean <- NULL; actual.erp <- NULL
      lambda <- NULL; lambda.wls <- NULL
      
      coefficientlist <- list()
      coefficientlist.wls <- list()
  
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
        
      n <- length(data$erp)
  
      ### Main loop ----
      
  for (m in (train*freq+period):(n-h+1)){
    
    #### WLS weights -------------
    
    rv.pred <- rv[2:(m-h)]
    
    rv.s <- rv.short[2:(m-h)]
    
    rv.l <- rv.long[2:(m-h)]
    
    rv.reg <- lm(rv.pred ~ rv.s+rv.l)
    
  	rv.exante <-
        sqrt(rv.reg$fitted.values)
      
      sigma.weights <- 1 / rv.exante
    
    
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
    
    spe.mean <- 
      (historical.mean-actual.erp)^2
    
      premium.longhorizon <- NULL
      
      for (t in 1:(m-h)){ 
        
        premium.longhorizon[t] <-
          sum(data$erp[t:(t+(h-1))])/h
        
      }
      
      
      premium <- 
        as.matrix(premium.longhorizon[2:(m-h)])
      
      pred <- 
        as.matrix(data[1:(m-h-1),2:(k+1)])
      
      ### Predictive regression ----
      
      if(CT == FALSE){
        enet <- 
          ic.glmnet(pred,
                    premium,
                    crit = "aicc",
                    alpha = alpha)
        
        enet.wls <- 
          ic.glmnet(pred,
                    premium,
                    crit = "aicc",
                    alpha = alpha,
                    weights = sigma.weights^2)
      }
      
      if(CT == TRUE){
      enet <- 
        ic.glmnet(pred,
                  premium,
                  crit = "aicc",
                  alpha = alpha,
                  lower.limits = lower.enet,
                  upper.limits = upper.enet)
      
      enet.wls <- 
        ic.glmnet(pred,
                  premium,
                  crit = "aicc",
                  alpha = alpha,
                  lower.limits = lower.enet,
                  upper.limits = upper.enet,
                  weights = sigma.weights^2)
      }
      lambda.recursive <- enet$lambda
      lambda.recursive.wls <- enet.wls$lambda

      ### Initialize coefficient vectors ----
      
    for (i in 2:(k+1)){
      
      coefficient <- coef(enet)[i]
      coefficient.wls <- coef(enet.wls)[i]
      
      name <- paste("beta",i-1,sep = "") 
      name.wls <- paste("beta.wls",i-1,sep = "")
      
      assign(name,coefficient)
      assign(name.wls,coefficient.wls)
      
    } 
    
      pred <- as.matrix(data[m-1,2:(k+1)])

      ### Forecast ----
      forecast <- predict(enet,pred)
      forecast.wls <- predict(enet.wls,pred)
    
      if(CT == T){
      forecast <- 
        ifelse(forecast > 0, 
               forecast, 
               0)
      forecast.wls <- 
        ifelse(forecast.wls > 0, 
               forecast.wls, 
               0)
      }
      predicted <- 
        rbind(predicted,
              zoo(forecast,index(data$erp)[m]))
      predicted.wls <- 
        rbind(predicted.wls,
              zoo(forecast.wls,index(data$erp)[m]))
      
      l <- length(predicted)
      
      lambda <- 
        rbind(lambda,
              zoo(lambda.recursive,index(data$erp)[m]))
      lambda.wls <- 
        rbind(lambda.wls,
              zoo(lambda.recursive.wls,index(data$erp)[m]))
      
      coef_lhs <- 
        paste("coefficients",1:k,sep = "")
      coef_rhs <- 
        paste("rbind(coefficients",1:k,",zoo(beta",1:k,
              ",index(data$erp)[m]))",sep = "")
      eq_coef <- 
        paste(paste(
          coef_lhs, coef_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_coef))
    
      coef.wls_lhs <- 
        paste("coefficients.wls",1:k,sep = "")
      coef.wls_rhs <- 
        paste("rbind(coefficients.wls",1:k,",zoo(beta.wls",1:k,
              ",index(data$erp)[m]))",sep = "")
      eq_coef.wls <- 
        paste(paste(
          coef.wls_lhs, coef.wls_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_coef.wls))
      
      spe.pred <- (actual.erp-predicted)^2
      
      spe.pred.wls<-(actual.erp-predicted.wls)^2
    
  }
  
      ## Coefficient list ----
      coeflist_lhs <- 
        paste("coefficientlist[[",1:k,"]]",sep = "")
      coeflist_rhs <- 
        paste("coefficients",1:k,sep = "")
      eq_coeflist <- 
        paste(paste(
          coeflist_lhs, coeflist_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_coeflist))
      
      coeflist.wls_lhs <- 
        paste("coefficientlist.wls[[",1:k,"]]",sep = "")
      coeflist.wls_rhs <- 
        paste("coefficients.wls",1:k,sep = "")
      eq_coeflist.wls <- 
        paste(paste(
          coeflist.wls_lhs, coeflist.wls_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_coeflist.wls))
      
      cspe.diff <- cumsum(spe.mean)-cumsum(spe.pred)
      cspe.diff.wls <- cumsum(spe.mean)-cumsum(spe.pred.wls)
  
      ## Output ----
  return(list(
    cspe.diff = cspe.diff,cspe.diff.wls = cspe.diff.wls,
    predicted = predicted,
    historical.mean = historical.mean,
    actual.erp = actual.erp,
    predicted.wls = predicted.wls,
    coefficientlist = coefficientlist,
    coefficientlist.wls = coefficientlist.wls,
    spe.pred = spe.pred,spe.pred.wls = spe.pred.wls,
    spe.mean = spe.mean,
    lambda = lambda,lambda.wls = lambda.wls))
    
  }
