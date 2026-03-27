################################################################
################# Out-of-sample DMSPE forecasts ################
################################################################

## Phi helper function ----

phi_calc <-
  function(theta,
           predictor,
           startyear,
           freq) {
    dat <- as.numeric(predictor)
    
    n <- length(dat)
    
    phi_ind <- vector("numeric", n)
    
    for (q in 1:n) {
      s <- 0
      
      for (t in (q - 1):0) {
        s <- s + theta ^ (q - 1 - t) * dat[t + 1]
        
        phi_ind[q] <- s
        
      }
      
    }
    
    phi_ind <-
      as.zoo(ts(phi_ind,
                start = startyear,
                frequency = freq))
    
    return(phi_ind = phi_ind)
    
  }

## Function for forecast calculation ------

dmspe.forecast <-
  function(erp,
           # unless noted, see 2.1 for definitions
           predictor,
           start,
           end,
           freq,
           train,
           k,
           period = 1,
           h = 1,
           holdout = 10,
           # length of hold-out period in years
           theta,
           # discount parameter
           CT = FALSE) {
    dat <- merge(erp, predictor)
    
    data <-
      window(dat,
             index(dat$erp)[(start - (train * freq)):end])
    
    n <- length(data$erp)
    
    ### Initialize vectors and lists ----
    
    historical.mean <- NULL
    actual.erp <- NULL
    
    forecastvector_lhs <-
      paste("predicted", 1:k, sep = "")
    forecastvector_rhs <-
      paste("NULL", sep = "")
    eq_forecastvector <-
      paste(paste(forecastvector_lhs, forecastvector_rhs, sep = "<-"),
            collapse = ";")
    eval(parse(text = eq_forecastvector))
    
    forecastvector.wls_lhs <-
      paste("predicted.wls", 1:k, sep = "")
    forecastvector.wls_rhs <-
      paste("NULL", sep = "")
    eq_forecastvector.wls <-
      paste(paste(forecastvector.wls_lhs, forecastvector.wls_rhs, sep = "<-"),
            collapse = ";")
    eval(parse(text = eq_forecastvector.wls))
    
    forecastlist <- list()
    spelist <- list()
    forecastlist.wls <- list()
    spelist.wls <- list()
    
    ### WLS preliminaries --------
    
    rv.longhorizon <- NULL
    
    l <- length(forecast.data$svar)
    svar <- forecast.data$svar
    
    for (t in 1:(l - h)) {
      rv.longhorizon[t] <-
        sum(svar[t:(t + (h - 1))]) / h
      
    }
    
    rv <-
      rv.longhorizon[(start - (train * freq)):end]
    rv.short <-
      stats::lag(svar, -1)
    rv.short <-
      rv.short[(start - (train * freq) - 1):(end - 1)]
    rv.long <-
      rollmean(svar,
               freq,
               align = "right")
    rv.long <-
      rv.long[(start - (train * freq) - freq):(end - freq)]
    rv.long <-
      stats::lag(rv.long, -1)
    
    ## Main loop ----
    for (m in ((train - holdout) * freq + period):(n - h + 1)) {
      #### WLS weights ----
      
      rv.pred <- rv[2:(m - h)]
      rv.s <- rv.short[2:(m - h)]
      rv.l <- rv.long[2:(m - h)]
      
      rv.reg <- lm(rv.pred ~ rv.s + rv.l)
      
      rv.exante <-
        sqrt(rv.reg$fitted.values)
      
      sigma.weights <- 1 / rv.exante

      ### Premium preparation ----
      premium.longhorizon <- NULL
      
      for (t in 1:(m - h)) {
        premium.longhorizon[t] <-
          sum(data$erp[t:(t + (h - 1))]) / h
        
      }
      
      premium <-
        premium.longhorizon[2:(m - h)]
      
      historical.avg <-
        mean(data$erp[2:(m - 1)])
      historical.mean <-
        rbind(historical.mean,
              zoo(historical.avg, index(data$erp)[m]))
      
      actual.erp.t <-
        sum(data$erp[m:(m + h - 1)]) * (1 / h)
      actual.erp <-
        rbind(actual.erp,
              zoo(actual.erp.t, index(data$erp)[m]))
      
      spe.mean <- (historical.mean - actual.erp) ^ 2
      
      ### Predictive regression ----
      predictor_lhs <-
        paste("predictor", 1:k, sep = "")
      predictor_rhs <-
        paste("data[1:(m-1),", 2:(k + 1), "]", sep = "")
      eq_predictor <-
        paste(paste(predictor_lhs, predictor_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_predictor))
      
      pred_lhs <-
        paste("pred", 1:k, sep = "")
      pred_rhs <-
        paste("predictor", 1:k, "[1:(m-h-1)]", sep = "")
      eq_pred <-
        paste(paste(pred_lhs, pred_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_pred))
      
      reg_lhs <-
        paste("reg", 1:k, sep = "")
      reg_rhs <-
        paste("lm(premium ~ pred", 1:k, ")", sep = "")
      eq_reg <-
        paste(paste(reg_lhs, reg_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_reg))
      
      reg.wls_lhs <-
        paste("reg.wls", 1:k, sep = "")
      reg.wls_rhs <-
        paste("lm(premium ~ pred", 1:k, ",weights = sigma.weights^2)", sep = "")
      eq_reg.wls <-
        paste(paste(reg.wls_lhs, reg.wls_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_reg.wls))
      
      ### Predictions ----
      xpred_lhs <-
        paste("x_pred", 1:k, sep = "")
      xpred_rhs <-
        paste("data.frame(pred",
              1:k,
              "<-data[(m-1),",
              2:(k + 1),
              "])",
              sep = "")
      eq_xpred <-
        paste(paste(xpred_lhs, xpred_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_xpred))
      
      prediction_lhs <-
        paste("prediction", 1:k, sep = "")
      prediction_rhs <-
        paste("(predict(reg", 1:k, ",x_pred", 1:k, "))", sep = "")
      eq_prediction <-
        paste(paste(prediction_lhs, prediction_rhs, sep = "<-"),
              collapse = ";")
      eval(parse(text = eq_prediction))
      
      prediction.wls_lhs <-
        paste("prediction.wls", 1:k, sep = "")
      prediction.wls_rhs <-
        paste("(predict(reg.wls", 1:k, ",x_pred", 1:k, "))", sep = "")
      eq_prediction.wls <-
        paste(paste(prediction.wls_lhs, prediction.wls_rhs, sep = "<-"),
              collapse = ";")
      eval(parse(text = eq_prediction.wls))
      
      predictionlist <-
        vector(mode = "list", length = k)
      predictionlist.wls <-
        vector(mode = "list", length = k)
      
      prediction_lhs <-
        paste("predictionlist[[", 1:k, "]]", sep = "")
      prediction_rhs <-
        paste("(predict(reg", 1:k, ",x_pred", 1:k, "))", sep = "")
      eq_prediction <-
        paste(paste(prediction_lhs, prediction_rhs, sep = "<-"),
              collapse = ";")
      eval(parse(text = eq_prediction))
      
      prediction.wls_lhs <-
        paste("predictionlist.wls[[", 1:k, "]]", sep = "")
      prediction.wls_rhs <-
        paste("(predict(reg.wls", 1:k, ",x_pred", 1:k, "))", sep = "")
      eq_prediction.wls <-
        paste(paste(prediction.wls_lhs, prediction.wls_rhs, sep = "<-"),
              collapse = ";")
      eval(parse(text = eq_prediction.wls))
      
      if (CT == TRUE) {
        ### CT restrictions ----
        #### Positive coefficients ----
        for (s in pos) {
          coefsign_lhs <-
            paste("predictionlist[[", s, "]]", sep = "")
          coefsign_rhs <-
            paste(
              "ifelse(coef(reg",
              s,
              ")[2]>0,predictionlist[[",
              s,
              "]],historical.avg)",
              sep = ""
            )
          eq_coefsign <-
            paste(paste(coefsign_lhs, coefsign_rhs, sep = "<-"),
                  collapse = ";")
          eval(parse(text = eq_coefsign))
          
          coefsign.wls_lhs <-
            paste("predictionlist.wls[[", s, "]]", sep = "")
          coefsign.wls_rhs <-
            paste(
              "ifelse(coef(reg.wls",
              s,
              ")[2]>0,predictionlist.wls[[",
              s,
              "]],historical.avg)",
              sep = ""
            )
          eq_coefsign.wls <-
            paste(paste(coefsign.wls_lhs, coefsign.wls_rhs, sep = "<-"),
                  collapse = ";")
          eval(parse(text = eq_coefsign.wls))
          
        }
        
        #### Negative coefficients ----
        for (s in neg) {
          coefsign_lhs <-
            paste("predictionlist[[", s, "]]", sep = "")
          coefsign_rhs <-
            paste(
              "ifelse(coef(reg",
              s,
              ")[2]<0,predictionlist[[",
              s,
              "]],historical.avg)",
              sep = ""
            )
          eq_coefsign <-
            paste(paste(coefsign_lhs, coefsign_rhs, sep = "<-"),
                  collapse = ";")
          eval(parse(text = eq_coefsign))
          
          coefsign.wls_lhs <-
            paste("predictionlist.wls[[", s, "]]", sep = "")
          coefsign.wls_rhs <-
            paste(
              "ifelse(coef(reg.wls",
              s,
              ")[2]<0,predictionlist.wls[[",
              s,
              "]],historical.avg)",
              sep = ""
            )
          eq_coefsign.wls <-
            paste(paste(coefsign.wls_lhs, coefsign.wls_rhs, sep = "<-"),
                  collapse = ";")
          eval(parse(text = eq_coefsign.wls))
          
        }
        
        #### Non-negativity -----
        nonneg_lhs <-
          paste("predictionlist[[", 1:k, "]]", sep = "")
        nonneg_rhs <-
          paste("ifelse(predictionlist[[",
                1:k,
                "]]>0,predictionlist[[",
                1:k,
                "]],0)",
                sep = "")
        eq_nonneg <-
          paste(paste(nonneg_lhs, nonneg_rhs, sep = "<-"), collapse = ";")
        eval(parse(text = eq_nonneg))
        
        nonneg.wls_lhs <-
          paste("predictionlist.wls[[", 1:k, "]]", sep = "")
        nonneg.wls_rhs <-
          paste(
            "ifelse(predictionlist.wls[[",
            1:k,
            "]]>0,predictionlist.wls[[",
            1:k,
            "]],0)",
            sep = ""
          )
        eq_nonneg.wls <-
          paste(paste(nonneg.wls_lhs, nonneg.wls_rhs, sep = "<-"),
                collapse = ";")
        eval(parse(text = eq_nonneg.wls))
      }
      forecast_lhs <- paste("predicted", 1:k, sep = "")
      forecast_rhs <-
        paste(
          "rbind(predicted",
          1:k,
          ",zoo(predictionlist[[",
          1:k,
          "]],index(data$erp)[m]))",
          sep = ""
        )
      eq_forecast <-
        paste(paste(forecast_lhs, forecast_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_forecast))
      
      forecast.wls_lhs <-
        paste("predicted.wls", 1:k, sep = "")
      forecast.wls_rhs <-
        paste(
          "rbind(predicted.wls",
          1:k,
          ",zoo(predictionlist.wls[[",
          1:k,
          "]],index(data$erp)[m]))",
          sep = ""
        )
      eq_forecast.wls <-
        paste(paste(forecast.wls_lhs, forecast.wls_rhs, sep = "<-"),
              collapse = ";")
      eval(parse(text = eq_forecast.wls))
      
         
    }
    
   ### Squared predictive error ----
    for (i in 1:k) {
      variable_name <- paste0("spe.pred", i)
      assign(variable_name, (actual.erp - get(paste0("predicted", i)))^2)
      
      variable_name <- paste0("spe.pred.wls", i)
      assign(variable_name, (actual.erp - get(paste0("predicted.wls", i)))^2)
    }

    ### List forecasts ----
    forecastlist_lhs <-
      paste("forecastlist[[", 1:k, "]]", sep = "")
    forecastlist_rhs <-
      paste("predicted", 1:k, sep = "")
    eq_forecastlist <-
      paste(paste(forecastlist_lhs, forecastlist_rhs, sep = "<-"),
            collapse = ";")
    eval(parse(text = eq_forecastlist))
    
    forecastlist.wls_lhs <-
      paste("forecastlist.wls[[", 1:k, "]]", sep = "")
    forecastlist.wls_rhs <-
      paste("predicted.wls", 1:k, sep = "")
    eq_forecastlist.wls <-
      paste(paste(forecastlist.wls_lhs, forecastlist.wls_rhs, sep = "<-"),
            collapse = ";")
    eval(parse(text = eq_forecastlist.wls))
    
    ### List SPE ----
    spelist_lhs <-
      paste("spelist[[", 1:k, "]]", sep = "")
    spelist_rhs <-
      paste("spe.pred", 1:k, sep = "")
    eq_spelist <-
      paste(paste(spelist_lhs, spelist_rhs, sep = "<-"), collapse = ";")
    eval(parse(text = eq_spelist))
    
    spelist.wls_lhs <-
      paste("spelist.wls[[", 1:k, "]]", sep = "")
    spelist.wls_rhs <-
      paste("spe.pred.wls", 1:k, sep = "")
    eq_spelist.wls <-
      paste(paste(spelist.wls_lhs, spelist.wls_rhs, sep = "<-"),
            collapse = ";")
    eval(parse(text = eq_spelist.wls))
    
    ### Phi calculation -----
    philist.theta1 <- list()
    philist.theta1.wls <- list()
    philist.theta0.9 <- list()
    philist.theta0.9.wls <- list()
    philist.theta0.5 <- list()
    philist.theta0.5.wls <- list()
    
    getindex <- as.yearmon(index(spelist[[1]]))[1]
    startyear <- as.numeric(lubridate::year(getindex))
    
    #### Phi loop -----
    for (i in 1:k) {
      philist.theta1[[i]] <-
        stats::lag(phi_calc(
          predictor = spelist[[i]],
          theta = 1,
          startyear = startyear,
          freq = freq
        ),-1)
      philist.theta0.9[[i]] <-
        stats::lag(phi_calc(
          predictor = spelist[[i]],
          theta = 0.9,
          startyear = startyear,
          freq = freq
        ),-1)
      philist.theta0.5[[i]] <-
        stats::lag(phi_calc(
          predictor = spelist[[i]],
          theta = 0.5,
          startyear = startyear,
          freq = freq
        ),-1)
      
      philist.theta1.wls[[i]] <-
        stats::lag(phi_calc(
          predictor = spelist.wls[[i]],
          theta = 1,
          startyear = startyear,
          freq = freq
        ),-1)
      philist.theta0.9.wls[[i]] <-
        stats::lag(
          phi_calc(
            predictor = spelist.wls[[i]],
            theta = 0.9,
            startyear = startyear,
            freq = freq
          ),-1
        )
      philist.theta0.5.wls[[i]] <-
        stats::lag(
          phi_calc(
            predictor = spelist.wls[[i]],
            theta = 0.5,
            startyear = startyear,
            freq = freq
          ),-1
        )
      
    }
    
    q <- length(philist.theta1[[1]])
    
    phi.theta1 <-
      data.frame(matrix(unlist(philist.theta1),
                        nrow = q,
                        byrow = FALSE))
    phi.theta0.9 <-
      data.frame(matrix(
        unlist(philist.theta0.9),
        nrow = q,
        byrow = FALSE
      ))
    phi.theta0.5 <-
      data.frame(matrix(
        unlist(philist.theta0.5),
        nrow = q,
        byrow = FALSE
      ))
    
    phi.theta1.wls <-
      data.frame(matrix(
        unlist(philist.theta1.wls),
        nrow = q,
        byrow = FALSE
      ))
    phi.theta0.9.wls <-
      data.frame(matrix(
        unlist(philist.theta0.9.wls),
        nrow = q,
        byrow = FALSE
      ))
    phi.theta0.5.wls <-
      data.frame(matrix(
        unlist(philist.theta0.5.wls),
        nrow = q,
        byrow = FALSE
      ))
    
    #### Zoo conversion ----
    phi.theta1 <-
      as.zoo(ts(phi.theta1,
                start = c((startyear), (period + 1)),
                frequency = freq))
    phi.theta0.9 <-
      as.zoo(ts(phi.theta0.9,
                start = c((startyear), (period + 1)),
                frequency = freq))
    phi.theta0.5 <-
      as.zoo(ts(phi.theta0.5,
                start = c((startyear), (period + 1)),
                frequency = freq))
    
    phi.theta1.wls <-
      as.zoo(ts(
        phi.theta1.wls,
        start = c((startyear), (period + 1)),
        frequency = freq
      ))
    phi.theta0.9.wls <-
      as.zoo(ts(
        phi.theta0.9.wls,
        start = c((startyear), (period + 1)),
        frequency = freq
      ))
    phi.theta0.5.wls <-
      as.zoo(ts(
        phi.theta0.5.wls,
        start = c((startyear), (period + 1)),
        frequency = freq
      ))
    
    #### Reciprocal of phi ----
    
    for (i in 1:k) {
      phi.theta1[, i] <- 1 / (phi.theta1[, i])
      phi.theta0.9[, i] <- 1 / (phi.theta0.9[, i])
      phi.theta0.5[, i] <- 1 / (phi.theta0.5[, i])
      
      phi.theta1.wls[, i] <- 1 / (phi.theta1.wls[, i])
      phi.theta0.9.wls[, i] <- 1 / (phi.theta0.9.wls[, i])
      phi.theta0.5.wls[, i] <- 1 / (phi.theta0.5.wls[, i])
      
    }
    
    #### Longhorizon ----
    
    if (h >= 2) {
      longhorizon.matrix <-
        matrix(NA, (h - 1), k)
      
      for (i in 1:(h - 1)) {
        longhorizon.matrix[i, ] <-
          rep(1, k)
        
      }
      
      longhorizon.matrix <-
        as.zoo(ts(
          longhorizon.matrix,
          start = startyear,
          frequency = freq
        ))
      
      colnames(longhorizon.matrix) <-
        colnames(phi.theta1)
      
      p <- nrow(phi.theta1)
      
      ##### Phi calculation -----
      phi.theta1 <-
        stats::lag(phi.theta1[1:(p - h + 1), ],-(h - 1))
      phi.theta0.9 <-
        stats::lag(phi.theta0.9[1:(p - h + 1), ],-(h - 1))
      phi.theta0.5 <-
        stats::lag(phi.theta0.5[1:(p - h + 1), ],-(h - 1))
      
      phi.theta1.wls <-
        stats::lag(phi.theta1.wls[1:(p - h + 1), ],-(h - 1))
      phi.theta0.9.wls <-
        stats::lag(phi.theta0.9.wls[1:(p - h + 1), ],-(h - 1))
      phi.theta0.5.wls <-
        stats::lag(phi.theta0.5.wls[1:(p - h + 1), ],-(h - 1))
      
      phi.theta1 <-
        rbind(longhorizon.matrix, phi.theta1)
      phi.theta0.9 <-
        rbind(longhorizon.matrix, phi.theta0.9)
      phi.theta0.5 <-
        rbind(longhorizon.matrix, phi.theta0.5)
      
      phi.theta1.wls <-
        rbind(longhorizon.matrix, phi.theta1.wls)
      phi.theta0.9.wls <-
        rbind(longhorizon.matrix, phi.theta0.9.wls)
      phi.theta0.5.wls <-
        rbind(longhorizon.matrix, phi.theta0.5.wls)
      
    }
    
    ### Row sums ----
    phi.theta1$phi_sum <-
      rowSums(phi.theta1[, 1:k])
    phi.theta0.9$phi_sum <-
      rowSums(phi.theta0.9[, 1:k])
    phi.theta0.5$phi_sum <-
      rowSums(phi.theta0.5[, 1:k])
    
    phi.theta1.wls$phi_sum <-
      rowSums(phi.theta1.wls[, 1:k])
    phi.theta0.9.wls$phi_sum <-
      rowSums(phi.theta0.9.wls[, 1:k])
    phi.theta0.5.wls$phi_sum <-
      rowSums(phi.theta0.5.wls[, 1:k])
    
    # define t as the number of periods from the start of the holdout period
    
    t <- length(phi.theta1[, 1])
    
    ### Weight calculation ----
    
    for (j in 1:t) {
      for (i in 1:k) {
        phi.theta1[j, i] <-
          phi.theta1[j, i] / (phi.theta1[j, "phi_sum"])
        phi.theta0.9[j, i] <-
          phi.theta0.9[j, i] / (phi.theta0.9[j, "phi_sum"])
        phi.theta0.5[j, i] <-
          phi.theta0.5[j, i] / (phi.theta0.5[j, "phi_sum"])
        
        phi.theta1.wls[j, i] <-
          phi.theta1.wls[j, i] / (phi.theta1.wls[j, "phi_sum"])
        phi.theta0.9.wls[j, i] <-
          phi.theta0.9.wls[j, i] / (phi.theta0.9.wls[j, "phi_sum"])
        phi.theta0.5.wls[j, i] <-
          phi.theta0.5.wls[j, i] / (phi.theta0.5.wls[j, "phi_sum"])
        
      }
      
    }
    
    
    forecast.length <- length(forecastlist[[1]])
    
    forecast <-
      data.frame(matrix(unlist(forecastlist),
                        nrow = forecast.length,
                        byrow = FALSE))
    forecast.wls <-
      data.frame(matrix(
        unlist(forecastlist.wls),
        nrow = forecast.length,
        byrow = FALSE
      ))
    
    ### Prediction -----
    pred <-
      as.zoo(ts(
        forecast,
        start = c(startyear, period),
        frequency = freq
      ))
    pred.wls <-
      as.zoo(ts(
        forecast.wls,
        start = c(startyear, period),
        frequency = freq
      ))
    
    pred <-
      window(pred)[(holdout * freq):forecast.length]
    pred.wls <-
      window(pred.wls)[(holdout * freq):forecast.length]
    
    #### Weights over OOS period ----
    weight.theta1 <-
      subset(phi.theta1, select = -c(phi_sum))
    weight.theta0.9 <-
      subset(phi.theta0.9, select = -c(phi_sum))
    weight.theta0.5 <-
      subset(phi.theta0.5, select = -c(phi_sum))
    
    weight.theta1.wls <-
      subset(phi.theta1.wls, select = -c(phi_sum))
    weight.theta0.9.wls <-
      subset(phi.theta0.9.wls, select = -c(phi_sum))
    weight.theta0.5.wls <-
      subset(phi.theta0.5.wls, select = -c(phi_sum))
    
    weight.theta1 <-
      window(weight.theta1,
             index(weight.theta1$X1))[(holdout * freq):(forecast.length -
                                                          1)]
    weight.theta0.9 <-
      window(weight.theta0.9,
             index(weight.theta0.9$X1))[(holdout * freq):(forecast.length -
                                                            1)]
    weight.theta0.5 <-
      window(weight.theta0.5,
             index(weight.theta0.5$X1))[(holdout * freq):(forecast.length -
                                                            1)]
    
    weight.theta1.wls <-
      window(weight.theta1.wls,
             index(weight.theta1.wls$X1))[(holdout * freq):(forecast.length -
                                                              1)]
    weight.theta0.9.wls <-
      window(weight.theta0.9.wls,
             index(weight.theta0.9.wls$X1))[(holdout * freq):(forecast.length -
                                                                1)]
    weight.theta0.5.wls <-
      window(weight.theta0.5.wls,
             index(weight.theta0.5.wls$X1))[(holdout * freq):(forecast.length -
                                                                1)]
    
    #### Element-wise multiplication ----
    pred.weighted.theta1 <-
      pred * weight.theta1
    pred.weighted.theta0.9 <-
      pred * weight.theta0.9
    pred.weighted.theta0.5 <-
      pred * weight.theta0.5
    
    pred.weighted.theta1.wls <-
      pred.wls * weight.theta1.wls
    pred.weighted.theta0.9.wls <-
      pred.wls * weight.theta0.9.wls
    pred.weighted.theta0.5.wls <-
      pred.wls * weight.theta0.5.wls
    
    #### Row sums as prediction -----
    predicted.theta1 <-
      rowSums(pred.weighted.theta1)
    predicted.theta0.9 <-
      rowSums(pred.weighted.theta0.9)
    predicted.theta0.5 <-
      rowSums(pred.weighted.theta0.5)
    
    predicted.theta1.wls <-
      rowSums(pred.weighted.theta1.wls)
    predicted.theta0.9.wls <-
      rowSums(pred.weighted.theta0.9.wls)
    predicted.theta0.5.wls <-
      rowSums(pred.weighted.theta0.5.wls)
    
    s <- length(actual.erp)
    
    actual.erp <-
      actual.erp[(holdout * freq + 1):s]
    spe.mean <-
      spe.mean[(holdout * freq + 1):s]
    historical.mean <-
      historical.mean[(holdout * freq + 1):s]
    
    #### Squared prediction error ----
    spe.pred.theta1 <-
      (actual.erp - predicted.theta1) ^ 2
    spe.pred.theta0.9 <-
      (actual.erp - predicted.theta0.9) ^ 2
    spe.pred.theta0.5 <-
      (actual.erp - predicted.theta0.5) ^ 2
    
    spe.pred.theta1.wls <-
      (actual.erp - predicted.theta1.wls) ^ 2
    spe.pred.theta0.9.wls <-
      (actual.erp - predicted.theta0.9.wls) ^ 2
    spe.pred.theta0.5.wls <-
      (actual.erp - predicted.theta0.5.wls) ^ 2
    
    #### Zoo forecasts ----
    predicted.theta1 <-
      as.zoo(ts(
        predicted.theta1,
        start = c(startyear + holdout, period),
        frequency = freq
      ))
    predicted.theta0.9 <-
      as.zoo(ts(
        predicted.theta0.9,
        start = c(startyear + holdout, period),
        frequency = freq
      ))
    predicted.theta0.5 <-
      as.zoo(ts(
        predicted.theta0.5,
        start = c(startyear + holdout, period),
        frequency = freq
      ))
    
    predicted.theta1.wls <-
      as.zoo(ts(
        predicted.theta1.wls,
        start = c(startyear + holdout, period),
        frequency = freq
      ))
    predicted.theta0.9.wls <-
      as.zoo(ts(
        predicted.theta0.9.wls,
        start = c(startyear + holdout, period),
        frequency = freq
      ))
    predicted.theta0.5.wls <-
      as.zoo(ts(
        predicted.theta0.5.wls,
        start = c(startyear + holdout, period),
        frequency = freq
      ))
    
    ### CSPE differences ----
    cspe.diff.theta1 <-
      cumsum(spe.mean) - cumsum(spe.pred.theta1)
    cspe.diff.theta0.9 <-
      cumsum(spe.mean) - cumsum(spe.pred.theta0.9)
    cspe.diff.theta0.5 <-
      cumsum(spe.mean) - cumsum(spe.pred.theta0.5)
    
    cspe.diff.theta1.wls <-
      cumsum(spe.mean) - cumsum(spe.pred.theta1.wls)
    cspe.diff.theta0.9.wls <-
      cumsum(spe.mean) - cumsum(spe.pred.theta0.9.wls)
    cspe.diff.theta0.5.wls <-
      cumsum(spe.mean) - cumsum(spe.pred.theta0.5.wls)
    
    ### Output prepation ----
    theta1 <-
      list(
        predicted = predicted.theta1,
        predicted.wls = predicted.theta1.wls,
        spe.pred = spe.pred.theta1,
        spe.pred.wls = spe.pred.theta1.wls,
        cspe.diff = cspe.diff.theta1,
        cspe.diff.wls = cspe.diff.theta1.wls
      )
    
    theta0.9 <-
      list(
        predicted = predicted.theta0.9,
        predicted.wls = predicted.theta0.9.wls,
        spe.pred = spe.pred.theta0.9,
        spe.pred.wls = spe.pred.theta0.9.wls,
        cspe.diff = cspe.diff.theta0.9,
        cspe.diff.wls = cspe.diff.theta0.9.wls
      )
    
    theta0.5 <-
      list(
        predicted = predicted.theta0.5,
        predicted.wls = predicted.theta0.5.wls,
        spe.pred = spe.pred.theta0.5,
        spe.pred.wls = spe.pred.theta0.5.wls,
        cspe.diff = cspe.diff.theta0.5,
        cspe.diff.wls = cspe.diff.theta0.5.wls
      )
    ### Output ----
    return(
      list(
        actual.erp = actual.erp,
        spe.mean = spe.mean,
        historical.mean = historical.mean,
        theta1 = theta1,
        theta0.9 = theta0.9,
        theta0.5 = theta0.5
      )
    )
    
  }
