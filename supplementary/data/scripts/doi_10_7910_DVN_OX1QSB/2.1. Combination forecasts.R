################################################################
###############Out-of-sample combination forecasts##############
################################################################

## Function for forecast calculation -----------------------------

combination.forecast <-
  function(erp, # equity premium
           predictor, # predictor set
           start, # index of first oos period
           end, # index of last oos period
           freq, # frequency (e.g. freq = 4 is quarterly)
           train, # number of training period (in years)
           k = ncol(predictor), # number of individual variables in the predictor set
           CT = FALSE, # should CT restrictions be used?
           period = 1, # indicates period of first year (e.g., period = 1 is Q1)
           h = 1) { # forecast horizon
    
    dat <- merge(erp, predictor)
    
    data <-
      window(dat,
             index(dat$erp)[(start - (train * freq)):end])
    
    n <- length(data$erp)
    
    ### Initialize vectors -------------------------------------
    
    historical.mean <- NULL
    actual.erp <- NULL
    
    predicted.mean <-
      NULL
    predicted.median <- NULL
    predicted.trim <- NULL
    
    predicted.mean.wls <- NULL
    predicted.median.wls <- NULL
    
    predicted.trim.wls <- NULL
    
    predicted.imc <-
      NULL
    predicted.imd <- NULL
    predicted.itc <- NULL
    
    predicted.imc.wls <- NULL
    predicted.imd.wls <- NULL
    
    predicted.itc.wls <- NULL
    
    lambda.imc <- NULL
    lambda.imd <- NULL
    lambda.itc <- NULL
    
    lambda.imc.wls <-
      NULL
    lambda.imd.wls <- NULL
    lambda.itc.wls <- NULL
    
    ### WLS preliminaries --------------------------------------
    
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
      stats::lag(svar,-1)
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
    
    ### Main loop for OOS forecasts ---------------------------
    
    for (m in (train * freq + period):(n - h + 1)) {
      
      #### WLS weights -------------
      
      rv.pred <- rv[2:(m - h)]
      
      rv.s <- rv.short[2:(m - h)]
      
      rv.l <- rv.long[2:(m - h)]
      
      rv.reg <- lm(rv.pred ~ rv.s + rv.l)
      
         rv.exante <-
        sqrt(rv.reg$fitted.values)
      
      sigma.weights <- 1 / rv.exante

      
      ### Equity premium calculation ------
      
      premium.longhorizon <- NULL

      for (t in 1:(m - h)) {
        premium.longhorizon[t] <-
          sum(data$erp[t:(t + (h - 1))]) / h
      }
      
      premium <-
        premium.longhorizon[2:(m - h)]
      
      ### Predictor data ------
      
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
      
      spe.mean <-
        (historical.mean - actual.erp) ^ 2
      
      ### Predictive regressions -----
      
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
      
      #### Fitted values -----------
      
      fittedlist <-
        vector(mode = "list", length = k)
      fittedlist.wls <-
        vector(mode = "list", length = k)
      
      fitted_lhs <-
        paste("fittedlist[[", 1:k, "]]", sep = "")
      fitted_rhs <-
        paste("fitted(reg", 1:k, ")", sep = "")
      eq_fitted <-
        paste(paste(fitted_lhs, fitted_rhs, sep = "<-"), collapse = ";")
      eval(parse(text = eq_fitted))
      
      fitted.wls_lhs <-
        paste("fittedlist.wls[[", 1:k, "]]", sep = "")
      fitted.wls_rhs <-
        paste("fitted(reg.wls", 1:k, ")", sep = "")
      eq_fitted.wls <-
        paste(paste(fitted.wls_lhs, fitted.wls_rhs, sep = "<-"),
              collapse = ";")
      eval(parse(text = eq_fitted.wls))
      
      l <- length(fittedlist[[1]])
      
      allfitted <-
        data.frame(matrix(unlist(fittedlist),
                          nrow = l,
                          byrow = F))
      allfitted.wls <-
        data.frame(matrix(
          unlist(fittedlist.wls),
          nrow = l,
          byrow = F
        ))
      
      #### In-sample combinations ------
      
      meanrow <-
        rowMeans(allfitted[, 1:k])
      medianrow <-
        rowMedians(as.matrix(allfitted[, 1:k]))
      trimrow <-
        apply(allfitted, 1, mean, trim = 1 / k)
      
      meanrow.wls <-
        rowMeans(allfitted.wls[, 1:k])
      medianrow.wls <-
        rowMedians(as.matrix(allfitted.wls[, 1:k]))
      trimrow.wls <-
        apply(allfitted.wls, 1, mean, trim = 1 / k)
      
      ### IC regressions -----
      
      meanreg <- lm(premium ~ meanrow)
      medianreg <- lm(premium ~ medianrow)
      trimreg <- lm(premium ~ trimrow)
      
      meanreg.wls <- lm(premium ~ meanrow.wls)
      medianreg.wls <- lm(premium ~ medianrow.wls)
      trimreg.wls <- lm(premium ~ trimrow.wls)
      
      lambda.imc <-
        rbind(lambda.imc,
              zoo(coef(meanreg)[2], index(data$erp)[m]))
      lambda.imd <-
        rbind(lambda.imd,
              zoo(coef(medianreg)[2], index(data$erp)[m]))
      lambda.itc <-
        rbind(lambda.itc,
              zoo(coef(trimreg)[2], index(data$erp)[m]))
      
      lambda.imc.wls <-
        rbind(lambda.imc.wls,
              zoo(coef(meanreg.wls)[2], index(data$erp)[m]))
      lambda.imd.wls <-
        rbind(lambda.imd.wls,
              zoo(coef(medianreg.wls)[2], index(data$erp)[m]))
      lambda.itc.wls <-
        rbind(lambda.itc.wls,
              zoo(coef(trimreg.wls)[2], index(data$erp)[m]))
      
      ### Predictions -------
      
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
        ### CT restrictions -----------
        #### Positive coefficients ----------
        
        for (s in pos) {
          coefsign_lhs <-
            paste("predictionlist[[", s, "]]", sep = "")
          coefsign_rhs <-
            paste(
              "ifelse(coef(reg",
              s,
              ")[2]>0,
                predictionlist[[",
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
              ")[2]>0,
                predictionlist.wls[[",
              s,
              "]],historical.avg)",
              sep = ""
            )
          eq_coefsign.wls <-
            paste(paste(coefsign.wls_lhs, coefsign.wls_rhs, sep = "<-"),
                  collapse = ";")
          eval(parse(text = eq_coefsign.wls))
          
        }
        
        #### Negative coefficients -----------
        
        for (s in neg) {
          coefsign_lhs <-
            paste("predictionlist[[", s, "]]", sep = "")
          coefsign_rhs <-
            paste(
              "ifelse(coef(reg",
              s,
              ")[2]<0,
                predictionlist[[",
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
              ")[2]<0,
                predictionlist.wls[[",
              s,
              "]],historical.avg)",
              sep = ""
            )
          eq_coefsign.wls <-
            paste(paste(coefsign.wls_lhs, coefsign.wls_rhs, sep = "<-"),
                  collapse = ";")
          eval(parse(text = eq_coefsign.wls))
          
        }
        
        #### Non-negativity ----------
        nonneg_lhs <-
          paste("predictionlist[[", 1:k, "]]", sep = "")
        nonneg_rhs <-
          paste("ifelse(predictionlist[[",
                1:k,
                "]]>0,
              predictionlist[[",
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
            "]]>0,
              predictionlist.wls[[",
            1:k,
            "]],0)",
            sep = ""
          )
        eq_nonneg.wls <-
          paste(paste(nonneg.wls_lhs, nonneg.wls_rhs, sep = "<-"),
                collapse = ";")
        eval(parse(text = eq_nonneg.wls))
      }
      
      ### Combination forecasts ------------
      prediction <-
        data.frame(matrix(
          unlist(predictionlist),
          nrow = 1,
          byrow = TRUE
        ))
      prediction.wls <-
        data.frame(matrix(
          unlist(predictionlist.wls),
          nrow = 1,
          byrow = TRUE
        ))
      
      meanforecast <-
        rowMeans(prediction)
      medianforecast <-
        rowMedians(as.matrix(prediction))
      trimforecast <-
        apply(prediction, 1, mean, trim = 1 / k)
      
      meanforecast.wls <-
        rowMeans(prediction.wls)
      medianforecast.wls <-
        rowMedians(as.matrix(prediction.wls))
      trimforecast.wls <-
        apply(prediction.wls, 1, mean, trim = 1 / k)
      
      ### IC forecasts -----
      imc <-
        coef(meanreg)[1] + coef(meanreg)[2] * meanforecast
      imd <-
        coef(medianreg)[1] + coef(medianreg)[2] * medianforecast
      itc <-
        coef(trimreg)[1] + coef(trimreg)[2] * trimforecast
      
      imc.wls <-
        coef(meanreg.wls)[1] + coef(meanreg.wls)[2] * meanforecast.wls
      imd.wls <-
        coef(medianreg.wls)[1] + coef(medianreg.wls)[2] * medianforecast.wls
      itc.wls <-
        coef(trimreg.wls)[1] + coef(trimreg.wls)[2] * trimforecast.wls
      #### Non-negativity IC ---------
      
      if (CT == TRUE) {
        imc <- ifelse(imc > 0, imc, 0)
        imd <- ifelse(imd > 0, imd, 0)
        itc <- ifelse(itc > 0, itc, 0)
        
        imc.wls <- ifelse(imc.wls > 0, imc.wls, 0)
        imd.wls <- ifelse(imd.wls > 0, imd.wls, 0)
        itc.wls <- ifelse(itc.wls > 0, itc.wls, 0)
      }
      predicted.mean <-
        rbind(predicted.mean,
              zoo(meanforecast, index(data$erp)[m]))
      predicted.median <-
        rbind(predicted.median,
              zoo(medianforecast, index(data$erp)[m]))
      predicted.trim <-
        rbind(predicted.trim,
              zoo(trimforecast, index(data$erp)[m]))
      
      predicted.mean.wls <-
        rbind(predicted.mean.wls,
              zoo(meanforecast.wls, index(data$erp)[m]))
      predicted.median.wls <-
        rbind(predicted.median.wls,
              zoo(medianforecast.wls, index(data$erp)[m]))
      predicted.trim.wls <-
        rbind(predicted.trim.wls,
              zoo(trimforecast.wls, index(data$erp)[m]))
      
      predicted.imc <-
        rbind(predicted.imc,
              zoo(imc, index(data$erp)[m]))
      predicted.imd <-
        rbind(predicted.imd,
              zoo(imd, index(data$erp)[m]))
      predicted.itc <-
        rbind(predicted.itc,
              zoo(itc, index(data$erp)[m]))
      
      predicted.imc.wls <-
        rbind(predicted.imc.wls,
              zoo(imc.wls, index(data$erp)[m]))
      predicted.imd.wls <-
        rbind(predicted.imd.wls,
              zoo(imd.wls, index(data$erp)[m]))
      predicted.itc.wls <-
        rbind(predicted.itc.wls,
              zoo(itc.wls, index(data$erp)[m]))
      
      ### Squared prediction error -----
      spe.mc <- (actual.erp - predicted.mean) ^ 2
      spe.md <- (actual.erp - predicted.median) ^ 2
      spe.tc <- (actual.erp - predicted.trim) ^ 2
      
      spe.mc.wls <- (actual.erp - predicted.mean.wls) ^ 2
      spe.md.wls <- (actual.erp - predicted.median.wls) ^ 2
      spe.tc.wls <- (actual.erp - predicted.trim.wls) ^ 2
      
      spe.imc <- (actual.erp - predicted.imc) ^ 2
      spe.imd <- (actual.erp - predicted.imd) ^ 2
      spe.itc <- (actual.erp - predicted.itc) ^ 2
      
      spe.imc.wls <- (actual.erp - predicted.imc.wls) ^ 2
      spe.imd.wls <- (actual.erp - predicted.imd.wls) ^ 2
      spe.itc.wls <- (actual.erp - predicted.itc.wls) ^ 2
      
    }
    
    ### CSPE differences -----
    cspe.diff.mean <-
      cumsum(spe.mean) - cumsum(spe.mc)
    cspe.diff.median <-
      cumsum(spe.mean) - cumsum(spe.md)
    cspe.diff.trim <-
      cumsum(spe.mean) - cumsum(spe.tc)
    
    cspe.diff.mean.wls <-
      cumsum(spe.mean) - cumsum(spe.mc.wls)
    cspe.diff.median.wls <-
      cumsum(spe.mean) - cumsum(spe.md.wls)
    cspe.diff.trim.wls <-
      cumsum(spe.mean) - cumsum(spe.tc.wls)
    
    cspe.diff.imc <-
      cumsum(spe.mean) - cumsum(spe.imc)
    cspe.diff.imd <-
      cumsum(spe.mean) - cumsum(spe.imd)
    cspe.diff.itc <-
      cumsum(spe.mean) - cumsum(spe.itc)
    
    cspe.diff.imc.wls <-
      cumsum(spe.mean) - cumsum(spe.imc.wls)
    cspe.diff.imd.wls <-
      cumsum(spe.mean) - cumsum(spe.imd.wls)
    cspe.diff.itc.wls <-
      cumsum(spe.mean) - cumsum(spe.itc.wls)
    
    ### Output preparation -------
    mc <-
      list(
        predicted = predicted.mean,
        predicted.wls = predicted.mean.wls,
        spe.pred = spe.mc,
        spe.pred.wls = spe.mc.wls,
        cspe.diff = cspe.diff.mean,
        cspe.diff.wls = cspe.diff.mean.wls
      )
    
    md <-
      list(
        predicted = predicted.median,
        predicted.wls = predicted.median.wls,
        spe.pred = spe.md,
        spe.pred.wls = spe.md.wls,
        cspe.diff = cspe.diff.median,
        cspe.diff.wls = cspe.diff.median.wls
      )
    
    tc <-
      list(
        predicted = predicted.trim,
        predicted.wls = predicted.trim.wls,
        spe.pred = spe.tc,
        spe.pred.wls = spe.tc.wls,
        cspe.diff = cspe.diff.trim,
        cspe.diff.wls = cspe.diff.trim.wls
      )
    
    imc <-
      list(
        predicted = predicted.imc,
        predicted.wls = predicted.imc.wls,
        spe.pred = spe.imc,
        spe.pred.wls = spe.imc.wls,
        cspe.diff = cspe.diff.imc,
        cspe.diff.wls = cspe.diff.imc.wls
      )
    
    imd <-
      list(
        predicted = predicted.imd,
        predicted.wls = predicted.imd.wls,
        spe.pred = spe.imd,
        spe.pred.wls = spe.imd.wls,
        cspe.diff = cspe.diff.imd,
        cspe.diff.wls = cspe.diff.imd.wls
      )
    
    itc <-
      list(
        predicted = predicted.itc,
        predicted.wls = predicted.itc.wls,
        spe.pred = spe.itc,
        spe.pred.wls = spe.itc.wls,
        cspe.diff = cspe.diff.itc,
        cspe.diff.wls = cspe.diff.itc.wls
      )
    
    ### Output -------
    return(
      list(
        mc = mc,
        md = md,
        tc = tc,
        spe.mean = spe.mean,
        imc = imc,
        imd = imd,
        itc = itc,
        actual.erp = actual.erp,
        historical.mean = historical.mean
      )
    )
  }
