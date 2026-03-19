
extract_dpm <- function(model) {
  
  ##Para datos imputados
  if(class(model)=="list") { 
    m <- length(attributes(model)$names)
    var_names <- summary(model[[1]])$coef[,"coef"]
    Ubar <- rowMeans(sapply(model, function(x) summary(x)$coef[,"Est."]))
    vv <- (sapply(model, function(x) summary(x)$coef[,"S.E."])^2)
    VarU_bw <- rowSums((vv - rowMeans(vv))^2)*((m+1)/(m-1))
    VarU_wh <- rowMeans((sapply(model, function(x) summary(x)$coef[,"S.E."]))^2)
    SE_Ubar <- sqrt(VarU_bw + VarU_wh)
    z <- abs(Ubar/SE_Ubar)
    p <- (1-pnorm(z))*2
    
    AIC <- mean(sapply(model, function(x) summary(x)$fitmeasures["aic"]))
    BIC <- mean(sapply(model, function(x) summary(x)$fitmeasures["bic"]))
    RMSEA <- mean(sapply(model, function(x) summary(x)$fitmeasures["rmsea"]))
    SRMR <- mean(sapply(model, function(x) summary(x)$fitmeasures["srmr"]))
    tot_obs <- attributes(summary(model[[1]]))$tot_obs
    com_obs <- attributes(summary(model[[1]]))$complete_obs
    
    l=return(createTexreg(coef.names=var_names, 
                          coef=Ubar, se=SE_Ubar, pvalues=p,
                          gof = c(AIC, BIC, RMSEA, SRMR, tot_obs, com_obs),
                          gof.names = c("AIC", "BIC", "RMSEA", "SRMR", "Total obs", "Complete Obs"), 
                          gof.decimal = c(rep(TRUE, 4), FALSE, FALSE)))
  }
  
  ##Para datos sin imputar
  if(class(model)=="dpm") { ##Para datos imputados
    mm <- summary(model)
    var_names <- mm$coef[,"coef"]
    Beta <- mm$coef[,"Est."]
    SE <- mm$coef[,"S.E."]
    z <- abs(Beta/SE)
    p <- (1-pnorm(z))*2
    
    AIC <- mm$fitmeasures["aic"]
    BIC <- mm$fitmeasures["bic"]
    RMSEA <- mm$fitmeasures["rmsea"]
    SRMR <- mm$fitmeasures["srmr"]
    tot_obs <- attributes(mm)$tot_obs
    com_obs <- attributes(mm)$complete_obs
    
    l=return(createTexreg(coef.names=var_names, 
                          coef=Beta, se=SE, pvalues=p,
                          gof = c(AIC, BIC, RMSEA, SRMR, tot_obs, com_obs),
                          gof.names = c("AIC", "BIC", "RMSEA", "SRMR", "Total obs", "Complete Obs"), 
                          gof.decimal = c(rep(TRUE, 4), FALSE, FALSE)))
  }
}
