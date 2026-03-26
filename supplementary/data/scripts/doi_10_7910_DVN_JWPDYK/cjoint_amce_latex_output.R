cjoint_amce_latex_output <- function(amce_obj, order_vec, round_decimale=4, pvalue=T) {
  
  n_list <- length(amce_obj)
  amce_list <- amce_obj
  res_list <- list(n_list)
  res_obs <- matrix(NA, 2, n_list)
  
  for(l in 1:n_list) {
    amce_obj <- amce_list[[l]]
    amce_est <- amce_obj$estimates[order(match(names(amce_obj$estimates),order_vec))]
    attrnames <- character()
    coef_se <- matrix(NA,0,2)
    for(i in 1:length(amce_est)) {
      for(j in 1:ncol(amce_est[[i]])) {
        attrnames <- c(attrnames, paste(names(amce_est)[i], colnames(amce_est[[i]])[j]))
        coef <- amce_est[[i]][1,j]
        se <- amce_est[[i]][2,j]
        coef_se <- rbind(coef_se, cbind(coef, se))
      }
    }
    
    if (pvalue) {
      zscr <- coef_se[,1] / coef_se[,2]
      pval <- 2*pnorm(-abs(zscr))
      sig_stars <- character(length(pval))
      for (i in 1:length(pval)) {
        if (pval[i] < .001) {
          sig_stars[i] <- "$^{***}$"
        } else if (pval[i] < .01) {
          sig_stars[i] <- "$^{**}$"
        } else if (pval[i] < .05) {
          sig_stars[i] <- "$^{*}$"
        } else {
          sig_stars[i] <- ""
        }
      }
    } else {
      sig_stars <- character(length(pval))
      sig_stars <- ""
    }
    coef_se <- round(coef_se, round_decimale)
    res_list[[l]] <- data.frame(coef_se, sig_stars)
    
    res_obs[1,l] <-  summary(amce_obj)$samplesize_estimates
    res_obs[2,l] <-  summary(amce_obj)$respondents
  }
  
  out_est <- character(length(attrnames))
  out_se <- character(length(attrnames))
  for(i in 1:length(attrnames)) {
    for (j in 1:n_list) {
      out_est[i] <- paste(out_est[i] , res_list[[j]][i,1],  res_list[[j]][i,3])
      if(j==1) {
        out_se[i] <- paste0(out_se[i], "& ", "(",res_list[[j]][i,2], ") ")
      } else {
        out_se[i] <- paste0(out_se[i] , "(",res_list[[j]][i,2], ") ")
      }  
      if(j<n_list){
        out_est[i] <- paste(out_est[i], "&")
        out_se[i] <- paste(out_se[i], "&")
      }
    }
  }
  
  output <- character()
  for(i in 1:length(attrnames)) {
    output <- c(output, paste(attrnames[i], "&", out_est[i], "\\\\ "))
    output <- c(output, paste0(out_se[i],  "\\\\ "))
  }
  
  if(n_list>1){
    out_samplesize <- paste0(res_obs[1,], collapse = " & ")
    out_respondents <- paste0(res_obs[2,], collapse = " & ")
  }
  output <- c(output, paste("Number of Observations", "&", out_samplesize, "\\\\ "))
  output <- c(output, paste("Number of Respondents", "&", out_respondents, "\\\\ "))
  
  cat(output,  sep = "\n")
}  