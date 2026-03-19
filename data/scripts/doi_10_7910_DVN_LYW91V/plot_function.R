# arguments take model, a set of variables (default is all)
# a toggle for intercept (default is on), and a CI range for plotting options
# shift is a parameter for what degree of change in the RHS variable you want. takes either range or SD
# num_sd specifies number of SD shifts in RHS if desired. default is 1
# Robust allows for including robust standard error (HC1 (Stata default) but you can edit the function)
to_plot <- function(model = NULL, vars = NULL, intercept = TRUE, robust = FALSE, ci = 95, shift = "range", num_sd = 1){
  ## Collect Variables to Plot
  if(length(vars) == 0){
    vars <- names(coef(model)) 
    vars <- grep("(Intercept)", vars, value = T, invert = T)
  }
  ## Get Coefficients
  if(intercept == TRUE){
    vars <- c("(Intercept)", vars)
    m_coefs <- coef(model)[vars] 
  }
  if(intercept == FALSE){
    m_coefs <- coef(model)[vars] 
  }
  ## Get Standard error
  if(robust == FALSE){
    m_ses <- sqrt(diag(vcov(model)))[vars] 
  }
  if(robust == TRUE){
    m_ses <- sqrt(diag(sandwich::vcovHC(model, type = "HC1")))[vars] 
  }
  
  ## Get CI
  # check ci format
  if(ci > 1){
    ci <- ci/100
  }
  alpha <- 1 - ci
  # store critical value
  ci_z <- abs(qnorm(alpha/2))
  ## Get range for variables in data
  tmp_dat <- model.matrix(model)
  
  shift <- tolower(shift)
  if(shift == "range"){
    if(length(vars) == 1){
      dat_shift <- diff(range(tmp_dat[,vars])) 
    }
    if(length(vars) > 1){
      dat_shift <- diff(apply(tmp_dat[,vars], 2, range)) 
    }
    if(intercept == TRUE){
      dat_shift[, "(Intercept)"] <- 1
    } 
  }
  ## Get SD for variables if this is desired
  if(shift == "sd"){
    if(length(vars) == 1){
      dat_shift <- sd(tmp_dat[,vars])*num_sd
    }
    if(length(vars) > 1){
      dat_shift <- apply(tmp_dat[,vars], 2, sd)*num_sd
    }
    if(intercept == TRUE){
      dat_shift["(Intercept)"] <- 1
    }
  }
  
  # Combine 
  m_coef_shift <- m_coefs*dat_shift
  m_ses_shift <- m_ses*dat_shift
  m_lci <- m_coef_shift - ci_z*m_ses_shift
  m_uci <- m_coef_shift + ci_z*m_ses_shift
    
  tmp_df <- data.frame(var = vars, coef = as.numeric(m_coef_shift), lci = as.numeric(m_lci), uci = as.numeric(m_uci))
  return(tmp_df)
}