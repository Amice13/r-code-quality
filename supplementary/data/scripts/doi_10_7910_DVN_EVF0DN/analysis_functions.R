

# add custom functions to extract estimates (tidy) and goodness-of-fit (glance) information
tidy.fect <- function(x, ...) {
  ret <- data.frame(
    term      = c(x$D, x$X),
    estimate  = c(x$est.avg[1], x$beta),
    std.error = c(x$est.avg[2], x$est.beta[, 2]),
    conf.low  = c(x$est.avg[3], x$est.beta[, 3]),
    conf.high = c(x$est.avg[4], x$est.beta[, 4]),
    p.value = c(x$est.avg[5], x$est.beta[, 5])
  )
  ret
}
glance.fect <- function(x, ...) {
  ret <- data.frame(
    r.squared = NA,
    adj.r.squared = NA,
    within.r.squared = NA,
    pseudo.r.squared = NA,
    sigma = NA,
    nobs = NA,
    AIC = NA,
    BIC = NA,
    logLik = NA
  )
  ret
}

get_fect_hits <- function(fect_model, input_data, out_path = NULL) {
  
  # find id variable
  id_unit <- fect_model$index[1]
  
  # Extract the units that contribute to the estimates
  dat_treat <-  
    input_data[pull(input_data[, id_unit]) %in% fect_model$id[fect_model$tr], 
               c(id_unit, "groupname", "id_segment")] %>% 
    distinct()
  
  # Extract period-wise effects
  complete.index.eff <- which(!is.na(fect_model$eff))
  complete.index.time <- which(!is.na(fect_model$T.on))
  complete.index <- intersect(complete.index.eff, complete.index.time)
  eff.use <- fect_model$eff[complete.index]
  time.use <- fect_model$T.on[complete.index]
  id.mat <- rep(colnames(fect_model$eff), each = dim(fect_model$eff)[1])
  id.use <- id.mat[complete.index]
  data.toplot <- cbind.data.frame(time = time.use, id = id.use, 
                                  eff = eff.use)
  colnames(data.toplot)[2] <- id_unit
  
  # Join ethnic dyad names
  dat_out <- data.toplot %>% 
    left_join(dat_treat)
  
  # Summarize effects
  dat_out <- dat_out %>% 
    mutate(period = ifelse(time < 0, "pre", "post")) %>% 
    group_by(get(id_unit), groupname, id_segment, period) %>% 
    summarise(eff_min = min(eff, na.rm = T),
              eff_max = max(eff, na.rm = T),
              eff_mean = mean(eff, na.rm = T))
  
  if (is.null(out_path)) {
    
    return(dat_out)
    
  } else {
    
    write_csv(dat_out, file = out_path)
    
  }
  
}

get_did_hits <- function(did_model, input_data, out_path = NULL) {
  
  # find id variable
  id_unit <- did_model$index[1]
  
  # Extract the units that contribute to the estimates
  dat_treat <-  did_model$DIDparams$data %>% 
    distinct(id_segment, rail_cohort_year)
  
  # Extract period-wise effects
  cband_lower <- did_model$att - did_model$c * did_model$se
  cband_upper <- did_model$att + did_model$c * did_model$se
  sig <- (cband_upper < 0) | (cband_lower > 0)
  sig[is.na(sig)] <- FALSE
  sig_text <- ifelse(sig, "*", "")
  data.toplot <- bind_cols(did_model$group, did_model$t, did_model$att, 
                                  did_model$se, cband_lower, cband_upper)
  colnames(data.toplot) <- c("rail_cohort_year", "year", "eff", "sd", 
                             "lower", "upper")
  
  dat_out <- data.toplot %>% 
    filter(!is.na(sd)) %>% 
    mutate(time = year - rail_cohort_year) %>% 
    left_join(
      dat_treat %>% 
        group_by(rail_cohort_year) %>% 
        summarise(id_segment = paste0(id_segment, collapse = "|"))
    )
  
  
  # Summarize effects
  dat_out <- dat_out %>% 
    mutate(period = ifelse(time < 0, "pre", "post")) %>% 
    group_by(id_segment, period) %>% 
    summarise(eff_min = min(eff, na.rm = T),
              eff_max = max(eff, na.rm = T),
              eff_mean = mean(eff, na.rm = T)) %>% 
    separate_rows(id_segment, sep = "\\|")
  
  if (is.null(out_path)) {
    
    return(dat_out)
    
  } else {
    
    write_csv(dat_out, file = out_path)
    
  }
  
}

# Standardize mechanism variables
standardize <- function(x) {
  
  z <- x - mean(x, na.rm = T)
  z <- z / sd(x, na.rm = T)
  
  return(z)
}