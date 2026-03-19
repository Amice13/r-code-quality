## Author: Kabir Khanna
## Updated: December 16, 2015
## Expectation-Maximization Algorithm for Pr(Race | Surname, Precinct, Party) without survey

EM1.Precinct <- function(df.prc, eth = c("whi", "bla", "his", "asi", "oth"), pid = c(0, 1, 2)) {
  keep.orig <- names(df.prc)

  l.diff.abs <- l.diff <- 1
  max.iter <- 20
  l <- rep(NA, max.iter)
  
  ## "Priors" for pi_px_eth
  for (p in pid) {
    for (i in 1:length(eth)) {
      df.prc[paste("pi_", p, "_", eth[i], sep = "")] <- 1 / length(eth)
    }
  }
  
  ## Start iterating
  n <- 0
  while (l.diff.abs > .0001 & n <= max.iter) {
    n <- n + 1
    print(paste("EM Iteration:", n))
    
    df.prc$Ind <- ifelse(df.prc$PID == 0, 1, 0)
    df.prc$Dem <- ifelse(df.prc$PID == 1, 1, 0)
    df.prc$Rep <- ifelse(df.prc$PID == 2, 1, 0)
    
    print("Calculate MLE of psi_p_gx_eth")
    for (i in 1:length(eth)) {
      for (p in pid) {
        df.prc[, paste("psi_", p, "_", eth[i], sep = "")] <- 
          sum(df.prc[, paste("pi_", p, "_", eth[i], sep = "")] * df.prc[, c("Ind", "Dem", "Rep")[p + 1]], na.rm = T) / 
          sum(df.prc[, paste("pi_", p, "_", eth[i], sep = "")], na.rm = T)
      }
    }
    
    ## Recalculate pi for each combination of race and party ID
    for (p in pid) {
      for (k in 1:length(eth)) {
        df.prc[, paste(p, "_", eth[k], sep = "")] <- 
          df.prc[, paste("psi_", p, "_", eth[k], sep = "")] * 
          df.prc[, paste("r_prc_", eth[k], sep = "")] * 
          df.prc[, paste("p_", eth[k], sep = "")]
      }
      
      df.prc[paste(p, "_tot", sep = "")] <- 0
      for (i in 1:length(eth)) {
        df.prc[paste(p, "_tot", sep = "")] <- rowSums(cbind(df.prc[paste(p, "_tot", sep = "")], df.prc[paste(p, "_", eth[i], sep = "")]), na.rm = T)
      }
    }
    
    for (p in pid) {
      for (k in 1:length(eth)) {
        df.prc[paste("pi_", p, "_", eth[k], sep = "")] <- df.prc[paste(p, "_", eth[k], sep = "")] / df.prc[paste(p, "_tot", sep = "")]
      }
    }
    
    print("Assign Pi")
    ## Assign final pi to each obs based on observed p
    for (p in pid) {
      for (k in 1:length(eth)) {
        df.prc[df.prc$PID == p, paste("pred", eth[k], sep = ".")] <- df.prc[df.prc$PID == p, paste("pi_", p, "_", eth[k], sep = "")]
      }
    }
    
    print("Calculate Observed Data Log-Likelihood")
    ## Log-Likelihood
    for (p in pid) {
      for (k in 1:length(eth)) {
        df.prc[df.prc$PID == p, paste("l_", eth[k], sep = "")] <- 
          df.prc[df.prc$PID == p, paste("pi_", p, "_", eth[k], sep = "")] * 
          df.prc[df.prc$PID == p, paste("psi_", p, "_", eth[k], sep = "")] * 
          df.prc[df.prc$PID == p, paste("p_", eth[k], sep = "")]
      }
    }
    
    df.prc[, "l"] <- 0
    for (k in 1:length(eth)) {
      df.prc[, "l"] <- rowSums(cbind(df.prc[, "l"], df.prc[paste("l_", eth[k], sep = "")]), na.rm = T)
    }
    df.prc$l <- ifelse(df.prc$l == 0, NA, df.prc$l)
    df.prc[, "ll"] <- log(df.prc[, "l"])
    
    l[n] <- sum(df.prc[is.na(df.prc$ll) == F, "ll"])
    l.diff <- ifelse(n > 1, l[n] - l[n-1], 1)
    l.diff <- ifelse(is.na(l.diff), 0, l.diff)
    l.diff.abs <- abs(l.diff)
    
    print(l[n])
    if (n > 1) {print(paste("Change of", round(l.diff, 5)))}
    
  }
  
  keep <- c(keep.orig, paste("pred", eth, sep = "."))
  return(df.prc[keep])
}
