## Author: Kabir Khanna
## Updated: December 16, 2015
## Expectation-Maximization Algorithm for Pr(Race | Surname, Block, Party) without survey

EM1.Block <- function(df.blk, eth = c("whi", "bla", "his", "asi", "oth"), pid = c(0, 1, 2)) {
  keep.orig <- names(df.blk)

  l.diff.abs <- l.diff <- 1
  max.iter <- 20
  l <- rep(NA, max.iter)
  
  ## "Priors" for pi_px_eth
  for (p in pid) {
    for (i in 1:length(eth)) {
      df.blk[paste("pi_", p, "_", eth[i], sep = "")] <- 1 / length(eth)
    }
  }
  
  ## Start iterating
  n <- 0
  while (l.diff.abs > .0001 & n <= max.iter) {
    n <- n + 1
    print(paste("EM Iteration:", n))
    
    df.blk$Ind <- ifelse(df.blk$PID == 0, 1, 0)
    df.blk$Dem <- ifelse(df.blk$PID == 1, 1, 0)
    df.blk$Rep <- ifelse(df.blk$PID == 2, 1, 0)
    
    print("Calculate MLE of psi_p_gx_eth")
    for (i in 1:length(eth)) {
      for (p in pid) {
        df.blk[, paste("psi_", p, "_", eth[i], sep = "")] <- 
          sum(df.blk[, paste("pi_", p, "_", eth[i], sep = "")] * df.blk[, c("Ind", "Dem", "Rep")[p + 1]], na.rm = T) / 
          sum(df.blk[, paste("pi_", p, "_", eth[i], sep = "")], na.rm = T)
      }
    }
    
    ## Recalculate pi for each combination of race and party ID
    for (p in pid) {
      for (k in 1:length(eth)) {
        df.blk[, paste(p, "_", eth[k], sep = "")] <- 
          df.blk[, paste("psi_", p, "_", eth[k], sep = "")] * 
          df.blk[, paste("r_blk_", eth[k], sep = "")] * 
          df.blk[, paste("p_", eth[k], sep = "")]
      }
      
      df.blk[paste(p, "_tot", sep = "")] <- 0
      for (i in 1:length(eth)) {
        df.blk[paste(p, "_tot", sep = "")] <- rowSums(cbind(df.blk[paste(p, "_tot", sep = "")], df.blk[paste(p, "_", eth[i], sep = "")]), na.rm = T)
      }
    }
    
    for (p in pid) {
      for (k in 1:length(eth)) {
        df.blk[paste("pi_", p, "_", eth[k], sep = "")] <- df.blk[paste(p, "_", eth[k], sep = "")] / df.blk[paste(p, "_tot", sep = "")]
      }
    }
    
    print("Assign Pi")
    ## Assign final pi to each obs based on observed p
    for (p in pid) {
      for (k in 1:length(eth)) {
        df.blk[df.blk$PID == p, paste("pred", eth[k], sep = ".")] <- df.blk[df.blk$PID == p, paste("pi_", p, "_", eth[k], sep = "")]
      }
    }
    
    print("Calculate Observed Data Log-Likelihood")
    ## Log-Likelihood
    for (p in pid) {
      for (k in 1:length(eth)) {
        df.blk[df.blk$PID == p, paste("l_", eth[k], sep = "")] <- 
          df.blk[df.blk$PID == p, paste("pi_", p, "_", eth[k], sep = "")] * 
          df.blk[df.blk$PID == p, paste("psi_", p, "_", eth[k], sep = "")] * 
          df.blk[df.blk$PID == p, paste("p_", eth[k], sep = "")]
      }
    }
    
    df.blk[, "l"] <- 0
    for (k in 1:length(eth)) {
      df.blk[, "l"] <- rowSums(cbind(df.blk[, "l"], df.blk[paste("l_", eth[k], sep = "")]), na.rm = T)
    }
    df.blk$l <- ifelse(df.blk$l == 0, NA, df.blk$l)
    df.blk[, "ll"] <- log(df.blk[, "l"])
    
    l[n] <- sum(df.blk[is.na(df.blk$ll) == F, "ll"])
    l.diff <- ifelse(n > 1, l[n] - l[n-1], 1)
    l.diff <- ifelse(is.na(l.diff), 0, l.diff)
    l.diff.abs <- abs(l.diff)
    
    print(l[n])
    if (n > 1) {print(paste("Change of", round(l.diff, 5)))}
    
  }
  
  keep <- c(keep.orig, paste("pred", eth, sep = "."))
  return(df.blk[keep])
}
