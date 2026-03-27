## Author: Kabir Khanna
## Updated: December 16, 2015
## Expectation-Maximization Algorithm for Pr(Race | Surname, Block, Party) without survey

EM2.Block <- function(df.blk, eth = c("whi", "bla", "his", "asi", "oth"), pid = c(0, 1, 2)) {
  vars.orig <- names(df.blk)
  
  l.diff.abs <- l.diff <- 1
  max.iter <- 20
  l <- rep(NA, max.iter)
  
  ## "Priors" for pi_px_eth
  for (p in pid) {
    for (k in 1:length(eth)) {
      df.blk[paste("pi_", p, "_", eth[k], sep = "")] <- 1 / length(eth)
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
    
    print("Add Census Age Categories")
    df.blk$agecat <- NA
    df.blk$agecat <- ifelse(df.blk$Voters_Age >= 18 & df.blk$Voters_Age <= 19, 5, df.blk$agecat)
    df.blk$agecat <- ifelse(df.blk$Voters_Age == 20, 6, df.blk$agecat)
    df.blk$agecat <- ifelse(df.blk$Voters_Age == 21, 7, df.blk$agecat)
    df.blk$agecat <- ifelse(df.blk$Voters_Age >= 22 & df.blk$Voters_Age <= 24, 8, df.blk$agecat)
    df.blk$agecat <- ifelse(df.blk$Voters_Age >= 25 & df.blk$Voters_Age <= 29, 9, df.blk$agecat)
    df.blk$agecat <- ifelse(df.blk$Voters_Age >= 30 & df.blk$Voters_Age <= 34, 10, df.blk$agecat)
    df.blk$agecat <- ifelse(df.blk$Voters_Age >= 35 & df.blk$Voters_Age <= 39, 11, df.blk$agecat)
    df.blk$agecat <- ifelse(df.blk$Voters_Age >= 40 & df.blk$Voters_Age <= 44, 12, df.blk$agecat)
    df.blk$agecat <- ifelse(df.blk$Voters_Age >= 45 & df.blk$Voters_Age <= 49, 13, df.blk$agecat)
    df.blk$agecat <- ifelse(df.blk$Voters_Age >= 50 & df.blk$Voters_Age <= 54, 14, df.blk$agecat)
    df.blk$agecat <- ifelse(df.blk$Voters_Age >= 55 & df.blk$Voters_Age <= 59, 15, df.blk$agecat)
    df.blk$agecat <- ifelse(df.blk$Voters_Age >= 60 & df.blk$Voters_Age <= 61, 16, df.blk$agecat)
    df.blk$agecat <- ifelse(df.blk$Voters_Age >= 62 & df.blk$Voters_Age <= 64, 17, df.blk$agecat)
    df.blk$agecat <- ifelse(df.blk$Voters_Age >= 65 & df.blk$Voters_Age <= 66, 18, df.blk$agecat)
    df.blk$agecat <- ifelse(df.blk$Voters_Age >= 67 & df.blk$Voters_Age <= 69, 19, df.blk$agecat)
    df.blk$agecat <- ifelse(df.blk$Voters_Age >= 70 & df.blk$Voters_Age <= 74, 20, df.blk$agecat)
    df.blk$agecat <- ifelse(df.blk$Voters_Age >= 75 & df.blk$Voters_Age <= 79, 21, df.blk$agecat)
    df.blk$agecat <- ifelse(df.blk$Voters_Age >= 80 & df.blk$Voters_Age <= 84, 22, df.blk$agecat)
    df.blk$agecat <- ifelse(df.blk$Voters_Age >= 85, 23, df.blk$agecat)
  
    print("Calculate MLE of psi_p_gx_eth")
    for (k in 1:length(eth)) {
      for (p in pid) {
        sex.temp <- names(table(df.blk[df.blk$PID == p, ]$Voters_Gender)[table(df.blk[df.blk$PID == p, ]$Voters_Gender) > 0])
        for (m in 1:length(sex.temp)) {
          age.temp <- names(table(df.blk[df.blk$PID == p & df.blk$Voters_Gender == sex.temp[m], ]$agecat)[table(df.blk[df.blk$PID == p & df.blk$Voters_Gender == sex.temp[m], ]$agecat) > 0])
          for (x in 1:length(age.temp)) {
            df.blk[!is.na(df.blk$Voters_Gender) & df.blk$Voters_Gender == sex.temp[m] & 
                     !is.na(df.blk$agecat == age.temp[x]) & df.blk$agecat == age.temp[x], 
                   paste("psi_", p, "_", eth[k], sep = "")] <- 
              sum(df.blk[!is.na(df.blk$Voters_Gender) & df.blk$Voters_Gender == sex.temp[m] & 
                     !is.na(df.blk$agecat == age.temp[x]) & df.blk$agecat == age.temp[x], 
                     paste("pi_", p, "_", eth[k], sep = "")] * 
                    df.blk[!is.na(df.blk$Voters_Gender) & df.blk$Voters_Gender == sex.temp[m] & 
                     !is.na(df.blk$agecat == age.temp[x]) & df.blk$agecat == age.temp[x], 
                     c("Ind", "Dem", "Rep")[p + 1]], na.rm = T) / 
              sum(df.blk[!is.na(df.blk$Voters_Gender) & df.blk$Voters_Gender == sex.temp[m] & 
                     !is.na(df.blk$agecat == age.temp[x]) & df.blk$agecat == age.temp[x], 
                     paste("pi_", p, "_", eth[k], sep = "")], na.rm = T)
          }
        }
      }
    }
    
    ## Recalculate pi for each combination of race and party ID
    for (p in pid) {
      for (k in 1:length(eth)) {
        df.blk[, paste(p, "_", eth[k], sep = "")] <- 
          df.blk[, paste("psi_", p, "_", eth[k], sep = "")] * 
          df.blk[, paste("r_blk_age_sex_", eth[k], sep = "")] * 
          df.blk[, paste("p_", eth[k], sep = "")]
      }
      
      df.blk[paste(p, "_tot", sep = "")] <- 0
      for (k in 1:length(eth)) {
        df.blk[paste(p, "_tot", sep = "")] <- rowSums(cbind(df.blk[paste(p, "_tot", sep = "")], df.blk[paste(p, "_", eth[k], sep = "")]), na.rm = T)
      }
    }
    
    for (p in pid) {
      for (k in 1:length(eth)) {
        df.blk[paste("pi_", p, "_", eth[k], sep = "")] <- df.blk[paste(p, "_", eth[k], sep = "")] / df.blk[paste(p, "_tot", sep = "")]
      }
    }
    
    print("Assign Pi")
    for (p in pid) {
      for (k in 1:length(eth)) {
        df.blk[df.blk$PID == p, paste("pred", eth[k], sep = ".")] <- df.blk[df.blk$PID == p, paste("pi_", p, "_", eth[k], sep = "")]
      }
    }
    
    print("Calculate Observed Data Log-Likelihood")
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
  
  keep <- c(vars.orig, paste("pred", eth, sep = "."))
  return(df.blk[keep])
}
