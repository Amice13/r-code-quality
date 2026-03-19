## Author: Kabir Khanna
## Updated: December 16, 2015
## Expectation-Maximization Algorithm for Pr(Race | Surname, Precinct, Party) without survey

EM2.Precinct <- function(df.prc, eth = c("whi", "bla", "his", "asi", "oth"), pid = c(0, 1, 2)) {
  vars.orig <- names(df.prc)
  
  l.diff.abs <- l.diff <- 1
  max.iter <- 20
  l <- rep(NA, max.iter)
  
  ## "Priors" for pi_px_eth
  for (p in pid) {
    for (k in 1:length(eth)) {
      df.prc[paste("pi_", p, "_", eth[k], sep = "")] <- 1 / length(eth)
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
    
    print("Add Census Age Categories")
    df.prc$agecat <- NA
    df.prc$agecat <- ifelse(df.prc$Voters_Age >= 18 & df.prc$Voters_Age <= 19, 5, df.prc$agecat)
    df.prc$agecat <- ifelse(df.prc$Voters_Age == 20, 6, df.prc$agecat)
    df.prc$agecat <- ifelse(df.prc$Voters_Age == 21, 7, df.prc$agecat)
    df.prc$agecat <- ifelse(df.prc$Voters_Age >= 22 & df.prc$Voters_Age <= 24, 8, df.prc$agecat)
    df.prc$agecat <- ifelse(df.prc$Voters_Age >= 25 & df.prc$Voters_Age <= 29, 9, df.prc$agecat)
    df.prc$agecat <- ifelse(df.prc$Voters_Age >= 30 & df.prc$Voters_Age <= 34, 10, df.prc$agecat)
    df.prc$agecat <- ifelse(df.prc$Voters_Age >= 35 & df.prc$Voters_Age <= 39, 11, df.prc$agecat)
    df.prc$agecat <- ifelse(df.prc$Voters_Age >= 40 & df.prc$Voters_Age <= 44, 12, df.prc$agecat)
    df.prc$agecat <- ifelse(df.prc$Voters_Age >= 45 & df.prc$Voters_Age <= 49, 13, df.prc$agecat)
    df.prc$agecat <- ifelse(df.prc$Voters_Age >= 50 & df.prc$Voters_Age <= 54, 14, df.prc$agecat)
    df.prc$agecat <- ifelse(df.prc$Voters_Age >= 55 & df.prc$Voters_Age <= 59, 15, df.prc$agecat)
    df.prc$agecat <- ifelse(df.prc$Voters_Age >= 60 & df.prc$Voters_Age <= 61, 16, df.prc$agecat)
    df.prc$agecat <- ifelse(df.prc$Voters_Age >= 62 & df.prc$Voters_Age <= 64, 17, df.prc$agecat)
    df.prc$agecat <- ifelse(df.prc$Voters_Age >= 65 & df.prc$Voters_Age <= 66, 18, df.prc$agecat)
    df.prc$agecat <- ifelse(df.prc$Voters_Age >= 67 & df.prc$Voters_Age <= 69, 19, df.prc$agecat)
    df.prc$agecat <- ifelse(df.prc$Voters_Age >= 70 & df.prc$Voters_Age <= 74, 20, df.prc$agecat)
    df.prc$agecat <- ifelse(df.prc$Voters_Age >= 75 & df.prc$Voters_Age <= 79, 21, df.prc$agecat)
    df.prc$agecat <- ifelse(df.prc$Voters_Age >= 80 & df.prc$Voters_Age <= 84, 22, df.prc$agecat)
    df.prc$agecat <- ifelse(df.prc$Voters_Age >= 85, 23, df.prc$agecat)
  
    print("Calculate MLE of psi_p_gx_eth")
    for (k in 1:length(eth)) {
      for (p in pid) {
        sex.temp <- names(table(df.prc[df.prc$PID == p, ]$Voters_Gender)[table(df.prc[df.prc$PID == p, ]$Voters_Gender) > 0])
        for (m in 1:length(sex.temp)) {
          age.temp <- names(table(df.prc[df.prc$PID == p & df.prc$Voters_Gender == sex.temp[m], ]$agecat)[table(df.prc[df.prc$PID == p & df.prc$Voters_Gender == sex.temp[m], ]$agecat) > 0])
          for (x in 1:length(age.temp)) {
            df.prc[!is.na(df.prc$Voters_Gender) & df.prc$Voters_Gender == sex.temp[m] & 
                     !is.na(df.prc$agecat == age.temp[x]) & df.prc$agecat == age.temp[x], 
                   paste("psi_", p, "_", eth[k], sep = "")] <- 
              sum(df.prc[!is.na(df.prc$Voters_Gender) & df.prc$Voters_Gender == sex.temp[m] & 
                     !is.na(df.prc$agecat == age.temp[x]) & df.prc$agecat == age.temp[x], 
                     paste("pi_", p, "_", eth[k], sep = "")] * 
                    df.prc[!is.na(df.prc$Voters_Gender) & df.prc$Voters_Gender == sex.temp[m] & 
                     !is.na(df.prc$agecat == age.temp[x]) & df.prc$agecat == age.temp[x], 
                     c("Ind", "Dem", "Rep")[p + 1]], na.rm = T) / 
              sum(df.prc[!is.na(df.prc$Voters_Gender) & df.prc$Voters_Gender == sex.temp[m] & 
                     !is.na(df.prc$agecat == age.temp[x]) & df.prc$agecat == age.temp[x], 
                     paste("pi_", p, "_", eth[k], sep = "")], na.rm = T)
          }
        }
      }
    }
    
    ## Recalculate pi for each combination of race and party ID
    for (p in pid) {
      for (k in 1:length(eth)) {
        df.prc[, paste(p, "_", eth[k], sep = "")] <- 
          df.prc[, paste("psi_", p, "_", eth[k], sep = "")] * 
          df.prc[, paste("r_prc_age_sex_", eth[k], sep = "")] * 
          df.prc[, paste("p_", eth[k], sep = "")]
      }
      
      df.prc[paste(p, "_tot", sep = "")] <- 0
      for (k in 1:length(eth)) {
        df.prc[paste(p, "_tot", sep = "")] <- rowSums(cbind(df.prc[paste(p, "_tot", sep = "")], df.prc[paste(p, "_", eth[k], sep = "")]), na.rm = T)
      }
    }
    
    for (p in pid) {
      for (k in 1:length(eth)) {
        df.prc[paste("pi_", p, "_", eth[k], sep = "")] <- df.prc[paste(p, "_", eth[k], sep = "")] / df.prc[paste(p, "_tot", sep = "")]
      }
    }
    
    print("Assign Pi")
    for (p in pid) {
      for (k in 1:length(eth)) {
        df.prc[df.prc$PID == p, paste("pred", eth[k], sep = ".")] <- df.prc[df.prc$PID == p, paste("pi_", p, "_", eth[k], sep = "")]
      }
    }
    
    print("Calculate Observed Data Log-Likelihood")
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
  
  keep <- c(vars.orig, paste("pred", eth, sep = "."))
  return(df.prc[keep])
}
