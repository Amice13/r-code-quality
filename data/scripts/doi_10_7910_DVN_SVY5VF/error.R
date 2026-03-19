## Author: Kabir Khanna
## Updated: December 15, 2015

error <- function(df) {
  
  eth <- c("whi", "bla", "his", "asi", "oth")
  SR <- c("SR.WHI", "SR.BLA", "SR.HIS", "SR.ASI", "SR.OTH")
  
  ## Create dummy variable indicating whether classification is correct
  total <- error <- NA
  fn.fp <- rep(NA, 20)
  df$pred.corr <- NA
  df$pred.corr <- ifelse(df[paste("pred", "whi.class", sep = ".")] == df$SR.WHI & 
                           df[paste("pred", "bla.class", sep = ".")] == df$SR.BLA & 
                           df[paste("pred", "his.class", sep = ".")] == df$SR.HIS & 
                           df[paste("pred", "asi.class", sep = ".")] == df$SR.ASI & 
                           df[paste("pred", "oth.class", sep = ".")] == df$SR.OTH, 
                         1, 0)
  error <- table(df$pred.corr)["0"]
  total <- sum(table(df$pred.corr))
      
  for (k in 1:5) {
    ## False Neg
    fn.fp[4 * k - 3] <- nrow(df[!is.na(df[SR[k]]) & df[SR[k]] == 1 & 
                                  !is.na(df[paste("pred", eth[k], "class", sep = ".")]) & 
                                  df[paste("pred", eth[k], "class", sep = ".")] == 0, ])
    
    ## Total Pos
    fn.fp[4 * k - 2] <- nrow(df[!is.na(df[SR[k]]) & df[SR[k]] == 1, ])
    
    ## False Pos
    fn.fp[4 * k - 1] <- nrow(df[!is.na(df[SR[k]]) & df[SR[k]] == 0 &
                                  !is.na(df[paste("pred", eth[k], "class", sep = ".")]) & 
                                  df[paste("pred", eth[k], "class", sep = ".")] == 1, ])
    
    ##  Total Neg
    fn.fp[4 * k - 0] <- nrow(df[!is.na(df[SR[k]]) & df[SR[k]] == 0, ])
  }
  error.df <- as.data.frame(matrix(NA, nrow = 1, ncol = 22))
  error.df[1, ] <- c(total, error, fn.fp)
  names(error.df) <- c("total", "error", 
                       "fn.whi", "pos.whi", "fp.whi", "neg.whi",
                       "fn.bla", "pos.bla", "fp.bla", "neg.bla", 
                       "fn.his", "pos.his", "fp.his", "neg.his", 
                       "fn.asi", "pos.asi", "fp.asi", "neg.asi", 
                       "fn.oth", "pos.oth", "fp.oth", "neg.oth")
  return(error.df)
}
