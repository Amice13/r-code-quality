## Author: Kabir Khanna
## Updated: December 9, 2015

classify <- function(df) {
  
  eth <- c("whi", "bla", "his", "asi", "oth")
  
  # Create dummy  for each race, where 1 indicates race with highest probability and all other dummies are 0
  for (k in 1:5) {
    df[paste("pred", eth[k], "class", sep = ".")] <- ifelse(is.na(df[paste("pred", eth[k], sep = ".")]), NA, 0)
    df[, paste("pred", eth[k], "class", sep = ".")] <- 
      ifelse(df[paste("pred", eth[k], sep = ".")] > df[paste("pred", eth[-k][1], sep = ".")] & 
               df[paste("pred", eth[k], sep = ".")] > df[paste("pred", eth[-k][2], sep = ".")] & 
               df[paste("pred", eth[k], sep = ".")] > df[paste("pred", eth[-k][3], sep = ".")] & 
               df[paste("pred", eth[k], sep = ".")] > df[paste("pred", eth[-k][4], sep = ".")], 
             1, df[, paste("pred", eth[k], "class", sep = ".")])
  }
  return(df)
}
