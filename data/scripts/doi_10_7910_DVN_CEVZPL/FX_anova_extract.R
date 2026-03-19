


# function to extact df, ch2, and p from anova() output
anova.extract <- function(anova.output){
  df  <- anova.output$Df
  ch2 <- anova.output$Chisq[[2]]
  p   <- anova.output$"Pr(>Chisq)"[[2]]
  
  anova.ex <- c(df[[1]],df[[2]],ch2,p)
  
  names(anova.ex) <- c("df1","df2","ch2","p")
  
  return(anova.ex)
}



