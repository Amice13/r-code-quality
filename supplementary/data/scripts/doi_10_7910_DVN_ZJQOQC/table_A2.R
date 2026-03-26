#remove(list=ls())
source("code/build.R")

#suppressMessages({
  
  
  contingency_table <- table(data$deserter,data$killed)
  
  # Add row and column marginals
  cross_tab_with_marginals <- addmargins(contingency_table)
  cross_tab_with_percents <-  round(cross_tab_with_marginals/cross_tab_with_marginals[,3]*100,1)
  
  print(cross_tab_with_marginals)
  print(cross_tab_with_percents)  
  
#})