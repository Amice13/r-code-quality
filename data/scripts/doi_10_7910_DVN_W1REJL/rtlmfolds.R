# RADIO RTLM: CREATE FOLDS FOR CROSS-VALIDATION

# Gordon Danning
# 2018-03-02

# Load required packages

library(foreign)
library(readr)

#import data set

Rwanda <- read_csv("Rwanda.csv")



# create folds

set.seed(1)

for (i in 1:100) { 
  #randomize dataframe
  Rwanda_random <- Rwanda[sample(1:nrow(Rwanda)), ]  
  
  #split into 4 sets
  
  testa <- as.data.frame(Rwanda_random[1:266, ])
  testb <- as.data.frame(Rwanda_random[267:532, ]) 
  testc<- as.data.frame(Rwanda_random[533:798, ]) 
  testd<- as.data.frame(Rwanda_random[799:1065, ])
  
  traina <- as.data.frame(rbind(testb, testc, testd)) 
  trainb <- as.data.frame(rbind(testa, testc, testd)) 
  trainc <- as.data.frame(rbind(testa, testb, testd)) 
  traind <- as.data.frame(rbind(testa, testb, testc)) 
  
  assign(paste("Rwanda_random", i, sep = ""), Rwanda_random)
  assign(paste("testa", i, sep = ""), testa)
  assign(paste("testb", i, sep = ""), testb)
  assign(paste("testc", i, sep = ""), testc)
  assign(paste("testd", i, sep = ""), testd)
  assign(paste("traina", i, sep = ""), traina)
  assign(paste("trainb", i, sep = ""), trainb)
  assign(paste("trainc", i, sep = ""), trainc)
  assign(paste("traind", i, sep = ""), traind)
  
}


#write to Stata

for (i in ls(pattern="train")){
  write.dta(get(i), paste0(i,".dta"))
}

for (i in ls(pattern="test")){
  write.dta(get(i), paste0(i,".dta"))
}

