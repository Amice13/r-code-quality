## Replication code for 'Locked Out of College: ...'
## Jacob R. Brown and Hanno Hilbig (hhilbig@g.harvard.edu)
## Nov 18, 2020
##
## This file: Table B1

## Set WD

rm(list = ls())
setwd("")

# Here, we analyze the pre-test survey data, which asked survey respondents to write down which race/ethnicity
# they thought most likely applied to the names employed in the experiment.
# The output is a vector containing the mean "correct" classification and 95% confidence interval
# Correct in this case is classifying Tyrone Booker, Darnell Banks, and Jamal Gaines as Black or African-American
# and classifying Kevin Schmidt, Bob Krueger, and Todd Novak as White or Caucasian. We also classify European 
# descriptors such as german, swedish, European, or Russian as "White"


# Load Data
data = read_csv("Data_NamesPretest.csv")

# Set column names
colnames(data) = data[1,]

# Drop first 10 rows, which are NA
data = data[10:nrow(data),]

# Cycle through columns and rows of data, recoding answers as "black" or "white"
for(i in 1:nrow(data)){
  for(j in 1:ncol(data)){
    data[i,j]= as.character(data[i,j])
    data[i, j] = tolower(data[i, j])
    if(grepl("black",data[i,j]) | grepl("blck",data[i,j]) | grepl("African",data[i,j]) | grepl("african",data[i,j])){
      data[i,j] = "black"
    }else if(grepl("white",data[i,j]) | grepl("german",data[i,j]) | grepl("swedish",data[i,j]) | grepl("european",data[i,j]) | grepl("russian",data[i,j]) | grepl("norwegian",data[i,j]) | grepl("caucasian",data[i,j])){
      data[i,j] = "white"
    }
  }
}

# do this again, this time looking for specific typos not picked up by the first loop.
for(i in 1:nrow(data)){
  for(j in 1:ncol(data)){
    data[i,j]= as.character(data[i,j])
    data[i, j] = tolower(data[i, j])
    if(grepl("black",data[i,j]) | grepl("africaan",data[i,j]) | grepl("ethnic",data[i,j])){
      data[i,j] = "black"
    }else if(grepl("whitw",data[i,j]) | grepl("scandinavian",data[i,j]) | grepl("polish",data[i,j]) | grepl("jewish",data[i,j]) | grepl("russian",data[i,j]) | grepl("norwegian",data[i,j]) | grepl("caucasion",data[i,j])){
      data[i,j] = "white"
    }
  }
}

for(i in 1:nrow(data)) {
  for(j in 1:ncol(data)){
    data[i,j]= as.character(data[i,j])
    data[i, j] = tolower(data[i, j])
    if(grepl("us",data[i,j]) | grepl("none",data[i,j]) | grepl("smart",data[i,j]) | grepl("finance",data[i,j]) | grepl("good",data[i,j]) | grepl("2",data[i,j])){
      data[i,j] = NA
      
    }
  }
  
}

# construct confidence intervals and mean vector, print as we go
for(col in colnames(data)){
  if(col %in% colnames(data[1:3])){
    a <- mean(data[,col]=="black", na.rm=T)
    s <- sd(data[,col]=="black", na.rm=T)
    n <- nrow(data[,col])
    error <- qt(0.975,df=n-1)*s/sqrt(n)
    left <- a-error
    right <- a+error
    print(paste(col, "mean: ", a))
    print(paste(col, "95% CI:", left, right))
    
    
  } else{
    a <- mean(data[,col]=="white", na.rm=T)
    s <- sd(data[,col]=="white", na.rm=T)
    n <- nrow(data[,col])
    error <- qt(0.975,df=n-1)*s/sqrt(n)
    left <- a-error
    right <- a+error
    print(paste(col, "mean: ", a))
    print(paste(col, "95% CI:", left, right))
    
  }
}
