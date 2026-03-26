######################################################################
# Project:  Replication of Rauh/Bes/Schoonvelde 2019 EJPR
#
# Task:     Cross-validate complexity measure with releated indicators
#           Appendix A of the main text
#
# Author:   Christian Rauh
# Date:     22.02.2019
#####################################################################

# R version 3.4.4 (2018-03-15)

# Packages ####
library(tidyverse) # # 1.2.1
library(GGally) 
library(Hmisc)
library(xtable)


# Set YOUR working directory here ####
# Root folder of replication archive, relative paths used from hereon
setwd("Your/Working/Directory") 


# The data ####
# If you are interested in how the variables are established, please contact @ChRauh for the respective scripts (computationally intensive)

dat <- readRDS("./data/ComplexityMeasures.Rds")


# Correlation table ####
# Function from: http://www.sthda.com/english/wiki/elegant-correlation-table-using-xtable-r-package

corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
}


dat2 <- dat[, c("Complexity", "Entropy", "Concreteness", "Familiarity")]


tab <- corstars(dat2, result = "html")
write(tab, file = "./tables/TabA1_ComplexityValidation.html")

