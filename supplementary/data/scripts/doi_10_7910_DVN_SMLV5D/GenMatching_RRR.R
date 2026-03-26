
# ------------------------------------------------------------------------------------------------------- #
#         
#		  AUTHOR:           RACHEL BRULE
#		  DATE:             13 September, 2018
#         PROJECT:          "Reform, Representation & Resistance"
#         DESCRIPTION:      This file creates a genetically matched data set
#                           using REDS 2006/9 data set that has been merged with sex ratio, total 
#                           population, and SC/ST proportion data from the provided 1991 Census datasets.
# ------------------------------------------------------------------------------------------------------- #

# LIBRARIES (if any of these is missing --> install.packages() command)

#install.packages("lfe")
#install.packages("lmtest")
#install.packages("sandwich")
#install.packages("multiwayvcov")
#install.packages("foreign")
#install.packages("Matching", dependencies = TRUE)
#install.packages("rgenoud")
#install.packages("parallel")
#install.packages("snow")
library("lfe")
library("lmtest")
library("sandwich")
library("multiwayvcov")
library("foreign")
library("Matching")
library("rgenoud")
library("parallel")
library("snow")


### Set working directory here: it should be the folder location where replication package has been saved (below is a template for uploading to NYUAD HPC resources)
#setwd("/scratch/netid/data_analysis/replicate")

setwd("")

## Set the seed used for random number generation to ensure replication
set.seed(20180814)

rm(list = ls())

### Input full sample (below is a template for running with NYUAD HPC resources)
#raw.subset <- read.csv("/scratch/netid/data_analysis/GenMatching_deidentified.csv")

raw.subset <- read.csv("GenMatching_deidentified.csv")
 

###################################################################
# SUBSETTING FOR MATCHING
###################################################################

matchdta <- raw.subset[complete.cases(raw.subset$fem_ratio, raw.subset$total_pop, raw.subset$Tiland_any, raw.subset$vwreserv, 
                                      raw.subset$prop_SC91, raw.subset$prop_ST91),]

# Set the number of clusters where matching should be run
# By default this is the number of available cores on the computer - 1
cores <- 28

cl <- makeCluster(cores)


attach(matchdta)


treatno <- sum(with(matchdta, vwreserv == 1))
controlno <- sum(with(matchdta, vwreserv == 0))

# We create a matrix (list of confounders) that we wish to match on.
X <- cbind(fem_ratio, total_pop, prop_SC91, prop_ST91, stateid)

#BalanceMatrix is what we wish to achieve balance on (it should include all the variables in X, but
# it may also have other interaction or quadratic variables in it)
BalanceMatrix <- cbind(fem_ratio, total_pop, prop_SC91, prop_ST91, stateid)

# We run GenMatch() function
#   Syntax for GenMatch:
#   GenMatch(Tr = TreatmentVariable, X = VariablesForMatching, BalanceMatrix = VariablesToBalance, [options])
genetic1 <- GenMatch(Tr = vwreserv, X=X , BalanceMatrix = BalanceMatrix, if (treatno > controlno) {estimand = "ATC"} else {estimand = "ATT"}, pop.size = 500, replace = FALSE, ties = FALSE, cluster = cl)

genetic1$matches <- genetic1$matches[, 1:3]

# -------------------------------------------------------------------------------------------------------------
#   In order to determine if we achieved balance, we have to transform Genetic Matching output data into
#   propensity score matched data:

#   Weight.matrix - denotes the weights the matching algorithm uses when weighting each of the covariates in X
#   It is usually provided by the GenMatch function.

m.out <- Match(Y=Tiland_any, Tr= vwreserv, X=X, Weight.matrix = genetic1, replace = FALSE)

#   Determining if balance has been achieved:
#   Syntax for MatchBalance:
# MatchBalance(treatmentVariable ~ [list of variables we wish to achieve balance on], match.out = [output from Match], nboots = (at least 500))

balance2 <- MatchBalance(vwreserv ~ fem_ratio+total_pop+prop_SC91+prop_ST91, match.out = m.out, nboots = 1000)

# Bind data together

treatedDTA <- matchdta[m.out$index.treated,]
controlDTA <- matchdta[m.out$index.control,]
treatedDTA$index.match <- c(1:dim(treatedDTA)[1])
controlDTA$index.match <- c(1:dim(controlDTA)[1])

matched.final <- rbind(treatedDTA, controlDTA)


# change variable names to make merging with STATA easier
#names(matched.final)[names(matched.final) == 'tehsil'] <- 'tehsil_label'
#names(matched.final)[names(matched.final) == 'district'] <- 'district_label'
#names(matched.final)[names(matched.final) == 'village'] <- 'village_label'

save.image(file = "Matching_deidentified.RData")

load("Matching_deidentified.RData")

# Write output to be used in STATA

write.dta(matched.final, file = paste("MatchedData_deidentified", ".dta", sep=""))

# Set working directory for figures
#setwd("")

###############################################
##BALANCE FIGURES
###############################################
  
  detach(matchdta)
  
  ##Balance on female ratio between reserved and unreserved constituencies 
  
  t.test(raw.subset$fem_ratio ~ raw.subset$vwreserv)
  t.test(matched.final$fem_ratio ~ matched.final$vwreserv)
  
  
  ###Figure showing balance on Female Ratio
  
  # Set working directory for saving output
  #setwd("")
  
  pdf(c(paste("FigureA8.pdf", sep = "")), width=10, height=5)
  par(mai = c(1, 0.9, 0.3, 0.6))
  par(mfrow=c(1,2))
  plot(density(raw.subset$fem_ratio[raw.subset$vwreserv==1 & complete.cases(raw.subset$fem_ratio)], na.rm =T),
       col="#FF1493", xlim = c(0.2,1), ylim = c(0, 20), main="Before genetic matching", xlab="Percentage of women in sub-district", ylab = "Density",
       las=1, cex.axis=0.75)
  lines(density(raw.subset$fem_ratio[raw.subset$vwreserv==0 & complete.cases(raw.subset$fem_ratio)], na.rm = T), col="#00688B", lty=2)
  legend("topright", c(paste("General (N=", format(length(which(raw.subset$vwreserv == 0)), big.mark = ","), ")", sep=""),
                       paste("Reserved (N=", format(length(which(raw.subset$vwreserv == 1)), big.mark = ","), ")", sep="")), lty=c(2,1),
         col=c("#00688B", "#FF1493"), cex = 0.75)
  
  plot(density(matched.final$fem_ratio[matched.final$vwreserv==1 & complete.cases(matched.final$fem_ratio)], na.rm=T),
       col="#FF1493", xlim = c(0.2,1), ylim = c(0, 20), main="After genetic matching", xlab="Percentage of women in sub-district", ylab = "Density",
       las=1, cex.axis = 0.75)
  lines(density(matched.final$fem_ratio[matched.final$vwreserv==0 & complete.cases(matched.final$fem_ratio)], na.rm = T), col="#00688B", lty=2)
  legend("topright", c(paste("General (N=", format(length(which(matched.final$vwreserv == 0)), big.mark = ",") , ")", sep=""),
                       paste("Reserved (N=", format(length(which(matched.final$vwreserv == 1)), big.mark = ","), ")",sep="")), 
         lty=c(2,1), col=c("#00688B", "#FF1493"), cex = 0.75)
  
  dev.off()
  
  