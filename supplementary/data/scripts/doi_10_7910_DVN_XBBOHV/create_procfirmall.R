

curcomp <- "C:/Users/iosgood/Dropbox/CRsurvey/code/rep_charmedlife/"

########################################################
### Load , clean and merge all of the needed datasets ##
########################################################

setwd(paste(curcomp, "covariates/", sep = ""))

### PROCOMER

YEARS <- 2000:2012
library(plyr)
warnings()
for(y in 1:length(YEARS)){
    year <-  YEARS[y]# 2004 #
    filename <- paste("./procomer/procomer", year, ".csv", sep="")
    procomer.y <- read.csv(filename, header=T, quote="\"")
    ## chosing variables
    if(year==2000){
        idx <- grep("year", colnames(procomer.y), ignore.case=T)
        idx <- c(idx,grep("procomer_id", colnames(procomer.y), ignore.case=T))
        idx <- c(idx, grep("hs_code", colnames(procomer.y), ignore.case=T))
        idx <- c(idx, grep("value", colnames(procomer.y), ignore.case=T))
        idx <- c(idx, grep("country", colnames(procomer.y), ignore.case=T))
    } else if(year==2001){
        procomer.y$year <- rep(2001, nrow(procomer.y))
        idx <- grep("year", colnames(procomer.y), ignore.case=T)
        idx <- c(idx,grep("nombre", colnames(procomer.y), ignore.case=T))
        idx <- c(idx, grep("partida", colnames(procomer.y), ignore.case=T))
        idx <- c(idx, grep("valor", colnames(procomer.y), ignore.case=T))
        idx <- c(idx, grep("pais", colnames(procomer.y), ignore.case=T))
    } else if(year==2002){
       procomer.y$year <- rep(2002, nrow(procomer.y))
        idx <- grep("year", colnames(procomer.y), ignore.case=T)
        idx <- c(idx,grep("nombre", colnames(procomer.y), ignore.case=T))
        idx <- c(idx, grep("partida", colnames(procomer.y), ignore.case=T))
        idx <- c(idx, grep("valor", colnames(procomer.y), ignore.case=T))
        idx <- c(idx, grep("pais", colnames(procomer.y), ignore.case=T))
   } else if(year == 2003 | year==2004 | year==2006){
        idx <- grep("^A.+O$", colnames(procomer.y), ignore.case=T)
        idx <- c(idx,grep("nombre", colnames(procomer.y), ignore.case=T))
        idx <- c(idx, grep("partida", colnames(procomer.y), ignore.case=T))
        idx <- c(idx, grep("valor", colnames(procomer.y), ignore.case=T))
        idx <- c(idx, grep("^pa.+s$", colnames(procomer.y), ignore.case=T))
    } else if(year==2005){
        idx <- grep("year", colnames(procomer.y), ignore.case=T)
        idx <- c(idx,grep("procomer_id", colnames(procomer.y), ignore.case=T))
        idx <- c(idx, grep("prod_code", colnames(procomer.y), ignore.case=T))
        idx <- c(idx, grep("value", colnames(procomer.y), ignore.case=T))
        idx <- c(idx, grep("country", colnames(procomer.y), ignore.case=T))
    } else if(year==2007){
        idx <- grep("year", colnames(procomer.y), ignore.case=T)
        idx <- c(idx,grep("nombre", colnames(procomer.y), ignore.case=T))
        idx <- c(idx, grep("partida", colnames(procomer.y), ignore.case=T))
        idx <- c(idx, grep("valor", colnames(procomer.y), ignore.case=T))
        idx <- c(idx, grep("pais", colnames(procomer.y), ignore.case=T))
    } else if(year==2008 | year==2010 | year==2011 | year==2012){
        idx <- grep("^A.+O$", colnames(procomer.y), ignore.case=T)
        idx <- c(idx,grep("nombre", colnames(procomer.y), ignore.case=T))
        idx <- c(idx, grep("partida", colnames(procomer.y), ignore.case=T))
        idx <- c(idx, grep("valo", colnames(procomer.y), ignore.case=T))
        idx <- c(idx, grep("pais", colnames(procomer.y), ignore.case=T))
    } else if(year==2009){
        idx <- grep("^A.+O$", colnames(procomer.y), ignore.case=T)
        idx <- c(idx,grep("nombre", colnames(procomer.y), ignore.case=T))
        idx <- c(idx, grep("partida", colnames(procomer.y), ignore.case=T))
        idx <- c(idx, grep("valor", colnames(procomer.y), ignore.case=T))
        idx <- c(idx, grep("desti", colnames(procomer.y), ignore.case=T))
    }

    procomer_sub <- procomer.y[,idx]
    colnames(procomer_sub) <- c("year", "procomerID", "hs10", "value", "destination")
    ## converting trade volume data from factor to numeric
    procomer_sub$value <- as.character(procomer_sub$value)
    a <- gsub("\\,", "", procomer_sub$value)
    a <- gsub("\\-", "", a)
    a <- gsub(" +", "", a)
    
    procomer_sub$value <- as.numeric(a)

    cat("the number of rows in year", year, "is:", nrow(procomer_sub), "\n")

    ## print(which(procomer_sub$procomerID=="E2153"))
    
    if(y==1){
        Data <- procomer_sub       
    } else {
        Data$hs10 <- factor(Data$hs10, levels = unique(c(levels(Data$hs10), unique(procomer_sub$hs10))))
        Data <- rbind.fill(Data, procomer_sub)
    }

}

Data$hs10 <- gsub("^p", "", Data$hs10)

## this function makes sure that each product is 10 digits: taking
## care of product number with 0 at the beginning

makeHS10 <- function(hs){
    hs <- as.character(hs)
    ## print(nchar(hs))
    if(nchar(hs)==9){
        hs <- paste("0", hs, sep="")
        return(hs)
    } else if (nchar(hs)==10){
        return(hs)
    } else if (nchar(hs)==8){
        hs <- paste(hs, "00", sep="")
        return(hs)
    } else if (nchar(hs)==7){
        hs <- paste("0", hs, sep="")
        hs <- paste(hs, "00", sep="")
        return(hs)
    } else {
        cat("somthing is wrong with: ",nchar(hs), hs, "\n")
        return(NA)
    }
}


Data$hs10 <- mapply(makeHS10, Data$hs10)
Data$hs10 <- paste("p", Data$hs10, sep="")
Data$hs10[which(Data$hs10=="pNA")] <- NA

write.csv(Data, "./procomer/procomerALL.csv", row.names=F)

#####################################
## Create firm-level Procomer data ##
#####################################

# load in the Procomer data cleaned by Insong
proc <- read.csv("procomer/procomerALL.csv", header = TRUE)
length(unique(proc$procomerID[proc$year == 2010]))

# develop some firm level covariates from the Procomer data
firms <- unique(proc$procomerID)

# nyear: number of years in Procomer dataset 
# totexp: total exports over all years
# avgannexp: avg annual exports over all years active
# nmarkets: total number of markets
# nprod: total number of unique hs10 products

procfirm <- data.frame(proccode = firms, nyear = NA, totexp = NA, avgannexp = NA, nmarkets = NA, nprod = NA, 
  act2000 = NA, act2001 = NA, act2002 = NA, act2003 = NA, act2004 = NA, act2005 = NA, act2006 = NA, act2007 = NA, 
  act2008 = NA, act2009 = NA, act2010 = NA, act2011 = NA, act2012 = NA, 
  isic2 = NA, isic4 = NA, exps2010 = NA, exps2012 = NA)

procisic <- read.csv("./procomer/procomer_isic.csv")

proc <- merge(proc, procisic[,c("proccode","isic")], by.x  = "procomerID", by.y = "proccode", all.x = TRUE)
proc$isic[nchar(proc$isic) == 3] <- paste("0", proc$isic[nchar(proc$isic) == 3], sep = "")
proc$isic2 <- substr(proc$isic, 1, 2)
proc$isic4 <- proc$isic

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

for(i in firms){
  # i <- firms[1]
  cur <- proc[proc$procomerID == i,]
  nyear <- length(unique(cur$year)); 
  totexp <- sum(cur$value, na.rm = TRUE)
  avgannexp <- totexp/nyear
  nmarkets <- length(unique(cur$destination))
  nprod <- length(unique(cur$hs10))
  act2000 <- 2000 %in% unique(cur$year)
  act2001 <- 2001 %in% unique(cur$year)
  act2002 <- 2002 %in% unique(cur$year)
  act2003 <- 2003 %in% unique(cur$year)
  act2004 <- 2004 %in% unique(cur$year)
  act2005 <- 2005 %in% unique(cur$year)
  act2006 <- 2006 %in% unique(cur$year)
  act2007 <- 2007 %in% unique(cur$year)
  act2008 <- 2008 %in% unique(cur$year)
  act2009 <- 2009 %in% unique(cur$year)
  act2010 <- 2010 %in% unique(cur$year)
  act2011 <- 2011 %in% unique(cur$year)
  act2012 <- 2012 %in% unique(cur$year)
  isic2 <- mode(cur$isic2)
  isic4 <- mode(cur$isic4)
  exps2010 <- sum(cur$value[cur$year == 2012], na.rm = TRUE)
  exps2012 <- sum(cur$value[cur$year == 2012], na.rm = TRUE)
  procfirm[procfirm$proccode == i, 2:ncol(procfirm)] <- c(nyear, totexp, avgannexp, nmarkets, nprod, 
  act2000, act2001, act2002, act2003, act2004, act2005, act2006, act2007, act2008, act2009, act2010, act2011, act2012, 
  isic2, isic4, exps2010, exps2012)
}

names(procfirm)[20] <- "indisic"
procfirm$indisic[nchar(procfirm$indisic) ==1] <- paste("0", procfirm$indisic[nchar(procfirm$indisic) ==1], sep = "")

# write to disk
# write.csv(procfirm, "./procomer/procfirmall.csv", row.names = FALSE)
