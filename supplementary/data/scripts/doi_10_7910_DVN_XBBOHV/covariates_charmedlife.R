
rm(list=ls())

# set base directory
curcomp <- "C:/Users/iosgood/Dropbox/CRsurvey/code/rep_charmedlife/"

# load packages
library(plyr)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)

########################################
## Load survey respondents ISIC codes ##
########################################

surv_isic <- read.csv(paste(curcomp, "covariates/surveyresp_isic.csv", sep=""))
proccodeinsurv <- unique(surv_isic$proccode)
surv_isic$isic[nchar(surv_isic$isic) == 3] <- paste("0", surv_isic$isic[nchar(surv_isic$isic) == 3], sep = "")
surv_isic$isic_2[nchar(surv_isic$isic_2) == 3] <- paste("0", surv_isic$isic_2[nchar(surv_isic$isic_2) == 3], sep = "")
surv_isic$isic_3[nchar(surv_isic$isic_3) == 3] <- paste("0", surv_isic$isic_3[nchar(surv_isic$isic_3) == 3], sep = "")
surv_isic$isic_4[nchar(surv_isic$isic_4) == 3] <- paste("0", surv_isic$isic_4[nchar(surv_isic$isic_4) == 3], sep = "")

###################################
## Create Costa Rica World Trade ##
###################################

# trade volume of Costa Rica
world <- read.csv(paste(curcomp, "covariates/CRtradeworld_2012.csv", sep=""))

# subsettting based on 6 digits HS code
idx.6 <- grep("H4-[0-9]{6}", world$Commodity.Code)
trade_hs6 <- world[idx.6,] ## we get 7685 HS products
trade_hs6 <- trade_hs6[,c(2,5,7)]
colnames(trade_hs6) <- c("flow", "hs6", "value")
trade_hs6$hs6 <- gsub("^H4\\-", "", as.character(trade_hs6$hs6))

# now aggregate upto each unique ISIC code (version 3)
hs2isic <- read.csv(paste(curcomp, "covariates/JobID-64_Concordance_HS_to_I3.CSV",
  sep=""), colClasses="character")
hs2isic <- unique(hs2isic[,c(1,3)])
colnames(hs2isic) <- c("hs6", "isic")
merged <- merge(trade_hs6, hs2isic, by=c("hs6"), all.x=T)
uniq.isic <- sort(unique(hs2isic$isic))

# create a dataset of 4-digit ISIC world trade for Costa Rica
data <- data.frame(isic=uniq.isic, imp=NA, exp=NA)
for(i in 1:length(uniq.isic)){
  isic.i <- uniq.isic[i]
  idx.i <- which(merged$isic==isic.i)
  if(length(idx.i)>0){
    tmp.i <- merged[idx.i,]
    imp <- sum(tmp.i[which(tmp.i$flow=="Import"), "value"], na.rm=T)
    exp <- sum(tmp.i[which(tmp.i$flow=="Export"), "value"], na.rm=T)
    data$imp[i] <- imp; data$exp[i] <- exp
  }
}

# write to covariates folder
write.csv(data, paste(curcomp, "covariates/isic_world.csv", sep=""), row.names=F)

##################################################
## Create world trade variables for respondents ##
##################################################

# this function takes one isic and returns total imp and exp
compAdt_main <- function(isic){
    if(nchar(isic)==3){isic <- paste("0", isic, sep="")}
    idx <- which(data$isic==isic)
    if(length(idx)>0){
        imp.i <- data$imp[idx]
        exp.i <- data$exp[idx]
        return(c(imp.i, exp.i))
    } else {
       return(c(0,0))
    }
}

# this function takes upto 4 isic and returns total imp and exp
compAdt_all <- function(isic){
    isic <- na.omit(isic)
    fix3 <- which(nchar(isic)==3)
    if(length(fix3)>0){isic[fix3] <- paste("0", isic[fix3], sep="")}
    idx <- which(data$isic%in%isic)
    if(length(idx)>0){
        imp.i <- sum(data$imp[idx], na.rm=T)
        exp.i <- sum(data$exp[idx], na.rm=T)
        return(c(imp.i, exp.i))
    } else {
        return(c(0,0))
    }
}

# create the 4-digit ISIC imports and exports for the main ISIC respondent industry
isic_main <- t(sapply(surv_isic$isic, compAdt_main))

# create the 4-digit ISIC imports and exports for all available ISIC respondent industries
for(i in 1:nrow(surv_isic)){
    isic_all <- c(surv_isic$isic[i], surv_isic$isic_2[i], surv_isic$isic_3[i], surv_isic$isic_4[i])
    isic_all <- isic_all[!is.na(isic_all)]
    if(length(isic_all)>0){
        a <- compAdt_all(isic_all)
    } else {
        a <- c(0,0)
    }
    if(i==1){
        result <- a
    } else {
        result <- rbind(result, a)
    }
}

# merge into surv_isic
compAdt <- data.frame(main_imp=isic_main[,1], main_exp=isic_main[,2], all_imp=result[,1], all_exp=result[,2])
compAdt[is.na(compAdt)] <- 0
surv_isic <- cbind(surv_isic, compAdt)

############################################# 
## Create World Trade Data and RCD Measure ##
#############################################

# import trade volume of World
trade <- read.csv(paste(curcomp, "covariates/worimp2012hs6.csv", sep = "")); 
trade <- trade[,c("ProductCode","TradeValue.in.1000.USD")]; names(trade)[2] <- "imp"
trade$ProductCode[nchar(trade$ProductCode)==5] <- paste("0", trade$ProductCode[nchar(trade$ProductCode)==5], sep = "")
names(trade)[1] <- "hs6"; names(trade)[2] <- "value"

# now aggregate upto each unique ISIC code (version 3)
merged <- merge(trade, hs2isic, by=c("hs6"), all.x=T)
uniq.isic <- sort(unique(hs2isic$isic))

# creating a dataset
data <- data.frame(isic=uniq.isic, imp=NA)
for(i in 1:length(uniq.isic)){
    isic.i <- uniq.isic[i]
    idx.i <- which(merged$isic==isic.i)
    if(length(idx.i)>0){
        tmp.i <- merged[idx.i,]
        imp <- sum(tmp.i[, "value"], na.rm=T)
        data$imp[i] <- imp
    }
}
write.csv(data, paste(curcomp, "covariates/world_imports.csv", sep=""), row.names=F)

# import trade volume of CR
trade <- read.csv(paste(curcomp, "covariates/crimp2012hs6.csv", sep = "")); 
trade <- trade[,c("ProductCode","TradeValue.in.1000.USD")]; names(trade)[2] <- "imp"
trade$ProductCode[nchar(trade$ProductCode)==5] <- paste("0", trade$ProductCode[nchar(trade$ProductCode)==5], sep = "")
names(trade)[1] <- "hs6"; names(trade)[2] <- "value"

# now aggregate upto each unique ISIC code (version 3)
merged <- merge(trade, hs2isic, by=c("hs6"), all.x=T)
uniq.isic <- sort(unique(hs2isic$isic))

# creating a dataset
data <- data.frame(isic=uniq.isic, imp=NA)
for(i in 1:length(uniq.isic)){
    isic.i <- uniq.isic[i]
    idx.i <- which(merged$isic==isic.i)
    if(length(idx.i)>0){
        tmp.i <- merged[idx.i,]
        imp <- sum(tmp.i[, "value"], na.rm=T)
        data$imp[i] <- imp
    }
}
write.csv(data, paste(curcomp, "covariates/cr_imports.csv", sep=""), row.names=F)

# export trade volume of World
trade <- read.csv(paste(curcomp, "covariates/worexp2012hs6.csv", sep = "")); 
trade <- trade[,c("ProductCode","TradeValue.in.1000.USD")]; names(trade)[2] <- "imp"
trade$ProductCode[nchar(trade$ProductCode)==5] <- paste("0", trade$ProductCode[nchar(trade$ProductCode)==5], sep = "")
names(trade)[1] <- "hs6"; names(trade)[2] <- "value"

# now aggregate upto each unique ISIC code (version 3)
merged <- merge(trade, hs2isic, by=c("hs6"), all.x=T)
uniq.isic <- sort(unique(hs2isic$isic))

# creating a dataset
data <- data.frame(isic=uniq.isic, exp=NA)
for(i in 1:length(uniq.isic)){
    isic.i <- uniq.isic[i]
    idx.i <- which(merged$isic==isic.i)
    if(length(idx.i)>0){
        tmp.i <- merged[idx.i,]
        exp <- sum(tmp.i[, "value"], na.rm=T)
        data$exp[i] <- exp
    }
}
write.csv(data, paste(curcomp, "covariates/world_exports.csv", sep=""), row.names=F)

# export trade volume of CR
trade <- read.csv(paste(curcomp, "covariates/crexp2012hs6.csv", sep = "")); 
trade <- trade[,c("ProductCode","TradeValue.in.1000.USD")]; names(trade)[2] <- "imp"
trade$ProductCode[nchar(trade$ProductCode)==5] <- paste("0", trade$ProductCode[nchar(trade$ProductCode)==5], sep = "")
names(trade)[1] <- "hs6"; names(trade)[2] <- "value"

# now aggregate upto each unique ISIC code (version 3)
merged <- merge(trade, hs2isic, by=c("hs6"), all.x=T)
uniq.isic <- sort(unique(hs2isic$isic))

# creating a dataset
data <- data.frame(isic=uniq.isic, exp=NA)
for(i in 1:length(uniq.isic)){
    isic.i <- uniq.isic[i]
    idx.i <- which(merged$isic==isic.i)
    if(length(idx.i)>0){
        tmp.i <- merged[idx.i,]
        exp <- sum(tmp.i[, "value"], na.rm=T)
        data$exp[i] <- exp
    }
}
write.csv(data, paste(curcomp, "covariates/cr_exports.csv", sep=""), row.names=F)

# now merge imports and exports together
worimps <- read.csv(paste(curcomp, "covariates/world_imports.csv", sep=""))
worexps <- read.csv(paste(curcomp, "covariates/world_exports.csv", sep=""))
data <- merge(worimps, worexps, by = "isic", all.x = TRUE)
data$isic[nchar(data$isic) == 3] <- paste("0", data$isic[nchar(data$isic) == 3], sep = "")
names(data)[2:3] <- c("worimp","worexp")

crimps <- read.csv(paste(curcomp, "covariates/cr_imports.csv", sep=""))
crexps <- read.csv(paste(curcomp, "covariates/cr_exports.csv", sep=""))
data2 <- merge(crimps, crexps, by = "isic", all.x = TRUE)
data2$isic[nchar(data2$isic) == 3] <- paste("0", data2$isic[nchar(data2$isic) == 3], sep = "")

data <- merge(data2, data, all.x = TRUE)

data$crimpprop <- data$imp/sum(as.numeric(data$imp), na.rm = TRUE)
data$worimpprop <- data$worimp/sum(as.numeric(data$worimp), na.rm = TRUE)
data$rcaimp <- data$crimpprop/data$worimpprop
data$crexpprop <- (data$exp/(sum(as.numeric(data$exp), na.rm = TRUE)))
data$worexpprop <- data$worexp/sum(as.numeric(data$worexp), na.rm = TRUE)
data$rcaexp <- data$crexpprop/data$worexpprop

write.csv(data, paste(curcomp, "covariates/crwortrade.csv", sep=""), row.names=F)

##################################################
## Merge in RCD measure with survey respondents ##
##################################################

rcaimp <- read.csv(paste(curcomp, "covariates/crwortrade.csv", sep = ""), header = TRUE)
rcaimp$isic[nchar(rcaimp$isic)==3] <- paste("0", rcaimp$isic[nchar(rcaimp$isic)==3], sep = "")
surv_isic <- join(surv_isic, rcaimp[,c("isic","rcaimp","rcaexp")], by = "isic")

# create a trichotmous version of rca
surv_isic$rcaimp2 <- NA
surv_isic$rcaimp2[surv_isic$rcaimp >= quantile(surv_isic$rcaimp, .66, na.rm = TRUE)] <- 0
surv_isic$rcaimp2[surv_isic$rcaimp < quantile(surv_isic$rcaimp, .33, na.rm = TRUE)] <- 2 
surv_isic$rcaimp2[surv_isic$rcaimp >= quantile(surv_isic$rcaimp, .33, na.rm = TRUE) & surv_isic$rcaimp < quantile(surv_isic$rcaimp, .66, na.rm = TRUE)] <- 1 
surv_isic$rcaimp2 <- factor(surv_isic$rcaimp2, levels = c(0,1,2))

# create a trichotomous version of ca
diff <- surv_isic$all_exp - surv_isic$all_imp
diff[surv_isic$all_exp == 0 & surv_isic$all_imp == 0] <- NA
surv_isic$ca2 <- NA
surv_isic$ca2[diff > quantile(diff, .66, na.rm = TRUE)] <- 2
surv_isic$ca2[diff < quantile(diff, .33, na.rm = TRUE)] <- 0
surv_isic$ca2[diff > quantile(diff, .33, na.rm = TRUE) & diff < quantile(diff, .66, na.rm = TRUE)] <- 1

###########################################
## Create Rauch differentiation variable ##
###########################################

# load concordance available from 
# (http://ec.europa.eu/eurostat/ramon/relations/index.cfm?TargetUrl=LST_REL&StrLanguageCode=EN&IntCurrentPage=7)
isic_to_sitc <- read.csv(paste(curcomp, "covariates/ISIC_REV3_SITC_REV3.csv", sep = ""),
                         colClasses="character", header=T)
isic_to_sitc <- isic_to_sitc[2:nrow(isic_to_sitc),]
colnames(isic_to_sitc) <- c("isic", "sitc")

# now checking Rauch index for each ISIC
rauch <- read.table(paste(curcomp, "covariates/rauch_classification_rev2.txt", sep = ""), colClasses="character", header=T)

check_rauch <- function(isic){
    idx <- which(isic_to_sitc$isic == isic)
    if(length(idx)>0){
        sitcs <- unique(isic_to_sitc$sitc[idx])
        sitcs <- gsub("\\.", "", sitcs)
        sitcs <- sitcs[nchar(sitcs)>=4] # taking only those more than 4 digits
        sitcs <- unique(substring(sitcs, 1,4))
        ## now getting Rauch
        con <- rauch[which(rauch$sitc4 %in% sitcs),2]
        lib <- rauch[which(rauch$sitc4 %in% sitcs),3]
        ## w=goods traded on an organized exchange (homogeneous goods)
        ## r=reference priced
        ## n=differentiated products
        diff.con <- length(which(con=="n"))/length(con)
        diff.lib <- length(which(lib=="n"))/length(lib)
        homo.con <- length(which(con=="w"))/length(con)
        homo.lib <- length(which(lib=="w"))/length(lib)
        return(c(diff.con, homo.con, diff.lib, homo.lib))
    } else {
        return(rep(NA,4))
    }
}

isic_diff <- data.frame("isic" = unique(isic_to_sitc$isic))

for(i in 1:nrow(isic_diff)){
    ## print(i)
    isic <- as.character(isic_diff$isic[i])
    diff_measure.i <- check_rauch(isic)
    if(i==1){
        diff_measure <- diff_measure.i
    } else {
        diff_measure <- rbind(diff_measure, diff_measure.i)
    }
}

diff_measure <- as.data.frame(diff_measure)
colnames(diff_measure) <- c("diff.con", "homo.con", "diff.lib", "homo.lib")
rownames(diff_measure) <- seq(1:nrow(isic_diff))

isic_diff <- cbind(isic_diff, diff_measure)

write.csv(isic_diff, paste(curcomp, "covariates/diffallisic.csv", sep = ""), row.names = FALSE)

#######################################################################
## Merge in diff measure with survey respondents and create variable ##
#######################################################################

isic_diff <- read.csv(paste(curcomp, "covariates/diffallisic.csv", sep = ""), header = TRUE)
isic_diff$isic[nchar(isic_diff$isic)==3] <- paste("0", isic_diff$isic[nchar(isic_diff$isic)==3], sep = "")
surv_isic <- join(surv_isic, isic_diff, by = "isic")

surv_isic$diff <- "Mod. differentiated"
surv_isic$diff[surv_isic$homo.con > 0] <- "Homogeneous"
surv_isic$diff[surv_isic$diff.con > .5] <- "Differentiated"
surv_isic$diff <- factor(surv_isic$diff, levels = c("Homogeneous", "Mod. differentiated", "Differentiated"))

###############################
## Create Procomer variables ##
###############################

# merge in the procomer trade data covariates
procfirmall <- read.csv(paste(curcomp, "covariates/procfirmall.csv", sep = ""), header = TRUE)
procfirmall$isic4[nchar(procfirmall$isic4) == 3] <- paste("0", procfirmall$isic4[nchar(procfirmall$isic4) == 3], sep = "")
procfirmall$indisic[nchar(procfirmall$indisic) == 1] <- paste("0", procfirmall$indisic[nchar(procfirmall$indisic) == 1], sep = "")

# create sales rank within sector variable 
procfirmall$indrank <- NA; procfirmall$indrank2 <- NA; procfirmall$indrank3 <- NA
isics <- sort(unique(procfirmall$isic4))
for(i in isics){
  ind <- procfirmall$isic == i; ind[is.na(ind)] <- FALSE
  indrank <- rank(procfirmall$avgannexp[ind])/length(procfirmall$avgannexp[ind])
  indrank2 <- rank(procfirmall$totexp[ind])/length(procfirmall$totexp[ind])
  indrank3 <- rank(procfirmall$nmarkets[ind])/length(procfirmall$nmarkets[ind])
  procfirmall$indrank[ind] <- indrank
  procfirmall$indrank2[ind] <- indrank2
  procfirmall$indrank3[ind] <- indrank3
}

surv_isic <- join(surv_isic, procfirmall, by = "proccode")
avgannexps <- surv_isic$avgannexp; avgannexps[is.na(avgannexps)] <- 0; surv_isic$avgannexps <- avgannexps
totexps <- surv_isic$totexp; totexps[is.na(totexps)] <- 0; surv_isic$totexps <- totexps
surv_isic$nmarket <- surv_isic$nmarkets
nmarkets <- surv_isic$nmarkets; nmarkets[is.na(nmarkets)] <- 0; surv_isic$nmarkets <- nmarkets

# measure number of Procomer exporters per 4-digit ISIC industry
procisic4digs <- sort(na.omit(unique(substr(rcaimp$isic, 1,4)))); 
nexpsallproc <- data.frame(isic = procisic4digs, nexps4 = NA, nexps42012 = NA)
nexpsallproc$isic <- as.character(nexpsallproc$isic)

for(i in procisic4digs){ 
  # i <- procisic4digs[1]
  nexpsallproc[nexpsallproc$isic == i, "nexps4"] <- length(unique(procfirmall$proccode[procfirmall$isic == i]))
  nexpsallproc[nexpsallproc$isic == i, "nexps42012"] <- length(unique(procfirmall$proccode[procfirmall$isic == i & procfirmall$act2012 == TRUE]))
}

surv_isic <- join(surv_isic, nexpsallproc[,c("isic","nexps4","nexps42012")], by = "isic")

# measure number of Procomer exporters per 2-digit ISIC industry
procisic2digs <- sort(unique(substr(surv_isic$isic[nchar(rcaimp$isic) == 4], 1,2)))
nexpsallproc <- data.frame(indisic = procisic2digs, nexps = NA, nexps2012 = NA)
nexpsallproc$indisic <- as.character(nexpsallproc$indisic)

for(i in procisic2digs){ 
  # i <- procisic2digs[1]
  nexpsallproc[nexpsallproc$indisic == i, "nexps"] <- length(unique(procfirmall$proccode[procfirmall$indisic == i]))
  nexpsallproc[nexpsallproc$indisic == i, "nexps2012"] <- length(unique(procfirmall$proccode[procfirmall$indisic == i & procfirmall$act2012 == TRUE]))
}

surv_isic$indisic <- substr(surv_isic$isic, 1, 2)
surv_isic <- join(surv_isic, nexpsallproc[,c("indisic","nexps","nexps2012")], by = "indisic")

# merge in the tariff data 
tariff <- read.csv(paste(curcomp, "covariates/cravgmfn.csv", sep = ""), header = TRUE)
tariff$isic[nchar(tariff$isic)==3] <- paste("0", tariff$isic[nchar(tariff$isic)==3], sep = "")
surv_isic <- join(surv_isic, tariff[,c("isic","avgmfn")], by = "isic")

##############################
## Write covariates to disk ##
##############################

write.csv(surv_isic, paste(curcomp, "covariates/covariates_charmedlife.csv", sep = ""), row.names = FALSE)

##################################################
## Examine our sample relative to entire sample ##
##################################################

procfirm <- read.csv(paste(curcomp, "covariates/procfirmall.csv", sep = ""), header = TRUE)
procfirm$indisic[nchar(procfirm$indisic) ==1] <- paste("0", procfirm$indisic[nchar(procfirm$indisic) ==1], sep = "")
procfirm$survfirm <- procfirm$proccode %in% proccodeinsurv

# create appendix figure A1
pdf(paste(curcomp, "figures/procchecks.pdf", sep = ""), height = 6, width = 6.5)

par(mfrow=c(2,2), mai = c(.4,.4,.4,.5)) # bottom, left, top and right

hist(log(procfirm$avgannexp[procfirm$act2010==1 & procfirm$survfirm == 0]), probability = TRUE, 
  main = "log Avg. Annual Exports", cex.main=.8)
lines(density(log(procfirm$avgannexp[procfirm$survfirm == TRUE])), lwd = 2)
legend("topright", lwd = 2, legend = "In sample", cex = .7)
points(mean(log(procfirm$avgannexp[procfirm$act2010==1 & procfirm$survfirm == 0])), y = 0, pch= 24, 
  col = "black", bg = "#f0f0f0")
points(mean(log(procfirm$avgannexp[procfirm$survfirm == TRUE])), y = .01, pch= 25, 
  col = "black", bg = "#636363")

# t.test(log10(procfirm$avgannexp[procfirm$act2010==1 & procfirm$survfirm == 0]), log10(procfirm$avgannexp[procfirm$act2010==1 & procfirm$survfirm == 1]))

hist(log(procfirm$nmarkets[procfirm$act2010==1 & procfirm$survfirm == 0]), probability = TRUE, 
  main = "log Number of Export Markets", cex.main=.8)
lines(density(log(procfirm$nmarkets[procfirm$survfirm == TRUE])), lwd = 2)
# legend("topright", lwd = 2, legend = "In sample", cex = .7)
points(mean(log(procfirm$nmarkets[procfirm$act2010==1 & procfirm$survfirm == 0])), y = 0, pch= 24, 
  col = "black", bg = "#f0f0f0")
points(mean(log(procfirm$nmarkets[procfirm$survfirm == TRUE])), y = .018, pch= 25, 
  col = "black", bg = "#636363")

# t.test(log10(procfirm$nmarkets[procfirm$act2010==1 & procfirm$survfirm == 0]), log10(procfirm$nmarkets[procfirm$act2010==1 & procfirm$survfirm == 1]))
# exp(1.0576 - .823)

hist(log(procfirm$nprod[procfirm$act2010==1 & procfirm$survfirm == 0]), probability = TRUE, 
  main = "log Number of Products", cex.main=.8)
lines(density(log(procfirm$nprod[procfirm$survfirm == TRUE])), lwd = 2)
legend("topright", pch = c(25,24), col = "black", pt.bg = c("#636363","#f0f0f0"), cex = .7, legend = c("Sample average", "Population average"))
points(mean(log(procfirm$nprod[procfirm$act2010==1 & procfirm$survfirm == 0])), y = 0, pch= 24, 
  col = "black", bg = "#f0f0f0")
points(mean(log(procfirm$nprod[procfirm$survfirm == TRUE])), y = .0135, pch= 25, 
  col = "black", bg = "#636363")

# t.test(log10(procfirm$nprod[procfirm$act2010==1 & procfirm$survfirm == 0]), log10(procfirm$nprod[procfirm$act2010==1 & procfirm$survfirm == 1]))

hist(procfirm$nyear[procfirm$act2010==1 & procfirm$survfirm == 0], probability = TRUE, 
  main = "Number of Years Exporting", cex.main=.8)
lines(density(procfirm$nyear[procfirm$survfirm == TRUE], bw =.75), lwd = 2)
points(mean(procfirm$nyear[procfirm$act2010==1 & procfirm$survfirm == 0]), y = 0, pch= 24, 
  col = "black", bg = "#f0f0f0")
points(mean(procfirm$nyear[procfirm$survfirm == TRUE]), y = .01, pch= 25, 
  col = "black", bg = "#636363")

# t.test(procfirm$nyear[procfirm$act2010==1 & procfirm$survfirm == 0], procfirm$nyear[procfirm$act2010==1 & procfirm$survfirm == 1])
dev.off()

##########################
## Trade profile figure ##
##########################

# create a new dataset for some additional figures
cr <- surv_isic

# remove some firms that answered twice
resptwice <- c("R_3lKP5vVopq27Q5T", "R_ewwjPKYf1mQtVkx", "R_8wVpt534HFyvbG5","R_7QwtntdSZ7ONCaV",
  "R_787WpeOkIatHWkZ", "R_8umeWuf3qISiJcV", "R_85MeGXniJgeyA6h", "R_0Sy2CDRAw2ddHql", "R_5pDS87UuBf5ekU5",
  "R_9YwTFMe01hCQIDP","R_38WL2rp1ocK9c1v","R_0qEMbRZcn0W3gMd","R_3UdWL2nJMFa61hP","R_ctKLc9LF3hANh4x")
resptwice %in% cr$V1 # should be all trues

# cut out firms that responded twice by removing second reponse or mainly incomplete response then merge with proc data
cr <- cr[cr$V1 %in% resptwice == FALSE,]
cr <- join(cr, procfirm, by = "proccode")

# reload world trade data
crworld <- read.csv(paste(curcomp, "covariates/isic_world.csv", sep = ""))
fullisic <- crworld$isic
fullisic[nchar(fullisic) ==3] <- paste("0", fullisic[nchar(fullisic) ==3], sep = "")
crworld$isic2 <- substr(fullisic, 1, 2)

isic2s <- sort(unique(crworld$isic2)); isic2world <- data.frame(isic2 = isic2s, exp = 0, imp = 0)
for(i in isic2s){
  # i <- "01"
  isic2world$exp[isic2world$isic2 == i] <- sum(as.numeric(crworld$exp[crworld$isic2 == i]), na.rm = TRUE)
  isic2world$imp[isic2world$isic2 == i] <- sum(as.numeric(crworld$imp[crworld$isic2 == i]), na.rm = TRUE)
}

tradablewor <- isic2world[1:30,]
tradablewor$isic2 <- factor(tradablewor$isic2, unique(tradablewor$isic2))

fullisic <- cr$isic; fullisic[nchar(fullisic) ==3] <- paste("0", fullisic[nchar(fullisic) ==3], sep = "")
cr$isic2 <- substr(fullisic, 1, 2)
fullisic <- cr$isic_2; fullisic[nchar(fullisic) ==3] <- paste("0", fullisic[nchar(fullisic) ==3], sep = "")
cr$isic2_2 <- substr(fullisic, 1, 2)
fullisic <- cr$isic_3; fullisic[nchar(fullisic) ==3] <- paste("0", fullisic[nchar(fullisic) ==3], sep = "")
cr$isic2_3 <- substr(fullisic, 1, 2)
fullisic <- cr$isic_4; fullisic[nchar(fullisic) ==3] <- paste("0", fullisic[nchar(fullisic) ==3], sep = "")
cr$isic2_4 <- substr(fullisic, 1, 2)
cr$nind <- 1; cr$nind[is.na(cr$isic2) == FALSE] <- 1; cr$nind[is.na(cr$isic2_2) == FALSE] <- 2;
cr$nind[is.na(cr$isic2_3) == FALSE] <- 3; cr$nind[is.na(cr$isic2_4) == FALSE] <- 3;

isic2dat <- data.frame(isic2 = tradablewor$isic2, exp = 0, imp = 0, nfirm = 0, nfirm2 = 0, nfirm3 = 0, proctotexp = 0)
for(i in isic2s){
  # i <- "01"
  isic2dat$exp[isic2dat$isic2 == i] <- sum(as.numeric(cr$main_exp[cr$isic2 == i]), na.rm = TRUE)
  isic2dat$imp[isic2dat$isic2 == i] <- sum(as.numeric(cr$main_imp[cr$isic2 == i]), na.rm = TRUE)
  isic2dat$nfirm[isic2dat$isic2 == i] <- sum(cr$isic2 == i, na.rm = TRUE)
  isic2dat$nfirm2[isic2dat$isic2 == i] <- sum(cr$isic2 == i |cr$isic2_2 == i |cr$isic2_3 == i |cr$isic2_4 == i, na.rm = TRUE)
  ting <- c(as.numeric(cr$isic2 == i) * 1/cr$nind, as.numeric(cr$isic2_2 == i) * 1/cr$nind, as.numeric(cr$isic2_3 == i) * 1/cr$nind, as.numeric(cr$isic2_4 == i) * 1/cr$nind)
  isic2dat$nfirm3[isic2dat$isic2 == i] <- sum(ting, na.rm = TRUE)  
  isic2dat$proctotexp[isic2dat$isic2 == i] <- sum(as.numeric(cr$totexp[cr$isic2 == i]), na.rm = TRUE)
}

tradable <- isic2dat
tradable$isic2 <- factor(tradable$isic2, unique(tradable$isic2))

dat <- data.frame(isic2 = rep(tradable$isic2, 3), prop = c(tradablewor$exp/sum(tradablewor$exp), 
  tradable$nfirm/sum(tradable$nfirm), tradable$nfirm3/sum(tradable$nfirm3)), 
  Proportions = rep(c("Costa Rican exports", "Sample firms (1 ISIC)","Sample firms (All ISIC)"), each = nrow(tradable)))
dat$Proportions <- factor(dat$Proportions, levels = c("Costa Rican exports", "Sample firms (1 ISIC)","Sample firms (All ISIC)"))
library(RColorBrewer)
greycols <- brewer.pal(7, "Greys")[6:1]

cols <- c("black", greycols[1], greycols[3]); names(cols) <- levels(dat$Proportions)
k <- ggplot(data = dat, aes(x=isic2, y=prop, group = Proportion))
k <- k + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
k <- k + geom_point(aes(group = Proportions, colour = Proportions, shape = Proportions, size = Proportions))
k <- k + scale_colour_manual(values = cols) + scale_shape_manual(values = c(45, 17, 19)) + scale_size_manual(values = c(10, 2, 2))
k <- k + guides(fill=guide_legend(title=NULL))
k1 <- k + scale_y_continuous(lim=c(0,.27)) + xlab("ISIC Rev. 3.1 (2 digit industry)") + ylab("Proportion of total") + ggtitle("Comparing Costa Rica's exports by industry to respondents' industries")

dat <- data.frame(isic2 = rep(tradable$isic2, 2), prop = c(tradablewor$exp/sum(tradablewor$exp), 
  tradable$proctotexp/sum(tradable$proctotexp)), 
  Proportions = rep(c("Costa Rican exports", "Sample firm exports"), each = nrow(tradable)))
dat$Proportions <- factor(dat$Proportions, levels = c("Costa Rican exports", "Sample firm exports"))
cols2 <- c("black", greycols[2]); names(cols2) <- levels(dat$Proportions)

k <- ggplot(data = dat, aes(x=isic2, y=prop, group = Proportion))
k <- k + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
k <- k + geom_point(aes(group = Proportions, colour = Proportions, shape = Proportions, size = Proportions))
k <- k + scale_colour_manual(values = cols2) + scale_shape_manual(values = c(45, 15)) + scale_size_manual(values = c(10, 2))
k <- k + guides(fill=guide_legend(title=NULL))
k2 <- k + scale_y_continuous(lim=c(0,.27)) + xlab("ISIC Rev. 3.1 (2 digit industry)") + ylab("Proportion of total") + ggtitle("Comparing Costa Rica's exports by industry to respondents' exports")

pdf(paste(curcomp, "figures/isiccheck2.pdf", sep = ""), height = 12, width = 14)
grid.arrange(k1,k2, ncol = 1, nrow = 2) 
dev.off()

##############################################################
## Consider skewness of distributions at 2-digit ISIC level ##
##############################################################

inds <- sort(unique(procfirm$indisic)); inds[nchar(inds) == 1] <- paste("0", inds[nchar(inds) == 1], sep = "")
conc <- data.frame(ind = inds, nfirm = NA, tot = NA, top1 = NA, top5 = NA, top10 = NA, top20 = NA, maxexp = NA)
for(i in inds){  
  # i <- "30"
  ind <- procfirm$indisic == i & is.na(procfirm$indisic) == FALSE
  cur <- procfirm[ind,]
  nfirm <- sum(is.na(cur$totexp) == FALSE)
  tot <- sum(cur$totexp, na.rm = TRUE)
  top1 <- sum(cur$totexp[cur$totexp >= quantile(cur$totexp, .99, na.rm = TRUE)], na.rm = TRUE)/tot
  top5 <- sum(cur$totexp[cur$totexp >= quantile(cur$totexp, .95, na.rm = TRUE)], na.rm = TRUE)/tot
  top10 <- sum(cur$totexp[cur$totexp >= quantile(cur$totexp, .90, na.rm = TRUE)], na.rm = TRUE)/tot
  top20 <- sum(cur$totexp[cur$totexp >= quantile(cur$totexp, .80, na.rm = TRUE)], na.rm = TRUE)/tot
  maxexp <- max(cur$totexp, na.rm = TRUE)
  conc[inds == i,2:ncol(conc)] <- c(nfirm, tot, top1, top5, top10, top20, maxexp)
}
concpre <- conc

conc <- conc[conc$nfirm > 1,]
conc <- conc[conc$ind %in% c("91","92","99") == FALSE,]
conc$ind <- as.factor(conc$ind)

conc2 <- conc
conc2$top100 <- 1 - conc2$top20
conc2$top20 <- conc2$top20 - conc2$top10
conc2$top10 <- conc2$top10 - conc2$top5
conc2$top5 <- conc2$top5 - conc2$top1

conc3 <- data.frame(ind = rep(conc2$ind, 5), top = c(conc2$top1, conc2$top5, conc2$top10, conc2$top20, conc2$top100), 
  percentile = rep(c("top 1%","top 5%","top 10%","top 20%", "All"), each = nrow(conc)))
conc3$percentile <- factor(conc3$percentile, levels = c("top 1%","top 5%","top 10%","top 20%", "All"))

cols <- brewer.pal(7, "Greys")[5:1]

pdf(paste(curcomp, "figures/exportdist.pdf", sep = ""), height = 6, width = 12)
k <- ggplot(conc3, aes(ind, y = top, fill=percentile))
k <- k + xlab("Industry") + ylab("Proportion of export sales") + labs(title = "Export Sales are Highly Skewed towards Large Exporters")
k <- k + geom_bar(position = "fill", colour="white", stat="identity") + scale_fill_manual(values = cols)
k <- k + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
k
dev.off()

mean(conc$top1)
weighted.mean(conc$top1, w = conc$tot)

sum(conc$top10 > .8)
nrow(conc)

sum(procfirm$totexp[procfirm$totexp > quantile(procfirm$totexp, .99, na.rm = TRUE)])/sum(procfirm$totexp, na.rm = TRUE)
sum(procfirm$totexp[procfirm$totexp > quantile(procfirm$totexp, .95, na.rm = TRUE)])/sum(procfirm$totexp, na.rm = TRUE)
sum(procfirm$totexp[procfirm$totexp > quantile(procfirm$totexp, .90, na.rm = TRUE)])/sum(procfirm$totexp, na.rm = TRUE)

###########################################################
## Number of exporting firms in net-importing isic codes ##
###########################################################

crworld <- read.csv(paste(curcomp, "covariates/isic_world.csv", sep = ""))
fullisic <- crworld$isic
fullisic[nchar(fullisic) ==3] <- paste("0", fullisic[nchar(fullisic) ==3], sep = "")
crworld$isic2 <- substr(fullisic, 1, 2)

isic2s <- sort(unique(crworld$isic2)); isic2world <- data.frame(isic2 = isic2s, exp = 0, imp = 0)
for(i in isic2s){
  # i <- "01"
  isic2world$exp[isic2world$isic2 == i] <- sum(as.numeric(crworld$exp[crworld$isic2 == i]), na.rm = TRUE)
  isic2world$imp[isic2world$isic2 == i] <- sum(as.numeric(crworld$imp[crworld$isic2 == i]), na.rm = TRUE)
}

isic2world$iitadj <- (isic2world$exp - isic2world$imp)/(isic2world$exp + isic2world$imp)
iit <- isic2world[c(-31,-32,-33,-34,-35),]

# load short descriptions
des <- read.csv(paste(curcomp, "covariates/isic2shortdes.csv", sep = ""))
des$isic2[nchar(des$isic2) == 1] <- paste("0", des$isic2[nchar(des$isic2) == 1], sep = "")

iit <- merge(iit, concpre[,c("ind","nfirm","maxexp","tot")], by.x = "isic2", by.y = "ind")
iit <- merge(iit, des, by.x = "isic2", by.y = "isic2")

iit$expperexpfirm <- iit$tot/nfirm

tab <- iit[,c("isic2", "des", "iitadj", "nfirm", "expperexpfirm","maxexp")]
tab[,3] <- round(tab[,3], 2)
tab[,5:6] <- round(tab[,5:6], -3)/(1000) 

pdf(paste(curcomp, "figures/iitillus.pdf", sep = ""), height = 10, width = 10)
par(mfrow = c(2,1), mai = c(1,1,.8,.5))
plot(tab$nfirm ~ tab$iitadj, xlim = c(-1,1), ylim = c(1,10000), log = "y", pch = 19, xlab = "Net trade",
  main = "Net-importing industries have many (sucessful) exporters", ylab = "Number of firms")
abline(v = 0, lty = "dashed")
text(x = .5, y = 8000, "Net-exporting")
text(x = -.5, y = 8000, "Net-importing")

plot(tab$maxexp ~ tab$iitadj, xlim = c(-1,1), ylim = c(1,100000000), log = "y", pch = 19, xlab = "Net trade",
  ylab = "Exports of largest exporter (2002-12)")
abline(v = 0, lty = "dashed")
text(x = .5, y = 80000000, "Net-exporting")
text(x = -.5, y = 80000000, "Net-importing")
dev.off()

## ISIC 4 version
crworld$isic[nchar(cr$isicworld)==3] <- paste("0", crworld$isic[nchar(cr$isicworld)==3], sep = "")
inds <- sort(unique(crworld$isic)); inds[nchar(inds) == 1] <- paste("0", inds[nchar(inds) == 1], sep = "")
iit4 <- data.frame(ind = inds, nfirm = NA, tot = NA, tot10 = NA, tot12 = NA, maxexp = NA)
for(i in inds){  
  # i <- "2101"
  ind <- procfirm$isic4 == i & is.na(procfirm$indisic) == FALSE
  cur <- procfirm[ind,]
  nfirm <- sum(is.na(cur$totexp[cur$act2012 == TRUE]) == FALSE)
  tot <- sum(cur$totexp, na.rm = TRUE)
  tot10 <- sum(cur$exps2010, na.rm = TRUE)
  tot12 <- sum(cur$exps2012, na.rm = TRUE)
  maxexp <- ifelse(nfirm == 0, 0, max(cur$totexp[cur$act2012 == TRUE], na.rm = TRUE))
  iit4[inds == i,2:ncol(iit4)] <- c(nfirm, tot, tot10, tot12, maxexp)
}

iit4 <- merge(iit4, crworld, by.x = "ind", by.y = "isic")
iit4$iitadj <- (iit4$tot12 - iit4$imp)/(iit4$tot12 + iit4$imp) 
iit4$lnfirm <- log10(iit4$nfirm)
iit4$lnfirm[iit4$lnfirm == "-Inf"] <- -.1
iit4$maxexp[iit4$maxexp == 0] <- 1000

iitadjrat <- NA
iitadjrat[iit4$tot12/iit4$imp > 1 & is.na(iit4$tot12/iit4$imp) == FALSE] <- log10(iit4$tot12/iit4$imp)[iit4$tot12/iit4$imp > 1 & is.na(iit4$tot12/iit4$imp) == FALSE]
iitadjrat[iit4$imp/iit4$tot12 > 1 & is.na(iit4$imp/iit4$tot12) == FALSE] <- -log10(iit4$imp/iit4$tot12)[iit4$imp/iit4$tot12 > 1 & is.na(iit4$imp/iit4$tot12) == FALSE]
iitadjrat[iitadjrat < -3] <- -3
iit4$iitadjrat <- iitadjrat

pdf(paste(curcomp, "figures/iitillus3.pdf", sep = ""), height = 10, width = 10)
par(mfrow = c(2,1), mai = c(1,1,.8,.5))

plot((iit4$lnfirm) ~ iit4$iitadjrat, xlim = c(-3,3), ylim = c(-.1,log10(2500)), pch = 19, xlab = "Export/Import Ratio",
  main = "Net-importing industries have many (sucessful) exporters", ylab = "Number of firms", axes = F)
abline(v = 0, lty = "dashed")
text(x = 1.5, y = log10(2000), "Net-exporting")
text(x = -1.5, y = log10(2000), "Net-importing")
axis(1, at = c(-3,-2,-1,0,1,2,3), c(".001",".01",".1","1","10","100","1000"))
axis(2, at = c(0,1,2,3), labels = c("1","10","100","1000"), las = 1 ,cex =.7)

# no export industries
# summary(iit4$lnfirm == -.1 & iit4$iitadjrat == -3)

plot(iit4$maxexp/1000 ~ iit4$iitadjrat, xlim = c(-3,3), ylim = c(1,1000000000), log = "y", pch = 19, xlab = "Export/Import Ratio",
  ylab = "Exports of largest exporter (2000-12)", axes = F)
abline(v = 0, lty = "dashed")
text(x = 1.5, y = 800000000, "Net-exporting")
text(x = -1.5, y = 800000000, "Net-importing")
axis(1, at = c(-3,-2,-1,0,1,2,3), c(".001",".01",".1","1","10","100","1000"))
axis(2, at = c(1,10,100,1000, 10000, 100000, 1000000), labels = c("0","","1e5","","1e7","","1e9"), las = 1 ,cex =.7)

# no export industries
# summary(iit4$maxexp == 1000 & iit4$iitadjrat == -3)
dev.off()



