
rm(list=ls())

# set curcomp to directory of folder holding rep_charmedlife
curcomp <- c("C:/Users/iosgood/Dropbox/CRsurvey/code"); 

# load packages
require(MASS)
require(Hmisc)
library(stargazer)
library(Amelia)
library(xtable)
library(Zelig)
library(ZeligChoice)

# set working directory 
setwd(paste(curcomp, "/rep_charmedlife", sep = ""))

# load data
crs <- read.csv("repdata_charmedlife.csv")
crs$diff <- factor(crs$diff, levels = c("Homogeneous","Mod. differentiated","Differentiated"))

# retain only Ag/Mining/Mnftr respondents and respondents that answered at least first three substantive questions
crsforimp <- crs[((is.na(crs$recip3) & is.na(crs$fp1) & is.na(crs$disagg_1)) == FALSE) & crs$amm,]

# 10 imputed datasets
set.seed(12345)
imps <- amelia(crsforimp, m = 10, idvars = c("V1","amm","isic","indrank","indrank2","indrank3","wave"), 
  logs = c("avgannexps","totexps","nmarkets","main_imp","main_exp","all_imp","all_exp"),
  noms = c("diff"), 
  ords = c("recip3","hetero2","asshet2","fp1","import","exporter","proddif","ca2","togeth",
    "disagg_1","disagg_2","trdpolimpt","trdseekinfo",
    "polact","trdpolact","effic_1","prod_1","exporter2","wtolib","rcaimp2")
)

# restore some factor levels and variable classes, and create some new variables
for(i in 1:10){
  # i =1
  imps$imputations[[i]][,"asshet2"][imps$imputations[[i]][,"togeth"]==2] <- NA
  imps$imputations[[i]][,"trdpolact"][imps$imputations[[i]][,"polact"]==0] <- 0
  imps$imputations[[i]][,"diff"] <- factor(imps$imputations[[i]][,"diff"], levels = c("Homogeneous","Mod. differentiated", "Differentiated"))
  imps$imputations[[i]][,"rcaimp2"] <- factor(imps$imputations[[i]][,"rcaimp2"], levels = c(0,1,2))
  imps$imputations[[i]]$recip3 <- factor(imps$imputations[[i]]$recip3, levels = c(1,2,3,4,5))
  imps$imputations[[i]]$trdpolimpt2 <- factor(imps$imputations[[i]]$trdpolimpt, levels = c(1,2,3,4,5))
  imps$imputations[[i]]$trdseekinfo2 <- factor(imps$imputations[[i]]$trdseekinfo, levels = c(1,2,3,4,5))
  imps$imputations[[i]]$nmarket <- imps$imputations[[i]]$nmarkets; imps$imputations[[i]]$nmarket[imps$imputations[[i]]$exporter == 0] <- NA
  imps$imputations[[i]]$avgannexp <- imps$imputations[[i]]$avgannexps; imps$imputations[[i]]$avgannexp[imps$imputations[[i]]$exporter == 0] <- NA
  imps$imputations[[i]]$totexp <- imps$imputations[[i]]$totexps; imps$imputations[[i]]$totexp[imps$imputations[[i]]$exporter == 0] <- NA
  imps$imputations[[i]]$wtolib2 <- factor(8 - imps$imputations[[i]]$wtolib, levels = c(1,2,3,4,5,6,7))
  imps$imputations[[i]]$indranks <-imps$imputations[[i]]$indrank; imps$imputations[[i]]$indranks[is.na(imps$imputations[[i]]$indranks)] <- 0
  imps$imputations[[i]]$indranks2 <-imps$imputations[[i]]$indrank2; imps$imputations[[i]]$indranks2[is.na(imps$imputations[[i]]$indranks2)] <- 0
  imps$imputations[[i]]$indranks3 <-imps$imputations[[i]]$indrank3; imps$imputations[[i]]$indranks3[is.na(imps$imputations[[i]]$indranks3)] <- 0
  imps$imputations[[i]][,"isic"] <- as.character(imps$imputations[[i]][,"isic"]); 
  imps$imputations[[i]][,"isic"][nchar(imps$imputations[[i]][,"isic"]) == 3] <- paste("0",  imps$imputations[[i]][,"isic"][nchar(imps$imputations[[i]][,"isic"]) == 3], sep = "")
}

# create function for rounding
myround <- function(vec, dig){
  return(formatC(round(as.numeric(as.character(vec)),dig),dig,format="f"))
}

# function for writing tables to disk
ptable <- function(contents, file){
  print(xtable(contents), type='latex', sanitize.text.function=identity, include.rownames = FALSE, include.colnames = FALSE, 
  file = file, only.contents = TRUE, hline.after = NULL)
}

###############################################
## Descriptive statistics for main variables ##
###############################################

vars <- c("recip3", "disagg_1", "disagg_2", "hetero2", "asshet2", "trdpolimpt2", "trdseekinfo2","polact","trdpolact",
          "exporter","avgannexps","nmarkets","proddif","fp1","import") 
sumstats <- data.frame(variable = vars, mean = NA, median = NA, min = NA, max = NA, sd = NA)
logs <- c("avgannexps","nmarkets")

for(j in vars){
  curvar <- c()
  for(i in 1:10){  # i <- 1
    if(j %in% logs){var <- log(imps$imputations[[i]][,j]+1)}else{var <- imps$imputations[[i]][,j]}
    curvar <- c(curvar, var)
  }  
  sumstats[sumstats$variable == j,] <-c(j,  myround(mean(curvar, na.rm = TRUE), 2), round(median(curvar, na.rm = TRUE),2), 
  round(min(curvar, na.rm = TRUE),2), round(max(curvar, na.rm = TRUE),2), myround(sd(curvar, na.rm = TRUE),2))
}

factsum <- function(fact, lev){ # fact = "diff"; lev = "Mod. differentiated"
  curvar <- c(); for(i in 1:10){curvar <- c(curvar, imps$imputations[[i]][,fact])}
  curvar <- as.numeric(curvar == lev); return(c(fact,  myround(mean(curvar, na.rm = TRUE), 2), round(median(curvar, na.rm = TRUE),2), 
  round(min(curvar, na.rm = TRUE),2), round(max(curvar, na.rm = TRUE),2), myround(sd(curvar, na.rm = TRUE),2)))
}

f1 <- factsum("ca2", 1); f2 <- factsum("ca2", 2); f3 <- factsum("rcaimp2", 1)
f4 <- factsum("rcaimp2", 2); f5 <- factsum("diff", 2); f6 <- factsum("diff", 3)

fvsumstats <- data.frame(rbind(f1, f2, f3, f4, f5, f6))
names(fvsumstats) <- names(sumstats)

sumstats <- rbind(sumstats, fvsumstats)

sumstats$variable <- c("Benefits from recip. lib.", "Import competition", "Export opportunities",
  "Divisions", "Association disagreement", "Trade important?", "Seek trade info", 
  "Political contact", "Political contact about trade", 
  "Exporter","$\\ln$ Annual exports", "$\\ln$ Number of markets", 
  "Substitutability", "Foreign production", "Importer",
  "Neutral Comparative Advantage","Comparative Advantage", "Neutral RCA", "RCA", "Mod. differentiated", "Differentiated")
print(xtable(sumstats), type='latex', sanitize.text.function=identity, include.rownames = FALSE)

######################################
## Firm char. and support for trade ##
######################################

## Table 1 (Main text)
mod2 <- zelig(recip3 ~ exporter + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod3 <- zelig(recip3 ~ log(totexps+1) + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod4 <- zelig(recip3 ~ log(avgannexps+1) + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod5 <- zelig(recip3 ~ log(nmarkets+1) + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)

mod2a <- zelig(recip3 ~ exporter + rcaimp2 + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod3a <- zelig(recip3 ~ log(totexps+1) + rcaimp2 + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod4a <- zelig(recip3 ~ log(avgannexps+1) + rcaimp2 + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod5a <- zelig(recip3 ~ log(nmarkets+1) + rcaimp2 + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)

mods <- list(summary(mod2)$coefficients[1:8,], summary(mod2a)$coefficients[1:8,],
  summary(mod4)$coefficients[1:8,], summary(mod4a)$coefficients[1:8,], summary(mod5)$coefficients[1:8,], summary(mod5a)$coefficients[1:8,]) 
vars <- c("exporter", "log(avgannexps + 1)", "log(nmarkets + 1)", row.names(mods[[1]])[2:3], row.names(mods[[2]])[2:8])
tab <- matrix(data = NA, ncol = length(mods), nrow = length(vars)*2)
rownames(tab) <- c(rep(vars, each = 2))
for(i in 1:length(mods)){ mod <- mods[[i]]; stars <- rep("", nrow(mod)); stars[mod[,4] < .1] <- "$^{*}$"; stars[mod[,4] < .05] <- "$^{**}$";
  stars[mod[,4] < .01] <- "$^{***}$"; 
  tab[rownames(tab) %in% rownames(mod),i] <- c(rbind(paste(myround(mod[,1],3), stars, sep = ""), paste("(", myround(mod[,2],3), ")", sep = "")))}
rownames(tab) <- c(rbind(vars, "")); rownames(tab)[c(1,3,5,7,9,11,13,15,17,19,21,23)] <- c("Exporter (Procomer)", 
   "$\\ln$ Annual exports", "$\\ln$ Number of markets", "Neutral Comp. Adv.", "Comparative Advantage", "Neutral RCA", "Positive RCA", "Mod. differentiated","Differentiated","Substitutability", "Foreign production", "Importer")
modsum <- summary(lm(as.numeric(recip3) ~ exporter + factor(ca2) + diff + proddif + fp1 + import, data = imps$imputations[[1]])); N <- modsum$df[1] + modsum$df[2]
tab <- rbind(tab, c(rep(N, length(mods)))); rownames(tab)[nrow(tab)] <- "N"
ptable(cbind(rownames(tab), tab), "tables/expsupMI.tex")

## assorted robustness checks
# leaving out our substitutability measure
mod2a <- zelig(recip3 ~ exporter + factor(ca2) + diff + fp1 + import, data = imps, model = "ologit", cite = F)
mod3a <- zelig(recip3 ~ log(totexps+1) + factor(ca2) + diff + fp1 + import, data = imps, model = "ologit", cite = F)
mod4a <- zelig(recip3 ~ log(avgannexps+1) + factor(ca2) + diff + fp1 + import, data = imps, model = "ologit", cite = F)
mod5a <- zelig(recip3 ~ log(nmarkets+1) + factor(ca2) + diff + fp1 + import, data = imps, model = "ologit", cite = F)

# excluding our dummy for `importing'
mod2a <- zelig(recip3 ~ exporter + factor(ca2) + diff + proddif + fp1, data = imps, model = "ologit", cite = F)
mod3a <- zelig(recip3 ~ log(totexps+1) + factor(ca2) + diff + proddif + fp1, data = imps, model = "ologit", cite = F)
mod4a <- zelig(recip3 ~ log(avgannexps+1) + factor(ca2) + diff + proddif + fp1, data = imps, model = "ologit", cite = F)
mod5a <- zelig(recip3 ~ log(nmarkets+1) + factor(ca2) + diff + proddif + fp1, data = imps, model = "ologit", cite = F)

# an alternative measure of ca
mod2a <- zelig(recip3 ~ exporter + log((main_exp+1)/(main_imp+1)) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod3a <- zelig(recip3 ~ log(totexps+1) + log((main_exp+1)/(main_imp+1)) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod4a <- zelig(recip3 ~ log(avgannexps+1) + log((main_exp+1)/(main_imp+1)) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod5a <- zelig(recip3 ~ log(nmarkets+1) + log((main_exp+1)/(main_imp+1)) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)

# a second alternative measure of ca
mod2a <- zelig(recip3 ~ exporter + log((all_exp+1)/(all_imp+1)) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod3a <- zelig(recip3 ~ log(totexps+1) + log((all_exp+1)/(all_imp+1)) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod4a <- zelig(recip3 ~ log(avgannexps+1) + log((all_exp+1)/(all_imp+1)) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod5a <- zelig(recip3 ~ log(nmarkets+1) + log((all_exp+1)/(all_imp+1)) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)

# including the wave of the survey as a control
mod2a <- zelig(recip3 ~ exporter + factor(ca2) + diff + proddif + fp1 + import + wave, data = imps, model = "ologit", cite = F)
mod3a <- zelig(recip3 ~ log(totexps+1) + factor(ca2) + diff + proddif + fp1 + import + wave, data = imps, model = "ologit", cite = F)
mod4a <- zelig(recip3 ~ log(avgannexps+1) + factor(ca2) + diff + proddif + fp1 + import + wave, data = imps, model = "ologit", cite = F)
mod5a <- zelig(recip3 ~ log(nmarkets+1) + factor(ca2) + diff + proddif + fp1 + import + wave, data = imps, model = "ologit", cite = F)

## Table A1 (Online appendix)
# self-identified productivity and sales
mod1 <- zelig(recip3 ~ exporter2 + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod2 <- zelig(recip3 ~ effic_1 + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod3 <- zelig(recip3 ~ prod_1 + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)

mods <- list(summary(mod1)$coefficients[1:8,], summary(mod2)$coefficients[1:8,], summary(mod3)$coefficients[1:8,]) 
vars <- c("exporter2", "effic_1", "prod_1", row.names(mods[[1]])[-1])
tab <- matrix(data = NA, ncol = length(mods), nrow = length(vars)*2)
rownames(tab) <- c(rep(vars, each = 2))
for(i in 1:length(mods)){ mod <- mods[[i]]; stars <- rep("", nrow(mod)); stars[mod[,4] < .1] <- "$^{*}$"; stars[mod[,4] < .05] <- "$^{**}$";
  stars[mod[,4] < .01] <- "$^{***}$"; # stars[mod[,4] < .001] <- "$^{***}$";
  tab[rownames(tab) %in% rownames(mod),i] <- c(rbind(paste(myround(mod[,1],3), stars, sep = ""), paste("(", myround(mod[,2],3), ")", sep = "")))}
rownames(tab) <- c(rbind(vars, "")); rownames(tab)[c(1,3,5,7,9,11,13,15,17,19)] <- c("Exporter (Reported)", 
   "Efficiency", "Productivity", "Neutral Comp. Adv.", "Comparative Advantage", "Mod. differentiated","Differentiated","Substitutability", "Foreign production", "Importer")
modsum <- summary(lm(as.numeric(recip3) ~ exporter2 + factor(ca2) + diff + proddif + fp1 + import, data = imps$imputations[[1]])); N <- modsum$df[1] + modsum$df[2]
tab <- rbind(tab, c(rep(N, length(mods)))); rownames(tab)[nrow(tab)] <- "N"
ptable(cbind(rownames(tab), tab), "tables/expsup2MI.tex")

## Table A2 (Online appendix)
## alternative measures of support for trade
mod2 <- zelig(wtolib2 ~ exporter + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod3 <- zelig(wtolib2 ~ log(totexps+1) + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod4 <- zelig(wtolib2 ~ log(avgannexps+1) + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod5 <- zelig(wtolib2 ~ log(nmarkets+1) + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod6 <- zelig(wtolib2 ~ exporter2 + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod7 <- zelig(wtolib2 ~ effic_1 + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod8 <- zelig(wtolib2 ~ prod_1 + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)

# create table by hand
mods <- list(summary(mod2)$coefficients[1:8,], summary(mod3)$coefficients[1:8,], summary(mod4)$coefficients[1:8,], 
  summary(mod5)$coefficients[1:8,], summary(mod6)$coefficients[1:8,], summary(mod7)$coefficients[1:8,], summary(mod8)$coefficients[1:8,]) 
vars <- c("exporter", "log(totexps + 1)", "log(avgannexps + 1)",
          "log(nmarkets + 1)", "exporter2", "effic_1", "prod_1", row.names(mods[[1]])[-1])
tab <- matrix(data = NA, ncol = length(mods), nrow = length(vars)*2)
rownames(tab) <- c(rep(vars, each = 2))
for(i in 1:length(mods)){ mod <- mods[[i]]; stars <- rep("", nrow(mod)); stars[mod[,4] < .1] <- "$^{*}$"; stars[mod[,4] < .05] <- "$^{**}$";
  stars[mod[,4] < .01] <- "$^{***}$"; # stars[mod[,4] < .001] <- "$^{***}$";
  tab[rownames(tab) %in% rownames(mod),i] <- c(rbind(paste(myround(mod[,1],3), stars, sep = ""), paste("(", myround(mod[,2],3), ")", sep = "")))}
rownames(tab) <- c(rbind(vars, "")); rownames(tab)[c(1,3,5,7,9,11,13,15,17,19,21,23,25,27)] <- c("Exporter (Procomer)", 
   "$\\ln$ Total exports", "$\\ln$ Avg. exports", "$\\ln$ Num. markets", "Exporter (Reported)", "Efficiency", "Productivity", 
   "Neutral Comp. Adv.", "Comparative Advantage", "Mod. differentiated","Differentiated","Substitutability", "Foreign production", "Importer")
modsum <- summary(lm(as.numeric(wtolib2) ~ exporter + factor(ca2) + diff + proddif + fp1 + import, data = imps$imputations[[1]])); N <- modsum$df[1] + modsum$df[2]
tab <- rbind(tab, c(rep(N, length(mods)))); rownames(tab)[nrow(tab)] <- "N"
ptable(cbind(rownames(tab), tab), "tables/expsup3MI.tex")

## Table A3 (Online appendix)
## second alternative measure of support for trade
mod2 <- zelig(tradesup ~ exporter + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ls", cite = F)
mod3 <- zelig(tradesup ~ log(totexps+1) + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ls", cite = F);
mod4 <- zelig(tradesup ~ log(avgannexps+1) + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ls", cite = F)
mod5 <- zelig(tradesup ~ log(nmarkets+1) + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ls", cite = F)
mod6 <- zelig(tradesup ~ exporter2 + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ls", cite = F)
mod7 <- zelig(tradesup ~ effic_1 + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ls", cite = F)
mod8 <- zelig(tradesup ~ prod_1 + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ls", cite = F)

# create table by hand
mods <- list(summary(mod2)$coefficients[2:9,], summary(mod3)$coefficients[2:9,], summary(mod4)$coefficients[2:9,], 
  summary(mod5)$coefficients[2:9,], summary(mod6)$coefficients[2:9,], summary(mod7)$coefficients[2:9,], summary(mod8)$coefficients[2:9,]) 
vars <- c("exporter", "log(totexps + 1)", "log(avgannexps + 1)",
          "log(nmarkets + 1)", "exporter2", "effic_1", "prod_1", row.names(mods[[1]])[-1])
tab <- matrix(data = NA, ncol = length(mods), nrow = length(vars)*2)
rownames(tab) <- c(rep(vars, each = 2))
for(i in 1:length(mods)){ mod <- mods[[i]]; stars <- rep("", nrow(mod)); stars[mod[,4] < .1] <- "$^{*}$"; stars[mod[,4] < .05] <- "$^{**}$";
  stars[mod[,4] < .01] <- "$^{***}$"; # stars[mod[,4] < .001] <- "$^{***}$";
  tab[rownames(tab) %in% rownames(mod),i] <- c(rbind(paste(myround(mod[,1],3), stars, sep = ""), paste("(", myround(mod[,2],3), ")", sep = "")))}
rownames(tab) <- c(rbind(vars, "")); rownames(tab)[c(1,3,5,7,9,11,13,15,17,19,21,23,25,27)] <- c("Exporter (Procomer)", 
   "$\\ln$ Total exports", "$\\ln$ Avg. exports", "$\\ln$ Num. markets", "Exporter (Reported)", "Efficiency", "Productivity", 
   "Neutral Comp. Adv.", "Comparative Advantage", "Mod. differentiated","Differentiated","Substitutability", "Foreign production", "Importer")
modsum <- summary(lm(tradesup ~ log(avgannexps+1) + factor(ca2) + diff + proddif + fp1 + import, data = imps$imputations[[1]])); N <- modsum$df[1] + modsum$df[2]
tab <- rbind(tab, c(rep(N, length(mods)))); rownames(tab)[nrow(tab)] <- "N"
ptable(cbind(rownames(tab), tab), "tables/expsup4MI.tex")

## Table A4 (Online appendix)
# industry rank measures of export competitiveness
mod0 <- zelig(recip3 ~ indranks + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod1 <- zelig(recip3 ~ indranks2 + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod1a <- zelig(recip3 ~ indranks3 + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod2 <- zelig(wtolib2 ~ indranks + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod3 <- zelig(wtolib2 ~ indranks2 + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod3a <- zelig(wtolib2 ~ indranks3 + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod4 <- zelig(tradesup ~ indranks + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ls", cite = F)
mod5 <- zelig(tradesup ~ indranks2 + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ls", cite = F)
mod5a <- zelig(tradesup ~ indranks3 + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ls", cite = F)

mods <- list(summary(mod0)$coefficients[1:8,], summary(mod1a)$coefficients[1:8,],
  summary(mod2)$coefficients[1:8,], summary(mod3a)$coefficients[1:8,],
  summary(mod4)$coefficients[2:9,], summary(mod5a)$coefficients[2:9,]) 
vars <- c("indranks", "indranks3", row.names(mods[[1]])[-1])
tab <- matrix(data = NA, ncol = length(mods), nrow = length(vars)*2)
rownames(tab) <- c(rep(vars, each = 2))
for(i in 1:length(mods)){ mod <- mods[[i]]; stars <- rep("", nrow(mod)); stars[mod[,4] < .1] <- "$^{*}$"; stars[mod[,4] < .05] <- "$^{**}$";
  stars[mod[,4] < .01] <- "$^{***}$"; # stars[mod[,4] < .001] <- "$^{***}$";
  tab[rownames(tab) %in% rownames(mod),i] <- c(rbind(paste(myround(mod[,1],3), stars, sep = ""), paste("(", myround(mod[,2],3), ")", sep = "")))}
rownames(tab) <- c(rbind(vars, "")); rownames(tab)[c(1,3,5,7,9,11,13,15,17)] <- c("Ind. Export Rank 1", "Ind. Export Rank 2", 
  "Neutral Comp. Adv.", "Comparative Advantage", "Mod. differentiated","Differentiated","Substitutability", "Foreign production", "Importer")
modsum <- summary(lm(as.numeric(recip3) ~ indranks + + factor(ca2) + diff + proddif + fp1 + import, data = imps$imputations[[1]])); N <- modsum$df[1] + modsum$df[2]
tab <- rbind(tab, c(rep(N, length(mods)))); rownames(tab)[nrow(tab)] <- "N"
ptable(cbind(rownames(tab), tab), "tables/expsupIndRankMI.tex")

##################################################################################
## Subsample analysis of relative effects of firm- and industry-level variables ## 
##################################################################################

## Table A7
mod2 <- zelig(recip3 ~ exporter + factor(ca2)*diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod3 <- zelig(recip3 ~ log(totexps+1) + factor(ca2)*diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod4 <- zelig(recip3 ~ log(avgannexps+1) + factor(ca2)*diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod5 <- zelig(recip3 ~ log(nmarkets+1) + factor(ca2)*diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)

# create table by hand
mods <- list(summary(mod2)$coefficients[1:12,], summary(mod3)$coefficients[1:12,], summary(mod4)$coefficients[1:12,], summary(mod5)$coefficients[1:12,]) 
vars <- c("exporter", "log(totexps + 1)", "log(avgannexps + 1)", "log(nmarkets + 1)", row.names(mods[[1]])[-1])
tab <- matrix(data = NA, ncol = length(mods), nrow = length(vars)*2)
rownames(tab) <- c(rep(vars, each = 2))
for(i in 1:length(mods)){ mod <- mods[[i]]; stars <- rep("", nrow(mod)); stars[mod[,4] < .1] <- "$^{*}$"; stars[mod[,4] < .05] <- "$^{**}$";
  stars[mod[,4] < .01] <- "$^{***}$"; # stars[mod[,4] < .001] <- "$^{***}$";
  tab[rownames(tab) %in% rownames(mod),i] <- c(rbind(paste(myround(mod[,1],3), stars, sep = ""), paste("(", myround(mod[,2],3), ")", sep = "")))}
rownames(tab) <- c(rbind(vars, "")); rownames(tab)[c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)] <- c("Exporter (Procomer)", "$\\ln$ Total exports", 
   "$\\ln$ Annual exports", "$\\ln$ Number of markets", "Neutral Comp. Adv.", "Comparative Advantage", "Mod. differentiated","Differentiated","Substitutability", "Foreign production", "Importer",
    "Neutral CA:Mod. differentiated", "CA:Mod. differentiated","Neutral CA:Differentiated", "CA:Differentiated")
modsum <- summary(lm(as.numeric(recip3) ~ exporter + factor(ca2) + diff + proddif + fp1 + import, data = imps$imputations[[1]])); N <- modsum$df[1] + modsum$df[2]
tab <- rbind(tab, c(rep(N, length(mods)))); rownames(tab)[nrow(tab)] <- "N"
ptable(cbind(rownames(tab), tab), "tables/expsupInteractionMI.tex")

## Table A8
mod2a <- zelig(recip3 ~ exporter + rcaimp2*diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod3a <- zelig(recip3 ~ log(totexps+1) + rcaimp2*diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod4a <- zelig(recip3 ~ log(avgannexps+1) + rcaimp2*diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod5a <- zelig(recip3 ~ log(nmarkets+1) + rcaimp2*diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)

# create table by hand
mods <- list(summary(mod2a)$coefficients[1:12,], summary(mod3a)$coefficients[1:12,], summary(mod4a)$coefficients[1:12,], summary(mod5a)$coefficients[1:12,]) 
vars <- c("exporter", "log(totexps + 1)", "log(avgannexps + 1)", "log(nmarkets + 1)", row.names(mods[[1]])[-1])
tab <- matrix(data = NA, ncol = length(mods), nrow = length(vars)*2)
rownames(tab) <- c(rep(vars, each = 2))
for(i in 1:length(mods)){ mod <- mods[[i]]; stars <- rep("", nrow(mod)); stars[mod[,4] < .1] <- "$^{*}$"; stars[mod[,4] < .05] <- "$^{**}$";
  stars[mod[,4] < .01] <- "$^{***}$"; # stars[mod[,4] < .001] <- "$^{***}$";
  tab[rownames(tab) %in% rownames(mod),i] <- c(rbind(paste(myround(mod[,1],3), stars, sep = ""), paste("(", myround(mod[,2],3), ")", sep = "")))}
rownames(tab) <- c(rbind(vars, "")); rownames(tab)[c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)] <- c("Exporter (Procomer)", "$\\ln$ Total exports", 
   "$\\ln$ Annual exports", "$\\ln$ Number of markets", "Neutral RCA", "Positive RCA", "Mod. differentiated","Differentiated","Substitutability", "Foreign production", "Importer",
    "Neutral RCA:Mod. differentiated", "RCA:Mod. differentiated","Neutral RCA:Differentiated", "RCA:Differentiated")
modsum <- summary(lm(as.numeric(recip3) ~ exporter + factor(ca2) + diff + proddif + fp1 + import, data = imps$imputations[[1]])); N <- modsum$df[1] + modsum$df[2]
tab <- rbind(tab, c(rep(N, length(mods)))); rownames(tab)[nrow(tab)] <- "N"
ptable(cbind(rownames(tab), tab), "tables/expsupInteraction2MI.tex")

#####################################
## Aspects of trade liberalization ##
#####################################

## Table 3 (Main text)
mod1 <- zelig(factor (disagg_1) ~ exporter + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod1a <- zelig(factor(disagg_1) ~ disagg_2, data = imps, model = "ologit", cite = F)
mod2a <- zelig(factor(disagg_2) ~ exporter + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod2b <- zelig(factor(disagg_2) ~ log(avgannexps+1) + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod2c <- zelig(factor(disagg_2) ~ log(nmarkets+1) + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)

# create table
mods <- list(summary(mod1)$coefficients[1:8,], matrix(summary(mod1a)$coefficients[1,], nrow = 1, ncol = 4), 
  summary(mod2a)$coefficients[1:8,], summary(mod2b)$coefficients[1:8,], summary(mod2c)$coefficients[1:8,]) 
rownames(mods[[2]]) <- "disagg_2"
vars <- c("exporter", "log(avgannexps + 1)", "log(nmarkets + 1)", row.names(mods[[1]])[-1], "disagg_2")
tab <- matrix(data = NA, ncol = length(mods), nrow = length(vars)*2)
rownames(tab) <- c(rep(vars, each = 2))
for(i in 1:length(mods)){mod <- mods[[i]]; stars <- rep("", nrow(mod)); stars[mod[,4] < .1] <- "$^{*}$"; stars[mod[,4] < .05] <- "$^{**}$";
  stars[mod[,4] < .01] <- "$^{***}$"; # stars[mod[,4] < .001] <- "$^{***}$";
  tab[rownames(tab) %in% rownames(mod),i] <- c(rbind(paste(myround(mod[,1],3), stars, sep = ""), paste("(", myround(mod[,2],3), ")", sep = "")))}
rownames(tab) <- c(rbind(vars, "")); rownames(tab)[c(1,3,5,7,9,11,13,15,17,19,21)] <- c("Exporter (Procomer)", "$\\ln$ Annual exports", "$\\ln$ Number of markets", 
  "Neutral Comp. Adv.", "Comparative Advantage", "Mod. differentiated","Differentiated","Substitutability", "Foreign production", "Importer", "Export opportunities")
modsum <- summary(lm(as.numeric(disagg_1) ~ exporter + factor(ca2) + diff + proddif + fp1 + import, data = imps$imputations[[1]])); N <- modsum$df[1] + modsum$df[2]
tab <- rbind(tab, c(rep(N, length(mods)))); rownames(tab)[nrow(tab)] <- "N"
ptable(cbind(rownames(tab), tab), "tables/libdimMI.tex")

# include control for the wave of the survey
mod1 <- zelig(factor(disagg_1) ~ exporter + factor(ca2) + diff + proddif + fp1 + import + wave, data = imps, model = "ologit", cite = F)
mod1a <- zelig(factor(disagg_1) ~ disagg_2 + wave, data = imps, model = "ologit", cite = F)
mod2a <- zelig(factor(disagg_2) ~ exporter + factor(ca2) + diff + proddif + fp1 + import + wave, data = imps, model = "ologit", cite = F)
mod2b <- zelig(factor(disagg_2) ~ log(avgannexps+1) + factor(ca2) + diff + proddif + fp1 + import + wave, data = imps, model = "ologit", cite = F)
mod2c <- zelig(factor(disagg_2) ~ log(nmarkets+1) + factor(ca2) + diff + proddif + fp1 + import + wave, data = imps, model = "ologit", cite = F)

###############
## Divisions ##
############### 

## Table A9 (Online appendix)
mod1a <- zelig(hetero2 ~ exporter + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "logit", cite = F)
mod1b <- zelig(hetero2 ~ log(avgannexps+1) + factor(ca2) + diff + proddif + factor(ca2) + fp1 + import, data = imps, model = "logit", cite = F)
mod2a <- zelig(asshet2 ~ exporter + factor(ca2) + diff + proddif + factor(ca2) + fp1 + import, data = imps, model = "logit", cite = F)
mod2b <- zelig(asshet2 ~ log(avgannexps+1) + factor(ca2) + diff + proddif + factor(ca2) + fp1 + import, data = imps, model = "logit", cite = F)

mods <- list(summary(mod1a)$coefficients[2:9,], summary(mod1b)$coefficients[2:9,], summary(mod2a)$coefficients[2:9,], summary(mod2b)$coefficients[2:9,])
vars <- c("exporter", "log(avgannexps + 1)", row.names(mods[[1]])[-1])
tab <- matrix(data = NA, ncol = length(mods), nrow = length(vars)*2)
rownames(tab) <- c(rep(vars, each = 2))
for(i in 1:length(mods)){mod <- mods[[i]]; stars <- rep("", nrow(mod)); stars[mod[,4] < .1] <- "$^{*}$"; stars[mod[,4] < .05] <- "$^{**}$";
  stars[mod[,4] < .01] <- "$^{***}$"; # stars[mod[,4] < .001] <- "$^{***}$";
  tab[rownames(tab) %in% rownames(mod),i] <- c(rbind(paste(myround(mod[,1],3), stars, sep = ""), paste("(", myround(mod[,2],3), ")", sep = "")))}
rownames(tab) <- c(rbind(vars, "")); rownames(tab)[c(1,3,5,7,9,11,13,15,17)] <- c("Exporter (Procomer)", "$\\ln$ Annual exports", 
  "Neutral Comp. Adv.", "Comparative Advantage", "Mod. differentiated","Differentiated","Substitutability", "Foreign production", "Importer")
modsum <- summary(lm(hetero2 ~ exporter + factor(ca2) + diff + proddif + fp1 + import, data = imps$imputations[[1]])); N <- modsum$df[1] + modsum$df[2]
modsum <- summary(lm(asshet2 ~ exporter + factor(ca2) + diff + proddif + fp1 + import, data = imps$imputations[[1]])); N2 <- modsum$df[1] + modsum$df[2]

tab <- rbind(tab, c(rep(N, 2), rep(N2, 2))); rownames(tab)[nrow(tab)] <- "N"
ptable(cbind(rownames(tab), tab), "tables/divmodelsMI.tex")

##########################
### Political activity ###
##########################

## Table 4 (Main text)
mod1b <- zelig(trdpolimpt2 ~ exporter + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod1d <- zelig(trdpolimpt2 ~ log(avgannexp+1) + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod1c <- zelig(trdpolimpt2 ~ log(nmarket+1) + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)

mod2b <- zelig(trdseekinfo2 ~ exporter + factor(ca2) + diff  + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod2d <- zelig(trdseekinfo2 ~ log(avgannexp+1) + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod2c <- zelig(trdseekinfo2 ~ log(nmarket+1) + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)

mod3b <- zelig(polact ~ exporter + factor(ca2) + diff  + proddif + fp1 + import, data = imps, model = "logit", cite = F)
mod3d <- zelig(polact ~ log(avgannexp+1) + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "logit", cite = F)
mod3c <- zelig(polact ~ log(nmarket+1) + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "logit", cite = F)

mod4b <- zelig(trdpolact ~ exporter + factor(ca2) + diff  + proddif + fp1 + import, data = imps, model = "logit", cite = F)
mod4d <- zelig(trdpolact ~ log(avgannexp+1) + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "logit", cite = F)
mod4c <- zelig(trdpolact ~ log(nmarket+1) + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "logit", cite = F)
 
mods <- list(summary(mod1d)$coefficients[1:8,], summary(mod1c)$coefficients[1:8,], 
  summary(mod2d)$coefficients[1:8,], summary(mod2c)$coefficients[1:8,],
  summary(mod3d)$coefficients[2:9,], summary(mod3c)$coefficients[2:9,],
  summary(mod4d)$coefficients[2:9,], summary(mod4c)$coefficients[2:9,])
vars <- c("log(avgannexp + 1)", "log(nmarket + 1)", row.names(mods[[1]])[c(-1)])
tab <- matrix(data = NA, ncol = length(mods), nrow = length(vars)*2)
rownames(tab) <- c(rep(vars, each = 2))
for(i in 1:length(mods)){
  mod <- mods[[i]]; stars <- rep("", nrow(mod)); stars[mod[,4] < .1] <- "$^{*}$"; stars[mod[,4] < .05] <- "$^{**}$";
  stars[mod[,4] < .01] <- "$^{***}$"; # stars[mod[,4] < .001] <- "$^{***}$";
  tab[rownames(tab) %in% rownames(mod),i] <- c(rbind(paste(myround(mod[,1],3), stars, sep = ""), paste("(", myround(mod[,2],3), ")", sep = "")))
}
rownames(tab) <- c(rbind(vars, "")); 
rownames(tab)[c(1,3,5,7,9,11,13,15,17)] <- c("$\\ln$ Annual exports", "$\\ln$ Number of markets", 
  "Neutral Comp. Adv.", "Comparative Advantage", "Mod. differentiated","Differentiated","Substitutability", "Foreign production", "Importer")
modsum <- summary(lm(as.numeric(trdpolimpt2) ~ log(avgannexp) + factor(ca2) + diff + proddif + fp1 + import, data = imps$imputations[[1]])); N <- modsum$df[1] + modsum$df[2]
tab <- rbind(tab, c(rep(N, length(mods)))); rownames(tab)[nrow(tab)] <- "N"
ptable(cbind(rownames(tab), tab), "tables/activityMI.tex")

## Table A6 (Online appendix)
# robustness with rank-based measures
mod1a <- zelig(trdpolimpt2 ~ indrank + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod1b <- zelig(trdpolimpt2 ~ indrank2 + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod1c <- zelig(trdpolimpt2 ~ indrank3 + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)

mod2a <- zelig(trdseekinfo2 ~ indrank + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod2b <- zelig(trdseekinfo2 ~ indrank2 + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)
mod2c <- zelig(trdseekinfo2 ~ indrank3 + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "ologit", cite = F)

mod3a <- zelig(polact ~ indrank + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "logit", cite = F)
mod3b <- zelig(polact ~ indrank2 + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "logit", cite = F)
mod3c <- zelig(polact ~ indrank3 + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "logit", cite = F)

mod4a <- zelig(trdpolact ~ indrank + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "logit", cite = F)
mod4b <- zelig(trdpolact ~ indrank2 + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "logit", cite = F)
mod4c <- zelig(trdpolact ~ indrank3 + factor(ca2) + diff + proddif + fp1 + import, data = imps, model = "logit", cite = F)

mods <- list(summary(mod1a)$coefficients[1:8,], summary(mod1c)$coefficients[1:8,], 
  summary(mod2a)$coefficients[1:8,], summary(mod2c)$coefficients[1:8,],
  summary(mod3a)$coefficients[2:9,], summary(mod3c)$coefficients[2:9,],
  summary(mod4a)$coefficients[2:9,], summary(mod4c)$coefficients[2:9,])
vars <- c("indrank", "indrank3", row.names(mods[[1]])[c(-1)])
tab <- matrix(data = NA, ncol = length(mods), nrow = length(vars)*2)
rownames(tab) <- c(rep(vars, each = 2))
for(i in 1:length(mods)){
  # i <- 5
  mod <- mods[[i]]; stars <- rep("", nrow(mod)); stars[mod[,4] < .1] <- "$^{*}$"; stars[mod[,4] < .05] <- "$^{**}$";
  stars[mod[,4] < .01] <- "$^{***}$"; # stars[mod[,4] < .001] <- "$^{***}$";
  tab[rownames(tab) %in% rownames(mod),i] <- c(rbind(paste(myround(mod[,1],3), stars, sep = ""), paste("(", myround(mod[,2],3), ")", sep = "")))
}
rownames(tab) <- c(rbind(vars, "")); 
rownames(tab)[c(1,3,5,7,9,11,13,15,17)] <- c("Ind. Export Rank 1", "Ind. Export Rank 2", 
  "Neutral Comp. Adv.", "Comparative Advantage", "Mod. differentiated","Differentiated","Substitutability", "Foreign production", "Importer")
modsum <- summary(lm(as.numeric(trdpolimpt2) ~ log(avgannexp) + factor(ca2) + diff + proddif + fp1 + import, data = imps$imputations[[1]])); N <- modsum$df[1] + modsum$df[2]
tab <- rbind(tab, c(rep(N, length(mods)))); rownames(tab)[nrow(tab)] <- "N"
ptable(cbind(rownames(tab), tab), "tables/activityIndRankMI.tex")




