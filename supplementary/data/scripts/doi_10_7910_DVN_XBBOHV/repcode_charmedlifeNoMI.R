

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
crs <- read.csv("repdata_charmedlife.csv", row.names = 1)
crs$diff <- factor(crs$diff, levels = c("Homogeneous","Mod. differentiated","Differentiated"))

# retain only Ag/Mining/Mnftr respondents and respondents that answered at least first three substantive questions
cur <- crs[((is.na(crs$recip3) & is.na(crs$fp1) & is.na(crs$disagg_1)) == FALSE) & crs$amm,]
table(cur$diff)/nrow(cur)

cur[,"asshet2"][cur[,"togeth"]==2] <- NA
cur[,"trdpolact"][cur[,"polact"]==0] <- 0
cur[,"diff"] <- factor(cur[,"diff"], levels = c("Homogeneous","Mod. differentiated", "Differentiated"))
cur[,"rcaimp2"] <- factor(cur[,"rcaimp2"], levels = c(0,1,2))
cur$recip3 <- factor(cur$recip3, levels = c(1,2,3,4,5))
cur$trdpolimpt2 <- factor(cur$trdpolimpt, levels = c(1,2,3,4,5))
cur$trdseekinfo2 <- factor(cur$trdseekinfo, levels = c(1,2,3,4,5))
cur$nmarket <- cur$nmarkets; cur$nmarket[cur$exporter == 0] <- NA
cur$avgannexp <- cur$avgannexps; cur$avgannexp[cur$exporter == 0] <- NA
cur$totexp <- cur$totexps; cur$totexp[cur$exporter == 0] <- NA
cur$wtolib2 <- factor(8 - cur$wtolib, levels = c(1,2,3,4,5,6,7))
cur$indranks <-cur$indrank; cur$indranks[is.na(cur$indranks)] <- 0
cur$indranks2 <-cur$indrank2; cur$indranks2[is.na(cur$indranks2)] <- 0
cur$indranks3 <-cur$indrank3; cur$indranks3[is.na(cur$indranks3)] <- 0
cur[,"isic"] <- as.character(cur[,"isic"]); 
cur[,"isic"][nchar(cur[,"isic"]) == 3] <- paste("0",  cur[,"isic"][nchar(cur[,"isic"]) == 3], sep = "")

# create function for rounding
myround <- function(vec, dig){
  return(formatC(round(as.numeric(as.character(vec)),dig),dig,format="f"))
}

# function for writing tables to disk
ptable <- function(contents, file){
  print(xtable(contents), type='latex', sanitize.text.function=identity, include.rownames = FALSE, include.colnames = FALSE, 
  file = file, only.contents = TRUE, hline.after = NULL)
}

######################################
## Firm char. and support for trade ##
######################################

## Table A11 (Online appendix)
mod2 <- zelig(recip3 ~ exporter + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod3 <- zelig(recip3 ~ log(totexps+1) + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod4 <- zelig(recip3 ~ log(avgannexps+1) + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod5 <- zelig(recip3 ~ log(nmarkets+1) + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)

mod2a <- zelig(recip3 ~ exporter + rcaimp2 + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod3a <- zelig(recip3 ~ log(totexps+1) + rcaimp2 + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod4a <- zelig(recip3 ~ log(avgannexps+1) + rcaimp2 + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod5a <- zelig(recip3 ~ log(nmarkets+1) + rcaimp2 + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)

mods <- list(summary(mod2)$coefficients[1:8,], summary(mod2a)$coefficients[1:8,],
  summary(mod4)$coefficients[1:8,], summary(mod4a)$coefficients[1:8,], summary(mod5)$coefficients[1:8,], summary(mod5a)$coefficients[1:8,]) 
vars <- c("exporter", "log(avgannexps + 1)", "log(nmarkets + 1)", row.names(mods[[1]])[2:3], row.names(mods[[2]])[2:8])
tab <- matrix(data = NA, ncol = length(mods), nrow = length(vars)*2)
rownames(tab) <- c(rep(vars, each = 2))
for(i in 1:length(mods)){ mod <- mods[[i]]; stars <- rep("", nrow(mod)); stars[mod[,3] > qnorm(.95)] <- "$^{*}$"; stars[mod[,3] > qnorm(.975)] <- "$^{**}$";
  stars[mod[,3] > qnorm(.995)] <- "$^{***}$"; # stars[mod[,4] < .001] <- "$^{***}$";
  tab[rownames(tab) %in% rownames(mod),i] <- c(rbind(paste(myround(mod[,1],3), stars, sep = ""), paste("(", myround(mod[,2],3), ")", sep = "")))}
rownames(tab) <- c(rbind(vars, "")); rownames(tab)[c(1,3,5,7,9,11,13,15,17,19,21,23)] <- c("Exporter (Procomer)", 
   "$\\ln$ Annual exports", "$\\ln$ Number of markets", "Neutral Comp. Adv.", "Comparative Advantage", "Neutral RCA", "Positive RCA", "Mod. differentiated","Differentiated","Substitutability", "Foreign production", "Importer")
modsum <- summary(lm(as.numeric(recip3) ~ exporter + factor(ca2) + diff + proddif + fp1 + import, data = cur)); N <- modsum$df[1] + modsum$df[2]
tab <- rbind(tab, c(rep(N, length(mods)))); rownames(tab)[nrow(tab)] <- "N"
ptable(cbind(rownames(tab), tab), "tables/expsup.tex")

## assorted robustness checks
# leaving out our substitutability measure
mod2a <- zelig(recip3 ~ exporter + factor(ca2) + diff + fp1 + import, data = cur, model = "ologit", cite = F)
mod3a <- zelig(recip3 ~ log(totexps+1) + factor(ca2) + diff + fp1 + import, data = cur, model = "ologit", cite = F)
mod4a <- zelig(recip3 ~ log(avgannexps+1) + factor(ca2) + diff + fp1 + import, data = cur, model = "ologit", cite = F)
mod5a <- zelig(recip3 ~ log(nmarkets+1) + factor(ca2) + diff + fp1 + import, data = cur, model = "ologit", cite = F)

# excluding our dummy for `importing'
mod2a <- zelig(recip3 ~ exporter + factor(ca2) + diff + proddif + fp1, data = cur, model = "ologit", cite = F)
mod3a <- zelig(recip3 ~ log(totexps+1) + factor(ca2) + diff + proddif + fp1, data = cur, model = "ologit", cite = F)
mod4a <- zelig(recip3 ~ log(avgannexps+1) + factor(ca2) + diff + proddif + fp1, data = cur, model = "ologit", cite = F)
mod5a <- zelig(recip3 ~ log(nmarkets+1) + factor(ca2) + diff + proddif + fp1, data = cur, model = "ologit", cite = F)

# an alternative measure of ca
mod2a <- zelig(recip3 ~ exporter + log((main_exp+1)/(main_imp+1)) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod3a <- zelig(recip3 ~ log(totexps+1) + log((main_exp+1)/(main_imp+1)) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod4a <- zelig(recip3 ~ log(avgannexps+1) + log((main_exp+1)/(main_imp+1)) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod5a <- zelig(recip3 ~ log(nmarkets+1) + log((main_exp+1)/(main_imp+1)) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)

# a second alternative measure of ca
mod2a <- zelig(recip3 ~ exporter + log((all_exp+1)/(all_imp+1)) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod3a <- zelig(recip3 ~ log(totexps+1) + log((all_exp+1)/(all_imp+1)) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod4a <- zelig(recip3 ~ log(avgannexps+1) + log((all_exp+1)/(all_imp+1)) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod5a <- zelig(recip3 ~ log(nmarkets+1) + log((all_exp+1)/(all_imp+1)) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)

# including the wave of the survey as a control
mod2a <- zelig(recip3 ~ exporter + factor(ca2) + diff + proddif + fp1 + import + wave, data = cur, model = "ologit", cite = F)
mod3a <- zelig(recip3 ~ log(totexps+1) + factor(ca2) + diff + proddif + fp1 + import + wave, data = cur, model = "ologit", cite = F)
mod4a <- zelig(recip3 ~ log(avgannexps+1) + factor(ca2) + diff + proddif + fp1 + import + wave, data = cur, model = "ologit", cite = F)
mod5a <- zelig(recip3 ~ log(nmarkets+1) + factor(ca2) + diff + proddif + fp1 + import + wave, data = cur, model = "ologit", cite = F)

## Table A13 (Online appendix)
# self-identified productivity and sales
mod1 <- zelig(recip3 ~ exporter2 + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod2 <- zelig(recip3 ~ effic_1 + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod3 <- zelig(recip3 ~ prod_1 + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)

mods <- list(summary(mod1)$coefficients[1:8,], summary(mod2)$coefficients[1:8,], summary(mod3)$coefficients[1:8,]) 
vars <- c("exporter2", "effic_1", "prod_1", row.names(mods[[1]])[-1])
tab <- matrix(data = NA, ncol = length(mods), nrow = length(vars)*2)
rownames(tab) <- c(rep(vars, each = 2))
for(i in 1:length(mods)){ mod <- mods[[i]]; stars <- rep("", nrow(mod)); stars[mod[,3] > qnorm(.95)] <- "$^{*}$"; stars[mod[,3] > qnorm(.975)] <- "$^{**}$";
  stars[mod[,3] > qnorm(.995)] <- "$^{***}$"; # stars[mod[,4] < .001] <- "$^{***}$";
  tab[rownames(tab) %in% rownames(mod),i] <- c(rbind(paste(myround(mod[,1],3), stars, sep = ""), paste("(", myround(mod[,2],3), ")", sep = "")))}
rownames(tab) <- c(rbind(vars, "")); rownames(tab)[c(1,3,5,7,9,11,13,15,17,19)] <- c("Exporter (Reported)", 
   "Efficiency", "Productivity", "Neutral Comp. Adv.", "Comparative Advantage", "Mod. differentiated","Differentiated","Substitutability", "Foreign production", "Importer")
modsum <- summary(lm(as.numeric(recip3) ~ exporter2 + factor(ca2) + diff + proddif + fp1 + import, data = cur)); N <- modsum$df[1] + modsum$df[2]
tab <- rbind(tab, c(rep(N, length(mods)))); rownames(tab)[nrow(tab)] <- "N"
ptable(cbind(rownames(tab), tab), "tables/expsup2.tex")

## Table A14 (Online appendix)
## alternative measures of support for trade
mod2 <- zelig(wtolib2 ~ exporter + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod3 <- zelig(wtolib2 ~ log(totexps+1) + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod4 <- zelig(wtolib2 ~ log(avgannexps+1) + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod5 <- zelig(wtolib2 ~ log(nmarkets+1) + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod6 <- zelig(wtolib2 ~ exporter2 + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod7 <- zelig(wtolib2 ~ effic_1 + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod8 <- zelig(wtolib2 ~ prod_1 + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)

# create table by hand
mods <- list(summary(mod2)$coefficients[1:8,], summary(mod3)$coefficients[1:8,], summary(mod4)$coefficients[1:8,], 
  summary(mod5)$coefficients[1:8,], summary(mod6)$coefficients[1:8,], summary(mod7)$coefficients[1:8,], summary(mod8)$coefficients[1:8,]) 
vars <- c("exporter", "log(totexps + 1)", "log(avgannexps + 1)",
          "log(nmarkets + 1)", "exporter2", "effic_1", "prod_1", row.names(mods[[1]])[-1])
tab <- matrix(data = NA, ncol = length(mods), nrow = length(vars)*2)
rownames(tab) <- c(rep(vars, each = 2))
for(i in 1:length(mods)){ mod <- mods[[i]]; stars <- rep("", nrow(mod)); stars[mod[,3] > qnorm(.95)] <- "$^{*}$"; stars[mod[,3] > qnorm(.975)] <- "$^{**}$";
  stars[mod[,3] > qnorm(.995)] <- "$^{***}$"; # stars[mod[,4] < .001] <- "$^{***}$";
  tab[rownames(tab) %in% rownames(mod),i] <- c(rbind(paste(myround(mod[,1],3), stars, sep = ""), paste("(", myround(mod[,2],3), ")", sep = "")))}
rownames(tab) <- c(rbind(vars, "")); rownames(tab)[c(1,3,5,7,9,11,13,15,17,19,21,23,25,27)] <- c("Exporter (Procomer)", 
   "$\\ln$ Total exports", "$\\ln$ Avg. exports", "$\\ln$ Num. markets", "Exporter (Reported)", "Efficiency", "Productivity", 
   "Neutral Comp. Adv.", "Comparative Advantage", "Mod. differentiated","Differentiated","Substitutability", "Foreign production", "Importer")
modsum <- summary(lm(as.numeric(wtolib2) ~ exporter + factor(ca2) + diff + proddif + fp1 + import, data = cur)); N <- modsum$df[1] + modsum$df[2]
tab <- rbind(tab, c(rep(N, length(mods)))); rownames(tab)[nrow(tab)] <- "N"
ptable(cbind(rownames(tab), tab), "tables/expsup3.tex")

## Table A15 (Online appendix)
## second alternative measure of support for trade
mod2 <- zelig(tradesup ~ exporter + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ls", cite = F)
mod3 <- zelig(tradesup ~ log(totexps+1) + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ls", cite = F);
mod4 <- zelig(tradesup ~ log(avgannexps+1) + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ls", cite = F)
mod5 <- zelig(tradesup ~ log(nmarkets+1) + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ls", cite = F)
mod6 <- zelig(tradesup ~ exporter2 + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ls", cite = F)
mod7 <- zelig(tradesup ~ effic_1 + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ls", cite = F)
mod8 <- zelig(tradesup ~ prod_1 + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ls", cite = F)

# create table by hand
mods <- list(summary(mod2)$coefficients[2:9,], summary(mod3)$coefficients[2:9,], summary(mod4)$coefficients[2:9,], 
  summary(mod5)$coefficients[2:9,], summary(mod6)$coefficients[2:9,], summary(mod7)$coefficients[2:9,], summary(mod8)$coefficients[2:9,]) 
vars <- c("exporter", "log(totexps + 1)", "log(avgannexps + 1)",
          "log(nmarkets + 1)", "exporter2", "effic_1", "prod_1", row.names(mods[[1]])[-1])
tab <- matrix(data = NA, ncol = length(mods), nrow = length(vars)*2)
rownames(tab) <- c(rep(vars, each = 2))
for(i in 1:length(mods)){ mod <- mods[[i]]; stars <- rep("", nrow(mod)); stars[mod[,3] > qnorm(.95)] <- "$^{*}$"; stars[mod[,3] > qnorm(.975)] <- "$^{**}$";
  stars[mod[,3] > qnorm(.995)] <- "$^{***}$"; # stars[mod[,4] < .001] <- "$^{***}$";
  tab[rownames(tab) %in% rownames(mod),i] <- c(rbind(paste(myround(mod[,1],3), stars, sep = ""), paste("(", myround(mod[,2],3), ")", sep = "")))}
rownames(tab) <- c(rbind(vars, "")); rownames(tab)[c(1,3,5,7,9,11,13,15,17,19,21,23,25,27)] <- c("Exporter (Procomer)", 
   "$\\ln$ Total exports", "$\\ln$ Avg. exports", "$\\ln$ Num. markets", "Exporter (Reported)", "Efficiency", "Productivity", 
   "Neutral Comp. Adv.", "Comparative Advantage", "Mod. differentiated","Differentiated","Substitutability", "Foreign production", "Importer")
modsum <- summary(lm(tradesup ~ log(avgannexps+1) + factor(ca2) + diff + proddif + fp1 + import, data = cur)); N <- modsum$df[1] + modsum$df[2]
tab <- rbind(tab, c(rep(N, length(mods)))); rownames(tab)[nrow(tab)] <- "N"
ptable(cbind(rownames(tab), tab), "tables/expsup4.tex")

## Table A16 (Online appendix)
# industry rank measures of export competitiveness
mod0 <- zelig(recip3 ~ indranks + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod1 <- zelig(recip3 ~ indranks2 + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod1a <- zelig(recip3 ~ indranks3 + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod2 <- zelig(wtolib2 ~ indranks + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod3 <- zelig(wtolib2 ~ indranks2 + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod3a <- zelig(wtolib2 ~ indranks3 + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod4 <- zelig(tradesup ~ indranks + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ls", cite = F)
mod5 <- zelig(tradesup ~ indranks2 + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ls", cite = F)
mod5a <- zelig(tradesup ~ indranks3 + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ls", cite = F)

mods <- list(summary(mod0)$coefficients[1:8,], summary(mod1a)$coefficients[1:8,],
  summary(mod2)$coefficients[1:8,], summary(mod3a)$coefficients[1:8,],
  summary(mod4)$coefficients[2:9,], summary(mod5a)$coefficients[2:9,]) 
vars <- c("indranks", "indranks3", row.names(mods[[1]])[-1])
tab <- matrix(data = NA, ncol = length(mods), nrow = length(vars)*2)
rownames(tab) <- c(rep(vars, each = 2))
for(i in 1:length(mods)){ mod <- mods[[i]]; stars <- rep("", nrow(mod)); stars[mod[,3] > qnorm(.95)] <- "$^{*}$"; stars[mod[,3] > qnorm(.975)] <- "$^{**}$";
  stars[mod[,3] > qnorm(.995)] <- "$^{***}$"; # stars[mod[,4] < .001] <- "$^{***}$";
  tab[rownames(tab) %in% rownames(mod),i] <- c(rbind(paste(myround(mod[,1],3), stars, sep = ""), paste("(", myround(mod[,2],3), ")", sep = "")))}
rownames(tab) <- c(rbind(vars, "")); rownames(tab)[c(1,3,5,7,9,11,13,15,17)] <- c("Ind. Export Rank 1", "Ind. Export Rank 2", 
  "Neutral Comp. Adv.", "Comparative Advantage", "Mod. differentiated","Differentiated","Substitutability", "Foreign production", "Importer")
modsum <- summary(lm(as.numeric(recip3) ~ indranks + + factor(ca2) + diff + proddif + fp1 + import, data = cur)); N <- modsum$df[1] + modsum$df[2]
tab <- rbind(tab, c(rep(N, length(mods)))); rownames(tab)[nrow(tab)] <- "N"
ptable(cbind(rownames(tab), tab), "tables/expsupIndRank.tex")

#####################################
## Aspects of trade liberalization ##
#####################################

## Table A11 (Online appendix)
mod1 <- zelig(factor (disagg_1) ~ exporter + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod1a <- zelig(factor(disagg_1) ~ disagg_2, data = cur, model = "ologit", cite = F)
mod2a <- zelig(factor(disagg_2) ~ exporter + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod2b <- zelig(factor(disagg_2) ~ log(avgannexps+1) + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod2c <- zelig(factor(disagg_2) ~ log(nmarkets+1) + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)

# create table
mods <- list(summary(mod1)$coefficients[1:8,], matrix(summary(mod1a)$coefficients[1,], nrow = 1, ncol = 4), 
  summary(mod2a)$coefficients[1:8,], summary(mod2b)$coefficients[1:8,], summary(mod2c)$coefficients[1:8,]) 
rownames(mods[[2]]) <- "disagg_2"
vars <- c("exporter", "log(avgannexps + 1)", "log(nmarkets + 1)", row.names(mods[[1]])[-1], "disagg_2")
tab <- matrix(data = NA, ncol = length(mods), nrow = length(vars)*2)
rownames(tab) <- c(rep(vars, each = 2))
for(i in 1:length(mods)){ mod <- mods[[i]]; stars <- rep("", nrow(mod)); stars[mod[,3] > qnorm(.95)] <- "$^{*}$"; stars[mod[,3] > qnorm(.975)] <- "$^{**}$";
  stars[mod[,3] > qnorm(.995)] <- "$^{***}$"; # stars[mod[,4] < .001] <- "$^{***}$";
  tab[rownames(tab) %in% rownames(mod),i] <- c(rbind(paste(myround(mod[,1],3), stars, sep = ""), paste("(", myround(mod[,2],3), ")", sep = "")))}
rownames(tab) <- c(rbind(vars, "")); rownames(tab)[c(1,3,5,7,9,11,13,15,17,19,21)] <- c("Exporter (Procomer)", "$\\ln$ Annual exports", "$\\ln$ Number of markets", 
  "Neutral Comp. Adv.", "Comparative Advantage", "Mod. differentiated","Differentiated","Substitutability", "Foreign production", "Importer", "Export opportunities")
modsum <- summary(lm(as.numeric(disagg_1) ~ exporter + factor(ca2) + diff + proddif + fp1 + import, data = imps$imputations[[1]])); N <- modsum$df[1] + modsum$df[2]
tab <- rbind(tab, c(rep(N, length(mods)))); rownames(tab)[nrow(tab)] <- "N"
ptable(cbind(rownames(tab), tab), "tables/libdim.tex")

# include control for the wave of the survey
mod1 <- zelig(factor(disagg_1) ~ exporter + factor(ca2) + diff + proddif + fp1 + import + wave, data = cur, model = "ologit", cite = F)
mod1a <- zelig(factor(disagg_1) ~ disagg_2 + wave, data = cur, model = "ologit", cite = F)
mod2a <- zelig(factor(disagg_2) ~ exporter + factor(ca2) + diff + proddif + fp1 + import + wave, data = cur, model = "ologit", cite = F)
mod2b <- zelig(factor(disagg_2) ~ log(avgannexps+1) + factor(ca2) + diff + proddif + fp1 + import + wave, data = cur, model = "ologit", cite = F)
mod2c <- zelig(factor(disagg_2) ~ log(nmarkets+1) + factor(ca2) + diff + proddif + fp1 + import + wave, data = cur, model = "ologit", cite = F)

###############
## Divisions ##
############### 

## Table A18 (Online appendix)
mod1a <- zelig(hetero2 ~ exporter + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "logit", cite = F)
mod1b <- zelig(hetero2 ~ log(avgannexps+1) + factor(ca2) + diff + proddif + factor(ca2) + fp1 + import, data = cur, model = "logit", cite = F)
mod2a <- zelig(asshet2 ~ exporter + factor(ca2) + diff + proddif + factor(ca2) + fp1 + import, data = cur, model = "logit", cite = F)
mod2b <- zelig(asshet2 ~ log(avgannexps+1) + factor(ca2) + diff + proddif + factor(ca2) + fp1 + import, data = cur, model = "logit", cite = F)

mods <- list(summary(mod1a)$coefficients[2:9,], summary(mod1b)$coefficients[2:9,], summary(mod2a)$coefficients[2:9,], summary(mod2b)$coefficients[2:9,])
vars <- c("exporter", "log(avgannexps + 1)", row.names(mods[[1]])[-1])
tab <- matrix(data = NA, ncol = length(mods), nrow = length(vars)*2)
rownames(tab) <- c(rep(vars, each = 2))
for(i in 1:length(mods)){ mod <- mods[[i]]; stars <- rep("", nrow(mod)); stars[mod[,3] > qnorm(.95)] <- "$^{*}$"; stars[mod[,3] > qnorm(.975)] <- "$^{**}$";
  stars[mod[,3] > qnorm(.995)] <- "$^{***}$"; # stars[mod[,4] < .001] <- "$^{***}$";
  tab[rownames(tab) %in% rownames(mod),i] <- c(rbind(paste(myround(mod[,1],3), stars, sep = ""), paste("(", myround(mod[,2],3), ")", sep = "")))}
rownames(tab) <- c(rbind(vars, "")); rownames(tab)[c(1,3,5,7,9,11,13,15,17)] <- c("Exporter (Procomer)", "$\\ln$ Annual exports", 
  "Neutral Comp. Adv.", "Comparative Advantage", "Mod. differentiated","Differentiated","Substitutability", "Foreign production", "Importer")
modsum <- summary(lm(hetero2 ~ exporter + factor(ca2) + diff + proddif + fp1 + import, data = cur)); N <- modsum$df[1] + modsum$df[2]
modsum <- summary(lm(asshet2 ~ exporter + factor(ca2) + diff + proddif + fp1 + import, data = cur)); N2 <- modsum$df[1] + modsum$df[2]

tab <- rbind(tab, c(rep(N, 2), rep(N2, 2))); rownames(tab)[nrow(tab)] <- "N"
ptable(cbind(rownames(tab), tab), "tables/divmodels.tex")

##########################
### Political activity ###
##########################

## Table A12 (Online appendix)
mod1b <- zelig(trdpolimpt2 ~ exporter + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod1d <- zelig(trdpolimpt2 ~ log(avgannexp+1) + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod1c <- zelig(trdpolimpt2 ~ log(nmarket+1) + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)

mod2b <- zelig(trdseekinfo2 ~ exporter + factor(ca2) + diff  + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod2d <- zelig(trdseekinfo2 ~ log(avgannexp+1) + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod2c <- zelig(trdseekinfo2 ~ log(nmarket+1) + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)

mod3b <- zelig(polact ~ exporter + factor(ca2) + diff  + proddif + fp1 + import, data = cur, model = "logit", cite = F)
mod3d <- zelig(polact ~ log(avgannexp+1) + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "logit", cite = F)
mod3c <- zelig(polact ~ log(nmarket+1) + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "logit", cite = F)

mod4b <- zelig(trdpolact ~ exporter + factor(ca2) + diff  + proddif + fp1 + import, data = cur, model = "logit", cite = F)
mod4d <- zelig(trdpolact ~ log(avgannexp+1) + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "logit", cite = F)
mod4c <- zelig(trdpolact ~ log(nmarket+1) + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "logit", cite = F)
 
mods <- list(summary(mod1d)$coefficients[1:8,], summary(mod1c)$coefficients[1:8,], 
  summary(mod2d)$coefficients[1:8,], summary(mod2c)$coefficients[1:8,],
  summary(mod3d)$coefficients[2:9,], summary(mod3c)$coefficients[2:9,],
  summary(mod4d)$coefficients[2:9,], summary(mod4c)$coefficients[2:9,])
vars <- c("log(avgannexp + 1)", "log(nmarket + 1)", row.names(mods[[1]])[c(-1)])
tab <- matrix(data = NA, ncol = length(mods), nrow = length(vars)*2)
rownames(tab) <- c(rep(vars, each = 2))
for(i in 1:length(mods)){ mod <- mods[[i]]; stars <- rep("", nrow(mod)); stars[mod[,3] > qnorm(.95)] <- "$^{*}$"; stars[mod[,3] > qnorm(.975)] <- "$^{**}$";
  stars[mod[,3] > qnorm(.995)] <- "$^{***}$"; # stars[mod[,4] < .001] <- "$^{***}$";
  tab[rownames(tab) %in% rownames(mod),i] <- c(rbind(paste(myround(mod[,1],3), stars, sep = ""), paste("(", myround(mod[,2],3), ")", sep = "")))}
rownames(tab) <- c(rbind(vars, "")); 
rownames(tab)[c(1,3,5,7,9,11,13,15,17)] <- c("$\\ln$ Annual exports", "$\\ln$ Number of markets", 
  "Neutral Comp. Adv.", "Comparative Advantage", "Mod. differentiated","Differentiated","Substitutability", "Foreign production", "Importer")
modsum <- summary(lm(as.numeric(trdpolimpt2) ~ log(avgannexp) + factor(ca2) + diff + proddif + fp1 + import, data = cur)); N <- modsum$df[1] + modsum$df[2]
tab <- rbind(tab, c(rep(N, length(mods)))); rownames(tab)[nrow(tab)] <- "N"
ptable(cbind(rownames(tab), tab), "tables/activity.tex")

## Table A17 (Online appendix)
# robustness with rank-based measures
mod1a <- zelig(trdpolimpt2 ~ indrank + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod1b <- zelig(trdpolimpt2 ~ indrank2 + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod1c <- zelig(trdpolimpt2 ~ indrank3 + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)

mod2a <- zelig(trdseekinfo2 ~ indrank + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod2b <- zelig(trdseekinfo2 ~ indrank2 + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)
mod2c <- zelig(trdseekinfo2 ~ indrank3 + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "ologit", cite = F)

mod3a <- zelig(polact ~ indrank + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "logit", cite = F)
mod3b <- zelig(polact ~ indrank2 + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "logit", cite = F)
mod3c <- zelig(polact ~ indrank3 + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "logit", cite = F)

mod4a <- zelig(trdpolact ~ indrank + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "logit", cite = F)
mod4b <- zelig(trdpolact ~ indrank2 + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "logit", cite = F)
mod4c <- zelig(trdpolact ~ indrank3 + factor(ca2) + diff + proddif + fp1 + import, data = cur, model = "logit", cite = F)

mods <- list(summary(mod1a)$coefficients[1:8,], summary(mod1c)$coefficients[1:8,], 
  summary(mod2a)$coefficients[1:8,], summary(mod2c)$coefficients[1:8,],
  summary(mod3a)$coefficients[2:9,], summary(mod3c)$coefficients[2:9,],
  summary(mod4a)$coefficients[2:9,], summary(mod4c)$coefficients[2:9,])
vars <- c("indrank", "indrank3", row.names(mods[[1]])[c(-1)])
tab <- matrix(data = NA, ncol = length(mods), nrow = length(vars)*2)
rownames(tab) <- c(rep(vars, each = 2))
for(i in 1:length(mods)){ mod <- mods[[i]]; stars <- rep("", nrow(mod)); stars[mod[,3] > qnorm(.95)] <- "$^{*}$"; stars[mod[,3] > qnorm(.975)] <- "$^{**}$";
  stars[mod[,3] > qnorm(.995)] <- "$^{***}$"; # stars[mod[,4] < .001] <- "$^{***}$";
  tab[rownames(tab) %in% rownames(mod),i] <- c(rbind(paste(myround(mod[,1],3), stars, sep = ""), paste("(", myround(mod[,2],3), ")", sep = "")))}
rownames(tab) <- c(rbind(vars, "")); 
rownames(tab)[c(1,3,5,7,9,11,13,15,17)] <- c("Ind. Export Rank 1", "Ind. Export Rank 2", 
  "Neutral Comp. Adv.", "Comparative Advantage", "Mod. differentiated","Differentiated","Substitutability", "Foreign production", "Importer")
modsum <- summary(lm(as.numeric(trdpolimpt2) ~ log(avgannexp) + factor(ca2) + diff + proddif + fp1 + import, data = cur)); N <- modsum$df[1] + modsum$df[2]
tab <- rbind(tab, c(rep(N, length(mods)))); rownames(tab)[nrow(tab)] <- "N"
ptable(cbind(rownames(tab), tab), "tables/activityIndRank.tex")


