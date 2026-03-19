## Replication file for: Donnelly, Michael J 
## "Material interests, identity, and linked fate in three countries"
## British Journal of Political Science

## To replicate: put all three data frames in a subdirectory 
## named "data" and create a separate "figs" subdirectory, and an 
## appendix subdirectory inside figs

## Set your working directory to the local directory using "setwd()"
# 

## You may need to install these packages
library(tidyverse)
library(foreign)
library(gtools)
library(Hmisc)
library(stargazer)

## Code below will replicate all tables and figures in the main text and
## in the online appendix. Note that the tables produced here include
## all covariates, while those in the text omit control variables for 
## clarity of presentation. 


uk <- read.csv("data/ukreplication.csv")
ger <- read.csv("data/gerreplication.csv")
can <- read.csv("data/canreplication.csv")

can$region <- can$province


####################################################################
## Figure 1
####################################################################

dats <- list(uk, ger, can)
cnames <- c("UK", "Ger.", "Can.")
dims <- c("racelink", "religionlink", "regionlink", "classlink")
dnames <- c("Ethnicity", "Religion", "Region", "Class")


effs <- data.frame(Country = rep(NA, 12), 
                   Dimension = rep(NA, 12), 
                   Mean = rep(NA, 12), 
                   SD = rep(NA, 12), 
                   N = rep(NA, 12))

for(i in 1:length(dats)){
  thisdat <- dats[[i]]
  for (j in 1:length(dims)){
    thisrow <- (i - 1)*length(dims) + j
    thisdat$thisvar <- thisdat[,dims[j]]
    effs$Country[thisrow] <- cnames[i]
    effs$Dimension[thisrow] <- dnames[j]
    effs$Mean[thisrow] <- weighted.mean(thisdat$thisvar, thisdat$weight, na.rm = TRUE)
    effs$SD[thisrow] <- sqrt(wtd.var(thisdat$thisvar, thisdat$weight, 
                                     na.rm = TRUE, normwt = TRUE))
    
    effs$N[thisrow] <- nrow(subset(thisdat, !is.na(thisvar)))
  }
  
}

effs$upp <- effs$Mean + 1.96*effs$SD/sqrt(effs$N)
effs$low <- effs$Mean - 1.96*effs$SD/sqrt(effs$N)
effs$cdim <- paste(effs$Country, effs$Dimension, sep = " ")

pdf("figs/Figure1.pdf", width = 6, height = 3)
ggplot(effs, aes(x = Mean, y = cdim, group = Dimension)) + 
  geom_point(cex = 2, aes(pch = Dimension)) + 
  geom_segment(aes(x = low, xend = upp, y = cdim, yend = cdim)) + 
  ylab("") + theme(legend.position="none") + 
  xlab("Mean (1-4)") + 
  xlim(c(1, 4)) + 
  scale_fill_grey() + 
  theme_bw() + 
  scale_shape_manual(values=c(0, 1, 2, 5))

dev.off()

####################################################################
## Tables 2-4 
####################################################################

can$Country <- "Canada"
ger$Country <- "Germany"
uk$Country <- "UK"
varstokeep <- c("racelink", "religionlink", "regionlink", "classlink", 
                "joblossnum", "Bachelor", "employment", "union", 
                "Male", "agecat", "marital", "weight", "incnum", 
                "respondent_class", "relfreq", "religion", "region", 
                "moreno", "race", "taxarg", "Country", "taxnum")

all <- rbind(subset(ger, select = varstokeep), 
             subset(uk, select = varstokeep), 
             subset(can, select = varstokeep))

ukworking <- subset(uk, (employment == "Working full time" | 
                           employment == "Working part-time" | 
                           employment == " On a long-term leave") & union != "Not Employed")
gerworking <- subset(ger, (employment == "Working full time" | 
                             employment == "Working part-time"|
                             employment == " On a long-term leave") & union != "Not Employed")
canworking <- subset(can, (employment == "Working full time" | 
                             employment == "Working part-time"| 
                             employment == " On a long-term leave") & union != "Not Employed")
allworking <- subset(all, (employment == "Working full time" | 
                             employment == "Working part-time"| 
                             employment == " On a long-term leave") & union != "Not Employed")

mod1all <- lm(racelink ~ joblossnum + Male + agecat + union + marital + Bachelor +incnum + race, 
              data = allworking, weights = weight)
mod2all <- lm(religionlink ~ joblossnum + Male + agecat + union+ marital + religion + Bachelor +incnum, 
              data = allworking, weights = weight)
mod3all <- lm(regionlink ~ joblossnum + Male + agecat + union+ marital + region + Bachelor +incnum, 
              data = allworking, weights = weight)
mod4all <- lm(classlink ~ joblossnum + Male + agecat + union+ marital + Bachelor +incnum, 
              data = allworking, weights = weight)


mod1uk <- lm(racelink ~ joblossnum + Male + agecat + union + marital + educsimple +incnum + race, 
             data = ukworking, weights = weight)
mod2uk <- lm(religionlink ~ joblossnum + Male + agecat + union+ marital + religion + educsimple +incnum, 
             data = ukworking, weights = weight)
mod3uk <- lm(regionlink ~ joblossnum + Male + agecat + union+ marital + region + educsimple +incnum, 
             data = ukworking, weights = weight)
mod4uk <- lm(classlink ~ joblossnum + Male + agecat + union+ marital + educsimple +incnum, 
             data = ukworking, weights = weight)


mod1can <- lm(racelink ~ joblossnum + Male + agecat + union+ marital + race + educsimple +incnum, 
              data = canworking, weights = weight)
mod2can <- lm(religionlink ~ joblossnum + Male + agecat + union+ marital + religion + educsimple +incnum, 
              data = canworking, weights = weight)
mod3can <- lm(regionlink ~ joblossnum + Male + agecat + union+ marital + region + educsimple +incnum, 
              data = canworking, weights = weight)
mod4can <- lm(classlink ~ joblossnum + Male + agecat + union+ marital +  educsimple +incnum, 
              data = canworking, weights = weight)

mod1ger <- lm(racelink ~ joblossnum + Male + agecat + union+ marital + educsimple +incnum + race, 
              data = gerworking, weights = weight)
mod2ger <- lm(religionlink ~ joblossnum + Male + agecat + union+ marital+ educsimple + religion +incnum , 
              data = gerworking, weights = weight)
mod3ger <- lm(regionlink ~ joblossnum + Male + agecat + union+ marital + educsimple + region +incnum, 
              data = gerworking, weights = weight)
mod4ger <- lm(classlink ~ joblossnum + Male + agecat + union+ marital+ educsimple + incnum, 
              data = gerworking, weights = weight)


keycovs <- names(coef(mod1uk))[2:11]
cov.labs <- c("Likelihood of Job Loss", "Male", "Age 25-44", "Age 45-64", "Age 65+", 
              "Union", "Married", "High School", "Some Post-Secondary", "Income (1-7)")

keycovsG <- names(coef(mod1ger))[2:10]  ## No post-sec ed for germany
cov.labsG <- c(cov.labs[1:7], "Less than Bachelor's", "Income")


## First in html for ease of checking
stargazer(list(mod1uk, mod2uk, mod3uk, mod4uk),
          out = "figs/UKjobloss.html",
          keep = keycovs, 
          type = "html", 
          align = TRUE,
          covariate.labels = cov.labs,
          dep.var.labels = c("Ethnicity", "Religion", "Region", "Class"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          star.cutoffs = c(0.1, 0.05, 0.01), 
          add.lines = list(c("Group FE", "Yes", "Yes", "Yes", "No")))

stargazer(list(mod1can, mod2can, mod3can, mod4can),
          out = "figs/CAjobloss.html",
          keep = keycovs, 
          type = "html", 
          align = TRUE,
          covariate.labels = cov.labs,
          dep.var.labels = c("Ethnicity", "Religion", "Region", "Class"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          add.lines = list(c("Group FE", "Yes", "Yes", "Yes", "No")),
          star.cutoffs = c(0.1, 0.05, 0.01))


stargazer(list(mod1ger, mod2ger, mod3ger, mod4ger),
          out = "figs/Gerjobloss.html",
          keep = keycovsG, 
          type = "html", 
          align = TRUE,
          covariate.labels = cov.labsG,
          dep.var.labels = c("Ethnicity", "Religion", "Region", "Class"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          add.lines = list(c("Group FE", "Yes", "Yes", "Yes", "No")),
          star.cutoffs = c(0.1, 0.05, 0.01))

### Repeating for TeX

stargazer(list(mod1uk, mod2uk, mod3uk, mod4uk),
          out = "figs/UKjobloss.tex",
          keep = keycovs, 
          type = "latex", 
          align = TRUE,
          covariate.labels = cov.labs,
          dep.var.labels = c("Ethnicity", "Religion", "Region", "Class"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          star.cutoffs = c(0.1, 0.05, 0.01), 
          add.lines = list(c("Group FE", "Yes", "Yes", "Yes", "No")))

stargazer(list(mod1can, mod2can, mod3can, mod4can),
          out = "figs/CAjobloss.tex",
          keep = keycovs, 
          type = "latex", 
          align = TRUE,
          covariate.labels = cov.labs,
          dep.var.labels = c("Ethnicity", "Religion", "Region", "Class"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          add.lines = list(c("Group FE", "Yes", "Yes", "Yes", "No")),
          star.cutoffs = c(0.1, 0.05, 0.01))


stargazer(list(mod1ger, mod2ger, mod3ger, mod4ger),
          out = "figs/Gerjobloss.tex",
          keep = keycovsG, 
          type = "latex", 
          align = TRUE,
          covariate.labels = cov.labsG,
          dep.var.labels = c("Ethnicity", "Religion", "Region", "Class"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          add.lines = list(c("Group FE", "Yes", "Yes", "Yes", "No")),
          star.cutoffs = c(0.1, 0.05, 0.01))


####################################################################
## Table 5
####################################################################

mod7can <- lm(classlink ~ union+ Male + agecat +  marital + Bachelor +incnum, data = can, weights = weight)
mod7uk <- lm(classlink ~ union+ Male + agecat + marital  + Bachelor +incnum, data = uk, weights = weight)
mod7ger<- lm(classlink ~   union+Male + agecat +  marital + Bachelor +incnum, data = ger, weights = weight)

mod8can <- lm(classlink ~  union+Male + agecat + marital + Bachelor +incnum, 
              data = subset(can, respondent_class == "working class"), weights = weight)
mod8uk <- lm(classlink ~ union+ Male + agecat + marital  + Bachelor +incnum, 
             data = subset(uk, respondent_class == "working class"), weights = weight)
mod8ger<- lm(classlink ~   union+ Male + agecat +  marital + Bachelor +incnum, 
             data = subset(ger, respondent_class == "working class"), weights = weight)

mod8all <- lm(classlink ~   union+ Male + agecat +  marital + Bachelor +incnum, 
              data = subset(all, respondent_class == "working class"), weights = weight)



newcovs3 <- c("unionNot Employed", "unionYes", "Male",  "agecatAge2544", "agecatAge4564", "agecatAge65p", 
              "marital", "Bachelor", "incnum")
newcovlabs3 <- c( "Not working", "Union",  "Male", "Age 25-44", "Age 45-64", "Age 65+", 
                  "Married", "Bachelor's degree", "Income (1-7)")
stargazer(list(mod8all, mod8can, mod8uk, mod8ger),
          out = "figs/classUnion3country.html",
          keep = newcovs3, 
          type = "html", 
          align = TRUE,
          covariate.labels = newcovlabs3,
          dep.var.labels = c(""),
          column.labels = c("All", "Canada", "UK", "Germany"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          star.cutoffs = c(0.1, 0.05, 0.01))

stargazer(list(mod8all, mod8can, mod8uk, mod8ger),
          out = "figs/classUnion3country.tex",
          keep = newcovs3, 
          type = "latex", 
          align = TRUE,
          covariate.labels = newcovlabs3,
          dep.var.labels = c(""),
          column.labels = c("All", "Canada", "UK", "Germany"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          star.cutoffs = c(0.1, 0.05, 0.01))


####################################################################
## Table 6
####################################################################
mod9can <- lm(racelink ~ taxarg, data = subset(can, taxarg != "Reg"), weights = weight)
mod9uk <- lm(racelink ~ taxarg, data = subset(uk, taxarg != "Reg"), weights = weight)
mod9ger <- lm(racelink ~ taxarg, data = subset(ger, taxarg != "Reg"), weights = weight)


mod10can <- lm(regionlink ~ taxarg, data = subset(can, taxarg != "Eth"), weights = weight)
mod10ger <- lm(regionlink ~ taxarg, data = subset(ger, taxarg != "Eth"), weights = weight)


stargazer(list(mod9can,  mod9uk, mod9ger, mod10can, mod10ger),
          out = "Figs/TaxToLink.html",
          # keep = c("taxargEth", "taxargReg"),  
          type = "html", 
          align = TRUE,
          covariate.labels = c("Ethnic prime", "Regional prime"),
          dep.var.labels = c("Ethnicity", "Region"),
          column.labels = c("Canada", "UK", "Germany", "Canada", "Germany"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          star.cutoffs = c(0.1, 0.05, 0.01))
stargazer(list(mod9can,  mod9uk, mod9ger, mod10can, mod10ger),
          out = "Figs/TaxToLink.tex",
          # keep = c("taxargEth", "taxargReg"),  
          type = "latex", 
          align = TRUE,
          covariate.labels = c("Ethnic prime", "Regional prime"),
          dep.var.labels = c("Ethnicity", "Region"),
          column.labels = c("Canada", "UK", "Germany", "Canada", "Germany"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          star.cutoffs = c(0.1, 0.05, 0.01))

####################################################################
## Table 7
####################################################################


mod11can <- lm(racelink ~ taxarg, data = 
                 subset(can, race != "White" & race != "Other" & taxarg != "Reg"), weights = weight)
mod11uk <- lm(racelink ~ taxarg, 
              data = subset(uk, race != "White - British/English/Welsh/Scottish/Northern Irish"), 
              weights = weight)
mod11ger <- lm(racelink ~ taxarg, data = subset(ger, race != "Germans" & taxarg != "Reg"), weights = weight)


mod12can <- lm(regionlink ~ taxarg, 
               data = subset(can, province != "Ontario" & province != "British Columbia" & taxarg != "Eth"),
               weights = weight)
mod12ger <- lm(regionlink ~ taxarg, 
               data = subset(ger, region != "Berlin" & region != "Bavaria" & taxarg != "Eth"), weights = weight)

stargazer(list(mod11can,  mod11uk, mod11ger, mod12can, mod12ger),
          out = "Figs/TaxToLinkMinoritiesOnly.html",
          #   keep = c("taxargEth", "taxargReg"),  
          type = "html", 
          align = TRUE,
          covariate.labels = c("Ethnic prime", "Regional prime"),
          dep.var.labels = c("Ethnicity", "Region"),
          column.labels = c("Canada", "UK", "Germany", "Canada", "Germany"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          star.cutoffs = c(0.1, 0.05, 0.01), 
          add.lines = list(c("Excludes", 
                             "White", "White British", 
                             "Germans", "Ont/BC", 
                             "Bavaria/Berlin")))

stargazer(list(mod11can,  mod11uk, mod11ger, mod12can, mod12ger),
          out = "Figs/TaxToLinkMinoritiesOnly.tex",
          #   keep = c("taxargEth", "taxargReg"),  
          type = "latex", 
          align = TRUE,
          covariate.labels = c("Ethnic prime", "Regional prime"),
          dep.var.labels = c("Ethnicity", "Region"),
          column.labels = c("Canada", "UK", "Germany", "Canada", "Germany"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          star.cutoffs = c(0.1, 0.05, 0.01), 
          add.lines = list(c("Excludes", 
                             "White", "White British", 
                             "Germans", "Ont/BC", 
                             "Bavaria/Berlin")))

####################################################################
## Table 8
####################################################################

mod6can <- lm(religionlink ~ relfreq +  Male + agecat + union+ marital + Bachelor +incnum + religion, 
              data = can, weights = weight)
mod6uk <- lm(religionlink ~ relfreq + Male + agecat + union+ marital  + Bachelor +incnum+ religion, 
             data = uk, weights = weight)
mod6ger<- lm(religionlink ~ relfreq +  Male + agecat + union+ marital + Bachelor +incnum+ religion , 
             data = ger, weights = weight)

mod6all <- lm(religionlink ~ relfreq +  Male + agecat + union+ marital + Bachelor +incnum+ religion , 
              data = all, weights = weight)


newcovs2 <- c("relfreq",  "Male",  "AgecatAge2544", "agecatAge4564", "agecatAge65p", 
              "unionNotEmployed", "unionYes",  "marital", "Bachelor", "incnum")
newcovlabs2 <- c("Religious attendance", "Male", "Age 25-44", "Age 45-64", "Age 65+", 
                 "Not working", "Union",  "Married", "Bachelor's degree", "Income (1-7)")
stargazer(list(mod6all, mod6can, mod6uk, mod6ger),
          out = "Figs/relFreq3country.html",
          keep = newcovs2, 
          type = "html", 
          align = TRUE,
          covariate.labels = newcovlabs2,
          dep.var.labels = c(""),
          column.labels = c("All", "Canada", "UK", "Germany"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          star.cutoffs = c(0.1, 0.05, 0.01), 
          add.lines = list(c("Religion FE", "Yes", "Yes", "Yes", "Yes")))
stargazer(list(mod6all, mod6can, mod6uk, mod6ger),
          out = "Figs/relFreq3country.tex",
          keep = newcovs2, 
          type = "latex", 
          align = TRUE,
          covariate.labels = newcovlabs2,
          dep.var.labels = c(""),
          column.labels = c("All", "Canada", "UK", "Germany"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          star.cutoffs = c(0.1, 0.05, 0.01), 
          add.lines = list(c("Religion FE", "Yes", "Yes", "Yes", "Yes")))


####################################################################
## Table 9
####################################################################

mod5can <- lm(regionlink ~ moreno +  Male + agecat + union+ marital + Bachelor +incnum + province, 
              data = can, weights = weight)
mod5uk <- lm(regionlink ~ moreno + Male + agecat + union+ marital  + Bachelor +incnum+ region, 
             data = uk, weights = weight)
mod5ger<- lm(regionlink ~ moreno +  Male + agecat + union+ marital + Bachelor +incnum+ region , 
             data = ger, weights = weight)
mod5all <- lm(regionlink ~ moreno +  Male + agecat + union+ marital + Bachelor +incnum+ region , 
              data = all, weights = weight)



newcovs <- c("moreno",  "Male",  "AgecatAge2544", "agecatAge4564", "agecatAge65p", 
             "unionYes", "unionNotEmployed", "marital", "Bachelor", "incnum")
newcovlabs <- c("Regional ID", "Male", "Age 25-44", "Age 45-64", "Age 65+", 
                "Not working", "Union", "Married", "Bachelor's degree", "Income (1-7)")
stargazer(list(mod5all, mod5can, mod5uk, mod5ger),
          out = "Figs/regMoreno3country.html",
          keep = newcovs, 
          type = "html", 
          align = TRUE,
          covariate.labels = newcovlabs,
          dep.var.labels = c(""),
          column.labels = c("All", "Canada", "UK", "Germany"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          star.cutoffs = c(0.1, 0.05, 0.01), 
          add.lines = list(c("Region FE", "Yes", "Yes", "Yes", "Yes")))
stargazer(list(mod5all, mod5can, mod5uk, mod5ger),
          out = "Figs/regMoreno3country.tex",
          keep = newcovs, 
          type = "latex", 
          align = TRUE,
          covariate.labels = newcovlabs,
          dep.var.labels = c(""),
          column.labels = c("All", "Canada", "UK", "Germany"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          star.cutoffs = c(0.1, 0.05, 0.01), 
          add.lines = list(c("Region FE", "Yes", "Yes", "Yes", "Yes")))



####################################################################
## Figure 2
####################################################################
canrelinc <- read.csv("data/CanRelIncs.csv") ## Note: these come from 2011 NHS PUMF
canincs <- read.csv("data/CanIncCats.csv")
canincs$mid <- (canincs$Upp + canincs$Low)/2

ocr <- c(4, 6, 7, 8, 9, 11)           
otherchristian <- apply(canrelinc[ocr, 2:ncol(canrelinc)], FUN = sum, 
                        MARGIN = 2)

canrelinc$Religion <- as.character(canrelinc$Religion)
canrelinc <- rbind(canrelinc, c("Other Christian", rep(NA, 33)))
canrelinc[nrow(canrelinc), 2:ncol(canrelinc)] <- otherchristian

avgincome <- rep(NA, nrow(canrelinc))

for(i in 1:length(avgincome)){
  avgincome[i] <- weighted.mean(canincs$mid[1:33], canrelinc[i, 2:34])
}

# List of rich/poor canadian religions

canrelsmall <- data.frame(religion = canrelinc$Religion, relincome = avgincome)
totinc <- subset(canrelsmall, religion == "Total")$relincome
canpoorRels <- canrelsmall$religion[canrelsmall$relincome < totinc]
canRichRels <- canrelsmall$religion[canrelsmall$relincome > totinc]

mod13can <- lm(taxnum ~ religionlink + union+ Male + 
                 agecat +  marital + Bachelor +incnum + taxarg + jl2 + religion, 
               data = subset(can, religion %in%canpoorRels & religion != "Non-religious"), 
               weights = weight)
mod14can <- lm(taxnum ~ religionlink + union+ Male + 
                 agecat +  marital + Bachelor +incnum + taxarg + jl2 + religion, 
               data = subset(can, religion %in%canRichRels & religion != "Non-religious"), 
               weights = weight)

# List of poor canadian ethnicities (all but white), leaving out other 
## See here: https://www12.statcan.gc.ca/nhs-enm/2011/dp-pd/dt-td/Rp-eng.cfm?TABID=2&LANG=E&A=R&APATH=3&DETAIL=0&DIM=0&FL=A&FREE=0&GC=01&GL=-1&GID=1142052&GK=1&GRP=0&O=D&PID=106746&PRID=0&PTYPE=105277&S=0&SHOWALL=0&SUB=0&Temporal=2013&THEME=98&VID=0&VNAMEE=&VNAMEF=&D1=0&D2=0&D3=0&D4=0&D5=0&D6=0
## 2011 data 


mod15can <- lm(taxnum ~ racelink + union+ Male + 
                 agecat +  marital + Bachelor +incnum + taxarg + jl2 + race, 
               data = subset(can, race != "White" & race != "Other"), 
               weights = weight)
mod16can <- lm(taxnum ~ racelink + union+ Male + 
                 agecat +  marital + Bachelor +incnum + taxarg + jl2, 
               data = subset(can, race =="White"), 
               weights = weight)

## List of poor/rich Canadian provinces
## From RA calulations using LAD

richcanprovs <- c("Alberta", "Ontario", "Saskatchewan")
poorcanprovs <- unique(can$province)[is.na(match(unique(can$province), richcanprovs))]

mod17can <- lm(taxnum ~ regionlink + union+ Male + 
                 agecat +  marital + Bachelor +incnum + jl2 + taxarg + province, 
               data = subset(can, province %in% poorcanprovs), 
               weights = weight)
mod18can <- lm(taxnum ~ regionlink + union+ Male + 
                 agecat +  marital + Bachelor +incnum + jl2 + taxarg + province, 
               data = subset(can, province %in% richcanprovs), 
               weights = weight)


stargazer(list(mod15can, mod16can, mod13can, mod14can, mod17can, mod18can),
          out = "Figs/TaxSubgrpsCan.html",
          keep = c("racelink", "religionlink", "regionlink"),  
          type = "html", 
          align = TRUE,
          covariate.labels = c("Ethnic linked fate", "Religious linked fate", 
                               "Regional linked fate"),
          dep.var.labels = c(""),
          column.labels = c("Poor", "Rich", "Poor", "Rich", "Poor", "Rich"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          star.cutoffs = c(0.1, 0.05, 0.01))

stargazer(list(mod15can, mod16can, mod13can, mod14can, mod17can, mod18can),
          out = "Figs/TaxSubgrpsCan.tex",
          keep = c("racelink", "religionlink", "regionlink"),  
          type = "latex", 
          align = TRUE,
          covariate.labels = c("Ethnic linked fate", "Religious linked fate", 
                               "Regional linked fate"),
          dep.var.labels = c(""),
          column.labels = c("Poor", "Rich", "Poor", "Rich", "Poor", "Rich"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          star.cutoffs = c(0.1, 0.05, 0.01))






## List of poor/rich uk religious groups: 
## See mean wage in Table 3 of
## http://csi.nuff.ox.ac.uk/wp-content/uploads/2015/03/religion-and-poverty-working-paper.pdf

ukpoorRels <- c("Muslim", "Sikh")
ukrichRels <- unique(uk$religion)[
  !(unique(uk$religion) %in% ukpoorRels)]

mod13uk <- lm(taxnum ~ religionlink + union+ Male + 
                agecat +  marital + Bachelor +incnum + taxarg + jl2 + religion, 
              data = subset(uk, religion %in%ukpoorRels), 
              weights = weight)
mod14uk <- lm(taxnum ~ religionlink + union+ Male + 
                agecat +  marital + Bachelor +incnum + taxarg + jl2 + religion, 
              data = subset(uk, religion %in%ukrichRels& religion != "Non-religious" & 
                              religion != "" & religion != "Other"), 
              weights = weight)



## List of poor UK ethnic groups
## See here https://www.ethnicity-facts-figures.service.gov.uk/work-pay-and-benefits/pay-and-income/household-income/latest
## Assuming both Black categories are poorer than avg. Dropping white irish & other white

UKpoorEths <- c("Bangladeshi", "Black African", "Black Caribbean", "Pakistani")
UKRichEths <- c("Chinese", "Indian", 
                "White - British/English/Welsh/Scottish/Northern Irish")


mod15uk <- lm(taxnum ~ racelink + union+ Male + 
                agecat +  marital + Bachelor +incnum + taxarg + jl2 + race, 
              data = subset(uk, race %in% UKpoorEths), 
              weights = weight)
mod16uk <- lm(taxnum ~ racelink + union+ Male + 
                agecat +  marital + Bachelor +incnum + taxarg + jl2 + race, 
              data = subset(uk, race %in% UKRichEths), 
              weights = weight)





## List of poor and rich UK regions come from Eurostat 2015 NUTS 2 data
## https://ec.europa.eu/eurostat/web/regions/data/database


ukRichRegions <- c("East of England", 
                   "South East", 
                   "Greater London") 
ukPoorRegions <- unique(uk$region)[
  !(unique(uk$region) %in% ukRichRegions)]


mod17uk <- lm(taxnum ~ regionlink + union+ Male + 
                agecat +  marital + Bachelor +incnum + jl2 + taxarg + region, 
              data = subset(uk, region %in% ukPoorRegions), 
              weights = weight)
mod18uk <- lm(taxnum ~ regionlink + union+ Male + 
                agecat +  marital + Bachelor +incnum + jl2 + taxarg + region, 
              data = subset(uk, region %in% ukRichRegions), 
              weights = weight)


stargazer(list(mod15uk, mod16uk, mod13uk, mod14uk, mod17uk, mod18uk),
          out = "Figs/TaxSubgrpsuk.html",
          keep = c("racelink", "religionlink", "regionlink"),  
          type = "html", 
          align = TRUE,
          covariate.labels = c("Ethnic linked fate", "Religious linked fate", 
                               "Regional linked fate"),
          dep.var.labels = c(""),
          column.labels = c("Poor", "Rich", "Poor", "Rich", "Poor", "Rich"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          star.cutoffs = c(0.1, 0.05, 0.01))

stargazer(list(mod15uk, mod16uk, mod13uk, mod14uk, mod17uk, mod18uk),
          out = "Figs/TaxSubgrpsuk.tex",
          keep = c("racelink", "religionlink", "regionlink"),  
          type = "latex", 
          align = TRUE,
          covariate.labels = c("Ethnic linked fate", "Religious linked fate", 
                               "Regional linked fate"),
          dep.var.labels = c(""),
          column.labels = c("Poor", "Rich", "Poor", "Rich", "Poor", "Rich"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          star.cutoffs = c(0.1, 0.05, 0.01))







## List of poor and rich German regions come from Eurostat 2015 NUTS 2 data
## https://ec.europa.eu/eurostat/web/regions/data/database



GerRichRegions <- c("Baden-Wurttemberg", 
                    "Bavaria", 
                    "Hamburg", 
                    "Hesse", 
                    "Rhineland-Palatinate") ## Note, there are a few very close to the average, 
## including some that are thought of as rich (NRW)
GerPoorRegions <- unique(ger$region)[
  !(unique(ger$region) %in% GerRichRegions)]


mod17ger <- lm(taxnum ~ regionlink + union+ Male + 
                 agecat +  marital + Bachelor +incnum + jl2 + taxarg + region, 
               data = subset(ger, region %in% GerPoorRegions), 
               weights = weight)
mod18ger <- lm(taxnum ~ regionlink + union+ Male + 
                 agecat +  marital + Bachelor +incnum + jl2 + taxarg + region, 
               data = subset(ger, region %in% GerRichRegions), 
               weights = weight)




## Data on eth in germany hard to come by. Dropping other and no answer, assuming
## Germans are rich and others are poor

gerpoorEths <- subset(unique(ger$race), unique(ger$race) != "Germans" & 
                        unique(ger$race) != "No information" &
                        unique(ger$race) != "Other")
gerRichEths <- c("Germans")


mod15ger <- lm(taxnum ~ racelink + union+ Male + 
                 agecat +  marital + Bachelor +incnum + taxarg + jl2 + race, 
               data = subset(ger, race %in% gerpoorEths), 
               weights = weight)
mod16ger <- lm(taxnum ~ racelink + union+ Male + 
                 agecat +  marital + Bachelor +incnum + taxarg + jl2, 
               data = subset(ger, race %in% gerRichEths), 
               weights = weight)



## Data on German religious income hard to come by as well. 
## Treating only muslims as poor and only Cath/Prot as rich


gerpoorRels <- c("Muslim", "Hindu", "Sikh", "Buddhist")
gerrichRels <- c("Catholic", "Protestant")

mod13ger <- lm(taxnum ~ religionlink + union+ Male + 
                 agecat +  marital + Bachelor +incnum + taxarg + jl2 + religion, 
               data = subset(ger, religion %in%gerpoorRels), 
               weights = weight)
mod14ger <- lm(taxnum ~ religionlink + union+ Male + 
                 agecat +  marital + Bachelor +incnum + taxarg + jl2 + religion, 
               data = subset(ger, religion %in%gerrichRels), 
               weights = weight)

stargazer(list(mod15ger, mod16ger, mod13ger, mod14ger, mod17ger, mod18ger),
          out = "Figs/TaxSubgrpsger.html",
          keep = c("racelink", "religionlink", "regionlink"),  
          type = "html", 
          align = TRUE,
          covariate.labels = c("Ethnic linked fate", "Religious linked fate", 
                               "Regional linked fate"),
          dep.var.labels = c(""),
          column.labels = c("Poor", "Rich", "Poor", "Rich", "Poor", "Rich"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          star.cutoffs = c(0.1, 0.05, 0.01))

stargazer(list(mod15ger, mod16ger, mod13ger, mod14ger,mod17ger, mod18ger),
          out = "Figs/TaxSubgrpsger.tex",
          keep = c("racelink", "religionlink", "regionlink"),  
          type = "latex", 
          align = TRUE,
          covariate.labels = c("Ethnic linked fate", "Religious linked fate", 
                               "Regional linked fate"),
          dep.var.labels = c(""),
          column.labels = c("Poor", "Rich", "Poor", "Rich", "Poor", "Rich"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          star.cutoffs = c(0.1, 0.05, 0.01))



## Now turning these into figures and a set of pooled effects
canpoorEths <- levels(can$race)[levels(can$race) != "White" & 
                                  levels(can$race) != "Other"]
canrichEths <- c("White")  


all <- smartbind(uk, ger, can)

mod13all <- lm(taxnum ~ religionlink + union+ Male + 
                 agecat +  marital + Bachelor +incnum + taxarg + jl2 + religion + Country, 
               data = subset(all, religion %in% c(gerpoorRels, canpoorRels, ukpoorRels)), 
               weights = weight)
mod14all <- lm(taxnum ~ religionlink + union+ Male + 
                 agecat +  marital + Bachelor +incnum + taxarg + jl2 + religion + Country, 
               data = subset(all, religion %in%c(gerrichRels, ukrichRels, canRichRels)), 
               weights = weight)

mod15all <- lm(taxnum ~ racelink + union+ Male + 
                 agecat +  marital + Bachelor +incnum + taxarg + jl2 + race + Country, 
               data = subset(all, race %in% c(gerpoorEths, UKpoorEths, canpoorEths)), 
               weights = weight)
mod16all <- lm(taxnum ~ racelink + union+ Male + 
                 agecat +  marital + Bachelor +incnum + taxarg + jl2 + Country, 
               data = subset(all, race %in% c(gerRichEths, UKRichEths, canrichEths)), 
               weights = weight)


mod17all <- lm(taxnum ~ regionlink + union+ Male + 
                 agecat +  marital + Bachelor +incnum + jl2 + taxarg + region, 
               data = subset(all, region %in% c(as.character(GerPoorRegions), 
                                                ukPoorRegions, poorcanprovs)), 
               weights = weight)
mod18all <- lm(taxnum ~ regionlink + union+ Male + 
                 agecat +  marital + Bachelor +incnum + jl2 + taxarg + region, 
               data = subset(all, region %in% c(GerRichRegions, ukRichRegions, richcanprovs)), 
               weights = weight)











regeffsub <- data.frame(Country = rep(NA, 24), 
                        Group = rep(NA, 24), 
                        Mean = rep(NA, 24), 
                        SD = rep(NA, 24),
                        Dimension = rep(NA, 24))


allsubmods <- list(mod15uk, mod16uk, mod13uk, mod14uk, mod17uk, mod18uk, 
                   mod15ger, mod16ger, mod13ger, mod14ger, mod17ger, mod18ger, 
                   mod15can, mod16can, mod13can, mod14can, mod17can, mod18can, 
                   mod15all, mod16all, mod13all, mod14all, mod17all, mod18all)

cntrys <- c(rep("UK", 6), rep("Germany", 6), rep("Canada", 6), rep("Pooled", 6))
grps <- rep(c("Poor Groups", "Rich Groups"), 12)
dimens <- rep(c("Ethnic", "Ethnic", "Religious", "Religious", "Regional", "Regional"), 4)

for(i in 1:24){
  thismod <- allsubmods[[i]]
  regeffsub$Country[i] <- cntrys[i]
  regeffsub$Group[i] <- grps[i]
  regeffsub$Mean[i] <- coef(thismod)[2]
  regeffsub$SD[i] <- sqrt(vcov(thismod)[2,2])
  regeffsub$Dimension[i] <- dimens[i]
}


regeffsub$Upp <- regeffsub$Mean + 1.96*regeffsub$SD
regeffsub$Low <- regeffsub$Mean - 1.96*regeffsub$SD
regeffsub$Dimension <- factor(regeffsub$Dimension, levels = c("Regional", "Religious", "Ethnic"))
regeffsub$Country <- factor(regeffsub$Country, levels = c("UK", "Germany", "Canada", "Pooled"))

pdf("Figs/Figure2.pdf", width = 6, height = 3)
ggplot(regeffsub, aes(y = Mean, x = Dimension, group = Group)) + 
  facet_wrap(vars(Country), nrow = 1) + 
  geom_point(cex = 2, aes(pch = Group), position=position_dodge(width=0.1)) + 
  geom_errorbar(aes(ymin = Low, ymax = Upp, x = Dimension),
                position=position_dodge(width=0.1), 
                width = 0) + 
  xlab("") + theme(legend.position="none") + 
  ylab("Effect of Linked Fate on Redistributive Attitudes") + 
  # xlim(c(1, 4)) + 
  scale_fill_grey() + 
  theme_bw() + 
  coord_flip() + 
  geom_hline(yintercept = 0, lty = "dashed") + 
  scale_shape_manual(values=c(15, 16)) + 
  theme(legend.title = element_blank()) 

dev.off()
#######################################################################
## Appendix
#######################################################################
#######################################################################
## Descriptives
#######################################################################


can <- cbind(can, model.matrix(~agecat - 1, data = can))
can <- cbind(can, model.matrix(~union - 1, data = can))



varstosum <- c("taxnum", "racelink", "religionlink", "regionlink", "classlink", 
               "unionNot Employed", "unionYes", "relfreq", "moreno", 
               "Male", "marital", "Bachelor", "incnum",
               "joblossnum", "agecatAge1524", "agecatAge2544", 
               "agecatAge4564", "agecatAge65p", "weight")

sumnames <- c("Redistributive preference", "Ethnic linked fate", "Religious linked fate", 
              "Regional linked fate", "Class linked fate", "Not employed", "Union member", 
              "Religious attendance", "Regional ID", "Male", "Married", 
              "Bachelor's degree", "Income (1-7 scale)", "Job uncertainty", 
              "Age 18-24", "Age 25-44", "Age 45-64", "Age 65+", "Survey weight")

cansum <- subset(can, select = varstosum)
stargazer(cansum, 
          out = "figs/appendix/Cansum.html", 
          type = "html", 
          covariate.labels = sumnames,
          summary.stat = c("n", "mean", "min", "max"), 
          digits = 2)
stargazer(cansum, 
          out = "figs/appendix/Cansum.tex", 
          type = "latex", 
          covariate.labels = sumnames,
          summary.stat = c("n", "mean", "min", "max"), 
          digits = 2)



ger <- subset(ger, !is.na(ger$agecat))
ger <- cbind(ger, model.matrix(~agecat - 1, data = ger))
ger <- cbind(ger, model.matrix(~union - 1, data = ger))





gersum <- subset(ger, select = varstosum)
stargazer(gersum, 
          out = "figs/appendix/gersum.html", 
          type = "html", 
          covariate.labels = sumnames,
          summary.stat = c("n", "mean", "min", "max"), 
          digits = 2)
stargazer(gersum, 
          out = "figs/appendix/gersum.tex", 
          type = "latex", 
          covariate.labels = sumnames,
          summary.stat = c("n", "mean", "min", "max"), 
          digits = 2)


uk <- cbind(uk, model.matrix(~agecat - 1, data = uk))
uk <- cbind(uk, model.matrix(~union - 1, data = uk))

uksum <- subset(uk, select = varstosum)
stargazer(uksum, 
          out = "figs/appendix/uksum.html", 
          type = "html", 
          covariate.labels = sumnames,
          summary.stat = c("n", "mean", "min", "max"), 
          digits = 2)
stargazer(uksum, 
          out = "figs/appendix/uksum.tex", 
          type = "latex", 
          covariate.labels = sumnames,
          summary.stat = c("n", "mean", "min", "max"), 
          digits = 2)

#######################################################################
############### Now describing Canadian groups
canracemat <- cbind(table(can$race), 
                    c("Poor", "Poor", "Poor", "NA", "Poor", "Rich"))

canrelmat <- cbind(table(can$religion), 
                   c("NA", "Rich", "Poor", "Poor", "Rich", "Rich", "Poor", 
                     "NA", "NA", "Rich", "Rich", "Rich"))
canregmat <- cbind(table(can$province), 
                   c("Rich", "Poor", "Poor", "Poor", "Poor", "Poor", "Poor", 
                     "Rich", "Poor", "Poor", "Rich", "Poor"))
stargazer(canracemat, 
          out = "figs/appendix/canraces.html", 
          type = "html")
stargazer(canrelmat, 
          out = "figs/appendix/canrels.html", 
          type = "html")
stargazer(canregmat, 
          out = "figs/appendix/canregs.html", 
          type = "html")

stargazer(canracemat, 
          out = "figs/appendix/canraces.tex", 
          type = "latex")
stargazer(canrelmat, 
          out = "figs/appendix/canrels.tex", 
          type = "latex")
stargazer(canregmat, 
          out = "figs/appendix/canregs.tex", 
          type = "latex")


#######################################################################
############### Now describing German groups
gerracemat <- cbind(table(ger$race), 
                    c("Poor", "Poor", "Poor", "Rich", "Poor", "Poor", "NA", "NA", 
                      "Poor", "Poor", "Poor", "Poor", "Poor"))

gerrelmat <- cbind(table(ger$religion), 
                   c("NA", "NA", "Rich", "Poor", "NA", "Poor", "NA", 
                     "NA", "Rich", "Poor"))
gerregmat <- cbind(table(ger$region), 
                   c("Rich", "Rich", "Poor", "Poor", "Poor", "Rich", 
                     "Rich",  "Poor", "Poor", "Poor", "Rich", "Poor", 
                     "Poor", "Poor", "Poor", "Poor"))




stargazer(gerracemat, 
          out = "figs/appendix/gerraces.html", 
          type = "html")
stargazer(gerrelmat, 
          out = "figs/appendix/gerrels.html", 
          type = "html")
stargazer(gerregmat, 
          out = "figs/appendix/gerregs.html", 
          type = "html")

stargazer(gerracemat, 
          out = "figs/appendix/gerraces.tex", 
          type = "latex")
stargazer(gerrelmat, 
          out = "figs/appendix/gerrels.tex", 
          type = "latex")
stargazer(gerregmat, 
          out = "figs/appendix/gerregs.tex", 
          type = "latex")

#######################################################################
############### Now describing UK groups
ukracemat <- cbind(table(uk$race), 
                   c("Poor", "Poor", "Poor", "Rich", "Rich", "NA", 
                     "NA", "Poor", "Rich", "NA"))

ukrelmat <- cbind(table(uk$religion), 
                  c("NA", "Rich", "Rich", "Rich", "Rich", "Rich", "Poor", 
                    "NA", "NA", "Rich", "Poor"))
ukregmat <- cbind(table(uk$region), 
                  c("Poor", "Rich", "Rich", "Poor", "Poor", "Poor", "Poor", 
                    "Rich", "Poor", "Poor", "Poor", "Poor"))
stargazer(ukracemat, 
          out = "figs/appendix/ukraces.html", 
          type = "html")
stargazer(ukrelmat, 
          out = "figs/appendix/ukrels.html", 
          type = "html")
stargazer(ukregmat, 
          out = "figs/appendix/ukregs.html", 
          type = "html")

stargazer(ukracemat, 
          out = "figs/appendix/ukraces.tex", 
          type = "latex")
stargazer(ukrelmat, 
          out = "figs/appendix/ukrels.tex", 
          type = "latex")
stargazer(ukregmat, 
          out = "figs/appendix/ukregs.tex", 
          type = "latex")


#######################################################################
## Unweighted
#######################################################################
mod1allUW <- lm(racelink ~ joblossnum + Male + agecat + union + marital + Bachelor +incnum + race, 
                data = allworking)
mod2allUW <- lm(religionlink ~ joblossnum + Male + agecat + union+ marital + religion + Bachelor +incnum, 
                data = allworking)
mod3allUW <- lm(regionlink ~ joblossnum + Male + agecat + union+ marital + region + Bachelor +incnum, 
                data = allworking)
mod4allUW <- lm(classlink ~ joblossnum + Male + agecat + union+ marital + Bachelor +incnum, 
                data = allworking)


mod1ukUW <- lm(racelink ~ joblossnum + Male + agecat + union + marital + educsimple +incnum + race, 
               data = ukworking)
mod2ukUW <- lm(religionlink ~ joblossnum + Male + agecat + union+ marital + religion + educsimple +incnum, 
               data = ukworking)
mod3ukUW <- lm(regionlink ~ joblossnum + Male + agecat + union+ marital + region + educsimple +incnum, 
               data = ukworking)
mod4ukUW <- lm(classlink ~ joblossnum + Male + agecat + union+ marital + educsimple +incnum, 
               data = ukworking)


mod1canUW <- lm(racelink ~ joblossnum + Male + agecat + union+ marital + race + educsimple +incnum, 
                data = canworking)
mod2canUW <- lm(religionlink ~ joblossnum + Male + agecat + union+ marital + religion + educsimple +incnum, 
                data = canworking)
mod3canUW <- lm(regionlink ~ joblossnum + Male + agecat + union+ marital + region + educsimple +incnum, 
                data = canworking)
mod4canUW <- lm(classlink ~ joblossnum + Male + agecat + union+ marital +  educsimple +incnum, 
                data = canworking)

mod1gerUW <- lm(racelink ~ joblossnum + Male + agecat + union+ marital + educsimple +incnum + race, 
                data = gerworking)
mod2gerUW <- lm(religionlink ~ joblossnum + Male + agecat + union+ marital+ educsimple + religion +incnum , 
                data = gerworking)
mod3gerUW <- lm(regionlink ~ joblossnum + Male + agecat + union+ marital + educsimple + region +incnum, 
                data = gerworking)
mod4gerUW <- lm(classlink ~ joblossnum + Male + agecat + union+ marital+ educsimple + incnum, 
                data = gerworking)

keycovs <- names(coef(mod1uk))[2:11]
cov.labs <- c("Likelihood of Job Loss", "Male", "Age 25-44", "Age 45-64", "Age 65+", 
              "Union", "Married", "High School", "Some Post-Secondary", "Income (1-7)")

keycovsG <- names(coef(mod1ger))[2:10]  ## No post-sec ed for germany
cov.labsG <- c(cov.labs[1:7], "Less than Bachelor's", "Income")

stargazer(list(mod1ukUW, mod2ukUW, mod3ukUW, mod4ukUW),
          out = "figs/appendix/UKjoblossUW.tex",
          keep = keycovs, 
          type = "latex", 
          align = TRUE,
          covariate.labels = cov.labs,
          dep.var.labels = c("Ethnicity", "Religion", "Region", "Class"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          star.cutoffs = c(0.1, 0.05, 0.01), 
          add.lines = list(c("Group FE", "Yes", "Yes", "Yes", "No")))

stargazer(list(mod1canUW, mod2canUW, mod3canUW, mod4canUW),
          out = "figs/appendix/CAjoblossUW.tex",
          keep = keycovs, 
          type = "latex", 
          align = TRUE,
          covariate.labels = cov.labs,
          dep.var.labels = c("Ethnicity", "Religion", "Region", "Class"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          add.lines = list(c("Group FE", "Yes", "Yes", "Yes", "No")),
          star.cutoffs = c(0.1, 0.05, 0.01))


stargazer(list(mod1gerUW, mod2gerUW, mod3gerUW, mod4gerUW),
          out = "figs/appendix/GerjoblossUW.tex",
          keep = keycovsG, 
          type = "latex", 
          align = TRUE,
          covariate.labels = cov.labsG,
          dep.var.labels = c("Ethnicity", "Religion", "Region", "Class"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          add.lines = list(c("Group FE", "Yes", "Yes", "Yes", "No")),
          star.cutoffs = c(0.1, 0.05, 0.01))


mod6canUW <- lm(religionlink ~ relfreq +  Male + agecat + union+ marital + Bachelor +incnum + religion, 
                data = can)
mod6ukUW <- lm(religionlink ~ relfreq + Male + agecat + union+ marital  + Bachelor +incnum+ religion, 
               data = uk)
mod6gerUW <- lm(religionlink ~ relfreq +  Male + agecat + union+ marital + Bachelor +incnum+ religion , 
                data = ger)

mod6allUW <- lm(religionlink ~ relfreq +  Male + agecat + union+ marital + Bachelor +incnum+ religion , 
                data = all)


newcovs2 <- c("relfreq",  "Male",  "AgecatAge2544", "agecatAge4564", "agecatAge65p", 
              "unionNotEmployed", "unionYes",  "marital", "Bachelor", "incnum")
newcovlabs2 <- c("Religious attendance", "Male", "Age 25-44", "Age 45-64", "Age 65+", 
                 "Not working", "Union",  "Married", "Bachelor's degree", "Income (1-7)")

stargazer(list(mod6allUW, mod6canUW, mod6ukUW, mod6gerUW),
          out = "figs/appendix/relFreq3countryUW.tex",
          keep = newcovs2, 
          type = "latex", 
          align = TRUE,
          covariate.labels = newcovlabs2,
          dep.var.labels = c(""),
          column.labels = c("All", "Canada", "UK", "Germany"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          star.cutoffs = c(0.1, 0.05, 0.01), 
          add.lines = list(c("Religion FE", "Yes", "Yes", "Yes", "Yes")))


#########################

mod13canUW <- lm(taxnum ~ religionlink + union+ Male + 
                   agecat +  marital + Bachelor +incnum + taxarg + jl2 + religion, 
                 data = subset(can, religion %in%canpoorRels & religion != "Non-religious"))
mod14canUW <- lm(taxnum ~ religionlink + union+ Male + 
                   agecat +  marital + Bachelor +incnum + taxarg + jl2 + religion, 
                 data = subset(can, religion %in%canRichRels & religion != "Non-religious"))
mod15canUW <- lm(taxnum ~ racelink + union+ Male + 
                   agecat +  marital + Bachelor +incnum + taxarg + jl2 + race, 
                 data = subset(can, race != "White" & race != "Other"))
mod16canUW <- lm(taxnum ~ racelink + union+ Male + 
                   agecat +  marital + Bachelor +incnum + taxarg + jl2, 
                 data = subset(can, race =="White"))
mod17canUW <- lm(taxnum ~ regionlink + union+ Male + 
                   agecat +  marital + Bachelor +incnum + jl2 + taxarg + province, 
                 data = subset(can, province %in% poorcanprovs))
mod18canUW <- lm(taxnum ~ regionlink + union+ Male + 
                   agecat +  marital + Bachelor +incnum + jl2 + taxarg + province, 
                 data = subset(can, province %in% richcanprovs))

mod13ukUW <- lm(taxnum ~ religionlink + union+ Male + 
                  agecat +  marital + Bachelor +incnum + taxarg + jl2 + religion, 
                data = subset(uk, religion %in%ukpoorRels))
mod14ukUW <- lm(taxnum ~ religionlink + union+ Male + 
                  agecat +  marital + Bachelor +incnum + taxarg + jl2 + religion, 
                data = subset(uk, religion %in%ukrichRels& religion != "Non-religious" & 
                                religion != "" & religion != "Other"))
mod15ukUW <- lm(taxnum ~ racelink + union+ Male + 
                  agecat +  marital + Bachelor +incnum + taxarg + jl2 + race, 
                data = subset(uk, race %in% UKpoorEths))
mod16ukUW <- lm(taxnum ~ racelink + union+ Male + 
                  agecat +  marital + Bachelor +incnum + taxarg + jl2 + race, 
                data = subset(uk, race %in% UKRichEths))
mod17ukUW <- lm(taxnum ~ regionlink + union+ Male + 
                  agecat +  marital + Bachelor +incnum + jl2 + taxarg + region, 
                data = subset(uk, region %in% ukPoorRegions))
mod18ukUW <- lm(taxnum ~ regionlink + union+ Male + 
                  agecat +  marital + Bachelor +incnum + jl2 + taxarg + region, 
                data = subset(uk, region %in% ukRichRegions))



mod13gerUW <- lm(taxnum ~ religionlink + union+ Male + 
                   agecat +  marital + Bachelor +incnum + taxarg + jl2 + religion, 
                 data = subset(ger, religion %in%gerpoorRels))
mod14gerUW <- lm(taxnum ~ religionlink + union+ Male + 
                   agecat +  marital + Bachelor +incnum + taxarg + jl2 + religion, 
                 data = subset(ger, religion %in%gerrichRels))

mod15gerUW <- lm(taxnum ~ racelink + union+ Male + 
                   agecat +  marital + Bachelor +incnum + taxarg + jl2 + race, 
                 data = subset(ger, race %in% gerpoorEths))
mod16gerUW <- lm(taxnum ~ racelink + union+ Male + 
                   agecat +  marital + Bachelor +incnum + taxarg + jl2, 
                 data = subset(ger, race %in% gerRichEths))

mod17gerUW <- lm(taxnum ~ regionlink + union+ Male + 
                   agecat +  marital + Bachelor +incnum + jl2 + taxarg + region, 
                 data = subset(ger, region %in% GerPoorRegions))
mod18gerUW <- lm(taxnum ~ regionlink + union+ Male + 
                   agecat +  marital + Bachelor +incnum + jl2 + taxarg + region, 
                 data = subset(ger, region %in% GerRichRegions))




mod13allUW <- lm(taxnum ~ religionlink + union+ Male + 
                   agecat +  marital + Bachelor +incnum + taxarg + jl2 + religion + Country, 
                 data = subset(all, religion %in% c(gerpoorRels, canpoorRels, ukpoorRels)))
mod14allUW <- lm(taxnum ~ religionlink + union+ Male + 
                   agecat +  marital + Bachelor +incnum + taxarg + jl2 + religion + Country, 
                 data = subset(all, religion %in%c(gerrichRels, ukrichRels, canRichRels)))

mod15allUW <- lm(taxnum ~ racelink + union+ Male + 
                   agecat +  marital + Bachelor +incnum + taxarg + jl2 + race + Country, 
                 data = subset(all, race %in% c(gerpoorEths, UKpoorEths, canpoorEths)))
mod16allUW <- lm(taxnum ~ racelink + union+ Male + 
                   agecat +  marital + Bachelor +incnum + taxarg + jl2 + Country, 
                 data = subset(all, race %in% c(gerRichEths, UKRichEths, canrichEths)))


mod17allUW <- lm(taxnum ~ regionlink + union+ Male + 
                   agecat +  marital + Bachelor +incnum + jl2 + taxarg + region, 
                 data = subset(all, region %in% c(as.character(GerPoorRegions), 
                                                  ukPoorRegions, poorcanprovs)))
mod18allUW <- lm(taxnum ~ regionlink + union+ Male + 
                   agecat +  marital + Bachelor +incnum + jl2 + taxarg + region, 
                 data = subset(all, region %in% c(GerRichRegions, ukRichRegions, richcanprovs)))











regeffsub <- data.frame(Country = rep(NA, 24), 
                        Group = rep(NA, 24), 
                        Mean = rep(NA, 24), 
                        SD = rep(NA, 24),
                        Dimension = rep(NA, 24))


allsubmods <- list(mod15ukUW, mod16ukUW, mod13ukUW, mod14ukUW, mod17ukUW, mod18ukUW, 
                   mod15gerUW, mod16gerUW, mod13gerUW, mod14gerUW, mod17gerUW, mod18gerUW, 
                   mod15canUW, mod16canUW, mod13canUW, mod14canUW, mod17canUW, mod18canUW, 
                   mod15allUW, mod16allUW, mod13allUW, mod14allUW, mod17allUW, mod18allUW)

cntrys <- c(rep("UK", 6), rep("Germany", 6), rep("Canada", 6), rep("Pooled", 6))
grps <- rep(c("Poor Groups", "Rich Groups"), 12)
dimens <- rep(c("Ethnic", "Ethnic", "Religious", "Religious", "Regional", "Regional"), 4)

for(i in 1:24){
  thismod <- allsubmods[[i]]
  regeffsub$Country[i] <- cntrys[i]
  regeffsub$Group[i] <- grps[i]
  regeffsub$Mean[i] <- coef(thismod)[2]
  regeffsub$SD[i] <- sqrt(vcov(thismod)[2,2])
  regeffsub$Dimension[i] <- dimens[i]
}


regeffsub$Upp <- regeffsub$Mean + 1.96*regeffsub$SD
regeffsub$Low <- regeffsub$Mean - 1.96*regeffsub$SD
regeffsub$Dimension <- factor(regeffsub$Dimension, levels = c("Regional", "Religious", "Ethnic"))
regeffsub$Country <- factor(regeffsub$Country, levels = c("UK", "Germany", "Canada", "Pooled"))

pdf("figs/appendix/SubgroupTaxPlotsUW.pdf", width = 6, height = 3)
ggplot(regeffsub, aes(y = Mean, x = Dimension, group = Group)) + 
  facet_wrap(vars(Country), nrow = 1) + 
  geom_point(cex = 2, aes(pch = Group), position=position_dodge(width=0.1)) + 
  geom_errorbar(aes(ymin = Low, ymax = Upp, x = Dimension),
                position=position_dodge(width=0.1), 
                width = 0) + 
  xlab("") + theme(legend.position="none") + 
  ylab("Effect of Linked Fate on Redistributive Attitudes") + 
  # xlim(c(1, 4)) + 
  scale_fill_grey() + 
  theme_bw() + 
  coord_flip() + 
  geom_hline(yintercept = 0, lty = "dashed") + 
  scale_shape_manual(values=c(15, 16)) + 
  theme(legend.title = element_blank())

dev.off()



## Union and link


mod8canUW <- lm(classlink ~  union+Male + agecat + marital + Bachelor +incnum, 
                data = subset(can, respondent_class == "working class"))
mod8ukUW <- lm(classlink ~ union+ Male + agecat + marital  + Bachelor +incnum, 
               data = subset(uk, respondent_class == "working class"))
mod8gerUW <- lm(classlink ~   union+ Male + agecat +  marital + Bachelor +incnum, 
                data = subset(ger, respondent_class == "working class"))

mod8allUW <- lm(classlink ~   union+ Male + agecat +  marital + Bachelor +incnum, 
                data = subset(all, respondent_class == "working class"))



newcovs3 <- c("unionNot Employed", "unionYes", "Male",  "agecatAge2544", "agecatAge4564", "agecatAge65p", 
              "marital", "Bachelor", "incnum")
newcovlabs3 <- c( "Not working", "Union",  "Male", "Age 25-44", "Age 45-64", "Age 65+", 
                  "Married", "Bachelor's degree", "Income (1-7)")
stargazer(list(mod8allUW, mod8canUW, mod8ukUW, mod8gerUW),
          out = "figs/appendix/classUnion3countryUW.tex",
          keep = newcovs3, 
          type = "latex", 
          align = TRUE,
          covariate.labels = newcovlabs3,
          dep.var.labels = c(""),
          column.labels = c("All", "Canada", "UK", "Germany"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          star.cutoffs = c(0.1, 0.05, 0.01))

## Moreno and link



mod5canUW <- lm(regionlink ~ moreno +  Male + agecat + union+ marital + Bachelor +incnum + province, 
                data = can)
mod5ukUW <- lm(regionlink ~ moreno + Male + agecat + union+ marital  + Bachelor +incnum+ region, 
               data = uk)
mod5gerUW <- lm(regionlink ~ moreno +  Male + agecat + union+ marital + Bachelor +incnum+ region , 
                data = ger)
mod5allUW  <- lm(regionlink ~ moreno +  Male + agecat + union+ marital + Bachelor +incnum+ region , 
                 data = all)



newcovs <- c("moreno",  "Male",  "AgecatAge2544", "agecatAge4564", "agecatAge65p", 
             "unionYes", "unionNotEmployed", "marital", "Bachelor", "incnum")
newcovlabs <- c("Regional ID", "Male", "Age 25-44", "Age 45-64", "Age 65+", 
                "Not working", "Union", "Married", "Bachelor's degree", "Income (1-7)")
stargazer(list(mod5allUW, mod5canUW, mod5ukUW, mod5gerUW),
          out = "figs/appendix/regMoreno3countryUW.tex",
          keep = newcovs, 
          type = "latex", 
          align = TRUE,
          covariate.labels = newcovlabs,
          dep.var.labels = c(""),
          column.labels = c("All", "Canada", "UK", "Germany"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          star.cutoffs = c(0.1, 0.05, 0.01), 
          add.lines = list(c("Region FE", "Yes", "Yes", "Yes", "Yes")))


## Priming groups

mod9canUW <- lm(racelink ~ taxarg, data = subset(can, taxarg != "Reg"))
mod9ukUW <- lm(racelink ~ taxarg, data = subset(uk, taxarg != "Reg"))
mod9gerUW <- lm(racelink ~ taxarg, data = subset(ger, taxarg != "Reg"))


mod10canUW <- lm(regionlink ~ taxarg, data = subset(can, taxarg != "Eth"))
mod10gerUW <- lm(regionlink ~ taxarg, data = subset(ger, taxarg != "Eth"))


stargazer(list(mod9canUW,  mod9ukUW, mod9gerUW, mod10canUW, mod10gerUW),
          out = "figs/appendix/TaxToLinkUW.tex",
          # keep = c("taxargEth", "taxargReg"),  
          type = "latex", 
          align = TRUE,
          covariate.labels = c("Ethnic prime", "Regional prime"),
          dep.var.labels = c("Ethnicity", "Region"),
          column.labels = c("Canada", "UK", "Germany", "Canada", "Germany"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          star.cutoffs = c(0.1, 0.05, 0.01))


mod11canUW <- lm(racelink ~ taxarg, data = 
                   subset(can, race != "White" & race != "Other" & taxarg != "Reg"))
mod11ukUW <- lm(racelink ~ taxarg, 
                data = subset(uk, race != "White - British/English/Welsh/Scottish/Northern Irish"), 
                weights = weight)
mod11gerUW <- lm(racelink ~ taxarg, data = subset(ger, race != "Germans" & taxarg != "Reg"))


mod12canUW <- lm(regionlink ~ taxarg, 
                 data = subset(can, province != "Ontario" & province != "British Columbia" & taxarg != "Eth"))
mod12gerUW <- lm(regionlink ~ taxarg, 
                 data = subset(ger, region != "Berlin" & region != "Bavaria" & taxarg != "Eth"))

stargazer(list(mod11canUW,  mod11ukUW, mod11gerUW, mod12canUW, mod12gerUW),
          out = "figs/appendix/TaxToLinkMinoritiesOnlyUW.tex",
          #   keep = c("taxargEth", "taxargReg"),  
          type = "latex", 
          align = TRUE,
          covariate.labels = c("Ethnic prime", "Regional prime"),
          dep.var.labels = c("Ethnicity", "Region"),
          column.labels = c("Canada", "UK", "Germany", "Canada", "Germany"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          star.cutoffs = c(0.1, 0.05, 0.01), 
          add.lines = list(c("Excludes", 
                             "White", "White British", 
                             "Germans", "Ont/BC", 
                             "Bavaria/Berlin")))


#######################################################################
## No controls
#######################################################################

mod1allNC <- lm(racelink ~ joblossnum, 
                data = allworking, weight = weight)
mod2allNC <- lm(religionlink ~ joblossnum, 
                data = allworking, weight = weight)
mod3allNC <- lm(regionlink ~ joblossnum, 
                data = allworking, weight = weight)
mod4allNC <- lm(classlink ~ joblossnum, 
                data = allworking, weight = weight)


mod1ukNC <- lm(racelink ~ joblossnum, 
               data = ukworking, weight = weight)
mod2ukNC <- lm(religionlink ~ joblossnum, 
               data = ukworking, weight = weight)
mod3ukNC <- lm(regionlink ~ joblossnum, 
               data = ukworking, weight = weight)
mod4ukNC <- lm(classlink ~ joblossnum, 
               data = ukworking, weight = weight)


mod1canNC <- lm(racelink ~ joblossnum, 
                data = canworking, weight = weight)
mod2canNC <- lm(religionlink ~ joblossnum, 
                data = canworking, weight = weight)
mod3canNC <- lm(regionlink ~ joblossnum, 
                data = canworking, weight = weight)
mod4canNC <- lm(classlink ~ joblossnum, 
                data = canworking, weight = weight)

mod1gerNC <- lm(racelink ~ joblossnum, 
                data = gerworking, weight = weight)
mod2gerNC <- lm(religionlink ~ joblossnum, 
                data = gerworking, weight = weight)
mod3gerNC <- lm(regionlink ~ joblossnum, 
                data = gerworking, weight = weight)
mod4gerNC <- lm(classlink ~ joblossnum, 
                data = gerworking, weight = weight)

keycovs <- keycovsG <- names(coef(mod1uk))[2:11]
cov.labs <- c("Likelihood of Job Loss")



stargazer(list(mod1ukNC, mod2ukNC, mod3ukNC, mod4ukNC),
          out = "figs/appendix/UKjoblossNC.tex",
          keep = keycovs, 
          type = "latex", 
          align = TRUE,
          covariate.labels = cov.labs,
          dep.var.labels = c("Ethnicity", "Religion", "Region", "Class"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          star.cutoffs = c(0.1, 0.05, 0.01), 
          add.lines = list(c("Group FE", "No", "No", "No", "No")))

stargazer(list(mod1canNC, mod2canNC, mod3canNC, mod4canNC),
          out = "figs/appendix/CAjoblossNC.tex",
          keep = keycovs, 
          type = "latex", 
          align = TRUE,
          covariate.labels = cov.labs,
          dep.var.labels = c("Ethnicity", "Religion", "Region", "Class"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          add.lines = list(c("Group FE", "No", "No", "No", "No")),
          star.cutoffs = c(0.1, 0.05, 0.01))


stargazer(list(mod1gerNC, mod2gerNC, mod3gerNC, mod4gerNC),
          out = "figs/appendix/GerjoblossNC.tex",
          keep = keycovsG, 
          type = "latex", 
          align = TRUE,
          covariate.labels = cov.labsG,
          dep.var.labels = c("Ethnicity", "Religion", "Region", "Class"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          add.lines = list(c("Group FE", "No", "No", "No", "No")),
          star.cutoffs = c(0.1, 0.05, 0.01))


mod6canNC <- lm(religionlink ~ relfreq, 
                data = can, weight = weight)
mod6ukNC <- lm(religionlink ~ relfreq, 
               data = uk, weight = weight)
mod6gerNC <- lm(religionlink ~ relfreq, 
                data = ger, weight = weight)

mod6allNC <- lm(religionlink ~ relfreq, 
                data = all, weight = weight)


newcovs2 <- c("relfreq")
stargazer(list(mod6allNC, mod6canNC, mod6ukNC, mod6gerNC),
          out = "figs/appendix/relFreq3countryNC.tex",
          keep = newcovs2, 
          type = "latex", 
          align = TRUE,
          covariate.labels = newcovlabs2,
          dep.var.labels = c(""),
          column.labels = c("All", "Canada", "UK", "Germany"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          star.cutoffs = c(0.1, 0.05, 0.01), 
          add.lines = list(c("Religion FE", "No", "No", "No", "No")))


#########################

mod13canNC <- lm(taxnum ~ religionlink, 
                 data = subset(can, religion %in%canpoorRels & religion != "Non-religious"), weights = weight)
mod14canNC <- lm(taxnum ~ religionlink, 
                 data = subset(can, religion %in%canRichRels & religion != "Non-religious"), weights = weight)
mod15canNC <- lm(taxnum ~ racelink, 
                 data = subset(can, race != "White" & race != "Other"))
mod16canNC <- lm(taxnum ~ racelink, 
                 data = subset(can, race =="White"))
mod17canNC <- lm(taxnum ~ regionlink, 
                 data = subset(can, province %in% poorcanprovs), weights = weight)
mod18canNC <- lm(taxnum ~ regionlink, 
                 data = subset(can, province %in% richcanprovs), weights = weight)

mod13ukNC <- lm(taxnum ~ religionlink, 
                data = subset(uk, religion %in%ukpoorRels), weights = weight)
mod14ukNC <- lm(taxnum ~ religionlink, 
                data = subset(uk, religion %in%ukrichRels& religion != "Non-religious" & 
                                religion != "" & religion != "Other"), weights = weight)
mod15ukNC <- lm(taxnum ~ racelink, 
                data = subset(uk, race %in% UKpoorEths), weights = weight)
mod16ukNC <- lm(taxnum ~ racelink, 
                data = subset(uk, race %in% UKRichEths), weights = weight)
mod17ukNC <- lm(taxnum ~ regionlink, 
                data = subset(uk, region %in% ukPoorRegions), weights = weight)
mod18ukNC <- lm(taxnum ~ regionlink, 
                data = subset(uk, region %in% ukRichRegions), weights = weight)



mod13gerNC <- lm(taxnum ~ religionlink, 
                 data = subset(ger, religion %in%gerpoorRels), weights = weight)
mod14gerNC <- lm(taxnum ~ religionlink, 
                 data = subset(ger, religion %in%gerrichRels), weights = weight)

mod15gerNC <- lm(taxnum ~ racelink, 
                 data = subset(ger, race %in% gerpoorEths), weights = weight)
mod16gerNC <- lm(taxnum ~ racelink, 
                 data = subset(ger, race %in% gerRichEths), weights = weight)

mod17gerNC <- lm(taxnum ~ regionlink, 
                 data = subset(ger, region %in% GerPoorRegions), weights = weight)
mod18gerNC <- lm(taxnum ~ regionlink, 
                 data = subset(ger, region %in% GerRichRegions), weights = weight)




mod13allNC <- lm(taxnum ~ religionlink, 
                 data = subset(all, religion %in% c(gerpoorRels, canpoorRels, ukpoorRels)), weights = weight)
mod14allNC <- lm(taxnum ~ religionlink, 
                 data = subset(all, religion %in%c(gerrichRels, ukrichRels, canRichRels)), weights = weight)

mod15allNC <- lm(taxnum ~ racelink, 
                 data = subset(all, race %in% c(gerpoorEths, UKpoorEths, canpoorEths)), weights = weight)
mod16allNC <- lm(taxnum ~ racelink, 
                 data = subset(all, race %in% c(gerRichEths, UKRichEths, canrichEths)), weights = weight)


mod17allNC <- lm(taxnum ~ regionlink, 
                 data = subset(all, region %in% c(as.character(GerPoorRegions), 
                                                  ukPoorRegions, poorcanprovs)), weights = weight)
mod18allNC <- lm(taxnum ~ regionlink, 
                 data = subset(all, region %in% c(GerRichRegions, ukRichRegions, richcanprovs)), weights = weight)



regeffsub <- data.frame(Country = rep(NA, 24), 
                        Group = rep(NA, 24), 
                        Mean = rep(NA, 24), 
                        SD = rep(NA, 24),
                        Dimension = rep(NA, 24))


allsubmods <- list(mod15ukNC, mod16ukNC, mod13ukNC, mod14ukNC, mod17ukNC, mod18ukNC, 
                   mod15gerNC, mod16gerNC, mod13gerNC, mod14gerNC, mod17gerNC, mod18gerNC, 
                   mod15canNC, mod16canNC, mod13canNC, mod14canNC, mod17canNC, mod18canNC, 
                   mod15allNC, mod16allNC, mod13allNC, mod14allNC, mod17allNC, mod18allNC)

cntrys <- c(rep("UK", 6), rep("Germany", 6), rep("Canada", 6), rep("Pooled", 6))
grps <- rep(c("Poor Groups", "Rich Groups"), 12)
dimens <- rep(c("Ethnic", "Ethnic", "Religious", "Religious", "Regional", "Regional"), 4)

for(i in 1:24){
  thismod <- allsubmods[[i]]
  regeffsub$Country[i] <- cntrys[i]
  regeffsub$Group[i] <- grps[i]
  regeffsub$Mean[i] <- coef(thismod)[2]
  regeffsub$SD[i] <- sqrt(vcov(thismod)[2,2])
  regeffsub$Dimension[i] <- dimens[i]
}


regeffsub$Upp <- regeffsub$Mean + 1.96*regeffsub$SD
regeffsub$Low <- regeffsub$Mean - 1.96*regeffsub$SD
regeffsub$Dimension <- factor(regeffsub$Dimension, levels = c("Regional", "Religious", "Ethnic"))
regeffsub$Country <- factor(regeffsub$Country, levels = c("UK", "Germany", "Canada", "Pooled"))

pdf("figs/appendix/SubgroupTaxPlotsNC.pdf", width = 6, height = 3)
ggplot(regeffsub, aes(y = Mean, x = Dimension, group = Group)) + 
  facet_wrap(vars(Country), nrow = 1) + 
  geom_point(cex = 2, aes(pch = Group), position=position_dodge(width=0.1)) + 
  geom_errorbar(aes(ymin = Low, ymax = Upp, x = Dimension),
                position=position_dodge(width=0.1), 
                width = 0) + 
  xlab("") + theme(legend.position="none") + 
  ylab("Effect of Linked Fate on Redistributive Attitudes") + 
  # xlim(c(1, 4)) + 
  scale_fill_grey() + 
  theme_bw() + 
  coord_flip() + 
  geom_hline(yintercept = 0, lty = "dashed") + 
  scale_shape_manual(values=c(15, 16)) + 
  theme(legend.title = element_blank())

dev.off()



## Union and link


mod8canNC <- lm(classlink ~  union, 
                data = subset(can, respondent_class == "working class"), weights = weight)
mod8ukNC <- lm(classlink ~ union, 
               data = subset(uk, respondent_class == "working class"), weights = weight)
mod8gerNC <- lm(classlink ~   union, 
                data = subset(ger, respondent_class == "working class"), weights = weight)

mod8allNC <- lm(classlink ~   union, 
                data = subset(all, respondent_class == "working class"), weights = weight)



newcovs3 <- c("unionNot Employed", "unionYes")
newcovlabs3 <- c( "Not working", "Union")

stargazer(list(mod8allNC, mod8canNC, mod8ukNC, mod8gerNC),
          out = "figs/appendix/classUnion3countryNC.tex",
          keep = newcovs3, 
          type = "latex", 
          align = TRUE,
          covariate.labels = newcovlabs3,
          dep.var.labels = c(""),
          column.labels = c("All", "Canada", "UK", "Germany"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          star.cutoffs = c(0.1, 0.05, 0.01))

## Moreno and link



mod5canNC <- lm(regionlink ~ moreno, 
                data = can, weights = weight)
mod5ukNC <- lm(regionlink ~ moreno, 
               data = uk, weights = weight)
mod5gerNC <- lm(regionlink ~ moreno, 
                data = ger, weights = weight)
mod5allNC  <- lm(regionlink ~ moreno, 
                 data = all, weights = weight)



newcovs <- c("moreno")
newcovlabs <- c("Regional ID")
stargazer(list(mod5allNC, mod5canNC, mod5ukNC, mod5gerNC),
          out = "figs/appendix/regMoreno3countryNC.tex",
          keep = newcovs, 
          type = "latex", 
          align = TRUE,
          covariate.labels = newcovlabs,
          dep.var.labels = c(""),
          column.labels = c("All", "Canada", "UK", "Germany"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          star.cutoffs = c(0.1, 0.05, 0.01), 
          add.lines = list(c("Region FE", "Yes", "Yes", "Yes", "Yes")))





### Adding controls to priming experiment


mod9canNC <- lm(racelink ~ taxarg+ union+Male + agecat + marital + Bachelor +incnum + race, 
                data = subset(can, taxarg != "Reg"), weights = weight)
mod9ukNC <- lm(racelink ~ taxarg+ union+Male + agecat + marital + Bachelor +incnum + race, 
               data = subset(uk, taxarg != "Reg"), weights = weight)
mod9gerNC <- lm(racelink ~ taxarg+ union+Male + agecat + marital + Bachelor +incnum + race, 
                data = subset(ger, taxarg != "Reg"), weights = weight)


mod10canNC <- lm(regionlink ~ taxarg+ union+Male + agecat + marital + Bachelor +incnum + region, 
                 data = subset(can, taxarg != "Eth"), weights = weight)
mod10gerNC <- lm(regionlink ~ taxarg+ union+Male + agecat + marital + Bachelor +incnum + region,
                 data = subset(ger, taxarg != "Eth"), weights = weight)


stargazer(list(mod9canNC,  mod9ukNC, mod9gerNC, mod10canNC, mod10gerNC),
          out = "figs/appendix/TaxToLinkWC.html",
          # keep = c("taxargEth", "taxargReg"),  
          type = "html", 
          align = TRUE,
          covariate.labels = c("Ethnic prime", "Regional prime"),
          dep.var.labels = c("Ethnicity", "Region"),
          column.labels = c("Canada", "UK", "Germany", "Canada", "Germany"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          star.cutoffs = c(0.1, 0.05, 0.01))
stargazer(list(mod9canNC,  mod9ukNC, mod9gerNC, mod10canNC, mod10gerNC),
          out = "figs/appendix/TaxToLinkWC.tex",
          # keep = c("taxargEth", "taxargReg"),  
          type = "latex", 
          align = TRUE,
          covariate.labels = c("Ethnic prime", "Regional prime"),
          dep.var.labels = c("Ethnicity", "Region"),
          column.labels = c("Canada", "UK", "Germany", "Canada", "Germany"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          star.cutoffs = c(0.1, 0.05, 0.01))




mod11canNC <- lm(racelink ~ taxarg + union+Male + agecat + marital + Bachelor +incnum + race, 
                 data = subset(can, race != "White" & race != "Other" & taxarg != "Reg"), weights = weight)
mod11ukNC <- lm(racelink ~ taxarg + union+Male + agecat + marital + Bachelor +incnum + race, 
                data = subset(uk, race != "White - British/English/Welsh/Scottish/Northern Irish"), 
                weights = weight)
mod11gerNC <- lm(racelink ~ taxarg + union+Male + agecat + marital + Bachelor +incnum + race, 
                 data = subset(ger, race != "Germans" & taxarg != "Reg"), weights = weight)


mod12canNC <- lm(regionlink ~ taxarg + union+Male + agecat + marital + Bachelor +incnum + region, 
                 data = subset(can, province != "Ontario" & province != "British Columbia" & taxarg != "Eth"),
                 weights = weight)
mod12gerNC <- lm(regionlink ~ taxarg + union+Male + agecat + marital + Bachelor +incnum + region, 
                 data = subset(ger, region != "Berlin" & region != "Bavaria" & taxarg != "Eth"), weights = weight)

stargazer(list(mod11canNC,  mod11ukNC, mod11gerNC, mod12canNC, mod12gerNC),
          out = "figs/appendix/TaxToLinkMinoritiesOnlyWC.html",
          #   keep = c("taxargEth", "taxargReg"),  
          type = "html", 
          align = TRUE,
          covariate.labels = c("Ethnic prime", "Regional prime"),
          dep.var.labels = c("Ethnicity", "Region"),
          column.labels = c("Canada", "UK", "Germany", "Canada", "Germany"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          star.cutoffs = c(0.1, 0.05, 0.01), 
          add.lines = list(c("Excludes", 
                             "White", "White British", 
                             "Germans", "Ont/BC", 
                             "Bavaria/Berlin")))

stargazer(list(mod11canNC,  mod11ukNC, mod11gerNC, mod12canNC, mod12gerNC),
          out = "figs/appendix/TaxToLinkMinoritiesOnlyWC.tex",
          #   keep = c("taxargEth", "taxargReg"),  
          type = "latex", 
          align = TRUE,
          covariate.labels = c("Ethnic prime", "Regional prime"),
          dep.var.labels = c("Ethnicity", "Region"),
          column.labels = c("Canada", "UK", "Germany", "Canada", "Germany"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          star.cutoffs = c(0.1, 0.05, 0.01), 
          add.lines = list(c("Excludes", 
                             "White", "White British", 
                             "Germans", "Ont/BC", 
                             "Bavaria/Berlin")))




########################################################################
### Balance tests
#######################################################################
## Function for p-value from 
# https://stackoverflow.com/questions/5587676/pull-out-p-values-and-r-squared-from-a-linear-regression
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

keydemos <- c("yob", "incnum", "Bachelor", "Male", "marital", "unionNot Employed", 
              "unionYes", "unionNo", "relfreq")
demolabs <- c("Year of birth", "Income (1-7)", "Bachelor", "Male", "Married",
              "Not working", "Working - union member", "Working - not member", 
              "Religious attendance (1-4)")

canbalance <- data.frame(Demographic = rep(NA, length(keydemos)), 
                         Control = rep(NA, length(keydemos)), 
                         Regional = rep(NA, length(keydemos)), 
                         Ethnic = rep(NA, length(keydemos)), 
                         SigDiff = rep(NA, length(keydemos)))
canctl <- subset(can, taxarg == "Ctl")
canreg <- subset(can, taxarg == "Reg")
caneth <- subset(can, taxarg == "Eth")

gerbalance <- data.frame(Demographic = rep(NA, length(keydemos)), 
                         Control = rep(NA, length(keydemos)), 
                         Regional = rep(NA, length(keydemos)), 
                         Ethnic = rep(NA, length(keydemos)), 
                         SigDiff = rep(NA, length(keydemos)))
gerctl <- subset(ger, taxarg == "Ctl")
gerreg <- subset(ger, taxarg == "Reg")
gereth <- subset(ger, taxarg == "Eth")


ukbalance <- data.frame(Demographic = rep(NA, length(keydemos)), 
                        Control = rep(NA, length(keydemos)), 
                        Ethnic = rep(NA, length(keydemos)), 
                        SigDiff = rep(NA, length(keydemos)))
ukctl <- subset(uk, taxarg == "Ctl")
uketh <- subset(uk, taxarg == "Eth")

for(i in 1:length(keydemos)){
  
  canbalance$Demographic[i] <- demolabs[i]
  canbalance$Control[i] <- mean(canctl[,keydemos[i]], na.rm = TRUE)
  canbalance$Regional[i] <- mean(canreg[,keydemos[i]], na.rm = TRUE)
  canbalance$Ethnic[i] <- mean(caneth[,keydemos[i]], na.rm = TRUE)
  
  mod <- lm(can[,keydemos[i]] ~ can$taxarg)
  canbalance$SigDiff[i] <- lmp(mod)
  
  
  
  ukbalance$Demographic[i] <- demolabs[i]
  ukbalance$Control[i] <- mean(ukctl[,keydemos[i]], na.rm = TRUE)
  ukbalance$Ethnic[i] <- mean(uketh[,keydemos[i]], na.rm = TRUE)
  
  mod <- lm(uk[,keydemos[i]] ~ uk$taxarg)
  ukbalance$SigDiff[i] <- lmp(mod)
  
  
  
  gerbalance$Demographic[i] <- demolabs[i]
  gerbalance$Control[i] <- mean(gerctl[,keydemos[i]], na.rm = TRUE)
  gerbalance$Regional[i] <- mean(gerreg[,keydemos[i]], na.rm = TRUE)
  gerbalance$Ethnic[i] <- mean(gereth[,keydemos[i]], na.rm = TRUE)
  
  mod <- lm(ger[,keydemos[i]] ~ ger$taxarg)
  gerbalance$SigDiff[i] <- lmp(mod)
  
}

stargazer(canbalance, 
          out = "figs/appendix/CanadianBalance.tex", 
          type = "latex", 
          summary = FALSE, 
          digits = 2)
stargazer(ukbalance, 
          out = "figs/appendix/UKBalance.tex", 
          type = "latex", 
          summary = FALSE, 
          digits = 2)
stargazer(gerbalance, 
          out = "figs/appendix/GerBalance.tex", 
          type = "latex", 
          summary = FALSE, 
          digits = 2)

#####################################################################
### Replacing subjective insecurity with low-income dummy
#####################################################################

can$lowinc <- ifelse(can$incnum < 4, 1, 0)
uk$lowinc <- ifelse(uk$incnum < 4, 1, 0)
ger$lowinc <- ifelse(ger$incnum < 4, 1, 0)
all$lowinc <- ifelse(all$incnum < 4, 1, 0)


modIncUK1 <- lm(racelink ~  lowinc +  Male + agecat + union + marital + Bachelor + race, 
                data = uk, weights = weight)
modIncUK2 <- lm(religionlink ~  lowinc +  Male + agecat + union + marital + Bachelor + religion, 
                data = uk, weights = weight)
modIncUK3 <- lm(regionlink ~  lowinc +  Male + agecat + union + marital + Bachelor + region, 
                data = uk, weights = weight)
modIncUK4 <- lm(classlink ~  lowinc +  Male + agecat + union + marital + Bachelor, 
                data = uk, weights = weight)

modIncCan1 <- lm(racelink ~  lowinc +  Male + agecat + union + marital + Bachelor + race, 
                 data = can, weights = weight)
modIncCan2 <- lm(religionlink ~  lowinc +  Male + agecat + union + marital + Bachelor + religion, 
                 data = can, weights = weight)
modIncCan3 <- lm(regionlink ~  lowinc +  Male + agecat + union + marital + Bachelor + region, 
                 data = can, weights = weight)
modIncCan4 <- lm(classlink ~  lowinc +  Male + agecat + union + marital + Bachelor, 
                 data = can, weights = weight)

modIncGer1 <- lm(racelink ~  lowinc +  Male + agecat + union + marital + Bachelor + race, 
                 data = ger, weights = weight)
modIncGer2 <- lm(religionlink ~  lowinc +  Male + agecat + union + marital + Bachelor + religion, 
                 data = ger, weights = weight)
modIncGer3 <- lm(regionlink ~  lowinc +  Male + agecat + union + marital + Bachelor + region, 
                 data = ger, weights = weight)
modIncGer4 <- lm(classlink ~  lowinc +  Male + agecat + union + marital + Bachelor, 
                 data = ger, weights = weight)

modIncAll1 <- lm(racelink ~  lowinc +  Male + agecat + union + marital + Bachelor + race, 
                 data = all, weights = weight)
modIncAll2 <- lm(religionlink ~  lowinc +  Male + agecat + union + marital + Bachelor + religion, 
                 data = all, weights = weight)
modIncAll3 <- lm(regionlink ~  lowinc +  Male + agecat + union + marital + Bachelor + region, 
                 data = all, weights = weight)
modIncAll4 <- lm(classlink ~  lowinc +  Male + agecat + union + marital + Bachelor + Country, 
                 data = all, weights = weight)

inceffs <- data.frame(Country = rep(NA, 16), 
                      Dimension = rep(NA, 16), 
                      Estimate = rep(NA, 16), 
                      SD = rep(NA, 16))
inceffs$Country <- c(rep("UK", 4), rep("Germany", 4),  rep("Canada", 4), rep("Pooled", 4))
inceffs$Dimension <- rep(c("Ethnicity", "Religion", "Region", "Class"), 4)


thesemods <- list(modIncUK1, modIncUK2, modIncUK3, modIncUK4, 
                  modIncGer1, modIncGer2, modIncGer3, modIncGer4, 
                  modIncCan1, modIncCan2, modIncCan3, modIncCan4, 
                  modIncAll1, modIncAll2, modIncAll3, modIncAll4)

for (i in 1:length(thesemods)){
  thismod <- thesemods[[i]]
  inceffs$Estimate[i] <- coef(thismod)[2]
  inceffs$SD[i] <- sqrt(vcov(thismod)[2,2])
}
inceffs$Upp <- inceffs$Estimate + 1.96*inceffs$SD
inceffs$Low <- inceffs$Estimate - 1.96*inceffs$SD



inceffs$Dimension <- factor(inceffs$Dimension, levels = c("Class", "Region", "Religion", "Ethnicity"))
inceffs$Country <- factor(inceffs$Country, levels = c("UK", "Germany", "Canada", "Pooled"))

pdf("figs/appendix/LowInc16.pdf", width = 6, height = 4)
ggplot(inceffs,  aes(y = Estimate, x = Dimension)) + 
  facet_wrap(vars(Country), nrow = 1) + 
  geom_point()  +
  ylab("Effect of Low-Income Dummy") + 
  xlab("Dimension of linked fate") + 
  geom_segment(aes(y = Low, yend = Upp, x = Dimension, xend = Dimension)) + 
  geom_hline(yintercept = 0, lty = "dashed") + 
  coord_flip()
dev.off()
