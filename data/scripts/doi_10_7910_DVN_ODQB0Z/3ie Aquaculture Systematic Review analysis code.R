## Syntax for Aquaculture SR meta-analysis  

## Script author(s): Shannon Shisler
## Date created 10, February, 2021
## Last edit date: 14 Sept 2021 (by CGP)

## clear global enviro if needed
rm(list = ls()) 

## set working directory
setwd("C:/Users/lshmg5/Desktop")

library(metafor)
library(robumeta)
library(grid)

## Load dataset
library(readxl)
aqcdata <- read_excel("3ie Aquaculture Systematic Review Data.xlsx")
View(aqcdata)

#### 0.a. check-ups & create dummies for key moderators ####

# Continent
table(aqcdata$continent)
aqcdata$continent2[aqcdata$continent=="East_Asia_and_Pacific"] <- "Asia"
aqcdata$continent2[aqcdata$continent=="South_Asia"] <- "Asia"
aqcdata$continent2[aqcdata$continent=="SSA"] <- "Africa"
table(aqcdata$continent, aqcdata$continent2)
table(aqcdata$continent2)

# Country
table(aqcdata$country)
aqcdata$Bangladesh <- 1
aqcdata$Bangladesh[aqcdata$country=="Cambodia"] <- 0
aqcdata$Bangladesh[aqcdata$country=="Indonesia"] <- 0
aqcdata$Bangladesh[aqcdata$country=="Kenya"] <- 0
aqcdata$Bangladesh[aqcdata$country=="Malawi"] <- 0
aqcdata$Bangladesh[aqcdata$country=="Nigeria"] <- 0
table(aqcdata$country, aqcdata$Bangladesh)
table(aqcdata$Bangladesh)

# Exposure to climate shocks index
table(aqcdata$ExpCliSho)
aqcdata$ExpCliSho_VH <- 1
aqcdata$ExpCliSho_VH[aqcdata$ExpCliSho=="2=high"] <- 0
aqcdata$ExpCliSho_VH[aqcdata$ExpCliSho=="3=medium"] <- 0
table(aqcdata$ExpCliSho, aqcdata$ExpCliSho_VH)
table(aqcdata$ExpCliSho_VH) 

# World Bank country income group
table(aqcdata$WB_incgroup)

# scale of intervention: At what scale was the intervention implemented?
table(aqcdata$scale)

# Programme size (estimated # of beneficiaries)
table(aqcdata$Prog_size)

# Comparison group
# 1=No intervention, 2=Other intervention, 3=Pipeline/wait-list
table(aqcdata$Comparison)
aqcdata$Comparison2[aqcdata$Comparison==1] <- "No intervention"
aqcdata$Comparison2[aqcdata$Comparison==3] <- "Pipeline"
table(aqcdata$Comparison, aqcdata$Comparison2)
table(aqcdata$Comparison2)

# Intervention components
table(aqcdata$comp_productivity)
table(aqcdata$comp_income)
table(aqcdata$comp_nutrition)
table(aqcdata$comp_WE)

  # nutrition or women's empowerment components
aqcdata$comp_nutORwe <- 0
aqcdata$comp_nutORwe[aqcdata$comp_nutrition==1 | aqcdata$comp_WE==1] <- 1
table(aqcdata$comp_nutrition, aqcdata$comp_WE)
table(aqcdata$comp_nutORwe)

  # nutrition and women's empowerment components
aqcdata$comp_nutANDwe <- 0
aqcdata$comp_nutANDwe[aqcdata$comp_nutrition==1 & aqcdata$comp_WE==1] <- 1
table(aqcdata$comp_nutrition, aqcdata$comp_WE)
table(aqcdata$comp_nutANDwe)

  # all four components
aqcdata$comp_all4 <- 0
aqcdata$comp_all4[aqcdata$comp_productivity==1 & aqcdata$comp_income==1 & 
                          aqcdata$comp_nutrition==1 & aqcdata$comp_WE==1] <- 1
table(aqcdata$comp_all4)

# Value chain: where in the aquaculture value chain is the intervention focused on? 
table(aqcdata$vchain_bfprod)
table(aqcdata$vchain_prod)
table(aqcdata$vchain_proc)
table(aqcdata$vchain_trad)
table(aqcdata$vchain_mktg)
aqcdata$vchain_aftprod <- 0
aqcdata$vchain_aftprod[aqcdata$vchain_proc==1 | aqcdata$vchain_trad==1 | 
                             aqcdata$vchain_mktg==1] <- 1
table(aqcdata$vchain_aftprod)

# Aquaculture plus: Does the estimated effect include interventions other than 
# aquaculture activities? (1=yes, 0=no)
table(aqcdata$AqcPlus)

# Is the intervention community based, or is it aimed at individuals?
# 1=community, 0=individuals
table(aqcdata$Community)

# Exposure to intervention (in months): How long is the intervention exposure itself?
table(aqcdata$ExpIntMo)

# Evaluation period (in months): Total number of months elapsed between the end 
# of an intervention and the point at which an outcome measure is taken post intervention
table(aqcdata$EvalInMo)

# Publication type
table(aqcdata$PubType)
aqcdata$peerrev_pub <- 0 # peer-reviewed and published
aqcdata$peerrev_pub[aqcdata$PubType=="1= Peer-reviewed journal article"] <- 1
table(aqcdata$PubType, aqcdata$peerrev_pub)
table(aqcdata$peerrev_pub)

# Year of publication
table(aqcdata$Year)

# Evaluation design: 0=Experimental Design (e.g., RCT), 1=Quasi-Experimental Design
table(aqcdata$Design)
aqcdata$Design2[aqcdata$Design==1] <- "QED"
aqcdata$Design2[aqcdata$Design==0] <- "RCT"
table(aqcdata$Design, aqcdata$Design2)
table(aqcdata$Design2)

# Assumptions made when extracting ES (1=yes, 0=no)
# > rename first
names(aqcdata)[which(names(aqcdata)=="Exact data or assumed data?")] <- "Assumed_data"
table(aqcdata$Assumed_data)

# ROB
table(aqcdata$ROB)

#### 0.b. check per-protocol outlier analysis ####

summary(aqcdata$yi)
aqcdata$zscore_yi <- ((aqcdata$yi-mean(aqcdata$yi, na.rm=TRUE))/sd(aqcdata$yi, na.rm=TRUE))
sd(aqcdata$zscore_yi, na.rm=TRUE) # sd should be 1
summary(aqcdata$zscore_yi)        # mean should be 0, check min/max
sort(aqcdata$zscore_yi)


#### 1. main effects by outcome (# of effects) ####
########################## PRODUCTION VALUE ANALYSIS (4) #######################

proval <- aqcdata[ which(aqcdata$`Production Value`==1 & aqcdata$use==1),]

# random-effects model
m_prval <- rma(yi, vi, method="DL", data=proval)
m_prval

# forest plot ordered by effect size
forest(m_prval, slab=paste(proval$Prog, " (", proval$Author, ", ", proval$Year,")", sep = ""), 
       xlim=c(-6,4), at=seq(-1, 1.5, 0.5), digits=c(2,1), 
       xlab="Standardized Mean Difference (Hedges'g)", order = order(proval$yi), 
       cex=0.8, ylim=c(-4,7), ilab=(proval$n), ilab.xpos=c(-1.5))
text(-6, 6, "Programme (Author(s), Year)", cex=0.8, font=2, pos=4)
text(-1.8, 6, "n", cex=0.8, font=2, pos=4)
text( 4, 6, "SMD [95% CI]", cex=0.8, font=2, pos=2)
text(-6, -1.75, pos=4, cex=0.7, 
     bquote(paste("(Q = ", .(formatC(m_prval$QE, digits=2, format="f")), 
                  ", df = ", .(m_prval$k - m_prval$p), ", p = ", 
                  .(formatC(m_prval$QEp, digits=2, format="f")), "; ", 
                  T^2, " = ", .(formatC(m_prval$tau2, digits=2, format="f")), 
                  "; ", I^2, " = ", .(formatC(m_prval$I2, digits=2, format="f")), "%)")))
text(c(-0.8, 1), -4, c("Favours control", "Favours programme"), cex=0.7)


########################## PRODUCTION VOLUME ANALYSIS (3) ######################

proyld <- aqcdata[ which(aqcdata$`Production Volume`==1 & aqcdata$use==1),]

# random-effects model
m_proyld <- rma(yi, vi, method="DL", data=proyld)
m_proyld

# forest plot ordered by effect size
forest(m_proyld, slab=paste(proyld$Prog, " (", proyld$Author, ", ", proyld$Year,")", sep = ""), 
       xlim=c(-10,5), at=seq(-1, 1.5, .5), digits=c(2,1), 
       xlab="Standardized Mean Difference (Hedges'g)", order = order(proyld$yi), 
       cex=0.8, ylim=c(-4,6), ilab=(proyld$n), ilab.xpos=c(-2))
text(-10, 5, "Programme (Author(s), Year)", cex=0.8, font=2, pos=4)
text(-2.5, 5, "n", cex=0.8, font=2, pos=4)
text( 5, 5, "SMD [95% CI]", cex=0.8, font=2, pos=2)
text(-10, -1.75, pos=4, cex=0.7, 
     bquote(paste("(Q = ", .(formatC(m_proyld$QE, digits=2, format="f")), 
                  ", df = ", .(m_proyld$k - m_proyld$p), ", p = ", 
                  .(formatC(m_proyld$QEp, digits=2, format="f")), "; ", 
                  T^2, " = ", .(formatC(m_proyld$tau2, digits=2, format="f")), 
                  "; ", I^2, " = ", .(formatC(m_proyld$I2, digits=2, format="f")), "%)")))
text(c(-1.2, 1.5), -4, c("Favours control", "Favours programme"), cex=0.7)

########################## PRODUCTION QUALITY (1) #######################

proqua <- subset(aqcdata, `Production Quality`==1)
m_proqua <- rma(yi, vi, method="DL", data=proqua)
m_proqua


########################## INCOME ANALYSIS (10) ################################

income <- aqcdata[ which(aqcdata$Inc==1 & aqcdata$use==1),]

# random-effects model
m_income <- rma(yi, vi, method="DL", data=income)
m_income

# forest plot ordered by effect size
forest(m_income, slab=paste(income$Prog, " (", income$Author, ", ", income$Year,")", sep = ""), 
       xlim=c(-8,4), at=seq(-1, 1.5, 0.5), digits=c(2,1), 
       xlab="Standardized Mean Difference (Hedges'g)", order = order(income$yi), 
       cex=0.8, ylim=c(-4,13), ilab=(income$n), ilab.xpos=c(-1.5))
text(-8, 12, "Programme (Author(s), Year)", cex=0.8, font=2, pos=4)
text(-1.8, 12, "n", cex=0.8, font=2, pos=4)
text( 4, 12, "SMD [95% CI]", cex=0.8, font=2, pos=2)
text(-8, -1.75, pos=4, cex=0.7, 
     bquote(paste("(Q = ", .(formatC(m_income$QE, digits=2, format="f")), 
                  ", df = ", .(m_income$k - m_income$p), ", p = ", 
                  .(formatC(m_income$QEp, digits=2, format="f")), "; ", 
                  T^2, " = ", .(formatC(m_income$tau2, digits=2, format="f")), 
                  "; ", I^2, " = ", .(formatC(m_income$I2, digits=2, format="f")), "%)")))
text(c(-1, 1.2), -4, c("Favours control", "Favours programme"), cex=0.7)


########################## EXPENDITURE total ANALYSIS (5) ######################

expend <- aqcdata[ which(aqcdata$`Expenditure total`==1 & aqcdata$use==1),]

# random-effects model
m_expend <- rma(yi, vi, method="DL", data=expend)
m_expend

# forest plot ordered by effect size
forest(m_expend, slab=paste(expend$Prog, " (", expend$Author, ", ", expend$Year,")", sep = ""), 
       xlim=c(-7,4), at=seq(-1, 1.5, 0.5), digits=c(2,1), 
       xlab="Standardized Mean Difference (Hedges'g)", order = order(expend$yi), 
       cex=0.8, ylim=c(-4,8), ilab=(expend$n), ilab.xpos=c(-2))
text(-7, 7, "Programme (Author(s), Year)", cex=0.8, font=2, pos=4)
text(-2.3, 7, "n", cex=0.8, font=2, pos=4)
text( 4, 7, "SMD [95% CI]", cex=0.8, font=2, pos=2)
text(-7, -1.75, pos=4, cex=0.7, 
     bquote(paste("(Q = ", .(formatC(m_expend$QE, digits=2, format="f")), 
                  ", df = ", .(m_expend$k - m_expend$p), ", p = ", 
                  .(formatC(m_expend$QEp, digits=2, format="f")), "; ", 
                  T^2, " = ", .(formatC(m_expend$tau2, digits=2, format="f")), 
                  "; ", I^2, " = ", .(formatC(m_expend$I2, digits=2, format="f")), "%)")))
text(c(-1, 1.2), -4, c("Favours control", "Favours programme"), cex=0.7)

########################## EXPENDITURE food ANALYSIS (2) #######################

expfood <- aqcdata[ which(aqcdata$`Expenditure food`==1 & aqcdata$use==1),]

# random-effects model
m_expfood <- rma(yi, vi, method="DL", data=expfood)
m_expfood

# forest plot ordered by effect size
forest(m_expfood, slab=paste(expfood$Prog, " (", expfood$Author, ", ", expfood$Year,")", sep = ""), 
       xlim=c(-7,4), at=seq(-1, 1.5, 0.5), digits=c(3,1), 
       xlab="Standardized Mean Difference (Hedges'g)", order = order(expfood$yi), 
       cex=0.8, ylim=c(-4,5), ilab=(expfood$n), ilab.xpos=c(-2))
text(-7, 4, "Programme (Author(s), Year)", cex=0.8, font=2, pos=4)
text(-2.3, 4, "n", cex=0.8, font=2, pos=4)
text( 4, 4, "SMD [95% CI]", cex=0.8, font=2, pos=2)
text(-7, -1.75, pos=4, cex=0.7, 
     bquote(paste("(Q = ", .(formatC(m_expfood$QE, digits=2, format="f")), 
                  ", df = ", .(m_expfood$k - m_expfood$p), ", p = ", 
                  .(formatC(m_expfood$QEp, digits=2, format="f")), "; ", 
                  T^2, " = ", .(formatC(m_expfood$tau2, digits=2, format="f")), 
                  "; ", I^2, " = ", .(formatC(m_expfood$I2, digits=2, format="f")), "%)")))
text(c(-1, 1.2), -4, c("Favours control", "Favours programme"), cex=0.7)

########################## Farm Profit ANALYSIS (3) ############################

profit <- aqcdata[ which(aqcdata$Profit==1 & aqcdata$use==1),]

# random-effects model
m_profit <- rma(yi, vi, method="DL", data=profit)
m_profit

# forest plot ordered by effect size
forest(m_profit, slab=paste(profit$Prog, " (", profit$Author, ", ", profit$Year,")", sep = ""), 
       xlim=c(-9,5), at=seq(-1, 1.5, .5), digits=c(2,1), 
       xlab="Standardized Mean Difference (Hedges'g)", order = order(profit$yi), 
       cex=0.8, ylim=c(-4,6), ilab=(profit$n), ilab.xpos=c(-2))
text(-9, 5, "Programme (Author(s), Year)", cex=0.8, font=2, pos=4)
text(-2.5, 5, "n", cex=0.8, font=2, pos=4)
text( 5, 5, "SMD [95% CI]", cex=0.8, font=2, pos=2)
text(-9, -1.75, pos=4, cex=0.7, 
     bquote(paste("(Q = ", .(formatC(m_profit$QE, digits=2, format="f")), 
                  ", df = ", .(m_profit$k - m_profit$p), ", p = ", 
                  .(formatC(m_profit$QEp, digits=2, format="f")), "; ", 
                  T^2, " = ", .(formatC(m_profit$tau2, digits=2, format="f")), 
                  "; ", I^2, " = ", .(formatC(m_profit$I2, digits=2, format="f")), "%)")))
text(c(-1.2, 1.5), -4, c("Favours control", "Favours programme"), cex=0.7)

########################## ASSETS ANALYSIS (2) #################################

assets <- aqcdata[ which(aqcdata$Assets==1 & aqcdata$use==1),]

# random-effects model
m_assets <- rma(yi, vi, method="DL", data=assets)
m_assets

# forest plot ordered by effect size
forest(m_assets, slab=paste(assets$Prog, " (", assets$Author, ", ", assets$Year,")", sep = ""), 
       xlim=c(-8,4), at=seq(-1, 1.5, 0.5), digits=c(2,1), 
       xlab="Standardized Mean Difference (Hedges'g)", order = order(assets$yi), 
       cex=0.8, ylim=c(-4,5), ilab=(assets$n), ilab.xpos=c(-2))
text(-8, 4, "Programme (Author(s), Year)", cex=0.8, font=2, pos=4)
text(-2.3, 4, "n", cex=0.8, font=2, pos=4)
text( 4, 4, "SMD [95% CI]", cex=0.8, font=2, pos=2)
text(-8, -1.75, pos=4, cex=0.7, 
     bquote(paste("(Q = ", .(formatC(m_assets$QE, digits=2, format="f")), 
                  ", df = ", .(m_assets$k - m_assets$p), ", p = ", 
                  .(formatC(m_assets$QEp, digits=2, format="f")), "; ", 
                  T^2, " = ", .(formatC(m_assets$tau2, digits=2, format="f")), 
                  "; ", I^2, " = ", .(formatC(m_assets$I2, digits=2, format="f")), "%)")))
text(c(-1, 1.2), -4, c("Favours control", "Favours programme"), cex=0.7)


########################## POVERTY ANALYSIS (2) ################################

prvrty <- aqcdata[ which(aqcdata$Poverty==1 & aqcdata$use==1),]

# random-effects model
m_prvrty <- rma(yi, vi, method="DL", data=prvrty)
m_prvrty

# forest plot ordered by effect size
forest(m_prvrty, slab=paste(prvrty$Prog, " (", prvrty$Author, ", ", prvrty$Year,")", sep = ""), 
       xlim=c(-8,4), at=seq(-1, 1.5, 0.5), digits=c(2,1), 
       xlab="Standardized Mean Difference (Hedges'g)", order = order(prvrty$yi), 
       cex=0.8, ylim=c(-4,5), ilab=(prvrty$n), ilab.xpos=c(-2))
text(-8, 4, "Programme (Author(s), Year)", cex=0.8, font=2, pos=4)
text(-2.3, 4, "n", cex=0.8, font=2, pos=4)
text( 4, 4, "SMD [95% CI]", cex=0.8, font=2, pos=2)
text(-8, -1.75, pos=4, cex=0.7, 
     bquote(paste("(Q = ", .(formatC(m_prvrty$QE, digits=2, format="f")), 
                  ", df = ", .(m_prvrty$k - m_prvrty$p), ", p = ", 
                  .(formatC(m_prvrty$QEp, digits=2, format="f")), "; ", 
                  T^2, " = ", .(formatC(m_prvrty$tau2, digits=2, format="f")), 
                  "; ", I^2, " = ", .(formatC(m_prvrty$I2, digits=2, format="f")), "%)")))
text(c(-1, 1.2), -4, c("Favours control", "Favours programme"), cex=0.7)


########################## FARM REVENUE (1) #######################

revenue <- subset(aqcdata, `Revenue`==1)
m_revenu <- rma(yi, vi, method="DL", data=revenue)
m_revenu


########################## MARKET PARTICIPATION (1) #######################

mktpar <- subset(aqcdata, `Market Participation`==1)
m_mktpar <- rma(yi, vi, method="DL", data=mktpar)
m_mktpar


########################## PRICES (1) #######################

prices <- subset(aqcdata, `Price received`==1)
m_prices <- rma(yi, vi, method="DL", data=prices)
m_prices
forest(m_prices)

########################## Fish Consumption ANALYSIS (2) #######################

fishc <- aqcdata[ which(aqcdata$`Fish Consumption`==1 & aqcdata$use==1),]

# random-effects model
m_fishc <- rma(yi, vi, method="DL", data=fishc)
m_fishc

# forest plot ordered by effect size
forest(m_fishc, slab=paste(fishc$Prog, " (", fishc$Author, ", ", fishc$Year,")", sep = ""), 
       xlim=c(-7,4), at=seq(-1, 1.5, 0.5), digits=c(2,1), 
       xlab="Standardized Mean Difference (Hedges'g)", order = order(fishc$yi), 
       cex=0.8, ylim=c(-4,5), ilab=(fishc$n), ilab.xpos=c(-2))
text(-7, 4, "Programme (Author(s), Year)", cex=0.8, font=2, pos=4)
text(-2.3, 4, "n", cex=0.8, font=2, pos=4)
text( 4, 4, "SMD [95% CI]", cex=0.8, font=2, pos=2)
text(-7, -1.75, pos=4, cex=0.7, 
     bquote(paste("(Q = ", .(formatC(m_fishc$QE, digits=2, format="f")), 
                  ", df = ", .(m_fishc$k - m_fishc$p), ", p = ", 
                  .(formatC(m_fishc$QEp, digits=2, format="f")), "; ", 
                  T^2, " = ", .(formatC(m_fishc$tau2, digits=2, format="f")), 
                  "; ", I^2, " = ", .(formatC(m_fishc$I2, digits=2, format="f")), "%)")))
text(c(-1, 1.2), -4, c("Favours control", "Favours programme"), cex=0.7)

########################## WOMEN's BMI ANALYSIS (3) #########################

wwomen <- aqcdata[ which(aqcdata$`Weight women`==1 & aqcdata$use==1),]

# random-effects model
m_wwomen <- rma(yi, vi, method="DL", data=wwomen)
m_wwomen

# forest plot ordered by effect size
forest(m_wwomen, slab=paste(wwomen$Prog, " (", wwomen$Author, ", ", wwomen$Year,")", sep = ""), 
       xlim=c(-8,5), at=seq(-1, 1.5, .5), digits=c(2,1), 
       xlab="Standardized Mean Difference (Hedges'g)", order = order(wwomen$yi), 
       cex=0.8, ylim=c(-4,6), ilab=(wwomen$n), ilab.xpos=c(-2))
text(-8, 5, "Programme (Author(s), Year)", cex=0.8, font=2, pos=4)
text(-2.3, 5, "n", cex=0.8, font=2, pos=4)
text( 5, 5, "SMD [95% CI]", cex=0.8, font=2, pos=2)
text(-8, -1.75, pos=4, cex=0.7, 
     bquote(paste("(Q = ", .(formatC(m_wwomen$QE, digits=2, format="f")), 
                  ", df = ", .(m_wwomen$k - m_wwomen$p), ", p = ", 
                  .(formatC(m_wwomen$QEp, digits=2, format="f")), "; ", 
                  T^2, " = ", .(formatC(m_wwomen$tau2, digits=2, format="f")), 
                  "; ", I^2, " = ", .(formatC(m_wwomen$I2, digits=2, format="f")), "%)")))
text(c(-1.2, 1.5), -4, c("Favours control", "Favours programme"), cex=0.7)

########################## MEN's BMI ANALYSIS (2) #########################

wmen <- aqcdata[ which(aqcdata$`Estimate ID`=="52177083_31" | 
                               aqcdata$`Estimate ID`=="52177083_64"),]

# random-effects model
m_wmen <- rma(yi, vi, method="DL", data=wmen)
m_wmen

# forest plot ordered by effect size
forest(m_wmen, slab=paste(wmen$Prog, " (", wmen$Author, ", ", wmen$Year,")", sep = ""), 
       xlim=c(-7,4), at=seq(-1, 1.5, 0.5), digits=c(2,1), 
       xlab="Standardized Mean Difference (Hedges'g)", order = order(wmen$yi), 
       cex=0.8, ylim=c(-4,5), ilab=(wmen$n), ilab.xpos=c(-2))
text(-7, 4, "Programme (Author(s), Year)", cex=0.8, font=2, pos=4)
text(-2.3, 4, "n", cex=0.8, font=2, pos=4)
text( 4, 4, "SMD [95% CI]", cex=0.8, font=2, pos=2)
text(-7, -1.75, pos=4, cex=0.7, 
     bquote(paste("(Q = ", .(formatC(m_wmen$QE, digits=2, format="f")), 
                  ", df = ", .(m_wmen$k - m_wmen$p), ", p = ", 
                  .(formatC(m_wmen$QEp, digits=2, format="f")), "; ", 
                  T^2, " = ", .(formatC(m_wmen$tau2, digits=2, format="f")), 
                  "; ", I^2, " = ", .(formatC(m_wmen$I2, digits=2, format="f")), "%)")))
text(c(-1, 1.2), -4, c("Favours control", "Favours programme"), cex=0.7)

########################## CHILDREN's HAZ ANALYSIS (3) ######################

wchild <- aqcdata[ which(aqcdata$`Weight children`==1 & aqcdata$use==1),]

# random-effects model
m_wchild <- rma(yi, vi, method="DL", data=wchild)
m_wchild

# forest plot ordered by effect size
forest(m_wchild, slab=paste(wchild$Prog, " (", wchild$Author, ", ", wchild$Year,")", sep = ""), 
       xlim=c(-8,5), at=seq(-1, 1.5, .5), digits=c(2,1), 
       xlab="Standardized Mean Difference (Hedges'g)", order = order(wchild$yi), 
       cex=0.8, ylim=c(-4,6), ilab=(wchild$n), ilab.xpos=c(-2))
text(-8, 5, "Programme (Author(s), Year)", cex=0.8, font=2, pos=4)
text(-2.3, 5, "n", cex=0.8, font=2, pos=4)
text( 5, 5, "SMD [95% CI]", cex=0.8, font=2, pos=2)
text(-8, -1.75, pos=4, cex=0.7, 
     bquote(paste("(Q = ", .(formatC(m_wchild$QE, digits=2, format="f")), 
                  ", df = ", .(m_wchild$k - m_wchild$p), ", p = ", 
                  .(formatC(m_wchild$QEp, digits=2, format="f")), "; ", 
                  T^2, " = ", .(formatC(m_wchild$tau2, digits=2, format="f")), 
                  "; ", I^2, " = ", .(formatC(m_wchild$I2, digits=2, format="f")), "%)")))
text(c(-1.2, 1.5), -4, c("Favours control", "Favours programme"), cex=0.7)

########################## DISAGGREGATED CHILDREN's HAZ ANALYSIS (1) ######################

wdgirl <- aqcdata[ which(aqcdata$`Shortened Definition`=="Height-for-age z-score (girls age 0-5 years)"),]
m_wdgirl <- rma(yi, vi, method="DL", data=wdgirl)
m_wdgirl
forest(m_wdgirl, order = order(wdgirl$yi))

wdboys <- aqcdata[ which(aqcdata$`Shortened Definition`=="Height-for-age z-score (boys age 0-5 years)"),]
m_wdboys <- rma(yi, vi, method="DL", data=wdboys)
m_wdboys
forest(m_wdboys, order = order(wdboys$yi))

########################## WAZ-WHZ CHILDREN (1) ######################

otwchi <- aqcdata[ which(aqcdata$`Shortened Definition`=="Wasting" | 
                                 aqcdata$`Shortened Definition`=="Underweight"),]
m_otwchi <- rma(yi, vi, method="DL", data=otwchi)
m_otwchi
forest(m_otwchi, order = order(otwchi$yi))

########################## FOOD SECURITY (1) #######################

fdsecu <- aqcdata[ which(aqcdata$`Food security`==1),]
m_fdsecu <- rma(yi, vi, method="DL", data=fdsecu)
m_fdsecu
forest(m_fdsecu, order = order(fdsecu$yi))

########################## NUTRIENTS INTAKE WOMEN (1) ######################

nutwom <- aqcdata[ which(aqcdata$`Nutrient Intake`==1 & 
                                 aqcdata$`If yes to subgroup, describe`=="Women"),]
m_nutwom <- rma(yi, vi, method="DL", data=nutwom)
m_nutwom
forest(m_nutwom, order = order(nutwom$yi))

########################## NUTRIENTS INTAKE CHILDREN (1) ######################

nutchi <- aqcdata[ which(aqcdata$`Nutrient Intake`==1 & 
                                 aqcdata$`If yes to subgroup, describe`=="Children"),]
m_nutchi <- rma(yi, vi, method="DL", data=nutchi)
m_nutchi
forest(m_nutchi, order = order(nutchi$yi))

########################## BLOOD MEASURES (1) ######################

blood <- subset(aqcdata, `Blood measures`==1)
m_blood <- rma(yi, vi, method="DL", data=blood)
m_blood
forest(m_blood, order = order(blood$yi))

########################## Women's Empowerment ######################

  # MAEP programme - Hallman et al. study
WE_maepH <- aqcdata[ which(aqcdata$`Women's Empowerment`==1 & aqcdata$Prog=="MAEP" & 
                                   aqcdata$Author=="Hallman et al."),]
m_WE_maepH <- rma(yi, vi, method="DL", data=WE_maepH)
m_WE_maepH
forest(m_WE_maepH, order = order(WE_maepH$yi))

  # MAEP programme - Quisumbing & Kumar study
WE_maepQ <- aqcdata[ which(aqcdata$`Women's Empowerment`==1 & aqcdata$Prog=="MAEP" & 
                                   aqcdata$Author=="Quisumbing & Kumar"),]
m_WE_maepQ <- rma(yi, vi, method="DL", data=WE_maepQ)
m_WE_maepQ
forest(m_WE_maepQ, order = order(WE_maepQ$yi))

  # MAEP programme - Danida study
WE_maepD <- aqcdata[ which(aqcdata$`Women's Empowerment`==1 & aqcdata$Prog=="MAEP" & 
                                   aqcdata$Author=="Danida"),]
m_WE_maepD <- rma(yi, vi, method="DL", data=WE_maepD)
m_WE_maepD
forest(m_WE_maepD, order = order(WE_maepD$yi))

  # NGO BS programme - Hallman et al. study
WE_BSH <- aqcdata[ which(aqcdata$`Women's Empowerment`==1 & aqcdata$Prog=="NGO BS" & 
                                 aqcdata$Author=="Hallman et al."),]
m_WE_BSH <- rma(yi, vi, method="DL", data=WE_BSH)
m_WE_BSH
forest(m_WE_BSH, order = order(WE_BSH$yi))

  # NGO BS programme - Quisumbing & Kumar study
WE_BSQ <- aqcdata[ which(aqcdata$`Women's Empowerment`==1 & aqcdata$Prog=="NGO BS" & 
                                 aqcdata$Author=="Quisumbing & Kumar"),]
m_WE_BSQ <- rma(yi, vi, method="DL", data=WE_BSQ)
m_WE_BSQ
forest(m_WE_BSQ, order = order(WE_BSQ$yi))

  # GNAEP programme - Danida study
WE_GNAEP <- aqcdata[ which(aqcdata$`Women's Empowerment`==1 & aqcdata$Prog=="GNAEP"),]
m_WE_GNAEP <- rma(yi, vi, method="DL", data=WE_GNAEP)
m_WE_GNAEP
forest(m_WE_GNAEP, order = order(WE_GNAEP$yi))


#### 2. moderator analyses by outcome (# of effects) ####

########################## Description #################################
# moderator analyses were conducted using mixed-effects models only for 
# outcomes with at least 4 effects, for categorical variables with 
# "cell count" of at least 2, and for numeric variables (Prog_size, 
# ExpIntMo, EvalInMo, Year).

########################## PRODUCTION VALUE ANALYSIS (4) #######################

# Continent
table(proval$continent)   # South_Asia = 4
table(proval$continent2)  # Asia = 4

# Country
table(proval$country)     # Bangladesh = 4
table(proval$Bangladesh)  # 1 = 4

# Exposure to climate shocks index
table(proval$ExpCliSho)    # very high = 4
table(proval$ExpCliSho_VH) # 1 = 4

# World Bank country income group
table(proval$WB_incgroup)  # Low = 3, Lower-middle = 1

# scale of intervention
table(proval$scale)  # Subnational = 4 

# Programme size
table(proval$Prog_size)
proval_Prog_size <- rma(yi, vi, mods = ~ Prog_size, data=proval)
proval_Prog_size

# Comparison group
table(proval$Comparison2) # No intervention = 4

# Intervention components
table(proval$comp_productivity)  # yes = 4
table(proval$comp_income)        # yes = 4
table(proval$comp_nutrition)     # yes = 3, no = 1
table(proval$comp_WE)            # yes = 4
table(proval$comp_nutORwe)       # yes = 4
table(proval$comp_nutANDwe)      # yes = 3, no = 1
table(proval$comp_all4)          # yes = 3, no = 1

# Value chain
table(proval$vchain_bfprod)  # yes = 4
table(proval$vchain_prod)    # yes = 4
table(proval$vchain_proc)    # yes = 1, no = 3
table(proval$vchain_trad)    # yes = 2, no = 2
table(proval$vchain_mktg)    # yes = 1, no = 3
table(proval$vchain_aftprod) # yes = 2, no = 2

proval_vchain_aftprod <- rma(yi, vi, mods = ~ vchain_aftprod, data=proval)
proval_vchain_aftprod

# Aquaculture plus
table(proval$AqcPlus)  # yes = 1, no = 3

# Is the intervention community based, or aimed at individuals?
table(proval$Community)  # yes = 2, no = 2
proval_Community <- rma(yi, vi, mods = ~ Community, data=proval)
proval_Community

# Exposure to intervention (in months)
table(proval$ExpIntMo)
proval_ExpIntMo <- rma(yi, vi, mods = ~ ExpIntMo, data=proval)
proval_ExpIntMo

# Evaluation period (in months)
table(proval$EvalInMo)

# Publication type
table(proval$PubType)
table(proval$peerrev_pub) # yes = 2, no = 2
proval_peerrev_pub <- rma(yi, vi, mods = ~ peerrev_pub, data=proval)
proval_peerrev_pub 

# Year of publication
table(proval$Year)
proval_Year <- rma(yi, vi, mods = ~ Year, data=proval)
proval_Year

# Evaluation design
table(proval$Design2) # QED = 4

# Assumptions made when extracting ES (1=yes, 0=no)
table(proval$Assumed_data) # yes = 3, no = 1

# ROB
table(proval$ROB)  # High RoB = 2, Some concerns = 2
proval_ROB <- rma(yi, vi, mods = ~ ROB, data=proval)
proval_ROB


########################## INCOME ANALYSIS (10) ################################

# Continent
table(income$continent)
table(income$continent2)
income_continent2 <- rma(yi, vi, mods = ~ continent2, data=income)
income_continent2

# Country
table(income$country)
table(income$Bangladesh)
income_Bangladesh <- rma(yi, vi, mods = ~ Bangladesh, data=income)
income_Bangladesh  

# Exposure to climate shocks index
table(income$ExpCliSho)
table(income$ExpCliSho_VH) 
income_ExpCliSho_VH <- rma(yi, vi, mods = ~ ExpCliSho_VH, data=income)
income_ExpCliSho_VH

# World Bank country income group
table(income$WB_incgroup)  # Low = 8, Lower-middle = 2
income_WB_incgroup <- rma(yi, vi, mods = ~ WB_incgroup, data=income)
income_WB_incgroup  

# scale of intervention
table(income$scale)  #  Local = 1, Subnational = 9

# Programme size
table(income$Prog_size)
income_Prog_size <- rma(yi, vi, mods = ~ Prog_size, data=income)
income_Prog_size

# Comparison group
table(income$Comparison2)  # no intervention = 8, pipeline = 2
income_Comparison2 <- rma(yi, vi, mods = ~ Comparison2, data=income)
income_Comparison2 

# Intervention components
table(income$comp_productivity) # yes = 9, no = 1
table(income$comp_income)       # yes = 9, no = 1
table(income$comp_nutrition)    # yes = 5, no = 5
table(income$comp_WE)           # yes = 5, no = 5
table(income$comp_nutORwe)      # yes = 7, no = 3
table(income$comp_nutANDwe)     # yes = 3, no = 7
table(income$comp_all4)         # yes = 3, no = 7

income_comp_nutrition <- rma(yi, vi, mods = ~ comp_nutrition, data=income)
income_comp_nutrition  
income_comp_WE <- rma(yi, vi, mods = ~ comp_WE, data=income)
income_comp_WE  
income_comp_nutORwe <- rma(yi, vi, mods = ~ comp_nutORwe, data=income)
income_comp_nutORwe  
income_comp_nutANDwe <- rma(yi, vi, mods = ~ comp_nutANDwe, data=income)
income_comp_nutANDwe  
income_comp_all4 <- rma(yi, vi, mods = ~ comp_all4, data=income)
income_comp_all4  

# Value chain
table(income$vchain_bfprod)  # yes = 9, no = 1
table(income$vchain_prod)    # yes = 8, no = 2
table(income$vchain_proc)    # yes = 2, no = 8
table(income$vchain_trad)    # yes = 3, no = 7
table(income$vchain_mktg)    # yes = 2, no = 8
table(income$vchain_aftprod) # yes = 3, no = 7

income_vchain_prod <- rma(yi, vi, mods = ~ vchain_prod, data=income)
income_vchain_prod  
income_vchain_proc <- rma(yi, vi, mods = ~ vchain_proc, data=income)
income_vchain_proc  
income_vchain_trad <- rma(yi, vi, mods = ~ vchain_trad, data=income)
income_vchain_trad  
income_vchain_mktg <- rma(yi, vi, mods = ~ vchain_mktg, data=income)
income_vchain_mktg  
income_vchain_aftprod <- rma(yi, vi, mods = ~ vchain_aftprod, data=income)
income_vchain_aftprod  

# Aquaculture plus
table(income$AqcPlus)  # yes = 1, no = 9

# Is the intervention community based, or aimed at individuals?
table(income$Community)  # community = 5, individuals = 5
income_Community <- rma(yi, vi, mods = ~ Community, data=income)
income_Community

# Exposure to intervention (in months)
table(income$ExpIntMo)  # 22=1, 24=1, 30=1, 36=3, 72=1, 99=1
income_ExpIntMo <- rma(yi, vi, mods = ~ ExpIntMo, data=income)
income_ExpIntMo

# Evaluation period (in months)
table(income$EvalInMo) # 0 mo = 8

# Publication type
table(income$PubType) # Peer-reviewed JA = 6, Institutional report = 2, Working paper = 2
income_PubType <- rma(yi, vi, mods = ~ PubType, data=income)
income_PubType  

table(income$peerrev_pub)  # yes = 6, no = 4
income_peerrev_pub <- rma(yi, vi, mods = ~ peerrev_pub, data=income)
income_peerrev_pub

# Year of publication
table(income$Year)
income_Year <- rma(yi, vi, mods = ~ Year, data=income)
income_Year

# Evaluation design
table(income$Design2)  # QED = 10

# Assumptions made when extracting ES (1=yes, 0=no)
table(income$Assumed_data)  # yes = 7, no = 3
income_Assumed_data <- rma(yi, vi, mods = ~ Assumed_data, data=income)
income_Assumed_data

# ROB
table(income$ROB)  # High RoB = 7, Some concerns = 3
income_ROB <- rma(yi, vi, mods = ~ ROB, data=income)
income_ROB 

########################## EXPENDITURE total ANALYSIS (5) ######################

# Continent
table(expend$continent)  # South_Asia = 5
table(expend$continent2) # Asia = 5 

# Country
table(expend$country)    # Bangladesh = 5
table(expend$Bangladesh) # yes = 5

# Exposure to climate shocks index
table(expend$ExpCliSho)    # very high = 5
table(expend$ExpCliSho_VH) # yes = 5

# World Bank country expend group
table(expend$WB_incgroup)  # Low = 4, Lower-middle = 1

# scale of intervention
table(expend$scale) # Local = 1, Subnational = 4 

# Programme size
table(expend$Prog_size)
expend_Prog_size <- rma(yi, vi, mods = ~ Prog_size, data=expend)
expend_Prog_size

# Comparison group
table(expend$Comparison2)  # no intervention = 4, pipeline = 1

# Intervention components
table(expend$comp_productivity)  # yes = 5
table(expend$comp_income)        # yes = 4, no = 1
table(expend$comp_nutrition)     # yes = 2, no = 3
table(expend$comp_WE)            # yes = 3, no = 2
table(expend$comp_nutORwe)       # yes = 4, no = 1
table(expend$comp_nutANDwe)      # yes = 1, no = 4
table(expend$comp_all4)          # yes = 1, no = 4

expend_comp_nutrition <- rma(yi, vi, mods = ~ comp_nutrition, data=expend)
expend_comp_nutrition
expend_comp_WE <- rma(yi, vi, mods = ~ comp_WE, data=expend)
expend_comp_WE

# Value chain
table(expend$vchain_bfprod)   # yes = 4, no = 1
table(expend$vchain_prod)     # yes = 4, no = 1
table(expend$vchain_proc)     # yes = 1, no = 4
table(expend$vchain_trad)     # yes = 1, no = 4
table(expend$vchain_mktg)     # yes = 1, no = 4
table(expend$vchain_aftprod)  # yes = 1, no = 4

# Aquaculture plus
table(expend$AqcPlus)  # yes = 1, no = 4

# Is the intervention community based, or aimed at individuals?
table(expend$Community)  # community = 4, individuals = 1

# Exposure to intervention (in months)
table(expend$ExpIntMo)
expend_ExpIntMo <- rma(yi, vi, mods = ~ ExpIntMo, data=expend)
expend_ExpIntMo

# Evaluation period (in months)
table(expend$EvalInMo)
expend_EvalInMo <- rma(yi, vi, mods = ~ EvalInMo, data=expend)
expend_EvalInMo

# Publication type
table(expend$PubType)
table(expend$peerrev_pub)  # yes = 2, no = 3
expend_peerrev_pub <- rma(yi, vi, mods = ~ peerrev_pub, data=expend)
expend_peerrev_pub

# Year of publication
table(expend$Year)
expend_Year <- rma(yi, vi, mods = ~ Year, data=expend)
expend_Year

# Evaluation design
table(expend$Design2) # QED = 5

# Assumptions made when extracting ES (1=yes, 0=no)
table(expend$Assumed_data)  # yes = 3, no = 2
expend_Assumed_data <- rma(yi, vi, mods = ~ Assumed_data, data=expend)
expend_Assumed_data

# ROB
table(expend$ROB) # High RoB = 1, Some concerns = 4



#### 3. sensitivity analyses ####

########################## Description #################################
# sensitivity analyses leaving each study out were conducted only for 
# outcomes with at least 3 effects.

########################## PRODUCTION VALUE ANALYSIS (4) #######################

# extract overall effect
sens_prval1 <- as.data.frame(c(m_prval$b, m_prval$se, m_prval$zval, m_prval$pval, 
                               m_prval$ci.lb, m_prval$ci.ub, m_prval$QE, m_prval$QEp, 
                               m_prval$tau2, m_prval$I2, m_prval$H2))
library(data.table)
t_sens_prval1 <- transpose(sens_prval1) 
colnames(t_sens_prval1) <- c("estimate", "se", "zval", "pval", "ci.lb", "ci.ub", 
                             "Q", "Qp", "tau2", "I2", "H2")

# conduct sensitivity analysis
sens_prval2 <- as.data.frame(leave1out(m_prval, digits = 4))

# merge overall effect data with sensitivity analysis data
sens_prval3 <- rbind.data.frame(t_sens_prval1, sens_prval2)
sens_names <- as.data.frame(c("Overall effect", "Leave study 1 out", "Leave study 2 out", 
                              "Leave study 3 out", "Leave study 4 out"))
colnames(sens_names) <- c("effect name")
sens_prval4 <- cbind.data.frame(sens_names, sens_prval3)

# forest plot ordered by effect size
forest(x=sens_prval4$estimate, sei=sens_prval4$se, ci.lb=sens_prval4$ci.lb, 
       ci.ub=sens_prval4$ci.ub, slab=sens_prval4$`effect name`, 
       xlim=c(-2,2), at=seq(-0.5, 0.5, .25), digits=c(2,1),
       xlab="Standardized Mean Difference (SMD)", 
       cex=0.8, ylim=c(-1,8))
text(-2, 7, "Sensitivity Analysis leaving each study out", cex=0.8, font=2, pos=4)
text( 2, 7, "SMD [95% CI]", cex=0.8, font=2, pos=2)
title("Effect of Aquaculture Interventions on Production Value", cex.main = 1.1)

# Plots of leave one out analyses
inf2 <- influence(m_prval)
plot(inf2)

########################## PRODUCTION VOLUME ANALYSIS (3) ######################

# extract overall effect
sens_proyld1 <- as.data.frame(c(m_proyld$b, m_proyld$se, m_proyld$zval, m_proyld$pval, 
                                m_proyld$ci.lb, m_proyld$ci.ub, m_proyld$QE, m_proyld$QEp, 
                                m_proyld$tau2, m_proyld$I2, m_proyld$H2))
library(data.table)
t_sens_proyld1 <- transpose(sens_proyld1) 
colnames(t_sens_proyld1) <- c("estimate", "se", "zval", "pval", "ci.lb", "ci.ub", 
                             "Q", "Qp", "tau2", "I2", "H2")

# conduct sensitivity analysis
sens_proyld2 <- as.data.frame(leave1out(m_proyld, digits = 4))

# merge overall effect data with sensitivity analysis data
sens_proyld3 <- rbind.data.frame(t_sens_proyld1, sens_proyld2)
sens_names <- as.data.frame(c("Overall effect", "Leave study 1 out", "Leave study 2 out", 
                              "Leave study 3 out"))
colnames(sens_names) <- c("effect name")
sens_proyld4 <- cbind.data.frame(sens_names, sens_proyld3)

# forest plot ordered by effect size
forest(x=sens_proyld4$estimate, sei=sens_proyld4$se, ci.lb=sens_proyld4$ci.lb, 
       ci.ub=sens_proyld4$ci.ub, slab=sens_proyld4$`effect name`, 
       xlim=c(-2,2), at=seq(-0.5, 0.75, .25), digits=c(2,1),
       xlab="Standardized Mean Difference (SMD)", 
       cex=0.8, ylim=c(-1,7))
text(-2, 6, "Sensitivity Analysis leaving each study out", cex=0.8, font=2, pos=4)
text( 2, 6, "SMD [95% CI]", cex=0.8, font=2, pos=2)
title("Effect of Aquaculture Interventions on Production Volume", cex.main = 1.1)

# Plots of leave one out analyses
inf2 <- influence(m_proyld)
plot(inf2)

########################## INCOME ANALYSIS (10) ################################

# extract overall effect
sens_income1 <- as.data.frame(c(m_income$b, m_income$se, m_income$zval, m_income$pval, 
                                m_income$ci.lb, m_income$ci.ub, m_income$QE, m_income$QEp, 
                                m_income$tau2, m_income$I2, m_income$H2))
library(data.table)
t_sens_income1 <- transpose(sens_income1) 
colnames(t_sens_income1) <- c("estimate", "se", "zval", "pval", "ci.lb", "ci.ub", 
                              "Q", "Qp", "tau2", "I2", "H2")

# conduct sensitivity analysis
sens_income2 <- as.data.frame(leave1out(m_income, digits = 4))

# merge overall effect data with sensitivity analysis data
sens_income3 <- rbind.data.frame(t_sens_income1, sens_income2)
sens_names <- as.data.frame(c("Overall effect", "Leave study 1 out", "Leave study 2 out", 
                              "Leave study 3 out", "Leave study 4 out", "Leave study 5 out", 
                              "Leave study 6 out", "Leave study 7 out", "Leave study 8 out", 
                              "Leave study 9 out", "Leave study 10 out"))
colnames(sens_names) <- c("effect name")
sens_income4 <- cbind.data.frame(sens_names, sens_income3)

# forest plot ordered by effect size
forest(x=sens_income4$estimate, sei=sens_income4$se, ci.lb=sens_income4$ci.lb, 
       ci.ub=sens_income4$ci.ub, slab=sens_income4$`effect name`, 
       xlim=c(-2,2), at=seq(-0.5, 0.5, .25), digits=c(2,1),
       xlab="Standardized Mean Difference (SMD)", 
       cex=0.8, ylim=c(-1,14))
text(-2, 13, "Sensitivity Analysis leaving each study out", cex=0.8, font=2, pos=4)
text( 2, 13, "SMD [95% CI]", cex=0.8, font=2, pos=2)
title("Effect of Aquaculture Interventions on Income", cex.main = 1.1)

#Plots of leave one out analyses
inf <- influence(m_income)
plot(inf)


########################## EXPENDITURE total ANALYSIS (5) ######################

# extract overall effect
sens_expend1 <- as.data.frame(c(m_expend$b, m_expend$se, m_expend$zval, m_expend$pval, 
                                m_expend$ci.lb, m_expend$ci.ub, m_expend$QE, m_expend$QEp, 
                                m_expend$tau2, m_expend$I2, m_expend$H2))
library(data.table)
t_sens_expend1 <- transpose(sens_expend1) 
colnames(t_sens_expend1) <- c("estimate", "se", "zval", "pval", "ci.lb", "ci.ub", 
                              "Q", "Qp", "tau2", "I2", "H2")

# conduct sensitivity analysis
sens_expend2 <- as.data.frame(leave1out(m_expend, digits = 4))

# merge overall effect data with sensitivity analysis data
sens_expend3 <- rbind.data.frame(t_sens_expend1, sens_expend2)
sens_names <- as.data.frame(c("Overall effect", "Leave study 1 out", "Leave study 2 out", 
                              "Leave study 3 out", "Leave study 4 out", "Leave study 5 out"))
colnames(sens_names) <- c("effect name")
sens_expend4 <- cbind.data.frame(sens_names, sens_expend3)

# forest plot ordered by effect size
forest(x=sens_expend4$estimate, sei=sens_expend4$se, ci.lb=sens_expend4$ci.lb, 
       ci.ub=sens_expend4$ci.ub, slab=sens_expend4$`effect name`, 
       xlim=c(-2,2), at=seq(-0.5, 0.5, .25), digits=c(2,1),
       xlab="Standardized Mean Difference (SMD)", 
       cex=0.8, ylim=c(-1,9))
text(-2, 8, "Sensitivity Analysis leaving each study out", cex=0.8, font=2, pos=4)
text( 2, 8, "SMD [95% CI]", cex=0.8, font=2, pos=2)
title("Effect of Aquaculture Interventions on Total Expenditures", cex.main = 1.1)

# Plots of leave one out analyses
inf3 <- influence(m_expend)
plot(inf3)


########################## Farm Profit ANALYSIS (3) ############################ 

# extract overall effect
sens_profit1 <- as.data.frame(c(m_profit$b, m_profit$se, m_profit$zval, m_profit$pval, 
                                m_profit$ci.lb, m_profit$ci.ub, m_profit$QE, m_profit$QEp, 
                                m_profit$tau2, m_profit$I2, m_profit$H2))
library(data.table)
t_sens_profit1 <- transpose(sens_profit1) 
colnames(t_sens_profit1) <- c("estimate", "se", "zval", "pval", "ci.lb", "ci.ub", 
                              "Q", "Qp", "tau2", "I2", "H2")

# conduct sensitivity analysis
sens_profit2 <- as.data.frame(leave1out(m_profit, digits = 4))

# merge overall effect data with sensitivity analysis data
sens_profit3 <- rbind.data.frame(t_sens_profit1, sens_profit2)
sens_names <- as.data.frame(c("Overall effect", "Leave study 1 out", "Leave study 2 out", 
                              "Leave study 3 out"))
colnames(sens_names) <- c("effect name")
sens_profit4 <- cbind.data.frame(sens_names, sens_profit3)

# forest plot ordered by effect size
forest(x=sens_profit4$estimate, sei=sens_profit4$se, ci.lb=sens_profit4$ci.lb, 
       ci.ub=sens_profit4$ci.ub, slab=sens_profit4$`effect name`, 
       xlim=c(-2,2), at=seq(-0.5, 0.75, .25), digits=c(2,1),
       xlab="Standardized Mean Difference (SMD)", 
       cex=0.8, ylim=c(-1,7))
text(-2, 6, "Sensitivity Analysis leaving each study out", cex=0.8, font=2, pos=4)
text( 2, 6, "SMD [95% CI]", cex=0.8, font=2, pos=2)
title("Effect of Aquaculture Interventions on Farm Profit", cex.main = 1.1)

# Plots of leave one out analyses
inf3 <- influence(m_profit)
plot(inf3)


########################## WOMEN's BMI ANALYSIS (3)########################## 

# extract overall effect
sens_wwomen1 <- as.data.frame(c(m_wwomen$b, m_wwomen$se, m_wwomen$zval, m_wwomen$pval, 
                                m_wwomen$ci.lb, m_wwomen$ci.ub, m_wwomen$QE, m_wwomen$QEp, 
                                m_wwomen$tau2, m_wwomen$I2, m_wwomen$H2))
library(data.table)
t_sens_wwomen1 <- transpose(sens_wwomen1) 
colnames(t_sens_wwomen1) <- c("estimate", "se", "zval", "pval", "ci.lb", "ci.ub", 
                              "Q", "Qp", "tau2", "I2", "H2")

# conduct sensitivity analysis
sens_wwomen2 <- as.data.frame(leave1out(m_wwomen, digits = 4))

# merge overall effect data with sensitivity analysis data
sens_wwomen3 <- rbind.data.frame(t_sens_wwomen1, sens_wwomen2)
sens_names <- as.data.frame(c("Overall effect", "Leave study 1 out", "Leave study 2 out", 
                              "Leave study 3 out"))
colnames(sens_names) <- c("effect name")
sens_wwomen4 <- cbind.data.frame(sens_names, sens_wwomen3)

# forest plot ordered by effect size
forest(x=sens_wwomen4$estimate, sei=sens_wwomen4$se, ci.lb=sens_wwomen4$ci.lb, 
       ci.ub=sens_wwomen4$ci.ub, slab=sens_wwomen4$`effect name`, 
       xlim=c(-2,2), at=seq(-0.5, 0.5, .25), digits=c(2,1),
       xlab="Standardized Mean Difference (SMD)", 
       cex=0.8, ylim=c(-1,7))
text(-2, 6, "Sensitivity Analysis leaving each study out", cex=0.8, font=2, pos=4)
text( 2, 6, "SMD [95% CI]", cex=0.8, font=2, pos=2)
title("Effect of Aquaculture Interventions on Women's BMI", cex.main = 1.1)

# Plots of leave one out analyses
inf3 <- influence(m_wwomen)
plot(inf3)

########################## CHILDREN's HAZ ANALYSIS (3) ############################ 

# extract overall effect
sens_wchild1 <- as.data.frame(c(m_wchild$b, m_wchild$se, m_wchild$zval, m_wchild$pval, 
                                m_wchild$ci.lb, m_wchild$ci.ub, m_wchild$QE, m_wchild$QEp, 
                                m_wchild$tau2, m_wchild$I2, m_wchild$H2))
library(data.table)
t_sens_wchild1 <- transpose(sens_wchild1) 
colnames(t_sens_wchild1) <- c("estimate", "se", "zval", "pval", "ci.lb", "ci.ub", 
                              "Q", "Qp", "tau2", "I2", "H2")

# conduct sensitivity analysis
sens_wchild2 <- as.data.frame(leave1out(m_wchild, digits = 4))

# merge overall effect data with sensitivity analysis data
sens_wchild3 <- rbind.data.frame(t_sens_wchild1, sens_wchild2)
sens_names <- as.data.frame(c("Overall effect", "Leave study 1 out", "Leave study 2 out", 
                              "Leave study 3 out"))
colnames(sens_names) <- c("effect name")
sens_wchild4 <- cbind.data.frame(sens_names, sens_wchild3)

# forest plot ordered by effect size
forest(x=sens_wchild4$estimate, sei=sens_wchild4$se, ci.lb=sens_wchild4$ci.lb, 
       ci.ub=sens_wchild4$ci.ub, slab=sens_wchild4$`effect name`, 
       xlim=c(-2,2), at=seq(-0.5, 0.5, .25), digits=c(2,1),
       xlab="Standardized Mean Difference (SMD)", 
       cex=0.8, ylim=c(-1,7))
text(-2, 6, "Sensitivity Analysis leaving each study out", cex=0.8, font=2, pos=4)
text( 2, 6, "SMD [95% CI]", cex=0.8, font=2, pos=2)
title("Effect of Aquaculture Interventions on Children's HAZ (0-5 yo)", cex.main = 1.0)

# Plots of leave one out analyses
inf3 <- influence(m_wchild)
plot(inf3)


#### 4. RVE analysis ####
########################## Description #################################

# Outcome Group 1: production and productivity
# there is small number of studies for this group (n = 6), so presenting 
# RVE analyses is not really informative; better to present right away the 
# traditional MA analyses using specific outcomes.

# Outcome Group 2: income
# this is the only group for which we have a higher number of studies (n = 15), 
# and following the advisory group's suggestion, we will present some general 
# analyses for this group using "constructs" such as income+expenditures+profit.

# Outcome Group 3: nutrition
# there is small number of studies for this group (n = 6), so presenting 
# RVE analyses is not really informative; better to present right away the 
# traditional MA analyses using specific outcomes.

# Outcome Group 4: women's empowerment
# there is a very small number of studies for this group (n = 3), so we will 
# present this section narratively. 

########################## income + expenditure (total and food) + profit outcomes (53 ES) ####

rve_income <- aqcdata[ which(aqcdata$Income==1),]        # 128 obs
rve_income <- rve_income[ which(!is.na(rve_income$d)),]  # 120 obs
rve_income <- rve_income[ which(!is.na(rve_income$yi)),] # 120 obs

rve_income$Income4 <- NA
rve_income$Income4[which(rve_income$Inc==1)] <- 1 
rve_income$Income4[which(rve_income$`Expenditure total`==1)] <- 1 
rve_income$Income4[which(rve_income$`Expenditure food`==1)] <- 1 
rve_income$Income4[which(rve_income$Profit==1)] <- 1 
rve_income4 <- rve_income[ which(rve_income$Income4==1),]  # 53 obs

rvemod_4 <- robu(formula = yi ~ 1, var.eff.size = vi, studynum = rve_income4$`Study ID`, 
                 data = rve_income4, modelweights = "CORR", rho = 0.8)
rvemod_4
sensitivity(rvemod_4)

########################## Moderator analyses ####
# done one at a time rather that all in one analysis to avoid 
# issues of multicolinearity.

# Continent
table(rve_income4$continent2)  
mod4_continent2 <- robu(yi ~ continent2, data = rve_income4, modelweights = "HIER", 
                        studynum = rve_income4$`Study ID`, var.eff.size = vi, small = TRUE)
mod4_continent2

# Country
table(rve_income4$Bangladesh) 
mod4_Bangladesh <- robu(yi ~ Bangladesh, data = rve_income4, modelweights = "HIER", 
                        studynum = rve_income4$`Study ID`, var.eff.size = vi, small = TRUE)
mod4_Bangladesh

# Exposure to climate shocks index
table(rve_income4$ExpCliSho_VH) 
mod4_ExpCliSho_VH <- robu(yi ~ ExpCliSho_VH, data = rve_income4, modelweights = "HIER", 
                          studynum = rve_income4$`Study ID`, var.eff.size = vi, small = TRUE)
mod4_ExpCliSho_VH

# World Bank country income group
table(rve_income4$WB_incgroup) 
mod4_WB_incgroup <- robu(yi ~ WB_incgroup, data = rve_income4, modelweights = "HIER", 
                         studynum = rve_income4$`Study ID`, var.eff.size = vi, small = TRUE)
mod4_WB_incgroup

# scale of intervention
table(rve_income4$scale)  
mod4_scale <- robu(yi ~ scale, data = rve_income4, modelweights = "HIER", 
                   studynum = rve_income4$`Study ID`, var.eff.size = vi, small = TRUE)
mod4_scale

# Programme size
table(rve_income4$Prog_size)
mod4_Prog_size <- robu(yi ~ Prog_size, data = rve_income4, modelweights = "HIER", 
                       studynum = rve_income4$`Study ID`, var.eff.size = vi, small = TRUE)
mod4_Prog_size

# Comparison group
table(rve_income4$Comparison2) 
mod4_Comparison2 <- robu(yi ~ Comparison2, data = rve_income4, modelweights = "HIER", 
                         studynum = rve_income4$`Study ID`, var.eff.size = vi, small = TRUE)
mod4_Comparison2

# Intervention components
table(rve_income4$comp_productivity)
mod4_comp_productivity <- robu(yi ~ comp_productivity, data = rve_income4, modelweights = "HIER", 
                               studynum = rve_income4$`Study ID`, var.eff.size = vi, small = TRUE)
mod4_comp_productivity

table(rve_income4$comp_income)
mod4_comp_income <- robu(yi ~ comp_income, data = rve_income4, modelweights = "HIER", 
                         studynum = rve_income4$`Study ID`, var.eff.size = vi, small = TRUE)
mod4_comp_income

table(rve_income4$comp_nutrition)
mod4_comp_nutrition <- robu(yi ~ comp_nutrition, data = rve_income4, modelweights = "HIER", 
                            studynum = rve_income4$`Study ID`, var.eff.size = vi, small = TRUE)
mod4_comp_nutrition

table(rve_income4$comp_WE)
mod4_comp_WE <- robu(yi ~ comp_WE, data = rve_income4, modelweights = "HIER", 
                     studynum = rve_income4$`Study ID`, var.eff.size = vi, small = TRUE)
mod4_comp_WE

table(rve_income4$comp_nutORwe)
mod4_comp_nutORwe <- robu(yi ~ comp_nutORwe, data = rve_income4, modelweights = "HIER", 
                          studynum = rve_income4$`Study ID`, var.eff.size = vi, small = TRUE)
mod4_comp_nutORwe

table(rve_income4$comp_nutANDwe)
mod4_comp_nutANDwe <- robu(yi ~ comp_nutANDwe, data = rve_income4, modelweights = "HIER",
                           studynum = rve_income4$`Study ID`, var.eff.size = vi, small = TRUE)
mod4_comp_nutANDwe

table(rve_income4$comp_all4)
mod4_comp_all4 <- robu(yi ~ comp_all4, data = rve_income4, modelweights = "HIER", 
                       studynum = rve_income4$`Study ID`, var.eff.size = vi, small = TRUE)
mod4_comp_all4


# Value chain
table(rve_income4$vchain_bfprod)
mod4_vchain_bfprod <- robu(yi ~ vchain_bfprod, data = rve_income4, modelweights = "HIER",
                           studynum = rve_income4$`Study ID`, var.eff.size = vi, small = TRUE)
mod4_vchain_bfprod

table(rve_income4$vchain_prod)
mod4_vchain_prod <- robu(yi ~ vchain_prod, data = rve_income4, modelweights = "HIER", 
                         studynum = rve_income4$`Study ID`, var.eff.size = vi, small = TRUE)
mod4_vchain_prod

table(rve_income4$vchain_proc)
mod4_vchain_proc <- robu(yi ~ vchain_proc, data = rve_income4, modelweights = "HIER", 
                         studynum = rve_income4$`Study ID`, var.eff.size = vi, small = TRUE)
mod4_vchain_proc

table(rve_income4$vchain_trad)
mod4_vchain_trad <- robu(yi ~ vchain_trad, data = rve_income4, modelweights = "HIER", 
                         studynum = rve_income4$`Study ID`, var.eff.size = vi, small = TRUE)
mod4_vchain_trad

table(rve_income4$vchain_mktg)
mod4_vchain_mktg <- robu(yi ~ vchain_mktg, data = rve_income4, modelweights = "HIER", 
                         studynum = rve_income4$`Study ID`, var.eff.size = vi, small = TRUE)
mod4_vchain_mktg

table(rve_income4$vchain_aftprod)
mod4_vchain_aftprod <- robu(yi ~ vchain_aftprod, data = rve_income4, modelweights = "HIER", 
                            studynum = rve_income4$`Study ID`, var.eff.size = vi, small = TRUE)
mod4_vchain_aftprod


# Aquaculture plus
table(rve_income4$AqcPlus)
mod4_AqcPlus <- robu(yi ~ AqcPlus, data = rve_income4, modelweights = "HIER", 
                     studynum = rve_income4$`Study ID`, var.eff.size = vi, small = TRUE)
mod4_AqcPlus

# Is the intervention community based, or aimed at individuals?
table(rve_income4$Community)  
mod4_Community <- robu(yi ~ Community, data = rve_income4, modelweights = "HIER", 
                       studynum = rve_income4$`Study ID`, var.eff.size = vi, small = TRUE)
mod4_Community

# Exposure to intervention (in months)
table(rve_income4$ExpIntMo)
mod4_ExpIntMo <- robu(yi ~ ExpIntMo, data = rve_income4, modelweights = "HIER", 
                      studynum = rve_income4$`Study ID`, var.eff.size = vi, small = TRUE)
mod4_ExpIntMo

# Evaluation period (in months)
table(rve_income4$EvalInMo)
mod4_EvalInMo <- robu(yi ~ EvalInMo, data = rve_income4, modelweights = "HIER", 
                      studynum = rve_income4$`Study ID`, var.eff.size = vi, small = TRUE)
mod4_EvalInMo

# Publication type
table(rve_income4$peerrev_pub)
mod4_peerrev_pub <- robu(yi ~ peerrev_pub, data = rve_income4, modelweights = "HIER", 
                         studynum = rve_income4$`Study ID`, var.eff.size = vi, small = TRUE)
mod4_peerrev_pub

# Year of publication
table(rve_income4$Year)
mod4_Year <- robu(yi ~ Year, data = rve_income4, modelweights = "HIER", 
                  studynum = rve_income4$`Study ID`, var.eff.size = vi, small = TRUE)
mod4_Year

# Evaluation design
table(rve_income4$Design2) # QED = 53, no variation

# Assumptions made when extracting ES (1=yes, 0=no)
table(rve_income4$Assumed_data)
mod4_Assumed_data <- robu(yi ~ Assumed_data, data = rve_income4, modelweights = "HIER", 
                          studynum = rve_income4$`Study ID`, var.eff.size = vi, small = TRUE)
mod4_Assumed_data

# ROB
table(rve_income4$ROB)  # High RoB = 35, Some concerns = 16
mod4_rob <- robu(yi ~ ROB, data = rve_income4, modelweights = "HIER", 
                 studynum = rve_income4$`Study ID`, var.eff.size = vi, small = TRUE)
mod4_rob



#### end ####