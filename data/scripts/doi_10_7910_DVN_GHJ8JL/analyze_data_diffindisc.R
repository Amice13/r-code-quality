### Replication code
### Article: "How patronage delivers: Political appointments, bureaucratic accountability, and service delivery in Brazil"
### Author: Guillermo Toral (www.guillermotoral.com)
### Date: June 2022
### This file analyzes the difference-in-discontinuities data, generating plots and tables
### R version, platform, and package versions reported at the end of the file

# Prepare the environment -------------------------------------------------

### This section of the code prepares the environment 

# Clean the environment
rm(list = ls())

# Install required packages if not previously installed
package_list <- c("tidyverse", "rdrobust", "rdd", "multcomp", "sandwich", "lfe", "Matching", "ebal", "xtable", "texreg", "here", "Hmisc") 
packages_to_install <- package_list[!(package_list %in% installed.packages()[,"Package"])]
if(length(packages_to_install)>0){
  install.packages(packages_to_install)
}

# Load required packages
library(tidyverse)
library(rdrobust)
library(rdd)
library(multcomp)
library(sandwich)
library(lfe)
library(Matching)
library(ebal)
library(xtable)
library(texreg)
library(here)
library(Hmisc)

# Set Working Directory to wherever this file is located.
setwd(here())
# The directory where the code folder is located must also have a "plots" and a "tables" subdirectory

# Load datasets -----------------------------------------------------------

# School-level dataset
s <- read_csv("../../datasets/analysis/diffindisc/diffindisc_analysis.csv")

# Municipality-level dataset
m <- read_csv("../../datasets/analysis/other/municipal_elections_2016.csv")

# Subset of the school-level dataset for the diffindisc models, keeping only schools where directors were deployed under the previous administration but who are still in their post after the election
dd <- subset(s, (s$director_here_lessthan3yr_2015==1) & (s$director_here_1to2yr_2017==1 | s$director_here_3to5yr_2017==1))

# Load function to generate RD plots ---------------------------------------------

myrdplot <- function(x, y, cutpoint=0, maintitle="", xtitle="", ytitle="", xrange=c(-1,1),
                     yrange=c(0,1), bins=100, legendabove="", legendbelow="", titlesize=1){
  lobelow <- loess(y~x,subset = x < cutpoint)
  loabove <- loess(y~x,subset = x > cutpoint)
  xl <- seq(min(x,na.rm=T),max(x,na.rm=T), (max(x,na.rm=T) - min(x,na.rm=T))/1000)
  xbelow <- seq(xrange[1],cutpoint,(xrange[2] - xrange[1])/900)
  xabove <- seq(cutpoint,xrange[2],(xrange[2] - xrange[1])/900)
  pred.below <- predict(lobelow,xbelow,se=T)
  pred.above <- predict(loabove,xabove,se=T)
  minbelow <- pred.below$fit - pred.below$s*1.96
  maxbelow <- pred.below$fit + pred.below$s*1.96
  minabove <- pred.above$fit - pred.above$s*1.96
  maxabove <- pred.above$fit + pred.above$s*1.96
  xsubset <- subset(x,x<xrange[2] & x > xrange[1])
  sub <- as.data.frame(cbind(x,y))
  sublow <- sub[which(sub$x > xrange[1] & sub$x < cutpoint),]
  subhigh <- sub[which(sub$x < xrange[2] & sub$x > cutpoint),]
  require(Hmisc)
  minx <- rep(NA,bins)
  maxx <- rep(NA,bins)
  meany <- rep(NA,bins)
  means <- as.data.frame(cbind(minx,maxx,meany))
  for(i in 1:(bins/2)){
    means$minx[i] <- min(split(sublow, cut2(sublow$x, g=bins/2))[[i]][[1]],na.rm=T)
    means$maxx[i] <- max(split(sublow, cut2(sublow$x, g=bins/2))[[i]][[1]],na.rm=T)
    means$meany[i] <- mean(split(sublow,cut2(sublow$x, g=bins/2))[[i]][[2]],na.rm=T)
  }
  for(i in 1:(bins/2)){
    means$minx[i+bins/2] <- min(split(subhigh, cut2(subhigh$x, g=bins/2))[[i]][[1]],na.rm=T)
    means$maxx[i+bins/2] <- max(split(subhigh, cut2(subhigh$x, g=bins/2))[[i]][[1]],na.rm=T)
    means$meany[i+bins/2] <- mean(split(subhigh,cut2(subhigh$x, g=bins/2))[[i]][[2]],na.rm=T)
  }
  means$center <- (means$minx+means$maxx)/2
  plot(x,y,pch=20,col="lightgray",main=maintitle,
       xlab=xtitle,
       ylab=ytitle,type="p",cex=.8,
       xlim=xrange,ylim=yrange, cex.lab=1.2, cex.main=titlesize)
  lines(xbelow, pred.below$fit, col='black', lwd=2)
  lines(xabove, pred.above$fit, col='black', lwd=2)
  rug(x,col="black",ticksize=0.03,lwd=0.3)
  polygon(c(xabove,rev(xabove)),c(maxabove,rev(minabove)),
          col=adjustcolor("darkgreen",alpha.f=0.3),border="white")
  polygon(c(rev(xbelow),xbelow),c(rev(maxbelow),minbelow),
          col=adjustcolor("red",alpha.f=0.3),border="white")
  abline(v=cutpoint,col="darkblue",lty=2,lwd=2)
  lines(means$center[1:bins/2],means$meany[1:bins/2],col="red",type="p",pch=19,cex=.8)
  lines(means$center[bins/2+1:bins],means$meany[bins/2+1:bins],col="darkgreen",type="p",pch=19,cex=.8)
  legend(x=cutpoint,y=yrange[2],legend = legendabove, bty ="n", pch=NA, text.col="darkgreen",text.width=3, cex=1.2) 
  legend(x=xrange[1],y=yrange[2],legend = legendbelow, bty ="n", pch=NA, text.col="red",text.width=3, cex=1.2) 
}

# Generate Table 2 (diff-in-disc: school quality scores) --------

# Generate dummies for appointed and treated (mayor lost), and the forcing variable, and their interactions
dd$appointed <- dd$director_appointed
dd$fv <- dd$challenger_margin
dd$treated <- dd$mayor_lost
dd$treated_fv_appointed <- dd$treated*dd$fv*dd$appointed
dd$treated_appointed <- dd$treated*dd$appointed
dd$fv_appointed <- dd$fv*dd$appointed

# Obtain the optimal bandwidth, subset data to observations within the bandwidth, and create the corresponding trinagular kernel weights for models 2-4 (with interaction treated*appointed)
## I calculate kernel weights by hand but regression results are the same using the canned function kernelwts (in the "rdd" package)
bw_calonico <- rdbwselect(dd$ideb_delta, x=dd$fv, covs = dd$treated_fv_appointed + dd$treated_appointed + dd$fv_appointed + dd$appointed, c= 0, cluster=dd$cod_ibge)
bw <- bw_calonico$bws[[1,1]]
dd_int <- subset(dd, abs(dd$fv)<bw) # dd_int is the dataset with observations within the bandwidth, when including the interaction treated*appointed, with the corresponding kernel weights
dd_int$w <- as.numeric(NA)
dd_int[which(dd_int$fv < 0),"w"] <- 1-abs(dd_int[which(dd_int$fv < 0),"fv"]/bw)
dd_int[which(dd_int$fv >= 0),"w"] <- 1-abs(dd_int[which(dd_int$fv >= 0),"fv"]/bw)

# Obtain the optimal bandwidth, subset data to observations within the bandwidth, and create the corresponding trinagular kernel weights for model 1 (no interaction)
bw_calonico_noint <- rdbwselect(dd$ideb_delta, x=dd$fv, c= 0, cluster=dd$cod_ibge)
bw_noint <- bw_calonico_noint$bws[[1,1]] # 0.21
dd_noint <- subset(dd, abs(dd$fv)<bw) # dd_noint is the dataset with observations within the bandwidth, when excluding the interaction treated*appointed, with the corresponding kernel weights
dd_noint$w <- as.numeric(NA)
dd_noint[which(dd_noint$fv < 0),"w"] <- 1-abs(dd_noint[which(dd_noint$fv < 0),"fv"]/bw)
dd_noint[which(dd_noint$fv >= 0),"w"] <- 1-abs(dd_noint[which(dd_noint$fv >= 0),"fv"]/bw)

r1 <- felm(ideb_delta ~ treated*fv | 0 | 0 | cod_ibge, data=dd_noint, weights=dd_noint$w)
r2 <- felm(ideb_delta ~ treated*fv*appointed | 0 | 0 | cod_ibge, data=dd_int, weights=dd_int$w)
# Express coefficient of interest in standard deviations
summary(r2)$coefficients["treated:appointed",1]/sd(s$ideb_2015,na.rm=T)
r3 <- felm(ideb_delta ~ treated*fv*appointed | uf | 0 | cod_ibge,  data=dd_int, weights=dd_int$w)
r4 <- felm(ideb_delta ~ treated*fv*appointed 
           + female + age_forties + race_white + race_black_or_brown + schooling_postgraduate + exclusive_dedication + teacher_experience_6to15yr + director_experience_3to10yr + director_experience_over10yr
           + log_gdp_percapita + log_population + deaths_perthousand + electoral_concentration 
           + rural + students_per_classroom + school_inse
           | uf | 0 | cod_ibge,  data=dd_int, weights=dd_int$w)

texreg(list(r1,r2,r3,r4),
       file="../../tables/diffindisc.tex", table=F, float.pos="htp", scalebox=.75,
       digits=3,booktabs = T, dcolumn=T, use.packages=F,single.row = F,custom.note="",
       custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
       custom.coef.map = list("treated" = "$\\hat \\beta_1$: Political turnover", "treated:appointed" = "$\\hat \\gamma_2$: Political turnover $\\times$ Appointed"),
       custom.gof.names = c("N"),
       include.adjrs=FALSE, include.rsq=F, include.groups=F, include.n=T,
       stars = c(0.05, 0.01), 
       custom.gof.rows = list("State fixed effects" = c("","",rep("$\\checkmark$",2)),
                              "Predictors of Appointed" = c(rep("",3),"$\\checkmark$"),
                              "Bandwidth" = c(bw_calonico_noint$bws[[1]], rep(bw_calonico$bws[[1]],3))))

# Generate Figure 2 (RD plots for appointed and unappointed directors) -----------------------------------------------

dd_appointed <- subset(dd, dd$appointed==1 & !is.na(dd$fv))
dd_notappointed <- subset(dd, dd$appointed==0 & !is.na(dd$fv))

pdf("../../plots/diffindisc.pdf",width=12,height=5)
par(mfrow=c(1,2))
myrdplot(x = dd_appointed$fv, 
          y = dd_appointed$ideb_delta, cutpoint = 0,
          xrange = c(-0.5,0.5), yrange=c(-0.075,0.65),bins=18,
          xtitle = "Challenger vote share - incumbent vote share (2016)",
          ytitle = "Change in school quality (2015-17)", 
          legendabove="Political turnover",
          legendbelow="No political turnover")
          # maintitle="Appointed directors", titlesize=1.1
grid()
myrdplot(x = dd_notappointed$fv, 
          y = dd_notappointed$ideb_delta, cutpoint = 0,
          xrange = c(-0.5,0.5), yrange=c(-0.075,0.65),bins=18,
          xtitle = "Challenger vote share - incumbent vote share (2016)",
          ytitle = "Change in school quality (2015-17)",  
          legendabove="Political turnover",
          legendbelow="No political turnover")
          # maintitle="Unappointed directors",titlesize=1.1
grid()
dev.off()

# Generate Figure 3, left-hand panel (results with alternative bandwidths) --------------------------------------------------

# Create empty vectors to store the bandwidths and the corresponding diff-in-disc estimates and their confidence intervals
bw <- c()
did_estimate <- c()
cilow <- c()
ciup <- c()

# Iterate through alternative bandwidths, from half to twice the optimal one
for (i in seq(from=bw_calonico$bws[[1]]/2, to=bw_calonico$bws[[1]]*2, by=0.0025)){ 
  # Store the value of the bandwidth
  bw <- c(bw,i) 
  # Create dataset within the bandwidth and assign kernel weights
  dd_bw <- subset(dd, abs(dd$fv)<i)
  dd_bw$w <- as.numeric(NA)
  dd_bw[which(dd_bw$fv < 0),"w"] <- 1-abs(dd_bw[which(dd_bw$fv < 0),"fv"]/i)
  dd_bw[which(dd_bw$fv >= 0),"w"] <- 1-abs(dd_bw[which(dd_bw$fv >= 0),"fv"]/i)
  r <- felm(ideb_delta ~ treated*fv*appointed 
            + female + age_forties + race_white + race_black_or_brown + schooling_postgraduate + exclusive_dedication + teacher_experience_6to15yr + director_experience_3to10yr + director_experience_over10yr
            + log_gdp_percapita + log_population + deaths_perthousand + electoral_concentration 
            + rural + students_per_classroom + school_inse
            | uf | 0 | cod_ibge, data=dd_bw, weights=dd_bw$w)
  did_estimate <- c(did_estimate, summary(r)$coef["treated:appointed",1])

  cilow <- c(cilow, confint(r)["treated:appointed",1])
  ciup <- c(ciup,  confint(r)["treated:appointed",2])
}

results <- data.frame(cbind(bw,did_estimate,cilow,ciup))

pdf("../../plots/diffindisc_bandwidths.pdf",width=6, height=5)
par(mar=c(5.1, 5.1, 2.1, 2.1))
plot(results$bw, results$did_estimate,
     ylim=c(-1,0.3),
     xlab="Bandwidth",
     ylab="Difference-in-discontinuities, \n appointed vs uappointed directors",pch=19)
     # main="School quality score"
grid()
arrows(results$bw, results$cilow, results$bw, results$ciup, length=0.05, angle=90, code=3,col="darkgrey")
abline(h=0,col="red",lty=2)
abline(v=bw_calonico$bws[[1,1]],col="blue",lwd=2)
legend("bottomright",c("Optimal bandwidth"),lwd=2,col="blue",bty="n")
dev.off()

# Generate Table 3 (diff-in-disc: support from above) --------

# Obtain the optimal bandwidth, subset data to observations within the bandwidth, and create the corresponding trinagular kernel weights for models 2-4 (with interaction treated*appointed)
bw_calonico <- rdbwselect(dd$support_from_above_delta, x=dd$fv, covs = dd$treated_fv_appointed + dd$treated_appointed + dd$fv_appointed + dd$appointed, c= 0, cluster=dd$cod_ibge)
bw <- bw_calonico$bws[[1,1]]
dd_int <- subset(dd, abs(dd$fv)<bw)
dd_int$w <- as.numeric(NA)
dd_int[which(dd_int$fv < 0),"w"] <- 1-abs(dd_int[which(dd_int$fv < 0),"fv"]/bw)
dd_int[which(dd_int$fv >= 0),"w"] <- 1-abs(dd_int[which(dd_int$fv >= 0),"fv"]/bw)

# Obtain the optimal bandwidth, subset data to observations within the bandwidth, and create the corresponding trinagular kernel weights for model 1 (without interaction treated*appointed)
bw_calonico_noint <- rdbwselect(dd$support_from_above_delta, x=dd$fv, c= 0, cluster=dd$cod_ibge)
bw_noint <- bw_calonico_noint$bws[[1,1]] # 0.21
dd_noint <- subset(dd, abs(dd$fv)<bw)
dd_noint$w <- as.numeric(NA)
dd_noint[which(dd_noint$fv < 0),"w"] <- 1-abs(dd_noint[which(dd_noint$fv < 0),"fv"]/bw)
dd_noint[which(dd_noint$fv >= 0),"w"] <- 1-abs(dd_noint[which(dd_noint$fv >= 0),"fv"]/bw)

r1 <- felm(support_from_above_delta ~ treated*fv | 0 | 0 | cod_ibge, data=dd_noint, weights=dd_noint$w)
r2 <- felm(support_from_above_delta ~ treated*fv*appointed | 0 | 0 | cod_ibge, data=dd_int, weights=dd_int$w)
r3 <- felm(support_from_above_delta ~ treated*fv*appointed | uf | 0 | cod_ibge, data=dd_int, weights=dd_int$w)
r4 <- felm(support_from_above_delta ~ treated*fv*appointed 
           + female + age_forties + race_white + race_black_or_brown + schooling_postgraduate + exclusive_dedication + teacher_experience_6to15yr + director_experience_3to10yr + director_experience_over10yr
           + log_gdp_percapita + log_population + deaths_perthousand + electoral_concentration 
           + rural + students_per_classroom + school_inse
           | uf | 0 | cod_ibge, data=dd_int, weights=dd_int$w)

texreg(list(r1,r2,r3,r4),
       file="../../tables/diffindisc_supportfromabove.tex", table=F, float.pos="htp", scalebox=.75,
       digits=3,booktabs = T, dcolumn=T,use.packages=F,single.row = F,custom.note="",
       custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
       custom.coef.map = list("treated" = "$\\hat \\beta_1$: Political turnover", "treated:appointed" = "$\\hat \\gamma_2$: Political turnover $\\times$ Appointed"),
       custom.gof.names = c("N"),
       include.adjrs=FALSE, include.rsq=F, include.groups=F, include.n=T,
       stars = c(0.05, 0.01),
       custom.gof.rows = list("State fixed effects" = c("","",rep("$\\checkmark$",2)),
                              "Predictors of Appointed" = c(rep("",3),"$\\checkmark$"),
                              "Bandwidth" = c(bw_calonico_noint$bws[[1]], rep(bw_calonico$bws[[1]],3))))

# Generate Figure 3, right-hand panel (alternative bandwidths) --------

# Create empty vectors to store the bandwidths and the corresponding diff-in-disc estimates and their confidence intervals
bw <- c()
did_estimate <- c()
cilow <- c()
ciup <- c()

# Iterate through alternative bandwidths, from half to twice the optimal one
for (i in seq(from=bw_calonico$bws[[1]]/2, to=bw_calonico$bws[[1]]*2, by=0.0025)){ 
  # Store the value of the bandwidth
  bw <- c(bw,i) 
  # Create dataset within the bandwidth and assign kernel weights
  dd_bw <- subset(dd, abs(dd$fv)<i)
  dd_bw$w <- as.numeric(NA)
  dd_bw[which(dd_bw$fv < 0),"w"] <- 1-abs(dd_bw[which(dd_bw$fv < 0),"fv"]/i)
  dd_bw[which(dd_bw$fv >= 0),"w"] <- 1-abs(dd_bw[which(dd_bw$fv >= 0),"fv"]/i)
  r <- felm(support_from_above_delta ~ treated*fv*appointed 
            + female + age_forties + race_white + race_black_or_brown + schooling_postgraduate + exclusive_dedication + teacher_experience_6to15yr + director_experience_3to10yr + director_experience_over10yr
            + log_gdp_percapita + log_population + deaths_perthousand + electoral_concentration 
            + rural + students_per_classroom + school_inse
            | uf | 0 | cod_ibge, data=dd_bw, weights=dd_bw$w)
  did_estimate <- c(did_estimate, summary(r)$coef["treated:appointed",1])
  
  cilow <- c(cilow, confint(r)["treated:appointed",1])
  ciup <- c(ciup,  confint(r)["treated:appointed",2])
}

results <- data.frame(cbind(bw,did_estimate,cilow,ciup))

pdf("../../plots/diffindisc_bandwidths_supportfromabove.pdf",width=6, height=5)
par(mar=c(5.1, 5.1, 2.1, 2.1))
plot(results$bw, results$did_estimate,
     ylim=c(-1,0.3),
     xlab="Bandwidth",
     ylab="Difference-in-discontinuities, \n appointed vs uappointed directors",pch=19)
     # main="Director reports being supported from above"
grid()
arrows(results$bw, results$cilow, results$bw, results$ciup, length=0.05, angle=90, code=3,col="darkgrey")
abline(h=0,col="red",lty=2)
abline(v=bw_calonico$bws[[1,1]],col="blue",lwd=2)
legend("bottomright",c("Optimal bandwidth"),lwd=2,col="blue",bty="n")
dev.off()

# SUPLEMENTARY ANALYSES (reported in Online Appendix) ---------------------
# Generate Table 6 (continuity of pre-treatment covariates) ----------------------------------

# Define list of covariates
covariate <- c("director_elected_2015","director_civil_service_2015","female","age_below40","age_forties","age_above50","race_white","race_black_or_brown","race_other","schooling_lessthantertiary", "schooling_tertiary", "schooling_postgraduate","exclusive_dedication","teacher_experience_lessthan6yr","teacher_experience_6to15yr", "teacher_experience_over15yr", "director_experience_lessthan3yr", "director_experience_3to10yr", "director_experience_over10yr", "director_here_lessthan3yr_2015", 
               "log_gdp_percapita", "log_population", "deaths_perthousand", "electoral_concentration" # ,"mayor_first_term", "party_large_programmatic"
               ,"rural", "number_workers", "in_settlement_land", "in_indigenous_land", "in_quilombola_land", "students_per_classroom", "school_inse",
               "ideb_2015", "ideb_target_2017")

# Create empty vectors to store quantities of interest
bandwidth <- c()
didestimate <- c()
didse <- c()
didpvalue <-
rdestimate <- c()
rdse <- c()
rdpvalue <- c()

# Examine continuity in each covariate
for(i in 1:length(covariate)){
  # Obtain optimal bandwidth
  calonico <- rdrobust(dd[,covariate[i]][[1]], x=dd$fv, covs = dd$treated_fv_appointed + dd$treated_appointed + dd$fv_appointed + dd$appointed, c= 0, cluster=dd$cod_ibge)
  bandwidth <- c(bandwidth, calonico$bws[[1,1]])
  # Data within the bandwidth and weights
  dd_bw <- subset(dd, abs(dd$fv)<calonico$bws[[1,1]])
  dd_bw$w <- as.numeric(NA)
  dd_bw[which(dd_bw$fv < 0),"w"] <- 1-abs(dd_bw[which(dd_bw$fv < 0),"fv"]/calonico$bws[[1,1]])
  dd_bw[which(dd_bw$fv >= 0),"w"] <- 1-abs(dd_bw[which(dd_bw$fv >= 0),"fv"]/calonico$bws[[1,1]])
  # Run diffindisc model
  rd <- felm(dd_bw[,covariate[i]][[1]] ~ treated*fv*appointed | 0 | 0 | cod_ibge, data=dd_bw,weights=dd_bw$w)
  # Store RD estimates
  rdestimate <- c(rdestimate, summary(rd)$coef["treated",1])
  rdpvalue <- c(rdpvalue, summary(rd)$coef["treated",4][[1]])
  rdse <- c(rdse, summary(rd)$coef["treated",2][[1]])
}

# Generate table with results
balance_table <- as.data.frame(cbind(covariate, rdestimate, rdse, rdpvalue, didestimate, didse, didpvalue))
balance_table$rdestimate <- as.numeric(as.character(balance_table$rdestimate))
balance_table$rdpvalue <- as.numeric(as.character(balance_table$rdpvalue))
balance_table$rdse <- as.numeric(as.character(balance_table$rdse))
balance_table <- balance_table[,c(2:4)]
rownames(balance_table) <- c("Director is elected","Director is civil service","Director is female", "Director is aged <40", "Director is aged 40-49", "Director is aged 50+", "Director is white", "Director is black/brown", "Director has other race", "Director has < tertiary education", "Director has a tertiary degree", "Director has a postgraduate degree", "Director has no other job", "Director has <6 years of teaching experience", "Director has 6-15 years of teaching experience", "Director has >15 years of teaching experience", "Director has <3 years of director experience", "Director has 3-10 years of director experience", "Director has >10 years of director experience", "Director has held position for <3 years", "Municipality GDP per capita (log)", "Municipality population (log)", "Municipality deaths per 1,000", "Municipal electoral concentration", "School is rural", "Number of staff in the school", "School is in a settlement", "School is in indigenous land", "School is in quilombola land", "Students per classroom (average)", "School socioeconomic index", "School IDEB score in 2015", "School IDEB target for 2017")
colnames(balance_table) <- c("RD estimate","Standard error","p value")

# Export results
print(file="../../tables/diffindisc_balance.tex",
      type="latex",
      xtable(balance_table, digits=3, align=c("lccc")), caption.placement="top", comment=F, floating=F, booktabs=T, column.names=T)

# Generate Figure 14 (continuity of the forcing variable)  ---------------------------------------------------------------

## Data at the municipality level, since that's where the turnover treatment takes place

pdf("../../plots/diffindisc_forcing_variable.pdf", width=12, height=5)
par(mfrow=c(1,2))
hist(m$challenger_margin, breaks=80,main="",xlab="Vote share of challenger - vote share of incumbent",col="gray")
abline(v=0,col="red")
legend("topright", c("RD threshold"),lty=1, col="red",bty="n")
DCdensity(m$challenger_margin,c=0)
title(main="", xlab = "Vote share of challenger - vote share of incumbent", ylab="Density",
      cex.lab=1,cex.main=1.2)
abline(v=0,col="red")
legend("topleft",c(paste0("p-value: ",round(DCdensity(m$challenger_margin,c=0,plot=F),3))),bty="n",cex=0.9)
dev.off()

# Generate Table 7 (diff-in-disc, donut approach) --------

# Exclude observations within 0.01 points of the discontinuity
dd_donut <- subset(dd, abs(dd$fv)>=0.01)
hist(dd_donut$fv, xlim=c(-0.2,0.2),breaks=1000,col="gray")

# Obtain the optimal bandwidth, subset data to observations within the bandwidth, and create the corresponding trinagular kernel weights for models 2-4 (with interaction treated*appointed)
bw_calonico <- rdbwselect(dd_donut$ideb_delta, x=dd_donut$fv, covs = dd_donut$treated_fv_appointed + dd_donut$treated_appointed + dd_donut$fv_appointed + dd_donut$appointed, c= 0, cluster=dd_donut$cod_ibge)
bw <- bw_calonico$bws[[1,1]]
dd_donut_int <- subset(dd_donut, abs(dd_donut$fv)<bw)
dd_donut_int$w <- as.numeric(NA)
dd_donut_int[which(dd_donut_int$fv < 0),"w"] <- 1-abs(dd_donut_int[which(dd_donut_int$fv < 0),"fv"]/bw)
dd_donut_int[which(dd_donut_int$fv >= 0),"w"] <- 1-abs(dd_donut_int[which(dd_donut_int$fv >= 0),"fv"]/bw)

# Obtain the optimal bandwidth, subset data to observations within the bandwidth, and create the corresponding trinagular kernel weights for model 1 (without interaction treated*appointed)
bw_calonico_noint <- rdbwselect(dd_donut$ideb_delta, x=dd_donut$fv, c= 0, cluster=dd_donut$cod_ibge)
bw_noint <- bw_calonico_noint$bws[[1,1]] # 0.21
dd_donut_noint <- subset(dd_donut, abs(dd_donut$fv)<bw)
dd_donut_noint$w <- as.numeric(NA)
dd_donut_noint[which(dd_donut_noint$fv < 0),"w"] <- 1-abs(dd_donut_noint[which(dd_donut_noint$fv < 0),"fv"]/bw)
dd_donut_noint[which(dd_donut_noint$fv >= 0),"w"] <- 1-abs(dd_donut_noint[which(dd_donut_noint$fv >= 0),"fv"]/bw)

r1 <- felm(ideb_delta ~ treated*fv | 0 | 0 | cod_ibge, data=dd_donut_noint,weights=dd_donut_noint$w)
r2 <- felm(ideb_delta ~ treated*fv*appointed | 0 | 0 | cod_ibge, data=dd_donut_int, weights=dd_donut_int$w)
r3 <- felm(ideb_delta ~ treated*fv*appointed | uf | 0 | cod_ibge,  data=dd_donut_int, weights=dd_donut_int$w)
r4 <- felm(ideb_delta ~ treated*fv*appointed 
           + female + age_forties + race_white + race_black_or_brown + schooling_postgraduate + exclusive_dedication + teacher_experience_6to15yr + director_experience_3to10yr + director_experience_over10yr
           + log_gdp_percapita + log_population + deaths_perthousand + electoral_concentration 
           + rural + students_per_classroom + school_inse
           | uf | 0 | cod_ibge,  data=dd_donut_int, weights=dd_donut_int$w)

# Export results
texreg(list(r1,r2,r3,r4),
       file="../../tables/diffindisc_donut.tex", table=F, float.pos="htp", scalebox=.75,
       digits=3,booktabs = T, dcolumn=T, use.packages=F,single.row = F,custom.note="",
       custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
       custom.coef.map = list("treated" = "$\\hat \\beta_1$: Political turnover", "treated:appointed" = "$\\hat \\gamma_2$: Political turnover $\\times$ Appointed"),
       custom.gof.names = c("N"),
       include.adjrs=FALSE, include.rsq=F, include.groups=F, include.n=T,
       stars = c(0.05, 0.01, 0.001),
       custom.gof.rows = list("State fixed effects" = c("","",rep("$\\checkmark$",2)),
                              "Predictors of Appointed" = c(rep("",3),"$\\checkmark$"),
                              "Bandwidth" = c(bw_calonico_noint$bws[[1]], rep(bw_calonico$bws[[1]],3))))

# Generate Figure 15 (parallel trends) ---------------------------------------------------------

# Get mean IDEB scores, before the 2016 election, by appointment mode
parallel_trends_appointment <- dd %>%
  group_by(director_appointed) %>%
  dplyr::summarise(count = n(),
                   mean_2015 = mean(ideb_2015, na.rm=T),
                   mean_2013 = mean(ideb_2013, na.rm=T),
                   mean_2011 = mean(ideb_2011, na.rm=T),
                   mean_2009 = mean(ideb_2009, na.rm=T))
parallel_trends_appointed <- rev(t(parallel_trends_appointment[2,c(3:6)]))
parallel_trends_notappointed <- rev(t(parallel_trends_appointment[1,c(3:6)]))

# Get mean IDEB scores, before the 2016 election, by mayoral turnover
parallel_trends_turnover <- dd %>%
  group_by(mayor_lost) %>%
  dplyr::summarise(count = n(),
                   mean_2015 = mean(ideb_2015, na.rm=T),
                   mean_2013 = mean(ideb_2013, na.rm=T),
                   mean_2011 = mean(ideb_2011, na.rm=T),
                   mean_2009 = mean(ideb_2009, na.rm=T))
parallel_trends_mayorlost <- rev(t(parallel_trends_turnover[2,c(3:6)]))
parallel_trends_mayorreelected <- rev(t(parallel_trends_turnover[1,c(3:6)]))

# Generate and export plot
pdf("../../plots/diffindisc_paralleltrends.pdf",width=9,height=4)
par(mfrow=c(1,2))
plot(parallel_trends_appointed,type="o",ylim=c(4,6),col="blue",ylab="IDEB score",lwd=2,xaxt="n",xlab="Years before the 2016 election")
lines(parallel_trends_notappointed,type="o",lwd=2)
legend("topleft", c("Appointed director", "Unappointed director"), lty=1, col=c("blue","black"), lwd=2,bty="n", pch=1)
axis(side=1,at=c(1:4),labels=c(seq(7,1,-2)))
# By turnover
plot(parallel_trends_mayorlost,type="o",ylim=c(4,6),col="blue",ylab="IDEB score",lwd=2,xaxt="n",xlab="Years before the 2016 election")
lines(parallel_trends_mayorreelected,type="o",lwd=2)
legend("topleft", c("Political turnover", "Political continuity"), lty=1, col=c("blue","black"), lwd=2,bty="n", pch=1)
axis(side=1,at=c(1:4),labels=c(seq(7,1,-2)))
dev.off()

# Generate Figure 16 (effect of political turnover on director turnover) --------

# Subset to schools with directors that were deployed in the pre-election term, but without conditioning on them staying in their post after the election
dx <- subset(s, s$director_here_lessthan3yr_2015==1)
dx$appointed <- dx$director_appointed_2015
dx$fv <- dx$challenger_margin
dx$treated <- dx$mayor_lost
dx$treated_fv_appointed <- dx$treated*dx$fv*dx$appointed
dx$treated_appointed <- dx$treated*dx$appointed
dx$fv_appointed <- dx$fv*dx$appointed

# Subset data to appointed and unappointed 
dx_appointed <- subset(dx, dx$appointed==1 & !is.na(dx$fv))
dx_notappointed <- subset(dx, dx$appointed==0 & !is.na(dx$fv))

pdf("../../plots/rdd_polturnover_dirturnover.pdf",width=12,height=5)
par(mfrow=c(1,2))
myrdplot(x = dx_appointed$fv, 
         y = dx_appointed$director_here_lessthan1yr_2017, cutpoint = 0,
         xrange = c(-0.5,0.5), yrange=c(0,1),bins=18,
         xtitle = "Challenger vote share - incumbent vote share",
         ytitle = "Director turnover after the election", 
         legendabove="Political turnover",
         legendbelow="No political turnover",
         maintitle="Appointed directors",titlesize=1.1)
grid()
myrdplot(x = dx_notappointed$fv, 
         y = dx_notappointed$director_here_lessthan1yr_2017, cutpoint = 0,
         xrange = c(-0.5,0.5), yrange=c(0,1),bins=12,
         xtitle = "Challenger vote share - incumbent vote share",
         ytitle = "Director turnover after the election",  
         legendabove="Political turnover",
         legendbelow="No political turnover",
         maintitle="Unappointed directors",titlesize=1.1)
grid()
dev.off()

# Generate Table 8 (effect of political turnover on director turnover) --------

# Bandwidth and weights for models with heterogeneity
bw_calonico <- rdbwselect(dx$director_here_lessthan1yr_2017, x=dx$fv, covs = dx$treated_fv_appointed + dx$treated_appointed + dx$fv_appointed + dx$appointed, c= 0, cluster=dx$cod_ibge)
bw <- bw_calonico$bws[[1,1]]
dx_int <- subset(dx, abs(dx$fv)<bw)
dx_int$w <- as.numeric(NA)
dx_int[which(dx_int$fv < 0),"w"] <- 1-abs(dx_int[which(dx_int$fv < 0),"fv"]/bw)
dx_int[which(dx_int$fv >= 0),"w"] <- 1-abs(dx_int[which(dx_int$fv >= 0),"fv"]/bw)

# Bandwidth and weights for models without heterogeneity
bw_calonico_noint <- rdbwselect(dx$director_here_lessthan1yr_2017, x=dx$fv, c= 0, cluster=dx$cod_ibge)
bw_noint <- bw_calonico_noint$bws[[1,1]] # 0.21
dx_noint <- subset(dx, abs(dx$fv)<bw)
dx_noint$w <- as.numeric(NA)
dx_noint[which(dx_noint$fv < 0),"w"] <- 1-abs(dx_noint[which(dx_noint$fv < 0),"fv"]/bw)
dx_noint[which(dx_noint$fv >= 0),"w"] <- 1-abs(dx_noint[which(dx_noint$fv >= 0),"fv"]/bw)

r1 <- felm(director_here_lessthan1yr_2017 ~ treated*fv | 0 | 0 | cod_ibge, data=dx_noint, weights=dx_noint$w)
r2 <- felm(director_here_lessthan1yr_2017 ~ treated*fv*appointed | 0 | 0 | cod_ibge, data=dx_int, weights=dx_int$w)
lh2 <- summary(glht(r2, linfct = c("treated + treated:appointed==0"),vcov=vcovCL(r2, dx_int$cod_ibge)))
r3 <- felm(director_here_lessthan1yr_2017 ~ treated*fv*appointed | uf | 0 | cod_ibge, data=dx_int, weights=dx_int$w)
lh3 <- summary(glht(r3, linfct = c("treated + treated:appointed==0"),vcov=vcovCL(r3, dx_int$cod_ibge)))
r4 <- felm(director_here_lessthan1yr_2017 ~ treated*fv*appointed 
           + female + age_forties + race_white + race_black_or_brown + schooling_postgraduate + exclusive_dedication + teacher_experience_6to15yr + director_experience_3to10yr + director_experience_over10yr
           + log_gdp_percapita + log_population + deaths_perthousand + electoral_concentration 
           + rural + students_per_classroom + school_inse
           | uf | 0 | cod_ibge, data=dx_int, weights=dx_int$w)
lh4 <- summary(glht(r4, linfct = c("treated + treated:appointed==0"),vcov=vcovCL(r4, dx_int$cod_ibge)))

# Export results
texreg(list(r1,r2,r3,r4),
       file="../../tables/rdd_polturnover_dirturnover.tex", table=F, float.pos="htp", scalebox=.75,
       digits=3, booktabs = T, dcolumn=T,use.packages=F,single.row = F,custom.note="",
       custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
       custom.coef.map = list("treated" = "$\\hat \\beta_1$: Political turnover", "treated:appointed" = "$\\hat \\gamma_2$: Political turnover $\\times$ Appointed"),
       custom.gof.names = c("N"),
       include.adjrs=FALSE, include.rsq=F, include.n=T,include.groups=F,
       stars = c(0.05, 0.01, 0.001),
       custom.gof.rows = list("$\\hat \\beta_1 + \\hat \\gamma_2$" = c("",paste0(round(lh2$test$coef[[1]],3),ifelse(lh2$test$pvalues[[1]]<0.001,"$^{***}$",ifelse(lh2$test$pvalues[[1]]<0.01,"$^{**}$",ifelse(lh2$test$pvalues[[1]]<0.05,"$^{*}$",""))),sep=""),
                                                                       paste0(round(lh3$test$coef[[1]],3),ifelse(lh3$test$pvalues[[1]]<0.001,"$^{***}$",ifelse(lh3$test$pvalues[[1]]<0.01,"$^{**}$",ifelse(lh3$test$pvalues[[1]]<0.05,"$^{*}$",""))),sep=""),
                                                                       paste0(round(lh4$test$coef[[1]],3),ifelse(lh4$test$pvalues[[1]]<0.001,"$^{***}$",ifelse(lh4$test$pvalues[[1]]<0.01,"$^{**}$",ifelse(lh4$test$pvalues[[1]]<0.05,"$^{*}$",""))),sep="")),
                              " " = c("", paste0("(",round(lh2$test$sigma[[1]],3),")",sep=""),paste0("(",round(lh3$test$sigma[[1]],3),")",sep=""),paste0("(",round(lh4$test$sigma[[1]],3),")",sep="")),
                              "State fixed effects" = c("","",rep("$\\checkmark$",2)),
                              "Predictors of Appointed" = c(rep("",3),"$\\checkmark$"),
                              "Bandwidth" = c(bw_calonico_noint$bws[[1]], rep(bw_calonico$bws[[1]],3))))

# Generate Table 9 (predictors of director turnover after political turnover) ----------------

# Subset to schools in municipalities where the mayor loses the 2016 election
dt <- subset(s, s$mayor_lost==1)

# Regress director turnover in 2017 on predictors
m1 <- felm(director_here_lessthan1yr_2017 ~ 
             director_appointed_2015 + ideb_2015_mec + female + age_forties + age_above50 + race_white + race_black_or_brown + schooling_tertiary + schooling_postgraduate + exclusive_dedication + teacher_experience_6to15yr + teacher_experience_over15yr + director_experience_3to10yr + director_experience_over10yr + director_here_lessthan3yr_2015
           , data=dt)
coeftest(m1,vcov=vcovHC(m1,type="HC1"))

# Export results
texreg(list(m1),
       file="../../tables/predictors_director_turnover_after_polturnover.tex",table=F,
       digits=3,
       single.row=T,
       override.se = list(coeftest(m1,vcov=vcovHC(m1,type="HC1"))[,2]),
       override.pvalues = list(coeftest(m1,vcov=vcovHC(m1,type="HC1"))[,4]),
       custom.model.names = c("(1)"),
       custom.coef.names = c("Intercept",
                             "Director was appointed",
                             "IDEB score before the election",
                             "Female", 
                             "Age 40-49 (vs <40)",
                             "Age 50+ (vs <40)",
                             "White (vs other race)",
                             "Black/brown (vs other race)",
                             "Tertiary degree (vs < tertiary)",
                             "Postgraduate degree (vs < tertiary)",
                             "No other jobs",
                             "6-15 years of teaching exp. (vs <6)",
                             ">15 years of teaching exp. (vs <6)",
                             "3-10 years of director exp. (vs <3)",
                             ">10 years of director exp. (vs <3)",
                             "1-2 years as director of school (vs >2)"),
       custom.note = c("%stars. HC1 standard errors in brackets"),
       include.rsquared = F, include.adjrs = T, include.nobs = T, include.groups=F,
       booktabs=TRUE,use.packages=FALSE,
       stars = c(0.01, 0.05, 0.1))

# Generate Table 10 (diff-in-disc after matching) --------

# Create a copy of the dataset for the matching analyses
dm <- dd

# Significant predictors of director turnover after mayor turnover (from analysis in Table A-9)
## paste(names(coefficients(m1))[which(coeftest(m1, type="HC1")[,4]<0.10)],collapse=" , ")
predictors <- c("director_appointed_2015" , "ideb_2015_mec" , "age_forties" , "age_above50" , "race_white" , "schooling_tertiary" , "schooling_postgraduate" , "exclusive_dedication" , "teacher_experience_over15yr" , "director_experience_over10yr")

# Remove cases with missing data
dm <- dm[which(complete.cases(dm$mayor_lost) & complete.cases(dm[,predictors]) & complete.cases(dm$ideb_delta)),]

# Create a matched sample, using exact matching on predictors, with replacement
matching1 <- Match(Tr = dm$mayor_lost, X = dm[,predictors], Y = dm$ideb_delta, exact=T, ties=T, replace=T)
dm <- dm[c(matching1$index.treated, matching1$index.control),]

# Bandwidth and weights for models with heterogeneity
bw_calonico <- rdbwselect(dm$ideb_delta, x=dm$fv, covs = dm$treated_fv_appointed + dm$treated_appointed + dm$fv_appointed + dm$appointed, c= 0, cluster=dm$cod_ibge)
bw <- bw_calonico$bws[[1,1]]
dm_int <- subset(dm, abs(dm$fv)<bw)
dm_int$w <- as.numeric(NA)
dm_int[which(dm_int$fv < 0),"w"] <- 1-abs(dm_int[which(dm_int$fv < 0),"fv"]/bw)
dm_int[which(dm_int$fv >= 0),"w"] <- 1-abs(dm_int[which(dm_int$fv >= 0),"fv"]/bw)

# Bandwidth and weights for models without heterogeneity
bw_calonico_noint <- rdbwselect(dm$ideb_delta, x=dm$fv, c= 0, cluster=dm$cod_ibge)
bw_noint <- bw_calonico_noint$bws[[1,1]] # 0.21
dm_noint <- subset(dm, abs(dm$fv)<bw)
dm_noint$w <- as.numeric(NA)
dm_noint[which(dm_noint$fv < 0),"w"] <- 1-abs(dm_noint[which(dm_noint$fv < 0),"fv"]/bw)
dm_noint[which(dm_noint$fv >= 0),"w"] <- 1-abs(dm_noint[which(dm_noint$fv >= 0),"fv"]/bw)

r1 <- felm(ideb_delta ~ treated*fv | 0 | 0 | cod_ibge, data=dm_noint, weights=dm_noint$w)
r2 <- felm(ideb_delta ~ treated*fv*appointed | 0 | 0 | cod_ibge, data=dm_noint, weights=dm_noint$w)
r3 <- felm(ideb_delta ~ treated*fv*appointed | uf | 0 | cod_ibge, data=dm_noint, weights=dm_noint$w)
r4 <- felm(ideb_delta ~ treated*fv*appointed 
           + female + age_forties + race_white + race_black_or_brown + schooling_postgraduate + exclusive_dedication + teacher_experience_6to15yr + director_experience_3to10yr + director_experience_over10yr
           + log_gdp_percapita + log_population + deaths_perthousand + electoral_concentration 
           + rural + students_per_classroom + school_inse
           | uf | 0 | cod_ibge, data=dm_noint, weights=dm_noint$w)

texreg(list(r1,r2,r3,r4),
       file="../../tables/diffindisc_matching.tex", table=F, float.pos="htp", scalebox=.75,
       digits=3,booktabs = T, dcolumn = T,use.packages=F,single.row = F,custom.note="",
       custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
       custom.coef.map = list("treated" = "$\\hat \\beta_1$: Political turnover", "treated:appointed" = "$\\hat \\gamma_2$: Political turnover $\\times$ Appointed"),
       custom.gof.names = c("N"),
       include.adjrs=FALSE, include.rsq=F, include.groups=F, include.n=T,
       stars = c(0.05, 0.01, 0.001),
       custom.gof.rows = list("State fixed effects" = c("","",rep("$\\checkmark$",2)),
                              "Predictors of Appointed" = c(rep("",3),"$\\checkmark$"),
                              "Bandwidth" = c(bw_calonico_noint$bws[[1]], rep(bw_calonico$bws[[1]],3))))


# Generate Lee bounds (Appendix C.6) --------------------------------------


# Generate Figure 17 (RD plots for support from above) --------

pdf("../../plots/diffindisc_supportfromabove.pdf",width=12,height=5)
par(mfrow=c(1,2))
myrdplot(x = dd_appointed$fv, 
         y = dd_appointed$support_from_above_delta, cutpoint = 0,
         xrange = c(-0.5,0.5), yrange=c(-0.25,0.25),bins=18,
         xtitle = "Challenger vote share - incumbent vote share (2016)",
         ytitle = "Change in support from above (2015-17)", 
         legendabove="Political turnover",
         legendbelow="No political turnover",
         maintitle="Appointed directors",titlesize=1.1)
grid()
myrdplot(x = dd_notappointed$fv, 
         y = dd_notappointed$support_from_above_delta, cutpoint = 0,
         xrange = c(-0.5,0.5), yrange=c(-0.25,0.25),bins=18,
         xtitle = "Challenger vote share - incumbent vote share (2016)",
         ytitle = "Change in support from above (2015-17)",  
         legendabove="Political turnover",
         legendbelow="No political turnover",
         maintitle="Unappointed directors",titlesize=1.1)
grid()
dev.off()

# Generate Table 11 (diff-in-disc: problems of teacher turnover) --------

# Obtain the optimal bandwidth, subset data to observations within the bandwidth, and create the corresponding trinagular kernel weights for models 2-4 (with interaction treated*appointed)
bw_calonico <- rdbwselect(dd$teacher_turnover_delta, x=dd$fv, covs = dd$treated_fv_appointed + dd$treated_appointed + dd$fv_appointed + dd$appointed, c= 0, cluster=dd$cod_ibge)
bw <- bw_calonico$bws[[1,1]]
dd_int <- subset(dd, abs(dd$fv)<bw)
dd_int$w <- as.numeric(NA)
dd_int[which(dd_int$fv < 0),"w"] <- 1-abs(dd_int[which(dd_int$fv < 0),"fv"]/bw)
dd_int[which(dd_int$fv >= 0),"w"] <- 1-abs(dd_int[which(dd_int$fv >= 0),"fv"]/bw)

# Obtain the optimal bandwidth, subset data to observations within the bandwidth, and create the corresponding trinagular kernel weights for model 1 (without interaction treated*appointed)
bw_calonico_noint <- rdbwselect(dd$teacher_turnover_delta, x=dd$fv, c= 0, cluster=dd$cod_ibge)
bw_noint <- bw_calonico_noint$bws[[1,1]] # 0.21
dd_noint <- subset(dd, abs(dd$fv)<bw)
dd_noint$w <- as.numeric(NA)
dd_noint[which(dd_noint$fv < 0),"w"] <- 1-abs(dd_noint[which(dd_noint$fv < 0),"fv"]/bw)
dd_noint[which(dd_noint$fv >= 0),"w"] <- 1-abs(dd_noint[which(dd_noint$fv >= 0),"fv"]/bw)

r1 <- felm(teacher_turnover_delta ~ treated*fv | 0 | 0 | cod_ibge, data=dd_noint, weights=dd_noint$w)
r2 <- felm(teacher_turnover_delta ~ treated*fv*appointed | 0 | 0 | cod_ibge, data=dd_int, weights=dd_int$w)
r3 <- felm(teacher_turnover_delta ~ treated*fv*appointed | uf | 0 | cod_ibge, data=dd_int, weights=dd_int$w)
r4 <- felm(teacher_turnover_delta ~ treated*fv*appointed 
           + female + age_forties + race_white + race_black_or_brown + schooling_postgraduate + exclusive_dedication + teacher_experience_6to15yr + director_experience_3to10yr + director_experience_over10yr
           + log_gdp_percapita + log_population + deaths_perthousand + electoral_concentration 
           + rural + students_per_classroom + school_inse
           | uf | 0 | cod_ibge, data=dd_int, weights=dd_int$w)

texreg(list(r1,r2,r3,r4),
       file="../../tables/diffindisc_teacherturnover.tex", table=F, float.pos="htp", scalebox=.75,
       digits=3,booktabs = T, dcolumn = T,use.packages=F,single.row = F,custom.note="",
       custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
       custom.coef.map = list("treated" = "$\\hat \\beta_1$: Political turnover", "treated:appointed" = "$\\hat \\gamma_2$: Political turnover $\\times$ Appointed"),
       custom.gof.names = c("N"),
       include.adjrs=FALSE, include.rsq=F, include.groups=F, include.n=T,
       stars = c(0.05, 0.01, 0.001),
       custom.gof.rows = list("State fixed effects" = c("","",rep("$\\checkmark$",2)),
                              "Predictors of Appointed" = c(rep("",3),"$\\checkmark$"),
                              "Bandwidth" = c(bw_calonico_noint$bws[[1]], rep(bw_calonico$bws[[1]],3))))

# Generate Table 12 (diff-in-disc: problems of insufficient teachers) --------

# Obtain the optimal bandwidth, subset data to observations within the bandwidth, and create the corresponding trinagular kernel weights for models 2-4 (with interaction treated*appointed)
bw_calonico <- rdbwselect(dd$insufficient_teachers_delta, x=dd$fv, covs = dd$treated_fv_appointed + dd$treated_appointed + dd$fv_appointed + dd$appointed, c= 0, cluster=dd$cod_ibge)
bw <- bw_calonico$bws[[1,1]]
dd_int <- subset(dd, abs(dd$fv)<bw)
dd_int$w <- as.numeric(NA)
dd_int[which(dd_int$fv < 0),"w"] <- 1-abs(dd_int[which(dd_int$fv < 0),"fv"]/bw)
dd_int[which(dd_int$fv >= 0),"w"] <- 1-abs(dd_int[which(dd_int$fv >= 0),"fv"]/bw)

# Obtain the optimal bandwidth, subset data to observations within the bandwidth, and create the corresponding trinagular kernel weights for model 1 (without interaction treated*appointed)
bw_calonico_noint <- rdbwselect(dd$insufficient_teachers_delta, x=dd$fv, c= 0, cluster=dd$cod_ibge)
bw_noint <- bw_calonico_noint$bws[[1,1]] # 0.21
dd_noint <- subset(dd, abs(dd$fv)<bw)
dd_noint$w <- as.numeric(NA)
dd_noint[which(dd_noint$fv < 0),"w"] <- 1-abs(dd_noint[which(dd_noint$fv < 0),"fv"]/bw)
dd_noint[which(dd_noint$fv >= 0),"w"] <- 1-abs(dd_noint[which(dd_noint$fv >= 0),"fv"]/bw)

r1 <- felm(insufficient_teachers_delta ~ treated*fv | 0 | 0 | cod_ibge, data=dd_noint, weights=dd_noint$w)
r2 <- felm(insufficient_teachers_delta ~ treated*fv*appointed | 0 | 0 | cod_ibge, data=dd_int, weights=dd_int$w)
r3 <- felm(insufficient_teachers_delta ~ treated*fv*appointed | uf | 0 | cod_ibge, data=dd_int, weights=dd_int$w)
r4 <- felm(insufficient_teachers_delta ~ treated*fv*appointed 
           + female + age_forties + race_white + race_black_or_brown + schooling_postgraduate + exclusive_dedication + teacher_experience_6to15yr + director_experience_3to10yr + director_experience_over10yr
           + log_gdp_percapita + log_population + deaths_perthousand + electoral_concentration 
           + rural + students_per_classroom + school_inse
           | uf | 0 | cod_ibge, data=dd_int, weights=dd_int$w)

texreg(list(r1,r2,r3,r4),
       file="../../tables/diffindisc_insufficientteachers.tex", table=F, float.pos="htp", scalebox=.75,
       digits=3,booktabs = T, dcolumn = T,use.packages=F,single.row = F,custom.note="",
       custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
       custom.coef.map = list("treated" = "$\\hat \\beta_1$: Political turnover", "treated:appointed" = "$\\hat \\gamma_2$: Political turnover $\\times$ Appointed"),
       custom.gof.names = c("N"),
       include.adjrs=FALSE, include.rsq=F, include.groups=F, include.n=T,
       stars = c(0.05, 0.01, 0.001),
       custom.gof.rows = list("State fixed effects" = c("","",rep("$\\checkmark$",2)),
                              "Predictors of Appointed" = c(rep("",3),"$\\checkmark$"),
                              "Bandwidth" = c(bw_calonico_noint$bws[[1]], rep(bw_calonico$bws[[1]],3))))

# Generate Table 13 (diff-in-disc: insufficient financial resources) --------

# Obtain the optimal bandwidth, subset data to observations within the bandwidth, and create the corresponding trinagular kernel weights for models 2-4 (with interaction treated*appointed)
bw_calonico <- rdbwselect(dd$insufficient_financial_resources_delta, x=dd$fv, covs = dd$treated_fv_appointed + dd$treated_appointed + dd$fv_appointed + dd$appointed, c= 0, cluster=dd$cod_ibge)
bw <- bw_calonico$bws[[1,1]]
dd_int <- subset(dd, abs(dd$fv)<bw)
dd_int$w <- as.numeric(NA)
dd_int[which(dd_int$fv < 0),"w"] <- 1-abs(dd_int[which(dd_int$fv < 0),"fv"]/bw)
dd_int[which(dd_int$fv >= 0),"w"] <- 1-abs(dd_int[which(dd_int$fv >= 0),"fv"]/bw)

# Obtain the optimal bandwidth, subset data to observations within the bandwidth, and create the corresponding trinagular kernel weights for model 1 (without interaction treated*appointed)
bw_calonico_noint <- rdbwselect(dd$insufficient_financial_resources_delta, x=dd$fv, c= 0, cluster=dd$cod_ibge)
bw_noint <- bw_calonico_noint$bws[[1,1]] # 0.21
dd_noint <- subset(dd, abs(dd$fv)<bw)
dd_noint$w <- as.numeric(NA)
dd_noint[which(dd_noint$fv < 0),"w"] <- 1-abs(dd_noint[which(dd_noint$fv < 0),"fv"]/bw)
dd_noint[which(dd_noint$fv >= 0),"w"] <- 1-abs(dd_noint[which(dd_noint$fv >= 0),"fv"]/bw)

r1 <- felm(insufficient_financial_resources_delta ~ treated*fv | 0 | 0 | cod_ibge, data=dd_noint, weights=dd_noint$w)
r2 <- felm(insufficient_financial_resources_delta ~ treated*fv*appointed | 0 | 0 | cod_ibge, data=dd_int, weights=dd_int$w)
r3 <- felm(insufficient_financial_resources_delta ~ treated*fv*appointed | uf | 0 | cod_ibge, data=dd_int, weights=dd_int$w)
r4 <- felm(insufficient_financial_resources_delta ~ treated*fv*appointed 
           + female + age_forties + race_white + race_black_or_brown + schooling_postgraduate + exclusive_dedication + teacher_experience_6to15yr + director_experience_3to10yr + director_experience_over10yr
           + log_gdp_percapita + log_population + deaths_perthousand + electoral_concentration 
           + rural + students_per_classroom + school_inse
           | uf | 0 | cod_ibge, data=dd_int, weights=dd_int$w)

texreg(list(r1,r2,r3,r4),
       file="../../tables/diffindisc_insufficientfinancialresources.tex", table=F, float.pos="htp", scalebox=.75,
       digits=3,booktabs = T, dcolumn=T,use.packages=F,single.row = F,custom.note="",
       custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
       custom.coef.map = list("treated" = "$\\hat \\beta_1$: Political turnover", "treated:appointed" = "$\\hat \\gamma_2$: Political turnover $\\times$ Appointed"),
       custom.gof.names = c("N"),
       include.adjrs=FALSE, include.rsq=F, include.groups=F, include.n=T,
       stars = c(0.05, 0.01, 0.001),
       custom.gof.rows = list("State fixed effects" = c("","",rep("$\\checkmark$",2)),
                              "Predictors of Appointed" = c(rep("",3),"$\\checkmark$"),
                              "Bandwidth" = c(bw_calonico_noint$bws[[1]], rep(bw_calonico$bws[[1]],3))))


# Generate Table 14 (diff-in-disc: school quality - interacting covariates) --------

# Obtain the optimal bandwidth, subset data to observations within the bandwidth, and create the corresponding trinagular kernel weights for models 2-4 (with interaction treated*appointed)
bw_calonico <- rdbwselect(dd$ideb_delta, x=dd$fv, covs = dd$treated_fv_appointed + dd$treated_appointed + dd$fv_appointed + dd$appointed, c= 0, cluster=dd$cod_ibge)
bw <- bw_calonico$bws[[1,1]]
dd_int <- subset(dd, abs(dd$fv)<bw)
dd_int$w <- as.numeric(NA)
dd_int[which(dd_int$fv < 0),"w"] <- 1-abs(dd_int[which(dd_int$fv < 0),"fv"]/bw)
dd_int[which(dd_int$fv >= 0),"w"] <- 1-abs(dd_int[which(dd_int$fv >= 0),"fv"]/bw)

# Obtain the optimal bandwidth, subset data to observations within the bandwidth, and create the corresponding trinagular kernel weights for model 1 (without interaction treated*appointed)
bw_calonico_noint <- rdbwselect(dd$ideb_delta, x=dd$fv, c= 0, cluster=dd$cod_ibge)
bw_noint <- bw_calonico_noint$bws[[1,1]] # 0.21
dd_noint <- subset(dd, abs(dd$fv)<bw)
dd_noint$w <- as.numeric(NA)
dd_noint[which(dd_noint$fv < 0),"w"] <- 1-abs(dd_noint[which(dd_noint$fv < 0),"fv"]/bw)
dd_noint[which(dd_noint$fv >= 0),"w"] <- 1-abs(dd_noint[which(dd_noint$fv >= 0),"fv"]/bw)

r1 <- felm(ideb_delta ~ treated*fv | 0 | 0 | cod_ibge, data=dd_noint,weights=dd_noint$w)
r2 <- felm(ideb_delta ~ treated*fv*appointed | 0 | 0 | cod_ibge, data=dd_int, weights=dd_int$w)
r3 <- felm(ideb_delta ~ treated*fv*appointed | uf | 0 | cod_ibge,  data=dd_int, weights=dd_int$w)
r4 <- felm(ideb_delta ~ treated*fv*appointed 
           + treated*(female + age_forties + race_white + race_black_or_brown + schooling_postgraduate + exclusive_dedication + teacher_experience_6to15yr + director_experience_3to10yr + director_experience_over10yr
           + log_gdp_percapita + log_population + deaths_perthousand + electoral_concentration 
           + rural + students_per_classroom + school_inse) | uf | 0 | cod_ibge, data=dd_int, weights=dd_int$w)

# Export results
texreg(list(r1,r2,r3,r4),
       file="../../tables/diffindisc_covariates_interacted.tex", table=F, float.pos="htp", scalebox=.75,
       digits=3,booktabs = T, dcolumn=T, use.packages=F,single.row = F,custom.note="",
       custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
       custom.coef.map = list("treated" = "$\\hat \\beta_1$: Political turnover", "treated:appointed" = "$\\hat \\gamma_2$: Political turnover $\\times$ Appointed"),
       custom.gof.names = c("N"),
       include.adjrs=FALSE, include.rsq=F, include.groups=F, include.n=T,
       stars = c(0.05, 0.01, 0.001),
       custom.gof.rows = list("State fixed effects" = c("","",rep("$\\checkmark$",2)),
                              "Predictors of Appointed" = c(rep("",3),"$\\checkmark$"),
                              "Bandwidth" = c(bw_calonico_noint$bws[[1]], rep(bw_calonico$bws[[1]],3))))

# Generate Table 15 (diff-in-disc, support from above - interacting covariates) --------

# Obtain the optimal bandwidth, subset data to observations within the bandwidth, and create the corresponding trinagular kernel weights for models 2-4 (with interaction treated*appointed)
bw_calonico <- rdbwselect(dd$support_from_above_delta, x=dd$fv, covs = dd$treated_fv_appointed + dd$treated_appointed + dd$fv_appointed + dd$appointed, c= 0, cluster=dd$cod_ibge)
bw <- bw_calonico$bws[[1,1]]
dd_int <- subset(dd, abs(dd$fv)<bw)
dd_int$w <- as.numeric(NA)
dd_int[which(dd_int$fv < 0),"w"] <- 1-abs(dd_int[which(dd_int$fv < 0),"fv"]/bw)
dd_int[which(dd_int$fv >= 0),"w"] <- 1-abs(dd_int[which(dd_int$fv >= 0),"fv"]/bw)

# Obtain the optimal bandwidth, subset data to observations within the bandwidth, and create the corresponding trinagular kernel weights for model 1 (without interaction treated*appointed)
bw_calonico_noint <- rdbwselect(dd$support_from_above_delta, x=dd$fv, c= 0, cluster=dd$cod_ibge)
bw_noint <- bw_calonico_noint$bws[[1,1]] # 0.21
dd_noint <- subset(dd, abs(dd$fv)<bw)
dd_noint$w <- as.numeric(NA)
dd_noint[which(dd_noint$fv < 0),"w"] <- 1-abs(dd_noint[which(dd_noint$fv < 0),"fv"]/bw)
dd_noint[which(dd_noint$fv >= 0),"w"] <- 1-abs(dd_noint[which(dd_noint$fv >= 0),"fv"]/bw)

r1 <- felm(support_from_above_delta ~ treated*fv | 0 | 0 | cod_ibge, data=dd_noint, weights=dd_noint$w)
r2 <- felm(support_from_above_delta ~ treated*fv*appointed | 0 | 0 | cod_ibge, data=dd_int, weights=dd_int$w)
r3 <- felm(support_from_above_delta ~ treated*fv*appointed | uf | 0 | cod_ibge, data=dd_int, weights=dd_int$w)
r4 <- felm(support_from_above_delta ~ treated*fv*appointed 
           + treated*(female + age_forties + race_white + race_black_or_brown + schooling_postgraduate + exclusive_dedication + teacher_experience_6to15yr + director_experience_3to10yr + director_experience_over10yr
           + log_gdp_percapita + log_population + deaths_perthousand + electoral_concentration 
           + rural + students_per_classroom + school_inse) | uf | 0 | cod_ibge, data=dd_int, weights=dd_int$w)

# Export results
texreg(list(r1,r2,r3,r4),
       file="../../tables/diffindisc_supportfromabove_covariates_interacted.tex", table=F, float.pos="htp", scalebox=.75,
       digits=3,booktabs = T, dcolumn=T,use.packages=F,single.row = F,custom.note="",
       custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
       custom.coef.map = list("treated" = "$\\hat \\beta_1$: Political turnover", "treated:appointed" = "$\\hat \\gamma_2$: Political turnover $\\times$ Appointed"),
       custom.gof.names = c("N"),
       include.adjrs=FALSE, include.rsq=F, include.groups=F, include.n=T,
       stars = c(0.05, 0.01, 0.001),
       custom.gof.rows = list("State fixed effects" = c("","",rep("$\\checkmark$",2)),
                              "Predictors of Appointed" = c(rep("",3),"$\\checkmark$"),
                              "Bandwidth" = c(bw_calonico_noint$bws[[1]], rep(bw_calonico$bws[[1]],3))))

# Extract statistics mentioned in the text: Directors' tenure after the election --------

## "Most directors who are not replaced in the first year of a new administration continue in their post three years after the election"

# As in the diff-in-disc, I examine directors who are deployed before the election but continue in their post in late 2017
dd_turnover <- subset(dd, dd$mayor_lost==1)
dd_noturnover <- subset(dd, dd$mayor_lost==0)
dd_turnover_appointed <- subset(dd, dd$mayor_lost==1 & dd$director_appointed==1)
dd_noturnover_appointed <- subset(dd, dd$mayor_lost==0 & dd$director_appointed==1)
dd_turnover_notappointed <- subset(dd, dd$mayor_lost==1 & dd$director_appointed==1)
dd_noturnover_notappointed <- subset(dd, dd$mayor_lost==0 & dd$director_appointed==0)

# For this analysis I use director responses to the question on how long they have been in their post, in the 2019 ANRESC survey

mean(dd$director_here_years_2019<=2,na.rm=T) # About 33% of directors report in the 2019 ANRESC survey having been deployed since 2017
mean(dd_turnover$director_here_years_2019<=2,na.rm=T) # The share is only slightly higher in places with mayoral turnover (37%)
mean(dd_noturnover$director_here_years_2019<=2,na.rm=T) # ... compared to 30% in places without mayoral turnover

mean(dd_turnover_appointed$director_here_years_2019<=2,na.rm=T) # By appointment mode differences are also small: 35% for appointed directors, under turnover...
mean(dd_noturnover_appointed$director_here_years_2019<=2,na.rm=T) # ... vs 28% for appointed directors without turnover

mean(dd_turnover_notappointed$director_here_years_2019<=2,na.rm=T) # And the gap is similar among unappointed directors: 36% under turnover
mean(dd_noturnover_notappointed$director_here_years_2019<=2,na.rm=T) # vs 33% without turnover

## Generate bounds to account for sample selection bias (Appendix C.6) --------

### This section generates bounds for the diff-in-disc estimate, as explained in Appendix C.6

# Set the seed and define the localized RD bandwidth ----------------------

# Set the seed
set.seed(123)

# Define the bandwidth for the localized RDD
local_bandwidth <- 0.015

# Analyze localized difference-in-differences -----------------------------

# Define subset of municipalities within the local bandwidth
m_local <- subset(m,abs(m$challenger_margin)<local_bandwidth)

# Density of the forcing variable is about flat around the discontinuity (as stated in Appendix C.6.2)
## Histogram
hist(m_local$challenger_margin, breaks=40,main="",xlab="Vote share of challenger - vote share of incumbent",col="gray")
abline(v=0,col="red")
legend("topright", c("RD threshold"),lty=1, col="red",bty="n")
## McCrary density test
DCdensity(m_local$challenger_margin,c=0)
title(main="", xlab = "Vote share of challenger - vote share of incumbent", ylab="Density",
      cex.lab=1,cex.main=1.2)
abline(v=0,col="red")
legend("topleft",c(paste0("p-value: ",round(DCdensity(m_local$challenger_margin,c=0,plot=F),3))),bty="n",cex=0.9)

# Subset of the school-level dataset, keeping only schools where directors were deployed under the previous administration but who are still in their post after the election
dd <- subset(s, abs(s$challenger_margin)<local_bandwidth & !is.na(s$ideb_delta) & !is.na(s$director_appointed) & (s$director_here_lessthan3yr_2015==1) & (s$director_here_1to2yr_2017==1 | s$director_here_3to5yr_2017==1))
dd$appointed <- dd$director_appointed
dd$treated <- dd$mayor_lost

# Diff-in-diff models within the local bandwidth return similar results (as stated in Appendix C.6.2)
r1 <- felm(ideb_delta ~ treated | 0 | 0 | cod_ibge, data=dd)
summary(r1)
r2 <- felm(ideb_delta ~ treated*appointed | 0 | 0 | cod_ibge, data=dd)
summary(r2)
r3 <- felm(ideb_delta ~ treated*appointed | uf | 0 | cod_ibge, data=dd)
summary(r3)
r4 <- felm(ideb_delta ~ treated*appointed 
           + female + age_forties + race_white + race_black_or_brown + schooling_postgraduate + exclusive_dedication + teacher_experience_6to15yr + director_experience_3to10yr + director_experience_over10yr
           + log_gdp_percapita + log_population + deaths_perthousand + electoral_concentration 
           + rural + students_per_classroom + school_inse
           | uf | 0 | cod_ibge, data=dd)
summary(r4)

# Generate bounds --------------------------------------------------------------

### Appointed directors

# Generate dataset of appointed directors
db <- subset(s, abs(s$challenger_margin)<local_bandwidth & !is.na(s$director_appointed) & !is.na(s$ideb_delta) & (s$director_here_lessthan3yr_2015==1) & (s$director_here_lessthan1yr_2017==1 | s$director_here_1to2yr_2017 ==1 | s$director_here_3to5yr_2017==1))
db$outcome_observed <- ifelse(db$director_here_lessthan1yr_2017==0,1,0)
dba <- subset(db, db$director_appointed==1)

# Calculate p_0, the proportion of units in the control group that are induced to have outcome data because of their assignment to control 
qta <- nrow(dba[which(dba$mayor_lost==1 & dba$outcome_observed==1),])/nrow(dba[which(dba$mayor_lost==1),])
qca <- nrow(dba[which(dba$mayor_lost==0 & dba$outcome_observed==1),])/nrow(dba[which(dba$mayor_lost==0),])
qa <- (qca - qta)/qca

# Build trimmed datasets for lower and upper bounds
dba_c <- subset(dba, dba$mayor_lost==0 & dba$outcome_observed==1)
dba_c_lower <- subset(dba_c, dba_c$ideb_delta >= quantile(dba_c$ideb_delta,qa,na.rm=T)[[1]])
dba_c_upper <- subset(dba_c, dba_c$ideb_delta <= quantile(dba_c$ideb_delta,1-qa,na.rm=T)[[1]])
dba_t <- subset(dba, dba$mayor_lost==1 & dba$outcome_observed==1)
dba_lower <- rbind(dba_c_lower, dba_t)
dba_upper <- rbind(dba_c_upper, dba_t)

### Unappointed directors

# Generate dataset of unappointed directors
db <- subset(s, abs(s$challenger_margin)<local_bandwidth & !is.na(s$director_appointed) & !is.na(s$ideb_delta) & (s$director_here_lessthan3yr_2015==1) & (s$director_here_lessthan1yr_2017==1 | s$director_here_1to2yr_2017 ==1 | s$director_here_3to5yr_2017==1))
db$outcome_observed <- ifelse(db$director_here_lessthan1yr_2017==0,1,0)
dbnota <- subset(db, db$director_appointed==0)
dbnota <- subset(dbnota, !is.na(dbnota$ideb_delta))

# Calculate p_0, the proportion of units in the control group that are induced to have outcome data because of their assignment to control 
qtna <- nrow(dbnota[which(dbnota$mayor_lost==1 & dbnota$outcome_observed==1),])/nrow(dbnota[which(dbnota$mayor_lost==1),])
qcna <- nrow(dbnota[which(dbnota$mayor_lost==0 & dbnota$outcome_observed==1),])/nrow(dbnota[which(dbnota$mayor_lost==0),])
qna <- (qcna - qtna)/qcna

# Build trimmed datasets for lower and upper bounds
dbnota_c <- subset(dbnota, dbnota$mayor_lost==0 & dbnota$outcome_observed==1)
dbnota_c_lower <- subset(dbnota_c, dbnota_c$ideb_delta >= quantile(dbnota_c$ideb_delta,qna,na.rm=T)[[1]])
dbnota_c_upper <- subset(dbnota_c, dbnota_c$ideb_delta <= quantile(dbnota_c$ideb_delta,1-qna,na.rm=T)[[1]])
dbnota_t <- subset(dbnota, dbnota$mayor_lost==1 & dbnota$outcome_observed==1)
dbnota_lower <- rbind(dbnota_c_lower, dbnota_t)
dbnota_upper <- rbind(dbnota_c_upper, dbnota_t)

### Merge data for appointed & Unappointed directors

db_lower <- rbind(dbnota_c_lower, dbnota_t, dba_c_lower, dba_t)
db_upper <- rbind(dbnota_c_upper, dbnota_t, dba_c_upper, dba_t)

### Estimate bounds
lb <- felm(ideb_delta ~ mayor_lost*director_appointed | 0 | 0 | cod_ibge, data=db_lower)
ub <- felm(ideb_delta ~ mayor_lost*director_appointed | 0 | 0 | cod_ibge, data=db_upper)

# Print bounds
print(paste0("Bounds: [", round(summary(lb)$coefficients[[4,1]],3),", ", round(summary(ub)$coefficients[[4,1]],3), "]"))

# Generate 95% confidence interval for the bounds ---------------------------------------------------------------

# Number of replications
nboots <- 50000

# Generate datasets for appointed and unappointed directors
db <- subset(s, abs(s$challenger_margin)<local_bandwidth & !is.na(s$director_appointed) & !is.na(s$ideb_delta) & (s$director_here_lessthan3yr_2015==1) & (s$director_here_lessthan1yr_2017==1 | s$director_here_1to2yr_2017 ==1 | s$director_here_3to5yr_2017==1))
db$outcome_observed <- ifelse(db$director_here_lessthan1yr_2017==0,1,0)
db$id <- c(1:nrow(db))
dba <- subset(db, db$director_appointed==1)
dbna <- subset(db, db$director_appointed==0)

# Create empty vectors to store bounds
lower_bounds <- c()
upper_bounds <- c()

# Replicate procedure fore 50,000 samples with replacement
for(i in 1:nboots){
  # Appointed directors
  ba <- dba[sample(1:nrow(dba), nrow(dba), replace=T),] # Sample with replacement
  qta <- nrow(ba[which(ba$mayor_lost==1 & ba$outcome_observed==1),])/nrow(ba[which(ba$mayor_lost==1),])
  qca <- nrow(ba[which(ba$mayor_lost==0 & ba$outcome_observed==1),])/nrow(ba[which(ba$mayor_lost==0),])
  if(qta < qca){ # The standard case, where treatment group has more attrition
    qa <- (qca - qta)/qca
    ba_c <- subset(ba, ba$mayor_lost==0 & ba$outcome_observed==1)
    ba_c_lower <- subset(ba_c, ba_c$ideb_delta >= quantile(ba_c$ideb_delta,qa,na.rm=T)[[1]])
    ba_c_upper <- subset(ba_c, ba_c$ideb_delta <= quantile(ba_c$ideb_delta,1-qa,na.rm=T)[[1]])
    ba_t <- subset(ba, ba$mayor_lost==1 & ba$outcome_observed==1)
    ba_lower <- rbind(ba_c_lower, ba_t)
    ba_upper <- rbind(ba_c_upper, ba_t)
  }
  if(qta > qca){ # If the control group has more attrition
    qa <- (qta - qca)/qta
    ba_c <- subset(ba, ba$mayor_lost==0 & ba$outcome_observed==1)
    ba_t <- subset(ba, ba$mayor_lost==1 & ba$outcome_observed==1)
    ba_t_lower <- subset(ba_t, ba_t$ideb_delta <= quantile(ba_t$ideb_delta,qa,na.rm=T)[[1]])
    ba_t_upper <- subset(ba_t, ba_t$ideb_delta >= quantile(ba_t$ideb_delta,1-qa,na.rm=T)[[1]])
    ba_lower <- rbind(ba_t_lower, ba_c)
    ba_upper <- rbind(ba_t_upper, ba_c)
  }
  # Unappointed directors
  bna <- dbna[sample(1:nrow(dbna), nrow(dbna), replace=T),]
  qtna <- nrow(bna[which(bna$mayor_lost==1 & bna$outcome_observed==1),])/nrow(bna[which(bna$mayor_lost==1),])
  qcna <- nrow(bna[which(bna$mayor_lost==0 & bna$outcome_observed==1),])/nrow(bna[which(bna$mayor_lost==0),])
  if(qtna < qcna){ # The standard case, where treatment group has more attrition
    qna <- (qcna - qtna)/qcna
    bna_c <- subset(bna, bna$mayor_lost==0 & bna$outcome_observed==1)
    bna_c_lower <- subset(bna_c, bna_c$ideb_delta >= quantile(bna_c$ideb_delta,qna,na.rm=T)[[1]])
    bna_c_upper <- subset(bna_c, bna_c$ideb_delta <= quantile(bna_c$ideb_delta,1-qna,na.rm=T)[[1]])
    bna_t <- subset(bna, bna$mayor_lost==1 & bna$outcome_observed==1)
    bna_lower <- rbind(bna_c_lower, bna_t)
    bna_upper <- rbind(bna_c_upper, bna_t)
  }
  if(qtna > qcna){
    qna <- (qtna - qcna)/qtna
    bna_c <- subset(bna, bna$mayor_lost==0 & bna$outcome_observed==1)
    bna_t <- subset(bna, bna$mayor_lost==1 & bna$outcome_observed==1)
    bna_t_lower <- subset(bna_t, bna_t$ideb_delta <= quantile(bna_t$ideb_delta,qna,na.rm=T)[[1]])
    bna_t_upper <- subset(bna_t, bna_t$ideb_delta >= quantile(bna_t$ideb_delta,1-qna,na.rm=T)[[1]])
    bna_lower <- rbind(bna_t_lower, ba_c)
    bna_upper <- rbind(bna_t_upper, ba_c)
  }
  # Localized diff-in-diff
  bl <- rbind(bna_lower, bna_t, ba_lower, ba_t)
  bu <- rbind(bna_upper, bna_t, ba_upper, ba_t)
  lb <- lm(ideb_delta ~ mayor_lost*director_appointed, data=bl)
  ub <- lm(ideb_delta ~ mayor_lost*director_appointed, data=bu)
  # Store coefficients
  lower_bounds <- c(lower_bounds, coef(lb)["mayor_lost:director_appointed"][[1]])
  upper_bounds <- c(upper_bounds, coef(ub)["mayor_lost:director_appointed"][[1]])
}
bounds <- as_tibble(cbind(lower_bounds,upper_bounds))

# Generate confidence intervals, following Imbens and Manski 2004
sd_lower_bounds <- sd(bounds$lower_bounds,na.rm=T)
sd_upper_bounds <- sd(bounds$upper_bounds,na.rm=T)
delta <- mean(bounds$upper_bounds,na.rm=T) - mean(bounds$lower_bounds,na.rm=T)
db <- subset(s, abs(s$challenger_margin)<local_bandwidth & (s$director_here_lessthan3yr_2015==1) & (s$director_here_1to2yr_2017 ==1 | s$director_here_3to5yr_2017==1))
cbar <- 1.6449
pnorm(cbar+(sqrt(nrow(db))*delta)/max(sd_upper_bounds,sd_lower_bounds),0,1)-pnorm(-cbar)
low_bound_lowci <- mean(bounds$lower_bounds,na.rm=T)-(cbar*max(sd_lower_bounds,sd_upper_bounds))/sqrt(nrow(db))
upper_bound_upci <- mean(bounds$upper_bounds,na.rm=T)+(cbar*max(sd_lower_bounds,sd_upper_bounds))/sqrt(nrow(db))

# Print confidence intervals for the bounds
print(paste0("95% confidence interval for the bounds: [", round(low_bound_lowci,2),", ", round(upper_bound_upci, 2), "]"))

# NOTES -- R version, platform, and loaded packages -------------------------

### sessionInfo(package = NULL)

# R version 3.6.3 (2020-02-29)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS  10.16
# 
# Matrix products: default
# LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib
# 
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] Hmisc_4.4-0     lattice_0.20-40 texreg_1.37.1   xtable_1.8-4    ebal_0.1-6      Matching_4.9-7 
# [7] lfe_2.8-5       Matrix_1.2-18   multcomp_1.4-13 TH.data_1.0-10  MASS_7.3-51.5   mvtnorm_1.1-1  
# [13] rdd_0.57        Formula_1.2-3   AER_1.2-9       survival_3.1-11 car_3.0-7       carData_3.0-3  
# [19] lmtest_0.9-37   zoo_1.8-7       sandwich_2.5-1  rdrobust_0.99.7 forcats_0.5.0   stringr_1.4.0  
# [25] dplyr_1.0.0     purrr_0.3.3     readr_1.4.0     tidyr_1.0.2     tibble_3.0.0    ggplot2_3.3.5  
# [31] tidyverse_1.3.0
# 
# loaded via a namespace (and not attached):
#   [1] nlme_3.1-145        fs_1.4.1            lubridate_1.7.4     RColorBrewer_1.1-2  httr_1.4.1         
# [6] tools_3.6.3         backports_1.1.5     R6_2.4.1            rpart_4.1-15        DBI_1.1.0          
# [11] colorspace_1.4-1    nnet_7.3-13         withr_2.1.2         tidyselect_1.1.0    gridExtra_2.3      
# [16] curl_4.3            compiler_3.6.3      cli_2.3.0           rvest_0.3.5         htmlTable_1.13.3   
# [21] xml2_1.3.0          scales_1.1.0        checkmate_2.0.0     digest_0.6.25       foreign_0.8-76     
# [26] rio_0.5.16          base64enc_0.1-3     jpeg_0.1-8.1        pkgconfig_2.0.3     htmltools_0.4.0    
# [31] dbplyr_1.4.2        htmlwidgets_1.5.1   rlang_1.0.0         readxl_1.3.1        rstudioapi_0.11    
# [36] generics_0.0.2      jsonlite_1.6.1      acepack_1.4.1       zip_2.0.4           magrittr_1.5       
# [41] Rcpp_1.0.7          munsell_0.5.0       abind_1.4-5         lifecycle_0.2.0     stringi_1.4.6      
# [46] yaml_2.2.1          grid_3.6.3          parallel_3.6.3      crayon_1.3.4        haven_2.3.1        
# [51] splines_3.6.3       hms_0.5.3           knitr_1.28          pillar_1.4.3        codetools_0.2-16   
# [56] reprex_0.3.0        glue_1.3.2          latticeExtra_0.6-29 data.table_1.12.8   modelr_0.1.6       
# [61] vctrs_0.3.1         png_0.1-7           cellranger_1.1.0    gtable_0.3.0        assertthat_0.2.1   
# [66] xfun_0.20           openxlsx_4.1.4      broom_0.5.5         cluster_2.1.0       ellipsis_0.3.0     