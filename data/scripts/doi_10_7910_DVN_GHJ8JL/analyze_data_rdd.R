### Replication code
### Article: "How patronage delivers: Political appointments, bureaucratic accountability, and service delivery in Brazil"
### Author: Guillermo Toral (www.guillermotoral.com)
### Date: June 2022
### This file analyzes the regression discontinuity design data and generates plots and tables
### R version, platform, and package versions reported at the end of the file

# Prepare the environment -------------------------------------------------

### This section of the code prepares the environment 

# Clean the environment
rm(list = ls())

# Install required packages if not previously installed
package_list <- c("tidyverse", "rdrobust", "rdd", "lfe", "multcomp", "xtable", "texreg", "here", "Hmisc", "lattice") 
packages_to_install <- package_list[!(package_list %in% installed.packages()[,"Package"])]
if(length(packages_to_install)>0){
  install.packages(packages_to_install)
}

# Load required packages
library(tidyverse)
library(rdrobust)
library(rdd)
library(lfe)
library(multcomp) 
library(xtable)
library(texreg)
library(here)
library(Hmisc)
library(lattice)

# Set Working Directory to wherever this file is located.
setwd(here())
# The directory where the code folder is located must also have a "plots" and a "tables" subdirectory

# Load dataset ------------------------------------------------------------

d <- read_csv("../../datasets/analysis/rdd/rdd_analysis.csv")

# Load function that creates RD plots ---------------------------------------------

### This function creates RDD plots with two loess-regression lines and their corresponding confidence intervals on both sides of a discontinuity

myrdplot <- function(x,y,cutpoint=0,maintitle="",xtitle="",ytitle="",xrange=c(-1,1),
                     yrange=c(0,1),bins=100,legendabove="",legendbelow="", titlesize=1){
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


# Generate Table 4 (effect of meeting performance target on director turnover, by political appointment) ---------------------------------------------------------------

# Define bandwidth and kernel weights for models with heterogeneity
bw_calonico <- rdbwselect(d$director_turnover, x=d$fv, covs = d$treated_fv_appointed + d$treated_appointed + d$fv_appointed + d$director_appointed, c= 0)
bw <- bw_calonico$bws[[1,1]]
d_int <- subset(d, abs(d$fv)<bw)
d_int$w <- as.numeric(NA)
d_int[which(d_int$fv < 0),"w"] <- 1-abs(d_int[which(d_int$fv < 0),"fv"]/bw)
d_int[which(d_int$fv >= 0),"w"] <- 1-abs(d_int[which(d_int$fv >= 0),"fv"]/bw)

# Define bandwidth and kernel for models without heterogeneity
bw_calonico_noint <- rdbwselect(d$director_turnover, x=d$fv, c= 0)
bw_noint <- bw_calonico_noint$bws[[1,1]] # 0.21
d_noint <- subset(d, abs(d$fv)<bw)
d_noint$w <- as.numeric(NA)
d_noint[which(d_noint$fv < 0),"w"] <- 1-abs(d_noint[which(d_noint$fv < 0),"fv"]/bw)
d_noint[which(d_noint$fv >= 0),"w"] <- 1-abs(d_noint[which(d_noint$fv >= 0),"fv"]/bw)

# Model 1: No heterogeneity
r1 <- lm(director_turnover ~ treated*fv, data=d_noint, weights=d_noint$w)
# Model 2: HLATE for appointed directors, no controls
r2 <- lm(director_turnover ~ treated*fv*director_appointed, data=d_int, weights=d_int$w)
lh2 <- summary(glht(r2, linfct = c("treated + treated:director_appointed==0"),vcov=vcovHC(r2,type = "HC1")))
# Model 3: HLATE for appointed directors, controlling for state fixed effects
r3 <- lm(director_turnover ~ treated*fv*director_appointed + as.factor(uf), data=d_int, weights=d_int$w)
lh3 <- summary(glht(r3, linfct = c("treated + treated:director_appointed==0"),vcov=vcovHC(r3,type = "HC1")))
# Model 4: HLATE for appointed directors, controlling for state fixed effects and predictors of appointed
r4 <- lm(director_turnover ~ treated*fv*director_appointed 
         + female + age_forties + race_white + race_black_or_brown + schooling_postgraduate + exclusive_dedication + teacher_experience_6to15yr + director_experience_3to10yr + director_experience_over10yr + director_here_3to10yr + director_here_over10yr + log_gdp_percapita + log_population + deaths_perthousand + electoral_concentration + rural + students_per_classroom + school_inse
         + as.factor(uf), data=d_int, weights=d_int$w)
lh4 <- summary(glht(r4, linfct = c("treated + treated:director_appointed==0"),vcov=vcovHC(r4,type = "HC1")))

# Predictors of appointed come from the following regression
m3 <- lm(director_appointed ~ # Director & municipality & school covariates
           female + age_forties + age_above50 + race_white + race_black_or_brown + schooling_tertiary + schooling_postgraduate + exclusive_dedication + teacher_experience_6to15yr + teacher_experience_over15yr + director_experience_3to10yr + director_experience_over10yr + director_here_3to10yr + director_here_over10yr
         + log_gdp_percapita + log_population + deaths_perthousand + mayor_first_term + electoral_concentration
         + rural + log_number_workers + students_per_classroom + school_inse
         , data=d)
### The following code creates a list of the significant predictors
# paste(names(coefficients(m3))[which(coeftest(m3,vcov=vcovHC(m3,type="HC1"))[,4]<0.05)],collapse=" + ")

# Export regression results, using HC1 standard errors
texreg(list(r1,r2,r3,r4),
       file="../../tables/rdd_appointed.tex", table=F, float.pos="htp", scalebox=.75,
       digits=3, booktabs = T, dcolumn=T,use.packages=F,single.row = F,custom.note="",
       override.pvalues = list(coeftest(r1,vcov=vcovHC(r1,type="HC1"))[,4],coeftest(r2,vcov=vcovHC(r2,type="HC1"))[,4],coeftest(r3,vcov=vcovHC(r3,type="HC1"))[,4],coeftest(r4,vcov=vcovHC(r4,type="HC1"))[,4]),
       override.se = list(coeftest(r1,vcov=vcovHC(r1,type="HC1"))[,2],coeftest(r2,vcov=vcovHC(r2,type="HC1"))[,2],coeftest(r3,vcov=vcovHC(r3,type="HC1"))[,2],coeftest(r4,vcov=vcovHC(r4,type="HC1"))[,2]),
       custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
       custom.coef.map = list("treated" = "$\\hat \\beta_1$: Quality target met", "treated:director_appointed" = "$\\hat \\gamma_2$: Quality target met $\\times$ Appointed"),
       custom.gof.names = c("N"),
       include.adjrs=FALSE, include.rsq=F, include.n=T,include.groups=F,
       stars = c(0.05, 0.01),
       custom.gof.rows = list("$\\hat \\beta_1 + \\hat \\gamma_2$" = c("",paste0(round(lh2$test$coef[[1]],3),ifelse(lh2$test$pvalues[[1]]<0.001,"$^{***}$",ifelse(lh2$test$pvalues[[1]]<0.01,"$^{**}$",ifelse(lh2$test$pvalues[[1]]<0.05,"$^{*}$",""))),sep=""),
                                                                       paste0(round(lh3$test$coef[[1]],3),ifelse(lh3$test$pvalues[[1]]<0.001,"$^{***}$",ifelse(lh3$test$pvalues[[1]]<0.01,"$^{**}$",ifelse(lh3$test$pvalues[[1]]<0.05,"$^{*}$",""))),sep=""),
                                                                       paste0(round(lh4$test$coef[[1]],3),ifelse(lh4$test$pvalues[[1]]<0.001,"$^{***}$",ifelse(lh4$test$pvalues[[1]]<0.01,"$^{**}$",ifelse(lh4$test$pvalues[[1]]<0.05,"$^{*}$",""))),sep="")),
                              " " = c("", paste0("(",round(lh2$test$sigma[[1]],3),")",sep=""),paste0("(",round(lh3$test$sigma[[1]],3),")",sep=""),paste0("(",round(lh4$test$sigma[[1]],3),")",sep="")),
                              "State fixed effects" = c("","",rep("$\\checkmark$",2)),
                              "Predictors of Appointed" = c(rep("",3),"$\\checkmark$"),
                              "Bandwidth" = c(bw_noint, rep(bw,3))))

# Generate Figure 4 (effect of meeting performance target on director turnover, among appointed directors) -----------------------------------------

# Subset the data to schools with appointed directors
d_appointed <- subset(d, d$director_appointed==1)

# Create plot with the myrdplot function
pdf("../../plots/rdplot_appointed.pdf",width=9, height=5)
par(mar=c(5.1, 6.1, 4.1, 2.1))
myrdplot(x = d_appointed$ideb_gap_centered, 
         y = d_appointed$director_turnover, cutpoint = 0,
         xrange = c(-1,1), yrange=c(0.05,0.3),bins=40,
         xtitle = "School quality score - target (2013)",
         ytitle = "Director turnover within one year of \n the publication of the results", 
         legendabove="Schools that met their target",
         legendbelow="Schools that missed their target")
grid()
dev.off()

# Generate Figure 5 (alternative RD bandwidths) ---------------------------------------------------

# Create empty vectors to store the HLATE, bandwidth, and confidence intervals
joint_effect <- c()
bandwidth <- c()
cilow <- c()
ciup <- c()

# Iterate through alternative bandwidths, from half to twice the optimal one
for (i in seq(from=bw_calonico$bws[[1]]/2, to=bw_calonico$bws[[1]]*2, by=0.005)){
  # For each bandwidth, subset the data to observations within the bandwidth
  d_bw <- subset(d, abs(d$fv)<i)
  d_bw$w <- as.numeric(NA)
  # Generate kernel weights
  d_bw[which(d_bw$fv < 0),"w"] <- 1-abs(d_bw[which(d_bw$fv < 0),"fv"]/i)
  d_bw[which(d_bw$fv >= 0),"w"] <- 1-abs(d_bw[which(d_bw$fv >= 0),"fv"]/i)
  # Run the regression 
  r <- lm(director_turnover ~ treated*fv*director_appointed
          + female + age_forties + race_white + race_black_or_brown + schooling_postgraduate + exclusive_dedication + teacher_experience_6to15yr + director_experience_3to10yr + director_experience_over10yr + director_here_3to10yr + director_here_over10yr + log_gdp_percapita + log_population + deaths_perthousand + electoral_concentration + rural + students_per_classroom + school_inse
          + as.factor(uf),data=d_bw, weights=d_bw$w)
  # Extract and store the HLATE
  joint_effect <- c(joint_effect, summary(glht(r, linfct = c("treated + treated:director_appointed==0"),vcov=vcovHC(r,type = "HC1")))$test$coef[[1]])
  # Extract and store the HLATE's standard error
  joint_effect_se <- summary(glht(r, linfct = c("treated + treated:director_appointed==0"),vcov=vcovHC(r,type = "HC1")))$test$sigma[[1]]
  # Store the bandwidth
  bandwidth <- c(bandwidth,i)
  # Extract and store the confidence interval for the HLATE
  cilow <- c(cilow,summary(glht(r, linfct = c("treated + treated:director_appointed==0"),vcov=vcovHC(r,type = "HC1")))$test$coef[[1]]-qt(.975,r$df)*joint_effect_se)
  ciup <- c(ciup,summary(glht(r, linfct = c("treated + treated:director_appointed==0"),vcov=vcovHC(r,type = "HC1")))$test$coef[[1]]+qt(.975,r$df)*joint_effect_se)
}

# Bind results in a dataframe 
results <- data.frame(bandwidth,joint_effect,cilow,ciup)

# Plot the HLATE estimates as a function of alternative bandwidths
pdf("../../plots/rdd_bandwidths.pdf",width=9, height=5)
par(mar=c(5.1, 5.1, 3.1, 2.1))
plot(results$bandwidth,results$joint_effect,ylim=c(-0.25,0.15),xlab="RD bandwidth",
     ylab="Treatment effect for schools \n with appointed directors",pch=19)
grid()
arrows(results$bandwidth, results$cilow, results$bandwidth, results$ciup, length=0.05, angle=90, code=3,col="darkgrey")
abline(h=0,col="red",lty=2)
abline(v=bw,col="blue",lwd=2)
legend("bottomright",c("Optimal bandwidth"),lwd=2,col="blue",bty="n")
dev.off()

# SUPPLEMENTARY ANALYSES (reported in the Online Appendix) ---------------------
# Generate Table 16 (continuity of pre-treatment covariates) ----------------------------------

# Define vector of covariates
covariate <- c("director_appointed","director_elected","director_civil_service","female","age_below40","age_forties","age_above50","race_white","race_black_or_brown","race_other","schooling_lessthantertiary", "schooling_tertiary", "schooling_postgraduate","exclusive_dedication","teacher_experience_lessthan6yr","teacher_experience_6to15yr", "teacher_experience_over15yr", "director_experience_lessthan3yr", "director_experience_3to10yr", "director_experience_over10yr", "director_here_lessthan3yr", "director_here_3to10yr", "director_here_over10yr",
               "log_gdp_percapita", "log_population", "deaths_perthousand", "mayor_first_term", "electoral_concentration","party_large_programmatic",
               "rural", "number_workers", "in_settlement_land", "in_indigenous_land", "in_quilombola_land", "students_per_classroom", "school_inse",
               "ideb_target", "ideb_2011", "test_scores_2011", "passing_2011")

# Create empty vectors to store bandwidth, estimates, p-values, and standard errors
bandwidth <- c()
rdestimate <- c()
rdpvalue <- c()
rdse <- c()

# Iterate through covariates
for(i in 1:length(covariate)){
  # Run an RD regression and extract the bandwidth
  calonico <- rdrobust(d[,covariate[i]][[1]], d$fv)
  bandwidth <- c(bandwidth, calonico$bws[[1,1]])
  # Subset dataset to data within the bandwidth and define kernel weights
  d_bw <- subset(d, abs(d$fv)<calonico$bws[[1,1]])
  d_bw$w <- as.numeric(NA)
  d_bw[which(d_bw$fv < 0),"w"] <- 1-abs(d_bw[which(d_bw$fv < 0),"fv"]/calonico$bws[[1,1]])
  d_bw[which(d_bw$fv >= 0),"w"] <- 1-abs(d_bw[which(d_bw$fv >= 0),"fv"]/calonico$bws[[1,1]])
  # Run regression within the bandwidth
  r <- lm(d_bw[,covariate[i]][[1]] ~ treated*fv, data=d_bw,weights=d_bw$w)
  # Store the RD estimate, its p-value, and its standard error
  rdestimate <- c(rdestimate, coeftest(r,vcov=vcovHC(r,type="HC1"))["treated",1])
  rdpvalue <- c(rdpvalue, coeftest(r,vcov=vcovHC(r,type="HC1"))["treated",4])
  rdse <- c(rdse, coeftest(r,vcov=vcovHC(r,type="HC1"))["treated",2])
}

# Bind it all togethre in a dataframe
balance_table <- as.data.frame(cbind(covariate, rdestimate, rdse, rdpvalue))
balance_table$rdestimate <- as.numeric(as.character(balance_table$rdestimate))
balance_table$rdpvalue <- as.numeric(as.character(balance_table$rdpvalue))
balance_table$rdse <- as.numeric(as.character(balance_table$rdse))
balance_table <- balance_table[,c(2:4)]
rownames(balance_table) <- c("Director is appointed","Director is elected","Director is civil service","Director is female", "Director is aged <40", "Director is aged 40-49", "Director is aged 50+", "Director is white", "Director is black/brown", "Director has other race", "Director has < tertiary education", "Director has a tertiary degree", "Director has a postgraduate degree", "Director has no other job", "Director has <6 years of teaching experience", "Director has 6-15 years of teaching experience", "Director has >15 years of teaching experience", "Director has <3 years of director experience", "Director has 3-10 years of director experience", "Director has >10 years of director experience", "Director has held position for <3 years", "Director has held position for 3-10 years", "Director has held position for >10 years", "Municipality GDP per capita (log)", "Municipality population (log)", "Municipality deaths per 1,000", "Mayor is in their first term", "Municipal electoral concentration", "Mayor belongs to a large, programmatic party", "School is rural", "Number of staff in the school", "School is in a settlement", "School is in indigenous land", "School is in quilombola land", "Students per classroom (average)", "School socioeconomic index", "School IDEB target for 2013", "School IDEB score in 2011", "School ANRESC test scores 2011", "School student passing rate 2011")
colnames(balance_table) <- c("RD estimate","Standard error","p value")

# Export as a table
print(file="../../tables/rdd_balance.tex",
      type="latex",
      xtable(balance_table, 
             digits=3, 
             align=c("lccc")),
      caption.placement="top",
      comment=F,
      floating=F,
      booktabs=T,
      column.names=T)

# Generate Figure 18 (continuity of the forcing variable)  ---------------------------------------------------------------

# Generate plot with histogram and density plot
pdf("../../plots/rdd_forcing_variable.pdf", width=12, height=5)
par(mfrow=c(1,2))
hist(d$ideb_gap_centered, breaks=80,main="",xlab="IDEB score - IDEB target (centered)",col="gray")
abline(v=0,col="red")
legend("topright", c("RD threshold"),lty=1, col="red",bty="n")
DCdensity(d$ideb_gap_centered,c=0)
title(main="", xlab = "IDEB target - IDEB score (centered)", ylab="Density",
      cex.lab=1,cex.main=1.2)
abline(v=0,col="red")
legend("topleft",c(paste0("p-value: ",round(DCdensity(d$ideb_gap_centered,c=0,plot=F),3))),bty="n",cex=0.9)
dev.off()

# Generate Figure 19 (effect among unappointed directors) --------------------------------------------

# Create subsets of the data for schools with elected and with civil service directors
d_elected <- subset(d, d$director_elected==1)
d_tenured <- subset(d, d$director_civil_service==1)

# Generate RD plots for each subset and place them side by side
pdf("../../plots/rdplot_unappointed.pdf",width=12,height=5)
par(mar=c(5.1, 6.1, 4.1, 2.1))
par(mfrow=c(1,2))
myrdplot(x = d_elected$ideb_gap_centered, 
         y = d_elected$director_turnover, cutpoint = 0,
         main = "Elected directors",
         xrange = c(-1,1), yrange=c(0,0.4),bins=40,
         xtitle = "School quality score - target (2013)",
         ytitle = "Director turnover within one year of \n the publication of the results", 
         legendabove="Schools that \n met their target",
         legendbelow="Schools that \n missed their target")
grid()
myrdplot(x = d_tenured$ideb_gap_centered, 
         y = d_tenured$director_turnover, cutpoint = 0,
         xrange = c(-1,1), yrange=c(0,0.4),bins=50,
         main = "Civil service directors",
         xtitle = "School quality score - target (2013)",
         ytitle = "Director turnover within one year of \n the publication of the results", 
         legendabove="Schools that \n met their target",
         legendbelow="Schools that \n missed their target")
grid()
dev.off()

# Generate Table 17 (effects among elected directors) ---------------------------------------------------------------

# Bandwidth and weights for models with heterogeneity
bw_calonico <- rdbwselect(d$director_turnover, x=d$fv, covs = d$treated_fv_elected + d$treated_elected + d$fv_elected + d$director_elected, c= 0)
bw <- bw_calonico$bws[[1,1]]
d_int <- subset(d, abs(d$fv)<bw)
d_int$w <- as.numeric(NA)
d_int[which(d_int$fv < 0),"w"] <- 1-abs(d_int[which(d_int$fv < 0),"fv"]/bw)
d_int[which(d_int$fv >= 0),"w"] <- 1-abs(d_int[which(d_int$fv >= 0),"fv"]/bw)

# Bandwidth and weights for models without heterogeneity
bw_calonico_noint <- rdbwselect(d$director_turnover, x=d$fv, c= 0)
bw_noint <- bw_calonico_noint$bws[[1,1]] # 0.21
d_noint <- subset(d, abs(d$fv)<bw)
d_noint$w <- as.numeric(NA)
d_noint[which(d_noint$fv < 0),"w"] <- 1-abs(d_noint[which(d_noint$fv < 0),"fv"]/bw)
d_noint[which(d_noint$fv >= 0),"w"] <- 1-abs(d_noint[which(d_noint$fv >= 0),"fv"]/bw)

# Regressions
r1 <- lm(director_turnover ~ treated*fv, data=d_noint,weights=d_noint$w)
r2 <- lm(director_turnover ~ treated*fv*director_elected, data=d_int, weights=d_int$w)
lh2 <- summary(glht(r2, linfct = c("treated + treated:director_elected==0"),vcov=vcovHC(r2,type = "HC1")))
r3 <- lm(director_turnover ~ treated*fv*director_elected + as.factor(uf),  data=d_int, weights=d_int$w)
lh3 <- summary(glht(r3, linfct = c("treated + treated:director_elected==0"),vcov=vcovHC(r3,type = "HC1")))
r4 <- lm(director_turnover ~ treated*fv*director_elected 
         + female + age_forties + race_white + schooling_tertiary + schooling_postgraduate + exclusive_dedication + teacher_experience_6to15yr + teacher_experience_over15yr + director_experience_3to10yr + director_experience_over10yr + director_here_3to10yr + director_here_over10yr + log_gdp_percapita + log_population + deaths_perthousand + mayor_first_term + rural + log_number_workers + students_per_classroom + school_inse
         + as.factor(uf),  data=d_int, weights=d_int$w)
lh4 <- summary(glht(r4, linfct = c("treated + treated:director_elected==0"),vcov=vcovHC(r4,type = "HC1")))

# Predictors of elected come from the following regression:
m4 <- lm(director_elected ~ 
             female + age_forties + age_above50 + race_white + race_black_or_brown + schooling_tertiary + schooling_postgraduate + exclusive_dedication + teacher_experience_6to15yr + teacher_experience_over15yr + director_experience_3to10yr + director_experience_over10yr + director_here_3to10yr + director_here_over10yr
           + log_gdp_percapita + log_population + deaths_perthousand + mayor_first_term + electoral_concentration
           + rural + log_number_workers + students_per_classroom + school_inse
           , data=d)
## The following line generates a list of the significant predictors
# paste(names(coefficients(m4))[which(coeftest(m4,vcov=vcovHC(m4,type="HC1"))[,4]<0.05)],collapse=" + ")

texreg(list(r1,r2,r3,r4),
       file="../../tables/rdd_elected.tex", table=F, float.pos="htp", scalebox=.75,
       digits=3, booktabs = T, dcolumn=T, use.packages=F,single.row = F,custom.note="",
       override.pvalues = list(coeftest(r1,vcov=vcovHC(r1,type="HC1"))[,4],coeftest(r2,vcov=vcovHC(r2,type="HC1"))[,4],coeftest(r3,vcov=vcovHC(r3,type="HC1"))[,4],coeftest(r4,vcov=vcovHC(r4,type="HC1"))[,4]),
       override.se = list(coeftest(r1,vcov=vcovHC(r1,type="HC1"))[,2],coeftest(r2,vcov=vcovHC(r2,type="HC1"))[,2],coeftest(r3,vcov=vcovHC(r3,type="HC1"))[,2],coeftest(r4,vcov=vcovHC(r4,type="HC1"))[,2]),
       custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
       custom.coef.map = list("treated" = "$\\hat \\beta_1$: Quality target met", "treated:director_elected" = "$\\hat \\gamma_2$: Quality target met $\\times$ Elected"),
       custom.gof.names = c("N"),
       include.adjrs=FALSE, include.rsq=F, include.n=T,include.groups=F,
       stars = c(0.05, 0.01, 0.001),
       custom.gof.rows = list("$\\hat \\beta_1 + \\hat \\gamma_2$" = c("",paste0(format(round(lh2$test$coef[[1]],3),nsmall=3),ifelse(lh2$test$pvalues[[1]]<0.001,"$^{***}$",ifelse(lh2$test$pvalues[[1]]<0.01,"$^{**}$",ifelse(lh2$test$pvalues[[1]]<0.05,"$^{*}$",""))),sep=""),
                                                                       paste0(format(round(lh3$test$coef[[1]],3),nsmall=3),ifelse(lh3$test$pvalues[[1]]<0.001,"$^{***}$",ifelse(lh3$test$pvalues[[1]]<0.01,"$^{**}$",ifelse(lh3$test$pvalues[[1]]<0.05,"$^{*}$",""))),sep=""),
                                                                       paste0(format(round(lh4$test$coef[[1]],3),nsmall=3),ifelse(lh4$test$pvalues[[1]]<0.001,"$^{***}$",ifelse(lh4$test$pvalues[[1]]<0.01,"$^{**}$",ifelse(lh4$test$pvalues[[1]]<0.05,"$^{*}$",""))),sep="")),
                              " " = c("", paste0("(",format(round(lh2$test$sigma[[1]],3),nsmall=3),")",sep=""),paste0("(",format(round(lh3$test$sigma[[1]],3),nsmall=3),")",sep=""),paste0("(",format(round(lh4$test$sigma[[1]],3),nsmall=3),")",sep="")),
                              "State fixed effects" = c("","",rep("$\\checkmark$",2)),
                              "Predictors of Elected" = c(rep("",3),"$\\checkmark$"),
                              "Bandwidth" = c(bw_noint, rep(bw,3))))

# Generate Table 18 (effects among civil service directors) ---------------------------------------------------------------

# Bandwidth and weights for models with heterogeneity
bw_calonico <- rdbwselect(d$director_turnover, x=d$fv, covs = d$treated_fv_civil_service + d$treated_civil_service + d$fv_civil_service + d$director_civil_service, c= 0)
bw <- bw_calonico$bws[[1,1]]
d_int <- subset(d, abs(d$fv)<bw)
d_int$w <- as.numeric(NA)
d_int[which(d_int$fv < 0),"w"] <- 1-abs(d_int[which(d_int$fv < 0),"fv"]/bw)
d_int[which(d_int$fv >= 0),"w"] <- 1-abs(d_int[which(d_int$fv >= 0),"fv"]/bw)

# Bandwidth and weights for models without heterogeneity
bw_calonico_noint <- rdbwselect(d$director_turnover, x=d$fv, c= 0)
bw_noint <- bw_calonico_noint$bws[[1,1]] # 0.21
d_noint <- subset(d, abs(d$fv)<bw)
d_noint$w <- as.numeric(NA)
d_noint[which(d_noint$fv < 0),"w"] <- 1-abs(d_noint[which(d_noint$fv < 0),"fv"]/bw)
d_noint[which(d_noint$fv >= 0),"w"] <- 1-abs(d_noint[which(d_noint$fv >= 0),"fv"]/bw)

# Regressions
r1 <- lm(director_turnover ~ treated*fv, data=d_noint,weights=d_noint$w)
r2 <- lm(director_turnover ~ treated*fv*director_civil_service, data=d_int,weights=d_int$w)
lh2 <- summary(glht(r2, linfct = c("treated + treated:director_civil_service==0"),vcov=vcovHC(r2,type = "HC1")))
r3 <- lm(director_turnover ~ treated*fv*director_civil_service + as.factor(uf), data=d_int,weights=d_int$w)
lh3 <- summary(glht(r3, linfct = c("treated + treated:director_civil_service==0"),vcov=vcovHC(r3,type = "HC1")))
r4 <- lm(director_turnover ~ treated*fv*director_civil_service 
         + female + age_forties + race_white + schooling_tertiary + schooling_postgraduate + exclusive_dedication + teacher_experience_6to15yr + teacher_experience_over15yr + director_experience_3to10yr + director_experience_over10yr + director_here_3to10yr + director_here_over10yr + log_gdp_percapita + log_population + deaths_perthousand + mayor_first_term + rural + log_number_workers + students_per_classroom + school_inse
         + as.factor(uf), data=d_int,weights=d_int$w)
lh4 <- summary(glht(r4, linfct = c("treated + treated:director_civil_service==0"),vcov=vcovHC(r4,type = "HC1")))

# Predictors of civil service come from the following regression:
m5 <- lm(director_civil_service ~ 
             female + age_forties + age_above50 + race_white + race_black_or_brown + schooling_tertiary + schooling_postgraduate + exclusive_dedication + teacher_experience_6to15yr + teacher_experience_over15yr + director_experience_3to10yr + director_experience_over10yr + director_here_3to10yr + director_here_over10yr
           + log_gdp_percapita + log_population + deaths_perthousand + mayor_first_term + electoral_concentration
           + rural + log_number_workers + students_per_classroom + school_inse
           , data=d)
paste(names(coefficients(m5))[which(coeftest(m5,vcov=vcovHC(m5,type="HC1"))[,4]<0.05)],collapse=" + ")

# Export results
texreg(list(r1,r2,r3,r4),
       file="../../tables/rdd_civil_service.tex", table=F, float.pos="htp", scalebox=.75,
       digits=3, booktabs = T, dcolumn=T,use.packages=F,single.row = F,custom.note="",
       override.pvalues = list(coeftest(r1, type="HC1")[,4],coeftest(r2, type="HC1")[,4],coeftest(r3, type="HC1")[,4],coeftest(r4, type="HC1")[,4]),
       override.se = list(coeftest(r1, type="HC1")[,2],coeftest(r2, type="HC1")[,2],coeftest(r3, type="HC1")[,2],coeftest(r4, type="HC1")[,2]),
       custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
       custom.coef.map = list("treated" = "$\\hat \\beta_1$: Quality target met", "treated:director_civil_service" = "$\\hat \\gamma_2$: Quality target met $\\times$ Civil service"),
       custom.gof.names = c("N"),
       include.adjrs=FALSE, include.rsq=F, include.n=T,include.groups=F,
       stars = c(0.05, 0.01, 0.001),
       custom.gof.rows = list("$\\hat \\beta_1 + \\hat \\gamma_2$" = c("",paste0(round(lh2$test$coef[[1]],3),ifelse(lh2$test$pvalues[[1]]<0.001,"$^{***}$",ifelse(lh2$test$pvalues[[1]]<0.01,"$^{**}$",ifelse(lh2$test$pvalues[[1]]<0.05,"$^{*}$",""))),sep=""),
                                                                       paste0(round(lh3$test$coef[[1]],3),ifelse(lh3$test$pvalues[[1]]<0.001,"$^{***}$",ifelse(lh3$test$pvalues[[1]]<0.01,"$^{**}$",ifelse(lh3$test$pvalues[[1]]<0.05,"$^{*}$",""))),sep=""),
                                                                       paste0(round(lh4$test$coef[[1]],3),ifelse(lh4$test$pvalues[[1]]<0.001,"$^{***}$",ifelse(lh4$test$pvalues[[1]]<0.01,"$^{**}$",ifelse(lh4$test$pvalues[[1]]<0.05,"$^{*}$",""))),sep="")),
                              " " = c("", paste0("(",round(lh2$test$sigma[[1]],3),")",sep=""),paste0("(",round(lh3$test$sigma[[1]],3),")",sep=""),paste0("(",round(lh4$test$sigma[[1]],3),")",sep="")),
                              "State fixed effects" = c("","",rep("$\\checkmark$",2)),
                              "Predictors of Civil service" = c(rep("",3),"$\\checkmark$"),
                              "Bandwidth" = c(bw_noint, rep(bw,3))))

# Generate Figure 20 (placebo tests varying the RD threshold) -----------------------------------------------------------------

joint_effect <- c()
threshold <- c()
cilow <- c()
ciup <- c()
for (i in seq(from=-1.5, to=1.5, by=0.15)){
  d_placebo <- d
  d_placebo$forcingvar_placebo <- d_placebo$fv-i
  d_placebo$treated_placebo <- ifelse(d_placebo$forcingvar_placebo>=0,1,0)
  d_placebo$treated_forcingvar_appointed_placebo <- d_placebo$treated_placebo*d_placebo$forcingvar_placebo*d_placebo$director_appointed
  d_placebo$treated_appointed_placebo <- d_placebo$treated_placebo*d_placebo$director_appointed
  d_placebo$forcingvar_appointed_placebo <- d_placebo$forcingvar_placebo*d_placebo$director_appointed
  bwselect_placebo <- rdrobust(d_placebo$director_turnover, d_placebo$forcingvar_placebo, covs=d_placebo$treated_forcingvar_appointed_placebo + d_placebo$forcingvar_appointed_placebo + d_placebo$treated_appointed_placebo + d_placebo$director_appointed, all=T)
  bw_placebo <- bwselect_placebo$bws[[1,1]]
  d_bw <- subset(d_placebo, abs(d_placebo$forcingvar_placebo)<bw_placebo)
  d_bw$w <- as.numeric(NA)
  d_bw[which(d_bw$forcingvar_placebo < 0),"w"] <- 1-abs(d_bw[which(d_bw$forcingvar_placebo < 0),"forcingvar_placebo"]/bw_placebo)
  d_bw[which(d_bw$forcingvar_placebo >= 0),"w"] <- 1-abs(d_bw[which(d_bw$forcingvar_placebo >= 0),"forcingvar_placebo"]/bw_placebo)
  r <- lm(director_turnover ~ treated_placebo*forcingvar_placebo*director_appointed,data=d_bw, weights=d_bw$w)
  joint_effect <- c(joint_effect, summary(glht(r, linfct = c("treated_placebo + treated_placebo:director_appointed==0"),vcov=vcovHC(r,type = "HC1")))$test$coef[[1]])
  joint_effect_se <- summary(glht(r,linfct = c("treated_placebo + treated_placebo:director_appointed==0"),vcov=vcovHC(r,type = "HC1")))$test$sigma[[1]]
  threshold <- c(threshold, i)
  cilow <- c(cilow,summary(glht(r,linfct = c("treated_placebo + treated_placebo:director_appointed==0"),vcov=vcovHC(r,type = "HC1")))$test$coef[[1]]-qt(.975,r$df)*joint_effect_se)
  ciup <- c(ciup,summary(glht(r,linfct = c("treated_placebo + treated_placebo:director_appointed==0"),vcov=vcovHC(r,type = "HC1")))$test$coef[[1]]+qt(.975,r$df)*joint_effect_se)
}

results <- data.frame(threshold,joint_effect,cilow,ciup)

pdf("../../plots/rdd_placebo_threshold.pdf",width=9, height=4)
par(mar=c(5.1, 5.1, 4.1, 2.1))
plot(results$threshold,results$joint_effect,ylim=c(-0.21,0.21),xlab="Placebo RD thresholds",
     ylab="Treatment effect for schools \n with appointed directors",pch=19)
arrows(results$threshold, results$cilow, results$threshold, results$ciup, length=0.05, angle=90, code=3,col="black")
abline(h=0,col="red",lty=2)
abline(v=0,col="blue",lwd=2)
legend("bottomright",c("Actual RD threshold (centered)"),lwd=2,col="blue",bty="n")
dev.off()


# Generate Table 19 (effects in municipalities with programmatic mayors) --------

# Subset to municipalities where the mayor belongs to a large, programmatic mayor
dp <- subset(d, d$party_large_programmatic==1)

# Bandwidth and weights for models with heterogeneity
bw_calonico <- rdbwselect(dp$director_turnover, x=dp$fv, covs = dp$treated_fv_appointed + dp$treated_appointed + dp$fv_appointed + dp$director_appointed, c= 0)
bw <- bw_calonico$bws[[1,1]]
d_int <- subset(dp, abs(dp$fv)<bw)
d_int$w <- as.numeric(NA)
d_int[which(d_int$fv < 0),"w"] <- 1-abs(d_int[which(d_int$fv < 0),"fv"]/bw)
d_int[which(d_int$fv >= 0),"w"] <- 1-abs(d_int[which(d_int$fv >= 0),"fv"]/bw)

# Bandwidth and weights for models without heterogeneity
bw_calonico_noint <- rdbwselect(dp$director_turnover, x=dp$fv, c= 0)
bw_noint <- bw_calonico_noint$bws[[1,1]] # 0.21
d_noint <- subset(dp, abs(dp$fv)<bw)
d_noint$w <- as.numeric(NA)
d_noint[which(d_noint$fv < 0),"w"] <- 1-abs(d_noint[which(d_noint$fv < 0),"fv"]/bw)
d_noint[which(d_noint$fv >= 0),"w"] <- 1-abs(d_noint[which(d_noint$fv >= 0),"fv"]/bw)

# Regressions
r1 <- lm(director_turnover ~ treated*fv, data=d_noint, weights=d_noint$w)
r2 <- lm(director_turnover ~ treated*fv*director_appointed, data=d_int, weights=d_int$w)
lh2 <- summary(glht(r2, linfct = c("treated + treated:director_appointed==0"),vcov=vcovHC(r2,type = "HC1")))
r3 <- lm(director_turnover ~ treated*fv*director_appointed + as.factor(uf), data=d_int, weights=d_int$w)
lh3 <- summary(glht(r3, linfct = c("treated + treated:director_appointed==0"),vcov=vcovHC(r3,type = "HC1")))
r4 <- lm(director_turnover ~ treated*fv*director_appointed 
         + female + age_forties + race_white + race_black_or_brown + schooling_postgraduate + exclusive_dedication + teacher_experience_6to15yr + director_experience_3to10yr + director_experience_over10yr + director_here_3to10yr + director_here_over10yr + log_gdp_percapita + log_population + deaths_perthousand + electoral_concentration + rural + students_per_classroom + school_inse
         + as.factor(uf), data=d_int, weights=d_int$w)
lh4 <- summary(glht(r4, linfct = c("treated + treated:director_appointed==0"),vcov=vcovHC(r4,type = "HC1")))

# # Predictors of appointed come from the following regression
# m3 <- lm(director_appointed ~ 
#              female + age_forties + age_above50 + race_white + race_black_or_brown + schooling_tertiary + schooling_postgraduate + exclusive_dedication + teacher_experience_6to15yr + teacher_experience_over15yr + director_experience_3to10yr + director_experience_over10yr + director_here_3to10yr + director_here_over10yr
#            + log_gdp_per_capita + log_population + deaths_per_thousand + mayor_first_term + electoral_concentration
#            + rural + log_number_workers + students_per_classroom + school_inse
#            , data=d)
# paste(names(coefficients(m3))[which(coeftest(m3,vcov=vcovHC(m3,type="HC1"))[,4]<0.05)],collapse=" + ")

# Export results
texreg(list(r1,r2,r3,r4),
       file="../../tables/rdd_programmatic.tex", table=F, float.pos="htp", scalebox=.75,
       digits=3, booktabs = T, dcolumn=T,use.packages=F,single.row = F,custom.note="",
       override.pvalues = list(coeftest(r1,vcov=vcovHC(r1,type="HC1"))[,4],coeftest(r2,vcov=vcovHC(r2,type="HC1"))[,4],coeftest(r3,vcov=vcovHC(r3,type="HC1"))[,4],coeftest(r4,vcov=vcovHC(r4,type="HC1"))[,4]),
       override.se = list(coeftest(r1,vcov=vcovHC(r1,type="HC1"))[,2],coeftest(r2,vcov=vcovHC(r2,type="HC1"))[,2],coeftest(r3,vcov=vcovHC(r3,type="HC1"))[,2],coeftest(r4,vcov=vcovHC(r4,type="HC1"))[,2]),
       custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
       custom.coef.map = list("treated" = "$\\hat \\beta_1$: Quality target met", "treated:director_appointed" = "$\\hat \\gamma_2$: Quality target met $\\times$ Appointed"),
       custom.gof.names = c("N"),
       include.adjrs=FALSE, include.rsq=F, include.n=T,include.groups=F,
       stars = c(0.05, 0.01, 0.001),
       custom.gof.rows = list("$\\hat \\beta_1 + \\hat \\gamma_2$" = c("",paste0(format(round(lh2$test$coef[[1]],3),nsmall=3),ifelse(lh2$test$pvalues[[1]]<0.001,"$^{***}$",ifelse(lh2$test$pvalues[[1]]<0.01,"$^{**}$",ifelse(lh2$test$pvalues[[1]]<0.05,"$^{*}$",""))),sep=""),
                                                                       paste0(format(round(lh3$test$coef[[1]],3),nsmall=3),ifelse(lh3$test$pvalues[[1]]<0.001,"$^{***}$",ifelse(lh3$test$pvalues[[1]]<0.01,"$^{**}$",ifelse(lh3$test$pvalues[[1]]<0.05,"$^{*}$",""))),sep=""),
                                                                       paste0(format(round(lh4$test$coef[[1]],3),nsmall=3),ifelse(lh4$test$pvalues[[1]]<0.001,"$^{***}$",ifelse(lh4$test$pvalues[[1]]<0.01,"$^{**}$",ifelse(lh4$test$pvalues[[1]]<0.05,"$^{*}$",""))),sep="")),
                              " " = c("", paste0("(",round(lh2$test$sigma[[1]],3),")",sep=""),paste0("(",round(lh3$test$sigma[[1]],3),")",sep=""),paste0("(",round(lh4$test$sigma[[1]],3),")",sep="")),
                              "State fixed effects" = c("","",rep("$\\checkmark$",2)),
                              "Predictors of Appointed" = c(rep("",3),"$\\checkmark$"),
                              "Bandwidth" = c(bw_noint, rep(bw,3))))


# NOTES -- R version, platform, and loaded packages -------------------------

### sessionInfo(package = NULL)

# R version 3.6.3 (2020-02-29)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS  10.16
# 
# Matrix products: default
# LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib
# 
# Random number generation:
#   RNG:     Mersenne-Twister 
# Normal:  Inversion 
# Sample:  Rounding 
# 
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] mgcv_1.8-38     nlme_3.1-145    Hmisc_4.4-0     lattice_0.20-40 texreg_1.37.1   xtable_1.8-4   
# [7] multcomp_1.4-13 TH.data_1.0-10  MASS_7.3-51.5   mvtnorm_1.1-1   lfe_2.8-5       Matrix_1.2-18  
# [13] rdd_0.57        Formula_1.2-3   AER_1.2-9       survival_3.1-11 car_3.0-7       carData_3.0-3  
# [19] lmtest_0.9-37   zoo_1.8-7       sandwich_2.5-1  rdrobust_0.99.7 codebook_0.9.2  readxl_1.3.1   
# [25] here_1.0.1      forcats_0.5.0   stringr_1.4.0   dplyr_1.0.0     purrr_0.3.3     readr_1.4.0    
# [31] tidyr_1.0.2     tibble_3.0.0    ggplot2_3.3.5   tidyverse_1.3.0
# 
# loaded via a namespace (and not attached):
#   [1] fs_1.4.1            lubridate_1.7.4     RColorBrewer_1.1-2  httr_1.4.1          rprojroot_2.0.2    
# [6] tools_3.6.3         backports_1.1.5     utf8_1.1.4          R6_2.4.1            rpart_4.1-15       
# [11] DBI_1.1.0           colorspace_1.4-1    nnet_7.3-13         withr_2.1.2         tidyselect_1.1.0   
# [16] gridExtra_2.3       curl_4.3            compiler_3.6.3      cli_2.3.0           rvest_0.3.5        
# [21] htmlTable_1.13.3    xml2_1.3.0          labeling_0.3        checkmate_2.0.0     scales_1.1.0       
# [26] digest_0.6.25       foreign_0.8-76      rio_0.5.16          htmltools_0.4.0     base64enc_0.1-3    
# [31] jpeg_0.1-8.1        pkgconfig_2.0.3     labelled_2.5.0      dbplyr_1.4.2        htmlwidgets_1.5.1  
# [36] rlang_1.0.0         rstudioapi_0.11     farver_2.0.3        generics_0.0.2      jsonlite_1.6.1     
# [41] acepack_1.4.1       zip_2.0.4           magrittr_1.5        Rcpp_1.0.7          munsell_0.5.0      
# [46] fansi_0.4.1         abind_1.4-5         lifecycle_0.2.0     stringi_1.4.6       yaml_2.2.1         
# [51] grid_3.6.3          parallel_3.6.3      crayon_1.3.4        haven_2.3.1         splines_3.6.3      
# [56] hms_0.5.3           knitr_1.28          pillar_1.4.3        codetools_0.2-16    reprex_0.3.0       
# [61] glue_1.3.2          latticeExtra_0.6-29 data.table_1.12.8   modelr_0.1.6        vctrs_0.3.1        
# [66] png_0.1-7           cellranger_1.1.0    gtable_0.3.0        assertthat_0.2.1    xfun_0.20          
# [71] openxlsx_4.1.4      broom_0.5.5         cluster_2.1.0       ellipsis_0.3.0 