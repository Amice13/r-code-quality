#########################################
###### Replication File for Analyses in "Going Nativist: HoW Nativism and Economic Ideology Interact to Shape Beliefs about Global Trade"
###### Load the coded data to ensure correct variable names
###### For measurement model, see PRS_Table1, implemented in MPlus 
###### For variable coding, see "PRS Replication File Variable Coding_US.do" and "PRS Replication File Variable_Coding_UK.do"
###### 23 November 2020
##################################

library(foreign)
library(ggplot2)
library(lattice)
library(psych)
library(stargazer)
library(arm)
library(survey)

#Set your working directory
setwd()

#Read in the coded data
data.us <- read.dta("PRS_replication_data_US.1.dta")
data.uk <- read.dta("PRS_replication_data_UK.1.dta")

#To create a log of output:
#sink(file = "./PRS_Routputmain.txt", append=T)


# Renaming/Rescaling variables --------------------------------------------
rescale <- function(x){
  return((x-min(x,na.rm=TRUE))/(max(x-min(x,na.rm=TRUE),na.rm=TRUE)))
}

#US
data.us$globtr1 <- rescale(data.us$globtr)
data.us$bustr1 <- rescale(data.us$bustr)
data.us$natism1 <- rescale(data.us$natism)
data.us$ideodk1 <- rescale(data.us$ideodk)
data.us$univ1 <- rescale(data.us$univ)
data.us$supptrade1 <- rescale(data.us$supptrade)
data.us$rightwing1 <- rescale(data.us$w2wing1a)
data.us$conform1 <-rescale(data.us$conform)
data.us$w1fremkt1r1 <- rescale(data.us$w1fremkt1r)
data.us$w1regmkt1r1 <- rescale(data.us$w1regmkt1r)
data.us$inc2nd <- data.us$inc3050
data.us$inc3rd <- data.us$inc5080
data.us$inc4th <- data.us$inc80up

#UK
data.uk$globtr1 <- rescale(data.uk$globaltr)
data.uk$bustr1 <- rescale(data.uk$businesstr)
data.uk$natism1 <- rescale(data.uk$natism)
data.uk$univ1 <- rescale(data.uk$univ)
data.uk$supptrade1 <- rescale(data.uk$supptrade)
data.uk$rightwing1 <- rescale(data.uk$w2wing1a)
data.uk$conform1 <- rescale(data.uk$conform)
data.uk$w1fremkt1r1 <- rescale(data.uk$w1fremkt1r)
data.uk$w1regmkt1r1 <- rescale(data.uk$w1regmkt1r)
data.uk$inc2nd <- data.uk$inc2035
data.uk$inc3rd <- data.uk$inc3560
data.uk$inc4th <- data.uk$inc60up

# ##########Table 2: Nativism and Beliefs about Global Trade --------------

####Models 1-6: Global Benefits 
#Model 1: No interaction, US
m.glob.us.0 <- lm(globtr1 ~ natism1 + w1fremkt1r1, data=data.us, weights=weight_w2)
summary(m.glob.us.0)
#Model 4: No interaction, UK
m.glob.uk.0 <- lm(globtr1 ~ natism1 + w1fremkt1r1, data=data.uk, weights=W8wave2)
summary(m.glob.uk.0)

#Model 2: Nativism, Freek Market, and Interaction, US
m.glob.us.1 <- lm(globtr1 ~ natism1 + w1fremkt1r1 + w1fremkt1r1*natism1, data=data.us, weights=weight_w2)
summary(m.glob.us.1)
#Model 5: Nativism, Freek Market, and Interaction, UK
m.glob.uk.1 <- lm(globtr1 ~ natism1 + w1fremkt1r1 + w1fremkt1r1*natism1, data=data.uk, weights=W8wave2)
summary(m.glob.uk.1)

#Model 3: Adding theoretically relevant controls, demographics, US
m.glob.us.2 <- lm(globtr1 ~ natism1 + w1fremkt1r1 + natism1*w1fremkt1r1 +
                    econsentimentw2 + w1regmkt1r1 + rightwing1 + univ1 + conform1 +
                    white + male + age + university + 
                    workfull + workpart + workstudent + workretired + workother, data=data.us, weights=weight_w2)
summary(m.glob.us.2)
#Model 6: Adding theoretically relevant controls, demographics, UK
m.glob.uk.2 <- lm(globtr1 ~ natism1 + w1fremkt1r1 + natism1*w1fremkt1r1 +
                    econsentimentw2 + w1regmkt1r1 + rightwing1 + univ1 + conform1 +
                    white + male + age + university + 
                    workfull + workpart + workstudent + workretired + workother, data=data.uk, weight=W8wave2)
summary(m.glob.uk.2)

###Models 7-12: National Benefits
#Model 7: No interaction, US
m.econ.us.0 <- lm(bustr1 ~ natism1 + w1fremkt1r1, data=data.us, weights=weight_w2)
summary(m.econ.us.0)
#Model 10: No interaction, UK
m.econ.uk.0 <- lm(bustr1 ~ natism1 + w1fremkt1r1, data=data.uk, weights=W8wave2)
summary(m.econ.uk.0)

#Model 8: Nativism, Freek Market, and Interaction, US
m.econ.us.1 <- lm(bustr1 ~ natism1 + w1fremkt1r1 + w1fremkt1r1*natism1, data=data.us, weights=weight_w2)
summary(m.econ.us.1)
#Model 11: Nativism, Freek Market, and Interaction, UK
m.econ.uk.1 <- lm(bustr1 ~ natism1 + w1fremkt1r1 + w1fremkt1r1*natism1, data=data.uk, weights=W8wave2)
summary(m.econ.uk.1)

#Model 9: Adding theoretically relevant controls, demographics, US
m.econ.us.2 <- lm(bustr1 ~ natism1 + w1fremkt1r1 + natism1*w1fremkt1r1 +
                    econsentimentw2 + w1regmkt1r1 + rightwing1 + univ1 + conform1 +
                    white + male + age + university + #inc2nd + inc3rd + inc4th + 
                    workfull + workpart + workstudent + workretired + workother, 
                  data=data.us, weights=weight_w2)
summary(m.econ.us.2)

#Model 12: Adding theoretically relevant controls, demographics, US
m.econ.uk.2 <- lm(bustr1 ~ natism1 + w1fremkt1r1 + natism1*w1fremkt1r1 +
                    econsentimentw2 + w1regmkt1r1 + rightwing1 + univ1 + conform1 +
                    white + male + age + university + 
                    workfull + workpart + workstudent + workretired + workother, 
                  data=data.uk, weights=W8wave2)
summary(m.econ.uk.2)

#Generates Table 2:
stargazer(m.glob.us.0, m.glob.us.1, m.glob.us.2, m.glob.uk.0, m.glob.uk.1, m.glob.uk.2, m.econ.us.0, m.econ.us.1, m.econ.us.2, m.econ.uk.0, m.econ.uk.1, m.econ.uk.2, title="Nativism and Beliefs about Global Trade", 
          omit.stat=c("LL", "ser", "f"), style="apsr", digits=2, label="tab:newecongood",
          covariate.labels=c("Nativism", "Free Market", "Economic Perceptions", "Govt. Regulation Good", 
                             "Right Wing", "Universalism", "Conformity", "White", "Male", "Age",
                             "University", "Full Time", "Part Time", "Student", "Retired", "Other", "Nativism x Free Market"), 
          star.cutoffs=c(0.05, 0.01))


#"The substantive effect of a 1-unit change in nativism on beliefs about global benefits is consistently large compared to a 1-unity change in belief in the free market --- 6 times larger in the U.S. sample and 1.8 times in the U.K."
round(abs(m.glob.us.0$coefficients[2]/m.glob.us.0$coefficients[3]), digits=1) #5.6
round(abs(m.glob.uk.0$coefficients[2]/m.glob.uk.0$coefficients[3]), digits=1) #1.8


# ###Figure 1: Beliefs about Global Trade in the U.S. and U.K. ------------
#Load required package: interflex v 1.0.8
#Install and load dependencies for interflex
library(sandwich)
library(lmtest)
library(pcse)
library(doParallel)
library(foreach)
library(Lmoments)
library(lfe)
library(RcppArmadillo)

#Plots produced with interflex version 1.0.8; new versions introduce different syntax; run below if you do not have interflex 1.0.8 installed
#library(devtools)
#install_version("interflex", version = "1.0.8", repos = "http://cran.us.r-project.org")
library(interflex)

#Panel a: The marginal effect of free market beliefs on beliefs about globalization's global benefits decreases with nativism, U.S. 
##First, ue the following to generate data frames without NAs; interflex introduces errors with NAs on the weighting factor
Y <- c("globtr1", "bustr1")
D <- "w1fremkt1r1"
X <- "natism1"
Z <- c("econsentimentw2", "w1regmkt1r1", "rightwing1", 
       "univ1", "conform1", "white", "male", "age", "university", 
       "workfull", "workpart", "workstudent", "workretired", "workother")
weights <- "weight_w2"

data.us.weightna <- na.omit(data.us[, c(Y, D, X, Z, weights)])

Y2 <-  c("globtr1", "bustr1")
D2 <- "w1fremkt1r1"
X2 <- "natism1"
Z2 <- c("econsentimentw2", "w1regmkt1r1", "rightwing1", 
       "univ1", "conform1", "white", "male", "age", "university", 
       "workfull", "workpart", "workstudent", "workretired", "workother")
weights2 <- "W8wave2"
data.uk.weightna <- na.omit(data.uk[, c(Y2, D2, X2, Z2, weights2)])


#Kernel estimation for interaction
kern.natmkt.us.na <- inter.kernel(Y="globtr1", D="w1fremkt1r1", X="natism1", 
                                 Z=c("econsentimentw2", "w1regmkt1r1", "rightwing1", 
                                     "univ1", "conform1", "white", "male", "age",
                                     "university", "workfull", "workpart", "workstudent", 
                                     "workretired", "workother"),  #treat.type = "continuous",
                                 data=data.us.weightna, na.rm=FALSE, weights="weight_w2",
                                 Dlabel= "Free Market", Xlabel="Nativism", Ylabel="Globalization Good",
                                 xlim=c(0,1), ylim=c(-0.3, 0.4), main="United States", neval=100,
                                 seed=43215, nboots=1000, parallel=TRUE)

#Formatting the plot
dev.new(height=7,width=8)
natmkt.us.na.g <- kern.natmkt.us.na$graph
natmkt.us.na.g2 <- natmkt.us.na.g + scale_x_continuous(name="Nativism") + ggtitle("a) United States") +
  scale_y_continuous(name="Marginal Effect of Free Market \n on Global Benefits") + geom_hline(yintercept=0, linetype="dashed", color = "red") +
  theme_bw() + theme(plot.title=element_text(size=18, hjust=0.5), axis.title=element_text(size=14))
natmkt.us.na.g2

#Panel b: The marginal effect of free market beliefs on beliefs about globalization's global benefits decreases with nativism, U.K. 
#Kernel estimation for interaction
kern.natfree.uk.na <- inter.kernel(Y="globtr1", D="w1fremkt1r1", X="natism1", 
                                Z=c("econsentimentw2", "w1regmkt1r1", "rightwing1", 
                                    "univ1", "conform1", "white", "male", "age",
                                    "university", "workfull", "workpart", "workstudent", 
                                    "workretired", "workother"), 
                                data=data.uk.weightna, na.rm=FALSE, weights="W8wave2",
                                Dlabel= "Free Market", Xlabel="Nativism", Ylabel="Globalization Good", 
                                xlim=c(0,1), ylim=c(-0.3, 0.4), main="United Kingdom", neval=100,
                                seed=43215, nboots=1000, parallel = TRUE)

#Formatting the plot
natmkt.uk.na.g <- kern.natfree.uk.na$graph
natmkt.uk.na.g2 <- natmkt.uk.na.g + scale_x_continuous(name="Nativism") + ggtitle("b) United Kingdom") +
  scale_y_continuous(name="") + geom_hline(yintercept=0, linetype="dashed", color = "red") +
  theme_bw() + theme(plot.title=element_text(size=18, hjust=0.5), axis.title=element_text(size=16))
natmkt.uk.na.g2

#Panel c: The marginal effect of free market beliefs on beliefs about globalization's national benefits decreases with nativism, U.S. 
kern.econnatmkt.us.na <- inter.kernel(Y="bustr1", D="w1fremkt1r1", X="natism1", 
                                      Z=c("econsentimentw2", "w1regmkt1r1", "rightwing1", 
                                          "univ1", "conform1", "white", "male", "age",
                                          "university", "workfull", "workpart", "workstudent", 
                                          "workretired", "workother"),
                                      data=data.us.weightna, na.rm=FALSE, weights="weight_w2",
                                      Dlabel= "Free Market", Xlabel="Nativism", Ylabel="National Benefits",
                                      xlim=c(0,1), ylim=c(-0.3, 0.4), main="United States", neval=100,
                                      seed=43215, nboots=1000, parallel=TRUE)
#Formatting the plot
econnatmkt.us.na.g <- kern.econnatmkt.us.na$graph
econnatmkt.us.na.g2 <- econnatmkt.us.na.g + scale_x_continuous(name="Nativism") + ggtitle("c) United States") +
  scale_y_continuous(name="Marginal Effect of Free Market \n on National Benefits") + geom_hline(yintercept=0, linetype="dashed", color = "red") +
  theme_bw() + theme(plot.title=element_text(size=18, hjust=0.5), axis.title=element_text(size=14))
econnatmkt.us.na.g2

#Panel d: The marginal effect of free market beliefs on beliefs about globalization's national benefits decreases with nativism, U.K. 
kern.econnatfree.uk <- inter.kernel(Y="bustr1", D="w1fremkt1r1", X="natism1", 
                                    Z=c("econsentimentw2", "w1regmkt1r1", "rightwing1", 
                                        "univ1", "conform1", "white", "male", "age",
                                        "university", #"inc2nd", "inc3rd", "inc4th",
                                        "workfull", "workpart", "workstudent", 
                                        "workretired", "workother"), 
                                    data=data.uk.weightna, na.rm=FALSE, weights="W8wave2",
                                    Dlabel= "Free Market", Xlabel="Nativism", Ylabel="National Benefits", 
                                    xlim=c(0,1), ylim=c(-0.3, 0.4), main="United Kingdom", neval=100,
                                    seed=43215, nboots=1000, parallel = TRUE)

econnatmkt.uk.na.g <- kern.econnatfree.uk$graph
econnatmkt.uk.na.g2 <- econnatmkt.uk.na.g + scale_x_continuous(name="Nativism") + ggtitle("d) United Kingdom") +
  scale_y_continuous(name="") + geom_hline(yintercept=0, linetype="dashed", color = "red") +
  theme_bw() + theme(plot.title=element_text(size=18, hjust=0.5), axis.title=element_text(size=16))
econnatmkt.uk.na.g2

###Combine plots for Figure 1:
library(gridExtra)
dev.new(height=7,width=8)
grid.arrange(natmkt.us.na.g2, natmkt.uk.na.g2, econnatmkt.us.na.g2, econnatmkt.uk.na.g2, nrow=2)

#"Panel a shows that in the U.S., the estimated effect of belief in the free market on beliefs that globalized trade has global benefits approaches 0 when nativism reaches 0.55 (the 70th percentile)."
kern.natmkt.us.na$est[55,] #CI contains 0 at X=0.55
quantile(data.us$natism1, c(0.5, 0.7, 0.75), na.rm=TRUE) #70th percentile = 0.56250




# ### Figure 2: Nativism and the Limits of Economic Messages ------------------------

library(plyr)
#Function to plot the means with 95% confidence intervals
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

#Summarize the data
datasupp.us <- summarySE(data.us, measurevar="supptrade1", groupvars="tradeinfohi")
datasupp.us <- na.omit(datasupp.us)

datasupp.uk <- summarySE(data.uk, measurevar="supptrade1", groupvars="tradeinfohi")
datasupp.uk <- na.omit(datasupp.uk)

#Combine US & UK 
dusuk <- rbind(datasupp.us, datasupp.uk)

#Figure 2: Plot Means and 95% confidence intervals
dev.new(height=7,width=12)
ggplot(dusuk, aes(y=supptrade1)) + 
  geom_point(aes(x=c(0.9, 1.0, 1.1, 1.2), y=supptrade1), size=3.3, shape=c(16,16,17,17)) + 
  geom_segment(size=1.2, aes(x=c(0.9, 1.0, 1.1, 1.2), y=supptrade1-ci, xend=c(0.9, 1.0, 1.1, 1.2), yend=supptrade1+ci)) +
  geom_hline(aes(yintercept=0.5), colour="gray88") +
  scale_x_continuous(name="", limits=c(.85, 1.25), breaks=c(0.9, 1.0, 1.1, 1.2),
                     labels=c("US \n Control", "US \n Treatment", "UK \n Control", "UK \n Treatment")) +
  scale_y_continuous( name="Support for Free Trade") + coord_cartesian(ylim=c(0.2,.8)) + 
  theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), text=element_text(size=18, colour="black"),
                     axis.text.x = element_text(color="black"), axis.text.y=element_text(color="black"))

#Difference in means
datasupp.us[2,3] - datasupp.us[1,3] #Difference = 0.44
t.test(supptrade1 ~ tradeinfohi, data=data.us) # t= -4.49
datasupp.uk[2,3] - datasupp.uk[1,3] #Difference = 0.44
t.test(supptrade1 ~ tradeinfohi, data=data.uk) # t=-4.81

#Appendix Table 9 Models 1 and 2: Does nativism moderate the effect of the economic message?
mod.nat.1.us <- lm(supptrade1 ~ tradeinfohi + natism1 + tradeinfohi*natism1, data=data.us)
summary(mod.nat.1.us)
mod.nat.1.uk <- lm(supptrade1 ~ tradeinfohi + natism1 + tradeinfohi*natism1, data=data.uk)
summary(mod.nat.1.uk)
mod.nat.1.us$coefficients[3] #nativism b=-0.24
mod.nat.1.uk$coefficients[3] #nativism b=-0.26
mod.nat.1.us$coefficients[4] #interaction b=-0.89; p = 0.052
mod.nat.1.uk$coefficients[4] #interaction b=0.029, p = 0.461


# ###Figure 3 -------------------------------------------------------------
#Load interflex v 1.0.8 (implemented above)
#Panel a: US
exp.kern.nat.us <- inter.kernel(Y="supptrade1", D="tradeinfohi", X="natism1", data=data.us, na.rm=TRUE,
                                Dlabel="Treatment", Ylabel="Support for Free Trade", Xlabel="Nativism",
                                ylim=c(-0.10, 0.20), seed=43215, nboots=1000)

exp.kern.nat.us$graph

exp.kern.nat.us.p <- exp.kern.nat.us$graph + scale_x_continuous(name="Nativism") + ggtitle("a) United States") +
  scale_y_continuous(name="Marginal Effect of Treatment") + geom_hline(yintercept=0, linetype="dashed", color = "red") +
  theme_bw() + theme(plot.title=element_text(size=20, hjust=0.5), axis.title=element_text(size=16))

#Panel b: UK
exp.kern.nat.uk <- inter.kernel(Y="supptrade1", D="tradeinfohi", X="natism1", data=data.uk, na.rm=TRUE,
                            Dlabel="Treatment", Ylabel="Support for Free Trade", Xlabel="Nativism",
                            ylim=c(-0.10, 0.20), seed=43215, nboots=1000)
exp.kern.nat.uk$graph
exp.kern.nat.uk.p <- exp.kern.nat.uk$graph + scale_x_continuous(name="Nativism") + ggtitle("b) United Kingdom") +
  scale_y_continuous(name="") + geom_hline(yintercept=0, linetype="dashed", color = "red") +
  theme_bw() + theme(plot.title=element_text(size=20, hjust=0.5), axis.title=element_text(size=16))

dev.new(height=7,width=8)
#Combine for Figure 3
grid.arrange(exp.kern.nat.us.p, exp.kern.nat.uk.p, nrow=1)

#Post hoc analysis: Nativism terciles in US
#Marginal effects at low, medium, and high levels of nativism
quantile(data.us$natism1, c(0.33, 0.66), na.rm=TRUE)
data.us$natlow <- ifelse(data.us$natism1 < 0.40625, 1, 0) 
data.us$nathi <- ifelse(data.us$natism1 >= 0.53125, 1, 0)
data.us$natmid <- ifelse(data.us$natism1 >= 0.40625 & data.us$natism1 < 0.53125, 1, 0)

mod.terciles <- lm(supptrade1 ~ natlow + natmid + tradeinfohi + natlow*tradeinfohi + natmid*tradeinfohi, data=data.us)
summary(mod.terciles)
mod.terciles$coefficients[5] # Low nativism x treatment interaction b=0.054, p=0.019


#Uncomment if you are logging output
#sink()

