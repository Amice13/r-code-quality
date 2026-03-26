
#####################################################################################
## A Gaussian process emulator of complex HIV and non-communicable disease models: ##
## A tutorial using Kenyan data and reproducible R code                            ##
#####################################################################################

# This code forms the basis for the emulator in the article: 
#
# Sawe S, Mugo M, Wilson-Barthes M, Osetinsky B, Chrysanthopolou S, Yego F, Mwangi A, Galárraga O.
# A Gaussian process emulator of complex HIV and non-communicable disease models: A tutorial using 
# Kenyan data and reproducible R code. Stat Med. 2022 (under review).
#
# Please cite the article when using this code
# 
# See the following Brown University Digital Repository for more information or code updates
# XX
#
# To program this tutorial we made use of 
# R: Version 4.1.2 (November, 2021) https://cran.r-project.org/bin/windows/base/old/4.1.2/


######################################################################################
################# Code used to run the Gaussian process emulator #####################
######################################################################################

# remove any variables in R's memory 
rm(list = ls())   

# Removing the graphics in memory
graphics.off()


# install relevant r packages needed to run the emulator
library(ggplot2)
library(GauPro)
library(bayesplot)
library(rstanarm)

################# Enter prevalence estimates and forecast period for HIV #####################

# Define the emulation period / number of years on the x-axis 
x <- seq(2018,2028,l=11)              # in this tutorial, start year for the inMODELA simulation model was 2018
                                      # in this tutorial, end year for the inMODELA simulation model was 2028
                                      # in this tutorial, number of years forecasted = 11 years

# Define the prevalence estimates (based on simulation model(s) and/or the literature) on the y-axis 
y <- seq(0.0481,0.0255, l=11)         # in this tutorial, HIV prevalence estimated from the inMODELA simulation model in 2018 was 4.81%
                                      # in this tutorial, HIV prevalence estimated from the inMODELA simulation model in 2023 was 2.55%
                                      # in this tutorial, number of years forecasted = 11 years

# Look at the trend(s) in HIV prevalence over the forecast time period
plot(x, y,
     ylim=c(0,0.05), xlab="years", ylab= "HIV Prevalence")


plot(x, y,
     xlab="years", ylab= "HIV Prevalence")
# Look at how A linear model (LM) would fit the simulation data.
lm_mod <- lm(y ~ x)
plot(x, y, xlab="years", ylab= "HIV Prevalence")
abline(a=lm_mod$coef[1], b=lm_mod$coef[2], col='red')    #The LM is insufficient for describing the underlying function producing the data.


# Instead of a linear model, use a Guassian process (GP) model that assumes that the distribution of points follows a multivariate distribution.
# This emulator uses the GauPro R Package: https://cran.r-project.org/web/packages/GauPro/index.html
# A guide to the Gaupro package can be found here: https://cran.r-project.org/web/packages/GauPro/vignettes/GauPro.html

# inMODELA simulator HIV Prevalence confidence intervals
l = c(0.0419,0.0395,0.0373,0.0351,0.0330,0.0309,0.0290,0.0271,0.0254,0.0238,0.0220)  #For each annual prevalence estimate, define the lower bound of the 95% Credibility Interval from the simulation estimates
u = c(0.0543,0.0512,0.0483,0.0455,0.0430,0.0402,0.0378,0.0354,0.0333,0.0312,0.0290)  #For each annual prevalence estimate, define the upper bound of the 95% Credibility Interval from the simulation estimates
                                                                                     # For example, the simulated prevalence of HIV in 2018 was mean=04.81, 95% CI= 04.19, 05.43. 

library(GauPro)
gp <- GauPro(x,y, parallel=FALSE)
gp
# Look at how the GP model fits the simulation data.
plot(x,y, xlab="years", ylab= "HIV Prevalence", main= "Projected prevalence of HIV",type="b", lwd=2)
curve(gp$predict(x), add=T, col=2)
curve(gp$predict(x)+1.96*gp$predict(x, se=T)$se, add=T, col=4) #Blue lines visually depict the CIs for emulator's estimates.
curve(gp$predict(x)-1.96*gp$predict(x, se=T)$se, add=T, col=4)                      
lines(x,l,col="yellow",lwd=4)                                  #Yellow lines visually depict the CIs for simulator's estimates.
lines(x,u,col="yellow",lwd=4)

# Abstract the confidence intervals for each annual prevalence estimate forecasted by the emulator.
set.seed(1234)                                                 # Create a dataframe using the emulator's annual prevalence predictions.
new_x <- seq(min(x), max(x), length.out = 11)                  # 11 observations = ll years in the forecast.
predict_df11       <- predict(gp, new_x, se.fit = TRUE)
predict_df11$x     <- new_x
predict_df11$y     <- predict_df11$mean
predict_df11$lower <- predict_df11$y - 1.96 * predict_df11$se
predict_df11$upper <- predict_df11$y + 1.96 * predict_df11$se
View(predict_df11)

# Abstract the confidence intervals for the mean annual prevalence between each point estimate forecasted by the emulator.
set.seed(1234)                                                 # Create a dataframe using the emulator's annual prevalence predictions.
new_x <- seq(min(x), max(x), length.out = 21)                  # 21 observations = ll annual point estimates + 10 mean estimates between each year. 
predict_df21       <- predict(gp, new_x, se.fit = TRUE)
predict_df21$x     <- new_x
predict_df21$y     <- predict_df21$mean
predict_df21$lower <- predict_df21$y - 1.96 * predict_df21$se
predict_df21$upper <- predict_df21$y + 1.96 * predict_df21$se
View(predict_df21)


################# Enter prevalence estimates and forecast period for Hypertension (HTN) #####################

# Define the emulation period / number of years on the x-axis 
x <- seq(2018,2028,l=11)              # in this tutorial, start year for the inMODELA simulation model was 2018
                                      # in this tutorial, end year for the inMODELA simulation model was 2028
                                      # in this tutorial, number of years forecasted = 11 years

# Define the prevalence estimates (based on simulation model(s) and/or the literature) on the y-axis 
y <- seq(0.2947,0.3430, l=11)         # in this tutorial, HTN prevalence estimated from the inMODELA simulation model in 2018 was 29.47%
                                      # in this tutorial, HIV prevalence estimated from the inMODELA simulation model in 2028 was 34.30%
                                      # in this tutorial, number of years forecasted = 11 years

# Look at the trend(s) in HTN prevalence over the forecast time period
plot(x, y,
     ylim=c(0,0.4), xlab="years", ylab= "Hypertension Prevalence")

# Look at how A linear model (LM) would fit the simulation data.
lm_mod <- lm(y ~ x)
plot(x, y, xlab="years", ylab= "Hypertension Prevalence")
abline(a=lm_mod$coef[1], b=lm_mod$coef[2], col='red')  #The LM is insufficient for describing the underlying function producing the data.

# Instead of a linear model, use a Guassian process (GP) model that assumes that the distribution of points follows a multivariate distribution.
# This emulator uses the GauPro R Package: https://cran.r-project.org/web/packages/GauPro/index.html

# inMODELA simulator Hypertension Prevalence confidence intervals
l = c(0.2845,0.2908,0.2965,0.3017,0.3073,0.3125,0.3171,0.3213,0.3250,0.3284,0.3313)  #For each annual prevalence estimate, define the lower bound of the 95% Credibility Interval from the simulation estimates
u = c(0.3050,0.3116,0.3177,0.3232,0.3292,0.3347,0.3396,0.3441,0.3480,0.3516,0.3547)  #For each annual prevalence estimate, define the upper bound of the 95% Credibility Interval from the simulation estimates

library(GauPro)
gp <- GauPro(x,y, parallel=FALSE)
gp
# Look at how the GP model fits the simulation data.
plot(x,y, xlab="years", ylab= "Hypertension Prevalence", main= "Projected prevalence of Hypertension",type="b", lwd=2)
curve(gp$predict(x), add=T, col=2)
curve(gp$predict(x)+1.96*gp$predict(x, se=T)$se, add=T, col=4)                       #Blue lines visually depict the CIs for emulator's estimates.
curve(gp$predict(x)-1.96*gp$predict(x, se=T)$se, add=T, col=4)
lines(x,l,col="yellow",lwd=4)                                                        #Yellow lines visually depict the CIs for simulator's estimates.
lines(x,u,col="yellow",lwd=4)


################# Enter prevalence estimates and forecast period for comorbid HIV and Hypertension (HIV+HTN) #####################

# Define the emulation period / number of years on the x-axis 
x <- seq(2018,2028,l=11)              # in this tutorial, start year for the inMODELA simulation model was 2018
                                      # in this tutorial, end year for the inMODELA simulation model was 2028
                                      # in this tutorial, number of years forecasted = 11 years

# Define the prevalence estimates (based on simulation model(s) and/or the literature) on the y-axis 
y <- seq(0.0206,0.0131, l=11)         # in this tutorial, HIV+HTN prevalence estimated from the inMODELA simulation model in 2018 was 2.06%
                                      # in this tutorial, HIV prevalence estimated from the inMODELA simulation model in 2028 was 1.31%
                                      # in this tutorial, number of years forecasted = 11 years

# Look at the trend(s) in HIV+HTN prevalence over the forecast time period
plot(x, y,
     ylim=c(0,0.2), xlab="years", ylab= "HIV & Hypertension Prevalence")

# Look at how A linear model (LM) would fit the simulation data.
lm_mod <- lm(y ~ x)
plot(x, y, xlab="years", ylab= "HIV & Hypertension Prevalence")
abline(a=lm_mod$coef[1], b=lm_mod$coef[2], col='red')  #The LM is insufficient for describing the underlying function producing the data.

# Instead of a linear model, use a Guassian process (GP) model that assumes that the distribution of points follows a multivariate distribution.
# This emulator uses the GauPro R Package: https://cran.r-project.org/web/packages/GauPro/index.html


# inMODELA simulator HIV+HTN Prevalence confidence intervals
l = c(0.0179,0.0173,0.0164,0.0158,0.0151,0.0144,0.0138,0.0132,0.0126,0.0120,0.0114)  #For each annual prevalence estimate, define the lower bound of the 95% Credibility Interval from the simulation estimates
u = c(0.0232,0.0224,0.0212,0.0203,0.0195,0.0186,0.0179,0.0170,0.0163,0.0156,0.0148)  #For each annual prevalence estimate, define the upper bound of the 95% Credibility Interval from the simulation estimates

library(GauPro)
gp <- GauPro(x,y, parallel=FALSE)
gp
# Look at how the GP model fits the simulation data.
plot(x,y, xlab="years", ylab= "HIV & Hypertension Prevalence", main= "Projected prevalence of HIV & Hypertension",type="b", lwd=2)#,
curve(gp$predict(x), add=T, col=2)
curve(gp$predict(x)+1.96*gp$predict(x, se=T)$se, add=T, col=4)                      #Blue lines visually depict the CIs for emulator's estimates.
curve(gp$predict(x)-1.96*gp$predict(x, se=T)$se, add=T, col=4)
lines(x,l,col="yellow",lwd=4)                                                       #Yellow lines visually depict the CIs for simulator's estimates.
lines(x,u,col="yellow",lwd=4)

################# Enter prevalence estimates and forecast period for depression among people living with HIV #####################

# Define the emulation period / number of years on the x-axis 
x <- seq(2018,2028,l=11)             # in this tutorial, start year for the Smit et al. simulation model was 2018
                                     # in this tutorial, end year for theSmit et al. simulation model was 2028
                                     # in this tutorial, number of years forecasted = 11 years

# Define the prevalence estimates (based on simulation model(s) and/or the literature) on the y-axis 
y <- seq(0.039,0.037, l=11)          # in this tutorial, depression prevalence estimated from the simulation model in 2018 was 3.9%
                                     # in this tutorial, depression prevalence estimated from the simulation model in 2028 was 3.7%. This was calculated by averaging the prevalence in 2030 and 2025 to maintain consistency with the inMODELA simulator.
                                     # in this tutorial, number of years forecasted = 11 years

# Look at the trend(s) in depression prevalence among PLWH over the forecast time period
plot(x, y,
     ylim=c(0,0.04), xlab="years", ylab= "Depression Prevalence")

# Look at how A linear model (LM) would fit the simulation data.
lm_mod <- lm(y ~ x)
plot(x, y, xlab="years", ylab= "Depression Prevalence")
abline(a=lm_mod$coef[1], b=lm_mod$coef[2], col='red')   #The LM is insufficient for describing the underlying function producing the data.

# Instead of a linear model, use a Guassian process (GP) model that assumes that the distribution of points follows a multivariate distribution.
# This emulator uses the GauPro R Package: https://cran.r-project.org/web/packages/GauPro/index.html

library(GauPro)
gp <- GauPro(x,y, parallel=FALSE)

# Look at how the GP model fits the simulation data.
gp
plot(x,y, xlab="years", ylab= "Depression Prevalence", main= "Projected prevalence of Depression",type="b", lwd=2)#,
curve(gp$predict(x), add=T, col=2)
curve(gp$predict(x)+1.96*gp$predict(x, se=T)$se, add=T, col=4)
curve(gp$predict(x)-1.96*gp$predict(x, se=T)$se, add=T, col=4)

################# Combine GP plots 1 - 4 #####################
line = 1
cex = 1
side = 3
adj=-0.05

tiff("D:/inMODELA TRAINING/CONSULTANCY 1/BMC medical journal manuscript review/Plots/Figure2.tiff", width=12, height=10, units="in", res=300)  # Adjust width, height, and resolution as needed

par(mfrow=c(2,2), oma=c(1,6,1,1), cex.lab=1.5, cex.axis=1.2, cex.sub=1.2, font.lab=2)
### PLOT 1
# HIV
x <- seq(2018,2028, l=11)
y <- seq(0.0481,0.0255, l=11)
# The simulator's confidence intervals
l = c(0.0419,0.0395,0.0373,0.0351,0.0330,0.0309,0.0290,0.0271,0.0254,0.0238,0.0220)
u = c(0.0543,0.0512,0.0483,0.0455,0.0430,0.0402,0.0378,0.0354,0.0333,0.0312,0.0290
)
# Fitting the gaussian process
library(GauPro)
gp <- GauPro(x,y, parallel=FALSE)
# Plotting
# Run the following lines of code together
plot(x,y,xlab="years", ylab= "HIV Prevalence",type="b", lwd=2, pch=16)#,
curve(gp$predict(x), add=T, col=2, lty=5)
curve(gp$predict(x)+1.96*gp$predict(x, se=T)$se, add=T, col=4)
curve(gp$predict(x)-1.96*gp$predict(x, se=T)$se, add=T, col=4)
lines(x,l,col="yellow",lwd=4)
lines(x,u,col="yellow",lwd=4)
mtext("A", side=side, line=line, cex=1.5, adj=adj)

# PLOT 2
# Hypertension
x <- seq(2018,2028, l=11)
y <- seq(0.2947,0.3430, l=11)
# Fitting the gaussian process
# The simulator's confidence intervals
l = c(0.2845,0.2908,0.2965,0.3017,0.3073,0.3125,0.3171,0.3213,0.3250,0.3284,0.3313)
u = c(0.3050,0.3116,0.3177,0.3232,0.3292,0.3347,0.3396,0.3441,0.3480,0.3516,0.3547)
# Fitting the gaussian process
library(GauPro)
gp <- GauPro(x,y, parallel=FALSE)
# Plotting
plot(x,y, xlab="years", ylab= "Hypertension Prevalence",type="b", lwd=2, pch=16)#,
curve(gp$predict(x), add=T, col=2, lty=5)
curve(gp$predict(x)+1.96*gp$predict(x, se=T)$se, add=T, col=4)
curve(gp$predict(x)-1.96*gp$predict(x, se=T)$se, add=T, col=4)
lines(x,l,col="yellow",lwd=4)
lines(x,u,col="yellow",lwd=4)
mtext("B", side=side, line=line, cex=1.5, adj=adj)
# PLOT 3
# Comorbid HIV and hypertension
x <- seq(2018,2028, l=11)
y <- seq(0.0206,0.0131, l=11)
# Fitting the gaussian process
# The simulator's confidence intervals
l = c(0.0179,0.0173,0.0164,0.0158,0.0151,0.0144,0.0138,0.0132,0.0126,0.0120,0.0114)
u = c(0.0232,0.0224,0.0212,0.0203,0.0195,0.0186,0.0179,0.0170,0.0163,0.0156,0.0148)
# Fitting the gaussian process
library(GauPro)
gp <- GauPro(x,y, parallel=FALSE)
# Plotting
plot(x,y, xlab="years", ylab= "HIV & Hypertension Prevalence",type="b", lwd=2, pch=16)#,
curve(gp$predict(x), add=T, col=2, lty=5)
curve(gp$predict(x)+1.96*gp$predict(x, se=T)$se, add=T, col=4)
curve(gp$predict(x)-1.96*gp$predict(x, se=T)$se, add=T, col=4)
lines(x,l,col="yellow",lwd=4)
lines(x,u,col="yellow",lwd=4)
mtext("C", side=side, line=line, cex=1.5, adj=adj)
# PLOT 4
x <- seq(2018,2028, l=11)
y <- seq(0.039,0.037, l=11)
# Fitting the gaussian process
library(GauPro)
gp <- GauPro(x,y, parallel=FALSE)
# Plotting
plot(x,y, xlab="years", ylab= "Depression Prevalence",type="b", lwd=2, pch=16)
curve(gp$predict(x), add=T, col = 2, lty=5)
curve(gp$predict(x)+1.96*gp$predict(x, se=T)$se, add=T, col = 4)
curve(gp$predict(x)-1.96*gp$predict(x, se=T)$se, add=T, col = 4)
mtext("D", side=side, line=line, cex=1.5, adj=adj)

dev.off()  # Close the TIFF device




################# BAYESIAN POSTERIOR PREDICTIVE ANALYSIS #####################
# This step provides posterior predictive distributions that can be used to analyze the credibility of future observable data based on the posterior distribution.
# The procedure is implemented via the Bayesian inference rstanarm package: https://cran.r-project.org/web/packages/bayesplot/vignettes/graphical-ppcs.html
# For this step, all the data points provided by the simulators are used to plot the posterior predictive distribution contrary to the Gaupro step which uses 2 points at start time and end time. 
side=3
line = 1
cex = 1
adj=-0.05

tiff("D:/inMODELA TRAINING/CONSULTANCY 1/BMC medical journal manuscript review/Plots/Figure3.tiff", width=12, height=10, units="in", res=300)  # Adjust width, height, and resolution as needed

par(mfrow=c(2,2), oma=c(1,6,1,1), cex.lab=1.5, cex.axis=1.2, cex.sub=1.2, font.lab=2)

set.seed(12345)

## PLOT 1 - Emulator's posterior predictions of HIV prevalence
x <- c(2018,2019,2020,2021,2022,2023,2024,2025,2026,2027,2028)                         # Enter each annual time point used in emulator's predictions.
y <- c(0.0481,0.0454,0.0428,0.0403,0.0380,0.0355,0.0334,0.0313,0.0294,0.0275,0.0255)   # Enter prevalence estimate for each time point. 

# using rstanarm's default priors. For details see the section on default weakly informative priors at https://mc-stan.org/rstanarm/articles/priors.html
sm1   <- stan_glm (y~x, 
                  family=gaussian(),                                                   # Specify distribution.
                  data = data.frame(x,y),                                             
                  seed=1234)
# Y predicted
ypred <- posterior_predict(sm1, draws = 50)                                            # Specify the number of draws to return (50 at minimum).

# density plot
plot(density(y),col="black",lwd=3, xlab="HIV prevalence", main="")
lines(density(ypred[1,]), col="red", lty=2, lwd=3)
for(i in 1:nrow(ypred))
{lines(density(ypred[i,]),col="grey",lty=3)}
lines(density(ypred[1,]), col="red", lty=2, lwd=3)
mtext("A", side=side, line=line, cex=1.5, adj=adj)



# PLOT 2 - Emulator's posterior predictions of Hypertension prevalence
x <- c(2018,2019,2020,2021,2022,2023,2024,2025,2026,2027,2028)
y <- c(0.2947,0.3012,0.3071,0.3125,0.3182,0.3236,0.3283,0.3327,0.3365,0.3400,0.3430)
# R stanam
sm2   <- stan_glm(y~x, family=gaussian(), data = data.frame(x,y), seed=1234)
# Y predicted
ypred <- posterior_predict(sm2, draws = 50)
# density plot
plot(density(y),col="black", lwd=3, xlab="Hypertension prevalence", main="")
lines(density(ypred[1,]), col="red", lty=2, lwd=3)
for(i in 1:nrow(ypred))
{lines(density(ypred[i,]),col="grey",lty=3)}
#lines(density(colMeans(ypred)), col="red", lty=2, lwd=3)
lines(density(ypred[1,]), col="red", lty=2, lwd=3)
mtext("B", side=side, line=line, cex=1.5, adj=adj)



# PLOT 3 - Emulator's posterior predictions of Comorbid HIV and hypertension
x <- c(2018,2019,2020,2021,2022,2023,2024,2025,2026,2027,2028)
y <- c(0.0206,0.0198,0.0188,0.0181,0.0173,0.0165,0.0158,0.0151,0.0144,0.0138,0.0131)
# R stanam
sm3   <- stan_glm(y~x, family=gaussian(), data = data.frame(x,y), seed=1234)
# Y predicted
ypred <- posterior_predict(sm3, draws = 50)
# density plot
plot(density(y), col="black", lwd=3, xlab="HIV & Hypertension prevalence", main="")
lines(density(ypred[1,]), col="red", lty=2, lwd=3)
for(i in 1:nrow(ypred))
{lines(density(ypred[i,]), col="grey", lty=3)}
lines(density(ypred[1,]), col="red", lty=2, lwd=3)
mtext("C", side=side, line=line, cex=1.5, adj=adj)



# PLOT 4 - Emulator's posterior predictions of Depression among PLWH
x <- c(2018,2020,2025,2030,2035)
y <- c(0.039,0.038,0.037,0.037,0.036)
# R stanam
sm4   <- stan_glm(y~x, family=gaussian(), data = data.frame(x,y), seed=1234)
# Y predicted
ypred <- posterior_predict(sm4, draws = 50)
# density plot
plot(density(y),col="black", lwd=3, xlab="Depression prevalence", main = "")
lines(density(ypred[1,]), col="red", lty=2, lwd=3)
for(i in 1:nrow(ypred))
{lines(density(ypred[i,]),col="grey",lty=3)}
lines(density(ypred[1,]), col="red", lty=2, lwd=3)
mtext("D", side=side, line=line, cex=1.5, adj=adj)

dev.off()  # Close the TIFF device


################# VALIDATE THE EMULATOR'S PREDICTIVE ACCURACY #####################
### Evaluate and validate the emulator's predictive accuracy using the leave-one-out cross validation (LOOCV) technique
### LOOCV is implemented using the loo R package: https://cran.r-project.org/web/packages/loo/vignettes/loo2-large-data.html & https://andrewproctor.github.io/rcourse/module6.html#model_testing

## 1. Validating the accuracy of the HIV emulation model compared to the simulation model

posterior_interval(sm1, prob=0.95)                       # Use sm1 from the above posterior predictive analysis to obtain the 95% credible intervals

plot(sm1)                                                # Graphically display the 95% credible intervals

posterior_vs_prior(sm1)                                  # Juxtaposing the prior and the posterior estimates

pp_check(sm1)                                            # Graphically display the posterior predictive analysis

waic(sm1)                                                # Watanabe-Akaike information criterion

loo(sm1, k_threshold = 0.7)                                                 # Leave One Out Cross-Validation


## 2. Validating the accuracy of the HTN emulation model compared to the simulation model

posterior_interval(sm2, prob=0.95)                       # Use sm2 from the above posterior predictive analysis to obtain the 95% credible intervals

plot(sm2)                                                # Graphically display the 95% credible intervals

posterior_vs_prior(sm2)                                  # Juxtaposing the prior and the posterior estimates

pp_check(sm2)                                            # Graphically display the posterior predictive analysis

waic(sm2)                                                # Watanabe-Akaike information criterion

loo(sm2, k_threshold = 0.7)                                                 # Leave One Out Cross-Validation


## 3. Validating the accuracy of the HIV+HTN emulation model compared to the simulation model

posterior_interval(sm3, prob=0.95)                       # Use sm3 from the above posterior predictive analysis to obtain the 95% credible intervals

plot(sm3)                                                # Graphically display the 95% credible intervals

posterior_vs_prior(sm3)                                  # Juxtaposing the prior and the posterior estimates
 
pp_check(sm3)                                            # Graphically display the posterior predictive analysis

waic(sm3)                                                # Watanabe-Akaike information criterion

loo(sm3)                                                 # This line gives a warning.
                                                         # It is recommended to call 'loo' again with argument 'k_threshold = 0.7' 
                                                         # in order to calculate the ELPD without the assumption that these observations are negligible. 
                                                         # This will refit the model 1 times to compute the ELPDs for the problematic observations directly. 

loo(sm3, k_threshold = 0.7)                              # Leave One Out Cross-Validation
                                                         # In this tutorial, the Pareto k diagnostic is set at 0.7.
                                                         # the Pareto K diagnostic estimates how far an individual leave-one-out distribution is from the full distribution. 
                                                         # If k<0.5, then the corresponding component is estimated with high accuracy. If 0.5<k<0.7 the accuracy is lower, but still ok. 

## 4. Validating the accuracy of the depression emulation model compared to the simulation model

posterior_interval(sm4, prob=0.95)                       # Use sm4 from the above posterior predictive analysis to obtain the 95% credible intervals

plot(sm4)                                                # Graphically display the 95% credible intervals

posterior_vs_prior(sm4)                                  # Juxtaposing the prior and the posterior estimates

pp_check(sm4)                                            # Graphically display the posterior predictive analysis

waic(sm4)                                                # Watanabe-Akaike information criterion

loo(sm4)                                                 # Leave One Out Cross-Validation

loo(sm4, k_threshold = 0.7)  
###############################################################################

###############################################################################################################3
######################################
# Additional emulations using exhibit 6 in the supplementary material of Osetinsky et al paper.
# To approximate simulations for different hypothetical intervention/disease control program options. 
# https://www.healthaffairs.org/doi/suppl/10.1377/hlthaff.2018.05287/suppl_file/2018-05287_suppl_appendix.pdf
################# Combine GP plots 1 - 4 #####################
line = 1
cex = 1
side = 3
adj=-0.05
tiff("D:/inMODELA TRAINING/CONSULTANCY 1/BMC medical journal manuscript review/Plots/additional_emul1.tiff", width=12, height=10, units="in", res=300)  # Adjust width, height, and resolution as needed

par(mfrow=c(2,2), oma=c(1,6,1,1), cex.lab=1.5, cex.axis=1.2, cex.sub=1.2, font.lab=2)

### PLOT 1
# ART coverage target
x <- seq(2018,2028, l=11)
y <- seq(5219,2935, l=11)
# The simulator's confidence intervals
l <- seq(4728,2434, length.out = 11)
u <- seq(5911,3436, length.out = 11)
# Fitting the gaussian process
library(GauPro)
gp <- GauPro(x,y, parallel=FALSE)
# Plotting
# Run the following lines of code together
plot(x,y,xlab="years", ylab= "ART coverage",type="b", lwd=2, pch=16)#,
curve(gp$predict(x), add=T, col=2, lty=5)
curve(gp$predict(x)+1.96*gp$predict(x, se=T)$se, add=T, col=4)
curve(gp$predict(x)-1.96*gp$predict(x, se=T)$se, add=T, col=4)
lines(x,l,col="green",lwd=4)
lines(x,u,col="green",lwd=4)
mtext("A", side=side, line=line, cex=1.5, adj=adj)

# PLOT 2
# Hypertension treatment target
x <- seq(2018,2028, l=11)
y <- seq(9440,9540, l=11)
# Fitting the gaussian process
# The simulator's confidence intervals
l <- seq(9403, 9499, length.out=11)
u <- seq(9476, 9580, length.out=11)

# Fitting the gaussian process
library(GauPro)
gp <- GauPro(x,y, parallel=FALSE)
# Plotting
plot(x,y, xlab="years", ylab= "Hypertension treatment",type="b", lwd=2, pch=16)#,
curve(gp$predict(x), add=T, col=2, lty=5)
curve(gp$predict(x)+1.96*gp$predict(x, se=T)$se, add=T, col=4)
curve(gp$predict(x)-1.96*gp$predict(x, se=T)$se, add=T, col=4)
lines(x,l,col="green",lwd=4)
lines(x,u,col="green",lwd=4)
mtext("B", side=side, line=line, cex=1.5, adj=adj)

# PLOT 3
# Comorbid HIV and hypertension
x <- seq(2018,2028, l=11)
y <- seq(2581,1774, l=11)
# Fitting the gaussian process
# The simulator's confidence intervals
l <- seq(2137, 1472, length.out=11)
u <- seq(3025, 2075, length.out=11)

# Fitting the gaussian process
library(GauPro)
gp <- GauPro(x,y, parallel=FALSE)
# Plotting
plot(x,y, xlab="years", ylab= "Comorbid HIV and Hypertension treatment",type="b", lwd=2, pch=16)#,
curve(gp$predict(x), add=T, col=2, lty=5)
curve(gp$predict(x)+1.96*gp$predict(x, se=T)$se, add=T, col=4)
curve(gp$predict(x)-1.96*gp$predict(x, se=T)$se, add=T, col=4)
lines(x,l,col="green",lwd=4)
lines(x,u,col="green",lwd=4)
mtext("C", side=side, line=line, cex=1.5, adj=adj)
# PLOT 4
# Either HIV or hypertension
x <- seq(2018,2028, l=11)
y <- seq(41108,43476, l=11)
# Fitting the gaussian process
# The simulator's confidence intervals
l <- seq(40171, 42858, length.out=11)
u <- seq(41700, 44093, length.out=11)

# Fitting the gaussian process
library(GauPro)
gp <- GauPro(x,y, parallel=FALSE)
# Plotting
plot(x,y, xlab="years", ylab= "Either HIV or hypertension treatment",type="b", lwd=2, pch=16)#,
curve(gp$predict(x), add=T, col=2, lty=5)
curve(gp$predict(x)+1.96*gp$predict(x, se=T)$se, add=T, col=4)
curve(gp$predict(x)-1.96*gp$predict(x, se=T)$se, add=T, col=4)
lines(x,l,col="green",lwd=4)
lines(x,u,col="green",lwd=4)
mtext("D", side=side, line=line, cex=1.5, adj=adj)
dev.off()  # Close the TIFF device



###### BAYESIAN PEOSTERIOR PREDICTIVE ANALYSIS FOR THE ADDITIONAL EMULATIONS#############
side=3
line = 1
cex = 1
adj=-0.05

tiff("D:/inMODELA TRAINING/CONSULTANCY 1/BMC medical journal manuscript review/Plots/additional_emul4.tiff", width=12, height=10, units="in", res=300)  # Adjust width, height, and resolution as needed

par(mfrow=c(2,2), oma=c(1,6,1,1), cex.lab=1.5, cex.axis=1.2, cex.sub=1.2, font.lab=2)

set.seed(12345)

## PLOT 1 - Emulator's posterior predictions of HIV prevalence
x <- seq(2018,2028, l=11)
y <- seq(5219,2935, l=11)
# Add some random noise to y to show variation
set.seed(123)  # Set seed for reproducibility
y <- y + rnorm(length(y), mean = 0, sd = 50)
# using rstanarm's default priors. For details see the section on default weakly informative priors at https://mc-stan.org/rstanarm/articles/priors.html
sm1   <- stan_glm (y~x, 
                   family=gaussian(),                                                   # Specify distribution.
                   data = data.frame(x,y),                                             
                   seed=1234)
# Y predicted
ypred <- posterior_predict(sm1, draws = 50)                                            # Specify the number of draws to return (50 at minimum).

# density plot
plot(density(y),col="black",lwd=3, xlab="ART coverage", main="")
lines(density(ypred[1,]), col="red", lty=2, lwd=3)
for(i in 1:nrow(ypred))
{lines(density(ypred[i,]),col="lightblue",lty=3)}
lines(density(ypred[1,]), col="red", lty=2, lwd=3)
mtext("A", side=side, line=line, cex=1.5, adj=adj)



# PLOT 2 - Emulator's posterior predictions of Hypertension prevalence
x <- seq(2018,2028, l=11)
y <- seq(9440,9540, l=11)
# Add some random noise to y to show variation
set.seed(123)  # Set seed for reproducibility
y <- y + rnorm(length(y), mean = 0, sd = 50)
# R stanam
sm2   <- stan_glm(y~x, family=gaussian(), data = data.frame(x,y), seed=1234)
# Y predicted
ypred <- posterior_predict(sm2, draws = 50)
# density plot
plot(density(y),col="black", lwd=3, xlab="Hypertension treatment", main="")
lines(density(ypred[1,]), col="red", lty=2, lwd=3)
for(i in 1:nrow(ypred))
{lines(density(ypred[i,]),col="lightblue",lty=3)}
#lines(density(colMeans(ypred)), col="red", lty=2, lwd=3)
lines(density(ypred[1,]), col="red", lty=2, lwd=3)
mtext("B", side=side, line=line, cex=1.5, adj=adj)



# PLOT 3 - Emulator's posterior predictions of Comorbid HIV and hypertension
x <- seq(2018,2028, l=11)
y <- seq(2581,1774, l=11)
# Add some random noise to y to show variation
set.seed(123)  # Set seed for reproducibility
y <- y + rnorm(length(y), mean = 0, sd = 50)
# R stanam
sm3   <- stan_glm(y~x, family=gaussian(), data = data.frame(x,y), seed=1234)
# Y predicted
ypred <- posterior_predict(sm3, draws = 50)
# density plot
plot(density(y), col="black", lwd=3, xlab="HIV & Hypertension treatment", main="")
lines(density(ypred[1,]), col="red", lty=2, lwd=3)
for(i in 1:nrow(ypred))
{lines(density(ypred[i,]), col="lightblue", lty=3)}
lines(density(ypred[1,]), col="red", lty=2, lwd=3)
mtext("C", side=side, line=line, cex=1.5, adj=adj)



# PLOT 4 - Emulator's posterior predictions of HIV or HTN  among PLWH
x <- seq(2018,2028, l=11)
y <- seq(41108,43476, l=11)
# Add some random noise to y to show variation
set.seed(123)  # Set seed for reproducibility
y <- y + rnorm(length(y), mean = 0, sd = 50)
# R stanam
sm4   <- stan_glm(y~x, family=gaussian(), data = data.frame(x,y), seed=1234)
# Y predicted
ypred <- posterior_predict(sm4, draws = 50)
# density plot
plot(density(y),col="black", lwd=3, xlab="HIV or hypertension treatment", main = "")
lines(density(ypred[1,]), col="red", lty=2, lwd=3)
for(i in 1:nrow(ypred))
{lines(density(ypred[i,]),col="lightblue",lty=3)}
lines(density(ypred[1,]), col="red", lty=2, lwd=3)
mtext("D", side=side, line=line, cex=1.5, adj=adj)

dev.off()  # Close the TIFF device


# 1. Validating the accuracy of the ART coverage target emulation model compared to the simulation model
# We call sm1 from the above posterior predictive analysis
# Getting the credible intervals
posterior_interval(sm1, prob=0.95) # Use sm1 from the above posterior predictive analysis to obtain the 95% credible intervals

plot(sm1) # Graphically display the 95% credible intervals

posterior_vs_prior(sm1) # Juxtaposing the prior and the posterior estimates

pp_check(sm1) # Graphically display the posterior predictive analysis

waic(sm1) # Watanabe-Akaike information criterion

loo(sm1) # Leave One Out Cross-Validation
# In this tutorial, the Pareto k diagnostic is set at 0.7.
# the Pareto K diagnostic estimates how far an individual leave-one-out distribution is from the full distribution. 
# If k<0.5, then the corresponding component is estimated with high accuracy. If 0.5<k<0.7 the accuracy is lower, but still ok.

# If # This line gives a warning.
# It is recommended to call 'loo' again with argument 'k_threshold = 0.7' 
# in order to calculate the ELPD without the assumption that these observations are negligible. 
# This will refit the model 1 times to compute the ELPDs for the problematic observations directly.
loo(sm1, k_threshold = 0.7)

# 2. Validating the accuracy of the hypertension treatment target emulation model compared to the simulation model

posterior_interval(sm2, prob=0.95) # Use sm2 from the above posterior predictive analysis to obtain the 95% credible intervals

plot(sm2)# Graphically display the 95% credible intervals

posterior_vs_prior(sm2)# Juxtaposing the prior and the posterior estimates

pp_check(sm2)# Graphically display the posterior predictive analysis

waic(sm2) # Watanabe-Akaike information criterion

# Leave One Out Cross-Validation
loo(sm2)

loo(sm2, k_threshold = 0.7) # Use this alternative if you get a warning in the above line  

# 3. Validating the accuracy of the comorbid HIV and hypertension treatment target emulation model compared to the simulation model

posterior_interval(sm3, prob=0.95) # Use sm3 from the above posterior predictive analysis to obtain the 95% credible intervals

plot(sm3) # Graphically display the 95% credible intervals

posterior_vs_prior(sm3) # Juxtaposing the prior and the posterior estimates

pp_check(sm3) # Graphically display the posterior predictive analysis

waic(sm3) # Watanabe-Akaike information criterion

loo(sm3)   # Leave One Out Cross-Validation

loo(sm3, k_threshold = 0.7) # Use this alternative if you get a warning in the above line

# 4. Validating the accuracy of the either HIV or hypertension treatment target emulation model compared to the simulation model

posterior_interval(sm4, prob=0.95) # Use sm4 from the above posterior predictive analysis to obtain the 95% credible intervals

plot(sm4) # Graphically display the 95% credible intervals

posterior_vs_prior(sm4) # Juxtaposing the prior and the posterior estimates

pp_check(sm4) # Graphically display the posterior predictive analysis

waic(sm4) # Watanabe-Akaike information criterion

loo(sm4) # Leave One Out Cross-Validation

loo(sm4, k_threshold = 0.7) # Use this alternative if you get a warning in the above line

