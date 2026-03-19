#############################################################################
### DO FILE FOR REPLICATION OF:                                           ###
### Winning Hearts and Minds in Civil Wars:                               ###
### Governance, Leadership Change, and Support for Violent Groups in Iraq ###
### (by Christoph Mikulaschek, Saurabh Pant, and Beza Tesfaye)            ###
############################################################################# 

# Note: This do file contains code for replication of all results displayed in figures in the main text or in the Appendix. Run the Dofile-dataset-construction-1.do in STATA and the Dofile-dataset-construction-2.R in R before replicating any figures.

################################
### FIGURES IN THE MAIN TEXT ###
################################

####################################################################################################
### FIGURE 2: Estimated change in Sunni and Shiite attitudes after al-Maliki’s announced resignation

####Please run Models 1 to 7 in Dofile-analyses.R first

##Sympathy with armed opposition and government's performance legitimacy (top panel)

#Estimate for change in attitudes of Sunni Arabs by dependent variable
mt.fig2.sunnisymp <- as.numeric(coef(fit1)[2]) + as.numeric(coef(fit1)[nrow(boot1) - 2]) #DV: Sympathy with armed opposition
mt.fig2.sunnigov <- as.numeric(coef(fit2)[2]) + as.numeric(coef(fit2)[nrow(boot2) - 2]) #DV: Rating of federal government
mt.fig2.sunnisec <- as.numeric(coef(fit3)[2]) + as.numeric(coef(fit3)[nrow(boot3) - 2]) #DV: Likely security improvement
mt.fig2.sunnijobs <- as.numeric(coef(fit4)[2]) + as.numeric(coef(fit4)[nrow(boot4) - 2]) #DV: Likely jobs improvement
mt.fig2.sunnielec <- as.numeric(coef(fit5)[2]) + as.numeric(coef(fit5)[nrow(boot5) - 2]) #DV: Likely electricity improvement

#Estimate for change in attitudes of Shia Arabs by dependent variable
mt.fig2.shiasymp <- as.numeric(coef(fit1)[2]) #DV: Sympathy with armed opposition
mt.fig2.shiagov <- as.numeric(coef(fit2)[2]) #DV: Rating of federal government
mt.fig2.shiasec <- as.numeric(coef(fit3)[2]) #DV: Likely security improvement
mt.fig2.shiajobs <- as.numeric(coef(fit4)[2]) #DV: Likely jobs improvement
mt.fig2.shiaelec <- as.numeric(coef(fit5)[2]) #DV: Likely electricity improvement

#Standard error for change in attitudes of Sunni Arabs by dependent variable
mt.fig2.sunnisympse <-  sqrt(boot1[2, 2] + boot1[nrow(boot1) - 2, nrow(boot1) - 2] + (2 * boot1[2, nrow(boot1) - 2])) #DV: Sympathy with armed opposition
mt.fig2.sunnigovse <-  sqrt(boot2[2, 2] + boot2[nrow(boot2) - 2, nrow(boot2) - 2] + (2 * boot2[2, nrow(boot2) - 2])) #DV: Rating of federal government
mt.fig2.sunnisecse <-  sqrt(boot3[2, 2] + boot3[nrow(boot3) - 2, nrow(boot3) - 2] + (2 * boot3[2, nrow(boot3) - 2])) #DV: Likely security improvement
mt.fig2.sunnijobsse <- sqrt(boot4[2, 2] + boot4[nrow(boot4) - 2, nrow(boot4) - 2] + (2 * boot4[2, nrow(boot4) - 2])) #DV: Likely jobs improvement
mt.fig2.sunnielecse <- sqrt(boot5[2, 2] + boot5[nrow(boot5) - 2, nrow(boot5) - 2] + (2 * boot5[2, nrow(boot5) - 2])) #DV: Likely electricity improvement

#Standard error for change in attitudes of Shia Arabs by dependent variable
mt.fig2.shiasympse <- sqrt(boot1[2, 2]) #DV: Sympathy with armed opposition
mt.fig2.shiagovse <-  sqrt(boot2[2, 2]) #DV: Rating of federal government
mt.fig2.shiasecse <-  sqrt(boot3[2, 2]) #DV: Likely security improvement
mt.fig2.shiajobsse <- sqrt(boot4[2, 2]) #DV: Likely jobs improvement
mt.fig2.shiaelecse <- sqrt(boot5[2, 2]) #DV: Likely electricity improvement

#Creating chart
par(mar=c(3, 7, 9.5, 4))
plot(1, type="n", xaxt = 'n', yaxt = 'n', xlab="", ylab="", xlim=c(1,16), ylim=c(-1, 1))
box(lwd = 3)

#Adding estimates to chart for DV: Sympathy with armed opposition
points(x = 1, y = mt.fig2.sunnisymp, pch = 19, cex = 4.15, col = "red") #Sunni estimate
points(x = 2, y = mt.fig2.shiasymp, pch = 19, cex = 4.15) #Shia estimate

#Adding estimates to chart for DV: Rating of federal government
points(x = 4.5, y = mt.fig2.sunnigov, pch = 19, cex = 4.15, col = "red") #Sunni estimate
points(x = 5.5, y = mt.fig2.shiagov, pch = 19, cex = 4.15) #Shia estimate

#Adding estimates to chart for DV: Likely security improvement
points(x = 8, y = mt.fig2.sunnisec, pch = 19, cex = 4.15, col = "red") #Sunni estimate
points(x = 9, y = mt.fig2.shiasec, pch = 19, cex = 4.15) #Shia estimate

#Adding estimates to chart for DV: Likely jobs improvement
points(x = 11.5, y = mt.fig2.sunnijobs, pch = 19, cex = 4.15, col = "red") #Sunni estimate
points(x = 12.5, y = mt.fig2.shiajobs, pch = 19, cex = 4.15) #Shia estimate

#Adding estimates to chart for DV: Likely electricity improvement
points(x = 15, y = mt.fig2.sunnielec, pch = 19, cex = 4.15, col = "red") #Sunni estimate
points(x = 16, y = mt.fig2.shiaelec, pch = 19, cex = 4.15) #Shia estimate

#Adding zero effect line
abline(h = 0, lty = 3, lwd = 4)

#Adding 95% confidence intervals to chart for DV: Sympathy with armed opposition
lines(x = c(1, 1), y = c(mt.fig2.sunnisymp - (qnorm(0.975)*mt.fig2.sunnisympse), mt.fig2.sunnisymp + (qnorm(0.975)*mt.fig2.sunnisympse)), lwd = 4, col = "red") #Sunni confidence interval
lines(x = c(2, 2), y = c(mt.fig2.shiasymp - (qnorm(0.975)*mt.fig2.shiasympse), mt.fig2.shiasymp + (qnorm(0.975)*mt.fig2.shiasympse)), lwd = 4) #Shia confidence interval

#Adding 95% confidence intervals to chart for DV: Rating of federal government
lines(x = c(4.5, 4.5), y = c(mt.fig2.sunnigov - (qnorm(0.975)*mt.fig2.sunnigovse), mt.fig2.sunnigov + (qnorm(0.975)*mt.fig2.sunnigovse)), lwd = 4, col = "red") #Sunni confidence interval
lines(x = c(5.5, 5.5), y = c(mt.fig2.shiagov - (qnorm(0.975)*mt.fig2.shiagovse), mt.fig2.shiagov + (qnorm(0.975)*mt.fig2.shiagovse)), lwd = 4) #Shia confidence interval

#Adding 95% confidence intervals to chart for DV: Likely security improvement
lines(x = c(8, 8), y = c(mt.fig2.sunnisec - (qnorm(0.975)*mt.fig2.sunnisecse), mt.fig2.sunnisec + (qnorm(0.975)*mt.fig2.sunnisecse)), lwd = 4, col = "red") #Sunni confidence interval
lines(x = c(9, 9), y = c(mt.fig2.shiasec - (qnorm(0.975)*mt.fig2.shiasecse), mt.fig2.shiasec + (qnorm(0.975)*mt.fig2.shiasecse)), lwd = 4) #Shia confidence interval

#Adding 95% confidence intervals to chart for DV: Likely jobs improvement
lines(x = c(11.5, 11.5), y = c(mt.fig2.sunnijobs - (qnorm(0.975)*mt.fig2.sunnijobsse), mt.fig2.sunnijobs + (qnorm(0.975)*mt.fig2.sunnijobsse)), lwd = 4, col = "red") #Sunni confidence interval
lines(x = c(12.5, 12.5), y = c(mt.fig2.shiajobs - (qnorm(0.975)*mt.fig2.shiajobsse), mt.fig2.shiajobs + (qnorm(0.975)*mt.fig2.shiajobsse)), lwd = 4) #Shia confidence interval

#Adding 95% confidence intervals to chart for DV: Likely electricity improvement
lines(x = c(15, 15), y = c(mt.fig2.sunnielec - (qnorm(0.975)*mt.fig2.sunnielecse), mt.fig2.sunnielec + (qnorm(0.975)*mt.fig2.sunnielecse)), lwd = 4, col = "red") #Sunni confidence interval
lines(x = c(16, 16), y = c(mt.fig2.shiaelec - (qnorm(0.975)*mt.fig2.shiaelecse), mt.fig2.shiaelec + (qnorm(0.975)*mt.fig2.shiaelecse)), lwd = 4) #Shia confidence interval

#Labels for chart
xlabel <- c("Sunni  ")
xlabel.2 <- c("  Shia")
xlabel.3 <- c("Sympathy w.", "Rating of", "Security", "Jobs", "Electricity")
xlabel.4 <- c("armed opp.", "government", "improve", "improve", "improve")
ytickmarks <- seq(-0.8, 0.8, 0.4)
ylabels <- seq(-0.8, 0.8, 0.4)

#Adding labels and titles
mtext(side = 1, xlabel, at = c(1.1, 4.5, 8, 11.5, 15), cex = 3, line = 1.9, col = "red") #x-axis Sunni label below the plot
mtext(side = 1, xlabel.2, at = c(2.1, 5.5, 9, 12.5, 16), cex = 3, line = 1.9) #x-axis Shia label below the plot
mtext(side = 3, xlabel.3, at = c(1.8, 5, 8.5, 12, 15.5), cex = 3,font = 2, line = 3) #x-axis title above the plot
mtext(side = 3, xlabel.4, at = c(1.8, 5, 8.5, 12, 15.5), cex = 3,font = 2, line = 1) #x-axis title above the plot
axis(side = 2, at = ytickmarks, labels = NA, cex.axis = 3, lwd = 4, lwd.ticks = 4, line = 0) #y-axis
mtext(ylabels, side = 2, at = ylabels, cex = 3, line = 1) #y-axis labels
mtext("Change in attitude", side = 2, cex = 3, line = 4) #y-axis title

#Save as jpeg with width = 2000 & height = 1500#

##Government's democratic legitimacy (bottom panel)

#Estimate for change in attitudes of Sunni Arabs by dependent variable
mt.fig2.sunniinfl <- as.numeric(coef(fit6)[2]) + as.numeric(coef(fit6)[nrow(boot6) - 2]) #DV: Influence government decisions
mt.fig2.sunnidem <- as.numeric(coef(fit7)[2]) + as.numeric(coef(fit7)[nrow(boot7) - 2]) #DV: Situation of democracy

#Estimate for change in attitudes of Shia Arabs by dependent variable
mt.fig2.shiainfl <- as.numeric(coef(fit6)[2]) #DV: Influence of government decisions
mt.fig2.shiadem <- as.numeric(coef(fit7)[2]) #DV: Situation of democracy

#Standard error for change in attitudes of Sunni Arabs by dependent variable
mt.fig2.sunniinflse <-  sqrt(boot6[2, 2] + boot6[nrow(boot6) - 2, nrow(boot6) - 2] + (2 * boot6[2, nrow(boot6) - 2])) #DV: Influence of government decisions
mt.fig2.sunnidemse <-  sqrt(boot7[2, 2] + boot7[nrow(boot7) - 2, nrow(boot7) - 2] + (2 * boot7[2, nrow(boot7) - 2])) #DV: Situation of democracy

#Standard error for change in attitudes of Shia Arabs by dependent variable
mt.fig2.shiainflse <- sqrt(boot6[2, 2]) #DV: Influence of government decisions
mt.fig2.shiademse <-  sqrt(boot7[2, 2]) #DV: Situation of democracy

#Creating chart
par(mar=c(3, 7, 9.5, 4))
plot(1, type="n", xaxt = 'n', yaxt = 'n', xlab="", ylab="", xlim=c(.25, 4.75), ylim=c(-1, 1))
box(lwd = 3)

#Adding estimates to chart for DV: Influence of government decisions
points(x = 1, y = mt.fig2.sunniinfl, pch = 19, cex = 4.15, col = "red") #Sunni estimate
points(x = 1.75, y = mt.fig2.shiainfl, pch = 19, cex = 4.15) #Shia estimate

#Adding estimates to chart for DV: Situation of democracy
points(x = 3.25, y = mt.fig2.sunnidem, pch = 19, cex = 4.15, col = "red") #Sunni estimate
points(x = 4, y = mt.fig2.shiadem, pch = 19, cex = 4.15) #Shia estimate

#Adding zero effect line
abline(h = 0, lty = 3, lwd = 4)

#Adding 95% confidence intervals to chart for DV: Influence of government decisions
lines(x = c(1, 1), y = c(mt.fig2.sunniinfl - (qnorm(0.975)*mt.fig2.sunniinflse), mt.fig2.sunniinfl + (qnorm(0.975)*mt.fig2.sunniinflse)), lwd = 4, col = "red") #Sunni confidence interval
lines(x = c(1.75, 1.75), y = c(mt.fig2.shiainfl - (qnorm(0.975)*mt.fig2.shiainflse), mt.fig2.shiainfl + (qnorm(0.975)*mt.fig2.shiainflse)), lwd = 4) #Shia confidence interval

#Adding 95% confidence intervals to chart for DV: Situation of democracy
lines(x = c(3.25, 3.25), y = c(mt.fig2.sunnidem - (qnorm(0.975)*mt.fig2.sunnidemse), mt.fig2.sunnidem + (qnorm(0.975)*mt.fig2.sunnidemse)), lwd = 4, col = "red") #Sunni confidence interval
lines(x = c(4, 4), y = c(mt.fig2.shiadem - (qnorm(0.975)*mt.fig2.shiademse), mt.fig2.shiadem + (qnorm(0.975)*mt.fig2.shiademse)), lwd = 4) #Shia confidence interval

#Labels for chart
xlabel <- c("Sunni  ")
xlabel.2 <- c("  Shia")
xlabel.3 <- c("Influence govt.", "Situation of")
xlabel.4 <- c("decisions", "democracy")
ytickmarks <- seq(-0.8, 0.8, 0.4)
ylabels <- seq(-0.8, 0.8, 0.4)

#Adding labels and titles
mtext(side = 1, xlabel, at = c(1, 3.25), cex = 3, line = 1.9, col = "red") # x-axis Sunni label below the plot
mtext(side = 1, xlabel.2, at = c(1.75, 4), cex = 3, line = 1.9) # x-axis Shia label below the plot
mtext(side = 3, xlabel.3, at = c(1.38, 3.63), cex = 3,font = 2, line = 3) # x-axis title above the plot
mtext(side = 3, xlabel.4, at = c(1.38, 3.63), cex = 3,font = 2, line = 1) # x axis title above the plot
axis(side = 2, at = ytickmarks, labels = NA, cex.axis = 3, lwd = 4, lwd.ticks = 4, line = 0) # y-axis
mtext(ylabels, side = 2, at = ylabels, cex = 3, line = 1) # y-axis labels
mtext("Change in attitude", side = 2, cex = 3, line = 4) # y-axis title

#Save as jpeg with width = 2000 & height = 1500#

#####################################################################################
### FIGURE 3: Results from 40 OLS models of the date on which Sunni attitudes changed

# Replicates for wildbootstrap

replicates <- 10000000

# Run the 40 OLS models

library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign.0 + sunni + kurd + other + sunni*resign.0 + kurd*resign.0 + other*resign.0 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 64 & responses.all$day.nr <= 93,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 64 & responses.all$day.nr <= 93], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit2 <- lm(armedopp.symp.bin ~ resign.1 + sunni + kurd + other + sunni*resign.1 + kurd*resign.1 + other*resign.1 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 65 & responses.all$day.nr <= 94,])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit2, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 65 & responses.all$day.nr <= 94], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild) 
coeftest(fit2, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit3 <- lm(armedopp.symp.bin ~ resign.2 + sunni + kurd + other + sunni*resign.2 + kurd*resign.2 + other*resign.2 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 66 & responses.all$day.nr <= 95,])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit3, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 66 & responses.all$day.nr <= 95], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild) 
coeftest(fit3, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit4 <- lm(armedopp.symp.bin ~ resign.3 + sunni + kurd + other + sunni*resign.3 + kurd*resign.3 + other*resign.3 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 67 & responses.all$day.nr <= 96,])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit4, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 67 & responses.all$day.nr <= 96], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild) 
coeftest(fit4, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit5 <- lm(armedopp.symp.bin ~ resign.4 + sunni + kurd + other + sunni*resign.4 + kurd*resign.4 + other*resign.4 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 68 & responses.all$day.nr <= 97,])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit5, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 68 & responses.all$day.nr <= 97], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild) 
coeftest(fit5, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit6 <- lm(armedopp.symp.bin ~ resign.5 + sunni + kurd + other + sunni*resign.5 + kurd*resign.5 + other*resign.5 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 69 & responses.all$day.nr <= 98,])
summary(fit6)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit6, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 69 & responses.all$day.nr <= 98], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot6 <- as.data.frame(bootwild) 
coeftest(fit6, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit7 <- lm(armedopp.symp.bin ~ resign.6 + sunni + kurd + other + sunni*resign.6 + kurd*resign.6 + other*resign.6 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 70 & responses.all$day.nr <= 99,])
summary(fit7)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit7, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 70 & responses.all$day.nr <= 99], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot7 <- as.data.frame(bootwild) 
coeftest(fit7, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit8 <- lm(armedopp.symp.bin ~ resign.7 + sunni + kurd + other + sunni*resign.7 + kurd*resign.7 + other*resign.7 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 71 & responses.all$day.nr <= 100,])
summary(fit8)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit8, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 71 & responses.all$day.nr <= 100], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot8 <- as.data.frame(bootwild) 
coeftest(fit8, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit9 <- lm(armedopp.symp.bin ~ resign.8 + sunni + kurd + other + sunni*resign.8 + kurd*resign.8 + other*resign.8 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 72 & responses.all$day.nr <= 101,])
summary(fit9)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit9, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 72 & responses.all$day.nr <= 101], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot9 <- as.data.frame(bootwild) 
coeftest(fit9, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit10 <- lm(armedopp.symp.bin ~ resign.9 + sunni + kurd + other + sunni*resign.9 + kurd*resign.9 + other*resign.9 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 73 & responses.all$day.nr <= 102,])
summary(fit10)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit10, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 73 & responses.all$day.nr <= 102], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot10 <- as.data.frame(bootwild) 
coeftest(fit10, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit11 <- lm(armedopp.symp.bin ~ resign.10 + sunni + kurd + other + sunni*resign.10 + kurd*resign.10 + other*resign.10 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 74 & responses.all$day.nr <= 103,])
summary(fit11)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit11, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 74 & responses.all$day.nr <= 103], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot11 <- as.data.frame(bootwild) 
coeftest(fit11, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit12 <- lm(armedopp.symp.bin ~ resign.11 + sunni + kurd + other + sunni*resign.11 + kurd*resign.11 + other*resign.11 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 75 & responses.all$day.nr <= 104,])
summary(fit12)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit12, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 75 & responses.all$day.nr <= 104], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot12 <- as.data.frame(bootwild) 
coeftest(fit12, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit13 <- lm(armedopp.symp.bin ~ resign.12 + sunni + kurd + other + sunni*resign.12 + kurd*resign.12 + other*resign.12 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 76 & responses.all$day.nr <= 105,])
summary(fit13)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit13, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 76 & responses.all$day.nr <= 105], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot13 <- as.data.frame(bootwild) 
coeftest(fit13, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit14 <- lm(armedopp.symp.bin ~ resign.13 + sunni + kurd + other + sunni*resign.13 + kurd*resign.13 + other*resign.13 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 77 & responses.all$day.nr <= 106,])
summary(fit14)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit14, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 77 & responses.all$day.nr <= 106], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot14 <- as.data.frame(bootwild) 
coeftest(fit14, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit15 <- lm(armedopp.symp.bin ~ resign.14 + sunni + kurd + other + sunni*resign.14 + kurd*resign.14 + other*resign.14 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 78 & responses.all$day.nr <= 107,])
summary(fit15)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit15, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 78 & responses.all$day.nr <= 107], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot15 <- as.data.frame(bootwild) 
coeftest(fit15, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit16 <- lm(armedopp.symp.bin ~ resign.15 + sunni + kurd + other + sunni*resign.15 + kurd*resign.15 + other*resign.15 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 79 & responses.all$day.nr <= 108,])
summary(fit16)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit16, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 79 & responses.all$day.nr <= 108], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot16 <- as.data.frame(bootwild) 
coeftest(fit16, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit17 <- lm(armedopp.symp.bin ~ resign.16 + sunni + kurd + other + sunni*resign.16 + kurd*resign.16 + other*resign.16 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 80 & responses.all$day.nr <= 109,])
summary(fit17)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit17, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 80 & responses.all$day.nr <= 109], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot17 <- as.data.frame(bootwild) 
coeftest(fit17, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit18 <- lm(armedopp.symp.bin ~ resign.17 + sunni + kurd + other + sunni*resign.17 + kurd*resign.17 + other*resign.17 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 81 & responses.all$day.nr <= 110,])
summary(fit18)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit18, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 81 & responses.all$day.nr <= 110], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot18 <- as.data.frame(bootwild) 
coeftest(fit18, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit19 <- lm(armedopp.symp.bin ~ resign.18 + sunni + kurd + other + sunni*resign.18 + kurd*resign.18 + other*resign.18 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 82 & responses.all$day.nr <= 111,])
summary(fit19)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit19, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 82 & responses.all$day.nr <= 111], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot19 <- as.data.frame(bootwild) 
coeftest(fit19, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit20 <- lm(armedopp.symp.bin ~ resign.19 + sunni + kurd + other + sunni*resign.19 + kurd*resign.19 + other*resign.19 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 83 & responses.all$day.nr <= 112,])
summary(fit20)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit20, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 83 & responses.all$day.nr <= 112], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot20 <- as.data.frame(bootwild) 
coeftest(fit20, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit21 <- lm(armedopp.symp.bin ~ resign.20 + sunni + kurd + other + sunni*resign.20 + kurd*resign.20 + other*resign.20 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 84 & responses.all$day.nr <= 113,])
summary(fit21)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit21, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 84 & responses.all$day.nr <= 113], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot21 <- as.data.frame(bootwild) 
coeftest(fit21, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit22 <- lm(armedopp.symp.bin ~ resign.21 + sunni + kurd + other + sunni*resign.21 + kurd*resign.21 + other*resign.21 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 85 & responses.all$day.nr <= 114,])
summary(fit22)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit22, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 85 & responses.all$day.nr <= 114], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot22 <- as.data.frame(bootwild) 
coeftest(fit22, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit23 <- lm(armedopp.symp.bin ~ resign.22 + sunni + kurd + other + sunni*resign.22 + kurd*resign.22 + other*resign.22 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 86 & responses.all$day.nr <= 115,])
summary(fit23)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit23, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 86 & responses.all$day.nr <= 115], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot23 <- as.data.frame(bootwild) 
coeftest(fit23, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit24 <- lm(armedopp.symp.bin ~ resign.23 + sunni + kurd + other + sunni*resign.23 + kurd*resign.23 + other*resign.23 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 87 & responses.all$day.nr <= 116,])
summary(fit24)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit24, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 87 & responses.all$day.nr <= 116], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot24 <- as.data.frame(bootwild) 
coeftest(fit24, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit25 <- lm(armedopp.symp.bin ~ resign.24 + sunni + kurd + other + sunni*resign.24 + kurd*resign.24 + other*resign.24 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 88 & responses.all$day.nr <= 117,])
summary(fit25)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit25, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 88 & responses.all$day.nr <= 117], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot25 <- as.data.frame(bootwild) 
coeftest(fit25, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit26 <- lm(armedopp.symp.bin ~ resign.25 + sunni + kurd + other + sunni*resign.25 + kurd*resign.25 + other*resign.25 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 89 & responses.all$day.nr <= 118,])
summary(fit26)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit26, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 89 & responses.all$day.nr <= 118], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot26 <- as.data.frame(bootwild) 
coeftest(fit26, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit27 <- lm(armedopp.symp.bin ~ resign.26 + sunni + kurd + other + sunni*resign.26 + kurd*resign.26 + other*resign.26 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 90 & responses.all$day.nr <= 119,])
summary(fit27)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit27, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 90 & responses.all$day.nr <= 119], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot27 <- as.data.frame(bootwild) 
coeftest(fit27, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit28 <- lm(armedopp.symp.bin ~ resign.27 + sunni + kurd + other + sunni*resign.27 + kurd*resign.27 + other*resign.27 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 91 & responses.all$day.nr <= 120,])
summary(fit28)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit28, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 91 & responses.all$day.nr <= 120], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot28 <- as.data.frame(bootwild) 
coeftest(fit28, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit29 <- lm(armedopp.symp.bin ~ resign.28 + sunni + kurd + other + sunni*resign.28 + kurd*resign.28 + other*resign.28 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 92 & responses.all$day.nr <= 121,])
summary(fit29)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit29, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 92 & responses.all$day.nr <= 121], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot29 <- as.data.frame(bootwild) 
coeftest(fit29, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit30 <- lm(armedopp.symp.bin ~ resign.29 + sunni + kurd + other + sunni*resign.29 + kurd*resign.29 + other*resign.29 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 93 & responses.all$day.nr <= 122,])
summary(fit30)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit30, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 93 & responses.all$day.nr <= 122], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot30 <- as.data.frame(bootwild) 
coeftest(fit30, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit31 <- lm(armedopp.symp.bin ~ resign.30 + sunni + kurd + other + sunni*resign.30 + kurd*resign.30 + other*resign.30 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 94 & responses.all$day.nr <= 123,])
summary(fit31)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit31, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 94 & responses.all$day.nr <= 123], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot31 <- as.data.frame(bootwild) 
coeftest(fit31, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit32 <- lm(armedopp.symp.bin ~ resign.31 + sunni + kurd + other + sunni*resign.31 + kurd*resign.31 + other*resign.31 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 95 & responses.all$day.nr <= 124,])
summary(fit32)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit32, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 95 & responses.all$day.nr <= 124], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot32 <- as.data.frame(bootwild) 
coeftest(fit32, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit33 <- lm(armedopp.symp.bin ~ resign.32 + sunni + kurd + other + sunni*resign.32 + kurd*resign.32 + other*resign.32 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 96 & responses.all$day.nr <= 125,])
summary(fit33)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit33, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 96 & responses.all$day.nr <= 125], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot33 <- as.data.frame(bootwild) 
coeftest(fit33, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit34 <- lm(armedopp.symp.bin ~ resign.33 + sunni + kurd + other + sunni*resign.33 + kurd*resign.33 + other*resign.33 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 97 & responses.all$day.nr <= 126,])
summary(fit34)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit34, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 97 & responses.all$day.nr <= 126], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot34 <- as.data.frame(bootwild) 
coeftest(fit34, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit35 <- lm(armedopp.symp.bin ~ resign.34 + sunni + kurd + other + sunni*resign.34 + kurd*resign.34 + other*resign.34 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 98 & responses.all$day.nr <= 127,])
summary(fit35)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit35, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 98 & responses.all$day.nr <= 127], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot35 <- as.data.frame(bootwild) 
coeftest(fit35, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit36 <- lm(armedopp.symp.bin ~ resign.35 + sunni + kurd + other + sunni*resign.35 + kurd*resign.35 + other*resign.35 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 99 & responses.all$day.nr <= 128,])
summary(fit36)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit36, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 99 & responses.all$day.nr <= 128], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot36 <- as.data.frame(bootwild) 
coeftest(fit36, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit37 <- lm(armedopp.symp.bin ~ resign.36 + sunni + kurd + other + sunni*resign.36 + kurd*resign.36 + other*resign.36 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 100 & responses.all$day.nr <= 129,])
summary(fit37)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit37, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 100 & responses.all$day.nr <= 129], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot37 <- as.data.frame(bootwild) 
coeftest(fit37, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit38 <- lm(armedopp.symp.bin ~ resign.37 + sunni + kurd + other + sunni*resign.37 + kurd*resign.37 + other*resign.37 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 101 & responses.all$day.nr <= 130,])
summary(fit38)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit38, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 101 & responses.all$day.nr <= 130], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot38 <- as.data.frame(bootwild) 
coeftest(fit38, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit39 <- lm(armedopp.symp.bin ~ resign.38 + sunni + kurd + other + sunni*resign.38 + kurd*resign.38 + other*resign.38 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 102 & responses.all$day.nr <= 131,])
summary(fit39)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit39, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 102 & responses.all$day.nr <= 131], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot39 <- as.data.frame(bootwild) 
coeftest(fit39, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit40 <- lm(armedopp.symp.bin ~ resign.39 + sunni + kurd + other + sunni*resign.39 + kurd*resign.39 + other*resign.39 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 103 & responses.all$day.nr <= 132,])
summary(fit40)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit40, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 103 & responses.all$day.nr <= 132], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot40 <- as.data.frame(bootwild) 
coeftest(fit40, bootwild) 
proc.time() - ptm
stopCluster(cl)

#Day number variable (day.nr) in dataset that corresponds to August 14 is day.nr = 99

#Creating data frame
rollingregs <- as.data.frame(matrix(nrow = 40)) #Creating data frame to store results from loop
rollingregs$Regnumber <- seq(1,40,1) #Value for each row in dataframe

#Putting in date
for (i in 1:nrow(rollingregs)){
   if (i < 8) {
      rollingregs$Date[i] <- paste(7, "/", 24 + i)
   }
   if (8 <= i & i < 39) {
      rollingregs$Date[i] <- paste(8, "/", i-7)
   }
   if (i >= 39) {
      rollingregs$Date[i] <- paste(9, "/", i - 38)
   }
}

#Estimates
rollingregs$Estimate[1] <- as.numeric(coef(fit1)[2]) + as.numeric(coef(fit1)[nrow(boot1) - 2])
rollingregs$Estimate[2] <- as.numeric(coef(fit2)[2]) + as.numeric(coef(fit2)[nrow(boot2) - 2])
rollingregs$Estimate[3] <- as.numeric(coef(fit3)[2]) + as.numeric(coef(fit3)[nrow(boot3) - 2])
rollingregs$Estimate[4] <- as.numeric(coef(fit4)[2]) + as.numeric(coef(fit4)[nrow(boot4) - 2])
rollingregs$Estimate[5] <- as.numeric(coef(fit5)[2]) + as.numeric(coef(fit5)[nrow(boot5) - 2])
rollingregs$Estimate[6] <- as.numeric(coef(fit6)[2]) + as.numeric(coef(fit6)[nrow(boot6) - 2])
rollingregs$Estimate[7] <- as.numeric(coef(fit7)[2]) + as.numeric(coef(fit7)[nrow(boot7) - 2])
rollingregs$Estimate[8] <- as.numeric(coef(fit8)[2]) + as.numeric(coef(fit8)[nrow(boot8) - 2])
rollingregs$Estimate[9] <- as.numeric(coef(fit9)[2]) + as.numeric(coef(fit9)[nrow(boot9) - 2])
rollingregs$Estimate[10] <- as.numeric(coef(fit10)[2]) + as.numeric(coef(fit10)[nrow(boot10) - 2])
rollingregs$Estimate[11] <- as.numeric(coef(fit11)[2]) + as.numeric(coef(fit11)[nrow(boot11) - 2])
rollingregs$Estimate[12] <- as.numeric(coef(fit12)[2]) + as.numeric(coef(fit12)[nrow(boot12) - 2])
rollingregs$Estimate[13] <- as.numeric(coef(fit13)[2]) + as.numeric(coef(fit13)[nrow(boot13) - 2])
rollingregs$Estimate[14] <- as.numeric(coef(fit14)[2]) + as.numeric(coef(fit14)[nrow(boot14) - 2])
rollingregs$Estimate[15] <- as.numeric(coef(fit15)[2]) + as.numeric(coef(fit15)[nrow(boot15) - 2])
rollingregs$Estimate[16] <- as.numeric(coef(fit16)[2]) + as.numeric(coef(fit16)[nrow(boot16) - 2])
rollingregs$Estimate[17] <- as.numeric(coef(fit17)[2]) + as.numeric(coef(fit17)[nrow(boot17) - 2])
rollingregs$Estimate[18] <- as.numeric(coef(fit18)[2]) + as.numeric(coef(fit18)[nrow(boot18) - 2])
rollingregs$Estimate[19] <- as.numeric(coef(fit19)[2]) + as.numeric(coef(fit19)[nrow(boot19) - 2])
rollingregs$Estimate[20] <- as.numeric(coef(fit20)[2]) + as.numeric(coef(fit20)[nrow(boot20) - 2])
rollingregs$Estimate[21] <- as.numeric(coef(fit21)[2]) + as.numeric(coef(fit21)[nrow(boot21) - 2])
rollingregs$Estimate[22] <- as.numeric(coef(fit22)[2]) + as.numeric(coef(fit22)[nrow(boot22) - 2])
rollingregs$Estimate[23] <- as.numeric(coef(fit23)[2]) + as.numeric(coef(fit23)[nrow(boot23) - 2])
rollingregs$Estimate[24] <- as.numeric(coef(fit24)[2]) + as.numeric(coef(fit24)[nrow(boot24) - 2])
rollingregs$Estimate[25] <- as.numeric(coef(fit25)[2]) + as.numeric(coef(fit25)[nrow(boot25) - 2])
rollingregs$Estimate[26] <- as.numeric(coef(fit26)[2]) + as.numeric(coef(fit26)[nrow(boot26) - 2])
rollingregs$Estimate[27] <- as.numeric(coef(fit27)[2]) + as.numeric(coef(fit27)[nrow(boot27) - 2])
rollingregs$Estimate[28] <- as.numeric(coef(fit28)[2]) + as.numeric(coef(fit28)[nrow(boot28) - 2])
rollingregs$Estimate[29] <- as.numeric(coef(fit29)[2]) + as.numeric(coef(fit29)[nrow(boot29) - 2])
rollingregs$Estimate[30] <- as.numeric(coef(fit30)[2]) + as.numeric(coef(fit30)[nrow(boot30) - 2])
rollingregs$Estimate[31] <- as.numeric(coef(fit31)[2]) + as.numeric(coef(fit31)[nrow(boot31) - 2])
rollingregs$Estimate[32] <- as.numeric(coef(fit32)[2]) + as.numeric(coef(fit32)[nrow(boot32) - 2])
rollingregs$Estimate[33] <- as.numeric(coef(fit33)[2]) + as.numeric(coef(fit33)[nrow(boot33) - 2])
rollingregs$Estimate[34] <- as.numeric(coef(fit34)[2]) + as.numeric(coef(fit34)[nrow(boot34) - 2])
rollingregs$Estimate[35] <- as.numeric(coef(fit35)[2]) + as.numeric(coef(fit35)[nrow(boot35) - 2])
rollingregs$Estimate[36] <- as.numeric(coef(fit36)[2]) + as.numeric(coef(fit36)[nrow(boot36) - 2])
rollingregs$Estimate[37] <- as.numeric(coef(fit37)[2]) + as.numeric(coef(fit37)[nrow(boot37) - 2])
rollingregs$Estimate[38] <- as.numeric(coef(fit38)[2]) + as.numeric(coef(fit38)[nrow(boot38) - 2])
rollingregs$Estimate[39] <- as.numeric(coef(fit39)[2]) + as.numeric(coef(fit39)[nrow(boot39) - 2])
rollingregs$Estimate[40] <- as.numeric(coef(fit40)[2]) + as.numeric(coef(fit40)[nrow(boot40) - 2])

#Standard errors
rollingregs$SE[1] <- sqrt(boot1[2, 2] + boot1[nrow(boot1) - 2, nrow(boot1) - 2] + (2 * boot1[2, nrow(boot1) - 2]))
rollingregs$SE[2] <- sqrt(boot2[2, 2] + boot2[nrow(boot2) - 2, nrow(boot2) - 2] + (2 * boot2[2, nrow(boot2) - 2]))
rollingregs$SE[3] <- sqrt(boot3[2, 2] + boot3[nrow(boot3) - 2, nrow(boot3) - 2] + (2 * boot3[2, nrow(boot3) - 2]))
rollingregs$SE[4] <- sqrt(boot4[2, 2] + boot4[nrow(boot4) - 2, nrow(boot4) - 2] + (2 * boot4[2, nrow(boot4) - 2]))
rollingregs$SE[5] <- sqrt(boot5[2, 2] + boot5[nrow(boot5) - 2, nrow(boot5) - 2] + (2 * boot5[2, nrow(boot5) - 2]))
rollingregs$SE[6] <- sqrt(boot6[2, 2] + boot6[nrow(boot6) - 2, nrow(boot6) - 2] + (2 * boot6[2, nrow(boot6) - 2]))
rollingregs$SE[7] <- sqrt(boot7[2, 2] + boot7[nrow(boot7) - 2, nrow(boot7) - 2] + (2 * boot7[2, nrow(boot7) - 2]))
rollingregs$SE[8] <- sqrt(boot8[2, 2] + boot8[nrow(boot8) - 2, nrow(boot8) - 2] + (2 * boot8[2, nrow(boot8) - 2]))
rollingregs$SE[9] <- sqrt(boot9[2, 2] + boot9[nrow(boot9) - 2, nrow(boot9) - 2] + (2 * boot9[2, nrow(boot9) - 2]))
rollingregs$SE[10] <- sqrt(boot10[2, 2] + boot10[nrow(boot10) - 2, nrow(boot10) - 2] + (2 * boot10[2, nrow(boot10) - 2]))
rollingregs$SE[11] <- sqrt(boot11[2, 2] + boot11[nrow(boot11) - 2, nrow(boot11) - 2] + (2 * boot11[2, nrow(boot11) - 2]))
rollingregs$SE[12] <- sqrt(boot12[2, 2] + boot12[nrow(boot12) - 2, nrow(boot12) - 2] + (2 * boot12[2, nrow(boot12) - 2]))
rollingregs$SE[13] <- sqrt(boot13[2, 2] + boot13[nrow(boot13) - 2, nrow(boot13) - 2] + (2 * boot13[2, nrow(boot13) - 2]))
rollingregs$SE[14] <- sqrt(boot14[2, 2] + boot14[nrow(boot14) - 2, nrow(boot14) - 2] + (2 * boot14[2, nrow(boot14) - 2]))
rollingregs$SE[15] <- sqrt(boot15[2, 2] + boot15[nrow(boot15) - 2, nrow(boot15) - 2] + (2 * boot15[2, nrow(boot15) - 2]))
rollingregs$SE[16] <- sqrt(boot16[2, 2] + boot16[nrow(boot16) - 2, nrow(boot16) - 2] + (2 * boot16[2, nrow(boot16) - 2]))
rollingregs$SE[17] <- sqrt(boot17[2, 2] + boot17[nrow(boot17) - 2, nrow(boot17) - 2] + (2 * boot17[2, nrow(boot17) - 2]))
rollingregs$SE[18] <- sqrt(boot18[2, 2] + boot18[nrow(boot18) - 2, nrow(boot18) - 2] + (2 * boot18[2, nrow(boot18) - 2]))
rollingregs$SE[19] <- sqrt(boot19[2, 2] + boot19[nrow(boot19) - 2, nrow(boot19) - 2] + (2 * boot19[2, nrow(boot19) - 2]))
rollingregs$SE[20] <- sqrt(boot20[2, 2] + boot20[nrow(boot20) - 2, nrow(boot20) - 2] + (2 * boot20[2, nrow(boot20) - 2]))
rollingregs$SE[21] <- sqrt(boot21[2, 2] + boot21[nrow(boot21) - 2, nrow(boot21) - 2] + (2 * boot21[2, nrow(boot21) - 2]))
rollingregs$SE[22] <- sqrt(boot22[2, 2] + boot22[nrow(boot22) - 2, nrow(boot22) - 2] + (2 * boot22[2, nrow(boot22) - 2]))
rollingregs$SE[23] <- sqrt(boot23[2, 2] + boot23[nrow(boot23) - 2, nrow(boot23) - 2] + (2 * boot23[2, nrow(boot23) - 2]))
rollingregs$SE[24] <- sqrt(boot24[2, 2] + boot24[nrow(boot24) - 2, nrow(boot24) - 2] + (2 * boot24[2, nrow(boot24) - 2]))
rollingregs$SE[25] <- sqrt(boot25[2, 2] + boot25[nrow(boot25) - 2, nrow(boot25) - 2] + (2 * boot25[2, nrow(boot25) - 2]))
rollingregs$SE[26] <- sqrt(boot26[2, 2] + boot26[nrow(boot26) - 2, nrow(boot26) - 2] + (2 * boot26[2, nrow(boot26) - 2]))
rollingregs$SE[27] <- sqrt(boot27[2, 2] + boot27[nrow(boot27) - 2, nrow(boot27) - 2] + (2 * boot27[2, nrow(boot27) - 2]))
rollingregs$SE[28] <- sqrt(boot28[2, 2] + boot28[nrow(boot28) - 2, nrow(boot28) - 2] + (2 * boot28[2, nrow(boot28) - 2]))
rollingregs$SE[29] <- sqrt(boot29[2, 2] + boot29[nrow(boot29) - 2, nrow(boot29) - 2] + (2 * boot29[2, nrow(boot29) - 2]))
rollingregs$SE[30] <- sqrt(boot30[2, 2] + boot30[nrow(boot30) - 2, nrow(boot30) - 2] + (2 * boot30[2, nrow(boot30) - 2]))
rollingregs$SE[31] <- sqrt(boot31[2, 2] + boot31[nrow(boot31) - 2, nrow(boot31) - 2] + (2 * boot31[2, nrow(boot31) - 2]))
rollingregs$SE[32] <- sqrt(boot32[2, 2] + boot32[nrow(boot32) - 2, nrow(boot32) - 2] + (2 * boot32[2, nrow(boot32) - 2]))
rollingregs$SE[33] <- sqrt(boot33[2, 2] + boot33[nrow(boot33) - 2, nrow(boot33) - 2] + (2 * boot33[2, nrow(boot33) - 2]))
rollingregs$SE[34] <- sqrt(boot34[2, 2] + boot34[nrow(boot34) - 2, nrow(boot34) - 2] + (2 * boot34[2, nrow(boot34) - 2]))
rollingregs$SE[35] <- sqrt(boot35[2, 2] + boot35[nrow(boot35) - 2, nrow(boot35) - 2] + (2 * boot35[2, nrow(boot35) - 2]))
rollingregs$SE[36] <- sqrt(boot36[2, 2] + boot36[nrow(boot36) - 2, nrow(boot36) - 2] + (2 * boot36[2, nrow(boot36) - 2]))
rollingregs$SE[37] <- sqrt(boot37[2, 2] + boot37[nrow(boot37) - 2, nrow(boot37) - 2] + (2 * boot37[2, nrow(boot37) - 2]))
rollingregs$SE[38] <- sqrt(boot38[2, 2] + boot38[nrow(boot38) - 2, nrow(boot38) - 2] + (2 * boot38[2, nrow(boot38) - 2]))
rollingregs$SE[39] <- sqrt(boot39[2, 2] + boot39[nrow(boot39) - 2, nrow(boot39) - 2] + (2 * boot39[2, nrow(boot39) - 2]))
rollingregs$SE[40] <- sqrt(boot40[2, 2] + boot40[nrow(boot40) - 2, nrow(boot40) - 2] + (2 * boot40[2, nrow(boot40) - 2]))

#Obtaining confidence intervals
rollingregs$Upper <- rollingregs$Estimate + (qnorm(0.975)*rollingregs$SE) #Upper 95% confidence interval
rollingregs$Lower <- rollingregs$Estimate - (qnorm(0.975)*rollingregs$SE) #Lower 95% confidence interval

#Number of observations
rollingregs$Nobs[1] <- length(fit1$fitted.values)
rollingregs$Nobs[2] <- length(fit2$fitted.values)
rollingregs$Nobs[3] <- length(fit3$fitted.values)
rollingregs$Nobs[4] <- length(fit4$fitted.values)
rollingregs$Nobs[5] <- length(fit5$fitted.values)
rollingregs$Nobs[6] <- length(fit6$fitted.values)
rollingregs$Nobs[7] <- length(fit7$fitted.values)
rollingregs$Nobs[8] <- length(fit8$fitted.values)
rollingregs$Nobs[9] <- length(fit9$fitted.values)
rollingregs$Nobs[10] <- length(fit10$fitted.values)
rollingregs$Nobs[11] <- length(fit11$fitted.values)
rollingregs$Nobs[12] <- length(fit12$fitted.values)
rollingregs$Nobs[13] <- length(fit13$fitted.values)
rollingregs$Nobs[14] <- length(fit14$fitted.values)
rollingregs$Nobs[15] <- length(fit15$fitted.values)
rollingregs$Nobs[16] <- length(fit16$fitted.values)
rollingregs$Nobs[17] <- length(fit17$fitted.values)
rollingregs$Nobs[18] <- length(fit18$fitted.values)
rollingregs$Nobs[19] <- length(fit19$fitted.values)
rollingregs$Nobs[20] <- length(fit20$fitted.values)
rollingregs$Nobs[21] <- length(fit21$fitted.values)
rollingregs$Nobs[22] <- length(fit22$fitted.values)
rollingregs$Nobs[23] <- length(fit23$fitted.values)
rollingregs$Nobs[24] <- length(fit24$fitted.values)
rollingregs$Nobs[25] <- length(fit25$fitted.values)
rollingregs$Nobs[26] <- length(fit26$fitted.values)
rollingregs$Nobs[27] <- length(fit27$fitted.values)
rollingregs$Nobs[28] <- length(fit28$fitted.values)
rollingregs$Nobs[29] <- length(fit29$fitted.values)
rollingregs$Nobs[30] <- length(fit30$fitted.values)
rollingregs$Nobs[31] <- length(fit31$fitted.values)
rollingregs$Nobs[32] <- length(fit32$fitted.values)
rollingregs$Nobs[33] <- length(fit33$fitted.values)
rollingregs$Nobs[34] <- length(fit34$fitted.values)
rollingregs$Nobs[35] <- length(fit35$fitted.values)
rollingregs$Nobs[36] <- length(fit36$fitted.values)
rollingregs$Nobs[37] <- length(fit37$fitted.values)
rollingregs$Nobs[38] <- length(fit38$fitted.values)
rollingregs$Nobs[39] <- length(fit39$fitted.values)
rollingregs$Nobs[40] <- length(fit40$fitted.values)

#Correct classification of treatment and control observations
rollingregs$Treatcorr <- rep(NA,nrow(rollingregs)) #Creating column for dataframe to indicate percentage of correct treatment classification
rollingregs$Contcorr <- rep(NA,nrow(rollingregs)) #Creating column for dataframe to indicate percentage of correct control classification

for (i in 79:118){
   temp <- responses.all[responses.all$day.nr >= i - 15 & responses.all$day.nr <= i + 14, ] #Restricting to sample used for regression
   temp$treat <- ifelse(temp$day.nr >= i, 1, 0) #Creating indicator for whether observation is in the treatment group
   temp$control <- ifelse(temp$day.nr < i, 1, 0) #Creating indicator for whether observation is in the control group
   temp$treatactual <- ifelse(temp$day.nr >= 99, 1, 0) #Creating indicator for whether observation is in the actual treatment group
   temp$contactual <- ifelse(temp$day.nr < 99, 1, 0) #Creating indicator for whether observation is in the actual control group
   temp <- temp[!is.na(temp$treat) & !is.na(temp$sunni) & !is.na(temp$other) & !is.na(temp$kurd) & 
                   !is.na(temp$female) & !is.na(temp$casualties.lag1x) & !is.na(temp$age.2) & !is.na(temp$educ.2) &
                   !is.na(temp$rural.2) & !is.na(temp$work.1) & !is.na(temp$eco.sit.bin),] #Restricts to those observations used in regression
   rollingregs[i-78,"Treatcorr"] <- (sum(temp$treatactual[temp$day.nr >= i])/sum(temp$treat))*100 #Percentage of treatment observations correctly classified
   rollingregs[i-78,"Contcorr"] <- (sum(temp$contactual[temp$day.nr < i])/sum(temp$control))*100 #Percentage of treatment observations correctly classified
}

#Creating layout for figures
Charts <- layout(matrix(c(1, 1, 2, 3), nrow = 2, ncol = 2, byrow = TRUE)) 
layout.show(Charts)

#Plot of change in Sunni attitudes from the 40 regressions
plot(rollingregs$Regnumber, rollingregs$Estimate, cex = 3, cex.axis = 2, cex.lab = 1.5,
     cex.main = 2, pch = 19, xlab = "Date", ylab = "Change in Attitude", 
     xlim = c(1,40), ylim = c(-0.5, 0.5), yaxt ='n', xaxt = 'n',
     main = "Change in Sunni Attitudes") 

abline(v = 21, lty = 2, col = 2, lwd = 4) #Adding in vertical dashed red line to indicate actual resignation date
abline(h = 0, cex = 4, lwd = 4) #Adding in horizontal zero effect line
axis(side=2, at=seq(-0.5,0.5, by = 0.1), cex.axis = 1.5) #Adding in y-axis of change in Sunni attitudes
axis(side=1, at = 1:40, labels = rollingregs$Date, cex.axis = 1.4) #Adding in x-axis of critical dates used for each regression

for (i in 1:nrow(rollingregs)){ 
   lines(c(rollingregs$Regnumber[i], rollingregs$Regnumber[i]), c(rollingregs$Lower[i],rollingregs$Upper[i]), lwd = 4) #Adding in confidence intervals
}

#Plot to show number of observations for each of the 40 regressions
plot(rollingregs$Regnumber, rollingregs$Nobs, lty = 1, cex = 3, cex.axis = 2, cex.lab = 1.5,
     cex.main = 2, type = "o", xlab = "Date", ylab = "Number",  
     xlim = c(1,40), ylim= c(0,3000), yaxt ='n', xaxt = 'n',
     main = "Number of Observations")

abline(v = 21, lty = 2, col = 2, lwd = 4) #Adding in vertical dashed red line to indicate actual resignation date
axis(side=2, at=seq(0, 3000, by = 500), cex.axis = 1.25) #Adding in y-axis of number of observations
axis(side=1, at=1:40, labels = rollingregs$Date, cex.axis = 1) #Adding in x-axis of critical dates used for each regression

#Plot to show percentage of observations correctly classified for each of the 40 regressions
plot(rollingregs$Regnumber, rollingregs$Treatcorr, lty = 1, cex = 1.5, cex.axis = 2, cex.lab = 1.5, pch = 0,
     cex.main = 2, type = "o", xlab = "Date", ylab = "Percent", 
     xlim = c(1,40), ylim= c(0,100), yaxt = 'n', xaxt ='n',
     main = "Percent Correctly Classified") #Main plot

lines(rollingregs$Contcorr, pch = 2, lty = 1, cex = 1.5, type = "o")
abline(v = 21, lty = 2, col = 2, lwd = 4) #Adding in vertical dashed red line to indicate actual resignation date
axis(side=2, at=seq(0, 100, by = 20), cex.axis = 1.25) #Adding in y-axis of percentage of observations correctly classified
axis(side=1, at=1:40, labels = rollingregs$Date, cex.axis = 1) #Adding in x-axis of critical dates used for each regression
legend(-2, 80, legend = c("Treatment", "Control"), pch = c(0, 2), x.intersp = 0.3, y.intersp = 0.3 , bty = "n", cex = 1.5)

#Save as jpeg with width = 1450 and height = 1000

###############################
### FIGURES IN THE APPENDIX ###
###############################

######################################################################################################################################
### APPENDIX FIGURE 1: Estimated change in Sunni and Shiite attitudes after al-Maliki’s announced resignation (in standard deviations)

#####Please run Models 1 to 7 in Dofile-analyses.R first

##Sympathy with armed opposition and government's performance legitimacy (top panel)

#Standard deviations by dependent variable
sd.symp <- sd(responses$armedopp.symp.bin, na.rm = TRUE) #DV: Sympathy with armed opposition
sd.gov <- sd(responses$gov.rating.good, na.rm = TRUE) #DV: Rating of federal government
sd.sec <- sd(responses$sec.improve, na.rm = TRUE) #DV: Likely security improvement
sd.jobs <- sd(responses$jobs.improve, na.rm = TRUE) #DV: Likely jobs improvement
sd.elec <- sd(responses$elec.improve, na.rm = TRUE) #DV: Likely electricity improvement

#Estimate for change in attitudes of Sunni Arabs by dependent variable
mt.fig2.sunnisympsd <- (as.numeric(coef(fit1)[2]) + as.numeric(coef(fit1)[nrow(boot1) - 2]))/sd.symp #DV: Sympathy with armed opposition
mt.fig2.sunnigovsd <- (as.numeric(coef(fit2)[2]) + as.numeric(coef(fit2)[nrow(boot2) - 2]))/sd.gov #DV: Rating of federal government
mt.fig2.sunnisecsd <- (as.numeric(coef(fit3)[2]) + as.numeric(coef(fit3)[nrow(boot3) - 2]))/sd.sec #DV: Likely security improvement
mt.fig2.sunnijobssd <- (as.numeric(coef(fit4)[2]) + as.numeric(coef(fit4)[nrow(boot4) - 2]))/sd.jobs #DV: Likely jobs improvement
mt.fig2.sunnielecsd <- (as.numeric(coef(fit5)[2]) + as.numeric(coef(fit5)[nrow(boot5) - 2]))/sd.elec #DV: Likely electricity improvement

#Estimate for change in attitudes of Shia Arabs by dependent variable
mt.fig2.shiasympsd <- as.numeric(coef(fit1)[2])/sd.symp #DV: Sympathy with armed opposition
mt.fig2.shiagovsd <- as.numeric(coef(fit2)[2])/sd.gov #DV: Rating of federal government
mt.fig2.shiasecsd <- as.numeric(coef(fit3)[2])/sd.sec #DV: Likely security improvement
mt.fig2.shiajobssd <- as.numeric(coef(fit4)[2])/sd.jobs #DV: Likely jobs improvement
mt.fig2.shiaelecsd <- as.numeric(coef(fit5)[2])/sd.elec #DV: Likely electricity improvement

#Standard error for change in attitudes of Sunni Arabs by dependent variable
mt.fig2.sunnisympse <-  sqrt(boot1[2, 2] + boot1[nrow(boot1) - 2, nrow(boot1) - 2] + (2 * boot1[2, nrow(boot1) - 2])) #DV: Sympathy with armed opposition
mt.fig2.sunnigovse <-  sqrt(boot2[2, 2] + boot2[nrow(boot2) - 2, nrow(boot2) - 2] + (2 * boot2[2, nrow(boot2) - 2])) #DV: Rating of federal government
mt.fig2.sunnisecse <-  sqrt(boot3[2, 2] + boot3[nrow(boot3) - 2, nrow(boot3) - 2] + (2 * boot3[2, nrow(boot3) - 2])) #DV: Likely security improvement
mt.fig2.sunnijobsse <- sqrt(boot4[2, 2] + boot4[nrow(boot4) - 2, nrow(boot4) - 2] + (2 * boot4[2, nrow(boot4) - 2])) #DV: Likely jobs improvement
mt.fig2.sunnielecse <- sqrt(boot5[2, 2] + boot5[nrow(boot5) - 2, nrow(boot5) - 2] + (2 * boot5[2, nrow(boot5) - 2])) #DV: Likely electricity improvement

#Standard error for change in attitudes of Shia Arabs by dependent variable
mt.fig2.shiasympse <- sqrt(boot1[2, 2]) #DV: Sympathy with armed opposition
mt.fig2.shiagovse <-  sqrt(boot2[2, 2]) #DV: Rating of federal government
mt.fig2.shiasecse <-  sqrt(boot3[2, 2]) #DV: Likely security improvement
mt.fig2.shiajobsse <- sqrt(boot4[2, 2]) #DV: Likely jobs improvement
mt.fig2.shiaelecse <- sqrt(boot5[2, 2]) #DV: Likely electricity improvement

#Creating chart
par(mar=c(3, 7, 9.5, 4))
plot(1, type="n", xaxt = 'n', yaxt = 'n', xlab="", ylab="", xlim=c(1,16), ylim=c(-1, 1))
box(lwd = 3)

#Adding estimates to chart for DV: Sympathy with armed opposition
points(x = 1, y = mt.fig2.sunnisympsd, pch = 19, cex = 4.15, col = "red") #Sunni estimate
points(x = 2, y = mt.fig2.shiasympsd, pch = 19, cex = 4.15) #Shia estimate

#Adding estimates to chart for DV: Rating of federal government
points(x = 4.5, y = mt.fig2.sunnigovsd, pch = 19, cex = 4.15, col = "red") #Sunni estimate
points(x = 5.5, y = mt.fig2.shiagovsd, pch = 19, cex = 4.15) #Shia estimate

#Adding estimates to chart for DV: Likely security improvement
points(x = 8, y = mt.fig2.sunnisecsd, pch = 19, cex = 4.15, col = "red") #Sunni estimate
points(x = 9, y = mt.fig2.shiasecsd, pch = 19, cex = 4.15) #Shia estimate

#Adding estimates to chart for DV: Likely jobs improvement
points(x = 11.5, y = mt.fig2.sunnijobssd, pch = 19, cex = 4.15, col = "red") #Sunni estimate
points(x = 12.5, y = mt.fig2.shiajobssd, pch = 19, cex = 4.15) #Shia estimate

#Adding estimates to chart for DV: Likely electricity improvement
points(x = 15, y = mt.fig2.sunnielecsd, pch = 19, cex = 4.15, col = "red") #Sunni estimate
points(x = 16, y = mt.fig2.shiaelecsd, pch = 19, cex = 4.15) #Shia estimate

#Adding zero effect line
abline(h = 0, lty = 3, lwd = 4)

#Adding 95% confidence intervals to chart for DV: Sympathy with armed opposition
lines(x = c(1, 1), y = c(mt.fig2.sunnisympsd - ((qnorm(0.975)*mt.fig2.sunnisympse)/sd.symp), mt.fig2.sunnisympsd + ((qnorm(0.975)*mt.fig2.sunnisympse)/sd.symp)), lwd = 4, col = "red") #Sunni confidence interval
lines(x = c(2, 2), y = c(mt.fig2.shiasympsd - ((qnorm(0.975)*mt.fig2.shiasympse)/sd.symp), mt.fig2.shiasympsd + ((qnorm(0.975)*mt.fig2.shiasympse)/sd.symp)), lwd = 4) #Shia confidence interval

#Adding 95% confidence intervals to chart for DV: Rating of federal government
lines(x = c(4.5, 4.5), y = c(mt.fig2.sunnigovsd - ((qnorm(0.975)*mt.fig2.sunnigovse)/sd.gov), mt.fig2.sunnigovsd + ((qnorm(0.975)*mt.fig2.sunnigovse)/sd.gov)), lwd = 4, col = "red") #Sunni confidence interval
lines(x = c(5.5, 5.5), y = c(mt.fig2.shiagovsd - ((qnorm(0.975)*mt.fig2.shiagovse)/sd.gov), mt.fig2.shiagovsd + ((qnorm(0.975)*mt.fig2.shiagovse)/sd.gov)), lwd = 4) #Shia confidence interval

#Adding 95% confidence intervals to chart for DV: Likely security improvement
lines(x = c(8, 8), y = c(mt.fig2.sunnisecsd - ((qnorm(0.975)*mt.fig2.sunnisecse)/sd.sec), mt.fig2.sunnisecsd + ((qnorm(0.975)*mt.fig2.sunnisecse)/sd.sec)), lwd = 4, col = "red") #Sunni confidence interval
lines(x = c(9, 9), y = c(mt.fig2.shiasecsd - ((qnorm(0.975)*mt.fig2.shiasecse)/sd.sec), mt.fig2.shiasecsd + ((qnorm(0.975)*mt.fig2.shiasecse)/sd.sec)), lwd = 4) #Shia confidence interval

#Adding 95% confidence intervals to chart for DV: Likely jobs improvement
lines(x = c(11.5, 11.5), y = c(mt.fig2.sunnijobssd - ((qnorm(0.975)*mt.fig2.sunnijobsse)/sd.jobs), mt.fig2.sunnijobssd + ((qnorm(0.975)*mt.fig2.sunnijobsse)/sd.jobs)), lwd = 4, col = "red") #Sunni confidence interval
lines(x = c(12.5, 12.5), y = c(mt.fig2.shiajobssd - ((qnorm(0.975)*mt.fig2.shiajobsse)/sd.jobs), mt.fig2.shiajobssd + ((qnorm(0.975)*mt.fig2.shiajobsse)/sd.jobs)), lwd = 4) #Shia confidence interval

#Adding 95% confidence intervals to chart for DV: Likely electricity improvement
lines(x = c(15, 15), y = c(mt.fig2.sunnielecsd - ((qnorm(0.975)*mt.fig2.sunnielecse)/sd.elec), mt.fig2.sunnielecsd + ((qnorm(0.975)*mt.fig2.sunnielecse)/sd.elec)), lwd = 4, col = "red") #Sunni confidence interval
lines(x = c(16, 16), y = c(mt.fig2.shiaelecsd - ((qnorm(0.975)*mt.fig2.shiaelecse)/sd.elec), mt.fig2.shiaelecsd + ((qnorm(0.975)*mt.fig2.shiaelecse)/sd.elec)), lwd = 4) #Shia confidence interval

#Labels for chart
xlabel <- c("Sunni  ")
xlabel.2 <- c("  Shia")
xlabel.3 <- c("Sympathy w.", "Rating of", "Security", "Jobs", "Electricity")
xlabel.4 <- c("armed opp.", "government", "improves", "improve", "improves")
ytickmarks <- seq(-1, 1, 0.2)
ylabels <- seq(-1, 1, 0.2)

#Adding labels and titles
mtext(side = 1, xlabel, at = c(1.1, 4.5, 8, 11.5, 15), cex = 3, line = 1.9, col = "red") #x-axis Sunni label below the plot
mtext(side = 1, xlabel.2, at = c(2.1, 5.5, 9, 12.5, 16), cex = 3, line = 1.9) #x-axis Shia label below the plot
mtext(side = 3, xlabel.3, at = c(1.8, 5, 8.5, 12, 15.5), cex = 3,font = 2, line = 3) #x-axis title above the plot
mtext(side = 3, xlabel.4, at = c(1.8, 5, 8.5, 12, 15.5), cex = 3,font = 2, line = 1) #x-axis title above the plot
axis(side = 2, at = ytickmarks, labels = NA, cex.axis = 3, lwd = 4, lwd.ticks = 4, line = 0) #y-axis
mtext(ylabels, side = 2, at = ylabels, cex = 3, line = 1) #y-axis labels
mtext("Change in attitude (in standard deviations of the DV)", side = 2, cex = 3, line = 4) #y-axis title

#Save as jpeg with width = 2000 & height = 1500#

##Government's democratic legitimacy (bottom panel)

#Standard deviations by dependent variable
sd.infl <- sd(responses.all$your.influence.gov, na.rm = TRUE) #DV: Influence government decisions
sd.dem <- sd(responses.all$sit.dem.good, na.rm = TRUE) #DV: Situation of democracy

#Estimate for change in attitudes of Sunni Arabs by dependent variable
mt.fig2.sunniinflsd <- (as.numeric(coef(fit6)[2]) + as.numeric(coef(fit6)[nrow(boot6) - 2]))/sd.infl #DV: Influence government decisions
mt.fig2.sunnidemsd <- (as.numeric(coef(fit7)[2]) + as.numeric(coef(fit7)[nrow(boot7) - 2]))/sd.dem #DV: Situation of democracy

#Estimate for change in attitudes of Shia Arabs by dependent variable
mt.fig2.shiainflsd <- (as.numeric(coef(fit6)[2]))/sd.infl #DV: Influence of government decisions
mt.fig2.shiademsd <- (as.numeric(coef(fit7)[2]))/sd.dem #DV: Situation of democracy

#Standard error for change in attitudes of Sunni Arabs by dependent variable
mt.fig2.sunniinflse <-  sqrt(boot6[2, 2] + boot6[nrow(boot6) - 2, nrow(boot6) - 2] + (2 * boot6[2, nrow(boot6) - 2])) #DV: Influence of government decisions
mt.fig2.sunnidemse <-  sqrt(boot7[2, 2] + boot7[nrow(boot7) - 2, nrow(boot7) - 2] + (2 * boot7[2, nrow(boot7) - 2])) #DV: Situation of democracy

#Standard error for change in attitudes of Shia Arabs by dependent variable
mt.fig2.shiainflse <- sqrt(boot6[2, 2]) #DV: Influence of government decisions
mt.fig2.shiademse <-  sqrt(boot7[2, 2]) #DV: Situation of democracy

#Creating chart
par(mar=c(3, 7, 9.5, 4))
plot(1, type="n", xaxt = 'n', yaxt = 'n', xlab="", ylab="", xlim=c(.25, 4.75), ylim=c(-1, 1))
box(lwd = 3)

#Adding estimates to chart for DV: Influence of government decisions
points(x = 1, y = mt.fig2.sunniinflsd, pch = 19, cex = 4.15, col = "red") #Sunni estimate
points(x = 1.75, y = mt.fig2.shiainflsd, pch = 19, cex = 4.15) #Shia estimate

#Adding estimates to chart for DV: Situation of democracy
points(x = 3.25, y = mt.fig2.sunnidemsd, pch = 19, cex = 4.15, col = "red") #Sunni estimate
points(x = 4, y = mt.fig2.shiademsd, pch = 19, cex = 4.15) #Shia estimate

#Adding zero effect line to chart
abline(h = 0, lty = 3, lwd = 4)

#Adding 95% confidence intervals to chart for DV: Influence of government decisions
lines(x = c(1, 1), y = c(mt.fig2.sunniinflsd - ((qnorm(0.975)*mt.fig2.sunniinflse)/sd.infl), mt.fig2.sunniinflsd + ((qnorm(0.975)*mt.fig2.sunniinflse)/sd.infl)), lwd = 4, col = "red") #Sunni confidence interval
lines(x = c(1.75, 1.75), y = c(mt.fig2.shiainflsd - ((qnorm(0.975)*mt.fig2.shiainflse)/sd.infl), mt.fig2.shiainflsd + ((qnorm(0.975)*mt.fig2.shiainflse)/sd.infl)), lwd = 4) #Shia confidence interval

#Adding 95% confidence intervals to chart for DV: Situation of democracy
lines(x = c(3.25, 3.25), y = c(mt.fig2.sunnidemsd - ((qnorm(0.975)*mt.fig2.sunnidemse)/sd.dem), mt.fig2.sunnidemsd + ((qnorm(0.975)*mt.fig2.sunnidemse)/sd.dem)), lwd = 4, col = "red") #Sunni confidence interval
lines(x = c(4, 4), y = c(mt.fig2.shiademsd - ((qnorm(0.975)*mt.fig2.shiademse)/sd.dem), mt.fig2.shiademsd + ((qnorm(0.975)*mt.fig2.shiademse)/sd.dem)), lwd = 4) #Shia confidence interval

#Labels for chart
xlabel <- c("Sunni  ")
xlabel.2 <- c("  Shia")
xlabel.3 <- c("Influence government", "Situation of")
xlabel.4 <- c("decisions", "democracy")
ytickmarks <- seq(-1, 1, 0.2)
ylabels <- seq(-1, 1, 0.2)

#Adding labels and titles
mtext(side = 1, xlabel, at = c(1, 3.25), cex = 3, line = 1.9, col = "red") # x-axis Sunni label below the plot
mtext(side = 1, xlabel.2, at = c(1.75, 4), cex = 3, line = 1.9) # x-axis Shia label below the plot
mtext(side = 3, xlabel.3, at = c(1.38, 3.63), cex = 3,font = 2, line = 3) # x-axis title above the plot
mtext(side = 3, xlabel.4, at = c(1.38, 3.63), cex = 3,font = 2, line = 1) # x axis title above the plot
axis(side = 2, at = ytickmarks, labels = NA, cex.axis = 3, lwd = 4, lwd.ticks = 4, line = 0) # y-axis
mtext(ylabels, side = 2, at = ylabels, cex = 3, line = 1) # y-axis labels
mtext("Change in attitude (in standard deviations of the DV)", side = 2, cex = 3, line = 4) # y-axis title

#Save as jpeg with width = 2000 & height = 1500#

#####################################################################
### APPENDIX FIGURE 2: Sunni respondents’ average responses over time

###Trends in Sunni Muslim Attitudes

#Selecting regions used in regression analysis
responses$regionr <- ifelse(responses$gov.fe == 1 | responses$gov.fe == 3
                          | responses$gov.fe == 4 | responses$gov.fe == 5
                          | responses$gov.fe == 6 | responses$gov.fe == 7
                          | responses$gov.fe == 9 | responses$gov.fe == 10
                          | responses$gov.fe == 11 | responses$gov.fe == 13
                          | responses$gov.fe == 14 | responses$gov.fe == 16
                          | responses$gov.fe == 17,
                          1, 0)

##DV: Sympathy with armed opposition

#Choosing sample: Sunni Arabs who provided a response to the question in one of the selected regions
sympathy <- responses[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$regionr == 1,]

#Creating trends data frame
symptrends <- as.data.frame(matrix(NA, nrow = 31, ncol = 1)) #Dataframe
symptrends$average <- rep(NA, nrow(symptrends)) #Average response
symptrends$number <- rep(NA, nrow(symptrends)) #Number of responses
symptrends$index <- seq(1,31,1) #Value for each row in dataframe

#Loop to obtain average responses and number of responses over 5 days before August 14
startaft <- 1 #Start day
endaft <- c(5,10) #End day
for (i in endaft){
   sample <- sympathy[sympathy$day <= i & sympathy$day >= startaft,] #Subsetting data to required date period
   symptrends$average[i] <- mean(sample$armedopp.symp.bin) #Average response
   symptrends$number[i] <- nrow(sample) #Number of responses
   startaft <- i+1
}

#Loop to obtain average responses and number of responses over 5 days on or after August 14
startaft <- 14 #Start day
endaft <- c(20, 25, 30) #End day
for (i in endaft){
   sample <- sympathy[sympathy$day <= i & sympathy$day >= startaft,] #Subsetting data to required date period
   symptrends$average[i] <- mean(sample$armedopp.symp.bin) #Average response
   symptrends$number[i] <- nrow(sample) #Number of responses
   startaft <- i+1
}

#Loop to create date variable in trends data frame
j <- 1 #First day
for (i in 1:nrow(symptrends)){
   symptrends$date[i] <- paste(8, "/", j) #Creating date variable
   j <- j +1
}

#Creating plot of averages where size of point reflects relative number of observations
plot(symptrends$index, symptrends$average, cex = symptrends$number/nrow(sympathy)*10, xaxt = "n", yaxt = "n", xlab = "Date", ylab = "Average response", ylim = c(0, 1),
     main = "Trends in Sunni Muslim Attitudes:\nSympathy for Armed Opposition Groups")
abline(v=14, col = "red") #Horizontal line indicating August 14
axis(side=2, at=seq(0,1, by = 0.2)) #Adding y-axis values
axis(side=1, at = 1:31, labels = symptrends$date) #Adding dates to x-axis

#Adding averages before and after August 14 to plot
lines(c(-1,14),c(mean(sympathy$armedopp.symp.bin[sympathy$day < 14]),mean(sympathy$armedopp.symp.bin[sympathy$day < 14])), lty = 2) #Average response before August 14
lines(c(14,32),c(mean(sympathy$armedopp.symp.bin[sympathy$day >= 14]),mean(sympathy$armedopp.symp.bin[sympathy$day >= 14])), lty = 2) #Average response after August 14


##DV: Rating of federal government

#Choosing sample: Sunni Arabs who provided a response to the question in one of the selected regions
government <- responses[is.na(responses$gov.rating.good) == FALSE & responses$sunni == 1 & responses$regionr == 1,]

#Creating trends data frame
gov.rating.goodtrends <- as.data.frame(matrix(NA, nrow = 31, ncol = 1)) #Dataframe
gov.rating.goodtrends$average <- rep(NA, nrow(gov.rating.goodtrends)) #Average response
gov.rating.goodtrends$number <- rep(NA, nrow(gov.rating.goodtrends)) #Number of responses
gov.rating.goodtrends$index <- seq(1,31,1) #Value for each row in dataframe

#Loop to obtain average responses and number of responses over 5 days before August 14
startbef <- 1 #Start day
endbef <- c(5,10) #End day
for (i in endbef){
   sample <- government[government$day <= i & government$day >= startbef,] #Subsetting data to required date period
   gov.rating.goodtrends$average[i] <- mean(sample$gov.rating.good) #Average response
   gov.rating.goodtrends$number[i] <- nrow(sample) #Number of responses
   startbef <- i+1
}

#Loop to obtain average responses and number of responses over 5 days on or after August 14
startaft <- 14 #Start day
endaft <- c(20, 25, 30) #End day
for (i in endaft){
   sample <- government[government$day <= i & government$day >= startaft,] #Subsetting data to required date period
   gov.rating.goodtrends$average[i] <- mean(sample$gov.rating.good) #Average response
   gov.rating.goodtrends$number[i] <- nrow(sample) #Number of responses
   startaft <- i+1
}

#Loop to create date variable in trends data frame
j <- 1 #First day
for (i in 1:nrow(gov.rating.goodtrends)){
   gov.rating.goodtrends$date[i] <- paste(8, "/", j) #Creating date variable
   j <- j +1
}

#Creating plot of averages where size of point reflects relative number of observations
plot(gov.rating.goodtrends$index, gov.rating.goodtrends$average, cex = gov.rating.goodtrends$number/nrow(government)*10, xaxt = "n", yaxt = "n", xlab = "Date", ylab = "Average response", ylim = c(2.4, 2.6),
     main = "Trends in Sunni Muslim Attitudes:\nRating of Federal Government")
abline(v=14, col = "red") #Horizontal line indicating August 14
axis(side=2, at=seq(2.4,2.6, by = 0.05)) #Adding y-axis values
axis(side=1, at = 1:31, labels = gov.rating.goodtrends$date) #Adding dates to x-axis

#Adding averages before and after August 14 to plot
lines(c(-1,14),c(mean(government$gov.rating.good[government$day < 14]),mean(government$gov.rating.good[government$day < 14])), lty = 2) #Average response before August 14
lines(c(14,32),c(mean(government$gov.rating.good[government$day >= 14]),mean(government$gov.rating.good[government$day >= 14])), lty = 2) #Average response after August 14


##DV: Likely security improvement

#Choosing sample: Sunni Arabs who provided a response to the question in one of the selected regions
security <- responses[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$regionr == 1,]

#Creating trends data frame
sec.improvetrends <- as.data.frame(matrix(NA, nrow = 31, ncol = 1)) #Dataframe
sec.improvetrends$average <- rep(NA, nrow(sec.improvetrends)) #Average response
sec.improvetrends$number <- rep(NA, nrow(sec.improvetrends)) #Number of responses
sec.improvetrends$index <- seq(1,31,1) #Value for each row in dataframe

#Loop to obtain average responses and number of responses over 5 days before August 14
startbef <- 1 #Start day
endbef <- c(5,10) #End day
for (i in endbef){
   sample <- security[security$day <= i & security$day >= startbef,] #Subsetting data to required date period
   sec.improvetrends$average[i] <- mean(sample$sec.improve) #Average response
   sec.improvetrends$number[i] <- nrow(sample) #Number of responses
   startbef <- i+1
}

#Loop to obtain average responses and number of responses over 5 days on or after August 14
startaft <- 14 #Start day
endaft <- c(20, 25, 30) #End day
for (i in endaft){
   sample <- security[security$day <= i & security$day >= startaft,] #Subsetting data to required date period
   sec.improvetrends$average[i] <- mean(sample$sec.improve) #Average response
   sec.improvetrends$number[i] <- nrow(sample) #Number of responses
   startaft <- i+1
}

#Loop to create date variable in trends data frame
j <- 1 #First day
for (i in 1:nrow(sec.improvetrends)){
   sec.improvetrends$date[i] <- paste(8, "/", j) #Creating date variable
   j <- j +1
}

#Creating plot of averages where size of point reflects relative number of observations
plot(sec.improvetrends$index, sec.improvetrends$average, cex = sec.improvetrends$number/nrow(security)*10, xaxt = "n", yaxt = "n", xlab = "Date", ylab = "Average response", ylim = c(1.8, 2.7),
     main = "Trends in Sunni Muslim Attitudes:\nExpectations of Future Security Provision")
abline(v=14, col = "red") #Horizontal line indicating August 14
axis(side=2, at=seq(1.8,2.7, by = 0.1)) #Adding y-axis values
axis(side=1, at = 1:31, labels = sec.improvetrends$date) #Adding dates to x-axis

#Adding averages before and after August 14 to plot
lines(c(-1,14),c(mean(security$sec.improve[security$day < 14]),mean(security$sec.improve[security$day < 14])), lty = 2) #Average response before August 14
lines(c(14,32),c(mean(security$sec.improve[security$day >= 14]),mean(security$sec.improve[security$day >= 14])), lty = 2) #Average response after August 14


##DV: Likely jobs improvement

#Choosing sample: Sunni Arabs who provided a response to the question in one of the selected regions
jobs <- responses[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$regionr == 1,]

#Creating trends data frame
jobs.improvetrends <- as.data.frame(matrix(NA, nrow = 31, ncol = 1)) #Dataframe
jobs.improvetrends$average <- rep(NA, nrow(jobs.improvetrends)) #Average response
jobs.improvetrends$number <- rep(NA, nrow(jobs.improvetrends)) #Number of responses
jobs.improvetrends$index <- seq(1,31,1) #Value for each row in dataframe

#Loop to obtain average responses and number of responses over 5 days before August 14
startbef <- 1 #Start day
endbef <- c(5,10) #End day
for (i in endbef){
   sample <- jobs[jobs$day <= i & jobs$day >= startbef,] #Subsetting data to required date period
   jobs.improvetrends$average[i] <- mean(sample$jobs.improve) #Average response
   jobs.improvetrends$number[i] <- nrow(sample) #Number of responses
   startbef <- i+1
}

#Loop to obtain average responses and number of responses over 5 days on or after August 14
startaft <- 14 #Start day
endaft <- c(20, 25, 30) #End day
for (i in endaft){
   sample <- jobs[jobs$day <= i & jobs$day >= startaft,] #Subsetting data to required date period
   jobs.improvetrends$average[i] <- mean(sample$jobs.improve) #Average response
   jobs.improvetrends$number[i] <- nrow(sample) #Number of responses
   startaft <- i+1
}

#Loop to create date variable in trends data frame
j <- 1 #First day
for (i in 1:nrow(jobs.improvetrends)){
   jobs.improvetrends$date[i] <- paste(8, "/", j) #Creating date variable
   j <- j +1
}

#Creating plot of averages where size of point reflects relative number of observations
plot(jobs.improvetrends$index, jobs.improvetrends$average, cex = jobs.improvetrends$number/nrow(jobs)*10, xaxt = "n", yaxt = "n", xlab = "Date", ylab = "Average response", ylim = c(2.08, 2.4),
     main = "Trends in Sunni Muslim Attitudes:\nExpectations of Future Job Provision")
abline(v=14, col = "red") #Horizontal line indicating August 14
axis(side=2, at=seq(2.1,2.4, by = 0.05)) #Adding y-axis values
axis(side=1, at = 1:31, labels = jobs.improvetrends$date) #Adding dates to x-axis

#Adding averages before and after August 14 to plot
lines(c(-1,14),c(mean(jobs$jobs.improve[jobs$day < 14]),mean(jobs$jobs.improve[jobs$day < 14])), lty = 2) #Average response before August 14
lines(c(14,32),c(mean(jobs$jobs.improve[jobs$day >= 14]),mean(jobs$jobs.improve[jobs$day >= 14])), lty = 2) #Average response after August 14


#DV: Likely electricity improvement

#Choosing sample: Sunni Arabs who provided a response to the question in one of the selected regions
electricity <- responses[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$regionr == 1,]

#Creating trends data frame
elec.improvetrends <- as.data.frame(matrix(NA, nrow = 31, ncol = 1)) #Dataframe
elec.improvetrends$average <- rep(NA, nrow(elec.improvetrends)) #Average response
elec.improvetrends$number <- rep(NA, nrow(elec.improvetrends)) #Number of responses
elec.improvetrends$index <- seq(1,31,1) #Value for each row in dataframe

#Loop to obtain average responses and number of responses over 5 days before August 14
startbef <- 1 #Start day
endbef <- c(5,10) #End day
for (i in endbef){
   sample <- electricity[electricity$day <= i & electricity$day >= startbef,] #Subsetting data to required date period
   elec.improvetrends$average[i] <- mean(sample$elec.improve) #Average response
   elec.improvetrends$number[i] <- nrow(sample) #Number of responses
   startbef <- i+1
}

#Loop to obtain average responses and number of responses over 5 days on or after August 14
startaft <- 14 #Start day
endaft <- c(20, 25, 30) #End day
for (i in endaft){
   sample <- electricity[electricity$day <= i & electricity$day >= startaft,] #Subsetting data to required date period
   elec.improvetrends$average[i] <- mean(sample$elec.improve) #Average response
   elec.improvetrends$number[i] <- nrow(sample) #Number of responses
   startaft <- i+1
}

#Loop to create date variable in trends data frame
j <- 1 #First day
for (i in 1:nrow(elec.improvetrends)){
   elec.improvetrends$date[i] <- paste(8, "/", j) #Creating date variable
   j <- j +1
}

#Creating plot of averages where size of point reflects relative number of observations
plot(elec.improvetrends$index, elec.improvetrends$average, cex = elec.improvetrends$number/nrow(electricity)*10, xaxt = "n", yaxt = "n", xlab = "Date", ylab = "Average response", ylim = c(2.3, 3.3),
     main = "Trends in Sunni Muslim Attitudes:\nExpectations of Future Electricity Provision")
abline(v=14, col = "red") #Horizontal line indicating August 14
axis(side=2, at=seq(2.3,3.3, by = 0.1)) #Adding y-axis values
axis(side=1, at = 1:31, labels = elec.improvetrends$date) #Adding dates to x-axis

#Adding averages before and after August 14 to plot
lines(c(-1,14),c(mean(electricity$elec.improve[electricity$day < 14]),mean(electricity$elec.improve[electricity$day < 14])), lty = 2) #Average response before August 14
lines(c(14,32),c(mean(electricity$elec.improve[electricity$day >= 14]),mean(electricity$elec.improve[electricity$day >= 14])), lty = 2) #Average response after August 14
