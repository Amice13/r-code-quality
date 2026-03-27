#### mediation script                        
#load libraries
library(mediation)
set.seed(2021)

################################################################################
## Mediation analysis

main1 <- read.csv("main.csv")  ## input data

## Subset and clean data for mediation analysis
keep <- subset(main1, select = c("mod1","mod2","mod3","female","income",
                                 "hawkish","education", "age1","party",
                                 "country","what","issue","appb","id",
                                 "pun2b", "ethnic"))

keep$democrat <- ifelse(keep$party == 2, 1, 0)  # create democrat dummy
keep$republican <- ifelse(keep$party == 3, 1, 0)  # create republican dummy
keep$college <- ifelse(keep$education >= 3, 1, 0)  # create college dummy

main2 <- keep[complete.cases(keep),]
main2$recent <- ifelse(main2$what == 5, 1, 0)  # focus on recent whataboutism
main3 <- main2[!(main2$what==2 | main2$what==3 | main2$what==4),] 


## Running Mediation Analysis 

#############################
### Figure 5 in main text ###
#############################

## Moral Equivalence Mediator
  #approval
med.m1 <- lm(mod1 ~ recent + age1 + democrat + republican + female + 
                    income + hawkish + college + country + ethnic, data = main3)
out.m1 <- glm(appb ~ mod1 + recent + age1 + democrat + republican + female + 
                     income + hawkish + college + country + ethnic, 
                     data = main3, family = binomial("probit"))
med.mod1 <- mediate(med.m1, out.m1, treat = "recent", 
                           mediator = "mod1", robustSE = TRUE, sims = 99)

## US Global Police
#approval
med.m2 <- lm(mod2 ~ recent + age1 + democrat + republican + female + 
               income + hawkish + college + country + 
               ethnic, data = main3)
out.m2 <- glm(appb ~ mod2 + recent + age1 + democrat + republican + female + 
               income + hawkish + college + country + 
               ethnic, data = main3, family = binomial("probit"))
med.mod2 <- mediate(med.m2, out.m2, treat = "recent", 
                    mediator = "mod2",  robustSE = TRUE, sims = 99)

## US Credibility
#approval
med.m3 <- lm(mod3 ~ recent + age1 + democrat + republican + female + 
               income + hawkish + college + country + 
               ethnic, data = main3)
out.m3 <- glm(appb ~ mod3 + recent + age1 + democrat + republican + female + 
               income + hawkish + college + country + 
                ethnic, data = main3, family = binomial("probit"))
med.mod3 <- mediate(med.m3, out.m3, treat = "recent", 
                    mediator = "mod3", robustSE = TRUE, sims = 99)

## Plotting mediator results for Figure 5 in main text -- see generated pdf
pdf(file = "Figure5.pdf", width = 8, height = 3)  # saving pdf plot
## Approval
windowsFonts(Times = windowsFont("Times New Roman"))  # Setting font
par(mfrow = c(1,3))

## Approval mediators
plot(med.mod1, xlab="Effect", xlim = c(-.25, 0.05), lwd = 1, cex=1,
     treatment = "treated", col="black", family="Times", 
     main="(a) Moral Equivalence")   
plot(med.mod2, xlab="Effect", xlim = c(-.25, 0.05), lwd = 1, cex=1,     
     treatment = "treated", col="black", family="Times",
     main="(b) US Unilateralism")
plot(med.mod3, xlab="Effect", xlim = c(-.25, 0.05), lwd = 1, cex=1,
     treatment = "treated", col="black", family="Times", 
     main="(c) US Credibility")

dev.off()

## Summary statistics on mediation results to reference

# approval mediators combined
summary(med.mod1)
summary(med.mod2)
summary(med.mod3)

##############################
### Figure A25 in appendix ###
##############################

## Moral Equivalence Mediator
  # punishment
med.s1 <- lm(mod1 ~ recent + age1 + democrat + republican + female + 
               income + hawkish + college + country + ethnic, data = main3)
out.s1 <- glm(pun2b ~ mod1 + recent + age1 + democrat + republican + female + 
                income + hawkish + college + country + 
                ethnic, data = main3, family = binomial("probit"))
med.mod1s <- mediate(med.s1, out.s1, treat = "recent", 
                     mediator = "mod1", robustSE = TRUE, sims = 99)

## US Global Police
  # punishment
med.s2 <- lm(mod2 ~ recent + age1 + democrat + republican + female + 
               income + hawkish + college + country + ethnic, data = main3)
out.s2 <- glm(pun2b ~ mod2 + recent + age1 + democrat + republican + female + 
                income + hawkish + college + country + 
                ethnic, data = main3, family = binomial("probit"))
med.mod2s <- mediate(med.s2, out.s2, treat = "recent", 
                     mediator = "mod2", robustSE = TRUE, sims = 99)

## US Credibility
  # punishment
med.s3 <- lm(mod3 ~ recent + age1 + democrat + republican + female + 
               income + hawkish + college + country + ethnic, data = main3)
out.s3 <- glm(pun2b ~ mod3 + recent + age1 + democrat + republican + female + 
               income + hawkish + college + country + 
               ethnic, data = main3, family = binomial("probit"))
med.mod3s <- mediate(med.s3, out.s3, treat = "recent", 
                     mediator = "mod3", robustSE = TRUE, sims = 99)

## Plotting mediator results for Figure 5 in main text -- see generated pdf
pdf(file = "FigureA25.pdf", width = 8, height = 3)  # saving pdf plot

## Approval
windowsFonts(Times = windowsFont("Times New Roman"))  # Setting font 
par(mfrow = c(1,3))

## Approval mediators
plot(med.mod1s, xlab="Effect", xlim = c(-.25, 0.05), lwd = 1, cex=1,
     treatment = "treated", col="black", family="Times", 
     main="(a) Moral Equivalence")   
plot(med.mod2s, xlab="Effect", xlim = c(-.25, 0.05), lwd = 1, cex=1,     
     treatment = "treated", col="black", family="Times", 
     main="(b) US Unilateralism")
plot(med.mod3s, xlab="Effect", xlim = c(-.25, 0.05), lwd = 1, cex=1,
     treatment = "treated", col="black", family="Times", 
     main="(c) US Credibility")
dev.off()

################################################################################
## Sensitivity Analysis


##############################
### Figure A26 in appendix ###
##############################


## Approval

sens.app1 <- medsens(med.mod1, rho.by = 0.1)
sens.app2 <- medsens(med.mod2, rho.by = 0.1)
sens.app3 <- medsens(med.mod3, rho.by = 0.1)

windowsFonts(Times = windowsFont("Times New Roman"))  # Setting font
# plot A26a
pdf(file = "FigureA26a.pdf", width = 3, height = 7)  # saving pdf plot
  par(mfrow=c(2,1))  # Setting graph window to 2 rows and 1 columns
  plot(sens.app1, sens.par = "rho", 
  family="Times", main="Moral Equivalence")   
  dev.off()

# plot A26b
pdf(file = "FigureA26b.pdf", width = 3, height = 7)  # saving pdf plot
  par(mfrow=c(2,1))  # Setting graph window to 2 rows and 1 columns
  plot(sens.app2, sens.par = "rho", family="Times", main="US Unilateralism")
  dev.off()

# plot A26c
pdf(file = "FigureA26c.pdf", width = 3, height = 7)  # saving pdf plot
  par(mfrow=c(2,1))  # Setting graph window to 2 rows and 1 columns
  plot(sens.app3, sens.par = "rho", family="Times", main="US Credibility")
  dev.off()

##############################
### Figure A27 in appendix ###
##############################

## Punishment Sanctions

sens.pun1 <- medsens(med.mod1s, rho.by = 0.1)
sens.pun2 <- medsens(med.mod2s, rho.by = 0.1)
sens.pun3 <- medsens(med.mod3s, rho.by = 0.1)

# plot A27a
pdf(file = "FigureA27a.pdf", width = 3, height = 7)  # saving pdf plot
  par(mfrow=c(2,1))  # Setting graph window to 2 rows and 1 columns
  plot(sens.pun1, sens.par = "rho", family="Times", main="Moral Equivalence")   
  dev.off()

# plot A27b
pdf(file = "FigureA27b.pdf", width = 3, height = 7)  # saving pdf plot
  par(mfrow=c(2,1))  # Setting graph window to 2 rows and 1 columns
  plot(sens.pun2, sens.par = "rho", family="Times", main="US Unilateralism")
  dev.off()

# plot A27c
pdf(file = "FigureA27c.pdf", width = 3, height = 7)  # saving pdf plot
  par(mfrow=c(2,1))  # Setting graph window to 2 rows and 1 columns
  plot(sens.pun3, sens.par = "rho", family="Times", main="US Credibility")
  dev.off()

