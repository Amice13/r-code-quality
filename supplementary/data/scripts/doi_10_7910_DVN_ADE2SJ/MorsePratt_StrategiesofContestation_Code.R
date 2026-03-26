################################
## Replication File for ########
## Strategies of Contestation ##
## Morse and Pratt #############
## November 18, 2021 ###########
################################



#######################################
## Read in survey data from May 2020 ##
#######################################

setwd("C:\\Users\\pasad\\Dropbox\\Replication JOP\\210435R1")
S <- read.csv("MorsePratt_StrategiesofContestation_Data.csv")

length(unique(S$IPAddress)) # 2,529 respondents
nrow(S) # 5,058 observations (each respondent gets 2 randomized scenarios)



##############################################
## Treatment effects of response strategies ##
## on dimensions of government image #########
##############################################

## Function for clustering SEs (we cluster by respondent)
cl <- function(dat,fm, cluster){  
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  coeftest(fm, vcovCL) }

## Code to produce Figure 2 - Effect of Response Strategies on Goverment Image
## First: estimate linear models for perceptions of performance, morality, allegiance, lawfulness
S$treatment3 <- relevel(as.factor(S$treatment3), ref="CON") # control condition is reference group

TE_performance <- cl(S, lm(gov_performance ~ treatment3, data=S), S$IPAddress) 
TE_performance

TE_morality <- cl(S, lm(gov_morality ~ treatment3, data=S), S$IPAddress) 
TE_morality

TE_allegiance <- cl(S, lm(gov_allegiance ~ treatment3, data=S), S$IPAddress) 
TE_allegiance

Sr <- S[!is.na(S$gov_lawfulness),] # 1 respondent failed to answer lawfulness question
TE_law <- cl(Sr, lm(gov_lawfulness ~ treatment3, data=Sr), as.character(Sr$IPAddress))
TE_law

## Second: plot pt estimates and 95 percent confidence intervals (Figure 2 in paper)
par(mfrow=c(3,1))
plot(1:3, 1:3, cex=0, #bty="n",
     xlim=c(-0.5,0.565), ylim=c(4.65,6),
     xaxt="n", yaxt="n", ylab="", xlab="")
abline(v=0, lty=2)

points(TE_morality["treatment3ATONE",1], 5.8, pch=19, col="red", cex=1.7)
lines(TE_morality["treatment3ATONE",1] + c(-1,1)*TE_morality["treatment3ATONE",2]*qnorm(0.975), c(5.8, 5.8), col="red", lwd=1.5)
points(TE_law["treatment3ATONE",1], 5.5, pch=15, col="blue", cex=1.7)
lines(TE_law["treatment3ATONE",1] + c(-1,1)*TE_law["treatment3ATONE",2]*qnorm(0.975), c(5.5, 5.5), col="blue", lwd=1.5)
points(TE_allegiance["treatment3ATONE",1], 5.2, pch=18, col="goldenrod3", cex=2.3)
lines(TE_allegiance["treatment3ATONE",1] + c(-1,1)*TE_allegiance["treatment3ATONE",2]*qnorm(0.975), c(5.2, 5.2), col="goldenrod3", lwd=1.5)
points(TE_performance["treatment3ATONE",1], 4.875, pch=17, col="dark green", cex=1.7)
lines(TE_performance["treatment3ATONE",1] + c(-1,1)*TE_performance["treatment3ATONE",2]*qnorm(0.975), c(4.875, 4.875), col="dark green", lwd=1.5)
text(x=-0.5, y=5.35, "Atonement", pos=4, cex=1.2)

plot(1:3, 1:3, cex=0, #bty="n",
     xlim=c(-0.5,0.565), ylim=c(4.65,6),
     xaxt="n", yaxt="n", ylab="", xlab="")
abline(v=0, lty=2)
points(TE_morality["treatment3DIS",1], 5.8, pch=19, col="red", cex=1.7)
lines(TE_morality["treatment3DIS",1] + c(-1,1)*TE_morality["treatment3DIS",2]*qnorm(0.975), c(5.8, 5.8), col="red", lwd=1.5)
points(TE_law["treatment3DIS",1], 5.5, pch=15, col="blue", cex=1.7)
lines(TE_law["treatment3DIS",1] + c(-1,1)*TE_law["treatment3DIS",2]*qnorm(0.975), c(5.5, 5.5), col="blue", lwd=1.5)
points(TE_allegiance["treatment3DIS",1], 5.2, pch=18, col="goldenrod3", cex=2.3)
lines(TE_allegiance["treatment3DIS",1] + c(-1,1)*TE_allegiance["treatment3DIS",2]*qnorm(0.975), c(5.2, 5.2), col="goldenrod3", lwd=1.5)
points(TE_performance["treatment3DIS",1], 4.875, pch=17, col="dark green", cex=1.7)
lines(TE_performance["treatment3DIS",1] + c(-1,1)*TE_performance["treatment3DIS",2]*qnorm(0.975), c(4.875, 4.875), col="dark green", lwd=1.5)
text(x=-0.5, y=5.35, "Disassociation", pos=4, cex=1.2)

plot(1:3, 1:3, cex=0, #bty="n",
     xlim=c(-0.5,0.565), ylim=c(4.65,6),
     xaxt="n", yaxt="n", ylab="", xlab="")
axis(side=1, at = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6))
abline(v=0, lty=2)
points(TE_morality["treatment3ATTACK",1], 5.8, pch=19, col="red", cex=1.7)
lines(TE_morality["treatment3ATTACK",1] + c(-1,1)*TE_morality["treatment3ATTACK",2]*qnorm(0.975), c(5.8, 5.8), col="red", lwd=1.5)
points(TE_law["treatment3ATTACK",1], 5.5, pch=15, col="blue", cex=1.7)
lines(TE_law["treatment3ATTACK",1] + c(-1,1)*TE_law["treatment3ATTACK",2]*qnorm(0.975), c(5.5, 5.5), col="blue", lwd=1.5)
points(TE_allegiance["treatment3ATTACK",1], 5.2, pch=18, col="goldenrod3", cex=2.3)
lines(TE_allegiance["treatment3ATTACK",1] + c(-1,1)*TE_allegiance["treatment3ATTACK",2]*qnorm(0.975), c(5.2, 5.2), col="goldenrod3", lwd=1.5)
points(TE_performance["treatment3ATTACK",1], 4.875, pch=17, col="dark green", cex=1.7)
lines(TE_performance["treatment3ATTACK",1] + c(-1,1)*TE_performance["treatment3ATTACK",2]*qnorm(0.975), c(4.875, 4.875), col="dark green", lwd=1.5)
text(x=-0.5, y=5.35, "Attack", pos=4, cex=1.2)



######################################
## Code to produce figure 3: interactive 
## effects of govt image and militant internationalism (MI)
## on predictions of political support
#######################################

Srm <- S[!is.na(S$gov_lawfulness) & !is.na(S$MI_score1),] # exclude those who did not answer MI questions

## First: estimate regressions examiningg marginal effect 
## of moral, lawful image across MI values 

moral_inter <- lm(votegov_binary ~ MI_score1*gov_morality + gov_lawfulness + 
                    gov_allegiance + gov_performance, data=Srm)

lawful_inter <- lm(votegov_binary ~ MI_score1*gov_lawfulness + gov_morality + 
                     gov_allegiance + gov_performance, data=Srm)

library(interplot)
## Second: plot the two estimated relationships across a range of MI values

# left panel
interplot(m = moral_inter, var1 = "gov_morality", var2 = "MI_score1", hist = TRUE) +
  aes(color = "pink") + theme(legend.position="none")

# right panel
interplot(m = lawful_inter, var1 = "gov_lawfulness", var2 = "MI_score1", hist = TRUE, ymax=1) +
  aes(color = "pink") + theme(legend.position="none")


########################
## Online Appendix #####
## Tables and Figures ##
########################

## Section A.1 contains text of survey

## Section A.2: Additional Results
##Table A1: Survey Sample Statistics
prop.table(table(S$pid))
prop.table(table(S$age_cat))
prop.table(table(S$edu_cat))
prop.table(table(S$ethnicity_cat))
prop.table(table(S$hispanic!=1))
prop.table(table(S$income_cat))
prop.table(table(S$region))
prop.table(table(S$geo_region))


## Table A2: Effect of Gov Image and Foreign Policy Values on Political Support

# saturated model with all interactions
MI_inter <- cl(Srm, lm(votegov_binary ~ gov_morality*MI_score1 + gov_lawfulness*MI_score1 + 
                         gov_allegiance*MI_score1 + gov_performance*MI_score1, data=S), Srm$IPAddress) 

# interactions one at a time
MI_morality <- cl(Srm, lm(votegov_binary ~ gov_morality*MI_score1 + gov_lawfulness + 
                            gov_allegiance + gov_performance, data=Srm), Srm$IPAddress)
MI_law <- cl(Srm, lm(votegov_binary ~ gov_lawfulness*MI_score1 + gov_morality + 
                       gov_allegiance + gov_performance, data=Srm), Srm$IPAddress)
MI_performance <- cl(Srm, lm(votegov_binary ~ gov_performance*MI_score1 + gov_lawfulness + 
                               gov_allegiance + gov_morality, data=Srm), Srm$IPAddress)
MI_allegiance <- cl(Srm, lm(votegov_binary ~ gov_allegiance*MI_score1 + gov_lawfulness + 
                              gov_morality + gov_performance, data=Srm), Srm$IPAddress)

MI_inter # Table A2, Column 1
MI_morality # Table A2, Column 2
MI_law # Table A2, Column 3
MI_performance # Table A2, Column 4
MI_allegiance ##Table A2, Column 5


## Figure A1: Effects of Five Response Strategies on Government Image

S$treatment <- relevel(as.factor(S$treatment), ref="CON")
Sr$treatment <- relevel(as.factor(Sr$treatment), ref="CON")

TE_performance2 <- cl(S, lm(gov_performance ~ treatment, data=S), S$IPAddress) 
TE_morality2 <- cl(S, lm(gov_morality ~ treatment, data=S), S$IPAddress) 
TE_allegiance2 <- cl(S, lm(gov_allegiance ~ treatment, data=S), S$IPAddress) 
TE_lawfulness2 <- cl(Sr, lm(gov_lawfulness ~ treatment, data=Sr), as.character(Sr$IPAddress))

# panel 1: apology
par(mfrow=c(1,1))
plot(1:3, 1:3, cex=0, xlim=c(-0.7,0.7), ylim=c(4.65,6),
     xaxt="n", yaxt="n", ylab="", xlab="")
abline(v=0, lty=2)
axis(side=1, at = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6))
points(TE_morality2["treatmentAPO",1], 5.8, pch=19, col="red", cex=1.7)
lines(TE_morality2["treatmentAPO",1] + c(-1,1)*TE_morality2["treatmentAPO",2]*qnorm(0.975), c(5.8, 5.8), col="red", lwd=1.5)
points(TE_lawfulness2["treatmentAPO",1], 5.5, pch=15, col="blue", cex=1.7)
lines(TE_lawfulness2["treatmentAPO",1] + c(-1,1)*TE_lawfulness2["treatmentAPO",2]*qnorm(0.975), c(5.5, 5.5), col="blue", lwd=1.5)
points(TE_allegiance2["treatmentAPO",1], 5.2, pch=18, col="goldenrod3", cex=2.3)
lines(TE_allegiance2["treatmentAPO",1] + c(-1,1)*TE_allegiance2["treatmentAPO",2]*qnorm(0.975), c(5.2, 5.2), col="goldenrod3", lwd=1.5)
points(TE_performance2["treatmentAPO",1], 4.875, pch=17, col="dark green", cex=1.7)
lines(TE_performance2["treatmentAPO",1] + c(-1,1)*TE_performance2["treatmentAPO",2]*qnorm(0.975), c(4.875, 4.875), col="dark green", lwd=1.5)
text(x=-0.73, y=5.35, "Atonement: Apology", pos=4, cex=1.2)

# panel 2: recommitment
plot(1:3, 1:3, cex=0, xlim=c(-0.7,0.7), ylim=c(4.65,6),
     xaxt="n", yaxt="n", ylab="", xlab="")
abline(v=0, lty=2)
axis(side=1, at = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6))
points(TE_morality2["treatmentREC",1], 5.8, pch=19, col="red", cex=1.7)
lines(TE_morality2["treatmentREC",1] + c(-1,1)*TE_morality2["treatmentREC",2]*qnorm(0.975), c(5.8, 5.8), col="red", lwd=1.5)
points(TE_lawfulness2["treatmentREC",1], 5.5, pch=15, col="blue", cex=1.7)
lines(TE_lawfulness2["treatmentREC",1] + c(-1,1)*TE_lawfulness2["treatmentREC",2]*qnorm(0.975), c(5.5, 5.5), col="blue", lwd=1.5)
points(TE_allegiance2["treatmentREC",1], 5.2, pch=18, col="goldenrod3", cex=2.3)
lines(TE_allegiance2["treatmentREC",1] + c(-1,1)*TE_allegiance2["treatmentREC",2]*qnorm(0.975), c(5.2, 5.2), col="goldenrod3", lwd=1.5)
points(TE_performance2["treatmentREC",1], 4.875, pch=17, col="dark green", cex=1.7)
lines(TE_performance2["treatmentREC",1] + c(-1,1)*TE_performance2["treatmentREC",2]*qnorm(0.975), c(4.875, 4.875), col="dark green", lwd=1.5)
text(x=-0.73, y=5.35, "Atonement: Recommit", pos=4, cex=1.2)

# panel 3: disassociation
plot(1:3, 1:3, cex=0, xlim=c(-0.7,0.7), ylim=c(4.65,6),
     xaxt="n", yaxt="n", ylab="", xlab="")
abline(v=0, lty=2)
axis(side=1, at = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6))
points(TE_morality2["treatmentDIS",1], 5.8, pch=19, col="red", cex=1.7)
lines(TE_morality2["treatmentDIS",1] + c(-1,1)*TE_morality2["treatmentDIS",2]*qnorm(0.975), c(5.8, 5.8), col="red", lwd=1.5)
points(TE_lawfulness2["treatmentDIS",1], 5.5, pch=15, col="blue", cex=1.7)
lines(TE_lawfulness2["treatmentDIS",1] + c(-1,1)*TE_lawfulness2["treatmentDIS",2]*qnorm(0.975), c(5.5, 5.5), col="blue", lwd=1.5)
points(TE_allegiance2["treatmentDIS",1], 5.2, pch=18, col="goldenrod3", cex=2.3)
lines(TE_allegiance2["treatmentDIS",1] + c(-1,1)*TE_allegiance2["treatmentDIS",2]*qnorm(0.975), c(5.2, 5.2), col="goldenrod3", lwd=1.5)
points(TE_performance2["treatmentDIS",1], 4.875, pch=17, col="dark green", cex=1.7)
lines(TE_performance2["treatmentDIS",1] + c(-1,1)*TE_performance2["treatmentDIS",2]*qnorm(0.975), c(4.875, 4.875), col="dark green", lwd=1.5)
text(x=-0.73, y=5.35, "Disassociation", pos=4, cex=1.2)

# panel 4: legitimacy attack
plot(1:3, 1:3, cex=0, xlim=c(-0.7,0.7), ylim=c(4.65,6),
     xaxt="n", yaxt="n", ylab="", xlab="")
abline(v=0, lty=2)
axis(side=1, at = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6))
points(TE_morality2["treatmentLEG",1], 5.8, pch=19, col="red", cex=1.7)
lines(TE_morality2["treatmentLEG",1] + c(-1,1)*TE_morality2["treatmentLEG",2]*qnorm(0.975), c(5.8, 5.8), col="red", lwd=1.5)
points(TE_lawfulness2["treatmentLEG",1], 5.5, pch=15, col="blue", cex=1.7)
lines(TE_lawfulness2["treatmentLEG",1] + c(-1,1)*TE_lawfulness2["treatmentLEG",2]*qnorm(0.975), c(5.5, 5.5), col="blue", lwd=1.5)
points(TE_allegiance2["treatmentLEG",1], 5.2, pch=18, col="goldenrod3", cex=2.3)
lines(TE_allegiance2["treatmentLEG",1] + c(-1,1)*TE_allegiance2["treatmentLEG",2]*qnorm(0.975), c(5.2, 5.2), col="goldenrod3", lwd=1.5)
points(TE_performance2["treatmentLEG",1], 4.875, pch=17, col="dark green", cex=1.7)
lines(TE_performance2["treatmentLEG",1] + c(-1,1)*TE_performance2["treatmentLEG",2]*qnorm(0.975), c(4.875, 4.875), col="dark green", lwd=1.5)
text(x=-0.73, y=5.35, "Attack: Legitimacy", pos=4, cex=1.2)

# panel 5: national interest attack
plot(1:3, 1:3, cex=0, xlim=c(-0.7,0.7), ylim=c(4.65,6),
     xaxt="n", yaxt="n", ylab="", xlab="")
abline(v=0, lty=2)
axis(side=1, at = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6))
points(TE_morality2["treatmentNI",1], 5.8, pch=19, col="red", cex=1.7)
lines(TE_morality2["treatmentNI",1] + c(-1,1)*TE_morality2["treatmentNI",2]*qnorm(0.975), c(5.8, 5.8), col="red", lwd=1.5)
points(TE_lawfulness2["treatmentNI",1], 5.5, pch=15, col="blue", cex=1.7)
lines(TE_lawfulness2["treatmentNI",1] + c(-1,1)*TE_lawfulness2["treatmentNI",2]*qnorm(0.975), c(5.5, 5.5), col="blue", lwd=1.5)
points(TE_allegiance2["treatmentNI",1], 5.2, pch=18, col="goldenrod3", cex=2.3)
lines(TE_allegiance2["treatmentNI",1] + c(-1,1)*TE_allegiance2["treatmentNI",2]*qnorm(0.975), c(5.2, 5.2), col="goldenrod3", lwd=1.5)
points(TE_performance2["treatmentNI",1], 4.875, pch=17, col="dark green", cex=1.7)
lines(TE_performance2["treatmentNI",1] + c(-1,1)*TE_performance2["treatmentNI",2]*qnorm(0.975), c(4.875, 4.875), col="dark green", lwd=1.5)
text(x=-0.73, y=5.35, "Attack: Nat Interest", pos=4, cex=1.2)


## Table A3: Effect of Government Image, Party ID on Political Support

# Column 1
cl(Sr, lm(votegov_binary ~ gov_morality + gov_lawfulness + 
            gov_allegiance + gov_performance + pid, data=S), Sr$IPAddress)

# Column 2
cl(Sr, lm(votegov_binary ~ gov_morality*pid + gov_lawfulness*pid + 
            gov_allegiance*pid + gov_performance*pid, data=S), Sr$IPAddress) 


## Table A4: Effect of Government Image, Cooperative Internationalism on Political Support

# Column 1
Src <- S[!is.na(S$gov_lawfulness) & !is.na(S$CI_score1),]
cl(Src, lm(votegov_binary ~ gov_morality + gov_lawfulness + 
             gov_allegiance + gov_performance + CI_score1, data=S), Src$IPAddress) 

# Column 2
cl(Src, lm(votegov_binary ~ gov_morality*CI_score1 + gov_lawfulness*CI_score1 + 
             gov_allegiance*CI_score1 + gov_performance*CI_score1, data=S), Src$IPAddress) 


## Figure A2: Treatment Effects of Response Strategies by Issue Area

# estimate effects separately by issue area (Torture, Trade, Chem Weapons)

TE_perf_torture <- cl(S[S$issue=="Torture",], lm(gov_performance ~ treatment3, data=S[S$issue=="Torture",]), S$IPAddress[S$issue=="Torture"]) 
TE_morality_torture <- cl(S[S$issue=="Torture",], lm(gov_morality ~ treatment3, data=S[S$issue=="Torture",]), S$IPAddress[S$issue=="Torture"]) 
TE_allegiance_torture <- cl(S[S$issue=="Torture",], lm(gov_allegiance ~ treatment3, data=S[S$issue=="Torture",]), S$IPAddress[S$issue=="Torture"]) 
Sr_torture <- S[!is.na(S$gov_lawfulness) & S$issue=="Torture",]
TE_law_torture <- cl(Sr_torture, lm(gov_lawfulness ~ treatment3, data=Sr_torture), as.character(Sr_torture$IPAddress))

TE_perf_trade <- cl(S[S$issue=="Trade",], lm(gov_performance ~ treatment3, data=S[S$issue=="Trade",]), S$IPAddress[S$issue=="Trade"]) 
TE_morality_trade <- cl(S[S$issue=="Trade",], lm(gov_morality ~ treatment3, data=S[S$issue=="Trade",]), S$IPAddress[S$issue=="Trade"]) 
TE_allegiance_trade <- cl(S[S$issue=="Trade",], lm(gov_allegiance ~ treatment3, data=S[S$issue=="Trade",]), S$IPAddress[S$issue=="Trade"]) 
Sr_trade <- S[!is.na(S$gov_lawfulness) & S$issue=="Trade",]
TE_law_trade <- cl(Sr_trade, lm(gov_lawfulness ~ treatment3, data=Sr_trade), as.character(Sr_trade$IPAddress))

TE_perf_chem <- cl(S[S$issue=="Weapons",], lm(gov_performance ~ treatment3, data=S[S$issue=="Weapons",]), S$IPAddress[S$issue=="Weapons"]) 
TE_morality_chem <- cl(S[S$issue=="Weapons",], lm(gov_morality ~ treatment3, data=S[S$issue=="Weapons",]), S$IPAddress[S$issue=="Weapons"]) 
TE_allegiance_chem <- cl(S[S$issue=="Weapons",], lm(gov_allegiance ~ treatment3, data=S[S$issue=="Weapons",]), S$IPAddress[S$issue=="Weapons"]) 
Sr_chem <- S[!is.na(S$gov_lawfulness) & S$issue=="Weapons",]
TE_law_chem <- cl(Sr_chem, lm(gov_lawfulness ~ treatment3, data=Sr_chem), as.character(Sr_chem$IPAddress))


# Figure A2, left panel
plot(1:3, 1:3, cex=0, bty="n", main="Effects of Atonement Strategy",
     xlim=c(-0.65,0.85), ylim=c(-1.5,6),
     xaxt="n", yaxt="n", ylab="", xlab="")
abline(v=0, lty=2)
axis(side=1, at = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6))

points(TE_perf_torture["treatment3ATONE",1], 5.75, pch=19, col="red")
lines(TE_perf_torture["treatment3ATONE",1] + c(-1,1)*TE_perf_torture["treatment3ATONE",2]*qnorm(0.975), c(5.75, 5.75), col="red", lwd=1.5)
points(TE_law_torture["treatment3ATONE",1], 5.5, pch=15, col="blue")
lines(TE_law_torture["treatment3ATONE",1] + c(-1,1)*TE_law_torture["treatment3ATONE",2]*qnorm(0.975), c(5.5, 5.5), col="blue", lwd=1.5)
points(TE_allegiance_torture["treatment3ATONE",1], 5.25, pch=18, col="goldenrod3", cex=1.5)
lines(TE_allegiance_torture["treatment3ATONE",1] + c(-1,1)*TE_allegiance_torture["treatment3ATONE",2]*qnorm(0.975), c(5.25, 5.25), col="goldenrod3", lwd=1.5)
points(TE_morality_torture["treatment3ATONE",1], 5, pch=17, col="dark green")
lines(TE_morality_torture["treatment3ATONE",1] + c(-1,1)*TE_morality_torture["treatment3ATONE",2]*qnorm(0.975), c(5, 5), col="dark green", lwd=1.5)
text(x=-0.3, y=5.5, "Torture", pos=2)

points(TE_perf_trade["treatment3ATONE",1], 5.75-2.5, pch=19, col="red")
lines(TE_perf_trade["treatment3ATONE",1] + c(-1,1)*TE_perf_trade["treatment3DIS",2]*qnorm(0.975), c(5.75, 5.75)-2.5, col="red", lwd=1.5)
points(TE_law_trade["treatment3ATONE",1], 5.5-2.5, pch=15, col="blue")
lines(TE_law_trade["treatment3ATONE",1] + c(-1,1)*TE_law_trade["treatment3DIS",2]*qnorm(0.975), c(5.5, 5.5)-2.5, col="blue", lwd=1.5)
points(TE_allegiance_trade["treatment3ATONE",1], 5.25-2.5, pch=18, col="goldenrod3", cex=1.5)
lines(TE_allegiance_trade["treatment3ATONE",1] + c(-1,1)*TE_allegiance_trade["treatment3DIS",2]*qnorm(0.975), c(5.25, 5.25)-2.5, col="goldenrod3", lwd=1.5)
points(TE_morality_trade["treatment3ATONE",1], 5-2.5, pch=17, col="dark green")
lines(TE_morality_trade["treatment3ATONE",1] + c(-1,1)*TE_morality_trade["treatment3DIS",2]*qnorm(0.975), c(5-2.5, 5-2.5), col="dark green", lwd=1.5)
text(x=-0.3, y=5.5-2.5, "Trade", pos=2)

points(TE_perf_chem["treatment3ATONE",1], 5.75-5, pch=19, col="red")
lines(TE_perf_chem["treatment3ATONE",1] + c(-1,1)*TE_perf_chem["treatment3ATTACK",2]*qnorm(0.975), c(5.75, 5.75)-5, col="red", lwd=1.5)
points(TE_law_chem["treatment3ATONE",1], 5.5-5, pch=15, col="blue")
lines(TE_law_chem["treatment3ATONE",1] + c(-1,1)*TE_law_chem["treatment3ATTACK",2]*qnorm(0.975), c(5.5, 5.5)-5, col="blue", lwd=1.5)
points(TE_allegiance_chem["treatment3ATONE",1], 5.25-5, pch=18, col="goldenrod3", cex=1.5)
lines(TE_allegiance_chem["treatment3ATONE",1] + c(-1,1)*TE_allegiance_chem["treatment3ATTACK",2]*qnorm(0.975), c(5.25, 5.25)-5, col="goldenrod3", lwd=1.5)
points(TE_morality_chem["treatment3ATONE",1], 5-5, pch=17, col="dark green")
lines(TE_morality_chem["treatment3ATONE",1] + c(-1,1)*TE_morality_chem["treatment3ATTACK",2]*qnorm(0.975), c(5-5, 5-5), col="dark green", lwd=1.5)
text(x=-0.3, y=5.5-5, "Chem Weapons", pos=2)

legend(x="bottomright", legend=c("Performance", "Lawfulness",
                                 "Allegiance", "Morality"),
       pch=c(19, 15, 18, 17), pt.cex=c(1.5,1.5,2,1.5),col=c("red", "blue", "goldenrod3", "dark green")) 


# Figure A2, middle panel
plot(1:3, 1:3, cex=0, bty="n", main="Effects of Disassociation Strategy",
     xlim=c(-0.65,0.85), ylim=c(-1.5,6),
     xaxt="n", yaxt="n", ylab="", xlab="")
abline(v=0, lty=2)
axis(side=1, at = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6))

points(TE_perf_torture["treatment3DIS",1], 5.75, pch=19, col="red")
lines(TE_perf_torture["treatment3DIS",1] + c(-1,1)*TE_perf_torture["treatment3DIS",2]*qnorm(0.975), c(5.75, 5.75), col="red", lwd=1.5)
points(TE_law_torture["treatment3DIS",1], 5.5, pch=15, col="blue")
lines(TE_law_torture["treatment3DIS",1] + c(-1,1)*TE_law_torture["treatment3DIS",2]*qnorm(0.975), c(5.5, 5.5), col="blue", lwd=1.5)
points(TE_allegiance_torture["treatment3DIS",1], 5.25, pch=18, col="goldenrod3", cex=1.5)
lines(TE_allegiance_torture["treatment3DIS",1] + c(-1,1)*TE_allegiance_torture["treatment3DIS",2]*qnorm(0.975), c(5.25, 5.25), col="goldenrod3", lwd=1.5)
points(TE_morality_torture["treatment3DIS",1], 5, pch=17, col="dark green")
lines(TE_morality_torture["treatment3DIS",1] + c(-1,1)*TE_morality_torture["treatment3DIS",2]*qnorm(0.975), c(5, 5), col="dark green", lwd=1.5)
text(x=-0.3, y=5.5, "Torture", pos=2)

points(TE_perf_trade["treatment3DIS",1], 5.75-2.5, pch=19, col="red")
lines(TE_perf_trade["treatment3DIS",1] + c(-1,1)*TE_perf_trade["treatment3DIS",2]*qnorm(0.975), c(5.75, 5.75)-2.5, col="red", lwd=1.5)
points(TE_law_trade["treatment3DIS",1], 5.5-2.5, pch=15, col="blue")
lines(TE_law_trade["treatment3DIS",1] + c(-1,1)*TE_law_trade["treatment3DIS",2]*qnorm(0.975), c(5.5, 5.5)-2.5, col="blue", lwd=1.5)
points(TE_allegiance_trade["treatment3DIS",1], 5.25-2.5, pch=18, col="goldenrod3", cex=1.5)
lines(TE_allegiance_trade["treatment3DIS",1] + c(-1,1)*TE_allegiance_trade["treatment3DIS",2]*qnorm(0.975), c(5.25, 5.25)-2.5, col="goldenrod3", lwd=1.5)
points(TE_morality_trade["treatment3DIS",1], 5-2.5, pch=17, col="dark green")
lines(TE_morality_trade["treatment3DIS",1] + c(-1,1)*TE_morality_trade["treatment3DIS",2]*qnorm(0.975), c(5-2.5, 5-2.5), col="dark green", lwd=1.5)
text(x=-0.3, y=5.5-2.5, "Trade", pos=2)

points(TE_perf_chem["treatment3DIS",1], 5.75-5, pch=19, col="red")
lines(TE_perf_chem["treatment3DIS",1] + c(-1,1)*TE_perf_chem["treatment3DIS",2]*qnorm(0.975), c(5.75, 5.75)-5, col="red", lwd=1.5)
points(TE_law_chem["treatment3DIS",1], 5.5-5, pch=15, col="blue")
lines(TE_law_chem["treatment3DIS",1] + c(-1,1)*TE_law_chem["treatment3DIS",2]*qnorm(0.975), c(5.5, 5.5)-5, col="blue", lwd=1.5)
points(TE_allegiance_chem["treatment3DIS",1], 5.25-5, pch=18, col="goldenrod3", cex=1.5)
lines(TE_allegiance_chem["treatment3DIS",1] + c(-1,1)*TE_allegiance_chem["treatment3DIS",2]*qnorm(0.975), c(5.25, 5.25)-5, col="goldenrod3", lwd=1.5)
points(TE_morality_chem["treatment3DIS",1], 5-5, pch=17, col="dark green")
lines(TE_morality_chem["treatment3DIS",1] + c(-1,1)*TE_morality_chem["treatment3DIS",2]*qnorm(0.975), c(5-5, 5-5), col="dark green", lwd=1.5)
text(x=-0.3, y=5.5-5, "Chem Weapons", pos=2)

legend(x="bottomright", legend=c("Performance", "Lawfulness",
                                 "Allegiance", "Morality"),
       pch=c(19, 15, 18, 17), pt.cex=c(1.5,1.5,2,1.5),col=c("red", "blue", "goldenrod3", "dark green")) 


# Figure A2, right panel
plot(1:3, 1:3, cex=0, bty="n", main="Effects of Attack Strategy",
     xlim=c(-0.65,0.85), ylim=c(-1.5,6),
     xaxt="n", yaxt="n", ylab="", xlab="")
abline(v=0, lty=2)
axis(side=1, at = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6))

points(TE_perf_torture["treatment3ATTACK",1], 5.75, pch=19, col="red")
lines(TE_perf_torture["treatment3ATTACK",1] + c(-1,1)*TE_perf_torture["treatment3ATTACK",2]*qnorm(0.975), c(5.75, 5.75), col="red", lwd=1.5)
points(TE_law_torture["treatment3ATTACK",1], 5.5, pch=15, col="blue")
lines(TE_law_torture["treatment3ATTACK",1] + c(-1,1)*TE_law_torture["treatment3ATTACK",2]*qnorm(0.975), c(5.5, 5.5), col="blue", lwd=1.5)
points(TE_allegiance_torture["treatment3ATTACK",1], 5.25, pch=18, col="goldenrod3", cex=1.5)
lines(TE_allegiance_torture["treatment3ATTACK",1] + c(-1,1)*TE_allegiance_torture["treatment3ATTACK",2]*qnorm(0.975), c(5.25, 5.25), col="goldenrod3", lwd=1.5)
points(TE_morality_torture["treatment3ATTACK",1], 5, pch=17, col="dark green")
lines(TE_morality_torture["treatment3ATTACK",1] + c(-1,1)*TE_morality_torture["treatment3ATTACK",2]*qnorm(0.975), c(5, 5), col="dark green", lwd=1.5)
text(x=-0.4, y=5.5, "Torture", pos=2)

points(TE_perf_trade["treatment3ATTACK",1], 5.75-2.5, pch=19, col="red")
lines(TE_perf_trade["treatment3ATTACK",1] + c(-1,1)*TE_perf_trade["treatment3ATTACK",2]*qnorm(0.975), c(5.75, 5.75)-2.5, col="red", lwd=1.5)
points(TE_law_trade["treatment3ATTACK",1], 5.5-2.5, pch=15, col="blue")
lines(TE_law_trade["treatment3ATTACK",1] + c(-1,1)*TE_law_trade["treatment3ATTACK",2]*qnorm(0.975), c(5.5, 5.5)-2.5, col="blue", lwd=1.5)
points(TE_allegiance_trade["treatment3ATTACK",1], 5.25-2.5, pch=18, col="goldenrod3", cex=1.5)
lines(TE_allegiance_trade["treatment3ATTACK",1] + c(-1,1)*TE_allegiance_trade["treatment3ATTACK",2]*qnorm(0.975), c(5.25, 5.25)-2.5, col="goldenrod3", lwd=1.5)
points(TE_morality_trade["treatment3ATTACK",1], 5-2.5, pch=17, col="dark green")
lines(TE_morality_trade["treatment3ATTACK",1] + c(-1,1)*TE_morality_trade["treatment3ATTACK",2]*qnorm(0.975), c(5-2.5, 5-2.5), col="dark green", lwd=1.5)
text(x=-0.4, y=5.5-2.5, "Trade", pos=2)

points(TE_perf_chem["treatment3ATTACK",1], 5.75-5, pch=19, col="red")
lines(TE_perf_chem["treatment3ATTACK",1] + c(-1,1)*TE_perf_chem["treatment3ATTACK",2]*qnorm(0.975), c(5.75, 5.75)-5, col="red", lwd=1.5)
points(TE_law_chem["treatment3ATTACK",1], 5.5-5, pch=15, col="blue")
lines(TE_law_chem["treatment3ATTACK",1] + c(-1,1)*TE_law_chem["treatment3ATTACK",2]*qnorm(0.975), c(5.5, 5.5)-5, col="blue", lwd=1.5)
points(TE_allegiance_chem["treatment3ATTACK",1], 5.25-5, pch=18, col="goldenrod3", cex=1.5)
lines(TE_allegiance_chem["treatment3ATTACK",1] + c(-1,1)*TE_allegiance_chem["treatment3ATTACK",2]*qnorm(0.975), c(5.25, 5.25)-5, col="goldenrod3", lwd=1.5)
points(TE_morality_chem["treatment3ATTACK",1], 5-5, pch=17, col="dark green")
lines(TE_morality_chem["treatment3ATTACK",1] + c(-1,1)*TE_morality_chem["treatment3ATTACK",2]*qnorm(0.975), c(5-5, 5-5), col="dark green", lwd=1.5)
text(x=-0.3, y=5.5-5, "Chem Weapons", pos=2)

legend(x="bottomright", legend=c("Performance", "Lawfulness",
                                 "Allegiance", "Morality"),
       pch=c(19, 15, 18, 17), pt.cex=c(1.5,1.5,2,1.5),col=c("red", "blue", "goldenrod3", "dark green")) 



## Figure A3: Treatment Effects of Compliance on Government Image

par(mfrow=c(1,1))
plot(1:3, 1:3, cex=0, xlim=c(-0.5,1.2), ylim=c(4.65,6),
     xaxt="n", yaxt="n", ylab="", xlab="")
abline(v=0, lty=2)
axis(side=1, at = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1, 1.2))
points(TE_morality["treatment3COMPLY",1], 5.8, pch=19, col="red", cex=1.7)
lines(TE_morality["treatment3COMPLY",1] + c(-1,1)*TE_morality["treatment3COMPLY",2]*qnorm(0.975), c(5.8, 5.8), col="red", lwd=1.5)
points(TE_law["treatment3COMPLY",1], 5.5, pch=15, col="blue", cex=1.7)
lines(TE_law["treatment3COMPLY",1] + c(-1,1)*TE_law["treatment3COMPLY",2]*qnorm(0.975), c(5.5, 5.5), col="blue", lwd=1.5)
points(TE_allegiance["treatment3COMPLY",1], 5.2, pch=18, col="goldenrod3", cex=2.3)
lines(TE_allegiance["treatment3COMPLY",1] + c(-1,1)*TE_allegiance["treatment3COMPLY",2]*qnorm(0.975), c(5.2, 5.2), col="goldenrod3", lwd=1.5)
points(TE_performance["treatment3COMPLY",1], 4.875, pch=17, col="dark green", cex=1.7)
lines(TE_performance["treatment3COMPLY",1] + c(-1,1)*TE_performance["treatment3COMPLY",2]*qnorm(0.975), c(4.875, 4.875), col="dark green", lwd=1.5)
text(x=-0.5, y=5.35, "Comply", pos=4, cex=1.2)


## Figure A4: Sample Distribution of Militant Internationalism
hist(S$MI_score1, main="", xlab="Militant Internationalism")


## Figure A5: Effect of Response Strategies on Public Support, Hawks vs. Doves
S_highMI <- S[which(S$MI_score1 >= quantile(S$MI_score1, na.rm=T)[4]),] # >= 75th percentile on MI 
S_lowMI <- S[which(S$MI_score1 <= quantile(S$MI_score1, na.rm=T)[2]),] # <= 25th percentile on MI

votechoice_highMI <- lm(votegov_binary ~ treatment3, data=S_highMI)
votechoice_lowMI <- lm(votegov_binary ~ treatment3, data=S_lowMI)
hcl <- cl(S_highMI, votechoice_highMI, S_highMI$IPAddress)
lcl <- cl(S_lowMI, votechoice_lowMI, S_lowMI$IPAddress)

par(mfrow=c(1,1))
plot(-.1+(1:4), coef(votechoice_highMI)[c(2,5,3,4)], 
     pch=19, ylab="Treatment Effect",
     xlab="", xaxt="n", xlim=c(0.5, 4.5), ylim=c(-.1,.6),
     main="Effect of Response Strategies, Hawks vs. Doves", col="red")
points(.1+(1:4), coef(votechoice_lowMI)[c(2,5,3,4)], 
       pch=15, col="blue")
abline(h=0, lty=2)
axis(side=1, tick=FALSE, at=1:4,
     labels=c("Atonement", "Disassociate", "Attack", "Comply"))
for(i in 1:4){
  lines(x=-.1+c(i,i), y=(coef(votechoice_highMI)[c(2,5,3,4)][i] + c(-1,1)*qnorm(0.975)*(hcl[c(2,5,3,4)[i],2])), col="red")
  lines(x=.1+c(i,i), y=(coef(votechoice_lowMI)[c(2,5,3,4)][i] + c(-1,1)*qnorm(0.975)*(lcl[c(2,5,3,4)[i],2])), col="blue")
}
legend(x=0.5, y=.6, legend=c("Hawks", "Doves"), pch=c(19, 15), col=c("red", "blue"))

