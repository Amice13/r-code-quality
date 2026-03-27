## Daniel J. Mallinson and Joshua Jansa
## Script for "When Is It Time to Do a Policy Diffusion Study?"
## R Version: 4.4.2

start_time <- Sys.time() #for estimating run time

rm(list=ls()) #clear workspace

install.packages("NetworkInference", "lme4", "survival")

library(foreign)
library(NetworkInference)
library(lme4)
library(survival)

"%notin%" <- Negate("%in%")

spid.all <- read.table("spid_all.tab", sep = "\t", header = TRUE)

spid.keep <- policies_metadata[which(policies_metadata$adopt_count >= 42),]
spid.keep <- spid.keep[which(spid.keep$first_year>=1964),]
spid.keep <- spid.keep[which(spid.keep$policy %notin% c("pubbrefeed", "hate crimes", "victim notification", "sonsam")),]
spid.keep <- spid.keep[which(spid.keep$last_year-spid.keep$first_year > 4),]

spid <- spid.all[which(spid.all$policy %in% unique(spid.keep$policy)),]

## Figure 1 - Sample Topics
table(spid.keep$majortopic)

bars <- c(30, 17, 10, 9, 6, 5, 4, 1, 1)
labs <- c("Law and Crime", "Domestic Commerce", "Transportation", "Civil Rights",
          "Health", "Gov't Operations", "Education", "Macroeconomics", "Social Welfare")

#Note: Figure title is not created in the code
jpeg("figure1.jpg", width=8, height=4, units="in", res=600)
par(mar=c(2,10,1,2))
bar <- barplot(bars, horiz=TRUE, yaxt="n", xaxt="n", xlim=c(0,35))
axis(2, at=bar, labels=labs, las=2)
axis(1, at=seq(0,35,5), labels=seq(0,35,5))
text(bars+1, bar, labels=as.character(bars))
dev.off()

table(spid.keep$adopt_count)

## Conduct Iterative EHA for Neighbor Adoptions Variable
use.keep.neigh <- data.frame(matrix(nrow=0, ncol=6))
names(use.keep.neigh) <- c("policy", "year", "adopts_total", "coef", "se", "p")

use.keep.ideo <- data.frame(matrix(nrow=0, ncol=6))
names(use.keep.ideo) <- c("policy", "year", "adopts_total", "coef", "se", "p")

policies <- unique(spid$policy)
for(i in 1:length(policies)){
  use.policy <- policies[i]
  use.data <- spid[which(spid$policy == use.policy),]
  use.adopters <- use.data[which(use.data$adopt==1),]
  adopt.years <- sort(unique(use.adopters$year))
  for(j in 5:length(adopt.years)){
    model <- glm(adopt~neighbor_prop + ideology_relative_hm + congress_majortopic + init_avail + init_qual + 
                  divided_gov + legprof_squire +  percap_log + population_log + year_count, 
                 data=use.data[which(use.data$year <= adopt.years[j]),], family=binomial(link="logit"))
  keep.neigh <- as.data.frame(cbind(use.policy, adopt.years[j], sum(use.data$adopt[use.data$year <= adopt.years[j]]), coef(model)[2], 
                              sqrt(diag(vcov(model)))[2], summary(model)$coefficients[2,4]))
  names(keep.neigh) <- c("policy", "year", "adopts_total", "coef", "se", "p")
  use.keep.neigh <- rbind(use.keep.neigh, keep.neigh)
  keep.ideo <- as.data.frame(cbind(use.policy, adopt.years[j], sum(use.data$adopt[use.data$year <= adopt.years[j]]), coef(model)[3], 
                                   sqrt(diag(vcov(model)))[3], summary(model)$coefficients[3,4]))
  names(keep.ideo) <- c("policy", "year", "adopts_total", "coef", "se", "p")
  use.keep.ideo <- rbind(use.keep.ideo, keep.ideo)
    }
}
use.keep.neigh$coef <- as.numeric(use.keep.neigh$coef)
use.keep.neigh$se <- as.numeric(use.keep.neigh$se)
use.keep.neigh$p <- as.numeric(use.keep.neigh$p)

use.keep.ideo$coef <- as.numeric(use.keep.ideo$coef)
use.keep.ideo$se <- as.numeric(use.keep.ideo$se)
use.keep.ideo$p <- as.numeric(use.keep.ideo$p)

#Figure 2 - Plot Coefficients for Neighbors
#Note: Figure title is not created in the code
png("figure2.png", height=6, width=8, units="in", res=600)
plot.new()
plot.window(xlim=c(0,50), ylim=c(-10,10))
for(l in 1:length(policies)){
  plot.policy <- use.keep.neigh[which(use.keep.neigh$policy==policies[l]),]
  lines(plot.policy$adopts_total, plot.policy$coef, col="black")
}
abline(h=0, col="red", lwd=2, lty=2)
axis(1, at=seq(0,50,10), labels=seq(0,50,10))
#axis(2, at=seq(-100,0,50), labels=seq(-100,0,50))
axis(2, at=seq(-10,10,10), labels=seq(-10,10,10))
abline(v=20)
title(main="", xlab="Number of Adopting States", ylab="Coefficient")
dev.off()

#Figure 3 - Plot p-values for Neighbors
#Note: Figure title is not created in the code
png("figure3.png", height=6, width=8, units="in", res=600)
plot.new()
plot.window(xlim=c(0,50), ylim=c(0,1))
for(l in 1:length(policies)){
  plot.policy <- use.keep.neigh[which(use.keep.neigh$policy==policies[l]),]
  if(min(plot.policy$p)<0.05){
  lines(plot.policy$adopts_total, plot.policy$p, col="gray")
    print(policies[l])
  }
}
lines(use.keep.neigh$adopts_total[use.keep.neigh$policy=="gambling_lottery_adoptio"], use.keep.neigh$p[use.keep.neigh$policy=="gambling_lottery_adoptio"], col="black", lwd=2)
text(5,.75, "Lottery")
lines(use.keep.neigh$adopts_total[use.keep.neigh$policy=="shoplift"], use.keep.neigh$p[use.keep.neigh$policy=="shoplift"], col="black", lwd=2)
text(25,.85, "Shoplifting")
lines(use.keep.neigh$adopts_total[use.keep.neigh$policy=="rapeshield"], use.keep.neigh$p[use.keep.neigh$policy=="rapeshield"], col="black", lwd=2)
text(45, .2, "Rape Shield")
abline(h=0.05, col="red", lwd=2, lty=2)
axis(1, at=seq(0,50,10), labels=seq(0,50,10))
axis(2, at=seq(0,1,0.05), labels=seq(0,1,0.05))
title(main="", xlab="Number of Adopting States", ylab="P-Value")
dev.off()

#Figure 4 - Plot Coefficients for Relative Ideology
#Note: Figure title is not created in the code
png("figure4.png", height=6, width=8, units="in", res=600)
plot.new()
plot.window(xlim=c(0,50), ylim=c(-.2,.2))
for(l in 1:length(policies)){
  plot.policy <- use.keep.ideo[which(use.keep.ideo$policy==policies[l]),]
  lines(plot.policy$adopts_total, plot.policy$coef, col="black")
}
abline(h=0, col="red", lwd=2, lty=2)
axis(1, at=seq(0,50,10), labels=seq(0,50,10))
axis(2, at=seq(-.2,.2,.1), labels=seq(-.2,.2,.1))
title(main="", xlab="Number of Adopting States", ylab="Coefficient")
dev.off()

#Figure 5 - Plot p-values for Relative Ideology
#Note: Figure title is not created in the code
png("figure5.png", height=6, width=8, units="in", res=600)
plot.new()
plot.window(xlim=c(0,50), ylim=c(0,1))
for(l in 1:length(policies)){
  plot.policy <- use.keep.ideo[which(use.keep.ideo$policy==policies[l]),]
  if(min(plot.policy$p)<0.05){
    lines(plot.policy$adopts_total, plot.policy$p, col="gray")
    print(policies[l])
  }
}
lines(use.keep.ideo$adopts_total[use.keep.ideo$policy=="hazing"], use.keep.ideo$p[use.keep.ideo$policy=="hazing"], col="black", lwd=2)
text(5,.4, "Hazing")
lines(use.keep.ideo$adopts_total[use.keep.ideo$policy=="child custody jurisdiction and enforcement act"], use.keep.ideo$p[use.keep.ideo$policy=="child custody jurisdiction and enforcement act"], col="black", lwd=2)
text(3,.02, "Child Custody")
lines(use.keep.ideo$adopts_total[use.keep.ideo$policy=="regulation_boehmke_livingwil"], use.keep.ideo$p[use.keep.ideo$policy=="regulation_boehmke_livingwil"], col="black", lwd=2)
text(45, 1, "Living Will")
abline(h=0.05, col="red", lwd=2, lty=2)
axis(1, at=seq(0,50,10), labels=seq(0,50,10))
axis(2, at=seq(0,1,0.05), labels=seq(0,1,0.05))
title(main="", xlab="Number of Adopting States", ylab="P-Value")
dev.off()

####### Supplemental Cox Analysis

## Add time2 counter for cox.ph modeling

spid$timecox2 <- spid$time + 1

use.keep.neigh <- data.frame(matrix(nrow=0, ncol=6))
names(use.keep.neigh) <- c("policy", "year", "adopts_total", "coef", "se", "p")

use.keep.ideo <- data.frame(matrix(nrow=0, ncol=6))
names(use.keep.ideo) <- c("policy", "year", "adopts_total", "coef", "se", "p")

policies <- unique(spid$policy)
for(i in 1:length(policies)){
  use.policy <- policies[i]
  use.data <- spid[which(spid$policy == use.policy),]
  use.adopters <- use.data[which(use.data$adopt==1),]
  adopt.years <- sort(unique(use.adopters$year))
  for(j in 5:length(adopt.years)){
    model <- coxph(Surv(time, timecox2, adopt)~neighbor_prop + ideology_relative_hm + init_avail + init_qual + 
                   divided_gov + legprof_squire +  percap_log + population_log, 
                 data=use.data[which(use.data$year <= adopt.years[j]),])
    keep.neigh <- as.data.frame(cbind(use.policy, adopt.years[j], sum(use.data$adopt[use.data$year <= adopt.years[j]]), coef(model)[1], 
                                      summary(model)$coefficients[1,3], summary(model)$coefficients[1,5]))
    names(keep.neigh) <- c("policy", "year", "adopts_total", "coef", "se", "p")
    use.keep.neigh <- rbind(use.keep.neigh, keep.neigh)
    keep.ideo <- as.data.frame(cbind(use.policy, adopt.years[j], sum(use.data$adopt[use.data$year <= adopt.years[j]]), coef(model)[2], 
                                     summary(model)$coefficients[2,3], summary(model)$coefficients[2,5]))
    names(keep.ideo) <- c("policy", "year", "adopts_total", "coef", "se", "p")
    use.keep.ideo <- rbind(use.keep.ideo, keep.ideo)
  }
}
use.keep.neigh$coef <- as.numeric(use.keep.neigh$coef)
use.keep.neigh$se <- as.numeric(use.keep.neigh$se)
use.keep.neigh$p <- as.numeric(use.keep.neigh$p)

use.keep.ideo$coef <- as.numeric(use.keep.ideo$coef)
use.keep.ideo$se <- as.numeric(use.keep.ideo$se)
use.keep.ideo$p <- as.numeric(use.keep.ideo$p)

#Figure A1 - Plot Coefficients for Neighbors
#Note: Figure title is not created in the code
png("figureA1.png", height=6, width=8, units="in", res=600)
plot.new()
plot.window(xlim=c(0,50), ylim=c(-5,5))
for(l in 1:length(policies)){
  plot.policy <- use.keep.neigh[which(use.keep.neigh$policy==policies[l]),]
  lines(plot.policy$adopts_total, plot.policy$coef, col="black")
}
abline(h=0, col="red", lwd=2, lty=2)
axis(1, at=seq(0,50,10), labels=seq(0,50,10))
axis(2, at=seq(-5,5,1), labels=seq(-5,5,1))
title(main="", xlab="Number of Adopting States", ylab="Coefficient")
dev.off()

#Figure A2 - Plot P=-values for Neighbors
#Note: Figure title is not created in the code
png("figureA2.png", height=6, width=8, units="in", res=600)
plot.new()
plot.window(xlim=c(0,50), ylim=c(0,1))
for(l in 1:length(policies)){
  plot.policy <- use.keep.neigh[which(use.keep.neigh$policy==policies[l]),]
  if(min(plot.policy$p)<0.05){
    lines(plot.policy$adopts_total, plot.policy$p, col="gray")
    print(policies[l])
  }
}
lines(use.keep.neigh$adopts_total[use.keep.neigh$policy=="gambling_lottery_adoptio"], use.keep.neigh$p[use.keep.neigh$policy=="gambling_lottery_adoptio"], col="black", lwd=2)
text(5,.75, "Lottery")
lines(use.keep.neigh$adopts_total[use.keep.neigh$policy=="shoplift"], use.keep.neigh$p[use.keep.neigh$policy=="shoplift"], col="black", lwd=2)
text(25,.85, "Shoplifting")
lines(use.keep.neigh$adopts_total[use.keep.neigh$policy=="rapeshield"], use.keep.neigh$p[use.keep.neigh$policy=="rapeshield"], col="black", lwd=2)
text(45, .2, "Rape Shield")
abline(h=0.05, col="red", lwd=2, lty=2)
axis(1, at=seq(0,50,10), labels=seq(0,50,10))
axis(2, at=seq(0,1,0.05), labels=seq(0,1,0.05))
title(main="", xlab="Number of Adopting States", ylab="P-Value")
dev.off()

#Figure A3 - Plot Coefficients for Relative Ideology
#Note: Figure title is not created in the code
png("figureA3.png", height=6, width=8, units="in", res=600)
plot.new()
plot.window(xlim=c(0,50), ylim=c(-.2,.2))
for(l in 1:length(policies)){
  plot.policy <- use.keep.ideo[which(use.keep.ideo$policy==policies[l]),]
  lines(plot.policy$adopts_total, plot.policy$coef, col="black")
}
abline(h=0, col="red", lwd=2, lty=2)
axis(1, at=seq(0,50,10), labels=seq(0,50,10))
#axis(2, at=seq(-100,0,50), labels=seq(-100,0,50))
axis(2, at=seq(-.2,.2,.1), labels=seq(-.2,.2,.1))
title(main="", xlab="Number of Adopting States", ylab="Coefficient")
dev.off()

#Figure A4 - Plot p-values for Relative Ideology
#Note: Figure title is not created in the code
png("figureA4.png", height=6, width=8, units="in", res=600)
plot.new()
plot.window(xlim=c(0,50), ylim=c(0,1))
for(l in 1:length(policies)){
  plot.policy <- use.keep.ideo[which(use.keep.ideo$policy==policies[l]),]
  if(min(plot.policy$p)<0.05){
    lines(plot.policy$adopts_total, plot.policy$p, col="gray")
    print(policies[l])
  }
}
lines(use.keep.ideo$adopts_total[use.keep.ideo$policy=="hazing"], use.keep.ideo$p[use.keep.ideo$policy=="hazing"], col="black", lwd=2)
text(5,.4, "Hazing")
lines(use.keep.ideo$adopts_total[use.keep.ideo$policy=="child custody jurisdiction and enforcement act"], use.keep.ideo$p[use.keep.ideo$policy=="child custody jurisdiction and enforcement act"], col="black", lwd=2)
text(3,.02, "Child Custody")
lines(use.keep.ideo$adopts_total[use.keep.ideo$policy=="regulation_boehmke_livingwil"], use.keep.ideo$p[use.keep.ideo$policy=="regulation_boehmke_livingwil"], col="black", lwd=2)
text(45, 1, "Living Will")
abline(h=0.05, col="red", lwd=2, lty=2)
axis(1, at=seq(0,50,10), labels=seq(0,50,10))
axis(2, at=seq(0,1,0.05), labels=seq(0,1,0.05))
title(main="", xlab="Number of Adopting States", ylab="P-Value")
dev.off()

#Completing estimation of run time for Readme
end_time <- Sys.time()
time_taken <- end_time - start_time
print(time_taken)
