## Replication Code for "The Political Legacies of Rebel Rule: Evidence from a Natural Experiment in Cote d'Ivoire"
## Dr. Philip Martin, Dr. Jeremy Speight, Dr. Giulia Piccolino 
## Software used: R version 4.0.5 (2021-03-31), R Studio, version 1.4.1106
## Please address queries to pmarti5@gmu.edu.


## load required packages
library(foreign)
library(haven)
library(readxl)
library(dplyr)
library(stargazer)
library(lmtest)
library(sandwich)
library("pastecs")
library(xtable)


## function for clustered standard errors 
vcovCluster <- function(
  model,
  cluster
)
{
  require(sandwich)
  require(lmtest)
  if(nrow(model.matrix(model))!=length(cluster)){
    stop("check your data: cluster variable has different N than model")
  }
  M <- length(unique(cluster))
  N <- length(cluster)           
  K <- model$rank   
  if(M<50){
    warning("Fewer than 50 clusters, variances may be unreliable (could try block bootstrap instead).")
  }
  dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
  uj  <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum));
  rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
  return(rcse.cov)
}


## function for extracting point estimates and confidence intervals
philplotfun <- function(object, dataset = dataset, df = NULL, level = 0.95, parm = NULL,
                        labels = TRUE, xlab = "Coefficient confidence intervals", ylab = "",
                        xlim = NULL, ylim = NULL,
                        las = 1, lwd = 1, lty = c(1, 2), pch = 19, col = 1,
                        length = 0, angle = 30, code = 3, ...)
{
  cf <- coef(object)[2]
  se <- sqrt(diag(vcovCluster(object, cluster = dataset$EANUMB)))[2]
  if(is.null(parm)) parm <- seq_along(cf)
  if(is.numeric(parm) | is.logical(parm)) parm <- names(cf)[parm]
  if(is.character(parm)) parm <- which(names(cf) %in% parm)
  cf <- cf[parm]
  se <- se[parm]
  k <- length(cf)
  
  if(is.null(df)) {
    df <- if(identical(class(object), "lm")) df.residual(object) else 0
  }
  
  critval <- if(df > 0 & is.finite(df)) {
    qt((1 - level)/2, df = df)
  } else {
    qnorm((1 - level)/2)
  }
  ci1 <- cf + critval * se
  ci2 <- cf - critval * se
  
  lwd <- rep(lwd, length.out = 2)
  lty <- rep(lty, length.out = 2)
  pch <- rep(pch, length.out = k)
  col <- rep(col, length.out = k)
  
  if(is.null(xlim)) xlim <- range(c(0, min(ci1), max(ci2)))
  if(is.null(ylim)) ylim <- c(1 - 0.05 * k, 1.05 * k)
  
  if(isTRUE(labels)) labels <- names(cf)
  if(identical(labels, FALSE)) labels <- ""
  labels <- rep(labels, length.out = k)
  
  out <- c(ci1, cf, ci2)
  return(out)
}


## load data
PTI <- read.csv("replication_data_full.csv")
d_sub <- read.csv("replication_data_sub.csv")


## Tables and Figures (main article)

# Table 2: Placebo Tests
placebo1 <- lm(mande~treat, data=d_sub)
placebo1 <- coeftest(placebo1, vcovHAC(placebo1, cluster=d_sub$EANUMB))
placebo2 <- lm(baoule~treat, data=d_sub)
placebo2 <- coeftest(placebo2, vcovHAC(placebo2, cluster=d_sub$EANUMB))
placebo3 <- lm(krou~treat, data=d_sub)
placebo3 <- coeftest(placebo3, vcovHAC(placebo3, cluster=d_sub$EANUMB))
placebo4 <- lm(christian~treat, data=d_sub)
placebo4 <- coeftest(placebo4, vcovHAC(placebo4, cluster=d_sub$EANUMB))
placebo5 <- lm(female~treat, data=d_sub)
placebo5 <- coeftest(placebo5, vcovHAC(placebo5, cluster=d_sub$EANUMB))
placebo8 <- lm(yearslived~treat, data=d_sub)
placebo8 <- coeftest(placebo8, vcovHAC(placebo8, cluster=d_sub$EANUMB))
placebo6 <- lm(education_2002 ~ treat, data = d_sub)
placebo6 <- coeftest(placebo6, vcovHAC(placebo6, cluster=d_sub$EANUMB))
placebo7 <- lm(poverty_2002~treat, data=d_sub)
placebo7 <- coeftest(placebo7, vcovHAC(placebo7, cluster=d_sub$EANUMB))

stargazer(placebo6, placebo7, placebo1, placebo2, placebo3, placebo4, placebo5, placebo8,
          no.space = TRUE, 
          column.labels = c("Education", "Poverty", "Mande", "Baoule", "Krou", "Christian", "Female", "Years Lived"),
          title = "Placebo Tests",
          out = "Placebo.tex")

# Figure 2: Estimated Impacts of Rebel Rule (OLS)

mod_attitude_pcorps <- lm(attitude_pcorps ~ treat, data = d_sub)
mod_attitude_pcorps_cr <- coeftest(mod_attitude_pcorps, vcovCluster(mod_attitude_pcorps, cluster=d_sub$EANUMB))
mod_attitude_police <- lm(attitude_police ~ treat, data = d_sub)
mod_attitude_police_cr <- coeftest(mod_attitude_police, vcovCluster(mod_attitude_police, cluster=d_sub$EANUMB))
mod_attitude_maire <- lm(attitude_maire ~ treat, data = d_sub)
mod_attitude_maire_cr <- coeftest(mod_attitude_maire, vcovCluster(mod_attitude_maire, cluster=d_sub$EANUMB))
mod_govattitudeindex <- lm(govattitudeindex ~ treat, data = d_sub)
mod_govattitudeindex_cr <- coeftest(mod_govattitudeindex, vcovCluster(mod_govattitudeindex, cluster=d_sub$EANUMB))
mod_attitude_taxes <- lm(attitude_taxes ~ treat, data = d_sub)
mod_attitude_taxes_cr <- coeftest(mod_attitude_taxes, vcovCluster(mod_attitude_taxes, cluster=d_sub$EANUMB))
mod_attitude_voting <- lm(attitude_voting ~ treat, data = d_sub)
mod_attitude_voting_cr <- coeftest(mod_attitude_voting, vcovCluster(mod_attitude_voting, cluster=d_sub$EANUMB))
d_subeligible2020 <- subset(d_sub, d_sub$Q15=="Oui")
mod_vote2020 <- lm(vote2020 ~ treat, data = d_subeligible2020)
mod_vote2020_cr <- coeftest(mod_vote2020, vcovCluster(mod_vote2020, cluster=d_subeligible2020$EANUMB))
mod_agree_demandintervention <- lm(agree_demandintervention ~ treat, data = d_sub)
mod_agree_demandintervention_cr <- coeftest(mod_agree_demandintervention, vcovCluster(mod_agree_demandintervention, cluster=d_sub$EANUMB))
mod_agree_contactmedia <- lm(agree_contactmedia ~ treat, data = d_sub)
mod_agree_contactmedia_cr <- coeftest(mod_agree_contactmedia, vcovCluster(mod_agree_contactmedia, cluster=d_sub$EANUMB))
mod_agree_contactgov <- lm(agree_contactgov ~ treat, data = d_sub)
mod_agree_contactgov_cr <- coeftest(mod_agree_contactgov, vcovCluster(mod_agree_contactgov, cluster=d_sub$EANUMB))
mod_civicactivismindex <- lm(civicactivismindex ~ treat, data = d_sub)
mod_civicactivismindex_cr <- coeftest(mod_civicactivismindex, vcovCluster(mod_civicactivismindex, cluster=d_sub$EANUMB))
mod_agree_refusetaxes <- lm(agree_refusetaxes ~ treat, data = d_sub)
mod_agree_refusetaxes_cr <- coeftest(mod_agree_refusetaxes, vcovCluster(mod_agree_refusetaxes, cluster=d_sub$EANUMB))
mod_agree_barrage <- lm(agree_barrage ~ treat, data = d_sub)
mod_agree_barrage_cr <- coeftest(mod_agree_barrage, vcovCluster(mod_agree_barrage, cluster=d_sub$EANUMB))
mod_agree_occupy <- lm(agree_occupy ~ treat, data = d_sub)
mod_agree_occupy_cr <- coeftest(mod_agree_occupy, vcovCluster(mod_agree_occupy, cluster=d_sub$EANUMB))
mod_antistateactivismindex <- lm(antistateactivismindex ~ treat, data = d_sub)
mod_antistateactivismindex_cr <- coeftest(mod_antistateactivismindex, vcovCluster(mod_antistateactivismindex, cluster=d_sub$EANUMB))

out.matrix <- as.data.frame(matrix(nrow=26, ncol = 3, NA))
out.matrix[1,] <- philplotfun(mod_attitude_pcorps, dataset = d_sub)
out.matrix[2,] <- philplotfun(mod_attitude_police, dataset = d_sub)
out.matrix[3,] <- philplotfun(mod_attitude_maire, dataset = d_sub)
out.matrix[4,] <- philplotfun(mod_govattitudeindex, dataset = d_sub)
out.matrix[10,] <- philplotfun(mod_attitude_taxes, dataset = d_sub)
out.matrix[11,] <- philplotfun(mod_attitude_voting, dataset = d_sub)
out.matrix[12,] <- philplotfun(mod_agree_demandintervention, dataset = d_sub)
out.matrix[13,] <- philplotfun(mod_agree_contactmedia, dataset = d_sub)
out.matrix[14,] <- philplotfun(mod_agree_contactgov, dataset = d_sub)
out.matrix[15,] <- philplotfun(mod_civicactivismindex, dataset = d_sub)
out.matrix[16,] <- philplotfun(mod_agree_refusetaxes, dataset = d_sub)
out.matrix[17,] <- philplotfun(mod_agree_barrage, dataset = d_sub)
out.matrix[18,] <- philplotfun(mod_agree_occupy, dataset = d_sub)
out.matrix[19,] <- philplotfun(mod_antistateactivismindex, dataset = d_sub)

pdf(file="Results_bivariate.pdf", width = 8.5, height = 14)
par(mar=c(4,20,2,2),
    mfrow=c(3,1))
plot(0, 0, type = "n", las = 1, ylim = c(0,5), xlab = "",
     ylab = "", axes=FALSE, 
     main = "Attitudes About Local State Institutions",
     cex.main = 1.5)
points(out.matrix$V2[1:4], 1:4, pch = 19, col = 1)
abline(v = 0, lty = 2, lwd = 2)
abline(v = c(-.5, .5), lty=2, lwd = 1, col = "gray")
abline(h = c(0:5), lty = 2, lwd = 1, col = "gray")
arrows(out.matrix$V1[1:4], 1:4, out.matrix$V3[1:4], 1:4, lty = 1, lwd = c(1,1,1,3), col = 1,
       angle = 90, length = 0.05, code = 3)
axis(1)
axis(2, at = 1:4, labels = c('Prefectoral corps','Police / Gendarmerie','Municipal government','State attitudes index (0-3)'), las = 1, cex.axis=1.75)
box()

plot(0, 0, type = "n", las = 1, xlim = c(-1.5,1.5), ylim = c(0,7), xlab="",
     ylab = "", axes=FALSE, cex.main = 1.5,
     main = "Attitudes About Civic Obligations")
points(out.matrix$V2[10:15], 1:6, pch = 19, col = 1)
abline(v = 0, lty = 2, lwd = 2)
abline(v = c(-1, -.5, .5, 1), lty=2, lwd = 1, col = "gray")
abline(h = c(1:6), lty = 2, lwd = 1, col = "gray")
arrows(out.matrix$V1[10:15], 1:6, out.matrix$V3[10:15], 1:6, lty = 1, lwd = c(1,1,1,1,1,3), col = 1,
       angle = 90, length = 0.05, code = 3)
axis(1)
axis(2, at = 1:6, labels = c('Should pay taxes','Should vote in elections','Should organize with others','Should contact media', 'Should contact government', 'Civic obligations index (0-5)'), las = 1, cex.axis = 1.75)
box()

plot(0, 0, type = "n", las = 1, ylim = c(0,5), xlab="Coefficient confidence intervals",
     ylab = "", axes=FALSE, cex.main = 1.5, cex.lab = 1.5,
     main = "Attitudes About Anti-State Actions")
points(out.matrix$V2[16:19], 1:4, pch = 19, col = 1)
abline(v = 0, lty = 2, lwd = 2)
abline(v = c(-1, -.5, .5, 1), lty=2, lwd = 1, col = "gray")
abline(h = c(1:4), lty = 2, lwd = 1, col = "gray")
arrows(out.matrix$V1[16:19], 1:4, out.matrix$V3[16:19], 1:4, lty = 1, lwd = c(1,1,1,3), col = 1,
       angle = 90, length = 0.05, code = 3)
axis(1)
axis(2, at = 1:4, labels = c('Refuse taxes','Road block','Occupy public building','Anti-state index (0-3)'), las = 1, cex.axis = 1.75)
box()

dev.off()

# Figure 3: Heterogenous Effects by Ethnicity

krou <- subset(d_sub, d_sub$krou==1)
akan <- subset(d_sub, d_sub$akan==1)
mande <- subset(d_sub, d_sub$mande==1)

krou_govattitudeindex <- lm(govattitudeindex ~ treat, data = krou)
krou_govattitudeindex_cr <- coeftest(krou_govattitudeindex, vcov = vcovCluster(krou_govattitudeindex, cluster = krou$EANUMB))
krou_publictrustindex <- lm(publictrust_index ~ treat, data = krou)
krou_publictrustindex_cr <- coeftest(krou_publictrustindex, vcov = vcovCluster(krou_publictrustindex, cluster = krou$EANUMB))
krou_civicactivismindex <- lm(civicactivismindex ~ treat, data = krou)
krou_civicactivismindex_cr <- coeftest(krou_civicactivismindex, vcov = vcovCluster(krou_civicactivismindex, cluster = krou$EANUMB))
krou_antistateattitudes <- lm(antistateactivismindex ~ treat, data = krou)
krou_antistateattitudes_cr <- coeftest(krou_antistateattitudes, vcov = vcovCluster(krou_antistateattitudes, cluster = krou$EANUMB))

akan_govattitudeindex <- lm(govattitudeindex ~ treat, data = akan)
akan_govattitudeindex_cr <- coeftest(akan_govattitudeindex, vcov = vcovCluster(akan_govattitudeindex, cluster = akan$EANUMB))
akan_publictrustindex <- lm(publictrust_index ~ treat, data = akan)
akan_publictrustindex_cr <- coeftest(akan_publictrustindex, vcov = vcovCluster(akan_publictrustindex, cluster = akan$EANUMB))
akan_civicactivismindex <- lm(civicactivismindex ~ treat, data = akan)
akan_civicactivismindex_cr <- coeftest(akan_civicactivismindex, vcov = vcovCluster(akan_civicactivismindex, cluster = akan$EANUMB))
akan_antistateattitudes <- lm(antistateactivismindex ~ treat, data = akan)
akan_antistateattitudes_cr <- coeftest(akan_antistateattitudes, vcov = vcovCluster(akan_antistateattitudes, cluster = akan$EANUMB))

mande_govattitudeindex <- lm(govattitudeindex ~ treat, data = mande)
mande_govattitudeindex_cr <- coeftest(mande_govattitudeindex, vcov = vcovCluster(mande_govattitudeindex, cluster = mande$EANUMB))
mande_publictrustindex <- lm(publictrust_index ~ treat, data = mande)
mande_publictrustindex_cr <- coeftest(mande_publictrustindex, vcov = vcovCluster(mande_publictrustindex, cluster = mande$EANUMB))
mande_civicactivismindex <- lm(civicactivismindex ~ treat, data = mande)
mande_civicactivismindex_cr <- coeftest(mande_civicactivismindex, vcov = vcovCluster(mande_civicactivismindex, cluster = mande$EANUMB))
mande_antistateattitudes <- lm(antistateactivismindex ~ treat, data = mande)
mande_antistateattitudes_cr <- coeftest(mande_antistateattitudes, vcov = vcovCluster(mande_antistateattitudes, cluster = mande$EANUMB))

het.mat <- as.data.frame(matrix(nrow=15, ncol = 3, NA))
het.mat[1,] <- philplotfun(krou_govattitudeindex, dataset = krou)
het.mat[2,] <- philplotfun(akan_govattitudeindex, dataset = akan)
het.mat[3,] <- philplotfun(mande_govattitudeindex, dataset = mande)
het.mat[4,] <- philplotfun(krou_publictrustindex, dataset = krou)
het.mat[5,] <- philplotfun(akan_publictrustindex, dataset = akan)
het.mat[6,] <- philplotfun(mande_govattitudeindex, dataset = mande)
het.mat[7,] <- philplotfun(krou_civicactivismindex, dataset = krou)
het.mat[8,] <- philplotfun(akan_civicactivismindex, dataset = akan)
het.mat[9,] <- philplotfun(mande_civicactivismindex, dataset = mande)
het.mat[10,] <- philplotfun(krou_antistateattitudes, dataset = krou)
het.mat[11,] <- philplotfun(akan_antistateattitudes, dataset = akan)
het.mat[12,] <- philplotfun(mande_antistateattitudes, dataset = mande)

pdf(file="Results_byethnicity.pdf")
par(mar=c(4,10,2,2),
    mfrow=c(3,1))
plot(0, 0, type = "n", las = 1, ylim = c(0,4), xlim = c(-1.75, 1.75), xlab = "",
     ylab = "", axes=FALSE, cex.main = 1.5, 
     main = "Attitudes about Local State Institutions (0-3 index)")
points(het.mat$V2[1:3], 1:3, pch = 19, col = c(4:2))
abline(v = 0, lty = 2, lwd = 2)
abline(v = c(-.5, .5), lty=2, lwd = 1, col = "gray")
abline(h = c(1:3), lty = 2, lwd = 1, col = "gray")
arrows(het.mat$V1[1:3], 1:3, het.mat$V3[1:3], 1:3, lty = 1, lwd = 1, col = c(4:2),
       angle = 90, length = 0.05, code = 3)
axis(1)
axis(2, at = 1:3, labels = c('Mande','Akan','Krou'), las = 1, cex.axis=1.75)
box()

plot(0, 0, type = "n", las = 1, ylim = c(0,4), xlim = c(-1.75, 1.75), xlab = "",
     ylab = "", axes=FALSE, cex.main = 1.5,
     main = "Attitudes About Civic Obligations (0-5 index)")
points(het.mat$V2[7:9], 1:3, pch = 19, col = c(4:2))
abline(v = 0, lty = 2, lwd = 2)
abline(v = c(-.5, .5), lty=2, lwd = 1, col = "gray")
abline(h = c(1:3), lty = 2, lwd = 1, col = "gray")
arrows(het.mat$V1[7:9], 1:3, het.mat$V3[7:9], 1:3, lty = 1, lwd = 1, col = c(4:2),
       angle = 90, length = 0.05, code = 3)
axis(1)
axis(2, at = 1:3, labels = c('Mande','Akan','Krou'), las = 1, cex.axis=1.75)
box()

plot(0, 0, type = "n", las = 1, ylim = c(0,4), xlim = c(-1.75, 1.75),
     ylab = "", axes=FALSE, cex.main=1.5, xlab = "Coefficient confidence intervals",
     main = "Attitudes About Anti-State Actions (0-3 index)")
points(het.mat$V2[10:12], 1:3, pch = 19, col = c(4:2))
abline(v = 0, lty = 2, lwd = 2)
abline(v = c(-.5, .5), lty=2, lwd = 1, col = "gray")
abline(h = c(1:3), lty = 2, lwd = 1, col = "gray")
arrows(het.mat$V1[10:12], 1:3, het.mat$V3[10:12], 1:3, lty = 1, lwd = 1, col = c(4:2),
       angle = 90, length = 0.05, code = 3)
axis(1)
axis(2, at = 1:3, labels = c('Mande','Akan','Krou'), las = 1, cex.axis=1.75)
box()

dev.off()

#Figure 4: Heterogenous Effects by Presence of Permanent Rebel Camp

camp <- subset(d_sub, d_sub$permanentcamp==1)
nocamp <- subset(d_sub, d_sub$permanentcamp==0)

camp_govattitudeindex <- lm(govattitudeindex ~ treat, data = camp)
camp_govattitudeindex_cr <- coeftest(camp_govattitudeindex, vcov = vcovCluster(camp_govattitudeindex, cluster = camp$EANUMB))
camp_publictrustindex <- lm(publictrust_index ~ treat, data = camp)
camp_publictrustindex_cr <- coeftest(camp_publictrustindex, vcov = vcovCluster(camp_publictrustindex, cluster = camp$EANUMB))
camp_civicactivismindex <- lm(civicactivismindex ~ treat, data = camp)
camp_civicactivismindex_cr <- coeftest(camp_civicactivismindex, vcov = vcovCluster(camp_civicactivismindex, cluster = camp$EANUMB))
camp_antistateattitudes <- lm(antistateactivismindex ~ treat, data = camp)
camp_antistateattitudes_cr <- coeftest(camp_antistateattitudes, vcov = vcovCluster(camp_antistateattitudes, cluster = camp$EANUMB))

nocamp_govattitudeindex <- lm(govattitudeindex ~ treat, data = nocamp)
nocamp_govattitudeindex_cr <- coeftest(nocamp_govattitudeindex, vcov = vcovCluster(nocamp_govattitudeindex, cluster = nocamp$EANUMB))
nocamp_publictrustindex <- lm(publictrust_index ~ treat, data = nocamp)
nocamp_publictrustindex_cr <- coeftest(nocamp_publictrustindex, vcov = vcovCluster(nocamp_publictrustindex, cluster = nocamp$EANUMB))
nocamp_civicactivismindex <- lm(civicactivismindex ~ treat, data = nocamp)
nocamp_civicactivismindex_cr <- coeftest(nocamp_civicactivismindex, vcov = vcovCluster(nocamp_civicactivismindex, cluster = nocamp$EANUMB))
nocamp_antistateattitudes <- lm(antistateactivismindex ~ treat, data = nocamp)
nocamp_antistateattitudes_cr <- coeftest(nocamp_antistateattitudes, vcov = vcovCluster(nocamp_antistateattitudes, cluster = nocamp$EANUMB))

het.mat <- as.data.frame(matrix(nrow=10, ncol = 3, NA))
het.mat[1,] <- philplotfun(camp_govattitudeindex, dataset = camp)
het.mat[2,] <- philplotfun(nocamp_govattitudeindex, dataset = nocamp)
het.mat[3,] <- philplotfun(camp_publictrustindex, dataset = camp)
het.mat[4,] <- philplotfun(nocamp_publictrustindex, dataset = nocamp)
het.mat[5,] <- philplotfun(camp_civicactivismindex, dataset = camp)
het.mat[6,] <- philplotfun(nocamp_civicactivismindex, dataset = nocamp)
het.mat[7,] <- philplotfun(camp_antistateattitudes, dataset = camp)
het.mat[8,] <- philplotfun(nocamp_antistateattitudes, dataset = nocamp)

pdf(file="Results_bygovernance.pdf")
par(mar=c(4,10,2,2),
    mfrow=c(3,1))
plot(0, 0, type = "n", las = 1, ylim = c(0,3), xlim = c(-1.75, 1.75), xlab = "",
     ylab = "", axes=FALSE, cex.main = 1.5,
     main = "Attitudes about Local State Institutions (0-3 index)")
points(het.mat$V2[1:2], 1:2, pch = 19, col = c(4:3))
abline(v = 0, lty = 2, lwd = 2)
abline(v = c(-.5, .5), lty=2, lwd = 1, col = "gray")
abline(h = c(1:2), lty = 2, lwd = 1, col = "gray")
arrows(het.mat$V1[1:2], 1:2, het.mat$V3[1:2], 1:2, lty = 1, lwd = 1, col = c(4:3),
       angle = 90, length = 0.05, code = 3)
axis(1)
axis(2, at = 1:2, labels = c('Camp', "No Camp"), las = 1, cex.axis=1.75)
box()

plot(0, 0, type = "n", las = 1, ylim = c(0,3), xlim = c(-1.75, 1.75), xlab = "",
     ylab = "", axes=FALSE, cex.main=1.5,
     main = "Attitudes About Civic Obligations (0-5 index)")
points(het.mat$V2[5:6], 1:2, pch = 19, col = c(4:3))
abline(v = 0, lty = 2, lwd = 2)
abline(v = c(-.5, .5), lty=2, lwd = 1, col = "gray")
abline(h = c(1:2), lty = 2, lwd = 1, col = "gray")
arrows(het.mat$V1[5:6], 1:2, het.mat$V3[5:6], 1:2, lty = 1, lwd = 1, col = c(4:3),
       angle = 90, length = 0.05, code = 3)
axis(1)
axis(2, at = 1:2, labels = c('Camp','No Camp'), las = 1, cex.axis=1.75)
box()

plot(0, 0, type = "n", las = 1, ylim = c(0,3), xlim = c(-1.75, 1.75),
     ylab = "", axes=FALSE, cex.main = 1.5, xlab = "Coefficient confidence intervals",
     main = "Attitudes About Anti-State Actions (0-3 index)")
points(het.mat$V2[7:8], 1:2, pch = 19, col = c(4:3))
abline(v = 0, lty = 2, lwd = 2)
abline(v = c(-.5, .5), lty=2, lwd = 1, col = "gray")
abline(h = c(1:2), lty = 2, lwd = 1, col = "gray")
arrows(het.mat$V1[7:8], 1:2, het.mat$V3[7:8], 1:2, lty = 1, lwd = 1, col = c(4:3),
       angle = 90, length = 0.05, code = 3)
axis(1)
axis(2, at = 1:2, labels = c('Camp','No Camp'), las = 1, cex.axis=1.75)
box()

dev.off()


## Tables and Figures (appendix)

# Table A1: Descriptive Statistics
sum_dat <- d_sub[, c("treat", "age", "christian", "muslim", "mande", "akan",
                     "krou", "education_2002", "female", "poverty_2002", "victim", "urban",
                     "socialassistance_2002", "debtinformal_2002", "wouldadopt_2002", "prewarsocialcapitalindex",
                     "permanentcamp", "forcesimpartiales", "ngo", "paidtax_war",
                     "order_wartime_DUMMY", "education_wartime_DUMMY", "health_wartime_DUMMY", 
                     "RDR_2001", "acled_events", "cacao_2002", "SQmean", "PREC_ANNUAL",
                     "govattitudeindex", "attitude_maire", "attitude_police", "attitude_pcorps",
                     "civicactivismindex", "agree_contactgov", "agree_contactmedia", "agree_demandintervention", "attitude_voting", "attitude_taxes",
                     "antistateactivismindex", "agree_occupy", "agree_barrage", "agree_refusetaxes",
                     "behaviorindex", "meeting_community", "volunteered", "villagegovernance", "meeting_party", "joinedCSO", "joinedparty")]
sumtab_1 <- t(stat.desc(sum_dat))[, c("nbr.val", "min", "max", "median",
                                      "mean", "std.dev")]
varnames <- c("Treated", "Age", "Christian", "Muslim", "Mande", "Akan", "Krou",
              "Education", "Female", "Poverty", "Victim", "Urban",
              "Social Assistance from Neighbor (prewar)", "Received Loan from Neighbor (prewar)", "Willing to Adopt Child (prewar)",
              "Social Capital Index (prewar)", "Permanent Camp", "Peacekeeping", "NGO", "Paid Taxes to Rebels",
              "Wartime Order", "Wartime Education", "Wartime Health",
              "RDR 2001 Vote", "ACLED Events (fall 2002)", "Cocoa Farmer", "Soil Quality", "Annual Precipitation",
              "State attitudes index", "Municipal government", "Policy / Gendarmerie", "Prefectoral corps",
              "Civic obligations index", "Should contact government", "Should contact media", "Should organize with others", "Should vote in elections", "Should pay taxes",
              "Anti-state actions index", "Occupy public building", "Road block", "Refuse taxes",
              "Collective action index", "Attended meeting (community)", "Volunteered in community", "Participated village governance", "Attended meeting (party)", "Joined CSO", "Joined party")
row.names(sumtab_1) <- varnames
colnames(sumtab_1) <- c("N obs", "Min", "Max", "Median", "Mean", "Std. Dev.")
sumtab_1 <- round(sumtab_1, 2)

sum_tab1x <- xtable(sumtab_1, caption = "Descriptive Statistics", type = "latex")
stargazer(sumtab_1,
          digits=2,
          title = "Descriptive Statistics", 
          out = "Descriptive Statistics.tex")

# Table A2: Placebo Tests (additional covariates)
placebo13 <- lm(RDR_2001 ~ treat, data = d_sub)
placebo13 <- coeftest(placebo13, vcovHAC(placebo13, cluster=d_sub$EANUMB))
placebo14 <- lm(acled_events ~ treat, data=d_sub)
placebo14 <- coeftest(placebo14, vcovHAC(placebo14, cluster=d_sub$EANUMB))
placebo15 <- lm(cacao_2002 ~ treat, data=d_sub)
placebo15 <- coeftest(placebo15, vcovHAC(placebo15, cluster=d_sub$EANUMB))
placebo17 <- lm(SQmean ~ treat, data=d_sub)
placebo17 <- coeftest(placebo17, vcovHAC(placebo17, cluster=d_sub$EANUMB))
placebo18 <- lm(PREC_ANNUAL ~ treat, data=d_sub)
placebo18 <- coeftest(placebo18, vcovHAC(placebo18, cluster=d_sub$EANUMB))

stargazer(placebo13, placebo14, placebo15, placebo17, placebo18,
          no.space = TRUE,
          column.labels = c("RDR 2001 Vote", "ACLED Events (fall 2002)", "Cocoa Farmer", "Soil Quality", "Annual Precipitation"),
          title = "Placebo Tests (additional covariates)",
          out = "Placebo3.tex")

# Table A3: Placebo Outcome Years Lived by Ethnic Group
yearsmod2 <- lm(yearslived ~ treat, data=krou)
yearsmod3 <- lm(yearslived ~ treat, data=mande)
yearsmod4 <- lm(yearslived ~ treat, data=akan)

stargazer(yearsmod2, yearsmod3, yearsmod4,
          no.space=TRUE,
          title = "Years Lived and Treatment Status by Ethnic Group",
          out = "YearsLivedMods.tex")

# Table A4: Constraining Sample Based on Years Lived in Current Enumeration Area
d_sub1 <- subset(d_sub, d_sub$yearslived > 7)
d_sub2 <- subset(d_sub, d_sub$yearslived > 15)

migrationmod1 <- lm(govattitudeindex ~ treat, data = d_sub1)
migrationmod1.c <- coeftest(migrationmod1, vcovCluster(migrationmod1, cluster=d_sub1$EANUMB))
migrationmod2 <- lm(civicactivismindex ~ treat, data = d_sub1)
migrationmod2.c <- coeftest(migrationmod2, vcovCluster(migrationmod2, cluster=d_sub1$EANUMB))
migrationmod3 <- lm(antistateactivismindex ~ treat, data = d_sub1)
migrationmod3.c <- coeftest(migrationmod3, vcovCluster(migrationmod3, cluster=d_sub1$EANUMB))

migrationmod4 <- lm(govattitudeindex ~ treat, data = d_sub2)
migrationmod4.c <- coeftest(migrationmod4, vcovCluster(migrationmod4, cluster=d_sub2$EANUMB))
migrationmod5 <- lm(civicactivismindex ~ treat, data = d_sub2)
migrationmod5.c <- coeftest(migrationmod5, vcovCluster(migrationmod5, cluster=d_sub2$EANUMB))
migrationmod6 <- lm(antistateactivismindex ~ treat, data = d_sub2)
migrationmod6.c <- coeftest(migrationmod6, vcovCluster(migrationmod6, cluster=d_sub2$EANUMB))

stargazer(migrationmod1.c, migrationmod2.c, migrationmod3.c, migrationmod4.c, migrationmod5.c, migrationmod6.c,
          no.space = TRUE,
          title = "Constraining Sample Based on Years Lived in Current EA",
          out = "MigrationChecks.tex")

# Figure A2: Main Results with Covariates Included
mod_attitude_pcorps <- lm(attitude_pcorps ~ treat + age + female + christian + mande + education_2002 + poverty_2002 + yearslived, data = d_sub)
mod_attitude_pcorps_cr <- coeftest(mod_attitude_pcorps, vcovCluster(mod_attitude_pcorps, cluster=d_sub$EANUMB))
mod_attitude_police <- lm(attitude_police ~ treat + age + female + christian + mande + education_2002 + poverty_2002 + yearslived, data = d_sub)
mod_attitude_police_cr <- coeftest(mod_attitude_police, vcovCluster(mod_attitude_police, cluster=d_sub$EANUMB))
mod_attitude_maire <- lm(attitude_maire ~ treat + age + female + christian + mande + education_2002 + poverty_2002 + yearslived, data = d_sub)
mod_attitude_maire_cr <- coeftest(mod_attitude_maire, vcovCluster(mod_attitude_maire, cluster=d_sub$EANUMB))
mod_govattitudeindex <- lm(govattitudeindex ~ treat + age + female + christian + mande + education_2002 + poverty_2002 + yearslived, data = d_sub)
mod_govattitudeindex_cr <- coeftest(mod_govattitudeindex, vcovCluster(mod_govattitudeindex, cluster=d_sub$EANUMB))
mod_attitude_taxes <- lm(attitude_taxes ~ treat + age + female + christian + mande + education_2002 + poverty_2002 + yearslived, data = d_sub)
mod_attitude_taxes_cr <- coeftest(mod_attitude_taxes, vcovCluster(mod_attitude_taxes, cluster=d_sub$EANUMB))
mod_attitude_voting <- lm(attitude_voting ~ treat + age + female + christian + mande + education_2002 + poverty_2002 + yearslived, data = d_sub)
mod_attitude_voting_cr <- coeftest(mod_attitude_voting, vcovCluster(mod_attitude_voting, cluster=d_sub$EANUMB))
d_subeligible2020 <- subset(d_sub, d_sub$Q15=="Oui")
mod_vote2020 <- lm(vote2020 ~ treat + age + female + christian + mande + education_2002 + poverty_2002 + yearslived, data = d_subeligible2020)
mod_vote2020_cr <- coeftest(mod_vote2020, vcovCluster(mod_vote2020, cluster=d_subeligible2020$EANUMB))
mod_agree_demandintervention <- lm(agree_demandintervention ~ treat + age + female + christian + mande + education_2002 + poverty_2002 + yearslived, data = d_sub)
mod_agree_demandintervention_cr <- coeftest(mod_agree_demandintervention, vcovCluster(mod_agree_demandintervention, cluster=d_sub$EANUMB))
mod_agree_contactmedia <- lm(agree_contactmedia ~ treat + age + female + christian + mande + education_2002 + poverty_2002 + yearslived, data = d_sub)
mod_agree_contactmedia_cr <- coeftest(mod_agree_contactmedia, vcovCluster(mod_agree_contactmedia, cluster=d_sub$EANUMB))
mod_agree_contactgov <- lm(agree_contactgov ~ treat + age + female + christian + mande + education_2002 + poverty_2002 + yearslived, data = d_sub)
mod_agree_contactgov_cr <- coeftest(mod_agree_contactgov, vcovCluster(mod_agree_contactgov, cluster=d_sub$EANUMB))
mod_civicactivismindex <- lm(civicactivismindex ~ treat + age + female + christian + mande + education_2002 + poverty_dummy + yearslived, data = d_sub)
mod_civicactivismindex_cr <- coeftest(mod_civicactivismindex, vcovCluster(mod_civicactivismindex, cluster=d_sub$EANUMB))
mod_agree_refusetaxes <- lm(agree_refusetaxes ~ treat + age + female + christian + mande + education_2002 + poverty_2002 + yearslived, data = d_sub)
mod_agree_refusetaxes_cr <- coeftest(mod_agree_refusetaxes, vcovCluster(mod_agree_refusetaxes, cluster=d_sub$EANUMB))
mod_agree_barrage <- lm(agree_barrage ~ treat + age + female + christian + mande + education_2002 + poverty_2002 + yearslived, data = d_sub)
mod_agree_barrage_cr <- coeftest(mod_agree_barrage, vcovCluster(mod_agree_barrage, cluster=d_sub$EANUMB))
mod_agree_occupy <- lm(agree_occupy ~ treat + age + female + christian + mande + education_2002 + poverty_2002 + yearslived, data = d_sub)
mod_agree_occupy_cr <- coeftest(mod_agree_occupy, vcovCluster(mod_agree_occupy, cluster=d_sub$EANUMB))
mod_antistateactivismindex <- lm(antistateactivismindex ~ treat + age + female + christian + mande + education_2002 + poverty_2002 + yearslived, data = d_sub)
mod_antistateactivismindex_cr <- coeftest(mod_antistateactivismindex, vcovCluster(mod_antistateactivismindex, cluster=d_sub$EANUMB))

out.matrix <- as.data.frame(matrix(nrow=26, ncol = 3, NA))
out.matrix[1,] <- philplotfun(mod_attitude_pcorps, dataset = d_sub)
out.matrix[2,] <- philplotfun(mod_attitude_police, dataset = d_sub)
out.matrix[3,] <- philplotfun(mod_attitude_maire, dataset = d_sub)
out.matrix[4,] <- philplotfun(mod_govattitudeindex, dataset = d_sub)
out.matrix[10,] <- philplotfun(mod_attitude_taxes, dataset = d_sub)
out.matrix[11,] <- philplotfun(mod_attitude_voting, dataset = d_sub)
out.matrix[12,] <- philplotfun(mod_agree_demandintervention, dataset = d_sub)
out.matrix[13,] <- philplotfun(mod_agree_contactmedia, dataset = d_sub)
out.matrix[14,] <- philplotfun(mod_agree_contactgov, dataset = d_sub)
out.matrix[15,] <- philplotfun(mod_civicactivismindex, dataset = d_sub)
out.matrix[16,] <- philplotfun(mod_agree_refusetaxes, dataset = d_sub)
out.matrix[17,] <- philplotfun(mod_agree_barrage, dataset = d_sub)
out.matrix[18,] <- philplotfun(mod_agree_occupy, dataset = d_sub)
out.matrix[19,] <- philplotfun(mod_antistateactivismindex, dataset = d_sub)

pdf(file="Results_combined.pdf", width = 8.5, height = 14)
par(mar=c(4,14,2,2),
    mfrow=c(3,1))
plot(0, 0, type = "n", las = 1, ylim = c(0,5), xlab = "",
     ylab = "", axes=FALSE, 
     main = "Attitudes About Local State Institutions")
points(out.matrix$V2[1:4], 1:4, pch = 19, col = 1)
abline(v = 0, lty = 2, lwd = 2)
abline(v = c(-.5, .5), lty=2, lwd = 1, col = "gray")
abline(h = c(0:5), lty = 2, lwd = 1, col = "gray")
arrows(out.matrix$V1[1:4], 1:4, out.matrix$V3[1:4], 1:4, lty = 1, lwd = c(1,1,1,3), col = 1,
       angle = 90, length = 0.05, code = 3)
axis(1)
axis(2, at = 1:4, labels = c('Prefectoral corps','Police / Gendarmerie','Municipal government','State attitudes index (0-3)'), las = 1)
box()
#dev.off()

plot(0, 0, type = "n", las = 1, xlim = c(-1.5,1.5), ylim = c(0,7), xlab="",
     ylab = "", axes=FALSE, 
     main = "Attitudes About Civic Obligations")
points(out.matrix$V2[10:15], 1:6, pch = 19, col = 1)
abline(v = 0, lty = 2, lwd = 2)
abline(v = c(-1, -.5, .5, 1), lty=2, lwd = 1, col = "gray")
abline(h = c(1:6), lty = 2, lwd = 1, col = "gray")
arrows(out.matrix$V1[10:15], 1:6, out.matrix$V3[10:15], 1:6, lty = 1, lwd = c(1,1,1,1,1,3), col = 1,
       angle = 90, length = 0.05, code = 3)
axis(1)
axis(2, at = 1:6, labels = c('Should pay taxes','Should vote in elections','Should organize with others','Should contact media', 'Should contact government', 'Civic obligations index (0-5)'), las = 1)
box()

plot(0, 0, type = "n", las = 1, ylim = c(0,5), 
     ylab = "", axes=FALSE, xlab = "Coefficient confidence intervals",
     main = "Attitudes About Anti-State Actions")
points(out.matrix$V2[16:19], 1:4, pch = 19, col = 1)
abline(v = 0, lty = 2, lwd = 2)
abline(v = c(-1, -.5, .5, 1), lty=2, lwd = 1, col = "gray")
abline(h = c(1:4), lty = 2, lwd = 1, col = "gray")
arrows(out.matrix$V1[16:19], 1:4, out.matrix$V3[16:19], 1:4, lty = 1, lwd = c(1,1,1,3), col = 1,
       angle = 90, length = 0.05, code = 3)
axis(1)
axis(2, at = 1:4, labels = c('Refuse taxes','Road block','Occupy public building','Anti-state actions index (0-3)'), las = 1)
box()

dev.off()

# Table A3: Heterogenous Effects by Victimization

victims <- subset(d_sub, d_sub$victim==1 | d_sub$victim_prop==1)
mod_victims_govattitudeindex <- lm(govattitudeindex ~ treat, data = victims)
mod_victims_govattitudeindex_cr <- coeftest(mod_victims_govattitudeindex, vcov = vcovCluster(mod_victims_govattitudeindex, cluster = victims$EANUMB))
mod_victims_civicactivismindex <- lm(civicactivismindex ~ treat, data = victims)
mod_victims_civicactivismindex_cr <- coeftest(mod_victims_civicactivismindex, vcov = vcovCluster(mod_victims_civicactivismindex, cluster = victims$EANUMB))
mod_victims_antistateattitudes <- lm(antistateactivismindex ~ treat, data = victims)
mod_victims_antistateattitudes_cr <- coeftest(mod_victims_antistateattitudes, vcov = vcovCluster(mod_victims_antistateattitudes, cluster = victims$EANUMB))

nonvictims <- subset(d_sub, d_sub$victim==0 | d_sub$victim_prop==0)
mod_nonvictims_govattitudeindex <- lm(govattitudeindex ~ treat, data = nonvictims)
mod_nonvictims_govattitudeindex_cr <- coeftest(mod_nonvictims_govattitudeindex, vcov = vcovCluster(mod_nonvictims_govattitudeindex, cluster = nonvictims$EANUMB))
mod_nonvictims_civicactivismindex <- lm(civicactivismindex ~ treat, data = nonvictims)
mod_nonvictims_civicactivismindex_cr <- coeftest(mod_nonvictims_civicactivismindex, vcov = vcovCluster(mod_nonvictims_civicactivismindex, cluster = nonvictims$EANUMB))
mod_nonvictims_antistateattitudes <- lm(antistateactivismindex ~ treat, data = nonvictims)
mod_nonvictims_antistateattitudes_cr <- coeftest(mod_nonvictims_antistateattitudes, vcov = vcovCluster(mod_nonvictims_antistateattitudes, cluster = nonvictims$EANUMB))

het.mat <- as.data.frame(matrix(nrow=10, ncol = 3, NA))
het.mat[1,] <- philplotfun(mod_victims_govattitudeindex, dataset = victims)
het.mat[2,] <- philplotfun(mod_nonvictims_govattitudeindex, dataset = nonvictims)
het.mat[5,] <- philplotfun(mod_victims_civicactivismindex, dataset = victims)
het.mat[6,] <- philplotfun(mod_nonvictims_civicactivismindex, dataset = nonvictims)
het.mat[7,] <- philplotfun(mod_victims_antistateattitudes, dataset = victims)
het.mat[8,] <- philplotfun(mod_nonvictims_antistateattitudes, dataset = nonvictims)


pdf(file="Results_byvictim.pdf")
par(mar=c(4,10,2,2),
    mfrow=c(3,1))
plot(0, 0, type = "n", las = 1, ylim = c(0,3), xlim = c(-1.75, 1.75), xlab = "",
     ylab = "", axes=FALSE, 
     main = "Attitudes about Local State Institutions (0-3 index)")
points(het.mat$V2[1:2], 1:2, pch = 19, col = c(4:3))
abline(v = 0, lty = 2, lwd = 2)
abline(v = c(-.5, .5), lty=2, lwd = 1, col = "gray")
abline(h = c(1:2), lty = 2, lwd = 1, col = "gray")
arrows(het.mat$V1[1:2], 1:2, het.mat$V3[1:2], 1:2, lty = 1, lwd = 1, col = c(4:3),
       angle = 90, length = 0.05, code = 3)
axis(1)
axis(2, at = 1:2, labels = c('Victims','Non-Victims'), las = 1)
box()

plot(0, 0, type = "n", las = 1, ylim = c(0,3), xlim = c(-1.75, 1.75), xlab = "",
     ylab = "", axes=FALSE, 
     main = "Attitudes About Civic Obligations (0-5 index)")
points(het.mat$V2[5:6], 1:2, pch = 19, col = c(4:3))
abline(v = 0, lty = 2, lwd = 2)
abline(v = c(-.5, .5), lty=2, lwd = 1, col = "gray")
abline(h = c(1:2), lty = 2, lwd = 1, col = "gray")
arrows(het.mat$V1[5:6], 1:2, het.mat$V3[5:6], 1:2, lty = 1, lwd = 1, col = c(4:3),
       angle = 90, length = 0.05, code = 3)
axis(1)
axis(2, at = 1:2, labels = c('Victims','Non-Victims'), las = 1)
box()

plot(0, 0, type = "n", las = 1, ylim = c(0,3), xlim = c(-1.75, 1.75), xlab = "Coefficient Confidence Intervals",
     ylab = "", axes=FALSE, 
     main = "Attitudes About Anti-State Actions (0-3 index)")
points(het.mat$V2[7:8], 1:2, pch = 19, col = c(4:3))
abline(v = 0, lty = 2, lwd = 2)
abline(v = c(-.5, .5), lty=2, lwd = 1, col = "gray")
abline(h = c(1:2), lty = 2, lwd = 1, col = "gray")
arrows(het.mat$V1[7:8], 1:2, het.mat$V3[7:8], 1:2, lty = 1, lwd = 1, col = c(4:3),
       angle = 90, length = 0.05, code = 3)
axis(1)
axis(2, at = 1:2, labels = c('Victims','Non-Victims'), las = 1)
box()

dev.off()

# Table A5: Full Rebel Zone versus Border Rebel Zone

FNzone <- subset(PTI, PTI$DEUX_ZONES_ENQUETE=="Zone ex CNO")
FNborderzone <- subset(FNzone, FNzone$TROIS_ZONES_ENQUETE=="Zone ex CNO (2)")
FNzonetable <- matrix(data=NA, nrow=6, ncol=3)
t1 = t.test(FNzone$permanentcamp, FNborderzone$permanentcamp)
FNzonetable[1,1:3] = c(t1$estimate[1:2], t1$statistic)
t3 = t.test(FNzone$education_wartime_DUMMY, FNborderzone$education_wartime_DUMMY)
FNzonetable[2,1:3] = c(t3$estimate[1:2], t3$statistic)
t4 = t.test(FNzone$health_wartime_DUMMY, FNborderzone$health_wartime_DUMMY)
FNzonetable[3,1:3] = c(t4$estimate[1:2], t4$statistic)
t5 = t.test(FNzone$forcesimpartiales, FNborderzone$forcesimpartiales)
FNzonetable[4,1:3] = c(t5$estimate[1:2], t5$statistic)
t6 = t.test(FNzone$victim, FNborderzone$victim)
FNzonetable[5,1:3] = c(t6$estimate[1:2], t6$statistic)
t7 = t.test(FNzone$paidtax_war, FNborderzone$paidtax_war)
FNzonetable[6,1:3] = c(t7$estimate[1:2], t7$statistic)
rownames(FNzonetable) <- c("Rebel Camp", "Education", "Health", "Peacekeeping", "Victim", "Taxed")
colnames(FNzonetable) <- c("Mean Rebel Zone", "Mean Border Zone", "t statistic")
FNzonetable <- round(FNzonetable, 3)
xtable(FNzonetable, caption = "Full Rebel Zone versus Border Rebel Zone", type = "latex",
       out = "fullzonetable.tex")

# Table A6: Rebel Zone versus Border Zone
community_data <- read.csv("community_dataset.csv")
community_data$taxes.DUMMY <- ifelse(community_data$FN.CollectedTaxes=="yes", 1, 0)
community_data$collaborative.DUMMY <- ifelse(community_data$EliteSupport==1, 1, 0)
community_border <- subset(community_data, community_data$Borderzone==1)
FNzonetable2 <- matrix(data=NA, nrow=5, ncol=3)
t1 = t.test(community_data$Recruitment.DUMMY, community_border$Recruitment.DUMMY)
FNzonetable2[1,1:3] = c(t1$estimate[1:2], t1$statistic)
t2 = t.test(community_data$FN.Organizedmeetings.DUMMY, community_border$FN.Organizedmeetings.DUMMY)
FNzonetable2[2,1:3] = c(t2$estimate[1:2], t2$statistic)
t3 = t.test(community_data$FN.Governance.Index2, community_border$FN.Governance.Index2)
FNzonetable2[3,1:3] = c(t3$estimate[1:2], t3$statistic)
t4 = t.test(community_data$taxes.DUMMY, community_border$taxes.DUMMY)
FNzonetable2[4,1:3] = c(t4$estimate[1:2], t4$statistic)
t5 = t.test(community_data$collaborative.DUMMY, community_border$collaborative.DUMMY)
FNzonetable2[5,1:3] = c(t5$estimate[1:2], t5$statistic)
rownames(FNzonetable2) <- c("Rebel Recruitment (Y/N)", "Rebels Organized Meetings (Y/N)", "No. of Rebel Goods Provided", "Rebels Taxed (Y/N)", "Elite Collaboration (Y/N)")
colnames(FNzonetable2) <- c("Mean Rebel Zone", "Mean Border Zone", "t statistic")
FNzonetable2 <- round(FNzonetable2, 3)
xtable(FNzonetable2, caption = "Rebel Zone versus Border Zone (Community Survey)", type = "latex",
       out = "fullzonetable2.tex")

#Table A7: Benchmarking against Extreme Poverty
benchmod_govattitudeindex <- lm(govattitudeindex ~ treat + poverty_dummy, data = d_sub)
benchmod_govattitudeindex_cr <- coeftest(benchmod_govattitudeindex, vcovCluster(benchmod_govattitudeindex, cluster=d_sub$EANUMB))
benchmod_civicactivismindex <- lm(civicactivismindex ~ treat + poverty_dummy, data = d_sub)
benchmod_civicactivismindex_cr <- coeftest(benchmod_civicactivismindex, vcovCluster(benchmod_civicactivismindex, cluster=d_sub$EANUMB))

stargazer(benchmod_govattitudeindex_cr, benchmod_civicactivismindex_cr, 
          no.space=TRUE,
          title = "Benchmarking Against Extreme Lived Poverty",
          out = "benchmark.tex")

#Table A8: Additional Placebo Tests (prewar social capital)
placebo8 <- lm(creditaccess_2002 ~ treat, data=d_sub)
placebo8 <- coeftest(placebo8, vcovHAC(placebo8, cluster=d_sub$EANUMB))
placebo9 <- lm(debtinformal_2002 ~ treat, data=d_sub)
placebo9 <- coeftest(placebo9, vcovHAC(placebo9, cluster=d_sub$EANUMB))
placebo10 <- lm(wouldadopt_2002 ~ treat, data=d_sub)
placebo10 <- coeftest(placebo10, vcovHAC(placebo10, cluster=d_sub$EANUMB))
placebo11 <- lm(prewarsocialcapitalindex ~ treat, data=d_sub)
placebo11 <- coeftest(placebo11, vcovHAC(placebo11, cluster=d_sub$EANUMB))

stargazer(placebo8, placebo9, placebo10, placebo11, 
          no.space = TRUE, 
          column.labels = c("Social Assistance", "Informal Credit", "Willing to Adopt", "Social Capital Index"),
          title = "Placebo Tests (prewar covariates)",
          out = "Placebo2.tex")

#Tables A9, A10, A11: Constraining to near ceasefire line areas
d_adjacent20 <- subset(d_sub, d_sub$ceasefire_distance_km < 21)
d_adjacent40 <- subset(d_sub, d_sub$ceasefire_distance_km < 41)
d_adjacent60 <- subset(d_sub, d_sub$ceasefire_distance_km < 61)
d_adjacent80 <- subset(d_sub, d_sub$ceasefire_distance_km < 81)
d_adjacent100 <- subset(d_sub, d_sub$ceasefire_distance_km < 101)

adj20_SA <- (lm(govattitudeindex ~ treat, data = d_adjacent20))
adj40_SA <- (lm(govattitudeindex ~ treat, data = d_adjacent40))
adj60_SA <- (lm(govattitudeindex ~ treat, data = d_adjacent60))
adj80_SA <- (lm(govattitudeindex ~ treat, data = d_adjacent80))
adj100_SA <- (lm(govattitudeindex ~ treat, data = d_adjacent100))

adj20_CO <- (lm(civicactivismindex ~ treat, data = d_adjacent20))
adj40_CO <- (lm(civicactivismindex ~ treat, data = d_adjacent40))
adj60_CO <- (lm(civicactivismindex ~ treat, data = d_adjacent60))
adj80_CO <- (lm(civicactivismindex ~ treat, data = d_adjacent80))
adj100_CO <- (lm(civicactivismindex ~ treat, data = d_adjacent100))

adj20_AS <- (lm(antistateactivismindex ~ treat, data = d_adjacent20))
adj40_AS <- (lm(antistateactivismindex ~ treat, data = d_adjacent40))
adj60_AS <- (lm(antistateactivismindex ~ treat, data = d_adjacent60))
adj80_AS <- (lm(antistateactivismindex ~ treat, data = d_adjacent80))
adj100_AS <- (lm(antistateactivismindex ~ treat, data = d_adjacent100))

stargazer(adj20_SA, adj40_SA, adj60_SA, adj80_SA, adj100_SA,
          no.space = TRUE,
          title = "Constraining to near-ceasefire line areas, increments of 20km (SA Index)",
          covariate.labels = c("Treatment", "Constant"),
          dep.var.labels = c("SA Index"),
          column.labels = c("20km", "40km", "60km", "80km", "100km"),
          #notes = "SA = State attitudes; CO = Civic obligations; AS = Anti-state attitudes",
          out = "Adjacent_SA.tex")

stargazer(adj20_CO, adj40_CO, adj60_CO, adj80_CO, adj100_CO,
          no.space = TRUE,
          title = "Constraining to near-ceasefire line areas, increments of 20km (CO Index)",
          covariate.labels = c("Treatment", "Constant"),
          dep.var.labels = c("CO Index"),
          column.labels = c("20km", "40km", "60km", "80km", "100km"),
          #notes = "SA = State attitudes; CO = Civic obligations; AS = Anti-state attitudes",
          out = "Adjacent_CO.tex")

stargazer(adj20_AS, adj40_AS, adj60_AS, adj80_AS, adj100_AS,
          no.space = TRUE,
          title = "Constraining to near-ceasefire line areas, increments of 20km (AS Index)",
          covariate.labels = c("Treatment", "Constant"),
          dep.var.labels = c("AS Index"),
          column.labels = c("20km", "40km", "60km", "80km", "100km"),
          #notes = "SA = State attitudes; CO = Civic obligations; AS = Anti-state attitudes",
          out = "Adjacent_AS.tex")

# Table A12: Placebo Borders
d_sub$treat_placebo1 <- ifelse(d_sub$treat==1 & d_sub$ceasefire_distance_km >=25, 1, 0)
d_sub$treat_placebo2 <- ifelse(d_sub$treat==1 & d_sub$ceasefire_distance_km >=50, 1, 0)
d_sub$treat_placebo3 <- ifelse(d_sub$treat==1 & d_sub$ceasefire_distance_km >=75, 1, 0)

placebotreat1 <- lm(govattitudeindex ~ treat_placebo1 + I(EANUMB), data=d_sub)
placebotreat2 <- lm(govattitudeindex ~ treat_placebo2 + I(EANUMB), data=d_sub)
placebotreat3 <- lm(govattitudeindex ~ treat_placebo3 + I(EANUMB), data=d_sub)

stargazer(placebotreat1, placebotreat2, placebotreat3,
          no.space = T, 
          dep.var.labels.include = T,
          title = "Placebo Borders (25km intervals)",
          out = "placebotreatmentmodels.tex")

# Table A13: Political Attitudes and Distance from Ceasefire Line
rebelside <- subset(d_sub, d_sub$TROIS_ZONES_ENQUETE=="Zone ex CNO (2)")
governmentside <- subset(d_sub, d_sub$TROIS_ZONES_ENQUETE=="Zone gouvernementale")

distmod1 <- lm(govattitudeindex ~ ceasefire_distance_km, data=rebelside)
distmod1 <- coeftest(distmod1, vcov=vcovHAC(distmod1, cluster = rebelside$EANUMB))
distmod2 <- lm(civicactivismindex ~ ceasefire_distance_km, data=rebelside)
distmod2 <- coeftest(distmod2, vcov=vcovHAC(distmod2, cluster = rebelside$EANUMB))
distmod3 <- lm(antistateactivismindex ~ ceasefire_distance_km, data=rebelside)
distmod3 <- coeftest(distmod3, vcov=vcovHAC(distmod3, cluster = rebelside$EANUMB))

distmod4 <- lm(govattitudeindex ~ ceasefire_distance_km, data=governmentside)
distmod4 <- coeftest(distmod4, vcov=vcovHAC(distmod4, cluster = governmentside$EANUMB))
distmod5 <- lm(civicactivismindex ~ ceasefire_distance_km, data=governmentside)
distmod5 <- coeftest(distmod5, vcov=vcovHAC(distmod5, cluster = governmentside$EANUMB))
distmod6 <- lm(antistateactivismindex ~ ceasefire_distance_km, data=governmentside)
distmod6 <- coeftest(distmod6, vcov=vcovHAC(distmod6, cluster = governmentside$EANUMB))

stargazer(distmod1, distmod2, distmod3, distmod4, distmod5, distmod6,
          no.space = TRUE,
          title = "Political Attitudes and Distance from Ceasefire Line",
          out = "DistanceModels.tex")

# Table A14: Region Fixed Effects
FEmod1 <- lm(govattitudeindex ~ treat + I(REGION_Departement), data=d_sub)
FEmod1.c <- coeftest(FEmod1, vcov=vcovHAC(FEmod1, cluster = d_sub$EANUMB))

FEmod2 <- lm(civicactivismindex ~ treat + I(REGION_Departement), data=d_sub)
FEmod2.c <- coeftest(FEmod2, vcov=vcovHAC(FEmod2, cluster = d_sub$EANUMB))

FEmod3 <- lm(antistateactivismindex ~ treat + I(REGION_Departement), data=d_sub)
FEmod3.c <- coeftest(FEmod3, vcov=vcovHAC(FEmod3, cluster = d_sub$EANUMB))

stargazer(FEmod1.c, FEmod2.c, FEmod3.c,
          no.space=TRUE,
          title = "Region Fixed Effects (OLS)",
          out = "RegionFE.tex")

# Table A15: Enumeration Area Level Averages
eamod_govattitudeindex <- lm(govattitudeindex ~ treat, data = eagrouped_sub)
summary(eamod_govattitudeindex)
eamod_civicactivismindex <- lm(civicactivismindex ~ treat, data = eagrouped_sub)
summary(eamod_civicactivismindex)
eamod_antistateactivismindex <- lm(antistateactivismindex ~ treat, data = eagrouped_sub)
summary(eamod_antistateactivismindex)

stargazer(eamod_govattitudeindex, eamod_civicactivismindex, eamod_antistateactivismindex,
          no.space = TRUE,
          title = "Enumeration Area Level Averages",
          covariate.labels = c("Treatment", "Constant"),
          dep.var.labels = c("SA Index", "CO Index", "AS Index"),
          # notes = "SA = State attitudes; RPA = Rely on public ; CO = Civic obligations; AS = Anti-state attitudes; CA = Collective action",
          out = "EA_Results.tex")

# Table A16: Controlling for Peacekeeping and NGO Presence
mod_govattitudeindex <- lm(govattitudeindex ~ treat + age + female + christian + mande + education_2002 + poverty_2002 + yearslived + forcesimpartiales + ngo, data = d_sub)
mod_govattitudeindex_cr <- coeftest(mod_govattitudeindex, vcovCluster(mod_govattitudeindex, cluster=d_sub$EANUMB))
mod_civicactivismindex <- lm(civicactivismindex ~ treat + age + female + christian + mande + education_2002 + poverty_2002 + yearslived + forcesimpartiales + ngo, data = d_sub)
mod_civicactivismindex_cr <- coeftest(mod_civicactivismindex, vcovCluster(mod_civicactivismindex, cluster=d_sub$EANUMB))
mod_antistateactivismindex <- lm(antistateactivismindex ~ treat + age + female + christian + mande + education_2002 + poverty_2002 + yearslived + forcesimpartiales + ngo, data = d_sub)
mod_antistateactivismindex_cr <- coeftest(mod_antistateactivismindex, vcovCluster(mod_antistateactivismindex, cluster=d_sub$EANUMB))

stargazer(mod_govattitudeindex_cr, mod_civicactivismindex_cr, mod_antistateactivismindex_cr, 
          no.space = TRUE,
          title = "Controlling for Peacekeeping and NGO presence",
          covariate.labels = c("Treatment", "Age", "Female", "Christian", "Mande", "Education", "Poverty", "Years Lived", "Peacekeeping", "NGO", "Constant"),
          dep.var.labels = c("SA Index", "CO Index", "AS Index"),
          notes = "SA = State attitudes; CO = Civic obligations; AS = Anti-state attitudes",
          out = "thirdpartycontrols.tex")

# Table A17: Wartime Collective Action
wartimeCA <- lm(civindex_war ~ I(DEUX_ZONES_ENQUETE) + age + female + christian + mande + education_num + poverty_num + yearslived, data = PTI)
wartimeCA_cr <- coeftest(wartimeCA, vcovCluster(wartimeCA, cluster=PTI$EANUMB))
mod_govattitudeindex <- lm(govattitudeindex ~ I(DEUX_ZONES_ENQUETE) + age + female + christian + mande + education_num + poverty_num + yearslived + forcesimpartiales + civindex_war, data = PTI)
mod_govattitudeindex_cr <- coeftest(mod_govattitudeindex, vcovCluster(mod_govattitudeindex, cluster=PTI$EANUMB))
mod_civicactivismindex <- lm(civicactivismindex ~ I(DEUX_ZONES_ENQUETE) + age + female + christian + mande + education_num + poverty_num + yearslived + forcesimpartiales + civindex_war, data = PTI)
mod_civicactivismindex_cr <- coeftest(mod_civicactivismindex, vcovCluster(mod_civicactivismindex, cluster=PTI$EANUMB))
mod_antistateactivismindex <- lm(antistateactivismindex ~ I(DEUX_ZONES_ENQUETE) + age + female + christian + mande + education_num + poverty_num + yearslived + forcesimpartiales + civindex_war, data = PTI)
mod_antistateactivismindex_cr <- coeftest(mod_antistateactivismindex, vcovCluster(mod_antistateactivismindex, cluster=PTI$EANUMB))
mod_behaviorindex <- lm(behaviorindex ~ I(DEUX_ZONES_ENQUETE) + age + female + christian + mande + education_num + poverty_num + yearslived + forcesimpartiales + civindex_war, data = PTI)
mod_behaviorindex_cr <- coeftest(mod_behaviorindex, vcovCluster(mod_behaviorindex, cluster=PTI$EANUMB))

wartimeCA <- lm(civindex_war ~ treat + age + female + christian + mande + education_2002 + poverty_2002 + yearslived, data = d_sub)
wartimeCA_cr <- coeftest(wartimeCA, vcovCluster(wartimeCA, cluster=d_sub$EANUMB))

mod_govattitudeindex <- lm(govattitudeindex ~ treat + age + female + christian + mande + education_2002 + poverty_2002 + yearslived + forcesimpartiales + civindex_war, data = d_sub)
mod_govattitudeindex_cr <- coeftest(mod_govattitudeindex, vcovCluster(mod_govattitudeindex, cluster=d_sub$EANUMB))
mod_civicactivismindex <- lm(civicactivismindex ~ treat + age + female + christian + mande + education_2002 + poverty_2002 + yearslived + forcesimpartiales + civindex_war, data = d_sub)
mod_civicactivismindex_cr <- coeftest(mod_civicactivismindex, vcovCluster(mod_civicactivismindex, cluster=d_sub$EANUMB))
mod_antistateactivismindex <- lm(antistateactivismindex ~ treat + age + female + christian + mande + education_2002 + poverty_2002 + yearslived + forcesimpartiales + civindex_war, data = d_sub)
mod_antistateactivismindex_cr <- coeftest(mod_antistateactivismindex, vcovCluster(mod_antistateactivismindex, cluster=d_sub$EANUMB))
mod_behaviorindex <- lm(behaviorindex ~ treat + age + female + christian + mande + education_2002 + poverty_2002 + yearslived + forcesimpartiales + civindex_war, data = d_sub)
mod_behaviorindex_cr <- coeftest(mod_behaviorindex, vcovCluster(mod_behaviorindex, cluster=d_sub$EANUMB))

stargazer(wartimeCA_cr, mod_behaviorindex_cr, mod_govattitudeindex_cr, mod_civicactivismindex_cr, mod_antistateactivismindex_cr,
          no.space = T,
          title = "Wartime Collective Action Analyses",
          out = "WartimeCAresults.tex")

# Table A18: Within-Rebel Zone Comparison
FNzone$noFNpresence <- ifelse(FNzone$Q30G=="Ne joue pas de rôle / pas de présence dans la zone", 1, 0)
FNzone_present <- subset(FNzone, FNzone$noFNpresence==0)
FNzone_present$qualityofservices <- FNzone_present$order_wartime_DUMMY + FNzone_present$health_wartime_DUMMY + FNzone_present$education_wartime_DUMMY

fullmod4 <- lm(govattitudeindex ~ qualityofservices + age + female + christian + nordiste + poverty_2002 + education_2002 + prewarsocialcapitalindex + yearslived + permanentcamp + forcesimpartiales + victim + paidtax_war + civindex_war + urban, data = FNzone_present)
fullmod4_cr <- coeftest(fullmod4, vcovCluster(fullmod4, cluster=FNzone_present$EANUMB))

fullmod5 <- lm(civicactivismindex ~ qualityofservices + age + female + christian + nordiste + poverty_2002 + education_2002 + prewarsocialcapitalindex + yearslived + permanentcamp + forcesimpartiales + victim + paidtax_war + civindex_war + urban, data = FNzone_present)
fullmod5_cr <- coeftest(fullmod5, vcovCluster(fullmod5, cluster=FNzone_present$EANUMB))

fullmod6 <- lm(antistateactivismindex ~ qualityofservices + age + female + christian + nordiste + poverty_2002 + education_2002 + prewarsocialcapitalindex + yearslived + permanentcamp + forcesimpartiales + victim + paidtax_war + civindex_war + urban, data = FNzone_present)
fullmod6_cr <- coeftest(fullmod6, vcovCluster(fullmod6, cluster=FNzone_present$EANUMB))

stargazer(fullmod4_cr, fullmod5_cr, fullmod6_cr, 
          no.space = TRUE,
          title = "Within-Rebel Zone Comparison",
          covariate.labels = c("Quality of Services (FN Rule)", "Age", "Female", "Christian", "Nordiste", "Education 2002", "Poverty 2002", "Social Capital 2002", "Years Lived", "Permanent Camp", "Peacekeeping", "Victim", "Paid Tax", "Wartime Collective Action", "Urban", "Constant"),
          #dep.var.labels = c("SA Index", "CO Index", "AS Index", "SA Index", "CO Index", "SA Index"),
          notes = "SA = State attitudes; CO = Civic obligations; AS = Anti-state attitudes",
          out = "FullFNzoneresults.tex")


