# -----------------------------------------------------------------------------------------
# Muslim Bias or Fear of Fundamentalism? 
# A Survey Experiment in Five Western European Democracies


# Accepted 23.11.2021
# ------------------------------------------------------------------

# Load and install used packages
p_needed <-  c("haven", "tidyr", "arm", "RColorBrewer", "stargazer", "psych")
packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
sapply(p_needed, require, character.only = TRUE)  


# Specify your working directory
#setwd()

# Load the data
data <- read_dta("data_muslim_bias_or_fundamentalism.dta") # Religionsmonitor 2016 by the Bertelsmann Stiftung

# Name Countries
data$c.name[data$country==1] <- "Germany"
data$c.name[data$country==2] <- "France"
data$c.name[data$country==3] <- "UK"
data$c.name[data$country==4] <- "Austria"
data$c.name[data$country==5] <- "Switzerland"
data$c.name[data$country==6] <- "Turkey"


# Restrict to General Population Survey
data <- data[data$stipro==1,]

# Remove religious Minorities in every country despite turkey
data_turkey <- data[data$c.name=="Turkey",]
data <- data[data$F7==1 | data$F7==7,]
data <- data[!data$c.name=="Turkey",]
data <- rbind(data, data_turkey)

#### ---- N per Country --------------------------------------------------------
table(data$c.name)

# -----------------------------------------------------------------------------------------
# Covariates ------------------------------------------------------------------------------

# Left-Right
data$leftright <- data$S14
data$leftright[data$S14==97 | data$S14==98] <- NA
data$leftright_dummy <- ifelse(data$leftright<=5, "Left", "Right")
data$leftright.z <- (data$leftright-mean(data$leftright, na.rm=T))/(2*sd(data$leftright, na.rm=T))

# Age
data$age <- data$S2
data$age[data$S2==997 | data$S2==998] <- NA

# Gender
data$gender <- data$S3

# Education
data$educ_ger <- data$S15
data$educ_ger[data$S15==96 | data$S15==97 | data$S15==98] <- NA

data$educ2 <- ifelse(data$S8==999, "Kein Schulabschluss", "Allgemeinbildende Vollzeitschulausbildung")
data$educ2[data$S8==997 | data$S8==998] <- NA
data$educ2[data$S8_1==1] <- "abgeschlossenes oder laufendes Studium"

data$educ_uni <- data$S8_1
data$educ_uni[data$S8_1==-1 | data$S8_1==7 | data$S8_1==8] <- NA

# -------------------------------------------------------------------------
# Vignettes ---------------------------------------------------------------
# 2 Country of Origin: Nigeria, Syria
# 2 Reason: Work, Asylum
# 3 Religiosity: Secular, Devout, Fundamentalist
# 2 Religion: Christian, Muslim
# --> 24 Vignettes
# 3 per Respondent (Person A, Person B, Person C)

subset.1 <- subset(data, select =c(ID, c.name, VIG1, VIG2, VIG3, F146_1_1, F146_1_2, F146_1_3,
                       F146_1_4, F146_1_5, F146_1_6,
                       F146_1_7, F146_1_8, F146_1_9,
                       F146_1_10, F146_1_11, F146_1_12,
                       F146_1_13, F146_1_14, F146_1_15,
                       F146_1_16, F146_1_17, F146_1_18,
                       F146_1_19, F146_1_20, F146_1_21,
                       F146_1_22, F146_1_23, F146_1_24))


subset.1 <- gather(subset.1, vignette, feel, VIG1, VIG2, VIG3,
                   c(F146_1_1, F146_1_2, F146_1_3,
                     F146_1_4, F146_1_5, F146_1_6,
                     F146_1_7, F146_1_8, F146_1_9,
                     F146_1_10, F146_1_11, F146_1_12,
                     F146_1_13, F146_1_14, F146_1_15,
                     F146_1_16, F146_1_17, F146_1_18,
                     F146_1_19, F146_1_20, F146_1_21,
                     F146_1_22, F146_1_23, F146_1_24))

subset.1$vignette <- sub("F146_1_", "", subset.1$vignette)
subset.1 <- subset.1[!is.na(subset.1$feel),]
subset.1 <- subset(subset.1, select =c(ID,c.name,vignette,feel))

subset.2 <- subset(data, select =c(ID, c.name, VIG1, VIG2, VIG3, F146_2_1, F146_2_2, F146_2_3,
                       F146_2_4, F146_2_5, F146_2_6,
                       F146_2_7, F146_2_8, F146_2_9,
                       F146_2_10, F146_2_11, F146_2_12,
                       F146_2_13, F146_2_14, F146_2_15,
                       F146_2_16, F146_2_17, F146_2_18,
                       F146_2_19, F146_2_20, F146_2_21,
                       F146_2_22, F146_2_23, F146_2_24))

subset.2 <- gather(subset.2, vignette, grant, 
                   c(F146_2_1, F146_2_2, F146_2_3,
                     F146_2_4, F146_2_5, F146_2_6,
                     F146_2_7, F146_2_8, F146_2_9,
                     F146_2_10, F146_2_11, F146_2_12,
                     F146_2_13, F146_2_14, F146_2_15,
                     F146_2_16, F146_2_17, F146_2_18,
                     F146_2_19, F146_2_20, F146_2_21,
                     F146_2_22, F146_2_23, F146_2_24))

subset.2$vignette <- sub("F146_2_", "", subset.2$vignette)
subset.2 <- subset.2[!is.na(subset.2$grant),]
subset.2 <- subset(subset.2, select =c(ID,c.name,vignette,grant))

subset <- merge(subset.1, subset.2, by=c("ID", "vignette", "c.name"))
subset <- subset[,c(1, 3, 2, 4, 5)]

subset$v.origin <- ifelse(subset$vignette %in% c(1:6, 13:18), "Nigeria", "Syria") 
subset$v.reason <- ifelse(subset$vignette %in% c(1:3, 7:9, 13:15, 19:21), "Work", "Asylum")
subset$v.religiosity[subset$vignette %in% c(1,4,7,10,13,16, 19, 22)] <- "Secular" 
subset$v.religiosity[subset$vignette %in% c(2,5,8,11,14,17,20,23)] <- "Devout" 
subset$v.religiosity[subset$vignette %in% c(3,6,9,12,15,18,21,24)] <- "Radical" 
subset$v.faith <- ifelse(subset$vignette %in% c(1:12), "Christian", "Muslim")

subset$grant <- ifelse(subset$grant %in% c(7,8), NA,  subset$grant)
subset$grant <- ifelse(subset$grant==2, 0, subset$grant)

subset$feel <- ifelse(subset$feel %in% c(7,8), NA,  subset$feel)


resp_char <- data[,c("ID","leftright", "leftright_dummy", "leftright.z",
                     "age", "gender", "educ_ger", "educ2", "educ_uni")]

data2 <- merge(subset, resp_char, by=c("ID"))

data <- data2

# -------------------------------------------------------------------------
# Analysis and figures ----------------------------------------------------

# Reason x Faith ----------------------------------------------------------
m.de.b <- lmer(grant ~  v.reason*v.faith + v.origin + v.religiosity + (1 | ID), data=data[data$c.name=="Germany",])  
m.uk.b <- lmer(grant ~  v.reason*v.faith + v.origin + v.religiosity + (1 | ID), data=data[data$c.name=="UK",])  
m.fr.b <- lmer(grant ~  v.reason*v.faith + v.origin + v.religiosity + (1 | ID), data=data[data$c.name=="France",])  
m.at.b <- lmer(grant ~  v.reason*v.faith + v.origin + v.religiosity + (1 | ID), data=data[data$c.name=="Austria",])  
m.ch.b <- lmer(grant ~  v.reason*v.faith + v.origin + v.religiosity + (1 | ID), data=data[data$c.name=="Switzerland",])
m.tr.b <- lmer(grant ~  v.reason*v.faith + v.origin + v.religiosity + (1 | ID), data=data[data$c.name=="Turkey",])
m.all.b <- lmer(grant ~  v.reason*v.faith + v.origin + v.religiosity + (1 | ID), data=data[data$c.name!="Turkey",])  

stargazer(m.de.b,  m.fr.b, m.ch.b, m.at.b,  m.uk.b, type ="text")
stargazer(m.all.b, type ="text")


#### ---- Fig 1 ----------------------------------------------------------------------------------------------------
####### Comparison reason, faith and origin

m  <- arm::sim(m.all.b, 5000)

dcsa.avg <- c(1, 0, 0, 1, 0, 0, 0) %*% t(fixef(m)) # Devout Christian Syria Asylum
dmsa.avg <- c(1, 0, 1, 1, 0, 0, 0) %*% t(fixef(m)) # Devout Muslim Syria Asylum

dcsw.avg <- c(1, 1, 0, 1, 0, 0, 0) %*% t(fixef(m)) # Devout Christian Syria Work
dmsw.avg <- c(1, 1, 1, 1, 0, 0, 1) %*% t(fixef(m)) # Devout Muslim Syria Work

dcna.avg <- c(1, 0, 0, 0, 0, 0, 0) %*% t(fixef(m)) # Devout Christian Nigeria Asylum
dmna.avg <- c(1, 0, 1, 0, 0, 0, 0) %*% t(fixef(m)) # Devout Muslim Nigeria Asylum

dcnw.avg <- c(1, 1, 0, 0, 0, 0, 0) %*% t(fixef(m)) # Devout Christian Nigeria Work
dmnw.avg <- c(1, 1, 1, 0, 0, 0, 1) %*% t(fixef(m)) # Devout Muslim Nigeria Work

#png("no_minorities_Comp_Origin_Faith.png", height = 450, width = 800)
par(mfrow=c(1,1), mar=c(4,3,3,2))
plot(c(mean(dcsa.avg), mean(dmsa.avg), mean(dcsw.avg), mean(dmsw.avg)),
     c(2.1, 1.9, 1.1, 0.9), cex=1.5, pch="", axes=F, xlim=c(0, 1),
     ylim=c(.5, 2.5), xlab="Pr(Grant)", ylab="", col=rep(c("maroon3", "darkolivegreen"), 2))
abline(h=c(3,2,1), lty=1, lwd=.5, col="grey")
abline(v=seq(0,1,.2), lty=1, lwd=.5, col="grey")
axis(2, at=c(2,1), label=c("Asylum", "Work"), col="white")
axis(1, col="white")
title("Comparison of reasons of migration, nominal faith and country origin", adj=0)


segments(apply(dcsw.avg, 1, quantile, prob=0.025), 1.15, apply(dcsw.avg, 1, quantile, prob=0.975), 1.15, col="maroon3", lwd=2)
segments(apply(dmsw.avg, 1, quantile, prob=0.025), 0.9, apply(dmsw.avg, 1, quantile, prob=0.975), 0.9, col="darkolivegreen", lwd=2)

segments(apply(dcsa.avg, 1, quantile, prob=0.025), 2.15, apply(dcsa.avg, 1, quantile, prob=0.975), 2.15, col="maroon3", lwd=2)
segments(apply(dmsa.avg, 1, quantile, prob=0.025), 1.9, apply(dmsa.avg, 1, quantile, prob=0.975), 1.9, col="darkolivegreen", lwd=2)

points(c(mean(dcsw.avg), mean(dmsw.avg), mean(dcsa.avg), mean(dmsa.avg)), c(1.15, 0.9, 2.15, 1.9), col=rep(c("maroon3","darkolivegreen"),2),
       pch = rep(c(19,17),2), cex=1.5)

text(c(apply(dcsw.avg, 1, quantile, prob=0.975), apply(dmsw.avg, 1, quantile, prob=0.975),
       apply(dcsa.avg, 1, quantile, prob=0.975), apply(dmsa.avg, 1, quantile, prob=0.975)),
     c(1.15, 0.9, 2.15, 1.9),
     pos=4, labels=c("SYR Christian", "SYR Muslim", "SYR Christian", "SYR Muslim"))


segments(apply(dcna.avg, 1, quantile, prob=0.025), 2.05, apply(dcna.avg, 1, quantile, prob=0.975), 2.05, col="maroon3", lwd=2)
segments(apply(dmna.avg, 1, quantile, prob=0.025), 1.80, apply(dmna.avg, 1, quantile, prob=0.975), 1.80, col="darkolivegreen", lwd=2)

segments(apply(dcnw.avg, 1, quantile, prob=0.025), 1.05, apply(dcnw.avg, 1, quantile, prob=0.975), 1.05, col="maroon3", lwd=2)
segments(apply(dmnw.avg, 1, quantile, prob=0.025), 0.80, apply(dmnw.avg, 1, quantile, prob=0.975), 0.80, col="darkolivegreen", lwd=2)

points(c(mean(dcnw.avg), mean(dmnw.avg), mean(dcna.avg), mean(dmna.avg)), c(1.05, 0.80, 2.05, 1.80), col=rep(c("maroon3","darkolivegreen"),2),
       pch = rep(c(1,2),2), cex=1.5)

text(c(apply(dcnw.avg, 1, quantile, prob=0.975), apply(dmnw.avg, 1, quantile, prob=0.975),
       apply(dcna.avg, 1, quantile, prob=0.975), apply(dmna.avg, 1, quantile, prob=0.975)),
     c(1.05, 0.80, 2.05, 1.80),
     pos=4, labels=c("NGA Christian", "NGA Muslim", "NGA Christian", "NGA Muslim"))

#dev.off()


#### ---- Fig A1 – country comparison for origin, faith and reason -----------------------------------------------------------------------------
# Same as Fig 1 divided by countries
models_for_plot <- c(m.de.b, m.fr.b, m.ch.b, m.at.b, m.uk.b)
n.vec <- c("Germany", "France", "Switzerland", "Austria", "United Kingdom")

#png("no_minorities_Reason_Origin_Faith_country_comparison.png", height = 1000, width = 800)
par(mfrow=c(5,1), mar=c(4,3,3,9))
for (i in 1:length(models_for_plot)) {
  
  m  <- arm::sim(models_for_plot[[i]], 5000)
  
  dcsa.avg <- c(1, 0, 0, 1, 0, 0, 0) %*% t(fixef(m)) # Devout Christian Syria Asylum
  dmsa.avg <- c(1, 0, 1, 1, 0, 0, 0) %*% t(fixef(m)) # Devout Muslim Syria Asylum
  
  dcsw.avg <- c(1, 1, 0, 1, 0, 0, 0) %*% t(fixef(m)) # Devout Christian Syria Work
  dmsw.avg <- c(1, 1, 1, 1, 0, 0, 1) %*% t(fixef(m)) # Devout Muslim Syria Work
  
  dcna.avg <- c(1, 0, 0, 0, 0, 0, 0) %*% t(fixef(m)) # Devout Christian Nigeria Asylum
  dmna.avg <- c(1, 0, 1, 0, 0, 0, 0) %*% t(fixef(m)) # Devout Muslim Nigeria Asylum
  
  dcnw.avg <- c(1, 1, 0, 0, 0, 0, 0) %*% t(fixef(m)) # Devout Christian Nigeria Work
  dmnw.avg <- c(1, 1, 1, 0, 0, 0, 1) %*% t(fixef(m)) # Devout Muslim Nigeria Work
  
  #par(mfrow=c(1,1))
  plot(c(mean(dcsa.avg), mean(dmsa.avg), mean(dcsw.avg), mean(dmsw.avg)),
       c(2.1, 1.9, 1.1, 0.9), cex=1.5, pch=26, axes=F, xlim=c(0, 1),
       ylim=c(.5, 2.5), xlab="Pr(Grant)", ylab="", col=rep(c("maroon3", "darkolivegreen"), 2))
  abline(h=c(3,2,1), lty=1, lwd=.5, col="grey")
  abline(v=seq(0,1,.2), lty=1, lwd=.5, col="grey")
  axis(2, at=c(2,1), label=c("Asylum", "Work"), col="white")
  axis(1, col="white")
  title(n.vec[i], adj=0)
  
  segments(apply(dcsw.avg, 1, quantile, prob=0.025), 1.1, apply(dcsw.avg, 1, quantile, prob=0.975), 1.1, col="maroon3", lwd=2)
  segments(apply(dmsw.avg, 1, quantile, prob=0.025), 0.9, apply(dmsw.avg, 1, quantile, prob=0.975), 0.9, col="darkolivegreen", lwd=2)
  
  segments(apply(dcsa.avg, 1, quantile, prob=0.025), 2.1, apply(dcsa.avg, 1, quantile, prob=0.975), 2.1, col="maroon3", lwd=2)
  segments(apply(dmsa.avg, 1, quantile, prob=0.025), 1.9, apply(dmsa.avg, 1, quantile, prob=0.975), 1.9, col="darkolivegreen", lwd=2)
  
  points(c(mean(dcsw.avg), mean(dmsw.avg), mean(dcsa.avg), mean(dmsa.avg)), c(1.1, 0.9, 2.1, 1.9), col=rep(c("maroon3","darkolivegreen"),2),
         pch = rep(c(19,17),2), cex=1.5)
  
  
  segments(apply(dcna.avg, 1, quantile, prob=0.025), 2.05, apply(dcna.avg, 1, quantile, prob=0.975), 2.05, col="maroon3", lwd=2)
  segments(apply(dmna.avg, 1, quantile, prob=0.025), 1.85, apply(dmna.avg, 1, quantile, prob=0.975), 1.85, col="darkolivegreen", lwd=2)
  
  segments(apply(dcnw.avg, 1, quantile, prob=0.025), 1.05, apply(dcnw.avg, 1, quantile, prob=0.975), 1.05, col="maroon3", lwd=2)
  segments(apply(dmnw.avg, 1, quantile, prob=0.025), 0.85, apply(dmnw.avg, 1, quantile, prob=0.975), 0.85, col="darkolivegreen", lwd=2)
  
  points(c(mean(dcnw.avg), mean(dmnw.avg), mean(dcna.avg), mean(dmna.avg)), c(1.05, 0.85, 2.05, 1.85), col=rep(c("maroon3","darkolivegreen"),2),
         pch = rep(c(1,2),2), cex=1.5)
  
  
  
  if(i==3){legend("right", inset=c(-0.1,0), legend=c("SYR Christian", "NGA Christian", "SYR Muslim","NGA Muslim"), 
                  col=c("maroon3","maroon3","darkolivegreen","darkolivegreen"), pch = c(19,1,17,2),
                  xpd=T, bty="n", cex = 1.5, x.intersp = 0.35, y.intersp = 0.85)}
}
#dev.off()


#### ---- Fig 2 ----------------------------------------------------------------------------------------------------
######## Comparison of reason, faith and religiosity
used_col <- rep(c("maroon3", "darkolivegreen"),6)

m.all.d <- lmer(grant ~  v.reason + v.origin + v.religiosity*v.faith + (1 | ID), data=data[data$c.name!="Turkey",])  
stargazer(m.all.d, type="text")

m  <- arm::sim(m.all.d, 5000)

avg.scs <- c(1, 0, .5, 0, 1, 0, 0, 0) %*% t(fixef(m)) # Secular Christian Asylum
avg.sms <- c(1, 0, .5, 0, 1, 1, 0, 1) %*% t(fixef(m)) # Secular Muslim Asylum

avg.dcs <- c(1, 0, .5, 0, 0, 0, 0, 0) %*% t(fixef(m)) # Devout Christian Asylum
avg.dms <- c(1, 0, .5, 0, 0, 1, 0, 0) %*% t(fixef(m)) # Devout Muslim Asylum

avg.rcs <- c(1, 0, .5, 1, 0, 0, 0, 0) %*% t(fixef(m)) # Radical Christian Asylum
avg.rms <- c(1, 0, .5, 1, 0, 1, 1, 0) %*% t(fixef(m)) # Radical Muslim Asylum

avg.scn <- c(1, 1, .5, 0, 1, 0, 0, 0) %*% t(fixef(m)) # Secular Christian Worker
avg.smn <- c(1, 1, .5, 0, 1, 1, 0, 1) %*% t(fixef(m)) # Secular Muslim Worker

avg.dcn <- c(1, 1, .5, 0, 0, 0, 0, 0) %*% t(fixef(m)) # Devout Christian Worker
avg.dmn <- c(1, 1, .5, 0, 0, 1, 0, 0) %*% t(fixef(m)) # Devout Muslim Worker

avg.rcn <- c(1, 1, .5, 1, 0, 0, 0, 0) %*% t(fixef(m)) # Radical Christian Worker
avg.rmn <- c(1, 1, .5, 1, 0, 1, 1, 0) %*% t(fixef(m)) # Radical Muslim Worker


#png("no_minorities_Faith_Religiosity_Reason.png", width = 1000, height = 500)
par(mfrow=c(1,1), mar=c(4,3,3,2))
plot(1, 1,
     cex=1.5, pch="", axes=F, xlim=c(0, 1), ylim=c(.5, 2.5), xlab="Pr(Grant)",
     ylab="", col=rep(c("maroon3", "darkolivegreen"), 2))
abline(h=c(3,2,1), lty=1, lwd=.5, col="grey")
abline(v=seq(0,1,.2), lty=1, lwd=.5, col="grey")
axis(1, at=seq(0, 1, .2), col="white", xlab="Pr(Grant)")
axis(2, at=c(2,1), label=c("Asylum", "Work"), col="white")
#text(c(0.08,0.08), c(2,1), labels=c("Asylum", "Work"), cex=2)
title("Comparison of reasons of migration, nominal faith and religiosity", adj=0)

pos_vec1 <- c(2.25, 1.95, 2.15, 1.85, 2.05, 1.75)
pos_vec2 <- c(2.15, 2.1, 2.025, 1.975, 1.9, 1.85)
pos_vec <- pos_vec1


segments(apply(avg.scs, 1, quantile, prob=0.025), pos_vec[1], apply(avg.scs, 1, quantile, prob=0.975), pos_vec[1], col=used_col[1], lwd=2)
segments(apply(avg.sms, 1, quantile, prob=0.025), pos_vec[2], apply(avg.sms, 1, quantile, prob=0.975), pos_vec[2], col=used_col[2], lwd=2)

segments(apply(avg.dcs, 1, quantile, prob=0.025), pos_vec[3], apply(avg.dcs, 1, quantile, prob=0.975), pos_vec[3], col=used_col[3], lwd=2)
segments(apply(avg.dms, 1, quantile, prob=0.025), pos_vec[4], apply(avg.dms, 1, quantile, prob=0.975), pos_vec[4], col=used_col[4], lwd=2)

segments(apply(avg.rcs, 1, quantile, prob=0.025), pos_vec[5], apply(avg.rcs, 1, quantile, prob=0.975), pos_vec[5], col=used_col[5], lwd=2)
segments(apply(avg.rms, 1, quantile, prob=0.025), pos_vec[6], apply(avg.rms, 1, quantile, prob=0.975), pos_vec[6], col=used_col[6], lwd=2)

points(c(mean(avg.scs), mean(avg.sms), mean(avg.dcs), mean(avg.dms), mean(avg.rcs), mean(avg.rms)), pos_vec, col=rep(c(used_col[1],used_col[2]),2),
       pch = c(19,19,17,17,15,15), cex=1.5)

text(c(apply(avg.scs, 1, quantile, prob=0.975), apply(avg.sms, 1, quantile, prob=0.975),
       apply(avg.dcs, 1, quantile, prob=0.975), apply(avg.dms, 1, quantile, prob=0.975),
       apply(avg.rcs, 1, quantile, prob=0.975), apply(avg.rms, 1, quantile, prob=0.975)), pos_vec, pos=4,
     labels=c("Secular Christian", "Secular Muslim", "Devout Christian",
              "Devout Muslim", "Radical Christian", "Radical Muslim"))

segments(apply(avg.scn, 1, quantile, prob=0.025), pos_vec[1]-1, apply(avg.scn, 1, quantile, prob=0.975), pos_vec[1]-1, col=used_col[7], lwd=2)
segments(apply(avg.smn, 1, quantile, prob=0.025), pos_vec[2]-1, apply(avg.smn, 1, quantile, prob=0.975), pos_vec[2]-1, col=used_col[8], lwd=2)

segments(apply(avg.dcn, 1, quantile, prob=0.025), pos_vec[3]-1, apply(avg.dcn, 1, quantile, prob=0.975), pos_vec[3]-1, col=used_col[9], lwd=2)
segments(apply(avg.dmn, 1, quantile, prob=0.025), pos_vec[4]-1, apply(avg.dmn, 1, quantile, prob=0.975), pos_vec[4]-1, col=used_col[10], lwd=2)

segments(apply(avg.rcn, 1, quantile, prob=0.025), pos_vec[5]-1, apply(avg.rcn, 1, quantile, prob=0.975), pos_vec[5]-1, col=used_col[11], lwd=2)
segments(apply(avg.rmn, 1, quantile, prob=0.025), pos_vec[6]-1, apply(avg.rmn, 1, quantile, prob=0.975), pos_vec[6]-1, col=used_col[12], lwd=2)

points(c(mean(avg.scn), mean(avg.smn), mean(avg.dcn), mean(avg.dmn), mean(avg.rcn), mean(avg.rmn)), 
       pos_vec-1, col=rep(c(used_col[7],used_col[8]),2),
       pch = c(19,19,17,17,15,15), cex=1.5)

text(c(apply(avg.scn, 1, quantile, prob=0.975), apply(avg.smn, 1, quantile, prob=0.975),
       apply(avg.dcn, 1, quantile, prob=0.975), apply(avg.dmn, 1, quantile, prob=0.975),
       apply(avg.rcn, 1, quantile, prob=0.975), apply(avg.rmn, 1, quantile, prob=0.975)), pos_vec-1, pos=4,
     labels=c("Secular Christian", "Secular Muslim", "Devout Christian",
              "Devout Muslim", "Radical Christian", "Radical Muslim"))
#dev.off()


#### ---- Figure A2 – country comparison for reason, faith and religiosity ----------------------------------------------

m.de.d <- lmer(grant ~  v.reason + v.origin + v.religiosity*v.faith + (1 | ID), data=data[data$c.name=="Germany",])  
m.fr.d <- lmer(grant ~  v.reason + v.origin + v.religiosity*v.faith + (1 | ID), data=data[data$c.name=="France",])  
m.ch.d <- lmer(grant ~  v.reason + v.origin + v.religiosity*v.faith + (1 | ID), data=data[data$c.name=="Switzerland",])  
m.at.d <- lmer(grant ~  v.reason + v.origin + v.religiosity*v.faith + (1 | ID), data=data[data$c.name=="Austria",])  
m.uk.d <- lmer(grant ~  v.reason + v.origin + v.religiosity*v.faith + (1 | ID), data=data[data$c.name=="UK",])  
#m.tr.d <- lmer(grant ~  v.reason + v.origin + v.religiosity*v.faith + (1 | ID), data=data[data$c.name=="Turkey",])  

models_for_plot_2_app <- c(m.de.d, m.fr.d, m.ch.d, m.at.d, m.uk.d)
n.vec <- c("Germany", "France", "Switzerland", "Austria", "United Kingdom")

#png("no_minorities_Faith_Religiosity_Reason_version_country_com.png", width = 800, height = 1000)
par(mfrow=c(5,1), mar=c(4,3,3,11))
for (i in 1:5) {
  m  <- arm::sim(models_for_plot_2_app[[i]], 5000)
  
  avg.scs <- c(1, 0, .5, 0, 1, 0, 0, 0) %*% t(fixef(m)) # Secular Christian Asylum
  avg.sms <- c(1, 0, .5, 0, 1, 1, 0, 1) %*% t(fixef(m)) # Secular Muslim Asylum
  
  avg.dcs <- c(1, 0, .5, 0, 0, 0, 0, 0) %*% t(fixef(m)) # Devout Christian Asylum
  avg.dms <- c(1, 0, .5, 0, 0, 1, 0, 0) %*% t(fixef(m)) # Devout Muslim Asylum
  
  avg.rcs <- c(1, 0, .5, 1, 0, 0, 0, 0) %*% t(fixef(m)) # Radical Christian Asylum
  avg.rms <- c(1, 0, .5, 1, 0, 1, 1, 0) %*% t(fixef(m)) # Radical Muslim Asylum
  
  avg.scn <- c(1, 1, .5, 0, 1, 0, 0, 0) %*% t(fixef(m)) # Secular Christian Worker
  avg.smn <- c(1, 1, .5, 0, 1, 1, 0, 1) %*% t(fixef(m)) # Secular Muslim Worker
  
  avg.dcn <- c(1, 1, .5, 0, 0, 0, 0, 0) %*% t(fixef(m)) # Devout Christian Worker
  avg.dmn <- c(1, 1, .5, 0, 0, 1, 0, 0) %*% t(fixef(m)) # Devout Muslim Worker
  
  avg.rcn <- c(1, 1, .5, 1, 0, 0, 0, 0) %*% t(fixef(m)) # Radical Christian Worker
  avg.rmn <- c(1, 1, .5, 1, 0, 1, 1, 0) %*% t(fixef(m)) # Radical Muslim Worker
  
  
  plot(1, 1,
       cex=1.5, pch="", axes=F, xlim=c(0, 1), ylim=c(.5, 2.5), xlab="Pr(Grant)",
       ylab="", col=rep(c("maroon3", "darkolivegreen"), 2))
  abline(h=c(3,2,1), lty=1, lwd=.5, col="grey")
  abline(v=seq(0,1,.2), lty=1, lwd=.5, col="grey")
  axis(1, at=seq(0, 1, .2), col="white", xlab="Pr(Grant)")
  axis(2, at=c(2,1), label=c("Asylum", "Work"), col="white")
  title(n.vec[i], adj=0)
  
  pos_vec1 <- c(2.15, 1.95, 2.1, 1.9, 2.05, 1.85)
  pos_vec2 <- c(2.15, 2.1, 2.025, 1.975, 1.9, 1.85)
  pos_vec <- pos_vec1
  
  
  segments(apply(avg.scs, 1, quantile, prob=0.025), pos_vec[1], apply(avg.scs, 1, quantile, prob=0.975), pos_vec[1], col=used_col[1], lwd=2)
  segments(apply(avg.sms, 1, quantile, prob=0.025), pos_vec[2], apply(avg.sms, 1, quantile, prob=0.975), pos_vec[2], col=used_col[2], lwd=2)
  
  segments(apply(avg.dcs, 1, quantile, prob=0.025), pos_vec[3], apply(avg.dcs, 1, quantile, prob=0.975), pos_vec[3], col=used_col[3], lwd=2)
  segments(apply(avg.dms, 1, quantile, prob=0.025), pos_vec[4], apply(avg.dms, 1, quantile, prob=0.975), pos_vec[4], col=used_col[4], lwd=2)
  
  segments(apply(avg.rcs, 1, quantile, prob=0.025), pos_vec[5], apply(avg.rcs, 1, quantile, prob=0.975), pos_vec[5], col=used_col[5], lwd=2)
  segments(apply(avg.rms, 1, quantile, prob=0.025), pos_vec[6], apply(avg.rms, 1, quantile, prob=0.975), pos_vec[6], col=used_col[6], lwd=2)
  
  points(c(mean(avg.scs), mean(avg.sms), mean(avg.dcs), mean(avg.dms), mean(avg.rcs), mean(avg.rms)), pos_vec, col=rep(c(used_col[1],used_col[2]),2),
         pch = c(19,19,17,17,15,15), cex=1.5)
  
  segments(apply(avg.scn, 1, quantile, prob=0.025), pos_vec[1]-1, apply(avg.scn, 1, quantile, prob=0.975), pos_vec[1]-1, col=used_col[7], lwd=2)
  segments(apply(avg.smn, 1, quantile, prob=0.025), pos_vec[2]-1, apply(avg.smn, 1, quantile, prob=0.975), pos_vec[2]-1, col=used_col[8], lwd=2)
  
  segments(apply(avg.dcn, 1, quantile, prob=0.025), pos_vec[3]-1, apply(avg.dcn, 1, quantile, prob=0.975), pos_vec[3]-1, col=used_col[9], lwd=2)
  segments(apply(avg.dmn, 1, quantile, prob=0.025), pos_vec[4]-1, apply(avg.dmn, 1, quantile, prob=0.975), pos_vec[4]-1, col=used_col[10], lwd=2)
  
  segments(apply(avg.rcn, 1, quantile, prob=0.025), pos_vec[5]-1, apply(avg.rcn, 1, quantile, prob=0.975), pos_vec[5]-1, col=used_col[11], lwd=2)
  segments(apply(avg.rmn, 1, quantile, prob=0.025), pos_vec[6]-1, apply(avg.rmn, 1, quantile, prob=0.975), pos_vec[6]-1, col=used_col[12], lwd=2)
  
  points(c(mean(avg.scn), mean(avg.smn), mean(avg.dcn), mean(avg.dmn), mean(avg.rcn), mean(avg.rmn)), pos_vec-1, col=rep(c(used_col[7],used_col[8]),2),
         pch = c(19,19,17,17,15,15), cex=1.5)
  
  if(i==3){legend("right", inset=c(-0.16,0),
         legend=c("Secular Christian", "Devout Christian", "Radical Christian",
                  "Secular Muslim", "Devout Muslim", "Radical Muslim"),
         col=used_col[c(rep(1,3),rep(8,3))], pch = c(19,17,15,19,17,15),
         xpd=T, bty="n", cex = 1.5, x.intersp = 0.45, y.intersp = 1.1)}
}
#dev.off()

#-----------------------------------------------------------------------------------------------------------------------
#### ---- Tables for the appendix---------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------

#### ---- Table A2 – Factorial Survey Design ---------------------------------------------------------------------------
vig_tab <- aggregate(grant ~  v.origin + v.reason  + v.religiosity + v.faith, data=data[data$c.name!="Turkey",], function (x) {round(mean(x),2)})
vig_tab$sd <- paste0("(",as.character(aggregate(grant ~   v.origin + v.reason + v.religiosity + v.faith, data=data[data$c.name!="Turkey",], function (x) {round(sd(x),2)})[,5]), ")")
vig_tab$n  <- paste0("N = ", as.character(aggregate(grant ~   v.origin + v.reason + v.religiosity+ v.faith, data=data[data$c.name!="Turkey",], length)[,5]))
#write.csv2(vig_tab, file = "no_minorities_vig_table.csv")

#### ---- Table A3 – Effects of Vignette Characteristics ---------------------------------------------------------------------------
m.all.a <- lmer(grant ~  v.reason + v.faith + v.origin + v.religiosity + (1 | ID), data=data[data$c.name!="Turkey",])  
m.all.b <- lmer(grant ~  v.origin + v.religiosity + v.reason + v.faith + v.reason:v.faith + (1 | ID), data=data[data$c.name!="Turkey",])  
m.all.d <- lmer(grant ~  v.reason + v.origin + v.religiosity + v.faith + v.religiosity:v.faith + (1 | ID), data=data[data$c.name!="Turkey",])  
m.all.e <- lmer(grant ~  v.reason + v.origin + v.religiosity + v.faith + v.religiosity:v.reason + (1 | ID), data=data[data$c.name!="Turkey",])
m.all.f <- lmer(grant ~  v.reason + v.origin + v.religiosity + v.faith + v.religiosity*v.reason*v.faith + (1 | ID), data=data[data$c.name!="Turkey",])

stargazer(m.all.a, m.all.b, m.all.d, m.all.e, m.all.f, type="text")
#stargazer(m.all.a, m.all.b, m.all.d, m.all.e, m.all.f, type="html", out = "no_minorities_reg_tab.html")


#### ---- Table A4 – Balance Check ---------------------------------------------------------------------------

data$vignette <- as.factor(data$vignette)

n_vig <- table(data$vignette)

# Age
data$age <- as.numeric(as.character(data$age))

a_mean <- aggregate(age ~ vignette, data = data[data$c.name!="Turkey",], function(x) mean(x, na.rm=T))
a_sd <- aggregate(age ~ vignette, data = data[data$c.name!="Turkey",], function(x) sd(x, na.rm=T))
a_n <- aggregate(age ~ vignette, data = data[data$c.name!="Turkey",], length)
a_se <- a_sd[,2]/sqrt(a_n[,2])

summary(aov(age ~ vignette, data = data[data$c.name!="Turkey",]))

# Gender
data$gender_dummy <- ifelse(data$gender==1, 0, 1)

g_mean <- aggregate(gender_dummy ~ vignette, data = data[data$c.name!="Turkey",], function(x) mean(x, na.rm=T))
g_sd <- aggregate(gender_dummy ~ vignette, data = data[data$c.name!="Turkey",], function(x) sd(x, na.rm=T))
g_n <- aggregate(gender_dummy ~ vignette, data = data[data$c.name!="Turkey",], length)
g_se <- g_sd[,2]/sqrt(g_n[,2])

summary(aov(gender_dummy ~ vignette, data = data[data$c.name!="Turkey",]))

# Education
data$educ_uni_dummy <- data$educ_uni
data$educ_uni_dummy[data$educ_uni==2] <- 0

e_mean <- aggregate(educ_uni_dummy ~ vignette, data = data[data$c.name!="Turkey",], function(x) mean(x, na.rm=T))
e_sd <- aggregate(educ_uni_dummy ~ vignette, data = data[data$c.name!="Turkey",], function(x) sd(x, na.rm=T))
e_n <- aggregate(educ_uni_dummy ~ vignette, data = data[data$c.name!="Turkey",], length)
e_se <- e_sd[,2]/sqrt(e_n[,2])

summary(aov(educ_uni_dummy ~ vignette, data = data[data$c.name!="Turkey",]))

# Left-right
data$leftright_2 <- data$leftright
data$leftright_2[data$leftright==97 | data$leftright==98] <- NA

lr_mean <- aggregate(leftright_2 ~ vignette, data = data[data$c.name!="Turkey",], function(x) mean(x, na.rm=T))
lr_sd <- aggregate(leftright_2 ~ vignette, data = data[data$c.name!="Turkey",], function(x) sd(x, na.rm=T))
lr_n <- aggregate(leftright_2 ~ vignette, data = data[data$c.name!="Turkey",], length)
lr_se <- lr_sd[,2]/sqrt(lr_n[,2])

summary(aov(leftright_2 ~ vignette, data = data[data$c.name!="Turkey",]))


# Combining the information into a table
balance_table <- cbind(n_vig, a_mean, a_se, g_mean, g_se, e_mean, e_se, lr_mean, lr_se)
balance_table <- balance_table[order(as.numeric(as.character(balance_table$vignette))),]
balance_table <- balance_table[,c(3,2,4,5,7,8,10,11,13,14)]
balance_table[,3:10] <- sapply(balance_table[,-c(1,2)], function (x) {round(x,3)})

balance_table2 <- cbind(balance_table[,1:2],
                        paste0(balance_table[,3], "\n(", balance_table[,4], ")"),
                        paste0(balance_table[,5], "\n(", balance_table[,6], ")"),
                        paste0(balance_table[,7], "\n(", balance_table[,8], ")"),
                        paste0(balance_table[,9], "\n(", balance_table[,10], ")"))
colnames(balance_table2) <- c("Vignette", "N", "Age", "Gender", "Visited/ Finished University", "Left-Right")
#write.csv(balance_table2, file="balance_table2.csv", fileEncoding= "UTF-8")


#-----------------------------------------------------------------------------------------------------------------------
#### ---- Values reported in the main text ---------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------

#### ---- Mean values of granted access for Christian/Muslim asylum seeker/worker with CI ------------------------------
m.all.b <- lmer(grant ~  v.reason*v.faith + v.origin + v.religiosity + (1 | ID), data=data[data$c.name!="Turkey",])  

m  <- arm::sim(m.all.b, 5000)

dcsa.avg <- c(1, 0, 0, 1, 0, 0, 0) %*% t(fixef(m)) # Devout Christian Syria Asylum
dmsa.avg <- c(1, 0, 1, 1, 0, 0, 0) %*% t(fixef(m)) # Devout Muslim Syria Asylum

dcsw.avg <- c(1, 1, 0, 1, 0, 0, 0) %*% t(fixef(m)) # Devout Christian Syria Work
dmsw.avg <- c(1, 1, 1, 1, 0, 0, 1) %*% t(fixef(m)) # Devout Muslim Syria Work

res_1 <- rbind(dcsa.avg, dcsw.avg, dmsa.avg, dmsw.avg)

# Values in descending order reported in the text
round(apply(res_1, 1, quantile, prob=0.5),3)
round(apply(res_1, 1, quantile, prob=0.025),3)
round(apply(res_1, 1, quantile, prob=0.975),3)



#### ---- Values for comparisons below figure 2 (comparison between religiosity dependent on religion)
m.all.d <- lmer(grant ~  v.reason + v.origin + v.religiosity*v.faith + (1 | ID), data=data[data$c.name!="Turkey",])  
stargazer(m.all.d, type="text")

m  <- arm::sim(m.all.d, 5000)

avg.scs <- c(1, 0, .5, 0, 1, 0, 0, 0) %*% t(fixef(m)) # Secular Christian Asylum
avg.sms <- c(1, 0, .5, 0, 1, 1, 0, 1) %*% t(fixef(m)) # Secular Muslim Asylum

avg.dcs <- c(1, 0, .5, 0, 0, 0, 0, 0) %*% t(fixef(m)) # Devout Christian Asylum
avg.dms <- c(1, 0, .5, 0, 0, 1, 0, 0) %*% t(fixef(m)) # Devout Muslim Asylum

avg.rcs <- c(1, 0, .5, 1, 0, 0, 0, 0) %*% t(fixef(m)) # Radical Christian Asylum
avg.rms <- c(1, 0, .5, 1, 0, 1, 1, 0) %*% t(fixef(m)) # Radical Muslim Asylum

res_2 <- rbind(avg.scs, avg.sms, avg.dcs, avg.dms, avg.rcs, avg.rms)

res_2_agg <- apply(res_2, 1, quantile, prob=0.5)
(round((res_2_agg[c(1,3,5)]-res_2_agg[c(2,4,6)]),3)) # Values reported in the text


