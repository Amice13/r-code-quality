## Setup ----
rm(list=ls())


## Install Packages (Uncomment if needed) ----
# install.packages('foreign');install.packages('stringr'); 
# install.packages('ggplot2'); install.packages('ggthemes'); 
# install.packages('dplyr'); install.packages('tidyr');
# install.packages('lfe'); install.packages('AER'); install.packages('car');
# install.packages('lmtest');install.packages('multiwayvcov')
# install.packages('psych'); install.packages('stargazer')


## Load Packages ----
library('foreign');library('stringr'); 
library('ggplot2'); library('ggthemes'); 
library('dplyr'); library('tidyr');library(lfe); library('AER');
library('lmtest');library('multiwayvcov')
library('psych'); library('stargazer');library('xtable')


## --- Set directory 
#setwd('..')

## --- Creating directory to store figures
plot.dir <- "plots"
dir.create(file.path(plot.dir))

inpaper.dir <- "plots/in paper"
dir.create(file.path(inpaper.dir))



##  Table 4: Importance of each Dimension in Evaluating Immigrants –scale from 0 to 100 (YouGov-religion Study) ----

## ---- data
load("data/YouGov_religion_2013.Rdata")

immchar <- data.frame(education = weighted.mean(d$imm_edu,w = d$psweight), 
                      children  = weighted.mean(d$imm_child,w = d$psweight), 
                      religion  = weighted.mean(d$imm_rel,w = d$psweight), 
                      marital   = weighted.mean(d$imm_marital,w = d$psweight))
tab4 <- t(immchar)

# Generate Table 4
xtable(tab4)


##  Figure 2: Protestant and Catholic Respondent Preferences over Immigrants of each Religion (YouGov-religion Study) ----


# Group by Religion of Respondent


pref_imm  <- group_by(d[,c(2, 7, 8, 19)], religpew) %>%
  filter((religpew %in% c("Protestant", "Roman Catholic"))) %>%
  summarise(prot = weighted.mean(imm_prot, w = psweight, na.rm = TRUE),
            cath = weighted.mean(imm_cath, w = psweight, na.rm = TRUE))

# Prepare for ggplot

protcath <- data.frame(imm_rel_pct = 100 * unlist(c(pref_imm[,2], pref_imm[,3])), 
                       imm_rel = c(rep("Protestant", 2), rep("Catholic", 2)), 
                       resp_rel = rep(c("Protestant","Catholic"), 2))


# Generate Figure 2

prot_cath <- ggplot(protcath, aes(x = factor(resp_rel), 
                                  y = imm_rel_pct, 
                                  fill = factor(imm_rel))) + 
  scale_fill_manual("Immigrant's Religion", 
                    labels = c("Catholic", "Protestant"),
                    values = c("Black", "Gray")) +
  geom_bar(stat="identity", position=position_dodge()) + 
  labs(x = "Religion of Respondent", 
       y = "% of respondents (per religion)")


# Save Figure 2
ggsave(plot = prot_cath, filename = "plots/in paper/Figure2.pdf", height = 4, width = 5)



##  Figure 3: Respondent Preferences Over Immigrants of the Same and Different Religions (SSI–conjoint Study) ----


rm(list=ls())
## ---- packages

library(dplyr); library(foreign); library(stringr); library(ggplot2); library(car); library(stargazer); library(ggthemes); library(multiwayvcov); library(AER)


## ---- functions
source("code/functions.R")

## ---- data
imm <- read.dta("data/SSI_conjoint_2014.dta")
imm$rel_important_resp <- factor(imm$rel_important_resp,levels = c('6','11','7','8','10'), labels = c('Extremely important','Very important','Somewhat important','Not very important','Not at all important'))


# Attributes tobe included in the plot
att.names  <- c("Atheist", "Catholic", "Hindu", "Jewish", "Muslim", "Protestant")



# Basic model
form_all <- formula(choice ~ gender + 
                      relevel(country, ref="Mexico") + 
                      education + language + 
                      religion + 
                      attends)

## --- Catholics as Base Category


data_cath <- create.plot.data(form = form_all, 
                              data = imm[imm$religion_resp == "Catholic",],
                              basecategory = c("Female","Mexico","No formal education","Used interpreter","Catholic","More than once a week"))



## --- Protestants as Base Category

data_prot <- create.plot.data(form = form_all, 
                              data =  imm[imm$religion_resp == "Protestant",],
                              basecategory = c("Female","Mexico","No formal education","Used interpreter","Protestant","More than once a week"))


## --- Jewish as Base Category


data_jew <- create.plot.data(form = form_all, 
                             data = imm[imm$religion_resp == "Jewish",],
                             basecategory = c("Female","Mexico","No formal education","Used interpreter","Jewish","More than once a week"))


## --- Atheists as Base Category


data_ath <- create.plot.data(form = form_all, 
                             data = imm[imm$religion_resp == "Atheist",],
                             basecategory = c("Female","Mexico","No formal education","Used interpreter","Atheist","More than once a week"))


## Calculating Confidence Intervals and selecting attributes
cath       <- CIs(data_cath[att.names,])
cath$level <- factor(att.names, levels = unique(att.names))

prot       <- CIs(data_prot[att.names,])
prot$level <- factor(att.names, levels = unique(att.names))

jew        <- CIs(data_jew[att.names,])
jew$level  <- factor(att.names, levels = unique(att.names))

ath        <- CIs(data_ath[att.names,])
ath$level  <- factor(att.names, levels = unique(att.names))


## Creating Plots
cath.plot <- ggplot(cath,aes(x = level, y = pe))+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .1) +
  geom_line() +
  geom_point() + 
  xlab("Immigrant's Religion")+ 
  coord_flip(ylim = c(-0.5, 0.5)) +
  labs(colour="Respondent's Religion")+
  scale_y_continuous(name = "Change in Pr(Immigrant Preferred for Admission to U.S.)", breaks = round(seq(-0.5,0.5,.1),1)) +
  theme(axis.text=element_text(colour="black")) +
  ggtitle("Catholic Respondents")

jew.plot <- ggplot(jew,aes(x = level, y = pe))+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .1) +
  geom_line() +
  geom_point() + 
  xlab("Immigrant's Religion")+ 
  coord_flip(ylim = c(-0.5, 0.5)) +
  labs(colour="Respondent's Religion")+
  scale_y_continuous(name = "Change in Pr(Immigrant Preferred for Admission to U.S.)", breaks = round(seq(-0.5,0.5,.1),1)) +
  theme(axis.text=element_text(colour="black")) +
  ggtitle("Jewish Respondents")

prot.plot <- ggplot(prot,aes(x = level, y = pe))+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .1) +
  geom_line() +
  geom_point() + 
  xlab("Immigrant's Religion")+ 
  coord_flip(ylim = c(-0.5, 0.5)) +
  labs(colour="Respondent's Religion")+
  scale_y_continuous(name = "Change in Pr(Immigrant Preferred for Admission to U.S.)", breaks = round(seq(-0.5,0.5,.1),1)) +
  theme(axis.text=element_text(colour="black")) +
  ggtitle("Protestant Respondents")

ath.plot <- ggplot(ath,aes(x = level, y = pe))+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .1) +
  geom_line() +
  geom_point() + 
  xlab("Immigrant's Religion")+ 
  coord_flip(ylim = c(-0.5, 0.5)) +
  labs(colour="Respondent's Religion")+
  scale_y_continuous(name = "Change in Pr(Immigrant Preferred for Admission to U.S.)", breaks = round(seq(-0.5,0.5,.1),1)) +
  theme(axis.text=element_text(colour="black")) +
  ggtitle("Atheist Respondents")


# Save Plots - Ignore error
ggsave(cath.plot, file="plots/in paper/Figure3_cath_plot.pdf", height =  7, width = 7)
ggsave(prot.plot, file="plots/in paper/Figure3_prot_plot.pdf", height =  7, width = 7)
ggsave(jew.plot, file="plots/in paper/Figure3_jew_plot.pdf", height =  7, width = 7)
ggsave(ath.plot, file="plots/in paper/Figure3_ath_plot.pdf", height =  7, width = 7)




###  Figure 4: Attribute Affinity and Religiosity (SSI–conjoint Study) ----

rm(list=ls())
## ---- packages

library(dplyr); library(foreign); library(stringr); library(ggplot2); library(car); library(stargazer); library(ggthemes); library(multiwayvcov); library(AER)

## ---- functions
source("code/functions.R")

## ---- data
imm <- read.dta("data/SSI_conjoint_2014.dta")

# Basic model
form_all <- formula(choice ~ gender + 
                      relevel(country, ref="Mexico") + 
                      education + language + 
                      religion + 
                      attends)


# Remove the atheist immigrants from the data
imm_noatheist <- filter(imm, religion!='Atheist')
imm_noatheist$religion <- factor(imm_noatheist$religion, levels = levels(imm_noatheist$religion)[-1],labels = levels(imm_noatheist$religion)[-1] )

#Names for function
names_noath <- c("Male","Female","Mexico","France","Germany","India","Iraq","Nigeria","Philippines","Russia","No formal education","4th Grade","8th Grade","High School","2 year College","College","Graduate","Used interpreter","Tried but unable","Broken English","Fluent English","Catholic","Hindu","Jewish","Muslim","Protestant","Once a week","Once or twice a month","A few times a year","Seldom","Never")


## Ploting data for religious
data_rel <- create.plot.data(form = form_all, 
                             names = names_noath,
                             data = imm_noatheist[imm_noatheist$attends_resp %in% c("More than once a week","Once a week"),],
                             basecategory = c("Female","Mexico","No formal education","Used interpreter","Jewish","More than once a week"))

data_rel <- CIs(data_rel) #adding CIs

## Attributes to  be included in plot
names_plot  <- c("More than once a week", "Once a week", "Once or twice a month", "A few times a year", "Seldom", "Never")

rel <- data_rel[names_plot,]
rel[1,1:4] <- 0
rel$level <- names_plot
rel$level <- factor(rel$level, levels=unique(rel$level))

# Plotting data for non-religious

data_notrel <- create.plot.data(form = form_all, 
                                names = names_noath,
                                data = imm_noatheist[imm_noatheist$attends_resp %in% c("Seldom","Never"),],
                                basecategory = c("Female","Mexico","No formal education","Used interpreter","Jewish","More than once a week"))

data_notrel <- CIs(data_notrel) #adding CIs


notrel <- data_notrel[names_plot,]
notrel[1,1:4] <- 0
notrel$level <- names_plot
notrel$level <- factor(notrel$level, levels=unique(notrel$level))



## Creating Plots
rel.plot <- ggplot(rel, aes(x = level, y = pe)) + xlab("Religiosity") + coord_flip(ylim = c(-0.5, 0.5)) +
  geom_point()+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .1) +
  scale_y_continuous(name = "Change in Pr(Immigrant Preferred for Admission to U.S.)", breaks = round(seq(-0.5,0.5,.1),1)) +
  theme(axis.text=element_text(colour="black")) +
  ggtitle("Religious Respondents")


notrel.plot <- ggplot(notrel, aes(x = level, y = pe)) + xlab("Religiosity") + coord_flip(ylim = c(-0.5, 0.5)) +
  geom_point()+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .1) +
  scale_y_continuous(name = "Change in Pr(Immigrant Preferred for Admission to U.S.)", breaks = round(seq(-0.5,0.5,.1),1)) +
  theme(axis.text=element_text(colour="black")) +
  ggtitle("Non-Religious Respondents")



ggsave(rel.plot, file="plots/in paper/Figure4_religiosity_relresp.pdf", height =  7, width = 7)
ggsave(notrel.plot, file="plots/in paper/Figure4_religiosity_nonrelresps.pdf", height =  7, width = 7)




###  Table 5: Mean Ratings of Immigrants (Mturk–religion/religiosity Study) ----

rm(list=ls())

## ---- packages

library(dplyr); library(foreign); library(stringr); library(ggplot2); library(car); library(stargazer); library(ggthemes); library(multiwayvcov); library(AER)


## ---- functions
source("code/functions.R")


## ---- load data
load("data/Mturk_rel_2013.Rdata")

mturk13$responseid <- as.character(mturk13$responseid)
mturk13$tx.time <- as.factor(mturk13$tx.time) 

## ---- Rescaling rating
mturk13$rating <- ((mturk13$rating - min(mturk13$rating, na.rm=T))/(max(mturk13$rating, na.rm=T) - min(mturk13$rating, na.rm=T)))


## Calculate difference in means for Religious Muslims/Catholics

fit.cathrel.mus <- lm(rating ~  Rel + as.factor(tx.time), 
                      data = mturk13[mturk13$cath==T & mturk13$resp_rel==T & mturk13$Mus == 1,])

fit.cathrel.cath <- lm(rating ~  Rel + as.factor(tx.time), 
                       data = mturk13[mturk13$cath==T & mturk13$resp_rel==T & mturk13$Mus == 0,])



# Cluster bootstrapped SE with period FE:

fit.cathrel.mus.cls <- clusbootreg(formula = (rating ~ Rel + as.factor(tx.time)), 
                                    data = mturk13[mturk13$cath==T & mturk13$resp_rel==T & mturk13$Mus == 1,],
                                    cluster = "responseid",
                                    reps    = 5000)

se <- sqrt(fit.cathrel.mus.cls$boot.vcov["Rel","Rel"])
df <- fit.cathrel.mus$df
t <- (fit.cathrel.mus$coef["Rel"])/(se)
diffse.cathrelmus <-  2*pt(-abs(t),df=df)


fit.cathrel.cath.cls <- clusbootreg(formula = (rating ~ Rel + as.factor(tx.time)), 
                                     data = mturk13[mturk13$cath==T & mturk13$resp_rel==T & mturk13$Mus == 0,],
                                     cluster = "responseid",
                                     reps    = 5000)



se <- sqrt(fit.cathrel.cath.cls$boot.vcov["Rel","Rel"])
df <- fit.cathrel.cath$df
t <- (fit.cathrel.cath$coef["Rel"])/(se)
diffse.cathrelcath <- 2*pt(-abs(t),df=df)



## Calculate difference in means for non religious Muslims/Catholics

fit.cathnonrel.mus <- lm(rating ~  Rel + as.factor(tx.time), 
                         data = mturk13[mturk13$cath==T & mturk13$resp_notrel==T & mturk13$Mus == 1,])


fit.cathnonrel.cath <- lm(rating ~  Rel + as.factor(tx.time), 
                          data = mturk13[mturk13$cath==T & mturk13$resp_notrel==T & mturk13$Mus == 0,])


# Cluster bootstrapped SE with period FE:

fit.cathnonrel.mus.cls <- clusbootreg(formula = (rating ~ Rel + as.factor(tx.time)), 
                                       data = mturk13[mturk13$cath==T & mturk13$resp_notrel==T & mturk13$Mus == 1,],
                                       cluster = "responseid",
                                       reps    = 5000)


se <- sqrt(fit.cathnonrel.mus.cls$boot.vcov["Rel","Rel"])
df <- fit.cathnonrel.mus$df
t <- (fit.cathnonrel.mus$coef["Rel"])/(se)
diffse.noncathrelmus <- 2*pt(-abs(t),df=df)


fit.cathnonrel.cath.cls <- clusbootreg(formula = (rating ~ Rel + as.factor(tx.time)), 
                                        data = mturk13[mturk13$cath==T & mturk13$resp_notrel==T & mturk13$Mus == 0,],
                                        cluster = "responseid",
                                        reps    = 5000)


se <- sqrt(fit.cathnonrel.cath.cls$boot.vcov["Rel","Rel"])
df <- fit.cathnonrel.cath$df
t <- (fit.cathnonrel.cath$coef["Rel"])/(se)
diffse.noncathrelcath <- 2*pt(-abs(t),df=df)


## Creating table with average rating per cell. Calculate difference in means pvalue from the 
## regression estimates that have period FE and clustered se calcualted above.

m <- matrix(NA,3,2)
m[1,1] <- mean(mturk13[mturk13$cath==1 & mturk13$Mus ==0 & mturk13$Rel == 0,]$rating)
m[2,1] <- mean(mturk13[mturk13$cath==1 & mturk13$Mus ==0 & mturk13$Rel == 1,]$rating)

m[1,2] <- mean(mturk13[mturk13$cath==1 & mturk13$resp_rel==1 & mturk13$Mus == 0 & mturk13$Rel == 0,]$rating)
m[2,2] <- mean(mturk13[mturk13$cath==1 & mturk13$resp_rel==1 &  mturk13$Mus == 0 & mturk13$Rel == 1,]$rating)

m[3,] <- c(diffse.noncathrelcath,diffse.cathrelcath)

colnames(m) <- c('Catholics','Religious Catholics')
rownames(m) <- c('Catholic Immigrant', 'Religious Catholic Immigrants',"P-Value Diff in Means")

mm <- matrix(NA,3,2)
mm[1,1] <- mean(mturk13[mturk13$cath==1 & mturk13$Mus ==1 & mturk13$Rel == 0,]$rating)
mm[2,1] <- mean(mturk13[mturk13$cath==1 & mturk13$Mus ==1 & mturk13$Rel == 1,]$rating)

mm[1,2] <- mean(mturk13[mturk13$cath==1 & mturk13$resp_rel==1 & mturk13$Mus ==1 & mturk13$Rel==0,]$rating)
mm[2,2] <- mean(mturk13[mturk13$cath==1 & mturk13$resp_rel==1 &  mturk13$Mus ==1 & mturk13$Rel==1,]$rating)

mm[3,] <- c(diffse.noncathrelmus,diffse.cathrelmus)

colnames(mm) <- c('Catholics','Religious Catholics')
rownames(mm) <- c('Muslim Immigrant', 'Religious Muslim Immigrants', "P-Value Diff in Means")


tab_mturk13 <- round(rbind(m,mm),2)

# Generate Table 5 
stargazer(tab_mturk13)


###  Table 6: Mean Ratings of Immigrants (SSI–conjoint Study)----


rm(list=ls())

## ---- packages

library(dplyr); library(foreign); library(stringr); library(ggplot2); library(car); library(stargazer); library(ggthemes); library(multiwayvcov); library(AER); library(multiwayvcov)

## ---- functions
source("code/functions.R")

## ---- load data conjoint 2013
imm <- read.dta("data/SSI_conjoint_2014.dta")

imm$Mus <- (imm$religion == "Muslim")
imm$Rel <- (imm$attends %in% c("Once a week","More than once a week") )
imm$NotRel <- (imm$attends %in% c("Seldom","Never") )
imm$MidRel <- (imm$attends %in% c("Once or twice a month","A few times a year") )

imm$rel_resp <- (imm$attends_resp %in% c("Once a week","More than once a week") )
imm$notrel_resp <- (imm$attends_resp %in% c("Seldom","Never"))

# Eliminate all profiles that are either very or not very religious
imm <- imm[imm$MidRel == F,]

## ---- Rescaling rating
imm$rating <- ((imm$rating - min(imm$rating, na.rm=T))/(max(imm$rating, na.rm=T) - min(imm$rating, na.rm=T)))

## ---- How Important is religion
imm$rel_important_resp_recode <- recode(imm$rel_important_resp, 'c(6,7,11) = 1; c(8,10) =0')



#Diff in means table


#Non religious REspondents on Non Muslim immigrants
fit.rel <- (lm(rating ~ Rel,
               data = imm[imm$notrel_resp == T & imm$religion_resp!="Atheist" & imm$Mus ==0,]))

fit.rel.vcov <- cluster.vcov(fit.rel, imm[imm$notrel_resp == T & imm$religion_resp!="Atheist" & imm$Mus ==0,]$caseid)


se <- sqrt(diag(fit.rel.vcov))[2]
df <- fit.rel$df
t <- (fit.rel$coef[2])/(se)
diffse.nonrel.nonmus <- 2*pt(-abs(t),df=df)


#Non religious Respondents on Muslim Immigrants

fit.rel2 <- (lm(rating ~ Rel,
                data = imm[imm$notrel_resp == T & imm$religion_resp!="Atheist" & imm$Mus ==1,]))

fit.rel.vcov2 <- cluster.vcov(fit.rel2, imm[imm$notrel_resp == T & imm$religion_resp!="Atheist" & imm$Mus ==1,]$caseid)


se <- sqrt(diag(fit.rel.vcov2))[2]
df <- fit.rel2$df
t <- (fit.rel2$coef[2])/(se)
diffse.nonrel.mus <- 2*pt(-abs(t),df=df)


# Religious Respondents on Non Muslim Immigrants

fit.rel3 <- (lm(rating ~ Rel,
                data = imm[imm$rel_resp == T & imm$religion_resp!="Atheist" & imm$Mus ==0,]))

fit.rel.vcov3 <- cluster.vcov(fit.rel3, imm[imm$rel_resp == T & imm$religion_resp!="Atheist" & imm$Mus ==0,]$caseid)

se <- sqrt(diag(fit.rel.vcov3))[2]
df <- fit.rel3$df
t <- (fit.rel3$coef[2])/(se)
diffse.rel.nonmus <- 2*pt(-abs(t),df=df)



# Religious Respondents on Muslim Immigrants

fit.rel4 <- (lm(rating ~ Rel,
                data = imm[imm$rel_resp == T & imm$religion_resp!="Atheist" & imm$Mus ==1,]))

fit.rel.vcov4 <- cluster.vcov(fit.rel4, imm[imm$rel_resp == T & imm$religion_resp!="Atheist" & imm$Mus == 1,]$caseid)

se <- sqrt(diag(fit.rel.vcov4))[2]
df <- fit.rel4$df
t <- (fit.rel4$coef[2])/(se)
diffse.rel.mus <- 2*pt(-abs(t),df=df)




## Creating table with average rating per cell. Calculate difference in means pvalue from the 
## regression estimates that have period FE and clustered se calcualted above.

m <- matrix(NA,3,2)
m[1,1] <- mean(imm[imm$notrel_resp == T & imm$religion_resp!="Atheist" & imm$Mus ==0 & imm$Rel == 0,]$rating, na.rm=T)
m[2,1] <- mean(imm[imm$notrel_resp == T & imm$religion_resp!="Atheist" & imm$Mus ==0 & imm$Rel == 1,]$rating, na.rm=T)

m[1,2] <- mean(imm[imm$religion_resp!="Atheist" & imm$rel_resp==1 & imm$Mus ==0 &  imm$Rel==0,]$rating, na.rm=T)
m[2,2] <- mean(imm[imm$religion_resp!="Atheist" & imm$rel_resp==1 &  imm$Mus ==0 & imm$Rel==1,]$rating, na.rm=T)

colnames(m) <- c('Non Relious','Religious Respondent')
rownames(m) <- c('Non Religous (Non Muslim) Immigrant', 'Religious (Non Muslim) Immigrants', "P-Value Diff in Means")
m[3,] <- c(diffse.nonrel.nonmus, diffse.rel.nonmus)

mm <- matrix(NA,3,2)
mm[1,1] <- mean(imm[imm$notrel_resp == T & imm$religion_resp!="Atheist" & imm$Mus ==1 & imm$Rel == 0,]$rating, na.rm=T)
mm[2,1] <- mean(imm[imm$notrel_resp == T & imm$religion_resp!="Atheist" & imm$Mus ==1 & imm$Rel == 1,]$rating, na.rm=T)

mm[1,2] <- mean(imm[imm$rel_resp == T & imm$religion_resp!="Atheist" & imm$Mus ==1 & imm$Rel == 0,]$rating, na.rm=T)
mm[2,2] <- mean(imm[imm$rel_resp == T & imm$religion_resp!="Atheist" & imm$Mus ==1 & imm$Rel == 1,]$rating, na.rm=T)

colnames(mm) <- c('Non Religious','Religious Respondent')
rownames(mm) <- c('Muslim Immigrant', 'Religious Muslim Immigrants',"P-Value Diff in Means" )
mm[3,] <- c(diffse.nonrel.mus, diffse.rel.mus)

tab_conjoint <- round(rbind(m,mm),2)

# Generate Table 6
stargazer(tab_conjoint)



