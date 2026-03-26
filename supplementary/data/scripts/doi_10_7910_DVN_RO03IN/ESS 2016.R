# Set Working Directory
setwd("")
getwd()

# Load Packages
library(rio)
library(lme4)
library(stargazer)
library(car)
library(psych)
library(nlme)
library(beepr)
library(arm)
library(ggplot2)
library(texreg)
library(psych)
library(dplyr)
library(effects)
library(interplot)
library(gridExtra)
library(patchwork)
library(cowplot)

#Open Dataset
ESS2016 <- import("ESS8e02.dta", haven = FALSE)
Saliency2016 <- import("Saliency2016.xlsx")

#Left-Right Self-Placement
table(ESS2016$lrscale)
ESS2016$lrscale <- car::recode(ESS2016$lrscale, "77:99=NA")
table(ESS2016$lrscale)

#Coding Country Variable
table(ESS2016$cntry)
ESS2016$cntry[ESS2016$cntry=="RU"] <- NA
ESS2016$cntry[ESS2016$cntry=="IS"] <- NA
ESS2016$cntry[ESS2016$cntry=="IL"] <- NA
table(ESS2016$cntry)

# Control Variables (Demographic)
table(ESS2016$age)
ESS2016$age[ESS2016$age==999] <- NA
table(ESS2016$age) #Age
ESS2016$AgeCategories <- car::recode(ESS2016$age, "15:29='Under 30'; 
                                30:39='30 to 39'; 
                                40:49='40-49';
                                50:59='50-59';
                                60:69='60-69'; 
                                70:100='70 and older'; else=NA", as.factor = TRUE)
table(ESS2016$AgeCategories)
table(ESS2016$hinctnta)
ESS2016$hinctnta[ESS2016$hinctnta==77] <- NA
ESS2016$hinctnta[ESS2016$hinctnta==88] <- NA
ESS2016$hinctnta[ESS2016$hinctnta==99] <- NA
table(ESS2016$hinctnta) #Household Income Decile
table(ESS2016$gndr)
ESS2016$female <- ESS2016$gndr
ESS2016$female[ESS2016$female==9] <- NA
ESS2016$female[ESS2016$female==1] <- 0
ESS2016$female[ESS2016$female==2] <- 1
table(ESS2016$female) #Female
table(ESS2016$domicil)
ESS2016$rural <- ESS2016$domicil
ESS2016$rural[ESS2016$rural==1] <- 0
ESS2016$rural[ESS2016$rural==2] <- 0
ESS2016$rural[ESS2016$rural==3] <- 0
ESS2016$rural[ESS2016$rural==4] <- 1
ESS2016$rural[ESS2016$rural==5] <- 1
ESS2016$rural[ESS2016$rural==7] <- NA
ESS2016$rural[ESS2016$rural==8] <- NA
ESS2016$rural[ESS2016$rural==9] <- NA
table(ESS2016$rural) #Rural
table(ESS2016$rlgdgr)
ESS2016$religiosity <- car::recode(ESS2016$rlgdgr, "77:99=NA", as.factor = FALSE)
table(ESS2016$religiosity) #Religiosity
table(ESS2016$rlgdnm)
table(ESS2016$rlgblg)
ESS2016$rlgdnm2 <- ifelse(ESS2016$rlgblg == 2, 9, ESS2016$rlgdnm)
table(ESS2016$rlgdnm)
table(ESS2016$rlgdnm2)
ESS2016$ReligDen <- car::recode(ESS2016$rlgdnm2, "1='1 Roman Catholic'; 
                                2='2 Protestant'; 3='3 Eastern Orthodox'; 
                                4='4 Other Christian denomination';
                                5='5 Jewish'; 6='6 Muslim';
                                7='7 Eastern religions'; 
                                8='8 Other non-Christian religions'; 
                                9='9 No Affiliation'; else=NA", as.factor = TRUE)
table(ESS2016$ReligDen) #Religious Denomination
table(ESS2016$blgetmg)
ESS2016$ethnicMinority <- ESS2016$blgetmg
ESS2016$ethnicMinority[ESS2016$ethnicMinority==2] <- 0
ESS2016$ethnicMinority[ESS2016$ethnicMinority==7] <- NA
ESS2016$ethnicMinority[ESS2016$ethnicMinority==8] <- NA
ESS2016$ethnicMinority[ESS2016$ethnicMinority==9] <- NA
table(ESS2016$ethnicMinority) #Ethnic Minority
table(ESS2016$mbtru)
ESS2016$union <- ESS2016$mbtru
ESS2016$union[ESS2016$union==2] <- 1
ESS2016$union[ESS2016$union==3] <- 0
ESS2016$union[ESS2016$union==7] <- NA
ESS2016$union[ESS2016$union==8] <- NA
ESS2016$union[ESS2016$union==9] <- NA
table(ESS2016$union) #Union Member
table(ESS2016$eisced)
ESS2016$EducationLevel <- ESS2016$eisced
ESS2016$EducationLevel[ESS2016$EducationLevel==55] <- NA
ESS2016$EducationLevel[ESS2016$EducationLevel==77] <- NA
ESS2016$EducationLevel[ESS2016$EducationLevel==88] <- NA
ESS2016$EducationLevel[ESS2016$EducationLevel==99] <- NA
ESS2016$EducationLevel[ESS2016$EducationLevel==1] <- "1 Lower Secondary or Less"
ESS2016$EducationLevel[ESS2016$EducationLevel==2] <- "1 Lower Secondary or Less"
ESS2016$EducationLevel[ESS2016$EducationLevel==3] <- "Upper Secondary or Vocational"
ESS2016$EducationLevel[ESS2016$EducationLevel==4] <- "Upper Secondary or Vocational"
ESS2016$EducationLevel[ESS2016$EducationLevel==5] <- "Upper Secondary or Vocational"
ESS2016$EducationLevel[ESS2016$EducationLevel==6] <- "Tertiary"
ESS2016$EducationLevel[ESS2016$EducationLevel==7] <- "Tertiary"
prop.table(table(ESS2016$EducationLevel)) #Level of Education

#Independent Variables
table(ESS2016$dfincac)
table(ESS2016$smdfslv)
table(ESS2016$gincdif)
ESS2016$EconomicInequality <- ((5 - car::recode(ESS2016$dfincac, "7:9=NA", as.factor = FALSE)) / 3) +
  ((car::recode(ESS2016$smdfslv, "7:9=NA", as.factor = FALSE) -1) / 3) +
  ((car::recode(ESS2016$gincdif, "7:9=NA", as.factor = FALSE) -1) / 3)
table(ESS2016$EconomicInequality) # Economic Inequality
table(ESS2016$mnrgtjb)
ESS2016$AntiFeministAttitude <- ESS2016$mnrgtjb
ESS2016$AntiFeministAttitude[ESS2016$AntiFeministAttitude==7] <- NA
ESS2016$AntiFeministAttitude[ESS2016$AntiFeministAttitude==8] <- NA
ESS2016$AntiFeministAttitude[ESS2016$AntiFeministAttitude==9] <- NA
ESS2016$AntiFeministAttitude <- 6 - ESS2016$AntiFeministAttitude
table(ESS2016$AntiFeministAttitude) #Anti-Feminist Attitudes
table(ESS2016$freehms)
ESS2016$LessHomoPositive <- ESS2016$freehms
ESS2016$LessHomoPositive[ESS2016$LessHomoPositive==7] <- NA
ESS2016$LessHomoPositive[ESS2016$LessHomoPositive==8] <- NA
ESS2016$LessHomoPositive[ESS2016$LessHomoPositive==9] <- NA
table(ESS2016$LessHomoPositive) #Not Pro Homosexuality
table(ESS2016$hmsfmlsh)
ESS2016$LessHomoPositive2 <- ESS2016$hmsfmlsh
ESS2016$LessHomoPositive2[ESS2016$LessHomoPositive2==7] <- NA
ESS2016$LessHomoPositive2[ESS2016$LessHomoPositive2==8] <- NA
ESS2016$LessHomoPositive2[ESS2016$LessHomoPositive2==9] <- NA
ESS2016$LessHomoPositive2 <- 6 - ESS2016$LessHomoPositive2
table(ESS2016$LessHomoPositive2) #Not Pro Homosexuality 2
table(ESS2016$hmsacld)
ESS2016$LessHomoPositive3 <- ESS2016$hmsacld
ESS2016$LessHomoPositive3[ESS2016$LessHomoPositive3==7] <- NA
ESS2016$LessHomoPositive3[ESS2016$LessHomoPositive3==8] <- NA
ESS2016$LessHomoPositive3[ESS2016$LessHomoPositive3==9] <- NA
table(ESS2016$LessHomoPositive3) #Not Pro Homosexuality 3
ESS2016$IntOfHomosexuality <- (car::recode(ESS2016$LessHomoPositive, "7:9=NA", as.factor = FALSE) / 3) +
  (car::recode(ESS2016$LessHomoPositive2, "7:9=NA", as.factor = FALSE) / 3) +
  (car::recode(ESS2016$LessHomoPositive3, "7:9=NA", as.factor = FALSE) / 3)
table(ESS2016$IntOfHomosexuality) #Intolerance of Homosexuality
table(ESS2016$imdfetn)
table(ESS2016$impcntr)
ESS2016$LessImmigration <- (car::recode(ESS2016$imdfetn, "7:9=NA", as.factor = FALSE) - 1) +
  (car::recode(ESS2016$impcntr, "7:9=NA", as.factor = FALSE) - 1)
table(ESS2016$LessImmigration) #Less Immigration

#Other Control Variables
table(ESS2016$imptrad)
ESS2016$TraditionNotImportant <- ESS2016$imptrad
ESS2016$TraditionNotImportant[ESS2016$TraditionNotImportant==7] <- NA
ESS2016$TraditionNotImportant[ESS2016$TraditionNotImportant==8] <- NA
ESS2016$TraditionNotImportant[ESS2016$TraditionNotImportant==9] <- NA
ESS2016$TraditionImportant <- 6 - ESS2016$TraditionNotImportant
table(ESS2016$TraditionImportant) #Tradition Important
table(ESS2016$ipfrule)
ESS2016$RuleFollowNotImportant <- ESS2016$ipfrule
ESS2016$RuleFollowNotImportant[ESS2016$RuleFollowNotImportant==7] <- NA
ESS2016$RuleFollowNotImportant[ESS2016$RuleFollowNotImportant==8] <- NA
ESS2016$RuleFollowNotImportant[ESS2016$RuleFollowNotImportant==9] <- NA
ESS2016$RuleFollowImportant <- 6 - ESS2016$RuleFollowNotImportant
table(ESS2016$RuleFollowImportant) #Rule-Following Important
table(ESS2016$impsafe)
ESS2016$SafetyNotImportant <- ESS2016$impsafe
ESS2016$SafetyNotImportant[ESS2016$SafetyNotImportant==7] <- NA
ESS2016$SafetyNotImportant[ESS2016$SafetyNotImportant==8] <- NA
ESS2016$SafetyNotImportant[ESS2016$SafetyNotImportant==9] <- NA
table(ESS2016$SafetyNotImportant) #Safety Not Important
ESS2016$SafetyImportant <- 6 - ESS2016$SafetyNotImportant
table(ESS2016$SafetyImportant) #Safety Important
table(ESS2016$ipcrtiv)
table(ESS2016$impdiff)
ESS2016$NotOpenToExperience <- car::recode(ESS2016$ipcrtiv, "7:9=NA", as.factor = FALSE) / 2 +
  car::recode(ESS2016$impdiff, "7:9=NA", as.factor = FALSE) / 2
table(ESS2016$NotOpenToExperience) #Not Open to New Experiences
ESS2016$OpenToExperience <- 6 - ESS2016$NotOpenToExperience
table(ESS2016$OpenToExperience) #Open to New Experiences
table(ESS2016$euftf)
ESS2016$ProEU <- ESS2016$euftf
ESS2016$ProEU[ESS2016$ProEU==77] <- NA
ESS2016$ProEU[ESS2016$ProEU==88] <- NA
ESS2016$ProEU[ESS2016$ProEU==99] <- NA
table(ESS2016$ProEU) #Pro EU Integration

# Excluding Other Countries
ESS2016 <- ESS2016 %>% filter(!is.na(ESS2016$cntry))

#Include Level-2 Predictor - Saliency
CountryList <- as.list(c("CZ", "EE", "HU", "LT", "PL", "SI", "AT", "BE",
                         "CH", "DE", "ES",  "FI", "FR", "GB", "IE", "IT", 
                         "NL", "NO", "PT", "SE"))
ESS2016$SocialLifeStyleSaliency <- NA
for (i in 1:length(unique(ESS2016$cntry))){
  ESS2016$SocialLifeStyleSaliency <- ifelse(ESS2016$cntry == paste(CountryList[i]),
                                    Saliency2016$SocialLifestyleSaliency[match(ESS2016$cntry, 
                                                                               Saliency2016$Country)], 
                                    ESS2016$SocialLifeStyleSaliency) 
}
table(ESS2016$SocialLifeStyleSaliency) #Traditional Value Saliency
ESS2016$ImmiSaliency <- NA
for (i in 1:length(unique(ESS2016$cntry))){
  ESS2016$ImmiSaliency <- ifelse(ESS2016$cntry == paste(CountryList[i]),
                                 Saliency2016$immisalience[match(ESS2016$cntry, 
                                                                 Saliency2016$Country)], 
                                 ESS2016$ImmiSaliency) 
}
table(ESS2016$ImmiSaliency) #Immigration Saliency
ESS2016$EcoSaliency <- NA
for (i in 1:length(unique(ESS2016$cntry))){
  ESS2016$EcoSaliency <- ifelse(ESS2016$cntry == paste(CountryList[i]),
                                Saliency2016$ecoSalience[match(ESS2016$cntry, 
                                                               Saliency2016$Country)], 
                                ESS2016$EcoSaliency) 
}
table(ESS2016$EcoSaliency) #Economics Saliency

#Saving Version of Dataset Without Standardization
ESS2016sd <- ESS2016

#Standardizing Variables
ESS2016sd$AntiFeministAttitude <- scale(ESS2016sd$AntiFeministAttitude, center = TRUE, scale = TRUE)
ESS2016sd$EconomicInequality <- scale(ESS2016sd$EconomicInequality, center = TRUE, scale = TRUE)
ESS2016sd$IntOfHomosexuality <- scale(ESS2016sd$IntOfHomosexuality, center = TRUE, scale = TRUE)
ESS2016sd$LessImmigration <- scale(ESS2016sd$LessImmigration, center = TRUE, scale = TRUE)

#Centering Variables
table(ESS2016sd$EcoSaliency)
ESS2016sd$EcoSaliency <- scale(ESS2016sd$EcoSaliency, center = TRUE, scale = FALSE)
table(ESS2016sd$EcoSaliency)
table(ESS2016sd$SocialLifeStyleSaliency)
ESS2016sd$SocialLifeStyleSaliency <- scale(ESS2016sd$SocialLifeStyleSaliency, center = TRUE, scale = FALSE)
table(ESS2016sd$SocialLifeStyleSaliency)
table(ESS2016sd$ImmiSaliency)
ESS2016sd$ImmiSaliency <- scale(ESS2016sd$ImmiSaliency, center = TRUE, scale = FALSE)
table(ESS2016sd$ImmiSaliency)

#Model X - Whole Model
modelX2016 <- lmer(lrscale ~ EconomicInequality + AntiFeministAttitude + 
                     LessImmigration + IntOfHomosexuality + 
                     age + EducationLevel + hinctnta + 
                     female + rural + religiosity + union + ethnicMinority + ProEU + 
                     ReligDen + TraditionImportant + RuleFollowImportant +
                     SafetyImportant + OpenToExperience + 
                     EconomicInequality * EcoSaliency + #Remove the three interaction effects to estimate the original model
                     LessImmigration * ImmiSaliency +
                     IntOfHomosexuality * SocialLifeStyleSaliency +
                     (1 + AntiFeministAttitude + EconomicInequality + LessImmigration +
                        IntOfHomosexuality + ProEU | cntry), data=ESS2016sd)
stargazer(modelX2016, type = "text", digits = 2)
beep()

#Estimating the Interaction Effects
p1Sal2016 <- interplot(m = modelX2016, var1 = "EconomicInequality", 
            var2 = "EcoSaliency") + lims(y=c(-.3, 1.1)) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme_light() + ggtitle("2016") +
  xlab("Salience of Economic Issues") + 
  ylab("*Economic Inequality*") + 
  theme(axis.title.y = ggtext::element_markdown(), 
        plot.title = element_text(hjust = 0.5), 
        axis.title=element_text(size=14))

dens12016 <- ggplot(ESS2016, aes(x = EcoSaliency)) + 
  geom_histogram(color = "black", fill = "white", bins = 20) + 
  theme_void()

p2Sal2016 <- interplot(m = modelX2016, 
                       var1 = "LessImmigration", 
                       var2 = "ImmiSaliency") + lims(y=c(-.3, 1.1)) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme_light() +
  ylab("*Ethnic/Immigration Inequality*") + 
  xlab("Salience of Immigration/
       Multiculturalism") + 
  theme(axis.title.y = ggtext::element_markdown(), 
        axis.title=element_text(size=14))

dens22016 <- ggplot(ESS2016, aes(x = ImmiSaliency)) + 
  geom_histogram(color = "black", fill = "white", bins = 20) + 
  theme_void()

p3Sal2016 <- interplot(m = modelX2016, var1 = "IntOfHomosexuality", 
                       var2 = "SocialLifeStyleSaliency") + lims(y=c(-.3, 1.1)) +
  theme_light() +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  xlab("Salience of Social Lifestyle") +  
  ylab("*Intolerance of Homosexuality*") + 
  theme(axis.title.y = ggtext::element_markdown(), 
        axis.title=element_text(size=14))

dens32016 <- ggplot(ESS2016, aes(x = SocialLifeStyleSaliency)) + 
  geom_histogram(color = "black", fill = "white", bins = 20) + 
  theme_void()

#Create Plots
plots2008Eco <- plot_grid(p1Sal2008, dens12008, align = "v", nrow = 2, 
                          rel_heights = c(1/2, 1/16))
plots2016Eco <- plot_grid(p1Sal2016, dens12016, align = "v", nrow = 2, 
                          rel_heights = c(1/2, 1/16))
pcombeco <- plots2008Eco + plot_spacer() + plots2016Eco +
  plot_layout(
    ncol = 3,
    nrow = 1,
    widths = c(100, 1, 100)
  )
plots2008Immi <- plot_grid(p2Sal2008, dens22008, align = "v", nrow = 2, 
                          rel_heights = c(1/2, 1/16))
plots2016Immi <- plot_grid(p2Sal2016, dens22016, align = "v", nrow = 2, 
                          rel_heights = c(1/2, 1/16))
pcombimmi <- plots2008Immi + plot_spacer() + plots2016Immi +
  plot_layout(
    ncol = 3,
    nrow = 1,
    widths = c(100, 1, 100)
  )
plots2008Soc <- plot_grid(p3Sal2008, dens32008, align = "v", nrow = 2, 
                           rel_heights = c(1/2, 1/16))
plots2016Soc <- plot_grid(p3Sal2016, dens32016, align = "v", nrow = 2, 
                           rel_heights = c(1/2, 1/16))
pcombsoc <- plots2008Soc + plot_spacer() + plots2016Soc +
  plot_layout(
    ncol = 3,
    nrow = 1,
    widths = c(100, 1, 100)
  )

pcomball <- plots2008Eco + plot_spacer() + plots2016Eco +
  plots2008Immi + plot_spacer() + plots2016Immi +
  plots2008Soc + plot_spacer() + plots2016Soc +
  plot_layout(
    ncol = 3,
    nrow = 3,
    widths = c(100, 1, 100)
  )

#Simple Regression
#Excluding Other Variables - So that n stays the same for the model with fewer variables
attach(ESS2016sd)
ESS2016sdb <- data.frame(cntry, lrscale, EconomicInequality, AntiFeministAttitude,
                        LessImmigration, IntOfHomosexuality, age,
                        EducationLevel, hinctnta, female, rural, religiosity, union,
                        ethnicMinority, ReligDen, ProEU, TraditionImportant, RuleFollowImportant,
                        SafetyImportant, OpenToExperience)
ESS2016sdb <- na.omit(ESS2016sdb)
detach(ESS2016sd)

#Simple Model
modelX2016XSIMPLE <- lmer(lrscale ~ EconomicInequality + AntiFeministAttitude + 
                            LessImmigration + IntOfHomosexuality + 
                            (1 + EconomicInequality + AntiFeministAttitude + 
                               LessImmigration + 
                               IntOfHomosexuality | cntry), data=ESS2016sdb)
stargazer(modelX2016XSIMPLE, type = "text", digits = 2)
beep()
summary(modelX2016XSIMPLE)