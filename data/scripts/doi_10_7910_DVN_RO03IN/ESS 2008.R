# Set WD
setwd("")
getwd()

# Load Packages
library(rio)
library(lme4)
library(stargazer)
library(car)
library(haven)
library(ggplot2)
library(beepr)
library(dplyr)
library(effects)
library(interplot)

#Open Dataset
ESS2008 <- import("ESS4e04_5.dta", haven = FALSE)
ESS2008SKINCOME <- read_spss("ESS4csSK.por")
ESS2008SKINCOME$HINCTNSK <- scale(ESS2008SKINCOME$HINCTNSK, center = TRUE, scale = TRUE)
ESS2008$hinctnta <- scale(ESS2008$hinctnta, center = TRUE, scale = TRUE)
ESS2008$hinctnta <- ifelse(ESS2008$cntry == "SK", 
                           ESS2008SKINCOME$HINCTNSK[match(ESS2008$idno, 
                                                          ESS2008SKINCOME$IDNO)],
                           ESS2008$hinctnt) #Integrating Slovakia Income Data
Saliency2008 <- import("Saliency2008.xlsx")

#Left-Right Self-Placement
table(ESS2008$lrscale)
ESS2008$lrscale <- car::recode(ESS2008$lrscale, "77:99=NA")
table(ESS2008$lrscale)

#Coding Country Variable - Remove not sufficiently democratic countries, non-European, and those without the right variables
table(ESS2008$cntry)
ESS2008$cntry[ESS2008$cntry=="RU"] <- NA
ESS2008$cntry[ESS2008$cntry=="BG"] <- NA
ESS2008$cntry[ESS2008$cntry=="CY"] <- NA
ESS2008$cntry[ESS2008$cntry=="IL"] <- NA
ESS2008$cntry[ESS2008$cntry=="TR"] <- NA
table(ESS2008$cntry)

# Control Variables (Demographic)
table(ESS2008$agea)
ESS2008$agea[ESS2008$agea==999] <- NA
ESS2008$agea[ESS2008$agea==123] <- NA
ESS2008$age <- ESS2008$agea
table(ESS2008$age) #Age
table(ESS2008$eduyrs)
ESS2008$EducationYears <- car::recode(ESS2008$eduyrs, "77:99=NA; 18:50=18", as.factor = FALSE)
table(ESS2008$EducationYears) #Years of Education
table(ESS2008$hinctnta)
ESS2008$hinctnta[ESS2008$hinctnta==77] <- NA
ESS2008$hinctnta[ESS2008$hinctnta==88] <- NA
ESS2008$hinctnta[ESS2008$hinctnta==99] <- NA
table(ESS2008$hinctnta) #Household Income Decile
table(ESS2008$gndr)
ESS2008$female <- ESS2008$gndr
ESS2008$female[ESS2008$female==9] <- NA
ESS2008$female[ESS2008$female==1] <- 0
ESS2008$female[ESS2008$female==2] <- 1
table(ESS2008$female) #Female
table(ESS2008$domicil)
ESS2008$rural <- ESS2008$domicil
ESS2008$rural[ESS2008$rural==1] <- 0
ESS2008$rural[ESS2008$rural==2] <- 0
ESS2008$rural[ESS2008$rural==3] <- 0
ESS2008$rural[ESS2008$rural==4] <- 1
ESS2008$rural[ESS2008$rural==5] <- 1
ESS2008$rural[ESS2008$rural==7] <- NA
ESS2008$rural[ESS2008$rural==8] <- NA
ESS2008$rural[ESS2008$rural==9] <- NA
table(ESS2008$rural) #Rural
table(ESS2008$rlgdgr)
ESS2008$religiosity <- car::recode(ESS2008$rlgdgr, "77:99=NA", as.factor = FALSE)
table(ESS2008$religiosity) #Religiosity
table(ESS2008$rlgblg)
ESS2008$rlgdnm2 <- ifelse(ESS2008$rlgblg == 2, 66, ESS2008$rlgdnm)
table(ESS2008$rlgdnm)
table(ESS2008$rlgdnm2)
ESS2008$ReligDen <- car::recode(ESS2008$rlgdnm2, "1='1 Roman Catholic'; 
                                2='2 Protestant'; 3='3 Eastern Orthodox'; 
                                4='4 Other Christian denomination';
                                5='5 Jewish'; 6='6 Islamic';
                                7='7 Eastern religions'; 
                                8='8 Other non-Christian religions';
                                66='9 No Affiliation'; else=NA", 
                                as.factor = TRUE) #Religious Denomination
table(ESS2008$ReligDen) #Religious Denomination
table(ESS2008$blgetmg)
ESS2008$ethnicMinority <- ESS2008$blgetmg
ESS2008$ethnicMinority[ESS2008$ethnicMinority==2] <- 0
ESS2008$ethnicMinority[ESS2008$ethnicMinority==7] <- NA
ESS2008$ethnicMinority[ESS2008$ethnicMinority==8] <- NA
ESS2008$ethnicMinority[ESS2008$ethnicMinority==9] <- NA
table(ESS2008$ethnicMinority) #Ethnic Minority
table(ESS2008$mbtru)
ESS2008$union <- ESS2008$mbtru
ESS2008$union[ESS2008$union==2] <- 1
ESS2008$union[ESS2008$union==3] <- 0
ESS2008$union[ESS2008$union==7] <- NA
ESS2008$union[ESS2008$union==8] <- NA
ESS2008$union[ESS2008$union==9] <- NA
table(ESS2008$union) #Union Member
table(ESS2008$eisced)
ESS2008$EducationLevel <- ESS2008$eisced
ESS2008$EducationLevel[ESS2008$EducationLevel==55] <- NA
ESS2008$EducationLevel[ESS2008$EducationLevel==77] <- NA
ESS2008$EducationLevel[ESS2008$EducationLevel==88] <- NA
ESS2008$EducationLevel[ESS2008$EducationLevel==99] <- NA
ESS2008$EducationLevel[ESS2008$EducationLevel==1] <- "1 Lower Secondary or Less"
ESS2008$EducationLevel[ESS2008$EducationLevel==2] <- "1 Lower Secondary or Less"
ESS2008$EducationLevel[ESS2008$EducationLevel==3] <- "Upper Secondary or Vocational"
ESS2008$EducationLevel[ESS2008$EducationLevel==4] <- "Upper Secondary or Vocational"
ESS2008$EducationLevel[ESS2008$EducationLevel==5] <- "Upper Secondary or Vocational"
ESS2008$EducationLevel[ESS2008$EducationLevel==6] <- "Tertiary"
ESS2008$EducationLevel[ESS2008$EducationLevel==7] <- "Tertiary"
table(ESS2008$EducationLevel) #Level of Education

#Independent Variables
table(ESS2008$dfincac)
table(ESS2008$smdfslv)
table(ESS2008$gincdif)
ESS2008$EconomicInequality <- ((5 - car::recode(ESS2008$dfincac, "7:9=NA", as.factor = FALSE)) / 3) +
  ((car::recode(ESS2008$smdfslv, "7:9=NA", as.factor = FALSE) -1) / 3) +
  ((car::recode(ESS2008$gincdif, "7:9=NA", as.factor = FALSE) -1) / 3)
table(ESS2008$EconomicInequality) # Against Economic Equality
table(ESS2008$mnrgtjb)
table(ESS2008$wmcpwrk)
ESS2008$AntiFeministAttitudeComb <- ((5 - car::recode(ESS2008$mnrgtjb, "7:9=NA", as.factor = FALSE)) / 2) +
  ((5 - car::recode(ESS2008$wmcpwrk, "7:9=NA", as.factor = FALSE)) / 2)
table(ESS2008$AntiFeministAttitudeComb) #Anti Feminist Attitudes
table(ESS2008$freehms)
ESS2008$IntOfHomosexuality <- ESS2008$freehms
ESS2008$IntOfHomosexuality[ESS2008$IntOfHomosexuality==7] <- NA
ESS2008$IntOfHomosexuality[ESS2008$IntOfHomosexuality==8] <- NA
ESS2008$IntOfHomosexuality[ESS2008$IntOfHomosexuality==9] <- NA
table(ESS2008$IntOfHomosexuality) #Intolerance of Homosexuality
table(ESS2008$imdfetn)
table(ESS2008$impcntr)
ESS2008$LessImmigration <- (car::recode(ESS2008$imdfetn, "7:9=NA", as.factor = FALSE) - 1) +
  (car::recode(ESS2008$impcntr, "7:9=NA", as.factor = FALSE) - 1)
table(ESS2008$LessImmigration)  #Less Immigration

#Other Control Variables
table(ESS2008$imptrad)
ESS2008$TraditionNotImportant <- ESS2008$imptrad
ESS2008$TraditionNotImportant[ESS2008$TraditionNotImportant==7] <- NA
ESS2008$TraditionNotImportant[ESS2008$TraditionNotImportant==8] <- NA
ESS2008$TraditionNotImportant[ESS2008$TraditionNotImportant==9] <- NA
table(ESS2008$TraditionNotImportant) #Tradition Not Important
ESS2008$TraditionImportant <- 6 - ESS2008$TraditionNotImportant
table(ESS2008$TraditionImportant) #Tradition Important
table(ESS2008$ipfrule)
ESS2008$RuleFollowNotImportant <- ESS2008$ipfrule
ESS2008$RuleFollowNotImportant[ESS2008$RuleFollowNotImportant==7] <- NA
ESS2008$RuleFollowNotImportant[ESS2008$RuleFollowNotImportant==8] <- NA
ESS2008$RuleFollowNotImportant[ESS2008$RuleFollowNotImportant==9] <- NA
table(ESS2008$RuleFollowNotImportant) #Rule-Following Not Important
ESS2008$RuleFollowImportant <- 6 - ESS2008$RuleFollowNotImportant
table(ESS2008$RuleFollowImportant) #Rule-Following Important
table(ESS2008$impsafe)
ESS2008$SafetyNotImportant <- ESS2008$impsafe
ESS2008$SafetyNotImportant[ESS2008$SafetyNotImportant==7] <- NA
ESS2008$SafetyNotImportant[ESS2008$SafetyNotImportant==8] <- NA
ESS2008$SafetyNotImportant[ESS2008$SafetyNotImportant==9] <- NA
table(ESS2008$SafetyNotImportant) #Safety Not Important
ESS2008$SafetyImportant <- 6- ESS2008$SafetyNotImportant
table(ESS2008$SafetyImportant) #Safety Important
table(ESS2008$impdiff)
table(ESS2008$ipcrtiv)
ESS2008$NotOpenToExperience <- car::recode(ESS2008$ipcrtiv, "7:9=NA", as.factor = FALSE) / 2 +
  car::recode(ESS2008$impdiff, "7:9=NA", as.factor = FALSE) / 2
table(ESS2008$NotOpenToExperience) #Not Open to New Experiences
ESS2008$OpenToExperience <- 6 - ESS2008$NotOpenToExperience
table(ESS2008$OpenToExperience) #Open to New Experiences
table(ESS2008$euftf)
ESS2008$ProEU <- ESS2008$euftf
ESS2008$ProEU[ESS2008$ProEU==77] <- NA
ESS2008$ProEU[ESS2008$ProEU==88] <- NA
ESS2008$ProEU[ESS2008$ProEU==99] <- NA
table(ESS2008$ProEU) #Pro EU

# Excluding Other Countries
ESS2008 <- ESS2008 %>% filter(!is.na(ESS2008$cntry))

CountryList <- as.list(c("CZ", "EE", "HR", "HU", "LV", 
                         "PL", "RO", "SI", "SK", "BE", 
                         "CH", "DE", "DK", "ES", "FI", 
                         "FR", "GB", "GR", "IE", "IL", 
                         "NL", "NO", "PT", "SE", "UA"))
ESS2008$SocialLifeStyleSaliency <- NA
for (i in 1:length(unique(ESS2008$cntry))){
  ESS2008$SocialLifeStyleSaliency <- ifelse(ESS2008$cntry == paste(CountryList[i]),
                                            Saliency2008$SocialLifestyleSaliency[match(ESS2008$cntry, 
                                                                                           Saliency2008$Country)], 
                                            ESS2008$SocialLifeStyleSaliency) 
}
table(ESS2008$cntry, ESS2008$SocialLifeStyleSaliency) #Traditional Value Saliency
ESS2008$ImmiSaliency <- NA
for (i in 1:length(unique(ESS2008$cntry))){
  ESS2008$ImmiSaliency <- ifelse(ESS2008$cntry == paste(CountryList[i]),
                                 Saliency2008$immisalience[match(ESS2008$cntry, 
                                                                     Saliency2008$Country)], 
                                 ESS2008$ImmiSaliency) 
}
table(ESS2008$ImmiSaliency) #Immigration Saliency
ESS2008$EcoSaliency <- NA
for (i in 1:length(unique(ESS2008$cntry))){
  ESS2008$EcoSaliency <- ifelse(ESS2008$cntry == paste(CountryList[i]),
                                Saliency2008$ecoSalience[match(ESS2008$cntry, 
                                                                   Saliency2008$Country)], 
                                ESS2008$EcoSaliency) 
}
table(ESS2008$EcoSaliency) #Economics Saliency

# Excluding Other Variables (dependent on the analysis)
ESS2008sd <- ESS2008
# attach(ESS2008sd)
# ESS2008sd <- data.frame(cntry, lrscale, EconomicInequality, AntiFeministAttitudeComb,
#                         LessImmigration, IntOfHomosexuality, age,
#                         EducationYears, hinctnta, female, rural, religiosity, union,
#                         ethnicMinority, ReligDen, ProEU, TraditionImportant, RuleFollowImportant,
#                         SafetyImportant, OpenToExperience, EcoSaliency, 
#                         SocialLifeStyleSaliency, ImmiSaliency)
# ESS2008sd <- na.omit(ESS2008sd)
# detach(ESS2008sd)

#Standardizing Variables
ESS2008sd$AntiFeministAttitudeComb <- scale(ESS2008sd$AntiFeministAttitudeComb, center = TRUE, scale = TRUE)
ESS2008sd$EconomicInequality <- scale(ESS2008sd$EconomicInequality, center = TRUE, scale = TRUE)
ESS2008sd$IntOfHomosexuality <- scale(ESS2008sd$IntOfHomosexuality, center = TRUE, scale = TRUE)
ESS2008sd$LessImmigration <- scale(ESS2008sd$LessImmigration, center = TRUE, scale = TRUE)

#Centering Variables
table(ESS2008sd$EcoSaliency)
ESS2008sd$EcoSaliency <- scale(as.numeric(ESS2008sd$EcoSaliency), center = TRUE, scale = FALSE)
table(ESS2008sd$EcoSaliency)
table(ESS2008sd$SocialLifeStyleSaliency)
ESS2008sd$SocialLifeStyleSaliency <- scale(as.numeric(ESS2008sd$SocialLifeStyleSaliency), center = TRUE, scale = FALSE)
table(ESS2008sd$SocialLifeStyleSaliency)
table(ESS2008sd$ImmiSaliency)
ESS2008sd$ImmiSaliency <- scale(as.numeric(ESS2008sd$ImmiSaliency), center = TRUE, scale = FALSE)
table(ESS2008sd$ImmiSaliency)

# # Descriptive statistics
# stargazer(ESS2008sd, type = "text", digits = 2)
# prop.table(table(ESS2008sd$rural))
# prop.table(table(ESS2008sd$female))
# prop.table(table(ESS2008sd$ethnicMinority))
# prop.table(table(ESS2008sd$union))
# prop.table(table(ESS2008sd$ReligDen))

# Excluding Other Variables for Table A.15
# attach(ESS2008sd)
# ESS2008A15 <- data.frame(cntry, lrscale, EconomicInequality, AntiFeministAttitudeComb, 
#                              LessImmigration, IntOfHomosexuality, age,
#                              EducationYears, hinctnta, female, rural, religiosity, union,
#                              ethnicMinority, ReligDen, ProEU, TraditionImportant, RuleFollowImportant, 
#                              SafetyImportant, OpenToExperience, EcoSaliency, 
#                              ImmiSaliency, SocialLifeStyleSaliency)
# ESS2008A15 <- na.omit(ESS2008A15)
# detach(ESS2008sd)

#Model X Simple
modelX2008SIMPLE <- lmer(lrscale ~ EconomicInequality + AntiFeministAttitudeComb + 
                           LessImmigration + IntOfHomosexuality +
                           (1 + EconomicInequality + AntiFeministAttitudeComb + 
                              LessImmigration + 
                              IntOfHomosexuality | cntry), data=ESS2008sd)
stargazer(modelX2008SIMPLE, type = "text", digits = 2)
summary(modelX2008SIMPLE)

#Model X - Whole Model
table(ESS2008sd$SocialLifeStyleSaliency)
modelX2008 <- lmer(lrscale ~ EconomicInequality + AntiFeministAttitudeComb + 
                     LessImmigration + 
                     IntOfHomosexuality + age + EducationYears + hinctnta +
                     female + rural + religiosity + union + ethnicMinority + 
                     ReligDen + ProEU + TraditionImportant + RuleFollowImportant +
                     SafetyImportant + OpenToExperience + 
                     EcoSaliency*EconomicInequality + #Remove the three interaction effects to estimate the original model
                     ImmiSaliency*LessImmigration + 
                     SocialLifeStyleSaliency*IntOfHomosexuality +
                     (1 + EconomicInequality + AntiFeministAttitudeComb + 
                        LessImmigration + 
                        IntOfHomosexuality + ProEU | cntry), data=ESS2008sd)
beep()
stargazer(modelX2008, type = "text", digits = 2)

#Estimating the Interaction Effects
p1Sal2008 <- interplot(m = modelX2008, var1 = "EconomicInequality", 
                       var2 = "EcoSaliency") + lims(y=c(-.3, 1.1)) +
  theme_light() + ggtitle("2008") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Salience of Economic Issues") + 
  ylab("*Economic Inequality*") + 
  theme(axis.title.y = ggtext::element_markdown(), 
        plot.title = element_text(hjust = 0.5), 
        axis.title=element_text(size=14))

dens12008 <- ggplot(ESS2008, aes(x = EcoSaliency)) + 
  geom_histogram(color = "black", fill = "white", bins = 20) + 
  theme_void()

p2Sal2008 <- interplot(m = modelX2008, 
                       var1 = "LessImmigration", 
                       var2 = "ImmiSaliency") + lims(y=c(-.3, 1.1)) +
  theme_light() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Salience of Multiculturalism") + 
  ylab("*Less Immigration*") + 
  theme(axis.title.y = ggtext::element_markdown(), 
        axis.title=element_text(size=14))

dens22008 <- ggplot(ESS2008, aes(x = ImmiSaliency)) + 
  geom_histogram(color = "black", fill = "white", bins = 20) + 
  theme_void()

p3Sal2008 <- interplot(m = modelX2008, var1 = "IntOfHomosexuality", 
                       var2 = "SocialLifeStyleSaliency") + lims(y=c(-.3, 1.1)) +
  theme_light() +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  xlab("Salience of Social Lifestyle") + 
  ylab("*Intolerance of Homosexuality*") + 
  theme(axis.title.y = ggtext::element_markdown(), 
        axis.title=element_text(size=14))

dens32008 <- ggplot(ESS2008, aes(x = SocialLifeStyleSaliency)) + 
  geom_histogram(color = "black", fill = "white", bins = 20) + 
  theme_void()
