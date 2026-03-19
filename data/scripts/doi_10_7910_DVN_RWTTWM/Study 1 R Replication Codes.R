# Cleaning the Work Environment ####
rm(list=ls(all=T))
gc()

getwd()

library(sjPlot)
library(multcomp)
library(stargazer)

#### Loading and Merging the Datasets ####

s1 <- readRDS(file = "Study 1 Data.rds")

#### Data Preperation ####

s1$Q31_1[s1$Q31_1==99] <- NA
names(s1)[names(s1)=="Q31_1"] <- "Self_Conservatism"

s1$Q36[s1$Q36==99] <- NA
s1$Q36[s1$Q36==2] <- 0
names(s1)[names(s1)=="Q36"] <- "sex" # 1 - female, 0 - male
table(s1$sex, exclude = NULL)
s1$sex <- 1 + s1$sex*-1 # recoding # 0 - female, 1 - male
table(s1$sex, exclude = NULL) 

s1$Q38[s1$Q38==99] <- NA
names(s1)[names(s1)=="Q38"] <- "education"
table(s1$education, exclude = NULL)

s1$Q7_1[s1$Q7_1==99] <- NA
names(s1)[names(s1)=="Q7_1"] <- "self_left_right"
table(s1$self_left_right, exclude = NULL)

s1$Affiliation <- NA
s1$Affiliation[s1$Q3==99] <- 0
s1$Affiliation[s1$Q3==1] <- 2
s1$Affiliation[s1$Q3==2] <- 1
table(s1$Affiliation, exclude = NULL)

table(s1$Q4_8_TEXT, exclude = NULL)

table(s1$Q4, exclude = NULL)
s1$Partisanship <- NA
s1$Partisanship[s1$Q4==1] <- "Justice and Development Party (AKP)"
s1$Partisanship[s1$Q4==2] <- "Republican People's Party (CHP)"
s1$Partisanship[s1$Q4==3] <- "Other Parties on the Left"
s1$Partisanship[s1$Q4==4] <- "Other Parties on the Right"
s1$Partisanship[s1$Q4==5] <- "Other Parties on the Right"
s1$Partisanship[s1$Q4==6] <- "Other Parties on the Right"
s1$Partisanship[s1$Q4==7] <- "Other Parties on the Left"
s1$Partisanship[s1$Q4_8_TEXT == "EMEP" | 
                  s1$Q4_8_TEXT == "Özgürlük ve dayanışma partisi" | 
                  s1$Q4_8_TEXT == "TKP" | 
                  s1$Q4_8_TEXT == "Türkiye işçi partisi" | 
                  s1$Q4_8_TEXT == "Türkiye Komünist Partisi (TKP)"] <- "Other Parties on the Left"
s1$Partisanship[s1$Q4_8_TEXT == "Ldp"] <- "Other Parties on the Right"
s1$Partisanship <- ifelse(is.na(s1$Partisanship),"Bipartisan or Non-Responsive", s1$Partisanship)
table(s1$Partisanship, exclude=NULL)
s1$Partisanship <- factor(s1$Partisanship, ordered = F) 
table(s1$Partisanship, exclude=NULL)

s1$bipartisan_na <- NA
s1$bipartisan_na[s1$Partisanship=="Bipartisan or Non-Responsive"] <- 1
s1$bipartisan_na[s1$Partisanship!="Bipartisan or Non-Responsive"] <- 0
table(s1$bipartisan_na, exclude = NULL)

s1$akp <- NA
s1$akp[s1$Partisanship=="Justice and Development Party (AKP)"] <- 1
s1$akp[s1$Partisanship!="Justice and Development Party (AKP)"] <- 0
table(s1$akp, exclude = NULL)

s1$chp <- NA
s1$chp[s1$Partisanship=="Republican People's Party (CHP)"] <- 1
s1$chp[s1$Partisanship!="Republican People's Party (CHP)"] <- 0
table(s1$chp, exclude = NULL)

s1$other_right <- NA
s1$other_right[s1$Partisanship=="Other Parties on the Right"] <- 1
s1$other_right[s1$Partisanship!="Other Parties on the Right"] <- 0
table(s1$other_right, exclude = NULL)

s1$other_left <- NA
s1$other_left[s1$Partisanship=="Other Parties on the Left"] <- 1
s1$other_left[s1$Partisanship!="Other Parties on the Left"] <- 0
table(s1$other_left, exclude = NULL)

mean(s1$akp, na.rm = T)
s1$beta_akp <- (s1$akp - mean(s1$akp, na.rm = T))
table(s1$beta_akp, exclude = NULL)

mean(s1$chp, na.rm = T)
s1$beta_chp <- (s1$chp - mean(s1$chp, na.rm = T))
table(s1$beta_chp, exclude = NULL)

mean(s1$other_right, na.rm = T)
s1$beta_other_right <- (s1$other_right - mean(s1$other_right, na.rm = T))
table(s1$beta_other_right, exclude = NULL)

mean(s1$other_left, na.rm = T)
s1$beta_other_left <- (s1$other_left - mean(s1$other_left, na.rm = T))
table(s1$beta_other_left, exclude = NULL)

s1$Terror_Prime <- NA
s1$Terror_Prime[s1$Group == "Treatment 2"] <- 1
s1$Terror_Prime[s1$Group == "Control"] <- 0
table(s1$Terror_Prime, exclude = NULL)

table(s1$Q15_1, exclude = NULL)
s1$Q15_1 <- ifelse(s1$Q15_1>11, NA, s1$Q15_1)
s1$Q15_1[s1$Terror_Prime==0] <- 0

table(s1$Q15_2, exclude = NULL)
s1$Q15_2 <- ifelse(s1$Q15_2>11, NA, s1$Q15_2)
s1$Q15_2[s1$Terror_Prime==0] <- 0

table(s1$Q15_3, exclude = NULL)
s1$Q15_3 <- ifelse(s1$Q15_3>11, NA, s1$Q15_3)
s1$Q15_3[s1$Terror_Prime==0] <- 0

table(s1$Q15_4, exclude = NULL)
s1$Q15_4 <- ifelse(s1$Q15_4>11, NA, s1$Q15_4)
s1$Q15_4[s1$Terror_Prime==0] <- 0

table(s1$Q15_5, exclude = NULL)
s1$Q15_5 <- ifelse(s1$Q15_5>11, NA, s1$Q15_5)
s1$Q15_5[s1$Terror_Prime==0] <- 0

table(s1$Q15_6, exclude = NULL)
s1$Q15_6 <- ifelse(s1$Q15_6>11, NA, s1$Q15_6)
s1$Q15_6[s1$Terror_Prime==0] <- 0

s1$terror_prime_fear <- s1$Q15_4 + s1$Q15_5 + s1$Q15_6
table(s1$terror_prime_fear, exclude = NULL)

#### Manipulation Check ####

s1$Q17_1[s1$Q17_1==99] <- NA
s1$Q17_2[s1$Q17_2==99] <- NA

s1$MC1 <- NA
s1$MC1[s1$Q17_1==6 & s1$Q17_2==8] <- 1
s1$MC1[(s1$Q17_1!=6 | s1$Q17_2!=8) & (!is.na(s1$Q17_1) & !is.na(s1$Q17_2))] <- 0
s1$MC1 <- ifelse(is.na(s1$MC1), 0, s1$MC1)
table(s1$MC1, exclude = NULL)

s1$Q18_1[s1$Q18_1==99] <- NA
s1$Q18_2[s1$Q18_2==99] <- NA

s1$MC2 <- NA
s1$MC2[s1$Q18_1==1 & s1$Q18_2==4] <- 1
s1$MC2[(s1$Q18_1!=1 | s1$Q18_2!=4) & (!is.na(s1$Q18_1) & !is.na(s1$Q18_2))] <- 0
s1$MC2 <- ifelse(is.na(s1$MC2), 0, s1$MC2)
table(s1$MC2, exclude = NULL)

s1$Q19_1[s1$Q19_1==99] <- NA
s1$Q19_2[s1$Q19_2==99] <- NA

s1$MC3 <- NA
s1$MC3[s1$Q19_1==1 & s1$Q19_2==4] <- 1
s1$MC3[(s1$Q19_1!=1 | s1$Q19_2!=4) & (!is.na(s1$Q19_1) & !is.na(s1$Q19_2))] <- 0
s1$MC3 <- ifelse(is.na(s1$MC3), 0, s1$MC3)
table(s1$MC3, exclude = NULL)

s1$Manipulation_Check <- s1$MC1 + s1$MC2+ s1$MC3
table(s1$Manipulation_Check, s1$Group, exclude = NULL)

bartlett.test(s1$Manipulation_Check ~ s1$Terror_Prime)
t.test(s1$Manipulation_Check ~ s1$Terror_Prime, var.equal=FALSE, alternative="less")

#### Manuscript, Table 1. Study 1 descriptive statistics. ####

library(table1)

my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=2), c("",
                                                           "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
}
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
                                                  sprintf("%d (%0.0f %%)", FREQ, PCT))))
}

table1(~ Self_Conservatism + terror_prime_fear + sex + age + education + self_left_right | Terror_Prime, data=s1, 
       render.continuous=my.render.cont, render.categorical=my.render.cat, overall=NULL)

treatment <- aov(Self_Conservatism ~ factor(Terror_Prime), data = s1)
summary(treatment)

sex <- aov(sex ~ factor(Terror_Prime), data = s1)
summary(sex)

age <- aov(age ~ factor(Terror_Prime), data = s1)
summary(age)

education <- aov(education ~ factor(Terror_Prime), data = s1)
summary(education)

self_left_right <- aov(self_left_right ~ factor(Terror_Prime), data = s1)
summary(self_left_right)

#### Standardization ####

s1 <- subset(s1, subset = !is.na(Self_Conservatism))

mean(s1$Terror_Prime, na.rm = T)
s1$beta_Terror_Prime <- (s1$Terror_Prime - mean(s1$Terror_Prime, na.rm = T))
table(s1$beta_Terror_Prime, exclude = NULL)

mean(s1$sex, na.rm = T)
s1$beta_sex <- (s1$sex - mean(s1$sex, na.rm = T))
table(s1$beta_sex, exclude = NULL)

mean(s1$age, na.rm = T)
sd(s1$age, na.rm = T)
s1$beta_age <- (s1$age - mean(s1$age, na.rm = T))/(2*sd(s1$age, na.rm = T))
table(s1$beta_age, exclude = NULL)

mean(s1$education, na.rm = T)
sd(s1$education, na.rm = T)
s1$beta_education <- (s1$education - mean(s1$education, na.rm = T))/(2*sd(s1$education, na.rm = T))
table(s1$beta_education, exclude = NULL)

mean(s1$self_left_right, na.rm = T)
sd(s1$self_left_right, na.rm = T)
s1$beta_self_left_right <- (s1$self_left_right - mean(s1$self_left_right, na.rm = T))/(2*sd(s1$self_left_right, na.rm = T))
table(s1$beta_self_left_right, exclude = NULL)

mean(s1$Self_Conservatism, na.rm = T)
sd(s1$Self_Conservatism, na.rm = T)
s1$beta_Self_Conservatism <- (s1$Self_Conservatism - mean(s1$Self_Conservatism, na.rm = T))/(2*sd(s1$Self_Conservatism, na.rm = T))
table(s1$beta_Self_Conservatism, exclude = NULL)

mean(s1$terror_prime_fear, na.rm = T)
sd(s1$terror_prime_fear, na.rm = T)
s1$beta_terror_prime_fear <- (s1$terror_prime_fear - mean(s1$terror_prime_fear, na.rm = T))/(2*sd(s1$terror_prime_fear, na.rm = T))
table(s1$beta_terror_prime_fear, exclude = NULL)

s1$region <- NA
s1$region[s1$State=='Istanbul'] <- "Istanbul"
s1$region[s1$State=='Tekirdag' |
            s1$State=='Edirne' |
            s1$State== 'Kirklareli' |
            s1$State== 'Balikesir' | 
            s1$State== 'Canakkale'] <- "West Marmara"
s1$region[s1$State=='Izmir' |
            s1$State== 'Aydin' | 
            s1$State==  'Denizli' |
            s1$State==  'Mugla' |
            s1$State==  'Manisa' |
            s1$State==  'Afyon' |
            s1$State== 'Kutahya' |
            s1$State== 'Usak'] <- "Aegean"
s1$region[s1$State=='Bursa' |
            s1$State== 'Eskisehir' |
            s1$State== 'Bilecik' |
            s1$State== 'Kocaeli' |
            s1$State== 'Sakarya' |
            s1$State==  'Duzce' |
            s1$State==  'Bolu' |
            s1$State== 'Yalova'] <- "East Marmara"
s1$region[s1$State=='Ankara' |
            s1$State==  'Konya' | s1$State== 'Karaman'] <- "West Anatolia"
s1$region[s1$State=='Antalya' |
            s1$State== 'Isparta' |
            s1$State==  'Adana' |
            s1$State==   'Mersin' |
            s1$State==   'Hatay' |
            s1$State==  'Kahramanmaras' |
            s1$State==   'Osmaniye' | s1$State== 'Burdur'] <- "Mediterranean"
s1$region[s1$State=='Kirikkale' |
            s1$State== 'Aksaray' |
            s1$State==  'Nigde' |
            s1$State==   'Nevsehir' |
            s1$State==   'Kirsehir' |
            s1$State==   'Kayseri' | 
            s1$State==  'Sivas' |
            s1$State==  'Yozgat'] <- "Central Anatolia"
s1$region[s1$State=='Zonguldak' |
            s1$State== 'Karabuk' |
            s1$State==  'Bartin' |
            s1$State==  'Kastamonu' |
            s1$State== 'Cankiri' |
            s1$State==  'Sinop' |
            s1$State==  'Samsun' |
            s1$State==  'Tokat' |
            s1$State==  'Corum' |
            s1$State== 'Amasya'] <- "West Black Sea"
s1$region[s1$State=='Ordu' |
            s1$State=='Trabzon' |
            s1$State=='Giresun' |
            s1$State== 'Rize' |
            s1$State== 'Artvin' |
            s1$State== 'Gumushane'] <- "East Black Sea"
s1$region[s1$State=='Erzurum' |
            s1$State=='Erzincan' |
            s1$State== 'Agri' |
            s1$State== 'Kars' | s1$State== 'Ardahan' | s1$State== 'Bayburt' | s1$State== 'Igdir'] <- "Northeast Anatolia"
s1$region[s1$State=='Malatya' |
            s1$State== 'Elazig' |
            s1$State== 'Bingol' |
            s1$State== 'Tunceli' |
            s1$State== 'Van' |
            s1$State== 'Mus' |
            s1$State== 'Bitlis' |
            s1$State== 'Hakkari'] <- "Central East Anatolia"
s1$region[s1$State=='Gaziantep' |
            s1$State== 'Adiyaman' |
            s1$State== 'Kilis' |
            s1$State== 'Sanliurfa' |
            s1$State== 'Diyarbakir' |
            s1$State== 'Mardin' |
            s1$State== 'Batman' |
            s1$State== 'Sirnak' |
            s1$State== 'Siirt'] <- "Southeast Anatolia"
table(s1$region, exclude = NULL)

s1$region <- ifelse(is.na(s1$region), "Outside Turkey", s1$region)

#### Online Appendices, Table 13. Study 1 logistic regression modeling of assignment to terrorism mortality salience. ####

library(nnet)

balance_test <-  multinom(Terror_Prime ~ beta_sex + beta_age + beta_education + beta_self_left_right + beta_akp + beta_chp + beta_other_right + beta_other_left, data=s1)
summary(balance_test)
lmtest::lrtest(balance_test)
tab_model(balance_test, show.ci = 0.95)

C <- diag(9)
model.mc <- glht(balance_test, linfct = C)
summary(model.mc)
BH.OUT <- summary(model.mc, test = adjusted(type = "BH"))
BH.OUT$test

stargazer(balance_test, type="html", out="balance_test.htm",
          star.cutoffs = c(0.05, 0.01, 0.001),
          digits=2,
          covariate.labels =c("Sex: Male", "Age", "Education", "Left-Right Ideological Placement", "Partisanship: AKP", "Partisanship: CHP", "Partisanship: Other Parties on the Right", "Partisanship: Other Parties on the Left", "Intercept"),
          dep.var.labels   = "Terrorism Mortality Salience")


#### Manuscript, Table 2. Study 1 Causal mediation analysis models. ####

library(mediation)
set.seed(123)

out.fit.m01 <- lm(beta_Self_Conservatism ~ beta_Terror_Prime, data=s1)
summary(out.fit.m01)
tab_model(out.fit.m01, show.ci = 0.95)

confint(out.fit.m01, 'beta_Terror_Prime', level=0.95)

C <- diag(2)
model.mc <- glht(out.fit.m01, linfct = C)
summary(model.mc)
BH.OUT <- summary(model.mc, test = adjusted(type = "BH"))
BH.OUT$test

med.fit.m1 <- lm(beta_terror_prime_fear ~ beta_Terror_Prime, data=s1)
summary(med.fit.m1)
tab_model(med.fit.m1, show.ci = 0.95)

C <- diag(2)
model.mc <- glht(med.fit.m1, linfct = C)
summary(model.mc)
BH.OUT <- summary(model.mc, test = adjusted(type = "BH"))
BH.OUT$test

out.fit.m1 <- lm(beta_Self_Conservatism ~ beta_Terror_Prime + beta_terror_prime_fear, data=s1)
summary(out.fit.m1)
tab_model(out.fit.m1, show.ci = 0.95)

confint(out.fit.m1, 'beta_Terror_Prime', level=0.95)

C <- diag(3)
model.mc <- glht(out.fit.m1, linfct = C)
summary(model.mc)
BH.OUT <- summary(model.mc, test = adjusted(type = "BH"))
BH.OUT$test

set.seed(123)
med.out.m1 <- mediate(med.fit.m1, out.fit.m1, treat = "beta_Terror_Prime", mediator = "beta_terror_prime_fear", robustSE = TRUE, sims = 10000, boot = T, conf.level = 0.95)
summary(med.out.m1)

out.fit.m02 <- lm(beta_Self_Conservatism ~ beta_Terror_Prime + beta_sex + beta_age + beta_education + beta_self_left_right, data=s1)
summary(out.fit.m02)
tab_model(out.fit.m02, show.ci = 0.95)

confint(out.fit.m02, 'beta_Terror_Prime', level=0.95)

C <- diag(6)
model.mc <- glht(out.fit.m02, linfct = C)
summary(model.mc)
BH.OUT <- summary(model.mc, test = adjusted(type = "BH"))
BH.OUT$test

med.fit.m2 <- lm(beta_terror_prime_fear ~ beta_Terror_Prime + beta_sex + beta_age + beta_education + beta_self_left_right, data=s1)
summary(med.fit.m2)
tab_model(med.fit.m2, show.ci = 0.95)

C <- diag(6)
model.mc <- glht(med.fit.m2, linfct = C)
summary(model.mc)
BH.OUT <- summary(model.mc, test = adjusted(type = "BH"))
BH.OUT$test

out.fit.m2 <- lm(beta_Self_Conservatism ~ beta_Terror_Prime + beta_terror_prime_fear + beta_sex + beta_age + beta_education + beta_self_left_right, data=s1)
summary(out.fit.m2)
tab_model(out.fit.m2, show.ci = 0.95)

confint(out.fit.m2, 'beta_Terror_Prime', level=0.95)

C <- diag(7)
model.mc <- glht(out.fit.m2, linfct = C)
summary(model.mc)
BH.OUT <- summary(model.mc, test = adjusted(type = "BH"))
BH.OUT$test

set.seed(123)
med.out.m2 <- mediate(med.fit.m2, out.fit.m2, treat = "beta_Terror_Prime", mediator = "beta_terror_prime_fear", robustSE = TRUE, sims = 10000, boot = T, conf.level = 0.95)
summary(med.out.m2)

library(lme4)

s1$region_partisanship <- paste(s1$region, s1$Partisanship)

out.fit.m03 <- lmer(beta_Self_Conservatism ~ beta_Terror_Prime + beta_sex + beta_age + beta_education + beta_self_left_right + (1 | region_partisanship), data=s1)
summary(out.fit.m03)
tab_model(out.fit.m03, show.ci = 0.95)

confint(out.fit.m03, 'beta_Terror_Prime', level=0.95)

C <- diag(6)
model.mc <- glht(out.fit.m02, linfct = C)
summary(model.mc)
BH.OUT <- summary(model.mc, test = adjusted(type = "BH"))
BH.OUT$test

med.fit.m3 <- lmer(beta_terror_prime_fear ~ beta_Terror_Prime + beta_sex + beta_age + beta_education + beta_self_left_right + (1 | region_partisanship), data=s1)
summary(med.fit.m3)
tab_model(med.fit.m3, show.ci = 0.95)

C <- diag(6)
model.mc <- glht(med.fit.m3, linfct = C)
summary(model.mc)
BH.OUT <- summary(model.mc, test = adjusted(type = "BH"))
BH.OUT$test

out.fit.m3 <- lmer(beta_Self_Conservatism ~ beta_Terror_Prime + beta_terror_prime_fear + beta_sex + beta_age + beta_education + beta_self_left_right + (1 | region_partisanship), data=s1)
summary(out.fit.m3)
tab_model(out.fit.m3, show.ci = 0.95)

confint(out.fit.m3, 'beta_Terror_Prime', level=0.95)

C <- diag(7)
model.mc <- glht(out.fit.m3, linfct = C)
summary(model.mc)
BH.OUT <- summary(model.mc, test = adjusted(type = "BH"))
BH.OUT$test

class(out.fit.m03) <- "lmerMod"
class(med.fit.m3) <- "lmerMod"
class(out.fit.m3) <- "lmerMod"

set.seed(123)
med.out.m3 <- mediate(med.fit.m3, out.fit.m3, treat = "beta_Terror_Prime", mediator = "beta_terror_prime_fear", sims = 10000, conf.level = 0.95)
summary(med.out.m3)

library(stargazer)

stargazer(out.fit.m01, med.fit.m1, out.fit.m1, out.fit.m02, med.fit.m2, out.fit.m2, out.fit.m03, med.fit.m3, out.fit.m3,
          type = "html", title=" ", digits=2, out="s1_model_outputs.htm",
          star.cutoffs = c(0.05, 0.01, 0.001),
          model.numbers = F,
          column.labels = c("Model 0.1.1", "Model 1.1", "Model 1.2", "Model 0.2.1", "Model 2.1", "Model 2.2", "Model 0.3.1", "Model 3.1", "Model 3.2"),
          covariate.labels = c("Terrorism Mortality Salience", 
                               "Terrorism Mortality Fear",
                               "Sex: Male",
                               "Age", 
                               "Education", 
                               "Left-Right Ideological Placement"),
          add.lines = list(c("Estimation", "OLS", "OLS", "OLS", "OLS", "OLS", "OLS", "MLM", "MLM", "MLM")))

#### Online Appendices, Figure 1. Study 1 serial mediation plot. ####

s1$Q20_1[s1$Q20_1==99] <- NA
s1$Q20_2[s1$Q20_2==99] <- NA
s1$Q20_3[s1$Q20_3==99] <- NA
s1$Q20_4[s1$Q20_4==99] <- NA
s1$Q21_1[s1$Q21_1==99] <- NA
s1$Q21_2[s1$Q21_2==99] <- NA
s1$Q21_3[s1$Q21_3==99] <- NA
s1$Q21_4[s1$Q21_4==99] <- NA
s1$Q22_1[s1$Q22_1==99] <- NA
s1$Q22_2[s1$Q22_2==99] <- NA
s1$Q22_3[s1$Q22_3==99] <- NA
s1$Q22_4[s1$Q22_4==99] <- NA
s1$Q23_1[s1$Q23_1==99] <- NA
s1$Q23_2[s1$Q23_2==99] <- NA
s1$Q23_3[s1$Q23_3==99] <- NA
s1$Q23_4[s1$Q23_4==99] <- NA
s1$Q24_1[s1$Q24_1==99] <- NA
s1$Q24_2[s1$Q24_2==99] <- NA
s1$Q24_3[s1$Q24_3==99] <- NA
s1$Q24_4[s1$Q24_4==99] <- NA
s1$Q25_1[s1$Q25_1==99] <- NA
s1$Q25_2[s1$Q25_2==99] <- NA
s1$Q25_3[s1$Q25_3==99] <- NA
s1$Q25_4[s1$Q25_4==99] <- NA
s1$Q26_1[s1$Q26_1==99] <- NA
s1$Q26_2[s1$Q26_2==99] <- NA
s1$Q26_3[s1$Q26_3==99] <- NA
s1$Q26_4[s1$Q26_4==99] <- NA
s1$Q27_1[s1$Q27_1==99] <- NA
s1$Q27_2[s1$Q27_2==99] <- NA
s1$Q27_3[s1$Q27_3==99] <- NA
s1$Q27_4[s1$Q27_4==99] <- NA
s1$Q28_1[s1$Q28_1==99] <- NA
s1$Q28_2[s1$Q28_2==99] <- NA
s1$Q28_3[s1$Q28_3==99] <- NA
s1$Q28_4[s1$Q28_4==99] <- NA
s1$Q29_1[s1$Q29_1==99] <- NA
s1$Q29_2[s1$Q29_2==99] <- NA
s1$Q29_3[s1$Q29_3==99] <- NA
s1$Q29_4[s1$Q29_4==99] <- NA
s1$Q30_1[s1$Q30_1==99] <- NA
s1$Q30_2[s1$Q30_2==99] <- NA
s1$Q30_3[s1$Q30_3==99] <- NA
s1$Q30_4[s1$Q30_4==99] <- NA

names(s1)[names(s1)=="Q20_1"] <- "extraversion_1"
s1$Q21_2R <- 12 - s1$Q21_2
names(s1)[names(s1)=="Q21_2R"] <- "extraversion_2"
names(s1)[names(s1)=="Q22_3"] <- "extraversion_3"
names(s1)[names(s1)=="Q23_4"] <- "extraversion_4"
s1$Q25_1R <- 12 - s1$Q25_1
names(s1)[names(s1)=="Q25_1R"] <- "extraversion_5"
names(s1)[names(s1)=="Q26_2"] <- "extraversion_6"
s1$Q27_3R <- 12 - s1$Q27_3
names(s1)[names(s1)=="Q27_3R"] <- "extraversion_7"
names(s1)[names(s1)=="Q28_4"] <- "extraversion_8"

psych::alpha(s1[,c("extraversion_1", "extraversion_2", "extraversion_3", "extraversion_4",
                   "extraversion_5", "extraversion_6", "extraversion_7", "extraversion_8")])

s1$extraversion <- s1$extraversion_1 + 
  s1$extraversion_2 + 
  s1$extraversion_3 + 
  s1$extraversion_4 + 
  s1$extraversion_5 + 
  s1$extraversion_6 + 
  s1$extraversion_7 + 
  s1$extraversion_8

psych::describeBy(s1$extraversion, s1$Terror_Prime)

mean(s1$extraversion, na.rm = T)
sd(s1$extraversion, na.rm = T)
s1$beta_extraversion <- (s1$extraversion - mean(s1$extraversion, na.rm = T))/(2*sd(s1$extraversion, na.rm = T))
table(s1$beta_extraversion, exclude = NULL)

s1$Q20_2R <- 12 - s1$Q20_2
names(s1)[names(s1)=="Q20_2R"] <- "agreeableness_1"
names(s1)[names(s1)=="Q21_3"] <- "agreeableness_2"
s1$Q22_4R <- 12 - s1$Q22_4
names(s1)[names(s1)=="Q22_4R"] <- "agreeableness_3"
names(s1)[names(s1)=="Q24_1"] <- "agreeableness_4"
names(s1)[names(s1)=="Q25_2"] <- "agreeableness_5"
s1$Q26_3R <- 12 - s1$Q26_3
names(s1)[names(s1)=="Q26_3R"] <- "agreeableness_6"
names(s1)[names(s1)=="Q27_4"] <- "agreeableness_7"
s1$Q29_1R <- 12 - s1$Q29_1
names(s1)[names(s1)=="Q29_1R"] <- "agreeableness_8"
names(s1)[names(s1)=="Q30_2"] <- "agreeableness_9"

psych::alpha(s1[,c("agreeableness_1", "agreeableness_2", "agreeableness_3", "agreeableness_4",
                   "agreeableness_5", "agreeableness_6", "agreeableness_7", "agreeableness_8", "agreeableness_9")])

s1$agreeableness <- s1$agreeableness_1 + 
  s1$agreeableness_2 + 
  s1$agreeableness_3 + 
  s1$agreeableness_4 + 
  s1$agreeableness_5 + 
  s1$agreeableness_6 + 
  s1$agreeableness_7 + 
  s1$agreeableness_8 +
  s1$agreeableness_9

psych::describeBy(s1$agreeableness, s1$Terror_Prime)

mean(s1$agreeableness, na.rm = T)
sd(s1$agreeableness, na.rm = T)
s1$beta_agreeableness <- (s1$agreeableness - mean(s1$agreeableness, na.rm = T))/(2*sd(s1$agreeableness, na.rm = T))
table(s1$beta_agreeableness, exclude = NULL)

names(s1)[names(s1)=="Q20_3"] <- "conscientiousness_1"
s1$Q21_4R <- 12 - s1$Q21_4
names(s1)[names(s1)=="Q21_4R"] <- "conscientiousness_2"
names(s1)[names(s1)=="Q23_1"] <- "conscientiousness_3"
s1$Q24_2R <- 12 - s1$Q24_2
names(s1)[names(s1)=="Q24_2R"] <- "conscientiousness_4"
s1$Q25_3R <- 12 - s1$Q25_3
names(s1)[names(s1)=="Q25_3R"] <- "conscientiousness_5"
names(s1)[names(s1)=="Q26_4"] <- "conscientiousness_6"
names(s1)[names(s1)=="Q28_1"] <- "conscientiousness_7"
names(s1)[names(s1)=="Q29_2"] <- "conscientiousness_8"
s1$Q30_3R <- 12 - s1$Q30_3
names(s1)[names(s1)=="Q30_3R"] <- "conscientiousness_9"

psych::alpha(s1[,c("conscientiousness_1", "conscientiousness_2", "conscientiousness_3", "conscientiousness_4",
                   "conscientiousness_5", "conscientiousness_6", "conscientiousness_7", "conscientiousness_8", "conscientiousness_9")])

s1$conscientiousness <- s1$conscientiousness_1 + 
  s1$conscientiousness_2 + 
  s1$conscientiousness_3 + 
  s1$conscientiousness_4 + 
  s1$conscientiousness_5 + 
  s1$conscientiousness_6 + 
  s1$conscientiousness_7 + 
  s1$conscientiousness_8 +
  s1$conscientiousness_9

psych::describeBy(s1$conscientiousness, s1$Terror_Prime)

mean(s1$conscientiousness, na.rm = T)
sd(s1$conscientiousness, na.rm = T)
s1$beta_conscientiousness <- (s1$conscientiousness - mean(s1$conscientiousness, na.rm = T))/(2*sd(s1$conscientiousness, na.rm = T))
table(s1$beta_conscientiousness, exclude = NULL)

names(s1)[names(s1)=="Q20_4"] <- "neuroticism_1"
s1$Q22_1R <- 12 - s1$Q22_1
names(s1)[names(s1)=="Q22_1R"] <- "neuroticism_2"
names(s1)[names(s1)=="Q23_2"] <- "neuroticism_3"
names(s1)[names(s1)=="Q24_3"] <- "neuroticism_4"
s1$Q25_4R <- 12 - s1$Q25_4
names(s1)[names(s1)=="Q25_4R"] <- "neuroticism_5"
names(s1)[names(s1)=="Q27_1"] <- "neuroticism_6"
s1$Q28_2R <- 12 - s1$Q28_2
names(s1)[names(s1)=="Q28_2R"] <- "neuroticism_7"
names(s1)[names(s1)=="Q29_3"] <- "neuroticism_8"

psych::alpha(s1[,c("neuroticism_1", "neuroticism_2", "neuroticism_3", "neuroticism_4",
                   "neuroticism_5", "neuroticism_6", "neuroticism_7", "neuroticism_8")])

s1$neuroticism <- s1$neuroticism_1 + 
  s1$neuroticism_2 + 
  s1$neuroticism_3 + 
  s1$neuroticism_4 + 
  s1$neuroticism_5 + 
  s1$neuroticism_6 + 
  s1$neuroticism_7 + 
  s1$neuroticism_8

psych::describeBy(s1$neuroticism, s1$Terror_Prime)

mean(s1$neuroticism, na.rm = T)
sd(s1$neuroticism, na.rm = T)
s1$beta_neuroticism <- (s1$neuroticism - mean(s1$neuroticism, na.rm = T))/(2*sd(s1$neuroticism, na.rm = T))
table(s1$beta_neuroticism, exclude = NULL)

names(s1)[names(s1)=="Q21_1"] <- "openness_1"
names(s1)[names(s1)=="Q22_2"] <- "openness_2"
names(s1)[names(s1)=="Q23_3"] <- "openness_3"
names(s1)[names(s1)=="Q24_4"] <- "openness_4"
names(s1)[names(s1)=="Q26_1"] <- "openness_5"
names(s1)[names(s1)=="Q27_2"] <- "openness_6"
s1$Q28_3R <- 12 - s1$Q28_3
names(s1)[names(s1)=="Q28_3R"] <- "openness_7"
names(s1)[names(s1)=="Q29_4"] <- "openness_8"
s1$Q30_1R <- 12 - s1$Q30_1
names(s1)[names(s1)=="Q30_1R"] <- "openness_9"
names(s1)[names(s1)=="Q30_4"] <- "openness_10"

psych::alpha(s1[,c("openness_1", "openness_2", "openness_3", "openness_4",
                   "openness_5", "openness_6", "openness_7", "openness_8", "openness_9", "openness_10")])

s1$openness <- s1$openness_1 + 
  s1$openness_2 + 
  s1$openness_3 + 
  s1$openness_4 + 
  s1$openness_5 + 
  s1$openness_6 + 
  s1$openness_7 + 
  s1$openness_8 +
  s1$openness_9 +
  s1$openness_10

psych::describeBy(s1$openness, s1$Terror_Prime)

mean(s1$openness, na.rm = T)
sd(s1$openness, na.rm = T)
s1$beta_openness <- (s1$openness - mean(s1$openness, na.rm = T))/(2*sd(s1$openness, na.rm = T))
table(s1$beta_openness, exclude = NULL)

library(lavaan)

model=
  "
  #Regressions
  beta_terror_prime_fear ~ t_f*beta_Terror_Prime + beta_sex + beta_age + beta_education + beta_self_left_right
  beta_openness ~ f_po*beta_terror_prime_fear + t_po*beta_Terror_Prime + beta_sex + beta_age + beta_education + beta_self_left_right
  beta_conscientiousness ~ f_pc*beta_terror_prime_fear + t_pc*beta_Terror_Prime + beta_sex + beta_age + beta_education + beta_self_left_right
  beta_extraversion ~ f_pe*beta_terror_prime_fear + t_pe*beta_Terror_Prime + beta_sex + beta_age + beta_education + beta_self_left_right
  beta_agreeableness ~ f_pa*beta_terror_prime_fear + t_pa*beta_Terror_Prime + beta_sex + beta_age + beta_education + beta_self_left_right
  beta_neuroticism ~ f_pn*beta_terror_prime_fear + t_pn*beta_Terror_Prime + beta_sex + beta_age + beta_education + beta_self_left_right
  beta_Self_Conservatism ~ po*beta_openness + pc*beta_conscientiousness + pe*beta_extraversion + pa*beta_agreeableness + pn*beta_neuroticism + beta_terror_prime_fear + d*beta_Terror_Prime + beta_sex + beta_age + beta_education + beta_self_left_right
  
  #Defined Parameters:
  ie_all_o := t_f*f_po*po
  ie_all_c := t_f*f_pc*pc
  ie_all_e := t_f*f_pe*pe
  ie_all_a := t_f*f_pa*pa
  ie_all_n := t_f*f_pn*pn
  ie_short_o := t_po*po
  ie_short_c := t_pc*pc
  ie_short_e := t_pe*pe
  ie_short_a := t_pa*pa
  ie_short_n := t_pn*pn
  de := d
  
  beta_openness ~~ beta_conscientiousness
  beta_openness ~~      beta_extraversion
  beta_openness ~~     beta_agreeableness
  beta_openness ~~       beta_neuroticism
  beta_conscientiousness ~~      beta_extraversion
  beta_conscientiousness ~~     beta_agreeableness
  beta_conscientiousness ~~       beta_neuroticism
  beta_extraversion ~~     beta_agreeableness
  beta_extraversion ~~       beta_neuroticism
  beta_agreeableness ~~       beta_neuroticism
"

fit <-  sem(model, data=s1, se = "bootstrap", bootstrap = 10000)
summary(fit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
parameterEstimates(fit, rsquare=T, output = "text", level = 0.95)

psych::describe(s1$beta_Terror_Prime)
psych::describe(s1$beta_terror_prime_fear)
psych::describe(s1$beta_agreeableness)
psych::describe(s1$beta_Self_Conservatism)

#### Online Appendices, Miscellaneous Statistics, Reliability Statistics of the Mediator in Study 1 ####

fear.reliability.data <- subset(s1, select = c("Q15_1", "Q15_2", "Q15_3", "Q15_4", "Q15_5", "Q15_6"), subset = Terror_Prime==1)
psych::alpha(fear.reliability.data[,c("Q15_1", "Q15_2", "Q15_3", "Q15_4", "Q15_5", "Q15_6")])
psych::alpha(fear.reliability.data[,c("Q15_1", "Q15_2", "Q15_3")])
psych::alpha(fear.reliability.data[,c("Q15_4", "Q15_5", "Q15_6")])

#### Online Appendices, Figure 4. Study 1 parallel analysis scree plot for the mortality fear items. ####
psych::fa.parallel(fear.reliability.data, main=" ", fm = 'minres', fa = 'fa', show.legend=F, ylabel="Eigen Values of Factors and Components")

#### Online Appendices, Table 16. Study 1 exploratory analysis output for the mediator. ####
factor <- psych::fa(fear.reliability.data,nfactors = 2,rotate = "oblimin",fm="minres", p=.05, alpha=0.05)
print(factor$loadings, cutoff = 0.3)




