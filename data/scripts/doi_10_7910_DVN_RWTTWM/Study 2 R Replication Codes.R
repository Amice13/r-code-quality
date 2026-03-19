# Cleaning the Work Environment ####
rm(list=ls(all=T))
gc()

getwd()

library(sjPlot)
library(multcomp)
library(stargazer)

#### Loading and Merging the Datasets ####

s2 <- readRDS(file = "Study 2 Data.rds")

#### Data Preperation ####

s2$Q31_1[s2$Q31_1==99] <- NA
names(s2)[names(s2)=="Q31_1"] <- "Self_Conservatism"

s2$Q36[s2$Q36==99] <- NA
s2$Q36[s2$Q36==2] <- 0
names(s2)[names(s2)=="Q36"] <- "sex" # 1 - female, 0 - male
table(s2$sex, exclude = NULL)
s2$sex <- 1 + s2$sex*-1 # recoding # 0 - female, 1 - male
table(s2$sex, exclude = NULL) 

s2$Q38[s2$Q38==99] <- NA
names(s2)[names(s2)=="Q38"] <- "education"
table(s2$education, exclude = NULL)

s2$Q7_1[s2$Q7_1==99] <- NA
names(s2)[names(s2)=="Q7_1"] <- "self_left_right"
table(s2$self_left_right, exclude = NULL)

s2$Affiliation <- NA
s2$Affiliation[s2$Q3==99] <- 0
s2$Affiliation[s2$Q3==1] <- 2
s2$Affiliation[s2$Q3==2] <- 1
table(s2$Affiliation, exclude = NULL)

table(s2$Q4_TEXT, exclude = NULL)

table(s2$Q4, exclude = NULL)
s2$Partisanship <- NA
s2$Partisanship[s2$Q3==0 | s2$Q3==1 | is.na(s2$Q3) | s2$Q4==99 | s2$Q4_TEXT == "Cevap vermek istemiyorum" | is.na(s2$Q4)] <- "Bipartisan or Non-Responsive"
s2$Partisanship[s2$Q4==1] <- "Justice and Development Party (AKP)"
s2$Partisanship[s2$Q4==2] <- "Republican People's Party (CHP)"
s2$Partisanship[s2$Q4==3] <- "Other Parties on the Left"
s2$Partisanship[s2$Q4==4] <- "Other Parties on the Right"
s2$Partisanship[s2$Q4==5] <- "Other Parties on the Right"
s2$Partisanship[s2$Q4==6] <- "Other Parties on the Right"
s2$Partisanship[s2$Q4==7] <- "Other Parties on the Left"
s2$Partisanship[s2$Q4_TEXT == "Emek Partisi" | 
                  s2$Q4_TEXT == "ÌÐDP" | 
                  s2$Q4_TEXT == "TÌ_rkiye €¡\u0081ÙÌ¤i Partisi" | 
                  s2$Q4_TEXT == "TÌ_rkiye komÌ_nist hareket partisi" | 
                  s2$Q4_TEXT == "TÌ_rkiye komÌ_nist partisi" | 
                  s2$Q4_TEXT == "TÌ_rkiye KomÌ_nist Partisi" | 
                  s2$Q4_TEXT == "TÌ_rkiye Kominist partisi" |
                  s2$Q4_TEXT == "TÌ_rkiye komunist partisi" |
                  s2$Q4_TEXT == "Tkh" |
                  s2$Q4_TEXT == "TKP"] <- "Other Parties on the Left"
s2$Partisanship[s2$Q4_TEXT == "Liberal Demokrat Parti" | 
                  s2$Q4_TEXT == "Sa€Ùduyu"] <- "Other Parties on the Right"
table(s2$Partisanship, exclude=NULL)
s2$Partisanship <- factor(s2$Partisanship, ordered = F) 
table(s2$Partisanship, exclude=NULL)

s2$bipartisan_na <- NA
s2$bipartisan_na[s2$Partisanship=="Bipartisan or Non-Responsive"] <- 1
s2$bipartisan_na[s2$Partisanship!="Bipartisan or Non-Responsive"] <- 0
table(s2$bipartisan_na, exclude = NULL)

s2$akp <- NA
s2$akp[s2$Partisanship=="Justice and Development Party (AKP)"] <- 1
s2$akp[s2$Partisanship!="Justice and Development Party (AKP)"] <- 0
table(s2$akp, exclude = NULL)

s2$chp <- NA
s2$chp[s2$Partisanship=="Republican People's Party (CHP)"] <- 1
s2$chp[s2$Partisanship!="Republican People's Party (CHP)"] <- 0
table(s2$chp, exclude = NULL)

s2$other_right <- NA
s2$other_right[s2$Partisanship=="Other Parties on the Right"] <- 1
s2$other_right[s2$Partisanship!="Other Parties on the Right"] <- 0
table(s2$other_right, exclude = NULL)

s2$other_left <- NA
s2$other_left[s2$Partisanship=="Other Parties on the Left"] <- 1
s2$other_left[s2$Partisanship!="Other Parties on the Left"] <- 0
table(s2$other_left, exclude = NULL)

mean(s2$akp, na.rm = T)
s2$beta_akp <- (s2$akp - mean(s2$akp, na.rm = T))
table(s2$beta_akp, exclude = NULL)

mean(s2$chp, na.rm = T)
s2$beta_chp <- (s2$chp - mean(s2$chp, na.rm = T))
table(s2$beta_chp, exclude = NULL)

mean(s2$other_right, na.rm = T)
s2$beta_other_right <- (s2$other_right - mean(s2$other_right, na.rm = T))
table(s2$beta_other_right, exclude = NULL)

mean(s2$other_left, na.rm = T)
s2$beta_other_left <- (s2$other_left - mean(s2$other_left, na.rm = T))
table(s2$beta_other_left, exclude = NULL)

s2$Terror_Prime <- NA
s2$Terror_Prime[s2$Group == "Treatment 2"] <- 1
s2$Terror_Prime[s2$Group == "Control"] <- 0
table(s2$Terror_Prime, exclude = NULL)

table(s2$Q15_7, exclude = NULL)
s2$Q15_7 <- ifelse(s2$Q15_7>11, NA, s2$Q15_7)
s2$Q15_7[s2$Terror_Prime==0] <- 0

names(s2)[names(s2)=="Q15_7"] <- "terror_prime_fear"

#### Manipulation Check ####

s2$Q17_1[s2$Q17_1==99] <- NA
s2$Q17_2[s2$Q17_2==99] <- NA

s2$MC1 <- NA
s2$MC1[s2$Q17_1==6 & s2$Q17_2==8] <- 1
s2$MC1[(s2$Q17_1!=6 | s2$Q17_2!=8) & (!is.na(s2$Q17_1) & !is.na(s2$Q17_2))] <- 0
s2$MC1 <- ifelse(is.na(s2$MC1), 0, s2$MC1)
table(s2$MC1, exclude = NULL)

s2$Q18_1[s2$Q18_1==99] <- NA
s2$Q18_2[s2$Q18_2==99] <- NA

s2$MC2 <- NA
s2$MC2[s2$Q18_1==1 & s2$Q18_2==4] <- 1
s2$MC2[(s2$Q18_1!=1 | s2$Q18_2!=4) & (!is.na(s2$Q18_1) & !is.na(s2$Q18_2))] <- 0
s2$MC2 <- ifelse(is.na(s2$MC2), 0, s2$MC2)
table(s2$MC2, exclude = NULL)

s2$Q19_1[s2$Q19_1==99] <- NA
s2$Q19_2[s2$Q19_2==99] <- NA

s2$MC3 <- NA
s2$MC3[s2$Q19_1==1 & s2$Q19_2==4] <- 1
s2$MC3[(s2$Q19_1!=1 | s2$Q19_2!=4) & (!is.na(s2$Q19_1) & !is.na(s2$Q19_2))] <- 0
s2$MC3 <- ifelse(is.na(s2$MC3), 0, s2$MC3)
table(s2$MC3, exclude = NULL)

s2$Manipulation_Check <- s2$MC1 + s2$MC2+ s2$MC3
table(s2$Manipulation_Check, s2$Group, exclude = NULL)

bartlett.test(s2$Manipulation_Check ~ s2$Terror_Prime)
t.test(s2$Manipulation_Check ~ s2$Terror_Prime, var.equal=FALSE, alternative="less")

#### Manuscript, Table 3. Study 2 descriptive statistics. ####

library(table1)

my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=2), c("",
                                                           "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
}
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
                                                  sprintf("%d (%0.0f %%)", FREQ, PCT))))
}

table1(~ Self_Conservatism + terror_prime_fear + sex + age + education + self_left_right | Terror_Prime, data=s2, 
       render.continuous=my.render.cont, render.categorical=my.render.cat, overall=NULL)

treatment <- aov(Self_Conservatism ~ factor(Terror_Prime), data = s2)
summary(treatment)

sex <- aov(sex ~ factor(Terror_Prime), data = s2)
summary(sex)

age <- aov(age ~ factor(Terror_Prime), data = s2)
summary(age)

education <- aov(education ~ factor(Terror_Prime), data = s2)
summary(education)

self_left_right <- aov(self_left_right ~ factor(Terror_Prime), data = s2)
summary(self_left_right)

#### Standardization ####

s2 <- subset(s2, subset = !is.na(Self_Conservatism))

mean(s2$Terror_Prime, na.rm = T)
s2$beta_Terror_Prime <- (s2$Terror_Prime - mean(s2$Terror_Prime, na.rm = T))
table(s2$beta_Terror_Prime, exclude = NULL)

mean(s2$sex, na.rm = T)
s2$beta_sex <- (s2$sex - mean(s2$sex, na.rm = T))
table(s2$beta_sex, exclude = NULL)

mean(s2$age, na.rm = T)
sd(s2$age, na.rm = T)
s2$beta_age <- (s2$age - mean(s2$age, na.rm = T))/(2*sd(s2$age, na.rm = T))
table(s2$beta_age, exclude = NULL)

mean(s2$education, na.rm = T)
sd(s2$education, na.rm = T)
s2$beta_education <- (s2$education - mean(s2$education, na.rm = T))/(2*sd(s2$education, na.rm = T))
table(s2$beta_education, exclude = NULL)

mean(s2$self_left_right, na.rm = T)
sd(s2$self_left_right, na.rm = T)
s2$beta_self_left_right <- (s2$self_left_right - mean(s2$self_left_right, na.rm = T))/(2*sd(s2$self_left_right, na.rm = T))
table(s2$beta_self_left_right, exclude = NULL)

mean(s2$Self_Conservatism, na.rm = T)
sd(s2$Self_Conservatism, na.rm = T)
s2$beta_Self_Conservatism <- (s2$Self_Conservatism - mean(s2$Self_Conservatism, na.rm = T))/(2*sd(s2$Self_Conservatism, na.rm = T))
table(s2$beta_Self_Conservatism, exclude = NULL)

mean(s2$terror_prime_fear, na.rm = T)
sd(s2$terror_prime_fear, na.rm = T)
s2$beta_terror_prime_fear <- (s2$terror_prime_fear - mean(s2$terror_prime_fear, na.rm = T))/(2*sd(s2$terror_prime_fear, na.rm = T))
table(s2$beta_terror_prime_fear, exclude = NULL)

s2$region <- NA
s2$region[s2$State=='Istanbul'] <- "Istanbul"
s2$region[s2$State=='Tekirdag' |
            s2$State=='Edirne' |
            s2$State== 'Kirklareli' |
            s2$State== 'Balikesir' | 
            s2$State== 'Canakkale'] <- "West Marmara"
s2$region[s2$State=='Izmir' |
            s2$State== 'Aydin' | 
            s2$State==  'Denizli' |
            s2$State==  'Mugla' |
            s2$State==  'Manisa' |
            s2$State==  'Afyon' |
            s2$State== 'Kutahya' |
            s2$State== 'Usak'] <- "Aegean"
s2$region[s2$State=='Bursa' |
            s2$State== 'Eskisehir' |
            s2$State== 'Bilecik' |
            s2$State== 'Kocaeli' |
            s2$State== 'Sakarya' |
            s2$State==  'Duzce' |
            s2$State==  'Bolu' |
            s2$State== 'Yalova'] <- "East Marmara"
s2$region[s2$State=='Ankara' |
            s2$State==  'Konya' | s2$State== 'Karaman'] <- "West Anatolia"
s2$region[s2$State=='Antalya' |
            s2$State== 'Isparta' |
            s2$State==  'Adana' |
            s2$State==   'Mersin' |
            s2$State==   'Hatay' |
            s2$State==  'Kahramanmaras' |
            s2$State==   'Osmaniye' | s2$State== 'Burdur'] <- "Mediterranean"
s2$region[s2$State=='Kirikkale' |
            s2$State== 'Aksaray' |
            s2$State==  'Nigde' |
            s2$State==   'Nevsehir' |
            s2$State==   'Kirsehir' |
            s2$State==   'Kayseri' | 
            s2$State==  'Sivas' |
            s2$State==  'Yozgat'] <- "Central Anatolia"
s2$region[s2$State=='Zonguldak' |
            s2$State== 'Karabuk' |
            s2$State==  'Bartin' |
            s2$State==  'Kastamonu' |
            s2$State== 'Cankiri' |
            s2$State==  'Sinop' |
            s2$State==  'Samsun' |
            s2$State==  'Tokat' |
            s2$State==  'Corum' |
            s2$State== 'Amasya'] <- "West Black Sea"
s2$region[s2$State=='Ordu' |
            s2$State=='Trabzon' |
            s2$State=='Giresun' |
            s2$State== 'Rize' |
            s2$State== 'Artvin' |
            s2$State== 'Gumushane'] <- "East Black Sea"
s2$region[s2$State=='Erzurum' |
            s2$State=='Erzincan' |
            s2$State== 'Agri' |
            s2$State== 'Kars' | s2$State== 'Ardahan' | s2$State== 'Bayburt' | s2$State== 'Igdir'] <- "Northeast Anatolia"
s2$region[s2$State=='Malatya' |
            s2$State== 'Elazig' |
            s2$State== 'Bingol' |
            s2$State== 'Tunceli' |
            s2$State== 'Van' |
            s2$State== 'Mus' |
            s2$State== 'Bitlis' |
            s2$State== 'Hakkari'] <- "Central East Anatolia"
s2$region[s2$State=='Gaziantep' |
            s2$State== 'Adiyaman' |
            s2$State== 'Kilis' |
            s2$State== 'Sanliurfa' |
            s2$State== 'Diyarbakir' |
            s2$State== 'Mardin' |
            s2$State== 'Batman' |
            s2$State== 'Sirnak' |
            s2$State== 'Siirt'] <- "Southeast Anatolia"
table(s2$region, exclude = NULL)

s2$region <- ifelse(is.na(s2$region), "Outside Turkey", s2$region)

#### Online Appendices, Table 14. Study 2 logistic regression modeling of assignment to terrorism mortality salience. ####

library(nnet)

balance_test <-  multinom(Terror_Prime ~ beta_sex + beta_age + beta_education + beta_self_left_right + beta_akp + beta_chp + beta_other_right + beta_other_left, data=s2)
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


#### Manuscript, Table 4. Study 2 Causal mediation analysis models. ####

library(mediation)
set.seed(123)

out.fit.m01 <- lm(beta_Self_Conservatism ~ beta_Terror_Prime, data=s2)
summary(out.fit.m01)
tab_model(out.fit.m01, show.ci = 0.95)

confint(out.fit.m01, 'beta_Terror_Prime', level=0.95)

C <- diag(2)
model.mc <- glht(out.fit.m01, linfct = C)
summary(model.mc)
BH.OUT <- summary(model.mc, test = adjusted(type = "BH"))
BH.OUT$test

med.fit.m1 <- lm(beta_terror_prime_fear ~ beta_Terror_Prime, data=s2)
summary(med.fit.m1)
tab_model(med.fit.m1, show.ci = 0.95)

C <- diag(2)
model.mc <- glht(med.fit.m1, linfct = C)
summary(model.mc)
BH.OUT <- summary(model.mc, test = adjusted(type = "BH"))
BH.OUT$test

out.fit.m1 <- lm(beta_Self_Conservatism ~ beta_Terror_Prime + beta_terror_prime_fear, data=s2)
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

out.fit.m02 <- lm(beta_Self_Conservatism ~ beta_Terror_Prime + beta_sex + beta_age + beta_education + beta_self_left_right, data=s2)
summary(out.fit.m02)
tab_model(out.fit.m02, show.ci = 0.95)

confint(out.fit.m02, 'beta_Terror_Prime', level=0.95)

C <- diag(6)
model.mc <- glht(out.fit.m02, linfct = C)
summary(model.mc)
BH.OUT <- summary(model.mc, test = adjusted(type = "BH"))
BH.OUT$test

med.fit.m2 <- lm(beta_terror_prime_fear ~ beta_Terror_Prime + beta_sex + beta_age + beta_education + beta_self_left_right, data=s2)
summary(med.fit.m2)
tab_model(med.fit.m2, show.ci = 0.95)

C <- diag(6)
model.mc <- glht(med.fit.m2, linfct = C)
summary(model.mc)
BH.OUT <- summary(model.mc, test = adjusted(type = "BH"))
BH.OUT$test

out.fit.m2 <- lm(beta_Self_Conservatism ~ beta_Terror_Prime + beta_terror_prime_fear + beta_sex + beta_age + beta_education + beta_self_left_right, data=s2)
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

s2$region_partisanship <- paste(s2$region, s2$Partisanship)

out.fit.m03 <- lmer(beta_Self_Conservatism ~ beta_Terror_Prime + beta_sex + beta_age + beta_education + beta_self_left_right + (1 | region_partisanship), data=s2)
summary(out.fit.m03)
tab_model(out.fit.m03, show.ci = 0.95)

confint(out.fit.m03, 'beta_Terror_Prime', level=0.95)

C <- diag(6)
model.mc <- glht(out.fit.m02, linfct = C)
summary(model.mc)
BH.OUT <- summary(model.mc, test = adjusted(type = "BH"))
BH.OUT$test

med.fit.m3 <- lmer(beta_terror_prime_fear ~ beta_Terror_Prime + beta_sex + beta_age + beta_education + beta_self_left_right + (1 | region_partisanship), data=s2)
summary(med.fit.m3)
tab_model(med.fit.m3, show.ci = 0.95)

C <- diag(6)
model.mc <- glht(med.fit.m3, linfct = C)
summary(model.mc)
BH.OUT <- summary(model.mc, test = adjusted(type = "BH"))
BH.OUT$test

out.fit.m3 <- lmer(beta_Self_Conservatism ~ beta_Terror_Prime + beta_terror_prime_fear + beta_sex + beta_age + beta_education + beta_self_left_right + (1 | region_partisanship), data=s2)
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
          type = "html", title=" ", digits=2, out="s2_model_outputs.htm",
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

#### Online Appendices, Figure 2. Study 2 serial mediation plot. ####

s2$Q20_1[s2$Q20_1==99] <- NA
s2$Q20_2[s2$Q20_2==99] <- NA
s2$Q20_3[s2$Q20_3==99] <- NA
s2$Q20_4[s2$Q20_4==99] <- NA
s2$Q21_1[s2$Q21_1==99] <- NA
s2$Q21_2[s2$Q21_2==99] <- NA
s2$Q21_3[s2$Q21_3==99] <- NA
s2$Q21_4[s2$Q21_4==99] <- NA
s2$Q22_1[s2$Q22_1==99] <- NA
s2$Q22_2[s2$Q22_2==99] <- NA
s2$Q22_3[s2$Q22_3==99] <- NA
s2$Q22_4[s2$Q22_4==99] <- NA
s2$Q23_1[s2$Q23_1==99] <- NA
s2$Q23_2[s2$Q23_2==99] <- NA
s2$Q23_3[s2$Q23_3==99] <- NA
s2$Q23_4[s2$Q23_4==99] <- NA
s2$Q24_1[s2$Q24_1==99] <- NA
s2$Q24_2[s2$Q24_2==99] <- NA
s2$Q24_3[s2$Q24_3==99] <- NA
s2$Q24_4[s2$Q24_4==99] <- NA
s2$Q25_1[s2$Q25_1==99] <- NA
s2$Q25_2[s2$Q25_2==99] <- NA
s2$Q25_3[s2$Q25_3==99] <- NA
s2$Q25_4[s2$Q25_4==99] <- NA
s2$Q26_1[s2$Q26_1==99] <- NA
s2$Q26_2[s2$Q26_2==99] <- NA
s2$Q26_3[s2$Q26_3==99] <- NA
s2$Q26_4[s2$Q26_4==99] <- NA
s2$Q27_1[s2$Q27_1==99] <- NA
s2$Q27_2[s2$Q27_2==99] <- NA
s2$Q27_3[s2$Q27_3==99] <- NA
s2$Q27_4[s2$Q27_4==99] <- NA
s2$Q28_1[s2$Q28_1==99] <- NA
s2$Q28_2[s2$Q28_2==99] <- NA
s2$Q28_3[s2$Q28_3==99] <- NA
s2$Q28_4[s2$Q28_4==99] <- NA
s2$Q29_1[s2$Q29_1==99] <- NA
s2$Q29_2[s2$Q29_2==99] <- NA
s2$Q29_3[s2$Q29_3==99] <- NA
s2$Q29_4[s2$Q29_4==99] <- NA
s2$Q30_1[s2$Q30_1==99] <- NA
s2$Q30_2[s2$Q30_2==99] <- NA
s2$Q30_3[s2$Q30_3==99] <- NA
s2$Q30_4[s2$Q30_4==99] <- NA

names(s2)[names(s2)=="Q20_1"] <- "extraversion_1"
s2$Q21_2R <- 12 - s2$Q21_2
names(s2)[names(s2)=="Q21_2R"] <- "extraversion_2"
names(s2)[names(s2)=="Q22_3"] <- "extraversion_3"
names(s2)[names(s2)=="Q23_4"] <- "extraversion_4"
s2$Q25_1R <- 12 - s2$Q25_1
names(s2)[names(s2)=="Q25_1R"] <- "extraversion_5"
names(s2)[names(s2)=="Q26_2"] <- "extraversion_6"
s2$Q27_3R <- 12 - s2$Q27_3
names(s2)[names(s2)=="Q27_3R"] <- "extraversion_7"
names(s2)[names(s2)=="Q28_4"] <- "extraversion_8"

psych::alpha(s2[,c("extraversion_1", "extraversion_2", "extraversion_3", "extraversion_4",
                   "extraversion_5", "extraversion_6", "extraversion_7", "extraversion_8")])

s2$extraversion <- s2$extraversion_1 + 
  s2$extraversion_2 + 
  s2$extraversion_3 + 
  s2$extraversion_4 + 
  s2$extraversion_5 + 
  s2$extraversion_6 + 
  s2$extraversion_7 + 
  s2$extraversion_8

psych::describeBy(s2$extraversion, s2$Terror_Prime)

mean(s2$extraversion, na.rm = T)
sd(s2$extraversion, na.rm = T)
s2$beta_extraversion <- (s2$extraversion - mean(s2$extraversion, na.rm = T))/(2*sd(s2$extraversion, na.rm = T))
table(s2$beta_extraversion, exclude = NULL)

s2$Q20_2R <- 12 - s2$Q20_2
names(s2)[names(s2)=="Q20_2R"] <- "agreeableness_1"
names(s2)[names(s2)=="Q21_3"] <- "agreeableness_2"
s2$Q22_4R <- 12 - s2$Q22_4
names(s2)[names(s2)=="Q22_4R"] <- "agreeableness_3"
names(s2)[names(s2)=="Q24_1"] <- "agreeableness_4"
names(s2)[names(s2)=="Q25_2"] <- "agreeableness_5"
s2$Q26_3R <- 12 - s2$Q26_3
names(s2)[names(s2)=="Q26_3R"] <- "agreeableness_6"
names(s2)[names(s2)=="Q27_4"] <- "agreeableness_7"
s2$Q29_1R <- 12 - s2$Q29_1
names(s2)[names(s2)=="Q29_1R"] <- "agreeableness_8"
names(s2)[names(s2)=="Q30_2"] <- "agreeableness_9"

psych::alpha(s2[,c("agreeableness_1", "agreeableness_2", "agreeableness_3", "agreeableness_4",
                   "agreeableness_5", "agreeableness_6", "agreeableness_7", "agreeableness_8", "agreeableness_9")])

s2$agreeableness <- s2$agreeableness_1 + 
  s2$agreeableness_2 + 
  s2$agreeableness_3 + 
  s2$agreeableness_4 + 
  s2$agreeableness_5 + 
  s2$agreeableness_6 + 
  s2$agreeableness_7 + 
  s2$agreeableness_8 +
  s2$agreeableness_9

psych::describeBy(s2$agreeableness, s2$Terror_Prime)

mean(s2$agreeableness, na.rm = T)
sd(s2$agreeableness, na.rm = T)
s2$beta_agreeableness <- (s2$agreeableness - mean(s2$agreeableness, na.rm = T))/(2*sd(s2$agreeableness, na.rm = T))
table(s2$beta_agreeableness, exclude = NULL)

names(s2)[names(s2)=="Q20_3"] <- "conscientiousness_1"
s2$Q21_4R <- 12 - s2$Q21_4
names(s2)[names(s2)=="Q21_4R"] <- "conscientiousness_2"
names(s2)[names(s2)=="Q23_1"] <- "conscientiousness_3"
s2$Q24_2R <- 12 - s2$Q24_2
names(s2)[names(s2)=="Q24_2R"] <- "conscientiousness_4"
s2$Q25_3R <- 12 - s2$Q25_3
names(s2)[names(s2)=="Q25_3R"] <- "conscientiousness_5"
names(s2)[names(s2)=="Q26_4"] <- "conscientiousness_6"
names(s2)[names(s2)=="Q28_1"] <- "conscientiousness_7"
names(s2)[names(s2)=="Q29_2"] <- "conscientiousness_8"
s2$Q30_3R <- 12 - s2$Q30_3
names(s2)[names(s2)=="Q30_3R"] <- "conscientiousness_9"

psych::alpha(s2[,c("conscientiousness_1", "conscientiousness_2", "conscientiousness_3", "conscientiousness_4",
                   "conscientiousness_5", "conscientiousness_6", "conscientiousness_7", "conscientiousness_8", "conscientiousness_9")])

s2$conscientiousness <- s2$conscientiousness_1 + 
  s2$conscientiousness_2 + 
  s2$conscientiousness_3 + 
  s2$conscientiousness_4 + 
  s2$conscientiousness_5 + 
  s2$conscientiousness_6 + 
  s2$conscientiousness_7 + 
  s2$conscientiousness_8 +
  s2$conscientiousness_9

psych::describeBy(s2$conscientiousness, s2$Terror_Prime)

mean(s2$conscientiousness, na.rm = T)
sd(s2$conscientiousness, na.rm = T)
s2$beta_conscientiousness <- (s2$conscientiousness - mean(s2$conscientiousness, na.rm = T))/(2*sd(s2$conscientiousness, na.rm = T))
table(s2$beta_conscientiousness, exclude = NULL)

names(s2)[names(s2)=="Q20_4"] <- "neuroticism_1"
s2$Q22_1R <- 12 - s2$Q22_1
names(s2)[names(s2)=="Q22_1R"] <- "neuroticism_2"
names(s2)[names(s2)=="Q23_2"] <- "neuroticism_3"
names(s2)[names(s2)=="Q24_3"] <- "neuroticism_4"
s2$Q25_4R <- 12 - s2$Q25_4
names(s2)[names(s2)=="Q25_4R"] <- "neuroticism_5"
names(s2)[names(s2)=="Q27_1"] <- "neuroticism_6"
s2$Q28_2R <- 12 - s2$Q28_2
names(s2)[names(s2)=="Q28_2R"] <- "neuroticism_7"
names(s2)[names(s2)=="Q29_3"] <- "neuroticism_8"

psych::alpha(s2[,c("neuroticism_1", "neuroticism_2", "neuroticism_3", "neuroticism_4",
                   "neuroticism_5", "neuroticism_6", "neuroticism_7", "neuroticism_8")])

s2$neuroticism <- s2$neuroticism_1 + 
  s2$neuroticism_2 + 
  s2$neuroticism_3 + 
  s2$neuroticism_4 + 
  s2$neuroticism_5 + 
  s2$neuroticism_6 + 
  s2$neuroticism_7 + 
  s2$neuroticism_8

psych::describeBy(s2$neuroticism, s2$Terror_Prime)

mean(s2$neuroticism, na.rm = T)
sd(s2$neuroticism, na.rm = T)
s2$beta_neuroticism <- (s2$neuroticism - mean(s2$neuroticism, na.rm = T))/(2*sd(s2$neuroticism, na.rm = T))
table(s2$beta_neuroticism, exclude = NULL)

names(s2)[names(s2)=="Q21_1"] <- "openness_1"
names(s2)[names(s2)=="Q22_2"] <- "openness_2"
names(s2)[names(s2)=="Q23_3"] <- "openness_3"
names(s2)[names(s2)=="Q24_4"] <- "openness_4"
names(s2)[names(s2)=="Q26_1"] <- "openness_5"
names(s2)[names(s2)=="Q27_2"] <- "openness_6"
s2$Q28_3R <- 12 - s2$Q28_3
names(s2)[names(s2)=="Q28_3R"] <- "openness_7"
names(s2)[names(s2)=="Q29_4"] <- "openness_8"
s2$Q30_1R <- 12 - s2$Q30_1
names(s2)[names(s2)=="Q30_1R"] <- "openness_9"
names(s2)[names(s2)=="Q30_4"] <- "openness_10"

psych::alpha(s2[,c("openness_1", "openness_2", "openness_3", "openness_4",
                   "openness_5", "openness_6", "openness_7", "openness_8", "openness_9", "openness_10")])

s2$openness <- s2$openness_1 + 
  s2$openness_2 + 
  s2$openness_3 + 
  s2$openness_4 + 
  s2$openness_5 + 
  s2$openness_6 + 
  s2$openness_7 + 
  s2$openness_8 +
  s2$openness_9 +
  s2$openness_10

psych::describeBy(s2$openness, s2$Terror_Prime)

mean(s2$openness, na.rm = T)
sd(s2$openness, na.rm = T)
s2$beta_openness <- (s2$openness - mean(s2$openness, na.rm = T))/(2*sd(s2$openness, na.rm = T))
table(s2$beta_openness, exclude = NULL)

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
  beta_Self_Conservatism ~ po*beta_openness + pc*beta_conscientiousness + pe*beta_extraversion + pa*beta_agreeableness + pn*beta_neuroticism + f*beta_terror_prime_fear + d*beta_Terror_Prime + beta_sex + beta_age + beta_education + beta_self_left_right
  
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
  ie_trt_f := t_f*f
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

fit <-  sem(model, data=s2, se = "bootstrap", bootstrap = 10000)
summary(fit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
parameterEstimates(fit, rsquare=T, output = "text", level = 0.95)

psych::describe(s2$beta_Terror_Prime)
psych::describe(s2$beta_terror_prime_fear)
psych::describe(s2$beta_conscientiousness)
psych::describe(s2$beta_Self_Conservatism)



