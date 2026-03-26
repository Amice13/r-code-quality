# Cleaning the Work Environment ####
rm(list=ls(all=T))
gc()

getwd()

#### Loading the Packages ####

library(sjPlot)
library(multcomp)
library(stargazer)

#### Loading and Merging the Datasets ####

s3 <- readRDS(file = "Study 3 Data.rds")

#### Data Preparation ####

table(s3$libcon_self, exclude = NULL)
names(s3)[names(s3)=="libcon_self"] <- "Self_Conservatism"

table(s3$gender, exclude = NULL)
names(s3)[names(s3)=="gender"] <- "sex" # 1 - female, 0 - male
table(s3$sex, exclude = NULL)

table(s3$education, exclude = NULL)

table(s3$lr_self, exclude = NULL)
names(s3)[names(s3)=="lr_self"] <- "self_left_right"
table(s3$self_left_right, exclude = NULL)

table(s3$pid1, exclude = NULL)
names(s3)[names(s3)=="pid1"] <- "Affiliation"
table(s3$Affiliation, exclude = NULL)

table(s3$pid2_7_TEXT, exclude = NULL)

table(s3$pid2, exclude = NULL)
s3$Partisanship <- NA
s3$Partisanship[s3$pid2==1] <- "Justice and Development Party (AKP)"
s3$Partisanship[s3$pid2==2] <- "Republican People's Party (CHP)"
s3$Partisanship[s3$pid2==3] <- "Other Parties on the Left"
s3$Partisanship[s3$pid2==4] <- "Other Parties on the Right"
s3$Partisanship[s3$pid2==5] <- "Other Parties on the Right"
s3$Partisanship[s3$pid2==6] <- "Other Parties on the Right"
s3$Partisanship[s3$pid2_7_TEXT == "Emek partisi" | 
                  s3$pid2_7_TEXT == "EMEP (Emeğin Partisi)" | 
                  s3$pid2_7_TEXT == "Hdp" |
                  s3$pid2_7_TEXT == "Komunist Parti" |
                  s3$pid2_7_TEXT == "Sol Parti" | 
                  s3$pid2_7_TEXT == "Sosyalist pati" |
                  s3$pid2_7_TEXT == "TİP" |
                  s3$pid2_7_TEXT == "Tkp" |
                  s3$pid2_7_TEXT == "Türkiye İşçi Partisi" |
                  s3$pid2_7_TEXT == "vatan partisi" |
                  s3$pid2_7_TEXT == "VATAN PARTİSİ" |
                  s3$pid2_7_TEXT == "VATAN PARTİSİ"] <- "Other Parties on the Left"
s3$Partisanship[s3$pid2_7_TEXT == "Deva partisi" | 
                  s3$pid2_7_TEXT == "Gelecek PARTİSİ"] <- "Other Parties on the Right"
s3$Partisanship <- ifelse(is.na(s3$Partisanship), "Bipartisan or Non-Responsive", s3$Partisanship)
table(s3$Partisanship, exclude=NULL)
s3$Partisanship <- factor(s3$Partisanship, levels=c("Republican People's Party (CHP)", "Justice and Development Party (AKP)",
                                                    "Other Parties on the Right", "Other Parties on the Left", "Bipartisan or Non-Responsive"), ordered = F) 
table(s3$Partisanship, exclude=NULL)

s3$bipartisan_na <- NA
s3$bipartisan_na[s3$Partisanship=="Bipartisan or Non-Responsive"] <- 1
s3$bipartisan_na[s3$Partisanship!="Bipartisan or Non-Responsive"] <- 0
table(s3$bipartisan_na, exclude = NULL)

s3$akp <- NA
s3$akp[s3$Partisanship=="Justice and Development Party (AKP)"] <- 1
s3$akp[s3$Partisanship!="Justice and Development Party (AKP)"] <- 0
table(s3$akp, exclude = NULL)

s3$chp <- NA
s3$chp[s3$Partisanship=="Republican People's Party (CHP)"] <- 1
s3$chp[s3$Partisanship!="Republican People's Party (CHP)"] <- 0
table(s3$chp, exclude = NULL)

s3$other_right <- NA
s3$other_right[s3$Partisanship=="Other Parties on the Right"] <- 1
s3$other_right[s3$Partisanship!="Other Parties on the Right"] <- 0
table(s3$other_right, exclude = NULL)

s3$other_left <- NA
s3$other_left[s3$Partisanship=="Other Parties on the Left"] <- 1
s3$other_left[s3$Partisanship!="Other Parties on the Left"] <- 0
table(s3$other_left, exclude = NULL)

mean(s3$bipartisan_na, na.rm = T)
s3$beta_bipartisan_na <- (s3$bipartisan_na - mean(s3$bipartisan_na, na.rm = T))
table(s3$beta_bipartisan_na, exclude = NULL)

mean(s3$akp, na.rm = T)
s3$beta_akp <- (s3$akp - mean(s3$akp, na.rm = T))
table(s3$beta_akp, exclude = NULL)

mean(s3$chp, na.rm = T)
s3$beta_chp <- (s3$chp - mean(s3$chp, na.rm = T))
table(s3$beta_chp, exclude = NULL)

mean(s3$other_right, na.rm = T)
s3$beta_other_right <- (s3$other_right - mean(s3$other_right, na.rm = T))
table(s3$beta_other_right, exclude = NULL)

mean(s3$other_left, na.rm = T)
s3$beta_other_left <- (s3$other_left - mean(s3$other_left, na.rm = T))
table(s3$beta_other_left, exclude = NULL)

table(s3$GROUP, exclude = NULL)
s3$Terror_Prime <- NA
s3$Terror_Prime[s3$GROUP == "Treatment 3 - Terrorist Attack"] <- 1
s3$Terror_Prime[s3$GROUP == "Control"] <- 0
table(s3$Terror_Prime, exclude = NULL)

table(s3$treatment_3_1, exclude = NULL)
s3$treatment_3_1 <- 8 - s3$treatment_3_1
s3$treatment_3_1 <- ifelse(s3$treatment_3_1>7, NA, s3$treatment_3_1)
s3$treatment_3_1[s3$Terror_Prime==0] <- 0

names(s3)[names(s3)=="treatment_3_1"] <- "terror_prime_fear"
table(s3$terror_prime_fear, exclude = NULL)


#### Manipulation Check ####

table(s3$mc_1, exclude = NULL)
table(s3$mc_2, exclude = NULL)

s3$Manipulation_Check <- NA
s3$Manipulation_Check[s3$mc_1==6 & s3$mc_2==8] <- 1
s3$Manipulation_Check <- ifelse(is.na(s3$Manipulation_Check), 0, s3$Manipulation_Check)
table(s3$Manipulation_Check, exclude = NULL)

bartlett.test(s3$Manipulation_Check ~ s3$Terror_Prime)
t.test(s3$Manipulation_Check ~ s3$Terror_Prime, var.equal=T, alternative = "less")

table(s3$`timer_lr_placements_Page Submit`, exclude=NULL)

table(s3$`timer_treatment_3_Page Submit`, exclude=NULL)

s3$timer_threats <- s3$`timer_treatment_3_Page Submit`

table(s3$timer_threats, exclude = NULL)

table(s3$`timer_big5op_b1_Page Submit`, exclude=NULL)

s3$timer_pre_during_post_threats <- rowSums(s3[, c("timer_lr_placements_Page Submit",
                                                   "timer_threats",
                                                   "timer_big5op_b1_Page Submit")], na.rm = T)

table(s3$timer_pre_during_post_threats, exclude=NULL)

table(s3$`timer_gender_Page Submit`, exclude=NULL)
table(s3$`timer_age_Page Submit`, exclude=NULL)
table(s3$`timer_education_Page Submit`, exclude=NULL)
table(s3$`timer_income_Page Submit`, exclude=NULL)
table(s3$`timer_pid1_Page Submit`, exclude=NULL)

s3$timer_all <- rowSums(s3[, c("timer_gender_Page Submit",
                               "timer_age_Page Submit",
                               "timer_education_Page Submit",
                               "timer_income_Page Submit",
                               "timer_pid1_Page Submit",
                               "timer_pre_during_post_threats")], na.rm = T)

table(s3$timer_all, exclude = NULL)

s3$attention_bias <- (s3$timer_pre_during_post_threats/s3$timer_all)*100
table(s3$attention_bias, exclude = NULL)

bartlett.test(s3$attention_bias ~ s3$Terror_Prime)
t.test(s3$attention_bias ~ s3$Terror_Prime, var.equal=T, alternative="less")

#### Manuscript, Table 5. Study 3 descriptive statistics. ####

library(table1)

my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=2), c("",
                                                           "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
}
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
                                                  sprintf("%d (%0.0f %%)", FREQ, PCT))))
}

table1(~ Self_Conservatism + terror_prime_fear + sex + age + education + self_left_right | Terror_Prime, data=s3, 
       render.continuous=my.render.cont, render.categorical=my.render.cat, overall=NULL)

treatment <- aov(Self_Conservatism ~ factor(Terror_Prime), data = s3)
summary(treatment)

sex <- aov(sex ~ factor(Terror_Prime), data = s3)
summary(sex)

age <- aov(age ~ factor(Terror_Prime), data = s3)
summary(age)

education <- aov(education ~ factor(Terror_Prime), data = s3)
summary(education)

self_left_right <- aov(self_left_right ~ factor(Terror_Prime), data = s3)
summary(self_left_right)

#### Standardization ####

s3 <- subset(s3, subset = !is.na(Self_Conservatism))

mean(s3$Terror_Prime, na.rm = T)
s3$beta_Terror_Prime <- (s3$Terror_Prime - mean(s3$Terror_Prime, na.rm = T))
table(s3$beta_Terror_Prime, exclude = NULL)

mean(s3$sex, na.rm = T)
s3$beta_sex <- (s3$sex - mean(s3$sex, na.rm = T))
table(s3$beta_sex, exclude = NULL)

mean(s3$age, na.rm = T)
sd(s3$age, na.rm = T)
s3$beta_age <- (s3$age - mean(s3$age, na.rm = T))/(2*sd(s3$age, na.rm = T))
table(s3$beta_age, exclude = NULL)

mean(s3$education, na.rm = T)
sd(s3$education, na.rm = T)
s3$beta_education <- (s3$education - mean(s3$education, na.rm = T))/(2*sd(s3$education, na.rm = T))
table(s3$beta_education, exclude = NULL)

mean(s3$self_left_right, na.rm = T)
sd(s3$self_left_right, na.rm = T)
s3$beta_self_left_right <- (s3$self_left_right - mean(s3$self_left_right, na.rm = T))/(2*sd(s3$self_left_right, na.rm = T))
table(s3$beta_self_left_right, exclude = NULL)

mean(s3$Self_Conservatism, na.rm = T)
sd(s3$Self_Conservatism, na.rm = T)
s3$beta_Self_Conservatism <- (s3$Self_Conservatism - mean(s3$Self_Conservatism, na.rm = T))/(2*sd(s3$Self_Conservatism, na.rm = T))
table(s3$beta_Self_Conservatism, exclude = NULL)

mean(s3$terror_prime_fear, na.rm = T)
sd(s3$terror_prime_fear, na.rm = T)
s3$beta_terror_prime_fear <- (s3$terror_prime_fear - mean(s3$terror_prime_fear, na.rm = T))/(2*sd(s3$terror_prime_fear, na.rm = T))
table(s3$beta_terror_prime_fear, exclude = NULL)

s3$region <- NA
s3$region[s3$State=='Istanbul'] <- "Istanbul"
s3$region[s3$State=='Tekirdag' |
            s3$State=='Edirne' |
            s3$State== 'Kirklareli' |
            s3$State== 'Balikesir' | 
            s3$State== 'Canakkale'] <- "West Marmara"
s3$region[s3$State=='Izmir' |
            s3$State== 'Aydin' | 
            s3$State==  'Denizli' |
            s3$State==  'Mugla' |
            s3$State==  'Manisa' |
            s3$State==  'Afyon' |
            s3$State== 'Kutahya' |
            s3$State== 'Usak'] <- "Aegean"
s3$region[s3$State=='Bursa' |
            s3$State== 'Eskisehir' |
            s3$State== 'Bilecik' |
            s3$State== 'Kocaeli' |
            s3$State== 'Sakarya' |
            s3$State==  'Duzce' |
            s3$State==  'Bolu' |
            s3$State== 'Yalova'] <- "East Marmara"
s3$region[s3$State=='Ankara' |
            s3$State==  'Konya' | s3$State== 'Karaman'] <- "West Anatolia"
s3$region[s3$State=='Antalya' |
            s3$State== 'Isparta' |
            s3$State==  'Adana' |
            s3$State==   'Mersin' |
            s3$State==   'Hatay' |
            s3$State==  'Kahramanmaras' |
            s3$State==   'Osmaniye' | s3$State== 'Burdur'] <- "Mediterranean"
s3$region[s3$State=='Kirikkale' |
            s3$State== 'Aksaray' |
            s3$State==  'Nigde' |
            s3$State==   'Nevsehir' |
            s3$State==   'Kirsehir' |
            s3$State==   'Kayseri' | 
            s3$State==  'Sivas' |
            s3$State==  'Yozgat'] <- "Central Anatolia"
s3$region[s3$State=='Zonguldak' |
            s3$State== 'Karabuk' |
            s3$State==  'Bartin' |
            s3$State==  'Kastamonu' |
            s3$State== 'Cankiri' |
            s3$State==  'Sinop' |
            s3$State==  'Samsun' |
            s3$State==  'Tokat' |
            s3$State==  'Corum' |
            s3$State== 'Amasya'] <- "West Black Sea"
s3$region[s3$State=='Ordu' |
            s3$State=='Trabzon' |
            s3$State=='Giresun' |
            s3$State== 'Rize' |
            s3$State== 'Artvin' |
            s3$State== 'Gumushane'] <- "East Black Sea"
s3$region[s3$State=='Erzurum' |
            s3$State=='Erzincan' |
            s3$State== 'Agri' |
            s3$State== 'Kars' | s3$State== 'Ardahan' | s3$State== 'Bayburt' | s3$State== 'Igdir'] <- "Northeast Anatolia"
s3$region[s3$State=='Malatya' |
            s3$State== 'Elazig' |
            s3$State== 'Bingol' |
            s3$State== 'Tunceli' |
            s3$State== 'Van' |
            s3$State== 'Mus' |
            s3$State== 'Bitlis' |
            s3$State== 'Hakkari'] <- "Central East Anatolia"
s3$region[s3$State=='Gaziantep' |
            s3$State== 'Adiyaman' |
            s3$State== 'Kilis' |
            s3$State== 'Sanliurfa' |
            s3$State== 'Diyarbakir' |
            s3$State== 'Mardin' |
            s3$State== 'Batman' |
            s3$State== 'Sirnak' |
            s3$State== 'Siirt'] <- "Southeast Anatolia"
table(s3$region, exclude = NULL)

s3$region <- ifelse(is.na(s3$region), "Outside Turkey", s3$region)

#### Online Appendices, Table 15. Study 3 logistic regression modeling of assignment to terrorism mortality salience. ####

library(nnet)

balance_test <-  multinom(Terror_Prime ~ beta_sex + beta_age + beta_education + beta_self_left_right + beta_akp + beta_chp + beta_other_right + beta_other_left, data=s3)
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


#### Manuscript, Table 6. Study 3 Causal mediation analysis models. ####

library(mediation)
set.seed(123)

out.fit.m01 <- lm(beta_Self_Conservatism ~ beta_Terror_Prime, data=s3)
summary(out.fit.m01)
tab_model(out.fit.m01, show.ci = 0.95)

confint(out.fit.m01, 'beta_Terror_Prime', level=0.95)

C <- diag(2)
model.mc <- glht(out.fit.m01, linfct = C)
summary(model.mc)
BH.OUT <- summary(model.mc, test = adjusted(type = "BH"))
BH.OUT$test

med.fit.m1 <- lm(beta_terror_prime_fear ~ beta_Terror_Prime, data=s3)
summary(med.fit.m1)
tab_model(med.fit.m1, show.ci = 0.95)

C <- diag(2)
model.mc <- glht(med.fit.m1, linfct = C)
summary(model.mc)
BH.OUT <- summary(model.mc, test = adjusted(type = "BH"))
BH.OUT$test

out.fit.m1 <- lm(beta_Self_Conservatism ~ beta_Terror_Prime + beta_terror_prime_fear, data=s3)
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

out.fit.m02 <- lm(beta_Self_Conservatism ~ beta_Terror_Prime + beta_sex + beta_age + beta_education + beta_self_left_right, data=s3)
summary(out.fit.m02)
tab_model(out.fit.m02, show.ci = 0.95)

confint(out.fit.m02, 'beta_Terror_Prime', level=0.95)

C <- diag(6)
model.mc <- glht(out.fit.m02, linfct = C)
summary(model.mc)
BH.OUT <- summary(model.mc, test = adjusted(type = "BH"))
BH.OUT$test

med.fit.m2 <- lm(beta_terror_prime_fear ~ beta_Terror_Prime + beta_sex + beta_age + beta_education + beta_self_left_right, data=s3)
summary(med.fit.m2)
tab_model(med.fit.m2, show.ci = 0.95)

C <- diag(6)
model.mc <- glht(med.fit.m2, linfct = C)
summary(model.mc)
BH.OUT <- summary(model.mc, test = adjusted(type = "BH"))
BH.OUT$test

out.fit.m2 <- lm(beta_Self_Conservatism ~ beta_Terror_Prime + beta_terror_prime_fear + beta_sex + beta_age + beta_education + beta_self_left_right, data=s3)
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

s3$region_partisanship <- paste(s3$region, s3$Partisanship)

out.fit.m03 <- lmer(beta_Self_Conservatism ~ beta_Terror_Prime + beta_sex + beta_age + beta_education + beta_self_left_right + (1 | region_partisanship), data=s3)
summary(out.fit.m03)
tab_model(out.fit.m03, show.ci = 0.95)

confint(out.fit.m03, 'beta_Terror_Prime', level=0.95)

C <- diag(6)
model.mc <- glht(out.fit.m02, linfct = C)
summary(model.mc)
BH.OUT <- summary(model.mc, test = adjusted(type = "BH"))
BH.OUT$test

med.fit.m3 <- lmer(beta_terror_prime_fear ~ beta_Terror_Prime + beta_sex + beta_age + beta_education + beta_self_left_right + (1 | region_partisanship), data=s3)
summary(med.fit.m3)
tab_model(med.fit.m3, show.ci = 0.95)

C <- diag(6)
model.mc <- glht(med.fit.m3, linfct = C)
summary(model.mc)
BH.OUT <- summary(model.mc, test = adjusted(type = "BH"))
BH.OUT$test

out.fit.m3 <- lmer(beta_Self_Conservatism ~ beta_Terror_Prime + beta_terror_prime_fear + beta_sex + beta_age + beta_education + beta_self_left_right + (1 | region_partisanship), data=s3)
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
          type = "html", title=" ", digits=2, out="s3_model_outputs.htm",
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

#### Online Appendices, Figure 3. Study 3 serial mediation plot. ####

table(s3$big5op_7, exclude = NULL)

names(s3)[names(s3)=="big5op_1"] <- "openness_1"
names(s3)[names(s3)=="big5op_2"] <- "openness_2"
names(s3)[names(s3)=="big5op_3"] <- "openness_3"
names(s3)[names(s3)=="big5op_4"] <- "openness_4"
names(s3)[names(s3)=="big5op_5"] <- "openness_5"
names(s3)[names(s3)=="big5op_6"] <- "openness_6"
s3$big5op_7 <- 8 - s3$big5op_7
names(s3)[names(s3)=="big5op_7"] <- "openness_7"
names(s3)[names(s3)=="big5op_8"] <- "openness_8"
s3$big5op_9 <- 8 - s3$big5op_9
names(s3)[names(s3)=="big5op_9"] <- "openness_9"
names(s3)[names(s3)=="big5op_10"] <- "openness_10"

psych::alpha(s3[,c("openness_1", "openness_2", "openness_3", "openness_4",
                   "openness_5", "openness_6", "openness_7", "openness_8", "openness_9", "openness_10")])

s3$openness <- s3$openness_1 + 
  s3$openness_2 + 
  s3$openness_3 + 
  s3$openness_4 + 
  s3$openness_5 + 
  s3$openness_6 + 
  s3$openness_7 + 
  s3$openness_8 +
  s3$openness_9 +
  s3$openness_10

psych::describeBy(s3$openness, s3$Terror_Prime)

mean(s3$openness, na.rm = T)
sd(s3$openness, na.rm = T)
s3$beta_openness <- (s3$openness - mean(s3$openness, na.rm = T))/(2*sd(s3$openness, na.rm = T))
table(s3$beta_openness, exclude = NULL)

library(lavaan)

model=
  "
  #Regressions
  beta_terror_prime_fear ~ t_f*beta_Terror_Prime + beta_sex + beta_age + beta_education + beta_self_left_right
  beta_openness ~ f_po*beta_terror_prime_fear + t_po*beta_Terror_Prime + beta_sex + beta_age + beta_education + beta_self_left_right
  beta_Self_Conservatism ~ po*beta_openness + f*beta_terror_prime_fear + d*beta_Terror_Prime + beta_sex + beta_age + beta_education + beta_self_left_right
  
  #Defined Parameters:
  ie_all_o := t_f*f_po*po
  ie_short_o := t_po*po
  ie_trt_f := t_f*f
  de := d

"

fit <-  sem(model, data=s3, se = "bootstrap", bootstrap = 10000)
summary(fit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
parameterEstimates(fit, rsquare=T, output = "text", level = 0.95)
