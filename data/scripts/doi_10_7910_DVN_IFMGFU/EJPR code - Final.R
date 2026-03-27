library(readxl)
library(plyr) # mapvalues()
library(see)
library(tidyverse)
library(modelsummary)
library(marginaleffects)
library(descr) # freq()
library(Hmisc) # correlation matrix
library(DescTools) # statistic tests
library(gridExtra) # combine graphs
library(lemon) # put legend in combined graphs
library(psych) # KMO()
library(ggeffects)
library(broom)

dataset0 <- read_excel("dataset.xlsx")

#################
### VARIABLES ###
#################

# Age

dataset1 <- subset(dataset0, dataset0$age <= 35)

dataset1$age <- as.numeric(dataset1$age)

# Gender

dataset2 <- subset(dataset1, dataset1$gender != "Other")
dataset2[dataset2 == "Man"] <- "Men"
dataset2[dataset2 == "Woman"] <- "Women"

dataset2 <- dataset2 %>%
  mutate(Gender = ifelse(gender == "Women", 1, 0))

# Education

dataset2$education <- mapvalues(dataset2$educode,
                                from = c("Primary", "Secondary", "Tertiary"),
                                to = c("Up to secondary", "Up to secondary", "Tertiary"))

dataset2 <- dataset2 %>%
  mutate(Education = ifelse(education == "Tertiary", 1, 0))

# Family background

dataset2 <- dataset2 %>%
  mutate(Familybackground = ifelse(familypartymember == "Yes", 1, 0))

# Party ideology

dataset2[dataset2 == "ALP"] <- "AYL"
dataset2[dataset2 == "FI"] <- "FIG"
dataset2[dataset2 == "Lib"] <- "YL"

dataset2$ideology <- mapvalues(dataset2$youthwing,
                                from = c("AYL", "GD", "JSE", "Jusos", "SJOE",
                                         "FIG", "JU", "JVP", "NNGG", "YL"),
                                to = c("Center-left", "Center-left", "Center-left",
                                       "Center-left", "Center-left",
                                       "Center-right", "Center-right", "Center-right",
                                       "Center-right", "Center-right"))

dataset2$ideology_num <- mapvalues(dataset2$ideology,
                                     from = c("Center-left", "Center-right"),
                                     to = c(0, 1))

dataset2$ideology_num <- as.numeric(dataset2$ideology_num)

# Youth wing

dataset2$youthwing_num <- mapvalues(dataset2$youthwing,
                                    from = c("AYL", "GD", "JSE", "Jusos", "SJOE",
                                             "FIG", "JU", "JVP", "NNGG", "YL"),
                                    to = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

# Demand-side factors

dataset2$womenMPs <- mapvalues(dataset2$youthwing,
                               from = c("AYL", "GD", "JSE", "Jusos", "SJOE",
                                        "FIG", "JU", "JVP", "NNGG", "YL"),
                               to = c(0.41, 0.33, 0.50, 0.42, 0.48,
                                      0.36, 0.23, 0.37, 0.43, 0.12))

dataset2$womenMPs <- as.numeric(dataset2$womenMPs)

dataset2$womanpleader <- mapvalues(dataset2$youthwing,
                                   from = c("AYL", "GD", "JSE", "Jusos", "SJOE",
                                            "FIG", "JU", "JVP", "NNGG", "YL"),
                                   to = c(0, 0, 0, 1, 1,
                                          0, 0, 0, 0, 0))

dataset2$womanywleader <- mapvalues(dataset2$youthwing,
                                    from = c("AYL", "GD", "JSE", "Jusos", "SJOE",
                                             "FIG", "JU", "JVP", "NNGG", "YL"),
                                    to = c(0, 0, 0, 1, 0,
                                           0, 0, 1, 0, 0))

dataset2$autonomousYW <- mapvalues(dataset2$youthwing,
                                    from = c("AYL", "GD", "JSE", "Jusos", "SJOE",
                                             "FIG", "JU", "JVP", "NNGG", "YL"),
                                    to = c(0, 0, 1, 1, 1,
                                           0, 1, 1, 0, 0))

# Country

dataset2$country_num <- mapvalues(dataset2$country,
                                  from = c("Australia",
                                           "Austria",
                                           "Germany",
                                           "Italy",
                                           "Spain"),
                                  to = c(1, 2, 3, 4, 5))

dataset2$country_num <- as.numeric(dataset2$country_num)


### Reasons for joining ###

dataset2$Peoplesharingvalues <- mapvalues(dataset2$peoplesharingvalues,
                                           from = c("Agree", "Disagree", "Don't know", "Strongly agree", "Strongly disagree"),
                                           to = c(4, 2, 3, 5, 1))
freq(dataset2$Peoplesharingvalues)
dataset2$Peoplesharingvalues <- as.numeric(dataset2$Peoplesharingvalues)

dataset2$Workelecampaign <- mapvalues(dataset2$workelecampaign,
                                       from = c("Agree", "Disagree", "Don't know", "Strongly agree", "Strongly disagree"),
                                       to = c(4, 2, 3, 5, 1))
freq(dataset2$Workelecampaign)

dataset2$Workelecampaign <- as.numeric(dataset2$Workelecampaign)
dataset2$Friendmembers <- mapvalues(dataset2$friendmembers,
                                     from = c("Agree", "Disagree", "Don't know", "Strongly agree", "Strongly disagree"),
                                     to = c(4, 2, 3, 5, 1))
freq(dataset2$Friendmembers)

dataset2$Friendmembers <- as.numeric(dataset2$Friendmembers)
dataset2$Influencepolicyjoin <- mapvalues(dataset2$influencepolicyjoin,
                                           from = c("Agree", "Disagree", "Don't know", "Strongly agree", "Strongly disagree"),
                                           to = c(4, 2, 3, 5, 1))
freq(dataset2$Influencepolicyjoin)

dataset2$Influencepolicyjoin <- as.numeric(dataset2$Influencepolicyjoin)
dataset2$Familymembers <- mapvalues(dataset2$familymembers,
                                     from = c("Agree", "Disagree", "Don't know", "Strongly agree", "Strongly disagree"),
                                     to = c(4, 2, 3, 5, 1))
freq(dataset2$Familymembers)

dataset2$Familymembers <- as.numeric(dataset2$Familymembers)
dataset2$Identificationplatform <- mapvalues(dataset2$identificationplatform,
                                              from = c("Agree", "Disagree", "Don't know", "Strongly agree", "Strongly disagree"),
                                              to = c(4, 2, 3, 5, 1))
freq(dataset2$Identificationplatform)

dataset2$Identificationplatform <- as.numeric(dataset2$Identificationplatform)
dataset2$Networking <- mapvalues(dataset2$networking,
                                  from = c("Agree", "Disagree", "Don't know", "Strongly agree", "Strongly disagree"),
                                  to = c(4, 2, 3, 5, 1))
freq(dataset2$Networking)
dataset2$Networking <- as.numeric(dataset2$Networking)

dataset2$Futurecandidate <- mapvalues(dataset2$futurecandidate,
                                       from = c("Agree", "Disagree", "Don't know", "Strongly agree", "Strongly disagree"),
                                       to = c(4, 2, 3, 5, 1))
freq(dataset2$Futurecandidate)
dataset2$Futurecandidate <- as.numeric(dataset2$Futurecandidate)


KMO(dataset2[c("Peoplesharingvalues", "Workelecampaign", "Friendmembers", "Influencepolicyjoin",
                "Familymembers", "Identificationplatform", "Networking", "Futurecandidate")])

fa.parallel(dataset2[c("Peoplesharingvalues", "Workelecampaign", "Friendmembers", "Influencepolicyjoin",
                        "Familymembers", "Identificationplatform", "Networking", "Futurecandidate")])

fa <- principal(dataset2[c("Peoplesharingvalues", "Workelecampaign", "Friendmembers", "Influencepolicyjoin",
                            "Familymembers", "Identificationplatform", "Networking", "Futurecandidate")],
                nfactors = 3, rotate = "varimax")
fa

dataset2 <- cbind(dataset2, fa$scores)
rm(fa)
psych::alpha(dataset2[c("Workelecampaign", "Influencepolicyjoin", "Identificationplatform")], check.keys = T)
psych::alpha(dataset2[c("Networking", "Futurecandidate")], check.keys = T)
psych::alpha(dataset2[c("Peoplesharingvalues", "Friendmembers", "Familymembers")], check.keys = T)

dataset2$purposive <- dataset2$RC1
dataset2$material <- dataset2$RC3
dataset2$social <- dataset2$RC2

hist(dataset2$purposive)
hist(dataset2$material)
hist(dataset2$social)

dataset2$purposive <- (dataset2$purposive - min(dataset2$purposive, na.rm = TRUE)) / 
  (max(dataset2$purposive, na.rm = TRUE) - min(dataset2$purposive, na.rm = TRUE))

dataset2$material <- (dataset2$material - min(dataset2$material, na.rm = TRUE)) / 
  (max(dataset2$material, na.rm = TRUE) - min(dataset2$material, na.rm = TRUE))

dataset2$social <- (dataset2$social - min(dataset2$social, na.rm = TRUE)) / 
  (max(dataset2$social, na.rm = TRUE) - min(dataset2$social, na.rm = TRUE))


### Reasons for joining - dummies ###

# Purposive

dataset2$Workelecampaign_dummy <- mapvalues(dataset2$workelecampaign,
                                            from = c("Agree", "Disagree", "Don't know",
                                                     "Strongly agree", "Strongly disagree"),
                                            to = c(1, 0, 0, 1, 0))

dataset2$Influencepolicyjoin_dummy <- mapvalues(dataset2$influencepolicyjoin,
                                                from = c("Agree", "Disagree", "Don't know",
                                                         "Strongly agree", "Strongly disagree"),
                                                to = c(1, 0, 0, 1, 0))

dataset2$Identificationplatform_dummy <- mapvalues(dataset2$identificationplatform,
                                                   from = c("Agree", "Disagree", "Don't know",
                                                            "Strongly agree", "Strongly disagree"),
                                                   to = c(1, 0, 0, 1, 0))

# Social

dataset2$Peoplesharingvalues_dummy <- mapvalues(dataset2$peoplesharingvalues,
                                                from = c("Agree", "Disagree", "Don't know",
                                                         "Strongly agree", "Strongly disagree"),
                                                to = c(1, 0, 0, 1, 0))

dataset2$Friendmembers_dummy <- mapvalues(dataset2$friendmembers,
                                          from = c("Agree", "Disagree", "Don't know",
                                                   "Strongly agree", "Strongly disagree"),
                                          to = c(1, 0, 0, 1, 0))

dataset2$Familymembers_dummy <- mapvalues(dataset2$familymembers,
                                          from = c("Agree", "Disagree", "Don't know",
                                                   "Strongly agree", "Strongly disagree"),
                                          to = c(1, 0, 0, 1, 0))

# Material

dataset2$Networking_dummy <- mapvalues(dataset2$networking,
                                       from = c("Agree", "Disagree", "Don't know",
                                                "Strongly agree", "Strongly disagree"),
                                       to = c(1, 0, 0, 1, 0))

dataset2$Futurecandidate_dummy <- mapvalues(dataset2$futurecandidate,
                                            from = c("Agree", "Disagree", "Don't know",
                                                     "Strongly agree", "Strongly disagree"),
                                            to = c(1, 0, 0, 1, 0))


##############################
### DESCRIPTIVE STATISTICS ###
##############################

dataframe <- dataset2 %>%
  select(Gender, age, Education, Familybackground, interest,
         youthwing, ideology, country, purposive, social, material,
         womenMPs, womanpleader, womanywleader, autonomousYW)

dataframe1 <- na.omit(dataframe)

dataframe1 %>% 
  count(Gender)

dataframe1 %>% 
  count(youthwing)

mean(dataframe1$purposive)
sd(dataframe1$purposive)
min(dataframe1$purposive)
max(dataframe1$purposive)

mean(dataframe1$social)
sd(dataframe1$social)
min(dataframe1$social)
max(dataframe1$social)

mean(dataframe1$material)
sd(dataframe1$material)
min(dataframe1$material)
max(dataframe1$material)

mean(dataframe1$Gender)
sd(dataframe1$Gender)

mean(dataframe1$age)
sd(dataframe1$age)
min(dataframe1$age)
max(dataframe1$age)

mean(dataframe1$Education)
sd(dataframe1$Education)

mean(dataframe1$Familybackground)
sd(dataframe1$Familybackground)

mean(dataframe1$interest)
sd(dataframe1$interest)
min(dataframe1$interest)
max(dataframe1$interest)

mean(dataset2$ideology_num, na.rm = TRUE)
sd(dataset2$ideology_num, na.rm = TRUE)

mean(dataset2$country_num, na.rm = TRUE)
sd(dataset2$country_num, na.rm = TRUE)


######################
##### BIVARIATES #####
######################

# Social desirability

CrossTable(dataset2$Networking_dummy, dataset2$Gender, prop.c=TRUE,
           prop.t=FALSE, prop.r=FALSE, prop.chisq = FALSE,
           total.r=FALSE, total.c=TRUE, chisq=TRUE)

CrossTable(dataset2$Futurecandidate_dummy, dataset2$Gender, prop.c=TRUE,
           prop.t=FALSE, prop.r=FALSE, prop.chisq = FALSE,
           total.r=FALSE, total.c=TRUE, chisq=TRUE)

# Gender & family ties

CrossTable(dataframe1$Familybackground, dataframe1$Gender, prop.c=TRUE,
           prop.t=FALSE, prop.r=FALSE, prop.chisq = FALSE,
           total.r=FALSE, total.c=TRUE, chisq=TRUE)


##############################
##### CORRELATION MATRIX #####
##############################

dataframe2 <- dataset2 %>%
  select(Gender, age, Education, Familybackground, interest, ideology_num, country_num)

dataframe3 <- na.omit(dataframe2)

corrmatrix <- rcorr(as.matrix(dataframe3))
corrmatrix$r

##############
### MODELS ###
##############

coefficientsnames <- c("countrySpain" = "Country: Spain",
                       "countryItaly" = "Country: Italy",
                       "countryGermany" = "Country: Germany",
                       "countryAustria" = "Country: Austria",
                       "ideologyCenter-right" = "Ideology: Center-right",
                       "interest" = "Political interest",
                       "Education" = "Tertiary education",
                       "age" = "Age",
                       "Familybackground" = "Party-affiliated family ties",
                       "Gender" = "Gender: Woman")

# Purposive reasons

model1p <- lm(purposive ~ Gender + country,
              data = dataframe1)
modelsummary(model1p, stars = TRUE, vcov = ~youthwing)

model2p <- lm(purposive ~ Familybackground + country,
              data = dataframe1)
modelsummary(model2p, stars = TRUE, vcov = ~youthwing)

model3p <- lm(purposive ~ Gender + Familybackground + age + Education + interest + country,
              data = dataframe1)
modelsummary(model3p, stars = TRUE, vcov = ~youthwing)

model5p <- lm(purposive ~ Gender + Familybackground + age + Education + interest + ideology + country,
              data = dataframe1)
modelsummary(model5p, stars = TRUE, vcov = ~youthwing)

model_purposive <- list(model1p, model2p, model3p, model5p)
modelsummary(model_purposive, stars = TRUE, vcov = ~youthwing, coef_rename = coefficientsnames,
             output = "Purposive incentives.docx")

# Solidary reasons

model1s <- lm(social ~ Gender + country,
              data = dataframe1)
modelsummary(model1s, stars = TRUE, vcov = ~youthwing)

model2s <- lm(social ~ Familybackground + country,
              data = dataframe1)
modelsummary(model2s, stars = TRUE, vcov = ~youthwing)

model3s <- lm(social ~ Gender + Familybackground + age + Education + interest + country,
              data = dataframe1)
modelsummary(model3s, stars = TRUE, vcov = ~youthwing)

model5s <- lm(social ~ Gender + Familybackground + age + Education + interest + ideology + country,
              data = dataframe1)
modelsummary(model5s, stars = TRUE, vcov = ~youthwing)

model_social <- list(model1s, model2s, model3s, model5s)
modelsummary(model_social, stars = TRUE, vcov = ~youthwing, coef_rename = coefficientsnames,
             output = "Social incentives.docx")

# Material reasons

model1m <- lm(material ~ Gender + country,
              data = dataframe1)
modelsummary(model1m, stars = TRUE, vcov = ~youthwing)

model2m <- lm(material ~ Familybackground + country,
              data = dataframe1)
modelsummary(model2m, stars = TRUE, vcov = ~youthwing)

model3m <- lm(material ~ Gender + Familybackground + age + Education + interest + country,
              data = dataframe1)
modelsummary(model3m, stars = TRUE, vcov = ~youthwing)

model5m <- lm(material ~ Gender + Familybackground + age + Education + interest + ideology + country,
              data = dataframe1)
modelsummary(model5m, stars = TRUE, vcov = ~youthwing)

model_material <- list(model1m, model2m, model3m, model5m)
modelsummary(model_material, stars = TRUE, vcov = ~youthwing, coef_rename = coefficientsnames,
             output = "Material incentives.docx")

# Material reasons - Same party family analysis

dataset2$sameparty_cat <- as.factor(dataset2$sameparty_cat)

dataset2x <- dataset2 %>%
  select(Gender, age, Education, sameparty_cat, interest, youthwing, ideology, country, purposive, social, material)

dataset2x <- na.omit(dataset2x)

model5msameparty <- lm(material ~ Gender + sameparty_cat + age + Education + interest + ideology + country,
              data = dataset2x)
modelsummary(model5msameparty, stars = TRUE, vcov = ~youthwing)

modelsummary(model5msameparty, stars = TRUE, vcov = ~youthwing, coef_rename = coefficientsnames,
             output = "Material incentives - Same party.docx")


###########################
### MODERATION ANALYSIS ###
###########################

### Models ###

model5pint <- lm(purposive ~ Gender + Familybackground + age + Education + interest + ideology + country +
                   Gender*Familybackground,
                 data = dataframe1)
modelsummary(model5pint, stars = TRUE, vcov = ~youthwing)

model5sint <- lm(social ~ Gender + Familybackground + age + Education + interest + ideology + country +
                   Gender*Familybackground,
                 data = dataframe1)
modelsummary(model5sint, stars = TRUE, vcov = ~youthwing)

model5mint <- lm(material ~ Gender + Familybackground + age + Education + interest + ideology + country +
                   Gender*Familybackground,
                 data = dataframe)
modelsummary(model5mint, stars = TRUE, vcov = ~youthwing)

model_interactions <- list(model5pint, model5sint, model5mint)
modelsummary(model_interactions, stars = TRUE, vcov = ~youthwing, coef_rename = coefficientsnames,
             output = "Models interactions.docx")


#############################
##### ROBUSTNESS CHECKS #####
#############################

### Clustered standard errors by country

model5pcseco <- lm(purposive ~ Gender + Familybackground + age + Education + interest + ideology + country,
              data = dataframe1)
modelsummary(model5pcseco, stars = TRUE, vcov = ~country)

model5scseco <- lm(social ~ Gender + Familybackground + age + Education + interest + ideology + country,
              data = dataframe1)
modelsummary(model5scseco, stars = TRUE, vcov = ~country)

model5mcseco <- lm(material ~ Gender + Familybackground + age + Education + interest + ideology + country,
              data = dataframe1)
modelsummary(model5mcseco, stars = TRUE, vcov = ~country)

model_cseco <- list(model5pcseco, model5scseco, model5mcseco)
modelsummary(model_cseco, stars = TRUE, vcov = ~country, coef_rename = coefficientsnames,
             output = "CSE country.docx")


### Purposive incentives, full models, one country at the time

australia <- dataset2 %>% 
  filter(country == "Australia")

austria <- dataset2 %>% 
  filter(country == "Austria")

germany <- dataset2 %>% 
  filter(country == "Germany")

italy <- dataset2 %>% 
  filter(country == "Italy")

spain <- dataset2 %>% 
  filter(country == "Spain")

rc1 <- lm(purposive ~ Gender + Familybackground + age + Education + interest + ideology,
          data = australia)
modelsummary(rc1, stars = TRUE, vcov = ~youthwing)

rc2 <- lm(purposive ~ Gender + Familybackground + age + Education + interest + ideology,
          data = austria)
modelsummary(rc2, stars = TRUE, vcov = ~youthwing)

rc3 <- lm(purposive ~ Gender + Familybackground + age + Education + interest + ideology,
          data = germany)
modelsummary(rc3, stars = TRUE, vcov = ~youthwing)

rc4 <- lm(purposive ~ Gender + Familybackground + age + Education + interest + ideology,
          data = italy)
modelsummary(rc4, stars = TRUE, vcov = ~youthwing)

rc5 <- lm(purposive ~ Gender + Familybackground + age + Education + interest + ideology,
          data = spain)
modelsummary(rc5, stars = TRUE, vcov = ~youthwing)

model_purposive_country <- list(rc1, rc2, rc3, rc4, rc5)
modelsummary(model_purposive_country, stars = TRUE, vcov = ~youthwing, coef_rename = coefficientsnames,
             output = "Purposive by country.docx")


### Social incentives, full models, one country at the time

rc6 <- lm(social ~ Gender + Familybackground + age + Education + interest + ideology,
          data = australia)
modelsummary(rc6, stars = TRUE, vcov = ~youthwing)

rc7 <- lm(social ~ Gender + Familybackground + age + Education + interest + ideology,
          data = austria)
modelsummary(rc7, stars = TRUE, vcov = ~youthwing)

rc8 <- lm(social ~ Gender + Familybackground + age + Education + interest + ideology,
          data = germany)
modelsummary(rc8, stars = TRUE, vcov = ~youthwing)

rc9 <- lm(social ~ Gender + Familybackground + age + Education + interest + ideology,
          data = italy)
modelsummary(rc9, stars = TRUE, vcov = ~youthwing)

rc10 <- lm(social ~ Gender + Familybackground + age + Education + interest + ideology,
           data = spain)
modelsummary(rc10, stars = TRUE, vcov = ~youthwing)

model_social_country <- list(rc6, rc7, rc8, rc9, rc10)
modelsummary(model_social_country, stars = TRUE, vcov = ~youthwing, coef_rename = coefficientsnames,
             output = "Social by country.docx")


### Material incentives, full models, one country at the time

rc11 <- lm(material ~ Gender + Familybackground + age + Education + interest + ideology,
           data = australia)
modelsummary(rc11, stars = TRUE, vcov = ~youthwing)

rc12 <- lm(material ~ Gender + Familybackground + age + Education + interest + ideology,
           data = austria)
modelsummary(rc12, stars = TRUE, vcov = ~youthwing)

rc13 <- lm(material ~ Gender + Familybackground + age + Education + interest + ideology,
           data = germany)
modelsummary(rc13, stars = TRUE, vcov = ~youthwing)

rc14 <- lm(material ~ Gender + Familybackground + age + Education + interest + ideology,
           data = italy)
modelsummary(rc14, stars = TRUE, vcov = ~youthwing)

rc15 <- lm(material ~ Gender + Familybackground + age + Education + interest + ideology,
           data = spain)
modelsummary(rc15, stars = TRUE, vcov = ~youthwing)

model_material_country <- list(rc11, rc12, rc13, rc14, rc15)
modelsummary(model_material_country, stars = TRUE, vcov = ~youthwing, coef_rename = coefficientsnames,
             output = "Material by country.docx")


### By party ideology ###

centerleft <- dataframe1 %>% 
  filter(ideology == "Center-left")

model5pLW <- lm(purposive ~ Gender + Familybackground + age + Education + interest + country,
                data = centerleft)
modelsummary(model5pLW, stars = TRUE, vcov = ~youthwing)

model5sLW <- lm(social ~ Gender + Familybackground + age + Education + interest + country,
                data = centerleft)
modelsummary(model5sLW, stars = TRUE, vcov = ~youthwing)

model5mLW <- lm(material ~ Gender + Familybackground + age + Education + interest + country,
                data = centerleft)
modelsummary(model5mLW, stars = TRUE, vcov = ~youthwing)

model_LW <- list(model5pLW, model5sLW, model5mLW)
modelsummary(model_LW, stars = TRUE, vcov = ~youthwing, coef_rename = coefficientsnames,
             output = "RC - Center-left.docx")

centerright <- dataframe1 %>% 
  filter(ideology == "Center-right")

model5pRW <- lm(purposive ~ Gender + Familybackground + age + Education + interest + country,
                data = centerright)
modelsummary(model5pRW, stars = TRUE, vcov = ~youthwing)

model5sRW <- lm(social ~ Gender + Familybackground + age + Education + interest + country,
                data = centerright)
modelsummary(model5sRW, stars = TRUE, vcov = ~youthwing)

model5mRW <- lm(material ~ Gender + Familybackground + age + Education + interest + country,
                data = centerright)
modelsummary(model5mRW, stars = TRUE, vcov = ~youthwing)

model_RW <- list(model5pRW, model5sRW, model5mRW)
modelsummary(model_RW, stars = TRUE, vcov = ~youthwing, coef_rename = coefficientsnames,
             output = "RC - Center-right.docx")


### By autonomy of youth wing ###

autonomousYWs <- dataframe1 %>% 
  filter(autonomousYW == 1)

model5pauto <- lm(purposive ~ Gender + Familybackground + age + Education + interest + ideology + country,
                data = autonomousYWs)
modelsummary(model5pauto, stars = TRUE, vcov = ~youthwing)

model5sauto <- lm(social ~ Gender + Familybackground + age + Education + interest + ideology + country,
                data = autonomousYWs)
modelsummary(model5sauto, stars = TRUE, vcov = ~youthwing)

model5mauto <- lm(material ~ Gender + Familybackground + age + Education + interest + ideology + country,
                data = autonomousYWs)
modelsummary(model5mauto, stars = TRUE, vcov = ~youthwing)

model_auto <- list(model5pauto, model5sauto, model5mauto)
modelsummary(model_auto, stars = TRUE, vcov = ~youthwing, coef_rename = coefficientsnames,
             output = "RC - Auto.docx")

nonautonomousYWs <- dataframe1 %>% 
  filter(autonomousYW == 0)

model5nopauto <- lm(purposive ~ Gender + Familybackground + age + Education + interest + ideology + country,
                  data = nonautonomousYWs)
modelsummary(model5nopauto, stars = TRUE, vcov = ~youthwing)

model5snoauto <- lm(social ~ Gender + Familybackground + age + Education + interest + ideology + country,
                  data = nonautonomousYWs)
modelsummary(model5snoauto, stars = TRUE, vcov = ~youthwing)

model5mnoauto <- lm(material ~ Gender + Familybackground + age + Education + interest + ideology + country,
                  data = nonautonomousYWs)
modelsummary(model5mnoauto, stars = TRUE, vcov = ~youthwing)

model_noauto <- list(model5nopauto, model5snoauto, model5mnoauto)
modelsummary(model_noauto, stars = TRUE, vcov = ~youthwing, coef_rename = coefficientsnames,
             output = "RC - No Auto.docx")


### Results by single incentive items ###

dataframe2 <- dataset2 %>%
  select(Gender, age, Education, Familybackground, interest, youthwing, ideology, country,
         Workelecampaign_dummy, Influencepolicyjoin_dummy, Identificationplatform_dummy,
         Peoplesharingvalues_dummy, Friendmembers_dummy, Familymembers_dummy,
         Networking_dummy, Futurecandidate_dummy)

dataframe3 <- na.omit(dataframe2)

dataframe3$Workelecampaign_dummy <- as.numeric(dataframe3$Workelecampaign_dummy)
dataframe3$Influencepolicyjoin_dummy <- as.numeric(dataframe3$Influencepolicyjoin_dummy)
dataframe3$Identificationplatform_dummy <- as.numeric(dataframe3$Identificationplatform_dummy)
dataframe3$Peoplesharingvalues_dummy <- as.numeric(dataframe3$Peoplesharingvalues_dummy)
dataframe3$Friendmembers_dummy <- as.numeric(dataframe3$Friendmembers_dummy)
dataframe3$Familymembers_dummy <- as.numeric(dataframe3$Familymembers_dummy)
dataframe3$Networking_dummy <- as.numeric(dataframe3$Networking_dummy)
dataframe3$Futurecandidate_dummy <- as.numeric(dataframe3$Futurecandidate_dummy)

rc91 <- glm(Workelecampaign_dummy ~ Gender + Familybackground + age + Education + interest + ideology + country,
            data = dataframe3, family = "binomial")
modelsummary(rc91, stars = TRUE, vcov = ~youthwing)

rc92 <- glm(Influencepolicyjoin_dummy ~ Gender + Familybackground + age + Education + interest + ideology + country,
            data = dataframe3, family = "binomial")
modelsummary(rc92, stars = TRUE, vcov = ~youthwing)

rc93 <- glm(Identificationplatform_dummy ~ Gender + Familybackground + age + Education + interest + ideology + country,
            data = dataframe3, family = "binomial")
modelsummary(rc93, stars = TRUE, vcov = ~youthwing)

rc94 <- glm(Peoplesharingvalues_dummy ~ Gender + Familybackground + age + Education + interest + ideology + country,
            data = dataframe3, family = "binomial")
modelsummary(rc94, stars = TRUE, vcov = ~youthwing)

rc95 <- glm(Friendmembers_dummy ~ Gender + Familybackground + age + Education + interest + ideology + country,
            data = dataframe3, family = "binomial")
modelsummary(rc95, stars = TRUE, vcov = ~youthwing)

rc96 <- glm(Familymembers_dummy ~ Gender + Familybackground + age + Education + interest + ideology + country,
            data = dataframe3, family = "binomial")
modelsummary(rc96, stars = TRUE, vcov = ~youthwing)

rc97 <- glm(Networking_dummy ~ Gender + Familybackground + age + Education + interest + ideology + country,
            data = dataframe3, family = "binomial")
modelsummary(rc97, stars = TRUE, vcov = ~youthwing)

rc98 <- glm(Futurecandidate_dummy ~ Gender + Familybackground + age + Education + interest + ideology + country,
            data = dataframe3, family = "binomial")
modelsummary(rc98, stars = TRUE, vcov = ~youthwing)

model_purposive_single <- list(rc92, rc93, rc91)
modelsummary(model_purposive_single, stars = TRUE, vcov = ~youthwing, coef_rename = coefficientsnames,
             output = "Purposive single.docx")

model_social_single <- list(rc94, rc96, rc95)
modelsummary(model_social_single, stars = TRUE, vcov = ~youthwing, coef_rename = coefficientsnames,
             output = "Social single.docx")

model_material_single <- list(rc97, rc98)
modelsummary(model_material_single, stars = TRUE, vcov = ~youthwing, coef_rename = coefficientsnames,
             output = "Material single.docx")


############################
##### FURTHER ANALYSES #####
############################

### Demand-side factors

# Purposive

model5pdem <- lm(purposive ~ Gender + Familybackground + age + Education + interest + ideology + 
                   womenMPs + womanpleader + womanywleader + country,
                 data = dataframe1)
modelsummary(model5pdem, stars = TRUE, vcov = ~youthwing)

model5pdem_int <- lm(purposive ~ Gender + Familybackground + age + Education + interest + ideology + 
                   womenMPs + womanpleader + womanywleader + country +
                  Gender*womenMPs + Gender*womanpleader + Gender*womanywleader,
              data = dataframe1)
modelsummary(model5pdem_int, stars = TRUE, vcov = ~youthwing)

# Solidary

model5sdem <- lm(social ~ Gender + Familybackground + age + Education + interest + ideology + 
                   womenMPs + womanpleader + womanywleader + country,
              data = dataframe1)
modelsummary(model5sdem, stars = TRUE, vcov = ~youthwing)

model5sdem_int <- lm(social ~ Gender + Familybackground + age + Education + interest + ideology + 
                       womenMPs + womanpleader + womanywleader + country +
                       Gender*womenMPs + Gender*womanpleader + Gender*womanywleader,
                     data = dataframe1)
modelsummary(model5sdem_int, stars = TRUE, vcov = ~youthwing)

# Material

model5mdem <- lm(material ~ Gender + Familybackground + age + Education + interest + ideology + 
                   womenMPs + womanpleader + womanywleader + country,
              data = dataframe1)
modelsummary(model5mdem, stars = TRUE, vcov = ~youthwing)

model5mdem_int <- lm(material ~ Gender + Familybackground + age + Education + interest + ideology + 
                       womenMPs + womanpleader + womanywleader + country +
                       Gender*womenMPs + Gender*womanpleader + Gender*womanywleader,
                     data = dataframe1)
modelsummary(model5mdem_int, stars = TRUE, vcov = ~youthwing)

model_demand <- list(model5pdem, model5pdem_int, model5sdem, model5sdem_int, model5mdem, model5mdem_int)
modelsummary(model_demand, stars = TRUE, vcov = ~youthwing, coef_rename = coefficientsnames,
             output = "Demand-side factors.docx")


####################################################################################
# Before running this, clean the environment and re-run main code until line no. 167
####################################################################################

### Social incentives without 'family' item ###

KMO(dataset2[c("Peoplesharingvalues", "Workelecampaign", "Friendmembers", "Influencepolicyjoin",
               "Identificationplatform", "Networking", "Futurecandidate")])

fa.parallel(dataset2[c("Peoplesharingvalues", "Workelecampaign", "Friendmembers", "Influencepolicyjoin",
                       "Identificationplatform", "Networking", "Futurecandidate")])

fa <- principal(dataset2[c("Peoplesharingvalues", "Workelecampaign", "Friendmembers", "Influencepolicyjoin",
                           "Identificationplatform", "Networking", "Futurecandidate")],
                nfactors = 3, rotate = "varimax")
fa

dataset2 <- cbind(dataset2, fa$scores)
rm(fa)
psych::alpha(dataset2[c("Peoplesharingvalues", "Friendmembers")], check.keys = T)

dataset2$social <- dataset2$RC2
hist(dataset2$social)

dataset2$social <- (dataset2$social - min(dataset2$social, na.rm = TRUE)) / 
  (max(dataset2$social, na.rm = TRUE) - min(dataset2$social, na.rm = TRUE))

dataframe <- dataset2 %>%
  select(Gender, age, Education, Familybackground, interest, youthwing, ideology, country, social)

dataframe1 <- na.omit(dataframe)

coefficientsnames <- c("countrySpain" = "Country: Spain",
                       "countryItaly" = "Country: Italy",
                       "countryGermany" = "Country: Germany",
                       "countryAustria" = "Country: Austria",
                       "ideologyCenter-right" = "Ideology: Center-right",
                       "interest" = "Political interest",
                       "Education" = "Tertiary education",
                       "age" = "Age",
                       "Familybackground" = "Party-affiliated family ties",
                       "Gender" = "Gender: Woman")

model1s <- lm(social ~ Gender + country,
              data = dataframe1)
modelsummary(model1s, stars = TRUE, vcov = ~youthwing)

model2s <- lm(social ~ Familybackground + country,
              data = dataframe1)
modelsummary(model2s, stars = TRUE, vcov = ~youthwing)

model3s <- lm(social ~ Gender + Familybackground + age + Education + interest + country,
              data = dataframe1)
modelsummary(model3s, stars = TRUE, vcov = ~youthwing)

model5s <- lm(social ~ Gender + Familybackground + age + + Education + interest + ideology + country,
              data = dataframe1)
modelsummary(model5s, stars = TRUE, vcov = ~youthwing)

model_RCsoc <- list(model1s, model2s, model3s, model5s)
modelsummary(model_RCsoc, stars = TRUE, vcov = ~youthwing, coef_rename = coefficientsnames,
             output = "RCs with new social incentives.docx")
