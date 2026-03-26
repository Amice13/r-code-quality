####################################################################
## author:    Steven M. Van Hauwaert & Robert A. Huber
## file name: terror_robustEB.R
## Context:   Causal Effects of Terror Attacks
## started:   2018-03-13
## Summary:   runs R-Scripts
######################################################################

## empty memory (!)
rm(list=ls())

originalData <- read.dta("./original data/ZA6643_v3-1-0.dta")

originalData <- subset(originalData, isocntry == "FR")

df <- data.frame(id = 1:nrow(originalData),
                 lrscale = ifelse(originalData$d1 == "Refusal" | originalData$d1 == "DK", NA,
                                  as.numeric(originalData$d1)),
                 genderF = factor(originalData$d10),
                 age = originalData$d11)

df$edu <- ifelse(originalData$d8r2 == "No full-time education", "Minimum Education",
                 ifelse(originalData$d8r2 == "Up to 15", "Up to 15",
                        ifelse(originalData$d8r2 == "16-19", "16-19",
                               ifelse(originalData$d8r2 == "20+", "20+",
                                      ifelse(originalData$d8r2 == "Still Studying", "Still Studying", NA)))))

#collapsing minimum education, as only few cases
df$edu <- ifelse(df$edu == "Still Studying", ifelse(originalData$d11 < 20, "16-19", "20+"), df$edu)
df$edu <- factor(df$edu, levels = c("Minimum Education", "Up to 15", "16-19", "20+"))
df$edu <- fct_collapse(df$edu,
                       "Up to 15" = c("Minimum Education", "Up to 15"))


#6-9
originalData$qa8a_6 <- as.numeric(car::recode(originalData$qa8a_6, "'DK'=NA"))
originalData$qa8a_8 <- as.numeric(car::recode(originalData$qa8a_8, "'DK'=NA"))
originalData$qa8a_9 <- as.numeric(car::recode(originalData$qa8a_9, "'DK'=NA"))

fut_vars <- paste0("qa8a_", c(6,8:9))
nScree(originalData[complete.cases(originalData[,fut_vars]),fut_vars]) # majority suggest 3
plotnScree(nScree(originalData[complete.cases(originalData[,fut_vars]),fut_vars]))

fac_fit_polTrust <- principal(originalData[, fut_vars], nfactors=1, rotate="oblimin")

#Next line contains all information for Table F.2
fac_fit_polTrust

df$polTrust <- fac_fit_polTrust$scores

originalData$qb4_2 <- as.numeric(car::recode(originalData$qb4_2, "'DK'=NA"))
originalData$qd11_3 <- as.numeric(car::recode(originalData$qd11_3, "'DK'=NA"))

df$posImm <- (originalData$qb4_2 + originalData$qd11_3) / 2 

df$salImm <- factor(ifelse(originalData$qa3a_9 == "Immigration", "High", "Low"), levels = c("Low", "High"))

df$salImm_num <- as.numeric(df$salImm)-1

df$socCoh <- car::recode(originalData$qd11_4, "'Totally agree' = 3; 
                         'Tend to agree' = 2; 'Tend to disagree' = 1;
                         'Totally disagree' = 0; 'DK' = NA", as.numeric = T)
df$socCoh <- as.numeric(df$socCoh)

df$date <- originalData$p1

df$cutoff <- as.numeric(df$date)-8

df$postTerror <- factor(ifelse(df$cutoff < 0, "Pre-Terror", "Post-Terror"), levels = c("Pre-Terror", "Post-Terror"))

df$extreme <- abs(df$lrscale-5.5) -.5

bw <- 3

m_socCoh <- lm(socCoh ~ postTerror + age + genderF + edu,
                   data = df[df$cutoff>-bw & df$cutoff<bw,])

m_polTrust <- lm(polTrust ~ postTerror + age + genderF + edu,
                   data = df[df$cutoff>-bw & df$cutoff<bw,])

m_posImm <- lm(posImm ~ postTerror + age + genderF + edu,
              data = df[df$cutoff>-bw & df$cutoff<bw,])


m_salImm <- lm(salImm_num ~ postTerror + age + genderF + edu,
               data = df[df$cutoff>-bw & df$cutoff<bw,])

m_lrscale <- lm(lrscale ~ postTerror + age + genderF + edu,
                data = df[df$cutoff>-bw & df$cutoff<bw,])

m_extreme <- lm(extreme ~ postTerror + age + genderF + edu,
                data = df[df$cutoff>-bw & df$cutoff<bw,])

texreg(list(m_socCoh, m_polTrust, m_posImm, m_salImm, m_extreme),
        file = "./tables/Table_F4.tex",
        caption = "Full regression discontinuity results using EB 84.3",
        caption.above = T,
        label = "t_regEB",  float.pos = "htb",
        digits = 2,
        custom.model.names = c("Social Cohesion", "Political Trust", "Immigration Opinions",
                               "Immigration Salience", "Political Polarisation"),
        custom.coef.names = c(NA, "Post Terror", "Age", "Gender", "Education (16-19)", "Education (20+)")) 

m_age <- lm(age ~ postTerror,
                data = df[df$cutoff>-bw & df$cutoff<bw,])

m_genderF <- lm(genderF == "Woman" ~ postTerror,
                data = df[df$cutoff>-bw & df$cutoff<bw,])

m_edu <- lm(as.numeric(edu) ~ postTerror,
            data = df[df$cutoff>-bw & df$cutoff<bw,])

texreg(list(m_age, m_genderF, m_edu),
        file = "./tables/Table_F3.tex",
        caption = "Balance tests for Eurobarometer 84.3",
        caption.above = T, float.pos = "htb",
        label = "t_balanceEB",
        digits = 3,
        custom.model.names = c("Age", "Gender", "Education"),
        custom.coef.names = c(NA, "Post Terror")) 

# Validation with +18 only ####

m_socCoh_18 <- lm(socCoh ~ postTerror + age + genderF + edu,
               data = df[df$cutoff>-bw & df$cutoff<bw & df$age > 17,])

m_polTrust_18 <- lm(polTrust ~ postTerror + age + genderF + edu,
                 data = df[df$cutoff>-bw & df$cutoff<bw & df$age > 17,])

m_posImm_18 <- lm(posImm ~ postTerror + age + genderF + edu,
               data = df[df$cutoff>-bw & df$cutoff<bw & df$age > 17,])

m_salImm_18 <- lm(salImm_num ~ postTerror + age + genderF + edu,
               data = df[df$cutoff>-bw & df$cutoff<bw & df$age > 17,])

m_lrscale_18 <- lm(lrscale ~ postTerror + age + genderF + edu,
                data = df[df$cutoff>-bw & df$cutoff<bw & df$age > 17,])

m_extreme_18 <- lm(extreme ~ postTerror + age + genderF + edu,
                data = df[df$cutoff>-bw & df$cutoff<bw & df$age > 17,])

#Next lines contains all information for Table F.1
describe(originalData$qd11_4 == "Totally agree")
describe(originalData$qd11_4 == "Tend to agree")
describe(originalData$qd11_4 == "Tend to disagree")
describe(originalData$qd11_4 == "Totally disagree")
describe(originalData$qd11_4 == "DK")

describe(df$polTrust)

describe(df$posImm)

describe(df$salImm == 'High')

describe(df$extreme)

describe(df$postTerror == "Post-Terror")

describe(df$cutoff)

describe(df$age)

describe(df$genderF == "Woman")

describe(df$edu == 'Up to 15')
describe(df$edu == '16-19')
describe(df$edu == '20+')
