library(xtable)
library(texreg)
library(plyr)
library(effsize)

raw_gender_data = read.csv("2025.01.04 - mrt data.csv")
ggi = read.csv("2020_ggi.csv")

## Making a nominal version of the gender variable:
raw_gender_data$gender.flipped = 1 - raw_gender_data$gender # now 1 = male; 0 = female
raw_gender_data$gender.f = factor(raw_gender_data$gender.flipped)

raw_gender_data <- subset(raw_gender_data, age >= 13)

raw_gender_data <- merge(raw_gender_data, ggi, by.x = "country_young", by.y = "country", all.x = TRUE)
raw_gender_data <- rename(raw_gender_data, c("GGI_young" = "GGI"))
raw_gender_data <- merge(raw_gender_data, ggi, by.x = "country_now", by.y = "country", all.x = TRUE)
raw_gender_data <- rename(raw_gender_data, c("GGI_now" = "GGI"))

## log age
raw_gender_data$log10_age = log10(raw_gender_data$age)

## picking just the relevant columns
relevant_vars = c("gender.f", "cmni27", "cmni43", "cmni41", "cmni24", "cmni8", "cmni25", "cmni36", "log10_age", "score")
extended_vars = c("gender.f", "cmni27", "cmni43", "cmni41", "cmni24", "cmni8", "cmni25", "cmni36", "GGI_young", "GGI_now", "log10_age", "score")
gender_data <- raw_gender_data[relevant_vars]
gender_data <- gender_data[complete.cases(gender_data),]

extended_gender_data <- raw_gender_data[extended_vars]
extended_gender_data <- extended_gender_data[complete.cases(extended_gender_data),]


## Making a subset of data with just binary genders
binary_gender_data <- subset(gender_data, gender.f == 0 | gender.f == 1)
binary_and_nonbinary_gender_data <- subset(gender_data, gender.f == 0 | gender.f == 1 | gender.f == -1)
# write.csv(binary_gender_data, "mite-binary_data.csv")
# write.csv(binary_and_nonbinary_gender_data, "mite-binary_and_nonbinary_data.csv")
extended_binary_gender_data <- subset(extended_gender_data, gender.f == 0 | gender.f == 1)
extended_binary_and_nonbinary_gender_data <- subset(extended_gender_data, gender.f == 0 | gender.f == 1 | gender.f == -1)

print("******* PARTICIPANT INFO ************")
print("Binary gender data:")
print(nrow(binary_gender_data))
print(tapply(binary_gender_data$gender.f, binary_gender_data$gender.f, length))
print("Binary and non-binary gender data:")
print(nrow(binary_and_nonbinary_gender_data))
print(tapply(binary_and_nonbinary_gender_data$gender.f, binary_and_nonbinary_gender_data$gender.f, length))
print("Extended binary and non-binary gender data:")
print(nrow(extended_binary_and_nonbinary_gender_data))
print(tapply(extended_binary_and_nonbinary_gender_data$gender.f, extended_binary_and_nonbinary_gender_data$gender.f, length))
print("Countries in raw data")
print(nrow(tapply(raw_gender_data$country_young, raw_gender_data$country_young, length)))
print(sort(tapply(raw_gender_data$country_young, raw_gender_data$country_young, length)))
print("Number of people in raw data")
print(nrow(raw_gender_data))
print("Number of people who grew up and still live in the same country")
print(nrow(subset(raw_gender_data, country_young != "", country_young == country_now)))

print("********* EFFECT SIZES ***********")
binary_gender_data_aux = binary_gender_data
binary_gender_data_aux$gender.n <- as.numeric(binary_gender_data_aux$gender.f)
print("score")
print(cohen.d(score ~ gender.n, data=binary_gender_data_aux))


# Data for Table 7 and the accompanying diagram
print("******* HIERARCHICAL REGRESSION ************")
m0 <- lm (score ~ log10_age + I(log10_age^2), data=extended_binary_gender_data)
m_gender <- lm (score ~ log10_age + I(log10_age^2) + gender.f, data=extended_binary_gender_data)
m_cmni <- lm (score ~ log10_age + I(log10_age^2) + cmni27 + cmni43 + cmni41 + cmni24 + cmni8 + cmni25 +cmni36, data=extended_binary_gender_data)
m_ggi <- lm (score ~ log10_age + I(log10_age^2) + GGI_young + GGI_now, data=extended_binary_gender_data)
m_gender_cmni <- lm (score ~ log10_age + I(log10_age^2) + gender.f + cmni27 + cmni43 + cmni41 + cmni24 + cmni8 + cmni25 + cmni36, data=extended_binary_gender_data)
m_gender_ggi <- lm (score ~ log10_age + I(log10_age^2) + gender.f + GGI_young + GGI_now, data=extended_binary_gender_data)
m_gender_cmni_ggi <- lm (score ~ log10_age + I(log10_age^2) + gender.f + cmni27 + cmni43 + cmni41 + cmni24 + cmni8 + cmni25 + cmni36 + GGI_young + GGI_now, data=extended_binary_gender_data)
m_gender_cmni_interactions <- lm (score ~ log10_age + I(log10_age^2) + gender.f + cmni27 + cmni43 + cmni41 + cmni24 + cmni8 + cmni25 + cmni36 + gender.f:cmni27 + gender.f:cmni43 + gender.f:cmni41 + gender.f:cmni24 + gender.f:cmni8 + gender.f:cmni25 + gender.f:cmni36, data=extended_binary_gender_data)
m_gender_ggi_interactions <- lm (score ~ log10_age + I(log10_age^2) + gender.f + GGI_young + GGI_now + gender.f:GGI_young + gender.f:GGI_now, data=extended_binary_gender_data)


print(texreg( list( m_gender, m_gender_cmni, m_gender_ggi, m_gender_cmni_ggi ), single.row = TRUE, dcolumn = TRUE ))
print(texreg( list( m_gender, m_gender_cmni, m_gender_ggi, m_gender_cmni_ggi ), single.row = TRUE, digits=3, dcolumn = TRUE ))
# print(texreg::screenreg( list( m0, m_gender, m_cmni, m_gender_cmni ), single.row = TRUE ))
# print(texreg::screenreg( list( m0, m_gender, m_cmni, m_gender_cmni, m_gender_cmni_interactions ), single.row = TRUE ))
print(texreg::screenreg( list( m0, m_gender, m_cmni, m_ggi, m_gender_cmni, m_gender_ggi, m_gender_cmni_ggi, m_gender_cmni_interactions, m_gender_ggi_interactions ), single.row = TRUE, digits = 3 ))
print("Gender to m0")
print(anova(m0, m_gender))
print("CMNI to m0")
print(anova(m0, m_cmni))
print("GGI to m0")
print(anova(m0, m_ggi))
print("Gender+CMNI to gender only")
print(anova(m_gender, m_gender_cmni))
print("Gender+CMNI to CMNI only")
print(anova(m_cmni, m_gender_cmni))
print("Gender+GGI to gender only")
print(anova(m_gender, m_gender_ggi))
print("Gender+CMNI+GGI to gender+CMNI")
print(anova(m_gender_cmni, m_gender_cmni_ggi))
print("Gender+CMNI+GGI to gender+GGI")
print(anova(m_gender_ggi, m_gender_cmni_ggi))
print("Gender+CMNI with interactions to Gender+CMNI")
print(anova(m_gender_cmni, m_gender_cmni_interactions))
print("Gender+GGI with interactions to Gender+GGI")
print(anova(m_gender_ggi, m_gender_ggi_interactions))


## ********** Table 6: Multiple Mediation Analysis
print("******* MMA ANALYSIS ************")
library("mma")
# adding squared log age as an explicit column
binary_gender_data$log10_age_sq <- binary_gender_data$log10_age^2
# Slicing the data for the mma analysis
binary_gender_data.x <- subset(binary_gender_data, select=-c(score,gender.f))
binary_gender_data.y <- binary_gender_data[c("score")]
binary_gender_data.pred <- binary_gender_data[c("gender.f")]

res <- mma(binary_gender_data.x, binary_gender_data.y, n2=1000, pred=binary_gender_data.pred, contmed=1:7, jointm=list(n=1, j1=1:7), predref=0, alpha=0.05, alpha2 = 0.05)
print(paste("N=", nrow(binary_gender_data.x)))
print(summary(res))

