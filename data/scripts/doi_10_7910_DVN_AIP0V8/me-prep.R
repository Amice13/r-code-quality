##
#
# Reproducibility material for 'Democracy for the Youth? The Impact of Mock Elections on Voting Age Attitudes'
#
# Erik Gahner Larsen, Klaus Levinsen, Ulrik Kjaer
#
# This file uses the full datasets to create a single csv dataset
#
##

# Load foreign package, needed
library("foreign")

# Read 2009 data
valgret.09 <- read.spss("KV09_UK_100701.sav", use.value.labels = F, to.data.frame = T)

# Create year and context variables
valgret.09$year <- 2009
valgret.09$komnr <- valgret.09$KomCPR

# Read 2013 data
valgret.13 <- read.spss("KVALG13_01_sendt.sav", use.value.labels = F, to.data.frame = T)

# Create year variable
valgret.13$year <- 2013

# Create dependent variable in 2009 data
valgret.09$valgret <- NA
valgret.09$valgret[valgret.09$q85 == 5] <- 1
valgret.09$valgret[valgret.09$q85 == 4] <- 2
valgret.09$valgret[valgret.09$q85 == 3] <- 3
valgret.09$valgret[valgret.09$q85 == 2] <- 4
valgret.09$valgret[valgret.09$q85 == 1] <- 5

# Create dependent variable (dummy) in 2009 data
valgret.09$valgret.d <- NA
valgret.09$valgret.d[valgret.09$valgret == 1] <- 0
valgret.09$valgret.d[valgret.09$valgret >= 2 & valgret.09$valgret <= 5] <- 1

# Create covariates
valgret.09$male <- ifelse(valgret.09$q118 == 1, 1, 0)
valgret.09$age <- valgret.09$Alder
valgret.09$educ <- valgret.09$q123
valgret.09$ideology[valgret.09$q43 != 12] <- valgret.09$q43[valgret.09$q43 != 12] - 1
valgret.09$party <- valgret.09$q39
valgret.09$party[valgret.09$q39 == 8] <- 7
valgret.09$party[valgret.09$q39 == 9] <- 8
valgret.09$party[valgret.09$q39 >= 10 & valgret.09$q39 <= 13] <- NA

valgret.09$child <- NA
valgret.09$child <- ifelse(valgret.09$q122n1 > 0, 1, 0)
valgret.09$child[valgret.09$q122 == 2] <- 0

# Create dependent variable in 2013 data
valgret.13$valgret <- NA
valgret.13$valgret[valgret.13$Spm32q == 5] <- 1
valgret.13$valgret[valgret.13$Spm32q == 4] <- 2
valgret.13$valgret[valgret.13$Spm32q == 3] <- 3
valgret.13$valgret[valgret.13$Spm32q == 2] <- 4
valgret.13$valgret[valgret.13$Spm32q == 1] <- 5

# Create dependent variable (dummy) in 2013 data
valgret.13$valgret.d <- NA
valgret.13$valgret.d[valgret.13$valgret == 1] <- 0
valgret.13$valgret.d[valgret.13$valgret >= 2 & valgret.13$valgret <= 5] <- 1

# Create covariates in 2013 data
valgret.13$male <- ifelse(valgret.13$Spm46 == 1, 1, 0)
valgret.13$age <- valgret.13$Spm47

valgret.13$educ <- valgret.13$Spm57
valgret.13$educ[valgret.13$Spm57 == 9] <- 8
valgret.13$educ[valgret.13$Spm57 == 10] <- NA

valgret.13$ideology[valgret.13$Spm24 != 12] <- valgret.13$Spm24[valgret.13$Spm24 != 12] - 1

valgret.13$party <- valgret.13$Spm14
valgret.13$party[valgret.13$Spm14 == 6] <- 5
valgret.13$party[valgret.13$Spm14 == 7] <- 6
valgret.13$party[valgret.13$Spm14 == 8] <- 7
valgret.13$party[valgret.13$Spm14 == 9] <- 8
valgret.13$party[valgret.13$Spm14 >= 10 & valgret.13$Spm14 <= 15] <- NA

valgret.13$child <- NA
valgret.13$child <- ifelse(valgret.13$Spm51c > 0, 1, 0)

# Bind rows from the two data frames
valgret <- rbind(
  valgret.09[c("year", "komnr", "valgret", "valgret.d", "male", "age", "educ", "ideology", "party", "child")],
  valgret.13[c("year", "komnr", "valgret", "valgret.d", "male", "age", "educ", "ideology", "party", "child")]
  )

# Create age category variable
valgret$age.cat <- NA
valgret$age.cat[valgret$age >= 18 & valgret$age <= 25] <- 0
valgret$age.cat[valgret$age > 25 & valgret$age <= 35] <- 1
valgret$age.cat[valgret$age > 35 & valgret$age <= 45] <- 2
valgret$age.cat[valgret$age > 45 & valgret$age <= 55] <- 3
valgret$age.cat[valgret$age > 55 & valgret$age <= 65] <- 4
valgret$age.cat[valgret$age > 65 & valgret$age <= 100] <- 5

# Create high school variable
valgret$highschool <- NA
valgret$highschool <- ifelse(valgret$educ == 1 | valgret$educ == 4,  0, 1)

# Create party variable
valgret$party <- ifelse(is.na(valgret$party),  9, valgret$party)

# Create mock election period variable
valgret$mep <- ifelse(valgret$year == 2009, 1, 0)

# Create age category variables
valgret$age.cat.0 <- NA
valgret$age.cat.0 <- ifelse(valgret$age.cat == 0, 1, 0)
valgret$age.cat.1 <- NA
valgret$age.cat.1 <- ifelse(valgret$age.cat == 1, 1, 0)
valgret$age.cat.2 <- NA
valgret$age.cat.2 <- ifelse(valgret$age.cat == 2, 1, 0)
valgret$age.cat.3 <- NA
valgret$age.cat.3 <- ifelse(valgret$age.cat == 3, 1, 0)
valgret$age.cat.4 <- NA
valgret$age.cat.4 <- ifelse(valgret$age.cat == 4, 1, 0)
valgret$age.cat.5 <- NA
valgret$age.cat.5 <- ifelse(valgret$age.cat == 5, 1, 0)

# Create party variables
valgret$party.1 <- NA
valgret$party.1 <- ifelse(valgret$party == 1, 1, 0)
valgret$party.2 <- NA
valgret$party.2 <- ifelse(valgret$party == 2, 1, 0)
valgret$party.3 <- NA
valgret$party.3 <- ifelse(valgret$party == 3, 1, 0)
valgret$party.4 <- NA
valgret$party.4 <- ifelse(valgret$party == 4, 1, 0)
valgret$party.5 <- NA
valgret$party.5 <- ifelse(valgret$party == 5, 1, 0)
valgret$party.6 <- NA
valgret$party.6 <- ifelse(valgret$party == 6, 1, 0)
valgret$party.7 <- NA
valgret$party.7 <- ifelse(valgret$party == 7, 1, 0)
valgret$party.8 <- NA
valgret$party.8 <- ifelse(valgret$party == 8, 1, 0)
valgret$party.9 <- NA
valgret$party.9 <- ifelse(valgret$party == 9, 1, 0)

# Write data frame to me-survey.csv
write.csv(valgret, file = "me-survey.csv")