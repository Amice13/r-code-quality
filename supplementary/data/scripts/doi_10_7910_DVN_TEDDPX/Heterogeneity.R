# load data
load("df.RData")

# create variables for analyses

## Treatment
df$treated <- NA
df$treated[df$vaagroup == 2] <- 1
df$treated[df$vaagroup == 1] <- 0

df$VAA_Treatment <- df$treated

## Are you interested in learning about party positions ahead of elections?
df$interested <- NA
df$interested[df$v93 == 2] <- 0
df$interested[df$v93 == 1] <- 1


## Which of the following statements best describe you? 1-3 didnt vote, 4 voted, 5 dont know
df$voted <- NA
df$voted[df$v104 == 1 | df$v104 == 2 | df$v104 == 3] <- 0
df$voted[df$v104 == 4] <- 1

df$Turnout <- df$voted


## Vote switch 
df$v12 <- as.numeric(df$v12) # pre-electoral vote intention
df$v105 <- as.numeric(df$v105) # post-electoral vote choice
df$voteswitch <- NA
df$voteswitch[df$v105 == df$v12] <- 0
df$voteswitch[df$v105 != df$v12] <- 1
df$voteswitch[df$v12 > 7] <- NA
df$voteswitch[df$v105 > 7] <- NA
df[1:200, c("v105", "v12", "voteswitch")]


df$Voteswitching <- df$voteswitch


## Knowledge 1: Which of the following are European party groups in the European Parliament? Select all that apply
df$party_know <- (df$v112_1 + df$v112_2 + df$v112_5) - (df$v112_3 + df$v112_4 + df$v112_6)
df[1:200, c("party_know", "v112_1", "v112_2", "v112_3", "v112_4", "v112_5", "v112_6")]

df$Party_Knowledge <- df$party_know


## Knowledge 2: Which of the following issues has the European Parliament voted on in the last 5 years? Select all that apply
df$policy_know <- (df$v113_1 + df$v113_3 + df$v113_5) - (df$v113_2 + df$v113_4 + df$v113_6)
df[1:200, c("policy_know", "v113_1", "v113_2", "v113_3", "v113_4", "v113_5", "v113_6")]

df$Policy_Knowledge <- df$policy_know


## Did respondents start the VAA
df$take_vaa <- 0
condition <- df$vaa_v1 > -2 | df$vaa_v2 > -2 | df$vaa_v3 > -2 | df$vaa_v4 > -2 | 
  df$vaa_v5 > -2 | df$vaa_v6 > -2 | df$vaa_v7 > -2 | df$vaa_v8 > -2 | 
  df$vaa_v9 > -2 | df$vaa_v10 > -2 | df$vaa_v11 > -2 | df$vaa_v12 > -2 | 
  df$vaa_v13 > -2 | df$vaa_v14 > -2 | df$vaa_v15 > -2 | df$vaa_v16 > -2 | 
  df$vaa_v17 > -2 | df$vaa_v18 > -2 | df$vaa_v19 > -2 | df$vaa_v20 > -2
df$take_vaa[condition] <- 1
df[10:200, c("take_vaa", "vaa_v1", "vaa_v10", "vaa_v20")]

## Did respondents finish the VAA
df$complete_vaa <- 0
df$complete_vaa[df$vaa_v20 > -2] <- 1
df[1:200, c("complete_vaa", "vaa_v20")]



## Gender
df$v76 <- as.character(df$v76)


## Education
df$v75_2[df$v75_2 > 50 | df$v75_2 < 10] <- NA # excluding outliers and unrealistic values   




## Country
df$country <- as.factor(df$country)






## moderating effect of time 

### prepare moderators
df <- as.data.frame(df)

# Convert post-electoral interview date character to Date-Time object
df$post_startdate <- dmy_hm(df$post_startdate)

# Convert Date-Time object to numeric (seconds since Unix epoch)
df$post_startdate_num <- as.numeric(df$post_startdate)

# Convert pre-electoral interview date character to Date-Time object
df$pre_startdate <- dmy_hm(df$pre_startdate)

# Convert Date-Time object to numeric (seconds since Unix epoch)
df$pre_startdate_num <- as.numeric(df$pre_startdate)

# Create variable representing the temporal distance between the first and the second interview
df$distance_time <- df$post_startdate_num - df$pre_startdate_num # second interview date minus first interview date
df$Days_to_Quiz <- df$distance_time / 86400 # from seconds to days


# Create variable representing the temporal distance between the first interview and the end of the elections
df$Distance_to_Election <- 1717966800 - df$pre_startdate_num # election date minus start date
df$Days_to_Election <- df$Distance_to_Election / 86400 # from seconds to days



# estimate interactions
time1 <-interflex(estimator = 'linear', Y = "Turnout", D = "VAA_Treatment", X = "Days_to_Election", Z = "country", na.rm = TRUE, theme.bw = TRUE, cex.lab = 1.2,
                       data = df)
time1


time2 <-interflex(estimator = 'linear', Y = "Voteswitching", D = "VAA_Treatment", X = "Days_to_Election", Z = "country", na.rm = TRUE, theme.bw = TRUE, cex.lab = 1.2,
                  data = df)
time2


time3 <-interflex(estimator = 'linear', Y = "Party_Knowledge", D = "VAA_Treatment", X = "Days_to_Quiz", Z = "country", na.rm = TRUE, theme.bw = TRUE, cex.lab = 1.2,
                  data = df)
time3


time4 <-interflex(estimator = 'linear', Y = "Policy_Knowledge", D = "VAA_Treatment", X = "Days_to_Quiz", Z = "country", na.rm = TRUE, theme.bw = TRUE, cex.lab = 1.2,
                  data = df)
time4




# estimate interactions for age for Appendix A8
h1_age <- lm(voted ~ treated + country + age + treated*age, data = df)
summary(h1_age)
h2_age  <- lm(voteswitch ~ treated + country + age + treated*age, data = df)
summary(h2_age)
h3_1_age <- lm(party_know ~ treated + country + age + treated*age, data = df)
summary(h3_1_age)
h3_2_age <- lm(policy_know ~ treated + country + age + treated*age, data = df)
summary(h3_2_age)


## moderating effect of education
df$Age_at_final_Degree <- df$v75_2



# estimate interactions
educ1 <-interflex(estimator = 'linear', Y = "Turnout", D = "VAA_Treatment", X = "Age_at_final_Degree", Z = "country", na.rm = TRUE, theme.bw = TRUE, cex.lab = 1.2,
                  data = df)
educ1


educ2 <-interflex(estimator = 'linear', Y = "Voteswitching", D = "VAA_Treatment", X = "Age_at_final_Degree", Z = "country", na.rm = TRUE, theme.bw = TRUE, cex.lab = 1.2,
                  data = df)
educ2


educ3 <-interflex(estimator = 'linear', Y = "Party_Knowledge", D = "VAA_Treatment", X = "Age_at_final_Degree", Z = "country", na.rm = TRUE, theme.bw = TRUE, cex.lab = 1.2,
                  data = df)
educ3


educ4 <-interflex(estimator = 'linear', Y = "Policy_Knowledge", D = "VAA_Treatment", X = "Age_at_final_Degree", Z = "country", na.rm = TRUE, theme.bw = TRUE, cex.lab = 1.2,
                  data = df)
educ4






## moderating effect of EU support
df$v44[df$v44 == 99] <- NA
df$EU_Support <- df$v44
df$EU_Support <- as.numeric(df$EU_Support)


# estimate interactions
eu1 <-interflex(estimator = 'linear', Y = "Turnout", D = "VAA_Treatment", X = "EU_Support", Z = "country", na.rm = TRUE, theme.bw = TRUE, cex.lab = 1.2,
                  data = df)
eu1


eu2 <-interflex(estimator = 'linear', Y = "Voteswitching", D = "VAA_Treatment", X = "EU_Support", Z = "country", na.rm = TRUE, theme.bw = TRUE, cex.lab = 1.2,
                  data = df)
eu2


eu3 <-interflex(estimator = 'linear', Y = "Party_Knowledge", D = "VAA_Treatment", X = "EU_Support", Z = "country", na.rm = TRUE, theme.bw = TRUE, cex.lab = 1.2,
                  data = df)
eu3


eu4 <-interflex(estimator = 'linear', Y = "Policy_Knowledge", D = "VAA_Treatment", X = "EU_Support", Z = "country", na.rm = TRUE, theme.bw = TRUE, cex.lab = 1.2,
                  data = df)
eu4









### non linear interactions: 

## moderating effect of time 
# estimate interactions
time5 <-interflex(estimator = 'kernel', Y = "Turnout", D = "VAA_Treatment", X = "Days_to_Election", Z = "country", na.rm = TRUE, theme.bw = TRUE, cex.lab = 1.2,
                  data = df)
time5


time6 <-interflex(estimator = 'kernel', Y = "Voteswitching", D = "VAA_Treatment", X = "Days_to_Election", Z = "country", na.rm = TRUE, theme.bw = TRUE, cex.lab = 1.2,
                  data = df)
time6


time7 <-interflex(estimator = 'kernel', Y = "Party_Knowledge", D = "VAA_Treatment", X = "Days_to_Quiz", Z = "country", na.rm = TRUE, theme.bw = TRUE, cex.lab = 1.2,
                  data = df)
time7


time8 <-interflex(estimator = 'kernel', Y = "Policy_Knowledge", D = "VAA_Treatment", X = "Days_to_Quiz", Z = "country", na.rm = TRUE, theme.bw = TRUE, cex.lab = 1.2,
                  data = df)
time8






## moderating effect of education
# estimate interactions
educ5 <-interflex(estimator = 'kernel', Y = "Turnout", D = "VAA_Treatment", X = "Age_at_final_Degree", Z = "country", na.rm = TRUE, theme.bw = TRUE, cex.lab = 1.2,
                  data = df)
educ5


educ6 <-interflex(estimator = 'kernel', Y = "Voteswitching", D = "VAA_Treatment", X = "Age_at_final_Degree", Z = "country", na.rm = TRUE, theme.bw = TRUE, cex.lab = 1.2,
                  data = df)
educ6


educ7 <-interflex(estimator = 'kernel', Y = "Party_Knowledge", D = "VAA_Treatment", X = "Age_at_final_Degree", Z = "country", na.rm = TRUE, theme.bw = TRUE, cex.lab = 1.2,
                  data = df)
educ7


educ8 <-interflex(estimator = 'kernel', Y = "Policy_Knowledge", D = "VAA_Treatment", X = "Age_at_final_Degree", Z = "country", na.rm = TRUE, theme.bw = TRUE, cex.lab = 1.2,
                  data = df)
educ8






## moderating effect of EU support
# estimate interactions
eu5 <-interflex(estimator = 'kernel', Y = "Turnout", D = "VAA_Treatment", X = "EU_Support", Z = "country", na.rm = TRUE, theme.bw = TRUE, cex.lab = 1.2,
                data = df)
eu5


eu6 <-interflex(estimator = 'kernel', Y = "Voteswitching", D = "VAA_Treatment", X = "EU_Support", Z = "country", na.rm = TRUE, theme.bw = TRUE, cex.lab = 1.2,
                data = df)
eu6


eu7 <-interflex(estimator = 'kernel', Y = "Party_Knowledge", D = "VAA_Treatment", X = "EU_Support", Z = "country", na.rm = TRUE, theme.bw = TRUE, cex.lab = 1.2,
                data = df)
eu7


eu8 <-interflex(estimator = 'kernel', Y = "Policy_Knowledge", D = "VAA_Treatment", X = "EU_Support", Z = "country", na.rm = TRUE, theme.bw = TRUE, cex.lab = 1.2,
                data = df)
eu8






















## heterogeneity across countries

fr <- df %>% 
  filter(country == "1")

de <- df %>% 
  filter(country == "2")

it <- df %>% 
  filter(country == "3")


# estimate ITTs for France

h1_itt_fr <- lm(voted ~ treated, data = fr)
h1_itt_c_fr <- lm(voted ~ treated + age + v76 + v75_2, data = fr)

h2_itt_fr <- lm(voteswitch ~ treated, data = fr)
h2_itt_c_fr <- lm(voteswitch ~ treated + age + v76 + v75_2 , data = fr)

h3_1_itt_fr <- lm(party_know ~ treated, data = fr)
h3_1_itt_c_fr <- lm(party_know ~ treated + age + v76 + v75_2 , data = fr)

h3_2_itt_fr <- lm(policy_know ~ treated, data = fr)
h3_2_itt_c_fr <- lm(policy_know ~ treated + age + v76 + v75_2 , data = fr)


### Extracting coefficients and confidence intervals for "treated"

# For h1_itt
coef_h1_itt_fr <- coef(h1_itt_fr)["treated"]
conf_h1_itt_fr <- confint(h1_itt_fr)["treated", ]
upper_h1_itt_fr <- conf_h1_itt_fr[2]
lower_h1_itt_fr <- conf_h1_itt_fr[1]

# For h1_itt_c
coef_h1_itt_c_fr <- coef(h1_itt_c_fr)["treated"]
conf_h1_itt_c_fr <- confint(h1_itt_c_fr)["treated", ]
upper_h1_itt_c_fr <- conf_h1_itt_c_fr[2]
lower_h1_itt_c_fr <- conf_h1_itt_c_fr[1]

# For h2_itt
coef_h2_itt_fr <- coef(h2_itt_fr)["treated"]
conf_h2_itt_fr <- confint(h2_itt_fr)["treated", ]
upper_h2_itt_fr <- conf_h2_itt_fr[2]
lower_h2_itt_fr <- conf_h2_itt_fr[1]

# For h2_itt_c
coef_h2_itt_c_fr <- coef(h2_itt_c_fr)["treated"]
conf_h2_itt_c_fr <- confint(h2_itt_c_fr)["treated", ]
upper_h2_itt_c_fr <- conf_h2_itt_c_fr[2]
lower_h2_itt_c_fr <- conf_h2_itt_c_fr[1]

# For h3_1_itt
coef_h3_1_itt_fr <- coef(h3_1_itt_fr)["treated"]
conf_h3_1_itt_fr <- confint(h3_1_itt_fr)["treated", ]
upper_h3_1_itt_fr <- conf_h3_1_itt_fr[2]
lower_h3_1_itt_fr <- conf_h3_1_itt_fr[1]

# For h3_1_itt_c
coef_h3_1_itt_c_fr <- coef(h3_1_itt_c_fr)["treated"]
conf_h3_1_itt_c_fr <- confint(h3_1_itt_c_fr)["treated", ]
upper_h3_1_itt_c_fr <- conf_h3_1_itt_c_fr[2]
lower_h3_1_itt_c_fr <- conf_h3_1_itt_c_fr[1]

# For h3_2_itt
coef_h3_2_itt_fr <- coef(h3_2_itt_fr)["treated"]
conf_h3_2_itt_fr <- confint(h3_2_itt_fr)["treated", ]
upper_h3_2_itt_fr <- conf_h3_2_itt_fr[2]
lower_h3_2_itt_fr <- conf_h3_2_itt_fr[1]

# For h3_2_itt_c
coef_h3_2_itt_c_fr <- coef(h3_2_itt_c_fr)["treated"]
conf_h3_2_itt_c_fr <- confint(h3_2_itt_c_fr)["treated", ]
upper_h3_2_itt_c_fr <- conf_h3_2_itt_c_fr[2]
lower_h3_2_itt_c_fr <- conf_h3_2_itt_c_fr[1]






# estimate ITTs for Germany

h1_itt_de <- lm(voted ~ treated, data = de)
h1_itt_c_de <- lm(voted ~ treated + age + v76 + v75_2, data = de)

h2_itt_de <- lm(voteswitch ~ treated, data = de)
h2_itt_c_de <- lm(voteswitch ~ treated + age + v76 + v75_2 , data = de)

h3_1_itt_de <- lm(party_know ~ treated, data = de)
h3_1_itt_c_de <- lm(party_know ~ treated + age + v76 + v75_2 , data = de)

h3_2_itt_de <- lm(policy_know ~ treated, data = de)
h3_2_itt_c_de <- lm(policy_know ~ treated + age + v76 + v75_2 , data = de)


### Extracting coefficients and confidence intervals for "treated"

# For h1_itt
coef_h1_itt_de <- coef(h1_itt_de)["treated"]
conf_h1_itt_de <- confint(h1_itt_de)["treated", ]
upper_h1_itt_de <- conf_h1_itt_de[2]
lower_h1_itt_de <- conf_h1_itt_de[1]

# For h1_itt_c
coef_h1_itt_c_de <- coef(h1_itt_c_de)["treated"]
conf_h1_itt_c_de <- confint(h1_itt_c_de)["treated", ]
upper_h1_itt_c_de <- conf_h1_itt_c_de[2]
lower_h1_itt_c_de <- conf_h1_itt_c_de[1]

# For h2_itt
coef_h2_itt_de <- coef(h2_itt_de)["treated"]
conf_h2_itt_de <- confint(h2_itt_de)["treated", ]
upper_h2_itt_de <- conf_h2_itt_de[2]
lower_h2_itt_de <- conf_h2_itt_de[1]

# For h2_itt_c
coef_h2_itt_c_de <- coef(h2_itt_c_de)["treated"]
conf_h2_itt_c_de <- confint(h2_itt_c_de)["treated", ]
upper_h2_itt_c_de <- conf_h2_itt_c_de[2]
lower_h2_itt_c_de <- conf_h2_itt_c_de[1]

# For h3_1_itt
coef_h3_1_itt_de <- coef(h3_1_itt_de)["treated"]
conf_h3_1_itt_de <- confint(h3_1_itt_de)["treated", ]
upper_h3_1_itt_de <- conf_h3_1_itt_de[2]
lower_h3_1_itt_de <- conf_h3_1_itt_de[1]

# For h3_1_itt_c
coef_h3_1_itt_c_de <- coef(h3_1_itt_c_de)["treated"]
conf_h3_1_itt_c_de <- confint(h3_1_itt_c_de)["treated", ]
upper_h3_1_itt_c_de <- conf_h3_1_itt_c_de[2]
lower_h3_1_itt_c_de <- conf_h3_1_itt_c_de[1]

# For h3_2_itt
coef_h3_2_itt_de <- coef(h3_2_itt_de)["treated"]
conf_h3_2_itt_de <- confint(h3_2_itt_de)["treated", ]
upper_h3_2_itt_de <- conf_h3_2_itt_de[2]
lower_h3_2_itt_de <- conf_h3_2_itt_de[1]

# For h3_2_itt_c
coef_h3_2_itt_c_de <- coef(h3_2_itt_c_de)["treated"]
conf_h3_2_itt_c_de <- confint(h3_2_itt_c_de)["treated", ]
upper_h3_2_itt_c_de <- conf_h3_2_itt_c_de[2]
lower_h3_2_itt_c_de <- conf_h3_2_itt_c_de[1]






# estimate ITTs for Italy

h1_itt_it <- lm(voted ~ treated, data = it)
h1_itt_c_it <- lm(voted ~ treated + age + v76 + v75_2, data = it)

h2_itt_it <- lm(voteswitch ~ treated, data = it)
h2_itt_c_it <- lm(voteswitch ~ treated + age + v76 + v75_2 , data = it)

h3_1_itt_it <- lm(party_know ~ treated, data = it)
h3_1_itt_c_it <- lm(party_know ~ treated + age + v76 + v75_2 , data = it)

h3_2_itt_it <- lm(policy_know ~ treated, data = it)
h3_2_itt_c_it <- lm(policy_know ~ treated + age + v76 + v75_2 , data = it)


### Extracting coefficients and confidence intervals for "treated"

# For h1_itt
coef_h1_itt_it <- coef(h1_itt_it)["treated"]
conf_h1_itt_it <- confint(h1_itt_it)["treated", ]
upper_h1_itt_it <- conf_h1_itt_it[2]
lower_h1_itt_it <- conf_h1_itt_it[1]

# For h1_itt_c
coef_h1_itt_c_it <- coef(h1_itt_c_it)["treated"]
conf_h1_itt_c_it <- confint(h1_itt_c_it)["treated", ]
upper_h1_itt_c_it <- conf_h1_itt_c_it[2]
lower_h1_itt_c_it <- conf_h1_itt_c_it[1]

# For h2_itt
coef_h2_itt_it <- coef(h2_itt_it)["treated"]
conf_h2_itt_it <- confint(h2_itt_it)["treated", ]
upper_h2_itt_it <- conf_h2_itt_it[2]
lower_h2_itt_it <- conf_h2_itt_it[1]

# For h2_itt_c
coef_h2_itt_c_it <- coef(h2_itt_c_it)["treated"]
conf_h2_itt_c_it <- confint(h2_itt_c_it)["treated", ]
upper_h2_itt_c_it <- conf_h2_itt_c_it[2]
lower_h2_itt_c_it <- conf_h2_itt_c_it[1]

# For h3_1_itt
coef_h3_1_itt_it <- coef(h3_1_itt_it)["treated"]
conf_h3_1_itt_it <- confint(h3_1_itt_it)["treated", ]
upper_h3_1_itt_it <- conf_h3_1_itt_it[2]
lower_h3_1_itt_it <- conf_h3_1_itt_it[1]

# For h3_1_itt_c
coef_h3_1_itt_c_it <- coef(h3_1_itt_c_it)["treated"]
conf_h3_1_itt_c_it <- confint(h3_1_itt_c_it)["treated", ]
upper_h3_1_itt_c_it <- conf_h3_1_itt_c_it[2]
lower_h3_1_itt_c_it <- conf_h3_1_itt_c_it[1]

# For h3_2_itt
coef_h3_2_itt_it <- coef(h3_2_itt_it)["treated"]
conf_h3_2_itt_it <- confint(h3_2_itt_it)["treated", ]
upper_h3_2_itt_it <- conf_h3_2_itt_it[2]
lower_h3_2_itt_it <- conf_h3_2_itt_it[1]

# For h3_2_itt_c
coef_h3_2_itt_c_it <- coef(h3_2_itt_c_it)["treated"]
conf_h3_2_itt_c_it <- confint(h3_2_itt_c_it)["treated", ]
upper_h3_2_itt_c_it <- conf_h3_2_itt_c_it[2]
lower_h3_2_itt_c_it <- conf_h3_2_itt_c_it[1]




model_names <- c("h1_itt_fr", "h1_itt_c_fr", "h2_itt_fr", "h2_itt_c_fr", "h3_1_itt_fr", "h3_1_itt_c_fr", "h3_2_itt_fr", "h3_2_itt_c_fr",
                 "h1_itt_de", "h1_itt_c_de", "h2_itt_de", "h2_itt_c_de", "h3_1_itt_de", "h3_1_itt_c_de", "h3_2_itt_de", "h3_2_itt_c_de",
                 "h1_itt_it", "h1_itt_c_it", "h2_itt_it", "h2_itt_c_it", "h3_1_itt_it", "h3_1_itt_c_it", "h3_2_itt_it", "h3_2_itt_c_it")
 
coefficients <- c(coef_h1_itt_fr, coef_h1_itt_c_fr, coef_h2_itt_fr, coef_h2_itt_c_fr, coef_h3_1_itt_fr, coef_h3_1_itt_c_fr, coef_h3_2_itt_fr, coef_h3_2_itt_c_fr,
                  coef_h1_itt_de, coef_h1_itt_c_de, coef_h2_itt_de, coef_h2_itt_c_de, coef_h3_1_itt_de, coef_h3_1_itt_c_de, coef_h3_2_itt_de, coef_h3_2_itt_c_de,
                  coef_h1_itt_it, coef_h1_itt_c_it, coef_h2_itt_it, coef_h2_itt_c_it, coef_h3_1_itt_it, coef_h3_1_itt_c_it, coef_h3_2_itt_it, coef_h3_2_itt_c_it)
                  
lower_ci <- c(lower_h1_itt_fr, lower_h1_itt_c_fr, lower_h2_itt_fr, lower_h2_itt_c_fr, lower_h3_1_itt_fr, lower_h3_1_itt_c_fr, lower_h3_2_itt_fr, lower_h3_2_itt_c_fr,
              lower_h1_itt_de, lower_h1_itt_c_de, lower_h2_itt_de, lower_h2_itt_c_de, lower_h3_1_itt_de, lower_h3_1_itt_c_de, lower_h3_2_itt_de, lower_h3_2_itt_c_de,
              lower_h1_itt_it, lower_h1_itt_c_it, lower_h2_itt_it, lower_h2_itt_c_it, lower_h3_1_itt_it, lower_h3_1_itt_c_it, lower_h3_2_itt_it, lower_h3_2_itt_c_it)

upper_ci <- c(upper_h1_itt_fr, upper_h1_itt_c_fr, upper_h2_itt_fr, upper_h2_itt_c_fr, upper_h3_1_itt_fr, upper_h3_1_itt_c_fr, upper_h3_2_itt_fr, upper_h3_2_itt_c_fr,
              upper_h1_itt_de, upper_h1_itt_c_de, upper_h2_itt_de, upper_h2_itt_c_de, upper_h3_1_itt_de, upper_h3_1_itt_c_de, upper_h3_2_itt_de, upper_h3_2_itt_c_de,
              upper_h1_itt_it, upper_h1_itt_c_it, upper_h2_itt_it, upper_h2_itt_c_it, upper_h3_1_itt_it, upper_h3_1_itt_c_it, upper_h3_2_itt_it, upper_h3_2_itt_c_it)

outcome <- c("H1 Turnout", "H1 Turnout", "H2 Vote Switching", "H2 Vote Switching", "H3 Party Knowledge", "H3 Party Knowledge", "H3 Policy Knowledge", "H3 Policy Knowledge",
             "H1 Turnout", "H1 Turnout", "H2 Vote Switching", "H2 Vote Switching", "H3 Party Knowledge", "H3 Party Knowledge", "H3 Policy Knowledge", "H3 Policy Knowledge",
             "H1 Turnout", "H1 Turnout", "H2 Vote Switching", "H2 Vote Switching", "H3 Party Knowledge", "H3 Party Knowledge", "H3 Policy Knowledge", "H3 Policy Knowledge")

controls <- c("No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes",
              "No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes",
              "No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes")

country <- c("France", "France", "France", "France", "France", "France", "France", "France",
              "Germany", "Germany", "Germany", "Germany", "Germany", "Germany", "Germany", "Germany",
              "Italy", "Italy", "Italy", "Italy", "Italy", "Italy", "Italy", "Italy")



# Creating the dataframe
results_countries <- data.frame(
  Model = model_names,
  Coefficient = coefficients,
  Lower_CI = lower_ci,
  Upper_CI = upper_ci,
  Outcome = outcome,
  Controls = controls,
  Country = country
)



# plot coefficients for France

results_france <- results_countries %>% 
  filter(Country == "France")

resultplot_france <- ggplot(results_france, aes(x = Outcome, y = Coefficient, shape = Controls)) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 1) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), size = 2, width = 0, color = "blue", position = position_dodge(width = 0.5)) +
  geom_point(size = 8, color = "blue", position = position_dodge(width = 0.5)) +
  theme_light(base_size = 20) +
  xlab("") +
  ylab("Effect of VAA Usage in France") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  scale_y_continuous(limits = c(-0.2, 0.2), breaks = seq(-0.2, 0.2, by = 0.05)) 

resultplot_france



# plot coefficients for Germany

results_germany <- results_countries %>% 
  filter(Country == "Germany")

resultplot_germany <- ggplot(results_germany, aes(x = Outcome, y = Coefficient, shape = Controls)) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 1) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), size = 2, width = 0, color = "black", position = position_dodge(width = 0.5)) +
  geom_point(size = 8, color = "black", position = position_dodge(width = 0.5)) +
  theme_light(base_size = 20) +
  xlab("") +
  ylab("Effect of VAA Usage in Germany") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  scale_y_continuous(limits = c(-0.2, 0.2), breaks = seq(-0.2, 0.2, by = 0.05)) 


resultplot_germany


# plot coefficients for Italy

results_italy <- results_countries %>% 
  filter(Country == "Italy")

resultplot_italy <- ggplot(results_italy, aes(x = Outcome, y = Coefficient, shape = Controls)) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 1) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), size = 2, width = 0, color = "green3", position = position_dodge(width = 0.5)) +
  geom_point(size = 8, color = "green3", position = position_dodge(width = 0.5)) +
  theme_light(base_size = 20) +
  xlab("") +
  ylab("Effect of VAA Usage in Italy") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  scale_y_continuous(limits = c(-0.2, 0.2), breaks = seq(-0.2, 0.2, by = 0.05)) 


resultplot_italy


ggarrange(resultplot_france, resultplot_germany, resultplot_italy, nrow = 1)


