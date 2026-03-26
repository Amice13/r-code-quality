# load data
load("df.RData")

# create variables for analyses

## Treatment
df$treated <- NA
df$treated[df$vaagroup == 2] <- 1
df$treated[df$vaagroup == 1] <- 0

## Are you interested in learning about party positions ahead of elections?
df$interested <- NA
df$interested[df$v93 == 2] <- 0
df$interested[df$v93 == 1] <- 1


## Which of the following statements best describe you? 1-3 didnt vote, 4 voted, 5 dont know
df$voted <- NA
df$voted[df$v104 == 1 | df$v104 == 2 | df$v104 == 3] <- 0
df$voted[df$v104 == 4] <- 1


## Vote switch
df$v12 <- as.numeric(df$v12) # pre-electoral vote intention
df$v105 <- as.numeric(df$v105) # post-electoral vote choice
df$voteswitch <- NA
df$voteswitch[df$v105 == df$v12] <- 0
df$voteswitch[df$v105 != df$v12] <- 1
df[1:200, c("v105", "v12", "voteswitch")]


## Vote switch 
df$v12 <- as.numeric(df$v12) # pre-electoral vote intention
df$v105 <- as.numeric(df$v105) # post-electoral vote choice
df$voteswitch <- NA
df$voteswitch[df$v105 == df$v12] <- 0
df$voteswitch[df$v105 != df$v12] <- 1
df$voteswitch[df$v12 > 7] <- NA
df$voteswitch[df$v105 > 7] <- NA
df[1:200, c("v105", "v12", "voteswitch")]

## check voteswitch proportions by treatment status
df %>%
  group_by(treated) %>%
  summarise(proportion_voteswitch = mean(voteswitch == 1, na.rm = TRUE)) # ~18% per group




## Knowledge 1: Which of the following are European party groups in the European Parliament? Select all that apply
df$party_know <- (df$v112_1 + df$v112_2 + df$v112_5) - (df$v112_3 + df$v112_4 + df$v112_6)
df[1:200, c("party_know", "v112_1", "v112_2", "v112_3", "v112_4", "v112_5", "v112_6")]


## Knowledge 2: Which of the following issues has the European Parliament voted on in the last 5 years? Select all that apply
df$policy_know <- (df$v113_1 + df$v113_3 + df$v113_5) - (df$v113_2 + df$v113_4 + df$v113_6)
df[1:200, c("policy_know", "v113_1", "v113_2", "v113_3", "v113_4", "v113_5", "v113_6")]


## Did respondents start the VAA
df$take_vaa <- NA
df$take_vaa <- apply(df[, paste0("vaa_v", 1:20)], 1, function(x) any(x > -1, na.rm = TRUE))
df$take_vaa <- ifelse(df$take_vaa, 1, 0)

df[10:200, c("take_vaa", "vaa_v1", "vaa_v10", "vaa_v20")]


## Did respondents finish the VAA
df$complete_vaa <- NA
df$complete_vaa <- ifelse(
  df$vaa_v20 > -1 | 
    (is.na(df$vaa_v20) & apply(df[, paste0("vaa_v", 1:19)], 1, function(x) any(x > -1, na.rm = TRUE))),
  1,
  0
)
df$complete_vaa[apply(df[, paste0("vaa_v", 1:20)], 1, function(x) all(is.na(x)))] <- 0

df[10:200, c("complete_vaa", "vaa_v20", "vaa_v1")]


## Gender
df$v76 <- as.character(df$v76)


## Education
df$v75_2[df$v75_2 > 50 | df$v75_2 < 10] <- NA # excluding outliers and unrealistic values


## Country
df$country <- as.character(df$country)




## Check compliance rate
# filter only treated respondents
df_treat <- df %>% 
  filter(treated == 1)

df_control <- df %>% 
  filter(treated == 0)

# soft compliance
table(df_treat$take_vaa) # ~40% compliance

# strict compliance
table(df_treat$complete_vaa) # ~34% compliance





# estimate ITTs

h1_itt <- lm(voted ~ treated + country, data = df)
h1_itt_c <- lm(voted ~ treated + age + v76 + v75_2 + country, data = df)

h2_itt <- lm(voteswitch ~ treated + country, data = df)
h2_itt_c <- lm(voteswitch ~ treated + age + v76 + v75_2 + country, data = df)

h3_1_itt <- lm(party_know ~ treated + country, data = df)
h3_1_itt_c <- lm(party_know ~ treated + age + v76 + v75_2 + country, data = df)

h3_2_itt <- lm(policy_know ~ treated + country, data = df)
h3_2_itt_c <- lm(policy_know ~ treated + age + v76 + v75_2 + country, data = df)

stargazer(h1_itt, h1_itt_c, h2_itt, h2_itt_c)

stargazer(h3_1_itt, h3_1_itt_c, h3_2_itt, h3_2_itt_c)


### Extracting coefficients and confidence intervals for "treated"

# For h1_itt
coef_h1_itt <- coef(h1_itt)["treated"]
conf_h1_itt <- confint(h1_itt)["treated", ]
upper_h1_itt <- conf_h1_itt[2]
lower_h1_itt <- conf_h1_itt[1]

# For h1_itt_c
coef_h1_itt_c <- coef(h1_itt_c)["treated"]
conf_h1_itt_c <- confint(h1_itt_c)["treated", ]
upper_h1_itt_c <- conf_h1_itt_c[2]
lower_h1_itt_c <- conf_h1_itt_c[1]

# For h2_itt
coef_h2_itt <- coef(h2_itt)["treated"]
conf_h2_itt <- confint(h2_itt)["treated", ]
upper_h2_itt <- conf_h2_itt[2]
lower_h2_itt <- conf_h2_itt[1]

# For h2_itt_c
coef_h2_itt_c <- coef(h2_itt_c)["treated"]
conf_h2_itt_c <- confint(h2_itt_c)["treated", ]
upper_h2_itt_c <- conf_h2_itt_c[2]
lower_h2_itt_c <- conf_h2_itt_c[1]

# For h3_1_itt
coef_h3_1_itt <- coef(h3_1_itt)["treated"]
conf_h3_1_itt <- confint(h3_1_itt)["treated", ]
upper_h3_1_itt <- conf_h3_1_itt[2]
lower_h3_1_itt <- conf_h3_1_itt[1]

# For h3_1_itt_c
coef_h3_1_itt_c <- coef(h3_1_itt_c)["treated"]
conf_h3_1_itt_c <- confint(h3_1_itt_c)["treated", ]
upper_h3_1_itt_c <- conf_h3_1_itt_c[2]
lower_h3_1_itt_c <- conf_h3_1_itt_c[1]

# For h3_2_itt
coef_h3_2_itt <- coef(h3_2_itt)["treated"]
conf_h3_2_itt <- confint(h3_2_itt)["treated", ]
upper_h3_2_itt <- conf_h3_2_itt[2]
lower_h3_2_itt <- conf_h3_2_itt[1]

# For h3_2_itt_c
coef_h3_2_itt_c <- coef(h3_2_itt_c)["treated"]
conf_h3_2_itt_c <- confint(h3_2_itt_c)["treated", ]
upper_h3_2_itt_c <- conf_h3_2_itt_c[2]
lower_h3_2_itt_c <- conf_h3_2_itt_c[1]



# estimate LATEs with 2SLS


## soft LATE
h1_late_1 <- ivreg(voted ~ take_vaa  + country| treated + country, data = df)
h1_late_1_c <- ivreg(voted ~ take_vaa  + age + v76 + v75_2 + country| treated + age + v76 + v75_2 + country, data = df)

h2_late_1 <- ivreg(voteswitch ~ take_vaa   + country| treated + country, data = df)
h2_late_1_c <- ivreg(voteswitch ~ take_vaa  + age + v76 + v75_2 + country| treated + age + v76 + v75_2 + country, data = df)

h3_1_late_1 <- ivreg(party_know ~ take_vaa + country| treated + country, data = df)
h3_1_late_1_c <- ivreg(party_know ~ take_vaa  + age + v76 + v75_2 + country| treated + age + v76 + v75_2 + country, data = df)

h3_2_late_1 <- ivreg(policy_know ~ take_vaa + country| treated + country, data = df)
h3_2_late_1_c <- ivreg(policy_know ~ take_vaa  + age + v76 + v75_2 + country| treated + age + v76 + v75_2 + country, data = df)

### Extracting coefficients and confidence intervals for "take_vaa"

# For h1_late_1
coef_h1_late_1 <- coef(h1_late_1)["take_vaa"]
conf_h1_late_1 <- confint(h1_late_1)["take_vaa", ]
upper_h1_late_1 <- conf_h1_late_1[2]
lower_h1_late_1 <- conf_h1_late_1[1]

# For h1_late_1_c
coef_h1_late_1_c <- coef(h1_late_1_c)["take_vaa"]
conf_h1_late_1_c <- confint(h1_late_1_c)["take_vaa", ]
upper_h1_late_1_c <- conf_h1_late_1_c[2]
lower_h1_late_1_c <- conf_h1_late_1_c[1]

# For h2_late_1
coef_h2_late_1 <- coef(h2_late_1)["take_vaa"]
conf_h2_late_1 <- confint(h2_late_1)["take_vaa", ]
upper_h2_late_1 <- conf_h2_late_1[2]
lower_h2_late_1 <- conf_h2_late_1[1]

# For h2_late_1_c
coef_h2_late_1_c <- coef(h2_late_1_c)["take_vaa"]
conf_h2_late_1_c <- confint(h2_late_1_c)["take_vaa", ]
upper_h2_late_1_c <- conf_h2_late_1_c[2]
lower_h2_late_1_c <- conf_h2_late_1_c[1]

# For h3_1_late_1
coef_h3_1_late_1 <- coef(h3_1_late_1)["take_vaa"]
conf_h3_1_late_1 <- confint(h3_1_late_1)["take_vaa", ]
upper_h3_1_late_1 <- conf_h3_1_late_1[2]
lower_h3_1_late_1 <- conf_h3_1_late_1[1]

# For h3_1_late_1_c
coef_h3_1_late_1_c <- coef(h3_1_late_1_c)["take_vaa"]
conf_h3_1_late_1_c <- confint(h3_1_late_1_c)["take_vaa", ]
upper_h3_1_late_1_c <- conf_h3_1_late_1_c[2]
lower_h3_1_late_1_c <- conf_h3_1_late_1_c[1]

# For h3_2_late_1
coef_h3_2_late_1 <- coef(h3_2_late_1)["take_vaa"]
conf_h3_2_late_1 <- confint(h3_2_late_1)["take_vaa", ]
upper_h3_2_late_1 <- conf_h3_2_late_1[2]
lower_h3_2_late_1 <- conf_h3_2_late_1[1]

# For h3_2_late_1_c
coef_h3_2_late_1_c <- coef(h3_2_late_1_c)["take_vaa"]
conf_h3_2_late_1_c <- confint(h3_2_late_1_c)["take_vaa", ]
upper_h3_2_late_1_c <- conf_h3_2_late_1_c[2]
lower_h3_2_late_1_c <- conf_h3_2_late_1_c[1]




## strict LATE
h1_late_2 <- ivreg(voted ~ complete_vaa  + country| treated + country, data = df)
h1_late_2_c <- ivreg(voted ~ complete_vaa  + age + v76 + v75_2 + country| treated + age + v76 + v75_2 + country, data = df)

h2_late_2 <- ivreg(voteswitch ~ complete_vaa   + country| treated + country, data = df)
h2_late_2_c <- ivreg(voteswitch ~ complete_vaa  + age + v76 + v75_2 + country| treated + age + v76 + v75_2 + country, data = df)

h3_1_late_2 <- ivreg(party_know ~ complete_vaa  + country | treated + country, data = df)
h3_1_late_2_c <- ivreg(party_know ~ complete_vaa  + age + v76 + v75_2 + country| treated + age + v76 + v75_2 + country, data = df)

h3_2_late_2 <- ivreg(policy_know ~ complete_vaa  + country| treated + country, data = df)
h3_2_late_2_c <- ivreg(policy_know ~ complete_vaa  + age + v76 + v75_2 + country| treated + age + v76 + v75_2 + country, data = df)


### Extracting coefficients and confidence intervals for "complete_vaa"

# For h1_late_2
coef_h1_late_2 <- coef(h1_late_2)["complete_vaa"]
conf_h1_late_2 <- confint(h1_late_2)["complete_vaa", ]
upper_h1_late_2 <- conf_h1_late_2[2]
lower_h1_late_2 <- conf_h1_late_2[1]

# For h1_late_2_c
coef_h1_late_2_c <- coef(h1_late_2_c)["complete_vaa"]
conf_h1_late_2_c <- confint(h1_late_2_c)["complete_vaa", ]
upper_h1_late_2_c <- conf_h1_late_2_c[2]
lower_h1_late_2_c <- conf_h1_late_2_c[1]

# For h2_late_2
coef_h2_late_2 <- coef(h2_late_2)["complete_vaa"]
conf_h2_late_2 <- confint(h2_late_2)["complete_vaa", ]
upper_h2_late_2 <- conf_h2_late_2[2]
lower_h2_late_2 <- conf_h2_late_2[1]

# For h2_late_2_c
coef_h2_late_2_c <- coef(h2_late_2_c)["complete_vaa"]
conf_h2_late_2_c <- confint(h2_late_2_c)["complete_vaa", ]
upper_h2_late_2_c <- conf_h2_late_2_c[2]
lower_h2_late_2_c <- conf_h2_late_2_c[1]

# For h3_1_late_2
coef_h3_1_late_2 <- coef(h3_1_late_2)["complete_vaa"]
conf_h3_1_late_2 <- confint(h3_1_late_2)["complete_vaa", ]
upper_h3_1_late_2 <- conf_h3_1_late_2[2]
lower_h3_1_late_2 <- conf_h3_1_late_2[1]

# For h3_1_late_2_c
coef_h3_1_late_2_c <- coef(h3_1_late_2_c)["complete_vaa"]
conf_h3_1_late_2_c <- confint(h3_1_late_2_c)["complete_vaa", ]
upper_h3_1_late_2_c <- conf_h3_1_late_2_c[2]
lower_h3_1_late_2_c <- conf_h3_1_late_2_c[1]

# For h3_2_late_2
coef_h3_2_late_2 <- coef(h3_2_late_2)["complete_vaa"]
conf_h3_2_late_2 <- confint(h3_2_late_2)["complete_vaa", ]
upper_h3_2_late_2 <- conf_h3_2_late_2[2]
lower_h3_2_late_2 <- conf_h3_2_late_2[1]

# For h3_2_late_2_c
coef_h3_2_late_2_c <- coef(h3_2_late_2_c)["complete_vaa"]
conf_h3_2_late_2_c <- confint(h3_2_late_2_c)["complete_vaa", ]
upper_h3_2_late_2_c <- conf_h3_2_late_2_c[2]
lower_h3_2_late_2_c <- conf_h3_2_late_2_c[1]






model_names <- c("h1_itt", "h1_itt_c", "h2_itt", "h2_itt_c", "h3_1_itt", "h3_1_itt_c", "h3_2_itt", "h3_2_itt_c",
                 "h1_late_1", "h1_late_1_c", "h2_late_1", "h2_late_1_c", "h3_1_late_1", "h3_1_late_1_c", "h3_2_late_1", "h3_2_late_1_c",
                 "h1_late_2", "h1_late_2_c", "h2_late_2", "h2_late_2_c", "h3_1_late_2", "h3_1_late_2_c", "h3_2_late_2", "h3_2_late_2_c")

coefficients <- c(coef_h1_itt, coef_h1_itt_c, coef_h2_itt, coef_h2_itt_c, coef_h3_1_itt, coef_h3_1_itt_c, coef_h3_2_itt, coef_h3_2_itt_c,
                  coef_h1_late_1, coef_h1_late_1_c, coef_h2_late_1, coef_h2_late_1_c, coef_h3_1_late_1, coef_h3_1_late_1_c, coef_h3_2_late_1, coef_h3_2_late_1_c,
                  coef_h1_late_2, coef_h1_late_2_c, coef_h2_late_2, coef_h2_late_2_c, coef_h3_1_late_2, coef_h3_1_late_2_c, coef_h3_2_late_2, coef_h3_2_late_2_c)

lower_ci <- c(lower_h1_itt, lower_h1_itt_c, lower_h2_itt, lower_h2_itt_c, lower_h3_1_itt, lower_h3_1_itt_c, lower_h3_2_itt, lower_h3_2_itt_c,
              lower_h1_late_1, lower_h1_late_1_c, lower_h2_late_1, lower_h2_late_1_c, lower_h3_1_late_1, lower_h3_1_late_1_c, lower_h3_2_late_1, lower_h3_2_late_1_c,
              lower_h1_late_2, lower_h1_late_2_c, lower_h2_late_2, lower_h2_late_2_c, lower_h3_1_late_2, lower_h3_1_late_2_c, lower_h3_2_late_2, lower_h3_2_late_2_c)

upper_ci <- c(upper_h1_itt, upper_h1_itt_c, upper_h2_itt, upper_h2_itt_c, upper_h3_1_itt, upper_h3_1_itt_c, upper_h3_2_itt, upper_h3_2_itt_c,
              upper_h1_late_1, upper_h1_late_1_c, upper_h2_late_1, upper_h2_late_1_c, upper_h3_1_late_1, upper_h3_1_late_1_c, upper_h3_2_late_1, upper_h3_2_late_1_c,
              upper_h1_late_2, upper_h1_late_2_c, upper_h2_late_2, upper_h2_late_2_c, upper_h3_1_late_2, upper_h3_1_late_2_c, upper_h3_2_late_2, upper_h3_2_late_2_c)

outcome <- c("H1 Turnout", "H1 Turnout", "H2 Vote Switching", "H2 Vote Switching", "H3 Party Knowledge", "H3 Party Knowledge", "H3 Policy Knowledge", "H3 Policy Knowledge",
                "H1 Turnout", "H1 Turnout", "H2 Vote Switching", "H2 Vote Switching", "H3 Party Knowledge", "H3 Party Knowledge", "H3 Policy Knowledge", "H3 Policy Knowledge",
                "H1 Turnout", "H1 Turnout", "H2 Vote Switching", "H2 Vote Switching", "H3 Party Knowledge", "H3 Party Knowledge", "H3 Policy Knowledge", "H3 Policy Knowledge")

controls <- c("No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes",
              "No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes",
              "No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes")

estimand <- c("ITT", "ITT", "ITT", "ITT", "ITT", "ITT", "ITT", "ITT",
              "Soft CACE", "Soft CACE", "Soft CACE", "Soft CACE", "Soft CACE", "Soft CACE", "Soft CACE", "Soft CACE",
              "Strict CACE", "Strict CACE", "Strict CACE", "Strict CACE", "Strict CACE", "Strict CACE", "Strict CACE", "Strict CACE")



# Creating the dataframe
results <- data.frame(
  Model = model_names,
  Coefficient = coefficients,
  Lower_CI = lower_ci,
  Upper_CI = upper_ci,
  Outcome = outcome,
  Controls = controls,
  Estimand = estimand
)



# plot coefficients

resultplot <- ggplot(results, aes(x = Outcome, y = Coefficient, color = Estimand, shape = Controls)) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 1) +
  geom_point(size = 7, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), size = 2, width = 0.2, position = position_dodge(width = 0.5)) +
  theme_light(base_size = 25) +
  xlab("Outcome") +
  ylab("Effect of VAA Usage")

resultplot

