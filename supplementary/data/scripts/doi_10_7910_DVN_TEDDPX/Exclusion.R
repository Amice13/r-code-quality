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
df$voteswitch[df$v12 > 7] <- NA
df$voteswitch[df$v105 > 7] <- NA
df[1:200, c("v105", "v12", "voteswitch")]



## Knowledge 1: Which of the following are European party groups in the European Parliament? Select all that apply
df$party_know <- (df$v112_1 + df$v112_2 + df$v112_5) - (df$v112_3 + df$v112_4 + df$v112_6)
df[1:200, c("party_know", "v112_1", "v112_2", "v112_3", "v112_4", "v112_5", "v112_6")]


## Knowledge 2: Which of the following issues has the European Parliament voted on in the last 5 years? Select all that apply
df$policy_know <- (df$v113_1 + df$v113_3 + df$v113_5) - (df$v113_2 + df$v113_4 + df$v113_6)
df[1:200, c("policy_know", "v113_1", "v113_2", "v113_3", "v113_4", "v113_5", "v113_6")]


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
df$country <- as.character(df$country)



## Excluding respondents who failed one attention check
attention_df <- df %>% 
  filter(v14 == 1 & v49 == 1 & v118 == 1 & v140 == 1)


# estimate ITTs

att1 <- lm(voted ~ treated + country, data = attention_df)
att2 <- lm(voted ~ treated + age + v76 + v75_2 + country, data = attention_df)

att3 <- lm(voteswitch ~ treated + country, data = attention_df)
att4 <- lm(voteswitch ~ treated + age + v76 + v75_2 + country, data = attention_df)

att5 <- lm(party_know ~ treated + country, data = attention_df)
att6 <- lm(party_know ~ treated + age + v76 + v75_2 + country, data = attention_df)

att7 <- lm(policy_know ~ treated + country, data = attention_df)
att8 <- lm(policy_know ~ treated + age + v76 + v75_2 + country, data = attention_df)



stargazer(att1,att2,att3,att4)


stargazer(att5,att6,att7,att8)

