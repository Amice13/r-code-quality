#Install and load needed packages Generalized estiating equations (GEE)

install.packages("geepack")
install.packages("mlogit")

library(geepack)
library(mlogit)

#Determine datasheet
data=TGA_data_with_scores_10_2020

# Logistic regression analysis for both hemispheres separately and on all data as comparison to the gee

Log_regression_score_single_supply<-glm(Group_control_0_TGA_1 ~ score_single_supply_no_0_yes_1 + sex_female_0_male_1 + age_at_7T, family=binomial, data=data, na.action = na.omit)
summary(Log_regression_score_single_supply)

Log_regression_score_single_supply_simple<-glm(Group_control_0_TGA_1 ~ score_single_supply_no_0_yes_1, family=binomial, data=data, na.action = na.omit)
summary(Log_regression_score_single_supply_simple)

Log_regression_score_Acha_contribution<-glm(Group_control_0_TGA_1 ~ acha_ja_nein_score + sex_female_0_male_1 + age_at_7T, family=binomial, data=data, na.action = na.omit)
summary(Log_regression_score_Acha_contribution)

Log_regression_score_Acha_contribution_simple<-glm(Group_control_0_TGA_1 ~ acha_ja_nein_score, family=binomial, data=data, na.action = na.omit)
summary(Log_regression_score_Acha_contribution_simple)
