# Replication syntax for all analyses in main manuscript, appendices, and figures to the article
## The Personalization of Electoral Participation?
### The Relationship Between Trait Evaluations of Presidential Candidates and Turnout Decisions in American Presidential Elections 1980-2020
#### By Segerberg, Tim (2024)

##Disposition of this document:
# 1: Load packages (rows 15-67)
# 2: CFA to validate the trait dimensions of comeptence and character (rows 70-88)
# 3: Descriptive results (rows 91-253)
# 4: Logistic regression analysis (rows 255-320)
# 5: Plot the effects (rows 322-593)
# 6: Robustness test: MLM and LOOCV (rows 595-846)
# 7: Robustness test: 1980-1988 with validated votes (rows 849-1081)

# 1: Load packages
library(psych)
library(MASS)
library(rstanarm)
library(lavaan)
library(cfa)
library(lavaan.survey)
library(semTools)
library(interactions)
library(DescTools)
library(haven)
library(lmtest)
library(PerformanceAnalytics)
library(MVN)
library(ggpubr)
library(semPlot)
library(semTools)
library(dplyr)
library(ltm)
library(psych)
library(haven)
library(ggplot2)
library(lme4)
library(sjstats)
library(ggeffects)
library(misty)
library(effects)
library(sjmisc)
library(sjlabelled)
library(interactions)
library(effects)
library(HLMdiag)
library(DHARMa)
library(car)
library(Matrix)
library(ggResidpanel)
library(lattice)
library(misty)
library(tidyverse)
library(gridExtra)
library(sjPlot)
library(sjlabelled)
library(devtools)
library(stargazer)
library(tidymv)
library(mgcv)
library(visreg)
library(boot)
library(ggplot2)
library(ggeffects)
library(bootstrap)
library(knitr)
library(cowplot)


# 2: CFA to validate the trait dimensions of comeptence and character
# Subset data for the CFA
traits_data_CFA <- subset(ANES, select = c("dem_intelligence", "rep_intelligence", "dem_knowledge", "dem_leadership", "dem_inspiring", "dem_moral", "dem_inspiring", "dem_caring", "dem_honest", "rep_knowledge", "rep_leadership", "rep_inspiring", "rep_moral", "rep_inspiring", "rep_caring", "rep_honest"))

## Fitting the four-factor model (Excluding honest evaluations, since it only measures 2008-2020 and risk produce biased results)
four.model <- ' Competence_dem =~ dem_knowledge + dem_intelligence + dem_leadership 
              Character_dem =~ dem_inspiring + dem_caring + dem_moral
              Competence_rep =~ rep_knowledge + rep_intelligence + rep_leadership 
              Character_rep =~ rep_inspiring + rep_caring + rep_moral '
four.fit <- lavaan::sem(four.model, data=traits_data_CFA)
summary(four.fit, fit.measures = TRUE, standardized = TRUE)
## Fitting the two-factor model (Excluding honest evaluations, since it only measures 2008-2020 and risk produce biased results)
two.model <- ' Competence_character_dem =~ dem_knowledge + dem_intelligence + dem_leadership + dem_inspiring + dem_caring + dem_moral
              Competence_character_rep =~ rep_knowledge + rep_intelligence + rep_leadership + rep_inspiring + rep_caring + rep_moral '
two.fit <- lavaan::sem(two.model, data=traits_data_CFA)
summary(two.fit, fit.measures = TRUE, standardized = TRUE)

anova(four.fit, two.fit)
semPaths(four.fit, whatLabels = "std", edge.label.cex = .6, layout = "tree2", rotation = 1, style = "lisrel", intercepts = FALSE, residuals = FALSE, curve = 1, curvature = 2, nCharNodes = 3, sizeMan = 5, sizeMan2 = 3, edge.color = "#000000")


# 3: Descriptive results
# Calculate the mean competence_dem and competence_rep values for each election year, using weighted data
# Calculate the mean competence_dem and competence_rep and comp_diff values for each election year, using weighted data
ANES_weighted <- ANES %>% 
  group_by(VCF0004) %>% 
  summarize(mean_competence_dem = weighted.mean(competence_dem, VCF0009z, na.rm = TRUE),
            mean_competence_rep = weighted.mean(competence_rep, VCF0009z, na.rm = TRUE),
            mean_comp_diff = weighted.mean(comp_diff, VCF0009z, na.rm = TRUE))%>% 
  filter(VCF0004 >= 1980)

# Create a line plot
p1<-ggplot(ANES_weighted, aes(x = VCF0004, group = 1)) +
  geom_line(aes(y = mean_competence_dem, color = "Democratic candidates"), size = 1, linetype = "solid") +
  geom_point(aes(y = mean_competence_dem, color = "Democratic candidates"), size = 2.5, shape = 21, fill = "white") +
  geom_line(aes(y = mean_competence_rep, color = "Republican candidates"), size = 1, linetype = "solid") +
  geom_point(aes(y = mean_competence_rep, color = "Republican candidates"), size = 2.5, shape = 22, fill = "white") +
  geom_line(aes(y = mean_comp_diff, color = "Absolute difference"), size = 1, linetype = "solid") +
  geom_point(aes(y = mean_comp_diff, color = "Absolute difference"), size = 2.5, shape = 24, fill = "white") +
  scale_x_continuous(limits = c(1980, 2020), breaks = seq(1980, 2020, by = 4)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, 0.2), expand = c(0, 0)) +
  labs(x = "", y = "Mean Competence evaluations", color = "") +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.title.align = 0.5, legend.box.margin = margin(t = 0, b = 0, l = 0, r = 0),
        plot.margin = margin(t = 10, r = 10, b = 30, l = 30))+
  scale_color_manual(name = "", values = c("Democratic candidates" = "black", "Republican candidates" = "black", "Absolute difference" = "black"),
                     guide = guide_legend(
                       override.aes = list(
                         shape = c(24, 21, 22)
                       )
                     ))

# Calculate the mean character_dem, character_rep and char_diff for each election year, using weighted data
ANES_weighted <- ANES %>% 
  group_by(VCF0004) %>% 
  summarize(mean_character_dem = weighted.mean(character_dem, VCF0009z, na.rm = TRUE),
            mean_character_rep = weighted.mean(character_rep, VCF0009z, na.rm = TRUE),
            mean_char_diff = weighted.mean(char_diff, VCF0009z, na.rm = TRUE))%>% 
  filter(VCF0004 >= 1980)

# Create a line plot
p2<-ggplot(ANES_weighted, aes(x = VCF0004, group = 1)) +
  geom_line(aes(y = mean_character_dem, color = "Democratic candidates"), size = 1, linetype = "solid") +
  geom_point(aes(y = mean_character_dem, color = "Democratic candidates"), size = 2.5, shape = 21, fill = "white") +
  geom_line(aes(y = mean_character_rep, color = "Republican candidates"), size = 1, linetype = "solid") +
  geom_point(aes(y = mean_character_rep, color = "Republican candidates"), size = 2.5, shape = 22, fill = "white") +
  geom_line(aes(y = mean_char_diff, color = "Absolute difference"), size = 1, linetype = "solid") +
  geom_point(aes(y = mean_char_diff, color = "Absolute difference"), size = 2.5, shape = 24, fill = "white") +
  scale_x_continuous(limits = c(1980, 2020), breaks = seq(1980, 2020, by = 4)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, 0.2), expand = c(0, 0)) +
  labs(x = "", y = "Mean", color = "") +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.title.align = 0.5, legend.box.margin = margin(t = 0, b = 0, l = 0, r = 0),
        plot.margin = margin(t = 10, r = 10, b = 30, l = 30)) +
  scale_color_manual(name = "", values = c("Democratic candidates" = "black", "Republican candidates" = "black", "Absolute difference" = "black"),
                     guide = guide_legend(
                       override.aes = list(
                         shape = c(24, 21, 22)
                       )
                     ))

# Strength of partisanship over time:
ANES_prop <- ANES %>%
  mutate(VCF0009z = as.numeric(VCF0009z)) %>%
  group_by(VCF0004, PID_strength) %>%
  summarize(n = n(), weight = sum(VCF0009z)) %>%
  ungroup() %>%
  group_by(VCF0004) %>%
  mutate(prop = prop.table(n))

p3 <- ggplot(data = ANES_prop %>% filter(!is.na(PID_strength)),
             aes(x = VCF0004, y = prop, group = PID_strength, shape = factor(PID_strength), linetype = factor(PID_strength), color = factor(PID_strength))) +
  geom_line(aes(weight = weight), size = 1) +
  geom_point(size = 2.5, fill = "white") +
  scale_shape_manual(values = c(21, 22, 24, 23), 
                     labels = c("Strong Partisan", "Weak Partisan", "Leaning Independent", "Independent or apolitical")) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid"),
                        labels = c("Strong Partisan", "Weak Partisan", "Leaning Independent", "Independent or apolitical")) +
  labs(x = "", y = "Proportion of partisans", color = "", shape = "", linetype = "") +
  scale_color_manual(values = c("black", "black", "black", "black"),
                     labels = c("Strong Partisan", "Weak Partisan", "Leaning Independent", "Independent or apolitical")) +
  theme_bw() +
  scale_x_continuous(limits = c(1980, 2020), breaks = seq(1980, 2020, by = 4)) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(legend.position = "bottom",
        legend.title.align = 0.5, legend.box.margin = margin(t = 0, b = 0, l = 0, r = 0),
        plot.margin = margin(t = 10, r = 10, b = 30, l = 30),
        legend.spacing = unit(0.1, "cm")) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE),
         shape = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(size = 3)),
         linetype = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(size = 1.2)))

##Turnout rate according to ANES and United states election project
# Create data frame for official turnout rates (United States Election Project, percent of VEP)
official_turnout <- data.frame(year = c(1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020),
                               rate = c(0.542, 0.552, 0.528, 0.582, 0.517, 0.543, 0.601, 0.625, 0.580, 0.592, 0.669))

# Create weighted mean turnout data
ANES_weighted <- ANES %>%
  group_by(VCF0004) %>%
  summarize(mean_turnout = weighted.mean(turnout, VCF0009z, na.rm = TRUE)) %>%
  filter(VCF0004 >= 1980)

# Merge official turnout rates with ANES data
turnout_data <- merge(ANES_weighted, official_turnout, by.x = "VCF0004", by.y = "year", all.x = TRUE)

# Create a line plot
p4<-ggplot(turnout_data, aes(x = VCF0004, y = mean_turnout)) +
  geom_line(aes(color = "ANES"), size = 1, linetype = "solid") +
  geom_point(aes(color = "ANES"), size = 2.5, shape = 21, fill = "white") +
  geom_line(aes(y = rate, color = "United States Election Project"), size = 1, linetype = "solid") +
  geom_point(aes(y = rate, color = "United States Election Project"), size = 2.5, shape = 22, fill = "white") +
  scale_x_continuous(limits = c(1980, 2020), breaks = seq(1980, 2020, by = 4)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), expand = c(0, 0), 
                     labels = scales::percent_format(accuracy = 1))+
  labs(x = "", y = "Turnout Rate", color = "") +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.title.align = 0.5, legend.box.margin = margin(t = 0, b = 0, l = 0, r = 0),
        plot.margin = margin(t = 10, r = 10, b = 30, l = 30)) +
  scale_color_manual(name = "", values = c("ANES" = "black", "United States Election Project" = "black"),
                     labels = c("ANES", "United States Election Project"),
                     guide = guide_legend(
                       override.aes = list(
                         shape = c(21, 22),
                         linetype = c("solid", "solid")
                       )
                     )) 

# ALSO include validated votes for 1980-1988 in the graph
# Create weighted mean turnout data
ANES_weighted <- ANES %>%
  group_by(VCF0004) %>%
  summarize(mean_turnout = weighted.mean(turnout, VCF0009z, na.rm = TRUE)) %>%
  filter(VCF0004 >= 1980)

# Calculate the mean validated turnout for the years 1980, 1984, and 1988
ANES_validated <- ANES %>%
  filter(VCF0004 %in% c(1980, 1984, 1988)) %>%
  group_by(VCF0004) %>%
  summarize(mean_validated_turnout = weighted.mean(turnout_validated, VCF0009z, na.rm = TRUE))

# Merge ANES validated turnout data with ANES weighted data
turnout_data <- merge(ANES_weighted, ANES_validated, by = "VCF0004", all = TRUE)

# Merge official turnout rates with the combined ANES data
turnout_data <- merge(turnout_data, official_turnout, by.x = "VCF0004", by.y = "year", all = TRUE)
p4a <- p4 + 
  geom_line(data = turnout_data, aes(x = VCF0004, y = mean_validated_turnout, color = "ANES (validated)"), size = 1, linetype = "solid") +
  geom_point(data = turnout_data, aes(x = VCF0004, y = mean_validated_turnout, color = "ANES (validated)"), size = 2.5, shape = 23, fill = "white") +
  scale_color_manual(
    name = "", 
    values = c("ANES" = "black", "ANES (validated)" = "black", "United States Election Project" = "black"),
    labels = c("ANES", "ANES (validated)", "United States Election Project"),
    limits = c("ANES", "ANES (validated)", "United States Election Project"),  # Set the order here
    guide = guide_legend(
      override.aes = list(
        shape = c(21, 23, 22),  # Make sure shapes match the new order
        linetype = c("solid", "solid", "solid")
      )
    )
  )

# 4: Logistic regression analysis
#LOGISTIC REGRESSION MODELS: Dependent variable (turnout, 0=abstained, 1=voted).
## The different models:
##Only H1:
#### Model101_w include weights (variable "VCF0009z"), which is the recommended weight variable according to ANES documentation
##### Include the focal predictors of trait evaluations on turnout.
##Only H2:
#### Model201_w include weights (variable "VCF0009z"), which is the recommended weight variable according to ANES documentation
##### Include the focal predictors of absolute difference on turnout.
## H1, H2, and H3
##### Model301_w include weights (variable "VCF0009z"), which is the recommended weight variable according to ANES documentation
######Include the focal predictors of trait evaluations, absolute difference and interaction terms of dealignment (PID_strength)
### In the following analysis I choose to analyze the relationships using weights. 
### Results without weights will be included in the appendix. 

#Fitting model101_w
model101_w<-glm(turnout~competence_dem+character_dem+competence_rep+character_rep+PID_strength+age_cat+gender+edu_f+election_factor, data=ANES, weights=VCF0009z, family = "binomial")
summary(model101_w)
#No multicollinearity issues
vif(model101_w)
#OR with CI
exp(cbind(OR = coef(model101_w), confint(model101_w)))
#Predicted probabilities:
ggpredict(model101_w,se=TRUE,interactive=FALSE,digits=3)
#Plot the Odds Ratios
plot_model(model101_w, show.values = TRUE, title = "Model 1",
           colors = "bw",
           dot.size = 1,
           axis.labels = c("2020", "2016", "2012", "2008", "2004", "2000", "1996", "1992", "1988", "1984", "High school degree", "Female", "51+", "31-50", "Strength of partisanship", "Republican candidates: Character", "Republican candidates: Competence", "Democratic candidates: Character", "Democratic candidates: Competence"))

#Fitting model201_w
model201_w<-glm(turnout~comp_diff+char_diff+PID_strength+age_cat+gender+edu_f+election_factor, data=ANES, weights=VCF0009z, family = "binomial")
summary(model201_w)
#No multicollinearity issues
vif(model201_w)
#OR with CI
exp(cbind(OR = coef(model201_w), confint(model201_w)))
#Predicted probabilities:
ggpredict(model201_w,se=TRUE,interactive=FALSE,digits=3)
#Plot the Odds Ratios
plot_model(model201_w, show.values = TRUE, title = "Model 2",
           colors = "bw",
           dot.size = 1,
           axis.labels = c("2020", "2016", "2012", "2008", "2004", "2000", "1996", "1992", "1988", "1984", "High school degree", "Female", "51+", "31-50", "Middle & high income", "Strength of partisanship", "Absolute difference: Competence", "Absolute difference: Character"))


#Fitting model301_w
model301_w<-glm(turnout~competence_dem_gm*PID_strength_gm+character_dem_gm*PID_strength_gm+competence_rep_gm*PID_strength_gm+character_rep_gm*PID_strength_gm+comp_diff_gm*PID_strength_gm+char_diff_gm*PID_strength_gm+competence_dem_gm+character_dem_gm+competence_rep_gm+character_rep_gm+comp_diff_gm+char_diff_gm+PID_strength_gm+age_cat+gender+edu_f+election_factor, data=ANES, weights=VCF0009z, family = "binomial")
summary(model301_w)
#No multicollinearity issues
vif(model301_w)
#OR with CI
exp(cbind(OR = coef(model301_w), confint(model301_w)))
#Predicted probabilities:
ggpredict(model301_w,se=TRUE,interactive=FALSE,digits=3)
#Plot the Odds Ratios
plot_model(model301_w, show.values = TRUE, title = "Model 3",
           colors = "bw",
           dot.size = 1,
           axis.labels = c("Character (absolute difference)*Strength of partisanship", "Competence (absolute difference) * Strength of partisanship", "Republican candidates: Character * Strength of partisanship", "Republican candidates: Competence * Strength of partisanship", "Democratic candidates: Character * Strength of partisanship", "Republican candidates: Competence * Strength of partisanship", "2020", "2016", "2012", "2008", "2004", "2000", "1996", "1992", "1988", "1984", "High school degree", "Female", "51+", "31-50", "Middle & high income", "Character (absolute difference)", "Competence (absolute difference)", "Republican candidates: Character", "Republican candidates: Competence", "Democratic candidates: Character", "Strength of partisanship", "Democratic candidates: Competence"))

## Model evaluation, showing that model301_w best fits the data
PseudoR2(model101_w, which = "McFadden")
PseudoR2(model201_w, which = "McFadden")
PseudoR2(model301_w, which = "McFadden")
anova(model101_w, model201_w, model301_w, test="Chisq")

# 5: Plot the effects
#Based on the model evaluation, we plot the results of the model301_w
##Visualizing the results on hypothesis 1: Both competence and character positively affect turnout

# The effects of Competence_dem_gm on turnout (H1) ( not significant )
predictors <- model.matrix(model301_w)[,-1]
means <- colMeans(predictors)
df <- data.frame(competence_dem_gm = seq(-0.536899615754083, 0.463100384245917, length.out = 100),
                 competence_rep_gm = means[1],
                 character_dem_gm = means[2],
                 character_rep_gm = means[3],
                 comp_diff_gm = means[4],
                 char_diff_gm = means[5],
                 PID_strength_gm = means[6],
                 age_cat = means[7],
                 gender = means[8],
                 edu_f = means[9],
                 election_factor = means[10])
# calculate predicted probabilities for each value of competence_dem_gm
predictors <- ggpredict(model301_w, terms = "competence_dem_gm", conditions = df, type = "fe", ci.lvl = 0.95)
# plot the predicted probabilities
plot_1<-ggplot(predictors, aes(x, predicted, ymin = conf.low, ymax = conf.high)) +
  geom_line(size = 1.2) +
  geom_ribbon(alpha = 0.3) +
  labs(x = "Democratic candidates: Competence", y = "Pr(turnout)") +
  ggtitle("Effect of perceived competence (Democratic party) on Probability of Turnout") +
  theme(plot.title = element_text(size = 16, face = "bold"), legend.position = "bottom")

# The effects of Competence_rep_gm on turnout (H1)
predictors <- model.matrix(model301_w)[,-1]
means <- colMeans(predictors)
df <- data.frame(competence_rep_gm = seq(-0.480090715160732, 0.519909284839268, length.out = 100),
                 competence_dem_gm = means[1],
                 character_dem_gm = means[2],
                 character_rep_gm = means[3],
                 comp_diff_gm = means[4],
                 char_diff_gm = means[5],
                 PID_strength_gm = means[6],
                 age_cat = means[7],
                 gender = means[8],
                 edu_f = means[9],
                 election_factor = means[10])
# calculate predicted probabilities for each value of competence_dem_gm
predictors <- ggpredict(model301_w, terms = "competence_rep_gm", conditions = df, type = "fe", ci.lvl = 0.95)
# plot the predicted probabilities
plot_2<-ggplot(predictors, aes(x, predicted, ymin = conf.low, ymax = conf.high)) +
  geom_line(size = 1.2) +
  geom_ribbon(alpha = 0.3) +
  labs(x = "Republican candidates: Competence", y = "Pr(turnout)") +
  ggtitle("") +
  theme(plot.title = element_text(size = 16, face = "bold"), legend.position = "bottom")

# The effects of Character_dem_gm on turnout (H1) (not significant)
predictors <- model.matrix(model301_w)[,-1]
means <- colMeans(predictors)
df <- data.frame(character_dem_gm = seq(-0.488046546306652, 0.511953453693348, length.out = 100),
                 competence_dem_gm = means[1],
                 competence_rep_gm = means[2],
                 character_rep_gm = means[3],
                 comp_diff_gm = means[4],
                 char_diff_gm = means[5],
                 PID_strength_gm = means[6],
                 age_cat = means[7],
                 gender = means[8],
                 edu_f = means[9],
                 election_factor = means[10])
# calculate predicted probabilities for each value of competence_dem_gm
predictors <- ggpredict(model301_w, terms = "character_dem_gm", conditions = df, type = "fe", ci.lvl = 0.95)
# plot the predicted probabilities
plot_3<-ggplot(predictors, aes(x, predicted, ymin = conf.low, ymax = conf.high)) +
  geom_line(size = 1.2) +
  geom_ribbon(alpha = 0.3) +
  labs(x = "Democratic candidates: Character", y = "Pr(turnout)") +
  ggtitle("Effect of perceived character (Democratic party) on Probability of Turnout") +
  theme(plot.title = element_text(size = 16, face = "bold"), legend.position = "bottom")

# The effects of Character_rep_gm on turnout (H1)
predictors <- model.matrix(model301_w)[,-1]
means <- colMeans(predictors)
df <- data.frame(character_rep_gm = seq(-0.454938859774496, 0.545061140225504, length.out = 100),
                 competence_dem_gm = means[1],
                 competence_rep_gm = means[2],
                 character_dem_gm = means[3],
                 comp_diff_gm = means[4],
                 char_diff_gm = means[5],
                 PID_strength_gm = means[6],
                 age_cat = means[7],
                 gender = means[8],
                 edu_f = means[9],
                 election_factor = means[10])
# calculate predicted probabilities for each value of competence_dem_gm
predictors <- ggpredict(model301_w, terms = "character_rep_gm", conditions = df, type = "fe", ci.lvl = 0.95)
# plot the predicted probabilities
plot_4<-ggplot(predictors, aes(x, predicted, ymin = conf.low, ymax = conf.high)) +
  geom_line(size = 1.2) +
  geom_ribbon(alpha = 0.3) +
  labs(x = "Republican candidates: Character", y = "Pr(turnout)") +
  ggtitle("") +
  theme(plot.title = element_text(size = 16, face = "bold"), legend.position = "bottom")
grid.arrange(plot_4, plot_2, ncol = 2)

##Visualizing the results on hypothesis 2: Larger distance in evaluations between the candidates promote turnout

library(ggplot2)
library(ggeffects)
library(gridExtra)

#The effects of competence (absolute difference) on turnout (H2)
predictors <- model.matrix(model301_w)[,-1]
means <- colMeans(predictors)
df <- data.frame(comp_diff_gm = seq(-0.430656498169453, 0.569343501830547, length.out = 100),
                 competence_dem_gm = means[1],
                 competence_rep_gm = means[2],
                 character_dem_gm = means[3],
                 character_rep_gm = means[4],
                 char_diff_gm = means[5],
                 PID_strength_gm = means[6],
                 age_cat = means[7],
                 gender = means[8],
                 edu_f = means[9],
                 election_factor = means[10])

# calculate predicted probabilities for each value of character_rep_gm
predictors <- ggpredict(model301_w, terms = "comp_diff_gm", conditions = df)
# plot the predicted probabilities
plot_5 <- ggplot(predictors, aes(x, predicted, ymin = conf.low, ymax = conf.high)) +
  geom_line(size = 1.2) +
  geom_ribbon(alpha = 0.3) +
  labs(x = "Competence (absolute difference)", y = "Pr(turnout)") +
  scale_x_continuous(breaks = c(-0.25, 0.00, 0.25, 0.50)) +
  ggtitle("") +
  theme(plot.title = element_text(size = 12, face = "bold"), legend.position = "bottom") +
  theme_bw()

#The effects of character (absolute difference) on turnout (H2)
predictors <- model.matrix(model301_w)[,-1]
means <- colMeans(predictors)
df <- data.frame(char_diff_gm = seq(0.0331076865321559, 1.03310768653216, length.out = 100),
                 competence_dem_gm = means[1],
                 competence_rep_gm = means[2],
                 character_dem_gm = means[3],
                 character_rep_gm = means[4],
                 comp_diff_gm = means[5],
                 PID_strength_gm = means[6],
                 age_cat = means[7],
                 gender = means[8],
                 edu_f = means[9],
                 election_factor = means[10])

# calculate predicted probabilities for each value of character_rep_gm
predictors <- ggpredict(model301_w, terms = "char_diff_gm", conditions = df)
# plot the predicted probabilities
plot_6 <- ggplot(predictors, aes(x, predicted, ymin = conf.low, ymax = conf.high)) +
  geom_line(size = 1.2) +
  geom_ribbon(alpha = 0.3) +
  labs(x = "Character (absolute difference)", y = "") +
  ggtitle("") +
  theme(plot.title = element_text(size = 12, face = "bold"), legend.position = "bottom") +
  theme_bw()

grid.arrange(plot_5, plot_6, ncol = 2)
ggarrange(plot_5, plot_6, ncol = 2, common.legend = TRUE, legend = "bottom")


##Visualizing the results on hypothesis 3: Effects are larger for dealigned voters
### The interaction of Personality traits*Strength of partisanship
### Republicans: Competence
plot_7<-interact_plot(model301_w, pred = competence_rep_gm, modx = PID_strength_gm, modx.values = c(-1.88, -0.88, 0.124, 1.124),
                      line.thickness = 1.5,
              main.title = "", 
              interval = TRUE,
              x.label = "Republican candidates: Competence",
              y.label = "Pr(turnout)",
              modx.labels = c("Independent\nor apolitical", "Leaning \nindependent", "Weak partisan", "Strong partisan"),
              legend.main = "Strength of partisanship",
              colors = "Rainbow") +
  theme(axis.title.y = element_text(size = 12), 
        plot.title = element_text(size = 12))

### Republicans: Character
plot_8<-interact_plot(model301_w, pred = character_rep_gm, modx = PID_strength_gm, modx.values = c(-1.88, -0.88, 0.124, 1.124),
                      line.thickness = 1.5,
                      main.title = "", 
                      interval = TRUE,
                      x.label = "Republican candidates: Character",
                      y.label = "",
                      modx.labels = c("Independent\nor apolitical", "Leaning \nindependent", "Weak partisan", "Strong partisan"),
                      legend.main = "Strength of partisanship",
                      colors = "Rainbow") + 
  theme(plot.title = element_text(size = 12))


ggarrange(plot_7, plot_8, ncol=2, align = "v", common.legend = TRUE, legend="bottom")

##Revision of plot, to align the y-axes
y_breaks <- seq(0.4, 0.85, by = 0.1)
plot_7 <- plot_7 +
  scale_y_continuous(breaks = y_breaks) +
  coord_cartesian(ylim = c(0.4, 0.83)) +
  theme(axis.title.y = element_text(size = 12), 
        plot.title = element_text(size = 12))

plot_8 <- plot_8 +
  scale_y_continuous(breaks = y_breaks) +
  coord_cartesian(ylim = c(0.4, 0.83)) +
  theme(plot.title = element_text(size = 12))
ggarrange(plot_7, plot_8, ncol=2, widths = 15, heights = 10, common.legend = TRUE, legend="bottom")



### Democrats: Competence (not significant)
plot_9<-interact_plot(model301_w, pred = competence_dem_gm, modx = PID_strength_gm, modx.values = c(-1.88, -0.88, 0.124, 1.124),
                      line.thickness = 1.5,
                      main.title = "", 
                      x.label = "Democratic candidates: Competence",
                      y.label = "Pr(turnout)",
                      modx.labels = c("Independent\nor apolitical", "Leaning \nindependent", "Weak partisan", "Strong partisan"),
                      legend.main = "Strength of partisanship",
                      colors = "Rainbow")


### Democrats: Character (not significant)
plot_10<-interact_plot(model301_w, pred = character_dem_gm, modx = PID_strength_gm, modx.values = c(-1.88, -0.88, 0.124, 1.124),
                      main.title = "", 
                      x.label = "Democratic candidates: Character",
                      y.label = "Pr(turnout)",
                      modx.labels = c("Independent\nor apolitical", "Leaning \nindependent", "Weak partisan", "Strong partisan"),
                      legend.main = "Strength of partisanship",
                      colors = "Greys")


### The interaction of Absolute difference*Strength of partisanship
### Competence (absolute difference)
plot_11<-interact_plot(model301_w, pred = comp_diff_gm, modx = PID_strength_gm, modx.values = c(-1.88, -0.88, 0.124, 1.124),
                       main.title = "", 
                       line.thickness = 1.5,
                       x.label = "Competence (absolute difference)",
                       y.label = "Pr(turnout)",
                       interval = TRUE,
                       modx.labels = c("Independent\nor apolitical", "Leaning \nindependent", "Weak partisan", "Strong partisan"),
                       legend.main = "Strength of partisanship",
                       colors = "Rainbow") +
  theme(axis.title.y = element_text(size = 12), 
        plot.title = element_text(size = 12))

### Character (absolute difference) (not signficant)
plot_12<-interact_plot(model301_w, pred = char_diff_gm, modx = PID_strength_gm, modx.values = c(-1.88, -0.88, 0.124, 1.124),
                       main.title = "", 
                       line.thickness = 1.5,
                       x.label = "Character (absolute difference)",
                       y.label = "",
                       interval = TRUE,
                       modx.labels = c("Independent\nor apolitical", "Leaning \nindependent", "Weak partisan", "Strong partisan"),
                       legend.main = "Strength of partisanship",
                       colors = "Rainbow") + 
  theme(plot.title = element_text(size = 12))

ggarrange(plot_11, plot_12, ncol=2, common.legend = TRUE, legend="bottom")

#Revision of plots, to align the y-axes:
y_breaks <- seq(0.4, 0.85, by = 0.1)
plot_11 <- plot_11 +
  scale_y_continuous(breaks = y_breaks) +
  coord_cartesian(ylim = c(0.36, 0.83)) +
  theme(axis.title.y = element_text(size = 12), 
        plot.title = element_text(size = 12))

plot_12 <- plot_12 +
  scale_y_continuous(breaks = y_breaks) +
  coord_cartesian(ylim = c(0.36, 0.83)) +
  theme(plot.title = element_text(size = 12))
ggarrange(plot_11, plot_12, ncol=2, widths = 10, heights = 10, common.legend = TRUE, legend="bottom")

# 6: Robustness test: MLM and LOOCV
## Robustness test 1: MLM with random intercepts for election year
#Apply the both models on a multi-level analysis where survey responses are clustered into the election years (eleven) and are given a random intercept
M0 <- glmer(turnout ~ 1 + (1 | election_year), data = ANES, weights = VCF0009z, family = "binomial")
summary(M0)
icc <- M0@theta[1]^2/ (M0@theta[1]^2 + (3.14159^2/3))
icc
model101_w_mlm<-glmer(turnout~competence_dem_gm+character_dem_gm+competence_rep_gm+character_rep_gm+PID_strength_gm+age_cat+gender_gm+edu_gm+(1 | election_year), data=ANES, weights = VCF0009z, family = "binomial")
model201_w_mlm<-glmer(turnout~char_diff_gm+comp_diff_gm+PID_strength_gm+age_cat+gender_gm+edu_gm+(1 | election_year), data=ANES, weights = VCF0009z, family = "binomial")
model301_w_mlm<-glmer(turnout~competence_dem_gm*PID_strength_gm+character_dem_gm*PID_strength_gm+competence_rep_gm*PID_strength_gm+character_rep_gm*PID_strength_gm+comp_diff_gm*PID_strength_gm+char_diff_gm*PID_strength_gm+comp_diff_gm+char_diff_gm+competence_dem_gm+character_dem_gm+competence_rep_gm+character_rep_gm+PID_strength_gm+age_cat+gender_gm+edu_gm+(1 | election_year), data=ANES, weights = VCF0009z, family = "binomial")
summary(model101_w_mlm)
#Get OR
se <- sqrt(diag(vcov(model101_w_mlm)))
tab <- cbind(Est = fixef(model101_w_mlm), LL = fixef(model101_w_mlm) - 1.96 * se, UL = fixef(model101_w_mlm) + 1.96 * se)
print(exp(tab), digits=3)
summary(model201_w_mlm)
#Get OR
se <- sqrt(diag(vcov(model201_w_mlm)))
tab <- cbind(Est = fixef(model201_w_mlm), LL = fixef(model201_w_mlm) - 1.96 * se, UL = fixef(model201_w_mlm) + 1.96 * se)
print(exp(tab), digits=3)
summary(model301_w_mlm)
#Get OR
se <- sqrt(diag(vcov(model301_w_mlm)))
tab <- cbind(Est = fixef(model301_w_mlm), LL = fixef(model301_w_mlm) - 1.96 * se, UL = fixef(model301_w_mlm) + 1.96 * se)
print(exp(tab), digits=3)
stargazer(model101_w_mlm, model201_w_mlm, model301_w_mlm, type="html", 
          dep.var.labels=c("turnout (0: Abstention, 1: Vote) in American presidential elections 1980-2020"),
          out="mlm_models.htm")

## Robustness test 2: LOOCV (Leave-One-Out Cross Validation)
## Conduct 11 regression analysis (one for each election year, leaving one out every time)
### First model101_w
# Define the model formula
model101_w_loocv <- formula(turnout ~ competence_dem + character_dem + competence_rep + character_rep + PID_strength + age_cat + gender + edu_f, data = ANES, weights = VCF0009z, family = "binomial")
# Define the data and weights
data <- ANES
weights <- ANES$VCF0009z
# Define the number of iterations (equal to the number of levels of the election_factor variable)
n_iterations <- length(unique(ANES$VCF0004))
# Create empty vectors to store the mean results
mean_coefs <- rep(0, length(model101_w_loocv[[2]]))
mean_se <- rep(0, length(model101_w_loocv[[2]]))
mean_pvals <- rep(0, length(model101_w_loocv[[2]]))
# Perform LOOCV for each iteration
for (i in 1:n_iterations) {
  # Create a subset of the data for the current iteration
  data_subset <- data[ANES$VCF0004 != unique(ANES$VCF0004)[i], ]
  weights_subset <- weights[ANES$VCF0004 != unique(ANES$VCF0004)[i]]
  # Fit the model using glm()
  model_fit_101_w_loocv <- glm(formula = model101_w_loocv, family = "binomial", data = data_subset, weights = weights_subset)
  # Extract the coefficients, standard errors, and p-values
  coefs <- coef(model_fit_101_w_loocv)
  se <- summary(model_fit_101_w_loocv)$coefficients[, 2]
  pvals <- summary(model_fit_101_w_loocv)$coefficients[, 4]
  # Add the results to the mean vectors
  mean_coefs <- mean_coefs + coefs / n_iterations
  mean_se <- mean_se + se / n_iterations
  mean_pvals <- mean_pvals + pvals / n_iterations
}
# Create a data frame with the mean coefficients, standard errors, and p-values
results <- data.frame(
  predictor = names(coef(model_fit_101_w_loocv)),
  mean_coef = mean_coefs,
  mean_se = mean_se,
  mean_pval = mean_pvals
)
# Print the results
print(results)
#Mean AIC
mean(AIC(model_fit_101_w_loocv))
#Mean observations used for each iteration
data_subset <- data[ANES$VCF0004 != unique(ANES$VCF0004)[i], ]


### Second model201_w
# Define the model formula
model201_w_loocv <- formula(turnout ~ comp_diff_gm + char_diff_gm + PID_strength + age_cat + gender + edu_f, data = ANES, weights = VCF0009z, family = "binomial")
# Define the data and weights
data <- ANES
weights <- ANES$VCF0009z
# Define the number of iterations (equal to the number of levels of the election_factor variable)
n_iterations <- length(unique(ANES$VCF0004))
# Create empty vectors to store the mean results
mean_coefs <- rep(0, length(model201_w_loocv[[2]]))
mean_se <- rep(0, length(model201_w_loocv[[2]]))
mean_pvals <- rep(0, length(model201_w_loocv[[2]]))
# Perform LOOCV for each iteration
for (i in 1:n_iterations) {
  # Create a subset of the data for the current iteration
  data_subset <- data[ANES$VCF0004 != unique(ANES$VCF0004)[i], ]
  weights_subset <- weights[ANES$VCF0004 != unique(ANES$VCF0004)[i]]
  # Fit the model using glm()
  model_fit_201_w_loocv <- glm(formula = model201_w_loocv, family = "binomial", data = data_subset, weights = weights_subset)
  # Extract the coefficients, standard errors, and p-values
  coefs <- coef(model_fit_201_w_loocv)
  se <- summary(model_fit_201_w_loocv)$coefficients[, 2]
  pvals <- summary(model_fit_201_w_loocv)$coefficients[, 4]
  # Add the results to the mean vectors
  mean_coefs <- mean_coefs + coefs / n_iterations
  mean_se <- mean_se + se / n_iterations
  mean_pvals <- mean_pvals + pvals / n_iterations
}
# Create a data frame with the mean coefficients, standard errors, and p-values
results <- data.frame(
  predictor = names(coef(model_fit_201_w_loocv)),
  mean_coef = mean_coefs,
  mean_se = mean_se,
  mean_pval = mean_pvals
)
# Print the results
print(results)
#Mean AIC
mean(AIC(model_fit_201_w_loocv))

### Third model301_w
# Define the model formula
model301_w_loocv <- formula(turnout ~ competence_dem_gm * PID_strength_gm + character_dem_gm * PID_strength_gm + competence_rep_gm * PID_strength_gm + character_rep_gm * PID_strength_gm + comp_diff_gm * PID_strength_gm + char_diff_gm * PID_strength_gm + competence_dem_gm + character_dem_gm + competence_rep_gm + character_rep_gm + comp_diff_gm + char_diff_gm + PID_strength_gm + age_cat + gender + edu_f, data = ANES, weights = VCF0009z, family = "binomial")
# Define the data and weights
data <- ANES
weights <- ANES$VCF0009z
# Define the number of iterations (equal to the number of levels of the election_factor variable)
n_iterations <- length(unique(ANES$VCF0004))
# Create empty vectors to store the mean results
mean_coefs <- rep(0, length(model301_w_loocv[[2]]))
mean_se <- rep(0, length(model301_w_loocv[[2]]))
mean_pvals <- rep(0, length(model301_w_loocv[[2]]))
# Perform LOOCV for each iteration
for (i in 1:n_iterations) {
  # Create a subset of the data for the current iteration
  data_subset <- data[ANES$VCF0004 != unique(ANES$VCF0004)[i], ]
  weights_subset <- weights[ANES$VCF0004 != unique(ANES$VCF0004)[i]]
  # Fit the model using glm()
  model_fit_301_w_loocv <- glm(formula = model301_w_loocv, family = "binomial", data = data_subset, weights = weights_subset)
  # Extract the coefficients, standard errors, and p-values
  coefs <- coef(model_fit_301_w_loocv)
  se <- summary(model_fit_301_w_loocv)$coefficients[, 2]
  pvals <- summary(model_fit_301_w_loocv)$coefficients[, 4]
  # Add the results to the mean vectors
  mean_coefs <- mean_coefs + coefs / n_iterations
  mean_se <- mean_se + se / n_iterations
  mean_pvals <- mean_pvals + pvals / n_iterations
}
# Create a data frame with the mean coefficients, standard errors, and p-values
results <- data.frame(
  predictor = names(coef(model_fit_301_w_loocv)),
  mean_coef = mean_coefs,
  mean_se = mean_se,
  mean_pval = mean_pvals
)
# Print the results
print(results)

#Mean AIC
mean(AIC(model_fit_301_w_loocv))

### Robustness test 3: Interaction effect with year to examine the election-specific outliers
model_robust_inter<-glm(turnout~competence_dem*election_factor+competence_rep*election_factor+character_dem*election_factor+character_rep*election_factor+competence_dem+competence_rep+character_dem+character_rep+comp_diff+char_diff+PID_strength+age_cat+gender+edu_f+election_factor, data=ANES, weights=VCF0009z, family = "binomial")
summary(model_robust_inter)
#OR with CI
exp(cbind(OR = coef(model_robust_inter), confint(model_robust_inter)))
#Predicted probabilities:
ggpredict(model101_w,se=TRUE,interactive=FALSE,digits=3)
# Save model summary to object
model_summary <- exp(cbind(OR = coef(model_robust_inter), confint(model_robust_inter)))
# Convert model summary to a data frame
model_summary_df <- as.data.frame(model_summary)
# Export table to file
write.table(model_summary_df, "output_table.csv", sep = ",", 
            row.names = FALSE, col.names = TRUE, quote = TRUE)
stargazer(model_robust_inter, type="html", 
          dep.var.labels=c("turnout (0: Abstention, 1: Vote) in American presidential elections 1980-2020"),
          out="model_robust_inter.htm")


#Plot the interaction effects
interactionplot_competence_dem<-plot_summs(model_robust_inter, exp = TRUE, colors = "Qual1",
           coefs = c("1984" = "competence_dem:election_factor1984",
                     "1988" = "competence_dem:election_factor1988",
                     "1992" = "competence_dem:election_factor1992",
                     "1996" = "competence_dem:election_factor1996",
                     "2000" = "competence_dem:election_factor2000",
                     "2004" = "competence_dem:election_factor2004",
                     "2008" = "competence_dem:election_factor2008",
                     "2012" = "competence_dem:election_factor2012",
                     "2016" = "competence_dem:election_factor2016",
                     "2020" = "competence_dem:election_factor2020"))
interactionplot_competence_dem + ggtitle('Figure 12. Democratic candidates: Competence * Election year') + 
  xlab("Effects on Pr(turnout)") +
  theme(plot.title = element_text(size = 11))
interactionplot_competence_rep<-plot_summs(model_robust_inter, exp = TRUE, colors = "Qual1",
                                           coefs = c("1984" = "election_factor1984:competence_rep",
                                                     "1988" = "election_factor1988:competence_rep",
                                                     "1992" = "election_factor1992:competence_rep",
                                                     "1996" = "election_factor1996:competence_rep",
                                                     "2000" = "election_factor2000:competence_rep",
                                                     "2004" = "election_factor2004:competence_rep",
                                                     "2008" = "election_factor2008:competence_rep",
                                                     "2012" = "election_factor2012:competence_rep",
                                                     "2016" = "election_factor2016:competence_rep",
                                                     "2020" = "election_factor2020:competence_rep"))
interactionplot_competence_rep + 
  ggtitle('Figure 13. Republican candidates: Competence * Election year') + 
  xlab("Effects on Pr(turnout)") +
  theme(plot.title = element_text(size = 11))
interactionplot_character_dem<-plot_summs(model_robust_inter, exp = TRUE, colors = "Qual1",
                                           coefs = c("1984" = "election_factor1984:character_dem",
                                                     "1988" = "election_factor1988:character_dem",
                                                     "1992" = "election_factor1992:character_dem",
                                                     "1996" = "election_factor1996:character_dem",
                                                     "2000" = "election_factor2000:character_dem",
                                                     "2004" = "election_factor2004:character_dem",
                                                     "2008" = "election_factor2008:character_dem",
                                                     "2012" = "election_factor2012:character_dem",
                                                     "2016" = "election_factor2016:character_dem",
                                                     "2020" = "election_factor2020:character_dem"))
interactionplot_character_dem + 
  ggtitle('Figure 14. Democratic candidates: Character * Election year') + 
  xlab("Effects on Pr(turnout)") +
  theme(plot.title = element_text(size = 11))
interactionplot_character_rep<-plot_summs(model_robust_inter, exp = TRUE, colors = "Qual1",
                                          coefs = c("1984" = "election_factor1984:character_rep",
                                                    "1988" = "election_factor1988:character_rep",
                                                    "1992" = "election_factor1992:character_rep",
                                                    "1996" = "election_factor1996:character_rep",
                                                    "2000" = "election_factor2000:character_rep",
                                                    "2004" = "election_factor2004:character_rep",
                                                    "2008" = "election_factor2008:character_rep",
                                                    "2012" = "election_factor2012:character_rep",
                                                    "2016" = "election_factor2016:character_rep",
                                                    "2020" = "election_factor2020:character_rep"))
interactionplot_character_rep + 
  ggtitle('Figure 15. Republican candidates: Character * Election year') + 
  xlab("Effects on Pr(turnout)") +
  theme(plot.title = element_text(size = 11))

interactionplot_character_rep + 

  grid.arrange(
    interactionplot_competence_dem + ggtitle('Figure 12. Democratic candidates:\nCompetence * Election year') + xlab("Effects on Pr(turnout)") +
      theme(plot.title = element_text(size = 10)),
    interactionplot_competence_rep + ggtitle('Figure 13. Republican candidates:\nCompetence * Election year') + xlab("Effects on Pr(turnout)") +
      theme(plot.title = element_text(size = 10)),
    interactionplot_character_dem + ggtitle('Figure 14. Democratic candidates:\nCharacter * Election year') + xlab("Effects on Pr(turnout)") +
      theme(plot.title = element_text(size = 10)),
    interactionplot_character_rep + ggtitle('Figure 15. Republican candidates:\nCharacter * Election year') + xlab("Effects on Pr(turnout)") +
      theme(plot.title = element_text(size = 10)),
    ncol = 2
  )


summ(model_robust_inter, digits = 3)
plot_summs(model_robust_inter)


# 7: Robustness test: 1980-1988 with validated votes
### ROBUSTNESS TEST 2; 1980-1988 with validated votes only
# Subset the ANES data for the years 1980, 1984, and 1988 for robustness test
ANES_subset <- ANES %>% filter(VCF0004 %in% c(1980, 1984, 1988))

# Subsetting the election year variable for 1980, 1984, and 1988
ANES$election_subset <- ifelse(ANES$VCF0004 %in% c(1980, 1984, 1988), ANES$VCF0004, NA)

# Creating the factor variable for the subsetted election years
election_factor_subset <- factor(ANES$election_subset, levels = c(1980, 1984, 1988), labels = c("1980", "1984", "1988"))
election_factor_subset <- na.omit(election_factor_subset)
summary(election_factor_subset)

#LOGISTIC REGRESSION MODELS: Dependent variable (turnout, 0=abstained, 1=voted).
## The different models:
##Only H1:
#### Model101_w include weights (variable "VCF0009z"), which is the recommended weight variable according to ANES documentation
##### Include the focal predictors of trait evaluations on turnout.
##Only H2:
#### Model201_w include weights (variable "VCF0009z"), which is the recommended weight variable according to ANES documentation
##### Include the focal predictors of absolute difference on turnout.
## H1, H2, and H3
##### Model301_w include weights (variable "VCF0009z"), which is the recommended weight variable according to ANES documentation
######Include the focal predictors of trait evaluations, absolute difference and interaction terms of dealignment (PID_strength)
### In the following analysis I choose to analyze the relationships using weights. 
### Results without weights will be included in the appendix. 

#Fitting model101_rob (original turnout variable, not validated)
model101_rob<-glm(turnout~competence_dem+character_dem+competence_rep+character_rep+PID_strength+age_cat+gender+edu_f+election_factor_subset, data=ANES_subset, weights=VCF0009z, family = "binomial")
summary(model101_rob)
#No multicollinearity issues
vif(model101_rob)
#OR with CI
exp(cbind(OR = coef(model101_rob), confint(model101_rob)))
#Predicted probabilities:
ggpredict(model101_rob,se=TRUE,interactive=FALSE,digits=3)
#Plot the Odds Ratios
plot_model(model101_rob, show.values = TRUE, title = "Model 1 (not validated)",
           colors = "bw",
           dot.size = 1,
           axis.labels = c("1988", "1984", "High school degree", "Female", "51+", "31-50", "Strength of partisanship", "Republican candidates: Character", "Republican candidates: Competence", "Democratic candidates: Character", "Democratic candidates: Competence"))

#Fitting model201_rob
model201_rob<-glm(turnout~comp_diff+char_diff+PID_strength+age_cat+gender+edu_f+election_factor_subset, data=ANES_subset, weights=VCF0009z, family = "binomial")
summary(model201_rob)
#No multicollinearity issues
vif(model201_rob)
#OR with CI
exp(cbind(OR = coef(model201_rob), confint(model201_rob)))
#Predicted probabilities:
ggpredict(model201_rob,se=TRUE,interactive=FALSE,digits=3)
#Plot the Odds Ratios
plot_model(model201_rob, show.values = TRUE, title = "Model 2",
           colors = "bw",
           dot.size = 1,
           axis.labels = c("1988", "1984", "High school degree", "Female", "51+", "31-50", "Middle & high income", "Strength of partisanship", "Absolute difference: Competence", "Absolute difference: Character"))


#Fitting model301_rob
model301_rob<-glm(turnout~competence_dem_gm*PID_strength_gm+character_dem_gm*PID_strength_gm+competence_rep_gm*PID_strength_gm+character_rep_gm*PID_strength_gm+comp_diff_gm*PID_strength_gm+char_diff_gm*PID_strength_gm+competence_dem_gm+character_dem_gm+competence_rep_gm+character_rep_gm+comp_diff_gm+char_diff_gm+PID_strength_gm+age_cat+gender+edu_f+election_factor_subset, data=ANES_subset, weights=VCF0009z, family = "binomial")
summary(model301_rob)
#No multicollinearity issues
vif(model301_rob)
#OR with CI
exp(cbind(OR = coef(model301_rob), confint(model301_rob)))
#Predicted probabilities:
ggpredict(model301_rob,se=TRUE,interactive=FALSE,digits=3)
#Plot the Odds Ratios
plot_model(model301_rob, show.values = TRUE, title = "Model 3",
           colors = "bw",
           dot.size = 1,
           axis.labels = c("Character (absolute difference)*Strength of partisanship", "Competence (absolute difference) * Strength of partisanship", "Republican candidates: Character * Strength of partisanship", "Republican candidates: Competence * Strength of partisanship", "Democratic candidates: Character * Strength of partisanship", "Republican candidates: Competence * Strength of partisanship", "2020", "2016", "2012", "2008", "2004", "2000", "1996", "1992", "1988", "1984", "High school degree", "Female", "51+", "31-50", "Middle & high income", "Character (absolute difference)", "Competence (absolute difference)", "Republican candidates: Character", "Republican candidates: Competence", "Democratic candidates: Character", "Strength of partisanship", "Democratic candidates: Competence"))

#Fitting model101_val (new turnout variable, validated votes only)
model101_val<-glm(turnout_validated~competence_dem+character_dem+competence_rep+character_rep+PID_strength+age_cat+gender+edu_f+election_factor_subset, data=ANES_subset, weights=VCF0009z, family = "binomial")
summary(model101_val)
#No multicollinearity issues
vif(model101_val)
#OR with CI
exp(cbind(OR = coef(model101_val), confint(model101_val)))
#Predicted probabilities:
ggpredict(model101_val,se=TRUE,interactive=FALSE,digits=3)
#Plot the Odds Ratios
plot_model(model101_val, show.values = TRUE, title = "Model 1 (validated)",
           colors = "bw",
           dot.size = 1,
           axis.labels = c("1988", "1984", "High school degree", "Female", "51+", "31-50", "Strength of partisanship", "Republican candidates: Character", "Republican candidates: Competence", "Democratic candidates: Character", "Democratic candidates: Competence"))

#Fitting model201_val
model201_val<-glm(turnout_validated~comp_diff+char_diff+PID_strength+age_cat+gender+edu_f+election_factor_subset, data=ANES_subset, weights=VCF0009z, family = "binomial")
summary(model201_val)
#No multicollinearity issues
vif(model201_val)
#OR with CI
exp(cbind(OR = coef(model201_val), confint(model201_val)))
#Predicted probabilities:
ggpredict(model201_val,se=TRUE,interactive=FALSE,digits=3)
#Plot the Odds Ratios
plot_model(model201_val, show.values = TRUE, title = "Model 2",
           colors = "bw",
           dot.size = 1,
           axis.labels = c("1988", "1984", "High school degree", "Female", "51+", "31-50", "Middle & high income", "Strength of partisanship", "Absolute difference: Competence", "Absolute difference: Character"))

#Fitting model301_val
model301_val<-glm(turnout_validated~competence_dem_gm*PID_strength_gm+character_dem_gm*PID_strength_gm+competence_rep_gm*PID_strength_gm+character_rep_gm*PID_strength_gm+comp_diff_gm*PID_strength_gm+char_diff_gm*PID_strength_gm+competence_dem_gm+character_dem_gm+competence_rep_gm+character_rep_gm+comp_diff_gm+char_diff_gm+PID_strength_gm+age_cat+gender+edu_f+election_factor_subset, data=ANES_subset, weights=VCF0009z, family = "binomial")
summary(model301_val)
#No multicollinearity issues
vif(model301_val)
#OR with CI
exp(cbind(OR = coef(model301_val), confint(model301_val)))
#Predicted probabilities:
ggpredict(model301_val,se=TRUE,interactive=FALSE,digits=3)
#Plot the Odds Ratios
plot_model(model301_val, show.values = TRUE, title = "Model 3",
           colors = "bw",
           dot.size = 1,
           axis.labels = c("Character (absolute difference)*Strength of partisanship", "Competence (absolute difference) * Strength of partisanship", "Republican candidates: Character * Strength of partisanship", "Republican candidates: Competence * Strength of partisanship", "Democratic candidates: Character * Strength of partisanship", "Republican candidates: Competence * Strength of partisanship", "2020", "2016", "2012", "2008", "2004", "2000", "1996", "1992", "1988", "1984", "High school degree", "Female", "51+", "31-50", "Middle & high income", "Character (absolute difference)", "Competence (absolute difference)", "Republican candidates: Character", "Republican candidates: Competence", "Democratic candidates: Character", "Strength of partisanship", "Democratic candidates: Competence"))

# Model evaluation
PseudoR2(model101_rob, which = "McFadden")
PseudoR2(model201_rob, which = "McFadden")
PseudoR2(model301_rob, which = "McFadden")
PseudoR2(model101_val, which = "McFadden")
PseudoR2(model201_val, which = "McFadden")
PseudoR2(model301_val, which = "McFadden")
anova(model101_rob, model201_rob, model301_rob, model101_val, model201_val, model301_val, test="Chisq")

### PLOT the significant coefficients from model 3 (validated and self-reported, respectively)
predictors <- model.matrix(model301_rob)[,-1]
means <- colMeans(predictors)
df <- data.frame(competence_dem_gm = seq(-0.536899615754083, 0.463100384245917, length.out = 100),
                 competence_rep_gm = means[1],
                 character_dem_gm = means[2],
                 character_rep_gm = means[3],
                 comp_diff_gm = means[4],
                 char_diff_gm = means[5],
                 PID_strength_gm = means[6],
                 age_cat = means[7],
                 gender = means[8],
                 edu_f = means[9],
                 election_factor = means[10])
# calculate predicted probabilities for each value of competence_dem_gm
predictors <- ggpredict(model301_rob, terms = "competence_dem_gm", conditions = df, type = "fe", ci.lvl = 0.95)
# plot the predicted probabilities
plot_1a<-ggplot(predictors, aes(x, predicted, ymin = conf.low, ymax = conf.high)) +
  geom_line(size = 1.2) +
  geom_ribbon(alpha = 0.3) +
  labs(x = "Democratic candidates: Competence", y = "Pr(turnout)") +
  ggtitle("Self-reported") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), legend.position = "bottom")

predictors <- model.matrix(model301_val)[,-1]
means <- colMeans(predictors)
df <- data.frame(competence_dem_gm = seq(-0.536899615754083, 0.463100384245917, length.out = 100),
                 competence_rep_gm = means[1],
                 character_dem_gm = means[2],
                 character_rep_gm = means[3],
                 comp_diff_gm = means[4],
                 char_diff_gm = means[5],
                 PID_strength_gm = means[6],
                 age_cat = means[7],
                 gender = means[8],
                 edu_f = means[9],
                 election_factor = means[10])
# calculate predicted probabilities for each value of competence_dem_gm
predictors <- ggpredict(model301_val, terms = "competence_dem_gm", conditions = df, type = "fe", ci.lvl = 0.95)
# plot the predicted probabilities
plot_1b<-ggplot(predictors, aes(x, predicted, ymin = conf.low, ymax = conf.high)) +
  geom_line(size = 1.2) +
  geom_ribbon(alpha = 0.3) +
  labs(x = "Democratic candidates: Competence", y = "") +
  ggtitle("Validated") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), legend.position = "bottom")
# For the self-reported plot
plot_1a <- plot_1a + ylim(0.2, 0.8)
# For the validated plot
plot_1b <- plot_1b + ylim(0.2, 0.8)

grid.arrange(plot_1a, plot_1b, ncol = 2)

# The effects of Character_rep_gm on turnout (SELF REPORT)
predictors <- model.matrix(model301_rob)[,-1]
means <- colMeans(predictors)
df <- data.frame(character_rep_gm = seq(-0.454938859774496, 0.545061140225504, length.out = 100),
                 competence_dem_gm = means[1],
                 competence_rep_gm = means[2],
                 character_dem_gm = means[3],
                 comp_diff_gm = means[4],
                 char_diff_gm = means[5],
                 PID_strength_gm = means[6],
                 age_cat = means[7],
                 gender = means[8],
                 edu_f = means[9],
                 election_factor = means[10])
# calculate predicted probabilities for each value of competence_dem_gm
predictors <- ggpredict(model301_rob, terms = "character_rep_gm", conditions = df, type = "fe", ci.lvl = 0.95)
# plot the predicted probabilities
plot_4a<-ggplot(predictors, aes(x, predicted, ymin = conf.low, ymax = conf.high)) +
  geom_line(size = 1.2) +
  geom_ribbon(alpha = 0.3) +
  labs(x = "Republican candidates: Character", y = "Pr(turnout)") +
  ggtitle("Self-reported") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), legend.position = "bottom")

# The effects of Character_rep_gm on turnout (VALIDATED)
predictors <- model.matrix(model301_val)[,-1]
means <- colMeans(predictors)
df <- data.frame(character_rep_gm = seq(-0.454938859774496, 0.545061140225504, length.out = 100),
                 competence_dem_gm = means[1],
                 competence_rep_gm = means[2],
                 character_dem_gm = means[3],
                 comp_diff_gm = means[4],
                 char_diff_gm = means[5],
                 PID_strength_gm = means[6],
                 age_cat = means[7],
                 gender = means[8],
                 edu_f = means[9],
                 election_factor = means[10])
# calculate predicted probabilities for each value of competence_dem_gm
predictors <- ggpredict(model301_val, terms = "character_rep_gm", conditions = df, type = "fe", ci.lvl = 0.95)
# plot the predicted probabilities
plot_5a<-ggplot(predictors, aes(x, predicted, ymin = conf.low, ymax = conf.high)) +
  geom_line(size = 1.2) +
  geom_ribbon(alpha = 0.3) +
  labs(x = "Republican candidates: Character", y = "") +
  ggtitle("Validated") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), legend.position = "bottom")

# For the self-reported plot
plot_4a <- plot_4a + ylim(0.2, 0.7)
# For the validated plot
plot_5a <- plot_5a + ylim(0.2, 0.7)
grid.arrange(plot_4a, plot_5a, ncol = 2)

