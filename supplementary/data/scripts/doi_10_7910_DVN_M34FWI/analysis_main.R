# install packages
install.packages("readr")
install.packages("tidyr")
install.packages("dplyr")
install.packages("plm")
install.packages("lavaan")
install.packages("lmtest")
install.packages("jtools")
install.packages("texreg")
install.packages("ggpubr")
install.packages("gt")
install.packages("gtsummary")

# load packages
library(readr)
library(tidyr)
library(dplyr)
library(plm)
library(lavaan)
library(lmtest)
library(jtools)
library(texreg)
library(ggpubr)
library(gt)
library(gtsummary)

#### read in datasets ####

# wide form
d <- read_csv("data_wide.csv")

# long form
dl <- read_csv("data_long.csv")


#### MAIN ANALYSIS ####

##### Table 1 #####

### change in fusion with Trump ##
fusionDJT <- lm(fusionLeaderChange34 ~ fusionUSChange34 + 
                      outgroupThreatChange34 + 
                      electionFraudChange34 +
                      fusionLeader_w2 + fusionUS_w2 +
                      outgroupThreat_w2 + 
                      DemocracyFaith_w2 + supportJan6_w3 + 
                      electionFraud_w3 + 
                      authoritarianActionsSelf_w2 + politicalOrientation_w2, d)

# change in fusion w/ Trump from wave 2 to 3
t.test(d$fusionLeader_w3, d$fusionLeader_w4, paired = T)

## Big Lie ##
lie <- lm(electionFraudChange34 ~ fusionLeaderChange34 + fusionUSChange34 + 
                outgroupThreatChange34 + 
                fusionLeader_w2 + fusionUS_w2 +
                outgroupThreat_w2 + DemocracyFaith_w2 + supportJan6_w3 + 
                electionFraud_w3 +
                authoritarianActionsSelf_w2 + politicalOrientation_w2, d)

# change in belief in big lie from wave 2 to 3
t.test(d$electionFraud_w3, d$electionFraud_w4, paired = T)

## Trump's legal trouble ##
crimes <- lm(cases ~ fusionLeaderChange34 + fusionUSChange34 +
                   outgroupThreatChange34 + electionFraudChange34 +
                   fusionLeader_w2 + fusionUS_w2 +
                   outgroupThreat_w2 + DemocracyFaith_w2 + supportJan6_w3 + 
                   electionFraud_w3 + 
                   authoritarianActionsSelf_w2 + politicalOrientation_w2, d)

## policy priorities ##
policies <- lm(trump_priorities ~ fusionLeaderChange34 + fusionUSChange34 + 
                     outgroupThreatChange34 + electionFraudChange34 +
                     fusionLeader_w2 + fusionUS_w2 +
                     outgroupThreat_w2 + DemocracyFaith_w2 + supportJan6_w3 + 
                     electionFraud_w3 +
                     authoritarianActionsSelf_w2 + politicalOrientation_w2, d)

# generate table
texreg(list(lie, fusionDJT, crimes, policies), 
       file = "main_table.tex",
       custom.header = list("Change 2021-2024" = 1:2, "Levels in 2024" = 3:4),
       custom.model.names = c("Big Lie", "Trump Fusion", "Crimes", "Policies"),
       custom.coef.map = list("fusionLeaderChange34" = "$\\Delta$ Trump Fusion",
                              "fusionUSChange34" = "$\\Delta$ US Fusion",
                              "electionFraudChange34" = "$\\Delta$ Big Lie", 
                              "outgroupThreatChange34" = "$\\Delta$ Outgroup Threat",
                              "fusionLeader_w2" = "Trump Fusion$_{\\text{Wave~ 1}}$",
                              "fusionUS_w2" = "US Fusion$_{\\text{Wave~ 1}}$",
                              "electionFraud_w3" = "Big Lie$_{\\text{Wave~ 2}}$",
                              "outgroupThreat_w2" = "Outgroup Threat$_{\\text{Wave~ 1}}$",
                              "authoritarianActionsSelf_w2" = "Authoritarian Actions$_{\\text{Wave~ 1}}$",
                              "DemocracyFaith_w2" = "Faith in Democracy$_{\\text{Wave~ 1}}$",
                              "supportJan6_w3" = "Support for Jan. 6$_{\\text{Wave~ 2}}$",
                              "politicalOrientation_w2" = "Ideological ID$_{\\text{Wave~ 1}}$"), 
       include.rsquared = F,
       booktabs = T, use.packages = F, float.pos = "ht", 
       caption = "Linear Models of Change between 2021 and 2024 and Levels in 2024", 
       threeparttable = T, 
       custom.note = "\n\\item %stars. \\item The dependent variables of the change models are first differences between wave 3 and wave 2. The dependent variable of the level models are values in wave 3. Standard errors in parentheses.",
       label = "tab:main",
       groups = list("Change Variables" = 1:4, "Baseline Levels" = 5:12))


##### Figure 1 #####
df_p1 <- make_predictions(fusionDJT, pred = "electionFraudChange34", partial.residuals = T) 
df_p2 <- make_predictions(crimes, pred = "electionFraudChange34", partial.residuals = T) 
df_p3 <- make_predictions(policies, pred = "electionFraudChange34", partial.residuals = T) 
df_p4 <- make_predictions(lie, pred = "fusionLeader_w2", partial.residuals = T) 

p1 <- ggplot(df_p1$predictions, aes(electionFraudChange34, fusionLeaderChange34)) + 
      geom_line() + 
      geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = .2) +
      geom_point(aes(electionFraudChange34, fusionLeaderChange34), 
                 data = df_p1$data, alpha = .2, position = "jitter") +
      labs(x = "Change in Belief in the Big Lie", y = "", 
           subtitle = "Change in Fusion with Trump") +
      theme_bw(base_size = 12)
p1


p2 <- ggplot(df_p2$predictions, aes(electionFraudChange34, cases)) + 
      geom_line() + 
      geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = .2) +
      geom_point(aes(electionFraudChange34, cases), 
                 data = df_p2$data, alpha = .2, position = "jitter") +
      labs(x = "Change in Belief in the Big Lie", y = "",
           subtitle = "Belief in Trump's Innoncence") +
      theme_bw(base_size = 12)
p2


p3 <- ggplot(df_p3$predictions, aes(electionFraudChange34, trump_priorities)) + 
      geom_line() + 
      geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = .2) +
      geom_point(aes(electionFraudChange34, trump_priorities), 
                 data = df_p3$data, alpha = .2, position = "jitter") +
      labs(x = "Change in Belief in the Big Lie", y = "",
           subtitle = "Support for Trump's Policy Priorities") +
      theme_bw(base_size = 12)
p3


p4 <- ggplot(df_p4$predictions, aes(fusionLeader_w2, electionFraudChange34)) + 
      geom_line() + 
      geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = .2) +
      geom_point(aes(fusionLeader_w2, electionFraudChange34), 
                 data = df_p4$data, alpha = .2, position = "jitter") +
      labs(x = "Baseline Fusion with Trump", y = "",
           subtitle = "Change in Belief in the Big Lie") +
      theme_bw(base_size = 12)
p4

# arrange the four plots into a grid
ggarrange(p4, p1, p2, p3, labels = "AUTO")

# save it
ggsave("results.pdf", dpi = 1000)



