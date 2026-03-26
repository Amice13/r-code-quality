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


#### APPENDIX ####

##### in-text analysis #####
# difference in wave 2 and wave 3 fusionLeader
t.test(d$fusionLeader_w4, d$fusionLeader_w3, paired = T)

# difference in wave 2 and wave 3 fusionUS
t.test(d$fusionUS_w4, d$fusionUS_w3, paired = T)

# difference in wave 2 and wave 3 big lie
t.test(d$electionFraud_w3, d$electionFraud_w4, paired = T)

# support for investigating Biden above midpoint
t.test(d$trump_priorities_10, mu = 4)

# effect of big lie on cases in SDs
.42/sd(d$cases, na.rm = T)

# effect of big lie on policy in SDs
.36/sd(d$trump_priorities, na.rm = T)

# effect of delta big lie on policy in SDs
.33/sd(d$trump_priorities, na.rm = T)

# percent of big lie belivers who and believe Trump's innocent4
prop.table(table(d$innocent > 4, d$electionFraud_w4 >= 4.333), margin = 2)


##### Table A1 #####

## need to create indicators for who attritted and when ##
# identify attrition groups
wave4takers <- dl$workerId2[dl$wave == 4]
wave3takers <- dl$workerId2[dl$wave == 3]
wave2takers <- dl$workerId2[dl$wave == 2]
wave3attritters <- setdiff(wave2takers, wave3takers)
wave4attritters <- setdiff(wave2takers, wave4takers)
wave2stayers <- intersect(wave2takers, wave4takers)

# attritted-in-wave indicator
dl <- mutate(dl, 
             attrittedw3 = workerId2 %in% wave3attritters,
             attrittedw4 = workerId2 %in% wave4attritters,
             attrition_g = case_when(
                   attrittedw3 == F  & attrittedw4 == F ~ "Wave 3",
                   attrittedw3 == T ~ "Wave 1",
                   TRUE ~ "Wave 2"
             ))

# fill in missing gender values for later waves
dl <- dl |> group_by(workerId2) |>
      fill(race) |>
      ungroup()

# generate table
tbl1 <- dl |>
      mutate(female = ifelse(gender == 2, 1, 0),
             white = ifelse(race == "white", 1, 0),
             wave = case_match(
                   wave,
                   2 ~ "Wave 1", 
                   3 ~ "Wave 2", 
                   4 ~ "Wave 3")) |>
      select(female, age, education, white, fusionLeader, electionFraud, 
             APDems_4, outgroupThreat, wave,
             politicalOrientation) |>
      tbl_summary(by = wave,
                  type = list(politicalOrientation ~ "continuous",
                              education ~ "continuous"),
                  label = list(female ~ "Female",
                               age ~ "Age",
                               education ~ "Education",
                               white ~ "White",
                               fusionLeader ~ "Trump Fusion",
                               electionFraud ~ "Big Lie Belief",
                               APDems_4 ~ "Affect toward Democrats",
                               outgroupThreat ~ "Outgroup Threat",
                               politicalOrientation ~ "Ideology"),
                  statistic = list(
                        all_continuous() ~ "{mean} ({sd})",
                        all_categorical() ~ "{p}%"),
                  missing = "no") |>
      modify_header(label = "Variable") |>
      bold_labels()

# look at it
tbl1

# the "Retained from Wave 1" row quantities were added by hand
# Wave 2 
round(284/402 * 100)

# Wave 3
round(130/402 * 100)

# save it
gtsave(as_gt(tbl1), filename = "wave_descriptives.tex")

##### Table A2 #####

# generate table
tbl2 <- dl |>
      filter(wave == 2) |>
      mutate(female = ifelse(gender == 2, 1, 0),
             white = ifelse(race == "white", 1, 0),
             wave = case_match(
                   wave,
                   2 ~ "Wave 1", 
                   3 ~ "Wave 2", 
                   4 ~ "Wave 3")) |>
      select(female, age, education, white, fusionLeader, 
             APDems_4, outgroupThreat, attrittedw4,
             politicalOrientation) |>
      mutate(attrittedw4 = ifelse(attrittedw4 == T, 
                                  "Attritted", "Did Not Attrit")) |>
      tbl_summary(by = attrittedw4,
                  type = list(politicalOrientation ~ "continuous",
                              education ~ "continuous"),
                  label = list(female ~ "Female",
                               age ~ "Age",
                               education ~ "Education",
                               white ~ "White",
                               fusionLeader ~ "Trump Fusion",
                               APDems_4 ~ "Affect toward Democrats",
                               outgroupThreat ~ "Outgroup Threat",
                               politicalOrientation ~ "Ideology"),
                  statistic = list(
                        all_continuous() ~ "{mean} ({sd})",
                        all_categorical() ~ "{p}%"),
                  missing = "no") |>
      modify_header(label = "Variable") |>
      bold_labels()

# look at it
tbl2

# save it
gtsave(as_gt(tbl2), filename = "wave1_descriptives_by_attrition.tex")

##### Table B3 #####
fusionDJT2 <- lm(fusionLeaderChange34 ~ fusionUSChange34 + 
                       #    outgroupThreatChange34 + 
                       electionFraudChange34 +
                       APDemsChange34 + 
                       fusionLeader_w2 + fusionUS_w2 +
                       APDems_4_w2 +
                       #    outgroupThreat_w2 + 
                       DemocracyFaith_w2 + supportJan6_w3 + 
                       electionFraud_w3 + 
                       authoritarianActionsSelf_w2 + politicalOrientation_w2, d)

lie2 <- lm(electionFraudChange34 ~ fusionLeaderChange34 + fusionUSChange34 + 
                 #   outgroupThreatChange34 + 
                 APDemsChange34 + 
                 fusionLeader_w2 + fusionUS_w2 +
                 APDems_4_w2 +
                 #   outgroupThreat_w2 + 
                 DemocracyFaith_w2 + supportJan6_w3 + 
                 electionFraud_w3 + 
                 authoritarianActionsSelf_w2 + politicalOrientation_w2, d)

crimes2 <- lm(cases ~ fusionUSChange34 + fusionLeaderChange34 +
                    #       outgroupThreatChange34 + 
                    electionFraudChange34 +
                    APDemsChange34 + 
                    fusionLeader_w2 + fusionUS_w2 +
                    APDems_4_w2 +
                    #      outgroupThreat_w2 + 
                    DemocracyFaith_w2 + supportJan6_w3 + 
                    electionFraud_w3 + 
                    authoritarianActionsSelf_w2 + politicalOrientation_w2, d)

policies2 <- lm(trump_priorities ~ fusionUSChange34 + fusionLeaderChange34 +
                      #        outgroupThreatChange34 + 
                      electionFraudChange34 +
                      APDemsChange34 + 
                      fusionLeader_w2 + fusionUS_w2 +
                      APDems_4_w2 +
                      #        outgroupThreat_w2 + 
                      DemocracyFaith_w2 + supportJan6_w3 + 
                      electionFraud_w3 + 
                      authoritarianActionsSelf_w2 + politicalOrientation_w2, d)

# generate table
texreg(list(lie2, fusionDJT2, crimes2, policies2), 
       file = "app_table_therms.tex",
       custom.header = list("Change 2021-2024" = 1:2, "Levels in 2024" = 3:4),
       custom.model.names = c("Big Lie", "Trump Fusion", "Crimes", "Policies"),
       custom.coef.map = list("fusionLeaderChange34" = "$\\Delta$ Trump Fusion",
                              "fusionUSChange34" = "$\\Delta$ US Fusion",
                              "electionFraudChange34" = "$\\Delta$ Big Lie", 
                              "outgroupThreatChange34" = "$\\Delta$ Outgroup Threat",
                              "APDemsChange34" = "$\\Delta$ Out-party Hate",
                              "APDems_4_w2" = "Out-party Hate$_{\\text{Wave~ 1}}$",
                              "fusionLeader_w2" = "Trump Fusion$_{\\text{Wave~ 1}}$",
                              "fusionUS_w2" = "US Fusion$_{\\text{Wave~ 1}}$",
                              "electionFraud_w3" = "Big Lie$_{\\text{Wave~ 2}}$",
                              "outgroupThreat_w2" = "Outgroup Threat$_{\\text{Wave~ 1}}$",
                              "authoritarianActionsSelf_w2" = "Authoritarian Actions$_{\\text{Wave~ 1}}$",
                              "DemocracyFaith_w2" = "Faith in Democracy$_{\\text{Wave~ 1}}$",
                              "supportJan6_w3" = "Support for Jan. 6$_{\\text{Wave~ 2}}$",
                              "politicalOrientation_w2" = "Ideological ID$_{\\text{Wave~ 1}}$"), 
       include.rsquared = F, 
       threeparttable = T, 
       custom.note = "\n\\item %stars. \\item The dependent variables of the change models are first differences between wave 3 and wave 2. The dependent variable of the level models are values in wave 3. Standard errors in parentheses.",
       booktabs = T, use.packages = F, float.pos = "ht", 
       caption = "Alternative Linear Models of Change between 2021 and 2024 and Levels in 2024 with Out-Party Hate Instead of Perceived Out-Party Threat", 
       label = "tab:app_therms",
       groups = list("Change Variables" = 1:4, "Baseline Levels" = 5:12))



##### Table B4 #####

fusion_fe <- plm(fusionLeader ~ electionFraud + APDems_4 +
                       outgroupThreat + fusionUS + politicalOrientation, 
                 dl, index = c("workerId2", "wave"), 
                 subset = democrat == 0, 
                 model = "within")
summary(fusion_fe, vcov = vcovHC)

lie_fe <- plm(electionFraud ~ fusionLeader + APDems_4 +
                    outgroupThreat + fusionUS + politicalOrientation, 
              dl, index = c("workerId2", "wave"), 
              subset = democrat == 0, 
              model = "within")
summary(lie_fe, vcov = vcovHC)

texreg(list(fusion_fe, lie_fe), 
       custom.header = list("Change 2021-2024" = 1:2),
       file = "app_table_fe.tex",
       custom.model.names = c("Trump Fusion", "Big Lie"),
       custom.coef.map = list("fusionLeader" = "Trump Fusion",
                              "fusionUS" = "US Fusion",
                              "electionFraud" = "Big Lie", 
                              "APDems_4" = "Outparty Hate",
                              "politicalOrientation" = "Ideological ID"),
       override.se = list(c(summary(fusion_fe, vcov = vcovHC)[[1]][6:10]),
                          c(summary(lie_fe, vcov = vcovHC)[[1]][6:10])),
       override.pvalues = list(c(summary(fusion_fe, vcov = vcovHC)[[1]][16:20]),
                               c(summary(lie_fe, vcov = vcovHC)[[1]][16:20])),
       caption = "Fixed-Effects Linear Models of Trump Fusion and Big Lie",
       threeparttable = T, use.packages = F,
       custom.note = "\n\\item OLS estimates of fixed-effects models with cluster-robust standard errors in parentheses. Larger N reflects repeated measures of each respondent over time. \\item %stars.",
       label = "tab:app_fe",
       include.rsquared = F,
       include.adjrs = F)

##### Table B5 #####
fusionDJT_ldv <- lm(fusionLeader_w4 ~ electionFraud_w3 + electionFraud_w4 + fusionLeader_w3 + 
                          fusionUS_w3 + fusionUS_w4 + #APDems_4_w3 + APDems_4_w4 +
                          outgroupThreat_w3 + outgroupThreat_w4 + 
                          DemocracyFaith_w2 + supportJan6_w3 + 
                          authoritarianActionsSelf_w2 + politicalOrientation_w2, d)

lie_ldv <- lm(electionFraud_w4 ~ electionFraud_w3 + fusionLeader_w3 + fusionLeader_w4 + 
                    fusionUS_w3 + fusionUS_w4 + #APDems_4_w3 + APDems_4_w4 +
                    outgroupThreat_w3 + outgroupThreat_w4 + 
                    DemocracyFaith_w2 + supportJan6_w3 + 
                    authoritarianActionsSelf_w2 + politicalOrientation_w2, d)

# generate table
texreg(list(fusionDJT_ldv, lie_ldv), 
       custom.header = list("Change 2021-2024" = 1:2),
       file = "app_table_ldv.tex",
       custom.model.names = c("Trump Fusion", "Big Lie"),
       custom.coef.map = list("electionFraud_w3" = "Big Lie$_{\\text{Wave~ 2}}$",
                              "electionFraud_w4" = "Big Lie$_{\\text{Wave~ 3}}$",
                              "fusionLeader_w3" = "Trump Fusion$_{\\text{Wave~ 2}}$",
                              "fusionLeader_w4" = "Trump Fusion$_{\\text{Wave~ 3}}$",
                              "fusionUS_w3" = "US Fusion$_{\\text{Wave~ 2}}$",
                              "fusionUS_w4" = "US Fusion$_{\\text{Wave~ 3}}$",
                              "APDems_4_w3" = "Out-party Hate$_{\\text{Wave~ 2}}$",
                              "APDems_4_w4" = "Out-party Hate$_{\\text{Wave~ 3}}$",
                              "outgroupThreat_w3" = "Outgroup Threat$_{\\text{Wave~ 2}}$",
                              "outgroupThreat_w4" = "Outgroup Threat$_{\\text{Wave~ 3}}$",
                              "DemocracyFaith_w2" = "Faith in Democracy$_{\\text{Wave~ 1}}$",
                              "supportJan6_w3" = "Support for Jan. 6$_{\\text{Wave~ 2}}$",
                              "authoritarianActionsSelf_w2" = "Authoritarian Actions$_{\\text{Wave~ 1}}$",
                              "politicalOrientation_w2" = "Ideological ID$_{\\text{Wave~ 1}}$"),
       override.se = list(c(lmtest::coeftest(fusionDJT_ldv, vcov. = vcovHC)[13:24]),
                          c(lmtest::coeftest(lie_ldv, vcov. = vcovHC)[13:24])),
       override.pvalues = list(c(lmtest::coeftest(fusionDJT_ldv, vcov. = vcovHC)[37:48]),
                               c(lmtest::coeftest(lie_ldv, vcov. = vcovHC)[37:48])),
       caption = "Lagged Dependent Variable Linear Models of Trump Fusion and Big Lie", 
       threeparttable = T, use.packages = F,
       custom.note = "\n\\item OLS estimates of lagged dependent variable models with cluster-robust standard errors in parentheses. \\item %stars.",
       label = "tab:app_ldv",
       include.rsquared = F)


##### Figure C1 #####
d |> ggplot(aes(fusionLeader_w2)) +
      geom_histogram(binwidth = .1) + 
      labs(x = "Fusion with Trump at Wave 1", y = "Count") +
      theme_bw()
ggsave("histogram_fusion_w1.pdf", dpi = 900)

##### Figure C2 #####
d |> ggplot(aes(fusionLeader_w4)) +
      geom_histogram(binwidth = .1) + 
      labs(x = "Fusion with Trump at Wave 3", y = "Count") +
      theme_bw()
ggsave("histogram_fusion_w3.pdf", dpi = 900)

##### Figure C3 #####
d |> ggplot(aes(fusionLeaderChange34)) +
      geom_histogram(binwidth = .15) + 
      coord_cartesian(xlim = c(-6, 6)) +
      labs(x = "Change in Fusion with Trump from Wave 2 to Wave 3", y = "Count") +
      theme_bw()
ggsave("histogram_fusion_change.pdf", dpi = 900)

##### Figure C4 #####
d |> ggplot(aes(fusionUS_w2)) +
      geom_histogram(binwidth = .1) + 
      labs(x = "Fusion with Trump at Wave 1", y = "Count") +
      theme_bw()
ggsave("histogram_fusionUS_w1.pdf", dpi = 900)

##### Figure C5 #####
d |> ggplot(aes(fusionUS_w4)) +
      geom_histogram(binwidth = .1) + 
      labs(x = "Fusion with Trump at Wave 3", y = "Count") +
      theme_bw()
ggsave("histogram_fusionUS_w3.pdf", dpi = 900)

##### Figure C6 #####
d |> ggplot(aes(fusionUSChange34)) +
      geom_histogram(binwidth = .15) + 
      coord_cartesian(xlim = c(-6, 6)) +
      labs(x = "Change in Fusion with Trump from Wave 2 to Wave 3", y = "Count") +
      theme_bw()
ggsave("histogram_fusionUS_change.pdf", dpi = 900)

##### Figure C7 #####
d |> ggplot(aes(electionFraud_w3)) +
      geom_histogram(binwidth = .15) + 
      coord_cartesian(xlim = c(1, 6)) +
      labs(x = "Belief in Big Lie at Wave 2", y = "Count") +
      theme_bw()
ggsave("histogram_bbl_w2.pdf", dpi = 900)

##### Figure C8 #####
d |> ggplot(aes(electionFraud_w3)) +
      geom_histogram(binwidth = .15) + 
      coord_cartesian(xlim = c(1, 6)) +
      labs(x = "Belief in Big Lie at Wave 3", y = "Count") +
      theme_bw()
ggsave("histogram_bbl_w3.pdf", dpi = 900)

##### Figure C9 #####
d |> ggplot(aes(electionFraudChange34)) +
      geom_histogram(binwidth = .15) + 
      coord_cartesian(xlim = c(-6, 6)) +
      labs(x = "Change in Belief in Big Lie from Wave 2 to Wave 3", y = "Count") +
      theme_bw()
ggsave("histogram_bbl_change.pdf", dpi = 900)


##### Table D6 #####
ds2 <- pivot_wider(dl, id_cols = workerId2, names_from = wave, names_prefix = "w",
                   values_from = c(starts_with("fusionTrump"), 
                                   starts_with("fraud"), republican))
ds2 <- filter(ds2, republican_w2 == 1, !is.na(fusionTrump1_w3))

semmod <- sem('
            # measurement model #
            fusionLeader_w4 =~ fusionTrump1_w4 + fusionTrump2_w4 + fusionTrump3_w4
            fusionLeader_w3 =~ fusionTrump1_w3 + fusionTrump2_w3 + fusionTrump3_w3
            fusionLeader_w2 =~ fusionTrump1_w2 + fusionTrump2_w2 + fusionTrump3_w2
            
            electionFraud_w3 =~ fraud1_8_w3 + fraud2_8_w3 + fraud3_8_w3
            electionFraud_w4 =~ fraud1_8_w4 + fraud2_8_w4 + fraud3_8_w4
            
            # cross-lagged panel model #
            fusionLeader_w4 ~ fusionLeader_w3 + electionFraud_w3
            electionFraud_w4 ~ electionFraud_w3 + fusionLeader_w3
            ', data = ds2, missing = "fiml.x")

summary(semmod,
        ci = T,
        standardize = TRUE,
        fit.measures = TRUE)

