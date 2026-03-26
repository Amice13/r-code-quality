
library(tidyverse)
library(list)

##Import the dataset
data <- read.csv("replication_hataandogura(2024).csv")

##Check the distribution of variables used (DQ and experimental group)
table(data$CT_DQ)
table(data$List_control_1)
table(data$List_treatment_1)

#Experimental group variable (exp)
data <- data %>% 
  mutate(exp = 
           case_when(!is.na(List_control_1) ~ 0,
                     !is.na(List_treatment_1) ~ 1,
           ))

#Outcome Variavble
data <- data %>% 
  mutate(outcome =
           case_when(List_control_1 == 0 | List_treatment_1 == 0 ~ 0,
                     List_control_1 == 1 | List_treatment_1 == 1 ~ 1,
                     List_control_1 == 2 | List_treatment_1 == 2 ~ 2,
                     List_control_1 == 3 | List_treatment_1 == 3 ~ 3,
                     List_treatment_1 == 4 ~ 4,
           ))

##Covariates (Party Identities:PID, Leaner:PID_I)
table(data$PID_I)

data <- data %>% 
  mutate(pid = 
           case_when(PID == 1 | PID_I == 1  ~ "Democrat",
                     PID == 2 | PID_I == 2 ~ "Republican",
                     TRUE ~ NA_character_,
           )) %>% 
  mutate(pid = factor(pid, levels = c("Democrat","Republican")))

##Estimate using `lm`, `ml`, and `nls` functions from the `{list}` package
###LM
all_lm <- ictreg(outcome ~ 1,
               data = data, method = "lm", treat = "exp",
               J = 3, overdispersed = FALSE, constrained = TRUE)

ave_all_lm <- predict(all_lm,
                    newdata = as.data.frame(matrix(1, 1, 1)), se.fit = TRUE,
                    avg = TRUE)

dem_lm <- ictreg(outcome ~ 1,
               data = data[data$pid == "Democrat",], method = "lm", treat = "exp",
               J = 3, overdispersed = FALSE, constrained = TRUE)

ave_dem_lm <- predict(dem_lm,
                    newdata = as.data.frame(matrix(1, 1, 1)), se.fit = TRUE,
                    avg = TRUE)

rep_lm <- ictreg(outcome ~ 1,
               data = data[data$pid == "Republican",], method = "lm", treat = "exp",
               J = 3, overdispersed = FALSE, constrained = TRUE)

ave_rep_lm <- predict(rep_lm,
                    newdata = as.data.frame(matrix(1, 1, 1)), se.fit = TRUE,
                    avg = TRUE)

lm1 <- ave_all_lm$fit
lm2 <- ave_dem_lm$fit
lm3 <- ave_rep_lm$fit

lmse1 <- ave_all_lm$se.fit
lmse2 <- ave_dem_lm$se.fit
lmse3 <- ave_rep_lm$se.fit

LM_df <- data.frame(estimate = c(lm1, lm2, lm3),
                se = c(lmse1, lmse2, lmse3),
                pid = c("All respondents","Democrat","Republican"),
                group = "LM")

####ML
all_ml <- ictreg(outcome ~ 1,
              data = data, method = "ml", treat = "exp",
              J = 3, overdispersed = FALSE, constrained = TRUE)

ave_all_ml <- predict(all_ml,
                   newdata = as.data.frame(matrix(1, 1, 1)), se.fit = TRUE,
                   avg = TRUE)

dem_ml <- ictreg(outcome ~ 1,
              data = data[data$pid == "Democrat",], method = "ml", treat = "exp",
              J = 3, overdispersed = FALSE, constrained = TRUE)

ave_dem_ml <- predict(dem_ml,
                   newdata = as.data.frame(matrix(1, 1, 1)), se.fit = TRUE,
                   avg = TRUE)

rep_ml <- ictreg(outcome ~ 1,
              data = data[data$pid == "Republican",], method = "ml", treat = "exp",
              J = 3, overdispersed = FALSE, constrained = TRUE)

ave_rep_ml <- predict(rep_ml,
                   newdata = as.data.frame(matrix(1, 1, 1)), se.fit = TRUE,
                   avg = TRUE)

ml1 <- ave_all_ml$fit
ml2 <- ave_dem_ml$fit
ml3 <- ave_rep_ml$fit

mlse1 <- ave_all_ml$se.fit
mlse2 <- ave_dem_ml$se.fit
mlse3 <- ave_rep_ml$se.fit

ML_df <- data.frame(estimate = c(ml1, ml2, ml3), 
                se = c(mlse1, mlse2, mlse3),
                pid = c("All respondents","Democrat","Republican"),
                group = "ML")



####NLS
all_nls <- ictreg(outcome ~ 1,
               data = data, method = "nls", treat = "exp",
               J = 3, overdispersed = FALSE, constrained = TRUE)

ave_all_nls <- predict(all_nls,
                    newdata = as.data.frame(matrix(1, 1, 1)), se.fit = TRUE,
                    avg = TRUE)

dem_nls <- ictreg(outcome ~ 1,
               data = data[data$pid == "Democrat",], method = "nls", treat = "exp",
               J = 3, overdispersed = FALSE, constrained = TRUE)

ave_dem_nls <- predict(dem_nls,
                    newdata = as.data.frame(matrix(1, 1, 1)), se.fit = TRUE,
                    avg = TRUE)

rep_nls <- ictreg(outcome ~ 1,
               data = data[data$pid == "Republican",], method = "nls", treat = "exp",
               J = 3, overdispersed = FALSE, constrained = TRUE)

ave_rep_nls <- predict(rep_nls,
                    newdata = as.data.frame(matrix(1, 1, 1)), se.fit = TRUE,
                    avg = TRUE)

nls1 <- ave_all_nls$fit
nls2 <- ave_dem_nls$fit
nls3 <- ave_rep_nls$fit

nlsse1 <- ave_all_nls$se.fit
nlsse2 <- ave_dem_nls$se.fit
nlsse3 <- ave_rep_nls$se.fit

NLS_df <- data.frame(estimate = c(nls1, nls2, nls3),
                se = c(nlsse1, nlsse2, nlsse3),
                pid = c("All respondents","Democrat","Republican"),
                group = "NLS")



##DQ
All_dq <- data %>% 
  mutate(dq = if_else(CT_DQ == 2, 0, 1)) %>% 
  drop_na(dq) %>% 
  summarise(estimate = mean(dq, na.rm = T),
            se = sd(dq)/sqrt(length(dq))) %>% 
  mutate(group = "DQ",
         pid = "All respondents") %>% 
  select(estimate, se, pid, group)

PID_dq <- data %>% 
  mutate(dq = if_else(CT_DQ == 2, 0, 1)) %>% 
  drop_na(pid,dq) %>% 
  group_by(pid) %>% 
  summarise(estimate = mean(dq, na.rm = T),
            se = sd(dq)/sqrt(length(dq))) %>% 
  mutate(group = "DQ") %>% 
  select(estimate, se, pid, group)

##Combine five data frames
Combine_df_95 <- rbind(LM_df, ML_df, NLS_df, All_dq, PID_dq) %>% 
  mutate(ci = 1.96 * se, #95% confidence interval
         group = factor(group, levels = c("DQ","LM","ML","NLS")),
         color = if_else(group == "DQ", "T", "F"),
         team = "95%CI")

Combine_df_83 <- rbind(LM_df, ML_df, NLS_df, All_dq, PID_dq) %>% 
  mutate(ci = 1.39 * se, #83.4% confidence interval
         group = factor(group, levels = c("DQ","LM","ML","NLS")),
         color = if_else(group == "DQ", "T", "F"),
         team = "83.4%CI")

Combine_df <- rbind(Combine_df_95, Combine_df_83) %>% 
  mutate(pid1 = 
           case_when(pid == "All respondents" ~ "All respondents \n (n=613(DQ)/1217(LE))",
                     pid == "Democrat" ~ "Democrat \n (n=288(DQ)/589(LE))",
                     pid == "Republican" ~ "Republican \n (n=230(DQ)/589(LE))")) #Rename



#Plot with both 95% and 83.4% confidence intervals (LM and DQ only, Figure 1)
Combine_df_RR <- Combine_df %>% 
  filter(group == "DQ" | group == "LM")

Fig_1 <- Combine_df_RR %>% 
  mutate(team = fct_rev(team)) %>% 
  ggplot(aes(pid1, estimate, shape = group, color = color)) +
  geom_point(position = position_dodge(.6),
             size = 3) +
  geom_errorbar(data = Combine_df_RR %>% filter(team == "83.4%CI"),
                aes(ymin = estimate - ci, ymax = estimate + ci),
                position = position_dodge(.6),
                width = 0.1,
                size = 1) +
  geom_errorbar(data = Combine_df_RR %>% filter(team == "95%CI"),
                aes(ymin = estimate - ci, ymax = estimate + ci),
                position = position_dodge(.6),
                width = 0.1,
                size = .5) +
  geom_text(Combine_df_RR　%>% filter(group == "DQ"),
            mapping=aes(x = pid1, y = estimate,
                        label = sprintf("%.3f",estimate)),size = 4,
            color="black",nudge_x = 0, nudge_y = 0, segment.size = 0) + 
  geom_text(Combine_df_RR　%>% filter(group == "LM"),
            mapping=aes(x = pid1, y = estimate,
                        label = sprintf("%.3f",estimate)),size = 4,
            color="black",nudge_x = .35, nudge_y = 0, segment.size = 0) + 
  theme_bw(base_size = 14, base_family = "HiraginoSans-W3") + 
  labs(x = "", y = "Estimated Probabilities/Difference",
       shape = "Estimate \n Method") + 
  scale_color_manual(values = c("T" = "red",
                                "F" = "black")) + 
  guides(color = "none")


Fig_1




##############################################################################################################################
##Appendix####################################################################################################################
##############################################################################################################################

##Balance Test(Figure C.1)


##Covariates (Gender:Q41, Age:Q42, Race:race, Region of residence:Q48_1, Education level:educ)
data <- data %>% 
  mutate(Gender = if_else(Q41==1, 1,0),
         Age = as.numeric(Q42),
         White = if_else(race == 1, 1, 0),
         Black = if_else(race == 2, 1, 0),
         Hispanic = if_else(race == 3, 1, 0),
         Other_ethnicity = if_else(race == 1|race == 2|race == 3, 0, 1),
         Northeast = if_else(Q48_1 == 1, 1, 0),
         West = if_else(Q48_1 == 11, 1, 0),
         Midwest = if_else(Q48_1 == 25, 1, 0),
         South = if_else(Q48_1 == 38, 1, 0),
         Education = if_else(educ == 1|educ == 2, 0, 1))

library(BalanceR) #R Package for balance tests

bt_df <- data %>% 
  mutate(exp =
           case_when(!is.na(List_control_1) ~ "Control",
                     !is.na(List_treatment_1) ~ "Treatment",
                     !is.na(CT_DQ) ~ "DQ"),
         exp = factor(exp,
                       levels = c("DQ","Control","Treatment")),
         Democrat =if_else(pid == "Democrat", 1, 0),
         Republican =if_else(pid == "Republican", 1, 0),
         Education = as.numeric(educ))

balancetest <- bt_df %>% 
  BalanceR(group = exp,
           cov   = c(Gender, Age, Education, White, Black, Hispanic,Other_ethnicity,
                     Northeast,West,Midwest,Northeast,South, Democrat, Republican)) %>%
  plot(point.size = 3, text.size = 15,color = FALSE) +
  theme_bw(base_size = 14, base_family = "HiraginoSans-W3") + 
  theme(legend.position = "bottom")

balancetest



##Results of Estimation including Covariates（Figure D.1)


###LM with covariates
all_lm_cov <- ictreg(outcome ~  Gender + Age + White + Black + Hispanic + Education + order + Midwest + West + South,
               data = data, method = "lm", treat = "exp",
               J = 3)

ave_all_lm_cov <- predict(all_lm_cov,
                    se.fit = TRUE,
                    avg = TRUE)


dem_lm_cov <- ictreg(outcome ~  Gender + Age + White + Black + Hispanic + Education + order + Midwest + West + South,
               data = data[data$pid == "Democrat",], method = "lm", treat = "exp",
               J = 3)

ave_dem_lm_cov <- predict(dem_lm_cov,
                    se.fit = TRUE,
                    avg = TRUE)

rep_lm_cov <- ictreg(outcome ~  Gender + Age + White + Black + Hispanic + Education + order + Midwest + West + South,
               data = data[data$pid == "Republican",], method = "lm", treat = "exp",
               J = 3)

ave_rep_lm_cov <- predict(rep_lm_cov,
                    se.fit = TRUE,
                    avg = TRUE)

lm1_cov <- ave_all_lm_cov$fit
lm2_cov <- ave_dem_lm_cov$fit
lm3_cov <- ave_rep_lm_cov$fit
lmse1_cov <- ave_all_lm_cov$se.fit
lmse2_cov <- ave_dem_lm_cov$se.fit
lmse3_cov <- ave_rep_lm_cov$se.fit

lm_cov_df <- data.frame(estimate = c(lm1_cov, lm2_cov, lm3_cov),
                se = c(lmse1_cov, lmse2_cov, lmse3_cov),
                pid = c("All respondents","Democrat","Republican"),
                group = "LM")
lm_cov_df


#Combine three data frames
Combine_df_cov_95 <- rbind(lm_cov_df, All_dq, PID_dq) %>% 
  mutate(ci = 1.96 * se, #95% confidence interval
         group = factor(group, levels = c("DQ","LM")),
         color = if_else(group == "DQ", "T", "F"),
         team = "95%CI")

Combine_df_cov_83 <- rbind(lm_cov_df, All_dq, PID_dq) %>% 
  mutate(ci = 1.39 * se, #83.4% confidence interval
         group = factor(group, levels = c("DQ","LM")),
         color = if_else(group == "DQ", "T", "F"),
         team = "83.4%CI")

Combine_df_cov <- rbind(Combine_df_cov_95, Combine_df_cov_83) %>% 
  mutate(pid1 = 
           case_when(pid == "All respondents" ~ "All respondents \n (n=613(DQ)/1217(LE))",
                     pid == "Democrat" ~ "Democrat \n (n=288(DQ)/589(LE))",
                     pid == "Republican" ~ "Republican \n (n=230(DQ)/589(LE))"))


Fig_D_1 <- Combine_df_cov %>% 
  mutate(team = fct_rev(team)) %>% 
  ggplot(aes(pid1, estimate, shape = group, color = color)) +
  geom_point(position = position_dodge(.6),
             size = 3) +
  geom_errorbar(data = Combine_df_cov %>% filter(team == "83.4%CI"),
                aes(ymin = estimate - ci, ymax = estimate + ci),
                position = position_dodge(.6),
                width = 0.1,
                size = 1) +
  geom_errorbar(data = Combine_df_cov %>% filter(team == "95%CI"),
                aes(ymin = estimate - ci, ymax = estimate + ci),
                position = position_dodge(.6),
                width = 0.1,
                size = .5) +
  geom_text(Combine_df_cov　%>% filter(group == "DQ"),
            mapping=aes(x = pid1, y = estimate,
                        label = sprintf("%.2f",estimate)),size = 4,
            color="black", nudge_x = -0.25, segment.size = 0) + 
  geom_text(Combine_df_cov　%>% filter(group == "LM"),
            mapping=aes(x = pid1, y = estimate,
                        label = sprintf("%.2f",estimate)),
            size = 4,
            color="black", nudge_x = 0.25, segment.size = 0) + 
  theme_bw(base_size = 14, base_family = "HiraginoSans-W3") + 
  labs(x = "", y = "Estimated Probabilities/Difference",
       shape = "Estimate \n Method") + 
  scale_color_manual(values = c("T" = "red",
                                "F" = "black")) + 
  guides(color = "none")


Fig_D_1


##Plot with both 95% and 83.4% confidence intervals (for all estimation methods; Figure E.1)

Fig_E_1 <- Combine_df %>% 
  mutate(team = fct_rev(team)) %>% 
  ggplot(aes(pid1, estimate, shape = group, color = color)) +
  geom_point(position = position_dodge(.6),
             size = 3) +
  geom_errorbar(data = Combine_df %>% filter(team == "83.4%CI"),
                aes(ymin = estimate - ci, ymax = estimate + ci),
                position = position_dodge(.6),
                width = 0.1,
                size = 1) +
  geom_errorbar(data = Combine_df %>% filter(team == "95%CI"),
                aes(ymin = estimate - ci, ymax = estimate + ci),
                position = position_dodge(.6),
                width = 0.1,
                size = .5) +
  geom_text(Combine_df　%>% filter(group == "DQ"),
            mapping=aes(x = pid1, y = estimate,
                        label = sprintf("%.3f",estimate)),size = 4,
            color="black",nudge_x = -0.22, nudge_y = 0.1, segment.size = 0) + 
  geom_text(Combine_df　%>% filter(group == "LM"),
            mapping=aes(x = pid1, y = estimate,
                        label = sprintf("%.3f",estimate)),size = 4,
            color="black",nudge_x = -0.068, nudge_y = 0.155, segment.size = 0) + 
  geom_text(Combine_df　%>% filter(group == "ML"),
            mapping=aes(x = pid1, y = estimate,
                        label = sprintf("%.3f",estimate)),size = 4,
            color="black",nudge_x = 0.068, nudge_y = 0.15, segment.size = 0) + 
  geom_text(Combine_df　%>% filter(group == "NLS"),
            mapping=aes(x = pid1, y = estimate,
                        label = sprintf("%.3f",estimate)),size = 4,
            color="black",nudge_x = 0.22, nudge_y = 0.15, segment.size = 0) + 
  theme_bw(base_size = 14, base_family = "HiraginoSans-W3") + 
  labs(x = "", y = "Estimated Probabilities/Difference",
       shape = "Estimate \n Method") + 
  scale_color_manual(values = c("T" = "red",
                                "F" = "black")) + 
  guides(color = "none")

Fig_E_1


