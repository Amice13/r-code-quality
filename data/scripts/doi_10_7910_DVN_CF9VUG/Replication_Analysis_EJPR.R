## -------------------------------------------------------------------------
library(lme4)
library(emmeans)
library(sjPlot)
library(jtools)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(haven)
library(broom.mixed)
library(insight)
library(stargazer)


## -------------------------------------------------------------------------
Americas_analysis <- readRDS("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Analysis Data/Americas_analysis.rds")



## -------------------------------------------------------------------------
Americas_hyp1 <- lmer(Satisfaction ~ quota_effect +
                     Female + 
                     Ideology + 
                    Education + 
                    Income +
                    Age + 
                    Polity_score + 
                    GDP_capita_logged + 
                    women_rep_lag + 
                    Corruption +
                    Presidentialism +
                    (1 | Country_year), data = Americas_analysis)



## -------------------------------------------------------------------------
summary(Americas_hyp1)
saveRDS(Americas_hyp1, "Americas_hyp1.rds")


## -------------------------------------------------------------------------
Americas_hyp1_generation <- lmer(Satisfaction ~ quota_effect +
                     Female + 
                     Ideology + 
                    Education + 
                    Income +
                    Age + 
                    Generation + 
                    Polity_score + 
                    GDP_capita_logged + 
                    women_rep_lag + 
                    Corruption +
                    Presidentialism +
                    (1 | Country_year), data = Americas_analysis)



## -------------------------------------------------------------------------

summary(Americas_hyp1_generation)
saveRDS(Americas_hyp1_generation, "Americas_hyp1_generation.rds")



## -------------------------------------------------------------------------
Americas_hyp2 <- lmer(Satisfaction ~ quota_effect*Female + 
                     Ideology + 
                    Education + 
                    Income +
                    Age + 
                    Polity_score + 
                    GDP_capita_logged + 
                    women_rep_lag + 
                    Corruption +
                    Presidentialism +
                    (Female | Country_year), data = Americas_analysis)



## -------------------------------------------------------------------------
summary(Americas_hyp2)
saveRDS(Americas_hyp2, "Americas_hyp2.rds")



## -------------------------------------------------------------------------
Americas_hyp3a <- lmer(Satisfaction ~ quota_effect*Ideology +
                    Female + 
                    Education + 
                    Income +
                    Age + 
                    Polity_score + 
                    GDP_capita_logged + 
                    women_rep_lag + 
                    Corruption +
                    Presidentialism +
                    (Ideology | Country_year), 
                    data = Americas_analysis)



## -------------------------------------------------------------------------
summary(Americas_hyp3a)
saveRDS(Americas_hyp3a, "Americas_hyp3a.rds")



## -------------------------------------------------------------------------
Americas_hyp3b <- lmer(Satisfaction ~ quota_effect*Quota_support +
                      Female + 
                      Ideology +
                    Education + 
                    Income +
                    Age + 
                    Polity_score + 
                    GDP_capita_logged + 
                    women_rep_lag + 
                    Corruption +
                    Presidentialism +
                    (1 | Country_year), data = Americas_analysis)



## -------------------------------------------------------------------------
summary(Americas_hyp3b)
saveRDS(Americas_hyp3b, "Americas_hyp3b.rds")



## -------------------------------------------------------------------------
Americas_hyp4 <- lmer(Satisfaction ~ quota_effect*Corruption +
                     Female + 
                     Ideology + 
                    Education + 
                    Income +
                    Age + 
                    Polity_score + 
                    GDP_capita_logged + 
                    women_rep_lag + 
                    Presidentialism +
                    (1 | Country_year), data = Americas_analysis)




## -------------------------------------------------------------------------
summary(Americas_hyp4)
saveRDS(Americas_hyp4, "Americas_hyp4.rds")



## -------------------------------------------------------------------------
CSES_analysis <- readRDS("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Analysis Data/CSES_analysis.rds")



## -------------------------------------------------------------------------
CSES_hyp1 <- lmer(Satisfaction ~ quota_effect +
                     Female + 
                     Ideology + 
                    Education + 
                    Income +
                    Age + 
                    Polity_score + 
                    GDP_capita_logged + 
                    women_rep_lag + 
                    Corruption +
                    Presidentialism +
                    (1 | Country_year), data = CSES_analysis)



## -------------------------------------------------------------------------

summary(CSES_hyp1)
saveRDS(CSES_hyp1, "CSES_hyp1.rds")



## -------------------------------------------------------------------------
CSES_hyp1_generation <- lmer(Satisfaction ~ quota_effect +
                     Female + 
                     Ideology + 
                    Education + 
                    Income +
                    Age + 
                    Generation + 
                    Polity_score + 
                    GDP_capita_logged + 
                    women_rep_lag + 
                    Corruption +
                    Presidentialism +
                    (1 | Country_year), data = CSES_analysis)



## -------------------------------------------------------------------------

summary(CSES_hyp1_generation)
saveRDS(CSES_hyp1_generation, "CSES_hyp1_generation.rds")



## -------------------------------------------------------------------------
CSES_hyp2 <- lmer(Satisfaction ~ quota_effect*Female + 
                     Ideology + 
                    Education + 
                    Income +
                    Age + 
                    Polity_score + 
                    GDP_capita_logged + 
                    women_rep_lag + 
                    Corruption +
                    Presidentialism +
                    (Female | Country_year), data = CSES_analysis)



## -------------------------------------------------------------------------

summary(CSES_hyp2)
saveRDS(CSES_hyp2, "CSES_hyp2.rds")



## -------------------------------------------------------------------------
CSES_hyp3a <- lmer(Satisfaction ~ quota_effect*Ideology + 
                     Female + 
                    Education + 
                    Income +
                    Age + 
                    Polity_score + 
                    GDP_capita_logged + 
                    women_rep_lag + 
                    Corruption +
                    Presidentialism +
                    (Ideology | Country_year), data = CSES_analysis)



## -------------------------------------------------------------------------

summary(CSES_hyp3a)
saveRDS(CSES_hyp3a, "CSES_hyp3a.rds")



## -------------------------------------------------------------------------
CSES_hyp4 <- lmer(Satisfaction ~ quota_effect*Corruption +
                     Female + 
                     Ideology + 
                    Education + 
                    Income +
                    Age + 
                    Polity_score + 
                    GDP_capita_logged + 
                    women_rep_lag + 
                    Presidentialism +
                    (1 | Country_year), data = CSES_analysis)



## -------------------------------------------------------------------------

summary(CSES_hyp4)
saveRDS(CSES_hyp4, "CSES_hyp4.rds")



## -------------------------------------------------------------------------
Euro_analysis <- readRDS("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Analysis Data/Euro_analysis.rds")



## -------------------------------------------------------------------------
Euro_hyp1 <- lmer(Satisfaction ~ quota_effect + 
                    Female +
                    Ideology + 
                    Age_education + 
                    Age + 
                    Polity_score +
                    GDP_capita_logged  +
                    women_rep_lag + 
                    Corruption +
                    Presidentialism + 
                    (1 | Country_year), data = Euro_analysis)



## -------------------------------------------------------------------------
summary(Euro_hyp1)
saveRDS(Euro_hyp1, "Euro_hyp1.rds")




## -------------------------------------------------------------------------
Euro_generation_hyp1 <- lmer(Satisfaction ~ quota_effect + 
                    Female +
                    Ideology + 
                    Age_education + 
                    Age + 
                    Generation + 
                    Polity_score +
                    GDP_capita_logged  +
                    women_rep_lag + 
                    Corruption +
                    Presidentialism + 
                    (1 | Country_year), data = Euro_analysis)



## -------------------------------------------------------------------------
summary(Euro_generation_hyp1)
saveRDS(Euro_generation_hyp1, "Euro_hyp1_generation.rds")



## -------------------------------------------------------------------------
Euro_hyp2 <- lmer(Satisfaction ~ quota_effect*Female +
                    Ideology + 
                    Age_education + 
                    Age + 
                    Polity_score +
                    GDP_capita_logged  +
                    women_rep_lag + 
                    Corruption +
                    Presidentialism + 
                    (Female | Country_year), data = Euro_analysis)



## -------------------------------------------------------------------------
summary(Euro_hyp2)
saveRDS(Euro_hyp2, "Euro_hyp2.rds")




## -------------------------------------------------------------------------
Euro_hyp3a <- lmer(Satisfaction ~ quota_effect*Ideology +
                    Female +
                    Age_education + 
                    Age + 
                    Polity_score +
                    GDP_capita_logged  +
                    women_rep_lag + 
                    Corruption +
                    Presidentialism + 
                    (Ideology | Country_year), data = Euro_analysis)



## -------------------------------------------------------------------------

summary(Euro_hyp3a)
saveRDS(Euro_hyp3a, "Euro_hyp3a.rds")




## -------------------------------------------------------------------------
Euro_hyp4 <- lmer(Satisfaction ~ quota_effect*Corruption +
                    Female +
                    Ideology + 
                    Age_education + 
                    Age + 
                    Polity_score +
                    GDP_capita_logged  +
                    women_rep_lag + 
                    Presidentialism + 
                    (1 | Country_year), data = Euro_analysis)



## -------------------------------------------------------------------------
summary(Euro_hyp4)
saveRDS(Euro_hyp4, "Euro_hyp4.rds")




## -------------------------------------------------------------------------
Latino_analysis <- readRDS("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Analysis Data/Latino_analysis.rds")



## -------------------------------------------------------------------------
Latino_hyp1 <- lmer(Satisfaction ~ 
                      quota_effect + 
                      Female +
                      Ideology + 
                      Education + 
                      Age + 
                      Polity_score + 
                      GDP_capita_logged +
                      women_rep_lag +
                      Corruption + 
                      Presidentialism+
                      (1 | Country_year),
                      data = Latino_analysis)


## -------------------------------------------------------------------------
summary(Latino_hyp1)
saveRDS(Latino_hyp1, "Latino_hyp1.rds")


## -------------------------------------------------------------------------
Latino_hyp1_generation <- lmer(Satisfaction ~ 
                      quota_effect + 
                      Female +
                      Ideology + 
                      Education + 
                      Age + 
                      Generation + 
                      Polity_score + 
                      GDP_capita_logged +
                      women_rep_lag +
                      Corruption + 
                      Presidentialism+
                      (1 | Country_year),
                      data = Latino_analysis)


## -------------------------------------------------------------------------
summary(Latino_hyp1_generation)
saveRDS(Latino_hyp1_generation, "Latino_hyp1_generation.rds")


## -------------------------------------------------------------------------
Latino_hyp2 <- lmer(Satisfaction ~ quota_effect*Female +
                      Ideology + 
                      Education+
                      Age + 
                      Polity_score + 
                      GDP_capita_logged + 
                      women_rep_lag + 
                      Corruption + 
                      Presidentialism
                      + (Female | Country_year),
                      data = Latino_analysis)


## -------------------------------------------------------------------------
summary(Latino_hyp2)
saveRDS(Latino_hyp2, "Latino_hyp2.rds")



## -------------------------------------------------------------------------
Latino_hyp3a <- lmer(Satisfaction ~ quota_effect*Ideology + 
                      Female + 
                      Education + 
                      Age + 
                      Polity_score +
                      GDP_capita_logged + 
                      women_rep_lag + 
                      Corruption + 
                      Presidentialism +
                      (Ideology| Country_year), 
                    data = Latino_analysis)


## -------------------------------------------------------------------------
summary(Latino_hyp3a)
saveRDS(Latino_hyp3a, "Latino_hyp3a.rds")



## -------------------------------------------------------------------------
Latino_hyp4 <- lmer(Satisfaction ~ 
                      quota_effect*Corruption + 
                      Female +
                      Ideology + 
                      Education + 
                      Age + 
                      Polity_score + 
                      GDP_capita_logged +
                      women_rep_lag +
                      Presidentialism+
                      (1 | Country_year),
                      data = Latino_analysis)


## -------------------------------------------------------------------------
summary(Latino_hyp4)
saveRDS(Latino_hyp4, "Latino_hyp4.rds")



## -------------------------------------------------------------------------
#bring in the loaded models 
Americas_hyp1 <- readRDS("Americas_hyp1.rds")
CSES_hyp1 <- readRDS("CSES_hyp1.rds")
Euro_hyp1 <- readRDS("Euro_hyp1.rds")
Latino_hyp1 <- readRDS("Latino_hyp1.rds")




## -------------------------------------------------------------------------
library(dplyr)



Americas_hyp1_fig <- sjPlot::plot_model(Americas_hyp1,
                                 type = "pred", 
                                 terms = "quota_effect")


Americas_hyp1_fig_data <- Americas_hyp1_fig$data



Americas_hyp1_fig_data <- Americas_hyp1_fig_data %>%
  dplyr::mutate(Quota = case_when(
                           x == 1 ~ "No quota",
                           x == 2 ~ "Non-effective quota",
                           x == 3 ~ "Effective quota"),
            Upper = predicted + 1.41*std.error, #1.41 aligns with the 84% level 
            Lower = predicted - 1.41*std.error, # which corresponds to the Bonferonni correction
            Data = "AmericasBarometer")




CSES_hyp1_fig <- sjPlot::plot_model(CSES_hyp1,
                                 type = "pred", 
                                 terms = "quota_effect")



CSES_hyp1_fig_data <- CSES_hyp1_fig$data



CSES_hyp1_fig_data <- CSES_hyp1_fig_data %>%
  mutate( Quota = case_when(
                           x == 1 ~ "No quota",
                           x == 2 ~ "Non-effective quota",
                           x == 3 ~ "Effective quota"),
            Upper = predicted + 1.41*std.error,
            Lower = predicted - 1.41*std.error, 
            Data = "CSES")


Euro_hyp1_fig <- sjPlot::plot_model(Euro_hyp1,
                                 type = "pred", 
                                 terms = "quota_effect")



Euro_hyp1_fig_data <- Euro_hyp1_fig$data



Euro_hyp1_fig_data <- Euro_hyp1_fig_data %>%
  mutate( Quota = case_when(
                           x == 1 ~ "No quota",
                           x == 2 ~ "Non-effective quota",
                           x == 3 ~ "Effective quota"),
            Upper = predicted + 1.41*std.error,
            Lower = predicted - 1.41*std.error, 
            Data = "Eurobarometer")


Latino_hyp1_fig <- sjPlot::plot_model(Latino_hyp1,
                                 type = "pred", 
                                 terms = "quota_effect")



Latino_hyp1_fig_data <- Latino_hyp1_fig$data



Latino_hyp1_fig_data <- Latino_hyp1_fig_data %>%
  mutate( Quota = case_when(
                           x == 1 ~ "No quota",
                           x == 2 ~ "Non-effective quota",
                           x == 3 ~ "Effective quota"),
            Upper = predicted + 1.41*std.error,
            Lower = predicted - 1.41*std.error, 
            Data = "Latinobarometer")


hyp1_fig_data <- rbind(Americas_hyp1_fig_data,
      CSES_hyp1_fig_data,
      Euro_hyp1_fig_data,
      Latino_hyp1_fig_data)

hyp1_fig_data$Quota <- as.factor(hyp1_fig_data$Quota)

hyp1_fig_data$Quota <- factor(hyp1_fig_data$Quota , levels = c("No quota", "Non-effective quota", "Effective quota"))


hyp1_fig_data <- na.omit(hyp1_fig_data)


## -------------------------------------------------------------------------
library(ggplot2)
ggplot(hyp1_fig_data, aes(x=Quota, y=predicted, fill = Quota)) + 
  geom_col(stat = "identity",position = "dodge", width=.9) + 
      scale_x_discrete(limits=rev) +

  ylim(0,0.65) +
       geom_text(aes(label=sprintf("%0.3f", predicted)), position=position_dodge(width=0.9), hjust = 2.5, vjust=0, 
                 size=3.8,
                 colour = "white", fontface = "bold") + 
  
   geom_text(aes(label=sprintf("(%0.3f, %0.3f)", Lower, Upper)), position=position_dodge(width=0.9), hjust = 1.6, vjust = 2, 
                 size=2.8, 
               colour = "white", fontface = "bold") + 

  
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=0,
                 position=position_dodge(.9)) + 
    scale_fill_grey(start = 0.75, end = .35,
                   guide = guide_legend(reverse = FALSE))  +


  labs(y = "Predicted Satisfaction with Democracy", 
       x = "Quota type",
       title = "") +
    theme_bw(12) +
    facet_wrap(~Data) + 


  theme(legend.position="none", legend.title = element_blank()) + 
  coord_flip()



## -------------------------------------------------------------------------
 ggsave("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Figures/figure_1.pdf", width = 8, height = 7, units = "in")



## -------------------------------------------------------------------------
#bring in the loaded models 
Americas_hyp2 <- readRDS("Americas_hyp2.rds")
CSES_hyp2 <- readRDS("CSES_hyp2.rds")
Euro_hyp2 <- readRDS("Euro_hyp2.rds")
Latino_hyp2 <- readRDS("Latino_hyp2.rds")




## -------------------------------------------------------------------------
library(sjPlot)
library(dplyr)
Americas_hyp2_fig <- sjPlot::plot_model(Americas_hyp2,
                                 type = "pred", 
                                 terms = c("quota_effect", "Female"))
  
Americas_hyp2_fig_data <- Americas_hyp2_fig$data


Americas_hyp2_fig_data <- Americas_hyp2_fig_data %>%
  mutate( Quota = case_when(
                           x == 1 ~ "No quota",
                           x == 2 ~ "Non-effective quota",
                           x == 3 ~ "Effective quota"),
          group = as.factor(group),
            Upper = predicted + 1.41*std.error,
            Lower = predicted - 1.41*std.error, 
            Data = "AmericasBarometer")


CSES_hyp2_fig <- sjPlot::plot_model(CSES_hyp2,
                                 type = "pred", 
                                 terms = c("quota_effect", "Female"))

CSES_hyp2_fig_data <- CSES_hyp2_fig$data


CSES_hyp2_fig_data <- CSES_hyp2_fig_data %>%
  mutate( Quota = case_when(
                           x == 1 ~ "No quota",
                           x == 2 ~ "Non-effective quota",
                           x == 3 ~ "Effective quota"),
          group = as.factor(group),
            Upper = predicted + 1.41*std.error,
            Lower = predicted - 1.41*std.error, 
            Data = "CSES")



Euro_hyp2_fig <- sjPlot::plot_model(Euro_hyp2,
                                 type = "pred", 
                                 terms = c("quota_effect", "Female"))

Euro_hyp2_fig_data <- Euro_hyp2_fig$data


Euro_hyp2_fig_data <- Euro_hyp2_fig_data %>%
  mutate( Quota = case_when(
                           x == 1 ~ "No quota",
                           x == 2 ~ "Non-effective quota",
                           x == 3 ~ "Effective quota"),
          group = as.factor(group),
            Upper = predicted + 1.41*std.error,
            Lower = predicted - 1.41*std.error, 
            Data = "Eurobarometer")



Latino_hyp2_fig <- sjPlot::plot_model(Latino_hyp2,
                                 type = "pred", 
                                 terms = c("quota_effect", "Female"))

Latino_hyp2_fig_data <- Latino_hyp2_fig$data


Latino_hyp2_fig_data <- Latino_hyp2_fig_data %>%
  mutate( Quota = case_when(
                           x == 1 ~ "No quota",
                           x == 2 ~ "Non-effective quota",
                           x == 3 ~ "Effective quota"),
          group = as.factor(group),
            Upper = predicted + 1.41*std.error,
            Lower = predicted - 1.41*std.error, 
            Data = "Latinobarometer")


hyp2_fig_data <- rbind(Americas_hyp2_fig_data,
      CSES_hyp2_fig_data,
      Euro_hyp2_fig_data,
      Latino_hyp2_fig_data)


hyp2_fig_data$Quota <- as.factor(hyp2_fig_data$Quota)

hyp2_fig_data$Quota <- factor(hyp2_fig_data$Quota , levels = c("No quota", "Non-effective quota", "Effective quota"))

hyp2_fig_data <- na.omit(hyp2_fig_data)


## -------------------------------------------------------------------------
library(ggplot2)
ggplot(hyp2_fig_data, aes(x=Quota, y=predicted, fill = group)) + 
  geom_col(stat = "identity",position = "dodge", width=.9) + 
      scale_x_discrete(limits=rev) +

  ylim(0,0.65) +
       geom_text(aes(label=sprintf("%0.3f", predicted)), position=position_dodge(width=0.9), hjust = 2.5, vjust=0, 
                 size=3.8,
                 colour = "white", fontface = "bold") + 
  
   geom_text(aes(label=sprintf("(%0.3f, %0.3f)", Lower, Upper)), position=position_dodge(width=0.9), hjust = 1.6, vjust = 2, 
                 size=2.8, 
               colour = "white", fontface = "bold") + 

  
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=0,
                 position=position_dodge(.9)) + 
    scale_fill_grey(start = 0.75, end = .35,
                   guide = guide_legend(reverse = FALSE))  +

  labs(y = "Predicted Satisfaction with Democracy", 
       x = "Quota type",
       title = "") +
    theme_bw(12) +
    facet_wrap(~Data) + 


  theme(legend.position="bottom", legend.title = element_blank()) + 
  coord_flip()



## -------------------------------------------------------------------------
 ggsave("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Figures/figure_2.pdf", width = 8, height = 9, units = "in")



## -------------------------------------------------------------------------
#bring in the loaded models 
Americas_hyp3a <- readRDS("Americas_hyp3a.rds")
CSES_hyp3a <- readRDS("CSES_hyp3a.rds")
Euro_hyp3a <- readRDS("Euro_hyp3a.rds")
Latino_hyp3a <- readRDS("Latino_hyp3a.rds")




## -------------------------------------------------------------------------
Americas_hyp3a_fig <- sjPlot::plot_model(Americas_hyp3a,
                                         type = "int", mdrt.values = "meansd")

Americas_hyp3a_fig_data <- Americas_hyp3a_fig$data


Americas_hyp3a_fig_data <- Americas_hyp3a_fig_data %>%
  mutate( Quota = case_when(
                           x == 1 ~ "No quota",
                           x == 2 ~ "Non-effective quota",
                           x == 3 ~ "Effective quota"),
          Ideology = case_when(
                           group == 0.23 ~ "Mean - 1sd (Left)",
                           group == 0.52 ~ "Mean (Center)",
                           group == 0.8 ~ "Mean + 1sd (Right)"),
          Ideology = as.factor(Ideology),
            Upper = predicted + 1.41*std.error,
            Lower = predicted - 1.41*std.error, 
            Data = "AmericasBarometer")



CSES_hyp3a_fig <- sjPlot::plot_model(CSES_hyp3a,
                                         type = "int", mdrt.values = "meansd")

CSES_hyp3a_fig_data <- CSES_hyp3a_fig$data


CSES_hyp3a_fig_data <- CSES_hyp3a_fig_data %>%
  mutate( Quota = case_when(
                           x == 1 ~ "No quota",
                           x == 2 ~ "Non-effective quota",
                           x == 3 ~ "Effective quota"),
          Ideology = case_when(
                           group == 0.23 ~ "Mean - 1sd (Left)",
                           group == 0.52 ~ "Mean (Center)",
                           group == 0.8 ~ "Mean + 1sd (Right)"),
          Ideology = as.factor(Ideology),
            Upper = predicted + 1.41*std.error,
            Lower = predicted - 1.41*std.error, 
            Data = "CSES")


CSES_hyp3a_fig <- sjPlot::plot_model(CSES_hyp3a,
                                         type = "int", mdrt.values = "meansd")

CSES_hyp3a_fig_data <- CSES_hyp3a_fig$data


CSES_hyp3a_fig_data <- CSES_hyp3a_fig_data %>%
  mutate( Quota = case_when(
                           x == 1 ~ "No quota",
                           x == 2 ~ "Non-effective quota",
                           x == 3 ~ "Effective quota"),
          Ideology = case_when(
                           group == 0.29 ~ "Mean - 1sd (Left)",
                           group == 0.54 ~ "Mean (Center)",
                           group == 0.79 ~ "Mean + 1sd (Right)"),
          Ideology = as.factor(Ideology),
            Upper = predicted + 1.41*std.error,
            Lower = predicted - 1.41*std.error, 
            Data = "CSES")



Euro_hyp3a_fig <- sjPlot::plot_model(Euro_hyp3a,
                                         type = "int", mdrt.values = "meansd")

Euro_hyp3a_fig_data <- Euro_hyp3a_fig$data


Euro_hyp3a_fig_data <- Euro_hyp3a_fig_data %>%
  mutate( Quota = case_when(
                           x == 1 ~ "No quota",
                           x == 2 ~ "Non-effective quota",
                           x == 3 ~ "Effective quota"),
          Ideology = case_when(
                           group == 0.25 ~ "Mean - 1sd (Left)",
                           group == 0.48 ~ "Mean (Center)",
                           group == 0.72 ~ "Mean + 1sd (Right)"),
          Ideology = as.factor(Ideology),
            Upper = predicted + 1.41*std.error,
            Lower = predicted - 1.41*std.error, 
            Data = "Eurobarometer")



Latino_hyp3a_fig <- sjPlot::plot_model(Latino_hyp3a,
                                         type = "int", mdrt.values = "meansd")

Latino_hyp3a_fig_data <- Latino_hyp3a_fig$data


Latino_hyp3a_fig_data <- Latino_hyp3a_fig_data %>%
  mutate( Quota = case_when(
                           x == 1 ~ "No quota",
                           x == 2 ~ "Non-effective quota",
                           x == 3 ~ "Effective quota"),
          Ideology = case_when(
                           group == 0.26 ~ "Mean - 1sd (Left)",
                           group == 0.53 ~ "Mean (Center)",
                           group == 0.8 ~ "Mean + 1sd (Right)"),
          Ideology = as.factor(Ideology),
            Upper = predicted + 1.41*std.error,
            Lower = predicted - 1.41*std.error, 
            Data = "Latinobarometer")


hyp3a_fig_data <- rbind(Americas_hyp3a_fig_data,
      CSES_hyp3a_fig_data,
      Euro_hyp3a_fig_data,
      Latino_hyp3a_fig_data)


hyp3a_fig_data$Quota <- as.factor(hyp3a_fig_data$Quota)

hyp3a_fig_data$Quota <- factor(hyp3a_fig_data$Quota , levels = c("No quota", "Non-effective quota", "Effective quota"))



hyp3a_fig_data$Ideology <- as.factor(hyp3a_fig_data$Ideology)

hyp3a_fig_data$Ideology <- factor(hyp3a_fig_data$Ideology , levels = c("Mean - 1sd (Left)", "Mean (Center)", "Mean + 1sd (Right)"))

hyp3a_fig_data <- na.omit(hyp3a_fig_data)





## -------------------------------------------------------------------------
library(ggplot2)
ggplot(hyp3a_fig_data, aes(x=Quota, y=predicted, fill = Ideology)) + 
  geom_col(stat = "identity",position = "dodge", width=.9) + 
      scale_x_discrete(limits=rev) +

  ylim(0,0.65) +
       geom_text(aes(label=sprintf("%0.3f", predicted)), position=position_dodge(width=0.9), hjust = 2.5, vjust=0, 
                 size=3.1,
                 colour = "white", fontface = "bold") + 
  
   geom_text(aes(label=sprintf("(%0.3f, %0.3f)", Lower, Upper)), position=position_dodge(width=0.9), hjust = 1.6, vjust = 2, 
                 size=2.1, 
               colour = "white", fontface = "bold") + 

  
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=0,
                 position=position_dodge(.9)) + 
    scale_fill_grey(start = 0.75, end = .35,
                   guide = guide_legend(reverse = FALSE))  +

  labs(y = "Predicted Satisfaction with Democracy", 
       x = "Quota type",
       title = "") +
    theme_bw(12) +
    facet_wrap(~Data) + 


  theme(legend.position="bottom", legend.title = element_blank()) + 
  coord_flip()



## -------------------------------------------------------------------------
 ggsave("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Figures/figure_3.pdf", width = 8, height = 11, units = "in")



## -------------------------------------------------------------------------
Americas_hyp3b <- readRDS("Americas_hyp3b.rds")



## -------------------------------------------------------------------------
Americas_hyp3b_fig <- sjPlot::plot_model(Americas_hyp3b,
                                         type = "int", mdrt.values = "meansd")

Americas_hyp3b_fig_data <- Americas_hyp3b_fig$data


Americas_hyp3b_fig_data <- Americas_hyp3b_fig_data %>%
  mutate( Quota = case_when(
                           x == 1 ~ "No quota",
                           x == 2 ~ "Non-effective quota",
                           x == 3 ~ "Effective quota"),
          Quota_support = case_when(
                           group == 0.37 ~ "Mean - 1sd (Do not support quotas)",
                           group == 0.68 ~ "Mean",
                           group == 0.99 ~ "Mean + 1sd (Support quotas)"),
          Quota_support = as.factor(Quota_support),
            Upper = predicted + 1.41*std.error,
            Lower = predicted - 1.41*std.error, 
            Data = "AmericasBarometer")


Americas_hyp3b_fig_data$Quota <- as.factor(Americas_hyp3b_fig_data$Quota)

Americas_hyp3b_fig_data$Quota <- factor(Americas_hyp3b_fig_data$Quota , levels = c("No quota", "Non-effective quota", "Effective quota"))


Americas_hyp3b_fig_data$Quota_support <- as.factor(Americas_hyp3b_fig_data$Quota_support)

Americas_hyp3b_fig_data$Quota_support <- factor(Americas_hyp3b_fig_data$Quota_support , levels = c("Mean - 1sd (Do not support quotas)", "Mean", "Mean + 1sd (Support quotas)"))

Americas_hyp3b_fig_data <- na.omit(Americas_hyp3b_fig_data)



## -------------------------------------------------------------------------
library(ggplot2)
ggplot(Americas_hyp3b_fig_data, aes(x=Quota, y=predicted, fill = Quota_support)) + 
  geom_col(stat = "identity",position = "dodge", width=.9) + 
      scale_x_discrete(limits=rev) +

  ylim(0,0.65) +
       geom_text(aes(label=sprintf("%0.3f", predicted)), position=position_dodge(width=0.9), hjust = 3.0, vjust=0, 
                 size=3.1,
                 colour = "white", fontface = "bold") + 
  
   geom_text(aes(label=sprintf("(%0.3f, %0.3f)", Lower, Upper)), position=position_dodge(width=0.9), hjust = 2.0, vjust = 2, 
                 size=2.1, 
               colour = "white", fontface = "bold") + 

  
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=0,
                 position=position_dodge(.9)) + 
    scale_fill_grey(start = 0.75, end = .35,
                   guide = guide_legend(reverse = FALSE))  +

  labs(y = "Predicted Satisfaction with Democracy", 
       x = "Quota type",
       title = "") +
    theme_bw(12) +


  theme(legend.position="bottom", legend.title = element_blank()) + 
  coord_flip()



## -------------------------------------------------------------------------
 ggsave("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Figures/figure_4.pdf", width = 7.5, height = 6, units = "in")



## -------------------------------------------------------------------------
#bring in the loaded models 
Americas_hyp4 <- readRDS("Americas_hyp4.rds")
CSES_hyp4 <- readRDS("CSES_hyp4.rds")
Euro_hyp4 <- readRDS("Euro_hyp4.rds")
Latino_hyp4 <- readRDS("Latino_hyp4.rds")




## -------------------------------------------------------------------------

Americas_hyp4_fig <- sjPlot::plot_model(Americas_hyp4,
                                         type = "int", mdrt.values = "meansd")

Americas_hyp4_fig_data <- Americas_hyp4_fig$data


Americas_hyp4_fig_data <- Americas_hyp4_fig_data %>%
  mutate( Quota = case_when(
                           x == 1 ~ "No quota",
                           x == 2 ~ "Non-effective quota",
                           x == 3 ~ "Effective quota"),
          Corruption = case_when(
                           group == 0.36 ~ "Mean - 1sd (Less corrupt)",
                           group == 0.63 ~ "Mean",
                           group == 0.9 ~ "Mean + 1sd (More corrupt)"),
          Corruption = as.factor(Corruption),
            Upper = predicted + 1.41*std.error,
            Lower = predicted - 1.41*std.error, 
            Data = "AmericasBarometer")



CSES_hyp4_fig <- sjPlot::plot_model(CSES_hyp4,
                                         type = "int", mdrt.values = "meansd")

CSES_hyp4_fig_data <- CSES_hyp4_fig$data


CSES_hyp4_fig_data <- CSES_hyp4_fig_data %>%
  mutate( Quota = case_when(
                           x == 1 ~ "No quota",
                           x == 2 ~ "Non-effective quota",
                           x == 3 ~ "Effective quota"),
          Corruption = case_when(
                           group == -0.08 ~ "Mean - 1sd (Less corrupt)",
                           group ==  0.2 ~ "Mean",
                           group == 0.47 ~ "Mean + 1sd (More corrupt)"),
          Corruption = as.factor(Corruption),
            Upper = predicted + 1.41*std.error,
            Lower = predicted - 1.41*std.error, 
            Data = "CSES")






Euro_hyp4_fig <- sjPlot::plot_model(Euro_hyp4,
                                         type = "int", mdrt.values = "meansd")

Euro_hyp4_fig_data <- Euro_hyp4_fig$data


Euro_hyp4_fig_data <- Euro_hyp4_fig_data %>%
  mutate( Quota = case_when(
                           x == 1 ~ "No quota",
                           x == 2 ~ "Non-effective quota",
                           x == 3 ~ "Effective quota"),
          Corruption = case_when(
                           group == -0.02 ~ "Mean - 1sd (Less corrupt)",
                           group == 0.13 ~ "Mean",
                           group == 0.27 ~ "Mean + 1sd (More corrupt)"),
          Corruption = as.factor(Corruption),
            Upper = predicted + 1.41*std.error,
            Lower = predicted - 1.41*std.error, 
            Data = "Eurobarometer")



Latino_hyp4_fig <- sjPlot::plot_model(Latino_hyp4,
                                         type = "int", mdrt.values = "meansd")

Latino_hyp4_fig_data <- Latino_hyp4_fig$data


Latino_hyp4_fig_data <- Latino_hyp4_fig_data %>%
  mutate( Quota = case_when(
                           x == 1 ~ "No quota",
                           x == 2 ~ "Non-effective quota",
                           x == 3 ~ "Effective quota"),
          Corruption = case_when(
                           group == 0.25 ~ "Mean - 1sd (Less corrupt)",
                           group == 0.54 ~ "Mean",
                           group == 0.84 ~ "Mean + 1sd (More corrupt)"),
          Corruption = as.factor(Corruption),
            Upper = predicted + 1.41*std.error,
            Lower = predicted - 1.41*std.error, 
            Data = "Latinobarometer")


hyp_4_fig_data <- rbind(Americas_hyp4_fig_data,
      CSES_hyp4_fig_data,
      Euro_hyp4_fig_data,
      Latino_hyp4_fig_data)


hyp_4_fig_data$Quota <- as.factor(hyp_4_fig_data$Quota)

hyp_4_fig_data$Quota <- factor(hyp_4_fig_data$Quota , levels = c("No quota", "Non-effective quota", "Effective quota"))



hyp_4_fig_data$Corruption <- as.factor(hyp_4_fig_data$Corruption)

hyp_4_fig_data$Corruption <- factor(hyp_4_fig_data$Corruption , levels = c("Mean - 1sd (Less corrupt)", "Mean", "Mean + 1sd (More corrupt)"))


hyp_4_fig_data <- na.omit(hyp_4_fig_data)


## -------------------------------------------------------------------------
ggplot(hyp_4_fig_data, aes(x=Quota, y=predicted, fill = Corruption)) + 
  geom_col(stat = "identity",position = "dodge", width=.9) + 
      scale_x_discrete(limits=rev) +

  ylim(0,0.65) +
       geom_text(aes(label=sprintf("%0.3f", predicted)), position=position_dodge(width=0.9), hjust = 2.5, vjust=0, 
                 size=3.1,
                 colour = "white", fontface = "bold") + 
  
   geom_text(aes(label=sprintf("(%0.3f, %0.3f)", Lower, Upper)), position=position_dodge(width=0.9), hjust = 1.6, vjust = 2, 
                 size=2.1, 
               colour = "white", fontface = "bold") + 

  
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=0,
                 position=position_dodge(.9)) + 
    scale_fill_grey(start = 0.75, end = .35,
                   guide = guide_legend(reverse = FALSE))  +

  labs(y = "Predicted Satisfaction with Democracy", 
       x = "Quota type",
       title = "") +
    theme_bw(12) +
    facet_wrap(~Data) + 


  theme(legend.position="bottom", legend.title = element_blank()) + 
  coord_flip()



## -------------------------------------------------------------------------
 ggsave("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Figures/figure_5.pdf", width = 8, height = 11, units = "in")



## -------------------------------------------------------------------------
#bring in the loaded models 
Americas_hyp1 <- readRDS("Americas_hyp1.rds")
CSES_hyp1 <- readRDS("CSES_hyp1.rds")
Euro_hyp1 <- readRDS("Euro_hyp1.rds")
Latino_hyp1 <- readRDS("Latino_hyp1.rds")




## -------------------------------------------------------------------------

Americas.base.emm <- emmeans(Americas_hyp1,
                                 pairwise ~ quota_effect,
                                 adjust = "fdr")
summary(Americas.base.emm)





CSES.base.emm <- emmeans(CSES_hyp1,
                                 pairwise ~ quota_effect,
                                 adjust = "fdr")
summary(CSES.base.emm)



Latino.base.emm <- emmeans(Latino_hyp1,
                                 pairwise ~ quota_effect,
                                 adjust = "fdr")
summary(Latino.base.emm)




Euro.base.emm <- emmeans(Euro_hyp1,
                                 pairwise ~ quota_effect,
                                 adjust = "fdr")
summary(Euro.base.emm)




## -------------------------------------------------------------------------
#bring in the loaded models 
Americas_hyp2 <- readRDS("Americas_hyp2.rds")
CSES_hyp2 <- readRDS("CSES_hyp2.rds")
Euro_hyp2 <- readRDS("Euro_hyp2.rds")
Latino_hyp2 <- readRDS("Latino_hyp2.rds")




## -------------------------------------------------------------------------

Americas.female.emm <- emmeans(Americas_hyp2, "quota_effect")


Americas.female.emm <- emmeans(Americas_hyp2, pairwise ~ quota_effect | Female, adjust = "fdr")
summary(Americas.female.emm)


Americas_female_diffs <- pairs(pairs(Americas.female.emm), by = NULL, adjust = "fdr" )
View(summary(Americas_female_diffs))



CSES.female.emm <- emmeans(CSES_hyp2, "quota_effect")

CSES_female_diffs <- pairs(pairs(CSES.female.emm), by = NULL, adjust = "fdr" )
View(summary(CSES_female_diffs))




Euro.female.emm <- emmeans(Euro_hyp2, "quota_effect")

Euro_female_diffs <- pairs(pairs(Euro.female.emm), by = NULL, adjust = "fdr" )
View(summary(Euro_female_diffs))



Latino.female.emm <- emmeans(Latino_hyp2, "quota_effect")

Latino_female_diffs <- pairs(pairs(Latino.female.emm), by = NULL, adjust = "fdr" )
View(summary(Latino_female_diffs))





## -------------------------------------------------------------------------
#bring in the loaded models 
Americas_hyp3a <- readRDS("Americas_hyp3a.rds")
CSES_hyp3a <- readRDS("CSES_hyp3a.rds")
Euro_hyp3a <- readRDS("Euro_hyp3a.rds")
Latino_hyp3a <- readRDS("Latino_hyp3a.rds")




## -------------------------------------------------------------------------
Americas.ideology.emm <- emmeans(Americas_hyp3a, pairwise ~ Ideology | quota_effect,  at = list(Ideology = c(0.23, 0.52,0.8)), adjust = "fdr")


Americas_ideology_diffs<- pairs(pairs(Americas.ideology.emm), by = NULL, adjust = "fdr" )
Americas_ideology_diffs_tab <- summary(Americas_ideology_diffs)
print(Americas_ideology_diffs_tab)



CSES.ideology.emm <- emmeans(CSES_hyp3a, pairwise ~ Ideology | quota_effect,  at = list(Ideology = c(0.23, 0.52,0.8)), adjust = "fdr")


CSES_ideology_diffs <- pairs(pairs(CSES.ideology.emm), by = NULL, adjust = "fdr" )
CSES_ideology_diffs_tab <- summary(CSES_ideology_diffs)



Euro.ideology.emm <- emmeans(Euro_hyp3a, pairwise ~ Ideology | quota_effect,  at = list(Ideology = c(0.25, 0.48,0.72)), adjust = "fdr")


Euro_ideology_diffs <- pairs(pairs(Euro.ideology.emm), by = NULL, adjust = "fdr" )
Euro_ideology_diffs_tab <- summary(Euro_ideology_diffs)
print(Euro_ideology_diffs_tab)


Latino.ideology.emm <- emmeans(Latino_hyp3a, pairwise ~ Ideology | quota_effect,  at = list(Ideology = c(0.26, 0.53,0.8)), adjust = "fdr")


Latino_ideology_diffs <- pairs(pairs(Latino.ideology.emm), by = NULL, adjust = "fdr" )
Latino_ideology_diffs_tab <- summary(Latino_ideology_diffs)
print(Latino_ideology_diffs_tab)



## -------------------------------------------------------------------------
Americas_hyp3b <- readRDS("Americas_hyp3b.rds")



## -------------------------------------------------------------------------
Americas.support.emm <- emmeans(Americas_hyp3b, pairwise ~ Quota_support | quota_effect,  at = list(Quota_support = c(0.37, 0.68,0.99)), adjust = "fdr")


Support_americas_diffs <- pairs(pairs(Americas.support.emm), by = NULL, adjust = "fdr" )
Support_americas_diffs_tab <- summary(Support_americas_diffs)




## -------------------------------------------------------------------------
#bring in the loaded models 
Americas_hyp4 <- readRDS("Americas_hyp4.rds")
CSES_hyp4 <- readRDS("CSES_hyp4.rds")
Euro_hyp4 <- readRDS("Euro_hyp4.rds")
Latino_hyp4 <- readRDS("Latino_hyp4.rds")




## -------------------------------------------------------------------------

Americas.corruption.emm <- emmeans(Americas_hyp3a, pairwise ~ Corruption | quota_effect,  at = list(Corruption = c(0.36, 0.63,0.9)), adjust = "fdr")
summary(Americas.corruption.emm)


Corruption_americas_diffs <- pairs(pairs(Americas.corruption.emm), by = NULL, adjust = "fdr" )
Corruption_americas_diffs_tab <- summary(Corruption_americas_diffs)



CSES.corruption.emm <- emmeans(CSES_hyp3a, pairwise ~ Corruption | quota_effect,  at = list(Corruption = c(-0.08, 0.2,0.42)), adjust = "fdr")
summary(CSES.corruption.emm)

Corruption_cses_diffs <- pairs(pairs(CSES.corruption.emm), by = NULL, adjust = "fdr" )
Corruption_cses_diffs_tab <- summary(Corruption_cses_diffs)



Euro.corruption.emm <- emmeans(Euro_hyp3a, pairwise ~ Corruption | quota_effect,  at = list(Corruption = c(-0.02, 0.13,0.27)), adjust = "fdr")
summary(Euro.corruption.emm)

Corruption_euro_diffs <- pairs(pairs(Euro.corruption.emm), by = NULL, adjust = "fdr" )
Corruption_euro_diffs_tab <- summary(Corruption_euro_diffs)



Latino.corruption.emm <- emmeans(Latino_hyp3a, pairwise ~ Corruption | quota_effect,  at = list(Corruption = c(0.25, 0.54,0.84)), adjust = "fdr")
summary(Latino.corruption.emm)


Corruption_latino_diffs <- pairs(pairs(Latino.corruption.emm), by = NULL, adjust = "fdr" )
Corruption_latino_diffs_tab <- summary(Corruption_latino_diffs)





## -------------------------------------------------------------------------
Americas_analysis <- readRDS("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Analysis Data/Americas_analysis.rds")



## -------------------------------------------------------------------------

Americas_agg <- Americas_analysis %>%
 group_by(Country_year, quota_effect)%>%
  summarize_at(vars(Satisfaction = Satisfaction), 
                                   mean, na.rm = TRUE)


Country_year_label <- attributes(Americas_agg$Country_year)$labels
Americas_agg$Country_year <- names(Country_year_label)[match(Americas_agg$Country_year, Country_year_label)]




Americas_agg <- Americas_agg[order(Americas_agg$Country_year),]

Americas_agg <- as.data.frame(Americas_agg[order(rev(Americas_agg$Country_year), decreasing = TRUE), ])


## -------------------------------------------------------------------------


Americas_dotplot <- ggplot(Americas_agg, aes(x = Satisfaction, y = reorder(Country_year, desc(Country_year)))) +
  geom_point(aes(shape=quota_effect, colour = quota_effect), size = 1.3, position=position_dodge(width=0)) + 
  scale_x_continuous(limits = c(0, 1)) +
  theme_bw(10) +
  theme(axis.text.y = element_text(size = rel(.7)),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(.9)),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.4),
        panel.grid.minor.x = element_blank(), 
        legend.position = "bottom", 
        legend.title = element_blank()) +
  scale_color_manual(values=c("springgreen3","steelblue1","indianred2")) +
  xlab("Satisfaction") +
  ylab("")

Americas_dotplot




## -------------------------------------------------------------------------
 ggsave("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Figures/figure_A1.pdf",width = 6.5, height = 12.5, units = "in")



## -------------------------------------------------------------------------
CSES_analysis <- readRDS("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Analysis Data/CSES_analysis.rds")



## -------------------------------------------------------------------------

CSES_analysis$Country_Year <- as.character(paste(CSES_analysis$Country, CSES_analysis$Year))



CSES_agg <- CSES_analysis %>%
 group_by(Country_Year, quota_effect, Country_year)%>%
  summarize_at(vars(Satisfaction = Satisfaction), 
                                   mean, na.rm = FALSE)




CSES_agg <- CSES_agg[order(CSES_agg$Country_Year),]

CSES_agg <- as.data.frame(CSES_agg[order(rev(CSES_agg$Country_Year), decreasing = TRUE), ])



CSES_agg1 <- CSES_agg[1:72,]
CSES_agg2 <- CSES_agg[73:144,]


## -------------------------------------------------------------------------
theme_dotplot <- theme_bw(10) +
  theme(axis.text.y = element_text(size = rel(.7)),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(.9)),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.4),
        panel.grid.minor.x = element_blank(), 
        legend.position = "none")




CSES_agg1 <- as.data.frame(CSES_agg1[order(rev(CSES_agg1$Country_year), decreasing = TRUE), ])

CSES_agg2 <- as.data.frame(CSES_agg2[order(rev(CSES_agg2$Country_year), decreasing = TRUE), ])



## -------------------------------------------------------------------------
plot1 <- ggplot(CSES_agg1, aes(x = Satisfaction, y = reorder(Country_Year, desc(Country_Year)))) +
  geom_point(aes(shape=quota_effect, colour = quota_effect), size = 1.3, position=position_dodge(width=0)) + 
  scale_x_continuous(limits = c(0, 1)) +
  theme_bw(10) +
  theme(axis.text.y = element_text(size = rel(.7)),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(.9)),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.4),
        panel.grid.minor.x = element_blank(), 
        legend.position = "bottom", 
        legend.title = element_blank()) +
  scale_color_manual(values=c("springgreen3","steelblue1","indianred2")) +
  xlab("Satisfaction") +
  ylab("")


## -------------------------------------------------------------------------
plot2 <- ggplot(CSES_agg2, aes(x = Satisfaction, y = reorder(Country_Year, desc(Country_Year)))) +
  geom_point(aes(shape=quota_effect, colour = quota_effect), size = 1.3, position=position_dodge(width=0)) + 
  scale_x_continuous(limits = c(0, 1)) +
  theme_bw(10) +
  theme(axis.text.y = element_text(size = rel(.65)),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(.9)),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.4),
        panel.grid.minor.x = element_blank(), 
        legend.position = "none") +
  scale_color_manual(values=c("springgreen3","steelblue1","indianred2")) +
xlab("Satisfaction") +
  ylab("")



ggarrange(plot1, plot2, ncol=2, nrow=1, common.legend = TRUE, legend="bottom") #5.5x7 inches


## -------------------------------------------------------------------------

CSES_dotchart <- ggarrange(plot1, plot2, ncol=2, nrow=1, common.legend = TRUE, legend="bottom") #5.5x7 inches

ggsave("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Figures/figure_A2.pdf", CSES_dotchart, width = 6.5, height = 11.5, units = "in")

  



## -------------------------------------------------------------------------
Euro_analysis <- readRDS("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Analysis Data/Euro_analysis.rds")



## -------------------------------------------------------------------------




Euro_analysis <- Euro_analysis %>%
  dplyr::mutate(quota_effect = case_when(
                           quota_effect == "No Quota" ~ "No quota",
                           quota_effect == "Non-Effective" ~ "Non-effective quota",
                           quota_effect == "Effective" ~ "Effective quota"))
                
                
Euro_agg <- Euro_analysis %>%
 group_by(Country_year, quota_effect)%>%
  summarize_at(vars(Satisfaction = Satisfaction), 
                                   mean, na.rm = FALSE)



Euro_agg$quota_effect <- factor(Euro_agg$quota_effect , levels = c("No quota", "Non-effective quota", "Effective quota"))

Euro_agg <- Euro_agg[order(Euro_agg$Country_year),]


Euro_agg1 <- Euro_agg[1:86,]
Euro_agg2 <- Euro_agg[87:174,]
Euro_agg3 <- Euro_agg[175:262,]
Euro_agg4 <- Euro_agg[263:350,]
Euro_agg5 <- Euro_agg[351:438,]
Euro_agg6 <- Euro_agg[439:521,]


## -------------------------------------------------------------------------
Euro_agg1 <- as.data.frame(Euro_agg1[order(rev(Euro_agg1$Country_year), decreasing = TRUE), ])

Euro_plot_1 <- ggplot(Euro_agg1, aes(x = Satisfaction, y = reorder(Country_year, desc(Country_year)))) +
  geom_point(aes(shape=quota_effect, colour = quota_effect), size = 1.3, position=position_dodge(width=0)) + 
  scale_x_continuous(limits = c(0, 1)) +
  theme_bw(12) +
  theme(axis.text.y = element_text(size = rel(.7)),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(1.1)),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.4),
        panel.grid.minor.x = element_blank(), 
        legend.position = "none", 
        legend.title = element_blank()) +
  scale_color_manual(values=c("No quota" = "springgreen3","Non-effective quota" = "steelblue1" ,"Effective quota"="indianred2"), drop = FALSE) + 
  scale_shape_manual(values=c("No quota" = 16,"Non-effective quota" = 17, "Effective quota" = 15), drop = FALSE)+
  xlab("Satisfaction") +
  ylab("")


## -------------------------------------------------------------------------

Euro_agg2 <- as.data.frame(Euro_agg2[order(rev(Euro_agg2$Country_year), decreasing = TRUE), ])

Euro_plot_2 <- ggplot(Euro_agg2, aes(x = Satisfaction, y = reorder(Country_year, desc(Country_year)))) +
  geom_point(aes(shape=quota_effect, colour = quota_effect), size = 1.3, position=position_dodge(width=0)) + 
  scale_x_continuous(limits = c(0, 1)) +
  theme_bw(12) +
  theme(axis.text.y = element_text(size = rel(.65)),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(1.1)),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.4),
        panel.grid.minor.x = element_blank(), 
        legend.position = "none") +
  scale_color_manual(values=c("No quota" = "springgreen3","Non-effective quota" = "steelblue1" ,"Effective quota"="indianred2"), drop = FALSE) + 
  scale_shape_manual(values=c("No quota" = 16,"Non-effective quota" = 17, "Effective quota" = 15), drop = FALSE)+
  xlab("Satisfaction") +
  ylab("")


## -------------------------------------------------------------------------
Euro_agg3 <- as.data.frame(Euro_agg3[order(rev(Euro_agg3$Country_year), decreasing = TRUE), ])


Euro_plot_3 <- ggplot(Euro_agg3, aes(x = Satisfaction, y = reorder(Country_year, desc(Country_year)))) +
  geom_point(aes(shape=quota_effect, colour = quota_effect), size = 1.3, position=position_dodge(width=0)) + 
  scale_x_continuous(limits = c(0, 1)) +
  theme_bw(12) +
  theme(axis.text.y = element_text(size = rel(.65)),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(1.1)),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.4),
        panel.grid.minor.x = element_blank(), 
        legend.position = "bottom", 
        legend.text=element_text(size=16),
        legend.title = element_blank()) +
  scale_color_manual(values=c("No quota" = "springgreen3","Non-effective quota" = "steelblue1" ,"Effective quota"="indianred2"), drop = FALSE) + 
  scale_shape_manual(values=c("No quota" = 16,"Non-effective quota" = 17, "Effective quota" = 15), drop = FALSE)+
  xlab("Satisfaction") +
  ylab("")


## -------------------------------------------------------------------------
Euro_agg4 <- as.data.frame(Euro_agg4[order(rev(Euro_agg4$Country_year), decreasing = TRUE), ])

Euro_plot_4 <- ggplot(Euro_agg4, aes(x = Satisfaction, y = reorder(Country_year, desc(Country_year)))) +
  geom_point(aes(shape=quota_effect, colour = quota_effect), size = 1.3, position=position_dodge(width=0)) + 
  scale_x_continuous(limits = c(0, 1)) +
  theme_bw(12) +
  theme(axis.text.y = element_text(size = rel(.65)),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(1.1)),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.4),
        panel.grid.minor.x = element_blank(), 
        legend.position = "none", 
        legend.title = element_blank()) +
  scale_color_manual(values=c("No quota" = "springgreen3","Non-effective quota" = "steelblue1" ,"Effective quota"="indianred2"), drop = FALSE) +  
  scale_shape_manual(values=c("No quota" = 16,"Non-effective quota" = 17, "Effective quota" = 15), drop = FALSE)+
  xlab("Satisfaction") +
  ylab("")


## -------------------------------------------------------------------------
Euro_agg5 <- as.data.frame(Euro_agg5[order(rev(Euro_agg5$Country_year), decreasing = TRUE), ])

Euro_plot_5 <- ggplot(Euro_agg5, aes(x = Satisfaction, y = reorder(Country_year, desc(Country_year)))) +
  geom_point(aes(shape=quota_effect, colour = quota_effect), size = 1.3, position=position_dodge(width=0)) + 
  scale_x_continuous(limits = c(0, 1)) +
  theme_bw(12) +
  theme(axis.text.y = element_text(size = rel(.65)),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(.9)),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.4),
        panel.grid.minor.x = element_blank(), 
        legend.position = "none") +
  scale_color_manual(values=c("No quota" = "springgreen3","Non-effective quota" = "steelblue1" ,"Effective quota"="indianred2"), drop = FALSE) + 
  scale_shape_manual(values=c("No quota" = 16,"Non-effective quota" = 17, "Effective quota" = 15, drop = FALSE))+
  xlab("Satisfaction") +
  ylab("")


## -------------------------------------------------------------------------
Euro_agg6 <- as.data.frame(Euro_agg6[order(rev(Euro_agg6$Country_year), decreasing = TRUE), ])

Euro_plot_6 <- ggplot(Euro_agg6, aes(x = Satisfaction, y = reorder(Country_year, desc(Country_year)))) +
  geom_point(aes(shape=quota_effect, colour = quota_effect), size = 1.3, position=position_dodge(width=0)) + 
  scale_x_continuous(limits = c(0, 1)) +
  theme_bw(12) +
  theme(axis.text.y = element_text(size = rel(.65)),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(.9)),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.4),
        panel.grid.minor.x = element_blank(), 
        legend.position = "none") +
  scale_color_manual(values=c("No quota" = "springgreen3","Non-effective quota" = "steelblue1" ,"Effective quota"="indianred2"), drop = FALSE) + 
  scale_shape_manual(values=c("No quota" = 16,"Non-effective quota" = 17, "Effective quota" = 15, drop = FALSE))+
  xlab("Satisfaction") +
  ylab("")
  



## -------------------------------------------------------------------------
ggarrange(Euro_plot_1, Euro_plot_2, Euro_plot_3, ncol=3, nrow=1, common.legend = TRUE, legend="bottom") 





## -------------------------------------------------------------------------
ggsave("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Figures/figure_A3.pdf", width = 8.5, height = 13.5, units = "in")




## -------------------------------------------------------------------------
ggarrange(Euro_plot_4, Euro_plot_5, Euro_plot_6, ncol=3, nrow=1, common.legend = TRUE, legend="bottom") 


## -------------------------------------------------------------------------
ggsave("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Figures/figure_A4.pdf", width = 8.5, height = 13.5, units = "in")



## -------------------------------------------------------------------------
Latino_analysis <- readRDS("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Analysis Data/Latino_analysis.rds")



## -------------------------------------------------------------------------
Latino_analysis <- Latino_analysis %>%
  dplyr::mutate(quota_effect = case_when(
                           quota_effect == "No Quota" ~ "No quota",
                           quota_effect == "Non-Effective" ~ "Non-effective quota",
                           quota_effect == "Effective" ~ "Effective quota"))
                



Latino_agg <- Latino_analysis %>%
 group_by(Country_year, quota_effect)%>%
  summarize_at(vars(Satisfaction = Satisfaction), 
                                   mean, na.rm = TRUE)



Latino_agg$quota_effect <- factor(Latino_agg$quota_effect , levels = c("No quota", "Non-effective quota", "Effective quota"))

Latino_agg <- Latino_agg[order(Latino_agg$Country_year),]


Latino_agg1 <- Latino_agg[1:102,]
Latino_agg2 <- Latino_agg[103:205,]
Latino_agg3 <- Latino_agg[206:319,]



## -------------------------------------------------------------------------


Latino_agg1 <- as.data.frame(Latino_agg1[order(rev(Latino_agg1$Country_year), decreasing = TRUE), ])

Latino_plot_1 <- ggplot(Latino_agg1, aes(x = Satisfaction, y = reorder(Country_year, desc(Country_year)))) +
  geom_point(aes(shape=quota_effect, colour = quota_effect), size = 1.3, position=position_dodge(width=0)) + 
  scale_x_continuous(limits = c(0, 1)) +
  theme_bw(10) +
  theme(axis.text.y = element_text(size = rel(.7)),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(.9)),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.4),
        panel.grid.minor.x = element_blank(), 
        legend.position = "bottom", 
        legend.title = element_blank()) +
  scale_color_manual(values=c("springgreen3","steelblue1","indianred2")) +
  xlab("Satisfaction") +
  ylab("")


Latino_agg2 <- as.data.frame(Latino_agg2[order(rev(Latino_agg2$Country_year), decreasing = TRUE), ])

Latino_plot_2 <- ggplot(Latino_agg2, aes(x = Satisfaction, y = reorder(Country_year, desc(Country_year)))) +
  geom_point(aes(shape=quota_effect, colour = quota_effect), size = 1.3, position=position_dodge(width=0)) + 
  scale_x_continuous(limits = c(0, 1)) +
  theme_bw(10) +
  theme(axis.text.y = element_text(size = rel(.65)),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(.9)),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.4),
        panel.grid.minor.x = element_blank(), 
        legend.position = "none") +
  scale_color_manual(values=c("springgreen3","steelblue1","indianred2")) +
  xlab("Satisfaction") +
  ylab("")


Latino_agg3 <- as.data.frame(Latino_agg3[order(rev(Latino_agg3$Country_year), decreasing = TRUE), ])


Latino_plot_3 <- ggplot(Latino_agg3, aes(x = Satisfaction, y = reorder(Country_year, desc(Country_year)))) +
  geom_point(aes(shape=quota_effect, colour = quota_effect), size = 1.3, position=position_dodge(width=0)) + 
  scale_x_continuous(limits = c(0, 1)) +
  theme_bw(10) +
  theme(axis.text.y = element_text(size = rel(.65)),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(.9)),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.4),
        panel.grid.minor.x = element_blank(), 
        legend.position = "none") +
  scale_color_manual(values=c("springgreen3","steelblue1","indianred2")) +
  xlab("Satisfaction") +
  ylab("")


## -------------------------------------------------------------------------

ggarrange(Latino_plot_1, Latino_plot_2, Latino_plot_3, ncol=3, nrow=1, common.legend = TRUE, legend="bottom") #5.5x7 inches


## -------------------------------------------------------------------------
ggsave("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Figures/figure_A5.pdf", width = 8.5, height = 13.5, units = "in")




## -------------------------------------------------------------------------
#bring in the loaded models 
Americas_hyp1 <- readRDS("Americas_hyp1.rds")
CSES_hyp1 <- readRDS("CSES_hyp1.rds")
Euro_hyp1 <- readRDS("Euro_hyp1.rds")
Latino_hyp1 <- readRDS("Latino_hyp1.rds")




## -------------------------------------------------------------------------
Americas_hyp1.mod <- broom.mixed::tidy(Americas_hyp1) %>% filter(effect == "fixed")
CSES_hyp1.mod <- broom.mixed::tidy(CSES_hyp1) %>% filter(effect == "fixed")

Euro_hyp1.mod <- broom.mixed::tidy(Euro_hyp1) %>% filter(effect == "fixed")
Latino_hyp1.mod <- broom.mixed::tidy(Latino_hyp1) %>% filter(effect == "fixed")





## -------------------------------------------------------------------------
# Number of unique surveys for that one random effect
Americas_hyp1_num_survey <- as.numeric(sapply(lme4::ranef(Americas_hyp1),nrow)[1])
CSES_hyp1_num_survey <- as.numeric(sapply(lme4::ranef(CSES_hyp1),nrow)[1])
Euro_hyp1_num_survey <- as.numeric(sapply(lme4::ranef(Euro_hyp1),nrow)[1])
Latino_hyp1_num_survey <- as.numeric(sapply(lme4::ranef(Latino_hyp1),nrow)[1])


## -------------------------------------------------------------------------


# variance for survey for base models
var_Americas_hyp1.mod <- round(as.numeric(get_variance_random(Americas_hyp1)), 3) 

var_CSES_hyp1.mod <- round(as.numeric(get_variance_random(CSES_hyp1)), 3) 

var_Euro_hyp1.mod <- round(as.numeric(get_variance_random(Euro_hyp1)), 3) 

var_Latino_hyp1.mod <- round(as.numeric(get_variance_random(Latino_hyp1)), 3)






## -------------------------------------------------------------------------
sd_Americas_hyp1.mod <- round(as.numeric(attributes(lme4::VarCorr(Americas_hyp1)$"Country_year")$stddev), 3)

sd_CSES_hyp1.mod <- round(as.numeric(attributes(lme4::VarCorr(CSES_hyp1)$"Country_year")$stddev), 3)
sd_Euro_hyp1.mod <- round(as.numeric(attributes(lme4::VarCorr(Euro_hyp1)$"Country_year")$stddev), 3)
sd_Latino_hyp1.mod <- round(as.numeric(attributes(lme4::VarCorr(Latino_hyp1)$"Country_year")$stddev), 3)


## -------------------------------------------------------------------------
# AIC for models
AIC_Americas_hyp1.mod <- round(as.numeric(AIC(logLik(Americas_hyp1))), 3)
AIC_CSES_hyp1.mod <- round(as.numeric(AIC(logLik(CSES_hyp1))), 3)
AIC_Euro_hyp1.mod <- round(as.numeric(AIC(logLik(Euro_hyp1))), 3)
AIC_Latino_hyp1.mod <- round(as.numeric(AIC(logLik(Latino_hyp1))), 3)


## -------------------------------------------------------------------------
hyp1_stats<- tribble(~stat, ~Americas_hyp1, ~CSES_hyp1, ~Euro_hyp1, ~Latino_hyp1,
"Number of respondents", nobs(Americas_hyp1), nobs(CSES_hyp1), nobs(Euro_hyp1), nobs(Latino_hyp1),      
"Number of surveys", Americas_hyp1_num_survey, CSES_hyp1_num_survey , Euro_hyp1_num_survey, Latino_hyp1_num_survey,
        "var(survey-level constants)", var_Americas_hyp1.mod,var_CSES_hyp1.mod, var_Euro_hyp1.mod, var_Latino_hyp1.mod,
        "sd(survey-level constants", sd_Americas_hyp1.mod, sd_CSES_hyp1.mod, sd_Euro_hyp1.mod, sd_Latino_hyp1.mod,
        "AIC", AIC_Americas_hyp1.mod,AIC_CSES_hyp1.mod, AIC_Euro_hyp1.mod, AIC_Latino_hyp1.mod,
        ) 


## -------------------------------------------------------------------------
stargazer(Americas_hyp1, CSES_hyp1, Euro_hyp1, Latino_hyp1, 
          type="latex", 
          # Below: manually supply tidied coefficients and standard errors
          coef = list(Americas_hyp1.mod$estimate ,CSES_hyp1.mod$estimate, Euro_hyp1.mod$estimate, Latino_hyp1.mod$estimate),
          se = list(Americas_hyp1.mod$std.error, CSES_hyp1.mod$std.error, Euro_hyp1.mod$std.error, Latino_hyp1.mod$std.error),
           star.char = c( "*"),
          star.cutoffs = c( .05),
          # Omit model statistics by default...
          omit.table.layout = "s",
          add.lines = lapply(1:nrow(hyp1_stats), function(i) unlist(hyp1_stats[i, ])),

          model.names = FALSE,
          column.labels = c("AmericasBarometer","CSES", "Eurobarometer", "Latinobarometer")
          )



## -------------------------------------------------------------------------
#bring in the loaded models 
Americas_hyp1_generation <- readRDS("Americas_hyp1_generation.rds")
CSES_hyp1_generation <- readRDS("CSES_hyp1_generation.rds")
Euro_hyp1_generation <- readRDS("Euro_hyp1_generation.rds")
Latino_hyp1_generation <- readRDS("Latino_hyp1_generation.rds")




## -------------------------------------------------------------------------
Americas_hyp1_generation.mod <- broom.mixed::tidy(Americas_hyp1_generation) %>% filter(effect == "fixed")
CSES_hyp1_generation.mod <- broom.mixed::tidy(CSES_hyp1_generation) %>% filter(effect == "fixed")

Euro_hyp1_generation.mod <- broom.mixed::tidy(Euro_hyp1_generation) %>% filter(effect == "fixed")
Latino_hyp1_generation.mod <- broom.mixed::tidy(Latino_hyp1_generation) %>% filter(effect == "fixed")





## -------------------------------------------------------------------------
# Number of unique surveys for that one random effect
Americas_hyp1_generation_num_survey <- as.numeric(sapply(lme4::ranef(Americas_hyp1_generation),nrow)[1])
CSES_hyp1_generation_num_survey <- as.numeric(sapply(lme4::ranef(CSES_hyp1_generation),nrow)[1])
Euro_hyp1_generation_num_survey <- as.numeric(sapply(lme4::ranef(Euro_hyp1_generation),nrow)[1])
Latino_hyp1_generation_num_survey <- as.numeric(sapply(lme4::ranef(Latino_hyp1_generation),nrow)[1])


## -------------------------------------------------------------------------

# variance for survey for base generation models
var_Americas_hyp1_generation.mod <- round(as.numeric(get_variance_random(Americas_hyp1_generation)), 3) 

var_CSES_hyp1_generation.mod <- round(as.numeric(get_variance_random(CSES_hyp1_generation)), 3) 

var_Euro_hyp1_generation.mod <- round(as.numeric(get_variance_random(Euro_hyp1_generation)), 3) 

var_Latino_hyp1_generation.mod <- round(as.numeric(get_variance_random(Latino_hyp1_generation)), 3)






## -------------------------------------------------------------------------
sd_Americas_hyp1_generation.mod <- round(as.numeric(attributes(lme4::VarCorr(Americas_hyp1_generation)$"Country_year")$stddev), 3)

sd_CSES_hyp1_generation.mod <- round(as.numeric(attributes(lme4::VarCorr(CSES_hyp1_generation)$"Country_year")$stddev), 3)
sd_Euro_hyp1_generation.mod <- round(as.numeric(attributes(lme4::VarCorr(Euro_hyp1_generation)$"Country_year")$stddev), 3)
sd_Latino_hyp1_generation.mod <- round(as.numeric(attributes(lme4::VarCorr(Latino_hyp1_generation)$"Country_year")$stddev), 3)


## -------------------------------------------------------------------------
# AIC for models
AIC_Americas_hyp1_generation.mod <- round(as.numeric(AIC(logLik(Americas_hyp1_generation))), 3)
AIC_CSES_hyp1_generation.mod <- round(as.numeric(AIC(logLik(CSES_hyp1_generation))), 3)
AIC_Euro_hyp1_generation.mod <- round(as.numeric(AIC(logLik(Euro_hyp1_generation))), 3)
AIC_Latino_hyp1_generation.mod <- round(as.numeric(AIC(logLik(Latino_hyp1_generation))), 3)


## -------------------------------------------------------------------------
hyp1_generation_stats<- tribble(~stat, ~Americas_hyp1_generation, ~CSES_hyp1_generation, ~Euro_hyp1_generation, ~Latino_hyp1_generation,
"Number of respondents", nobs(Americas_hyp1_generation), nobs(CSES_hyp1_generation), nobs(Euro_hyp1_generation), nobs(Latino_hyp1_generation),      
"Number of surveys", Americas_hyp1_generation_num_survey, CSES_hyp1_generation_num_survey , Euro_hyp1_generation_num_survey, Latino_hyp1_generation_num_survey,
        "var(survey-level constants)", var_Americas_hyp1_generation.mod,var_CSES_hyp1_generation.mod, var_Euro_hyp1_generation.mod, var_Latino_hyp1_generation.mod,
        "sd(survey-level constants", sd_Americas_hyp1_generation.mod, sd_CSES_hyp1_generation.mod, sd_Euro_hyp1_generation.mod, sd_Latino_hyp1_generation.mod,
        "AIC", AIC_Americas_hyp1_generation.mod,AIC_CSES_hyp1_generation.mod, AIC_Euro_hyp1_generation.mod, AIC_Latino_hyp1_generation.mod,
        ) 


## -------------------------------------------------------------------------
##shorter mod name otherwise there is a stargazer issue


amer.gen <- Americas_hyp1_generation
cses.gen <- CSES_hyp1_generation
euro.gen <- Euro_hyp1_generation
lat.gen<- Latino_hyp1_generation


## -------------------------------------------------------------------------
stargazer::stargazer(amer.gen,
                     cses.gen, 
                     euro.gen, 
                     lat.gen, 
          type="latex", 
          # ^  Notice M2 is called twice. I'm going somewhere with this.
          # Below: manually supply tidied coefficients and standard errors
          coef = list(Americas_hyp1_generation.mod$estimate ,CSES_hyp1_generation.mod$estimate, Euro_hyp1_generation.mod$estimate, Latino_hyp1_generation.mod$estimate),
          se = list(Americas_hyp1_generation.mod$std.error, CSES_hyp1_generation.mod$std.error, Euro_hyp1_generation.mod$std.error, Latino_hyp1_generation.mod$std.error),
           star.char = c( "*"),
          star.cutoffs = c( .05),
          # Omit model statistics by default...
          omit.table.layout = "s",
          # ...but supply your own that you created (with random effects)
          add.lines = lapply(1:nrow(hyp1_generation_stats), function(i) unlist(hyp1_generation_stats[i, ])),
        #  covariate.labels = c("Age","Female","Years of Education", "Unemployed", "Household Income (Deciles)", "Ideology (L to R)"),
         # notes="<small>Data: ESS, Round 9 (United Kingdom)</small>",
        #  dep.var.labels="Pro-Immigration Sentiment",
          model.names = FALSE,
          column.labels = c("AmericasBarometer","CSES", "Eurobarometer", "Latinobarometer")
          )



## -------------------------------------------------------------------------
#bring in the loaded models 
Americas_hyp2 <- readRDS("Americas_hyp2.rds")
CSES_hyp2 <- readRDS("CSES_hyp2.rds")
Euro_hyp2 <- readRDS("Euro_hyp2.rds")
Latino_hyp2 <- readRDS("Latino_hyp2.rds")




## -------------------------------------------------------------------------
Americas_hyp2.mod <- broom.mixed::tidy(Americas_hyp2) %>% filter(effect == "fixed")
CSES_hyp2.mod <- broom.mixed::tidy(CSES_hyp2) %>% filter(effect == "fixed")

Euro_hyp2.mod <- broom.mixed::tidy(Euro_hyp2) %>% filter(effect == "fixed")
Latino_hyp2.mod <- broom.mixed::tidy(Latino_hyp2) %>% filter(effect == "fixed")





## -------------------------------------------------------------------------
# Number of unique surveys for that one random effect
Americas_hyp2_num_survey <- as.numeric(sapply(lme4::ranef(Americas_hyp2),nrow)[1])
CSES_hyp2_num_survey <- as.numeric(sapply(lme4::ranef(CSES_hyp2),nrow)[1])
Euro_hyp2_num_survey <- as.numeric(sapply(lme4::ranef(Euro_hyp2),nrow)[1])
Latino_hyp2_num_survey <- as.numeric(sapply(lme4::ranef(Latino_hyp2),nrow)[1])


## -------------------------------------------------------------------------


# variance for survey for gender models
var_Americas_hyp2.mod <- round(as.numeric(get_variance_random(Americas_hyp2)), 3) 

var_CSES_hyp2.mod <- round(as.numeric(get_variance_random(CSES_hyp2)), 3) 

var_Euro_hyp2.mod <- round(as.numeric(get_variance_random(Euro_hyp2)), 3) 

var_Latino_hyp2.mod <- round(as.numeric(get_variance_random(Latino_hyp2)), 3)






## -------------------------------------------------------------------------
# sd for survey for gender models

sd_Americas_hyp2.mod <- round(as.numeric(attributes(lme4::VarCorr(Americas_hyp2)$"Country_year")$stddev), 3)

sd_CSES_hyp2.mod <- round(as.numeric(attributes(lme4::VarCorr(CSES_hyp2)$"Country_year")$stddev), 3)
sd_Euro_hyp2.mod <- round(as.numeric(attributes(lme4::VarCorr(Euro_hyp2)$"Country_year")$stddev), 3)
sd_Latino_hyp2.mod <- round(as.numeric(attributes(lme4::VarCorr(Latino_hyp2)$"Country_year")$stddev), 3)


## -------------------------------------------------------------------------

# variance for slope for gender models
var_slope_Americas_hyp2.mod <- round(as.numeric(get_variance_slope(Americas_hyp2)), 4) 

var_slope_CSES_hyp2.mod <- round(as.numeric(get_variance_slope(CSES_hyp2)), 4) 

var_slope_Euro_hyp2.mod <- round(as.numeric(get_variance_slope(Euro_hyp2)), 4) 

var_slope_Latino_hyp2.mod <- round(as.numeric(get_variance_slope(Latino_hyp2)), 4)






## -------------------------------------------------------------------------
# sd for slope in gender models
sd_slope_Americas_hyp2.mod <- round(as.numeric(attributes(lme4::VarCorr(Americas_hyp2)$"Country_year")$stddev)[2], 3)

sd_slope_CSES_hyp2.mod <- round(as.numeric(attributes(lme4::VarCorr(CSES_hyp2)$"Country_year")$stddev)[2], 3)

sd_slope_Euro_hyp2.mod <- round(as.numeric(attributes(lme4::VarCorr(Euro_hyp2)$"Country_year")$stddev)[2], 3)

sd_slope_Latino_hyp2.mod <- round(as.numeric(attributes(lme4::VarCorr(Latino_hyp2)$"Country_year")$stddev)[2], 3)



## -------------------------------------------------------------------------
# AIC for models
AIC_Americas_hyp2.mod <- round(as.numeric(AIC(logLik(Americas_hyp2))), 3)
AIC_CSES_hyp2.mod <- round(as.numeric(AIC(logLik(CSES_hyp2))), 3)
AIC_Euro_hyp2.mod <- round(as.numeric(AIC(logLik(Euro_hyp2))), 3)
AIC_Latino_hyp2.mod <- round(as.numeric(AIC(logLik(Latino_hyp2))), 3)


## -------------------------------------------------------------------------
hyp_gender_stats<- tribble(~stat, ~Americas_hyp2, ~CSES_hyp2, ~Euro_hyp2, ~Latino_hyp2,
"Number of respondents", nobs(Americas_hyp2), nobs(CSES_hyp2), nobs(Euro_hyp2), nobs(Latino_hyp2),      
"Number of surveys", Americas_hyp2_num_survey, CSES_hyp2_num_survey , Euro_hyp2_num_survey, Latino_hyp2_num_survey,
        "var(survey-level constants)", var_Americas_hyp2.mod,var_CSES_hyp2.mod, var_Euro_hyp2.mod, var_Latino_hyp2.mod,
        "sd(survey-level constants)", sd_Americas_hyp2.mod, sd_CSES_hyp2.mod, sd_Euro_hyp2.mod, sd_Latino_hyp2.mod,
        "var(Gender random coefficients)", var_slope_Americas_hyp2.mod,var_slope_CSES_hyp2.mod, var_slope_Euro_hyp2.mod, var_slope_Latino_hyp2.mod,
        "sd(Gender random coefficients)", sd_slope_Americas_hyp2.mod, sd_slope_CSES_hyp2.mod, sd_slope_Euro_hyp2.mod, sd_slope_Latino_hyp2.mod,
        "AIC", AIC_Americas_hyp2.mod,AIC_CSES_hyp2.mod, AIC_Euro_hyp2.mod, AIC_Latino_hyp2.mod,
        ) 


## -------------------------------------------------------------------------
stargazer(Americas_hyp2, CSES_hyp2, Euro_hyp2, Latino_hyp2, 
          type="latex", 
          # ^  Notice M2 is called twice. I'm going somewhere with this.
          # Below: manually supply tidied coefficients and standard errors
          coef = list(Americas_hyp2.mod$estimate ,CSES_hyp2.mod$estimate, Euro_hyp2.mod$estimate, Latino_hyp2.mod$estimate),
          se = list(Americas_hyp2.mod$std.error, CSES_hyp2.mod$std.error, Euro_hyp2.mod$std.error, Latino_hyp2.mod$std.error),
           star.char = c( "*"),
          star.cutoffs = c( .05),
          # Omit model statistics by default...
          omit.table.layout = "s",
          # ...but supply your own that you created (with random effects)
          add.lines = lapply(1:nrow(hyp_gender_stats), function(i) unlist(hyp_gender_stats[i, ])),
        #  covariate.labels = c("Age","Female","Years of Education", "Unemployed", "Household Income (Deciles)", "Ideology (L to R)"),
         # notes="<small>Data: ESS, Round 9 (United Kingdom)</small>",
        #  dep.var.labels="Pro-Immigration Sentiment",
          model.names = FALSE,
          column.labels = c("AmericasBarometer","CSES", "Eurobarometer", "Latinobarometer")
          )



## -------------------------------------------------------------------------
#bring in the loaded models 
Americas_hyp3a <- readRDS("Americas_hyp3a.rds")
CSES_hyp3a <- readRDS("CSES_hyp3a.rds")
Euro_hyp3a <- readRDS("Euro_hyp3a.rds")
Latino_hyp3a <- readRDS("Latino_hyp3a.rds")




## -------------------------------------------------------------------------
Americas_hyp3a.mod <- broom.mixed::tidy(Americas_hyp3a) %>% filter(effect == "fixed")
CSES_hyp3a.mod <- broom.mixed::tidy(CSES_hyp3a) %>% filter(effect == "fixed")

Euro_hyp3a.mod <- broom.mixed::tidy(Euro_hyp3a) %>% filter(effect == "fixed")
Latino_hyp3a.mod <- broom.mixed::tidy(Latino_hyp3a) %>% filter(effect == "fixed")





## -------------------------------------------------------------------------
# Number of unique surveys for that one random effect
Americas_hyp3a_num_survey <- as.numeric(sapply(lme4::ranef(Americas_hyp3a),nrow)[1])
CSES_hyp3a_num_survey <- as.numeric(sapply(lme4::ranef(CSES_hyp3a),nrow)[1])
Euro_hyp3a_num_survey <- as.numeric(sapply(lme4::ranef(Euro_hyp3a),nrow)[1])
Latino_hyp3a_num_survey <- as.numeric(sapply(lme4::ranef(Latino_hyp3a),nrow)[1])


## -------------------------------------------------------------------------

# variance for survey for ideology models
var_Americas_hyp3a.mod <- round(as.numeric(get_variance_random(Americas_hyp3a)), 3) 

var_CSES_hyp3a.mod <- round(as.numeric(get_variance_random(CSES_hyp3a)), 3) 

var_Euro_hyp3a.mod <- round(as.numeric(get_variance_random(Euro_hyp3a)), 3) 

var_Latino_hyp3a.mod <- round(as.numeric(get_variance_random(Latino_hyp3a)), 3)






## -------------------------------------------------------------------------
# SD for survey in models
sd_Americas_hyp3a.mod <- round(as.numeric(attributes(lme4::VarCorr(Americas_hyp3a)$"Country_year")$stddev), 3)

sd_CSES_hyp3a.mod <- round(as.numeric(attributes(lme4::VarCorr(CSES_hyp3a)$"Country_year")$stddev), 3)

sd_Euro_hyp3a.mod <- round(as.numeric(attributes(lme4::VarCorr(Euro_hyp3a)$"Country_year")$stddev), 3)

sd_Latino_hyp3a.mod <- round(as.numeric(attributes(lme4::VarCorr(Latino_hyp3a)$"Country_year")$stddev), 3)


## -------------------------------------------------------------------------

# variance for slope for ideology models
var_slope_Americas_hyp3a.mod <- round(as.numeric(get_variance_slope(Americas_hyp3a)), 4) 

var_slope_CSES_hyp3a.mod <- round(as.numeric(get_variance_slope(CSES_hyp3a)), 4) 

var_slope_Euro_hyp3a.mod <- round(as.numeric(get_variance_slope(Euro_hyp3a)), 4) 

var_slope_Latino_hyp3a.mod <- round(as.numeric(get_variance_slope(Latino_hyp3a)), 4)






## -------------------------------------------------------------------------
# SD for slope in ideology models
sd_slope_Americas_hyp3a.mod <- round(as.numeric(attributes(lme4::VarCorr(Americas_hyp3a)$"Country_year")$stddev)[2], 3)

sd_slope_CSES_hyp3a.mod <- round(as.numeric(attributes(lme4::VarCorr(CSES_hyp3a)$"Country_year")$stddev)[2], 3)

sd_slope_Euro_hyp3a.mod <- round(as.numeric(attributes(lme4::VarCorr(Euro_hyp3a)$"Country_year")$stddev)[2], 3)

sd_slope_Latino_hyp3a.mod <- round(as.numeric(attributes(lme4::VarCorr(Latino_hyp3a)$"Country_year")$stddev)[2], 3)



## -------------------------------------------------------------------------
# AIC for models
AIC_Americas_hyp3a.mod <- round(as.numeric(AIC(logLik(Americas_hyp3a))), 3)
AIC_CSES_hyp3a.mod <- round(as.numeric(AIC(logLik(CSES_hyp3a))), 3)
AIC_Euro_hyp3a.mod <- round(as.numeric(AIC(logLik(Euro_hyp3a))), 3)
AIC_Latino_hyp3a.mod <- round(as.numeric(AIC(logLik(Latino_hyp3a))), 3)


## -------------------------------------------------------------------------
hyp_ideology_stats<- tribble(~stat, ~Americas_hyp3a, ~CSES_hyp3a, ~Euro_hyp3a, ~Latino_hyp3a,
"Number of respondents", nobs(Americas_hyp3a), nobs(CSES_hyp3a), nobs(Euro_hyp3a), nobs(Latino_hyp3a),      
"Number of surveys", Americas_hyp3a_num_survey, CSES_hyp3a_num_survey , Euro_hyp3a_num_survey, Latino_hyp3a_num_survey,
        "var(survey-level constants)", var_Americas_hyp3a.mod,var_CSES_hyp3a.mod, var_Euro_hyp3a.mod, var_Latino_hyp3a.mod,
        "sd(survey-level constants)", sd_Americas_hyp3a.mod, sd_CSES_hyp3a.mod, sd_Euro_hyp3a.mod, sd_Latino_hyp3a.mod,
        "var(Gender random coefficients)", var_slope_Americas_hyp3a.mod,var_slope_CSES_hyp3a.mod, var_slope_Euro_hyp3a.mod, var_slope_Latino_hyp3a.mod,
        "sd(Gender random coefficients)", sd_slope_Americas_hyp3a.mod, sd_slope_CSES_hyp3a.mod, sd_slope_Euro_hyp3a.mod, sd_slope_Latino_hyp3a.mod,
        "AIC", AIC_Americas_hyp3a.mod,AIC_CSES_hyp3a.mod, AIC_Euro_hyp3a.mod, AIC_Latino_hyp3a.mod,
        ) 


## -------------------------------------------------------------------------
stargazer(Americas_hyp3a, CSES_hyp3a, Euro_hyp3a, Latino_hyp3a, 
          type="latex", 
          coef = list(Americas_hyp3a.mod$estimate ,CSES_hyp3a.mod$estimate, Euro_hyp3a.mod$estimate, Latino_hyp3a.mod$estimate),
          se = list(Americas_hyp3a.mod$std.error, CSES_hyp3a.mod$std.error, Euro_hyp3a.mod$std.error, Latino_hyp3a.mod$std.error),
           star.char = c( "*"),
          star.cutoffs = c( .05),
          # Omit model statistics by default...
          omit.table.layout = "s",
          # ...but supply your own that you created (with random effects)
          add.lines = lapply(1:nrow(hyp_ideology_stats), function(i) unlist(hyp_ideology_stats[i, ])),

          model.names = FALSE,
          column.labels = c("AmericasBarometer","CSES", "Eurobarometer", "Latinobarometer")
          )



## -------------------------------------------------------------------------
#bring in the loaded models 
Americas_hyp3b <- readRDS("Americas_hyp3b.rds")




## -------------------------------------------------------------------------
Americas_hyp3b.mod <- broom.mixed::tidy(Americas_hyp3b) %>% filter(effect == "fixed")





## -------------------------------------------------------------------------
# Number of unique surveys for that one random effect
Americas_hyp3b_num_survey <- as.numeric(sapply(lme4::ranef(Americas_hyp3b),nrow)[1])



## -------------------------------------------------------------------------

# variance for survey for support model
var_Americas_hyp3b.mod <- round(as.numeric(get_variance_random(Americas_hyp3b)), 3) 







## -------------------------------------------------------------------------
# SD for survey in model
sd_Americas_hyp3b.mod <- round(as.numeric(attributes(lme4::VarCorr(Americas_hyp3b)$"Country_year")$stddev), 3)




## -------------------------------------------------------------------------
# AIC for model
AIC_Americas_hyp3b.mod <- round(as.numeric(AIC(logLik(Americas_hyp3b))), 3)



## -------------------------------------------------------------------------
hyp3b_stats<- tribble(~stat, ~Americas_hyp3b,
"Number of respondents", nobs(Americas_hyp3b),
"Number of surveys", Americas_hyp3b_num_survey, 
        "var(survey-level constants)", var_Americas_hyp3b.mod,
        "sd(survey-level constants)", sd_Americas_hyp3b.mod,
        "AIC", AIC_Americas_hyp3b.mod,
        ) 


## -------------------------------------------------------------------------
stargazer(Americas_hyp3b, 
          type="latex", 
          coef = list(Americas_hyp3b.mod$estimate),
          se = list(Americas_hyp3b.mod$std.error),
           star.char = c( "*"),
          star.cutoffs = c( .05),
          # Omit model statistics by default...
          omit.table.layout = "s",
          # ...but supply your own that you created (with random effects)
          add.lines = lapply(1:nrow(hyp3b_stats), function(i) unlist(hyp3b_stats[i, ])),
          model.names = FALSE,
          column.labels = c("AmericasBarometer")
          )



## -------------------------------------------------------------------------
#bring in the loaded models 
Americas_hyp4 <- readRDS("Americas_hyp4.rds")
CSES_hyp4 <- readRDS("CSES_hyp4.rds")
Euro_hyp4 <- readRDS("Euro_hyp4.rds")
Latino_hyp4 <- readRDS("Latino_hyp4.rds")




## -------------------------------------------------------------------------
library(broom.mixed)
Americas_hyp4.mod <- broom.mixed::tidy(Americas_hyp4) %>% filter(effect == "fixed")
CSES_hyp4.mod <- broom.mixed::tidy(CSES_hyp4) %>% filter(effect == "fixed")

Euro_hyp4.mod <- broom.mixed::tidy(Euro_hyp4) %>% filter(effect == "fixed")
Latino_hyp4.mod <- broom.mixed::tidy(Latino_hyp4) %>% filter(effect == "fixed")





## -------------------------------------------------------------------------
# Number of unique surveys for that one random effect
Americas_hyp4_num_survey <- as.numeric(sapply(lme4::ranef(Americas_hyp4),nrow)[1])

CSES_hyp4_num_survey <- as.numeric(sapply(lme4::ranef(CSES_hyp4),nrow)[1])

Euro_hyp4_num_survey <- as.numeric(sapply(lme4::ranef(Euro_hyp4),nrow)[1])

Latino_hyp4_num_survey <- as.numeric(sapply(lme4::ranef(Latino_hyp4),nrow)[1])


## -------------------------------------------------------------------------
# variance for survey for corruption models
var_Americas_hyp4.mod <- round(as.numeric(get_variance_random(Americas_hyp4)), 3) 

var_CSES_hyp4.mod <- round(as.numeric(get_variance_random(CSES_hyp4)), 3) 

var_Euro_hyp4.mod <- round(as.numeric(get_variance_random(Euro_hyp4)), 3) 

var_Latino_hyp4.mod <- round(as.numeric(get_variance_random(Latino_hyp4)), 3)






## -------------------------------------------------------------------------
# SD for survey random effect
sd_Americas_hyp4.mod <- round(as.numeric(attributes(lme4::VarCorr(Americas_hyp4)$"Country_year")$stddev), 3)

sd_CSES_hyp4.mod <- round(as.numeric(attributes(lme4::VarCorr(CSES_hyp4)$"Country_year")$stddev), 3)

sd_Euro_hyp4.mod <- round(as.numeric(attributes(lme4::VarCorr(Euro_hyp4)$"Country_year")$stddev), 3)

sd_Latino_hyp4.mod <- round(as.numeric(attributes(lme4::VarCorr(Latino_hyp4)$"Country_year")$stddev), 3)


## -------------------------------------------------------------------------
# AIC for models
AIC_Americas_hyp4.mod <- round(as.numeric(AIC(logLik(Americas_hyp4))), 3)

AIC_CSES_hyp4.mod <- round(as.numeric(AIC(logLik(CSES_hyp4))), 3)

AIC_Euro_hyp4.mod <- round(as.numeric(AIC(logLik(Euro_hyp4))), 3)

AIC_Latino_hyp4.mod <- round(as.numeric(AIC(logLik(Latino_hyp4))), 3)


## -------------------------------------------------------------------------
hyp4_stats<- tribble(~stat, ~Americas_hyp4, ~CSES_hyp4, ~Euro_hyp4, ~Latino_hyp4,
"Number of respondents", nobs(Americas_hyp4), nobs(CSES_hyp4), nobs(Euro_hyp4), nobs(Latino_hyp4),      
"Number of surveys", Americas_hyp4_num_survey, CSES_hyp4_num_survey , Euro_hyp4_num_survey, Latino_hyp4_num_survey,
        "var(survey-level constants)", var_Americas_hyp4.mod,var_CSES_hyp4.mod, var_Euro_hyp4.mod, var_Latino_hyp4.mod,
        "sd(survey-level constants", sd_Americas_hyp4.mod, sd_CSES_hyp4.mod, sd_Euro_hyp4.mod, sd_Latino_hyp4.mod,
        "AIC", AIC_Americas_hyp4.mod,AIC_CSES_hyp4.mod, AIC_Euro_hyp4.mod, AIC_Latino_hyp4.mod,
        ) 


## -------------------------------------------------------------------------
stargazer(Americas_hyp4, CSES_hyp4, Euro_hyp4, Latino_hyp4, 
          type="latex", 
          coef = list(Americas_hyp4.mod$estimate ,CSES_hyp4.mod$estimate, Euro_hyp4.mod$estimate, Latino_hyp4.mod$estimate),
          se = list(Americas_hyp4.mod$std.error, CSES_hyp4.mod$std.error, Euro_hyp4.mod$std.error, Latino_hyp4.mod$std.error),
           star.char = c( "*"),
          star.cutoffs = c( .05),
          # Omit model statistics by default...
          omit.table.layout = "s",
          # ...but supply your own that you created (with random effects)
          add.lines = lapply(1:nrow(hyp4_stats), function(i) unlist(hyp4_stats[i, ])),
          model.names = FALSE,
          column.labels = c("AmericasBarometer","CSES", "Eurobarometer", "Latinobarometer")
          )



## -------------------------------------------------------------------------
library(dplyr)
library(Synth)
library(gsynth)
library(panelView)
library(ggplot2)
library(stargazer)




## -------------------------------------------------------------------------
Synth_analysis <- readRDS("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Analysis Data/Synth_analysis.rds")



## ----echo = TRUE----------------------------------------------------------

   synth_mod <- gsynth(Satisfaction_mean ~ Quota  + Satisfaction_diff,
                  data = Synth_analysis, index = c("Country","Year"), force = "two-way", CV = TRUE, r = c(0, 3),
                  se = TRUE, estimator = "ife", inference = "parametric", EM = TRUE,
                  nboots = 1000, parallel = FALSE,
                 min.T0 = 5) #minimum pre-treatment


## -------------------------------------------------------------------------
print(synth_mod)
saveRDS(synth_mod, "synth_mod.rds")


## -------------------------------------------------------------------------
est.att <-   synth_mod$est.att

# Average effect on the treated (ATT)
    synth_mod$est.avg
    
# Estimated beta     
    synth_mod$est.beta


## ----echo = TRUE, message = FALSE, results= "hide", fig.show="hide"-------

p1 <-    plot(synth_mod, type = "counterfactual", raw = "all", legendOff = TRUE, shade.post = FALSE,
         main = "", ylab = "Satisfaction with Democracy (All treated)", 
         xlab = "Time relative to treatment (Years)", xlim = c(-31,8), ylim = c(0,1),
         theme.bw = TRUE) +
       theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))  +
      scale_color_manual(values=c("black", "gray87", "gray40")) + #first is the synth, then raw data, then actual
     scale_linetype_manual(values = c("dashed","solid", "solid"))


## -------------------------------------------------------------------------
ggsave("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Figures/figure_6_counterfactual.pdf", width = 6.5, height = 5, units = "in")


## -------------------------------------------------------------------------
p2 <-    plot(synth_mod, type = "gap", raw = "all", legendOff = TRUE, xlim = c(-31,8), ylim = c(-.5,.2), 
         main="", ylab = "Gaps in Satisfaction: Synthetic minus Actual (All treated)",
         xlab = "Time relative to treatment (Years)", 
         theme.bw = TRUE) +
      theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), axis.line = element_line(colour = "black"))  


## -------------------------------------------------------------------------
ggsave("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Figures/figure_6_gap.pdf", width = 6.5, height = 5, units = "in")




## ----echo = TRUE, message = FALSE, results= "hide", fig.show="hide"-------
p3 <-     plot(synth_mod, type = "counterfactual", id = "Greece", raw = "all", legendOff = TRUE, shade.post = FALSE,
         main = "", ylab = "Satisfaction with Democracy (Greece)",  xlab = "Year", ylim = c(0,1),
         theme.bw = TRUE)+
      theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), axis.line = element_line(colour = "black"))  +
      scale_color_manual(values=c("black", "gray87", "gray40")) + #first is the synth, then raw data, then actual
      scale_linetype_manual(values = c("dashed","solid", "solid"))


## -------------------------------------------------------------------------
ggsave("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Figures/figure_7_greece_counterfactual.pdf", width = 5, height = 5, units = "in")


## -------------------------------------------------------------------------
p4 <-    plot(synth_mod, type = "gap", id = "Greece", raw = "all", legendOff = TRUE, xlim = c(-31,15), ylim = c(-.5,.2), 
              main="", ylab = "Gaps in Satisfaction: Synthetic minus Actual (Greece)", xlab = "Time relative to treatment (Years)", 
              theme.bw = TRUE)+
      theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), axis.line = element_line(colour = "black"))  


## -------------------------------------------------------------------------
 ggsave("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Figures/figure_7_greece_gap.pdf", width = 5, height = 5, units = "in")


## -------------------------------------------------------------------------
 p5 <- plot(synth_mod, type = "counterfactual", id = "Portugal", raw = "all", legendOff = TRUE, shade.post = FALSE,
         main = "", ylab = "Satisfaction with Democracy (Portugal)",  xlab = "Year", ylim = c(0,1),
         theme.bw = TRUE)+
      theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      scale_color_manual(values=c("black", "gray87", "gray40")) + #first is the synth, then raw data, then actual
      scale_linetype_manual(values = c("dashed","solid", "solid"))


## -------------------------------------------------------------------------
ggsave("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Figures/figure_7_portugal_counterfactual.pdf", width = 5, height = 5, units = "in")


## -------------------------------------------------------------------------
p6 <-      plot(synth_mod, type = "gap", id = "Portugal", raw = "all", legendOff = TRUE, xlim = c(-31,15), ylim = c(-.5,.2),  
         main="", ylab = "Gaps in Satisfaction: Synthetic minus Actual (Portugal)", xlab = "Time relative to treatment (Years)", 
         theme.bw = TRUE)+
      theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), axis.line = element_line(colour = "black"))  


## -------------------------------------------------------------------------
 ggsave("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Figures/figure_7_portugal_gap.pdf", width = 5, height = 5, units = "in")


## -------------------------------------------------------------------------
p7 <-     plot(synth_mod, type = "counterfactual", id = "Spain", raw = "all", legendOff = TRUE, shade.post = FALSE,
         main = "", ylab = "Satisfaction with Democracy (Spain)",  xlab = "Year", ylim = c(0,1),
         theme.bw = TRUE)+
      theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), axis.line = element_line(colour = "black"))  +
      scale_color_manual(values=c("black", "gray87", "gray40")) + #first is the synth, then raw data, then actual
      scale_linetype_manual(values = c("dashed","solid", "solid"))


## -------------------------------------------------------------------------
ggsave("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Figures/figure_7_spain_counterfactual.pdf", width = 5, height = 5, units = "in")


## -------------------------------------------------------------------------
p8 <-     plot(synth_mod, type = "gap", id = "Spain", raw = "all", legendOff = TRUE, xlim = c(-31,15), ylim = c(-.5,.2), 
         main="", ylab = "Gaps in Satisfaction: Synthetic minus Actual (Spain)", xlab = "Time relative to treatment (Years)", 
         theme.bw = TRUE)+
      theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), axis.line = element_line(colour = "black"))  


## -------------------------------------------------------------------------
 ggsave("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Figures/figure_7_spain_gap.pdf", width = 5, height = 5, units = "in")


## -------------------------------------------------------------------------
p9 <-     plot(synth_mod, type = "counterfactual", id = "Mexico", raw = "all", legendOff = TRUE, shade.post = FALSE,
         main = "", ylab = "Satisfaction with Democracy (Mexico)",  xlab = "Year", ylim = c(0,1),
         theme.bw = TRUE)+
      theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), axis.line = element_line(colour = "black"))  +
      scale_color_manual(values=c("black", "gray87", "gray40")) + #first is the synth, then raw data, then actual
      scale_linetype_manual(values = c("dashed","solid", "solid"))


## -------------------------------------------------------------------------
ggsave("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Figures/figure_7_mexico_counterfactual.pdf", width = 5, height = 5, units = "in")


## -------------------------------------------------------------------------
p10 <-     plot(synth_mod, type = "gap", id = "Mexico", raw = "all", legendOff = TRUE, xlim = c(-31,15), ylim = c(-.5,.2), 
         main="", ylab = "Gaps in Satisfaction: Synthetic minus Actual (Mexico)", xlab = "Time relative to treatment (Years)", 
         theme.bw = TRUE)+
      theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), axis.line = element_line(colour = "black"))  


## -------------------------------------------------------------------------
 ggsave("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Figures/figure_7_mexico_gap.pdf", width = 5, height = 5, units = "in")



## -------------------------------------------------------------------------

   synth_robust_predictors <- gsynth(Satisfaction_mean ~ Quota  + GDP_capita_logged + GDP_growth + Corruption +  Polity_score,
                  data = Synth_analysis, index = c("Country","Year"), force = "two-way", CV = TRUE, r = c(0, 3),
                  se = TRUE, estimator = "ife", inference = "parametric", EM = TRUE,
                  nboots = 1000, parallel = FALSE,
                  min.T0 = 5) #minimum pre-treatment

## -------------------------------------------------------------------------
print(synth_robust_predictors)
saveRDS(synth_robust_predictors, "synth_mod_robust.rds")


## -------------------------------------------------------------------------
  
    synth_robust_predictors$est.att
    synth_robust_predictors$est.avg
    synth_robust_predictors$est.beta


## -------------------------------------------------------------------------
  #weights for each control unit
    synth_robust_predictors$wgt.implied
    sort(synth_robust_predictors$wgt.implied[,1]) #Greece
    sort(synth_robust_predictors$wgt.implied[,2]) #Portugal
    sort(synth_robust_predictors$wgt.implied[,3]) #Spain


## -------------------------------------------------------------------------
stargazer(print(synth_mod))


## -------------------------------------------------------------------------
  #weights for each control unit
country.weights <- synth_mod$wgt.implied
print(country.weights)
stargazer(country.weights)


## -------------------------------------------------------------------------
  
 panelview(Satisfaction_mean ~ Quota, data = Synth_analysis,  index = c("Country","Year"),
            pre.post = TRUE, by.timing = TRUE, 
            main = "",
            cex.axis = 8,  background = "white", gridOff = FALSE, 
            color = c("springgreen3","steelblue1","indianred2", "gray60") )


## -------------------------------------------------------------------------
  
ggsave("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Figures/figure_A6.pdf", width = 11.5, height = 6, units = "in")

  


## -------------------------------------------------------------------------
plot(synth_mod, type = "factors", xlab="Year")


## -------------------------------------------------------------------------
ggsave("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Figures/figure_A7.pdf", width = 6.5, height = 5, units = "in") 


## ----echo = TRUE, message = FALSE, results= "hide", fig.show="hide"-------

robust1 <-    plot(synth_robust_predictors, type = "counterfactual", raw = "all", legendOff = TRUE, shade.post = FALSE,
         main = "", ylab = "Satisfaction with Democracy (All treated)", 
         xlab = "Time relative to treatment (Years)", xlim = c(-31,8), ylim = c(0,1),
         theme.bw = TRUE) +
       theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))  +
      scale_color_manual(values=c("black", "gray87", "gray40")) + #first is the synth, then raw data, then actual
     scale_linetype_manual(values = c("dashed","solid", "solid"))


## -------------------------------------------------------------------------
ggsave("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Figures/figure_A8_counterfactual.pdf", width = 6.5, height = 5, units = "in")


## -------------------------------------------------------------------------
robust2 <-    plot(synth_robust_predictors, type = "gap", raw = "all", legendOff = TRUE, xlim = c(-31,8), ylim = c(-.5,.2), 
         main="", ylab = "Gaps in Satisfaction: Synthetic minus Actual (All treated)",
         xlab = "Time relative to treatment (Years)", 
         theme.bw = TRUE) +
      theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), axis.line = element_line(colour = "black"))  


## -------------------------------------------------------------------------
ggsave("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Figures/figure_A8_gap.pdf", width = 6.5, height = 5, units = "in")




## ----echo = TRUE, message = FALSE, results= "hide", fig.show="hide"-------
robust3 <-     plot(synth_robust_predictors, type = "counterfactual", id = "Greece", raw = "all", legendOff = TRUE, shade.post = FALSE,
         main = "", ylab = "Satisfaction with Democracy (Greece)",  xlab = "Year", ylim = c(0,1),
         theme.bw = TRUE)+
      theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), axis.line = element_line(colour = "black"))  +
      scale_color_manual(values=c("black", "gray87", "gray40")) + #first is the synth, then raw data, then actual
      scale_linetype_manual(values = c("dashed","solid", "solid"))


## -------------------------------------------------------------------------
ggsave("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Figures/figure_A9_greece_counterfactual.pdf", width = 5, height = 5, units = "in")


## -------------------------------------------------------------------------
robust4 <-    plot(synth_robust_predictors, type = "gap", id = "Greece", raw = "all", legendOff = TRUE, xlim = c(-31,15), ylim = c(-.5,.2), 
              main="", ylab = "Gaps in Satisfaction: Synthetic minus Actual (Greece)", xlab = "Time relative to treatment (Years)", 
              theme.bw = TRUE)+
      theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), axis.line = element_line(colour = "black"))  


## -------------------------------------------------------------------------
 ggsave("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Figures/figure_A9_greece_gap.pdf", width = 5, height = 5, units = "in")


## -------------------------------------------------------------------------
 robust5 <- plot(synth_robust_predictors, type = "counterfactual", id = "Portugal", raw = "all", legendOff = TRUE, shade.post = FALSE,
         main = "", ylab = "Satisfaction with Democracy (Portugal)",  xlab = "Year", ylim = c(0,1),
         theme.bw = TRUE)+
      theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      scale_color_manual(values=c("black", "gray87", "gray40")) + #first is the synth, then raw data, then actual
      scale_linetype_manual(values = c("dashed","solid", "solid"))


## -------------------------------------------------------------------------
ggsave("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Figures/figure_A9_portugal_counterfactual.pdf", width = 5, height = 5, units = "in")


## -------------------------------------------------------------------------
robust6 <-      plot(synth_robust_predictors, type = "gap", id = "Portugal", raw = "all", legendOff = TRUE, xlim = c(-31,15), ylim = c(-.5,.2),  
         main="", ylab = "Gaps in Satisfaction: Synthetic minus Actual (Portugal)", xlab = "Time relative to treatment (Years)", 
         theme.bw = TRUE)+
      theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), axis.line = element_line(colour = "black"))  


## -------------------------------------------------------------------------
 ggsave("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Figures/figure_A9_portugal_gap.pdf", width = 5, height = 5, units = "in")


## -------------------------------------------------------------------------
robust7 <-     plot(synth_robust_predictors, type = "counterfactual", id = "Spain", raw = "all", legendOff = TRUE, shade.post = FALSE,
         main = "", ylab = "Satisfaction with Democracy (Spain)",  xlab = "Year", ylim = c(0,1),
         theme.bw = TRUE)+
      theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), axis.line = element_line(colour = "black"))  +
      scale_color_manual(values=c("black", "gray87", "gray40")) + #first is the synth, then raw data, then actual
      scale_linetype_manual(values = c("dashed","solid", "solid"))


## -------------------------------------------------------------------------
ggsave("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Figures/figure_A9_spain_counterfactual.pdf", width = 5, height = 5, units = "in")


## -------------------------------------------------------------------------
robust8 <-     plot(synth_robust_predictors, type = "gap", id = "Spain", raw = "all", legendOff = TRUE, xlim = c(-31,15), ylim = c(-.5,.2), 
         main="", ylab = "Gaps in Satisfaction: Synthetic minus Actual (Spain)", xlab = "Time relative to treatment (Years)", 
         theme.bw = TRUE)+
      theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), axis.line = element_line(colour = "black"))  


## -------------------------------------------------------------------------
 ggsave("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Figures/figure_A9_spain_gap.pdf", width = 5, height = 5, units = "in")


## -------------------------------------------------------------------------
robust9 <-     plot(synth_robust_predictors, type = "counterfactual", id = "Mexico", raw = "all", legendOff = TRUE, shade.post = FALSE,
         main = "", ylab = "Satisfaction with Democracy (Mexico)",  xlab = "Year", ylim = c(0,1),
         theme.bw = TRUE)+
      theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), axis.line = element_line(colour = "black"))  +
      scale_color_manual(values=c("black", "gray87", "gray40")) + #first is the synth, then raw data, then actual
      scale_linetype_manual(values = c("dashed","solid", "solid"))


## -------------------------------------------------------------------------
ggsave("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Figures/figure_A9_mexico_counterfactual.pdf", width = 5, height = 5, units = "in")


## -------------------------------------------------------------------------
robust10 <-     plot(synth_robust_predictors, type = "gap", id = "Mexico", raw = "all", legendOff = TRUE, xlim = c(-31,15), ylim = c(-.5,.2), 
         main="", ylab = "Gaps in Satisfaction: Synthetic minus Actual (Mexico)", xlab = "Time relative to treatment (Years)", 
         theme.bw = TRUE)+
      theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), axis.line = element_line(colour = "black"))  


## -------------------------------------------------------------------------
 ggsave("C:/Users/snpwi/Dropbox/Quota-Satisfaction/EJPR Submission/Data Analysis/Figures/figure_A9_mexico_gap.pdf", width = 5, height = 5, units = "in")


