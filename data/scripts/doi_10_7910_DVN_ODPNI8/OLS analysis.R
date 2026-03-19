### OLS Models ALL

if (!require("pacman")) install.packages("pacman")
pacman::p_load("weights", "interactions", "cjoint", "plm", "interactions", "jtools", "stats", "miceadds", "broom", "RColorBrewer", "ggstatsplot", "ggpubr", "stargazer", "sandwich", "hrbrthemes", "rms", "interplot", "coefplot", "gmodels", "car", "lattice","foreign", "ggplot2", "MASS", "Hmisc", "reshape2", "oddsratio", "tidyr", "psych", "dplyr", "tidyverse", "cjoint")

civility_dat <- read_rds("Gubitz cleaned.rds")

#Demographics
#gender
prop.table(table(civility_dat$gender))

#age, where 35-44 is 1, 35-45 is 2, 45-64 is 3, 65-84 is 4, and 85+ is 5
civility_dat$age.demo <- cut(civility_dat$age, c(17,34,44,66,84,88), labels = c(1:5))
prop.table(table(civility_dat$age.demo))

#income
prop.table(table(civility_dat$income))

#education
prop.table(table(civility_dat$education))

#partyID
prop.table(table(civility_dat$pid))

#ideology
prop.table(table(civility_dat$ideology))

#OLS Analyses

civility_dat$s_job <- ordered(civility_dat$s_job, levels = c("non-elite", "elite"))
civility_dat$s_job <- as.numeric(civility_dat$s_job)
civility_dat$t_job <- ordered(civility_dat$t_job, levels = c("non-elite", "elite"))
civility_dat$t_job <- as.numeric(civility_dat$t_job)
civility_dat$s_white <- ordered(civility_dat$s_race, levels = c("Black", "White"))
civility_dat$s_white <- as.numeric(civility_dat$s_white)
civility_dat$t_white <- ordered(civility_dat$t_race, levels = c("Black", "White"))
civility_dat$t_white <- as.numeric(civility_dat$t_white)

str(civility_dat$s_job)
str(civility_dat$s_inparty)

table(civility_dat$t_white)

civility_dat$uncivil <- nalevs(civility_dat$uncivil)

#Racial Resentment Alpha = 0.74
dfracial <- dplyr::select(civility_dat, rr1, rr2, rr3, rr4)
psych::alpha(dfracial)

#hostile sexism alpha = 0.89
dfsexism <- dplyr::select(civility_dat, sex1, sex2, sex3, sex4)
psych::alpha(dfsexism)

#social dominance orientation alpha = 0.83
dfsdo <- dplyr::select(civility_dat, sdo1, sdo2, sdo3, sdo4, sdo5, sdo6, sdo7, sdo8)
psych::alpha(dfsdo)

#testing some replication stuff for Frimer and Skitka (2020) (Appendix E)
civility_dat <- civility_dat %>% mutate(s_inparty.elite = case_when(s_inparty == 1 & s_job == "elite" ~ 1,
            s_inparty == 1 & s_job == "non-elite" ~ 0))

civility_dat <- civility_dat %>% mutate(t_inparty.elite = case_when(t_inparty == 1 & t_job == "elite" ~ 1,
                                                                    t_inparty == 1 & t_job == "non-elite" ~ 0))

table(civility_dat$t_inparty.elite)

OLS.E <- civility_dat %>% lm(formula = uncivil ~ s_inparty.elite + incivility + police + s_gender + t_gender + s_white + t_white)

stargazer(OLS.E, type = "text",
          se = list(coeftest(OLS.E,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2]), 
          covariate.labels = c("Speaker In-Party Elite", "Slurs", "Threats", "Civility Policing", 
                               "Female Speaker", "Female Target", "White Speaker",
                               "White Target"),
          dep.var.caption  = "Dependent Variable",
          dep.var.labels   = "Perception of Incivility",
          omit.stat = c("rsq", "ser"),
          notes.label = "Table E. ",
          model.numbers = F,
          out = "tableE.html")

#UNCOMMENT OUT THIS LINE IF YOU WANT TO SEE RESULTS WITH JUST THE FIRST SCENARIO (Appendix F)
#civility_dat <- civility_dat %>% filter(iteration == 1)

#AMCE

amce1 <- amce(formula = uncivil ~ incivility + police + s_job + t_job + s_inparty + t_inparty + s_gender + t_gender + s_white + t_white, data = civility_dat, na.ignore = T)

plot.amce(amce1, colors = "black")
ggsave("figureX", device = "jpeg", dpi = 320, width = 9, height = 6, units = "in")

#base model
OLS1 <- civility_dat %>% lm(formula = uncivil ~ incivility + police + s_job + t_job + s_inparty + t_inparty + s_gender + t_gender + s_white + t_white)

stargazer(OLS1, type = "text",
          se = list(coeftest(OLS1,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2]), 
          covariate.labels = c("Slurs", "Threats", "Civility Policing", "Elite Speaker", 
                               "Elite Target", "Speaker In-Party", "Target In-Party", 
                               "Female Speaker", "Female Target", "White Speaker",
                               "White Target"),
          dep.var.caption  = "Dependent Variable",
          dep.var.labels   = "Perception of Incivility",
          omit.stat = c("rsq", "ser"),
          notes.label = "Table 1. ",
          model.numbers = F,
          out = "table1.html")

#Figure 2
modelcoef = summary(OLS1)$coefficients[1:length(OLS1$coefficients), 1]
modelse = as.vector(coeftest(OLS1,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2])
ylo = modelcoef - qt(.975, nrow(civility_dat))*(modelse)
yhi = modelcoef + qt(.975, nrow(civility_dat))*(modelse)
names = c("Intercept", "Slurs", "Threats", "Civility Policing", "Elite Speaker", 
                                 "Elite Target", "Speaker In-Party", "Target In-Party", 
                                 "Female Speaker", "Female Target", "White Speaker",
                                 "White Target")
names = factor(names, levels = unique(names))
hypotheses = c("Intercept", "H1 and H2", "H1 and H2", "H1 and H2", "H3a and H3b", "H3a and H3b",
               "H4a and H4b", "H4a and H4b", "H5a and H5b", "H5a and H5b", "No Hypothesis", "No Hypothesis")
dfplot = data.frame(names, modelcoef, modelse, ylo, yhi, hypotheses)
dfplot = dfplot[-c(1),]

figure2 <- ggplot(dfplot, aes(x=names, y=modelcoef, ymin=ylo, ymax=yhi)) + geom_point() +
  geom_pointrange(aes(ymin = ylo, ymax = yhi)) + theme_bw()  + coord_flip() + geom_hline(aes(x=0), lty=2, yintercept = 0) + 
  labs(title = "Figure 2: Perceptions of incivility and how they vary", 
                                     caption = "Note: Bars represent 95% confidence intervals; Corresponding regression table can be found in Appendix B",
                                     y = "Perception of incivility (low to high)", x = " ") + 
  facet_wrap( ~ hypotheses, ncol = 1, scales = "free_y", strip.position = "left")
ggsave("figure2", device = "jpeg", dpi = 320, width = 9, height = 6, units = "in")
figure2

#Figure 2, new
modelcoef = summary(OLS1)$coefficients[1:length(OLS1$coefficients), 1]
modelse = as.vector(coeftest(OLS1,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2])
ylo = modelcoef - qt(.975, nrow(civility_dat))*(modelse)
yhi = modelcoef + qt(.975, nrow(civility_dat))*(modelse)
names = c("Intercept", "Slurs", "Threats", "Civility Policing", "Elite Speaker", 
          "Elite Target", "Speaker In-Party", "Target In-Party", 
          "Female Speaker", "Female Target", "White Speaker",
          "White Target")
names = factor(names, levels = unique(names))
hypotheses = c("Intercept", "No Hypothesis", "No Hypothesis", "No Hypothesis", "No Hypothesis", "No Hypothesis",
               "H1a and H1b", "H1a and H1b", "H2a and H2b", "H2a and H2b", "No Hypothesis", "No Hypothesis")
dfplot = data.frame(names, modelcoef, modelse, ylo, yhi, hypotheses)
dfplot = dfplot[-c(1),]

figure2 <- ggplot(dfplot, aes(x=names, y=modelcoef, ymin=ylo, ymax=yhi)) + geom_point() +
  geom_pointrange(aes(ymin = ylo, ymax = yhi)) + theme_bw()  + coord_flip() + geom_hline(aes(x=0), lty=2, yintercept = 0) + 
  labs(title = "Figure 2: Perceptions of incivility and how they vary", 
       caption = "Note: Bars represent 95% confidence intervals; Corresponding regression table can be found in Appendix B",
       y = "Perception of incivility (low to high)", x = " ") + 
  facet_wrap( ~ hypotheses, ncol = 1, scales = "free_y", strip.position = "left")
ggsave("figure2", device = "jpeg", dpi = 320, width = 9, height = 6, units = "in")
figure2

#breaking main effects down by gender

table(civility_dat$gid)

ds.female <- civility_dat %>% filter(gender == "Female")
ds.male <- civility_dat %>% filter(gender == "Male")

mean(ds.female$uncivil, na.rm=TRUE)
mean(ds.male$uncivil, na.rm=TRUE)

t.test(ds.female$uncivil, ds.male$uncivil)

str(civility_dat$uncivil)

table(civility_dat$uncivil)

OLS1.1 <- civility_dat %>% filter(gender == "Male") %>% lm(formula = uncivil ~ incivility + police + s_job + t_job + s_inparty + t_inparty + s_gender + t_gender + s_race + t_race)

stargazer(OLS1.1, type = "text", se = list(coeftest(OLS1.1,  cluster = filter(civility_dat, gender == "Male")$respondent_id, vcovCL, type = 'HC1')[, 2]))

OLS1.2 <- civility_dat %>% filter(gender == "Female") %>% lm(formula = uncivil ~ incivility + police + s_job + t_job + s_inparty + t_inparty + s_ingender + t_ingender + s_race + t_race)

stargazer(OLS1.2, type = "text", se = list(coeftest(OLS1.2,  cluster = filter(civility_dat, gender == "Female")$respondent_id, vcovCL, type = 'HC1')[, 2]))

#interactions with GID = nope
OLS1.3 <- civility_dat %>% filter(gender == "Male") %>% lm(formula = uncivil ~ incivility + police + s_job + t_job + s_inparty + t_inparty + s_ingender*gid + t_ingender + s_race + t_race)

stargazer(OLS1.3, type = "text", se = list(coeftest(OLS1.3,  cluster = filter(civility_dat, gender == "Male")$respondent_id, vcovCL, type = 'HC1')[, 2]))

OLS1.4 <- civility_dat %>% filter(gender == "Female") %>% lm(formula = uncivil ~ incivility + police + s_job + t_job + s_inparty + t_inparty + s_ingender*gid + t_ingender + s_race + t_race)

stargazer(OLS1.4, type = "text", se = list(coeftest(OLS1.4,  cluster = filter(civility_dat, gender == "Female")$respondent_id, vcovCL, type = 'HC1')[, 2]))


#Interactions
# RR 
OLS2 <- civility_dat %>% lm(formula = uncivil ~ incivility + police + s_job + t_job + s_inparty + t_inparty + s_gender + t_gender + t_race + s_race*racial)

# Dominance
#nope
OLS3 <- civility_dat %>% lm(formula = uncivil ~ incivility + police + s_job + t_job + s_inparty + t_inparty + s_gender + t_gender + t_race + s_race*dominance)

# Sexism
#nope
OLS4 <- civility_dat %>% lm(formula = uncivil ~ incivility + police + s_job + t_job + s_inparty + t_inparty + t_gender + t_race + s_race + s_gender*sexism)

stargazer(OLS2, OLS3, OLS4, type = "text", 
          se = list(coeftest(OLS2,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2], 
                    coeftest(OLS3,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2], 
                    coeftest(OLS4,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2]),
          covariate.labels = c("Slurs", "Threats", "Civility Policing", "Elite Speaker", "Elite Target", 
                               "Speaker In-Party", "Target In-Party", "Female Speaker", "Hostile Sexism",
                               "Female Speaker*Sexism", "Female Target", "Black Target", "Black Speaker",
                               "Racial Resentment", "Black Speaker*RR", "SDO", "Black Speaker*SDO"),
          dep.var.caption  = "Dependent Variable",
          dep.var.labels   = "Perception of Incivility",
          omit.stat = c("rsq", "ser"),
          notes.label = "Table 2. ",
          column.labels = c("H3a Racial Resentment", "H3a SDO", "H3a Sexism"),
          model.numbers = F,
          out = "table2.html")

stargazer(OLS2, OLS3, type = "text", 
          se = list(coeftest(OLS2,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2], 
                    coeftest(OLS3,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2]),
          covariate.labels = c("Slurs", "Threats", "Civility Policing", "Elite Speaker", "Elite Target", 
                               "Speaker In-Party", "Target In-Party", "Female Speaker",
                               "Female Target", "Black Target", "Black Speaker",
                               "Racial Resentment", "Black Speaker*RR", "SDO", "Black Speaker*SDO"),
          dep.var.caption  = "Dependent Variable",
          dep.var.labels   = "Perception of Incivility",
          omit.stat = c("rsq", "ser"),
          notes.label = "Table 2. ",
          column.labels = c("H6a Racial Resentment", "H6a SDO"),
          model.numbers = F,
          out = "table2.html")

lm.figure2.1 <- glm.cluster(formula = uncivil ~ incivility + police + s_job + t_job + s_inparty + t_inparty + s_gender + t_gender + t_race + s_race*racial, cluster = "respondent_id", data = civility_dat)
figure2.1 <- interplot(m=lm.figure2.1$glm_res, var1 = "s_race", var2 = "racial", ci=.95) +
  xlab('Racial Resentment') +
  ylab('Coefficient') +
  ggtitle('Black Speaker and Racial Resentment') + geom_hline(aes(x=0), lty=2, yintercept = 0) 

#new figure 3
lm.figure2.2 <- glm.cluster(formula = uncivil ~ incivility + police + s_job + t_job + s_inparty + t_inparty + s_gender + t_gender + t_race + s_race*dominance, cluster = "respondent_id", data = civility_dat)
figure2.2 <- interplot(m=lm.figure2.2$glm_res, var1 = "s_race", var2 = "dominance", ci=.95) +
  xlab('Social Dominance Orientation') +
  ylab('Coefficient') + geom_hline(aes(x=0), lty=2, yintercept = 0)
annotate_figure(figure2.2,
                top = text_grob("Figure 3: OLS Interaction Model for Racism toward Speaker", color = "black", face = "bold", size = 14),
                bottom = text_grob("Note: Bars represent 95% confidence intervals; Corresponding regression tables can be found in Appendix B", color = "black",
                                   hjust = 1, x = 1, face = "italic", size = 10))
ggsave("figure3", device = "jpeg", dpi = 320, width = 12, height = 6, units = "in")

lm.figure2.3 <- glm.cluster(formula = uncivil ~ incivility + police + s_job + t_job + s_inparty + t_inparty + t_gender + t_race + s_race + s_gender*sexism, cluster = "respondent_id", data = civility_dat)
figure2.3 <- interplot(m=lm.figure2.3$glm_res, var1 = "s_gender", var2 = "sexism", ci=.95) +
  xlab('Hostile Sexism') +
  ylab('Coefficient') +
  ggtitle('Female Speaker and Sexism') + geom_hline(aes(x=0), lty=2, yintercept = 0)

#old figure 3
figure2 <- ggarrange(figure2.1, figure2.2, nrow = 1)
annotate_figure(figure2,
                top = text_grob("Figure 3: OLS Interaction Models for Racism toward Speaker", color = "black", face = "bold", size = 14),
                bottom = text_grob("Note: Bars represent 95% confidence intervals; Corresponding regression tables can be found in Appendix B", color = "black",
                                   hjust = 1, x = 1, face = "italic", size = 10))
ggsave("figure3", device = "jpeg", dpi = 320, width = 12, height = 6, units = "in")

# H3b
##RR
OLS5 <- civility_dat %>% lm(formula = uncivil ~ incivility + police + s_job + t_job + s_inparty + t_inparty + s_gender + t_gender + s_race + t_race*racial)

# Dominance
#nope
OLS6 <- civility_dat %>% lm(formula = uncivil ~ incivility + police + s_job + t_job + s_inparty + t_inparty + s_gender + t_gender + s_race + t_race*dominance)

# Sexism
#nope
OLS7 <- civility_dat %>% lm(formula = uncivil ~ incivility + police + s_job + t_job + s_inparty + t_inparty + s_gender + s_race + t_race + t_gender*sexism)

stargazer(OLS5, OLS6, OLS7, type = "text", 
          se = list(coeftest(OLS5,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2], 
                    coeftest(OLS6,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2], 
                    coeftest(OLS7,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2]),
          covariate.labels = c("Slurs", "Threats", "Civility Policing", "Elite Speaker", "Elite Target", 
                               "Speaker In-Party", "Target In-Party", "Female Speaker", "Female Target",
                               "Hostile Sexism", "Female Target*Sexism", "Black Speaker", "Black Target",
                               "Racial Resentment", "Black Target*RR", "SDO", "Black Target*SDO"),
          dep.var.caption  = "Dependent Variable",
          dep.var.labels   = "Perception of Incivility",
          omit.stat = c("rsq", "ser"),
          notes.label = "Table 3. ",
          column.labels = c("H3b Racial Resentment", "H3b SDO", "H3b Sexism"),
          model.numbers = F,
          out = "table3.html")

stargazer(OLS5, OLS6, type = "text", 
          se = list(coeftest(OLS5,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2], 
                    coeftest(OLS6,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2]),
          covariate.labels = c("Slurs", "Threats", "Civility Policing", "Elite Speaker", "Elite Target", 
                               "Speaker In-Party", "Target In-Party", "Female Speaker", "Female Target",
                               "Black Speaker", "Black Target",
                               "Racial Resentment", "Black Target*RR", "SDO", "Black Target*SDO"),
          dep.var.caption  = "Dependent Variable",
          dep.var.labels   = "Perception of Incivility",
          omit.stat = c("rsq", "ser"),
          notes.label = "Table 3. ",
          column.labels = c("H6b Racial Resentment", "H6b SDO"),
          model.numbers = F,
          out = "table3.html")

lm.figure3.1 <- glm.cluster(formula = uncivil ~ incivility + police + s_job + t_job + s_inparty + t_inparty + s_gender + t_gender + s_race + t_race*racial, cluster = "respondent_id", data = civility_dat)
figure3.1 <- interplot(m=lm.figure3.1$glm_res, var1 = "t_race", var2 = "racial", ci=.95) +
  xlab('Racial Resentment') +
  ylab('Coefficient') +
  ggtitle('Black Target and Racial Resentment') + geom_hline(aes(x=0), lty=2, yintercept = 0)

#new figure 4
lm.figure3.2 <- glm.cluster(formula = uncivil ~ incivility + police + s_job + t_job + s_inparty + t_inparty + s_gender + t_gender + s_race + t_race*dominance, cluster = "respondent_id", data = civility_dat)
figure3.2 <- interplot(m=lm.figure3.2$glm_res, var1 = "t_race", var2 = "dominance", ci=.95) +
  xlab('Social Dominance Orientation') +
  ylab('Coefficient') + geom_hline(aes(x=0), lty=2, yintercept = 0)
annotate_figure(figure3.2,
                top = text_grob("Figure 4: OLS Interaction Model for Racism toward Target", color = "black", face = "bold", size = 14),
                bottom = text_grob("Note: Bars represent 95% confidence intervals; Corresponding regression tables can be found in Appendix B", color = "black",
                                   hjust = 1, x = 1, face = "italic", size = 10))
ggsave("figure4", device = "jpeg", dpi = 320, width = 12, height = 6, units = "in")


lm.figure3.3 <- glm.cluster(formula = uncivil ~ incivility + police + s_job + t_job + s_inparty + t_inparty + s_gender + s_race + t_race + t_gender*sexism, cluster = "respondent_id", data = civility_dat)
figure3.3 <- interplot(m=lm.figure3.3$glm_res, var1 = "t_gender", var2 = "sexism", ci=.95) +
  xlab('Hostile Sexism') +
  ylab('Coefficient') +
  ggtitle('Female Target and Sexism') + geom_hline(aes(x=0), lty=2, yintercept = 0)

#old figure 4
figure3 <- ggarrange(figure3.1, figure3.2, nrow = 1)
annotate_figure(figure3,
                top = text_grob("Figure 4: OLS Interaction Models for Racism toward Target", color = "black", face = "bold", size = 14),
                bottom = text_grob("Note: Bars represent 95% confidence intervals; Corresponding regression tables can be found in Appendix B", color = "black",
                                   hjust = 1, x = 1, face = "italic", size = 10))
ggsave("figure4", device = "jpeg", dpi = 320, width = 12, height = 6, units = "in")

# H4a
#nope
OLS8 <- civility_dat %>% 
  lm(formula = uncivil ~ incivility + police + s_job + t_job + s_inparty + t_inparty + s_ingender + t_ingender + s_race + t_race + justification)

# H4b 
# SJT
#wrong direction, but p < 0.10
OLS9 <- civility_dat %>% 
  lm(formula = uncivil ~ incivility + police + s_job + t_job + s_inparty + t_inparty + s_ingender + t_ingender + t_race + s_race*justification)

#nope
OLS10 <- civility_dat %>% 
  lm(formula = uncivil ~ incivility + police + s_job + t_job + s_inparty + t_inparty + s_race + t_race + s_gender*justification)


stargazer(OLS8, OLS9, OLS10, type = "text", 
          se = list(coeftest(OLS8,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2], 
                    coeftest(OLS9,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2], 
                    coeftest(OLS10,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2]),
          covariate.labels = c("Slurs", "Threats", "Civility Policing", "Speaker Elite", "Target Elite",
                               "Speaker In-Party", "Target In-Party", "Speaker In-Gender", "Target In-Gender",
                               "Black Speaker", "Target Black", "Female Speaker", "System Justification",
                               "Black Speaker:SJ", "Female Speaker:SJ"),
          dep.var.caption  = "Dependent Variable",
          dep.var.labels   = "Perception of Incivility",
          omit.stat = c("rsq", "ser"),
          notes.label = "Table 4. ",
          column.labels = c("H4a", "H4b Race", "H4b Gender"),
          model.numbers = F,
          out = "table4.html")


#Pre-registered dyadic analyses
# Bivariate 
civility_dat %>% 
  lm(formula = uncivil ~ incivility) %>% 
  stargazer(type = "text", se = list(coeftest(.,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2]))

# 'Base'
civility_dat %>% 
  lm(formula = uncivil ~ incivility + police) %>%
  stargazer(type = "text", se = list(coeftest(.,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2]),
            covariate.labels = c("Slurs", "Threats", "Civility Policing"),
            dep.var.caption  = "Dependent Variable",
            dep.var.labels   = "Perception of Incivility",
            omit.stat = c("rsq", "ser"),
            notes.label = "Table C1. ",
            out = "tableC1.html")

# Hypotheses
# H1a 
#nope

civility_dat$s_job <- ordered(civility_dat$s_job, levels = c("non-elite", "elite"))

OLS1 <- civility_dat %>% 
  lm(formula = uncivil ~ incivility + police + s_job)

# H1B
#nope
OLS2 <- civility_dat %>% 
  lm(formula = uncivil ~ incivility + police + h1b)

stargazer(OLSC1, OLSC2, type = "text", 
          se = list(coeftest(OLSC1,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2], coeftest(OLSC2,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2]),
          covariate.labels = c("Slurs", "Threats", "Civility Policing", "Speaker Elite", "Speaker Non Elite x Target Elite"),
          dep.var.caption  = "Dependent Variable",
          dep.var.labels   = "Perception of Incivility",
          omit.stat = c("rsq", "ser"),
          notes.label = "Table C2. ",
          column.labels = c("H1a", "H1b"),
          model.numbers = F,
          out = "tableC2.html")



# H2 a
#nope
OLS3 <- civility_dat %>% 
  lm(formula = uncivil ~ incivility + police + h2a_party)

OLS4 <- civility_dat %>% 
  lm(formula = uncivil ~ incivility + police + h2a_gender)

OLS5 <- civility_dat %>% 
  lm(formula = uncivil ~ incivility + police + h2a_race)

stargazer(OLS3, OLS4, OLS5, type = "text", 
          se = list(coeftest(OLS3,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2], 
                    coeftest(OLS4,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2], 
                    coeftest(OLS5,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2]),
          covariate.labels = c("Slurs", "Threats", "Civility Policing", "Speaker Out-Party x Target In-Party", "Speaker Out-Gender x Target In-Gender", "Speaker Out-Race x Target In-Race"),
          dep.var.caption  = "Dependent Variable",
          dep.var.labels   = "Perception of Incivility",
          omit.stat = c("rsq", "ser"),
          notes.label = "Table 3. ",
          column.labels = c("H2a Partisanship", "H2a Gender", "H2a Race"),
          model.numbers = F,
          out = "table3.html")

#huge effect if you just look at whether the target is in-party or not.
civility_dat %>% 
  lm(formula = uncivil ~ incivility + police + t_inparty)  %>% 
  stargazer(type = "text", se = list(coeftest(.,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2]))


# H2 b
#YES!
OLS6 <- civility_dat %>% 
  lm(formula = uncivil ~ incivility + police + h2b_party)

#Yes, p < 0.10, but wrong direction
OLS7 <- civility_dat %>% 
  lm(formula = uncivil ~ incivility + police + h2b_gender)

#No
OLS8 <- civility_dat %>% 
  lm(formula = uncivil ~ incivility + police + h2b_race)

stargazer(OLS6, OLS7, OLS8, type = "text", 
          se = list(coeftest(OLS6,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2], 
                    coeftest(OLS7,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2], 
                    coeftest(OLS8,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2]),
          covariate.labels = c("Slurs", "Threats", "Civility Policing", "Speaker Out-Party x Target In-Party", "Speaker Out-Gender x Target In-Gender", "Speaker Out-Race x Target In-Race"),
          dep.var.caption  = "Dependent Variable",
          dep.var.labels   = "Perception of Incivility",
          omit.stat = c("rsq", "ser"),
          notes.label = "Table 4. ",
          column.labels = c("H2b Partisanship", "H2b Gender", "H2b Race"),
          model.numbers = F,
          out = "table4.html")

# H2 c
#yes, p < 0.10
OLS9 <- civility_dat %>% 
  lm(formula = uncivil ~ incivility + police + h2c_party)

#yes, p < 0.10, but wrong direction
OLS10 <- civility_dat %>% 
  lm(formula = uncivil ~ incivility + police + h2c_gender)

#yes, p < 0.05, but wrong direction
OLS11 <- civility_dat %>% 
  lm(formula = uncivil ~ incivility + police + h2c_race)

stargazer(OLS9, OLS10, OLS11, type = "text", 
          se = list(coeftest(OLS9,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2], 
                    coeftest(OLS10,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2], 
                    coeftest(OLS11,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2]),
          covariate.labels = c("Slurs", "Threats", "Civility Policing", "Speaker In-Party x Target Out-Party", "Speaker In-Gender x Target Out-Gender", "Speaker In-Race x Target Out-Race"),
          dep.var.caption  = "Dependent Variable",
          dep.var.labels   = "Perception of Incivility",
          omit.stat = c("rsq", "ser"),
          notes.label = "Table 5. ",
          column.labels = c("H2c Partisanship", "H2c Gender", "H2c Race"),
          model.numbers = F,
          out = "table5.html")

# H2 d
#yes, p < 0.10, but wrong direction
OLS12 <- civility_dat %>% 
  lm(formula = uncivil ~ incivility + police + h2d_party)

#nope
OLS13 <- civility_dat %>% 
  lm(formula = uncivil ~ incivility + police + h2d_gender)

#yes, p < 0.05
OLS14 <- civility_dat %>% 
  lm(formula = uncivil ~ incivility + police + h2d_race)

stargazer(OLS12, OLS13, OLS14, type = "text", 
          se = list(coeftest(OLS12,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2], 
                    coeftest(OLS13,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2], 
                    coeftest(OLS14,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2]),
          covariate.labels = c("Slurs", "Threats", "Civility Policing", "Speaker In-Party x Target In-Party", "Speaker In-Gender x Target In-Gender", "Speaker In-Race x Target In-Race"),
          dep.var.caption  = "Dependent Variable",
          dep.var.labels   = "Perception of Incivility",
          omit.stat = c("rsq", "ser"),
          notes.label = "Table 6. ",
          column.labels = c("H2d Partisanship", "H2d Gender", "H2d Race"),
          model.numbers = F,
          out = "table6.html")

# H3a
#nope
# RR 
OLS15 <- civility_dat %>% 
  lm(formula = uncivil ~ incivility + police + s_race + s_race*racial)

# Dominance
#nope
OLS16 <- civility_dat %>% 
  lm(formula = uncivil ~ incivility + police + s_race + s_race*dominance)

# Sexism
#nope
OLS17 <- civility_dat %>% 
  filter(gender == "Male") %>% lm(formula = uncivil ~ incivility + police + s_gender + s_gender*sexism)

stargazer(OLS15, OLS16, OLS17, type = "text", 
          se = list(coeftest(OLS15,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2], 
                    coeftest(OLS16,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2], 
                    coeftest(OLS17,  cluster = filter(civility_dat, gender == "Male")$respondent_id, vcovCL, type = 'HC1')[, 2]),
          covariate.labels = c("Slurs", "Threats", "Civility Policing", "Black Speaker", "Racial Resentment",
                               "Black Speaker:Racial Resentment", "SDO", "Black Speaker:SDO", "Female Speaker",
                               "Hostile Sexism", "Female Speaker:Hostile Sexism"),
          dep.var.caption  = "Dependent Variable",
          dep.var.labels   = "Perception of Incivility",
          omit.stat = c("rsq", "ser"),
          notes.label = "Table 7. ",
          column.labels = c("H3a Racial Resentment", "H3a SDO", "H3a Sexism"),
          model.numbers = F,
          out = "table7.html")


# H3b
# RR
#yes, p < 0.10
OLS18 <- civility_dat %>% 
  lm(formula = uncivil ~ incivility + police + t_race + t_race*racial)

# Dominance
#yes, p < 0.05
OLS19 <- civility_dat %>% 
  lm(formula = uncivil ~ incivility + police + t_race + t_race*dominance)

# Sexism
#nope
OLS20 <- civility_dat %>% 
  filter(gender == "Male") %>% lm(formula = uncivil ~ incivility + police + t_gender + t_gender*sexism)

stargazer(OLS18, OLS19, OLS20, type = "text", 
          se = list(coeftest(OLS18,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2], 
                    coeftest(OLS19,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2], 
                    coeftest(OLS20,  cluster = filter(civility_dat, gender == "Male")$respondent_id, vcovCL, type = 'HC1')[, 2]),
          covariate.labels = c("Slurs", "Threats", "Civility Policing", "Black Target", "Racial Resentment",
                               "Black Target:Racial Resentment", "SDO", "Black Target:SDO", "Female Target",
                               "Hostile Sexism", "Female Target:Hostile Sexism"),
          dep.var.caption  = "Dependent Variable",
          dep.var.labels   = "Perception of Incivility",
          omit.stat = c("rsq", "ser"),
          notes.label = "Table 8. ",
          column.labels = c("H3b Racial Resentment", "H3b SDO", "H3b Sexism"),
          model.numbers = F,
          out = "table8.html")

# H4a
#nope
OLS21 <- civility_dat %>% 
  lm(formula = uncivil ~ incivility + police + justification)

# H4b 
# SJT
#wrong direction, but p < 0.10
OLS22 <- civility_dat %>% 
  lm(formula = uncivil ~ incivility + police + s_race + s_race*justification)

#nope
OLS23 <- civility_dat %>% 
  lm(formula = uncivil ~ incivility + police + s_gender + s_gender*justification)


stargazer(OLS21, OLS22, OLS23, type = "text", 
          se = list(coeftest(OLS21,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2], 
                    coeftest(OLS22,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2], 
                    coeftest(OLS23,  cluster = civility_dat$respondent_id, vcovCL, type = 'HC1')[, 2]),
          covariate.labels = c("Slurs", "Threats", "Civility Policing", "Black Speaker", "Female Speaker",
                               "System Justification", "Black Speaker:SJ", "Female Speaker:SJ"),
          dep.var.caption  = "Dependent Variable",
          dep.var.labels   = "Perception of Incivility",
          omit.stat = c("rsq", "ser"),
          notes.label = "Table 9. ",
          column.labels = c("H4a", "H4b Race", "H4b Gender"),
          model.numbers = F,
          out = "table9.html")
