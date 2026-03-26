load("experiment_data.RData")

library(sandwich); library(stargazer); library(tidyverse); library(xtable)

####################################################
# Table A-12 (Turk Descriptive Principles Results) # 
####################################################
# All
turk.all.plain <- round(prop.table(table(turk$plain.meaning)), 2)
turk.all.intent <- round(prop.table(table(turk$intent)), 2)
turk.all.adopted <- round(prop.table(table(turk$meaning.at.adoption)), 2)
turk.all.precedent <- round(prop.table(table(turk$precedent)), 2)

turk.all.consequences <- round(prop.table(table(turk$consequence)), 2)
turk.all.countries <- round(prop.table(table(turk$other.countries)), 2)
turk.all.opinion <- round(prop.table(table(turk$public.opinion)), 2)

turk.all.security <- round(prop.table(table(turk$national.security)), 2)
turk.all.political <- round(prop.table(table(turk$political.activity)), 2)
turk.all.deference <- round(prop.table(table(turk$deference.legislatures)), 2)

# Liberals
turk_liberals <- turk[which(turk$liberal == 1),]

turk.liberals.plain <- round(prop.table(table(turk_liberals$plain.meaning)), 2)
turk.liberals.intent <- round(prop.table(table(turk_liberals$intent)), 2)
turk.liberals.adopted <- round(prop.table(table(turk_liberals$meaning.at.adoption)), 2)
turk.liberals.precedent <- round(prop.table(table(turk_liberals$precedent)), 2)

turk.liberals.consequences <- round(prop.table(table(turk_liberals$consequence)), 2)
turk.liberals.countries <- round(prop.table(table(turk_liberals$other.countries)), 2)
turk.liberals.opinion <- round(prop.table(table(turk_liberals$public.opinion)), 2)

turk.liberals.security <- round(prop.table(table(turk_liberals$national.security)), 2)
turk.liberals.political <- round(prop.table(table(turk_liberals$political.activity)), 2)
turk.liberals.deference <- round(prop.table(table(turk_liberals$deference.legislatures)), 2)

# Moderates
turk_moderates <- turk[which(turk$moderate == 1),]

turk.moderates.plain <- round(prop.table(table(turk_moderates$plain.meaning)), 2)
turk.moderates.intent <- round(prop.table(table(turk_moderates$intent)), 2)
turk.moderates.adopted <- round(prop.table(table(turk_moderates$meaning.at.adoption)), 2)
turk.moderates.precedent <- round(prop.table(table(turk_moderates$precedent)), 2)

turk.moderates.consequences <- round(prop.table(table(turk_moderates$consequence)), 2)
turk.moderates.countries <- round(prop.table(table(turk_moderates$other.countries)), 2)
turk.moderates.opinion <- round(prop.table(table(turk_moderates$public.opinion)), 2)

turk.moderates.security <- round(prop.table(table(turk_moderates$national.security)), 2)
turk.moderates.political <- round(prop.table(table(turk_moderates$political.activity)), 2)
turk.moderates.deference <- round(prop.table(table(turk_moderates$deference.legislatures)), 2)

# Conservatives
turk_conservatives <- turk[which(turk$conservative == 1),]

turk.conservatives.plain <- round(prop.table(table(turk_conservatives$plain.meaning)), 2)
turk.conservatives.intent <- round(prop.table(table(turk_conservatives$intent)), 2)
turk.conservatives.adopted <- round(prop.table(table(turk_conservatives$meaning.at.adoption)), 2)
turk.conservatives.precedent <- round(prop.table(table(turk_conservatives$precedent)), 2)

turk.conservatives.consequences <- round(prop.table(table(turk_conservatives$consequence)), 2)
turk.conservatives.countries <- round(prop.table(table(turk_conservatives$other.countries)), 2)
turk.conservatives.opinion <- round(prop.table(table(turk_conservatives$public.opinion)), 2)

turk.conservatives.security <- round(prop.table(table(turk_conservatives$national.security)), 2)
turk.conservatives.political <- round(prop.table(table(turk_conservatives$political.activity)), 2)
turk.conservatives.deference <- round(prop.table(table(turk_conservatives$deference.legislatures)), 2)

ta12.plain.meaning <- c(turk.all.plain[2], turk.liberals.plain[2], turk.moderates.plain[2], turk.conservatives.plain[2])
ta12.original.intent <- c(turk.all.intent[2], turk.liberals.intent[2], turk.moderates.intent[2], turk.conservatives.intent[2])
ta12.opinion.when.adopted <- c(turk.all.adopted[2], turk.liberals.adopted[2], turk.moderates.adopted[2], turk.conservatives.adopted[2])
ta12.precedent <- c(turk.all.precedent[2], turk.liberals.precedent[2], turk.moderates.precedent[2], turk.conservatives.precedent[2])
ta12.average.traditional <- apply(rbind(ta12.plain.meaning, ta12.original.intent, ta12.opinion.when.adopted, ta12.precedent), 2, mean)

ta12.plain.meaning <- paste(as.character(round(ta12.plain.meaning*100,0)),"%",sep="")
ta12.original.intent <- paste(as.character(round(ta12.original.intent*100,0)),"%",sep="")
ta12.opinion.when.adopted <- paste(as.character(round(ta12.opinion.when.adopted*100,0)),"%",sep="")
ta12.precedent <- paste(as.character(round(ta12.precedent*100,0)),"%",sep="")
ta12.average.traditional <- paste(as.character(round(ta12.average.traditional*100,0)),"%",sep="") 

ta12.consequences <- c(turk.all.consequences[2], turk.liberals.consequences[2], turk.moderates.consequences[2], turk.conservatives.consequences[2])
ta12.other.countries <- c(turk.all.countries[2], turk.liberals.countries[2], turk.moderates.countries[2], turk.conservatives.countries[2])
ta12.public.opinion <- c(turk.all.opinion[2], turk.liberals.opinion[2], turk.moderates.opinion[2], turk.conservatives.opinion[2])
ta12.average.non.traditional <- apply(rbind(ta12.consequences, ta12.other.countries, ta12.public.opinion), 2, mean)

ta12.consequences <- paste(as.character(round(ta12.consequences*100,0)),"%",sep="")
ta12.other.countries <- paste(as.character(round(ta12.other.countries*100,0)),"%",sep="")
ta12.public.opinion <- paste(as.character(round(ta12.public.opinion*100,0)),"%",sep="")
ta12.average.non.traditional <- paste(as.character(round(ta12.average.non.traditional*100,0)),"%",sep="") 

ta12.strong.reason <- c(turk.all.security[2], turk.liberals.security[2], turk.moderates.security[2], turk.conservatives.security[2])
ta12.political.activity <- c(turk.all.political[2], turk.liberals.political[2], turk.moderates.political[2], turk.conservatives.political[2])
ta12.deference <- c(turk.all.deference[2], turk.liberals.deference[2], turk.moderates.deference[2], turk.conservatives.deference[2])

ta12.strong.reason <- paste(as.character(round(ta12.strong.reason*100,0)),"%",sep="")
ta12.political.activity <- paste(as.character(round(ta12.political.activity*100,0)),"%",sep="")
ta12.deference <- paste(as.character(round(ta12.deference*100,0)),"%",sep="")

ta12.dataframe <- data.frame(rbind(ta12.average.traditional, ta12.plain.meaning, ta12.original.intent, ta12.opinion.when.adopted, ta12.precedent,
                                  ta12.average.non.traditional, ta12.consequences, ta12.other.countries, ta12.public.opinion,
                                  ta12.strong.reason, ta12.political.activity, ta12.deference),
                            row.names = c("Traditional principles","Plain meaning","Original intent","Opinion when adopted","Precedent",
                                          "Non-traditional principles","Consequences","Other countries","Public opinion",
                                          "Strong reason","Political activity","Legislative deference"))
colnames(ta12.dataframe) <- c("All","Liberal","Moderate","Conservative")
xtable(ta12.dataframe)

###############################
# Figure A-5 (Knowledge Plot) # 
###############################
# Creating a 4 point measure of evaluation of public opinion principle
turk_pubop <- turk %>%
  mutate(public.opinion03 = case_when(P10 == "Very important" ~ 3,
                                      P10 == "Somewhat important" ~ 2,
                                      P10 == "A little important" ~ 1,
                                      P10 == "Not important at all" ~ 0),
         ideo3 = case_when(moderate == 1 ~ "Moderate",
                           liberal == 1 ~ "Liberal",
                           conservative == 1 ~ "Conservative"),
         Principle = "Public Opinion",
         y = public.opinion03 / 3)

# Creating a 4 point measure of evaluation of other countries principle
turk_othercountries <- turk %>%
  mutate(other.countries03 = case_when(P11 == "Very important" ~ 3,
                                       P11 == "Somewhat important" ~ 2,
                                       P11 == "A little important" ~ 1,
                                       P11 == "Not important at all" ~ 0),
         ideo3 = case_when(moderate == 1 ~ "Moderate",
                           liberal == 1 ~ "Liberal",
                           conservative == 1 ~ "Conservative"),
         Principle = "Other Countries",
         y = other.countries03 / 3)

# Plotting results as a function of knowledge and ideology for other countries and public opinion
pdf("figure-a-5.pdf")
turk_pubop %>%
  bind_rows(turk_othercountries) %>%
  filter(ideo3 != "Moderate") %>% 
  ggplot(aes(y=y,x=knowledge,color=ideo3,linetype = ideo3)) +
  geom_smooth(method="lm") +
  facet_wrap(~Principle) +
  ylab("Average Support for Principle") +
  xlab("Court Knowledge") +
  guides(color=guide_legend(title="Ideology"),
         linetype = guide_legend(title = "Ideology")) + theme_bw() +
  scale_color_manual(values=c("Red","Blue")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
dev.off()

###############################################
# Table A-13 (Sample Descriptive Information) # 
###############################################
round(prop.table(table(turk$gender)), 3)
round(prop.table(table(turk$race)), 3)
round(prop.table(table(turk$pid3)), 3)
round(prop.table(table(turk$ideology)), 3)

#################################################################
# Table A-14 (Descriptive Information, Consequences Experiment) # 
#################################################################
# Subset to those randomized into the consequences experiment
turk_experiment_2 <- turk[(turk$ExperimentalCondition == 2),]

round(prop.table(table(turk_experiment_2$gender)), 3)
round(prop.table(table(turk_experiment_2$race)), 3)
round(prop.table(table(turk_experiment_2$pid3)), 3)
round(prop.table(table(turk_experiment_2$ideology)), 3)

###################################
# Table A-15 (Regression Results) # 
###################################
# Base OLS regression, no controls, just respondents who were randomized into the Consequences experiment (ExperimentalCondition == 2)
fit.OLS <- lm(outcome ~ cueStrength, data=filter(turk,ExperimentalCondition==2))
vcov.OLS <- vcovHC(fit.OLS, type = "HC0")
# OLS regression with controls
fit.OLS.controls <- lm(outcome ~ cueStrength + liberal*I(ReasoningCondition==2) + white + female + factor(principles.sum.5) + knowledge, 
                       data = filter(turk, ExperimentalCondition==2))
vcov.OLS.controls <- vcovHC(fit.OLS.controls, type = "HC0")
# Base logistic regression, no controls
fit.logit <- glm(outcome ~ cueStrength, data= filter(turk, ExperimentalCondition==2), family = "binomial")
vcov.logit <- vcovHC(fit.logit, type = "HC0")
# Logistic regression with controls
fit.logit.controls <- glm(outcome ~ cueStrength + liberal*I(ReasoningCondition==2) + white + female + factor(principles.sum.5) + knowledge,
                          data = filter(turk, ExperimentalCondition==2), family = "binomial")
vcov.logit.controls <- vcovHC(fit.logit.controls, type = "HC0")

stargazer(fit.OLS, fit.OLS.controls, fit.logit, fit.logit.controls,
          se = list(c(sqrt(diag(vcov.OLS))), sqrt(diag(vcov.OLS.controls)),
                    sqrt(diag(vcov.logit)), sqrt(diag(vcov.logit.controls))),
          style = "AJPS",
          digits = 2,
          keep.stat = c("n", "adj.rsq", "ll"),
          title = "Support for Court Ruling as a Function of Judicial Principles",
          dep.var.labels = "Support",
          covariate.labels = c("Mixed Importance", "Neither Important", "No Justification Given",
                               "Liberal Respondent",
                               "Town of Greece",
                               "White Respondent",
                               "Female Respondent",
                               "One Principle Important",
                               "Two Principles Important",
                               "Three Principles Important",
                               "Four Principles Important",
                               "Five Principles Important",
                               "Knowledge of the Court",
                               "Liberal $\\times$ Town of Greece"),
          #keep = c("Constant", "cueStrength"),
          label = "experiment",
          add.lines = list(c("Respondent-Level Covariates", "", "\\checkmark", "", "\\checkmark")))


##########################################################
# Table A-16 (Regression Results, Pooled Mixed/Neither) # 
##########################################################
# Creating a "pooled treatment" indicator that combines the Mixed Importance and Neither Important treatments
turk <- turk %>%
  mutate(treatment.pool = case_when(cueStrength == "Both Important" ~ "Both Important",
                                    cueStrength == "Mixed Importance" ~ "Mixed/Neither Important",
                                    cueStrength == "Neither Important" ~ "Mixed/Neither Important",
                                    cueStrength == "No Justification Given" ~ "No Justification Given",
                                    TRUE ~ "Experiment 1")) 
# Base OLS regression, pooled Mixed/Neither, no controls, just respondents who were randomized into the Consequences experiment (ExperimentalCondition == 2)
fit.OLS.pool <- lm(outcome ~ treatment.pool, data = filter(turk, ExperimentalCondition==2))
vcov.OLS.pool <- vcovHC(fit.OLS.pool, type = "HC0")
# OLS regression, pooled Mixed/Neither, with controls
fit.OLS.controls.pool <- lm(outcome ~ treatment.pool + liberal*I(ReasoningCondition==2) + white + female + factor(principles.sum.5) + knowledge,
                            data = filter(turk, ExperimentalCondition==2))
vcov.OLS.controls.pool <- vcovHC(fit.OLS.controls.pool, type = "HC0")
# Base logit regression, pooled Mixed/Neither, no controls
fit.logit.pool <- glm(outcome ~ treatment.pool, data = filter(turk, ExperimentalCondition==2), family = "binomial")
vcov.logit.pool <- vcovHC(fit.logit.pool, type = "HC0")
# Logit regression, pooled Mixed/Neither, with controls
fit.logit.controls.pool <- glm(outcome ~ treatment.pool + liberal*I(ReasoningCondition==2) + white + female + factor(principles.sum.5) + knowledge, 
                               data=filter(turk, ExperimentalCondition==2), family = "binomial")
vcov.logit.controls.pool <- vcovHC(fit.logit.controls.pool, type = "HC0")

stargazer(fit.OLS.pool, fit.OLS.controls.pool, fit.logit.pool, fit.logit.controls.pool,
          se = list(c(sqrt(diag(vcov.OLS.pool))), sqrt(diag(vcov.OLS.controls.pool)),
                    sqrt(diag(vcov.logit.pool)), sqrt(diag(vcov.logit.controls.pool))),
          style = "AJPS",
          digits = 2,
          keep.stat = c("n", "adj.rsq", "ll"),
          title = "Support for Court Ruling as a Function of Judicial Principles: Pooling Neither and Mixed",
          dep.var.labels = "Support",
          covariate.labels = c("Mixed/Neither Important", "No Justification Given",
                               "Liberal Respondent",
                               "Town of Greece",
                               "White Respondent",
                               "Female Respondent",
                               "One Principle Important",
                               "Two Principles Important",
                               "Three Principles Important",
                               "Four Principles Important",
                               "Five Principles Important",
                               "Knowledge of the Court",
                               "Liberal $\\times$ Town of Greece"),
          #          keep = c("Constant", "treatment.pool"),
          label = "experiment.pool",
          add.lines = list(c("Respondent-Level Covariates", "", "\\checkmark", "", "\\checkmark")))


#################################################
# Table A-17 (Regression Results, Reasoning DV) # 
#################################################
# outcome2 = binary support for the reasoning the Court used to come to its decision in the case
# Base OLS regression, no controls, binary reasoning DV, just respondents who were randomized into the Consequences experiment (ExperimentalCondition == 2) 
fit_outcome2 <- lm(outcome2 ~ cueStrength, data = filter(turk, ExperimentalCondition == 2))
vcov_outcome2 <- vcovHC(fit_outcome2, type = "HC0")
# OLS regression with controls, binary reasoning DV
outcome2_controls <- lm(outcome2 ~ cueStrength + liberal*I(ReasoningCondition==2) + white + female + factor(principles.sum.5) + knowledge,
                        data = filter(turk, ExperimentalCondition==2))
vcov_outcome2_controls <- vcovHC(outcome2_controls, type = "HC0")

stargazer(fit_outcome2, outcome2_controls,
          se = list(c(sqrt(diag(vcov_outcome2))), sqrt(diag(vcov_outcome2_controls))),
          #type = "text",
          style = "AJPS",
          digits = 2,
          keep.stat = c("n", "adj.rsq", "ll"),
          title = "Support for Reasoning as a Function of Judicial Principles",
          dep.var.labels = "Support Reasoning",
          covariate.labels = c("Mixed Importance", "Neither Important",
                               "Liberal Respondent",
                               "Town of Greece",
                               "White Respondent",
                               "Female Respondent",
                               "One Principle Important",
                               "Two Principles Important",
                               "Three Principles Important",
                               "Four Principles Important",
                               "Five Principles Important",
                               "Knowledge of the Court",
                               "Liberal $\\times$ Town of Greece"),
          #keep = c("Constant", "cueStrength"),
          label = "reasoning",
          add.lines = list(c("Respondent-Level Covariates", "", "\\checkmark")))

##################################################
# Table A-18 (Regression Results, Legitimacy DV) # 
##################################################
# Base OLS regression, no controls, legitimacy DV, just respondents who were randomized into the Consequences experiment (ExperimentalCondition == 2)
fit_legit <- lm(legit ~ cueStrength, data = filter(turk, ExperimentalCondition == 2))
vcov_legit <- vcovHC(fit_legit, type = "HC0")
# OLS regression with controls, legitimacy DV
legit_controls <- lm(legit ~ cueStrength + liberal*I(ReasoningCondition==2) + white + female + factor(principles.sum.5) + knowledge,
                     data = filter(turk, ExperimentalCondition==2))
vcov_legit_controls <- vcovHC(legit_controls, type = "HC0")

stargazer(fit_legit, legit_controls,
          se = list(c(sqrt(diag(vcov_legit))), sqrt(diag(vcov_legit_controls))),
          style = "AJPS",
          digits = 2,
          keep.stat = c("n", "adj.rsq", "ll"),
          title = "Legitimacy as a Function of Judicial Principles",
          dep.var.labels = "Legitimacy",
          covariate.labels = c("Mixed Importance", "Neither Important", "No Justification Given",
                               "Liberal Respondent",
                               "Town of Greece",
                               "White Respondent",
                               "Female Respondent",
                               "One Principle Important",
                               "Two Principles Important",
                               "Three Principles Important",
                               "Four Principles Important",
                               "Five Principles Important",
                               "Knowledge of the Court",
                               "Liberal $\\times$ Town of Greece"),
          #keep = c("Constant", "cueStrength"),
          label = "legit",
          add.lines = list(c("Respondent-Level Covariates", "", "\\checkmark")))
