library(texreg)
library(tidyverse)
setwd('~/Dropbox/crime')

crimeData <- read_csv('data/crimeData_215.csv')
crimeData <- crimeData %>% mutate(yr_factor = as.factor(yr))

## worry ~ covariates, table 1 in paper
lm_worry <- lm(worryScale ~ crime_victimization + chgCrimeRate + white + male + incomeLt20 + income20t30 + income30t50 + 
                 income50t75 + senior + under30 + someCollege + baDegree + postGrad + rural + suburban, data = crimeData)

texreg(
  l = lm_worry,
  scalebox = .8,
  use.packages = FALSE,
  include.aic = FALSE,
  include.bic = FALSE,
  include.adjrs = TRUE,
  include.loglik = FALSE,
  include.deviance = FALSE,
  float.pos = "t",
  custom.coef.names = c("Intercept", 'Crime Victimization', '$\\Delta$ Crime Rate', 'White', 'Male', "Less than \\$20,000", "\\$20,00 to \\$29,999", "\\$30,000 to \\$49,999", "\\$50,000 to \\$74,999", "Senior", "Under 30", "Some College", "College Degree", "Post graduate degree", 'Rural', 'Suburban'),
  #  
  custom.model.names = c("Crime Anxiety"),
  dcolumn = T,
  reorder.coef = c(2:16, 1), 
  groups = list(Income = c(5:8), Age = c(9:10), Education = c(11:13), Urban=c(14,15))) #,
  # file = "~/Dropbox/crime/paper/tables/worryReg7.tex",
  # label = c("t:worryReg"),
  # caption = "Correlates of individual anxiety about crime. Coefficients are from OLS regression where the dependent variable is Crime Anxiety, 
  # a five-question scaled measure capturing a respodnents' level of worry about specific types of crime.")


# presapp ~ measure, table1 / figure 1 in paper
# worry model
presapp_worry <- glm(presapp ~ copartisan + independent + worryScale + crime_victimization + chgCrimeRate + white + male +  
                        incomeLt20 + income20t30 + income30t50 + income50t75 + senior + under30 + someCollege + baDegree + 
                        postGrad + rural + suburban + yr_factor, data = crimeData, family = "binomial")

# retrospective model
presapp_us <- glm(presapp ~ copartisan + independent + crime_us_yr_ago + crime_victimization + chgCrimeRate + white + male + 
                        incomeLt20 + income20t30 + income30t50 + income50t75 + senior + under30 + someCollege + baDegree + 
                        postGrad + rural + suburban + yr_factor, data = crimeData, family = "binomial")


texreg(
  l = list(presapp_worry, presapp_us),
  omit.coef = "yr_factor|state",
  scalebox = .9,
  use.packages = FALSE,
  include.aic = FALSE,
  include.bic = FALSE,
  include.adjrs = TRUE,
  include.loglik = FALSE,
  include.deviance = FALSE,
  float.pos = "h",
  custom.coef.names = c("Intercept", "Co-partisan", "Independent", "Crime Anxiety", 'Crime Victimization', '$\\Delta$ Crime Rate', 
                        'White', "Male", "Less than \\$20,000", "\\$20,00 to \\$29,999", "\\$30,000 to \\$49,999", "\\$50,000 to \\$74,999", 
                        "Senior", "Under 30", "Some College", "College Degree", "Post graduate degree", 'Rural', 'Suburban', "Retrospective"),
  #  
  custom.model.names = c('(1)', '(2)'),
  custom.header = list('Presidential Approval' = 1:2),
  dcolumn = T,
  reorder.coef = c(4, 20, 5:6, 2:3, 7:17, 19, 18, 1),
  groups = list(Income=c(9:12),Age=c(13,14), Education=c(15:17), Urban = c(18,19))) #,
  # file = "~/Dropbox/crime/paper/tables/presapp7.tex",
  # label = c("t:presapp"),
  # caption = "Model of Presidential Approval, 2000-2018. Coefficients are from a logit model where the dependent variable is presidential approval. 
  # Indicators for year are included in the model but not presented in the table.")



## run code to produce 1000 bootstrap first differences for
## high/low anxiety vs restrospective eval; counter will print in console
source('code/AllCode_321/boot_measures.R')

measures_plot <- ggplot(fd_measures, aes(x = measure, y = pp)) +
  geom_point() +
  coord_cartesian(ylim = c(-0.2, 0.1)) + 
  geom_errorbar(aes(x = measure, ymin = low, ymax = high), width = 0) + 
  geom_hline(yintercept = 0, linetype = 'dashed') +
  labs(x = 'Measure', y = 'First Difference (Low to High)') + 
  geom_text(aes(label=round(pp, 2)), hjust = -0.25, size = 3, show.legend = FALSE) +
  scale_color_grey(start = 0, end = 0.4) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(file = "paper/figures/crimeScoreFD_presapp_POQ.pdf", measures_plot, width = 6, height = 4, units = 'in') # save plot


# race x worry model, table 1, figure 2 in paper
all_pres <- glm(presapp ~ copartisan + independent + worryScale * black + crime_victimization + chgCrimeRate + 
                  male + incomeLt20 + income20t30 + income30t50 + income50t75 + senior + under30 + 
                  someCollege + baDegree + postGrad + rural + suburban + yr_factor,
                data = crimeData, 
                family = 'binomial')

dem_pres <- glm(presapp ~ copartisan + independent + worryScale * black + crime_victimization + chgCrimeRate + 
                  male + incomeLt20 + income20t30 + income30t50 + income50t75 + senior + under30 + 
                  someCollege + baDegree + postGrad + rural + suburban + yr_factor,
                data = crimeData %>% filter(president %in% c("Clinton", "Obama")), 
                family = 'binomial')

rep_pres <- glm(presapp ~ copartisan + independent + worryScale * black + crime_victimization + chgCrimeRate + 
                  male + incomeLt20 + income20t30 + income30t50 + income50t75 + senior + under30 + 
                  someCollege + baDegree + postGrad + rural + suburban + yr_factor,
                data = crimeData %>% filter(president %in% c("Bush", "Trump")), 
                family = 'binomial')

texreg(
  l = list(all_pres, dem_pres, rep_pres),
  omit.coef = "yr_|state",
  scalebox = .75,
  use.packages = FALSE,
  include.aic = FALSE,
  include.bic = FALSE,
  include.adjrs = TRUE,
  include.loglik = FALSE,
  include.deviance = FALSE,
  float.pos = "h",
  custom.coef.names = c("Intercept", "Co-partisan", "Independent", "Crime Anxiety", "Black", 'Crime Victimization', 
                        '$\\Delta$ Crime Rate', "Male", "Less than \\$20,000", "\\$20,00 to \\$29,999", "\\$30,000 to \\$49,999", "\\$50,000 to \\$74,999", "Senior", 
                        "Under 30", "Some College", "College Degree", "Post graduate degree", 'Rural', 'Suburban', "Crime Anxiety $\\times$ Black"),
  #  
  custom.header = list('Presidential Approval' = 1:3),
  custom.model.names = c("All", "Clinton / Obama", "Bush / Trump"),
  dcolumn = T,
  reorder.coef = c(4:5, 20, 2, 3, 6:19, 1),
  groups = list(Income=c(9:12),Age=c(13:14), Education=c(15:17), Urban = c(18:19)))#,
  # file = "~/Dropbox/crime/paper/tables/racemodels7.tex",
  # label = c("t:race"),
  # caption = "Models of Presidential Approval, 2000-2018. Coefficients are from logit regressions where the dependent variable is presidential approval.
  # Indicators for year are included in the model but not presented in the table.")

## run code to produce 1000 bootstrap first differences for
## each race x worry; counter will print in console
source('code/AllCode_321/boot_first_diffs.R')

race_fd_plot <- ggplot(race_fd, aes(x = race, y = pp, shape = race, color = race)) + 
  geom_point() + 
  facet_wrap(~president) + 
  geom_errorbar(aes(x = race, ymin = low, ymax = high), width = 0) + 
  coord_cartesian(ylim = c(-0.4, 0.1)) + 
  geom_hline(yintercept = 0, linetype = 'dashed') +
  labs(x = 'Race', y = 'First Difference (Low to High Anxiety)', shape = 'Race', color = 'Race') + 
  geom_text(aes(label=round(pp, 2), group = race), hjust = -0.25, size = 3, position = position_dodge(width = 0.5), show.legend = FALSE) +
  scale_color_grey(start = 0, end = 0.4) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave(file = "paper/figures/FD_raceparty_POQ.pdf", race_fd_plot, width = 8, height = 4, units = 'in') # save plot

quantile(pp_white_dem - pp_black_dem, c(.025, .5, .975)) # within differences, dem; no sig
quantile(pp_white_rep - pp_black_rep, c(.025, .5, .975)) # within differences, rep; yes sig

quantile(pp_white_dem - pp_white_rep, c(.025, .5, .975)) # between differences, white; yes sig
quantile(pp_black_dem - pp_black_rep, c(.05, .5, .95)) # within differences, black; no sig

# alternative crime measures
presapp_diffNat <- glm(presapp ~ copartisan + independent + worryScale + crime_victimization + diff_nat + white + male +  
                         incomeLt20 + income20t30 + income30t50 + income50t75 + senior + under30 + someCollege + baDegree + 
                         postGrad + rural + suburban + yr_factor, data = crimeData, family = "binomial")

presapp_absCh <- glm(presapp ~ copartisan + independent + worryScale + crime_victimization + abs_change_state_crime + white + male +  
                       incomeLt20 + income20t30 + income30t50 + income50t75 + senior + under30 + someCollege + baDegree + 
                       postGrad + rural + suburban + yr_factor, data = crimeData, family = "binomial")

presapp_homicide <- glm(presapp ~ copartisan + independent + worryScale + crime_victimization + chNumHomicide + white + male +  
                          incomeLt20 + income20t30 + income30t50 + income50t75 + senior + under30 + someCollege + baDegree + 
                          postGrad + rural + suburban + yr_factor, data = crimeData, family = "binomial")


texreg(
  l = list(presapp_worry, presapp_diffNat, presapp_absCh, presapp_homicide),
  omit.coef = "factor",
  scalebox = .65,
  use.packages = FALSE,
  include.aic = FALSE,
  include.bic = FALSE,
  include.adjrs = TRUE,
  include.loglik = FALSE,
  include.deviance = FALSE,
  float.pos = "h",
  custom.coef.names = c("Intercept", "Co-partisan", "Independent", "Anxiety", 'Crime Victimization', 
                        '$\\Delta$ State Crime Rate', 'White', "Male", "Less than \\$20,000", "\\$20,00 to \\$29,999", "\\$30,000 to \\$49,999", "\\$50,000 to \\$74,999", "Senior", 
                        "Under 30", "Some College", "College Degree", "Post graduate degree", 'Rural', 'Suburban', '$\\Delta$ State Crime Rate \n vs National',
                        '$\\Delta$ State Crime Counts', '$\\Delta$ State Homicide Counts'),
  custom.header = list('Presidential Approval' = 1:4),
  dcolumn = T,
  reorder.coef = c(6, 20:22, 4, 5, 2, 3, 7:19, 1),
  groups = list(Crime=c(1:4), Income=c(11:14),Age=c(15,16), Education=c(17:19), Urban =c(20,21)))#,
# file = "~/Dropbox/crime/paper/tables/alt_crime.tex",
# label = c("t:alt_crime"),
# caption = "Model of Presidential Approval with various objective measures of crime.")

# race x retrospective model
all_retro_mod <- glm(presapp ~ copartisan + independent + crime_us_yr_ago * black + crime_victimization
                      + chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + 
                        income50t75 + senior + under30 + someCollege + baDegree + 
                        postGrad + rural + suburban + yr_factor,
                      data = crimeData, 
                      family = "binomial")

dem_retro_mod <- glm(presapp ~ copartisan + independent + crime_us_yr_ago * black + crime_victimization
                      + chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + 
                        income50t75 + senior + under30 + someCollege + baDegree + 
                        postGrad + rural + suburban + yr_factor,
                      data = subset(crimeData, president %in% c("Clinton","Obama")), 
                      family = "binomial")

rep_retro_mod <- glm(presapp ~ copartisan + independent + crime_us_yr_ago * black + crime_victimization
                      + chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + 
                        income50t75 + senior + under30 + someCollege + baDegree + 
                        postGrad + rural + suburban + yr_factor,
                      data = subset(crimeData, president %in% c("Bush","Trump")),
                      family = "binomial")


texreg(
  l = list(all_retro_mod, dem_retro_mod, rep_retro_mod),
  omit.coef = "factor",
  scalebox = .65,
  use.packages = FALSE,
  include.aic = FALSE,
  include.bic = FALSE,
  include.adjrs = TRUE,
  include.loglik = FALSE,
  include.deviance = FALSE,
  float.pos = "h",
  custom.coef.names = c("Intercept", "Co-partisan", "Independent", "Retrospective", 'Black', 'Crime Victimization', 
                        '$\\Delta$ Crime Rate', "Male", "Less than \\$20,000", "\\$20,00 to \\$29,999", "\\$30,000 to \\$49,999", "\\$50,000 to \\$74,999", "Senior", 
                        "Under 30", "Some College", "College Degree", "Post graduate degree", 'Rural', 'Suburban', "Retrospective $\\times$ Black"),
  custom.header = list('Presidential Approval' = 1:3),
  custom.model.names = c("All", "Clinton / Obama", "Bush / Trump"),
  dcolumn = T,
  reorder.coef = c(4, 5, 20, 6:8, 2:3, 9:19, 1),
  groups = list(Income=c(9:12),Age=c(13,14), Education=c(15:17), Urban =c(18,19)))#,
  # file = "~/Dropbox/crime/paper/tables/retroreg7.tex",
  # label = c("t:retroReg"),
  # caption = "Model of Presidential Approval. Indicators for year are included in the model but not presented in the table.")


## run code to produce 1000 bootstrap first differences for
## each race x retrospective crime; counter will print in console
source('code/AllCode_321/boot_fd_retrospective.R')

retro_plot <- ggplot(retro_fd, aes(x = race, y = pp, shape = race, color = race)) + geom_point() +
  facet_wrap(~president) + 
  geom_errorbar(aes(x = race, ymin = low, ymax = high), width = 0) + 
  coord_cartesian(ylim = c(-0.6, 0.1)) + 
  geom_hline(yintercept = 0, linetype = 'dashed') +
  labs(x = 'Race', y = 'First Difference (Crime Better to Worse)', shape = 'Race', color = 'Race') + 
  geom_text(aes(label=round(pp, 2), group = race), hjust = -0.25, size = 3, show.legend = FALSE) +
  scale_color_grey(start = 0, end = 0.4) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(file = "paper/figures/retrospectiveplot_POQ.pdf", retro_plot, width = 8, height = 4, units = 'in') # save plot

# are these differences statistically significant between races?
quantile(pp_white_all - pp_black_all, c(0.025, 0.5, 0.975)) # not under all presidents
quantile(pp_white_dem - pp_black_dem, c(0.025, 0.5, 0.975)) # yes under dem
quantile(pp_white_rep - pp_black_rep, c(0.025, 0.5, 0.975)) # yes under republicans

## econonmic evaluations by race
crimeData <- crimeData %>% 
  mutate(econ_retro_rev = case_when(
    econ_retro == 1 ~ -1,
    econ_retro == 0 ~ 0,
    econ_retro == -1 ~ 1))

repdata <- subset(crimeData, president %in% c("Bush","Trump"))
repdata$yr_factor <- droplevels(repdata$yr_factor)
rep_econ_full <- glm(presapp ~ copartisan + independent + econ_retro_rev * black + crime_victimization
                      + chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + 
                        income50t75 + senior + under30 + someCollege + baDegree + 
                        postGrad + rural + suburban + yr_factor,
                      data = repdata,
                      family = "binomial")

texreg(
  l = list(rep_econ_full),
  omit.coef = "yr_|state",
  scalebox = .75,
  use.packages = FALSE,
  include.aic = FALSE,
  include.bic = FALSE,
  include.adjrs = TRUE,
  include.loglik = FALSE,
  include.deviance = FALSE,
  float.pos = "h",
  custom.coef.names = c("Intercept", "Co-partisan", "Independent", "Economic Evaluations", "Black", 'Crime Victimization', 
                        '$\\Delta$ Crime Rate', "Male", "Less than \\$20,000", "\\$20,00 to \\$29,999", "\\$30,000 to \\$49,999", "\\$50,000 to \\$74,999", "Senior", 
                        "Under 30", "Some College", "College Degree", "Post graduate degree", 'Rural', 'Suburban', "Economic Evaluations $\\times$ Black"),
  #  
  custom.header = list('Presidential Approval' = 1),
  custom.model.names = c("Bush / Trump"),
  dcolumn = T,
  reorder.coef = c(4:5, 20, 2, 3, 6:19, 1),
  groups = list(Income=c(10:13),Age=c(14:15), Education=c(16:18), Urban = c(19:20)))


source('code/AllCode_321/boot_fd_econ.R')

econ_plot <- ggplot(preds_rep, aes(x = race, y = pp, shape = race, color = race)) + geom_point() +
  facet_wrap(~president) + 
  geom_errorbar(aes(x = race, ymin = low, ymax = high), width = 0) + 
  coord_cartesian(ylim = c(-0.55, 0.1)) + 
  geom_hline(yintercept = 0, linetype = 'dashed') +
  labs(x = 'Race', y = 'First Difference (Economy Better to Worse)', shape = 'Race', color = 'Race') + 
  geom_text(aes(label=round(pp, 2), group = race), hjust = -0.25, size = 3, show.legend = FALSE) +
  scale_color_grey(start = 0, end = 0.4) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(file = "paper/figures/econplot_POQ.pdf", econ_plot, width = 4, height = 3, units = 'in') # save plot



# controlling for policy positions
approach_all_mod <- glm(presapp ~ copartisan + independent + worryScale * black + crime_victimization + 
                               chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + income50t75 + senior + under30 + 
                               someCollege + baDegree + postGrad + rural + suburban + yr_factor + crimeApproach,
                             data = crimeData, 
                             family = "binomial")

issue_all_mod <- glm(presapp ~ copartisan + independent + worryScale * black + crime_victimization + 
                            chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + income50t75 + senior + under30 + 
                            someCollege + baDegree + postGrad + rural + suburban + yr_factor + gunLaws + deathPenalty,
                          data = crimeData, 
                          family = "binomial")

approach_dem_mod <- glm(presapp ~ copartisan + independent + worryScale * black + crime_victimization + 
                               chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + income50t75 + senior + under30 + 
                               someCollege + baDegree + postGrad + rural + suburban + yr_factor + crimeApproach,
                             data = crimeData %>% filter(president %in% c('Clinton', 'Obama')), 
                             family = "binomial")

issue_dem_mod <- glm(presapp ~ copartisan + independent + worryScale * black + crime_victimization + 
                            chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + income50t75 + senior + under30 + 
                            someCollege + baDegree + postGrad + rural + suburban + yr_factor + gunLaws + deathPenalty,
                          data = crimeData %>% filter(president %in% c('Clinton', 'Obama')), 
                          family = "binomial")


approach_rep_mod <- glm(presapp ~ copartisan + independent + worryScale * black + crime_victimization + 
                               chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + income50t75 + senior + under30 + 
                               someCollege + baDegree + postGrad + rural + suburban + yr_factor + crimeApproach,
                             data = crimeData %>% filter(president %in% c('Bush', 'Trump')), 
                             family = "binomial")

issue_rep_mod <- glm(presapp ~ copartisan + independent + worryScale * black + crime_victimization + 
                            chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + income50t75 + senior + under30 + 
                            someCollege + baDegree + postGrad + rural + suburban + yr_factor + gunLaws + deathPenalty,
                          data = crimeData %>% filter(president %in% c('Bush', 'Trump')), 
                          family = "binomial")

texreg(
  l = list(approach_all_mod, approach_dem_mod, approach_rep_mod),
  omit.coef = "yr_|state",
  scalebox = .75,
  use.packages = FALSE,
  include.aic = FALSE,
  include.bic = FALSE,
  include.adjrs = TRUE,
  include.loglik = FALSE,
  include.deviance = FALSE,
  float.pos = "h",
  custom.coef.names = c("Intercept", "Co-partisan", "Independent", "Crime Anxiety", "Black", 'Crime Victimization', 
                        '$\\Delta$ Crime Rate', "Male", "Less than \\$20,000", "\\$20,00 to \\$29,999", "\\$30,000 to \\$49,999", "\\$50,000 to \\$74,999", "Senior", 
                        "Under 30", "Some College", "College Degree", "Post graduate degree", 'Rural', 'Suburban', 'Crime Approach', "Crime Anxiety $\\times$ Black"),
  #  
  custom.header = list('Presidential Approval' = 1:3),
  custom.model.names = c("All", "Clinton / Obama", "Bush / Trump"),
  dcolumn = T,
  reorder.coef = c(4:5, 21, 20, 2, 3, 6:19, 1),
  groups = list(Income=c(10:13),Age=c(14:15), Education=c(16:18), Urban = c(19:20))) #,
  # file = "~/Dropbox/crime/paper/tables/approach_POQ.tex",
  # label = c("t:approach"),
  # caption = "Models of Presidential Approval controlling for one's preferred approach to crime, 2000-2019. 
  # Coefficients are from logit regressions where the dependent variable is presidential approval.
  # Indicators for year are included in the model but not presented in the table.")

texreg(
  l = list(issue_all_mod, issue_dem_mod, issue_rep_mod),
  omit.coef = "yr_|state",
  scalebox = .75,
  use.packages = FALSE,
  include.aic = FALSE,
  include.bic = FALSE,
  include.adjrs = TRUE,
  include.loglik = FALSE,
  include.deviance = FALSE,
  float.pos = "h",
  custom.coef.names = c("Intercept", "Co-partisan", "Independent", "Crime Anxiety", "Black", 'Crime Victimization', 
                        '$\\Delta$ Crime Rate', "Male", "Less than \\$20,000", "\\$20,00 to \\$29,999", "\\$30,000 to \\$49,999", "\\$50,000 to \\$74,999", "Senior", 
                        "Under 30", "Some College", "College Degree", "Post graduate degree", 'Rural', 'Suburban', 'Stricter Gun Laws', 
                        'Favor Death Penalty', "Crime Anxiety $\\times$ Black"),
  #  
  custom.header = list('Presidential Approval' = 1:3),
  custom.model.names = c("All", "Clinton / Obama", "Bush / Trump"),
  dcolumn = T,
  reorder.coef = c(4:5, 22, 20:21, 2, 3, 6:19, 1),
  groups = list(Income=c(11:14),Age=c(15:16), Education=c(17:19), Urban = c(20:21)))#,
# file = "~/Dropbox/crime/paper/tables/crime_issues_POQ.tex",
# label = c("t:issues"),
# caption = "Models of Presidential Approval controlling for whether one thinks gun laws should be more strict and whether one favors the death penalty, 2001-2019.
# Coefficients are from logit regressions where the dependent variable is presidential approval.
# Indicators for year are included in the model but not presented in the table.")


## run code to produce 1000 bootstrap first differences
## controlling for approach and gun/death penalty opinion
source('code/AllCode_321/boot_policy.R')

policy_fd_plot <- ggplot(policy_fd, aes(x = race, y = pp, shape = race, color = race)) + 
  geom_point() + 
  facet_grid(cols = vars(president), rows = vars(type)) + 
  geom_errorbar(aes(x = race, ymin = low, ymax = high), width = 0) + 
  coord_cartesian(ylim = c(-0.8, 0.1)) + 
  geom_hline(yintercept = 0, linetype = 'dashed') +
  labs(x = 'Race', y = 'First Difference (Low to High Anxiety)', shape = 'Race', color = 'Race') + 
  geom_text(aes(label=round(pp, 2), group = race), hjust = -0.25, size = 3, position = position_dodge(width = 0.5), show.legend = FALSE) +
  scale_color_grey(start = 0, end = 0.4) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave(file = "paper/figures/policy_POQ.pdf", policy_fd_plot, width = 10, height = 6, units = 'in') # save plot

# Triple interaction, race x party x worry
all_triple_mod <- glm(presapp ~ independent + worryScale * black * democrat + crime_victimization
                       + chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + 
                         income50t75 + senior + under30 + someCollege + baDegree + 
                         postGrad + rural + suburban + yr_factor,
                       data = crimeData, 
                       family = "binomial")


dem_triple_mod <- glm(presapp ~ independent + worryScale * black * democrat + crime_victimization
                       + chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + 
                         income50t75 + senior + under30 + someCollege + baDegree + 
                         postGrad + rural + suburban + yr_factor,
                       data = crimeData %>% filter(president %in% c("Clinton", "Obama")), 
                       family = "binomial")

rep_triple_mod <- glm(presapp ~ independent + worryScale * black * democrat + crime_victimization
                       + chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + 
                         income50t75 + senior + under30 + someCollege + baDegree + 
                         postGrad + rural + suburban + yr_factor,
                       data = crimeData %>% filter(president %in% c("Bush", "Trump")), 
                       family = "binomial")


texreg(
  l = list(all_triple_mod, dem_triple_mod, rep_triple_mod),
  omit.coef = "yr_|state",
  scalebox = .75,
  use.packages = FALSE,
  include.aic = FALSE,
  include.bic = FALSE,
  include.adjrs = TRUE,
  include.loglik = FALSE,
  include.deviance = FALSE,
  float.pos = "h",
  custom.coef.names = c("Intercept", "Independent", "Crime Anxiety", "Black", 'Democrat', 'Crime Victimization', 
                        '$\\Delta$ Crime Rate', "Male", "Less than \\$20,000", "\\$20,00 to \\$29,999", "\\$30,000 to \\$49,999", "\\$50,000 to \\$74,999", "Senior", 
                        "Under 30", "Some College", "College Degree", "Post graduate degree", 'Rural', 'Suburban', "Crime Anxiety $\\times$ Black",
                        "Crime Anxiety $\\times$ Democrat", "Democrat $\\times$ Black", "Crime Anxiety $\\times$ Black $\\times$ Democrat"),
  #  
  custom.header = list('Presidential Approval' = 1:3),
  custom.model.names = c("All", "Clinton / Obama", "Bush / Trump"),
  dcolumn = T,
  reorder.coef = c(3:5, 20:23, 2, 6:19, 1),
  groups = list(Income=c(12:15),Age=c(16:17), Education=c(18:20), Urban = c(21:22)))#,
  # file = "~/Dropbox/crime/paper/tables/triple_int_POQ.tex",
  # label = c("t:triple"),
  # caption = "Models of Presidential Approval interacting race, party identification, and crime anxiety, 2000-2019.
  # Coefficients are from logit regressions where the dependent variable is presidential approval.
  # Indicators for year are included in the model but not presented in the table.")

## run code to produce 1000 bootstrap first differences for
## each race x party; counter will print in console
source('code/AllCode_321/boot_fd_triple_int2.R')

triple_int_plot <- ggplot(triple_int_diffs, aes(x = party, y = pp, shape = race, color = race)) + 
  geom_point(position=position_dodge(width = .5)) +
  facet_wrap(~president) + 
  geom_errorbar(aes(x = party, ymin = low, ymax = high), width = 0, position = position_dodge(width = 0.5)) + 
  coord_cartesian(ylim = c(-0.6, 0.1)) + 
  geom_hline(yintercept = 0, linetype = 'dashed') +
  labs(x = 'Party', y = 'First Difference (Low to High Anxiety)', shape = 'Race', color = 'Race') + 
  geom_text(aes(label=round(pp, 2), group = race), hjust = -0.25, size = 3, position = position_dodge(width = 0.5), show.legend = FALSE) +
  scale_color_grey(start = 0, end = 0.4) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(file = "paper/figures/fd_triple_int_POQ.pdf", triple_int_plot, width = 12, height = 6, units = 'in') # save plot
