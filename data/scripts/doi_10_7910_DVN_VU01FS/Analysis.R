
# create conjoint plots and write latex table with the main results
rm(list=ls())

options(stringsAsFactors = FALSE)
library(dplyr)
library(stargazer)
library(ggplot2)
library(ggthemes)
library(forcats)
library(assertthat)
if (!require(wpmarble)){
  devtools::install_github("wpmarble/wpmarble")
  library(wpmarble)
}
library(car)
library(nlWaldTest)

# Read cleaned conjoint data
d = readRDS("cleaned_conjoint.rds")

#dat = readRDS("cleaned_survey.rds")
#dat = dat %>% filter(as.numeric(Progress) > 49)



# Function for creating subsample data frames -----------------------------


subsample_conjoint = function(data, formula, variable, levels, clustvar = "responseid"){
  
  # only two levels
  stopifnot(length(levels) == 2)
  
  # drop missing values
  data = subset(data, !is.na(data[[variable]]))
  stopifnot(all(data[[variable]] %in% levels))
  
  # run analysis for subsample defined by first level
  lev1 = levels[1]
  dat1 = subset(data, data[[variable]] == lev1)
  reg1 = lm(formula, dat1)
  cov1 = vcovCluster(reg1, dat1[[clustvar]])
  ste1 = sqrt(diag(cov1))
  
  out1 = data_frame(variable = names(coef(reg1)[2:8]),
                    coef = coef(reg1)[2:8],
                    se = ste1[2:8])
  out1 = rbind(out1, refcats)
  out1$attribute = NA_character_
  out1$attribute[grepl("commtype", out1$variable)] = "Mode of communication"
  out1$attribute[grepl("numconst", out1$variable)] = "Number of constituents"
  out1$attribute[grepl("typeconst", out1$variable)] = "Type of constituents"
  out1$attribute[grepl("typearg", out1$variable)] = "Type of argument"
  
  out1$variable = gsub("commtype|numconst|typeconst_comb|typearg_comb", "", out1$variable)
  
  out1$variable = factor(out1$variable, 
                         c("Visit in office", "Social media",     # mode of communication
                           "5 people", "15 people", "30 people",  # number of constituents
                           "Natural opponents", "Natural allies", "Both", # type of constituent
                           "Objective studies", "Personal story", "Questioning motives") # type of arg
  ) 
  out1$attribute = factor(out1$attribute, 
                          c("Mode of communication", 
                            "Number of constituents", 
                            "Type of constituents", 
                            "Type of argument"))
  out1$subsample = lev1
  
  
  # run analysis for subsample defined by second level
  lev2 = levels[2]
  dat2 = subset(data, data[[variable]] == lev2)
  reg2 = lm(formula, dat2)
  cov2 = vcovCluster(reg2, dat2[[clustvar]])
  ste2 = sqrt(diag(cov2))
  
  out2 = data_frame(variable = names(coef(reg2)[2:8]),
                    coef = coef(reg2)[2:8],
                    se = ste2[2:8])
  out2 = rbind(out2, refcats)
  out2$attribute = NA_character_
  out2$attribute[grepl("commtype", out2$variable)] = "Mode of communication"
  out2$attribute[grepl("numconst", out2$variable)] = "Number of constituents"
  out2$attribute[grepl("typeconst", out2$variable)] = "Type of constituents"
  out2$attribute[grepl("typearg", out2$variable)] = "Type of argument"
  
  out2$variable = gsub("commtype|numconst|typeconst_comb|typearg_comb", "", out2$variable)
  
  out2$variable = factor(out2$variable, 
                         c("Visit in office", "Social media",     # mode of communication
                           "5 people", "15 people", "30 people",  # number of constituents
                           "Natural opponents", "Natural allies", "Both", # type of constituent
                           "Objective studies", "Personal story", "Questioning motives") # type of arg
  ) 
  out2$attribute = factor(out2$attribute, 
                          c("Mode of communication", 
                            "Number of constituents", 
                            "Type of constituents", 
                            "Type of argument"))
  out2$subsample = lev2
  
  
  
  # combine into one df
  out = rbind(out1, out2)
  
  return(out)
}




# Analyze conjoint --------------------------------------------------------

info_formula = formula(informative_num ~ commtype + numconst + typeconst_comb + typearg_comb + conjoint_id + factor(responseid))
infl_formula = formula(influential_num ~ commtype + numconst + typeconst_comb + typearg_comb + conjoint_id + factor(responseid))


refcats = data_frame(variable = c("commtypeVisit in office", 
                                  "numconst5 people",
                                  "typeconst_combNatural opponents",
                                  "typearg_combObjective studies"),
                     coef = 0, se = 0) 



### Outcome: Informativeness ### --- CODE FOR FIGURE 2


info_reg = lm(info_formula, d)
info_cov = vcovCluster(info_reg, d$responseid)
info_se = sqrt(diag(info_cov))


out_df_info = data_frame(variable = names(coef(info_reg)[2:8]),
                         coef = coef(info_reg)[2:8],
                         se = info_se[2:8])
out_df_info = rbind(out_df_info, refcats)
out_df_info$attribute = NA_character_
out_df_info$attribute[grepl("commtype", out_df_info$variable)] = "Mode of communication"
out_df_info$attribute[grepl("numconst", out_df_info$variable)] = "Number of constituents"
out_df_info$attribute[grepl("typeconst", out_df_info$variable)] = "Type of constituents"
out_df_info$attribute[grepl("typearg", out_df_info$variable)] = "Type of argument"

out_df_info$variable = gsub("commtype|numconst|typeconst_comb|typearg_comb", "", out_df_info$variable)

out_df_info$variable = factor(out_df_info$variable, 
                              c("Visit in office", "Social media",     # mode of communication
                                "5 people", "15 people", "30 people",  # number of constituents
                                "Natural opponents", "Natural allies", "Both", # type of constituent
                                "Objective studies", "Personal story", "Questioning motives") # type of arg
) 
out_df_info$attribute = factor(out_df_info$attribute, 
                               c("Mode of communication", 
                                 "Number of constituents", 
                                 "Type of constituents", 
                                 "Type of argument"))

info_sd = round(sd(d$informative_num, na.rm = TRUE), 2)
ggplot(out_df_info, aes(y = fct_rev(variable), x = coef, 
                        xmin = coef - 1.96*se, xmax = coef + 1.96*se)) + 
  geom_vline(xintercept = 0, lty = 3) + 
  geom_errorbarh(height = 0) + 
  geom_point() + 
  facet_wrap(~attribute, ncol = 1, scales = "free_y") + 
  theme_bw() + 
  theme(panel.spacing = unit(0, "in")) +
  scale_x_continuous(breaks = seq(-.5, .5, .25)) +
  coord_cartesian(xlim = c(-.5, .5)) + 
  # ggtitle("Effect of Message Type on Informativeness of Message") + 
  labs(x = paste0("Informativeness: Effect estimate\non 1-4 Scale (SD = ", info_sd, ")"),
       y = NULL) 
ggsave("figs/informativeness_conjoint.pdf",  width=4, height=6)
ggsave("figs/informativeness_conjoint.png",  width=4, height=6, units="in")






### Outcome: Influence ### --- CODE FOR FIGURE 2
infl_reg = lm(infl_formula, d)
infl_cov = vcovCluster(infl_reg, d$responseid)
infl_se = sqrt(diag(infl_cov))


out_df_infl = data_frame(variable = names(coef(infl_reg)[2:8]),
                         coef = coef(infl_reg)[2:8],
                         se = infl_se[2:8])
out_df_infl = rbind(out_df_infl, refcats)
out_df_infl$attribute = NA_character_
out_df_infl$attribute[grepl("commtype", out_df_infl$variable)] = "Mode of communication"
out_df_infl$attribute[grepl("numconst", out_df_infl$variable)] = "Number of constituents"
out_df_infl$attribute[grepl("typeconst", out_df_infl$variable)] = "Type of constituents"
out_df_infl$attribute[grepl("typearg", out_df_infl$variable)] = "Type of argument"

out_df_infl$variable = gsub("commtype|numconst|typeconst_comb|typearg_comb", "", out_df_infl$variable)

out_df_infl$variable = factor(out_df_infl$variable, 
                              c("Visit in office", "Social media",     # mode of communication
                                "5 people", "15 people", "30 people",  # number of constituents
                                "Natural opponents", "Natural allies", "Both", # type of constituent
                                "Objective studies", "Personal story", "Questioning motives") # type of arg
) 
out_df_infl$attribute = factor(out_df_infl$attribute, 
                               c("Mode of communication", 
                                 "Number of constituents", 
                                 "Type of constituents", 
                                 "Type of argument"))

infl_sd = round(sd(d$influential_num, na.rm = TRUE), 2)
ggplot(out_df_infl, aes(y = fct_rev(variable), x = coef, 
                        xmin = coef - 1.96*se, xmax = coef + 1.96*se)) + 
  geom_vline(xintercept = 0, lty = 3) + 
  geom_errorbarh(height = 0) + 
  geom_point() + 
  facet_wrap(~attribute, ncol = 1, scales = "free_y") + 
  theme_bw() + 
  scale_x_continuous(breaks = seq(-.5, .5, .25)) +
  coord_cartesian(xlim = c(-.5, .5)) + 
  theme(panel.spacing = unit(0, "in")) +
  # ggtitle("Effect of Message Type on Influence of Message") +
  labs(x = paste0("Influence: Effect estimate\non 1-4 Scale (SD = ", infl_sd, ")"),
       y = NULL) 
ggsave("figs/influence_conjoint.pdf", width=4, height=6)
ggsave("figs/influence_conjoint.png", width=4, height=6, units="in")




## write latex tables ##
cov.lab = c("Mode of Communication: Social Media",
            "Number of Constituents: 15 people",
            "Number of Constituents: 30 People",
            "Type of Constituents: Natural allies",
            "Type of Constituents: Both",
            "Type of Argument: Personal story",
            "Type of Argument: Questioning motives", 
            "Constant")

stargazer(info_reg, infl_reg,
          se = list(info_se, infl_se),
          omit = "responseid|conjoint",
          covariate.labels = cov.lab,
          add.lines = list(c("Respondent FE", "$\\checkmark$","$\\checkmark$"),
                           c("Conjoint topic indicator", "$\\checkmark$", "$\\checkmark$")),
          dep.var.labels = c("Informative", "Influential"),
          float = FALSE, 
          no.space = TRUE,
          align = TRUE,
          omit.stat = c("F", "ser"),
          out = "tables/conjoint_table.tex")




# Allow for different effects across topics -------------------------------

# we pool together the three different topics. Show that this is justified
# by allowing the slopes to vary across topics, then testing if the 
# coefficients are equal.

# outcome: influence
infl_formula_differ = formula(influential_num ~( commtype + numconst + typeconst_comb +
                                typearg_comb) * conjoint_id)
infl_formula_nofe = formula(influential_num ~ commtype + numconst + typeconst_comb +
                                                 typearg_comb + conjoint_id)
mod_differ = lm(infl_formula_differ, d)
vcv_differ = vcovCluster(mod_differ, d$responseid)
mod_nofe = lm(infl_formula_nofe, d)
waldtest(mod_differ, mod_nofe, vcov = vcv_differ)

# outcome: informativeness
info_formula_differ = formula(informative_num ~( commtype + numconst + typeconst_comb +
                                                   typearg_comb) * conjoint_id)
info_formula_nofe = formula(informative_num ~ commtype + numconst + typeconst_comb +
                              typearg_comb + conjoint_id)
mod_differ = lm(info_formula_differ, d)
vcv_differ = vcovCluster(mod_differ, d$responseid)
mod_nofe = lm(info_formula_nofe, d)
waldtest(mod_differ, mod_nofe, vcov = vcv_differ)



# Substantive effect sizes ------------------------------------------------

# Calculate predicted values for each possible profile, then use that to 
# give a sense of the substantive effect sizes. 

# create all profiles
unique_noNA = function(x) unique(x[!is.na(x)])
all_profiles = expand.grid(
  commtype = unique_noNA(d$commtype),
  numconst = unique_noNA(d$numconst),
  typeconst_comb = unique_noNA(d$typeconst_comb),
  typearg_comb = unique_noNA(d$typearg_comb)
)

# set responseID and conjoint id to level w/ median FE
response_fes = coef(infl_reg)[grepl("responseid", names(coef(infl_reg)))]
response_fes = sort(response_fes)
median_responseid = names(response_fes)[ceiling(length(response_fes)/ 2)]
median_responseid = gsub("factor(responseid)", "", median_responseid, fixed = TRUE)

all_profiles$responseid = median_responseid
all_profiles$conjoint_id = "dev"


predicted_dv = predict(infl_reg, all_profiles, se.fit = TRUE)
all_profiles$pred_infl    = predicted_dv$fit
all_profiles$pred_infl_se = predicted_dv$se.fit


## CODE FOR FIGURE 3

# Plot predicted outcomes of every possible profile that has visits in office,
# relative to the best possible social media profile
all_profiles = all_profiles %>% 
  arrange(pred_infl) %>% 
  mutate(rank = 1:length(pred_infl))
ggplot(all_profiles) +
  aes(y = pred_infl, x = rank, colour = commtype) + 
  geom_point() + 
  theme_bw() +
  scale_colour_hc(name = "Mode of\nCommunication") + 
  scale_x_continuous(breaks = seq(0, 100, 10)) + 
  scale_y_continuous(breaks = seq(0, 5, .25)) + 
  labs(x = "Rank", y = "Predicted influence") 
ggsave("figs/all_profiles_pred_infl.pdf", width=6,height=4)

#saveRDS(all_profiles, file = "data/all_profiles_predicted_influence.rds")





# Now fit a model that allows number of constitutents to enter linearly,
# rather than as separate effects for 5, 15, and 30. Then see how many
# more people would need to message to offset being on SM
d$numconst_num = as.numeric(substr(d$numconst, 1, 2))

infl_reg2 = lm(
  influential_num ~ commtype + numconst_num + 
    typeconst_comb + typearg_comb + conjoint_id + factor(responseid), 
  d)
infl_cov2 = vcovCluster(infl_reg2, d$responseid)


# calculate numbers tradeoff, holding all else equal. find X s.t. 
#   B_sm + B_num * X = B_num * 1
#   X = (B_num - B_sm) / B_num
nlConfint(infl_reg2, Vcov = infl_cov2, texts = "(b[3] - b[2] )/ b[3]")

info_reg2 = lm(
  informative_num ~ commtype + numconst_num + 
    typeconst_comb + typearg_comb + conjoint_id + factor(responseid), 
  d)
info_cov2 = vcovCluster(info_reg2, d$responseid)

# get CI for social media coefficient divided by number constituents coef
nlConfint(info_reg2, Vcov = info_cov2, texts = "(b[3] - b[2] )/ b[3]")


## Write latex table for this regression
cov.lab2 = c("Mode of Communication: Social Media",
            "Number of Constituents (linear)",
            "Type of Constituents: Natural allies",
            "Type of Constituents: Both",
            "Type of Argument: Personal story",
            "Type of Argument: Questioning motives", 
            "Constant")

stargazer(list(info_reg2, infl_reg2), 
          se = list(sqrt(diag(info_cov2)), sqrt(diag(infl_cov2))),
          omit = "responseid|conjoint",
          covariate.labels = cov.lab2,
          add.lines = list(c("Respondent FE", "$\\checkmark$","$\\checkmark$"),
                           c("Conjoint topic indicator", "$\\checkmark$", "$\\checkmark$")),
          dep.var.labels = c("Informative", "Influential"),
          float = FALSE, 
          no.space = TRUE,
          align = TRUE,
          omit.stat = c("F", "ser"),
          out = "tables/conjoint_table_linearnumconst.tex")



# How many more people would you need to hear from on SM if the messages
# were rude + diverse vs. in-person civil but low diversity? 
# To calculate this, need to find X such that:
#  B_SM + B_questioningmotives + B_diverse + B_num * X = B_num * 1
#  X = (B_num - S_sm - B_question - B_diverse) / B_num
nlConfint(infl_reg2, Vcov = infl_cov2, texts = "(b[5] + b[3] - b[7] - b[2]) / b[3]")


# Treatment interactions --------------------------------------------------

# run basic model with no FE
info_reg_nofe = lm(informative_num ~ commtype + numconst + typeconst_comb + typearg_comb + conjoint_id, d)
infl_reg_nofe = lm(influential_num ~ commtype + numconst + typeconst_comb + typearg_comb + conjoint_id, d)

# interact the "communication type" variable with all the others. 
# Cannot reject the (joint) null that there are no interactions

info_interact_form = formula(informative_num ~ commtype + numconst + typeconst_comb + typearg_comb + conjoint_id + 
                          commtype : numconst + 
                          commtype : typeconst_comb + 
                          commtype : typearg_comb)
info_interact_reg = lm(info_interact_form, d)
info_interact_cov = vcovCluster(info_interact_reg, d$responseid)
info_interact_se  = sqrt(diag(info_interact_cov))
cat("\n\nTest for interactions between social media treatment and other treatments on informativeness outcome:\n")
waldtest(info_interact_reg, info_reg_nofe, vcov = info_interact_cov, test = "F")




infl_interact_form = formula(influential_num ~ commtype + numconst + typeconst_comb + typearg_comb + conjoint_id + 
                               commtype : numconst + 
                               commtype : typeconst_comb + 
                               commtype : typearg_comb)
infl_interact_reg = lm(infl_interact_form, d)
infl_interact_cov = vcovCluster(infl_interact_reg, d$responseid)
cat("\n\nTest for interactions between social media treatment and other treatments on influence outcome:\n")
waldtest(infl_interact_reg, infl_reg_nofe, vcov = infl_interact_cov, test = "F")





# Subsample analyses ------------------------------------------------------

## To test for heterogeneous treatment effects, run the conjoint interacting
## each of the factor indicators with the desired covariate. Then use an 
## F-test to test the null hypothesis that all the interaction terms are all
## equal to 0. To compare nested comparisons, run the main analyses without fixed 
# effects on the same data.




# load covariates and merge with conjoint
cov_dat = readRDS("cleaned_survey.rds")
cov_dat = cov_dat %>% 
  select("ResponseId", "age", "educ", "pid5", 
         starts_with("off_sm_use"), "sm_type_personal") %>% 
  rename(responseid = ResponseId)

# geographic variabels
#geo = readRDS("Responses_with_Census_info.rds")
#geo = geo[-c(1,2),]
#geo = geo %>% 
#  select(ResponseId, Urban_prop, Population, College_prop) %>% 
#  rename(responseid = ResponseId) %>% 
#  mutate(responseid = as.character(responseid))

#saveRDS(geo, "geo.rds")
geo = readRDS("geo.rds")

cov_dat = left_join(cov_dat, geo, by = "responseid") %>% 
  mutate(cov_dat = 1)

# merge and summarise
d = left_join(d, cov_dat, "responseid")
assert_that(all(d$cov_dat == 1))




# Age Subsample Analysis --------------------------------------------------


## AGE: young vs. old, defined as bottom and top terciles, ##  
## which is below 53 / above 68                             ## 
## Result: no significant differences.                      ##
age_lo = quantile(cov_dat$age, .33, na.rm = TRUE)
age_hi = quantile(cov_dat$age, .66, na.rm = TRUE)

d$age_hilo = NA
d$age_hilo[d$age <= age_lo] = "Young"
d$age_hilo[d$age >= age_hi] = "Old"


# Run standard specifications w/o FE
info_reg_nofe = lm(informative_num ~ commtype + numconst + typeconst_comb + 
                     typearg_comb + conjoint_id, filter(d, !is.na(age_hilo)))
infl_reg_nofe = lm(influential_num ~ commtype + numconst + typeconst_comb + 
                     typearg_comb + conjoint_id, filter(d, !is.na(age_hilo)))

# outcome: info
info_formula_age = informative_num ~ (commtype + numconst + 
  typeconst_comb + typearg_comb + 
  conjoint_id) * age_hilo
info_reg_age = lm(info_formula_age, d)
info_cov_age = vcovCluster(info_reg_age, d$responseid)

cat("\n\nTest for heterogeneous treatment effects by age on informativeness outcome:\n")
waldtest(info_reg_age, info_reg_nofe, vcov = info_cov_age, test = "F")



# outcome: influence
infl_formula_age = influential_num ~ (commtype + numconst + 
                                        typeconst_comb + typearg_comb + 
                                        conjoint_id) * age_hilo
infl_reg_age = lm(infl_formula_age, d)
infl_cov_age = vcovCluster(infl_reg_age, d$responseid)

cat("\n\nTest for heterogeneous treatment effects by age on influence outcome:\n")
waldtest(infl_reg_age, infl_reg_nofe, vcov = infl_cov_age, test = "F")



# make plots using function defined at the top
info_age = subsample_conjoint(d, info_formula, "age_hilo", c("Old", "Young"))
ggplot(info_age, aes(y = fct_rev(variable), x = coef, colour = subsample,
                     xmin = coef - 1.96*se, xmax = coef + 1.96*se)) + 
  geom_vline(xintercept = 0, lty = 3) + 
  geom_errorbarh(height = 0, position = position_dodgev(height=.35)) + 
  geom_point(position = position_dodgev(height=.35)) + 
  facet_wrap(~attribute, ncol = 1, scales = "free_y") + 
  theme_bw() + 
  scale_x_continuous(breaks = seq(-.5, .5, .25)) +
  scale_colour_hc(name = "Age") + 
  coord_cartesian(xlim = c(-.55, .55)) + 
  theme(panel.spacing = unit(0, "in")) +
  theme(legend.position = "bottom", legend.direction = "vertical") + 
  # ggtitle("Effect of Message Type on Influence of Message") +
  labs(x = paste0("Informativeness: Effect estimate\non 1-4 Scale (SD = ", infl_sd, ")"),
       y = NULL) 
ggsave("figs/informativeness_conjoint_age.pdf", width=4, height=6)


infl_age = subsample_conjoint(d, infl_formula, "age_hilo", c("Old", "Young"))
ggplot(infl_age, aes(y = fct_rev(variable), x = coef, colour = subsample,
                     xmin = coef - 1.96*se, xmax = coef + 1.96*se)) + 
  geom_vline(xintercept = 0, lty = 3) + 
  geom_errorbarh(height = 0, position = position_dodgev(height=.35)) + 
  geom_point(position = position_dodgev(height=.35)) + 
  facet_wrap(~attribute, ncol = 1, scales = "free_y") + 
  theme_bw() + 
  scale_x_continuous(breaks = seq(-.5, .5, .25)) +
  scale_colour_hc(name = "Age") + 
  coord_cartesian(xlim = c(-.65, .65)) + 
  theme(panel.spacing = unit(0, "in")) +
  theme(legend.position = "bottom", legend.direction = "vertical") + 
  # ggtitle("Effect of Message Type on Influence of Message") +
  labs(x = paste0("Influence: Effect estimate\non 1-4 Scale (SD = ", infl_sd, ")"),
       y = NULL) 
ggsave("figs/influence_conjoint_age.pdf", width=4, height=6)





# Party Subsample Analysis ------------------------------------------------


## PARTY: democrat vs. republican, including leaners.  ##  
## Result: no significant differences.                 ##
d$party = NA
d$party[d$pid5 %in% c("Democrat", "Lean Democrat")] = "Democrat"
d$party[d$pid5 %in% c("Republican", "Lean Republican")] = "Republican"


# Run standard specifications w/o FE
info_reg_nofe = lm(informative_num ~ commtype + numconst + typeconst_comb + 
                     typearg_comb + conjoint_id, filter(d, !is.na(party)))
infl_reg_nofe = lm(influential_num ~ commtype + numconst + typeconst_comb + 
                     typearg_comb + conjoint_id, filter(d, !is.na(party)))

# outcome: info
info_formula_party = informative_num ~ (commtype + numconst + 
                                        typeconst_comb + typearg_comb + 
                                        conjoint_id) * party
info_reg_party = lm(info_formula_party, d)
info_cov_party = vcovCluster(info_reg_party, d$responseid)

cat("\n\nTest for heterogeneous treatment effects by party on informativeness outcome:\n")
waldtest(info_reg_party, info_reg_nofe, vcov = info_cov_party)



# outcome: influence
infl_formula_party = influential_num ~ (commtype + numconst + 
                                        typeconst_comb + typearg_comb + 
                                        conjoint_id) * party
infl_reg_party = lm(infl_formula_party, d)
infl_cov_party = vcovCluster(infl_reg_party, d$responseid)

cat("\n\nTest for heterogeneous treatment effects by party on influence outcome:\n")
waldtest(infl_reg_party, infl_reg_nofe, vcov = infl_cov_party)




# make plots using function defined at the top
info_party = subsample_conjoint(d, info_formula, "party", c("Democrat", "Republican"))
ggplot(info_party, aes(y = fct_rev(variable), x = coef, colour = subsample,
                     xmin = coef - 1.96*se, xmax = coef + 1.96*se)) + 
  geom_vline(xintercept = 0, lty = 3) + 
  geom_errorbarh(height = 0, position = position_dodgev(height=.35)) + 
  geom_point(position = position_dodgev(height=.35)) + 
  facet_wrap(~attribute, ncol = 1, scales = "free_y") + 
  theme_bw() + 
  scale_x_continuous(breaks = seq(-.5, .5, .25)) +
  coord_cartesian(xlim = c(-.55, .55)) + 
  scale_colour_manual(name = "Party", values = c("blue", "red")) + 
  theme(panel.spacing = unit(0, "in")) +
  theme(legend.position = "bottom", legend.direction = "vertical") + 
  # ggtitle("Effect of Message Type on Influence of Message") +
  labs(x = paste0("Informativeness: Effect estimate\non 1-4 Scale (SD = ", infl_sd, ")"),
       y = NULL) 
ggsave("figs/informativeness_conjoint_party.pdf", width=4, height=6)


infl_party = subsample_conjoint(d, infl_formula, "party", c("Democrat", "Republican"))
ggplot(infl_party, aes(y = fct_rev(variable), x = coef, colour = subsample,
                     xmin = coef - 1.96*se, xmax = coef + 1.96*se)) + 
  geom_vline(xintercept = 0, lty = 3) + 
  geom_errorbarh(height = 0, position = position_dodgev(height=.35)) + 
  geom_point(position = position_dodgev(height=.35)) + 
  facet_wrap(~attribute, ncol = 1, scales = "free_y") + 
  theme_bw() + 
  scale_x_continuous(breaks = seq(-.5, .5, .25)) +
  scale_colour_manual(name = "Party", values = c("blue", "red")) + 
  coord_cartesian(xlim = c(-.55, .55)) + 
  theme(panel.spacing = unit(0, "in")) +
  theme(legend.position = "bottom", legend.direction = "vertical") + 
  # ggtitle("Effect of Message Type on Influence of Message") +
  labs(x = paste0("Influence: Effect estimate\non 1-4 Scale (SD = ", infl_sd, ")"),
       y = NULL) 
ggsave("figs/influence_conjoint_party.pdf", width=4, height=6)







# Education Subsample Analysis --------------------------------------------


## EDUCATION: graduate degree vs. no graduate degree.     ##  
## Result: margina differences for the influence outcome, ##
## though they don't appear substantively all that large. ##
d$graddeg = "No graduate degree"
d$graddeg[d$educ == "Graduate degree"] = "Graduate degree"


# Run standard specifications w/o FE
info_reg_nofe = lm(informative_num ~ commtype + numconst + typeconst_comb + 
                     typearg_comb + conjoint_id, filter(d, !is.na(graddeg)))
infl_reg_nofe = lm(influential_num ~ commtype + numconst + typeconst_comb + 
                     typearg_comb + conjoint_id, filter(d, !is.na(graddeg)))

# outcome: info
info_formula_graddeg = informative_num ~ (commtype + numconst + 
                                            typeconst_comb + typearg_comb + 
                                            conjoint_id) * graddeg
info_reg_graddeg = lm(info_formula_graddeg, d)
info_cov_graddeg = vcovCluster(info_reg_graddeg, d$responseid)

cat("\n\nTest for heterogeneous treatment effects by education on informativeness outcome:\n")
waldtest(info_reg_graddeg, info_reg_nofe, vcov = info_cov_graddeg)



# outcome: influence
infl_formula_graddeg = influential_num ~ (commtype + numconst + 
                                            typeconst_comb + typearg_comb + 
                                            conjoint_id) * graddeg
infl_reg_graddeg = lm(infl_formula_graddeg, d)
infl_cov_graddeg = vcovCluster(infl_reg_graddeg, d$responseid)

cat("\n\nTest for heterogeneous treatment effects by education on influence outcome:\n")
waldtest(infl_reg_graddeg, infl_reg_nofe, vcov = infl_cov_graddeg)




# make plots using function defined at the top
info_graddeg = subsample_conjoint(d, info_formula, "graddeg", c("No graduate degree", "Graduate degree"))
ggplot(info_graddeg, aes(y = fct_rev(variable), x = coef, colour = subsample,
                         xmin = coef - 1.96*se, xmax = coef + 1.96*se)) + 
  geom_vline(xintercept = 0, lty = 3) + 
  geom_errorbarh(height = 0, position = position_dodgev(height=.35)) + 
  geom_point(position = position_dodgev(height=.35)) + 
  facet_wrap(~attribute, ncol = 1, scales = "free_y") + 
  theme_bw() + 
  scale_x_continuous(breaks = seq(-.5, .5, .25)) +
  coord_cartesian(xlim = c(-.55, .55)) + 
  scale_colour_hc(name = "Education") + 
  theme(panel.spacing = unit(0, "in")) +
  theme(legend.position = "bottom", legend.direction = "vertical") + 
  # ggtitle("Effect of Message Type on Influence of Message") +
  labs(x = paste0("Informativeness: Effect estimate\non 1-4 Scale (SD = ", infl_sd, ")"),
       y = NULL) 
ggsave("figs/informativeness_conjoint_educ.pdf", width=4, height=6)


infl_graddeg = subsample_conjoint(d, infl_formula, "graddeg", c("No graduate degree", "Graduate degree"))
ggplot(infl_graddeg, aes(y = fct_rev(variable), x = coef, colour = subsample,
                         xmin = coef - 1.96*se, xmax = coef + 1.96*se)) + 
  geom_vline(xintercept = 0, lty = 3) + 
  geom_errorbarh(height = 0, position = position_dodgev(height=.35)) + 
  geom_point(position = position_dodgev(height=.35)) + 
  facet_wrap(~attribute, ncol = 1, scales = "free_y") + 
  theme_bw() + 
  scale_x_continuous(breaks = seq(-.5, .5, .25)) +
  scale_colour_hc(name = "Education") + 
  coord_cartesian(xlim = c(-.55, .55)) + 
  theme(panel.spacing = unit(0, "in")) +
  theme(legend.position = "bottom", legend.direction = "vertical") + 
  # ggtitle("Effect of Message Type on Influence of Message") +
  labs(x = paste0("Influence: Effect estimate\non 1-4 Scale (SD = ", infl_sd, ")"),
       y = NULL) 
ggsave("figs/influence_conjoint_educ.pdf", width=4, height=6)








# Social Media Use Subsample Analysis -------------------------------------


## SOCIAL MEDIA USE: high vs. low offical use of facebook ##  
## Result: margina differences for the influence outcome, ##
## though they don't appear substantively all that large. ##
d$fb_use = "Uses Facebook"
d$fb_use[d$off_sm_use_fb == "Never"] = "Doesn't use Facebook"


# Run standard specifications w/o FE
info_reg_nofe = lm(informative_num ~ commtype + numconst + typeconst_comb + 
                     typearg_comb + conjoint_id, filter(d, !is.na(fb_use)))
infl_reg_nofe = lm(influential_num ~ commtype + numconst + typeconst_comb + 
                     typearg_comb + conjoint_id, filter(d, !is.na(fb_use)))

# outcome: info
info_formula_fb_use = informative_num ~ (commtype + numconst + 
                                           typeconst_comb + typearg_comb + 
                                           conjoint_id) * fb_use
info_reg_fb_use = lm(info_formula_fb_use, d)
info_cov_fb_use = vcovCluster(info_reg_fb_use, d$responseid)

cat("\n\nTest for heterogeneous treatment effects by social media use on informativeness outcome:\n")
waldtest(info_reg_fb_use, info_reg_nofe, vcov = info_cov_fb_use)



# outcome: influence
infl_formula_fb_use = influential_num ~ (commtype + numconst + 
                                           typeconst_comb + typearg_comb + 
                                           conjoint_id) * fb_use
infl_reg_fb_use = lm(infl_formula_fb_use, d)
infl_cov_fb_use = vcovCluster(infl_reg_fb_use, d$responseid)

cat("\n\nTest for heterogeneous treatment effects by social media use on influence outcome:\n")
waldtest(infl_reg_fb_use, infl_reg_nofe, vcov = infl_cov_fb_use)




# make plots using function defined at the top
info_fb_use = subsample_conjoint(d, info_formula, "fb_use", c("Uses Facebook", "Doesn't use Facebook"))
ggplot(info_fb_use, aes(y = fct_rev(variable), x = coef, colour = subsample,
                        xmin = coef - 1.96*se, xmax = coef + 1.96*se)) + 
  geom_vline(xintercept = 0, lty = 3) + 
  geom_errorbarh(height = 0, position = position_dodgev(height=.35)) + 
  geom_point(position = position_dodgev(height=.35)) + 
  facet_wrap(~attribute, ncol = 1, scales = "free_y") + 
  theme_bw() + 
  scale_x_continuous(breaks = seq(-.5, .5, .25)) +
  coord_cartesian(xlim = c(-.55, .55)) + 
  scale_colour_hc(name = "Facebook use") + 
  theme(panel.spacing = unit(0, "in")) +
  theme(legend.position = "bottom", legend.direction = "vertical") + 
  # ggtitle("Effect of Message Type on Influence of Message") +
  labs(x = paste0("Informativeness: Effect estimate\non 1-4 Scale (SD = ", infl_sd, ")"),
       y = NULL) 
ggsave("figs/informativeness_conjoint_fbuse.pdf", width=4, height=6)


infl_fb_use = subsample_conjoint(d, infl_formula, "fb_use", c("Uses Facebook", "Doesn't use Facebook"))
ggplot(infl_fb_use, aes(y = fct_rev(variable), x = coef, colour = subsample,
                        xmin = coef - 1.96*se, xmax = coef + 1.96*se)) + 
  geom_vline(xintercept = 0, lty = 3) + 
  geom_errorbarh(height = 0, position = position_dodgev(height=.35)) + 
  geom_point(position = position_dodgev(height=.35)) + 
  facet_wrap(~attribute, ncol = 1, scales = "free_y") + 
  theme_bw() + 
  scale_x_continuous(breaks = seq(-.5, .5, .25)) +
  scale_colour_hc(name = "Facebook use") + 
  coord_cartesian(xlim = c(-.55, .55)) + 
  theme(panel.spacing = unit(0, "in")) +
  theme(legend.position = "bottom", legend.direction = "vertical") + 
  # ggtitle("Effect of Message Type on Influence of Message") +
  labs(x = paste0("Influence: Effect estimate\non 1-4 Scale (SD = ", infl_sd, ")"),
       y = NULL) 
ggsave("figs/influence_conjoint_fbuse.pdf", width=4, height=6)







# Constituency Educ Subsample Analysis ------------------------------------


## CONSTITUENCY EDUCATION: high vs. low education of constituency ## 
# compare top vs. bottom tercile of proportion of constituency with
# a college education.

college_lo = quantile(cov_dat$College_prop, .33, na.rm = TRUE)
college_hi = quantile(cov_dat$College_prop, .66, na.rm = TRUE)

d$college_hilo = NA
d$college_hilo[d$College_prop <= college_lo] = "Low education"
d$college_hilo[d$College_prop >= college_hi] = "High education"





# Run standard specifications w/o FE
info_reg_nofe = lm(informative_num ~ commtype + numconst + typeconst_comb + 
                     typearg_comb + conjoint_id, filter(d, !is.na(college_hilo)))
infl_reg_nofe = lm(influential_num ~ commtype + numconst + typeconst_comb + 
                     typearg_comb + conjoint_id, filter(d, !is.na(college_hilo)))

# outcome: info
info_formula_college = informative_num ~ (commtype + numconst + 
                                            typeconst_comb + typearg_comb + 
                                            conjoint_id) * college_hilo
info_reg_college = lm(info_formula_college, d)
info_cov_college = vcovCluster(info_reg_college, d$responseid)

cat("\n\nTest for heterogeneous treatment effects by constituency education use on informativeness outcome:\n")
waldtest(info_reg_college, info_reg_nofe, vcov = info_cov_college)



# outcome: influence
infl_formula_college = influential_num ~ (commtype + numconst + 
                                            typeconst_comb + typearg_comb + 
                                            conjoint_id) * college_hilo
infl_reg_college = lm(infl_formula_college, d)
infl_cov_college = vcovCluster(infl_reg_college, d$responseid)

cat("\n\nTest for heterogeneous treatment effects by constituency education use on influence outcome:\n")
waldtest(infl_reg_college, infl_reg_nofe, vcov = infl_cov_college)




# make plots using function defined at the top
info_college = subsample_conjoint(d, info_formula, "college_hilo", c("High education", "Low education"))
ggplot(info_college, aes(y = fct_rev(variable), x = coef, colour = subsample,
                         xmin = coef - 1.96*se, xmax = coef + 1.96*se)) + 
  geom_vline(xintercept = 0, lty = 3) + 
  geom_errorbarh(height = 0, position = position_dodgev(height=.35)) + 
  geom_point(position = position_dodgev(height=.35)) + 
  facet_wrap(~attribute, ncol = 1, scales = "free_y") + 
  theme_bw() + 
  scale_x_continuous(breaks = seq(-.5, .5, .25)) +
  coord_cartesian(xlim = c(-.55, .55)) + 
  scale_colour_hc(name = "Proportion of constituency\nw/ urban degree") + 
  theme(panel.spacing = unit(0, "in")) +
  theme(legend.position = "bottom", legend.direction = "vertical") + 
  # ggtitle("Effect of Message Type on Influence of Message") +
  labs(x = paste0("Informativeness: Effect estimate\non 1-4 Scale (SD = ", infl_sd, ")"),
       y = NULL) 
ggsave("figs/informativeness_conjoint_constituency_educ.pdf", width=4, height=6)


infl_college = subsample_conjoint(d, infl_formula, "college_hilo", c("High education", "Low education"))
ggplot(infl_college, aes(y = fct_rev(variable), x = coef, colour = subsample,
                         xmin = coef - 1.96*se, xmax = coef + 1.96*se)) + 
  geom_vline(xintercept = 0, lty = 3) + 
  geom_errorbarh(height = 0, position = position_dodgev(height=.35)) + 
  geom_point(position = position_dodgev(height=.35)) + 
  facet_wrap(~attribute, ncol = 1, scales = "free_y") + 
  theme_bw() + 
  scale_x_continuous(breaks = seq(-.5, .5, .25)) +
  scale_colour_hc(name = "Proportion of constituency\nw/ college degree") + 
  coord_cartesian(xlim = c(-.55, .55)) + 
  theme(panel.spacing = unit(0, "in")) +
  theme(legend.position = "bottom", legend.direction = "vertical") + 
  # ggtitle("Effect of Message Type on Influence of Message") +
  labs(x = paste0("Influence: Effect estimate\non 1-4 Scale (SD = ", infl_sd, ")"),
       y = NULL) 
ggsave("figs/influence_conjoint_constituency_educ.pdf", width=4, height=6)






# Constituency Urbanness Subsample Analysis -------------------------------


## CONSTITUENCY URBAN/RURAL: high vs. low percent rural in constituency ## 
# compare top vs. bottom tercile of proportion of constituency in urban areas

urban_lo = quantile(cov_dat$Urban_prop, .33, na.rm = TRUE)
urban_hi = quantile(cov_dat$Urban_prop, .66, na.rm = TRUE)

d$urban_hilo = NA
d$urban_hilo[d$Urban_prop <= urban_lo] = "Low urban"
d$urban_hilo[d$Urban_prop >= urban_hi] = "High urban"





# Run standard specifications w/o FE
info_reg_nofe = lm(informative_num ~ commtype + numconst + typeconst_comb + 
                     typearg_comb + conjoint_id, filter(d, !is.na(urban_hilo)))
infl_reg_nofe = lm(influential_num ~ commtype + numconst + typeconst_comb + 
                     typearg_comb + conjoint_id, filter(d, !is.na(urban_hilo)))

# outcome: info
info_formula_urban = informative_num ~ (commtype + numconst + 
                                          typeconst_comb + typearg_comb + 
                                          conjoint_id) * urban_hilo
info_reg_urban = lm(info_formula_urban, d)
info_cov_urban = vcovCluster(info_reg_urban, d$responseid)

cat("\n\nTest for heterogeneous treatment effects by constituency urbanness on informativeness outcome:\n")
waldtest(info_reg_urban, info_reg_nofe, vcov = info_cov_urban)



# outcome: influence
infl_formula_urban = influential_num ~ (commtype + numconst + 
                                          typeconst_comb + typearg_comb + 
                                          conjoint_id) * urban_hilo
infl_reg_urban = lm(infl_formula_urban, d)
infl_cov_urban = vcovCluster(infl_reg_urban, d$responseid)

cat("\n\nTest for heterogeneous treatment effects by constituency urbanness on influence outcome:\n")
waldtest(infl_reg_urban, infl_reg_nofe, vcov = infl_cov_urban)




# make plots using function defined at the top
info_urban = subsample_conjoint(d, info_formula, "urban_hilo", c("High urban", "Low urban"))
ggplot(info_urban, aes(y = fct_rev(variable), x = coef, colour = subsample,
                       xmin = coef - 1.96*se, xmax = coef + 1.96*se)) + 
  geom_vline(xintercept = 0, lty = 3) + 
  geom_errorbarh(height = 0, position = position_dodgev(height=.35)) + 
  geom_point(position = position_dodgev(height=.35)) + 
  facet_wrap(~attribute, ncol = 1, scales = "free_y") + 
  theme_bw() + 
  scale_x_continuous(breaks = seq(-.5, .5, .25)) +
  coord_cartesian(xlim = c(-.55, .55)) + 
  scale_colour_hc(name = "Urbanness of constituency") + 
  theme(panel.spacing = unit(0, "in")) +
  theme(legend.position = "bottom", legend.direction = "vertical") + 
  # ggtitle("Effect of Message Type on Influence of Message") +
  labs(x = paste0("Informativeness: Effect estimate\non 1-4 Scale (SD = ", infl_sd, ")"),
       y = NULL) 
ggsave("figs/informativeness_conjoint_constituency_urban.pdf", width=4, height=6)


infl_urban = subsample_conjoint(d, infl_formula, "urban_hilo", c("High urban", "Low urban"))
ggplot(infl_urban, aes(y = fct_rev(variable), x = coef, colour = subsample,
                       xmin = coef - 1.96*se, xmax = coef + 1.96*se)) + 
  geom_vline(xintercept = 0, lty = 3) + 
  geom_errorbarh(height = 0, position = position_dodgev(height=.35)) + 
  geom_point(position = position_dodgev(height=.35)) + 
  facet_wrap(~attribute, ncol = 1, scales = "free_y") + 
  theme_bw() + 
  scale_x_continuous(breaks = seq(-.5, .5, .25)) +
  scale_colour_hc(name = "Urbanness of constituency") + 
  coord_cartesian(xlim = c(-.55, .55)) + 
  theme(panel.spacing = unit(0, "in")) +
  theme(legend.position = "bottom", legend.direction = "vertical") + 
  # ggtitle("Effect of Message Type on Influence of Message") +
  labs(x = paste0("Influence: Effect estimate\non 1-4 Scale (SD = ", infl_sd, ")"),
       y = NULL) 
ggsave("figs/influence_conjoint_constituency_urban.pdf", width=4, height=6)








# Constituency Population Subsample Analysis ------------------------------


## CONSTITUENCY POPULATION: high vs. low population ## 
# compare top vs. bottom tercile of constituency population

pop_lo = quantile(cov_dat$Population, .33, na.rm = TRUE)
pop_hi = quantile(cov_dat$Population, .66, na.rm = TRUE)

d$pop_hilo = NA
d$pop_hilo[d$Urban_prop <= urban_lo] = "Low population"
d$pop_hilo[d$Urban_prop >= urban_hi] = "High population"





# Run standard specifications w/o FE
info_reg_nofe = lm(informative_num ~ commtype + numconst + typeconst_comb + 
                     typearg_comb + conjoint_id, filter(d, !is.na(pop_hilo)))
infl_reg_nofe = lm(influential_num ~ commtype + numconst + typeconst_comb + 
                     typearg_comb + conjoint_id, filter(d, !is.na(pop_hilo)))

# outcome: info
info_formula_pop = informative_num ~ (commtype + numconst + 
                                        typeconst_comb + typearg_comb + 
                                        conjoint_id) * pop_hilo
info_reg_pop = lm(info_formula_pop, d)
info_cov_pop = vcovCluster(info_reg_pop, d$responseid)

cat("\n\nTest for heterogeneous treatment effects by constituency population on informativeness outcome:\n")
waldtest(info_reg_pop, info_reg_nofe, vcov = info_cov_pop)



# outcome: influence
infl_formula_pop = influential_num ~ (commtype + numconst + 
                                        typeconst_comb + typearg_comb + 
                                        conjoint_id) * pop_hilo
infl_reg_pop = lm(infl_formula_pop, d)
infl_cov_pop = vcovCluster(infl_reg_pop, d$responseid)

cat("\n\nTest for heterogeneous treatment effects by constituency population on influence outcome:\n")
waldtest(infl_reg_pop, infl_reg_nofe, vcov = infl_cov_pop)




# make plots using function defined at the top
info_pop = subsample_conjoint(d, info_formula, "pop_hilo", c("High population", "Low population"))
ggplot(info_pop, aes(y = fct_rev(variable), x = coef, colour = subsample,
                     xmin = coef - 1.96*se, xmax = coef + 1.96*se)) + 
  geom_vline(xintercept = 0, lty = 3) + 
  geom_errorbarh(height = 0, position = position_dodgev(height=.35)) + 
  geom_point(position = position_dodgev(height=.35)) + 
  facet_wrap(~attribute, ncol = 1, scales = "free_y") + 
  theme_bw() + 
  scale_x_continuous(breaks = seq(-.5, .5, .25)) +
  coord_cartesian(xlim = c(-.55, .55)) + 
  scale_colour_hc(name = "Constituency population") + 
  theme(panel.spacing = unit(0, "in")) +
  theme(legend.position = "bottom", legend.direction = "vertical") + 
  # ggtitle("Effect of Message Type on Influence of Message") +
  labs(x = paste0("Informativeness: Effect estimate\non 1-4 Scale (SD = ", infl_sd, ")"),
       y = NULL) 
ggsave("figs/informativeness_conjoint_constituency_population.pdf", width=4, height=6)


infl_pop = subsample_conjoint(d, infl_formula, "pop_hilo", c("High population", "Low population"))
ggplot(infl_pop, aes(y = fct_rev(variable), x = coef, colour = subsample,
                     xmin = coef - 1.96*se, xmax = coef + 1.96*se)) + 
  geom_vline(xintercept = 0, lty = 3) + 
  geom_errorbarh(height = 0, position = position_dodgev(height=.35)) + 
  geom_point(position = position_dodgev(height=.35)) + 
  facet_wrap(~attribute, ncol = 1, scales = "free_y") + 
  theme_bw() + 
  scale_x_continuous(breaks = seq(-.5, .5, .25)) +
  scale_colour_hc(name = "Constituency population") + 
  coord_cartesian(xlim = c(-.55, .55)) + 
  theme(panel.spacing = unit(0, "in")) +
  theme(legend.position = "bottom", legend.direction = "vertical") + 
  # ggtitle("Effect of Message Type on Influence of Message") +
  labs(x = paste0("Influence: Effect estimate\non 1-4 Scale (SD = ", infl_sd, ")"),
       y = NULL) 
ggsave("figs/influence_conjoint_constituency_population.pdf", width=4, height=6)


