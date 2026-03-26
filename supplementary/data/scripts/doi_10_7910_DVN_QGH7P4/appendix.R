
# Call helper functions

source("helpers.R")

# Read in data

elite_df <- read.csv("../data/analysis/elite_survey.csv")
citizen_df <- read.csv("../data/analysis/citizen_survey.csv")
admin_df <- read.csv("../data/analysis/resignation_admin.csv")
experiment_df <- read.csv("../data/analysis/survey_experiment_clean.csv")
expert_df <- read.csv("../data/analysis/expert_predictions.csv")

# Table B.1: President characteristics in sample

president_summary_statistics <- skim(elite_df %>% select(reserved_sc,
                                             reserved_obc,
                                             reserved_women,
                                             sarpanch_male,
                                             sarpanch_maratha,
                                             sarpanch_land_own_name,
                                             pacca_house,
                                             three_four_wheeler))

table_output <- president_summary_statistics %>% select(
  skim_variable,
  numeric.mean,
  numeric.sd
) %>% rename(
  Variable = skim_variable,
  Mean = numeric.mean,
  SD = numeric.sd
) %>% mutate(Variable = recode(Variable,
                           reserved_sc = "Reserved SC",
                           reserved_obc = "Reserved OBC",
                           reserved_women = "Reserved Women",
                           sarpanch_male = "Sarpanch is Male",
                           sarpanch_maratha = "Sarpanch is Maratha",
                           sarpanch_land_own_name = "Owns Land (Own Name)",
                           pacca_house = "Lives in Pacca House",
                           three_four_wheeler = "Owns Three/Four Wheeler"
),
,
Mean = round(Mean, 3),
SD = round(SD, 3))

latex_table <- kable(table_output, format = "latex", booktabs = TRUE,
      caption = "President characteristics in sample")

output_path <- "../results/tables/tableb1.tex"

writeLines(latex_table, con = output_path)

# Table B.2: Citizen characteristics in sample

citizen_summary_statistics <- skim(citizen_df %>% select(citizen_female,
                                                       citizen_SC,
                                                       citizen_OBC,
                                                       citizen_GEN,
                                                       citizen_ST,
                                                       citizen_education,
                                                       citizen_age,
                                                       citizen_schoolteacher,
                                                       citizen_anganwadiworker,
                                                       citizen_arogyasevika,
                                                       citizen_pdsshopkeeper,
                                                       citizen_shg_leader,
                                                       citizen_doctor_etc,
                                                       citizen_party_worker,
                                                       citizen_phc_worker,
                                                       citizen_kotwal,
                                                       citizen_govt_employee,
                                                       citizen_talathi,
                                                       citizen_shg_president,
                                                       citizen_informal_leader,
                                                       citizen_previous_gp_member,
                                                       citizen_previous_upa_sarpanch,
                                                       citizen_previous_sarpanch,
                                                       citizen_prior_experience_politics,
                                                       citizen_muslim,
                                                       citizen_hindu,
                                                       citizen_live_in_village,
                                                       citizen_monthly_wage,
                                                       citizen_hh_vehicle,
                                                       citizen_smartphone,
                                                       citizen_outside_time,
                                                       citizen_time_to_GP))

table_output <- citizen_summary_statistics %>% select(
  skim_variable,
  numeric.mean,
  numeric.sd
) %>% rename(
  Variable = skim_variable,
  Mean = numeric.mean,
  SD = numeric.sd
) %>% mutate(Variable = recode(Variable,
                               citizen_female = "Female",
                               citizen_SC = "SC",
                               citizen_OBC = "OBC",
                               citizen_GEN = "GEN",
                               citizen_ST = "ST",
                               citizen_education = "Education",
                               citizen_age = "Age",
                               citizen_schoolteacher = "School teacher",
                               citizen_anganwadiworker = "Anganwadi worker",
                               citizen_arogyasevika = "Arogya sevika",
                               citizen_pdsshopkeeper = "PDS shopkeeper",
                               citizen_shg_leader = "SHG leader",
                               citizen_doctor_etc = "Doctor",
                               citizen_party_worker = "Party worker",
                               citizen_phc_worker = "PHD worker",
                               citizen_kotwal = "Kotwal",
                               citizen_govt_employee = "Government employee",
                               citizen_talathi = "Talathi",
                               citizen_shg_president = "SHG president",
                               citizen_informal_leader = "Informal leader",
                               citizen_previous_gp_member = "Previous council member",
                               citizen_previous_upa_sarpanch = "Previous vice president",
                               citizen_previous_sarpanch = "Previous president",
                               citizen_prior_experience_politics = "Prior political experience",
                               citizen_muslim = "Muslim",
                               citizen_hindu = "Hindu",
                               citizen_live_in_village = "Lives in village",
                               citizen_monthly_wage = "Monthly wage (INR)",
                               citizen_hh_vehicle = "Has a vehicle",
                               citizen_smartphone = "Has a smartphone",
                               citizen_outside_time = "Hours spent outside in a day",
                               citizen_time_to_GP = "Time to council office (hours)"
),
Mean = round(Mean, 3),
SD = round(SD, 3))

latex_table <- kable(table_output, format = "latex", booktabs = TRUE,
                     caption = "Citizen characteristics in sample")

output_path <- "../results/tables/tableb2.tex"

writeLines(latex_table, con = output_path)


# Figure C.1: Staggered establishment dates in Maharashtra

elite_df$establishment_date <- as.Date(elite_df$establishment_date)

est_dates <- 
  ggplot(elite_df, aes(x = establishment_date)) +
  geom_histogram(binwidth = 30, color = "black", fill = "skyblue") +
  scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
  labs(title = "Establishment dates of councils in survey sample",
       x = "Establishment date",
       y = "Frequency") +
  geom_vline(xintercept = as.Date("1958-01-01"), color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = as.Date("1995-01-01"), color = "red", linetype = "dashed", size = 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("../results/figures/figurec1.png", width=8, height=6, units = "in", est_dates)


# Table C.1: Establishment date predicts electoral cycle

model1 <- lm_robust(sarpanch_election_year ~ establishment_year, data = elite_df)

options(modelsummary_panel_labels = "roman")
options(modelsummary_get = "broom")
options(modelsummary_format_numeric_latex = "mathmode")

latex_code <- modelsummary(model1,
             title = "Establishment date predicts electoral cycle",
             stars = TRUE,
             coef_rename = c("establishment_year" = "Establishment year"),
             gof_omit = 'DF|AIC|BIC|RMSE|Std.Errors',
             escape = FALSE,
             output = "latex",
             note = "Regression of the current electoral cycle year on council establishment year with robust standard errors."
)

latex_string <- as.character(latex_code)

writeLines(latex_string, con = "../results/tables/tablec1.tex")


# Table C.2: Direct elections do not predict establishment dates

models <- list(
  "Established pre-1958" = lm_robust(establishment_pre_1958 ~ direct, data = elite_df),
  "Established between 1958 and 1995" = lm_robust(establishment_period_between_1 ~ direct, data = elite_df),
  "Established post-1995" = lm_robust(establishment_post_1995 ~ direct, data = elite_df)
)

options(modelsummary_panel_labels = "roman")
options(modelsummary_get = "broom")
options(modelsummary_format_numeric_latex = "mathmode")

latex_code <- modelsummary(models,
             title = "Direct elections do not predict establishment",
             stars = TRUE,
             coef_rename = c("direct" = "Direct election"),
             gof_omit = 'DF|AIC|BIC|RMSE|Std.Errors',
             escape = FALSE,
             output = "latex",
             note = "Regression of various outcomes on the whether the village council had a direct election, with robust standard errors."
)

latex_string <- as.character(latex_code)

writeLines(latex_string, con = "../results/tables/tablec2.tex")

# Table C.3: Joint prediction of direct election

model1 <- lm_robust(direct ~ 
                      number_villages_GP + 
                      total_population_gp + 
                      overall_percent_SC_gp + 
                      km_from_block_office + 
                      total_time_from_block_office +
                      speed_to_block_office + 
                      pilgrimage_sites +
                      historic_sites + 
                      economic_opportunities + 
                      all_villages_electricity_grid + 
                      reserved_women + 
                      reserved_obc + 
                      reserved_sc, data = elite_df)

options(modelsummary_panel_labels = "roman")
options(modelsummary_get = "broom")
options(modelsummary_format_numeric_latex = "mathmode")

latex_code <- modelsummary(model1,
             title = "Joint prediction of direct election",
             stars = TRUE,
             coef_rename = c("number_villages_GP" = "No. villages", 
                             "km_from_block_office" = "Distance to block (km)",
                             "total_time_from_block_office" = "Time to block (hr)",
                             "speed_to_block_office" = "Speed to block (km/hr)",
                             "pilgrimage_sites" = "Pilgrimage sites",
                             "historic_sites" = "Historic sites",
                             "economic_opportunities" = "Economic opportunities",
                             "all_villages_electricity_grid" = "Electricity grid",
                             "reserved_women" = "Gender quota",
                             "reserved_obc" = "OBC caste quota",
                             "reserved_sc" = "SC caste quota",
                             "total_population_gp" = "Population",
                             "overall_percent_SC_gp" = "Percent SC pop."),
             gof_function = get_f_stat_pvalue,
             gof_omit = 'DF|AIC|BIC|RMSE|Std.Errors',
             coef_omit = "Intercept",
             escape = FALSE,
             note = "Regression of a binary indicator for direct election on covariates with robust standard errors. Open caste variable left out as reference category.",
             output = "latex"
)

latex_string <- as.character(latex_code)

writeLines(latex_string, con = "../results/tables/tablec3.tex")


# Table C.4: Joint prognosis of covariates

# merge datasets to use covariates in citizen dataset analyses

df1_subset <- elite_df %>%
  select(r_villageid, 
         number_villages_GP, 
         total_population_gp, 
         overall_percent_SC_gp, 
         km_from_block_office, 
         total_time_from_block_office,
         speed_to_block_office, 
         pilgrimage_sites,
         historic_sites, 
         economic_opportunities, 
         all_villages_electricity_grid, 
         reserved_women, 
         reserved_obc, 
         reserved_sc,
         reserved_open,
         sarpanch_election_year)

merged_citizen_df <- left_join(citizen_df, df1_subset, by = "r_villageid")

# subset to potential outcomes under control 

controlgroupelite <- elite_df %>% filter(direct == 0)

controlgroupcitizen <- merged_citizen_df %>% filter(direct == 0)

modelsprognosis <- list(
  
  "President self-perception as main decision-maker in monthly meeting" = lm_robust(i_decide_masik_sabha ~ number_villages_GP +
                                                                                      total_population_gp + 
                                                                                      overall_percent_SC_gp +
                                                                                      km_from_block_office + 
                                                                                      total_time_from_block_office + 
                                                                                      speed_to_block_office +
                                                                                      pilgrimage_sites +
                                                                                      historic_sites +
                                                                                      economic_opportunities +
                                                                                      all_villages_electricity_grid +
                                                                                      reserved_women +
                                                                                      reserved_obc + 
                                                                                      reserved_sc,
                                                                                    data = controlgroupelite),
  
  "President percevied as most influential actor in discussion" = lm_robust(most_influential_gd ~ number_villages_GP + 
                                                                              total_population_gp +
                                                                              overall_percent_SC_gp +
                                                                              km_from_block_office + 
                                                                              total_time_from_block_office + 
                                                                              speed_to_block_office +
                                                                              pilgrimage_sites +
                                                                              historic_sites +
                                                                              economic_opportunities +
                                                                              all_villages_electricity_grid +
                                                                              reserved_women +
                                                                              reserved_obc + 
                                                                              reserved_sc,
                                                                            data = controlgroupelite),
  
  "Proportion of time president spoke in discussion" = lm_robust(prop_speakingtime_sarpanch ~ number_villages_GP + 
                                                                   total_population_gp +
                                                                   overall_percent_SC_gp +
                                                                   km_from_block_office + 
                                                                   total_time_from_block_office + 
                                                                   speed_to_block_office +
                                                                   pilgrimage_sites +
                                                                   historic_sites +
                                                                   economic_opportunities +
                                                                   all_villages_electricity_grid +
                                                                   reserved_women +
                                                                   reserved_obc + 
                                                                   reserved_sc,
                                                                 data = controlgroupelite),
  
  "Citizen nominated president as village representative to bureaucrat" = lm_robust(sarpanch_nominate_BDO ~ number_villages_GP + 
                                                                                      total_population_gp + 
                                                                                      overall_percent_SC_gp +
                                                                                      km_from_block_office + 
                                                                                      total_time_from_block_office + 
                                                                                      speed_to_block_office +
                                                                                      pilgrimage_sites +
                                                                                      historic_sites +
                                                                                      economic_opportunities +
                                                                                      all_villages_electricity_grid +
                                                                                      reserved_women +
                                                                                      reserved_obc + 
                                                                                      reserved_sc,
                                                                                    data = controlgroupcitizen,
                                                                                    clusters = r_villageid),
  
  "Citizen reports president leads village events" = lm_robust(sarpanch_inauguralevents ~ number_villages_GP + 
                                                                 total_population_gp +
                                                                 overall_percent_SC_gp +
                                                                 km_from_block_office + 
                                                                 total_time_from_block_office + 
                                                                 speed_to_block_office +
                                                                 pilgrimage_sites +
                                                                 historic_sites +
                                                                 economic_opportunities +
                                                                 all_villages_electricity_grid +
                                                                 reserved_women +
                                                                 reserved_obc + 
                                                                 reserved_sc,
                                                               data = controlgroupcitizen,
                                                               clusters = r_villageid))

options(modelsummary_panel_labels = "roman")
options(modelsummary_get = "broom")
options(modelsummary_format_numeric_latex = "mathmode")

latex_code <- modelsummary(modelsprognosis,
             title = "Joint prognosis of covariates",
             stars = TRUE,
             coef_rename = c("number_villages_GP" = "No. villages", 
                             "km_from_block_office" = "Distance to block (km)",
                             "total_time_from_block_office" = "Time to block (hr)",
                             "speed_to_block_office" = "Speed to block (km/hr)",
                             "pilgrimage_sites" = "Pilgrimage sites",
                             "historic_sites" = "Historic sites",
                             "economic_opportunities" = "Economic opportunities",
                             "all_villages_electricity_grid" = "Electricity grid",
                             "reserved_women" = "Gender quota",
                             "reserved_sc" = "SC caste quota",
                             "reserved_obc" = "OBC caste quota",
                             "total_population_gp" = "Population",
                             "overall_percent_SC_gp" = "Percent SC pop."),
             gof_function = get_f_stat_pvalue,
             gof_omit = 'DF|AIC|BIC|RMSE|Std.Errors|R2 Adj.',
             coef_omit = "Intercept",
             escape = FALSE,
             notes = 'Each model is a linear regression of each main outcome on covariates, among the control group (indirectly elected councils). All regressions use robust standard errors. Citizen survey outcomes cluster SEs at the village council level.',
             output = "latex")

latex_string <- as.character(latex_code)

writeLines(latex_string, con = "../results/tables/tablec4.tex")


# Table C.5: Prognostic-weighted covariate balance tests


# NOTE TO USERS: This code uses a beta version of the package 'pwtest' by 
# Bicalho et al (2025). The version used in this paper is downloadable in the folder
# pwtest-package. The authors intend to update and make publicly accessable the 
# package soon.

# Define outcomes and run pw_test function for each
outcomes <- c("most_influential_gd", "prop_speakingtime_sarpanch", "i_decide_masik_sabha", "sarpanch_nominate_BDO", "sarpanch_inauguralevents")
elite_results <- run_pwtest(outcomes[1:3], elite_df)
citizen_results <- run_pwtest(outcomes[4:5], merged_citizen_df)

# Combine and format the results
final_results <- rbind(elite_results, citizen_results)
colnames(final_results) <- c("Outcome", "Prognostic-weighted test statistic", "Bootstrapped p-value")
rownames(final_results) <- NULL
final_results$Outcome <- c("President perceived as most influential actor in discussion", 
                           "Proportion of time president spoke in discussion",
                           "President self-perception as main decision-maker in monthly meeting",
                           "Citizen nominated president as village representative to bureaucrat",
                           "Citizen reports president leads village events")

latex_table <- xtable(final_results, digits = c(0,0,3,3), include.rownames = FALSE)

output_path <- "../results/tables/tablec5.tex"

print(latex_table, 
      file = output_path, 
      include.rownames = FALSE)


# Table D.1: The impact of direct election on de facto authority

models <- list(
  
  "President self-perception as main decision-maker in monthly meeting" = lm_robust(i_decide_masik_sabha ~ direct,
                                                                                    data = elite_df),
  
  "President percevied as most influential actor in discussion" = lm_robust(most_influential_gd ~ direct,
                                                                            data = elite_df),
  
  "Proportion of time president spoke in discussion" = lm_robust(prop_speakingtime_sarpanch ~ direct,
                                                                 data = elite_df),
  
  "Citizen nominated president as village representative to bureaucrat" = lm_robust(sarpanch_nominate_BDO ~ direct,
                                                                                    data = citizen_df,
                                                                                    clusters = r_villageid),
  
  "Citizen reports president leads village events" = lm_robust(sarpanch_inauguralevents ~ direct,
                                                               data = citizen_df,
                                                               clusters = r_villageid))


options(modelsummary_panel_labels = "roman")
options(modelsummary_get = "broom")
options(modelsummary_format_numeric_latex = "mathmode")

latex_code <- modelsummary(models,
             title = "The impact of direct election on de facto authority",
             stars = TRUE,
             coef_rename = c("direct" = "Direct election"),
             gof_omit = 'DF|AIC|BIC|RMSE|Std.Errors|R2 Adj.',
             escape = FALSE,
             notes = 'Each model is a linear regression of each main outcome on direct election. All regressions use robust standard errors. Outcomes from the citizen survey use cluster robust standard errors at the council level.',
             output = "latex")

latex_string <- as.character(latex_code)

writeLines(latex_string, con = "../results/tables/tabled1.tex")


# Table D.2: The impact of direct election on de facto authority, by transition

## create period variables

elite_df <- elite_df %>%
  mutate(
    
    period = case_when(sarpanch_election_year <= 2017 & direct == 0 ~ 1,
                       sarpanch_election_year >= 2017 & sarpanch_election_year <= 2020 & 
                         direct == 1 ~ 2,
                       sarpanch_election_year >= 2020 & direct == 0 ~ 3)
  )

merged_citizen_df <- merged_citizen_df %>%
  mutate(
    
    period = case_when(sarpanch_election_year <= 2017 & direct == 0 ~ 1,
                       sarpanch_election_year >= 2017 & sarpanch_election_year <= 2020 & 
                         direct == 1 ~ 2,
                       sarpanch_election_year >= 2020 & direct == 0 ~ 3)
  )

models <- list(
  
  "President self-perception as main decision-maker in monthly meeting" = list(
    "Impact of direct: first transition" = lm_robust(i_decide_masik_sabha ~ direct,
                                                     data = elite_df %>% filter(period == 1 | period == 2)),
    "Impact of direct: second transition" = lm_robust(i_decide_masik_sabha ~ direct,
                                                      data = elite_df %>% filter(period == 2 | period == 3))
  ),
  
  "President perceived as most influential actor in discussion" = list(
    "Impact of direct: first transition" = lm_robust(most_influential_gd ~ direct,
                                                     data = elite_df %>% filter(period == 1 | period == 2)),
    "Impact of direct: second transition" = lm_robust(most_influential_gd ~ direct,
                                                      data = elite_df %>% filter(period == 2 | period == 3))
  ),
  "Proportion of time president spoke in discussion" = list(
    "Impact of direct: first transition" = lm_robust(prop_speakingtime_sarpanch ~ direct,
                                                     data = elite_df %>% filter(period == 1 | period == 2)),
    "Impact of direct: second transition" = lm_robust(prop_speakingtime_sarpanch ~ direct,
                                                      data = elite_df %>% filter(period == 2 | period == 3))
  ),
  "Citizen nominated president as village representative to bureaucrat" = list(
    "Impact of direct: first transition" = lm_robust(sarpanch_nominate_BDO ~ direct,
                                                     data = merged_citizen_df %>% filter(period == 1 | period == 2),
                                                     clusters = r_villageid),
    "Impact of direct: second transition" = lm_robust(sarpanch_nominate_BDO ~ direct,
                                                      data = merged_citizen_df %>% filter(period == 2 | period == 3),
                                                      clusters = r_villageid)
  ),
  "Citizen reports president leads village events" = list(
    "Impact of direct: first transition" = lm_robust(sarpanch_inauguralevents ~ direct,
                                                     data = merged_citizen_df %>% filter(period == 1 | period == 2),
                                                     clusters = r_villageid),
    "Impact of direct: second transition" = lm_robust(sarpanch_inauguralevents ~ direct,
                                                      data = merged_citizen_df %>% filter(period == 2 | period == 3),
                                                      clusters = r_villageid)
  )
  
) 

options(modelsummary_panel_labels = "roman")
options(modelsummary_get = "broom")
options(modelsummary_format_numeric_latex = "mathmode")

latex_code <- modelsummary(models,
             shape = "rbind",
             title = "The impact of direct election on de facto authority, by transition",
             stars = TRUE,
             coef_rename = c("direct" = "Direct election"),
             gof_omit = 'DF|AIC|BIC|RMSE|Std.Errors|R2 Adj.',
             escape = FALSE,
             notes = 'Each model is a linear regression of each main outcome on direct election. The left column does so for the first transition, and the right column for the second transition. All regressions use robust standard errors. Outcomes from the citizen survey use cluster robust standard errors at the council level.',
             output = "latex")

latex_string <- as.character(latex_code)

writeLines(latex_string, con = "../results/tables/tabled2.tex")


# Table D.3: The impact of direct election on de facto authority, by quotas

models <- list(
  
  "President self-perception as main decision-maker in monthly meeting" = list(
    
    "Gender-reserved seats" = lm_robust(i_decide_masik_sabha ~ direct,
                                        data = elite_df %>% filter(reserved_women == 1)),
    
    "OBC/Open seats" = lm_robust(i_decide_masik_sabha ~ direct,
                                 data = elite_df %>% filter(reserved_obc == 1 |
                                                              reserved_open == 1)),
    
    "SC-reserved seats" = lm_robust(i_decide_masik_sabha ~ direct,
                                    data = elite_df %>% filter(reserved_sc == 1))
    
  ),
  
  "President perceived as most influential actor in discussion" = list(
    "Gender-reserved seats" = lm_robust(most_influential_gd ~ direct,
                                        data = elite_df %>% filter(reserved_women == 1)),
    
    "OBC/Open seats" = lm_robust(most_influential_gd ~ direct,
                                 data = elite_df %>% filter(reserved_obc == 1 |
                                                              reserved_open == 1)),
    
    "SC-reserved seats" = lm_robust(most_influential_gd ~ direct,
                                    data = elite_df %>% filter(reserved_sc == 1))
    
  ),
  "Proportion of time president spoke in discussion" = list(
    "Gender-reserved seats" = lm_robust(prop_speakingtime_sarpanch ~ direct,
                                        data = elite_df %>% filter(reserved_women == 1)),
    
    "OBC/Open seats" = lm_robust(prop_speakingtime_sarpanch ~ direct,
                                 data = elite_df %>% filter(reserved_obc == 1 |
                                                              reserved_open == 1)),
    
    "SC-reserved seats" = lm_robust(prop_speakingtime_sarpanch ~ direct,
                                    data = elite_df %>% filter(reserved_sc == 1))
  ),
  "Citizen nominated president as village representative to bureaucrat" = list(
    "Gender-reserved seats" = lm_robust(sarpanch_nominate_BDO ~ direct,
                                        data = merged_citizen_df %>% filter(reserved_women == 1),
                                        clusters = r_villageid),
    
    "OBC/Open seats" = lm_robust(sarpanch_nominate_BDO ~ direct,
                                 data = merged_citizen_df %>% filter(reserved_obc == 1 |
                                                                reserved_open == 1),
                                 clusters = r_villageid),
    
    "SC-reserved seats" = lm_robust(sarpanch_nominate_BDO ~ direct,
                                    data = merged_citizen_df %>% filter(reserved_sc == 1),
                                    clusters = r_villageid)
  ),
  "Citizen reports president leads village events" = list(
    "Gender-reserved seats" = lm_robust(sarpanch_inauguralevents ~ direct,
                                        data = merged_citizen_df %>% filter(reserved_women == 1),
                                        clusters = r_villageid),
    
    "OBC/Open seats" = lm_robust(sarpanch_inauguralevents ~ direct,
                                 data = merged_citizen_df %>% filter(reserved_obc == 1 |
                                                                reserved_open == 1),
                                 clusters = r_villageid),
    
    "SC-reserved seats" = lm_robust(sarpanch_inauguralevents ~ direct,
                                    data = merged_citizen_df %>% filter(reserved_sc == 1),
                                    clusters = r_villageid)
  )
  
) 

options(modelsummary_panel_labels = "roman")
options(modelsummary_get = "broom")
options(modelsummary_format_numeric_latex = "mathmode")

latex_code <- modelsummary(models,
             shape = "rbind",
             title = "The impact of direct election on de facto authority, by quotas",
             stars = TRUE,
             coef_rename = c("direct" = "Direct election"),
             gof_omit = 'DF|AIC|BIC|RMSE|Std.Errors|R2 Adj.',
             escape = FALSE,
             notes = 'Each model is a linear regression of each main outcome on direct election. The left column does so among gender-reserved seats, the middle column for OBC/Open seats, and the right column for SC-reserved seats. All regressions use robust standard errors. Outcomes from the citizen survey use cluster robust standard errors at the council level.',
             output = "latex")

latex_string <- as.character(latex_code)

writeLines(latex_string, con = "../results/tables/tabled3.tex")


# Table D.4: Benchmarking the impact of direct election on de facto authority

models <- list(
  
  "President self-perception as main decision-maker in monthly meeting" = list(
    "Impact of direct" = lm_robust(i_decide_masik_sabha ~ direct,
                                   data = elite_df),
    
    "Impact of gender quota" = lm_robust(i_decide_masik_sabha ~ reserved_women,
                                         data = elite_df),
    
    "Impact of SC quota" = lm_robust(i_decide_masik_sabha ~ reserved_sc,
                                     data = elite_df)),
  
  "President perceived as most influential actor in discussion" = list(
    "Impact of direct" = lm_robust(most_influential_gd ~ direct,
                                   data = elite_df),
    
    "Impact of gender quota" = lm_robust(most_influential_gd ~ reserved_women,
                                         data = elite_df),
    
    "Impact of SC quota" = lm_robust(most_influential_gd ~ reserved_sc,
                                     data = elite_df)),
  "Proportion of time president spoke in discussion" = list(
    "Impact of direct" = lm_robust(prop_speakingtime_sarpanch ~ direct,
                                   data = elite_df),
    
    "Impact of gender quota" = lm_robust(prop_speakingtime_sarpanch ~ reserved_women,
                                         data = elite_df),
    
    "Impact of SC quota" = lm_robust(prop_speakingtime_sarpanch ~ reserved_sc,
                                     data = elite_df)),
  "Citizen nominated president as village representative to bureaucrat" = list(
    "Impact of direct" = lm_robust(sarpanch_nominate_BDO ~ direct,
                                   data = merged_citizen_df, clusters = r_villageid),

    "Impact of gender quota" = lm_robust(sarpanch_nominate_BDO ~ reserved_women,
                                         data = merged_citizen_df, clusters = r_villageid),
    
    "Impact of SC quota" = lm_robust(sarpanch_nominate_BDO ~ reserved_sc,
                                     data = merged_citizen_df, clusters = r_villageid)),
  "Citizen reports president leads village events" = list(
    "Impact of direct" = lm_robust(sarpanch_inauguralevents ~ direct,
                                   data = merged_citizen_df, clusters = r_villageid),
    
    "Impact of gender quota" = lm_robust(sarpanch_inauguralevents ~ reserved_women,
                                         data = merged_citizen_df, clusters = r_villageid),
    
    "Impact of SC quota" = lm_robust(sarpanch_inauguralevents ~ reserved_sc,
                                     data = merged_citizen_df, clusters = r_villageid))
) 

options(modelsummary_panel_labels = "roman")
options(modelsummary_get = "broom")
options(modelsummary_format_numeric_latex = "mathmode")

latex_code <- modelsummary(models,
             shape = "rbind",
             title = "Benchmarking the impact of direct election on de facto authority",
             stars = TRUE,
             coef_rename = c("direct" = "Direct election",
                             "reserved_sc" = "SC quota",
                             "reserved_women" = "Gender quota"),
             gof_omit = 'DF|AIC|BIC|RMSE|Std.Errors|R2 Adj.',
             escape = FALSE,
             notes = 'Each model is a linear regression of each main outcome on either direct election, gender quota, or SC quota. All regressions use robust standard errors. Outcomes from the citizen survey use cluster robust standard errors at the council level.',
             output = "latex")


latex_string <- as.character(latex_code)

writeLines(latex_string, con = "../results/tables/tabled4.tex")


# Table D.5: The impact of direct election on de facto authority, controlling for observables

models <- list(
  
  "President self-perception as main decision-maker in monthly meeting" = lm_robust(i_decide_masik_sabha ~ direct + 
                                                                                      number_villages_GP + 
                                                                                      total_population_gp +
                                                                                      overall_percent_SC_gp +
                                                                                      km_from_block_office + 
                                                                                      total_time_from_block_office + 
                                                                                      speed_to_block_office +
                                                                                      pilgrimage_sites +
                                                                                      historic_sites +
                                                                                      economic_opportunities +
                                                                                      all_villages_electricity_grid +
                                                                                      reserved_women +
                                                                                      reserved_obc + 
                                                                                      reserved_sc,
                                                                                    data = elite_df),
  
  "President percevied as most influential actor in discussion" = lm_robust(most_influential_gd ~ direct + 
                                                                              number_villages_GP + 
                                                                              total_population_gp +
                                                                              overall_percent_SC_gp +
                                                                              km_from_block_office + 
                                                                              total_time_from_block_office + 
                                                                              speed_to_block_office +
                                                                              pilgrimage_sites +
                                                                              historic_sites +
                                                                              economic_opportunities +
                                                                              all_villages_electricity_grid +
                                                                              reserved_women +
                                                                              reserved_obc + 
                                                                              reserved_sc,
                                                                            data = elite_df),
  
  "Proportion of time president spoke in discussion" = lm_robust(prop_speakingtime_sarpanch ~ direct + 
                                                                   number_villages_GP + 
                                                                   total_population_gp +
                                                                   overall_percent_SC_gp +
                                                                   km_from_block_office + 
                                                                   total_time_from_block_office + 
                                                                   speed_to_block_office +
                                                                   pilgrimage_sites +
                                                                   historic_sites +
                                                                   economic_opportunities +
                                                                   all_villages_electricity_grid +
                                                                   reserved_women +
                                                                   reserved_obc + 
                                                                   reserved_sc,
                                                                 data = elite_df),
  
  "Citizen nominated president as village representative to bureaucrat" = lm_robust(sarpanch_nominate_BDO ~ direct + 
                                                                                      number_villages_GP + 
                                                                                      total_population_gp +
                                                                                      overall_percent_SC_gp +
                                                                                      km_from_block_office + 
                                                                                      total_time_from_block_office + 
                                                                                      speed_to_block_office +
                                                                                      pilgrimage_sites +
                                                                                      historic_sites +
                                                                                      economic_opportunities +
                                                                                      all_villages_electricity_grid +
                                                                                      reserved_women +
                                                                                      reserved_obc + 
                                                                                      reserved_sc,
                                                                                    data = merged_citizen_df,
                                                                                    clusters = r_villageid),
  
  "Citizen reports president leads village events" = lm_robust(sarpanch_inauguralevents ~ direct + 
                                                                 number_villages_GP + 
                                                                 total_population_gp +
                                                                 overall_percent_SC_gp +
                                                                 km_from_block_office + 
                                                                 total_time_from_block_office + 
                                                                 speed_to_block_office +
                                                                 pilgrimage_sites +
                                                                 historic_sites +
                                                                 economic_opportunities +
                                                                 all_villages_electricity_grid +
                                                                 reserved_women +
                                                                 reserved_obc + 
                                                                 reserved_sc,
                                                               data = merged_citizen_df,
                                                               clusters = r_villageid))


options(modelsummary_panel_labels = "roman")
options(modelsummary_get = "broom")
options(modelsummary_format_numeric_latex = "mathmode")

latex_code <- modelsummary(models,
             title = "The impact of direct election on de facto authority, controlling for observables",
             stars = TRUE,
             coef_rename = c("direct" = "Direct election",
                             "number_villages_GP" = "No. villages",
                             "total_population_gp" = "Population",
                             "overall_percent_SC_gp" = "Percent SC pop.",
                             "km_from_block_office" = "Distance to block (km)",
                             "total_time_from_block_office" = "Time to block (hr)",
                             "speed_to_block_office" = "Speed to block (km/hr)",
                             "pilgrimage_sites" = "Pilgrimage sites",
                             "historic_sites" = "Historic sites",
                             "economic_opportunities" = "Economic opportunities", 
                             "all_villages_electricity_grid" = "Electricity grid",
                             "reserved_women" = "Gender quota",
                             "reserved_obc" = "OBC caste quota",
                             "reserved_sc" = "SC caste quota"),
             gof_omit = 'DF|AIC|BIC|RMSE|Std.Errors|R2 Adj.',
             escape = FALSE,
             notes = 'Each model is a linear regression of each main outcome on direct election and the covariates from Figure 3. All regressions use robust standard errors. Outcomes from the citizen survey use cluster robust standard errors at the council level.',
             output = "latex")

latex_string <- as.character(latex_code)

writeLines(latex_string, con = "../results/tables/tabled5.tex")


# Table D.6: The impact of direct election on de facto authority, by transition (with controls)

models <- list(
  
  "President self-perception as main decision-maker in monthly meeting" = list(
    "Impact of direct: first transition" = lm_robust(i_decide_masik_sabha ~ direct + 
                                                       number_villages_GP + 
                                                       total_population_gp +
                                                       overall_percent_SC_gp +
                                                       km_from_block_office + 
                                                       total_time_from_block_office + 
                                                       speed_to_block_office +
                                                       pilgrimage_sites +
                                                       historic_sites +
                                                       economic_opportunities +
                                                       all_villages_electricity_grid +
                                                       reserved_women +
                                                       reserved_obc + 
                                                       reserved_sc,
                                                     data = elite_df %>% filter(period == 1 | period == 2)),
    "Impact of direct: second transition" = lm_robust(i_decide_masik_sabha ~ direct + 
                                                        number_villages_GP + 
                                                        total_population_gp +
                                                        overall_percent_SC_gp +
                                                        km_from_block_office + 
                                                        total_time_from_block_office + 
                                                        speed_to_block_office +
                                                        pilgrimage_sites +
                                                        historic_sites +
                                                        economic_opportunities +
                                                        all_villages_electricity_grid +
                                                        reserved_women +
                                                        reserved_obc + 
                                                        reserved_sc,
                                                      data = elite_df %>% filter(period == 2 | period == 3))
  ),
  
  "President perceived as most influential actor in discussion" = list(
    "Impact of direct: first transition" = lm_robust(most_influential_gd ~ direct + 
                                                       number_villages_GP + 
                                                       total_population_gp +
                                                       overall_percent_SC_gp +
                                                       km_from_block_office + 
                                                       total_time_from_block_office + 
                                                       speed_to_block_office +
                                                       pilgrimage_sites +
                                                       historic_sites +
                                                       economic_opportunities +
                                                       all_villages_electricity_grid +
                                                       reserved_women +
                                                       reserved_obc + 
                                                       reserved_sc,
                                                     data = elite_df %>% filter(period == 1 | period == 2)),
    "Impact of direct: second transition" = lm_robust(most_influential_gd ~ direct + 
                                                        number_villages_GP + 
                                                        total_population_gp +
                                                        overall_percent_SC_gp +
                                                        km_from_block_office + 
                                                        total_time_from_block_office + 
                                                        speed_to_block_office +
                                                        pilgrimage_sites +
                                                        historic_sites +
                                                        economic_opportunities +
                                                        all_villages_electricity_grid +
                                                        reserved_women +
                                                        reserved_obc + 
                                                        reserved_sc,
                                                      data = elite_df %>% filter(period == 2 | period == 3))
  ),
  "Proportion of time president spoke in discussion" = list(
    "Impact of direct: first transition" = lm_robust(prop_speakingtime_sarpanch ~ direct + 
                                                       number_villages_GP + 
                                                       total_population_gp +
                                                       overall_percent_SC_gp +
                                                       km_from_block_office + 
                                                       total_time_from_block_office + 
                                                       speed_to_block_office +
                                                       pilgrimage_sites +
                                                       historic_sites +
                                                       economic_opportunities +
                                                       all_villages_electricity_grid +
                                                       reserved_women +
                                                       reserved_obc + 
                                                       reserved_sc,
                                                     data = elite_df %>% filter(period == 1 | period == 2)),
    "Impact of direct: second transition" = lm_robust(prop_speakingtime_sarpanch ~ direct + 
                                                        number_villages_GP + 
                                                        total_population_gp +
                                                        overall_percent_SC_gp +
                                                        km_from_block_office + 
                                                        total_time_from_block_office + 
                                                        speed_to_block_office +
                                                        pilgrimage_sites +
                                                        historic_sites +
                                                        economic_opportunities +
                                                        all_villages_electricity_grid +
                                                        reserved_women +
                                                        reserved_obc + 
                                                        reserved_sc,
                                                      data = elite_df %>% filter(period == 2 | period == 3))
  ),
  "Citizen nominated president as village representative to bureaucrat" = list(
    "Impact of direct: first transition" = lm_robust(sarpanch_nominate_BDO ~ direct + 
                                                       number_villages_GP + 
                                                       total_population_gp +
                                                       overall_percent_SC_gp +
                                                       km_from_block_office + 
                                                       total_time_from_block_office + 
                                                       speed_to_block_office +
                                                       pilgrimage_sites +
                                                       historic_sites +
                                                       economic_opportunities +
                                                       all_villages_electricity_grid +
                                                       reserved_women +
                                                       reserved_obc + 
                                                       reserved_sc,
                                                     data = merged_citizen_df %>% filter(period == 1 | period == 2),
                                                     clusters = r_villageid),
    "Impact of direct: second transition" = lm_robust(sarpanch_nominate_BDO ~ direct + 
                                                        number_villages_GP + 
                                                        total_population_gp +
                                                        overall_percent_SC_gp +
                                                        km_from_block_office + 
                                                        total_time_from_block_office + 
                                                        speed_to_block_office +
                                                        pilgrimage_sites +
                                                        historic_sites +
                                                        economic_opportunities +
                                                        all_villages_electricity_grid +
                                                        reserved_women +
                                                        reserved_obc + 
                                                        reserved_sc,
                                                      data = merged_citizen_df %>% filter(period == 2 | period == 3),
                                                      clusters = r_villageid)
  ),
  "Citizen reports president leads village events" = list(
    "Impact of direct: first transition" = lm_robust(sarpanch_inauguralevents ~ direct + 
                                                       number_villages_GP + 
                                                       total_population_gp +
                                                       overall_percent_SC_gp +
                                                       km_from_block_office + 
                                                       total_time_from_block_office + 
                                                       speed_to_block_office +
                                                       pilgrimage_sites +
                                                       historic_sites +
                                                       economic_opportunities +
                                                       all_villages_electricity_grid +
                                                       reserved_women +
                                                       reserved_obc + 
                                                       reserved_sc,
                                                     data = merged_citizen_df %>% filter(period == 1 | period == 2),
                                                     clusters = r_villageid),
    "Impact of direct: second transition" = lm_robust(sarpanch_inauguralevents ~ direct + 
                                                        number_villages_GP + 
                                                        total_population_gp +
                                                        overall_percent_SC_gp +
                                                        km_from_block_office + 
                                                        total_time_from_block_office + 
                                                        speed_to_block_office +
                                                        pilgrimage_sites +
                                                        historic_sites +
                                                        economic_opportunities +
                                                        all_villages_electricity_grid +
                                                        reserved_women +
                                                        reserved_obc + 
                                                        reserved_sc,
                                                      data = merged_citizen_df %>% filter(period == 2 | period == 3),
                                                      clusters = r_villageid)
  )
  
) 

options(modelsummary_panel_labels = "roman")
options(modelsummary_get = "broom")
options(modelsummary_format_numeric_latex = "mathmode")

latex_code <- modelsummary(models,
             shape = "cbind",
             title = "The impact of direct election on de facto authority, by transition (with controls)",
             stars = TRUE,
             coef_rename = c("direct" = "Direct election",
                             "number_villages_GP" = "No. villages",
                             "total_population_gp" = "Population",
                             "overall_percent_SC_gp" = "Percent SC pop.",
                             "km_from_block_office" = "Distance to block (km)",
                             "total_time_from_block_office" = "Time to block (hr)",
                             "speed_to_block_office" = "Speed to block (km/hr)",
                             "pilgrimage_sites" = "Pilgrimage sites",
                             "historic_sites" = "Historic sites",
                             "economic_opportunities" = "Economic opportunities", 
                             "all_villages_electricity_grid" = "Electricity grid",
                             "reserved_women" = "Gender quota",
                             "reserved_obc" = "OBC caste quota",
                             "reserved_sc" = "SC caste quota"),
             gof_omit = 'DF|AIC|BIC|RMSE|Std.Errors|R2 Adj.',
             escape = FALSE,
             notes = 'Each model is a linear regression of each main outcome on direct election and controls. For each outcome, the left column does so for the first transition, and the right column for the second transition. All regressions use robust standard errors. Outcomes from the citizen survey use cluster robust standard errors at the council level.',
             output = "latex")

latex_string <- as.character(latex_code)

writeLines(latex_string, con = "../results/tables/tabled6.tex")


# Figure D.1: Sensitivity of t-value to unobservables, de facto authority outcomes

model1 <- lm(i_decide_masik_sabha ~ direct +
               number_villages_GP + 
               total_population_gp +
               overall_percent_SC_gp +
               km_from_block_office + 
               total_time_from_block_office + 
               speed_to_block_office +
               pilgrimage_sites +
               historic_sites +
               economic_opportunities +
               all_villages_electricity_grid +
               reserved_women +
               reserved_obc + 
               reserved_sc, data = elite_df)

model2 <- lm(most_influential_gd ~ direct +
               number_villages_GP + 
               total_population_gp +
               overall_percent_SC_gp +
               km_from_block_office + 
               total_time_from_block_office + 
               speed_to_block_office +
               pilgrimage_sites +
               historic_sites +
               economic_opportunities +
               all_villages_electricity_grid +
               reserved_women +
               reserved_obc + 
               reserved_sc, data = elite_df)

model3 <- lm(prop_speakingtime_sarpanch ~ direct +
               number_villages_GP + 
               total_population_gp +
               overall_percent_SC_gp +
               km_from_block_office + 
               total_time_from_block_office + 
               speed_to_block_office +
               pilgrimage_sites +
               historic_sites +
               economic_opportunities +
               all_villages_electricity_grid +
               reserved_women +
               reserved_obc + 
               reserved_sc, data = elite_df)

model4 <- lm(sarpanch_nominate_BDO ~ direct +
               number_villages_GP + 
               total_population_gp +
               overall_percent_SC_gp +
               km_from_block_office + 
               total_time_from_block_office + 
               speed_to_block_office +
               pilgrimage_sites +
               historic_sites +
               economic_opportunities +
               all_villages_electricity_grid +
               reserved_women +
               reserved_obc + 
               reserved_sc, data = merged_citizen_df)

model5 <- lm(sarpanch_inauguralevents ~ direct +
               number_villages_GP + 
               total_population_gp +
               overall_percent_SC_gp +
               km_from_block_office + 
               total_time_from_block_office + 
               speed_to_block_office +
               pilgrimage_sites +
               historic_sites +
               economic_opportunities +
               all_villages_electricity_grid +
               reserved_women +
               reserved_obc + 
               reserved_sc, data = merged_citizen_df)

sensitivity_1 <- sensemakr(model = model1, 
                           treatment = "direct",
                           benchmark_covariates = "reserved_women",
                           kd = 1:3,
                           ky = 1:3, 
                           q = 1,
                           alpha = 0.05, 
                           reduce = TRUE)

sensitivity_2 <- sensemakr(model = model2, 
                           treatment = "direct",
                           benchmark_covariates = "reserved_women",
                           kd = 1:3,
                           ky = 1:3, 
                           q = 1,
                           alpha = 0.05, 
                           reduce = TRUE)

sensitivity_3 <- sensemakr(model = model3, 
                           treatment = "direct",
                           benchmark_covariates = "reserved_women",
                           kd = 1:3,
                           ky = 1:3, 
                           q = 1,
                           alpha = 0.05, 
                           reduce = TRUE)

sensitivity_4 <- sensemakr(model = model4, 
                           treatment = "direct",
                           benchmark_covariates = "reserved_women",
                           kd = 1:3,
                           ky = 1:3, 
                           q = 1,
                           alpha = 0.05, 
                           reduce = TRUE)

sensitivity_5 <- sensemakr(model = model5, 
                           treatment = "direct",
                           benchmark_covariates = "reserved_women",
                           kd = 1:3,
                           ky = 1:3, 
                           q = 1,
                           alpha = 0.05, 
                           reduce = TRUE)

# Arrange the plots in a 2x3 grid layout
plot_1 <- as.ggplot(~ plot(sensitivity_1, sensitivity.of = "t-value", main = "Model 1"))
plot_2 <- as.ggplot(~ plot(sensitivity_2, sensitivity.of = "t-value", main = "Model 2"))
plot_3 <- as.ggplot(~ plot(sensitivity_3, sensitivity.of = "t-value", main = "Model 3"))
plot_4 <- as.ggplot(~ plot(sensitivity_4, sensitivity.of = "t-value", main = "Model 4"))
plot_5 <- as.ggplot(~ plot(sensitivity_5, sensitivity.of = "t-value", main = "Model 5"))

combined_plot <- grid.arrange(plot_1, plot_2, plot_3, plot_4, plot_5, ncol = 2)

ggsave("../results/figures/figured1.png", combined_plot, width = 10, height = 12)


# Table D.7: The impact of direct election on formal elite capture

models <- list(
  
  "Formal capture measures" = list(
    
    "President Maratha" = lm_robust(sarpanch_maratha ~ direct, data = elite_df),
    
    "Proportion land held by president's caste in village" = lm_robust(avg_prop_land_held_sarpanch_caste ~ direct,
                                                                       data = elite_df),
    
    "President owns pacca house" = lm_robust(pacca_house ~ direct, data = elite_df),
    
    "President owns three/four-wheeler" = lm_robust(three_four_wheeler ~ direct, data = elite_df),
    
    "President landed" = lm_robust(sarpanch_land_own_name ~ direct, data = elite_df),
    
    "President male" = lm_robust(sarpanch_male ~ direct, data = elite_df)
  ))

options(modelsummary_panel_labels = "roman")
options(modelsummary_get = "broom")
options(modelsummary_format_numeric_latex = "mathmode")

latex_code <- modelsummary(models,
             title = "The impact of direct election on formal elite capture",
             shape = "cbind",
             stars = TRUE,
             coef_rename = c("direct" = "Direct election"),
             gof_omit = 'DF|AIC|BIC|RMSE|Std.Errors|R2 Adj.',
             escape = FALSE,
             notes = 'Each model is a linear regression of each formal capture outcome on direct election. All regressions use robust standard errors. Outcomes from the citizen survey use cluster robust standard errors at the council level.',
             output = "latex")

latex_string <- as.character(latex_code)

writeLines(latex_string, con = "../results/tables/tabled7.tex")


# Table D.8: The impact of direct election on informal elite capture

models <- list(
  "Informal capture measures" = list(
    
    "Citizen reports former president/other villager leads village events" = lm_robust(formersarpanch_otherperson_inauguralevents ~ direct, data = citizen_df,
                                                                                       clusters = r_villageid),
    
    "Citizen nominated former president/other villager as village representative to bureaucrat" = lm_robust(other_nominate_BDO ~ direct,
                                                                                                            data = citizen_df,
                                                                                                            clusters = r_villageid),
    
    "President reports rich landowner influences council" = lm_robust(rich_landowner_influences_gp ~ direct, data = elite_df),
    
    "Resignation filed: Administrative data" = lm_robust(resigned ~ direct, data = admin_df),
    
    "Citizen reports resignation" = lm_robust(resignation_reported ~ direct, data = citizen_df,
                                              clusters = r_villageid),
    
    "President reports resignation" = lm_robust(resignation ~ direct, data = elite_df),
    
    "President elected unopposed" = lm_robust(sarpanch_uncontested ~ direct, data = elite_df)
    
  )
  
)

options(modelsummary_panel_labels = "roman")
options(modelsummary_get = "broom")
options(modelsummary_format_numeric_latex = "mathmode")

latex_code <- modelsummary(models,
                           title = "The impact of direct election on informal elite capture",
                           shape = "cbind",
                           stars = TRUE,
                           coef_rename = c("direct" = "Direct election"),
                           gof_omit = 'DF|AIC|BIC|RMSE|Std.Errors|R2 Adj.',
                           escape = FALSE,
                           notes = 'Each model is a linear regression of each informal capture outcome on direct election. All regressions use robust standard errors. Outcomes from the citizen survey use cluster robust standard errors at the council level.',
                           output = "latex")

latex_string <- as.character(latex_code)

writeLines(latex_string, con = "../results/tables/tabled8.tex")


# Table D.9: The impact of direct election on formal elite capture, by transition

models <- list(
  
  "President Maratha" = list(
    "Impact of direct: first transition (no controls)" = lm_robust(sarpanch_maratha ~ direct,
                                                                   data = elite_df %>% filter(period == 1 | period == 2)),
    "Impact of direct: first transition (controls)" = lm_robust(sarpanch_maratha ~ direct  +
                                                                  number_villages_GP + 
                                                                  total_population_gp +
                                                                  overall_percent_SC_gp +
                                                                  km_from_block_office + 
                                                                  total_time_from_block_office + 
                                                                  speed_to_block_office +
                                                                  pilgrimage_sites +
                                                                  historic_sites +
                                                                  economic_opportunities +
                                                                  all_villages_electricity_grid +
                                                                  reserved_women +
                                                                  reserved_obc + 
                                                                  reserved_sc,
                                                                data = elite_df %>% filter(period == 1 | period == 2)),
    "Impact of direct: second transition (no controls)" = lm_robust(sarpanch_maratha ~ direct,
                                                                    data = elite_df %>% filter(period == 2 | period == 3)),
    "Impact of direct: second transition (controls)" = lm_robust(sarpanch_maratha ~ direct  +
                                                                   number_villages_GP + 
                                                                   total_population_gp +
                                                                   overall_percent_SC_gp +
                                                                   km_from_block_office + 
                                                                   total_time_from_block_office + 
                                                                   speed_to_block_office +
                                                                   pilgrimage_sites +
                                                                   historic_sites +
                                                                   economic_opportunities +
                                                                   all_villages_electricity_grid +
                                                                   reserved_women +
                                                                   reserved_obc + 
                                                                   reserved_sc,
                                                                 data = elite_df %>% filter(period == 2 | period == 3))
  ),
  
  "Proportion land held by president's caste in village" = list(
    "Impact of direct: first transition (no controls)" = lm_robust(avg_prop_land_held_sarpanch_caste ~ direct,
                                                                   data = elite_df %>% filter(period == 1 | period == 2)),
    "Impact of direct: first transition (controls)" = lm_robust(avg_prop_land_held_sarpanch_caste ~ direct +
                                                                  number_villages_GP + 
                                                                  total_population_gp +
                                                                  overall_percent_SC_gp +
                                                                  km_from_block_office + 
                                                                  total_time_from_block_office + 
                                                                  speed_to_block_office +
                                                                  pilgrimage_sites +
                                                                  historic_sites +
                                                                  economic_opportunities +
                                                                  all_villages_electricity_grid +
                                                                  reserved_women +
                                                                  reserved_obc + 
                                                                  reserved_sc,
                                                                data = elite_df %>% filter(period == 1 | period == 2)),
    "Impact of direct: second transition (no controls)" = lm_robust(avg_prop_land_held_sarpanch_caste ~ direct,
                                                                    data = elite_df %>% filter(period == 2 | period == 3)),
    "Impact of direct: second transition (controls)" = lm_robust(avg_prop_land_held_sarpanch_caste ~ direct +
                                                                   number_villages_GP + 
                                                                   total_population_gp +
                                                                   overall_percent_SC_gp +
                                                                   km_from_block_office + 
                                                                   total_time_from_block_office + 
                                                                   speed_to_block_office +
                                                                   pilgrimage_sites +
                                                                   historic_sites +
                                                                   economic_opportunities +
                                                                   all_villages_electricity_grid +
                                                                   reserved_women +
                                                                   reserved_obc + 
                                                                   reserved_sc,
                                                                 data = elite_df %>% filter(period == 2 | period == 3))
  ),
  "President owns pacca house" = list(
    "Impact of direct: first transition (no controls)" = lm_robust(pacca_house ~ direct,
                                                                   data = elite_df %>% filter(period == 1 | period == 2)),
    "Impact of direct: first transition (controls)" = lm_robust(pacca_house ~ direct +
                                                                  number_villages_GP + 
                                                                  total_population_gp +
                                                                  overall_percent_SC_gp +
                                                                  km_from_block_office + 
                                                                  total_time_from_block_office + 
                                                                  speed_to_block_office +
                                                                  pilgrimage_sites +
                                                                  historic_sites +
                                                                  economic_opportunities +
                                                                  all_villages_electricity_grid +
                                                                  reserved_women +
                                                                  reserved_obc + 
                                                                  reserved_sc,
                                                                data = elite_df %>% filter(period == 1 | period == 2)),
    "Impact of direct: second transition (no controls)" = lm_robust(pacca_house ~ direct,
                                                                    data = elite_df %>% filter(period == 2 | period == 3)),
    "Impact of direct: second transition (controls)" = lm_robust(pacca_house ~ direct +
                                                                   number_villages_GP + 
                                                                   total_population_gp +
                                                                   overall_percent_SC_gp +
                                                                   km_from_block_office + 
                                                                   total_time_from_block_office + 
                                                                   speed_to_block_office +
                                                                   pilgrimage_sites +
                                                                   historic_sites +
                                                                   economic_opportunities +
                                                                   all_villages_electricity_grid +
                                                                   reserved_women +
                                                                   reserved_obc + 
                                                                   reserved_sc,
                                                                 data = elite_df %>% filter(period == 2 | period == 3))
  ),
  "President owns three/four wheeler" = list(
    "Impact of direct: first transition (no controls)" = lm_robust(three_four_wheeler ~ direct,
                                                                   data = elite_df %>% filter(period == 1 | period == 2)),
    "Impact of direct: first transition (controls)" = lm_robust(three_four_wheeler ~ direct +
                                                                  number_villages_GP + 
                                                                  total_population_gp +
                                                                  overall_percent_SC_gp +
                                                                  km_from_block_office + 
                                                                  total_time_from_block_office + 
                                                                  speed_to_block_office +
                                                                  pilgrimage_sites +
                                                                  historic_sites +
                                                                  economic_opportunities +
                                                                  all_villages_electricity_grid +
                                                                  reserved_women +
                                                                  reserved_obc + 
                                                                  reserved_sc,
                                                                data = elite_df %>% filter(period == 1 | period == 2)),
    "Impact of direct: second transition (no controls)" = lm_robust(three_four_wheeler ~ direct ,
                                                                    data = elite_df %>% filter(period == 2 | period == 3)),
    "Impact of direct: second transition (controls)" = lm_robust(three_four_wheeler ~ direct +
                                                                   number_villages_GP + 
                                                                   total_population_gp +
                                                                   overall_percent_SC_gp +
                                                                   km_from_block_office + 
                                                                   total_time_from_block_office + 
                                                                   speed_to_block_office +
                                                                   pilgrimage_sites +
                                                                   historic_sites +
                                                                   economic_opportunities +
                                                                   all_villages_electricity_grid +
                                                                   reserved_women +
                                                                   reserved_obc + 
                                                                   reserved_sc,
                                                                 data = elite_df %>% filter(period == 2 | period == 3))
  ),
  "President landed" = list(
    "Impact of direct: first transition (no controls)" = lm_robust(sarpanch_land_own_name ~ direct,
                                                                   data = elite_df %>% filter(period == 1 | period == 2)),
    "Impact of direct: first transition (controls)" = lm_robust(sarpanch_land_own_name ~ direct +
                                                                  number_villages_GP + 
                                                                  total_population_gp +
                                                                  overall_percent_SC_gp +
                                                                  km_from_block_office + 
                                                                  total_time_from_block_office + 
                                                                  speed_to_block_office +
                                                                  pilgrimage_sites +
                                                                  historic_sites +
                                                                  economic_opportunities +
                                                                  all_villages_electricity_grid +
                                                                  reserved_women +
                                                                  reserved_obc + 
                                                                  reserved_sc,
                                                                data = elite_df %>% filter(period == 1 | period == 2)),
    "Impact of direct: second transition (no controls)" = lm_robust(sarpanch_land_own_name ~ direct,
                                                                    data = elite_df %>% filter(period == 2 | period == 3)),
    "Impact of direct: second transition (controls)" = lm_robust(sarpanch_land_own_name ~ direct +
                                                                   number_villages_GP + 
                                                                   total_population_gp +
                                                                   overall_percent_SC_gp +
                                                                   km_from_block_office + 
                                                                   total_time_from_block_office + 
                                                                   speed_to_block_office +
                                                                   pilgrimage_sites +
                                                                   historic_sites +
                                                                   economic_opportunities +
                                                                   all_villages_electricity_grid +
                                                                   reserved_women +
                                                                   reserved_obc + 
                                                                   reserved_sc,
                                                                 data = elite_df %>% filter(period == 2 | period == 3))
  ),
  
  "President male" = list(
    "Impact of direct: first transition (no controls)" = lm_robust(sarpanch_male ~ direct,
                                                                   data = elite_df %>% filter(period == 1 | period == 2)),
    "Impact of direct: first transition (controls)" = lm_robust(sarpanch_male ~ direct +
                                                                  number_villages_GP + 
                                                                  total_population_gp +
                                                                  overall_percent_SC_gp +
                                                                  km_from_block_office + 
                                                                  total_time_from_block_office + 
                                                                  speed_to_block_office +
                                                                  pilgrimage_sites +
                                                                  historic_sites +
                                                                  economic_opportunities +
                                                                  all_villages_electricity_grid +
                                                                  reserved_women +
                                                                  reserved_obc + 
                                                                  reserved_sc,
                                                                data = elite_df %>% filter(period == 1 | period == 2)),
    "Impact of direct: second transition (no controls)" = lm_robust(sarpanch_male ~ direct,
                                                                    data = elite_df %>% filter(period == 2 | period == 3)),
    "Impact of direct: second transition (controls)" = lm_robust(sarpanch_male ~ direct +
                                                                   number_villages_GP + 
                                                                   total_population_gp +
                                                                   overall_percent_SC_gp +
                                                                   km_from_block_office + 
                                                                   total_time_from_block_office + 
                                                                   speed_to_block_office +
                                                                   pilgrimage_sites +
                                                                   historic_sites +
                                                                   economic_opportunities +
                                                                   all_villages_electricity_grid +
                                                                   reserved_women +
                                                                   reserved_obc + 
                                                                   reserved_sc,
                                                                 data = elite_df %>% filter(period == 2 | period == 3))
    
  )
)


options(modelsummary_panel_labels = "roman")
options(modelsummary_get = "broom")
options(modelsummary_format_numeric_latex = "mathmode")

latex_code <- modelsummary(models,
             shape = "rbind",
             title = "The impact of direct election on formal elite capture, by transition",
             stars = TRUE,
             coef_rename = c("direct" = "Direct election"),
             coef_omit = c(3:15),
             gof_omit = 'DF|AIC|BIC|RMSE|Std.Errors|R2 Adj.',
             escape = FALSE,
             notes = 'Each model is a linear regression of each formal capture outcome on direct election. The left column does so for the first transition, and the right column for the second transition. All regressions use robust standard errors. Outcomes from the citizen survey use cluster robust standard errors at the council level.',
             output = "latex")

latex_string <- as.character(latex_code)

writeLines(latex_string, con = "../results/tables/tabled9.tex")


# Table D.10: The impact of direct election on informal elite capture, by transition


models <- list(
  
  "Citizen reports former president/other villager leads village events" = list(
    "Impact of direct: first transition (no controls)" = lm_robust(formersarpanch_otherperson_inauguralevents ~ direct,
                                                                   data = merged_citizen_df %>% filter(period == 1 | period == 2)),
    "Impact of direct: first transition (controls)" = lm_robust(formersarpanch_otherperson_inauguralevents ~ direct  +
                                                                  number_villages_GP + 
                                                                  total_population_gp +
                                                                  overall_percent_SC_gp +
                                                                  km_from_block_office + 
                                                                  total_time_from_block_office + 
                                                                  speed_to_block_office +
                                                                  pilgrimage_sites +
                                                                  historic_sites +
                                                                  economic_opportunities +
                                                                  all_villages_electricity_grid +
                                                                  reserved_women +
                                                                  reserved_obc + 
                                                                  reserved_sc,
                                                                data = merged_citizen_df %>% filter(period == 1 | period == 2),
                                                                clusters = r_villageid),
    "Impact of direct: second transition (no controls)" = lm_robust(formersarpanch_otherperson_inauguralevents ~ direct,
                                                                    data = merged_citizen_df %>% filter(period == 2 | period == 3),
                                                                    clusters = r_villageid),
    "Impact of direct: second transition (controls)" = lm_robust(formersarpanch_otherperson_inauguralevents ~ direct  +
                                                                   number_villages_GP + 
                                                                   total_population_gp +
                                                                   overall_percent_SC_gp +
                                                                   km_from_block_office + 
                                                                   total_time_from_block_office + 
                                                                   speed_to_block_office +
                                                                   pilgrimage_sites +
                                                                   historic_sites +
                                                                   economic_opportunities +
                                                                   all_villages_electricity_grid +
                                                                   reserved_women +
                                                                   reserved_obc + 
                                                                   reserved_sc,
                                                                 data = merged_citizen_df %>% filter(period == 2 | period == 3),
                                                                 clusters = r_villageid)
  ),
  
  "Citizen nominated former president/other villager as village representative to bureaucrat" = list(
    "Impact of direct: first transition (no controls)" = lm_robust(other_nominate_BDO ~ direct,
                                                                   data = merged_citizen_df %>% filter(period == 1 | period == 2),
                                                                   clusters = r_villageid),
    "Impact of direct: first transition (controls)" = lm_robust(other_nominate_BDO ~ direct +
                                                                  number_villages_GP + 
                                                                  total_population_gp +
                                                                  overall_percent_SC_gp +
                                                                  km_from_block_office + 
                                                                  total_time_from_block_office + 
                                                                  speed_to_block_office +
                                                                  pilgrimage_sites +
                                                                  historic_sites +
                                                                  economic_opportunities +
                                                                  all_villages_electricity_grid +
                                                                  reserved_women +
                                                                  reserved_obc + 
                                                                  reserved_sc,
                                                                data = merged_citizen_df %>% filter(period == 1 | period == 2),
                                                                clusters = r_villageid),
    "Impact of direct: second transition (no controls)" = lm_robust(other_nominate_BDO ~ direct,
                                                                    data = merged_citizen_df %>% filter(period == 2 | period == 3),
                                                                    clusters = r_villageid),
    "Impact of direct: second transition (controls)" = lm_robust(other_nominate_BDO ~ direct +
                                                                   number_villages_GP + 
                                                                   total_population_gp +
                                                                   overall_percent_SC_gp +
                                                                   km_from_block_office + 
                                                                   total_time_from_block_office + 
                                                                   speed_to_block_office +
                                                                   pilgrimage_sites +
                                                                   historic_sites +
                                                                   economic_opportunities +
                                                                   all_villages_electricity_grid +
                                                                   reserved_women +
                                                                   reserved_obc + 
                                                                   reserved_sc,
                                                                 data = merged_citizen_df %>% filter(period == 2 | period == 3),
                                                                 clusters = r_villageid)
  ),
  "President reports rich landowner influences council" = list(
    "Impact of direct: first transition (no controls)" = lm_robust(rich_landowner_influences_gp ~ direct,
                                                                   data = elite_df %>% filter(period == 1 | period == 2)),
    "Impact of direct: first transition (controls)" = lm_robust(rich_landowner_influences_gp ~ direct +
                                                                  number_villages_GP + 
                                                                  total_population_gp +
                                                                  overall_percent_SC_gp +
                                                                  km_from_block_office + 
                                                                  total_time_from_block_office + 
                                                                  speed_to_block_office +
                                                                  pilgrimage_sites +
                                                                  historic_sites +
                                                                  economic_opportunities +
                                                                  all_villages_electricity_grid +
                                                                  reserved_women +
                                                                  reserved_obc + 
                                                                  reserved_sc,
                                                                data = elite_df %>% filter(period == 1 | period == 2)),
    "Impact of direct: second transition (no controls)" = lm_robust(rich_landowner_influences_gp ~ direct,
                                                                    data = elite_df %>% filter(period == 2 | period == 3)),
    "Impact of direct: second transition (controls)" = lm_robust(rich_landowner_influences_gp ~ direct +
                                                                   number_villages_GP + 
                                                                   total_population_gp +
                                                                   overall_percent_SC_gp +
                                                                   km_from_block_office + 
                                                                   total_time_from_block_office + 
                                                                   speed_to_block_office +
                                                                   pilgrimage_sites +
                                                                   historic_sites +
                                                                   economic_opportunities +
                                                                   all_villages_electricity_grid +
                                                                   reserved_women +
                                                                   reserved_obc + 
                                                                   reserved_sc,
                                                                 data = elite_df %>% filter(period == 2 | period == 3))
  ),
  
  "Citizen reports resignation" = list(
    "Impact of direct: first transition (no controls)" = lm_robust(resignation_reported ~ direct,
                                                                   data = merged_citizen_df %>% filter(period == 1 | period == 2),
                                                                   clusters = r_villageid),
    "Impact of direct: first transition (controls)" = lm_robust(resignation_reported ~ direct +
                                                                  number_villages_GP + 
                                                                  total_population_gp +
                                                                  overall_percent_SC_gp +
                                                                  km_from_block_office + 
                                                                  total_time_from_block_office + 
                                                                  speed_to_block_office +
                                                                  pilgrimage_sites +
                                                                  historic_sites +
                                                                  economic_opportunities +
                                                                  all_villages_electricity_grid +
                                                                  reserved_women +
                                                                  reserved_obc + 
                                                                  reserved_sc,
                                                                data = merged_citizen_df %>% filter(period == 1 | period == 2),
                                                                clusters = r_villageid),
    "Impact of direct: second transition (no controls)" = lm_robust(resignation_reported ~ direct,
                                                                    data = merged_citizen_df %>% filter(period == 2 | period == 3),
                                                                    clusters = r_villageid),
    "Impact of direct: second transition (controls)" = lm_robust(resignation_reported ~ direct +
                                                                   number_villages_GP + 
                                                                   total_population_gp +
                                                                   overall_percent_SC_gp +
                                                                   km_from_block_office + 
                                                                   total_time_from_block_office + 
                                                                   speed_to_block_office +
                                                                   pilgrimage_sites +
                                                                   historic_sites +
                                                                   economic_opportunities +
                                                                   all_villages_electricity_grid +
                                                                   reserved_women +
                                                                   reserved_obc + 
                                                                   reserved_sc,
                                                                 data = merged_citizen_df %>% filter(period == 2 | period == 3),
                                                                 clusters = r_villageid)
  ),
  
  "President reports resignation" = list(
    "Impact of direct: first transition (no controls)" = lm_robust(resignation ~ direct,
                                                                   data = elite_df %>% filter(period == 1 | period == 2)),
    "Impact of direct: first transition (controls)" = lm_robust(resignation ~ direct +
                                                                  number_villages_GP + 
                                                                  total_population_gp +
                                                                  overall_percent_SC_gp +
                                                                  km_from_block_office + 
                                                                  total_time_from_block_office + 
                                                                  speed_to_block_office +
                                                                  pilgrimage_sites +
                                                                  historic_sites +
                                                                  economic_opportunities +
                                                                  all_villages_electricity_grid +
                                                                  reserved_women +
                                                                  reserved_obc + 
                                                                  reserved_sc,
                                                                data = elite_df %>% filter(period == 1 | period == 2)),
    "Impact of direct: second transition (no controls)" = lm_robust(resignation ~ direct,
                                                                    data = elite_df %>% filter(period == 2 | period == 3)),
    "Impact of direct: second transition (controls)" = lm_robust(resignation ~ direct +
                                                                   number_villages_GP + 
                                                                   total_population_gp +
                                                                   overall_percent_SC_gp +
                                                                   km_from_block_office + 
                                                                   total_time_from_block_office + 
                                                                   speed_to_block_office +
                                                                   pilgrimage_sites +
                                                                   historic_sites +
                                                                   economic_opportunities +
                                                                   all_villages_electricity_grid +
                                                                   reserved_women +
                                                                   reserved_obc + 
                                                                   reserved_sc,
                                                                 data = elite_df %>% filter(period == 2 | period == 3))
  ),
  
  "President elected unopposed" = list(
    "Impact of direct: first transition (no controls)" = lm_robust(sarpanch_uncontested ~ direct,
                                                                   data = elite_df %>% filter(period == 1 | period == 2)),
    "Impact of direct: first transition (controls)" = lm_robust(sarpanch_uncontested ~ direct +
                                                                  number_villages_GP + 
                                                                  total_population_gp +
                                                                  overall_percent_SC_gp +
                                                                  km_from_block_office + 
                                                                  total_time_from_block_office + 
                                                                  speed_to_block_office +
                                                                  pilgrimage_sites +
                                                                  historic_sites +
                                                                  economic_opportunities +
                                                                  all_villages_electricity_grid +
                                                                  reserved_women +
                                                                  reserved_obc + 
                                                                  reserved_sc,
                                                                data = elite_df %>% filter(period == 1 | period == 2)),
    "Impact of direct: second transition (no controls)" = lm_robust(sarpanch_uncontested ~ direct,
                                                                    data = elite_df %>% filter(period == 2 | period == 3)),
    "Impact of direct: second transition (controls)" = lm_robust(sarpanch_uncontested ~ direct +
                                                                   number_villages_GP + 
                                                                   total_population_gp +
                                                                   overall_percent_SC_gp +
                                                                   km_from_block_office + 
                                                                   total_time_from_block_office + 
                                                                   speed_to_block_office +
                                                                   pilgrimage_sites +
                                                                   historic_sites +
                                                                   economic_opportunities +
                                                                   all_villages_electricity_grid +
                                                                   reserved_women +
                                                                   reserved_obc + 
                                                                   reserved_sc,
                                                                 data = elite_df %>% filter(period == 2 | period == 3)))
)

options(modelsummary_panel_labels = "roman")
options(modelsummary_get = "broom")
options(modelsummary_format_numeric_latex = "mathmode")

latex_code <- modelsummary(models,
                           shape = "rbind",
                           title = "The impact of direct election on informal elite capture, by transition",
                           stars = TRUE,
                           coef_rename = c("direct" = "Direct election"),
                           coef_omit = c(3:15),
                           gof_omit = 'DF|AIC|BIC|RMSE|Std.Errors|R2 Adj.',
                           escape = FALSE,
                           notes = 'Each model is a linear regression of each informal capture outcome on direct election (Except the administrative record outcomes). The left column does so for the first transition, and the right column for the second transition. All regressions use robust standard errors. Outcomes from the citizen survey use cluster robust standard errors at the council level.',
                           output = "latex")

latex_string <- as.character(latex_code)

writeLines(latex_string, con = "../results/tables/tabled10.tex")


# Table D.11: The impact of direct election on formal capture, by quotas


models <- list(
  
  "President Maratha" = list(
    
    "Gender-reserved seats" = lm_robust(sarpanch_maratha ~ direct,
                                        data = elite_df %>% filter(reserved_women == 1)),
    
    "OBC/Open seats" = lm_robust(sarpanch_maratha ~ direct,
                                 data = elite_df %>% filter(reserved_obc == 1 |
                                                              reserved_open == 1)),
    
    "SC-reserved seats" = lm_robust(sarpanch_maratha ~ direct,
                                    data = elite_df %>% filter(reserved_sc == 1))
    
  ),
  
  "Proportion land held by president's caste in village" = list(
    "Gender-reserved seats" = lm_robust(avg_prop_land_held_sarpanch_caste ~ direct,
                                        data = elite_df %>% filter(reserved_women == 1)),
    
    "OBC/Open seats" = lm_robust(avg_prop_land_held_sarpanch_caste ~ direct,
                                 data = elite_df %>% filter(reserved_obc == 1 |
                                                              reserved_open == 1)),
    
    "SC-reserved seats" = lm_robust(avg_prop_land_held_sarpanch_caste ~ direct,
                                    data = elite_df %>% filter(reserved_sc == 1))
    
  ),
  "President owns pacca house" = list(
    "Gender-reserved seats" = lm_robust(pacca_house ~ direct,
                                        data = elite_df %>% filter(reserved_women == 1)),
    
    "OBC/Open seats" = lm_robust(pacca_house ~ direct,
                                 data = elite_df %>% filter(reserved_obc == 1 |
                                                              reserved_open == 1)),
    
    "SC-reserved seats" = lm_robust(pacca_house ~ direct,
                                    data = elite_df %>% filter(reserved_sc == 1))
  ),
  "President owns three/four-wheeler" = list(
    "Gender-reserved seats" = lm_robust(three_four_wheeler ~ direct,
                                        data = elite_df %>% filter(reserved_women == 1)),
    
    "OBC/Open seats" = lm_robust(three_four_wheeler ~ direct,
                                 data = elite_df %>% filter(reserved_obc == 1 |
                                                              reserved_open == 1)),
    
    "SC-reserved seats" = lm_robust(three_four_wheeler ~ direct,
                                    data = elite_df %>% filter(reserved_sc == 1))
  ),
  "President landed" = list(
    "Gender-reserved seats" = lm_robust(sarpanch_land_own_name ~ direct,
                                        data = elite_df %>% filter(reserved_women == 1)),
    
    "OBC/Open seats" = lm_robust(sarpanch_land_own_name ~ direct,
                                 data = elite_df %>% filter(reserved_obc == 1 |
                                                              reserved_open == 1)),
    
    "SC-reserved seats" = lm_robust(sarpanch_land_own_name ~ direct,
                                    data = elite_df %>% filter(reserved_sc == 1))
  ),
  
  "President male" = list(
    "Gender-reserved seats" = lm_robust(sarpanch_male ~ direct,
                                        data = elite_df %>% filter(reserved_women == 1)),
    
    "OBC/Open seats" = lm_robust(sarpanch_male ~ direct,
                                 data = elite_df %>% filter(reserved_obc == 1 |
                                                              reserved_open == 1)),
    
    "SC-reserved seats" = lm_robust(sarpanch_male ~ direct,
                                    data = elite_df %>% filter(reserved_sc == 1))
  )
  
)

options(modelsummary_panel_labels = "roman")
options(modelsummary_get = "broom")
options(modelsummary_format_numeric_latex = "mathmode")

latex_code <- modelsummary(models,
             shape = "rbind",
             title = "The impact of direct election on formal capture, by quotas",
             stars = TRUE,
             coef_rename = c("direct" = "Direct election"),
             gof_omit = 'DF|AIC|BIC|RMSE|Std.Errors|R2 Adj.',
             escape = FALSE,
             notes = 'Each model is a linear regression of each formal capture outcome on direct election. The left column does so among gender-reserved seats, the middle column for OBC/Open seats, and the right column for SC-reserved seats. All regressions use robust standard errors. Outcomes from the citizen survey use cluster robust standard errors at the council level.',
             output = "latex")

latex_string <- as.character(latex_code)

writeLines(latex_string, con = "../results/tables/tabled11.tex")


# Table D.12: The impact of direct election on informal capture, by quotas


models <- list(
  
  "Citizen reports former president/other villager leads village events" = list(
    
    "Gender-reserved seats" = lm_robust(formersarpanch_otherperson_inauguralevents ~ direct,
                                        data = merged_citizen_df %>% filter(reserved_women == 1),
                                        clusters = r_villageid),
    
    "OBC/Open seats" = lm_robust(formersarpanch_otherperson_inauguralevents ~ direct,
                                 data = merged_citizen_df %>% filter(reserved_obc == 1 |
                                                                reserved_open == 1),
                                 clusters = r_villageid),
    
    "SC-reserved seats" = lm_robust(formersarpanch_otherperson_inauguralevents ~ direct,
                                    data = merged_citizen_df %>% filter(reserved_sc == 1),
                                    clusters = r_villageid)
    
  ),
  
  "Citizen nominated former president/other villager as village representative to bureaucrat" = list(
    "Gender-reserved seats" = lm_robust(other_nominate_BDO ~ direct,
                                        data = merged_citizen_df %>% filter(reserved_women == 1),
                                        clusters = r_villageid),
    
    "OBC/Open seats" = lm_robust(other_nominate_BDO ~ direct,
                                 data = merged_citizen_df %>% filter(reserved_obc == 1 |
                                                                reserved_open == 1),
                                 clusters = r_villageid),
    
    "SC-reserved seats" = lm_robust(other_nominate_BDO ~ direct,
                                    data = merged_citizen_df %>% filter(reserved_sc == 1),
                                    clusters = r_villageid)
    
  ),
  "President reports rich landowner influences council" = list(
    "Gender-reserved seats" = lm_robust(rich_landowner_influences_gp ~ direct,
                                        data = elite_df %>% filter(reserved_women == 1)),
    
    "OBC/Open seats" = lm_robust(rich_landowner_influences_gp ~ direct,
                                 data = elite_df %>% filter(reserved_obc == 1 |
                                                              reserved_open == 1)),
    
    "SC-reserved seats" = lm_robust(rich_landowner_influences_gp ~ direct,
                                    data = elite_df %>% filter(reserved_sc == 1))
  ),
  "Resignation filed: Administrative data" = list(
    "Gender-reserved seats" = lm_robust(resigned ~ direct,
                                        data = admin_df %>% filter(reserved_women == 1)),
    
    "OBC/Open seats" = lm_robust(resigned ~ direct,
                                 data = admin_df %>% filter(reserved_obc == 1 |
                                                              reserved_open == 1)),
    
    "SC-reserved seats" = lm_robust(resigned ~ direct,
                                    data = admin_df %>% filter(reserved_sc == 1))
  ),
  
  "Citizen reports resignation" = list(
    "Gender-reserved seats" = lm_robust(resignation_reported ~ direct,
                                        data = merged_citizen_df %>% filter(reserved_women == 1),
                                        clusters = r_villageid),
    
    "OBC/Open seats" = lm_robust(resignation_reported ~ direct,
                                 data = merged_citizen_df %>% filter(reserved_obc == 1 |
                                                                reserved_open == 1),
                                 clusters = r_villageid),
    
    "SC-reserved seats" = lm_robust(resignation_reported ~ direct,
                                    data = merged_citizen_df %>% filter(reserved_sc == 1),
                                    clusters = r_villageid)
  ),
  
  "President reports resignation" = list(
    "Gender-reserved seats" = lm_robust(resignation ~ direct,
                                        data = elite_df %>% filter(reserved_women == 1)),
    
    "OBC/Open seats" = lm_robust(resignation ~ direct,
                                 data = elite_df %>% filter(reserved_obc == 1 |
                                                              reserved_open == 1)),
    
    "SC-reserved seats" = lm_robust(resignation ~ direct,
                                    data = elite_df %>% filter(reserved_sc == 1))
  ),
  
  "President elected unopposed" = list(
    "Gender-reserved seats" = lm_robust(sarpanch_uncontested ~ direct,
                                        data = elite_df %>% filter(reserved_women == 1)),
    
    "OBC/Open seats" = lm_robust(sarpanch_uncontested ~ direct,
                                 data = elite_df %>% filter(reserved_obc == 1 |
                                                              reserved_open == 1)),
    
    "SC-reserved seats" = lm_robust(sarpanch_uncontested ~ direct,
                                    data = elite_df %>% filter(reserved_sc == 1))
  )
  
)


latex_code <- modelsummary(models,
             shape = "rbind",
             title = "The impact of direct election on informal capture, by quotas",
             stars = TRUE,
             coef_rename = c("direct" = "Direct election"),
             gof_omit = 'DF|AIC|BIC|RMSE|Std.Errors|R2 Adj.',
             escape = FALSE,
             notes = 'Each model is a linear regression of each informal capture outcome on direct election. The left column does so among gender-reserved seats, the middle column for OBC/Open seats, and the right column for SC-reserved seats. All regressions use robust standard errors. Outcomes from the citizen survey use cluster robust standard errors at the council level.',
             output = "latex")

latex_string <- as.character(latex_code)

writeLines(latex_string, con = "../results/tables/tabled12.tex")


# Table D.13: Benchmarking the impact of direct election on formal capture

models <- list(
  
  "President Maratha" = list(
    "Impact of direct" = lm_robust(sarpanch_maratha ~ direct,
                                   data = elite_df),
    
    "Impact of gender quota" = lm_robust(sarpanch_maratha ~ reserved_women,
                                         data = elite_df),
    
    "Impact of SC quota" = lm_robust(sarpanch_maratha ~ reserved_sc,
                                     data = elite_df)),
  
  "Proportion land held by president's caste in village" = list(
    "Impact of direct" = lm_robust(avg_prop_land_held_sarpanch_caste ~ direct,
                                   data = elite_df),
    
    "Impact of gender quota" = lm_robust(avg_prop_land_held_sarpanch_caste ~ reserved_women,
                                         data = elite_df),
    
    "Impact of SC quota" = lm_robust(avg_prop_land_held_sarpanch_caste ~ reserved_sc,
                                     data = elite_df)),
  
  "President owns pacca house" = list(
    "Impact of direct" = lm_robust(pacca_house ~ direct,
                                   data = elite_df),
    
    "Impact of gender quota" = lm_robust(pacca_house ~ reserved_women,
                                         data = elite_df),
    
    "Impact of SC quota" = lm_robust(pacca_house ~ reserved_sc,
                                     data = elite_df)),
  
  "President owns three/four-wheeler" = list(
    "Impact of direct" = lm_robust(three_four_wheeler ~ direct,
                                   data = elite_df),
    
    "Impact of gender quota" = lm_robust(three_four_wheeler ~ reserved_women,
                                         data = elite_df),
    
    "Impact of SC quota" = lm_robust(three_four_wheeler ~ reserved_sc,
                                     data = elite_df)),
  
  "President landed" = list(
    
    "Impact of direct" = lm_robust(sarpanch_land_own_name ~ direct,
                                   data = elite_df),
    
    "Impact of gender quota" = lm_robust(sarpanch_land_own_name ~ reserved_women,
                                         data = elite_df),
    
    "Impact of SC quota" = lm_robust(sarpanch_land_own_name ~ reserved_sc,
                                     data = elite_df)),
  
  "President male" = list(
    
    "Impact of direct" = lm_robust(sarpanch_male ~ direct,
                                   data = elite_df),
    
    "Impact of gender quota" = lm_robust(sarpanch_male ~ reserved_women,
                                         data = elite_df),
    
    "Impact of SC quota" = lm_robust(sarpanch_male ~ reserved_sc,
                                     data = elite_df))
)

options(modelsummary_panel_labels = "roman")
options(modelsummary_get = "broom")
options(modelsummary_format_numeric_latex = "mathmode")

latex_code <- modelsummary(models,
             shape = "rbind",
             title = "Benchmarking the impact of direct election on formal capture",
             stars = TRUE,
             coef_rename = c("direct" = "Direct election",
                             "reserved_sc" = "SC quota",
                             "reserved_women" = "Gender quota"),
             gof_omit = 'DF|AIC|BIC|RMSE|Std.Errors|R2 Adj.',
             escape = FALSE,
             notes = 'Each model is a linear regression of each formal capture outcome on either direct election, gender quota, or SC quota. All regressions use robust standard errors. Outcomes from the citizen survey use cluster robust standard errors at the council level.',
             output = "latex")

latex_string <- as.character(latex_code)

writeLines(latex_string, con = "../results/tables/tabled13.tex")


# Table D.14: Benchmarking the impact of direct election on informal capture


models <- list(
  
  "Citizen reports former president/other villager leads village events" = list(
    "Impact of direct" = lm_robust(formersarpanch_otherperson_inauguralevents ~ direct,
                                   data = merged_citizen_df,
                                   clusters = r_villageid),
    
    "Impact of gender quota" = lm_robust(formersarpanch_otherperson_inauguralevents ~ reserved_women,
                                         data = merged_citizen_df,
                                         clusters = r_villageid),
    
    "Impact of SC quota" = lm_robust(formersarpanch_otherperson_inauguralevents ~ reserved_sc,
                                     data = merged_citizen_df,
                                     clusters = r_villageid)),
  
  "Citizen nominated former president/other villager as village representative to bureaucrat" = list(
    "Impact of direct" = lm_robust(other_nominate_BDO ~ direct,
                                   data = merged_citizen_df,
                                   clusters = r_villageid),
    
    "Impact of gender quota" = lm_robust(other_nominate_BDO ~ reserved_women,
                                         data = merged_citizen_df,
                                         clusters = r_villageid),
    
    "Impact of SC quota" = lm_robust(other_nominate_BDO ~ reserved_sc,
                                     data = merged_citizen_df,
                                     clusters = r_villageid)),
  
  "President reports rich landowner influences council" = list(
    "Impact of direct" = lm_robust(rich_landowner_influences_gp ~ direct,
                                   data = elite_df),
    
    "Impact of gender quota" = lm_robust(rich_landowner_influences_gp ~ reserved_women,
                                         data = elite_df),
    
    "Impact of SC quota" = lm_robust(rich_landowner_influences_gp ~ reserved_sc,
                                     data = elite_df)),
  
  "Resignation filed: Administrative data" = list(
    "Impact of direct" = lm_robust(resigned ~ direct,
                                   data = admin_df),
    
    "Impact of gender quota" = lm_robust(resigned ~ reserved_women,
                                         data = admin_df),
    
    "Impact of SC quota" = lm_robust(resigned ~ reserved_sc,
                                     data = admin_df)),
  
  "Citizen reports resignation" = list(
    "Impact of direct" = lm_robust(resignation_reported ~ direct,
                                   data = merged_citizen_df,
                                   clusters = r_villageid),
    
    "Impact of gender quota" = lm_robust(resignation_reported ~ reserved_women,
                                         data = merged_citizen_df,
                                         clusters = r_villageid),
    
    "Impact of SC quota" = lm_robust(resignation_reported ~ reserved_sc,
                                     data = merged_citizen_df,
                                     clusters = r_villageid)),
  
  "President reports resignation" = list(
    "Impact of direct" = lm_robust(resignation ~ direct,
                                   data = elite_df),
    
    "Impact of gender quota" = lm_robust(resignation ~ reserved_women,
                                         data = elite_df),
    
    "Impact of SC quota" = lm_robust(resignation ~ reserved_sc,
                                     data = elite_df)),
  
  "President elected unopposed" = list(
    
    "Impact of direct" = lm_robust(sarpanch_uncontested ~ direct,
                                   data = elite_df),
    
    "Impact of gender quota" = lm_robust(sarpanch_uncontested ~ reserved_women,
                                         data = elite_df),
    
    "Impact of SC quota" = lm_robust(sarpanch_uncontested ~ reserved_sc,
                                     data = elite_df))
) 

latex_code <- modelsummary(models,
             shape = "rbind",
             title = "Benchmarking the impact of direct election on informal capture",
             stars = TRUE,
             coef_rename = c("direct" = "Direct election",
                             "reserved_sc" = "SC quota",
                             "reserved_women" = "Gender quota"),
             gof_omit = 'DF|AIC|BIC|RMSE|Std.Errors|R2 Adj.',
             escape = FALSE,
             notes = 'Each model is a linear regression of each informal capture outcome on either direct election, gender quota, or SC quota. All regressions use robust standard errors. Outcomes from the citizen survey use cluster robust standard errors at the council level.',
             output = "latex")

latex_string <- as.character(latex_code)

writeLines(latex_string, con = "../results/tables/tabled14.tex")


# Table D.15: The impact of direct election on formal elite capture, with controls

models <- list(
  
  "Formal capture measures" = list(
    
    "President Maratha" = lm_robust(sarpanch_maratha ~ direct + 
                                      number_villages_GP + 
                                      total_population_gp +
                                      overall_percent_SC_gp +
                                      km_from_block_office + 
                                      total_time_from_block_office + 
                                      speed_to_block_office +
                                      pilgrimage_sites +
                                      historic_sites +
                                      economic_opportunities +
                                      all_villages_electricity_grid +
                                      reserved_women +
                                      reserved_obc + 
                                      reserved_sc, data = elite_df),
    
    "Proportion land held by president's caste in village" = lm_robust(avg_prop_land_held_sarpanch_caste ~ direct + 
                                                                         number_villages_GP + 
                                                                         total_population_gp +
                                                                         overall_percent_SC_gp +
                                                                         km_from_block_office + 
                                                                         total_time_from_block_office + 
                                                                         speed_to_block_office +
                                                                         pilgrimage_sites +
                                                                         historic_sites +
                                                                         economic_opportunities +
                                                                         all_villages_electricity_grid +
                                                                         reserved_women +
                                                                         reserved_obc + 
                                                                         reserved_sc,
                                                                       data = elite_df),
    
    "President owns pacca house" = lm_robust(pacca_house ~ direct + 
                                               number_villages_GP + 
                                               total_population_gp +
                                               overall_percent_SC_gp +
                                               km_from_block_office + 
                                               total_time_from_block_office + 
                                               speed_to_block_office +
                                               pilgrimage_sites +
                                               historic_sites +
                                               economic_opportunities +
                                               all_villages_electricity_grid +
                                               reserved_women +
                                               reserved_obc + 
                                               reserved_sc, data = elite_df),
    
    "President owns three/four-wheeler" = lm_robust(three_four_wheeler ~ direct + 
                                                      number_villages_GP + 
                                                      total_population_gp +
                                                      overall_percent_SC_gp +
                                                      km_from_block_office + 
                                                      total_time_from_block_office + 
                                                      speed_to_block_office +
                                                      pilgrimage_sites +
                                                      historic_sites +
                                                      economic_opportunities +
                                                      all_villages_electricity_grid +
                                                      reserved_women +
                                                      reserved_obc + 
                                                      reserved_sc, data = elite_df),
    
    "President landed" = lm_robust(sarpanch_land_own_name ~ direct + 
                                     number_villages_GP + 
                                     total_population_gp +
                                     overall_percent_SC_gp +
                                     km_from_block_office + 
                                     total_time_from_block_office + 
                                     speed_to_block_office +
                                     pilgrimage_sites +
                                     historic_sites +
                                     economic_opportunities +
                                     all_villages_electricity_grid +
                                     reserved_women +
                                     reserved_obc + 
                                     reserved_sc, data = elite_df),
    
    "President male" = lm_robust(sarpanch_male ~ direct + 
                                   number_villages_GP + 
                                   total_population_gp +
                                   overall_percent_SC_gp +
                                   km_from_block_office + 
                                   total_time_from_block_office + 
                                   speed_to_block_office +
                                   pilgrimage_sites +
                                   historic_sites +
                                   economic_opportunities +
                                   all_villages_electricity_grid +
                                   reserved_women +
                                   reserved_obc + 
                                   reserved_sc, data = elite_df)
  ))


options(modelsummary_panel_labels = "roman")
options(modelsummary_get = "broom")
options(modelsummary_format_numeric_latex = "mathmode")

latex_code <- modelsummary(models,
             title = "The impact of direct election on formal elite capture, with controls",
             shape = "cbind",
             stars = TRUE,
             coef_rename = c("direct" = "Direct election",
                             "number_villages_GP" = "No. villages",
                             "total_population_gp" = "Population",
                             "overall_percent_SC_gp" = "Percent SC pop.",
                             "km_from_block_office" = "Distance to block (km)",
                             "total_time_from_block_office" = "Time to block (hr)",
                             "speed_to_block_office" = "Speed to block (km/hr)",
                             "pilgrimage_sites" = "Pilgrimage sites",
                             "historic_sites" = "Historic sites",
                             "economic_opportunities" = "Economic opportunities", 
                             "all_villages_electricity_grid" = "Electricity grid",
                             "reserved_women" = "Gender quota",
                             "reserved_obc" = "OBC caste quota",
                             "reserved_sc" = "SC caste quota"),
             gof_omit = 'DF|AIC|BIC|RMSE|Std.Errors|R2 Adj.',
             escape = FALSE,
             notes = 'Each model is a linear regression of each formal capture outcome on direct election and controls from Figure 3 in the main paper. All regressions use robust standard errors. Outcomes from the citizen survey use cluster robust standard errors at the council level.',
             output = "latex")

latex_string <- as.character(latex_code)

writeLines(latex_string, con = "../results/tables/tabled15.tex")


# Table D.16: The impact of direct election on informal elite capture, with controls


models <- list(
  "Informal capture measures" = list(
    
    "Citizen reports former president/other villager leads village events" = lm_robust(formersarpanch_otherperson_inauguralevents ~ direct + 
                                                                                         number_villages_GP + 
                                                                                         total_population_gp +
                                                                                         overall_percent_SC_gp +
                                                                                         km_from_block_office + 
                                                                                         total_time_from_block_office + 
                                                                                         speed_to_block_office +
                                                                                         pilgrimage_sites +
                                                                                         historic_sites +
                                                                                         economic_opportunities +
                                                                                         all_villages_electricity_grid +
                                                                                         reserved_women +
                                                                                         reserved_obc + 
                                                                                         reserved_sc, data = merged_citizen_df,
                                                                                       clusters = r_villageid),
    
    "Citizen nominated former president/other villager as village representative to bureaucrat" = lm_robust(other_nominate_BDO ~ direct + 
                                                                                                              number_villages_GP + 
                                                                                                              total_population_gp +
                                                                                                              overall_percent_SC_gp +
                                                                                                              km_from_block_office + 
                                                                                                              total_time_from_block_office + 
                                                                                                              speed_to_block_office +
                                                                                                              pilgrimage_sites +
                                                                                                              historic_sites +
                                                                                                              economic_opportunities +
                                                                                                              all_villages_electricity_grid +
                                                                                                              reserved_women +
                                                                                                              reserved_obc + 
                                                                                                              reserved_sc,
                                                                                                            data = merged_citizen_df,
                                                                                                            clusters = r_villageid),
    
    "President reports rich landowner influences council" = lm_robust(rich_landowner_influences_gp ~ direct + 
                                                                        number_villages_GP + 
                                                                        total_population_gp +
                                                                        overall_percent_SC_gp +
                                                                        km_from_block_office + 
                                                                        total_time_from_block_office + 
                                                                        speed_to_block_office +
                                                                        pilgrimage_sites +
                                                                        historic_sites +
                                                                        economic_opportunities +
                                                                        all_villages_electricity_grid +
                                                                        reserved_women +
                                                                        reserved_obc + 
                                                                        reserved_sc, data = elite_df),
    
    "Citizen reports resignation" = lm_robust(resignation_reported ~ direct + 
                                                number_villages_GP + 
                                                total_population_gp +
                                                overall_percent_SC_gp +
                                                km_from_block_office + 
                                                total_time_from_block_office + 
                                                speed_to_block_office +
                                                pilgrimage_sites +
                                                historic_sites +
                                                economic_opportunities +
                                                all_villages_electricity_grid +
                                                reserved_women +
                                                reserved_obc + 
                                                reserved_sc, data = merged_citizen_df,
                                              clusters = r_villageid),
    
    "President reports resignation" = lm_robust(resignation ~ direct + 
                                                  number_villages_GP + 
                                                  total_population_gp +
                                                  overall_percent_SC_gp +
                                                  km_from_block_office + 
                                                  total_time_from_block_office + 
                                                  speed_to_block_office +
                                                  pilgrimage_sites +
                                                  historic_sites +
                                                  economic_opportunities +
                                                  all_villages_electricity_grid +
                                                  reserved_women +
                                                  reserved_obc + 
                                                  reserved_sc, data = elite_df),
    
    "President elected unopposed" = lm_robust(sarpanch_uncontested ~ direct + 
                                                number_villages_GP + 
                                                total_population_gp +
                                                overall_percent_SC_gp +
                                                km_from_block_office + 
                                                total_time_from_block_office + 
                                                speed_to_block_office +
                                                pilgrimage_sites +
                                                historic_sites +
                                                economic_opportunities +
                                                all_villages_electricity_grid +
                                                reserved_women +
                                                reserved_obc + 
                                                reserved_sc, data = elite_df)
    
  )
  
)

latex_code <- modelsummary(models,
             title = "The impact of direct election on informal elite capture, with controls",
             shape = "cbind",
             stars = TRUE,
             coef_rename = c("direct" = "Direct election",
                             "number_villages_GP" = "No. villages",
                             "total_population_gp" = "Population",
                             "overall_percent_SC_gp" = "Percent SC pop.",
                             "km_from_block_office" = "Distance to block (km)",
                             "total_time_from_block_office" = "Time to block (hr)",
                             "speed_to_block_office" = "Speed to block (km/hr)",
                             "pilgrimage_sites" = "Pilgrimage sites",
                             "historic_sites" = "Historic sites",
                             "economic_opportunities" = "Economic opportunities", 
                             "all_villages_electricity_grid" = "Electricity grid",
                             "reserved_women" = "Gender quota",
                             "reserved_obc" = "OBC caste quota",
                             "reserved_sc" = "SC caste quota"),
             gof_omit = 'DF|AIC|BIC|RMSE|Std.Errors|R2 Adj.',
             escape = FALSE,
             notes = 'Each model is a linear regression of each informal capture outcome on direct election and controls from Figure 3 in the main paper. All regressions use robust standard errors. Outcomes from the citizen survey use cluster robust standard errors at the council level.',
             output = "latex")


latex_string <- as.character(latex_code)

writeLines(latex_string, con = "../results/tables/tabled16.tex")


# Figure D.2: Sensitivity of t-value to unobservables, formal capture outcomes


model1 <- lm(sarpanch_maratha ~ direct + number_villages_GP + 
               total_population_gp +
               overall_percent_SC_gp +
               km_from_block_office + 
               total_time_from_block_office + 
               speed_to_block_office +
               pilgrimage_sites +
               historic_sites +
               economic_opportunities +
               all_villages_electricity_grid +
               reserved_women +
               reserved_obc + 
               reserved_sc, data = elite_df)

model2 <- lm(avg_prop_land_held_sarpanch_caste ~ direct + number_villages_GP + 
               total_population_gp +
               overall_percent_SC_gp +
               km_from_block_office + 
               total_time_from_block_office + 
               speed_to_block_office +
               pilgrimage_sites +
               historic_sites +
               economic_opportunities +
               all_villages_electricity_grid +
               reserved_women +
               reserved_obc + 
               reserved_sc,
             data = elite_df)

model3 <- lm(pacca_house ~ direct + 
               number_villages_GP + 
               total_population_gp +
               overall_percent_SC_gp +
               km_from_block_office + 
               total_time_from_block_office + 
               speed_to_block_office +
               pilgrimage_sites +
               historic_sites +
               economic_opportunities +
               all_villages_electricity_grid +
               reserved_women +
               reserved_obc + 
               reserved_sc, data = elite_df)


model4 <- lm(three_four_wheeler ~ direct + 
               number_villages_GP + 
               total_population_gp +
               overall_percent_SC_gp +
               km_from_block_office + 
               total_time_from_block_office + 
               speed_to_block_office +
               pilgrimage_sites +
               historic_sites +
               economic_opportunities +
               all_villages_electricity_grid +
               reserved_women +
               reserved_obc + 
               reserved_sc, data = elite_df)

model5 <- lm(sarpanch_land_own_name ~ direct + 
               number_villages_GP + 
               total_population_gp +
               overall_percent_SC_gp +
               km_from_block_office + 
               total_time_from_block_office + 
               speed_to_block_office +
               pilgrimage_sites +
               historic_sites +
               economic_opportunities +
               all_villages_electricity_grid +
               reserved_women +
               reserved_obc + 
               reserved_sc, data = elite_df)

model6 <- lm(sarpanch_male ~ direct + 
               number_villages_GP + 
               total_population_gp +
               overall_percent_SC_gp +
               km_from_block_office + 
               total_time_from_block_office + 
               speed_to_block_office +
               pilgrimage_sites +
               historic_sites +
               economic_opportunities +
               all_villages_electricity_grid +
               reserved_women +
               reserved_obc + 
               reserved_sc, data = elite_df)


sensitivity_1 <- sensemakr(model = model1, 
                           treatment = "direct",
                           benchmark_covariates = "reserved_women",
                           kd = 1:3,
                           ky = 1:3, 
                           q = 1,
                           alpha = 0.05, 
                           reduce = TRUE)

sensitivity_2 <- sensemakr(model = model2, 
                           treatment = "direct",
                           benchmark_covariates = "reserved_women",
                           kd = 1:3,
                           ky = 1:3, 
                           q = 1,
                           alpha = 0.05, 
                           reduce = TRUE)

sensitivity_3 <- sensemakr(model = model3, 
                           treatment = "direct",
                           benchmark_covariates = "reserved_women",
                           kd = 1:3,
                           ky = 1:3, 
                           q = 1,
                           alpha = 0.05, 
                           reduce = TRUE)

sensitivity_4 <- sensemakr(model = model4, 
                           treatment = "direct",
                           benchmark_covariates = "reserved_women",
                           kd = 1:3,
                           ky = 1:3, 
                           q = 1,
                           alpha = 0.05, 
                           reduce = TRUE)

sensitivity_5 <- sensemakr(model = model5, 
                           treatment = "direct",
                           benchmark_covariates = "reserved_women",
                           kd = 1:3,
                           ky = 1:3, 
                           q = 1,
                           alpha = 0.05, 
                           reduce = TRUE)

sensitivity_6 <- sensemakr(model = model6, 
                           treatment = "direct",
                           benchmark_covariates = "reserved_women",
                           kd = 1:3,
                           ky = 1:3, 
                           q = 1,
                           alpha = 0.05, 
                           reduce = TRUE)

# Arrange the plots in a 2x3 grid layout
plot_1 <- as.ggplot(~ plot(sensitivity_1, sensitivity.of = "t-value", main = "Model 1"))
plot_2 <- as.ggplot(~ plot(sensitivity_2, sensitivity.of = "t-value", main = "Model 2"))
plot_3 <- as.ggplot(~ plot(sensitivity_3, sensitivity.of = "t-value", main = "Model 3"))
plot_4 <- as.ggplot(~ plot(sensitivity_4, sensitivity.of = "t-value", main = "Model 4"))
plot_5 <- as.ggplot(~ plot(sensitivity_5, sensitivity.of = "t-value", main = "Model 5"))
plot_6 <- as.ggplot(~ plot(sensitivity_6, sensitivity.of = "t-value", main = "Model 6"))

combined_plot <- grid.arrange(plot_1, plot_2, plot_3, plot_4, plot_5, plot_6, ncol = 2)

ggsave("../results/figures/figured2.png", combined_plot, width = 10, height = 12)


# Figure D.3: Sensitivity of t-value to unobservables, informal capture outcomes


model1 <- lm(formersarpanch_otherperson_inauguralevents ~ direct + 
               number_villages_GP + 
               total_population_gp +
               overall_percent_SC_gp +
               km_from_block_office + 
               total_time_from_block_office + 
               speed_to_block_office +
               pilgrimage_sites +
               historic_sites +
               economic_opportunities +
               all_villages_electricity_grid +
               reserved_women +
               reserved_obc + 
               reserved_sc, data = merged_citizen_df)

model2<- lm(other_nominate_BDO ~ direct + 
              number_villages_GP + 
              total_population_gp +
              overall_percent_SC_gp +
              km_from_block_office + 
              total_time_from_block_office + 
              speed_to_block_office +
              pilgrimage_sites +
              historic_sites +
              economic_opportunities +
              all_villages_electricity_grid +
              reserved_women +
              reserved_obc + 
              reserved_sc,
            data = merged_citizen_df)

model3 <- lm(rich_landowner_influences_gp ~ direct + 
               number_villages_GP + 
               total_population_gp +
               overall_percent_SC_gp +
               km_from_block_office + 
               total_time_from_block_office + 
               speed_to_block_office +
               pilgrimage_sites +
               historic_sites +
               economic_opportunities +
               all_villages_electricity_grid +
               reserved_women +
               reserved_obc + 
               reserved_sc, data = elite_df)

model4 <- lm(resignation_reported ~ direct + 
               number_villages_GP + 
               total_population_gp +
               overall_percent_SC_gp +
               km_from_block_office + 
               total_time_from_block_office + 
               speed_to_block_office +
               pilgrimage_sites +
               historic_sites +
               economic_opportunities +
               all_villages_electricity_grid +
               reserved_women +
               reserved_obc + 
               reserved_sc, data = merged_citizen_df)

model5 <- lm(resignation ~ direct + 
               number_villages_GP + 
               total_population_gp +
               overall_percent_SC_gp +
               km_from_block_office + 
               total_time_from_block_office + 
               speed_to_block_office +
               pilgrimage_sites +
               historic_sites +
               economic_opportunities +
               all_villages_electricity_grid +
               reserved_women +
               reserved_obc + 
               reserved_sc, data = elite_df)

model6 <- lm(sarpanch_uncontested ~ direct + 
               number_villages_GP + 
               total_population_gp +
               overall_percent_SC_gp +
               km_from_block_office + 
               total_time_from_block_office + 
               speed_to_block_office +
               pilgrimage_sites +
               historic_sites +
               economic_opportunities +
               all_villages_electricity_grid +
               reserved_women +
               reserved_obc + 
               reserved_sc, data = elite_df)


sensitivity_1 <- sensemakr(model = model1, 
                           treatment = "direct",
                           benchmark_covariates = "reserved_women",
                           kd = 1:3,
                           ky = 1:3, 
                           q = 1,
                           alpha = 0.05, 
                           reduce = TRUE)

sensitivity_2 <- sensemakr(model = model2, 
                           treatment = "direct",
                           benchmark_covariates = "reserved_women",
                           kd = 1:3,
                           ky = 1:3, 
                           q = 1,
                           alpha = 0.05, 
                           reduce = TRUE)

sensitivity_3 <- sensemakr(model = model3, 
                           treatment = "direct",
                           benchmark_covariates = "reserved_women",
                           kd = 1:3,
                           ky = 1:3, 
                           q = 1,
                           alpha = 0.05, 
                           reduce = TRUE)

sensitivity_4 <- sensemakr(model = model4, 
                           treatment = "direct",
                           benchmark_covariates = "reserved_women",
                           kd = 1:3,
                           ky = 1:3, 
                           q = 1,
                           alpha = 0.05, 
                           reduce = TRUE)

sensitivity_5 <- sensemakr(model = model5, 
                           treatment = "direct",
                           benchmark_covariates = "reserved_women",
                           kd = 1:3,
                           ky = 1:3, 
                           q = 1,
                           alpha = 0.05, 
                           reduce = TRUE)

sensitivity_6 <- sensemakr(model = model6, 
                           treatment = "direct",
                           benchmark_covariates = "reserved_women",
                           kd = 1:3,
                           ky = 1:3, 
                           q = 1,
                           alpha = 0.05, 
                           reduce = TRUE)

# Arrange the plots in a 2x3 grid layout
plot_1 <- as.ggplot(~ plot(sensitivity_1, sensitivity.of = "t-value", main = "Model 1"))
plot_2 <- as.ggplot(~ plot(sensitivity_2, sensitivity.of = "t-value", main = "Model 2"))
plot_3 <- as.ggplot(~ plot(sensitivity_3, sensitivity.of = "t-value", main = "Model 3"))
plot_4 <- as.ggplot(~ plot(sensitivity_4, sensitivity.of = "t-value", main = "Model 4"))
plot_5 <- as.ggplot(~ plot(sensitivity_5, sensitivity.of = "t-value", main = "Model 5"))
plot_6 <- as.ggplot(~ plot(sensitivity_6, sensitivity.of = "t-value", main = "Model 6"))

combined_plot <- grid.arrange(plot_1, plot_2, plot_3, plot_4, plot_5, plot_6, ncol = 2)

ggsave("../results/figures/figured3.png", combined_plot, width = 10, height = 12)


# Table D.18: Alternative explanations: President party support


models = list(
  "President reports having received party support" = lm_robust(party_support_contesting_elections ~ direct, data = elite_df),
  "President reports party encouragement to contest" = lm_robust(party_told_me_to_run ~ direct, data = elite_df),
  "President reports party mobilized voters" = lm_robust(party_mobilized_voters ~ direct, data = elite_df),
  "President member of political party" = lm_robust(member_political_party ~ direct, data = elite_df),
  "President party worker previously" = lm_robust(sarpanch_political_party_worker_before ~ direct, data = elite_df)
)

options(modelsummary_panel_labels = "roman")
options(modelsummary_get = "broom")
options(modelsummary_format_numeric_latex = "mathmode")

latex_code <- modelsummary(models,
             title = "Alternative explanations: President party support",
             stars = TRUE,
             coef_rename = c("direct" = "Direct election"),
             gof_omit = 'DF|AIC|BIC|RMSE|Std.Errors',
             escape = FALSE,
             output = "latex",
             note = "Regression of outcomes on direct election indicator with robust standard errors. Citizen voting outcome uses robust standard errors clustered at the council level."
)

latex_string <- as.character(latex_code)

writeLines(latex_string, con = "../results/tables/tabled18.tex")

# Table D.19: Alternative explanations: Citizen voting behavior

models = list(
  "Citizen voted in village council election" = lm_robust(voted_last_gp_election ~ direct, data = citizen_df, clusters = r_villageid),
  "Citizen prefers BJP" = lm_robust(bjp_preferred ~ direct, data = citizen_df, clusters = r_villageid),
  "Citizen prefers Congresss" = lm_robust(congress_preferred ~ direct, data = citizen_df, clusters = r_villageid)
)

options(modelsummary_panel_labels = "roman")
options(modelsummary_get = "broom")
options(modelsummary_format_numeric_latex = "mathmode")

latex_code <- modelsummary(models,
             title = "Alternative explanations: Citizen voting behavior",
             stars = TRUE,
             coef_rename = c("direct" = "Direct election"),
             gof_omit = 'DF|AIC|BIC|RMSE|Std.Errors',
             escape = FALSE,
             output = "latex",
             note = "Regression of outcomes on direct election indicator with robust standard errors. Citizen voting outcome uses robust standard errors clustered at the council level."
)

latex_string <- as.character(latex_code)

writeLines(latex_string, con = "../results/tables/tabled19.tex")


# Table F.2: Proportions across treatment groups, each pre-specified covariate


variables_of_interest <- c("rural", 
                           "age",
                           "gender",
                           "religion",
                           "caste",
                           "education",
                           "prior_vote", 
                           "knowledge_local_politics", 
                           "gender_norms")

latex_code <- datasummary_balance(rural + 
                      age + 
                      gender + 
                      religion + 
                      caste + 
                      education + 
                      prior_vote + 
                      knowledge_local_politics + 
                      gender_norms ~ sarpanchgendercaste_combination,
                    data = experiment_df,
                    title = "Proportions across treatment groups, each pre-specified covariate",
                    output = 'latex')

latex_string <- as.character(latex_code)

writeLines(latex_string, con = "../results/tables/tablef2.tex")


# Table F.3: Balance test


experiment_df <- experiment_df %>% 
  mutate(
    "Bharti Marathe" = case_when(
      sarpanchgendercaste_combination == "bharti marathe" ~ 1,
      TRUE ~ 0
    ),
    "Rohit Marathe" = case_when(
      sarpanchgendercaste_combination == "rohit marathe" ~ 1,
      TRUE ~ 0
    ),
    "Bharti Kamble" = case_when(
      sarpanchgendercaste_combination == "bharti kamble" ~ 1,
      TRUE ~ 0
    ),
    "Rohit Kamble" = case_when(
      sarpanchgendercaste_combination == "rohit kamble" ~ 1,
      TRUE ~ 0
    )
  )

models <- list(
  "Bharti Kamble" = lm_robust(`Bharti Kamble` ~ rural + 
                                age + gender + religion + 
                                caste + education + prior_vote + 
                                knowledge_local_politics + gender_norms, data = experiment_df),
  
  "Bharti Marathe" = lm_robust(`Bharti Marathe` ~ rural + 
                                 age + gender + religion + 
                                 caste + education + prior_vote + 
                                 knowledge_local_politics + gender_norms, data = experiment_df),
  
  "Rohit Kamble" = lm_robust(`Rohit Kamble` ~ rural + 
                               age + gender + religion + 
                               caste + education + prior_vote + 
                               knowledge_local_politics + gender_norms, data = experiment_df),
  
  "Rohit Marathe" = lm_robust(`Rohit Marathe` ~ rural + 
                                age + gender + religion + 
                                caste + education + prior_vote + 
                                knowledge_local_politics + gender_norms, data = experiment_df)
  
)

options(modelsummary_panel_labels = "roman")
options(modelsummary_get = "broom")
options(modelsummary_format_numeric_latex = "mathmode")

latex_code <-
  modelsummary(models,
             title = "Balance test",
             stars = TRUE,
             coef_rename = c("rural" = "Rural",
                             "age" = "Age",
                             "gender" = "Gender",
                             "religion" = "Religion",
                             "caste" = "Caste",
                             "education" = "Education",
                             "prior_vote" = "Voted previously",
                             "knowledge_local_politics" = "Knowledge of local politics",
                             "gender_norms" = "Gender norms"),
             gof_function = get_f_stat_pvalue,
             gof_omit = 'DF|AIC|BIC|RMSE|Std.Errors|R2 Adj.',
             coef_omit = "Intercept",
             escape = FALSE,
             notes = 'Each model is a linear regression of each treatment group indicator on covariates. All regressions use robust standard errors.',
             output = "latex")

latex_string <- as.character(latex_code)

writeLines(latex_string, con = "../results/tables/tablef3.tex")


# Table F.4: Pre-specified main and secondary hypotheses


models <- list(
  
  "M1: Male advantage in authority" = list(
    
    "Unadjusted" = lm_robust(authority ~ Male, data = experiment_df),
    
    "Covariate-adjusted" = lm_robust(authority ~ Male + rural + age + gender + religion + caste + education + prior_vote + knowledge_local_politics + gender_norms, data = experiment_df)
    
  ),
  
  "M2: Dominant caste advantage in authority" = list(
    "Unadjusted" = lm_robust(authority ~ Maratha, data = experiment_df),
    
    "Covariate-adjusted" = lm_robust(authority ~ Maratha + rural + age + gender + religion + caste + education + prior_vote + knowledge_local_politics + gender_norms, data = experiment_df)
  ),
  
  "M3: Male advantage in pliability" = list(
    "Unadjusted" = lm_robust(pliability ~ Male, data = experiment_df),
    
    "Covariate-adjusted" = lm_robust(pliability ~ Male + rural + age + gender + religion + caste + education + prior_vote + knowledge_local_politics + gender_norms, data = experiment_df)
  ),
  
  "M4: Dominant caste advantage in pliability" = list(
    
    "Unadjusted" = lm_robust(pliability ~ Maratha, data = experiment_df),
    
    "Covariate-adjusted" = lm_robust(pliability ~ Maratha + rural + age + gender + religion + caste + education + prior_vote + knowledge_local_politics + gender_norms, data = experiment_df)
  ),
  
  "S1: Male advantage in backlash?" = list(
    "Unadjusted" = lm_robust(backlash ~ Male, data = experiment_df),
    
    "Covariate-adjusted" = lm_robust(backlash ~ Male + rural + age + gender + religion + caste + education + prior_vote + knowledge_local_politics + gender_norms, data = experiment_df)
  ),
  
  "S2: Dominant caste advantage in backlash" = list(
    "Unadjusted" = lm_robust(backlash ~ Maratha, data = experiment_df),
    "Covariate-adjusted" = lm_robust(backlash ~ Maratha + rural + age + gender + religion + caste + education + prior_vote + knowledge_local_politics + gender_norms, data = experiment_df)
  )
  
)


options(modelsummary_panel_labels = "roman")
options(modelsummary_get = "broom")
options(modelsummary_format_numeric_latex = "mathmode")

latex_code <- modelsummary(models,
             title = "Pre-registered main and secondary hypotheses",
             shape = "cbind",
             stars = TRUE,
             coef_map = c("Male", "Maratha", "rural", "age", "gender", 
                          "religionBuddhist",
                          "religionChristian",
                          "religionHindu",
                          "religionJain",
                          "religionMuslim",
                          "casteOBC",
                          "casteOther General Caste",
                          "casteScheduled Caste (Dalit)",
                          "casteScheduled Tribe (Adivasi)",
                          "casteVJNT (Bhatke Vimukt)",
                          "educationSecondary school", 
                          "educationUndergraduate and above",
                          "prior_voteYES", 
                          "knowledge_local_politics", "gender_norms"),
             gof_omit = 'DF|AIC|BIC|RMSE|Std.Errors|R2 Adj.',
             escape = FALSE,
             notes = 'Each model is a linear regression of each treatment indicator (either male or Maratha) on each outcome, corresponding to each hypothesis. For each hypothesis, unadjusted and covariate-adjusted estimates are presented. All regressions use robust standard errors.',
             output = "latex")

latex_string <- as.character(latex_code)

writeLines(latex_string, con = "../results/tables/tablef4.tex")


# Figure G.1: Expert predictions on the impact of direct elections on president's authority

# predictions

expert_df <- expert_df %>% mutate(
  ate_prediction = ((direct_authority_score - 20)/100))

# histogram of predictions

mean_value <- mean(expert_df$ate_prediction)

predictions <- ggplot(expert_df, aes(x = ate_prediction)) +
  geom_density(fill = "gray", color = "black") +
  labs(
    title = "Predictions on the impact of direct elections (ATE estimates)",
    x = "Priors for ATE",
    y = "Density"
  ) +
  geom_vline(xintercept = mean_value, color = "red", linetype = "dotted", linewidth = 1) +  # Add red dotted line
  annotate("text", x = mean_value, y = 0.2, label = paste("Mean =", round(mean_value, 2)), 
           color = "red", angle = 90, vjust = -0.5, hjust = 0.5) +  # Add label
  xlim(-1, 1) + 
  theme_classic()

# Save the plot as a PNG file
ggsave("../results/figures/figureg1.png", plot = predictions, width = 8, height = 6, dpi = 300)


# Figure H.1: The impact of direct elections in SC-reserved seats

sc_seats_elite_df <- elite_df %>% filter(reserved_sc == 1)
sc_seats_citizen_df <- merged_citizen_df %>% filter(reserved_sc == 1)
sc_seats_admin_df <- admin_df %>% filter(reserved_sc == 1)

# de facto authority outcomes

vars <- c("most_influential_gd", "prop_speakingtime_sarpanch", "i_decide_masik_sabha")
clustered_vars <- c("sarpanch_nominate_BDO", "sarpanch_inauguralevents")

results1 <- run_regressions(vars, data_nonclustered = sc_seats_elite_df, data_clustered = sc_seats_citizen_df, clustered_vars = clustered_vars)

results1 <- results1 %>%
  mutate(significance_level = case_when(
    p_value < 0.001 ~ 0.001,
    p_value < 0.01  ~ 0.01,
    p_value < 0.05  ~ 0.05,
    p_value < 0.1   ~ 0.1,
    p_value > 0.1 ~ round(p_value, digits = 3)
  ))

results1$title2 <- "De facto authority"

results1$names <- c("President perceived as most influential actor in discussion",
                    "Proportion of time president spoke in discussion",
                    "President self-perception as main decision-maker in monthly meeting",
                    "Citizen nominated president as village representative to bureaucrat",
                    "Citizen reports president leads village events")

results1 <- results1 %>%
  mutate(
    indirect_mean_label = round(indirect_mean, 3),
    direct_mean_label = round(direct_mean, 3)
  )

results1$names <- str_wrap(results1$names, width = 25)

results1$val <- 1:5

results1$variable_type <- c("Perception", 
                            "Behavior",
                            "Perception",
                            "Behavior",
                            "Behavior")

# formal capture outcomes

vars <- c("sarpanch_maratha", 
          "avg_prop_land_held_sarpanch_caste",
          "sarpanch_male",
          "sarpanch_land_own_name",
          "three_four_wheeler",
          "pacca_house")

results2 <- run_regressions(
  vars = vars,
  data_nonclustered = sc_seats_elite_df
)

results2$title2 <- "Formal capture"

results2$names <- c("President Maratha",
                    "Proportion land held by president's caste in village",
                    "President male",
                    "President landed",
                    "President owns three/four-wheeler",
                    "President owns pacca house")

results2 <- results2 %>%
  mutate(
    indirect_mean_label = paste0(round(indirect_mean, 2), " (", round(indirect_mean_se, 2), ")"),
    direct_mean_label = paste0(round(direct_mean, 2), " (", round(direct_mean_se, 2), ")")
  )

results2 <- results2 %>%
  mutate(significance_level = case_when(
    p_value < 0.001 ~ 0.001,
    p_value < 0.01  ~ 0.01,
    p_value < 0.05  ~ 0.05,
    p_value < 0.1   ~ 0.1,
    p_value > 0.1 ~ round(p_value, digits = 3)
  ),
  indirect_mean_label = round(indirect_mean, 3),
  direct_mean_label = round(direct_mean, 3),
  indirectlabel = "i",
  directlabel = "d"
  )

results2$names <- str_wrap(results2$names, width = 25)

results2$val <- 1:6

results2$variable_type <- c("Caste", 
                            "Caste",
                            "Gender",
                            "Class",
                            "Class",
                            "Class")

# informal capture outcomes

vars <- c("sarpanch_uncontested",
          "resignation",
          "rich_landowner_influences_gp")

clustered_vars <- c("other_nominate_BDO",
                    "formersarpanch_otherperson_inauguralevents",
                    "resignation_reported")

admin_vars <- c("resigned")

results3 <- run_regressions(
  vars = vars,
  data_nonclustered = sc_seats_elite_df,
  data_clustered = sc_seats_citizen_df,
  admin_data = sc_seats_admin_df,  
  clustered_vars = clustered_vars,
  admin_vars = admin_vars
)

results3 <- results3 %>%
  mutate(significance_level = case_when(
    p_value < 0.001 ~ 0.001,
    p_value < 0.01  ~ 0.01,
    p_value < 0.05  ~ 0.05,
    p_value < 0.1   ~ 0.1,
    p_value > 0.1 ~ round(p_value, digits = 3)
  ),
  indirect_mean_label = round(indirect_mean, 3),
  direct_mean_label = round(direct_mean, 3)
  )

results3$title2 <- "Informal capture"

results3$names <- c("President elected unopposed",
                    "President reports resignation",
                    "President reports rich landowner influences council",
                    "Citizen nominated former president/other villager as village representative to bureaucrat",
                    "Citizen reports former president/other villager leads village events",
                    "Citizen reports resignation",
                    "Resignation filed: Administrative data")

results3$names <- str_wrap(results3$names, width = 25)

results3$val <- 1:7

results3$variable_type <- c("Implicit",
                            "Implicit",
                            "Explicit", 
                            "Explicit",
                            "Explicit",
                            "Implicit",
                            "Implicit")

# plot results

authority <- results1 %>% 
  arrange(val) %>%   
  mutate(names = factor(names, levels = names)) %>% 
  ggplot() + 
  geom_point(aes(x = names, y = ATE), size = 4, colour = "black") + 
  geom_linerange(aes(x = names, ymax = CI_upper_95, ymin = CI_lower_95), lwd = 1) +
  scale_y_continuous(name = "Difference in Means", limits = c(-0.5, 0.5), breaks = c(-0.5, 0, 0.5)) +
  facet_grid(variable_type ~ title2, scales = "free", switch = "y", space = "free_y") +
  theme_classic() +
  theme(
    strip.background = element_rect(size = 0.5),
    strip.placement = "outside",
    panel.spacing = unit(0, 'npc'),
    strip.text.x = element_text(size = 14),
    strip.text.y = element_text(size = 14),  # Shows facet labels on the left
    axis.line = element_line(color = 'black'),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.title.y = element_text(size = 14),  # Restores y-axis title
    axis.text.y = element_text(size = 13),  # Restores y-axis text
    axis.text.x = element_text(size = 13)
  ) +
  labs(x = "") +
  ggtitle(" .") +
  coord_flip() +
  geom_hline(data = data.frame(yint = 0), aes(yintercept = yint), 
             colour = "grey50", linetype = "longdash") +
  geom_text(aes(
    x = names, y = ATE, 
    label = ifelse(
      significance_level > 0.1, 
      sprintf("Impact of direct: %.3f (p = %.2f)\nMean, indirect: %.3f", ATE, significance_level, indirect_mean),
      sprintf("Impact of direct: %.3f (p < %.2f)\nMean, indirect: %.3f", ATE, significance_level, indirect_mean)
    )
  ), hjust = 0.3, vjust = 0.5, size = 4)

formalcapture <- results2 %>% 
  arrange(val) %>%   
  mutate(names = factor(names, levels = names)) %>% 
  ggplot() + 
  geom_point(aes(x = names, y = ATE), size = 4, colour = "black") + 
  geom_linerange(aes(x = names, ymax = CI_upper_95, ymin = CI_lower_95), lwd = 1) +
  scale_y_continuous(name = "Difference in Means", limits = c(-0.5, 0.5), breaks = c(-0.5, 0, 0.5)) +
  facet_grid(variable_type ~ title2, scales = "free", switch = "y", space = "free_y") +
  theme_classic() +
  theme(
    strip.background = element_rect(size = 0.5),
    strip.placement = "outside",
    panel.spacing = unit(0, 'npc'),
    strip.text.x = element_text(size = 14),
    strip.text.y = element_text(size = 14),  # Shows facet labels on the left
    axis.line = element_line(color = 'black'),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.title.y = element_text(size = 14),  # Restores y-axis title
    axis.text.y = element_text(size = 13),  # Restores y-axis text
    axis.text.x = element_text(size = 13)
  ) +
  labs(x = "") +
  ggtitle(" .") +
  coord_flip() +
  geom_hline(data = data.frame(yint = 0), aes(yintercept = yint), 
             colour = "grey50", linetype = "longdash") +
  geom_text(aes(
    x = names, y = ATE, 
    label = ifelse(
      significance_level > 0.1, 
      sprintf("Impact of direct: %.3f (p = %.2f)\nMean, indirect: %.3f", ATE, significance_level, indirect_mean),
      sprintf("Impact of direct: %.3f (p < %.2f)\nMean, indirect: %.3f", ATE, significance_level, indirect_mean)
    )
  ), hjust = 0.3, vjust = 0.5, size = 4)


informalcapture <- results3 %>% 
  arrange(val) %>%   
  mutate(names = factor(names, levels = names)) %>% 
  ggplot() + 
  geom_point(aes(x = names, y = ATE), size = 4, colour = "black") + 
  geom_linerange(aes(x = names, ymax = CI_upper_95, ymin = CI_lower_95), lwd = 1) +
  scale_y_continuous(name = "Difference in Means", limits = c(-0.5, 0.5), breaks = c(-0.5, 0, 0.5)) +
  facet_grid(variable_type ~ title2, scales = "free", switch = "y", space = "free_y") +
  theme_classic() +
  theme(
    strip.background = element_rect(size = 0.5),
    strip.placement = "outside",
    panel.spacing = unit(0, 'npc'),
    strip.text.x = element_text(size = 14),
    strip.text.y = element_text(size = 14),  # Shows facet labels on the left
    axis.line = element_line(color = 'black'),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.title.y = element_text(size = 14),  # Restores y-axis title
    axis.text.y = element_text(size = 13),  # Restores y-axis text
    axis.text.x = element_text(size = 13)
  ) +
  labs(x = "") +
  ggtitle(" .") +
  coord_flip() +
  geom_hline(data = data.frame(yint = 0), aes(yintercept = yint), 
             colour = "grey50", linetype = "longdash") +
  geom_text(aes(
    x = names, y = ATE, 
    label = ifelse(
      significance_level > 0.1, 
      sprintf("Impact of direct: %.3f (p = %.2f)\nMean, indirect: %.3f", ATE, significance_level, indirect_mean),
      sprintf("Impact of direct: %.3f (p < %.2f)\nMean, indirect: %.3f", ATE, significance_level, indirect_mean)
    )
  ), hjust = 0.3, vjust = 0.5, size = 4)


# save plot

authorityformalinformal <- grid.arrange(authority, formalcapture, informalcapture, nrow = 3)

ggsave("../results/figures/figureh1.png", authorityformalinformal, width = 12, height = 16, units = "in")


# Figure H.2: The impact of social disadvantage on the threat of backlash

# Code reverse variables to measure disadvantage

experiment_df <- experiment_df %>% mutate(
  Female = case_when(Male == 1 ~ 0, 
                     Male == 0 ~ 1),
  
  Dalit = case_when(Maratha == 1 ~ 0,
                    Maratha == 0 ~ 1)
)

# outcome variable
outcome <- "backlash"

# independent variables
independent_vars <- c("Female", "Dalit")

# Run the function and store the results in a dataframe
results_df <- run_regressions_surveyexperiment(experiment_df, outcome, independent_vars)

results_df <- results_df %>%
  mutate(significance_level = case_when(
    P_Value < 0.001 ~ 0.001,
    P_Value < 0.01  ~ 0.01,
    P_Value < 0.05  ~ 0.05,
    P_Value < 0.1   ~ 0.1,
    P_Value > 0.1 ~ round(P_Value, digits = 3)
  ))

# make graph
plot1 <- results_df %>%
  mutate(Independent_Var = factor(Independent_Var, levels = Independent_Var),
         Label = case_when(
           Independent_Var == "Dalit" ~ sprintf("Mean Maratha: %.3f", Mean_Indep_0),
           Independent_Var == "Female" ~ sprintf("Mean male: %.3f", Mean_Indep_0),
           TRUE ~ sprintf("Control Mean: %.3f", Mean_Indep_0)  
         )) %>%
  ggplot() + 
  geom_point(aes(x = Independent_Var, y = ATE), size = 3, colour = "black") + 
  geom_linerange(aes(x = Independent_Var, ymax = CI_Upper, ymin = CI_Lower), lwd = 1) +
  scale_y_continuous(name = "Covariate-adjusted OLS ATE estimate (percentage points)", limits = c(-0.10, 0.25), breaks = c(-0.1,0, 0.25)) +
  theme_classic() +
  theme(
    strip.background = element_rect(size = 0.5),
    strip.placement = "outside",
    panel.spacing = unit(0, 'npc'),
    strip.text.x = element_text(size = 12),
    axis.line = element_line(color = 'black'),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.text = element_text(size = 11)
  ) +
  labs(x = "", y = "ATE") +  # Set the x-axis label here
  ggtitle("Non-elite status increases the threat of backlash") +  
  coord_flip() +
  geom_hline(aes(yintercept = 0), colour = "grey50", linetype = "longdash") + 
  geom_text(aes(
    x = Independent_Var, 
    y = ATE, 
    label = ifelse(
      significance_level > 0.1, 
      sprintf("ATE estimate: %.3f\n%s\n(p = %.3f)", ATE, Label, significance_level),
      sprintf("ATE estimate: %.3f\n%s\n(p < %.3f)", ATE, Label, significance_level)
    )
  ), vjust = -0.8, hjust = 0.3, size = 4)


df_long <- experiment_df %>%
  select(sarpanchgendercaste_combination, backlash) %>%
  pivot_longer(cols = c(backlash),
               names_to = "variable", values_to = "value")

df_long$variable <- factor(df_long$variable, levels = c("backlash"))

mean_values <- df_long %>%
  group_by(sarpanchgendercaste_combination) %>%
  summarise(mean_value = mean(value, na.rm = TRUE))

mean_values$sarpanchgendercaste_combination <- factor(
  mean_values$sarpanchgendercaste_combination, 
  levels = c("rohit marathe", "bharti marathe", "rohit kamble", "bharti kamble")
)

plot2 <- ggplot(mean_values, aes(x = sarpanchgendercaste_combination, y = mean_value)) +
  geom_bar(stat = "identity", position = "dodge", fill = "darkgray") +
  geom_text(aes(label = round(mean_value, 2)), 
            vjust = -0.5, size = 4) + 
  theme_minimal() +
  labs(x = "Hypothetical president", y = "Mean value", title = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("authority" = "darkgray"))

combined_plot <- grid.arrange(plot1, plot2, ncol = 2)

# save results
ggsave("../results/figures/figureh2.png", combined_plot, width = 12, height = 6, units = "in", dpi = 300)
