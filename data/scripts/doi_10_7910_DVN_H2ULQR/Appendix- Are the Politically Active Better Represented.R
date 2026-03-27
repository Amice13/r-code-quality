##### Are the Politically Active Better Represented? #####

### (4) Appendix Analyses ###

### Appendix A

## Descriptive Statistics
desc_regdata <- regdata |> 
  select(n_policyquestions, meancong, meansupport, meanimp, income_unified, lrself, 
         AgeCategories, University, woman, rural, PolInterested, ethnic_minority, 
         Pol_efficacy, Pol_trust, Satisfied_w_Dem, voted, protest, contact, attendedmeeting, 
         petition, boycott, donated, internetforum, contactmedia, polwork, otherwork,
         wornbadge, ethcon, illegalprotest, meeting)

table(desc_regdata$AgeCategories)
desc_regdata <- desc_regdata |> 
  mutate(Ageunder30 = case_when(AgeCategories == "30 to 39" ~ 0,
                                AgeCategories == "40-49" ~ 0,
                                AgeCategories == "50-59" ~ 0,
                                AgeCategories == "60-69" ~ 0,
                                AgeCategories == "70 and older" ~ 0,
                                AgeCategories == "Under 30" ~ 1,
                                TRUE ~ NA_real_),
         Agemiddle = case_when(AgeCategories == "30 to 39" ~ 1,
                               AgeCategories == "40-49" ~ 1,
                               AgeCategories == "50-59" ~ 1,
                               AgeCategories == "60-69" ~ 0,
                               AgeCategories == "70 and older" ~ 0,
                               AgeCategories == "Under 30" ~ 0,
                               TRUE ~ NA_real_),
         AgeAbove60 = case_when(AgeCategories == "30 to 39" ~ 0,
                                AgeCategories == "40-49" ~ 0,
                                AgeCategories == "50-59" ~ 0,
                                AgeCategories == "60-69" ~ 1,
                                AgeCategories == "70 and older" ~ 1,
                                AgeCategories == "Under 30" ~ 0,
                                TRUE ~ NA_real_))

stargazer(as.data.frame(desc_regdata), type = "html")


## Create a table for number of policy observations by country, for the appendix
table(fulldata_long$country)
policies_by_country <- fulldata_long |>
  summarize(number_of_policies = n_distinct(q_id),
            .by = c(country, year)) |>
  select(country, number_of_policies) |> 
  summarise(sum_number_of_policies = sum(number_of_policies),
            .by = country)

# Print the result to an Excel file
# write_xlsx(policies_by_country, "data/n_policies_by_country.xlsx")

## Create a table for percentage of protestors and voters for the appendix
percentage_dem_vote_by_country <- regdata |>
  summarize(percentage_demonstrators = round(mean(protest, na.rm = T) * 100),
            percentage_voters = round(mean(voted, na.rm = T) * 100),
            .by = country)

# Print the result to an Excel file
# write_xlsx(percentage_dem_vote_by_country, "data/percentage_of_demonstratorsandvoters_by_country.xlsx")


### Appendix B

# Calculate confidence intervals of differences between groups
weighted_diff_func <- function(data, indices) {
  sample_data <- data[indices, ]
  
  ci1_data <- subset(sample_data, protest == 1) # change groups here
  ci1_mean <- weighted.mean(ci1_data$meancong,
                            w = ci1_data$n_policyquestions,
                            na.rm = TRUE)
  
  ci2_data <- subset(sample_data, protest == 0) # and here
  ci2_mean <- weighted.mean(ci2_data$meancong,
                            w = ci2_data$n_policyquestions,
                            na.rm = TRUE)
  
  return(ci1_mean - ci2_mean)
}

bootstrap_diff_cis <- boot(data = regdata,
                               statistic = weighted_diff_func,
                               R = 400)

boot_ci_diff <- boot.ci(bootstrap_diff_cis, type = "basic")
boot_ci_diff
mean(boot_ci_diff$t0)

# Introducing an interaction effect between demonstrated and voted
int_data <- regdata |> 
  filter(!is.na(meancong) & !is.na(voted) & !is.na(protest) & !is.na(countrysurveyyear) & !is.na(income_unified) & 
           !is.na(University) & !is.na(woman) & !is.na(AgeCategories) & !is.na(rural))
b_int_1 <- lm(meancong ~ voted + protest + as.factor(countrysurveyyear), data=int_data, weights = n_policyquestions)
b_int_2 <- lm(meancong ~ voted*protest + as.factor(countrysurveyyear), data=int_data, weights = n_policyquestions)
b_int_3 <- lm(meancong ~ voted*protest + income_unified + University + woman +
                eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") +
                rural + as.factor(countrysurveyyear), data=int_data, weights = n_policyquestions)

stargazer(b_int_1, b_int_2, b_int_3, type="text", omit.stat=c("SER", "F"), omit=c("country", "year", "countrysurveyyear"), star.cutoffs = c(0.05, 0.01, 0.001))


### Appendix C

## Create an appendix table with model specifications for additional non-electoral political participation variables

## Without control variables

# Define the names of the main independent variables
main_vars <- c("voted", "protest", "contact", "attendedmeeting", "petition", 
               "boycott", "donated", "internetforum", "contactmedia", 
               "polwork", "otherwork", "wornbadge", "ethcon", 
               "illegalprotest", "meeting")

# Create a list of the models
models <- list(
  m1 <- lm(meancong ~ voted             + as.factor(countrysurveyyear), data=filter(regdata, !is.na(protest))),
  m2 <- lm(meancong ~ protest           + as.factor(countrysurveyyear), data=regdata),
  m3 <- lm(meancong ~ contact           + as.factor(countrysurveyyear), data=filter(regdata, !is.na(protest))),
  m4 <- lm(meancong ~ attendedmeeting   + as.factor(countrysurveyyear), data=filter(regdata, !is.na(protest))),
  m5 <- lm(meancong ~ petition          + as.factor(countrysurveyyear), data=filter(regdata, !is.na(protest))),
  m6 <- lm(meancong ~ boycott           + as.factor(countrysurveyyear), data=filter(regdata, !is.na(protest))),
  m7 <- lm(meancong ~ donated           + as.factor(countrysurveyyear), data=filter(regdata, !is.na(protest))),
  m8 <- lm(meancong ~ internetforum     + as.factor(countrysurveyyear), data=filter(regdata, !is.na(protest))),
  m9 <- lm(meancong ~ contactmedia      + as.factor(countrysurveyyear), data=filter(regdata, !is.na(protest))),
  m10 <- lm(meancong ~ polwork          + as.factor(countrysurveyyear), data=filter(regdata, !is.na(protest))),
  m11 <- lm(meancong ~ otherwork        + as.factor(countrysurveyyear), data=filter(regdata, !is.na(protest))),
  m12 <- lm(meancong ~ wornbadge        + as.factor(countrysurveyyear), data=filter(regdata, !is.na(protest))),
  m13 <- lm(meancong ~ ethcon           + as.factor(countrysurveyyear), data=filter(regdata, !is.na(protest))),
  m14 <- lm(meancong ~ illegalprotest   + as.factor(countrysurveyyear), data=filter(regdata, !is.na(protest))),
  m15 <- lm(meancong ~ meeting          + as.factor(countrysurveyyear), data=filter(regdata, !is.na(protest)))
  
)

# Extract the information for each model
results <- lapply(models, function(model) {
  tidy_model <- tidy(model) |>
    filter(term %in% main_vars) |>  # Keep only main independent variables
    select(term, estimate, std.error)
  
  glance_model <- glance(model)
  
  tidy_model |>
    mutate(nobs = glance_model$nobs,
           r.squared = glance_model$r.squared)
})

# Combine all results into a single data frame
final_results <- bind_rows(results)

# Rename the term column to variable name
final_results <- final_results |>
  rename(variable_name = term) |> 
  mutate(with_controls = "No")

## With control variables
models_wc <- list(
  c1 <- lm(meancong ~ voted             + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(protest))),
  c2 <- lm(meancong ~ protest           + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=regdata),
  c3 <- lm(meancong ~ contact           + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(protest))),
  c4 <- lm(meancong ~ attendedmeeting   + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(protest))),
  c5 <- lm(meancong ~ petition          + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(protest))),
  c6 <- lm(meancong ~ boycott           + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(protest))),
  c7 <- lm(meancong ~ donated           + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(protest))),
  c8 <- lm(meancong ~ internetforum     + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(protest))),
  c9 <- lm(meancong ~ contactmedia      + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(protest))),
  c10 <- lm(meancong ~ polwork          + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(protest))),
  c11 <- lm(meancong ~ otherwork        + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(protest))),
  c12 <- lm(meancong ~ wornbadge        + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(protest))),
  c13 <- lm(meancong ~ ethcon           + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(protest))),
  c14 <- lm(meancong ~ illegalprotest   + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(protest))),
  c15 <- lm(meancong ~ meeting          + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(protest)))
  
)

# Extract the information for each model
results_wc <- lapply(models_wc, function(model) {
  tidy_model <- tidy(model) |>
    filter(term %in% main_vars) |>  # Keep only main independent variables
    select(term, estimate, std.error)
  
  glance_model <- glance(model)
  
  tidy_model |>
    mutate(nobs = glance_model$nobs,
           r.squared = glance_model$r.squared)
})

# Combine all results into a single data frame
final_results_wc <- bind_rows(results_wc)

# Rename the term column to variable name
final_results_wc <- final_results_wc |>
  rename(variable_name = term) |> 
  mutate(with_controls = "Yes")

# Combine 
combined_results <- bind_rows(final_results, final_results_wc)
combined_results <- combined_results |> 
  mutate(across(where(is.numeric), round, digits=3)) |> 
  arrange(variable_name)

# Export the results to Excel
# write_xlsx(combined_results, "data/Appendix_Fig6_B1_models_info.xlsx")


## All forms of participation
# Different forms of participation controlled for each other
multi0 <- lm(meancong ~ voted + protest                                                                         + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=regdata, weights = n_policyquestions)
multi1 <- lm(meancong ~ voted + protest + contact + petition + boycott                                          + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=regdata, weights = n_policyquestions)
multi2 <- lm(meancong ~ voted + protest + polwork + otherwork + wornbadge + boycott + contact + petition        + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=regdata, weights = n_policyquestions)
multi3 <- lm(meancong ~ voted + protest + contact + petition + boycott + internetforum + contactmedia           + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=regdata, weights = n_policyquestions)
stargazer(multi0, multi1, multi2, multi3, type="text", omit.stat=c("SER", "F"), omit=c("countrysurveyyear"), star.cutoffs = c(0.05, 0.01, 0.001))


## Utilizing a variable measuring any nonelectoral political participation
# Controlling for socio-economic factors - Re-estimating Table 3 with general political participation variable
b1_app <- lm(meancong ~ voted + polparticipation + as.factor(countrysurveyyear), data=regdata, weights = n_policyquestions)
b2_app <- lm(meancong ~ voted + polparticipation + income_unified + as.factor(countrysurveyyear), data=regdata, weights = n_policyquestions)
b3_app <- lm(meancong ~ voted + polparticipation + University + as.factor(countrysurveyyear), data=regdata, weights = n_policyquestions)


b4_0_app <- lm(meancong ~ voted + polparticipation + as.factor(countrysurveyyear),
               data=filter(regdata, !is.na(income_unified) & !is.na(University) & !is.na(woman) & !is.na(AgeCategories) & 
                             !is.na(rural)), weights = n_policyquestions) # running the first regression model with the observations of the fourth

b4_app <- lm(meancong ~ voted + polparticipation + income_unified + University + woman +
               eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") +
               rural + as.factor(countrysurveyyear), data=regdata, weights = n_policyquestions)

stargazer(b4_0_app, type="text", omit.stat=c("SER", "F"), omit=c("country", "year", "countrysurveyyear"), star.cutoffs = c(0.05, 0.01, 0.001)) # Model b1 with sample of Model b4
stargazer(b1_app, b2_app, b3_app, b4_app, type="text", omit.stat=c("SER", "F"), omit=c("country", "year", "countrysurveyyear"), star.cutoffs = c(0.05, 0.01, 0.001))


### Appendix D

## Full Table 4 results are presented in the main R code

## Separating out ISSP RoG and running the main regression with and without this survey

# ESS and ISSP Citizen alone
table(fulldata_individual$survey2)
fulldata_individual_ISSPCit_ESS <- subset(fulldata_individual, survey2 == "ISSP Citizen I" | survey2 == "ISSP Citizen II" | survey2 == "ESS")

Reg_ISSPCit_ESS_0 <- lm(meancong ~ voted + protest + as.factor(countrysurveyyear),
                        data=filter(fulldata_individual_ISSPCit_ESS, !is.na(income_unified) & !is.na(University) & 
                                      !is.na(woman) & !is.na(AgeCategories) & !is.na(rural)), weights = n_policyquestions)
Reg_ISSPCit_ESS_1 <- lm(meancong ~ voted + protest + income_unified + University + woman +
                          eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural + 
                          as.factor(countrysurveyyear), data=fulldata_individual_ISSPCit_ESS, weights = n_policyquestions)
Reg_ISSPCit_ESS_2 <- lm(meancong ~ voted + protest + income_unified + University + woman +
                          eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural + ethnic_minority +
                          as.factor(countrysurveyyear), data=fulldata_individual_ISSPCit_ESS, weights = n_policyquestions)

stargazer(Reg_ISSPCit_ESS_0, Reg_ISSPCit_ESS_1, Reg_ISSPCit_ESS_2, type="text", omit.stat=c("SER", "F"), omit=c("countrysurveyyear"), star.cutoffs = c(0.05, 0.01, 0.001))

## Controlling for left-right self-placement
Reg_lrself_0 <- lm(meancong ~ voted + protest + as.factor(countrysurveyyear),
                   data=filter(regdata, !is.na(income_unified) & !is.na(University) & 
                                 !is.na(woman) & !is.na(AgeCategories) & !is.na(rural) & !is.na(lrself)), weights = n_policyquestions)
Reg_lrself_1 <- lm(meancong ~ voted + protest + income_unified + University + woman + 
                     eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural + 
                     as.factor(countrysurveyyear),data=filter(regdata, !is.na(income_unified) & !is.na(University) & 
                                                                !is.na(woman) & !is.na(AgeCategories) & !is.na(rural) & !is.na(lrself)), weights = n_policyquestions)
Reg_lrself_2 <- lm(meancong ~ voted + protest + income_unified + University + woman + 
                     eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural + lrself + 
                     as.factor(countrysurveyyear), data=regdata, weights = n_policyquestions)

stargazer(Reg_lrself_0, Reg_lrself_1, Reg_lrself_2, type="text", omit.stat=c("SER", "F"), omit=c("countrysurveyyear"), star.cutoffs = c(0.05, 0.01, 0.001))


### Appendix E

## Examining different issue areas alone

# Subset data to only include the right issues
table(fulldata_long$q_id)
fulldata_long_civlib <- subset(fulldata_long, q_id %in% c(4, 22, 84, 85, 103, 164, 227))
table(fulldata_long_civlib$q_id)
fulldata_long_econ<- subset(fulldata_long, q_id %in% c(12, 26, 32, 40, 42, 153, 154, 157, 
                                                       159, 177, 178, 184, 218, 221, 222, 
                                                       224, 233, 234, 239, 250))
fulldata_long_immi <- subset(fulldata_long, q_id %in% c(107, 134, 136, 186, 193, 194, 200, 205, 210))
table(fulldata_long_immi$q_id)

#Civil Liberties
fulldata_civlib_individual <- fulldata_long_civlib |> 
  filter(!is.na(value), !is.na(y3_imp)) |>
  summarize(n_policyquestions = n(),
            meancong = mean(congruence, na.rm=TRUE),
            meansupport = mean(value, na.rm=TRUE),
            meanimp = mean(y3_imp, na.rm=TRUE),
            meansupportsq = mean(supportsq, na.rm=TRUE),
            across(.cols=c(AgeCategories, University, woman, rural, income_unified, PolInterested, ethnic_minority,
                           Pol_efficacy, Pol_trust, Satisfied_w_Dem, voted, protest, lrself,
                           contact, attendedmeeting, petition, protest, boycott, donated, internetforum,
                           contactmedia, polwork, otherwork, wornbadge, ethcon, illegalprotest, meeting), ~ first(.x)),
            .by = c(survey2, country, year, own_id)) |> 
  mutate(countrysurveyyear = paste0(country, "_", survey2, "_", as.character(year))) # Create a variable for country-survey-year

Reg_civlib_0 <- lm(meancong ~ voted + protest + as.factor(countrysurveyyear),
                   data=filter(fulldata_civlib_individual, !is.na(income_unified) & !is.na(University) & 
                                 !is.na(woman) & !is.na(AgeCategories) & !is.na(rural)), weights = n_policyquestions)
Reg_civlib_1 <- lm(meancong ~ voted + protest + income_unified + University + woman +
                     eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural +
                     as.factor(countrysurveyyear), data=fulldata_civlib_individual, weights = n_policyquestions)

stargazer(Reg_civlib_0, Reg_civlib_1, type="text", omit.stat=c("SER", "F"), omit=c("countrysurveyyear"), star.cutoffs = c(0.05, 0.01, 0.001))

#Economics
fulldata_econ_individual <- fulldata_long_econ |> 
  filter(!is.na(value), !is.na(y3_imp)) |>
  summarize(n_policyquestions = n(),
            meancong = mean(congruence, na.rm=TRUE),
            meansupport = mean(value, na.rm=TRUE),
            meanimp = mean(y3_imp, na.rm=TRUE),
            meansupportsq = mean(supportsq, na.rm=TRUE),
            across(.cols=c(AgeCategories, University, woman, rural, income_unified, PolInterested, ethnic_minority,
                           Pol_efficacy, Pol_trust, Satisfied_w_Dem, voted, protest, lrself,
                           contact, attendedmeeting, petition, protest, boycott, donated, internetforum,
                           contactmedia, polwork, otherwork, wornbadge, ethcon, illegalprotest, meeting), ~ first(.x)),
            .by = c(survey2, country, year, own_id)) |> 
  mutate(countrysurveyyear = paste0(country, "_", survey2, "_", as.character(year))) # Create a variable for country-survey-year

Reg_economics_0 <- lm(meancong ~ voted + protest + as.factor(countrysurveyyear),
                      data=filter(fulldata_econ_individual, !is.na(income_unified) & !is.na(University) & 
                                    !is.na(woman) & !is.na(AgeCategories) & !is.na(rural)), weights = n_policyquestions)
Reg_economics_1 <- lm(meancong ~ voted + protest + income_unified + University + woman + 
                        eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural +
                        as.factor(countrysurveyyear), data=fulldata_econ_individual, weights = n_policyquestions)

stargazer(Reg_economics_0, Reg_economics_1, type="text", omit.stat=c("SER", "F"), omit=c("countrysurveyyear"), star.cutoffs = c(0.05, 0.01, 0.001))

#Immigration/Ethnic minorities
fulldata_immi_individual <- fulldata_long_immi |> 
  filter(!is.na(value), !is.na(y3_imp)) |>
  summarize(n_policyquestions = n(),
            meancong = mean(congruence, na.rm=TRUE),
            meansupport = mean(value, na.rm=TRUE),
            meanimp = mean(y3_imp, na.rm=TRUE),
            meansupportsq = mean(supportsq, na.rm=TRUE),
            across(.cols=c(AgeCategories, University, woman, rural, income_unified, PolInterested, ethnic_minority,
                           Pol_efficacy, Pol_trust, Satisfied_w_Dem, voted, protest, lrself,
                           contact, attendedmeeting, petition, protest, boycott, donated, internetforum,
                           contactmedia, polwork, otherwork, wornbadge, ethcon, illegalprotest, meeting), ~ first(.x)),
            .by = c(survey2, country, year, own_id)) |> 
  mutate(countrysurveyyear = paste0(country, "_", survey2, "_", as.character(year))) # Create a variable for country-survey-year

Reg_immi_0 <- lm(meancong ~ voted + protest + as.factor(countrysurveyyear),
                 data=filter(fulldata_immi_individual, !is.na(income_unified) & !is.na(University) & 
                               !is.na(woman) & !is.na(AgeCategories) & !is.na(rural)), weights = n_policyquestions)
Reg_immi_1 <- lm(meancong ~ voted + protest + income_unified + University + woman +
                   eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural +
                   as.factor(countrysurveyyear), data=fulldata_immi_individual, weights = n_policyquestions)

stargazer(Reg_immi_0, Reg_immi_1, type="text", omit.stat=c("SER", "F"), omit=c("countrysurveyyear"), star.cutoffs = c(0.05, 0.01, 0.001))


## At the group-policy level

## ISSP Citizen I

# Create a data set for the group-level analysis - Examining protest
table(ISSPCitizenI$RacialPrejMeeting)
ISSPCitizenI_protest1 <- ISSPCitizenI |>
  filter(!is.na(polparticipation_meetorprotest)) |> 
  summarize(mean_RacialPrejMeeting = mean(RacialPrejMeeting, na.rm = TRUE),
            mean_OverthrowGovMeeting = mean(OverthrowGovMeeting, na.rm = TRUE),
            mean_ReligExtMeeting = mean(ReligExtMeeting, na.rm = TRUE),
            .by = c(polparticipation_meetorprotest, country, survey, year))
ISSPCitizenI_protest1 <- ISSPCitizenI_protest1 |> pivot_longer(cols=c('mean_RacialPrejMeeting', 
                                                                      'mean_OverthrowGovMeeting', 
                                                                      'mean_ReligExtMeeting'),
                                                               names_to= 'question',
                                                               values_to= 'Support') # Pivot longer to get all policy proposals as their own observations 

# Apply weights for West Germany and East Germany
ISSPCitizenI_protest1Germany <- ISSPCitizenI_protest1 |>
  filter(country %in% c("DE-W", "DE-E")) |>
  mutate(Support = case_when(
    country == "DE-W" ~ Support * 0.8, # times the weight
    country == "DE-E" ~ Support * 0.2  # times the weight
  )) 

# Summed together to estimate the total German attitude
ISSPCitizenI_protest1Germany <- ISSPCitizenI_protest1Germany |>
  group_by(polparticipation_meetorprotest, survey, year, question) |>
  summarize(Support = mean(Support)) |>
  mutate(country = "DE") |>  # Add country column as "DE"
  ungroup()

# Combine with the original data set and remove East and West German observations
ISSPCitizenI_protest1 <- bind_rows(ISSPCitizenI_protest1, ISSPCitizenI_protest1Germany)
ISSPCitizenI_protest1 <- ISSPCitizenI_protest1 |> filter(country != "DE-E" & country != "DE-W")

# Create a data set where each policy has one observation
ISSPCitizenI_protest_2 <- ISSPCitizenI_protest1 |> # pivot wider so that each group's support has its own column
  filter(!is.na(polparticipation_meetorprotest)) |>
  pivot_wider(names_from = polparticipation_meetorprotest, values_from = Support, values_fill = NULL) |>
  filter(!is.na(`0`) | !is.na(`1`)) |> # remove NAs
  rename(Participator_support = `1`, NON_Participator_support = `0`)

# Adding q_id to the data set
ISSPCitizenI_protest_2 <- ISSPCitizenI_protest_2 |>
  mutate(q_id = case_match(question,
                           "mean_RacialPrejMeeting"  ~ 84,
                           "mean_OverthrowGovMeeting" ~ 85,
                           "mean_ReligExtMeeting" ~ 103))

# Match on y3 implementation variable
ISSPCitizenI_protest_final <- ISSPCitizenI_protest_2 %>% 
  left_join(., impdata_slim, by=c("country", "year", "q_id", "survey"))

# Classify policies into policy categories - for the combined group-level regressions
ISSPCitizenI_protest_final$category <- "Civil Liberties"


## ISSP Citizen II

# Create a data set for the group-level analysis - Examining protest
ISSPCitizenII_protest1 <- ISSPCitizenII |> 
  filter(!is.na(polparticipation_meetorprotest)) |> 
  summarize(mean_RacialPrejMeeting = mean(RacialPrejMeeting, na.rm = TRUE), # here we create mean support for each policy proposal
            mean_OverthrowGovMeeting = mean(OverthrowGovMeeting, na.rm = TRUE),
            mean_ReligExtMeeting = mean(ReligExtMeeting, na.rm = TRUE),
            mean_LongTermResVote = mean(LongTermResVote, na.rm = TRUE),
            mean_RightToNotVote = mean(RightToNotVote, na.rm = TRUE),
            .by = c(polparticipation_meetorprotest, country, survey, year))

ISSPCitizenII_protest1 <- ISSPCitizenII_protest1 |> pivot_longer(cols=c('mean_RacialPrejMeeting', 
                                                                        'mean_OverthrowGovMeeting', 
                                                                        'mean_ReligExtMeeting',
                                                                        'mean_LongTermResVote',
                                                                        'mean_RightToNotVote'),
                                                                 names_to= 'question',
                                                                 values_to= 'Support') # pivot longer to get all policy proposals as their own observations 


# Create a data set where each policy has one observation
ISSPCitizenII_protest_2 <- ISSPCitizenII_protest1 |> # pivot wider so that each group's support has its own column
  filter(!is.na(polparticipation_meetorprotest)) |>
  pivot_wider(names_from = polparticipation_meetorprotest, values_from = Support, values_fill = NULL) |>
  filter(!is.na(`0`) | !is.na(`1`)) |> # remove NAs
  rename(Participator_support = `1`, NON_Participator_support = `0`)

# Adding q_id to the data set
ISSPCitizenII_protest_2 <- ISSPCitizenII_protest_2 |>
  mutate(q_id = case_match(question,
                           "mean_RacialPrejMeeting"  ~ 84,
                           "mean_OverthrowGovMeeting" ~ 85,
                           "mean_ReligExtMeeting" ~ 103,
                           "mean_LongTermResVote" ~ 4,
                           "mean_RightToNotVote" ~ 22))

# Match on y3 implementation variable
ISSPCitizenII_protest_final <- ISSPCitizenII_protest_2 %>% 
  left_join(., impdata_slim, by=c("country", "year", "q_id", "survey"))

# Classify policies into policy categories
ISSPCitizenII_protest_final$category <- "Civil Liberties"

# Combining the two ISSP Citizen data sets
ISSPCitizenI_protest_final <- ISSPCitizenI_protest_final |> select(where(~any(!is.na(.))))
ISSPCitizen_protest_final <- rbind(ISSPCitizenI_protest_final, ISSPCitizenII_protest_final)


## ESS

# Create a data set for the group-level analysis - Examining Protest
policy_varname_vec <- c("disclaw", "anycrime", "seriouscrime", "yesrefugeefamily", 
                        "hatelaw", "noownschools", "supportrefapp", "refappcanwork", 
                        "ifunemployedleave", "overthrowdem", "keepterrorsuspect", "paidleavefamilysick", 
                        "higherearnershigherunemp", "higherearnershigherpen", "benefitsonlyforpoor",
                        "basicincomescheme", "banappliances", "fossilfueltaxincrease", "subrenew")
table(ess$protest)
ess_protest1 <- ess |> # here we create mean support for each policy proposal
  summarize(across(all_of(policy_varname_vec), ~mean(., na.rm = TRUE), .names = "mean_{.col}"),
            .by = c(protest, cntry, year, survey))
table(ess_protest1$protest)
ess_protest1 <- ess_protest1 |> pivot_longer(cols=c('mean_disclaw', 'mean_anycrime', 'mean_seriouscrime', 
                                                    'mean_yesrefugeefamily', 'mean_hatelaw', 'mean_noownschools', 
                                                    'mean_supportrefapp', 'mean_refappcanwork', 
                                                    'mean_ifunemployedleave', 'mean_overthrowdem', 
                                                    'mean_keepterrorsuspect', 'mean_paidleavefamilysick',
                                                    'mean_higherearnershigherpen', 'mean_higherearnershigherunemp',
                                                    'mean_fossilfueltaxincrease', 'mean_subrenew', 'mean_banappliances',
                                                    'mean_benefitsonlyforpoor', 'mean_basicincomescheme'),
                                             names_to= 'question',
                                             values_to= 'Support') # pivot longer to get all policy proposals as their own observations 

ess_protest_2 <- ess_protest1 |> # pivot wider so that each group's support has its own column
  filter(!is.na(protest)) |>
  pivot_wider(names_from = protest, values_from = Support, values_fill = NULL) |>
  filter(!is.na(`0`) | !is.na(`1`)) |> # remove NAs
  rename(Participator_support = `1`, NON_Participator_support = `0`)

# Adding q_id to the data set
ess_protest_2 <- ess_protest_2 |>
  mutate(q_id = case_match(question,
                           "mean_disclaw"  ~ 107,
                           "mean_anycrime" ~ 186,
                           "mean_seriouscrime" ~ 193,
                           "mean_yesrefugeefamily" ~ 194,
                           "mean_hatelaw" ~ 136,
                           "mean_noownschools" ~ 134,
                           "mean_supportrefapp" ~ 200,
                           "mean_refappcanwork" ~ 205,
                           "mean_ifunemployedleave" ~ 210,
                           "mean_overthrowdem" ~ 164,
                           "mean_keepterrorsuspect" ~ 227,
                           "mean_paidleavefamilysick" ~ 154,
                           "mean_higherearnershigherunemp" ~ 250,
                           "mean_higherearnershigherpen" ~ 239,
                           "mean_benefitsonlyforpoor" ~ 177,
                           "mean_basicincomescheme" ~ 42,
                           "mean_banappliances" ~ 158,
                           "mean_fossilfueltaxincrease" ~ 159,
                           "mean_subrenew" ~ 178))

# Match on y3 implementation data
ess_protest_final <- ess_protest_2 %>%  
  mutate(country = cntry)%>% 
  left_join(., impdata_slim, by=c("country", "year", "q_id", "survey"))

# Define mapping between question names and categories
category_mapping <- c(
  "mean_anycrime" = "Immigration/Ethnicity",
  "mean_disclaw" = "Immigration/Ethnicity",
  "mean_hatelaw" = "Immigration/Ethnicity",
  "mean_ifunemployedleave" = "Immigration/Ethnicity",
  "mean_noownschools" = "Immigration/Ethnicity",
  "mean_refappcanwork" = "Immigration/Ethnicity",
  "mean_seriouscrime" = "Immigration/Ethnicity",
  "mean_supportrefapp" = "Immigration/Ethnicity",
  "mean_yesrefugeefamily" = "Immigration/Ethnicity",
  "mean_basicincomescheme" = "Economics",
  "mean_benefitsonlyforpoor" = "Economics",
  "mean_higherearnershigherpen" = "Economics",
  "mean_higherearnershigherunemp" = "Economics",
  "mean_paidleavefamilysick" = "Economics",
  "mean_banappliances" = "Environment",
  "mean_fossilfueltaxincrease" = "Environment",
  "mean_subrenew" = "Environment",
  "mean_keepterrorsuspect" = "Civil Liberties",
  "mean_overthrowdem" = "Civil Liberties"
)

# Apply categories
ess_protest_final$category <- category_mapping[ess_protest_final$question]
table(ess_protest_final$category)


## ISSP RoG III

# Create a data set for the group-level analysis - Examining protest

# Vector of policy questions
policy_varname_vec <- c("cutspending", "govcreatejobs", "lessregulation", "reduceworkweek", "controlwages",
                        "healthspending", "educationspending", "environmentspending", "oldagepensionsspending",
                        "culturespending", "unemploymentspending", "militaryspending", "policespending",
                        "banksprivate", "hospitalsprivate", "electricityprivate")

# Create a protest data set for the group level
table(ISSPRoGIII$polparticipation_dum)
ISSPRoGIII_protest1 <- ISSPRoGIII |>
  filter(!is.na(polparticipation_dum)) |> 
  summarize(across(all_of(policy_varname_vec), ~mean(., na.rm = TRUE)),
            .by = c(polparticipation_dum, country, survey, year)) |> 
  pivot_longer(cols = all_of(policy_varname_vec),
               names_to = 'question',
               values_to = 'Support')

# Apply weights for West Germany and East Germany
ISSPRoGIII_protest1Germany <- ISSPRoGIII_protest1 |>
  filter(country %in% c("DE-W", "DE-E")) |>
  mutate(Support = case_when(
    country == "DE-W" ~ Support * 0.8,
    country == "DE-E" ~ Support * 0.2
  )) 

# Summed together to estimate the total German attitude
ISSPRoGIII_protest1Germany <- ISSPRoGIII_protest1Germany |>
  summarize(Support = mean(Support),
            .by = c(polparticipation_dum, survey, year, question)) |>
  mutate(country = "DE") # Add country column as "DE"

# Combine with the original data set and remove East and West German observations
ISSPRoGIII_protest1 <- bind_rows(ISSPRoGIII_protest1, ISSPRoGIII_protest1Germany)
ISSPRoGIII_protest1 <- ISSPRoGIII_protest1 |> filter(country != "DE-E" & country != "DE-W")

# Create a data set where each policy has one observation
ISSPRoGIII_protest_2 <- ISSPRoGIII_protest1 |> # Pivot wider so that each group's support has its own column
  filter(!is.na(polparticipation_dum)) |>
  pivot_wider(names_from = polparticipation_dum, values_from = Support, values_fill = NULL) |>
  filter(!is.na(`0`) | !is.na(`1`)) |> #Remove NAs
  rename(Participator_support = `1`, NON_Participator_support = `0`)

# Adding q_id to the data set
ISSPRoGIII_protest_2 <- ISSPRoGIII_protest_2 |>
  mutate(q_id = case_match(question,
                           "cutspending" ~ 12,
                           "govcreatejobs" ~ 40,
                           "lessregulation" ~ 26,
                           "reduceworkweek" ~ 32,
                           "controlwages" ~ 184,
                           "healthspending" ~ 218,
                           "educationspending" ~ 221,
                           "environmentspending" ~ 222,
                           "oldagepensionsspending" ~ 224,
                           "culturespending" ~ 231,
                           "unemploymentspending" ~ 233,
                           "militaryspending" ~ 234,
                           "policespending" ~ 237,
                           "banksprivate" ~ 153,
                           "hospitalsprivate" ~ 157,
                           "electricityprivate" ~ 161))

ISSPRoGIII_protest_final <- ISSPRoGIII_protest_2 %>% 
  left_join(., impdata_slim, by=c("country", "year", "q_id", "survey"))

# Classify policies into policy categories
ISSPRoGIII_protest_final$category <- "Economics"


## Combining data sets to do issue-specific and full analysis

# Create one big data set
common_cols_protest <- Reduce(intersect, list(names(ISSPRoGIII_protest_final), 
                                              names(ess_protest_final),
                                              names(ISSPCitizen_protest_final)
))

BigGroup_Imp_protest <- bind_rows(
  ISSPRoGIII_protest_final |> select(all_of(common_cols_protest)),
  ess_protest_final |> select(all_of(common_cols_protest)),
  ISSPCitizen_protest_final |> select(all_of(common_cols_protest))
)

# Issue-specific regressions
BigGroup_Imp_protest_Economics <- subset(BigGroup_Imp_protest, category == "Economics")
BigGroup_Imp_protest_Civil_Liberties <- subset(BigGroup_Imp_protest, category == "Civil Liberties")
BigGroup_Imp_protest_Immigration_Ethnicity <- subset(BigGroup_Imp_protest, category == "Immigration/Ethnicity")

# Civil liberties
Reg1 <- lm(y3_imp ~ Participator_support + as.factor(country), data = BigGroup_Imp_protest_Civil_Liberties)
Reg2 <- lm(y3_imp ~ NON_Participator_support + as.factor(country), data = BigGroup_Imp_protest_Civil_Liberties)
Reg3 <- lm(y3_imp ~ Participator_support + NON_Participator_support + as.factor(country), data = BigGroup_Imp_protest_Civil_Liberties)
stargazer(Reg1, Reg2, Reg3, type = "text", 
          title = "Politcal Participation and Opinion-Poliy Congruence - Civil Liberties",
          covariate.labels = c("Political Participator Support","Non-Political Participator Support","Constant"),
          style = "apsr", star.char = c("*", "**"), star.cutoffs = c(.05, .01), notes = c("*p < .05; **p < .01"), 
          notes.append = FALSE, digits = 2,  omit = c("country", "q_id"),
          dep.var.labels = "Implementation", column.sep.width = "1pt", omit.stat=c("LL","ser","f"))

# Economics
Reg1 <- lm(y3_imp ~ Participator_support + as.factor(country), data = BigGroup_Imp_protest_Economics)
Reg2 <- lm(y3_imp ~ NON_Participator_support + as.factor(country), data = BigGroup_Imp_protest_Economics)
Reg3 <- lm(y3_imp ~ Participator_support + NON_Participator_support + as.factor(country), data = BigGroup_Imp_protest_Economics)
stargazer(Reg1, Reg2, Reg3, type = "text", 
          title = "Politcal Participation and Opinion-Poliy Congruence - Economics",
          covariate.labels = c("Political Participator Support","Non-Political Participator Support","Constant"),
          style = "apsr", star.char = c("*", "**"), star.cutoffs = c(.05, .01), notes = c("*p < .05; **p < .01"), 
          notes.append = FALSE, digits = 2,  omit = c("country", "q_id"),
          dep.var.labels = "Implementation", column.sep.width = "1pt", omit.stat=c("LL","ser","f"))

# Immigration and ethnic minority questions
Reg1 <- lm(y3_imp ~ Participator_support + as.factor(country), data = BigGroup_Imp_protest_Immigration_Ethnicity)
Reg2 <- lm(y3_imp ~ NON_Participator_support + as.factor(country), data = BigGroup_Imp_protest_Immigration_Ethnicity)
Reg3 <- lm(y3_imp ~ Participator_support + NON_Participator_support + as.factor(country), data = BigGroup_Imp_protest_Immigration_Ethnicity)
stargazer(Reg1, Reg2, Reg3, type = "text", 
          title = "Politcal Participation and Opinion-Poliy Congruence - Immigration/Ethnicity",
          covariate.labels = c("Political Participator Support","Non-Political Participator Support","Constant"),
          style = "apsr", star.char = c("*", "**"), star.cutoffs = c(.05, .01), notes = c("*p < .05; **p < .01"), 
          notes.append = FALSE, digits = 2,  omit = c("country", "q_id"),
          dep.var.labels = "Implementation", column.sep.width = "1pt", omit.stat=c("LL","ser","f"))

# Combining all categories
Reg1 <- lm(y3_imp ~ Participator_support, data = BigGroup_Imp_protest)
Reg2 <- lm(y3_imp ~ NON_Participator_support, data = BigGroup_Imp_protest)
Reg3 <- lm(y3_imp ~ Participator_support + NON_Participator_support, data = BigGroup_Imp_protest)
Reg4 <- lm(y3_imp ~ Participator_support + as.factor(country), data = BigGroup_Imp_protest)
Reg5 <- lm(y3_imp ~ NON_Participator_support + as.factor(country), data = BigGroup_Imp_protest)
Reg6 <- lm(y3_imp ~ Participator_support + NON_Participator_support + as.factor(country), data = BigGroup_Imp_protest)
stargazer(Reg1, Reg2, Reg3, Reg4, Reg5, Reg6, type = "text", 
          title = "Politcal Participation and Opinion-Poliy Congruence",
          covariate.labels = c("Political Participator Support","Non-Political Participator Support","Constant"),
          style = "apsr", star.char = c("*", "**"), star.cutoffs = c(.05, .01), notes = c("*p < .05; **p < .01"), 
          notes.append = FALSE, digits = 2,  omit = c("country", "q_id"),
          dep.var.labels = "Implementation", column.sep.width = "1pt", omit.stat=c("LL","ser","f"))


### Appendix F

## Create graphs for difference in support for different policies among the different groups
# Protest
Difference_in_Support_data_protest <- fulldata_long |>
  select(country, q_id, protest, value) |> 
  filter(!is.na(protest)) |> 
  summarize(support = mean(value, na.rm=TRUE),
            .by = c(country, q_id, protest)) |> 
  left_join(dplyr::select(questionwordings, c(q_id, shortwording)))

diff_data_protest <- Difference_in_Support_data_protest |>
  pivot_wider(names_from = protest,
              values_from=support, id_cols = c(q_id, country, shortwording)) |>  
  mutate(diff = `1`-`0`,
         meanvalue = `1`*0.5 + `0`*0.5,
         ave_policy_support_Participator = `1`-meanvalue,
         ave_policy_support_NON_Participator = `0`-meanvalue) |> 
  summarize(ave_policy_support_Participator = mean(ave_policy_support_Participator, na.rm = T),
            ave_policy_support_NON_Participator = mean(ave_policy_support_NON_Participator, na.rm = T),
            diff = mean(diff, na.rm = T),
            meanvalue = mean(meanvalue, na.rm = T),
            .by = c(q_id, shortwording))  |> 
  arrange(desc(diff)) |> 
  mutate(order = seq(n(), 1))

diff_data_protest |>
  ggplot(aes(y = order)) +
  geom_segment(aes(x = ave_policy_support_Participator, xend = ave_policy_support_NON_Participator, yend = order)) +
  geom_point(aes(x = ave_policy_support_Participator), col = "red") +
  geom_point(aes(x = ave_policy_support_NON_Participator), col = "blue") +
  geom_text(aes(label = shortwording), x = 0.25, hjust = 0) +
  theme_minimal() +
  labs(x = "Difference in policy support", y = element_blank()) +
  scale_x_continuous(breaks = c(-0.2, 0, 0.2), limits = c(-0.15, 0.55)) +
  scale_y_continuous(limits = c(0, 44)) +
  theme(axis.text.y = element_blank()) +
  annotate("segment", y = 43, yend = 40.5, x = -0.065, xend = -0.065, arrow = arrow(length = unit(3, "mm"))) +
  annotate("segment", y = 43, yend = 40.5, x = 0.065, xend = 0.065, arrow = arrow(length = unit(3, "mm"))) +
  annotate("text", label = c("Non-Demonstrators", "Demonstrators"), y = 43.5, x = c(-0.08, 0.08), hjust = 0.5)

# Vote
Difference_in_Support_data_vote <- fulldata_long |>
  select(country, q_id, voted, value) |> 
  filter(!is.na(voted)) |> 
  summarize(support = mean(value, na.rm=TRUE),
            .by = c(country, q_id, voted)) |> 
  left_join(dplyr::select(questionwordings, c(q_id, shortwording)))

diff_data_vote <- Difference_in_Support_data_vote |>
  pivot_wider(names_from = voted,
              values_from=support, id_cols = c(q_id, country, shortwording)) |>  
  mutate(diff = `1`-`0`,
         meanvalue = `1`*0.5 + `0`*0.5,
         ave_policy_support_Participator = `1`-meanvalue,
         ave_policy_support_NON_Participator = `0`-meanvalue) |> 
  summarize(ave_policy_support_Participator = mean(ave_policy_support_Participator, na.rm = T),
            ave_policy_support_NON_Participator = mean(ave_policy_support_NON_Participator, na.rm = T),
            diff = mean(diff, na.rm = T),
            meanvalue = mean(meanvalue, na.rm = T),
            .by = c(q_id, shortwording))  |> 
  arrange(desc(diff)) |> 
  mutate(order = seq(n(), 1))

diff_data_vote |>
  ggplot(aes(y = order)) +
  geom_segment(aes(x = ave_policy_support_Participator, xend = ave_policy_support_NON_Participator, yend = order)) +
  geom_point(aes(x = ave_policy_support_Participator), col = "red") +
  geom_point(aes(x = ave_policy_support_NON_Participator), col = "blue") +
  geom_text(aes(label = shortwording), x = 0.25, hjust = 0) +
  theme_minimal() +
  labs(x = "Difference in policy support", y = element_blank()) +
  scale_x_continuous(breaks = c(-0.20, 0, 0.20), limits = c(-0.15, 0.55)) +
  scale_y_continuous(limits = c(0, 44)) +
  theme(axis.text.y = element_blank()) +
  annotate("segment", y = 43, yend = 40.5, x = -0.025, xend = -0.025, arrow = arrow(length = unit(3, "mm"))) +
  annotate("segment", y = 43, yend = 40.5, x = 0.025, xend = 0.025, arrow = arrow(length = unit(3, "mm"))) +
  annotate("text", label = c("Non-Voters", "Voters"), y = 43.5, x = c(-0.035, 0.035), hjust = 0.5)


### Appendix G

# Differences between groups concerning indifferent answers
prop.table(table(fulldata_long$protest, fulldata_long$value), 1)
prop.table(table(fulldata_long$voted, fulldata_long$value), 1)

# To produce the other tables, search for "comment" or "uncomment" to see which code lines need to be changed (also in main code).
# After commenting and uncommenting the relevant code, re-run the whole code and then run the code producing Table 1 and Table 3.
# When removing don't know and non-answers, the code for Table 1 and 3 produces Table G.1 and Table G.2 respectively.
# When removing indifferent answers, the code for Table 1 and 3 produces Table G.3 and Table G.4 respectively.


### End of Appendix Analyses ###

### End of Document ###