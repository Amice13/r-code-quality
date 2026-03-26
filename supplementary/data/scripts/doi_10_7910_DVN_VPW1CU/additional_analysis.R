#### Additional analysis ####

## This script produces additional analysis results presented in sections D and E of the supplementary materials
## These test various alternative explanations for the main findings, drawing on a range of datasets
## First, it produces and saves full results tables (tables SM39-45)
## Then it produces the relevant figures presented (figures SM12-13)

## Set up 

rm(list=ls())

library(dplyr)
library(fixest)
library(tidyr)
library(broom)
library(ggplot2)
library(purrr)
library(gridExtra)

## Section D.1 - Economic development

load("inside_full_dhs_analysis.RData")
load("nightlights_long_analysis.RData")

inside_full_dhs_analysis <- inside_full_dhs_analysis %>%
  rename(country_name = country)

econ1 <- feols(clean_wealth_index ~ inside + ever_inside +
                 urbrur | country_name + year,
               cluster = ~ea_code, 
               data = inside_full_dhs_analysis)

econ2 <- feols(clean_wealth_index ~ inside + ever_inside +
                 urbrur | country_name[year] + year,
               cluster = ~ea_code, 
               data = inside_full_dhs_analysis)

econ3 <- feols(nl_log ~ inside | unique_id + year, 
               data=nightlights_long_analysis, 
               cluster = ~unique_id)

econ4 <- feols(nl_log ~ inside | unique_id + year + country_name[year], 
               data=nightlights_long_analysis, 
               cluster = ~unique_id)

## Make table SM39

setFixest_dict(inside = "Enter coverage", 
               unique_id = "Enumeration area", 
               ea_code = "Enumeration area", 
               year = "Year", 
               country_name = "Country", 
               nl_log = "Nightlights (log)", 
               clean_wealth_index = "DHS wealth index (quintile)")

tab_SM39 <- etable(econ1, econ2, econ3, econ4,
       title = "Mobile coverage and DHS wealth index",
       keep = c("Enter coverage"), 
       signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
       digits = 3,
       digits.stats = 3,
       fontsize = "tiny",
       convergence = FALSE,
       tex=T, 
       notes = "Notes: Marginal effect of mobile coverage on DHS wealth index and logged VIIRS nightlights. TWFE specifications, with year and country or enumeration area FEs.")

## Section D.2 - Mobilisation and participation 

ABcombined_rural <- readRDS("ABcombined.rds") %>% 
  filter(urbrur == "Rural")

### TWFE

d2_1 <- feols(c(attend_community, raise_issue, protest) ~ inside + ever_inside + 
                       news_radio + news_tv + news_paper + 
                       election_year + nightlights_present +
                       cash_income + food + water + medical + age + gender + education | 
                       country_name + year, 
      data = ABcombined_rural, 
      vcov = ~ea_code)

d2_2 <- feols(c(attend_community, raise_issue, protest) ~ inside + ever_inside + 
                news_radio + news_tv + news_paper + 
                election_year + nightlights_present +
                cash_income + food + water + medical + age + gender + education | 
                country_name[year] + year, 
              data = ABcombined_rural, 
              vcov = ~ea_code)


##### If have access to data, run the code below ###
###### If do not have access, go to next chunk to read in pre-loaded results ###

#d2_3 <- feols(c(attend_community, raise_issue, protest) ~ inside + ever_inside + 
                #news_radio + news_tv + news_paper + 
                #election_year + nightlights_present +
                #cash_income + food + water + medical + age + gender + education | 
                #country_name[year] + year, 
              #data = ABcombined_rural, 
              #conley(50, distance="spherical"))

#twfe_models_d2 <- list(d2_3)
#saveRDS(d2_3, file = "twfe_models_d2.rds")

d2_3 <- readRDS("twfe_models_d2.rds")

### Sun Abraham (run separately for each outcome, as doesn't work with c() as above)

d2_4a <- feols(attend_community ~
                sunab(first_inside, time_to_inside) + 
                news_radio + news_tv + news_paper + 
                election_year + nightlights_present +
                cash_income + food + water + medical + age + gender + education | 
                country_name + year, 
              data = ABcombined_rural, 
              vcov = ~ea_code) %>% summary(agg="ATT")
  

d2_4b <- feols(raise_issue ~ 
                sunab(first_inside, time_to_inside) + 
                news_radio + news_tv + news_paper + 
                election_year + nightlights_present +
                cash_income + food + water + medical + age + gender + education | 
                country_name + year, 
              data = ABcombined_rural, 
              vcov = ~ea_code) %>% summary(agg="ATT")

d2_4c <- feols(protest ~ 
                sunab(first_inside, time_to_inside) + 
                news_radio + news_tv + news_paper + 
                election_year + nightlights_present +
                cash_income + food + water + medical + age + gender + education | 
                country_name + year, 
              data = ABcombined_rural, 
              vcov = ~ea_code) %>% summary(agg="ATT")

d2_5a <- feols(attend_community ~
                 sunab(first_inside, time_to_inside) + 
                 news_radio + news_tv + news_paper + 
                 election_year + nightlights_present +
                 cash_income + food + water + medical + age + gender + education | 
                 country_name[year] + year, 
               data = ABcombined_rural, 
               vcov = ~ea_code) %>% summary(agg="ATT")

d2_5b <- feols(raise_issue ~ 
                 sunab(first_inside, time_to_inside) + 
                 news_radio + news_tv + news_paper + 
                 election_year + nightlights_present +
                 cash_income + food + water + medical + age + gender + education | 
                 country_name[year] + year, 
               data = ABcombined_rural, 
               vcov = ~ea_code) %>% summary(agg="ATT")

d2_5c <- feols(protest ~ 
                 sunab(first_inside, time_to_inside) + 
                 news_radio + news_tv + news_paper + 
                 election_year + nightlights_present +
                 cash_income + food + water + medical + age + gender + education | 
                 country_name[year] + year, 
               data = ABcombined_rural, 
               vcov = ~ea_code) %>% summary(agg="ATT")

##### If have access to data, run the code below ###
###### If do not have access, go to next chunk to read in pre-loaded results ###

#d2_6a <- feols(attend_community ~
                 #sunab(first_inside, time_to_inside) + 
                 #news_radio + news_tv + news_paper + 
                 #election_year + nightlights_present +
                 #cash_income + food + water + medical + age + gender + education | 
                 #country_name[year] + year, 
               #data = ABcombined_rural, 
               #conley(50, distance="spherical")) %>% summary(agg="ATT")

#d2_6b <- feols(raise_issue ~ 
                 #sunab(first_inside, time_to_inside) + 
                 #news_radio + news_tv + news_paper + 
                 #election_year + nightlights_present +
                 #cash_income + food + water + medical + age + gender + education | 
                 #country_name[year] + year, 
               #data = ABcombined_rural, 
               #conley(50, distance="spherical")) %>% summary(agg="ATT")

#d2_6c <- feols(protest ~ 
                 #sunab(first_inside, time_to_inside) + 
                 #news_radio + news_tv + news_paper + 
                 #election_year + nightlights_present +
                 #cash_income + food + water + medical + age + gender + education | 
                 #country_name[year] + year, 
               #data = ABcombined_rural, 
               #conley(50, distance="spherical")) %>% summary(agg="ATT")

#sa_models_d2 <- list(d2_6a, d2_6b, d2_6c)
#saveRDS(sa_models_d2, file = "sa_models_d2.rds")

sa_models_d2 <- readRDS("sa_models_d2.rds")

### Make and save tables

setFixest_dict(c(inside = "Enter coverage",
                 attend_community = "Community meeting", 
                 raise_issue = "Raise issue", 
                 protest = "Protest",
                 country_name = "Country",
                 year = "Year",
                 round = "Survey round",
                 ea_code = "Enumeration area",
                 ATT = "Aggregated ATT"))

tab_SM40 <- etable(
  d2_1[1], d2_2[1], d2_3[1],
  d2_4a, d2_5a, sa_models_d2[1],
  title = "Mobile coverage and political participation (community meetings)",
  headers = list("^:_:Estimator" = list("TWFE" = 3, "Sun Abraham" = 3)),
  keep = c("Enter", "ATT"), 
  signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
  digits = 3,
  digits.stats = 3,
  fontsize = "tiny",
  convergence = FALSE,
  tex=T, 
  notes = "Notes: Marginal effect of mobile phone coverage on political participation (attend community meetings). Data, specification and controls as described in table A2, using alternative outcome variables.")

tab_SM41 <- etable(
  d2_1[2], d2_2[2], d2_3[2],
  d2_4b, d2_5b, sa_models_d2[2],
  title = "Mobile coverage and political participation (raise issue)",
  headers = list("^:_:Estimator" = list("TWFE" = 3, "Sun Abraham" = 3)),
  keep = c("Enter", "ATT"), 
  signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
  digits = 3,
  digits.stats = 3,
  fontsize = "tiny",
  convergence = FALSE,
  tex=T, 
  notes = "Notes: Marginal effect of mobile phone coverage on political participation (join others to raise an issue). Data, specification and controls as described in table A2, using alternative outcome variables.")

tab_SM42 <- etable(
  d2_1[3], d2_2[3], d2_3[3],
  d2_4c, d2_5c, sa_models_d2[3],
  title = "Mobile coverage and political participation (protest)",
  headers = list("^:_:Estimator" = list("TWFE" = 3, "Sun Abraham" = 3)),
  keep = c("Enter", "ATT"), 
  signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
  digits = 3,
  digits.stats = 3,
  fontsize = "tiny",
  convergence = FALSE,
  tex=T, 
  notes = "Notes: Marginal effect of mobile phone coverage on political participation (attend a protest or demonstration). Data, specification and controls as described in table A2, using alternative outcome variables.")


## Section D.3 - Clientelism

## Load Ghana survey data

load("gha_survey.RData")

## Baseline list experiment

list1 <- feols(clientelism_count ~ clientelism_treat_bin, data=gha_survey_analysis)
list2 <- feols(clientelism_count ~ clientelism_treat_bin + 
                 age + gender + education + vote2020_inc + region, 
               data=gha_survey_analysis)

## Heterogeneity by urban contact

list3 <- feols(clientelism_count ~ i(contact_urban_week, clientelism_treat_bin) + 
                 contact_urban_week, data=gha_survey_analysis)
list4 <- feols(clientelism_count ~ i(contact_urban_week, clientelism_treat_bin) + 
                 contact_urban_week + age + gender + education + vote2020_inc + region, data=gha_survey_analysis)

## Heterogeneity by sensitive discussion topics

list5 <- feols(clientelism_count ~ i(phone_politics_bin, clientelism_treat_bin) + 
                 phone_politics_bin, data=gha_survey_analysis)
list6 <- feols(clientelism_count ~ i(phone_politics_bin, clientelism_treat_bin) + 
                 phone_politics_bin + age + gender + education + vote2020_inc + region, data=gha_survey_analysis)

list7 <- feols(clientelism_count ~ i(phone_econ_bin, clientelism_treat_bin) + 
                 phone_econ_bin, data=gha_survey_analysis) 
list8 <- feols(clientelism_count ~ i(phone_econ_bin, clientelism_treat_bin) + 
                 phone_econ_bin + age + gender + education + vote2020_inc + region, data=gha_survey_analysis) 

list9 <- feols(clientelism_count ~ i(phone_corruption_bin, clientelism_treat_bin) + 
                 phone_corruption_bin, data=gha_survey_analysis)
list10 <- feols(clientelism_count ~ i(phone_corruption_bin, clientelism_treat_bin) + 
                  phone_corruption_bin + age + gender + education + vote2020_inc + region, data=gha_survey_analysis)

## Remittances
list11 <- feols(clientelism_count ~ i(domestic_remit_bin, clientelism_treat_bin) + 
                  domestic_remit_bin, data=gha_survey_analysis)
list12 <- feols(clientelism_count ~ i(domestic_remit_bin, clientelism_treat_bin) + 
                  domestic_remit_bin + age + gender + education + vote2020_inc + region, data=gha_survey_analysis)


### Make table SM43-44

### Set dictionary
setFixest_dict(c(clientelism_count = "List item count", 
                 clientelism_treat_bin = "Treatment",
                 contact_urban_week = "Contact",
                 phone_politics_bin = "Discussion",
                 phone_econ_bin = "Discussion",
                 phone_corruption_bin = "Discussion",
                 domestic_remit_bin = "Remittances"))

tab_SM43 <- etable(list1, list3, 
                       list5, list7, 
                       list9,  list11,
                       title = "Clientelism list experiment results (raw treatment effects)",
                       headers = list("Baseline" = 1, 
                                      "Contact urban" = 1, 
                                      "Politics" = 1, 
                                      "Economy" = 1, 
                                      "Corruption" = 1, 
                                      "Remittances" = 1),
                       keep = c("Treatment"), 
                       signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
                       digits = 3,
                       digits.stats = 3,
                       fontsize = "tiny",
                       convergence = FALSE,
                       tex=T, 
                       notes = "Notes: Baseline treatment effects from clientelism list experiment in Ghana, using original author survey.")

tab_SM44 <- etable(list2, list4, 
                       list6, list8, 
                       list10,  list12,
                       title = "Clientelism list experiment results (including demographic covariates)",
                       headers = list("Baseline" = 1, 
                                      "Contact urban" = 1, 
                                      "Politics" = 1, 
                                      "Economy" = 1, 
                                      "Corruption" = 1, 
                                      "Remittances" = 1),
                       keep = c("Treatment"), 
                       signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
                       digits = 3,
                       digits.stats = 3,
                       fontsize = "tiny",
                       convergence = FALSE,
                       tex=T, 
                       notes = "Notes: Treatment effects from clientelism list experiment in Ghana, using original author survey. All specifications control for remittances, age, gender, education, previous vote choice, and include region FEs.")


## Section D.4 - Outward migration

### Load HH panel data

load("gh_hh_panel.RData")

### Run models 

count1 <- feols(hh_count_urban ~ mobile | FPrimary + wave,
                data=combined_hh_rural_analysis)

count2 <- feols(hh_count_urban_alt ~ mobile | FPrimary + wave,
                data=combined_hh_rural_analysis)

count3 <- feols(hh_count_urban ~ mobile | FPrimary + wave,
                data=combined_hh_rural1_12)

count4 <- feols(hh_count_urban_alt ~ mobile | FPrimary + wave,
                data=combined_hh_rural1_12)

count5 <- feols(hh_count_urban ~ mobile | FPrimary + wave,
                data=combined_hh_rural1_23)

count6 <- feols(hh_count_urban_alt ~ mobile | FPrimary + wave,
                data=combined_hh_rural1_23)

setFixest_dict(c(mobile = "Mobile", 
                 FPrimary = "Household",
                 wave = "Survey wave",
                 hh_count_urban = "Count",
                 hh_count_urban_alt = "Count"))

tab_SM45 <- etable(count1, count3, count5, 
       count2, count4, count6,
       title = "Mobile phone access and outward migration",
       keep = "Mobile",
       signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
       digits = 3,
       digits.stats = 2,
       fitstat = c("n", "r2"),
       headers = list("^:_:Type" = list("Total urban relatives" = 3, 
                                        "Accra/Kumasi only" = 3)),
       extralines=list("_^Sample"=c("Full", "Wave 1-2", "Wave 2-3",
                                    "Full", "Wave 1-2", "Wave 2-3")),
       fontsize = "tiny",
       tex=T,
       label = "tab:mobile-to-urban-contact-tabs-I", 
       notes = "Notes: Marginal effect of mobile phone access on the number of urban relatives named by a rural household. Data from rural households in the Ghana socioeconomic panel")

#### Save tables

save(tab_SM39, 
     tab_SM40, tab_SM41, tab_SM42,
     tab_SM43, tab_SM44, 
     tab_SM45, file = "additional_tabs.RData")


#### Figures

## Section D.1 - Economic development

rm(list=ls())

load("inside_full_dhs_analysis.RData")
load("nightlights_long_analysis.RData")

inside_full_dhs_analysis <- inside_full_dhs_analysis %>%
  rename(country_name = country)

### Analysis 2: Country by country

results1 <- inside_full_dhs_analysis %>%
  filter(urbrur == "R") %>% 
  group_by(country_name) %>%
  nest() %>%
  mutate(model = map(data, ~ feols(clean_wealth_index ~ inside + ever_inside  |
                                     year,
                                   cluster = ~ea_code, data = .x)),
         tidied = map(model, tidy)) %>%
  unnest(tidied)

results2 <- nightlights_long_analysis %>%
  group_by(country_name) %>%
  nest() %>%
  mutate(model = map(data, ~ feols(nl_log ~ inside + ever_inside +
                                     as.numeric(year) |
                                     unique_id + year,
                                   cluster = ~unique_id, data = .x)),
         tidied = map(model, broom::tidy)) %>%
  unnest(tidied)

# Extract coefficients related to the variables of interest
coefficients1 <- results1 %>%
  filter(term %in% c("inside")) %>%
  dplyr::select(country_name, term, estimate, std.error) %>%
  mutate(outcome = "a) DHS wealth index\n")

coefficients2 <- results2 %>%
  filter(term %in% c("inside")) %>%
  dplyr::select(country_name, term, estimate, std.error) %>%
  mutate(outcome = "b) Nightlights\n")


# Plot the coefficients

plot1 <- ggplot(coefficients1, aes(x = reorder(country_name, estimate), y = estimate)) +
  geom_point() +
  geom_hline(yintercept = 0, lty = "dashed") +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), 
                width = 0.2) +
  labs(y = "\nATT") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~ factor(outcome), scales = "free") +
  coord_flip() + 
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), 
        axis.title.y = element_blank()) +
  theme(axis.line.x = element_line(size = 0.5, 
                                   linetype = "solid", 
                                   colour = "#000000"),
        axis.line.y = element_line(size = 0.5, 
                                   linetype = "solid", 
                                   colour = "#000000"),
        panel.spacing = unit(2, "lines"),
        axis.title = element_text(colour = "#000000"),
        axis.text = element_text(colour = "#000000"), 
        plot.title = element_text(colour = "#000000", face = "bold"))

plot2 <- ggplot(coefficients2, aes(x = reorder(country_name, estimate), y = estimate)) +
  geom_point() +
  geom_hline(yintercept = 0, lty = "dashed") +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), 
                width = 0.2) +
  labs(y = "\nATT") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~ factor(outcome), scales = "free") +
  coord_flip() +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), 
        axis.title.y = element_blank()) +
  theme(axis.line.x = element_line(size = 0.5, 
                                   linetype = "solid", 
                                   colour = "#000000"),
        axis.line.y = element_line(size = 0.5, 
                                   linetype = "solid", 
                                   colour = "#000000"),
        panel.spacing = unit(2, "lines"),
        axis.title = element_text(colour = "#000000"),
        axis.text = element_text(colour = "#000000"), 
        plot.title = element_text(colour = "#000000", face = "bold"))

fig_SM12 <- grid.arrange(plot1, plot2, nrow=1)

fig_SM12 ## Print figure


## Section E - Miscellaneous

## Load data, subset for Ghana and standardise outcomes to ease interpretation

rm(list=ls())

AB_ghana <- readRDS("ABcombined.rds") %>% 
  filter(country_name == "Ghana") %>%
  mutate(new_mean_trust = as.numeric(scale(new_mean_trust)), 
         trust_pres = as.numeric(scale(trust_pres)))

## Run regressions for various outcomes

gha_urbrur <- feols(new_mean_trust ~ urbrur | round, 
                    vcov="iid",
                    data=AB_ghana)

gha_urbrur_r1 <- feols(new_mean_trust ~ urbrur, 
                       vcov="iid",
                       data=subset(AB_ghana, round == "1"))

gha_urbrur_r2 <- feols(new_mean_trust ~ urbrur, 
                       vcov="iid",
                       data=subset(AB_ghana, round == "2"))

gha_urbrur_r3 <- feols(new_mean_trust ~ urbrur, 
                       vcov="iid",
                       data=subset(AB_ghana, round == "3"))

gha_urbrur_r4 <- feols(new_mean_trust ~ urbrur, 
                       vcov="iid",
                       data=subset(AB_ghana, round == "4"))

gha_urbrur_r5 <- feols(new_mean_trust ~ urbrur, 
                       vcov="iid",
                       data=subset(AB_ghana, round == "5"))

gha_urbrur_r6 <- feols(new_mean_trust ~ urbrur, 
                       vcov="iid",
                       data=subset(AB_ghana, round == "6"))

gha_urbrur_covs <- feols(new_mean_trust ~ urbrur + age + gender + education | round, 
                         vcov="iid",
                         data=AB_ghana)

gha_urbrur_r1_covs <- feols(new_mean_trust ~ urbrur + age + gender + education, 
                            vcov="iid",
                            data=subset(AB_ghana, round == "1"))

gha_urbrur_r2_covs <- feols(new_mean_trust ~ urbrur + age + gender + education, 
                            vcov="iid",
                            data=subset(AB_ghana, round == "2"))

gha_urbrur_r3_covs <- feols(new_mean_trust ~ urbrur + age + gender + education, 
                            vcov="iid",
                            data=subset(AB_ghana, round == "3"))

gha_urbrur_r4_covs <- feols(new_mean_trust ~ urbrur + age + gender + education, 
                            vcov="iid",
                            data=subset(AB_ghana, round == "4"))

gha_urbrur_r5_covs <- feols(new_mean_trust ~ urbrur + age + gender + education, 
                            vcov="iid",
                            data=subset(AB_ghana, round == "5"))

gha_urbrur_r6_covs <- feols(new_mean_trust ~ urbrur + age + gender + education, 
                            vcov="iid",
                            data=subset(AB_ghana, round == "6"))

## Make plot

gha_urbrur_list <- list(gha_urbrur, gha_urbrur_covs,
                        gha_urbrur_r1, gha_urbrur_r1_covs,
                        gha_urbrur_r2, gha_urbrur_r2_covs,
                        gha_urbrur_r3, gha_urbrur_r3_covs,
                        gha_urbrur_r4, gha_urbrur_r4_covs,
                        gha_urbrur_r5, gha_urbrur_r5_covs,
                        gha_urbrur_r6, gha_urbrur_r6_covs)

gha_urbrur_df <- map_df(gha_urbrur_list, broom::tidy, .id="model") %>%
  filter(term == "urbrurUrban") %>% 
  mutate(conf.low = estimate - 1.96*std.error, conf.high = estimate + 1.96*std.error,
         model = c("a) Pooled\n", "a) Pooled\n", rep("b) Round-specific\n", 12)),
         round = c("Pooled", "Pooled", 
                   "Round 1\n(1999)", "Round 1\n(1999)",
                   "Round 2\n(2002)", "Round 2\n(2002)",
                   "Round 3\n(2005)", "Round 3\n(2005)",
                   "Round 4\n(2008)", "Round 4\n(2008)",
                   "Round 5\n(2012)", "Round 5\n(2012)",
                   "Round 6\n(2014)", "Round 6\n(2014)"), 
         spec = rep(c("Baseline", "Covariates"), 7))

fig_SM13 <- ggplot(gha_urbrur_df, aes(y=round, x=estimate, 
                          col=spec)) +
  facet_grid(~model, scales="free", space="free") +
  geom_point(position = position_dodge(width=0.5)) +
  geom_vline(xintercept=0, col="#000000") +
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.high, height=0),
                 position = position_dodge(width=0.5)) +
  theme_minimal() +
  scale_colour_manual(values = c("darkgrey", "darkblue")) +
  theme(axis.title.x = element_blank(), 
        legend.title = element_blank(), 
        legend.position = "bottom",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  coord_flip() +
  xlab("Urban-rural gap\n(% standard deviation)\n") +
  geom_hline(yintercept=0, col="#000000") +
  theme(axis.line.x = element_line(size = 0.5, 
                                   linetype = "solid", 
                                   colour = "#000000"),
        axis.line.y = element_line(size = 0.5, 
                                   linetype = "solid", 
                                   colour = "#000000"),
        panel.spacing = unit(2, "lines"),
        axis.title = element_text(colour = "#000000"),
        axis.text = element_text(colour = "#000000"), 
        plot.title = element_text(colour = "#000000", face = "bold"))

fig_SM13 ## Print figure



