# *****************************************************************
# OVERVIEW ####
# *****************************************************************
# CES20_REGRESSIONS.R
# Regression Analyses from the CES 2020 Data
# Jennifer Lin and Kristin Lunz Trujillo
# Created On: 2021 08 11

# *** The code in this file replicates the regression analyses in
#   the paper and includes code on components that are in the main text
#   and Appendix. ***

# ** Before running this file, ensure that you have ran
#   - Functions.R
#   - Load_RUCA.R
#   - CES2020_CLEAN.R
#  or components of this code will result in an error. See 
#  00_GENERAL.R for more details **

# *****************************************************************
# POISSON REGRESSIONS ####
# *****************************************************************

# To start, the first section addresses the poisson regressions that
#   are in Supplemental Appendix F. The first section provides the 
#   general political behavior series and the second provides the 
#   social media behavior series. Since the process is similar, only
#   the first section will be discussed in detail

## General Political Behaviors ####

# Caclulate the poisson regression using svyglm()
pois_behaviors_2020 <- svyglm(
  behavior_series ~ factor(residence) + pid7 + ideo5 + 
    rr_work + rr_slavery + RUCAx +
    educ + income + FEMALE + AGE + factor(race) + factor(CHURCH),
  design = CCES_20_survey,
  family = quasipoisson()
) 

# Calculate N
n_behseries <- length(pois_behaviors_2020$residuals)

# Format for export, including getting significance stars
pois_behaviors_2020 <- pois_behaviors_2020 %>% 
  tidy() %>% CES_Predictors() %>% 
  mutate(
    signif = case_when(
      p.value <= 0.001 ~ "***",
      p.value > 0.001 & p.value <= 0.05 ~ "**",
      p.value > 0.05 & p.value <= 0.1 ~ "*",
      TRUE ~ ""
    ),
    outcome = paste0(
      round(`estimate`, 2), signif
    ),
    SE = paste0("(", round(std.error, 3), ")")
  ) %>% 
  select(term, outcome, SE) %>% 
  reshape2::melt(id = "term") %>% 
  arrange(term) %>% 
  mutate(
    variable = as.character(variable),
    term = as.character(term),
    term = if_else(variable == "outcome", 
                   term, ""),
    variable = case_when(
      variable == "SE" ~ "",
      TRUE ~ variable
    )
  ) %>% 
  select(term, `2020 Political Behaviors` = value)

## Social Media Behaviors ####

pois_social_2020 <- svyglm(
  social_series ~ factor(residence) + pid7 + ideo5 + 
    rr_work + rr_slavery + RUCAx +
    educ + income + FEMALE + AGE + factor(race) + factor(CHURCH),
  design = CCES_20_survey,
  family = quasipoisson()
) 

n_socialseries <- length(pois_social_2020$residuals)

pois_social_2020 <- pois_social_2020 %>% 
  tidy() %>% CES_Predictors() %>% 
  mutate(
    signif = case_when(
      p.value <= 0.001 ~ "***",
      p.value > 0.001 & p.value <= 0.05 ~ "**",
      p.value > 0.05 & p.value <= 0.1 ~ "*",
      TRUE ~ ""
    ),
    outcome = paste0(
      round(`estimate`, 2), signif
    ),
    SE = paste0("(", round(std.error, 3), ")")
  ) %>% 
  select(term, outcome, SE) %>% 
  reshape2::melt(id = "term") %>% 
  arrange(term) %>% 
  mutate(
    variable = as.character(variable),
    term = as.character(term),
    term = if_else(variable == "outcome", 
                   term, ""),
    variable = case_when(
      variable == "SE" ~ "",
      TRUE ~ variable
    )
  ) %>% 
  select(term, `2020 Social Media` = value)


## Generate Export table ####

# Combine the n calculations from both sections above
n_poission_20 <- c("N", n_behseries, n_socialseries)

# Bind the columns and select the columns of interest
poisson_exp_20 <- bind_cols(pois_behaviors_2020, pois_social_2020) %>% 
  select(Term = 1, 2, 4)

# Bind extimataes with n -- export this table to Latex or word
#   using xtable or other export package
poisson_exp_20 <- rbind(poisson_exp_20, n_poission_20)

# *****************************************************************
# LOGIT REGRESSIONS ####
# *****************************************************************

# The following section includes logit models to represent each of
#   the political behaviors of interest. As with the previous section,
#   the rocess is quite repetitive so the first 2 will be commented
#   in greater detail and the rest follows the same pattern.

## General Political Behaviors ####

### Voted in the Election ####

# Run the regression model
reg_Voted <- svyglm(
  p_VOTED ~ factor(residence) + pid7 + ideo5 + 
    rr_work + rr_slavery + RUCAx +
    educ + income + FEMALE + AGE + factor(race) + factor(CHURCH),
  design = CCES_20_survey,
  family = quasibinomial(link = "logit")
)
summary(reg_Voted)

# For stargazer export use -- get the terms labeled from the model
#   and generate a vector for automatic label of covariates 
Terms <- get_probability(reg_Voted) %>% 
  CES_Predictors() %>% 
  mutate(term = as.character(term)) %>% 
  filter(term != "(Intercept)")
covariates <- Terms$term
covariates <- c(covariates, "Constant")

# Get a table of estimates in odds ratio form and save for
#   later use (export)
tab_voted <- prob_tab(reg_Voted) %>% 
  rename(`Voted in 2020` = value)

### Attend a meeting ####

# Run the regression model
reg_meeting <- svyglm(
  p_meeting ~ factor(residence) + pid7 + ideo5 + 
    rr_work + rr_slavery + RUCAx +
    educ + income + FEMALE + AGE + factor(race) + factor(CHURCH),
  design = CCES_20_survey,
  family = quasibinomial(link = "logit")
)
summary(reg_meeting)

# Get a table of estimates in odds ratio form and save for
#   later use (export)
tab_meeting <- prob_tab(reg_meeting) %>% 
  rename(`Attended a Meeting` = value)

### Put up a political yard sign ####

reg_sign <- svyglm(
  p_sign ~ factor(residence) + pid7 + ideo5 + 
    rr_work + rr_slavery + RUCAx +
    educ + income + FEMALE + AGE + factor(race) + factor(CHURCH),
  design = CCES_20_survey,
  family = quasibinomial(link = "logit")
)
summary(reg_sign)

tab_sign <- prob_tab(reg_sign) %>% 
  rename(`Put up a political sign` = value)

### Work on a campaign ####

reg_work_campaign <- svyglm(
  p_work_campaign ~ factor(residence) + pid7 + ideo5 + 
    rr_work + rr_slavery + RUCAx +
    educ + income + FEMALE + AGE + factor(race) + factor(CHURCH),
  design = CCES_20_survey,
  family = quasibinomial(link = "logit")
)
summary(reg_work_campaign)

tab_work_campaign <- prob_tab(reg_work_campaign) %>% 
  rename(`Work in Campaign` = value)

### Attend a protest ####

reg_protest <- svyglm(
  p_protest ~ factor(residence) + pid7 + ideo5 + 
    rr_work + rr_slavery + RUCAx +
    educ + income + FEMALE + AGE + factor(race) + factor(CHURCH),
  design = CCES_20_survey,
  family = quasibinomial(link = "logit")
)
summary(reg_protest)

tab_protest <- prob_tab(reg_protest) %>% 
  rename(`Attended a Protest` = value)

### Contact Elected Officials ####

reg_contact <- svyglm(
  p_contact ~ factor(residence) + pid7 + ideo5 + 
    rr_work + rr_slavery + RUCAx +
    educ + income + FEMALE + AGE + factor(race) + factor(CHURCH),
  design = CCES_20_survey,
  family = quasibinomial(link = "logit")
)
summary(reg_contact)

tab_contact <- prob_tab(reg_contact) %>% 
  rename(`Contact Elected Officials` = value)

### Donate Money ####

reg_donate_money <- svyglm(
  p_donate_money ~ factor(residence) + pid7 + ideo5 +
    rr_work + rr_slavery + RUCAx +
    educ + income + FEMALE + AGE + factor(race) + factor(CHURCH),
  design = CCES_20_survey,
  family = quasibinomial(link = "logit")
)
summary(reg_donate_money)

tab_donate_money <- prob_tab(reg_donate_money) %>% 
  rename(`Donate Money` = value)

### Donate Blood ####

reg_donate_blood <- svyglm(
  p_donate_blood ~ factor(residence) + pid7 + ideo5 + 
    rr_work + rr_slavery + RUCAx +
    educ + income + FEMALE + AGE + factor(race) + factor(CHURCH),
  design = CCES_20_survey,
  family = quasibinomial(link = "logit")
)
summary(reg_donate_blood)

tab_donate_blood <- prob_tab(reg_donate_blood) %>% 
  rename(`Donate Blood` = value)

### Participate in None of the Above (NOTA) ####

reg_NOTA <- svyglm(
  p_NOTA ~ factor(residence) + pid7 + ideo5 + 
    rr_work + rr_slavery + RUCAx +
    educ + income + FEMALE + AGE + factor(race) + factor(CHURCH),
  design = CCES_20_survey,
  family = quasibinomial(link = "logit")
)
summary(reg_NOTA)

tab_NOTA <- prob_tab(reg_NOTA) %>% 
  rename(`None of the Above` = value)

### Export General Political Behaviors ####

# The below code generates Table 2 in the Supplemental Appendix and
#   produces a table with all the odds ratios for all the predictors
#   in each of the models above.

# Get all of the tables that were saved for export from above. This
#   is indicated by objects that start with "tab_"
CES20 <- mget(ls(pattern="tab_")) %>%
  reduce(bind_cols) %>% 
  select(-c(starts_with("term"), starts_with("variable")))

# Get the names of the terms
names <- tab_voted %>% 
  select(term, variable)

# Prepare for export -- resulting object can be exported using
#   xtable for Latex or word
CES20_Results <- bind_cols(names, CES20) %>% 
  mutate(
    term = if_else(variable %in% c("outcome", ""), 
                   term, ""),
    variable = case_when(
      variable == "out_SE" ~ "",
      TRUE ~ variable
    )
  ) %>% 
  select(-variable) %>% 
  select(
    term, `Voted in 2020`, `Attended a Meeting`,
    `Put up a political sign`,
    `Work in Campaign`,
    `Attended a Protest`, 
    `Contact Elected Officials`,
    `Donate Money`,
    `Donate Blood`,
    `None of the Above`
  )

### Generate Dot-whisker plot ####

# The following code generates the dot whisker plot shown in
#   Figure 5 in the main paper

# Get all the regression objects from above.
CC20_plot_tab <- mget(ls(pattern="reg_")) 

# Generate empty list to store the estimates that are needed
#   for the plot
plot_df_20 <- list()

# The for loop below parses each of the regression objects and 
#   generates odds ratios. It also appends the name of the regression
#   as an identifier. Will recode for plotting later.
for (i in 1:length(CC20_plot_tab)) {
  plot_df_20[[i]] <- get_probability(CC20_plot_tab[[i]])
  plot_df_20[[i]]$reg_name <- names(CC20_plot_tab)[i]
}

# Recode regression names to political behaviors and 
#   filter table to just include RUCA and rural self-
#   identity to plot. Call both factors so we control the
#   resulting order.
plot_df_20_clean <- bind_rows(plot_df_20) %>% 
  mutate(
    Behavior = case_when(
      reg_name == "reg_contact" ~ "Contact Elected\nOfficials",
      reg_name == "reg_donate_blood" ~ "Donate Blood",
      reg_name == "reg_donate_money" ~ "Donate Money",
      reg_name == "reg_meeting" ~ "Attended a\nMeeting",
      reg_name == "reg_NOTA" ~ "None of\nthe Above",
      reg_name == "reg_protest" ~ "Attended a\nProtest",
      reg_name == "reg_sign" ~ "Put up a\npolitical\nsign",
      reg_name == "reg_Voted" ~ "Voted in\n2020",
      reg_name == "reg_work_campaign" ~ "Work in\nCampaign"
    ),
    Behavior = factor(
      Behavior,
      levels = c(
        "Voted in\n2020", "Attended a\nMeeting", "Put up a\npolitical\nsign",
        "Work in\nCampaign", "Attended a\nProtest", "Contact Elected\nOfficials",
        "Donate Money", "Donate Blood", "None of\nthe Above"
      )
    )
  ) %>% 
  filter(grepl("residence", term) |
           grepl("RUCA", term)) %>% 
  mutate(
    rural = case_when(
      grepl("residence", term) ~ "Self-Identified",
      grepl("RUCA", term) ~ "RUCA Category"
    ),
    term = case_when(
      term == "factor(residence)Rural" ~ "Rural",
      term == "factor(residence)Suburb" ~ "Suburb",
      term == "factor(residence)Town" ~ "Small Town",
      term == "RUCAx2" ~ "RUCA -- City",
      term == "RUCAx3" ~ "RUCA -- Town",
      term == "RUCAx4" ~ "RUCA -- Rural"
    ),
    term = factor(
      term, 
      levels = c(
        "Suburb", "Small Town", "Rural",
        "RUCA -- City", "RUCA -- Town", "RUCA -- Rural"
      ))
  )

# Generate plot
ggplot(plot_df_20_clean, aes(x = term, y = odds))+
  geom_pointrange(
    aes(ymin=odds-odds_sd, ymax=odds+odds_sd), 
    position = position_dodge(1))+
  coord_flip()+
  facet_grid(vars(rural), vars(Behavior), scales = "free_y")+
  xlab("Rural Measure Variable Levels")+
  ylab("Odds Ratios")+
  ylim(0.4, 1.8)+
  labs(
    title = "Odds Ratios for Logistic Regressions",
    subtitle = "Political Behaviors in 2020",
    caption = "Data: Cooperative Election Studies 2020"
  )+
  theme_bw()+
  theme(
    plot.title         = element_text(hjust = 0.5, size = 20, colour="black", face = "bold"),
    plot.subtitle      = element_text(hjust = 0.5, size = 16, colour="black"),
    legend.title       = element_text(hjust = 0.5, size = 14, colour="black", face = "bold"),
    plot.caption       = element_text(size = 10, colour="black"),
    axis.title         = element_text(size = 14, colour="black"),
    axis.text.x        = element_text(size = 12, colour="black", angle = 45, hjust = 0.5),
    axis.text.y        = element_text(size = 12, colour="black"),
    legend.position    = 'bottom',
    legend.direction   = "horizontal",
    legend.text        = element_text(size = 12, colour="black")
  )

## Social Media Political Behaviors ####

### Post content about politics ####

# Calculate Regression
smreg_post <- svyglm(
  sm_post ~ factor(residence) + pid7 + ideo5 + 
    rr_work + rr_slavery + RUCAx +
    educ + income + FEMALE + AGE + factor(race) + factor(CHURCH),
  design = CCES_20_survey,
  family = quasibinomial(link = "logit")
)
summary(smreg_post)

# Generate table for use later in the export process
smtab_post <- prob_tab(smreg_post) %>% 
  rename(`Posted Media about Politics` = value)

### Post comment about politics ####

smreg_comment <- svyglm(
  sm_comment ~ factor(residence) + pid7 + ideo5 + 
    rr_work + rr_slavery + RUCAx +
    educ + income + FEMALE + AGE + factor(race) + factor(CHURCH),
  design = CCES_20_survey,
  family = quasibinomial(link = "logit")
)
summary(smreg_comment)

smtab_comment <- prob_tab(smreg_comment) %>% 
  rename(`Posted Comment about Politics` = value)

### Read content about politics ####

smreg_read <- svyglm(
  sm_read ~ factor(residence) + pid7 + ideo5 + 
    rr_work + rr_slavery + RUCAx +
    educ + income + FEMALE + AGE + factor(race) + factor(CHURCH),
  design = CCES_20_survey,
  family = quasibinomial(link = "logit")
)
summary(smreg_read)

smtab_read <- prob_tab(smreg_read) %>% 
  rename(`Read Story/Watched Video about Politics` = value)

### Followed a political event ####

smreg_event <- svyglm(
  sm_event ~ factor(residence) + pid7 + ideo5 + 
    rr_work + rr_slavery + RUCAx +
    educ + income + FEMALE + AGE + factor(race) + factor(CHURCH),
  design = CCES_20_survey,
  family = quasibinomial(link = "logit")
)
summary(smreg_event)

smtab_event <- prob_tab(smreg_event) %>% 
  rename(`Followed a Political Event` = value)

### Forward content about politics ####

smreg_forward <- svyglm(
  sm_forward ~ factor(residence) + pid7 + ideo5 + 
    rr_work + rr_slavery + RUCAx +
    educ + income + FEMALE + AGE + factor(race) + factor(CHURCH),
  design = CCES_20_survey,
  family = quasibinomial(link = "logit")
)
summary(smreg_forward)

smtab_forward <- prob_tab(smreg_forward) %>% 
  rename(`Forwarded Media about Politics` = value)

### Export Social Media Behaviors ####

# The below code generates Table 3 in the Supplemental Appendix and
#   produces a table with all the odds ratios for all the predictors
#   in each of the models above.

# Get all of the tables that were saved for export from above. This
#   is indicated by objects that start with "smtab_"
CES20sm <- mget(ls(pattern="smtab_")) %>%
  reduce(bind_cols) %>% 
  select(-c(starts_with("term"), starts_with("variable")))

# Get the names of the terms
namessm <- smtab_post %>% 
  dplyr::select(term, variable)

# Prepare for export -- resulting object can be exported using
#   xtable for Latex or word
CES20_smResults <- bind_cols(namessm, CES20sm)

CES20_Csm <- CES20_smResults %>% 
  dplyr::select(
    term,
    `Posted Media about Politics`,
    `Posted Comment about Politics`,
    `Read Story/Watched Video about Politics`,
    `Followed a Political Event`,
    `Forwarded Media about Politics`
  )

### Generate Dot-Whisker Plot ####

# The following code generates Figure 6 in the main text

# Get all the regression objects from above.
CC20_smplot_tab <- mget(ls(pattern="smreg_")) 

# Generate empty list to store the estimates that are needed
#   for the plot
smplot_df_20 <- list()

# The for loop below parses each of the regression objects and 
#   generates odds ratios. It also appends the name of the regression
#   as an identifier. Will recode for plotting later.
for (i in 1:length(CC20_smplot_tab)) {
  smplot_df_20[[i]] <- get_probability(CC20_smplot_tab[[i]])
  smplot_df_20[[i]]$reg_name <- names(CC20_smplot_tab)[i]
}

# Recode regression names to political behaviors and 
#   filter table to just include RUCA and rural self-
#   identity to plot. Call both factors so we control the
#   resulting order.
smplot_df_20_clean <- bind_rows(smplot_df_20) %>% 
  mutate(
    Behavior = case_when(
      reg_name == "smreg_comment" ~ "Posted Comment\nabout Politics",
      reg_name == "smreg_event" ~ "Followed a\nPolitical Event",
      reg_name == "smreg_forward" ~ "Forwarded Media\nabout Politics",
      reg_name == "smreg_post" ~ "Posted Media\nabout Politics",
      reg_name == "smreg_read" ~ "Read Story/Watched\nVideo about Politics"
    ),
    Behavior = factor(
      Behavior,
      levels = c(
        "Posted Media\nabout Politics", "Posted Comment\nabout Politics",
        "Read Story/Watched\nVideo about Politics", 
        "Followed a\nPolitical Event", "Forwarded Media\nabout Politics"
      )
    )
  ) %>% 
  filter(grepl("residence", term) |
           grepl("RUCA", term)) %>% 
  mutate(
    rural = case_when(
      grepl("residence", term) ~ "Self-Identified",
      grepl("RUCA", term) ~ "RUCA Category"
    ),
    term = case_when(
      term == "factor(residence)Rural" ~ "Rural",
      term == "factor(residence)Suburb" ~ "Suburb",
      term == "factor(residence)Town" ~ "Small Town",
      term == "RUCAx2" ~ "RUCA -- City",
      term == "RUCAx3" ~ "RUCA -- Town",
      term == "RUCAx4" ~ "RUCA -- Rural"
    ),
    term = factor(
      term, 
      levels = c(
        "Suburb", "Small Town", "Rural",
        "RUCA -- City", "RUCA -- Town", "RUCA -- Rural"
      ))
  )

# Generate Plot
ggplot(smplot_df_20_clean, aes(x = term, y = odds))+
  geom_pointrange(
    aes(ymin=odds-odds_sd, ymax=odds+odds_sd), 
    position = position_dodge(1))+
  coord_flip()+
  facet_grid(vars(rural), vars(Behavior), scales = "free_y")+
  xlab("Rural Measure Variable Levels")+
  ylab("Odds Ratios")+
  ylim(0.4, 1.8)+
  labs(
    title = "Odds Ratios for Logistic Regressions",
    subtitle = "Social Media Political Behaviors in 2020",
    caption = "Data: Cooperative Election Studies 2020"
  )+
  theme_bw()+
  theme(
    plot.title         = element_text(hjust = 0.5, size = 20, colour="black", face = "bold"),
    plot.subtitle      = element_text(hjust = 0.5, size = 16, colour="black"),
    legend.title       = element_text(hjust = 0.5, size = 14, colour="black", face = "bold"),
    plot.caption       = element_text(size = 10, colour="black"),
    axis.title         = element_text(size = 14, colour="black"),
    axis.text.x        = element_text(size = 12, colour="black", angle = 45, hjust = 0.5),
    axis.text.y        = element_text(size = 12, colour="black"),
    legend.position    = 'bottom',
    legend.direction   = "horizontal",
    legend.text        = element_text(size = 12, colour="black")
  )

