#### ---------- Setup --------------------------------------------------- ####

## Read in survey data
norc_survey <- read_rds("norc_survey.rds") 

lucid_survey <- read_rds("lucid_survey.rds")

## Read in conjoint data
lucid_conjoint_a <- read_rds("lucid_conjoint_agreement.rds")
lucid_conjoint_p <- read_rds("lucid_conjoint_person.rds")

norc_conjoint <- read_rds("norc_conjoint.rds")

## Join conjoint data w/ survey data
norc_conjoint_long <-
  right_join(norc_conjoint, norc_survey, by = "CaseId")

lucid_conjoint_a_long <- 
  left_join(lucid_conjoint_a, lucid_survey, by = "admin_ResponseId")

lucid_conjoint_p_long <- 
  left_join(lucid_conjoint_p, lucid_survey, by = "admin_ResponseId")

## Plotting params
fixed <- 1
spacing <- .1285


#### ---------- Figure 1: estimate AMCEs --------------------------------- ####
est_dat <-
  lucid_conjoint_p_long %>%
  mutate(
    ## Create indicator for USA v. others
    i_foreign = case_when(
      country_ == "United States" ~ "United States",
      country_ != "United States" ~ "Other"
    ),
    i_foreign = factor(i_foreign, levels = rev(c("United States", "Other"))),
    ## Collapse age category
    age_ = case_when(
      age_ == "18-24" ~ "18-24",
      age_ %in% c("25-34", "35-44") ~ "25-44",
      age_ %in% c("45-54", "55-64") ~ "45-64",
      TRUE ~ "65+"
    ),
    age_ = factor(age_, levels = c("18-24", "25-44", "45-64", "65+"))
  )

## Print marginals for paired subsets. NB: p. 3 of manuscript uses these 
## averages: Americans with low risk of serious illness were selected w/ 
## probability 0.52 and non-Americans were selected with probability 0.49. 
est_dat %>% 
  filter(usa_pair == 1) %>% 
  group_by(i_foreign, harm_) %>% 
  summarise(y_bar = mean(conjoint_chosen))


## Estimate AMCEs for paired comparisons between USA and another country of 
## origin. 
est_amce_pairs <-
  lm_robust(
    conjoint_chosen ~ sex_ + age_ + harm_ + exposure_ +  i_foreign  +
      occupation_ + work_,
    clusters = admin_ResponseId,
    data = est_dat %>% 
      filter(usa_pair == 1)
  ) %>%
  tidy() %>% 
  mutate(group = "US pairs")

#### ---------- Figure 1: plot estimated AMCEs and export ---------------- ####
## Create canvass for plotting 
gg_fig1 <-
  est_amce_pairs %>%
  filter(term != "(Intercept)") %>%
  add_row(
    estimate = rep(0, 7),
    conf.low = rep(0, 7),
    conf.high = rep(0, 7),
    term = c(
      "sex_Female",
      "age_18-24",
      "harm_Low",
      "exposure_Low",
      "i_foreignAnother country",
      "occupation_Non-essential workers",
      "work_Yes"
    ),
    group = "Filler"
  ) %>%
  mutate(
    attribute_ref =
      case_when(
        str_detect(term, "sex_") ~ "Sex:",
        str_detect(term, "age_") ~ "Age group:",
        str_detect(term, "harm_") ~ "Risk of serious illness:",
        str_detect(term, "exposure_") ~ "Risk of exposure:",
        str_detect(term, "i_foreign") ~  "Country of origin:",
        str_detect(term, "occupation_") ~ "Occupation group:",
        str_detect(term, "work_") ~ "Can work from home:"
      ),
    attribute_ref = factor(
      attribute_ref,
      levels = c(
        "Country of origin:",
        "Risk of exposure:",
        "Risk of serious illness:",
        "Age group:",
        "Occupation group:",
        "Can work from home:",
        "Sex:"
      )
    ),
    level = str_replace(
      term,
      "sex_|age_|harm_|exposure_|country_|occupation_|work_|i_foreign",
      ""
    ),
    level = str_replace(level, " \\(e.g., police/fire\\)", ""),
    level = case_when(
      level == "Non-essential worker" ~ "Non-essential workers",
      TRUE ~ paste(level)
    ),
    level = factor(
      level,
      levels = c(
        "Female",
        "Male",
        "18-24",
        "25-44",
        "45-64",
        "65+",
        "Low",
        "Moderate",
        "High",
        "Another country",
        "United States",
        "Non-essential workers",
        "Public transit",
        "Education and childcare",
        "First responders",
        "Healthcare workers",
        "Yes",
        "No"
      )
    )
  )


## Create Fig. 1 and export as pdf 
p_fig1 <-
  ggplot(gg_fig1,
         aes(estimate,
             level)) +
  geom_vline(
    xintercept = 0,
    color = "black",
    lty = 2,
    size = 0.2
  ) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0,
    position = position_dodgev(height = .9),
    size = 1
  ) +
  geom_point(
    size = 2.5,
    pch = 22,
    color = "white",
    fill = "black"
  ) +
  facet_col(attribute_ref ~ ., scales = "free_y", space = "free") +
  labs(x = "Average marginal component effect",
       y = "",
       caption = "") +
  theme_tufte() +
  theme(
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(size = 0.25),
    strip.text = element_text(size = 11, hjust = 0, face = "bold"),
    axis.line.x = element_line(size = 0.25),
    axis.title.x = element_text(size = 10),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 10),
    strip.placement = "inside",
    plot.margin = unit(c(
      t = 0.1,
      r = 0.5,
      b = -0.5,
      l = 0.1
    ), "lines")
  ) 

p_fig1

## Export plot as PDF
ggsave(
  plot = p_fig1,
  filename = "manuscript_fig1.pdf",
  width = 5,
  height = 2 + fixed + spacing * (nrow(p_fig1$data))
)

## Export underlying estimates to CSV
p_fig1$data %>%
  filter(group != "Filler") %>%
  select(attribute_ref,
         level,
         estimate,
         std.error,
         statistic,
         p.value,
         conf.low,
         conf.high) %>%
  mutate_if(is.numeric, round, 2) %>%
  write_csv("manuscript_fig1.csv")

#### ---------- Figure 2: estimate AMCEs --------------------------------- ####

## Relevel benefits attribute
lucid_conjoint_a_long$benefits_ <- 
  relevel(lucid_conjoint_a_long$benefits_, 
          ref = "Countries that pay more get more in return")

est_amce_lucid_a <-
  lm_robust(
    conjoint_chosen ~ price_ + participants_ + costs_ + benefits_ +
      supply_ + sharing_ + monitoring_,
    clusters = admin_ResponseId,
    data = lucid_conjoint_a_long
  ) %>%
  tidy() %>% 
  mutate(group = "AMCE")


#### ---------- Figure 2: plot estimated AMCEs and export ---------------- ####

## Create canvass for plotting
gg_fig2 <-
  est_amce_lucid_a %>%
  filter(term != "(Intercept)") %>%
  add_row(
    estimate = rep(0, 7),
    conf.low = rep(0, 7),
    conf.high = rep(0, 7),
    term = c(
      "price_$1",
      "participants_20 of 192",
      "costs_Only rich countries contribute",
      "benefits_Countries that pay more get more in return",
      "sharing_Compulsory",
      "monitoring_U.S. government",
      "supply_No"
    ),
    group = "Filler"
  ) %>%
  mutate(
    attribute_ref =
      case_when(
        str_detect(term, "price_") ~ "Cost (reference: $1 per household)",
        str_detect(term, "participants_") ~ "Participating countries (reference: 20 of 192)",
        str_detect(term, "costs_") ~ "Distribution of Costs (reference: Only rich countries)",
        str_detect(term, "benefits_") ~ "Distribution of Benefits (reference: Only poor countries)",
        str_detect(term, "supply_") ~ "External supply (reference: Countries are barred from separate agreements)",
        str_detect(term, "sharing_") ~ "Vaccine technology (reference: Compulsory sharing)",
        str_detect(term, "monitoring_") ~ "Monitoring (reference: U.S. government)"
      ),
    attribute_ref = factor(
      attribute_ref,
      levels = c(
        "Cost (reference: $1 per household)",
        "Participating countries (reference: 20 of 192)",
        "Distribution of Benefits (reference: Only poor countries)",
        "Distribution of Costs (reference: Only rich countries)",
        "Monitoring (reference: U.S. government)",
        "Vaccine technology (reference: Compulsory sharing)",
        "External supply (reference: Countries are barred from separate agreements)"
      ),
      labels = c(
        "Costs to average household:",
        "Number of participating countries:",
        "Distribution of benefits:",
        "Distribution of costs:",
        "Monitoring for non-compliance:",
        "Sharing of vaccine technology:",
        "External supply agreements:"
      )
    ),
    level = str_replace(
      term,
      "price_|participants_|costs_|benefits_|supply_|sharing_|monitoring_",
      ""
    ),
    level = factor(
      level,
      levels = c(
        "$1",
        "$5",
        "$10",
        "$15",
        "$20",
        "20 of 192",
        "80 of 192",
        "100 of 192",
        "170 of 192",
        "Only rich countries contribute",
        "Countries that need more vaccines pay more",
        "Rich countries contribute more than poor countries",
        "Countries that pay more get more in return",
        "Only poor countries",
        "Poor countries get more than rich countries",
        "Proportional to a country's population size",
        "Countries with more people at risk of severe illness get more",
        "No",
        "Yes",
        "Compulsory",
        "Voluntary",
        "U.S. government",
        "World Health Organization",
        "Independent commission",
        "United Nations"
      ),
      labels = c(
        "$1",
        "$5",
        "$10",
        "$15",
        "$20",
        "20 of 192",
        "80 of 192",
        "100 of 192",
        "170 of 192",
        "Only rich countries contribute",
        "Prop. to vaccine demand",
        "Rich countries more than poor",
        "Prop. to contribution",
        "Only poor countries benefit",
        "Poor countries more than rich",
        "Prop. to total population",
        "Prop. to at-risk population",
        "Not permitted",
        "Permitted",
        "Compulsory",
        "Voluntary",
        "U.S. government",
        "World Health Organization",
        "Independent commission",
        "United Nations"
      )
    )
  )
  
p_fig2 <-
  ggplot(gg_fig2, aes(estimate, level)) +
  geom_vline(
    xintercept = 0,
    color = "black",
    lty = 2,
    size = 0.2
  ) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0,
    position = position_dodgev(height = .9),
    size = 1
  ) +
  geom_point(
    size = 2.5,
    pch = 22,
    color = "white",
    fill = "black"
  ) +
  facet_col(attribute_ref ~ ., scales = "free_y", space = "free") +
  labs(x = "Average marginal component effect",
       y = "",
       caption = "") +
  theme_tufte() +
  theme(
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(size = 0.25),
    strip.text = element_text(size = 11, hjust = 0, face = "bold"),
    axis.line.x = element_line(size = 0.25),
    axis.title.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    strip.placement = "inside",
    plot.margin = unit(c(
      t = 0.1,
      r = -0.5,
      b = -0.5,
      l = -0.75
    ), "lines")
  )

p_fig2

## Export plot as PDF
ggsave(
  plot = p_fig2,
  filename = "manuscript_fig2.pdf",
  width = 5,
  height = 2 + fixed + spacing * (nrow(p_fig2$data))
)

## Export underlying estimates to CSV
p_fig2$data %>%
  filter(group != "Filler") %>%
  select(attribute_ref,
         level,
         estimate,
         std.error,
         statistic,
         p.value,
         conf.low,
         conf.high) %>%
  mutate_if(is.numeric, round, 2) %>%
  write_csv("manuscript_fig2.csv")


#### ---------- Figure 3: estimate AMCEs --------------------------------- ####
est_amce_norc <- 
  lm_robust(
    conjoint_chosen ~ cost_ + prop_ + purpose_ + benefit_ + duration_,
    data = norc_conjoint_long,
    clusters = CaseId
  ) %>%
  tidy() %>% 
  mutate(group = "AMCE")

#### ---------- Figure 3: plot estimated AMCEs and export ---------------- ####

## Create canvass for plotting
gg_fig3 <-
  est_amce_norc %>%
  filter(term != "(Intercept)") %>%
  add_row(
    estimate = rep(0, 5),
    conf.low = rep(0, 5),
    conf.high = rep(0, 5),
    term = c(
      "cost_$25 billion",
      "prop_0%",
      "purpose_Purchase vaccines made outside the U.S.",
      "benefit_U.S. allies and aligned countries",
      "duration_1 year"
    ),
    group = "Filler"
  ) %>%
  mutate(
    attribute_ref = str_to_title(word(term, 1, sep = "_")),
    attribute_ref = factor(
      attribute_ref,
      levels = c("Cost", "Prop", "Purpose", "Benefit", "Duration"),
      labels = c(
        "Total cost of agreement:",
        "Proportion paid by US:",
        "Funding directed toward:",
        "Benefits directed toward:",
        "Duration of agreement:"
      )
    ),
    level = word(term, 2, sep = "_"),
    level = factor(level,
                   levels = rev(
                     c(
                       "$100 billion",
                       "$75 billion",
                       "$50 billion",
                       "$25 billion",
                       "100%",
                       "75%",
                       "50%",
                       "25%",
                       "0%",
                       "Purchase patents for vaccine production",
                       "Purchase vaccines made in the U.S.",
                       "Finance public health infrastructure",
                       "Provide economic aid and debt forgiveness",
                       "Purchase vaccines made outside the U.S.",
                       "Countries most at risk for outbreaks",
                       "Poor and low-income countries",
                       "U.S. allies and aligned countries",
                       "9 years",
                       "7 years",
                       "5 years",
                       "3 years",
                       "1 year"
                     )
                   ),
                   labels = rev(
                     c(
                       "$100 billion",
                       "$75 billion",
                       "$50 billion",
                       "$25 billion",
                       "100%",
                       "75%",
                       "50%",
                       "25%",
                       "0%",
                       "Patents for vaccine production",
                       "Vaccines made in the U.S.",
                       "Public health infrastructure",
                       "Economic aid/debt forgiveness",
                       "Vaccines made outside U.S.",
                       "Countries most at risk",
                       "Poor/low-income countries",
                       "U.S. allies/aligned countries",
                       "9 years",
                       "7 years",
                       "5 years",
                       "3 years",
                       "1 year"
                     )
                   ))
  )

p_fig3 <-
  ggplot(gg_fig3, aes(x = estimate, y = level)) +
  geom_vline(
    xintercept = 0,
    color = "black",
    lty = 2,
    size = 0.2
  ) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0,
    color = "black",
    position = position_dodgev(height = .9),
    size = 1
  ) +
  geom_point(
    size = 2.5,
    pch = 22,
    color = "white",
    fill = "black",
    position = position_dodgev(height = .9)
  ) +
  facet_col(attribute_ref ~ ., scales = "free_y", space = "free") +
  labs(x = "Average marginal component effect",
       y = "",
       caption = "") +
  theme_tufte() +
  theme(
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(size = 0.25),
    strip.text = element_text(size = 11, hjust = 0, face = "bold"),
    axis.line.x = element_line(size = 0.25),
    axis.title.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    strip.placement = "inside",
    plot.margin = unit(c(
      t = 0.1,
      r = -0.5,
      b = -0.5,
      l = -0.75
    ), "lines")
  )

p_fig3

## Export plot as PDF
ggsave(
  plot = p_fig3,
  filename = "manuscript_fig3.pdf",
  width = 5,
  height = 2 + fixed + spacing * (nrow(p_fig3$data))
)

## Export underlying estimates to CSV
p_fig3$data %>%
  filter(group != "Filler") %>%
  select(attribute_ref,
         level,
         estimate,
         std.error,
         statistic,
         p.value,
         conf.low,
         conf.high) %>%
  mutate_if(is.numeric, round, 2) %>%
  write_csv("manuscript_fig3.csv") 
  
#### ---------- Figure 4: estimate ATEs --------------------------------- ####

## Specify covariates for regression adjustment
covariates <-
  c(
    "x_ideo_7n",
    "x_pid_7n",
    "x_edu",
    "x_hhi_short",
    "x_female",
    "x_age_cat",
    "x_region",
    "x_race",
    "x_employ_short",
    "x_altruism_n",
    "x_vaxnat_n",
    "x_patnat1_n",
    "x_patnat2_n"
  )

## Estimate ATEs on index of all outcomes. Create index using inverse covariance
## weighting, then standardize using glass's delta.
tmp <-
  norc_survey %>%
  select(y_donate,
         y_petition,
         y_prop_cost,
         all_of(covariates),
         Z_frame) %>%
  filter(!is.na(y_donate),!is.na(y_petition),!is.na(y_prop_cost))

## Pairwise application of function to each treatment/control group:
tmp_ineq <-
  tmp %>%
  filter(Z_frame %in% c("Control", "Inequality")) %>%
  mutate(
    Z = as.numeric(Z_frame != "Control"),
    y_idx = invcov(
      Z = Z,
      outcome_mat = cbind(y_donate, y_petition, y_prop_cost),
      reorient = FALSE
    )
  )

tmp_past <-
  tmp %>%
  filter(Z_frame %in% c("Control", "Past Success")) %>%
  mutate(
    Z = as.numeric(Z_frame != "Control"),
    y_idx = invcov(
      Z = Z,
      outcome_mat = cbind(y_donate, y_petition, y_prop_cost),
      reorient = FALSE
    )
  )

tmp_mutate <-
  tmp %>%
  filter(Z_frame %in% c("Control", "Mutation Risk")) %>%
  mutate(
    Z = as.numeric(Z_frame != "Control"),
    y_idx = invcov(
      Z = Z,
      outcome_mat = cbind(y_donate, y_petition, y_prop_cost),
      reorient = FALSE
    )
  )

tmp_econ <-
  tmp %>%
  filter(Z_frame %in% c("Control", "Economic Benefits")) %>%
  mutate(
    Z = as.numeric(Z_frame != "Control"),
    y_idx = invcov(
      Z = Z,
      outcome_mat = cbind(y_donate, y_petition, y_prop_cost),
      reorient = FALSE
    )
  )


tmp_diplo <-
  tmp %>%
  filter(Z_frame %in% c("Control", "Vaccine Diplomacy")) %>%
  mutate(
    Z = as.numeric(Z_frame != "Control"),
    y_idx = invcov(
      Z = Z,
      outcome_mat = cbind(y_donate, y_petition, y_prop_cost),
      reorient = FALSE
    )
  )

## Now stack scaled datasets, standardize using Glass's Delta
est_dat <-
  bind_rows(tmp_diplo, tmp_econ, tmp_ineq, tmp_mutate, tmp_past) %>%
  filter(Z == 1) %>%
  bind_rows(., tmp_diplo %>% filter(Z_frame == "Control")) %>%
  mutate(y_idx_s = glass_delta(Y = y_idx, Z = Z_frame, reference = "Control"))

## Print summary stats by group
est_dat %>%
  group_by(Z_frame) %>%
  summarise(
    y_bar_s = mean(y_idx_s),
    y_sd_s = sd(y_idx_s),
    y_bar = mean(y_idx),
    y_sd = sd(y_idx),
    n = sum(!is.na(y_idx))
  )

## Estimate treatment effects on standardized index of outcomes
est_idx <-
  lm_lin(y_idx_s ~ Z_frame,
         as.formula(paste("~", paste(
           covariates, collapse = "+"
         ))),
         data = est_dat) %>%
  tidy() 

## Print estimated treatment effects
est_idx %>%
  filter(str_detect(term, "Z_"), !str_detect(term, ":")) %>% 
  mutate_if(is.numeric, round, 2)

## Provide some context for effect sizes via partisan gap in outcome index:
party_gap <- 
  est_dat %>% 
  filter(Z_frame == "Control", !is.na(x_pid_7n), x_pid_7n != 4) %>% 
  ungroup() %>% 
  mutate(X = as.numeric(x_pid_7n >= 5),
         y = glass_delta(Y = y_idx, Z = X, reference = 0)) %>% 
  do(tidy(lm_robust(y ~ X, data = .))) %>% 
  filter(term != "(Intercept)")

## Effect size of 0.20 is about 1/5 the size of the partisan gap 
0.20/abs(party_gap$estimate)

#### ---------- Figure 4: plot estimated ATEs and export ---------------- ####

## Create canvass for plotting
gg_fig4 <-
  est_idx %>%
  filter(str_detect(term, "Z_"), !str_detect(term, ":"))  %>%
  mutate(
    Z = case_when(
      term == "Z_frameInequality" ~ "Global inequality",
      term == "Z_framePast Success" ~ "Past success",
      term == "Z_frameMutation Risk" ~ "Mutation risk",
      term == "Z_frameEconomic Benefits" ~ "Economic benefits",
      term == "Z_frameVaccine Diplomacy" ~ "Vaccine diplomacy"
    ),
    outcome = "Index of all outcome measures",
    conf.low90 = estimate - std.error * 1.65,
    conf.high90 = estimate + std.error * 1.65
  )

## Negligible effects: 90% CI is contained within MOE.
gg_fig4 %>% 
  filter(conf.low90 >= -0.20 & conf.high90 <= 0.20)

p_fig4 <-
  ggplot(gg_fig4, aes(x = estimate, y = reorder(Z, estimate))) +
  geom_vline(
    xintercept = 0,
    color = "black",
    linetype = "solid",
    size = 0.2
  ) +
  geom_vline(
    xintercept = -0.2,
    color = "black",
    linetype = "dotted",
    size = 0.2
  ) +
  geom_vline(
    xintercept = 0.2,
    color = "black",
    linetype = "dotted",
    size = 0.2
  ) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0,
    position = position_dodgev(height = 0.7),
    size = .5
  ) +
  geom_errorbarh(
    aes(xmin = conf.low90, xmax = conf.high90),
    height = 0,
    position = position_dodgev(height = 0.7),
    size = 1
  ) +
  geom_point(
    position = position_dodgev(height = 0.7),
    size = 2.5,
    fill = "black",
    pch = 22,
    color = "white"
  ) +
  #facet_col( ~ outcome) +
  scale_x_continuous(limits = c(-0.22, 0.25)) +
  theme_tufte(base_size = 14) +
  theme(
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(size = 0.25),
    strip.text = element_text(size = 11),
    axis.line.x = element_line(size = 0.25),
    axis.title.x = element_text(size = 10),
    axis.text.y = element_text(size = 11),
    plot.margin = unit(c(
      t = -1,
      r = -0.5,
      b = 0.1,
      l = -1
    ), "lines")
  ) +
  labs(subtitle = "",
       x = "Average treatment effect estimate",
       y = "")

p_fig4

ggsave(
  filename = "manuscript_fig4.pdf",
  p_fig4,
  width = 5,
  height = 1 + fixed + spacing * (nrow(p_fig4$data))
)

## Export estimates to CSV
p_fig4$data %>% 
  select(Z, estimate, std.error, statistic, p.value, 
         conf.low, conf.high, conf.low90, conf.high90) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  write_csv("manuscript_fig4.csv")





#### ---------- Misc. analyses reported in text ---------------------------####
#### ---------------- Compare Lucid survey to Pew survey (p. 2) ------------####
## p. 2: 67% of respondents believe the U.S. should ensure there are enough
## vaccines for people in the U.S., even if it means people in developing
## countries need to wait longer to ge vaccines
mean(lucid_survey$vnat_binary_n)

#### ---------------- Average support by proportion U.S. paid (p. 5) -------####
norc_conjoint %>%
  mutate(y_reject = as.numeric(conjoint_chosen == 0)) %>% 
  group_by(prop_) %>%
  summarise(y_bar = mean(y_reject, na.rm = TRUE))

#### ---------------- Implied burden analyses (p. 5) ----------------------####
norc_conjoint %>%
  group_by(burden_) %>%
  do(tidy(lm_robust(
    conjoint_chosen ~ 1, data = .,
    clusters = CaseId
  ))) %>%
  select(-term,-outcome) %>%
  mutate_if(is.numeric, round, 2)

## Standard error for difference
(se_diff <- sqrt(0.02^2 + 0.02^2))

## P-value
(z_diff <- (0.61-0.32)/se_diff)
2*(1-pnorm(abs(z_diff)))

#### ---------------- Predict switches across aid purpose (p. 6) --------- ####

norc_fit <-
  lm_robust(
    conjoint_chosen ~ cost_ + prop_ + purpose_ + benefit_ + duration_,
    clusters = CaseId,
    data = norc_conjoint
  )

## Prediction for patents
newdat <-
  data.frame(cost_ = "$50 billion", prop_ = "50%", 
             purpose_ = "Purchase patents for vaccine production",
             benefit_ = "Countries most at risk for outbreaks",
             duration_ = "5 years")

predict(norc_fit, newdata = newdat, interval = "confidence", se.fit = TRUE)

## Prediction for switch to purchasing U.S. produced vaccines
newdat <-
  data.frame(cost_ = "$50 billion", prop_ = "50%", 
             purpose_ = "Purchase vaccines made in the U.S.",
             benefit_ = "Countries most at risk for outbreaks",
             duration_ = "5 years")

predict(norc_fit, newdata = newdat, interval = "confidence", se.fit = TRUE)

## Prediction for switch to purchasing non-U.S. produced vaccines
newdat <-
  data.frame(cost_ = "$50 billion", prop_ = "50%", 
             purpose_ = "Purchase vaccines made outside the U.S.",
             benefit_ = "Countries most at risk for outbreaks",
             duration_ = "5 years")

predict(norc_fit, newdata = newdat, interval = "confidence", se.fit = TRUE)


#### ---------------- Control means from persuasion experiment (p. 6) ---- ####
ctrl <-
  norc_survey %>%
  filter(Z_frame == "Control") 

## Median proportion U.S. should contribute (as % of total funding to vax world)
median(ctrl$y_prop_cost, na.rm = TRUE)

## Proportion willing to sign petition
table(ctrl$y_petition)/sum(table(ctrl$y_petition))

## Median donation (as proportion of $10 USD)
median(ctrl$y_donate, na.rm = TRUE)/10

#### ---------------- Associations bt petition and clicking (p. 6 fn) ----- ####

## Proportion that clicked through, among those willing to sign
mean(ctrl$y_click[ctrl$y_petition == 1], na.rm = TRUE)

## Proportion that clicked through, among those unwilling to sign
mean(ctrl$y_click[ctrl$y_petition == 0], na.rm = TRUE)
