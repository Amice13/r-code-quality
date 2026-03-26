### ---- Fig. S15: Marginal means on ordinal outcome, person conjoint (Lucid) -------- 
est_mm_out_u <- list()
est_mm_out_w <- list()
var_list <- c("sex_",
              "age_",
              "harm_",
              "exposure_",
              "country_",
              "occupation_",
              "work_")

for (i in 1:length(var_list)) {
  col <- sym(var_list[i])
  est_mm_out_w[[i]] <-
    lucid_conjoint_p_long %>%
    group_by(!!col) %>%
    do(tidy(
      lm_robust(conjoint_rating ~ 1, data = ., weights = survey_weight,
                clusters = admin_ResponseId)
    )) %>%
    mutate(Z = paste0(var_list[i],!!col),
           estimator = "Weighted")
  
  est_mm_out_u[[i]] <-
    lucid_conjoint_p_long %>%
    group_by(!!col) %>%
    do(tidy(
      lm_robust(conjoint_rating ~ 1, data = .,
                clusters = admin_ResponseId)
    )) %>%
    mutate(Z = paste0(var_list[i],!!col),
           estimator = "Unweighted")
}

gg_pconjoint_mm <- 
  bind_rows(est_mm_out_u, est_mm_out_w) %>% 
  ungroup() %>%
  select(Z, estimate, std.error, statistic, p.value, conf.low, conf.high, 
         estimator) %>%
  mutate(
    attribute = str_to_title(word(Z, 1, sep = "_")),
    level = word(Z, 2, sep = "_"),
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
        "25-34",
        "35-44",
        "45-54",
        "55-64",
        "65-74",
        "75+",
        "Low",
        "Moderate",
        "High",
        "United States",
        "Canada",
        "Brazil",
        "South Africa",
        "Nigeria",
        "India",
        "Australia",
        "Pakistan",
        "China",
        "Non-essential workers",
        "Public transit",
        "Education and childcare",
        "First responders",
        "Healthcare workers",
        "Yes",
        "No"
      )
    ),
    attribute = factor(
      attribute, 
      levels = c(
        "Country",
        "Exposure",
        "Harm",
        "Occupation",
        "Work",
        "Age",
        "Sex"
      ),
      labels = c(
        "Country of Origin",
        "Risk of exposure to COVID-19",
        "Risk of serious illness from COVID-19",
        "Occupation group",
        "Can work from home",
        "Age group",
        "Sex"
      )
    )
  )

g1 <- 
  gg_pconjoint_mm %>% 
  ggplot(.,
         aes(
           x = estimate,
           y = level,
           color = estimator,
           fill = estimator, 
           group = estimator
         )) +
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
    position = position_dodgev(height = .9)
  ) +
  facet_col(attribute ~ ., scales = "free_y", space = "free") +
  scale_fill_manual("", values = c("black", "darkgrey")) +
  scale_color_manual("", values = c("black", "darkgrey")) +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.01)) +
  labs(
    x = "Support for vaccinating individual",
    y = "",
    caption = ""
  ) +
  theme_tufte() +
  theme(axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(size = 0.25),
        strip.text = element_text(size = 11),
        axis.line.x = element_line(size = 0.25),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 11),
        plot.margin = unit(c(t = 0.1, r = 0.1, b = -0.5, l = 0.1), "lines"),
        legend.position = "bottom",
        legend.justification = "left",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10)
  ) 

ggsave(filename = "lucid_pconjoint_means_cont_wts.pdf",
       g1,
       width = 7.6,
       height = fixed + spacing * (nrow(g1$data)))

## Export underlying estimates to CSV
g1$data %>%
  select(estimator, 
         attribute,
         level,
         estimate,
         std.error,
         statistic,
         p.value,
         conf.low,
         conf.high) %>%
  mutate_if(is.numeric, round, 2) %>%
  write_csv("lucid_pconjoint_means_cont_wts.csv")

### ---- Fig. S16: AMCEs on ordinal outcome, person conjoint (Lucid) -------- 
lucid_pconjoint_amce_u <-
  lm_robust(
    conjoint_rating ~ sex_ + age_ + harm_ + exposure_ + country_ +
      occupation_ + work_,
    clusters = admin_ResponseId,
    data = lucid_conjoint_p_long
  ) %>%
  tidy() %>% 
  mutate(estimator = "Unweighted")

lucid_pconjoint_amce_w <- 
  lm_robust(
    conjoint_rating ~ sex_ + age_ + harm_ + exposure_ + country_ +
      occupation_ + work_,
    clusters = admin_ResponseId,
    weights = survey_weight,
    data = lucid_conjoint_p_long
  ) %>%
  tidy() %>% 
  mutate(estimator = "Weighted")

gg_pconjoint_amce <-
  bind_rows(lucid_pconjoint_amce_u, lucid_pconjoint_amce_w) %>%
  filter(term != "(Intercept)") %>% 
  mutate(
    attribute_ref =
      case_when(
        str_detect(term, "sex_") ~ "Sex (reference: Female)",
        str_detect(term, "age_") ~ "Age group (reference: 18-24)",
        str_detect(term, "harm_") ~ "Risk of serious illness from COVID-19 (reference: Low)",
        str_detect(term, "exposure_") ~ "Risk of exposure to COVID-19 (reference: Low)",
        str_detect(term, "country_") ~  "Country of origin (reference: United States)",
        str_detect(term, "occupation_") ~ "Occupation group (reference: Non-essential workers)",
        str_detect(term, "work_") ~ "Can work from home (reference: Yes)"
      ),
    attribute_ref = factor(
      attribute_ref,
      levels = c(
        "Country of origin (reference: United States)",
        "Risk of exposure to COVID-19 (reference: Low)",
        "Risk of serious illness from COVID-19 (reference: Low)",
        "Occupation group (reference: Non-essential workers)",
        "Can work from home (reference: Yes)",
        "Age group (reference: 18-24)",
        "Sex (reference: Female)"
      )
    ),
    level = str_replace(
      term,
      "sex_|age_|harm_|exposure_|country_|occupation_|work_",
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
        "Male",
        "25-34",
        "35-44",
        "45-54",
        "55-64",
        "65-74",
        "75+",
        "Moderate",
        "High",
        "China",
        "Pakistan",
        "Australia",
        "India",
        "Nigeria",
        "South Africa",
        "Brazil",
        "Canada",
        "Public transit",
        "Education and childcare",
        "First responders",
        "Healthcare workers",
        "No"
      )
    )
  )

## AMCEs w/ and w/o weights
g2 <- 
  gg_pconjoint_amce %>% 
  ggplot(.,
         aes(x = estimate,
             y = level,
             color = estimator, 
             fill = estimator, 
             group = estimator
         )) +
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
    #fill = "black",
    position = position_dodgev(height = .9)
  ) +
  scale_fill_manual("", values = c("black", "darkgrey")) +
  scale_color_manual("", values = c("black", "darkgrey")) +
  facet_col(attribute_ref ~ ., scales = "free_y", space = "free") +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.02)) +
  labs(
    x = "Average marginal component effect",
    y = "",
    caption = ""
  ) +
  theme_tufte(base_size = 14) +
  theme(
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(size = 0.25),
    strip.text = element_text(size = 11, hjust = 0),
    axis.line.x = element_line(size = 0.25),
    axis.title.x = element_text(size = 10),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 10),
    strip.placement = "inside",
    plot.margin = unit(c(t = 0.1, r = 0.5, b = -0.5, l = 0.1), "lines"),
    legend.position = "bottom",
    legend.justification = "left",
    legend.margin=margin(0,0,0,0),
    legend.box.margin=margin(-10,-10,-10,-10)
  ) 

g2 %>%
  ggsave(
    filename = "lucid_pconjoint_amces_cont_wts.pdf",
    .,
    width = 7.6,
    height = 2.5 + fixed + spacing * nrow(g2$data)
  )

## Export underlying estimates to CSV
g2$data %>%
  select(estimator, 
         attribute_ref,
         level,
         estimate,
         std.error,
         statistic,
         p.value,
         conf.low,
         conf.high) %>%
  mutate_if(is.numeric, round, 2) %>%
  write_csv("lucid_pconjoint_amces_cont_wts.csv")


### ---- Fig. S17: Marginal means on ordinal outcome, agreement conjoint (Lucid) -------- 

## Rescale subjective vote likelihood to 0, 1
lucid_conjoint_a_long$conjoint_likelihood <- 
  scales::rescale(lucid_conjoint_a_long$conjoint_rating, to = c(0, 1))

## Lucid
est_mm_out_u <- list()
est_mm_out_w <- list()
var_list <-
  c(
    "price_",
    "participants_",
    "costs_",
    "benefits_",
    "supply_",
    "sharing_",
    "monitoring_"
  )

for (i in 1:length(var_list)) {
  col <- sym(var_list[i])
  est_mm_out_w[[i]] <-
    lucid_conjoint_a_long %>%
    group_by(!!col) %>%
    do(tidy(
      lm_robust(conjoint_likelihood ~ 1, data = ., weights = survey_weight,
                clusters = admin_ResponseId)
    )) %>%
    mutate(Z = paste0(var_list[i],!!col),
           estimator = "Weighted")
  
  est_mm_out_u[[i]] <-
    lucid_conjoint_a_long %>%
    group_by(!!col) %>%
    do(tidy(
      lm_robust(conjoint_likelihood ~ 1, data = .,
                clusters = admin_ResponseId)
    )) %>%
    mutate(Z = paste0(var_list[i],!!col),
           estimator = "Unweighted")
}


gg_lucid_mm <-
  bind_rows(est_mm_out_u, est_mm_out_w) %>%
  ungroup() %>%
  select(Z, estimate, std.error, statistic, p.value, conf.low, conf.high, 
         estimator) %>%
  mutate(
    attribute = str_to_title(word(Z, 1, sep = "_")),
    level = word(Z, 2, sep = "_"),
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
        "Countries that need more vaccines pay more",
        "Rich countries contribute more than poor",
        "Proportional to a country's contribution",
        "Only poor countries benefit",
        "Poor countries benefit more than rich",
        "Proportional to population size",
        "Proportional to size of at-risk population",
        "No",
        "Yes",
        "Compulsory",
        "Voluntary",
        "U.S. government",
        "World Health Organization",
        "Independent commission",
        "United Nations"
      )
    ),
    attribute = factor(
      attribute,
      levels = c(
        "Price",
        "Participants",
        "Benefits",
        "Costs",
        "Monitoring",
        "Sharing",
        "Supply"
      ),
      labels = c(
        "Costs to average household",
        "Number of participating countries",
        "Distribution of benefits",
        "Distribution of costs",
        "Monitoring for non-compliance",
        "Sharing of vaccine technology",
        "External supply agreements allowed"
      )
    )
  )

## Weighted v. unweighted estimates
g1 <- 
  gg_lucid_mm %>% 
  ggplot(.,
         aes(
           x = estimate,
           y = level,
           color = estimator,
           fill = estimator, 
           group = estimator
         )) +
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
    position = position_dodgev(height = .9)
  ) +
  facet_col(attribute ~ ., scales = "free_y", space = "free") +
  scale_fill_manual("", values = c("black", "darkgrey")) +
  scale_color_manual("", values = c("black", "darkgrey")) +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.01)) +
  labs(
    x = "Likelihood of voting for agreement",
    y = "",
    caption = ""
  ) +
  theme_tufte() +
  theme(axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(size = 0.25),
        strip.text = element_text(size = 11),
        axis.line.x = element_line(size = 0.25),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 11),
        plot.margin = unit(c(t = 0.1, r = 0.1, b = -0.5, l = 0.1), "lines"),
        legend.position = "bottom",
        legend.justification = "left",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10)
  ) 

g1

ggsave(filename = "lucid_aconjoint_means_cont_wts.pdf",
       g1,
       width = 7.6,
       height = fixed + spacing * (nrow(g1$data)))


## Export underlying estimates to CSV
g1$data %>%
  select(estimator, 
         attribute,
         level,
         estimate,
         std.error,
         statistic,
         p.value,
         conf.low,
         conf.high) %>%
  mutate_if(is.numeric, round, 2) %>%
  write_csv("lucid_aconjoint_means_cont_wts.csv")

### ---- Fig. S18: AMCEs on ordinal outcome, agreement conjoint (Lucid) -------- 

lucid_conjoint_amce_u <-
  lm_robust(
    conjoint_likelihood ~ price_ + participants_ + costs_ + benefits_ +
      supply_ + sharing_ + monitoring_,
    clusters = admin_ResponseId,
    data = lucid_conjoint_a_long
  ) %>%
  tidy() %>% 
  mutate(estimator = "Unweighted")

lucid_conjoint_amce_w <- 
  lm_robust(
    conjoint_likelihood ~ price_ + participants_ + costs_ + benefits_ +
      supply_ + sharing_ + monitoring_,
    clusters = admin_ResponseId,
    data = lucid_conjoint_a_long,
    weights = survey_weight
  ) %>%
  tidy() %>% 
  mutate(estimator = "Weighted")

gg_lucid_amce <-
  bind_rows(lucid_conjoint_amce_u, lucid_conjoint_amce_w) %>%
  filter(term != "(Intercept)") %>%
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
        "Costs to average household (reference: $1)",
        "Number of participating countries (reference: 20 of 192)",
        "Distribution of benefits (reference: Only poor countries benefit)",
        "Distribution of costs (reference: Only rich countries contribute)",
        "Monitoring for non-compliance (reference: U.S. government)",
        "Sharing of vaccine technology (reference: Compulsory)",
        "External supply agreements allowed (reference: No)"
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
        "$5",
        "$10",
        "$15",
        "$20",
        "80 of 192",
        "100 of 192",
        "170 of 192",
        "Countries that pay more get more in return",
        "Poor countries get more than rich countries",
        "Proportional to a country's population size",
        "Countries with more people at risk of severe illness get more",
        "Countries that need more vaccines pay more",
        "Rich countries contribute more than poor countries",
        "Yes",
        "Voluntary",
        "World Health Organization",
        "United Nations",
        "Independent commission"
      ),
      labels = c(
        "$5",
        "$10",
        "$15",
        "$20",
        "80 of 192",
        "100 of 192",
        "170 of 192",
        "Proportional to a country's contribution",
        "Poor countries benefit more than rich",
        "Proportional to population size",
        "Proportional to size of at-risk population",
        "Countries that need more contribute more",
        "Rich countries contribute more than poor",
        "Yes",
        "Voluntary",
        "World Health Organization",
        "United Nations",
        "Independent commission"
      )
    )
  )

## AMCEs w/ and w/o weights
g2 <- 
  gg_lucid_amce %>% 
  ggplot(.,
         aes(x = estimate,
             y = level,
             color = estimator, 
             fill = estimator, 
             group = estimator
         )) +
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
    position = position_dodgev(height = .9)
  ) +
  scale_fill_manual("", values = c("black", "darkgrey")) +
  scale_color_manual("", values = c("black", "darkgrey")) +
  facet_col(attribute_ref ~ ., scales = "free_y", space = "free") +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.02)) +
  labs(
    x = "Average marginal component effect",
    y = "",
    caption = ""
  ) +
  theme_tufte(base_size = 14) +
  theme(
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(size = 0.25),
    strip.text = element_text(size = 11, hjust = 0),
    axis.line.x = element_line(size = 0.25),
    axis.title.x = element_text(size = 10),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 10),
    strip.placement = "inside",
    plot.margin = unit(c(t = 0.1, r = 0.5, b = -0.5, l = 0.1), "lines"),
    legend.position = "bottom",
    legend.justification = "left",
    legend.margin=margin(0,0,0,0),
    legend.box.margin=margin(-10,-10,-10,-10)
  ) 

g2

g2 %>%
  ggsave(
    filename = "lucid_aconjoint_amces_cont_wts.pdf",
    .,
    width = 7.6,
    height = 2.5 + fixed + spacing * nrow(g2$data)
  )

## Export underlying estimates to CSV
g2$data %>%
  select(estimator, 
         attribute_ref,
         level,
         estimate,
         std.error,
         statistic,
         p.value,
         conf.low,
         conf.high) %>%
  mutate_if(is.numeric, round, 2) %>%
  write_csv("lucid_aconjoint_amces_cont_wts.csv")

### ---- Fig. S19: Marginal means on ordinal outcome, agreement conjoint (NORC) -------- 
est_mm_out_w <- list()
est_mm_out_u <- list()
var_list <- c(
  "cost_",
  "prop_",
  "purpose_",
  "benefit_",
  "duration_"
)


for (i in 1:length(var_list)) {
  col <- sym(var_list[i])
  est_mm_out_w[[i]] <-
    norc_conjoint_long %>%
    group_by(!!col) %>%
    do(tidy(lm_robust(
      conjoint_vote ~ 1, data = ., weights = survey_weight,
      clusters = CaseId
    ))) %>%
    mutate(Z = paste0(var_list[i], !!col),
           estimator = "Weighted")
  
  est_mm_out_u[[i]] <-
    norc_conjoint_long %>%
    group_by(!!col) %>%
    do(tidy(lm_robust(
      conjoint_vote ~ 1, data = ., 
      clusters = CaseId
    ))) %>%
    mutate(Z = paste0(var_list[i], !!col),
           estimator = "Unweighted")
}

gg_norc_mm <-
  bind_rows(est_mm_out_w, est_mm_out_u) %>%
  ungroup() %>%
  select(Z, estimate, std.error, statistic, p.value, conf.low, conf.high,
         estimator) %>%
  mutate(
    attribute = str_to_title(word(Z, 1, sep = "_")),
    attribute = factor(
      attribute,
      levels = c("Cost", "Prop", "Purpose", "Benefit", "Duration"),
      labels = c(
        "Total cost of the agreement",
        "Proportion of total cost paid by the U.S.",
        "Funding used to",
        "Benefits directed toward",
        "Duration of agreement"
      )
    ),
    level = word(Z, 2, sep = "_"),
    level = factor(
      level,
      levels = rev(c(
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
      ))
    )
  )

## Weighted v. unweighted estimates
g1 <- 
  gg_norc_mm %>% 
  ggplot(.,
         aes(
           x = estimate,
           y = level,
           color = estimator,
           fill = estimator, 
           group = estimator
         )) +
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
    position = position_dodgev(height = .9)
  ) +
  facet_col(attribute ~ ., scales = "free_y", space = "free") +
  scale_fill_manual("", values = c("black", "darkgrey")) +
  scale_color_manual("", values = c("black", "darkgrey")) +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.01)) +
  labs(
    x = "Likelihood of voting for agreement",
    y = "",
    caption = ""
  ) +
  theme_tufte() +
  theme(axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(size = 0.25),
        strip.text = element_text(size = 11),
        axis.line.x = element_line(size = 0.25),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 11),
        plot.margin = unit(c(t = 0.1, r = 0.1, b = -0.5, l = 0.1), "lines"),
        legend.position = "bottom",
        legend.justification = "left",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10)
  ) 

ggsave(filename = "norc_conjoint_means_cont_wts.pdf",
       g1,
       width = 7.6,
       height = fixed + spacing * (nrow(g1$data)))


## Export underlying estimates to CSV
g1$data %>%
  select(estimator, 
         attribute,
         level,
         estimate,
         std.error,
         statistic,
         p.value,
         conf.low,
         conf.high) %>%
  mutate_if(is.numeric, round, 2) %>%
  write_csv("norc_conjoint_means_cont_wts.csv")



### ---- Fig. S20: AMCEs on ordinal outcome, agreement conjoint (NORC) -------- 

norc_conjoint_amce_u <- 
  lm_robust(
    conjoint_vote ~ cost_ + prop_ + purpose_ + benefit_ + duration_,
    data = norc_conjoint_long,
    clusters = CaseId
  ) %>%
  tidy() %>% 
  mutate(estimator = "Unweighted")

norc_conjoint_amce_w <- 
  lm_robust(
    conjoint_vote ~ cost_ + prop_ + purpose_ + benefit_ + duration_,
    data = norc_conjoint_long,
    clusters = CaseId, weights = survey_weight
  ) %>%
  tidy() %>% 
  mutate(estimator = "Weighted")


gg_norc_amce <- 
  bind_rows(norc_conjoint_amce_u, norc_conjoint_amce_w) %>% 
  filter(term != "(Intercept)") %>%
  mutate(
    attribute_ref = str_to_title(word(term, 1, sep = "_")),
    attribute_ref = factor(
      attribute_ref,
      levels = c("Cost", "Prop", "Purpose", "Benefit", "Duration"),
      labels = c(
        "Total cost of the agreement (reference: $25 billion)",
        "Proportion of total cost paid by the U.S. (reference: 0%)",
        "Funding used to (reference: Purchase vaccines made outside the U.S.)",
        "Benefits directed toward (reference: U.S. allies and aligned countries)",
        "Duration of agreement (reference: 1 year)"
      )
    ),
    level = word(term, 2, sep = "_"),
    level = factor(
      level,
      levels = rev(c(
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
      ))
    )
  )

## AMCEs w/ and w/o weights
g2 <- 
  gg_norc_amce %>% 
  ggplot(.,
         aes(x = estimate,
             y = level,
             color = estimator, 
             fill = estimator, 
             group = estimator
         )) +
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
    #fill = "black",
    position = position_dodgev(height = .9)
  ) +
  scale_fill_manual("", values = c("black", "darkgrey")) +
  scale_color_manual("", values = c("black", "darkgrey")) +
  facet_col(attribute_ref ~ ., scales = "free_y", space = "free") +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.02)) +
  labs(
    x = "Average marginal component effect",
    y = "",
    caption = ""
  ) +
  theme_tufte(base_size = 14) +
  theme(
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(size = 0.25),
    strip.text = element_text(size = 11, hjust = 0),
    axis.line.x = element_line(size = 0.25),
    axis.title.x = element_text(size = 10),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 10),
    strip.placement = "inside",
    plot.margin = unit(c(t = 0.1, r = 0.5, b = -0.5, l = 0.1), "lines"),
    legend.position = "bottom",
    legend.justification = "left",
    legend.margin=margin(0,0,0,0),
    legend.box.margin=margin(-10,-10,-10,-10)
  ) 

g2 %>%
  ggsave(
    filename = "norc_conjoint_amces_cont_wts.pdf",
    .,
    width = 7.6,
    height = 2.5 + fixed + spacing * nrow(g2$data)
  )

g2$data %>%
  select(estimator, 
         attribute_ref,
         level,
         estimate,
         std.error,
         statistic,
         p.value,
         conf.low,
         conf.high) %>%
  mutate_if(is.numeric, round, 2) %>%
  write_csv("norc_conjoint_amces_cont_wts.csv")
