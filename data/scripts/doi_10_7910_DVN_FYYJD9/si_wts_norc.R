### ---- Fig. S7: marginal means for agreements conjoint-------
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
      conjoint_chosen ~ 1, data = ., weights = survey_weight,
      clusters = CaseId
    ))) %>%
    mutate(Z = paste0(var_list[i], !!col),
           estimator = "Weighted")
  
  est_mm_out_u[[i]] <-
    norc_conjoint_long %>%
    group_by(!!col) %>%
    do(tidy(lm_robust(
      conjoint_chosen ~ 1, data = ., 
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
        "Purchase vaccines made outside the U.S.",
        "Provide economic aid and debt forgiveness",
        "Finance public health infrastructure",
        "Purchase vaccines made in the U.S.",
        "Purchase patents for vaccine production",
        "U.S. allies and aligned countries",
        "Poor and low-income countries",
        "Countries most at risk for outbreaks",
        "9 years",
        "7 years",
        "5 years",
        "3 years",
        "1 year"
      ))
    )
  )

g <-
  gg_norc_mm %>% 
  ggplot(.,
         aes(
           x = estimate,
           y = level,
           fill = estimator,
           color = estimator,
           group = estimator
         )) +
  geom_vline(
    xintercept = 0.5,
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
  facet_col(attribute ~ ., scales = "free_y", space = "free") +
  scale_fill_manual("", values = c("black", "darkgrey")) +
  scale_color_manual("", values = c("black", "darkgrey")) +
  scale_x_continuous(
    limits = c(0.38, 0.65)) +
  labs(
    x = "Probability of supporting agreement",
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

ggsave(filename = "norc_conjoint_means_wts.pdf",
       g,
       width = 7.6,
       height = 2 + fixed + spacing * (nrow(g$data)))

## Export underlying estimates to CSV
g$data %>%
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
  write_csv("norc_conjoint_means_wts.csv")

### ---- Fig. S8: AMCEs for agreements conjoint-------
norc_conjoint_amce_u <- 
  lm_robust(
    conjoint_chosen ~ cost_ + prop_ + purpose_ + benefit_ + duration_,
    data = norc_conjoint_long,
    clusters = CaseId
  ) %>%
  tidy() %>% 
  mutate(estimator = "Unweighted")

norc_conjoint_amce_w <- 
  lm_robust(
    conjoint_chosen ~ cost_ + prop_ + purpose_ + benefit_ + duration_,
    data = norc_conjoint_long,
    clusters = CaseId, 
    weights = survey_weight
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
        "100%",
        "75%",
        "50%",
        "25%",
        "Provide economic aid and debt forgiveness",
        "Finance public health infrastructure",
        "Purchase vaccines made in the U.S.",
        "Purchase patents for vaccine production",
        "Poor and low-income countries",
        "Countries most at risk for outbreaks",
        "9 years",
        "7 years",
        "5 years",
        "3 years"
      ))
    )
  )

g <-
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
    position = position_dodgev(height = .9)
  ) +
  scale_fill_manual("", values = c("black", "darkgrey")) +
  scale_color_manual("", values = c("black", "darkgrey")) +
  facet_col(attribute_ref ~ ., scales = "free_y", space = "free") +
  scale_x_continuous(
    limits = c(-0.21, 0.21)) +
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

g %>%
  ggsave(
    filename = "norc_conjoint_amces_wts.pdf",
    .,
    width = 7.6,
    height = fixed + 1.5*spacing * nrow(g$data)
  )

## Export underlying estimates to CSV
g$data %>%
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
  write_csv("norc_conjoint_amces_wts.csv")

### ---- Fig. S9: Estimated ATEs with and without weights-------

covariates <- 
  c("x_ideo_7n", "x_pid_7n", "x_edu", "x_hhi_short", "x_female", "x_age_cat", 
    "x_region", "x_race", "x_employ_short", "x_altruism_n", "x_vaxnat_n", 
    "x_patnat1_n", "x_patnat2_n")


est_donate_u <-
  norc_survey %>%
  mutate(y_donate = y_donate / 10) %>%
  lm_lin(y_donate ~ Z_frame,
         covariates = as.formula(paste("~", paste(
           covariates, collapse = "+"
         ))),
         data = .) %>%
  tidy() %>%
  filter(str_detect(term, "Z_"),!str_detect(term, ":")) %>%
  mutate(estimator = "Unweighted")

est_donate_w <-
  norc_survey %>%
  mutate(y_donate = y_donate / 10) %>%
  lm_lin(
    y_donate ~ Z_frame,
    covariates = as.formula(paste("~", paste(
      covariates, collapse = "+"
    ))),
    weights = survey_weight,
    data = .
  ) %>%
  tidy() %>%
  filter(str_detect(term, "Z_"),!str_detect(term, ":")) %>%
  mutate(estimator = "Weighted")

est_petition_u <-
  norc_survey %>%
  lm_lin(y_petition ~ Z_frame,
         covariates = as.formula(paste("~", paste(
           covariates, collapse = "+"
         ))),
         data = .) %>%
  tidy() %>%
  filter(str_detect(term, "Z_"),!str_detect(term, ":")) %>%
  mutate(estimator = "Unweighted")

est_petition_w <-
  norc_survey %>%
  lm_lin(
    y_petition ~ Z_frame,
    covariates = as.formula(paste("~", paste(
      covariates, collapse = "+"
    ))),
    weights = survey_weight,
    data = .
  ) %>%
  tidy() %>%
  filter(str_detect(term, "Z_"),!str_detect(term, ":")) %>%
  mutate(estimator = "Weighted")

est_prop_cost_u <-
  norc_survey %>%
  mutate(y_prop_cost = y_prop_cost / 100) %>%
  lm_lin(y_prop_cost ~ Z_frame,
         covariates = as.formula(paste("~", paste(
           covariates, collapse = "+"
         ))),
         data = .) %>%
  tidy() %>%
  filter(str_detect(term, "Z_"),!str_detect(term, ":")) %>%
  mutate(estimator = "Unweighted")

est_prop_cost_w <-
  norc_survey %>%
  mutate(y_prop_cost = y_prop_cost / 100) %>%
  lm_lin(
    y_prop_cost ~ Z_frame,
    covariates = as.formula(paste("~", paste(
      covariates, collapse = "+"
    ))),
    weights = survey_weight,
    data = .
  ) %>%
  tidy() %>%
  filter(str_detect(term, "Z_"),!str_detect(term, ":")) %>%
  mutate(estimator = "Weighted")

gg_est <-
  bind_rows(
    est_donate_u,
    est_petition_u,
    est_prop_cost_u,
    est_donate_w,
    est_petition_w,
    est_prop_cost_w
  ) %>%
  mutate(
    Z = case_when(
      term == "Z_frameInequality" ~ "Global inequality",
      term == "Z_framePast Success" ~ "Past success",
      term == "Z_frameMutation Risk" ~ "Mutation risk",
      term == "Z_frameEconomic Benefits" ~ "Economic benefits",
      term == "Z_frameVaccine Diplomacy" ~ "Vaccine diplomacy"
    ),
    outcome = factor(outcome,
                     levels = rev(
                       c("y_donate", "y_petition", "y_prop_cost")
                     ),
                     labels = rev(
                       c(
                         "Proportion of $10 bonus\ndonated to COVAX",
                         "Proportion supporting petition\nfor increased spending",
                         "Proportion of global costs\nthe U.S. should fund"
                       )
                     )),
    conf.low90 = estimate - std.error * 1.65,
    conf.high90 = estimate + std.error * 1.65
  )


g <-
  gg_est %>% 
  ggplot(., aes(x = estimate,
                y = Z,
                color = estimator,
                fill = estimator,
                group = estimator)) +
  geom_vline(xintercept = 0,
             color = "black",
             linetype = 2,
             size = 0.2) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0,
    position = position_dodgev(height = 0.7),
    size = 1
  ) +
  geom_point(
    position = position_dodgev(height = 0.7),
    size = 2.5,
    pch = 22,
    color = "white"
  ) +
  facet_row( ~ outcome) +
  scale_fill_manual("", values = c("black", "darkgrey")) +
  scale_color_manual("", values = c("black", "darkgrey")) +
  theme_tufte() +
  theme(axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(size = 0.25),
        strip.text = element_text(size = 11),
        axis.line.x = element_line(size = 0.25),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 11),
        plot.margin = unit(c(t = 0.1, r = 0.1, b = 0.5, l = 0.1), "lines"),
        legend.position = "bottom",
        legend.justification = "left",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10)) +
  labs(
    subtitle = "",
    x = "Average treatment effect estimate",
    y = ""
  ) 


ggsave(
  filename = "norc_persuasion_ATEs_wts.pdf",
  g,
  width = 7.5,
  height = fixed + spacing * (nrow(g$data))
)


### ---- Table S3: Estimated ATEs with and without weights-------

## DiM estimators (no covariate-adjustment)
dim_donate_u <-
  norc_survey %>%
  mutate(y_donate = y_donate / 10) %>%
  lm_robust(y_donate ~ Z_frame,
            data = .) %>%
  tidy() %>%
  filter(str_detect(term, "Z_"),!str_detect(term, ":")) %>%
  mutate(estimator = "Unweighted")

dim_donate_w <-
  norc_survey %>%
  mutate(y_donate = y_donate / 10) %>%
  lm_robust(y_donate ~ Z_frame,
            weights = survey_weight,
            data = .) %>%
  tidy() %>%
  filter(str_detect(term, "Z_"),!str_detect(term, ":")) %>%
  mutate(estimator = "Weighted")

dim_petition_u <-
  norc_survey %>%
  lm_robust(y_petition ~ Z_frame,
            data = .) %>%
  tidy() %>%
  filter(str_detect(term, "Z_"),!str_detect(term, ":")) %>%
  mutate(estimator = "Unweighted")

dim_petition_w <-
  norc_survey %>%
  lm_robust(y_petition ~ Z_frame,
            weights = survey_weight,
            data = .) %>%
  tidy() %>%
  filter(str_detect(term, "Z_"),!str_detect(term, ":")) %>%
  mutate(estimator = "Weighted")

dim_prop_cost_u <-
  norc_survey %>%
  mutate(y_prop_cost = y_prop_cost / 100) %>%
  lm_robust(y_prop_cost ~ Z_frame,
            data = .) %>%
  tidy() %>%
  filter(str_detect(term, "Z_"),!str_detect(term, ":")) %>%
  mutate(estimator = "Unweighted")

dim_prop_cost_w <-
  norc_survey %>%
  mutate(y_prop_cost = y_prop_cost / 100) %>%
  lm_robust(y_prop_cost ~ Z_frame,
            weights = survey_weight,
            data = .) %>%
  tidy() %>%
  filter(str_detect(term, "Z_"),!str_detect(term, ":")) %>%
  mutate(estimator = "Weighted")

gg_dim <-
  bind_rows(
    dim_donate_u,
    dim_petition_u,
    dim_prop_cost_u,
    dim_donate_w,
    dim_petition_w,
    dim_prop_cost_w
  ) %>%
  mutate(
    Z = case_when(
      term == "Z_frameInequality" ~ "Global inequality",
      term == "Z_framePast Success" ~ "Past success",
      term == "Z_frameMutation Risk" ~ "Mutation risk",
      term == "Z_frameEconomic Benefits" ~ "Economic benefits",
      term == "Z_frameVaccine Diplomacy" ~ "Vaccine diplomacy"
    ),
    outcome = factor(outcome,
                     levels = rev(
                       c("y_donate", "y_petition", "y_prop_cost")
                     ),
                     labels = rev(
                       c(
                         "Proportion of $10 bonus\ndonated to COVAX",
                         "Proportion supporting petition\nfor increased spending",
                         "Proportion of global costs\nthe U.S. should fund"
                       )
                     )),
    conf.low90 = estimate - std.error * 1.65,
    conf.high90 = estimate + std.error * 1.65
  )

gg_ate <-
  bind_rows(
    gg_est %>%
      mutate(
        weighted = as.logical(estimator == "Weighted"),
        estimator =  "Covariate-Adjusted"
      ),
    
    gg_dim %>%
      mutate(
        weighted = as.logical(estimator == "Weighted"),
        estimator = "Difference-in-Means"
      )
  )

## Generate table for SI
gg_ate %>%
  arrange(desc(estimator), outcome, Z, weighted) %>%
  group_by(estimator, weighted) %>%
  mutate(entry = make_entry(
    est = estimate,
    se = std.error,
    p = p.adjust(p.value, "fdr"),
    digits = 2
  )) %>%
  dplyr::select(Z, estimator, outcome, weighted, entry) %>%
  pivot_wider(names_from = outcome:weighted,
              values_from = c(entry)) %>%
  xtable() %>%
  print(
    include.rownames = FALSE,
    include.colnames = FALSE,
    hline.after = c(),
    only.contents = TRUE,
    type = "latex",
    file = "compare_ates_tab.tex"
  )
