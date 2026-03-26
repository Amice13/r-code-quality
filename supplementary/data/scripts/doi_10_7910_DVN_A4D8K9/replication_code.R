##---------------------------------------------------------------------
## Replication code for the IJPOR article "Anxious but Hesitant to Act: The impact of concurrent priming of the climate crisis and the war in Ukraine on public support"
##---------------------------------------------------------------------
## CREATED:		  23 Sep 2024
## AUTHOR:		  Søren Damsbo-Svendsen
## AFFILIATION:	University of Copenhagen
## WEBSITE:		  https://soerendamsbo.github.io
##---------------------------------------------------------------------

# Load R packages
pacman::p_load(tidyverse, janitor, scales, lubridate, fixest, mice, miceadds, patchwork, Amelia, purrr, modelsummary)

# Create custom function to pool results from each data permutation
# https://stackoverflow.com/questions/65627475/correcting-for-robust-clustered-standard-errors-within-the-lm-function-or-replac
pool_models <- function(model_list){

  library(purrr)
  quiet_summary <- quietly(summary)
  
  betas <- map(model_list, coef) # pull out coefs
  vcovs <- map(model_list, vcov) # pull out vcovs
  parms <- pool_mi(qhat = betas, u = vcovs) # the magic happens
  
  out <- quiet_summary(parms) |> 
    pluck("result") |> 
    rownames_to_column("term") |> 
    as_tibble() 
  
  broom_col_names <- names(broom::tidy(lm(mpg ~ 1, mtcars), conf.int = TRUE)) # (dummy data)
  names(out)[1:length(broom_col_names)] <- broom_col_names
  
  # add N obs
  outN <- out |> mutate(N = nobs(model_list[[1]]))
  return(outN)
  
}

# Load data
imputed_data <- read_rds("imputed_data.rds")
unimputed_data <- read_rds("unimputed_data.rds")

# set outcomes
outcomes <- c("lottery", "policy", "worry", "challenge", "threat", "wta")

##---------------------------------------------------------------------
##---------------------------------------------------------------------
##                                                                   
##       ~       FIGURE 2: MAIN RESULTS
##                                                                   
##---------------------------------------------------------------------
##---------------------------------------------------------------------

#######################################################################
## Run regression models
#######################################################################

m_post <- list()

for (i in seq_along(outcomes)){
  
  (y <- outcomes[[i]])
  
  # climate
  m_post[[paste0("imp_climate_", y)]] <- imputed_data |>
    map(~ filter(., issue == "climate")) |>
    map(~ lm(as.formula(paste0(y, " ~ post + mip_climate + mip_ukraine + polaware + misinfo_personal + misinfo_general + age + gender + region + education + personal_income + household_income + leftright + party_last + party_next + device_category")), data = .)) |> 
  mice::pool()
  
  # ukraine
  m_post[[paste0("imp_ukraine_", y)]] <- imputed_data |>
    map(~ filter(., issue == "ukraine")) |>
    map(~ lm(as.formula(paste0(y, " ~ post + mip_climate + mip_ukraine + polaware + misinfo_personal + misinfo_general + age + gender + region + education + personal_income + household_income + leftright + party_last + party_next + device_category")), data = .)) |> 
    mice::pool()
  
  # combined
  m_post[[paste0("imp_combo_", y)]] <- imputed_data |>
    map(~ lm(as.formula(paste0(y, " ~ post + issue + mip_climate + mip_ukraine + polaware + misinfo_personal + misinfo_general + age + gender + region + education + personal_income + household_income + leftright + party_last + party_next + device_category")), data = .)) |>
    mice::pool()
}

# merge results
m_df <- map(m_post, tidy, conf.int = TRUE) |> 
  bind_rows(.id = "model") |> 
  mutate(model_name = "m_post") |> 
  separate(model, sep = "_", into = c("model_type", "issue", "outcome"), extra = "drop") |> 
  mutate(term = str_remove(term, "1"),
         conf.low90 = estimate - std.error * 1.645,
         conf.high90 = estimate + std.error * 1.645,
         across(c(estimate, conf.low, conf.low90, conf.high, conf.high90), ~ . * 100),
         issue = case_when(issue == "climate" ~ "Climate",
                           issue == "combo" ~ "Combined",
                           issue == "ukraine" ~ "Ukraine"),
         issue = factor(issue, 
                        levels = c("Climate", "Ukraine", "Combined")),
         outcome_cat = case_when(str_detect(outcome, "lottery|policy") ~ "WTP",
                                 str_detect(outcome, "challenge|threat|worry") ~ "Salience",
                                 str_detect(outcome, "wta") ~ "WTA",
                                 str_starts(outcome, "en") ~ "WTA_items",
                                 TRUE ~ "Other"),
         outcome_cat = factor(outcome_cat, 
                              levels = c("WTP", "Salience", "WTA", "Other", "WTA_items")),
         outcome = factor(outcome, 
                          levels = c("lottery", "policy", "challenge", "threat", "worry", "wta", "refugees", "nimby", "stillproblem", "entrans", "enfly", "enpower", "enheat", "enlocal", "enrecycle"),
                          labels = c("Donations", "Policy", "Challenge", "Threat", "Worry", "WTA index", "Refugees", "NIMBYism", "Still a problem", "Use public transportation", "Fly less", "Conserve electricity", "Use less heating", "Buy local goods", "Recycle more"))) |> 
    filter(term == "post")

#######################################################################
## Figure 2
#######################################################################

# Plot data
p_dat <- unimputed_data |> 
  pivot_longer(all_of(outcomes), names_to = "outcome", values_to = "response") |> 
  mutate(issue = case_when(issue == "climate" ~ "Climate",
                           issue == "combo" ~ "Combined",
                           issue == "ukraine" ~ "Ukraine"),
         issue = factor(issue, 
                        levels = c("Climate", "Ukraine", "Combined")),
         outcome_cat = case_when(str_detect(outcome, "lottery|policy") ~ "WTP",
                                 str_detect(outcome, "challenge|threat|worry") ~ "Salience",
                                 str_detect(outcome, "wta") ~ "WTA",
                                 str_starts(outcome, "en") ~ "WTA_items",
                                 TRUE ~ "Other"),
         outcome_cat = factor(outcome_cat, 
                              levels = c("WTP", "Salience", "WTA", "Other", "WTA_items")),
         outcome = factor(outcome, 
                          levels = c("lottery", "policy", "challenge", "threat", "worry", "wta", "refugees", "nimby", "stillproblem", "entrans", "enfly", "enpower", "enheat", "enlocal", "enrecycle"),
                          labels = c("Donations", "Policy", "Challenge", "Threat", "Worry", "WTA index", "Refugees", "NIMBYism", "Still a problem", "Use public transportation", "Fly less", "Conserve electricity", "Use less heating", "Buy local goods", "Recycle more"))) 

p_dat_exp <- p_dat |> 
  group_by(post, issue, outcome, outcome_cat) |> 
  mutate(response = response * 100) |> 
  summarise(mean_cl_normal(response), .groups = "drop") |> 
  filter(!is.na(post)) 

p_dat_con <- p_dat |> 
  filter((polyorder == "post_climate_ukraine" & issue == "Climate") | (polyorder == "post_ukraine_climate" & issue == "Ukraine")) |> 
  group_by(issue, outcome, outcome_cat) |> 
  mutate(response = response * 100) |> 
  summarise(mean_cl_normal(response), .groups = "drop") |> 
  mutate(post = factor(-1))

#######################################################################
## Figure 2: TOP
#######################################################################

plot_limits <- c(0,85)

p_wtp <- bind_rows(p_dat_con, p_dat_exp) |> 
  filter(outcome_cat == "WTP") |> 
  mutate(post = factor(post, levels = c(-1,0,1), labels = c("Pure", "Control", "Treatment"))) |> 
  filter(post != "Pure") |> 
  ggplot() +
  aes(x = as.integer(post), y = y, ymin = ymin, ymax = ymax, fill = issue, color = issue) +
  geom_col(position = position_dodge(width = .9), alpha = .75, color = NA) +
  geom_errorbar(position = position_dodge(width = .9), linewidth = .4, show.legend = FALSE, width = .25) +
  facet_wrap(~ outcome, nrow = 1) +
  theme_minimal() +
  scale_x_continuous(breaks = 1:3, labels = c("Pure", "Control", "Treatment")) +
  scale_y_continuous(breaks = seq(-100,100,10), expand = c(0,0)) +
  scale_color_manual(name = NULL, values = c("#028A04", "#0057B8")) +
  scale_fill_manual(name = NULL, values = c("#028A04", "#0057B8")) +
  labs(x = NULL, y = "Percent", color = NULL, fill = NULL, subtitle = "(A) Willingness to pay")  +
  coord_cartesian(ylim = plot_limits) +
  theme(legend.position = c(.2,.85), 
        legend.direction = "vertical")

p_sal <- bind_rows(p_dat_con, p_dat_exp) |> 
  filter(outcome_cat == "Salience") |> 
  mutate(post = factor(post, levels = c(-1,0,1), labels = c("Pure", "Control", "Treatment"))) |> 
  filter(post != "Pure") |> 
  ggplot() +
  aes(x = as.integer(post), y = y, ymin = ymin, ymax = ymax, fill = issue, color = issue) +
  geom_col(position = position_dodge(width = .9), alpha = .75, color = NA) +
  geom_errorbar(position = position_dodge(width = .9), linewidth = .4, show.legend = FALSE, width = .25) +
  facet_wrap(~ outcome, nrow = 1) +
  theme_minimal() +
  scale_x_continuous(breaks = 1:3, labels = c("Pure", "Control", "Treatment")) +
  scale_y_continuous(breaks = seq(-100,100,10), expand = c(0,0)) +
  scale_color_manual(values = c("#028A04", "#0057B8")) +
  scale_fill_manual(values = c("#028A04", "#0057B8")) +
  labs(x = NULL, y = NULL, fill = NULL, subtitle = "(B) Crisis perception")  +
  coord_cartesian(ylim = plot_limits) +
  theme(axis.text.y = element_blank()) +
  guides(fill = "none")

p_wta <- bind_rows(p_dat_con, p_dat_exp) |> 
  filter(outcome_cat == "WTA") |> 
  mutate(post = factor(post, levels = c(-1,0,1), labels = c("Pure", "Control", "Treatment"))) |> 
  filter(post != "Pure") |> 
  ggplot() +
  aes(x = as.integer(post), y = y, ymin = ymin, ymax = ymax, fill = issue, color = issue) +
  geom_col(position = position_dodge(width = .9), alpha = .75, color = NA) +
  geom_errorbar(position = position_dodge(width = .9), linewidth = .4, show.legend = FALSE, width = .25) +
  facet_wrap(~ outcome, nrow = 1) +
  theme_minimal() +
  scale_x_continuous(breaks = 1:3, labels = c("Pure", "Control", "Treatment")) +
  scale_y_continuous(breaks = seq(-100,100,10), expand = c(0,0)) +
  scale_color_manual(values = c("#028A04", "#0057B8")) +
  scale_fill_manual(values = c("#028A04", "#0057B8")) +
  labs(x = NULL, y = NULL, fill = NULL, subtitle = "(C) Willingness to act")  +
  coord_cartesian(ylim = plot_limits) +
  theme(axis.text.y = element_blank()) +
  guides(fill = "none")

p1 <- (p_wtp | p_sal | p_wta) + 
  plot_layout(widths = c(8*1.1, 12*1.1, 4*1.1)) & 
  theme(plot.subtitle = element_text(size = 12, hjust = .5, face = "bold"),         
        text = element_text(family = "Times New Roman"),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 7),
        strip.text = element_text(size = 1, color = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title.align = .5,
        legend.key = element_rect(fill = "white", color = "white"))

p1

#######################################################################
## Figure 2: BOTTOM
#######################################################################

plot_limits <- c(-14.9, 7.4)

p_post_wtp <- m_df |> 
  # choose coefs
  filter(model_name == "m_post", model_type == "imp", outcome_cat == "WTP") |> 
  ggplot() +
  aes(x = outcome, y = estimate, ymin = conf.low, ymax = conf.high, color = issue) +
  geom_hline(aes(yintercept = 0), linetype = "solid", linewidth = .5) +
  geom_linerange(aes(ymin = conf.low90, ymax = conf.high90), linewidth = .8, position = position_dodge(width = .3)) +
  geom_pointrange(size = .25, linewidth = .3, position = position_dodge(width = .3)) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(-100,100,5), expand = c(0,0), limits = plot_limits) +
  scale_color_manual(breaks = c("Combined", "Climate", "Ukraine"), values = c("grey5", "#028A04", "#0057B8"), labels = c("Average", "Climate", "Ukraine")) +
  labs(x = NULL, y = "Effect estimate, %-points", color = NULL, subtitle = "Willingness to pay")

p_post_sal <- m_df |> 
  # choose coefs
  filter(model_name == "m_post", model_type == "imp", outcome_cat == "Salience") |> 
  ggplot() +
  aes(x = outcome, y = estimate, ymin = conf.low, ymax = conf.high, color = issue) +
  geom_hline(aes(yintercept = 0), linetype = "solid", linewidth = .5) +
  geom_linerange(aes(ymin = conf.low90, ymax = conf.high90), linewidth = .8, position = position_dodge(width = .3)) +
  geom_pointrange(size = .25, linewidth = .3, position = position_dodge(width = .3)) +
  theme_minimal() +
  theme(axis.text.y = element_blank()) +
  scale_y_continuous(breaks = seq(-100,100,5), expand = c(0,0), limits = plot_limits) +
  scale_color_manual(breaks = c("Combined", "Climate", "Ukraine"), values = c("grey5", "#028A04", "#0057B8"), labels = c("Average", "Climate", "Ukraine")) +
  labs(x = NULL, y = NULL, color = NULL, subtitle = "Issue salience")

p_post_wta <- m_df |> 
  # choose coefs
  filter(model_name == "m_post", model_type == "imp", outcome_cat == "WTA") |> 
  ggplot() +
  aes(x = outcome, y = estimate, ymin = conf.low, ymax = conf.high, color = issue) +
  geom_hline(aes(yintercept = 0), linetype = "solid", linewidth = .5) +
  geom_linerange(aes(ymin = conf.low90, ymax = conf.high90), linewidth = .8, position = position_dodge(width = .3)) +
  geom_pointrange(size = .25, linewidth = .3, position = position_dodge(width = .3)) +
  theme_minimal() +
  theme(axis.text.y = element_blank()) +
  scale_y_continuous(breaks = seq(-100,100,5), expand = c(0,0), limits = plot_limits) +
  scale_color_manual(breaks = c("Combined", "Climate", "Ukraine"), values = c("grey5", "#028A04", "#0057B8"), labels = c("Average", "Climate", "Ukraine")) +
  labs(x = NULL, y = NULL, color = NULL, subtitle = "Willingness to act")

p2 <- (p_post_wtp | p_post_sal | p_post_wta) + 
  plot_layout(widths = c(8*1.1, 12*1.1, 4*1.1), guides = "collect") & 
  theme(legend.position = "bottom", 
        legend.direction = "horizontal", 
        plot.subtitle = element_text(size = 12, hjust = .5, face = "bold"),
        text = element_text(family = "Times New Roman"),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9),
        strip.text = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid = element_blank(),
  )

p2

#######################################################################
## Figure 2: COMBINED
#######################################################################

p1b <- (p_wtp + guides(fill = "none", color = "none") | p_sal | p_wta) + 
  plot_layout(widths = c(8*1.1, 12*1.1, 4*1.1)) & 
  theme(plot.subtitle = element_text(size = 12, hjust = .5, face = "bold"),         
        text = element_text(family = "Times New Roman"),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 7),
        strip.text = element_text(size = 9),
        panel.grid = element_blank(),
        legend.title.align = .5,
        legend.key = element_rect(fill = "white", color = "white"))

p2b <- (p_post_wtp + labs(subtitle = NULL) | p_post_sal + labs(subtitle = NULL) | p_post_wta + labs(subtitle = NULL)) + 
  plot_layout(widths = c(8*1.1, 12*1.1, 4*1.1), guides = "collect") & 
  theme(legend.position = "bottom", 
        legend.direction = "horizontal", 
        plot.subtitle = element_text(size = 12, hjust = .5, face = "bold"),
        text = element_text(family = "Times New Roman"),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9),
        strip.text = element_blank(),
        panel.grid = element_blank())

p_multi <- p1b / p2b
p_multi

ggsave("figure2.pdf", p_multi, bg = "white", width = 8.75, height = 5.75, device = cairo_pdf)
ggsave("figure2.png", p_multi, bg = "white", width = 8.75, height = 5.75, dpi = 300)

##---------------------------------------------------------------------
##---------------------------------------------------------------------
##                                                                   
##       ~       FIGURE 3: POLICY SUPPORT AND POCKETBOOK COSTS
##                                                                   
##---------------------------------------------------------------------
##---------------------------------------------------------------------

#######################################################################
## A policy support vs. price -- descriptive
#######################################################################

p_dat <- unimputed_data |> 
  pivot_longer(all_of(outcomes), names_to = "outcome", values_to = "response") |> 
  mutate(issue = case_when(issue == "climate" ~ "Climate",
                           issue == "combo" ~ "Combined",
                           issue == "ukraine" ~ "Ukraine"),
         issue = factor(issue, 
                        levels = c("Climate", "Ukraine", "Combined")),
         outcome_cat = case_when(str_detect(outcome, "lottery|policy") ~ "WTP",
                                 str_detect(outcome, "challenge|threat|worry") ~ "Salience",
                                 str_detect(outcome, "wta") ~ "WTA",
                                 str_starts(outcome, "en") ~ "WTA_items",
                                 TRUE ~ "Other"),
         outcome_cat = factor(outcome_cat, 
                              levels = c("WTP", "Salience", "WTA", "Other", "WTA_items")),
         outcome = factor(outcome, 
                          levels = c("lottery", "policy", "challenge", "threat", "worry", "wta", "refugees", "nimby", "stillproblem", "entrans", "enfly", "enpower", "enheat", "enlocal", "enrecycle"),
                          labels = c("Donations", "Policy", "Challenge", "Threat", "Worry", "WTA index", "Refugees", "NIMBYism", "Still a problem", "Use public transportation", "Fly less", "Conserve electricity", "Use less heating", "Buy local goods", "Recycle more"))) 

p_dat_con <- p_dat |> 
  filter(outcome == "Policy") |> 
  filter(post == 1) |> 
  group_by(issue, policyamount3) |> 
  mutate(response = response * 100) |> 
  summarise(mean_cl_normal(response), .groups = "drop") |> 
  mutate(post = factor(-1))

# simple version
p_policy_price <- p_dat_con |> 
  ggplot() +
  aes(x = policyamount3, y = y, ymin = ymin, ymax = ymax, fill = issue, color = issue) +
  geom_col(position = position_dodge(width = .9), alpha = .75, color = NA) +
  geom_linerange(linewidth = .25, position = position_dodge(width = .9), color = "black", alpha = .66) +
  scale_y_continuous(breaks = seq(-100,100,10), expand = c(0,0)) +
  scale_color_manual(values = c("#028A04", "#0057B8")) +
  scale_fill_manual(values = c("#028A04", "#0057B8")) +
  theme_minimal() +
  labs(x = "Policy cost (DKK)", y = "Policy support (%)", fill = NULL, color = NULL) + 
  theme(plot.subtitle = element_text(size = 12, hjust = .5, face = "bold"),   
        legend.position = "bottom",
        legend.direction = "horizontal",
        text = element_text(family = "Times New Roman"),
        axis.text = element_text(size = 9),
        strip.text = element_text(size = 9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title.align = .5,
        legend.key = element_rect(fill = "white", color = "white"))

p_policy_price

#######################################################################
## B policy support vs. price -- experimental effect
#######################################################################

# 1: POST effect at low-mid-high prices 
# climate
m_price_climate <- imputed_data |>
  map(~ filter(., issue == "climate")) |>
  map(~ lm(policy ~ policyamount3 / post  + mip_climate + mip_ukraine + polaware + misinfo_personal + misinfo_general + age + gender + region + education + personal_income + household_income + leftright + party_last + party_next + device_category, data = .)) |> 
  mice::pool() |> 
  tidy(conf.int = TRUE) |> 
  mutate(issue = "climate")

m_price_ukraine <- imputed_data |>
  map(~ filter(., issue == "ukraine")) |>
  map(~ lm(policy ~ policyamount3 / post  + mip_climate + mip_ukraine + polaware + misinfo_personal + misinfo_general + age + gender + region + education + personal_income + household_income + leftright + party_last + party_next + device_category, data = .)) |> 
  mice::pool() |> 
  tidy(conf.int = TRUE) |> 
  mutate(issue = "ukraine")

p_policy_price_effect <- bind_rows(m_price_climate, m_price_ukraine) |> 
  filter(str_detect(term, "poly|post|proximate|vignette")) |> 
  filter(str_detect(term, ":")) |> 
  mutate(conf.low90 = estimate - std.error * 1.645,
         conf.high90 = estimate + std.error * 1.645,
         across(c(estimate, conf.low, conf.low90, conf.high, conf.high90), ~ . * 100),
         x = factor(str_extract(term, "Low|Mid|High"), levels = c("Low", "Mid", "High"), labels = c("Low\n(10-20)", "Mid\n(50-200)", "High\n(500-1000)")),
         issue = case_when(issue == "climate" ~ "Climate",
                           issue == "combo" ~ "Combined",
                           issue == "ukraine" ~ "Ukraine"),
         issue = factor(issue, 
                        levels = c("Climate", "Ukraine", "Combined"))) |>
  ggplot() +
  aes(x = x, y = estimate, ymin = conf.low, ymax = conf.high, color = issue) +
  geom_hline(aes(yintercept = 0), linetype = "solid", size = .5) +
  geom_linerange(aes(ymin = conf.low90, ymax = conf.high90), linewidth = .8, position = position_dodge(width = .3)) +
  geom_pointrange(size = .5, linewidth = .4, position = position_dodge(width = .3)) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(-100,100,5), expand = c(0,0)) +
  scale_color_manual(values = c("#028A04", "#0057B8", "grey33")) +
  labs(x = "Policy cost (DKK)", y = "Marginal effect estimate, %-points", subtitle = NULL, color = NULL) +
  theme(plot.subtitle = element_text(size = 12, hjust = .5, face = "bold"),   
        legend.position = c(.85,.9),
        text = element_text(family = "Times New Roman"),
        axis.text = element_text(size = 9),
        strip.text = element_text(size = 9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title.align = .5)

#######################################################################
## combine as Figure 3
#######################################################################

p4 <- (p_policy_price | (p_policy_price_effect + guides(color = "none"))) + 
  plot_layout(guides = "collect") & 
  theme(plot.subtitle = element_text(size = 11, hjust = .5, face = "bold"),         
        text = element_text(family = "Times New Roman"),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 7),
        strip.text = element_text(size = 9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title.align = .5,
        legend.position = "bottom",
        legend.key = element_rect(fill = "white", color = "white")) &
  plot_annotation(tag_levels = "A") 

p4

ggsave("figure3.pdf", p4, bg = "white", width = 8, height = 3.5, device = cairo_pdf)
ggsave("figure3.png", p4, bg = "white", width = 8, height = 3.5)
