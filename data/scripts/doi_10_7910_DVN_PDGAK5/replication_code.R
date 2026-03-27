##---------------------------------------------------------------------
## Replication code for the POBE article "Pro-climate voting in response to local flooding"
##---------------------------------------------------------------------
## CREATED:		  24 Sep 2024
## AUTHOR:		  Søren Damsbo-Svendsen
## AFFILIATION:	University of Copenhagen
## WEBSITE:		  https://soerendamsbo.github.io
##---------------------------------------------------------------------

# Load R packages
pacman::p_load(tidyverse, janitor, scales, lubridate, fixest, did, patchwork, purrr, modelsummary, ggthemes)

# Create function to create treatment indicator from treatment threshold
treatdummy_rev <- function(x, cutoff = 1){
  factor(if_else(x <= cutoff, 1, 0, 0))
}

# ggplot2 theme
theme_set(theme_tufte(base_size = 12, 
                      base_family = "Times New Roman"))
theme_update(plot.subtitle = element_text(hjust=0.5),
             plot.title = element_text(hjust=0.5),
             plot.margin = unit(c(0.2,0.1,0.2,0.1), "cm"),
             axis.line = element_line(linewidth=0.5),
             axis.text = element_text(size = 9),
             panel.grid.major.x = element_blank(),
             panel.grid.minor.x = element_blank(),
             panel.grid.major.y = element_line(linewidth = 0.2),
             panel.grid.minor.y = element_blank(),
             strip.text = element_text(size = 12, face = "bold"))

##---------------------------------------------------------------------
##---------------------------------------------------------------------
##                                                                   
##       ~       STUDY 1: Local flooding and pro-climate party voting                             
##                                                                   
##---------------------------------------------------------------------
##---------------------------------------------------------------------

# Load data
study1_dataset <- read_rds("study1_dataset.rds")

#######################################################################
## Figure 2: Parallel trends plot (70 worst hit districts)
#######################################################################

# plot data
p_dat <- study1_dataset |>
  mutate(treatment_group = treatdummy_rev(treatment_size, 70)) |> 
  group_by(election,
           election_date,
           year, 
           post, 
           treatment_group) |> 
  summarise(voteshare_climate = mean(voteshare_climate)) |>
  group_by(treatment_group) |> 
  arrange(election) |> 
  mutate(outcome_diff = c(NA, diff(voteshare_climate))) |> 
  group_by(election) |>
  arrange(desc(treatment_group)) |> 
  mutate(outcome_did = first(outcome_diff) - last(outcome_diff)) |>  
  ungroup() |> 
  mutate(election_label = paste0(year, " election"),
         treament_group = factor(treatment_group,
                                 levels = c(1,0),
                                 labels = c("Treatment",
                                            "Control")))  

counterfactual_data <- p_dat |> 
  filter(year %in% c(2011, 2015)) |> 
  group_by(treatment_group) |> 
  mutate(diff = diff(voteshare_climate)) |>
  group_by(post) |> 
  arrange(treatment_group) |> 
  mutate(diffindiff = diff(diff),
         counterfactual = ifelse(treatment_group == 1 & post == 1, voteshare_climate - diffindiff, voteshare_climate)) |> 
  filter(treatment_group == 1) 

# plot colors
cols <- c("#84c652", MetBrewer::met.brewer("Tiepolo")[8])

p2 <- p_dat |>
  ggplot(aes(x = election_date, y = voteshare_climate, color = fct_rev(factor(treatment_group,
                                                                              levels = c(1,0),
                                                                              labels = c("Treatment group",
                                                                                         "Control group"))))) +
  geom_hline(aes(yintercept = 35),size = 3, color = "white") +
  annotate("rect", xmin = ymd("20110915")-75, xmax = ymd("20150618")+75, ymin = 33.33, ymax = 55, alpha = .1) +
  geom_vline(aes(xintercept = ymd("20131205")), linetype = "dashed", size = .5, color = cols[[2]]) +
  geom_line() +
  geom_point() +
  geomtextpath::geom_texthline(aes(yintercept = 35, label = "Difference in\ndifferences\n"), size = 3.5, hjust = .961, vjust = .25, family = "Times New Roman") +
  geom_hline(aes(yintercept = 35), size = .5) +
  geom_segment(aes(y = 35, yend = 35 + outcome_did, x = election_date, xend = election_date, fill = NULL, color = NULL), data = p_dat |> filter(treatment_group == 1, year <= 2015), show.legend = FALSE,size = .5) +
  geom_point(aes(y = 35 + outcome_did, x = election_date, fill = NULL, color = NULL), 
             data = p_dat |> filter(treatment_group == 1, year <= 2015), show.legend = FALSE) +
  geom_line(aes(y = counterfactual), size = 4, data = counterfactual_data, linewidth = .5, linetype = "dotted", show.legend = FALSE) +
  geomtextpath::geom_textsegment(aes(x = ymd("20150618"), xend = ymd("20150618"), yend = counterfactual[2], y = voteshare_climate[2], label = "ATT"), size = 3, data = counterfactual_data, linetype = "dotted", show.legend = FALSE, family = "Times New Roman") +
  geom_point(aes(y = counterfactual), data = counterfactual_data) +
  annotate("label", x = ymd("20130501"), y = 42, label = "2013 storm surge", size = 4, hjust = 1, vjust = 0.5, color = cols[[2]], label.size = 0.35, family = "Times New Roman", fill = "white") +
  annotate("curve", x = ymd("20130501"), xend = ymd("20131206"), y = 42, yend = 41, size = 0.35, curvature = -0.33, color = cols[[2]]) +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 10, face = "plain")) +
  scale_x_date(breaks = p_dat$election_date, date_labels = "%Y", expand = c(0,365)) +
  scale_y_continuous(breaks = seq(0,100, 5), limits = c(33.33, 55), expand = c(0,0),
                     sec.axis = sec_axis(~ . - 35, breaks = seq(-15,15,5))) +
  scale_color_manual(values = cols) +
  labs(color = NULL, y = "Pro-climate party vote share (%)", 
       x = "Election") +
  guides(color = guide_legend(reverse=TRUE)) +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14, face = "plain"),
        legend.title = element_text(size = 16, face = "plain"),
        legend.text = element_text(size = 16, face = "plain"),
        plot.subtitle = element_text(size = 16, face = "plain"))

p2

ggsave("Fig2.png", dpi = 300, width = 6.5, height = 4.5, bg = "white")
ggsave("Fig2.pdf", dpi = 300, width = 6.5, height = 4.5, bg = "white", device = cairo_pdf)

#######################################################################
## figure 3: main did analysis
#######################################################################

# Iteration: intensity
dat_iter <- map(seq(300, 30, -10),
                ~study1_dataset |>
                  filter(election %in% c("FV2011", "FV2015")) |> 
                  mutate(treatment = as.double(treatdummy_rev(treatment_size, .x)) - 1))

m_iter <- map(dat_iter, ~feols(voteshare_climate ~ post * treatment, 
                               data = .,
                               vcov = vcov_cluster(~pplace_id))) |> 
  map(tidy, conf.int = TRUE) |> 
  bind_rows(.id = "id") |> 
  filter(term == "post1:treatment") |> 
  mutate(x = seq(300, 30, -10))

# example: 70 worst hit districts
study1_dataset |>
  filter(election %in% c("FV2011", "FV2015")) |> 
  mutate(treatment = as.double(treatdummy_rev(treatment_size, 70)) - 1) |> 
  feols(voteshare_climate ~ post * treatment, 
        data = _,
        vcov = vcov_cluster(~pplace_id)) |> 
  etable()

# plot
m_iter |> 
  ggplot() +
  aes(x = x, ymin = conf.low, ymax = conf.high, y = estimate) +
  geom_pointrange(shape = 21, fill = "black") +
  geom_hline(aes(yintercept = 0), linetype = "dashed", size = .5, color = "black") +
  scale_x_continuous(breaks = seq(300,0,-50), expand = c(0,5), trans = "reverse") +
  scale_y_continuous(breaks = seq(-10,10,.5), expand = c(0,.2)) +
  scale_color_manual(name = NA, values = c("black", "black", "darkred", "black")) +
  labs(x = "Number of worst hit polling districts in treatment group", y = "DID estimate (%-points)", color = "Treatment") +
  theme(panel.border = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 9),
        legend.position = "bottom")

## Add matching-based estimates
dat_analysis <- study1_dataset |> 
  filter(election %in% c("FV2011", "FV2015")) |> 
  mutate(time_fv = case_when(year == 2011 ~ 0,
                             year == 2015 ~ 1,
                             year == 2019 ~ 2,
                             year == 2007 ~ -1,
                             year == 2005 ~ -2,
                             year == 2001 ~ -3,
                             year == 1998 ~ -4,
                             year == 1994 ~ -5)) |> 
  # matching covariates: average values
  group_by(pplace_id) |> 
  mutate(turnout = mean(ifelse(year <= 2013, turnout, NA_real_), na.rm = TRUE),
         votes_eligible = mean(ifelse(year <= 2013, votes_eligible, NA_real_), na.rm = TRUE),
         pplace_id_f = factor(pplace_id),
         log_pplace_dist_to_coast = log1p(pplace_dist_to_coast),
         log_pplace_area = log1p(pplace_area),
         log_wealth_sum = log1p(wealth_sum - min(study1_dataset$wealth_sum, na.rm = TRUE)),
         log_wealth_avg = log1p(wealth_avg - min(study1_dataset$wealth_avg, na.rm = TRUE)),
         log_income_50p = log1p(income_50p),
         log_households_n = log1p(households_n)) |> 
  ungroup()

# use latent treatment intensity variable
dat_iter2 <- map(seq(300, 30, -10),
                 ~dat_analysis |>
                   filter(election %in% c("FV2011", "FV2015")) |> 
                   mutate(treatment = as.double(treatdummy_rev(treatment_size, .x)) - 1))

names(dat_iter2) <- paste0("n", seq(300, 30, -10))

# IPW model: varying treatment intensity
mipw <- dat_iter2 |>
  map(~att_gt(yname = "voteshare_climate",
              tname = "time_fv",
              idname = "pplace_id",
              gname = "treatment",
              xformla = ~turnout + education_academic + education_only_primary + noncitizens_prop + log_pplace_area + houses_regular + log_income_50p + log_wealth_sum,
              est_method = "ipw",
              base_period = "varying",
              control_group = "nevertreated",
              anticipation = 0,
              panel = TRUE,
              clustervars = "pplace_id",
              bstrap = TRUE,
              biters = 1000,
              allow_unbalanced_panel = TRUE,
              pl = TRUE,
              cores = 6,
              print_details = FALSE,
              data = .)) |>
  map(~aggte(., type="dynamic", na.rm = FALSE, cband = TRUE, bstrap = TRUE, biters = 1000)) |>
  map(tidy) |>
  bind_rows(.id = "treatment")

# Example: 70 worst hit districts
dat_analysis |>
  filter(election %in% c("FV2011", "FV2015")) |> 
  mutate(treatment = as.double(treatdummy_rev(treatment_size, 70)) - 1) |> 
  att_gt(yname = "voteshare_climate",
         tname = "time_fv",
         idname = "pplace_id",
         gname = "treatment",
         xformla = ~turnout + education_academic + education_only_primary + noncitizens_prop + log_pplace_area + houses_regular + log_income_50p + log_wealth_sum,
         est_method = "ipw",
         base_period = "varying",
         control_group = "nevertreated",
         anticipation = 0,
         panel = TRUE,
         clustervars = "pplace_id",
         bstrap = TRUE,
         biters = 1000,
         allow_unbalanced_panel = TRUE,
         pl = TRUE,
         cores = 6,
         print_details = FALSE,
         data = _) |>
  aggte(type="dynamic", na.rm = FALSE, cband = TRUE, bstrap = TRUE, biters = 1000) 

p_dat <- mipw |> 
  mutate(treatment_intensity = as.integer(str_extract(treatment, "[0-9]{1,3}")),
         x = seq(300, 30, -10))

# Plot
p_dat |> 
  ggplot() +
  aes(x = treatment_intensity, y = estimate, ymin = conf.low, ymax = conf.high) +
  geom_pointrange(size = .5, linewidth = .5, position = position_dodge(0.4), shape = 21, fill = "white") +
  geom_hline(aes(yintercept = 0), linetype = "dashed", size = .5, color = "black") +
  scale_color_grey() +
  scale_x_continuous(breaks = seq(300,0,-50), expand = c(0,5), trans = "reverse") +
  scale_y_continuous(breaks = seq(-10,10,.5), expand = c(0,.2)) +
  labs(x = "Number of worst hit polling districts in treatment group", y = "DID estimate (%-points)", color = "Treatment") +
  theme(panel.border = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 9),
        legend.position = "bottom")

## combine raw and matching estimates in Figure 3

# Plot data
p_dat_combo <- bind_rows(
  mutate(m_iter, adjust = "No matching"),
  mutate(p_dat, adjust = "Propensity score matching")
) 

p_dat_combo |> 
  ggplot() +
  aes(x = x, y = estimate, ymin = conf.low, ymax = conf.high, fill = adjust) +
  geom_pointrange(size = .35, linewidth = .25, position = position_dodge2(width = 5), stroke = .3, shape = 21) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", size = .5, color = "black") +
  scale_x_continuous(breaks = seq(300,0,-50), expand = c(0,5), trans = "reverse") +
  scale_y_continuous(breaks = seq(-10,10,.5), expand = c(0,.2)) +
  scale_fill_manual(values = c("black", "white")) +
  labs(x = "Number of worst hit polling districts in treatment group", y = "DID estimate (%-points)", color = "Treatment", fill = NULL) +
  guides(shape = "none",
         fill = guide_legend(override.aes = list(fill = c("black", "white"),
                                                 shape = 21, 
                                                 size = .6))) +
  theme(panel.border = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 9),
        legend.position = "bottom")

ggsave("Fig3.png", width = 6.5, height = 4, bg = "white")
ggsave("Fig3.pdf", width = 6.5, height = 4, bg = "white", device = cairo_pdf)

#######################################################################
## figure 4: event study plot
#######################################################################

mipw_data <- study1_dataset |> 
  mutate(treatment = as.integer(treatdummy_rev(treatment_size, 70))-1) |> 
  # covariates: average values
  group_by(pplace_id) |> 
  mutate(turnout = mean(ifelse(year <= 2013, turnout, NA_real_), na.rm = TRUE),
         votes_eligible = mean(ifelse(year <= 2013, votes_eligible, NA_real_), na.rm = TRUE),
         pplace_id_f = factor(pplace_id),
         log_pplace_dist_to_coast = log1p(pplace_dist_to_coast),
         log_pplace_area = log1p(pplace_area),
         log_wealth_sum = log1p(wealth_sum - min(study1_dataset$wealth_sum, na.rm = TRUE)),
         log_wealth_avg = log1p(wealth_avg - min(study1_dataset$wealth_avg, na.rm = TRUE)),
         log_income_50p = log1p(income_50p),
         log_households_n = log1p(households_n)) |> 
  ungroup() |> glimpse()

# IPW model and event study plot: damage
mipw <- mipw_data |> 
  att_gt(yname = "voteshare_climate",
         tname = "time_fv",
         idname = "pplace_id",
         gname = "treatment",
         xformla = ~turnout + education_academic + education_only_primary + noncitizens_prop + log_pplace_area + houses_regular + log_income_50p + log_wealth_sum,
         est_method = "ipw",
         base_period = "varying",
         control_group = "nevertreated", 
         anticipation = 0,
         panel = TRUE,
         clustervars = "pplace_id",
         bstrap = TRUE,
         biters = 1000,
         allow_unbalanced_panel = TRUE,
         pl = TRUE,
         cores = 6,
         print_details = FALSE,
         data = _) 

mipw_list <- mipw |> 
  aggte(type="dynamic", na.rm = TRUE, cband = TRUE, bstrap = TRUE, biters = 1000)
msummary(mipw_list)

# plot data
pdat <- mipw_list |> 
  tidy()

# plot
pdat |> 
  ggplot() +
  aes(x = event.time, y = estimate, ymin = conf.low, ymax = conf.high) +
  geom_pointrange(size = .5, linewidth = .5, stroke = .5, shape = 21, fill = "black") +
  geom_hline(aes(yintercept = 0), linetype = "dashed", size = .5, color = "black") +
  scale_color_grey() +
  scale_x_continuous(breaks = seq(-10,10,1), expand = c(0,.5)) +
  scale_y_continuous(breaks = seq(-10,10,1), expand = c(0,.2)) +
  labs(x = "Period", y = "DID estimate (%-points)", color = "Treatment") +
  theme(panel.border = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 9),
        legend.position = "bottom")

ggsave("Fig4.png", width = 6.5, height = 3.5, bg = "white")
ggsave("Fig4.pdf", width = 6.5, height = 3.5, bg = "white", device = cairo_pdf)

##---------------------------------------------------------------------
##---------------------------------------------------------------------
##                                                                   
##       ~       STUDY 2: Local flooding and pro-climate candidate voting                             
##                                                                   
##---------------------------------------------------------------------
##---------------------------------------------------------------------

# Load data
study2_dataset <- read_rds("study2_dataset.rds")

#######################################################################
## Figure 5: didid intuition plot
#######################################################################

p1_didid <- study2_dataset |> 
  group_by(election, climate, treatment70) |> 
  summarise(mean_cl_normal(candidate_voteshare),
            .groups = "drop") |> 
  mutate(x = case_when(treatment70 == 0 & !climate ~ "Non-climate\nControl",
                       treatment70 == 0 & climate ~ "Pro-climate\nControl",
                       treatment70 == 1 & !climate ~ "Non-climate\nFlooding",
                       treatment70 == 1 & climate ~ "Pro-climate\nFlooding") |> 
           factor(levels = c("Non-climate\nControl", "Non-climate\nFlooding", "Pro-climate\nControl", "Pro-climate\nFlooding")),
         election = ifelse(election == "KV13", 2013, 2017)) |> 
  ggplot() +
  aes(x = x, y = y, ymin = ymin, ymax = ymax, fill = as.character(election)) +
  geom_col(position = position_dodge(width = .6), width = .6) +
  geom_hline(aes(yintercept = 0), linewidth = .5) +
  #geom_errorbar(position = position_dodge(width = .6), width = .2, linewidth = .2) +
  scale_x_discrete(expand = c(0,.1)) +
  scale_y_continuous(breaks = seq(0, 2, .1), expand = c(0,0)) +
  coord_cartesian(ylim = c(-.1, 1.2)) +
  scale_fill_grey() +
  theme_tufte() +
  theme(panel.grid.major.y = element_blank(),
        strip.text = element_text(size = 12, face = "plain"),
        axis.title = element_text(size = 12, face = "plain"),
        axis.text.x = element_text(size = 12, face = "plain"),
        legend.title = element_text(size = 12, face = "plain"),
        legend.text = element_text(size = 10, face = "plain"),
        legend.key.size = unit(.5, "cm"),
        plot.subtitle = element_text(size = 12, face = "plain", hjust = .5),
        legend.position = c(.88, .95),
        legend.direction = "vertical",
        axis.ticks.x = element_blank()) +
  labs(fill = NULL, y = "Vote share (%)", x = NULL, subtitle = "Group-wise average vote shares\n") +
  guides(fill = "none") +
  geom_text(aes(label = "2013      2017", y = -0.025), size = 2.25, family = "Times New Roman")

values_did <- study2_dataset |> 
  group_by(election, climate, treatment70) |> 
  summarise(y = mean(candidate_voteshare),
            .groups = "drop") |> 
  # change over time
  group_by(treatment70, climate) |> 
  arrange(climate, treatment70, election) |> 
  mutate(y = diff(y)) |> 
  ungroup() |> 
  select(-election) |> 
  distinct() |> 
  # difference (in differences) between flooding and control
  group_by(climate) |> 
  arrange(climate, treatment70) |> 
  mutate(y = diff(y)) |> 
  ungroup() |> 
  select(-treatment70) |> 
  distinct()

# study2_dataset
values_didid <- values_did |> 
  summarise(y = diff(y))

p2_didid <- bind_rows(values_did, values_didid) |> 
  mutate(x = case_when(climate ~ "Pro-climate",
                       !climate ~ "Non-climate",
                       TRUE ~ "DIDID") |> 
           factor(levels = c("Non-climate", "Pro-climate", "DIDID"),
                  labels = c("Non-climate", "Pro-climate", "DIDID")
           )) |> 
  ggplot() +
  aes(x = x, y = y, fill = x) +
  geom_col(position = position_dodge(width = .3), width = .3) +
  geom_hline(aes(yintercept = 0), linewidth = .5) +
  scale_x_discrete(expand = c(0,.5)) +
  scale_y_continuous(breaks = seq(0, 3, .1), expand = c(0,0)) +
  coord_cartesian(ylim = c(-.1, 1.2)) +
  #scale_fill_grey() +
  scale_fill_manual(values = rep("black", 3)) +
  #scale_fill_manual(values = c(tiepolo[8], cols[1], tiepolo[7])) +
  theme_tufte() +
  theme(panel.grid.major.y = element_blank(),
        strip.text = element_text(size = 12, face = "plain"),
        axis.title = element_text(size = 12, face = "plain"),
        axis.text.x = element_text(size = 12, face = "plain"),
        legend.title = element_text(size = 12, face = "plain"),
        legend.text = element_text(size = 12, face = "plain"),
        plot.subtitle = element_text(size = 12, face = "plain", hjust = .5),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  labs(y = NULL, x = NULL, subtitle = "Differences in differences") +
  guides(fill = "none")

(p1_didid | plot_spacer() | p2_didid) + plot_annotation(tag_levels = "A") + 
  plot_layout(widths = c(1+2/3, 1/6, 1+1/3))

ggsave("Fig5.png", dpi = 300, width = 8, height = 4, bg = "white")
ggsave("Fig5.pdf", dpi = 300, width = 8, height = 4, bg = "white", device = cairo_pdf)

#######################################################################
## Figure 6: Treatment intensity and flooding effect 
#######################################################################

# function to create data
create_study2_dataset_data <- function(x, treatment_var, treatment_val = 1) {
  x |> 
    mutate(across(all_of(treatment_var), ~ . , .names = "treatment")) |> 
    mutate(flooding = treatdummy_rev(treatment_size, treatment_val))
}

treatment_matrix <- tibble(iter = 1:28,
                           treatment_var = "treatment_size", 
                           treatment_val = seq(300, 30, -10)) |> 
  glimpse()

study2_dataset_data <- map(1:nrow(treatment_matrix), 
                  ~create_study2_dataset_data(x = study2_dataset, 
                                     treatment_var = treatment_matrix$treatment_var[.],
                                     treatment_val = treatment_matrix$treatment_val[.]))

miter1 <- study2_dataset_data |>   
  map(~feols(candidate_voteshare ~ flooding * post * climate | municipality_id,
             #subset = ~treatment == 0 | flooding,
             vcov = vcov_cluster(~pplace_id), 
             data = .), .progress = TRUE) |> 
  map(tidy, conf.int = TRUE) |> 
  bind_rows(.id = "iter") |> 
  filter(term == "flooding1:post:climateTRUE") |> 
  mutate(x = seq(300, 30, -10))

# plot
miter1 |>
  ggplot() +
  aes(x = x, y = estimate, ymin = conf.low, ymax = conf.high) +
  geom_pointrange(linewidth = .3, size = .35, stroke = .3, fill = "black", shape = 21) +
  geom_hline(aes(yintercept = 0), linewidth = .4, linetype = "dashed") +
  scale_x_continuous(breaks = seq(500,0,-50), expand = c(0,5), trans = "reverse") +
  scale_y_continuous(breaks = seq(-10,10,.1), expand = c(0,.02)) +
  scale_color_manual(values = c("grey5", "grey45", "grey90")) +
  labs(x = "Number of worst hit polling districts in treatment group", y = "study2_dataset estimate (%-points)", color = "Fixed effects", shape = NULL) +
  guides(shape = "none") +
  theme(panel.border = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 9),
        legend.position = "bottom")

ggsave("Fig6.png", width = 6.5, height = 3.5, bg = "white")
ggsave("Fig6.pdf", width = 6.5, height = 3.5, bg = "white", device = cairo_pdf)

