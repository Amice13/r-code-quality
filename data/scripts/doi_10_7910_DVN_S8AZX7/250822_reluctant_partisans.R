#### LOAD PACKAGES ####
pacman::p_load(haven, dplyr, tidyr, ggplot2, fixest, ggtext, data.table, ggrepel,
               ggcorrplot, patchwork, modelsummary, ggalluvial)

#### DEFINE FUNCTIONS ####
# this function takes in a model and returns the odds ratios for the first row of the model
generate_odds_ci <- function(model, coefnum = 1, use_odds = T){
  name <- paste0(model$fml[[3]], collapse = "") # get the name of the model from the equation
  estimate <- model["coeftable"][[1]][[coefnum, 1]]
  se <- model["coeftable"][[1]][[coefnum, 2]]
  ci <- qnorm(.975)*se
  if(use_odds){
    data_row <- exp(c(estimate, estimate + ci, estimate - ci))
  }
  else{
    data_row <- c(estimate, estimate + ci, estimate - ci)
  }
  return_df <- data.frame(name = name, est = data_row[1],
                          ci.max = data_row[2], ci.min = data_row[3])
  return(return_df)
}

# this function takes in a model and returns the coefficient for the SECOND row of the model
# note the unusual approach to extract the variable of interest - different from the earlier model
# because of the interaction
get_interaction <- function(model) {
  coef <- coef(model)[2]
  se <- se(model)[2]
  name <- paste0(model$fml[[3]][[2]], collapse = "") # get the name of the var from the equation
  return_df <- data.frame(name = name, est = coef,
                          ci.max = coef + qnorm(.975)*se,
                          ci.min = coef - qnorm(.975)*se)
  return(return_df)
}

# this function wraps text for ggplot graphics
wrap_text <- function(label, dev_width = dev.size("cm")[1], dev_scaler = 12)  {
  paste(strwrap(label, dev_width * dev_scaler), collapse = "\n")
}

# this function gets the range for a vector
my.range <- function(x){
  ifelse(!all(is.na(x)) & length(x) > 1,
         max(x, na.rm = T) - min(x, na.rm = T),
         NA)
}

#### FIGURE 1 - GALLUP PLOT ####
gallup_df <- fread(file = "data/gallup_pid_2024.csv") |>
  mutate(year = Year,
         Republican = `% Republican`,
         Independent = `% Independent`,
         Democrat = `% Democrat`) |>
  pivot_longer(cols = c(Republican, Independent, Democrat),
               names_to = "Party",
               values_to = "Proportion")
gallup_df$Party <- factor(gallup_df$Party, levels = c("Independent", "Democrat", "Republican"))

png(filename = "img/gallup_plot_v2.png",
    width = 20, height = 12, units = "cm", type = "cairo-png", res = 300)
ggplot(gallup_df,
       aes(x = year, y = Proportion, color = Party)) +
  geom_line(alpha = 0.7, linewidth = 1.1) +
  scale_color_manual(values = c("black", "blue", "red")) +
  labs(x = "Year",
       y = "Proportion of Respondents",
       color = "Party",
       caption = "Lines represent yearly average for the question \"In politics, as of today, do you consider yourself a Republican, a Democrat or an independent?\" on Gallup surveys.") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_textbox_simple(hjust = 0,
                                              width = grid::unit(16, "cm"),
                                              size = 9,
                                              margin = margin(5,0,0,0)))
dev.off()

#### LOAD PANEL DATA ####
df <- read_dta("data/250425_amerispeak.dta")

#### ADD STATIC VARIABLES BY INITIAL LEVELS ####
# for interest and self-censorship, we want to get the initial level for each respondent
# these are used for the interaction plots in the appendix
df <- df |> group_by(ID) |>
  mutate(init_interest = dplyr::first(na.omit(pol_interest)),
         init_selfcensor = dplyr::first(na.omit(censorship_index))) |>
  ungroup()

#### FIGURE 2 MODELS ####
# identify which respondents entered the panel as democrats or republicans
starting_reps <- df |> filter(init_pid7 %in% c(5:7))
starting_dems <- df |> filter(init_pid7 %in% c(1:3))

# Run all of the models. Every model is two way fixed effects with HC1 standard errors

# models where data is all respondents
percpolar.model <- feglm(ind_B ~ percpolar_scaled | wave + ID, family = binomial(), vcov = "HC1", data = df)
civildiscourse.model <- feglm(ind_B ~ civildiscourse_scaled | wave + ID, family = binomial(), vcov = "HC1",  data = df)
party_favorability.model <- feglm(ind_B ~ init_partyhigher_scaled | wave + ID, family = binomial(), vcov = "HC1", data = df)
party_unfavorability.model <- feglm(ind_B ~ init_partylower_scaled | wave + ID, family = binomial(), vcov = "HC1", data = df)
in_party_candidate.model <- feglm(ind_B ~ in_party_candidate_scaled | wave + ID, family = binomial(), vcov = "HC1", data = df)
out_party_candidate.model <- feglm(ind_B ~ out_party_candidate_scaled | wave + ID, family = binomial(), vcov = "HC1", data = df)
in_party_policy.model <- feglm(ind_B ~ in_party_disagreement_scaled | wave + ID, family = binomial(), vcov = "HC1", data = df)
out_party_policy.model <- feglm(ind_B ~ out_party_disagreement_scaled | wave + ID, family = binomial(), vcov = "HC1", data = df)
selfcensor.model <- feglm(ind_B ~ censorship_scaled | wave + ID, family = binomial(), vcov = "HC1", data = df)

# models where data is respondents who entered the panel as democrats
percpolar.dems <- feglm(ind_B ~ percpolar_scaled | wave + ID, family = binomial(), vcov = "HC1", data = starting_dems)
civildiscourse.dems <- feglm(ind_B ~ civildiscourse_scaled | wave + ID, family = binomial(), vcov = "HC1",  data = starting_dems)
party_favorability.dems <- feglm(ind_B ~ init_partyhigher_scaled | wave + ID, family = binomial(), vcov = "HC1", data = starting_dems)
party_unfavorability.dems <- feglm(ind_B ~ init_partylower_scaled | wave + ID, family = binomial(), vcov = "HC1", data = starting_dems)
in_party_candidate.dems <- feglm(ind_B ~ in_party_candidate_scaled | wave + ID, family = binomial(), vcov = "HC1", data = starting_dems)
out_party_candidate.dems <- feglm(ind_B ~ out_party_candidate_scaled | wave + ID, family = binomial(), vcov = "HC1", data = starting_dems)
in_party_policy.dems <- feglm(ind_B ~ in_party_disagreement_scaled | wave + ID, family = binomial(), vcov = "HC1", data = starting_dems)
out_party_policy.dems <- feglm(ind_B ~ out_party_disagreement_scaled | wave + ID, family = binomial(), vcov = "HC1", data = starting_dems)
selfcensor.dems <- feglm(ind_B ~ censorship_scaled | wave + ID, family = binomial(), vcov = "HC1", data = starting_dems)

# models where data is respondents who entered the panel as republicans
percpolar.reps <- feglm(ind_B ~ percpolar_scaled | wave + ID, family = binomial(), vcov = "HC1", data = starting_reps)
civildiscourse.reps <- feglm(ind_B ~ civildiscourse_scaled | wave + ID, family = binomial(), vcov = "HC1",  data = starting_reps)
party_favorability.reps <- feglm(ind_B ~ init_partyhigher_scaled | wave + ID, family = binomial(), vcov = "HC1", data = starting_reps)
party_unfavorability.reps <- feglm(ind_B ~ init_partylower_scaled | wave + ID, family = binomial(), vcov = "HC1", data = starting_reps)
in_party_candidate.reps <- feglm(ind_B ~ in_party_candidate_scaled | wave + ID, family = binomial(), vcov = "HC1", data = starting_reps)
out_party_candidate.reps <- feglm(ind_B ~ out_party_candidate_scaled | wave + ID, family = binomial(), vcov = "HC1", data = starting_reps)
in_party_policy.reps <- feglm(ind_B ~ in_party_disagreement_scaled | wave + ID, family = binomial(), vcov = "HC1", data = starting_reps)
out_party_policy.reps <- feglm(ind_B ~ out_party_disagreement_scaled | wave + ID, family = binomial(), vcov = "HC1", data = starting_reps)
selfcensor.reps <- feglm(ind_B ~ censorship_scaled | wave + ID, family = binomial(), vcov = "HC1", data = starting_reps)

# Convert all those glm models into odds ratios for easier visualization
# and combine into a big dataframe
odds_df <- generate_odds_ci(percpolar.model)
odds_df <- rbind(odds_df, generate_odds_ci(civildiscourse.model))
odds_df <- rbind(odds_df, generate_odds_ci(party_favorability.model))
odds_df <- rbind(odds_df, generate_odds_ci(party_unfavorability.model))
odds_df <- rbind(odds_df, generate_odds_ci(in_party_candidate.model))
odds_df <- rbind(odds_df, generate_odds_ci(out_party_candidate.model))
odds_df <- rbind(odds_df, generate_odds_ci(in_party_policy.model))
odds_df <- rbind(odds_df, generate_odds_ci(out_party_policy.model))
odds_df <- rbind(odds_df, generate_odds_ci(selfcensor.model))
odds_df$respondents = "All"

dems_df <- generate_odds_ci(percpolar.dems)
dems_df <- rbind(dems_df, generate_odds_ci(civildiscourse.dems))
dems_df <- rbind(dems_df, generate_odds_ci(party_favorability.dems))
dems_df <- rbind(dems_df, generate_odds_ci(party_unfavorability.dems))
dems_df <- rbind(dems_df, generate_odds_ci(in_party_candidate.dems))
dems_df <- rbind(dems_df, generate_odds_ci(out_party_candidate.dems))
dems_df <- rbind(dems_df, generate_odds_ci(in_party_policy.dems))
dems_df <- rbind(dems_df, generate_odds_ci(out_party_policy.dems))
dems_df <- rbind(dems_df, generate_odds_ci(selfcensor.dems))
dems_df$respondents = "Democrats"

reps_df <- generate_odds_ci(percpolar.reps)
reps_df <- rbind(reps_df, generate_odds_ci(civildiscourse.reps))
reps_df <- rbind(reps_df, generate_odds_ci(party_favorability.reps))
reps_df <- rbind(reps_df, generate_odds_ci(party_unfavorability.reps))
reps_df <- rbind(reps_df, generate_odds_ci(in_party_candidate.reps))
reps_df <- rbind(reps_df, generate_odds_ci(out_party_candidate.reps))
reps_df <- rbind(reps_df, generate_odds_ci(in_party_policy.reps))
reps_df <- rbind(reps_df, generate_odds_ci(out_party_policy.reps))
reps_df <- rbind(reps_df, generate_odds_ci(selfcensor.reps))
reps_df$respondents = "Republicans"

# combine all of the smaller dataframes together
# this is the dataframe that you will use to plat figure 2
combined_df_toplot <- rbind(odds_df, dems_df, reps_df)
combined_df_toplot$respondents = factor(combined_df_toplot$respondents,
                                        labels = c("All", "Democrats", "Republicans"))

#### FIGURE 2 - MAIN TWFE PLOT ####
# plot the giant effect plot
# this is the critical figure for the entire paper
png(filename = "img/giant_effect_plot_v3.png",
    width = 20, height = 20, units = "cm", type = "cairo-png", res = 300)
ggplot(combined_df_toplot, aes(x = est, y = name,
                               xmin = ci.min, xmax = ci.max,
                               color = respondents)) +
  geom_pointrange(position = position_dodge2(width = 0.2, reverse = TRUE)) +
  geom_vline(xintercept = 1, color = "black") +
  labs(x = "Odds ratio of switching to Independent",
       y = "",
       color = "Initial Party",
       caption = wrap_text("Points represent odds ratios for switching party identification to Independent given a one-standard deviation increase in the independent variable of interest. Bars represent 95% confidence intervals. X axis is on log scale.")) +
  scale_x_log10() +
  scale_color_manual(labels = c("All", "Democrats", "Republicans"),
                     values = c("black", "blue", "red")) +
  scale_y_discrete(
    limits = rev(c(
      "censorship_scaled", "percpolar_scaled", "civildiscourse_scaled",
      "init_partyhigher_scaled", "init_partylower_scaled", "in_party_candidate_scaled",
      "out_party_candidate_scaled", "in_party_disagreement_scaled", "out_party_disagreement_scaled")),
    labels = rev(c(
      "Self-Censorship", "Perceived\n Polarization", "Perceived\n Incivility",
      "In-Party\n Favorability", "Out-Party\n Favorability",
      "In-Party Candidate\n Favorability", "Out-Party Candidate\n Favorability",
      "In-Party Policy\n Disagreement", "Out-Party Policy\n Disagreement"))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_textbox_simple(hjust = 0,
                                              width = grid::unit(16, "cm"),
                                              size = 9,
                                              margin = margin(5,0,0,0)))
dev.off()


#### TABLE 1 ####

# Data quality was calculated by NORC and provided,
# so no R code was used to generate this table
# This section exists as a placeholder to indicate that the table is part of the paper

#### FIGURE 3 - ALLUVIAL PLOT ####
png(filename = "img/alluvial_change.png",
    width = 20, height = 12, units = "cm", type = "cairo-png", res = 300)
ggplot(df[df$pid3_B %in% c("D", "I", "R"),], # exclude NAs
       aes(x = wave, stratum = pid3_B, alluvium = ID,
           fill = pid3_B, label = pid3_B)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  scale_fill_manual(
    values = c("blue", "forestgreen", "red"),
    labels = c("Democrat", "Independent", "Republican")) +
  scale_x_continuous(breaks = c(1:8)) +
  # geom_text(stat = "stratum") +
  labs(x = "Wave", y = "Number of Respondents") +
  theme_minimal() +
  expand_limits(x = c(1, 9)) +
  annotate("text", x = 8.2, y=3500, label= "Democrat", hjust = 0) +
  annotate("text", x = 8.2, y=2000, label = "Independent", hjust = 0) +
  annotate("text", x = 8.2, y=550, label = "Republican", hjust = 0) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))
dev.off()

#### FIGURE 4 - CORRELATION PLOT FOR VARIABLES ####
# self-censorship
selfcensor_vars <- df |> select(selfcensor1, selfcensor2, selfcensor3, selfcensor4) |>
  rename(`Self-Censorship 1` = selfcensor1,
         `Self-Censorship 2` = selfcensor2,
         `Self-Censorship 3` = selfcensor3,
         `Self-Censorship 4` = selfcensor4)
selfcensor_corr <- cor(selfcensor_vars, use = "pairwise.complete.obs")

# perceived polarization
percpolar_vars <- df |> select(apolar1, apolar2, apolar3, apolar4) |>
  rename(`Perceived Polarization 1` = apolar1,
         `Perceived Polarization 2` = apolar2,
         `Perceived Polarization 3 (reversed)` = apolar3,
         `Perceived Polarization 4 (reversed)` = apolar4)
percpolar_corr <- cor(percpolar_vars, use = "pairwise.complete.obs")

# incivility
uncivil_vars <- df |> select(civil1, civil2, civil3, civil4) |>
  rename(`Incivility 1` = civil1,
         `Incivility 2 (reversed)` = civil2,
         `Incivility 3` = civil3,
         `Incivility 4 (reversed)` = civil4)
uncivil_corr <- cor(uncivil_vars, use = "pairwise.complete.obs")

# make plots
selfcensor_corrplot <-
  ggcorrplot(selfcensor_corr, type = "full",
             outline.col = "white",
             ggtheme = theme_bw,
             lab = TRUE,
             colors = c("#6D9EC1", "white", "#E46726"))

percpolar_corrplot <-
  ggcorrplot(percpolar_corr, type = "full",
             outline.col = "white",
             ggtheme = theme_bw,
             lab = TRUE,
             colors = c("#6D9EC1", "white", "#E46726"))

civility_corrplot <-
  ggcorrplot(uncivil_corr, type = "full",
             outline.col = "white",
             ggtheme = theme_bw,
             lab = TRUE,
             colors = c("#6D9EC1", "white", "#E46726"))

# stack our three plots on top of each other
stacked_corrs <- selfcensor_corrplot +
  percpolar_corrplot +
  civility_corrplot +
  plot_layout(ncol = 2)

png(filename = "img/stacked_corrs.png",
    width = 30, height = 30, units = "cm", type = "cairo-png", res = 300)
stacked_corrs
dev.off()

#### FIGURE 5 - WITHIN-SUBJECT RANGES ####

cols <- c("censorship_scaled", "percpolar_scaled",
          "civildiscourse_scaled", "init_partyhigher_scaled",
          "init_partylower_scaled", "in_party_candidate_scaled",
          "out_party_candidate_scaled", "in_party_disagreement_scaled",
          "out_party_disagreement_scaled")
ranges <- df |>
  group_by(ID) |>
  summarise(across(all_of(cols),
                   list(range = my.range))) |>
  select(ID, censorship_scaled_range, percpolar_scaled_range, civildiscourse_scaled_range,
         init_partyhigher_scaled_range, init_partylower_scaled_range,
         in_party_candidate_scaled_range, out_party_candidate_scaled_range,
         in_party_disagreement_scaled_range, out_party_disagreement_scaled_range)

# convert ranges for plotting
ranges.toplot <- ranges |>
  pivot_longer(!ID, names_to = "var_name", values_to = "range")

ranges.toplot$var_name <-
  factor(ranges.toplot$var_name,
         levels = c("censorship_scaled_range", "percpolar_scaled_range",
                    "civildiscourse_scaled_range", "init_partyhigher_scaled_range",
                    "init_partylower_scaled_range", "in_party_candidate_scaled_range",
                    "out_party_candidate_scaled_range", "in_party_disagreement_scaled_range",
                    "out_party_disagreement_scaled_range"),
         labels = c("Self-Censorship", "Perceived Polarization", "Perceived Incivility",
                    "In-Party Favorability", "Out-Party Favorability",
                    "In-Party Candidate Favorability", "Out-Party Candidate Favorability",
                    "In-Party Policy Disagreement", "Out-Party Policy Disagreement")
  )

# get stats for all ranges
range_stats <-
  ranges.toplot |>
  group_by(var_name) |>
  summarise(Mean = mean(range, na.rm = T),
            Counterfactual = 1) |>
  gather(key = Key, value = value, Mean:Counterfactual)

# plot histogram of all ranges
png(filename = "img/within_plausible_3.png",
    width = 20, height = 14, units = "cm", type = "cairo-png", res = 300)
ggplot(ranges.toplot, aes(range)) +
  geom_histogram(bins = 25) +
  labs(x = "Within-subject Ranges, measured in Between-subject Standard Deviations",
       y = "Frequency") +
  facet_wrap(~var_name, ncol = 3) +
  geom_vline(data = range_stats, aes(xintercept = value, color = Key, linetype = Key)) +
  scale_color_manual(values = c("black", "red")) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  theme_bw() +
  theme(legend.position="bottom", legend.title=element_blank())
dev.off()

#### FIGURE 6 - POLITICAL INTEREST INTERACTIONS ####

# run all the models - for all models show the INTERACTION of political interest
# with the independent variable of interest
percpolar.interest <- feglm(ind_B ~ percpolar_scaled*init_interest | wave + ID, family = binomial(), vcov = "HC1", data = df)
civildiscourse.interest <- feglm(ind_B ~ civildiscourse_scaled*init_interest | wave + ID, family = binomial(), vcov = "HC1",  data = df)
party_favorability.interest <- feglm(ind_B ~ init_partyhigher_scaled*init_interest | wave + ID, family = binomial(), vcov = "HC1", data = df)
party_unfavorability.interest <- feglm(ind_B ~ init_partylower_scaled*init_interest | wave + ID, family = binomial(), vcov = "HC1", data = df)
in_party_candidate.interest <- feglm(ind_B ~ in_party_candidate_scaled*init_interest | wave + ID, family = binomial(), vcov = "HC1", data = df)
out_party_candidate.interest <- feglm(ind_B ~ out_party_candidate_scaled*init_interest | wave + ID, family = binomial(), vcov = "HC1", data = df)
in_party_policy.interest <- feglm(ind_B ~ in_party_disagreement_scaled*init_interest | wave + ID, family = binomial(), vcov = "HC1", data = df)
out_party_policy.interest <- feglm(ind_B ~ out_party_disagreement_scaled*init_interest | wave + ID, family = binomial(), vcov = "HC1", data = df)
selfcensor.interest <- feglm(ind_B ~ censorship_scaled*init_interest | wave + ID, family = binomial(), vcov = "HC1", data = df)

# build our giant interaction plot for political interest
interact_df <- get_interaction(percpolar.interest)
interact_df <- rbind(interact_df, get_interaction(civildiscourse.interest))
interact_df <- rbind(interact_df, get_interaction(party_favorability.interest))
interact_df <- rbind(interact_df, get_interaction(party_unfavorability.interest))
interact_df <- rbind(interact_df, get_interaction(in_party_candidate.interest))
interact_df <- rbind(interact_df, get_interaction(out_party_candidate.interest))
interact_df <- rbind(interact_df, get_interaction(in_party_policy.interest))
interact_df <- rbind(interact_df, get_interaction(out_party_policy.interest))
interact_df <- rbind(interact_df, get_interaction(selfcensor.interest))

# interaction plot
png(filename = "img/interaction_plot_appendix.png",
    width = 20, height = 20, units = "cm", type = "cairo-png", res = 300)
ggplot(interact_df, aes(x = est, y = name,
                        xmin = ci.min, xmax = ci.max)) +
  geom_pointrange(position = position_dodge2(width = 0.2, reverse = TRUE)) +
  geom_vline(xintercept = 0, color = "black") +
  labs(x = "Coefficient for Interaction between Political Interest and Independent Variable",
       y = "",
       caption = wrap_text("Points represent estimates for the interactions between political interest and the independent variables for switching party identification to Independent. Bars represent 95% confidence intervals for those interactions.")) +
  scale_y_discrete(
    limits = rev(c(
      "censorship_scaled", "percpolar_scaled", "civildiscourse_scaled",
      "init_partyhigher_scaled", "init_partylower_scaled", "in_party_candidate_scaled",
      "out_party_candidate_scaled", "in_party_disagreement_scaled", "out_party_disagreement_scaled")),
    labels = rev(c(
      "Self-Censorship", "Perceived\n Polarization", "Perceived\n Incivility",
      "In-Party\n Favorability", "Out-Party\n Favorability",
      "In-Party Candidate\n Favorability", "Out-Party Candidate\n Favorability",
      "In-Party Policy\n Disagreement", "Out-Party Policy\n Disagreement"))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_textbox_simple(hjust = 0,
                                              width = grid::unit(16, "cm"),
                                              size = 9,
                                              margin = margin(5,0,0,0)))
dev.off()

#### FIGURE 7 - SELF_CENSORSHIP INTERACTIONS ####

# run all of our self-censorship models
percpolar.selfcensor <- feglm(ind_B ~ percpolar_scaled*init_selfcensor | wave + ID, family = binomial(), vcov = "HC1", data = df)
civildiscourse.selfcensor <- feglm(ind_B ~ civildiscourse_scaled*init_selfcensor | wave + ID, family = binomial(), vcov = "HC1",  data = df)
party_favorability.selfcensor <- feglm(ind_B ~ init_partyhigher_scaled*init_selfcensor | wave + ID, family = binomial(), vcov = "HC1", data = df)
party_unfavorability.selfcensor <- feglm(ind_B ~ init_partylower_scaled*init_selfcensor | wave + ID, family = binomial(), vcov = "HC1", data = df)
in_party_candidate.selfcensor <- feglm(ind_B ~ in_party_candidate_scaled*init_selfcensor | wave + ID, family = binomial(), vcov = "HC1", data = df)
out_party_candidate.selfcensor <- feglm(ind_B ~ out_party_candidate_scaled*init_selfcensor | wave + ID, family = binomial(), vcov = "HC1", data = df)
in_party_policy.selfcensor <- feglm(ind_B ~ in_party_disagreement_scaled*init_selfcensor | wave + ID, family = binomial(), vcov = "HC1", data = df)
out_party_policy.selfcensor <- feglm(ind_B ~ out_party_disagreement_scaled*init_selfcensor | wave + ID, family = binomial(), vcov = "HC1", data = df)

# build our interaction plot for self censorship
selfcensor_df <- get_interaction(percpolar.selfcensor)
selfcensor_df <- rbind(selfcensor_df, get_interaction(civildiscourse.selfcensor))
selfcensor_df <- rbind(selfcensor_df, get_interaction(party_favorability.selfcensor))
selfcensor_df <- rbind(selfcensor_df, get_interaction(party_unfavorability.selfcensor))
selfcensor_df <- rbind(selfcensor_df, get_interaction(in_party_candidate.selfcensor))
selfcensor_df <- rbind(selfcensor_df, get_interaction(out_party_candidate.selfcensor))
selfcensor_df <- rbind(selfcensor_df, get_interaction(in_party_policy.selfcensor))
selfcensor_df <- rbind(selfcensor_df, get_interaction(out_party_policy.selfcensor))

# interaction part for SELF-CENSORSHIP
png(filename = "img/selfcensor_plot_appendix.png",
    width = 20, height = 20, units = "cm", type = "cairo-png", res = 300)
ggplot(selfcensor_df, aes(x = est, y = name,
                          xmin = ci.min, xmax = ci.max)) +
  geom_pointrange(position = position_dodge2(width = 0.2, reverse = TRUE)) +
  geom_vline(xintercept = 0, color = "black") +
  labs(x = "Coefficient for Interaction between Self-Censorship and Independent Variable",
       y = "",
       caption = wrap_text("Points represent estimates for the interactions between self-censorship and the independent variables for switching party identification to Independent. Bars represent 95% confidence intervals for those interactions.")) +
  scale_y_discrete(
    limits = rev(c(
      "percpolar_scaled", "civildiscourse_scaled",
      "init_partyhigher_scaled", "init_partylower_scaled", "in_party_candidate_scaled",
      "out_party_candidate_scaled", "in_party_disagreement_scaled", "out_party_disagreement_scaled")),
    labels = rev(c(
      "Perceived\n Polarization", "Perceived\n Incivility",
      "In-Party\n Favorability", "Out-Party\n Favorability",
      "In-Party Candidate\n Favorability", "Out-Party Candidate\n Favorability",
      "In-Party Policy\n Disagreement", "Out-Party Policy\n Disagreement"))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_textbox_simple(hjust = 0,
                                              width = grid::unit(16, "cm"),
                                              size = 9,
                                              margin = margin(5,0,0,0)))
dev.off()

#### TABLE 2 - INDIVIDUAL SELFCENSOR ITEMS ####
# robustness check - test the self-censorship items INDIVIDUALLY
selfcensor.mod1 <- feglm(ind_B ~ selfcensor1 | wave + ID, family = binomial(), vcov = "HC1", data = df)
selfcensor.mod2 <- feglm(ind_B ~ selfcensor2 | wave + ID, family = binomial(), vcov = "HC1", data = df)
selfcensor.mod3 <- feglm(ind_B ~ selfcensor3 | wave + ID, family = binomial(), vcov = "HC1", data = df)
selfcensor.mod4 <- feglm(ind_B ~ selfcensor4 | wave + ID, family = binomial(), vcov = "HC1", data = df)

# create table of these self-censorship checks.
modelsummary(models = list(
  "Item 1" = selfcensor.mod1, "Item 2" = selfcensor.mod2,
  "Item 3" = selfcensor.mod3, "Item 4" = selfcensor.mod4),
  estimate = "{estimate} ({std.error}) {stars}",
  statistic = NULL,
  stars = c("*" = .05, "**" = .01, "***" = .001),
  output = "latex")