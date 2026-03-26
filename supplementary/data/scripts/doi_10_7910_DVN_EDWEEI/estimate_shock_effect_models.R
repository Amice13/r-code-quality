# Alexander F. Gazmararian
# agazmararian@gmail.com

library(fixest)
library(here)
library(lmtest)
library(marginaleffects)
library(modelsummary)
library(sandwich)
library(tidyverse)
library(tinytable)
library(sf)
library(lme4)
library(interflex)
library(sensemakr)
library(CBPS)
library(MatchIt)
library(cobalt)
library(parameters)
library(bayestestR)
library(scales)
library(metafor)
library(esci)
library(viridis)

source("Code/fun/desc_stat_fun.R")
source("Code/19_wrp/fun/est_models.R")
source("Code/19_wrp/fun/est_placebo.R") # Called by estimate_and_prep_placebo_df.R
source("Code/19_wrp/fun/extract_results.R")
source("Code/19_wrp/fun/plot_model_estimates.R")
source("Code/19_wrp/fun/estimate_and_prep_placebo_df.R")
source("Code/19_wrp/fun/plot_het_effects.R")
source("Code/19_wrp/fun/est_sensitivity.R")
source("Code/19_wrp/fun/est_equiv.R") # Called by est_and_plot_equiv.R
source("Code/19_wrp/fun/est_and_plot_equiv.R")

set.seed(10)

g <- readRDS(here("Data", "output", "19_wrp", "19_wrp_analysis.rds"))

# Check all covariates

cat("\n")
cat("================================================================================\n")
cat("                           OUTCOME MEAN (p. 24)                                \n")
cat("================================================================================\n")
print(summary(g$salient))
cat("================================================================================\n")
cat("\n")

shocks <- c("best_tanom_7d_z", "best_tanom_2sd_7d_z", "modis_burnanomp_mu_6m_w1_z", "noaa_cpc_tdev_7d_z")

# Quantiles to be used for treatment effect heterogeneity
dem_qtl <- quantile(unique(subset(g, select = c(country, v2x_polyarchy)))$v2x_polyarchy, c(.2, .4, .6, .8), na.rm = TRUE)
dem_mean <- g %>%
  dplyr::select(country, v2x_polyarchy) %>%
  summarize(mean = mean(v2x_polyarchy), sd = sd(v2x_polyarchy))
dem_std <- c(dem_mean$mean, dem_mean$sd + dem_mean$mean)

crop_qtl <- quantile(unique(subset(g, select = c(cty_reg, crop)))$crop, c(.2, .4, .6, .8), na.rm = TRUE)
crop_mean <- g %>%
  dplyr::select(cty_reg, crop) %>%
  summarize(mean = mean(crop), sd = sd(crop))
crop_std <- c(crop_mean$mean, crop_mean$sd + crop_mean$mean)

# Figure specifications
lwidth <- 1.5
ptsize <- 1
fwidth <- 6.5
fdpi <- 300
fscale <- 1.5

# Model specification
hetvars <- "(riskund + v2x_polyarchy + edu + crop + rural + age + age2)"
covars <- paste0(
  "+ female + incfeel + hhsize + internet + kids_bin + riskaverse +",
  "gdp_log + pop_log + co2_log + oil + coal + urban_reg + ",
  "gdppc_nat_log + edu_sec + pop_nat_log + co2_nat_log + ag_share"
)
outcome <- "salient"
fe <- "globalreg"

# Plot labels
lab_best_7d <- "7 day temperature anomalies"
lab_fire <- "6 month fire burn area"
lab_best_2sd <- "+2SD heat days"
lab_noaa_7d <- "7 day temperature anomalies (recent baseline)"
lab_sum <- "Summary"

# Covariate names and table setup ----
gm <- modelsummary::gof_map
gm$omit <- TRUE
gm[gm$raw == "adj.r.squared", ]$clean <- "Adjusted $R^2$"
gm[gm$raw == "adj.r.squared", ]$omit <- FALSE
gm[gm$raw == "nobs", ]$clean <- "$N$"
gm[gm$raw == "nobs", ]$omit <- FALSE
gm[gm$raw == "r2.marginal", ]$omit <- FALSE

varmap <- c(
  "reg_loser_50" = "Severe damage",
  "best_tanom_7d_z" = "Climate shock",
  "best_tanom_2sd_7d_z" = "Climate shock",
  "modis_burnanomp_mu_6m_w1_z" = "Climate shock",
  "noaa_cpc_tdev_7d_z" = "Climate shock",
  
  "v2x_polyarchy" = "Polyarchy",
  "best_tanom_7d_z:v2x_polyarchy" = "Climate shock x Polyarchy",
  "best_tanom_2sd_7d_z:v2x_polyarchy" = "Climate shock x Polyarchy",
  "modis_burnanomp_mu_6m_w1_z:v2x_polyarchy" = "Climate shock x Polyarchy",
  "noaa_cpc_tdev_7d_z:v2x_polyarchy" = "Climate shock x Polyarchy",

  "reg_loser_50:best_tanom_7d_z" = "Severe damage x Climate shock",
  "reg_loser_50:best_tanom_2sd_7d_z" = "Severe damage x Climate shock",
  "reg_loser_50:modis_burnanomp_mu_6m_w1_z" = "Severe damage x Climate shock",
  "reg_loser_50:noaa_cpc_tdev_7d_z" = "Severe damage x Climate shock",
  
  "reg_loser_50:v2x_polyarchy" = "Severe damage x Polyarchy",
  "reg_loser_50:best_tanom_7d_z:v2x_polyarchy" = "Severe damage x Climate shock x Polyarchy",
  "reg_loser_50:best_tanom_2sd_7d_z:v2x_polyarchy" = "Severe damage x Climate shock x Polyarchy",
  "reg_loser_50:modis_burnanomp_mu_6m_w1_z:v2x_polyarchy" = "Severe damage x Climate shock x Polyarchy",
  "reg_loser_50:noaa_cpc_tdev_7d_z:v2x_polyarchy" = "Severe damage x Climate shock x Polyarchy"
)
  
# Distribution of People by Birth Year----
sum(g$age_year <= 1980) / nrow(g)
median(g$age_year)
sum(g$age_year >= 1981 & g$age_year <= 2010) / nrow(g)
sum(g$age_year >= 1971 & g$age_year <= 2000) / nrow(g)
sum(g$age_year >= 1961 & g$age_year <= 1990) / nrow(g)

# Climate Shock Effect Moderated by Vulnerability ----
message("Estimating models for climate shock effect moderated by vulnerability...")
# Top and major daily risk
top_major <- lapply(shocks, function(x) {
  est_models(x, 
            outcome = "salient", 
            hetvars = hetvars, 
            covars = covars)
})

## Create book figure (not included in replication package - for book manuscript only)
# source("~/Dropbox/Research/cleavages/Code/individual/19_wrp/02_analysis/book_fig_3.3_shock_effect.R")
# create_book_fig_3.3_shock_effect(top_major)

## AJPS Fig. 3----
N_ame <- formatC(nrow(g), format = "f", big.mark = ",", digits = 0)
district_ame <- formatC(n_distinct(g$cty_reg), format = "f", big.mark = ",", digits = 0)
countries_ame <- formatC(n_distinct(g$country), format = "f", big.mark = ",", digits = 0)

p_ame <- plot_model_estimates(top_major, N = N_ame, zones = district_ame, countries = countries_ame)
ggsave(p_ame, filename = here("Output", "figures", "fig_3_ame_wrp.pdf"),
       width = fwidth, height = 4.5, scale = fscale)
message("Created Fig. 3. Located at Output/figures/fig_3_ame_wrp.pdf")

model_out <- extract_results(nested_list = top_major)
tab_out <- model_out$first

betas_dam <- subset(tab_out, reg_loser_50 == 1)$estimate
se_dam <- subset(tab_out, reg_loser_50 == 1)$std.error
meta_dam <- rma.uni(yi = betas_dam, sei = se_dam)
cat("\n")
cat("================================================================================\n")
cat("                         Average climate shock effect (p. 5; p. 23)               \n")
cat("================================================================================\n")
cat("Note: Multiply probability by 100 to convert to percentage points.\n")
cat("\n")
cat("...Facing damages...")
print(meta_dam)
cat("================================================================================\n")
cat("\n")

cat("...Facing limited damages...")
betas_ben <- subset(tab_out, reg_loser_50 == 0)$estimate
se_ben <- subset(tab_out, reg_loser_50 == 0)$std.error
meta_ben <- rma.uni(yi = betas_ben, sei = se_ben)
print(meta_ben)
cat("================================================================================\n")
cat("\n")

betas_h <- model_out$second$estimate
se_h <- model_out$second$std.error
meta_h <- rma.uni(yi = betas_h, sei = se_h)

# Placebo Tests ----
placebos <- c(
  "placebo_work2",
  "placebo_politics2",
  "placebo_powerline_worry",
  "placebo_appliance_worry",
  "placebo_ai",
  "placebo_transport2",
  "placebo_house2"
)

lab_work <- "Work-related accidents, such as physical injuries, are a top or major daily risk."
lab_politics <- "Politics, the political situation, or corruption are a top or major daily risk."
lab_transport <- "Transportation-related accidents/injuries, such as subway, train, plane, are a top or major daily risk."
lab_house <- "Cooking or other household accidents/injuries are a top or major daily risk."
lab_powerline_worry <- "Very worried that electrical power lines could cause serious personal harm."
lab_ai <- "Artificial intelligence will mostly help country in the next 20 years."
lab_appliance_worry <- "Very worried that household appliances, such as a washing machine, dryer, or refrigerator, could cause serious personal harm."

placebo_result <- lapply(placebos, estimate_and_prep_placebo_df)
placebo_df <- do.call(rbind, placebo_result)

placebo_df <- placebo_df %>%
  dplyr::mutate(
    placebo_lab = case_when(
      placebo == "placebo_work2" ~ lab_work,
      placebo == "placebo_politics2" ~ lab_politics,
      placebo == "placebo_transport2" ~ lab_transport,
      placebo == "placebo_house2" ~ lab_house,
      placebo == "placebo_powerline_worry" ~ lab_powerline_worry,
      placebo == "placebo_ai" ~ lab_ai,
      placebo == "placebo_appliance_worry" ~ lab_appliance_worry
    )
  )

## AJPS Fig. 4----
# Get average effect size
betas_dam <- subset(placebo_df, reg_loser_50 == 1)$estimate
se_dam <- subset(placebo_df, reg_loser_50 == 1)$std.error
meta_dam <- rma.uni(yi = betas_dam, sei = se_dam)

betas_ben <- subset(placebo_df, reg_loser_50 == 0)$estimate
se_ben <- subset(placebo_df, reg_loser_50 == 0)$std.error
meta_ben <- rma.uni(yi = betas_ben, sei = se_ben)

meta_df <- data.frame(
  term = "Summary",
  estimate = c(meta_dam$beta, meta_ben$beta),
  conf.low = c(meta_dam$ci.lb, meta_ben$ci.lb),
  conf.high = c(meta_dam$ci.ub, meta_ben$ci.ub),
  reg_loser_50 = c("(b) Severe damage", "(a) Limited damage"),
  model = "Summary",
  placebo_lab = lab_sum
)

placebo_df$placebo_lab <- factor(
  placebo_df$placebo_lab,
  c(
    lab_sum,
    lab_work,
    lab_politics,
    lab_transport,
    lab_house,
    lab_powerline_worry,
    lab_ai,
    lab_appliance_worry
  )
)

placebo_df <- placebo_df %>%
  dplyr::mutate(reg_loser_50 = ifelse(reg_loser_50 == 1, "(b) Severe damage", "(a) Limited damage")) %>%
  dplyr::mutate(
    term = case_when(
      term == "best_tanom_7d_z" ~ lab_best_7d,
      term == "best_tanom_2sd_7d_z" ~ lab_best_2sd,
      term == "modis_burnanomp_mu_6m_w1_z" ~ lab_fire,
      term == "noaa_cpc_tdev_7d_z" ~ "7 day temp. anomalies (recent baseline)",
      term == "Summary" ~ lab_sum
    )
  )

p_placebo <- placebo_df %>%
  ggplot(aes(
    x = estimate, y = placebo_lab, xmin = conf.low, xmax = conf.high,
    color = term, group = term
  )) +
  geom_vline(xintercept = 0, color = "grey") +
  geom_pointrange(position = position_dodge(.7), linewidth = lwidth, size = ptsize * .75) +
  geom_meta_diamond_h(
    data = meta_df,
    height = .15,
    color = "black",
    fill = "black",
    size = 1
  ) +
  scale_y_discrete(labels = label_wrap_gen(55)) +
  scale_x_continuous(limits = c(-0.03, 0.03)) +
  facet_wrap(~reg_loser_50, ncol = 2) +
  theme_classic(base_size = 14) +
  scale_color_manual(values = c(viridis(4), "black")) +
  labs(
    x = "Average effect of +1SD climate shock on placebo outcome",
    subtitle = NULL, color = "", shape = "", y = NULL,
    caption = str_wrap(
      paste(
        "Notes: Bars denote 95% confidence intervals with robust standard errors clustered by administrative zone.",
        "Black diamonds report meta-analysis across the studies with the horizontal bar width corresponding with the same confidence threshold.",
        N_ame, "respondents across", district_ame, "zones in 137 countries.",
        sep = " "
      ),
      138
    )
  ) +
  theme(
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 14, margin = margin(b = 15)),
    axis.text = element_text(color = "black"),
    axis.title.x = element_text(margin = margin(t = 15)),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.justification = "center",
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = .15, margin = margin(b = -20))
  ) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    shape = guide_legend(nrow = 2, byrow = TRUE)
  )

ggsave(p_placebo, filename = here("Output", "figures", "fig_4_placebo_wrp.pdf"),
       width = fwidth, height = 5.25, scale = fscale)
message("Created Fig. 4. Located at Output/figures/fig_4_placebo_wrp.pdf")

# Book placebo plot (not included in replication package - for book manuscript only)
# source("~/Dropbox/Research/cleavages/Code/individual/19_wrp/02_analysis/book_fig_3.4_placebos.R")
# create_book_fig_3.4_placebos(placebo_df = placebo_df)

# Democracy's Moderating Effect ----
# Estimate model for heterogeneous effects analyses
m_het <- lapply(
  shocks,
  function(x) {
    feols(
      fml = as.formula(paste0("salient ~ reg_loser_50 * (", x, "*", hetvars, covars, ")", "|", fe)),
      data = g,
      vcov = ~cty_reg
    )
  }
)

## AJPS Fig. 5----
dem_ame_ajps <- mapply(
  FUN = function(x, shockvar) {
    avg_slopes(
      x,
      variables = shockvar,
      by = c("reg_loser_50", "v2x_polyarchy"),
      newdata = datagrid(reg_loser_50 = 0:1, v2x_polyarchy = dem_qtl)
    )
  },
  x = m_het,
  shockvar = shocks
)
plot_het_effects(
  dem_ame_ajps, "regime type", "dem", ylb = -.02, yub = 0.02,
  newdata = FALSE,
  fig_label = "5",
  figheight = 5,
  N = N_ame, zones = district_ame, countries = countries_ame,
  moderator_note = "Q1 corresponds with least democratic, while Q4 denotes most democratic using the polyarchy index."
  )

## Book plot (not included in replication package - for book manuscript only)
# source("~/Dropbox/Research/cleavages/Code/individual/19_wrp/02_analysis/book_fig_3.5_dem_moderating.R")
# create_book_fig_3.5_dem_moderating(dem.ame = dem_ame_ajps, dem_qtl = dem_qtl)

## Hypothesis Tests----
# Calculate hypotheses tests for effect modification relative to mean
dem_h <- mapply(
  FUN = function(x, shockvar) {
    avg_slopes(
      x,
      variables = shockvar,
      by = c("reg_loser_50", "v2x_polyarchy"),
      newdata = datagrid(reg_loser_50 = 0:1, v2x_polyarchy = dem_std),
      hypothesis = "b4 - b3 = 0"
    )
  },
  x = m_het, shockvar = shocks
)
dem_h.df <- do.call(rbind, data.frame(dem_h)) |> data.frame()
dem_h.df$term <- shocks
dem_h.df <- dem_h.df %>% dplyr::mutate(across(where(is.list), ~ unlist(.x)))
cat("\n")
cat("================================================================================\n")
cat("                         Democracy effect modification (p. 26)                   \n")
cat("================================================================================\n")
print(dem_h.df)
cat("================================================================================\n")
cat("\n")
write.table(dem_h.df, file = here("Output", "tables", "tab_dem_wrp_htests.txt"))


# Appendix: Robustness to Risk Understanding (Fig B6) ----
df_risk <- g %>% filter(riskund == "Danger")
risk <- lapply(shocks, function(x) {
  est_models(x, 
            outcome = "salient", 
            df = df_risk,
            hetvars = hetvars, 
            covars = covars)
})

N.risk <- formatC(nrow(df_risk), format = "f", big.mark = ",", digits = 0)
Z.risk <- formatC(n_distinct(df_risk$cty_reg), format = "f", big.mark = ",", digits = 0)
C.risk <- formatC(n_distinct(df_risk$country), format = "f", big.mark = ",", digits = 0)

p_risk <- plot_model_estimates(risk, N = N.risk, zones = Z.risk, countries = C.risk)
ggsave(p_risk, filename = here("Output", "figures", "fig_B16_ame_wrp_risk.pdf"), width = fwidth, height = 4.5, scale = fscale)
message("Created Fig. 16. Located at Output/figures/fig_B16_ame_wrp_risk.pdf")

# Appendix: Robustness to Alternative Fire Measures (Fig B17) ----

summary(g$modis_burnanomp_mu_6m_w5_z)

fire_shocks <- c("modis_burnanomp_mu_6m_w5_z", "modis_burnanomp_mu_5m_w1_z", "modis_burnanomp_mu_7m_w1_z", "modis_burnanomp_mu_6m_w.5_z")
alt_fire <- lapply(fire_shocks, function(x) {
  est_models(x, 
            outcome = "salient",
            hetvars = hetvars, 
            covars = covars)
})

lab_fire_1 <- "Windsorized 5%"
lab_fire_2 <- "Windsorized 0.5%"
lab_fire_3 <- "5 months"
lab_fire_4 <- "7 months"

ame_fire <- plot_model_estimates(alt_fire, fire = TRUE, xub = .02, N = N_ame, zones = district_ame, countries = countries_ame)

ggsave(ame_fire, filename = here("Output", "figures", "fig_B17_ame_wrp_fire.pdf"), width = fwidth, height = 4.5, scale = fscale)
message("Created Fig. 17. Located at Output/figures/fig_B17_ame_wrp_fire.pdf")

# Appendix: Equivalence Tests (Figs. B1-B4)----
varlist <- c(
  "age", "age2", "female", "kids_bin", "riskaverse", "edu", "rural", "internet", "hhsize", "incfeel", "riskund",
  "gdp_log", "co2_log", "pop_log", "coal", "oil", "crop", "urban_reg",
  "v2x_polyarchy", "gdppc_nat_log", "edu_sec", "pop_nat_log", "co2_nat_log", "ag_share"
)
varmap <- c(
  "age" = "Age",
  "age2" = "Age (Squared)",
  "femalemale"= "Female",
  "kids_bin" = "Children",
  "riskaverse" = "Risk Aversion",
  "eduPrimary" = "Primary Education",
  "eduSecondary" = "Secondary Education",
  "eduTertiary" = "Tertiary Education",
  "rural" = "Rural",
  "internet" = "Internet Access",
  "hhsize10 or more" = "10 or more in household",
  "hhsize5-9" = "5-9 in household",
  "hhsize3-4" = "3-4 in household",
  "incfeelDifficult" = "Difficult to get by on income",
  "incfeelGetting by" = "Getting by on current income",
  "incfeelOther" = "Other feeling about income",
  "incfeelVery difficult" = "Very difficult to get by on income",
  "riskundNeither" = "Risk is neither danger or opportunity",
  "riskundOpportunity" = "Risk is opportunity",
  "riskundOpportunityAndDanger" = "Risk is opportunity and danger",
  "riskundother" = "Other feeling about risk",
  "gdp_log"="Zone GDP (log)",
  "co2_log"="Zone CO2 Emissions (log)",
  "pop_log"="Zone Population (log)",
  "coal"="Coal Development Potential",
  "oil"="Oil Development Potential",
  "crop"="Cropland",
  "urban_reg"="Urbanization",
  "v2x_polyarchy"="Polyarchy",
  "gdppc_nat_log"="National GDP per capita (log)",
  "edu_sec"="National Education",
  "pop_nat_log"="National Population (log)",
  "co2_nat_log"="National CO2 Emissions (log)",
  "ag_share"="Agricultural Share of GDP"
)

fig_label <- c("B1", "B2", "B3", "B4")
tvar <- gsub("_z", "", paste0("treat_", shocks))
wtvar <- gsub("_z", "", paste0("w_treat_", shocks))
args_list <- list(
  shock_in = tvar,
  wt_in = wtvar,
  fig_label = fig_label
)

purrr::pmap(args_list, est_and_plot_equiv)
message("Created Figs. B1-B4. Equivalence tests of covariate balance.")

# Appendix: Sensitivity Analyses (Figs B6-B14) ----

## Temperature shock----
m_sens_temp <- feols(
  fml = as.formula(paste0("salient ~ reg_loser_50 * (", "best_tanom_7d_z", "*", hetvars, covars, ")", "|", fe)),
  data = g,
  vcov = ~ cty_reg
)
# Interaction effect
sens_temp_ame_h <- avg_slopes(m_sens_temp, variables = "best_tanom_7d_z", by = "reg_loser_50", hypothesis = "b2 - b1 = 0") |> data.frame()
# Coefficients and degrees of freedom
ols_temp_cl <- m_sens_temp$coeftable
ols_temp_df <- degrees_freedom(m_sens_temp, "resid")

# Regress interaction on covariates
# Estimate unweighted regression to be conservative; adding weights makes income coefficient less precise
D_X_temp <- feols(
  fml = as.formula(paste0("treat_best_tanom_7d ~ ", hetvars, covars, "|", fe)),
  data = g,
  vcov = ~ cty_reg
)
temp_dof <- degrees_freedom(D_X_temp, "resid")

est_sensitivity(
  estimate_in = sens_temp_ame_h$estimate,
  se_in = sens_temp_ame_h$std.error,
  benchmark_fml = "incfeelVery difficult",
  benchmark_label = "Income",
  senseof = "inter",
  fig_label = "B5",
  D.X = D_X_temp,
  D.X.dof = temp_dof,
  ols_cl = ols_temp_cl,
  ols_df = ols_temp_df
)

est_sensitivity(
  estimate_in = sens_temp_ame_h$estimate,
  se_in = sens_temp_ame_h$std.error,
  benchmark_fml = "hhsize10 or more",
  benchmark_label = "Household Size",
  senseof = "inter",
  fig_label = "B6",
  D.X = D_X_temp,
  D.X.dof = temp_dof,
  ols_cl = ols_temp_cl,
  ols_df = ols_temp_df
)

est_sensitivity(
  estimate_in = sens_temp_ame_h$estimate,
  se_in = sens_temp_ame_h$std.error,
  benchmark_fml = "internet",
  benchmark_label = "Internet Access",
  senseof = "inter",
  fig_label = "B7",
  D.X = D_X_temp,
  D.X.dof = temp_dof,
  ols_cl = ols_temp_cl,
  ols_df = ols_temp_df
)

est_sensitivity(
  estimate_in = sens_temp_ame_h$estimate,
  se_in = sens_temp_ame_h$std.error,
  benchmark_fml = "riskaverse",
  benchmark_label = "Risk",
  senseof = "inter",
  fig_label = "B8",
  D.X = D_X_temp,
  D.X.dof = temp_dof,
  ols_cl = ols_temp_cl,
  ols_df = ols_temp_df
)

est_sensitivity(
  estimate_in = sens_temp_ame_h$estimate,
  se_in = sens_temp_ame_h$std.error,
  benchmark_fml = "pop_nat_log",
  benchmark_label = "National Population",
  senseof = "inter",
  fig_label = "B9",
  D.X = D_X_temp,
  D.X.dof = temp_dof,
  ols_cl = ols_temp_cl,
  ols_df = ols_temp_df
)

message("Created Figs. B5-B9. Sensitivity analyses of temperature shock.")

## Fires----
m_sens_fire <- feols(
  fml = as.formula(paste0("salient ~ reg_loser_50 * (", "modis_burnanomp_mu_6m_w1_z", "*", hetvars, covars, ")", "|", fe)),
  data = g,
  vcov = ~cty_reg
)
# Interaction effect
sens_fire_ame_h <- avg_slopes(m_sens_fire, variables = "modis_burnanomp_mu_6m_w1_z", by = "reg_loser_50", hypothesis = "b2 - b1 = 0") |> 
  data.frame()
# Coefficients and degrees of freedom
ols_fire_cl <- m_sens_fire$coeftable
ols_fire_df <- degrees_freedom(m_sens_fire, "resid")

# Regress interaction on covariates
# Estimate unweighted regression to be conservative; adding weights makes income coefficient less precise
D_X_fire <- feols(
  fml = as.formula(paste0("modis_burnanomp_mu_6m_w1_z ~ ", hetvars, covars, "|", fe)),
  data = g,
  vcov = ~cty_reg
)
fire_dof <- degrees_freedom(D_X_fire, "resid")

est_sensitivity(
  estimate_in = sens_fire_ame_h$estimate,
  se_in = sens_fire_ame_h$std.error,
  benchmark_fml = "incfeelVery difficult",
  benchmark_label = "Income",
  senseof = "inter",
  fig_label = "B10",
  D.X = D_X_fire,
  D.X.dof = fire_dof,
  ols_cl = ols_fire_cl,
  ols_df = ols_fire_df,
  shockvar = "fire"
)

est_sensitivity(
  estimate_in = sens_fire_ame_h$estimate,
  se_in = sens_fire_ame_h$std.error,
  benchmark_fml = "hhsize10 or more",
  benchmark_label = "Household Size",
  senseof = "inter",
  fig_label = "B11",
  D.X = D_X_fire,
  D.X.dof = fire_dof,
  ols_cl = ols_fire_cl,
  ols_df = ols_fire_df,
  shockvar = "fire"
)

est_sensitivity(
  estimate_in = sens_fire_ame_h$estimate,
  se_in = sens_fire_ame_h$std.error,
  benchmark_fml = "internet",
  benchmark_label = "Internet Access",
  senseof = "inter",
  fig_label = "B12",
  D.X = D_X_fire,
  D.X.dof = fire_dof,
  ols_cl = ols_fire_cl,
  ols_df = ols_fire_df,
  shockvar = "fire"
)

est_sensitivity(
  estimate_in = sens_fire_ame_h$estimate,
  se_in = sens_fire_ame_h$std.error,
  benchmark_fml = "riskaverse",
  benchmark_label = "Risk",
  senseof = "inter",
  fig_label = "B13",
  D.X = D_X_fire,
  D.X.dof = fire_dof,
  ols_cl = ols_fire_cl,
  ols_df = ols_fire_df,
  shockvar = "fire"
)

est_sensitivity(
  estimate_in = sens_fire_ame_h$estimate,
  se_in = sens_fire_ame_h$std.error,
  benchmark_fml = "pop_nat_log",
  benchmark_label = "National Population",
  senseof = "inter",
  fig_label = "B14",
  D.X = D_X_fire,
  D.X.dof = fire_dof,
  ols_cl = ols_fire_cl,
  ols_df = ols_fire_df,
  shockvar = "fire"
)

message("Created Figs. B10-B14. Sensitivity analyses of fire shock.")

# Appendix: Treatment Effect Heterogeneity ----

## Crops ----
crop_ame <- mapply(
  FUN = function(x, shockvar) {
    avg_slopes(
      x,
      variables = shockvar,
      by = c("reg_loser_50", "crop"),
      newdata = datagrid(reg_loser_50 = 0:1, crop = crop_qtl)
    )
  },
  x = m_het,
  shockvar = shocks
)
plot_het_effects(ame_in = crop_ame, het_lab = "Cropland", file_name = "crop", fig_label = "B21", newdata = FALSE, N = N_ame, zones = district_ame, countries = countries_ame)
message("Created Fig. 21. Located at Output/figures/fig_B21_wrp_het_crop.pdf")

crop_h <- mapply(
  FUN = function(x, shockvar) {
    avg_slopes(
      x,
      variables = shockvar,
      by = c("reg_loser_50", "crop"),
      newdata = datagrid(reg_loser_50 = 0:1, crop = crop_std),
      hypothesis = "b4 - b3 = 0"
    )
  },
  x = m_het, shockvar = shocks
)
crop_h.df <- do.call(rbind, data.frame(crop_h)) |>
  data.frame() |>
  dplyr::mutate(across(where(is.list), unlist))
crop_h.df$term <- shocks
crop_h.df <- subset(crop_h.df, select = c(term, estimate, p.value))
write.table(crop_h.df, file = here("Output", "tables", "tab_crop_wrp_htests.txt"))

## Education----
edu <- mapply(
  FUN = function(x, shockvar) avg_slopes(x, variables = shockvar, by = c("reg_loser_50", "edu")),
  x = m_het,
  shockvar = shocks
)

plot_het_effects(edu, "Education", "edu", fig_label = "B19", newdata = FALSE, N = N_ame, zones = district_ame, countries = countries_ame)
message("Created Fig. 19. Located at Output/figures/fig_B19_wrp_het_edu.pdf")

edu_h <- mapply(
  FUN = function(x, shockvar) {
    avg_slopes(
      x,
      variables = shockvar,
      by = c("reg_loser_50", "edu"),
      hypothesis = "b8 - b6 = 0"
    )
  },
  x = m_het, shockvar = shocks
)
edu_h.df <- do.call(rbind, data.frame(edu_h)) |>
  data.frame() |>
  dplyr::mutate(across(where(is.list), unlist))
edu_h.df$term <- shocks
edu_h.df <- subset(edu_h.df, select = c(term, estimate, p.value))
write.table(edu_h.df, file = here("Output", "tables", "tab_edu_wrp_htests.txt"))

## Sex ----
hetvars_sex <- c("riskund + v2x_polyarchy + edu + crop + rural + age + age2 + female")

m_het_sex <- lapply(
  shocks,
  function(x) {
    feols(
      fml = as.formula(paste0("salient ~ reg_loser_50 * (", x, "*", hetvars_sex, covars, ")", "|", fe)),
      data = g,
      vcov = ~cty_reg
    )
  }
)

sex <- mapply(
  FUN = function(x, shockvar) avg_slopes(x, variables = shockvar, by = c("reg_loser_50", "female")),
  x = m_het_sex,
  shockvar = shocks
)
plot_het_effects(sex, "Sex", "sex", fig_label = "B22", newdata = FALSE, N = N_ame, zones = district_ame, countries = countries_ame)
message("Created Fig. 22. Located at Output/figures/fig_B22_wrp_het_sex.pdf")

## Age ----
g$age_gen <- factor(g$age_gen, levels = c("Gen Z", "Millennial", "Gen X", "Boomer", "Silent"))

hetvars_age <- "(riskund + v2x_polyarchy + edu + crop + rural + age_gen)"

het_age <- lapply(
  shocks,
  function(x) {
    feols(
      fml = as.formula(paste0("salient ~ reg_loser_50 * (", x, "*", hetvars_age, covars, ")", "|", fe)),
      data = g,
      vcov = ~cty_reg
    )
  }
)
age <- mapply(
  FUN = function(x, shockvar) avg_slopes(x, variables = shockvar, by = c("reg_loser_50", "age_gen")),
  x = het_age,
  shockvar = shocks
)

plot_het_effects(age, "Generation", "age", fig_label = "B20", newdata = FALSE, N = N_ame, zones = district_ame, countries = countries_ame)
message("Created Fig. 20. Located at Output/figures/fig_B20_wrp_het_age.pdf")


# Appendix: Top Daily Risk----
top_only <- lapply(shocks, function(x) {
  est_models(x, 
            outcome = "climatetop",
            hetvars = hetvars, 
            covars = covars)
})
p_ame <- plot_model_estimates(top_only)
ggsave(p_ame, filename = here("Output", "figures", "fig_B15_ame_toponly_wrp.pdf"),
       width = fwidth, height = 4, scale = fscale)
message("Created Fig. 15. Located at Output/figures/fig_B15_ame_toponly_wrp.pdf")


# Appendix: Continuous Moderator----
# Flip direction for consistency with economic measure in Study 2
# gdp_50 is a ratio where values less than 1 indicate damages;
# converting to a metric where values greater than 0 indicate percentage damage to GDP
summary(g$gdp_50)
g$gdp_50_d <- 100 - g$gdp_50 * 100
summary(g$gdp_50_d)

# Check residual variation
g_agg <- g %>%
  dplyr::select(gdp_50_d, globalreg) %>%
  dplyr::distinct()
dim(g_agg)
gdp_resid <- lm(gdp_50_d ~ globalreg, data = g_agg) %>% resid()
gdp_resid.sd <- sd(gdp_resid)
hist(gdp_resid / gdp_resid.sd)

m_ct <- feols(
  fml = salient ~ gdp_50_d * (best_tanom_7d_z * (riskund + v2x_polyarchy + edu + crop + rural + age + age2) +
                              female + incfeel + hhsize + internet + kids_bin + riskaverse +
                              gdp_log + pop_log + co2_log + oil + coal + urban_reg +
                              gdppc_nat_log + edu_sec + pop_nat_log + co2_nat_log + ag_share) | globalreg,
  data = g,
  vcov = ~ cty_reg
  )
summary(m_ct)
m_ct_df <- plot_comparisons(m_ct, variables = "best_tanom_7d_z", condition = "gdp_50_d", conf_level = .9, draw = FALSE)

m_ct2 <- feols(
  fml = salient ~ gdp_50_d * (best_tanom_2sd_7d_z * (riskund + v2x_polyarchy + edu + crop + rural + age + age2) +
                              female + incfeel + hhsize + internet + kids_bin + riskaverse +
                              gdp_log + pop_log + co2_log + oil + coal + urban_reg +
                              gdppc_nat_log + edu_sec + pop_nat_log + co2_nat_log + ag_share) | globalreg,
  data = g,
  vcov = ~ cty_reg
)
m_ct2_df <- plot_comparisons(m_ct2, variables = "best_tanom_2sd_7d_z", condition = "gdp_50_d", draw = FALSE)

m_ct3 <- feols(
  fml = salient ~ gdp_50_d * (modis_burnanomp_mu_6m_w1_z * (riskund + v2x_polyarchy + edu + crop + rural + age + age2) +
                              female + incfeel + hhsize + internet + kids_bin + riskaverse +
                              gdp_log + pop_log + co2_log + oil + coal + urban_reg +
                              gdppc_nat_log + edu_sec + pop_nat_log + co2_nat_log + ag_share) | globalreg,
  data = g,
  vcov = ~ cty_reg
)
m_ct3_df <- plot_comparisons(m_ct3, variables = "modis_burnanomp_mu_6m_w1_z", condition = "gdp_50_d", draw = FALSE)

m_ct4 <- feols(
  fml = salient ~ gdp_50_d * (noaa_cpc_tdev_7d_z * (riskund + v2x_polyarchy + edu + crop + rural + age + age2) +
                              female + incfeel + hhsize + internet + kids_bin + riskaverse +
                              gdp_log + pop_log + co2_log + oil + coal + urban_reg +
                              gdppc_nat_log + edu_sec + pop_nat_log + co2_nat_log + ag_share) | globalreg,
  data = g,
  vcov = ~ cty_reg
)
m_ct4_df <- plot_comparisons(m_ct4, variables = "noaa_cpc_tdev_7d_z", condition = "gdp_50_d", draw = FALSE)

m_ct_out <- bind_rows(m_ct_df, m_ct2_df, m_ct3_df, m_ct4_df)
p_ct <- m_ct_out %>%
  dplyr::mutate(
    term = case_when(
      term == "best_tanom_7d_z" ~ lab_best_7d,
      term == "best_tanom_2sd_7d_z" ~ lab_best_2sd, 
      term == "modis_burnanomp_mu_6m_w1_z" ~ lab_fire,
      term == "noaa_cpc_tdev_7d_z" ~ lab_noaa_7d
    )
  ) %>%
  ggplot(aes(
    x = gdp_50_d, y = estimate, ymin = conf.low, ymax = conf.high
  )) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_vline(xintercept = 0, color = "grey") +
  geom_line(linewidth = 1) +
  geom_ribbon(alpha = .3) +
  facet_wrap(~ term) +
  theme_classic(base_size = 14) +
  labs(
    y = "Average Shock Effect",
    x = "Climate Change Effect on GDP (%)",
    caption = str_wrap(
      paste(
        "Notes: Shaded bands represent 95% confidence intervals around linear regression estimates,",
        "with standard errors clustered by administrative zone.",
        "Individual-level controls include age, sex, education, rurality, income, household size, internet access,",
        "children, risk aversion, and understanding of climate risk.",
        "Zone-level controls include cropland, GDP, population, CO2 emissions, oil and coal resources, and urbanization.",
        "National-level controls include democracy, GDP per capita, education, population, CO2 emissions, and the share of GDP from agriculture.",
        "Positive values of the GDP moderator reflect projected climate damages; negative values reflect projected gains.",
        "The model includes interactions between the moderator and individual covariates.",
        "The climate shock variable is standardized such that a one-unit increase represents a one standard deviation increase within each region.",
        "The binary outcome indicates whether a respondent identifies climate change as a top or major daily risk.",
        "Data are from the 2019 World Risk Poll, covering 148,712 respondents in 2,458 zones across 137 countries.",
        sep = " "
      ),
      138
    )
  ) +
  theme(
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0)
  )
ggsave(
  p_ct,
  filename = here("Output", "figures", "fig_B18_ame_wrp_ct.pdf"),
  width = fwidth, height = 5, scale = fscale
)
message("Created Fig. 18. Located at Output/figures/fig_B18_ame_wrp_ct.pdf")
message("Finished estimate_shock_effect_models.R")