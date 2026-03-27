# Alexander F. Gazmararian
# agazmararian@gmail.com

library(Amelia)
library(broom)
library(fixest)
library(here)
library(interflex)
library(kableExtra)
library(lmtest)
library(marginaleffects)
library(modelsummary)
library(PanelMatch)
library(plm)
library(sensemakr)
library(tidyverse)
library(tinytable)
library(viridis)
library(zoo)
options("tinytable_theme_placement_latex_float" = "H")

source("Code/ces/est_imp_model.R")
source("Code/fun/desc_stat_fun.R")

set.seed(10)

# Set number of desired cores for running placebo tests
n_cores <- 4

g <- readRDS(here("Data", "output", "ces", "cespanel.rds"))
g <- as.data.frame(g)

# adjust gof
gm <- modelsummary::gof_map
gm$omit <- TRUE
gm[gm$raw == "adj.r.squared", ]$clean <- "Adjusted $R^2$"
gm[gm$raw == "adj.r.squared", ]$omit <- FALSE
gm[gm$raw == "nobs", ]$clean <- "N"
gm[gm$raw == "nobs", ]$omit <- FALSE
gm[gm$raw == "statistic", ]$omit <- FALSE
gm[gm$raw == "statistic", ]$clean <- "F-statistic"
gm[gm$raw == "statistic", ]$fmt <- 1

table_coef <-
  c(
    "fire" = "Climate Shock",
    "tanom" = "Climate Shock",
    "damage_bin" = "Severe Damage (=1)",
    "damage_z" = "Severe Damage",
    "fire:damage_bin" = "Climate Shock $\\times$ Severe Damage",
    "fire:damage_z" = "Climate Shock $\\times$ Severe Damage",
    "tanom:damage_bin" = "Climate Shock $\\times$ Severe Damage",
    "tanom:damage_z" = "Climate Shock $\\times$ Severe Damage",
    "employ" = "Employed",
    "edu_2" = "Education: Some College",
    "edu_3" = "Education: Bachelor's or Post-Grad",
    "income5Q2" = "Household Income: Q2",
    "income5Q3" = "Household Income: Q3",
    "income5Q4" = "Household Income: Q4",
    "income5Not Say" = "Household Income: Not Say",
    "ownhome_bin" = "Homeowner",
    "dem" = "Democrat",
    "rep" = "Republican",
    "ideo3Liberal" = "Ideology: Liberal",
    "ideo3Moderate" = "Ideology: Moderate",
    "ideo3Not Sure" = "Ideology: Not Sure",
    "religimpNot too important" = "Religion: Not too important",
    "religimpSomewhat important" = "Religion: Somewhat important",
    "religimpVery important" = "Religion: Very important",
    "newparent" = "New parent"
  )

covs <- c(
  "damage_bin", "employ", "edu_2", "edu_3",
  "income_q1", "income_q2", "income_q3", "income_q4", "income_ns",
  "dem", "rep", "lib", "con", "relig_1", "relig_2", "relig_3",
  "birthyr", "female", "black", "latino", "ownhome_bin", "newparent"
)
covs_formula_str <- paste("~", paste(covs, collapse = " + "))
covs_formula <- as.formula(covs_formula_str)

# Descriptive Statistics --------------------------------------------------
print("Descriptive Stats...")
file_sum <- here("Output", "tables", "tab_C1_ces_summary.tex")
g %>%
  rename(
    `Wildfire (Placebo)` = n_fire_placebo_2,
    `Severe Damage (=1)` = damage_bin,
    Employed = employ,
  ) %>%
  datasummary(
    formula = (`Climate Policy Support` = gw_binary) + (`Wildfire` = fire) + 
      (`Extreme Heat` = tanom) + (`Severe Damage (=1)`) +
      Employed + (`HS or Less` = edu_1) + (`Some College` = edu_2) + (`BA or Higher` = edu_3) + 
      (Democrat = dem) + (Republican = rep) + (Conservative = con) + (Liberal = lib) + 
      (`New Parent` = newparent) + (`Home Owner` = ownhome_bin) +
      (`Income Q1` = income_q1) + (`Income Q2` = income_q2) + (`Income Q3` = income_q3) + 
      (`Income Q4` = income_q4) + (`Income Q5` = income_q5) + (`Income Not Say` = income_ns) +
      (`Religion Very Important` = relig_4) + (`Religion Somewhat Important` = relig_3) + 
      (`Religion Not Too Important` = relig_2) + (`Religion Not At All Important` = relig_1) 
      ~ Mean + SD + Min + Max + Missing,
    data = .,
    escape = FALSE,
    title = "Panel Survey Summary Statistics"
  ) %>%
  save_tt(file_sum, overwrite = TRUE)

message("Created Table C1. Panel survey summary statistics.")

# Estimate models ----
print("Create imputed data....")

g_imp <- subset(g, select = c(
  caseid, year, gw_binary, damage_bin, damage_z, fire, tanom, 
  employ, dem, rep, ideo3, income5, religimp, ownhome_bin,
  edu_2, edu_3, newparent, female, skeptic, undecided
))

with(g_imp, cor(cbind(
  gw_binary, fire, tanom, damage_bin, employ, dem, rep, ideo3,
  income5, religimp, ownhome_bin, edu_2, edu_3
), use = "pairwise.complete.obs"))

invisible(capture.output(
  a_out <- amelia(
    g_imp,
    m = 30,
    ts = "year",
    cs = "caseid",
    noms = c(
      "gw_binary", "fire", "tanom", "damage_bin", "employ", "dem", 
      "rep", "ideo3", "income5", "ownhome_bin", "female", "skeptic", 
      "undecided", "newparent", "edu_2", "edu_3"
      ),
    ords = c("religimp"),
    autopri = .5,
    parallel = if (.Platform$OS.type == "windows") "snow" else "multicore",
    ncpus = parallel::detectCores() - 1,
    verbose = FALSE
  )
))

while (any(sapply(a_out$imputations, function(x) is.logical(x) && is.na(x)))) {
  invisible(capture.output(
    a_out <- amelia(
      g_imp,
      m = 4,
      ts = "year",
      cs = "caseid",
      noms = c(
      "gw_binary", "fire", "tanom", "damage_bin", "employ", "dem", 
      "rep", "ideo3", "income5", "ownhome_bin", "female", "skeptic", 
      "undecided", "newparent", "edu_2", "edu_3"
      ),
      ords = c("religimp"),
      autopri = .5,
      parallel = if (.Platform$OS.type == "windows") "snow" else "multicore",
      ncpus = parallel::detectCores() - 1,
      verbose = FALSE
    )
  ))
}
message("Created imputed data.")

# Model specification

coef_vector <- c(
  "fire", "damage_bin", "employ", "edu_2", "edu_3", "dem", "rep", 
  "ideo3Liberal", "ideo3Moderate", "ideo3Not Sure",
  "income5Not Say", "income5Q2", "income5Q3", "income5Q4", "income5Q5", 
  "religimpNot too important", "religimpSomewhat important",
  "religimpVery important", "newparent", "ownhome_bin"
)

f <- gw_binary ~ fire + damage_bin + employ + edu_2 + edu_3 + 
  dem + rep + ideo3 + income5 + religimp + newparent + ownhome_bin

# Estimate main outcome
message("Estimate imputed models....")

# Calculate fire effect
m <- list()
m[[1]] <- est_imp_model(formula = f, coefvector = coef_vector)
m[[2]] <- est_imp_model(formula = update(f, ~ fire * damage_bin + .), coefvector = c(coef_vector, "fire:damage_bin"))
m[[3]] <- est_imp_model(formula = update(f, ~ fire * damage_z + . - damage_bin), coefvector = c("fire", "damage_z", coef_vector[3:length(coef_vector)], "fire:damage_z"))

coefs_anom <- c("tanom", coef_vector[2:length(coef_vector)])
m[[4]] <- est_imp_model(formula = update(f, ~ tanom + . - fire), coefvector = coefs_anom)

coefs_anom_t <- c("tanom", coef_vector[2:length(coef_vector)], "tanom:damage_bin")
m[[5]] <- est_imp_model(formula = update(f, ~ tanom * damage_bin + . - fire), coefvector = coefs_anom_t)

coefs_anom_tz <- c("tanom", "damage_z", coef_vector[3:length(coef_vector)], "tanom:damage_z")
m[[6]] <- est_imp_model(formula = update(f, ~ tanom * damage_z + . - fire - damage_bin), coefvector = coefs_anom_t)

tab_did <- here("Output", "tables", "tab_C2_ces_did.tex")
cap <- "Climate Shocks and Change in Climate Policy Support"
notes <- paste(
  "This model does not account for treatment reversal like the panel matching estimates in the main text, so treat the estimates with caution.",
  "Estimates from a linear regression model with \\textcite{newey1986} standard errors in parentheses to account for heteroskedasticity and serial correlation.",
  "The outcome is a binary indicator that takes the value 1 if an individual supports climate policy and 0 if not.",
  "Missing values imputed using 30 multiple imputations \\parencite{blackwell2017}.",
  "$^{*}p<0.1$; $^{**}p<0.05$; $^{***}p<0.01$",
  sep = " "
)
modelsummary(
  m,
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_map = gm,
  fmt = 2,
  coef_map = table_coef,
  escape = FALSE,
  add_rows = data.frame(
    c("Individual Fixed Effects", "Panel Wave Fixed Effects"),
    c("Yes", "Yes"),
    c("Yes", "Yes"),
    c("Yes", "Yes"),
    c("Yes", "Yes"),
    c("Yes", "Yes"),
    c("Yes", "Yes")
  ),
  title = cap,
  notes = notes
) %>%
  group_tt(
    j = list(
      "Wildfires" = 2:4,
      "Extreme Heat" = 5:7
    )
  ) %>%
  group_tt(
    j = list("Outcome: Climate Policy Support" = 2:7)
  ) %>%
  theme_tt("resize", width = .8, escape = FALSE) %>%
  save_tt(tab_did, overwrite = TRUE)

message("Created Table C2. Climate Shocks and Change in Climate Policy Support.")

# Continuous damage moderator ----------------------------------------------------
# Reload data to avoid "external pointer is not valid" errors after parallel imputation
# The amelia() parallel processing can corrupt R objects with external pointers
g <- readRDS(here("Data", "output", "ces", "cespanel.rds"))
g <- as.data.frame(g)
gc(verbose = FALSE)  # Clean up memory

message("Estimate binning model...")

mod_bin <-
  interflex(
    estimator = "binning",
    data = g,
    Y = "gw_binary",
    D = "fire",
    X = "totaldamage",
    treat.type = "continuous",
    Z = c("employ", "edu_2", "edu_3", "dem", "rep", "ideo3", "income5", "religimp",
          "newparent", "ownhome_bin"),
    FE = c("year", "caseid"),
    vcov.type = "cluster",
    cl = "caseid",
    na.rm = TRUE,
    theme.bw = TRUE,
    ylab = "Climate Shock Effect on Mitigation Support",
    Xlabel = "Severe Damage",
    main = "Wildfires",
    show.grid = FALSE,
    nbin = 3
  )
mod_bin_temp <-
  interflex(
    estimator = "binning",
    data = g,
    Y = "gw_binary",
    D = "tanom",
    X = "totaldamage",
    treat.type = "continuous",
    Z = c("employ", "edu_2", "edu_3", "dem", "rep", "ideo3", "income5", "religimp", "newparent", "ownhome_bin"),
    FE = c("year", "caseid"),
    vcov.type = "cluster",
    cl = "caseid",
    na.rm = TRUE,
    main = "Extreme Heat",
    theme.bw = TRUE,
    ylab = "Climate Shock Effect on Mitigation Support",
    Xlabel = "Severe Damage",
    show.grid = FALSE,
    nbin = 3
  )

p_out <- gridExtra::grid.arrange(mod_bin$figure, mod_bin_temp$figure, ncol = 2)
ggsave(p_out, filename = here("Output", "figures", "fig_C7_ces_binning.pdf"),
       scale = 1.6, width = 6.5, height = 3)
message("Created Figure C5. Continuous moderator plots.")

# Sensitivity analyses----
coef_sens <- coef_vector[-c(1, 2)]
# Regress interaction term on benchmark variable and other covariates
## Fire
f_in_fire <- update(f, interact_fire ~ . - damage_bin - fire)
inter_fire <- est_imp_model(f_in_fire, coefvector = coef_sens)
## Temp
f_in_temp <- update(f, interact_heat ~ . - damage_bin - tanom - fire)
inter_temp <- est_imp_model(f_in_temp, coefvector = coef_sens)
## Model for estimating sensitivty
m.fire <- m[[2]]$tidy
att.fire <- m.fire[m.fire$term == "fire:damage_bin", "estimate"]
att.fire.se <- m.fire[m.fire$term == "fire:damage_bin", "std.error"]
dof.fire <- m[[2]]$glance$dof

m.temp <- m[[5]]$tidy
att.temp <- m.temp[m.temp$term == "tanom:damage_bin", "estimate"]
att.temp.se <- m.temp[m.temp$term == "tanom:damage_bin", "std.error"]
dof.temp <- m[[5]]$glance$dof

# Function
est_sens <- function(shockvar, benchmark, kd = c(10, 20), benchmark_lab, fig_prefix) {
  if (shockvar == "fire") {
    r2dxj_dem <- partial_r2(t_statistic = inter_fire$tidy[inter_fire$tidy$term == benchmark, "statistic"], dof = dof.fire)
    r2yxj_dem <- partial_r2(t_statistic = m.fire[m.fire$term == benchmark, "statistic"], dof = dof.fire)

    sense_dem <- sensemakr(
      estimate = att.fire,
      se = att.fire.se,
      r2dxj.x = r2dxj_dem,
      r2yxj.dx = r2yxj_dem,
      dof = dof.fire,
      kd = kd,
      alpha = .1
      )
        
    sense_dem$bounds$bound_label <- paste0(kd, "x ", benchmark_lab)

    pdf(here("Output", "figures", paste0("fig_", fig_prefix, "_ces_sense_", shockvar, "_", benchmark, ".pdf")), width = 6.5, height = 3, pointsize = 7)
    par(mfrow = c(1, 2))
    plot(sense_dem, sensitivity.of = "t-value", main = "Sensitivity of t-value", lim = .04, lim.y = .04, label.bump.x = 0.005, label.bump.y = 0.0005, round = 3)
    plot(sense_dem, main = "Sensitivity of the Interaction Effect", lim = .04, lim.y = .04, label.bump.x = 0.005, label.bump.y = 0.0005, round = 3)
    dev.off()
  }
  if (shockvar == "tanom") {
    r2dxj_dem <- partial_r2(t_statistic = inter_temp$tidy[inter_temp$tidy$term == benchmark, "statistic"], dof = dof.temp)
    r2yxj_dem <- partial_r2(t_statistic = m.temp[m.temp$term == benchmark, "statistic"], dof = dof.temp)

    sense_dem <- sensemakr(
      estimate = att.temp,
      se = att.temp.se,
      r2dxj.x = r2dxj_dem,
      r2yxj.dx = r2yxj_dem,
      dof = dof.temp,
      kd = kd,
      alpha = .1
      )
  
    sense_dem$bounds$bound_label <- paste0(kd, "x ", benchmark_lab)

    pdf(here("Output", "figures", paste0("fig_", fig_prefix, "_ces_sense_", shockvar, "_", benchmark, ".pdf")), width = 6.5, height = 3, pointsize = 7)
    par(mfrow = c(1, 2))
    plot(sense_dem, sensitivity.of = "t-value", main = "Sensitivity of t-value", lim = .04, lim.y = .04, label.bump.x = 0.005, label.bump.y = 0.0005, round = 3)
    plot(sense_dem, main = "Sensitivity of the Interaction Effect", lim = .04, lim.y = .04, label.bump.x = 0.005, label.bump.y = 0.0005, round = 3)
    dev.off()
  }

  return(f)
}

est_sens(shockvar = "fire", benchmark = "dem", kd = c(10, 20), benchmark_lab = "Democrat", fig_prefix = "C1")
est_sens(shockvar = "tanom", benchmark = "dem", kd = c(10, 20), benchmark_lab = "Democrat", fig_prefix = "C4")

est_sens(shockvar = "fire", benchmark = "edu_2", kd = c(5, 10), benchmark_lab = "Education", fig_prefix = "C2")
est_sens(shockvar = "tanom", benchmark = "edu_2", kd = c(5, 10), benchmark_lab = "Education", fig_prefix = "C5")

est_sens(shockvar = "fire", benchmark = "newparent", kd = c(15), benchmark_lab = "Parenthood", fig_prefix = "C3")
est_sens(shockvar = "tanom", benchmark = "newparent", kd = c(15), benchmark_lab = "Parenthood", fig_prefix = "C6")

message("Finished sensitivity analyses.")

# Effect Heterogeneity----
message("Starting effect heterogeneity analyses.")

est_panel_match <- function(data, subgroup_label, covs_in = covs_formula) {
  
  pm_data <- PanelData(data, "caseid", "time", "fire", "gw_binary")
  
  pm_out <-
    PanelMatch(
      panel.data = pm_data,
      lag = 1,
      refinement.method = "CBPS.match",
      covs.formula = covs_in,
      match.missing = TRUE,
      qoi = "att"
    )
  pm_att <- PanelEstimate(sets = pm_out, panel.data = pm_data, moderator = "damage_bin", se.method = "conditional")
  
  pm_data.2 <- PanelData(data, "caseid", "time", "tanom", "gw_binary")
  pm_out.2 <-
    PanelMatch(
      panel.data = pm_data.2,
      lag = 1,
      refinement.method = "CBPS.match",
      covs.formula = covs_in,
      match.missing = TRUE,
      qoi = "att"
    )
  pm_att.2 <- PanelEstimate(sets = pm_out.2, panel.data = pm_data.2, moderator = "damage_bin", se.method = "conditional")
  

  df_out <- data.frame(
    damage_bin = c("Severe Damage", "Limited Damage", "Severe Damage", "Limited Damage"),
    
    estimate = c(
      pm_att$`1`$estimate, pm_att$`0`$estimate, 
      pm_att.2$`1`$estimate, pm_att.2$`0`$estimate
    ),
    std.error = c(
      pm_att$`1`$standard.error, pm_att$`0`$standard.error,
      pm_att.2$`1`$standard.error, pm_att.2$`0`$standard.error
    ),
    
    treat = c("Wildfire", "Wildfire", "Extreme Heat", "Extreme Heat"),
    group = rep(subgroup_label, 4)
  )
  df_out$ub <- df_out$estimate + 1.96 * df_out$std.error
  df_out$lb <- df_out$estimate - 1.96 * df_out$std.error
  return(df_out)
}

## Skepticism----
g_believer <- subset(g, believer == 1)
g_skeptic <- subset(g, skeptic == 1)
g_undecided <- subset(g, undecided == 1)

m_believer <- est_panel_match(data = g_believer, subgroup_label = "Believer")
m_skeptic <- est_panel_match(data = g_skeptic, subgroup_label = "Skeptic")
m_undecided <- est_panel_match(data = g_undecided, subgroup_label = "Undecided")

p_belief <- rbind(m_believer, m_skeptic, m_undecided) %>%
  mutate(group = factor(group, levels = c("Skeptic", "Undecided", "Believer"))) %>%
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_pointrange(aes(x = group, y = estimate, ymin = lb, ymax = ub, color = damage_bin, shape = damage_bin),
    position = position_dodge(.2), linewidth = 1.5, size = 1
  ) +
  theme_bw(base_size = 14) +
  facet_wrap(~treat, ncol = 2) +
  scale_color_manual(values = c("blue", "black")) +
  labs(x = "Climate Shock Effect on Mitigation Policy Support\nModerated by Prior Climate Beliefs", y = "Average Change in Probability of Support", color = "", shape = "") +
  theme(
    panel.grid = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(.9, .9),
    legend.background = element_blank()
  )
ggsave(
  p_belief,
      filename = here("Output", "figures", "fig_C9_ces_het_skeptic.pdf"),
  width = 6.5,
  height = 2.9,
  scale = 1.5
)

## Education----
g <- g %>%
  filter(year == 2010) %>%
  rename(
    edu_1_t0 = edu_1,
    edu_2_t0 = edu_2,
    edu_3_t0 = edu_3
  ) %>%
  dplyr::select(caseid, edu_1_t0, edu_2_t0, edu_3_t0) %>%
  left_join(g, ., by = "caseid")
covs_in <- as.formula(paste(gsub("\\+ edu_2 \\+ edu_3", "", covs_formula), collapse = ""))

edu1 <- subset(g, edu_1_t0 == 1)
m_hs <- est_panel_match(data = edu1, "HS or Less", covs_in = covs_in)

edu2 <- subset(g, edu_2_t0 == 1)
m_ass <- est_panel_match(data = edu2, "2-Year Degree or Some College", covs = covs_in)

edu3 <- subset(g, edu_3_t0 == 1)
m_ba <- est_panel_match(data = edu3, "Bachelors or More", covs = covs_in)

p_edu <- bind_rows(m_hs, m_ass, m_ba) %>%
  mutate(group = factor(group, levels = c("HS or Less", "2-Year Degree or Some College", "Bachelors or More"))) %>%
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_pointrange(aes(x = group, y = estimate, ymin = lb, ymax = ub, color = damage_bin, shape = damage_bin),
    position = position_dodge(.2), linewidth = 1.5, size = 1
  ) +
  theme_bw(base_size = 14) +
  facet_wrap(~treat, ncol = 2) +
  scale_color_manual(values = c("blue", "black")) +
  labs(
    x = "Climate Shock Effect on Mitigation Policy Support\nModerated by Education", 
    y = "Average Change in Probability of Support", 
    color = "", 
    shape = ""
    ) +
  scale_x_discrete(labels = label_wrap_gen(18)) +
  theme(
    panel.grid = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(.9, .9),
    legend.background = element_blank()
  )
ggsave(
  p_edu,
  filename = here("Output", "figures", "fig_C10_ces_het_edu.pdf"),
  width = 6.5,
  height = 2.9,
  scale = 1.5
)
message("Created Figure C10. Education heterogeneity.")

## Age Cohort/Generation----
table(g$birthyr)
g <- g %>%
  dplyr::mutate(
    age_gen = factor(
      case_when(
        birthyr >= 1997 ~ "Gen Z",
        birthyr >= 1981 & birthyr < 1997 ~ "Millennial",
        birthyr >= 1965 & birthyr < 1981 ~ "Gen X",
        birthyr >= 1946 & birthyr < 1965 ~ "Boomer",
        birthyr < 1946 ~ "Silent"
      )
    )
  )
with(g, table(age_gen, fire))
with(g, table(age_gen, tanom))

genmil <- subset(g, age_gen == "Millennial")
m_genmil <- est_panel_match(data = genmil, "Millennial")

genx <- subset(g, age_gen == "Gen X")
m_genx <- est_panel_match(data = genx, "Gen X")

genboom <- subset(g, age_gen == "Boomer")
m_genboom <- est_panel_match(data = genboom, "Boomer")

gensilent <- subset(g, age_gen == "Silent")
m_gensilent <- est_panel_match(data = gensilent, "Silent")

p_age <- rbind(m_genmil, m_genx, m_genboom, m_gensilent) %>%
  mutate(group = factor(group, levels = c("Millennial", "Gen X", "Boomer", "Silent"))) %>%
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_pointrange(aes(x = group, y = estimate, ymin = lb, ymax = ub, color = damage_bin, shape = damage_bin),
    position = position_dodge(.2), linewidth = 1.5, size = 1
  ) +
  theme_bw(base_size = 14) +
  facet_wrap(~treat, ncol = 2) +
  scale_color_manual(values = c("blue", "black")) +
  labs(
    x = "Climate Shock Effect on Mitigation Policy Support\nModerated by Generation", 
    y = "Average Change in Probability of Support", 
    color = "",
    shape = ""
    ) +
  scale_x_discrete(labels = label_wrap_gen(18)) +
  theme(
    panel.grid = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(.9, .15),
    legend.background = element_blank()
  )
ggsave(
  p_age,
      filename = here("Output", "figures", "fig_C11_ces_het_age.pdf"),
  width = 6.5,
  height = 2.9,
  scale = 1.5
)
message("Created Figure C11. Age cohort/generation heterogeneity.")


# Panel matching estimator ----------------------------------------------------------------

## Fire effect----
fire_panel <- PanelData(g, "caseid", "time", "fire", "gw_binary")

pm_out <-
  PanelMatch(
    panel.data = fire_panel,
    lag = 1,
    refinement.method = "CBPS.match",
    covs.formula = covs_formula,
    match.missing = TRUE,
    qoi = "att"
  )
pm_att <- PanelEstimate(sets = pm_out, panel.data = fire_panel, moderator = "damage_bin", se.method = "conditional")
cat
cat("\n")
cat("================================================================================\n")
cat("                   Average effect of fire shock on climate policy support (p. 6) \n")
cat("================================================================================\n")
cat("\n")
cat("...Facing severe damages...")
print(pm_att$`1`)
cat("\n")
cat("...Facing limited damages...")
print(pm_att$`0`)
cat("\n")
cat("================================================================================\n")
cat("\n")

# Panel matching placebo tests
damage <- subset(g, damage_bin == 1)
fire_damage <- PanelData(damage, "caseid", "time", "fire", "gw_binary")

benefit <- subset(g, damage_bin == 0)
fire_benefit <- PanelData(benefit, "caseid", "time", "fire", "gw_binary")

placebo_cov <- covs[-1]
placebo_covs <- as.formula(paste("~", paste(covs, collapse = " + ")))

placebo_damage <-
  PanelMatch(
    lag = 2,
    refinement.method = "CBPS.match",
    covs.formula = placebo_covs,
    panel.data = fire_damage,
    match.missing = TRUE,
    qoi = "att",
    placebo = TRUE
  )
placebo_damage_test <- placebo_test(pm.obj = placebo_damage, 
                                    panel.data = fire_damage, 
                                    number.iterations = 2000,
                                    parallel = TRUE,
                                    num.cores = n_cores)
placebo_damage_test
df_placebo_damage <-
  data.frame(placebo_damage_test$bootstrapped.estimates) %>%
  summarize(
    damage_bin = 1,
    estimate = mean(t.2),
    lb = quantile(t.2, .025),
    ub = quantile(t.2, 0.975),
    treat = "Fire",
    facet = "(b) Placebo"
  )

placebo_benefit <-
  PanelMatch(
    lag = 2, 
    refinement.method = "CBPS.match",
    covs.formula = placebo_covs,
    panel.data = fire_benefit,
    match.missing = TRUE,
    qoi = "att",
    placebo = TRUE
  )

placebo_benefit_test <- placebo_test(pm.obj = placebo_benefit, 
                                     panel.data = fire_benefit,
                                     number.iterations = 2000,
                                     parallel = TRUE,
                                     num.cores = n_cores)
df_placebo_benefit <-
  data.frame(placebo_benefit_test$bootstrapped.estimates) %>%
  summarize(
    damage_bin = 0,
    estimate = mean(t.2),
    lb = quantile(t.2, .025),
    ub = quantile(t.2, 0.975),
    treat = "Fire",
    facet = "(b) Placebo"
  )

## Heat shocks----
temp_panel <- PanelData(g, "caseid", "time", "tanom", "gw_binary")

pm_temp <-
  PanelMatch(
    lag = 1,
    refinement.method = "CBPS.match",
    covs.formula = covs_formula,
    panel.data = temp_panel,
    match.missing = TRUE,
    qoi = "att",
    forbid.treatment.reversal = TRUE
  )
pm_temp.att <- PanelEstimate(sets = pm_temp, panel.data = temp_panel, 
                             moderator = "damage_bin", se.method = "conditional")
cat("\n")
cat("================================================================================\n")
cat("                   Average effect of heat shock on climate policy support (p. 6) \n")
cat("================================================================================\n")
cat("\n")
cat("...Facing severe damages...")
print(pm_temp.att$`1`)
cat("\n")
cat("...Facing limited damages...")
print(pm_temp.att$`0`)
cat("\n")
cat("================================================================================\n")
cat("\n")

temp_damage <- subset(g, damage_bin == 1)
temp_damage <- PanelData(temp_damage, "caseid", "time", "tanom", "gw_binary")

placebo_damage_temp <-
  PanelMatch(
    lag = 2,
    refinement.method = "CBPS.match",
    covs.formula = placebo_covs,
    panel.data = temp_damage,
    match.missing = TRUE,
    qoi = "att",
    placebo = TRUE
  )
placebo_damage_temp.test <- placebo_test(pm.obj = placebo_damage_temp, 
                                         panel.data = temp_damage,
                                         number.iterations = 2000,
                                         parallel = TRUE,
                                         num.cores = n_cores)
df_placebo_damage_temp <-
  data.frame(placebo_damage_temp.test$bootstrapped.estimates) %>%
  summarize(
    damage_bin = 1,
    estimate = mean(t.2),
    lb = quantile(t.2, .025),
    ub = quantile(t.2, 0.975),
    treat = "+2SD heat anomaly",
    facet = "(b) Placebo"
  )

temp_benefit <- subset(g, damage_bin == 0)
temp_benefit <- PanelData(temp_benefit, "caseid", "time", "tanom", "gw_binary")

placebo_benefit_temp <-
  PanelMatch(
    lag = 2,
    refinement.method = "CBPS.match",
    covs.formula = placebo_covs,
    panel.data = temp_benefit,
    match.missing = TRUE,
    qoi = "att",
    placebo = TRUE
  )
placebo_benefit_temp.test <- placebo_test(pm.obj = placebo_benefit_temp, 
                                          panel.data = temp_benefit, 
                                          number.iterations = 2000,
                                          parallel = TRUE,
                                          num.cores = n_cores)
df_placebo_benefit_temp <-
  data.frame(placebo_benefit_temp.test$bootstrapped.estimates) %>%
  summarize(
    damage_bin = 0,
    estimate = mean(t.2),
    lb = quantile(t.2, .025),
    ub = quantile(t.2, 0.975),
    treat = "+2SD heat anomaly",
    facet = "(b) Placebo"
  )

### Covariate balance plots----
covbal_temp <- get_covariate_balance(pm_temp, panel.data = temp_panel, covariates = covs, include.unrefined = TRUE)
covbal_fire <- get_covariate_balance(pm_out, panel.data = fire_panel, covariates = covs, include.unrefined = TRUE)

pdf(here("Output", "figures", "fig_C8_ces_panel_balance_improve.pdf"), width = 6.5, height = 3, pointsize = 7)
par(mfrow = c(1, 2))
plot(covbal_fire, type = "scatter", main = "Fire", ylab = "After refinement")
plot(covbal_temp, type = "scatter", main = "Extreme Heat", ylab = "After refinement")
dev.off()


### AJPS Fig. 8----
pmatch_df <-
  data.frame(
    damage_bin = c(1, 0, 1, 0),
    estimate = c(
      pm_att$`1`$estimate, pm_att$`0`$estimate,
      pm_temp.att$`1`$estimate, pm_temp.att$`0`$estimate
    ),
    std.error = c(
      pm_att$`1`$standard.error, pm_att$`0`$standard.error,
      pm_temp.att$`1`$standard.error, pm_temp.att$`0`$standard.error
    ),
    facet = "(a) Climate shock effect",
    treat = c("Fire", "Fire", "+2SD heat anomaly", "+2SD heat anomaly")
  )
pmatch_df$ub <- with(pmatch_df, estimate + qnorm(.975) * std.error)
pmatch_df$lb <- with(pmatch_df, estimate + qnorm(.025) * std.error)

p_result <- bind_rows(
  df_placebo_damage_temp, df_placebo_benefit_temp,
  df_placebo_damage, df_placebo_benefit, pmatch_df
) %>%
  mutate(
    damage_bin = ifelse(damage_bin == 1, "Severe damage", "Limited damage")
  ) %>%
  ggplot(aes(
    x = estimate, xmax = ub, xmin = lb,
    y = treat, color = damage_bin, shape = damage_bin
  )) +
  geom_vline(xintercept = 0, color = "grey") +
  geom_pointrange(
    position = position_dodge(.2),
    linewidth = 1.5, size = 1
  ) +
  facet_wrap(~facet) +
  theme_classic(base_size = 14) +
  scale_x_continuous(limits = c(-.125, .2)) +
  labs(
    x = "Climate shock effect on climate policy support moderated by vulnerability",
    y = NULL,
    color = "",
    shape = "",
    caption = str_wrap(
      paste(
        "Notes: Bars indicate 95% confidence intervals.",
        "The outcome is coded as 1 if the respondent supports climate mitigation, and 0 otherwise.",
        "Panel (b) shows the effect of lagged climate shocks as a placebo.",
        "Estimates are from a panel matching estimator that balances survey respondents on employment,",
        "education, income, partisanship, ideology, religiosity, age, gender, race, parental status, and home ownership.",
        "Based on a nationally representative sample of American adults.",
        "Data come from the Cooperative Election Study, which surveyed the same 9,500 individuals in 2010, 2012, and 2014.",
        sep = " "
      ),
      138
    )
  ) +
  scale_color_manual(values = c("blue", "black")) +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    legend.background = element_blank(),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 14, margin = margin(b = 10), color = "black"),
    axis.text = element_text(color = "black"),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0)
  )

ggsave(
  plot = p_result,
  filename = here("Output", "figures", "fig_8_ces_panel_matching.pdf"),
  scale = 1.5,
  width = 6.5,
  height = 3.5
)
message("Created Figure 8. Panel matching estimator.")

# Book figure (not included in replication package - for book manuscript only)
# source("~/Dropbox/Research/cleavages/Code/individual/ces/book_fig_3.7_panel.R")
# create_book_fig_3.7_panel(pm_att = pm_att, pm_temp.att = pm_temp.att, df_placebo_damage_temp = df_placebo_damage_temp, df_placebo_benefit_temp = df_placebo_benefit_temp, df_placebo_damage = df_placebo_damage, df_placebo_benefit = df_placebo_benefit)
message("Finished analyze_ces.R")