### Multi-level regression models for trends in trust
# libraries
if (!require("pacman")) install.packages("pacman")
p_load(dplyr, mgcv, tidyverse, ggplot2, ggpubr, haven, plyr, lme4, broom, optimx,
       gmodels, stargazer, labelled, sjPlot, janitor, viridis, margins, ggeffects, reshape2, texreg, sjstats, lmerTest)

# Models on the individual level
# These models require data on the individual level which is not included in the replication folder
# Information on where the data can be accessed can be found in the file "Trends_readme" in the replication folder
df_indtrust <- read_dta("Trust_ind_trends.dta")

# Include only countries which were liberal or electoral democracies in a majority of years included, according to the VDem Regions of the world measure
df_indtrust.dem <- subset(df_indtrust, vdem_row_maj == 1)

# Remove observations before 1980 for all data, because they are few and unrepresentative globally
df_indtrust.dem <- df_indtrust.dem [!df_indtrust.dem$year < 1980,]

# Remove observations before 1990 for all but the WENA region, because they are also few and unrepresentative in other regions
# These are only in the WENA region and mostly in the US
df_indtrust.dem2 <- df_indtrust.dem [!df_indtrust.dem$year < 1990,]

model_trends_parl <- lmer(trust_parl ~ year + (1 | country/study), data = df_indtrust.dem2, weights=weight)
summary(model_trends_parl)

model_trends_gov <- lmer(trust_gov ~ year + (1 | country/study), data = df_indtrust.dem2, weights=weight)
summary(model_trends_gov)

model_trends_polpar <- lmer(trust_polpar ~ year + (1 | country/study), data = df_indtrust.dem2, weights=weight)
summary(model_trends_polpar)

model_trends_leg <- lmer(trust_leg ~ year + (1 | country/study), data = df_indtrust.dem2, weights=weight)
summary(model_trends_leg)

model_trends_police <- lmer(trust_police ~ year + (1 | country/study), data = df_indtrust.dem2, weights=weight)
summary(model_trends_police)

model_trends_civil <- lmer(trust_civil ~ year + (1 | country/study), data = df_indtrust.dem2, weights=weight)
summary(model_trends_civil)

model_trends_list <- list(model_trends_parl, model_trends_gov, model_trends_polpar,
                          model_trends_civil, model_trends_leg, model_trends_police)
model_names <- c("Parliament", "Government", "Political parties",
                 "Civil service", "Legal system", "Police")

wordreg(model_trends_list, file = "MLM_Global.doc", custom.model.names = model_names, digits = 4)

# No weights
model_trends_parl_noweight <- lmer(trust_parl ~ year + (1 | country/study), data = df_indtrust.dem2)
summary(model_trends_parl_noweight)

model_trends_gov_noweight <- lmer(trust_gov ~ year + (1 | country/study), data = df_indtrust.dem2)
summary(model_trends_gov_noweight)

model_trends_polpar_noweight <- lmer(trust_polpar ~ year + (1 | country/study), data = df_indtrust.dem2)
summary(model_trends_polpar_noweight)

model_trends_leg_noweight <- lmer(trust_leg ~ year + (1 | country/study), data = df_indtrust.dem2)
summary(model_trends_leg_noweight)

model_trends_police_noweight <- lmer(trust_police ~ year + (1 | country/study), data = df_indtrust.dem2)
summary(model_trends_police_noweight)

model_trends_civil_noweight <- lmer(trust_civil ~ year + (1 | country/study), data = df_indtrust.dem2)
summary(model_trends_civil_noweight)

model_trends_list_noweight <- list(model_trends_parl_noweight, model_trends_gov_noweight, model_trends_polpar_noweight,
                          model_trends_civil_noweight, model_trends_leg_noweight, model_trends_police_noweight)
model_names_noweight <- c("Parliament", "Government", "Political parties",
                 "Civil service", "Legal system", "Police")

wordreg(model_trends_list_noweight, file = "MLM_Global_noweights.doc", custom.model.names = model_names_noweight, digits = 4)

# Logit models with odds ratios - these may fail to converge and be "nearly unidentifiable" because of large eigenvalues and eigenvalue ratios
model_trends_parl_logit <- glmer(trust_parl ~ year + (1 | country/study), data = df_indtrust.dem2, weights=weight,
                           family = binomial(link = "logit"))
summary(model_trends_parl_logit)
parl_logit_year_coef <- fixef(model_trends_parl_logit)["year"]
parl_logit_year_or <- exp(parl_logit_year_coef)
se_logit_parl_var <- vcov(model_trends_parl_logit)["year", "year"]
se_or_parl_year <- sqrt(se_logit_parl_var * parl_logit_year_or^2)

model_trends_gov_logit <- glmer(trust_gov ~ year + (1 | country/study), data = df_indtrust.dem2, weights=weight,
                               family = binomial(link = "logit"))
summary(model_trends_gov_logit)
gov_logit_year_coef <- fixef(model_trends_gov_logit)["year"]
gov_logit_year_or <- exp(gov_logit_year_coef)
se_logit_gov_var <- vcov(model_trends_gov_logit)["year", "year"]
se_or_gov_year <- sqrt(se_logit_gov_var * gov_logit_year_or^2)

model_trends_polpar_logit <- glmer(trust_polpar ~ year + (1 | country/study), data = df_indtrust.dem2, weights=weight,
                                  family = binomial(link = "logit"))
summary(model_trends_polpar_logit)
polpar_logit_year_coef <- fixef(model_trends_polpar_logit)["year"]
polpar_logit_year_or <- exp(polpar_logit_year_coef)
se_logit_polpar_var <- vcov(model_trends_polpar_logit)["year", "year"]
se_or_polpar_year <- sqrt(se_logit_polpar_var * polpar_logit_year_or^2)

model_trends_civil_logit <- glmer(trust_civil ~ year + (1 | country/study), data = df_indtrust.dem2, weights=weight,
                                  family = binomial(link = "logit"))
summary(model_trends_civil_logit)
civil_logit_year_coef <- fixef(model_trends_civil_logit)["year"]
civil_logit_year_or <- exp(civil_logit_year_coef)
se_logit_civil_var <- vcov(model_trends_civil_logit)["year", "year"]
se_or_civil_year <- sqrt(se_logit_civil_var * civil_logit_year_or^2)

model_trends_leg_logit <- glmer(trust_leg ~ year + (1 | country/study), data = df_indtrust.dem2, weights=weight,
                               family = binomial(link = "logit"))
summary(model_trends_leg_logit)
leg_logit_year_coef <- fixef(model_trends_leg_logit)["year"]
leg_logit_year_or <- exp(leg_logit_year_coef)
se_logit_leg_var <- vcov(model_trends_leg_logit)["year", "year"]
se_or_leg_year <- sqrt(se_logit_leg_var * leg_logit_year_or^2)

model_trends_police_logit <- glmer(trust_police ~ year + (1 | country/study), data = df_indtrust.dem2, weights=weight,
                                  family = binomial(link = "logit"))
summary(model_trends_police_logit)
police_logit_year_coef <- fixef(model_trends_police_logit)["year"]
police_logit_year_or <- exp(police_logit_year_coef)
se_logit_police_var <- vcov(model_trends_police_logit)["year", "year"]
se_or_police_year <- sqrt(se_logit_police_var * police_logit_year_or^2)

model_trends_list_logit <- list(model_trends_parl_logit, model_trends_gov_logit, model_trends_polpar_logit,
                                   model_trends_civil_logit, model_trends_leg_logit, model_trends_police_logit)
model_names_logit <- c("Parliament", "Government", "Political parties",
                          "Civil service", "Legal system", "Police")

wordreg(model_trends_list_logit, file = "MLM_Global_logit.doc", custom.model.names = model_names_logit, digits = 4)

# Table / dataframe with odds ratios and approximal odds ratios standard errors (delta method)
models_logit_or_df <- data.frame(Model = model_names_logit, OR = numeric(length(model_names_logit)), SE = numeric(length(model_names_logit)))

models_logit_or_df$OR <- c(parl_logit_year_or, gov_logit_year_or, polpar_logit_year_or,
                           civil_logit_year_or, leg_logit_year_or, police_logit_year_or)

models_logit_or_df$SE <- c(se_or_parl_year, se_or_gov_year, se_or_polpar_year,
                           se_or_civil_year, se_or_leg_year, se_or_police_year)
View(models_logit_or_df)

# Models using aggregated data
trust.agg = read_dta("Trust_trends_rep.dta")
# Include only countries which were liberal or electoral democracies in a majority of years included, according to the VDem Regions of the world measure
df_indtrust.dem <- subset(df_indtrust, vdem_row_maj == 1)

# Remove observations before 1980 for all data, because they are very few and unrepresentative
df_indtrust.dem <- df_indtrust.dem [!df_indtrust.dem$year < 1980,]

# Remove observations before 1990 for all but the WENA region, because they are also few and unrepresentative
# These are only in the WENA region and mostly in the US
df_indtrust.dem2 <- df_indtrust.dem [!df_indtrust.dem$year < 1990,]

trust.agg.dem <- subset(trust.agg, vdem_row_maj == 1)
trust.agg.dem2 <- trust.agg.dem [!trust.agg.dem$year < 1990,]

trust.agg.dem$trust_parl <- trust.agg.dem$trust_parl * 100
trust.agg.dem$trust_gov <- trust.agg.dem$trust_gov * 100
trust.agg.dem$trust_polpar <- trust.agg.dem$trust_polpar * 100
trust.agg.dem$trust_civil <- trust.agg.dem$trust_civil * 100
trust.agg.dem$trust_leg <- trust.agg.dem$trust_leg * 100
trust.agg.dem$trust_police <- trust.agg.dem$trust_police * 100

model_trends_parl_agg <- lmer(trust_parl ~ year + (1 | country/study), data = trust.agg.dem)
summary(model_trends_parl_agg)

model_trends_gov_agg <- lmer(trust_gov ~ year + (1 | country/study), data = trust.agg.dem)
summary(model_trends_gov_agg)

model_trends_polpar_agg <- lmer(trust_polpar ~ year + (1 | country/study), data = trust.agg.dem)
summary(model_trends_polpar_agg)

model_trends_leg_agg <- lmer(trust_leg ~ year + (1 | country/study), data = trust.agg.dem)
summary(model_trends_leg_agg)

model_trends_police_agg <- lmer(trust_police ~ year + (1 | country/study), data = trust.agg.dem)
summary(model_trends_police_agg)

model_trends_civil_agg <- lmer(trust_civil ~ year + (1 | country/study), data = trust.agg.dem)
summary(model_trends_civil_agg)

model_trends_list_agg <- list(model_trends_parl_agg, model_trends_gov_agg, model_trends_polpar_agg,
                          model_trends_civil_agg, model_trends_leg_agg, model_trends_police_agg)
model_names_agg <- c("Parliament", "Government", "Political parties",
                 "Civil service", "Legal system", "Police")

wordreg(model_trends_list_agg, file = "MLM_Global_agg.doc", custom.model.names = model_names_agg, digits = 2)

## Plot margins
# Colour codes
col_swd = rgb(0, 158, 115, 255, maxColorValue = 255)
col_swd_ci = rgb(0, 158, 115, 100, maxColorValue = 255)
col_parl = rgb(213, 94, 0, 255, maxColorValue = 255)
col_parl_ci = rgb(213, 94, 0, 100, maxColorValue = 255)
col_gov = rgb(220, 135, 0, 255, maxColorValue = 255)
col_gov_ci = rgb(220, 135, 0, 100, maxColorValue = 255)
col_polpar = rgb(230, 159, 0, 255, maxColorValue = 255)
col_polpar_ci = rgb(230, 159, 0, 100, maxColorValue = 255)

col_civil = rgb(0, 158, 115, 255, maxColorValue = 255)
col_civil_ci = rgb(0, 158, 115, 100, maxColorValue = 255)
col_leg = rgb(10, 128, 158, 255, maxColorValue = 255)
col_leg_ci = rgb(10, 128, 158, 100, maxColorValue = 255)
col_police = rgb(19,93,216, 255, maxColorValue = 255)
col_police_ci = rgb(19,93,216, 100, maxColorValue = 255)

col_ap = rgb(0, 255, 0, 255, maxColorValue = 255)
col_eeca = rgb(255, 0, 0, 255, maxColorValue = 255)
col_lac = rgb(255, 165, 0, 255, maxColorValue = 255)
col_mena = rgb(0, 96, 0, 255, maxColorValue = 255)
col_ssa = rgb(156, 136, 71, 255, maxColorValue = 255)
col_wena = rgb(65, 105, 225, 255, maxColorValue = 255)
col_glob = rgb(0, 0, 0, 255, maxColorValue = 255)

col_gov2 = rgb(0, 0, 0, 255, maxColorValue = 255)
col_gov_ci2 = rgb(0, 0, 0, 100, maxColorValue = 255)
col_leg2 = rgb(128, 128, 128, 255, maxColorValue = 255)
col_leg_ci2 = rgb(128, 128, 128, 100, maxColorValue = 255)

col_regions <- c(col_wena, col_eeca, col_lac, col_mena, col_ssa, col_ap)
col_type <- c(col_parl, col_gov, col_polpar, col_civil, col_leg, col_police)
col_type_ci <- c(col_parl_ci, col_gov_ci, col_polpar_ci, col_civil_ci, col_leg_ci, col_police_ci)
col_type2 <- c(col_parl, col_gov2, col_polpar, col_civil, col_leg2, col_police)
col_type2_ci <- c(col_parl_ci, col_gov_ci2, col_polpar_ci, col_civil_ci, col_leg_ci2, col_police_ci)

col_type_ssa <- c(col_parl, col_leg, col_police)
col_type_ssa_ci <- c(col_parl_ci, col_leg_ci, col_police_ci)

# Graphs
plot_trends_parl <- plot_model(model_trends_parl, type = "pred", terms = "year", colors = col_parl) +
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),
                     labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%")) +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  labs(y = "",
       x = "",
       title = "Parliament") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 0.7)) +
  theme(plot.title = element_text(hjust=0.5)) +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.major.y = element_blank())
plot_trends_parl

plot_trends_gov <- plot_model(model_trends_gov, type = "pred", terms = "year", colors = col_gov) +
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),
                     labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%")) +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  labs(y = "",
       x = "",
       title = "Government") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 0.7)) +
  theme(plot.title = element_text(hjust=0.5)) +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.major.y = element_blank())
plot_trends_gov

plot_trends_polpar <- plot_model(model_trends_polpar, type = "pred", terms = "year", colors = col_polpar) +
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),
                     labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%")) +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  labs(y = "",
       x = "",
       title = "Political parties") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 0.7)) +
  theme(plot.title = element_text(hjust=0.5)) +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.major.y = element_blank())
plot_trends_polpar

plot_trends_civil <- plot_model(model_trends_civil, type = "pred", terms = "year", colors = col_civil) +
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),
                     labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%")) +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  labs(y = "",
       x = "",
       title = "Civil service") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 0.7)) +
  theme(plot.title = element_text(hjust=0.5)) +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.major.y = element_blank())
plot_trends_civil

plot_trends_leg <- plot_model(model_trends_leg, type = "pred", terms = "year", colors = col_leg) +
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),
                     labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%")) +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  labs(y = "",
       x = "",
       title = "Legal system") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 0.7)) +
  theme(plot.title = element_text(hjust=0.5)) +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.major.y = element_blank())
plot_trends_leg

plot_trends_police <- plot_model(model_trends_police, type = "pred", terms = "year", colors = col_police) +
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),
                     labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%")) +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  labs(y = "",
       x = "",
       title = "Police") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 0.7)) +
  theme(plot.title = element_text(hjust=0.5)) +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.major.y = element_blank())
plot_trends_police

plot_trends_all <- ggpubr::ggarrange(plotlist = list(plot_trends_parl, plot_trends_gov + rremove("y.text"), plot_trends_polpar + rremove("y.text"),
                                                     plot_trends_civil, plot_trends_leg + rremove("y.text"), plot_trends_police + rremove("y.text")),
                                  ncol = 3, nrow = 2, common.legend = TRUE, legend = "bottom")
plot_trends_all
ggexport(plot_trends_all, filename = "MLMs_ind_global.png", width = 3020, height = 2080, res = 300)

# By region
df_indtrust.dem$regfact <- as.factor(df_indtrust.dem$regpol6)
df_indtrust.dem$regnum <- as.numeric(df_indtrust.dem$regfact)
df_indtrust.dem2$regfact <- as.factor(df_indtrust.dem2$regpol6)
df_indtrust.dem2$regnum <- as.numeric(df_indtrust.dem2$regfact)
reglist <- list(2, 3, 4, 5, 6)
typelist <- list("parl", "gov", "polpar", "leg", "civil", "police")

for (x in reglist) {
  df_indtrust.dem.x <- subset(df_indtrust.dem2, regnum == x)
  
  for (y in typelist) {
    depvar <- paste("trust", y, sep = "_")
    
    tryCatch({
      model_formula <- as.formula(paste(depvar, "~ year + (1 | country/study)"))
      model_trends.y.x <- lmer(model_formula, data = df_indtrust.dem.x, weights=weight)
      assign(paste("model", y, "reg", x, sep = "_"), model_trends.y.x)
      
    }, error = function(e) {
      
      # If an error occurs, run the model with only country as the grouping variable
      # The error is almost certainly because in at least one region/variable combination, there is only one study source
      model_formula <- as.formula(paste(depvar, "~ year + (1 | country)"))
      model_trends.y.x <- lmer(model_formula, data = df_indtrust.dem.x, weights=weight)
      assign(paste("model", y, "reg", x, sep = "_"), model_trends.y.x)
    })
  }
}

# Separately for the WENA region to use data since 1980
df_indtrust.reg_1 <- subset(df_indtrust.dem, regnum == 1)

model_parl_reg_1 <- lmer(trust_parl ~ year + (1 | country/study), data = df_indtrust.reg_1, weights=weight)
model_gov_reg_1 <- lmer(trust_gov ~ year + (1 | country/study), data = df_indtrust.reg_1, weights=weight)
model_polpar_reg_1 <- lmer(trust_polpar ~ year + (1 | country/study), data = df_indtrust.reg_1, weights=weight)
model_leg_reg_1 <- lmer(trust_leg ~ year + (1 | country/study), data = df_indtrust.reg_1, weights=weight)
model_police_reg_1 <- lmer(trust_police ~ year + (1 | country/study), data = df_indtrust.reg_1, weights=weight)
model_civil_reg_1 <- lmer(trust_civil ~ year + (1 | country/study), data = df_indtrust.reg_1, weights=weight)

# Export regression output tables
model_trends_reg_1_list <- list(model_parl_reg_1, model_gov_reg_1, model_polpar_reg_1,
                               model_civil_reg_1, model_leg_reg_1, model_police_reg_1)
model_names_reg_1 <- c("Parliament", "Government", "Political parties",
                 "Civil service", "Legal system", "Police")

wordreg(model_trends_reg_1_list, file = "MLM_reg_1.doc", custom.model.names = model_names_reg_1, digits = 4)

model_trends_reg_2_list <- list(model_parl_reg_2, model_gov_reg_2, model_polpar_reg_2,
                                model_civil_reg_2, model_leg_reg_2, model_police_reg_2)
model_names_reg_2 <- c("Parliament", "Government", "Political parties",
                       "Civil service", "Legal system", "Police")

wordreg(model_trends_reg_2_list, file = "MLM_reg_2.doc", custom.model.names = model_names_reg_2, digits = 4)

model_trends_reg_3_list <- list(model_parl_reg_3, model_gov_reg_3, model_polpar_reg_3,
                                model_civil_reg_3, model_leg_reg_3, model_police_reg_3)
model_names_reg_3 <- c("Parliament", "Government", "Political parties",
                       "Civil service", "Legal system", "Police")

wordreg(model_trends_reg_3_list, file = "MLM_reg_3.doc", custom.model.names = model_names_reg_3, digits = 4)

model_trends_reg_4_list <- list(model_parl_reg_4, model_gov_reg_4, model_polpar_reg_4,
                                model_civil_reg_4, model_leg_reg_4, model_police_reg_4)
model_names_reg_4 <- c("Parliament", "Government", "Political parties",
                       "Civil service", "Legal system", "Police")

wordreg(model_trends_reg_4_list, file = "MLM_reg_4.doc", custom.model.names = model_names_reg_4, digits = 4)

model_trends_reg_5_list <- list(model_parl_reg_5,
                                model_leg_reg_5, model_police_reg_5)
model_names_reg_5 <- c("Parliament", "Legal system", "Police")

wordreg(model_trends_reg_5_list, file = "MLM_reg_5.doc", custom.model.names = model_names_reg_5, digits = 4)

model_trends_reg_6_list <- list(model_parl_reg_6, model_gov_reg_6, model_polpar_reg_6,
                                model_civil_reg_6, model_leg_reg_6, model_police_reg_6)
model_names_reg_6 <- c("Parliament", "Government", "Political parties",
                       "Civil service", "Legal system", "Police")

wordreg(model_trends_reg_6_list, file = "MLM_reg_6.doc", custom.model.names = model_names_reg_6, digits = 4)

# Plot for WENA
gg_parl_1 <- as.data.frame(ggpredict(model_parl_reg_1))
gg_gov_1 <- as.data.frame(ggpredict(model_gov_reg_1))
gg_polpar_1 <- as.data.frame(ggpredict(model_polpar_reg_1))
gg_leg_1 <- as.data.frame(ggpredict(model_leg_reg_1))
gg_civil_1 <- as.data.frame(ggpredict(model_civil_reg_1))
gg_police_1 <- as.data.frame(ggpredict(model_police_reg_1))

# Combine the data from margins and ggeffects
comb_margins_1 <- bind_rows(
  mutate(gg_parl_1, Model = "Parliament"),
  mutate(gg_gov_1, Model = "Government"),
  mutate(gg_polpar_1, Model = "Political parties"),
  mutate(gg_leg_1, Model = "Legal system"),
  mutate(gg_civil_1, Model = "Civil service"),
  mutate(gg_police_1, Model = "Police")
)

# Create a custom plot using ggplot2
viridis_6 <- viridis(6, end=0.9, direction = -1)
comb_margins_1$model_f <- factor(comb_margins_1$Model, levels = c("Parliament", "Government", "Political parties",
                                                        "Civil service", "Legal system", "Police"))

marginsplot_reg_1 <- ggplot(comb_margins_1, aes(x = year.x, y = year.predicted, color = model_f, linetype = model_f)) +
  geom_line(size=1.2) +
  labs(x = "", y = "") +
  theme_minimal() +
  geom_ribbon(aes(ymin = year.conf.low, ymax = year.conf.high, fill = model_f),
              alpha = 0.05, size = 0.5, color = NA, show.legend = FALSE) +
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                     labels = c("0%", "20%", "40%", "60%", "80%", "100%")) +
  scale_x_continuous(breaks = c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  scale_color_manual(values = col_type) +
  scale_fill_manual(values = col_type_ci) +
  labs(y = "",
       x = "",
       linetype = NULL,
       color = NULL,
       title = "Western Europe & North America") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 1)) +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.major.y = element_blank()) +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "22", "42", "44", "13", "1343")),
                              byrow = TRUE)) +
  theme(title = element_text(size = 8))  # Adjust the size as needed
marginsplot_reg_1
ggexport(marginsplot_reg_1, filename = "MLMs_ind_WENA.png", width = 3020, height = 2080, res = 300)

# Plot for EECA
gg_parl_2 <- as.data.frame(ggpredict(model_parl_reg_2))
gg_gov_2 <- as.data.frame(ggpredict(model_gov_reg_2))
gg_polpar_2 <- as.data.frame(ggpredict(model_polpar_reg_2))
gg_leg_2 <- as.data.frame(ggpredict(model_leg_reg_2))
gg_civil_2 <- as.data.frame(ggpredict(model_civil_reg_2))
gg_police_2 <- as.data.frame(ggpredict(model_police_reg_2))

# Combine the data from margins and ggeffects
comb_margins_2 <- bind_rows(
  mutate(gg_parl_2, Model = "Parliament"),
  mutate(gg_gov_2, Model = "Government"),
  mutate(gg_polpar_2, Model = "Political parties"),
  mutate(gg_leg_2, Model = "Legal system"),
  mutate(gg_civil_2, Model = "Civil service"),
  mutate(gg_police_2, Model = "Police")
)

# Create a custom plot using ggplot2
viridis_6 <- viridis(6, end=0.9, direction = -1)
comb_margins_2$model_f <- factor(comb_margins_2$Model, levels = c("Parliament", "Government", "Political parties",
                                                                  "Civil service", "Legal system", "Police"))

marginsplot_reg_2 <- ggplot(comb_margins_2, aes(x = year.x, y = year.predicted, color = model_f, linetype = model_f)) +
  geom_line(size=1.2) +
  labs(x = "", y = "") +
  theme_minimal() +
  geom_ribbon(aes(ymin = year.conf.low, ymax = year.conf.high, fill = model_f),
              alpha = 0.05, size = 0.5, color = NA, show.legend = FALSE) +
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                     labels = c("0%", "20%", "40%", "60%", "80%", "100%")) +
  scale_x_continuous(breaks = c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  scale_color_manual(values = col_type) +
  scale_fill_manual(values = col_type_ci) +
  labs(y = "",
       x = "",
       linetype = NULL,
       color = NULL,
       title = "Eastern Europe & Central Asia") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 1)) +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.major.y = element_blank()) +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "22", "42", "44", "13", "1343")),
                              byrow = TRUE)) +
  theme(title = element_text(size = 8))  # Adjust the size as needed
marginsplot_reg_2
ggexport(marginsplot_reg_2, filename = "MLMs_ind_EECA.png", width = 3020, height = 2080, res = 300)

# Plot for LAC
gg_parl_3 <- as.data.frame(ggpredict(model_parl_reg_3))
gg_gov_3 <- as.data.frame(ggpredict(model_gov_reg_3))
gg_polpar_3 <- as.data.frame(ggpredict(model_polpar_reg_3))
gg_leg_3 <- as.data.frame(ggpredict(model_leg_reg_3))
gg_civil_3 <- as.data.frame(ggpredict(model_civil_reg_3))
gg_police_3 <- as.data.frame(ggpredict(model_police_reg_3))

# Combine the data from margins and ggeffects
comb_margins_3 <- bind_rows(
  mutate(gg_parl_3, Model = "Parliament"),
  mutate(gg_gov_3, Model = "Government"),
  mutate(gg_polpar_3, Model = "Political parties"),
  mutate(gg_leg_3, Model = "Legal system"),
  mutate(gg_civil_3, Model = "Civil service"),
  mutate(gg_police_3, Model = "Police")
)

# Create a custom plot using ggplot2
viridis_6 <- viridis(6, end=0.9, direction = -1)
comb_margins_3$model_f <- factor(comb_margins_3$Model, levels = c("Parliament", "Government", "Political parties",
                                                                  "Civil service", "Legal system", "Police"))

marginsplot_reg_3 <- ggplot(comb_margins_3, aes(x = year.x, y = year.predicted, color = model_f, linetype = model_f)) +
  geom_line(size=1.2) +
  labs(x = "", y = "") +
  theme_minimal() +
  geom_ribbon(aes(ymin = year.conf.low, ymax = year.conf.high, fill = model_f),
              alpha = 0.05, size = 0.5, color = NA, show.legend = FALSE) +
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                     labels = c("0%", "20%", "40%", "60%", "80%", "100%")) +
  scale_x_continuous(breaks = c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  scale_color_manual(values = col_type) +
  scale_fill_manual(values = col_type_ci) +
  labs(y = "",
       x = "",
       linetype = NULL,
       color = NULL,
       title = "Latin America & The Caribbean") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 1)) +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.major.y = element_blank()) +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "22", "42", "44", "13", "1343")),
                              byrow = TRUE)) +
  theme(title = element_text(size = 8))  # Adjust the size as needed
marginsplot_reg_3
ggexport(marginsplot_reg_3, filename = "MLMs_ind_LAC.png", width = 3020, height = 2080, res = 300)

# Plot for MENA
gg_parl_4 <- as.data.frame(ggpredict(model_parl_reg_4))
gg_gov_4 <- as.data.frame(ggpredict(model_gov_reg_4))
gg_polpar_4 <- as.data.frame(ggpredict(model_polpar_reg_4))
gg_leg_4 <- as.data.frame(ggpredict(model_leg_reg_4))
gg_civil_4 <- as.data.frame(ggpredict(model_civil_reg_4))
gg_police_4 <- as.data.frame(ggpredict(model_police_reg_4))

# Combine the data from margins and ggeffects
comb_margins_4 <- bind_rows(
  mutate(gg_parl_4, Model = "Parliament"),
  mutate(gg_gov_4, Model = "Government"),
  mutate(gg_polpar_4, Model = "Political parties"),
  mutate(gg_leg_4, Model = "Legal system"),
  mutate(gg_police_4, Model = "Police")
)

# Create a custom plot using ggplot2
viridis_6 <- viridis(6, end=0.9, direction = -1)
comb_margins_4$model_f <- factor(comb_margins_4$Model, levels = c("Parliament", "Government", "Political parties",
                                                                  "Civil service", "Legal system", "Police"))

marginsplot_reg_4 <- ggplot(comb_margins_4, aes(x = year.x, y = year.predicted, color = model_f, linetype = model_f)) +
  geom_line(size=1.2) +
  labs(x = "", y = "") +
  theme_minimal() +
  geom_ribbon(aes(ymin = year.conf.low, ymax = year.conf.high, fill = model_f),
              alpha = 0.05, size = 0.5, color = NA, show.legend = FALSE) +
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                     labels = c("0%", "20%", "40%", "60%", "80%", "100%")) +
  scale_x_continuous(breaks = c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  scale_color_manual(values = col_type) +
  scale_fill_manual(values = col_type_ci) +
  labs(y = "",
       x = "",
       linetype = NULL,
       color = NULL,
       title = "Middle East & North Africa") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 1)) +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.major.y = element_blank()) +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "22", "42", "44", "13")),
                              byrow = TRUE)) +
  theme(title = element_text(size = 8))  # Adjust the size as needed
marginsplot_reg_4
ggexport(marginsplot_reg_4, filename = "MLMs_ind_MENA.png", width = 3020, height = 2080, res = 300)

# Plot for SSA
gg_parl_5 <- as.data.frame(ggpredict(model_parl_reg_5))
gg_leg_5 <- as.data.frame(ggpredict(model_leg_reg_5))
gg_police_5 <- as.data.frame(ggpredict(model_police_reg_5))

# Combine the data from margins and ggeffects
comb_margins_5 <- bind_rows(
  mutate(gg_parl_5, Model = "Parliament"),
  mutate(gg_leg_5, Model = "Legal system"),
  mutate(gg_police_5, Model = "Police")
)

# Create a custom plot using ggplot2
viridis_4 <- c("#BBDF27FF", "#2E6F8EFF", "#433E85FF", "#440154FF")
comb_margins_5$model_f <- factor(comb_margins_5$Model, levels = c("Parliament", "Legal system", "Police"))

marginsplot_reg_5 <- ggplot(comb_margins_5, aes(x = year.x, y = year.predicted, color = model_f, linetype = model_f)) +
  geom_line(size=1.2) +
  labs(x = "", y = "") +
  theme_minimal() +
  geom_ribbon(aes(ymin = year.conf.low, ymax = year.conf.high, fill = model_f),
              alpha = 0.05, size = 0.5, color = NA, show.legend = FALSE) +
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                     labels = c("0%", "20%", "40%", "60%", "80%", "100%")) +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  scale_color_manual(values = col_type_ssa) +
  scale_fill_manual(values = col_type_ssa_ci) +
  labs(y = "",
       x = "",
       linetype = NULL,
       color = NULL,
       title = "Sub-Saharan Africa") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 1)) +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.major.y = element_blank()) +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "22", "42")),
                              byrow = TRUE)) +
  theme(title = element_text(size = 8))  # Adjust the size as needed
marginsplot_reg_5
ggexport(marginsplot_reg_5, filename = "MLMs_ind_SSA.png", width = 3020, height = 2080, res = 300)

# Plot for APAC
gg_parl_6 <- as.data.frame(ggpredict(model_parl_reg_6))
gg_gov_6 <- as.data.frame(ggpredict(model_gov_reg_6))
gg_polpar_6 <- as.data.frame(ggpredict(model_polpar_reg_6))
gg_leg_6 <- as.data.frame(ggpredict(model_leg_reg_6))
gg_civil_6 <- as.data.frame(ggpredict(model_civil_reg_6))
gg_police_6 <- as.data.frame(ggpredict(model_police_reg_6))

# Combine the data from margins and ggeffects
comb_margins_6 <- bind_rows(
  mutate(gg_parl_6, Model = "Parliament"),
  mutate(gg_gov_6, Model = "Government"),
  mutate(gg_polpar_6, Model = "Political parties"),
  mutate(gg_leg_6, Model = "Legal system"),
  mutate(gg_civil_6, Model = "Civil service"),
  mutate(gg_police_6, Model = "Police")
)

# Create a custom plot using ggplot2
viridis_6 <- viridis(6, end=0.9, direction = -1)
comb_margins_6$model_f <- factor(comb_margins_6$Model, levels = c("Parliament", "Government", "Political parties",
                                                                  "Civil service", "Legal system", "Police"))

marginsplot_reg_6 <- ggplot(comb_margins_6, aes(x = year.x, y = year.predicted, color = model_f, linetype = model_f)) +
  geom_line(size=1.2) +
  labs(x = "", y = "") +
  theme_minimal() +
  geom_ribbon(aes(ymin = year.conf.low, ymax = year.conf.high, fill = model_f),
              alpha = 0.05, size = 0.5, color = NA, show.legend = FALSE) +
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                     labels = c("0%", "20%", "40%", "60%", "80%", "100%")) +
  scale_x_continuous(breaks = c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  scale_color_manual(values = col_type) +
  scale_fill_manual(values = col_type_ci) +
  labs(y = "",
       x = "",
       linetype = NULL,
       color = NULL,
       title = "Asia & The Pacific") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 1)) +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.major.y = element_blank()) +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "22", "42", "44", "13", "1343")),
                              byrow = TRUE)) +
  theme(title = element_text(size = 8))  # Adjust the size as needed
marginsplot_reg_6
ggexport(marginsplot_reg_6, filename = "MLMs_ind_APAC.png", width = 3020, height = 2080, res = 300)

# Combine plots for all six regions
plot_trends_reg_all <- ggpubr::ggarrange(plotlist = list(marginsplot_reg_1, marginsplot_reg_2 + rremove("y.text"), marginsplot_reg_3 + rremove("y.text"),
                                                         marginsplot_reg_4, marginsplot_reg_5 + rremove("y.text"), marginsplot_reg_6 + rremove("y.text")),
                                     ncol = 3, nrow = 2, common.legend = TRUE, legend = "bottom") +
  guides(guide_legend (byrow = TRUE))

plot_trends_reg_all
ggexport(plot_trends_reg_all, filename = "MLMs_ind_reg_all.png", width = 3020, height = 2080, res = 300)

## Models on the aggregate level with random slopes (as well as intercepts) for country and study
trust_agg <- read_dta("C:\\Users\\vikto\\Dropbox\\Datasets\\Combined Int\\Trust Trends\\Trust_trends.dta")

# Include only countries which were liberal or electoral democracies in a majority of years included, according to the VDem Regions of the world measure
trust_agg <- subset(trust_agg, vdem_row_maj == 1)

# Remove observations before 1990
trust_agg <- trust_agg [!trust_agg$year < 1990,]

# Global models with random slopes  - all fail to converge!
model_trends_parl_agg <- lmer(trust_parl ~ year + (year | country/study), data = trust_agg)
summary(model_trends_parl_agg)

model_trends_gov_agg <- lmer(trust_gov ~ year + (year | country/study), data = trust_agg)
summary(model_trends_gov_agg)

model_trends_polpar_agg <- lmer(trust_polpar ~ year + (year | country/study), data = trust_agg)
summary(model_trends_polpar_agg)

model_trends_leg_agg <- lmer(trust_leg ~ year + (year | country/study), data = trust_agg)
summary(model_trends_leg_agg)

model_trends_police_agg <- lmer(trust_police ~ year + (year | country/study), data = trust_agg)
summary(model_trends_police_agg)

model_trends_civil_agg <- lmer(trust_civil ~ year + (year | country/study), data = trust_agg)
summary(model_trends_civil_agg)

model_trends_list_agg <- list(model_trends_parl_agg, model_trends_gov_agg, model_trends_polpar_agg,
                          model_trends_civil_agg, model_trends_leg_agg, model_trends_police_agg)
model_names <- c("Parliament", "Government", "Political parties",
                 "Civil service", "Legal system", "Police")

wordreg(model_trends_list_agg, file = "MLM_Global_agg.doc", custom.model.names = model_names, digits = 4)

## Regression models by country
# Create an empty list to store results
lm_results_list <- list()

# Get unique countries in the dataframe
unique_countries <- unique(df_indtrust.dem2$country)

# List of trust measures / objects
typelist <- c("trust_parl", "trust_gov", "trust_polpar", "trust_civil", "trust_leg", "trust_police")

# Loop through each country
for (country in unique_countries) {
  # Loop through each outcome variable
  for (y in typelist) {
    depvar <- as.formula(paste(y, "~ year + factor(study)"))
    
    # Subset the dataframe for the current country and dependent variable
    subset_df <- df_indtrust.dem2[df_indtrust.dem2$country == country, c("year", "study", y)]
    
    # Exclude rows where the outcome variable is missing
    subset_df <- subset_df[!is.na(subset_df[[y]]), ]
    
    # Check if we have at least 5 years where the measure is included in this country
    if (length(unique(subset_df$year)) >= 5) {
      tryCatch({
        # Fit linear regression models for each outcome
        lm_model <- lm(depvar, data = subset_df)
        
        # Store coefficient and p-value along with the country identifier
        lm_results_list[[paste(country, y, sep = "_")]] <- c(country = country,
                                                             outcome = y,
                                                             coefficient = coef(lm_model)[2],  # Coefficient for 'year'
                                                             p_val = summary(lm_model)$coefficients[2, 4])  # P-value for 'year'
      }, error = function(e) {
        cat("Error fitting model for", country, "and outcome", y, ": ", conditionMessage(e), "\n")
        
        # Try fitting the model without the 'study' factor variable
        tryCatch({
          lm_model_without_study <- lm(depvar, data = subset_df[, c("year", y)])
          
          # Store coefficient and p-value along with the country identifier
          lm_results_list[[paste(country, y, "without_study", sep = "_")]] <- c(country = country,
                                                                                outcome = y,
                                                                                coefficient = coef(lm_model_without_study)[2],  # Coefficient for 'year'
                                                                                p_val = summary(lm_model_without_study)$coefficients[2, 4])  # P-value for 'year'
        }, error = function(e_without_study) {
          cat("Error fitting model without 'study' for", country, "and outcome", y, ": ", conditionMessage(e_without_study), "\n")
        })
      })
    } else {
      # Add country with NA values for coefficient and p-value if fewer than 5 years included
      lm_results_list[[paste(country, y, sep = "_")]] <- c(country = country,
                                                           outcome = y,
                                                           coefficient = NA,
                                                           p_val = NA)
      cat("Adding", country, "with NA values due to fewer than 5 unique values for the variable 'year' in the outcome", y, "\n")
    }
  }
}
lm_results_df <- do.call(rbind, lm_results_list)
lm_results_df <- data.frame(lm_results_df)
View(lm_results_df)

lm_results_df$coefficient.year <- as.numeric(as.character(lm_results_df$coefficient.year))
lm_results_df$coef_cat <- ifelse(lm_results_df$coefficient.year < -0.005, "Below -0.005",
                                             ifelse(lm_results_df$coefficient.year > 0.005, "Above 0.005",
                                                    "Between -0.005 and 0.005"))
lm_results_df$pval_cat <- ifelse(lm_results_df$p_val < 0.05, "Significant",
                                 "Not significant")

lm_results_df$category <- ifelse(lm_results_df$pval_cat == "Not significant", "Not significant",
                                 ifelse(lm_results_df$coef_cat == "Below -0.005", "Decline",
                                        ifelse(lm_results_df$coef_cat == "Above 0.005", "Rise",
                                               "Trendless")))
  
prop.table(table(lm_results_df$category))
prop.table(table(lm_results_df$pval_cat))

table(lm_results_df$pval_cat[lm_results_df$category=="Trendless"])

# Reshape the dataframe to wide format
wide_lm_results_df <- reshape(lm_results_df, idvar = "country", timevar = "outcome", direction = "wide")

prop.table(table(wide_lm_results_df$category.trust_parl))
prop.table(table(wide_lm_results_df$category.trust_gov))
prop.table(table(wide_lm_results_df$category.trust_polpar))
prop.table(table(wide_lm_results_df$category.trust_leg))
prop.table(table(wide_lm_results_df$category.trust_civil))
prop.table(table(wide_lm_results_df$category.trust_police))