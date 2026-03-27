## Patterns between estimates of trends in trust in different institutions
# libraries
# install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
if (!require("pacman")) install.packages("pacman")

p_load(dplyr, mgcv, tidyverse, ggplot2, ggpubr, haven, plyr, lme4, gmodels, stargazer,
       labelled, sjPlot, janitor, funtimes, psych, lavaan, rstatix, ggcorrplot)

# Read and merge Bayesian trends estimates data
df_est_parl <- read.csv("parl_mood_est.csv")
df_est_gov <- read.csv("gov_mood_est.csv")
df_est_polpar <- read.csv("polpar_mood_est.csv")
df_est_civil <- read.csv("civil_mood_est.csv")
df_est_leg <- read.csv("leg_mood_est.csv")
df_est_police <- read.csv("police_mood_est.csv")

df_est_parl <- df_est_parl %>% dplyr::rename(first_yr_parl = First_yr)
df_est_gov <- df_est_gov %>% dplyr::rename(first_yr_gov = First_yr)
df_est_polpar <- df_est_polpar %>% dplyr::rename(first_yr_polpar = First_yr)
df_est_civil <- df_est_civil %>% dplyr::rename(first_yr_civil = First_yr)
df_est_leg <- df_est_leg %>% dplyr::rename(first_yr_leg = First_yr)
df_est_police <- df_est_police %>% dplyr::rename(first_yr_police = First_yr)

formerge <- list (df_est_parl, df_est_gov, df_est_polpar, df_est_civil, df_est_leg, df_est_police)
df_est <- join_all(formerge, by=c("Country", "Year"), type = "full")

write_dta(df_est, "estimates_all.dta")

# Only democracies
trust_source = read_dta("Trust_trends_rep.dta")

trust_source$Country <- trust_source$country
trust_source$Year <- trust_source$year

df_est_all <- join(df_est, trust_source, by=c("Country", "Year"))

df_est_all <- subset(df_est_all, vdem_row_maj == 1)

# Correlations overall and within-country
df_est_all$Parl. <- df_est_all$parl
df_est_all$Gov. <- df_est_all$gov
df_est_all$Pol.Par. <- df_est_all$polpar
df_est_all$Civil <- df_est_all$civil
df_est_all$Leg. <- df_est_all$leg
df_est_all$Police <- df_est_all$police

variable_list <- c("Parl.", "Gov.", "Pol.Par.", "Civil", "Leg.", "Police")
cor_df_all <- df_est_all %>% dplyr::select(all_of(variable_list))
correls <- round(cor(cor_df_all, use = "pairwise.complete.obs"), 2)
correls
p.mat <- cor_pmat(cor_df_all)

plot_cor_all <- ggcorrplot(correls, type = "lower",
                           outline.col = "white",
                           #method = "circle",
                           p.mat = p.mat,
                           insig = "blank",
                           ggtheme = ggplot2::theme_classic,
                           lab = T,
                           colors = c("#E46726", "white", "#6D9EC1"))
plot_cor_all
ggsave("Estimates_cor_all.jpg", plot_cor_all, device="jpg", dpi=500)

# Create centered variables - I'm not managing to do this with a for loop, which is annoying
df_est_all <- df_est_all %>%
  dplyr::group_by(Country) %>%
  dplyr::mutate(parl_mean = mean(parl, na.rm = TRUE)) %>%
  dplyr::ungroup()
df_est_all <- df_est_all %>%
  dplyr::group_by(Country) %>%
  dplyr::mutate(gov_mean = mean(gov, na.rm = TRUE)) %>%
  dplyr::ungroup()
df_est_all <- df_est_all %>%
  dplyr::group_by(Country) %>%
  dplyr::mutate(polpar_mean = mean(polpar, na.rm = TRUE)) %>%
  dplyr::ungroup()
df_est_all <- df_est_all %>%
  dplyr::group_by(Country) %>%
  dplyr::mutate(civil_mean = mean(civil, na.rm = TRUE)) %>%
  dplyr::ungroup()
df_est_all <- df_est_all %>%
  dplyr::group_by(Country) %>%
  dplyr::mutate(leg_mean = mean(leg, na.rm = TRUE)) %>%
  dplyr::ungroup()
df_est_all <- df_est_all %>%
  dplyr::group_by(Country) %>%
  dplyr::mutate(police_mean = mean(police, na.rm = TRUE)) %>%
  dplyr::ungroup()

df_est_all2 <- df_est_all

df_est_all2$Parl. <- df_est_all2$parl - df_est_all2$parl_mean
df_est_all2$Gov. <- df_est_all2$gov - df_est_all2$gov_mean
df_est_all2$Pol.Par. <- df_est_all2$polpar - df_est_all2$polpar_mean
df_est_all2$Civil <- df_est_all2$civil - df_est_all2$civil_mean
df_est_all2$Leg. <- df_est_all2$leg - df_est_all2$leg_mean
df_est_all2$Police <- df_est_all2$police - df_est_all2$police_mean

variable_list_ce <- c("Parl.", "Gov.", "Pol.Par.", "Civil", "Leg.", "Police")

cor_df_all_ce <- df_est_all2 %>% dplyr::select(all_of(variable_list_ce))

new_names <- sub("_w$", "", variable_list_ce)
colnames(cor_df_all_ce) <- new_names

correls_ce <- round(cor(cor_df_all_ce, use = "pairwise.complete.obs"), 2)
correls_ce
p.mat_ce <- cor_pmat(cor_df_all_ce)

plot_cor_all_ce <- ggcorrplot(correls_ce, type = "lower",
                              outline.col = "white",
                              #method = "circle",
                              p.mat = p.mat_ce,
                              insig = "blank",
                              ggtheme = ggplot2::theme_classic,
                              lab = T,
                              colors = c("#E46726", "white", "#6D9EC1"))
plot_cor_all_ce
ggsave("Estimates_cor_within.jpg", plot_cor_all_ce, device="jpg", dpi=500)

# Factors
ev <- eigen(cor(cor_df_all, use="pair"))

ev$values
scree(cor_df_all, pc = F)
paratest <- fa.parallel(cor_df_all, fa="fa")

nfacs <- 2

cordf <- na.omit(cor_df_all)

fit <- fa(cor_df_all, fm="pa", nfactors=nfacs, rotate = "promax")
fa(cor_df_all, fm="pa", nfactors=nfacs, rotate = "promax")
factors_diagram <- fa.diagram(fit, digits=3)
png("factors_diagram.png", width = 1000, height = 900, res = 150) # Adjust width, height, and res as needed
fa.diagram(fit, digits=3)
dev.off()

# Within country
ev_ce <- eigen(cor(cor_df_all_ce, use="pair"))

ev_ce$values
scree(cor_df_all_ce, pc = F)
paratest <- fa.parallel(cor_df_all_ce, fa="fa")

nfacs <- 2

cordf_ce <- na.omit(cor_df_all_ce)

fit_ce <- fa(cor_df_all_ce, fm="pa", nfactors=nfacs, rotate = "promax")
fa(cor_df_all_ce, fm="pa", nfactors=nfacs, rotate = "promax")
factors_diagram_ce <- fa.diagram(fit_ce, digits=3)
png("factors_diagram_ce.png", width = 1000, height = 900, res = 150) # Adjust width, height, and res as needed
fa.diagram(fit_ce, digits=3)
dev.off()

## Confirmatory factor analysis
# One factor
cfa_1 <- 'f1 =~ Parl. + Gov. + Pol.Par. + Civil + Leg. + Police' 
cfa_1_mod <- cfa(cfa_1, data=cor_df_all,std.lv=TRUE) 
summary(cfa_1_mod,fit.measures=TRUE,standardized=TRUE)

# Correlated two factor solution, marker method
cfa_2 <- 'f1 =~ Parl. + Gov. + Pol.Par.
        f2 =~ Civil + Leg. + Police' 
cfa_2_mod <- cfa(cfa_2, data=cor_df_all,std.lv=TRUE) 
summary(cfa_2_mod,fit.measures=TRUE,standardized=TRUE)

# Same for within-country variables
# One factor
cfa_1 <- 'f1 =~ Parl. + Gov. + Pol.Par. + Civil + Leg. + Police' 
cfa_1_mod_ce <- cfa(cfa_1, data=cor_df_all_ce,std.lv=TRUE) 
summary(cfa_1_mod_ce,fit.measures=TRUE,standardized=TRUE)

# Correlated two factor solution, marker method
cfa_2 <- 'f1 =~ Parl. + Gov. + Pol.Par.
        f2 =~ Civil + Leg. + Police' 
cfa_2_mod_ce <- cfa(cfa_2, data=cor_df_all_ce,std.lv=TRUE) 
summary(cfa_2_mod_ce,fit.measures=TRUE,standardized=TRUE)

# By region
df_est_all2_reg1 <- subset(df_est_all2, regpol6 == 1)
df_est_all2_reg2 <- subset(df_est_all2, regpol6 == 2)
df_est_all2_reg3 <- subset(df_est_all2, regpol6 == 3)
df_est_all2_reg4 <- subset(df_est_all2, regpol6 == 4)
df_est_all2_reg5 <- subset(df_est_all2, regpol6 == 5)
df_est_all2_reg6 <- subset(df_est_all2, regpol6 == 6)

cor_df_all_ce_reg1 <- df_est_all2_reg1 %>% dplyr::select(all_of(variable_list_ce))
cor_df_all_ce_reg2 <- df_est_all2_reg2 %>% dplyr::select(all_of(variable_list_ce))
cor_df_all_ce_reg3 <- df_est_all2_reg3 %>% dplyr::select(all_of(variable_list_ce))
cor_df_all_ce_reg4 <- df_est_all2_reg4 %>% dplyr::select(all_of(variable_list_ce))
cor_df_all_ce_reg5 <- df_est_all2_reg5 %>% dplyr::select(all_of(variable_list_ce))
cor_df_all_ce_reg6 <- df_est_all2_reg6 %>% dplyr::select(all_of(variable_list_ce))

# WENA
correls_ce_reg1 <- round(cor(cor_df_all_ce_reg1, use = "pairwise.complete.obs"), 2)
correls_ce_reg1
p.mat_ce_reg1 <- cor_pmat(cor_df_all_ce_reg1)

plot_cor_all_ce_reg1 <- ggcorrplot(correls_ce_reg1, type = "lower",
                              outline.col = "white",
                              #method = "circle",
                              p.mat = p.mat_ce,
                              insig = "blank",
                              ggtheme = ggplot2::theme_classic,
                              lab = T,
                              colors = c("#E46726", "white", "#6D9EC1"))
plot_cor_all_ce_reg1
ggsave("Estimates_cor_wi_reg1.jpg", plot_cor_all_ce_reg1, device="jpg", dpi=500)

ev <- eigen(cor(cor_df_all_ce_reg1, use="pair"))

ev$values
scree(cor_df_all_ce_reg1, pc = F)
paratest <- fa.parallel(cor_df_all_ce_reg1, fa="fa")

# EECA
correls_ce_reg2 <- round(cor(cor_df_all_ce_reg2, use = "pairwise.complete.obs"), 2)
correls_ce_reg2
p.mat_ce_reg2 <- cor_pmat(cor_df_all_ce_reg2)

plot_cor_all_ce_reg2 <- ggcorrplot(correls_ce_reg2, type = "lower",
                                   outline.col = "white",
                                   #method = "circle",
                                   p.mat = p.mat_ce,
                                   insig = "blank",
                                   ggtheme = ggplot2::theme_classic,
                                   lab = T,
                                   colors = c("#E46726", "white", "#6D9EC1"))
plot_cor_all_ce_reg2
ggsave("Estimates_cor_wi_reg2.jpg", plot_cor_all_ce_reg2, device="jpg", dpi=500)

ev <- eigen(cor(cor_df_all_ce_reg2, use="pair"))

ev$values
scree(cor_df_all_ce_reg2, pc = F)
paratest <- fa.parallel(cor_df_all_ce_reg2, fa="fa")

# LAC
correls_ce_reg3 <- round(cor(cor_df_all_ce_reg3, use = "pairwise.complete.obs"), 2)
correls_ce_reg3
p.mat_ce_reg3 <- cor_pmat(cor_df_all_ce_reg3)

plot_cor_all_ce_reg3 <- ggcorrplot(correls_ce_reg3, type = "lower",
                                   outline.col = "white",
                                   #method = "circle",
                                   p.mat = p.mat_ce,
                                   insig = "blank",
                                   ggtheme = ggplot2::theme_classic,
                                   lab = T,
                                   colors = c("#E46726", "white", "#6D9EC1"))
plot_cor_all_ce_reg3
ggsave("Estimates_cor_wi_reg3.jpg", plot_cor_all_ce_reg3, device="jpg", dpi=500)

ev <- eigen(cor(cor_df_all_ce_reg3, use="pair"))

ev$values
scree(cor_df_all_ce_reg3, pc = F)
paratest <- fa.parallel(cor_df_all_ce_reg3, fa="fa")

# MENA
correls_ce_reg4 <- round(cor(cor_df_all_ce_reg4, use = "pairwise.complete.obs"), 2)
correls_ce_reg4
p.mat_ce_reg4 <- cor_pmat(cor_df_all_ce_reg4)

plot_cor_all_ce_reg4 <- ggcorrplot(correls_ce_reg4, type = "lower",
                                   outline.col = "white",
                                   #method = "circle",
                                   p.mat = p.mat_ce,
                                   insig = "blank",
                                   ggtheme = ggplot2::theme_classic,
                                   lab = T,
                                   colors = c("#E46726", "white", "#6D9EC1"))
plot_cor_all_ce_reg4
ggsave("Estimates_cor_wi_reg4.jpg", plot_cor_all_ce_reg4, device="jpg", dpi=500)

ev <- eigen(cor(cor_df_all_ce_reg4, use="pair"))

ev$values
scree(cor_df_all_ce_reg4, pc = F)
paratest <- fa.parallel(cor_df_all_ce_reg4, fa="fa")

# SSA
correls_ce_reg5 <- round(cor(cor_df_all_ce_reg5, use = "pairwise.complete.obs"), 2)
correls_ce_reg5
p.mat_ce_reg5 <- cor_pmat(cor_df_all_ce_reg5)

plot_cor_all_ce_reg5 <- ggcorrplot(correls_ce_reg5, type = "lower",
                                   outline.col = "white",
                                   #method = "circle",
                                   p.mat = p.mat_ce,
                                   insig = "blank",
                                   ggtheme = ggplot2::theme_classic,
                                   lab = T,
                                   colors = c("#E46726", "white", "#6D9EC1"))
plot_cor_all_ce_reg5
ggsave("Estimates_cor_wi_reg5.jpg", plot_cor_all_ce_reg5, device="jpg", dpi=500)

ev <- eigen(cor(cor_df_all_ce_reg5, use="pair"))

ev$values
scree(cor_df_all_ce_reg5, pc = F)
paratest <- fa.parallel(cor_df_all_ce_reg5, fa="fa")

# ASPAC
correls_ce_reg6 <- round(cor(cor_df_all_ce_reg6, use = "pairwise.complete.obs"), 2)
correls_ce_reg6
p.mat_ce_reg6 <- cor_pmat(cor_df_all_ce_reg6)

plot_cor_all_ce_reg6 <- ggcorrplot(correls_ce_reg6, type = "lower",
                                   outline.col = "white",
                                   #method = "circle",
                                   p.mat = p.mat_ce,
                                   insig = "blank",
                                   ggtheme = ggplot2::theme_classic,
                                   lab = T,
                                   colors = c("#E46726", "white", "#6D9EC1"))
plot_cor_all_ce_reg6
ggsave("Estimates_cor_wi_reg6.jpg", plot_cor_all_ce_reg6, device="jpg", dpi=500)

ev <- eigen(cor(cor_df_all_ce_reg6, use="pair"))

ev$values
scree(cor_df_all_ce_reg6, pc = F)
paratest <- fa.parallel(cor_df_all_ce_reg6, fa="fa")

