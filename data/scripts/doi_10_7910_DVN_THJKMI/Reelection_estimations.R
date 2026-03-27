#####################################
########## Adee Weller ##############
## Defending the Status Quo? How Reelection Shapes Criminal Collusion in Mexico
#####################################

# Clear workspace
rm(list = ls())

# Load or install necessary packages
pkgs <- c("tmap", "dplyr", "plm", "fixest", "kableExtra", "stargazer", "ggplot2", "modelsummary", "ggthemes", "did", "bacondecomp", "spdep", "xtable", "broom", "purrr", "lfe", "did2s", "didimputation", "glue", "MASS", "sjPlot", "lmtest", "gridExtra", "readr", "foreign", "stringi", "tidyverse")

for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
  library(pkg, character.only = TRUE)
}

# For user specification. Ensure that subfiles are appropriately set up.
setwd("C:\\Users\\adeew\\OneDrive\\Documents\\Reelect_Ref\\BJPS\\replication\\")

# load data
panel <- read_csv('data\\stacked_data.csv', locale = locale(encoding = "Latin1"))



#########################################
#### Main estimations ###################
#########################################


# criminally competitive municipalities
head(panel)

first_subset <- panel %>%
  filter(Bin == 1, party_alt == 1)

mun_to_keep <- first_subset$ADM2_PCODE

df_alt <- panel %>%
  filter(ADM2_PCODE %in% mun_to_keep)

head(df_alt)

# criminal entrenched
df_alt2 <- subset(df_alt, df_alt$Poppies > 0)

# non entrenchement
df_alt3 <- subset(df_alt, df_alt$Poppies == 0)


### incumbents
poppy_party_inc <- felm(Bin_inc ~ stag_ind_bin + Pres_misalign + population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + state_misalign + Bin:State | Bin + State | 0 | State, data = df_alt2)

summary(poppy_party_inc)

no_poppy_party_inc <- felm(Bin_inc ~ stag_ind_bin + Pres_misalign + population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + state_misalign + Bin:State | Bin+ State | 0 | State, data = df_alt3)

summary(no_poppy_party_inc)

inc <- felm(Bin_inc ~ stag_ind_bin + Pres_misalign + population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + state_misalign + Bin:State | Bin + State | 0 | State, data = panel)

summary(inc)



### challengers
poppy_party_chal <- felm(Bin_chal ~ stag_ind_bin + Pres_misalign + population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + state_misalign + Bin:State | Bin + State | 0 | State, data = df_alt2)

summary(poppy_party_chal)

no_poppy_party_chal <- felm(Bin_chal ~ stag_ind_bin + Pres_misalign + population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + state_misalign + Bin:State | Bin + State | 0 | State, data = df_alt3)

summary(no_poppy_party_chal)

chal <- felm(Bin_chal ~ stag_ind_bin + Pres_misalign + population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + state_misalign + Bin:State | Bin + State | 0 | State, data = panel)

summary(chal)

control <- subset(panel, panel$stag_ind_bin == 0)
control <- subset(control, control$Poppies == 1)

mean(control$Bin_chal)
mean(panel$Bin_chal)

poppy_party_attacks <- felm(Bin_attacks2 ~ stag_ind_bin + Pres_misalign + population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + state_misalign + Bin:State | Bin + State | 0 | State, data = df_alt2)

summary(poppy_party_attacks)

no_poppy_party_attacks <- felm(Bin_attacks2 ~ stag_ind_bin + Pres_misalign + population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + state_misalign + Bin:State | Bin + State | 0 | State, data = df_alt3)

summary(no_poppy_party_attacks)

baseline <- felm(Bin_attacks2 ~ stag_ind_bin + Pres_misalign + population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + state_misalign + Bin:State | Bin + State | 0 | State, data = panel)

summary(baseline)

models2 <- list('All Candidates, CE' = poppy_party_attacks, 'Incumbent, CE' = poppy_party_inc, 'Challengers, CE' = poppy_party_chal, 'All Candidates, not CE' = no_poppy_party_attacks, 'Incumbent, not CE' = no_poppy_party_inc, 'Challengers, not CE' = no_poppy_party_chal)

cm_pa <- c('stag_ind_bin' = 'Mayoral Reelection',
    'Poppies' = 'Criminal Presence',
    'stag_ind_bin:Poppies' = 'Mayoral Reelection × Criminal Presence',
    'stag_ind_bin:party_alt' = 'Mayoral Reelection × Partisan Alternation', 
    'Poppies:party_alt' = 'Criminal Presence × Partisan Alternation', 
    'stag_ind_bin:Poppies:party_alt' = 'Mayoral Reelection × Partisan Alternation × Criminal Presence')

footnote_text <- "Significance levels: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001."

# Table 1
modelsummary(models2, 
  estimate = "{estimate}{stars}",
  output = "latex", 
  coef_map = cm_pa,
  gof_map = c("nobs", "r.squared"),
  title = "Treatment Effect on Attacks in Criminally-Entrenched Areas and non-Entrenched Areas",
  notes = footnote_text
  )



### Graphical output

# helper function
extract_clustered_coef <- function(model, outcome_name, setting, level = 0.95) {
  coef_summary <- summary(model)$coefficients
  est <- coef_summary["stag_ind_bin", "Estimate"]
  se <- coef_summary["stag_ind_bin", "Cluster s.e."]

  crit <- qnorm((1 + level)/2)
  lower <- est - crit * se
  upper <- est + crit * se

  data.frame(
    estimate = est,
    conf.low = lower,
    conf.high = upper,
    Outcome = outcome_name,
    Setting = setting
  )
}


df_coef <- bind_rows(
  extract_clustered_coef(poppy_party_attacks, "All Candidates", "Entrenched"),
  extract_clustered_coef(poppy_party_inc, "Incumbents", "Entrenched"),
  extract_clustered_coef(poppy_party_chal, "Challengers", "Entrenched"),
  extract_clustered_coef(no_poppy_party_attacks, "All Candidates", "Non-Entrenched"),
  extract_clustered_coef(no_poppy_party_inc, "Incumbents", "Non-Entrenched"),
  extract_clustered_coef(no_poppy_party_chal, "Challengers", "Non-Entrenched")
)

# Figure A7
ggplot(df_coef, aes(x = Outcome, y = estimate, color = Setting)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge(width = 0.5),
                width = 0.2) +
  labs(
    y = "Overall ATT",
    x = "Targets of Violence",
    title = "ATT Estimated with Cluster-Robust SEs",
    color = "Setting"
  ) +
  theme_minimal()

# test if challengers coefficient is actually different
b_chal_ce <- coef(poppy_party_chal)["stag_ind_bin"]
vcov_chal_ce <- vcov(poppy_party_chal)["stag_ind_bin", "stag_ind_bin"]

b_chal_noce <- coef(no_poppy_party_chal)["stag_ind_bin"]
vcov_chal_noce <- vcov(no_poppy_party_chal)["stag_ind_bin", "stag_ind_bin"]

diff_chal <- b_chal_ce - b_chal_noce

se_diff_chal <- sqrt(vcov_chal_ce + vcov_chal_noce)

z_chal <- diff_chal / se_diff_chal
p_chal <- 2 * (1 - pnorm(abs(z_chal)))

stars <- function(p) {
  if (p < 0.001) return('***')
  if (p < 0.01)  return('**')
  if (p < 0.05)  return('*')
  if (p < 0.1)   return('+')
  return('')
}

cat(sprintf("Diff (Challengers): CE - Non-CE = \\num{%.3f}%s \\\\\n", diff_chal, stars(p_chal)))
cat(sprintf("SE = \\num{%.3f}, p = %.3f \\\\\n", se_diff_chal, p_chal))

# check for all candidates
compare_felm_coefs <- function(model1, model2, term = "stag_ind_bin") {
  b1 <- coef(model1)[term]
  v1 <- vcov(model1)[term, term]
  b2 <- coef(model2)[term]
  v2 <- vcov(model2)[term, term]

  diff <- b1 - b2
  se <- sqrt(v1 + v2)
  z <- diff / se
  p <- 2 * (1 - pnorm(abs(z)))

  return(data.frame(
    Outcome = NA,
    Estimate = diff,
    SE = se,
    p = p
  ))
}

res_all <- compare_felm_coefs(poppy_party_attacks, no_poppy_party_attacks)
res_inc <- compare_felm_coefs(poppy_party_inc, no_poppy_party_inc)
res_chal <- compare_felm_coefs(poppy_party_chal, no_poppy_party_chal)

res_all$Outcome <- "All Candidates"
res_inc$Outcome <- "Incumbents"
res_chal$Outcome <- "Challengers"

df_diffs <- bind_rows(res_all, res_inc, res_chal) %>%
  dplyr::select(Outcome, Estimate, SE, p)

df_diffs$Stars <- cut(df_diffs$p,
                      breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                      labels = c("***", "**", "*", "+", ""),
                      right = FALSE)

rownames(df_diffs) <- NULL

# Table A7
df_diffs %>%
  mutate(
    Estimate = sprintf("%.3f", Estimate),
    SE = sprintf("(%.3f)", SE),
    p = sprintf("%.3f", p),
    Coefficient = paste0(Estimate, Stars, " ", SE)
  ) %>%
  dplyr::select(Coefficient) %>%
  kable("latex", booktabs = TRUE,
        col.names = c("Difference (Entrenched − Non-Entrenched)"),
        caption = "Difference in ATT Coefficients across Settings by Target Type")




###### Add Esberg Data ###########

panel2 <- panel %>%
  dplyr::filter(Year < 2021)


mx_panel <- read.csv('data\\mx_panel.csv')

crim_groups <- read.csv('data\\criminal_groups.csv')

head(crim_groups)

mean(crim_groups$unique_groups)
mean(crim_groups$splinters)
mean(crim_groups$emergence)

# Note, Esberg data is only from 2013-2020
crim_groups$ADM2_PCODE <- sprintf("%03d", crim_groups$CVE_MUN)

crim_groups$ADM2_PCODE <- as.integer(paste0(crim_groups$CVE_ENT, crim_groups$ADM2_PCODE, sep = ''))

unique(crim_groups$ADM2_PCODE)

setdiff(unique(panel$ADM2_PCODE),unique(crim_groups$ADM2_PCODE))

crim_groups2 <- crim_groups %>%
  dplyr::select(-c("muni_id", "inegi", "ife", "NOM_EST", "NOM_MUN", "CVE_ENT", "CVE_MUN"))

test1 <- left_join(panel2, crim_groups2, by = c("ADM2_PCODE" = "ADM2_PCODE", "Year" = "year"))

mx_panel$ADM2_PCODE <- sprintf("%03d", mx_panel$CVE_MUN)

mx_panel$ADM2_PCODE <- as.integer(paste0(mx_panel$CVE_ENT, mx_panel$ADM2_PCODE, sep = ''))

mx_panel2 <- mx_panel %>%
  dplyr::select(-c("muni_id", "inegi", "ife", "NOM_EST", "NOM_MUN", "CVE_ENT", "CVE_MUN"))

test2 <- left_join(test1, mx_panel2, by = c("ADM2_PCODE" = "ADM2_PCODE", "Year" = "year"))

test2$any_groups <- ifelse(test2$unique_groups > 0, 1, 0)

first_subset1 <- test2 %>%
  filter(Bin == 1, party_alt == 1)

mun_to_keep <- first_subset1$ADM2_PCODE

esberg <- test2 %>%
  filter(ADM2_PCODE %in% mun_to_keep)




# criminal entrenched
poppies_esberg <- subset(esberg, esberg$Poppies > 0)

# non entrenchement
np_esberg <- subset(esberg, esberg$Poppies == 0)

# control for party alternation due to reduced sample
# unique groups
es1 <- felm(unique_groups ~ stag_ind_bin + party_alt+ population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor   | Bin + State | 0 | State, data = poppies_esberg)
summary(es1)

es2 <- felm(unique_groups ~ stag_ind_bin + party_alt+ population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor  | Bin + State | 0 | State, data = np_esberg)
summary(es2)

unique_groups <- list('Unique Groups, CE' = es1, 'Unique Groups' = es2)

# Part of Table A14
modelsummary(unique_groups, 
  estimate = "{estimate}{stars}",
  output = "latex", 
  coef_map = cm_pa,
  gof_map = c("nobs", "r.squared"),
  title = "Treatment Effect on Criminal Organizations in Criminally-Entrenched Areas and non-Entrenched Areas",
  notes = footnote_text
  )


# number of splinter groups
exam1 <- felm(splinters ~ stag_ind_bin  + population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor  + party_alt  | Bin + State | 0 | State, data = poppies_esberg)

exam2 <- felm(splinters ~ stag_ind_bin  + population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor  + party_alt  | Bin + State | 0 | State, data = np_esberg)

mod_list <- list('DV, CE' = exam1, 'DV' = exam2)

# Part of Table A14
modelsummary(mod_list, 
  estimate = "{estimate}{stars}",
  output = "latex", 
  coef_map = cm_pa,
  gof_map = c("nobs", "r.squared"),
  title = "Treatment Effect on Criminal Organizations in Criminally-Entrenched Areas and non-Entrenched Areas",
  notes = footnote_text
  )


# monopoly
poppies_esberg <- poppies_esberg %>%
  mutate(monopoly = ifelse(unique_groups == 1, 1, 0))

np_esberg <- np_esberg %>%
  mutate(monopoly = ifelse(unique_groups == 1, 1, 0))

monopoly1 <- felm(monopoly ~ stag_ind_bin  + population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor  + party_alt  | Bin + State | 0 | State, data = poppies_esberg)
summary(monopoly1)

monopoly2 <- felm(monopoly ~ stag_ind_bin  + population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor  + party_alt  | Bin + State | 0 | State, data = np_esberg)
summary(monopoly2)

mod_list <- list('DV, CE' = monopoly1, 'DV' = monopoly2)

# Part of Table A14
modelsummary(mod_list, 
  estimate = "{estimate}{stars}",
  output = "latex", 
  coef_map = cm_pa,
  gof_map = c("nobs", "r.squared"),
  title = "Treatment Effect on Criminal Organizations in Criminally-Entrenched Areas and non-Entrenched Areas",
  notes = footnote_text
  )


# group emergence
exam1 <- felm(emergence ~ stag_ind_bin  + population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor  + party_alt  | Bin + State | 0 | State, data = poppies_esberg)

exam2 <- felm(emergence ~ stag_ind_bin  + population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor  + party_alt  | Bin + State | 0 | State, data = np_esberg)

mod_list <- list('DV, CE' = exam1, 'DV' = exam2)

# Part of Table A14
modelsummary(mod_list, 
  estimate = "{estimate}{stars}",
  output = "latex", 
  coef_map = cm_pa,
  gof_map = c("nobs", "r.squared"),
  title = "Treatment Effect on Criminal Organizations in Criminally-Entrenched Areas and non-Entrenched Areas",
  notes = footnote_text
  )





####### Other indicators of governance quality

encig_2023 <- read.csv('data//encig_2023_data.csv', encoding = "windows-1252")

# same as 2023
encig_2021 <- read.csv('data//encig_2023_data.csv', encoding = "windows-1252")

encig_2019 <- read_csv("data/encig_2019_data.csv", locale = locale(encoding = "windows-1252"))

encig_2017 <- read.csv('data//encig_2017_data.csv', encoding = "windows-1252")

encig_2015 <- read_csv("data/encig_2015_data.csv", locale = locale(encoding = "windows-1252"))

encig_2013 <- read.dbf('data//Encig2013_01_viv_sec_1_3_4_5_8_9.DBF', as.is = TRUE)

# verify encoding
encig_2013[] <- lapply(encig_2013, function(col) {
  if (is.character(col)) iconv(col, from = "latin1", to = "windows-1252") else col
})


# syncronize names and rbind with a year indicator
standardize_data <- function(df, year, rename_map) {
  df %>%
    rename(any_of(rename_map)) %>%
    mutate(year = year) %>%
    mutate(across(c(state_code, mun_code, corrup_muni, corrup_police, water_safe,
                  street_lighting, trash_collection, satisfaction_police),
                ~ as.numeric(.))) %>%
    dplyr::select(year, state_code, mun_code, corrup_muni, corrup_police, 
           water_safe, street_lighting, trash_collection, satisfaction_police)
}

rename_2013 <- c(
  state_code = "ENT",
  mun_code = "MUN",
  corrup_muni = "P3_3_8",
  corrup_police = "P3_3_3",
  water_safe = "P4_1_3",
  street_lighting = "P4_2_1",
  trash_collection = "P4_4_2",
  satisfaction_police = "P4_5A"
)

rename_2015_2019 <- c(
  state_code = "ENT",
  mun_code = "MUN",
  corrup_muni = "P3_3_8",
  corrup_police = "P3_3_2",
  water_safe = "P4_1_3",
  street_lighting = "P4_3_1",
  trash_collection = "P4_5_2",
  satisfaction_police = "P4_6A"
)

rename_2021_2023 <- c(
  state_code = "CVE_ENT",
  mun_code = "CVE_MUN",
  corrup_muni = "P3_3_08",
  corrup_police = "P3_3_02",
  water_safe = "P4_1_3",
  street_lighting = "P4_3_1",
  trash_collection = "P4_5_2",
  satisfaction_police = "P4_6A"
)

df_2013_clean <- standardize_data(encig_2013, 2013, rename_2013)
df_2015_clean <- standardize_data(encig_2015, 2015, rename_2015_2019)
df_2017_clean <- standardize_data(encig_2017, 2017, rename_2015_2019)
df_2019_clean <- standardize_data(encig_2019, 2019, rename_2015_2019)
df_2021_clean <- standardize_data(encig_2021, 2021, rename_2021_2023)
df_2023_clean <- standardize_data(encig_2023, 2023, rename_2021_2023)

table(df_2023_clean$state_code)

encig_all <- bind_rows(df_2013_clean, df_2015_clean, df_2017_clean, 
                    df_2019_clean, df_2021_clean, df_2023_clean)

# get averages in each municipality per year
encig_muni_year <- encig_all %>%
  group_by(state_code, mun_code, year) %>%
  summarise(
    corrup_muni = mean(corrup_muni, na.rm = TRUE),
    corrup_police = mean(corrup_police, na.rm = TRUE),
    water_safe = mean(water_safe, na.rm = TRUE),
    street_lighting = mean(street_lighting, na.rm = TRUE),
    trash_collection = mean(trash_collection, na.rm = TRUE),
    satisfaction_police = mean(satisfaction_police, na.rm = TRUE),
    .groups = "drop"
  )

# format AD2_PCODE:
encig_muni_year$ADM2_PCODE <- sprintf("%03d", encig_muni_year$mun_code)

encig_muni_year$ADM2_PCODE <- as.integer(paste0(encig_muni_year$state_code, encig_muni_year$ADM2_PCODE, sep = ''))

# merge into panel:
encig_muni_year %>%
  count(ADM2_PCODE, year) %>%
  filter(n > 1)
# should be 0

test_merge <- left_join(panel, encig_muni_year, by = c("ADM2_PCODE", "Year" = "year"))

# criminal entrenched
poppies_panel3 <- subset(test_merge, test_merge$Poppies > 0)

# non entrenchement
np_panel3 <- subset(test_merge, test_merge$Poppies == 0)


####### Policing Quality: "corrup_muni", "corrup_police", "satisfaction_police"

pretreat_poppies_panel3 <- subset(poppies_panel3, poppies_panel3$Year < 2018)
pretreat_np_panel3 <- subset(np_panel3, np_panel3$Year < 2018)

mean(pretreat_poppies_panel3$corrup_muni, na.rm = TRUE)
mean(pretreat_np_panel3$corrup_muni, na.rm = TRUE)


corrup_muni1 <- felm(corrup_muni ~ stag_ind_bin + party_alt+ population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + Pres_misalign  + state_misalign  | Bin + State | 0 | State, data = poppies_panel3)
summary(corrup_muni1)

corrup_muni2 <- felm(corrup_muni ~ stag_ind_bin + party_alt+ population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + Pres_misalign  + state_misalign  | Bin + State | 0 | State, data = np_panel3)
summary(corrup_muni2)

corrup_muni3 <- felm(corrup_muni ~ stag_ind_bin + MajorHighway + population  + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + Pres_misalign  + state_misalign | Bin + State | 0 | State, data = test_merge)
summary(corrup_muni3)

corrup_muni_m <- list('CE' = corrup_muni1, 'Non CE' = corrup_muni2, 'National' = corrup_muni3)

modelsummary(corrup_muni_m, 
  estimate = "{estimate}{stars}",
  output = "latex", 
  coef_map = cm_pa,
  gof_map = c("nobs", "r.squared"),
  title = "Treatment Effect on Corruption Measures in Criminally-Entrenched Areas and non-Entrenched Areas",
  notes = footnote_text
  )

mean(pretreat_poppies_panel3$corrup_police, na.rm = TRUE)
mean(pretreat_np_panel3$corrup_police, na.rm = TRUE)


corrup_police1 <- felm(corrup_police ~ stag_ind_bin + party_alt+ population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + Pres_misalign  + state_misalign  | Bin + State | 0 | State, data = poppies_panel3)
summary(corrup_police1)

corrup_police2 <- felm(corrup_police ~ stag_ind_bin + party_alt+ population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + Pres_misalign  + state_misalign  | Bin + State | 0 | State, data = np_panel3)
summary(corrup_police2)

corrup_police3 <- felm(corrup_police ~ stag_ind_bin + MajorHighway + population  + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + Pres_misalign  + state_misalign | Bin + State | 0 | State, data = test_merge)
summary(corrup_police3)


corrup_police_m <- list('CE' = corrup_police1, 'Non CE' = corrup_police2, 'National' = corrup_police3)

modelsummary(corrup_police_m, 
  estimate = "{estimate}{stars}",
  output = "latex", 
  coef_map = cm_pa,
  gof_map = c("nobs", "r.squared"),
  title = "Treatment Effect on Corruption Measures in Criminally-Entrenched Areas and non-Entrenched Areas",
  notes = footnote_text
  )

mean(pretreat_poppies_panel3$satisfaction_police, na.rm = TRUE)
mean(pretreat_np_panel3$satisfaction_police, na.rm = TRUE)


satisfaction_police1 <- felm(satisfaction_police ~ stag_ind_bin + party_alt+ population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + Pres_misalign  + state_misalign  | Bin + State | 0 | State, data = poppies_panel3)
summary(satisfaction_police1)

satisfaction_police2 <- felm(satisfaction_police ~ stag_ind_bin + party_alt+ population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + Pres_misalign  + state_misalign   | Bin + State | 0 | State, data = np_panel3)
summary(satisfaction_police2)

satisfaction_police3 <- felm(satisfaction_police ~ stag_ind_bin + MajorHighway + population  + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + Pres_misalign  + state_misalign | Bin + State | 0 | State, data = test_merge)
summary(satisfaction_police3)
    
satisfaction_police_m <- list('CE' = satisfaction_police1, 'Non CE' = satisfaction_police2, 'National' = satisfaction_police3)

modelsummary(satisfaction_police_m, 
  estimate = "{estimate}{stars}",
  output = "latex", 
  coef_map = cm_pa,
  gof_map = c("nobs", "r.squared"),
  title = "Treatment Effect on Corruption Measures in Criminally-Entrenched Areas and non-Entrenched Areas",
  notes = footnote_text
  )

governance_m <- list('Municipal Corruption' = corrup_muni3, 'Police Corruption' = corrup_police3, 'Police Satisfaction' = satisfaction_police3)

# Table A13
modelsummary(governance_m, 
  estimate = "{estimate}{stars}",
  output = "latex", 
  coef_map = cm_pa,
  gof_map = c("nobs", "r.squared"),
  title = "Treatment Effect on Corruption Measures",
  notes = footnote_text
  )

####### Other Governance Quality: "water_safe" , "street_lighting",  "trash_collection" 

mean(pretreat_poppies_panel3$water_safe, na.rm = TRUE)
mean(pretreat_np_panel3$water_safe, na.rm = TRUE)

water_safe1 <- felm(water_safe ~ stag_ind_bin + party_alt+ population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + Pres_misalign  + state_misalign  | Bin + State | 0 | State, data = poppies_panel3)
summary(water_safe1)

water_safe2 <- felm(water_safe ~ stag_ind_bin + party_alt+ population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor  | Bin + State | 0 | State, data = np_panel3)
summary(water_safe2)

water_safe3 <- felm(water_safe ~ stag_ind_bin + MajorHighway + population  + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + Pres_misalign  + state_misalign | Bin + State | 0 | State, data = test_merge)
summary(water_safe3)

water_safe_m <- list('CE' = water_safe1, 'Non CE' = water_safe2, 'National' = water_safe3)

modelsummary(water_safe_m, 
  estimate = "{estimate}{stars}",
  output = "latex", 
  coef_map = cm_pa,
  gof_map = c("nobs", "r.squared"),
  title = "Treatment Effect on Corruption Measures in Criminally-Entrenched Areas and non-Entrenched Areas",
  notes = footnote_text
  )


mean(pretreat_poppies_panel3$street_lighting, na.rm = TRUE)
mean(pretreat_np_panel3$street_lighting, na.rm = TRUE)


street_lighting1 <- felm(street_lighting ~ stag_ind_bin + party_alt+ population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + Pres_misalign  + state_misalign  | Bin + State | 0 | State, data = poppies_panel3)
summary(street_lighting1)

street_lighting2 <- felm(street_lighting ~ stag_ind_bin + party_alt+ population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor  | Bin + State | 0 | State, data = np_panel3)
summary(street_lighting2)

street_lighting3 <- felm(street_lighting ~ stag_ind_bin + MajorHighway + population  + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + Pres_misalign  + state_misalign | Bin + State | 0 | State, data = test_merge)
summary(street_lighting3)

street_lighting_m <- list('CE' = street_lighting1, 'Non CE' = street_lighting2, 'National' = street_lighting3)

modelsummary(street_lighting_m, 
  estimate = "{estimate}{stars}",
  output = "latex", 
  coef_map = cm_pa,
  gof_map = c("nobs", "r.squared"),
  title = "Treatment Effect on Corruption Measures in Criminally-Entrenched Areas and non-Entrenched Areas",
  notes = footnote_text
  )


mean(pretreat_poppies_panel3$trash_collection, na.rm = TRUE)
mean(pretreat_np_panel3$trash_collection, na.rm = TRUE)


trash_collection1 <- felm(trash_collection ~ stag_ind_bin + party_alt+ population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + Pres_misalign  + state_misalign  | Bin + State | 0 | State, data = poppies_panel3)
summary(trash_collection1)

trash_collection2 <- felm(trash_collection ~ stag_ind_bin + party_alt+ population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor  | Bin + State | 0 | State, data = np_panel3)
summary(trash_collection2)


trash_collection3 <- felm(trash_collection ~ stag_ind_bin + MajorHighway + population  + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + Pres_misalign  + state_misalign | Bin + State | 0 | State, data = test_merge)
summary(trash_collection3)

trash_collection_m <- list('CE' = trash_collection1, 'Non CE' = trash_collection2, 'National' = trash_collection3)

modelsummary(trash_collection_m, 
  estimate = "{estimate}{stars}",
  output = "latex", 
  coef_map = cm_pa,
  gof_map = c("nobs", "r.squared"),
  title = "Treatment Effect on Corruption Measures in Criminally-Entrenched Areas and non-Entrenched Areas",
  notes = footnote_text
  )

governance2_m <- list('Water Drinkability' = water_safe3, 'Street Lighting' = street_lighting3, 'Trash Collection' = trash_collection3)

# Table A12
modelsummary(governance2_m, 
  estimate = "{estimate}{stars}",
  output = "latex", 
  coef_map = cm_pa,
  gof_map = c("nobs", "r.squared"),
  title = "Treatment Effect on Governance Quality",
  notes = footnote_text
  )



### DiDiD estimation ######


### incumbents
inc <- felm(Bin_inc ~ stag_ind_bin*Poppies  + Pres_misalign + population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + state_misalign + party_alt | Bin + State | 0 | State, data = panel)
   
summary(inc)

chal <- felm(Bin_chal ~ stag_ind_bin*Poppies + Pres_misalign + population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + state_misalign + party_alt | Bin + State | 0 | State, data = panel)

summary(chal)

baseline <- felm(Bin_attacks2 ~ stag_ind_bin*Poppies + Pres_misalign + population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + state_misalign + party_alt | Bin + State | 0 | State, data = panel)

summary(baseline)

models_je <- list("Total Attacks" = baseline, "Incumbents" = inc, "Challengers" = chal)

cm_pa <- c('stag_ind_bin' = 'Mayoral Reelection',
    'Poppies' = 'Criminal Presence',
    'stag_ind_bin:Poppies' = 'Mayoral Reelection × Criminal Presence',
    'stag_ind_bin:party_alt' = 'Mayoral Reelection × Partisan Alternation', 
    'Poppies:party_alt' = 'Criminal Presence × Partisan Alternation', 
    'stag_ind_bin:Poppies:party_alt' = 'Mayoral Reelection × Partisan Alternation × Criminal Presence')

footnote_text <- "Significance levels: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001."

gof_custom <- tibble(
  raw = c("nobs", "r.squared"),
  clean = c("Stacked Obs.", "R-squared")
)

gof_custom$fmt <- list(
  function(x) sprintf("\\num{%d}", x),
  function(x) sprintf("%.3f", x)
)

unique_obs <- length(unique(panel$ADM2_PCODE))

# Table A8
modelsummary(models_je, 
  estimate = "{estimate}{stars}",
  output = "latex", 
  coef_map = cm_pa,
  gof_map = gof_custom,
  add_rows = data.frame(
    term = "Unique Obs.",
    model1 = sprintf("%d", unique_obs),
    model2 = sprintf("%d", unique_obs),
    model3 = sprintf("%d", unique_obs)
  ),
  title = "Treatment Effect on Attacks Against Politicians",
  notes = footnote_text
)

# test if coefs of stag_bin:Poppies =/= stag_bin?
coefs <- coef(baseline)
vcov_mat <- vcov(baseline)

# Difference of coefficients
diff <- coefs["stag_ind_bin:Poppies"] - coefs["stag_ind_bin"]

# Standard error of the difference
se_diff <- sqrt(
  vcov_mat["stag_ind_bin:Poppies", "stag_ind_bin:Poppies"] +
  vcov_mat["stag_ind_bin", "stag_ind_bin"] -
  2 * vcov_mat["stag_ind_bin:Poppies", "stag_ind_bin"]
)

# z statistic and p-value
z_stat <- diff / se_diff
p_value <- 2 * (1 - pnorm(abs(z_stat)))


test_coef_diff <- function(model, coef1, coef2) {
  b <- coef(model)
  V <- vcov(model)
  diff <- b[coef2] - b[coef1]
  se <- sqrt(V[coef1, coef1] + V[coef2, coef2] - 2 * V[coef1, coef2])
  z <- diff / se
  p <- 2 * (1 - pnorm(abs(z)))
  list(diff = diff, se = se, z = z, p = p)
}

result <- test_coef_diff(baseline, "stag_ind_bin", "stag_ind_bin:Poppies")

stars <- function(p) {
  if (p < 0.001) return('***')
  if (p < 0.01)  return('**')
  if (p < 0.05)  return('*')
  if (p < 0.1)   return('+')
  return('')
}

cat(sprintf("Diff: Criminal - Not & \\num{%.3f}%s & & \\\\\n", result$diff, stars(result$p)))
cat(sprintf(" & (\\num{%.3f}) \\\\\n", result$se))

result2 <- test_coef_diff(inc, "stag_ind_bin", "stag_ind_bin:Poppies")

result3 <- test_coef_diff(chal, "stag_ind_bin", "stag_ind_bin:Poppies")


# Part of Table A8
cat(sprintf("Diff: Criminal - Not & & \\num{%.3f}%s & \\\\\n", result2$diff, stars(result2$p)))
cat(sprintf(" & (\\num{%.3f}) \\\\\n", result2$se))


cat(sprintf("Diff: Criminal - Not & \\num{%.3f}%s \\\\\n", result3$diff, stars(result3$p)))
cat(sprintf(" & (\\num{%.3f}) \\\\\n", result3$se))


# manually add the unique N -- not stacked N
num_obs <- length(unique(panel$ADM2_PCODE))

# Part of Table A8
cat(sprintf("Unique Obs. & %d & %d & %d \\\\\n", num_obs, num_obs, num_obs))


# mean rate of attacks in high-crime and low-crime groups
total_control <- subset(panel, panel$Poppies == 0)
total_pop <- subset(panel, panel$Poppies == 1)

mean(panel$Bin_attacks2, na.rm = TRUE)

mean(total_control$Bin_attacks2, na.rm = TRUE)
mean(total_pop$Bin_attacks2, na.rm = TRUE)


###############################################
### Callaway and Sant'Anna Estimator ##########
###############################################

# load non-stacked panel data
year_p <- read.csv("data/annual_data.csv")

year_p$id_number <- seq(1,length(year_p$Year),1)

out <- att_gt(yname = "Attack_count",
              gname = "treat_stag",
              idname = "id_number",
              tname = "Year",
              xformla = ~homicidio,
              data = year_p,
              est_method = "reg",
              allow_unbalanced_panel = TRUE,
              clustervars = "State"
              )
# this will drop 135 observations due to missingness

out_simple <- aggte(out, type = "simple")
out_es <- aggte(out, type = "dynamic")

out2 <- att_gt(yname = "Attack_inc",
              gname = "treat_stag",
              idname = "id_number",
              tname = "Year",
              xformla = ~homicidio,
              data = year_p,
              est_method = "reg",
              allow_unbalanced_panel = TRUE,
              clustervars = "State"
              )

out_simple2 <- aggte(out2, type = "simple")
out_es2 <- aggte(out2, type = "dynamic")

out3 <- att_gt(yname = "Attack_chal",
              gname = "treat_stag",
              idname = "id_number",
              tname = "Year",
              xformla = ~homicidio,
              data = year_p,
              est_method = "reg",
              allow_unbalanced_panel = TRUE,
              clustervars = "State"
              )

out_simple3 <- aggte(out3, type = "simple")
out_es3 <- aggte(out3, type = "dynamic")

###### Compare with places that have poppies
year_p2 <- subset(year_p, year_p$Poppies == 1)

dim(year_p)
dim(year_p2)

out_p <- att_gt(yname = "Attack_count",
              gname = "treat_stag",
              idname = "id_number",
              tname = "Year",
              xformla = ~homicidio,
              data = year_p2,
              est_method = "reg",
              allow_unbalanced_panel = TRUE,
              clustervars = "State",
              control_group="notyettreated"
              )
# this will drop 45 observations due to missingness

out_simple_p <- aggte(out_p, type = "simple")
out_es_p <- aggte(out_p, type = "dynamic")

# Figure A6
ggdid(out_es_p)

out2_p <- att_gt(yname = "Attack_inc",
              gname = "treat_stag",
              idname = "id_number",
              tname = "Year",
              xformla = ~homicidio,
              data = year_p2,
              est_method = "reg",
              allow_unbalanced_panel = TRUE,
              clustervars = "State",
              control_group="notyettreated"
              )

out_simple2_p <- aggte(out2_p, type = "simple")
out_es2 <- aggte(out2_p, type = "dynamic")

out3_p <- att_gt(yname = "Attack_chal",
              gname = "treat_stag",
              idname = "id_number",
              tname = "Year",
              xformla = ~homicidio,
              data = year_p2,
              est_method = "reg",
              allow_unbalanced_panel = TRUE,
              clustervars = "State",
              control_group="notyettreated"
              )

out_simple3_p <- aggte(out3_p, type = "simple")
out_es3_p <- aggte(out3_p, type = "dynamic")

# no poppies
year_p3 <- subset(year_p, year_p$Poppies ==0)

out_p3 <- att_gt(yname = "Attack_count",
              gname = "treat_stag",
              idname = "id_number",
              tname = "Year",
              xformla = ~homicidio,
              data = year_p3,
              est_method = "reg",
              allow_unbalanced_panel = TRUE,
              clustervars = "State",
              control_group="notyettreated"
              )
# this will drop 90 observations due to missingness

out_es_p3 <- aggte(out_p3, type = "dynamic")
out_simple_p3 <- aggte(out_p3, type = "simple")

# Figure A5
ggdid(out_es_p3)

out2_p3 <- att_gt(yname = "Attack_inc",
              gname = "treat_stag",
              idname = "id_number",
              tname = "Year",
              xformla = ~homicidio,
              data = year_p3,
              est_method = "reg",
              allow_unbalanced_panel = TRUE,
              clustervars = "State",
              control_group="notyettreated"
              )

out_es2_p3 <- aggte(out2_p3, type = "dynamic")
out_simple2_p3 <- aggte(out2_p3, type = "simple")



out3_p3 <- att_gt(yname = "Attack_chal",
              gname = "treat_stag",
              idname = "id_number",
              tname = "Year",
              xformla = ~homicidio,
              data = year_p3,
              est_method = "reg",
              allow_unbalanced_panel = TRUE,
              clustervars = "State",
              control_group="notyettreated"
              )

out_es3_p3 <- aggte(out3_p3, type = "dynamic")
out_simple3_p3 <- aggte(out3_p3, type = "simple")




extract_att_gt <- function(att_gt_obj, setting, dv) {
  data.frame(
    time = att_gt_obj$t,
    estimate = att_gt_obj$att,
    se = att_gt_obj$se,
    lower = att_gt_obj$att - 1.96 * att_gt_obj$se,
    upper = att_gt_obj$att + 1.96 * att_gt_obj$se,
    setting = setting,
    dv = dv
  )
}


att_gt_df <- bind_rows(
  extract_att_gt(out_p3, "Non-Entrenched", "Total"),
  extract_att_gt(out2_p3, "Non-Entrenched", "Incumbents"),
  extract_att_gt(out3_p3, "Non-Entrenched", "Challengers"),
  extract_att_gt(out_p, "Entrenched", "Total"),
  extract_att_gt(out2_p, "Entrenched", "Incumbents"),
  extract_att_gt(out3_p, "Entrenched", "Challengers"),
)

extract_aggte <- function(aggte_obj, setting, dv) {
  data.frame(
    estimate = aggte_obj$overall.att,
    se = aggte_obj$overall.se,
    lower = aggte_obj$overall.att - 1.96 * aggte_obj$overall.se,
    upper = aggte_obj$overall.att + 1.96 * aggte_obj$overall.se,
    setting = setting,
    dv = dv
  )
}

aggte_df <- bind_rows(
  extract_aggte(out_simple_p3, "Non-Entrenched", "Total"),
  extract_aggte(out_simple2_p3, "Non-Entrenched", "Incumbents"),
  extract_aggte(out_simple3_p3, "Non-Entrenched", "Challengers"),
  extract_aggte(out_simple_p, "Entrenched", "Total"),
  extract_aggte(out_simple2_p, "Entrenched", "Incumbents"),
  extract_aggte(out_simple3_p, "Entrenched", "Challengers"),
)


aggte_diff_test <- aggte_df %>%
  group_by(dv) %>%
  summarise(
    diff = diff(estimate), 
    se_diff = sqrt(sum(se^2)),  # Approximate SE for the difference
    p_value = 2 * (1 - pnorm(abs(diff / se_diff)))  # Two-tailed test
  )

# Combine data for a summary table
summary_table <- aggte_df %>%
  tidyr::pivot_wider(
    names_from = setting,
    values_from = c(estimate, se, lower, upper),
    names_sep = "_"
  )

# Add p-values from the difference test
summary_table <- summary_table %>%
  left_join(aggte_diff_test, by = "dv")

# reorder and force factor
aggte_df <- aggte_df %>%
  mutate(dv = factor(dv, levels = c("Total", "Incumbents", "Challengers")))

# Figure A8
ggplot(aggte_df, aes(x = dv, y = estimate, color = setting)) +
  geom_point(position = position_dodge(0.5), size = 3) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper), 
    width = 0.2, 
    position = position_dodge(0.5)
  ) +
  labs(
    x = "Outcome",
    y = "Overall ATT",
    color = "Setting"
  ) +
  theme_minimal()




# Subset for municipalities without criminal presence
df_no_poppies <- subset(df_alt, df_alt$Poppies == 0)


baseline2 <- felm(Bin_attacks2 ~ stag_ind_bin + Pres_misalign + population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + state_misalign + Bin:Year + Bin:State | Bin + State | 0 | State, data = df_alt2)

summary(baseline2)

baseline3 <- felm(Bin_attacks2 ~ stag_ind_bin + Pres_misalign + population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + state_misalign + Bin:Year + Bin:State | Bin + State | 0 | State, data = df_no_poppies)

summary(baseline3)



### Incumbents
no_poppy_inc <- felm(Bin_inc ~ stag_ind_bin + Pres_misalign + population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + state_misalign + Bin:Year + Bin:State | Bin + State | 0 | State, data = df_no_poppies)
summary(no_poppy_inc)

### Challengers
no_poppy_chal <- felm(Bin_chal ~ stag_ind_bin + Pres_misalign + population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + state_misalign + Bin:Year + Bin:State | Bin + State | 0 | State, data = df_no_poppies)
summary(no_poppy_chal)

# Combine all models into a list
models3 <- list('Baseline' = baseline2, 'Incumbent' = poppy_party_inc, 'Challenger' = poppy_party_chal, 'No Poppies' = baseline3, 'No Poppy Incumbent' = no_poppy_inc, 'No Poppy Challenger' = no_poppy_chal)

# Updating the coefficient map
cm_pa2 <- c('stag_ind_bin' = 'Mayoral Reelection',
           'Poppies' = 'Criminal Presence',
           'stag_ind_bin:Poppies' = 'Mayoral Reelection × Criminal Presence',
           'party_alt' = 'Partisan Alternation',
           'stag_ind_bin:party_alt' = 'Mayoral Reelection × Partisan Alternation',
           'Poppies:party_alt' = 'Criminal Presence × Partisan Alternation',
           'stag_ind_bin:Poppies:party_alt' = 'Mayoral Reelection × Partisan Alternation × Criminal Presence')

# Model summary
modelsummary(models3, 
             estimate = "{estimate}{stars}",
             output = "latex", 
             coef_map = cm_pa2,
             gof_map = c("nobs", "r.squared"),
             title = "Treatment Effect on Attacks (Poppies and Party Alternation (2012) Subset)",
             notes = footnote_text)



####################################
### FISM Audit Selection ###########
####################################

# in non poppy
corrupt_did <- felm(if_audit ~ stag_ind_bin + Pres_misalign + population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + state_misalign + Bin:Year + Bin:State | Bin + State | 0 | State, data = df_alt3)
summary(corrupt_did)

## in poppy places
poppy_party_alt_corrupt <- felm(if_audit ~ stag_ind_bin + Pres_misalign + population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + state_misalign + Bin:Year + Bin:State | Bin + State | 0 | State, data = df_alt2)
summary(poppy_party_alt_corrupt)

models_audit <- list('Criminally-Entrenched Areas' = poppy_party_alt_corrupt, 'Non-Entrenched Areas' = corrupt_did)

# Table 2
modelsummary(models_audit, 
  estimate = "{estimate}{stars}",
  output = "latex", 
  coef_map = cm_pa,
  gof_map = c("nobs", "r.squared"),
  title = "Treatment Effect on Audit Selection",
  notes = footnote_text
  )




####################################
### FISM Audit Outcomes ############
####################################

df_alt4 <- subset(df_alt3, df_alt3$if_audit == 1)
df_alt5 <- subset(df_alt2, df_alt2$if_audit == 1)


# no poppy
audit_f_did <- felm(Audit_found ~ stag_ind_bin + Pres_misalign + population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + state_misalign | Bin + State | 0 | State, data = df_alt4)
summary(audit_f_did)


## in poppy places
audit_f_alt <- felm(Audit_found ~ stag_ind_bin + Pres_misalign + population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + state_misalign | Bin + State | 0 | State, data = df_alt5)
summary(audit_f_alt)


models_audit_find <- list('Criminally-Entrenched Areas' = audit_f_alt, 'Non-Entrenched Areas' =	audit_f_did)

# Table 3
modelsummary(models_audit_find, 
  estimate = "{estimate}{stars}",
  output = "latex", 
  coef_map = cm_pa,
  gof_map = c("nobs", "r.squared"),
  title = "Treatment Effect on Audit Outcomes: Portion of Budget Correctly Allocated",
  notes = footnote_text
  )






#########################
#### Event Study ########
#########################


np_panel3 <- np_panel3 %>%
  mutate(Bin = factor(Bin, levels = c(1, 2, 3, 4)))

poppies_panel3 <- poppies_panel3 %>%
  mutate(Bin = factor(Bin, levels = c(1, 2, 3, 4)))

np_panel3 <- np_panel3 %>%
  arrange(Bin)

poppies_panel3 <- poppies_panel3 %>%
  arrange(Bin)

unique(np_panel3$Bin)
unique(poppies_panel3$Bin)

np_panel3 <- np_panel3 %>%
	mutate(Bin = as.numeric(Bin))
poppies_panel3 <- poppies_panel3 %>%
	mutate(Bin = as.numeric(Bin))


np_panel3 <- np_panel3 %>%
  mutate(event_time = Bin - stag_treat_bin)

np_panel3 <- np_panel3 %>%
  mutate(event_time = factor(event_time))


poppies_panel3 %>%
	distinct(stag_treat_bin, Bin)
np_panel3 %>%
	distinct(stag_treat_bin, Bin)


poppies_panel3 <- poppies_panel3 %>%
  mutate(event_time = Bin - stag_treat_bin)

poppies_panel3 <- poppies_panel3 %>%
  mutate(event_time = factor(event_time))

unique(poppies_panel3$Bin)
table(np_panel3$Bin)
unique(poppies_panel3$stag_treat_bin)
table(np_panel3$stag_treat_bin)
unique(poppies_panel3$event_time)
unique(np_panel3$event_time)

np_panel3 %>%
  group_by(stag_treat_bin) %>%
  summarise(num_unique_states = n_distinct(ADM1_PCODE)) %>%
  arrange(stag_treat_bin)


poppies_panel3 %>%
  group_by(stag_treat_bin) %>%
  summarise(num_unique_states = n_distinct(ADM1_PCODE)) %>%
  arrange(stag_treat_bin)

# no poppies
es_1 <- feols(Bin_attacks2 ~ i(event_time, ref = -1) + Pres_misalign + population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + state_misalign | 0, data = np_panel3)

# poppies
es_2 <- feols(Bin_attacks2 ~ i(event_time, ref = -1) + Pres_misalign + population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + state_misalign | 0, data = poppies_panel3)

es_models <- list("Criminally-Entrenched Areas" = es_2, "Non-Entrenched Areas" = es_1)

names(coef(es_2))


es_coef_map <- c(
  "event_time::-2" = "Event time = -2", 
  "event_time::-1" = "Event time = -1", 
  "event_time::1" = "Event time = 1", 
  "event_time::2" = "Event time = 2", 
  "event_time::3" = "Event time = 3", 
  "event_time::4" = "Event time = 4" )

# Table A3
modelsummary(es_models,
  estimate = "{estimate}{stars}", 
  output = "latex",
  coef_map = es_coef_map,
  title = "Event Study: Impact on Attacks against Politicians",
  notes = footnote_text)


length(unique(poppies_panel3$ADM2_PCODE))
length(unique(np_panel3$ADM2_PCODE))

cat(length(unique(poppies_panel3$ADM2_PCODE)), "&", length(unique(np_panel3$ADM2_PCODE)))




es_1_table <- broom::tidy(es_1) %>% mutate(Model = "Non-Entrenched Areas")
es_2_table <- broom::tidy(es_2) %>% mutate(Model = "Criminally-Entrenched Areas")

es_plot_data <- bind_rows(es_1_table, es_2_table)

es_plot_data <- es_plot_data %>%
  dplyr::filter(grepl("event_time::", term)) %>%
  mutate(event_time = as.numeric(gsub("event_time::", "", term)))

# Figure A2
ggplot(es_plot_data, aes(x = event_time, y = estimate, color = Model, shape = Model)) +
  geom_point(position = position_dodge(width = 0.4), size = 2) +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error),
                position = position_dodge(width = 0.4), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Event Time", y = "Estimated Effect on Violence against Politicians", title = "Event Study Estimates: Entrenched vs Non-Entrenched Areas") +
  theme_minimal() +
  geom_vline(xintercept = 0, linetype = "dotted") + 
  theme(legend.title = element_blank())




#### Incumbents

# no poppies
es_1_inc <- feols(Bin_inc ~ i(event_time, ref = -1) + Pres_misalign + population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + state_misalign | 0, data = np_panel3)

# poppies
es_2_inc <- feols(Bin_inc ~ i(event_time, ref = -1) + Pres_misalign + population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + state_misalign | 0, data = poppies_panel3)

es_models_inc <- list("Criminally-Entrenched Areas" = es_2_inc, "Non-Entrenched Areas" = es_1_inc)

names(coef(es_2_inc))


# Table A4
modelsummary(es_models_inc,
  estimate = "{estimate}{stars}", 
  output = "latex",
  coef_map = es_coef_map,
  title = "Event Study: Impact on Attacks against Incumbents",
  notes = footnote_text)


length(unique(poppies_panel3$ADM2_PCODE))
length(unique(np_panel3$ADM2_PCODE))

# Table A4
cat(length(unique(poppies_panel3$ADM2_PCODE)), "&", length(unique(np_panel3$ADM2_PCODE)))

es_1_table_inc <- broom::tidy(es_1_inc) %>% mutate(Model = "Non-Entrenched Areas")
es_2_table_inc <- broom::tidy(es_2_inc) %>% mutate(Model = "Criminally-Entrenched Areas")

es_plot_data_inc <- bind_rows(es_1_table_inc, es_2_table_inc)

es_plot_data_inc <- es_plot_data_inc %>%
  dplyr::filter(grepl("event_time::", term)) %>%
  mutate(event_time = as.numeric(gsub("event_time::", "", term)))

# Figure A3
ggplot(es_plot_data_inc, aes(x = event_time, y = estimate, color = Model, shape = Model)) +
  geom_point(position = position_dodge(width = 0.4), size = 2) +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error),
                position = position_dodge(width = 0.4), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Event Time", y = "Estimated Effect on Violence against Incumbents", title = "Event Study Estimates: Entrenched vs Non-Entrenched Areas") +
  theme_minimal() +
  geom_vline(xintercept = 0, linetype = "dotted") + 
  theme(legend.title = element_blank())




#### Challengers


# no poppies
es_1_chal <- feols(Bin_chal ~ i(event_time, ref = -1) + Pres_misalign + population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + state_misalign | 0, data = np_panel3)

# poppies
es_2_chal <- feols(Bin_chal ~ i(event_time, ref = -1) + Pres_misalign + population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + state_misalign | 0, data = poppies_panel3)

es_models_chal <- list("Criminally-Entrenched Areas" = es_2_chal, "Non-Entrenched Areas" = es_1_chal)

names(coef(es_2_chal))

# Table A5
modelsummary(es_models_chal,
  estimate = "{estimate}{stars}", 
  output = "latex",
  coef_map = es_coef_map,
  title = "Event Study: Impact on Attacks against Challengers",
  notes = footnote_text)


length(unique(poppies_panel3$ADM2_PCODE))
length(unique(np_panel3$ADM2_PCODE))

# Table A5
cat(length(unique(poppies_panel3$ADM2_PCODE)), "&", length(unique(np_panel3$ADM2_PCODE)))

es_1_table_chal <- broom::tidy(es_1_chal) %>% mutate(Model = "Non-Entrenched Areas")
es_2_table_chal <- broom::tidy(es_2_chal) %>% mutate(Model = "Criminally-Entrenched Areas")

es_plot_data_chal <- bind_rows(es_1_table_chal, es_2_table_chal)

es_plot_data_chal <- es_plot_data_chal %>%
  dplyr::filter(grepl("event_time::", term)) %>%
  mutate(event_time = as.numeric(gsub("event_time::", "", term)))

# Figure A4
ggplot(es_plot_data_chal, aes(x = event_time, y = estimate, color = Model, shape = Model)) +
  geom_point(position = position_dodge(width = 0.4), size = 2) +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error),
                position = position_dodge(width = 0.4), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Event Time", y = "Estimated Effect on Violence against Challengers", title = "Event Study Estimates: Entrenched vs Non-Entrenched Areas") +
  theme_minimal() +
  geom_vline(xintercept = 0, linetype = "dotted") + 
  theme(legend.title = element_blank())




##########################################
#### Poppies Alterative Estimation #######
##########################################


year <- read.csv("data\\annual_data.csv")

# stack
yr <- unique(year$Year)
stacked_year <- NULL

# subset each bin 
for (j in 1:length(yr)){
  ob_yr <- yr[j]
  panel_yr <- subset(year, year$Year == ob_yr)

  # for each observation in that year
  for (i in 1:length(panel_yr$Municipality)){
    obs <- panel_yr[i,]
      # if pure control, add to dataframe
      if (obs$treat_stag == 0){
            stacked_year <- rbind(stacked_year, obs)
          # if not-yet-treated or first-treated, add to dataframe
          } else if (obs$treat_stag >= obs$Year){
            stacked_year <- rbind(stacked_year, obs)
          # if already treated, skip
          }
        }
      }


##### correlations of poppies

corrs <- data.frame(poppies = year$Poppies,
  nCarteles_2010 = year$nCarteles_2010,
  hom_rate = year$hom_rate,
  groups_active_2015 = year$groups_active_2015,
  groups_active_alt_2015 = year$groups_active_alt_2015,
  val_index = year$val_index,
  group_strength_2015 = year$group_strength_2015)

mean_hom <- mean(corrs$hom_rate, na.rm = TRUE)

# binary
corrs <- corrs %>%
	mutate(nCarteles_2010 = ifelse(nCarteles_2010 > 0, 1, 0),
		hom_rate = ifelse(hom_rate > mean_hom, 1, 0),
		groups_active_2015 = ifelse(groups_active_2015 > 0, 1, 0),
		groups_active_alt_2015 = ifelse(groups_active_alt_2015 > 0, 1, 0),
		group_strength_2015 = ifelse(group_strength_2015 > 0, 1, 0))

# Create confusion matrix components
TP <- sum(corrs$poppies == 1 & corrs$nCarteles_2010 == 1 & !is.na(corrs$poppies) & !is.na(corrs$nCarteles_2010))  # True Positives
FN <- sum(corrs$poppies == 0 & corrs$nCarteles_2010 == 1 & !is.na(corrs$poppies) & !is.na(corrs$nCarteles_2010))  # False Negatives
TN <- sum(corrs$poppies == 0 & corrs$nCarteles_2010 == 0 & !is.na(corrs$poppies) & !is.na(corrs$nCarteles_2010))  # True Negatives
FP <- sum(corrs$poppies == 1 & corrs$nCarteles_2010 == 0 & !is.na(corrs$poppies) & !is.na(corrs$nCarteles_2010))  # False Positives

# Calculate False Positive Rate
FP / (FP + TN)

# Create confusion matrix components
TP <- sum(corrs$poppies == 1 & corrs$groups_active_2015 == 1 & !is.na(corrs$poppies) & !is.na(corrs$groups_active_2015))  # True Positives
FN <- sum(corrs$poppies == 0 & corrs$groups_active_2015 == 1 & !is.na(corrs$poppies) & !is.na(corrs$groups_active_2015))  # False Negatives
TN <- sum(corrs$poppies == 0 & corrs$groups_active_2015 == 0 & !is.na(corrs$poppies) & !is.na(corrs$groups_active_2015))  # True Negatives
FP <- sum(corrs$poppies == 1 & corrs$groups_active_2015 == 0 & !is.na(corrs$poppies) & !is.na(corrs$groups_active_2015))  # False Positives

# Calculate False Positive Rate
FP / (FP + TN)


############################
##### Other Crimes #########
############################

# write a function to do the estimations
run_models <- function(data, dep_vars, indep_vars, fixed_effects, cluster_var, coef_map = NULL, gof_map, title, notes) {
  
  models_list <- list()

  for(dep_var in dep_vars) {
    formula <- as.formula(paste(dep_var, "~", indep_vars, "|", fixed_effects, "| 0 |", cluster_var))
    
    model <- felm(formula, data = data)
    
    models_list[[dep_var]] <- model
  }
  
  if (!is.null(coef_map)) {
    modelsummary(models_list, 
                 estimate = "{estimate}{stars}",
                 output = "latex", 
                 coef_map = coef_map,
                 gof_map = gof_map,
                 title = title,
                 notes = notes)
  } else {
    modelsummary(models_list, 
                 estimate = "{estimate}{stars}",
                 output = "latex", 
                 gof_map = gof_map,
                 title = title,
                 notes = notes)
  }
}


dependent_vars <- c("Bin_attacks2", "Bin_inc", "Bin_chal")

independent_vars <- "stag_ind_bin + Pres_misalign + population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + state_misalign + Bin:Year + Bin:State"

fixed_effects <- "Bin + Year + State"
cluster_variable <- "State"

cm_pa2 <- c('stag_ind_bin' = 'Mayoral Reelection',
           'Poppies' = 'Criminal Presence',
           'stag_ind_bin:Poppies' = 'Mayoral Reelection × Criminal Presence',
           'party_alt' = 'Partisan Alternation',
           'stag_ind_bin:party_alt' = 'Mayoral Reelection × Partisan Alternation',
           'Poppies:party_alt' = 'Criminal Presence × Partisan Alternation',
           'stag_ind_bin:Poppies:party_alt' = 'Mayoral Reelection × Partisan Alternation × Criminal Presence')

coef_map <- cm_pa2 
gof_map <- c("nobs", "r.squared")

footnote_text <- "Significance levels: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001."

names(df_alt2)

names(df_alt2)[61] <- "Extorsion"
names(df_alt3)[61] <- "Extorsion"

cm_pa3 <- c('stag_ind_bin' = 'Mayoral Reelection')

# poppies
# Table A15
run_models(data = df_alt2, 
           dep_vars = c("Secuestro", "Extorsion",  "Homicidio.doloso", "Robo.a.negocio"), 
           indep_vars = independent_vars,
           fixed_effects = "Bin",
           cluster_var = cluster_variable,     
           coef_map = cm_pa3,        
           gof_map = gof_map, 
           title = "Treatment Effect on Crimes (Poppies)", 
           notes = footnote_text)

# no poppies
# Table A16
run_models(data = df_alt3, 
           dep_vars = c("Secuestro", "Extorsion",  "Homicidio.doloso", "Robo.a.negocio"), 
           indep_vars = independent_vars,
           fixed_effects = "Bin",
           cluster_var = cluster_variable,     
           coef_map = cm_pa3,   
           gof_map = gof_map, 
           title = "Treatment Effect on Crimes (no poppies)", 
           notes = footnote_text)


############################################################
#### Descriptive Statistics for Running for Reelection #####
############################################################

#### Likelihood of receiving treatment assignment ########

# state level means
names(panel)

state_panel <- panel %>% 
  group_by(ADM1_PCODE, Year) %>%
  summarise(
    stag_treat_bin = mean(stag_treat_bin, na.rm = TRUE),
    total_alt = mean(total_alt, na.rm = TRUE),
    Upper_misalign = mean(Upper_misalign, na.rm = TRUE),
    state_misalign = mean(state_misalign, na.rm = TRUE),
    Pres_misalign = mean(Pres_misalign, na.rm = TRUE),
    MajorHighway = mean(MajorHighway, na.rm = TRUE),
    MajorPort = mean(MajorPort, na.rm = TRUE),
    Airports = mean(Airports, na.rm = TRUE),
    Railline = mean(Railline, na.rm = TRUE),
    Oilline = mean(Oilline, na.rm = TRUE),
    Intlborder = mean(Intlborder, na.rm = TRUE),
    Shoreline = mean(Shoreline, na.rm = TRUE),
    MajCity = mean(MajCity, na.rm = TRUE),
    population = mean(population, na.rm = TRUE),
    pri_mayor = mean(pri_mayor, na.rm = TRUE),
    Poppies = mean(Poppies, na.rm = TRUE),
    Bin_attacks2 = mean(Bin_attacks2, na.rm = TRUE)
  ) %>%
  ungroup()

# regress covariates on treatment assigment
stag_treat_assign <- plm(stag_treat_bin ~ total_alt + Upper_misalign + state_misalign + Pres_misalign  + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + MajCity + population + pri_mayor + Poppies + Bin_attacks2, 
          data = state_panel, model="within", effect="twoways",
          index = c("ADM1_PCODE","Year"))

summary(stag_treat_assign)

cm_assign <- c("total_alt" = "Municipal partisan alternation", 'Upper_misalign' = 'State-federal partisan misalignment', 'state_misalign' = "State-Municipal misalignment",'Pres_misalign'= "Federal-Municipal misalignment", 'MajorHighway' =  "Major highway", "MajorPort" = "Major Port", "Railline" = "Rail Line", "Oilline" = "Oil Line", "Intlborder" = "International Border", 'population' = "Population", 'pri_mayor'= "PRI mayor", 'Poppies'= "Poppies", 'Bin_attacks2'= "Attack Count")

footnote_text <- "Significance levels: . p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001."

# Table A2
modelsummary(stag_treat_assign, 
	estimate = "{estimate}{stars}",
  statistic = "std.error",
	output = "latex", 
	coef_map = cm_assign,
	gof_map = c("nobs", "r.squared"),
	title = "Logistic Regression of Covariates on Assignment of Treatment",
	notes = footnote_text)


#### Likelihood of being selected from main panel ####

selected_chars <- felm(if_audit ~ Poppies + Pres_misalign + population + MajorHighway + MajorPort + Airports + Railline + Oilline + Intlborder + Shoreline + pri_mayor + state_misalign | State + Bin | 0 | State, data = panel)
summary(selected_chars)

cm_pa4 <- c("Poppies" = "Poppies", 'Pres_misalign' = 'Federal partisan misalignment', 'state_misalign' = "State partisan misalignment", 'MajorHighway' =  "Major highway", "MajorPort" = "Major Port", "Railline" = "Rail Line", "Oilline" = "Oil Line", "Intlborder" = "International Border", "Shoreline" = "Shoreline", 'population' = "Population", 'pri_mayor'= "PRI mayor")

# Table A11
modelsummary(selected_chars, 
  estimate = "{estimate}{stars}",
  output = "latex", 
  coef_map = cm_pa4,
  gof_map = c("nobs", "r.squared"),
  title = "Municipal Characteristics of Audit Selection",
  notes = footnote_text
  )

# manually add the unique N -- not stacked N
num_obs <- length(unique(panel$ADM2_PCODE))

# Part of Table A8
cat(sprintf("Unique Obs. & %d \\\\\n", num_obs))



######################################
############# Mapping poppies ########
######################################

# load map
mexico <- st_read(dsn ="data/maps/mex_admbnda_govmex_20210618_SHP/mex_admbnda_adm2_govmex_20210618.shp")


# rewrite code for each municipality to match shape file
shape_mex <- as.data.frame(mexico)

head(shape_mex)

municip_poppies <- panel

### make the correct number
codes <- municip_poppies$ADM2_PCODE
class(codes)
codes <- as.character(codes)
tes <- ifelse(nchar(codes)>4,paste("MX",codes,sep = ""),paste("MX0",codes,sep = ""))
municip_poppies$ADM2_PCODE <- tes
mex_shape2 <- merge(mexico, municip_poppies, by = "ADM2_PCODE")


head(mex_shape2)

# visualize where poppies are
# Figure A1
ggplot() +
  geom_sf(data = mex_shape2, aes(fill = factor(`Poppies`))) +
  scale_fill_manual(values = c("0" = "white", "1" = "blue")) +
  theme_minimal() +
  labs(fill = "Poppy fields")


# approval map and figure

states <- st_read(dsn ="data/maps/mex_admbnda_govmex_20210618_SHP/mex_admbnda_adm1_govmex_20210618.shp")

head(states)

tes <- panel

tes2 <- tes %>% 
		dplyr::select(ADM1_PCODE, State, treat_stag) %>%
		dplyr::distinct()

mex_shape2 <- merge(states, tes2, by = "ADM1_PCODE")

# rename
mex_shape2$tes <- ifelse(mex_shape2$treat_stag == 2018, 0, mex_shape2$treat_stag)

ggplot() +
  geom_sf(data = mex_shape2, aes(fill = factor(tes))) +
  theme_minimal(base_family = "serif") +
  theme(legend.position = c(1, 0.95),  
        legend.justification = c("right", "top")) + 
  scale_fill_manual(
    values = c("0" = "#da291c",  # Red for no reelection
               "2015" = "#002878",  # EmoryBlue for 2015
               "2016" = "#348338",  # KellyGreen for 2016
               "2017" = "#f2a900"),  # Yellow for 2017
    name = "Adoption of Mayoral Reelection",  
    labels = c("No reelection in 2018", 
               "Approved in 2015", 
               "Approved in 2016", 
               "Approved in 2017")
  )

 

sessionInfo()
