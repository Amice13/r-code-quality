### analysis: alternative DiD estimators

# Setup ------
library(dplyr)
library(here)
library(fixest)
library(ggplot2)
library(did2s)
library(did)
library(modelsummary)
library(kableExtra)
library(devtools)
library(fect)
source(here("scripts", "functions", "analysis_functions.R"))

setFixest_notes(FALSE)
config_modelsummary(factory_latex = "kableExtra")

### define output paths
tab.path <- here("tables")
fig.path <- here("figures")

### load segment data
rail.df <- readRDS(here("analysis_data","analysis_rails_df.rds"))

#### multiply outcomes by 100 for percentage point interpretation
rail.df$separatism_cw_sec_ind_aut_yn_100 <- 100*rail.df$separatism_cw_sec_ind_aut_yn

# Create treatment group indicator
rail.df <- rail.df %>% 
  group_by(seg_fe) %>% 
  mutate(treat = max(rail_any_yn == 1),
         id_segment = cur_group_id()) %>% 
  ungroup()


### define countries to exclude from analysis
drop_cntrs <- c("United Kingdom","Egypt","Cherkessy","Ireland")

## code country_year fe
rail.df$country_year <- factor(paste(rail.df$id_cap_rule_corr,rail.df$year,sep="_"))


##### subset to analysis sample
rail.df.sample <- rail.df[which(rail.df$rail_ttt_5years_binned%in%c(seq(-60,90,5),-1000,1000) &
                                  rail.df$year>1815  & rail.df$year<1946 &
                                  rail.df$capital_yn_dist_corr==0 &
                                  rail.df$area_sqkm_raw>2000 &
                                  !rail.df$name_cap_rule_corr%in%drop_cntrs),]

# Correct year of adoption for units that get is too close to 1945
rail.df.sample <- rail.df.sample %>% 
  mutate(rail_cohort_year = ifelse(rail_cohort_year > 1940, 0, rail_cohort_year))

# Keep complete obs
rail.df.sample <- rail.df.sample %>% 
  filter(!is.na(separatism_cw_sec_ind_aut_yn_100), !is.na(rail_any_yn),
         !is.na(war_hist_terr_cw), !is.na(time_since_cw_terr_combined),
         !is.na(seg_fe), !is.na(year))


# Fit fect models ------
# Note: here the lambda parameter for mc models is provided to save time (was the 
#  parameter selected by our previous run), but commenting it will have the
#  function select the optimal lambda through bootstrapping.

m1.fect.fe <- fect(
  separatism_cw_sec_ind_aut_yn_100 ~ rail_any_yn + war_hist_terr_cw +
    i(time_since_cw_terr_combined,ref=1) + i(time_since_ind_aut,ref=1),
  index = c("seg_fe", "year"),
  data = rail.df.sample,
  method = "fe",
  se = T,
  nboots = 500
)
# plot(m1.fect.fe, type = "equiv")

m1.fect.ife <- fect(
  separatism_cw_sec_ind_aut_yn_100 ~ rail_any_yn + war_hist_terr_cw +
    i(time_since_cw_terr_combined,ref=1) + i(time_since_ind_aut,ref=1),
  index = c("seg_fe", "year"),
  data = rail.df.sample,
  method = "ife",
  se = T,
  nboots = 500
)
# plot(m1.fect.ife, type = "equiv")

m1.fect.mc <- fect(
  separatism_cw_sec_ind_aut_yn_100 ~ rail_any_yn + war_hist_terr_cw +
    i(time_since_cw_terr_combined,ref=1) + i(time_since_ind_aut,ref=1),
  index = c("seg_fe", "year"),
  lambda = 0.0316227766016838,
  data = rail.df.sample,
  method = "mc",
  se = T,
  nboots = 500
)
# plot(m1.fect.mc, type = "equiv")

m2.fect.cfe <- fect(
  separatism_cw_sec_ind_aut_yn_100 ~ rail_any_yn + war_hist_terr_cw +
    i(time_since_cw_terr_combined,ref=1) + i(time_since_ind_aut,ref=1),
  index = c("seg_fe", "year"),
  sfe = c("country_year"),
  data = rail.df.sample,
  method = "cfe",
  se = T,
  nboots = 500
)
# plot(m2.fect.cfe, type = "equiv")

# Make table
m.list <- list(
  m1.fect.fe, 
  m1.fect.ife, 
  m1.fect.mc, 
  m2.fect.cfe
)
gm <- list(
  list("raw" = "nobs", "clean" = "Observations", "fmt" = 0)
)
rows <- tribble(~term, ~M1,  ~M2, ~M3, ~M4,
                "\\midrule Method", "FE", "IFE",  "MC", "CFE",
                "\\midrule Segment FE", "Yes",  "Yes",  "Yes", "Yes",
                "Year FE", "Yes",  "Yes", "Yes",  "Yes",
                "Country-Year FE", "No", "No", "No", "Yes"
)
attr(rows, 'position') <- c(7:9)
cap <-  "Alternative panel data estimator: \\texttt{fect} package \\label{tab:did_separatism_fect}"
note <- "\\\\scriptsize{\\\\textit{Notes:} The unit of analysis is the ethnic segment year. 
State-leading segments and segments smaller than 2000 sqkm dropped. 
Method acronyms: 'FE' $=$ two-way fixed effects; 'IFE' $=$ interactive fixed effects; 'MC' $=$ matrix completion; 'CFE' $=$ complex fixed effects.
Segment clustered standard errors in parentheses.
\\\\textit{+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001.}}" 

# make table
tab.out <- modelsummary(m.list,
                        coef_map=c("rail_any_yn" = "Rails (Y/N)",
                                   "war_hist_terr_cw" = "Civil war history",
                                   "time_since_cw_terr_combined" = "Time since civil war",
                                   "time_since_ind_aut" = "Time since ind.\\ or aut. claim"),
                        estimate=c("{estimate}{stars}"),
                        stars=T, 
                        add_rows = rows,
                        title=cap,
                        escape=F,
                        output="latex") %>%
  kable_styling(font_size = 9) %>%
  footnote(general=c(note), threeparttable = T, escape = F, general_title = "")%>%
  column_spec(2:5,width="2.5cm") %>%
  add_header_above(header=c(" "=1,"100 $\\\\times$ Secession, Terr. CW or Claim"=4), escape=F)

# write out
kableExtra::save_kable(tab.out, file = here(tab.path,"table_a2.tex"))


# Clean up ----
rm(m1.fect.fe, 
   m1.fect.ife, 
   m1.fect.mc, 
   m2.fect.cfe, tab.out)

gc()