### analysis: tests causal mechanisms
library(dplyr)
library(here)
library(fixest)
library(ggplot2)
library(did2s)
library(modelsummary)
library(kableExtra)
library(devtools)
library(fect)
library(interflex)

setFixest_notes(FALSE)
source(here("scripts", "functions", "analysis_functions.R"))

### define output paths
tab.path <- here("tables")
fig.path <- here("figures")

### load segment data
rail.df <- readRDS(here("analysis_data","analysis_rails_df.rds"))

#### multiply outcomes by 100 for percentage point interpretation
rail.df$separatism_cw_sec_ind_aut_yn_100 <- 100*rail.df$separatism_cw_sec_ind_aut_yn

### define countries to exclude from analysis
drop_cntrs <- c("United Kingdom","Egypt","Cherkessy","Ireland")

## code country_year fe
rail.df$country_year <- factor(paste(rail.df$id_cap_rule_corr,rail.df$year,sep="_"))
rail.df$ma.nat.donal.log[is.infinite(rail.df$ma.nat.donal.log)] <- NA

##### subset to analysis sample
rail.df.sample <- rail.df[which(rail.df$rail_ttt_5years_binned%in%c(seq(-60,90,5),-1000,1000) &
                                  rail.df$year>1815  & rail.df$year<1946 &
                                  rail.df$capital_yn_dist_corr==0 &
                                  rail.df$area_sqkm_raw>2000 &
                                  !rail.df$name_cap_rule_corr%in%drop_cntrs),]

## Make Leads
lead.vars <- c("ma.nat.donal.log", "state_cap", "mob_cap")
rail.df.sample[,paste0(lead.vars, ".lead")] <- NA
for(t in c(1, 2, 5)){
  ldf <- rail.df.sample[, c("seg_id", "year", lead.vars)]
  ldf$year <- ldf$year - t
  colnames(ldf)[colnames(ldf) %in% lead.vars] <- new.names <-
    paste0(colnames(ldf)[colnames(ldf) %in% lead.vars], ".l",t)
  rail.df.sample <- rail.df.sample[, !colnames(rail.df.sample) %in% new.names]
  rail.df.sample <- plyr::join(rail.df.sample,
                               ldf,
                               by = c("seg_id", "year"))
  rail.df.sample[,paste0(lead.vars, ".lead")] <- rail.df.sample[,paste0(lead.vars, ".lead")] + rail.df.sample[,new.names]
}
rail.df.sample[,paste0(lead.vars, ".lead")] <- rail.df.sample[,paste0(lead.vars, ".l5")] - rail.df.sample[,paste0(lead.vars, ".l1")]
rail.df.sample[,paste0(lead.vars, ".yoyc")] <- rail.df.sample[,paste0(lead.vars, ".l2")] - rail.df.sample[,paste0(lead.vars, ".l1")]

## Make Lags
lag.vars <- c("ma.nat.donal.log", "state_cap", "mob_cap")
for(t in c(1, 2, 5)){
  ldf <- rail.df.sample[, c("seg_id", "year", lead.vars)]
  ldf$year <- ldf$year + t
  colnames(ldf)[colnames(ldf) %in% lead.vars] <- new.names <-
    paste0(colnames(ldf)[colnames(ldf) %in% lead.vars], ".l",t)
  rail.df.sample <- rail.df.sample[, !colnames(rail.df.sample) %in% new.names]
  rail.df.sample <- plyr::join(rail.df.sample,
                               ldf,
                               by = c("seg_id", "year"))
}
rail.df.sample[,paste0(lead.vars, ".lag")] <- rail.df.sample[,paste0(lead.vars, ".l1")] - rail.df.sample[,paste0(lead.vars, ".l5")]


rail.df.sample$ma.nat.donal.log.st <- standardize(rail.df.sample$ma.nat.donal.log)
rail.df.sample$state_cap.st <- standardize(rail.df.sample$state_cap)
rail.df.sample$mob_cap.st <- standardize(rail.df.sample$mob_cap)


##### ##### ##### ##### ##### ##### ##### ##### ##### #####
##### Estimate Models with segment and year FE ----
##### ##### ##### ##### ##### ##### ##### ##### ##### #####

mech.ma <- feols(separatism_cw_sec_ind_aut_yn_100 ~ ma.nat.donal.log +
                  war_hist_terr_cw + i(time_since_cw_terr_combined) + i(time_since_ind_aut) | seg_fe + year,
                 data=rail.df.sample,
                 cluster="seg_fe")

mech.state <-  feols(separatism_cw_sec_ind_aut_yn_100 ~ state_cap +
                       war_hist_terr_cw + i(time_since_cw_terr_combined) + i(time_since_ind_aut) | seg_fe + year,
                     data=rail.df.sample,
                     cluster="seg_fe")

mech.int <- feols(separatism_cw_sec_ind_aut_yn_100 ~ mob_cap +
                    war_hist_terr_cw + i(time_since_cw_terr_combined) + i(time_since_ind_aut) | seg_fe + year,
                  data=rail.df.sample,
                  cluster="seg_fe")

mech.all <- feols(separatism_cw_sec_ind_aut_yn_100 ~ ma.nat.donal.log + state_cap + mob_cap +
                    war_hist_terr_cw + i(time_since_cw_terr_combined) + i(time_since_ind_aut) | seg_fe + year,
                  data=rail.df.sample,
                  cluster="seg_fe")

# Prepare table
m.list <- list(
  mech.ma,
  mech.state,
  mech.int,
  mech.all
)
vcov.list <- list(mech.ma$cov.scaled,
                  mech.state$cov.scaled,
                  mech.int$cov.scaled,
                  mech.all$cov.scaled)

gm <- list(
  list("raw" = "nobs", "clean" = "Observations", "fmt" = 0)
)
rows <- tribble(~term, ~M1,  ~M2, ~M3, ~M4,
                "Segment FE", "Yes",  "Yes", "Yes", "Yes",
                "Year FE", "Yes",  "Yes", "Yes", "Yes",
                "\\midrule Mean DV",
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100[mech.ma$obs_selection$obsRemoved]),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100[mech.all$obs_selection$obsRemoved]),3))
)
attr(rows, 'position') <- c(7:9)
cap <-  "Network Structure and Causal Mechanisms \\label{tab:rails.network}"
note <- "\\\\scriptsize{\\\\textit{Notes:} The unit of analysis is the ethnic segment year.
State-leading segments and segments smaller than 2000 sqkm dropped.  All models control for the number of past conflicts and peace years indicators.
Segment clustered standard errors in parentheses.
\\\\textit{+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001.}}"


# make table
tab.out <- modelsummary(m.list,
                        coef_map=c("ma.nat.donal.log"= "National Market Access",
                                   "state_cap" = "State Reach",
                                   "mob_cap" = "Internal Connectivity"),
                        estimate=c("{estimate}{stars}"),
                        gof_map = gm, stars=T,
                        vcov = vcov.list,
                        add_rows = rows,
                        title=cap,
                        escape=F,
                        output="latex") %>%
  kable_styling(font_size = 10) %>%
  footnote(general=c(note),threeparttable = T, escape = F, general_title = "")%>%
  column_spec(2:5,width="2.5cm") %>%
  add_header_above(header=c(" "=1,"100 $\\\\times$ Secession, Terr. CW or Claim"=4),escape=F)

# write out
kableExtra::save_kable(tab.out, file = here(tab.path,"table_3.tex"))


##### ##### ##### ##### ##### ##### ##### ##### ##### #####
##### Estimate log-transformed models with segment and year FE ----
##### ##### ##### ##### ##### ##### ##### ##### ##### #####

## Variables
rail.df.sample$state_cap.log <- log(1+rail.df.sample$state_cap)
rail.df.sample$mob_cap.log <- log(1+rail.df.sample$mob_cap)


m.list <-  list(feols(separatism_cw_sec_ind_aut_yn_100 ~ ma.nat.donal.log + state_cap.log + mob_cap.log+
                        war_hist_terr_cw | seg_fe + year +  time_since_cw_terr_combined + time_since_ind_aut ,
                      data=rail.df.sample,
                      cluster="seg_fe"),
                feols(separatism_cw_sec_ind_aut_yn_100 ~ ma.nat.donal.log + state_cap.log + mob_cap.log+
                        war_hist_terr_cw | seg_fe + country_year +  time_since_cw_terr_combined + time_since_ind_aut ,
                      data=rail.df.sample,
                      cluster="seg_fe"),
                feols(separatism_cw_sec_ind_aut_yn_100 ~ ma.nat.donal + state_cap + mob_cap+
                        war_hist_terr_cw | seg_fe + year +  time_since_cw_terr_combined + time_since_ind_aut ,
                      data=rail.df.sample,
                      cluster="seg_fe"),
                feols(separatism_cw_sec_ind_aut_yn_100 ~ ma.nat.donal + state_cap + mob_cap+
                        war_hist_terr_cw | seg_fe + country_year +  time_since_cw_terr_combined + time_since_ind_aut ,
                      data=rail.df.sample,
                      cluster="seg_fe"))


## Prepare regression table
vcov.list <- lapply(m.list, function(m){m$cov.scaled})

gm <- list(
  list("raw" = "nobs", "clean" = "Observations", "fmt" = 0)
)
rows <- tribble(~term, ~M1,  ~M2, ~M3, ~M4,
                "Segment FE", "Yes",  "Yes", "Yes", "Yes",
                "Year FE", "Yes",  "No", "Yes", "No",
                "Country-year FE", "No",  "Yes", "No", "Yes",
                "\\midrule Mean DV",
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100[m.list[[1]]$obs_selection$obsRemoved]),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100[m.list[[2]]$obs_selection$obsRemoved]),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100),3))
)
attr(rows, 'position') <- c(7:9) + 6
cap <-  "Network Structure: With and without Log-transform \\label{tab:rails_logtransform}"
note <- "\\\\scriptsize{\\\\textit{Notes:} The unit of analysis is the ethnic segment year.
State-leading segments and segments smaller than 2000 sqkm dropped.  All models control for the number of past conflicts and peace years indicators.
Segment clustered standard errors in parentheses.
\\\\textit{+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001.}}"

# make table
tab.out <- modelsummary(m.list,
                        coef_map=c("ma.nat.donal.log"= "National Market Access (log)",
                                   "state_cap.log" = "State Reach (log)",
                                   "mob_cap.log" = "Internal Connectivity (log)", "ma.nat.donal"= "National Market Access",
                                   "state_cap" = "State Reach",
                                   "mob_cap" = "Internal Connectivity"),
                        estimate=c("{estimate}{stars}"),
                        gof_map = gm, stars=T,
                        vcov = vcov.list,
                        add_rows = rows,
                        title=cap,
                        escape=F,
                        output="latex") %>%
  kable_styling(font_size = 9, latex_options = "hold_position") %>%
  footnote(general=c(note),threeparttable = T, escape = F, general_title = "")%>%
  column_spec(2:5,width="2.5cm") %>%
  add_header_above(header=c(" "=1,"100 $\\\\times$ Secession, Terr. CW or Claim"=4),escape=F)

# write out
kableExtra::save_kable(tab.out, file = here(tab.path,"table_a14.tex"))



##### ##### ##### ##### ##### ##### ##### ##### ##### #####
##### Estimate Models with segment and year FE, time-variant population ----
##### ##### ##### ##### ##### ##### ##### ##### ##### #####

mech.ma <- feols(separatism_cw_sec_ind_aut_yn_100 ~ ma.nat.donal.ptv.log+ log(1+popc_corr) +
                   war_hist_terr_cw | seg_fe + year  + time_since_cw_terr_combined + time_since_ind_aut,
                 data=rail.df.sample,
                 cluster="seg_fe")

mech.state <-  feols(separatism_cw_sec_ind_aut_yn_100 ~ state_cap_ptv + log(1+popc_corr) +
                       war_hist_terr_cw | seg_fe + year  + time_since_cw_terr_combined + time_since_ind_aut,
                     data=rail.df.sample,
                     cluster="seg_fe")

mech.int <- feols(separatism_cw_sec_ind_aut_yn_100 ~ mob_cap_ptv + log(1+popc_corr) +
                    war_hist_terr_cw | seg_fe + year  + time_since_cw_terr_combined + time_since_ind_aut,
                  data=rail.df.sample,
                  cluster="seg_fe")

mech.all <-  feols(separatism_cw_sec_ind_aut_yn_100 ~ ma.nat.donal.ptv.log + state_cap_ptv + mob_cap_ptv + log(1+popc_corr) +
                     war_hist_terr_cw | seg_fe + year  + time_since_cw_terr_combined + time_since_ind_aut,
                   data=rail.df.sample,
                   cluster="seg_fe")

## Prepare regression table
m.list <- list(
  mech.ma,
  mech.state,
  mech.int,
  mech.all
)

vcov.list <- list(mech.ma$cov.scaled,
                  mech.state$cov.scaled,
                  mech.int$cov.scaled,
                  mech.all$cov.scaled)

gm <- list(
  list("raw" = "nobs", "clean" = "Observations", "fmt" = 0)
)
rows <- tribble(~term, ~M1,  ~M2, ~M3, ~M4,
                "Segment FE", "Yes",  "Yes", "Yes", "Yes",
                "Year FE", "Yes",  "Yes", "Yes", "Yes",
                "\\midrule Mean DV",
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100[mech.ma$obs_selection$obsRemoved]),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100[mech.all$obs_selection$obsRemoved]),3))
)
attr(rows, 'position') <- c(7:9)
cap <-  "Network Structure and Causal Mechanisms with Time-Variant Population Data\\label{tab:rails.network.ptv}"
note <- "\\\\scriptsize{\\\\textit{Notes:} The unit of analysis is the ethnic segment year.
State-leading segments and segments smaller than 2000 sqkm dropped.  All models control for the number of past conflicts and peace years indicators.
Segment clustered standard errors in parentheses.
\\\\textit{+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001.}}"

# make table

tab.out <- modelsummary(m.list,
                        coef_map=c("ma.nat.donal.ptv.log"= "National Market Access",
                                   "state_cap_ptv" = "State Reach",
                                   "mob_cap_ptv" = "Internal Connectivity"),
                        estimate=c("{estimate}{stars}"),
                        gof_map = gm, stars=T,
                        vcov = vcov.list,
                        add_rows = rows,
                        title=cap,
                        escape=F,
                        output="latex") %>%
  kable_styling(font_size = 9) %>%
  footnote(general=c(note),threeparttable = T, escape = F, general_title = "")%>%
  column_spec(2:5,width="2.5cm") %>%
  add_header_above(header=c(" "=1,"100 $\\\\times$ Secession, Terr. CW or Claim"=4),escape=F)

# write out
kableExtra::save_kable(tab.out, file = here(tab.path,"table_a15.tex"))



##### ##### ##### ##### ##### ##### ##### ##### ##### #####
##### Estimate Models with segment and year FE, time-variant population and countryyear FE
##### ##### ##### ##### ##### ##### ##### ##### ##### #####

mech.ma <- feols(separatism_cw_sec_ind_aut_yn_100 ~ ma.nat.donal.ptv.log+ log(1+popc_corr) +
                   war_hist_terr_cw | seg_fe + country_year  + time_since_cw_terr_combined + time_since_ind_aut,
                 data=rail.df.sample,
                 cluster="seg_fe")

mech.state <-  feols(separatism_cw_sec_ind_aut_yn_100 ~ state_cap_ptv + log(1+popc_corr) +
                       war_hist_terr_cw | seg_fe + country_year  + time_since_cw_terr_combined + time_since_ind_aut,
                     data=rail.df.sample,
                     cluster="seg_fe")

mech.int <- feols(separatism_cw_sec_ind_aut_yn_100 ~ mob_cap_ptv + log(1+popc_corr) +
                    war_hist_terr_cw | seg_fe + country_year  + time_since_cw_terr_combined + time_since_ind_aut,
                  data=rail.df.sample,
                  cluster="seg_fe")

mech.all <-  feols(separatism_cw_sec_ind_aut_yn_100 ~ ma.nat.donal.ptv.log + state_cap_ptv + mob_cap_ptv + log(1+popc_corr) +
                     war_hist_terr_cw | seg_fe + country_year  + time_since_cw_terr_combined + time_since_ind_aut,
                   data=rail.df.sample,
                   cluster="seg_fe")

## Prepare regression table
m.list <- list(
  mech.ma,
  mech.state,
  mech.int,
  mech.all
)

vcov.list <- list(mech.ma$cov.scaled,
                  mech.state$cov.scaled,
                  mech.int$cov.scaled,
                  mech.all$cov.scaled)

gm <- list(
  list("raw" = "nobs", "clean" = "Observations", "fmt" = 0)
)
rows <- tribble(~term, ~M1,  ~M2, ~M3, ~M4,
                "Segment FE", "Yes",  "Yes", "Yes", "Yes",
                "Country-year FE", "Yes",  "Yes", "Yes", "Yes",
                "\\midrule Mean DV",
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100[mech.ma$obs_selection$obsRemoved]),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100[mech.all$obs_selection$obsRemoved]),3))
)
attr(rows, 'position') <- c(7:9)
cap <-  "Network Structure and Causal Mechanisms with Time-Variant Population Data and Country-Year FEs\\label{tab:rails.network.ptv.cy}"
note <- "\\\\scriptsize{\\\\textit{Notes:} The unit of analysis is the ethnic segment year.
State-leading segments and segments smaller than 2000 sqkm dropped.  All models control for the number of past conflicts and peace years indicators.
Segment clustered standard errors in parentheses.
\\\\textit{+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001.}}"

# make table
tab.out <- modelsummary(m.list,
                        coef_map=c("ma.nat.donal.ptv.log"= "National Market Access",
                                   "state_cap_ptv" = "State Reach",
                                   "mob_cap_ptv" = "Internal Connectivity"),
                        estimate=c("{estimate}{stars}"),
                        gof_map = gm, stars=T,
                        vcov = vcov.list,
                        add_rows = rows,
                        title=cap,
                        escape=F,
                        output="latex") %>%
  kable_styling(font_size = 9) %>%
  footnote(general=c(note),threeparttable = T, escape = F, general_title = "")%>%
  column_spec(2:5,width="2.5cm") %>%
  add_header_above(header=c(" "=1,"100 $\\\\times$ Secession, Terr. CW or Claim"=4),escape=F)

# write out
kableExtra::save_kable(tab.out, file = here(tab.path,"table_a16.tex"))



##### ##### ##### ##### ##### ##### ##### ##### ##### #####
##### Estimate standardized models with segment and year FE ----
##### ##### ##### ##### ##### ##### ##### ##### ##### #####

mech.ma <- feols(separatism_cw_sec_ind_aut_yn_100 ~ ma.nat.donal.log.st +
                   war_hist_terr_cw + i(time_since_cw_terr_combined) + i(time_since_ind_aut) | seg_fe + year,
                 data=rail.df.sample,
                 cluster="seg_fe")

mech.state <-  feols(separatism_cw_sec_ind_aut_yn_100 ~ state_cap.st +
                       war_hist_terr_cw +  i(time_since_cw_terr_combined) + i(time_since_ind_aut) | seg_fe + year,
                     data=rail.df.sample,
                     cluster="seg_fe")

mech.int <- feols(separatism_cw_sec_ind_aut_yn_100 ~ mob_cap.st +
                    war_hist_terr_cw + i(time_since_cw_terr_combined) + i(time_since_ind_aut) | seg_fe + year,
                  data=rail.df.sample,
                  cluster="seg_fe")

mech.all <-  feols(separatism_cw_sec_ind_aut_yn_100 ~ ma.nat.donal.log.st + state_cap.st + mob_cap.st +
                     war_hist_terr_cw + i(time_since_cw_terr_combined) + i(time_since_ind_aut) | seg_fe + year,
                   data=rail.df.sample,
                   cluster="seg_fe")


## Prepare regression table
m.list <- list(
  mech.ma,
  mech.state,
  mech.int,
  mech.all
)

vcov.list <- list(mech.ma$cov.scaled,
                  mech.state$cov.scaled,
                  mech.int$cov.scaled,
                  mech.all$cov.scaled)

gm <- list(
  list("raw" = "nobs", "clean" = "Observations", "fmt" = 0)
)
rows <- tribble(~term, ~M1,  ~M2, ~M3, ~M4,
                "Segment FE", "Yes",  "Yes", "Yes", "Yes",
                "Year FE", "Yes",  "Yes", "Yes", "Yes",
                "\\midrule Mean DV",
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100[mech.ma$obs_selection$obsRemoved]),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100[mech.all$obs_selection$obsRemoved]),3))
)
attr(rows, 'position') <- c(7:9)
cap <-  "Network Structure and Causal Mechanisms \\label{tab:rails.network.st}"
note <- "\\\\scriptsize{\\\\textit{Notes:} The unit of analysis is the ethnic segment year.
State-leading segments and segments smaller than 2000 sqkm dropped.  All models control for the number of past conflicts and peace years indicators.
Segment clustered standard errors in parentheses. All explanatory variables are standardized.
\\\\textit{+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001.}}"

# make table
tab.out <- modelsummary(m.list,
                        coef_map=c("ma.nat.donal.log.st"= "National Market Access",
                                   "state_cap.st" = "State Reach",
                                   "mob_cap.st" = "Internal Connectivity"),
                        estimate=c("{estimate}{stars}"),
                        gof_map = gm, stars=T,
                        vcov = vcov.list,
                        add_rows = rows,
                        title=cap,
                        escape=F,
                        output="latex") %>%
  kable_styling(font_size = 9) %>%
  footnote(general=c(note),threeparttable = T, escape = F, general_title = "")%>%
  column_spec(2:5,width="2.5cm") %>%
  add_header_above(header=c(" "=1,"100 $\\\\times$ Secession, Terr. CW or Claim"=4),escape=F)

# write out
kableExtra::save_kable(tab.out, file = here(tab.path,"table_a17.tex"))



##### ##### ##### ##### ##### ##### ##### ##### ##### #####
##### Estimate Models with segment and country-year FE ----
##### ##### ##### ##### ##### ##### ##### ##### ##### #####

mech.ma <- feols(separatism_cw_sec_ind_aut_yn_100 ~ ma.nat.donal.log +
                   war_hist_terr_cw+ i(time_since_separatism_cw_sec_ind_aut) +  i(time_since_ind_aut)  | seg_fe + country_year,
                 data=rail.df.sample,
                 cluster="seg_fe")

mech.state <-  feols(separatism_cw_sec_ind_aut_yn_100 ~ state_cap +
                       war_hist_terr_cw + i(time_since_cw_terr_combined) + i(time_since_ind_aut)  | seg_fe + country_year,
                     data=rail.df.sample,
                     cluster="seg_fe")

mech.int <- feols(separatism_cw_sec_ind_aut_yn_100 ~ mob_cap +
                    war_hist_terr_cw+ i(time_since_cw_terr_combined) + i(time_since_ind_aut) | seg_fe + country_year,
                  data=rail.df.sample,
                  cluster="seg_fe")

mech.all <-  feols(separatism_cw_sec_ind_aut_yn_100 ~ exp(ma.nat.donal.log) + state_cap + mob_cap +
                     war_hist_terr_cw+
                     i(time_since_cw_terr_combined) + i(time_since_ind_aut) | seg_fe + country_year,
                   data=rail.df.sample[rail.df.sample$rail_ttt_5years_binned!=1000,],
                   cluster="seg_fe")


## Prepare regression table
m.list <- list(
  mech.ma,
  mech.state,
  mech.int,
  mech.all
)

vcov.list <- list(mech.ma$cov.scaled,
                  mech.state$cov.scaled,
                  mech.int$cov.scaled,
                  mech.all$cov.scaled)

gm <- list(
  list("raw" = "nobs", "clean" = "Observations", "fmt" = 0)
)
rows <- tribble(~term, ~M1,  ~M2, ~M3, ~M4,
                "Segment FE", "Yes",  "Yes", "Yes", "Yes",
                "Country-Year FE", "Yes",  "Yes", "Yes", "Yes",
                "\\midrule Mean DV",
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100[mech.ma$obs_selection$obsRemoved]),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100[mech.all$obs_selection$obsRemoved]),3))
)
attr(rows, 'position') <- c(7:9)
cap <-  "Network Structure (Country-Year Fixed Effects) \\label{tab:rails.network.cy}"
note <- "\\\\scriptsize{\\\\textit{Notes:} The unit of analysis is the ethnic segment year.
State-leading segments and segments smaller than 2000 sqkm dropped.  All models control for the number of past conflicts and peace years indicators.
Segment clustered standard errors in parentheses.
\\\\textit{+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001.}}"

# make table
tab.out <- modelsummary(m.list,
                        coef_map=c("ma.nat.donal.log"= "National Market Access",
                                   "state_cap" = "State Reach",
                                   "mob_cap" = "Internal Connectivity"),
                        estimate=c("{estimate}{stars}"),
                        gof_map = gm, stars=T,
                        vcov = vcov.list,
                        add_rows = rows,
                        title=cap,
                        escape=F,
                        output="latex") %>%
  kable_styling(font_size = 9) %>%
  footnote(general=c(note),threeparttable = T, escape = F, general_title = "")%>%
  column_spec(2:5,width="2.5cm") %>%
  add_header_above(header=c(" "=1,"100 $\\\\times$ Secession, Terr. CW or Claim"=4),escape=F)

# write out
kableExtra::save_kable(tab.out, file = here(tab.path,"table_a18.tex"))



##### ##### ##### ##### ##### ##### ##### ##### ##### #####
##### Estimate Models with segment and year FE and leads ----
##### ##### ##### ##### ##### ##### ##### ##### ##### #####

## Models
mech.ma <- feols(as.formula(paste("separatism_cw_sec_ind_aut_yn_100 ~ ma.nat.donal.log + ",
                                  paste(paste0("ma.nat.donal.log", ".lead"), collapse = "+"),
                                  " + war_hist_terr_cw  | seg_fe + year + time_since_cw_terr_combined + time_since_ind_aut")),
                 data=rail.df.sample,
                 cluster="seg_fe")

mech.state <-  feols(as.formula(paste("separatism_cw_sec_ind_aut_yn_100 ~ state_cap+",
                                      paste(paste0("state_cap", ".lead"), collapse = "+"),
                                      " + war_hist_terr_cw  | seg_fe + year + time_since_cw_terr_combined + time_since_ind_aut")),
                     data=rail.df.sample,
                     cluster="seg_fe")

mech.int <- feols(as.formula(paste("separatism_cw_sec_ind_aut_yn_100 ~ mob_cap+ ",
                                   paste(paste0("mob_cap", ".lead"), collapse = "+"),
                                   " + war_hist_terr_cw  | seg_fe + year + time_since_cw_terr_combined + time_since_ind_aut")),
                  data=rail.df.sample,
                  cluster="seg_fe")

mech.all <-  feols(as.formula(paste("separatism_cw_sec_ind_aut_yn_100 ~ ma.nat.donal.log + state_cap + mob_cap+",
                                    paste(paste0(lead.vars, ".lead"), collapse = "+"),
                                    "+ war_hist_terr_cw  | seg_fe + year + time_since_cw_terr_combined + time_since_ind_aut")),
                   data=rail.df.sample,
                   cluster="seg_fe")

## Prepare regression table
m.list <- list(
  mech.ma,
  mech.state,
  mech.int,
  mech.all
)

vcov.list <- list(mech.ma$cov.scaled,
                  mech.state$cov.scaled,
                  mech.int$cov.scaled,
                  mech.all$cov.scaled)

gm <- list(
  list("raw" = "nobs", "clean" = "Observations", "fmt" = 0)
)
rows <- tribble(~term, ~M1,  ~M2, ~M3, ~M4,
                "Segment FE", "Yes",  "Yes", "Yes", "Yes",
                "Year FE", "Yes",  "Yes", "Yes", "Yes",
                "\\midrule Mean DV",
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100[mech.ma$obs_selection$obsRemoved]),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100[mech.all$obs_selection$obsRemoved]),3))
)
attr(rows, 'position') <- c(7:9) + 6
cap <-  "Network Structure and Causal Mechanisms  with 5-year Leads \\label{tab:rails.network.leads}"
note <- "\\\\scriptsize{\\\\textit{Notes:} The unit of analysis is the ethnic segment year.
State-leading segments and segments smaller than 2000 sqkm dropped.  All models control for the number of past conflicts and peace years indicators.
Segment clustered standard errors in parentheses.
\\\\textit{+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001.}}"

# make table
tab.out <- modelsummary(m.list,
                        coef_map=c("ma.nat.donal.log"= "National Market Access",
                                   "state_cap" = "State Reach",
                                   "mob_cap" = "Internal Connectivity",
                                   "ma.nat.donal.log.lead"= "$\\Delta$ National Market Access $t_{+5}-t_0$",
                                   "state_cap.lead" = "$\\Delta$ State Reach $t_{+5}-t_0$",
                                   "mob_cap.lead" = "$\\Delta$ Internal Connectivity $t_{+5}-t_0$"),
                        estimate=c("{estimate}{stars}"),
                        gof_map = gm, stars=T,
                        vcov = vcov.list,
                        add_rows = rows,
                        title=cap,
                        escape=F,
                        output="latex") %>%
  kable_styling(font_size = 9) %>%
  footnote(general=c(note),threeparttable = T, escape = F, general_title = "")%>%
  column_spec(2:5,width="2.5cm") %>%
  add_header_above(header=c(" "=1,"100 $\\\\times$ Secession, Terr. CW or Claim"=4),escape=F)

# write out
kableExtra::save_kable(tab.out, file = here(tab.path,"table_a19.tex"))



##### ##### ##### ##### ##### ##### ##### ##### ##### #####
##### Estimate Models with segment and year FE and leads -----
##### ##### ##### ##### ##### ##### ##### ##### ##### #####

mech.ma <- feols(as.formula(paste("separatism_cw_sec_ind_aut_yn_100 ~ ma.nat.donal.log + ",
                                  paste(paste0("ma.nat.donal.log", ".lead"), collapse = "+"),
                                  " + war_hist_terr_cw  | seg_fe + country_year + time_since_cw_terr_combined + time_since_ind_aut")),
                 data=rail.df.sample,
                 cluster="seg_fe")

mech.state <-  feols(as.formula(paste("separatism_cw_sec_ind_aut_yn_100 ~ state_cap+",
                                      paste(paste0("state_cap", ".lead"), collapse = "+"),
                                      " + war_hist_terr_cw  | seg_fe + country_year + time_since_cw_terr_combined + time_since_ind_aut")),
                     data=rail.df.sample,
                     cluster="seg_fe")

mech.int <- feols(as.formula(paste("separatism_cw_sec_ind_aut_yn_100 ~ mob_cap+ ",
                                   paste(paste0("mob_cap", ".lead"), collapse = "+"),
                                   " + war_hist_terr_cw  | seg_fe + country_year + time_since_cw_terr_combined + time_since_ind_aut")),
                  data=rail.df.sample,
                  cluster="seg_fe")

mech.all <-  feols(as.formula(paste("separatism_cw_sec_ind_aut_yn_100 ~ ma.nat.donal.log + state_cap + mob_cap+",
                                    paste(paste0(lead.vars, ".lead"), collapse = "+"),
                                    "+ war_hist_terr_cw  | seg_fe + country_year + time_since_cw_terr_combined + time_since_ind_aut")),
                   data=rail.df.sample,
                   cluster="seg_fe")

## Prepare regression table
m.list <- list(
  mech.ma,
  mech.state,
  mech.int,
  mech.all
)

vcov.list <- list(mech.ma$cov.scaled,
                  mech.state$cov.scaled,
                  mech.int$cov.scaled,
                  mech.all$cov.scaled)

gm <- list(
  list("raw" = "nobs", "clean" = "Observations", "fmt" = 0)
)
rows <- tribble(~term, ~M1,  ~M2, ~M3, ~M4,
                "Segment FE", "Yes",  "Yes", "Yes", "Yes",
                "Year FE", "Yes",  "Yes", "Yes", "Yes",
                "\\midrule Mean DV",
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100[mech.ma$obs_selection$obsRemoved]),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100[mech.all$obs_selection$obsRemoved]),3))
)
attr(rows, 'position') <- c(7:9) + 6
cap <-  "Network Structure and Causal Mechanisms with 5-year Leads and Country-Year FEs \\label{tab:rails.network.leads.cy}"
note <- "\\\\scriptsize{\\\\textit{Notes:} The unit of analysis is the ethnic segment year.
State-leading segments and segments smaller than 2000 sqkm dropped.  All models control for the number of past conflicts and peace years indicators.
Segment clustered standard errors in parentheses.
\\\\textit{+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001.}}"

# make table
tab.out <- modelsummary(m.list,
                        coef_map=c("ma.nat.donal.log"= "National Market Access",
                                   "state_cap" = "State Reach",
                                   "mob_cap" = "Internal Connectivity",
                                   "ma.nat.donal.log.lead"= "$\\Delta$ National Market Access $t_{+5}-t_0$",
                                   "state_cap.lead" = "$\\Delta$ State Reach $t_{+5}-t_0$",
                                   "mob_cap.lead" = "$\\Delta$ Internal Connectivity $t_{+5}-t_0$"),
                        estimate=c("{estimate}{stars}"),
                        gof_map = gm, stars=T,
                        vcov = vcov.list,
                        add_rows = rows,
                        title=cap,
                        escape=F,
                        output="latex") %>%
  kable_styling(font_size = 9) %>%
  footnote(general=c(note),threeparttable = T, escape = F, general_title = "")%>%
  column_spec(2:5,width="2.5cm") %>%
  add_header_above(header=c(" "=1,"100 $\\\\times$ Secession, Terr. CW or Claim"=4),escape=F)
tab.out
# write out
kableExtra::save_kable(tab.out, file = here(tab.path,"table_a20.tex"))


# clean up
rm(mech.all, mech.int, mech.ma, mech.state, p, dat_plot, vcov.list, m.list,
   vcov.list, gm, cap, note, tab.out, f.stat, plot.data) %>%
  suppressWarnings()
