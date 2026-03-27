### analysis: tests causal mechanisms on 1816-1922 sample
library(dplyr)
library(here)
library(fixest)
library(ggplot2)
library(did2s)
library(modelsummary)
library(kableExtra)
library(devtools)
library(interflex)

setFixest_notes(FALSE)
config_modelsummary(factory_latex = "kableExtra")

### define output paths
tab.path <- here("tables")
fig.path <- here("figures")

# Define end-of-sample year
year_end <- 1922

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
                                  rail.df$year>1815  & rail.df$year<=year_end &
                                  rail.df$capital_yn_dist_corr==0 &
                                  rail.df$area_sqkm_raw>2000 &
                                  !rail.df$name_cap_rule_corr%in%drop_cntrs),]

## Make Leads
lead.vars <- c("ma.nat.donal.log", "state_cap", "mob_cap")
rail.df.sample[,paste0(lead.vars, ".lead")] <- NA
for(t in c(1, 2,5)){
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
for(t in c(1, 2,5)){
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

mech.ma <- feols(separatism_cw_sec_ind_aut_yn_100 ~ ma.nat.donal.log+
                   war_hist_terr_cw +  i(time_since_cw_terr_combined) + i(time_since_ind_aut) | seg_fe + year,
                 data=rail.df.sample,
                 cluster="seg_fe")

mech.state <-  feols(separatism_cw_sec_ind_aut_yn_100 ~ state_cap +
                       war_hist_terr_cw +  i(time_since_cw_terr_combined) + i(time_since_ind_aut) | seg_fe + year,
                     data=rail.df.sample,
                     cluster="seg_fe")

mech.int <- feols(separatism_cw_sec_ind_aut_yn_100 ~ mob_cap +
                    war_hist_terr_cw +  i(time_since_cw_terr_combined) + i(time_since_ind_aut) | seg_fe + year,
                  data=rail.df.sample,
                  cluster="seg_fe")

mech.all <-  feols(separatism_cw_sec_ind_aut_yn_100 ~ ma.nat.donal.log + state_cap + mob_cap+
                     war_hist_terr_cw +  i(time_since_cw_terr_combined) + i(time_since_ind_aut)  | seg_fe + year,
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
cap <-  "Network Structure and Causal Mechanisms \\label{tab:rails.network.1922}"
note <- "\\\\scriptsize{\\\\textit{Notes:} The unit of analysis is the ethnic segment year. 
State-leading segments and segments smaller than 2000 sqkm dropped. 
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
kableExtra::save_kable(tab.out, 
                       file = here(tab.path, paste0("table_a7.tex")))

# clean up
rm(mech.all, mech.int, mech.ma, mech.state, p, dat_plot, vcov.list, m.list, 
   vcov.list, gm, cap, note, tab.out, f.stat, plot.data) %>% 
  suppressWarnings()


