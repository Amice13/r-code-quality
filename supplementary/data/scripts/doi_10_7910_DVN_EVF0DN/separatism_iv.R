### analysis: runs IV analyses
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

### load segment data
rail.df <- readRDS(here("analysis_data","analysis_rails_df.rds"))


### load simulated rails:
simrails <- readRDS(here("analysis_data","simrails2segments_tv.rds"))
rail.df <- left_join(rail.df,simrails)

#### multiply outcomes by 100 for percentage point interpretation
rail.df$separatism_cw_sec_ind_aut_yn_100 <- 100*rail.df$separatism_cw_sec_ind_aut_yn

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



##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### Estimate Models
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 

m1 <- feols(separatism_cw_sec_ind_aut_yn_100 ~ rail_any_yn + i(time_since_cw_terr_combined) + i(time_since_ind_aut) | seg_id + year, 
            data=rail.df.sample)

s1 <- feols(rail_any_yn ~ rail_any_yn_sim +
              war_hist_terr_cw + i(time_since_cw_terr_combined) + i(time_since_ind_aut)   | seg_id + year, 
            data=rail.df.sample)

rf <- feols(separatism_cw_sec_ind_aut_yn_100 ~ rail_any_yn_sim+
              war_hist_terr_cw + i(time_since_cw_terr_combined) + i(time_since_ind_aut)   | seg_id + year, 
            data=rail.df.sample)

iv <- feols(separatism_cw_sec_ind_aut_yn_100 ~  
              war_hist_terr_cw +  i(time_since_cw_terr_combined) + i(time_since_ind_aut)   | seg_id + year  | rail_any_yn ~ rail_any_yn_sim, 
            data=rail.df.sample, cluster="seg_id")
f.stat <- fitstat(iv,"ivwald1")
f.stat[[1]]$stat
iv$coefficients[1]/m1$coefficients[1]

# prepare table
m.list <- list(
  "First Stage" = s1,
  "OLS" = m1,
  "Reduced Form" = rf,
  "Second Stage" = iv
)

gm <- list(
  list("raw" = "nobs", "clean" = "Observations", "fmt" = 0)
)
rows <- tribble(~term, ~M1,  ~M2, ~M3, ~M4,
                "Segment FE", "Yes",  "Yes", "Yes", "Yes",
                "Year FE", "Yes",  "Yes", "Yes", "Yes",
                "\\midrule First Stage F",
                paste(round(f.stat[[1]]$stat,3)),
                " ",
                " ",
                paste(round(f.stat[[1]]$stat,3)),
                "Mean DV",
                paste(round(mean(rail.df.sample$rail_any_yn),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100),3))
)
attr(rows, 'position') <- c(7:10)
cap <-  "Instrumenting Railroad Access \\label{tab:rails.iv}"
note <- "\\\\scriptsize{\\\\textit{Notes:} The unit of analysis is the ethnic segment year. 
State-leading segments and segments smaller than 2000 sqkm dropped. All models control for the number of past conflicts and peace years indicators.
Segment clustered standard errors in parentheses.
\\\\textit{+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001.}}" 


# make table
tab.out <- modelsummary(m.list,
                        coef_map=c("rail_any_yn_sim"= "Rails (Y/N, simulated)",
                                   "rail_any_yn"= "Rails (Y/N)",
                                   "fit_rail_any_yn"= "Rails (Y/N, instrumented)"),
                        estimate=c("{estimate}{stars}"),
                        gof_map = gm, stars=T, 
                        #vcov = vcov.list,
                        add_rows = rows,
                        title=cap,
                        escape=F,
                        output="latex") %>%
  kable_styling(font_size = 10) %>%
  footnote(general=c(note),threeparttable = T, escape = F, general_title = "")%>%
  column_spec(2:5,width="2.5cm") %>%
  add_header_above(header=c(" "=1,"Rails (Y/N)"=1,"100 $\\\\times$ Separatism"=3),escape=F)

# write out
kableExtra::save_kable(tab.out, file = here(tab.path,"table_2.tex"))


##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### Estimate Models
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 

m1 <- feols(separatism_cw_sec_ind_aut_yn_100 ~ rail_any_yn +i(time_since_cw_terr_combined) + i(time_since_ind_aut) | seg_id + country_year, 
            data=rail.df.sample)

s1 <- feols(rail_any_yn ~ rail_any_yn_sim +
              war_hist_terr_cw + i(time_since_cw_terr_combined) + i(time_since_ind_aut)   | seg_id + country_year, 
            data=rail.df.sample)

rf <- feols(separatism_cw_sec_ind_aut_yn_100 ~ rail_any_yn_sim+
              war_hist_terr_cw + i(time_since_cw_terr_combined) + i(time_since_ind_aut)   | seg_id + country_year, 
            data=rail.df.sample)

iv <- feols(separatism_cw_sec_ind_aut_yn_100 ~  
              war_hist_terr_cw +  i(time_since_cw_terr_combined) + i(time_since_ind_aut)   | seg_id + country_year | rail_any_yn ~ rail_any_yn_sim, 
            data=rail.df.sample, cluster="seg_id")
f.stat <- fitstat(iv,"ivwald1")
f.stat[[1]]$stat
iv$coefficients[1]/m1$coefficients[1]

m.list <- list(
  "First Stage" = s1,
  "OLS" = m1,
  "Reduced Form" = rf,
  "Second Stage" = iv
)

gm <- list(
  list("raw" = "nobs", "clean" = "Observations", "fmt" = 0)
)
rows <- tribble(~term, ~M1,  ~M2, ~M3, ~M4,
                "Segment FE", "Yes",  "Yes", "Yes", "Yes",
                "Year FE", "Yes",  "Yes", "Yes", "Yes",
                "\\midrule First Stage F",
                paste(round(f.stat[[1]]$stat,3)),
                " ",
                " ",
                paste(round(f.stat[[1]]$stat,3)),
                "Mean DV",
                paste(round(mean(rail.df.sample$rail_any_yn),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100),3))
)
attr(rows, 'position') <- c(7:10)
cap <-  "Instrumenting Railroads: Country-Year FE \\label{tab:rails.iv.cy}"
note <- "\\\\scriptsize{\\\\textit{Notes:} The unit of analysis is the ethnic segment year. 
State-leading segments and segments smaller than 2000 sqkm dropped.  All models control for the number of past conflicts and peace years indicators.
Segment clustered standard errors in parentheses.
\\\\textit{+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001.}}" 

# make table
tab.out <- modelsummary(m.list,
                        coef_map=c("rail_any_yn_sim"= "Rails (Y/N, simulated)",
                                   "rail_any_yn"= "Rails (Y/N)",
                                   "fit_rail_any_yn"= "Rails (Y/N, instrumented)"),
                        estimate=c("{estimate}{stars}"),
                        gof_map = gm, stars=T, 
                        #vcov = vcov.list,
                        add_rows = rows,
                        title=cap,
                        escape=F,
                        output="latex") %>%
  kable_styling(font_size = 9, latex_options = "hold_position") %>%
  footnote(general=c(note),threeparttable = T, escape = F, general_title = "")%>%
  column_spec(2:5,width="2.5cm") %>%
  add_header_above(header=c(" "=1,"Rails (Y/N)"=1,"100 $\\\\times$ Separatism"=3),escape=F)

# write out
kableExtra::save_kable(tab.out, file = here(tab.path,"table_a3.tex"))


# clean up
rm(m1, s1, rf, iv, p, m.list, vcov.list, gm, cap, note, tab.out,
   es.cy, es.y, f.stat) %>% 
  suppressWarnings()
