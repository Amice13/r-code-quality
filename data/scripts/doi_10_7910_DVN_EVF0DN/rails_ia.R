### analysis: interaction models
library(tidyverse)
library(here)
library(fixest)
library(did2s)
library(modelsummary)
library(kableExtra)
library(broom)
library(interflex)
library(car)
library(fastDummies)

setFixest_notes(FALSE)
config_modelsummary(factory_latex = "kableExtra")

### define output paths
tab.path <- here("tables")
fig.path <- here("figures")

### load segment data
rail.df <- readRDS(here("analysis_data","analysis_rails_df.rds"))

#### multiply outcomes by 100 for percentage point interpretation
rail.df$separatism_cw_sec_ind_aut_yn_100 <- 100*rail.df$separatism_cw_sec_ind_aut_yn
# rail.df$separatism_cw_sec_ind_aut_yn_do_100 <- 100*rail.df$separatism_cw_sec_ind_aut_yn_do
# rail.df$separatism_cw_sec_ind_yn_100 <- 100*rail.df$separatism_cw_sec_ind_yn
# rail.df$separatism_cw_sec_ind_yn_do_100 <- 100*rail.df$separatism_cw_sec_ind_yn_do
### define countries to exclude from analysis
drop_cntrs <- c("United Kingdom","Egypt","Cherkessy","Ireland")

## code country_year fe
rail.df$country_year <- factor(paste(rail.df$id_cap_rule_corr,rail.df$year,sep="_"))

rail.df$popc_log <- log(rail.df$popc_corr)

##### subset to analysis sample
rail.df.sample <- rail.df[which(rail.df$rail_ttt_5years_binned%in%c(seq(-60,90,5),-1000,1000) &
                                  rail.df$year>1815  & rail.df$year<1946 &
                                  rail.df$capital_yn_dist_corr==0 &
                                  rail.df$area_sqkm_raw>2000 &
                                  !rail.df$name_cap_rule_corr%in%drop_cntrs),]

rail.df.sample <- dummy_cols(rail.df.sample,select_columns = c("time_since_cw_terr_combined","time_since_ind_aut"))
cntr <- c(paste0("time_since_cw_terr_combined_",c(2:130)),
          paste0("time_since_ind_aut_",c(2:130)),
          "war_hist_terr_cw")



##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### Estimate Models ----
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
m0.ia.base <- feols(separatism_cw_sec_ind_aut_yn_100 ~ rail_any_yn*distance +
                      war_hist_terr_cw + i(time_since_cw_terr_combined) + i(time_since_ind_aut)  | seg_id + year, 
                    data=rail.df.sample,cluster="seg_id")

m1.ia.base <- feols(separatism_cw_sec_ind_aut_yn_100 ~ rail_any_yn*pop_share_cntr_alt_cap +
                      war_hist_terr_cw + i(time_since_cw_terr_combined) + i(time_since_ind_aut) | seg_id + year, 
                    data=rail.df.sample,cluster="seg_id")

m2.ia.base <- feols(separatism_cw_sec_ind_aut_yn_100 ~ rail_any_yn*popc_log +
                      war_hist_terr_cw + i(time_since_cw_terr_combined) + i(time_since_ind_aut) | seg_id + year, 
                    data=rail.df.sample,cluster="seg_id")

m3.ia.base <- feols(separatism_cw_sec_ind_aut_yn_100 ~ rail_any_yn*gdppc_log +
                      war_hist_terr_cw + i(time_since_cw_terr_combined) + i(time_since_ind_aut)  | seg_id + year, 
                    data=rail.df.sample,cluster="seg_id")

m4.ia.base <- feols(separatism_cw_sec_ind_aut_yn_100 ~ rail_any_yn*v2stfisccap +
                      war_hist_terr_cw + i(time_since_cw_terr_combined) + i(time_since_ind_aut)  | seg_id + year, 
                    data=rail.df.sample,cluster="seg_id")

m5.ia.base <- feols(separatism_cw_sec_ind_aut_yn_100 ~ rail_any_yn*v2x_liberal+
                      war_hist_terr_cw + i(time_since_cw_terr_combined) + i(time_since_ind_aut)   | seg_id + year, 
                    data=rail.df.sample,cluster="seg_id")


#### make tables
m.list <- list(
  m0.ia.base,
  m1.ia.base,
  m2.ia.base,
  m3.ia.base,
  m4.ia.base,
  m5.ia.base
)


vcov.list <- list(m0.ia.base$cov.scaled,
                  m1.ia.base$cov.scaled,
                  m2.ia.base$cov.scaled,
                  m3.ia.base$cov.scaled,
                  m4.ia.base$cov.scaled,
                  m5.ia.base$cov.scaled)

gm <- list(
  list("raw" = "nobs", "clean" = "Observations", "fmt" = 0)
)
rows <- tribble(~term, ~M1,  ~M2, ~M3, ~M4, ~M4, ~M6,
                "Segment FE", "Yes",  "Yes", "Yes", "Yes","Yes","Yes",
                "Year FE", "Yes",  "Yes", "Yes", "Yes","Yes","Yes",
                "\\midrule Mean DV",
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100[m3.ia.base$obs_selection$obsRemoved]),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100[m4.ia.base$obs_selection$obsRemoved]),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100[m5.ia.base$obs_selection$obsRemoved]),3))
)
attr(rows, 'position') <- c(25:27)
cap <-  "Separatism: Linear Interaction Models \\label{tab:rails.ia}"
note <- "\\\\scriptsize{\\\\textit{Notes:} The unit of analysis is the ethnic segment year. 
State-leading segments and segments smaller than 2000 sqkm dropped. 
Segment clustered standard errors in parentheses.
\\\\textit{+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001.}}" 

# make table
tab.out <- modelsummary(m.list,
                        coef_map=c("rail_any_yn"= "Rails (Y/N)",
                                   "rail_any_yn:distance" = "Rails $\\times$ Ling. Dist to Core",
                                   "pop_share_cntr_alt_cap"="Pop. Share Core Group",
                                   "rail_any_yn:pop_share_cntr_alt_cap" = "Rails $\\times$ Pop. Share Core",
                                   "popc_log"="Group Population (log)",
                                   "rail_any_yn:popc_log" = "Rails $\\times$ Group Pop.",
                                   "gdppc_log"="GDP per capita (log)",
                                   "rail_any_yn:gdppc_log" = "Rails $\\times$ GDP p.c.",
                                   "v2stfisccap"="Fiscal Capacity (VDEM)",
                                   "rail_any_yn:v2stfisccap" = "Rails $\\times$ Fiscal Cap.",
                                   "v2x_liberal"="Liberal Democracy (VDEM)",
                                   "rail_any_yn:v2x_liberal" = "Rails $\\times$ Lib. Dem."
                                   ),
                        estimate=c("{estimate}{stars}"),
                        gof_map = gm, stars=T, 
                        vcov = vcov.list,
                        add_rows = rows,
                        title=cap,
                        escape=F,
                        output="latex") %>%
  kable_styling(font_size = 9) %>%
  footnote(general=c(note),threeparttable = T, escape = F, general_title = "")%>%
  column_spec(2:5,width="1.5cm") %>%
  add_header_above(header=c(" "=1,"100 $\\\\times$ Secession, Terr. CW or Claim"=6),escape=F)

# write out
kableExtra::save_kable(tab.out, file = here(tab.path,"table_a13.tex"))


#################################################################################
#################### Binning estimates and plots ----
#################################################################################

m0.ia <- interflex(estimator="binning", 
                   data=rail.df.sample,
                   Y="separatism_cw_sec_ind_aut_yn", D="rail_any_yn", X="distance",
                   Z=cntr,FE=c("seg_id","year"), vcov.type = "cluster", cl="seg_id", 
                   na.rm=T, nbins=4, full.moderate=F,Xunif = F)

plot.interflex(m0.ia,ylab="Marginal Effect of Rail (Y/N)",
               xlab="Moderator: Linguistic Distance",
               ylim = c(-0.05, 0.05),
               theme.bw = T, show.grid = F, bin.labs = F)
ggsave(here("figures", "figure_4a.pdf"),
       width = 8, height = 5.6)

deltaMethod(m0.ia$model.bin,"D.Group.2.G.1-D.Group.2.G.3",level=0.95)
deltaMethod(m0.ia$model.bin,"D.Group.2.G.1-D.Group.2.G.2",level=0.95)
deltaMethod(m0.ia$model.bin,"D.Group.2.G.2-D.Group.2.G.3",level=0.95)


m1.ia <- interflex(estimator="binning", data=rail.df.sample,
                   Y="separatism_cw_sec_ind_aut_yn", D="rail_any_yn", X="pop_share_cntr_alt_cap",
                   Z=cntr,FE=c("seg_id","year"), vcov.type = "cluster", cl="seg_id", 
                   na.rm=T, nbins=3, full.moderate=F,Xunif = F)

plot.interflex(m1.ia,ylab="Marginal Effect of Rail (Y/N)",
               xlab="Moderator: Pop. Share Core Group",
               ylim = c(-0.05, 0.05),
               theme.bw = T, show.grid = F, bin.labs = F)
ggsave(here("figures", "figure_4b.pdf"),
       width = 8, height = 5.6)

deltaMethod(m1.ia$model.bin,"D.Group.2.G.1-D.Group.2.G.3",level=0.95)
deltaMethod(m1.ia$model.bin,"D.Group.2.G.1-D.Group.2.G.2",level=0.95)
deltaMethod(m1.ia$model.bin,"D.Group.2.G.2-D.Group.2.G.3",level=0.95)


m2.ia <- interflex(estimator="binning", 
                   data=rail.df.sample,
                   Y="separatism_cw_sec_ind_aut_yn", D="rail_any_yn", X="popc_log",
                   Z=cntr,FE=c("seg_id","year"), vcov.type = "cluster", cl="seg_id", 
                   na.rm=T, nbins=3, full.moderate=F,Xunif = F)

plot.interflex(m2.ia,ylab="Marginal Effect of Rail (Y/N)",
               xlab="Moderator: Segment Pop. (log)",
               ylim = c(-0.05, 0.05),
               theme.bw = T, show.grid = F, bin.labs = F)
ggsave(here("figures", "figure_4c.pdf"),
       width = 8, height = 5.6)

deltaMethod(m2.ia$model.bin,"D.Group.2.G.1-D.Group.2.G.3",level=0.95)
deltaMethod(m2.ia$model.bin,"D.Group.2.G.1-D.Group.2.G.2",level=0.95)
deltaMethod(m2.ia$model.bin,"D.Group.2.G.2-D.Group.2.G.3",level=0.95)


m3.ia <- interflex(estimator="binning", 
                   data=rail.df.sample,
                   Y="separatism_cw_sec_ind_aut_yn", D="rail_any_yn", X="gdppc_log",
                   Z=cntr,FE=c("seg_id","year"), vcov.type = "cluster", cl="seg_id", 
                   na.rm=T, nbins=3, full.moderate=F,Xunif = F)

plot.interflex(m3.ia,ylab="Marginal Effect of Rail (Y/N)",
               xlab="Moderator: GDP per capita (log)",
               ylim = c(-0.05, 0.05),
               theme.bw = T, show.grid = F, bin.labs = F)
ggsave(here("figures", "figure_4d.pdf"),
       width = 8, height = 5.6)

deltaMethod(m3.ia$model.bin,"D.Group.2.G.1-D.Group.2.G.3",level=0.95)
deltaMethod(m3.ia$model.bin,"D.Group.2.G.1-D.Group.2.G.2",level=0.95)
deltaMethod(m3.ia$model.bin,"D.Group.2.G.2-D.Group.2.G.3",level=0.95)


m4.ia <- interflex(estimator="binning", 
                   data=rail.df.sample,
                   Y="separatism_cw_sec_ind_aut_yn", D="rail_any_yn", X="v2stfisccap",
                   Z=cntr,FE=c("seg_id","year"), vcov.type = "cluster", cl="seg_id", 
                   na.rm=T, nbins=3, full.moderate=F,Xunif = F)

plot.interflex(m4.ia,ylab="Marginal Effect of Rail (Y/N)",
               xlab="Moderator: Fiscal Capacity (VDEM)",
               ylim = c(-0.05, 0.05),
               theme.bw = T, show.grid = F, bin.labs = F)
ggsave(here("figures", "figure_4e.pdf"),
       width = 8, height = 5.6)

deltaMethod(m4.ia$model.bin,"D.Group.2.G.1-D.Group.2.G.3",level=0.95)
deltaMethod(m4.ia$model.bin,"D.Group.2.G.1-D.Group.2.G.2",level=0.95)
deltaMethod(m4.ia$model.bin,"D.Group.2.G.2-D.Group.2.G.3",level=0.95)


m5.ia <- interflex(estimator="binning", 
                   data=rail.df.sample,
                   Y="separatism_cw_sec_ind_aut_yn", D="rail_any_yn", X="v2x_liberal",
                   Z=cntr,FE=c("seg_id","year"), vcov.type = "cluster", cl="seg_id", 
                   na.rm=T, nbins=3, full.moderate=F,Xunif = F)
plot.interflex(m5.ia,ylab="Marginal Effect of Rail (Y/N)",
               xlab="Moderator: Liberal Democracy (VDEM)",
               ylim = c(-0.05, 0.05),
               theme.bw = T, show.grid = F, bin.labs = F)
ggsave(here("figures", "figure_4f.pdf"),
       width = 8, height = 5.6)

deltaMethod(m5.ia$model.bin,"D.Group.2.G.1-D.Group.2.G.3",level=0.9)
deltaMethod(m5.ia$model.bin,"D.Group.2.G.1-D.Group.2.G.2",level=0.95)
deltaMethod(m5.ia$model.bin,"D.Group.2.G.2-D.Group.2.G.3",level=0.95)


# Produce table of binned estimates
m.list <- list(
  m0.ia$model.bin,
  m1.ia$model.bin,
  m2.ia$model.bin,
  m3.ia$model.bin,
  m4.ia$model.bin,
  m5.ia$model.bin
)

gm <- list(
  list("raw" = "nobs", "clean" = "Observations", "fmt" = 0)
)
rows <- tribble(~term, ~M1,  ~M2, ~M3, ~M4, ~M4, ~M6,
                "Moderator", "Ling. Dist to Core", "Pop. Share Core Group", "Group Pop.", "GDP p.c.", "Fiscal Cap.", "Lib. Dem.",
                "Segment FE", "Yes",  "Yes", "Yes", "Yes","Yes","Yes",
                "Year FE", "Yes",  "Yes", "Yes", "Yes","Yes","Yes",
                "\\midrule Mean DV",
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100[m3.ia.base$obs_selection$obsRemoved]),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100[m4.ia.base$obs_selection$obsRemoved]),3)),
                paste(round(mean(rail.df.sample$separatism_cw_sec_ind_aut_yn_100[m5.ia.base$obs_selection$obsRemoved]),3))
)
# attr(rows, 'position') <- c(25:27)
cap <-  "Separatism: Binned Interaction Models \\label{tab:rails.ia.binned}"
note <- "\\\\scriptsize{\\\\textit{Notes:} The unit of analysis is the ethnic segment year. 
State-leading segments and segments smaller than 2000 sqkm dropped. 
Segment clustered standard errors in parentheses.
\\\\textit{+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001.}}" 

# make table
tab.out <- modelsummary(m.list, 
                        coef_omit = "time|hist|X",
                        coef_map=c("rail_any_yn"= "Rails (Y/N)",
                                   "D.Group.2.G.1" = "1st tertile",
                                   "D.Group.2.G.2" = "2nd tertile",
                                   "D.Group.2.G.3" = "3rd tertile",
                                   "rail_any_yn:distance" = "Rails $\\times$ Ling. Dist to Core",
                                   "pop_share_cntr_alt_cap"="Pop. Share Core Group",
                                   "rail_any_yn:pop_share_cntr_alt_cap" = "Rails $\\times$ Pop. Share Core",
                                   "popc_log"="Group Population (log)",
                                   "rail_any_yn:popc_log" = "Rails $\\times$ Group Pop.",
                                   "gdppc_log"="GDP per capita (log)",
                                   "rail_any_yn:gdppc_log" = "Rails $\\times$ GDP p.c.",
                                   "v2stfisccap"="Fiscal Capacity (VDEM)",
                                   "rail_any_yn:v2stfisccap" = "Rails $\\times$ Fiscal Cap.",
                                   "v2x_liberal"="Liberal Democracy (VDEM)",
                                   "rail_any_yn:v2x_liberal" = "Rails $\\times$ Lib. Dem."
                        ),
                        estimate=c("{estimate}{stars}"),
                        gof_map = gm, stars=T, 
                        add_rows = rows,
                        title=cap,
                        escape=F,
                        output="latex") %>%
  kable_styling(font_size = 9, latex_options = "hold_position") %>%
  footnote(general=c(note),threeparttable = T, escape = F, general_title = "")%>%
  column_spec(2:5,width="1.5cm") %>%
  add_header_above(header=c(" "=1,"100 $\\\\times$ Secession, Terr. CW or Claim"=6),escape=F)

# write out
kableExtra::save_kable(tab.out, file = here(tab.path,"table_a12.tex"))


# clean up ----
rm(m0.ia.base, m1.ia.base, m2.ia.base, m3.ia.base, m4.ia.base, m5.ia.base, 
   m6.ia, m5.ia, m4.ia, m3.ia, m2.ia, m1.ia,  m0.ia, 
   p, m.list, vcov.list, gm, cap, note, tab.out,
   es.cy, es.y, f.stat, plot.data) %>% 
  suppressWarnings()
gc()

