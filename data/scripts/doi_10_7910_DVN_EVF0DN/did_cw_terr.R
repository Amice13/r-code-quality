### analysis: secessionist conflict onset as outcome only
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
config_modelsummary(factory_latex = "kableExtra")

setFixest_notes(FALSE)

### define output paths
tab.path <- here("tables")
fig.path <- here("figures")

### load segment data
rail.df <- readRDS(here("analysis_data","analysis_rails_df.rds"))

#### multiply outcomes by 100 for percentage point interpretation
rail.df$cw_terr_onset_combined_100 <- 100*rail.df$cw_terr_onset_combined

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

m1.twfe <- feols(cw_terr_onset_combined_100 ~ rail_any_yn  + war_hist_terr_cw + i(time_since_cw_terr_combined) + i(time_since_ind_aut)   | seg_fe + year,
                 data=rail.df.sample,
                 cluster="seg_fe")

m1.did2s <- did2s(rail.df.sample,
                  yname = "cw_terr_onset_combined_100", 
                  first_stage = ~ 0 + war_hist_terr_cw + i(time_since_cw_terr_combined,ref=1) + i(time_since_ind_aut,ref=1) | seg_fe + year, 
                  second_stage = ~ rail_any_yn,
                  treatment="rail_any_yn",
                  cluster_var = "seg_fe")
ATT.y <- m1.did2s$coefficients[1]
p.val.y <- wald(m1.did2s)$p

m2.twfe <- feols(cw_terr_onset_combined_100 ~ rail_any_yn +
                   war_hist_terr_cw + i(time_since_cw_terr_combined) + i(time_since_ind_aut) | seg_fe + country_year,
                 data=rail.df.sample,
                 cluster="seg_fe")

m2.did2s <- did2s(rail.df.sample,
                  yname = "cw_terr_onset_combined_100",
                  first_stage = ~ 0 +
                    war_hist_terr_cw + i(time_since_cw_terr_combined,ref=1) + i(time_since_ind_aut,ref=1) | seg_fe + country_year, 
                  second_stage = ~ rail_any_yn ,
                  treatment="rail_any_yn",
                  cluster_var = "seg_fe")
ATT.cy <- m2.did2s$coefficients[1]
p.val.cy <- wald(m2.did2s)$p



## Prepare regression table
m.list <- list(
  m1.twfe,
  m2.twfe,
  m1.did2s,
  m2.did2s
)

vcov.list <- list(m1.twfe$cov.scaled,
                  m2.twfe$cov.scaled,
                  m1.did2s$cov.scaled,
                  m2.did2s$cov.scaled)

gm <- list(
  list("raw" = "nobs", "clean" = "Observations", "fmt" = 0)
)
rows <- tribble(~term, ~M1,  ~M2, ~M3, ~M4,
                "Segment FE", "Yes",  "Yes", "Yes", "Yes",
                "Year FE", "Yes",  "No", "Yes", "No",
                "Country-Year FE", "No",  "Yes", "No", "Yes",
                "Estimator", "TWFE",  "TWFE", "2S-DiD", "2S-DiD",
                "\\midrule Mean DV",
                paste(round(mean(rail.df.sample$cw_terr_onset_combined_100),3)),
                paste(round(mean(rail.df.sample$cw_terr_onset_combined_100),3)),
                paste(round(mean(rail.df.sample$cw_terr_onset_combined_100[m1.did2s$obs_selection$obsRemoved]),3)),
                paste(round(mean(rail.df.sample$cw_terr_onset_combined_100[m2.did2s$obs_selection$obsRemoved]),3))
)
attr(rows, 'position') <- c(3:7)
cap <-  "Railroads and Separatist Conflict (1816-1945) \\label{tab:did.cw.terr}"
note <- "\\\\scriptsize{\\\\textit{Notes:} The unit of analysis is the ethnic segment year. 
State-leading segments and segments smaller than 2000 sqkm dropped. All models control for the number of past conflicts and peace years indicators.
Segment clustered standard errors in parentheses.
\\\\textit{+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001.}}" 

# make table
tab.out <- modelsummary(m.list,
                        coef_map=c("rail_any_yn"= "Rails (Y/N)"),
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
  add_header_above(header=c(" "=1,"100 $\\\\times$ Terr. CW"=4),escape=F)

# write out
kableExtra::save_kable(tab.out, file = here(tab.path,"table_a9.tex"))



##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### event study year fe
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
es.y <- did2s(rail.df.sample,
              yname = "cw_terr_onset_combined_100", 
              first_stage = ~ 0 +war_hist_terr_cw + i(time_since_cw_terr_combined,ref=1) + i(time_since_ind_aut,ref=1) | seg_fe + year, 
              second_stage = ~ i(rail_ttt_5years_binned, ref=c(-5,-1000,1000)),
              treatment="rail_any_yn",
              cluster_var = "seg_fe")

###  plot
beta <- coef(es.y)[grep("rail_ttt_5years_binned",names(coef(es.y)))]
se <- sqrt(diag(es.y$cov.scaled))[grep("rail_ttt_5years_binned",names(coef(es.y)))]


plot.data <- data.frame(beta=beta,se=se)
plot.data$lb <- plot.data$beta-1.96*plot.data$se
plot.data$ub <- plot.data$beta+1.96*plot.data$se
plot.data$at <- c(seq(-60,-10,5),seq(0,90,5))
plot.data <- rbind(plot.data,c(0,0,0,0,-5))
plot.data <- plot.data[order(plot.data$at),]

lab.att <- paste0("ATT = ", round(ATT.y,3), "\n p.val = ", format(round(p.val.y,4), scientific=F))

p <- ggplot(plot.data) + 
  geom_ribbon(aes(x=at,ymin=lb,ymax=ub),color="grey75", fill="grey75",alpha=0.4)+
  geom_segment(aes(x=-5,y=ATT.y, xend=90,yend=ATT.y),color="red",lty="dashed") +
  geom_linerange(aes(x=at,ymin=lb,ymax=ub),linewidth=0.5, color="grey50") +
  geom_line(aes(x=at,y=beta)) +
  geom_hline(yintercept=0,color="grey40") +
  geom_vline(xintercept=-5,color="grey40",linetype="dashed") +
  geom_point(aes(x=at,y=beta)) +
  geom_text(aes(x=13,y=3),label=lab.att,col="red")+
  scale_x_continuous(name="Time to/since first railway (5-year bins)",n.breaks=9) +
  scale_y_continuous(name="Estimate and 95% CI") +
  labs(title = "Railroad Access and Separatist Conflict",
       subtitle = "Event Study Results")+
  theme_minimal(base_size=14) +
  theme(panel.grid.minor = element_blank())
p  
ggsave(here(fig.path,"figure_a9.pdf"),width=7.5,height=5)



##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### event study country-year fe
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 

es.cy <- did2s(rail.df.sample,
               yname = "cw_terr_onset_combined_100", 
               first_stage = ~ 0 +  war_hist_terr_cw + i(time_since_cw_terr_combined) + i(time_since_ind_aut) | seg_fe + country_year, 
               second_stage = ~ i(rail_ttt_5years_binned, ref=c(-5,-1000,1000)),
               treatment="rail_any_yn",
               cluster_var = "seg_fe")

### prepare plot
beta <- coef(es.cy)[grep("rail_ttt_5years_binned",names(coef(es.cy)))]
se <- sqrt(diag(es.cy$cov.scaled))[grep("rail_ttt_5years_binned",names(coef(es.cy)))]

plot.data <- data.frame(beta=beta,se=se)
plot.data$lb <- plot.data$beta-1.96*plot.data$se
plot.data$ub <- plot.data$beta+1.96*plot.data$se
plot.data$at <- c(seq(-60,-10,5),seq(0,90,5))
plot.data <- rbind(plot.data,c(0,0,0,0,-5))
plot.data <- plot.data[order(plot.data$at),]

lab.att <- paste0("ATT = ", round(ATT.cy,3), "\n p.val = ", format(round(p.val.cy,4), scientific=F))

p <- ggplot(plot.data) + 
  geom_ribbon(aes(x=at,ymin=lb,ymax=ub),color="grey75", fill="grey75",alpha=0.4)+
  geom_segment(aes(x=-5,y=ATT.cy, xend=90,yend=ATT.cy),color="red",lty="dashed") +
  geom_linerange(aes(x=at,ymin=lb,ymax=ub),linewidth=0.5, color="grey50") +
  geom_line(aes(x=at,y=beta)) +
  geom_hline(yintercept=0,color="grey40") +
  geom_vline(xintercept=-5,color="grey40",linetype="dashed") +
  geom_point(aes(x=at,y=beta)) +
  geom_text(aes(x=83,y=2.5),label=lab.att,col="red")+
  scale_x_continuous(name="Time to/since first railway (5-year bins)",n.breaks=9) +
  scale_y_continuous(name="Estimate and 95% CI") +
  labs(title = "Railroad Access and Separatist Conflict",
       subtitle = "Event Study Results")+
  theme_minimal(base_size=14) + 
  theme(panel.grid.minor = element_blank())
p  
# ggsave(here(fig.path,"event_study_cy_cw.pdf"),width=7.5,height=5)


# clean up ----
rm(m1.twfe, m1.did2s, m2.twfe, m2.did2s, p, m.list, vcov.list, gm, cap, note, tab.out,
   es.cy, es.y, f.stat) %>% 
  suppressWarnings()








