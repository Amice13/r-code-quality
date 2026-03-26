## Preamble --------------------------------------------------------------------
library(tidyverse)
library(foreign)
library(PanelMatch)

# lead and lag functions that account for missing years
lag.new <- function(x, n = 1L, along_with){
  index <- match(along_with - n, along_with, incomparable = NA)
  out <- x[index]
  attributes(out) <- attributes(x)
  out
}

lead.new <- function(x, n = 1L, along_with){
  index <- match(along_with + n, along_with, incomparable = NA)
  out <- x[index]
  attributes(out) <- attributes(x)
  out
}


## Load data: ------------------------------------------------------------------
data_panel <- read_rds(file = "data_mayors_housing_fordid.rds")


## Create panel:  --------------------------------------------------------------
data_panel <- data_panel %>% 
  select(place_fips,city,abb,year,population_est,population_2020,demshare,pid_final_win,council_approval_norez) %>%
  mutate(dem_elected = as.numeric(pid_final_win=="Dem"),
         rep_elected = as.numeric(pid_final_win=="Rep")
  ) %>%
  group_by(place_fips) %>%
  complete(year = seq.int(1970, 2020,by = 1)) %>% # expand to have all years so can get leads/lags even if values==NA in that year
  fill(place_fips,city,abb,population_2020,.direction = "downup") %>%
  arrange(year) %>%
  group_by(place_fips) %>%
  mutate(dem_in_power = lag.new(dem_elected,n=1,along_with = year),
         rep_in_power = lag.new(rep_elected,n=1,along_with = year)) %>%
  fill(dem_in_power,rep_in_power,population_est,council_approval_norez,.direction = "down") %>%
  mutate(dem_in_power = replace_na(dem_in_power,0),
         rep_in_power = replace_na(rep_in_power,0))


## Merge in CBPS data:  --------------------------------------------------------
cbps <- read_rds("cbps_allyears_newvars.rds")

## add CBPS delta 2/3 avg outcomes:
cbps <- cbps %>%
  rowwise() %>%
  mutate(
    total_bldgs_delta23avg = mean(c(total_bldgs_delta2, total_bldgs_delta3), na.rm=T),
    total_bldgs_multi_delta23avg = mean(c(total_bldgs_multi_delta2, total_bldgs_multi_delta3), na.rm=T),
    total_bldgs_single_delta23avg = mean(c(total_bldgs_single_delta2, total_bldgs_single_delta3), na.rm=T),
    
    total_units_delta23avg = mean(c(total_units_delta2, total_units_delta3), na.rm=T),
    total_units_multi_delta23avg = mean(c(total_units_multi_delta2, total_units_multi_delta3), na.rm=T),
    total_units_single_delta23avg = mean(c(total_units_single_delta2, total_units_single_delta3), na.rm=T),
    
    total_bldgs_ln_delta23avg = mean(c(total_bldgs_ln_delta2, total_bldgs_ln_delta3), na.rm=T),
    total_bldgs_multi_ln_delta23avg = mean(c(total_bldgs_multi_ln_delta2, total_bldgs_multi_ln_delta3), na.rm=T),
    total_bldgs_single_ln_delta23avg = mean(c(total_bldgs_single_ln_delta2, total_bldgs_single_ln_delta3), na.rm=T),
    
    total_units_delta23avg = mean(c(total_units_delta2, total_units_delta3), na.rm=T),
    total_units_multi_delta23avg = mean(c(total_units_multi_delta2, total_units_multi_delta3), na.rm=T),
    total_units_single_delta23avg = mean(c(total_units_single_delta2, total_units_single_delta3), na.rm=T),
    
    total_units_ln_delta23avg = mean(c(total_units_ln_delta2, total_units_ln_delta3), na.rm=T),
    total_units_multi_ln_delta23avg = mean(c(total_units_multi_ln_delta2, total_units_multi_ln_delta3), na.rm=T),
    total_units_single_ln_delta23avg = mean(c(total_units_single_ln_delta2, total_units_single_ln_delta3), na.rm=T),
    
    total_units_ln_delta234avg = mean(c(total_units_ln_delta2, total_units_ln_delta3, total_units_ln_delta4), na.rm=T),
    total_units_multi_ln_delta234avg = mean(c(total_units_multi_ln_delta2, total_units_multi_ln_delta3, total_units_multi_ln_delta4), na.rm=T),
    total_units_single_ln_delta234avg = mean(c(total_units_single_ln_delta2, total_units_single_ln_delta3, total_units_single_ln_delta4), na.rm=T),
    
    
    total_units_ln_delta1234avg = mean(c(total_units_ln_delta1,total_units_ln_delta2, total_units_ln_delta3, total_units_ln_delta4), na.rm=T),
    total_units_multi_ln_delta1234avg = mean(c(total_units_multi_ln_delta1, total_units_multi_ln_delta2, total_units_multi_ln_delta3, total_units_multi_ln_delta4), na.rm=T),
    total_units_single_ln_delta1234avg = mean(c(total_units_single_ln_delta1,total_units_single_ln_delta2, total_units_single_ln_delta3, total_units_single_ln_delta4), na.rm=T),
    
    total_units_ln_lead234avg = mean(c(total_units_ln_lead2, total_units_ln_lead3, total_units_ln_lead4), na.rm=T),
    total_units_multi_ln_lead234avg = mean(c(total_units_multi_ln_lead2, total_units_multi_ln_lead3, total_units_multi_ln_lead4), na.rm=T),
    total_units_single_ln_lead234avg = mean(c(total_units_single_ln_lead2, total_units_single_ln_lead3, total_units_single_ln_lead4), na.rm=T),
    
    
    total_units_ln_deltaterm234avg = (total_units_ln_lead234avg-total_units_ln_lagterm4avg),
    total_units_multi_ln_deltaterm234avg = (total_units_multi_ln_lead234avg-total_units_multi_ln_lagterm4avg),
    total_units_single_ln_deltaterm234avg = (total_units_single_ln_lead234avg-total_units_single_ln_lagterm4avg),
    
    ratio_bldgs_multisingle_delta23avg = mean(c(ratio_bldgs_multisingle_delta2, ratio_bldgs_multisingle_delta3), na.rm=T),
    ratio_units_multisingle_delta23avg = mean(c(ratio_units_multisingle_delta2, ratio_units_multisingle_delta3), na.rm=T),
    ratio_units_multisingle_lead234avg = mean(c(ratio_units_multisingle_lead2, ratio_units_multisingle_lead3, ratio_units_multisingle_lead4), na.rm=T),
    ratio_units_multisingle_delta234avg = mean(c(ratio_units_multisingle_delta2, ratio_units_multisingle_delta3, ratio_units_multisingle_delta4), na.rm=T),
    ratio_units_multisingle_delta1234avg = mean(c(ratio_units_multisingle_delta1, ratio_units_multisingle_delta2, ratio_units_multisingle_delta3, ratio_units_multisingle_delta4), na.rm=T),
    ratio_units_multisingle_deltaterm234avg = ratio_units_multisingle_lead234avg-ratio_units_multisingle_lagterm4avg,
    ratio_bldgs_multisingle_lead23avg = mean(c(ratio_bldgs_multisingle_lead2, ratio_bldgs_multisingle_lead3), na.rm=T),
    ratio_units_multisingle_lead23avg = mean(c(ratio_units_multisingle_lead2, ratio_units_multisingle_lead3), na.rm=T)
  )


data_panel <- mutate(data_panel, year = as.numeric(year))
dim(data_panel) # 38902
data_panel <- left_join(data_panel, cbps, by=c("place_fips" = "place_fips",
                                               "year" = "SurveyDate"))
dim(data_panel) # 38902


## Subset data -----------------------------------------------------------------

## create cluster variable:
data_panel <- data_panel %>%
  mutate(cluster = (place_fips))

# subset to big cities:
data_panel <- data_panel %>%
  filter(population_2020>=75000)

dim(data_panel) # 20681


## add per capita measures:
data_panel <- data_panel %>%
  mutate(total_units_pc = (total_units/(population_est/100000)),
         total_units_single_pc = (total_units_single/(population_est/100000)),
         total_units_multi_pc = (total_units_multi/(population_est/100000)),
         total_bldgs_pc = (total_bldgs/(population_est/100000)),
         total_bldgs_single_pc = (total_bldgs_single/(population_est/100000)),
         total_bldgs_multi_pc = (total_bldgs_multi/(population_est/100000)),
         
         total_units_pc_lead1 = (total_units_lead1/(population_est/100000)),
         total_units_single_pc_lead1 = (total_units_single_lead1/(population_est/100000)),
         total_units_multi_pc_lead1 = (total_units_multi_lead1/(population_est/100000)),
         total_bldgs_pc_lead1 = (total_bldgs_lead1/(population_est/100000)),
         total_bldgs_single_pc_lead1 = (total_bldgs_single_lead1/(population_est/100000)),
         total_bldgs_multi_pc_lead1 = (total_bldgs_multi_lead1/(population_est/100000)),
         
         total_units_pc_lead2 = (total_units_lead2/(population_est/100000)),
         total_units_single_pc_lead2 = (total_units_single_lead2/(population_est/100000)),
         total_units_multi_pc_lead2 = (total_units_multi_lead2/(population_est/100000)),
         total_bldgs_pc_lead2 = (total_bldgs_lead2/(population_est/100000)),
         total_bldgs_single_pc_lead2 = (total_bldgs_single_lead2/(population_est/100000)),
         total_bldgs_multi_pc_lead2 = (total_bldgs_multi_lead2/(population_est/100000)),
         
         total_units_pc_lead3 = (total_units_lead3/(population_est/100000)),
         total_units_single_pc_lead3 = (total_units_single_lead3/(population_est/100000)),
         total_units_multi_pc_lead3 = (total_units_multi_lead3/(population_est/100000)),
         total_bldgs_pc_lead3 = (total_bldgs_lead3/(population_est/100000)),
         total_bldgs_single_pc_lead3 = (total_bldgs_single_lead3/(population_est/100000)),
         total_bldgs_multi_pc_lead3 = (total_bldgs_multi_lead3/(population_est/100000)),

         total_units_pc_delta2 = (total_units_pc_lead2 - total_units_pc),
         total_units_single_pc_delta2 = (total_units_single_pc_lead2-total_units_single_pc),
         total_units_multi_pc_delta2 = (total_units_multi_pc_lead2 - total_units_multi_pc),
         total_bldgs_pc_delta2 = (total_bldgs_pc_lead2-total_bldgs_pc),
         total_bldgs_single_pc_delta2 = (total_bldgs_single_pc_lead2-total_bldgs_single_pc),
         total_bldgs_multi_pc_delta2 = (total_bldgs_multi_pc_lead2-total_bldgs_multi_pc),
         
         total_units_pc_delta3 = (total_units_pc_lead3 - total_units_pc),
         total_units_single_pc_delta3 = (total_units_single_pc_lead3-total_units_single_pc),
         total_units_multi_pc_delta3 = (total_units_multi_pc_lead3 - total_units_multi_pc),
         total_bldgs_pc_delta3 = (total_bldgs_pc_lead3-total_bldgs_pc),
         total_bldgs_single_pc_delta3 = (total_bldgs_single_pc_lead3-total_bldgs_single_pc),
         total_bldgs_multi_pc_delta3 = (total_bldgs_multi_pc_lead3-total_bldgs_multi_pc)
  )

data_panel$year <- as.integer(data_panel$year)

data_panel_post1990 <- data_panel %>%
  filter(year>=1990)

data_panel_post1990$year <- as.integer(data_panel_post1990$year)

mean(data_panel_post1990$total_units_multi, na.rm=T) # 412
mean(data_panel_post1990$total_units_multi_pc, na.rm=T) # 190
writeLines(as.character(round(mean(data_panel_post1990$total_units_multi_pc, na.rm=T), 0)),"mayors/mean_units_multi_pc.tex",sep = "%")
mean(data_panel_post1990$total_units_multi_pc_delta2, na.rm=T) # 13
mean(data_panel_post1990$total_units_multi_pc_delta3, na.rm=T) # 21


## Descriptive plots -----------------------------------------------------------

pop <- read_csv("places_2020_population.csv")
pop <- select(pop, GEOCODE, population_2020)


cbps_summ <- cbps %>%
  left_join(pop, by=c("place_fips"="GEOCODE")) %>%
  # filter(SurveyDate>=1990) %>%
  filter(population_2020>=75000) %>%
  rename(year = SurveyDate) %>%
  group_by(year) %>%
  summarize(total_bldgs = mean(total_bldgs,na.rm=T),
            total_units = mean(total_units,na.rm=T),
            total_bldgs_single = mean(total_bldgs_single,na.rm=T),
            total_units_single = mean(total_units_single,na.rm=T),
            total_bldgs_multi = mean(total_bldgs_multi,na.rm=T),
            total_units_multi = mean(total_units_multi,na.rm=T),
            ratio_bldgs_multisingle = mean(ratio_bldgs_multisingle,na.rm=T),
            ratio_units_multisingle = mean(ratio_units_multisingle,na.rm=T)
  ) %>%
  mutate(ratio_bldgs_multisingle_wtd = total_bldgs_multi/total_bldgs,
         ratio_units_multisingle_wtd = total_units_multi/total_units
  )

## Figure 3a:
(cbps_timeplot_ratio_bldgs3 <- ggplot(cbps_summ) + 
    geom_line(aes(x=year,y=(total_bldgs/3000)),col="blue",lty="dotted") +
    geom_line(aes(x=year,y=(ratio_bldgs_multisingle_wtd)),col="black",lty="solid") + 
    annotate("text",x=1996,y=0.1,label="Multi-family\n% of buildings",col="black") +
    annotate("text",x=2016,y=0.19,label="Total number of buildings",col="blue") +    
    scale_x_continuous("",limits=c(1980,2020),breaks=seq(1980,2020,4)) + 
    scale_y_continuous("Weighted average multifamily\npercent of buildings permitted",labels=scales::percent_format(),limits=c(0,.32),
                       sec.axis = sec_axis(trans = ~ .*3000,name="Average number of buildings permitted",breaks=c(c(0,0.1,0.2,0.3)*3000))) + 
    theme_minimal() + 
    theme(legend.position = "none") + 
    theme(axis.line = element_line(),axis.ticks = element_line(),panel.grid = element_blank())
)
ggsave("mayors/timeplot_ratio_bldgs_wtotal_fullcbps_longer.pdf",cbps_timeplot_ratio_bldgs3,height=3.5,width=9)



## Figure 3b:
(cbps_timeplot_ratio_units3 <- ggplot(cbps_summ) + 
    geom_line(aes(x=year,y=(total_units/3000)),col="blue",lty="dotted") +
    geom_line(aes(x=year,y=(ratio_units_multisingle_wtd)),col="black",lty="solid") + 
    annotate("text",x=1991,y=0.55,label="Multi-family\n% of units",col="black") +
    annotate("text",x=2016,y=0.18,label="Total number of units",col="blue") +
    scale_x_continuous("",limits=c(1980,2020),breaks=seq(1980,2020,4)) + 
    scale_y_continuous("Weighted average multifamily\npercent of units permitted",breaks=c(0,0.2,0.4,0.6),labels=c("0%","20%","40%","60%"),limits=c(0,0.68),
                       sec.axis = sec_axis(trans = ~ .*3000,name="Average number of units permitted", breaks=waiver())) + 
    theme_minimal() + 
    theme(legend.position = "none") + 
    theme(axis.line = element_line(),axis.ticks = element_line(),panel.grid = element_blank())
)
ggsave("mayors/timeplot_ratio_units_wtotal_fullcbps_longer.pdf",cbps_timeplot_ratio_units3,height=3.5,width=9)


## PanelMatch analyses ---------------------------------------------------------

#### Development ~ Party control ####
##### ATTs ------
PM.results <- PanelMatch(lag = 4, time.id = "year", unit.id = "place_fips", 
                         treatment = "dem_in_power", 
                         data = as.data.frame(filter(data_panel_post1990,year >= 1990 & (dem_in_power==1 | rep_in_power==1))),
                         match.missing = TRUE,   covs.formula = ~ I(lag(total_units_multi_ln, 1:2)),
                         refinement.method = "mahalanobis", 
                         size.match = 5, qoi = "att" ,outcome.var = "total_units_multi_ln",
                         lead = 0:3, forbid.treatment.reversal = FALSE)
# errors from some internal functions w/ refinement.method="mahalanobis" unless set to "none" 

PE.results <- PanelEstimate(sets = PM.results, data = data.frame(filter(data_panel_post1990,year >= 1990 & (dem_in_power==1 | rep_in_power==1))))
PE.results.90 <- PanelEstimate(sets = PM.results, data = data.frame(filter(data_panel_post1990,year >= 1990 & (dem_in_power==1 | rep_in_power==1))),confidence.level = 0.9)
# summary(PE.results)
# plot(PE.results)

coefs <- summary(PE.results)$summary %>% 
  as_tibble() %>%
  mutate(Year = c("t+1","t+2","t+3","t+4"),
         year_num = seq(1:4)) %>%
  rename(ci_lo = `2.5%`,
         ci_hi = `97.5%`)
coefs_90 <- summary(PE.results.90)$summary %>% 
  as_tibble() %>%
  mutate(Year = c("t+1","t+2","t+3","t+4"),
         year_num = seq(1:4)) %>%
  rename(ci_lo_90 = `5%`,
         ci_hi_90 = `95%`) %>%
  select(-estimate,-std.error)
coefs <- left_join(coefs, coefs_90)


## Figure A34d:
(pm_plot_units_multi_ln <- ggplot(coefs) + 
    geom_hline(yintercept = 0,lty=2) + 
    geom_point(aes(y=estimate,x=year_num),size=2) + 
    geom_errorbar(aes(x=year_num,ymin=ci_lo_90, ymax=ci_hi_90),width=0, size=1) +
    geom_errorbar(aes(x=year_num,ymin=ci_lo, ymax=ci_hi),width=0, size=0.5) +
    geom_text(aes(y=estimate+0.05,x=year_num+0.2,label=round(estimate,2))) + 
    theme_minimal() + 
    scale_x_continuous("Year after election",breaks = coefs$year_num,labels=coefs$Year) + 
    scale_y_continuous("Avg. Treatment Effect on Treated\nof Democratic control on\nlog(multifamily units permitted + 1)")
)
ggsave(pm_plot_units_multi_ln,filename = "mayors/did_coefs_att_multi_units_ln.pdf",height=3,width=5)

# total units
PM.results <- PanelMatch(lag = 4, time.id = "year", unit.id = "place_fips", 
                         treatment = "dem_in_power", 
                         data = as.data.frame(filter(data_panel_post1990,year >= 1990 & (dem_in_power==1 | rep_in_power==1))),
                         match.missing = TRUE, 
                         # refinement.method = "none", 
                         covs.formula = ~ I(lag(total_units_ln, 1:2)),
                         refinement.method = "mahalanobis", 
                         size.match = 5, qoi = "att" ,outcome.var = "total_units_ln",
                         lead = 0:3, forbid.treatment.reversal = FALSE)
# errors from some internal functions w/ refinement.method="mahalanobis" unless set to "none" 

PE.results <- PanelEstimate(sets = PM.results, data = data.frame(filter(data_panel_post1990,year >= 1990 & (dem_in_power==1 | rep_in_power==1))))
PE.results.90 <- PanelEstimate(sets = PM.results, data = data.frame(filter(data_panel_post1990,year >= 1990 & (dem_in_power==1 | rep_in_power==1))),confidence.level = 0.9)
# summary(PE.results)
# plot(PE.results)

coefs <- summary(PE.results)$summary %>% 
  as_tibble() %>%
  mutate(Year = c("t+1","t+2","t+3","t+4"),
         year_num = seq(1:4)) %>%
  rename(ci_lo = `2.5%`,
         ci_hi = `97.5%`)
coefs_90 <- summary(PE.results.90)$summary %>% 
  as_tibble() %>%
  mutate(Year = c("t+1","t+2","t+3","t+4"),
         year_num = seq(1:4)) %>%
  rename(ci_lo_90 = `5%`,
         ci_hi_90 = `95%`) %>%
  select(-estimate,-std.error)
coefs <- left_join(coefs, coefs_90)

## Figure A34b:
(pm_plot_total_units_ln <- ggplot(coefs) + 
    geom_hline(yintercept = 0,lty=2) + 
    geom_point(aes(y=estimate,x=year_num),size=2) + 
    geom_errorbar(aes(x=year_num,ymin=ci_lo_90, ymax=ci_hi_90),width=0, size=1) +
    geom_errorbar(aes(x=year_num,ymin=ci_lo, ymax=ci_hi),width=0, size=0.5) +
    geom_text(aes(y=estimate+0.05,x=year_num+0.2,label=round(estimate,2))) + 
    theme_minimal() + 
    scale_x_continuous("Year after election",breaks = coefs$year_num,labels=coefs$Year) + 
    scale_y_continuous("Avg. Treatment Effect on Treated\nof Democratic control on\nlog(total units permitted + 1)")
)
ggsave(pm_plot_total_units_ln,filename = "mayors/did_coefs_att_total_units_ln.pdf",height=3,width=5)


## ratio units:
PM.results <- PanelMatch(lag = 4, time.id = "year", unit.id = "place_fips", 
                         treatment = "dem_in_power", 
                         data = as.data.frame(filter(data_panel_post1990,year >= 1990 & (dem_in_power==1 | rep_in_power==1))),
                         match.missing = TRUE, 
                         # refinement.method = "none", 
                         covs.formula = ~ I(lag(ratio_units_multisingle, 1:2)),
                         refinement.method = "mahalanobis", 
                         size.match = 5, qoi = "att" ,outcome.var = "ratio_units_multisingle",
                         lead = 0:3, forbid.treatment.reversal = FALSE)
# errors from some internal functions w/ refinement.method="mahalanobis" unless set to "none" 

PE.results <- PanelEstimate(sets = PM.results, data = data.frame(filter(data_panel_post1990,year >= 1990 & (dem_in_power==1 | rep_in_power==1))))
PE.results.90 <- PanelEstimate(sets = PM.results, data = data.frame(filter(data_panel_post1990,year >= 1990 & (dem_in_power==1 | rep_in_power==1))),confidence.level = 0.9)
# summary(PE.results)
# plot(PE.results)

coefs <- summary(PE.results)$summary %>% 
  as_tibble() %>%
  mutate(Year = c("t+1","t+2","t+3","t+4"),
         year_num = seq(1:4)) %>%
  rename(ci_lo = `2.5%`,
         ci_hi = `97.5%`)
coefs_90 <- summary(PE.results.90)$summary %>% 
  as_tibble() %>%
  mutate(Year = c("t+1","t+2","t+3","t+4"),
         year_num = seq(1:4)) %>%
  rename(ci_lo_90 = `5%`,
         ci_hi_90 = `95%`) %>%
  select(-estimate,-std.error)
coefs <- left_join(coefs, coefs_90)

## Figure A34f:
(pm_plot_ratio_units_multi <- ggplot(coefs) + 
    geom_hline(yintercept = 0,lty=2) + 
    geom_point(aes(y=estimate,x=year_num),size=2) + 
    geom_errorbar(aes(x=year_num,ymin=ci_lo_90, ymax=ci_hi_90),width=0, size=1) +
    geom_errorbar(aes(x=year_num,ymin=ci_lo, ymax=ci_hi),width=0, size=0.5) +
    geom_text(aes(y=estimate+0.01,x=year_num+0.2,label=round(estimate,2))) + 
    theme_minimal() + 
    scale_x_continuous("Year after election",breaks = coefs$year_num,labels=coefs$Year) + 
    scale_y_continuous("Avg. Treatment Effect on Treated of\nDemocratic control on multifamily\nprop. of units permitted")
)
ggsave(pm_plot_ratio_units_multi,filename = "mayors/did_coefs_att_ratio_multi_units.pdf",height=3,width=5)


# multifamily buildings:
PM.results <- PanelMatch(lag = 4, time.id = "year", unit.id = "place_fips", 
                         treatment = "dem_in_power", 
                         data = as.data.frame(filter(data_panel_post1990,year >= 1990 & (dem_in_power==1 | rep_in_power==1))),
                         match.missing = TRUE,   covs.formula = ~ I(lag(total_bldgs_multi_ln, 1:2)),
                         refinement.method = "mahalanobis", 
                         size.match = 5, qoi = "att" ,outcome.var = "total_bldgs_multi_ln",
                         lead = 0:3, forbid.treatment.reversal = FALSE)
# errors from some internal functions w/ refinement.method="mahalanobis" unless set to "none" 

PE.results <- PanelEstimate(sets = PM.results, data = data.frame(filter(data_panel_post1990,year >= 1990 & (dem_in_power==1 | rep_in_power==1))))
PE.results.90 <- PanelEstimate(sets = PM.results, data = data.frame(filter(data_panel_post1990,year >= 1990 & (dem_in_power==1 | rep_in_power==1))),confidence.level = 0.9)
# summary(PE.results)
# plot(PE.results)

coefs <- summary(PE.results)$summary %>% 
  as_tibble() %>%
  mutate(Year = c("t+1","t+2","t+3","t+4"),
         year_num = seq(1:4)) %>%
  rename(ci_lo = `2.5%`,
         ci_hi = `97.5%`)
coefs_90 <- summary(PE.results.90)$summary %>% 
  as_tibble() %>%
  mutate(Year = c("t+1","t+2","t+3","t+4"),
         year_num = seq(1:4)) %>%
  rename(ci_lo_90 = `5%`,
         ci_hi_90 = `95%`) %>%
  select(-estimate,-std.error)
coefs <- left_join(coefs, coefs_90)

## Figure A34c:
(pm_plot_bldgs_multi_ln <- ggplot(coefs) + 
    geom_hline(yintercept = 0,lty=2) + 
    geom_point(aes(y=estimate,x=year_num),size=2) + 
    geom_errorbar(aes(x=year_num,ymin=ci_lo_90, ymax=ci_hi_90),width=0, size=1) +
    geom_errorbar(aes(x=year_num,ymin=ci_lo, ymax=ci_hi),width=0, size=0.5) +
    geom_text(aes(y=estimate+0.05,x=year_num+0.2,label=round(estimate,2))) + 
    theme_minimal() + 
    scale_x_continuous("Year after election",breaks = coefs$year_num,labels=coefs$Year) + 
    scale_y_continuous("Avg. Treatment Effect on Treated\nof Democratic control on\nlog(multifamily buildings permitted + 1)")
)
ggsave(pm_plot_bldgs_multi_ln,filename = "mayors/did_coefs_att_multi_bldgs_ln.pdf",height=3,width=5)


## total buildings
PM.results <- PanelMatch(lag = 4, time.id = "year", unit.id = "place_fips", 
                         treatment = "dem_in_power", 
                         data = as.data.frame(filter(data_panel_post1990,year >= 1990 & (dem_in_power==1 | rep_in_power==1))),
                         match.missing = TRUE,   covs.formula = ~ I(lag(total_bldgs_ln, 1:2)),
                         refinement.method = "mahalanobis", 
                         size.match = 5, qoi = "att" ,outcome.var = "total_bldgs_ln",
                         lead = 0:3, forbid.treatment.reversal = FALSE)
# errors from some internal functions w/ refinement.method="mahalanobis" unless set to "none" 

PE.results <- PanelEstimate(sets = PM.results, data = data.frame(filter(data_panel_post1990,year >= 1990 & (dem_in_power==1 | rep_in_power==1))))
PE.results.90 <- PanelEstimate(sets = PM.results, data = data.frame(filter(data_panel_post1990,year >= 1990 & (dem_in_power==1 | rep_in_power==1))),confidence.level = 0.9)
# summary(PE.results)
# plot(PE.results)

coefs <- summary(PE.results)$summary %>% 
  as_tibble() %>%
  mutate(Year = c("t+1","t+2","t+3","t+4"),
         year_num = seq(1:4)) %>%
  rename(ci_lo = `2.5%`,
         ci_hi = `97.5%`)
coefs_90 <- summary(PE.results.90)$summary %>% 
  as_tibble() %>%
  mutate(Year = c("t+1","t+2","t+3","t+4"),
         year_num = seq(1:4)) %>%
  rename(ci_lo_90 = `5%`,
         ci_hi_90 = `95%`) %>%
  select(-estimate,-std.error)
coefs <- left_join(coefs, coefs_90)

## Figure A34a:
(pm_plot_bldgs_ln <- ggplot(coefs) + 
    geom_hline(yintercept = 0,lty=2) + 
    geom_point(aes(y=estimate,x=year_num),size=2) + 
    geom_errorbar(aes(x=year_num,ymin=ci_lo_90, ymax=ci_hi_90),width=0, size=1) +
    geom_errorbar(aes(x=year_num,ymin=ci_lo, ymax=ci_hi),width=0, size=0.5) +
    geom_text(aes(y=estimate+0.05,x=year_num+0.2,label=round(estimate,2))) + 
    theme_minimal() + 
    scale_x_continuous("Year after election",breaks = coefs$year_num,labels=coefs$Year) + 
    scale_y_continuous("Avg. Treatment Effect on Treated\nof Democratic control on\nlog(total buildings permitted + 1)")
)
ggsave(pm_plot_bldgs_ln,filename = "mayors/did_coefs_att_total_bldgs_ln.pdf",height=3,width=5)


## ratio units:
PM.results <- PanelMatch(lag = 4, time.id = "year", unit.id = "place_fips", 
                         treatment = "dem_in_power", 
                         data = as.data.frame(filter(data_panel_post1990,year >= 1990 & (dem_in_power==1 | rep_in_power==1))),
                         match.missing = TRUE, 
                         # refinement.method = "none", 
                         covs.formula = ~ I(lag(ratio_bldgs_multisingle, 1:2)),
                         refinement.method = "mahalanobis", 
                         size.match = 5, qoi = "att" ,outcome.var = "ratio_bldgs_multisingle",
                         lead = 0:3, forbid.treatment.reversal = FALSE)
# errors from some internal functions w/ refinement.method="mahalanobis" unless set to "none" 

PE.results <- PanelEstimate(sets = PM.results, data = data.frame(filter(data_panel_post1990,year >= 1990 & (dem_in_power==1 | rep_in_power==1))))
PE.results.90 <- PanelEstimate(sets = PM.results, data = data.frame(filter(data_panel_post1990,year >= 1990 & (dem_in_power==1 | rep_in_power==1))),confidence.level = 0.9)
# summary(PE.results)
# plot(PE.results)

coefs <- summary(PE.results)$summary %>% 
  as_tibble() %>%
  mutate(Year = c("t+1","t+2","t+3","t+4"),
         year_num = seq(1:4)) %>%
  rename(ci_lo = `2.5%`,
         ci_hi = `97.5%`)
coefs_90 <- summary(PE.results.90)$summary %>% 
  as_tibble() %>%
  mutate(Year = c("t+1","t+2","t+3","t+4"),
         year_num = seq(1:4)) %>%
  rename(ci_lo_90 = `5%`,
         ci_hi_90 = `95%`) %>%
  select(-estimate,-std.error)
coefs <- left_join(coefs, coefs_90)

## Figure A34e:
(pm_plot_ratio_bldgs_multi <- ggplot(coefs) + 
    geom_hline(yintercept = 0,lty=2) + 
    geom_point(aes(y=estimate,x=year_num),size=2) + 
    geom_errorbar(aes(x=year_num,ymin=ci_lo_90, ymax=ci_hi_90),width=0, size=1) +
    geom_errorbar(aes(x=year_num,ymin=ci_lo, ymax=ci_hi),width=0, size=0.5) +
    geom_text(aes(y=estimate+0.01,x=year_num+0.2,label=round(estimate,2))) + 
    theme_minimal() + 
    scale_x_continuous("Year after election",breaks = coefs$year_num,labels=coefs$Year) + 
    scale_y_continuous("Avg. Treatment Effect on Treated of\nDemocratic control on multifamily\nprop. of buildings permitted")
)
ggsave(pm_plot_ratio_bldgs_multi,filename = "mayors/did_coefs_att_ratio_multi_bldgs.pdf",height=3,width=5)




