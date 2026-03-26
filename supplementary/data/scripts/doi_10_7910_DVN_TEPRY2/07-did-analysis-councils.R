## Preamble ------
library(tidyverse)
library(foreign)
library(stargazer)
library(janitor)
library(broom)
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
council_comp <- read_rds("council_comp_wcbps.rds") %>%
  select(place_fips,city,abb,year,seats_total,total_dem,total_rep,total_oth,count_pid,population_est,population_2020)


## Create panel:  --------------------------------------------------------------
council_comp <- council_comp %>%
  mutate(dem_seatshare = total_dem/seats_total,
         rep_seatshare = total_rep/seats_total,
         unk_seatshare = total_oth/seats_total) %>%
  mutate(dem_control = case_when(dem_seatshare>=0.5 ~ 1,
                                 rep_seatshare>=0.5 ~ 0))


data_panel <- council_comp %>% 
  select(place_fips,city,abb,year,population_est,population_2020,dem_seatshare,dem_control) %>%
  group_by(place_fips) %>%
  complete(year = seq.int(1990, 2022,by = 1)) %>% # expand to have all years so can get leads/lags even if values==NA in that year
  arrange(year) %>%
  fill(place_fips,city,abb,population_2020,.direction = "downup") %>%
  group_by(place_fips) %>%
  fill(dem_control,dem_seatshare,population_est,.direction = "down")


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

dim(data_panel) # 10238
data_panel <- left_join(data_panel, cbps, by=c("place_fips" = "place_fips",
                                               "year" = "SurveyDate"))
dim(data_panel) # 10238


## Subset data -----------------------------------------------------------------

## create cluster variable:
data_panel <- data_panel %>%
  mutate(cluster = (place_fips))

# subset to big cities: - already done
# data_panel <- data_panel %>%
#   filter(population_2020>=75000)

dim(data_panel) # 11748


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

data_panel$year <- as.integer(data_panel$year) # for panelmatch to work, needs consecutive integers

mean(data_panel$total_units_multi, na.rm=T) # 495
mean(data_panel$total_units_multi_pc, na.rm=T) # 202
mean(data_panel$total_units_multi_pc_delta2, na.rm=T) # 14
mean(data_panel$total_units_multi_pc_delta3, na.rm=T) # 24


## PanelMatch analyses ---------------------------------------------------------

#### Development ~ Party control ####
##### ATTs ------
## total buildings
PM.results <- PanelMatch(lag = 4, time.id = "year", unit.id = "place_fips", 
                         treatment = "dem_control", 
                         data = as.data.frame(filter(data_panel,year >= 1990 & (dem_control==1 | dem_control==0))),
                         match.missing = TRUE,   covs.formula = ~ I(lag(total_bldgs_ln, 1:2)),
                         refinement.method = "mahalanobis", 
                         size.match = 5, qoi = "att" ,outcome.var = "total_bldgs_ln",
                         lead = 0:3, forbid.treatment.reversal = FALSE)
# errors from some internal functions w/ refinement.method="mahalanobis" unless set to "none" 

PE.results <- PanelEstimate(sets = PM.results, data = data.frame(filter(data_panel,year >= 1990 & (dem_control==1 | dem_control==0))))
PE.results.90 <- PanelEstimate(sets = PM.results, data = data.frame(filter(data_panel,year >= 1990 & (dem_control==1 | dem_control==0))),confidence.level = 0.9)
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

## Figure A35a ##
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
ggsave(pm_plot_bldgs_ln,filename = "councils/did_coefs_att_total_bldgs_ln.pdf",height=3,width=5)



# total units
PM.results <- PanelMatch(lag = 4, time.id = "year", unit.id = "place_fips", 
                         treatment = "dem_control", 
                         data = as.data.frame(filter(data_panel,year >= 1990 & (dem_control==1 | dem_control==0))),
                         match.missing = TRUE, 
                         # refinement.method = "none", 
                         covs.formula = ~ I(lag(total_units_ln, 1:2)),
                         refinement.method = "mahalanobis", 
                         size.match = 5, qoi = "att" ,outcome.var = "total_units_ln",
                         lead = 0:3, forbid.treatment.reversal = FALSE)
# errors from some internal functions w/ refinement.method="mahalanobis" unless set to "none" 

PE.results <- PanelEstimate(sets = PM.results, data = data.frame(filter(data_panel,year >= 1990 & (dem_control==1 | dem_control==0))))
PE.results.90 <- PanelEstimate(sets = PM.results, data = data.frame(filter(data_panel,year >= 1990 & (dem_control==1 | dem_control==0))),confidence.level = 0.9)
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

## Figure A35b ##
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
ggsave(pm_plot_total_units_ln,filename = "councils/did_coefs_att_total_units_ln.pdf",height=3,width=5)


# multifamily buildings:
PM.results <- PanelMatch(lag = 4, time.id = "year", unit.id = "place_fips", 
                         treatment = "dem_control", 
                         data = as.data.frame(filter(data_panel,year >= 1990 & (dem_control==1 | dem_control==0))),
                         match.missing = TRUE,   covs.formula = ~ I(lag(total_bldgs_multi_ln, 1:2)),
                         refinement.method = "mahalanobis", 
                         size.match = 5, qoi = "att" ,outcome.var = "total_bldgs_multi_ln",
                         lead = 0:3, forbid.treatment.reversal = FALSE)
# errors from some internal functions w/ refinement.method="mahalanobis" unless set to "none" 

PE.results <- PanelEstimate(sets = PM.results, data = data.frame(filter(data_panel,year >= 1990 & (dem_control==1 | dem_control==0))))
PE.results.90 <- PanelEstimate(sets = PM.results, data = data.frame(filter(data_panel,year >= 1990 & (dem_control==1 | dem_control==0))),confidence.level = 0.9)
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

## Figure A35c ##
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
ggsave(pm_plot_bldgs_multi_ln,filename = "councils/did_coefs_att_multi_bldgs_ln.pdf",height=3,width=5)


PM.results <- PanelMatch(lag = 4, time.id = "year", unit.id = "place_fips", 
                         treatment = "dem_control", 
                         data = as.data.frame(filter(data_panel,year >= 1990 & (dem_control==1 | dem_control==0))),
                         match.missing = TRUE,   covs.formula = ~ I(lag(total_units_multi_ln, 1:2)),
                         refinement.method = "mahalanobis", 
                         size.match = 5, qoi = "att" ,outcome.var = "total_units_multi_ln",
                         lead = 0:3, forbid.treatment.reversal = FALSE)

PE.results <- PanelEstimate(sets = PM.results, data = data.frame(filter(data_panel,year >= 1990 & (dem_control==1 | dem_control==0))))
PE.results.90 <- PanelEstimate(sets = PM.results, data = data.frame(filter(data_panel,year >= 1990 & (dem_control==1 | dem_control==0))),confidence.level = 0.9)
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

## Figure A35d ##
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
ggsave(pm_plot_units_multi_ln,filename = "councils/did_coefs_att_multi_units_ln.pdf",height=3,width=5)



## ratio bldgs:
PM.results <- PanelMatch(lag = 4, time.id = "year", unit.id = "place_fips", 
                         treatment = "dem_control", 
                         data = as.data.frame(filter(data_panel,year >= 1990 & (dem_control==1 | dem_control==0))),
                         match.missing = TRUE, 
                         # refinement.method = "none", 
                         covs.formula = ~ I(lag(ratio_bldgs_multisingle, 1:2)),
                         refinement.method = "mahalanobis", 
                         size.match = 5, qoi = "att" ,outcome.var = "ratio_bldgs_multisingle",
                         lead = 0:3, forbid.treatment.reversal = FALSE)
# errors from some internal functions w/ refinement.method="mahalanobis" unless set to "none" 

PE.results <- PanelEstimate(sets = PM.results, data = data.frame(filter(data_panel,year >= 1990 & (dem_control==1 | dem_control==0))))
PE.results.90 <- PanelEstimate(sets = PM.results, data = data.frame(filter(data_panel,year >= 1990 & (dem_control==1 | dem_control==0))),confidence.level = 0.9)
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

## Figure A35e ##
(pm_plot_ratio_bldgs_multi <- ggplot(coefs) + 
    geom_hline(yintercept = 0,lty=2) + 
    geom_point(aes(y=estimate,x=year_num),size=2) + 
    geom_errorbar(aes(x=year_num,ymin=ci_lo_90, ymax=ci_hi_90),width=0, size=1) +
    geom_errorbar(aes(x=year_num,ymin=ci_lo, ymax=ci_hi),width=0, size=0.5) +
    geom_text(aes(y=estimate+0.01,x=year_num+0.2,label=round(estimate,2))) + 
    theme_minimal() + 
    scale_x_continuous("Year after election",breaks = coefs$year_num,labels=coefs$Year) + 
    scale_y_continuous("Avg. Treatment Effect on Treated of\nDemocratic control on multifamily\nprop. of total buildings permitted")
)
ggsave(pm_plot_ratio_bldgs_multi,filename = "councils/did_coefs_att_ratio_multi_bldgs.pdf",height=3,width=5)



## ratio units:
PM.results <- PanelMatch(lag = 4, time.id = "year", unit.id = "place_fips", 
                         treatment = "dem_control", 
                         data = as.data.frame(filter(data_panel,year >= 1990 & (dem_control==1 | dem_control==0))),
                         match.missing = TRUE, 
                         # refinement.method = "none", 
                         covs.formula = ~ I(lag(ratio_units_multisingle, 1:2)),
                         refinement.method = "mahalanobis", 
                         size.match = 5, qoi = "att" ,outcome.var = "ratio_units_multisingle",
                         lead = 0:3, forbid.treatment.reversal = FALSE)
# errors from some internal functions w/ refinement.method="mahalanobis" unless set to "none" 

PE.results <- PanelEstimate(sets = PM.results, data = data.frame(filter(data_panel,year >= 1990 & (dem_control==1 | dem_control==0))))
PE.results.90 <- PanelEstimate(sets = PM.results, data = data.frame(filter(data_panel,year >= 1990 & (dem_control==1 | dem_control==0))),confidence.level = 0.9)
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

## Figure A35f ##
(pm_plot_ratio_units_multi <- ggplot(coefs) + 
    geom_hline(yintercept = 0,lty=2) + 
    geom_point(aes(y=estimate,x=year_num),size=2) + 
    geom_errorbar(aes(x=year_num,ymin=ci_lo_90, ymax=ci_hi_90),width=0, size=1) +
    geom_errorbar(aes(x=year_num,ymin=ci_lo, ymax=ci_hi),width=0, size=0.5) +
    geom_text(aes(y=estimate+0.01,x=year_num+0.2,label=round(estimate,2))) + 
    theme_minimal() + 
    scale_x_continuous("Year after election",breaks = coefs$year_num,labels=coefs$Year) + 
    scale_y_continuous("Avg. Treatment Effect on Treated of\nDemocratic control on multifamily\nprop. of total units permitted")
)
ggsave(pm_plot_ratio_units_multi,filename = "councils/did_coefs_att_ratio_multi_units.pdf",height=3,width=5)



