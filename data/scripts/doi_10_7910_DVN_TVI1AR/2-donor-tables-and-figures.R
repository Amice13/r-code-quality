# Run time: 01:02:00
# CPU memory usage: 24.8GB

library(here)
dir <- here()
setwd(dir)
source("0-init.R")





##### Load data #####

dat <- readRDS("donor-regression-data.rds")
ltv_summ <- readRDS("ltv-summ.rds")
census_race <- readRDS("census-racial-composition.rds")





##### Table 2 #####

mod_tea <- 
  dat %>% 
  filter(prior_donors) %>%
  feols(teaparty_cand ~ foreclosure_rate_pop_tract | parcel_hh_id + district_cycle, 
        data = ., cluster="GEOID10")
mod_nottea <-
  dat %>% 
  filter(prior_donors) %>%
  feols(notteaparty_cand ~ foreclosure_rate_pop_tract | parcel_hh_id + district_cycle, 
        data = ., cluster="GEOID10")

setFixest_dict(c(teaparty_cand = "Tea Party Candidates",
                 notteaparty_cand = "Other Republican Candidates",
                 foreclosure_rate_pop_tract = "Census Tract Foreclosure Rate",
                 parcel_hh_id = "Donor",
                 district_cycle = "District-Cycle",
                 GEOID10 = "Census Tract"
)
)

setFixest_etable(fitstat = ~ n)

tablestyle  = style.tex(main="aer",
                        fixef.suffix = " FEs",
                        fixef.where ="var",
                        fixef.title = "\\midrule",
                        stats.title = "\\midrule",
                        tablefoot=F,
                        yesNo="\\checkmark")

etable(mod_tea, mod_nottea, 
       file = paste0("table-2.tex"),
       title = "Comparing Rates of Itemized Contributions Across Republican Candidate Type",
       cluster=~GEOID10,
       drop = "(Intercept)",
       label = "table:main-results",
       digits=3,
       digits.stats=2,
       style.tex=tablestyle,
       replace = TRUE,
       depvar = TRUE
)

rm(list=ls(pattern="mod_"))
gc()





##### Tables 3-5 #####

mod_tea_gen <- 
  dat %>% 
  filter(prior_donors) %>%
  feols(teaparty_gen ~ foreclosure_rate_pop_tract | parcel_hh_id + district_cycle, 
        data = ., cluster="GEOID10")
mod_tea_pri <-
  dat %>% 
  filter(prior_donors) %>%
  feols(teaparty_pri ~ foreclosure_rate_pop_tract | parcel_hh_id + district_cycle, 
        data = ., cluster="GEOID10")

mod_tea_incum <- 
  dat %>% 
  filter(prior_donors) %>%
  feols(teaparty_incum ~ foreclosure_rate_pop_tract | parcel_hh_id + district_cycle, 
        data = ., cluster="GEOID10")
mod_tea_nonincum <-
  dat %>% 
  filter(prior_donors) %>%
  feols(teaparty_nonincum ~ foreclosure_rate_pop_tract | parcel_hh_id + district_cycle, 
        data = ., cluster="GEOID10")

mod_tea_in_dist <- 
  dat %>% 
  filter(prior_donors) %>%
  feols(teaparty_in_dist ~ foreclosure_rate_pop_tract | parcel_hh_id + district_cycle, 
        data = ., cluster="GEOID10")
mod_tea_out_dist <-
  dat %>% 
  filter(prior_donors) %>%
  feols(teaparty_out_dist ~ foreclosure_rate_pop_tract | parcel_hh_id + district_cycle, 
        data = ., cluster="GEOID10")

setFixest_dict(c(teaparty_gen = "General Election",
                 teaparty_pri = "Primary Election",
                 teaparty_incum = "Incumbents",
                 teaparty_nonincum = "Non-Incumbents",
                 teaparty_in_dist = "In-District",
                 teaparty_out_dist = "Out-of-District",
                 foreclosure_rate_pop_tract = "Census Tract Foreclosure Rate",
                 parcel_hh_id = "Donor",
                 district_cycle = "District-Cycle",
                 GEOID10 = "Census Tract"
)
)

setFixest_etable(fitstat = ~ n)

tablestyle  = style.tex(main="aer",
                        fixef.suffix = " FEs",
                        fixef.where ="var",
                        fixef.title = "\\midrule",
                        stats.title = "\\midrule",
                        tablefoot=F,
                        yesNo="\\checkmark")

etable(mod_tea_gen, mod_tea_pri,
       file = paste0("table-3.tex"),
       title = "Heterogeneous Effects by Tea Party Candidates' Election Types",
       cluster=~GEOID10,
       drop = "(Intercept)",
       label = "table:institution-genvpri",
       digits=3,
       digits.stats=2,
       style.tex=tablestyle,
       replace = TRUE,
       depvar = TRUE
)
etable(mod_tea_incum, mod_tea_nonincum,
       file = paste0("table-4.tex"),
       title = "Heterogeneous Effects by Tea Party Candidates' Incumbency Statuses",
       cluster=~GEOID10,
       drop = "(Intercept)",
       label = "table:institution-incumvchall",
       digits=3,
       digits.stats=2,
       style.tex=tablestyle,
       replace = TRUE,
       depvar = TRUE
)
etable(mod_tea_in_dist, mod_tea_out_dist,
       file = paste0("table-5.tex"),
       title = "Heterogeneous Effects by Tea Party Candidates' District Statuses",
       cluster=~GEOID10,
       drop = "(Intercept)",
       label = "table:institution-indistvoutdist",
       digits=3,
       digits.stats=2,
       style.tex=tablestyle,
       replace = TRUE,
       depvar = TRUE
)

rm(list=ls(pattern="mod_"))
gc()





##### Table 6 #####

########## NOTICE: exceptions for public access ########## 
# The following individual-level homeownership covariates, which are used in Table 6, are suppressed to comply with CoreLogic's Memorandum of Understanding (MOU) -----
# 1) "loan" (whether a campaign donor had taken out any HELOCs prior to the financial crisis); and
# 2) "ltv_high" (whether a campaign donor had a high estimated leverage ratio prior to the financial crisis).
# Users who wish to test alternative specifications to those presented in Table 6 may supply their code to me, and I would be happy to run their code on my copy of the CoreLogic data set for as long as I have access to it.

# mod_tea_loan <- 
#   dat %>% 
#   filter(prior_donors) %>%
#   mutate(
#     foreclosure_loan = foreclosure_rate_pop_tract*loan,
#     teaparty_cand_loan = teaparty_cand
#   ) %>%
#   feols(teaparty_cand_loan ~ foreclosure_rate_pop_tract + foreclosure_loan | parcel_hh_id + district_cycle_loan, 
#         data = ., cluster="GEOID10")
# 
# mod_tea_lev <-
#   dat %>% 
#   filter(prior_donors) %>%
#   mutate(
#     foreclosure_high = foreclosure_rate_pop_tract*ltv_high,
#     teaparty_cand_lev = teaparty_cand
#   ) %>% 
#   feols(teaparty_cand_lev ~ foreclosure_rate_pop_tract + foreclosure_high | parcel_hh_id + district_cycle_lev, 
#         data = ., cluster="GEOID10")
# 
# setFixest_dict(c(teaparty_cand_loan = "Prior HELOC",
#                  teaparty_cand_lev = "High LTV",
#                  foreclosure_rate_pop_tract = "Census Tract Foreclosure Rate",
#                  foreclosure_loan = "Census Tract Foreclosure Rate $\\times$ High Risk",
#                  foreclosure_high = "Census Tract Foreclosure Rate $\\times$ High Risk",
#                  parcel_hh_id = "Donor",
#                  district_cycle_lev = "District-Cycle-Risk",
#                  district_cycle_loan = "District-Cycle-Risk",
#                  GEOID10 = "Census Tract"
# )
# )
# 
# setFixest_etable(fitstat = ~ n)
# 
# tablestyle  = style.tex(main="aer",
#                         fixef.suffix = " FEs",
#                         fixef.where ="var",
#                         fixef.title = "\\midrule",
#                         stats.title = "\\midrule",
#                         tablefoot=F,
#                         yesNo="\\checkmark")
# 
# etable(mod_tea_lev, mod_tea_loan, 
#        file = paste0("table-6.tex"),
#        title = "Heterogeneous Effects by Donors' Risks of Mortgage Default",
#        cluster=~GEOID10,
#        drop = "(Intercept)",
#        label = "reg:risk",
#        digits=3,
#        digits.stats=2,
#        style.tex=tablestyle,
#        replace = TRUE,
#        depvar = TRUE
# )





##### Table 7 #####

mod_tea_white <- 
  dat %>% 
  filter(prior_donors) %>%
  feols(teaparty_cand ~ foreclosure_sharewhite_l + foreclosure_sharewhite_m + foreclosure_sharewhite_h | parcel_hh_id + district_cycle_sharewhite, 
        data = ., cluster="GEOID10")

setFixest_dict(c(teaparty_cand = "Tea Party Candidates",
                 foreclosure_sharewhite_l = "Census Tract Foreclosure Rate $\\times I$(low \\% white)",
                 foreclosure_sharewhite_m = "Census Tract Foreclosure Rate $\\times I$(medium \\% white)",
                 foreclosure_sharewhite_h = "Census Tract Foreclosure Rate $\\times I$(high \\% white)",
                 parcel_hh_id = "Donor",
                 district_cycle_sharewhite = "District-Cycle-Tercile",
                 GEOID10 = "Census Tract"
)
)

setFixest_etable(fitstat = ~ n)

tablestyle  = style.tex(main="aer",
                        fixef.suffix = " FEs",
                        fixef.where ="var",
                        fixef.title = "\\midrule",
                        stats.title = "\\midrule",
                        tablefoot=F,
                        yesNo="\\checkmark")

etable(mod_tea_white, 
       file = paste0("table-7.tex"),
       title = "Heterogeneous Effects by Census Tract Racial Composition",
       cluster=~GEOID10,
       drop = "(Intercept)",
       label = "table:het-white",
       digits=3,
       digits.stats=2,
       style.tex=tablestyle,
       replace = TRUE,
       depvar = FALSE,
       signif.code=c("***"=0.001,"**"=0.01,"*"=0.05)
)

rm(list=ls(pattern="mod_"))
gc()





##### Table A.2 #####

varnames <- c("", "N", "mean", "s.d.", "min.", "max")
varnames %<>% t()

temp <- 
  c("Count of Itemized Donations to Tea Party Candidates", 
    format(length(dat$count_tea_party[dat$prior_donors]), big.mark=","),
    signif(mean(dat$count_tea_party[dat$prior_donors], na.rm=TRUE), digits=3),
    signif(sd(dat$count_tea_party[dat$prior_donors], na.rm=TRUE), digits=3),
    round(min(dat$count_tea_party[dat$prior_donors], na.rm=TRUE), digits=3),
    round(max(dat$count_tea_party[dat$prior_donors], na.rm=TRUE), digits=3)
  )
temp %<>% t() 
summstats <- rbind(varnames, temp)

temp <- 
  c("Count of Itemized Donations to Non-Tea Party Republican Candidates", 
    format(length(dat$count_not_tea_party[dat$prior_donors]), big.mark=","),
    signif(mean(dat$count_not_tea_party[dat$prior_donors], na.rm=TRUE), digits=3),
    signif(sd(dat$count_not_tea_party[dat$prior_donors], na.rm=TRUE), digits=3),
    round(min(dat$count_not_tea_party[dat$prior_donors], na.rm=TRUE), digits=3),
    round(max(dat$count_not_tea_party[dat$prior_donors], na.rm=TRUE), digits=3)
  )
temp %<>% t() 
summstats <- rbind(summstats, temp)

summstats[1, ] <- c("\\textbf{Variable}", "\\textbf{N}", "\\textbf{Mean}", 
                    "\\textbf{S.D.}", "\\textbf{Min.}", "\\textbf{Max.}")

summstats[2:nrow(summstats),1] <- paste0("\\hangindent=2em ", summstats[2:nrow(summstats), 1])

filename = paste0("table-A2.tex")
summstats.final <-
  xtable::xtable(
    summstats,
    label = "table:summ-donation-counts",
    caption = "Summary Statistics of Donation Counts",
    align = "lp{7cm}|rrrrr"
  )
summstats.final %>%
  xtable::print.xtable(
    size="\\small",
    include.rownames=FALSE,
    include.colnames=FALSE,
    type="latex",
    file=filename,
    hline.after=c(0,1,nrow(summstats)),
    caption.placement="top",
    sanitize.text.function=function(x){x}, table.placement="H")





##### Table A.3 #####

mod_count_tea <- 
  dat %>% 
  filter(prior_donors) %>%
  feols(count_tea_party ~ foreclosure_rate_pop_tract | parcel_hh_id + district_cycle, data=., cluster="GEOID10")
mod_count_nottea <- 
  dat %>% 
  filter(prior_donors) %>%
  feols(count_not_tea_party ~ foreclosure_rate_pop_tract | parcel_hh_id + district_cycle, data=., cluster="GEOID10")

setFixest_dict(c(count_tea_party = "Tea Party Candidates",
                 count_not_tea_party = "Other Republican Candidates",
                 foreclosure_rate_pop_tract = "Census Tract Foreclosure Rate",
                 parcel_hh_id = "Donor",
                 district_cycle = "District-Cycle",
                 GEOID10 = "Census Tract"
)
)

setFixest_etable(fitstat = ~ n)

tablestyle  = style.tex(main="aer",
                        fixef.suffix = " FEs",
                        fixef.where ="var",
                        fixef.title = "\\midrule",
                        stats.title = "\\midrule",
                        tablefoot=F,
                        yesNo="\\checkmark")

etable(mod_count_tea, mod_count_nottea, 
       file = paste0("table-A3.tex"),
       title = "Comparing Counts of Itemized Contributions Across Republican Candidate Types",
       cluster=~GEOID10,
       drop = "(Intercept)",
       label = "table:main-results-count",
       digits=3,
       digits.stats=2,
       style.tex=tablestyle,
       replace = TRUE,
       depvar = TRUE,
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05)
)

rm(list=ls(pattern="mod_"))
gc()





##### Table A.4 #####

mod_post2010_tea <- 
  dat %>% 
  filter(prior_donors) %>%
  mutate(
    teaparty_cand = ifelse(cycle<2010, 0, teaparty_cand)
  ) %>%
  feols(teaparty_cand ~ foreclosure_rate_pop_tract | parcel_hh_id + district_cycle, data=., cluster="GEOID10")

mod_post2010_nottea <- 
  dat %>% 
  filter(prior_donors) %>%
  mutate(
    notteaparty_cand = ifelse(cycle<2010, notteaparty_cand+teaparty_cand,
                              notteaparty_cand)
  ) %>%
  feols(notteaparty_cand ~ foreclosure_rate_pop_tract | parcel_hh_id + district_cycle, data=., cluster="GEOID10")

setFixest_dict(c(teaparty_cand = "Tea Party (Post-2009 Only)",
                 notteaparty_cand = "Other Republican Candidates",
                 foreclosure_rate_pop_tract = "Census Tract Foreclosure Rate",
                 parcel_hh_id = "Donor",
                 district_cycle = "District-Cycle",
                 GEOID10 = "Census Tract"
)
)

setFixest_etable(fitstat = ~ n)

tablestyle  = style.tex(main="aer",
                        fixef.suffix = " FEs",
                        fixef.where ="var",
                        fixef.title = "\\midrule",
                        stats.title = "\\midrule",
                        tablefoot=F,
                        yesNo="\\checkmark")

etable(mod_post2010_tea, mod_post2010_nottea, 
       file = paste0("table-A4.tex"),
       title = "Comparing Rates of Itemized Contributions Across Republican Candidate Types",
       cluster=~GEOID10,
       drop = "(Intercept)",
       label = "table:main-results-post2010",
       digits=3,
       digits.stats=2,
       style.tex=tablestyle,
       replace = TRUE,
       depvar = TRUE,
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05)
)

rm(list=ls(pattern="mod_"))
gc()





##### Table A.5 #####

mod_post2010data_tea <- 
  dat %>% 
  filter(prior_donors) %>%
  filter(cycle>=2010) %>%
  feols(teaparty_cand ~ foreclosure_rate_pop_tract | parcel_hh_id + district_cycle, data=., cluster="GEOID10")

mod_post2010data_nottea <- 
  dat %>% 
  filter(prior_donors) %>%
  filter(cycle>=2010) %>%
  feols(notteaparty_cand ~ foreclosure_rate_pop_tract | parcel_hh_id + district_cycle, data=., cluster="GEOID10")

setFixest_dict(c(teaparty_cand = "Tea Party Candidates",
                 notteaparty_cand = "Other Republican Candidates",
                 foreclosure_rate_pop_tract = "Census Tract Foreclosure Rate",
                 parcel_hh_id = "Donor",
                 district_cycle = "District-Cycle",
                 GEOID10 = "Census Tract"
)
)

setFixest_etable(fitstat = ~ n)

tablestyle  = style.tex(main="aer",
                        fixef.suffix = " FEs",
                        fixef.where ="var",
                        fixef.title = "\\midrule",
                        stats.title = "\\midrule",
                        tablefoot=F,
                        yesNo="\\checkmark")

etable(mod_post2010data_tea, mod_post2010data_nottea, 
       file = paste0("table-A5.tex"),
       title = "Comparing Rates of Itemized Contributions Across Republican Candidate Types (Post-2009 Data Only)",
       cluster=~GEOID10,
       drop = "(Intercept)",
       label = "table:main-results-post2010data",
       digits=3,
       digits.stats=2,
       style.tex=tablestyle,
       replace = TRUE,
       depvar = TRUE,
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05)
)

rm(list=ls(pattern="mod_"))
gc()





##### Table A.6 #####

mod_sw_tea <- 
  dat %>% 
  filter(prior_donors) %>%
  feols(teaparty_cand_sw ~ foreclosure_rate_pop_tract | parcel_hh_id + district_cycle, data=., cluster="GEOID10")

mod_sw_nottea <- 
  dat %>% 
  filter(prior_donors) %>%
  feols(notteaparty_cand_sw ~ foreclosure_rate_pop_tract | parcel_hh_id + district_cycle, data=., cluster="GEOID10")

setFixest_dict(c(teaparty_cand_sw = "Tea Party (Skocpol \\& Williamson)",
                 notteaparty_cand_sw = "Other Republican Candidates",
                 foreclosure_rate_pop_tract = "Census Tract Foreclosure Rate",
                 parcel_hh_id = "Donor",
                 district_cycle = "District-Cycle",
                 GEOID10 = "Census Tract"
)
)

setFixest_etable(fitstat = ~ n)

tablestyle  = style.tex(main="aer",
                        fixef.suffix = " FEs",
                        fixef.where ="var",
                        fixef.title = "\\midrule",
                        stats.title = "\\midrule",
                        tablefoot=F,
                        yesNo="\\checkmark")

etable(mod_sw_tea, mod_sw_nottea, 
       file = paste0("table-A6.tex"),
       title = "Comparing Rates of Itemized Contributions Across Republican Candidate Types",
       cluster=~GEOID10,
       drop = "(Intercept)",
       label = "table:main-results-sw",
       digits=3,
       digits.stats=2,
       style.tex=tablestyle,
       replace = TRUE,
       fontsize = "small",
       depvar = TRUE,
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05)
)

rm(list=ls(pattern="mod_"))
gc()





##### Table A.7 #####

mod_tea_hu <- 
  dat %>%
  filter(prior_donors) %>%
  feols(teaparty_cand ~ foreclosure_rate_hu_tract | parcel_hh_id + district_cycle, cluster="GEOID10", data=.)

mod_nottea_hu <- 
  dat %>%
  filter(prior_donors) %>%
  feols(notteaparty_cand ~ foreclosure_rate_hu_tract | parcel_hh_id + district_cycle, cluster="GEOID10", data=.)

setFixest_dict(c(teaparty_cand = "Tea Party Candidates",
                 notteaparty_cand = "Other Republican Candidates",
                 parcel_hh_id = "Donor",
                 district_cycle = "District-Cycle",
                 foreclosure_rate_hu_tract = "Census-Tract Foreclosure Rate"
)
)


setFixest_etable(fitstat = ~ n)

tablestyle  = style.tex(main="aer",
                        fixef.suffix = " FEs",
                        fixef.where ="var",
                        fixef.title = "\\midrule",
                        stats.title = "\\midrule",
                        tablefoot=F,
                        yesNo="\\checkmark")

etable(mod_tea_hu, mod_nottea_hu, 
       file = paste0("table-A7.tex"),
       title = "Treatment Effects from Census-Tract Foreclosure Rate (Household-Based) By Candidate Type",
       cluster=~GEOID10,
       drop = "(Intercept)",
       label = "table:foreclosure-hu",
       digits=3,
       digits.stats=2,
       style.tex=tablestyle,
       replace = TRUE,
       depvar = TRUE
)

rm(list=ls(pattern="mod_"))
gc()





##### Table A.8 #####

leads <-
  dat %>% filter(prior_donors) %>% select(GEOID10, cycle, foreclosure_rate_pop_tract) %>%
  group_by(GEOID10, cycle) %>% slice(1) %>% ungroup() %>%
  arrange(GEOID10, cycle) %>% group_by(GEOID10) %>% mutate(
    foreclosure_lead1 = tlead(foreclosure_rate_pop_tract, n=2, cycle),
    foreclosure_lead2 = tlead(foreclosure_rate_pop_tract, n=4, cycle),
    foreclosure_lead3 = tlead(foreclosure_rate_pop_tract, n=6, cycle)
  ) %>% ungroup()

leads %<>% select(GEOID10, cycle,
                  starts_with("foreclosure_lead")
)
gc()
dat %<>% left_join(leads, by=c("GEOID10", "cycle"))


leads <- 1:3
leads %<>% as.data.frame()
names(leads) <- c("nlead")

for(i in 1:nrow(leads)) {
  
  nlead <- leads$nlead[i]
  if(nlead==0) {
    lead.vars <- NULL
  } else {
    lead.vars <- paste("foreclosure_lead", 1:nlead)
    lead.vars %<>% str_replace_all(., " ", "")
  }
  
  vars <- c("foreclosure_rate_pop_tract", lead.vars)
  
  assign(paste0("formula", i), paste0("teaparty_cand ~ ", paste0(vars, collapse=" + "), " | parcel_hh_id + district_cycle"))
  assign(paste0("mod", i), feols(as.formula(get(paste0("formula", i))), data=dat[dat$prior_donors, ], cluster="GEOID10"))
  
}

setFixest_dict(c(teaparty_cand = "Tea Party Candidates",
                 foreclosure_rate_pop_tract = "Census Tract Foreclosure Rate",
                 foreclosure_lead1 = "Census Tract Foreclosure Rate (t+1)",
                 foreclosure_lead2 = "Census Tract Foreclosure Rate (t+2)",
                 foreclosure_lead3 = "Census Tract Foreclosure Rate (t+3)",
                 parcel_hh_id = "Donor",
                 district_cycle = "District-Cycle",
                 GEOID10 = "Census Tract"
)
)

setFixest_etable(fitstat = ~ n)

tablestyle  = style.tex(main="aer",
                        fixef.suffix = " FEs",
                        fixef.where ="var",
                        fixef.title = "\\midrule",
                        stats.title = "\\midrule",
                        tablefoot=F,
                        yesNo="\\checkmark")

etable(mod1, mod2, mod3,
       file = paste0("table-A8.tex"),
       title = "Test for Pre-Trends in Itemized Contributions to Tea Party Candidates",
       cluster=~GEOID10,
       drop = "(Intercept)",
       label = "table:granger",
       digits=3,
       digits.stats=2,
       style.tex=tablestyle,
       replace = TRUE,
       depvar = FALSE,
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05)
)

rm(list=ls(pattern="mod_"))
gc()





##### Table A.9 #####

mod_popden_tea <- 
  dat %>% 
  filter(prior_donors) %>%
  feols(teaparty_cand ~ foreclosure_rate_pop_tract | parcel_hh_id + district_cycle_popdenscat, data=., cluster="GEOID10")

setFixest_dict(c(teaparty_cand = "Tea Party Candidates",
                 notteaparty_cand = "Other Republican Candidates",
                 foreclosure_rate_pop_tract = "Census Tract Foreclosure Rate",
                 parcel_hh_id = "Donor",
                 district_cycle_popdenscat = "District-Cycle-Population Density Tercile",
                 GEOID10 = "Census Tract"
)
)

setFixest_etable(fitstat = ~ n)

tablestyle  = style.tex(main="aer",
                        fixef.suffix = " FEs",
                        fixef.where ="var",
                        fixef.title = "\\midrule",
                        stats.title = "\\midrule",
                        tablefoot=F,
                        yesNo="\\checkmark")

etable(mod_popden_tea, 
       file = paste0("table-A9.tex"),
       title = "Treatment Effect on Itemized Contributions to Tea Party Candidates, Allowing Separate Common Trends by Population Density Terciles",
       cluster=~GEOID10,
       drop = "(Intercept)",
       label = "table:popden-robustness",
       digits=3,
       digits.stats=2,
       style.tex=tablestyle,
       replace = TRUE,
       depvar = FALSE,
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05)
)

rm(list=ls(pattern="mod_"))
gc()





##### Table A.10 #####

mod_type_tea <- 
  dat %>% 
  filter(prior_donors) %>%
  feols(teaparty_cand ~ foreclosure_rate_pop_tract | parcel_hh_id + district_type_cycle, data=., cluster="GEOID10")

mod_type_tea_2008 <- 
  dat %>% 
  filter(prior_donors) %>%
  filter(cycle>=2008) %>%
  mutate(
    teaparty_cand2008 = teaparty_cand
  ) %>%
  feols(teaparty_cand2008 ~ foreclosure_rate_pop_tract | parcel_hh_id + district_type_cycle, data=., cluster="GEOID10")

setFixest_dict(c(teaparty_cand = "All Cycles",
                 teaparty_cand2008 = "2008 Onward",
                 foreclosure_rate_pop_tract = "Census Tract Foreclosure Rate",
                 parcel_hh_id = "Donor",
                 district_type_cycle = "District-Cycle-Donor Type",
                 GEOID10 = "Census Tract"
)
)

setFixest_etable(fitstat = ~ n)

tablestyle  = style.tex(main="aer",
                        fixef.suffix = " FEs",
                        fixef.where ="var",
                        fixef.title = "\\midrule",
                        stats.title = "\\midrule",
                        tablefoot=F,
                        yesNo="\\checkmark")

etable(mod_type_tea, mod_type_tea_2008, 
       file = paste0("table-A10.tex"),
       title = "Treatment Effect on Itemized Contributions to Tea Party Candidates, Allowing Separate Common Trends by Donor Type",
       cluster=~GEOID10,
       drop = "(Intercept)",
       label = "table:precrisis-robustness",
       digits=3,
       digits.stats=2,
       style.tex=tablestyle,
       replace = TRUE,
       depvar = FALSE,
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05)
)

rm(list=ls(pattern="mod_"))
gc()





##### Table A.11 #####

########## NOTICE: exceptions for public access ########## 
# The following individual-level homeownership covariate, which is used in Table A.11, is suppressed to comply with CoreLogic's Memorandum of Understanding (MOU) -----
# 3) "foreclosure" (an indicator for whether a given campaign donor's residence was foreclosed in each election cycle).
# Users who wish to test alternative specifications to those presented in Table A.11 may supply their code to me, and I would be happy to run their code on my copy of the CoreLogic data set for as long as I have access to it.

# mod_tea <- 
#   dat %>%
#   filter(prior_donors) %>%
#   mutate(
#     foreclosure = ifelse(is.na(foreclosure), FALSE, TRUE)
#   ) %>%
#   feols(teaparty_cand ~ foreclosure_rate_pop_tract + foreclosure_hall | parcel_hh_id + district_cycle, cluster="GEOID10", data=.)
# 
# mod_nottea <- 
#   dat %>%
#   filter(prior_donors) %>%
#   mutate(
#     foreclosure = ifelse(is.na(foreclosure), FALSE, TRUE)
#   ) %>%
#   feols(notteaparty_cand ~ foreclosure_rate_pop_tract + foreclosure_hall | parcel_hh_id + district_cycle, cluster="GEOID10", data=.)
# 
# setFixest_dict(c(teaparty_cand = "Tea Party Candidates",
#                  notteaparty_cand = "Other Republican Candidates",
#                  parcel_hh_id = "Donor",
#                  district_cycle = "District-Cycle",
#                  foreclosure_rate_pop_tract = "Census-Tract Foreclosure Rate",
#                  foreclosureTRUE = "Own Foreclosure"
# )
# )
# 
# 
# setFixest_etable(fitstat = ~ n)
# 
# tablestyle  = style.tex(main="aer",
#                         fixef.suffix = " FEs",
#                         fixef.where ="var",
#                         fixef.title = "\\midrule",
#                         stats.title = "\\midrule",
#                         tablefoot=F,
#                         yesNo="\\checkmark")
# 
# etable(mod_tea, mod_nottea, 
#        file = paste0("table-A11.tex"),
#        title = "Treatment Effects from Census-Tract Foreclosure Rate vs. Donors' Own Foreclosures on Rates of Itemized Giving By Candidate Type",
#        cluster=~GEOID10,
#        drop = "(Intercept)",
#        label = "table:own-foreclosure",
#        digits=3,
#        digits.stats=2,
#        style.tex=tablestyle,
#        replace = TRUE,
#        depvar = TRUE
# )





##### Table A.12 #####

for(cut in c(200, 500, 1000, 2000, 5000)) {
    
    lhs <- paste0("teaparty_cand_cut", cut)
    
    assign(paste0("formula", cut), paste0(lhs, " ~ foreclosure_rate_pop_tract | parcel_hh_id + district_cycle"))
    assign(paste0("mod_cut_", cut), feols(as.formula(get(paste0("formula", cut))), data=dat[dat$prior_donors, ], cluster="GEOID10"))
    gc()
    
}

setFixest_dict(c(teaparty_cand_cut200 = "$\\geq \\$200$",
                 teaparty_cand_cut500 = "$\\geq \\$500$",
                 teaparty_cand_cut1000 = "$\\geq \\$1,000$",
                 teaparty_cand_cut2000 = "$\\geq \\$2,000$",
                 teaparty_cand_cut5000 = "$\\geq \\$5,000$",
                 foreclosure_rate_pop_tract = "Census Tract Foreclosure Rate",
                 parcel_hh_id = "Donor",
                 district_cycle = "District-Cycle",
                 GEOID10 = "Census Tract"
)
)

setFixest_etable(fitstat = ~ n)

tablestyle  = style.tex(main="aer",
                        fixef.suffix = " FEs",
                        fixef.where ="var",
                        fixef.title = "\\midrule",
                        stats.title = "\\midrule",
                        tablefoot=F,
                        yesNo="\\checkmark")

etable(mod_cut_200, mod_cut_500, mod_cut_1000, mod_cut_2000, mod_cut_5000,
       file = paste0("table-A12.tex"),
       title = "Treatment Effect on Itemized Contributions to Tea Party Candidates Across Amount Thresholds",
       cluster=~GEOID10,
       drop = "(Intercept)",
       label = "table:main-results-cutoff",
       digits=3,
       digits.stats=2,
       style.tex=tablestyle,
       replace = TRUE,
       depvar = TRUE,
       fontsize="small",
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05)
)

# rm(list=ls(pattern="mod_"))
# gc()





##### Table A.13 #####

vec <- c(200, 500, 1000, 2000, 5000)
vec_mean <- vec_coef <- rep(NA_real_, length(vec))
vec_coef <- rep(NA_real_, length(vec))
vec_counterfactual4 <- rep(NA_real_, length(vec))
vec_counterfactual10 <- rep(NA_real_, length(vec))
vec_counterfactual56 <- rep(NA_real_, length(vec))
for(a in vec) {
  vec_mean[vec==a] <- mean(dat$sum_teaparty_cand>=a)
  temp <- get(paste0("mod_cut_", a))
  vec_coef[vec==a] <- temp$coefficients
  vec_counterfactual4[vec==a] <- (temp$coefficients*4/1000)/mean(dat$sum_teaparty_cand>=a)
  vec_counterfactual10[vec==a] <- (temp$coefficients*10/1000)/mean(dat$sum_teaparty_cand>=a)
  vec_counterfactual56[vec==a] <- (temp$coefficients*56/1000)/mean(dat$sum_teaparty_cand>=a)
}

magnitude_table <- cbind(vec, vec_counterfactual4, vec_counterfactual10, vec_counterfactual56)
magnitude_table %<>% as.data.frame()
magnitude_table %<>% mutate(
  vec = paste0("\\$", format(vec, digits=0, big.interval=3L, big.mark=",")),
  vec_counterfactual4 = paste0(as.character(round(vec_counterfactual4*100, 1)), "\\%"),
  vec_counterfactual10 = paste0(as.character(round(vec_counterfactual10*100, 1)), "\\%"),
  vec_counterfactual56 = paste0(as.character(round(vec_counterfactual56*100, 1)), "\\%")
)

names(magnitude_table) <- NULL

addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- 0
addtorow$command <- 
  c("\\hline \\\\[-1.8ex] \n
    & \\multicolumn{3}{c}{Predicted Reduction in Likelihood of Itemized Contributions} \\\\  
    \\cmidrule(l){2-4} \n 
    Cutoff Amount for Itemized Giving & +4 foreclosures per 1,000 residents & +10 foreclosures per 1,000 residents & +56 foreclosures per 1,000 residents \\\\ \n
    \\hline \\\\[-1.8ex] \n")

filename = paste0("table-A13.tex")
xtable(magnitude_table,
       caption="Foreclosure-Induced Reduction in Campaign Contributions to Tea Partiers Is Sharpest for Large Contributions",
       label="table:counterfactual-magnitude-cutoff",
       align= c("p{0\\textwidth}",
                "p{0.22\\textwidth}",
                "p{0.22\\textwidth}",
                "p{0.22\\textwidth}",
                "p{0.22\\textwidth}"
       )
) %>% 
  print.xtable(include.rownames = FALSE, comment=FALSE, type="latex",
               sanitize.text.function=function(x){x}, 
               add.to.row = addtorow, 
               hline.after = 5,
               file=filename,
               caption.placement = "top"
  )

rm(list=ls(pattern="mod_"))
gc()





##### Figure 2 #####

ltv <- NA
ltv %<>% as.data.frame()
ltv$mine_us <- 
  ltv_summ %>% filter(!is.na(rate_high_ltv)) %$%
  {sum(rate_high_ltv*n, na.rm=TRUE)/sum(n, na.rm=TRUE)}
ltv$mine_nc <- 
  ltv_summ %>% filter(state=="NC") %$% rate_high_ltv

# Summary statistics from McCartney (2021)
ltv$us <- 0.359 
ltv$nc <- 0.233
ltv %<>% melt()
ltv$variable %<>% factor(levels=c("mine_us", "mine_nc", "nc", "us"))

p <- 
  ltv %>% 
  filter(variable!="us") %>%
  mutate(
    variable = case_when(
      as.character(variable)=="mine_us" ~ "All Republican donors", 
      as.character(variable)=="mine_nc" ~ "NC Republican donors", 
      as.character(variable)=="nc" ~ "NC voters (McCartney 2021)"
    )
  ) %>% ggplot(mapping=aes(x = reorder(variable, value), y=value, 
                           fill=variable))+
  geom_bar(stat="identity")+
  theme_light()+
  scale_fill_manual(values=c("#636363", "#f0f0f0", "#bdbdbd"))+
  scale_y_continuous(labels=percent, limits=c(0, 0.28))+
  labs(
    title = "Shares of Households with High Loan-to-Value Ratios in 2008"
  )+
  theme(
    axis.text = element_text(size=13),
    axis.title = element_blank(),
    legend.title = element_blank(),
    legend.text = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size=15, vjust=1),
    legend.position="none")+
  annotate(geom="text", label=paste0(as.character(round(ltv$value[ltv$variable=="mine_us"]*100, 1)), "%"), x=1, y=0.06, size=5)+
  annotate(geom="text", label=paste0(as.character(round(ltv$value[ltv$variable=="mine_nc"]*100, 1)), "%"), x=2, y=0.06, size=5)+
  annotate(geom="text", label=paste0(as.character(round(ltv$value[ltv$variable=="nc"]*100, 1)), "%"), x=3, y=0.26, size=5)
ggsave(filename = paste0("figure-2.pdf"), device = cairo_pdf, width=7.5, height=2.5)





##### Figure 3 #####

donor_weights <- 
  dat %>% 
  group_by(GEOID10, parcel_hh_id) %>% slice(1) %>% ungroup() %>% 
  group_by(GEOID10) %>% summarize(
    n_parcel_hh_id = sum(prior_donors)
  ) %>% ungroup()
missing.tracts <- census_race$GEOID10[!(census_race$GEOID10 %in% donor_weights$GEOID10)] %>% unique()
temp <- missing.tracts %>% as.data.frame()
names(temp) = "GEOID10"
temp$GEOID10 %<>% as.character()
temp$n_parcel_hh_id = 0
donor_weights <- bind_rows(donor_weights, temp)
census_race %<>% left_join(donor_weights, by="GEOID10")

tempdat <- census_race
tempdat2 <- tempdat
tempdat %<>% mutate(
  version = "Weighted mean by no. unique donors: 0.866",
  weight = n_parcel_hh_id/sum(n_parcel_hh_id)
)
tempdat2 %<>% mutate(
  version = "Unweighted mean: 0.761",
  weight = 1/nrow(tempdat2)
)
tempdat %$% weighted.mean(SHRWHT, weight) # 0.886
tempdat2 %$% mean(SHRWHT) # 0.761

tempdat3 <- bind_rows(tempdat, tempdat2)
tempdat3 %<>% select(-n_parcel_hh_id)
tempdat3 %<>% mutate(
  SHRWHT = case_when(
    .$SHRWHT<0 ~ NA_real_,
    .$SHRWHT>1 ~ NA_real_,
    TRUE ~ SHRWHT
  )
)

p.white <- 
  tempdat3 %>% 
  rename(`% White Residents` = SHRWHT, `Median value` = version) %>%
  ggplot(mapping=
           aes(x=`% White Residents`, weight=weight, group=`Median value`,
               fill=`Median value`))+
  geom_density(alpha=0.5)+
  theme_light()+
  scale_fill_grey()+
  labs(
    title = "Share of White Residents in Census Tracts in 2000",
  )+
  theme(
    axis.text = element_text(size=13),
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size=15, vjust=1),
    legend.text = element_text(size=13),
    legend.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill="transparent",colour=NA),
    legend.position = c(0.43, 0.65)
  )+
  scale_alpha(guide = 'none')
ggsave(filename = paste0("figure-3.pdf"), device = cairo_pdf, width=5, height=2.5)

