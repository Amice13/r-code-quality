# Run time: 00:01:51
# CPU memory usage:	5.9 GB




library(here)
dir <- here()
setwd(dir)
source("0-init.R")





##### Load data #####

dat <- readRDS("donor-panel.rds")









##### Generate leverage measures #####

########## NOTICE: exceptions for public access ########## 
# The following individual-level homeownership covariates are suppressed to comply with CoreLogic's Memorandum of Understanding (MOU) -----
# 1) "loan" (whether a campaign donor had taken out any HELOCs prior to the financial crisis); and
# 2) "ltv_high" (whether a campaign donor had a high estimated leverage ratio prior to the financial crisis).
# Users who wish to test alternative ways to construct these measures may supply their code to me, and I would be happy to run their code on my copy of the CoreLogic data set for as long as I have access to it.

# # Loan
# dat %<>% mutate(
#   loan = !is.na(pre_crisis_equity_years),
#   loan = ifelse(start_cycle>2006, NA, loan)
# )

# # LTV
# lev_summ <- dat %>% filter(cycle==2006)
# names(lev_summ)[names(lev_summ)=="ltv_trim_tract_rate"] = "pre_crisis_ltv_trim_tract"
# lev_summ <- lev_summ[, c("parcel_hh_id", "pre_crisis_ltv_trim_tract")]
# dat %<>% left_join(lev_summ, by="parcel_hh_id")
# dat %<>% mutate(
#   ltv_high = pre_crisis_ltv_trim_tract>=0.9
# )





##### Generate indicator for own foreclosure #####

########## NOTICE: exceptions for public access ########## 
# The following individual-level homeownership covariate is suppressed to comply with CoreLogic's Memorandum of Understanding (MOU) -----
# 3) "foreclosure" (an indicator of whether a campaign donor was foreclosed upon in a given cycle)
# Users who wish to test alternative ways to construct this measure may supply their code to me, and I would be happy to run their code on my copy of the CoreLogic data set for as long as I have access to it.

# apns_sales <- readRDS("apns-sales.rds")
# foreclosures <- 
#   apns_sales %>% 
#   filter(foreclosure.hall) %>% 
#   mutate(
#     cycle = ifelse(transaction.year%%2==0, transaction.year, transaction.year+1)
#   ) %>% 
#   select(consolidated.parcel, cycle, foreclosure.hall) %>%
#   group_by(consolidated.parcel, cycle) %>% slice(1) %>% ungroup()
# foreclosures %<>% mutate(
#   cycle = cycle-2
# )
# dat %<>% left_join(foreclosures, by=c("consolidated.parcel", "cycle"))





##### Make fixed effects factor variables #####

########## NOTICE: exceptions for public access ########## 
# Fixed effects that involve interactions with either of the aforementioned suppressed variables ("loan" or "ltv_high") are similarly suppressed to comply with CoreLogic's Memorandum of Understanding (MOU) -----
# Users who wish to test alternative ways to construct these measures may supply their code to me, and I would be happy to run their code on my copy of the CoreLogic data set for as long as I have access to it.

dat %<>% mutate(
  
  district_cycle = as.factor(paste0(as.character(district), " ~ ", cycle)),

  # district_cycle_loan =
  #   as.factor(paste(district, cycle, as.character(loan), sep="~")),

  # district_cycle_lev =
  #   as.factor(paste(district, cycle, as.character(ltv_high), sep="~")),
  
  district_cycle_sharewhite =
    as.factor(paste(as.character(district), cycle, case_when(
      sharewhite_l ~ "L",
      sharewhite_m ~ "M",
      sharewhite_h ~ "H"
    )
    )),
  
  district_cycle_popdenscat = paste0(as.character(district_cycle),'~',popdenscat),
  district_cycle_popdenscat = ifelse(is.na(popdenscat),NA,district_cycle_popdenscat),
  district_cycle_popdenscat = factor(district_cycle_popdenscat),
  
  district_type_cycle = as.factor(paste0(as.character(district_cycle), "~", precrisis_tea))
  
)

dat$GEOID10 %<>% as.factor()
dat$district %<>% as.factor()





##### Winsorize foreclosure rates ######

cutoff.pop = 0.0618 
dat %<>% mutate(
  
  foreclosure_rate_pop_tract =
    ifelse(foreclosure_rate_pop_tract>=cutoff.pop, cutoff.pop, foreclosure_rate_pop_tract),
  
  foreclosure_sharewhite_l = foreclosure_rate_pop_tract*sharewhite_l,
  foreclosure_sharewhite_m = foreclosure_rate_pop_tract*sharewhite_m,
  foreclosure_sharewhite_h = foreclosure_rate_pop_tract*sharewhite_h
  
)

cutoff.hu = 0.132
dat %<>% mutate(
  foreclosure_rate_hu_tract =
    ifelse(foreclosure_rate_hu_tract>=cutoff.hu, cutoff.hu, foreclosure_rate_hu_tract)
)





##### Generate dummies for Tea Party donations #####

sum.vars <- names(dat)[str_detect(names(dat), "sum_")]
sum.vars <- sum.vars[str_detect(sum.vars, "tea")]
print(sum.vars)
for (var in sum.vars) {
  command <- paste0(
    "dat %<>% mutate( \n",
    str_sub(var,5,-1), " = (", var, ">0) \n",
    ")"
  )
  eval(parse(text=command))
}

dat %<>% mutate(
  netteaparty_cand = teaparty_cand - notteaparty_cand
)



##### Generate dummies for Tea Party donation amount cutoffs #####

# sum.vars <- names(dat)[str_detect(names(dat), "sum_")]
# sum.vars <- sum.vars[str_detect(sum.vars, "tea")]

# print(sum.vars)

sum.vars <- "sum_teaparty_cand"

for(cut in c(200, 500, 1000, 2000, 5000)) {
  for (var in sum.vars) {
    command <- paste0(
      "dat %<>% mutate( \n",
      str_sub(var,5,-1), "_cut", cut, " = (", var, ">", cut, ") \n",
      ")"
    )
    eval(parse(text=command))
  }
}





##### Drop unused variables to save space #####

vars.to.drop <- names(dat)[str_sub(names(dat), 1, 4)=="sum_"]
vars.to.drop <- setdiff(vars.to.drop, "sum_teaparty_cand")
dat %<>% select(-vars.to.drop)

gc()





##### Save regression data #####

dat %>% saveRDS("donor-regression-data.rds")
