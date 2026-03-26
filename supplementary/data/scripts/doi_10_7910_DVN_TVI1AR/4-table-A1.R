# Run time: 1:20
# CPU memory use: 5.5 GB


library(here)
dir <- here()
setwd(dir)
source("0-init.R")





##### Load data #####

dat <- readRDS("donor-regression-data.rds")
census_race <- readRDS("census-racial-composition.rds")
cand <- readRDS("congressional-district-data.rds")

dat %<>% filter(prior_donors)
gc()

varnames <- c("", "N", "mean", "s.d.", "min.", "max")
varnames %<>% t()





# Census Tract Foreclosure Rate -----

foreclosure.rate <- 
  dat %>% 
  group_by(GEOID10, cycle) %>% slice(1) %>% ungroup() %$% 
  foreclosure_rate_pop_tract

temp <- 
  c("Census-Tract Level Foreclosure Rate By Cycle", 
    format(length(foreclosure.rate[!is.na(foreclosure.rate)]), big.mark=","),
    signif(mean(foreclosure.rate, na.rm=TRUE), digits=3),
    signif(sd(foreclosure.rate, na.rm=TRUE), digits=3),
    round(min(foreclosure.rate, na.rm=TRUE), digits=3),
    round(max(foreclosure.rate, na.rm=TRUE), digits=3)
  )
temp %<>% t() 
summstats <- rbind(varnames, temp)





# All Donation Indicators -----

sum.vars <- c(
  "teaparty_cand",
  "notteaparty_cand",
  "teaparty_gen",
  "teaparty_pri",
  "teaparty_incum",
  "teaparty_nonincum",
  "teaparty_in_dist",
  "teaparty_out_dist"
)

for(var in sum.vars) {
  
  summstats2 <- 
    c(var, 
      format(nrow(dat), big.mark=","),
      signif(mean(as.numeric(dat[[var]]), na.rm=TRUE), digits=3),
      signif(sd(as.numeric(dat[[var]]), na.rm=TRUE), digits=3),
      round(min(as.numeric(dat[[var]]), na.rm=TRUE), digits=3),
      round(max(as.numeric(dat[[var]]), na.rm=TRUE), digits=3)
    )
  summstats2 %<>% t() 
  summstats %<>% rbind(summstats2)
  
}


# Pre-Crisis Loan-To-Value (LTV) Ratio -----
# Any Pre-Crisis Home Equity Line of Credit (HELOC) -----

########## NOTICE: exceptions for public access ########## 
# The following individual-level homeownership covariates are suppressed to comply with CoreLogic's Memorandum of Understanding (MOU) -----
# 1) "loan" (whether a campaign donor had taken out any HELOCs prior to the financial crisis); and
# 2) "ltv" (a campaign donor's estimated loan-to-value ratio prior to the financial crisis).
# Users who wish to test alternative ways to construct these measures may supply their code to me, and I would be happy to run their code on my copy of the CoreLogic data set for as long as I have access to it.

# # ltv
# ltv.trim.tract <- 
#   dat %>% 
#   group_by(parcel_hh_id) %>% 
#   slice(1) %>% ungroup() %$% ltv_trim_tract
# # windsorize at 99% percentile
# ltv.trim.tract <- 
#   ifelse(ltv.trim.tract>quantile(ltv.trim.tract, 0.99, na.rm=TRUE), 
#          quantile(ltv.trim.tract, 0.99, na.rm=TRUE), ltv.trim.tract)
# summstats2 <-
#   c("ltv.trim.tract", 
#     format(length(ltv.trim.tract[!is.na(ltv.trim.tract)]), big.mark=","),
#     signif(mean(ltv.trim.tract, na.rm=TRUE), digits=3),
#     signif(sd(ltv.trim.tract, na.rm=TRUE), digits=3),
#     round(min(ltv.trim.tract, na.rm=TRUE), digits=3),
#     round(max(ltv.trim.tract, na.rm=TRUE), digits=3)
#   )
# summstats2 %<>% t() 
# summstats %<>% rbind(summstats2)

# # HELOC
# pre.crisis.equity.years <- 
#   dat %>% 
#   select(parcel_hh_id, pre_crisis_equity_years) %>% 
#   group_by(parcel_hh_id) %>% 
#   slice(1) %>% ungroup() %$% pre_crisis_equity_years
# # windsorize at 99% percentile
# HELOC <- !is.na(pre.crisis.equity.years)
# summstats2 <-
#   c("HELOC", 
#     format(length(HELOC), big.mark=","),
#     signif(mean(as.numeric(HELOC), na.rm=TRUE), digits=3),
#     signif(sd(as.numeric(HELOC), na.rm=TRUE), digits=3),
#     round(min(as.numeric(HELOC), na.rm=TRUE), digits=3),
#     round(max(as.numeric(HELOC), na.rm=TRUE), digits=3)
#   )
# summstats2 %<>% t() 
# summstats %<>% rbind(summstats2)





# Pre-Crisis No. of Unique GOP Donors By Census Tract -----

n.unique.donors <-
  dat %>% select(GEOID10, parcel_hh_id) %>% 
  group_by(GEOID10, parcel_hh_id) %>% slice(1) %>% ungroup() %>% 
  group_by(GEOID10) %>% summarize(
    n.unique.donors = n()
  ) %>% ungroup() %$% n.unique.donors
summstats2 <-
  c("n.unique.donors", 
    format(length(n.unique.donors), big.mark=","),
    signif(mean(as.numeric(n.unique.donors), na.rm=TRUE), digits=3),
    signif(sd(as.numeric(n.unique.donors), na.rm=TRUE), digits=3),
    round(min(as.numeric(n.unique.donors), na.rm=TRUE), digits=3),
    round(max(as.numeric(n.unique.donors), na.rm=TRUE), digits=3)
  )
summstats2 %<>% t() 
summstats %<>% rbind(summstats2)



# 2000 Share of White Residents By Census Tract -----

census_race %<>% filter(GEOID10 %in% dat$GEOID10)
SHRWHT <- ifelse(census_race$SHRWHT>1, 1, census_race$SHRWHT)
summstats2 <-
  c("SHRWHT", 
    format(length(SHRWHT[!is.na(SHRWHT)]), big.mark=","),
    signif(mean(as.numeric(SHRWHT), na.rm=TRUE), digits=3),
    signif(sd(as.numeric(SHRWHT), na.rm=TRUE), digits=3),
    round(min(as.numeric(SHRWHT), na.rm=TRUE), digits=3),
    round(max(as.numeric(SHRWHT), na.rm=TRUE), digits=3)
  )
summstats2 %<>% t() 
summstats %<>% rbind(summstats2)





# Change in Congressional District Mortgage Default Rate, 2001-07 -----

default.rates <- cand$chHOMdefrate0208
summstats2 <-
  c("default.rates", 
    format(length(default.rates[!is.na(default.rates)]), big.mark=","),
    signif(mean(as.numeric(default.rates), na.rm=TRUE), digits=3),
    signif(sd(as.numeric(default.rates), na.rm=TRUE), digits=3),
    round(min(as.numeric(default.rates), na.rm=TRUE), digits=3),
    round(max(as.numeric(default.rates), na.rm=TRUE), digits=3)
  )
summstats2 %<>% t() 
summstats %<>% rbind(summstats2)





# Any Tea Party Non-incumbent in 2010 -----

emergence <- cand$is_tea_party_challpri2010
summstats2 <-
  c("emergence", 
    format(length(emergence[!is.na(emergence)]), big.mark=","),
    signif(mean(as.numeric(emergence), na.rm=TRUE), digits=3),
    signif(sd(as.numeric(emergence), na.rm=TRUE), digits=3),
    round(min(as.numeric(emergence), na.rm=TRUE), digits=3),
    round(max(as.numeric(emergence), na.rm=TRUE), digits=3)
  )
summstats2 %<>% t() 
summstats %<>% rbind(summstats2)





# Tea Party Candidate’s 2010 Primary Vote Share -----

primary.votes <- cand$tea_party_share
summstats2 <-
  c("primary.votes", 
    format(length(primary.votes[!is.na(primary.votes)]), big.mark=","),
    signif(mean(as.numeric(primary.votes), na.rm=TRUE), digits=3),
    signif(sd(as.numeric(primary.votes), na.rm=TRUE), digits=3),
    round(min(as.numeric(primary.votes), na.rm=TRUE), digits=3),
    round(max(as.numeric(primary.votes), na.rm=TRUE), digits=3)
  )
summstats2 %<>% t() 
summstats %<>% rbind(summstats2)





# Assembling table -----

summstats[, 1] <- c(
  "", 
  "Census Tract Foreclosure Rate", 
  "Any Itemized Contrib. to Tea Partiers",
  "Any Itemized Contrib. to Non-Tea Party House GOP Candidates",
  "Any Itemized Contrib. to Tea Partiers in General Elections",
  "Any Itemized Contrib. to Tea Partiers in Primary Elections",
  "Any Itemized Contrib. to Incumbent Tea Partiers",
  "Any Itemized Contrib. to Non-incumbent Tea Partiers",
  "Any Itemized Contrib. to In-District Tea Partiers",
  "Any Itemized Contrib. to Out-of-District Tea Partiers",
  # "Pre-Crisis Loan-To-Value (LTV) Ratio", 
  # "Any Pre-Crisis Home Equity Line of Credit (HELOC)",
  "Pre-Crisis No. of Unique GOP Donors By Census Tract", 
  "2000 Share of White Residents By Census Tract",
  "Change in Congressional District Mortgage Default Rate, 2001-07",
  "Any Tea Party Non-incumbent in 2010",
  "Tea Party Candidate's 2010 Primary Vote Share"
)

summstats[1, ] <- c("\\textbf{Variable}", "\\textbf{N}", "\\textbf{Mean}", 
                    "\\textbf{S.D.}", "\\textbf{Min.}", "\\textbf{Max.}")

summstats[2:nrow(summstats),1] <- paste0("\\hangindent=2em ", summstats[2:nrow(summstats), 1])

filename = paste0("table-A1.tex")
summstats.final <-
  xtable::xtable(
    summstats,
    label = "table:summ",
    caption = "Summary Statistics of Key Variable",
    align = "lp{7cm}|rrrrr"
  )
summstats.final %>%
  xtable::print.xtable(
    size="\\small",
    include.rownames=FALSE,
    include.colnames=FALSE,
    type="latex",
    file=filename,
    hline.after=c(0,1,nrow(summstats.final)),
    caption.placement="top",
    sanitize.text.function=function(x){x}, table.placement="H")
