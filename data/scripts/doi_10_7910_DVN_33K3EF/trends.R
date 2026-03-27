# Uncomment the line below to install packages if not already installed
# install.packages(c("lfe","readstata13","stargazer","devtools"))

library(lfe)
library(readstata13)
library(stargazer)
library(devtools)
remotes::install_github("ChandlerLutz/starpolishr")
library(starpolishr)

# set working directory
# setwd()

# Load data
us_panel = read.dta13("us_panel.dta")

## Drop observations with missing covariates
us_panel = us_panel[ which(us_panel$ln_pop!="NA" & us_panel$ln_foreign!="NA"),]

## Create linear time trend variable
us_panel$trend = rep(NA,nrow(us_panel))
us_panel$trend[us_panel$year==1840] = 1
us_panel$trend[us_panel$year==1850] = 2
us_panel$trend[us_panel$year==1860] = 3
us_panel$trend[us_panel$year==1870] = 4
us_panel$trend[us_panel$year==1880] = 5
us_panel$trend[us_panel$year==1890] = 6
us_panel$trend[us_panel$year==1900] = 7

## Quadratic time trend variable
us_panel$trend2 = us_panel$trend*us_panel$trend

## Four model specifications for each DV. For example, "farm1" corresponds to estimates displayed in column (1), 
## "farm2" corresponds to estimates in column (2), and so forth.

farm1 = felm(ln_realfarm ~ ln_po  | as.factor(fips) + as.factor(year) + as.numeric(trend):as.factor(fips)| 0 | fips,data=us_panel) 
farm2 = felm(ln_realfarm ~ ln_po + ln_pop + ln_foreign | as.factor(fips) + as.factor(year) + as.numeric(trend):as.factor(fips)| 0 | fips,data=us_panel) 
farm3 = felm(ln_realfarm ~ ln_po | as.factor(fips) + as.factor(year) + (as.numeric(trend) + as.numeric(trend2)):as.factor(fips)|  0 | fips,data=us_panel) 
farm4 = felm(ln_realfarm ~ ln_po  + ln_pop + ln_foreign | as.factor(fips) + as.factor(year) + (as.numeric(trend) + as.numeric(trend2)):as.factor(fips)|  0 | fips,data=us_panel) 

mfgout1 = felm(ln_realmfgout ~ ln_po  | as.factor(fips) + as.factor(year) + as.numeric(trend):as.factor(fips)| 0 | fips,data=us_panel) 
mfgout2 = felm(ln_realmfgout ~ ln_po + ln_pop + ln_foreign | as.factor(fips) + as.factor(year) + as.numeric(trend):as.factor(fips)| 0 | fips,data=us_panel) 
mfgout3 = felm(ln_realmfgout ~ ln_po | as.factor(fips) + as.factor(year) + (as.numeric(trend) + as.numeric(trend2)):as.factor(fips)|  0 | fips,data=us_panel) 
mfgout4 = felm(ln_realmfgout ~ ln_po  + ln_pop + ln_foreign | as.factor(fips) + as.factor(year) + (as.numeric(trend) + as.numeric(trend2)):as.factor(fips)|  0 | fips,data=us_panel) 

mfgcap1 = felm(ln_realmfgcap ~ ln_po  | as.factor(fips) + as.factor(year) + as.numeric(trend):as.factor(fips)| 0 | fips,data=us_panel) 
mfgcap2 = felm(ln_realmfgcap ~ ln_po + ln_pop + ln_foreign | as.factor(fips) + as.factor(year) + as.numeric(trend):as.factor(fips)| 0 | fips,data=us_panel) 
mfgcap3 = felm(ln_realmfgcap ~ ln_po | as.factor(fips) + as.factor(year) + (as.numeric(trend) + as.numeric(trend2)):as.factor(fips)|  0 | fips,data=us_panel) 
mfgcap4 = felm(ln_realmfgcap ~ ln_po  + ln_pop + ln_foreign | as.factor(fips) + as.factor(year) + (as.numeric(trend) + as.numeric(trend2)):as.factor(fips)|  0 | fips,data=us_panel) 



farm_panel = stargazer(farm1,farm2,farm3,farm4,
               title = "Proximal Effects of Postal Infrastructure in U.S. Counties, 1850-1900 (County-specific trends)",
               dep.var.labels = c(""),
               dep.var.caption = "",
               label="county2-trend",align=TRUE,
               omit = c("trend","trend2","ln_pop","ln_foreign"),
               covariate.labels = c("Post offices (ln)"),                              
               add.lines = list(c("Controls","\\multicolumn{1}{c}{No}","\\multicolumn{1}{c}{Yes}",
                                  "\\multicolumn{1}{c}{No}","\\multicolumn{1}{c}{Yes}"),
						c("Trends","\\multicolumn{1}{c}{Linear}","\\multicolumn{1}{c}{Linear}",
                                  "\\multicolumn{1}{c}{Quadratic}","\\multicolumn{1}{c}{Quadratic}"),
						c("County fixed effects","\\checkmark","\\checkmark","\\checkmark","\\checkmark"),
						c("Year fixed effects","\\checkmark","\\checkmark","\\checkmark","\\checkmark")),
               keep.stat = c("n"),
               star.cutoffs = 0.05,star.char = "*",table.placement="!ht",
               notes.append = FALSE,notes.label = "",column.sep.width="-10pt",
               # table.layout ="-ld-#-t-as-n",
               font.size="small",
			notes="\\parbox[t]{0.95\\textwidth}{\\footnotesize \\textit{Note}: Entries are linear regression coefficients with 
                     standard errors clustered on county in parentheses. The dependent variables are the logged values of the outcomes 
				listed at the top of each panel. $^{*}$p$<$0.05 (two-tailed test).}")


out_panel = stargazer(mfgout1,mfgout2,mfgout3,mfgout4,
               title = "",
               dep.var.labels = c(""),
               dep.var.caption = "",
               label="",align=TRUE,
               omit = c("trend","trend2","ln_pop","ln_foreign"),
               covariate.labels = c("Post offices (ln)"),                              
               add.lines = list(c("Controls","\\multicolumn{1}{c}{No}","\\multicolumn{1}{c}{Yes}",
                                  "\\multicolumn{1}{c}{No}","\\multicolumn{1}{c}{Yes}"),
						c("Trends","\\multicolumn{1}{c}{Linear}","\\multicolumn{1}{c}{Linear}",
                                  "\\multicolumn{1}{c}{Quadratic}","\\multicolumn{1}{c}{Quadratic}"),
						c("County fixed effects","\\checkmark","\\checkmark","\\checkmark","\\checkmark"),
						c("Year fixed effects","\\checkmark","\\checkmark","\\checkmark","\\checkmark")),
               keep.stat = c("n"),
               star.cutoffs = 0.05,star.char = "*",table.placement="!ht",
               notes.append = FALSE,notes.label = "",column.sep.width="-10pt",
               # table.layout ="-ld-#-t-as-n",
               font.size="small",
			notes="\\parbox[t]{0.95\\textwidth}{\\footnotesize \\textit{Note}: Entries are linear regression coefficients with 
                     standard errors clustered on county in parentheses. The dependent variables are the logged values of the outcomes 
				listed at the top of each panel. $^{*}$p$<$0.05 (two-tailed test).}")

cap_panel = stargazer(mfgcap1,mfgcap2,mfgcap3,mfgcap4,
               title = "",
               dep.var.labels = c(""),
               dep.var.caption = "",
               label="",align=TRUE,
               omit = c("trend","trend2","ln_pop","ln_foreign"),
               covariate.labels = c("Post offices (ln)"),                              
               add.lines = list(c("Controls","\\multicolumn{1}{c}{No}","\\multicolumn{1}{c}{Yes}",
                                  "\\multicolumn{1}{c}{No}","\\multicolumn{1}{c}{Yes}"),
						c("Trends","\\multicolumn{1}{c}{Linear}","\\multicolumn{1}{c}{Linear}",
                                  "\\multicolumn{1}{c}{Quadratic}","\\multicolumn{1}{c}{Quadratic}"),
						c("County fixed effects","\\checkmark","\\checkmark","\\checkmark","\\checkmark"),
						c("Year fixed effects","\\checkmark","\\checkmark","\\checkmark","\\checkmark")),
               keep.stat = c("n"),
               star.cutoffs = 0.05,star.char = "*",table.placement="!ht",
               notes.append = FALSE,notes.label = "",column.sep.width="-10pt",
               # table.layout ="-ld-#-t-as-n",
               font.size="small",
			notes="\\parbox[t]{0.95\\textwidth}{\\footnotesize \\textit{Note}: Entries are linear regression coefficients with 
                     standard errors clustered on county in parentheses. The dependent variables are the logged values of the outcomes 
				listed at the top of each panel. $^{*}$p$<$0.05 (two-tailed test).}")
     



