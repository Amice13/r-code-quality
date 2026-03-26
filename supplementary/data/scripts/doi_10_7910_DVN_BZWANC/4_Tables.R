# Code updated: 2021 April 27

#### DESCRIPTION ####
#
# Generates and saves (as .tex) tables:
# (1) Summary statistics / balance checks for WaterSmart RCT
# (2) RD estimates for automated enforcement using nonparametric estimator: Panel [A] is reduced-form (and first stage) and Panel [B] is LATE
# (3) RD estimates separately by month: July, August, September, October
# (4) RCT estimates for WaterSmart HWR for pooled treatment period: (1) no controls (2) household controls (3) household FE and week-of-sample FE
# (5) RCT estimates for WaterSmart HWR separated by time period: 
#      (1) pooled treatement period (replicate) (2) Late-May to June (3) July-October (4) July-Oct irrigation not allowed (5) July-Oct irrigation allowed
# (6) RCT estimates for WaterSmart HWR separated by time period, using only households within the highest-third of pre-treatment water use
# (7) RCT estimates for WaterSmart HWR separated by time period, days of week groups (non-T/S, T/S), and hour of day groups (pre-9am/post-6pm, 9am-6pm)
# (A1) RD estimates separately by month in 2016: July, August, September
# (A2) RD estimates for automated enforcement using nonparametric estimator and only HWR-treated household sample
# (A3) RCT estimates for WaterSmart HWR by day-of-week and time block (00:00-09:00, 09:00-18:00, 18:00-00:00), for Late May - June and for July - October
# (A4) Robustness of RCT estimates for WaterSmart HWR separated by time period (Table 5), with added panels for weather controls and for week-of-sample fixed effects
#
# Notes:
# 1. All tables use only 'insample' households (insample == 1):
#    1. Burbank household in WaterSmart 'experimental' or 'control' arms (b1, b0)
#    2. Nonmissing pretreatment consumption: first observed no later than 2014-04-01
#    3. Nonmissing posttreatment consumption: is observed during 2015-05-01 through 2015-10-31
#    4. Nonmissing running variable for irrigation violation warnings 
#

#### PREAMBLE ####
rm(list = ls()) # Clear workspace
wd = "~/Dropbox/Shared/Watersmart/data" ; setwd(wd) # * SET THIS * Directory containing compiled WaterSmart data
tablesdir = "~/Dropbox/Overleaf/WFPR_Watersmart/tables" # * SET THIS * Directory in which to save generated tables
pl = c('data.table', 'fasttime', 'zoo', 'lfe', 'rdrobust', 'stargazer') ; lapply(pl, require, character.only = T) # R packages to load

#### READ IN DATA ####

# 1. Cross section of Burbank households
# 2. Panel of household daily consumption measures
# 3. Panel including all irrigation violation warnings

cs <- fread(file.path(wd, 'Analysis_Burbank_Cross_Section.csv'))
dcon <- fread(file.path(wd, 'Analysis_Burbank_Daily_Consumption.csv'))
viols <- fread(file.path(wd, 'Analysis_Burbank_Violations.csv'))
weather <- fread(file.path(wd, 'burbank_precip_temp_byday_201516.csv'))
setkey(cs, residence_id) ; setkey(dcon, residence_id, readdate) ; setkey(viols, residence_id, violdate)

# Keep only 'insample' households (see definition in above note)
cs <- cs[insample == 1] ; dcon <- dcon[insample == 1] ; viols <- viols[insample == 1]

# Format dates
cs[, wsdate := as.Date(wsdate, format = '%Y-%m-%d')][, changeoccdate := as.Date(changeoccdate, format = '%Y-%m-%d')]
dcon[, readdate := as.Date(readdate, format = '%Y-%m-%d')]
viols[, violdate := as.Date(violdate, format = '%Y-%m-%d')]
weather[, readdate := as.Date(DATE, format = '%m/%d/%Y')]

# Generate week-of-sample indicator for consumption
dcon[, readwk := as.integer(floor(difftime(readdate, '2015-06-30', units = 'days')/7))]
dcon[, weekdate := min(readdate), by = readwk]

# Compute average weekly consumption by household: April 1st 2014 through December 31st 2015
dcon[, ts96 := 0L] ; dcon[wday(readdate) %in% c(3, 7), ts96 := b9gal + a6gal]
wcon <- dcon[weekdate >= as.Date('2014-04-01') & weekdate <= as.Date('2015-12-31'), 
             .(wgal = sum(dailygal), ts96 = sum(ts96)), by = .(residence_id, arm, weekdate)]
wcon[, nonts96 := wgal - ts96]
setkey(wcon, residence_id) ; setkey(cs, residence_id)
wcon <- merge(wcon, cs[, .(residence_id, autoviol, runningv)])

# Compute average weekly consumption by household during summer of 2016: July 1st 2016 - September 30th 2016 (available months)
wcon.2016 <- dcon[weekdate >= as.Date('2016-07-01') & weekdate <= as.Date('2016-09-30'), 
             .(wgal = sum(dailygal), ts96 = sum(ts96)), by = .(residence_id, arm, weekdate)]
wcon.2016[, nonts96 := wgal - ts96]
setkey(wcon.2016, residence_id) ; setkey(cs, residence_id)
wcon.2016 <- merge(wcon.2016, cs[, .(residence_id, autoviol, runningv)])

## Form data set of consumption by day of week and time block (00:00-09:00, 09:00-18:00, 18:00-00:00)
dcon.timeblock <- dcon[readdate >= as.Date('2015-05-20') & readdate < as.Date('2015-11-01'), 
                            .(residence_id, arm, readdate, dailygal, b9gal, a9b6gal = dailygal - b9gal - a6gal, a6gal)]
dcon.timeblock[, regime := ifelse(readdate < as.Date('2015-07-01'), '1-LMJ', '2-JO')]

## Form data set of regression control covariates

cs.controls <- cs[, .(residence_id, arm, everws, autoviol, pretviol, lotsize = `Lot Size (SqFt)`, irrarea =  `Irrigable Area (SqFt)`, 
                      sqft = `Home Size (SqFt)`, yearbuilt =  `Year Home Built`, floors = `Num Floors`, 
                      bedrooms = `Num Bedrooms`, bathrooms = `Num Bathrooms`)]
setkey(cs.controls, residence_id)

# Correct for missing data: continuous variables
cs.controls[is.na(lotsize), lotsize := 0] ; cs.controls[, lotsize_missing := ifelse(lotsize == 0, 1, 0)] ; table(cs.controls$lotsize_missing) # 222
cs.controls[is.na(sqft), sqft := 0] ; cs.controls[, sqft_missing := ifelse(sqft == 0, 1, 0)] ; table(cs.controls$sqft_missing) # 253
cs.controls[is.na(yearbuilt), yearbuilt := 0] ; cs.controls[, yearbuilt_missing := ifelse(yearbuilt == 0, 1, 0)] ; table(cs.controls$yearbuilt_missing) # 231

# Correct for missing data: indicator variables
cs.controls[is.na(floors), floors := 99] ; table(cs.controls$floors == 99) # 923
cs.controls[is.na(bedrooms), bedrooms := 99] ; table(cs.controls$bedrooms == 99) # 239
cs.controls[is.na(bathrooms), bathrooms := 99] ; table(cs.controls$bathrooms == 99) # 236

summary(cs.controls) # Should have no missing values now

#### (1) SUMMARY STATISTICS WITH RCT BALANCE CHECKS ####

## Compile subset of variables for which to generate statistics (means and group t-tests)

# Cross-sectional variables:
stats.cs <- cs[, .(residence_id, arm, everws, pretviol, lotsize = `Lot Size (SqFt)`, irrarea =  `Irrigable Area (SqFt)`, 
                   sqft = `Home Size (SqFt)`, yearbuilt =  `Year Home Built`, floors = `Num Floors`, 
                   bedrooms = `Num Bedrooms`, bathrooms = `Num Bathrooms`)]
setkey(stats.cs, residence_id)

# Average weekly consumption per residence_id for pre-treatment period
preawc <- wcon[weekdate >= '2014-04-01' & weekdate <= '2015-03-31', .(prewkcon = mean(wgal)), by = residence_id]
setkey(preawc, residence_id)
sumstats <- merge(stats.cs, preawc)
rm(stats.cs, preawc)

# Quick check of means
sumstats[, lapply(.SD, mean, na.rm = T), by = arm]

# Generate table
dt.sumstats <- lapply(c('everws', 'pretviol', 'lotsize', 'irrarea', 'sqft', 
                        'yearbuilt', 'floors', 'bedrooms', 'bathrooms', 'prewkcon'), function(v) {
                          data.table(
                            covariate = v,
                            sumstats[arm == 'b0', .(mean_b0 = sprintf('%.4g', mean(get(v), na.rm = T)))],
                            sumstats[arm == 'b1', .(mean_b1 = sprintf('%.4g', mean(get(v), na.rm = T)))],
                            sumstats[, .(pval = sprintf('%.2G', t.test(get(v) ~ arm)[['p.value']]))]
                          )
                        })
dt.sumstats <- rbindlist(dt.sumstats) 

dt.sumstats <- rbind(data.table(
  covariate = 'numhh', 
  mean_b0 = length(sumstats[arm == 'b0']$arm),
  mean_b1 = length(sumstats[arm == 'b1']$arm),
  pval = ''), dt.sumstats)

dt.sumstats[, difference := sprintf('%.6g', as.numeric(mean_b1) - as.numeric(mean_b0))]

dt.sumstats <- cbind(data.table(
  cov_label = c(
    'Number of households',
    'Sent WaterSmart HWR',
    'Prior water violation',
    'Lot size (SqFt)',
    'Irrigable area (SqFt)',
    'House size (SqFt)',
    'Year built',
    'Number of floors',
    'Number of bedrooms',
    'Number of bathrooms',
    'Weekly water gallons'
  )), dt.sumstats)

dt.sumstats <- dt.sumstats[, .(cov_label, mean_b0, mean_b1, difference, pval)]
setnames(dt.sumstats, c('Covariate', 'Control', 'HWR-treated', 'Difference', 'p-value'))
dt.sumstats[Covariate %in% c('Number of households', 'Sent WaterSmart HWR'), c('Difference', 'p-value') := '']
dt.sumstats

## Save table as .tex using Stargazer

sink(file.path(tablesdir, 'Stargazer_SumStats.tex'))
stargazer(dt.sumstats, summary = F, rownames = F,
          table.placement = "H", digits = 3, header = F,
          notes.align = "l", notes.append = F, notes.label = "\\textit{Notes:}",
          title = "Summary statistics and randomization balance checks", label = "tab:Summary-Statistics",
          notes = "\\begin{minipage}[t]{13.5cm}{\\footnotesize
  Notes: Table~\\ref{tab:Summary-Statistics} shows statistics by WaterSmart Home Water Reports (HWR) treatment arm for household-level covariates. 
The first two columns show means by treatment arm for all households in the randomization sample, 
Column (3) shows the difference in means, and Column (4) shows the p-values for t-tests of whether 
the difference in group means is significantly different from zero. Initial HWR were sent to treated households during the billing 
cycle spanning from mid April through mid May 2015. All outcomes in the lower panel are 
determined prior to the randomization and prior to the automated irrigation restrictions enforcement. For pre-treatment weekly water consumption, we use each 
household's average weekly gallons consumed during April 2014 through March 2015, spanning a full year prior to both treatments. 
}\\end{minipage}"
)
sink()

rm(dt.sumstats, sumstats)

#### (2) RD ESTIMATES USING NONPARAMETRIC ESTIMATOR ####

# Note: using a fixed bandwidth of 80 gallons

wcon.rdd <- wcon[year(weekdate) == 2015 & month(weekdate) %in% 7:10 & abs(runningv) <= 80]
setkey(wcon.rdd, runningv)

## Panel [A]: Reduced-form estimates

# Column 1: first-stage
mod.rd.firststage <- rdrobust(wcon.rdd$autoviol, wcon.rdd$runningv, h = 80)

# Column 2: reduced-form for all hours of the week
mod.rd.rf.allhours <- rdrobust(wcon.rdd$wgal, wcon.rdd$runningv, h = 80)

# Column 3: reduced-form for hours of the week when irrigation is NOT allowed
mod.rd.rf.notallowed <- rdrobust(wcon.rdd$nonts96, wcon.rdd$runningv, h = 80)

# Column 4: reduced-form for hours of the week when irrigation is allowed
mod.rd.rf.allowed <- rdrobust(wcon.rdd$ts96, wcon.rdd$runningv, h = 80)

## Panel [B]: LATE estimates

# Column 2: LATE for all hours of the week
mod.rd.late.allhours <- rdrobust(wcon.rdd$wgal, wcon.rdd$runningv, fuzzy = wcon.rdd$autoviol, h = 80)

# Column 3: LATE for hours of the week when irrigation is NOT allowed
mod.rd.late.notallowed <- rdrobust(wcon.rdd$nonts96, wcon.rdd$runningv, fuzzy = wcon.rdd$autoviol, h = 80)

# Column 4: LATE for hours of the week when irrigation is allowed
mod.rd.late.allowed <- rdrobust(wcon.rdd$ts96, wcon.rdd$runningv, fuzzy = wcon.rdd$autoviol, h = 80)

## Sample means of outcome variables
mean.rdd.firststage <- sprintf('%0.2f', mean(wcon.rdd[abs(runningv) <= 80]$autoviol))
mean.rdd.allhours <- sprintf('%4.0f',mean(wcon.rdd[abs(runningv) <= 80]$wgal))
mean.rdd.notallowed <- sprintf('%4.0f',mean(wcon.rdd[abs(runningv) <= 80]$nonts96))
mean.rdd.allowed <- sprintf('%3.0f',mean(wcon.rdd[abs(runningv) <= 80]$ts96))

# Extract results from estimated models and combine into a table
dt.mod.rd <- data.table(
  outcome = c('First-stage', 'All hours', 'Non-irrig. hours', 'Irrig. hours.', 'All hours', 'Non-irrig. hours', 'Irrig. hours.'
  ),
  coef = c(mod.rd.firststage$coef[[1]],
            mod.rd.rf.allhours$coef[[1]],
            mod.rd.rf.notallowed$coef[[1]],
            mod.rd.rf.allowed$coef[[1]],
            mod.rd.late.allhours$coef[[1]],
            mod.rd.late.notallowed$coef[[1]],
            mod.rd.late.allowed$coef[[1]]
  ),
  se = c(mod.rd.firststage$se[[3]],
         mod.rd.rf.allhours$se[[3]],
         mod.rd.rf.notallowed$se[[3]],
         mod.rd.rf.allowed$se[[3]],
         mod.rd.late.allhours$se[[3]],
         mod.rd.late.notallowed$se[[3]],
         mod.rd.late.allowed$se[[3]]
  ),
  bw = c(mod.rd.firststage$bws[1],
         mod.rd.rf.allhours$bws[1],
         mod.rd.rf.notallowed$bws[1],
         mod.rd.rf.allowed$bws[1],
         mod.rd.late.allhours$bws[1],
         mod.rd.late.notallowed$bws[1],
         mod.rd.late.allowed$bws[1]
  ),
  obs = c(sum(mod.rd.firststage$N_h),
          sum(mod.rd.rf.allhours$N_h),
          sum(mod.rd.rf.notallowed$N_h),
          sum(mod.rd.rf.allowed$N_h),
          sum(mod.rd.late.allhours$N_h),
          sum(mod.rd.late.notallowed$N_h),
          sum(mod.rd.late.allowed$N_h)
  )
)

# Determine significance stars
dt.mod.rd[, stars := '']
dt.mod.rd[abs(coef / se) > 1.645, stars := '*']
dt.mod.rd[abs(coef / se) > 1.96, stars := '**']
dt.mod.rd[abs(coef / se) > 2.576, stars := '***']

# Format table
dt.mod.rd <- dt.mod.rd[, .(
  outcome,
  starcoef = paste0(sprintf('%1.4g', coef), stars),
  se = paste0('(', sprintf('%1.4g', se), ')'),
  bw,
  obs = formatC(obs, big.mark = ',')
)]

# Reshape into a results table to export using Stargazer
dt.rd.table <- data.table(
  col0 = c('', 'Discontinuity', '', 'Discontinuity', '', 'Sample mean', 'Bandwidth (gal)', 'Observations'),
  col1 = c(dt.mod.rd$outcome[1], dt.mod.rd$starcoef[1], dt.mod.rd$se[1], '', '', mean.rdd.firststage, dt.mod.rd$bw[1], dt.mod.rd$obs[1]),
  col2 = c(dt.mod.rd$outcome[2], dt.mod.rd$starcoef[2], dt.mod.rd$se[2], dt.mod.rd$starcoef[5], dt.mod.rd$se[5], mean.rdd.allhours, dt.mod.rd$bw[2], dt.mod.rd$obs[2]),
  col3 = c(dt.mod.rd$outcome[3], dt.mod.rd$starcoef[3], dt.mod.rd$se[3], dt.mod.rd$starcoef[6], dt.mod.rd$se[6], mean.rdd.notallowed, dt.mod.rd$bw[3], dt.mod.rd$obs[3]),
  col4 = c(dt.mod.rd$outcome[4], dt.mod.rd$starcoef[4], dt.mod.rd$se[4], dt.mod.rd$starcoef[7], dt.mod.rd$se[7], mean.rdd.allowed, dt.mod.rd$bw[4], dt.mod.rd$obs[4])
)

## Save regression output as .tex using Stargazer

sink(file.path(tablesdir, 'Stargazer_RD_Estimates.tex'))
stargazer(dt.rd.table, summary = F, rownames = F,
          title = "Regression discontinuity estimates of effects of irrigation violation notice", label = "tab:RD-Estimates",
          table.placement = "H", header = F, digit.separator = '',
          notes.align = "l", notes.append = F, 
          notes = "\\begin{minipage}[t]{15.5cm}{\\footnotesize $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01 \\hspace{0.25cm}
Notes: Each cell presents a nonparametric regression discontinuity estimate at the cutoff for automated violation notices.
All regressions use the
 \\href{https://sites.google.com/site/rdpackages/rdrobust}{``rdrobust''} software package developed and 
provided by \\cite{CalonicoCattaneoTitiunik_RDD}.
Heteroskedasticity-robust bias-corrected standard errors are estimated using the same package.
Column (1) provides the estimated first-stage for automated violation notices.
These notices were sent to households during the first week of July 2015.
Columns (2) - (4) present estimates for household weekly water consumption during July through October 2015, 
the remainder of the legal and technical local summer water season following the violation notices. 
Panel [A] shows the reduced-form estimates and Panel [B] shows the estimated local average treatment effects.
Column (2) includes water consumption pooled across all hours of the week.
Column (3) includes consumption only during hours of the week when irrigation was not legally allowed.
Column (4) includes consumption only during hours irrigation was legally allowed: Tuesdays and Saturdays 
before 9:00 a.m. or after 6:00 p.m.
          }\\end{minipage}"
)
sink()

rm(dt.mod.rd, dt.rd.table, mod.rd.firststage, mod.rd.rf.allhours, mod.rd.rf.notallowed, mod.rd.rf.allowed, mod.rd.late.allhours,
   mod.rd.late.notallowed, mod.rd.late.allowed, mean.rdd.firststage, mean.rdd.allhours, mean.rdd.notallowed, mean.rdd.allowed)

#### (3) RD ESTIMATES USING NONPARAMETRIC ESTIMATOR, SEPARATELY BY MONTH: JULY, AUGUST, SEPTEMBER, OCTOBER 2015 ####

# Note: using a fixed bandwidth of 80 gallons

## Reduced-form estimates

# Reduced form for all hours of the week: July
mod.rd.rf.allhours.july <- rdrobust(wcon.rdd[month(weekdate) == 7]$wgal, wcon.rdd[month(weekdate) == 7]$runningv, h = 80)

# Reduced form for all hours of the week: August
mod.rd.rf.allhours.august <- rdrobust(wcon.rdd[month(weekdate) == 8]$wgal, wcon.rdd[month(weekdate) == 8]$runningv, h = 80)

# Reduced form for all hours of the week: September
mod.rd.rf.allhours.september <- rdrobust(wcon.rdd[month(weekdate) == 9]$wgal, wcon.rdd[month(weekdate) == 9]$runningv, h = 80)

# Reduced form for all hours of the week: October
mod.rd.rf.allhours.october <- rdrobust(wcon.rdd[month(weekdate) == 10]$wgal, wcon.rdd[month(weekdate) == 10]$runningv, h = 80)

## LATE estimates

# LATE for all hours of the week: July
mod.rd.late.allhours.july <- rdrobust(wcon.rdd[month(weekdate) == 7]$wgal, wcon.rdd[month(weekdate) == 7]$runningv, fuzzy = wcon.rdd[month(weekdate) == 7]$autoviol, h = 80)

# LATE for all hours of the week: August
mod.rd.late.allhours.august <- rdrobust(wcon.rdd[month(weekdate) == 8]$wgal, wcon.rdd[month(weekdate) == 8]$runningv, fuzzy = wcon.rdd[month(weekdate) == 8]$autoviol, h = 80)

# LATE for all hours of the week: September
mod.rd.late.allhours.september <- rdrobust(wcon.rdd[month(weekdate) == 9]$wgal, wcon.rdd[month(weekdate) == 9]$runningv, fuzzy = wcon.rdd[month(weekdate) == 9]$autoviol, h = 80)

# LATE for all hours of the week: October
mod.rd.late.allhours.october <- rdrobust(wcon.rdd[month(weekdate) == 10]$wgal, wcon.rdd[month(weekdate) == 10]$runningv, fuzzy = wcon.rdd[month(weekdate) == 10]$autoviol, h = 80)

## Sample means
mean.july <- sprintf('%4.0f', mean(wcon.rdd[month(weekdate) == 7]$wgal))
mean.august <- sprintf('%4.0f', mean(wcon.rdd[month(weekdate) == 8]$wgal))
mean.september <- sprintf('%4.0f', mean(wcon.rdd[month(weekdate) == 9]$wgal))
mean.october <- sprintf('%4.0f', mean(wcon.rdd[month(weekdate) == 10]$wgal))

# Extract results from estimated models and combine into a table
table.rd.monthly <- data.table(
  month = c('July', 'August', 'September', 'October'),
  rfcoef = c(mod.rd.rf.allhours.july$coef[[1]],
             mod.rd.rf.allhours.august$coef[[1]],
             mod.rd.rf.allhours.september$coef[[1]],
             mod.rd.rf.allhours.october$coef[[1]]
  ),
  rfse = c(mod.rd.rf.allhours.july$se[[3]],
           mod.rd.rf.allhours.august$se[[3]],
           mod.rd.rf.allhours.september$se[[3]],
           mod.rd.rf.allhours.october$se[[3]]
  ),
  latecoef = c(mod.rd.late.allhours.july$coef[[1]],
               mod.rd.late.allhours.august$coef[[1]],
               mod.rd.late.allhours.september$coef[[1]],
               mod.rd.late.allhours.october$coef[[1]]
  ),
  latese = c(mod.rd.late.allhours.july$se[[3]],
             mod.rd.late.allhours.august$se[[3]],
             mod.rd.late.allhours.september$se[[3]],
             mod.rd.late.allhours.october$se[[3]]
  ),
  obs = c(sum(mod.rd.rf.allhours.july$N_h),
          sum(mod.rd.rf.allhours.august$N_h),
          sum(mod.rd.rf.allhours.september$N_h),
          sum(mod.rd.rf.allhours.october$N_h)
  ),
  bw = c(mod.rd.rf.allhours.july$bws[1],
         mod.rd.rf.allhours.august$bws[1],
         mod.rd.rf.allhours.september$bws[1],
         mod.rd.rf.allhours.october$bws[1]
  ),
  mean = c(mean.july, mean.august, mean.september, mean.october)
)

# Determine significance stars
table.rd.monthly[, rfstars := '']
table.rd.monthly[abs(rfcoef / rfse) > 1.645, rfstars := '*']
table.rd.monthly[abs(rfcoef / rfse) > 1.96, rfstars := '**']
table.rd.monthly[abs(rfcoef / rfse) > 2.576, rfstars := '***']
table.rd.monthly[, latestars := '']
table.rd.monthly[abs(latecoef / latese) > 1.645, latestars := '*']
table.rd.monthly[abs(latecoef / latese) > 1.96, latestars := '**']
table.rd.monthly[abs(latecoef / latese) > 2.576, latestars := '***']

# Format table
table.rd.monthly <- table.rd.monthly[, .(
  month,
  rfstarcoef = paste0(sprintf('%3.1f', rfcoef), rfstars),
  rfse = paste0('(', sprintf('%2.2f', rfse), ')'),
  latestarcoef = paste0(sprintf('%3.1f', latecoef), latestars),
  latese = paste0('(', sprintf('%3.1f', latese), ')'),
  obs = formatC(obs, big.mark = ','),
  bw,
  mean 
)]

## Save regression output as .tex using Stargazer

sink(file.path(tablesdir, 'Stargazer_RD_Estimates_Month.tex'))
stargazer(table.rd.monthly, summary = F, rownames = F,
          title = "RD estimates of effects of irrigation violation notice by month", label = "tab:RD-Estimates-Month-2015",
          table.placement = "H", header = F, digit.separator = '',
          notes.align = "l", notes.append = F, 
          notes = "\\begin{minipage}[t]{15.7cm}{\\footnotesize $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01 \\hspace{0.25cm}
Notes: Each row presents nonparametric regression discontinuity estimates at the cutoff for automated violation notices.
The outcome variable is total weekly water consumption.
The month for each week is defined based on which month the first day of the week falls within.
All regressions use the
 \\href{https://sites.google.com/site/rdpackages/rdrobust}{``rdrobust''} software package developed and 
provided by \\cite{CalonicoCattaneoTitiunik_RDD}.
Heteroskedasticity-robust bias-corrected standard errors are estimated using the same package.
          }\\end{minipage}"
)
sink()

rm(table.rd.monthly, mean.july, mean.august, mean.september, mean.october, wcon.rdd,
   mod.rd.rf.allhours.july, mod.rd.rf.allhours.august, mod.rd.rf.allhours.september, mod.rd.rf.allhours.october,
   mod.rd.late.allhours.july, mod.rd.late.allhours.august, mod.rd.late.allhours.september, mod.rd.late.allhours.october)

#### (4) RCT ESTIMATES USING POOLED TREATMENT PERIOD: LATE-MAY THROUGH OCTOBER 2015 ####

wcon.rct <- wcon[weekdate >= as.Date('2014-04-01') & weekdate <= as.Date('2015-10-31'), .(residence_id, arm, weekdate, wgal, ts96, nonts96)]
setkey(wcon.rct, residence_id) ; setkey(cs.controls, residence_id) ; setkey(cs, residence_id)
rct.panel <- merge(wcon.rct, cs[!is.na(wsdate), .(residence_id, wsdate)], all.x = T)
rct.panel <- merge(rct.panel, cs.controls[, .(residence_id, lotsize, irrarea, sqft, yearbuilt, floors, bedrooms, bathrooms, lotsize_missing, sqft_missing, yearbuilt_missing)])
setkey(rct.panel, residence_id, weekdate)
rm(wcon.rct)

# Determine HWR-treated households using 2015-May-20 as date when 'last' initial HWR report was sent
rct.panel[, hwr_treated := 0] ; rct.panel[arm == 'b1' & weekdate >= as.Date('2015-05-20'), hwr_treated := 1]

## Regression models:

# Column 1: no controls or fixed effects; using only post-treatment period (May 20 - October 31, 2015)
mod.rct.nofe <- felm(wgal ~ hwr_treated 
                     | 0 | 0 | weekdate + residence_id, data = rct.panel[weekdate >= as.Date('2015-05-20') & weekdate <= as.Date('2015-10-31')])

# Column 2: household-level pre-determined controls but no fixed effects; using only post-treatment period (May 20 - October 31, 2015)
mod.rct.controls <- felm(wgal ~ hwr_treated + lotsize + irrarea + sqft + yearbuilt + floors + bedrooms + bathrooms + lotsize_missing + sqft_missing + yearbuilt_missing
                         | 0 | 0 | weekdate + residence_id, data = rct.panel[weekdate >= as.Date('2015-05-20') & weekdate <= as.Date('2015-10-31')])

# Column 3: household fixed effects and week-of-sample fixed effects; using April 2014 through post-treatment period (April 1, 2014 - October 31, 2015)
mod.rct.hfe.wfe <- felm(wgal ~ hwr_treated
                        | weekdate + residence_id | 0 | weekdate + residence_id, data = rct.panel[weekdate >= as.Date('2015-01-01') & weekdate <= as.Date('2015-10-31')])

## Statistics and meta data to include in table
num.households <- formatC(length(unique(rct.panel[weekdate >= as.Date('2015-05-20') & weekdate <= as.Date('2015-10-31'), .(residence_id)], by = NULL)$residence_id), big.mark = ',')
mean.rct.allhours <- sprintf('%4.0f', mean(rct.panel[weekdate >= as.Date('2015-05-20') & weekdate <= as.Date('2015-10-31') & arm == 'b0']$wgal))
mean.rct.allhours.janoct <- sprintf('%4.0f', mean(rct.panel[weekdate >= as.Date('2015-01-01') & weekdate <= as.Date('2015-10-31') & arm == 'b0']$wgal))

## Save regression output as .tex using Stargazer

sink(file.path(tablesdir, 'Stargazer_RCT_Estimates.tex'))
stargazer(mod.rct.nofe, mod.rct.controls, mod.rct.hfe.wfe,
          table.placement = "H", digits = 2, keep.stat = c("n"), dep.var.labels.include = F, header = F,
          notes.align = "l", notes.append = F, notes.label = "\\textit{Notes:}",
          title = "Estimated effects of randomized WaterSmart Home Water Reports", label = "tab:RCT-Estimates",
          dep.var.caption = "Weekly water consumption in 2015 (gallons)",
          keep = c("hwr_treated"), covariate.labels = c("I\\{HWR\\}"),
          add.lines = list(c("Time period", "Late May - October", "Late May - October", "January - October"),
                           c("Household controls", "No", "Yes", "\\textemdash"),
                           c("Household fixed effects", "No", "No", "Yes"),
                           c("Week of sample FE", "No", "No", "Yes"),
                           c("Control group mean", mean.rct.allhours, mean.rct.allhours, mean.rct.allhours.janoct),
                           c("Num. of households", num.households, num.households, num.households)),
          notes = "\\begin{minipage}[t]{12.5cm}{\\footnotesize $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01 \\hspace{0.25cm}
Notes: Table~\\ref{tab:RCT-Estimates} presents estimates of the average intent-to-treat effect of the randomized WaterSmart HWR for weekly water consumption 
during 2015 for the month ranges indicated by the column titles. 
Home Water Report treated households each had been sent one HWR as of late May 2015, and monthly reports continued to be sent 
throughout (and following) October, the end of the legal and technical local summer water season.
The household control terms in Column (2) include residential lot size, irrigable area, and the home's square footage, year of construction, number of floors, 
number of bedrooms, and number of bathrooms. Standard errors in parentheses are two-way clustered by household and week. 
          }\\end{minipage}"
)
sink()

rm(num.households, mod.rct.nofe, mod.rct.controls, mod.rct.hfe.wfe, mean.rct.allhours, mean.rct.allhours.janoct)

#### (5) RCT ESTIMATES ACROSS DIFFERENT TIME PERIODS WITHIN POST-TREATMENT PERIOD ####

## Regression models:

# Column 1: pooled treatement period of late-May through October 2015 (replicate)
mod.rct.pooled <- felm(wgal ~ hwr_treated + lotsize + irrarea + sqft + yearbuilt + floors + bedrooms + bathrooms + lotsize_missing + sqft_missing + yearbuilt_missing
                       | 0 | 0 | weekdate + residence_id, data = rct.panel[weekdate >= as.Date('2015-05-20') & weekdate <= as.Date('2015-10-31')])

# Column 2: Late-May through June 2015
mod.rct.lmj <- felm(wgal ~ hwr_treated + lotsize + irrarea + sqft + yearbuilt + floors + bedrooms + bathrooms + lotsize_missing + sqft_missing + yearbuilt_missing
                    | 0 | 0 | weekdate + residence_id, data = rct.panel[weekdate >= as.Date('2015-05-20') & weekdate <= as.Date('2015-06-30')])

# Column 3: July through October 2015
mod.rct.jo <- felm(wgal ~ hwr_treated + lotsize + irrarea + sqft + yearbuilt + floors + bedrooms + bathrooms + lotsize_missing + sqft_missing + yearbuilt_missing
                   | 0 | 0 | weekdate + residence_id, data = rct.panel[weekdate >= as.Date('2015-07-01') & weekdate <= as.Date('2015-10-31')])

# Column 4: July through October 2015 only for hours when irrigation is NOT allowed
mod.rct.jo.nonts96 <- felm(nonts96 ~ hwr_treated + lotsize + irrarea + sqft + yearbuilt + floors + bedrooms + bathrooms + lotsize_missing + sqft_missing + yearbuilt_missing
                           | 0 | 0 | weekdate + residence_id, data = rct.panel[weekdate >= as.Date('2015-07-01') & weekdate <= as.Date('2015-10-31')])

# Column 5: July through October 2015 only for hours when irrigation is allowed
mod.rct.jo.ts96 <- felm(ts96 ~ hwr_treated + lotsize + irrarea + sqft + yearbuilt + floors + bedrooms + bathrooms + lotsize_missing + sqft_missing + yearbuilt_missing
                        | 0 | 0 | weekdate + residence_id, data = rct.panel[weekdate >= as.Date('2015-07-01') & weekdate <= as.Date('2015-10-31')])

## Statistics and meta data to include in table
num.households <- formatC(length(unique(rct.panel[weekdate >= as.Date('2015-05-20') & weekdate <= as.Date('2015-10-31'), .(residence_id)], by = NULL)$residence_id), big.mark = ',')
mean.rct.allhours.lmoct <- sprintf('%4.0f', mean(rct.panel[weekdate >= as.Date('2015-05-20') & weekdate <= as.Date('2015-10-31') & arm == 'b0']$wgal))
mean.rct.allhours.lmj <- sprintf('%4.0f', mean(rct.panel[weekdate >= as.Date('2015-05-20') & weekdate <= as.Date('2015-06-30') & arm == 'b0']$wgal))
mean.rct.allhours.jo <- sprintf('%4.0f', mean(rct.panel[weekdate >= as.Date('2015-07-01') & weekdate <= as.Date('2015-10-31') & arm == 'b0']$wgal))
mean.rct.nonts96.jo <- sprintf('%4.0f', mean(rct.panel[weekdate >= as.Date('2015-07-01') & weekdate <= as.Date('2015-10-31') & arm == 'b0']$nonts96))
mean.rct.ts96.jo <- sprintf('%4.0f', mean(rct.panel[weekdate >= as.Date('2015-07-01') & weekdate <= as.Date('2015-10-31') & arm == 'b0']$ts96))

## Save regression output as .tex using Stargazer

sink(file.path(tablesdir, 'Stargazer_RCT_TimePeriods.tex'))
stargazer(mod.rct.pooled, mod.rct.lmj, mod.rct.jo, mod.rct.jo.nonts96, mod.rct.jo.ts96,
          table.placement = "H", digits = 2, keep.stat = c("n"), dep.var.labels.include = F, header = F,
          notes.align = "l", notes.append = F, notes.label = "\\textit{Notes:}",
          title = "Estimated effects of randomized WaterSmart Home Water Reports by time period", label = "tab:RCT-TimePeriod",
          dep.var.caption = "Weekly water consumption in 2015 (gallons)",
          keep = c("hwr_treated"), covariate.labels = c("I\\{HWR\\}"),
          add.lines = list(c("Time period", "Late May - October", "Late May - June", "July - October", "July - October (non-allowed)", "July - October (allowed)"),
                           c("Household controls", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Control group mean", mean.rct.allhours.lmoct, mean.rct.allhours.lmj, mean.rct.allhours.jo, mean.rct.nonts96.jo, mean.rct.ts96.jo),
                           c("Num. of households", num.households, num.households, num.households, num.households, num.households)),
          notes = "\\begin{minipage}[t]{17cm}{\\footnotesize $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01 \\hspace{0.25cm}
Notes: Table~\\ref{tab:RCT-TimePeriod} presents estimates of the average intent-to-treat effect of the randomized WaterSmart HWR for weekly water consumption 
during 2015 for the time periods indicated by the column titles. 
Specifically, Column (1) includes the full 2015 summer treatment period from late May through October; Column (2) includes only late May through June, 
before the automated violation notices were sent; and Columns (3) - (5) includes July through October, after the automated violation notices were sent.
Column (4) includes consumption only during hours of the week when irrigation was not legally allowed.
Column (5) includes consumption only during hours irrigation was legally allowed: Tuesdays and Saturdays before 9:00 a.m. or after 6:00 p.m.
Home Water Report treated households each had been sent one HWR as of late May 2015, and monthly reports continued to be sent 
throughout (and following) October, the end of the legal and technical local summer water season.
The household control terms include residential lot size, irrigable area, and the home's square footage, year of construction, number of floors, 
number of bedrooms, and number of bathrooms. Standard errors in parentheses are two-way clustered by household and week. 
          }\\end{minipage}"
)
sink()

rm(num.households, mod.rct.pooled, mod.rct.lmj, mod.rct.jo, mod.rct.jo.nonts96, mod.rct.jo.ts96,
   mean.rct.allhours.lmoct, mean.rct.allhours.lmj, mean.rct.allhours.jo, mean.rct.nonts96.jo, mean.rct.ts96.jo)

#### (6) RCT ESTIMATES ACROSS DIFFERENT TIME PERIODS, USING ONLY HOUSEHOLDS WITHIN HIGHEST-THIRD OF PRE-TREATMENT CONSUMPTIONS ####

# Using full year of pre-treatment consumption spanning 2014-04-01 through 2015-03-31
ptuse <- wcon[weekdate >= as.Date('2014-04-01') & weekdate <= as.Date('2015-03-31'), .(wgal = mean(wgal, na.rm = T)), by = .(residence_id)]
ptuse[, prequantile := frank(wgal) / .N]
setkey(ptuse, residence_id) ; setkey(cs, residence_id) ; setkey(rct.panel, residence_id)
ptuse <- merge(ptuse[, .(residence_id, prequantile)], cs[, .(residence_id, autoviol)])
rct.panel.prequant <- merge(rct.panel, ptuse)

## Regression models:

# Column 1: pooled treatement period of late-May through October 2015 (replicate)
mod.q67.pooled <- felm(wgal ~ hwr_treated + lotsize + irrarea + sqft + yearbuilt + floors + bedrooms + bathrooms + lotsize_missing + sqft_missing + yearbuilt_missing
                       | 0 | 0 | weekdate + residence_id, data = rct.panel.prequant[weekdate >= as.Date('2015-05-20') & weekdate <= as.Date('2015-10-31') & prequantile > 0.667])

# Column 2: Late-May through June 2015
mod.q67.lmj <- felm(wgal ~ hwr_treated + lotsize + irrarea + sqft + yearbuilt + floors + bedrooms + bathrooms + lotsize_missing + sqft_missing + yearbuilt_missing
                    | 0 | 0 | weekdate + residence_id, data = rct.panel.prequant[weekdate >= as.Date('2015-05-20') & weekdate <= as.Date('2015-06-30') & prequantile > 0.667])

# Column 3: July through October 2015
mod.q67.jo <- felm(wgal ~ hwr_treated + lotsize + irrarea + sqft + yearbuilt + floors + bedrooms + bathrooms + lotsize_missing + sqft_missing + yearbuilt_missing
                   | 0 | 0 | weekdate + residence_id, data = rct.panel.prequant[weekdate >= as.Date('2015-07-01') & weekdate <= as.Date('2015-10-31') & prequantile > 0.667])

# Column 4: July through October 2015 only for hours when irrigation is NOT allowed
mod.q67.jo.nonts96 <- felm(nonts96 ~ hwr_treated + lotsize + irrarea + sqft + yearbuilt + floors + bedrooms + bathrooms + lotsize_missing + sqft_missing + yearbuilt_missing
                           | 0 | 0 | weekdate + residence_id, data = rct.panel.prequant[weekdate >= as.Date('2015-07-01') & weekdate <= as.Date('2015-10-31') & prequantile > 0.667])

# Column 5: July through October 2015 only for hours when irrigation is allowed
mod.q67.jo.ts96 <- felm(ts96 ~ hwr_treated + lotsize + irrarea + sqft + yearbuilt + floors + bedrooms + bathrooms + lotsize_missing + sqft_missing + yearbuilt_missing
                        | 0 | 0 | weekdate + residence_id, data = rct.panel.prequant[weekdate >= as.Date('2015-07-01') & weekdate <= as.Date('2015-10-31') & prequantile > 0.667])

## Statistics and meta data to include in table
num.households.q67 <- formatC(length(unique(rct.panel.prequant[weekdate >= as.Date('2015-05-20') & weekdate <= as.Date('2015-10-31') & prequantile > 0.667, .(residence_id)], 
                                            by = NULL)$residence_id), big.mark = ',')
mean.q67.allhours.lmoct <- sprintf('%4.0f', mean(rct.panel.prequant[weekdate >= as.Date('2015-05-20') & weekdate <= as.Date('2015-10-31') & prequantile > 0.667 & arm == 'b0']$wgal))
mean.q67.allhours.lmj <- sprintf('%4.0f', mean(rct.panel.prequant[weekdate >= as.Date('2015-05-20') & weekdate <= as.Date('2015-06-30') & prequantile > 0.667 & arm == 'b0']$wgal))
mean.q67.allhours.jo <- sprintf('%4.0f', mean(rct.panel.prequant[weekdate >= as.Date('2015-07-01') & weekdate <= as.Date('2015-10-31') & prequantile > 0.667 & arm == 'b0']$wgal))
mean.q67.nonts96.jo <- sprintf('%4.0f', mean(rct.panel.prequant[weekdate >= as.Date('2015-07-01') & weekdate <= as.Date('2015-10-31') & prequantile > 0.667 & arm == 'b0']$nonts96))
mean.q67.ts96.jo <- sprintf('%4.0f', mean(rct.panel.prequant[weekdate >= as.Date('2015-07-01') & weekdate <= as.Date('2015-10-31') & prequantile > 0.667 & arm == 'b0']$ts96))

prop.table(table(ptuse[prequantile > 0.667]$autoviol))

## Save regression output as .tex using Stargazer

sink(file.path(tablesdir, 'Stargazer_RCT_TimePeriods_Q67.tex'))
stargazer(mod.q67.pooled, mod.q67.lmj, mod.q67.jo, mod.q67.jo.nonts96, mod.q67.jo.ts96,
          table.placement = "H", digits = 2, keep.stat = c("n"), dep.var.labels.include = F, header = F,
          notes.align = "l", notes.append = F, notes.label = "\\textit{Notes:}",
          title = "Estimated effects of HWR by time period for high-volume consumers", label = "tab:RCT-TimePeriod-Q67",
          dep.var.caption = "Weekly water consumption in 2015 (gallons)",
          keep = c("hwr_treated"), covariate.labels = c("I\\{HWR\\}"),
          add.lines = list(c("Time period", "Late May - October", "Late May - June", "July - October", "July - October (non-allowed)", "July - October (allowed)"),
                           c("Household controls", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Control group mean", mean.q67.allhours.lmoct, mean.q67.allhours.lmj, mean.q67.allhours.jo, mean.q67.nonts96.jo, mean.q67.ts96.jo),
                           c("Num. of households", num.households.q67, num.households.q67, num.households.q67, num.households.q67, num.households.q67)),
          notes = "\\begin{minipage}[t]{17.5cm}{\\footnotesize $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01 \\hspace{0.25cm}
Notes: This table replicates the specifications in Table~\\ref{tab:RCT-TimePeriod} for the sub-sample of households that had pre-treatment water consumption
in the top tercile of weekly volume. Of these households, 70 percent were sent an automated violation notice in early July 2015.
Specifically, Column (1) includes the full 2015 summer treatment period from late May through October; Column (2) includes only late May through June, 
before the automated violation notices were sent; and Columns (3) - (5) includes July through October, after the automated violation notices were sent.
Column (4) includes consumption only during hours of the week when irrigation was not legally allowed.
Column (5) includes consumption only during hours irrigation was legally allowed: Tuesdays and Saturdays before 9:00 a.m. or after 6:00 p.m.
The household control terms include residential lot size, irrigable area, and the home's square footage, year of construction, number of floors, 
number of bedrooms, and number of bathrooms. Standard errors in parentheses are two-way clustered by household and week. 
          }\\end{minipage}"
)
sink()

rm(ptuse, num.households.q67, mod.q67.pooled, mod.q67.lmj, mod.q67.jo, mod.q67.jo.nonts96, mod.q67.jo.ts96, rct.panel.prequant,
   mean.q67.allhours.lmoct, mean.q67.allhours.lmj, mean.q67.allhours.jo, mean.q67.nonts96.jo, mean.q67.ts96.jo)

#### (7) RCT ESTIMATES ACROSS DIFFERENT TIME PERIODS: (NON) T/S -X- (NON) PRE-9AM/POST-6PM, WITH 2 PANELS [A] LATE MAY - JUNE [B] JULY - OCTOBER ####

## Form data set of consumption for eight time blocks: (non) Tues/Sat -X- (non) before-9/after-6 -X- Late-May-June/July-Oct.

dcon[, tuesat := 0L] ; dcon[wday(readdate) %in% c(3, 7), tuesat := 1L]
dcon[, b9a6gal := b9gal + a6gal]
dcon[, a9b6gal := dailygal - b9a6gal]
dcon[readdate >= as.Date('2015-05-20') & readdate <= as.Date('2015-06-30'), regime := '1-LMJ']
dcon[readdate >= as.Date('2015-07-01') & readdate <= as.Date('2015-10-31'), regime := '2-JO']

wcon.rct.blocks <- dcon[!is.na(regime), .(b9a6gal = sum(b9a6gal), a9b6gal = sum(a9b6gal)), by = .(residence_id, arm, weekdate, tuesat, regime)]
setkey(wcon.rct.blocks, residence_id) ; setkey(cs.controls, residence_id) ; setkey(cs, residence_id)
rct.panel.blocks <- merge(wcon.rct.blocks, cs[!is.na(wsdate), .(residence_id, wsdate)], all.x = T)
rct.panel.blocks <- merge(rct.panel.blocks, cs.controls[, .(residence_id, lotsize, irrarea, sqft, yearbuilt, floors, bedrooms, bathrooms, lotsize_missing, sqft_missing, yearbuilt_missing)])
setkey(rct.panel.blocks, residence_id, weekdate)
rm(wcon.rct.blocks)

# Determine HWR-treated households using 2015-May-20 as date when 'last' initial HWR report was sent
rct.panel.blocks[, hwr_treated := 0] ; rct.panel.blocks[arm == 'b1' & weekdate >= as.Date('2015-05-20'), hwr_treated := 1]

## Regression models:

# PANEL [A] Late-May to June

# Column 1: Non-irrig. days before-9AM/after-6PM
rct.blocks.lmj.nonirr.b9a6 <- felm(b9a6gal ~ hwr_treated + lotsize + irrarea + sqft + yearbuilt + floors + bedrooms + bathrooms + lotsize_missing + sqft_missing + yearbuilt_missing
                                   | 0 | 0 | weekdate + residence_id, data = rct.panel.blocks[regime == '1-LMJ' & tuesat == 0])

# Column 2: Non-irrig. days between 9AM-6PM
rct.blocks.lmj.nonirr.a9b6 <- felm(a9b6gal ~ hwr_treated + lotsize + irrarea + sqft + yearbuilt + floors + bedrooms + bathrooms + lotsize_missing + sqft_missing + yearbuilt_missing
                                   | 0 | 0 | weekdate + residence_id, data = rct.panel.blocks[regime == '1-LMJ' & tuesat == 0])

# Column 3: Irrig. days before-9AM/after-6PM
rct.blocks.lmj.irr.b9a6 <- felm(b9a6gal ~ hwr_treated + lotsize + irrarea + sqft + yearbuilt + floors + bedrooms + bathrooms + lotsize_missing + sqft_missing + yearbuilt_missing
                                   | 0 | 0 | weekdate + residence_id, data = rct.panel.blocks[regime == '1-LMJ' & tuesat == 1])

# Column 4: Irrig. days between 9AM-6PM
rct.blocks.lmj.irr.a9b6 <- felm(a9b6gal ~ hwr_treated + lotsize + irrarea + sqft + yearbuilt + floors + bedrooms + bathrooms + lotsize_missing + sqft_missing + yearbuilt_missing
                                   | 0 | 0 | weekdate + residence_id, data = rct.panel.blocks[regime == '1-LMJ' & tuesat == 1])

# PANEL [B] July to October

# Column 1: Non-irrig. days before-9AM/after-6PM
rct.blocks.jo.nonirr.b9a6 <- felm(b9a6gal ~ hwr_treated + lotsize + irrarea + sqft + yearbuilt + floors + bedrooms + bathrooms + lotsize_missing + sqft_missing + yearbuilt_missing
                                   | 0 | 0 | weekdate + residence_id, data = rct.panel.blocks[regime == '2-JO' & tuesat == 0])

# Column 2: Non-irrig. days between 9AM-6PM
rct.blocks.jo.nonirr.a9b6 <- felm(a9b6gal ~ hwr_treated + lotsize + irrarea + sqft + yearbuilt + floors + bedrooms + bathrooms + lotsize_missing + sqft_missing + yearbuilt_missing
                                   | 0 | 0 | weekdate + residence_id, data = rct.panel.blocks[regime == '2-JO' & tuesat == 0])

# Column 3: Irrig. days before-9AM/after-6PM
rct.blocks.jo.irr.b9a6 <- felm(b9a6gal ~ hwr_treated + lotsize + irrarea + sqft + yearbuilt + floors + bedrooms + bathrooms + lotsize_missing + sqft_missing + yearbuilt_missing
                                | 0 | 0 | weekdate + residence_id, data = rct.panel.blocks[regime == '2-JO' & tuesat == 1])

# Column 4: Irrig. days between 9AM-6PM
rct.blocks.jo.irr.a9b6 <- felm(a9b6gal ~ hwr_treated + lotsize + irrarea + sqft + yearbuilt + floors + bedrooms + bathrooms + lotsize_missing + sqft_missing + yearbuilt_missing
                                | 0 | 0 | weekdate + residence_id, data = rct.panel.blocks[regime == '2-JO' & tuesat == 1])

## Statistics and meta data to include in table
num.households <- formatC(length(unique(rct.panel.blocks$residence_id, by = NULL)), big.mark = ',')
mean.lmj.nonirr.b9a6 <- sprintf('%4.0f', mean(rct.panel.blocks[regime == '1-LMJ' & tuesat == 0 & arm == 'b0']$b9a6gal))
mean.lmj.nonirr.a9b6 <- sprintf('%4.0f', mean(rct.panel.blocks[regime == '1-LMJ' & tuesat == 0 & arm == 'b0']$a9b6gal))
mean.lmj.irr.b9a6 <- sprintf('%4.0f', mean(rct.panel.blocks[regime == '1-LMJ' & tuesat == 1 & arm == 'b0']$b9a6gal))
mean.lmj.irr.a9b6 <- sprintf('%4.0f', mean(rct.panel.blocks[regime == '1-LMJ' & tuesat == 1 & arm == 'b0']$a9b6gal))
mean.jo.nonirr.b9a6 <- sprintf('%4.0f', mean(rct.panel.blocks[regime == '2-JO' & tuesat == 0 & arm == 'b0']$b9a6gal))
mean.jo.nonirr.a9b6 <- sprintf('%4.0f', mean(rct.panel.blocks[regime == '2-JO' & tuesat == 0 & arm == 'b0']$a9b6gal))
mean.jo.irr.b9a6 <- sprintf('%4.0f', mean(rct.panel.blocks[regime == '2-JO' & tuesat == 1 & arm == 'b0']$b9a6gal))
mean.jo.irr.a9b6 <- sprintf('%4.0f', mean(rct.panel.blocks[regime == '2-JO' & tuesat == 1 & arm == 'b0']$a9b6gal))

## Save regression output as .tex using Stargazer

# PANEL [A] Late-May to June
sink(file.path(tablesdir, 'Stargazer_RCT_TimeBlocks_LMJ.tex'))
stargazer(rct.blocks.lmj.nonirr.b9a6, rct.blocks.lmj.nonirr.a9b6, rct.blocks.lmj.irr.b9a6, rct.blocks.lmj.irr.a9b6,
          table.placement = "H", digits = 2, keep.stat = c("n"), dep.var.labels.include = F, header = F,
          notes.align = "l", notes.append = F, notes.label = "\\textit{Notes:}",
          title = "Estimated effects of randomized WaterSmart Home Water Reports by time block", label = "tab:RCT-TimeBlocks-Regime",
          dep.var.caption = "Weekly water consumption in 2015 (gallons)",
          keep = c("hwr_treated"), covariate.labels = c("I\\{HWR\\}"),
          add.lines = list(c("Weeks of sample", "Late May-June", "Late May-June", "Late May-June", "Late May-June"),
                           c("Days of week", "Non-Tue/Sat.", "Non-Tue/Sat.", "Tue/Sat.", "Tue/Sat."),
                           c("Hours of day", "0-9/18-24", "9-18", "0-9/18-24", "9-18"),
                           c("Household controls", "Yes", "Yes", "Yes", "Yes"),
                           c("Panel [A] control group mean", mean.lmj.nonirr.b9a6, mean.lmj.nonirr.a9b6, mean.lmj.irr.b9a6, mean.lmj.irr.a9b6),
                           c("Num. of households", num.households, num.households, num.households, num.households)),
          notes = "\\begin{minipage}[t]{15cm}{\\footnotesize $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01 \\hspace{0.25cm}
Notes: Table~\\ref{tab:RCT-TimeBlocks-Regime} presents estimates of the average intent-to-treat effect of the randomized WaterSmart HWR for weekly water 
consumption during 2015 for the time blocks within each week indicated by the column titles. Specifically, Column (1) includes the hours from midnight 
to 9:00 and 18:00 to midnight on days other than Tuesday and Saturday. Column (2) includes hours from 9:00-18:00 on days other than Tuesday and Saturday. 
Column (3) includes the hours from midnight to 9:00 and 18:00 to midnight on Tuesday and Saturday. Column (4) includes hours from 9:00-18:00 on Tuesday 
and Saturday. Panel [A] shows estimates for the period that includes only late May through June, and Panel [B] shows estimates for the period that includes 
only July through October, after the automated violation notices were sent. Throughout the entire late May through October period, irrigation was allowed 
only on Tuesdays and Saturday before 9:00 or after 18:00, i.e. in the hours for Column (3) only. Home Water Report treated households each had been sent 
one HWR as of late May 2015, and monthly reports continued to be sent throughout (and following) October, the end of the legal and technical local summer 
water season. The household control terms include residential lot size, irrigable area, and the home's square footage, year of construction, number of 
floors, number of bedrooms, and number of bathrooms. Standard errors in parentheses are two-way clustered by household and week. 
          }\\end{minipage}"
)
sink()

# PANEL [B] July to October
sink(file.path(tablesdir, 'Stargazer_RCT_TimeBlocks_JO.tex'))
stargazer(rct.blocks.jo.nonirr.b9a6, rct.blocks.jo.nonirr.a9b6, rct.blocks.jo.irr.b9a6, rct.blocks.jo.irr.a9b6,
          table.placement = "H", digits = 2, keep.stat = c("n"), dep.var.labels.include = F, header = F,
          notes.align = "l", notes.append = F, notes.label = "\\textit{Notes:}",
          title = "Estimated effects of randomized WaterSmart Home Water Reports by time block", label = "tab:RCT-TimeBlocks-Regime",
          dep.var.caption = "Weekly water consumption in 2015 (gallons)",
          keep = c("hwr_treated"), covariate.labels = c("I\\{HWR\\}"),
          add.lines = list(c("Weeks of sample", "July-October", "July-October", "July-October", "July-October"),
                           c("Days of week", "Non-Tue/Sat.", "Non-Tue/Sat.", "Tue/Sat.", "Tue/Sat."),
                           c("Hours of day", "0-9/18-24", "9-18", "0-9/18-24", "9-18"),
                           c("Household controls", "Yes", "Yes", "Yes", "Yes"),
                           c("Panel [B] control group mean", mean.jo.nonirr.b9a6, mean.jo.nonirr.a9b6, mean.jo.irr.b9a6, mean.jo.irr.a9b6),
                           c("Num. of households", num.households, num.households, num.households, num.households)),
          notes = "\\begin{minipage}[t]{15cm}{\\footnotesize $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01 \\hspace{0.25cm}
Notes: see those for Panel [A] }\\end{minipage}"
)
sink()

rm(rct.blocks.lmj.nonirr.b9a6, rct.blocks.lmj.nonirr.a9b6, rct.blocks.lmj.irr.b9a6, rct.blocks.lmj.irr.a9b6,
   rct.blocks.jo.nonirr.b9a6, rct.blocks.jo.nonirr.a9b6, rct.blocks.jo.irr.b9a6, rct.blocks.jo.irr.a9b6,
   mean.lmj.nonirr.b9a6, mean.lmj.nonirr.a9b6, mean.lmj.irr.b9a6, mean.lmj.irr.a9b6,
   mean.jo.nonirr.b9a6, mean.jo.nonirr.a9b6, mean.jo.irr.b9a6, mean.jo.irr.a9b6, num.households, rct.panel.blocks)

#### (A1) RD ESTIMATES USING NONPARAMETRIC ESTIMATOR, SEPARATELY BY MONTH: JULY, AUGUST, SEPTEMBER 2016 ####

# Note: using a fixed bandwidth of 80 gallons

wcon.rdd.2016 <- wcon.2016[year(weekdate) == 2016 & month(weekdate) %in% 7:9 & abs(runningv) <= 80]
setkey(wcon.rdd.2016, runningv)

## Reduced-form estimates

# Reduced form for all hours of the week: July 2016
mod.rd.rf.allhours.july.2016 <- rdrobust(wcon.rdd.2016[month(weekdate) == 7]$wgal, wcon.rdd.2016[month(weekdate) == 7]$runningv, h = 80)

# Reduced form for all hours of the week: August 2016
mod.rd.rf.allhours.august.2016 <- rdrobust(wcon.rdd.2016[month(weekdate) == 8]$wgal, wcon.rdd.2016[month(weekdate) == 8]$runningv, h = 80)

# Reduced form for all hours of the week: September 2016
mod.rd.rf.allhours.september.2016 <- rdrobust(wcon.rdd.2016[month(weekdate) == 9]$wgal, wcon.rdd.2016[month(weekdate) == 9]$runningv, h = 80)

# Reduced form for all hours of the week: July - September 2016
mod.rd.rf.allhours.julsep.2016 <- rdrobust(wcon.rdd.2016[month(weekdate) %in% 7:9]$wgal, wcon.rdd.2016[month(weekdate) %in% 7:9]$runningv, h = 80)

## LATE estimates

# LATE for all hours of the week: July 2016
mod.rd.late.allhours.july.2016 <- rdrobust(wcon.rdd.2016[month(weekdate) == 7]$wgal, wcon.rdd.2016[month(weekdate) == 7]$runningv, fuzzy = wcon.rdd.2016[month(weekdate) == 7]$autoviol, h = 80)

# LATE for all hours of the week: August 2016
mod.rd.late.allhours.august.2016 <- rdrobust(wcon.rdd.2016[month(weekdate) == 8]$wgal, wcon.rdd.2016[month(weekdate) == 8]$runningv, fuzzy = wcon.rdd.2016[month(weekdate) == 8]$autoviol, h = 80)

# LATE for all hours of the week: September 2016
mod.rd.late.allhours.september.2016 <- rdrobust(wcon.rdd.2016[month(weekdate) == 9]$wgal, wcon.rdd.2016[month(weekdate) == 9]$runningv, fuzzy = wcon.rdd.2016[month(weekdate) == 9]$autoviol, h = 80)

# LATE for all hours of the week: July - September 2016
mod.rd.late.allhours.julsep.2016 <- rdrobust(wcon.rdd.2016[month(weekdate) %in% 7:9]$wgal, wcon.rdd.2016[month(weekdate) %in% 7:9]$runningv, fuzzy = wcon.rdd.2016[month(weekdate) %in% 7:9]$autoviol, h = 80)

## Sample means
mean.july.2016 <- sprintf('%4.0f', mean(wcon.rdd.2016[month(weekdate) == 7]$wgal))
mean.august.2016 <- sprintf('%4.0f', mean(wcon.rdd.2016[month(weekdate) == 8]$wgal))
mean.september.2016 <- sprintf('%4.0f', mean(wcon.rdd.2016[month(weekdate) == 9]$wgal))
mean.julsep.2016 <- sprintf('%4.0f', mean(wcon.rdd.2016[month(weekdate) %in% 7:9]$wgal))

# Extract results from estimated models and combine into a table
table.rd.monthly.2016 <- data.table(
  month = c('July', 'August', 'September', 'July-Sept.'),
  rfcoef = c(mod.rd.rf.allhours.july.2016$coef[[1]],
             mod.rd.rf.allhours.august.2016$coef[[1]],
             mod.rd.rf.allhours.september.2016$coef[[1]],
             mod.rd.rf.allhours.julsep.2016$coef[[1]]
  ),
  rfse = c(mod.rd.rf.allhours.july.2016$se[[3]],
           mod.rd.rf.allhours.august.2016$se[[3]],
           mod.rd.rf.allhours.september.2016$se[[3]],
           mod.rd.rf.allhours.julsep.2016$se[[3]]
  ),
  latecoef = c(mod.rd.late.allhours.july.2016$coef[[1]],
               mod.rd.late.allhours.august.2016$coef[[1]],
               mod.rd.late.allhours.september.2016$coef[[1]],
               mod.rd.late.allhours.julsep.2016$coef[[1]]
  ),
  latese = c(mod.rd.late.allhours.july.2016$se[[3]],
             mod.rd.late.allhours.august.2016$se[[3]],
             mod.rd.late.allhours.september.2016$se[[3]],
             mod.rd.late.allhours.julsep.2016$se[[3]]
  ),
  obs = c(sum(mod.rd.rf.allhours.july.2016$N_h),
          sum(mod.rd.rf.allhours.august.2016$N_h),
          sum(mod.rd.rf.allhours.september.2016$N_h),
          sum(mod.rd.rf.allhours.julsep.2016$N_h)
  ),
  bw = c(mod.rd.rf.allhours.july.2016$bws[1],
         mod.rd.rf.allhours.august.2016$bws[1],
         mod.rd.rf.allhours.september.2016$bws[1],
         mod.rd.rf.allhours.julsep.2016$bws[1]
  ),
  mean = c(mean.july.2016, mean.august.2016, mean.september.2016, mean.julsep.2016)
)

# Determine significance stars
table.rd.monthly.2016[, rfstars := '']
table.rd.monthly.2016[abs(rfcoef / rfse) > 1.645, rfstars := '*']
table.rd.monthly.2016[abs(rfcoef / rfse) > 1.96, rfstars := '**']
table.rd.monthly.2016[abs(rfcoef / rfse) > 2.576, rfstars := '***']
table.rd.monthly.2016[, latestars := '']
table.rd.monthly.2016[abs(latecoef / latese) > 1.645, latestars := '*']
table.rd.monthly.2016[abs(latecoef / latese) > 1.96, latestars := '**']
table.rd.monthly.2016[abs(latecoef / latese) > 2.576, latestars := '***']

# Format table
table.rd.monthly.2016 <- table.rd.monthly.2016[, .(
  month,
  rfstarcoef = paste0(sprintf('%3.1f', rfcoef), rfstars),
  rfse = paste0('(', sprintf('%2.2f', rfse), ')'),
  latestarcoef = paste0(sprintf('%3.1f', latecoef), latestars),
  latese = paste0('(', sprintf('%3.1f', latese), ')'),
  obs = formatC(obs, big.mark = ','),
  bw,
  mean 
)]

## Save regression output as .tex using Stargazer

sink(file.path(tablesdir, 'Stargazer_RD_Estimates_Month_2016.tex'))
stargazer(table.rd.monthly.2016, summary = F, rownames = F,
          title = "RD estimates of effects of irrigation violation notice by month in 2016", label = "tab:RD-Estimates-Month-2016",
          table.placement = "H", header = F, digit.separator = '',
          notes.align = "l", notes.append = F, 
          notes = "\\begin{minipage}[t]{15.7cm}{\\footnotesize $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01 \\hspace{0.25cm}
Notes: Each row presents nonparametric regression discontinuity estimates at the cutoff for automated violation notices.
The outcome variable is total weekly water consumption.
The month for each week is defined based on which month the first day of the week falls within.
All regressions use the
 \\href{https://sites.google.com/site/rdpackages/rdrobust}{``rdrobust''} software package developed and 
provided by \\cite{CalonicoCattaneoTitiunik_RDD}.
Heteroskedasticity-robust bias-corrected standard errors are estimated using the same package.
          }\\end{minipage}"
)
sink()

rm(table.rd.monthly.2016, mean.july.2016, mean.august.2016, mean.september.2016, mean.julsep.2016, wcon.rdd.2016,
   mod.rd.rf.allhours.july.2016, mod.rd.rf.allhours.august.2016, mod.rd.rf.allhours.september.2016, mod.rd.rf.allhours.julsep.2016,
   mod.rd.late.allhours.july.2016, mod.rd.late.allhours.august.2016, mod.rd.late.allhours.september.2016, mod.rd.late.allhours.julsep.2016)

#### (A2) RD ESTIMATES USING NONPARAMETRIC ESTIMATOR, USING HWR-TREATED HOUSEHOLDS ONLY ####

# Note: using a fixed bandwidth of 80 gallons

wcon.rdd.hwr <- wcon[year(weekdate) == 2015 & month(weekdate) %in% 7:10 & abs(runningv) <= 80 & arm == 'b1']
setkey(wcon.rdd.hwr, runningv)

## Panel [A]: Reduced-form estimates

# Column 1: first-stage
mod.rd.hwr.firststage <- rdrobust(wcon.rdd.hwr$autoviol, wcon.rdd.hwr$runningv, h = 80)

# Column 2: reduced-form for all hours of the week
mod.rd.hwr.rf.allhours <- rdrobust(wcon.rdd.hwr$wgal, wcon.rdd.hwr$runningv, h = 80)

# Column 3: reduced-form for hours of the week when irrigation is NOT allowed
mod.rd.hwr.rf.notallowed <- rdrobust(wcon.rdd.hwr$nonts96, wcon.rdd.hwr$runningv, h = 80)

# Column 4: reduced-form for hours of the week when irrigation is allowed
mod.rd.hwr.rf.allowed <- rdrobust(wcon.rdd.hwr$ts96, wcon.rdd.hwr$runningv, h = 80)

## Panel [B]: LATE estimates

# Column 2: LATE for all hours of the week
mod.rd.hwr.late.allhours <- rdrobust(wcon.rdd.hwr$wgal, wcon.rdd.hwr$runningv, fuzzy = wcon.rdd.hwr$autoviol, h = 80)

# Column 3: LATE for hours of the week when irrigation is NOT allowed
mod.rd.hwr.late.notallowed <- rdrobust(wcon.rdd.hwr$nonts96, wcon.rdd.hwr$runningv, fuzzy = wcon.rdd.hwr$autoviol, h = 80)

# Column 4: LATE for hours of the week when irrigation is allowed
mod.rd.hwr.late.allowed <- rdrobust(wcon.rdd.hwr$ts96, wcon.rdd.hwr$runningv, fuzzy = wcon.rdd.hwr$autoviol, h = 80)

## Sample means of outcome variables
mean.rdd.hwr.firststage <- sprintf('%0.2f', mean(wcon.rdd.hwr[abs(runningv) <= 80]$autoviol))
mean.rdd.hwr.allhours <- sprintf('%4.0f',mean(wcon.rdd.hwr[abs(runningv) <= 80]$wgal))
mean.rdd.hwr.notallowed <- sprintf('%4.0f',mean(wcon.rdd.hwr[abs(runningv) <= 80]$nonts96))
mean.rdd.hwr.allowed <- sprintf('%3.0f',mean(wcon.rdd.hwr[abs(runningv) <= 80]$ts96))

# Extract results from estimated models and combine into a table
dt.mod.rd.hwr <- data.table(
  outcome = c('First-stage', 'All hours', 'Non-irrig. hours', 'Irrig. hours.', 'All hours', 'Non-irrig. hours', 'Irrig. hours.'
  ),
  coef = c(mod.rd.hwr.firststage$coef[[1]],
           mod.rd.hwr.rf.allhours$coef[[1]],
           mod.rd.hwr.rf.notallowed$coef[[1]],
           mod.rd.hwr.rf.allowed$coef[[1]],
           mod.rd.hwr.late.allhours$coef[[1]],
           mod.rd.hwr.late.notallowed$coef[[1]],
           mod.rd.hwr.late.allowed$coef[[1]]
  ),
  se = c(mod.rd.hwr.firststage$se[[3]],
         mod.rd.hwr.rf.allhours$se[[3]],
         mod.rd.hwr.rf.notallowed$se[[3]],
         mod.rd.hwr.rf.allowed$se[[3]],
         mod.rd.hwr.late.allhours$se[[3]],
         mod.rd.hwr.late.notallowed$se[[3]],
         mod.rd.hwr.late.allowed$se[[3]]
  ),
  bw = c(mod.rd.hwr.firststage$bws[1],
         mod.rd.hwr.rf.allhours$bws[1],
         mod.rd.hwr.rf.notallowed$bws[1],
         mod.rd.hwr.rf.allowed$bws[1],
         mod.rd.hwr.late.allhours$bws[1],
         mod.rd.hwr.late.notallowed$bws[1],
         mod.rd.hwr.late.allowed$bws[1]
  ),
  obs = c(sum(mod.rd.hwr.firststage$N_h),
          sum(mod.rd.hwr.rf.allhours$N_h),
          sum(mod.rd.hwr.rf.notallowed$N_h),
          sum(mod.rd.hwr.rf.allowed$N_h),
          sum(mod.rd.hwr.late.allhours$N_h),
          sum(mod.rd.hwr.late.notallowed$N_h),
          sum(mod.rd.hwr.late.allowed$N_h)
  )
)

# Determine significance stars
dt.mod.rd.hwr[, stars := '']
dt.mod.rd.hwr[abs(coef / se) > 1.645, stars := '*']
dt.mod.rd.hwr[abs(coef / se) > 1.96, stars := '**']
dt.mod.rd.hwr[abs(coef / se) > 2.576, stars := '***']

# Format table
dt.mod.rd.hwr <- dt.mod.rd.hwr[, .(
  outcome,
  starcoef = paste0(sprintf('%1.4g', coef), stars),
  se = paste0('(', sprintf('%1.4g', se), ')'),
  bw,
  obs = formatC(obs, big.mark = ',')
)]

# Reshape into a results table to export using Stargazer
dt.rd.hwr.table <- data.table(
  col0 = c('', 'Discontinuity', '', 'Discontinuity', '', 'Sample mean', 'Bandwidth (gal)', 'Observations'),
  col1 = c(dt.mod.rd.hwr$outcome[1], dt.mod.rd.hwr$starcoef[1], dt.mod.rd.hwr$se[1], '', '', mean.rdd.hwr.firststage, dt.mod.rd.hwr$bw[1], dt.mod.rd.hwr$obs[1]),
  col2 = c(dt.mod.rd.hwr$outcome[2], dt.mod.rd.hwr$starcoef[2], dt.mod.rd.hwr$se[2], dt.mod.rd.hwr$starcoef[5], dt.mod.rd.hwr$se[5], mean.rdd.hwr.allhours, dt.mod.rd.hwr$bw[2], dt.mod.rd.hwr$obs[2]),
  col3 = c(dt.mod.rd.hwr$outcome[3], dt.mod.rd.hwr$starcoef[3], dt.mod.rd.hwr$se[3], dt.mod.rd.hwr$starcoef[6], dt.mod.rd.hwr$se[6], mean.rdd.hwr.notallowed, dt.mod.rd.hwr$bw[3], dt.mod.rd.hwr$obs[3]),
  col4 = c(dt.mod.rd.hwr$outcome[4], dt.mod.rd.hwr$starcoef[4], dt.mod.rd.hwr$se[4], dt.mod.rd.hwr$starcoef[7], dt.mod.rd.hwr$se[7], mean.rdd.hwr.allowed, dt.mod.rd.hwr$bw[4], dt.mod.rd.hwr$obs[4])
)

## Save regression output as .tex using Stargazer

sink(file.path(tablesdir, 'Stargazer_RD_Estimates_HWR.tex'))
stargazer(dt.rd.hwr.table, summary = F, rownames = F,
          title = "RD estimates of effects of irrigation violation notice for HWR-treated sample", label = "tab:RD-Estimates-HWR",
          table.placement = "H", header = F, digit.separator = '',
          notes.align = "l", notes.append = F, 
          notes = "\\begin{minipage}[t]{15.5cm}{\\footnotesize $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01 \\hspace{0.25cm}
Notes: Each cell presents a nonparametric regression discontinuity estimate at the cutoff for automated violation notices.
All regressions use the
 \\href{https://sites.google.com/site/rdpackages/rdrobust}{``rdrobust''} software package developed and 
provided by \\cite{CalonicoCattaneoTitiunik_RDD}.
Heteroskedasticity-robust bias-corrected standard errors are estimated using the same package.
Column (1) provides the estimated first-stage for automated violation notices.
These notices were sent to households during the first week of July 2015.
Columns (2) - (4) present estimates for household weekly water consumption during July through October 2015, 
the remainder of the legal and technical local summer water season following the violation notices. 
Panel [A] shows the reduced-form estimates and Panel [B] shows the estimated local average treatment effects.
Column (2) includes water consumption pooled across all hours of the week.
Column (3) includes consumption only during hours of the week when irrigation was not legally allowed.
Column (4) includes consumption only during hours irrigation was legally allowed: Tuesdays and Saturdays 
before 9:00 a.m. or after 6:00 p.m.
          }\\end{minipage}"
)
sink()

rm(dt.mod.rd.hwr, dt.rd.hwr.table, mod.rd.hwr.firststage, mod.rd.hwr.rf.allhours, mod.rd.hwr.rf.notallowed, mod.rd.hwr.rf.allowed, mod.rd.hwr.late.allhours,
   mod.rd.hwr.late.notallowed, mod.rd.hwr.late.allowed, mean.rdd.hwr.firststage, mean.rdd.hwr.allhours, mean.rdd.hwr.notallowed, mean.rdd.hwr.allowed, wcon.rdd.hwr)

#### (A3) RCT ESTIMATES BY DAY OF WEEK AND TIME BLOCK (00:00-09:00, 09:00-18:00, 18:00-00:00) ####

# Regressions: HWR coef. for each weekday by time block, for each of the two regimes
dow_list <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

timeblock.dtl <- lapply(1:length(dow_list), function(dow) {
  data.table(Weekday = rep(dow_list[[dow]], 3),
             `Time block` = c("00:00-09:00", "09:00-18:00", "18:00-00:00"),
             `Irrig.` = c("-", "-", "-"),
             `Late May - June` = c(lm(b9gal ~ arm, data = dcon.timeblock[regime == "1-LMJ" & wday(readdate) == dow])$coef[[2]],
                                   lm(a9b6gal ~ arm, data = dcon.timeblock[regime == "1-LMJ" & wday(readdate) == dow])$coef[[2]],
                                   lm(a6gal ~ arm, data = dcon.timeblock[regime == "1-LMJ" & wday(readdate) == dow])$coef[[2]]),
             `July - October` = c(lm(b9gal ~ arm, data = dcon.timeblock[regime == "2-JO" & wday(readdate) == dow])$coef[[2]],
                                  lm(a9b6gal ~ arm, data = dcon.timeblock[regime == "2-JO" & wday(readdate) == dow])$coef[[2]],
                                  lm(a6gal ~ arm, data = dcon.timeblock[regime == "2-JO" & wday(readdate) == dow])$coef[[2]])
  )
})

timeblock.tab <- rbindlist(timeblock.dtl)
timeblock.tab[, `Irrig.` := ifelse(Weekday %in% c("Tuesday", "Saturday") & `Time block` %in% c("00:00-09:00", "18:00-00:00"), "Yes", "No")]
timeblock.tab.totals <- data.table(Weekday = "Total", `Time block` = "", `Irrig.` = "",
                                   `Late May - June` = lm(wgal ~ hwr_treated, data = rct.panel[weekdate >= as.Date('2015-05-20') & weekdate <= as.Date('2015-06-30')])$coef[[2]],
                                   `July - October` =  lm(wgal ~ hwr_treated, data = rct.panel[weekdate > as.Date('2015-06-30') & weekdate <= as.Date('2015-10-31')])$coef[[2]])
timeblock.tab <- rbind(timeblock.tab, timeblock.tab.totals, fill = T)
timeblock.tab[, `Diff.` := `July - October` - `Late May - June`]
timeblock.tab[, `Late May - June` := sprintf('%2.2f', `Late May - June`)]
timeblock.tab[, `July - October` := sprintf('%2.2f', `July - October`)]
timeblock.tab[, `Diff.` := sprintf('%2.2f', `Diff.`)]

## Save table as .tex using Stargazer

sink(file.path(tablesdir, 'Stargazer_RCT_Weekday_Timeblock.tex'))
stargazer(timeblock.tab, summary = F, rownames = F,
          table.placement = "H", digits = 3, header = F,
          notes.align = "l", notes.append = F, notes.label = "\\textit{Notes:}",
          title = "Estimated effects of Home Water Reports by weekday and time block in 2015", label = "tab:RCT-Estimates-TimeBlock",
          notes = "\\begin{minipage}[t]{15.5cm}{\\footnotesize Notes:
Table~\\ref{tab:RCT-Estimates-TimeBlock} shows the point estimates of the average intent-to-treat effect of the randomized WaterSmart HWR
for weekly water consumption during the indicated weekday and time block, separately by policy regime calendar period during 2015.
          }\\end{minipage}"
)
sink()

rm(dow_list, timeblock.dtl, timeblock.tab, timeblock.tab.totals)

#### (A4) ROBUSTNESS: RCT ESTIMATES ACROSS DIFFERENT TIME PERIODS WITHIN POST-TREATMENT PERIOD, ADDING PANELS FOR WEATHER CONTROLS AND WEEK-OF-SAMPLE FIXED EFFECTS ####

# Merge in weather data to the weekly rct.panel data
weekdate.readdate <- unique(dcon[, .(weekdate, readdate)], by = NULL)
weekdate.readdate.weather <- merge(weekdate.readdate, weather, by = 'readdate')
weekdate.weather <- weekdate.readdate.weather[, .(tmax = max(TMAX), tmin = min(TMIN), prcp = sum(PRCP)), by = weekdate]
rct.panel.w <- merge(rct.panel, weekdate.weather, by = 'weekdate')
rm(weekdate.readdate, weekdate.readdate.weather, weekdate.weather)

## Regression models:

## Panel [A] Baseline (reproduce Table 5) 

# Column 1: pooled treatement period of late-May through October 2015 (replicate)
mod.rct.pooled <- felm(wgal ~ hwr_treated + lotsize + irrarea + sqft + yearbuilt + floors + bedrooms + bathrooms + lotsize_missing + sqft_missing + yearbuilt_missing
                       | 0 | 0 | weekdate + residence_id, data = rct.panel.w[weekdate >= as.Date('2015-05-20') & weekdate <= as.Date('2015-10-31')])

# Column 2: Late-May through June 2015
mod.rct.lmj <- felm(wgal ~ hwr_treated + lotsize + irrarea + sqft + yearbuilt + floors + bedrooms + bathrooms + lotsize_missing + sqft_missing + yearbuilt_missing
                    | 0 | 0 | weekdate + residence_id, data = rct.panel.w[weekdate >= as.Date('2015-05-20') & weekdate <= as.Date('2015-06-30')])

# Column 3: July through October 2015
mod.rct.jo <- felm(wgal ~ hwr_treated + lotsize + irrarea + sqft + yearbuilt + floors + bedrooms + bathrooms + lotsize_missing + sqft_missing + yearbuilt_missing
                   | 0 | 0 | weekdate + residence_id, data = rct.panel.w[weekdate >= as.Date('2015-07-01') & weekdate <= as.Date('2015-10-31')])

## Panel [B] add weather controls for temperature (weekly max, weekly min) and precipitation by week (weekly total)

# Column 1: pooled treatement period of late-May through October 2015 (replicate)
mod.rct.pooled.weather <- felm(wgal ~ hwr_treated + lotsize + irrarea + sqft + yearbuilt + floors + bedrooms + bathrooms + lotsize_missing + sqft_missing + yearbuilt_missing + tmax + tmin + prcp
                           | 0 | 0 | weekdate + residence_id, data = rct.panel.w[weekdate >= as.Date('2015-05-20') & weekdate <= as.Date('2015-10-31')])

# Column 2: Late-May through June 2015
mod.rct.lmj.weather <- felm(wgal ~ hwr_treated + lotsize + irrarea + sqft + yearbuilt + floors + bedrooms + bathrooms + lotsize_missing + sqft_missing + yearbuilt_missing + tmax + tmin + prcp
                        | 0 | 0 | weekdate + residence_id, data = rct.panel.w[weekdate >= as.Date('2015-05-20') & weekdate <= as.Date('2015-06-30')])

# Column 3: July through October 2015
mod.rct.jo.weather <- felm(wgal ~ hwr_treated + lotsize + irrarea + sqft + yearbuilt + floors + bedrooms + bathrooms + lotsize_missing + sqft_missing + yearbuilt_missing + tmax + tmin + prcp
                       | 0 | 0 | weekdate + residence_id, data = rct.panel.w[weekdate >= as.Date('2015-07-01') & weekdate <= as.Date('2015-10-31')])

## Panel [C] add week-of-sample fixed effects

# Column 1: pooled treatement period of late-May through October 2015 (replicate)
mod.rct.pooled.wfe <- felm(wgal ~ hwr_treated + lotsize + irrarea + sqft + yearbuilt + floors + bedrooms + bathrooms + lotsize_missing + sqft_missing + yearbuilt_missing
                       | weekdate | 0 | weekdate + residence_id, data = rct.panel.w[weekdate >= as.Date('2015-05-20') & weekdate <= as.Date('2015-10-31')])

# Column 2: Late-May through June 2015
mod.rct.lmj.wfe <- felm(wgal ~ hwr_treated + lotsize + irrarea + sqft + yearbuilt + floors + bedrooms + bathrooms + lotsize_missing + sqft_missing + yearbuilt_missing
                    | weekdate | 0 | weekdate + residence_id, data = rct.panel.w[weekdate >= as.Date('2015-05-20') & weekdate <= as.Date('2015-06-30')])

# Column 3: July through October 2015
mod.rct.jo.wfe <- felm(wgal ~ hwr_treated + lotsize + irrarea + sqft + yearbuilt + floors + bedrooms + bathrooms + lotsize_missing + sqft_missing + yearbuilt_missing
                   | weekdate | 0 | weekdate + residence_id, data = rct.panel.w[weekdate >= as.Date('2015-07-01') & weekdate <= as.Date('2015-10-31')])

## Statistics and meta data to include in table
num.households <- formatC(length(unique(rct.panel.w[weekdate >= as.Date('2015-05-20') & weekdate <= as.Date('2015-10-31'), .(residence_id)], by = NULL)$residence_id), big.mark = ',')
mean.rct.allhours.lmoct <- sprintf('%4.0f', mean(rct.panel.w[weekdate >= as.Date('2015-05-20') & weekdate <= as.Date('2015-10-31') & arm == 'b0']$wgal))
mean.rct.allhours.lmj <- sprintf('%4.0f', mean(rct.panel.w[weekdate >= as.Date('2015-05-20') & weekdate <= as.Date('2015-06-30') & arm == 'b0']$wgal))
mean.rct.allhours.jo <- sprintf('%4.0f', mean(rct.panel.w[weekdate >= as.Date('2015-07-01') & weekdate <= as.Date('2015-10-31') & arm == 'b0']$wgal))

## Save regression output as .tex using Stargazer: Panel [A]

sink(file.path(tablesdir, 'Stargazer_RCT_TimePeriods_WeatherControls_PanelA.tex'))
stargazer(mod.rct.pooled, mod.rct.lmj, mod.rct.jo,
          table.placement = "H", digits = 2, keep.stat = c("n"), dep.var.labels.include = F, header = F,
          notes.align = "l", notes.append = F, notes.label = "\\textit{Notes:}",
          title = "Estimated effects of randomized WaterSmart Home Water Reports by time period", label = "tab:RCT-TimePeriod-WeatherControls",
          dep.var.caption = "Weekly water consumption in 2015 (gallons)",
          keep = c("hwr_treated"), covariate.labels = c("I\\{HWR\\}"),
          add.lines = list(c("Time period", "Late May - October", "Late May - June", "July - October"),
                           c("Household controls", "Yes", "Yes", "Yes"),
                           c("Control group mean", mean.rct.allhours.lmoct, mean.rct.allhours.lmj, mean.rct.allhours.jo),
                           c("Num. of households", num.households, num.households, num.households)),
          notes = "\\begin{minipage}[t]{13cm}{\\footnotesize $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01 \\hspace{0.25cm}
Notes: see notes for Table~\\ref{tab:RCT-TimePeriod}. Panel [A] replicates the first three columns of Table~\\ref{tab:RCT-TimePeriod}.
          Panel [B] adds controls for the weekly total precipitation, maximum temperature, and minimum temperature.
          Panel [C] adds week-of-sample fixed effects. 
          }\\end{minipage}"
)
sink()

## Save regression output as .tex using Stargazer: Panel [B]

sink(file.path(tablesdir, 'Stargazer_RCT_TimePeriods_WeatherControls_PanelB.tex'))
stargazer(mod.rct.pooled.weather, mod.rct.lmj.weather, mod.rct.jo.weather,
          table.placement = "H", digits = 2, keep.stat = c("n"), dep.var.labels.include = F, header = F,
          notes.align = "l", notes.append = F, notes.label = "\\textit{Notes:}",
          title = "Estimated effects of randomized WaterSmart Home Water Reports by time period", label = "tab:RCT-TimePeriod-WeatherControls",
          dep.var.caption = "Weekly water consumption in 2015 (gallons)",
          keep = c("hwr_treated"), covariate.labels = c("I\\{HWR\\}"),
          add.lines = list(c("Time period", "Late May - October", "Late May - June", "July - October"),
                           c("Household controls", "Yes", "Yes", "Yes"),
                           c("Weather controls", "Yes", "Yes", "Yes"),
                           c("Control group mean", mean.rct.allhours.lmoct, mean.rct.allhours.lmj, mean.rct.allhours.jo),
                           c("Num. of households", num.households, num.households, num.households)),
          notes = "\\begin{minipage}[t]{13cm}{\\footnotesize $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01 \\hspace{0.25cm}
Notes: see notes for Panel [A]. 
          }\\end{minipage}"
)
sink()

## Save regression output as .tex using Stargazer: Panel [C]

sink(file.path(tablesdir, 'Stargazer_RCT_TimePeriods_WeatherControls_PanelC.tex'))
stargazer(mod.rct.pooled.wfe, mod.rct.lmj.wfe, mod.rct.jo.wfe,
          table.placement = "H", digits = 2, keep.stat = c("n"), dep.var.labels.include = F, header = F,
          notes.align = "l", notes.append = F, notes.label = "\\textit{Notes:}",
          title = "Estimated effects of randomized WaterSmart Home Water Reports by time period", label = "tab:RCT-TimePeriod-WeatherControls",
          dep.var.caption = "Weekly water consumption in 2015 (gallons)",
          keep = c("hwr_treated"), covariate.labels = c("I\\{HWR\\}"),
          add.lines = list(c("Time period", "Late May - October", "Late May - June", "July - October"),
                           c("Household controls", "Yes", "Yes", "Yes"),
                           c("Week-of-sample FE", "Yes", "Yes", "Yes"),
                           c("Control group mean", mean.rct.allhours.lmoct, mean.rct.allhours.lmj, mean.rct.allhours.jo),
                           c("Num. of households", num.households, num.households, num.households)),
          notes = "\\begin{minipage}[t]{13cm}{\\footnotesize $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01 \\hspace{0.25cm}
Notes: see notes for Panel [A]. 
          }\\end{minipage}"
)
sink()

rm(num.households, mean.rct.allhours.lmoct, mean.rct.allhours.lmj, mean.rct.allhours.jo, rct.panel.w,
   mod.rct.pooled, mod.rct.lmj, mod.rct.jo,
   mod.rct.pooled.weather, mod.rct.lmj.weather, mod.rct.jo.weather,
   mod.rct.pooled.wfe, mod.rct.lmj.wfe, mod.rct.jo.wfe)

## END
