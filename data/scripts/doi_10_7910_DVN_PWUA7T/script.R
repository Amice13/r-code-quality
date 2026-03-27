rm(list=ls(all=TRUE))
setwd("/Users/xsunde/Dropbox/Inflationprojekt/")
library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(stargazer)
library(ggpubr)
library(broom)
library(ggpubr)
library(rlang)
library(miceadds)
library(sandwich)
library(lmtest)
library(car)
library(modelsummary)
library(clubSandwich)
library(texreg)

# Load inflation data, 
uk_infl <- read_xlsx("uk_inflation.xlsx") %>% 
  mutate(date_year = year(date))

# Load Bank of England data
boe <- read_xlsx("bankofengland_mar25.xlsx", sheet="Dataset",
                 col_types="numeric")

boe <- boe %>%
  mutate(yyyyqq = as.character(yyyyqq))

uk_bankrate <- read_xlsx("bankrate.xlsx") %>%
  mutate(date = ymd(paste0(year, "-", month, "-", day)))

uk_bankrate_dates <- tibble(date = seq.Date(from=as.Date("1997-05-06"),
                                            to=as.Date("2025-03-01"),
                                            by=1))

uk_bankrate_dates <- left_join(uk_bankrate_dates, uk_bankrate, by="date")

bankrates_nonmissing <- na.locf(uk_bankrate_dates$bankrate)

uk_bankrate_dates <- uk_bankrate_dates %>%
  mutate(bankrate = bankrates_nonmissing) %>%
  dplyr::select(date, bankrate)

uk_bankrate_dates2 <- uk_bankrate_dates %>%
  mutate(date_3monthforward = date + days(90)) %>%
  left_join(., uk_bankrate_dates, by=c("date_3monthforward"="date")) %>%
  rename("bankrate_now" = bankrate.x,
         "bankrate_3month" = bankrate.y) %>%
  mutate(bankrate_diff = bankrate_3month - bankrate_now)

uk_bankrate_pastyear <- uk_bankrate_dates %>%
  mutate(date_1yearpast = date - years(1)) %>%
  left_join(., uk_bankrate_dates, by=c("date_1yearpast"="date")) %>%
  rename("bankrate_now" = bankrate.x,
         "bankrate_1yearago" = bankrate.y) %>%
  mutate(bankrate_diff1yearago = bankrate_now - bankrate_1yearago) %>% 
  dplyr::select(date, bankrate_1yearago, bankrate_diff1yearago)

uk_bankrate_dates2 <- left_join(uk_bankrate_dates2, uk_bankrate_pastyear, by="date")

uk_bankrates_cpi <- left_join(uk_bankrate_dates2, uk_infl, by="date") %>%
  mutate(year = year(date),
         month = month(date))

# q1: Price change last 12 months
# q2: Inflation expectation
# q4: Inflation target too high or too low
# q5: Interest rate change
# q6: Interest rate expectation
# q7: Interest rate preference - general
# q8: Interest rate preference - personal
# q10: Interest rates or prices
# q11: Which group of people sets interest rate
# q12: Which group sets interest rate

### INFLATION FORECAST DATA
forecasts <- read_xlsx("CPI Projections/projections_condensed.xlsx") %>% 
  mutate(year = as.character(year),
         quarter = case_match(month,
                              c("January", "February") ~ "01",
                                "May" ~ "02",
                                "August" ~ "03",
                                "November" ~ "04")) %>% 
  dplyr::select(year, quarter, forecast_combined)



slimdata <- boe %>%
  dplyr::select(yyyyqq, weight, sex, age, class, work, income, educ, tenure, sreg, weight,
                q1, q1b, q1c, q2, q2a_agg, q4, q5, q6,
                q7, q8, q9a, q9b, q10, q11, q12, q13, q14) %>% 
  mutate(work = case_match(work, 1 ~ 1, 2 ~ 0, TRUE ~ as.double(NA)))


slimdata <- slimdata %>%
  mutate(year = str_sub(yyyyqq, start=1, end= 4),
         quarter = str_sub(yyyyqq, start=5, end= 6),
         month = case_when(quarter=="01" ~ 2,
                           quarter=="02" ~ 5,
                           quarter=="03" ~ 8,
                           quarter=="04" ~ 11),
         date = ym(paste0(year, "-", month)))


# Add forecast,
slimdata <- slimdata %>% 
  left_join(., forecasts, by=c("year", "quarter"))

slimdata <- slimdata %>%
  mutate(past_price_dk = case_when(q1==9 ~ 1,
                                   q1<9 ~ 0,
                                   TRUE ~ as.double(NA)),
         past_price = case_when(q1==9 ~ as.double(NA),
                                TRUE ~ q1),
         past_price_num = case_match(q1c,
                                     1 ~ -0.5,
                                     2 ~ -1.5,
                                     3 ~ -2.5,
                                     4 ~ -3.5,
                                     5 ~ -4.5,
                                     6 ~ -5.5,
                                     7 ~ 0,
                                     8 ~ +0.5,
                                     9 ~ +1.5,
                                     10 ~ +2.5,
                                     11 ~ +3.5,
                                     12 ~ +4.5,
                                     13 ~ +5.5,
                                     14 ~ +6.5,
                                     15 ~ +7.5,
                                     16 ~ +8.5,
                                     17 ~ +9.5,
                                     18 ~ +10.5,
                                     19 ~ as.double(NA),
                                     20 ~ +10.5,
                                     21 ~ +11.5,
                                     22 ~ +12.5,
                                     23 ~ +13.5,
                                     24 ~ +14.5,
                                     25 ~ +15.5),
         past_price_num = case_when(is.na(past_price_num) & q1 == 1 ~ -0.5,
                                    is.na(past_price_num) & q1 == 2 ~ 0,
                                    is.na(past_price_num) & q1 == 3 ~ +0.5,
                                    is.na(past_price_num) & q1 == 4 ~ +1.5,
                                    is.na(past_price_num) & q1 == 5 ~ +2.5,
                                    is.na(past_price_num) & q1 == 6 ~ +3.5,
                                    is.na(past_price_num) & q1 == 7 ~ +4.5,
                                    is.na(past_price_num) & q1 == 8 ~ +5.5,
                                    !is.na(past_price_num) ~ past_price_num),
         past_price_short = case_when(q1 == 1 ~ -0.5,
                                      q1 == 2 ~ 0,
                                      q1 == 3 ~ +0.5,
                                      q1 == 4 ~ +1.5,
                                      q1 == 5 ~ +2.5,
                                      q1 == 6 ~ +3.5,
                                      q1 == 7 ~ +4.5,
                                      q1 == 8 ~ +5.5,
                                      TRUE ~ as.double(NA)),
         interest_preference = case_match(q7,
                                          1 ~ 1,
                                          2 ~ -1,
                                          3 ~ 0,
                                          4 ~ as.double(NA),
                                          5 ~ as.double(NA)),
         interest_preference_personal = case_match(q8,
                                          1 ~ 1,
                                          2 ~ -1,
                                          3 ~ 0,
                                          4 ~ as.double(NA),
                                          5 ~ as.double(NA)),
         target_preference = case_match(q4,
                                        1 ~ 1,
                                        2 ~ -1,
                                        3 ~ 0,
                                        TRUE ~ as.double(NA)),
         better_interest_vs_price = case_match(q10,
                                               1 ~ 1,
                                               2 ~ 0,
                                               TRUE ~ as.double(NA)),
         income = case_when(income==12 ~ as.double(NA),
                         TRUE ~ income),
         mortgage = case_match(tenure,
                               1 ~ 0,
                               2 ~ 1,
                               3 ~ 0,
                               4 ~ 0,
                               TRUE ~ as.double(NA)),
         ownedoutright = case_match(tenure,
                               1 ~ 1,
                               2 ~ 0,
                               3 ~ 0,
                               4 ~ 0,
                               TRUE ~ as.double(NA)),
         councilrent = case_match(tenure,
                                    1 ~ 0,
                                    2 ~ 0,
                                    3 ~ 1,
                                    4 ~ 0,
                                    TRUE ~ as.double(NA)),
         otherhouse = case_match(tenure,
                                    1 ~ 0,
                                    2 ~ 0,
                                    3 ~ 0,
                                    4 ~ 1,
                                    TRUE ~ as.double(NA)),
         know_howitworks_shortterm = case_when(q9a<=5 ~ 6-q9a,
                                     TRUE ~ as.double(NA)),
         know_howitworks = case_when(q9b<=5 ~ 6-q9b,
                                     TRUE ~ as.double(NA)),
         satisfaction = case_match(q14,
                                   1 ~ 2,
                                   2 ~ 1,
                                   3 ~ 0,
                                   4 ~ -1,
                                   5 ~ -2,
                                   6 ~ as.double(NA)),
         satisfaction_tri  = case_match(q14,
                                        1 ~ 1,
                                        2 ~ 1,
                                        3 ~ 0,
                                        4 ~ -1,
                                        5 ~ -1,
                                        6 ~ as.double(NA)),
         perception_rate = case_match(q5,
                                      c(1, 2) ~ 1,
                                      3 ~ 0,
                                      c(4, 5) ~ -1,
                                      6 ~ as.double(NA)),
         dum_understands = case_match(know_howitworks,
                                      c(4, 5) ~ 1,
                                      c(1, 2, 3) ~ 0))



# Future expectations
slimdata <- slimdata %>%
  mutate(q2_combined = case_when(date< as.Date("2008-08-01") ~ q2,
                                 date>= as.Date("2008-08-01") ~ q2a_agg),
         q2_combined = case_when(date< as.Date("2008-08-01") & q2_combined==9 ~ 99,
                                 date>= as.Date("2008-08-01") & q2_combined==14 ~ 99,
                                 TRUE ~ q2_combined),
         future_price_dk = case_when(q2_combined == 99 ~ 1,
                                     !is.na(q2_combined) ~ 0,
                                     TRUE ~ as.double(NA)),
         q2_combined = case_when(future_price_dk==1 ~ as.double(NA),
                             TRUE ~ q2_combined),
         future_price_num = case_match(q2_combined,
                                     1 ~ -0.5,
                                     2 ~ 0,
                                     3 ~ 0.5,
                                     4 ~ +1.5,
                                     5 ~ +2.5,
                                     6 ~ +3.5,
                                     7 ~ +4.5,
                                     8 ~ +5.5,
                                     9 ~ +6.5,
                                     10 ~ +7.5,
                                     11 ~ +8.5,
                                     12 ~ +9.5,
                                     13 ~ +10.5,
                                     15 ~ +10.5,
                                     16 ~ +11.5,
                                     17 ~ +12.5,
                                     18 ~ +13.5,
                                     19 ~ +14.5,
                                     20 ~ +15.5),
         future_price_short= case_match(q2, 
                                     1 ~ -0.5,
                                     2 ~ 0,
                                     3 ~ 0.5,
                                     4 ~ +1.5,
                                     5 ~ +2.5,
                                     6 ~ +3.5,
                                     7 ~ +4.5,
                                     8 ~ +5.5,
                                     TRUE ~ as.double(NA)),
         correct_forecast = case_when(q2==1 & forecast_combined<0 ~ 1,
                                      q2==2 & forecast_combined>=-0.5 & forecast_combined<=0.5 ~ 1,
                                      q2==3 & forecast_combined>0 & forecast_combined <=1 ~ 1,
                                      q2==4 & forecast_combined>=1 & forecast_combined <2 ~ 1,
                                      q2==5 & forecast_combined>=2 & forecast_combined <3 ~ 1,
                                      q2==6 & forecast_combined>=3 & forecast_combined <4 ~ 1,
                                      q2==7 & forecast_combined>=4 & forecast_combined <5 ~ 1,
                                      q2==8 & forecast_combined>=5 ~ 1,
                                      q2==9 ~ 0,
                                      !is.na(q2) ~ 0,
                                      TRUE ~ as.double(NA)),
         future_trend = future_price_num-past_price_num)


# Merge
slimdata_merge <- left_join(slimdata, uk_bankrates_cpi, by="date")

# Merge with future cpi
uk_infl_future <- uk_infl %>%
  mutate(date = as.Date(date-years(1))) %>%
  rename(cpi_future = cpi)

slimdata_merge <- left_join(slimdata_merge, uk_infl_future, by="date")


# Perc_correct
slimdata_merge <- slimdata_merge %>%
  mutate(correct = case_when(q1 == 1 & cpi < 0                 ~ 1,
                             q1 == 2 & cpi >= -0.5 & cpi <0.5  ~ 1,
                             q1 == 3 & cpi >  0 & cpi <1       ~ 1,
                             q1 == 4 & cpi >= 1 & cpi < 2      ~ 1,
                             q1 == 5 & cpi >= 2 & cpi < 3      ~ 1,
                             q1 == 6 & cpi >= 3 & cpi <4       ~ 1,
                             q1 == 7 & cpi >= 4 & cpi <5       ~ 1,
                             q1 == 8 & cpi >= 5                ~ 1,
                             q1 == 9                           ~ 0,
                             !is.na(q1)                        ~ 0,
                             TRUE                              ~ as.double(NA)),
         accuracy = abs(past_price_num-cpi),
         correct_future = case_when(future_price_num == -0.5 & cpi_future < 0                           ~ 1,
                                    future_price_num == 0 & cpi_future >= -0.5 & cpi_future <0.5     ~ 1,
                                    future_price_num == 0.5 & cpi_future >  0 &    cpi_future <1       ~ 1,
                                    future_price_num == 1.5 & cpi_future >= 1 &    cpi_future < 2      ~ 1,
                                    future_price_num == 2.5 & cpi_future >= 2 &    cpi_future < 3      ~ 1,
                                    future_price_num == 3.5 & cpi_future >= 3 &    cpi_future <4       ~ 1,
                                    future_price_num == 4.5 & cpi_future >= 4 &    cpi_future <5       ~ 1,
                                    future_price_num >= 5.5 & cpi_future >= 5                          ~ 1,
                                    future_price_dk == 1 | is.na(future_price_num) | is.na(cpi_future)  ~ as.double(NA),
                                    TRUE                              ~ 0),
         correct_rate = case_when(perception_rate == -1 & bankrate_diff1yearago <0 ~ 1,
                                  perception_rate == 0 & (bankrate_diff1yearago >= -0.25 & bankrate_diff1yearago<=0.25) ~ 1,
                                  perception_rate == 1 & bankrate_diff1yearago > 0 ~ 1,
                                  q5==6 ~ 0,
                                  !is.na(perception_rate) ~ 0),
         smart_expect = case_when(future_price_num<1.5 & interest_preference==-1 ~ 1,
                                  future_price_num>=1.5 & future_price_num<=2.5 & interest_preference==0 ~ 1,
                                  future_price_num>2.5 & interest_preference==1 ~ 1,
                                  !is.na(interest_preference) ~ 0,
                                  TRUE ~ as.double(NA)))

# Now determine whether people want the right action relative to both their perception and reality

slimdata_rates <- slimdata_merge %>%
  mutate(congruent = case_when(interest_preference==-1 & bankrate_diff<0 ~ 1,
                               interest_preference==0  & bankrate_diff==0 ~ 1,
                               interest_preference==1  & bankrate_diff>0 ~ 1,
                               interest_preference==-1 & bankrate_diff>=0 ~ 0,
                               interest_preference==0  & bankrate_diff!=0 ~ 0,
                               interest_preference==1  & bankrate_diff<=0 ~ 0,
                               TRUE ~ as.double(NA)),
         congruent_index = case_when(interest_preference==-1 & bankrate_diff<0 ~ 1,
                                     interest_preference==-1 & bankrate_diff== 0 ~ 0,
                                     interest_preference==-1 & bankrate_diff> 0 ~ -0,
                                     interest_preference== 0 & bankrate_diff<0 ~ 0,
                                     interest_preference== 0 & bankrate_diff== 0 ~ 1,
                                     interest_preference== 0 & bankrate_diff> 0 ~ 0,
                                     interest_preference== 1 & bankrate_diff<0 ~ -1,
                                     interest_preference== 1 & bankrate_diff== 0 ~ 0,
                                     interest_preference==-1 & bankrate_diff>0 ~ 1,
                                     TRUE ~ as.double(NA)),
         congruent_personal = case_when(interest_preference_personal==-1 & bankrate_diff<0 ~ 1,
                                        interest_preference_personal==0 & bankrate_diff==0 ~ 1,
                                        interest_preference_personal==1 & bankrate_diff>0 ~ 1,
                                        interest_preference_personal==-1 & bankrate_diff>=0 ~ 0,
                                        interest_preference_personal==0 & bankrate_diff!=0 ~ 0,
                                        interest_preference_personal==1 & bankrate_diff<=0 ~ 0,
                                        TRUE ~ as.double(NA)),
         congruent_raise = case_when(interest_preference==1 & bankrate_diff>0 ~ 1,
                                    !is.na(interest_preference) & !is.na(bankrate_diff) ~ 0,
                                    TRUE ~ as.double(NA)),
         congruent_lowering = case_when(interest_preference==-1 & bankrate_diff<0 ~ 1,
                                    !is.na(interest_preference) & !is.na(bankrate_diff) ~ 0,
                                    TRUE ~ as.double(NA)),
         congruent_statusquo = case_when(interest_preference==0 & bankrate_diff==0 ~ 1,
                                        !is.na(interest_preference) & !is.na(bankrate_diff) ~ 0,
                                        TRUE ~ as.double(NA)),
         congruent_personal_raise = case_when(interest_preference_personal==1 & bankrate_diff>0 ~ 1,
                                     !is.na(interest_preference_personal) & !is.na(bankrate_diff) ~ 0,
                                     TRUE ~ as.double(NA)),
         congruent_personal_lowering = case_when(interest_preference_personal==-1 & bankrate_diff<0 ~ 1,
                                        !is.na(interest_preference_personal) & !is.na(bankrate_diff) ~ 0,
                                        TRUE ~ as.double(NA)),
         congruent_personal_statusquo = case_when(interest_preference_personal==0 & bankrate_diff==0 ~ 1,
                                         !is.na(interest_preference_personal) & !is.na(bankrate_diff) ~ 0,
                                         TRUE ~ as.double(NA)))





### Calculating income
midpoints <- slimdata_rates %>%
  filter(income!=12) %>% 
  group_by(date, income) %>%
  summarize(n = n(), .groups = 'drop') %>%
  group_by(date) %>% 
  mutate(proportion = n / sum(n)) %>%
  arrange(date, income) %>%
  group_by(date) %>%
  mutate(cumulative_prop = cumsum(proportion),
         prev_cumulative_prop = lag(cumulative_prop, default = 0),
         income_midpoint = (cumulative_prop + prev_cumulative_prop) / 2) %>%
  ungroup() %>%
  select(date, income, income_midpoint)

slimdata_percentiles <- left_join(slimdata_rates, midpoints, by=c("date", "income")) %>% 
  rename("income_percentile_mid" = income_midpoint)



##########################################################
# Timetrends data frame
##########################################################
# Create cutoffs
cutoff_hi <- 0.5000001
cutoff_lo <- 0.5

# Create timetrend data frame
timetrends <- slimdata_percentiles %>%
  dplyr::mutate(date = as_date(date),
         interest_preference_combined = (interest_preference+interest_preference_personal)/2,
         interest_raise = case_match(interest_preference,
                                     1 ~ 1,
                                     c(-1, 0) ~ 0,
                                     TRUE ~ as.double(NA)),
         interest_statusquo = case_match(interest_preference,
                              0 ~ 1,
                              c(-1, 1) ~ 0,
                              TRUE ~ as.double(NA)),
          interest_lower = case_match(interest_preference,
                                  -1 ~ 1,
                                  c(0, 1) ~ 0,
                                  TRUE ~ as.double(NA)),
         interest_nodifference = case_match(q7,
                                         4 ~ 1,
                                         c(1, 2, 3, 5) ~ 0,
                                         TRUE ~ as.double(NA)),
         interest_dk = case_match(q7,
                                            5 ~ 1,
                                            c(1, 2, 3, 4) ~ 0,
                                            TRUE ~ as.double(NA)),
         interest_personal_raise = case_match(interest_preference_personal,
                                     1 ~ 1,
                                     c(-1, 0) ~ 0,
                                     TRUE ~ as.double(NA)),
         interest_personal_statusquo = case_match(interest_preference_personal,
                                         0 ~ 1,
                                         c(-1, 1) ~ 0,
                                         TRUE ~ as.double(NA)),
         interest_personal_lower = case_match(interest_preference_personal,
                                     -1 ~ 1,
                                     c(0, 1) ~ 0,
                                     TRUE ~ as.double(NA)))
  
  timetrends <- timetrends %>% 
    mutate(across(
      c(interest_preference, interest_preference_personal,
        interest_raise, interest_statusquo, interest_lower, interest_nodifference, interest_dk,
        interest_personal_raise, interest_personal_statusquo, interest_personal_lower,
        future_price_num, future_trend, congruent, mortgage, ownedoutright, councilrent, otherhouse, work, target_preference),
      list(
        all = ~ .,
        work = ~ if_else(work == 1, ., as.double(NA)),
        nowork = ~ if_else(work == 0, ., as.double(NA)),
        workmortgage = ~ if_else(work == 1 & mortgage==1, ., as.double(NA)),
        noworkmortgage = ~ if_else(work == 0 & mortgage==1, ., as.double(NA)),
        worknomortgage = ~ if_else(work == 1 & mortgage==0, ., as.double(NA)),
        noworknomortgage = ~ if_else(work == 0 & mortgage==0, ., as.double(NA)),
        mortgage = ~ if_else(mortgage == 1, ., as.double(NA)),
        nomortgage = ~ if_else(mortgage == 0, ., as.double(NA)),
        ownoutright = ~ if_else(ownedoutright == 1, ., as.double(NA)),
        rent = ~ if_else(councilrent == 1, ., as.double(NA)),
        otherhouse = ~ if_else(otherhouse == 1, ., as.double(NA)),
        understands = ~ if_else(dum_understands == 1, ., as.double(NA)),
        nounderstands = ~ if_else(dum_understands == 0, ., as.double(NA)),
        bottom = ~ if_else(income_percentile_mid < cutoff_lo, ., as.double(NA)),
        top = ~ if_else(income_percentile_mid >= cutoff_hi, ., as.double(NA)),
        bottom25 = ~ if_else(income_percentile_mid < 0.25, ., as.double(NA)),
        mid50 = ~ if_else(income_percentile_mid >= 0.25 & income_percentile_mid < 0.75, ., as.double(NA)),
        top25 = ~ if_else(income_percentile_mid >= 0.75, ., as.double(NA)),
        rightrate = ~ if_else(correct_rate == 1, ., as.double(NA)),
        wrongrate = ~ if_else(correct_rate == 0, ., as.double(NA)),
        wrongforecast = ~ if_else(correct_forecast == 0, ., as.double(NA)),
        rightforecast = ~ if_else(correct_forecast == 1, ., as.double(NA)),
        wrongcpi = ~ if_else(correct == 0, ., as.double(NA)),
        rightcpi = ~ if_else(correct == 1, ., as.double(NA)),
        educ1 = ~ if_else(educ == 1, ., as.double(NA)),
        educ2 = ~ if_else(educ == 2, ., as.double(NA)),
        educ3 = ~ if_else(educ == 3, ., as.double(NA))
      ),
      .names = "{.col}_{.fn}"
    ))
  
  
  
timetrends <- timetrends %>% 
  group_by(date) %>%
  rename("past_price_mean" = past_price_num,
         "past_price_mean_short" = past_price_short,
         "preference_mean" = interest_preference,
         "preference_mean_personal" = interest_preference_personal,
         "preference_mean_combined" = interest_preference_combined,
         "future_price_mean" = future_price_num) %>% 
  dplyr::summarize(across(.cols = c(cpi,                    # Unweighted variablesd
                             cpi_future, 
                             work,
                             bankrate_now,
                             bankrate_3month,
                             bankrate_diff,
                             bankrate_diff1yearago,
                             forecast_combined), ~ mean(.x, na.rm=TRUE)),
            across(.cols=c(past_price_mean,          # Weighted variables - general
                           past_price_mean_short,
                           preference_mean,
                           preference_mean_personal,
                           preference_mean_combined,
                           correct,
                           correct_rate,
                           correct_forecast,
                           congruent,
                           income_percentile_mid,
                           mortgage,
                           starts_with("interest_"),
                           starts_with("congruent_"),
                           starts_with("future_price_num_"),
                           starts_with("future_trend"),
                           starts_with("mortgage"),
                           starts_with("ownedoutright"),
                           starts_with("councilrent"),
                           starts_with("otherhouse"),
                           starts_with("work"),
                           starts_with("target_preference"),), ~ Hmisc::wtd.mean(.x, weights=weight, na.rm=TRUE)), # Weighting
            
            bankrate_diff_3month_rounded = case_when(bankrate_diff<0 ~ -1,
                                              bankrate_diff==0 ~ 0,
                                              bankrate_diff>0 ~ 1,
                                              TRUE ~ as.double(NA)),
            bankrate_diff_3month_raises = case_when(bankrate_diff<0 ~ 0,
                                                     bankrate_diff==0 ~ 0,
                                                     bankrate_diff>0 ~ 1,
                                                     TRUE ~ as.double(NA)),
            bankrate_diff_3month_sq = case_when(bankrate_diff<0 ~ 0,
                                                    bankrate_diff==0 ~ 1,
                                                    bankrate_diff>0 ~ 0,
                                                    TRUE ~ as.double(NA)),
            bankrate_diff_3month_lowerings = case_when(bankrate_diff<0 ~ 1,
                                                    bankrate_diff==0 ~ 0,
                                                    bankrate_diff>0 ~ 0,
                                                    TRUE ~ as.double(NA))) %>%
  ungroup()

##########################################################
# Descriptives
##########################################################


#### FIGURE 2
### Preference for rate increases bargraph
bargraph_data <- slimdata_percentiles %>% 
  filter(!is.na(income_percentile_mid), !is.na(interest_preference)) %>% 
  mutate(rich = case_when(income_percentile_mid>0.5 ~ 1,
                          income_percentile_mid<=0.5 ~ 0,
                          TRUE ~ as.double(NA)),
         preference_raise = case_match(interest_preference,
                                       1 ~ 1,
                                       c(-1, 0) ~ 0,
                                       TRUE ~ as.double(NA)),
         preference_sq = case_match(interest_preference,
                                       0 ~ 1,
                                       c(-1, 1) ~ 0,
                                    TRUE ~ as.double(NA)),
         preference_lower = case_match(interest_preference,
                                       -1 ~ 1,
                                       c(1, 0) ~ 0,
                                       TRUE ~ as.double(NA))) %>% 
  group_by(rich) %>% 
  summarize(across(c(preference_raise, preference_sq, preference_lower), ~ weighted.mean(.x, na.rm=TRUE, w=weight)),
            n = n()) %>% 
  pivot_longer(cols=starts_with("preference"), names_to="action")

# N
52008+62874

max(slimdata_percentiles$date)

bargraph <- bargraph_data %>% 
  mutate(
    rich = case_match(rich,
                      0 ~ "Below median income",
                      1 ~ "Above median income") |>
      factor(levels = c("Below median income",    # first (left)
                        "Above median income")),  # second (right)
    action_order = case_match(action,
                              "preference_lower" ~ -1,
                              "preference_sq"    ~  0,
                              "preference_raise" ~  1)
  ) %>% 
  ggplot(aes(x = action_order,
             y = value * 100,
             fill = rich)) +              # no need for as.factor() now
  geom_col(position = position_dodge(width = .9)) +
  geom_text(aes(label = paste0(round(value * 100, 1), "%"),
                y     = value * 100 + 1),
            position = position_dodge(width = .9),
            vjust = 0) +
  theme_minimal() +
  scale_x_continuous(breaks = c(-1, 0, 1),
                     labels = c("Lower\nrate",
                                "Keep\nstatus quo",
                                "Raise\nrate")) +
  labs(x = NULL,
       y = "Percent supporting action",
       fill = NULL) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "top") +
  scale_fill_manual(values = c("#ff5555", "#000077"))

png("Output/bargraph_preference.png", width=5, height=4, units="in", res=300)
bargraph
dev.off()




slimdata_percentiles %>% 
  filter(!is.na(income_percentile_mid), !is.na(interest_preference)) %>% 
  group_by(above = income_percentile_mid>0.5, interest_preference) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(above) %>% 
  mutate(total = sum(n),
         perc = n/total)

m1 <- lm(interest_preference==-1 ~ income_percentile_mid, weights=weight, data=slimdata_percentiles)
m2 <- lm(interest_preference==0  ~ income_percentile_mid, weights=weight, data=slimdata_percentiles)
m3 <- lm(interest_preference==1  ~ income_percentile_mid, weights=weight, data=slimdata_percentiles)

m4 <- lm(interest_preference_personal==-1 ~ income_percentile_mid, weights=weight, data=slimdata_percentiles)
m5 <- lm(interest_preference_personal==0  ~ income_percentile_mid, weights=weight, data=slimdata_percentiles)
m6 <- lm(interest_preference_personal==1  ~ income_percentile_mid, weights=weight, data=slimdata_percentiles)

stargazer(m1, m2, m3, m4, m5, m6, type="text", omit.stat=c("SER", "F"))

stargazer(m1, m2, m3, m4, m5, m6,
          out = "Output/preferences_income.tex",
          type="latex", 
          omit=c("date"),
          keep.stat = c("n", "rsq", "adj.rsq"),
          single.row=FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          column.sep.width="1pt",
          ord.intercepts = FALSE,
          label = "tab_preferences",
          dep.var.labels = c("Lower", "Status quo", "Raise", "Personal - Lower", "Personal - Status quo", "Personal - Raise"),
          title = "Regression analysis: Preferences")

############################
# Figure 1: Bank rate graph
############################

graph_function <- function(yvar, titletext){
  
  timetrends %>%
    ggplot(aes(x=date, y=get(yvar))) +
    geom_line(col="black", linetype="solid") +
    theme_minimal() +
    labs(x=element_blank(), y=element_blank(), title=titletext) +
    lims(y = c(0, 10)) +
    geom_vline(xintercept=as.Date("2008-09-15"), linetype="dashed", col="red") +
    geom_vline(xintercept=as.Date("2020-03-23"), linetype="dashed", col="blue") +
    theme(plot.title = element_text(hjust = 0.5, size=8)) +
    scale_x_date(
      breaks = seq(as.Date("2000-01-01"), as.Date("2025-01-01"), by = "5 years"),
      labels = scales::date_format("%Y"),
      limits = c(as.Date("2000-01-01"), as.Date("2025-03-01"))
    )
}

graph_rate <- graph_function(yvar = "bankrate_now", title = "Bank Rate (%)")

graph_rate <- uk_bankrate_dates %>%
  ggplot(aes(x=date, y=bankrate)) +
  geom_step(col="black", linetype="solid", direction="vh") +
  theme_minimal() +
  labs(x=element_blank(), y=element_blank(), title="Bank Rate (%)") +
  lims(y = c(0, 10)) +
  geom_vline(xintercept=as.Date("2008-09-15"), linetype="dashed", col="red") +
  geom_vline(xintercept=as.Date("2020-03-23"), linetype="dashed", col="blue") +
  theme(plot.title = element_text(hjust = 0.5, size=8)) +
  annotate("text", x=as.Date("2008-08-01"), y=7.5, label="Lehman brothers\ncollapse", hjust=1,
           size=2.5) +
  annotate("text", x=as.Date("2020-02-01"), y=7.5, label="First UK Covid\n lockdown", hjust=1,
           size=2.5) +
  scale_x_date(
    breaks = seq(as.Date("2000-01-01"), as.Date("2025-01-01"), by = "5 years"),
    labels = scales::date_format("%Y"),
    limits = c(as.Date("2000-01-01"), as.Date("2025-03-01"))
  )

help(scale_x_date)

graph_inflation <- graph_function("cpi", "Inflation (%)") + geom_hline(yintercept=2, linetype="dashed", col="purple")

png("Output/rate_inflation.png", width=6, height=4, units="in", res=300)
ggarrange(graph_rate, graph_inflation, nrow=2, ncol=1, align = "hv")
dev.off()

##############################
# Figure 2: Income histogram
##############################

hist_income <- slimdata_percentiles %>% 
  ggplot(aes(x=income_percentile_mid)) +
  geom_histogram(fill="#cccccc", col="black", binwidth=0.025, boundary=0) +
  theme_minimal() +
  labs(x="Place in income distribution", y=c("Frequency"))

png("Output/appendix_income_histogram.png", width=6, height=4, units="in", res=300)
hist_income
dev.off()


####################################
# Properly informed
####################################

m1 <- lm(smart_expect ~ income_percentile_mid + as.factor(date), data=slimdata_percentiles, weights=weight)
m2 <- lm(correct_forecast ~ income_percentile_mid + as.factor(date), data=filter(slimdata_percentiles, !is.na(smart_expect)), weights=weight)
m3 <- lm(eval(correct_forecast*smart_expect) ~ income_percentile_mid + as.factor(date), data=slimdata_percentiles, weights=weight)
stargazer(m1, m2, m3, type="text", omit.stat=c("SER", "F"), omit=c("date"))


m1 <- lm(congruent ~ smart_expect, data=slimdata_percentiles, weights=weight)
m2 <- lm(congruent ~ correct_forecast, data=filter(slimdata_percentiles, !is.na(smart_expect)), weights=weight)
m3 <- lm(congruent ~ smart_expect * correct_forecast, data=slimdata_percentiles, weights=weight)

stargazer(m1, m2, m3, type="text", omit.stat=c("SER", "F"), omit=c("date"))

slimdata_percentiles %>% 
  #filter(income_percentile_mid>0.5) %>% 
  filter(!is.na(smart_expect), !is.na(correct_forecast), !is.na(congruent)) %>% 
  group_by(smart_expect, correct_forecast) %>% 
  summarize(congruent = mean(congruent, na.rm=TRUE),
            n = n(), .groups="drop") %>% 
  mutate(perc = n/sum(n))

m0 <- lm(congruent ~ income_percentile_mid + as.factor(date), data=slimdata_percentiles, weights=weight)
m1 <- lm(congruent ~ smart_expect + as.factor(date), data=slimdata_percentiles, weights=weight)
m2 <- lm(congruent ~ correct_forecast + as.factor(date), data=filter(slimdata_percentiles, !is.na(smart_expect)), weights=weight)
m3 <- lm(congruent ~ smart_expect * correct_forecast + as.factor(date), data=slimdata_percentiles, weights=weight)
m4 <- lm(congruent ~ income_percentile_mid + smart_expect * correct_forecast + as.factor(date), data=slimdata_percentiles, weights=weight)
stargazer(m0, m1, m2, m3, m4, type="text", omit.stat=c("SER", "F"), omit=c("date"))



##########################################################
# ANALYSIS - STACKED DATA
##########################################################

# Helper function to process each subset of columns
process_stack <- function(df, cols, value_name) {
  df %>%
    dplyr::select(date, all_of(cols)) %>%
    pivot_longer(cols = starts_with("interest"), names_to = "variable") %>%
    mutate(type = case_when(
      str_detect(variable, "raise") ~ "raise",
      str_detect(variable, "statusquo") ~ "sq",
      str_detect(variable, "lower") ~ "lower"
    )) %>%
    rename(!!value_name := value) %>%
    dplyr::select(-variable)
}

pastenames <- c("interest_raise_", "interest_statusquo_", "interest_lower_")
pastenames_personal <- c("interest_personal_raise_", "interest_personal_statusquo_", "interest_personal_lower_")
# Define the columns for each subset
columns_list <- list(
  "all"             = paste0(pastenames, "all"),
  "top"             = paste0(pastenames, "top"),
  "bottom"          = paste0(pastenames, "bottom"),
  "bottom25"          = paste0(pastenames, "bottom25"),
  "mid50"          = paste0(pastenames, "mid50"),
  "top25"          = paste0(pastenames, "top25"),
  "wrongrate"       = paste0(pastenames, "wrongrate"),
  "rightrate"       = paste0(pastenames, "rightrate"),
  "nomortgage"      = paste0(pastenames, "nomortgage"),
  "mortgage"        = paste0(pastenames, "mortgage"),
  "rent"            = paste0(pastenames, "rent"),
  "otherhouse"      = paste0(pastenames, "otherhouse"),
  "ownoutright"     = paste0(pastenames, "ownoutright"),
  "understands"     = paste0(pastenames, "understands"),
  "nounderstands"   = paste0(pastenames, "nounderstands"),
  "wrongforecast"   = paste0(pastenames, "wrongforecast"),
  "rightforecast"   = paste0(pastenames, "rightforecast"),
  "wrongcpi"        = paste0(pastenames, "wrongcpi"),
  "rightcpi"        = paste0(pastenames, "rightcpi"),
  "educ1"           = paste0(pastenames, "educ1"),
  "educ2"           = paste0(pastenames, "educ2"),
  "educ3"           = paste0(pastenames, "educ3"),
  "personal_all"    = paste0(pastenames_personal, "all"),
  "personal_top"    = paste0(pastenames_personal, "top"),
  "personal_bottom" = paste0(pastenames_personal, "bottom"),
  "personal_bottom25" = paste0(pastenames_personal, "bottom25"),
  "personal_mid50" = paste0(pastenames_personal, "mid50"),
  "personal_top25" = paste0(pastenames_personal, "top25")
)

# Apply the function to all subsets and store in a list
stack_list <- lapply(names(columns_list), function(name) {
  process_stack(timetrends, columns_list[[name]], paste0("support_", name))
})

# Reduce and merge all stacks together by `date` and `type`
stack <- Reduce(function(x, y) left_join(x, y, by = c("date", "type")), stack_list)

# Add final 'happened' column
stack <- stack %>%
  left_join(., select(timetrends, c(date, starts_with("bankrate_"))), by="date") %>% 
  mutate(happened = case_when(
    type == "raise" & bankrate_diff_3month_rounded == 1 ~ 1,
    type == "sq" & bankrate_diff_3month_rounded == 0 ~ 1,
    type == "lower" & bankrate_diff_3month_rounded == -1 ~ 1,
    TRUE ~ 0
  ))

##############################
# Figure 4: Implementation line graph
##############################

graph_implementation_stack <- stack %>% 
  mutate(support_all = floor(support_all*20)*5+2.5) %>% 
  group_by(support_all) %>% 
  summarize(support_all = mean(support_all, na.rm=TRUE),
            happened = mean(happened, na.rm=TRUE),
            n = n()) %>%
  ggplot(aes(x=support_all, y=happened*100)) +
  geom_line() +
  geom_point() +
  lims(y=c(0, 100)) +
  theme_minimal() +
  labs(x="Percent believing that policy\nwould be best for economy", y="Policy outcome implemented (%)", title=element_blank()) +
  annotate("curve", curvature=-0.5, x = 40, y=77, xend=47.5, yend=63, arrow=arrow(length=unit(0.2, units="cm")),
           col="#333333") +
  annotate("text", x=39, y=77, hjust=1, vjust=0.5, label="Outcomes (raise, keep or\nlower interest rate)\nsupported by 45-50% were\nimplemented 60% of the time", size=3) +
  theme(aspect.ratio=1, panel.grid.minor.x = element_blank(), plot.title = element_text(hjust=0.5),
        axis.title = element_text(size=8), panel.grid = element_blank(),
        panel.border=element_rect(fill=NA), axis.ticks = element_line()) +
  scale_x_continuous(breaks=seq(from=2.5, to=57.5, by=5),
                     labels=c("0-\n5", "5-\n10", "10-\n15", "15-\n20", "20-\n25", "25-\n30", "30-\n35",
                              "35-\n40", "40-\n45", "45-\n50", "50-\n55", "55-\n60"))

png("Output/stack_implementation_national.png", width=4, height=4, units="in", res=300)
graph_implementation_stack
dev.off()

##############################
# Appendix: Implementation line graph, best for R personally
##############################

graph_implementation_stack_personal <- stack %>% 
  mutate(support_all = floor(support_personal_all*20)*5+2.5) %>% 
  group_by(support_all) %>% 
  summarize(support_all = mean(support_all, na.rm=TRUE),
            happened = mean(happened, na.rm=TRUE),
            n = n()) %>% 
  ggplot(aes(x=support_all, y=happened*100)) +
  geom_line() +
  geom_point() +
  lims(y=c(0, 100)) +
  theme_minimal() +
  labs(x="Percent believing that policy\nwould be best for R personally", y="") +
  theme(aspect.ratio=1, panel.grid.minor.x = element_blank(), plot.title = element_text(hjust=0.5)) +
  scale_x_continuous(breaks=seq(from=2.5, to=62.5, by=5),
                     labels=c("0-\n5", "5-\n10", "10-\n15", "15-\n20", "20-\n25", "25-\n30", "30-\n35",
                              "35-\n40", "40-\n45", "45-\n50", "50-\n55", "55-\n60", "60-\n65"))

png("Output/stack_implementation_personal.png", width=4, height=4, units="in", res=300)
graph_implementation_stack_personal
dev.off()

##############################
# Appendix: Implementation line graph, divided by type of outcome
##############################

graph_implementation_stack_facets <- stack %>% 
  mutate(support_all = floor(support_all*20)*5+2.5) %>%
  mutate(type = factor(type, levels=c("lower", "sq", "raise"), labels = c("Lower", "Keep", "Raise"))) %>% 
  group_by(type, support_all) %>% 
  summarize(support_all = mean(support_all, na.rm=TRUE),
            happened = mean(happened, na.rm=TRUE),
            n = n()) %>% 
  ggplot(aes(x=support_all, y=happened*100)) +
  geom_line() +
  geom_point() +
  lims(y=c(0, 100)) +
  theme_minimal() +
  facet_wrap(~type) +
  labs(x="Percent believing that policy\nwould be best for economy", y="Policy outcome implemented (%)") +
  theme(aspect.ratio=1, panel.grid.minor.x = element_blank(), plot.title = element_text(hjust=0.5)) +
  scale_x_continuous(breaks=seq(from=2.5, to=57.5, by=5),
                     labels=c("0-\n5", "5-\n10", "10-\n15", "15-\n20", "20-\n25", "25-\n30", "30-\n35",
                              "35-\n40", "40-\n45", "45-\n50", "50-\n55", "55-\n60"))

png("Output/appendix_stack_implementation_facets.png", width=7, height=3.5, units="in", res=300)
graph_implementation_stack_facets
dev.off()

##############################
# Appendix: Implementation line graph, divided by type of outcome, best for R personally
##############################

graph_implementation_stack_personal_facets <- stack %>% 
  mutate(support_all = floor(support_personal_all*20)*5+2.5) %>%
  mutate(type = factor(type, levels=c("lower", "sq", "raise"), labels = c("Lower", "Keep", "Raise"))) %>% 
  group_by(type, support_all) %>% 
  summarize(support_all = mean(support_all, na.rm=TRUE),
            happened = mean(happened, na.rm=TRUE),
            n = n()) %>% 
  ggplot(aes(x=support_all, y=happened*100)) +
  geom_line() +
  geom_point() +
  lims(y=c(0, 100)) +
  theme_minimal() +
  facet_wrap(~type) +
  labs(x="Percent believing that policy\nwould be best for R personally", y="Policy outcome implemented (%)") +
  theme(aspect.ratio=1, panel.grid.minor.x = element_blank(), plot.title = element_text(hjust=0.5)) +
  scale_x_continuous(breaks=seq(from=2.5, to=57.5, by=5),
                     labels=c("0-\n5", "5-\n10", "10-\n15", "15-\n20", "20-\n25", "25-\n30", "30-\n35",
                              "35-\n40", "40-\n45", "45-\n50", "50-\n55", "55-\n60"))
png("Output/stack_implementation_facets_personal.png", width=7, height=3.5, units="in", res=300)
graph_implementation_stack_personal_facets
dev.off()




########################################################################  

##############################
# Table 1: Regression analysis on stacked data
##############################
# Run the models
m1 <- lm(happened ~ support_bottom, data = stack)
m2 <- lm(happened ~ support_top, data = stack)
m3 <- lm(happened ~ support_bottom + support_top, data = stack)

# Clustered VCVs
m1_vcov <- vcovCL(m1, cluster = ~ stack$date)
m2_vcov <- vcovCL(m2, cluster = ~ stack$date)
m3_vcov <- vcovCL(m3, cluster = ~ stack$date)

# Clustered SEs
se_list <- list(
  sqrt(diag(m1_vcov)),
  sqrt(diag(m2_vcov)),
  sqrt(diag(m3_vcov))
)

full_tex <- capture.output(
  stargazer(m1, m2, m3,
            se = list(
              sqrt(diag(m1_vcov)),
              sqrt(diag(m2_vcov)),
              sqrt(diag(m3_vcov))
            ),
            type = "latex",
            dep.var.caption = "Dependent variable: Policy implemented",
            dep.var.labels.include = TRUE,
            covariate.labels = c(
              "\\shortstack[l]{Support among\\\\low-income respondents}",
              "\\shortstack[l]{Support among\\\\high-income respondents}"
            ),
            add.lines = list(c("Survey waves", "91", "91", "91")),
            keep.stat = c("n", "rsq", "adj.rsq"),
            star.cutoffs = c(0.05, 0.01, 0.001),
            single.row = FALSE,
            font.size = "footnotesize",
            column.sep.width = "1pt",
            table.layout = "=l#-t-a-s="
  )
)

# Strip \begin{table} and \end{table}
start <- which(grepl("^\\\\begin\\{tabular\\}", full_tex))
end <- which(grepl("^\\\\end\\{tabular\\}", full_tex))
tabular_only <- full_tex[start:end]

# Write just the tabular to file
writeLines(tabular_only, "Output/stack_analysis_body.tex")

##########################################################
# ANALYSIS INDIVIDUAL LEVEL
##########################################################

##############################
# Table 2: Regression analysis, congruence on individual level
##############################

i1 <- lm(congruent ~ income_percentile_mid, data=slimdata_percentiles, weights = weight)
i2 <- lm(congruent ~ income_percentile_mid + as.factor(date) -1, data=slimdata_percentiles, weights = weight)

full_tex <- capture.output(
  stargazer(i1, i2,
            type = "latex",
            omit = "date",
            dep.var.caption = "\\shortstack{Dependent variable: Policy\\\\follows respondent preference}",
            dep.var.labels.include = TRUE,
            covariate.labels = c(
              "\\shortstack[l]{Place in income\\\\distribution (0-1)}"
            ),
            add.lines = list(c("Survey wave FE", "No", "Yes")),
            keep.stat = c("n", "rsq", "adj.rsq"),
            star.cutoffs = c(0.05, 0.01, 0.001),
            single.row = FALSE,
            font.size = "footnotesize",
            column.sep.width = "1pt",
            table.layout = "=l#-t-a-s="
  )
)

# Strip \begin{table} and \end{table}
start <- which(grepl("^\\\\begin\\{tabular\\}", full_tex))
end <- which(grepl("^\\\\end\\{tabular\\}", full_tex))
tabular_only <- full_tex[start:end]

# Write just the tabular to file
writeLines(tabular_only, "Output/congruence_analysis_body.tex")


##############################
# Appendix: Regression analysis, congruence on individual level, best for R personally
##############################

stargazer(i3, i4, type="text", omit = "date", omit.stat=c("SER", "F"))
stargazer(i3, i4, omit = "date", omit.stat=c("SER", "F"),
          out = "Output/congruence_analysis_personal.tex",
          type="latex", star.cutoffs = c(0.05, 0.01, 0.001),
          covariate.labels="Income percentile (0-1)")


##############################
# Table 3: Regression analysis, accuracy of perceptions
##############################

c1 <- lm(correct                               ~ income_percentile_mid + as.factor(date) -1, data=filter(slimdata_percentiles, !is.na(interest_preference)), weights = weight)
c2 <- lm(correct_forecast                      ~ income_percentile_mid + as.factor(date) -1, data=filter(slimdata_percentiles, !is.na(interest_preference)), weights = weight)
c3 <- lm(correct_rate                          ~ income_percentile_mid + as.factor(date) -1, data=filter(slimdata_percentiles, !is.na(interest_preference)), weights = weight)
c4 <- lm(eval(correct_forecast*smart_expect)   ~ income_percentile_mid + as.factor(date) -1, data=filter(slimdata_percentiles, !is.na(interest_preference)), weights = weight)
c5 <- lm(dum_understands                       ~ income_percentile_mid + as.factor(date) -1, data=filter(slimdata_percentiles, !is.na(interest_preference)), weights = weight)

full_tex <- capture.output(
  stargazer(c1, c2, c3, c4, c5,
            type = "latex",
            omit = "date",
            #dep.var.caption = "\\shortstack{Dependent variable: Policy\\\\follows respondent preference}",
            dep.var.labels = c("\\shortstack{Correct\\\\inflation\\\\perception}",
                              "\\shortstack{Same forecast\\\\as bank}",
                              "\\shortstack{Correct interest\\\\rate perception}",
                              "\\shortstack{Same forecast\\\\+ correct\\\\preference}",
                              "\\shortstack{Understands\\\\interest-\\\\inflation\\\\relationship}"),
            column.labels = c("\\textit{Accuracy:}", "\\textit{Dynamics:}"),
            column.separate = c(3, 2),
            covariate.labels = c(
              "\\shortstack[l]{Place in income\\\\distribution (0-1)}"
            ),
            add.lines = list(c("Survey wave FE", "Yes", "Yes", "Yes", "Yes", "Yes")),
            keep.stat = c("n", "rsq", "adj.rsq"),
            star.cutoffs = c(0.05, 0.01, 0.001),
            single.row = FALSE,
            font.size = "footnotesize",
            column.sep.width = "1pt",
            table.layout = "=cd#-t-a-s=",
            escape=FALSE
  )
)

# Strip \begin{table} and \end{table}
start <- which(grepl("^\\\\begin\\{tabular\\}", full_tex))
end <- which(grepl("^\\\\end\\{tabular\\}", full_tex))
tabular_only <- full_tex[start:end]

# Write just the tabular to file
writeLines(tabular_only, "Output/correct_analysis_body.tex")



##############################
# Table 4: Regression analysis, congruence controlling for accuracy of perceptions
##############################

cm1 <- lm(congruent ~ income_percentile_mid                                                                                  + as.factor(date) -1, data=slimdata_percentiles, weights = weight)
cm2 <- lm(congruent ~ income_percentile_mid + correct + correct_forecast + correct_rate                                      + as.factor(date) -1, data=slimdata_percentiles, weights = weight)
cm3 <- lm(congruent ~ income_percentile_mid + correct + correct_rate + eval(correct_forecast*smart_expect)                   + as.factor(date) -1, data=slimdata_percentiles, weights = weight)
cm4 <- lm(congruent ~ income_percentile_mid + correct + correct_rate + eval(correct_forecast*smart_expect) + dum_understands + as.factor(date) -1, data=slimdata_percentiles, weights = weight)

stargazer(cm1, cm2, cm3, cm4, type="text", omit = "date", omit.stat=c("SER", "F"), star.cutoffs=c(0.05, 0.01, 0.001))

full_tex <- capture.output(
  stargazer(cm1, cm2, cm3, cm4,
            type = "latex",
            omit = "date",
            dep.var.caption = "\\shortstack{Dependent variable: Policy\\\\follows respondent preference}",
            dep.var.labels.include = FALSE,
            covariate.labels = c(
              "\\shortstack[l]{Place in income\\\\distribution (0-1)}",
              "\\shortstack[l]{Correct inflation\\\\perception}",
              "\\shortstack[l]{Same forecast\\\\as bank}",
              "\\shortstack[l]{Correct rate\\\\perception}",
              "\\shortstack[l]{Same forecast +\\\\correct preference}",
              "\\shortstack[l]{Understands interest-\\\\inflation relationship}"
            ),
            add.lines = list(c("Survey wave FE", "Yes", "Yes", "Yes", "Yes")),
            keep.stat = c("n", "rsq", "adj.rsq"),
            star.cutoffs = c(0.05, 0.01, 0.001),
            single.row = FALSE,
            font.size = "footnotesize",
            column.sep.width = "1pt",
            table.layout = "=l#-t-a-s=",
            escape=FALSE
  )
)

# Strip \begin{table} and \end{table}
start <- which(grepl("^\\\\begin\\{tabular\\}", full_tex))
end <- which(grepl("^\\\\end\\{tabular\\}", full_tex))
tabular_only <- full_tex[start:end]

# Write just the tabular to file
writeLines(tabular_only, "Output/mediation_analysis_body.tex")





#######################################################################################
# Preferences for actions conditional on inflation expectations
#######################################################################################
agg <- slimdata_percentiles %>% 
  mutate(rich = case_when(income_percentile_mid > 0.5 ~ 1,
                          income_percentile_mid<= 0.5 ~ 0,
                          TRUE ~ as.double(NA)),
         future_price_num = case_when(future_price_num > 5.5 ~ 5.5,
                                      TRUE ~ future_price_num),
        #future_price_num = case_when(q2 == 9 ~ -2,
        #                              TRUE ~ future_price_num),
         past_price_num = case_when(past_price_num > 5.5 ~ 5.5,
                                    TRUE ~ past_price_num),
         interest_lower = case_match(interest_preference,
                                     -1 ~ 1,
                                     0 ~ 0,
                                     1 ~ 0,
                                     TRUE ~ as.double(NA)),
         interest_sq = case_match(interest_preference,
                                  -1 ~0,
                                  0 ~ 1,
                                  1 ~ 0,
                                  TRUE ~ as.double(NA)),
         interest_raise = case_match(interest_preference,
                                     -1 ~ 0,
                                     0 ~ 0,
                                     1 ~ 1,
                                     TRUE ~ as.double(NA)),
         house = case_match(tenure,
                            1 ~ "Owned outright",
                            2 ~ "Mortgage",
                            c(3, 4) ~ "Rent"))


# By quintiles
agg_future <- agg %>% 
  group_by(future_price_num) %>% 
  summarize(across(c(interest_preference,
                     interest_preference_personal,
                     interest_lower,
                     interest_sq,
                     interest_raise), ~ mean(.x, na.rm=TRUE)), .groups="drop")

agg_future_richpoor <- agg %>% 
  filter(!is.na(rich)) %>% 
  group_by(future_price_num, rich) %>% 
  summarize(across(c(interest_preference,
                     interest_preference_personal,
                     interest_lower,
                     interest_sq,
                     interest_raise), ~ mean(.x, na.rm=TRUE)),
            n = n(), .groups="drop")


# Get n for graph comment
agg_future_richpoor %>% 
  filter(!is.na(future_price_num)) %>% 
  summarize(n = sum(n))


graph_expectations <- agg_future %>% 
  filter(!is.na(future_price_num)) %>% 
  ggplot(aes(x=future_price_num, y=interest_sq)) +
  geom_vline(xintercept=2, linetype="dashed") +
  geom_line() +
  geom_point(shape=18, size=3) +
  geom_line(aes(y=interest_lower), col="red") +
  geom_point(aes(y=interest_lower), col="red", fill="red", shape=25) +
  geom_line(aes(y=interest_raise), col="blue") +
  geom_point(aes(y=interest_raise), col="blue", fill="blue", shape=24) +
  theme_minimal() +
  annotate("text", x=5.7, y=c(0.414, 0.321, 0.265), label=c("Status\nquo", "Lower", "Raise"), hjust=0, lineheight=0.6) +
  annotate("text", x=2.1, y=0.58, label="2% target", hjust=0) + 
  scale_x_continuous(labels = c("No idea", "<0%", "0%", "0-1%", "1-2%", "2-3%", "3-4%", "4-5%", ">5%"),
                     breaks = c(-1.5, -0.5, 0, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5),
                     limit=c(-2, 6)) +
  scale_y_continuous(breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6), limit=c(0, 0.6)) +
  labs(x="Expectated inflation next year", y="Proportion supporting change in interest rate") +
  theme(panel.grid.minor.x = element_blank())

png("Output/graph_expectations.png", width=6, height=4, units="in", res=300)
graph_expectations
dev.off()

graph_expectations_poor <- agg_future_richpoor %>% 
  filter(!is.na(future_price_num), !is.na(rich), rich==0) %>% 
  ggplot(aes(x=future_price_num, y=interest_sq*100)) +
  geom_line() +
  geom_point(shape=16, size=2.5) +
  geom_line(aes(y=interest_lower*100), col="red") +
  geom_point(aes(y=interest_lower*100), col="red", fill="red", shape=25) +
  geom_line(aes(y=interest_raise*100), col="blue") +
  geom_point(aes(y=interest_raise*100), col="blue", fill="blue", shape=24) +
  theme_minimal() +
  scale_x_continuous(labels = c("<0", "0", "0-1", "1-2", "2-3", "3-4", "4-5", ">5"),
                     breaks = c(-0.5, 0, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5),
                     limit=c(-1, 6)) +
  scale_y_continuous(breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6)*100, limit=c(0, 0.6)*100) +
  labs(x="Expected inflation next year", y="Percent supporting policy action", title="Low income respondents") +
  theme(panel.grid=element_blank(),
        axis.ticks = element_line(),
        panel.border = element_rect(fill=NA),
        plot.title = element_text(hjust=0.5)) +
  annotate("curve", curvature=0.5, x = 3.8, y=10, xend=5.5, yend=24, arrow=arrow(length=unit(0.2, units="cm")),
           col="#333333") +
  annotate("text", x=3.75, y=10, label="Among those who expect more than 5%\ninflation next year, 36% want to lower\nthe interest rate and 26% to raise it", hjust=1,
           size=3) +
  annotate("text", x=5.6, y=c(25.7, 38.0, 35.6), label=c("Raise", "Status\nquo", "Lower"), hjust=0, size=2.5, lineheight=0.8)


graph_expectations_rich <- agg_future_richpoor %>% 
  filter(!is.na(future_price_num), !is.na(rich), rich==1) %>% 
  ggplot(aes(x=future_price_num, y=interest_sq*100)) +
  geom_line() +
  geom_point(shape=16, size=2.5) +
  geom_line(aes(y=interest_lower*100), col="red") +
  geom_point(aes(y=interest_lower*100), col="red", fill="red", shape=25) +
  geom_line(aes(y=interest_raise*100), col="blue") +
  geom_point(aes(y=interest_raise*100), col="blue", fill="blue", shape=24) +
  theme_minimal() +
  scale_x_continuous(labels = c("<0", "0", "0-1", "1-2", "2-3", "3-4", "4-5", ">5"),
                     breaks = c(-0.5, 0, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5),
                     limit=c(-1, 6)) +
  scale_y_continuous(breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6)*100, limit=c(0, 0.6)*100) +
  labs(x="Expected inflation next year", y=element_blank(), title="High income respondents") +
  theme(panel.grid=element_blank(),
        axis.ticks = element_line(),
        panel.border = element_rect(fill=NA),
        plot.title = element_text(hjust=0.5)) +
  annotate("curve", curvature=0.5, x = 2.5, y=57, xend=0.1, yend=55.5, arrow=arrow(length=unit(0.2, units="cm")),
           col="#333333") +
  annotate("text", x=2.6, y=56, label="54% of those who expect prices\nto stand still next year want to\nkeep interest rate at status quo", hjust=0,
           size=3) +
  annotate("text", x=5.6, y=c(27.8, 41.0, 31.3), label=c("Raise", "Status\nquo", "Lower"), hjust=0, size=2.5, lineheight=0.8)

png("Output/graph_expectations_richpoor.png", width=8, height=4, units="in", res=300)
ggarrange(graph_expectations_poor, graph_expectations_rich, ncol=2, nrow=1)
dev.off()








########################################################################################################
# APPENDIX
########################################################################################################

# Correspondence between national and personal policy preference


slimdata_percentiles %>% 
  filter(!is.na(interest_preference), !is.na(interest_preference_personal)) %>% 
  group_by(interest_preference, interest_preference_personal) %>% 
  summarize(n = n(), .groups="drop") %>% 
  mutate(perc = round(n/sum(n)*100,1)) %>% 
  pivot_wider(id_cols=interest_preference_personal, names_from=interest_preference, values_from=perc)

# Total = 148036





#### FIGURE BAR GRAPH APPENDIX
bargraph_data_personal <- slimdata_percentiles %>% 
  filter(!is.na(income_percentile_mid), !is.na(interest_preference_personal)) %>% 
  mutate(rich = case_when(income_percentile_mid>0.5 ~ 1,
                          income_percentile_mid<=0.5 ~ 0,
                          TRUE ~ as.double(NA)),
         preference_raise = case_match(interest_preference_personal,
                                       1 ~ 1,
                                       c(-1, 0) ~ 0,
                                       TRUE ~ as.double(NA)),
         preference_sq = case_match(interest_preference_personal,
                                    0 ~ 1,
                                    c(-1, 1) ~ 0,
                                    TRUE ~ as.double(NA)),
         preference_lower = case_match(interest_preference_personal,
                                       -1 ~ 1,
                                       c(1, 0) ~ 0,
                                       TRUE ~ as.double(NA))) %>% 
  group_by(rich) %>% 
  summarize(across(c(preference_raise, preference_sq, preference_lower), ~ weighted.mean(.x, na.rm=TRUE, w=weight)),
            n = n()) %>% 
  pivot_longer(cols=starts_with("preference"), names_to="action")



bargraph_personal <- bargraph_data_personal %>% 
  mutate(
    rich = case_match(rich,
                      0 ~ "Below median income",
                      1 ~ "Above median income") |>
      factor(levels = c("Below median income",    # first (left)
                        "Above median income")),  # second (right)
    action_order = case_match(action,
                              "preference_lower" ~ -1,
                              "preference_sq"    ~  0,
                              "preference_raise" ~  1)
  ) %>% 
  ggplot(aes(x = action_order,
             y = value * 100,
             fill = rich)) +              # no need for as.factor() now
  geom_col(position = position_dodge(width = .9)) +
  geom_text(aes(label = paste0(round(value * 100, 1), "%"),
                y     = value * 100 + 1),
            position = position_dodge(width = .9),
            vjust = 0) +
  theme_minimal() +
  scale_x_continuous(breaks = c(-1, 0, 1),
                     labels = c("Lower\nrate",
                                "Keep\nstatus quo",
                                "Raise\nrate")) +
  labs(x = NULL,
       y = "Percent supporting action",
       fill = NULL) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "top") +
  scale_fill_manual(values = c("#ff5555", "#000077"))

png("Output/appendix_bargraph_preference_personal.png", width=5, height=4, units="in", res=300)
bargraph_personal
dev.off()





### Target too high or too low
# 1 = Too high, 2=Too low, 3=About right, 4=Don't know
slimdata_percentiles %>% 
  filter(q4!=4) %>% 
  group_by(q4) %>% 
  summarize(n=n()) %>% 
  mutate(perc = n/sum(n)*100,
         total = sum(n))




########################################################################################################
# Appendix regressions
########################################################################################################



##############################
# Regression analysis on stacked data, personal preference
##############################
# Run the models
pm1 <- lm(happened ~ support_personal_bottom, data = stack)
pm2 <- lm(happened ~ support_personal_top, data = stack)
pm3 <- lm(happened ~ support_personal_bottom + support_personal_top, data = stack)

# Clustered VCVs
pm1_vcov <- vcovCL(pm1, cluster = ~ stack$date)
pm2_vcov <- vcovCL(pm2, cluster = ~ stack$date)
pm3_vcov <- vcovCL(pm3, cluster = ~ stack$date)

# Clustered SEs
se_list <- list(
  sqrt(diag(pm1_vcov)),
  sqrt(diag(pm2_vcov)),
  sqrt(diag(pm3_vcov))
)

full_tex <- capture.output(
  stargazer(pm1, pm2, pm3,
            se = list(
              sqrt(diag(pm1_vcov)),
              sqrt(diag(pm2_vcov)),
              sqrt(diag(pm3_vcov))
            ),
            type = "latex",
            dep.var.caption = "Dependent variable: Policy implemented",
            dep.var.labels.include = TRUE,
            covariate.labels = c(
              "\\shortstack[l]{Support among\\\\low-income respondents}",
              "\\shortstack[l]{Support among\\\\high-income respondents}"
            ),
            add.lines = list(c("Survey waves", "91", "91", "91")),
            keep.stat = c("n", "rsq", "adj.rsq"),
            star.cutoffs = c(0.05, 0.01, 0.001),
            single.row = FALSE,
            font.size = "footnotesize",
            column.sep.width = "1pt",
            table.layout = "=l#-t-a-s="
  )
)

# Strip \begin{table} and \end{table}
start <- which(grepl("^\\\\begin\\{tabular\\}", full_tex))
end <- which(grepl("^\\\\end\\{tabular\\}", full_tex))
tabular_only <- full_tex[start:end]

# Write just the tabular to file
writeLines(tabular_only, "Output/appendix_stack_analysis_personal_body.tex")

