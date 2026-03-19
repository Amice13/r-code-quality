# Tremblay & Waring analysis ----------------------
# Cooperative and grocery store dictator game results
# read data (from desktop here; could substitute repository url later)
data <- read.csv("~/Desktop/coopexp.csv", sep = ",")
View(data)
# some notes on data: -----------------------------
## Coop - 1 if test site was co-op, 0 if grocery
## Random - contribution received by player in round 2 of game, simulating contribution from other player
## Consent - affirmed consent to participate in experiment
## Contrib - amount (dollars) player contributed in first round of experiment
## Payout - amount (dollars) participant was paid. Payout = 10 - Contrib + Random
## Age - participant age (years)
## Female - 1 if female, 0 if male
## Edu_Lvl - 1 - some high school, 2 - high school diploma, 3 - some college, 4 - associate degree, 5 - bachelor's degree, 6 - master's degree, 7 - doctorate or equivalent
## College - 1 if associate degree or higher, 0 otherwise
## House_Size - number of persons in household
## Income - 1 - <10k/yr, 2 - 10k+, 3 - 20k+, 4 - 30k+, 5 - 40k+, 6 - 50k+, 7 - 60k+, 8 - 70k+, 9 - 80k+, 10 - 90k+, 11 - 100k+, 12 - 200k+, 13 - 300k+
## Zipcode - USPS postal code (should have leading 0)
## Res_Zip - length of residence in current town. 1 - <2 yrs, 2 - 2-4 yrs, 3 - 5-7 yrs, 4 - 8-10 yrs, 5 - 11-20 yrs, 6 - 20+ yrs
## Res_ME - length of residence in Maine. Same as Res_Zip.
## Difficulty - How difficult did you find this game? 1 - Very difficult, 2 - Difficult, 3 - Somewhat difficult, 4 - Neutral, 5 - Somewhat easy, 6 - Easy, 7 - Very easy
## Weekly - 1 if shops at test site weekly or more often; 0 otherwise
## Freq_Shop - How often do you shop at this store? 1 - Never, 2 - <Once/month, 3 - Monthly, 4 - 2-3 times/month, 5 - Once a week, 6 - 2-3 times/week, 7 - Daily
## Primary_Store - 1 if 50% or more of shopping is done at test site, 0 otherwise
## Amt_Shop - What percentage of your grocery shopping is done at this store? 1 - <25%, 2 - 25-50%, 3 - 50-75%, 4 - >75%
## Satis_Day_Dummy - 1 if Somewhat Satisfied or better that day, 0 otherwise
## Satis_Day - How satisfied are you with your shopping experience today? 1 - Very unsatisfied, 2 - Unsatisfied, 3 - Somewhat unsatisfied, 4 - Neutral, 5 - Somewhat satisfied, 6 - Satisfied, 7 - Very satisfied
## Satis_Gen_Dummy - 1 if Somewhat Satisfied or better in general, 0 otherwise
## Satis_Gen - How satisfied are you with your shopping experience with this store in general? Same scale as Satis_Day
## Member - 1 if a member of Co-op, 0 otherwise
## Mem_Length - if respondent is a co-op member, the number of years they have been a member. Same scale as res_len.
## Pro1 - Pro8 - testing prosociality scale, beyond the scope of this analysis.
# additional notes -------------------------------
# a total of 126 individuals are presented, with 70 tested at the grocery and 56 at the co-op.
# the only required questions were those necessary to complete the dictator game. Survey questions were all optional; some, sucha s income, have lower response rates than others.
#
# analysis ---------------------------------------
# model specification ----------------------------

model <- lm(data$Contrib ~ 
              data$Coop + 
              data$Age + 
              data$Female + 
              data$College + 
              data$House_Size +
              data$Income +
              data$Res_Zip +
              data$Weekly +
              data$Primary_Store +
              data$Satis_Day_Dummy +
              data$Member)
summary(model)

# heteroskedasticity testing
require(lmtest)
bptest(model)

