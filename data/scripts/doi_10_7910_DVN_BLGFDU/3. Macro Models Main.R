############## Macrolevel Data Analysis
install.packages("stringr")
#install.packages("data.table")
#install.packages("dplyr")
install.packages("lubridate")
install.packages("lfe")
install.packages("stargazer")
install.packages("lme4")

library(stringr)
library(data.table)
library(dplyr)
library(lubridate)
library(lfe)
library(stargazer)
library(lme4)

#### Set your WD
#setwd("")

#### visits data
visits <- fread("NewsVisits.csv")

#### daily sentiment
Rsentiment <- fread("RSentiment.csv")
Dsentiment <- fread("DSentiment.csv")

Rsentiment$Date <- mdy(Rsentiment$`Post Created Date`)
Dsentiment$Date <- mdy(Dsentiment$`Post Created Date`)

dailysentimentR <- Rsentiment %>%
  group_by(Date) %>%
  dplyr::summarize(
    Sentiment = mean(RSent_Feb2024)
  ) %>%
  ungroup()

dailysentimentR$RSentimentRoll <- frollmean(dailysentimentR$Sentiment, 3)
dailysentimentR$RSentimentRoll7 <- frollmean(dailysentimentR$Sentiment, 7)

dailysentimentD <- Dsentiment %>%
  group_by(Date) %>%
  dplyr::summarize(
    Sentiment = mean(DSent_Feb2024)
  ) %>%
  ungroup()

dailysentimentD$DSentimentRoll <- frollmean(dailysentimentD$Sentiment, 3)
dailysentimentD$DSentimentRoll7 <- frollmean(dailysentimentD$Sentiment, 7)

#### daily averages by wave
visits$right <- ifelse(visits$wilson > 0,1,0)
visits$left <- ifelse(visits$wilson < 0,1,0)

waveinfo <- visits %>%
  group_by(person_id, day, wave) %>%
  dplyr::summarize(
    newsvisits = n(),
    right_pct = mean(right, na.rm = TRUE)*100,
    left_pct = mean(left, na.rm = TRUE)*100,
    ideo_avg = mean(wilson, na.rm = TRUE),
    political = sum(pol_title),
    political_pct = mean(pol_title)*100,
    RSent = mean(RSent),
    DSent = mean(DSent)
  ) %>%
  ungroup()

#### daily negative visits average

dailyneg <- visits %>%
  group_by(day) %>%
  dplyr::summarize(
    negative_avgr = mean(RSent, na.rm = TRUE),
    negative_avgd = mean(DSent, na.rm = TRUE)
  ) %>%
  ungroup()

waveinfo <- merge(waveinfo, dailyneg, by = "day")
waveinfo <- filter(waveinfo, wave > 0)
waveinfo <- filter(waveinfo, wave < 4)
waveinfo <- filter(waveinfo, day > "2018-12-31")

########################## get days of the year

maxday <- waveinfo %>%
  group_by(person_id) %>%
  dplyr::summarize(
    maxday = max(day),
    minday = min(day)
  ) %>%
  ungroup()

days <- waveinfo %>%
  group_by(day) %>%
  dplyr::summarize(
    visits = n()
  ) %>%
  ungroup()

days <- select(days, -visits)

daysmatrix <- merge(days, maxday, by = NULL)
daysmatrix <- filter(daysmatrix, day <= maxday)
daysmatrix <- filter(daysmatrix, day >= minday)

daysmatrix$personday <- paste0(as.character(daysmatrix$person_id),"-", as.character(daysmatrix$day))
waveinfo$personday <- paste0(as.character(waveinfo$person_id),"-", as.character(waveinfo$day))
waveinfo <- select(waveinfo, - person_id)

waveinfo <- left_join(daysmatrix, waveinfo, by = "personday")
waveinfo$newsvisits[is.na(waveinfo$newsvisits)] <- 0

##### merge sentiment in

waveinfo <- merge(waveinfo, dailysentimentR, by.x = "day.x", by.y = "Date")
waveinfo <- merge(waveinfo, dailysentimentD, by.x = "day.x", by.y = "Date")


# demographic data
demos <- readRDS("Respondent_Info.rds")

waveinfo <- merge(waveinfo, demos, by = "person_id")

# fix demographics and age variable invalid responses
waveinfo$white <- ifelse(waveinfo$ETHN == 1 & waveinfo$HISP == 2, 1, 0)
waveinfo$male = ifelse(waveinfo$GENDER == 1, 1, 0)
waveinfo$age <- floor(waveinfo$AGE_1/10)
waveinfo$edu <- ifelse(waveinfo$EDU %in% c(1, 5, 6), "no high school",
                       ifelse(waveinfo$EDU == 8, "high school",
                              ifelse(waveinfo$EDU %in% c(9, 10, 11), "associate/junior college",
                                     ifelse(waveinfo$EDU %in% c(12, 13), "undergraduate",
                                            ifelse(waveinfo$EDU %in% c(14, 15), "graduate", NA)))))


###############
########################## Create Regression Variables
waveinfoRin <- filter(waveinfo, affiliation == "Republican")
waveinfoDin <- filter(waveinfo, affiliation == "Democrat")

waveinfoDin$comedia <- waveinfoDin$left_pct
waveinfoRin$comedia <- waveinfoRin$right_pct
waveinfoDin$conegative <- waveinfoDin$DSent*100
waveinfoRin$conegative <- waveinfoRin$RSent*100
waveinfoDin$negativeavgco <- waveinfoDin$negative_avgd
waveinfoRin$negativeavgco <- waveinfoRin$negative_avgr
waveinfoDin$negativeavgout <- waveinfoDin$negative_avgr
waveinfoRin$negativeavgout <- waveinfoRin$negative_avgd
waveinfoDin$SentimentRollco <- waveinfoDin$DSentimentRoll
waveinfoRin$SentimentRollco <- waveinfoRin$RSentimentRoll
waveinfoDin$SentimentRollout <- waveinfoDin$RSentimentRoll
waveinfoRin$SentimentRollout <- waveinfoRin$DSentimentRoll
waveinfoDin$outnegative <- waveinfoDin$RSent*100
waveinfoRin$outnegative <- waveinfoRin$DSent*100

waveinfoin <- rbind(waveinfoDin, waveinfoRin)

############################################## Main Models (tables created here, plotted after in script 5)
### models in-party (Main FE models (1-3), Appendix random effects models (4-6), Appendix random effects plus controls (7-9))
### Main model tables shown in appendix section E (table A14 to 21)
### Appendix robustness models plotted in script 5, corresponding to Appendix F

#News visits inparty
mod1 <- felm(newsvisits ~ SentimentRollco | person_id, data = waveinfoin) 
mod2 <- felm(newsvisits ~ SentimentRollco | person_id, data = filter(waveinfoin, affiliation == "Democrat")) 
mod3 <- felm(newsvisits ~ SentimentRollco | person_id, data = filter(waveinfoin, affiliation == "Republican")) 

mod4 <- lmer(newsvisits ~ SentimentRollco + (1 | person_id), data = waveinfoin) 
mod5 <- lmer(newsvisits ~ SentimentRollco + (1 | person_id), data = filter(waveinfoin, affiliation == "Democrat")) 
mod6 <- lmer(newsvisits ~ SentimentRollco + (1 | person_id), data = filter(waveinfoin, affiliation == "Republican")) 

mod7 <- lmer(newsvisits ~ SentimentRollco + age + male + white + edu + ideology + FOLLOW + (1 | person_id), data = waveinfoin) 
mod8 <- lmer(newsvisits ~ SentimentRollco + age + male + white + edu + ideology + FOLLOW + (1 | person_id), data = filter(waveinfoin, affiliation == "Democrat")) 
mod9 <- lmer(newsvisits ~ SentimentRollco + age + male + white + edu + ideology + FOLLOW + (1 | person_id), data = filter(waveinfoin, affiliation == "Republican")) 

#print tables, with hand wrangling in latex needed
stargazer(mod1, mod2, mod3, type = "html", out= "TableA14_News Consumption inparty.htm")
stargazer(mod1, mod2, mod3)
stargazer(mod4, mod5, mod6, mod7, mod8, mod9)

#Copartisan media inparty
mod1 <- felm(comedia ~ SentimentRollco | person_id, data = waveinfoin) 
mod2 <- felm(comedia ~ SentimentRollco | person_id, data = filter(waveinfoin, affiliation == "Democrat")) 
mod3 <- felm(comedia ~ SentimentRollco | person_id, data = filter(waveinfoin, affiliation == "Republican")) 

mod4 <- lmer(comedia ~ SentimentRollco + (1 | person_id), data = waveinfoin) 
mod5 <- lmer(comedia ~ SentimentRollco + (1 | person_id), data = filter(waveinfoin, affiliation == "Democrat")) 
mod6 <- lmer(comedia ~ SentimentRollco + (1 | person_id), data = filter(waveinfoin, affiliation == "Republican")) 

mod7 <- lmer(comedia ~ SentimentRollco + age + male + white + edu + ideology + FOLLOW + (1 | person_id), data = waveinfoin) 
mod8 <- lmer(comedia ~ SentimentRollco + age + male + white + edu + ideology + FOLLOW + (1 | person_id), data = filter(waveinfoin, affiliation == "Democrat")) 
mod9 <- lmer(comedia ~ SentimentRollco + age + male + white + edu + ideology + FOLLOW + (1 | person_id), data = filter(waveinfoin, affiliation == "Republican")) 

stargazer(mod1, mod2, mod3, type = "html", out= "TableA15_copartisan media inparty.htm")
stargazer(mod4, mod5, mod6, mod7, mod8, mod9)

#Political Pct. inparty
mod1 <- felm(political_pct ~ SentimentRollco | person_id, data = waveinfoin) 
mod2 <- felm(political_pct ~ SentimentRollco | person_id, data = filter(waveinfoin, affiliation == "Democrat")) 
mod3 <- felm(political_pct ~ SentimentRollco | person_id, data = filter(waveinfoin, affiliation == "Republican"))

mod4 <- lmer(political_pct ~ SentimentRollco + (1 | person_id), data = waveinfoin) 
mod5 <- lmer(political_pct ~ SentimentRollco + (1 | person_id), data = filter(waveinfoin, affiliation == "Democrat")) 
mod6 <- lmer(political_pct ~ SentimentRollco + (1 | person_id), data = filter(waveinfoin, affiliation == "Republican")) 

mod7 <- lmer(political_pct ~ SentimentRollco + age + male + white + edu + ideology + FOLLOW + (1 | person_id), data = waveinfoin) 
mod8 <- lmer(political_pct ~ SentimentRollco + age + male + white + edu + ideology + FOLLOW + (1 | person_id), data = filter(waveinfoin, affiliation == "Democrat")) 
mod9 <- lmer(political_pct ~ SentimentRollco + age + male + white + edu + ideology + FOLLOW + (1 | person_id), data = filter(waveinfoin, affiliation == "Republican")) 

stargazer(mod1, mod2, mod3, type = "html", out= "TableA16_Political Pct inparty.htm")
stargazer(mod4, mod5, mod6, mod7, mod8, mod9)


#Negative media inparty
mod1 <- felm(conegative ~ SentimentRollco + log(newsvisits) | person_id, data = waveinfoin) 
mod2 <- felm(conegative ~ SentimentRollco + log(newsvisits) | person_id, data = filter(waveinfoin, affiliation == "Democrat")) 
mod3 <- felm(conegative ~ SentimentRollco + log(newsvisits) | person_id, data = filter(waveinfoin, affiliation == "Republican"))

mod4 <- lmer(conegative ~ SentimentRollco + log(newsvisits)+ (1 | person_id), data = waveinfoin) 
mod5 <- lmer(conegative ~ SentimentRollco + log(newsvisits)+ (1 | person_id), data = filter(waveinfoin, affiliation == "Democrat")) 
mod6 <- lmer(conegative ~ SentimentRollco + log(newsvisits)+ (1 | person_id), data = filter(waveinfoin, affiliation == "Republican")) 

mod7 <- lmer(conegative ~ SentimentRollco + log(newsvisits) + age + male + white + edu + ideology + FOLLOW + (1 | person_id), data = waveinfoin) 
mod8 <- lmer(conegative ~ SentimentRollco + log(newsvisits) + age + male + white + edu + ideology + FOLLOW + (1 | person_id), data = filter(waveinfoin, affiliation == "Democrat")) 
mod9 <- lmer(conegative ~ SentimentRollco + log(newsvisits) + age + male + white + edu + ideology + FOLLOW + (1 | person_id), data = filter(waveinfoin, affiliation == "Republican")) 

stargazer(mod1, mod2, mod3, type = "html", out= "TableA17_Neg inparty.htm")
stargazer(mod4, mod5, mod6, mod7, mod8, mod9)

##########################################################################################
### models outparty
mod1 <- felm(newsvisits ~ SentimentRollout | person_id, data = waveinfoin) 
mod2 <- felm(newsvisits ~ SentimentRollout | person_id, data = filter(waveinfoin, affiliation == "Democrat")) 
mod3 <- felm(newsvisits ~ SentimentRollout | person_id, data = filter(waveinfoin, affiliation == "Republican")) 

mod4 <- lmer(newsvisits ~ SentimentRollout + (1 | person_id), data = waveinfoin) 
mod5 <- lmer(newsvisits ~ SentimentRollout + (1 | person_id), data = filter(waveinfoin, affiliation == "Democrat")) 
mod6 <- lmer(newsvisits ~ SentimentRollout + (1 | person_id), data = filter(waveinfoin, affiliation == "Republican")) 

mod7 <- lmer(newsvisits ~ SentimentRollout + age + male + white + edu + ideology + FOLLOW + (1 | person_id), data = waveinfoin) 
mod8 <- lmer(newsvisits ~ SentimentRollout + age + male + white + edu + ideology + FOLLOW + (1 | person_id), data = filter(waveinfoin, affiliation == "Democrat")) 
mod9 <- lmer(newsvisits ~ SentimentRollout + age + male + white + edu + ideology + FOLLOW + (1 | person_id), data = filter(waveinfoin, affiliation == "Republican")) 

stargazer(mod1, mod2, mod3, type = "html", out= "TableA18_News Consumption outparty.htm")
stargazer(mod4, mod5, mod6, mod7, mod8, mod9)

mod1 <- felm(comedia ~ SentimentRollout | person_id, data = waveinfoin) 
mod2 <- felm(comedia ~ SentimentRollout | person_id, data = filter(waveinfoin, affiliation == "Democrat")) 
mod3 <- felm(comedia ~ SentimentRollout | person_id, data = filter(waveinfoin, affiliation == "Republican")) 

mod4 <- lmer(comedia ~ SentimentRollout + (1 | person_id), data = waveinfoin) 
mod5 <- lmer(comedia ~ SentimentRollout + (1 | person_id), data = filter(waveinfoin, affiliation == "Democrat")) 
mod6 <- lmer(comedia ~ SentimentRollout + (1 | person_id), data = filter(waveinfoin, affiliation == "Republican")) 

mod7 <- lmer(comedia ~ SentimentRollout + age + male + white + edu + ideology + FOLLOW + (1 | person_id), data = waveinfoin) 
mod8 <- lmer(comedia ~ SentimentRollout + age + male + white + edu + ideology + FOLLOW + (1 | person_id), data = filter(waveinfoin, affiliation == "Democrat")) 
mod9 <- lmer(comedia ~ SentimentRollout + age + male + white + edu + ideology + FOLLOW + (1 | person_id), data = filter(waveinfoin, affiliation == "Republican")) 

stargazer(mod1, mod2, mod3, type = "html", out= "TableA19_copartisan Pct outparty.htm")
stargazer(mod4, mod5, mod6, mod7, mod8, mod9)

mod1 <- felm(political_pct ~ SentimentRollout | person_id, data = waveinfoin) 
mod2 <- felm(political_pct ~ SentimentRollout | person_id, data = filter(waveinfoin, affiliation == "Democrat")) 
mod3 <- felm(political_pct ~ SentimentRollout | person_id, data = filter(waveinfoin, affiliation == "Republican"))

mod4 <- lmer(political_pct ~ SentimentRollout + (1 | person_id), data = waveinfoin) 
mod5 <- lmer(political_pct ~ SentimentRollout + (1 | person_id), data = filter(waveinfoin, affiliation == "Democrat")) 
mod6 <- lmer(political_pct ~ SentimentRollout + (1 | person_id), data = filter(waveinfoin, affiliation == "Republican")) 

mod7 <- lmer(political_pct ~ SentimentRollout + age + male + white + edu + ideology + FOLLOW + (1 | person_id), data = waveinfoin) 
mod8 <- lmer(political_pct ~ SentimentRollout + age + male + white + edu + ideology + FOLLOW + (1 | person_id), data = filter(waveinfoin, affiliation == "Democrat")) 
mod9 <- lmer(political_pct ~ SentimentRollout + age + male + white + edu + ideology + FOLLOW + (1 | person_id), data = filter(waveinfoin, affiliation == "Republican")) 

stargazer(mod1, mod2, mod3, type = "html", out= "TableA20_Political Pct outparty.htm")
stargazer(mod4, mod5, mod6, mod7, mod8, mod9)


mod1 <- felm(outnegative ~ SentimentRollout + log(newsvisits) | person_id, data = waveinfoin) 
mod2 <- felm(outnegative ~ SentimentRollout + log(newsvisits) | person_id, data = filter(waveinfoin, affiliation == "Democrat")) 
mod3 <- felm(outnegative ~ SentimentRollout + log(newsvisits) | person_id, data = filter(waveinfoin, affiliation == "Republican"))

mod4 <- lmer(outnegative ~ SentimentRollout + log(newsvisits) + (1 | person_id), data = waveinfoin) 
mod5 <- lmer(outnegative ~ SentimentRollout + log(newsvisits) + (1 | person_id), data = filter(waveinfoin, affiliation == "Democrat")) 
mod6 <- lmer(outnegative ~ SentimentRollout + log(newsvisits) + (1 | person_id), data = filter(waveinfoin, affiliation == "Republican")) 

mod7 <- lmer(outnegative ~ SentimentRollout + log(newsvisits)  + age + male + white + edu + ideology + FOLLOW + (1 | person_id), data = waveinfoin) 
mod8 <- lmer(outnegative ~ SentimentRollout + log(newsvisits)  + age + male + white + edu + ideology + FOLLOW + (1 | person_id), data = filter(waveinfoin, affiliation == "Democrat")) 
mod9 <- lmer(outnegative ~ SentimentRollout + log(newsvisits)  + age + male + white + edu + ideology + FOLLOW + (1 | person_id), data = filter(waveinfoin, affiliation == "Republican")) 

stargazer(mod1, mod2, mod3, type = "html", out= "TableA21_Neg outparty.htm")
stargazer(mod4, mod5, mod6, mod7, mod8, mod9)


