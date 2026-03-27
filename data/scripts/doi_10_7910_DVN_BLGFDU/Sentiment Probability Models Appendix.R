#Creates appendix models using probabilistic sentiment (Section I in Appendix)
#install.packages("stringr")
#install.packages("data.table")
#install.packages("dplyr")
#install.packages("lubridate")
#install.packages("lfe")
#install.packages("stargazer")
#install.packages("lme4")
#install.packages("ggplot2")

library(stringr)
library(data.table)
library(dplyr)
library(lubridate)
library(lfe)
library(stargazer)
library(lme4)
library(ggplot2)

#### Set your WD
#setwd("")


#### visits data
visits <- fread("NewsVisits.csv")

#### daily sentiment (probabilistic sentiment)
Rsentiment <- fread("RSentiment.csv")
Dsentiment <- fread("DSentiment.csv")
Rsentiment$Date <- mdy(Rsentiment$`Post Created Date`)
Dsentiment$Date <- mdy(Dsentiment$`Post Created Date`)

dailysentimentR <- Rsentiment %>%
  group_by(Date) %>%
  dplyr::summarize(
    Sentiment = mean(prob2)
  ) %>%
  ungroup()

dailysentimentR$RSentimentRoll <- frollmean(dailysentimentR$Sentiment, 3)
dailysentimentR$RSentimentRoll7 <- frollmean(dailysentimentR$Sentiment, 7)

dailysentimentD <- Dsentiment %>%
  group_by(Date) %>%
  dplyr::summarize(
    Sentiment = mean(prob2)
  ) %>%
  ungroup()

dailysentimentD$DSentimentRoll <- frollmean(dailysentimentD$Sentiment, 3)
dailysentimentD$DSentimentRoll7 <- frollmean(dailysentimentD$Sentiment, 7)

#### daily averages
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

####

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

##########################

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

#####

waveinfo <- merge(waveinfo, dailysentimentR, by.x = "day.x", by.y = "Date")
waveinfo <- merge(waveinfo, dailysentimentD, by.x = "day.x", by.y = "Date")


# demographic data
demos <- readRDS("Respondent_Info.rds")

waveinfo <- merge(waveinfo, demos, by = "person_id")

# problem with age variable, many invalid responses
waveinfo$white <- ifelse(waveinfo$ETHN == 1 & waveinfo$HISP == 2, 1, 0)
waveinfo$male = ifelse(waveinfo$GENDER == 1, 1, 0)
waveinfo$age <- floor(waveinfo$AGE_1/10)
waveinfo$edu <- ifelse(waveinfo$EDU %in% c(1, 5, 6), "no high school",
                       ifelse(waveinfo$EDU == 8, "high school",
                              ifelse(waveinfo$EDU %in% c(9, 10, 11), "associate/junior college",
                                     ifelse(waveinfo$EDU %in% c(12, 13), "undergraduate",
                                            ifelse(waveinfo$EDU %in% c(14, 15), "graduate", NA)))))


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

############################################## Main Models (Tables were removed from appendix due to space limitations, converted to coefficient plot below)
### models in-party

mod1 <- felm(newsvisits ~ SentimentRollco | person_id, data = waveinfoin) 
mod2 <- felm(newsvisits ~ SentimentRollco | person_id, data = filter(waveinfoin, affiliation == "Democrat")) 
mod3 <- felm(newsvisits ~ SentimentRollco | person_id, data = filter(waveinfoin, affiliation == "Republican")) 

stargazer(mod1, mod2, mod3, type = "html", out= "News Consumption inparty SentimentRoll (prob).htm")


mod1 <- felm(comedia ~ SentimentRollco | person_id, data = waveinfoin) 
mod2 <- felm(comedia ~ SentimentRollco | person_id, data = filter(waveinfoin, affiliation == "Democrat")) 
mod3 <- felm(comedia ~ SentimentRollco | person_id, data = filter(waveinfoin, affiliation == "Republican")) 

stargazer(mod1, mod2, mod3, type = "html", out= "copartisan media inparty SentimentRoll (prob).htm")


mod1 <- felm(political_pct ~ SentimentRollco | person_id, data = waveinfoin) 
mod2 <- felm(political_pct ~ SentimentRollco | person_id, data = filter(waveinfoin, affiliation == "Democrat")) 
mod3 <- felm(political_pct ~ SentimentRollco | person_id, data = filter(waveinfoin, affiliation == "Republican"))

stargazer(mod1, mod2, mod3, type = "html", out= "Political Pct inparty SentimentRoll (prob).htm")


mod1 <- felm(conegative ~ SentimentRollco + log(newsvisits) | person_id, data = waveinfoin) 
mod2 <- felm(conegative ~ SentimentRollco + log(newsvisits) | person_id, data = filter(waveinfoin, affiliation == "Democrat")) 
mod3 <- felm(conegative ~ SentimentRollco + log(newsvisits) | person_id, data = filter(waveinfoin, affiliation == "Republican"))

stargazer(mod1, mod2, mod3, type = "html", out= "Neg inparty SentimentRoll (prob).htm")



### models outparty
mod1 <- felm(newsvisits ~ SentimentRollout | person_id, data = waveinfoin) 
mod2 <- felm(newsvisits ~ SentimentRollout | person_id, data = filter(waveinfoin, affiliation == "Democrat")) 
mod3 <- felm(newsvisits ~ SentimentRollout | person_id, data = filter(waveinfoin, affiliation == "Republican")) 

stargazer(mod1, mod2, mod3, type = "html", out= "News Consumption outparty SentimentRoll (prob).htm")


mod1 <- felm(comedia ~ SentimentRollout | person_id, data = waveinfoin) 
mod2 <- felm(comedia ~ SentimentRollout | person_id, data = filter(waveinfoin, affiliation == "Democrat")) 
mod3 <- felm(comedia ~ SentimentRollout | person_id, data = filter(waveinfoin, affiliation == "Republican")) 

stargazer(mod1, mod2, mod3, type = "html", out= "copartisan Pct outparty SentimentRoll (prob).htm")


mod1 <- felm(political_pct ~ SentimentRollout | person_id, data = waveinfoin) 
mod2 <- felm(political_pct ~ SentimentRollout | person_id, data = filter(waveinfoin, affiliation == "Democrat")) 
mod3 <- felm(political_pct ~ SentimentRollout | person_id, data = filter(waveinfoin, affiliation == "Republican"))

stargazer(mod1, mod2, mod3, type = "html", out= "Political Pct outparty SentimentRoll (prob).htm")


mod1 <- felm(outnegative ~ SentimentRollout + log(newsvisits) | person_id, data = waveinfoin) 
mod2 <- felm(outnegative ~ SentimentRollout + log(newsvisits) | person_id, data = filter(waveinfoin, affiliation == "Democrat")) 
mod3 <- felm(outnegative ~ SentimentRollout + log(newsvisits) | person_id, data = filter(waveinfoin, affiliation == "Republican"))

stargazer(mod1, mod2, mod3, type = "html", out= "Neg outparty SentimentRoll (prob).htm")


######################################## plots (Figures A21 and A22)
#############inout plot
coefs <- read.csv("Macro Rolling Coefs Probabilistic.csv")

coefs$Model <- factor(coefs$Model, levels = c("News Visits Total", "Copartisan News Pct.", "Hard News Pct.", "Negative Pct."))

coefsin <- filter(coefs, Sentiment == "In Party")
coefsout <- filter(coefs, Sentiment == "Out Party")

ggplot(coefsin, aes(x = Party, y = coef)) +
  geom_point(position = position_dodge(.5), size = 2.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(.5),
                width = 0.2) +
  scale_shape_discrete(name = "") +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50")+
  ylab("Change in Consumption (Probabilistic In-Party Sentiment)")+
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank())+
  facet_wrap(~Model, scales = "free")
ggsave(filename = "FigA21_In Party Plot prob.png", width = 2042, height = 1312, units = "px")


ggplot(coefsout, aes(x = Party, y = coef)) +
  geom_point(position = position_dodge(.5), size = 2.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(.5),
                width = 0.2) +
  scale_shape_discrete(name = "") +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50")+
  ylab("Change in Consumption (Probabilistic Out-Party Sentiment)")+
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank())+
  facet_wrap(~Model, scales = "free")
ggsave(filename = "FigA22_Out Party Plot prob.png", width = 2042, height = 1312, units = "px")