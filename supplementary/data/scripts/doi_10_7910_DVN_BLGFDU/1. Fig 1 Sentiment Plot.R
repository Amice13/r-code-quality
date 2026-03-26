#plot Figure 1 
install.packages("data.table")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("lubridate")

library(data.table)
library(ggplot2)
library(dplyr)
library(lubridate)


#### Set your WD
#setwd("")


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

dailysentimentR$SentimentRoll7 <- frollmean(dailysentimentR$Sentiment, 7)

dailysentimentD <- Dsentiment %>%
  group_by(Date) %>%
  dplyr::summarize(
    Sentiment = mean(DSent_Feb2024)
  ) %>%
  ungroup()

dailysentimentD$SentimentRoll7 <- frollmean(dailysentimentD$Sentiment, 7)

############get means
print("Get media party sentiment means, page 16")
mean(dailysentimentR$Sentiment)
mean(dailysentimentD$Sentiment)
############


# plotting Sentiment
dailysentimentD$Party <- "Democratic"
dailysentimentR$Party <- "Republican"

dailysentiment <- rbind(dailysentimentD, dailysentimentR)

dailysentiment <- filter(dailysentiment, Date != "2020-01-01")

ggplot(data=dailysentiment, aes(x=Date, y=SentimentRoll7, group=Party, color=Party)) +
  geom_line(size = 1.2) +
  ylab("") +
  xlab("") +
  geom_vline(xintercept = as.numeric(as.Date("2019-04-17")), linetype="dotted", 
             color = "black", size=1) +
  geom_vline(xintercept = as.numeric(as.Date("2019-07-14")), linetype="dotted", 
             color = "black", size=1) +
  geom_vline(xintercept = as.numeric(as.Date("2019-07-24")), linetype="dotted", 
             color = "black", size=1) +
  geom_vline(xintercept = as.numeric(as.Date("2019-09-18")), linetype="dotted", 
             color = "black", size=1) +
  geom_vline(xintercept = as.numeric(as.Date("2019-12-17")), linetype="dotted", 
             color = "black", size=1) +
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 month",
               labels = function(x) {
                 ifelse(x == as.Date("2019-01-01"), 
                        "Jan\n2019",  # First label with line break
                        ifelse(x == as.Date("2020-01-01"),
                               "Jan\n2020",  # Final label with line break
                               format(x, "%b")  # Other months: "Feb", "Mar", etc.
                        )
                 )
               }) +
  theme_bw() +
  scale_color_manual(values = c("grey50", "grey10")) +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  theme(
    axis.text.x = element_text(color="black"),
    axis.text.y = element_text(color="black"),
    axis.ticks = element_line(color = "black"),
    axis.title.x = element_text(margin = margin(t = 0)),
    axis.title.y = element_text(margin = margin(r = 5)),
    legend.title = element_blank()) +  # This line removes the legend title
  theme(legend.position="bottom")
ggsave("Figure1.png", width = 9, height = 5)


########################################################## Probability Model (Appendix A20)
#### daily sentiment
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

dailysentimentR$SentimentRoll <- frollmean(dailysentimentR$Sentiment, 3)
dailysentimentR$SentimentRoll7 <- frollmean(dailysentimentR$Sentiment, 7)

dailysentimentD <- Dsentiment %>%
  group_by(Date) %>%
  dplyr::summarize(
    Sentiment = mean(prob2)
  ) %>%
  ungroup()

dailysentimentD$SentimentRoll <- frollmean(dailysentimentD$Sentiment, 3)
dailysentimentD$SentimentRoll7 <- frollmean(dailysentimentD$Sentiment, 7)

# plotting Sentiment
dailysentimentD$Party <- "D"
dailysentimentR$Party <- "R"

dailysentiment <- rbind(dailysentimentD, dailysentimentR)

ggplot(data=dailysentiment, aes(x=Date, y=SentimentRoll7, group=Party, color=Party)) +
  geom_line(size = 1.2) +
  # curved poly fit stat_smooth(e = F, method = "lm", formula = y ~ poly(x, 8)) +
  ylab("Daily Negative News Sentiment by Party (Probabilistic Estimates)") +
  xlab("Date (2019)")+
  geom_vline(xintercept = as.numeric(as.Date("2019-04-17")), linetype="dotted", 
             color = "black", size=1)+
  geom_vline(xintercept = as.numeric(as.Date("2019-07-14")), linetype="dotted", 
             color = "black", size=1)+
  geom_vline(xintercept = as.numeric(as.Date("2019-07-24")), linetype="dotted", 
             color = "black", size=1)+
  geom_vline(xintercept = as.numeric(as.Date("2019-09-18")), linetype="dotted", 
             color = "black", size=1)+
  geom_vline(xintercept = as.numeric(as.Date("2019-12-17")), linetype="dotted", 
             color = "black", size=1)+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 month",
               date_labels = "%B")+
  theme_bw()+
  scale_color_manual(values = c("grey50", "grey80", "grey10"))
ggsave("FigureA20_DailySentiment_Probabilistic_Appendix.png", width = 9, height = 5)
