###Create graphs to map differences in sentiment between German and Russian newspapers + Mainstream parties and the AfD

library(tidyverse)
library(haven)
library(lubridate)
library(ggpubr)


GetSentimentData <- function(data, outlet){
  dat <- read_csv(paste0("../",data)) %>% 
    transmute(month_year, pos_words = `positive words`, neg_words = `negative words`, posnegProp = pos_words/(pos_words + neg_words)) %>% 
    mutate(date = parse_date(month_year, "%m%Y")) %>% 
    arrange(date) %>% 
    mutate(outlet = outlet) %>% 
    dplyr::select(outlet, date, pos_words, neg_words, posnegProp)
  
  return(dat)
}

BildSentiment <- GetSentimentData("bild_sentiment.csv", "BILD")
FAZSentiment <- GetSentimentData("faz_sentiment.csv", "FAZ")
RTSentiment <- GetSentimentData("rt_sentiment.csv", "RT")
SputnikSentiment <- GetSentimentData("sputnik_sentiment.csv", "Sputnik")
SZSentiment <- GetSentimentData("sueddeutsche_sentiment.csv", "SZ")
tazSentiment <- GetSentimentData("taz_sentiment.csv", "taz")
WeltSentiment <- GetSentimentData("welt_sentiment.csv", "Welt")


CDUSentiment <- GetSentimentData("cdu_sentiment.csv", "CDU")
SPDSentiment <- GetSentimentData("spd_sentiment.csv", "SPD")
AfDSentiment <- GetSentimentData("afd_sentiment.csv", "AfD")
FDPSentiment <- GetSentimentData("fdp_sentiment.csv", "FDP")
LinkeSentiment <- GetSentimentData("linke_sentiment.csv", "The Left")
GreensSentiment <- GetSentimentData("greens_sentiment.csv", "Greens")


dataSentiment <- AfDSentiment %>% 
  bind_rows(BildSentiment, CDUSentiment, FAZSentiment, RTSentiment, SPDSentiment, SputnikSentiment, SZSentiment, tazSentiment, WeltSentiment, FDPSentiment, LinkeSentiment, GreensSentiment) %>% 
  dplyr::filter(date >= "2014-01-01" & date <= "2019-12-01")


media <- c("BILD", "FAZ", "RT", "Sputnik", "SZ", "taz", "Welt")

parties <- c("AfD", "CDU", "SPD", "FDP", "The Left", "Greens")

dataMedia <- dataSentiment %>% 
  filter(outlet %in% media) %>% 
  dplyr::select(outlet, date, posnegProp) %>% 
  mutate(RussianNews = if_else(outlet == "Sputnik" | outlet == "RT", 1, 0)) %>% 
  mutate(RussianNews = factor(RussianNews, levels = c("1", "0"), labels = c("Russian", "German")))
  
dataParties <- dataSentiment %>% 
  filter(outlet %in% parties) %>% 
  dplyr::select(outlet, date, posnegProp) %>% 
  mutate(afd = if_else(outlet == "AfD", 1, 0)) %>% 
  mutate(afd = factor(afd, levels = c("1", "0"), labels = c("AfD", "Mainstream")))


###Create plots for main analysis
##Media
dataMediaMain <- dataMedia %>% 
  group_by(RussianNews, date) %>% 
  summarize(posnegProp = mean(posnegProp)) %>% 
  ungroup() %>% 
  filter(date >= "2014-01-01")

MediaPlotMainBarArrange <- ggplot(dataMediaMain, aes(x = date, y = posnegProp)) + 
  geom_point() + 
  geom_segment( aes(x=date, xend=date, y=0, yend=posnegProp)) +
  theme_bw() +
  lims(y = c(0,1)) +
  labs(x = "", y = "Monthly Means") + 
  facet_wrap(~RussianNews) + 
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        text = element_text(size=14))

MediaBoxPlot <- ggplot(dataMediaMain, aes(x = factor(0), y=posnegProp)) + 
  geom_boxplot() + 
  geom_hline(yintercept = 0.55, linetype = "dashed") + 
  facet_wrap(~RussianNews) + 
  theme_bw() +
  lims(y = c(0.4,0.8)) + 
  scale_x_discrete(labels=c("0" = "0.00")) +
  theme(axis.text.y = element_text(color = "#FFFFFF"),
        axis.ticks.y = element_blank(),
        text = element_text(size=14)) + 
  labs(x = "Boxplot", y = "") + 
  coord_flip()

# pdf("MediaPlotMainBarBox.pdf", width = 10)
ggarrange(MediaBoxPlot,
          MediaPlotMainBarArrange,
          nrow = 2,
          heights = c(1,2))
# dev.off()


##Parties
dataPartiesMain <- dataParties %>% 
  filter(outlet != "The Left") %>% 
  group_by(afd, date) %>% 
  summarize(posnegProp = mean(posnegProp)) %>% 
  ungroup() %>% 
  filter(date >= "2014-01-01")


PartiesPlotMainBarArrange <- ggplot(dataPartiesMain, aes(x = date, y = posnegProp)) + 
  geom_point() + 
  geom_segment( aes(x=date, xend=date, y=0, yend=posnegProp)) +
  theme_bw() +
  lims(y = c(0,1)) +
  labs(x = "", y = "Monthly Means") + 
  facet_wrap(~afd) + 
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        text = element_text(size=14))

PartiesBoxPlot <- ggplot(dataPartiesMain, aes(x = factor(0), y=posnegProp)) + 
  geom_boxplot() + 
  geom_hline(yintercept = 0.65, linetype = "dashed") + 
  facet_wrap(~afd) + 
  lims(y = c(0.4,0.8)) + 
  theme_bw() +
  scale_x_discrete(labels=c("0" = "0.00")) +
  theme(axis.text.y = element_text(color = "#FFFFFF"),
        axis.ticks.y = element_blank(),
        text = element_text(size=14)) + 
  labs(x = "Boxplot", y = "") + 
  coord_flip()

# pdf("PartiesPlotMainBarBox.pdf", width = 10)
ggarrange(PartiesBoxPlot,
          PartiesPlotMainBarArrange,
          nrow = 2,
          heights = c(1,2))
# dev.off()

##Create plots for appendix
MediaPlotAppendix <- dataMedia %>% 
  ggplot(aes(x = date, y = posnegProp)) + 
  geom_line() + 
  theme_bw() +
  lims(y = c(0,1)) +
  labs(x = "Year", y = "Proportion positive", color = "Outlet") + 
  facet_wrap(~outlet)

# pdf("mediaPlotFullAppendix.pdf")
MediaPlotAppendix
# dev.off()


PartyPlotAppendix <- ggplot(dataParties, aes(x = date, y = posnegProp)) + 
  geom_line() + 
  theme_bw() +
  lims(y = c(0,1)) +
  labs(x = "Year", y = "Proportion positive", color = "Outlet") + 
  facet_wrap(~outlet)

# pdf("partyPlotAppendix.pdf")
PartyPlotAppendix
# dev.off()


###T-Tests

#Media
t.test(posnegProp ~ RussianNews, data = dataMedia)

#Parties
t.test(posnegProp ~ afd, data = dataParties)

