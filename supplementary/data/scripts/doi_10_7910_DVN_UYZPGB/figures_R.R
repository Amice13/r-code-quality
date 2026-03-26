###############################################################################
# Replication of graphs made in R: Figure 1, SI-1, SI-2, SI-3, SI-4, and SI-5 #
###############################################################################

setwd("~/Dropbox/Corona/Replication repo")

### Set Up

# load packages
library(rio)
library(dplyr)
library(ggplot2)

# load data
load('data/r-figures-data.RData')

# define Democrat/Republican color palette
custom.palette <- brewer.pal(n = 4, name = "RdBu")[c(4,1)]

### Figure 1: Governors’ tweets by topic -----

# create temporary data frames that have party,data, and whether or not each state first tweeted / posted about covid on that day
covid.first.dates.tmp <-
  first.tweets %>%
  select(state, party, covid) %>%
  filter(!is.na(covid)) %>%
  mutate(dummy = 1) %>%
  spread(key = 'state', value = "dummy", fill = 0) 

sd.first.dates.tmp <-
  first.tweets %>%
  select(state, party, social_distancing) %>%
  filter(!is.na(social_distancing)) %>%
  mutate(dummy = 1) %>%
  spread(key = 'state', value = "dummy", fill = 0) 

policy.tweet.difference.tmp <-
  first.tweets %>%
  select(state, party, stay_at_home) %>%
  filter(!is.na(stay_at_home)) %>%
  mutate(dummy = 1) %>%
  spread(key = 'state', value = "dummy", fill = 0) 

# florida never tweeted about SD
first.tweets$state[is.na(first.tweets$social_distancing)]
# alaska, florida, georgia, iowa, south caroline, and south dakota did not tweet about Stay At Home
first.tweets$state[is.na(first.tweets$stay_at_home)]

# sum first states on each date
covid.first.dates.tmp$states.first.on.date <- rowSums(covid.first.dates.tmp[3:52]) # ranges are based on how many states are present
sd.first.dates.tmp$states.first.on.date <- rowSums(sd.first.dates.tmp[3:51])
policy.tweet.difference.tmp$states.first.on.date <- rowSums(policy.tweet.difference.tmp[3:46])

# dataframes of how many states (by party) had tweeted about covid/social distancing/ stay at home by day
covid.first.dates.sum <- 
  covid.first.dates.tmp %>%
  select(party, date = covid, states.first.on.date) %>%
  group_by(party) %>%
  mutate(sum.to.date = cumsum(states.first.on.date))

sd.first.dates.sum <- 
  sd.first.dates.tmp %>%
  select(party, date = social_distancing, states.first.on.date) %>%
  group_by(party) %>%
  mutate(sum.to.date = cumsum(states.first.on.date))

policy.tweet.difference.sum <- 
  policy.tweet.difference.tmp %>%
  select(party, date = stay_at_home, states.first.on.date) %>%
  group_by(party) %>%
  mutate(sum.to.date = cumsum(states.first.on.date))

# graphing first covid tweets by party
covid.first.dates.graph <- 
  ggplot(covid.first.dates.sum, aes(x = date, y = sum.to.date, color = party)) +
  geom_line(alpha = 0.5, show.legend = FALSE) +
  geom_point(size = 9, color = "white") +
  geom_point(size = 7, alpha = 0.15) +
  geom_point(size = 7, shape = 1) +
  geom_text(aes(label = sum.to.date), show.legend = FALSE) +
  scale_color_manual(values = custom.palette) +
  theme_light() +
  labs(title = "First Tweet about Coronavirus",
       y = "Cumulative Number of Governors",
       x = "",
       color = "Governor's Party",
       shape = "Governor's Party",
       fill = "Governor's Party") +
  scale_x_date(date_breaks = "3 days",
               date_minor_breaks = "1 day",
               date_labels = "%b %d") +
  theme(legend.position = c(.9,.22),
        legend.background = element_rect(fill = "white", color = "grey"))

# graphing first social distancing tweets by party
sd.first.dates.graph <- 
  ggplot(sd.first.dates.sum, aes(x = date, y = sum.to.date, color = party)) +
  geom_line(alpha = 0.5, show.legend = FALSE) +
  geom_point(size = 9, color = "white") +
  geom_point(size = 7, alpha = 0.2) +
  geom_point(size = 7, shape = 1) +
  geom_text(aes(label = sum.to.date), show.legend = FALSE) +
  scale_color_manual(values = custom.palette) +
  theme_light() +
  labs(title = "First Tweet about Social Distancing",
       y = "Cumulative Number of Governors",
       x = "",
       color = "Governor's Party", shape = "Governor's Party",
       fill = "Governor's Party") +
  scale_x_date(date_breaks = "3 days",
               date_minor_breaks = "1 day",
               date_labels = "%b %d") +
  theme(legend.position = c(.9,.22),
        legend.background = element_rect(fill = "white", color = "grey"),
        plot.caption = element_text(size = 7))

# graphing first stay at home tweets by party
policy.tweet.difference.graph <- 
  ggplot(policy.tweet.difference.sum, aes(x = date, y = sum.to.date, color = party)) +
  geom_line(alpha = 0.5, show.legend = FALSE) +
  geom_point(size = 9, color = "white") +
  geom_point(size = 7, alpha = 0.15) +
  geom_point(size = 7, shape = 1) +
  geom_text(aes(label = sum.to.date), show.legend = FALSE) +
  scale_color_manual(values = custom.palette) +
  theme_light() +
  labs(title = "First Tweet about Staying Home",
       y = "Cumulative Number of Governors",
       x = "",
       color = "Governor's Party", shape = "Governor's Party",
       fill = "Governor's Party") +
  scale_x_date(date_breaks = "3 days",
               date_minor_breaks = "1 day",
               date_labels = "%b %d") +
  theme(legend.position = c(.9,.22),
        legend.background = element_rect(fill = "white", color = "grey"),
        plot.caption = element_text(size = 7))

# putting all twitter graphs together
all.first.dates.graph <-
  grid.arrange(
    covid.first.dates.graph + scale_x_date(
      limits = as.Date(c("2020-02-17", "2020-04-02")),
      date_breaks = "3 days",
      date_minor_breaks = "1 day",
      date_labels = "%b %d") +
      scale_y_continuous(limits = c(0, 25)),
    sd.first.dates.graph  + scale_x_date(
      limits = as.Date(c("2020-02-17", "2020-04-02")),
      date_breaks = "3 days",
      date_minor_breaks = "1 day",
      date_labels = "%b %d") +
      scale_y_continuous(limits = c(0, 25)),
    policy.tweet.difference.graph  + scale_x_date(
      limits = as.Date(c("2020-02-17", "2020-04-02")),
      date_breaks = "3 days",
      date_minor_breaks = "1 day",
      date_labels = "%b %d") +
      scale_y_continuous(limits = c(0, 25)),
    ncol = 1)

ggsave(plot = all.first.dates.graph, file = "output/Fig1.pdf",
       height = 15, width = 14, units = "in")


### Figure SI-1: Days between date of stay at home order and date of first tweet -----

# using policy data whether a mandate was put in place
policy <- subset(policy, Mandate == 1)

# convert dates to date foramt
policy <- policy %>%
  mutate(DateEnacted2 = as.Date(as.character(DateEnacted), format = "%Y%m%d"),
         DateIssued2 = as.Date(as.character(DateIssued), format = "%Y%m%d"))

# dataframe of the dates of the first policy issued/enacted and the first tweets
policy.tweet.difference <- policy %>%
  filter(StatePolicy == "StayAtHome") %>%
  group_by(StateName) %>%
  dplyr::summarize(home.policy.enacted = min(DateEnacted2),
                   home.policy.issued = min(DateIssued2)) %>%
  right_join(first.tweets, by = c("StateName" = "state"))


# calculate the number of days between tweet and policy
policy.tweet.difference <-
  policy.tweet.difference %>%
  mutate(enacted.to.tweet = as.Date(as.character(policy.tweet.difference$stay_at_home),
                                    format = "%Y-%m-%d") - home.policy.enacted,
         issued.to.tweet = as.Date(as.character(policy.tweet.difference$stay_at_home),
                                   format = "%Y-%m-%d") - home.policy.issued)

# calculate median values for graphing
median.dem.enacted <- median(subset(policy.tweet.difference, party == "Democrat")$enacted.to.tweet, na.rm=T)
median.rep.enacted <- median(subset(policy.tweet.difference, party == "Republican")$enacted.to.tweet, na.rm=T)
median.dem.issued <- median(subset(policy.tweet.difference, party == "Democrat")$issued.to.tweet, na.rm=T)
median.rep.issued <- median(subset(policy.tweet.difference, party == "Republican")$issued.to.tweet, na.rm=T)

# density plot of days between first stay at home tweet and policy ENACTED (% of states)
policy.tweet.difference.issued.density <- 
  ggplot(policy.tweet.difference, aes(x = issued.to.tweet, fill = party, color = party)) +
  geom_density(alpha = 0.2) +
  scale_x_continuous(breaks = seq(-24,14,2), limits = c(-24,14)) +
  scale_fill_manual(values = custom.palette) +
  scale_color_manual(values = custom.palette) +
  theme_light() +
  theme(legend.position = c(.85,.85),
        legend.background = element_rect(fill = "white", color = "grey")) +
  scale_y_continuous(breaks = c(0, .02, .04, .06, .08, .1, .12,.14), 
                     labels = c("0%", "2%", "4%", "6%", '8%', '10%', "12%","14%")) +
  labs(x = "Days between policy and first tweet",
       y = "Density",
       color = "Governor's Party", shape = "Governor's Party",
       fill = "Governor's Party") +
  geom_vline(xintercept = median.dem.issued, col=custom.palette[1],size=1) +
  geom_vline(xintercept = median.rep.issued, col=custom.palette[2],size=1)

ggsave(plot = policy.tweet.difference.issued.density, file = "output/incorrect-SI-1.jpg",
       height = 6, width = 8, units = "in")

### Figure SI-2: Intensity of Governors’ COVID-19 tweets over time -----

# count total number of covid-related tweets per day by partisanship
covid.tally.twitter <- covid.tweets %>% 
  group_by(party, date) %>%
  tally() %>%
  mutate(covid = cumsum(n)) %>%
  select(-n)

# count total number of social-distancing-related tweets per day by partisanship
sd.tally.twitter <- covid.tweets %>% 
  filter(social_distancing == 1) %>%
  group_by(party, date) %>%
  tally() %>%
  mutate(sd = cumsum(n)) %>%
  select(-n)

# count total number of stay-at-home-related tweets per day by partisanship
home.tally.twitter <- covid.tweets %>% 
  filter(stay_at_home == 1) %>%
  group_by(party, date) %>%
  tally() %>%
  mutate(home = cumsum(n)) %>%
  select(-n)

# join together
all.tally.twitter <- full_join(full_join(sd.tally.twitter, home.tally.twitter),covid.tally.twitter)
all.tally.twitter$sd[is.na(all.tally.twitter$sd)] <- 0
all.tally.twitter$home[is.na(all.tally.twitter$home)] <- 0

# manipulate data into long format where each row is the party, data, and topic of tweet
all.tally.twitter.long <- all.tally.twitter %>%
  gather(key = category, value = num_tweets, -c(party, date, party)) 

# create new column for labeling purposes
all.tally.twitter.long$`Tweet Subject`<- factor(all.tally.twitter.long$category, levels=c("covid","sd","home"),
                                                labels = c("Coronavirus", "Social Distancing", "Stay At Home"))

# graph cumulative number of tweets about all topics by partisanship
all.tweets.graph <-
  ggplot(all.tally.twitter.long, aes(x = date, y = num_tweets, color = party, shape = `Tweet Subject`)) +
  geom_point(size = 1) +
  geom_line(alpha = 0.5) +
  scale_color_manual(values = custom.palette) +
  theme_light() +
  labs(title = "",
       x = "",
       y = "Cumulative Tweets",
       color = "Governor's Party",
       shape = "Tweet Subject") +
  scale_x_date(date_breaks = "6 days",
               date_minor_breaks = "2 day",
               date_labels = "%b %d")+
  theme(legend.position = c(0.15, 0.65),
        legend.background = element_rect(fill = "white", color = "grey"))

ggsave(plot = all.tweets.graph, file = "output/SI-2.jpg",
       height = 5, width = 8, units = "in")

### Figure SI-3: Date of governors’ first tweets and Facebook posts -----

# dataset combining the first post dates by state
first.dates <- 
  left_join(first.tweets, first.fb.post, by = c("state", "party"),
            suffix = c(".twitter",".fb"))  %>%
  mutate(twitter.integer = as.integer(stay_at_home.twitter),
         fb.integer =  as.integer(stay_at_home.fb))

# calculating r^2 for facebook and twitter posts
home.post.lm = lm(twitter.integer ~ fb.integer, data = first.dates)
r.squared.home <- as.character(round(summary(home.post.lm)$r.squared, 3))

# first tweet about staying home versus first facebook post about staying home, by state
stay.home.posts <- 
  first.dates %>%
  ggplot(mapping = aes(x = stay_at_home.twitter, y = stay_at_home.fb, label = state, color = party, shape = party)) +
  geom_abline(intercept = 0, slope = 1, color = "grey30", linetype = "dashed") +
  geom_point(size = 3, alpha = .5) +
  theme_light() +
  scale_x_date(limits = as.Date(c("2020-03-14","2020-03-31")),
               date_breaks = "4 days",
               date_minor_breaks = "2 day",
               date_labels = "%b %d") +
  scale_y_date(limits = as.Date(c("2020-03-14","2020-03-31")),
               date_breaks = "4 days",
               date_minor_breaks = "2 day",
               date_labels = "%b %d") +
  scale_color_manual(values = custom.palette) +
  labs(
    x = "First Tweet About Stay At Home",
    y = "First Facebook Post About Stay At Home",
    color = "Governor's Party", shape = "Governor's Party"
  ) + 
  theme(legend.position = c(.8,.15),
        legend.background = element_rect(fill = "white", color = "grey")) +
  annotate(geom = "text", label = expression(~ R^2 ~ " = "),
           x = as.Date("2020-03-27"), y = as.Date("2020-03-31")) +
  annotate(geom = "text", label = r.squared.home,
           x = as.Date("2020-03-29"), y = as.Date("2020-03-31"))

ggsave(plot = stay.home.posts, file = "output/SI-3.jpg",
       height = 5, width = 5.2, units = "in")

### Figure SI-4: Governors’ followers on Facebook and Twitter -----

followers <- merge(followers.fb, followers.twitter, by = c("state", "type"),
                   suffixes = c(".fb", ".twitter")) %>%
  left_join(first.dates[1:2]) %>%
  mutate(type2 = stringr::str_to_title(type))

# calculating r^2 for facebook and twitter followers
official.lm = lm(followers_count.twitter ~ followers_count.fb, data = filter(followers, type2 == "Official"))
r.squared.official<- as.character(round(summary(official.lm)$r.squared, 3))
personal.lm = lm(followers_count.twitter ~ followers_count.fb, data = filter(followers, type2 == "Personal"))
r.squared.personal<- as.character(round(summary(personal.lm)$r.squared, 3))

# dataframe to bringr values into graph
facet.data.frame <- data.frame(type2 = c("Official", "Personal"), labels = c(r.squared.official, r.squared.personal), 
                               x = c(850000, 850000), y = c(950000, 950000))

# number of twitter followers by facebook followers -- offical versus personal facet
followers.facet <- 
  ggplot(followers, aes(x = followers_count.twitter, y = followers_count.fb)) +
  geom_smooth(method='lm', color = "black", alpha = 0.5, se = F, size = 0.5, linetype = "dashed") +
  geom_point(aes(color = party, shape = party), alpha = 0.8, size = 2) +
  scale_y_log10(breaks = c(10000, 100000, 1000000, 10000000),
                labels = c("10k", "100k", "1mil", "10 mil")) +
  scale_x_log10(breaks = c(1000, 10000, 100000, 1000000, 10000000),
                labels = c("1k","10k", "100k", "1mil", "10 mil")) +
  theme_light() +
  labs(caption = "Axes are in logarithmic scale",
       color = "Governor's Party",
       shape = "Governor's Party",
       x = "Number of Twitter Followers",
       y = "Number of Facebook Followers") + 
  facet_wrap(~type2) +
  scale_color_manual(values = custom.palette) +
  theme(legend.position = c(0.9, 0.17),
        legend.background = element_rect(fill = "white", color = "grey"),
        plot.caption = element_text(size = 6))  +
  annotate(geom = "text", label = expression(~ R^2 ~ " = "),
           x = 350000, 950000) +
  geom_text(data = facet.data.frame, mapping = aes(label = labels, x = x, y = y), inherit.aes = F)

# save figures
ggsave(plot = followers.facet, file = "output/SI-4.jpg",
       height = 4, width = 8, units = "in")

### Figure SI-5: Google search interest by partisan alignment and governor’s party over time -----

# smoothed lines divided by voting
gtrends$rep_dma2 <- factor(gtrends$rep_dma, labels = c("Voted Democrat", "Voted Republican"))
gtrends$rep_gov2 <- factor(gtrends$rep_gov, labels = c("Democratic Governor", "Republican Governor"))

smoothed.gtrends.coronavirus <- ggplot(gtrends) +
  geom_line(aes(x = day, y = gtrends_coronavirus, color = rep_dma2, group = dma),
            stat="smooth",method = "loess", size = 0.7, alpha = 0.15) +
  geom_line(aes(x = day, y = gtrends_coronavirus, color = rep_dma2),
            stat="smooth",method = "loess", size = 1.1) +
  scale_color_manual(values = brewer.pal(n = 4, name = "RdBu")[c(4,1)]) +
  theme_light() +
  theme(legend.position = "bottom") + 
  facet_wrap(~ rep_gov2) + 
  scale_x_date(breaks = as.Date(c("2020-03-01","2020-03-07",
                                  "2020-03-14","2020-03-21","2020-03-28")), 
               labels=date_format("%B %d"),
               limits = as.Date(c("2020-03-01","2020-03-31"))) +
  scale_y_continuous(limits = c(-10,100)) +
  labs(x = "",
       y = "Searches Relative to Peak",
       title = "Coronavirus",
       color = "")

smoothed.gtrends.social_distancing <- ggplot(gtrends) +
  geom_line(aes(x = day, y = gtrends_social_distancing, color = rep_dma2, group = dma),
            stat="smooth",method = "loess", size = 0.7, alpha = 0.15) +
  geom_line(aes(x = day, y = gtrends_social_distancing, color = rep_dma2),
            stat="smooth",method = "loess", size = 1.1) +
  scale_color_manual(values = brewer.pal(n = 4, name = "RdBu")[c(4,1)]) +
  theme_light() +
  theme(legend.position = "bottom") + 
  facet_wrap(~ rep_gov2) + 
  scale_x_date(breaks = as.Date(c("2020-03-01","2020-03-07",
                                  "2020-03-14","2020-03-21","2020-03-28")), 
               labels=date_format("%B %d"),
               limits = as.Date(c("2020-03-01","2020-03-31"))) +
  scale_y_continuous(limits = c(-10,100)) +
  labs(x = "",
       y = "Searches Relative to Peak",
       title = "Social Distancing",
       color = "")

ggsave(plot = smoothed.gtrends.coronavirus,
      filename =  "output/SI-5-coronavirus.jpg",
      height = 5, width = 10, units = "in")

ggsave(plot = smoothed.gtrends.social_distancing,
      filename =  "output/SI-5-social-distancing.jpg",
      height = 5, width = 10, units = "in")

