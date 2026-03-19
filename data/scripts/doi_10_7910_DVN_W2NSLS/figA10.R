############################################################################################
#### Fig A10: New Users Stay on Twitter at the Same Rates across Locations
#### data: twitter/data/figA10_daily_new_user_tweets_sample_2020.csv
#### outputs: twitter/figs/FigA10_decay_cn_hk_tw_2020.pdf
############################################################################################

df = read_csv(here("twitter/data/figA10_daily_new_user_tweets_sample_2020.csv"))

## Below is vestigial from when aggregation happened here.
# 
# cn_tweets = read_csv("Data/sample_new_geolocated_user/cn_2020_new_user_tweets.csv")
# hk_tweets = read_csv("Data/sample_new_geolocated_user/hk_2020_new_user_tweets.csv")
# tw_tweets = read_csv("Data/sample_new_geolocated_user/tw_2020_new_user_tweets.csv")
# df = rbind(cn_tweets %>%
#              group_by(user.screen_name) %>%
#              mutate(tweet_by_user_count = n()) %>%
#              filter(tweet_by_user_count < 3100) %>%
#              group_by(days_since_join) %>%
#              summarize(tweet_count = n()) %>%
#              mutate(location = "China 2020") %>%
#              mutate(tweet_count_percentage = tweet_count / max(tweet_count)),
#            hk_tweets %>%
#              group_by(user.screen_name) %>%
#              mutate(tweet_by_user_count = n()) %>%
#              filter(tweet_by_user_count < 3100) %>%
#              group_by(days_since_join) %>%
#              summarize(tweet_count = n()) %>%
#              mutate(location = "Hong Kong 2020") %>%
#              mutate(tweet_count_percentage = tweet_count / max(tweet_count)),
#            tw_tweets %>%
#              group_by(user.screen_name) %>%
#              mutate(tweet_by_user_count = n()) %>%
#              filter(tweet_by_user_count < 3100) %>%
#              group_by(days_since_join) %>%
#              summarize(tweet_count = n()) %>%
#              mutate(location = "Taiwan 2020") %>%
#              mutate(tweet_count_percentage = tweet_count / max(tweet_count)))
# write_csv(df, here("twitter/data/figA10_daily_new_user_tweets_sample_2020.csv"))

p = df %>%
  ggplot(aes(x=as.numeric(days_since_join), y=tweet_count_percentage, color = location)) +
  geom_line() +
  theme_bw() +
  xlab("Days Since Join Twitter") +
  xlim(c(0, 300)) +
  ylab("Number of Tweets (Normalized, max = 1)") +
  theme(legend.position = "top") +
  labs(color="Location") +
  scale_colour_manual(values = RColorBrewer::brewer.pal(3, "Set1")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title="Decay of Daily Unique User Activity")

pdf(here("twitter/figs/FigA10_decay_cn_hk_tw_2020.pdf"), width = 6, height = 4)
print(p)
dev.off()