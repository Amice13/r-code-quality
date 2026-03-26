############################################################################################
#### Fig 4: Increases in GeoLocated Twitter Activity by Province (modeled)
#### Fig A1: Increases in Geolocated Twitter Activity by Province (modeled)
#### data: twitter/data/a_CN_2019-12-01_2020-07-02_agg_fig4.csv
#### outputs: twitter/figs/Fig4_twitter_daily_jump_v_30days_later.pdf
####          twitter/figs/FigA1_twitter_daily_jump_v_30days_later_province_by_province.pdf
############################################################################################

library("tidyverse"); library("RColorBrewer"); library("here")

df = read_csv(here("twitter/data/a_CN_2019-12-01_2020-07-02_agg_fig4.csv"))

## Below is vestigial from when aggregation happened here.
# 
# source("Scripts/functions.R")
# provinces = read_csv("Data/tweets_raw/TwitterAdminMapping.csv")
# tweets = read_csv("Data/tweets_raw/a_CN_2019-12-01_2020-07-02.csv",
#              col_types = cols(.default = "c"))## ,
# tweets = left_join(tweets, provinces,
#                    by = c("place.name" = "province_fromTwitter"))
# tweets = tweets %>%
#   mutate(province = province_English) %>%
#   filter(province != "Cantón" &
#            province != "Jammu And Kashmir") %>%
#   filter(!is.na(province)) %>%
#   mutate(tweet_time=as.POSIXct(local_time_dt,
#                                format="%Y-%m-%d %H:%M:%S",
#                                tz="Asia/Hong_Kong")) %>%
#   mutate(tweet_date = as.Date(tweet_time)) %>%
#   filter(tweet_date >= as.Date("2019-11-01"))
# tweets$province = as.factor(tweets$province)
# tweets$tweet_clean = CleanTweet(tweets$text)
# tweets$simplified = DetectSimplified(tweets$tweet_clean)
# tweets$porn = DetectPorn(tweets$text)
# 
# df = tweets %>%
#     filter(lang == "zh") %>%
#     mutate(province = relevel(factor(province), ref="Guangdong")) %>%
#   group_by(province, user.id_str, tweet_date) %>%
#   summarise(the_n = n()) %>%
#   group_by(province, tweet_date) %>%
#     summarise(count = n()) %>%
#     ungroup() %>%
#   mutate(before = ifelse(tweet_date < as.Date("2020-01-23"), 1, 0),
#          after = ifelse(tweet_date > as.Date("2020-01-23"), 1, 0),
#          hubei = ifelse(province == "Hubei", 1, 0),
#          day = as.numeric(difftime(tweet_date, as.Date("2020-01-23"), units = "days")) / 30
#          )
# 
# tweets = tweets %>%
#     mutate(
#         user.created_date = as.Date(
#             tweets$user.created_at,
#             format="%a %b %d %H:%M:%S %z %Y",
#         )
#     )
# 
# length(unique(tweets %>% filter(lang=="zh") %>% pull(user.id_str)))
# length(unique(tweets %>% filter(lang=="zh" & substr(tweet_date, 1, 7)=="2020-05") %>% 
#                 pull(user.id_str))) / length(unique(tweets %>% 
#                                                       filter(lang=="zh" & substr(tweet_date, 1, 7)=="2019-12") %>% 
#                                                       pull(user.id_str)))
# df = subset(df, province != "PRC")

df = df %>%
  mutate(province = relevel(factor(province), ref="Guangdong"))

fit = df %>%
  lm(count ~ before:province + after:province + after:poly((abs(day)), 5):province - 1, data = .)

df_supp = rbind(
  df,
  data.frame(province = "Tibet", tweet_date = NA, count = 0, before = 1, after = 0, hubei = 0, day = -1 / 30),
  data.frame(province = "Qinghai", tweet_date = NA, count = 0, before = 1, after = 0, hubei = 0, day = 2))

predicted_df = data.frame(pred = predict(fit, df_supp, interval = "confidence")[, 1],
                          pred_lower = predict(fit, df_supp, interval = "confidence")[, 2],
                          pred_upper = predict(fit, df_supp, interval = "confidence")[, 3],
                          count = df_supp$count,
                          day=df_supp$day,
                          province = df_supp$province) %>%
    arrange(province, day)

#### Fig 4

pdf(here("twitter/figs/Fig4_twitter_daily_jump_v_30days_later.pdf"), width = 7, height = 4)
ggplot() +
    geom_text(
        aes(
            x = log(subset(predicted_df, near(day, 1/30))$pred / subset(predicted_df, near(day, -1/30))$pred),
            y = log(subset(predicted_df, near(day, 1))$pred / subset(predicted_df, near(day, -1/30))$pred),
            label = subset(predicted_df, near(day, 1/30))$province,
            size = (subset(predicted_df, day > 0) %>% group_by(province) %>% summarise(thecount = mean(count)))$thecount)
    ) +
    geom_text(
        aes(
            x = log(subset(predicted_df, province=="Hubei" & near(day, 1/30))$pred / subset(predicted_df, province=="Hubei" & near(day, -1/30))$pred),
            y = log(subset(predicted_df, province=="Hubei" & near(day, 1))$pred / subset(predicted_df, province=="Hubei" & near(day, -1/30))$pred),
            label = subset(predicted_df, province=="Hubei" & near(day, 1/30))$province,
            size = (subset(predicted_df, province=="Hubei" & day > 0) %>% group_by(province) %>% summarise(thecount = mean(count)))$thecount,
            color = "red"
        ),
        show.legend=FALSE
    ) +
    geom_smooth(
        aes(
            x = log(subset(predicted_df, near(day, 1/30))$pred / subset(predicted_df, near(day, -1/30))$pred),
            y = log(subset(predicted_df, near(day, 1))$pred / subset(predicted_df, near(day, -1/30))$pred)
        ),
        method = "lm", se = FALSE) +
    scale_x_continuous(label = exp, breaks=log(seq(0.1, 3, 0.4)), limits=c(log(0.6), log(3.6))) +
    scale_y_continuous(label = exp, breaks=log(seq(0.1, 3, 0.1)), limits=c(log(0.6), log(2))) +
    xlab("Jump - January 24, 2020") +
    ylab("Jump after 30 days") +
    guides(size=guide_legend(title="Average Number of \nUsers Geo-Locating\nEach Day After\nWuhan Lockdown")) +
    theme_classic() + geom_vline(xintercept=0, col="red", linetype=2) + geom_hline(yintercept=0, col="red", linetype=2)
dev.off()


#### Fig A1

pdf(here("twitter/figs/FigA1_twitter_daily_jump_v_30days_later_province_by_province.pdf"), width = 15, height = 10)
df %>%
  ggplot(aes(x=day, y=count)) +
  geom_ribbon(data = predicted_df %>% filter(day!=0),
              aes(ymin = pred_lower, ymax = pred_upper),
              fill = "grey70", alpha = 0.3) +
  geom_line(data = predicted_df %>% filter(day<0), aes(x=day, y=pred),
            color='blue', lwd=2) +
  geom_line(data = predicted_df %>% filter(day>0), aes(x=day, y=pred),
            color='blue', lwd=2) +
  geom_point(shape=1, size = 0.5) +
  geom_vline(xintercept = 0) +
  facet_wrap(~province, ncol=5, scales = "free_y") +
  theme_classic() + theme(strip.background = element_blank(), strip.text.x = element_text(size = 24), axis.text=element_text(size=16), axis.title=element_text(size=28,face="bold")) +
  ylab("Number of Unique Users Tweeting in Chinese") +
    xlab("Months Since Wuhan Quarantine")
dev.off()

