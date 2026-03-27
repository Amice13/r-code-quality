############################################################################################
#### Fig A4: Weekly changes in within city movement and geolocated Twitter users relative to 
#### pre-lockdown period, after adjusting for the same period in 2019.
#### data: twitter/data/figA4_tweet_by_province_2019.csv  
####       twitter/data/figA4_bottompanel_2019.csv
####       # Note that tweet_date for 2019 is the date adjusting for 2020 Chinese New Year
####       tweet_date = tweet_date + difftime(as.Date("2020-01-25"), as.Date("2019-02-05"))
####       twitter/data/figA4_tweet_by_province_2020.csv
####       twitter/data/figA4_bottompanel_2020.csv
####       twitter/data/Index_City_CH_EN.csv
####       twitter/data/baidu_within_city_movement.xlsx
#### outputs: twitter/figs/FigA4_1_twitter_v_mobility_ratio_2019_one_week.pdf
####          twitter/figs/FigA4_2_twitter_v_mobility_ratio_2019_two_week.pdf
####          twitter/figs/FigA4_3_twitter_v_mobility_ratio_2019_three_week.pdf
####          twitter/figs/FigA4_4_twitter_v_mobility_ratio_2019_four_week.pdf
####          twitter/figs/FigA4_bottom_twitter_compare_2020_2019_absolute_one_week.pdf
############################################################################################

library("tidyverse"); library("RColorBrewer"); library("here")

df_2019 = read_csv(here("twitter/data/figA4_tweet_by_province_2019.csv"))
df_2020 = read_csv(here("twitter/data/figA4_tweet_by_province_2020.csv"))
cities = read_csv(here("twitter/data/Index_City_CH_EN.csv"))
movement = readxl::read_excel(here("twitter/data/baidu_within_city_movement.xlsx"))

### Figure A4

provinces = cities %>%
  group_by(Prov_EN) %>%
  filter(row_number()==1) %>%
  select(Prov_EN, Prov_CH, GbProv) %>%
  filter(!is.na(Prov_EN)) %>%
  # consistent with Twitter province naming
  mutate(Prov_EN = ifelse(Prov_EN=="Xizang", "Tibet", Prov_EN)) %>%
  mutate(Prov_EN = ifelse(Prov_EN=="Neimenggu", "Inner Mongolia", Prov_EN))
movement = movement %>%
  rename(GbCity = `城市代码`) %>%
  select(-`城市`) %>%
  pivot_longer(!GbCity, names_to = "date", values_to = "index_movement")
movement_city_2019 = movement %>%
  mutate(date = as.Date(date, format = "%Y%m%d")) %>%
  filter(date <= as.Date("2019-03-28")) %>%
  mutate(date = date + difftime(as.Date("2020-01-25"), # 2020 Chinse New Year
                                as.Date("2019-02-05"), # 2019 Chinse New Year
                                units=c("days"))) %>%
  rename(index_2019 = index_movement) %>%
  mutate(GbProv = substr(GbCity, 1, 2)) %>%
  merge(provinces, by = c("GbProv"))
movement_city_2020 = movement %>%
  mutate(date = as.Date(date, format = "%Y%m%d")) %>%
  filter(date <= as.Date("2020-03-16") & 
           date >= as.Date("2020-01-01")) %>%
  mutate(GbProv = substr(GbCity, 1, 2)) %>%
  merge(provinces, by = c("GbProv"))
movement_combined = merge(movement_city_2019, movement_city_2020, 
                          by=c("GbCity", "date", "Prov_EN", "Prov_CH", "GbProv")) %>%
  mutate(Prov_EN = relevel(factor(Prov_EN), ref="Guangdong")) %>%
  filter(Prov_EN != "Chongqing") %>%
  mutate(index_movement_diff = index_movement - index_2019,
         index_movement_ratio = index_movement/index_2019) %>%
  group_by(GbProv, date) %>%
  mutate(index_movement_2020_mean = mean(index_movement),
         index_movement_2019_mean = mean(index_movement),
         index_movement_diff_mean = mean(index_movement_diff),
         index_movement_ratio_mean = mean(index_movement_ratio)) %>%
  ungroup()
movement_before_after_ratio = merge(
  movement_combined %>%
    filter(date <= as.Date("2020-01-21") &
             date >= as.Date("2020-01-15")) %>%
    group_by(Prov_EN) %>%
    summarize(before_sum_2020=sum(index_movement),
              before_sum_2019=sum(index_2019),
              before_sum_ratio=sum(index_movement)/sum(index_2019)),
  movement_combined %>%
    filter(date >= as.Date("2020-01-23") &
             date <= as.Date("2020-01-29")) %>%
    group_by(Prov_EN) %>%
    summarize(after_sum_2020=sum(index_movement),
              after_sum_2019=sum(index_2019),
              after_sum_ratio=sum(index_movement)/sum(index_2019)),
  by = "Prov_EN") %>%
  mutate(movement_ratio_ratio = after_sum_ratio / before_sum_ratio)

## Below is vestigial from when aggregation happened here.
# 
# source("Scripts/functions.R")
# provinces = read_csv("Data/tweets_raw/TwitterAdminMapping.csv")
# tweets_2020 = read_csv("Data/tweets_raw/a_CN_2019-12-01_2020-07-02.csv",
#                        col_types = cols(.default = "c"))
# tweets_2020 = left_join(tweets_2020, provinces,
#                         by = c("place.name" = "province_fromTwitter"))
# tweets_2020 = tweets_2020 %>%
#   mutate(province = province_English) %>%
#   filter(province != "Cantón" &
#            ## province != "Tibet" & ## Xizang is the same as Tibet
#            province != "Jammu And Kashmir") %>%
#   filter(!is.na(province)) %>%
#   mutate(tweet_time=as.POSIXct(local_time_dt,
#                                format="%Y-%m-%d %H:%M:%S",
#                                tz="Asia/Hong_Kong")) %>%
#   mutate(tweet_date = as.Date(substr(tweet_time, 1, 10))) %>%
#   filter(tweet_date >= as.Date("2019-11-01"))
# tweets_2020$province = as.factor(tweets_2020$province)
# tweets_2020$tweet_clean = CleanTweet(tweets_2020$text)
# tweets_2020$simplified = DetectSimplified(tweets_2020$tweet_clean)
# tweets_2020$porn = DetectPorn(tweets_2020$text)
# 
# tweets_2019 = read_csv("Data/tweets_raw/a_CN_2018-12-01_2019-07-02.csv",
#                        col_types = cols(.default = "c"))
# tweets_2019 = left_join(tweets_2019, provinces,
#                         by = c("place.name" = "province_fromTwitter"))
# tweets_2019 = tweets_2019 %>%
#   mutate(province = province_English) %>%
#   filter(province != "Cantón" &
#            ## province != "Tibet" & ## Xizang is the same as Tibet
#            province != "Jammu And Kashmir") %>%
#   filter(!is.na(province)) %>%
#   mutate(tweet_time=as.POSIXct(local_time_dt,
#                                format="%Y-%m-%d %H:%M:%S",
#                                tz="Asia/Hong_Kong")) %>%
#   mutate(tweet_date = as.Date(substr(tweet_time, 1, 10))) %>%
#   filter(tweet_date <= as.Date("2019-06-01"))
# tweets_2019$province = as.factor(tweets_2019$province)
# tweets_2019$tweet_clean = CleanTweet(tweets_2019$text)
# tweets_2019$simplified = DetectSimplified(tweets_2019$tweet_clean)
# tweets_2019$porn = DetectPorn(tweets_2019$text)
# tweets_2019 = tweets_2019 %>%
#   mutate(tweet_date = tweet_date + difftime(as.Date("2020-01-25"), # 2020 Chinese New Year
#                                             as.Date("2019-02-05"), # 2019 Chinese New Year
#                                             units=c("days")))
# tweets_2019 = tweets_2019 %>%
#   filter(tweet_date <= as.Date("2020-03-16") & 
#            tweet_date >= as.Date("2020-01-01"))
# tweets_2020 = tweets_2020 %>%
#   filter(tweet_date <= as.Date("2020-03-16") & 
#            tweet_date >= as.Date("2020-01-01"))
# 
# df_2020 = tweets_2020 %>%
#   filter(lang == "zh" & simplified == 1 & porn == 0) %>%
#   #filter(lang == "zh") %>%
#   mutate(province = relevel(factor(province), ref="Guangdong")) %>%
#   group_by(province, user.id_str, tweet_date) %>%
#   summarise(the_n = n()) %>%
#   ## filter(n()==1) %>%
#   group_by(province, tweet_date) %>%
#   summarise(count = n()) %>%
#   ungroup() %>%
#   complete(province, nesting(tweet_date), fill=list(count = 0)) %>%
#   mutate(before = ifelse(tweet_date < as.Date("2020-01-23"), 1, 0),
#          after = ifelse(tweet_date > as.Date("2020-01-23"), 1, 0),
#          hubei = ifelse(province == "Hubei", 1, 0),
#          day = as.numeric(difftime(tweet_date, as.Date("2020-01-23"), units = "days")) / 30
#   )
# 
# df_2019 = tweets_2019 %>%
#   filter(lang == "zh" & simplified == 1 & porn == 0) %>%
#   #filter(lang == "zh") %>%
#   mutate(province = relevel(factor(province), ref="Guangdong")) %>%
#   group_by(province, user.id_str, tweet_date) %>%
#   summarise(the_n = n()) %>%
#   ## filter(n()==1) %>%
#   group_by(province, tweet_date) %>%
#   summarise(count = n()) %>%
#   ungroup() %>%
#   complete(province, nesting(tweet_date), fill=list(count = 0)) %>%
#   mutate(before = ifelse(tweet_date < as.Date("2020-01-23"), 1, 0),
#          after = ifelse(tweet_date > as.Date("2020-01-23"), 1, 0),
#          hubei = ifelse(province == "Hubei", 1, 0),
#          day = as.numeric(difftime(tweet_date, as.Date("2020-01-23"), units = "days")) / 30
#   )
# df_2019 = df_2019 %>%
#   rename(count_2019 = count)
# write_csv(df_2019, here("twitter/data/figA4_tweet_by_province_2019.csv"))
# write_csv(df_2020, here("twitter/data/figA4_tweet_by_province_2020.csv"))

# consistent with twitter jump figure
tweet_combined = merge(df_2020, df_2019, 
                       by=c("province", "tweet_date", "before", "after", "hubei", "day")) %>%
  mutate(province = relevel(factor(province), ref="Guangdong")) %>%
  filter(province != "Chongqing") %>%
  mutate(count_diff = count - count_2019,
         count_ratio = count/count_2019)


### Figure A4 - 1

tweet_before_after_ratio = merge(
  tweet_combined %>%
    filter(tweet_date <= as.Date("2020-01-22") &
             tweet_date >= as.Date("2020-01-16")) %>%
    group_by(province) %>%
    summarize(before_sum_2020=sum(count),
              before_sum_2019=sum(count_2019),
              before_sum_ratio=sum(count)/sum(count_2019)),
  tweet_combined %>%
    filter(tweet_date >= as.Date("2020-01-23") &
             tweet_date <= as.Date("2020-01-29")) %>%
    group_by(province) %>%
    summarize(after_sum_2020=sum(count),
              after_sum_2019=sum(count_2019),
              after_sum_ratio=sum(count)/sum(count_2019)),
  by = "province") %>%
  mutate(tweet_ratio_ratio = after_sum_ratio / before_sum_ratio)


plot_df = merge(tweet_before_after_ratio %>% 
                  #filter(before_sum_2019 > 35) %>%
                  select(province, tweet_ratio_ratio),
                movement_before_after_ratio %>%
                  select(Prov_EN, movement_ratio_ratio),
                by.x = c("province"), by.y = c("Prov_EN")) %>%
  mutate(color = ifelse(province=="Hubei", "red", "black"))

corr_label = paste0("Corr = ", 
                    round(cor(plot_df$movement_ratio_ratio, plot_df$tweet_ratio_ratio), 2),
                    "\n95% CI = [",
                    round(cor.test(plot_df$movement_ratio_ratio, plot_df$tweet_ratio_ratio)$conf.int[1], 2),
                    ",",
                    round(cor.test(plot_df$movement_ratio_ratio, plot_df$tweet_ratio_ratio)$conf.int[2], 2),
                    "]")

pdf(here("twitter/figs/FigA4_1_twitter_v_mobility_ratio_2019_one_week.pdf"), width = 7, height = 5)
ggplot(plot_df, aes(x = movement_ratio_ratio, y = tweet_ratio_ratio)) +
  #geom_vline(xintercept=1, col="red", linetype=2) +
  geom_hline(yintercept=1, col="red") +
  geom_smooth(method="lm", se=FALSE) +
  # geom_text(aes(label = province, color=color),
  #                          show.legend = FALSE) +
  ggrepel::geom_text_repel(aes(label = province, color=color),
                           show.legend = FALSE) +
  scale_color_manual(values=c("black", "red")) +
  annotate(geom = 'text', 
           x = min(plot_df$movement_ratio_ratio), 
           y = min(plot_df$tweet_ratio_ratio), 
           hjust = 0,
           vjust = 0,
           label = as.character(corr_label)) +
  xlab("Within City Movement (Relative Change adjusting for 2019)") +
  ylab("Geolocated Twitter Users (Relative Change)") +
  labs(title = "Reduced Mobility vs. Twitter Users (1st Week)",
       subtitle = "Relative Change (Ratios) 1st Week After Wuhan Lockdown, Adjusting for Same Period in 2019") +
  theme_classic() + 
  theme(strip.background = element_blank(),
        plot.title = element_text(size=16, hjust = 0.5))
dev.off()


### Figure A4 - 2


tweet_before_after_ratio = merge(
  tweet_combined %>%
    filter(tweet_date <= as.Date("2020-01-21") &
             tweet_date >= as.Date("2020-01-15")) %>%
    group_by(province) %>%
    summarize(before_sum_2020=sum(count),
              before_sum_2019=sum(count_2019),
              before_sum_ratio=sum(count)/sum(count_2019)),
  tweet_combined %>%
    filter(tweet_date >= as.Date("2020-01-30") &
             tweet_date <= as.Date("2020-02-05")) %>%
    group_by(province) %>%
    summarize(after_sum_2020=sum(count),
              after_sum_2019=sum(count_2019),
              after_sum_ratio=sum(count)/sum(count_2019)),
  by = "province") %>%
  mutate(tweet_ratio_ratio = after_sum_ratio / before_sum_ratio)

movement_before_after_ratio = merge(
  movement_combined %>%
    filter(date <= as.Date("2020-01-21") &
             date >= as.Date("2020-01-15")) %>%
    group_by(Prov_EN) %>%
    summarize(before_sum_2020=sum(index_movement),
              before_sum_2019=sum(index_2019),
              before_sum_ratio=sum(index_movement)/sum(index_2019)),
  movement_combined %>%
    filter(date >= as.Date("2020-01-30") &
             date <= as.Date("2020-02-05")) %>%
    group_by(Prov_EN) %>%
    summarize(after_sum_2020=sum(index_movement),
              after_sum_2019=sum(index_2019),
              after_sum_ratio=sum(index_movement)/sum(index_2019)),
  by = "Prov_EN") %>%
  mutate(movement_ratio_ratio = after_sum_ratio / before_sum_ratio)


plot_df = merge(tweet_before_after_ratio %>% 
                  #filter(before_sum_2019 > 35) %>%
                  select(province, tweet_ratio_ratio),
                movement_before_after_ratio %>%
                  select(Prov_EN, movement_ratio_ratio),
                by.x = c("province"), by.y = c("Prov_EN")) %>%
  mutate(color = ifelse(province=="Hubei", "red", "black"))

corr_label = paste0("Corr = ", 
                    round(cor(plot_df$movement_ratio_ratio, plot_df$tweet_ratio_ratio), 2),
                    "\n95% CI = [",
                    round(cor.test(plot_df$movement_ratio_ratio, plot_df$tweet_ratio_ratio)$conf.int[1], 2),
                    ",",
                    round(cor.test(plot_df$movement_ratio_ratio, plot_df$tweet_ratio_ratio)$conf.int[2], 2),
                    "]")

pdf(here("twitter/figs/FigA4_2_twitter_v_mobility_ratio_2019_two_week.pdf"), width = 7, height = 5)
ggplot(plot_df, aes(x = movement_ratio_ratio, y = tweet_ratio_ratio)) +
  #geom_vline(xintercept=1, col="red", linetype=2) +
  geom_hline(yintercept=1, col="red") +
  geom_smooth(method="lm", se=FALSE) +
  # geom_text(aes(label = province, color=color),
  #                          show.legend = FALSE) +
  ggrepel::geom_text_repel(aes(label = province, color=color),
                           show.legend = FALSE) +
  scale_color_manual(values=c("black", "red"))+
  annotate(geom = 'text', 
           x = max(plot_df$movement_ratio_ratio), 
           y = max(plot_df$tweet_ratio_ratio), 
           hjust = 1,
           vjust = 1,
           label = as.character(corr_label)) +
  xlab("Within City Movement (Relative Change)") +
  ylab("Geolocated Twitter Users (Relative Change)") +
  labs(title = "Reduced Mobility vs. Twitter Users (2nd Week)",
       subtitle = "Relative Change (Ratios) 2nd Week After Wuhan Lockdown, Adjusting for Same Period in 2019") +
  theme_classic() + 
  theme(strip.background = element_blank(),
        plot.title = element_text(size=16, hjust = 0.5))
dev.off()


### Figure A4 - 3


tweet_before_after_ratio = merge(
  tweet_combined %>%
    filter(tweet_date <= as.Date("2020-01-21") &
             tweet_date >= as.Date("2020-01-15")) %>%
    group_by(province) %>%
    summarize(before_sum_2020=sum(count),
              before_sum_2019=sum(count_2019),
              before_sum_ratio=sum(count)/sum(count_2019)),
  tweet_combined %>%
    filter(tweet_date >= as.Date("2020-02-06") &
             tweet_date <= as.Date("2020-02-12")) %>%
    group_by(province) %>%
    summarize(after_sum_2020=sum(count),
              after_sum_2019=sum(count_2019),
              after_sum_ratio=sum(count)/sum(count_2019)),
  by = "province") %>%
  mutate(tweet_ratio_ratio = after_sum_ratio / before_sum_ratio)

movement_before_after_ratio = merge(
  movement_combined %>%
    filter(date <= as.Date("2020-01-21") &
             date >= as.Date("2020-01-15")) %>%
    group_by(Prov_EN) %>%
    summarize(before_sum_2020=sum(index_movement),
              before_sum_2019=sum(index_2019),
              before_sum_ratio=sum(index_movement)/sum(index_2019)),
  movement_combined %>%
    filter(date >= as.Date("2020-02-06") &
             date <= as.Date("2020-02-12")) %>%
    group_by(Prov_EN) %>%
    summarize(after_sum_2020=sum(index_movement),
              after_sum_2019=sum(index_2019),
              after_sum_ratio=sum(index_movement)/sum(index_2019)),
  by = "Prov_EN") %>%
  mutate(movement_ratio_ratio = after_sum_ratio / before_sum_ratio)


plot_df = merge(tweet_before_after_ratio %>% 
                  #filter(before_sum_2019 > 35) %>%
                  select(province, tweet_ratio_ratio),
                movement_before_after_ratio %>%
                  select(Prov_EN, movement_ratio_ratio),
                by.x = c("province"), by.y = c("Prov_EN")) %>%
  mutate(color = ifelse(province=="Hubei", "red", "black"))


pdf(here("twitter/figs/FigA4_3_twitter_v_mobility_ratio_2019_three_week.pdf"), width = 7, height = 5)
ggplot(plot_df, aes(x = movement_ratio_ratio, y = tweet_ratio_ratio)) +
  #geom_vline(xintercept=1, col="red", linetype=2) +
  geom_hline(yintercept=1, col="red") +
  #geom_smooth(method="lm", se=FALSE) +
  # geom_text(aes(label = province, color=color),
  #                          show.legend = FALSE) +
  ggrepel::geom_text_repel(aes(label = province, color=color),
                           show.legend = FALSE) +
  scale_color_manual(values=c("black", "red"))+
  xlab("Within City Movement (Relative Change)") +
  ylab("Geolocated Twitter Users (Relative Change)") +
  labs(title = "Reduced Mobility vs. Twitter Users (3rd Week)",
       subtitle = "Relative Change (Ratios) 3rd Week After Wuhan Lockdown, Adjusting for Same Period in 2019") +
  theme_classic() + 
  theme(strip.background = element_blank(),
        plot.title = element_text(size=16, hjust = 0.5))
dev.off()


### Figure A4 - 4


tweet_before_after_ratio = merge(
  tweet_combined %>%
    filter(tweet_date <= as.Date("2020-01-21") &
             tweet_date >= as.Date("2020-01-15")) %>%
    group_by(province) %>%
    summarize(before_sum_2020=sum(count),
              before_sum_2019=sum(count_2019),
              before_sum_ratio=sum(count)/sum(count_2019)),
  tweet_combined %>%
    filter(tweet_date >= as.Date("2020-02-13") &
             tweet_date <= as.Date("2020-02-19")) %>%
    group_by(province) %>%
    summarize(after_sum_2020=sum(count),
              after_sum_2019=sum(count_2019),
              after_sum_ratio=sum(count)/sum(count_2019)),
  by = "province") %>%
  mutate(tweet_ratio_ratio = after_sum_ratio / before_sum_ratio)

movement_before_after_ratio = merge(
  movement_combined %>%
    filter(date <= as.Date("2020-01-21") &
             date >= as.Date("2020-01-15")) %>%
    group_by(Prov_EN) %>%
    summarize(before_sum_2020=sum(index_movement),
              before_sum_2019=sum(index_2019),
              before_sum_ratio=sum(index_movement)/sum(index_2019)),
  movement_combined %>%
    filter(date >= as.Date("2020-02-13") &
             date <= as.Date("2020-02-19")) %>%
    group_by(Prov_EN) %>%
    summarize(after_sum_2020=sum(index_movement),
              after_sum_2019=sum(index_2019),
              after_sum_ratio=sum(index_movement)/sum(index_2019)),
  by = "Prov_EN") %>%
  mutate(movement_ratio_ratio = after_sum_ratio / before_sum_ratio)


plot_df = merge(tweet_before_after_ratio %>% 
                  #filter(before_sum_2019 > 35) %>%
                  select(province, tweet_ratio_ratio),
                movement_before_after_ratio %>%
                  select(Prov_EN, movement_ratio_ratio),
                by.x = c("province"), by.y = c("Prov_EN")) %>%
  mutate(color = ifelse(province=="Hubei", "red", "black"))


pdf(here("twitter/figs/FigA4_4_twitter_v_mobility_ratio_2019_four_week.pdf"), width = 7, height = 5)
ggplot(plot_df, aes(x = movement_ratio_ratio, y = tweet_ratio_ratio)) +
  #geom_vline(xintercept=1, col="red", linetype=2) +
  geom_hline(yintercept=1, col="red") +
  #geom_smooth(method="lm", se=FALSE) +
  # geom_text(aes(label = province, color=color),
  #                          show.legend = FALSE) +
  ggrepel::geom_text_repel(aes(label = province, color=color),
                           show.legend = FALSE) +
  scale_color_manual(values=c("black", "red"))+
  xlab("Within City Movement (Relative Change)") +
  ylab("Geolocated Twitter Users (Relative Change)") +
  labs(title = "Reduced Mobility vs. Twitter Users (4th Week)",
       subtitle = "Relative Change (Ratios) 4th Week After Wuhan Lockdown, Adjusting for Same Period in 2019") +
  theme_classic() + 
  theme(strip.background = element_blank(),
        plot.title = element_text(size=16, hjust = 0.5))
dev.off()





### Figure A4, bottom panel

df_2019 <- read.csv(here('twitter/data/figA4_bottompanel_2019.csv'), stringsAsFactors=FALSE)
df_2020 <- read.csv(here('twitter/data/figA4_bottompanel_2020.csv'), stringsAsFactors=FALSE)

tweet_combined = merge(
    df_2020,
    df_2019,
    by=c("tweet_date")
)

agg_tweets_cny <- tweet_combined %>%
    ## group_by(tweet_date) %>%
    mutate(
        ## count = sum(count), count_2019 = sum(count_2019),
        before_dates = tweet_date >= "2020-01-15" & tweet_date <= "2020-01-21"
    ) %>% ## ungroup() %>%
    mutate(
        before_ratio = sum(count[before_dates]) / sum(count_2019[before_dates])
    ) %>% filter(tweet_date >= "2020-01-16" & tweet_date <= "2020-01-29")

pdf(here("twitter/figs/FigA4_bottom_twitter_compare_2020_2019_absolute_one_week.pdf"), height=3.5, width=4.25)
with(
    agg_tweets_cny,
    plot(
        as.Date(tweet_date), count,
        type="l", bty="n",
        main="Geo-located Twitter Users by Day\n2020 versus 2019 aligned by lunar calendar",
        ylab="Geolocated Twitter Users",
        xlab="Date", ylim=c(600, 1500), cex.lab=1.3,
        cex.main=1
    )
)
with(
    agg_tweets_cny,
    lines(
        as.Date(tweet_date), count_2019,
        lty=3
    )
)
abline(v=as.Date("2020-01-25"), lty=2)
abline(h=1, col="red", lty=2)
mtext("Lunar New Year (day of)", at=as.Date("2020-01-25"))
text(x=as.Date("2020-01-27"), y=1300, labels="2020")
text(x=as.Date("2020-01-27"), y=640, labels="2019")
dev.off()
