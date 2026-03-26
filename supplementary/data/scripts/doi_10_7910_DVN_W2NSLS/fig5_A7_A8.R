############################################################################################
#### Fig 5: Increases in Twitter Followers from China vs Hong Kong By Category
#### Fig A7: Increases in Twitter Followers from China versus Taiwan
#### Fig A8: Increases in Twitter Followers from China versus US
#### data: twitter/data/fig5_daily_follower_china_relative_hk.csv
####       twitter/data/figA7_daily_follower_china_relative_tw.csv
####       twitter/data/figA7_daily_follower_china_relative_us.csv
#### outputs: twitter/figs/Fig5_rel_dec_diff_log_six_category_china_vs_hk_zh.pdf
####          twitter/figs/FigA7_rel_dec_diff_log_six_category_china_vs_tw_zh.pdf
####          twitter/figs/FigA8_rel_dec_diff_log_six_category_china_vs_us_zh.pdf
############################################################################################

library("tidyverse"); library("RColorBrewer"); library("data.table"); library("here")

ReorderAccountType = function(df) {
  df = df %>%
    mutate(type_complete = fct_relevel(type_complete,
                                       "International News Agencies",
                                       "Citizen Journalists / Political Bloggers",
                                       "Activists or US / Taiwan / Hong Kong Politics",
                                       "Pornography Accounts",
                                       "State Media or Chinese Officials",
                                       "Non-Political Bloggers or Entertainment Accounts"))
  return(df)
}

agg_diff_hk = read_csv(here("twitter/data/fig5_daily_follower_china_relative_hk.csv")) %>%
  ReorderAccountType(.)
agg_diff_tw = read_csv(here("twitter/data/figA7_daily_follower_china_relative_tw.csv")) %>%
  ReorderAccountType(.)
agg_diff_us = read_csv(here("twitter/data/figA7_daily_follower_china_relative_us.csv")) %>%
  ReorderAccountType(.)

## Below is vestigial from when aggregation happened here.
# 
# files = list.files('Data/top_followers_fastCrawl_aggs', full.names=TRUE) %>%
#   .[grep('followers_count_zh', .)]
# files = files[files!="Data/top_followers_fastCrawl/followers_count_by_location_day.csv"]
# pageids = str_split_fixed(files, "_|/", 7)[, 6]
# df_by_day = lapply(files, function(x) {read_csv(x, col_types = "cDcd")}) %>%
#   data.table::rbindlist(.) %>%
#   complete(user_id, follow_date, location_classified, fill = list(followers = 0)) %>%
#   filter(follow_date <= as.Date("2020-04-30"))
# accounts = read_csv("Data/CN_popular_account_list.csv", col_types = "cnccccncccncnncc")
# length(unique(accounts$id_str))
# length(unique(df_by_day$user_id))
# 
# df_by_day = df_by_day %>%
#   inner_join(accounts, by = c("user_id" = "id_str"))
# 
# agg_diff_hk = df_by_day %>%
#   mutate(location_classified=case_when(
#     location_classified=="Other Location But Not NA"~"Other Locations",
#     TRUE ~ location_classified)) %>%
#   filter(location_classified=="China" | location_classified=="Hong Kong") %>%
#   filter(follow_date >= "2019-12-01" & follow_date < "2020-04-01") %>%
#   filter(zh == 1) %>%
#   filter(location_classified!="Unknown") %>%
#   mutate(type_complete = fct_relevel(type_complete,
#                                      "International News Agencies",
#                                      "Citizen Journalists / Political Bloggers",
#                                      "Activists or US / Taiwan / Hong Kong Politics",
#                                      "Pornography Accounts",
#                                      "State Media or Chinese Officials",
#                                      "Non-Political Bloggers or Entertainment Accounts")) %>%
#   group_by(location_classified, follow_date, type_complete) %>%
#   summarize(followers=sum(followers)) %>%
#   ungroup() %>%
#   complete(type_complete, follow_date, location_classified, fill = list(followers = 0)) %>%
#   group_by(location_classified, type_complete) %>%
#   mutate(dec_mean = mean(followers[follow_date<=as.Date("2019-12-31")], na.rm=T)) %>%
#   mutate(diff = followers - dec_mean,
#          percentage = (followers - dec_mean)/dec_mean,
#          rel_dec = followers/dec_mean,
#          rel_dec_adj = ifelse(followers>0, followers/dec_mean, 0.001/dec_mean)) %>%
#   mutate(followers_ma3 = zoo::rollmean(followers, k=3, fill = NA),
#          diff_ma3 = zoo::rollmean(diff, k=3, fill = NA),
#          percentage_ma3 = zoo::rollmean(percentage, k=3, fill = NA),
#          rel_dec_ma3 = zoo::rollmean(rel_dec, k=3, fill = NA)) %>%
#   select(type_complete, follow_date, location_classified, diff, percentage, rel_dec, rel_dec_adj) %>%
#   group_by(type_complete, follow_date) %>%
#   arrange(type_complete, follow_date, desc(location_classified)) %>%
#   mutate(diff_diff = diff - lag(diff),
#          percentage_diff = percentage - lag(percentage),
#          rel_dec_diff = rel_dec - lag(rel_dec),
#          rel_dec_log_diff = log(rel_dec) - log(lag(rel_dec)),
#          rel_dec_ratio = rel_dec/lag(rel_dec),) %>%
#   filter(location_classified == "China") %>%
#   select(type_complete, follow_date, diff_diff, percentage_diff, rel_dec_diff, rel_dec_log_diff, rel_dec_ratio) %>%
#   group_by(type_complete) %>%
#   arrange(type_complete, follow_date)
# 
# write_csv(agg_diff_hk, here("twitter/data/fig5_daily_follower_china_relative_hk.csv"))
# 
# agg_diff_tw = df_by_day %>%
#   mutate(location_classified=case_when(
#     location_classified=="Other Location But Not NA"~"Other Locations",
#     TRUE ~ location_classified)) %>%
#   filter(location_classified=="China" | location_classified=="Taiwan") %>%
#   filter(follow_date >= "2019-12-01" & follow_date < "2020-04-01") %>%
#   filter(zh == 1) %>%
#   filter(location_classified!="Unknown") %>%
#   mutate(type_complete = fct_relevel(type_complete,
#                                      "International News Agencies",
#                                      "Citizen Journalists / Political Bloggers",
#                                      "Activists or US / Taiwan / Hong Kong Politics",
#                                      "Pornography Accounts",
#                                      "State Media or Chinese Officials",
#                                      "Non-Political Bloggers or Entertainment Accounts")) %>%
#   group_by(location_classified, follow_date, type_complete) %>%
#   summarize(followers=sum(followers)) %>%
#   ungroup() %>%
#   complete(type_complete, follow_date, location_classified, fill = list(followers = 0)) %>%
#   group_by(location_classified, type_complete) %>%
#   mutate(dec_mean = mean(followers[follow_date<=as.Date("2019-12-31")], na.rm=T)) %>%
#   mutate(diff = followers - dec_mean,
#          percentage = (followers - dec_mean)/dec_mean,
#          rel_dec = followers/dec_mean,
#          rel_dec_adj = ifelse(followers>0, followers/dec_mean, 0.001/dec_mean)) %>%
#   mutate(followers_ma3 = zoo::rollmean(followers, k=3, fill = NA),
#          diff_ma3 = zoo::rollmean(diff, k=3, fill = NA),
#          percentage_ma3 = zoo::rollmean(percentage, k=3, fill = NA),
#          rel_dec_ma3 = zoo::rollmean(rel_dec, k=3, fill = NA)) %>%
#   select(type_complete, follow_date, location_classified, diff, percentage, rel_dec, rel_dec_adj) %>%
#   group_by(type_complete, follow_date) %>%
#   arrange(type_complete, follow_date, desc(location_classified)) %>%
#   mutate(diff_diff = diff - lag(diff),
#          percentage_diff = percentage - lag(percentage),
#          rel_dec_diff = rel_dec - lag(rel_dec),
#          rel_dec_log_diff = log(rel_dec) - log(lag(rel_dec)),
#          rel_dec_ratio = rel_dec/lag(rel_dec),) %>%
#   filter(location_classified == "China") %>%
#   select(type_complete, follow_date, diff_diff, percentage_diff, rel_dec_diff, rel_dec_log_diff, rel_dec_ratio) %>%
#   group_by(type_complete) %>%
#   arrange(type_complete, follow_date)
# 
# write_csv(agg_diff_tw, here("twitter/data/figA7_daily_follower_china_relative_tw.csv"))
# 
# agg_diff_us = df_by_day %>%
#   mutate(location_classified=case_when(
#     location_classified=="Other Location But Not NA"~"Other Locations",
#     TRUE ~ location_classified)) %>%
#   filter(location_classified=="China" | location_classified=="US") %>%
#   filter(follow_date >= "2019-12-01" & follow_date < "2020-04-01") %>%
#   filter(zh == 1) %>%
#   filter(location_classified!="Unknown") %>%
#   mutate(type_complete = fct_relevel(type_complete,
#                                      "International News Agencies",
#                                      "Citizen Journalists / Political Bloggers",
#                                      "Activists or US / Taiwan / Hong Kong Politics",
#                                      "Pornography Accounts",
#                                      "State Media or Chinese Officials",
#                                      "Non-Political Bloggers or Entertainment Accounts")) %>%
#   group_by(location_classified, follow_date, type_complete) %>%
#   summarize(followers=sum(followers)) %>%
#   ungroup() %>%
#   complete(type_complete, follow_date, location_classified, fill = list(followers = 0)) %>%
#   group_by(location_classified, type_complete) %>%
#   mutate(dec_mean = mean(followers[follow_date<=as.Date("2019-12-31")], na.rm=T)) %>%
#   mutate(diff = followers - dec_mean,
#          percentage = (followers - dec_mean)/dec_mean,
#          rel_dec = followers/dec_mean,
#          rel_dec_adj = ifelse(followers>0, followers/dec_mean, 0.001/dec_mean)) %>%
#   mutate(followers_ma3 = zoo::rollmean(followers, k=3, fill = NA),
#          diff_ma3 = zoo::rollmean(diff, k=3, fill = NA),
#          percentage_ma3 = zoo::rollmean(percentage, k=3, fill = NA),
#          rel_dec_ma3 = zoo::rollmean(rel_dec, k=3, fill = NA)) %>%
#   select(type_complete, follow_date, location_classified, diff, percentage, rel_dec, rel_dec_adj) %>%
#   group_by(type_complete, follow_date) %>%
#   arrange(type_complete, follow_date, desc(location_classified)) %>%
#   mutate(diff_diff = diff - lag(diff),
#          percentage_diff = percentage - lag(percentage),
#          rel_dec_diff = rel_dec - lag(rel_dec),
#          rel_dec_log_diff = log(rel_dec) - log(lag(rel_dec)),
#          rel_dec_ratio = rel_dec/lag(rel_dec),) %>%
#   filter(location_classified == "China") %>%
#   select(type_complete, follow_date, diff_diff, percentage_diff, rel_dec_diff, rel_dec_log_diff, rel_dec_ratio) %>%
#   group_by(type_complete) %>%
#   arrange(type_complete, follow_date)
# 
# write_csv(agg_diff_us, here("twitter/data/figA7_daily_follower_china_relative_us.csv"))



### Fig 5

post_means = agg_diff_hk %>%
  group_by(type_complete) %>%
  summarize(post_mean = mean(rel_dec_log_diff[follow_date>as.Date("2020-01-23") &
                                                follow_date<as.Date("2020-03-13")])) %>%
  mutate(post_mean_exp = exp(post_mean))

p = agg_diff_hk %>%
  ggplot(aes(x=follow_date, y=rel_dec_log_diff)) +
  geom_rect(aes(xmin=as.Date("2020-01-23"), xmax=as.Date("2020-03-13"),
                ymin=-Inf, ymax=Inf),
            fill = "grey95", colour = "white", alpha = 0.2) +
  geom_vline(xintercept=as.Date(c("2020-01-23")), color = "black", lwd=.5) +
  geom_vline(xintercept=as.Date(c("2020-03-13")), color = "black", linetype = "dashed", lwd=.5) +
  scale_x_date(breaks = as.Date(c("2019-12-01", "2020-04-01",
                                  "2020-01-23", "2020-03-13")),
               labels = c("Dec\n2019", "Apr\n2020",
                          "Wuhan\nLockdown",
                          "First Hubei\nLockdown\nRemoval")) +
  geom_hline(yintercept=0, color = "black", lwd=.5) +
  geom_smooth(span = 0.15, se = TRUE) +
  #stat_summary_bin(fun='mean', bins=50, color='black', size=.75, geom='point') +
  geom_point(size = 0.8) +
  theme_classic(base_size = 14) +
  facet_wrap(~ type_complete, ncol = 2, scales = "free") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(size = c(9, 9, 8, 8))) +
  labs(title=paste0("New Followers Compared to Baseline, China / ", "Hong Kong")
  ) +
  scale_y_continuous(breaks=log(c(0.4, 0.7, 1, 2, 3)),
                     labels=c("0.4x", "0.7x", "1x", "2x", "3x"),
                     limits=log(c(0.3, 3))) +
  geom_segment(data=post_means,
               aes(x=as.Date("2020-01-23"),
                   xend=as.Date("2020-03-13"),
                   y=post_mean,
                   yend=post_mean), color = "#e41a1c", lwd=1) +
  geom_text(data=post_means,
            aes(label=paste0(round(post_mean_exp, 2), "x")),
            x = as.Date("2020-03-01"),
            y = log(2.8), color = "#e41a1c")


pdf(here("twitter/figs/Fig5_rel_dec_diff_log_six_category_china_vs_hk_zh.pdf"), width = 8, height = 10)
print(p)
dev.off()



### Fig A7


post_means = agg_diff_tw %>%
  group_by(type_complete) %>%
  summarize(post_mean = mean(rel_dec_log_diff[follow_date>as.Date("2020-01-23") &
                                                follow_date<as.Date("2020-03-13")])) %>%
  mutate(post_mean_exp = exp(post_mean))

p = agg_diff_tw %>%
  ggplot(aes(x=follow_date, y=rel_dec_log_diff)) +
  geom_rect(aes(xmin=as.Date("2020-01-23"), xmax=as.Date("2020-03-13"),
                ymin=-Inf, ymax=Inf),
            fill = "grey95", colour = "white", alpha = 0.2) +
  geom_vline(xintercept=as.Date(c("2020-01-23")), color = "black", lwd=.5) +
  geom_vline(xintercept=as.Date(c("2020-03-13")), color = "black", linetype = "dashed", lwd=.5) +
  scale_x_date(breaks = as.Date(c("2019-12-01", "2020-04-01",
                                  "2020-01-23", "2020-03-13")),
               labels = c("Dec\n2019", "Apr\n2020",
                          "Wuhan\nLockdown",
                          "First Hubei\nLockdown\nRemoval")) +
  geom_hline(yintercept=0, color = "black", lwd=.5) +
  geom_smooth(span = 0.15, se = TRUE) +
  #stat_summary_bin(fun='mean', bins=50, color='black', size=.75, geom='point') +
  geom_point(size = 0.8) +
  theme_classic(base_size = 14) +
  facet_wrap(~ type_complete, ncol = 2, scales = "free") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(size = c(9, 9, 8, 8))) +
  labs(title=paste0("New Followers Compared to Baseline, China / ", "Taiwan")
  ) +
  scale_y_continuous(breaks=log(c(0.4, 0.7, 1, 2, 3)),
                     labels=c("0.4x", "0.7x", "1x", "2x", "3x"),
                     limits=log(c(0.3, 3))) +
  geom_segment(data=post_means,
               aes(x=as.Date("2020-01-23"),
                   xend=as.Date("2020-03-13"),
                   y=post_mean,
                   yend=post_mean), color = "#e41a1c", lwd=1) +
  geom_text(data=post_means,
            aes(label=paste0(round(post_mean_exp, 2), "x")),
            x = as.Date("2020-03-01"),
            y = log(2.8), color = "#e41a1c")


pdf(here("twitter/figs/FigA7_rel_dec_diff_log_six_category_china_vs_tw_zh.pdf"), width = 8, height = 10)
print(p)
dev.off()



### Fig A8


post_means = agg_diff_us %>%
  group_by(type_complete) %>%
  summarize(post_mean = mean(rel_dec_log_diff[follow_date>as.Date("2020-01-23") &
                                                follow_date<as.Date("2020-03-13")])) %>%
  mutate(post_mean_exp = exp(post_mean))

p = agg_diff_us %>%
  ggplot(aes(x=follow_date, y=rel_dec_log_diff)) +
  geom_rect(aes(xmin=as.Date("2020-01-23"), xmax=as.Date("2020-03-13"),
                ymin=-Inf, ymax=Inf),
            fill = "grey95", colour = "white", alpha = 0.2) +
  geom_vline(xintercept=as.Date(c("2020-01-23")), color = "black", lwd=.5) +
  geom_vline(xintercept=as.Date(c("2020-03-13")), color = "black", linetype = "dashed", lwd=.5) +
  scale_x_date(breaks = as.Date(c("2019-12-01", "2020-04-01",
                                  "2020-01-23", "2020-03-13")),
               labels = c("Dec\n2019", "Apr\n2020",
                          "Wuhan\nLockdown",
                          "First Hubei\nLockdown\nRemoval")) +
  geom_hline(yintercept=0, color = "black", lwd=.5) +
  geom_smooth(span = 0.15, se = TRUE) +
  #stat_summary_bin(fun='mean', bins=50, color='black', size=.75, geom='point') +
  geom_point(size = 0.8) +
  theme_classic(base_size = 14) +
  facet_wrap(~ type_complete, ncol = 2, scales = "free") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(size = c(9, 9, 8, 8))) +
  labs(title=paste0("New Followers Compared to Baseline, China / ", "US")
  ) +
  scale_y_continuous(breaks=log(c(0.4, 0.7, 1, 2, 3)),
                     labels=c("0.4x", "0.7x", "1x", "2x", "3x"),
                     limits=log(c(0.3, 3))) +
  geom_segment(data=post_means,
               aes(x=as.Date("2020-01-23"),
                   xend=as.Date("2020-03-13"),
                   y=post_mean,
                   yend=post_mean), color = "#e41a1c", lwd=1) +
  geom_text(data=post_means,
            aes(label=paste0(round(post_mean_exp, 2), "x")),
            x = as.Date("2020-03-01"),
            y = log(2.8), color = "#e41a1c")


pdf(here("twitter/figs/FigA8_rel_dec_diff_log_six_category_china_vs_us_zh.pdf"), width = 8, height = 10)
print(p)
dev.off()

