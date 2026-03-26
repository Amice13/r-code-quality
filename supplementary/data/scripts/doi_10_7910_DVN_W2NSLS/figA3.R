############################################################################################
#### Fig A3: Reduction in within city movement and increase in geolocated Twitter users 
#### during the month of Wuhan lockdown (left); degree of moving out and increase in 
#### geolocated Twitter users on the day of Wuhan lockdown (right).
#### data: twitter/data/a_CN_2019-12-01_2020-07-02_agg_fig4.csv
####       twitter/data/Index_City_CH_EN.csv
####       twitter/data/baidu_within_city_movement.xlsx
####       twitter/data/baidu_move_in_index.csv
####       twitter/data/baidu_move_out_index.csv
#### outputs: twitter/figs/FigA3_left_twitter_30days_later_v_mobility_one_month.pdf
####          twitter/figs/FigA3_right_twitter_daily_jump_v_move_out_lockdown.pdf
############################################################################################

library("tidyverse"); library("RColorBrewer"); library("here")

df = read_csv(here("twitter/data/a_CN_2019-12-01_2020-07-02_agg_fig4.csv"))
cities = read_csv(here("twitter/data/Index_City_CH_EN.csv"))
movement = readxl::read_excel(here("twitter/data/baidu_within_city_movement.xlsx"))
move_in = read_csv(here("twitter/data/baidu_move_in_index.csv"))
move_out = read_csv(here("twitter/data/baidu_move_out_index.csv"))

#### Fig A3 Left Panel

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
movement_diff_df = movement %>%
  mutate(GbProv = substr(str_extract(GbCity, "[^_]+$"), 1, 2)) %>%
  mutate(date = as.Date(date, format = "%Y%m%d")) %>%
  filter(date == as.Date("2020-01-22") |
           date == as.Date("2020-02-22")) %>%
  group_by(GbProv, GbCity) %>%
  summarize(movement_diff_one_month = index_movement[2] - index_movement[1]) %>%
  group_by(GbProv) %>%
  summarize(mean_movement_diff_one_month = mean(movement_diff_one_month)) %>%
  merge(provinces, by = c("GbProv")) %>%
  select("Prov_EN", "Prov_CH", "GbProv", "mean_movement_diff_one_month")
# consistent with twitter jump figure
movement_diff_df = movement_diff_df %>%
  mutate(Prov_EN = relevel(factor(Prov_EN), ref="Guangdong")) %>%
  filter(Prov_EN != "Chongqing")

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

predicted_df = merge(predicted_df, movement_diff_df,
                     by.x = c("province"), by.y = c("Prov_EN"))

pdf(here("twitter/figs/FigA3_left_twitter_30days_later_v_mobility_one_month.pdf"), width = 7, height = 4)
ggplot() +
  geom_text(
    aes(
      x = -subset(predicted_df, near(day, 1))$mean_movement_diff_one_month,
      y = log(subset(predicted_df, near(day, 1))$pred / subset(predicted_df, near(day, -1/30))$pred),
      label = subset(predicted_df, near(day, 1/30))$province,
      size = (subset(predicted_df, day > 0) %>% group_by(province) %>% summarise(thecount = mean(count)))$thecount
      )
  ) +
  geom_text(
    aes(
      x = -subset(predicted_df, province=="Hubei" & near(day, 1))$mean_movement_diff_one_month,
      y = log(subset(predicted_df, province=="Hubei" & near(day, 1))$pred / subset(predicted_df, province=="Hubei" & near(day, -1/30))$pred),
      label = subset(predicted_df, province=="Hubei" & near(day, 1/30))$province,
      size = (subset(predicted_df, province=="Hubei" & day > 0) %>% group_by(province) %>% summarise(thecount = mean(count)))$thecount,
      color = "red"
    ),
    show.legend=FALSE
  ) +
  ## geom_abline(
  ##     slope = -0.5, intercept = 0
  ## ) +
  geom_smooth(
    aes(
      x = -subset(predicted_df, near(day, 1))$mean_movement_diff_one_month,
      y = log(subset(predicted_df, near(day, 1))$pred / subset(predicted_df, near(day, -1/30))$pred)
    ),
    method = "lm", se = FALSE) +
  ## geom_abline(
  ##     slope = -0.5, intercept = 0
  ## ) +
  #scale_x_continuous(label = exp, breaks=log(seq(0.1, 3, 0.4)), limits=c(log(0.6), log(3.6))) +
  scale_y_continuous(label = exp, breaks=log(seq(0.1, 3, 0.1)), limits=c(log(0.9), log(2))) +
  xlab("Decrease in Within City Movement Index (Feb 22 - Jan 22, Average across Cities)") +
  ylab("Jump in Geo-located Users after 30 days") +
  guides(size=guide_legend(title="Average Number of \nUsers Geo-Locating\nEach Day After\nWuhan Lockdown")) +
  theme_classic() + geom_hline(yintercept=0, col="red", linetype=2)
dev.off()


#### Fig A3 Right Panel

move_in = move_in %>%
  filter(stringr::str_sub(GbCity_EN, -4, -1)=="0000") %>%
  # get province ids
  mutate(GbProv = substr(str_extract(GbCity_EN, "[^_]+$"), 1, 2)) %>%
  select(-GbCity_EN) %>%
  pivot_longer(!GbProv, names_to = "date", values_to = "index_in")

move_out = move_out %>%
  filter(stringr::str_sub(GbCity_EN, -4, -1)=="0000") %>%
  # get province ids
  mutate(GbProv = substr(str_extract(GbCity_EN, "[^_]+$"), 1, 2)) %>%
  select(-GbCity_EN) %>%
  pivot_longer(!GbProv, names_to = "date", values_to = "index_out")

move_province_df = merge(move_in, move_out, by = c("GbProv", "date")) %>%
  merge(provinces, by = c("GbProv")) %>%
  # process date string to date format
  mutate(date = as.Date(date, format = "%Y%m%d")) %>%
  # reorder columns
  select("Prov_EN", "Prov_CH", "GbProv", "date", "index_in", "index_out") %>%
  # create net move in
  mutate(index_net_in = index_in - index_out)

# consistent with titter jump figure
move_province_df = move_province_df %>%
  mutate(Prov_EN = relevel(factor(Prov_EN), ref="Guangdong")) %>%
  filter(Prov_EN != "Chongqing")

move_df_plot = move_province_df %>%
  filter(date == as.Date("2020-01-23"))

predicted_df = data.frame(pred = predict(fit, df_supp, interval = "confidence")[, 1],
                          pred_lower = predict(fit, df_supp, interval = "confidence")[, 2],
                          pred_upper = predict(fit, df_supp, interval = "confidence")[, 3],
                          count = df_supp$count,
                          day=df_supp$day,
                          province = df_supp$province) %>%
  arrange(province, day)

predicted_df = merge(predicted_df, move_df_plot,
                     by.x = c("province"), by.y = c("Prov_EN"))

pdf(here("twitter/figs/FigA3_right_twitter_daily_jump_v_move_out_lockdown.pdf"), width = 7, height = 4)
ggplot() +
  geom_text(
    aes(
      x = log(subset(predicted_df, near(day, 1/30))$pred / subset(predicted_df, near(day, -1/30))$pred),
      y = subset(predicted_df, near(day, 3))$index_out,
      label = subset(predicted_df, near(day, 1/30))$province,
      size = (subset(predicted_df, day > 0) %>% group_by(province) %>% summarise(thecount = mean(count)))$thecount)
  ) +
  geom_text(
    aes(
      x = log(subset(predicted_df, province=="Hubei" & near(day, 1/30))$pred / subset(predicted_df, province=="Hubei" & near(day, -1/30))$pred),
      y = subset(predicted_df, province=="Hubei" & near(day, 3))$index_out,
      label = subset(predicted_df, province=="Hubei" & near(day, 1/30))$province,
      size = (subset(predicted_df, province=="Hubei" & day > 0) %>% group_by(province) %>% summarise(thecount = mean(count)))$thecount,
      color = "red"
    ),
    show.legend=FALSE
  ) +
  ## geom_abline(
  ##     slope = -0.5, intercept = 0
  ## ) +
  geom_smooth(
    aes(
      x = log(subset(predicted_df, near(day, 1/30))$pred / subset(predicted_df, near(day, -1/30))$pred),
      y = subset(predicted_df, near(day, 3))$index_out
    ),
    method = "lm", se = FALSE) +
  ## geom_abline(
  ##     slope = -0.5, intercept = 0
  ## ) +
  scale_x_continuous(label = exp, breaks=log(seq(0.1, 3, 0.4)), limits=c(log(0.6), log(3.6))) +
  #scale_y_continuous(label = exp, breaks=log(seq(0.1, 3, 0.1)), limits=c(log(0.6), log(2))) +
  xlab("Jump in Geo-located Tweets on Jan 23") +
  ylab("Moving Out Index on Jan 23") +
  guides(size=guide_legend(title="Average Number of \nUsers Geo-Locating\nEach Day After\nWuhan Lockdown")) +
  theme_classic() + geom_vline(xintercept=0, col="red", linetype=2) +
  coord_flip()
dev.off()

