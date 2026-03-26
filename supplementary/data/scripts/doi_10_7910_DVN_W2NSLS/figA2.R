############################################################################################
#### Fig A2: Within city movement index by Province (black: 2020, dotted: same period in 2019)
#### data: twitter/data/baidu_within_city_movement.xlsx
####       twitter/data/Index_City_CH_EN.csv
#### outputs: twitter/figs/FigA2_baidu_movement_by_province.pdf
############################################################################################

library("tidyverse"); library("RColorBrewer"); library("here")

movement = readxl::read_excel(here("twitter/data/baidu_within_city_movement.xlsx"))
cities = read_csv(here("twitter/data/Index_City_CH_EN.csv"))

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
movement_df = movement %>%
  # get province ids
  mutate(GbProv = substr(GbCity, 1, 2)) %>%
  # aggregate to provincial level
  group_by(GbProv, date) %>%
  summarize(index_movement = mean(index_movement)) %>%
  # merge with province info
  merge(provinces, by = c("GbProv")) %>%
  # process date string to date format
  mutate(date = as.Date(date, format = "%Y%m%d")) %>%
  # reorder columns
  select("Prov_EN", "Prov_CH", "GbProv", "date", "index_movement")
movement_2019 = movement_df %>%
  filter(date <= as.Date("2019-03-28")) %>%
  mutate(date = date + difftime(as.Date("2020-01-25"), # Chinese new year in 2020
                                as.Date("2019-02-05"), # Chinese new year in 2019
                                units=c("days"))) %>%
  rename(index_2019 = index_movement)
movement_2020 = movement_df %>%
  filter(date <= as.Date("2020-03-16") &
           date >= as.Date("2020-01-01"))
# consistent with twitter jump figure
movement_combined = merge(movement_2020, movement_2019,
                          by=c("Prov_EN", "Prov_CH", "GbProv", "date")) %>%
  mutate(Prov_EN = relevel(factor(Prov_EN), ref="Guangdong")) %>%
  filter(Prov_EN != "Chongqing") %>%
  mutate(index_movement_normalized = index_movement/index_2019)


pdf(here("twitter/figs/FigA2_baidu_movement_by_province.pdf"), width = 15, height = 10)
movement_combined %>%
  filter(date <= as.Date("2020-04-01")) %>%
  ggplot() +
  #geom_line(aes(x=date, y=index_in), color='blue', lwd=1) +
  #geom_line(aes(x=date, y=index_out), color='red', lwd=1) +
  geom_vline(xintercept = as.Date("2020-01-24"), color = "red", lwd=1) +
  geom_line(aes(x=date, y=index_2019), color='grey40', linetype = "dotted", lwd=1) +
  geom_line(aes(x=date, y=index_movement), color='black', lwd=1) +
  #geom_point(shape=1, size = 0.5) +
  facet_wrap(~Prov_EN, ncol=5, scales = "free") +
  #facet_wrap(~Prov_EN, ncol=5) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 18),
        axis.text=element_text(size=12),
        axis.title=element_text(size=20, face="bold"),
        plot.title = element_text(size=22, hjust = 0.5)) +
  ylab("") +
  xlab("") +
  ggtitle("Within City Movement Index (Black: 2020, Dotted: 2019, Dates Adjust for Lunar New Year)")
dev.off()

