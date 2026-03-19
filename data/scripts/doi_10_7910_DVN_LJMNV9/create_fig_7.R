# This R-file is generating the Figure 7

library(tidyverse)
library(data.table)
library(haven)
library(estimatr)
library(latex2exp)
library(plyr)
require(RColorBrewer)

### Load data
setwd("~/Dropbox/Israeli Newspapers Study/JOP_REPLICATION/Dataverse submission")
getwd()

data_raw <- read_dta('Data/yeshuv_period_final_Figs.dta')


pop_count = data_raw %>% select(area_num, period, potential) %>%
  group_by(area_num, period) %>%
  summarize_at(.vars = 'potential', .funs = sum)

data_full = data_raw %>% select(area_num, period, IHE, IVsm) %>% distinct() %>%
  left_join(pop_count, by = c("area_num","period"))

data_duplicate = data_full %>% filter(period != 1) %>% mutate(period = "Pooled")
data_period2_4 = data_full %>%
  filter(period != 1) %>%
  mutate(period = case_when(
    period == 2 ~ "2009",
    period == 3 ~ "2013",
    period == 4 ~ "2015",
  ))

data = bind_rows(data_duplicate, data_period2_4)
data = data %>% mutate(period = factor(period, levels = c("Pooled","2009","2013","2015"),
                                       labels = c("Pooled","2009","2013","2015")))

lm_eq = function(df){
  m = lm(IHE ~ IVsm, df)
  fstats = summary(m)
  as.character(round(fstats$fstatistic[1],2))}
eq = ddply(data,.(period), lm_eq)

p5 = data %>%
  ggplot() +
  geom_point(aes(x = IVsm, y=IHE, size = potential), alpha = 0.6, show.legend = F,color = "lightsteelblue4") +
  geom_smooth(aes(x = IVsm, y=IHE), method = 'lm', se = T) +
  facet_wrap(~period) +
  geom_text(data = eq, aes(x = 21, y = 53, label = paste("F-statistic =",V1))) +
  xlab("Yediot readership 2007") +
  ylab("IH exposure") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        strip.text = element_text(size = 12))

ggsave(filename = "Figures/Fig_7.pdf",
       plot = p5,
       bg = "transparent",
       width = 12, height = 8, units = "in",dpi = 1000)
