# This R-file is generating the Figure SI-7

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

data <- read_dta('Data/yeshuv_period_final_Figs.dta')

# Scatter plot - DV: IVsm, period 4

## Get the median of BRB in period 1
BRB1_quantile <- data %>% filter(period == 1)
BRB1_quantile <- quantile(BRB1_quantile$BRB, probs = seq(0, 1, 1/3))

## Create a dummy based on the median of BRB in period 1
data2 <- data %>%
  mutate(BRB_quantile = case_when(
    BRB <= BRB1_quantile[2] ~ "Low",
    (BRB > BRB1_quantile[2]) & (BRB <= BRB1_quantile[3]) ~ "Medium",
    BRB >= BRB1_quantile[3] ~ "High",
  )) %>%
  mutate(BRB_quantile = factor(BRB_quantile,
                               levels = c("High","Medium","Low"),
                               labels = c("High","Medium","Low"))) %>%
  filter(period==2, area_num != 2)

high = data2 %>% filter(BRB_quantile == "High") %>% mutate(ltd = (ltd-min(ltd))/(max(ltd)-min(ltd)))
medium = data2 %>% filter(BRB_quantile == "Medium") %>% mutate(ltd = (ltd-min(ltd))/(max(ltd)-min(ltd)))
low = data2 %>% filter(BRB_quantile == "Low") %>% mutate(ltd = (ltd-min(ltd))/(max(ltd)-min(ltd)))

data2 = bind_rows(high, medium, low)

scatter_scaled_color <- ggplot(data2) +
  geom_point(aes(x = ltd, y=IHE, size = potential), alpha = 0.6, color = "lightsteelblue4", show.legend = F) +
  geom_smooth(aes(x = ltd, y=IHE), method = 'lm') +
  ggpubr::stat_cor(aes(x = ltd, y=IHE), method = "spearman", cor.coef.name = "rho",p.accuracy = 0.001, label.x = 0, label.y = 33.5) +
  scale_size(range = c(0.05,5),name="Population Size", trans = "log10") +
  #scale_x_log10() +
  facet_wrap(~BRB_quantile) +
  ylab("Israel-Hayom Exposure (2009)") +
  xlab("Scaled distance to Tel-Aviv") +
  theme_bw() +
  theme(axis.title.x = element_text(size=12.5, face="bold", colour = "black"),
        axis.title.y = element_text(size=12.5, face="bold", colour = "black"),
        strip.text.x = element_text(size=10, face="bold", colour = "black"))

ggsave(filename = "Figures/Fig_SI-7.pdf",
       plot = scatter_scaled_color,
       bg = "transparent",
       width = 12, height = 6, units = "in",dpi = 1000)
