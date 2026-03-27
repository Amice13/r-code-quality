# This R-file is generating the Figure 4

library(ggplot2)
library(gridExtra)
library(lemon)

setwd("~/Dropbox/Israeli Newspapers Study/JOP_REPLICATION/Dataverse submission")

load("Data/newspapers.rdata")

rw_fp = ggplot(newspapers$front_pages, aes(x=date2, y=score_normalized, color=Newspaper)) +
  geom_smooth(method="loess", aes(fill = Newspaper)) + theme_minimal() +
  ylab("Right Wing Slant") + xlab("Date") + theme(legend.position="none") +
  ggtitle("A. Front pages")

rw_np = ggplot(newspapers$news_pages, aes(x=date2, y=score_normalized, color=Newspaper)) +
  geom_smooth(method="loess", aes(fill = Newspaper)) + theme_minimal() +
  ylab("Right Wing Slant") + xlab("Date") + theme(legend.position="none") +
  ggtitle("B. News pages")

fig_4 = grid_arrange_shared_legend(rw_fp, rw_np, ncol = 2, nrow = 1)
ggsave(file="Figures/Fig_4.pdf", fig_4, width=9, height=4)
