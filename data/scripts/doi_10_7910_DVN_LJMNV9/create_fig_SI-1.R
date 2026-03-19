# This R-file is generating the Figure SI-1

library(ggplot2)

setwd("~/Dropbox/Israeli Newspapers Study/JOP_REPLICATION/Dataverse submission")

load("Data/ih_length_df.rdata")

options(scipen = 999)
ggplot(data = ih_length_df, aes(x = date, y = nchar))+
  geom_point(alpha = 1/10) +
  geom_smooth(se=F, color="black") + theme_minimal() + xlab("Date") + ylab("Number of characters")
  ggsave(file="Figures/Fig_SI-1.pdf", width=5, height = 5)
