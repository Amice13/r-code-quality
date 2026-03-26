############################################################################################################################################
## Replication Data for:                                                                                                                   #
## Wawro, Gregory, and Ira Katznelson. Time Counts: Quantitative Analysis for Historical Social Science. Princeton University Press, 2022. #
############################################################################################################################################

rm(list = ls())
library(ggplot2)
library(dplyr)

hse_polar <- data.frame(read.csv("../Data/VoteviewPolarization.csv")) %>% subset(chamber=="House")

pdf("../Figures/Figure 5-7.pdf", height = 6, width = 8)
ggplot(hse_polar, aes(x = congress,y = party.mean.diff.d1)) +
  geom_line(size=1.2) + 
  scale_x_continuous(breaks = c(seq(50,115,5)), 
                     labels = c("  50 \n 1887", "  55 \n 1897", "  60 \n 1907", "  65 \n 1917", "  70 \n 1927", "  75 \n 1937", "  80 \n 1947", 
                                "  85 \n 1957", "  90 \n 1967", "  95 \n 1977", "  100 \n 1987", "  105 \n 1997", "  110 \n 2007", "115 \n 2017")) +
  xlab("\n Congress/Year") +
  ylab("Difference in party means") +
  theme_bw() +
  theme(text=element_text(size=14, color="black"),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=14, color="black"),
        axis.text.x = element_text(size=12.5, color="black"),
        axis.line = element_line(color='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
  )
dev.off()
