print("Running fig2_si.R...")

load("../results/fig2_si.RData")

library(ggplot2)
library(metafolio)
library(tidyverse)
# In-text number, page 13-14
pf %>% group_by(source) %>% summarize(mean(agree))

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

ggplot(pf, aes(x=agree, fill = source)) + geom_density(alpha=0.5, bw=0.05, kernel="gaussian") + 
  scale_x_continuous(limits = c(0, 1), breaks=c(0,0.25,0.5,0.75,1), labels = c("0", "25", "50", "75", "100")) +
  scale_fill_manual(values=c(gg_color_hue(3)[1], gg_color_hue(3)[3], NA))+
  theme(legend.position = "none") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.y=element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size=24),
        axis.title.x = element_text(size=24),
        legend.position = "none") + 
  scale_y_continuous(limits =c(0,7), expand = c(0, 0))+
  xlab("% Agreement") + ylab("") + 
  ggplot2::annotate("text", x=.73, y=4.4, label= "Ranking", col=gg_color_hue(3)[1], size=8) + 
  ggplot2::annotate("text", x = .65, y=3, label = "Paired", col=gg_color_hue(3)[3], size=8) +
  ggplot2::annotate("text", x = .305, y=2.5, label="Random", col="black", size=8)

ggsave("../results/mturk_tpc_intracoder.png", width=10, height=6)