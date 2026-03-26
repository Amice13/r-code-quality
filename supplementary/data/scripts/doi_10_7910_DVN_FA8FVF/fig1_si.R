print("Running fig1_si.R...")

load("../results/fig1_si.RData")
library(ggplot2)
library(metafolio)
library(tidyverse)

## In-text number, page 13
pf %>% group_by(Legend) %>% summarize(mean(val))


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

ggplot(pf, aes(x=val, fill=Legend)) + geom_density(alpha=0.5, bw=0.04) + 
  xlab("% Agreement") + ylab("") +
  scale_fill_manual(values=c(gg_color_hue(3)[1], NA, gg_color_hue(3)[3]))+
  scale_x_continuous(limits = c(0, 1), breaks=c(0,0.25,0.5,0.75,1), labels = c("0", "25", "50", "75", "100")) +
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
  scale_y_continuous(limits = c(0,6), expand = c(0, 0))+
  ggplot2::annotate("text", x=.69, y=4.5, label= "Ranking", col=gg_color_hue(3)[1], size=8) + 
  ggplot2::annotate("text", x = .45, y=.6, label = "Paired", col="steelblue4", size=8) + 
  ggplot2::annotate("text", x = .34, y=2.8, label = "Random", col="black", size=8)
ggsave("../results/mturk_tpc_intercoder.png", width=10, height=6)

