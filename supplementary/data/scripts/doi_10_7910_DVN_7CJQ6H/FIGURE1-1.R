dat <- read.csv("exp_emit_graph_data.csv")
hjust <- dat$hjust
vjust <- dat$vjust
library(ggplot2)

ggplot(dat, aes(x=export_int, y=emit_int, size=output/sum(output), label=sector)) + geom_point() +
  xlab("Share of output that is exported") +
  ylab("Emissions intensity (t CO2/$1,000)") +
  theme_bw() +
  geom_text(size=5,hjust=hjust,vjust=vjust) +
  xlim(c(-0.1,1)) +
  scale_size_continuous(guide = guide_legend(title = "Share of BC economy")) +
  theme(legend.position = c(0.15, 0.8))