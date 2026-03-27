require(ggplot2)

dFubar <- read.csv("FUBAR_output_ALLseqs.csv", sep = ",", header = TRUE)
head(dFubar)

ggplot(dFubar) +
  geom_segment(aes(x=Position, y=beta_alpha, xend = Position, yend = 0, group = 1), color = "#1f78b4", size=0.5) +
  #geom_line(aes(y=alpha), color = "#fb9a99", size=0.5) +
  theme_classic() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  ylab(beta~"-"~alpha) +
  xlab("\nCodon position") + 
  theme(axis.title = element_text(size = 16),
        axis.text.y = element_text(size=8),
        axis.text.x = element_text(angle = 90, size = 8, hjust = 1))