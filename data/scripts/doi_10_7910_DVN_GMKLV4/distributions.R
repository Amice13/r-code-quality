library(ggplot2)
dat <- read.delim("distribution_du.csv", header = T)
colnames(dat) <- c("Lemma","Stars","Freq","RelFreq")
ggplot(data = dat, aes(Stars, RelFreq, col = Lemma)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = F)
#ggsave("du.png", width = 6, height = 3)
