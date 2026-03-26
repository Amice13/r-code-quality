sink(file="figureC2_log.txt")


# Install and load libraries

install.packages('ggplot2')
install.packages('farver')
install.packages('gridExtra')

library(ggplot2)
library(farver)
library(gridExtra)

# Set the working directory
#setwd("")

# Load data
load("tscs.RData")

##
y <- aggregate(parole_restriction ~ half_decade + trifecta, v, mean)


p_parole <- ggplot(y, aes(x = half_decade, y = parole_restriction, group=trifecta, color=trifecta)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values=c('Blue','purple1','Red'), labels=c("Democrat", "Divided", "Republican")) +
  labs(title = "Parolee Restrictions",
       x = "",
       y = "Fraction", 
       col="Trifecta") +
  theme_bw() +
  scale_y_continuous(limits=c(0,1)) +
  #scale_x_discrete(limits=c("1995-1999", "2015-2019")) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1), axis.title=element_text(size=12), plot.title = element_text(size = 14)) +
  theme(legend.position="none") +   theme(axis.title.x = element_blank())

y <- aggregate(probation_restriction ~ half_decade + trifecta, v, mean)

p_probation <- ggplot(y, aes(x = half_decade, y = probation_restriction, group=trifecta, color=trifecta)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values=c('Blue','purple1','Red'), labels=c("Democrat", "Divided", "Republican")) +
  labs(title = "Probationer Restrictions",
       x = "",
       y = "Fraction", 
       col="Trifecta") +
  theme_bw() +
  scale_y_continuous(limits=c(0,1)) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1), axis.title=element_text(size=12), plot.title = element_text(size = 14)) +
  theme(legend.position="none") +   theme(axis.title.x = element_blank())

p_code <- grid.arrange(grobs = list(p_parole, p_probation),
                       ncol = 2, nrow = 1)

p_code
ggsave("felonCode.pdf", p_code, width = 9.02, height = 4.46)
