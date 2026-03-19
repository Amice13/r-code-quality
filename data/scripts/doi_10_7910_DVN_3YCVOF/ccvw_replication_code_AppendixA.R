### Replication code for split-sample study reported in Online Appendix for
### Campbell, R., Cowley, P., Vivyan, N & Wagner, M. (2016) 'Legislator dissent as a valence signal'.
### British Journal of Political Science


### set working directory and options

rm(list = ls()); gc()
options("contrasts" = c("contr.treatment", "contr.treatment"))
setwd("~/Dropbox/MP independence/replication_materials")


### load packages

library(car)
library(ggplot2)
library(rms)
library(stargazer)
library(lmtest)
library(reshape)
library(plyr)


### Load data

ssdat <- readRDS("data/appendixAdata.rds")


### Estimate treatment effects and plot

summary(m1 <- lm(prefb ~ treatment, data = ssdat))

eff.tab <- data.frame(mean = coef(m1), cilo = confint(m1)[,1], cihi = confint(m1)[,2])
eff.tab[1,] <- c(0,0,0)
eff.tab <- 100*eff.tab
eff.tab$labels <- factor(c("Never votes against\n(baseline)", "Never votes against\nbut outspoken",
                           "Rarely votes against", "Rarely votes against\nbut outspoken",
                           "Regularly votes against"), levels = rev(c("Never votes against\n(baseline)", "Never votes against\nbut outspoken",
                                                                      "Rarely votes against", "Rarely votes against\nbut outspoken",
                                                                      "Regularly votes against")))

ggplot(eff.tab, aes(x = labels, y = mean)) + 
  geom_hline(xintercept = 0, linetype = "dashed", size = 0.5) +
  geom_pointrange(aes(ymin = cilo, ymax = cihi), size = 0.75, colour = "#0072B2") + 
  labs(y = "Effect on support for MP B", x = "MP dissent behaviour") + 
  theme(axis.title = element_text(size = 12, vjust = 0.1, face = "bold")) +
  coord_flip() + 
  theme(axis.text = element_text(colour = "black")) +
  theme(axis.text.y = element_text(hjust = 1)) +
  theme(text = element_text(size = 15)) +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_text(hjust = 1), # remove ticks and justify
        axis.title.x = element_text(vjust = 0)) +
  theme(legend.position = "none")
ggsave("figures/figureS1.eps", height = 4, width = 6)

























