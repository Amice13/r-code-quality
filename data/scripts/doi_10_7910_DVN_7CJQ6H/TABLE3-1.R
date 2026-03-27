# Compare econometric and CGE results
library(tidyverse)
econ <- read.csv("../exportdata/econ_predict.csv")
cge <- read.csv("../exportdata//cge_sensitivity.csv")
cge <- subset(cge, scenario == "rev", select = -1)
econ <- subset(econ, select = -1)
names(cge)[1] = "variable"

names(econ)[3] <- "econ"
names(cge)[3] <- "cge"

cge$variable <- tolower(cge$variable)
cge$variable[cge$variable == "export"] = "exports"
cge$sector <- toupper(cge$sector)

dat <- merge(econ,cge, by=c("sector","variable"))

# Weights
output <- read.csv("../cge_results/benchmark_output.csv")
dat <- merge(dat, output, by="sector")
dat <- dat %>%
  filter(variable == "employment")

# Regression
m1 <- lm(cge ~ econ, data=dat)
m2 <- lm(cge ~ econ, weights=output, data=dat)

library(car)
# Hypothesis that slope =1
linearHypothesis(m1, "econ=1")
linearHypothesis(m2, "econ=1")

linearHypothesis(m1, c("(Intercept)=0","econ=1"))
linearHypothesis(m2, c("(Intercept)=0","econ=1"))

library(stargazer)
# TABLE 3
stargazer(m1,m2,column.labels = c("Unweighted","Weighted"), type = "text", title="Regression of CGE point estimates on econometric point estimates")

# FIGURE 3
library(ggrepel)
ggplot(dat, aes(x=econ, y=cge, label=sector)) + geom_text_repel() + geom_point(color="green") + coord_fixed(ratio=1) +
  theme_bw() + xlab("Percent change in employment from econometric model") +
  ylab("Percent change in employment from CGE model") +
  geom_abline(intercept=0, slope=1, linetype="dashed") +
  stat_smooth(method = "lm", aes(group=1), se=FALSE, colour="red", linetype="dotdash") +
  stat_smooth(method = "lm", aes(group=1,weight=output), se=FALSE, colour="blue", linetype="longdash") +
  scale_x_continuous(breaks=c(-15,-10,-5,0,5),limits=c(-18,6)) +
  scale_y_continuous(breaks=c(-15,-10,-5,0,5),limits=c(-18,6))
