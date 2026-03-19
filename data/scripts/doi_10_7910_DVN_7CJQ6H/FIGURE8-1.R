# Comparison of CGE and Econometric results
econ <- read.csv("../exportdata/econ_predict.csv")
econ <- subset(econ, variable == "employment")
names(econ)[4] <- "econometric"

cge <- read.csv("../cge_results/cge_armington_sens.csv")
cge$sector <- toupper(cge$sector)
cge$scenario <- gsub("arm","",cge$scenario)
cge$scenario[cge$scenario == "1"] <- "10"
cge$scenario[cge$scenario == "2"] <- "20"
cge$scenario <- as.numeric(cge$scenario) / 10
cge <- subset(cge, item == "Employment")

cge <- merge(cge, econ, by="sector")

library(dplyr)
library(broom)

dat <- cge %>%
  group_by(scenario) %>%
  do(fit = lm(value ~ econometric, data = .))  


plotdat <- glance(dat,fit)[1:2]
plotdat2 <- subset(tidy(dat,fit), term == "econometric", select = c("scenario","estimate"))

labels <- data.frame(rbind(as.numeric(plotdat[nrow(plotdat),]),as.numeric(plotdat2[nrow(plotdat2),])))
names(labels) <- c("x","y")
labels$text <- c("R squared", "Slope coefficient")

library(ggplot2)

ggplot(plotdat, aes(x=scenario, y=r.squared)) + geom_line(colour="red") + geom_point(colour="red") +
  theme_bw() + xlab("Armington multiplier") + ylab("R-squared and regression coefficient") +
  geom_line(data=plotdat2, aes(x=scenario, y=estimate), colour="blue") +
  geom_point(data=plotdat2, aes(x=scenario, y=estimate), shape=2, colour="blue") +
  geom_label(data=labels, aes(x=x,y=y,label=text), nudge_x = -0.1)

