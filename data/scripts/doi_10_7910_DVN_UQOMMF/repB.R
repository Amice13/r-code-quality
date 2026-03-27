# repB.R: Simple responsiveness tests conditional on interest group support for change

library(foreign)

rm(list = ls())

d <- read.dta("gp_data.dta")

# Authors say to drop cases of partial change (2.5, 3.5, 4.5) and 99s
d <- subset(d, !(OUTCOME==2.5|OUTCOME==3.5|OUTCOME==4.5|OUTCOME==99))

# Create outcome variable from raw data
d$outcome <- 0
d$outcome[d$OUTCOME>=2 & d$OUTCOME<=4] <- 1

# Logit transform of authors' independent variables as indicated in the paper
# Note: log() in R is natural log
d$logit.pred90_sw <- log(d$pred90_sw / (1-d$pred90_sw))
d$logit.pred50_sw <- log(d$pred50_sw / (1-d$pred50_sw))
d$net.intgrp <- log(d$INTGRP_STFAV + (0.5*d$INTGRP_SWFAV) + 1) - log(d$INTGRP_STOPP + (0.5*d$INTGRP_SWOPP)+1)
d$scaled.intgrp <- (d$net.intgrp-min(d$net.intgrp))/(max(d$net.intgrp)-min(d$net.intgrp))

######

# Construct subsets based on combinations of support/opposition to policy change

# Conditional on interest group opposition to change
d.both.against <- subset(d,d$pred90_sw<0.5 & d$pred50_sw<0.5 & d$scaled.intgrp<0.5)
d.only.avg.want <- subset(d,d$pred90_sw<0.5 & d$pred50_sw>0.5 & d$scaled.intgrp<0.5)
d.only.wealthy.want <- subset(d,d$pred90_sw>0.5 & d$pred50_sw<0.5 & d$scaled.intgrp<0.5)
d.both.want <- subset(d,d$pred90_sw>0.5 & d$pred50_sw>0.5 & d$scaled.intgrp<0.5)

print(mean(d.both.against$outcome))
print(mean(d.only.avg.want$outcome))
print(mean(d.only.wealthy.want$outcome))
print(mean(d.both.want$outcome))
print(nrow(d.both.against))
print(nrow(d.only.avg.want))
print(nrow(d.only.wealthy.want))
print(nrow(d.both.want))

###############

# Redo but conditional on interest group support for change
d.both.against <- subset(d,d$pred90_sw<0.5 & d$pred50_sw<0.5 & d$scaled.intgrp>0.5)
d.only.avg.want <- subset(d,d$pred90_sw<0.5 & d$pred50_sw>0.5 & d$scaled.intgrp>0.5)
d.only.wealthy.want <- subset(d,d$pred90_sw>0.5 & d$pred50_sw<0.5 & d$scaled.intgrp>0.5)
d.both.want <- subset(d,d$pred90_sw>0.5 & d$pred50_sw>0.5 & d$scaled.intgrp>0.5)

print(mean(d.both.against$outcome))
print(mean(d.only.avg.want$outcome))
print(mean(d.only.wealthy.want$outcome))
print(mean(d.both.want$outcome))
print(nrow(d.both.against))
print(nrow(d.only.avg.want))
print(nrow(d.only.wealthy.want))
print(nrow(d.both.want))

