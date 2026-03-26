# rep3.R: Disagreement tests

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


######### SECTION A ##########
# What happens when the wealthy and the average disagree?

# Subset to only cases of disagreement
ddis <- subset(d,(d$pred90_sw>0.5 & d$pred50_sw<0.5) | (d$pred90_sw<0.5 & d$pred50_sw>0.5))

# To restrict to only cases of disagreement AND a large preference gap, uncomment this:
#ddis <- subset(ddis,abs(ddis$pred90_sw-ddis$pred50_sw)>0.1)

wealthy.win <- 0
avg.win <- 0
for (m in 1:nrow(ddis)) {
  if (abs(ddis$outcome[m]-ddis$pred90_sw[m]) > abs(ddis$outcome[m]-ddis$pred50_sw[m])) {
    avg.win <- avg.win + 1
  } else {
    wealthy.win <- wealthy.win + 1
  }
}
point.est <- (100*avg.win/(avg.win+wealthy.win))
cat("How often the average saw their favored outcome when opposed by the wealthy (percent):", point.est)
print(nrow(ddis))



########## SECTION B ##########
# Introduce interest group opposition to the wishes of each group

d.wealthy.alone <- subset(d,(d$pred90_sw>0.5 & d$pred50_sw<0.5 & d$scaled.intgrp<0.5) | (d$pred90_sw<0.5 & d$pred50_sw>0.5 & d$scaled.intgrp>0.5))

wealthy.win <- 0
for (m in 1:nrow(d.wealthy.alone)) {
  if (d.wealthy.alone$outcome[m]==1 & d.wealthy.alone$pred90_sw[m]>0.5) {
    wealthy.win <- wealthy.win + 1
  }
  if (d.wealthy.alone$outcome[m]==0 & d.wealthy.alone$pred90_sw[m]<0.5) {
    wealthy.win <- wealthy.win + 1
  }
}
point.est.w <- (100*wealthy.win/nrow(d.wealthy.alone))

# Repeat for average instead
d.avg.alone <- subset(d,(d$pred90_sw>0.5 & d$pred50_sw<0.5 & d$scaled.intgrp>0.5) | (d$pred90_sw<0.5 & d$pred50_sw>0.5 & d$scaled.intgrp<0.5))

avg.win <- 0
for (m in 1:nrow(d.avg.alone)) {
  if (d.avg.alone$outcome[m]==1 & d.avg.alone$pred50_sw[m]>0.5) {
    avg.win <- avg.win + 1
  }
  if (d.avg.alone$outcome[m]==0 & d.avg.alone$pred50_sw[m]<0.5) {
    avg.win <- avg.win + 1
  }
}
point.est.a <- (100*avg.win/nrow(d.avg.alone))

# Print results for this section
cat("How often the wealthy see a more favorable outcome despite joint opposition from the other 2 groups (percent):", point.est.w)
print(nrow(d.wealthy.alone))
cat("How often the average see a more favorable outcome despite joint opposition from the other 2 groups (percent):", point.est.a)
print(nrow(d.avg.alone))
