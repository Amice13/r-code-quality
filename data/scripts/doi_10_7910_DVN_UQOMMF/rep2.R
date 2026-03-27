# rep2.R: Status-quo bias and high preference tests

library(foreign)

rm(list = ls())
d <- read.dta("gp_data.dta")

# Authors say to drop cases of partial change (2.5, 3.5, 4.5) and 99s
d <- subset(d, !(OUTCOME==2.5|OUTCOME==3.5|OUTCOME==4.5|OUTCOME==99))

# Create outcome variable from raw data
d$outcome <- 0
d$outcome[d$OUTCOME>=2 & d$OUTCOME<=4] <- 1

######

# Flag to study changes only (1 to study the status quo bias or 0 to study all outcomes)
changes.only <- 1

# First, how often do the wealthy get what they want when they have a strong preference?
if (changes.only == 1) {
  dstrong <- subset(d,d$pred90_sw>0.8)
} else {
  dstrong <- subset(d,d$pred90_sw>0.8 | d$pred90_sw<0.2)
}
wealthy.win <- 0
for (m in 1:nrow(dstrong)) {
  if (dstrong$outcome[m]==1) {
    if (dstrong$pred90_sw[m] > 0.5) {
      wealthy.win <- wealthy.win + 1
    }
  } else {
    if (dstrong$pred90_sw[m] < 0.5) {
      wealthy.win <- wealthy.win + 1
    }
  }
}
wealthy.win.pct <- 100*wealthy.win/nrow(dstrong)


# Same analysis, but for the average
if (changes.only == 1) {
  dstrong <- subset(d,d$pred50_sw>0.8)
} else {
  dstrong <- subset(d,d$pred50_sw>0.8 | d$pred50_sw<0.2)
}
avg.win <- 0
for (m in 1:nrow(dstrong)) {
  if (dstrong$outcome[m]==1) {
    if (dstrong$pred50_sw[m] > 0.5) {
      avg.win <- avg.win + 1
    }
  } else {
    if (dstrong$pred50_sw[m] < 0.5) {
      avg.win <- avg.win + 1
    }
  }
}
avg.win.pct <- 100*avg.win/nrow(dstrong)

cat("Percent of time that outcome coincides with wealthy or average\n preferences, respectively, when each has strong preference\n (regardless of other group's preference):", wealthy.win.pct, "vs.", avg.win.pct)

