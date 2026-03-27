# set working directory
# wd <- ""
setwd(wd)

# load database
library(haven)
oxford <- read_dta("Data/oxford_graduates.dta")

# compute proportion lawyers by year
years <- seq(from=1500,
             to=1714,
             by = 1)

total.church <- c()
total.law <- c()
prop.secular <- c()


for(i in 1:length(years)){
  total.church[i] <- sum(oxford$church[oxford$year==years[i]], na.rm = TRUE)
  total.law[i] <- sum(oxford$law[oxford$year==years[i]], na.rm=TRUE)
  prop.secular[i] <- (total.law[i])/(total.law[i] + total.church[i])
}

# compute moving average

total.church.ma <- c()
total.law.ma <- c()
prop.sec.ma <- c()

for(i in 21:length(years)){
  total.church.ma[i] = mean(total.church[(i-20):i])
  total.law.ma[i] = mean(total.law[(i-20):i])
  prop.sec.ma[i] = mean(prop.secular[(i-20):i])
}

# Plot the results
plot(y = prop.secular[years>1515],
     x = years[years>1515],
     type = "l",
     col = "gray60",
     lwd = 1.5,
     ylim = c(0,0.6),
     xlim = c(1515,1714),
     main = "",
     ylab = "",
     xlab = "")
par(new=TRUE)
plot(y = prop.sec.ma[years>1515],
     x = years[years>1515],
     type = "l",
     col = "black",
     lwd = 6,
     ylim = c(0,0.6),
     xlim = c(1515,1714),
     main = "",
     ylab = "Proportion of secular careers",
     xlab = "")
abline(v = 1536, col = "black", lty = 2, lwd = 3)

