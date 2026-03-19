# set working directory
# wd <- ""
setwd(wd)

# load database
library(haven)
d <- read_dta("Data/france_chancelleries_ts.dta")

# set graphical parameters
d$col = ifelse(d$war==1,"gray60","black")
d$cex = ifelse(d$rate!=0,1,0.5)
d$pch = ifelse(d$war!=0,1,2)

# make plot
plot(y = d$rate,
     x = d$year,
     xlim = c(1484,1789),
     type = "b",
     pch = d$pch,
     col = d$col,
     main = "",
     lwd = 1.2,
     xlab = "",
     cex = d$cex,
     ylab = "Rate of change of number of offices")

legend("topright",
       c("France at war",
         "France not at war"),
       xpd = TRUE, 
       horiz = FALSE, 
       inset = c(0, 0), 
       pch = c(1,2), 
       bty = 'n',
       col = c("gray60","black"), 
       cex = 0.9)

mean(d$rate[d$war==1])
mean(d$rate[d$war==0], na.rm=TRUE)
