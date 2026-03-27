# set working directory
# wd <- ""
setwd(wd)

# load database
library(haven)
d <- read_dta("Data//england_finance_ts.dta")

# define graphical parameters
d$col = ifelse(d$war==1,"gray60","black")
d$pch = ifelse(d$war!=0,1,2)

# make the plot
plot(y = d$ASSALES,
     x = d$YEAR,
     type = "b",
     col = d$col,
     pch = d$pch,
     xlab = "",
     ylab = "Net revenue from sales of assets")

legend("topleft",
       c("England at war",
         "England not at war"),
       xpd = TRUE, 
       horiz = FALSE, 
       inset = c(0, 0), 
       pch = c(1,2), 
       bty = 'n',
       col = c("gray60","black"), 
       cex = 0.9)


# Calculate the means across years of war and peace
mean(d$ASSALES[d$war==1]) # total
mean(d$ASSALES[d$war==0])

d$prop_assets = d$ASSALES/d$REVTOT # proportion
mean(d$prop_assets[d$war==1], na.rm=TRUE)
mean(d$prop_assets[d$war==0], na.rm=TRUE)
