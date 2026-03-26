## Author: Kabir Khanna
## Updated: December 20, 2015
## Note: First run Residuals.R

load("resid.all.RData")
load("resid.all.cond.RData")
load("resid.whi.RData")
load("resid.whi.cond.RData")
load("resid.bla.RData")
load("resid.bla.cond.RData")
load("resid.his.RData")
load("resid.his.cond.RData")
load("resid.asi.RData")
load("resid.asi.cond.RData")
load("resid.oth.RData")
load("resid.oth.cond.RData")

print("Loaded residuals")

## Figure 2
png('figure2.png', width = 1600, height = 1100, res = 180)
par(mfrow = c(2, 3), mar = c(4.1, 4.1, 3.1, 2.1)) #bottom, left, top, right

qqplot(abs(resid.all), abs(resid.all.cond), pch = 20, col = rgb(0, 0, 0, alpha = .2), cex.axis = .8, 
     main = "All Races", xlab = "No Conditioning", ylab = "Conditioning on Race")
abline(a = 0, b = 1, lty = 2)
print("Plotted All Races")

qqplot(abs(resid.whi), abs(resid.whi.cond), pch = 20, col = rgb(0, 0, 0, alpha = .2), cex.axis = .8, 
     main = "Whites", xlab = "No Conditioning", ylab = "Conditioning on Race")
abline(a = 0, b = 1, lty = 2)
print("Plotted Whites")

qqplot(abs(resid.bla), abs(resid.bla.cond), pch = 20, col = rgb(0, 0, 0, alpha = .2), cex.axis = .8, 
     main = "Blacks", xlab = "No Conditioning", ylab = "Conditioning on Race")
abline(a = 0, b = 1, lty = 2)
print("Plotted Blacks")

qqplot(abs(resid.his), abs(resid.his.cond), pch = 20, col = rgb(0, 0, 0, alpha = .2), cex.axis = .8, 
     main = "Latinos", xlab = "No Conditioning", ylab = "Conditioning on Race")
abline(a = 0, b = 1, lty = 2)
print("Plotted Latinos")

qqplot(abs(resid.asi), abs(resid.asi.cond), pch = 20, col = rgb(0, 0, 0, alpha = .2), cex.axis = .8, 
     main = "Asians", xlab = "No Conditioning", ylab = "Conditioning on Race")
abline(a = 0, b = 1, lty = 2)
print("Plotted Asians")

qqplot(abs(resid.oth), abs(resid.oth.cond), pch = 20, col = rgb(0, 0, 0, alpha = .2), cex.axis = .8, 
     main = "Others", xlab = "No Conditioning", ylab = "Conditioning on Race")
abline(a = 0, b = 1, lty = 2)
print("Plotted Others")

dev.off()
