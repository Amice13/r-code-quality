#####################
#####################
### _7_figures.R ###
#####################
#####################

# This script produces:
# Figures 1-2

#####################
### preliminaries ###
#####################
## directories
appendixPath <- "~/Dropbox/coattails/_replication/appendix"
paperPath <- "~/Dropbox/coattails/_replication/paper"

## R packages
install.packages("pBrackets")
library(pBrackets)


################
### analysis ###
################
# read in the estimates
spec <- read.csv(paste0(appendixPath,"/specification_distribution.csv"))
specH <- read.csv(paste0(appendixPath,"/specification_distribution_H.csv"))
specH$se <- as.numeric(specH$se)

# un-reverse the coding of scandal for the equivalence plot 
specH2 <- specH
specH2$b[specH2$measure==" scandal_rev"] <- specH2$b[specH2$measure==" scandal_rev"]*(-1)
spec2 <- spec
spec2$b[spec$measure==" scandal_rev"] <- spec$b[spec$measure==" scandal_rev"]*(-1)


### Figure 1
# equivalence test
pdf(paste0(paperPath,"/Figure1_equivalence_plot.pdf"),width = 8, height = 6)
par(mar=c(0.0,5.1,1.6,1.1), las = 1)
spec.equiv <- spec2[spec2$specification=="main",]
specH.equiv <- specH2[specH2$specification=="main",]
spec.equiv$lb90 <- spec.equiv$b - qt(p = 1 - .05, df = spec.equiv$df) * spec.equiv$se
spec.equiv$ub90 <- spec.equiv$b + qt(p = 1 - .05, df = spec.equiv$df) * spec.equiv$se
specH.equiv$lb90 <- specH.equiv$b - qt(p = 1 - .05, df = specH.equiv$df) * specH.equiv$se
specH.equiv$ub90 <- specH.equiv$b + qt(p = 1 - .05, df = specH.equiv$df) * specH.equiv$se
plot(x = 1:sum(spec.equiv$years==" all") - .1, y = spec.equiv$b[spec.equiv$years==" all"],
     xlim = c(0.5,8.5),ylim = c(-.11,.105), xaxt = "n", xlab = "", pch = 19, bty = "n",
     col = "darkblue", cex = 1, ylab = "", yaxt = "n")
mtext("Estimated coefficient", side = 2, line = 4, las = 0)
axis(side = 2, at = seq(-.10,.10,.02), las = 1, tick = T, cex.axis = .9)
abline(h = 0, lty = 1, lwd = .75, col = "black")
abline(h = c(-.01,.01), lty = 2, col = "gray48", lwd = 1)
my.x1 <- seq(1.2,8.2,1)
my.x2 <- seq(.8,7.8,1)
my.y <- c(-.012,-.012,-.07,-.012,-.012,-.012,-.012,-.012)
my.y.txt <- my.y - c(.018,.018,.012,.012,.015,.015,.015,.015)
my.labs <- c("Newspaper-\nendorsement\nquality\ndifferential", "Relevant-\nexperience\nquality\ndifferential",
            "Scandal\nincidence","Incumbency\nstatus","Dynamic\nCFscore\nmidpoint",
            "Static\nCFscore\nmidpoint","Incumbent\nNokken-Poole\nscore","Incumbent\nNOMINATE\nscore")
for(i in 1:8){
  brackets(x1 = my.x1[i], x2 = my.x2[i], y1 = my.y[i], y2 = my.y[i], h = .005, curvature = .3)
  text(x = my.x1[i]-.2, y = my.y.txt[i], labels = my.labs[i], cex = .75)
}
brackets(x1 =4.3, x2 = .7, y1 = -.0875, y2 = -.0875, type = 1, h = .01, curvature = .3)
text(x = 2.5, y = -.1025, labels = "Measures of quality", cex = .75)
brackets(x1 =8.3, x2 = 4.7, y1 = -.0875, y2 = -.0875, type = 1, h = .01, curvature = .3)
text(x = 6.5, y = -.1025, labels = "Measures of ideology", cex = .75)
points(x = 1:sum(spec.equiv$years==" all") - .1, y = spec.equiv$b[spec.equiv$years==" all"],
     pch = 19, col = "darkblue", cex = 1)
points(x = 1:sum(spec.equiv$years==" midterm") + .1, 
       y = spec.equiv$b[spec.equiv$years==" midterm"], 
       pch = 17, col = "darkblue", cex = 1)
segments(y0 = spec.equiv$lb90[spec.equiv$years==" all"], y1 = spec.equiv$ub90[spec.equiv$years==" all"],
         x0 = 1:sum(spec.equiv$years==" all") - .1, col = "darkblue", lwd = 2)
segments(y0 = spec.equiv$lb90[spec.equiv$years==" midterm"], y1 = spec.equiv$ub90[spec.equiv$years==" midterm"],
         x0 = 1:sum(spec.equiv$years==" midterm") + .1, col = "darkblue", lwd = 2)
points(x = 1:sum(specH.equiv$years==" all") - .1, y = specH.equiv$b[spec.equiv$years==" all"],
       pch = 19, col = "darkgreen", cex = 1)
points(x = 1:sum(specH.equiv$years==" midterm") + .1, y = specH.equiv$b[specH.equiv$years==" midterm"],
       pch = 17, col = "darkgreen", cex = 1)
segments(y0 = specH.equiv$lb90[specH.equiv$years==" all"], y1 = specH.equiv$ub90[specH.equiv$years==" all"],
         x0 = 1:sum(specH.equiv$years==" all") - .1, col = "darkgreen", lwd = 2)
segments(y0 = specH.equiv$lb90[specH.equiv$years==" midterm"], y1 = specH.equiv$ub90[specH.equiv$years==" midterm"],
         x0 = 1:sum(specH.equiv$years==" midterm") + .1, col = "darkgreen", lwd = 2)
arrows(x0 = 1.7, x1 = 1.9, y0 = .02, y1 = spec.equiv$b[3]+.004, length = .05, code = 2, lwd = 1.5, col = "darkblue")
arrows(x0 = 2.3, x1 = 2.1, y0 = .02, y1 = spec.equiv$b[3]+.004, length = .05, code = 2, lwd = 1.5, col = "darkblue")
text(x = 2, y = .03, labels = "Top-of-ticket effect:", cex = .65, col = "darkblue")
text(x = 2, y = .019, labels = "all years    midterm years", cex = .65, col = "darkblue", pos = 3)
arrows(x0 = 1.7, x1 = 1.9, y0 = .065, y1 = specH.equiv$b[3]+.006, length = .05, code = 2, lwd = 1.5, col = "darkgreen")
arrows(x0 = 2.3, x1 = 2.12, y0 = .065, y1 = specH.equiv$b[3]+.0085, length = .05, code = 2, lwd = 1.5, col = "darkgreen")
text(x = 2, y = .075, labels = "House effect:", cex = .65, col = "darkgreen")
text(x = 2, y = .064, labels = "all years    midterm years", cex = .65, col = "darkgreen", pos = 3)
graphics.off()


### Figure 2
# distribution of quality and ideology top-of-ticket estimates
pdf(paste0(paperPath,"/Figure2_specification_plot.pdf"),width = 8, height = 6)
top.est <- spec$b
par(mar=c(4.1,4.1,2.1,1.1), las = 1)
hist(top.est, xlab = "Estimated statewide top-of-the-ticket effect",freq = F, xlim = c(-.1,.1), 
     breaks = 12,ylim = c(0,140),
     col = "gray88", main = "", xaxs = "i", yaxs = "i")
abline(v = mean(top.est), col = "darkblue", lwd = 2, lty = 1)
abline(v = median(top.est), col = "firebrick", lwd = 2, lty = 2)
abline(v = c(-.01,.01), lty = 2, col = "gray28", lwd = 1)
graphics.off()