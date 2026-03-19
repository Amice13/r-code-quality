#####
## Replication File:
##
## American Politics in Two Dimensions: 
## Partisan and Ideological Identities 
## versus Anti-Establishment Orientations
#####

##########################################################################

####
## Load necessary packages
## 
## Analyses conducted in RStudio 1.0.153,
## using R 3.6 (Mac OS Catalina 10.15.7)
####

#install.packages("stats") # version 3.6.3
#install.packages("car") # version 3.0-10
#install.packages("lattice") # version 0.20-38
#install.packages("latticeExtra") # version 0.6-29
#install.packages("foreign") # version 0.8-75
#install.packages("psych") # version 2.0.9
#install.packages("Matrix") # version 1.2-18

require(stats)
require(car)
require(lattice)
require(latticeExtra)
require(foreign) 
require(psych) 
require(Matrix)

##########################################################################

####
## Read in Study 1 and Study 2 data
####

# Set personal working directory

ct2019 <- read.dta("Clean Data, Study 1.dta")

ct2020 <- read.dta("Clean Data, Study 2.dta")

##########################################################################

####
## Figure 2
####

hist1 <- histogram(~suspicion2, 
          data = ct2019,
          aspect = 1,
          xlab = "Anti-Establishment Dimension"
)

hist2 <- histogram(~leftright2, 
          data = ct2019,
          aspect = 1,
          xlab = "Left-Right Dimension"
)

pdf("figure2.pdf", width = 8, height = 4)
print(hist1, position = c(0, 0, .5, 1), more = TRUE)
print(hist2, position = c(.5, 0, 1, 1), more = FALSE)
dev.off()

##########################################################################

####
## Figure 3
####

set.seed(1234)
scat1 <- xyplot(jitter(manipulate) ~ jitter(leftright2),
                data = ct2019,
                aspect = 1,
                xlab = "Left-Right Orientations",
                ylab = "Machiavellianism",
                panel = function(x, y,...){
                  panel.xyplot(x, y, col = "dark grey")
                  panel.lmline(ct2019$leftright2, ct2019$manipulate, col = "red")
                  panel.loess(ct2019$leftright2, ct2019$manipulate, col = "black", lwd = 2)
                }
)

set.seed(1234)
scat2 <- xyplot(jitter(narcissism) ~ jitter(leftright2),
       data = ct2019,
       aspect = 1,
       xlab = "Left-Right Orientations",
       ylab = "Narcissism",
       panel = function(x, y,...){
         panel.xyplot(x, y, col = "dark grey")
         panel.lmline(ct2019$leftright2, ct2019$narcissism, col = "red")
         panel.loess(ct2019$leftright2, ct2019$narcissism, col = "black", lwd = 2)
       }
)

set.seed(1234)
scat3 <- xyplot(jitter(sociopathy) ~ jitter(leftright2),
                data = ct2019,
                aspect = 1,
                xlab = "Left-Right Orientations",
                ylab = "Sociopathy",
                panel = function(x, y,...){
                  panel.xyplot(x, y, col = "dark grey")
                  panel.lmline(ct2019$leftright2, ct2019$sociopathy, col = "red")
                  panel.loess(ct2019$leftright2, ct2019$sociopathy, col = "black", lwd = 2)
                }
)

set.seed(1234)
scat4 <- xyplot(jitter(violence) ~ jitter(leftright2),
       data = ct2019,
       aspect = 1,
       xlab = "Left-Right Orientations",
       ylab = "Support for Violence",
       panel = function(x, y,...){
         panel.xyplot(x, y, col = "dark grey")
         panel.lmline(ct2019$leftright2, ct2019$violence, col = "red")
         panel.loess(ct2019$leftright2, ct2019$violence, col = "black", lwd = 2)
       }
)

pdf("figure3.pdf", width = 8, height = 8)
print(scat1, position = c(0, .5, .5, 1), more = T)
print(scat2, position = c(.5, .5, 1, 1), more = T)
print(scat3, position = c(0, 0, .5, .5), more = T)
print(scat4, position = c(.5, 0, 1, .5), more = F)
dev.off()

##########################################################################

####
## Figure 4 
####

online <- read.dta("Online Behavior Predictions.dta")

my.panel.bands <-
        function(x, y, upper, lower,
                 fill, col,
                 subscripts, ..., font, fontface)
        {
                upper <- upper[subscripts]
                lower <- lower[subscripts]
                panel.polygon(c(x, rev(x)), c(upper, rev(lower)),
                              col = fill, border = FALSE,
                              ...)
        }

pdf("figure4.pdf", width = 7, height = 3.5)
xyplot(estimate ~ scale | as.factor(order),
       data = online,
       aspect = 1,
       ylab = "Predicted Behavior",
       xlab = "Anti-Establishment Dimension",
       ylim = c(-.1, 4.2),
       groups = order,
       col = "black",
       fill = c("light grey"),
       type = "l",
       upper = online$upper,
       lower = online$lower,
       strip=strip.custom(factor.levels=c("Argue Online", "Reddit",
                                          "4chan/8chan")),
       panel = function(x, y, ...){
               panel.superpose(x, y, panel.groups = 'my.panel.bands', ...)
               panel.xyplot(x, y, ...)
       }
)
dev.off()

##########################################################################

####
## Figure 5
####

candpred1 <- read.dta("Candidate Predictions, AE.dta")

ae1 <- xyplot(estimate ~ scale | as.factor(order),
              data = candpred1,
              aspect = 1,
              ylab = "Predicted Thermometer Score",
              xlab = "",
              ylim = c(-6, 101),
              groups = order,
              col = "black",
              type = "l",
              fill = c("light grey"),
              upper = candpred1$upper,
              lower = candpred1$lower,
              strip=strip.custom(factor.levels=c("Donald Trump", 
                                                 "Bernie Sanders",  
                                                 "Joe Biden")),
              key=list(columns=1, lines=list(lty=c(1, 2), lwd=2, col="black"),
                       text=list(c("Anti-Establishment", 
                                   "Left-Right"))),
              panel = function(x, y, ...){
                      panel.superpose(x, y, panel.groups = 'my.panel.bands', ...)
                      panel.xyplot(x, y, ...)
              }
)

candpred2 <- read.dta("Candidate Predictions, LR.dta")

lr1 <- xyplot(estimate ~ scale | as.factor(order),
              data = candpred2,
              aspect = 1,
              #ylab = "Predicted Thermometer Score",
              ylim = c(-6, 101),
              xlab = "",
              groups = order,
              col = "black",
              type = "l",
              lty = 2,
              fill = c("dark grey"),
              upper = candpred2$upper,
              lower = candpred2$lower,
              panel = function(x, y, ...){
                      panel.superpose(x, y, panel.groups = 'my.panel.bands', ...)
                      panel.xyplot(x, y, ...)
              }
)

pdf("figure5.pdf", width = 7, height = 4)
ae1 + lr1
dev.off()

##########################################################################

####
## Figure 6
####

mfx <- read.dta("Candidate Marginal Effects.dta")

pdf("figure6.pdf", width = 8, height = 4)
xyplot(estimate ~ scale | order,
       data = mfx,
       aspect = 1,
       groups = order,
       type = "l",
       fill = c("light grey"),
       upper = mfx$upper,
       lower = mfx$lower,
       xlab = "Anti-Establishment Dimension",
       ylab = "Marginal Effect of Left-Right",
       strip=strip.custom(factor.levels=c("Donald Trump", 
                                          "Bernie Sanders",  
                                          "Joe Biden")),
       scales = list(y = list(relation="free", limit = list(c(67, 123), 
                                                            c(-90, -60),
                                                            c( -100, -45)))),
       col = "black",
       panel = function(x, y, ...){
               panel.superpose(x, y, panel.groups = 'my.panel.bands', ...)
               panel.xyplot(x, y, ...)
       }
       )
dev.off()

##########################################################################

####
## Figure 7
####

ctpred1 <- read.dta("CT Belief Predictions, AE.dta")

ae2 <- xyplot(estimate ~ scale | as.factor(order),
              data = ctpred1,
              aspect = 1,
              ylab = "Predicted Belief/Support",
              xlab = "",
              ylim = c(-.1, 0.9),
              groups = order,
              col = "black",
              type = "l",
              fill = c("light grey"),
              upper = ctpred1$upper,
              lower = ctpred1$lower,
              strip=strip.custom(factor.levels=c("Reps Steal Elections",
                                                 "Russian Asset",
                                                 "Trump Collusion",
                                                 "QAnon Support",
                                                 "COVID-19 Exaggerated",
                                                 "Birther",
                                                 "Climate Denial",
                                                 "Clinton Nuke")),
              key=list(columns=1, lines=list(lty=c(1, 2), lwd=2, col=c("black")),
                       text=list(c("Anti-Establishment", 
                                   "Left-Right"))),
              panel = function(x, y, ...){
                      panel.superpose(x, y, panel.groups = 'my.panel.bands', ...)
                      panel.xyplot(x, y, ...)
              }
)

ctpred2 <- read.dta("CT Belief Predictions, LR.dta")

lr2 <- xyplot(estimate ~ scale | as.factor(order),
              data = ctpred2,
              aspect = 1,
              xlab = "",
              groups = order,
              col = "black",
              type = "l",
              lty = 2,
              fill = c("dark grey"),
              upper = ctpred2$upper,
              lower = ctpred2$lower,
              panel = function(x, y, ...){
                      panel.superpose(x, y, panel.groups = 'my.panel.bands', ...)
                      panel.xyplot(x, y, ...)
              }
)

pdf("figure7.pdf", width = 9, height = 6)
ae2 + lr2
dev.off()

##########################################################################

####
## Appendix, Figure D1
####

# 2019
myvars <- c("pid", "ideo", "dempartyft", "reppartyft", "con1", "con2",
            "con3", "con4", "pop1", "pop2", "goodevil", "official")

items <- na.omit(ct2019[myvars])

check.scree <- fa(cor(items), fm = "pa", smc = TRUE, rotate = "none")

scree1 <- xyplot(check.scree$values ~ 1:ncol(cor(items)),
       aspect = 1,
       xlab = "Factor",
       ylab = "Eigenvalue",
       panel = function(x, y, ...){
         panel.xyplot(x, y, type="b", col="black", pch = 16)
         panel.abline(h = 1, lty = 2)
       }
)

# 2020
items2 <- na.omit(ct2020[myvars])

check.scree2 <- fa(cor(items2), fm = "pa", smc = TRUE, rotate = "none")

scree2 <- xyplot(check.scree2$values ~ 1:ncol(cor(items2)),
       aspect = 1,
       xlab = "Factor",
       ylab = "Eigenvalue",
       panel = function(x, y, ...){
         panel.xyplot(x, y, type="b", col="black", pch = 16)
         panel.abline(h = 1, lty = 2)
       }
)

pdf("figureD1.pdf", width = 8, height = 4)
print(scree1, position = c(0, 0, .5, 1), more = TRUE)
print(scree2, position = c(.5, 0, 1, 1), more = FALSE)
dev.off()

##########################################################################

####
## Appendix, Figure E1
####

dens1 <- densityplot(~suspicion2, 
            data = na.omit(ct2019[c("suspicion2", "rep")]),
            aspect = 1,
            xlab = "Anti-Establishment Dimension",
            group = rep,
            col = c("blue", "red"),
            plot.points = FALSE
)

dens2 <- densityplot(~leftright2, 
            data = na.omit(ct2019[c("leftright2", "rep")]),
            aspect = 1,
            xlab = "Left-Right Dimension",
            group = rep,
            plot.points = FALSE,
            col = c("blue", "red")
)

pdf("figureE1.pdf", width = 8, height = 4)
print(dens1, position = c(0, 0, .5, 1), more = TRUE)
print(dens2, position = c(.5, 0, 1, 1), more = FALSE)
dev.off()

##########################################################################

####
## Appendix Figure M1
####

candpred2 <- read.dta("Candidate Predictions, AE REP.dta")

ae2 <- xyplot(estimate ~ scale | as.factor(order),
              data = candpred2,
              aspect = 1,
              ylab = "Predicted Thermometer Score",
              xlab = "",
              ylim = c(-20, 110),
              col = "red3",
              type = "l",
              strip=strip.custom(factor.levels=c("Donald Trump", 
                                                 "Joe Biden")),
              key=list(columns=1, lines=list(lty=c(1, 1), col=c("red3", "blue3")),
                       text=list(c("Anti-Establishment", 
                                   "Left-Right"))),
              panel = function(x, y, subscripts, ...){
                      panel.xyplot(x, y, col=...)
                      panel.xyplot(candpred2$scale[subscripts], candpred2$lower[subscripts], 
                                   type = "l", lty=2, col="red3")
                      panel.xyplot(candpred2$scale[subscripts], candpred2$upper[subscripts], 
                                   type = "l", lty=2, col="red3")
              }
)

candpred3 <- read.dta("Candidate Predictions, LR REP.dta")

lr2 <- xyplot(estimate ~ scale | as.factor(order),
              data = candpred3,
              aspect = 1,
              ylim = c(-20, 110),
              xlab = "",
              col = "blue3",
              type = "l",
              panel = function(x, y, subscripts, ...){
                      panel.xyplot(x, y, col=...)
                      panel.xyplot(candpred3$scale[subscripts], candpred3$lower[subscripts], 
                                   type = "l", lty=2, col="blue3")
                      panel.xyplot(candpred3$scale[subscripts], candpred3$upper[subscripts], 
                                   type = "l", lty=2, col="blue3")
              }
)

pdf("figureM1.pdf", width = 7, height = 4.5)
ae2 + lr2
dev.off()

##########################################################################

####
## Appendix Figure M2
####

ctpred3 <- read.dta("CT Belief Predictions, AE REP.dta")

ae3 <- xyplot(estimate ~ scale | as.factor(order),
              data = ctpred3,
              aspect = 1,
              ylab = "Predicted Belief/Support",
              xlab = "",
              ylim = c(-.1, 0.9),
              col = "red3",
              type = "l",
              strip=strip.custom(factor.levels=c("Reps Steal Elections",
                                                 "Faking COVID-19",
                                                 "Trump Collusion",
                                                 "QAnon Support",
                                                 "COVID-19 Exaggerated",
                                                 "Climate Denial")),
              key=list(columns=1, lines=list(lty=c(1, 1), col=c("red3", "blue3")),
                       text=list(c("Anti-Establishment", 
                                   "Left-Right"))),
              panel = function(x, y, subscripts, ...){
                      panel.xyplot(x, y, col=...)
                      panel.xyplot(ctpred3$scale[subscripts], ctpred3$lower[subscripts], 
                                   type = "l", lty=2, col="red3")
                      panel.xyplot(ctpred3$scale[subscripts], ctpred3$upper[subscripts], 
                                   type = "l", lty=2, col="red3")
              }
)

ctpred4 <- read.dta("CT Belief Predictions, LR REP.dta")

lr3 <- xyplot(estimate ~ scale | as.factor(order),
              data = ctpred4,
              aspect = 1,
              xlab = "",
              col = "blue3",
              type = "l",
              panel = function(x, y, subscripts, ...){
                      panel.xyplot(x, y, col=...)
                      panel.xyplot(ctpred4$scale[subscripts], ctpred4$lower[subscripts], 
                                   type = "l", lty=2, col="blue3")
                      panel.xyplot(ctpred4$scale[subscripts], ctpred4$upper[subscripts], 
                                   type = "l", lty=2, col="blue3")
              }
)

pdf("figureM2.pdf", width = 8, height = 6)
ae3 + lr3
dev.off()
