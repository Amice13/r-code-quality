#####
## Effects of Impeachment Trial on
## Attitudes about the Court
#####

library(foreign)
library(lattice)
library(latticeExtra)
library(grid)
library(ggplot2)
library(ggthemes)

#####
## Plot of JGR actions on diffuse and acceptance
## (saved as "jgroutcomes," 6X4.5)
####

jgr <- read.csv("jgr actions.csv")

jgr$cond[jgr$dv=="Acceptance"] <- 1
jgr$cond[jgr$dv=="Diffuse Support"] <- 2

xyplot(est ~ as.factor(order) | dv, 
       data = jgr,
       aspect = 1,
       col = "black",
       group = dv,
       scond = jgr$cond,
       ylab = "Treatment Effect",
       xlab = "",
       scales=list(
        x=list(
          rot = 45,
          at=c(1, 2, 3, 4),
          labels=c("JGR Admonish",
                   "JGR Break Protocol", "Whistleblower"),
          alternating=FALSE)),
       pch = 16,
       ylim = c(-.15, .05),
       layout = c(2,1),
       panel = panel.superpose,
       panel.groups = function(x, y, subscripts, groups, col, lty, scond, ...) {
         pnl = panel.number()
         panel.xyplot(x, y, pch=...)
         panel.abline(h=0, lty = 3, col = "darkgrey")
         panel.segments(x0=jgr$order[scond==pnl], y0=jgr$lb[scond==pnl],
                        x1=jgr$order[scond==pnl], y1=jgr$ub[scond==pnl], col = "black")
       }
)

##########################################################################

####
## JGR actions, interactive
####

jgr2 <- read.csv("jgr actions, inter.csv")

ggplot(jgr2, aes(x = pid, y = est, ymin = lb, ymax = ub)) +
  geom_pointrange() +
  facet_wrap(dv~treat) +
  theme(aspect.ratio = 1) +
  theme_few() +
  scale_x_discrete("") +
  scale_y_continuous("Treatment Effect") +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "gray", size=.5)

######
## Warren's statement
######

war <- read.csv("warren full.csv")
war2 <- war[1:2,]
xyplot(est ~ as.factor(order), 
       data = war2,
       aspect = 1,
       col = "black",
       ylab = "Treatment Effect",
       xlab = "",
       group = order,
       scales=list(
         x=list(
           rot=45,
          labels=c("Acceptance", "Diffuse Support"))),
       pch = 16,
       ylim = c(-.155, .055),
       #strip=strip.custom(factor.levels=c("Specific", "Diffuse")),
       panel = panel.superpose,
       panel.groups = function(x, y, subscripts, groups, col, lty, ...) {
         pnl = panel.number()
         panel.xyplot(x, y, pch=...)
         panel.abline(h=0, lty = 3, col = "darkgrey")
         panel.segments(x0=war$order, y0=war$lb,
                        x1=war$order, y1=war$ub, col = "black")
       }
)

