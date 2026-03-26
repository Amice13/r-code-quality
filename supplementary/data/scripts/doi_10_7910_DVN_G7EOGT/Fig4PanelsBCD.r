###
###
### This code can be used to reproduce panels B, C, and D in Figure 4
### as well as corresponding ANAVAs.
###
###

library(optiscale)
library(lattice)
library(foreign)

# Load 2002 TESS Data
vals <- read.dta(file.choose())

# Using this set of commands to identify
# respondents that ranked all values equally,
# then deleting those obs. from data
vals$del <- 0
vals$del[vals$RankFree6 == vals$RankEq6 & 
           vals$RankFree6 == vals$RankES6 &
           vals$RankFree6 == vals$RankLO6 &
           vals$RankFree6 == vals$RankMT6 &
           vals$RankFree6 == vals$RankPat6] <- NA

# Make sure no NAs in Data
vals <- na.omit(vals)

# Create Ideology Variable
vals$ideol3 <- NA
vals$ideol3[vals$ideol == 4] <- "Moderate"
vals$ideol3[vals$ideol == 5 | vals$ideol == 6 | vals$ideol == 7] <- "Liberal"
vals$ideol3[vals$ideol == 1 | vals$ideol == 2 | vals$ideol == 3] <- "Conservative"

# Again, make sure no NAs
vals <- na.omit(vals)

# Create Race Vars
vals$Black <- NA
vals$Black[vals$PPETH == "Black, Non-Hispanic"] <- 1
vals$Black[vals$PPETH == "Hispanic"] <- 0
vals$Black[vals$PPETH == "White, Non-Hispanic"] <- 0

vals$Hispanic <- NA
vals$Hispanic[vals$PPETH == "Black, Non-Hispanic"] <- 0
vals$Hispanic[vals$PPETH == "Hispanic"] <- 1
vals$Hispanic[vals$PPETH == "White, Non-Hispanic"] <- 0

vals$White <- NA
vals$White[vals$PPETH == "Black, Non-Hispanic"] <- 0
vals$White[vals$PPETH == "Hispanic"] <- 0
vals$White[vals$PPETH == "White, Non-Hispanic"] <- 1

vals$Race <- NA
vals$Race[vals$Hispanic == 1] <- "Latino"
vals$Race[vals$Black == 1] <- "African American"
vals$Race[vals$White == 1] <- "White"

# Create Race by Ideology Var
vals$raceideol[vals$Race == "African American" & vals$ideol3 == "Liberal"] <- "AfAmLib"
vals$raceideol[vals$Race == "African American" & vals$ideol3 == "Moderate"] <- "AfAmMod"
vals$raceideol[vals$Race == "African American" & vals$ideol3 == "Conservative"] <- "AfAmCons"

vals$raceideol[vals$Race == "Latino" & vals$ideol3 == "Liberal"] <- "LatLib"
vals$raceideol[vals$Race == "Latino" & vals$ideol3 == "Moderate"] <- "LatMod"
vals$raceideol[vals$Race == "Latino" & vals$ideol3 == "Conservative"] <- "LatCons"

vals$raceideol[vals$Race == "White" & vals$ideol3 == "Liberal"] <- "WLib"
vals$raceideol[vals$Race == "White" & vals$ideol3 == "Moderate"] <- "WMod"
vals$raceideol[vals$Race == "White" & vals$ideol3 == "Conservative"] <- "WCons"

table(vals$raceideol)

vars <- c("raceideol", "RankFree6", "RankEq6", "RankES6", "RankLO6", "RankMT6", "RankPat6")

vals <- vals[vars]

vals <- na.omit(vals)

newvars <- c("RankFree6", "RankEq6", "RankES6", "RankLO6", "RankMT6", "RankPat6")

# Starting Vector Model
values <- vals[newvars]

values[1:10, ]

###
###   Standardize data within rows
###

vals.std <- t(apply(values, 1, scale))

vals.std[1:10,]

###
###   Initialize the matrix of optimally-scaled
###   values, the previous fit, the iteration
###   number, and the improvement in fit over
###   the previous iteration
###

vals.os <- vals.std

prev.fit2 <- 0

niter <- 0

improve = 1

###
###   Start iterations
###

while (improve > .01 & niter <= 25) {
  niter <- niter + 1
  
  ###
  ###   Perform SVD on OS version of rank-orders
  ###
  
  decomp <- svd(vals.os)
  
  ###
  ###   Calculate 2-dimensional goodness-of-fit
  ###   and improvement in fit over previous iteration
  ###
  
  d.sqd <- decomp$d^2
  
  fit2 <- sum(d.sqd[1:2]) / sum(d.sqd)
  
  fitvector <- cumsum(d.sqd) / rep(sum(d.sqd), length(d.sqd))
  
  improve <- fit2 - prev.fit2
  
  ###
  ###   Create iteration history
  ###
  
  if (niter == 1) {
    history <- c(niter, fit2, improve)
  }
  if (niter > 1)  {
    history <- rbind(history, c(niter, fit2, improve))
  }
  
  ###
  ###   Obtain terminal points of vectors for row objects
  ###   (respondents in this case), calculate predicted
  ###   ranks, norm the row object vectors to unit length
  ###
  
  subj.c1 <- decomp$u[, 1:2] %*% diag(decomp$d[1:2])
  
  pred.vals <- subj.c1 %*% t(decomp$v[,1:2])
  
  sumsqd <- apply(subj.c1^2, 1, sum)
  
  root.sumsqd <- (matrix(sumsqd, nrow = length(sumsqd), ncol = 1)) ^ .5
  
  subj.coord <- subj.c1 / (root.sumsqd %*% matrix(1, nrow = 1, ncol = 2))
  
  ###
  ###   Obtain new optimally scaled data values,
  ###   using predicted ranks from fitted model
  ###
  
  for (i in 1:nrow(vals.os)) {
    opped <- opscale(x.qual = vals.std[i,],
                     x.quant = pred.vals[i,],
                     level = 2,
                     process = 1,
                     rescale = T)
    vals.os[i,] <- opped$os
  }
  
  ###
  ###   Set current fit to previous fit for next iteration
  ###
  
  
  prev.fit2 <- fit2
  
}

###
###   Print iteration history
###

history

###
###   The "fitvector" object shows the R-squared
###   for the solution in each dimensionality
###

fitvector

###
###   Plot value points
###

val.coords <- as.data.frame(decomp$v[, 1:2])

rownames(val.coords) <- colnames(values)

val.coords

xyplot(V2 ~ V1, val.coords,
       aspect = 1,
       xlim = c(-.99, .99),
       ylim = c(-.99, .99),
       panel = function (x, y) {
         panel.xyplot(x, y, col = "black")
         panel.text(x, y, labels = rownames(val.coords),
                    pos = 1, cex = .75)
       }
)

###
###   Plot terminal points of row object vectors
###   in same space as value points
###

subj.coord <- as.data.frame(subj.coord)

set.seed(123)
xyplot(V2 ~ V1, val.coords,
       aspect = 1,
       xlim = c(-1.2, 1.2),
       ylim = c(-1.2, 1.2),
       panel = function (x, y) {
         panel.xyplot(x, y, col = "black", pch = 16)
         panel.xyplot(jitter(subj.coord$V1, amount = .05), 
                      jitter(subj.coord$V2, amount = .05), col = "black")
         panel.text(x, y, labels = rownames(val.coords),
                    pos = 1, cex = .75)
       }
)

###
###   Calculate overall mean direction and
###   mean resultant length
###

meand1 <- mean(subj.coord$V1)

meand2 <- mean(subj.coord$V2)

meand1 

meand2

mean.length <- (meand1^2 + meand2^2)^.5

mean.length

###
###   Plot mean vector along with 
###   individual vectors and value points *not included in article*
###

set.seed(123)
xyplot(V2 ~ V1, val.coords,
       aspect = 1,
       xlim = c(-1.2, 1.2),
       ylim = c(-1.2, 1.2),
       panel = function (x, y) {
         panel.xyplot(x, y, col = "black", pch = 16)
         panel.xyplot(jitter(subj.coord$V1, amount = .05), 
                      jitter(subj.coord$V2, amount = .05), col = "black")
         panel.text(x, y, labels = rownames(val.coords),
                    pos = c(4,1,3,3,2), cex = .75)
         panel.arrows(0, 0, meand1, meand2, angle = 15, length = .15)
       }
)

### Plotting Vectors ###

### merging some data ###
# val.coords is vector model results, plotting points for values in 2d space

demos <- c("raceideol")

vals2 <- vals[demos]

vals3 <- data.frame(cbind(vals2, subj.coord))

# Now calculate mean vectors for respondents in the three categories. Also
# calculate mean resultant lengths for the category vectors.

mean.pty.d1 <- tapply(vals3$V1, vals3$raceideol, mean, na.rm = T)

mean.pty.d2 <- tapply(vals3$V2, vals3$raceideol, mean, na.rm = T)

pty.mean.lengths <- (mean.pty.d1^2 + mean.pty.d2^2)^.5

###
###   Plot mean vectors for party identification
###   categories, along with value points
###
###   
###

# AF AMS (Fig. 4, Panel D) #

xyplot(V1 ~ V2, data = vals3,
       aspect = 1,
       panel = function (x, y) {
         panel.xyplot(val.coords$V1, val.coords$V2, pch = 4, 
                      cex = .5, col = "black")
         panel.text(val.coords$V1, val.coords$V2, 
                    labels = c("Lbty", "Eq", "ES",  
                               "SO", "Mor", "Pat"),
                    pos = c(1, 1, 1, 1, 2, 3), cex = .725)
         panel.segments(rep(0, 3), rep(0, 3), 
                        mean.pty.d1[c(1,2,3)], mean.pty.d2[c(1,2,3)], lwd = 1.5)
         panel.text(mean.pty.d1[c(1,2,3)], mean.pty.d2[c(1,2,3)], 
                    labels = c("Cons", "Lib", "Mod"), pos = c(1,4,4), cex = 1)
       },
       xlab = "",
       ylab = "",
       scales = list(draw = FALSE)
)

# LATINOS (Fig. 4, Panel C) 

xyplot(V1 ~ V2, data = vals3,
       aspect = 1,
       panel = function (x, y) {
         panel.xyplot(val.coords$V1, val.coords$V2, pch = 4, 
                      cex = .5, col = "black")
         panel.text(val.coords$V1, val.coords$V2, 
                    labels = c("Lbty", "Eq", "ES",  
                               "SO", "Mor", "Pat"),
                    pos = c(1, 1, 1, 1, 1, 4), cex = .725)
         panel.segments(rep(0, 3), rep(0, 3), 
                        mean.pty.d1[c(4,5,6)], mean.pty.d2[c(4,5,6)], lwd = 1.5)
         panel.text(mean.pty.d1[c(4,5,6)], mean.pty.d2[c(4,5,6)], 
                    labels = c("Cons", "Lib", "Mod"), pos = c(2,3,2), cex = 1)
       },
       xlab = "",
       ylab = "",
       scales = list(draw = FALSE)
)

# WHITES (Fig. 4, Panel B)

xyplot(V1 ~ V2, data = vals3,
       aspect = 1,
       panel = function (x, y) {
         panel.xyplot(val.coords$V1, val.coords$V2, pch = 4, 
                      cex = .5, col = "black")
         panel.text(val.coords$V1, val.coords$V2, 
                    labels = c("Lbty", "Eq", "ES",  
                               "SO", "Mor", "Pat"),
                    pos = c(1, 1, 1, 1, 1, 4), cex = .725)
         panel.segments(rep(0, 3), rep(0, 3), 
                        mean.pty.d1[c(7,8,9)], mean.pty.d2[c(7,8,9)], lwd = 1.5)
         panel.text(mean.pty.d1[c(7,8,9)], mean.pty.d2[c(7,8,9)], 
                    labels = c("Cons", "Lib", "Mod"), pos = c(2,3,3), cex = 1)
       },
       xlab = "",
       ylab = "",
       scales = list(draw = FALSE)
)

### ANAVA ###

# AF AMS #

vals3.black <- vals3[ which(vals3$raceideol == "AfAmLib" | 
                              vals3$raceideol == "AfAmMod" | vals3$raceideol == "AfAmCons"), ]

meand1 <- mean(vals3.black$V1)

meand2 <- mean(vals3.black$V2)

mean.res.length <- (meand1^2 + meand2^2)^.5

res.length <- length(vals3.black$V1) * mean.res.length

ideol.mean.lengths <- c(.279, .235, .324)

raceideol.between <- sum(ideol.mean.lengths * as.vector(table(vals3.black$raceideol))) - res.length

raceideol.within <- length(vals3.black$V1) - sum(ideol.mean.lengths * as.vector(table(vals3.black$raceideol)))

dfnum <- 2

dfdenom <- length(vals3.black$V1) - 3

raceideol.ms.between <- raceideol.between / dfnum

raceideol.ms.within <- raceideol.within / dfdenom

F.raceideolBLACK <- raceideol.ms.between / raceideol.ms.within

obs.prob.raceideolBLACK <- 1 - pf(F.raceideolBLACK, dfnum, dfdenom)

# LATINOS #

vals3.latino <- vals3[ which(vals3$raceideol == "LatLib" | 
                               vals3$raceideol == "LatMod" | vals3$raceideol == "LatCons"), ]

meand1 <- mean(vals3.latino$V1)

meand2 <- mean(vals3.latino$V2)

mean.res.length <- (meand1^2 + meand2^2)^.5

res.length <- length(vals3.latino$V1) * mean.res.length

ideol.mean.lengths <- c(.545, .117, .210)

raceideol.between <- sum(ideol.mean.lengths * as.vector(table(vals3.latino$raceideol))) - res.length

raceideol.within <- length(vals3.latino$V1) - sum(ideol.mean.lengths * as.vector(table(vals3.latino$raceideol)))

dfnum <- 2

dfdenom <- length(vals3.latino$V1) - 3

raceideol.ms.between <- raceideol.between / dfnum

raceideol.ms.within <- raceideol.within / dfdenom

F.raceideolLATINO <- raceideol.ms.between / raceideol.ms.within

obs.prob.raceideolLATINO <- 1 - pf(F.raceideolLATINO, dfnum, dfdenom)

# WHITES #

vals3.white <- vals3[ which(vals3$raceideol == "WLib" | 
                              vals3$raceideol == "WMod" | vals3$raceideol == "WCons"), ]

meand1 <- mean(vals3.white$V1)

meand2 <- mean(vals3.white$V2)

mean.res.length <- (meand1^2 + meand2^2)^.5

res.length <- length(vals3.white$V1) * mean.res.length

ideol.mean.lengths <- c(.703, .296, .403)

raceideol.between <- sum(ideol.mean.lengths * as.vector(table(vals3.white$raceideol))) - res.length

raceideol.within <- length(vals3.white$V1) - sum(ideol.mean.lengths * as.vector(table(vals3.white$raceideol)))

dfnum <- 2

dfdenom <- length(vals3.white$V1) - 3

raceideol.ms.between <- raceideol.between / dfnum

raceideol.ms.within <- raceideol.within / dfdenom

F.raceideolWHITE <- raceideol.ms.between / raceideol.ms.within

obs.prob.raceideolWHITE <- 1 - pf(F.raceideolWHITE, dfnum, dfdenom)