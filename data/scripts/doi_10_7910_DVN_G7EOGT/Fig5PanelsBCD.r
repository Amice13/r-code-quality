###
###
### This code can be used to reproduce panel A in Figure 5
### as well as corresponding ANAVA.
###
###

library(optiscale)
library(lattice)
library(foreign)

# Load 2002 TESS data
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

# Delete NAs
vals <- na.omit(vals)

# Create Party Vars
vals$party3 <- NA
vals$party3[vals$party == 4] <- "Independent"
vals$party3[vals$party == 5 | vals$party == 6 | vals$party == 7] <- "Democrat"
vals$party3[vals$party == 1 | vals$party == 2 | vals$party == 3] <- "Republican"

# Delete NAs
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

# Create Race by Party Vars
vals$raceparty[vals$Race == "African American" & vals$party3 == "Democrat"] <- "AfAmDem"
vals$raceparty[vals$Race == "African American" & vals$party3 == "Independent"] <- "AfAmInd"
vals$raceparty[vals$Race == "African American" & vals$party3 == "Republican"] <- "AfAmRep"

vals$raceparty[vals$Race == "Latino" & vals$party3 == "Democrat"] <- "LatDem"
vals$raceparty[vals$Race == "Latino" & vals$party3 == "Independent"] <- "LatInd"
vals$raceparty[vals$Race == "Latino" & vals$party3 == "Republican"] <- "LatRep"

vals$raceparty[vals$Race == "White" & vals$party3 == "Democrat"] <- "WDem"
vals$raceparty[vals$Race == "White" & vals$party3 == "Independent"] <- "WInd"
vals$raceparty[vals$Race == "White" & vals$party3 == "Republican"] <- "WRep"

table(vals$raceparty)

# Trim data set to relevant vars
vars <- c("raceparty", "RankFree6", "RankEq6", "RankES6", "RankLO6", "RankMT6", "RankPat6")

vals <- vals[vars]

# Take out NAs
vals <- na.omit(vals)

newvars <- c("RankFree6", "RankEq6", "RankES6", "RankLO6", "RankMT6", "RankPat6")

# Vector Model
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

demos <- c("raceparty")

vals2 <- vals[demos]

vals3 <- data.frame(cbind(vals2, subj.coord))

# Now calculate mean vectors for respondents in the three categories. Also
# calculate mean resultant lengths for the category vectors.

mean.pty.d1 <- tapply(vals3$V1, vals3$raceparty, mean, na.rm = T)

mean.pty.d2 <- tapply(vals3$V2, vals3$raceparty, mean, na.rm = T)

pty.mean.lengths <- (mean.pty.d1^2 + mean.pty.d2^2)^.5

###
###   Plot mean vectors for party identification
###   categories, along with value points
###
###   
###

# African Americans (Fig. 5, Panel D) #

xyplot(V1 ~ V2, data = vals3,
       aspect = 1,
       panel = function (x, y) {
         panel.xyplot(val.coords$V1, val.coords$V2, pch = 4, 
                      cex = .5, col = "black")
         panel.text(val.coords$V1, val.coords$V2, 
                    labels = c("Lbty", "Eq", "ES",  
                               "SO      ", "Mor", "Pat  "),
                    pos = c(1,1,1,3,2,3), cex = .725)
         panel.segments(rep(0, 3), rep(0, 3), 
                        mean.pty.d1[c(1,2,3)], mean.pty.d2[c(1,2,3)], lwd = 1.5)
         panel.text(mean.pty.d1[c(1,2,3)], mean.pty.d2[c(1,2,3)], 
                    labels = c("       Dem", "Ind    ", "Rep"), pos = c(1,1,1), cex = 1)
       },
       xlab = "",
       ylab = "",
       scales = list(draw = FALSE)
)

# Latinos (Fig. 5, Panel C) #

xyplot(V1 ~ V2, data = vals3,
       aspect = 1,
       panel = function (x, y) {
         panel.xyplot(val.coords$V1, val.coords$V2, pch = 4, 
                      cex = .5, col = "black")
         panel.text(val.coords$V1, val.coords$V2, 
                    labels = c("Lbty", "Eq", "ES",  
                               "SO", "Mor", "Pat"),
                    pos = c(1,1,1,1,1,4), cex = .725)
         panel.segments(rep(0, 3), rep(0, 3), 
                        mean.pty.d1[c(4,5,6)], mean.pty.d2[c(4,5,6)], lwd = 1.5)
         panel.text(mean.pty.d1[c(4,5,6)], mean.pty.d2[c(4,5,6)], 
                    labels = c("Dem", "Ind", "Rep"), pos = c(3,2,2), cex = 1)
       },
       xlab = "",
       ylab = "",
       scales = list(draw = FALSE)
)

# Whites (Fig. 5, Panel B) #

xyplot(V1 ~ V2, data = vals3,
       aspect = 1,
       panel = function (x, y) {
         panel.xyplot(val.coords$V1, val.coords$V2, pch = 4, 
                      cex = .5, col = "black")
         panel.text(val.coords$V1, val.coords$V2, 
                    labels = c("Lbty", "Eq", "ES",  
                               "SO", "Mor", "Pat"),
                    pos = c(1,1,1,1,1,4), cex = .725)
         panel.segments(rep(0, 3), rep(0, 3), 
                        mean.pty.d1[c(7,8,9)], mean.pty.d2[c(7,8,9)], lwd = 1.5)
         panel.text(mean.pty.d1[c(7,8,9)], mean.pty.d2[c(7,8,9)], 
                    labels = c("Dem", "Ind", "Rep"), pos = c(3,2,2), cex = 1)
       },
       xlab = "",
       ylab = "",
       scales = list(draw = FALSE)
)

# ANAVA #

# African Americans #

vals3.black <- vals3[ which(vals3$raceparty == "AfAmDem" | 
                              vals3$raceparty == "AfAmInd" | vals3$raceparty == "AfAmRep"), ]

meand1 <- mean(vals3.black$V1)

meand2 <- mean(vals3.black$V2)

mean.res.length <- (meand1^2 + meand2^2)^.5

res.length <- length(vals3.black$V1) * mean.res.length

party.mean.lengths <- c(.289, .202, .444)

raceparty.between <- sum(party.mean.lengths * as.vector(table(vals3.black$raceparty))) - res.length

raceparty.within <- length(vals3.black$V1) - sum(party.mean.lengths * as.vector(table(vals3.black$raceparty)))

dfnum <- 2

dfdenom <- length(vals3.black$V1) - 3

raceparty.ms.between <- raceparty.between / dfnum

raceparty.ms.within <- raceparty.within / dfdenom

F.racepartyBLACK <- raceparty.ms.between / raceparty.ms.within

obs.prob.racepartyBLACK <- 1 - pf(F.racepartyBLACK, dfnum, dfdenom)

# LATINOS #

vals3.latino <- vals3[ which(vals3$raceparty == "LatDem" | 
                               vals3$raceparty == "LatInd" | vals3$raceparty == "LatRep"), ]

meand1 <- mean(vals3.latino$V1)

meand2 <- mean(vals3.latino$V2)

mean.res.length <- (meand1^2 + meand2^2)^.5

res.length <- length(vals3.latino$V1) * mean.res.length

party.mean.lengths <- c(.120, .297, .582)

raceparty.between <- sum(party.mean.lengths * as.vector(table(vals3.latino$raceparty))) - res.length

raceparty.within <- length(vals3.latino$V1) - sum(party.mean.lengths * as.vector(table(vals3.latino$raceparty)))

dfnum <- 2

dfdenom <- length(vals3.latino$V1) - 3

raceparty.ms.between <- raceparty.between / dfnum

raceparty.ms.within <- raceparty.within / dfdenom

F.racepartyLATINO <- raceparty.ms.between / raceparty.ms.within

obs.prob.racepartyLATINO <- 1 - pf(F.racepartyLATINO, dfnum, dfdenom)

# WHITES #

vals3.white <- vals3[ which(vals3$raceparty == "WDem" | 
                              vals3$raceparty == "WInd" | vals3$raceparty == "WRep"), ]

meand1 <- mean(vals3.white$V1)

meand2 <- mean(vals3.white$V2)

mean.res.length <- (meand1^2 + meand2^2)^.5

res.length <- length(vals3.white$V1) * mean.res.length

party.mean.lengths <- c(.295, .399, .696)

raceparty.between <- sum(party.mean.lengths * as.vector(table(vals3.white$raceparty))) - res.length

raceparty.within <- length(vals3.white$V1) - sum(party.mean.lengths * as.vector(table(vals3.white$raceparty)))

dfnum <- 2

dfdenom <- length(vals3.white$V1) - 3

raceparty.ms.between <- raceparty.between / dfnum

raceparty.ms.within <- raceparty.within / dfdenom

F.racepartyWHITE <- raceparty.ms.between / raceparty.ms.within

obs.prob.racepartyWHITE <- 1 - pf(F.racepartyWHITE, dfnum, dfdenom)


