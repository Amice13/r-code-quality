###
###
### This code can be used to reproduce panel A in Figure 3
### as well as corresponding ANAVA.
###
###

library(optiscale)
library(lattice)
library(foreign)

# Load 2010 TESS Data
vals <- read.dta(file.choose())

# Use only respondents in ranking group
vals <- vals[which(vals$rankings == 1), ]

# Delete respondents that do not differentiate between values (all 5 values tied)
vals <- vals[-c(557, 461, 432, 413, 389, 36), ]

# Create Party Variable
vals$party3[vals$party == 4] <- "Independent"
vals$party3[vals$party == 1 | vals$party == 2 | vals$party == 3] <- "Republican"
vals$party3[vals$party == 5 | vals$party == 6 | vals$party == 7] <- "Democrat"

table(vals$party3)

vars <- c("party3", "RankFree", "RankEq", "RankES", "RankLO", "RankMT")

# Cut data set to relevant vars
vals <- vals[vars]

# Make sure there are no NAs
vals <- na.omit(vals)

newvars <- c("RankFree", "RankEq", "RankES", "RankLO", "RankMT")

# Vector model 
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
###   individual vectors and value points *does not appear in article*
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

demos <- c("party3")

vals2 <- vals[demos]

vals3 <- data.frame(cbind(vals2, subj.coord))

# Now calculate mean vectors for respondents in the three categories. Also
# calculate mean resultant lengths for the category vectors.

mean.pty.d1 <- tapply(vals3$V1, vals3$party3, mean, na.rm = T)

mean.pty.d2 <- tapply(vals3$V2, vals3$party3, mean, na.rm = T)

pty.mean.lengths <- (mean.pty.d1^2 + mean.pty.d2^2)^.5

###
###   Plot mean vectors for party identification
###   categories, along with value points (Fig. 3, Panel A)
###
###   
###

xyplot(V1 ~ V2, data = vals3,
       aspect = 1,
       panel = function (x, y) {
         panel.xyplot(val.coords$V1, val.coords$V2, pch = 4, 
                      cex = .5, col = "black")
         panel.text(val.coords$V1, val.coords$V2, 
                    labels = c("Fr", "Eq", "ES",  
                               "LO", "MT"),
                    pos = c(2, 1, 1, 1, 1), cex = .725)
         panel.segments(rep(0, 3), rep(0, 3), 
                        mean.pty.d1[c(1,3)], mean.pty.d2[c(1,3)], lwd = 1.5)
         panel.text(mean.pty.d1[c(1,3)], mean.pty.d2[c(1,3)], 
                    labels = c("Dem", "Rep"), pos = c(3,1), cex = 1)
       },
       xlab = "",
       ylab = "",
       scales = list(draw = FALSE)
)

## ANAVA ##

meand1 <- mean(vals3$V1)

meand2 <- mean(vals3$V2)

mean.res.length <- (meand1^2 + meand2^2)^.5

res.length <- length(vals3$V1) * mean.res.length

party.mean.lengths <- c(.651, .581)

partyNs <- c(341, 264)

party.between <- sum(party.mean.lengths * partyNs) - res.length

party.within <- length(vals3$V1) - sum(party.mean.lengths * partyNs)

dfnum <- 1

dfdenom <- length(vals3$V1) - 2

party.ms.between <- party.between / dfnum

party.ms.within <- party.within / dfdenom

F.party <- party.ms.between / party.ms.within

obs.prob.party <- 1 - pf(F.party, dfnum, dfdenom)
