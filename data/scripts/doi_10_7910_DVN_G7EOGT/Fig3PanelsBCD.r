###
###
### This code can be used to reproduce panels B, C, and D in Figure 3
### as well as corresponding ANAVAs.
###
###

library(optiscale)
library(lattice)
library(foreign)

# Load 2010 TESS Data
vals <- read.dta(file.choose())

# Use respondents in ranking group
vals <- vals[which(vals$rankings == 1), ]

# Delete obs where all 5 values are tied
vals <- vals[-c(557, 461, 432, 413, 389, 36), ]

# Create race categories
vals$White <- ifelse(vals$PPETHM == "White, Non-Hispanic", 1, 0)
table(vals$White)

vals$Black <- ifelse(vals$PPETHM == "Black, Non-Hispanic", 1, 0)
table(vals$Black)

vals$Hispanic <- ifelse(vals$PPETHM == "Hispanic", 1, 0)
table(vals$Hispanic)

vals$Race <- NA
vals$Race[vals$Hispanic == 1] <- "Latino"
vals$Race[vals$Black == 1] <- "African American"
vals$Race[vals$White == 1] <- "White"

# Create Party Categories
vals$party3[vals$party == 4] <- "Independent"
vals$party3[vals$party == 1 | vals$party == 2 | vals$party == 3] <- "Republican"
vals$party3[vals$party == 5 | vals$party == 6 | vals$party == 7] <- "Democrat"

# Creat Race by Party categories
vals$raceparty[vals$Race == "African American" & vals$party3 == "Democrat"] <- "AfAmDem"
vals$raceparty[vals$Race == "African American" & vals$party3 == "Independent"] <- "AfAmInd"
vals$raceparty[vals$Race == "African American" & vals$party3 == "Republican"] <- "AfAmRep"

vals$raceparty[vals$Race == "Latino" & vals$party3 == "Democrat"] <- "LatDem"
vals$raceparty[vals$Race == "Latino" & vals$party3 == "Independent"] <- "LatInd"
vals$raceparty[vals$Race == "Latino" & vals$party3 == "Republican"] <- "LatRep"

vals$raceparty[vals$Race == "White" & vals$party3 == "Democrat"] <- "WDem"
vals$raceparty[vals$Race == "White" & vals$party3 == "Independent"] <- "WInd"
vals$raceparty[vals$Race == "White" & vals$party3 == "Republican"] <- "WRep"

table(vals$party[vals$Race == "African American"])
table(vals$raceparty)

vars <- c("raceparty", "RankFree", "RankEq", "RankES", "RankLO", "RankMT")

# Cut data set to relevant variables
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

# AF AMS (Fig 3, panel D)

xyplot(V1 ~ V2, data = vals3,
       aspect = 1,
       panel = function (x, y) {
         panel.xyplot(val.coords$V1, val.coords$V2, pch = 4, 
                      cex = .5, col = "black")
         panel.text(val.coords$V1, val.coords$V2, 
                    labels = c("Fr", "Eq", "ES",  
                               "LO", "MT"),
                    pos = c(1,1,1,1,1), cex = .725)
         panel.segments(rep(0, 3), rep(0, 3), 
                        mean.pty.d1[c(1,3)], mean.pty.d2[c(1,3)], lwd = 1.5)
         panel.text(mean.pty.d1[c(1,3)], mean.pty.d2[c(1,3)], 
                    labels = c("Dem", "Rep"), pos = c(2,4), cex = 1)
       },
       xlab = "",
       ylab = "",
       scales = list(draw = FALSE)
)

# LATINOS (Fig 3, panel C)

xyplot(V1 ~ V2, data = vals3,
       aspect = 1,
       panel = function (x, y) {
         panel.xyplot(val.coords$V1, val.coords$V2, pch = 4, 
                      cex = .5, col = "black")
         panel.text(val.coords$V1, val.coords$V2, 
                    labels = c("Fr", "Eq", "ES",  
                               "LO", "MT"),
                    pos = c(1,1,1,1,1), cex = .725)
         panel.segments(rep(0, 3), rep(0, 3), 
                        mean.pty.d1[c(4,5)], mean.pty.d2[c(4,5)], lwd = 1.5)
         panel.text(mean.pty.d1[c(4,5)], mean.pty.d2[c(4,5)], 
                    labels = c("Dem", "Rep"), pos = c(3,1), cex = 1)
       },
       xlab = "",
       ylab = "",
       scales = list(draw = FALSE)
)

# WHITES (Fig. 3, panel B)

xyplot(V1 ~ V2, data = vals3,
       aspect = 1,
       panel = function (x, y) {
         panel.xyplot(val.coords$V1, val.coords$V2, pch = 4, 
                      cex = .5, col = "black")
         panel.text(val.coords$V1, val.coords$V2, 
                    labels = c("Fr", "Eq", "ES",  
                               "LO", "MT"),
                    pos = c(2,1,1,1,1), cex = .725)
         panel.segments(rep(0, 3), rep(0, 3), 
                        mean.pty.d1[c(6,8)], mean.pty.d2[c(6,8)], lwd = 1.5)
         panel.text(mean.pty.d1[c(6,8)], mean.pty.d2[c(6,8)], 
                    labels = c("Dem", "Rep"), pos = c(2,1), cex = 1)
       },
       xlab = "",
       ylab = "",
       scales = list(draw = FALSE)
)

### ANAVA ###

# BLACK #

### ANAVA ###

# BLACKS #

vals3.black <- vals3[ which(vals3$raceparty == "AfAmDem" | vals3$raceparty == "AfAmRep"), ]

meand1 <- mean(vals3.black$V1)

meand2 <- mean(vals3.black$V2)

mean.res.length <- (meand1^2 + meand2^2)^.5

res.length <- length(vals3.black$V1) * mean.res.length

party.mean.lengths <- c(.717, .046)

partyNs <- c(57, 7)

raceparty.between <- sum(party.mean.lengths * partyNs) - res.length

raceparty.within <- length(vals3.black$V1) - sum(party.mean.lengths * partyNs)

dfnum <- 1

dfdenom <- length(vals3.black$V1) - 2

raceparty.ms.between <- raceparty.between / dfnum

raceparty.ms.within <- raceparty.within / dfdenom

F.racepartyBLACK <- raceparty.ms.between / raceparty.ms.within

obs.prob.racepartyBLACK <- 1 - pf(F.racepartyBLACK, dfnum, dfdenom)

# LATINOS #

vals3.latino <- vals3[ which(vals3$raceparty == "LatDem" | vals3$raceparty == "LatRep"), ]

meand1 <- mean(vals3.latino$V1)

meand2 <- mean(vals3.latino$V2)

mean.res.length <- (meand1^2 + meand2^2)^.5

res.length <- length(vals3.latino$V1) * mean.res.length

party.mean.lengths <- c(.643, .561)

partyNs <- c(37, 20)

raceparty.between <- sum(party.mean.lengths * partyNs) - res.length

raceparty.within <- length(vals3.latino$V1) - sum(party.mean.lengths * partyNs)

dfnum <- 1

dfdenom <- length(vals3.latino$V1) - 2

raceparty.ms.between <- raceparty.between / dfnum

raceparty.ms.within <- raceparty.within / dfdenom

F.racepartyLATINO <- raceparty.ms.between / raceparty.ms.within

obs.prob.racepartyLATINO <- 1 - pf(F.racepartyLATINO, dfnum, dfdenom)


# WHITES #

vals3.white <- vals3[ which(vals3$raceparty == "WDem" | vals3$raceparty == "WRep"), ]

meand1 <- mean(vals3.white$V1)

meand2 <- mean(vals3.white$V2)

mean.res.length <- (meand1^2 + meand2^2)^.5

res.length <- length(vals3.white$V1) * mean.res.length

party.mean.lengths <- c(.646, .606)

partyNs <- c(220, 226)

raceparty.between <- sum(party.mean.lengths * partyNs) - res.length

raceparty.within <- length(vals3.white$V1) - sum(party.mean.lengths * partyNs)

dfnum <- 1

dfdenom <- length(vals3.white$V1) - 2

raceparty.ms.between <- raceparty.between / dfnum

raceparty.ms.within <- raceparty.within / dfdenom

F.racepartyWHITE <- raceparty.ms.between / raceparty.ms.within

obs.prob.racepartyWHITE <- 1 - pf(F.racepartyWHITE, dfnum, dfdenom)
