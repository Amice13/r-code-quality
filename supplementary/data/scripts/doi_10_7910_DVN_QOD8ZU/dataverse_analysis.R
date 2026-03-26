## Replicate analyses reported in paper:
## Montero-Melis, Jaeger & Bylund (accepted) "Thinking is modulated by recent
## linguistic experience: Second language priming affects perceived event 
## similarity", Language Learning.


# load necessary libraries
library("plyr")
library("ggplot2")
library("lme4")

###########################################################
## Process arrangement data to get pairwise similarity measures
###########################################################

# The raw data consists of X- and Y-coordinates in which the items were
# placed during an arrangement block. We first extract similarity measures
# from these coordinates:

# Read the file with the arrangement data
# NB: For this and all following files to be loaded, adjust the path as needed!
coord <- read.csv("../data/dataverse_data_arrangement_coordinates_processed.csv")

# this function computes the distances between video pairs:
mydist <- function(df){
  mymatrix <- as.matrix(df[, c("X", "Y")])
  d <- dist(mymatrix)
  as.vector(d)
}
# This function outputs a vector with the corresponding names of video pairs
mypairs <- function(x) {
  le <- length(x)
  out <- c()
  for (i in 1:(le-1)){
    curr <- paste(x[i], x[(i+1):le], sep = ":")
    out <- c(out, curr)
  }
  out
}
# create vectors for the pairs and their distances
pairs_v <- unlist(by(coord[, "Item"], coord[, c("Subj_ID", "Block")], mypairs))
dist_v <- unlist(by(coord, coord[, c("Subj_ID", "Block")], mydist))
subj_uni <- unique(coord$Subj_ID)

# put into data frame (note the order of the by() output)
dist <- data.frame(
  Subj_ID = rep(subj_uni, each = 496),
  Block = rep(1:3, each = length(subj_uni) * 496),
  Item = pairs_v,
  Dist = dist_v
)

# Function to transform distance to similarity measure between 0 (minimal
# similarity) and 1 (maximal similarity) -- to be applied to each participant-
# block combination
mysim <- function(df){
  di <- df[["Dist"]]
  maxdist <- max(di)
  sim <- 1 - (di / maxdist)
  sim
}

# compute as vector and add to data frame
sim_v <- unlist(by(dist, dist[, c("Subj_ID", "Block")], mysim))
dist$Sim <- sim_v

# clean up
rm(mydist, mypairs, pairs_v, dist_v, subj_uni, mysim, sim_v)

# sort the data frame
dist <- dist[with(dist, order(Subj_ID, Block, Item)), ]


###########################################################
## Add predictors of similarity derived from item features
###########################################################

# In the dist data frame we now have one row for each pair of items.
# We want to add which features each pair shares and which ones it doesn't
# share. For convenience, this has been saved onto a file which we now load.
# vpairs has 496 rows, one for each pairwise combination of the 32 target items.
vpairs <- read.csv("../data/dataverse_videopairs.csv")
head(vpairs)
# 1 means that the two items share this feature, 0 means they don't-
# E.g., the two items in the first row (items pgd_valcol and pgd_valnei) 
# share Path, Manner and Object, but they differ as to Direction and Ground:
head(vpairs, 1)

# We now add this info to dist
dist <- join(dist, vpairs)
# items that have NA's are pairs of same videos...
head(dist[is.na(dist$Path),])
# so they share all components:
dist[is.na(dist$Path), c("Path", "Manner", "Direction",
                         "Object", "Ground")] <- 1
sum(is.na(dist$Path))  # we've removed them all


###########################################################
## Add participant descriptors
###########################################################

# # now add participant information
ppts <- read.csv("../data/dataverse_data_subject_info.csv", fileEncoding = "UTF-8")
head(ppts)
# join
dist <- join(dist, ppts)
head(dist)


###########################################################
## No differences in L2 proficiency between conditions
###########################################################

# A one-way ANOVA, with cloze test scores as the dependent measure and prime
# condition as the independent factor, yields no significant difference in
# proficiency between the three conditions:
# run anova
aov_cloze <- aov(ClozeScore ~ Condition, data = ppts)
summary(aov_cloze)
# graphically
ggplot(ppts, aes(x = Condition, y = ClozeScore)) + geom_boxplot()


###############################################################
## Get data set into right shape for fitting mixed model
###############################################################

# F Jaegers convenience function to center categorical predictors
# http://hlplab.wordpress.com/2009/04/27/centering-several-variables/
myCenter= function(x) {
  if (is.numeric(x)) { return(x - mean(x, na.rm=T)) }
  if (is.factor(x)) {
    x= as.numeric(x)
    return(x - mean(x, na.rm=T))
  }
  if (is.data.frame(x) || is.matrix(x)) {
    m= matrix(nrow=nrow(x), ncol=ncol(x))
    #     colnames(m)= paste("c", colnames(x), sep="")
    colnames(m) <- colnames(x)
    for (i in 1:ncol(x)) {
      m[,i]= myCenter(x[,i])
    }
    return(as.data.frame(m))
  }
}

# simplify predictor names for ease of reference
names(dist)[6:10] <- c("P", "M", "Di", "Ob", "Gr")

# sensible order for Condition
dist$Condition <- factor(dist$Condition, levels = c("PathPrime", "Control",
                                                    "MannerPrime"))

# define contrasts for Blocks and Condition (forward coding)
cm <- matrix(c(1/3, 1/3, 1/3,
               1, -1, 0,
               0, 1, -1), 3, byrow=T)
myforward <- solve(cm)[, 2:3]
# Block
dist$Block <- factor(dist$Block)
contrasts(dist$Block) <- myforward
colnames(contrasts(dist$Block)) <- c("1_vs_2", "2_vs_3")
contrasts(dist$Block)
# Condition
# NB: Forward coding is only used when all three conditions go into the
# analysis (second analysis reported below and in the paper); for the main
# analysis between primed conditions normal centering is used.
contrasts(dist$Condition) <- myforward
colnames(contrasts(dist$Condition)) <- c("P_vs_C", "C_vs_M")
contrasts(dist$Condition)

# clean up
rm(myforward, cm)


###############################################################
## Fit mixed model for manipulated conditions (and time the process)
###############################################################

# We fit the model comparing only the primed conditions (Path and Manner).
# This is the main analysis of the paper.

# Subset data for the main results section:
# keep manipulated conditions only (Path and Manner primed), remove control
dist_primed <- dist[dist$Condition %in% c("MannerPrime", "PathPrime"), ]
# Drop unused factor levels from Condition
dist_primed$Condition <- factor(dist_primed$Condition)
# select order of factor levels, so that they remain sensible after centering
dist_primed$Condition <- factor(dist_primed$Condition, 
                                levels = c("PathPrime", "MannerPrime"))

# center predictors
tocenter <- names(dist_primed)[6:11]
grouping <- names(dist_primed)[c(1:3,5)]
# dataframe with centered data ready for fitting mixed model
data_pri_c <- cbind(dist_primed[, grouping], myCenter(dist_primed[, tocenter]))
head(data_pri_c)
rm(tocenter, grouping, dist_primed)

# NB: the model might take long time to converge, might be close to 24 hours.

# start counting time
ptm <- proc.time()

# fit model
fm_manip <- lmer(
  Sim ~ 1 + (Gr + Ob + Di + P + M) * Block * Condition +
    (1 + Gr + Ob + Di + (P + M) * Block | Subj_ID) + (1 + Condition | Item),
  data = data_pri_c, verbose = 2, 
  control = lmerControl(optCtrl = list(maxfun = 200000)))

# how much time did it take?
t.fm_manip <- proc.time() - ptm
print(t.fm_manip)

# model summary
summary(fm_manip)


###############################################################
## Fit mixed model for all three conditions (and time the process)
###############################################################

# We fit the model comparing all three conditions: the primed conditions
# (Path and Manner) and the control condition (no priming).
# This is the second analysis reported in the paper.

# center predictors
tocenter <- names(dist)[6:10]
grouping <- names(dist)[c(1:3, 5, 11)]
# dataframe with centered data ready for fitting mixed model
data_all3_c <- cbind(dist[, grouping], myCenter(dist[, tocenter]))
head(data_all3_c)
rm(tocenter, grouping)


# NB: the model might take long time to converge, might be close to 24 hours.

# start counting time
ptm <- proc.time()

# fit model
fm_all3 <- lmer(
  Sim ~ 1 + (Gr + Ob + Di + P + M) * Block * Condition +
    (1 + Gr + Ob + Di + (P + M) * Block | Subj_ID) + (1 + Condition | Item),
  data = data_all3_c, verbose = 2, 
  control = lmerControl(optCtrl = list(maxfun = 200000)))

# how much time did it take?
t.fm_all3 <- proc.time() - ptm
print(t.fm_all3)

# model summary
summary(fm_all3)
