###############################################################################
## R script for Novack-Gottshall, Purcell, Sultan, Ranjha, Deline, & Sumrall ##
## manuscript on trends in echinoderm life habits and body size during the   ##
## Cambrian and Ordovician                                                   ##
###############################################################################

## WORKFLOW ####################################################################

# 0. During manuscript revision, the editor and reviewers requested we (1) 
#    include an alternative EAT time-scaled phylogeny, (2) use updated 
#    stratigraphic ranges, and (3) modify some life habits. The data used herein 
#    is therefore updated from that used in Novack-Gottshall, et al., 2022. 
#    Refer to following R scripts for analyses to rebuild time-scaled trees and 
#    reconstruct ancestral states used prior to using the script below.

#    1-MakeTimeTrees.R: Make time-scaled trees using cal3 method.

#    2-InferAncestralStates.R: Infer ancestral states for each tree, using
#    Claddis package.

#    3-DisparityDistances&PCOA.R: Convert taxon-character matrix to distance
#    matrix using Wills GED with alpha = 0.5 to downgrade weight of dependent
#    character.

#    4-GetStratRanges&AssignClasses.R: Get time-scaled stratigraphic ranges 
#    (FAD - LAD) for tips, and then assign taxonomic classes to nodes. Then 
#    perform principal coordinates analysis (PCOA) ordination.

# 1. Using data from Novack-Gottshall, et al. (2022, Nature Ecology & Evolution)
#    paper, of 366 Cambrian-Ordovician echinoderm genera. Explore sensitivity to
#    using only the tip genera (and PBDB discrete strat ranges) or the 
#    time-scaled phylogeny (366 tips + 365 ancestral nodes with inferred 
#    ancestral states [excluding discrete body sizes, but with binned sizes], 
#    plus using the continuous-time 'cal3' time-scaled ranges).

# 2. Document trends in number of life habits (raw and sample-standardized),
#    individual life-habit characters and body sizes. For body size, also run
#    paleoTS analyses.

# 3. Tally lineage/class and life-habit diversity curves (incl. binned
#    series vs. "continuous" 1-Myr and rarefied)

# 4. Describe trends in body size and tiering. Run paleoTS analyses to determine 
#    whether the trends are consistent with some model of evolution.

# 5. Describe trends in mobility, habitat, diet, foraging, and other ecological
#    characters.


## 1 - PREPARATIONS ############################################################
rm(list = ls())
op <- par()

# Set working directory (point to the folder containing the input files on your
# own machine):
# setwd("[filepath to folder containing data files on your personal machine]")
setwd("~/Manuscripts/CamOrdEchino_EcoTrends/Data & analyses")

# But note that unmodified output used from Novack-Gottshall, et al. (2022
# NatEcoEvol) analyses will be imported directly from the source file, in
# "~/Manuscripts/Published/2022_CamOrdEchinos_NatEcoEvol/Data & analyses".

# Each of the R objects from the 2022 paper is also publicly accessible at
# https://dataverse.harvard.edu/dataverse/CamOrdEchinoderms and direct links are
# provided below, as relevant.

# Load packages (code written using R 4.2.1 and RStudio 2022.07.1+554)
library(ape)        # v. 5.7-1
library(beepr)      # v. 1.3
library(geoscale)   # v. 2.0.1
library(KScorrect)  # v. 1.4.0
library(mclust)     # v. 6.0.0
library(paleoTS)    # v. 0.5.3
library(plotrix)    # v. 3.8-2
library(viridisLite)# v. 0.4.2

# Modification of geoscale::geoscalePlot to allow ICS 2020 timescale 
source("~/Manuscripts/Published/2022_CamOrdEchinos_NatEcoEvol/geoscalePlot2.R")

# Corrected resampling function (used for rarefaction / sample standardization)
sample2 <- function(x, ...) x[sample.int(length(x), ...)]

# Plotting colors (using 8 levels for consistency across figures; use levels 2
# and 7 for bicolor, levels 2, 5, and 7 for tricolor, and 2, 5, 7, and 8 [or 2,
# 4, 5, 7) for quadricolor])
cols <- viridisLite::turbo(8) # Color palette chosen for manuscript
trans.cols <- viridisLite::turbo(8, alpha = 0.5)



## 2 - IMPORT (AND PROCESS) DATA FILES #########################################

## Data set no. 1: Taxon life-habit codings with ancestral state reconstructions ####

# Built using 2-InferAncestralStates.R
load("mode.anc")

# Note these include 1 taxon x life-habit matrix for each of 100 cal3
# time-scaled trees. Tips are the first 366 rows and nodes are the subsequent
# 365 rows.
mode.anc[[57]]$matrix_1$matrix[c(360:370), 1:10]

# These objects also embed the time-scaled tree, which is used to obtain
# stratigraphic ranges.
plot(mode.anc[[57]]$topper$tree)

# Summarize data
ape::Ntip(mode.anc[[57]]$topper$tree)  # 366 tip taxa
ape::Nnode(mode.anc[[57]]$topper$tree) # 365 ancestral nodes

# Obtain root ages (used for Table 2)
sq <- 1:length(mode.anc)
roots <- sapply(sq, function(sq) mode.anc[[sq]]$topper$tree$root.time)
hist(roots, 20) # Definitely asymmetric distribution
summary(roots)
# Median = 527.1 Ma, IQR = 532.9 - 523.9 Ma, range = 555.9 - 520.7 Ma

# Explore ancestral reconstructions (noting that taxon 367 is the root node)
root.LHs <- t(sapply(sq, function(sq) mode.anc[[sq]]$matrix_1$matrix[367, ]))
root.LH <- apply(root.LHs, 1, paste, collapse="")
sort(table(root.LH))

# The most common life habit in the UEH topology is the root LH in tree no. 1
# (actually all 50 UEH trees) (in 50 trees)
LHs <- apply(mode.anc[[1]]$matrix_1$matrix, 1, paste, collapse="")
mode.anc[[1]]$matrix_1$matrix[which(LHs == LHs[367]), ]

# Stem echinoderm in 50 of 100 trees (UEH topology) is reconstructed as
# relatively small (0.1 - 1 cubic cm), epifaunal (1-10 mm tier), free-living
# (unattached), intermittently mobile, microbivorous filter feeder with low
# filtration density feeding elements, atop a soft substrate. (Same life habit
# as found in stem-echinoderm Ctenoimbricata, cinctans Davidocinctus,
# Elliptocinctus, Graciacystis, Gyrocystis, Lignanicystis, Ludwigicinctus,
# Nelegerocystis, Protocinctus, Rozanovicystis, Sucocystis, Trochocystites, and
# Trochocystoides, mitrates Aspidocarpus, Balanocystites, and Lagynocystis,
# stylophoran Ponticulocarpus, and solute Syringocrinus.)

# The most common life habit in the EAT topology is the root LH in tree no. 51
# (actually all 50 EAT trees) (in 50 trees)
LHs <- apply(mode.anc[[51]]$matrix_1$matrix, 1, paste, collapse="")
mode.anc[[51]]$matrix_1$matrix[which(LHs == LHs[367]), ]

# Stem echinoderm in 50 of 100 trees (EAT topology) is reconstructed as slightly
# larger (1 = 10 cubic cm), raised epifaunal (10-100 mm tier), attached,
# sedentary, microbivorous filter feeder with low filtration density feeding
# elements, atop a hard substrate. (Same life habit as found in helicoplacoids
# Helicoplacus, Polyplacus, and Waucobella, sten echinoderm Helicocystis of
# uncertain affinity, stem edrioasteroid Anedriophus, eocrinoids Balangicystis,
# Gogia, Guizhoueocrinus, Mandalacystis, Trachelocrinus, Turbanicystis, and
# Wudingeocrinus, rhombiferans Echinosphaerites, diploporitans Asteroblastus,
# Diplosphaeronis, Eucystis, Parasphaeronites, Sphaeronites, Tetreucystis, and
# Tholocystis, paracrinoid Oklahomacystis, and crinoids Hybocrinus and
# Hybocystis.




## Data set no. 2. Distance matrices (for counting unique life habits) #########

# For counting number of unique life habits, using Will's GED with alpha = 0.5
# distance matrix. Doing so better handles unknown character states in tips and
# polymorphisms and uncertain states in the ancestral state reconstructions. Two
# taxa are considered in the same life habit unless there are definite
# differences.

# Built using 3-DisparityDistances.R
load("mode.distances.GED.5")

# Note these include 1 distance matrix for each of 100 cal3 time-scaled trees.
mode.distances.GED.5[[57]]$distance_matrix[1:4, 1:4]

# How many unique life habits (for tips only)?
sq <- 1:length(mode.distances.GED.5)
table(sapply(sq, function(sq)
  nrow(unique(mode.distances.GED.5[[sq]]$distance_matrix[1:366, 1:366]))))
# All identical, as expected, because tips are invariant across trees: 164

# How many unique life habits (including ancestral nodes)?
sq <- 1:length(mode.distances.GED.5)
LHs <- sapply(sq, function(sq)
  nrow(unique(mode.distances.GED.5[[sq]]$distance_matrix)))
table(LHs)
summary(LHs) # mean/median = 193, range = 186 - 201
sd(LHs)      # SD = 2.9




## Data set no. 3. Stratigraphic ranges (incl. PBDB) ###########################
# Includes ranges for each tip (in both files) and nodes (in first)

# Built using 4-GetStratRanges&AssignClasses.R
load("ranges")

# These include time-scaled FADs and LADs for each tip and node (although the
# ranges vary across time-scaled trees)
ranges[[57]][360:370, ]

# What is the median oldest FAD for any taxon, across trees?
sq <- 1:length(ranges)
(oldest.median.FAD <- 
    median(unlist(lapply(sq, function(sq) max(ranges[[sq]][,1])))))
# median = 527.1 Ma (Stage 2 of Terreneuvian epoch)

# PBDB stratigraphic ranges (downloaded July 18, 2023)
PBDB.ranges <- read.csv("GenusStratRanges.csv", header = TRUE)
# Process so same format as 'ranges'
rownames(PBDB.ranges) <- PBDB.ranges$Genus
PBDB.ranges <- PBDB.ranges[, -1]
head(PBDB.ranges)

# Observe how raw PBDB ranges were altered by the cal3 algorithm
load("median.ranges")  # Built in 4-GetStratRanges&AssignClass

# FADs: (asymmetrical because uses phylogeny to estimate from sister lineages)
summary(PBDB.ranges[, "max_ma"] - ranges[[57]][1:366, "FAD"])
hist(PBDB.ranges[, "max_ma"] - ranges[[57]][1:366, "FAD"], main = "FAD extensions")
# (negative means range pushed earlier, positive means pushed more recently)
# median = -5.3 Ma, mean = -7.7 Ma, IQR = -10.4 - -1.2 Ma, range = -54.8 - +6.5 Ma

# LADs: (symmetrical because samples LADs randomly)
summary(PBDB.ranges[, "min_ma"] - ranges[[57]][1:366, "LAD"])
hist(PBDB.ranges[, "min_ma"] - ranges[[57]][1:366, "LAD"], main = "LAD extensions")
# median = -1.0 Ma, mean = -1.0 Ma, IQR = -2.2 - +0.1 Ma, range = -8.8 - +11.0 Ma



## Data set no. 4. Taxonomic class assignments for each tip and node ###########

# Built using 4-GetStratRanges&AssignClasses.R
load("taxon.list")

# Note these include subphylum and class assignments for each of 50 cal3
# time-scaled trees.
taxon.list[[57]][360:370, ]

# How many trees in data sets? (Used below when summarizing statistics across
# trees)
(ntrees <- length(ranges))



## Data set no. 5. Original life-habit data files ##############################
# Note these include additional information (for tips only), and using ranges
# directly from PBDB

# The original data files include additional information (including operational
# taxonomic names and other metadata), which is used for documenting trends in
# tiering and body size. These trends are only conducted for the tip taxa. These
# files can be obtained at
# https://dataverse.harvard.edu/dataverse/CamOrdEchinoderms.
x <- read.csv(file="EchinoLHData_Mode.csv", header=TRUE, stringsAsFactors=FALSE)
head(x, 5)

# Set proper classes and order (if factors), etc. for variables
scales <- c("Species", "Subgenus", "Genus", "Subfamily", "Family", "Superfamily", 
            "Suborder", "Order", "Subclass", "Class", "Subphylum", "Phylum", "", NA)
scales <- factor(scales, levels = scales, ordered = TRUE)
x$EcologyScale <- factor(x$EcologyScale, levels = scales, ordered = TRUE)
x$BodySizeScale <- factor(x$BodySizeScale, levels = scales, ordered = TRUE)
x$AbsStratDistance <- as.numeric(x$AbsStratDistance)
x$BodyVolume <- as.numeric(x$BodyVolume)
# Ensure all others are integers (converting missings or ?s to NAs)
trait.col.names <- c(21:60)
x[, trait.col.names] <- apply(x[, trait.col.names], 2, as.numeric)
str(x)

# Summarize data
nrow(x) # Number of tip taxa

# Taxonomic coverage and richness for tip taxa. Note some (e.g.,
# "Dendrocrinida", UNCERTAIN-FLAT, and stem Flexibilia) are operational names
# because known to be paraphyletic.
sort(table(x$Order)) # 42 orders (excl. UNCERTAIN, paraphyletic, and informal names)
sort(table(x$Class)) # 21 classes (excl. UNCERTAIN)
sort(table(x$Phylum))
length(unique(x$Phylum))
length(unique(x$Class))
length(unique(x$Order))
length(unique(x$Suborder))
length(unique(x$Superfamily))
length(unique(x$Family))
length(unique(x$Genus))

# Resolution of life-habit and size codings
table(x$EcologyScale); table(x$BodySizeScale)
round(100 * table(x$EcologyScale) / nrow(x), 1)
round(cumsum(100 * table(x$EcologyScale) / nrow(x)), 1)  # 63% genus or better; 87% family; 91% suborder
round(100 * table(x$BodySizeScale) / nrow(x), 1)
round(cumsum(100 * table(x$BodySizeScale) / nrow(x)), 1) # 72% genus or better; 91% family



## 3 - PREP TIME SCALE & BINS ##################################################

# Using manually downloaded alternative (better for consistency and if running
# code offline. Downloaded July 31, 2023. Tab-delimited version available at
# https://dataverse.harvard.edu/file.xhtml?fileId=5373905&version=1.5

# strat_names <-read.csv("https://www.paleobiodb.org/data1.2/intervals/list.csv?all_records&vocab=pbdb")
strat_names <- read.csv("strat_names.csv")
# Eons are level 1, eras = level 2, periods = 3, epochs = 4, ages = 5
ages <- strat_names[which(strat_names$scale_level == 5), ]
# Limit to Cambrian and Ordovician
ages <- ages[which(ages$max_ma > 444), ]
# Add Ediacaran for any pre-Cambrian nodes
ages <-
  rbind(ages, strat_names[which(strat_names$interval_name == "Ediacaran"),])
# Replace outdated Series 3 with Miaolingian (if using epochs)
ages$interval_name <-
  replace(ages$interval_name, which(ages$interval_name == "Series 3"), 
          "Miaolingian")
ages[, c(5, 9:10)]

# Manually set boundaries for more intuitive plotting later
series.boundaries <- c(521, 509, 497, 470, 458.4)
pd.boundaries <- c(541, 485.4, 443.8)

# Import ICS 2020 timescale to use (only) in plotting
ICS2020 <- read.csv("timescales2020.csv", stringsAsFactors =  TRUE)
head(ICS2020)
# Note that we are only using the 2020 ICS timescale for plotting and NOT
# analyses, because the strat ranges in PBDB (and 'geoscale') are still using
# the 2012 ICS ages. The primary change is the naming of Series 3 as
# Miaolingian.


## Assign base, top, and midpoint ages for bins used in trend curves.

# Use 'discrete' for time scale intervals or 'continuous' for user-specified
# (equal-spaced) intervals
discrete <- FALSE   #  'discrete' or not(= 'continuous')?
# discrete <- TRUE  #  'discrete' or not(= 'continuous')?
nbins <- 100         #  How many equally spaced bins? (Only used if continuous)
if(discrete & nbins > 0L)
  warning("Number of bins 'nbins' is ignored if using 'discrete' binning\n")
if(discrete) {
  top <- ages$min_ma
  base <- ages$max_ma
} else {
  # Limiting to Cambrian - Ordovician
  boundaries <- seq(from = min(ages$min_ma), to = max(ages$min_ma), 
                    length.out = nbins + 1)
  top <- boundaries[1:nbins]
  base <- boundaries[2:(nbins + 1)]
}
mids <- apply(rbind(top, base), 2, mean)

summary(base - top)
sd(base - top)
# 0.976 million-year-long bins for continuous bins (if include Ediacaran)
# ~ 10.6 +- 20.96 My-long bins for binned ages/stages (if include Ediacaran)
# ~  5.7 +-  2.68 My-long bins for binned ages/stages (if exclude Ediacaran)

# Save 'mids' for future uses
# save(mids, file = "mids")

# Save discrete PBDB version for future uses
# PBDB.mids <- mids; save(PBDB.mids, file = "PBDB.mids")

# Assign interval names (based on time interval used)
if(round(min(base - top), 6) != round(max(base - top), 6)) {
  interval_names <- ages$interval_name
} else {
  interval_names <- c(rev(seq.int(mids)))
}
print(interval_names)
  






## 4 - DIVERSITY TRENDS (INCL. RAREFACTION) FOR GENUS, CLASS, & LIFE HABIT #####

# Using 'Total Diversity' of Foote (2000) to build diversity curve

# For counting number of unique life habits, using Will's GED with alpha = 0.5
# distance matrix. Doing so better handles unknown character states in tips and
# polymorphisms and uncertain states in the ancestral state reconstructions. Two
# taxa are considered in the same life habit unless there are definite
# differences.

# For class richness, using the second column of 'taxon.list' where class
# assignments for nodes are assigned when both tips are members of the same
# class (and assigned to UNCERTAIN not not).

# For non-sample-standardized trends, taking average trend across 100 time-scaled
# trees (and reporting standard deviation as measure of sampling variability).
# For rarefied trends, taking average across 100 time-scaled trees X 20
# replicates per tree = 2000 resampling replicates, and reporting standard error
# of resampling distribution as estimate of standard error.

## Use following code to identify relevant sampling quotas
incl.nodes <- TRUE   # FALSE if tips only, TRUE if tips + nodes (phylogenetic lineages)
inc.rows <- if(incl.nodes) 1:731 else 1:366
divs <- data.frame(interval = interval_names, base = base, top = top,
                   midpt = mids, genus.mean = NA, genus.min = NA)
tree.divs <- vector(mode = "list", length = length(ranges))
for(i in 1:length(ranges)) tree.divs[[i]] <- divs[, 5]
for (tree in 1:length(ranges)) {
  for(t in 1:nrow(divs)) {
    wh.bin <- as.vector(which(ranges[[tree]][inc.rows, "FAD"] > divs$top[t] &
                                ranges[[tree]][inc.rows, "LAD"] < divs$base[t]))
    tree.divs[[tree]][t] <- length(wh.bin)
  }
}
divs$genus.mean <- apply(simplify2array(tree.divs), 1, mean, na.rm = TRUE)
divs$genus.min <- apply(simplify2array(tree.divs), 1, min, na.rm = TRUE)
print(divs)

# Conclusions:
# Use 24 for discrete (tips only)
# Use 22 for continuous bins (tips only)
# Use 18 or 37 for continuous bins (tips + nodes)
# 20 seems a reasonable round number to use across diversity treatments.

## Rarefaction parameters
ntrees <- length(ranges) # How many trees to sample?
std.g <- 20              # Number of genera per interval
numrep <- 2000           # How many replicates per time interval? 
#                        #   (Divided equally across time-scaled trees)
per.tree.reps <- numrep / ntrees  # Self-calculates per-tree replicates

if(!abs(per.tree.reps - round(per.tree.reps)) < .Machine$double.eps ^ 0.5)
  stop("The number of per-tree replicates needs to be an integer. Choose a 
       different combination of 'numrep' and 'ntree' that will divide more equally.\n")




## Trend no. 1: Discrete trends (using PBDB tip ranges only, and no error bars) ####

# Make sure to use the discrete/binned strat bins above
divs <- data.frame(interval = interval_names, base = base, top = top, 
                   midpt = mids, genus = NA, class = NA, LH = NA)
incl.nodes <- FALSE   # FALSE if tips only, TRUE if tips + nodes (phylogenetic lineages)
inc.rows <- if(incl.nodes) 1:731 else 1:366
for(t in 1:nrow(divs)) {
  wh.bin <- which(PBDB.ranges$max_ma[inc.rows] > divs$top[t] &
                    PBDB.ranges$min_ma[inc.rows] < divs$base[t])
  divs$genus[t] <- length(wh.bin)
  # Using first 'taxon.list' matrix because tips are identical across trees
  classes <- unique(taxon.list[[1]][wh.bin, "class"])
  # Remove UNCERTAIN
  classes <- classes[which(classes != "UNCERTAIN")]
  divs$class[t] <- length(classes)
  # Using first 'mode.distances.GED.5' matrix because tip distances are
  # identical across trees (and using 'mode' treatment because most complete)
  LHs <- mode.distances.GED.5[[1]]$distance_matrix[wh.bin, wh.bin]
  divs$LH[t] <- nrow(unique(LHs))
}
beepr::beep()
divs
summary(divs$genus)
PBDB.div.discrete <- divs

# Save trends
# write.csv(divs, file = "div_PBDB_discrete.csv", row.names = FALSE)
# divs <- PBDB.div.discrete <- read.csv(file = "div_PBDB_discrete.csv")

# Plot binned (discrete) PBDB diversity curve, tips only
geoscalePlot2(divs$midpt, divs$genus, units = c("Epoch", "Period"), 
              tick.scale = "Period", boxes = "Age", cex.age = 0.65, 
              cex.ts = 0.7, cex.pt = 1, age.lim = c(540, 445), 
              data.lim = c(0, 250), ts.col = TRUE, label = "richness", 
              timescale = ICS2020, type = "n", abbrev = "Period")
mtext(text = "binned PBDB richness", side = 3, cex = 1.25)
lines(divs$midpt, divs$genus, lty = 1, lwd = 3, col = cols[2])
lines(divs$midpt, divs$LH, lty = 2, lwd = 3, col = cols[5])
lines(divs$midpt, divs$class, lty = 3, lwd = 3, col = cols[7])
legend("topleft", legend = c("genera", "life habits", "classes"), box.col = NA, 
       lty = c(1, 2, 3), col = cols[c(2, 5, 7)], lwd = 2, pch = NA, inset = .05)




## Trend no. 2: Discrete trends (using tips only), averaging across trees ######

# Make sure to use the discrete/binned strat bins above
divs <- data.frame(interval = interval_names, base = base, top = top, 
                   midpt = mids, gen.mean.raw = NA, gen.SE.raw = NA, 
                   cl.mean.raw = NA, cl.SE.raw = NA, LH.mean.raw = NA, 
                   LH.SE.raw = NA, gen.mean = NA, gen.SE = NA, cl.mean = NA, 
                   cl.SE = NA, LH.mean = NA, LH.SE = NA)
incl.nodes <- FALSE   # FALSE if tips only, TRUE if tips + nodes (phylogenetic lineages)
inc.rows <- if(incl.nodes) 1:731 else 1:366

# Create list to store 'numrep' and raw 'ntrees' replicates
sub.divs <- vector(mode = "list", length = numrep)
tree.divs <- vector(mode = "list", length = ntrees)
tmp.holder <- matrix(NA, nrow = length(mids), ncol = 3)
colnames(tmp.holder) <- c("genus", "class", "LH")
for (i in 1:numrep) sub.divs[[i]] <- tmp.holder
for (i in 1:ntrees) tree.divs[[i]] <- tmp.holder
cat("Mean trend uses average across", numrep, "sample-standardized replicates of", std.g, 
    "lineages \n (genus tips and/or ancestral nodes) per time bin, repeated across", 
    per.tree.reps, "replicates \n per each of", ntrees, "time-scaled trees.\n")

set.seed(3124) # So rarefaction can be replicated
increm <- 1    # Counter for incrementing replicates to 'sub.divs' (first 'pt.rep's from tree #1, etc.)
for (tree in 1:ntrees) {
  
  # Record raw (non-sample-standardized) statistics, across trees
  for(t in 1:nrow(divs)) {
    # Identify lineages in bin
    wh.bin <- as.vector(which(ranges[[tree]][inc.rows, "FAD"] > divs$top[t] &
                                ranges[[tree]][inc.rows, "LAD"] < divs$base[t]))
    tree.divs[[tree]][t, "genus"] <- length(wh.bin)
    classes <- unique(taxon.list[[tree]][wh.bin, "class"])
    # Remove UNCERTAIN
    classes <- classes[which(classes != "UNCERTAIN")]
    tree.divs[[tree]][t, "class"] <- length(classes)
    # Using 'mode' treatment because most complete
    LHs <- mode.distances.GED.5[[tree]]$distance_matrix[wh.bin, wh.bin]
    if (length(LHs) == 1L) {
      tree.divs[[tree]][t, "LH"] <- 1 } else {
      tree.divs[[tree]][t, "LH"] <- nrow(unique(LHs)) }
  }
    
    # Record sample-standardized statistics, across tree-replicates
  for (pt.rep in  1:per.tree.reps) {
    for(t in 1:nrow(divs)) {
      # Identify lineages in bin
      wh.bin <- as.vector(which(ranges[[tree]][inc.rows, "FAD"] > divs$top[t] &
                                  ranges[[tree]][inc.rows, "LAD"] < divs$base[t]))
      # Got to next time interval if below sampling quota
      if (std.g > length(wh.bin))
        next
      # Calculate statistics on subsamples
      sampled <- sample2(wh.bin, std.g, replace = FALSE)
      sub.divs[[increm]][t, "genus"] <- length(sampled)
      classes <- unique(taxon.list[[tree]][sampled, "class"])
      # Remove UNCERTAIN
      classes <- classes[which(classes != "UNCERTAIN")]
      sub.divs[[increm]][t, "class"] <- length(classes)
      # Using 'mode' treatment because most complete
      LHs <-
        mode.distances.GED.5[[tree]]$distance_matrix[sampled, sampled]
      if (length(LHs) == 1L) {
        sub.divs[[increm]][t, "LH"] <- 1
      } else {
        sub.divs[[increm]][t, "LH"] <- nrow(unique(LHs))
      }
    }
    increm <- increm + 1
  }
}
beepr::beep()

# Calculate mean and SD across trees/replicates
divs.mean1 <- apply(simplify2array(tree.divs), 1:2, mean)
divs.sd1 <- apply(simplify2array(tree.divs), 1:2, sd)
divs.mean2 <- apply(simplify2array(sub.divs), 1:2, mean)
divs.sd2 <- apply(simplify2array(sub.divs), 1:2, sd)
divs[, c(5, 7, 9)] <- divs.mean1
divs[, c(6, 8, 10)] <- divs.sd1
divs[, c(11, 13, 15)] <- divs.mean2
divs[, c(12, 14, 16)] <- divs.sd2

print(divs)
tip.div.discrete <- divs

# Save trends
# write.csv(divs, file = "div_tip_discrete.csv", row.names = FALSE)
# divs <- read.csv(file = "div_tip_discrete.csv")

# Plot binned (discrete) raw (non-sample-standardized) diversity curve, tips only
geoscalePlot2(divs$midpt, divs$gen.mean.raw, units = c("Epoch", "Period"), 
              tick.scale = "Period", boxes = "Age", cex.age = 0.65, 
              cex.ts = 0.7, cex.pt = 1, age.lim = c(540, 445), 
              data.lim = c(0, 250), ts.col = TRUE, label = "richness", 
              timescale = ICS2020, type = "n", abbrev = "Period")
mtext(text = "raw binned richness (tips only)", side = 3, cex = 1.25)
gen.column <- cbind(c(divs$midpt, rev(divs$midpt)), 
                    c(divs$gen.mean.raw - divs$gen.SE.raw, rev(divs$gen.mean.raw + divs$gen.SE.raw)))
gen.column <- na.omit(gen.column)
polygon(gen.column[ ,1], gen.column[ ,2], col = trans.cols[2], lwd = 2, border = NA)
LH.column <- cbind(c(divs$midpt, rev(divs$midpt)), 
                   c(divs$LH.mean.raw - divs$LH.SE.raw, rev(divs$LH.mean.raw + divs$LH.SE.raw)))
LH.column <- na.omit(LH.column)
polygon(LH.column[ ,1], LH.column[ ,2], col = trans.cols[5], lwd = 2, border = NA)
cl.column <- cbind(c(divs$midpt, rev(divs$midpt)), 
                   c(divs$cl.mean.raw - divs$cl.SE.raw, rev(divs$cl.mean.raw + divs$cl.SE.raw)))
cl.column <- na.omit(cl.column)
polygon(cl.column[ ,1], cl.column[ ,2], col = trans.cols[7], lwd = 2, border = NA)
lines(divs$midpt, divs$gen.mean.raw, lty = 1, lwd = 3, col = cols[2])
lines(divs$midpt, divs$LH.mean.raw, lty = 2, lwd = 3, col = cols[5])
lines(divs$midpt, divs$cl.mean.raw, lty = 3, lwd = 3, col = cols[7])
legend("topleft", legend = c("genera", "life habits", "classes"), box.col = NA, 
       lty = c(1, 2, 3), col = cols[c(2, 5, 7)], lwd = 2, pch = NA, inset = .05)

# Plot binned (discrete) sample-standardized diversity curve, tips only
geoscalePlot2(divs$midpt, divs$LH.mean, units = c("Epoch", "Period"), 
              tick.scale = "Period", boxes = "Age", cex.age = 0.65, 
              cex.ts = 0.7, cex.pt = 1, age.lim = c(540, 445), 
              data.lim = c(0, std.g), ts.col = TRUE, 
              label = paste0("mean richness (G = ", std.g, ")"), 
              timescale = ICS2020, type = "n", abbrev = "Period")
mtext(text = "sample-standardized richness (binned, tips only)", side = 3, cex = 1.25)
gen.column <- cbind(c(divs$midpt, rev(divs$midpt)), 
                    c(divs$gen.mean - divs$gen.SE, rev(divs$gen.mean + divs$gen.SE)))
gen.column <- na.omit(gen.column)
polygon(gen.column[ ,1], gen.column[ ,2], col = trans.cols[2], lwd = 2, border = NA)
LH.column <- cbind(c(divs$midpt, rev(divs$midpt)), 
                   c(divs$LH.mean - divs$LH.SE, rev(divs$LH.mean + divs$LH.SE)))
LH.column <- na.omit(LH.column)
polygon(LH.column[ ,1], LH.column[ ,2], col = trans.cols[5], lwd = 2, border = NA)
cl.column <- cbind(c(divs$midpt, rev(divs$midpt)), 
                   c(divs$cl.mean - divs$cl.SE, rev(divs$cl.mean + divs$cl.SE)))
cl.column <- na.omit(cl.column)
polygon(cl.column[ ,1], cl.column[ ,2], col = trans.cols[7], lwd = 2, border = NA)
lines(divs$midpt, divs$gen.mean, lty = 1, lwd = 3, col = cols[2])
lines(divs$midpt, divs$LH.mean, lty = 2, lwd = 3, col = cols[5])
lines(divs$midpt, divs$cl.mean, lty = 3, lwd = 3, col = cols[7])
legend("topleft", legend = c("genera", "life habits", "classes"), box.col = NA, 
       lty = c(1, 2, 3), col = cols[c(2, 5, 7)], lwd = 2, pch = NA, inset = .05)




## Trend no. 3: "Continuous" trends (using tips only), averaging across trees ####

# Make sure to use the "continuous" strat bins above
divs <- data.frame(interval = interval_names, base = base, top = top, 
                   midpt = mids, gen.mean.raw = NA, gen.SE.raw = NA, 
                   cl.mean.raw = NA, cl.SE.raw = NA, LH.mean.raw = NA, 
                   LH.SE.raw = NA, gen.mean = NA, gen.SE = NA, cl.mean = NA, 
                   cl.SE = NA, LH.mean = NA, LH.SE = NA)
incl.nodes <- FALSE   # FALSE if tips only, TRUE if tips + nodes (phylogenetic lineages)
inc.rows <- if(incl.nodes) 1:731 else 1:366

# Create list to store 'numrep' and raw 'ntrees' replicates
sub.divs <- vector(mode = "list", length = numrep)
tree.divs <- vector(mode = "list", length = ntrees)
tmp.holder <- matrix(NA, nrow = length(mids), ncol = 3)
colnames(tmp.holder) <- c("genus", "class", "LH")
for (i in 1:numrep) sub.divs[[i]] <- tmp.holder
for (i in 1:ntrees) tree.divs[[i]] <- tmp.holder
cat("Mean trend uses average across", numrep, "sample-standardized replicates of", std.g, 
    "lineages \n (genus tips and/or ancestral nodes) per time bin, repeated across", 
    per.tree.reps, "replicates \n per each of", ntrees, "time-scaled trees.\n")

set.seed(3124) # So rarefaction can be replicated
increm <- 1    # Counter for incrementing replicates to 'sub.divs' (first 'pt.rep's from tree #1, etc.)
for (tree in 1:ntrees) {
  
  # Record raw (non-sample-standardized) statistics, across trees
  for(t in 1:nrow(divs)) {
    # Identify lineages in bin
    wh.bin <- as.vector(which(ranges[[tree]][inc.rows, "FAD"] > divs$top[t] &
                                ranges[[tree]][inc.rows, "LAD"] < divs$base[t]))
    tree.divs[[tree]][t, "genus"] <- length(wh.bin)
    classes <- unique(taxon.list[[tree]][wh.bin, "class"])
    # Remove UNCERTAIN
    classes <- classes[which(classes != "UNCERTAIN")]
    tree.divs[[tree]][t, "class"] <- length(classes)
    # Using 'mode' treatment because most complete
    LHs <- mode.distances.GED.5[[tree]]$distance_matrix[wh.bin, wh.bin]
    if (length(LHs) == 1L) {
      tree.divs[[tree]][t, "LH"] <- 1 } else {
        tree.divs[[tree]][t, "LH"] <- nrow(unique(LHs)) }
  }
  
  # Record sample-standardized statistics, across tree-replicates
  for (pt.rep in  1:per.tree.reps) {
    for(t in 1:nrow(divs)) {
      # Identify lineages in bin
      wh.bin <- as.vector(which(ranges[[tree]][inc.rows, "FAD"] > divs$top[t] &
                                  ranges[[tree]][inc.rows, "LAD"] < divs$base[t]))
      # Got to next time interval if below sampling quota
      if (std.g > length(wh.bin))
        next
      # Calculate statistics on subsamples
      sampled <- sample2(wh.bin, std.g, replace = FALSE)
      sub.divs[[increm]][t, "genus"] <- length(sampled)
      classes <- unique(taxon.list[[tree]][sampled, "class"])
      # Remove UNCERTAIN
      classes <- classes[which(classes != "UNCERTAIN")]
      sub.divs[[increm]][t, "class"] <- length(classes)
      # Using 'mode' treatment because most complete
      LHs <-
        mode.distances.GED.5[[tree]]$distance_matrix[sampled, sampled]
      if (length(LHs) == 1L) {
        sub.divs[[increm]][t, "LH"] <- 1
      } else {
        sub.divs[[increm]][t, "LH"] <- nrow(unique(LHs))
      }
    }
    increm <- increm + 1
  }
}
beepr::beep() # ~ 1 minute

# Calculate mean and SD across trees/replicates
divs.mean1 <- apply(simplify2array(tree.divs), 1:2, mean)
divs.sd1 <- apply(simplify2array(tree.divs), 1:2, sd)
divs.mean2 <- apply(simplify2array(sub.divs), 1:2, mean)
divs.sd2 <- apply(simplify2array(sub.divs), 1:2, sd)
divs[, c(5, 7, 9)] <- divs.mean1
divs[, c(6, 8, 10)] <- divs.sd1
divs[, c(11, 13, 15)] <- divs.mean2
divs[, c(12, 14, 16)] <- divs.sd2

print(divs)
tip.div.continuous <- divs

# Save trends
# write.csv(divs, file = "div_tip_continuous.csv", row.names = FALSE)
# divs <- tip.div.continuous <- read.csv(file = "div_tip_continuous.csv")

# Plot continuous, raw (non-sample-standardized) diversity curve, tips only
geoscalePlot2(divs$midpt, divs$gen.mean.raw, units = c("Epoch", "Period"), 
              tick.scale = "Period", boxes = "Age", cex.age = 0.65, 
              cex.ts = 0.7, cex.pt = 1, age.lim = c(540, 445), 
              data.lim = c(0, 250), ts.col = TRUE, label = "richness", 
              timescale = ICS2020, type = "n", abbrev = "Period")
mtext(text = "raw continuous richness (tips only)", side = 3, cex = 1.25)
gen.column <- cbind(c(divs$midpt, rev(divs$midpt)), 
                    c(divs$gen.mean.raw - divs$gen.SE.raw, rev(divs$gen.mean.raw + divs$gen.SE.raw)))
gen.column <- na.omit(gen.column)
polygon(gen.column[ ,1], gen.column[ ,2], col = trans.cols[2], lwd = 2, border = NA)
LH.column <- cbind(c(divs$midpt, rev(divs$midpt)), 
                   c(divs$LH.mean.raw - divs$LH.SE.raw, rev(divs$LH.mean.raw + divs$LH.SE.raw)))
LH.column <- na.omit(LH.column)
polygon(LH.column[ ,1], LH.column[ ,2], col = trans.cols[5], lwd = 2, border = NA)
cl.column <- cbind(c(divs$midpt, rev(divs$midpt)), 
                   c(divs$cl.mean.raw - divs$cl.SE.raw, rev(divs$cl.mean.raw + divs$cl.SE.raw)))
cl.column <- na.omit(cl.column)
polygon(cl.column[ ,1], cl.column[ ,2], col = trans.cols[7], lwd = 2, border = NA)
lines(divs$midpt, divs$gen.mean.raw, lty = 1, lwd = 3, col = cols[2])
lines(divs$midpt, divs$LH.mean.raw, lty = 2, lwd = 3, col = cols[5])
lines(divs$midpt, divs$cl.mean.raw, lty = 3, lwd = 3, col = cols[7])
legend("topleft", legend = c("genera", "life habits", "classes"), box.col = NA, 
       lty = c(1, 2, 3), col = cols[c(2, 5, 7)], lwd = 2, pch = NA, inset = .05)

# Plot continuous, sample-standardized diversity curve, tips only
geoscalePlot2(divs$midpt, divs$LH.mean, units = c("Epoch", "Period"), 
              tick.scale = "Period", boxes = "Age", cex.age = 0.65, 
              cex.ts = 0.7, cex.pt = 1, age.lim = c(540, 445), 
              data.lim = c(0, std.g), ts.col = TRUE, 
              label = paste0("mean richness (G = ", std.g, ")"), 
              timescale = ICS2020, type = "n", abbrev = "Period")
mtext(text = "sample-standardized richness (continuous, tips only)", side = 3, cex = 1.25)
gen.column <- cbind(c(divs$midpt, rev(divs$midpt)), 
                    c(divs$gen.mean - divs$gen.SE, rev(divs$gen.mean + divs$gen.SE)))
gen.column <- na.omit(gen.column)
polygon(gen.column[ ,1], gen.column[ ,2], col = trans.cols[2], lwd = 2, border = NA)
LH.column <- cbind(c(divs$midpt, rev(divs$midpt)), 
                   c(divs$LH.mean - divs$LH.SE, rev(divs$LH.mean + divs$LH.SE)))
LH.column <- na.omit(LH.column)
polygon(LH.column[ ,1], LH.column[ ,2], col = trans.cols[5], lwd = 2, border = NA)
cl.column <- cbind(c(divs$midpt, rev(divs$midpt)), 
                   c(divs$cl.mean - divs$cl.SE, rev(divs$cl.mean + divs$cl.SE)))
cl.column <- na.omit(cl.column)
polygon(cl.column[ ,1], cl.column[ ,2], col = trans.cols[7], lwd = 2, border = NA)
lines(divs$midpt, divs$gen.mean, lty = 1, lwd = 3, col = cols[2])
lines(divs$midpt, divs$LH.mean, lty = 2, lwd = 3, col = cols[5])
lines(divs$midpt, divs$cl.mean, lty = 3, lwd = 3, col = cols[7])
legend("topleft", legend = c("genera", "life habits", "classes"), box.col = NA, 
       lty = c(1, 2, 3), col = cols[c(2, 5, 7)], lwd = 2, pch = NA, inset = .05)




## Trend no. 4: "Continuous" trends (using tip and nodes), averaging across trees ####

# Make sure to use the "continuous" strat bins above
divs <- data.frame(interval = interval_names, base = base, top = top, 
                   midpt = mids, gen.mean.raw = NA, gen.SE.raw = NA, 
                   cl.mean.raw = NA, cl.SE.raw = NA, LH.mean.raw = NA, 
                   LH.SE.raw = NA, gen.mean = NA, gen.SE = NA, cl.mean = NA, 
                   cl.SE = NA, LH.mean = NA, LH.SE = NA)
incl.nodes <- TRUE   # FALSE if tips only, TRUE if tips + nodes (phylogenetic lineages)
inc.rows <- if(incl.nodes) 1:731 else 1:366

# Create list to store 'numrep' and raw 'ntrees' replicates
sub.divs <- vector(mode = "list", length = numrep)
tree.divs <- vector(mode = "list", length = ntrees)
tmp.holder <- matrix(NA, nrow = length(mids), ncol = 3)
colnames(tmp.holder) <- c("genus", "class", "LH")
for (i in 1:numrep) sub.divs[[i]] <- tmp.holder
for (i in 1:ntrees) tree.divs[[i]] <- tmp.holder
cat("Mean trend uses average across", numrep, "sample-standardized replicates of", std.g, 
    "lineages \n (genus tips and/or ancestral nodes) per time bin, repeated across", 
    per.tree.reps, "replicates \n per each of", ntrees, "time-scaled trees.\n")

set.seed(3124) # So rarefaction can be replicated
increm <- 1    # Counter for incrementing replicates to 'sub.divs' (first 'pt.rep's from tree #1, etc.)
for (tree in 1:ntrees) {
  
  # Record raw (non-sample-standardized) statistics, across trees
  for(t in 1:nrow(divs)) {
    # Identify lineages in bin
    wh.bin <- as.vector(which(ranges[[tree]][inc.rows, "FAD"] > divs$top[t] &
                                ranges[[tree]][inc.rows, "LAD"] < divs$base[t]))
    tree.divs[[tree]][t, "genus"] <- length(wh.bin)
    classes <- unique(taxon.list[[tree]][wh.bin, "class"])
    # Remove UNCERTAIN
    classes <- classes[which(classes != "UNCERTAIN")]
    tree.divs[[tree]][t, "class"] <- length(classes)
    # Using 'mode' treatment because most complete
    LHs <- mode.distances.GED.5[[tree]]$distance_matrix[wh.bin, wh.bin]
    if (length(LHs) == 1L) {
      tree.divs[[tree]][t, "LH"] <- 1 } else {
        tree.divs[[tree]][t, "LH"] <- nrow(unique(LHs)) }
  }
  
  # Record sample-standardized statistics, across tree-replicates
  for (pt.rep in  1:per.tree.reps) {
    for(t in 1:nrow(divs)) {
      # Identify lineages in bin
      wh.bin <- as.vector(which(ranges[[tree]][inc.rows, "FAD"] > divs$top[t] &
                                  ranges[[tree]][inc.rows, "LAD"] < divs$base[t]))
      # Got to next time interval if below sampling quota
      if (std.g > length(wh.bin))
        next
      # Calculate statistics on subsamples
      sampled <- sample2(wh.bin, std.g, replace = FALSE)
      sub.divs[[increm]][t, "genus"] <- length(sampled)
      classes <- unique(taxon.list[[tree]][sampled, "class"])
      # Remove UNCERTAIN
      classes <- classes[which(classes != "UNCERTAIN")]
      sub.divs[[increm]][t, "class"] <- length(classes)
      # Using 'mode' treatment because most complete
      LHs <-
        mode.distances.GED.5[[tree]]$distance_matrix[sampled, sampled]
      if (length(LHs) == 1L) {
        sub.divs[[increm]][t, "LH"] <- 1
      } else {
        sub.divs[[increm]][t, "LH"] <- nrow(unique(LHs))
      }
    }
    increm <- increm + 1
  }
}
beepr::beep() # ~ 1 minute

# Calculate mean and SD across trees/replicates
divs.mean1 <- apply(simplify2array(tree.divs), 1:2, mean)
divs.sd1 <- apply(simplify2array(tree.divs), 1:2, sd)
divs.mean2 <- apply(simplify2array(sub.divs), 1:2, mean)
divs.sd2 <- apply(simplify2array(sub.divs), 1:2, sd)
divs[, c(5, 7, 9)] <- divs.mean1
divs[, c(6, 8, 10)] <- divs.sd1
divs[, c(11, 13, 15)] <- divs.mean2
divs[, c(12, 14, 16)] <- divs.sd2

print(divs)

# Save trends
all.div.continuous <- divs
# write.csv(divs, file = "div_tip&node_continuous.csv", row.names = FALSE)
# divs <- all.div.continuous <- read.csv(file = "div_tip&node_continuous.csv")

# Plot continuous, raw (non-sample-standardized) diversity curve, tips + nodes
geoscalePlot2(divs$midpt, divs$gen.mean.raw, units = c("Epoch", "Period"), 
              tick.scale = "Period", boxes = "Age", cex.age = 0.65, 
              cex.ts = 0.7, cex.pt = 1, age.lim = c(540, 445), 
              data.lim = c(0, 250), ts.col = TRUE, label = "richness", 
              timescale = ICS2020, type = "n", abbrev = "Period")
mtext(text = "raw continuous phylogenetic richness (tips + nodes)", side = 3, cex = 1.25)
gen.column <- cbind(c(divs$midpt, rev(divs$midpt)), 
                    c(divs$gen.mean.raw - divs$gen.SE.raw, rev(divs$gen.mean.raw + divs$gen.SE.raw)))
gen.column <- na.omit(gen.column)
polygon(gen.column[ ,1], gen.column[ ,2], col = trans.cols[2], lwd = 2, border = NA)
LH.column <- cbind(c(divs$midpt, rev(divs$midpt)), 
                   c(divs$LH.mean.raw - divs$LH.SE.raw, rev(divs$LH.mean.raw + divs$LH.SE.raw)))
LH.column <- na.omit(LH.column)
polygon(LH.column[ ,1], LH.column[ ,2], col = trans.cols[5], lwd = 2, border = NA)
cl.column <- cbind(c(divs$midpt, rev(divs$midpt)), 
                   c(divs$cl.mean.raw - divs$cl.SE.raw, rev(divs$cl.mean.raw + divs$cl.SE.raw)))
cl.column <- na.omit(cl.column)
polygon(cl.column[ ,1], cl.column[ ,2], col = trans.cols[7], lwd = 2, border = NA)
lines(divs$midpt, divs$gen.mean.raw, lty = 1, lwd = 3, col = cols[2])
lines(divs$midpt, divs$LH.mean.raw, lty = 2, lwd = 3, col = cols[5])
lines(divs$midpt, divs$cl.mean.raw, lty = 3, lwd = 3, col = cols[7])
legend("topleft", legend = c("genera", "life habits", "classes"), box.col = NA, 
       lty = c(1, 2, 3), col = cols[c(2, 5, 7)], lwd = 2, pch = NA, inset = .05)

# Plot continuous, sample-standardized diversity curve, tips + nodes
geoscalePlot2(divs$midpt, divs$LH.mean, units = c("Epoch", "Period"), 
              tick.scale = "Period", boxes = "Age", cex.age = 0.65, 
              cex.ts = 0.7, cex.pt = 1, age.lim = c(540, 445), 
              data.lim = c(0, std.g), ts.col = TRUE, 
              label = paste0("mean richness (G = ", std.g, ")"), 
              timescale = ICS2020, type = "n", abbrev = "Period")
mtext(text = "phylogenetic richness (continuous, tips + nodes)", side = 3, cex = 1.25)
gen.column <- cbind(c(divs$midpt, rev(divs$midpt)), 
                    c(divs$gen.mean - divs$gen.SE, rev(divs$gen.mean + divs$gen.SE)))
gen.column <- na.omit(gen.column)
polygon(gen.column[ ,1], gen.column[ ,2], col = trans.cols[2], lwd = 2, border = NA)
LH.column <- cbind(c(divs$midpt, rev(divs$midpt)), 
                   c(divs$LH.mean - divs$LH.SE, rev(divs$LH.mean + divs$LH.SE)))
LH.column <- na.omit(LH.column)
polygon(LH.column[ ,1], LH.column[ ,2], col = trans.cols[5], lwd = 2, border = NA)
cl.column <- cbind(c(divs$midpt, rev(divs$midpt)), 
                   c(divs$cl.mean - divs$cl.SE, rev(divs$cl.mean + divs$cl.SE)))
cl.column <- na.omit(cl.column)
polygon(cl.column[ ,1], cl.column[ ,2], col = trans.cols[7], lwd = 2, border = NA)
lines(divs$midpt, divs$gen.mean, lty = 1, lwd = 3, col = cols[2])
lines(divs$midpt, divs$LH.mean, lty = 2, lwd = 3, col = cols[5])
lines(divs$midpt, divs$cl.mean, lty = 3, lwd = 3, col = cols[7])
legend("topleft", legend = c("genera", "life habits", "classes"), box.col = NA, 
       lty = c(1, 2, 3), col = cols[c(2, 5, 7)], lwd = 2, pch = NA, inset = .05)




## Compare diversity treatments ################################################
PBDB.div.discrete <- read.csv(file = "div_PBDB_discrete.csv")
tip.div.discrete <- read.csv(file = "div_tip_discrete.csv")
tip.div.continuous <- read.csv(file = "div_tip_continuous.csv")
all.div.continuous <- read.csv(file = "div_tip&node_continuous.csv")

# Raw genus/lineage richness
geoscalePlot2(tip.div.discrete$midpt, tip.div.discrete$gen.mean.raw, units = c("Epoch", "Period"), 
              tick.scale = "Period", boxes = "Age", cex.age = 0.65, 
              cex.ts = 0.7, cex.pt = 1, age.lim = c(540, 445), 
              data.lim = c(0, 250), ts.col = TRUE, label = "richness", 
              timescale = ICS2020, type = "n", abbrev = "Period")
mtext(text = "genus richness", side = 3, cex = 1.25)
lines(PBDB.div.discrete$midpt, PBDB.div.discrete$genus, lwd = 3, col = cols[1], lty = 1)
lines(tip.div.discrete$midpt, tip.div.discrete$gen.mean.raw, lwd = 3, col = cols[2], lty = 2)
lines(tip.div.continuous$midpt, tip.div.continuous$gen.mean.raw, lwd = 3, col = cols[3], lty = 3)
lines(all.div.continuous$midpt, all.div.continuous$gen.mean.raw, lwd = 3, col = cols[4], lty = 4)
legend("topleft", legend = c("PBDB genus richness (binned)", "genus richness (binned)", 
       "genus richness (continuous)", "lineage richness (continuous)"), 
       box.col = NA, lty = c(1, 2, 3, 4), col = cols[1:4], lwd = 2, pch = NA, 
       inset = .01)

# Raw (non-sample-standardized) life habit richness
geoscalePlot2(tip.div.discrete$midpt, tip.div.discrete$LH.mean.raw, units = c("Epoch", "Period"), 
              tick.scale = "Period", boxes = "Age", cex.age = 0.65, 
              cex.ts = 0.7, cex.pt = 1, age.lim = c(540, 445), 
              data.lim = c(0, 140), ts.col = TRUE, label = "life-habit richness", 
              timescale = ICS2020, type = "n", abbrev = "Period")
mtext(text = "number of unique life habits", side = 3, cex = 1.25)
lines(PBDB.div.discrete$midpt, PBDB.div.discrete$LH, lwd = 3, col = cols[1], lty = 1)
lines(tip.div.discrete$midpt, tip.div.discrete$LH.mean.raw, lwd = 3, col = cols[2], lty = 2)
lines(tip.div.continuous$midpt, tip.div.continuous$LH.mean.raw, lwd = 3, col = cols[3], lty = 3)
lines(all.div.continuous$midpt, all.div.continuous$LH.mean.raw, lwd = 3, col = cols[4], lty = 4)
legend("topleft", legend = c("PBDB (binned)", "tips only (binned)", 
      "tips only (continuous)", "tips + nodes (continuous)"), 
       box.col = NA, lty = c(1, 2, 3, 4), col = cols[1:4], lwd = 2, pch = NA, 
       inset = .01)

# Sample-standardized life habit richness
geoscalePlot2(tip.div.discrete$midpt, tip.div.discrete$LH.mean, units = c("Epoch", "Period"), 
              tick.scale = "Period", boxes = "Age", cex.age = 0.65, 
              cex.ts = 0.7, cex.pt = 1, age.lim = c(540, 445), 
              data.lim = c(0, 25), ts.col = TRUE, label = "life-habit richness", 
              timescale = ICS2020, type = "n", abbrev = "Period")
mtext(text = "number of unique life habits", side = 3, cex = 1.25)
LH.column <- cbind(c(tip.div.discrete$midpt, rev(tip.div.discrete$midpt)), 
                   c(tip.div.discrete$LH.mean - tip.div.discrete$LH.SE, 
                     rev(tip.div.discrete$LH.mean + tip.div.discrete$LH.SE)))
LH.column <- na.omit(LH.column)
polygon(LH.column[ ,1], LH.column[ ,2], col = trans.cols[2], lwd = 2, border = NA)
LH.column <- cbind(c(tip.div.continuous$midpt, rev(tip.div.continuous$midpt)), 
                   c(tip.div.continuous$LH.mean - tip.div.continuous$LH.SE, 
                     rev(tip.div.continuous$LH.mean + tip.div.continuous$LH.SE)))
LH.column <- na.omit(LH.column)
polygon(LH.column[ ,1], LH.column[ ,2], col = trans.cols[3], lwd = 2, border = NA)
LH.column <- cbind(c(all.div.continuous$midpt, rev(all.div.continuous$midpt)), 
                   c(all.div.continuous$LH.mean - all.div.continuous$LH.SE, 
                     rev(all.div.continuous$LH.mean + all.div.continuous$LH.SE)))
LH.column <- na.omit(LH.column)
polygon(LH.column[ ,1], LH.column[ ,2], col = trans.cols[4], lwd = 2, border = NA)
lines(tip.div.discrete$midpt, tip.div.discrete$LH.mean, lwd = 3, col = cols[2], lty = 2)
lines(tip.div.continuous$midpt, tip.div.continuous$LH.mean, lwd = 3, col = cols[3], lty = 3)
lines(all.div.continuous$midpt, all.div.continuous$LH.mean, lwd = 3, col = cols[4], lty = 4)
legend("bottomright", legend = c("tips only (binned) (G = 25)", "tips only (continuous) (G = 24)", 
       "tips + nodes (continuous)  (G = 20)"), box.col = NA, lty = c(2, 3, 4), 
       col = cols[2:4], lwd = 2, pch = NA, inset = .01)

# Raw (non-sample-standardized) class richness
geoscalePlot2(tip.div.discrete$midpt, tip.div.discrete$cl.mean.raw, units = c("Epoch", "Period"), 
              tick.scale = "Period", boxes = "Age", cex.age = 0.65, 
              cex.ts = 0.7, cex.pt = 1, age.lim = c(540, 445), 
              data.lim = c(0, 20), ts.col = TRUE, label = "class richness", 
              timescale = ICS2020, type = "n", abbrev = "Period")
mtext(text = "number of classes", side = 3, cex = 1.25)
lines(PBDB.div.discrete$midpt, PBDB.div.discrete$cl, lwd = 3, col = cols[1], lty = 1)
lines(tip.div.discrete$midpt, tip.div.discrete$cl.mean.raw, lwd = 3, col = cols[2], lty = 2)
lines(tip.div.continuous$midpt, tip.div.continuous$cl.mean.raw, lwd = 3, col = cols[3], lty = 3)
lines(all.div.continuous$midpt, all.div.continuous$cl.mean.raw, lwd = 3, col = cols[4], lty = 4)
legend("topleft", legend = c("PBDB (binned)", "tips only (binned)", 
       "tips only (continuous)", "tips + nodes (continuous)"), 
       box.col = NA, lty = c(1, 2, 3, 4), col = cols[1:4], lwd = 2, pch = NA, 
       inset = .01)

# Sample-standardized class richness
geoscalePlot2(tip.div.discrete$midpt, tip.div.discrete$cl.mean, units = c("Epoch", "Period"), 
              tick.scale = "Period", boxes = "Age", cex.age = 0.65, 
              cex.ts = 0.7, cex.pt = 1, age.lim = c(540, 445), 
              data.lim = c(0, 11), ts.col = TRUE, label = "Class richness", 
              timescale = ICS2020, type = "n", abbrev = "Period")
mtext(text = "number of classes", side = 3, cex = 1.25)
cl.column <- cbind(c(tip.div.discrete$midpt, rev(tip.div.discrete$midpt)), 
                   c(tip.div.discrete$cl.mean - tip.div.discrete$cl.SE, 
                     rev(tip.div.discrete$cl.mean + tip.div.discrete$cl.SE)))
cl.column <- na.omit(cl.column)
polygon(cl.column[ ,1], cl.column[ ,2], col = trans.cols[2], lwd = 2, border = NA)
cl.column <- cbind(c(tip.div.continuous$midpt, rev(tip.div.continuous$midpt)), 
                   c(tip.div.continuous$cl.mean - tip.div.continuous$cl.SE, 
                     rev(tip.div.continuous$cl.mean + tip.div.continuous$cl.SE)))
cl.column <- na.omit(cl.column)
polygon(cl.column[ ,1], cl.column[ ,2], col = trans.cols[3], lwd = 2, border = NA)
cl.column <- cbind(c(all.div.continuous$midpt, rev(all.div.continuous$midpt)), 
                   c(all.div.continuous$cl.mean - all.div.continuous$cl.SE, 
                     rev(all.div.continuous$cl.mean + all.div.continuous$cl.SE)))
cl.column <- na.omit(cl.column)
polygon(cl.column[ ,1], cl.column[ ,2], col = trans.cols[4], lwd = 2, border = NA)
lines(tip.div.discrete$midpt, tip.div.discrete$cl.mean, lwd = 3, col = cols[2], lty = 2)
lines(tip.div.continuous$midpt, tip.div.continuous$cl.mean, lwd = 3, col = cols[3], lty = 3)
lines(all.div.continuous$midpt, all.div.continuous$cl.mean, lwd = 3, col = cols[4], lty = 4)
legend("bottomright", legend = c("tips only (binned) (G = 25)", "tips only (continuous) (G = 24)", 
       "tips + nodes (continuous)  (G = 20)"), box.col = NA, lty = c(2, 3, 4), 
       col = cols[2:4], lwd = 2, pch = NA, inset = .01)




## CORRELATIONS AMONG TRENDS ###################################################

# Select the "continuous" intervals to use to approximately compare to
# "discrete" intervals. But need to also remove the Ediacaran from the discrete
# trends, too, because absent in the continuous.

shared.bins <- c(1, 6, 13, 20, 26, 32, 40, 46, 50, 54, 57, 61, 65, 70, 76, 84, 94)
# Confirm all offset by less than 1 Myr
summary(apply(cbind(PBDB.div.discrete$midpt[-18],
                    all.div.continuous$midpt[shared.bins]), 1, diff))
# Average offset of 133,900 years (maximum absolute difference of 0.402 Myr)

# View to confirm well matched
round(cbind(PBDB.div.discrete$midpt[-18], 
            tip.div.discrete$midpt[-18],
            tip.div.continuous$midpt[shared.bins],
            all.div.continuous$midpt[shared.bins]), 1)

# Calculate detrended correlation coefficients
a <-
  diff(PBDB.div.discrete$genus[-18]) / diff(PBDB.div.discrete$midpt[-18])
b <-
  diff(tip.div.discrete$gen.mean.raw[-18]) / diff(tip.div.discrete$midpt[-18])
c <-
  diff(tip.div.continuous$gen.mean.raw[shared.bins]) / diff(tip.div.continuous$midpt[shared.bins])
d <-
  diff(all.div.continuous$gen.mean.raw[shared.bins]) / diff(all.div.continuous$midpt[shared.bins])

# Visualize
plot(a, b)
plot(a, c)
plot(a, d)

# Calculate correlations (updating a, b, c, d as needed)
trend1 <- a
trend2 <- d
plot(trend1, trend2)
abline(lm(trend1 ~ trend2), lty = 2)
round(cor(trend1, trend2), 4)
summary(lm(trend1 ~ trend2))

## RESULTS:
#         #  Tip (d) #  Tip (c) #  All (c)  #  
# - - - - - - - - - - - - - - - - - - - - - #
# PBDB    #  0.8830  #  0.7763  #  0.6736   #
# - - - - - - - - - - - - - - - - - - - - - #
# Tip (d) #          #  0.9180  #  0.9000   #
# - - - - - - - - - - - - - - - - - - - - - #
# Tip (c) #          #          #  0.9715   #

# Maximum p-value: 0.004224 (for PBDB vs all.div.continuous)





## Stacked class diversity #####################################################

# Create tallies (using continuous time scaling, tips + nodes, across trees)
unique.classes <- sort(unique(taxon.list[[1]][, "class"]))
class.bins <- matrix(NA, nrow = length(unique.classes), ncol = length(mids))
colnames(class.bins) <- if (ncol(class.bins) == nrow(ages)) 
  ages$interval_name else rev(seq.int(mids))
rownames(class.bins) <- unique.classes
pt.class.bins <- vector(mode = "list", length = ntrees)
for (i in 1:ntrees) pt.class.bins[[i]] <- class.bins
for (tr in 1:ntrees) {
  for (cl in 1:nrow(class.bins)) {
    wh.class <- which(taxon.list[[tr]][, "class"] == rownames(class.bins)[cl])
    for (t in 1:ncol(class.bins)) {
      wh.time <- which(ranges[[tr]][wh.class, "FAD"] > top[t] & 
                         ranges[[tr]][wh.class, "LAD"] < base[t])
      pt.class.bins[[tr]][cl, t] <- length(wh.time)
    }
  }
}

# Summarize across trees
class.bins <- apply(simplify2array(pt.class.bins), 1:2, mean)
head(class.bins)
# Save for future use
# save(class.bins, file = "class.bins")




## Identify support (no. of trees) for Cambrian origins for particular classes

# Best to use the binned ages/epochs time scale here

# Pick your class (from 'unique.classes')
(unique.classes <- sort(unique(taxon.list[[1]][, "class"])))
cl <- 4    # Crinoids
unique.classes[cl]

# Confirm that you are using the correct latest Cambrian bin (Cam/Ord = 485.4 Ma)
t <- 45
# t <- 16
# 4 if using epochs, 8 if using ages, and 45 if using 'continuous' [the latest
# bin that is completely in the Cambrian]
cbind(base, top)[t, ]
# Use 16 (if using ages) to identify Stage 2 echinoderms, 17 for Fortunian, 18
# for Ediacaran

tree.counts <- rep(0, length(taxon.list))
for(tr in 1:length(taxon.list)){
  wh.class <- which(taxon.list[[tr]][, "class"] == rownames(class.bins)[cl])
  # wh.class <- 1:731 # Use if identifying ALL echinoderms
  wh.time <-
    which(ranges[[tr]][wh.class, "FAD"] > top[t] & ranges[[tr]][wh.class, "LAD"] < base[t])
  tree.counts[[tr]] <- length(wh.time)
  }

# Number of trees (out of 100) that support a Cambrian origin for this class
sum(tree.counts > 0)
#  100 trees have Cambrian crinoids using discrete bins (ages)
#  98 trees have Terreneuvian (Stage 2) echinoderms: 
#             trees 20 & 45 lack Stage 2 echinoderms (59 trees root here)
#  39 trees have Terreneuvian (Fortunian) echinoderms (31 trees root here)
#   8 trees have Ediacaran echinoderms (2 UEH and 6 EAT)

## Modification to identify the most likely Cambrian crinoid genera
tree.counts <- vector("list", length(taxon.list))
for(tr in 1:length(taxon.list)){
  wh.class <- which(taxon.list[[tr]][1:366, "class"] == rownames(class.bins)[cl])
  # wh.class <- 1:731 # Use if identifying ALL echinoderms
  wh.time <-
    which(ranges[[tr]][wh.class, "FAD"] > top[t] & ranges[[tr]][wh.class, "LAD"] < base[t])
  tree.counts[[tr]] <- names(wh.time)
}
sort(table(unlist(tree.counts)))
# RESULTS: Likely latest Cambrian crinoids: Acolocrinus (in 100 trees),
# Aethocrinus (100), Apektocrinus (100), Cornucrinus (100), Hoplocrinus (100),
# Tripatocrinus (100), Glenocrinus (77), Titanocrinus (77), Alphacrinus (75),
# Eknomocrinus (54), Inyocrinus (44), Cnemecrinus (34), and Rosfacrinus,
# Pogonipocrinus, Carabocrinus, Hybocystis, Hybocrinus, Habrotecrinus,
# Maennilicrinus, Putilovocrinus, Quechuacrinus, Reteocrinus, Othneiocrinus, and
# Ramseyocrinus in 7 or fewer trees.

# Note that several of these (Cornucrinus, Acolocrinus, and Hoplocrinus) are a
# result of unresolved polytomies due to uncertain relationships, and should not
# be considered good candidates for Cambrian crinoids.

# Only Titanocrinus (7), Glenocrinus (7), and Apektocrinus (2) are inferred
# during the Miaolingian (and only in the EAT topology).

# Distribution of crinoid tip genera per tree
sq <- 1:length(tree.counts)
crin.counts <- sapply(sq, function(sq) length(tree.counts[[sq]]))
summary(crin.counts)

# Note no differences by tree model
summary(crin.counts[1:50])
summary(crin.counts[51:100])


## Modification to identify the most likely early echinoderm genera (& nodes) &
## their classes
tree.counts <- vector("list", length(taxon.list))
cl.counts <- vector("list", length(taxon.list))
for(tr in 1:length(taxon.list)){
  wh.class <- 1:731 # Use if identifying ALL echinoderms
  # wh.class <- 1:366 # Use if identifying just tip genera
  wh.time <-
    which(ranges[[tr]][wh.class, "FAD"] > top[t] & ranges[[tr]][wh.class, "LAD"] < base[t])
  tree.counts[[tr]] <- names(wh.time)
  if (length(tree.counts[[tr]]) > 0L)
    cl.counts[[tr]] <- taxon.list[[tr]][which(taxon.list[[tr]][, "genus"] %in% names(wh.time)), "class"]
}
sort(table(unlist(tree.counts)))
sort(table(unlist(cl.counts)))
tree.counts
cl.counts

tree.counts[c(35, 58, 80)] # Trees with Terreneuvian tip genera
cl.counts[c(35, 58, 80)]

# Likely Stage 2 echinoderms: Lots of ancestral nodes, plus helicoplacoids
# Polyplacus (in 30 trees), Waucobella (29), and Helicoplacus (29), Helicocystis
# (24), stem edrioasteroid Camptostroma (19), eocrinoid Felbabkacystis (13),
# solute Coleicarpus (9), and Ctenoimbricata, Rozanovicystis, Stromatocystites,
# Kinzercystis, Davidocinctus, Cambraster, Walcottidiscus, Vyscystis,
# Lepidocystis, Kailidiscus, Wudingeocrinus, Trochocystoides, Trochocystites,
# Nolichukia, Ceratocystis, and Alanisicystis in 5 or fewer trees.

# Likely Fortunian echinoderms: Lots of ancestral nodes. Only 1 tree each
# places stem echinoderm Ctenoimbricata, helicoplacoid Helicoplacus, and stem
# edrioasteroids Kailidiscus and Walcottidiscus in the Terreneuvian.

# All 8 trees that infer Ediacaran roots only place ancestral nodes at that
# time, and no tip genera.

## Modification to tally number of trees in which classes are present (either as
## tips or tips + nodes), and summary of time-scaled FADs (regardless of time
## bin), used for Table 2

#Pick time bin
t <- 4  # For Furongian if using epochs
cbind(base, top)[t, ]

for (cl in 1:21) {
  tree.counts.tips <- tree.counts.tips.and.nodes <- class.FADs <- rep(0, length(taxon.list))
  for (tr in 1:length(taxon.list)) {
    wh.class.tips <-
      which(taxon.list[[tr]][1:366, "class"] == rownames(class.bins)[cl])
    wh.class.tips.and.nodes <- 
      which(taxon.list[[tr]][, "class"] == rownames(class.bins)[cl])
    wh.time.tips <-
      which(ranges[[tr]][wh.class.tips, "FAD"] > top[t] &
              ranges[[tr]][wh.class.tips, "LAD"] < base[t])
    wh.time.tips.and.nodes <-
      which(ranges[[tr]][wh.class.tips.and.nodes, "FAD"] > top[t] &
              ranges[[tr]][wh.class.tips.and.nodes, "LAD"] < base[t])
    tree.counts.tips[[tr]] <- length(wh.time.tips)
    tree.counts.tips.and.nodes[[tr]] <- length(wh.time.tips.and.nodes)
    class.FADs[[tr]] <- max(ranges[[tr]][wh.class.tips.and.nodes, 1])
  }
  cat(unique.classes[cl], ":\n")
  cat("trees with tips + nodes:", sum(tree.counts.tips.and.nodes > 0), "; trees with tips: ", sum(tree.counts.tips > 0), "\n")
  cat(" Min. 1st Qu. Med. Mean 3rd Qu. Max.\n")
  cat(round(summary(class.FADs), 1), "\n\n")
}





## Alternative, to provide summary of time-scaled FAD for select crinoid genera

Camb.crinoids <- c("Aethocrinus", "Apektocrinus", "Tripatocrinus", 
                   "Glenocrinus", "Titanocrinus", "Alphacrinus", "Eknomocrinus")
sq <- 1:length(ranges)
for (cr in 1:length(Camb.crinoids)) {
  wh.row <- which(rownames(ranges[[1]]) == Camb.crinoids[cr])
  FADs <- sapply(sq, function(sq) ranges[[sq]][wh.row, 1])
  cat(Camb.crinoids[cr], ":\n")
  cat(" Min. 1st Qu. Med. Mean 3rd Qu. Max.\n")
  cat(round(summary(FADs), 1), "\n\n")
}

# If want to lump all together
wh.row <- which(rownames(ranges[[1]]) %in% Camb.crinoids)
summary(c(sapply(sq, function(sq) ranges[[sq]][wh.row, 1])))



## Plot diversity curve of classes
cl.div <- sapply(1:ncol(class.bins), function(x) sum(class.bins[, x] > 0))
geoscalePlot2(mids, cl.div, units = c("Epoch", "Period"), 
              tick.scale = "Period", boxes = "Age", cex.age = 0.65, 
              cex.ts = 0.7, cex.pt = 1, age.lim = c(540, 445), 
              data.lim = c(0, max(cl.div) + 1), ts.col = TRUE, label = "class richness", 
              timescale = ICS2020, type = "n", abbrev = "Period")
mtext(text = "class lineage richness", side = 3, cex = 1.25)
lines(mids, cl.div, lwd=3)

# Plot stacked plot version
# pdf(file = "Stacked class richness.pdf")
par(op)
par(mar = c(5, 4, 2, 2))
# Put most diverse at bottom
class.order <- order(table(taxon.list[[1]][, "class"]), decreasing = TRUE)
# Use next if prefer non-adjacents (Seeds 5, 11, 15, 16 are pleasing)
set.seed(5); stack.color <- sample(viridisLite::turbo(nrow(class.bins)))
stackpoly(x = -mids, y = t(class.bins[class.order, ]), col = stack.color,
          xat = -pretty(range(mids)), xlim = c(-541, -443), 
          xlab = "time", ylab = "number of genera", stack = TRUE, 
          main = "genus richness (by class)")
abline(v = -series.boundaries, col = "gray85", lty = 5)
abline(v = -pd.boundaries, col = "white", lty = 1, lwd = 2)
box()
legend("topleft", legend = rev(unique.classes[class.order]), pch = 15, ncol = 3, 
       pt.cex = 1, cex = .55, col = rev(stack.color), bty = "n")
par(op)
# dev.off()





## 5 - BODY SIZE ANALYSES ######################################################
# A. Calculate phylum body-size trend during Cambrian-Ordovician.
# B. Calculate model fits using paleoTS.
# C. Re-do, using sample-standardization.
# D. Re-do for (common) individual classes and orders. (Optional)

# Note this only uses the tip genera because body size was not inferred for
# ancestral nodes. It also only uses the mode treatment (although the size data
# is shared across treatments).

# Confirm row names of median.ranges matches that in 'x'
all.equal(row.names(ranges[[1]])[1:366], x$Genus)
# TRUE except for two subgenus mismatches in genus Anatifopsis
cbind(row.names(ranges[[1]])[1:366], x$Genus)[c(14, 166), ]

# Use if wish to select individual echinoderm class
group.name <- NULL; wh.gr <- 1:366; group <- x    # Use all echinoderms
# group.name <- "Crinoidea"; wh.gr <- which(x$Class == group.name); group <- x[wh.gr, ]
# group.name <- "Edrioasteroidea"; wh.gr <- which(x$Class == group.name); group <- x[wh.gr, ]
# group.name <- "Eocrinoidea"; wh.gr <- which(x$Class == group.name); group <- x[wh.gr, ]
# group.name <- "Stylophora"; wh.gr <- which(x$Class == group.name); group <- x[wh.gr, ]
# group.name <- "Rhombifera"; wh.gr <- which(x$Class == group.name); group <- x[wh.gr, ]
# group.name <- "Homostelea"; wh.gr <- which(x$Class == group.name); group <- x[wh.gr, ]
# group.name <- "Asteroidea"; wh.gr <- which(x$Class == group.name); group <- x[wh.gr, ]
group[1:6, 2:4]  # Confirm you've chosen the correct data




## Continuous body size trends #################################################

## Histogram
par(op)
hist(log10(group$BodyVolume), col = "darkgray", border = "white", n = 20, 
     main  =  "log10 body size distribution", xlab  =  "log10(Body volume, cm3)")
mtext(text = group.name, side = 3, line = -1, cex = 1)


## Test whether actually consistent log-normal
set.seed(3124) # Set seed so can replicate (LcKS uses Monte Carlo sampling)
Lc <- KScorrect::LcKS(group$BodyVolume, cdf = "plnorm")
Lc$D.obs   #       D = 0.0496
Lc$p.value # p-value = 0.0274
# For the Kolmogorov-Smirnov test, p-values greater than 0.05 are consistent
# with the given distribution. Therefore, the body-size distribution is
# approximately log-normally distributed.

# Aggregate size statistics
summary(log10(group$BodyVolume))
10 ^ summary(log10(group$BodyVolume))


## Summarize trend across trees (different FADs/LADs, although invariant sizes)

# Use the 100 continuous bins
if (length(mids) < 100L)
  stop("Rebuild the time scale so it has 100 continuous bins.\n")

sizes <- matrix(NA, nrow = length(mids), ncol = 5)
rownames(sizes) <- if (nrow(sizes) == nrow(ages)) 
  ages$interval_name else rev(seq.int(mids))
colnames(sizes) <- c("mean", "median", "n", "sd", "var")

pt.sizes <- vector(mode = "list", length = ntrees)
for (tr in 1:ntrees) pt.sizes[[tr]] <- sizes
for (tr in 1:ntrees) {
  for (t in 1:length(mids)) {
    wh.time <- which(ranges[[tr]][wh.gr, "FAD"] > top[t] &
                       ranges[[tr]][wh.gr, "LAD"] < base[t])
    if (length(wh.time) == 0L)
      next
    wh.sizes <- log10(group$BodyVolume[wh.time])
    pt.sizes[[tr]][t, "mean"] <- mean(wh.sizes, na.rm = TRUE)
    pt.sizes[[tr]][t, "median"] <- median(wh.sizes, na.rm = TRUE)
    pt.sizes[[tr]][t, "n"] <- length(na.omit(wh.sizes))
    pt.sizes[[tr]][t, "sd"] <- sd(wh.sizes, na.rm = TRUE)
    pt.sizes[[tr]][t, "var"] <- var(wh.sizes, na.rm = TRUE)
  }
}

# Save 'pt.sizes' for future uses
# save(pt.sizes, file = "pt.sizes")


# Summarize statistics across trees
sizes <- apply(simplify2array(pt.sizes), 1:2, mean, na.rm = TRUE)
sizes.sd <- apply(simplify2array(pt.sizes), 1:2, sd, na.rm = TRUE)

# Aggregate mean and median
10 ^ mean(sizes[, "mean"], na.rm = TRUE)   # 0.176 log10 cm3, 1.50 cm3
10 ^ sd(sizes[, "mean"], na.rm = TRUE)     # +- 0.180 log10 cm3, 1.51 cm3

10 ^ mean(sizes[, "median"], na.rm = TRUE) # 0.172 log10 cm3, 1.49 cm3
10 ^ sd(sizes[, "median"], na.rm = TRUE)   # +- 0.236 log10 cm3, 1.72 cm3

# Summarize across periods (note that interval 43 spans the C/O boundary so is
# in both)
Ord <- which(base > 443.4 & top < 485.4)
Cm <- which(base > 485.4)

mean(sizes[Ord, "mean"], na.rm = TRUE)       # 0.285 log10 cm3
10 ^ mean(sizes[Ord, "mean"], na.rm = TRUE)  # 1.93 cm3

mean(sizes[Cm, "mean"], na.rm = TRUE)        # 0.076 log10 cm3
10 ^ mean(sizes[Cm, "mean"], na.rm = TRUE)   # 1.19 cm3


## Plot size trend, with individual genus sizes and mean/median trend (only
## using tips)

lim <- range(log10(group$BodyVolume))
geoscalePlot2(mids, rep(lim[1], length(mids)), units = c("Epoch", "Period"), 
              tick.scale = "Period", boxes = "Age", cex.age = 0.65, 
              cex.ts = 0.7, cex.pt = 1, age.lim = c(540, 445), data.lim = lim, 
              ts.col = TRUE, label = "log10 body volume (cm3)", 
              timescale = ICS2020, type = "n", abbrev = "Period")
mtext(text = "Body size", side = 3, cex = 1.25)
mtext(text = group.name, side = 3, line = -1, cex = 1)
mean.column <- cbind(c(mids, rev(mids)), 
                    c(sizes[, "mean"] - sizes.sd[, "mean"], 
                      rev(sizes[, "mean"] + sizes.sd[, "mean"])))
mean.column <- na.omit(mean.column)
polygon(mean.column[ ,1], mean.column[ ,2], col = "#7F7F7F50", lwd = 2, border = NA)
for(tip in 1:366) {
  polygon(c(median.ranges[tip, "FAD"], median.ranges[tip, "LAD"]), 
          rep(log10(group$BodyVolume[tip]), 2), border = "gray35")
}
lines(mids, sizes[, "mean"], lwd = 3)
lines(mids, sizes[, "median"], lwd = 3, lty = 2)
legend("topleft", legend = c("mean", "median"), box.col = NA, lty = c(1, 2), 
       lwd = 2, pch = NA, inset = .05)
# Note that the Terreneuvian region is because some trees infer Stage 2 FADs for
# some tip genera. The wide variation is due to variation in tree ages.


# Plot just the trend
lim2 <- range(c(sizes[, 1:2]), na.rm = TRUE)
geoscalePlot2(mids, rep(lim2[1], length(mids)), units = c("Epoch", "Period"), 
              tick.scale = "Period", boxes = "Age", cex.age = 0.65, 
              cex.ts = 0.7, cex.pt = 1, age.lim = c(540, 445), data.lim = lim2, 
              ts.col = TRUE, label = "log10 body volume (cm3)", 
              timescale = ICS2020, type = "n", abbrev = "Period")
mtext(text = "Body size", side = 3, cex = 1.25)
mtext(text = group.name, side = 3, line = -1, cex = 1)
lines(mids, sizes[, "mean"], lwd = 3)
lines(mids, sizes[, "median"], lwd = 3, lty = 2)
legend("bottomright", legend = c("mean", "median"), box.col = NA, lty = c(1, 2), 
       lwd = 2, pch = NA, inset = .05)




## Calculate paleoTS model fits ################################################
# Algorithm: Calculate individually on each time-scaled tree, and summarize the
# weights across the trees

# Use the 100 continuous bins
if (length(mids) < 100L)
  stop("Rebuild the time scale so it has 100 continuous bins.\n")

fits <- ts <- vector(mode = "list", length = ntrees)
for (tr in 1:ntrees) {
  omits <- which(is.na(pt.sizes[[tr]][, "var"])) # omit if no data
  if (length(omits) == 0L) {
    ts[[tr]] <- paleoTS::as.paleoTS(mm = pt.sizes[[tr]][, "mean"],
                                    vv = pt.sizes[[tr]][, "var"],
                                    nn = pt.sizes[[tr]][, "n"],
                                    tt = mids, oldest = "last")
    } else {
    ts[[tr]] <- paleoTS::as.paleoTS(mm = pt.sizes[[tr]][-omits, "mean"],
                                    vv = pt.sizes[[tr]][-omits, "var"],
                                    nn = pt.sizes[[tr]][-omits, "n"],
                                    tt = mids[-omits], oldest = "last")
    }
  # In case of samples with zero variance, replace them with pooled variance
  # (thanks to Gene Hunt for solution!)
  zero.var <- ts[[tr]]$vv == 0
  ts[[tr]]$vv[zero.var] <- paleoTS::pool.var(ts[[tr]])
  # Calculate model fits
  fits[[tr]] <- paleoTS::fit3models(ts[[tr]], method = "Joint", pool = FALSE, 
                                    silent = TRUE)
}
beepr::beep() # ~ 1 minute


# Summarize model fits
model.fits <- fits[[1]]$modelFits
sq <- 1:length(fits)
for(c in 1: ncol(model.fits)) {
  model.fits[, c] <-
    apply(simplify2array(lapply(sq, function(sq)
      fits[[sq]]$modelFits[, c])), 1, mean, na.rm = TRUE)
}
print(model.fits)
# 0.713 support for URW and 0.287 support for GRW

# GRW distribution
par(op)
GRWs <- unlist(lapply(sq, function(sq) fits[[sq]]$modelFits$Akaike.wt[1]))
summary(GRWs)
hist(GRWs, main = "GRW support across trees", xlab = "Akaike weight", 
     xlim = c(0, 1))

# URW distribution
par(op)
URWs <- unlist(lapply(sq, function(sq) fits[[sq]]$modelFits$Akaike.wt[2]))
summary(URWs)
hist(URWs, main = "URW support across trees", xlab = "Akaike weight", 
     xlim = c(0, 1))

# How often is URW support > GRW support?
table(unlist(lapply(sq, function(sq) fits[[sq]]$modelFits$Akaike.wt[2])) > 
        unlist(lapply(sq, function(sq) fits[[sq]]$modelFits$Akaike.wt[1])))
# Only 3 exceptions: trees 10, 13, and 37 (but close to 50:50)
fits[[10]]$modelFits # 0.510 for GRW, 0.490 for URW
fits[[13]]$modelFits # 0.527 for GRW, 0.473 for URW
fits[[15]]$modelFits # 0.521 for GRW, 0.479 for URW

# Note no trees support the statis model
stasis <- unlist(lapply(sq, function(sq) fits[[sq]]$modelFits$Akaike.wt[3]))
summary(stasis)


# Save model fits for future uses
# GRWs.size <- GRWs; save(GRWs.size, file = "GRWs.size")
# URWs.size <- URWs; save(URWs.size, file = "URWs.size")



# Plot model support as stacked plot
model.order <- order(URWs)
model.matrix <- data.frame(URW = URWs[model.order], GRW = GRWs[model.order])
stackpoly(x = 1:100, y = model.matrix, col = cols[c(2, 5)],
          xat = c(1, seq(from = 5, to = 50, by = 5)), xlim = c(1, 50), 
          ylim = c(0, 1), xlab = "tree", ylab = "Akaike weight", stack = TRUE, 
          main = "Model support (sorted by weight)")
abline(h = 0.9, col = "white", lty = 5, lwd = 2)
abline(h = 0.5, col = "white", lty = 5, lwd = 2)
legend("bottomright", legend = c("General (biased) RW", "Unbiased RW"), pch = 22, 
       pt.cex = 4, cex = 1.5, pt.bg = cols[c(5, 2)], col = "white", bty = "n")


# If want to overlay the best-fit model (semi-juxtaposed)
geoscalePlot2(mids, rep(lim[1], length(mids)), units = c("Epoch", "Period"), 
              tick.scale = "Period", boxes = "Age", cex.age = 0.65, 
              cex.ts = 0.7, cex.pt = 1, age.lim = c(540, 445), data.lim = lim, 
              ts.col = TRUE, label = "log10 body volume (cm3)", 
              timescale = ICS2020, type = "n", abbrev = "Period")
mtext(text = "Body size", side = 3, cex = 1.25)
mtext(text = group.name, side = 3, line = -1, cex = 1)
for(tr in 1:length(fits)) {
  if (which.max(fits[[tr]]$modelFits$Akaike.wt) == 1) {
    plot(ts[[tr]], add = TRUE, modelFit = fitSimple(ts[[tr]], model = "GRW", 
                                                    pool = FALSE))
    }
  if (which.max(fits[[tr]]$modelFits$Akaike.wt) == 2) {
    plot(ts[[tr]], add = TRUE, modelFit = fitSimple(ts[[tr]], model = "URW", 
                                                    pool = FALSE))
    }
  if (which.max(fits[[tr]]$modelFits$Akaike.wt) == 3) {
    plot(ts[[tr]], add = TRUE, modelFit = fitSimple(ts[[tr]], model = "Stasis", 
                                                    pool = FALSE))
  }
}
# Overlay average
lines(mids, sizes[, "mean"], lwd = 3)
beepr::beep() # ~ 2 minutes





## Sample-standardized trend ###################################################

# Are there any missing body sizes?
if(anyNA(x$BodyVolume))
  stop("There are taxa with missing body sizes. Code below assumes there is no 
       missing data. If NAs are present, need to alter code.\n")

## Rarefaction parameters
ntrees <- length(ranges) # How many trees to sample?
std.g <- 24              # Number of genera per interval (24 for continuous, tips only)
numrep <- 2000           # How many replicates per time interval? 
#                        #   (Divided equally across time-scaled trees)
per.tree.reps <- numrep / ntrees  # Self-calculates per-tree replicates

if(!abs(per.tree.reps - round(per.tree.reps)) < .Machine$double.eps ^ 0.5)
  stop("The number of per-tree replicates need to be an integer. Choose a 
       different combination of 'numrep' and 'ntree' that will divide more equally.\n")

# For storing summary statistics
nc <- length(mids)
metrics <- data.frame(midpt = as.numeric(mids), genera = std.g, mean.size = NA, 
                      SE.mean = NA, median.size = NA, SE.median = NA)

# Create list to store 'numrep' replicates across trees
sub.sizes <- vector(mode = "list", length = numrep)
tmp.holder <- matrix(NA, nrow = length(mids), ncol = 2)
colnames(tmp.holder) <- c("mean", "median")
for (i in 1:numrep) sub.sizes[[i]] <- tmp.holder
cat("Mean trend uses average across", numrep, "sample-standardized replicates of", std.g, 
    "lineages \n (genus tips and/or ancestral nodes) per time bin, repeated across", 
    per.tree.reps, "replicates \n per each of", ntrees, "time-scaled trees.\n")

set.seed(3124) # So rarefaction can be replicated
increm <- 1    # Counter for incrementing replicates to 'sub.sizes' (first 'pt.rep's from tree #1, etc.)
for (tree in 1:ntrees) {
  # Record sample-standardized statistics, across tree-replicates
  for (pt.rep in  1:per.tree.reps) {
    for(t in 1:nc) {
      wh.bin <- as.vector(which(ranges[[tree]][1:366, "FAD"] > top[t] &
                                ranges[[tree]][1:366, "LAD"] < base[t]))
      if (std.g > length(wh.bin))
        next
      sampled <- sample2(wh.bin, std.g, replace = FALSE)
      sampled.sizes <- log10(group$BodyVolume[sampled])
      means  <- mean(sampled.sizes)
      medians <- median(sampled.sizes)
      sub.sizes[[increm]][t, ] <- c(means, medians)
    }
    increm <- increm + 1
  }
}
beepr::beep()
# ~ 25 sec. for 2000 replicates using 24 genera in each of 100 bins

# Calculate mean and SD across trees/replicates
sizes.means <- apply(simplify2array(sub.sizes), 1:2, mean)
sizes.means.sd <- apply(simplify2array(sub.sizes), 1:2, sd)
metrics[, c(3, 5)] <- sizes.means
metrics[, c(4, 6)] <- sizes.means.sd
print(na.omit(metrics))

# Save trends
# write.csv(metrics, file = "stdG24_size_tip_continuous.csv", row.names = FALSE)
# metrics <- read.csv(file = "stdG24_size_tip_continuous.csv")

# Plot continuous, sample-standardized body-size trend, tips only
mean.column <- cbind(c(metrics$midpt, rev(metrics$midpt)),
                     c(metrics$mean.size - metrics$SE.mean, rev(metrics$mean.size + metrics$SE.mean)))
mean.column <- na.omit(mean.column)
median.column <- cbind(c(metrics$midpt, rev(metrics$midpt)),
                       c(metrics$median.size - metrics$SE.median, 
                         rev(metrics$median.size + metrics$SE.median)))
median.column <- na.omit(median.column)
lim <- range(c(mean.column[, 2], median.column[, 2]), na.rm = TRUE)
geoscalePlot2(metrics$midpt, metrics$mean.size, units = c("Epoch", "Period"), 
              tick.scale = "Period", boxes = "Age", cex.age = 0.65, 
              cex.ts = 0.7, cex.pt = 1, age.lim = c(540, 445), 
              data.lim = lim, ts.col = TRUE, 
              label = paste0("body volume (log10 cm3, G = ", std.g, ")"), 
              timescale = ICS2020, type = "n", abbrev = "Period")
mtext(text = "sample-standardized body size (continuous, tips only)", side = 3, cex = 1.25)
polygon(median.column[ ,1], median.column[ ,2], col = trans.cols[7], lwd = 2, border = NA)
polygon(mean.column[ ,1], mean.column[ ,2], col = trans.cols[2], lwd = 2, border = NA)
lines(metrics$midpt, metrics$mean.size, lty = 1, lwd = 3, col = cols[2])
lines(metrics$midpt, metrics$median.size, lty = 2, lwd = 3, col = cols[7])
legend("topleft", legend = c("mean", "median"), box.col = NA, 
       lty = c(1, 2), col = cols[c(2, 7)], lwd = 2, pch = NA, inset = .05)



# Plot trends (overlaying raw sizes)
# Plot size trend, with individual genus sizes and mean trend
lim <- range(log10(group$BodyVolume))
geoscalePlot2(mids, rep(lim[1], length(mids)), units = c("Epoch", "Period"), 
              tick.scale = "Period", boxes = "Age", cex.age = 0.65, 
              cex.ts = 0.7, cex.pt = 1, age.lim = c(540, 445), data.lim = lim, 
              ts.col = TRUE, label = "log10 body volume (cm3)", 
              timescale = ICS2020, type = "n", abbrev = "Period")
mtext(text = "Body size", side = 3, cex = 1.25)
mtext(text = group.name, side = 3, line = -1, cex = 1)
for(tip in 1:366) {
  polygon(c(median.ranges[tip, "FAD"], median.ranges[tip, "LAD"]), 
          rep(log10(group$BodyVolume[tip]), 2), border = "gray35")
}
polygon(median.column[ ,1], median.column[ ,2], col = trans.cols[7], lwd = 2, border = NA)
polygon(mean.column[ ,1], mean.column[ ,2], col = trans.cols[2], lwd = 2, border = NA)
lines(metrics$midpt, metrics$mean.size, lty = 1, lwd = 3, col = cols[2])
lines(metrics$midpt, metrics$median.size, lty = 2, lwd = 3, col = cols[7])
legend("topleft", legend = c("mean", "median"), box.col = NA, 
       lty = c(1, 2), col = cols[c(2, 7)], lwd = 2, pch = NA, inset = .05)

# Conclusion: Because the body-size distribution is approximately normally
# distributed, sample standardization has a negligible effect.



## Do different classes have different body sizes? --------------
cl.table <- table(group$Class)
classes <- cl.table[which(cl.table >= 10L)]
out <- data.frame(Class = names(classes), log.mean = NA, log.sd = NA)
for(c in 1:length(classes)){
  wh.cl <- which(group$Class == out$Class[c])
  BVs <- na.omit(log10(group$BodyVolume[wh.cl]))
  out$log.mean[c] <- round(mean(BVs), 2)
  out$log.sd[c] <- round(sd(BVs), 2)
}
sort.o <- order(out$log.mean, decreasing = TRUE)
out[sort.o, ]

# All classes (alphabetical order)
bp.names <- substr(sort(unique(group$Class)), 1, 1)
boxplot(log10(group$BodyVolume) ~ group$Class, names = bp.names,
        main = "Body volume (by class)")

# Most diverse classes
div.cl <- which(group$Class %in% names(classes))
bp.names <- substr(names(classes), 1, 2)
boxplot(log10(group$BodyVolume[div.cl]) ~ group$Class[div.cl], names = bp.names, 
        main = "Body volume (by class)")




## Discrete body size trends (using inferred ancestors) ########################

# Pick traits to summarize and build matrices to store summary statistics
trait.col.names <- 1 # which column is body volume code?
trait.abbr <- c("BV")
trait.long <- c("Body volume (log10 bins)")

# Any multistate ancestral state inferences?
for(tr in 1:ntrees) {
  cat(tr)
  print(table(mode.anc[[tr]]$matrix_1$matrix[, trait.col.names]))
}
# Several trees have multistate ancestors (i.e., 50:50 probability of size bin 2
# and 3 so inferred as 2/3). These are not treated as a new life habit
# combination so long as at least one other lineage is inferred as a 2 and
# another as a 3.

## Summarize trend across trees (different FADs/LADs, although invariant traits)
traits <- matrix(NA, nrow = length(mids), ncol = (length(trait.col.names) * 2))
rownames(traits) <- if (nrow(traits) == nrow(ages))
  ages$interval_name else rev(seq.int(mids))
colnames(traits) <-
  paste0(rep(paste0(trait.abbr, "."), 2), c("mean", "median"))

pt.traits <- vector(mode = "list", length = ntrees)
for (tr in 1:ntrees) pt.traits[[tr]] <- traits
# Work with tmp version because will override values
tmp <- mode.anc
for (tr in 1:ntrees) {
  # If any multistate sizes (e.g., 0/1) replace with their average
  wh.1.2 <- which(tmp[[tr]]$matrix_1$matrix[, trait.col.names] == "1/2")
  wh.2.3 <- which(tmp[[tr]]$matrix_1$matrix[, trait.col.names] == "2/3")
  wh.3.4 <- which(tmp[[tr]]$matrix_1$matrix[, trait.col.names] == "3/4")
  tmp[[tr]]$matrix_1$matrix[, trait.col.names][wh.1.2] <- 1.5
  tmp[[tr]]$matrix_1$matrix[, trait.col.names][wh.2.3] <- 2.5
  tmp[[tr]]$matrix_1$matrix[, trait.col.names][wh.3.4] <- 3.5
  for (t in 1:length(mids)) {
    wh.time <- which(ranges[[tr]][wh.gr, "FAD"] > top[t] &
                       ranges[[tr]][wh.gr, "LAD"] < base[t])
    if (length(wh.time) == 0L)
      next
    for(trait in 1:length(trait.col.names)) {
      wh.traits <- as.numeric(tmp[[tr]]$matrix_1$matrix[wh.time, trait.col.names[trait]])
      pt.traits[[tr]][t, trait] <- mean(wh.traits, na.rm = TRUE)
      pt.traits[[tr]][t, (trait + length(trait.col.names))] <- median(wh.traits, na.rm = TRUE)
    }
  }
}

# Summarize statistics across trees
traits <- apply(simplify2array(pt.traits), 1:2, mean, na.rm = TRUE)
traits.SE <- apply(simplify2array(pt.traits), 1:2, sd, na.rm = TRUE)

## Plot trends (+/- SE)
par(op)
lim <- c(0, 5)
lcn <- length(trait.col.names)
for(trait in 1:lcn) {
  geoscalePlot2(mids, rep(lim[1], length(mids)), units = c("Epoch", "Period"), 
                tick.scale = "Period", boxes = "Age", cex.age = 0.65, 
                cex.ts = 0.7, cex.pt = 1, age.lim = c(540, 445), data.lim = lim, 
                ts.col = TRUE, label = trait.long, 
                timescale = ICS2020, type = "n", abbrev = "Period")
  mtext(text = "Body volume", side = 3, cex = 1.25)
  mtext(text = group.name, side = 3, line = -1, cex = 1)
  mean.column <- cbind(c(mids, rev(mids)), 
                       c(traits[, trait] - traits.SE[, trait], 
                         rev(traits[, trait] + traits.SE[, trait])))
  mean.column <- na.omit(mean.column)
  median.column <- cbind(c(mids, rev(mids)), 
                         c(traits[, c(trait + lcn)] - traits.SE[, c(trait + lcn)], 
                           rev(traits[, c(trait + lcn)] + traits.SE[, c(trait + lcn)])))
  median.column <- na.omit(median.column)
  polygon(median.column[ ,1], median.column[ ,2], col = trans.cols[7], lwd = 2, border = NA)
  polygon(mean.column[ ,1], mean.column[ ,2], col = trans.cols[2], lwd = 2, border = NA)
  lines(mids, traits[, trait], col = cols[2], lwd = 3)
  lines(mids, traits[, (trait + lcn)], col = cols[7], lwd = 3, lty = 2)
  legend("topleft", legend = c("mean", "median"), box.col = NA, lty = c(1, 2), 
         col = cols[c(2,7)], lwd = 2, pch = NA, inset = .05)
}
par(op)



## Trend through time, as stacked plot

## Replace quantitative codings with factors

# Note (from above) that all trees have all four factor levels (simplifying
# table() below, but essentially irrelevant because converted to factors with
# pre-set levels, which table will default to.)

# Note several taxa have two-state options (e.g., size "1/2") and will be
# dropped when converted to factor levels (but added back in secondarily)

# Because the trait matrix is a matrix (which cannot store factors), store trait
# column separately
trait.levels <- as.character(0:5)
trait.values <- vector(mode = "list", length = ntrees)
for (tr in 1:ntrees) {
  trait.values[[tr]] <- factor(mode.anc[[tr]]$matrix_1$matrix[, trait.col.names], 
                               levels = trait.levels, ordered = TRUE)
}

# Calculate average number of occurrences (across trees and across time)
trait.occs <- matrix(data = NA, nrow = length(mids), 
                     ncol = length(trait.levels))
rownames(trait.occs) <- if (nrow(trait.occs) == nrow(ages))
  ages$interval_name else rev(seq.int(mids))
colnames(trait.occs) <- trait.levels
pt.traits <- vector(mode = "list", length = ntrees)
for (tr in 1:ntrees) pt.traits[[tr]] <- trait.occs
for (tr in 1:ntrees) {
  for (t in 1:length(mids)) {
    wh.time <- which(ranges[[tr]][wh.gr, "FAD"] > top[t] &
                       ranges[[tr]][wh.gr, "LAD"] < base[t])
    if (length(wh.time) == 0L)
      next
    pt.traits[[tr]][t, ] <- table(trait.values[[tr]][wh.time])
  }
}

# Manually add back in skipped multistate ancestors
for(tr in 1:ntrees){
  which.multi <- which(!trait.values[[tr]] %in% trait.levels)
  if (length(which.multi) == 0L)
    next
  for(g in 1:length(which.multi)) {
    wh.state <- mode.anc[[tr]]$matrix_1$matrix[which.multi[g], trait.col.names]
    wh.time <- which(base > ranges[[tr]][which.multi[g], "LAD"] & 
                       top < ranges[[tr]][which.multi[g], "FAD"])
    # Go to next, if pre-Cambrian (and below the oldest 'base' used here)
    if (length(wh.time) == 0L)
      next
    if (wh.state == "0/1")
      pt.traits[[tr]][wh.time, 1:2] <- pt.traits[[tr]][wh.time, 1:2] + 1
    if (wh.state == "1/2")
      pt.traits[[tr]][wh.time, 2:3] <- pt.traits[[tr]][wh.time, 2:3] + 1
    if (wh.state == "2/3")
      pt.traits[[tr]][wh.time, 3:4] <- pt.traits[[tr]][wh.time, 3:4] + 1
    if (wh.state == "3/4")
      pt.traits[[tr]][wh.time, 4:5] <- pt.traits[[tr]][wh.time, 4:5] + 1
    if (wh.state == "4/5")
      pt.traits[[tr]][wh.time, 5:6] <- pt.traits[[tr]][wh.time, 5:6] + 1
  }
}
    
# Summarize statistics across trees
trait.occs <- apply(simplify2array(pt.traits), 1:2, mean, na.rm = TRUE)
trait.occs.SE <- apply(simplify2array(pt.traits), 1:2, sd, na.rm = TRUE)
mean.SE <- mean(trait.occs.SE, na.rm = TRUE)

# Plot as a time-independent pie chart
par(mar = c(0, 0, 1, 0))
pie(apply(trait.occs, 2, mean, na.rm = TRUE), col = cols, labels = trait.levels, 
    main = trait.long, clockwise = TRUE)
par(op)

# Graph as stacked plots, through time (average proportions across trees)
par(mar = c(4, 4, 1, 2.5))
# Replace missing values with zeros
wh.missing <- which(is.na(trait.occs), arr.ind = TRUE)
trait.occs[wh.missing] <- 0
stackpoly(x = -mids, y = trait.occs, col = cols, 
          xlab = "time (Ma)", ylab = "number of lineages", stack = TRUE, 
          xat = -pretty(range(mids)))
mtext(trait.long, side = 3, cex = 1.25)
abline(v = -series.boundaries, col = "gray85", lty = 5)
abline(v = -pd.boundaries, col = "white", lty = 1, lwd = 2)
box()
legend("topleft", legend = paste("size bin", rev(trait.levels)), pch = 15, 
       col = cols[length(trait.levels):1], ncol = 1, 
       pt.cex = 2, cex = .85, bty = "n")
SE.start <- 167
SE.end <- SE.start + mean.SE
SE.time <- -538.5
lines(x = c(SE.time, SE.time), y = c(SE.start, SE.end), lwd = 1, lty = 1, col = "gray35")
points(x = SE.time, y = SE.start, pch = "-", cex = .75, col = "gray35")
points(x = SE.time, y = SE.end, pch = "-", cex = .75, col = "gray35")

# Same but proportionally standardized:
sums <- apply(trait.occs, 1, sum)
trait.props <- trait.occs / sums
# Replace missing values with zeros
wh.missing <- which(is.na(trait.props), arr.ind = TRUE)
trait.props[wh.missing] <- 0
stackpoly(x = -mids, y = trait.props, col = cols, 
          xlab = "time (Ma)", ylab = "proportion of lineages", stack = TRUE, 
          xat = -pretty(range(mids)), xlim = c(-541, -443), 
          ylim = c(0,1))
mtext(trait.long, side = 3, cex = 1.25)
abline(v = -series.boundaries, col = "gray85", lty = 5)
abline(v = -pd.boundaries, col = "white", lty = 1, lwd = 2)
box()
legend("left", legend = paste("size bin", rev(trait.levels)), pch = 22, 
       col = "white", pt.bg = cols[length(trait.levels):1], ncol = 1, 
       pt.cex = 2, cex = .85, bty = "n", text.col = "white", inset = c(0.025, 0.025))
par(op)









## 6 - TIERING ANALYSES ########################################################
# Tiering is a pattern of space utilization above and below the seafloor through
# time, for filter-feeding animals. But here we are plotting for ALL
# echinoderms to get a sense of overall habitat usage.)

# Like for body size, only using tip genera (because continuous variable
# AbsStratDist was not inferred for ancestral nodes).

# A. Calculate phylum tiering trend during Cambrian-Ordovician.
# B. Calculate model fits using paleoTS.
# C. Re-do, using sample-standardization.
# D. Re-do for (common) individual classes and orders. (Optional)


## Create separate infaunal and epifaunal filter-feeding trends

# Use if wish to select individual echinoderm class
group.name <- NULL; wh.gr <- 1:366; group <- x    # Use all echinoderms
# group.name <- "Crinoidea"; wh.gr <- which(x$Class == group.name); group <- x[wh.gr, ]
# group.name <- "Edrioasteroidea"; wh.gr <- which(x$Class == group.name); group <- x[wh.gr, ]
# group.name <- "Eocrinoidea"; wh.gr <- which(x$Class == group.name); group <- x[wh.gr, ]
# group.name <- "Stylophora"; wh.gr <- which(x$Class == group.name); group <- x[wh.gr, ]
# group.name <- "Rhombifera"; wh.gr <- which(x$Class == group.name); group <- x[wh.gr, ]
# group.name <- "Homostelea"; wh.gr <- which(x$Class == group.name); group <- x[wh.gr, ]
# group.name <- "Asteroidea"; wh.gr <- which(x$Class == group.name); group <- x[wh.gr, ]
group[1:6, 2:4]  # Confirm you've chosen the correct data


# Tiering is typically only discussed in terms of filter feeders. Extract out
# just the filter feeders (which for echinoderms is approximately 90% of them.)
wh.ff <- which(group$FilterFeeder == 1)
# Excludes 34 genera of asteroids, crinoid Protaxocrinus (coded as raptorial),
# echinoids, holothuroids, ophiuroids, somasteroids, and stenuroids.
table(group$Class[wh.ff])

# Which are infaunal? 6 mitrate stylophorans
wh.inf <- which(group$AbsStratDistance < 0)
group[wh.inf, c(3, 5, 10)]


## Tiering trends ##############################################################

## Histograms (for all filter feeders)
par(op)
hist(group$AbsStratDistance[wh.ff], col = "darkgray", border = "white", n = 20, 
     main  =  "Tiering distribution", xlab = "Distance from sea floor (mm)")
abline(v = 0)
mtext(text = group.name, side = 3, line = -1, cex = 1)

# "Epifaunal" only because log-transforming
hist(log10(group$AbsStratDistance[wh.ff]), col = "darkgray", border = "white", n = 20, 
     main  =  "Epifaunal tiering distribution", 
     xlab = "Distance from sea floor (log10 mm)")
mtext(text = group.name, side = 3, line = -1, cex = 1)


## Test whether actually consistent log-normal
epi.ffs <- group$AbsStratDistance[wh.ff][group$AbsStratDistance[wh.ff] > 0]
set.seed(3124) # Set seed so can replicate (LcKS uses Monte Carlo sampling)
Lc <- KScorrect::LcKS(na.omit(epi.ffs), nreps = 9999, cdf = "plnorm")
Lc$p.value # p-value = 0.0315
# For the Kolmogorov-Smirnov test, p-values greater than 0.05 are consistent
# with the given distribution. Therefore, the epifaunal tiering distribution is
# not log-normally distributed. (However, comparisons with other distributions
# show that lognormal is the second best fitting after mixture models with more
# than 4 subdistributions.)



## Summarize trend across trees (different FADs/LADs, although invariant tiers)

# Only including epifaunal taxa (because log-transformed and because only 6
# infaunal echinoderms)
tiers <- matrix(NA, nrow = length(mids), ncol = 7)
rownames(tiers) <- if (nrow(tiers) == nrow(ages)) 
  ages$interval_name else rev(seq.int(mids))
colnames(tiers) <- c("mean", "median", "n", "sd", "var", "75th", "99th")

pt.tiers <- vector(mode = "list", length = ntrees)
for (tr in 1:ntrees) pt.tiers[[tr]] <- tiers
for (tr in 1:ntrees) {
  for (t in 1:length(mids)) {
    wh.time <- which(ranges[[tr]][wh.ff, "FAD"] > top[t] &
                       ranges[[tr]][wh.ff, "LAD"] < base[t])
    if (length(wh.time) == 0L)
      next
    wh.tiers <- log10(group$AbsStratDistance[wh.ff[wh.time]])
    pt.tiers[[tr]][t, "mean"] <- mean(wh.tiers, na.rm = TRUE)
    pt.tiers[[tr]][t, "median"] <- median(wh.tiers, na.rm = TRUE)
    pt.tiers[[tr]][t, "n"] <- length(na.omit(wh.tiers))
    pt.tiers[[tr]][t, "sd"] <- sd(wh.tiers, na.rm = TRUE)
    pt.tiers[[tr]][t, "var"] <- var(wh.tiers, na.rm = TRUE)
    pt.tiers[[tr]][t, "75th"] <- quantile(wh.tiers, probs = 0.75, na.rm = TRUE)
    pt.tiers[[tr]][t, "99th"] <- quantile(wh.tiers, probs = 0.99, na.rm = TRUE)
  }
}
beepr::beep()

# Save 'pt.tiers' for future uses
# save(pt.tiers, file = "pt.tiers")



# Summarize statistics across trees
tiers <- apply(simplify2array(pt.tiers), 1:2, mean, na.rm = TRUE)
head(tiers)

# Aggregate mean and median
10 ^ mean(tiers[, "mean"], na.rm = TRUE)   # 21.8 mm
10 ^ sd(tiers[, "mean"], na.rm = TRUE)     # +- 1.8 mm

10 ^ mean(tiers[, "median"], na.rm = TRUE) # 23.3 mm
10 ^ sd(tiers[, "median"], na.rm = TRUE)   # +- 2.0 mm

10 ^ mean(tiers[, "75th"], na.rm = TRUE) # 64.4 mm
10 ^ sd(tiers[, "75th"], na.rm = TRUE)   # +- 2.3 mm

10 ^ mean(tiers[, "99th"], na.rm = TRUE) # 223 mm
10 ^ sd(tiers[, "99th"], na.rm = TRUE)   # +- 3.2 mm

# Cambrian statistics:
wh.cam <- which(mids > 485.4)
10 ^ mean(tiers[wh.cam, "mean"], na.rm = TRUE)   # 13.6 mm
10 ^ sd(tiers[wh.cam, "mean"], na.rm = TRUE)     # +- 1.4 mm

10 ^ mean(tiers[wh.cam, "median"], na.rm = TRUE) # 13.6 mm
10 ^ sd(tiers[wh.cam, "median"], na.rm = TRUE)   # +- 1.6 mm

10 ^ mean(tiers[wh.cam, "75th"], na.rm = TRUE) # 33.6 mm
10 ^ sd(tiers[wh.cam, "75th"], na.rm = TRUE)   # +- 1.7 mm

10 ^ mean(tiers[wh.cam, "99th"], na.rm = TRUE) # 100.3 mm
10 ^ sd(tiers[wh.cam, "99th"], na.rm = TRUE)   # +- 2.8 mm

# Ordovician statistics:
10 ^ mean(tiers[-wh.cam, "mean"], na.rm = TRUE)   # 36.5 mm
10 ^ sd(tiers[-wh.cam, "mean"], na.rm = TRUE)     # +- 1.4 mm

10 ^ mean(tiers[-wh.cam, "median"], na.rm = TRUE) # 41.8 mm
10 ^ sd(tiers[-wh.cam, "median"], na.rm = TRUE)   # +- 1.4 mm

10 ^ mean(tiers[-wh.cam, "75th"], na.rm = TRUE) # 131.3 mm
10 ^ sd(tiers[-wh.cam, "75th"], na.rm = TRUE)   # +- 1.4 mm

10 ^ mean(tiers[-wh.cam, "99th"], na.rm = TRUE) # 536.0 mm
10 ^ sd(tiers[-wh.cam, "99th"], na.rm = TRUE)   # +- 1.6 mm


## Any evidence for discrete (multimodal) tiers? ###############################

# First, identify best model (analyzed at epoch level), tallying best model
# across trees (allowing 1:9 lognormal mixtures, and allowing both equal and
# variable variances, totaling 18 models). Allowing this many mixtures is
# absolutely not  justified by the modest sample sizes, but because BIC model
# selection penalizes against model complexity, the more complicated models will
# only be supported when they are especially strong fits.

models <- matrix(0, nrow = length(mids), ncol = 18)
rownames(models) <- if (nrow(models) == nrow(ages)) 
  ages$interval_name else rev(seq.int(mids))
colnames(models) <- c("E,1", "V,1", "E,2", "V,2", "E,3", "V,3", "E,4", "V,4", 
                      "E,5", "V,5", "E,6", "V,6", "E,7", "V,7", "E,8", "V,8", 
                      "E,9", "V,9")

pt.models <- vector(mode = "list", length = ntrees)
for (tr in 1:ntrees) pt.models[[tr]] <- models
for (tr in 1:ntrees) {
  for (t in 1:length(mids)) {
    wh.time <- which(ranges[[tr]][wh.ff, "FAD"] > top[t] &
                       ranges[[tr]][wh.ff, "LAD"] < base[t])
    if (length(wh.time) == 0L)
      next
    wh.tiers <- log10(group$AbsStratDistance[wh.ff[wh.time]])
    # Mixture model fitting
    m <- mclust::mclustBIC(na.omit(wh.tiers), G = 1:9, modelNames = c("E", "V"), 
                           verbose = FALSE)
    m <- mclust::pickBIC(m, k = sum(!is.na(m)))
    best.model <- names(m)[1]
    pt.models[[tr]][t, match(best.model, colnames(models))] <- 1
  }
}
beepr::beep()

# Tally across trees
models <- apply(simplify2array(pt.models), 1:2, sum, na.rm = TRUE)
models

apply(models, 2, sum)

# Across trees, BIC model fitting overwhelmingly supports lognormal
# distributions for all epochs (except for Hirnantian that has strong support
# for trimodal distributions, but edge effects might impact confidence in
# results). Most intervals have modest support for bimodal distributions.


## Plot tiering trend, with individual genus tiering heights and mean/median
## trend (only using tips), but here including infaunal and epifaunal genus
## ranges. Trends are only for epifaunal tiers.

lim <- range(group$AbsStratDistance, na.rm = TRUE)
# Magnify infaunal if shallow
if (min(lim) > -10)
  lim[1] <- -10
median.column <- cbind(c(mids, rev(mids)), 
                       10 ^ (c(tiers[, "median"], rep(0, length(mids)))))
median.column <- na.omit(median.column)
column.75th <- cbind(c(mids, rev(mids)), 
                     10 ^ (c(tiers[, "75th"], rep(0, length(mids)))))
column.75th <- na.omit(column.75th)
column.99th <- cbind(c(mids, rev(mids)), 
                     10 ^ (c(tiers[, "99th"], rep(0, length(mids)))))
column.99th <- na.omit(column.99th)

geoscalePlot2(mids, rep(lim[1], length(mids)), units = c("Epoch", "Period"), 
              tick.scale = "Period", boxes = "Age", cex.age = 0.65, 
              cex.ts = 0.7, cex.pt = 1, age.lim = c(540, 445), data.lim = lim, 
              ts.col = TRUE, label = "distance from seafloor (mm)", 
              timescale = ICS2020, type = "n", abbrev = "Period")
mtext(text = "Tiering", side = 3, cex = 1.25)
mtext(text = group.name, side = 3, line = -1, cex = 1)
# Alternative (grayscale) shading
# polygon(column.99th[ ,1], column.99th[ ,2], col = "#7F7F7F60", lwd = 2, border = NA)
# polygon(column.75th[ ,1], column.75th[ ,2], col = "#7F7F7F60", lwd = 2, border = NA)
# polygon(median.column[ ,1], median.column[ ,2], col = "#7F7F7F60", lwd = 2, border = NA)
# polygon(median.column[ ,1], median.column[ ,2], col = "#7F7F7F60", lwd = 2, border = NA)
polygon(column.99th[ ,1], column.99th[ ,2], col = trans.cols[2], lwd = 2, border = NA)
polygon(column.75th[ ,1], column.75th[ ,2], col = trans.cols[5], lwd = 2, border = NA)
polygon(median.column[ ,1], median.column[ ,2], col = trans.cols[7], lwd = 2, border = NA)
for(tip in wh.ff) {
  polygon(c(median.ranges[tip, "FAD"], median.ranges[tip, "LAD"]), 
          rep(group$AbsStratDistance[tip], 2), border = "gray35")
}
# Highlight infaunal ranges
for(tip in wh.inf) {
  polygon(c(median.ranges[tip, "FAD"], median.ranges[tip, "LAD"]), 
          rep(group$AbsStratDistance[tip], 2), border = cols[7])
}
abline(h = 0, lwd = 2)




# Just plot trend
geoscalePlot2(mids, rep(lim[1], length(mids)), units = c("Epoch", "Period"), 
              tick.scale = "Period", boxes = "Age", cex.age = 0.65, 
              cex.ts = 0.7, cex.pt = 1, age.lim = c(540, 445), data.lim = lim, 
              ts.col = TRUE, label = "distance from seafloor (mm)", 
              timescale = ICS2020, type = "n", abbrev = "Period")
mtext(text = "Tiering", side = 3, cex = 1.25)
mtext(text = group.name, side = 3, line = -1, cex = 1)
# Alternative (grayscale) shading
# polygon(column.99th[ ,1], column.99th[ ,2], col = "#7F7F7F60", lwd = 2, border = NA)
# polygon(column.75th[ ,1], column.75th[ ,2], col = "#7F7F7F60", lwd = 2, border = NA)
# polygon(median.column[ ,1], median.column[ ,2], col = "#7F7F7F60", lwd = 2, border = NA)
# polygon(median.column[ ,1], median.column[ ,2], col = "#7F7F7F60", lwd = 2, border = NA)
polygon(column.99th[ ,1], column.99th[ ,2], col = trans.cols[2], lwd = 2, border = NA)
polygon(column.75th[ ,1], column.75th[ ,2], col = trans.cols[5], lwd = 2, border = NA)
polygon(median.column[ ,1], median.column[ ,2], col = trans.cols[7], lwd = 2, border = NA)
abline(h = 0, lwd = 2)






## Calculate paleoTS model fits ################################################
# Algorithm: Calculate individually on each time-scaled tree, and summarize the
# weights across the trees

fits <- ts <- vector(mode = "list", length = ntrees)
for (tr in 1:ntrees) {
  omits <- which(is.na(pt.tiers[[tr]][, "var"])) # omit if no data
  if (length(omits) == 0L) {
    ts[[tr]] <- paleoTS::as.paleoTS(mm = pt.tiers[[tr]][, "mean"],
                                    vv = pt.tiers[[tr]][, "var"],
                                    nn = pt.tiers[[tr]][, "n"],
                                    tt = mids, oldest = "last")
  } else {
    ts[[tr]] <- paleoTS::as.paleoTS(mm = pt.tiers[[tr]][-omits, "mean"],
                                    vv = pt.tiers[[tr]][-omits, "var"],
                                    nn = pt.tiers[[tr]][-omits, "n"],
                                    tt = mids[-omits], oldest = "last")
  }
  # In case of samples with zero variance, replace them with pooled variance
  # (thanks to Gene Hunt for solution)
  zero.var <- ts[[tr]]$vv == 0
  ts[[tr]]$vv[zero.var] <- paleoTS::pool.var(ts[[tr]])
  # Calculate model fits
  fits[[tr]] <- paleoTS::fit3models(ts[[tr]], method = "Joint", pool = FALSE, 
                                    silent = TRUE)
}
beepr::beep() # ~ 2 minutes

# Summarize model fits
model.fits <- fits[[1]]$modelFits
sq <- 1:length(fits)
for(c in 1: ncol(model.fits)) {
  model.fits[, c] <-
    apply(simplify2array(lapply(sq, function(sq)
      fits[[sq]]$modelFits[, c])), 1, mean, na.rm = TRUE)
}
print(model.fits)
# URW = 0.593, GRW = 0.407, stasis = 0.000

# GRW distribution
par(op)
GRWs <- unlist(lapply(sq, function(sq) fits[[sq]]$modelFits$Akaike.wt[1]))
summary(GRWs)
hist(GRWs, main = "GRW support across trees", xlab = "Akaike weight", 
     xlim = c(0, 1))

# URW distribution
par(op)
URWs <- unlist(lapply(sq, function(sq) fits[[sq]]$modelFits$Akaike.wt[2]))
summary(URWs)
hist(URWs, main = "URW support across trees", xlab = "Akaike weight", 
     xlim = c(0, 1))

# How often is URW support > GRW support?
table(unlist(lapply(sq, function(sq) fits[[sq]]$modelFits$Akaike.wt[2])) > 
        unlist(lapply(sq, function(sq) fits[[sq]]$modelFits$Akaike.wt[1])))
# 22 have URW > GRW

# Note no trees support the statis model
stasis <- unlist(lapply(sq, function(sq) fits[[sq]]$modelFits$Akaike.wt[3]))
summary(stasis)

# Save model fits for future uses
# GRWs.tier <- GRWs; save(GRWs.tier, file = "GRWs.tier")
# URWs.tier <- URWs; save(URWs.tier, file = "URWs.tier")


# Plot model support as stacked plot
model.order <- order(URWs)
model.matrix <- data.frame(URW = URWs[model.order], GRW = GRWs[model.order])
stackpoly(x = 1:100, y = model.matrix, col = cols[c(2, 5)],
          xat = c(1, seq(from = 5, to = 50, by = 5)), xlim = c(1, 50), 
          ylim = c(0, 1), xlab = "tree", ylab = "Akaike weight", stack = TRUE, 
          main = "Model support (sorted by weight)")
abline(h = 0.9, col = "white", lty = 5, lwd = 2)
abline(h = 0.5, col = "white", lty = 5, lwd = 2)
legend("bottomright", legend = c("General (biased) RW", "Unbiased RW"), pch = 22, 
       pt.cex = 4, cex = 1.5, pt.bg = cols[c(5, 2)], col = "white", bty = "n")


# If want to overlay the best-fit model (semi-juxtaposed)
lim <- range(log10(group$AbsStratDistance), na.rm = TRUE)
geoscalePlot2(mids, rep(lim[1], length(mids)), units = c("Epoch", "Period"), 
              tick.scale = "Period", boxes = "Age", cex.age = 0.65, 
              cex.ts = 0.7, cex.pt = 1, age.lim = c(540, 445), data.lim = lim, 
              ts.col = TRUE, label = "distance from seafloor (log10 mm)", 
              timescale = ICS2020, type = "n", abbrev = "Period")
mtext(text = "Tiering", side = 3, cex = 1.25)
mtext(text = group.name, side = 3, line = -1, cex = 1)
for(tr in 1:length(fits)) {
  if (which.max(fits[[tr]]$modelFits$Akaike.wt) == 1) {
    plot(ts[[tr]], add = TRUE, modelFit = fitSimple(ts[[tr]], model = "GRW", 
                                                    pool = FALSE))
  }
  if (which.max(fits[[tr]]$modelFits$Akaike.wt) == 2) {
    plot(ts[[tr]], add = TRUE, modelFit = fitSimple(ts[[tr]], model = "URW", 
                                                    pool = FALSE))
  }
  if (which.max(fits[[tr]]$modelFits$Akaike.wt) == 3) {
    plot(ts[[tr]], add = TRUE, modelFit = fitSimple(ts[[tr]], model = "Stasis", 
                                                    pool = FALSE))
  }
}
lines(mids, tiers[, "mean"], lwd = 3)
beepr::beep() # ~ 2 minutes




## Sample-standardized trend ###################################################

## Rarefaction parameters
ntrees <- length(ranges) # How many trees to sample?
std.g <- 16              # Number of genera per interval (16 is best trade-off)
numrep <- 2000           # How many replicates per time interval? 
#                        #   (Divided equally across time-scaled trees)
per.tree.reps <- numrep / ntrees  # Self-calculates per-tree replicates

if(!abs(per.tree.reps - round(per.tree.reps)) < .Machine$double.eps ^ 0.5)
  stop("The number of per-tree replicates need to be an integer. Choose a 
       different combination of 'numrep' and 'ntree' that will divide more equally.\n")

# For storing summary statistics
nc <- length(mids)
metrics <- data.frame(midpt = as.numeric(mids), genera = std.g, mean.tier = NA, 
                      SE.mean = NA, median.tier = NA, SE.median = NA, 
                      tier.75th = NA, SE.75th = NA, tier.99th = NA, 
                      SE.99th = NA)

# Create list to store 'numrep' replicates across trees
sub.tiers <- vector(mode = "list", length = numrep)
tmp.holder <- matrix(NA, nrow = length(mids), ncol = 4)
colnames(tmp.holder) <- c("mean", "median", "75th", "99th")
for (i in 1:numrep) sub.tiers[[i]] <- tmp.holder
cat("Mean trend uses average across", numrep, "sample-standardized replicates of", std.g, 
    "lineages \n (genus tips and/or ancestral nodes) per time bin, repeated across", 
    per.tree.reps, "replicates \n per each of", ntrees, "time-scaled trees.\n")

set.seed(3124) # So rarefaction can be replicated
increm <- 1    # Counter for incrementing replicates to 'sub.tiers' (first 'pt.rep's from tree #1, etc.)
# Because taking log, only using epifaunal trends
wh.epi <- which(group$AbsStratDistance > 0)
for (tree in 1:ntrees) {
  # Record sample-standardized statistics, across tree-replicates
  for (pt.rep in  1:per.tree.reps) {
    for(t in 1:nc) {
      wh.bin <- as.vector(which(ranges[[tree]][wh.epi, "FAD"] > top[t] &
                                  ranges[[tree]][wh.epi, "LAD"] < base[t]))
      if (std.g > length(wh.bin))
        next
      sampled <- sample2(wh.epi[wh.bin], std.g, replace = FALSE)
      sampled.tiers <- log10(group$AbsStratDistance[sampled])
      means  <- mean(sampled.tiers)
      medians <- median(sampled.tiers)
      x.75ths <- quantile(sampled.tiers, probs = 0.75)
      x.99ths <- quantile(sampled.tiers, probs = 0.99)
      sub.tiers[[increm]][t, ] <- c(means, medians, x.75ths, x.99ths)
    }
    increm <- increm + 1
  }
}
beepr::beep()
# ~ 2 minutes for 2000 replicates using 16 genera in each of 100 bins

# Calculate mean and SD across trees/replicates
tiers.means <- apply(simplify2array(sub.tiers), 1:2, mean, na.rm = TRUE)
tiers.means.sd <- apply(simplify2array(sub.tiers), 1:2, sd, na.rm = TRUE)
metrics[, c(3, 5, 7, 9)] <- tiers.means
metrics[, c(4, 6, 8, 10)] <- tiers.means.sd
print(na.omit(metrics))

# Save trends
# write.csv(metrics, file = "stdG16_tier_tip_continuous.csv", row.names = FALSE)
# metrics <- read.csv(file = "stdG16_tier_tip_continuous.csv")

# Plot continuous, sample-standardized body-size trend, tips only
mean.column <- cbind(c(metrics$midpt, rev(metrics$midpt)),
                     c(metrics$mean.tier - metrics$SE.mean, rev(metrics$mean.tier + metrics$SE.mean)))
mean.column <- na.omit(mean.column)
median.column <- cbind(c(metrics$midpt, rev(metrics$midpt)),
                       c(metrics$median.tier - metrics$SE.median, 
                         rev(metrics$median.tier + metrics$SE.median)))
median.column <- na.omit(median.column)
column.75th <- cbind(c(metrics$midpt, rev(metrics$midpt)),
                       c(metrics$tier.75th - metrics$SE.75th, 
                         rev(metrics$tier.75th + metrics$SE.75th)))
column.75th <- na.omit(column.75th)
column.99th <- cbind(c(metrics$midpt, rev(metrics$midpt)),
                       c(metrics$tier.99th - metrics$SE.99th, 
                         rev(metrics$tier.99th + metrics$SE.99th)))
column.99th <- na.omit(column.99th)
lim <- c(0, 10 ^ max(column.99th[, 2], na.rm = TRUE))
geoscalePlot2(metrics$midpt, 10 ^ metrics$tier.99th, units = c("Epoch", "Period"), 
              tick.scale = "Period", boxes = "Age", cex.age = 0.65, 
              cex.ts = 0.7, cex.pt = 1, age.lim = c(540, 445), 
              data.lim = lim, ts.col = TRUE, 
              label = paste0("distance from seafloor (mm, G = ", std.g, ")"), 
              timescale = ICS2020, type = "n", abbrev = "Period")
mtext(text = "sample-standardized tiering (continuous, tips only)", side = 3, cex = 1)
polygon(column.99th[ ,1], 10 ^ column.99th[ ,2], col = trans.cols[2], lwd = 2, border = NA)
polygon(column.75th[ ,1], 10 ^ column.75th[ ,2], col = trans.cols[5], lwd = 2, border = NA)
polygon(median.column[ ,1], 10 ^ median.column[ ,2], col = trans.cols[7], lwd = 2, border = NA)
polygon(mean.column[ ,1], 10 ^ mean.column[ ,2], col = trans.cols[8], lwd = 2, border = NA)
lines(metrics$midpt, 10 ^ metrics$tier.99th, lty = 2, lwd = 3, col = cols[2])
lines(metrics$midpt, 10 ^ metrics$tier.75th, lty = 4, lwd = 3, col = cols[5])
lines(metrics$midpt, 10 ^ metrics$mean.tier, lty = 1, lwd = 3, col = cols[8])
lines(metrics$midpt, 10 ^ metrics$median.tier, lty = 3, lwd = 3, col = cols[7])
abline(h = 0, lwd = 1)
legend("topleft", legend = c("99%ile", "75%ile", "median (50%ile)", "mean"), 
       box.col = NA, lty = c(2, 4, 3, 1), col = cols[c(2, 5, 7, 8)], lwd = 2, 
       pch = NA, inset = .05)



# Plot trends (overlaying raw sizes)
# Plot tiering trend, with individual genus sizes and quantile trends
lim <- range(group$AbsStratDistance, na.rm = TRUE)
geoscalePlot2(metrics$midpt, 10 ^ metrics$tier.99th, units = c("Epoch", "Period"), 
              tick.scale = "Period", boxes = "Age", cex.age = 0.65, 
              cex.ts = 0.7, cex.pt = 1, age.lim = c(540, 445), 
              data.lim = lim, ts.col = TRUE, 
              label = paste0("distance from seafloor (mm, G = ", std.g, ")"), 
              timescale = ICS2020, type = "n", abbrev = "Period")
mtext(text = "sample-standardized tiering (continuous, tips only)", side = 3, cex = 1)
for(tip in wh.ff) {
  polygon(c(median.ranges[tip, "FAD"], median.ranges[tip, "LAD"]), 
          rep(group$AbsStratDistance[tip], 2), border = "gray35")
}
polygon(column.99th[ ,1], 10 ^ column.99th[ ,2], col = trans.cols[2], lwd = 2, border = NA)
polygon(column.75th[ ,1], 10 ^ column.75th[ ,2], col = trans.cols[5], lwd = 2, border = NA)
polygon(median.column[ ,1], 10 ^ median.column[ ,2], col = trans.cols[7], lwd = 2, border = NA)
lines(metrics$midpt, 10 ^ metrics$tier.99th, lty = 2, lwd = 3, col = cols[2])
lines(metrics$midpt, 10 ^ metrics$tier.75th, lty = 4, lwd = 3, col = cols[5])
lines(metrics$midpt, 10 ^ metrics$median.tier, lty = 3, lwd = 3, col = cols[7])
abline(h = 0, lwd = 1)
legend("topleft", legend = c("99%ile", "75%ile", "median (50%ile)"), 
       box.col = NA, lty = c(2, 4, 3), col = cols[c(2, 5, 7)], lwd = 2, 
       pch = NA, inset = .05)

# Conclusion: Because the tiering distribution is approximately (log)normally
# distributed, sample standardization has a negligible effect.




## Do different classes have different body tiers? --------------
wh.ff <- which(group$FilterFeeder == 1 & group$AbsStratDistance > 0)
cl.table <- table(group$Class[wh.ff])
classes <- cl.table[which(cl.table >= 10L)]
out <- data.frame(Class = names(classes), log.mean = NA, log.sd = NA)
for(c in 1:length(classes)){
  wh.cl <- which(group$Class == out$Class[c])
  Tiers <- na.omit(log10(group$AbsStratDistance[wh.cl]))
  out$log.mean[c] <- round(mean(Tiers), 2)
  out$log.sd[c] <- round(sd(Tiers), 2)
}
sort.o <- order(out$log.mean, decreasing = TRUE)
out[sort.o, ]

# All classes (alphabetical order)
ff <- group[wh.ff, ]
bp.names <- substr(sort(unique(ff$Class)), 1, 1)
boxplot(log10(ff$AbsStratDistance) ~ ff$Class, names = bp.names,
        main = "Tiering height (by class)")

# Most diverse classes
div.cl <- which(ff$Class %in% names(classes))
bp.names <- substr(names(classes), 1, 3)
boxplot(log10(ff$AbsStratDistance[div.cl]) ~ ff$Class[div.cl], names = bp.names, 
        main = "Tiering height (by class)")





## Llandoverian (early Silurian) AbsStratDists (for comparison) ################
Sil <- read.csv(file = "LlandoveryEpifaunalFilterFeeders.csv", header = TRUE)
head(Sil)
summary(Sil$AbsStratDistance)
summary(log10(Sil$AbsStratDistance))
quantile(log10(Sil$AbsStratDistance), probs = c(0.5, .75, .99))
10 ^ quantile(log10(Sil$AbsStratDistance), probs = c(0.5, .75, .99))




## 7 - LIFE HABIT TRENDS #######################################################

## RUN FIRST: Pre-life-habit trend data processing #############################

# Note these trends are using tips plus inferred ancestral nodes (using 'mode'
# data treatment). The following pre-processing is required to run before
# subsequent downstream code.

# Use if wish to select individual echinoderm class
group.name <- NULL; wh.gr <- 1:nrow(taxon.list[[1]])    # Use all echinoderms
# group.name <- "Crinoidea"; wh.gr <- which(taxon.list[[1]][, "class"] == group.name)
# group.name <- "Edrioasteroidea"; wh.gr <- which(taxon.list[[1]][, "class"] == group.name)
# group.name <- "Eocrinoidea"; wh.gr <- which(taxon.list[[1]][, "class"] == group.name)
# group.name <- "Stylophora"; wh.gr <- which(taxon.list[[1]][, "class"] == group.name)
# group.name <- "Rhombifera"; wh.gr <- which(taxon.list[[1]][, "class"] == group.name)
# group.name <- "Homostelea"; wh.gr <- which(taxon.list[[1]][, "class"] == group.name)
# group.name <- "Asteroidea"; wh.gr <- which(taxon.list[[1]][, "class"] == group.name)

# Reassign trait names (because not included on ancestral state matrix)
trait.names <- colnames(x[, 21:60])
for(tr in 1:length(mode.anc)){
  colnames(mode.anc[[tr]]$matrix_1$matrix) <- trait.names
}
head(mode.anc[[57]]$matrix_1$matrix, 2)

# Arbitrarily using 57th tree for indexing for taxa (because constant across
# trees)
group <- mode.anc[[57]]$matrix_1$matrix
# Confirm you've chosen the correct data
group[wh.gr[1:6], 2:4]





## Stratification trends #######################################################

# Pick traits to summarize and build matrixes to store summary statistics
trait.col.names <- which(colnames(group) == "AbsStratification" |
                           colnames(group) == "RelStratification" |
                           colnames(group) == "AbsFoodStratification" |
                           colnames(group) == "RelFoodStratification")
trait.abbr <- c("AbsStrat", "RelStrat", "AbsFoodStrat", "RelFoodStrat")
trait.long <- c("Absolute stratification", "Relative stratification", 
                "Absolute food stratification", "Relative food stratification")
trait.levels <- as.character(0:3)

# Histograms (50th tree only, because no time component in these histograms)
par(mfrow = c(2, 2), mar = c(3, 4, 1.5, .2))
for(trait in 1:length(trait.col.names)){
  hist(as.numeric(mode.anc[[57]]$matrix_1$matrix[, trait.col.names[trait]]),
       main = trait.long[trait], border = "white", col = "darkgray")
}
par(op)



# Confirm states 0, 1, 2, and 3 occur in all trees
lcn <- length(trait.col.names)
for (tr in 1:ntrees) {
  for (trait in 1:lcn) {
    if (!all(trait.levels %in% 
        levels(factor(mode.anc[[tr]]$matrix_1$matrix[, trait.col.names[trait]])))) {
      cat("tree = ", tr, ", ", trait.long[trait], "lacks a level\n")
    }
  }
}
# If nothing printed, means states 0, 1, 2, and 3 are in all trees for each
# trait


# Any multistate ancestral state inferences? Yes, frequently
multis <- c("0/1", "1/2", "2/3")
for(tr in 1:ntrees) {
  for(trait in 1:lcn) {
    if (any(mode.anc[[tr]]$matrix_1$matrix[, trait.col.names[trait]] %in% multis)) {
      cat(tr)
      print(table(mode.anc[[tr]]$matrix_1$matrix[, trait.col.names[trait]]))
    }
  }
}





## Summarize trend across trees (including inferred ancestors)

traits <- matrix(NA, nrow = length(mids), ncol = (lcn * 2))
rownames(traits) <- if (nrow(traits) == nrow(ages))
  ages$interval_name else rev(seq.int(mids))
colnames(traits) <-
  paste0(rep(paste0(trait.abbr, "."), 2), c("mean", "median"))

pt.traits <- vector(mode = "list", length = ntrees)
for (tr in 1:ntrees) pt.traits[[tr]] <- traits
# Work with tmp version because will override values
tmp <- mode.anc
for (tr in 1:ntrees) {
  for(trait in 1:lcn) {
    # If any multistate sizes (e.g., 0/1) replace with their average
    wh.0.1 <- which(tmp[[tr]]$matrix_1$matrix[, trait.col.names[trait]] == "0/1")
    wh.1.2 <- which(tmp[[tr]]$matrix_1$matrix[, trait.col.names[trait]] == "1/2")
    wh.2.3 <- which(tmp[[tr]]$matrix_1$matrix[, trait.col.names[trait]] == "2/3")
    tmp[[tr]]$matrix_1$matrix[, trait.col.names[trait]][wh.0.1] <- 0.5
    tmp[[tr]]$matrix_1$matrix[, trait.col.names[trait]][wh.1.2] <- 1.5
    tmp[[tr]]$matrix_1$matrix[, trait.col.names[trait]][wh.2.3] <- 2.5
  }
  for (t in 1:length(mids)) {
    wh.time <- which(ranges[[tr]][wh.gr, "FAD"] > top[t] &
                       ranges[[tr]][wh.gr, "LAD"] < base[t])
    if (length(wh.time) == 0L)
      next
    for(trait in 1:lcn) {
      wh.traits <- 
        as.numeric(tmp[[tr]]$matrix_1$matrix[wh.time, trait.col.names[trait]])
      pt.traits[[tr]][t, trait] <- mean(wh.traits, na.rm = TRUE)
      pt.traits[[tr]][t, (trait + lcn)] <- 
        median(wh.traits, na.rm = TRUE)
    }
  }
}
beepr::beep() # ~ 10 seconds

# Summarize statistics across trees
traits <- apply(simplify2array(pt.traits), 1:2, mean, na.rm = TRUE)
traits.SE <- apply(simplify2array(pt.traits), 1:2, sd, na.rm = TRUE)

## Plot trends (+/- SE)
par(op)
lim <- c(0, 3)
lcn <- length(trait.col.names)
for(trait in 1:lcn) {
  geoscalePlot2(mids, rep(lim[1], length(mids)), units = c("Epoch", "Period"), 
                tick.scale = "Period", boxes = "Age", cex.age = 0.65, 
                cex.ts = 0.7, cex.pt = 1, age.lim = c(540, 445), data.lim = lim, 
                ts.col = TRUE, label = "Tier", 
                timescale = ICS2020, type = "n", abbrev = "Period")
  mtext(text = trait.long[trait], side = 3, cex = 1.25)
  mtext(text = group.name, side = 3, line = -1, cex = 1)
  mean.column <- cbind(c(mids, rev(mids)), 
                       c(traits[, trait] - traits.SE[, trait], 
                         rev(traits[, trait] + traits.SE[, trait])))
  mean.column <- na.omit(mean.column)
  median.column <- cbind(c(mids, rev(mids)), 
                       c(traits[, c(trait + lcn)] - traits.SE[, c(trait + lcn)], 
                         rev(traits[, c(trait + lcn)] + traits.SE[, c(trait + lcn)])))
  median.column <- na.omit(median.column)
  polygon(median.column[ ,1], median.column[ ,2], col = trans.cols[7], lwd = 2, border = NA)
  polygon(mean.column[ ,1], mean.column[ ,2], col = trans.cols[2], lwd = 2, border = NA)
  lines(mids, traits[, trait], col = cols[2], lwd = 3)
  lines(mids, traits[, (trait + lcn)], col = cols[7], lwd = 3, lty = 2)
  legend("topleft", legend = c("mean", "median"), box.col = NA, lty = c(1, 2), 
         col = cols[c(2,7)], lwd = 2, pch = NA, inset = .05)
}





## Trend through time, as stacked plot

## Replace quantitative codings with factors

# Note (from above) that all trees have all four factor levels (simplifying
# table() below [but essentially irrelevant because converted to factors with
# pre-set levels, which table will default to])

# Note several taxa have two-state options (e.g., size "1/2") and will be
# dropped when converted to factor levels (but added back in secondarily)

# Because the trait matrix is a matrix (which can not store factors), store
# trait columns separately
lcn <- length(trait.col.names)
trait.values <- vector(mode = "list", length = ntrees)
for (tr in 1:ntrees) {
  trait.values[[tr]] <-
    as.data.frame(matrix(NA, nrow = length(wh.gr), ncol = lcn))
  }
for (tr in 1:ntrees) {
  for(trait in 1:lcn) {
  trait.values[[tr]][, trait] <- 
    factor(mode.anc[[tr]]$matrix_1$matrix[, trait.col.names[trait]], 
           levels = trait.levels, ordered = TRUE)
  }
}

## Calculate average number of occurrences (across trees and across time)

# Matrix to store average number (+SE) of occurrences through time
trait.occs <- vector(mode = "list", length = lcn * 2)
for (trait in 1:lcn) {
  trait.occs[[trait]] <- matrix(data = NA, nrow = length(mids), ncol = lcn)
  rownames(trait.occs[[trait]]) <- if (nrow(trait.occs[[trait]]) == nrow(ages))
    ages$interval_name else rev(seq.int(mids))
  trait.occs[[trait + lcn]] <- trait.occs[[trait]]
  colnames(trait.occs[[trait]]) <- trait.levels
  colnames(trait.occs[[trait + lcn]]) <- paste0(trait.levels, ".SE")
}

# Now loop through trees, one trait at a time
for (trait in 1:lcn) {
  cat("Started trait", trait, "of", lcn, "\n")
  # For storing trait counts through time, per tree
  pt.traits <- vector(mode = "list", length = ntrees)
  for (tr in 1:ntrees) pt.traits[[tr]] <- trait.occs[[1]]
  for (tr in 1:ntrees) {
    for (t in 1:length(mids)) {
      wh.time <- which(ranges[[tr]][wh.gr, "FAD"] > top[t] &
                         ranges[[tr]][wh.gr, "LAD"] < base[t])
      if (length(wh.time) == 0L)
        next
      pt.traits[[tr]][t, ] <- table(trait.values[[tr]][wh.time, trait])
    }
    # Manually add back in skipped multistate ancestors
    which.multi <- which(!trait.values[[tr]][, trait] %in% trait.levels)
    if (length(which.multi) == 0L)
      next
    multi.states <-
      mode.anc[[tr]]$matrix_1$matrix[which.multi, trait.col.names[trait]]
    if (all(is.na(multi.states)))
      next
    for (g in 1:length(which.multi)) {
      wh.state <-
        mode.anc[[tr]]$matrix_1$matrix[which.multi[g], trait.col.names[trait]]
      if (is.na(wh.state))
        next
      wh.time <- which(base > ranges[[tr]][which.multi[g], "LAD"] &
                         top < ranges[[tr]][which.multi[g], "FAD"])
      # Go to next, if pre-Cambrian (and below the oldest 'base' used here)
      if (length(wh.time) == 0L)
        next
      if (wh.state == "0/1")
        pt.traits[[tr]][wh.time, 1:2] <- pt.traits[[tr]][wh.time, 1:2] + 1
      if (wh.state == "1/2")
        pt.traits[[tr]][wh.time, 2:3] <- pt.traits[[tr]][wh.time, 2:3] + 1
      if (wh.state == "2/3")
        pt.traits[[tr]][wh.time, 3:4] <- pt.traits[[tr]][wh.time, 3:4] + 1
    }
  }
  
  # Summarize statistics across trees
  trait.occs[[trait]] <- 
    apply(simplify2array(pt.traits), 1:2, mean, na.rm = TRUE)
  trait.occs[[trait + 4]] <- 
    apply(simplify2array(pt.traits), 1:2, sd, na.rm = TRUE)
}



# Plot as a time-independent pie chart
par(mar = c(0, 0, 1, 0))
for (trait in 1:lcn) {
  pie(apply(trait.occs[[trait]], 2, mean), col = cols, labels = trait.levels, 
      main = trait.long[[trait]], clockwise = TRUE)
}
par(op)

# Graph as stacked plots, through time (average proportions across trees)
par(mar = c(4, 4, 1, 2.5))
for (trait in 1:lcn) {
  stackpoly(x = -mids, y = trait.occs[[trait]], col = cols,
            xlab = "time (Ma)", ylab = "number of lineages", stack = TRUE,
            xat = -pretty(range(mids)))
  mtext(trait.long[[trait]], side = 3, cex = 1.25)
  abline(v = -series.boundaries, col = "gray85", lty = 5)
  abline(v = -pd.boundaries, col = "white", lty = 1, lwd = 2)
  box()
  legend("topleft", legend = paste("stratification bin", rev(trait.levels)), 
         pch = 15, col = cols[length(trait.levels):1], ncol = 1, 
         pt.cex = 2, cex = .85, bty = "n")
  mean.SE <- mean(trait.occs[[trait + 4]], na.rm = TRUE)
  SE.start <- 156
  SE.end <- SE.start + mean.SE
  SE.time <- -539.75
  lines(x = c(SE.time, SE.time), y = c(SE.start, SE.end), lwd = 1, lty = 1, col = "gray35")
  points(x = SE.time, y = SE.start, pch = "-", cex = .75, col = "gray35")
  points(x = SE.time, y = SE.end, pch = "-", cex = .75, col = "gray35")
}

# Same but proportionally standardized:
par(mar = c(4, 4, 1, 2.5))
for (trait in 1:lcn) {
  sums <- apply(trait.occs[[trait]], 1, sum)
  trait.props <- trait.occs[[trait]] / sums
  # Replace missing values with zeros
  wh.missing <- which(is.na(trait.props), arr.ind = TRUE)
  trait.props[wh.missing] <- 0
  stackpoly(x = -mids, y = trait.props, col = cols, 
            xlab = "time (Ma)", ylab = "proportion of lineages", stack = TRUE, 
            xat = -pretty(range(mids)), xlim = c(-541, -443), 
            ylim = c(0,1))
  mtext(trait.long[[trait]], side = 3, cex = 1.25)
  abline(v = -series.boundaries, col = "gray85", lty = 5)
  abline(v = -pd.boundaries, col = "white", lty = 1, lwd = 2)
  box()
  legend("topleft", legend = paste("stratification bin", rev(trait.levels)), 
         pch = 22, col = "white", pt.bg = cols[length(trait.levels):1], 
         ncol = 1, pt.cex = 2, cex = .85, bty = "n", text.col = "white", 
         inset = c(0.025, 0.13))
  }
par(op)






## Mobility trends #############################################################

# Pick traits to summarize and build matrixes to store summary statistics
trait.col.names <- which(colnames(group) == "Mobility")
trait.abbr <- c("Mobility")
trait.long <- c("Mobility")

# Any multistate ancestral state inferences? Rarely
for(tr in 1:ntrees) {
  cat(tr)
  print(table(mode.anc[[tr]]$matrix_1$matrix[, trait.col.names]))
}

## Summarize trend across trees (different FADs/LADs, although invariant traits)
lcn <- length(trait.col.names)
traits <- matrix(NA, nrow = length(mids), ncol = (lcn * 2))
rownames(traits) <- if (nrow(traits) == nrow(ages))
  ages$interval_name else rev(seq.int(mids))
colnames(traits) <-
  paste0(rep(paste0(trait.abbr, "."), 2), c("mean", "median"))

pt.traits <- vector(mode = "list", length = ntrees)
for (tr in 1:ntrees) pt.traits[[tr]] <- traits
for (tr in 1:ntrees) {
  for (t in 1:length(mids)) {
    wh.time <- which(ranges[[tr]][wh.gr, "FAD"] > top[t] &
                       ranges[[tr]][wh.gr, "LAD"] < base[t])
    if (length(wh.time) == 0L)
      next
    for(trait in 1:lcn) {
      wh.traits <- as.numeric(mode.anc[[tr]]$matrix_1$matrix[wh.time, trait.col.names[trait]])
      pt.traits[[tr]][t, trait] <- mean(wh.traits, na.rm = TRUE)
      pt.traits[[tr]][t, (trait + lcn)] <- median(wh.traits, na.rm = TRUE)
    }
  }
}
beepr::beep()

# Summarize statistics across trees
traits <- apply(simplify2array(pt.traits), 1:2, mean, na.rm = TRUE)
traits.SE <- apply(simplify2array(pt.traits), 1:2, sd, na.rm = TRUE)

## Plot trends (+/- SE)
par(op)
lim <- c(0, 3)
for(trait in 1:lcn) {
  geoscalePlot2(mids, rep(lim[1], length(mids)), units = c("Epoch", "Period"), 
                tick.scale = "Period", boxes = "Age", cex.age = 0.65, 
                cex.ts = 0.7, cex.pt = 1, age.lim = c(540, 445), data.lim = lim, 
                ts.col = TRUE, label = "Degree of mobility", 
                timescale = ICS2020, type = "n", abbrev = "Period")
  mtext(text = trait.long[trait], side = 3, cex = 1.25)
  mtext(text = group.name, side = 3, line = -1, cex = 1)
  mean.column <- cbind(c(mids, rev(mids)), 
                       c(traits[, trait] - traits.SE[, trait], 
                         rev(traits[, trait] + traits.SE[, trait])))
  mean.column <- na.omit(mean.column)
  median.column <- cbind(c(mids, rev(mids)), 
                         c(traits[, c(trait + lcn)] - traits.SE[, c(trait + lcn)], 
                           rev(traits[, c(trait + lcn)] + traits.SE[, c(trait + lcn)])))
  median.column <- na.omit(median.column)
  polygon(median.column[ ,1], median.column[ ,2], col = trans.cols[7], lwd = 2, border = NA)
  polygon(mean.column[ ,1], mean.column[ ,2], col = trans.cols[2], lwd = 2, border = NA)
  lines(mids, traits[, trait], col = cols[2], lwd = 3)
  lines(mids, traits[, (trait + lcn)], col = cols[7], lwd = 3, lty = 2)
  legend("topleft", legend = c("mean", "median"), box.col = NA, lty = c(1, 2), 
         col = cols[c(2,7)], lwd = 2, pch = NA, inset = .05)
}
par(op)



## Trend through time, as stacked plot

## Replace quantitative codings with factors
# Note no passively mobile in Cambrian/Ordovician echinoderms
table(group[, "Mobility"])

# And all trees have all four factor levels (simplifying table() below, but
# essentially irrelevant because converted to factors with pre-set levels, which
# table will default to)

# Note following taxa have two-state options and will be dropped when converted
# to factor levels
table(mode.anc[[5]]$matrix_1$matrix[, "Mobility"])  # Anc 391 has state 1/2
table(mode.anc[[22]]$matrix_1$matrix[, "Mobility"]) # Anc 392 & 393 have state 1/2
table(mode.anc[[26]]$matrix_1$matrix[, "Mobility"]) # Anc 643 has 1/2
table(mode.anc[[36]]$matrix_1$matrix[, "Mobility"]) # Anc 389 has 0/1
table(mode.anc[[66]]$matrix_1$matrix[, "Mobility"]) # Anc 591 has 1/2
table(mode.anc[[74]]$matrix_1$matrix[, "Mobility"]) # Anc 683 & 690 have 1/2

trait.levels <- c("sedentary", "facultatively mobile", 
                "intermittently mobile", "habitually mobile")
# Because the trait matrix is a matrix (which can not store factors), store
# trait column separately
trait.values <- vector(mode = "list", length = ntrees)
for (tr in 1:ntrees) {
  trait.values[[tr]] <- factor(mode.anc[[tr]]$matrix_1$matrix[, trait.col.names], levels = c(0, 1, 2, 3),
           ordered = TRUE)
}

# Calculate average number of occurrences (across trees and across time)
trait.occs <- matrix(data = NA, nrow = length(mids), 
                     ncol = length(trait.levels))
rownames(trait.occs) <- if (nrow(trait.occs) == nrow(ages))
  ages$interval_name else rev(seq.int(mids))
colnames(trait.occs) <- trait.levels
pt.traits <- vector(mode = "list", length = ntrees)
for (tr in 1:ntrees) pt.traits[[tr]] <- trait.occs
for (tr in 1:ntrees) {
  for (t in 1:length(mids)) {
    wh.time <- which(ranges[[tr]][wh.gr, "FAD"] > top[t] &
                       ranges[[tr]][wh.gr, "LAD"] < base[t])
    if (length(wh.time) == 0L)
      next
    pt.traits[[tr]][t, ] <- table(trait.values[[tr]][wh.time])
  }
}

# Manually add back in skipped multistate ancestors
wh.time1 <- 
  which(base > ranges[[1]]["390", "LAD"] & top < ranges[[1]]["390", "FAD"])
wh.time6 <- 
  which(base > ranges[[6]]["389", "LAD"] & top < ranges[[6]]["389", "FAD"])
wh.time40.389 <- 
  which(base > ranges[[40]]["389", "LAD"] & top < ranges[[40]]["389", "FAD"])
wh.time40.390 <- 
  which(base > ranges[[40]]["390", "LAD"] & top < ranges[[40]]["390", "FAD"])
pt.traits[[1]][wh.time1, 1:2] <- pt.traits[[1]][wh.time1, 1:2] + 1
pt.traits[[6]][wh.time6, 1:2] <- pt.traits[[6]][wh.time6, 1:2] + 1
pt.traits[[40]][wh.time40.389, 1:2] <- pt.traits[[40]][wh.time40.389, 2:3] + 1
pt.traits[[40]][wh.time40.390, 1:2] <- pt.traits[[40]][wh.time40.390, 2:3] + 1

# Save for future use
# mobility.pt.traits <- pt.traits; save(mobility.pt.traits, file = "mobility.pt.traits")

# Summarize statistics across trees
trait.occs <- apply(simplify2array(pt.traits), 1:2, mean, na.rm = TRUE)
trait.occs.SE <- apply(simplify2array(pt.traits), 1:2, sd, na.rm = TRUE)
mean.SE <- mean(trait.occs.SE)

# Plot as a time-independent pie chart
par(mar = c(0, 0, 1, 0))
pie(apply(trait.occs, 2, mean), col = cols, labels = trait.levels, 
    main = "Mobility", clockwise = TRUE, init.angle = 40, radius = 0.74)
par(op)

# Graph as stacked plots, through time (average proportions across trees)
par(mar = c(4, 4, 1, 2.5))
stackpoly(x = -mids, y = trait.occs, col = cols, 
          xlab = "time (Ma)", ylab = "number of lineages", stack = TRUE, 
          xat = -pretty(range(mids)))
mtext(trait.long, side = 3, cex = 1.25)
abline(v = -series.boundaries, col = "gray85", lty = 5)
abline(v = -pd.boundaries, col = "white", lty = 1, lwd = 2)
box()
# Add in passively mobile (for completeness)
legend("topleft", legend = c("passively mobile", rev(trait.levels)), 
       pch = 15, col = cols[(length(trait.levels) + 1):1], ncol = 1, 
       pt.cex = 2, cex = .85, bty = "n")
SE.start <- 182
SE.end <- SE.start + mean.SE
SE.time <- -538.5
lines(x = c(SE.time, SE.time), y = c(SE.start, SE.end), lwd = 1, lty = 1, col = "gray35")
points(x = SE.time, y = SE.start, pch = "-", cex = .75, col = "gray35")
points(x = SE.time, y = SE.end, pch = "-", cex = .75, col = "gray35")

# Same but proportionally standardized:
sums <- apply(trait.occs, 1, sum)
trait.props <- trait.occs / sums
# Replace missing values with zeros
wh.missing <- which(is.na(trait.props), arr.ind = TRUE)
trait.props[wh.missing] <- 0
stackpoly(x = -mids, y = trait.props, col = cols, 
          xlab = "time (Ma)", ylab = "proportion of lineages", stack = TRUE, 
          xat = -pretty(range(mids)), xlim = c(-541, -443), 
          ylim = c(0,1))
mtext(trait.long, side = 3, cex = 1.25)
abline(v = -series.boundaries, col = "gray85", lty = 5)
abline(v = -pd.boundaries, col = "white", lty = 1, lwd = 2)
box()
legend("bottomleft", legend = c("passively mobile", rev(trait.levels)), 
       pch = 22, col = "white", pt.bg = cols[(length(trait.levels) + 1):1], 
       ncol = 1, pt.cex = 2, cex = .85, bty = "n", text.col = "white", inset = c(0.025, 0.025))
par(op)


# Summarize across periods (note that interval 43 spans the C/O boundary so is
# in both)
Ord <- which(base > 443.4 & top < 485.4)
Cm <- which(base > 485.4)

round(apply(trait.props, 2, mean), 3)
round(apply(trait.props, 2, sd), 3)

round(apply(trait.props[Ord, ], 2, mean), 3)
round(apply(trait.props[Ord, ], 2, sd), 3)

round(apply(trait.props[Cm, ], 2, mean), 3)
round(apply(trait.props[Cm, ], 2, sd), 3)



# Same, but using raw PBDB (non-time-scaled) ranges (for tip taxa only).
if(length(mids) > 18L)
  stop("Rebuild 'mids' using discrete bins\n")
trait.levels <- c("sedentary", "facultatively mobile", 
                  "intermittently mobile", "habitually mobile")
# Because the trait matrix is a matrix (which can not store factors), store
# trait column separately
trait.values <- factor(x$Mobility, levels = c(0, 1, 2, 3), ordered = TRUE)
trait.occs <- matrix(data = NA, nrow = length(mids), 
                     ncol = length(levels(trait.values)))
rownames(trait.occs) <- if (nrow(trait.occs) == nrow(ages))
  ages$interval_name else rev(seq.int(mids))
colnames(trait.occs) <- levels(trait.values)
pt.traits <- trait.occs
for (t in 1:length(mids)) {
  wh.time <- which(PBDB.ranges[1:366, "max_ma"] > top[t] &
                     PBDB.ranges[1:366, "min_ma"] < base[t])
  if (length(wh.time) == 0L)
    next
  pt.traits[t, ] <- table(trait.values[wh.time])
}
colnames(pt.traits) <- c("sedentary", "facultatively mobile",
                         "intermittently mobile", "habitually mobile")
# Save for future use
# PBDB.mobility.pt.traits <- pt.traits; save(PBDB.mobility.pt.traits, file = "PBDB.mobility.pt.traits")




## Trends in binary life habits ################################################

# Pick traits to summarize and build matrixes to store summary statistics
trait.col.names <- c(9:10, 12:13, 15:28, 31:32, 34, 36, 38:40)
trait.long <- colnames(group)[trait.col.names]
trait.abbr <- trait.long
trait.levels <- c(0, 1)
trait.long
# Confirm 25 traits: Biotic  <------>  BulkFeeder excluding following:
# FilterDensity (because multistate) and these that do not vary: Sexual,
# Asexual, Fluidic, Insubstantial, AttachmentFeeder, Autotroph, Herbivore, and
# Incorporeal


# Confirm states 0 & 1 occur in all traits, across all trees
lcn <- length(trait.col.names)
for (tr in 1:ntrees) {
  for (trait in 1:lcn) {
    if (!all(trait.levels %in% 
             levels(factor(mode.anc[[tr]]$matrix_1$matrix[, trait.col.names[trait]])))) {
      cat("tree = ", tr, ", ", trait.long[trait], "lacks a level\n")
    }
  }
}
# If nothing printed, means states 0 and 1 occur in all trees for each trait


# Any multistate ancestral state inferences? Yes, for 1-2 ancestral nodes per tree
multis <- c("0/1")
for(tr in 1:ntrees) {
  for(trait in 1:lcn) {
    if (any(mode.anc[[tr]]$matrix_1$matrix[, trait.col.names[trait]] %in% multis)) {
      cat(tr)
      print(table(mode.anc[[tr]]$matrix_1$matrix[, trait.col.names[trait]]))
    }
  }
}
# If anything printed, need to adjust for in downstream analyses



## Summarize trend across trees (including inferred ancestors)
traits <- matrix(NA, nrow = length(mids), ncol = (lcn * 2))
rownames(traits) <- if (nrow(traits) == nrow(ages))
  ages$interval_name else rev(seq.int(mids))
colnames(traits) <-
  paste0(rep(paste0(trait.abbr, "."), 2), 
         c(rep("mean", lcn), rep("median", lcn)))

pt.traits <- vector(mode = "list", length = ntrees)
for (tr in 1:ntrees) pt.traits[[tr]] <- traits
# Work with tmp version because will override values
tmp <- mode.anc
for (tr in 1:ntrees) {
  for(trait in 1:lcn) {
    # If any multistate sizes (e.g., 0/1) replace with their average
    wh.0.1 <- which(tmp[[tr]]$matrix_1$matrix[, trait.col.names[trait]] == "0/1")
    tmp[[tr]]$matrix_1$matrix[, trait.col.names[trait]][wh.0.1] <- 0.5
  }
  for (t in 1:length(mids)) {
    wh.time <- which(ranges[[tr]][wh.gr, "FAD"] > top[t] &
                       ranges[[tr]][wh.gr, "LAD"] < base[t])
    if (length(wh.time) == 0L)
      next
    for(trait in 1:lcn) {
      wh.traits <- 
        as.numeric(tmp[[tr]]$matrix_1$matrix[wh.time, trait.col.names[trait]])
      pt.traits[[tr]][t, trait] <- mean(wh.traits, na.rm = TRUE)
      pt.traits[[tr]][t, (trait + lcn)] <- 
        median(wh.traits, na.rm = TRUE)
    }
  }
}
beepr::beep() # ~ 25 sec.

# Summarize statistics across trees
traits <- apply(simplify2array(pt.traits), 1:2, mean, na.rm = TRUE)
traits.SE <- apply(simplify2array(pt.traits), 1:2, sd, na.rm = TRUE)

# Save for later use
# save(traits, file = "traits")
# save(traits.SE, file = "traits.SE")

## Plot trends (+/- SE)
par(op)
lim <- c(0, 1)
for(trait in 1:lcn) {
  geoscalePlot2(mids, rep(lim[1], length(mids)), units = c("Epoch", "Period"), 
                tick.scale = "Period", boxes = "Age", cex.age = 0.65, 
                cex.ts = 0.7, cex.pt = 1, age.lim = c(540, 445), data.lim = lim, 
                ts.col = TRUE, label = "Average state", 
                timescale = ICS2020, type = "n", abbrev = "Period")
  mtext(text = trait.long[trait], side = 3, cex = 1.25)
  mtext(text = group.name, side = 3, line = -1, cex = 1)
  mean.column <- cbind(c(mids, rev(mids)), 
                       c(traits[, trait] - traits.SE[, trait], 
                         rev(traits[, trait] + traits.SE[, trait])))
  mean.column <- na.omit(mean.column)
  median.column <- cbind(c(mids, rev(mids)), 
                         c(traits[, c(trait + lcn)] - traits.SE[, c(trait + lcn)], 
                           rev(traits[, c(trait + lcn)] + traits.SE[, c(trait + lcn)])))
  median.column <- na.omit(median.column)
  polygon(median.column[ ,1], median.column[ ,2], col = trans.cols[7], lwd = 2, border = NA)
  polygon(mean.column[ ,1], mean.column[ ,2], col = trans.cols[2], lwd = 2, border = NA)
  lines(mids, traits[, trait], col = cols[2], lwd = 3)
  lines(mids, traits[, (trait + lcn)], col = cols[7], lwd = 3, lty = 2)
  legend("topleft", legend = c("mean", "median"), box.col = NA, lty = c(1, 2), 
         col = cols[c(2,7)], lwd = 2, pch = NA, inset = .05)
}


# Summarize across periods (note that interval 43 spans the C/O boundary so is
# in both)
Ord <- which(base > 443.4 & top < 485.4)
Cm <- which(base > 485.4)

# Choose trait
trait <- 13
trait.long[trait]

round(mean(traits[, trait], na.rm = TRUE), 3)
round(sd(traits[, trait], na.rm = TRUE), 3)

round(mean(traits[Ord, trait], na.rm = TRUE), 3)
round(sd(traits[Ord, trait], na.rm = TRUE), 3)

round(mean(traits[Cm, trait], na.rm = TRUE), 3)
round(sd(traits[Cm, trait], na.rm = TRUE), 3)






## 8 - STACKED PLOTS FOR LIFE-HABIT COMBINATIONS ###############################

## A. Habitats #################################################################

# Plot as stacked plots trends in infaunal, epifaunal, semi-infaunal, etc.
# habitats. Note no pelagics b/c none until Mesozoic roveacrinids, but still
# tallying. Shallow = epifaunal on lithic sustrates and less than 10 mm (tier
# states 0 and 1); intermediate = same 1-10 cm (tier state 2), and tall = same
# but greater than 10 cm (tier states 3 & 4, although no 4s until Silurian.
# Based on earlier analyses, these tiers conveniently break the echinoderms into
# 3 approximately equally frequent samples, whether described using discrete
# states (for all echinoderms) or using continuous AbsStratDistance (for
# filter-feeding ones alone).

# Because we only want to identify confirmed members of a habitat, we are
# ignoring polymorphic (multistate) ancestral trait inferences. In other words,
# a taxon coded as "0/1" for biotic is not treated as confidently biotic, so is
# not included in the count of biotic lineages. Given the small number of such
# ancestral reconstructions, this ought to have minimal impact (especially
# because we are averaging across 100 trees, where some of the same taxa will
# have more confirmed inferences in other trees.)

# Summarizing trend across trees (including inferred ancestors)

trait.abbr <- c("inf", "semi", "sh.epi", "int.epi", "tall.epi", "epibio", "pel")
trait.long <- c("infaunal", "semi-infaunal", "short epifaunal",
                "intermediate epifaunal", "tall epifaunal", "epibiotic", 
                "pelagic")
lcn <- length(trait.abbr)
trait.occs <- matrix(data = NA, nrow = length(mids), ncol = lcn)
rownames(trait.occs) <- if (nrow(trait.occs) == nrow(ages))
  ages$interval_name else rev(seq.int(mids))
colnames(trait.occs) <- trait.abbr

# For storing trait counts through time, per tree
pt.traits <- vector(mode = "list", length = ntrees)
for (tr in 1:ntrees) pt.traits[[tr]] <- trait.occs
for (tr in 1:ntrees) {
  for (t in 1:length(mids)) {
    wh.time <- which(ranges[[tr]][wh.gr, "FAD"] > top[t] &
                       ranges[[tr]][wh.gr, "LAD"] < base[t])
    if (length(wh.time) == 0L)
      next
    tmp <- mode.anc[[tr]]$matrix_1$matrix
    pt.traits[[tr]][t, "inf"] <- length(which(tmp[wh.time, "AbovePrimary"] == 0 & 
                                              tmp[wh.time, "WithinPrimary"] == 1 &
                                              tmp[wh.time, "Lithic"] == 1))
    pt.traits[[tr]][t, "semi"] <- length(which(tmp[wh.time, "AbovePrimary"] == 1 &
                                               tmp[wh.time, "WithinPrimary"] == 1 &
                                               tmp[wh.time, "Lithic"]== 1))
    pt.traits[[tr]][t, "sh.epi"] <- length(which(tmp[wh.time, "AbovePrimary"] == 1 & 
                                              tmp[wh.time, "WithinPrimary"] == 0 & 
                                              tmp[wh.time, "Lithic"] == 1 & 
                                              tmp[wh.time, "AbsStratification"] <= 1))
    pt.traits[[tr]][t, "int.epi"] <- length(which(tmp[wh.time, "AbovePrimary"] == 1 &
                                                  tmp[wh.time, "WithinPrimary"] == 0 &
                                                  tmp[wh.time, "Lithic"] == 1 &
                                                  tmp[wh.time, "AbsStratification"] == 2))
    pt.traits[[tr]][t, "tall.epi"] <- length(which(tmp[wh.time, "AbovePrimary"] == 1 &
                                                   tmp[wh.time, "WithinPrimary"] == 0 &
                                                   tmp[wh.time, "Lithic"] == 1 & 
                                                   tmp[wh.time, "AbsStratification"] > 2))
    pt.traits[[tr]][t, "epibio"] <- length(which(tmp[wh.time, "AbovePrimary"] == 1 & 
                                                 tmp[wh.time, "AboveImmediate"] == 1 & 
                                                 tmp[wh.time, "Biotic"] == 1))
    pt.traits[[tr]][t, "pel"] <- length(which(tmp[wh.time, "AbovePrimary"] == 1 &
                                              tmp[wh.time, "WithinPrimary"] == 0 &
                                              tmp[wh.time, "Fluidic"] == 1))
  }
}
# Save for future use
# habitat.pt.traits <- pt.traits; save(habitat.pt.traits, file = "habitat.pt.traits")

# Summarize statistics across trees
trait.occs <- 
  apply(simplify2array(pt.traits), 1:2, mean, na.rm = TRUE)
trait.occs.SE <- 
  apply(simplify2array(pt.traits), 1:2, sd, na.rm = TRUE)
mean.SE <- mean(trait.occs.SE, na.rm = TRUE)

# Plot as a time-independent pie chart
par(mar = c(0, 0, 1, 0))
pie(apply(trait.occs[, 1:6], 2, mean), col = cols, labels = trait.long[1:6], 
    main = "Habitat", clockwise = TRUE, init.angle = 88, radius = 0.71)
par(op)

# Graph as stacked plots, through time (average proportions across trees)
par(mar = c(4, 4, 1, 2.5))
stackpoly(x = -mids, y = trait.occs, col = cols, 
          xlab = "time (Ma)", ylab = "number of lineages", stack = TRUE, 
          xat = -pretty(range(mids)))
mtext("Habitat", side = 3, cex = 1.25)
abline(v = -series.boundaries, col = "gray85", lty = 5)
abline(v = -pd.boundaries, col = "white", lty = 1, lwd = 2)
box()
legend("topleft", legend = rev(trait.long), pch = 15, 
       col = cols[length(trait.long):1], ncol = 1, 
       pt.cex = 2, cex = .85, bty = "n")
SE.start <- 190
SE.end <- SE.start + mean.SE
SE.time <- -538.25
lines(x = c(SE.time, SE.time), y = c(SE.start, SE.end), lwd = 1, lty = 1, col = "gray35")
points(x = SE.time, y = SE.start, pch = "-", cex = .75, col = "gray35")
points(x = SE.time, y = SE.end, pch = "-", cex = .75, col = "gray35")

# Same but proportionally standardized:
sums <- apply(trait.occs, 1, sum)
trait.props <- trait.occs / sums
# Replace missing values with zeros
wh.missing <- which(is.na(trait.props), arr.ind = TRUE)
trait.props[wh.missing] <- 0
stackpoly(x = -mids, y = trait.props, col = cols, 
          xlab = "time (Ma)", ylab = "proportion of lineages", stack = TRUE, 
          xat = -pretty(range(mids)), xlim = c(-541, -443), 
          ylim = c(0,1))
mtext("Habitat", side = 3, cex = 1.25)
abline(v = -series.boundaries, col = "gray85", lty = 5)
abline(v = -pd.boundaries, col = "white", lty = 1, lwd = 2)
box()
legend("topleft", legend = rev(trait.long), pch = 22, col = "white",
       pt.bg = cols[length(trait.long):1], ncol = 1, pt.cex = 2, cex = .85, 
       bty = "n", text.col = "white", inset = c(0.025, 0.15))
par(op)

# Summarize across periods (note that interval 43 spans the C/O boundary so is
# in both)
Ord <- which(base > 443.4 & top < 485.4)
Cm <- which(base > 485.4)

round(apply(trait.props[Ord, ], 2, mean), 3)
round(apply(trait.props[Ord, ], 2, sd), 3)

round(apply(trait.props[Cm, ], 2, mean), 3)
round(apply(trait.props[Cm, ], 2, sd), 3)



# Same, but using raw PBDB (non-time-scaled) ranges (for tip taxa only)
if(length(mids) > 18L)
  stop("Rebuild 'mids' using discrete bins\n")
trait.abbr <- c("inf", "semi", "sh.epi", "int.epi", "tall.epi", "epibio", "pel")
trait.long <- c("infaunal", "semi-infaunal", "short epifaunal",
                "intermediate epifaunal", "tall epifaunal", "epibiotic", 
                "pelagic")
lcn <- length(trait.abbr)
trait.occs <- matrix(data = NA, nrow = length(mids), ncol = lcn)
rownames(trait.occs) <- if (nrow(trait.occs) == nrow(ages))
  ages$interval_name else rev(seq.int(mids))
colnames(trait.occs) <- trait.abbr

# For storing trait counts through time (because only using tips, no trees
# used)
pt.traits <- trait.occs
for (t in 1:length(mids)) {
  wh.time <- which(PBDB.ranges[1:366, "max_ma"] > top[t] &
                     PBDB.ranges[1:366, "min_ma"] < base[t])
  if (length(wh.time) == 0L)
    next
  pt.traits[t, "inf"] <- length(which(x[wh.time, "AbovePrimary"] == 0 &
                                        x[wh.time, "WithinPrimary"] == 1 &
                                        x[wh.time, "Lithic"] == 1))
  pt.traits[t, "semi"] <- length(which(x[wh.time, "AbovePrimary"] == 1 &
                                         x[wh.time, "WithinPrimary"] == 1 &
                                         x[wh.time, "Lithic"]== 1))
  pt.traits[t, "sh.epi"] <- length(which(x[wh.time, "AbovePrimary"] == 1 &
                                           x[wh.time, "WithinPrimary"] == 0 &
                                           x[wh.time, "Lithic"] == 1 &
                                           x[wh.time, "AbsStratification"] <= 1))
  pt.traits[t, "int.epi"] <- length(which(x[wh.time, "AbovePrimary"] == 1 &
                                            x[wh.time, "WithinPrimary"] == 0 &
                                            x[wh.time, "Lithic"] == 1 &
                                            x[wh.time, "AbsStratification"] == 2))
  pt.traits[t, "tall.epi"] <- length(which(x[wh.time, "AbovePrimary"] == 1 &
                                             x[wh.time, "WithinPrimary"] == 0 &
                                             x[wh.time, "Lithic"] == 1 &
                                             x[wh.time, "AbsStratification"] > 2))
  pt.traits[t, "epibio"] <- length(which(x[wh.time, "AbovePrimary"] == 1 &
                                           x[wh.time, "AboveImmediate"] == 1 &
                                           x[wh.time, "Biotic"] == 1))
  pt.traits[t, "pel"] <- length(which(x[wh.time, "AbovePrimary"] == 1 &
                                        x[wh.time, "WithinPrimary"] == 0 &
                                        x[wh.time, "Fluidic"] == 1))
}
# Save for future use
# PBDB.habitat.pt.traits <- pt.traits; save(PBDB.habitat.pt.traits, file = "PBDB.habitat.pt.traits")




## B. Diet #################################################################

# Plot as stacked plots trends in different foraging habits

# As above (c.f., habitats), ignoring polymorphic (multistate) ancestral trait
# inferences. However, taxa that have multiple foraging habits that are coded as
# both a microbivore and carnivore) will be counted in both counts. However,
# these are relatively small numbers so their impact is minimal. Although no
# autotrophic (sensu strictu) animals exist, including for completeness, and
# also including herbivores (although oldest herbivorous echinoderms don't first
# occur until Cretaceous euechinoids).

# Summarizing trend across trees (including inferred ancestors)
trait.abbr <- c("auto", "herb", "microb", "carn")
trait.long <- c("autotroph", "herbivore", "microbivore", "carnivore")
lcn <- length(trait.abbr)
trait.occs <- matrix(data = NA, nrow = length(mids), ncol = lcn)
rownames(trait.occs) <- if (nrow(trait.occs) == nrow(ages))
  ages$interval_name else rev(seq.int(mids))
colnames(trait.occs) <- trait.abbr

# For storing trait counts through time, per tree
pt.traits <- vector(mode = "list", length = ntrees)
for (tr in 1:ntrees) pt.traits[[tr]] <- trait.occs
for (tr in 1:ntrees) {
  for (t in 1:length(mids)) {
    wh.time <- which(ranges[[tr]][wh.gr, "FAD"] > top[t] &
                       ranges[[tr]][wh.gr, "LAD"] < base[t])
    if (length(wh.time) == 0L)
      next
    tmp <- mode.anc[[tr]]$matrix_1$matrix
    pt.traits[[tr]][t, "auto"] <- length(which(tmp[wh.time, "Autotroph"] == 1))
    pt.traits[[tr]][t, "herb"] <- length(which(tmp[wh.time, "Herbivore"] == 1))
    pt.traits[[tr]][t, "microb"] <- length(which(tmp[wh.time, "Microbivore"] == 1))
    pt.traits[[tr]][t, "carn"] <- length(which(tmp[wh.time, "Carnivore"] == 1))
  }
}

# Save for future use
# diet.pt.traits <- pt.traits; save(diet.pt.traits, file = "diet.pt.traits")

# Summarize statistics across trees
trait.occs <- 
  apply(simplify2array(pt.traits), 1:2, mean, na.rm = TRUE)
trait.occs.SE <- 
  apply(simplify2array(pt.traits), 1:2, sd, na.rm = TRUE)
mean.SE <- mean(trait.occs.SE)

# Plot as a time-independent pie chart
par(mar = c(0, 0, 1, 0))
pie(apply(trait.occs, 2, mean), col = cols[c(8, 2, 5, 7)], labels = trait.long, 
    main = "Diet", clockwise = TRUE, init.angle = 90, radius = 0.9)
par(op)

# Graph as stacked plots, through time (average proportions across trees)
par(mar = c(4, 4, 1, 2.5))
stackpoly(x = -mids, y = trait.occs, col = cols[c(5, 4, 2, 7)], 
          xlab = "time (Ma)", ylab = "number of lineages", stack = TRUE, 
          xat = -pretty(range(mids)))
mtext("Diet", side = 3, cex = 1.25)
abline(v = -series.boundaries, col = "gray85", lty = 5)
abline(v = -pd.boundaries, col = "white", lty = 1, lwd = 2)
box()
legend("topleft", legend = rev(trait.long), pch = 15, 
       col = cols[c(7, 2, 4, 5)], ncol = 1, pt.cex = 2, cex = .85, bty = "n")
SE.start <- 205
SE.end <- SE.start + mean.SE
SE.time <- -538.25
lines(x = c(SE.time, SE.time), y = c(SE.start, SE.end), lwd = 1, lty = 1, col = "gray35")
points(x = SE.time, y = SE.start, pch = "-", cex = .75, col = "gray35")
points(x = SE.time, y = SE.end, pch = "-", cex = .75, col = "gray35")

# Same but proportionally standardized:
sums <- apply(trait.occs, 1, sum)
trait.props <- trait.occs / sums
# Replace missing values with zeros
wh.missing <- which(is.na(trait.props), arr.ind = TRUE)
trait.props[wh.missing] <- 0
stackpoly(x = -mids, y = trait.props, col = cols[c(5, 4, 2, 7)], 
          xlab = "time (Ma)", ylab = "proportion of lineages", stack = TRUE, 
          xat = -pretty(range(mids)), xlim = c(-541, -443), 
          ylim = c(0,1))
mtext("Diet", side = 3, cex = 1.25)
abline(v = -series.boundaries, col = "gray85", lty = 5)
abline(v = -pd.boundaries, col = "white", lty = 1, lwd = 2)
box()
legend("topleft", legend = rev(trait.long), pch = 22, col = "white",
       pt.bg = cols[c(7, 2, 4, 5)], ncol = 1, pt.cex = 2, cex = .85, 
       bty = "n", text.col = "white", inset = c(0.025, 0.025))
par(op)


# Summarize across periods (note that interval 43 spans the C/O boundary so is
# in both)
Ord <- which(base > 443.4 & top < 485.4)
Cm <- which(base > 485.4)

round(apply(trait.props, 2, mean), 3)
round(apply(trait.props, 2, sd), 3)

round(apply(trait.props[Ord, ], 2, mean), 3)
round(apply(trait.props[Ord, ], 2, sd), 3)

round(apply(trait.props[Cm, ], 2, mean), 3)
round(apply(trait.props[Cm, ], 2, sd), 3)



# Same, but using raw PBDB (non-time-scaled) ranges (for tip taxa only)
if(length(mids) > 18L)
  stop("Rebuild 'mids' using discrete bins\n")
trait.abbr <- c("auto", "herb", "microb", "carn")
trait.long <- c("autotroph", "herbivore", "microbivore", "carnivore")
lcn <- length(trait.abbr)
trait.occs <- matrix(data = NA, nrow = length(mids), ncol = lcn)
rownames(trait.occs) <- if (nrow(trait.occs) == nrow(ages))
  ages$interval_name else rev(seq.int(mids))
colnames(trait.occs) <- trait.abbr

# For storing trait counts through time, per tree
pt.traits <- trait.occs
for (t in 1:length(mids)) {
  wh.time <- which(PBDB.ranges[1:366, "max_ma"] > top[t] &
                     PBDB.ranges[1:366, "min_ma"] < base[t])
  if (length(wh.time) == 0L)
    next
  pt.traits[t, "auto"] <- length(which(x[wh.time, "Autotroph"] == 1))
  pt.traits[t, "herb"] <- length(which(x[wh.time, "Herbivore"] == 1))
  pt.traits[t, "microb"] <- length(which(x[wh.time, "Microbivore"] == 1))
  pt.traits[t, "carn"] <- length(which(x[wh.time, "Carnivore"] == 1))
}

# Save for future use
# PBDB.diet.pt.traits <- pt.traits; save(PBDB.diet.pt.traits, file = "PBDB.diet.pt.traits")






## C. Foraging #################################################################

# Plot as stacked plots trends in different foraging habits

# As above (c.f., habitats), ignoring polymorphic (multistate) ancestral trait
# inferences. However, taxa that have multiple foraging habits (i.e., both a
# filterer and an absorptive, or coded as both raptorial and mass-feeding) will
# be counted in both counts. However, these are relatively small numbers so
# their impact is minimal.

# Summarizing trend across trees (including inferred ancestors)
trait.abbr <- c("absorb", "NA.filter", "l.filter", "m.filter", "h.filter", 
                "mass", "attach", "raptor")
trait.long <- c("absorptive", "unknown-density filterer", "low-density filterer",
                "medium-density filterer", "high-density filterer", "mass-feeder",
                "attachment feeder", "raptorial feeder")
lcn <- length(trait.abbr)
trait.occs <- matrix(data = NA, nrow = length(mids), ncol = lcn)
rownames(trait.occs) <- if (nrow(trait.occs) == nrow(ages))
  ages$interval_name else rev(seq.int(mids))
colnames(trait.occs) <- trait.abbr

# For storing trait counts through time, per tree
pt.traits <- vector(mode = "list", length = ntrees)
for (tr in 1:ntrees) pt.traits[[tr]] <- trait.occs
for (tr in 1:ntrees) {
  for (t in 1:length(mids)) {
    wh.time <- which(ranges[[tr]][wh.gr, "FAD"] > top[t] &
                       ranges[[tr]][wh.gr, "LAD"] < base[t])
    if (length(wh.time) == 0L)
      next
    tmp <- mode.anc[[tr]]$matrix_1$matrix
    pt.traits[[tr]][t, "absorb"] <- length(which(tmp[wh.time, "AmbientFeeder"] == 1))
    pt.traits[[tr]][t, "NA.filter"] <- length(which(tmp[wh.time, "FilterFeeder"] == 1 &
                                                    is.na(tmp[wh.time, "FilterDensity"])))
    pt.traits[[tr]][t, "l.filter"] <- length(which(tmp[wh.time, "FilterFeeder"] == 1 & 
                                                   tmp[wh.time, "FilterDensity"] == 0))
    pt.traits[[tr]][t, "m.filter"] <- length(which(tmp[wh.time, "FilterFeeder"] == 1 & 
                                                   tmp[wh.time, "FilterDensity"] == 1))
    pt.traits[[tr]][t, "h.filter"] <- length(which(tmp[wh.time, "FilterFeeder"] == 1 & 
                                                   tmp[wh.time, "FilterDensity"] == 2))
    pt.traits[[tr]][t, "mass"] <- length(which(tmp[wh.time, "MassFeeder"] == 1))
    pt.traits[[tr]][t, "attach"] <- length(which(tmp[wh.time, "AttachmentFeeder"] == 1))
    pt.traits[[tr]][t, "raptor"] <- length(which(tmp[wh.time, "RaptorFeeder"] == 1))
  }
}

# Save for future use
# foraging.pt.traits <- pt.traits; save(foraging.pt.traits, file = "foraging.pt.traits")

# Summarize statistics across trees
trait.occs <- 
  apply(simplify2array(pt.traits), 1:2, mean, na.rm = TRUE)
trait.occs.SE <- 
  apply(simplify2array(pt.traits), 1:2, sd, na.rm = TRUE)
mean.SE <- mean(trait.occs.SE)

# Plot as a time-independent pie chart
par(mar = c(0, 0, 1, 0))
pie(apply(trait.occs, 2, mean), col = cols, labels = trait.long, 
    main = "Foraging habit", clockwise = TRUE, init.angle = 88, radius = 0.71)
par(op)

# Graph as stacked plots, through time (average proportions across trees)
par(mar = c(4, 4, 1, 2.5))
stackpoly(x = -mids, y = trait.occs, col = cols, 
          xlab = "time (Ma)", ylab = "number of lineages", stack = TRUE, 
          xat = -pretty(range(mids)))
mtext("Foraging habit", side = 3, cex = 1.25)
abline(v = -series.boundaries, col = "gray85", lty = 5)
abline(v = -pd.boundaries, col = "white", lty = 1, lwd = 2)
box()
legend("topleft", legend = rev(trait.long), pch = 15, 
       col = cols[length(trait.long):1], ncol = 1, 
       pt.cex = 2, cex = .85, bty = "n")
SE.start <- 171
SE.end <- SE.start + mean.SE
SE.time <- -538.25
lines(x = c(SE.time, SE.time), y = c(SE.start, SE.end), lwd = 1, lty = 1, col = "gray35")
points(x = SE.time, y = SE.start, pch = "-", cex = .75, col = "gray35")
points(x = SE.time, y = SE.end, pch = "-", cex = .75, col = "gray35")

# Same but proportionally standardized:
sums <- apply(trait.occs, 1, sum)
trait.props <- trait.occs / sums
# Replace missing values with zeros
wh.missing <- which(is.na(trait.props), arr.ind = TRUE)
trait.props[wh.missing] <- 0
stackpoly(x = -mids, y = trait.props, col = cols, 
          xlab = "time (Ma)", ylab = "proportion of lineages", stack = TRUE, 
          xat = -pretty(range(mids)), xlim = c(-541, -443), 
          ylim = c(0,1))
mtext("Foraging habit", side = 3, cex = 1.25)
abline(v = -series.boundaries, col = "gray85", lty = 5)
abline(v = -pd.boundaries, col = "white", lty = 1, lwd = 2)
box()
legend("topleft", legend = rev(trait.long), pch = 22, col = "white",
       pt.bg = cols[length(trait.long):1], ncol = 1, pt.cex = 2, cex = .85, 
       bty = "n", text.col = "white", inset = c(0.025, 0.1))
par(op)

# Summarize across periods (note that interval 43 spans the C/O boundary so is
# in both)
Ord <- which(base > 443.4 & top < 485.4)
Cm <- which(base > 485.4)

round(apply(trait.props[Ord, ], 2, mean), 3)
round(apply(trait.props[Ord, ], 2, sd), 3)

round(apply(trait.props[Cm, ], 2, mean), 3)
round(apply(trait.props[Cm, ], 2, sd), 3)



# Same, but using raw PBDB (non-time-scaled) ranges (for tip taxa only)
if(length(mids) > 18L)
  stop("Rebuild 'mids' using discrete bins\n")
trait.abbr <- c("absorb", "NA.filter", "l.filter", "m.filter", "h.filter", 
                "mass", "attach", "raptor")
trait.long <- c("absorptive", "unknown-density filterer", "low-density filterer",
                "medium-density filterer", "high-density filterer", "mass-feeder",
                "attachment feeder", "raptorial feeder")
lcn <- length(trait.abbr)
trait.occs <- matrix(data = NA, nrow = length(mids), ncol = lcn)
rownames(trait.occs) <- if (nrow(trait.occs) == nrow(ages))
  ages$interval_name else rev(seq.int(mids))
colnames(trait.occs) <- trait.abbr

# For storing trait counts through time (because only using tips, no trees
# used)
pt.traits <- trait.occs
for (t in 1:length(mids)) {
  wh.time <- which(PBDB.ranges[1:366, "max_ma"] > top[t] &
                     PBDB.ranges[1:366, "min_ma"] < base[t])
  if (length(wh.time) == 0L)
    next
  pt.traits[t, "absorb"] <- length(which(x[wh.time, "AmbientFeeder"] == 1))
  pt.traits[t, "NA.filter"] <- length(which(x[wh.time, "FilterFeeder"] == 1 &
                                                    is.na(x[wh.time, "FilterDensity"])))
  pt.traits[t, "l.filter"] <- length(which(x[wh.time, "FilterFeeder"] == 1 & 
                                                   x[wh.time, "FilterDensity"] == 0))
  pt.traits[t, "m.filter"] <- length(which(x[wh.time, "FilterFeeder"] == 1 & 
                                                   x[wh.time, "FilterDensity"] == 1))
  pt.traits[t, "h.filter"] <- length(which(x[wh.time, "FilterFeeder"] == 1 & 
                                                   x[wh.time, "FilterDensity"] == 2))
  pt.traits[t, "mass"] <- length(which(x[wh.time, "MassFeeder"] == 1))
  pt.traits[t, "attach"] <- length(which(x[wh.time, "AttachmentFeeder"] == 1))
  pt.traits[t, "raptor"] <- length(which(x[wh.time, "RaptorFeeder"] == 1))
}

# Save for future use
# PBDB.foraging.pt.traits <- pt.traits; save(PBDB.foraging.pt.traits, file = "PBDB.foraging.pt.traits")

  

## MISCELLANEOUS CODE USED TO WRITE MANUSCRIPT TEXT ############################

# When writing manuscript text, the following code helps quickly confirm things
# like taxonomic coverage, mean and actual time-scaled stratigraphic ranges, and
# other facts in the data sources.


# How many orders of magnitude increase from Cambrian to Ordovican?
PBDB.div.discrete <- read.csv(file = "div_PBDB_discrete.csv")
tip.div.discrete <- read.csv(file = "div_tip_discrete.csv")
tip.div.continuous <- read.csv(file = "div_tip_continuous.csv")
all.div.continuous <- read.csv(file = "div_tip&node_continuous.csv")

(Ord.max <- max(PBDB.div.discrete$LH[1:7]))
(Cm.max <- max(PBDB.div.discrete$LH[8:18]))
round(Ord.max / Cm.max, 2) # 4.12

(Ord.max <- max(tip.div.discrete$LH.mean.raw[1:7]))
(Cm.max <- max(tip.div.discrete$LH.mean.raw[8:18]))
round(Ord.max / Cm.max, 2) # 3.66

(Ord.max <- max(tip.div.continuous$LH.mean.raw[1:43]))
(Cm.max <- max(tip.div.continuous$LH.mean.raw[44:100]))
round(Ord.max / Cm.max, 2) # 3.64

(Ord.max <- max(all.div.continuous$LH.mean.raw[1:43]))
(Cm.max <- max(all.div.continuous$LH.mean.raw[44:100]))
round(Ord.max / Cm.max, 2) # 2.7


# If want to specify class
group <- x # Default, all classes of echinoderms
# group.name <- "Crinoidea"; wh.gr <- which(x$Class == group.name); group <- x[wh.gr, ]
# group.name <- "Edrioasteroidea"; wh.gr <- which(x$Class == group.name); group <- x[wh.gr, ]
# group.name <- "Eocrinoidea"; wh.gr <- which(x$Class == group.name); group <- x[wh.gr, ]
# group.name <- "Stylophora"; wh.gr <- which(x$Class == group.name); group <- x[wh.gr, ]


## Specify a tree, because the results can vary among trees
tree <- 57

## Get information on a genus
genus <- "Ophioxenikos"

# Confirm a genus was used in this study
genus %in% rownames(ranges[[tree]])

# Get mean range for a genus
median.ranges[genus, ]

# Get and summarize all FADs for a genus (across 100 time-scaled trees)
sq <- 1:length(ranges)
strat <- unlist(lapply(sq, function(sq) ranges[[sq]][genus, "FAD"]))
# sort(strat)
summary(strat)
hist(strat, 20); abline(v = c(pd.boundaries, series.boundaries))

# Get and summarize all LADs for a genus (across 100 time-scaled trees)
strat <- unlist(lapply(sq, function(sq) ranges[[sq]][genus, "LAD"]))
summary(strat)

## Identify proportion of classes found in different time intervals
load("class.bins")
round(class.bins[, 6:8], 1)



## Sort body volumes (or AbsStratDist) to identify smallest/largest genera
group[order(group$BodyVolume)[1:10], c(3, 10, 20)]    # Smallest 10
group[order(group$BodyVolume, decreasing = TRUE)[1:15], c(3, 10, 20)] # Largest 10

group[order(group$AbsStratDistance)[1:10], c(3, 10, 19, 37:38)]
group[order(group$AbsStratDistance, decreasing = TRUE)[1:15], c(3, 10, 19, 37:38)]

# Subset these to particular periods (or other intervals)
# !!! NOTE NOT RESTRICTED HERE TO FILTER-FEEDERS AND ONLY FOR TIPS!!!
Ord <- which(median.ranges[, "FAD"] > 443.4 & median.ranges[, "LAD"] < 485.4)
# Ord <- which(median.ranges[, "FAD"] > 443.4 & median.ranges[, "LAD"] < 470) # Mid/Late Ordovician
# Ord <- which(median.ranges[, "FAD"] > 477.7 & median.ranges[, "LAD"] < 485.4) # Early Ordovician
group[Ord[order(group$AbsStratDistance[Ord])][1:10], c(3, 10, 19, 37:38)]
group[Ord[order(group$AbsStratDistance[Ord], decreasing = TRUE)][1:10], c(3, 10, 19, 37:38)]

Cm <- which(median.ranges[, "FAD"] > 485.4)
# Cm <- which(median.ranges[, "FAD"] > 509) # Terreneuvian & Series 2
# Cm <- which(median.ranges[, "FAD"] > 521) # 0 Terreneuvian
group[Cm[order(group$AbsStratDistance[Cm])][1:10], c(3, 10, 19, 37:38)]
group[Cm[order(group$AbsStratDistance[Cm], decreasing = TRUE)][1:10], c(3, 10, 19, 37:38)]

# Compare Ordovician to Cambrian 99th%ile.
table(group$AbsStratDistance[Ord] > 100.3) # 94 FALSE , 32 TRUE
32 / (94 + 32)

# Use following subsets to distinguish Cambrian from Ordovician taxa (note only
# uses tips)
Ord <- which(median.ranges[, "FAD"] > 443.4 & median.ranges[, "LAD"] < 485.4)
Cm <- which(median.ranges[, "FAD"] > 485.4)


# Cambrian vs. Ordovician character distributions (tips only)
table(mode.anc[[tree]]$matrix_1$matrix[Cm, "AbsStratification"])
length(na.omit(mode.anc[[tree]]$matrix_1$matrix[Cm, "AbsStratification"]))
table(mode.anc[[tree]]$matrix_1$matrix[Cm, "AbsStratification"], 
      mode.anc[[tree]]$matrix_1$matrix[Cm, "WithinPrimary"])

table(mode.anc[[tree]]$matrix_1$matrix[Ord, "AbsStratification"])
length(na.omit(mode.anc[[tree]]$matrix_1$matrix[Ord, "AbsStratification"]))

# Self-supported echinoderms on shells/bryozoans/etc. (Only a few edrios are
# supported: Belochthus, Euryeschatia, Isorophus, Isorophusella, and node 399)
table(mode.anc[[tree]]$matrix_1$matrix[Ord, "Lithic"], 
      mode.anc[[tree]]$matrix_1$matrix[Ord, "Biotic"], 
      mode.anc[[tree]]$matrix_1$matrix[Ord, "SelfSupport"])

table(mode.anc[[tree]]$matrix_1$matrix[Ord, "Lithic"], 
      mode.anc[[tree]]$matrix_1$matrix[Ord, "Biotic"], 
      mode.anc[[tree]]$matrix_1$matrix[Ord, "HardSubstratum"])

table(mode.anc[[tree]]$matrix_1$matrix[Cm, "Mobility"])



# See life-habit characters for a genus
group[which(group$Genus == genus), ]

# See class, body volume, AbsStratDist for a genus
group[which(group$Genus == genus), c(3, 10, 19:20)]



## Find which genera (including nodes) have some life habit trait (but note need
## to specify a particular tree). If get an error, re-run "7 - RUN FIRST
## pre-life-habit trend data processing

# High-density feeders:
hd.filts <-
  rownames(mode.anc[[tree]]$matrix_1$matrix[which(mode.anc[[tree]]$matrix_1$matrix[, "FilterDensity"] == 2),])
hd.filts <- taxon.list[[tree]][which(taxon.list[[tree]][, "genus"] %in% hd.filts), ]
sort(table(hd.filts[, "class"]))

# Soft substrates:
soft <-
  rownames(mode.anc[[tree]]$matrix_1$matrix[which(mode.anc[[tree]]$matrix_1$matrix[, "SoftSubstratum"] == 1),])
softs <- taxon.list[[tree]][which(taxon.list[[tree]][, "genus"] %in% soft), ]
sort(table(softs[, "class"]))

# Epibiotic:
epib <-
  rownames(mode.anc[[tree]]$matrix_1$matrix[which(mode.anc[[tree]]$matrix_1$matrix[, "Biotic"] == 1 &
                                                    mode.anc[[tree]]$matrix_1$matrix[, "AbovePrimary"] == 1),])
epib
wh.epibs <- which(taxon.list[[tree]][, "genus"] %in% epib)
epibs <- taxon.list[[tree]][wh.epibs, ]
table(epibs[, "class"])
table(paste0(mode.anc[[tree]]$matrix_1$matrix[wh.epibs, 9], 
            mode.anc[[tree]]$matrix_1$matrix[wh.epibs, 10]))

# Infaunal:
# Only apodid holothuroid Porosothyone, so following modified from rest
wh.inf <-
  which(mode.anc[[tree]]$matrix_1$matrix[, "WithinPrimary"] == 1 &
          mode.anc[[tree]]$matrix_1$matrix[, "AbovePrimary"] == 0)
names(wh.inf)
taxon.list[[tree]][wh.inf, ]

# Semi-infaunal:
semi <-
  rownames(mode.anc[[tree]]$matrix_1$matrix[which(mode.anc[[tree]]$matrix_1$matrix[, "AbovePrimary"] == 1 &
                                                    mode.anc[[tree]]$matrix_1$matrix[, "WithinPrimary"] == 1),])
semi
semis <- taxon.list[[tree]][which(taxon.list[[tree]][, "genus"] %in% semi), ]
table(semis[, "class"])

# Mass feeders:
mf <-
  rownames(mode.anc[[tree]]$matrix_1$matrix[which(mode.anc[[tree]]$matrix_1$matrix[, "MassFeeder"] == 1),])
mfs <- taxon.list[[tree]][which(taxon.list[[tree]][, "genus"] %in% mf), ]
sort(table(mfs[, "class"]))

# Mobility (here for sedentary)
seds <-
  rownames(mode.anc[[tree]]$matrix_1$matrix[which(mode.anc[[tree]]$matrix_1$matrix[, "Mobility"] == 0), ])
seds <- taxon.list[[tree]][which(taxon.list[[tree]][, "genus"] %in% seds), ]
sort(table(seds[, "class"]))


# Separate Cambrian and Ordovician taxa, and find which ones had certain traits
Ord <- which(median.ranges[, "LAD"] < 485.4)
Cm <- which(median.ranges[, "FAD"] > 485.4)
# Cm <- which(median.ranges[, "FAD"] > 521) # 0 Terreneuvian tips
names(Cm)

# which are Cambrian mass feeders?
Cm[which(names(Cm) %in% mf)]
taxon.list[[tree]][Cm[which(names(Cm) %in% mf)], ]



## How many early Cambrian (Terreneuvian-Series 2) echinoderms share (exactly)
## the most-commonly inferred root life habit? (Using life habits of
## Helicoplacus [taxon 177, root habit for EAT tree] and most common root LH
## Ctenoimbricata [taxon 101, root habit for UEH tree].)

Cms <- Cm.LHs <- basals <- sedentaries <- rep(0, 100)
for (tree in 1:100) {
  Cm <- which(ranges[[tree]][, "FAD"] > 485.4)
  # Cm <- which(ranges[[tree]][, "FAD"] > 509)
  Cms[tree] <- length(Cm)
  Cm.LH <- mode.distances.GED.5[[tree]]$distance_matrix[Cm, Cm]
  Cm.LHs[tree] <- nrow(unique(Cm.LH))
  wh.Ctenoimbricata <- which(names(Cm) == "Ctenoimbricata")
  wh.Helicoplacus <- which(names(Cm) == "Helicoplacus")
  basals[tree] <- length(which(Cm.LH[wh.Ctenoimbricata, ] == 0))
  sedentaries[tree] <- length(which(Cm.LH[wh.Helicoplacus, ] == 0))
}

mean(Cms)         # 284.0 Cambrian taxa (tips + nodes)
mean(Cm.LHs)      #  74.5 Cambrian life habits
mean(basals)      #  36.1 Cambrian taxa (tips + nodes) with same LH as Ctenoimbricata
mean(sedentaries) #  65.2 Cambrian taxa (tips + nodes) with same LH as Helicoplacus


Cm <- which(ranges[[1]][, "FAD"] > 485.4)
length(Cm)     # 287 Cambrian lineages (119 tips & 168 stem genera)
Cm.LHs <- apply(mode.anc[[1]]$matrix_1$matrix[Cm, ], 1, paste, collapse="")
unique(Cm.LHs) # 75 unique life habits


nrow(mode.anc[[1]]$matrix_1$matrix[
  which(Cm.LHs == "2111110001001001101010101001000001000010"),])
# 42 Cambrian lineages share the root (soft-substrate, mobile) life habit of
# Ctenoimbricata

nrow(mode.anc[[1]]$matrix_1$matrix[
  which(Cm.LHs == "3222200001010010101010101001000001000010"),])
# 60 Cambrian lineages share the other basal (hard-substrate, sedentary, small)
# life habit, of Helicoplacus

# What is the root life habit across trees?
for(tr in 1:100) {
  cat(paste(mode.anc[[tr]]$matrix_1$matrix[367,]), "\n")
}
# Ctenoimbricata for UEH (with 1 polymorphism for AbsFoodStrat) and Helicoplacus
# for EAT

# Explore all Terreneuvian life habits
Terr.LHs <- vector("list", 100)
for(tree in 1:100) {
  Terr <- which(ranges[[tree]][, "FAD"] > 521)
  Terr.LHs[[tree]] <- apply(mode.anc[[tree]]$matrix_1$matrix[Terr, ], 1, paste, collapse="")
}

unique(unlist(Terr.LHs, use.names = FALSE)) # 58 unique Terreneuvian life habits across 100 trees

# Those in tree 49 (UEH tree)
Terr.LHs[[49]]
table(Terr.LHs[[49]])

sort(table(unlist(Terr.LHs, use.names = FALSE)))
# 3222200001010010101010101001000001000010 in 631 taxa (tips/nodes): Helicoplacus
# 2111110001001001101010101001000001000010 in 290 taxa (tips/nodes): Ctenoimbricata
# rest less than 57

# Confirming location of various genera. Fig. 6 uses the UEH topology for tree
# 49 and fig. S6 uses the EAT toplogy for tree 57. Note the rownames for tip
# genera are constant across PCoA ordinations.
load("mode.pcoa") # Tree 57
load("mode.pcoa.49") # Tree 49

wh.gen <-
  which(rownames(mode.pcoa$vectors.cor) %in% 
          c("Helicoplacus", "Ctenoimbricata", "Nolichuckia", "Ophioxenikos"))
mode.pcoa.49$vectors.cor[wh.gen, 1:4]
mode.pcoa$vectors.cor[wh.gen, 1:4]

