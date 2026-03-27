## GET TIME-SCALED STRATIGRAPHIC RANGES FOR TIPS AND NODES, ####################
## AND ASSIGN TAXONOMIC CLASSES FOR NODES ######################################



## 1 - PREPARATIONS ############################################################
rm(list = ls())
op <- par()

# Set working directory (point to the folder containing the input files on your
# own machine):
# setwd("[filepath to folder containing data files on your personal machine]")
setwd("~/Manuscripts/CamOrdEchino_EcoTrends/Data & analyses")

# Load packages
library(ape)        # v. 5.7-1

# Load tree and character matrices (with inferred ancestral states)
load("mode.anc")

# Note because the cal3trees were modified after creation (to remove zero-length
# branches), using the tree objects appended to the Claddis object instead.




## GET STRAT RANGES (FADs AND LADs) FOR TIPS AND NODES #########################

# Based on code written by Dave Bapst. Thanks, Dave! Root node set as root age,
# with FAD = LAD.

ranges <- vector("list", length(mode.anc))
for(t in 1:length(mode.anc)) {
  tree <- mode.anc[[t]]$topper$tree
  tree.ranges <- tree$edge
  # Use time-scaled tree branch lengths to get relative node ages, and use root
  # time to scale to absolute time
  ntime <- tree$root.time - ape::node.depth.edgelength(tree)
  tree.ranges[, 1] <- ntime[tree.ranges[, 1]]
  tree.ranges[, 2] <- ntime[tree.ranges[, 2]]
  # Now assign names for each lineage as row names
  terminalEdges <- which(tree$edge[, 2] <= Ntip(tree))
  colnames(tree.ranges) <- c("FAD", "LAD")
  rownames(tree.ranges) <- as.character(tree$edge[, 2])
  rownames(tree.ranges)[terminalEdges] <- tree$tip.label[tree$edge[terminalEdges, 2]]
  # Add root node with FAD = LAD = root.time
  tree.ranges <- rbind(tree.ranges, rep(tree$root.time, 2))
  rownames(tree.ranges)[Nnode(tree) + Ntip(tree)] <- as.character(Ntip(tree) + 1)
  # Reorder so matches order of the cladistic matrix
  tree.ranges <-
    tree.ranges[match(rownames(mode.anc[[t]]$matrix_1$matrix), rownames(tree.ranges)),]
  ranges[[t]] <- tree.ranges
}

# Save for later use
# save(ranges, file = "ranges")
# load("ranges")

# Manually copy the 57th tree ranges to the "EchinoLHData_Mode_Anc.csv"
# spreadsheet.
# write.csv(ranges[[57]], file = "ranges57.csv")



## Observe mean ranges for tips and nodes ######################################

# It is straightforward to calculate the mean FAD and LAD for tip genera because
# they are consistently labelled across the 100 trees. However, because cal3 was
# implemented where polytomies were resolved stochastically and because some
# trees were built with anc.wt = 1 that allows direct ancestor-descendent
# relationships, the label number (i.e., ANC501) can vary across trees.

# Therefore, all downstream analyses calculate necessary statistics on
# individual trees (where the node name does not matter), and then resulting
# statistics are summarized across trees. When 'mean ranges' are used, they are
# used ONLY for tip genera and NOT ancestral nodes.

# Recombine modified trees
sq <- 1:length(mode.anc)
trees <- lapply(sq, function(sq) mode.anc[[sq]]$topper$tree)

# Identify node-based (bifurcating) partitions (nodes sharing identical tips)
pp <- ape::prop.part(trees)
shared <- which(summary(pp) == 100) 
length(shared)
# 270 nodes (of 365 possible, or 74.0%) are consistently shared across 100
# trees. These ones could be averaged across trees [provided the ancestral node
# name is identified correctly within each tree], but are not done so here.


# Calculate mean ranges for tips (but not yet nodes)
sq <- 1:length(ranges)
ntips <- ape::Ntip(trees[[1]])
median.FADs <-
  apply(simplify2array(lapply(sq, function(sq) ranges[[sq]][1:ntips, "FAD"])), 1, median)
median.LADs <-
  apply(simplify2array(lapply(sq, function(sq) ranges[[sq]][1:ntips, "LAD"])), 1, median)
median.ranges <- cbind(FAD = median.FADs, LAD = median.LADs)

# Save for later use
# save(median.ranges, file = "median.ranges")
# load("median.ranges")

head(median.ranges)
tail(median.ranges)

# Export time-scaled ranges, converting to .csv
# write.csv(median.ranges, file = "TSRanges.csv", row.names = TRUE)

# Which are the 30 oldest taxa?
median.ranges[tail(order(median.ranges[, "FAD"]), 30), ]
# On average, the 10 oldest tip taxa include helicoplacoids Polyplacus,
# Waucobella, and Helicoplacus as the oldest tips followed by Helicocystis of
# uncertain affinity, stem edrioasteroid Camptostroma, eocrinoids
# Felbabkacystis, Kinzercystis, Lepidocystis, and Alanisicystis, and solute
# Coleicarpus. Ancestral nodes likely have earlier and shorter ranges than these
# tip genera.

# Stratigraphic range lengths (for tips only)
diffs <- median.ranges[, "FAD"] - median.ranges[, "LAD"]
hist(diffs, 50)
summary(diffs)          # median = 20 Myr, min = 1.8 Myr, max = 129.4 Myr 
which(diffs < 1L)       # none
which(diffs < .5)       # none
which.max(diffs)        # Parisocrinus, a Late Ord-Mississippian crinoid





## ASSIGN TAXONOMIC CLASS AND SUBPHYLUM FROM TREE ##############################

# Logic: Move from tips to nodes, classifying a node to a taxonomic class and
# subphylum (only) if both descendants are in same class/subphylum, and leaving
# unclassified otherwise. Also appends genus names to ease in future handling,
# and more intuitive confirmation working as intended. Because the order of tip
# labels varies among trees, need to re-order each set so output in the same
# order as the input data sets (which are roughly alphabetical).

# Import ecological data set:
data <- read.csv(file = "EchinoLHData_Mode.csv", header = TRUE, stringsAsFactors = FALSE)
data[1:10, 1:12]

# Confirm list of genera in spreadsheet is same as in tree
setdiff(mode.anc[[1]]$topper$tree$tip.label, data$Genus)
setdiff(data$Genus, mode.anc[[1]]$topper$tree$tip.label)
# Anatifopsis is false-positive because replaced with two subgenera
# (Anatiferocystis & Guichenocarpos in the tree files. These require special
# handling below.

# Change names to match tree tip labels
data$Genus[which(data$Genus == "Anatifopsis")] <-
  c("Anatiferocystis", "Guichenocarpos")

# Two passes. Start at tips and move down list, then reverse, moving from last
# node to root. Note the order of the list is determined by the tree$tip.labels,
# which differs from that in the raw input data (e.g., EchinoLHData_Mode.csv).
taxon.list <- vector("list", length(mode.anc))
for(t in 1:length(taxon.list)) {
  tree <- mode.anc[[t]]$topper$tree
  t.class.list <- t.subphylum.list <- character(Ntip(tree) + Nnode(tree))
  genus.match <- match(tree$tip.label, data$Genus)
  genus.list <- data$Genus[genus.match]
  t.class.list[seq.int(Ntip(tree))] <- data$Class[genus.match]
  t.subphylum.list[seq.int(Ntip(tree))] <- data$Subphylum[genus.match]
  orig.classes <- t.class.list
  orig.subphyla <- t.subphylum.list
  # First pass along tips
  for (n in 1:Ntip(tree)) {
    wh.node <-
      tree$edge[which(tree$edge[, 2] == n), ][1]   # Find node for this tip
    tip.classes <- t.class.list[tree$edge[which(tree$edge[,1] == wh.node), 2]]
    if (identical(tip.classes[1], tip.classes[2]))
      t.class.list[wh.node] <- tip.classes[1]
    tip.subphyla <- t.subphylum.list[tree$edge[which(tree$edge[,1] == wh.node), 2]]
    if (identical(tip.subphyla[1], tip.subphyla[2]))
      t.subphylum.list[wh.node] <- tip.subphyla[1]
  }
  # Second pass moving nodes down to root (incl. some redundancy from above)
  start <- Ntip(tree) + Nnode(tree)
  end <- Ntip(tree) + 1               # End at root (= first node)
  for (n in start:end) {
    wh.node <-
      tree$edge[which(tree$edge[, 2] == n), ][1]   # Find node for this tip
    tip.classes <- t.class.list[tree$edge[which(tree$edge[,1] == wh.node), 2]]
    if (identical(tip.classes[1], tip.classes[2]))
      t.class.list[wh.node] <- tip.classes[1]
    tip.subphyla <- t.subphylum.list[tree$edge[which(tree$edge[,1] == wh.node), 2]]
    if (identical(tip.subphyla[1], tip.subphyla[2]))
      t.subphylum.list[wh.node] <- tip.subphyla[1]
  }
  # Replace class/subphylum = "" with class = "UNCERTAIN"
  t.class.list <-
    replace(t.class.list, which(t.class.list == ""), "UNCERTAIN")
  t.subphylum.list <-
    replace(t.subphylum.list, which(t.subphylum.list == ""), "UNCERTAIN")
  # Change back to original input data order
  tip.match <- match(data$Genus, tree$tip.label)
  reordered.classes <-
    c(t.class.list[tip.match], t.class.list[(Ntip(tree) + 1):(Ntip(tree) + Nnode(tree))])
  reordered.subphyla <-
    c(t.subphylum.list[tip.match], t.subphylum.list[(Ntip(tree) + 1):(Ntip(tree) + Nnode(tree))])
  genus.list <-
    c(data$Genus, as.character((Ntip(tree) + 1):(Ntip(tree) + Nnode(tree))))
  taxon.list[[t]] <- cbind(subphylum = reordered.subphyla, 
                           class = reordered.classes, genus = genus.list)
}

# save(taxon.list, file = "taxon.list")
# load("taxon.list")

# View output to confirm worked as intended (If monophyletic, nodes should be
# adjacent and one less than the tips)
head(taxon.list[[57]])
taxon.list[[57]][360:370, ]
taxon.list[[57]][which(taxon.list[[57]][, "class"] == "Helicoplacoidea"), ]
taxon.list[[57]][which(taxon.list[[57]][, "class"] == "Ctenocystoidea"), ]
taxon.list[[57]][which(taxon.list[[57]][, "subphylum"] == "Echinozoa"), ]
table(taxon.list[[57]][, "class"], taxon.list[[57]][, "subphylum"])
# Note all Subphylum Uncertain also in Class Uncertain, but not the other way
# around (as expected if tips are in different classes in same subphylum)

# If each class is monophyletic, the final tallies should be 2 * Ntip(class i) -
# 1.  Note this tallies across both the UEH and EAT tree topologies.

# Monophyletic vs not (for tree no. 57 only):
tree <- mode.anc[[57]]$topper$tree
t.class.list <- character(Ntip(tree) + Nnode(tree))
genus.match <- match(tree$tip.label, data$Genus)
t.class.list[seq.int(Ntip(tree))] <- data$Class[genus.match]
orig.classes <- t.class.list
orig.classes <- replace(orig.classes, which(orig.classes == ""), "UNCERTAIN") 
sort((table(orig.classes) * 2 - 1) - table(taxon.list[[57]][, "class"]), 
     decreasing = FALSE)
# For tree 57, most classes are monophyletic, but rhombiferans, eocrinoids, and
# diploporitans have some non-monophyly, with smaller amounts among
# edrioasteroids, stenuroids, somasteroids, paracrinoids, holothuroids, and
# asteroids.

orig.subphyla <- replace(orig.subphyla, which(orig.subphyla == ""), "UNCERTAIN") 
sort((table(orig.subphyla) * 2 - 1) - table(taxon.list[[57]][, "subphylum"]), 
     decreasing = FALSE)
# Results: Blastozoans have decent amount of non-monophyly. But crinozoans,
# asterozoans, and echinozoans are monophyletic.

# Taxonomic class coverage
sort(table(orig.classes), decreasing = FALSE)
sort(table(taxon.list[[57]][, "class"]), decreasing = FALSE)

sort(table(orig.subphyla), decreasing = FALSE)
sort(table(taxon.list[[57]][, "subphylum"]), decreasing = FALSE)

# Manually copy the 57th taxonomic affinities to the "EchinoLHData_Mode_Anc.csv"
# spreadsheet.
# write.csv(taxon.list[[57]], file = "classes57.csv")

