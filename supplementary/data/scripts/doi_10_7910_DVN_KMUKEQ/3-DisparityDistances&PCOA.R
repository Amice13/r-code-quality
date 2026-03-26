## CONVERT TAXON-CHARACTER MATRIX TO DISTANCE MATRIX USING CLADDIS AND #########
## PERFORM PCOA ORDINATION #####################################################

# Prior to running, run 2-InferAncestralStates.R to infer ancestral states using
# 'Claddis' package.


## PREPARATIONS ################################################################
rm(list = ls())
op <- par()

# Set working directory (point to the folder containing the input files on your
# own machine):
setwd("~/Manuscripts/CamOrdEchino_EcoTrends/Data & analyses")
# setwd("[filepath to folder containing data files on your personal machine]")

# Load packages
library(ade4)       # v. 1.7-22
library(beepr)      # v. 1.3
library(Claddis)    # v. 0.6.3 - Check SI for Lloyd 2018 for walk-through on code functions
if(packageVersion("Claddis") < "0.6.0")
  stop("install a more recent version 'Claddis' Substantial coding changes
       accompanied the update to Claddis v. 0.6.0 in August 2020.\n")
library(snowfall)   # v. 1.84-6.2
library(parallel)   # v. 4.3.1




## IMPORT FILES ################################################################

# See 2-InferAncestralStates.R for how created.

load("mode.anc")
length(mode.anc)
mode.anc[[57]]$topper
mode.anc[[57]]$matrix_1$matrix[360:370, ]


# Create character dependencies: Two-column matrix with colnames
# "dependent_character" and "independent_character" that specifies character
# hierarchies. Here, FilterDensity (character  no. 29) depends on FilterFeed
# (character no. 28).
CharDepends <- matrix(data = c(29, 28), nrow = 1, dimnames = 
                        list("", c("dependent_character", 
                                   "independent_character")))
CharDepends



## CALCULATE DISPARITY DISTANCE MATRIX #########################################

# Lloyd (2016) does sensitivity tests of four distance metrics (raw Euclidean
# distance [RED], general Euclidean distance [GED], Gower's coefficient [GC],
# and maximum observable re-scaled distance [MORD, equal to Gower when data are
# all binary or unordered]. All are fine, but best depends on proportion of
# missing data, presence of ordered data, and size of data set. Using Wills
# generalized Euclidean distance (GED) with alpha = 0.5 for consistency with
# that used in Novack-Gottshall, et al., 2022. See SI in that paper for
# confirmation that use of others measures results in consistent results.

# Unless specified, behavior for polymorphisms (= "min_difference"), uncertain
# (= "min_difference"), and inapplicable (= "missing") characters use the
# default settings for the function.

# GED: Wills' Generalized Euclidean distance, with alpha = 0.5 (H&SJ 2018)
# - "When Alpha = 0.5, the primary character contributes weight according to the
#    fraction of shared secondary [and tertiary, etc.] characters." (Hopkins and
#    St. John 2018: p. 3)

(t.start0 <- Sys.time())
cpus <- parallel::detectCores()
sfInit(parallel = TRUE, cpus = cpus, slaveOutfile = "initfile")
stopifnot(sfCpus() == cpus)		    # Confirm set up CPUs properly
stopifnot(sfParallel() == TRUE)		# Confirm now running in parallel
sfExportAll()				              # Export all libraries, files, & objects
sfLibrary(Claddis)
mode.distances.GED.5 <-
  sfClusterApplyLB(x = mode.anc, fun = calculate_morphological_distances, 
                   distance_metric = "ged", ged_type = "wills",
                   alpha = 0.5, inapplicable_behaviour = "hsj",
                   character_dependencies = CharDepends)
sfStop()
(Sys.time() - t.start0) # 56.1 min for mode life-habit data set with 8 cores
save(mode.distances.GED.5, file = "mode.distances.GED.5")
beepr::beep(3)





## PERFORM PHYLOGENETIC PRINCIPAL COORDINATES ANALYSIS (PCoA) ##################

# Skip 'Claddis::ordinate_cladistic_matrix' [which bundles ancestral state
# reconstruction and calculation of distance matrices] by using direct
# calculation of PCoA so can directly import in previously calculated distance
# matrices.

# For consistency with other Claddis functions, using ape::pcoa() instead of
# 'ecospace' and 'FD's use of ade4::dudi.pco(), which produces identical output
# (in different formats), and are essentially equally fast. Variation typically
# has to do with how many non-negative eigenvectors are returned, and with
# arbitrary reversing of axes. Eigenvectors are identical for at least first
# 11-15 axes (not demonstrated here, see "X_Diff pcoa trials.R" in
# Novack-Gottshall, et al. 2022 NEE).

# Because many of the distance matrices are non-Euclidean (with non-diagonal
# zeroes) and return negative eigenvalues, we are using the "Lingoes" correction
# here. Tests on earlier trials (not demonstrated here, see "X_Diff pcoa
# trials.R") found little difference between using the Cailliez and Lingoes
# corrections, but the Lingoes correction is mathematically more tractable with
# a greater number of the trees used here (and data sets used in associated
# disparity analyses in Novack-Gottshall, et al., 2022 NEE). Ultimately, the
# impact of corrections is minor because comparison of the character-space
# (ecospace) structure for the first 6 axes are essentially identical across
# corrections (for those distance matrices that can be calculated for them).

## *** ONLY USING TREE #57 (See 1-MakeTimeTrees.R for rationale) ***
t <- 57
# t <- 49 # Making one for the UEH topology for comparison)

# Built using 3-DisparityDistances.R
load("mode.distances.GED.5")
dist.matrix <- mode.distances.GED.5[[t]]
dist.matrix$distance_matrix[1:4, 1:4]

# Built using 2-InferAncestralStates.R
load("mode.anc")
mode.anc <- mode.anc[[t]]

# Collect garbage to increase available memory
rm("mode.distances.GED.5")
gc()

mode.pcoa <- ape::pcoa(dist.matrix$distance_matrix,
                       correction = "lingoes",
                       rn = rownames(dist.matrix$distance_matrix))
# Append tree, required for downstream Claddis functions
if (is.null(mode.pcoa$tree))
  mode.pcoa$tree <- mode.anc$topper$tree

# Save (and re-load) PCoA output:
save(mode.pcoa, file = "mode.pcoa")
# mode.pcoa.49 <- mode.pcoa; save(mode.pcoa.49, file = "mode.pcoa.49")
# load("mode.pcoa"); mode.pcoa <- mode.pcoa
beepr::beep()


# Plot sample scree plot and biplot
barplot(100 * mode.pcoa$values$Rel_corr_eig[1:15], main = "first 10 axes", 
        names.arg = 1:15, xlab = "Axes", ylab = "relative % explained")
barplot(100 * cumsum(mode.pcoa$values$Rel_corr_eig)[1:15], names.arg = 1:15,
        xlab = "Axes", ylab = "relative % explained", 
        main = "cum var explained for first 10 axes")
plot(mode.pcoa$vectors.cor[, 1], mode.pcoa$vectors.cor[, 2], xlab = "PCoA axis 1", 
     ylab = "PCoA axis 2")

round(100 * mode.pcoa$values$Rel_corr_eig[1:10], 2)
round(100 * cumsum(mode.pcoa$values$Rel_corr_eig)[1:30], 1)


# RESULTS FOR TREE 57 (% explained; cumulative % explained):
# Mode:     1- 1.38%, 2-0.40% (1.8%), 3-0.32% (2.1%), 4-0.26% (2.4%), 
#                     5-0.22% (2.6%), 6-0.21% (2.8%)
# Note that the results are essentially identical for tree 49, too.

## Calculate (mock) "factor loadings" (for tree #57)
# Because PCoA uses the distance matrix instead of observed variables, it is not
# possible to relate the original variables to the PCoA axes. However, the basic
# premise of a correlation between PCoA space and original variables can be
# informative.
#   orig.vars = data with original variables
#   ord.coord = principal coordinate vectors output from ape::pcoa
#   vars      = how many principal coordinate axes do you want?
#   cutoff    = what absolute value do you want to mask uncorrelated variables? 
mock.loadings <- function(orig.vars = NULL, ord.coord = NULL, vars = 6, 
                          cutoff = 0.5) {
  out <- matrix(NA, nrow = ncol(orig.vars), ncol = vars)
  for (r in 1:nrow(out)) {
    # exclude any characters that are all NAs
    for (c in 1:vars) {
      if (all(is.na(as.numeric(orig.vars[, r]))))
        next
      # exclude any characters in which only two two are coded (b/c r is
      # uninformative because must equal 1)
      if (sum(!is.na(as.numeric(orig.vars[, r]))) < 3L)
        next
      out[r, c] <- cor(as.numeric(orig.vars[, r]), ord.coord[, c],
                       use = "complete.obs")
    }
  }
  out <- round(out, 3)
  out <- as.data.frame(replace(out, which(abs(out) < cutoff), "-"))
  return(out)
}

loadings.mode <- mock.loadings(orig.vars = mode.anc$matrix_1$matrix, 
                               ord.coord = mode.pcoa$vectors.cor, vars = 6, 
                               cutoff = 0.4)
na.omit(loadings.mode)


# RESULTS (*** USING ONLY TREE #57 ***):

# MODE DATA SET:      -                                +
#  PCO 1:     6, 13, 16, (24, 26, 31)                  2-5, (9), 12, 15, (23, 25, 28-29)
#   +: Attached (filter feeders) living far from hard (or biotic) substrate
#   -: Free-living, mobile (mass-feeding deposit/scavenging feeders) living on soft substrates)
#  PCO 2:   (23, 25, 28)                               1, (24, 26, 31, 36, 40)
#   +: Large bodied echinoderms (especially mass/bulk/deposit/carnivore/scavenging feeders)
#   -: Filter feeders (and those feeding above substrate)
#  PCO 3:    (1)                                     (29)
#   +: (Smallest with) highest density filter feeders
#   -: Largest (non-filterers?)
#  PCO 4:    (9, 12, 24, 26)                         (13)
#  *** NOTE THIS AXIS IS REVERSED FROM THAT IN 2002 NEE ***
#   +: (Soft substrates)
#   -: (Biotic and hard substrates and feeding within substrate) 
#  PCO 5:   (23)                                     (10, 24, 26, 32, 40)
#  *** NOTE THIS AXIS IS REVERSED FROM THAT IN 2002 NEE ***
#   +: (Raptorial/bulk feeders [= scavengers] on lithic substrates with infaunal food)
#   -: (Food above substrate)
#  PCO 6:                                            (5)
#  *** NOTE THIS AXIS IS REVERSED FROM THAT IN 2002 NEE ***
#   +: Large RelFoodStrat (= food at a distance)
#   -: Small RelFoodStrat (= eating in contact with food)


# Note that the results are essentially identical for tree 49, too.




## PLOT PCOA ORDINATION USING CLADDIS FUNCTIONS ################################

## *** ONLY USING TREE #57 (See 1-MakeTimeTrees.R for rationale) ***

# Plot a 2-dimensional morphospace with tips, nodes, and root:
Claddis::plot_morphospace(mode.pcoa)
tip.seq <- 1:ape::Ntip(mode.pcoa$tree)
node.seq <-
  (ape::Ntip(mode.pcoa$tree) + 1):(ape::Ntip(mode.pcoa$tree) + ape::Nnode(mode.pcoa$tree))
# Nodes
points(x = mode.pcoa$vectors.cor[node.seq, 1],
       y = mode.pcoa$vectors.cor[node.seq, 2], col = "gray", pch = 16)
# Tips
points(x = mode.pcoa$vectors.cor[tip.seq, 1], 
       y = mode.pcoa$vectors.cor[tip.seq, 2])
# Root (ANC367)
points(x = mode.pcoa$vectors.cor[min(node.seq), 1], 
       y = mode.pcoa$vectors.cor[min(node.seq), 2], col = "red", pch = 16, 
       cex = 1.5)
