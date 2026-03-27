# run_flow_cyto_pipeline.R
# J.D. Hogan
# Systems Biology Team
# MRL Exploratory Science Center
# Script for running the cytometry data analysis pipeline on the sepsis flow
# cytometry data

# load libraries
library(CATALYST)             # for data analysis
library(flowCore)             # for loading and analyzing much of the data
library(magrittr)             # needed to run the altered daFrame code
library(dplyr)                # needed to run the altered daFrame code
library(SingleCellExperiment) # needed to run the altered daFrame code
library(diffcyt)              # for differential expression calculations

# load functions
source("./functions.R")

# set global variables
FDIR  <- "../data/fcs_files/"      # directory containing FACS files
MDFL  <- "../data/metadata.csv"    # metadata file
PNL   <- "../data/panel.csv"       # panel file
GRP   <- "condition"               # grouping variable
NCELL <- 4000                      # number of cells to randomly sample per file
ODIR  <- "../data/results/"        # output directory
USEED <- 836572855                 # seed, for reproducibility
NCLU  <- 20                        # number of clusters
CFCTR <- 150                       # cofactor for asinh transformation
MRGFL <- "../data/merge_table.csv" # file with which clusters to merge

# load metadata file
md <- read.csv(MDFL)

# load panel file
pnl <- read.csv(PNL)

# make necessary adjustments to panel data frame
pnl$fcs_colname <- make.names(pnl$fcs_colname) # fix names to match column names
pnl             <- pnl[pnl$Keep %in% c("Yes", "yes"),] # throw out non-keepers

# set seed
set.seed(USEED)

# load .fcs data
fs_raw <- read.flowSet(path = FDIR, transformation = F, alter.names = T)

# correct FILENAME in fs
for (i in seq_len(length(fs_raw))) {
  fs_raw[[i]]@description$FILENAME <- fs_raw[[i]]@description$GUID
}

# subset md to make sure same number of samples
md <- md[which(md$file_name %in% rownames(fs_raw@phenoData)), ]

# get md in correct order
rownames(md) <- md$file_name
md           <- md[rownames(fs_raw@phenoData), ]

# get cell counts and add to metadata
ccounts <- fsApply(fs_raw, nrow)
md      <- cbind(md, ccounts)

# downsample and transform data
fs <- fs_raw[seq(along = fs_raw)] # want to keep raw values
for (i in seq_len(length(fs))) {
  # downsampling
  if (md$ccounts[i] > NCELL) { # downsampling # can't be larger than # of cells
    exprs(fs[[i]]) <- exprs(fs[[i]])[sample(1:md$ccounts[i], NCELL), ]
    
    # update parameters file
    ps_tmp <- parameters(fs[[i]])
    for (j in seq_len(nrow(ps_tmp@data))) {
      ps_tmp@data$minRange[j] <- min(exprs(fs[[i]])[, j])
      ps_tmp@data$maxRange[j] <- max(exprs(fs[[i]])[, j])
      ps_tmp@data$range[j] <- max(exprs(fs[[i]])[, j]) -
        min(exprs(fs[[i]])[, j])
    }
    parameters(fs[[i]]) <- ps_tmp
  }
  
  # transformation
  fs[[i]]@exprs <- apply(fs[[i]]@exprs[,
                                       as.character(pnl$fcs_colname),
                                       drop = F],
                         2,
                         cytofAsinh,
                         cofactor = CFCTR)
  
  # give cells unique names
  curf <- fs[[i]]@description$FILENAME # current filename
  curs <- md[which(md$file_name == curf), "sample_id"] # current sample name
  rownames(fs[[i]]@exprs) <- sapply(seq_len(nrow(fs[[i]]@exprs)),
                                    function(x) paste0(curs, "_", x))
}

# create daFrame
daf <- prepData2(x = fs,
                 panel = pnl,
                 md = md,
                 md_cols = list(file = "file_name",
                                id = "sample_id",
                                factors = c(GRP, "patient_id")),
                 cofactor = CFCTR)

# flowSOM clustering
daf <- cluster(x = daf,
               features = NULL,
               xdim = 10,
               ydim = 10,
               maxK = NCLU,
               verbose = F)

# merge clusters that were manually selected for merging
mrg_tbl <- read.csv(MRGFL)
daf     <- mergeClusters(x = daf,
                         k = "meta20",
                         table = mrg_tbl,
                         id = "merged_clusters")

# dimensionality reduction
daf <- runDR(daf, "UMAP")

# save daFrame object to .RData file
save(daf, file = "../results/daFrame.RData")
