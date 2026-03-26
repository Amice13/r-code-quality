# stat_analysis.R
# J.D. Hogan
# Systems Biology Team
# MRL Exploratory Science Center
# Script for performing statistical analyses on the sepsis flow cytometry data

# load libraries
library(SingleCellExperiment) # to handle daFrame
library(diffcyt)              # for getting proper data format for GLM

# load daFrame
load("../results/daFrame.RData")

# load metadata file
md <- read.csv("../data/metadata.csv")

# set necessary variables
clustering_to_use <- "merged_clusters"
code_id           <- colData(daf)$cluster_id
cluster_id        <- metadata(daf)$cluster_codes[, clustering_to_use][code_id]

# add columns to colData
colData(daf)$code_id    <- code_id
colData(daf)$cluster_id <- cluster_id

# make experiment_info data frame
experiment_info <- merge(md,
                         metadata(daf)$experiment_info[, c("sample_id",
                                                           "n_cells")],
                         by = "sample_id")
metadata        <- metadata(daf)

# split cells by combined sample
cs_by_s <- split(seq_len(ncol(daf)), colData(daf)$sample_id)

# re-order according to experiment_info
cs <- unlist(cs_by_s[as.character(experiment_info$sample_id)])
es <- t(assays(daf)[["exprs"]])[cs, , drop = FALSE]

# create SummarizedExperiment
d_se <- SummarizedExperiment(
  assays   = list(exprs = es),
  rowData  = colData(daf)[cs, ],
  colData  = rowData(daf),
  metadata = metadata
)

# calculate cell counts per sample
d_counts <- calcCounts(d_se)

# create proportion matrix
prop_mat <- d_counts@assays@data$counts / 4000 # we downsampled to 4000 cells

# set up result data frame
rslt <- data.frame(cluster = seq_len(nrow(prop_mat)),
                   t.value = rep(0, nrow(prop_mat)),
                   p.value = rep(0, nrow(prop_mat)))

# loop
for (i in seq_len(nrow(prop_mat))) {
  # GLM
  mod <- glm(prop_mat[i, ] ~ md$condition, family = "quasibinomial")
  
  # add to result data frame
  rslt[i, ] <- c(i, summary(mod)$coef[2, 3], summary(mod)$coef[2, 4])
}

# multiple testing correction
rslt$fdr <- p.adjust(rslt$p.value, method = "fdr")

# order by p-value
rslt <- rslt[order(rslt$p.value), ]

# write to table
write.table(x = rslt,
            file = "../results/glm_results.csv",
            quote = F,
            sep = ",",
            row.names = F)
