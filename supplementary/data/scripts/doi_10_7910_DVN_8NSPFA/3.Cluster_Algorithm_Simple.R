# ============================================================================
# Clustering Algorithm - Streamlined Version
# Direct approach using PCA composite variables and Spectral Clustering
# ============================================================================

# Load required libraries
library(randomForest)
library(cluster)
library(anocva)
library(ClustOfVar)
library(psych)
library(tibble)
library(clustree)
library(readxl)
library(writexl)
library(fmsb)
library(haven)
library(readr)
library(patchwork)

# Create Result directory if it doesn't exist
if (!dir.exists("Result")) {
  dir.create("Result")
}

if (!dir.exists("Result/simplified_version")) {
  dir.create("Result/simplified_version", recursive = TRUE)
}

# ============================================================================
# STEP 1: Load Data and Variable Clustering Analysis
# ============================================================================
# Load data
inputvars.covnetps <- read_excel("covnetps_forRF.xlsx")
inputvars.covnetps$hasPartner_i <- as.factor(inputvars.covnetps$hasPartner_i)
inputvars.covnetps$edu_status_c <- as.factor(inputvars.covnetps$edu_status_c)

cat("Data loaded successfully\n")
cat("Total observations:", nrow(inputvars.covnetps), "\n")
cat("Wave 1 observations: 1-2047\n")
cat("Wave 2 observations: 2048-2571\n\n")

cat("Performing variable clustering stability analysis...\n")
set.seed(111)

# Prepare data for variable clustering (using Wave 1 data)
data_for_var_clustering <- inputvars.covnetps[1:2047, ]

# Separate quantitative and qualitative variables
x.quanti <- as.data.frame(subset(data_for_var_clustering, 
                                 select = -c(wave, egoid, hasPartner_i, edu_status_c)))
x.quali <- data.frame(
  hasPartner_i = as.factor(data_for_var_clustering$hasPartner_i),
  edu_status_c = as.factor(data_for_var_clustering$edu_status_c)
)

# Hierarchical clustering of variables
tree <- hclustvar(x.quanti, x.quali)

# Stability analysis
set.seed(111)
stab <- stability(tree, B = 40)

# Plot variable clustering stability
pdf("Result/simplified_version/Fig_1_Variable_Clusters.pdf", 
    width = 10, height = 7.5)
plot(stab, cex = 0.6, 
     main = "Fig 1. Number of Variable Clusters by Mean Adjusted Rand Criterion")
dev.off()

# Optimal Cut-off point of tree structure (n=12)
tree.cut <- cutreevar(tree, 12)

# Create, sort, and save cluster assignments
tree_cluster <- data.frame(
  Variable = names(tree.cut$cluster),
  Cluster = tree.cut$cluster
)
tree_cluster <- tree_cluster[order(tree_cluster$Cluster), ]
rownames(tree_cluster) <- NULL

write.csv(tree_cluster, "Result/simplified_version/Composite_variable_list.csv", row.names = FALSE)

cat("Variable clustering stability analysis completed\n")
cat("Optimal number of variable clusters can be identified from the plot\n\n")

# ============================================================================
# STEP 2: Create PCA Composite Variables
# PS: Composite variables are grouped based on human knowledge and optimal cut-off analysis above
# ============================================================================

# Prepare variables for PCA
group_var <- inputvars.covnetps[, 3:45]

# Standardize variables
for (i in 1:ncol(group_var)) {
  group_var[, i] <- as.numeric(unlist(group_var[, i]))
  group_var[, i] <- scale(group_var[, i])
}

# Define 10 composite variables
variable_groups <- list(
  kin_support = c("kin_total_n", "kin_exchange_n", "kin_personal_n", 
                  "kin_help_n", "kin_entertainment_n", "kin_hobbies_n"),
  kin_involvement = c("kin_pct_close_p", "kin_live_far_p", 
                      "kin_freq_see_m", "kin_freq_online_m"),
  nonkin_support = c("nonkin_total_n", "nonkin_entertainment_n", 
                     "nonkin_exchange_n", "nonkin_personal_n", 
                     "nonkin_help_n", "nonkin_hobbies_n"),
  homophily = c("hom_age_p", "hom_gender_p", "hom_province_p"),
  school_based = c("hom_school_p", "hom_college_p"),
  nonkin_distance = c("nonkin_live_near_p", "nonkin_live_far_p"),
  outdoor_activities = c("freq_restaurant_o", "freq_hangout_o", "freq_visit_o"),
  nonkin_tie_strength = c("nonkin_pct_close_p", "avg_years_known_m"),
  nonkin_online = c("nonkin_freq_see_m", "nonkin_freq_online_m"),
  work_based = c("hom_work_p", "freq_work_o")
)

# Create composite variables
cat("Creating PCA composite variables...\n")
for (group_name in names(variable_groups)) {
  vars <- variable_groups[[group_name]]
  data_subset <- group_var[, vars, drop = FALSE]
  pca_scores <- principal(data_subset, nfactors = 1, score = TRUE)$scores
  inputvars.covnetps[, group_name] <- pca_scores
  cat("  -", group_name, "created\n")
}

cat("\n10 composite variables created successfully\n\n")

# ============================================================================
# STEP 3: Unsupervised Random Forest with Composite Variables
# ============================================================================

cat("Running unsupervised Random Forest on composite variables...\n")

set.seed(111)
RFN <- randomForest(
  inputvars.covnetps[1:2047, 46:55],
  ntree = 10000,
  mtry = 6,
  importance = TRUE,
  proximity = TRUE,
  nodesize = 10,
  keep.forest = TRUE,
  do.trace = FALSE
)

cat("Random Forest completed\n\n")

# Save variable importance plot
pdf("Result/simplified_version/Fig_2_Composite_Variable_Importance.pdf", 
    width = 10, height = 7.5)
varImpPlot(RFN, n.var = 10, cex = 0.6, 
           main = "Fig 2. Performance of 10 Composite Variables")
dev.off()

pdf("Result/simplified_version/Proximity_Density.pdf", 
    width = 10, height = 7.5)
plot(density(RFN$proximity), main = "Proximity Matrix Density",
     xlab = "Proximity", ylab = "Density")
dev.off()

cat("Importance plots saved\n\n")

RFN_dist <- as.dist(1 - RFN$proximity)

# ============================================================================
# STEP 4: Spectral Clustering (Comparing with Agglomerative Clustering )
# ============================================================================

cat("Performing spectral clustering (k = 1 to 20)...\n")
set.seed(111)
pred.part.list <- lapply(1:20, function(k) {
  if (k %% 5 == 0) cat("  - k =", k, "\n")
  spectralClustering(RFN$proximity, k = k)
})
names(pred.part.list) <- paste0("pred.part.", 1:20)
list2env(pred.part.list, envir = .GlobalEnv)
cat("Spectral clustering completed\n\n")

cat("Performing agglomerative clustering (k = 1 to 20)...\n")
HC <- agnes(RFN_dist, method = "gaverage", diss = TRUE)
agg.part.list <- lapply(1:20, function(k) {
  if (k %% 5 == 0) cat("  - k =", k, "\n")
  cutree(HC, k = k)
})
names(agg.part.list) <- paste0("agg.part.", 1:20)
list2env(agg.part.list, envir = .GlobalEnv)
cat("Agglomerative clustering completed\n\n")

# Spectral clustering tree
clust_spectral <- data.frame(
  c_1_clusters = pred.part.1,
  c_2_clusters = pred.part.2,
  c_3_clusters = pred.part.3,
  c_4_clusters = pred.part.4,
  c_5_clusters = pred.part.5,
  c_6_clusters = pred.part.6,
  c_7_clusters = pred.part.7,
  c_8_clusters = pred.part.8,
  c_9_clusters = pred.part.9,
  c_10_clusters = pred.part.10,
  c_11_clusters = pred.part.11,
  c_12_clusters = pred.part.12,
  c_13_clusters = pred.part.13,
  c_14_clusters = pred.part.14
)

# Agglomerative clustering tree
clust_agglom <- data.frame(
  c_1_clusters = agg.part.1,
  c_2_clusters = agg.part.2,
  c_3_clusters = agg.part.3,
  c_4_clusters = agg.part.4,
  c_5_clusters = agg.part.5,
  c_6_clusters = agg.part.6,
  c_7_clusters = agg.part.7,
  c_8_clusters = agg.part.8,
  c_9_clusters = agg.part.9,
  c_10_clusters = agg.part.10,
  c_11_clusters = agg.part.11,
  c_12_clusters = agg.part.12,
  c_13_clusters = agg.part.13,
  c_14_clusters = agg.part.14
)

png("Result/simplified_version/Fig_3_Clustering_Comparison.png", 
    width = 8000, height = 3200, res = 300)

p1 <- clustree(clust_spectral, prefix = "c_", suffix = "_clusters", edge_width = 0.75) +
  ggtitle("Spectral Clustering")

p2 <- clustree(clust_agglom, prefix = "c_", suffix = "_clusters", edge_width = 0.75) +
  ggtitle("Agglomerative Clustering")

print(p1 | p2)
dev.off()

cat("Clustree comparison plot saved\n\n")

# ============================================================================
# STEP 5: Random Forest Models for Cluster Prediction
# ============================================================================

cat("Creating Random Forest models for k = 2 to 20...\n")

# Helper function to create Random Forest models
create_rf_model <- function(k, data, clusters, seed = 111) {
  set.seed(seed)
  randomForest(
    x = data[1:2047, 46:55],
    y = as.factor(clusters),
    ntree = 10000,
    mtry = 6,
    importance = FALSE,
    proximity = FALSE,
    nodesize = 1,
    keep.forest = TRUE,
    do.trace = FALSE
  )
}

PC.models <- lapply(2:20, function(k) {
  if (k %% 5 == 0) cat("  - Creating model for k =", k, "\n")
  create_rf_model(k, inputvars.covnetps, pred.part.list[[k]])
})
names(PC.models) <- paste0("PC", 2:20)
list2env(PC.models, envir = .GlobalEnv)

cat("All Random Forest models created\n\n")

# Tune mtry parameter
pdf("Result/simplified_version/tuneRF_plot.pdf", 
    width = 10, height = 7.5)
tuneRF(inputvars.covnetps[1:2047, 46:55], pred.part.10, 
       main = "mtry Tuning for 10-cluster Solution")
dev.off()

# ============================================================================
# STEP 6: Calculate Error Metrics
# ============================================================================

cat("Calculating prediction error metrics...\n")

# Helper function to extract error metrics
extract_error_metrics <- function(model, k) {
  error_rates <- model$confusion[, k + 1]
  cluster_sizes <- table(pred.part.list[[k]])
  total_misclass <- sum(error_rates * cluster_sizes)
  
  list(
    error_rates = error_rates,
    cluster_sizes = cluster_sizes,
    total_misclass = total_misclass
  )
}

metrics <- lapply(2:20, function(k) {
  extract_error_metrics(PC.models[[k - 1]], k)
})
names(metrics) <- paste0("PC", 2:20)

total_misclassified <- sapply(metrics, function(x) x$total_misclass)

# Calculate cluster counts by error threshold
calculate_cluster_counts <- function(threshold) {
  sapply(2:20, function(k) {
    sum((metrics[[k - 1]]$error_rates < threshold) * metrics[[k - 1]]$cluster_sizes)
  })
}

cluster_15_error <- calculate_cluster_counts(0.15)
cluster_20_error <- calculate_cluster_counts(0.20)
cluster_25_error <- calculate_cluster_counts(0.25)
cluster_30_error <- calculate_cluster_counts(0.30)

data <- data.frame(
  total_misclassified = total_misclassified,
  n_cluster_less20_error = cluster_20_error,
  n_cluster_less30_error = cluster_30_error
)

# Plot error metrics
pdf("Result/simplified_version/Fig_4_Prediction_Accuracy.pdf", 
    width = 10, height = 7.5)
matplot(data, type = "l", pch = 1, col = 1:5, 
        xlab = "Number of Clusters", ylab = "Observations",
        main = "Fig 4. Random Forest Prediction Accuracy by Number of Clusters", 
        cex = 0.6, lwd = 2)
legend("left", legend = names(data), col = 1:5, pch = 1, cex = 0.8, lwd = 2)
dev.off()

data$number_cluster <- 2:20

cat("Error metrics calculated and plotted\n\n")

# ============================================================================
# STEP 7: Analyze 8-Cluster Solution
# PS: The number of 8-cluster is determined from Fig 4.
# ============================================================================

cat("Analyzing 8-cluster solution...\n")

input.covnetps <- inputvars.covnetps[1:2047, ]

for (i in 1:ncol(input.covnetps)) {
  input.covnetps[, i] <- as.numeric(unlist(input.covnetps[, i]))
}

input.covnetps$n_cluster_8 <- as.numeric(pred.part.8)

# Filter clusters with error < 0.2
PC8E <- metrics$PC8$error_rates
PC8N <- metrics$PC8$cluster_sizes
clusters_to_keep_8 <- (PC8E < 0.2) * PC8N
exist8 <- as.numeric(names(clusters_to_keep_8)[clusters_to_keep_8 != 0])
count8 <- as.vector(clusters_to_keep_8[clusters_to_keep_8 != 0])
select8 <- input.covnetps$n_cluster_8 %in% exist8
data8 <- input.covnetps[select8, ]

cat("Clusters retained (error < 0.2):", length(exist8), "out of 8\n")
cat("Cluster IDs:", exist8, "\n")
cat("Cluster sizes:", count8, "\n\n")

# Calculate z-scored means
agg_8_list <- aggregate(data8, 
                        by = list(as.factor(data8$n_cluster_8)), 
                        FUN = list)

agg_new_8 <- agg_8_list[FALSE, ]
for (i in 1:ncol(agg_new_8)) {
  agg_new_8[, names(agg_new_8)[i]] <- as.numeric(unlist(agg_new_8[, names(agg_new_8)[i]]))
}

for (i in 1:8) {
  for (j in 2:ncol(agg_8_list)) {
    var_values <- unlist(agg_8_list[, names(agg_new_8)[j]][i])
    pop_mean <- mean(unlist(input.covnetps[, names(agg_new_8)[j]]))
    pop_sd <- sd(unlist(input.covnetps[, names(agg_new_8)[j]]))
    agg_new_8[i, j] <- mean((var_values - pop_mean) / pop_sd)
  }
}

agg_new_8 <- agg_new_8[!is.na(agg_new_8$egoid), ]
agg_new_8$Group.1 <- agg_8_list$Group.1
names(agg_new_8)[1] <- "cluster"
rownames(agg_new_8) <- agg_new_8$cluster
agg_new_8$n_observations <- count8
agg_new_8$prediction_error <- PC8E[PC8E < 0.2]

mean_8 <- data.frame(t(subset(agg_new_8, select = -c(wave, egoid, n_cluster_8))))
mean_8 <- add_column(mean_8, variables = rownames(mean_8), .before = "X1")

write_xlsx(mean_8, "Result/simplified_version/Mean_zscore_table_wave1.xlsx")

cat("Z-scored means calculated and saved\n\n")

# ============================================================================
# STEP 8: Create Radar Charts
# ============================================================================

cat("Creating radar charts...\n")

# Prepare data for radar charts
n <- mean_8$variables
mean_8t <- as.data.frame(t(mean_8[, -1]))
colnames(mean_8t) <- n

mean_8n <- mean_8t[c(1, 2), ]
mean_8n[1, ] <- rep(1.5, 57)
mean_8n[2, ] <- rep(-1.5, 57)

mean_8_data <- rbind(mean_8n, mean_8t)
for (i in 1:ncol(mean_8_data)) {
  mean_8_data[, i] <- as.numeric(mean_8_data[, i])
}

# Define the desired order of composite variables (clockwise from top)
desired_order <- c(
  "kin_support",
  "nonkin_support",
  "homophily", 
  "school_based",   
  "nonkin_distance",    
  "nonkin_tie_strength",  
  "outdoor_activities", 
  "nonkin_online",  
  "kin_involvement",     
  "work_based"
)

# Reorder the data columns to match desired order
mean_8_data_ordered <- mean_8_data[, desired_order]

# Create beautiful radar chart function
create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 1,
                                        caxislabels = NULL, title = NULL, ...) {
  radarchart(
    data, axistype = 1,
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    axislabcol = "black",
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}

# Define colors
colors <- c("#E74C3C", "#F39C12", "#F1C40F", "#2ECC71", 
            "#5DADE2", "#2980B9", "#8E44AD", "#95A5A6")

# Define cluster names
cluster_names <- c("Family", "Friends", "Restricted", 
                   "Family&Community", "School&Career",
                   "Homebody", "Just Activity", "Other")

# Calculate dynamic percentages from actual cluster sizes
cluster_sizes <- agg_new_8$n_observations
total_obs <- sum(cluster_sizes)
cluster_percentages <- round(100 * cluster_sizes / total_obs, 1)

# Generate dynamic titles with calculated percentages
titles <- paste0(cluster_names[1:length(cluster_sizes)], 
                 " (", cluster_percentages, "%)")

cat("Cluster distribution:\n")
for (i in 1:length(titles)) {
  cat("  Cluster", i, ":", titles[i], "- n =", cluster_sizes[i], "\n")
}
cat("\n")

# Create radar charts (PNG for better rendering)
png("Result/simplified_version/Fig_5_Radar_Charts.png", 
    width = 6000, height = 2500, res = 300)
op <- par(mar = c(1, 1, 1, 1))
par(mfrow = c(2, 4))
for (i in 1:length(cluster_sizes)) {
  create_beautiful_radarchart(
    data = mean_8_data_ordered[c(1:2, i + 2), ], 
    caxislabels = c(-1.5, -0.75, 0, 0.75, 1.5),
    color = colors[i], 
    title = titles[i]
  )
}
par(op)
dev.off()

cat("Radar charts saved\n\n")

# ============================================================================
# STEP 9: Prediction on Wave 2 Data
# ============================================================================

cat("Predicting clusters for Wave 2 data...\n")

inputvars.covnetps[1:2047, "cluster_ID"] <- as.numeric(pred.part.8)
RFN_pred_8 <- predict(PC8, inputvars.covnetps[2048:2571, 46:55])
inputvars.covnetps <- as.data.frame(inputvars.covnetps)
inputvars.covnetps[2048:2571, "cluster_ID"] <- RFN_pred_8

cat("Wave 2 predictions completed\n")
cat("Wave 2 cluster distribution:\n")
print(table(RFN_pred_8))
cat("\n")

# Analyze Wave 2 clusters
input.covnetps2 <- inputvars.covnetps[2048:2571, ]

for (i in 1:ncol(input.covnetps2)) {
  input.covnetps2[, i] <- as.numeric(unlist(input.covnetps2[, i]))
}

agg2_8_list <- aggregate(input.covnetps2,
                         by = list(as.factor(inputvars.covnetps[2048:2571, ]$cluster_ID)),
                         FUN = list)

agg_new2_8 <- agg2_8_list[FALSE, ]
for (i in 1:ncol(agg_new2_8)) {
  agg_new2_8[, names(agg_new2_8)[i]] <- as.numeric(unlist(agg_new2_8[, names(agg_new2_8)[i]]))
}

for (i in 1:8) {
  for (j in 2:ncol(agg2_8_list)) {
    var_values <- unlist(agg2_8_list[, names(agg_new2_8)[j]][i])
    pop_mean <- mean(unlist(input.covnetps2[, names(agg_new2_8)[j]]))
    pop_sd <- sd(unlist(input.covnetps2[, names(agg_new2_8)[j]]))
    agg_new2_8[i, j] <- mean((var_values - pop_mean) / pop_sd)
  }
}

agg_new2_8 <- agg_new2_8[!is.na(agg_new2_8$egoid), ]
agg_new2_8$Group.1 <- agg2_8_list$Group.1
names(agg_new2_8)[1] <- "cluster"
rownames(agg_new2_8) <- agg_new2_8$cluster
agg_new2_8$n_observations <- table(RFN_pred_8)[1:8]

mean2_8 <- data.frame(t(subset(agg_new2_8, select = -c(wave, egoid, cluster_ID))))
mean2_8 <- add_column(mean2_8, variables = rownames(mean2_8), .before = "X1")

write_xlsx(mean2_8, "Result/simplified_version/Mean_zscore_table_wave2.xlsx")

cat("Wave 2 analysis completed and saved\n\n")

# ============================================================================
# STEP 10: Save Final Data for Analysis
# ============================================================================

cat("Saving final dataset...\n")

original <- read_xlsx("Second&firstwave.xlsx", sheet = 1, guess_max = 10000)
inputvars.covnetps.bind <- subset(inputvars.covnetps, select = -c(wave, egoid))

final.inputvars.covnetps <- cbind(original, inputvars.covnetps.bind)

# Save in multiple formats
save(final.inputvars.covnetps, file = "Result/simplified_version/cov_netps_nettype_0612.Rdata")
write_csv(final.inputvars.covnetps, file = "Result/simplified_version/cov_netps_nettype_0612.csv")
write_dta(final.inputvars.covnetps, path = "Result/simplified_version/cov_netps_nettype_0612.dta")

cat("Final dataset saved in multiple formats\n\n")

# ============================================================================
# Summary Statistics
# ============================================================================

cat("\n========================================\n")
cat("ANALYSIS COMPLETE - SUMMARY\n")
cat("========================================\n\n")

cat("Output directory: Result/simplified_version/\n\n")

cat("Generated files:\n")
cat("  PLOTS (PDF/PNG):\n")
cat("    - Fig_1_Composite_Variable_Importance.pdf\n")
cat("    - Proximity_Density.pdf\n")
cat("    - Fig_2_Variable_Clusters.pdf\n")
cat("    - Fig_3_Clustree_Plot.png\n")
cat("    - tuneRF_plot.pdf\n")
cat("    - Fig_4_Prediction_Accuracy.pdf\n")
cat("    - Fig_5_Radar_Charts.png\n\n")

cat("  DATA TABLES:\n")
cat("    - Mean_zscore_table_wave1.xlsx (Wave 1 cluster profiles)\n")
cat("    - Mean_zscore_table_wave2.xlsx (Wave 2 cluster profiles)\n\n")

cat("  FINAL DATASETS:\n")
cat("    - final_clustered_data.Rdata\n")
cat("    - final_clustered_data.csv\n")
cat("    - final_clustered_data.dta\n\n")

cat("Key results:\n")
cat("  - Number of clusters: 8\n")
cat("  - Clusters retained (error < 0.2):", length(exist8), "\n")
cat("  - Total Wave 1 observations analyzed:", sum(cluster_sizes), "\n")
cat("  - Total Wave 2 observations predicted:", nrow(input.covnetps2), "\n\n")

cat("Cluster distribution (Wave 1):\n")
for (i in 1:length(cluster_sizes)) {
  cat(sprintf("  %s: %d (%.1f%%)\n", 
              cluster_names[i], 
              cluster_sizes[i], 
              cluster_percentages[i]))
}

cat("\n========================================\n")
cat("Analysis workflow:\n")
cat("  1. Variable clustering stability analysis\n")
cat("  2. Created 10 PCA composite variables\n")
cat("  3. Unsupervised Random Forest on composites\n")
cat("  4. Spectral clustering (k=1-20)\n")
cat("  5. Supervised RF models for validation\n")
cat("  6. Error metrics and cluster filtering\n")
cat("  7. Selected 8-cluster solution analysis\n")
cat("  8. Generated radar chart visualizations\n")
cat("  9. Predicted Wave 2 cluster membership\n")
cat(" 10. Saved final analytical dataset\n")
cat("========================================\n\n")

# ============================================================================
# End of Script
# ============================================================================