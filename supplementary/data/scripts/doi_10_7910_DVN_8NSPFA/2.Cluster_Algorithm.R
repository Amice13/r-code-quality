# ============================================================================
# Clustering Algorithm - Original Version
# Unsupervised Random Forest and Spectral Clustering Analysis
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

if (!dir.exists("Result/full_version")) {
  dir.create("Result/full_version", recursive = TRUE)
}

# ============================================================================
# STEP 1: Load Data and Initial Unsupervised Random Forest
# ============================================================================

# Load data
inputvars.covnetps <- read_excel("covnetps_forRF.xlsx")
inputvars.covnetps$hasPartner_i <- as.factor(inputvars.covnetps$hasPartner_i)
inputvars.covnetps$edu_status_c <- as.factor(inputvars.covnetps$edu_status_c)

# Unsupervised Random Forest
set.seed(111)
RFN <- randomForest(
  inputvars.covnetps[1:2047, 3:45],
  ntree = 10000,
  mtry = 6,
  importance = TRUE,
  proximity = TRUE,
  nodesize = 10,
  keep.forest = TRUE,
  do.trace = FALSE
)

# Save variable importance plot (PDF for publication quality)
pdf("Result/full_version/Fig_B1_Variable_Importance.pdf", 
    width = 10, height = 7.5)
varImpPlot(RFN, n.var = 43, cex = 0.55, 
           main = "Fig B1. Performance of Random Forest of 45 variables")
dev.off()

# Save proximity density plot
pdf("Result/full_version/Proximity_Density.pdf", 
    width = 10, height = 7.5)
plot(density(RFN$proximity))
dev.off()

RFN_dist <- as.dist(1 - RFN$proximity)

# ============================================================================
# STEP 2: Agglomerative Clustering
# ============================================================================

HC <- agnes(RFN_dist, method = "gaverage", diss = TRUE)
pred.part <- agnes(RFN_dist, method = "gaverage", diss = TRUE)

# Generate cluster partitions (1 to 50)
pred.part.list <- lapply(1:50, function(k) {
  cutree(pred.part, k = k)
})
names(pred.part.list) <- paste0("pred.part.", 1:50)
list2env(pred.part.list, envir = .GlobalEnv)

# ============================================================================
# STEP 3: First Random Forest - Predict Cluster Assignments
# ============================================================================

# Helper function to create Random Forest models
create_rf_model <- function(k, data, clusters, seed = 111) {
  set.seed(seed)
  randomForest(
    x = data[1:2047, 3:45],
    y = as.factor(clusters),
    ntree = 10000,
    mtry = 6,
    importance = FALSE,
    proximity = FALSE,
    nodesize = 1,
    keep.forest = FALSE,
    do.trace = FALSE
  )
}

# Create models for k = 2 to 50
PC.models <- lapply(2:50, function(k) {
  cat("Creating RF model for k =", k, "\n")
  create_rf_model(k, inputvars.covnetps, pred.part.list[[k]])
})
names(PC.models) <- paste0("PC", 2:50)
list2env(PC.models, envir = .GlobalEnv)

# Tune mtry parameter
pdf("Result/full_version/tuneRF_plot.pdf", 
    width = 10, height = 7.5)
tuneRF(inputvars.covnetps[1:2047, 3:45], pred.part.10, mtryStart = 2)
dev.off()

# ============================================================================
# STEP 4: Calculate Error Metrics
# ============================================================================

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

# Calculate metrics for all models
metrics <- lapply(2:50, function(k) {
  extract_error_metrics(PC.models[[k - 1]], k)
})
names(metrics) <- paste0("PC", 2:50)

# Extract components
total_misclassified <- sapply(metrics, function(x) x$total_misclass)

# Calculate cluster counts by error threshold
calculate_cluster_counts <- function(threshold) {
  sapply(2:50, function(k) {
    sum((metrics[[k - 1]]$error_rates < threshold) * metrics[[k - 1]]$cluster_sizes)
  })
}

cluster_15_error <- calculate_cluster_counts(0.15)
cluster_20_error <- calculate_cluster_counts(0.20)
cluster_25_error <- calculate_cluster_counts(0.25)
cluster_30_error <- calculate_cluster_counts(0.30)

# Create summary data frame
data <- data.frame(
  total_misclassified = total_misclassified,
  n_cluster_less20_error = cluster_20_error,
  n_cluster_less25_error = cluster_25_error,
  n_cluster_less30_error = cluster_30_error
)

# Plot error metrics
pdf("Result/full_version/Fig_A1_Prediction_Accuracy.pdf", 
    width = 10, height = 7.5)
matplot(data, type = "l", pch = 1, col = 1:5, 
        xlab = "number of clusters", ylab = "observations",
        main = "Fig. A1: Random Forest Prediction Accuracy by Number of Clusters", 
        cex = 0.6)
legend("center", legend = names(data), col = 1:5, pch = 1, cex = 0.8)
dev.off()

# ============================================================================
# STEP 5: Analyze 32-Cluster Solution
# ============================================================================

input.covnetps2 <- read_excel("covnetps_forRF.xlsx")
input.covnetps2 <- input.covnetps2[1:2047, ]

# Convert to numeric
for (i in 1:ncol(input.covnetps2)) {
  input.covnetps2[, i] <- as.numeric(unlist(input.covnetps2[, i]))
}

input.covnetps2$n_cluster_32 <- as.numeric(pred.part.32)

# Filter clusters with error < 0.25
PC32E <- metrics$PC32$error_rates
PC32N <- metrics$PC32$cluster_sizes
clusters_to_keep <- (PC32E < 0.25) * PC32N
exist32 <- as.numeric(names(clusters_to_keep)[clusters_to_keep != 0])
count32 <- as.vector(clusters_to_keep[clusters_to_keep != 0])
select32 <- input.covnetps2$n_cluster_32 %in% exist32
data32 <- input.covnetps2[select32, ]

# Calculate z-scored means
agg_32_list <- aggregate(data32, 
                         by = list(as.factor(data32$n_cluster_32)), 
                         FUN = list)

# Initialize aggregated means
agg_new_32 <- agg_32_list[FALSE, ]
for (i in 1:ncol(agg_new_32)) {
  agg_new_32[, names(agg_new_32)[i]] <- as.numeric(unlist(agg_new_32[, names(agg_new_32)[i]]))
}

# Calculate z-scores
for (i in 1:32) {
  for (j in 2:ncol(agg_32_list)) {
    var_values <- unlist(agg_32_list[, names(agg_new_32)[j]][i])
    pop_mean <- mean(unlist(input.covnetps2[, names(agg_new_32)[j]]))
    pop_sd <- sd(unlist(input.covnetps2[, names(agg_new_32)[j]]))
    agg_new_32[i, j] <- mean((var_values - pop_mean) / pop_sd)
  }
}

agg_new_32 <- agg_new_32[!is.na(agg_new_32$egoid), ]
agg_new_32$Group.1 <- agg_32_list$Group.1
names(agg_new_32)[1] <- "cluster"
rownames(agg_new_32) <- agg_new_32$cluster
agg_new_32$n_observations <- count32
agg_new_32$prediction_error <- PC32E[PC32E < 0.25]

mean_32 <- data.frame(t(subset(agg_new_32, select = -c(wave, egoid, n_cluster_32))))
mean_32 <- add_column(mean_32, variables = rownames(mean_32), .before = "X1")

# Save mean z-score table
write_xlsx(mean_32, "Result/full_version/Mean_zscore_table.xlsx")

# Variable clustering stability
x.quanti <- as.data.frame(subset(data32, select = -c(wave, egoid, n_cluster_32, hasPartner_i, edu_status_c)))
x.quali <- data.frame(
  hasPartner_i = as.factor(data32$hasPartner_i),
  edu_status_c = as.factor(data32$edu_status_c)
)

tree <- hclustvar(x.quanti, x.quali)
set.seed(111)
stab <- stability(tree, B = 40)

pdf("Result/full_version/Fig_A2_Variable_Clusters.pdf", 
    width = 10, height = 7.5)
plot(stab, cex = 0.6, 
     main = "Fig. A2: Number of Variables Clusters by mean adjusted Rand criterion")
dev.off()

tree.cut <- cutreevar(tree, 11)

# Create, sort, and save cluster assignments
tree_cluster <- data.frame(
  Variable = names(tree.cut$cluster),
  Cluster = tree.cut$cluster
)
tree_cluster <- tree_cluster[order(tree_cluster$Cluster), ]
rownames(tree_cluster) <- NULL

write.csv(tree_cluster, "Result/full_version/Composite_variable_list.csv", row.names = FALSE)

# ============================================================================
# STEP 6: Create PCA Composite Variables
# ============================================================================

group_var <- inputvars.covnetps[, 3:45]

# Standardize variables
for (i in 1:ncol(group_var)) {
  group_var[, i] <- as.numeric(unlist(group_var[, i]))
  group_var[, i] <- scale(group_var[, i])
}

# Define variable groups
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

# Create PCA composite variables
for (group_name in names(variable_groups)) {
  vars <- variable_groups[[group_name]]
  data_subset <- group_var[, vars, drop = FALSE]
  pca_scores <- principal(data_subset, nfactors = 1, score = TRUE)$scores
  inputvars.covnetps[, group_name] <- pca_scores
}

# ============================================================================
# STEP 7: Second Random Forest with Composite Variables
# ============================================================================

set.seed(111)
RFN2 <- randomForest(
  inputvars.covnetps[1:2047, 46:55],
  ntree = 10000,
  mtry = 6,
  importance = TRUE,
  proximity = TRUE,
  nodesize = 10,
  keep.forest = TRUE,
  do.trace = FALSE
)

pdf("Result/full_version/Fig_B2_Composite_Variable_Importance.pdf", 
    width = 10, height = 7.5)
varImpPlot(RFN2, n.var = 10, cex = 0.6, 
           main = "Fig.B2 Performance of 10 Composite variables")
dev.off()

pdf("Result/full_version/Proximity_Density_2.pdf", 
    width = 10, height = 7.5)
plot(density(RFN2$proximity))
dev.off()

RFN2_dist <- as.dist(1 - RFN2$proximity)

# ============================================================================
# STEP 8: Spectral Clustering
# ============================================================================
set.seed(111)
pred2.part.list <- lapply(1:20, function(k) {
  spectralClustering(RFN2$proximity, k = k)
})
names(pred2.part.list) <- paste0("pred2.part.", 1:20)
list2env(pred2.part.list, envir = .GlobalEnv)

HC2 <- agnes(RFN2_dist, method = "gaverage", diss = TRUE)
agg2.part.list <- lapply(1:20, function(k) {
  cutree(HC2, k = k)
})
names(agg2.part.list) <- paste0("agg2.part.", 1:20)
list2env(agg2.part.list, envir = .GlobalEnv)

# Spectral clustering tree
clust_spectral <- data.frame(
  c_1_clusters = pred2.part.1,
  c_2_clusters = pred2.part.2,
  c_3_clusters = pred2.part.3,
  c_4_clusters = pred2.part.4,
  c_5_clusters = pred2.part.5,
  c_6_clusters = pred2.part.6,
  c_7_clusters = pred2.part.7,
  c_8_clusters = pred2.part.8,
  c_9_clusters = pred2.part.9,
  c_10_clusters = pred2.part.10,
  c_11_clusters = pred2.part.11,
  c_12_clusters = pred2.part.12,
  c_13_clusters = pred2.part.13,
  c_14_clusters = pred2.part.14
)

# Agglomerative clustering tree
clust_agglom <- data.frame(
  c_1_clusters = agg2.part.1,
  c_2_clusters = agg2.part.2,
  c_3_clusters = agg2.part.3,
  c_4_clusters = agg2.part.4,
  c_5_clusters = agg2.part.5,
  c_6_clusters = agg2.part.6,
  c_7_clusters = agg2.part.7,
  c_8_clusters = agg2.part.8,
  c_9_clusters = agg2.part.9,
  c_10_clusters = agg2.part.10,
  c_11_clusters = agg2.part.11,
  c_12_clusters = agg2.part.12,
  c_13_clusters = agg2.part.13,
  c_14_clusters = agg2.part.14
)

png("Result/full_version/Clustree_Plot_Comparison.png", 
    width = 8000, height = 3200, res = 300)

p1 <- clustree(clust_spectral, prefix = "c_", suffix = "_clusters", edge_width = 0.75) +
  ggtitle("Spectral Clustering")

p2 <- clustree(clust_agglom, prefix = "c_", suffix = "_clusters", edge_width = 0.75) +
  ggtitle("Agglomerative Clustering")

print(p1 | p2)
dev.off()

# ============================================================================
# STEP 9: Second Random Forest Models
# ============================================================================

create_rf2_model <- function(k, data, clusters, seed = 111) {
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

P2C.models <- lapply(2:20, function(k) {
  cat("Creating RF2 model for k =", k, "\n")
  create_rf2_model(k, inputvars.covnetps, pred2.part.list[[k]])
})
names(P2C.models) <- paste0("P2C", 2:20)
list2env(P2C.models, envir = .GlobalEnv)

pdf("Result/full_version/tuneRF_plot2.pdf", 
    width = 10, height = 7.5)
tuneRF(inputvars.covnetps[1:2047, 46:55], pred2.part.10, main = "Hi")
dev.off()

# ============================================================================
# STEP 10: Calculate Error Metrics for Second RF
# ============================================================================

metrics2 <- lapply(2:20, function(k) {
  extract_error_metrics(P2C.models[[k - 1]], k)
})
names(metrics2) <- paste0("P2C", 2:20)

total_misclassified2 <- sapply(metrics2, function(x) x$total_misclass)

calculate_cluster_counts2 <- function(threshold) {
  sapply(2:20, function(k) {
    sum((metrics2[[k - 1]]$error_rates < threshold) * metrics2[[k - 1]]$cluster_sizes)
  })
}

cluster_15_error2 <- calculate_cluster_counts2(0.15)
cluster_20_error2 <- calculate_cluster_counts2(0.20)
cluster_25_error2 <- calculate_cluster_counts2(0.25)
cluster_30_error2 <- calculate_cluster_counts2(0.30)

data2 <- data.frame(
  total_misclassified = total_misclassified2,
  n_cluster_less20_error = cluster_20_error2,
  n_cluster_less30_error = cluster_30_error2
)

pdf("Result/full_version/Fig_A3_Prediction_Accuracy_2.pdf", 
    width = 10, height = 7.5)
matplot(data2, type = "l", pch = 1, col = 1:5, 
        xlab = "number of clusters", ylab = "observations",
        main = "Fig. A3: Random Forest Prediction Accuracy by Number of Clusters", 
        cex = 0.6)
legend("left", legend = names(data2), col = 1:5, pch = 1, cex = 0.8)
dev.off()

data2$number_cluster <- 2:20

# ============================================================================
# STEP 11: Analyze 8-Cluster Solution
# ============================================================================

input.covnetps3 <- inputvars.covnetps[1:2047, ]

for (i in 1:ncol(input.covnetps3)) {
  input.covnetps3[, i] <- as.numeric(unlist(input.covnetps3[, i]))
}

input.covnetps3$n_cluster_8 <- as.numeric(pred2.part.8)

# Filter clusters with error < 0.2
P2C8E <- metrics2$P2C8$error_rates
P2C8N <- metrics2$P2C8$cluster_sizes
clusters_to_keep_8 <- (P2C8E < 0.2) * P2C8N
exist8 <- as.numeric(names(clusters_to_keep_8)[clusters_to_keep_8 != 0])
count8 <- as.vector(clusters_to_keep_8[clusters_to_keep_8 != 0])
select8 <- input.covnetps3$n_cluster_8 %in% exist8
data8 <- input.covnetps3[select8, ]

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
    pop_mean <- mean(unlist(input.covnetps3[, names(agg_new_8)[j]]))
    pop_sd <- sd(unlist(input.covnetps3[, names(agg_new_8)[j]]))
    agg_new_8[i, j] <- mean((var_values - pop_mean) / pop_sd)
  }
}

agg_new_8 <- agg_new_8[!is.na(agg_new_8$egoid), ]
agg_new_8$Group.1 <- agg_8_list$Group.1
names(agg_new_8)[1] <- "cluster"
rownames(agg_new_8) <- agg_new_8$cluster
agg_new_8$n_observations <- count8
agg_new_8$prediction_error <- P2C8E[P2C8E < 0.2]

mean_8 <- data.frame(t(subset(agg_new_8, select = -c(wave, egoid, n_cluster_8))))
mean_8 <- add_column(mean_8, variables = rownames(mean_8), .before = "X1")

write_xlsx(mean_8, "Result/full_version/Mean_zscore_table2.xlsx")

# ============================================================================
# STEP 12: Create Radar Charts
# ============================================================================

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

# Define cluster names (adjust these to match your clusters)
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

# Create radar charts (kept as PNG for better rendering of complex graphics)
png("Result/full_version/Radar_Charts.png", 
    width = 5600, height = 3200, res = 300)
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

cat("\nRadar charts saved to Result/full_version/Radar_Charts.png\n")

# ============================================================================
# STEP 13: Prediction on Wave 2 Data
# ============================================================================

inputvars.covnetps[1:2047, "cluster_ID"] <- as.numeric(pred2.part.8)
RFN_pred_8 <- predict(P2C8, inputvars.covnetps[2048:2571, 46:55])
inputvars.covnetps <- as.data.frame(inputvars.covnetps)
inputvars.covnetps[2048:2571, "cluster_ID"] <- RFN_pred_8

# Analyze Wave 2 clusters
input.covnetps4 <- inputvars.covnetps[2048:2571, ]

for (i in 1:ncol(input.covnetps4)) {
  input.covnetps4[, i] <- as.numeric(unlist(input.covnetps4[, i]))
}

agg2_8_list <- aggregate(input.covnetps4,
                         by = list(as.factor(inputvars.covnetps[2048:2571, ]$cluster_ID)),
                         FUN = list)

agg_new2_8 <- agg2_8_list[FALSE, ]
for (i in 1:ncol(agg_new2_8)) {
  agg_new2_8[, names(agg_new2_8)[i]] <- as.numeric(unlist(agg_new2_8[, names(agg_new2_8)[i]]))
}

for (i in 1:8) {
  for (j in 2:ncol(agg2_8_list)) {
    var_values <- unlist(agg2_8_list[, names(agg_new2_8)[j]][i])
    pop_mean <- mean(unlist(input.covnetps4[, names(agg_new2_8)[j]]))
    pop_sd <- sd(unlist(input.covnetps4[, names(agg_new2_8)[j]]))
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

write_xlsx(mean2_8, "Result/full_version/Mean_zscore_table3.xlsx")


# ============================================================================
# STEP 14: Save Final Data for Analysis
# ============================================================================

original <- read_xlsx("Second&firstwave.xlsx", sheet = 1, guess_max = 10000)
inputvars.covnetps.bind <- subset(inputvars.covnetps, select = -c(wave, egoid))

final.inputvars.covnetps <- cbind(original, inputvars.covnetps.bind)

# Save in multiple formats
save(final.inputvars.covnetps, file = "Result/full_version/cov_netps_nettype_0612.Rdata")
write_csv(final.inputvars.covnetps, file = "Result/full_version/cov_netps_nettype_0612.csv")
write_dta(final.inputvars.covnetps, path = "Result/full_version/cov_netps_nettype_0612.dta")

cat("\n=== Analysis Complete ===\n")
cat("All results saved to 'Result/full_version' folder\n")
cat("\nOutput format summary:\n")
cat("  - Most plots: PDF (vector graphics, publication quality)\n")
cat("  - Clustree plot: PNG at 300 dpi (better rendering)\n")
cat("  - Radar charts: PNG at 300 dpi (better rendering)\n")
cat("  - Data tables: XLSX format\n")
cat("  - Final dataset: .Rdata, .csv, and .dta formats\n")

# ============================================================================
# End of Script
# ============================================================================