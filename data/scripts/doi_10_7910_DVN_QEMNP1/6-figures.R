#########################################################
# Policy Diffusion: The Issue-Definition Stage
# Fabrizio Gilardi, Charles R. Shipan & Bruno Wueest
# Make figures
# 2019-11-18
#########################################################


rm(list = ls())
setwd("/Users/fgilardi/Documents/AJPS/AJPS-final/")

# library(devtools)
library(ggplot2)
library(reshape2)
library(igraph)
library(GGally)
library(intergraph)

# sessionInfo()
# R version 3.6.1 (2019-07-05)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS Catalina 10.15.1

# Matrix products: default
# BLAS:   /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRblas.0.dylib
# LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib

# locale:
# [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     

# other attached packages:
# [1] GGally_1.4.0   igraph_1.2.4.1 reshape2_1.4.3 ggplot2_3.2.1 intergraph_2.0-2

# loaded via a namespace (and not attached):
#  [1] Rcpp_1.0.3         magrittr_1.5       tidyselect_0.2.5   munsell_0.5.0     
#  [5] colorspace_1.4-1   R6_2.4.1           rlang_0.4.1        stringr_1.4.0     
#  [9] plyr_1.8.4         dplyr_0.8.3        tools_3.6.1        grid_3.6.1        
# [13] gtable_0.3.0       withr_2.1.2        lazyeval_0.2.2     assertthat_0.2.1  
# [17] tibble_2.1.3       crayon_1.3.4       purrr_0.3.3        RColorBrewer_1.1-2
# [21] glue_1.3.1         stringi_1.4.3      compiler_3.6.1     pillar_1.4.2      
# [25] scales_1.0.0       reshape_0.8.8      pkgconfig_2.0.3   


load("./corpus/corpus_txt.RData")
load("./output/STM_model_network.RData")

path_output <- c("./output/")
path_figures <- c("./figures/")

# Topic labels

topic_labels <- c(
  "HEALTH\nHealth", #1 Health
  "REGULATIONS\nRules", #2 Regulations
  "NORMATIVE\nFreedom", #3 Normative
  "SPACES\nOutdoors", #4 Spaces
  "SPACES\nSchools and universities", #5 Spaces
  "REGULATIONS\nLocal legislation", #6 Regulations
  "CASINO LEGISLATION\nState legislation", #7 Casinos legislation
  "POLITICS\nElectoral politics", #8 Politics
  "REGULATIONS\nEnforcement", #9 Regulations
  "INTEREST GROUPS\nTobacco companies", #10 Interest groups
  "REGULATIONS\nBars and restaurants", #11 Regulations
  "CASINO LEGISLATION\nCasinos" #12 Casinos legislation
)

topic_labels_2 <- c(
  "HEALTH: Health", #1 Health
  "REGULATIONS: Rules", #2 Regulations
  "NORMATIVE: Freedom", #3 Normative
  "SPACES: Outdoors", #4 Spaces
  "SPACES: Schools and universities", #5 Spaces
  "REGULATIONS: Local legislation", #6 Regulations
  "CASINO LEGISLATION: State legislation", #7 Casinos legislation
  "POLITICS: Electoral politics", #8 Politics
  "REGULATIONS: Enforcement", #9 Regulations
  "INTEREST GROUPS: Tobacco companies", #10 Interest groups
  "REGULATIONS: Bars and restaurants", #11 Regulations
  "CASINO LEGISLATION: Casinos" #12 Casinos legislation
)

topic_groups_labels <- c(
  "Health", #1
  "Regulations", #2
  "Normative", #3
  "Spaces", #4
  "Spaces", #5
  "Regulations", #6
  "Casino legislation", #7
  "Politics", #8
  "Regulations", #9
  "Interest groups", #10
  "Regulations", #11
  "Casino legislation" #12
)

vlabels <- c(
  "Health", #1
  "Rules", #2
  "Freedom", #3
  "Outdoors", #4
  "Schools and\nuniversities", #5
  "Local\nlegislation", #6
  "State\nlegislation", #7
  "Electoral\npolitics", #8
  "Enforcement", #9
  "Tobacco\ncompanies", #10
  "Bars and\nrestaurants", #11
  "Casinos" #12
)

var_labels_long <- c(
    "Months",
    "Smoking ban was enacted this month",
    "Months before/after smoking ban was enacted",
    "Share of prior policy adoptions within a state's diffusion network",
    "Tobacco-producer state",
    "Pro sentiment",
    "Share Democrats in state lower house",
    "Media slant (larger = more conservative)",
    "Percent smokers within a state",
    "Unified Democratic government",
    "Unified Republican government"
)

ord <- c(
  5, # 1 Health
  1, # 2 Regulations
  6, # 3 Freedom
  12, # 4 Outdoors 
  11, # 5 Schools
  4, # 6 Local
  8, # 7 State
  9, # 8 Electoral
  2, # 9 Enforcement
  10, # 10 Tobacco
  3, # 11 Bars
  7 # 12 Casinos
  )

#load key figures for words
scores <- read.delim2(paste0(path_output, "scores_network.txt"))
scores <- data.frame(scores)
co <- ncol(scores) - 1
for(i in 1:co){
    scores[,i] <- as.numeric(levels(scores[,i]))[scores[,i]]
    stopifnot(is.numeric(scores[,i]) == TRUE)	
}

# load words
words <- read.delim2(paste0(path_output, "words_network.txt"))
  
# get indices for key numbers in scores
pr <- seq(1,co/3)
ex <- seq(max(pr) + 1, max(pr) + co/3)
fr <- seq(max(ex) + 1, max(ex) + co/3)

# small multiple word plot
n <- 50
scores_pr <- melt(scores[,c(min(pr):max(pr), co + 1)], id = c("vocab"))
scores_pr$topic_nr <- NA
scores_pr$topic_label <- "NA"
scores_pr$topic_groups_label <- "NA"
for(i in 1:ncol(words)){
  scores_pr$topic_nr[scores_pr$variable == paste("probability_", i, sep = "")] <- i
  scores_pr$topic_label[scores_pr$variable == paste("probability_", i, sep = "")] <- topic_labels[i]
  scores_pr$topic_groups_label[scores_pr$variable == paste("probability_", i, sep = "")] <- topic_groups_labels[i]
}
scores_pr$variable <- c("Probability")
colnames(scores_pr) <- c("Words", "Variable", "Probability", "Topic_Nr", "Topic", "Group")

scores_ex <- melt(scores[,c(min(ex):max(ex), co + 1)], id = c("vocab"))
scores_ex$topic_nr <- NA
scores_ex$topic_label <- "NA"
scores_ex$topic_groups_label <- "NA"
for(i in 1:ncol(words)){
  scores_ex$topic_nr[scores_ex$variable == paste("exclusivity_", i, sep = "")] <- i
  scores_ex$topic_label[scores_ex$variable == paste("exclusivity_", i, sep = "")] <- topic_labels[i]
  scores_ex$topic_groups_label[scores_ex$variable == paste("probability_", i, sep = "")] <- topic_groups_labels[i]
}
scores_ex$variable <- c("Exclusivity")
colnames(scores_ex) <- c("Words", "Variable", "Exclusivity", "Topic_Nr", "Topic", "Group")

scores_fr <- melt(scores[,c(min(fr):max(fr), co + 1)], id = c("vocab"))
scores_fr$topic_nr <- NA
scores_fr$topic_label <- "NA"
scores_fr$topic_groups_label <- "NA"
for(i in 1:ncol(words)){
  scores_fr$topic_nr[scores_fr$variable == paste("frequency_", i, sep = "")] <- i
  scores_fr$topic_label[scores_fr$variable == paste("frequency_", i, sep = "")] <- topic_labels[i]
  scores_fr$topic_groups_label[scores_fr$variable == paste("probability_", i, sep = "")] <- topic_groups_labels[i]
}
scores_fr$variable <- c("Frequency")
colnames(scores_fr) <- c("Words", "Variable", "Frequency", "Topic_Nr", "Topic", "Group")

scores_long <- cbind(scores_pr, scores_ex, scores_fr)
scores_long <- scores_long[,c(1,3,9,15,4,5,6)]

scores_long$Top_Words <- NA
for(i in 1:ncol(words)){
  wo <- scores_long$Words[scores_long$Topic_Nr == i]
  top <- words[1:n,i]
  scores_long$Top_Words[scores_long$Topic_Nr == i] <- wo %in% top
}

scores_long$Topic <- factor(scores_long$Topic, levels = unique(scores_long$Topic[order(scores_long$Group, decreasing = FALSE)]))

scores_long$Topic <- factor(scores_long$Topic, levels = topic_labels[order(ord)])

# -----------------------------------------------------------------------------
# Figure 1
# -----------------------------------------------------------------------------

file_name <- "month_network.txt" #fns_network[1]
var_lab <- var_labels_long[1]

d <- data.frame(read.delim(paste(path_output, "/", file_name, sep = "")))
d <- d[order(d$topic, d$month),]
mo <- seq(as.Date("1996-01-01"), by = "month", length.out = max(d$month))
d$date <- rep(mo, length(topic_labels))

var_label <- colnames(d)[5]
colnames(d)[5] <- c("var")

d$topicLabels <- NA
d$pointEstimateAverage <- NA

for(j in 1:length(topic_labels)){
  d$topicLabels[d$topic == j] <- topic_labels[j]
  d$pointEstimateAverage[d$topic == j] <- mean(d$pointEstimate[d$topic == j])
}

d$groupLabels <- NA
for(j in 1:length(topic_groups_labels)){
  d$groupLabels[d$topic == j] <- topic_groups_labels[j]
}

d$topicLabels <- factor(d$topicLabels, levels = unique(d$topicLabels[order(d$pointEstimateAverage, decreasing = TRUE)]))

# Color
ggplot(data = d, aes(x = date, y = pointEstimate)) +
    geom_line(aes(color = groupLabels)) +
    geom_hline(aes(yintercept = pointEstimateAverage, color = groupLabels)) +
    facet_wrap(~ topicLabels, ncol = 3) +
    labs(x = "Year", y = "Topic prevalence", title = "") +
    theme_light() + 
    theme(legend.position = "bottom", legend.title = element_blank(), legend.key.size = unit(0.5, "cm"), strip.background = element_blank(), strip.text = element_text(color = "black")) +
    scale_alpha(guide = "none") +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    geom_ribbon(aes(ymin = lb, ymax= ub, fill = groupLabels, alpha = 0.5))
ggsave(file = paste(path_figures, "Figure-1-color.pdf", sep = ""), width = 7, height = 8.5)

# Grayscale
ggplot(data = d, aes(x = date, y = pointEstimate)) +
    geom_line() +
    geom_hline(aes(yintercept = pointEstimateAverage)) +
    facet_wrap(~ topicLabels, ncol = 3) +
    labs(x = "Year", y = "Topic prevalence", title = "") +
    theme_light() + 
    theme(legend.position = "bottom", legend.title = element_blank(), legend.key.size = unit(0.5, "cm"), strip.background = element_blank(), strip.text = element_text(color = "black")) +
    scale_alpha(guide = "none") +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    geom_ribbon(aes(ymin = lb, ymax= ub, alpha = 0.15))
ggsave(file = paste(path_figures, "Figure-1-grayscale.pdf", sep = ""), width = 7, height = 8.5)


# -----------------------------------------------------------------------------
# Figure 2
# -----------------------------------------------------------------------------

file_name <- "policySpatialLag_network.txt" #fns_network[4]
var_lab <- var_labels_long[4]

d <- data.frame(read.delim(paste(path_output, "/", file_name, sep = "")))
var_label <- colnames(d)[5]
colnames(d)[5] <- c("var")

d$topicLabels <- NA
d$slope <- NA

for(j in 1:length(topic_labels)){
  d$topicLabels[d$topic == j] <- topic_labels[j]

  x1 <- min(d$var[d$topic == j])
  x2 <- max(d$var[d$topic == j])
  y1 <- d$pointEstimate[d$topic == j & d$var == x1]
  y2 <- d$pointEstimate[d$topic == j & d$var == x2]

  d$slope[d$topic == j] <- (y2 - y1)/(x2 - x1)

}

d$groupLabels <- NA
for(j in 1:length(topic_groups_labels)){
  d$groupLabels[d$topic == j] <- topic_groups_labels[j]
}

d$topicLabels <- factor(d$topicLabels, levels = topic_labels[order(ord)])


# Color
ggplot(data = d, aes(x = var, y = pointEstimate)) +
    geom_line(aes(color = groupLabels)) +
    facet_wrap(~ topicLabels, ncol = 3) +
    labs(x = paste(var_lab), y = "Topic prevalence", title = "") +
    scale_alpha(guide = "none") +
    scale_x_continuous(breaks = seq(0, 1, 0.2)) +
    geom_ribbon(aes(ymin = lb, ymax= ub, fill = groupLabels, alpha = 0.5)) +
    theme_light() + 
    theme(legend.position = "bottom", legend.title = element_blank(), legend.key.size = unit(0.5, "cm"), strip.background = element_blank(), strip.text = element_text(color = "black"))
ggsave(file = paste(path_figures, "Figure-2-color.pdf", sep = ""), width = 7, height = 8.5)

# Grayscale
ggplot(data = d, aes(x = var, y = pointEstimate)) +
    geom_line() +
    facet_wrap(~ topicLabels, ncol = 3) +
    labs(x = paste(var_lab), y = "Topic prevalence", title = "") +
    scale_alpha(guide = "none") +
    scale_x_continuous(breaks = seq(0, 1, 0.2), labels = c("0.00", "0.20", "0.40", "0.60", "0.80", "1.00")) +
    geom_ribbon(aes(ymin = lb, ymax= ub, alpha = 0.15)) +
    theme_light() + 
    theme(legend.position = "bottom", legend.title = element_blank(), legend.key.size = unit(0.5, "cm"), strip.background = element_blank(), strip.text = element_text(color = "black"))
ggsave(file = paste(path_figures, "Figure-2-grayscale.pdf", sep = ""), width = 7, height = 8.5)


# -----------------------------------------------------------------------------
# Figure 3 
# -----------------------------------------------------------------------------

cutoff <- 0

sp_lag <- corpus$meta$Policy_Spatial_Lag_Network_Pcent
thetas <- as.data.frame(STM$theta)

thetas$sp_lag <- sp_lag
thetas$half <- 0
thetas$half[thetas$sp_lag > 0.5] <- 1
thetas$sp_lag <- NULL

cormat <- cor(thetas[,1:12])
cormat_low <- cor(thetas[thetas$half == 0,1:12])
cormat_high <- cor(thetas[thetas$half == 1,1:12])

out <- list()
adjmat <- ifelse(cormat > cutoff,1,0)
out$posadj <- adjmat
out$poscor <- cormat*adjmat
out$cor <- ifelse(abs(cormat) > cutoff, cormat, 0)
class(out) <- "topicCorr"

out_low <- list()
adjmat <- ifelse(cormat_low > cutoff,1,0)
out_low$posadj <- adjmat
out_low$poscor <- cormat_low*adjmat
out_low$cor <- ifelse(abs(cormat_low)> cutoff, cormat_low, 0)

out_high <- list()
adjmat <- ifelse(cormat_high > cutoff,1,0)
out_high$posadj <- adjmat
out_high$poscor <- cormat_high*adjmat
out_high$cor <- ifelse(abs(cormat_high)> cutoff, cormat_high, 0)

x <- out
weights <- x$cor
diag(weights) <- 0
weights[weights <= 0] <- 0
weights <- as.numeric(weights)
weights <- weights[weights!=0]*100

set.seed(12345)
g <- graph.adjacency(out$posadj, mode = "directed", weighted = TRUE, diag = FALSE)

E(g)$width <- weights
E(g)$lty <- 1
E(g)$color <- "darkgrey"
V(g)$color <- "grey"
V(g)$label <- vlabels
V(g)$size <- colMeans(as.data.frame(thetas[,1:12]))*100

x <- out_low
weights <- x$cor
diag(weights) <- 0
weights[weights <= 0] <- 0
weights <- as.numeric(weights)
weights <- weights[weights!=0]*100

g_low <- graph.adjacency(out_low$posadj, mode="directed", weighted=TRUE, diag=FALSE)
E(g_low)$width <- weights
E(g_low)$lty <- 1
E(g_low)$color <- "darkgrey"
V(g_low)$color <- "grey"
V(g_low)$label <- vlabels
V(g_low)$size <- colMeans(as.data.frame(thetas[thetas$half == 0,1:12]))*100

x <- out_high
weights <- x$cor
diag(weights) <- 0
weights[weights <= 0] <- 0
weights <- as.numeric(weights)
weights <- weights[weights!=0]*100

g_high <- graph.adjacency(out_high$posadj, mode="directed", weighted=TRUE, diag=FALSE)
E(g_high)$width <- weights
E(g_high)$lty <- 1
E(g_high)$color <- "darkgrey"
V(g_high)$color <- "grey"
V(g_high)$label <- vlabels
V(g_high)$size <- colMeans(as.data.frame(thetas[thetas$half == 1,1:12]))*100

layout <- layout.fruchterman.reingold

set.seed(12345)
ggnet2(g, label = vlabels, edge.size = "width", layout.exp = 0.5) +
  labs(title = "OVERALL")
ggsave("./figures/Figure-3a.pdf", width = 7, height = 4.5)

set.seed(123)
ggnet2(g_low, label = vlabels, edge.size = "width", layout.exp = 0.5) +
  labs(title = "LESS THAN 50% OF OTHER STATES")
ggsave("./figures/Figure-3b.pdf", width = 7, height = 4.5)

set.seed(123)
ggnet2(g_high, label = vlabels, edge.size = "width", layout.exp = 0.5) +
  labs(title = "50% OF OTHER STATES OR MORE")
ggsave("./figures/Figure-3c.pdf", width = 7, height = 4.5)

# -----------------------------------------------------------------------------
# Figure C1
# -----------------------------------------------------------------------------

# Prerequisites
# 0. install dependencies
#   A. Xcode
#      a. open Terminal
#      b. enter <xcode-select --install>
#   B. X11
#      a. download and launch https://dl.bintray.com/xquartz/downloads/XQuartz-2.7.11.dmg
#      b. run the pkg installer
#   C. Homebrew
#      a. open Terminal
#      b. enter </usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)">
#   D. python 3.7.:
#      a. open Terminal
#      b. enter <brew install python3>
# 2. install python modules 
#   A. open Terminal
#   B. enter:
#      <pip3 install 'pandas==0.25.3'>
#      <pip3 install 'nltk==3.4.5'>
#      <pip3 install 'numpy==1.17.3'>
#      <pip3 install 'gensim==3.8.1'>
# 3. install nltk packages
#   A. open Terminal
#   B. enter <python3>
#   C. enter:
#      <import nltk>
#      <nltk.download('stopwords')>
#      <nltk.download('punkt')>
# 4. get full path of the right Python interpreter 
#   A. open Terminal
#   B. enter <python3>
#   C. enter:
#      <import sys>
#      <print(sys.executable)>
#   D. copy path and replace if necessary

# define path to Python interpreter
pythonPath <- "/usr/local/opt/python/bin/python3.7" 

# run the w2v model on the word lists
system2(pythonPath, args = "./evaluation/stm/coherence_discrimination.py")

# load and plot evaluation data
w2v <- read.csv("./evaluation/stm/stm-evaluation.txt", header = FALSE)
colnames(w2v) <- c("coherence", "coherence.discrimination", "k")

ggplot(w2v, aes(x=k, y=coherence.discrimination)) + 
  geom_point() +
  theme_bw() + 
  geom_smooth(method = "loess", colour = "black", span = 0.35)
ggsave(file = "./figures/Figure-C1.pdf", height=5, width=8.09)

# -----------------------------------------------------------------------------
# Figure C2
# -----------------------------------------------------------------------------

file_name <- "policySpatialLag_neighbors.txt" #fns_neighbors[4]
var_lab <- var_labels_long[4]

d <- data.frame(read.delim(paste(path_output, "/", file_name, sep = "")))
var_label <- colnames(d)[5]
colnames(d)[5] <- c("var")

d$topicLabels <- NA
d$slope <- NA

for(j in 1:length(topic_labels)){
  d$topicLabels[d$topic == j] <- topic_labels[j]

  x1 <- min(d$var[d$topic == j])
  x2 <- max(d$var[d$topic == j])
  y1 <- d$pointEstimate[d$topic == j & d$var == x1]
  y2 <- d$pointEstimate[d$topic == j & d$var == x2]

  d$slope[d$topic == j] <- (y2 - y1)/(x2 - x1)

}

d$groupLabels <- NA
for(j in 1:length(topic_groups_labels)){
  d$groupLabels[d$topic == j] <- topic_groups_labels[j]
}

d$topicLabels <- factor(d$topicLabels, levels = topic_labels[order(ord)])

ggplot(data = d, aes(x = var, y = pointEstimate)) +
    geom_line(aes(color = groupLabels)) +
    facet_wrap(~ topicLabels, ncol = 3) +
    labs(x = paste(var_lab), y = "Topic prevalence", title = "Neighbors") +
    scale_alpha(guide = "none") +
    scale_x_continuous(breaks = seq(0, 1, 0.2)) +
    geom_ribbon(aes(ymin = lb, ymax= ub, fill = groupLabels, alpha = 0.5)) +
    theme_light() + 
    theme(legend.position = "bottom", legend.title = element_blank(), legend.key.size = unit(0.5, "cm"), strip.background = element_blank(), strip.text = element_text(color = "black"))
ggsave(file = paste(path_figures, "/Figure-C2.pdf", sep = ""), width = 7, height = 8.5)


# -----------------------------------------------------------------------------
# Figure C3
# -----------------------------------------------------------------------------

file_name <- "policySpatialLag_all.txt" #fns_all[4]
var_lab <- var_labels_long[4]

d <- data.frame(read.delim(paste(path_output, "/", file_name, sep = "")))
var_label <- colnames(d)[5]
colnames(d)[5] <- c("var")

d$topicLabels <- NA
d$slope <- NA

for(j in 1:length(topic_labels)){
  d$topicLabels[d$topic == j] <- topic_labels[j]

  x1 <- min(d$var[d$topic == j])
  x2 <- max(d$var[d$topic == j])
  y1 <- d$pointEstimate[d$topic == j & d$var == x1]
  y2 <- d$pointEstimate[d$topic == j & d$var == x2]

  d$slope[d$topic == j] <- (y2 - y1)/(x2 - x1)

}

d$groupLabels <- NA
for(j in 1:length(topic_groups_labels)){
  d$groupLabels[d$topic == j] <- topic_groups_labels[j]
}

d$topicLabels <- factor(d$topicLabels, levels = topic_labels[order(ord)])

ggplot(data = d, aes(x = var, y = pointEstimate)) +
    geom_line(aes(color = groupLabels)) +
    facet_wrap(~ topicLabels, ncol = 3) +
    labs(x = paste(var_lab), y = "Topic prevalence", title = "All") +
    scale_alpha(guide = "none") +
    scale_x_continuous(breaks = seq(0, 1, 0.2)) +
    geom_ribbon(aes(ymin = lb, ymax= ub, fill = groupLabels, alpha = 0.5)) +
    theme_light() + 
    theme(legend.position = "bottom", legend.title = element_blank(), legend.key.size = unit(0.5, "cm"), strip.background = element_blank(), strip.text = element_text(color = "black"))
ggsave(file = paste(path_figures, "/Figure-C3.pdf", sep = ""), width = 7, height = 8.5)


# -----------------------------------------------------------------------------
# Figure C4
# -----------------------------------------------------------------------------

file_name <- "sentiment_difference_network.txt"
var_label_long <- "Pro sentiment"

d <- data.frame(read.delim(paste(path_output, file_name, sep = "")))
var_label <- colnames(d)[5]
colnames(d)[5] <- c("var")

#add topic labels
d$topicLabels <- NA
for(j in 1:length(topic_labels_2)){
  d$topicLabels[d$topic == j] <- topic_labels_2[j]
}

#add group labels
d$groupLabels <- NA
for(j in 1:length(topic_groups_labels)){
  d$groupLabels[d$topic == j] <- topic_groups_labels[j]
}

d$topicLabels <- factor(d$topicLabels, levels = unique(d$topicLabels[order(d$diff, decreasing = TRUE)]))

#d$topicLabels <- as.factor(d$topicLabels)[order(d$diff, decreasing = FALSE)]
d$var <- as.factor(d$var)

ggplot(data = d, aes(x = topicLabels, y = diff01, ymin = lb, ymax = ub, color = groupLabels)) +
  geom_pointrange() +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(d$topicLabels))) +
  geom_hline(yintercept = 0, lty = 2, size = 0.5) +
  labs(x = "", y = "Difference in topic prevalence (pro - anti or neutral)", title = "") +
  theme_light() + 
  theme(legend.position = "bottom", legend.title = element_blank())
ggsave(file = paste(path_figures, "/Figure-C4.pdf", sep=""), width = 7.5, height = 5)


# -----------------------------------------------------------------------------
# Figure C5
# -----------------------------------------------------------------------------

file_name <- "Policy_Enacted_This_Month_difference_network.txt"
var_label_long <- "Smoking ban enacted this month"

d <- data.frame(read.delim(paste(path_output, file_name, sep = "")))
var_label <- colnames(d)[5]
colnames(d)[5] <- c("var")

#add topic labels
d$topicLabels <- NA
for(j in 1:length(topic_labels_2)){
  d$topicLabels[d$topic == j] <- topic_labels_2[j]
}

#add group labels
d$groupLabels <- NA
for(j in 1:length(topic_groups_labels)){
  d$groupLabels[d$topic == j] <- topic_groups_labels[j]
}

d$topicLabels <- factor(d$topicLabels, levels = unique(d$topicLabels[order(d$diff, decreasing = TRUE)]))
d$var <- as.factor(d$var)

ggplot(data = d, aes(x = topicLabels, y = diff01, ymin = lb, ymax = ub, color = groupLabels)) +
  geom_pointrange() +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(d$topicLabels))) +
  geom_hline(yintercept = 0, lty = 2, size = 0.5) +
  labs(x = "", y = "Difference in topic prevalence (month of enactment - other months)", title = "") +
  theme_light() + 
  theme(legend.position = "bottom", legend.title = element_blank())
ggsave(file = paste(path_figures, "/Figure-C5.pdf", sep=""), width = 7.5, height = 5)


# -----------------------------------------------------------------------------
# Figure C6
# -----------------------------------------------------------------------------

file_name <- "policyMonthsBeforeAfterEnacted_network.txt" #fns_network[3]
topics_legislation <- c(7)
var_lab <- var_labels_long[3]

d <- data.frame(read.delim(paste(path_output, "/", file_name, sep = "")))
var_label <- colnames(d)[5]
colnames(d)[5] <- c("var")

d$topicLabels <- NA

for(j in 1:length(topic_labels)){
  d$topicLabels[d$topic == j] <- topic_labels[j]
}

d$groupLabels <- NA
for(j in 1:length(topic_groups_labels)){
  d$groupLabels[d$topic == j] <- topic_groups_labels[j]
}

#d$topicLabels <- factor(d$topicLabels, levels = unique(d$topicLabels[order(d$slope, decreasing = TRUE)]))

ggplot(data = subset(d, var %in% seq(-48,48)), aes(x = var, y = pointEstimate)) +
        geom_line(aes(color = groupLabels)) +
        geom_vline(xintercept = 0, linetype = "dashed") +
        facet_wrap(~ topicLabels, ncol = 3) +
        labs(x = "Number of months before/after smoking ban was enacted", y = "Topic prevalence", title = "") +
        theme_light() + 
        theme(legend.position = "bottom", legend.title = element_blank(), legend.key.size = unit(0.5, "cm"), strip.background = element_blank(), strip.text = element_text(color = "black")) +
        scale_alpha(guide = "none") +
        scale_x_continuous(breaks = seq(-48,48,24)) +
        geom_ribbon(aes(ymin = lb, ymax= ub, fill = groupLabels, alpha = 0.5))
ggsave(file = paste(path_figures, "/Figure-C6.pdf", sep=""), width = 7, height = 8.5)


# -----------------------------------------------------------------------------
# Figure C7
# -----------------------------------------------------------------------------

file_name <- c( "shareDemLow_network.txt", "unifiedDemocrats_network.txt", "unifiedRepublicans_network.txt", "smokers_network.txt") #fns_network[c(7,10,11,9)]
var_lab <- var_labels_long[c(7,10,11,9)]

topics_politics <- 8

d_1 <- data.frame(read.delim(paste(path_output, "/", file_name[1], sep = "")))
var_label <- colnames(d_1)[5]
colnames(d_1)[5] <- c("var")
d_1 <- subset(d_1, topic %in% topics_politics)
d_1$var_lab <- var_lab[1]

d_2 <- data.frame(read.delim(paste(path_output, "/", file_name[2], sep = "")))
var_label <- colnames(d_2)[5]
colnames(d_2)[5] <- c("var")
d_2 <- subset(d_2, topic %in% topics_politics)
d_2$var_lab <- var_lab[2]

d_3 <- data.frame(read.delim(paste(path_output, "/", file_name[3], sep = "")))
var_label <- colnames(d_3)[5]
colnames(d_3)[5] <- c("var")
d_3 <- subset(d_3, topic %in% topics_politics)
d_3$var_lab <- var_lab[3]

d_4 <- data.frame(read.delim(paste(path_output, "/", file_name[4], sep = "")))
var_label <- colnames(d_4)[5]
colnames(d_4)[5] <- c("var")
d_4 <- subset(d_4, topic %in% topics_politics & pointEstimate >= 0)
d_4$var_lab <- var_lab[4]

d <- rbind(d_1, d_2, d_3, d_4)

ggplot(data = subset(d, topic %in% topics_politics), aes(x = var, y = pointEstimate)) +
    geom_line() +
    facet_wrap(~ var_lab, ncol = 2, scales = "free_x") +
    labs(x = "", y = "Prevalence of the topic 'Electoral politics'", title = "") +
    theme_light() + 
    theme(legend.position = "none", strip.background = element_blank(), strip.text = element_text(color = "black")) +
    geom_ribbon(aes(ymin = lb, ymax= ub, alpha = 0.5))
ggsave(file = paste0(path_figures, "/Figure-C7.pdf"), width = 6, height = 5.5)


# -----------------------------------------------------------------------------
# Figure C8
# -----------------------------------------------------------------------------

ggplot(data = subset(scores_long, Top_Words == TRUE), aes(x = Exclusivity, y = Frequency, label = Words)) +
	geom_text(aes(size = Probability, color = Group))+
	scale_size(guide = "none") +
	facet_wrap(~ Topic, ncol = 3, scales = "free") +
	theme_light() +
	theme(legend.position = "bottom", legend.title = element_blank(), strip.background = element_blank(), strip.text = element_text(color = "black"))
ggsave(file = paste0(path_figures, "/Figure-C8.pdf"), width = 7, height = 8.5)

