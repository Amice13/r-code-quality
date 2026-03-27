
# ===========================
# Alternative validity tests 
# ============================
# Revision for POQ
# ==========================
# Date: 13/12/2024
# ==========================

# clean
rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   #Clean Plots


load("Data/fdata")
wp3_wide <- read.csv("Data/wp3_wide.csv")

# ======================== BIVARIATE CORRELATIONS =========================================
library(dplyr)
library(corrr)

#######################################################
# Construct validity
#######################################################
# Select exogenous exposure measures  
newsexp <-  fdata %>%
  select ( person_id, news_visits, news_minutes, political_url_visits, political_url_times, # exogenous exposure measures
           political_url_ukraine, political_url_ukraine_times, int_pnews, TV_rec, Newspapers_rec, Radio_rec, Wsites_rec, Smedia_rec,
           social_media_visits)

# Select social media minutes and entertainment
entertainment <- wp3_wide %>%
  select (person_id, social_media_minutes,
          entertainment_visits, entertainment_minutes)

# Left join
newdata <- newsexp %>%
  left_join (entertainment, by = "person_id")

# Bind data  
pcdata <- newdata [,-1]

colnames(pcdata) <- c("News_visits", "News_minutes", "Political_visits", "Political_minutes", "Ukraine_visits","Ukraine_minutes", "Interest_pnews", 
                      "Tv_freq", "Newspapers_freq", "Radio_freq", "Websites_freq", "Socialmedia_freq", "Socialmedia_visits", "Socialmedia_minutes", 
                      "Entertainment_visits", "Entertainment_minutes") 

# ===================================
# Compute pearson and Spearman correlations
# ==================================
pearson_corr <- cor(pcdata, use = "pairwise.complete.obs")
spearman_corr <- cor(pcdata, use = "pairwise.complete.obs", method = "spearman")

# Compute p-values 
library (Hmisc)
# First get rid of NAs
pcdata <- na.omit(pcdata)

# Compute 
corr_test <- rcorr(as.matrix(pcdata), type = "spearman")
p_values <- corr_test$P # still gives NAs

# Compute p_values manually 
#library(corpcor)

# Number of observations
#n <-  nrow (pcdata)

# Calculate the t-value 
#t_value <-  spearman_corr * sqrt ((n - 16) / (1 - spearman_corr^2))

# Calculate the p-value 
#p_value <- 2 * pt(-abs(t_value), df = n - 16)

# Visualize results
library (corrplot)
# Headmap
png("corrplot.png", width = 2000, height = 2000, res = 300)  # High resolution
corrplot (spearman_corr, method = "color", type = "lower", tl.col = "black", tl.cex = 0.8) 
dev.off ()

# ==================
# Compute p-values and filter only signficant values
# ===================

# Function to calculate significance of correlations 
p.mat <- cor.mtest <- function(mat, method = "spearman", ...) { 
  mat <- as.matrix(mat) 
  n <- ncol(mat) 
  p.mat <- matrix(NA, n, n) 
  diag(p.mat) <- 0 
  for (i in 1:(n - 1)) { 
    for (j in (i + 1):n) { 
      tmp <- cor.test(mat[, i], mat[, j], ...) 
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value 
      } 
    } 
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat) 
  p.mat } 

# Get the p-value matrix 
p_matrix <- cor.mtest(pcdata, method = "spearman") 

# Apply significance threshold (e.g., 0.05) 
cor_matrix_filtered <- spearman_corr
cor_matrix_filtered[p_matrix > 0.05] <- NA # Mask non-significant correlations # Plot using corrplot corrplot(cor_matrix_filtered, na.label = " ", method = "color", type = "upper")

# I need to fix row and column names before plotting
rownames(cor_matrix_filtered) <- c("News Visits", "News Minutes",
                                   "Political Visits", "Political Minutes",
                                   "Ukraine Visits", "Ukraine Minutes",
                                   "Interest in Political News", "Self-Rep. TV Freq.", 
                                   "Self-Rep. Newspaper Freq.", 
                                   "Self_Rep. Radio Freq.",
                                   "Self-Rep. Website Freq.", 
                                   "Self-Rep. Social Media Freq.",
                                   "Social Media Visits",
                                   "Social Media Minutes",
                                   "Entertainment Visits",
                                   "Entertainment Minutes")

colnames(cor_matrix_filtered) <- c("News Visits", "News Minutes",
                                   "Political Visits", "Political Minutes",
                                   "Ukraine Visits", "Ukraine Minutes",
                                   "Interest in Political News", "Self-Rep. TV Freq.", 
                                   "Self-Rep. Newspaper Freq.", 
                                   "Self_Rep. Radio Freq.",
                                   "Self-Rep. Website Freq.", 
                                   "Self-Rep. Social Media Freq.",
                                   "Social Media Visits",
                                   "Social Media Minutes",
                                   "Entertainment Visits",
                                   "Entertainment Minutes")


png("corrplot_filtered.png", width = 2000, height = 2000, res = 300)  # High resolution
corrplot (cor_matrix_filtered, na.label = " ", method = "color", type = "lower", tl.col = "black", tl.cex = 0.8) 
dev.off ()








# Convert the correlation matrix into a long-format table (and include significance stars for p-values)
library (reshape2)
corr_table <- melt (spearman_corr)
colnames (corr_table) <- c("Variable 1", "Variable 2", "Spearman Correlation")

# Add p_values
pvalues <- melt (p_values)
pvalues <- pvalues$value
corr_table <- cbind (corr_table, pvalues)

# Filter signficant correlations
significant_corres <- corr_table %>%
  filter (pvalues < 0.05) 

# Check non-significant correlations
non_significant_corres <- corr_table %>%
  filter (!(pvalues < 0.05))

# Plot significant correlations
png("corrplot2.png", width = 4000, height = 2000, res = 300)  # High resolution
plot_corr1 <- ggplot (significant_corres, aes (x = `Variable 1`, y = `Variable 2`, fill = `Spearman Correlation`)) +
  geom_tile () +
  scale_fill_gradient2 (low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal () +
  labs (title = "", x = "", y = "") +
  theme (axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
         axis.text.y = element_text (size = 15),
         title = element_text(size =15)
         )

dev.off()

# The same but now for correlations greater than 0.5 
# Filter signficant correlations
significant_corres <- corr_table %>%
  filter (`Spearman Correlation` > 0.3 & p_values < 0.05) 

# Plot significant correlations
plot_corr2 <- ggplot (significant_corres, aes (x = `Variable 1`, y = `Variable 2`, fill = `Spearman Correlation`)) +
  geom_tile () +
  scale_fill_gradient2 (low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal () +
  labs (title = "Correlations > 0.3", x = "", y = "") +
  theme (axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
         axis.text.y = element_text (size = 15),
         title = element_text(size =15)
         )

grid.arrange(plot_corr1, plot_corr2, ncol = 2)

###########################################################
# Factor analysis
###########################################################

# Install if necessary
install.packages(c("psych", "lavaan", "semPlot"))

# Load libraries
library(psych)      # For EFA
library(lavaan)     # For CFA
library(semPlot)    # For visualizing CFA

# =====================
# Exploratory Analysis
# ======================

# Bartlett's test of sphericity and KMO test
cortest.bartlett(cor(pcdata, use = "pairwise.complete.obs"))  # Bartlett's test
KMO(cor(pcdata, use = "pairwise.complete.obs"))               # Kaiser-Meyer-Olkin test


#	Bartlett’s test should be significant (p < 0.05).
#	KMO values > 0.6 are acceptable; > 0.8 are good.

# Determine the number of factors
#set.seed(123)
fa.parallel(pcdata, fa = "both", n.iter = 1000)

# Perform EFA (adjust factors based on the parallel test results)
efa_result <- fa(pcdata, nfactors = 4, rotate = "varimax")  # Replace 2 with the appropriate number of factors
print(efa_result)

# View factor loadings
print(efa_result$loadings)

# =======================
# Confirmatory analysis
# =======================

class(exog)


# Specify the model
cfa_model <- '
  Factor1 =~ news_visits + news_minutes + political_url_visits + political_url_times + political_url_ukraine + political_url_ukraine_times 
  Factor2 =~ social_media_visits + social_media_minutes + entertainment_visits + entertainment_minutes
  Factor3 =~ news_visits + int_pnews + TV_rec + Newspapers_rec + Radio_rec + Wsites_rec + Smedia_rec + entertainment_minutes
  Factor4 =~ news_visits + news_minutes + political_url_visits + political_url_times + political_url_ukraine
'

# Fit the model
cfa_fit <- cfa(cfa_model, data = news_exp_and_enter)

# Summarize the results
summary(cfa_fit, fit.measures = TRUE, standardized = TRUE)

# Plot the CFA model
semPaths(cfa_fit, "std", layout = "tree", edge.label.cex = 0.8, sizeMan = 5)



# =================================
# Principal component analysis 
# =================================
install.packages("factoextra")
library (factoextra)

pcdata <- na.omit(pcdata)

# Change names in dataset set 
colnames(pcdata) <- c("News_visits", "News_minutes", "Political_visits", "Political_minutes", "Ukraine_visits","Ukraine_minutes", "Interest_pnews", 
                      "Tv_freq", "Newspapers_freq", "Radio_freq", "Websites_freq", "Socialmedia_freq", "Socialmedia_visits", "Socialmedia_minutes", 
                      "Entertainment_visits", "Entertainment_minutes") 

# Principal component
pca <- prcomp(pcdata, scale. = T)
pca <- prcomp(pcdata)

# Customize biplot
fviz_pca_biplot (
  pca, 
  label = "var",
  geom.ind = "point",
  repel = T,
  labelsize = 5,
  pointsize = 3
  ) +
  theme (axis.text.x = element_text(size=15),
         axis.text.y = element_text(size=15),
         axis.title = element_text(size=15)
         )




