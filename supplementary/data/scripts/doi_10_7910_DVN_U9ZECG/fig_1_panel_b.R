# Clear workspace
rm(list = ls())

# List of required packages
required_packages <- c("here", "grid", "ggplot2", "data.table", "gridExtra", "sandwich", "reshape2", "ggthemes", "rdrobust")

# Function to check and install missing packages
check_and_install_packages <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cat("Package", pkg, "is not installed. Installing now...\n")
      install.packages(pkg)
    } else {
      cat("Package", pkg, "is already installed.\n")
    }
  }
}

# Call the function with the required packages
check_and_install_packages(required_packages)

# Load required libraries
library(grid)
library(ggplot2)
library(data.table)
library(gridExtra)
library(sandwich)
library(reshape2)
library(ggthemes)
library(rdrobust)
library(here)

# Load data
load(here("data","ver_data.RData"))

# Preprocess data
ver_b[, dv := dv_homicides]
temp <- unique(ver_b[!is.na(dv) & abs(marginal_muni)< .0015 & marginal_muni !=0, .(muni_code, year, elected_muni, dv, marginal_muni)]) 

# Set up a ranking column and a position column
temp[, ranking := abs(marginal_muni)]
temp[, position := rank(ranking, ties.method = "first")]
setkey(temp,position)

# Initialize a data frame to store results
small <- data.frame(upper=as.numeric(),
                    lower=as.numeric(),
                    estimate=as.numeric(),
                    treat=as.numeric(),
                    control=as.numeric(),
                    study_group=as.numeric(),
                    max_margin=as.numeric(),
                    stringsAsFactors=FALSE)

# Loop through different study group sizes
for(i in seq(10,50,5)){
  teteia = temp[position <= i]
  test = t.test(teteia$dv ~ teteia$elected_muni, alternative='two.sided', conf.level=0.95)
  estimate <- abs(mean(teteia$dv[teteia$elected_muni == 1]) - mean(teteia$dv[teteia$elected_muni == 0]))
  small[i,1] = test$conf.int[[1]]*-1
  small[i,2] = test$conf.int[[2]]*-1
  small[i,3] = estimate
  small[i,4] = nrow(teteia[teteia$elected_muni ==1])
  small[i,5] = nrow(teteia[teteia$elected_muni ==0])
  small[i,6] = i
  small[i,7] = max(teteia$marginal,na.rm=TRUE)
} 

# Remove rows with missing values and round numeric columns
small = data.table(small[!is.na(small$upper),])
small[,c('upper','lower','estimate'):=lapply(.SD, round, 2), .SDcols = c('upper','lower','estimate')]

# Create ggplot2 visualization
small.sample <- ggplot(small, aes(x = (study_group), y = estimate)) +
  geom_errorbar(aes(ymax = (upper), ymin = (lower)), width=0, linewidth=.5,position=position_dodge(width=0.9)) +
  geom_point(alpha=.9,size=2.5) +
  geom_hline(yintercept=0, linetype="dotted",colour='gray20') +
  theme_minimal() +
  scale_colour_grey() +
  xlab("Study Group Size") +
  ylab('Difference in Homicide Rate\n(Yearly deaths per 100 thousand pop.)') +
  geom_text(aes(y = 39, label = treat), alpha=.8,size=3,family='Times') +
  geom_text(aes(y = 35, label = control), size=3,fontface = 'italic',family='Times') +
  annotate('text', y = 41, x=18,label = 'Treatment Group',size=3,col='gray20') +
  annotate('text', y = 36, x=18,label = 'Control Group',size=3,col='gray20',fontface = 'italic') +
  NULL

# Save plot
ggsave(here("writing","img","figure_1_panel_b.pdf"), plot = small.sample, device = 'pdf',height = 10, width = 10, units = 'cm')
