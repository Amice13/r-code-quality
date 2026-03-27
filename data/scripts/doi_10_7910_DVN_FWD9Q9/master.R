#
# Master replication file for Das, Sinclair, Webster, and Yan:
# "All (Mayoral) Politics is Local?"
# The Journal of Politics
#

# If needed, install the required packages

pkgs <- c("tidyverse","stringr","stargazer","ggridges")
install.packages(pkgs)

# Next, set your working directory
dir <- "SET YOUR OWN DIRECTORY HERE"
setwd(dir)

# Source the two R scripts necessary for producing the tables found in the paper
# The first script cleans the mayoral covariate data and merges in the data
# containing the "Congressional Similarity" score. The second script produces
# the regression tables and summary statistics tables found in the manuscript 
# and the Online Appendix. Finally, the third script produces Figure 1.

source("data_cleaning_and_merging.R")
source("regressions.R")
source("figure1.R")

# End file