### master script for Generative Dynamics of Supreme Court Citations: Analysis with a New Statistical Network Model Replication Code

# read in data by Fowler and Jeon (http://jhfowler.ucsd.edu)
# this takes 6 hours
source("scripts/supreme_court_code.R")

# create figures 1 and 2. this requires supreme.RData obtained from supreme_court_code.R
# this takes a few seconds
source("scripts/scc_intro_plots_PA.R")

# create figure 3
# this takes a few seconds
source("scripts/daggishIllustration.R")

# Next, we need to read in the data. 
# This takes about 3 hours
source("scripts/data_readin.R")

# create degree distribution plot in figure 4 and numbers in table 1
# takes less than a minute to run
source("scripts/EDA.R")

# create adjacency matrix and network plot in figure 4
# this takes about 10 minutes
source("scripts/visualizations_PA.R")

# fit cERGM for SCOTUS citation data
# this takes 13 days. However, the for loop can be parallelized
source("scripts/Model_Fit.R")

# create figures 5, 6, and 7
# this takes less than a minute
source("scripts/Plot_SCC.R", echo=TRUE)

# run GOF test on the 1950 network
# this takes 3 hours
source("scripts/GOF_1950.R")

# run GOF test on the 2015 network
# this takes 31.5 hours
source("scripts/GOF_2015.R")

# plot figures 1a, 1b, 2 and 3 in the appendix
# this takes 15 minutes
source("scripts/GOF_Plot.R")
