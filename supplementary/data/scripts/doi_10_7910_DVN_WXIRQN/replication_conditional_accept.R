#######################
# set working directory
# and load libraries
#######################

# remove all objects
rm(list=ls())
# and detach all libraries
# in case you have something lurking in 
# your global environment
detachAllPackages <- function() {
  # list of basic packages you want
  basic.packages <- c("package:stats",
                      "package:graphics",
                      "package:grDevices",
                      "package:utils",
                      "package:datasets",
                      "package:methods",
                      "package:base")
  # check if there are any "non-basic" packages
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, T, F)]
  package.list <- setdiff(package.list, basic.packages)
  # if there are any packages hidding, remove them
  if (length(package.list)>0){
    for (package in package.list){
      detach(package, character.only=T)
    }
  } 
}
detachAllPackages()

# set working directory to parent replication folder
# this shouldn't be impacted where you downloaded 
# the replication files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# need to first fix issue with Zelig
# this will take a minute or two probably
source("zelig_fix.R")

# to load 'openEnded' you'll need to 
# install it the first time you run script
# INSTALL: devtools::install_github('jeffreyziegler/openEnded', force=T)

# you'll most likely just skip updating other packages ("3: None")
# shouldn't be necessary for openEnded to function properly
library(openEnded)

###########
# load data
###########

# can load data from package with data("kaneData")
# create jaccard and cosine similarity measures
kaneData <- similarityMeasures(dataframe=kaneData,
                               n_gram_measures_to_calculate=c("jaccard", "cosine", "jw", "dl"),
                               prompt="textViewed", 
                               response="openResponse", 
                               ngrams=3)

###########
# Figure 1
###########

# use base function plotSimilarity to plot cosine distances
figure1 <- plotSimilarity(dataframe=kaneData,
                           measure="cosineSimilarity") 

# then, add additional features that are displayed in Figure 1
pdf("../figures/Fig1.pdf", width=10, height=10)
figure1 + annotate("label", 
                    hjust = 0, 
                    family = "serif", 
                    x = c(0.24, 0.05), 
                    y = c(55, 120), 
                    size=10.25,
                    label = c("Cosine distance: 0.79
Written response: 'Biographical 
information about president trump'.
Text viewed: 'Basic biographical
information about President Trump'",
                     "Cosine distance: 0.08
Written response: 'Trumps policies 
and decisions and views'
Text viewed: 'Trump recently pleasing 
many of his conservative supporters'")) +
  geom_segment(aes(x = 0.1, y = 90, xend = 0.1, yend = 55), 
               colour='black', size=.25, arrow = arrow(length = unit(0.5, "cm")))+
  geom_segment(aes(x = .7, y = 25, xend = 0.75, yend = 20), 
               colour='black', size=.25, arrow = arrow(length = unit(0.5, "cm")))

dev.off()

###########
# Figure 2
###########

plotCorrectness(kaneData, 
                measures=c("cosineSimilarity", "jaccardSimilarity"), 
                correct_vec="codeCorrect",
                plot_path="../figures/Fig2.pdf"
                )

###########
# Figure 3
###########

# produces plot overall marginal effects 
# and w/ 'print_regs=T'reproduces columns 1-3 in Table SM.4

# execute regressions to compare overall results
kane_formula <- SelectTrump ~ Conditions*ideology_factor
regressionComparison(dataframe=kaneData, 
                     formula=kane_formula, 
                     plot_treatment="Conditions",
                     plot_interact_x="ideology_factor",
                     similarity_measures=c("cosineSimilarity", "jaccardSimilarity"),
                     k=3, 
                     model_type="logit", 
                     up_down_weight="down",
                     user_seed=5,  
                     n_sims=10000, 
                     correct_vec="correct", 
                     display_plot=T,
                     plot_path="../figures/Fig3.pdf", 
                     print_regs=F,
                     plotDifferences = F
                     )

###########
# Figure 4
###########

# produces plot of marginal effects for "compliers" and "non-compliers"
# this might take a minute because we're calculating a 
# regression and marginal effects 2 x levels x treatments x n 
# (2x3x3x100 = 1800 marginal effects)
complierATE(dataframe=kaneData,
            formula=kane_formula,
            plot_treatment="Conditions",
            plot_interact_x="ideology_factor",
            similarity_measures=c("cosineSimilarity", "jaccardSimilarity"),
            bounds=c(0, 0.1), 
            n=100, 
            user_seed=5, 
            model_type="logit", 
            k=3,
            display_plot=T,
            plot_path="../figures/Fig4.pdf",
            stable_x=T, 
            k_plot=F)

# switch k_plot=T to replicate Figure SM.4 