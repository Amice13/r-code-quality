
#setup

rm(list=ls())
setwd("/Users/Vincent/Dropbox/Historical Legacies/replication")
color.printing <- FALSE

source("scripts/libraries.R")
source("scripts/helpers.R")

#couldn't figure out how to vectorize this, there are three results files
load("output/results_cross.Rdata")
load("output/results_dyads.Rdata")
load("output/results_match.Rdata")

#combined 

results.cross$reg <- "Regression"
results.dyads$reg <- "Neighbors"
results.match$reg <- "Matching"

cols <- c("var", "type", "coef", "se", "x", "sig", "reg")
results.comb <- rbind(results.cross[,cols], results.dyads[,cols], results.match[,cols])
results.comb <- results.comb[results.comb$type=="Full Controls" |
                                 results.comb$reg=="Neighbors",]
results.comb$x <- as.numeric(factor(results.comb$reg, levels=c("Regression", "Neighbors", "Matching")))

plot1 <- myCoefplot(matrix=results.comb, var="Night Lights", letter="A") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot2 <- myCoefplot(matrix=results.comb, var="Infant Mortality", letter="B")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot3 <- myCoefplot(matrix=results.comb, var="School Years", letter="C")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot <- grid_arrange_shared_legend(plot1, plot2, plot3, nrow=1) 
ggsave(file=paste0("charts/fig_main", ifelse(color.printing, "_col", "_bw"), ".png"), plot = plot, width = 6.5, height=4, units="in")

