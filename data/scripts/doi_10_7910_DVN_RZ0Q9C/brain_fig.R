# Code to produce example brain connectivity network figure
#
# The input here is the data in brain_binary_network_list.rds.  
#
# Last modified 8/22/21 by CTB

#Load required libraries
library(sna)

#Load the brain networks
brains<-readRDS("brain_binary_network_list.rds")

#Create coordinates in 2D, based on distances
coord<-MASS::isoMDS(brains[[1]]%n%"Distance")$points

#Plot the braaaaaaiinnns
pdf("brain_network_subj002_003.pdf",8,4.5)
par(mfrow=c(1,2),mar=c(0.5,0.5,2,0.5)+0.1)
gplot(brains[[1]],coord=coord,gmode="graph", edge.col=rgb(0,0,0,0.15), vertex.col=2+2*(brains[[1]]%v%"Hemisphere"=="R"),pad=0, main="Subject 002")
gplot(brains[[2]],coord=coord,gmode="graph", edge.col=rgb(0,0,0,0.15), vertex.col=2+2*(brains[[2]]%v%"Hemisphere"=="R"),pad=0, main="Subject 003")
legend("bottomleft",fill=c(2,4),legend=c("Left Hemisphere","Right Hemisphere"), bty="n")
dev.off()
