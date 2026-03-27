# Code to fit the ground-truth model for the Faux Mesa High Simulations
#
# Per the paper, the model is edges, nodematch(sex), and GWESP(0.25)
# Output is saved in fms_groundtruth_model.RData; object is fmsfit
#
# Last modified 8/13/21 by CTB

#Load the library
library(ergm)  #Analyses performed with ergm version 4.1.2

#Load the data
data(faux.mesa.high)

#Fit the model, using the default settings
set.seed(1331)
fmsfit<-ergm(faux.mesa.high~edges+nodematch("Sex")+gwesp(0.25,fixed=TRUE))

#Save the result
save(fmsfit,file="fms_groundtruth_model.RData")
