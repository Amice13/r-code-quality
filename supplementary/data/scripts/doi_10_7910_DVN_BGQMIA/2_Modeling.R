rm(list = ls())
# only focus on 1979-2014
yrs <- as.character(seq(1979, 2014, by =1))
#################
load("./Data/makevisiList.RData")
load("./Data/hostvisitList.RData")
load("./Data/xNodeL.RData")
load("./Data/xDyadL.RData")


makevisiList <- makevisiList[yrs]
hostvisitList <- hostvisitList[yrs]
xNodeL <- xNodeL[yrs]
xDyadL <- xDyadL[yrs]
dimnames(xDyadL[[1]])[3]
dimnames(xNodeL[[1]])[2]
# set up model specs
subListArray = function(lA, vars, dims=2){
  if(dims==2){ return( lapply(lA, function(x){ x[,vars, drop=FALSE] }) ) }
  if(dims==3){ return( lapply(lA, function(x){ x[,,vars,drop=FALSE] }) ) } }

########################################################################################
### we need to use the AMEN 1.4
#library(devtools)
#devtools::install_github('s7minhas/amen', ref='pa2018_version')
library(amen)



#2:14pm-3:45pm, 1.5h

## run three AME models
AME_makevisiList = ame_repL(
  Y = makevisiList, Xdyad = xDyadL,
  Xrow = xNodeL, Xcol = xNodeL,
  symmetric= FALSE, nvar=TRUE, R=2, 
  model='bin', intercept=TRUE, seed=6886,
  burn= 10000, nscan= 40000, odens=20,
  plot=FALSE, gof=FALSE, periodicSave=FALSE )
summary(AME_makevisiList)
save(AME_makevisiList, file = "./Results/AME_makevisiList.RData")

AME_hostvisiList = ame_repL(
  Y = hostvisitList, Xdyad = xDyadL,
  Xrow = xNodeL, Xcol = xNodeL,
  symmetric= FALSE, nvar=TRUE, R=2, 
  model='bin', intercept=TRUE, seed=6886,
  burn= 10000, nscan= 40000, odens=20,
  plot=FALSE, gof=FALSE, periodicSave=FALSE )
summary(AME_hostvisiList)
save(AME_hostvisiList, file = "./Results/AME_hostvisiList.RData")


#############

rm(list=ls())
library(btergm)
#library(tergm)
library(ergm)
library(sna)
library(network)
# only focus on 1979-2014
yrs <- as.character(seq(1979, 2014, by =1))
#################
load("Data/makevisiList.RData")
load("Data/hostvisitList.RData")
load("Data/xNodeL.RData")
load("Data/xDyadL.RData")
source("Rcode/listToArray.R")

makevisiList <- makevisiList[yrs]
hostvisitList <- hostvisitList[yrs]
xNodeL <- xNodeL[yrs]
xDyadL <- xDyadL[yrs]
dimnames(xDyadL[[1]])[3]
dimnames(xNodeL[[1]])[2]

# get a uniqu set of actors
actorSet <- sort(unique(unlist( lapply(makevisiList, rownames) )))

## Y list to array
arrayObj <- listToArray(actors = actorSet, Y=makevisiList,
                        Xdyad= xDyadL, Xrow = xNodeL, Xcol = xNodeL)


Y<-arrayObj$Y ; Xdyad<-arrayObj$Xdyad
Xrow<-arrayObj$Xrow ; Xcol<-arrayObj$Xcol

rm(arrayObj)

# prep for btergm
yMats = lapply(1:dim(Y)[3], function(t){
  #
  ySlice = Y[,,t]
  ySlice[is.na(ySlice)] = 10
  diag(ySlice) = 0
  return( ySlice ) })

prepCovar = function(data, var){
  covar = lapply(1:dim(data)[4], function(t){
    xSlice = data[,,var,t]
    xSlice[is.na(xSlice)] = NA
    diag(xSlice) = 0
    return( xSlice ) })
  return(covar) }


##get dyadic covariates
idealDyCovar = prepCovar(Xdyad, 'idealpointdistance')
demoDyCovar = prepCovar(Xdyad, 'diffv2x_polyarchy')
allyDyCovar = prepCovar(Xdyad, 'atopally')
distDyCovar = prepCovar(Xdyad, 'minidist_log')
tradeDyCovar = prepCovar(Xdyad, 'imports_log')
popDyCovar = prepCovar(Xdyad, 'diff_pop_log')
gdppctDyCovar = prepCovar(Xdyad, 'diff_gdppc_log')
milexpDyCovar = prepCovar(Xdyad, 'diff_miliexp')

#
yMats = handleMissings(yMats, na=NA, method='remove')



idealDyCovar = handleMissings(idealDyCovar, na=NA, method='remove')
demoDyCovar = handleMissings(demoDyCovar, na=NA, method='remove')
allyDyCovar = handleMissings(allyDyCovar, na=NA, method='remove')
distDyCovar = handleMissings(distDyCovar, na=NA, method='remove')
tradeDyCovar = handleMissings(tradeDyCovar, na=NA, method='remove')
popDyCovar = handleMissings(popDyCovar, na=NA, method='remove')
gdppctDyCovar = handleMissings(gdppctDyCovar, na=NA, method='remove')
milexpDyCovar = handleMissings(milexpDyCovar, na=NA, method='remove')


#
yNets = lapply(1:length(yMats), function(t){
  #
  yNet = network(yMats[[t]])
  
  #
  actors = network.vertex.names(yNet)
  
  yNet = set.vertex.attribute(yNet, "v2x_polyarchy", Xrow[actors,'v2x_polyarchy',t])
  yNet = set.vertex.attribute(yNet, "pop_log", Xrow[actors,'pop_log',t])
  yNet = set.vertex.attribute(yNet, "gdppc_log", Xrow[actors,'gdppc_log',t])
  yNet = set.vertex.attribute(yNet, "unsc_p5", Xrow[actors,'unsc_p5',t])
  yNet = set.vertex.attribute(yNet, "civiwar", Xrow[actors,'civiwar',t])
  #
  return(yNet) })
################

################
set.seed(6886)
# need to deal with missing values for node
make_mod = btergm(
  yNets ~  edges + istar(2) + ostar(2) + mutual + triangles+
    edgecov(idealDyCovar) + edgecov(demoDyCovar) +
    edgecov(allyDyCovar) + edgecov(distDyCovar) +
    edgecov(tradeDyCovar) +  edgecov(popDyCovar) + edgecov(gdppctDyCovar) + 
    edgecov(milexpDyCovar) + 
    nodeocov('v2x_polyarchy') + nodeocov('pop_log') + nodeocov('gdppc_log') +
    nodeocov('unsc_p5') + nodeocov('civiwar') + 
    nodeicov('v2x_polyarchy') + nodeicov('pop_log') + nodeicov('gdppc_log') +
    nodeicov('unsc_p5') + nodeicov('civiwar')+
    gwesp(.5,fixed=TRUE), R = 1000
)
summary(make_mod)
save(make_mod, file='Data/make_tergm.RData')



##################################################################################################
####### host visit network
##################################################################################################
# get a uniqu set of actors
actorSet <- sort(unique(unlist( lapply(hostvisitList, rownames) )))

## Y list to array
arrayObj <- listToArray(actors = actorSet, Y=hostvisitList,
                        Xdyad= xDyadL, Xrow = xNodeL, Xcol = xNodeL)


Y<-arrayObj$Y ; Xdyad<-arrayObj$Xdyad
Xrow<-arrayObj$Xrow ; Xcol<-arrayObj$Xcol

rm(arrayObj)

# prep for btergm
yMats = lapply(1:dim(Y)[3], function(t){
  #
  ySlice = Y[,,t]
  ySlice[is.na(ySlice)] = 10
  diag(ySlice) = 0
  return( ySlice ) })


#
#
prepCovar = function(data, var){
  covar = lapply(1:dim(data)[4], function(t){
    xSlice = data[,,var,t]
    xSlice[is.na(xSlice)] = NA
    diag(xSlice) = 0
    return( xSlice ) })
  return(covar) }


##get dyadic covariates
idealDyCovar = prepCovar(Xdyad, 'idealpointdistance')
demoDyCovar = prepCovar(Xdyad, 'diffv2x_polyarchy')
allyDyCovar = prepCovar(Xdyad, 'atopally')
distDyCovar = prepCovar(Xdyad, 'minidist_log')
tradeDyCovar = prepCovar(Xdyad, 'imports_log')
popDyCovar = prepCovar(Xdyad, 'diff_pop_log')
gdppctDyCovar = prepCovar(Xdyad, 'diff_gdppc_log')
milexpDyCovar = prepCovar(Xdyad, 'diff_miliexp')

#
yMats = handleMissings(yMats, na=NA, method='remove')



idealDyCovar = handleMissings(idealDyCovar, na=NA, method='remove')
demoDyCovar = handleMissings(demoDyCovar, na=NA, method='remove')
allyDyCovar = handleMissings(allyDyCovar, na=NA, method='remove')
distDyCovar = handleMissings(distDyCovar, na=NA, method='remove')
tradeDyCovar = handleMissings(tradeDyCovar, na=NA, method='remove')
popDyCovar = handleMissings(popDyCovar, na=NA, method='remove')
gdppctDyCovar = handleMissings(gdppctDyCovar, na=NA, method='remove')
milexpDyCovar = handleMissings(milexpDyCovar, na=NA, method='remove')


#
yNets = lapply(1:length(yMats), function(t){
  #
  yNet = network(yMats[[t]])
  
  #
  actors = network.vertex.names(yNet)
  
  yNet = set.vertex.attribute(yNet, "v2x_polyarchy", Xrow[actors,'v2x_polyarchy',t])
  yNet = set.vertex.attribute(yNet, "pop_log", Xrow[actors,'pop_log',t])
  yNet = set.vertex.attribute(yNet, "gdppc_log", Xrow[actors,'gdppc_log',t])
  yNet = set.vertex.attribute(yNet, "unsc_p5", Xrow[actors,'unsc_p5',t])
  yNet = set.vertex.attribute(yNet, "civiwar", Xrow[actors,'civiwar',t])
  #
  return(yNet) })
################

################
set.seed(6886)
# need to deal with missing values for node
host_mod = btergm(
  yNets ~  edges + istar(2) + ostar(2) + mutual + triangles+
    edgecov(idealDyCovar) + edgecov(demoDyCovar) +
    edgecov(allyDyCovar) + edgecov(distDyCovar) +
    edgecov(tradeDyCovar) +  edgecov(popDyCovar) + edgecov(gdppctDyCovar) + 
    edgecov(milexpDyCovar) + 
    nodeocov('v2x_polyarchy') + nodeocov('pop_log') + nodeocov('gdppc_log') +
    nodeocov('unsc_p5') + nodeocov('civiwar') + 
    nodeicov('v2x_polyarchy') + nodeicov('pop_log') + nodeicov('gdppc_log') +
    nodeicov('unsc_p5') + nodeicov('civiwar')+
    gwesp(.5,fixed=TRUE), R = 1000
)
summary(host_mod)
save(host_mod, file='Data/host_tergm.RData')


