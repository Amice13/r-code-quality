##' Table of contents
##' 
#'    Load libraries  40 - 71
#'    Load/create helper objects  75 - 153
#'    Custom functions  159 - 576
#'    Baikal Taxa Included  578 - 1138
#     	Anatomical Data 588 - 882
#' 		      FIGURE 1  789 - 798
#'		      Local Superimposition Code 815 - 878
#'    	Analysis of shape 886 - 898
#'    	Phylogenetic comparative methods  902 -
#'		      FIGURE 2  907 - 940
#'		      Outlier analysis  945 - 949
#'		      Phylogenetic MANOVA 954 - 972
#'    	    Disparity analysis 973 - 983
#'    	    Disparity through time- Agonidae  998 - 999
#'    	    Disparity through time- Cottidae  1007 - 1008
#'    	    Disparity through time- Psychrolutidae  1016 - 1017
#'    	Hypothesis 1: differing rates of morph evol 1024 - 1049
#'    	Hypothesis 2: differing mode of morph evol (lineage density)  1053 - 1137
#'		      Lineage Density Code  1057 - 1137
#'    Baikal Taxa Excluded  1139 - 
#     	Anatomical Data 1147 - 1525
#'		      Local Superimposition Code  1452 - 1522
#'    	Analysis of shape 1529 - 1537
#'    	Phylogenetic comparative methods  1541 - 1695
#'		      ALT FIGURE 2  1545 - 1596
#'		      Phylogenetic MANOVA 1619 - 1648
#'    	    Disparity analysis 1650 - 1658
#'    	    Disparity through time- Agonidae  1673 - 1674
#'    	    Disparity through time- Cottidae  1682 - 1683
#'    	    Disparity through time- Psychrolutidae  1691 - 1692
#'    	Hypothesis 1: differing rates of morph evol 1699 - 1718
#'    	Hypothesis 2: differing mode of morph evol (lineage density)  1722 - 1802
#'		      Lineage Density Code  1726 - 1798
##'   
###############################################################################'
###############################################################################'
##'
#'    Load libraries - begin
library("geomorph")
library("Morpho")
library("phytools")
library("geiger")
library("fishtree")
library("phytools")
library("Rvcg")
library("rgl")
library("stringi")
require("ape")
library("recexcavAAR")
library("randomcoloR")
library("cluster")
library("abind")
# All analyses were run using the following versions:

#> sessionInfo()
#R version 4.2.3 (2023-03-15 ucrt)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows 10 x64 (build 19045)

#attached base packages:
#  [1] stats     graphics  grDevices utils     datasets  methods   base     
#
#other attached packages:
#   [1] abind_1.4-5         cluster_2.1.4       randomcoloR_1.1.0.1 recexcavAAR_0.3.0   kriging_1.2        
#   [6] stringi_1.7.12      Rvcg_0.22.1         fishtree_0.3.4      geiger_2.0.10       phytools_1.2-0     
#   [11] maps_3.4.1          ape_5.6-2           Morpho_2.11         geomorph_4.0.5      Matrix_1.5-3       
#   [16] rgl_1.0.1           RRPP_1.3.1       
##'
#'    Load libraries - end
###############################################################################'
###############################################################################'
##'
#' Load/create helper objects - begin
##'
##' Designate a working drive- user will need to specify file location
setwd("/Supplementary_Rfolder")
#

# Correspondences for paired (symmetric) landmarks and semilandmarks
skull.land.pairs.fullslide <- as.matrix(read.csv(file = "skull.full.slider.corresponences.csv", header = F))

# Read in ecosystem data
sculpin.habitat.table <- read.csv(file = "SkullTest_habitat.csv", header = T, row.names = 1)

# Defining semilandmark slider matrix. This corresponds to Supplementary Table 2.
skull.curvesliders <- rbind(
  define.sliders(c(2, 55:65, 3), nsliders = 11),  # Curve 1 LHS
  define.sliders(c(8, 66:69, 11), nsliders = 4),  # Curve 2 LHS
  define.sliders(c(14, 70:80, 22), nsliders = 11),  # Curve 3 LHS
  define.sliders(c(18, 81:89, 20), nsliders = 9),  # Curve 4 LHS
  define.sliders(c(10, 90:93, 12), nsliders = 4),  # Curve 5 LHS
  define.sliders(c(9, 94:97, 12), nsliders = 4),  # Curve 6 LHS
  define.sliders(c(2, 98:102, 5), nsliders = 5),  # Curve 7 LHS
  define.sliders(c(37, 103:106, 40), nsliders = 4),  # Curve 8 LHS
  define.sliders(c(37, 107:110, 38), nsliders = 4), # Curve 9 LHS
  define.sliders(c(39, 111:114, 41), nsliders = 4), # Curve 10 LHS
  define.sliders(c(42, 115:117, 44), nsliders = 3), # Curve 11 LHS
  define.sliders(c(43, 118:120, 45), nsliders = 3), # Curve 12 LHS
  define.sliders(c(46, 121:125, 48), nsliders = 5), # Curve 13 LHS
  define.sliders(c(47, 126:130, 49), nsliders = 5), # Curve 14 LHS
  define.sliders(c(50, 131:135, 52), nsliders = 5), # Curve 15 LHS
  define.sliders(c(50, 136:140, 54), nsliders = 5), # Curve 16 LHS
  define.sliders(c(142, 187:197, 143), nsliders = 11), # Curve 1 RHS
  define.sliders(c(148, 198:201, 151), nsliders = 4), # Curve 2 RHS
  define.sliders(c(157, 202:210, 159), nsliders = 9), #Curve 4 RHS
  define.sliders(c(150, 211:214, 152), nsliders = 4), # Curve 5 RHS
  define.sliders(c(149, 215:218, 152), nsliders = 4), # Curve 6 RHS
  define.sliders(c(142, 219:223, 145), nsliders = 5), # Curve 7 RHS
  define.sliders(c(173, 224:227, 176), nsliders = 4), # Curve 8 RHS
  define.sliders(c(173, 228:231, 174), nsliders = 4), # Curve 9 RHS
  define.sliders(c(175, 232:235, 177), nsliders = 4), # Curve 10 RHS
  define.sliders(c(178, 236:238, 180), nsliders = 3), # Curve 11 RHS
  define.sliders(c(179, 239:241, 181), nsliders = 3), # Curve 12 RHS
  define.sliders(c(182, 242:246, 184), nsliders = 5), # Curve 15 RHS
  define.sliders(c(182, 247:251, 186), nsliders = 5), # Curve 16 RHS
  define.sliders(c(267, 280:339, 270), nsliders = 60), # Curve 17 LHS
  define.sliders(c(355, 368:427, 358), nsliders = 60) # Curve 17 RHS
)
#' 
#' File that contains the slider data for the semilandmark numbers that are located on each 
#' bone on the midline and left hand side of the skull
Bone_Slider_LHS2 <- read.csv(file = "Bone_Sliders_LHS2.csv", header = T)
#'
#' File that contains the slider data for the semilandmark numbers that are located on each 
#' bone on the right hand side of the skull
Bone_Slider_RHS2 <- read.csv(file = "Bone_Sliders_RHS2.csv", header = T) 
#' 
# Read-in information for each specimen
skulltest1.data <- read.csv(file = "SkullTest_220124_Names.csv", header = T, row.names = 1)
#
# Isolate pertinent information for each species
skulltest1.species.data <- skulltest1.data[!duplicated(skulltest1.data$species),]
rownames(skulltest1.species.data) <- skulltest1.species.data$species
skulltest1.species.data <- skulltest1.species.data[,2:5]
#'
#' File that relates the set of landmark numbers that are located on both sides of the head
#' for each bone or set of paired bones
Bone_LM_comb <- read.csv(file = "Bone_LM_combined.csv", header = T)

#' Isolate the set of landmark numbers that are located on each bone on the midline
#' and left hand side of the skull
Bone_LM_LHS <- Bone_LM_comb[1:228,]
#'
#' Isolate the set of lanmark numbers that are located on each bone on the right 
#' hand side of the skull
Bone_LM_RHS <- Bone_LM_comb[229:427,]
#'
#'
##'
##'
############### Load/create helper objects - end #######################'
########################################################################
##'
##'
#'
##########################################################################
#### custom functions- begin ####################################################
#' Going to use a custom function for species-level landmark averaging 
#' from Buser et al. (2019):
#' 
#' Buser, T.J., Burns, M.D. and López, J.A., 2017. Littorally adaptive? 
#' Testing the link between habitat, morphology, and reproduction in 
#' the intertidal sculpin subfamily Oligocottinae (Pisces: Cottoidea). 
#' PeerJ, 5, p.e3634.
#' 
#' First the landmark function:
######## Species Average for landmark data ###############
#'
#requires two variables:
# 1) "classifier" - must be an n by 2 matrix, with "ID" as the first column header
#' and "Species" as the second column header. Each row should contain the information
#' for a given specimen, such that "ID" is the entry in the array (see below) for that 
#' individual, and "Species" is the species of that inividual.
#' 2) "aligned.coords" - must be a p x k x n array of procrustes-aligned landmark data. 
#' Where "p" is the number of landmarks, "k" is the number of landmark dimensions 
#' (i.e., "2" or "3"), and "n" is the number of specimens. This format is the default 
#' output for the procrustes alignment functions found in the package "geomorph," 
#' specifically "gpagen()" and "bilat.symmetry." For gpagen, you'll need to specify the
#' "$coords" componenet of the functions output, for bilat.symmetry, you'll need to 
#' specify either "$symm.shape" or "$asymm.shape."
#'
#'
#'
my.landmark.species.average <- function(classifier, aligned.coords){
  #'  
  require(geomorph)
  # Number of species
  species.n <- as.character(unique(classifier$Species))
  
  # Set up an array to hold the data for each species
  # Need the number of coordinates, dimensions, and species
  #pick the first entry to get coordinates and dimensions
  dummy.dim <- aligned.coords[,,1] 
  #set up array with appropriate attributes
  coords.array <- array(0, dim = c(nrow(dummy.dim), ncol(dummy.dim), length(species.n)))   #do this only the first time
  #label with species names
  dimnames(coords.array) <- list(c(), c(), c(species.n))  #do this only the first time
  
  
  #### loop section
  #i <-1
  
  for(i in 1:length(species.n)){
    # Species to be worked on for loop i
    fish.sp <-classifier[classifier$Species==species.n[i],]  ##
    if(length(fish.sp$ID)==1){
      dummy1 <- as.numeric(row.names(fish.sp))
      species.coords <- aligned.coords[,,c(dummy1)]
      coords.array[,,species.n[i]] <- species.coords
    }
    else{
      # Rows containing relevant species
      dummy1 <- as.numeric(row.names(fish.sp))                  ## 
      # Array of coordinate data for each individual of pertinent species
      species.coords <- aligned.coords[,,c(dummy1)]           ##
      # matrix of average coordinates data for pertinent species
      mean.species.coords <- mshape(species.coords)            ##
      # put the data in its appropriate place in the array (i.e., for the pert. species)
      coords.array[,,species.n[i]] <- mean.species.coords ##
    }
  }
  print(coords.array)
}
#'
#' Proper syntax for using the function:
#' my.landmark.species.average(classifier = , aligned.coords = )
#' 
########## End #####################
#'
#' The function requires a classifier table to specify which
#' specimens belong to the same species and should be averaged
#' together.
#' 
# Create a classifier table for species averaging
skulltest1.classifier <- as.data.frame(cbind(as.character(rownames(skulltest1.data)), as.character(skulltest1.data$species)), stringsAsFactors = F)
colnames(skulltest1.classifier) <- c("ID", "Species")
#'
#'
#' Now I'll use a custom function for reflecting the coordinates of one side of structures 
#' with paired (matching) symmetry can be averaged with the other side in the bilat.symmetry 
#' function. This function is from Buser et al. 2018:
#' 
#' Buser, T.J., Sidlauskas, B.L. and Summers, A.P., 2018. 2D or not 2D? Testing the 
#' utility of 2D vs. 3D landmark data in geometric morphometrics of the sculpin 
#' subfamily Oligocottinae (Pisces; Cottoidea). The Anatomical Record, 301(5), 
#' pp.806-818.
#' 
# Reading in, reversing, and converting to .tps format, combining left and right sides
my.landmark.array.reflector.plain <- function(landmark.array.reverse){
  
  landmark.array <- landmark.array.reverse
  landmark.array.dummy <- landmark.array
  for(i in 1:length(dimnames(landmark.array)[[3]])){
    landmark.array.dummy[,,i][,1] <- landmark.array[,,i][,1]*(-1)
  }
  return(landmark.array.dummy)
}

##############
#' Custom functions for lineage density analysis adapted from Sidlauskas (2008)
#
#' Euclidean distance between two points.
##' my.matrix = a matrix, where rows represent nodes/points
##' and columns represent axes (e.g., x,y,z, etc.).
##' Returns a single value that is the distance between
##' the two points across n dimensions.
my.dist <- function(my.matrix){
  my.vector <- c()
  for(i in 1:ncol(my.matrix)){
    my.vector[i] <- (my.matrix[2,i] - my.matrix[1,i])^2  
  }  
  return(sqrt(sum(my.vector)))
}
#'
####################
#' 
#' Identify pairs of nodes that make up each edge in a given clade 
#' within a phylogeny.
##' phy = an object of class "phylo"
##' my.range = a range of (internal) node numbers within the phylogeny
##' that include all internal branches within the clade of interest. It
##' is likely necessary to use the plotTree() function with "node.numbers=T"
##' specified in order to ID this numbers. Then they can be expressed as a
##' range when calling the function. E.g., "my.edge.pairs(phy = phy, 
##' my.range = c(20:23))".
##'
my.edge.pairs <- function(phy, my.range){
  edge.pairs <-  phy$edge[phy$edge[,1] == my.range[1],]
  
  for(i in my.range[2]:my.range[length(my.range)]){
    edge.pairs <-  rbind(edge.pairs, phy$edge[phy$edge[,1] == i,]) 
  }
  return(edge.pairs)
}  
#'
####################
#'
#' Create a matrix of PC values for all nodes in a phylogeny, calculated by
#' Ancestral State Reconstruction from tip data.
#' 
##' working.phy = phylogeny of taxa
#' 
##' working.PCcoords = 2D array of PC values for all extant taxa in the 
##' phylogeny
#' 
my.PCA.ASR <- function(working.phy, working.PCcoords){
  # set up a matrix to hold our data
  phylomorph.dummy.holder <- phylomorphospace(working.phy, working.PCcoords[,1:2])
  working.coords.matrix <- matrix(data = NA, nrow = length(phylomorph.dummy.holder$xx), ncol = ncol(working.PCcoords))
  colnames(working.coords.matrix) <- colnames(working.PCcoords)
  rownames(working.coords.matrix) <- c(1:length(phylomorph.dummy.holder$xx))
  is.whole <- function(a) { floor(a)==a }
  #i = 1
  # Now populate the matrix with a loop
  if (is.whole(ncol(working.PCcoords)/2)){
    for(i in seq(from=1, to=ncol(working.PCcoords), by =2)){            
      phylomorphospace.holder <- phylomorphospace(working.phy, working.PCcoords[,i:(i+1)])           
      working.coords.matrix[,i] <- phylomorphospace.holder$xx
      working.coords.matrix[,(i+1)] <- phylomorphospace.holder$yy
      #print(i)
      #print("even")
    }
    #print("That's even!")
    
  }else { 
    for(i in 1:(ncol(working.PCcoords)-2)){
      phylomorphospace.holder <- phylomorphospace(working.phy, working.PCcoords[,i:(i+1)])           
      working.coords.matrix[,i] <- phylomorphospace.holder$xx
    }
    i = (ncol(working.PCcoords)-1)
    #print(i)
    phylomorphospace.holder <- phylomorphospace(working.phy, working.PCcoords[,i:(i+1)])           
    working.coords.matrix[,i] <- phylomorphospace.holder$xx
    working.coords.matrix[,(i+1)] <-  phylomorphospace.holder$yy
    #print(i)
    #print("odd")
    
    #print("That's odd!")
    
  }
  return(working.coords.matrix)
}

#'
####################
#'
#' Morphological distance function.
#' 
#' This is kind of a "wrapper" function that uses the me.edge.pairs, my.dist, 
#' and my.PCA.ASR functions to calculate the total "morpholigical distance" for 
#' a given clade in some specified morphospace.
#' The inputs are the same as for the three functions listed above.
#' 
##' phy = an object of class "phylo"
#' 
##' my.range = a range of (internal) node numbers within the phylogeny
##' that include all internal branches within the clade of interest. It
##' is likely necessary to use the plotTree() function with "node.numbers=T"
##' specified in order to ID this numbers. Then they can be expressed as a
##' range when calling the function. E.g., "my.edge.pairs(phy = phy, 
##' my.range = c(20:23))".
#' 
##' PC.data = 2D array of PC coordinate data for all extant taxa in phylogeny
##' "phy", arranged such that each row represents a node and each column
##' represents the value of said taxon for a given PC axis.
##' 
clade.morpho.dist <- function(phy, my.range, PC.data){
  
  my.node.matrix <- my.edge.pairs(phy = phy, my.range = my.range)
  morpho.data <- my.PCA.ASR(working.phy = phy, working.PCcoords = PC.data)
  #i=1
  morph.dist <- c()
  for(i in 1:nrow(my.node.matrix)){
    node1 <- my.node.matrix[i,1]
    node2 <- my.node.matrix[i,2]
    node.coords.matrix <- matrix(c(morpho.data[node1,],morpho.data[node2,]), nrow = 2, byrow = T)
    morph.dist <- c(morph.dist, my.dist(my.matrix = node.coords.matrix))
  }
  return(sum(morph.dist))
}
#'
##################################################
#'
#'
#' Lineage Densities function
#' 
#' This calculates the three lineage density estimates described in the 
#' supplementary materials of Sidlauskas (2008). LD1 and LD2 are the 
#' two lineage density formulae described in the main text.
#' 
#' Necessary variables:
#' phy : A phylogeny
#' 
#' node : node number of desired clade, could even expand out to include
#' multiple clades if I wanted to.
#' 
#' my.range : range of node number from desired clade.
#' 
#' PCcoords : coordinate values of each PC axis
#' 
my.lineage.densities <- function(phy, node, my.range, PCcoords){
  n <- length(phy$tip.label)
  clade.tips <- prop.part(phy)[[node - n]]
  # okay, now I need to grab the data
  tipmatrix <- as.matrix(PCcoords[clade.tips,])
  ellipsoid <- ellipsoidhull(tipmatrix, maxit = 50000)
  eg <- eigen(ellipsoid$cov)
  vol <- volume(ellipsoid)
  sumvol <- (2*sum(sqrt(eg$values)))*ellipsoid$d
  
  totallength <- clade.morpho.dist(phy = phy, my.range=my.range, PC.data = PCcoords)
  morpho.length <- totallength
  edge.pairs <- my.edge.pairs(phy = phy, my.range = my.range)
  
  ave.length <- morpho.length/nrow(edge.pairs)
  total.volume <- vol
  ldensity1 <- totallength/vol
  ldensity2 <- ((totallength)^(1/ncol(PCcoords)))/sumvol
  ldensity3 <- (totallength/sumvol)
  output.matrix <- matrix(c(morpho.length, total.volume, ldensity1, ldensity2, ldensity3, ave.length), nrow = 1, byrow=T)
  colnames(output.matrix) <- c("morpho.length", "total.volume", "ldensity1", "ldensity2", "ldensity3", "ave.length")
  return(output.matrix)
}

######################################################
#'
#' Data simulator function
#' 
#' This function simulates the evolution of trait data for a clade, then
#' extracts the data for three specified subclades.
#' 
#' dat = PCcoordinate data, this should be the real data that the parameterization
#' and naming of the simulated data will be drawn from
#' 
#' phy = phylogeny of the taxa in dat
#' 
#' node1-3 = the node number of your target (sub)clade within phy
#' 
#' nsim = number of simulations to run
#' 
#' my.range1-3 = sequence of internal node numbers that are in the clades you
#' designated by "node"
#' 
my.data.simulator.three <- function(dat, phy, node1, node2, node3, nsim, 
                                    my.range1, my.range2, my.range3, 
                                    node1.name = "clade1", node2.name = "clade2", node3.name = "clade3", maxPC){
  eve.vcv <- ratematrix(phy = phy, dat = dat[,1:(length(phy$tip.label)-1)])
  
  simulation.output.matrix1 <- matrix(data = NA, nrow = nsim, byrow = T, ncol = 6)
  colnames(simulation.output.matrix1) <- c("morpho.length", "total.volume", "ldensity1", "ldensity2", "ldensity3", "ave.length")
  
  simulation.output.matrix2 <- matrix(data = NA, nrow = nsim, byrow = T, ncol = 6)
  colnames(simulation.output.matrix2) <- c("morpho.length", "total.volume", "ldensity1", "ldensity2", "ldensity3", "ave.length")
  
  simulation.output.matrix3 <- matrix(data = NA, nrow = nsim, byrow = T, ncol = 6)
  colnames(simulation.output.matrix3) <- c("morpho.length", "total.volume", "ldensity1", "ldensity2", "ldensity3", "ave.length")
  
  
  for(i in 1:nsim){
    sim.char.holder <- sim.char(phy = phy, par = eve.vcv, model = "BM")
    sim.char.holder.subset1 <- sim.char.holder[,,1][,1:maxPC]
    colnames(sim.char.holder.subset1) <- 1:maxPC
    sim.char.holder.subset2 <- sim.char.holder[,,1][,1:maxPC]
    sim.char.holder.subset3 <- sim.char.holder[,,1][,1:maxPC]
    
    test.holder.sim1 <- my.lineage.densities(phy = phy, node = node1, my.range= my.range1, 
                                             PCcoords = sim.char.holder.subset1)
    test.holder.sim2 <- my.lineage.densities(phy = phy, node = node2, my.range= my.range2, 
                                             PCcoords = sim.char.holder.subset2)
    test.holder.sim3 <- my.lineage.densities(phy = phy, node = node3, my.range= my.range3, 
                                             PCcoords = sim.char.holder.subset3)
    
    
    simulation.output.matrix1[i,] <- test.holder.sim1
    simulation.output.matrix2[i,] <- test.holder.sim2
    simulation.output.matrix3[i,] <- test.holder.sim3
  }
  output.list <- (list(simulation.output.matrix1, simulation.output.matrix2, simulation.output.matrix3))
  names(output.list) <- c(node1.name, node2.name, node3.name)
  return(output.list)
}
#'
#'
#' Now some custom functions from Larouche et al. 2022:
#' 
#' Larouche, O., S. M. Gartner, M. W. Westneat, and K. M. Evans. 2022.
#' Mosaic Evolution of the Skull in Labrid Fishes Involves Differences 
#' in both Tempo and Mode of Morphological Change. Systematic Biology.
#' 
################################
# Translating the individually-superimposed, scaled coordinate sets to the centroid position of their corresponding bone in the common superimposition
# 'coords' is a 3D coordinate array, centroid is a
translate.lm <- function(coords,centroid){
  dummy.array<-array(NA,dim=c(dim(coords)[[1]],dim(coords)[[2]],dim(coords)[[3]]))
  dummy.array[,1,]<-coords[,1,]+centroid[1]
  dummy.array[,2,]<-coords[,2,]+centroid[2]
  dummy.array[,3,]<-coords[,3,]+centroid[3]
  dimnames(dummy.array)[[3]]<-dimnames(coords)[[3]]
  return(dummy.array)
}

# Finding the optimal rotation between one set of 3D coordinates and another, and rotating one set to minimize the sum of squared distances between the coordinate sets
# code was adapted from the 'fPsup' function from Claude's 'Morphometrics with R' ... (which I think I have the pdf of if needed/wanted, a very helpful resource)
rot2ref <- function(M, Mref){
  k <- ncol(M)
  Z1 <- M
  Z2 <- Mref
  sv <- svd(t(Z2)%*%Z1)
  V <- sv$v; U <- sv$u; Delt <- sv$d
  Gam<-V%*%t(U)
  list(coords=Z1%*%Gam, rotation=Gam)
}

# for finding the optimal rotation between two shapes, there's also the 'rotonto' function in the package 'Morpho' that does the same thing.
# I've found that both my 'rot2ref' and Morpho's 'rotonto' don't ALWAYS perfectly rotate the landmarks, but usually one of them rotates the coords correctly
# this is why there's 'matchLM' and 'matchLM2', one for each rotation function
# and when both don't work perfectly (like the dentary), I manually rotate the landmarks as close as possible using the 'rotate.lm' function and then try again until it works (it always does eventually, just gets tedious rotating  so much)


### if needed, this function just rotates each lm configuration in an array by the same amount
rotate.lm <- function(array, deg.y, deg.z, deg.x) {
  x <- vector("list",dim(array)[3])
  for (i in 1:dim(array)[3])
  {
    x[[i]] <- 
      rotate(x=as.vector(array[,1,i]),
             y= as.vector(array[,2,i]),
             z= as.vector(array[,3,i]),
             degry=deg.y,degrz=deg.z,degrx=deg.x)
  }
  y <- bindArr(lapply(x,FUN=as.matrix),along=3)
  return(y)
}

# function that inputs two arrays (individually-superimposed coords and subset of common superimposition) and translates, scales, and rotates one to another
# relies on the functions 'translate.lm', 'rot2ref' or 'rotonto'
# array: a (P x K x N) array of the locally superimposed landmark configurations
# ref:  a (P x K x N) array of the corresponding bone from the common superimposition
matchLM <- function(array,ref)
{
  p <- dim(array)[1]
  k <- dim(array)[2]
  n <- dim(array)[3]
  M <- array
  ref.mean <- mshape(ref)
  ref.centroid <- c(mean(ref.mean[,1]),mean(ref.mean[,2]),mean(ref.mean[,3]))
  ref.cSize <- cSize(ref.mean)
  M.ts <- translate.lm(M*ref.cSize,ref.centroid)
  M.ts.rot.all <- array(NA,dim=c(p,k,n))
  for(i in 1:n) {M.ts.rot.all[,,i] <- rotonmat(M.ts[,,i],mshape(M.ts), ref.mean, reflection = TRUE)}
  return(M.ts.rot.all)
}


matchLM2 <- function(array,ref)
{
  p <- dim(array)[1]
  k <- dim(array)[2]
  n <- dim(array)[3]
  M <- array
  ref.mean <- mshape(ref)
  ref.centroid <- c(mean(ref.mean[,1]),mean(ref.mean[,2]),mean(ref.mean[,3]))
  ref.cSize <- cSize(ref.mean)
  M.ts <- translate.lm(M*ref.cSize,ref.centroid)
  ts.mean.rotated <- rotonto(mshape(M.ts),ref.mean)
  rotation <- ts.mean.rotated$gamm
  M.ts.rot.all <- array(NA,dim=c(p,k,n))
  for(i in 1:n) {M.ts.rot.all[,,i] <- M.ts[,,i]%*%rotation}
  return(M.ts.rot.all)
}
############################################
#
#
### Custom functions -- End
# 
###############################################################################'
###############################################################################'
##'
##'
###############################################################################'
########## BAIKAL TAXA INCLUDED ###############################################'
###############################################################################'
##'
##'
##'
#' Anatomical Data - begin
##'
##' 
#' Read-in landmark coordinate data
Sculp.skull.full.slide <- readland.tps(file = "SculpinSkullData.tps", specID = "ID", warnmsg =F)
dimnames(Sculp.skull.full.slide)[[2]] <- c("x", "y","z")
#' 
#' Now I'll make a geomorph data frame to hold the landmark data (shape data) and the
#' individual (species) of each specimen. 
gdf.fullslide <- geomorph.data.frame(shape = Sculp.skull.full.slide, ind=dimnames(Sculp.skull.full.slide)[[3]])
#'
#' Now we'll perform the Generalized Procrustes Analysis, analysis of symmetry, and
#' semilandmark sliding all at once. Because we are allowing the semilandmarks
#' to slide along the curve in 3D. We'll use the results for our future analyses.
skull.lm.fullslide.sym <- bilat.symmetry(A = shape, ind = ind, object.sym = TRUE,
                                         
                                         land.pairs=skull.land.pairs.fullslide, 
                                         
                                         curves = skull.curvesliders, ProcD = F,
                                         
                                         data = gdf.fullslide, RRPP = TRUE,
                                         
                                         iter = 499, print.progress = T)
#'
#'
#'
### Now we'll make a loop to do the operation for all paired bones
#'
#' First, we'll make a list of all of the skeletal units (we'll call
#' them "bones" for simplicity).
bone.list <- unique(as.character(Bone_LM_LHS$Bone))
#'
for(i in c(1:6,8,10)){
  #Grab the landmark data for a each fish for a given bone
  working_LM_bone_LHS <- Sculp.skull.full.slide[Bone_LM_LHS[Bone_LM_LHS$Bone ==bone.list[i],2],,]
  rownames(working_LM_bone_LHS) <- 1:nrow(working_LM_bone_LHS)
  #
  working_LM_bone_RHS <- Sculp.skull.full.slide[Bone_LM_RHS[Bone_LM_RHS$Bone ==bone.list[i],2],,]
  rownames(working_LM_bone_RHS) <- 1:nrow(working_LM_bone_RHS)
  #'
  #' Gonna combine LHS and RHS, so gonna give them separate names
  for (k in 1:length(dimnames(working_LM_bone_RHS)[[3]])){
    dimnames(working_LM_bone_RHS)[[3]][k] <- paste(dimnames(working_LM_bone_RHS)[[3]][k],"_RHS", sep = "")
  }
  #'
  #'Reverse the RHS using a custom function
  working_LM_bone_RHS.rev <- my.landmark.array.reflector.plain(working_LM_bone_RHS)
  #
  # Combine the left and right side arrays
  working_LM_bone_combined <- abind(working_LM_bone_LHS, working_LM_bone_RHS.rev, along = 3)
  #'
  # Make a slider matrix
  working_sliders_bone <-  Bone_Slider_LHS2[Bone_Slider_LHS2$Bone == bone.list[i],2:4]
  rownames(working_sliders_bone) <- c()
  working_sliders_bone <- as.matrix(working_sliders_bone[,1:3])
  #'
  working.data <- data.frame(side= as.vector( c(rep("left", length(dimnames(working_LM_bone_LHS)[[3]])), rep("right", length(dimnames(working_LM_bone_RHS)[[3]])))),
                             isolate = as.factor(dimnames(working_LM_bone_LHS)[[3]]), row.names = dimnames(working_LM_bone_combined)[[3]])
  #'
  working.bone.gdf <- geomorph.data.frame(coords= working_LM_bone_combined,
                                          side= working.data$side,
                                          isolate = working.data$isolate)
  
  if(nrow(working_sliders_bone) > 0){
    
    working.bone.gpa.sym <- bilat.symmetry(A= working.bone.gdf$coords,
                                           ind = working.bone.gdf$isolate,
                                           side = as.character(working.bone.gdf$side),
                                           curves = working_sliders_bone,
                                           ProcD = F)
  }
  else(
    working.bone.gpa.sym <- bilat.symmetry(A= working.bone.gdf$coords,
                                           ind = working.bone.gdf$isolate,
                                           side = as.character(working.bone.gdf$side),
                                           ProcD = F)
  )
  working.bone.gpa.sym.sppAve <- my.landmark.species.average(classifier = skulltest1.classifier, aligned.coords = working.bone.gpa.sym$symm.shape)
  
  assign(bone.list[i], working.bone.gpa.sym.sppAve, envir = .GlobalEnv)
  }
#' 
# Now I need to process the medial bones- the neurocranium and urohyal
#'
#First, we'll do the Urohyal
i = 9
working_LM_bone_LHS <- Sculp.skull.full.slide[Bone_LM_LHS[Bone_LM_LHS$Bone ==bone.list[i],2],,]
rownames(working_LM_bone_LHS) <- 1:nrow(working_LM_bone_LHS) #only LHS landmarks for urohyal
#'
#' Make a slider matrix
working_sliders_bone <-  Bone_Slider_LHS2[Bone_Slider_LHS2$Bone == bone.list[i],2:4]
rownames(working_sliders_bone) <- c()
working_sliders_bone <- as.matrix(working_sliders_bone[,1:3])
#'
#'
working.bone.gpa.sym <- gpagen(A= working_LM_bone_LHS,
                               curves = working_sliders_bone,
                               ProcD = F)
working.bone.gpa.sym.sppAve <- my.landmark.species.average(classifier = skulltest1.classifier, aligned.coords = working.bone.gpa.sym$coords)
assign(bone.list[i], working.bone.gpa.sym.sppAve, envir = .GlobalEnv)
#'
#' Okay now the neurocranium
i = 7
#
working_LM_bone_LHS <- Sculp.skull.full.slide[Bone_LM_LHS[Bone_LM_LHS$Bone ==bone.list[i],2],,]
rownames(working_LM_bone_LHS) <- 1:nrow(working_LM_bone_LHS)
#
working_LM_bone_RHS <- Sculp.skull.full.slide[Bone_LM_RHS[Bone_LM_RHS$Bone ==bone.list[i],2],,]
rownames(working_LM_bone_RHS) <- (nrow(working_LM_bone_LHS)+1):((nrow(working_LM_bone_LHS))+nrow(working_LM_bone_RHS))
#'
working_sliders_bone_LHS <-  Bone_Slider_LHS2[Bone_Slider_LHS2$Bone == bone.list[i],2:4]
working_sliders_bone_RHS <-  Bone_Slider_RHS2[Bone_Slider_RHS2$Bone == bone.list[i],2:4]
working_slider_bone_combined <- rbind(working_sliders_bone_LHS, working_sliders_bone_RHS)

rownames(working_slider_bone_combined) <- c()
working_slider_bone_combined <- as.matrix(working_slider_bone_combined[,1:3])
#'
#'
working.LMs.bone = array(dim = c(((nrow(working_LM_bone_LHS))+nrow(working_LM_bone_RHS)), 
                                 3, length(dimnames(working_LM_bone_LHS)[[3]])),
                         dimnames = list(1:((nrow(working_LM_bone_LHS))+nrow(working_LM_bone_RHS)), 
                                         c("x", "y","z"), as.character(dimnames(working_LM_bone_LHS)[[3]])))

for (m in 1:length(dimnames(working_LM_bone_LHS)[[3]])) {
  landmark.LHS <- working_LM_bone_LHS[,,m]
  landmark.RHS <- working_LM_bone_RHS[,,m]
  working.LMs.bone[,,m] <- abind(landmark.LHS, landmark.RHS, along = 1)
}


working.isolate <- as.factor(dimnames(working.LMs.bone)[[3]])

working.gdf <- geomorph.data.frame(shape = working.LMs.bone, ind = working.isolate)

working.land.pairs <- as.matrix(read.csv(file = "neurocraniumLM_corresponences.csv", header = F))

working.lm.gpa.sym <- bilat.symmetry(A = shape, ind = ind, object.sym = TRUE, 
                                     
                                     curves = working_slider_bone_combined, 
                                     
                                     land.pairs=working.land.pairs, 
                                     
                                     data = working.gdf, RRPP = TRUE,
                                     
                                     iter = 999, print.progress = FALSE, 
                                     
                                     ProcD = F) # perform object symmetry GPA summary(scallop.sym)


working.bone.gpa.sym.sppAve <- my.landmark.species.average(classifier = skulltest1.classifier, aligned.coords = working.lm.gpa.sym$sym)

assign(bone.list[i], working.bone.gpa.sym.sppAve, envir = .GlobalEnv)
#'
#'
#'
########################'
############# BEGIN BONES MORPHOSPACE LOOP: This section performs
##' a principal component analysis of the landmark data for each
##' skeletal unit of the skull individually.
##' 
##' We'll need a phylogenetic hypothesis for some of the steps, so we'll start
##' by reading in the pertinent subtree of the Rabosky phylogeny
cottoid.tree <- fishtree_phylogeny(rank = "Cottoidei", type = "chronogram_mrca")
#'
#' We'll make some reasonable assumptions about the phylogenetic placement of some taxa 
#' that were not included in the fish tree.
cottoid.tree$tip.label[175] <- "Hemitripterus_bolini"
cottoid.tree$tip.label[31] <- "Myoxocephalus_niger"
cottoid.tree$tip.label[157] <- "Percis_japonica"
#'
#' We'll trim the phylogeny
keep.tips <- as.character(row.names(skulltest1.species.data))
pruned.test.tree <- keep.tip(cottoid.tree, keep.tips)
pruned.test.tree.notrich <- drop.tip(pruned.test.tree, tip = "Trichodon_trichodon" )
skulltest1.species.data.phy <- skulltest1.species.data[pruned.test.tree$tip.label,]
#
#############' Now we'll paint the clades by taxonomic family
paint.tree <- paintSubTree(tree = pruned.test.tree, node = c(188), 
                           state = "Agonidae", anc.state = "0")

paint.tree <- paintSubTree(tree = paint.tree, node = c(186), 
                           state = "Agonidae")

paint.tree <- paintSubTree(tree = paint.tree, node = 109, 
                           state = "Psychrolutidae")

paint.tree <- paintSubTree(tree = paint.tree, node = 157, 
                           state = "Cottidae")

paint.tree <- paintSubTree(tree = paint.tree, node = 79, 
                           state = "Jordaniidae", stem = T)

paint.tree <- paintSubTree(tree = paint.tree, node = 103, 
                           state = "Rhamphocottidae", stem = T)

paint.tree <- paintSubTree(tree = paint.tree, node = 104, 
                           state = "Trichodontidae", stem = T)

palette(c("black", "darkorange", "darkolivegreen",  "darkslategray3", "dodgerblue3", "deeppink", "firebrick3"))

#############################################'
######## THIS CORRESPONDS TO FIGURE 1 #######'
#############################################'
#############################################'
#' Plot the phylogeny, color-coded by taxonomic family. THIS
#' CORRESPONDS TO FIGURE 1
plot(paint.tree,ftype="off",lwd=3)
dev.off()
#############################################'
#############################################'
#############################################'
#
# Individual bone GPA and PCA
for(i in 1:length(bone.list)){
  
  assign(paste(bone.list[i],".pca", sep = ""),
         gm.prcomp(get(bone.list[i])),      
         envir = .GlobalEnv)
  
  working.gpa <- get(bone.list[i])
  working.gpa.coords <- working.gpa[,, rownames(skulltest1.species.data)]
  
  assign(paste(bone.list[i],".mshape.coords", sep = ""),
         mshape(get(bone.list[i])),
         envir = .GlobalEnv)
  }
#' 
#' Now we'll perform the local superimposition
#' 
#The full configuration, which will be used as the template to superimpose parts
skull_sup <- skull.lm.fullslide.sym$symm.shape
skull_sup_mean <- mshape(skull.lm.fullslide.sym$symm.shape)

# Use a loop to assemble the shape data for each bone unit
for(l in c(1:10)){
  assign(
    paste(bone.list[l],"_sup", sep = ""),
    get(bone.list[l]),
    envir = .GlobalEnv
  )
}
#'
#' Also grab data for full skull
skull_sup <- my.landmark.species.average(classifier = skulltest1.classifier, aligned.coords = skull.lm.fullslide.sym$symm.shape)
#'
#' Now match landmark data for each bone (other than neurocranium) to that bone's 
#' position on the skull 
for(l in c(1:6,8:10)){
  
  bone.sup.holder <- get(paste(bone.list[l],"_sup", sep = ""))
  bone.coords.holder <- skull_sup[Bone_LM_LHS[Bone_LM_LHS == bone.list[l],2],,]
  #  bone.matched.holder <- matchLM(bone.sup.holder, bone.coords.holder)
  assign(
    paste(bone.list[l],".matched", sep = ""),
    matchLM(bone.sup.holder, bone.coords.holder),
    envir = .GlobalEnv
  )
}
#'
#' Now let's try the neurocranium, which is more complex due to object symmetry
l <- 7
bone.sup.holder <- get(paste(bone.list[l],"_sup", sep = ""))
bone.coords.holder.LHS <- skull_sup[Bone_LM_LHS[Bone_LM_LHS == bone.list[l],2],,]
bone.coords.holder.RHS <- skull_sup[Bone_LM_RHS[Bone_LM_RHS == bone.list[l],2],,]


bone.coords.holder.combined <- array(dim = c(((nrow(bone.coords.holder.LHS))+nrow(bone.coords.holder.RHS)), 
                                             3, length(dimnames(bone.coords.holder.LHS)[[3]])),
                                     dimnames = list(1:((nrow(bone.coords.holder.LHS))+nrow(bone.coords.holder.RHS)), 
                                                     c("x", "y","z"), as.character(dimnames(bone.coords.holder.LHS)[[3]])))


for (m in 1:length(dimnames(bone.coords.holder.LHS)[[3]])) {
  landmark.LHS <- bone.coords.holder.LHS[,,m]
  landmark.RHS <- bone.coords.holder.RHS[,,m]
  bone.coords.holder.combined[,,m] <- abind(landmark.LHS, landmark.RHS, along = 1)
}


assign(
  paste(bone.list[l],".matched", sep = ""),
  matchLM(bone.sup.holder, bone.coords.holder.combined),
  envir = .GlobalEnv
)

#Combining the configurations
skull_local_sup<-combine.subsets(Angular.matched, Dentary.matched, Hyoid.matched, Hyomandibula.matched, Maxilla.matched,
                                 Nasal.matched, Neurocranium.matched, Premax.matched,Urohyal.matched, Preopercle.matched,   gpa=FALSE, CS.sets = NULL)
skull_locsup_coords<-skull_local_sup$coords
rownames(skull_locsup_coords)<-c(1:249)
dimnames(skull_locsup_coords)[3]<-dimnames(skull_sup)[3]
#
##'
##'
#'    Anatomical Data - end
###############################################################################'
###############################################################################'
##'
#'  Analysis of shape- begin
##'
##'
#' Okay, now we'll generate some figures for the local superimposition dataset. 
skull_locsup.pca <- gm.prcomp(skull_locsup_coords)  
summary(skull_locsup.pca)

write.csv(skull_locsup.pca$x, "skull_locsup.pca.axes.csv")
write.csv(skull_locsup.pca$x[,1:20], "skull_locsup.pca.axes20.csv")
#'
##'
##'
#'    Analysis of shape - end
###############################################################################'
###############################################################################'
##'
#'  Phylogenetic comparative methods- begin
##'
##'
#' PHYLOMORPHOSPACE
#############################################'
#### THIS CORRESPONDS TO FIGURE 2 ###########'
#############################################'
#############################################'
working.pca <- skull_locsup.pca
working.pca.phycoords <- working.pca$x[pruned.test.tree$tip.label,]

phylomorphospace(tree = paint.tree,
                 X = working.pca.phycoords[,1:2], label = "off",
                 node.size=c(0,1.2),node.by.map=TRUE)
legend(x="topleft",legend=c("Agonidae", "Cottidae", "Jordaniidae", "Psychrolutidae", "Rhamphocottidae", "Trichodontidae"),
       pch=21,pt.cex=1.5,pt.bg=palette()[2:7],bty="n")
title(main = "Local Superimposition Full Skull")
#'
#'
#' Here is a version of the phylomorphospace, color-coded by
#' freshwater vs. saltwater taxa
#'
#' Re-organize the habitat data table
sculpin.habitat.table <- sculpin.habitat.table[rownames(skulltest1.species.data.phy),]
#'
#' First, we'll make a subset of the coordinate data that matches the taxa
#' and order of the taxa in the phylogeny
skull_locsup_coords.phy <- skull_locsup_coords[,,pruned.test.tree$tip.label]
skull_locsup_coords.phy.2darray <- two.d.array(skull_locsup_coords.phy)
#' Isolate the freshwater vs saltwater variable
sculp.habitat <- sculpin.habitat.table$Fresh
names(sculp.habitat) <- rownames(skulltest1.species.data.phy)
names(sculp.habitat) == dimnames(skull_locsup_coords.phy)[[3]] #double check
working.axes <- working.pca.phycoords[,1:2]
phylomorphospace(tree = paint.tree,
                 X = working.axes, label = "off",
                 node.size=c(0,1.2),node.by.map=F)

points(working.axes[,1], working.axes[,2], pch = 21, cex = 1.5, bg = as.character(sculp.habitat))
#############################################'
#############################################'
#'
#'
######' This is the OUTLIER analysis referenced in the main text #####
######################################################################'
### Check for outliers ##############'
plotOutliers(skull_locsup_coords)
# Comephorus baikalensis is the Lake Baikal Oilfish.
#####################################'
#####################################'
#'
#'
##############' Phylogenetic MANOVA
#'
#'
#' Next, isolate family variable
sculp.families <- skulltest1.species.data.phy$family
names(sculp.families) <- rownames(skulltest1.species.data.phy)
#
#' Now make a geomorph data frame to hold the data
gdf.phyANOVA <- geomorph.data.frame(coords = skull_locsup_coords.phy, family = sculp.families, habitat = sculp.habitat)
#'
################
#################' Now run the analysis for habitat
habitat.ANOVA <- procD.pgls(coords ~ habitat, phy = pruned.test.tree, data = gdf.phyANOVA)
summary(habitat.ANOVA)
############################'
############################' Now run the analysis for family
family.ANOVA <- procD.pgls(coords ~ family, phy = pruned.test.tree, data = gdf.phyANOVA)
summary(family.ANOVA)
#'
######' This is the DISPARITY analysis referenced in the main text #####
######################################################################'
############ COMPARING DISPARITY
#'
#' First compare disparity of freshwater vs. saltwater
morphol.disparity(f1 = skull_locsup_coords.phy~1 + sculp.families, groups = sculp.families)
#'
#' Next, compare disparity of family groups
morphol.disparity(f1 = skull_locsup_coords.phy~1 + sculp.families, groups = sculp.families)
#
###########################################################
#'
#' Now let's look at disparity through time of each of the three major sculpin
#' families
#'
#' First, make some new sortings of the coordinate data
pruned.test.tree$tip.label == row.names(skull_locsup.pca$x)
skull_locsup.pca.coords.phy <- skull_locsup.pca$x[pruned.test.tree$tip.label,]
sum(pruned.test.tree$tip.label == row.names(skull_locsup.pca.coords.phy))
#'
#' We'll start with Agonidae, an exclusively marine group
agonidae.tree <- extract.clade(pruned.test.tree, node = 188)
plotTree(agonidae.tree)
agonidae.data <- skull_locsup.pca.coords.phy[agonidae.tree$tip.label,]
dev.off()
dtt(phy = agonidae.tree, data = agonidae.data, index = "avg.sq", nsim = 999)
title(main= "DTT Agonidae")
#'
#'
#' Next, the primarily freshwater family, Cottidae
cottidae.tree <- extract.clade(pruned.test.tree, node = 157)
plotTree(cottidae.tree)
cottidae.data <- skull_locsup.pca.coords.phy[cottidae.tree$tip.label,]
dev.off()
dtt(phy = cottidae.tree, data = cottidae.data, index = "avg.sq", nsim = 999)
title(main= "DTT Cottidae")
#'
#'
#' Finally, the primarily saltwater family, Psychrolutidae
psychrolutidae.tree <- extract.clade(pruned.test.tree, node = 109)
plotTree(psychrolutidae.tree)
psychrolutidae.data <- skull_locsup.pca.coords.phy[psychrolutidae.tree$tip.label,]
dev.off()
dtt(phy = psychrolutidae.tree, data = psychrolutidae.data, index = "avg.sq", nsim= 999)
title(main= "DTT Psychrolutidae")
##'
##'
#'    Phylogenetic comparative methods - end
###############################################################################'
###############################################################################'
##'
#'  Hypothesis 1: differing rates of morph evol- begin
##'
##'
#
#######' This is the MORPHOLOGICAL EVOLUTIONARY RATES analysis referenced #####
#######'  in the main text ####################################################
######################################################################'
#'
#' First, compare freshwater vs saltwater
SculpHabitats.rates <- compare.evol.rates(A = skull_locsup_coords.phy, 
                                          gp = sculp.habitat,
                                          method = "simulation", phy = pruned.test.tree,
                                          iter=999)
summary(SculpHabitats.rates)
#'
#'
#' Next, compare family groups
SculpFamilies.rates <- compare.evol.rates(A = skull_locsup_coords.phy, 
                                          gp = sculp.families,
                                          method = "simulation", phy = pruned.test.tree,
                                          iter=999)
summary(SculpFamilies.rates)
#'
##'
##'
#'    Hypothesis 1: differing rates of morph evol - end
###############################################################################'
###############################################################################'
##'
#'  Hypothesis 2: differing mode of morph evol (lineage density)
##'
##'
#
############ LINEAGE DENSITY
#
# PCA data from local superimposition dataset
sculp.skull_locsup.pca.coords <- working.pca.phycoords
#'
plotTree(pruned.test.tree, node.numbers = T)
#
# Take note of the node number of each of the three major families
sculp.skull_locsup.pca.coords <- sculp.skull_locsup.pca.coords[pruned.test.tree$tip.label,]
#' 
#' Agonidae = node 188, 188:207
#' Cottidae = node 157, 157:185
#' Psychrolutidae = node 109, 109:155
#
#' 
node1 <-188
node2<- 157
node3 <- 109
#
dev.off()
#' 
phy <- pruned.test.tree
n <- length(phy$tip.label)
#'
#' Lineage denisty calculations are heavily influenced by ultra-low volume
#' axes, so we reduce the number of axes to 4 (see main text).
Max.PCs <- 4

Agonidae.obs <- my.lineage.densities(phy = pruned.test.tree, node = node1, 
                                     my.range=c(188:207), PCcoords = sculp.skull_locsup.pca.coords[,1:Max.PCs])

Cottidae.obs <- my.lineage.densities(phy = pruned.test.tree, node = node2,
                                     my.range=c(157:185), PCcoords = sculp.skull_locsup.pca.coords[,1:Max.PCs])

Psychrolutidae.obs <- my.lineage.densities(phy = pruned.test.tree, node = node3,
                                           my.range=c(109:155), PCcoords = sculp.skull_locsup.pca.coords[,1:Max.PCs])
#'
#' Observed data holder
Obs.data.list <- list(Agonidae = Agonidae.obs,
                      Cottidae = Cottidae.obs,
                      Pyschrolutidae = Psychrolutidae.obs)
#'
#' Now we'll simulate data to calculate a test statistic
simulated.data.list <- my.data.simulator.three(dat =sculp.skull_locsup.pca.coords, phy=pruned.test.tree, 
                                               node1 = node1,  node2 = node2, node3 = node3, nsim = 100, my.range1 = c(188:207), 
                                               my.range2 = c(157:185), my.range3 = c(109:155), node1.name = "Agonidae", node2.name = "Cottidae",
                                               node3.name = "Psychrolutidae", maxPC = Max.PCs)
##########################################
#' Now, let's try to do some of the clade comparisons from
#' Sidlauskas (2008).


Obs.ratios <- list(Psychrolutid.Cottid.obs = Psychrolutidae.obs/Cottidae.obs,
                   Cottid.Psychrolutid.obs = Cottidae.obs/Psychrolutidae.obs,
                   Pyschrolutid.Agonid.obs = Psychrolutidae.obs/Agonidae.obs,
                   Agonid.Cottid.obs = Agonidae.obs/Cottidae.obs)
#
Sim.ratios <- list(Psychrolutid.Cottid = simulated.data.list$Psychrolutidae / simulated.data.list$Cottidae,
                   Cottid.Psychrolutid = simulated.data.list$Cottidae / simulated.data.list$Psychrolutidae,
                   Pyschrolutid.Agonid = simulated.data.list$Psychrolutidae / simulated.data.list$Agonidae,
                   Agonid.Cottid =  simulated.data.list$Agonidae / simulated.data.list$Cottidae)
#
true.false.test <- Sim.ratios
for(i in 1:ncol(Obs.ratios$Psychrolutid.Cottid)){
  true.false.test$Psychrolutid.Cottid[,i] <- Obs.ratios$Psychrolutid.Cottid.obs[,i] < Sim.ratios$Psychrolutid.Cottid[,i]
  true.false.test$Cottid.Psychrolutid[,i] <- Obs.ratios$Cottid.Psychrolutid.obs[,i] < Sim.ratios$Cottid.Psychrolutid[,i]
  true.false.test$Pyschrolutid.Agonid[,i] <- Obs.ratios$Pyschrolutid.Agonid.obs[,i] < Sim.ratios$Pyschrolutid.Agonid[,i]
  true.false.test$Agonid.Cottid[,i] <- Obs.ratios$Agonid.Cottid.obs[,i] < Sim.ratios$Agonid.Cottid[,i]
}
#
p.values <- Obs.ratios
names(p.values) <- c("Psychrolutid.Cottid", "Cottid.Psychrolutid", "Pyschrolutid.Agonid" , "Agonid.Cottid")
for(i in 1:ncol(Obs.ratios$Psychrolutid.Cottid.obs)){
  p.values$Psychrolutid.Cottid[,i] <- mean(true.false.test$Psychrolutid.Cottid[,i])
  p.values$Cottid.Psychrolutid[,i] <- mean(true.false.test$Cottid.Psychrolutid[,i])
  p.values$Pyschrolutid.Agonid[,i] <- mean(true.false.test$Pyschrolutid.Agonid[,i])
  p.values$Agonid.Cottid[,i] <- mean(true.false.test$Agonid.Cottid[,i])
}
p.values


###############################################################################'
########## BAIKAL TAXA EXCLUDED ###############################################'
###############################################################################'

######### Remove Baikal taxa and rerun all analyses
##'
###############################################################################'
###############################################################################'
##'
#'  Anatomical data- begin
##'
##'
###' REMOVING THE BAIKAL TAXA
###' 
# Remove Baikal sculpins from phylogeny
pruned.test.tree <- drop.tip(pruned.test.tree, c("Cottocomephorus_grewingkii", "Cottocomephorus_inermis", "Leocottus_kesslerii", "Paracottus_knerii", "Comephorus_baikalensis"))
#'
#' Remove Baikal sculpins from data sets
skulltest1.data <- skulltest1.data[ skulltest1.data$species %in% pruned.test.tree$tip.label,]
#
Sculp.skull.full.slide <- Sculp.skull.full.slide[,,rownames(skulltest1.data)]
#
# Now modify the classifier
skulltest1.classifier
rownames(skulltest1.classifier) <- skulltest1.classifier$ID
skulltest1.classifier <- skulltest1.classifier[dimnames(Sculp.skull.full.slide)[[3]],]
rownames(skulltest1.classifier) <- 1:nrow(skulltest1.classifier)
#
# Now modify the species-only dataset
skulltest1.species.data <- skulltest1.species.data[pruned.test.tree$tip.label,]
#
############## Analysis of the full skull
#'
#' Now I'll make a geomorph data frame to hold the landmark data (shape data) and the
#' individual (species) of each specimen. 
#gdf.fullslide <- geomorph.data.frame(shape = Sculp.skull.full.slide, ind=skulltest1.data$species)
gdf.fullslide <- geomorph.data.frame(shape = Sculp.skull.full.slide, ind=dimnames(Sculp.skull.full.slide)[[3]]) # Not averaging species data YET. May do so in a seperate script in the future...
#'
#' Now we'll perform the Generalized Procrustes Analysis, analysis of symmetry, and
#' semilandmark sliding all at once. Because we are allowing the semilandmarks
#' to slide along the curve in 3D.
skull.lm.fullslide.sym <- bilat.symmetry(A = shape, ind = ind, object.sym = TRUE,
                                         
                                         land.pairs=skull.land.pairs.fullslide, 
                                         
                                         curves = skull.curvesliders, ProcD = F,
                                         
                                         data = gdf.fullslide, RRPP = TRUE,
                                         
                                         iter = 499, print.progress = T) # perform object symmetry GPA summary
#'
skull.lm.fullslide.sym.PCA <- gm.prcomp(skull.lm.fullslide.sym$symm.shape)  
#
plotOutliers(skull.lm.fullslide.sym$symm.shape)
#
### Now use a loop to do the whole operation for me for all paired bones
for(i in c(1:6,8,10)){
  #Grab the landmark data for a each fish for a given bone
  working_LM_bone_LHS <- Sculp.skull.full.slide[Bone_LM_LHS[Bone_LM_LHS$Bone ==bone.list[i],2],,]
  rownames(working_LM_bone_LHS) <- 1:nrow(working_LM_bone_LHS)
  #
  working_LM_bone_RHS <- Sculp.skull.full.slide[Bone_LM_RHS[Bone_LM_RHS$Bone ==bone.list[i],2],,]
  rownames(working_LM_bone_RHS) <- 1:nrow(working_LM_bone_RHS)
  #'
  #' Gonna combine LHS and RHS, so gonna give them separate names
  for (k in 1:length(dimnames(working_LM_bone_RHS)[[3]])){
    dimnames(working_LM_bone_RHS)[[3]][k] <- paste(dimnames(working_LM_bone_RHS)[[3]][k],"_RHS", sep = "")
  }
  #'
  #'Reverse the RHS using a custom function
  working_LM_bone_RHS.rev <- my.landmark.array.reflector.plain(working_LM_bone_RHS)
  #
  # Combine the left and right side arrays
  working_LM_bone_combined <- abind(working_LM_bone_LHS, working_LM_bone_RHS.rev, along = 3)
  #'
  # Make a slider matrix
  working_sliders_bone <-  Bone_Slider_LHS2[Bone_Slider_LHS2$Bone == bone.list[i],2:4]
  rownames(working_sliders_bone) <- c()
  working_sliders_bone <- as.matrix(working_sliders_bone[,1:3])
  #'
  working.data <- data.frame(side= as.vector( c(rep("left", length(dimnames(working_LM_bone_LHS)[[3]])), rep("right", length(dimnames(working_LM_bone_RHS)[[3]])))),
                             isolate = as.factor(dimnames(working_LM_bone_LHS)[[3]]), row.names = dimnames(working_LM_bone_combined)[[3]])
  #'
  working.bone.gdf <- geomorph.data.frame(coords= working_LM_bone_combined,
                                          side= working.data$side,
                                          isolate = working.data$isolate)
  
  if(nrow(working_sliders_bone) > 0){
    
    working.bone.gpa.sym <- bilat.symmetry(A= working.bone.gdf$coords,
                                           ind = working.bone.gdf$isolate,
                                           side = as.character(working.bone.gdf$side),
                                           curves = working_sliders_bone,
                                           ProcD = F)
  }
  else(
    working.bone.gpa.sym <- bilat.symmetry(A= working.bone.gdf$coords,
                                           ind = working.bone.gdf$isolate,
                                           side = as.character(working.bone.gdf$side),
                                           ProcD = F)
  )
  #
  
  
  
  working.bone.gpa.sym.sppAve <- my.landmark.species.average(classifier = skulltest1.classifier, aligned.coords = working.bone.gpa.sym$symm.shape)
  
  assign(bone.list[i], working.bone.gpa.sym.sppAve, envir = .GlobalEnv)
  pdf(file = paste(bone.list[i], ".outliers.pdf", sep = ""), 
      width = 11, height = 8.5, useDingbats = F)
  plotOutliers(working.bone.gpa.sym.sppAve)
  title(sub = paste(bone.list[i]))
  dev.off()
  write.csv(
    c(print(bone.list[i]),print(plotOutliers(working.bone.gpa.sym.sppAve))),
    paste(bone.list[i], ".outliers.csv", sep = ""))
  #
  s <- print(summary(working.bone.gpa.sym))
  capture.output(s, 
                 file = paste(bone.list[i], ".bilat.symm.summary.txt", sep = ""))
  #
}
#######################
# Now I need to process the medial bones- the neurocranium and urohyal

#First, we'll do the Urohyal
i = 9
working_LM_bone_LHS <- Sculp.skull.full.slide[Bone_LM_LHS[Bone_LM_LHS$Bone ==bone.list[i],2],,]
rownames(working_LM_bone_LHS) <- 1:nrow(working_LM_bone_LHS) #only LHS landmarks for urohyal

# Make a slider matrix
working_sliders_bone <-  Bone_Slider_LHS2[Bone_Slider_LHS2$Bone == bone.list[i],2:4]
rownames(working_sliders_bone) <- c()
working_sliders_bone <- as.matrix(working_sliders_bone[,1:3])
#'
#'
working.bone.gpa.sym <- gpagen(A= working_LM_bone_LHS,
                               curves = working_sliders_bone,
                               ProcD = F)

working.bone.gpa.sym.sppAve <- my.landmark.species.average(classifier = skulltest1.classifier, aligned.coords = working.bone.gpa.sym$coords)

#
assign(bone.list[i], working.bone.gpa.sym.sppAve, envir = .GlobalEnv)

plotOutliers(working.bone.gpa.sym.sppAve)
title(sub = paste(bone.list[i]))


s <- print(summary(working.bone.gpa.sym))


#' Okay now the neurocranium
#' 
i = 7
#
working_LM_bone_LHS <- Sculp.skull.full.slide[Bone_LM_LHS[Bone_LM_LHS$Bone ==bone.list[i],2],,]
rownames(working_LM_bone_LHS) <- 1:nrow(working_LM_bone_LHS)
#
working_LM_bone_RHS <- Sculp.skull.full.slide[Bone_LM_RHS[Bone_LM_RHS$Bone ==bone.list[i],2],,]
rownames(working_LM_bone_RHS) <- (nrow(working_LM_bone_LHS)+1):((nrow(working_LM_bone_LHS))+nrow(working_LM_bone_RHS))
#'

working_sliders_bone_LHS <-  Bone_Slider_LHS2[Bone_Slider_LHS2$Bone == bone.list[i],2:4]
working_sliders_bone_RHS <-  Bone_Slider_RHS2[Bone_Slider_RHS2$Bone == bone.list[i],2:4]
working_slider_bone_combined <- rbind(working_sliders_bone_LHS, working_sliders_bone_RHS)

rownames(working_slider_bone_combined) <- c()
working_slider_bone_combined <- as.matrix(working_slider_bone_combined[,1:3])
#'
#'
working.LMs.bone = array(dim = c(((nrow(working_LM_bone_LHS))+nrow(working_LM_bone_RHS)), 
                                 3, length(dimnames(working_LM_bone_LHS)[[3]])),
                         dimnames = list(1:((nrow(working_LM_bone_LHS))+nrow(working_LM_bone_RHS)), 
                                         c("x", "y","z"), as.character(dimnames(working_LM_bone_LHS)[[3]])))

for (m in 1:length(dimnames(working_LM_bone_LHS)[[3]])) {
  landmark.LHS <- working_LM_bone_LHS[,,m]
  landmark.RHS <- working_LM_bone_RHS[,,m]
  working.LMs.bone[,,m] <- abind(landmark.LHS, landmark.RHS, along = 1)
}


working.isolate <- as.factor(dimnames(working.LMs.bone)[[3]])

working.gdf <- geomorph.data.frame(shape = working.LMs.bone, ind = working.isolate)

working.land.pairs <- as.matrix(read.csv(file = "neurocraniumLM_corresponences.csv", header = F))

working.lm.gpa.sym <- bilat.symmetry(A = shape, ind = ind, object.sym = TRUE, 
                                     
                                     curves = working_slider_bone_combined, 
                                     
                                     land.pairs=working.land.pairs, 
                                     
                                     data = working.gdf, RRPP = TRUE,
                                     
                                     iter = 999, print.progress = FALSE, 
                                     
                                     ProcD = F) # perform object symmetry GPA summary(scallop.sym)
summary(working.lm.gpa.sym)

working.bone.gpa.sym.sppAve <- my.landmark.species.average(classifier = skulltest1.classifier, aligned.coords = working.lm.gpa.sym$sym)


assign(bone.list[i], working.bone.gpa.sym.sppAve, envir = .GlobalEnv)

plotOutliers(working.bone.gpa.sym.sppAve)

s <- print(summary(working.lm.gpa.sym))
#'
#'
#'
########################'

############# BEGIN BONES MORPHOSPACE LOOP
skulltest1.species.data.phy <- skulltest1.species.data

#############' Now we'll paint the clades by taxonomic family

paint.tree <- paintSubTree(tree = pruned.test.tree, node = c(176), 
                           state = "Agonidae", anc.state = "0")

paint.tree <- paintSubTree(tree = paint.tree, node = c(178), 
                           state = "Agonidae", stem = T)

paint.tree <- paintSubTree(tree = paint.tree, node = 104, 
                           state = "Psychrolutidae", stem = T)

paint.tree <- paintSubTree(tree = paint.tree, node = 152, 
                           state = "Cottidae", stem = T)

paint.tree <- paintSubTree(tree = paint.tree, node = 74, 
                           state = "Jordaniidae", stem = T)

paint.tree <- paintSubTree(tree = paint.tree, node = 98, 
                           state = "Rhamphocottidae", stem = T)

paint.tree <- paintSubTree(tree = paint.tree, node = 99, 
                           state = "Trichodontidae", stem = T)

palette(c("black", "darkorange", "darkolivegreen",  "darkslategray3", "dodgerblue3", "deeppink", "firebrick3"))

plot(paint.tree,ftype="off",lwd=3)
dev.off()
#   "Agonidae"    "Psychrolutidae"      "Cottidae"        "Jordaniidae" "Rhamphocottidae" "Trichodontidae" 
# "darkorange"      "dodgerblue3"     "darkolivegreen"  "darkslategray3" "deeppink"      "firebrick3"  

#############'
#############'
#############'
##############################################'

# Individual bone morphospaces and phylomorphospaces!
for(i in 1:length(bone.list)){
  
  
  assign(paste(bone.list[i],".pca", sep = ""),
         gm.prcomp(get(bone.list[i])),      
         envir = .GlobalEnv)
  
  
  plot(get(paste(bone.list[i],".pca", sep = ""))$x[,1], get(paste(bone.list[i],".pca", sep = ""))$x[,2])
  points(get(paste(bone.list[i],".pca", sep = ""))$x[,1], get(paste(bone.list[i],".pca", sep = ""))$x[,2], pch = 21, cex = 1.5, bg = as.character(skulltest1.species.data$family.col))
  legend(x="topleft", legend = unique(skulltest1.species.data$family), pch=21, pt.cex = 1.5, col = "black", pt.bg = as.character(unique(skulltest1.species.data$family.col)))
  title(main = as.character(bone.list[i]))
 
  plot(get(paste(bone.list[i],".pca", sep = ""))$x[,1], get(paste(bone.list[i],".pca", sep = ""))$x[,2])
  points(get(paste(bone.list[i],".pca", sep = ""))$x[,1], get(paste(bone.list[i],".pca", sep = ""))$x[,2], pch = 21, cex = 1.5, bg = as.character(skulltest1.species.data$family.col))
  text(get(paste(bone.list[i],".pca", sep = ""))$x[,1], get(paste(bone.list[i],".pca", sep = ""))$x[,2], as.character(rownames(get(paste(bone.list[i],".pca", sep = ""))$x)), pos = 4)
  #legend(x="topleft", legend = unique(skulltest1.data$family), pch=21, pt.cex = 1.5, col = "black", pt.bg = as.character(unique(skulltest1.data$family.col)))
  title(main = as.character(bone.list[i]))
  
  working.gpa <- get(bone.list[i])
  working.gpa.coords <- working.gpa[,, rownames(skulltest1.species.data)]
  
  assign(paste(bone.list[i],".mshape.coords", sep = ""),
         mshape(get(bone.list[i])),
         envir = .GlobalEnv)

  # phylomorphospace
  working.pca <- get(paste(bone.list[i],".pca", sep = ""))
  working.pca.phycoords <- working.pca$x[pruned.test.tree$tip.label,]
  
  for(z in 1:3){
    
    working.axes <- working.pca.phycoords[,z:(z+1)]
    
    
    phylomorphospace(tree = paint.tree,
                     X = working.axes, label = "off",
                     node.size=c(0,1.2),node.by.map=TRUE)
    legend(x="topleft",legend=c("Agonidae", "Cottidae", "Jordaniidae", "Psychrolutidae", "Rhamphocottidae", "Trichodontidae"),
           pch=21,pt.cex=1.5,pt.bg=palette()[2:7],bty="n")
    title(main = as.character(bone.list[i]))

  }
  
  
  for(l in 1:4){
    for(t in 1:2){
      
      plotRefToTarget(M1= get(paste(bone.list[i],".mshape.coords", sep = "")), 
                      M2= get(paste(bone.list[i], ".pca", sep = ""))$shapes[l][[1]][[t]], mag = 1)
      title(sub = paste(bone.list[i], names(get(paste(bone.list[i], ".pca", sep = ""))$shapes[l][1]), names(get(paste(bone.list[i], ".pca", sep = ""))$shapes[l][[1]][t]), sep = " "))
      
    
    }  
  }
  
  
}

########################### END BONES LOOP.
# Now the local superimposition part

#The full configuration, which will be used as the template to superimpose parts
skull_sup <- skull.lm.fullslide.sym$symm.shape
skull_sup_mean <- mshape(skull.lm.fullslide.sym$symm.shape)


# Use a loop
for(l in c(1:10)){
  assign(
    paste(bone.list[l],"_sup", sep = ""),
    get(bone.list[l]),
    envir = .GlobalEnv
  )
}

# Do the full skull too
skull_sup <- my.landmark.species.average(classifier = skulltest1.classifier, aligned.coords = skull.lm.fullslide.sym$symm.shape)


#' Let's try generalizing this into a loop for every bone except the neurocranium
for(l in c(1:6,8:10)){
  
  bone.sup.holder <- get(paste(bone.list[l],"_sup", sep = ""))
  bone.coords.holder <- skull_sup[Bone_LM_LHS[Bone_LM_LHS == bone.list[l],2],,]

  assign(
    paste(bone.list[l],".matched", sep = ""),
    matchLM(bone.sup.holder, bone.coords.holder),
    envir = .GlobalEnv
  )
}


#' Now let's try the neurocranium
l <- 7
bone.sup.holder <- get(paste(bone.list[l],"_sup", sep = ""))
bone.coords.holder.LHS <- skull_sup[Bone_LM_LHS[Bone_LM_LHS == bone.list[l],2],,]
bone.coords.holder.RHS <- skull_sup[Bone_LM_RHS[Bone_LM_RHS == bone.list[l],2],,]


bone.coords.holder.combined <- array(dim = c(((nrow(bone.coords.holder.LHS))+nrow(bone.coords.holder.RHS)), 
                                             3, length(dimnames(bone.coords.holder.LHS)[[3]])),
                                     dimnames = list(1:((nrow(bone.coords.holder.LHS))+nrow(bone.coords.holder.RHS)), 
                                                     c("x", "y","z"), as.character(dimnames(bone.coords.holder.LHS)[[3]])))


for (m in 1:length(dimnames(bone.coords.holder.LHS)[[3]])) {
  landmark.LHS <- bone.coords.holder.LHS[,,m]
  landmark.RHS <- bone.coords.holder.RHS[,,m]
  bone.coords.holder.combined[,,m] <- abind(landmark.LHS, landmark.RHS, along = 1)
}


assign(
  paste(bone.list[l],".matched", sep = ""),
  matchLM(bone.sup.holder, bone.coords.holder.combined),
  envir = .GlobalEnv
)


#Combining the configurations
skull_local_sup<-combine.subsets(Angular.matched, Dentary.matched, Hyoid.matched, Hyomandibula.matched, Maxilla.matched,
                                 Nasal.matched, Neurocranium.matched, Premax.matched,Urohyal.matched, Preopercle.matched,   gpa=FALSE, CS.sets = NULL)

skull_locsup_coords<-skull_local_sup$coords


rownames(skull_locsup_coords)<-c(1:249)

dimnames(skull_locsup_coords)[3]<-dimnames(skull_sup)[3]
##'
##'
#'    Anatomical Data - end
###############################################################################'
###############################################################################'
##'
#'  Analysis of shape- begin
##'
##'
#' Okay, now we'll generate some figures for the local superimposition dataset.
skull_locsup.pca <- gm.prcomp(skull_locsup_coords)  
summary(skull_locsup.pca)
##'
#'
#'    Analysis of shape - end
###############################################################################'
###############################################################################'
##'
#'    Phylogenetic comparative methods- begin
##'
##'
######################################'
####### Alternative (no Baikal taxa)##'
### Version of FIGURE 2, toggle to ###
### "comp1" on x-axis and "comp2" on # 
### y-axis ###########################
######################################'
######################################'
#phylomorphospace
working.pca <- skull_locsup.pca
working.pca.phycoords <- working.pca$x[pruned.test.tree$tip.label,]

for(z in 1:1){
  
  working.axes <- working.pca.phycoords[,z:(z+1)]
  
  
  phylomorphospace(tree = paint.tree,
                   X = working.axes, label = "off",
                   node.size=c(0,1.2),node.by.map=TRUE)
  legend(x="topleft",legend=c("Agonidae", "Cottidae", "Jordaniidae", "Psychrolutidae", "Rhamphocottidae", "Trichodontidae"),
         pch=21,pt.cex=1.5,pt.bg=palette()[2:7],bty="n")
  title(main = "Local Superimposition Full Skull")

}
######################################'
######################################'

# visualize
plot(working.pca.phycoords[,1], (working.pca.phycoords[,2])) 
points(working.pca.phycoords[,1], (working.pca.phycoords[,2]), pch = 21, cex = 1.5, bg = as.character(skulltest1.species.data$family.col))
legend(x="topleft", legend = unique(skulltest1.species.data$family), pch=21, pt.cex = 1.5, col = "black", pt.bg = as.character(unique(skulltest1.species.data$family.col)))

#save as pdf
plot(working.pca.phycoords[,1], working.pca.phycoords[,2])
points(working.pca.phycoords[,1], working.pca.phycoords[,2], pch = 21, cex = 1.5, bg = as.character(skulltest1.species.data$family.col))
legend(x="topleft", legend = unique(skulltest1.species.data$family), pch=21, pt.cex = 1.5, col = "black", pt.bg = as.character(unique(skulltest1.species.data$family.col)))
title("skull_locsup.pca")



#' Let's add the specimen names. 
# Visualize
plot(working.pca.phycoords[,1], working.pca.phycoords[,2])
points(working.pca.phycoords[,1], working.pca.phycoords[,2], pch = 21, cex = 1.5, bg = as.character(skulltest1.species.data$family.col))
text(working.pca.phycoords[,1], working.pca.phycoords[,2], dimnames(working.pca.phycoords)[[1]], pos = 4)



#' Specimen names with PC2 vs PC1. 
plot(working.pca.phycoords[,2], working.pca.phycoords[,1])
points(working.pca.phycoords[,2], working.pca.phycoords[,1], pch = 21, cex = 1.5, bg = as.character(skulltest1.species.data$family.col))
text(working.pca.phycoords[,2], working.pca.phycoords[,1], dimnames(working.pca.phycoords)[[1]], pos = 4)
title("skull_locsup.pca")


# Check for outliers
plotOutliers(skull_locsup_coords)
#'
#' Now, we'll calculate the mean skull shape
skull_locsup_coords.meanshape <- mshape(skull_locsup_coords)
#'
#' make some TPS warp grid transforms
for(l in 1:4){
  for(t in 1:2){
    
    plotRefToTarget(M1= skull_locsup_coords.meanshape, 
                    M2= skull_locsup.pca$shapes[l][[1]][[t]],
                    method = "TPS",
                    mag = 1)
    title(sub = paste("skull_locsup.pca", names(skull_locsup.pca$shapes[l][[1]][t]), sep = "."))
    
  }  
}
#
##'
##############' Phylogenetic MANOVA
#'
skull_locsup_coords.phy <- skull_locsup_coords[,,pruned.test.tree$tip.label]
skull_locsup_coords.phy.2darray <- two.d.array(skull_locsup_coords.phy)
##' Start with comparison of shape between freshwater vs saltwater species
#'
#' Re-organize the habitat data table
sculpin.habitat.table <- sculpin.habitat.table[rownames(skulltest1.species.data.phy),]
#'
#' Isolate the freshwater vs saltwater variable
sculp.habitat <- sculpin.habitat.table$Fresh
names(sculp.habitat) <- rownames(skulltest1.species.data.phy)
names(sculp.habitat) == dimnames(skull_locsup_coords.phy)[[3]] #double check
#'
#' Next, isolate family variable
sculp.families <- skulltest1.species.data.phy$family
names(sculp.families) <- rownames(skulltest1.species.data.phy)

#' Now make a geomorph data frame to hold the data
gdf.phyANOVA <- geomorph.data.frame(coords = skull_locsup_coords.phy, family = sculp.families, habitat = sculp.habitat)
#'
#' Now run the Habitat analysis
habitat.ANOVA <- procD.pgls(coords ~ habitat, phy = pruned.test.tree, data = gdf.phyANOVA)
summary(habitat.ANOVA)
#'
# Now compare taxonomic families
family.ANOVA <- procD.pgls(coords ~ family, phy = pruned.test.tree, data = gdf.phyANOVA)
summary(family.ANOVA)
##'
######' This is the DISPARITY analysis referenced in the main text #####
######################################################################'
############ COMPARING DISPARITY
#'
#' First compare disparity of freshwater vs. saltwater
morphol.disparity(f1 = skull_locsup_coords.phy~1 + sculp.families, groups = sculp.families)
#'
#' Next, compare disparity of family groups
morphol.disparity(f1 = skull_locsup_coords.phy~1 + sculp.families, groups = sculp.families)
#
###########################################################
#'
#' Now let's look at disparity through time of each of the three major sculpin
#' families
#'
#' First, make some new sortings of the coordinate data
pruned.test.tree$tip.label == row.names(skull_locsup.pca$x)
skull_locsup.pca.coords.phy <- skull_locsup.pca$x[pruned.test.tree$tip.label,]
sum(pruned.test.tree$tip.label == row.names(skull_locsup.pca.coords.phy))
#'
#' We'll start with Agonidae, an exclusively marine group
agonidae.tree <- extract.clade(pruned.test.tree, node = 178)
plotTree(agonidae.tree)
agonidae.data <- skull_locsup.pca.coords.phy[agonidae.tree$tip.label,]
dev.off()
dtt(phy = agonidae.tree, data = agonidae.data, index = "avg.sq", nsim = 999)
title(main= "DTT Agonidae")
#'
#'
#' Next, the primarily freshwater family, Cottidae
cottidae.tree <- extract.clade(pruned.test.tree, node = 152)
plotTree(cottidae.tree)
cottidae.data <- skull_locsup.pca.coords.phy[cottidae.tree$tip.label,]
dev.off()
dtt(phy = cottidae.tree, data = cottidae.data, index = "avg.sq", nsim = 999)
title(main= "DTT Cottidae")
#'
#'
#' Finally, the primarily saltwater family, Psychrolutidae
psychrolutidae.tree <- extract.clade(pruned.test.tree, node = 104)
plotTree(psychrolutidae.tree)
psychrolutidae.data <- skull_locsup.pca.coords.phy[psychrolutidae.tree$tip.label,]
dev.off()
dtt(phy = psychrolutidae.tree, data = psychrolutidae.data, index = "avg.sq", nsim= 999)
title(main= "DTT Psychrolutidae")
##'
##'
#'    Phylogenetic comparative methods - end
###############################################################################'
###############################################################################'
##'
#'    Hypothesis 1: differing rates of morph evol- begin
##'
##'
#' First, compare freshwater vs saltwater
SculpHabitats.rates <- compare.evol.rates(A = skull_locsup_coords.phy, 
                                          gp = sculp.habitat,
                                          method = "simulation", phy = pruned.test.tree,
                                          iter=999)
summary(SculpHabitats.rates)
#'

# Next, compare taxonomic families
SculpFamilies.rates <- compare.evol.rates(A = skull_locsup_coords.phy, 
                                          gp = sculp.families,
                                          method = "simulation", phy = pruned.test.tree,
                                          iter=999)
summary(SculpFamilies.rates)
##'
##'
#'    Hypothesis 1: differing rates of morph evol - end
###############################################################################'
###############################################################################'
##'
#'    Hypothesis 2: differing mode of morph evol (lineage density)
##'
##'
####'
####' Lineage Density

# PCA data from local superimposition dataset NO BAIKAL
sculp.skull_locsup.pca.coords <- skull_locsup.pca$x


sculp.skull_locsup.pca.coords <- sculp.skull_locsup.pca.coords[pruned.test.tree$tip.label,]

# Info for each species
skulltest1.species.data <- skulltest1.species.data[rownames(sculp.skull_locsup.pca.coords),]

#' 
#' Need to limit the number of PC's that I consider in the analyses 
Max.PCs <- 4
#' 
phy <- pruned.test.tree
n <- length(phy$tip.label)
node1 <-178
node2<- 152
node3 <- 104



Agonidae.obs <- my.lineage.densities(phy = pruned.test.tree, node = 178, 
                                     my.range=c(178:197), PCcoords = sculp.skull_locsup.pca.coords[,1:Max.PCs])

Cottidae.obs <- my.lineage.densities(phy = pruned.test.tree, node = 152,
                                     my.range=c(152:175), PCcoords = sculp.skull_locsup.pca.coords[,1:Max.PCs])

Psychrolutidae.obs <- my.lineage.densities(phy = pruned.test.tree, node = 104,
                                           my.range=c(104:150), PCcoords = sculp.skull_locsup.pca.coords[,1:Max.PCs])

Obs.data.list <- list(Agonidae = Agonidae.obs,
                      Cottidae = Cottidae.obs,
                      Pyschrolutidae = Psychrolutidae.obs)
simulated.data.list <- my.data.simulator.three(dat =sculp.skull_locsup.pca.coords, phy=pruned.test.tree, 
                                               node1 = 178,  node2 = 152, node3 = 104, nsim = 1000, my.range1 = c(178:197), 
                                               my.range2 = c(152:175), my.range3 = c(104:150), node1.name = "Agonidae", node2.name = "Cottidae",
                                               node3.name = "Psychrolutidae", maxPC = Max.PCs)
#' 
#'
##########################################################################################'
#'
#' Now, let's try to do some of the clade comparisons from
#' Sidlauskas (2008).
Obs.ratios <- list(Psychrolutid.Cottid.obs = Psychrolutidae.obs/Cottidae.obs,
                   Cottid.Psychrolutid.obs = Cottidae.obs/Psychrolutidae.obs,
                   Pyschrolutid.Agonid.obs = Psychrolutidae.obs/Agonidae.obs,
                   Agonid.Cottid.obs = Agonidae.obs/Cottidae.obs)

Sim.ratios <- list(Psychrolutid.Cottid = simulated.data.list$Psychrolutidae / simulated.data.list$Cottidae,
                   Cottid.Psychrolutid = simulated.data.list$Cottidae / simulated.data.list$Psychrolutidae,
                   Pyschrolutid.Agonid = simulated.data.list$Psychrolutidae / simulated.data.list$Agonidae,
                   Agonid.Cottid =  simulated.data.list$Agonidae / simulated.data.list$Cottidae)


true.false.test <- Sim.ratios
for(i in 1:ncol(Obs.ratios$Psychrolutid.Cottid)){
  true.false.test$Psychrolutid.Cottid[,i] <- Obs.ratios$Psychrolutid.Cottid.obs[,i] < Sim.ratios$Psychrolutid.Cottid[,i]
  true.false.test$Cottid.Psychrolutid[,i] <- Obs.ratios$Cottid.Psychrolutid.obs[,i] < Sim.ratios$Cottid.Psychrolutid[,i]
  true.false.test$Pyschrolutid.Agonid[,i] <- Obs.ratios$Pyschrolutid.Agonid.obs[,i] < Sim.ratios$Pyschrolutid.Agonid[,i]
  true.false.test$Agonid.Cottid[,i] <- Obs.ratios$Agonid.Cottid.obs[,i] < Sim.ratios$Agonid.Cottid[,i]
}

p.values <- Obs.ratios
names(p.values) <- c("Psychrolutid.Cottid", "Cottid.Psychrolutid", "Pyschrolutid.Agonid" , "Agonid.Cottid")
for(i in 1:ncol(Obs.ratios$Psychrolutid.Cottid.obs)){
  p.values$Psychrolutid.Cottid[,i] <- mean(true.false.test$Psychrolutid.Cottid[,i])
  p.values$Cottid.Psychrolutid[,i] <- mean(true.false.test$Cottid.Psychrolutid[,i])
  p.values$Pyschrolutid.Agonid[,i] <- mean(true.false.test$Pyschrolutid.Agonid[,i])
  p.values$Agonid.Cottid[,i] <- mean(true.false.test$Agonid.Cottid[,i])
}
p.values


###############################################################################'
######## END ##################################################################'
###############################################################################'
###############################################################################'
###############################################################################'
###############################################################################'
###############################################################################'