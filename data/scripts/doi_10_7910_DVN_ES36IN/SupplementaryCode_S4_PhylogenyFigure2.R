#The R code used to build Figure 2 and Figure S1 in the manuscript

#make bar plot diet & derived compounds colored by diet category next to ultrametric phylogeny
#see: http://blog.phytools.org/2018/04/update-hack-to-get-stacked-bars-in.html
#see: https://rdrr.io/cran/ape/man/axisPhylo.html
#see: https://stackoverflow.com/questions/16905535/r-legend-trouble-how-to-change-the-text-size-in-legend
#http://blog.phytools.org/2018/05/customizing-bar-plot-paired-with.html (plot bars, change edge color, axis titles)
#http://blog.phytools.org/2015/04/plottreewbars-with-tip-labels.html (plot tree with bars on circular tree)
#http://blog.phytools.org/2014/05/new-version-of-plottreewbars-that.html (new version)
#https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/#:~:text=RColorBrewer%20is%20an%20R%20package,Install%20and%20load%20RcolorBrewer%20package (color brewer color choices)
#http://blog.phytools.org/2017/09/plotting-fan-style-circular-tree-with.html (plot tree with time axis - with gap between branches furthest apart)

library(ape)
library(geiger)
library(phytools)
library(RColorBrewer)

#ultrametric majority rule consensus tree with internal ancestral nodes labeled (SupplementaryData_S5; 259 species excluding Colaptes auratus cafer)
birdtree <- read.nexus(file = "SupplementaryData_S5_UltrametricConsensusTree.nex")

#import data classification for species
dietdata <- read.csv(file = "SupplementaryTable_S1_CarotenoidNetworkMeasures.csv", header = T, row.names = 1)

#add column for number of metabolized (non-dietary compounds)
dietdata$metabN <- dietdata$NodeN -  dietdata$DietN

#remove Colaptes auratus cafer from data set b/c not in tree
dietdata <- dietdata[-grep("Colaptes_auratus_cafer", rownames(dietdata)),,drop=F]

#check that names in data set to see if they match names in phylogeny
name.check(birdtree, dietdata)

#change names in tree to match updated taxa names in data set
birdtree$tip.label <- gsub("Carduelis_atrata", "Spinus_atrata", birdtree$tip.label)
birdtree$tip.label <- gsub("Carduelis_cannabina", "Linaria_cannabina", birdtree$tip.label)
birdtree$tip.label <- gsub("Carduelis_chloris", "Chloris_chloris", birdtree$tip.label)
birdtree$tip.label <- gsub("Carduelis_cucullata", "Spinus_cucullata", birdtree$tip.label)
birdtree$tip.label <- gsub("Carduelis_flammea", "Acanthis_flammea", birdtree$tip.label)
birdtree$tip.label <- gsub("Carduelis_hornemanni", "Acanthis_hornemanni", birdtree$tip.label)
birdtree$tip.label <- gsub("Carduelis_sinica", "Chloris_sinica", birdtree$tip.label)
birdtree$tip.label <- gsub("Carduelis_spinoides", "Chloris_spinoides", birdtree$tip.label)
birdtree$tip.label <- gsub("Carduelis_spinus", "Spinus_spinus", birdtree$tip.label)
birdtree$tip.label <- gsub("Carduelis_tristis", "Spinus_tristis", birdtree$tip.label)
birdtree$tip.label <- gsub("Carpodacus_mexicanus", "Haemorhous_mexicanus", birdtree$tip.label)
birdtree$tip.label <- gsub("Carpornis_cucullata", "Carpornis_cucullatus", birdtree$tip.label) #don't need for 7/4/20 diet cats
birdtree$tip.label <- gsub("Dendroica_coronata", "Setophaga_coronata", birdtree$tip.label)
birdtree$tip.label <- gsub("Dendroica_palmarum", "Setophaga_palmarum", birdtree$tip.label)
birdtree$tip.label <- gsub("Dendroica_petechia", "Setophaga_petechia", birdtree$tip.label)
birdtree$tip.label <- gsub("Dryocopus_pileatus", "Hylatomus_pileatus", birdtree$tip.label)
birdtree$tip.label <- gsub("Larus_pipixcan", "Leucophaeus_pipixcan", birdtree$tip.label)
birdtree$tip.label <- gsub("Parus_ater", "Periparus_ater", birdtree$tip.label)
birdtree$tip.label <- gsub("Parus_caeruleus", "Cyanistes_caeruleus", birdtree$tip.label)
birdtree$tip.label <- gsub("Picoides_villosus", "Leuconotopicus_villosus", birdtree$tip.label)
birdtree$tip.label <- gsub("Pipra_chloromeros", "Ceratopipra_chloromeros", birdtree$tip.label)
birdtree$tip.label <- gsub("Pipra_erythrocephala", "Ceratopipra_erythrocephala", birdtree$tip.label)
birdtree$tip.label <- gsub("Rhodopechys_obsoletus", "Rhodospiza_obsoleta", birdtree$tip.label)
birdtree$tip.label <- gsub("Sterna_elegans", "Thalasseus_elegans", birdtree$tip.label)
birdtree$tip.label <- gsub("Vermivora_ruficapilla", "Leiothlypis_ruficapilla", birdtree$tip.label)
birdtree$tip.label <- gsub("Vermivora_virginiae", "Leiothlypis_virginiae", birdtree$tip.label)
birdtree$tip.label <- gsub("Gallus_gallus", "Gallus_gallus_domesticus", birdtree$tip.label)

#need to reverse order of nodes in the tree for phylogeny figure (so that root is at top) - makes figure look better
birdtree1 <- birdtree #birdtree1 = original topology
birdtree <- rotateNodes(birdtree, "all") #birdtree = reversed topology

##########
##Figure 2

#make a vector of tip colors based on diet - colors listed in order of tips on tree
#color brewer "Dark2", color blind friendly, just eliminated second light green color for a yellow
#brewer.pal(n = 6, name = "Dark2")
#[1] "#1B9E77"(dark green) "#D95F02"(orange) "#7570B3"(purple) "#E7298A"(pink) "#66A61E"(light green) "#E6AB02"(yellow)
dietcolor <- c()
for (n in 1:length(birdtree$tip.label)) {
  diet <- dietdata$DietClass[grep(birdtree$tip.label[n], rownames(dietdata))]
  dietcolor[n] <- ifelse(diet == "F", "#E7298A", #pink
                                   ifelse(diet == "P", "#1B9E77", #dark green
                                          ifelse(diet == "I", "#D95F02", #orange
                                                 ifelse(diet == "O", "#7570B3", #purple
                                                        "#E6AB02")))) #yellow verts
}

#make 'named' vectors for compound & sensitivity data (required to put bars on tree)
totcompounds <- setNames(dietdata$NodeN, row.names(dietdata))
dietcompounds <- setNames(dietdata$DietN, row.names(dietdata))
sensitivityphylo <- setNames((dietdata$NodeS*10) + 26, row.names(dietdata)) #max totcomps = 24, add 26 to create space between compounds and sensitivity, multiply by 10 to increase scale
shannonphylo <- setNames((dietdata$Shannon*10) + 26, row.names(dietdata))
blankbars <- setNames(rep(26, length(birdtree$tip.label)), row.names(compounddata))

scale <- 0.75*max(nodeHeights(birdtree))/max(sensitivityphylo) #scale for bars (all will have same scale - sensitivity has largest value). Conserving scale b/c diet bars are in all of them

#####DIET + sensitivity phylogeny:
#lwd = edge width, part = allows tree to be split at most distant edges, plot grey bars for sensitivity plotted first added 26 to total to leave space between compound and sensitivity bars - will cover 28 of these grey bars with white bars
plotTree.wBars(birdtree, sensitivityphylo, method = "plotTree", type = "fan", scale = scale, col= "grey50", width = 3.4, border = "white", lwd = 0.25, part = 0.93) #lwd = edge width, part = allows tree to be split at most distant edges, plot grey bars for sensitivity
obj <- get("last_plot.phylo", envir = .PlotPhyloEnv)
#plots white bars with length 28 to cover space between sensitivity bars and compound bars
plotTree.wBars(birdtree, blankbars, method = "plotTree", type="fan", scale = scale, add=T, lims=obj$x.lim, col= "white", width = 3.5, border = "white", lwd = 0.25, part = 0.93)
obj <- get("last_plot.phylo", envir = .PlotPhyloEnv)
#plot white bars with black outline for total compounds (white bars will represent total derived comps)
plotTree.wBars(birdtree, totcompounds, method = "plotTree", type="fan", scale = scale, add=T, lims=obj$x.lim, col= "white", width = 2, border = "black", lwd = 0.25, part = 0.93)
obj <- get("last_plot.phylo", envir = .PlotPhyloEnv)
#plot color bars (based on diet category) for bars taht represent number of diet compounds
plotTree.wBars(birdtree,dietcompounds, method = "plotTree", type="fan", scale = scale, add=T, lims=obj$x.lim, col=dietcolor, width = 2, border = "black", lwd = 0.25, part = 0.93)

#plot time axis for tree in the split in tree with a scale from 0-90MYA
obj<-axis(1,pos=-1.75,at=seq(max(nodeHeights(birdtree)), 0, by=-10),tck = -0.005, cex.axis=0.5,labels=FALSE, lwd = 0.5)
text(obj,rep(-5,length(obj)), (max(nodeHeights(birdtree))-obj), cex=0.5)
text(mean(obj),-10,"time (mya)",cex=0.7)

#plot legend for diet category colors in the phylogeny gap
legend(x = 90, y = -15, legend = names(setNames(c("#E7298A", "#1B9E77", "#D95F02", "#7570B3", "#E6AB02"), 
                                              c("fruit & nectar", "plants & seeds", "invertebrates", "omnivorous", "vertebrates & scavengers"))),
       pch = 22, cex = 0.7, pt.cex = 1.5, pt.bg = c("#E7298A", "#1B9E77", "#D95F02", "#7570B3", "#E6AB02"), box.col = "transparent", bg = "transparent", 
       x.intersp = 0.3, y.intersp = 0.35)


###############################################################
##Plot phylogeny with taxa names only and time (mya) scale bar (Figure S1)
plotTree(birdtree, type = "fan", fsize = 0.45, lwd = 0.4, part = 0.93)
#plot time axis for tree in the split in tree with a scale from 0-90MYA
obj<-axis(1,pos=-1.75,at=seq(max(nodeHeights(birdtree)), 0, by=-10),tck = -0.005, cex.axis=0.5,labels=FALSE, lwd = 0.5)
text(obj,rep(-5,length(obj)), (max(nodeHeights(birdtree))-obj), cex=0.5)
text(mean(obj),-10,"time (mya)",cex=0.7)

