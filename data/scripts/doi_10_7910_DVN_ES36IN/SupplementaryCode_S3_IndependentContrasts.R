#script for phylogenetic independent contrasts of diet data measures
#analyses were done using R 4.0.1

library(ape) #ape 5.4 was used for analyses

#read in data for independent contrasts (259 species excluding Colaptes auratus cafer)
dietdata <- read.csv(file = "SupplementaryTable_S1_CarotenoidNetworkMeasures.csv", header = T, row.names = 1)

#retain only the DegenN (number of metabolically-degenerate compounds) and Shannon (modified Shannon diversity index) for the extant species
dietdata <- dietdata[,c(which(colnames(dietdata) == "DegenN"),which(colnames(dietdata) == "Shannon"))]
#delete ancestral networks from the dataset
dietdata <- dietdata[1:(which(row.names(dietdata) == "ancestralnode_1")-1),]
#remove Colaptes auratus cafer from the dataset (not in consensus phylogeny)
dietdata <- dietdata[-(which(row.names(dietdata) == "Colaptes_auratus_cafer")),]

#Ultrametric majority rule consensus tree (Supplementary Data S5) with internal ancestral nodes labeled (259 species excluding Colaptes auratus cafer)
birdtree <- read.nexus(file = "SupplementaryData_S5_UltrametricConsensusTree.nex")

#check that names in data set to see if they match names in phylogeny
name.check(birdtree, dietdata)

#Change names in tree to match updated taxa names in dataset
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
birdtree$tip.label <- gsub("Carpornis_cucullata", "Carpornis_cucullatus", birdtree$tip.label)
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

#Change names in data set to match tree tips
rownames(dietdata) <- gsub("Gallus_gallus_domesticus", "Gallus_gallus", rownames(dietdata))

#check that names one more time to see if names in data set match names in phylogeny
name.check(birdtree, dietdata)

#Randomly resolve polytomies: (need this for independent contrasts)
birdtree <- multi2di(birdtree, random = TRUE)

#Label internal nodes created by randomly resolving polytomies: (starting at 300 to denote added internal node)
n <- 300
for (i in which(birdtree$node.label == "")) {
  birdtree$node.label[i] <- as.character(n)
  n <- n + 1
}

#Write tree used for independent contrasts with newly formed internal nodes due to resolved polytomies labeled starting at 300 (now located in Supplementary Data S3)
#write.tree(birdtree,"IndependentContrastTree.nex")

#Create data frame to collect PICs (phylogenetic independent contrasts) of all measurements
PIC <- data.frame(matrix(ncol = 24, nrow = 258))
row.names(PIC) <- birdtree$node.label
colnames(PIC) <- colnames(dietdata[1:2])

#Independent contrasts for all measures in data set
for (i in 1:2) {
  measure <- setNames(dietdata[,i], rownames(dietdata)) #make a list of the variable with the taxa names attached
  pic_measure <- pic(measure, birdtree) #phylogenetic independent contrast (PIC)
  PIC[,i] <- pic_measure #add PIC for variable to data frame 
}

#Export PIC dataframe
write.table(PIC, "IndependentContrastsResults.txt",col.names = T, row.names = T)
