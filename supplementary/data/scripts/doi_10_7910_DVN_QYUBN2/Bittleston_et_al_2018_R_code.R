#####
### This file includes the code for Figures 1, 3, 4, 5 and their figure supplements, and for Tables S3 and S4
### ***Note: this code was run in R version 3.2.1***
#####

## Set working directory
setwd("~/Desktop/Bittleston_et_al_2018_R_code_and_data/") ## Change depending on where you have the folder

## Load packages
if (!require("vegan")) {install.packages("vegan"); require("vegan")}
if (!require("picante")) {install.packages("picante"); require("picante")}
if (!require("phyloseq")) {install.packages("phyloseq"); require("phyloseq")}
if (!require("beanplot")) {install.packages("beanplot"); require("beanplot")}
if (!require("reshape2")) {install.packages("reshape2"); require("reshape2")}
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}

## Start with 16S, read in OTU table
botu.df <- read.table("otu_table_16S_obs10_bittleston.txt",sep="\t",head=T,row.names=1, check.names=F)
botu.notax <- botu.df[,1:492] # Remove taxonomy 
summary(rowSums(botu.notax))
summary(colSums(botu.notax))
botu.t <- t(botu.notax) # transpose
botu.t <- botu.t[order(row.names(botu.t)),] # order samples alphabetically

## Metadata
bmdat <- read.csv("Supplementary_Dataset_1_Metadata.csv", head=T, row.names=1) #get metadata
bmdat <- bmdat[order(row.names(bmdat)),]
bmdat <- subset(bmdat, row.names(bmdat) %in% row.names(botu.t)) 
row.names(botu.t)==row.names(bmdat) # sanity check

## Subset OTU table and metadata to the relevant samples for Figure 1
bcom <- subset(botu.t, bmdat$Project=="Comparison")
mdat.bcom <- subset(bmdat, row.names(bmdat) %in% row.names(bcom)) 
bcom <- subset(bcom, mdat.bcom$Nep_Sar!="Other")
mdat.bcom <- subset(mdat.bcom, row.names(mdat.bcom) %in% row.names(bcom)) 

## Remove factors that are no longer in this subset
mdat.bcom$Location <- mdat.bcom$Location[drop=T]
mdat.bcom$Type_2 <- mdat.bcom$Type_2[drop=T]
mdat.bcom$Type_3 <- mdat.bcom$Type_3[drop=T]
mdat.bcom$Site <- mdat.bcom$Site[drop=T]
mdat.bcom$pH <- mdat.bcom$pH[drop=T]
mdat.bcom$Volume <- mdat.bcom$Volume[drop=T]
mdat.bcom$DNA_conc <- mdat.bcom$DNA_conc[drop=T]

## Check OTU table and rarefy to 4000 sequences per sample
summary(colSums(bcom))
summary(rowSums(bcom))
bcom <- bcom[,colSums(bcom) > 0]
bcom.r <- rrarefy(bcom,4000)
summary(colSums(bcom.r))
bcom.r <- bcom.r[,colSums(bcom.r) > 0]

## Read in phylogenetic tree, subset to relevant samples, put in format for phyloseq
btree <- read.tree("rep_set_16S_obs10_bittleston.tre")
bcom.tree <- prune.sample(bcom.r, btree)
OTU.bcom = otu_table(as.matrix(bcom.r), taxa_are_rows = FALSE)
SAM.bcom = sample_data(mdat.bcom)
bcom.physeq <-merge_phyloseq(phyloseq(OTU.bcom),SAM.bcom,bcom.tree)

## Calculate Unifrac distance and run NMDS
bcom.uu.dist <- distance(bcom.physeq,"uUniFrac")
bcom.uu.nmds <- metaMDS(bcom.uu.dist, trymax=200)
#bcom.uu.nmds <- metaMDS(bcom.uu.dist, trymax=200, previous.best=bcom.uu.nmds) # if necessary

## Plot NMDS
plot(bcom.uu.nmds$points, xlab="NMDS Axis 1", ylab="NMDS Axis 2", 
     main="Bacterial community by Type", 
     col= c("brown","red","blue","black")[mdat.bcom$Type_2],
     pch = c(15,15,17,17)[mdat.bcom$Location])
legend("topleft", 
       legend=c("Bog","Nepenthes","Sarracenia","Soil"),
       col= c("brown","red","blue","black"),
       pch= 19)
legend("topright",
       legend=c("North America", "Southeast Asia"),
       pch = c(15, 17),
       col = "gray")
ordispider(bcom.uu.nmds, groups=mdat.bcom$Type_2, show.groups="Soil", col=adjustcolor("black",0.2), spiders = "median", lwd=3)
ordispider(bcom.uu.nmds, groups=mdat.bcom$Type_2, show.groups="Bog", col=adjustcolor("brown",0.2), spiders = "median", lwd=3)
ordispider(bcom.uu.nmds, groups=mdat.bcom$Type_2, show.groups="Nepenthes", col=adjustcolor("red",0.2), spiders = "median", lwd=3)
ordispider(bcom.uu.nmds, groups=mdat.bcom$Type_2, show.groups="Sarracenia", col=adjustcolor("blue",0.2), spiders = "median", lwd=3)

## Bray-curtis distance matrix
bcom.bc.dist <- vegdist(bcom.r,method = "bray")

## Adonis (PERMANOVA) tests
bcom.ad <- adonis(bcom.uu.dist ~ Type_1, data=mdat.bcom, permutations = 9999)
bcom.ad

bcom.ad1 <- adonis(bcom.uu.dist ~ Location, data=mdat.bcom, permutations = 9999)
bcom.ad1


bcom.bc.ad <- adonis(bcom.bc.dist ~ Type_1, data=mdat.bcom, permutations = 9999)
bcom.bc.ad

bcom.bc.ad1 <- adonis(bcom.bc.dist ~ Location, data=mdat.bcom, permutations = 9999)
bcom.bc.ad1

## Envfit tests
bcom.env <- envfit(bcom.uu.nmds, mdat.bcom[c("Location","Type_1")], permutations = 9999)
bcom.env


## Adjust p-values for multiple comparisons
bcom.pvals <- c(bcom.ad[[1]][["Pr(>F)"]][[1]],bcom.ad1[[1]][["Pr(>F)"]][[1]], +
                bcom.bc.ad[[1]][["Pr(>F)"]][[1]],bcom.bc.ad1[[1]][["Pr(>F)"]][[1]], +
                bcom.env[[2]][["pvals"]][[1]],bcom.env[[2]][["pvals"]][[2]])
bcom.adjpvals <- p.adjust(bcom.pvals, method = "BH")
bcom.adjpvals

#### Subset to just Nepenthes
bcomn <- subset(bcom, mdat.bcom$Type_2=="Nepenthes")
mdat.bcomn <- subset(mdat.bcom, row.names(mdat.bcom) %in% row.names(bcomn)) 

## Remove samples that don't have pH measures
bcomnp <- subset(bcomn, mdat.bcomn$pH!="NA")
mdat.bcomnp <- subset(mdat.bcomn, row.names(mdat.bcomn) %in% row.names(bcomnp)) 

## Remove factors no longer present in subset
mdat.bcomnp$Location <- mdat.bcomnp$Location[drop=T]
mdat.bcomnp$Type_1 <- mdat.bcomnp$Type_1[drop=T]
mdat.bcomnp$Type_2 <- mdat.bcomnp$Type_2[drop=T]
mdat.bcomnp$Type_3 <- mdat.bcomnp$Type_3[drop=T]
mdat.bcomnp$Site <- mdat.bcomnp$Site[drop=T]
mdat.bcomnp$pH <- mdat.bcomnp$pH[drop=T]

## Check OTU table and rarefy to 4000 seqs
summary(colSums(bcomnp))
summary(rowSums(bcomnp))
bcomnp <- bcomnp[,colSums(bcomnp) > 0]
bcomnp.r <- rrarefy(bcomnp,4000)
summary(colSums(bcomnp.r))
bcomnp.r <- bcomnp.r[,colSums(bcomnp.r) > 0]

## Prune tree and put into format for phyloseq
bcomnp.tree <- prune.sample(bcomnp.r, btree)
OTU.bcomnp = otu_table(as.matrix(bcomnp.r), taxa_are_rows = FALSE)
SAM.bcomnp = sample_data(mdat.bcomnp)
bcomnp.physeq <-merge_phyloseq(phyloseq(OTU.bcomnp),SAM.bcomnp,bcomnp.tree)

## Measure Unifrac distance and run NMDS
bcomnp.uu.dist <- distance(bcomnp.physeq,"uUniFrac")
bcomnp.uu.nmds <- metaMDS(bcomnp.uu.dist, trymax=800)
#bcomnp.uu.nmds <- metaMDS(bcomnp.uu.dist, trymax=800, previous.best=bcomnp.uu.nmds) # If necessary for NMDS to converge

## Plot NMDS by host species
customcol8 <- c("darkred","tomato1","orange","yellow","palegreen2","dodgerblue2","darkblue","violetred")
plot(bcomnp.uu.nmds$points, xlab="NMDS Axis 1", ylab="NMDS Axis 2", 
     main="Nepenthes bacterial community by host species", 
     col= customcol8[mdat.bcomnp$Type_3],
     pch =19)
legend("bottomright", 
       legend=c("ampullaria","gracilis","hirsuta","rafflesiana","reinwardtiana","stenophylla","tentaculara","veitchii"),
       col= customcol8,
       pch = 19)
ordisurf(bcomnp.uu.nmds,mdat.bcomnp$pH, add=T, col="darkgray")
ordispider(bcomnp.uu.nmds, groups=mdat.bcomnp$Type_3, show.groups="ampullaria", col = adjustcolor(customcol8[1],0.2), spiders = "median", lwd=3)
ordispider(bcomnp.uu.nmds, groups=mdat.bcomnp$Type_3, show.groups="gracilis", col = adjustcolor(customcol8[2],0.2),spider="median", lwd=3)
ordispider(bcomnp.uu.nmds, groups=mdat.bcomnp$Type_3, show.groups="hirsuta", col = adjustcolor(customcol8[3],0.2),spider="median", lwd=3)
ordispider(bcomnp.uu.nmds, groups=mdat.bcomnp$Type_3, show.groups="rafflesiana", col = adjustcolor(customcol8[4],0.2),spider="median", lwd=3)
ordispider(bcomnp.uu.nmds, groups=mdat.bcomnp$Type_3, show.groups="reinwardtiana", col = adjustcolor(customcol8[5],0.2),spider="median", lwd=3)
ordispider(bcomnp.uu.nmds, groups=mdat.bcomnp$Type_3, show.groups="stenophylla", col = adjustcolor(customcol8[6],0.2),spider="median", lwd=3)
ordispider(bcomnp.uu.nmds, groups=mdat.bcomnp$Type_3, show.groups="tentaculata", col = adjustcolor(customcol8[7],0.2),spider="median", lwd=3)
ordispider(bcomnp.uu.nmds, groups=mdat.bcomnp$Type_3, show.groups="veitchii", col = adjustcolor(customcol8[8],0.2),spider="median", lwd=3)

## Bray-Curtis distance matrix
bcomnp.bc.dist <- vegdist(bcomnp.r)

## Run mantel test and ordisurf on pH, volume, and DNA concentration
min(mdat.bcomnp$pH)
max(mdat.bcomnp$pH)
bcomnp.pH <- vegdist(mdat.bcomnp$pH,method="euclidean") 
bcomnppH.man <- mantel(bcomnp.uu.dist,bcomnp.pH, permutations=9999)
bcomnppH.man

min(mdat.bcomnp$Volume)
max(mdat.bcomnp$Volume)
bcomnp.vol <- vegdist(mdat.bcomnp$Volume,method="euclidean") 
bcomnpvol.man <- mantel(bcomnp.uu.dist,bcomnp.vol, permutations=9999)
bcomnpvol.man

bcomnp.dnac <- vegdist(mdat.bcomnp$DNA_conc,method="euclidean") 
bcomnp.dnac.man <- mantel(bcomnp.uu.dist,bcomnp.dnac, permutations=9999)
bcomnp.dnac.man

bcomnppH.bc.man <- mantel(bcomnp.bc.dist,bcomnp.pH, permutations=9999)
bcomnppH.bc.man

bcomnpvol.bc.man <- mantel(bcomnp.bc.dist,bcomnp.vol, permutations=9999)
bcomnpvol.bc.man

bcomnp.dnac.bc.man <- mantel(bcomnp.bc.dist,bcomnp.dnac, permutations=9999)
bcomnp.dnac.bc.man


bcomnpph.ord <- ordisurf(bcomnp.uu.nmds,mdat.bcomnp$pH, permutations=9999, plot=F)
summary(bcomnpph.ord)

bcomnpvol.ord <- ordisurf(bcomnp.uu.nmds,mdat.bcomnp$Volume, permutations=9999, plot=F)
summary(bcomnpvol.ord)

bcomnpdnac.ord <- ordisurf(bcomnp.uu.nmds,mdat.bcomnp$DNA_conc, permutations=9999, plot=F)
summary(bcomnpdnac.ord)

## Run adonis and envfit on site and species
bcomnp.ad <- adonis(bcomnp.uu.dist ~ Type_3 + Site, data=mdat.bcomnp, permutations=9999)
bcomnp.ad

bcomnp.env <- envfit(bcomnp.uu.nmds,mdat.bcomnp[c("Type_3","Site")], permutations=9999)
bcomnp.env

bcomnp.bc.ad <- adonis(bcomnp.bc.dist ~ Type_3 + Site, data=mdat.bcomnp, permutations=9999)
bcomnp.bc.ad

## Adjust p-values for multiple comparisons
bcomnp.pvals <- c(bcomnp.ad[[1]][["Pr(>F)"]][[1]],bcomnp.ad[[1]][["Pr(>F)"]][[2]], +
                  bcomnp.bc.ad[[1]][["Pr(>F)"]][[1]],bcomnp.bc.ad[[1]][["Pr(>F)"]][[2]], +
                  bcomnp.env[[2]][["pvals"]][[1]],bcomnp.env[[2]][["pvals"]][[2]], +
                  bcomnppH.man[[4]],bcomnpvol.man[[4]],bcomnp.dnac.man[[4]], +
                  bcomnppH.bc.man[[4]],bcomnpvol.bc.man[[4]],bcomnp.dnac.bc.man[[4]], +
                  summary(bcomnpph.ord)[[8]],summary(bcomnpvol.ord)[[8]],summary(bcomnpdnac.ord)[[8]])
bcomnp.adjpvals <- p.adjust(bcomnp.pvals, method = "BH")
bcomnp.adjpvals
format(bcomnp.adjpvals, scientific=FALSE)

####### Subset to just N. gracilis
bcomng <- subset(bcomnp, mdat.bcomnp$Type_3=="gracilis")
mdat.bcomng <- subset(mdat.bcomnp, row.names(mdat.bcomnp) %in% row.names(bcomng)) 

mdat.bcomng$Location <- mdat.bcomng$Location[drop=T]
mdat.bcomng$Type_2 <- mdat.bcomng$Type_2[drop=T]
mdat.bcomng$Type_3 <- mdat.bcomng$Type_3[drop=T]
mdat.bcomng$Site <- mdat.bcomng$Site[drop=T]
mdat.bcomng$pH <- mdat.bcomng$pH[drop=T]

## Check OTU table and rarefy to 4000 seqs
summary(colSums(bcomng))
summary(rowSums(bcomng))
bcomng <- bcomng[,colSums(bcomng) > 0]
bcomng.r <- rrarefy(bcomng,4000)
summary(colSums(bcomng.r))
bcomng.r <- bcomng.r[,colSums(bcomng.r) > 0]

## Prune tree and put into format for phyloseq
bcomng.tree <- prune.sample(bcomng.r, btree)
OTU.bcomng = otu_table(as.matrix(bcomng.r), taxa_are_rows = FALSE)
SAM.bcomng = sample_data(mdat.bcomng)
bcomng.physeq <-merge_phyloseq(phyloseq(OTU.bcomng),SAM.bcomng,bcomng.tree)

## Measure Unifrac distance 
bcomng.uu.dist <- distance(bcomng.physeq,"uUniFrac")

## Run mantel test on pH 
min(mdat.bcomng$pH)
max(mdat.bcomng$pH)
bcomng.pH <- vegdist(mdat.bcomng$pH,method="euclidean") 
bcomngpH.man <- mantel(bcomng.uu.dist,bcomng.pH, permutations=9999)
bcomngpH.man

####### Subset to just N. rafflesiana
bcomnr <- subset(bcomnp, mdat.bcomnp$Type_3=="rafflesiana")
mdat.bcomnr <- subset(mdat.bcomnp, row.names(mdat.bcomnp) %in% row.names(bcomnr)) 

mdat.bcomnr$Location <- mdat.bcomnr$Location[drop=T]
mdat.bcomnr$Type_2 <- mdat.bcomnr$Type_2[drop=T]
mdat.bcomnr$Type_3 <- mdat.bcomnr$Type_3[drop=T]
mdat.bcomnr$Site <- mdat.bcomnr$Site[drop=T]
mdat.bcomnr$pH <- mdat.bcomnr$pH[drop=T]

## Check OTU table and rarefy to 4000 seqs
summary(colSums(bcomnr))
summary(rowSums(bcomnr))
bcomnr <- bcomnr[,colSums(bcomnr) > 0]
bcomnr.r <- rrarefy(bcomnr,4000)
summary(colSums(bcomnr.r))
bcomnr.r <- bcomnr.r[,colSums(bcomnr.r) > 0]

## Prune tree and put into format for phyloseq
bcomnr.tree <- prune.sample(bcomnr.r, btree)
OTU.bcomnr = otu_table(as.matrix(bcomnr.r), taxa_are_rows = FALSE)
SAM.bcomnr = sample_data(mdat.bcomnr)
bcomnr.physeq <-merge_phyloseq(phyloseq(OTU.bcomnr),SAM.bcomnr,bcomnr.tree)

## Measure Unifrac distance 
bcomnr.uu.dist <- distance(bcomnr.physeq,"uUniFrac")

## Run mantel test on pH 
min(mdat.bcomnr$pH)
max(mdat.bcomnr$pH)
bcomnr.pH <- vegdist(mdat.bcomnr$pH,method="euclidean") 
bcomnrpH.man <- mantel(bcomnr.uu.dist,bcomnr.pH, permutations=9999)
bcomnrpH.man


####### Subset to just N. stenophylla
bcomns <- subset(bcomnp, mdat.bcomnp$Type_3=="stenophylla")
mdat.bcomns <- subset(mdat.bcomnp, row.names(mdat.bcomnp) %in% row.names(bcomns)) 

mdat.bcomns$Location <- mdat.bcomns$Location[drop=T]
mdat.bcomns$Type_2 <- mdat.bcomns$Type_2[drop=T]
mdat.bcomns$Type_3 <- mdat.bcomns$Type_3[drop=T]
mdat.bcomns$Site <- mdat.bcomns$Site[drop=T]
mdat.bcomns$pH <- mdat.bcomns$pH[drop=T]

## Check OTU table and rarefy to 4000 seqs
summary(colSums(bcomns))
summary(rowSums(bcomns))
bcomns <- bcomns[,colSums(bcomns) > 0]
bcomns.r <- rrarefy(bcomns,4000)
summary(colSums(bcomns.r))
bcomns.r <- bcomns.r[,colSums(bcomns.r) > 0]

## Prune tree and put into format for phyloseq
bcomns.tree <- prune.sample(bcomns.r, btree)
OTU.bcomns = otu_table(as.matrix(bcomns.r), taxa_are_rows = FALSE)
SAM.bcomns = sample_data(mdat.bcomns)
bcomns.physeq <-merge_phyloseq(phyloseq(OTU.bcomns),SAM.bcomns,bcomns.tree)

## Measure Unifrac distance 
bcomns.uu.dist <- distance(bcomns.physeq,"uUniFrac")

## Run mantel test on pH
min(mdat.bcomns$pH)
max(mdat.bcomns$pH)
bcomns.pH <- vegdist(mdat.bcomns$pH,method="euclidean") 
bcomnspH.man <- mantel(bcomns.uu.dist,bcomns.pH, permutations=9999)
bcomnspH.man


#####################

##### Subset to just the Sarracenia and repeat
row.names(bcom)==row.names(mdat.bcom)
bcoms <- subset(bcom, mdat.bcom$Type_2=="Sarracenia")
mdat.bcoms <- subset(mdat.bcom, row.names(mdat.bcom) %in% row.names(bcoms)) 


mdat.bcoms$Location <- mdat.bcoms$Location[drop=T]
mdat.bcoms$Type_1 <- mdat.bcoms$Type_1[drop=T]
mdat.bcoms$Type_2 <- mdat.bcoms$Type_2[drop=T]
mdat.bcoms$Type_3 <- mdat.bcoms$Type_3[drop=T]
mdat.bcoms$Site <- mdat.bcoms$Site[drop=T]
mdat.bcoms$pH <- mdat.bcoms$pH[drop=T]
mdat.bcoms$Volume <- mdat.bcoms$Volume[drop=T]

summary(colSums(bcoms))
summary(rowSums(bcoms))
bcoms <- bcoms[,colSums(bcoms) > 0]
bcoms.r <- rrarefy(bcoms,4000)
summary(colSums(bcoms.r))
bcoms.r <- bcoms.r[,colSums(bcoms.r) > 0]

bcoms.tree <- prune.sample(bcoms.r, btree)
OTU.bcoms = otu_table(as.matrix(bcoms.r), taxa_are_rows = FALSE)
SAM.bcoms = sample_data(mdat.bcoms)
bcoms.physeq <-merge_phyloseq(phyloseq(OTU.bcoms),SAM.bcoms,bcoms.tree)

bcoms.uu.dist <- distance(bcoms.physeq,"uUniFrac")
bcoms.uu.nmds <- metaMDS(bcoms.uu.dist, trymax=800)

customcol6 <- c("darkred","tomato1","orange","palegreen2","dodgerblue2","darkblue")

plot(bcoms.uu.nmds$points, xlab="NMDS Axis 1", ylab="NMDS Axis 2", 
     main="Sarracenia bacterial community by host species", 
     col= customcol6[mdat.bcoms$Type_3],
     pch = 19)
legend("bottomright", 
       legend=c("alata","flava","leucophylla","purpurea","rosea","rubra"),
       col= customcol6,
       pch = 19)
ordisurf(bcoms.uu.nmds,mdat.bcoms$Volume, add=T, col="darkgray")
ordispider(bcoms.uu.nmds, groups=mdat.bcoms$Type_3, show.groups="alata", col = adjustcolor(customcol6[1],0.2), spiders = "median", lwd=3)
ordispider(bcoms.uu.nmds, groups=mdat.bcoms$Type_3, show.groups="flava", col = adjustcolor(customcol6[2],0.2),spider="median", lwd=3)
ordispider(bcoms.uu.nmds, groups=mdat.bcoms$Type_3, show.groups="leucophylla", col = adjustcolor(customcol6[3],0.2),spider="median", lwd=3)
ordispider(bcoms.uu.nmds, groups=mdat.bcoms$Type_3, show.groups="purpurea", col = adjustcolor(customcol6[4],0.2),spider="median", lwd=3)
ordispider(bcoms.uu.nmds, groups=mdat.bcoms$Type_3, show.groups="rosea", col = adjustcolor(customcol6[5],0.2),spider="median", lwd=3)
ordispider(bcoms.uu.nmds, groups=mdat.bcoms$Type_3, show.groups="rubra", col = adjustcolor(customcol6[6],0.2),spider="median", lwd=3)

## Bray-Curtis distance matrix
bcoms.bc.dist <- vegdist(bcoms.r)

## Run mantel test and ordisurf on pH, volume and DNA concentration
min(mdat.bcoms$pH)
max(mdat.bcoms$pH)
bcoms.pH <- vegdist(mdat.bcoms$pH,method="euclidean") 
bcomspH.man <- mantel(bcoms.uu.dist,bcoms.pH, permutations=9999)
bcomspH.man

min(mdat.bcoms$Volume)
max(mdat.bcoms$Volume)
bcoms.vol <- vegdist(mdat.bcoms$Volume,method="euclidean") 
bcomsvol.man <- mantel(bcoms.uu.dist,bcoms.vol, permutations=9999)
bcomsvol.man

min(mdat.bcoms$DNA_conc)
max(mdat.bcoms$DNA_conc)
bcoms.dna <- vegdist(mdat.bcoms$DNA_conc,method="euclidean") 
bcomsdna.man <- mantel(bcoms.uu.dist,bcoms.dna, permutations=9999)
bcomsdna.man

bcomspH.bc.man <- mantel(bcoms.bc.dist,bcoms.pH, permutations=9999)
bcomspH.bc.man

bcomsvol.bc.man <- mantel(bcoms.bc.dist,bcoms.vol, permutations=9999)
bcomsvol.bc.man

bcomsdna.bc.man <- mantel(bcoms.bc.dist,bcoms.dna, permutations=9999)
bcomsdna.bc.man

bcomsph.ord <- ordisurf(bcoms.uu.nmds,mdat.bcoms$pH, permutations=9999, plot=F)
summary(bcomsph.ord)

bcomsvol.ord <- ordisurf(bcoms.uu.nmds,mdat.bcoms$Volume, permutations=9999, plot = F)
summary(bcomsvol.ord)

bcomsdna.ord <- ordisurf(bcoms.uu.nmds,mdat.bcoms$DNA_conc, permutations=9999, plot=F)
summary(bcomsdna.ord)

## Run PERMANOVA and envfit tests
bcoms.ad <- adonis(bcoms.uu.dist ~ Type_3 + Site, data=mdat.bcoms, permutations=9999)
bcoms.ad

bcoms.bc.ad <- adonis(bcoms.bc.dist ~ Type_3 + Site, data=mdat.bcoms, permutations=9999)
bcoms.bc.ad

bcoms.env <- envfit(bcoms.uu.nmds, mdat.bcoms[c("Type_3","Site")], permutations=9999)
bcoms.env 

## Adjust p-values for multiple comparisons
bcoms.pvals <- c(bcoms.ad[[1]][["Pr(>F)"]][[1]],bcoms.ad[[1]][["Pr(>F)"]][[2]], +
                    bcoms.bc.ad[[1]][["Pr(>F)"]][[1]],bcoms.bc.ad[[1]][["Pr(>F)"]][[2]], +
                    bcoms.env[[2]][["pvals"]][[1]],bcoms.env[[2]][["pvals"]][[2]], +
                    bcomspH.man[[4]],bcomsvol.man[[4]],bcomsdna.man[[4]], +
                    bcomspH.bc.man[[4]],bcomsvol.bc.man[[4]],bcomsdna.bc.man[[4]], +
                    summary(bcomsph.ord)[[8]],summary(bcomsvol.ord)[[8]],summary(bcomsdna.ord)[[8]])
bcoms.adjpvals <- p.adjust(bcoms.pvals, method = "BH")
bcoms.adjpvals
format(bcoms.adjpvals, scientific=FALSE)


#### Subset to natural samples
bnepsar <- subset(bcom, subset = mdat.bcom$Nep_Sar %in% c("Nep_nat","Sar_nat"))
mdat.bnepsar <- subset(mdat.bcom, row.names(mdat.bcom) %in% row.names(bnepsar)) 
mdat.bnepsar <- mdat.bnepsar[order(row.names(mdat.bnepsar)),]
row.names(bnepsar) == row.names(mdat.bnepsar)

mdat.bnepsar$Location <- mdat.bnepsar$Location[drop=T]
mdat.bnepsar$Type_1 <- mdat.bnepsar$Type_1[drop=T]
mdat.bnepsar$Type_2 <- mdat.bnepsar$Type_2[drop=T]
mdat.bnepsar$Type_3 <- mdat.bnepsar$Type_3[drop=T]
mdat.bnepsar$Site <- mdat.bnepsar$Site[drop=T]
mdat.bnepsar$pH <- mdat.bnepsar$pH[drop=T]
mdat.bnepsar$Nep_Sar <- mdat.bnepsar$Nep_Sar[drop=T]

summary(colSums(bnepsar))
summary(rowSums(bnepsar))
bnepsar <- bnepsar[,colSums(bnepsar) > 0]
bnepsar.r <- rrarefy(bnepsar,4000)
summary(colSums(bnepsar.r))
bnepsar.r <- bnepsar.r[,colSums(bnepsar.r) > 0]


bnepsar.tree <- prune.sample(bnepsar.r, btree)
OTU.bnepsar = otu_table(as.matrix(bnepsar.r), taxa_are_rows = FALSE)
SAM.bnepsar = sample_data(mdat.bnepsar)
bnepsar.physeq <-merge_phyloseq(phyloseq(OTU.bnepsar),SAM.bnepsar,bnepsar.tree)

bnepsar.uu.dist <- distance(bnepsar.physeq,"uUniFrac")
bnepsar.uu.nmds <- metaMDS(bnepsar.uu.dist, trymax=800)

plot(bnepsar.uu.nmds$points, xlab="NMDS Axis 1", ylab="NMDS Axis 2", 
     main="Natural Nepenthes and Sarracenia bacterial communities", 
     col= c("red","blue")[mdat.bnepsar$Nep_Sar],
     pch=19)
legend("topright", 
       legend=c("N natural","S natural"),
       col= c("red","blue"),
       pch=19)

## Bray-Curtis distance matrix
bnepsar.bc.dist <- vegdist(bnepsar.r)

## Test for differences by genus with PERMANOVA/adonis
bnepsar.ad <- adonis(bnepsar.uu.dist ~ Type_2, mdat.bnepsar, permutations = 9999)
bnepsar.ad

bnepsar.bc.ad <- adonis(bnepsar.bc.dist ~ Type_2, mdat.bnepsar, permutations = 9999)
bnepsar.bc.ad

bnepsar.env <- envfit(bnepsar.uu.nmds, mdat.bnepsar["Type_2"], permutations = 9999)
bnepsar.env

## Adjust P-values for multiple comparisons
bnepsar.pvals <- c(bnepsar.ad[[1]][["Pr(>F)"]][[1]],bnepsar.bc.ad[[1]][["Pr(>F)"]][[1]],bnepsar.env[[2]][["pvals"]][[1]])
bnepsar.adjpvals <- p.adjust(bnepsar.pvals, method = "BH")
bnepsar.adjpvals
format(bnepsar.adjpvals, scientific=FALSE)


#### Subset to experimental samples, including glass tube controls
bnib <- subset(botu.t, bmdat$Project == "Experiment")
mdat.bnib <- subset(bmdat, row.names(bmdat) %in% row.names(bnib)) 
bnib2 <- subset(bnib, subset = mdat.bnib$Type_2 != "Bog")
mdat.bnib2 <- subset(mdat.bnib, row.names(mdat.bnib) %in% row.names(bnib2)) 

## Add in natural Nepenthes samples for comparison
bfig0 <- subset(botu.t, subset = bmdat$Nep_Sar %in% c("Nep_nat"))
bfig <- rbind(bfig0,bnib2)
bfig <- bfig[order(row.names(bfig)),]
mdat.bfig <- subset(bmdat, row.names(bmdat) %in% row.names(bfig)) 
mdat.bfig <- mdat.bfig[order(row.names(mdat.bfig)),]
row.names(bfig) == row.names(mdat.bfig)

mdat.bfig$Location <- mdat.bfig$Location[drop=T]
mdat.bfig$Type_1 <- mdat.bfig$Type_1[drop=T]
mdat.bfig$Type_2 <- mdat.bfig$Type_2[drop=T]
mdat.bfig$Type_3 <- mdat.bfig$Type_3[drop=T]
mdat.bfig$Site <- mdat.bfig$Site[drop=T]
mdat.bfig$pH <- mdat.bfig$pH[drop=T]
mdat.bfig$Nep_Sar <- mdat.bfig$Nep_Sar[drop=T]

summary(colSums(bfig))
summary(rowSums(bfig))
bfig <- bfig[,colSums(bfig) > 0]
bfig.r <- rrarefy(bfig,4000)
summary(colSums(bfig.r))
bfig.r <- bfig.r[,colSums(bfig.r) > 0]

## remove samples without pH measures
bfig1.r <- subset(bfig.r, mdat.bfig$pH != "NA")
mdat.bfig1 <- subset(mdat.bfig, row.names(mdat.bfig) %in% row.names(bfig1.r)) 

mdat.bfig1.pH <- as.factor(mdat.bfig1$pH)

bfig1.tree <- prune.sample(bfig1.r, btree)
OTU.bfig1 = otu_table(as.matrix(bfig1.r), taxa_are_rows = FALSE)
SAM.bfig1 = sample_data(mdat.bfig1)
bfig1.physeq <-merge_phyloseq(phyloseq(OTU.bfig1),SAM.bfig1,bfig1.tree)

bfig1.uu.dist <- distance(bfig1.physeq,"uUniFrac")
bfig1.uu.nmds <- metaMDS(bfig1.uu.dist, trymax=800)


plot(bfig1.uu.nmds$points, xlab="NMDS Axis 1", ylab="NMDS Axis 2", 
     col= c("gray60","black","orange","red","dodgerblue")[mdat.bfig1$Nep_Sar],
     pch=c(17,17,17,17,17,17,17,17,19,19,19,19,19,19,19,19,19,19,19)[mdat.bfig1.pH])
legend("bottomleft", 
       legend=c("Gt","Gtp","N (exp)","N (nat)","S (exp)"),
       col= c("gray60","black","orange","red","dodgerblue"),
       pch=19)
legend("bottomright", 
       legend=c("pH < 4"),
       col= "gray",
       pch=17)

## Bray-Curtis distance matrix
bfig1.bc.dist <- vegdist(bfig1.r)

## Test correlations with mantel and ordisurf tests
bfig1.pH <- vegdist(mdat.bfig1$pH,method="euclidean") 
bfig1pH.man <- mantel(bfig1.uu.dist,bfig1.pH, permutations=9999)
bfig1pH.man

bfig1pH.bc.man <- mantel(bfig1.bc.dist,bfig1.pH, permutations=9999)
bfig1pH.bc.man

bfig1ph.ord <- ordisurf(bfig1.uu.nmds,mdat.bfig1$pH, plot=F, permutations=9999)
summary(bfig1ph.ord)

## Run PERMANOVA tests
bfig1.ad <- adonis(bfig1.uu.dist ~ Location, data=mdat.bfig1, permutations=9999)
bfig1.ad

bfig1.ad1 <- adonis(bfig1.uu.dist ~ Type_1, data=mdat.bfig1, permutations=9999) # pitcher vs tube
bfig1.ad1


bfig1.bc.ad <- adonis(bfig1.bc.dist ~ Location, data=mdat.bfig1, permutations=9999)
bfig1.bc.ad

bfig1.bc.ad1 <- adonis(bfig1.bc.dist ~ Type_1, data=mdat.bfig1, permutations=9999) # pitcher vs tube
bfig1.bc.ad1

## ENVFIT test
bfig1.env <- envfit(bfig1.uu.nmds, mdat.bfig1[c("Location","Type_1")], permutations=9999)
bfig1.env 

## Adjust p-values for multiple comparisons
bfig1.pvals <- c(bfig1.ad[[1]][["Pr(>F)"]][[1]],bfig1.ad1[[1]][["Pr(>F)"]][[1]], +
                   bfig1.bc.ad[[1]][["Pr(>F)"]][[1]],bfig1.bc.ad1[[1]][["Pr(>F)"]][[1]], +
                   bfig1.env[[2]][["pvals"]][[1]],bfig1.env[[2]][["pvals"]][[2]], +
                   bfig1pH.man[[4]],bfig1pH.bc.man[[4]],summary(bfig1ph.ord)[[8]])
bfig1.adjpvals <- p.adjust(bfig1.pvals, method = "BH")
bfig1.adjpvals
format(bfig1.adjpvals, scientific=FALSE)


### Figure 1 figure supplement 1 of all Southeast Asia samples
bcomsea <- subset(botu.t, subset = bmdat$Location %in% c("Maliau_Basin","Singapore"))
mdat.bcomsea <- subset(bmdat, row.names(bmdat) %in% row.names(bcomsea)) 

mdat.bcomsea$Location <- mdat.bcomsea$Location[drop=T]
mdat.bcomsea$Type_2 <- mdat.bcomsea$Type_2[drop=T]
mdat.bcomsea$Type_3 <- mdat.bcomsea$Type_3[drop=T]
mdat.bcomsea$Site <- mdat.bcomsea$Site[drop=T]
mdat.bcomsea$pH <- mdat.bcomsea$pH[drop=T]

summary(colSums(bcomsea))
summary(rowSums(bcomsea))
bcomsea <- bcomsea[,colSums(bcomsea) > 0]

bcomsea.r <- rrarefy(bcomsea,4000)
summary(colSums(bcomsea.r))
bcomsea.r <- bcomsea.r[,colSums(bcomsea.r) > 0]

bcomsea.tree <- prune.sample(bcomsea.r, btree)
OTU.bcomsea = otu_table(as.matrix(bcomsea.r), taxa_are_rows = FALSE)
SAM.bcomsea = sample_data(mdat.bcomsea)
bcomsea.physeq <-merge_phyloseq(phyloseq(OTU.bcomsea),SAM.bcomsea,bcomsea.tree)

bcomsea.uu.dist <- distance(bcomsea.physeq,"uUniFrac")
bcomsea.uu.nmds <- metaMDS(bcomsea.uu.dist, trymax=500)

plot(bcomsea.uu.nmds$points, xlab="NMDS Axis 1", ylab="NMDS Axis 2", 
     main="Bacterial communities in Southeast Asian habitats", 
     col= c("brown","green3","red","black","gray")[mdat.bcomsea$Type_2],
     pch=19)
legend("topright", 
       legend=c("bog water","leaf water", "Nepenthes","soil","tube"),
       col= c("brown","green3","red","black","gray"),
       pch = 19)
ordispider(bcomsea.uu.nmds, groups=mdat.bcomsea$Type_2, show.groups="Bog", col = adjustcolor("brown",0.2), spiders = "median", lwd=3)
ordispider(bcomsea.uu.nmds, groups=mdat.bcomsea$Type_2, show.groups="Nepenthes", col = adjustcolor("red",0.2),spider="median", lwd=3)
ordispider(bcomsea.uu.nmds, groups=mdat.bcomsea$Type_2, show.groups="Leaf", col = adjustcolor("green3",0.2),spider="median", lwd=3)
ordispider(bcomsea.uu.nmds, groups=mdat.bcomsea$Type_2, show.groups="Soil", col = adjustcolor("black",0.2),spider="median", lwd=3)
ordispider(bcomsea.uu.nmds, groups=mdat.bcomsea$Type_2, show.groups="Tube", col = adjustcolor("gray",0.2),spider="median", lwd=3)



#########################
######### 18S ###########

### Repeat the above steps with the 18S data
otu.df <- read.table("otu_table_18S_obs10_bittleston.txt",sep="\t",head=T,row.names=1, check.names=F)
otu.notax <- otu.df[,1:472]
summary(rowSums(otu.notax))
summary(colSums(otu.notax))
otu.t <- t(otu.notax)
otu.t <- otu.t[order(row.names(otu.t)),]

mdat <- read.csv("Supplementary_Dataset_1_Metadata.csv", head=T, row.names=1) #get metadata
mdat <- mdat[order(row.names(mdat)),]
mdat <- subset(mdat, row.names(mdat) %in% row.names(otu.t)) 

com <- subset(otu.t, mdat$Project=="Comparison")
mdat.com <- subset(mdat, row.names(mdat) %in% row.names(com)) 
com <- subset(com, mdat.com$Nep_Sar!="Other")
mdat.com <- subset(mdat.com, row.names(mdat.com) %in% row.names(com)) 

mdat.com$Location <- mdat.com$Location[drop=T]
mdat.com$Type_2 <- mdat.com$Type_2[drop=T]
mdat.com$Type_3 <- mdat.com$Type_3[drop=T]
mdat.com$Site <- mdat.com$Site[drop=T]
mdat.com$pH <- mdat.com$pH[drop=T]

summary(colSums(com))
summary(rowSums(com))
com <- com[,colSums(com) > 0]
com.r <- rrarefy(com,4000)
summary(colSums(com.r))
com.r <- com.r[,colSums(com.r) > 0]

tree <- read.tree("rep_set_18S_obs10_bittleston.tre")
com.tree <- prune.sample(com.r, tree)
OTU.com = otu_table(as.matrix(com.r), taxa_are_rows = FALSE)
SAM.com = sample_data(mdat.com)
com.physeq <-merge_phyloseq(phyloseq(OTU.com),SAM.com,com.tree)

com.uu.dist <- distance(com.physeq,"uUniFrac")
com.uu.nmds <- metaMDS(com.uu.dist, k=3, trymax=1000)

plot(com.uu.nmds$points, xlab="NMDS Axis 1", ylab="NMDS Axis 2", 
     main="Eukaryotic community by Type", 
     col= c("brown","red","blue","black")[mdat.com$Type_2],
     pch = c(15,15,17,17)[mdat.com$Location])
legend("topleft", 
       legend=c("Bog","Nepenthes","Sarracenia","Soil"),
       col= c("brown","red","blue","black"),
       pch= 19)
legend("topright",
       legend=c("North America", "Southeast Asia"),
       pch = c(15, 17),
       col = "gray")
ordispider(com.uu.nmds, groups=mdat.com$Type_2, show.groups="Soil", col=adjustcolor("black",0.2), spiders = "median", lwd=3)
ordispider(com.uu.nmds, groups=mdat.com$Type_2, show.groups="Bog", col=adjustcolor("brown",0.2), spiders = "median", lwd=3)
ordispider(com.uu.nmds, groups=mdat.com$Type_2, show.groups="Nepenthes", col=adjustcolor("red",0.2), spiders = "median", lwd=3)
ordispider(com.uu.nmds, groups=mdat.com$Type_2, show.groups="Sarracenia", col=adjustcolor("blue",0.2), spiders = "median", lwd=3)


com.bc.dist <- vegdist(com.r,method = "bray")

com.ad <- adonis(com.uu.dist ~ Type_1, data=mdat.com, permutations = 9999)
com.ad

com.ad1 <- adonis(com.uu.dist ~ Location, data=mdat.com, permutations = 9999)
com.ad1

com.bc.ad <- adonis(com.bc.dist ~ Type_1, data=mdat.com, permutations = 9999)
com.bc.ad

com.bc.ad1 <- adonis(com.bc.dist ~ Location, data=mdat.com, permutations = 9999)
com.bc.ad1

com.env <- envfit(com.uu.nmds, mdat.com[c("Type_1","Location")], permutations = 9999)
com.env

## Adjust p-values for multiple comparisons
com.pvals <- c(com.ad[[1]][["Pr(>F)"]][[1]],com.ad1[[1]][["Pr(>F)"]][[1]], +
                  com.bc.ad[[1]][["Pr(>F)"]][[1]],com.bc.ad1[[1]][["Pr(>F)"]][[1]], +
                  com.env[[2]][["pvals"]][[1]],com.env[[2]][["pvals"]][[2]])
com.adjpvals <- p.adjust(com.pvals, method = "BH")
com.adjpvals

#### Subset to just the Nepenthes

comn <- subset(com, mdat.com$Type_2=="Nepenthes")
mdat.comn <- subset(mdat.com, row.names(mdat.com) %in% row.names(comn)) 

### Remove the samples without pH
comnp <- subset(comn, mdat.comn$pH!="NA")
mdat.comnp <- subset(mdat.comn, row.names(mdat.comn) %in% row.names(comnp)) 

mdat.comnp$Location <- mdat.comnp$Location[drop=T]
mdat.comnp$Type_1 <- mdat.comnp$Type_1[drop=T]
mdat.comnp$Type_2 <- mdat.comnp$Type_2[drop=T]
mdat.comnp$Type_3 <- mdat.comnp$Type_3[drop=T]
mdat.comnp$Site <- mdat.comnp$Site[drop=T]
mdat.comnp$pH <- mdat.comnp$pH[drop=T]
mdat.comnp$Volume <- mdat.comnp$Volume[drop=T]
mdat.comnp$DNA_conc <- mdat.comnp$DNA_conc[drop=T]

summary(colSums(comnp))
summary(rowSums(comnp))
comnp <- comnp[,colSums(comnp) > 0]
comnp.r <- rrarefy(comnp,4000)
summary(colSums(comnp.r))
comnp.r <- comnp.r[,colSums(comnp.r) > 0]

comnp.tree <- prune.sample(comnp.r, tree)
OTU.comnp = otu_table(as.matrix(comnp.r), taxa_are_rows = FALSE)
SAM.comnp = sample_data(mdat.comnp)
comnp.physeq <-merge_phyloseq(phyloseq(OTU.comnp),SAM.comnp,comnp.tree)

comnp.uu.dist <- distance(comnp.physeq,"uUniFrac")
comnp.uu.nmds <- metaMDS(comnp.uu.dist, k=3, trymax=200)
# comnp.uu.nmds <- metaMDS(comnp.uu.dist, k=3, trymax=200, previous.best=comnp.uu.nmds)

customcol8 <- c("darkred","tomato1","orange","yellow","palegreen2","dodgerblue2","darkblue","violetred")
plot(comnp.uu.nmds$points, xlab="NMDS Axis 1", ylab="NMDS Axis 2", 
     main="Nepenthes eukaryotic community by host species", 
     col= customcol8[mdat.comnp$Type_3],
     pch =19)
legend("bottomright", 
       legend=c("ampullaria","gracilis","hirsuta","rafflesiana","reinwardtiana","stenophylla","tentaculara","veitchii"),
       col= customcol8,
       pch = 19)
ordispider(comnp.uu.nmds, groups=mdat.comnp$Type_3, show.groups="ampullaria", col = adjustcolor(customcol8[1],0.2), spiders = "median", lwd=3)
ordispider(comnp.uu.nmds, groups=mdat.comnp$Type_3, show.groups="gracilis", col = adjustcolor(customcol8[2],0.2),spider="median", lwd=3)
ordispider(comnp.uu.nmds, groups=mdat.comnp$Type_3, show.groups="hirsuta", col = adjustcolor(customcol8[3],0.2),spider="median", lwd=3)
ordispider(comnp.uu.nmds, groups=mdat.comnp$Type_3, show.groups="rafflesiana", col = adjustcolor(customcol8[4],0.2),spider="median", lwd=3)
ordispider(comnp.uu.nmds, groups=mdat.comnp$Type_3, show.groups="reinwardtiana", col = adjustcolor(customcol8[5],0.2),spider="median", lwd=3)
ordispider(comnp.uu.nmds, groups=mdat.comnp$Type_3, show.groups="stenophylla", col = adjustcolor(customcol8[6],0.2),spider="median", lwd=3)
ordispider(comnp.uu.nmds, groups=mdat.comnp$Type_3, show.groups="tentaculata", col = adjustcolor(customcol8[7],0.2),spider="median", lwd=3)
ordispider(comnp.uu.nmds, groups=mdat.comnp$Type_3, show.groups="veitchii", col = adjustcolor(customcol8[8],0.2),spider="median", lwd=3)


# ### Axes 2 and 3
# plot(comnp.uu.nmds$points[,2:3], xlab="NMDS Axis 2", ylab="NMDS Axis 3", 
#      main="Nepenthes eukaryotic community by host species", 
#      col= customcol8[mdat.comnp$Type_3],
#      pch =19)
# legend("bottomright", 
#        legend=c("ampullaria","gracilis","hirsuta","rafflesiana","reinwardtiana","stenophylla","tentaculara","veitchii"),
#        col= customcol8,
#        pch = 19)
# ordispider(comnp.uu.nmds$points[,2:3], groups=mdat.comnp$Type_3, show.groups="ampullaria", col = adjustcolor(customcol8[1],0.2), spiders = "median", lwd=3)
# ordispider(comnp.uu.nmds$points[,2:3], groups=mdat.comnp$Type_3, show.groups="gracilis", col = adjustcolor(customcol8[2],0.2),spider="median", lwd=3)
# ordispider(comnp.uu.nmds$points[,2:3], groups=mdat.comnp$Type_3, show.groups="hirsuta", col = adjustcolor(customcol8[3],0.2),spider="median", lwd=3)
# ordispider(comnp.uu.nmds$points[,2:3], groups=mdat.comnp$Type_3, show.groups="rafflesiana", col = adjustcolor(customcol8[4],0.2),spider="median", lwd=3)
# ordispider(comnp.uu.nmds$points[,2:3], groups=mdat.comnp$Type_3, show.groups="reinwardtiana", col = adjustcolor(customcol8[5],0.2),spider="median", lwd=3)
# ordispider(comnp.uu.nmds$points[,2:3], groups=mdat.comnp$Type_3, show.groups="stenophylla", col = adjustcolor(customcol8[6],0.2),spider="median", lwd=3)
# ordispider(comnp.uu.nmds$points[,2:3], groups=mdat.comnp$Type_3, show.groups="tentaculata", col = adjustcolor(customcol8[7],0.2),spider="median", lwd=3)
# ordispider(comnp.uu.nmds$points[,2:3], groups=mdat.comnp$Type_3, show.groups="veitchii", col = adjustcolor(customcol8[8],0.2),spider="median", lwd=3)

comnp.bc.dist <- vegdist(comnp.r)

comnp.pH <- vegdist(mdat.comnp$pH,method="euclidean") 
comnppH.man <- mantel(comnp.uu.dist,comnp.pH, permutations=9999)
comnppH.man

comnp.vol <- vegdist(mdat.comnp$Volume,method="euclidean") 
comnpvol.man <- mantel(comnp.uu.dist,comnp.vol, permutations=9999)
comnpvol.man

comnp.dnac <- vegdist(mdat.comnp$DNA_conc,method="euclidean") 
comnp.dnac.man <- mantel(comnp.uu.dist,comnp.dnac, permutations=9999)
comnp.dnac.man

comnppH.bc.man <- mantel(comnp.bc.dist,comnp.pH, permutations=9999)
comnppH.bc.man

comnpvol.bc.man <- mantel(comnp.bc.dist,comnp.vol, permutations=9999)
comnpvol.bc.man

comnp.dnac.bc.man <- mantel(comnp.bc.dist,comnp.dnac, permutations=9999)
comnp.dnac.bc.man


comnpph.ord <- ordisurf(comnp.uu.nmds,mdat.comnp$pH, plot=F, permutations=9999)
summary(comnpph.ord)

comnpvol.ord <- ordisurf(comnp.uu.nmds,mdat.comnp$Volume, plot=F, permutations=9999)
summary(comnpvol.ord)

comnpdnac.ord <- ordisurf(comnp.uu.nmds,mdat.comnp$DNA_conc, permutations=9999, plot=F)
summary(comnpdnac.ord)


comnp.ad <- adonis(comnp.uu.dist ~ Type_3 + Site, data=mdat.comnp, permutations=9999)
comnp.ad

comnp.bc.ad <- adonis(comnp.bc.dist ~ Type_3 + Site, data=mdat.comnp, permutations=9999)
comnp.bc.ad

comnp.env <- envfit(comnp.uu.nmds,mdat.comnp[c("Type_3","Site")], permutations=9999)
comnp.env

## Adjust p-values for multiple comparisons
comnp.pvals <- c(comnp.ad[[1]][["Pr(>F)"]][[1]],comnp.ad[[1]][["Pr(>F)"]][[2]], +
                    comnp.bc.ad[[1]][["Pr(>F)"]][[1]],comnp.bc.ad[[1]][["Pr(>F)"]][[2]], +
                    comnp.env[[2]][["pvals"]][[1]],comnp.env[[2]][["pvals"]][[2]], +
                    comnppH.man[[4]],comnpvol.man[[4]],comnp.dnac.man[[4]], +
                    comnppH.bc.man[[4]],comnpvol.bc.man[[4]],comnp.dnac.bc.man[[4]], +
                    summary(comnpph.ord)[[8]],summary(comnpvol.ord)[[8]],summary(comnpdnac.ord)[[8]])
comnp.adjpvals <- p.adjust(comnp.pvals, method = "BH")
comnp.adjpvals
format(comnp.adjpvals, scientific=FALSE)

#####################

##### Subset to just the Sarracenia and repeat
coms <- subset(com, mdat.com$Type_2=="Sarracenia")
mdat.coms <- subset(mdat.com, row.names(mdat.com) %in% row.names(coms)) 

mdat.coms$Location <- mdat.coms$Location[drop=T]
mdat.coms$Type_1 <- mdat.coms$Type_1[drop=T]
mdat.coms$Type_2 <- mdat.coms$Type_2[drop=T]
mdat.coms$Type_3 <- mdat.coms$Type_3[drop=T]
mdat.coms$Site <- mdat.coms$Site[drop=T]
mdat.coms$pH <- mdat.coms$pH[drop=T]
mdat.coms$Volume <- mdat.coms$Volume[drop=T]
mdat.coms$DNA_conc <- mdat.coms$DNA_conc[drop=T]

summary(colSums(coms))
summary(rowSums(coms))
coms <- coms[,colSums(coms) > 0]
coms.r <- rrarefy(coms,4000)
summary(colSums(coms.r))
coms.r <- coms.r[,colSums(coms.r) > 0]

coms.tree <- prune.sample(coms.r, tree)
OTU.coms = otu_table(as.matrix(coms.r), taxa_are_rows = FALSE)
SAM.coms = sample_data(mdat.coms)
coms.physeq <-merge_phyloseq(phyloseq(OTU.coms),SAM.coms,coms.tree)

coms.uu.dist <- distance(coms.physeq,"uUniFrac")
coms.uu.nmds <- metaMDS(coms.uu.dist, trymax=800)

customcol6 <- c("darkred","tomato1","orange","palegreen2","dodgerblue2","darkblue")

plot(coms.uu.nmds$points, xlab="NMDS Axis 1", ylab="NMDS Axis 2", 
     main="Sarracenia eukaryotic community by host species", 
     col= customcol6[mdat.coms$Type_3],
     pch = 19)
legend("topright", 
       legend=c("alata","flava","leucophylla","purpurea","rosea","rubra"),
       col= customcol6,
       pch = 19)
ordispider(coms.uu.nmds, groups=mdat.coms$Type_3, show.groups="alata", col = adjustcolor(customcol6[1],0.2), spiders = "median", lwd=3)
ordispider(coms.uu.nmds, groups=mdat.coms$Type_3, show.groups="flava", col = adjustcolor(customcol6[2],0.2),spider="median", lwd=3)
ordispider(coms.uu.nmds, groups=mdat.coms$Type_3, show.groups="leucophylla", col = adjustcolor(customcol6[3],0.2),spider="median", lwd=3)
ordispider(coms.uu.nmds, groups=mdat.coms$Type_3, show.groups="purpurea", col = adjustcolor(customcol6[4],0.2),spider="median", lwd=3)
ordispider(coms.uu.nmds, groups=mdat.coms$Type_3, show.groups="rosea", col = adjustcolor(customcol6[5],0.2),spider="median", lwd=3)
ordispider(coms.uu.nmds, groups=mdat.coms$Type_3, show.groups="rubra", col = adjustcolor(customcol6[6],0.2),spider="median", lwd=3)

coms.bc.dist <- vegdist(coms.r)

coms.pH <- vegdist(mdat.coms$pH,method="euclidean") 
comspH.man <- mantel(coms.uu.dist,coms.pH, permutations=9999)
comspH.man

coms.vol <- vegdist(mdat.coms$Volume,method="euclidean") 
comsvol.man <- mantel(coms.uu.dist,coms.vol, permutations=9999)
comsvol.man

coms.dna <- vegdist(mdat.coms$DNA_conc,method="euclidean") 
comsdna.man <- mantel(coms.uu.dist,coms.dna, permutations=9999)
comsdna.man

comspH.bc.man <- mantel(coms.bc.dist,coms.pH, permutations=9999)
comspH.bc.man

comsvol.bc.man <- mantel(coms.bc.dist,coms.vol, permutations=9999)
comsvol.bc.man

comsdna.bc.man <- mantel(coms.bc.dist,coms.dna, permutations=9999)
comsdna.bc.man

comsph.ord <- ordisurf(coms.uu.nmds,mdat.coms$pH, plot=F, permutations=9999)
summary(comsph.ord)

comsvol.ord <- ordisurf(coms.uu.nmds,mdat.coms$Volume, plot = F, permutations=9999)
summary(comsvol.ord)

comsdna.ord <- ordisurf(coms.uu.nmds,mdat.coms$DNA_conc, permutations=9999, plot=F)
summary(comsdna.ord)

coms.ad <- adonis(coms.uu.dist ~ Type_3 + Site, data=mdat.coms, permutations=9999)
coms.ad

coms.bc.ad <- adonis(coms.bc.dist ~ Type_3 + Site, data=mdat.coms, permutations=9999)
coms.bc.ad

coms.env <- envfit(coms.uu.nmds, mdat.coms[c("Type_3","Site")], permutations=9999)
coms.env 

## Adjust p-values for multiple comparisons
coms.pvals <- c(coms.ad[[1]][["Pr(>F)"]][[1]],coms.ad[[1]][["Pr(>F)"]][[2]], +
                   coms.bc.ad[[1]][["Pr(>F)"]][[1]],coms.bc.ad[[1]][["Pr(>F)"]][[2]], +
                   coms.env[[2]][["pvals"]][[1]],coms.env[[2]][["pvals"]][[2]], +
                   comspH.man[[4]],comsvol.man[[4]],comsdna.man[[4]], +
                   comspH.bc.man[[4]],comsvol.bc.man[[4]],comsdna.bc.man[[4]], +
                   summary(comsph.ord)[[8]],summary(comsvol.ord)[[8]],summary(comsdna.ord)[[8]])
coms.adjpvals <- p.adjust(coms.pvals, method = "BH")
coms.adjpvals
format(coms.adjpvals, scientific=FALSE)


#### Subset to natural samples
nepsar <- subset(com, subset = mdat.com$Nep_Sar %in% c("Nep_nat","Sar_nat"))
mdat.nepsar <- subset(mdat.com, row.names(mdat.com) %in% row.names(nepsar)) 
mdat.nepsar <- mdat.nepsar[order(row.names(mdat.nepsar)),]
row.names(nepsar) == row.names(mdat.nepsar)

mdat.nepsar$Location <- mdat.nepsar$Location[drop=T]
mdat.nepsar$Type_1 <- mdat.nepsar$Type_1[drop=T]
mdat.nepsar$Type_2 <- mdat.nepsar$Type_2[drop=T]
mdat.nepsar$Type_3 <- mdat.nepsar$Type_3[drop=T]
mdat.nepsar$Site <- mdat.nepsar$Site[drop=T]
mdat.nepsar$pH <- mdat.nepsar$pH[drop=T]
mdat.nepsar$Nep_Sar <- mdat.nepsar$Nep_Sar[drop=T]

summary(colSums(nepsar))
summary(rowSums(nepsar))
nepsar <- nepsar[,colSums(nepsar) > 0]
nepsar.r <- rrarefy(nepsar,4000)
summary(colSums(nepsar.r))
nepsar.r <- nepsar.r[,colSums(nepsar.r) > 0]


nepsar.tree <- prune.sample(nepsar.r, tree)
OTU.nepsar = otu_table(as.matrix(nepsar.r), taxa_are_rows = FALSE)
SAM.nepsar = sample_data(mdat.nepsar)
nepsar.physeq <-merge_phyloseq(phyloseq(OTU.nepsar),SAM.nepsar,nepsar.tree)

nepsar.uu.dist <- distance(nepsar.physeq,"uUniFrac")
nepsar.uu.nmds <- metaMDS(nepsar.uu.dist, k=3, trymax=600)

plot(nepsar.uu.nmds$points, xlab="NMDS Axis 1", ylab="NMDS Axis 2", 
     main="Natural Nepenthes and Sarracenia eukaryotic communities", 
     col= c("red","blue")[mdat.nepsar$Nep_Sar],
     pch=19)
legend("bottomleft", 
       legend=c("N natural","S natural"),
       col= c("red","blue"),
       pch=19)

nepsar.bc.dist <- vegdist(nepsar.r)

nepsar.ad <- adonis(nepsar.uu.dist ~ Type_2, mdat.nepsar, permutations = 9999)
nepsar.ad

nepsar.bc.ad <- adonis(nepsar.bc.dist ~ Type_2, mdat.nepsar, permutations = 9999)
nepsar.bc.ad

nepsar.env <- envfit(nepsar.uu.nmds, mdat.nepsar["Type_2"], permutations = 9999)
nepsar.env

## Adjust P-values for multiple comparisons
nepsar.pvals <- c(nepsar.ad[[1]][["Pr(>F)"]][[1]],nepsar.bc.ad[[1]][["Pr(>F)"]][[1]],nepsar.env[[2]][["pvals"]][[1]])
nepsar.adjpvals <- p.adjust(nepsar.pvals, method = "BH")
nepsar.adjpvals
format(nepsar.adjpvals, scientific=FALSE)

### Subset to experimental samples, including glass tube controls
nib <- subset(otu.t, mdat$Project == "Experiment")
mdat.nib <- subset(mdat, row.names(mdat) %in% row.names(nib)) 
nib2 <- subset(nib, subset = mdat.nib$Type_2 != "Bog")

fig0 <- subset(otu.t, subset = mdat$Nep_Sar %in% c("Nep_nat"))
fig <- rbind(fig0,nib2)
fig <- fig[order(row.names(fig)),]
mdat.fig <- subset(mdat, row.names(mdat) %in% row.names(fig)) 
mdat.fig <- mdat.fig[order(row.names(mdat.fig)),]
row.names(fig) == row.names(mdat.fig)

mdat.fig$Location <- mdat.fig$Location[drop=T]
mdat.fig$Type_1 <- mdat.fig$Type_1[drop=T]
mdat.fig$Type_2 <- mdat.fig$Type_2[drop=T]
mdat.fig$Type_3 <- mdat.fig$Type_3[drop=T]
mdat.fig$Site <- mdat.fig$Site[drop=T]
mdat.fig$pH <- mdat.fig$pH[drop=T]
mdat.fig$Volume <- mdat.fig$Volume[drop=T]
mdat.fig$Nep_Sar <- mdat.fig$Nep_Sar[drop=T]

summary(colSums(fig))
summary(rowSums(fig))
fig <- fig[,colSums(fig) > 0]
fig.r <- rrarefy(fig,4000)
summary(colSums(fig.r))
fig.r <- fig.r[,colSums(fig.r) > 0]

fig1.r <- subset(fig.r, mdat.fig$pH != "NA")
mdat.fig1 <- subset(mdat.fig, row.names(mdat.fig) %in% row.names(fig1.r)) 

mdat.fig1.pH <- as.factor(mdat.fig1$pH)

fig1.tree <- prune.sample(fig1.r, tree)
OTU.fig1 = otu_table(as.matrix(fig1.r), taxa_are_rows = FALSE)
SAM.fig1 = sample_data(mdat.fig1)
fig1.physeq <-merge_phyloseq(phyloseq(OTU.fig1),SAM.fig1,fig1.tree)

fig1.uu.dist <- distance(fig1.physeq,"uUniFrac")
fig1.uu.nmds <- metaMDS(fig1.uu.dist, trymax=600)


plot(fig1.uu.nmds$points, xlab="NMDS Axis 1", ylab="NMDS Axis 2", 
     col= c("gray60","black","orange","red","dodgerblue")[mdat.fig1$Nep_Sar],
     pch=c(17,17,17,17,17,17,17,17,19,19,19,19,19,19,19,19,19,19,19)[mdat.fig1.pH])
legend("topright", 
       legend=c("Gt","Gtp","N (exp)","N (nat)","S (exp)"),
       col= c("gray60","black","orange","red","dodgerblue"),
       pch=19)
legend("topleft", 
       legend=c("pH < 4"),
       col= "gray",
       pch=17)

fig1.bc.dist <- vegdist(fig1.r)

fig1.pH <- vegdist(mdat.fig1$pH,method="euclidean") 
fig1pH.man <- mantel(fig1.uu.dist,fig1.pH, permutations=9999)
fig1pH.man

fig1pH.bc.man <- mantel(fig1.bc.dist,fig1.pH, permutations=9999)
fig1pH.bc.man

fig1ph.ord <- ordisurf(fig1.uu.nmds,mdat.fig1$pH, plot=F, permutations=9999)
summary(fig1ph.ord)


fig1.ad <- adonis(fig1.uu.dist ~ Location, data=mdat.fig1, permutations=9999)
fig1.ad

fig1.ad1 <- adonis(fig1.uu.dist ~ Type_1, data=mdat.fig1, permutations=9999) # pitcher vs tube
fig1.ad1

fig1.bc.ad <- adonis(fig1.bc.dist ~ Location, data=mdat.fig1, permutations=9999)
fig1.bc.ad

fig1.bc.ad1 <- adonis(fig1.bc.dist ~ Type_1, data=mdat.fig1, permutations=9999) # pitcher vs tube
fig1.bc.ad1

fig1.env <- envfit(fig1.uu.nmds, mdat.fig1[c("Location","Type_1")], permutations=9999)
fig1.env 

## Adjust p-values for multiple comparisons
fig1.pvals <- c(fig1.ad[[1]][["Pr(>F)"]][[1]],fig1.ad1[[1]][["Pr(>F)"]][[1]], +
                   fig1.bc.ad[[1]][["Pr(>F)"]][[1]],fig1.bc.ad1[[1]][["Pr(>F)"]][[1]], +
                   fig1.env[[2]][["pvals"]][[1]],fig1.env[[2]][["pvals"]][[2]], +
                   fig1pH.man[[4]],fig1pH.bc.man[[4]],summary(fig1ph.ord)[[8]])
fig1.adjpvals <- p.adjust(fig1.pvals, method = "BH")
fig1.adjpvals
format(fig1.adjpvals, scientific=FALSE)

### Subset to Southeast Asia samples
comsea <- subset(otu.t, subset = mdat$Location %in% c("Maliau_Basin","Singapore"))
mdat.comsea <- subset(mdat, row.names(mdat) %in% row.names(comsea)) 

mdat.comsea$Location <- mdat.comsea$Location[drop=T]
mdat.comsea$Type_2 <- mdat.comsea$Type_2[drop=T]
mdat.comsea$Type_3 <- mdat.comsea$Type_3[drop=T]
mdat.comsea$Site <- mdat.comsea$Site[drop=T]
mdat.comsea$pH <- mdat.comsea$pH[drop=T]
mdat.comsea <- mdat.comsea[rownames(mdat.comsea) != "S06", ]
comsea <- subset(comsea, row.names(comsea) %in% row.names(mdat.comsea)) 

summary(colSums(comsea))
summary(rowSums(comsea))
comsea <- comsea[,colSums(comsea) > 0]

comsea.r <- rrarefy(comsea,4000)
summary(colSums(comsea.r))
comsea.r <- comsea.r[,colSums(comsea.r) > 0]

comsea.tree <- prune.sample(comsea.r, tree)
OTU.comsea = otu_table(as.matrix(comsea.r), taxa_are_rows = FALSE)
SAM.comsea = sample_data(mdat.comsea)
comsea.physeq <-merge_phyloseq(phyloseq(OTU.comsea),SAM.comsea,comsea.tree)

comsea.uu.dist <- distance(comsea.physeq,"uUniFrac")
comsea.uu.nmds <- metaMDS(comsea.uu.dist, k=3, trymax=500)

plot(comsea.uu.nmds$points, xlab="NMDS Axis 1", ylab="NMDS Axis 2", 
     main="Eukaryotic communities in Southeast Asian habitats", 
     col= c("brown","green3","red","black","gray")[mdat.comsea$Type_2],
     pch=19)
legend("bottomleft", 
       legend=c("bog water","leaf water", "Nepenthes","soil","tube"),
       col= c("brown","green3","red","black","gray"),
       pch = 19)
ordispider(comsea.uu.nmds, groups=mdat.comsea$Type_2, show.groups="Nepenthes", col = adjustcolor("red",0.2),spider="median", lwd=3)
ordispider(comsea.uu.nmds, groups=mdat.comsea$Type_2, show.groups="Leaf", col = adjustcolor("green3",0.2),spider="median", lwd=3)
ordispider(comsea.uu.nmds, groups=mdat.comsea$Type_2, show.groups="Soil", col = adjustcolor("black",0.2),spider="median", lwd=3)
ordispider(comsea.uu.nmds, groups=mdat.comsea$Type_2, show.groups="Tube", col = adjustcolor("gray",0.2),spider="median", lwd=3)

# plot(comsea.uu.nmds$points[,2:3], xlab="NMDS Axis 2", ylab="NMDS Axis 3", 
#      main="Eukaryotic communities in Southeast Asian habitats", 
#      col= c("brown","green3","red","black","gray")[mdat.comsea$Type_2],
#      pch=19)
# legend("bottomleft", 
#        legend=c("bog water","leaf water", "Nepenthes","soil","tube"),
#        col= c("brown","green3","red","black","gray"),
#        pch = 19)
# ordispider(comsea.uu.nmds$points[,2:3], groups=mdat.comsea$Type_2, show.groups="Nepenthes", col = adjustcolor("red",0.2),spider="median", lwd=3)
# ordispider(comsea.uu.nmds$points[,2:3], groups=mdat.comsea$Type_2, show.groups="Leaf", col = adjustcolor("green3",0.2),spider="median", lwd=3)
# ordispider(comsea.uu.nmds$points[,2:3], groups=mdat.comsea$Type_2, show.groups="Soil", col = adjustcolor("black",0.2),spider="median", lwd=3)
# ordispider(comsea.uu.nmds$points[,2:3], groups=mdat.comsea$Type_2, show.groups="Tube", col = adjustcolor("gray",0.2),spider="median", lwd=3)


#############
##### Comparing Shannon Diversity for Figs 1B, S3, and S4, and Table S3
### 16S
adiv.b <- read.table("16s_adiv_shannon_pd_bittleston.txt",sep="\t",head=T,row.names=1, check.names=F)
adiv.b <- adiv.b[order(row.names(adiv.b)),]

### Subset to soil, bog, Nepenthes and Sarracenia
mdat1.bcom <- subset(mdat.bcom, mdat.bcom$Type_2 %in% c("Bog","Nepenthes","Sarracenia","Soil"))

## Drop factors no longer present in dataset
mdat1.bcom$Type_2 <- mdat1.bcom$Type_2[drop=T]
mdat1.bcom$Type_2 <- factor(mdat1.bcom$Type_2, c("Soil","Bog","Nepenthes","Sarracenia"))

adiv1.bcom <- subset(adiv.b, row.names(adiv.b) %in% row.names(mdat1.bcom)) 

## Shannon diversity by Type
beanplot(adiv1.bcom$shannon~mdat1.bcom$Type_2) # Make beanplot

kruskal.test(adiv1.bcom$shannon~mdat1.bcom$Type_2) # Mann Whitney U test, also known as Kruskal-Wallis rank sum test

## Mean and sd for volume by type
tapply(mdat1.bcom$Volume, mdat1.bcom$Type_2, mean, na.rm=TRUE)
tapply(mdat1.bcom$Volume, mdat1.bcom$Type_2, sd, na.rm=TRUE)

## Mean and sd for shannon diversity by type
tapply(adiv1.bcom$shannon, mdat1.bcom$Type_2, mean)
tapply(adiv1.bcom$shannon, mdat1.bcom$Type_2, sd)

### Repeat with dataset of samples all extracted with the same volume
bcomv <- subset(bcom, mdat.bcom$DNAext_set =="Yes")
mdat.bcomv <- subset(mdat1.bcom, row.names(mdat1.bcom) %in% row.names(bcomv)) 

## Remove factors no longer present in subset
mdat.bcomv$Type_2 <- mdat.bcomv$Type_2[drop=T]

## Subset alpha diversity table
adiv2.bcom <- subset(adiv.b, row.names(adiv.b) %in% row.names(mdat.bcomv)) 

## Mean and standard deviation of volume within this set, by type
tapply(mdat.bcomv$Volume, mdat.bcomv$Type_2, mean)
tapply(mdat.bcomv$Volume, mdat.bcomv$Type_2, sd)

## Shannon diversity by Type for subset
beanplot(adiv2.bcom$shannon~mdat.bcomv$Type_2) # Make beanplot

kruskal.test(adiv2.bcom$shannon~mdat.bcomv$Type_2) 

tapply(mdat1.bcom$Volume, mdat1.bcom$Type_2, mean, na.rm=TRUE)
tapply(mdat1.bcom$Volume, mdat1.bcom$Type_2, sd, na.rm=TRUE)

## Mean and sd for shannon diversity in subset by type
tapply(adiv2.bcom$shannon, mdat.bcomv$Type_2, mean)
tapply(adiv2.bcom$shannon, mdat.bcomv$Type_2, sd)

### Subset Shannon alpha diversity to just Sarracenia
adivs.bcom <- subset(adiv.b, row.names(adiv.b) %in% row.names(mdat.bcoms)) 

## Shannon diversity by DNA concentration
model.adivbsdna <- lm(adivs.bcom$shannon ~ mdat.bcoms$DNA_conc)
summary(model.adivbsdna)

gplot.adivbsdna <- ggplot(mdat.bcoms, aes(x= DNA_conc, y= adivs.bcom$shannon, label=row.names(mdat.bcoms))) +
  geom_point() + geom_text(aes(label=row.names(mdat.bcoms)),hjust=0, vjust=0) +
  geom_smooth(method='lm')
gplot.adivbsdna + labs(y="Shannon diversity", x="DNA concentration") + theme_minimal()

## quadratic model for Shannon alpha diversity by pH
pH2bs <- (mdat.bcoms$pH)^2
model.adivbsph <- lm(adivs.bcom$shannon ~ mdat.bcoms$pH + pH2bs)
summary(model.adivbsph)

gplot.adivbsph <- ggplot(mdat.bcoms, aes(x= pH, y= adivs.bcom$shannon, label=row.names(mdat.bcoms))) +
  geom_point() + geom_text(aes(label=row.names(mdat.bcoms)),hjust=0, vjust=0) +
  geom_smooth(method='lm',formula = y ~ x + I(x^2))
gplot.adivbsph + labs(y="Shannon diversity") + theme_minimal()

### Subset Shannon alpha diversity to just Nepenthes with pH values
adivn.bcom <- subset(adiv.b, row.names(adiv.b) %in% row.names(mdat.bcomnp)) 

## Shannon diversity by DNA concentration
model.adivbndna <- lm(adivn.bcom$shannon ~ mdat.bcomnp$DNA_conc)
summary(model.adivbndna)

gplot.adivbndna <- ggplot(mdat.bcomnp, aes(x= DNA_conc, y= adivn.bcom$shannon, label=row.names(mdat.bcomnp))) +
  geom_point() + geom_text(aes(label=row.names(mdat.bcomnp)),hjust=0, vjust=0) 
gplot.adivbndna + labs(y="Shannon diversity", x="DNA concentration") + theme_minimal()


## quadratic model for alpha diversity by pH
pH2bn <- (mdat.bcomnp$pH)^2
model.adivbnph <- lm(adivn.bcom$shannon ~ mdat.bcomnp$pH + pH2bn)
summary(model.adivbnph)

gplot.adivbnph <- ggplot(mdat.bcomnp, aes(x= pH, y= adivn.bcom$shannon, label=row.names(mdat.bcomnp))) +
  geom_point() + geom_text(aes(label=row.names(mdat.bcomnp)),hjust=0, vjust=0) +
  geom_smooth(method='lm',formula = y ~ x + I(x^2))
gplot.adivbnph + labs(y="Shannon diversity") + theme_minimal()


####### Repeat Shannon diversity analyses with 18S

adiv <- read.table("18s_adiv_shannon_pd_bittleston.txt",sep="\t",head=T,row.names=1, check.names=F)
adiv <- adiv[order(row.names(adiv)),]

### Subset to soil, bog, Nepenthes and Sarracenia
mdat1.com <- subset(mdat.com, mdat.com$Type_2 %in% c("Bog","Nepenthes","Sarracenia","Soil"))
adiv1.com <- subset(adiv, row.names(adiv) %in% row.names(mdat1.com)) 

mdat1.com$Type_2 <- mdat1.com$Type_2[drop=T]
mdat1.com$Type_2 <- factor(mdat1.com$Type_2, c("Soil","Bog","Nepenthes","Sarracenia"))

beanplot(adiv1.com$shannon~mdat1.com$Type_2)

kruskal.test(adiv1.com$shannon~mdat1.com$Type_2)

## Mean and sd for sample volume by type
tapply(mdat1.com$Volume, mdat1.com$Type_2, mean, na.rm=TRUE)
tapply(mdat1.com$Volume, mdat1.com$Type_2, sd, na.rm=TRUE)

## Mean and sd for shannon diversity by type
tapply(adiv1.com$shannon, mdat1.com$Type_2, mean)
tapply(adiv1.com$shannon, mdat1.com$Type_2, sd)

### Repeat with dataset of samples all extracted with the same volume
comv <- subset(com, mdat.com$DNAext_set =="Yes")
mdat.comv <- subset(mdat1.com, row.names(mdat1.com) %in% row.names(comv)) 

## Remove factors no longer present in subset
mdat.comv$Type_2 <- mdat.comv$Type_2[drop=T]

## Subset alpha diversity table
adiv2.com <- subset(adiv, row.names(adiv) %in% row.names(mdat.comv)) 

## Mean and SD of volume within subset, by type
tapply(mdat.comv$Volume, mdat.comv$Type_2, mean)
tapply(mdat.comv$Volume, mdat.comv$Type_2, sd)

## Shannon diversity by Type for subset
beanplot(adiv2.com$shannon~mdat.comv$Type_2) # Make beanplot

kruskal.test(adiv2.com$shannon~mdat.comv$Type_2) 

## Mean and sd for shannon diversity in subset by type
tapply(adiv2.com$shannon, mdat.comv$Type_2, mean)
tapply(adiv2.com$shannon, mdat.comv$Type_2, sd)



##########
### Rarefaction curves for Figure 1B

## 16S observed species data
obs <- read.table("observed_species_16s_6500.txt",sep="\t",head=T,row.names=1, check.names=F)

## Alpha rarefactions, means
ar1 <- obs[mean(1:10),3:441]
ar2 <- obs[mean(11:20),3:441]
ar3 <- obs[mean(21:30),3:441]
ar4 <- obs[mean(31:40),3:441]
ar5 <- obs[mean(41:50),3:441]
ar6 <- obs[mean(51:60),3:441]
ar7 <- obs[mean(61:70),3:441]
ar8 <- obs[mean(71:80),3:441]
ar9 <- obs[mean(81:90),3:441]
ar10 <- obs[mean(91:100),3:441]
ar11 <- obs[mean(101:110),3:441]

mar <- rbind(ar1,ar2,ar3,ar4,ar5,ar6,ar7,ar8,ar9,ar10,ar11)
mar <- mar[,order(colnames(mar))]
sequences <- c(10,659,1308,1957,2606,3255,3904,4553,5202,5851,6500)
row.names(mar) <- sequences
tmar <- t(mar)

tmar2 <- subset(tmar, row.names(tmar) %in% row.names(mdat1.bcom)) 
mdat2.bcom <- subset(mdat1.bcom, row.names(mdat1.bcom) %in% row.names(tmar2)) 

m1 <- by(tmar2[,1],mdat2.bcom$Type_2,mean)
m2 <- by(tmar2[,2],mdat2.bcom$Type_2,mean)
m3 <- by(tmar2[,3],mdat2.bcom$Type_2,mean)
m4 <- by(tmar2[,4],mdat2.bcom$Type_2,mean)
m5 <- by(tmar2[,5],mdat2.bcom$Type_2,mean)
m6 <- by(tmar2[,6],mdat2.bcom$Type_2,mean)
m7 <- by(tmar2[,7],mdat2.bcom$Type_2,mean)
m8 <- by(tmar2[,8],mdat2.bcom$Type_2,mean)
m9 <- by(tmar2[,9],mdat2.bcom$Type_2,mean)
m10 <- by(tmar2[,10],mdat2.bcom$Type_2,mean)
m11 <- by(tmar2[,11],mdat2.bcom$Type_2,mean)

tmar2.m <- cbind(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11)
colnames(tmar2.m) <- sequences

tmar2.ml <- melt(tmar2.m, value.name = "Observed_OTUs")
names(tmar2.ml)[names(tmar2.ml)=="Var1"] <- "Type"
names(tmar2.ml)[names(tmar2.ml)=="Var2"] <- "Sequences"

## Standard deviations
sd1 <- by(tmar2[,1],mdat2.bcom$Type_2,sd)
sd2 <- by(tmar2[,2],mdat2.bcom$Type_2,sd)
sd3 <- by(tmar2[,3],mdat2.bcom$Type_2,sd)
sd4 <- by(tmar2[,4],mdat2.bcom$Type_2,sd)
sd5 <- by(tmar2[,5],mdat2.bcom$Type_2,sd)
sd6 <- by(tmar2[,6],mdat2.bcom$Type_2,sd)
sd7 <- by(tmar2[,7],mdat2.bcom$Type_2,sd)
sd8 <- by(tmar2[,8],mdat2.bcom$Type_2,sd)
sd9 <- by(tmar2[,9],mdat2.bcom$Type_2,sd)
sd10 <- by(tmar2[,10],mdat2.bcom$Type_2,sd)
sd11 <- by(tmar2[,11],mdat2.bcom$Type_2,sd)

tmar2.sd <- cbind(sd1,sd2,sd3,sd4,sd5,sd6,sd7,sd8,sd9,sd10,sd11)
tmar2.sdl <- melt(tmar2.sd, value.name = "SD")

tmar2.ml$SD <- tmar2.sdl$SD

## Plot curves with SDs
ggplot(tmar2.ml, aes(x = Sequences, y = Observed_OTUs, colour = Type)) + 
  geom_line(aes(group = Type)) + 
  geom_point() + 
  geom_errorbar(aes(ymax = Observed_OTUs + SD,ymin = Observed_OTUs - SD), width=50) +
  scale_color_manual(values=c("black","brown","red","blue")) +
  theme_classic() 

## Check if observed OTUs correlate with sample volume
model.obsotus.vol <- lm(tmar2[,11] ~ mdat2.bcom$Volume)
summary(model.obsotus.vol)


##### Now do the same for the 18s rarefaction plot

eobs <- read.table("observed_species_18S_4300.txt",sep="\t",head=T,row.names=1, check.names=F)

ear1 <- eobs[mean(1:10),3:418]
ear2 <- eobs[mean(11:20),3:418]
ear3 <- eobs[mean(21:30),3:418]
ear4 <- eobs[mean(31:40),3:418]
ear5 <- eobs[mean(41:50),3:418]
ear6 <- eobs[mean(51:60),3:418]
ear7 <- eobs[mean(61:70),3:418]
ear8 <- eobs[mean(71:80),3:418]
ear9 <- eobs[mean(81:90),3:418]
ear10 <- eobs[mean(91:100),3:418]
ear11 <- eobs[mean(101:110),3:418]

mear <- rbind(ear1,ear2,ear3,ear4,ear5,ear6,ear7,ear8,ear9,ear10,ear11)
mear <- mear[,order(colnames(mear))]
esequences <- c(10,439,868,1297,1726,2155,2584,3013,3442,3871,4300)
row.names(mear) <- esequences
tmear <- t(mear)

tmear2 <- subset(tmear, row.names(tmear) %in% row.names(mdat1.com)) 
mdat2.com <- subset(mdat1.com, row.names(mdat1.com) %in% row.names(tmear2)) 

em1 <- by(tmear2[,1],mdat2.com$Type_2,mean)
em2 <- by(tmear2[,2],mdat2.com$Type_2,mean)
em3 <- by(tmear2[,3],mdat2.com$Type_2,mean)
em4 <- by(tmear2[,4],mdat2.com$Type_2,mean)
em5 <- by(tmear2[,5],mdat2.com$Type_2,mean)
em6 <- by(tmear2[,6],mdat2.com$Type_2,mean)
em7 <- by(tmear2[,7],mdat2.com$Type_2,mean)
em8 <- by(tmear2[,8],mdat2.com$Type_2,mean)
em9 <- by(tmear2[,9],mdat2.com$Type_2,mean)
em10 <- by(tmear2[,10],mdat2.com$Type_2,mean)
em11 <- by(tmear2[,11],mdat2.com$Type_2,mean)

tmear2.m <- cbind(em1,em2,em3,em4,em5,em6,em7,em8,em9,em10,em11)
colnames(tmear2.m) <- esequences

tmear2.ml <- melt(tmear2.m, value.name = "Observed_OTUs")
names(tmear2.ml)[names(tmear2.ml)=="Var1"] <- "Type"
names(tmear2.ml)[names(tmear2.ml)=="Var2"] <- "Sequences"

esd1 <- by(tmear2[,1],mdat2.com$Type_2,sd)
esd2 <- by(tmear2[,2],mdat2.com$Type_2,sd)
esd3 <- by(tmear2[,3],mdat2.com$Type_2,sd)
esd4 <- by(tmear2[,4],mdat2.com$Type_2,sd)
esd5 <- by(tmear2[,5],mdat2.com$Type_2,sd)
esd6 <- by(tmear2[,6],mdat2.com$Type_2,sd)
esd7 <- by(tmear2[,7],mdat2.com$Type_2,sd)
esd8 <- by(tmear2[,8],mdat2.com$Type_2,sd)
esd9 <- by(tmear2[,9],mdat2.com$Type_2,sd)
esd10 <- by(tmear2[,10],mdat2.com$Type_2,sd)
esd11 <- by(tmear2[,11],mdat2.com$Type_2,sd)

tmear2.sd <- cbind(esd1,esd2,esd3,esd4,esd5,esd6,esd7,esd8,esd9,esd10,esd11)
tmear2.sdl <- melt(tmear2.sd, value.name = "SD")

tmear2.ml$SD <- tmear2.sdl$SD

ggplot(tmear2.ml, aes(x = Sequences, y = Observed_OTUs, colour = Type)) + 
  geom_line(aes(group = Type)) + 
  geom_point() + 
  geom_errorbar(aes(ymax = Observed_OTUs + SD,ymin = Observed_OTUs - SD), width=50) +
  scale_color_manual(values=c("black","brown","red","blue")) +
  theme_classic() 

model.eobsotus.vol <- lm(tmear2[,11] ~ mdat2.com$Volume)
summary(model.eobsotus.vol)

######### Code for Fig. 5B and 5C

### Load data and metadata files
metag <- read.csv("metagenomes_combined_genefam_simple_ko_norm.csv",row.names = 1)
metag <- metag[order(row.names(metag)),]

mdc <- read.table("Metageome_comparison_metadata.txt", header = T)
row.names(mdc) <- mdc$MG_Name
mdc <- mdc[order(row.names(mdc)),]

row.names(mdc)==row.names(metag) ## sanity check

mdc$TYPE2 = factor(mdc$TYPE2,c("Nepenthes","Sarracenia","Phyllo","Lake","Soil"))

## Run NMDS of KO gene pathways
metag.nmds <- metaMDS(metag, trymax = 500)

## Plot NMDS
plot(metag.nmds$points,
     main="NMDS Plot of Gene Families in KO Groups", 
     xlab="NMDS1", ylab="NMDS2",
     col=c("red","blue","green3","lightblue2","brown")[mdc$TYPE2],pch=19)
ordispider(metag.nmds, groups=mdc$TYPE2, show.groups="Lake", col=adjustcolor("lightblue2", 0.2), spiders = "median", lwd=5)
ordispider(metag.nmds, groups=mdc$TYPE2, show.groups="Nepenthes", col=adjustcolor("red", 0.2), spiders = "median", lwd=5)
ordispider(metag.nmds, groups=mdc$TYPE2, show.groups="Phyllo", col=adjustcolor("green3", 0.2), spiders = "median", lwd=5)
ordispider(metag.nmds, groups=mdc$TYPE2, show.groups="Sarracenia", col=adjustcolor("blue", 0.2), spiders = "median", lwd=5)
ordispider(metag.nmds, groups=mdc$TYPE2, show.groups="Soil", col=adjustcolor("brown", 0.2), spiders = "median", lwd=5)
legend("topright", legend=c("Nepenthes","Sarracenia","phyllosphere","lake","soil"),col=c("red","blue","green3","lightblue2","brown"),pch=19,cex=.8)

#### Beanplots and statistical tests for relevant enzymes

metag.df <- as.data.frame(metag)

beanplot(metag.df$K01183~mdc$TYPE2) # chitinase
title(main = "chitinase",ylab = "Log(normalized abundance)")
wilcox.1 <- wilcox.test(metag.df$K01183~mdc$TYPE)
wilcox.1

beanplot(metag.df$K01256~mdc$TYPE2) #  pepN; aminopeptidase N
title(main = "pepN; aminopeptidase N",ylab = "Log(normalized abundance)")
wilcox.2 <- wilcox.test(metag.df$K01256~mdc$TYPE)
wilcox.2

beanplot(metag.df$K01581~mdc$TYPE2) # ornithine decarboxylase
title(main = "ornithine decarboxylase",ylab = "Log(normalized abundance)")
wilcox.3 <-wilcox.test(metag.df$K01581~mdc$TYPE)
wilcox.3

beanplot(metag.df$K01582~mdc$TYPE2) # lysine decarboxylase
title(main = "lysine decarboxylase",ylab = "Log(normalized abundance)")
wilcox.4 <-wilcox.test(metag.df$K01582~mdc$TYPE)
wilcox.4

beanplot(metag.df$K00261~mdc$TYPE2) # glutamate dehydrogenase
title(main = "glutamate dehydrogenase",ylab = "Log(normalized abundance)")
wilcox.5 <-wilcox.test(metag.df$K00261~mdc$TYPE)
wilcox.5

cellulase <- (metag.df$K01225+metag.df$K05350+metag.df$K01179) 
beanplot(cellulase~mdc$TYPE2, log = "y") # cellulase
title(main = "cellulase",ylab = "Normalized abundance")
wilcox.6 <-wilcox.test(cellulase~mdc$TYPE)
wilcox.6

## Adjust pvals for multiple comparisons
wilcox.pvals <- c(wilcox.1[[3]],wilcox.2[[3]],wilcox.3[[3]],wilcox.4[[3]],wilcox.5[[3]],wilcox.6[[3]])
wilcox.adjpvals <- p.adjust(wilcox.pvals, method = "BH")
wilcox.adjpvals 
