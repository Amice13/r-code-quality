### Microcosm experiment analysis, R code for manuscript

### Initial set-up #####
## Set working directory 
setwd("~/Microcosm_Analysis/") ### Change to relevant folder

## Load packages
if (!require("vegan")) {install.packages("vegan"); require("vegan")}
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}
if (!require("dendextend")) {install.packages("dendextend"); require("dendextend")} 
if (!require("reshape2")) {install.packages("reshape2"); require("reshape2")}
if (!require("plyr")) {install.packages("plyr"); require("plyr")}
if (!require("biclust")) {install.packages("biclust"); require("biclust")}
if (!require("ggtern")) {install.packages("ggtern"); require("ggtern")}
if (!require("picante")) {install.packages("picante"); require("picante")}
# source('http://bioconductor.org/biocLite.R')
# biocLite('phyloseq') ## Phyloseq may need to be installed with bioclite
require("phyloseq")


asv.table <- read.table("asv-table-expmt-dada2.txt",sep="\t",head=T,row.names=1) ## read in asv table
summary(rowSums(asv.table))
summary(colSums(asv.table))
asv.t <- t(asv.table) # transpose
asv.t <- asv.t[order(row.names(asv.t)),] # order samples alphabetically
asv.t <- asv.t[,order(colnames(asv.t))] # order ASVs alphabetically

### Simplify ASV names
nums <- sprintf('%0.3d', 1:872)
ASVnames <- paste("ASV",nums, sep="")
ASVconversion <- data.frame(ASV_ID = colnames(asv.t),ASVnames, stringsAsFactors = F)
# write.csv(ASVconversion,"ASV_conversion.csv")
colnames(asv.t) <- ASVnames

### Load phylogenetic tree, and update ASV names
asv.tree <- read.tree("sepp_microcosm_exp_tree.tre")
asv.tree$tip.label<-ASVconversion[[2]][match(asv.tree$tip.label, ASVconversion[[1]])]

## Metadata
md <- read.csv("microcosm_expmt_metadata.csv", head=T, row.names = 1)
md <- md[order(row.names(md)),]
row.names(asv.t)==row.names(md) # sanity check

## Subset table and metadata to only the experimental Filtered ones
expf <- subset(asv.t, md$Filtered=="F") ## subset to only Filtered ones
md.expf <- subset(md, row.names(md) %in% row.names(expf)) ## subset metadata

## Normalize table 
expf <- expf[,colSums(expf) > 0]
summary(colSums(expf))
summary(rowSums(expf))
expfn <- (expf/rowSums(expf))*10000
## just frequencies
expff <- expf/rowSums(expf)

## Subset table and metadata to only the unfiltered ones
expu <- subset(asv.t, md$Filtered=="U") ## subset to only unfiltered ones
md.expu <- subset(md, row.names(md) %in% row.names(expu)) ## subset metadata

## Normalize table for unfiltered samples
expu <- expu[,colSums(expu) > 0]
summary(colSums(expu))
summary(rowSums(expu))
expun <- (expu/rowSums(expu))*10000
## just frequencies
expuf <- expu/rowSums(expu)

### Table with both filtered and unfiltered samples
exp <- asv.t
summary(colSums(exp))
summary(rowSums(exp))
expn <- (exp/rowSums(exp))*10000
summary(colSums(expn))
summary(rowSums(expn))

######### ASV Barplots ######
### Barplot at the ASV level (normalized table)
expfnt <- t(expfn)
expfnto <- expfnt[order(rowSums(expfnt),decreasing = T),]
expfntop <- expfnto[1:20,]
other <- colSums(expfnto[21:764,])
expfntopo <- rbind(expfntop,other)
# write.csv(expfntopo, "Figure1a_barplot.csv",quote = F)

col21 <- c("cadetblue4","red","orchid2","seagreen3","purple4","dodgerblue2","tomato1","darkblue","turquoise1","lightblue","darkred","mediumblue","purple","bisque","greenyellow","yellow","violetred2","darkgreen","darkgoldenrod1","deeppink3","gray27")

barplot(expfntopo,col=col21,legend.text=F,axes=F,cex.names= .3,border=NA,space=-0.1, las=2, args.legend = list(x = "topleft", bty = "n", inset=c(-0.15, 0)))
barplot(expfntopo,col=col21,legend.text=T,axes=F,cex.names=.3,las=2,border=NA,space=0,args.legend = list(x = "topleft", bty = "n", cex=0.5, inset=c(-0.1, -0.1)))

## Calculating what percent of total is the top 20 ASVs
top20 <- colSums(expfntop)
top20sum <- sum(top20)
total <- sum(top20,other)
top20sum/total

### Barplot - repeat for unfiltered samples, with same ASVs
expunt <- t(expun)
expuntop <- expunt[row.names(expunt) %in% row.names(expfntop), ]
expuntop <- expuntop[match(rownames(expfntop), rownames(expuntop)),]
other <- (10000 - colSums(expuntop))
expuntopo <- rbind(expuntop,other)
# write.csv(expuntopo, "SupplementaryFigure2a_barplot.csv",quote = F)

col21 <- c("cadetblue4","red","orchid2","seagreen3","purple4","dodgerblue2","tomato1","darkblue","turquoise1","lightblue","darkred","mediumblue","purple","bisque","greenyellow","yellow","violetred2","darkgreen","darkgoldenrod1","deeppink3","gray27")

barplot(expuntopo,col=col21,legend.text=F,axes=F,cex.names= .3,border=NA,space=-0.1, las=2, args.legend = list(x = "topleft", bty = "n", inset=c(-0.15, 0)))
barplot(expuntopo,col=col21,legend.text=T,axes=F,cex.names=.3,las=2,border=NA,space=0,args.legend = list(x = "topleft", bty = "n", cex=0.5, inset=c(-0.1, -0.1)))

####### Barplots at the Family level for SuppFig S1a ####
all.tax <- read.csv("taxonomy_fams_updated.csv")
all.tax <- all.tax[order(all.tax$Feature.ID),]

all.tax$Feature.ID == ASVconversion$ASV_ID
all.tax$Feature.ID <- as.character(ASVconversion$ASVnames)
tax.expfn <- subset(all.tax, all.tax$Feature.ID %in% colnames(expfn))
tax.expfn$Feature.ID == colnames(expfn)

newtax <- strsplit(as.character(tax.expfn$Taxon),";")
newtax <- lapply(lapply(newtax, unlist), `length<-`, max(lengths(newtax)))
newtax.df <- as.data.frame(do.call(rbind,newtax),stringsAsFactors = F)
colnames(newtax.df) <- c("Kingdom","Phylum","Class","Order","Family","Genus","Species")

row.names(newtax.df) <- tax.expfn$Feature.ID

newtax.df[is.na(newtax.df)] <- "Unknown"
newtax.df[newtax.df == " f__"] <- "Unknown"

expfn.t <- t(expfn)
summary(colSums(expfn.t))
summary(rowSums(expfn.t))

row.names(newtax.df) == row.names(expfn.t)

expfn.t2 <- data.frame(Family=as.character(newtax.df$Family),expfn.t)

expfn.t2sums <- aggregate(. ~ expfn.t2$Family, expfn.t2, sum)
summary(colSums(expfn.t2sums[,-c(1:2)]))

expfnt.fam <- as.matrix(expfn.t2sums[,-c(1:2)])
row.names(expfnt.fam) <- expfn.t2sums[,1]

expfnt.famo <- expfnt.fam[order(rowSums(expfnt.fam),decreasing = T),]
expfnt.famop <- expfnt.famo[1:20,]
other.fam <- colSums(expfnt.famo[21:88,])
expfnt.famopo <- rbind(expfnt.famop,other.fam)
# write.csv(expfnt.famopo, "SupplementaryFigure1a_barplot.csv",quote = F)

customcol <- c("darkred","cyan","mediumvioletred","tomato1","red2","mediumblue","lightseagreen","dodgerblue2","darkmagenta","yellow","orange","darkblue","darksalmon","purple4","lightsalmon","purple","lightgoldenrod","green","palegreen2","mediumspringgreen","gray27")

barplot(expfnt.famopo,col=customcol,legend.text=F,axes=F,cex.names= .3,border=NA,space=-0.1, las=2, args.legend = list(x = "topleft", bty = "n", inset=c(-0.15, 0)))
barplot(expfnt.famopo,col=customcol,legend.text=T,axes=F,cex.names=.3,las=2,border=NA,space=0,args.legend = list(x = "topleft", bty = "n", cex=0.5, inset=c(-0.08, -0.08)))



########### Alpha diversity ############

##### Shannon diversity 
expfn.sh <- diversity(expfn)

##### N, or e^Shannon diversity
expfn.N <- exp(expfn.sh)

expfn.N.md <- as.data.frame(cbind(md.expf$Microcosm,md.expf$Day,expfn.N))
names(expfn.N.md) <- c("Microcosm","Day","N")

plotN <- ggplot(subset(expfn.N.md, Day > 18), aes(x = as.factor(Microcosm), y=N)) +
  geom_boxplot(aes(fill=as.factor(Microcosm))) +
  labs(x="Microcosm",y="Effective number of species, post-Day 21") +
  theme_minimal() +
  ylim(0, 20) +
  theme(legend.position="none")
plotN

## Just the numbers for Figure 1
Day21plus.N <- subset(expfn.N.md, Day > 18)
aggregate(Day21plus.N$N, list(Day21plus.N$Microcosm), mean)

### Faith's Phylogenetic Diversity
expfn.tree <- prune.sample(expfn, asv.tree)
expfn.pd <- pd(expfn, expfn.tree, include.root=TRUE)

expfn.pd.md <- as.data.frame(cbind(md.expf$Microcosm,md.expf$Day,expfn.pd))
names(expfn.pd.md) <- c("Microcosm","Day","PD")

# write.csv(expfn.pd.md, "SupplementaryFigure1b_FaithPD.csv",quote = F)

plotPD <- ggplot(subset(expfn.pd.md, Day > 18), aes(x = as.factor(Microcosm), y=PD)) +
  geom_boxplot(aes(fill=as.factor(Microcosm))) +
  labs(x="Microcosm",y="Faith's PD, post-Day 21") +
  theme_minimal() +
  ylim(0, 20) +
  theme(legend.position="none")
plotPD

plotPD063 <- ggplot(expfn.pd.md, aes(x = Day, y=PD, group=Microcosm, col=Microcosm)) +
  geom_point() +
  geom_line() +
  labs(x="Day",y="Faith's PD") +
  theme_minimal() +
  ylim(0, 40) 
plotPD063 ### Supplementary Figure 1b


#### Alpha diversity for unfiltered samples ####
##### Unfiltered Shannon diversity ###
expun.sh <- diversity(expun)

##### Unfiltered N, or e^Shannon diversity ###
expun.N <- exp(expun.sh)

expun.N.md <- as.data.frame(cbind(md.expu$Microcosm,md.expu$Day,expun.N))
names(expun.N.md) <- c("Microcosm","Day","N")

plotN <- ggplot(subset(expun.N.md, Day > 18), aes(x = as.factor(Microcosm), y=N)) +
  geom_boxplot(aes(fill=as.factor(Microcosm))) +
  labs(x="Microcosm",y="Effective number of species, post-Day 21") +
  theme_minimal() +
  ylim(0, 20) +
  theme(legend.position="none")
plotN

## Just the numbers 
Day21plus.N <- subset(expun.N.md, Day > 18)
aggregate(Day21plus.N$N, list(Day21plus.N$Microcosm), mean)

### Faith's Phylogenetic Diversity
expun.tree <- prune.sample(expun, asv.tree)
expun.pd <- pd(expun, expun.tree, include.root=TRUE)

expun.pd.md <- as.data.frame(cbind(md.expu$Microcosm,md.expu$Day,expun.pd))
names(expun.pd.md) <- c("Microcosm","Day","PD")

plotPDu <- ggplot(subset(expun.pd.md, Day > 18), aes(x = as.factor(Microcosm), y=PD)) +
  geom_boxplot(aes(fill=as.factor(Microcosm))) +
  labs(x="Microcosm",y="Faith's PD, post-Day 21") +
  theme_minimal() +
  ylim(0, 20) +
  theme(legend.position="none")
plotPDu

plotPDu363 <- ggplot(expun.pd.md, aes(x = Day, y=PD, group=Microcosm, col=Microcosm)) +
  geom_point() +
  geom_line() +
  labs(x="Day",y="Faith's PD") +
  theme_minimal() +
  ylim(0, 40) 
plotPDu363

##### DNA Concentration Graph for Figure 1 ####
DNAconc <- md.expf[,c(1:2,5)]
DNAconc.m <- melt(DNAconc, id=c("Microcosm","Day"))
DNAconc.m <- DNAconc.m[order(DNAconc.m$Microcosm),]
# write.csv(DNAconc.m, "Figure1a_DNAconc.csv",quote = F)

ggplot(DNAconc.m, aes(Day, value, colour= Microcosm)) + 
  geom_smooth(method = 'loess') +
  geom_point(size = 2) +
  facet_grid(~ Microcosm) +
  scale_y_continuous(name="DNA Concentration (ng/uL)", limits=c(0, 60)) +
  theme_minimal() +
  theme(legend.position="none")

### DNA conc. unfiltered, Supp. Fig. S2 ####
DNAconcu <- md[,c(1:3,5)]
DNAconcu.m <- melt(DNAconcu, id=c("Microcosm","Day","Filtered"))
DNAconcu.m <- DNAconcu.m[order(DNAconcu.m$Microcosm),]
# write.csv(DNAconcu.m, "SupplementaryFigure2a_DNAconc.csv",quote = F)

ggplot(DNAconcu.m, aes(Day, value, colour= Microcosm, group=Filtered)) + 
  geom_smooth(method = 'loess', aes(linetype=Filtered), se=F) +
  geom_point(size = 2, aes(shape=Filtered)) +
  scale_shape_manual(values=c(19, 24)) +
  facet_grid(~ Microcosm) +
  scale_y_continuous(name="DNA Concentration (ng/uL)", limits=c(0, 60)) +
  theme_minimal() +
  theme(legend.position="none")



############# Community dissimilarity across days for Figure 1 ########

microcosm = unique(c(as.character(md.expf$Microcosm)))

big = data.frame()
Days = seq(3,63, by=3)

for (i in microcosm) {
  # i == "M02"
  sps <- subset(expfn, md.expf$Microcosm == i) 
  sps <- sps[,colSums(sps) > 0]
  iDist = vegdist(sps, method="bray") # Bray-Curtis dissimilarity
  vov = as.matrix(iDist)
  comps = c()
  for (n in 1:(ncol(vov)-1)){
    comps = c(comps, vov[n, n+1])}
  pop = data.frame(Microcosm = rep(i,21), Day = Days, Dist = comps )
  big = rbind(big, pop)
}

# write.csv(big, "Figure1c_Dissim.csv",quote = F)

ggplot(big) +
  geom_vline(xintercept = 21) +
  geom_jitter(aes(x = Day, y = Dist, color = Microcosm), width = 0.3) +
  labs(x = "Days", y = "Bray-Curtis distance between adjacent days") +
  geom_smooth(aes(x = Day, y = Dist), colour="black", method = "loess", formula = y~x) +
  theme_minimal() 

#### Comm. dissim. for unfiltered samples ####
bigu = data.frame()

for (i in microcosm) {
  # i == "M02"
  spsu <- subset(expun, md.expu$Microcosm == i) 
  spsu <- spsu[,colSums(spsu) > 0]
  iDistu = vegdist(spsu, method="bray") # Bray-Curtis dissimilarity
  vovu = as.matrix(iDistu)
  compsu = c()
  for (n in 1:(ncol(vovu)-1)){
    compsu = c(compsu, vovu[n, n+1])}
  popu = data.frame(Microcosm = rep(i,20), Day = Days[2:21], Dist = compsu )
  bigu = rbind(bigu, popu)
}

# write.csv(bigu, "SupplementaryFigure2c_DissimUnfilt.csv",quote = F)

ggplot(bigu) +
  geom_vline(xintercept = 21) +
  geom_jitter(aes(x = Day, y = Dist, color = Microcosm), width = 0.3) +
  labs(x = "Days", y = "Bray-Curtis distance between adjacent days") +
  geom_smooth(aes(x = Day, y = Dist), colour="black", method = "loess", formula = y~x) +
  theme_minimal() 

### Plotting both datasets together
ggplot() +
  geom_jitter(data = bigu, aes(x = Day, y = Dist, color = Microcosm), shape = 24, width = 0.3) +
  geom_jitter(data = big, aes(x = Day, y = Dist, color = Microcosm), width = 0.3) +
  geom_vline(xintercept = 21) +
  labs(x = "Days", y = "Bray-Curtis distance between adjacent days") +
  geom_smooth(data = bigu, aes(x = Day, y = Dist), colour="blue", alpha = 0.5, method = "loess", formula = y~x) +
  geom_smooth(data = big, aes(x = Day, y = Dist), colour="black", method = "loess", formula = y~x) +
  theme_minimal() 

########### Change in wUniFrac dissimilarity over time for Supp. Fig. S1d ######
### Put into phyloseq format, filtered set
asv.expfn = otu_table(as.matrix(expfn), taxa_are_rows = FALSE)
sam.expf = sample_data(md.expf)
expfn.physeq <-merge_phyloseq(phyloseq(asv.expfn),sam.expf,expfn.tree)

## Calculate wUniFrac
expfn.wu.dist <- distance(expfn.physeq,"wUniFrac")
expfn.wu.m <- as.matrix(expfn.wu.dist)

bigw = data.frame()
for (i in microcosm) {
  # i == "M02"
  vovw = expfn.wu.m[md.expf$Microcosm == i,md.expf$Microcosm == i]
  compsw = c()
  for (n in 1:(ncol(vov)-1)){
    compsw = c(compsw, vovw[n, n+1])}
  popw = data.frame(Microcosm = rep(i,21), Day = Days, Dist = compsw )
  bigw = rbind(bigw, popw)
}

# write.csv(bigw, "SupplementaryFigure1d_WUdissim.csv",quote = F)

ggplot(bigw) +
  geom_vline(xintercept = 21) +
  geom_jitter(aes(x = Day, y = Dist, color = Microcosm), width = 0.3) +
  labs(x = "Days", y = "Weighted Unifrac distance between adjacent days") +
  geom_smooth(aes(x = Day, y = Dist), colour="black", method = "loess", formula = y~x) +
  theme_minimal() 


######## NMDS plots #########

# expfn.nmds <- metaMDS(expfn,trymax = 500)
# expfn.nmds <- metaMDS(expfn,trymax = 2000, previous.best = expfn.nmds)
# save(expfn.nmds, file = "data/expfn_nmds.rda")

load("expfn_nmds.rda") ### For reproducibility

ggplotcol10 <- c("#F8766D","#D89000","#A3A500","#39B600","#00BF7D","#00BFC4","#00B0F6","#9590FF","#E76BF3","#FF62BC")

plotnmds1 <- ordiplot(expfn.nmds, type = "none", display = "sites")
ordiarrows(expfn.nmds, groups=as.factor(md.expf$Microcosm), col=c(ggplotcol10), length=0.1)
text(plotnmds1, "sites", cex=0.5)
points(plotnmds1, "sites", pch=19, col=ggplotcol10[md.expf$Microcosm])

## Without text or visible arrowheads
plotnmds2 <- ordiplot(expfn.nmds, type = "none", display = "sites")
ordiarrows(expfn.nmds, groups=as.factor(md.expf$Microcosm), col=c(ggplotcol10), length=0.05)
points(plotnmds2, "sites", pch=19, col=ggplotcol10[md.expf$Microcosm])

# write.csv(expfn.nmds$points, "Figure1b_NMDS.csv",quote = F)

#### UniFrac Analyses ####

## Calculate Unifrac distance and run NMDS
# expfn.wu.dist <- distance(expfn.physeq,"wUniFrac")
# expfn.wu.nmds <- metaMDS(expfn.wu.dist, trymax=500)
# save(expfn.wu.nmds, file = "data/expfn_wu_nmds.rda")
load("expfn_wu_nmds.rda")

ggplotcol10 <- c("#F8766D","#D89000","#A3A500","#39B600","#00BF7D","#00BFC4","#00B0F6","#9590FF","#E76BF3","#FF62BC")

plotnmds.wu <- ordiplot(expfn.wu.nmds, type = "none", display = "sites")
ordiarrows(expfn.wu.nmds, groups=as.factor(md.expf$Microcosm), col=c(ggplotcol10), length=0.1)
text(plotnmds.wu, "sites", cex=0.5)
points(plotnmds.wu, "sites", pch=19, col=ggplotcol10[md.expf$Microcosm])

plotnmds2.wu <- ordiplot(expfn.wu.nmds, type = "none", display = "sites")
ordiarrows(expfn.wu.nmds, groups=as.factor(md.expf$Microcosm), col=c(ggplotcol10), length=0.05)
points(plotnmds2.wu, "sites", pch=19, col=ggplotcol10[md.expf$Microcosm])

# write.csv(expfn.wu.nmds$points, "SupplementaryFigure1c_NMDS.csv",quote = F)

### NMDS with both repetitions (filt and unfilt) ####
exp <- asv.t
summary(colSums(exp))
summary(rowSums(exp))
expn <- (exp/rowSums(exp))*10000

summary(colSums(expn))
summary(rowSums(expn))

# expn.nmds <- metaMDS(expn, trymax = 500)
# expn.nmds <- metaMDS(expn,trymax = 10000, previous.best = expn.nmds)
# save(expn.nmds, file = "data/expn_nmds.rda")
load("expn_nmds.rda")

md$Type <- paste(md$Microcosm,md$Filtered) ## just to make something unique for each

plotnmds.both <- ordiplot(expn.nmds, type = "none", display = "sites")
ordiarrows(expn.nmds, groups=as.factor(md$Type), col=rep(ggplotcol10,each=2), length=0.1)
# text(plotnmds.both, "sites", cex=0.5)
points(plotnmds.both, "sites", pch=c(19,2)[md$Filtered], lwd=c(1,2)[md$Filtered], col=ggplotcol10[md$Microcosm])

# write.csv(expn.nmds$points, "SupplementaryFigure2b_NMDS.csv",quote = F)

#### Filtered and unfiltered uUniFrac distance comparison ####
expn.tree <- prune.sample(expn, asv.tree)
asv.expn = otu_table(as.matrix(expn), taxa_are_rows = FALSE)
sam.exp = sample_data(md)
expn.physeq <-merge_phyloseq(phyloseq(asv.expn),sam.exp,expn.tree)

expnf.physeq <- subset_samples(expn.physeq, Filtered=="F" & Day < 21 & Day !=0)
expnu.physeq <- subset_samples(expn.physeq, Filtered=="U" & Day < 21)

expnf.uu.dist <- distance(expnf.physeq,"uUniFrac")
expnu.uu.dist <- distance(expnu.physeq,"uUniFrac")

plot(expnf.uu.dist,expnu.uu.dist)

filt.unfilt.uu.man <- mantel(expnf.uu.dist,expnu.uu.dist, permutations = 9999)
filt.unfilt.uu.man

expnf.uu.dist.m <- melt(as.matrix(expnf.uu.dist))
expnu.uu.dist.m <- melt(as.matrix(expnu.uu.dist))

together.uu <- data.frame(filt=expnf.uu.dist.m$value, unfilt=expnu.uu.dist.m$value)

lm.uu <- lm(filt~unfilt, data = together.uu)
summary(lm.uu)

ggplot() + 
  geom_hex(data=together.uu, mapping=aes(x=filt, y=unfilt), bins = 30) + 
  labs(x="Unweighted Unifrac, filtered", y="Unweighted Unifrac, unfiltered") +
  theme_classic() 

# write.csv(together.uu, "Figure3c_hexplot.csv",quote = F)

##### EcoPlate functional data analysis #####
ecofxn <- read.csv("Ecoplate_data_D0-63.csv", row.names = 1, check.names = F)
ecofxn <- ecofxn[order(row.names(ecofxn)),]

# ecofxn.nmds <- metaMDS(ecofxn,trymax = 500)
# save(ecofxn.nmds, file = "data/ecofxn_nmds.rda")
load("ecofxn_nmds.rda")

## Make ASV table and NMDS to match EcoPlates function ones
expffxn <- subset(expf, md.expf$Day %in% c("0","9","18","27","36","45","54","63")) ## same Days as ecoplates
expffxn <- expffxn[-2,] #get rid of M01.D09 to match ecoplates
md.expffxn <- subset(md.expf, row.names(md.expf) %in% row.names(expffxn))

summary(colSums(expffxn))
summary(rowSums(expffxn))
expffxnn <- (expffxn/rowSums(expffxn))*10000
expffxnn <- expffxnn[,colSums(expffxnn) > 0] # remove any ASVs that are 0

plotnmds.fxn <- ordiplot(ecofxn.nmds, type = "none", display = "sites")
ordiarrows(ecofxn.nmds, groups=as.factor(md.expffxn$Microcosm), col = c(ggplotcol10), length=0.05)
# text(plotnmds.fxn, "sites", cex=0.6)
points(plotnmds.fxn, "sites", pch=19, col=ggplotcol10[md.expffxn$Microcosm])

# write.csv(ecofxn.nmds$points, "Figure4b_NMDS.csv",quote = F)

### Subset both to just Day 63
row.names(ecofxn) <- row.names(md.expffxn) ## Give them the same row names
expf63 <- subset(expf, md.expf$Day %in% "63") 
expf63 <- expf63[,colSums(expf63) > 0] # remove any ASVs that are 0
expf63n <- (expf63/rowSums(expf63)*10000)
ecofxn63 <- subset(ecofxn, md.expffxn$Day %in% "63") 
md.expf63 <- subset(md.expf, row.names(md.expf) %in% row.names(expf63)) 

# expf63n.nmds <- metaMDS(expf63n,trymax = 500)
# save(expf63n.nmds, file = "data/expf63n_nmds.rda")
load("expf63n_nmds.rda")

# ecofxn63.nmds <- metaMDS(ecofxn63,trymax = 500)
# save(ecofxn63.nmds, file = "data/ecofxn63_nmds.rda")
load("ecofxn63_nmds.rda")

##### Correlating composition and function ######
#### Procrustes analysis comparing functional and compositional NMDS plots for D63
expf.pro63 <- protest(expf63n.nmds,ecofxn63.nmds, symmetric=T)
expf.pro63 ## Correlation in a symmetric Procrustes rotation: 0.8991, Significance:  0.001 
summary(expf.pro63)
plot(expf.pro63)

## Re-plotting the procrustes 
prodat <- as.data.frame(expf.pro63$X)
prodat <- cbind(prodat,expf.pro63$Yrot)
colnames(prodat)[colnames(prodat)=="1"] <- "Xend"
colnames(prodat)[colnames(prodat)=="2"] <- "Yend"

ggplot() + 
  geom_segment(data=prodat, mapping=aes(x=NMDS1, y=NMDS2, xend=Xend, yend=Yend), size=0.8, color=ggplotcol10) + 
  geom_point(data=prodat, mapping=aes(x=NMDS1, y=NMDS2), size=4, shape=19, color = ggplotcol10) +
  geom_point(data=prodat, mapping=aes(x=Xend, y=Yend), size=4, shape=17, color = ggplotcol10) +
  labs(x="Procrustes axis 1", y="Procrustes axis 2") +
  theme_classic() 

# write.csv(prodat, "Figure4c_Procrustes.csv",quote = F)

## Mantel test measuring correlation between the distance matrices
ecofxn.dist <- vegdist(ecofxn)
expffxnn.dist <- vegdist(expffxnn)
expf.mant <- mantel(expffxnn.dist,ecofxn.dist, permutations = 9999) 
expf.mant ## r = 0.6403, P < 0.0001

### Plotting the compositional and functional community dissimilarities

plot((expffxnn.dist)^2,sqrt(ecofxn.dist)^2, 
     xlab="Composition: BC distance squared", 
     ylab = "EcoPlates: BC distanced squared",
     ylim=c(0, 0.5))

# write.csv(cbind((expffxnn.dist)^2,sqrt(ecofxn.dist)^2), "Figure4d_BCdissims.csv",quote = F)

## Mantel test with just D63
ecofxn63.dist <- vegdist(ecofxn63)
expf63n.dist <- vegdist(expf63n)
expf63.mant <- mantel(expf63n.dist,ecofxn63.dist, permutations = 9999) 
expf63.mant ## r = 0.6907, P = 0.0021



####### ASV dynamics within microcosms #######
row.names(expf) == row.names(md.expf) ## sanity check
expf.b <- (expf > 0) # make binary logical matrix

## Define factors and make data frame
microcosm <- unique(c(as.character(md.expf$Microcosm)))
Day <- unique(c(md.expf$Day))

ASVdrop <- data.frame(matrix(nrow = 22))

## Run loop to find when ASVs drop out of each microcosm
for (i in microcosm) {
  expf.bM <- t(subset(expf.b, md.expf$Microcosm %in% i))
  expf.bMC <- apply(expf.bM,1,function(a)
    sapply(1:ncol(expf.bM),function(k)
      any(a[k:ncol(expf.bM)])))
  expf.bMCS <- rowSums(expf.bMC)
  ASVdrop <- cbind(ASVdrop,expf.bMCS)
}

ASVdrop2 <- cbind(Day,ASVdrop[,2:11])
colnames(ASVdrop2)[2:11] <- microcosm

ASVdrop.g <- melt(ASVdrop2,id.vars = "Day")
colnames(ASVdrop.g)[2:3] <- c("Microcosm", "ASVs")

ggplot(subset(ASVdrop.g, Day < 63), aes(x = Day, y = ASVs, colour = Microcosm, group=Microcosm)) + 
  geom_line() + 
  geom_point() + 
  labs(y="Richness") +
  scale_y_continuous(limits = c(0,250)) +
  theme_classic()

# write.csv(ASVdrop.g, "Figure2a_richness.csv",quote = F)

### Normalize richness by richness on Day 3 and re-plot
ASVdropnorm <- data.frame(matrix(nrow = 21))
for (i in microcosm) {
  #i == "M02"
  expf.bM <- t(subset(expf.b, md.expf$Microcosm %in% i))
  expf.bMC <- apply(expf.bM,1,function(a)
    sapply(1:ncol(expf.bM),function(k)
      any(a[k:ncol(expf.bM)])))
  expf.bMCS <- rowSums(expf.bMC)
  expf.bMCSnorm <- expf.bMCS[2:22]/expf.bMCS[2]
  ASVdropnorm <- cbind(ASVdropnorm,expf.bMCSnorm)
}

ASVdropnorm2 <- cbind(Day=Day[2:22],ASVdropnorm[,2:11])
colnames(ASVdropnorm2)[2:11] <- microcosm

ASVdropnorm3 <- ASVdropnorm2[1:18,] ## remove last three Days to avoid bias in the model

ASVdropnorm.g <- melt(ASVdropnorm3,id.vars = "Day")
colnames(ASVdropnorm.g)[2:3] <- c("Microcosm", "ASVs")

### Line from model using parameters (model done in Matlab) 
A <- 0.5097662408189789
tau <- 16.4901908762861

yy = (1-A)*exp(-(ASVdropnorm.g$Day-3)/tau)+A

ggplot(ASVdropnorm.g, aes(x = Day, y = ASVs)) + 
  geom_point(aes(group = Microcosm, col = Microcosm)) + 
  geom_line(aes(group = Microcosm, col = Microcosm)) + 
  geom_line(aes(x=ASVdropnorm.g$Day,y=yy),cex=1.5) +
  # geom_smooth(method="lm", formula= (y ~ exp(-x)), se=FALSE, col = "black") +
  labs(y="Normalized ASV Abundance") +
  scale_y_continuous(limits = c(0.1,1.1), breaks = c((1:5)/5)) +
  theme_classic() +
  theme(legend.position = "none")

# write.csv(ASVdropnorm.g, "Figure2d_normrich.csv",quote = F)

### Correlation of ASV Richness on Day 3 vs Day 63 ####
ASV.3.63 <- t(ASVdrop2[c(2,22),-1])
colnames(ASV.3.63) <- c("Day_3","Day_63")
ASV.3.63 <- data.frame(microcosm, ASV.3.63)

lm.3.63 <- lm(ASV.3.63$Day_63 ~ ASV.3.63$Day_3)
summary(lm.3.63) ## Multiple R-squared:  0.9118,	Adjusted R-squared:  0.9008, F-statistic: 82.74 on 1 and 8 DF,  p-value: 1.714e-05

plotT <- ggplot(ASV.3.63, aes(x=Day_3, y=Day_63)) +
  geom_smooth(method = "lm", col="red", fill="grey90") +
  geom_point() +
  geom_text(aes(label=microcosm),hjust=0, vjust=0) +
  labs(x="ASV Abundance on Day 3", y="ASV Abundance on Day 63") +
  theme_minimal()
plotT

# write.csv(ASV.3.63, "Figure2b_Corr.csv",quote = F)

### Correlation of Richness on Day 0 vs Day 63 for comparison
ASV.0.63 <- t(ASVdrop2[c(1,22),-1])
colnames(ASV.0.63) <- c("Day_0","Day_63")
ASV.0.63 <- data.frame(microcosm, ASV.0.63)

lm.0.63 <- lm(ASV.0.63$Day_63 ~ ASV.0.63$Day_0)
summary(lm.0.63) ## Multiple R-squared:  0.2869,	Adjusted R-squared:  0.1978, F-statistic: 3.219 on 1 and 8 DF,  p-value: 0.1105

plotT <- ggplot(ASV.0.63, aes(x=Day_0, y=Day_63)) +
  geom_smooth(method = "lm", col="red", fill="grey90") +
  geom_point() +
  geom_text(aes(label=microcosm),hjust=0, vjust=0) +
  labs(x="ASV Richness on Day 0", y="ASV Richness on Day 63") +
  theme_minimal()
plotT

#### Correlation Richness on Day 3 and Day 63, unfiltered communities ####

row.names(expu) == row.names(md.expu) ## sanity check
expu.b <- (expu > 0) # make binary logical matrix

## Define factors and make data frame
microcosm <- unique(c(as.character(md.expu$Microcosm)))
Day <- unique(c(md.expu$Day))

ASVdropu <- data.frame(matrix(nrow = 21))

## Run loop to find when ASVs drop out of each microcosm
for (i in microcosm) {
  expu.bM <- t(subset(expu.b, md.expu$Microcosm %in% i))
  expu.bMC <- apply(expu.bM,1,function(a)
    sapply(1:ncol(expu.bM),function(k)
      any(a[k:ncol(expu.bM)])))
  expu.bMCS <- rowSums(expu.bMC)
  ASVdropu <- cbind(ASVdropu,expu.bMCS)
}

ASVdropu2 <- cbind(Day,ASVdropu[,2:11])
colnames(ASVdropu2)[2:11] <- microcosm

### Correlation of ASV Abundance on Day 3 vs Day 63
ASVu.3.63 <- t(ASVdropu2[c(1,21),-1])
colnames(ASVu.3.63) <- c("Day_3","Day_63")
ASVu.3.63 <- data.frame(microcosm, ASVu.3.63)

lmu.3.63 <- lm(ASVu.3.63$Day_63 ~ ASVu.3.63$Day_3)
summary(lmu.3.63) ## Multiple R-squared:  0.6314,	Adjusted R-squared:  0.5853, F-statistic:  13.7 on 1 and 8 DF,  p-value: 0.006026

ggplot(ASVu.3.63, aes(x=Day_3, y=Day_63)) +
  geom_smooth(method = "lm", col="red", fill="grey90") +
  geom_point() +
  geom_text(aes(label=microcosm),hjust=0, vjust=0) +
  labs(x="ASV Abundance on Day 3", y="ASV Abundance on Day 63") +
  theme_minimal()

### Plot both filtered and unfiltered samples
ggplot() +
  geom_smooth(data=ASVu.3.63, aes(x=Day_3, y=Day_63), method = "lm", lty=2, col="gray", fill="lightgray") +
  geom_smooth(data=ASV.3.63, aes(x=Day_3, y=Day_63), method = "lm", col="black", fill="gray") +
  geom_point(data=ASVu.3.63, aes(x=Day_3, y=Day_63), col="gray", shape = 24) +
  geom_point(data=ASV.3.63, aes(x=Day_3, y=Day_63)) +
  geom_text(data=ASVu.3.63, aes(x=Day_3, y=Day_63, label=microcosm), col="gray",hjust=0, vjust=0) +
  geom_text(data=ASV.3.63, aes(x=Day_3, y=Day_63, label=microcosm),hjust=0, vjust=0) +
  labs(x="Richness Day 3", y="Richness Day 63") +
  theme_minimal()

# write.csv(cbind(Type=rep("filt",10),ASV.3.63,Type=rep("unfilt",10),ASVu.3.63), "SupplementaryFigure2d_3-63_Corr.csv",quote = F)

#### Graph all shared ASVs and when they drop out of different microcosms, Fig. 3a #####

## Subset to only ASVs present in multiple microcosms
W <- split(data.frame(expff,check.names = F), md.expf$Microcosm)
W[[1]][,1] ## look at it
Z <- sapply(W, function(y) colSums(y) > 0)
ASVs.tokeep <- rowSums(Z) > 1

expf.sub <- expf[,ASVs.tokeep]
expff.shared <- expff[,ASVs.tokeep]

ASVdropI <- data.frame(matrix(nrow = 209))
for (i in microcosm) {
  #i == "M02"
  expf.bM <- t(subset(expf.sub, md.expf$Microcosm %in% i))
  expf.bM[rowSums(expf.bM) == 0,] <- NA # Make any ASVs that are 0 into NA
  # expf.bM <- (expf.bM > 0)
  expf.bM <- (expf.bM > 0.001) ## Take observations above 0.001 
  expf.bME <- apply(expf.bM,1,function(a)
    max(which(a)))
  ASVdropI <- cbind(ASVdropI,expf.bME)
}

ASVdropI2 <- ASVdropI[,2:11]
colnames(ASVdropI2) <- microcosm
ASVdropI2[ASVdropI2 == "-Inf"] <- NA

ASVdropI2m <- melt(as.matrix(ASVdropI2))
colnames(ASVdropI2m) <- c("ASV","Microcosm","Timepoint")

ASVdropI2mp <- subset(ASVdropI2m, !is.na(Timepoint)) ### this is the same as using na.rm = T in melt above
ASVdropI2mp <- ASVdropI2mp[order(ASVdropI2mp$Timepoint),]


ggplot(ASVdropI2mp, aes(x = Timepoint, y = reorder(ASV,Timepoint), colour = Microcosm)) + 
  geom_line(aes(group=ASV), color="gray") +
  geom_point() + 
  labs(y="ASVs") +
  theme_classic() 
  # theme(axis.text.y = element_blank()) ## Use this to remove ASV names

# write.csv(ASVdropI2mp, "Figure3a_sharedASVs.csv",quote = F)

##### Dynamics of individual ASVs that drop out at different times, Fig. 3b ####
expff.df <- as.data.frame(expff)

ASV707sub <- expff.df["ASV707"]
ASV707sub <- data.frame(ASV707sub,md.expf[,1:2])
ggplot(ASV707sub, aes(x=Day,y=ASV707,group=Microcosm, color=Microcosm)) +
  geom_line() +
  labs(x = "Days", y = "Frequency of ASV707") +
  geom_point() +
  theme_classic() +
  theme(legend.position = "none")

ASV681sub <- expff.df["ASV681"]
ASV681sub <- data.frame(ASV681sub,md.expf[,1:2])
ggplot(ASV681sub, aes(x=Day,y=ASV681,group=Microcosm, color=Microcosm)) +
  geom_line() +
  labs(x = "Days", y = "Frequency of ASV681") +
  geom_point() +
  theme_classic() +
  theme(legend.position = "none")

ASV284sub <- expff.df["ASV284"]
ASV284sub <- data.frame(ASV284sub,md.expf[,1:2])
ggplot(ASV284sub, aes(x=Day,y=ASV284,group=Microcosm, color=Microcosm)) +
  geom_line() +
  labs(x = "Days", y = "Frequency of ASV284") +
  geom_point() +
  theme_classic() +
  theme(legend.position = "none")

# write.csv(cbind(ASV707sub,ASV681sub,ASV284sub), "Figure3b_ASVdynamics.csv",quote = F)

#### Correlations of initial frequency with time of drop out

expffno0 <- subset(expff, md.expf$Day != 0)
md.expfno0 <- subset(md.expf, row.names(md.expf) %in% row.names(expffno0))

expffExtinct <- matrix(nrow = 764)
for (i in microcosm) {
  #i == "M02"
  expfft2M <- t(subset(expffno0, md.expfno0$Microcosm %in% i))
  expfft2MD1f <- apply(expfft2M,1, function(x) x[!is.na(x)][1]) ## first non-NA frequency
  expffExtinct <- cbind(expffExtinct,expfft2MD1f)
}

expffExtinct2 <- expffExtinct[,2:11]
colnames(expffExtinct2) <- microcosm

ASVdropI3 <- ASVdropI2
ASVdropI3[ASVdropI3 == 22] <- NA
ASVdropI3[ASVdropI3 == 1] <- NA

expffExtinct2sub <- subset(expffExtinct2, row.names(expffExtinct2) %in% row.names(ASVdropI3))
rownames(expffExtinct2sub) == rownames(ASVdropI3)

expffExtinct2m <- melt(as.matrix(expffExtinct2sub), na.rm = T)
colnames(expffExtinct2m) <- c("ASV","Microcosm","Initial_Freq")

ASVdropI3m2 <- melt(as.matrix(ASVdropI3))
colnames(ASVdropI3m2) <- c("ASV","Microcosm","Timepoint")

ASVdropI3m2sub <- subset(ASVdropI3m2, row.names(ASVdropI3m2) %in% row.names(expffExtinct2m))
expffExtinct2m$ASV == ASVdropI3m2sub$ASV
expffExtinct2m$Timepoint <- ASVdropI3m2sub$Timepoint

expffExtinct2m <- subset(expffExtinct2m, !is.na(Timepoint))

cor.test(expffExtinct2m$Initial_Freq,expffExtinct2m$Timepoint)
### Correlation, t = -0.87771, df = 338, p-value = 0.3807, sample estimates: cor -0.04768704

### Optional plots
plot(Initial_Freq ~ Timepoint, data=expffExtinct2m)
with(expffExtinct2m, text(Initial_Freq ~ Timepoint, labels = paste(ASV,Microcosm, sep="-"), pos = 4))



##### Correlating chitinase enzymatic activity in strains and microcosms ######

strain_chitinase = read.table("strain_chitinase_activity.csv", sep = ",", header = TRUE)
colnames(strain_chitinase) = c("ASV_ID", "Strain", "N_Acetyl", "Endochitinase", "Chitobiosidase")

#Average across replicates, remove "strain" information and add in simplified ASV names 
strain_chitinase_avg <-ddply(strain_chitinase,.(ASV_ID),summarize, Endochitinase=mean(Endochitinase), N_Acetyl = mean(N_Acetyl), Chitobiosidase = mean(Chitobiosidase))
ASVconversion2 <- subset(ASVconversion,ASVconversion$ASV_ID %in% strain_chitinase_avg$ASV_ID)
strain_chitinase_avg <- cbind(ASV_ID = ASVconversion2$ASVnames,strain_chitinase_avg[,-1])

mean(strain_chitinase_avg$Endochitinase) #92.65
mean(strain_chitinase_avg$N_Acetyl) #31.23
mean(strain_chitinase_avg$Chitobiosidase) #65.06


## Get ASV names 
strain_asvs = unique(c(as.character(strain_chitinase_avg$ASV_ID)))

## Plot barcharts of activity to look at potential cutoff
Endo = ggplot(strain_chitinase_avg) +
  geom_bar(aes(x = ASV_ID, y = Endochitinase), stat = "identity")+
  geom_hline(yintercept=1, linetype="solid", color = "red")+
  theme(axis.text.x = element_blank()) 
Chito = ggplot(strain_chitinase_avg) +
  geom_bar(aes(x = ASV_ID, y = Chitobiosidase), stat = "identity")+
  geom_hline(yintercept=1, linetype="solid", color = "red")+
  theme(axis.text.x = element_blank()) 
N = ggplot(strain_chitinase_avg) +
  geom_bar(aes(x = ASV_ID, y = N_Acetyl), stat = "identity")+
  geom_hline(yintercept=1, linetype="solid", color = "red")+
  theme(axis.text.x = element_blank()) 

Endo
Chito
N

strain_chitinase_avg$Endo_Production = binarize(strain_chitinase_avg$Endochitinase, threshold=1)
strain_chitinase_avg$Chito_Production = binarize(strain_chitinase_avg$Chitobiosidase, threshold=1)
strain_chitinase_avg$Acteyl_Production = binarize(strain_chitinase_avg$N_Acetyl, threshold=1)

endo_producers = c(as.character(subset(strain_chitinase_avg, Endo_Production==1)$ASV_ID))
chito_producers = c(as.character(subset(strain_chitinase_avg, Chito_Production==1)$ASV_ID))
acetyl_producers = c(as.character(subset(strain_chitinase_avg, Acteyl_Production==1)$ASV_ID))

## Import microcosm-level activity profiling
cosm_activity = read.table("Microcosm_chitinase_activity.csv", sep = ",", header = TRUE)
cosm_activity$SID = paste(cosm_activity$Microcosm, ".D", cosm_activity$Day, sep = "")

## Quick look at the activity in plots
e1 = ggplot(cosm_activity) +
  geom_line(aes(x = Day, y = Endochitinase, group = Microcosm, color = Microcosm)) + labs(title = "Endo") + theme(legend.position = "none")

c1=   ggplot(cosm_activity) +
  geom_line(aes(x = Day, y = Chitobiosidase, group = Microcosm, color = Microcosm)) + labs(title = "Chito") + theme(legend.position = "none")

n1 = ggplot(cosm_activity) +
  geom_line(aes(x = Day, y = X_.N.acetylglucosaminidase, group = Microcosm, color = Microcosm)) + labs(title = "N") + theme(legend.position = "none")

e1
c1
n1

e2 = ggplot(cosm_activity) +
  geom_line(aes(x = Day, y = Endochitinase, group = Microcosm, color = Microcosm)) + 
  facet_grid(~ Microcosm)  +
  theme_minimal() +
  theme(legend.position = "none")

c2 = ggplot(cosm_activity) +
  geom_line(aes(x = Day, y = Chitobiosidase, group = Microcosm, color = Microcosm)) + 
  facet_grid(~ Microcosm)  +
  theme_minimal() +
  theme(legend.position = "none")

n2 = ggplot(cosm_activity) +
  geom_line(aes(x = Day, y = X_.N.acetylglucosaminidase, group = Microcosm, color = Microcosm)) + 
  facet_grid(~ Microcosm)  +
  theme_minimal() +
  theme(legend.position = "none")

e2
c2
n2

# write.csv(cosm_activity, "Figure4e_ChitActivity.csv",quote = F)

#### Subset only to relevant Days to match the chitinase assay
ChitoDays <- c(0,6,12,18,24,30,36,42,48,54,63)
expff.sub <- subset(expff, md.expf$Day %in% ChitoDays)
md.expf.sub <- subset(md.expf, row.names(md.expf) %in% row.names(expff.sub))

plot1list <- list()

for ( i in microcosm ) {
  moc = subset(expff.sub, md.expf.sub$Microcosm == i)
  moc= moc[,colSums(moc) > 0]
  moc_eprod = moc[,intersect(colnames(moc),endo_producers)]
  
  moctu = as.data.frame(t(moc_eprod))
  moctu$ASV = rownames(moctu)
  
  moctu$row <- seq_len(nrow(moctu))
  moctu2 <- melt(moctu, id.vars = "ASV")
  
  colnames(moctu2) = c("ASV_ID", "Sample_ID", "Norm_Freq")
  moctu2$Day = substr(moctu2$Sample_ID, 6,7)
  moctu2$Sample_ID = as.character(moctu2$Sample_ID)
  moctu2$Microcosm = with(md.expf,
                         Microcosm[match(moctu2$Sample_ID,
                                             row.names(md.expf))])
  moctu2$Activity = with(strain_chitinase_avg,
                         Endochitinase[match(moctu2$ASV_ID,
                                             ASV_ID)])
  
  moctu_plot= moctu2[!moctu2$Norm_Freq ==0,]
  moctu_plot = moctu_plot[!moctu_plot$Day == "",] #get rid of artefacts from melting 
  moctu_plot$Day = as.numeric(moctu_plot$Day)
  moctu_plot$ActFreq = moctu_plot$Norm_Freq*(moctu_plot$Activity)
  plot1list[[i]] <- moctu_plot
}

plot1df <- do.call("rbind",plot1list)

plotto1 = ggplot(plot1df) +
  geom_line(aes(x = Day, y = Norm_Freq, group = ASV_ID, color = Activity), stat = "identity") +
  facet_grid(~ Microcosm)  +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_gradient("Activity", low = "lightgray", high = "red")
plotto1

# write.csv(plot1df, "Figure4e_FreqASVactivity.csv",quote = F)

## with legend
plotto1leg = ggplot(plot1df) +
  geom_line(aes(x = Day, y = Norm_Freq, group = ASV_ID, color = Activity), stat = "identity") +
  facet_grid(~ Microcosm)  +
  theme_minimal() +
  scale_color_gradient("Activity", low = "lightgray", high = "red")
plotto1leg

### Frequency by activity
plotto2 = ggplot(plot1df) +
  geom_line(aes(x = Day, y = ActFreq, group = ASV_ID, color = Activity), stat = "identity") +
  facet_grid(~ Microcosm)  +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_gradient("Activity", low = "lightgray", high = "red")
plotto2

# plot1df <- subset(plot1df, !(plot1df$ASV_ID %in% c("ASV018")))
plot1dfsums <- aggregate(plot1df$ActFreq,by=list(plot1df$Day,plot1df$Microcosm),sum)
names(plot1dfsums) <- c("Day","Microcosm","ActFreq")

plot1dfsums5 <- subset(plot1dfsums, plot1dfsums$Microcosm %in% c("M03","M05","M07","M09","M10"))
plot1dfsums5$SID = paste(plot1dfsums5$Microcosm, ".D", plot1dfsums5$Day, sep = "")

cosm_activity5 <- subset(cosm_activity[,c(1,3,5,6)], cosm_activity$Microcosm %in% c("M03","M05","M07","M09","M10"))
cosm_activity5 <- subset(cosm_activity5, cosm_activity5$SID %in% plot1dfsums5$SID)

compcosm <- data.frame(SID=cosm_activity5$SID,Microcosm=cosm_activity5$Microcosm,Day=cosm_activity5$Day,MicrocosmAct=cosm_activity5$Endochitinase,StrainAct=plot1dfsums5$ActFreq)

lmcomp <- lm(log(MicrocosmAct+1) ~ log(StrainAct+1), compcosm)
summary(lmcomp)
hist(residuals(lmcomp))

ggplotcol5 <- c("#A3A500","#00BF7D","#00B0F6","#E76BF3","#FF62BC")

ggplot(compcosm,aes(x = log(StrainAct+1), y = log(MicrocosmAct+1))) +
  geom_point(aes(col=Microcosm)) +
  scale_color_manual(values=ggplotcol5) +
  geom_smooth(method = lm) +
  labs(x = "Cumulative Strain Activity", y = "Microcosm Activity") +
  theme_minimal()

ggplot(compcosm,aes(x = log(StrainAct+1), y = log(MicrocosmAct+1), col=Microcosm)) +
  geom_point() +
  facet_grid(~ Microcosm)  +
  scale_color_manual(values=ggplotcol5) +
  geom_smooth(method = lm) +
  labs(x = "Cumulative Strain Activity", y = "Microcosm Activity") +
  theme_minimal()

# write.csv(compcosm, "SupplementaryFigure6_LMs.csv",quote = F)

lmcompM03 <- lm(log(MicrocosmAct+1) ~ log(StrainAct+1), data=subset(compcosm, compcosm$Microcosm == "M03"))
summary(lmcompM03)

lmcompM05 <- lm(log(MicrocosmAct+1) ~ log(StrainAct+1), data=subset(compcosm, compcosm$Microcosm == "M05"))
summary(lmcompM05)

lmcompM07 <- lm(log(MicrocosmAct+1) ~ log(StrainAct+1), data=subset(compcosm, compcosm$Microcosm == "M07"))
summary(lmcompM07)

lmcompM09 <- lm(log(MicrocosmAct+1) ~ log(StrainAct+1), data=subset(compcosm, compcosm$Microcosm == "M09"))
summary(lmcompM09)

lmcompM10 <- lm(log(MicrocosmAct+1) ~ log(StrainAct+1), data=subset(compcosm, compcosm$Microcosm == "M10"))
summary(lmcompM10)

##### Fraction of 5 cultured microcosms that is cultured strains ####
expff.sub5 <- subset(expff.sub, md.expf.sub$Microcosm %in% c("M03","M05","M07","M09","M10"))
md.expf.sub5 <- subset(md.expf.sub, row.names(md.expf.sub) %in% row.names(expff.sub5))
expff.sub5D63 <- subset(expff.sub5, md.expf.sub5$Day == 63)
md.expf.sub5D63 <- subset(md.expf.sub5, row.names(md.expf.sub5) %in% row.names(expff.sub5D63))

expff.sub5D63 <- expff.sub5D63[,colSums(expff.sub5D63) > 0]
summary(colSums(expff.sub5D63))
summary(rowSums(expff.sub5D63))

strain_asvs

expff.sub5D63cult <- expff.sub5D63[,colnames(expff.sub5D63) %in% strain_asvs]
rowSums(expff.sub5D63cult)
####   M03.D63   M05.D63   M07.D63   M09.D63   M10.D63 
####  0.8849193 0.6704296 0.6944522 0.8652121 0.8201349 


### Plots of MicroResp CO2 data ####

co2reps <- read.csv("MicroResp_initial_replicates.csv",header = T)

ggplot(co2reps, aes(sample, Percent_CO2, group = sample, color= sample)) + 
  geom_point(size = 2) +
  geom_line() +
  theme_minimal() 

co2dat <- read.csv("MicroResp_data_norm1_percentCO2.csv",header = T)
co2dat1 <- melt(co2dat,id="Day")
co2dat2 <- co2dat1
colnames(co2dat2) <- c("Day","Microcosm","Percent_CO2")

CO2_Variance <- aggregate(Percent_CO2~Day, co2dat2, var)

ggplot(CO2_Variance, aes(Day, Percent_CO2)) + 
  geom_line() +
  geom_point(size = 2) +
  ylab("CO2 Variance") +
  theme_minimal() 

# write.csv(CO2_Variance, "Figure4a_CO2var.csv",quote = F)

######### Heatmap of Ecoplate info #####
### Collapse by microcosm to get means for each post-stabilization

ecofxn2 <- subset(ecofxn, md.expffxn$Day %in% c("27","36","45","54","63")) 
md.expffxn2 <- subset(md.expffxn, row.names(md.expffxn) %in% row.names(ecofxn2)) 

name2 <- c(substr(rownames(ecofxn2),1,3))
md.expffxn2 <- cbind(md.expffxn2,name2)

ecofxn2.Mmeans <- aggregate(ecofxn2, by=list(md.expffxn2$name2), mean)
row.names(ecofxn2.Mmeans) <- ecofxn2.Mmeans[,1]
ecofxn2.Mmeans.matrix <- as.matrix(ecofxn2.Mmeans[,2:32])

mycols <- colorRampPalette(colors = c("black","red"))(length(ecofxn2.Mmeans.matrix))
heatmap(t(ecofxn2.Mmeans.matrix), scale = "none", cexRow = 0.8, cexCol = 0.8, col = mycols)
levelplot(ecofxn2.Mmeans.matrix, col.regions=mycols) ## can just use this for the legend

# write.csv(ecofxn2.Mmeans.matrix, "SupplementaryFigure5_heatmap.csv",quote = F)

##### EcoPlate heatmap for Strains #####
ecostr <- read.csv("ecoplate_strain_all_data.csv",header = T, check.names = F)
summary(ecostr$Microcosm)

## Subset to microcosms where strains were cultured
cult <- c("M03","M05","M07","M09","M10")

ecostr.cult <- subset(ecostr, ecostr$Microcosm %in% cult)
ecostr.cult$Microcosm <- ecostr.cult$Microcosm[drop=T]
summary(ecostr.cult$Microcosm)

Mnames <- paste(ecostr.cult$Microcosm,ecostr.cult$Strain, sep = "_")
ecostr.cultm <- as.matrix(ecostr.cult[,4:34])
row.names(ecostr.cultm) <- Mnames
ecostr.cultmo <- ecostr.cultm[order(row.names(ecostr.cultm)),]

## plot heatmap
mycols <- colorRampPalette(colors = c("black","red"))(length(ecostr.cultmo))
heatmap(t(ecostr.cultmo), Rowv = NA, Colv = NA, scale = "none", cexRow = 0.8, cexCol = 0.8, col = mycols)
levelplot(ecostr.cultmo, col.regions=mycols) ## just for the legend

# write.csv(ecostr.cultmo, "SupplementaryFigure8_heatmap.csv",quote = F)

### Compare with Day 63 (when strains were cultured) for Microcosms
ecofxn63.2 <- ecofxn63
rownames(ecofxn63.2) <- microcosm
ecofxn63.sub <- subset(ecofxn63.2, rownames(ecofxn63.2) %in% cult)
heatmap(t(ecofxn63.sub), Rowv = NA, Colv = NA, scale = "none", cexRow = 0.8, cexCol = 0.8, col = mycols)


#### Ternary Plot of Strain Enzyme Assays ####
datass <- read.csv("Strain_enzyme_assay_data.csv")

## look at the data in histograms
hist(datass$Lipase)
hist(datass$Protease)
hist(datass$Endochitinase)

datass2 <- aggregate(datass[,3:5], by=list(Category=datass$ASV_ID), FUN=mean)
ASVconversion3 <- subset(ASVconversion,ASVconversion$ASV_ID %in% datass2$Category)
datass3 <- datass2[,2:4]
rownames(datass3) <- ASVconversion3$ASVnames

datass3[datass3 < 0.04] <- 0 ## Only look at the higher levels of enzymes

datass3 <- datass3[rowSums(datass3) > 0,]
datass3$Lipase <- datass3$Lipase[drop=T]
datass3$Protease <- datass3$Protease[drop=T]
datass3$Endochitinase <- datass3$Endochitinase[drop=T]


## Use n equally spaced breaks to assign each value to n-1 equal sized bins 
ii <- cut(apply(datass3,1,max), breaks = seq(min(apply(datass3,1,max)), max(apply(datass3,1,max)), len = 57), 
          include.lowest = TRUE)

## Use bin indices, ii, to select color from vector of n-1 equally spaced colors
colors <- colorRampPalette(c("lightgray", "red"))(57)[ii]

## Create ternary plot with labels
ternplot <- ggtern(datass3,aes(Lipase,Protease,Endochitinase)) +
  geom_point(color=colors) +
  theme_nomask() + #Allow Labels to Spool Over Edges
  theme_legend_position('topleft') +
  theme_showarrows()+ 
  geom_text(aes(label=row.names(datass3)),size=3)
ternplot

# write.csv(datass3, "SupplementaryFigure7_ternary.csv",quote = F)

## Make a legend (will have to be re-organized)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", # position
       legend = ii, 
       title = "Legend",
       fill = colors,
       cex = 1,
       bty = "n")

