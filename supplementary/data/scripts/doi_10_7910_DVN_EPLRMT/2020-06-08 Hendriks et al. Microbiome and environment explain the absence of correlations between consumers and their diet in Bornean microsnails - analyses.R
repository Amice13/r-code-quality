### R scipt to perform analyses with Hendriks et al. (2020) Microbiome and environment explain the absence of correlations between
### consumers and their diet in Bornean microsnails, Ecology.

#This script uses packrat, a tool to save and run R packages alongside the script, to mitigate problems with version updates.
library(packrat)
packrat::init()

#One-time installation of packages in packrat (installs all dependencies, too).
# source("https://raw.githubusercontent.com/joey711/phyloseq/master/inst/scripts/installer.R",local = TRUE)
# install.packages("glmmTMB")
# install.packages("lme4")
# install.packages("plspm")
# install.packages("ggplot2")
# install.packages("reshape")
# install.packages("cowplot")
# install.packages("ggplotify")
# install.packages("cowplot")
# install.packages("picante")
# install.packages("fitdistrplus")
# install.packages("emmeans")


packrat::snapshot()
packrat::restore()

#The function status can also be used to clean up unused packages. Packrat checks the code for functions.
packrat::status()

#Clear the environment & load relevant packages.
#rm(list=ls())
library(phyloseq)
library(vegan)
library(picante)
library(ggplot2)
library(cowplot)
library(fitdistrplus)
library(glmmTMB)
library(emmeans)
library(plspm)
library(reshape2)
library(ggpubr)
library(dplyr)


#### STEP 1: LOAD, CLEAN, & SUBSET THE DATA ####
  
  #We  first load all metabarcoding and concensus data as previously arrived at by the authors.
  #Raw files can be downloaded from https://www.ncbi.nlm.nih.gov/sra/PRJNA530120 and need bioinformatics as
  #described in Supplemetary Methods S3. 
  
  #A good way to start is to simpy load all author results.
  #load(file="saved_R_objects/Hendriks_et_al_2020.rda")
  #Many of the below analyses take a long time to run, due to randomizations etc.
  #Furthermore, some analyses are based on randomizations and chance events, so slight deviations in the outcome may occur when re-running such code.
  #Now one can run the code below one by one and check if results correspond to the author results as should.

  #Load metabarcoding data.
  physeq_raw<-readRDS("input_data/physeq_raw.RDS")
  
  #REMOVE? metabarcoding_diet_data<-physeq_raw$rbcL
  #REMOVE? saveRDS(metabarcoding_diet_data, file="metabarcoding_diet_data.RDS")
  
  #Remove samples from locations and plots we won't consider in current study.
  physeq_metabarcoding<-lapply(physeq_raw, function(x) subset_samples(x, !LOCATION %in% c("HappyGarden", "unknown", "control", "empty", "contaminated","TomanggongBesar") &
                                                              !PLOT %in% c("5_outside", "2_outside")))
  
  #In the rbcL diet study, Pool_3 was sequenced twice on Illumina; we will prune out the data from the first run for this pool.
  physeq_metabarcoding$rbcL<-subset_samples(physeq_metabarcoding$rbcL, SOURCE_POOL_rbcL != "rbcL_3")
  
  #From the 16S microbiome dataset, remove all reads with a possible chloroplast or mitochondrial origin.
  physeq_metabarcoding$`16S`<-prune_taxa(as.vector(physeq_metabarcoding$`16S`@tax_table[,3]!="Chloroplast" & physeq_metabarcoding$`16S`@tax_table[,5]!="mitochondria"),physeq_metabarcoding$`16S`)
  
  #Any taxa (rbcL, or 16S) found from negative controls included in the lab procedures need to be excluded from all samples:
  #we consider these taxa to be likely of environmental contamination origin.
  control_taxa_rbcL<-prune_samples(substr(sample_names(physeq_raw$rbcL),1,6)=="negcon",physeq_raw$rbcL)
  control_taxa_rbcL<-rownames(otu_table(prune_taxa(rowSums(control_taxa_rbcL@otu_table)>0,control_taxa_rbcL)))
  control_taxa_16S<-prune_samples(substr(sample_names(physeq_raw$`16S`),1,6)=="negcon",physeq_raw$`16S`)
  control_taxa_16S<-rownames(otu_table(prune_taxa(rowSums(control_taxa_16S@otu_table)>0,control_taxa_16S)))
  
  #Then prune these taxa from our raw datasets for diet and microbiome.
  physeq_metabarcoding$rbcL<-prune_taxa(!(taxa_names(physeq_metabarcoding$rbcL) %in% control_taxa_rbcL),physeq_metabarcoding$rbcL)
  physeq_metabarcoding$`16S`<-prune_taxa(!(taxa_names(physeq_metabarcoding$`16S`) %in% control_taxa_16S),physeq_metabarcoding$`16S`)
  rm(control_taxa_rbcL,control_taxa_16S)  
  
  #Remove samples with zero data left.
  physeq_metabarcoding<-lapply(physeq_metabarcoding, function(x) prune_samples(sample_sums(x)>0,x))
  
  #And we need to rename samples with suffix "b" (indicating the Illumina run "rbcL_3b"), so they can be connected to 16S data next.
  sample_names(physeq_metabarcoding$rbcL)<-substr(sample_names(physeq_metabarcoding$rbcL),1,11)
  
  #Let's extract PhyloSeq objects.
  microbiome.ph<-physeq_metabarcoding$`16S`
  diet.ph<-physeq_metabarcoding$rbcL
  
  #And we load data for the snail communities from both shells (2015+2016) and live samples (2017).
  community.shells.ph<-readRDS("input_data/physeq_snails_shells.RDS")
  community.live.ph<-readRDS("input_data/physeq_snails_live.RDS")
  
  #Community shell data from plots Batangan_5 and _6 may have been swapped during fieldwork. 
  #Therefore data has been pooled and we now take these data to represent the community in each of these plots.
  phyloseq_Batangan_6<-prune_samples(sample_names(community.shells.ph)=="Batangan_5",community.shells.ph)
  sample_names(phyloseq_Batangan_6)<-"Batangan_6"
  community.shells.ph<-merge_phyloseq(community.shells.ph,phyloseq_Batangan_6)
  rm(phyloseq_Batangan_6)
  
  #Load environmental data.
  environment<-read.csv("input_data/environmental_data.csv", header = TRUE)


#### STEP 2: CALCULATE SUMMARY STATISTICS FROM THESE DATA AND COLLECT IN DATAFRAME ####

  #Use PhyloSeq function to calculate richness and diversity metrics for these data.
  microbiome.ss<-data.frame(reshape(aggregate(value~samples+variable,plot_richness(microbiome.ph,measures = c("Observed","Chao1","Shannon","Simpson","InvSimpson"))$data,mean),idvar = "samples",timevar = "variable",direction = "wide"))
  diet.ss<-data.frame(reshape(aggregate(value~samples+variable,plot_richness(diet.ph,measures = c("Observed","Chao1","Shannon","Simpson","InvSimpson"))$data,mean),idvar = "samples",timevar = "variable",direction = "wide"))
  community.shells.ss<-data.frame(reshape(aggregate(value~samples+variable,plot_richness(community.shells.ph,measures = c("Observed","Chao1","Shannon","Simpson","InvSimpson"))$data,mean),idvar = "samples",timevar = "variable",direction = "wide"))
  community.live.ss<-data.frame(reshape(aggregate(value~samples+variable,plot_richness(community.live.ph,measures = c("Observed","Chao1","Shannon","Simpson","InvSimpson"))$data,mean),idvar = "samples",timevar = "variable",direction = "wide"))
  
  #Add Shannon evenness, which also is not by default in PhyloSeq, but defined as 1/D/ln(S) (Magurran & McGill, 2011, p 57).
  microbiome.ss$ShannonEvenness<-1/ifelse(microbiome.ss$value.Shannon==0,NA,microbiome.ss$value.Shannon)/log(microbiome.ss$value.Observed)
  diet.ss$ShannonEvenness<-1/ifelse(diet.ss$value.Shannon==0,NA,diet.ss$value.Shannon)/log(diet.ss$value.Observed)
  community.shells.ss$ShannonEvenness<-1/ifelse(community.shells.ss$value.Shannon==0,NA,community.shells.ss$value.Shannon)/log(community.shells.ss$value.Observed)
  community.live.ss$ShannonEvenness<-1/ifelse(community.live.ss$value.Shannon==0,NA,community.live.ss$value.Shannon)/log(community.live.ss$value.Observed)
  
  #Add Faith's PD, which is not by default included in a PhyloSeq function, but can be calculted using the picante package.
  microbiome.ss<-merge.data.frame(microbiome.ss,picante::pd(samp = t(microbiome.ph@otu_table@.Data), tree = microbiome.ph@phy_tree, include.root = FALSE), by.x = 1, by.y = 0)
  diet.ss<-merge.data.frame(diet.ss,picante::pd(samp = t(diet.ph@otu_table@.Data), tree = diet.ph@phy_tree, include.root = FALSE), by.x = 1, by.y = 0)
  community.shells.ss<-merge.data.frame(community.shells.ss,picante::pd(samp = t(community.shells.ph@otu_table@.Data), tree = community.shells.ph@phy_tree, include.root = FALSE), by.x = 1, by.y = 0)
  community.live.ss<-merge.data.frame(community.live.ss,picante::pd(samp = t(community.live.ph@otu_table@.Data), tree = community.live.ph@phy_tree, include.root = FALSE), by.x = 1, by.y = 0)
  
  #Gather these results in a single data frame. Use merging to combine data from different data frames.
  data<-merge.data.frame(microbiome.ss,diet.ss,by = 1, suffixes = c(".micr",".diet"), all = T)
  data<-merge.data.frame(data.frame(physeq_raw$rbcL@sam_data[,c(1,10:13)]),data,by.x = 0, by.y = 1)
  data<-merge.data.frame(data,community.shells.ss, by.x=6,by.y=1)
  data<-merge.data.frame(data,community.live.ss, by=1, suffixes = c(".comm.shells",".comm.live"))
  colnames(data)<-ifelse(substr(colnames(data),1,6)=="value.",substr(colnames(data),7,100),colnames(data))
  colnames(data)[2]<-"SAMPLE_ID"
  data<-merge.data.frame(data,environment[,c(1,9:18)], by = 1)
  data<-data[,c(3,2,4:6,1,7:ncol(data))]
  
  #And save a copy of the pbject data.
  saveRDS(data, file="saved_R_objects/data.RDS")
  saveRDS(microbiome.ph, file="saved_R_objects/microbiome.pd.RDS")
  saveRDS(diet.ph, file="saved_R_objects/diet.ph.RDS")


#### STEP 3: ORDINATION OF COMMUNITY DATASETS AND PERAMOVA TO TEST FOR CORRELATIONS ####

  #First We ordinate data merged by species and plot to visually investigate differences between species and locations.
  #We do so for Bray-Curtis, Jaccard, Weighted UniFrac, and Unweighted UniFrac distance measures.
  
  #For the diet data.
  #First we add a sample data column and define the target vs. non-target species to be plotted.
  diet.ph@sam_data$ordination<-diet.ph@sam_data$GENUS_SPECIES
  diet.ph@sam_data$ordination[!(diet.ph@sam_data$ordination %in% c("A_jagori","G_similis","P_concinnum"))]<-"non-target_species"
  #Focus on three target species plus the pool of non-target species, for which we have enough data.
  diet_byPlot.ph<-prune_samples(diet.ph@sam_data$ordination %in% c("A_jagori","G_similis","P_concinnum","non-target_species"), diet.ph)
  #Merge samples by plot and species.
  diet_byPlot.ph@sam_data$LOCATION_PLOT_GENUS_SPECIES<-paste0(diet_byPlot.ph@sam_data$LOCATION_PLOT,"_",diet_byPlot.ph@sam_data$ordination)
  diet_byPlot.ph<-phyloseq::merge_samples(diet_byPlot.ph, group=diet_byPlot.ph@sam_data$LOCATION_PLOT_GENUS_SPECIES, fun = sum)
  #Remove singleton samples (there are three in the diet data).
  diet_byPlot.ph<-prune_samples(diet_byPlot.ph@sam_data$COMMENTS>1, diet_byPlot.ph)
  #And update sample data in PhyloSeq objects.
  diet_byPlot.ph@sam_data$GENUS_SPECIES<-paste0(sapply(strsplit(sample_names(diet_byPlot.ph),"_"),'[',3),"_",sapply(strsplit(sample_names(diet_byPlot.ph),"_"),'[',4))
  diet_byPlot.ph@sam_data$LOCATION<-sapply(strsplit(sample_names(diet_byPlot.ph),"_"),'[',1)
  diet_byPlot.ph@sam_data$PLOT<-sapply(strsplit(sample_names(diet_byPlot.ph),"_"),'[',2)
  #Prune data to exclude taxa with zero data left.
  diet_byPlot.ph<-prune_taxa(colSums(diet_byPlot.ph@otu_table@.Data)>0, diet_byPlot.ph)
  #Use the PhyloSeq wrapper function for ordination.
  #Note: it may be necessary to initiate the line below multiple times before reaching a converged solution.
  diet_byPlot_Bray.ord<-ordinate(diet_byPlot.ph, method = "NMDS", distance = "bray")
  #And create the ordination plot.
  (diet_byPlot_Bray.plot<-plot_ordination(diet_byPlot.ph, diet_byPlot_Bray.ord, type="samples", color="GENUS_SPECIES")+
      geom_point(color="white", size=3)+
      stat_ellipse(level=0.95,aes(lty=GENUS_SPECIES))+
      geom_point(aes(shape=LOCATION), size=3)+
      geom_text(aes(label=PLOT), hjust=-1, check_overlap = TRUE)+
      scale_color_manual(values=c("#C4961A", "#00AFBB", "darkgrey","#FC4E07"))+
      scale_x_continuous(breaks = round(seq(-.8, 0.5, by = 0.2),1))+
      scale_y_continuous(breaks = round(seq(-.5, 0.5, by = 0.1),1))+
      labs(subtitle=paste0("Diet Bray-Curtis (stress: ",round(diet_byPlot_Bray.ord$stress,2),")"),  shape="Location", color="Species", lty="Species")+
      theme(plot.subtitle=element_text(size=12, hjust=0.1))+
      theme_bw(base_size = 15))
  
  #Again, now for Jaccard distance.
  diet_byPlot_Jaccard.ord<-ordinate(diet_byPlot.ph, method = "NMDS", distance = "jaccard")
  #And create the ordination plot.
  (diet_byPlot_Jaccard.plot<-plot_ordination(diet_byPlot.ph, diet_byPlot_Jaccard.ord, type="samples", color="GENUS_SPECIES")+
      geom_point(color="white", size=3)+
      stat_ellipse(level=0.95,aes(lty=GENUS_SPECIES))+
      geom_point(aes(shape=LOCATION), size=3)+
      geom_text(aes(label=PLOT), hjust=-1, check_overlap = TRUE)+
      scale_color_manual(values=c("#C4961A", "#00AFBB", "darkgrey","#FC4E07"))+
      scale_x_continuous(breaks = round(seq(-.8, 0.5, by = 0.2),1))+
      scale_y_continuous(breaks = round(seq(-.5, 0.5, by = 0.1),1))+
      labs(subtitle=paste0("Diet Jaccard (stress: ",round(diet_byPlot_Jaccard.ord$stress,2),")"),  shape="Location", color="Species", lty="Species")+
      theme(plot.subtitle=element_text(size=12, hjust=0.1))+
      theme_bw(base_size = 15))
  
  #Again, now for Weighted UniFrac distance.
  diet_byPlot_WeightedUniFrac.ord<-ordinate(diet_byPlot.ph, method = "NMDS", distance = "wunifrac")
  #And create the ordination plot.
  (diet_byPlot_WeightedUniFrac.plot<-plot_ordination(diet_byPlot.ph, diet_byPlot_WeightedUniFrac.ord, type="samples", color="GENUS_SPECIES")+
      geom_point(color="white", size=3)+
      stat_ellipse(level=0.95,aes(lty=GENUS_SPECIES))+
      geom_point(aes(shape=LOCATION), size=3)+
      geom_text(aes(label=PLOT), hjust=-1, check_overlap = TRUE)+
      scale_color_manual(values=c("#C4961A", "#00AFBB", "darkgrey","#FC4E07"))+
      scale_x_continuous(breaks = round(seq(-.8, 0.5, by = 0.2),1))+
      scale_y_continuous(breaks = round(seq(-.5, 0.5, by = 0.1),1))+
      labs(subtitle=paste0("Diet Weighted UniFrac (stress: ",round(diet_byPlot_WeightedUniFrac.ord$stress,2),")"),  shape="Location", color="Species", lty="Species")+
      theme(plot.subtitle=element_text(size=12, hjust=0.1))+
      theme_bw(base_size = 15))
  
  #Again, now for Unweighted UniFrac distance.
  diet_byPlot_UniFrac.ord<-ordinate(diet_byPlot.ph, method = "NMDS", distance = "unifrac")
  #And create the ordination plot.
  (diet_byPlot_UniFrac.plot<-plot_ordination(diet_byPlot.ph, diet_byPlot_UniFrac.ord, type="samples", color="GENUS_SPECIES")+
      geom_point(color="white", size=3)+
      stat_ellipse(level=0.95,aes(lty=GENUS_SPECIES))+
      geom_point(aes(shape=LOCATION), size=3)+
      geom_text(aes(label=PLOT), hjust=-1, check_overlap = TRUE)+
      scale_color_manual(values=c("#C4961A", "#00AFBB", "darkgrey","#FC4E07"))+
      scale_x_continuous(breaks = round(seq(-.8, 0.5, by = 0.2),1))+
      scale_y_continuous(breaks = round(seq(-.5, 0.5, by = 0.1),1))+
      labs(subtitle=paste0("Diet Unweighted UniFrac (stress: ",round(diet_byPlot_UniFrac.ord$stress,2),")"),  shape="Location", color="Species", lty="Species")+
      theme(plot.subtitle=element_text(size=12, hjust=0.1))+
      theme_bw(base_size = 15))
  
  
  #For the microbiome data.
  #First we add a sample data column and define the target vs. non-target species to be plotted.
  microbiome.ph@sam_data$ordination<-microbiome.ph@sam_data$GENUS_SPECIES
  microbiome.ph@sam_data$ordination[!(microbiome.ph@sam_data$ordination %in% c("A_jagori","G_similis","P_concinnum"))]<-"non-target_species"
  #Focus on three target species vs. non-target species as a single group, for which we have enough data.
  microbiome_byPlot.ph<-prune_samples(microbiome.ph@sam_data$ordination %in% c("A_jagori","G_similis","P_concinnum","non-target_species"), microbiome.ph)
  #Merge samples by plot and species.
  microbiome_byPlot.ph@sam_data$LOCATION_PLOT_GENUS_SPECIES<-paste0(microbiome_byPlot.ph@sam_data$LOCATION_PLOT,"_",microbiome_byPlot.ph@sam_data$ordination)
  microbiome_byPlot.ph<-phyloseq::merge_samples(microbiome_byPlot.ph, group=microbiome_byPlot.ph@sam_data$LOCATION_PLOT_GENUS_SPECIES, fun = sum)
  #Remove singleton samples (there are three in the microbiome data).
  microbiome_byPlot.ph<-prune_samples(microbiome_byPlot.ph@sam_data$COMMENTS>1, microbiome_byPlot.ph)
  #And update sample data in PhyloSeq objects.
  microbiome_byPlot.ph@sam_data$GENUS_SPECIES<-paste0(sapply(strsplit(sample_names(microbiome_byPlot.ph),"_"),'[',3),"_",sapply(strsplit(sample_names(microbiome_byPlot.ph),"_"),'[',4))
  microbiome_byPlot.ph@sam_data$LOCATION<-sapply(strsplit(sample_names(microbiome_byPlot.ph),"_"),'[',1)
  microbiome_byPlot.ph@sam_data$PLOT<-sapply(strsplit(sample_names(microbiome_byPlot.ph),"_"),'[',2)
  #Prune data to exclude taxa with zero data left.
  microbiome_byPlot.ph<-prune_taxa(colSums(microbiome_byPlot.ph@otu_table@.Data)>0, microbiome_byPlot.ph)
  #Use the PhyloSeq wrapper function for ordination.
  #Note: it may be necessary to initiate the line below multiple times before reaching a converged solution.
  microbiome_byPlot_Bray.ord<-ordinate(microbiome_byPlot.ph, method = "NMDS", distance = "bray")
  #And create the ordination plot.
  (microbiome_byPlot_Bray.plot<-plot_ordination(microbiome_byPlot.ph, microbiome_byPlot_Bray.ord, type="samples", color="GENUS_SPECIES")+
    geom_point(color="white", size=3)+
    stat_ellipse(level=0.95,aes(lty=GENUS_SPECIES))+
    geom_point(aes(shape=LOCATION), size=3)+
    geom_text(aes(label=PLOT), hjust=-1, check_overlap = TRUE)+
    scale_color_manual(values=c("#C4961A", "#00AFBB", "darkgrey","#FC4E07"))+
    scale_x_continuous(breaks = round(seq(-.8, 0.5, by = 0.2),1))+
    scale_y_continuous(breaks = round(seq(-.5, 0.6, by = 0.1),1))+
    labs(subtitle=paste0("Microbiome Bray-Curtis (stress: ",round(microbiome_byPlot_Bray.ord$stress,2),")"),  shape="Location", color="Species", lty="Species")+
    theme(plot.subtitle=element_text(size=12, hjust=0.1))+
    theme_bw(base_size = 15))
  
  #Again, now for Jaccard distance.
  microbiome_byPlot_Jaccard.ord<-ordinate(microbiome_byPlot.ph, method = "NMDS", distance = "jaccard")
  #And create the ordination plot.
  (microbiome_byPlot_Jaccard.plot<-plot_ordination(microbiome_byPlot.ph, microbiome_byPlot_Jaccard.ord, type="samples", color="GENUS_SPECIES")+
      geom_point(color="white", size=3)+
      stat_ellipse(level=0.95,aes(lty=GENUS_SPECIES))+
      geom_point(aes(shape=LOCATION), size=3)+
      geom_text(aes(label=PLOT), hjust=-1, check_overlap = TRUE)+
      scale_color_manual(values=c("#C4961A", "#00AFBB", "darkgrey","#FC4E07"))+
      scale_x_continuous(breaks = round(seq(-.8, 0.5, by = 0.2),1))+
      scale_y_continuous(breaks = round(seq(-.5, 0.5, by = 0.1),1))+
      labs(subtitle=paste0("Microbiome Jaccard (stress: ",round(microbiome_byPlot_Jaccard.ord$stress,2),")"),  shape="Location", color="Species", lty="Species")+
      theme(plot.subtitle=element_text(size=12, hjust=0.1))+
      theme_bw(base_size = 15))
  
  #Again, now for Weighted UniFrac distance.
  microbiome_byPlot_WeightedUniFrac.ord<-ordinate(microbiome_byPlot.ph, method = "NMDS", distance = "wunifrac")
  #And create the ordination plot.
  (microbiome_byPlot_WeightedUniFrac.plot<-plot_ordination(microbiome_byPlot.ph, microbiome_byPlot_WeightedUniFrac.ord, type="samples", color="GENUS_SPECIES")+
      geom_point(color="white", size=3)+
      stat_ellipse(level=0.95,aes(lty=GENUS_SPECIES))+
      geom_point(aes(shape=LOCATION), size=3)+
      geom_text(aes(label=PLOT), hjust=-1, check_overlap = TRUE)+
      scale_color_manual(values=c("#C4961A", "#00AFBB", "darkgrey","#FC4E07"))+
      scale_x_continuous(breaks = round(seq(-.8, 0.5, by = 0.2),1))+
      scale_y_continuous(breaks = round(seq(-.5, 0.5, by = 0.1),1))+
      labs(subtitle=paste0("Microbiome Weighted UniFrac (stress: ",round(microbiome_byPlot_WeightedUniFrac.ord$stress,2),")"),  shape="Location", color="Species", lty="Species")+
      theme(plot.subtitle=element_text(size=12, hjust=0.1))+
      theme_bw(base_size = 15))
  
  #Again, now for Unweighted UniFrac distance.
  microbiome_byPlot_UniFrac.ord<-ordinate(microbiome_byPlot.ph, method = "NMDS", distance = "unifrac")
  #And create the ordination plot.
  (microbiome_byPlot_UniFrac.plot<-plot_ordination(microbiome_byPlot.ph, microbiome_byPlot_UniFrac.ord, type="samples", color="GENUS_SPECIES")+
      geom_point(color="white", size=3)+
      stat_ellipse(level=0.95,aes(lty=GENUS_SPECIES))+
      geom_point(aes(shape=LOCATION), size=3)+
      geom_text(aes(label=PLOT), hjust=-1, check_overlap = TRUE)+
      scale_color_manual(values=c("#C4961A", "#00AFBB", "darkgrey","#FC4E07"))+
      scale_x_continuous(breaks = round(seq(-.8, 0.5, by = 0.2),1))+
      scale_y_continuous(breaks = round(seq(-.5, 0.5, by = 0.1),1))+
      labs(subtitle=paste0("Microbiome Unweighted UniFrac (stress: ",round(microbiome_byPlot_UniFrac.ord$stress,2),")"),  shape="Location", color="Species", lty="Species")+
      theme(plot.subtitle=element_text(size=12, hjust=0.1))+
      theme_bw(base_size = 15))
  
  #Second, we statistically test for regional differences in between target species and among locations using PERMANOVA.
  
  #We study again the different distance measures, and use the "target species" as grouping variable in the test.
  (diet_byPlot_permanovaBray<-adonis(distance(diet_byPlot.ph, method = "bray", type="samples")~GENUS_SPECIES*LOCATION, data=data.frame(sample_data(diet_byPlot.ph)), permutations = 4999))
  (diet_byPlot_betadisperBray_species<-permutest(betadisper((distance(diet_byPlot.ph, method = "bray", type="samples")), group=diet_byPlot.ph@sam_data$GENUS_SPECIES), permutations = 4999))
  (diet_byPlot_betadisperBray_location<-permutest(betadisper((distance(diet_byPlot.ph, method = "bray", type="samples")), group=diet_byPlot.ph@sam_data$LOCATION), permutations = 4999))
  (microbiome_byPlot_permanovaBray<-adonis(distance(microbiome_byPlot.ph, method = "bray", type="samples")~GENUS_SPECIES*LOCATION, data=data.frame(sample_data(microbiome_byPlot.ph)), permutations = 4999))
  (microbiome_byPlot_betadisperBray_species<-permutest(betadisper((distance(microbiome_byPlot.ph, method = "bray", type="samples")), group=microbiome_byPlot.ph@sam_data$GENUS_SPECIES), permutations = 4999))
  (microbiome_byPlot_betadisperBray_location<-permutest(betadisper((distance(microbiome_byPlot.ph, method = "bray", type="samples")), group=microbiome_byPlot.ph@sam_data$LOCATION), permutations = 4999))
  
  (diet_byPlot_permanovaJaccard<-adonis(distance(diet_byPlot.ph, method = "jaccard", type="samples")~GENUS_SPECIES*LOCATION, data=data.frame(sample_data(diet_byPlot.ph)), permutations = 4999))
  (diet_byPlot_betadisperJaccard_species<-permutest(betadisper((distance(diet_byPlot.ph, method = "jaccard", type="samples")), group=diet_byPlot.ph@sam_data$GENUS_SPECIES), permutations = 4999))
  (diet_byPlot_betadisperJaccard_location<-permutest(betadisper((distance(diet_byPlot.ph, method = "jaccard", type="samples")), group=diet_byPlot.ph@sam_data$LOCATION), permutations = 4999))
  (microbiome_byPlot_permanovaJaccard<-adonis(distance(microbiome_byPlot.ph, method = "jaccard", type="samples")~GENUS_SPECIES*LOCATION, data=data.frame(sample_data(microbiome_byPlot.ph)), permutations = 4999))
  (microbiome_byPlot_betadisperJaccard_species<-permutest(betadisper((distance(microbiome_byPlot.ph, method = "jaccard", type="samples")), group=microbiome_byPlot.ph@sam_data$GENUS_SPECIES), permutations = 4999))
  (microbiome_byPlot_betadisperJaccard_location<-permutest(betadisper((distance(microbiome_byPlot.ph, method = "jaccard", type="samples")), group=microbiome_byPlot.ph@sam_data$LOCATION), permutations = 4999))
  
  (diet_byPlot_permanovaWeightedUniFrac<-adonis(distance(diet_byPlot.ph, method = "wunifrac", type="samples")~GENUS_SPECIES*LOCATION, data=data.frame(sample_data(diet_byPlot.ph)), permutations = 4999))
  (diet_byPlot_betadisperWeightedUniFrac_species<-permutest(betadisper((distance(diet_byPlot.ph, method = "wunifrac", type="samples")), group=diet_byPlot.ph@sam_data$GENUS_SPECIES), permutations = 4999))
  (diet_byPlot_betadisperWeightedUniFrac_location<-permutest(betadisper((distance(diet_byPlot.ph, method = "wunifrac", type="samples")), group=diet_byPlot.ph@sam_data$LOCATION), permutations = 4999))
  (microbiome_byPlot_permanovaWeightedUniFrac<-adonis(distance(microbiome_byPlot.ph, method = "wunifrac", type="samples")~GENUS_SPECIES*LOCATION, data=data.frame(sample_data(microbiome_byPlot.ph)), permutations = 4999))
  (microbiome_byPlot_betadisperWeightedUniFrac_species<-permutest(betadisper((distance(microbiome_byPlot.ph, method = "wunifrac", type="samples")), group=microbiome_byPlot.ph@sam_data$GENUS_SPECIES), permutations = 4999))
  (microbiome_byPlot_betadisperWeightedUniFrac_location<-permutest(betadisper((distance(microbiome_byPlot.ph, method = "wunifrac", type="samples")), group=microbiome_byPlot.ph@sam_data$LOCATION), permutations = 4999))
  
  (diet_byPlot_permanovaUniFrac<-adonis(distance(diet_byPlot.ph, method = "unifrac", type="samples")~GENUS_SPECIES*LOCATION, data=data.frame(sample_data(diet_byPlot.ph)), permutations = 4999))
  (diet_byPlot_betadisperUniFrac_species<-permutest(betadisper((distance(diet_byPlot.ph, method = "unifrac", type="samples")), group=diet_byPlot.ph@sam_data$GENUS_SPECIES), permutations = 4999))
  (diet_byPlot_betadisperUniFrac_location<-permutest(betadisper((distance(diet_byPlot.ph, method = "unifrac", type="samples")), group=diet_byPlot.ph@sam_data$LOCATION), permutations = 4999))
  (microbiome_byPlot_permanovaUniFrac<-adonis(distance(microbiome_byPlot.ph, method = "unifrac", type="samples")~GENUS_SPECIES*LOCATION, data=data.frame(sample_data(microbiome_byPlot.ph)), permutations = 4999))
  (microbiome_byPlot_betadisperUniFrac_species<-permutest(betadisper((distance(microbiome_byPlot.ph, method = "unifrac", type="samples")), group=microbiome_byPlot.ph@sam_data$GENUS_SPECIES), permutations = 4999))
  (microbiome_byPlot_betadisperUniFrac_location<-permutest(betadisper((distance(microbiome_byPlot.ph, method = "unifrac", type="samples")), group=microbiome_byPlot.ph@sam_data$LOCATION), permutations = 4999))
  
  
  
#### STEP 4: REGRESSION OF SHANNON DIVERSITY OF DIET, COMMUNITY, AND MICROBIOME, AT BOTH PLOT AND INDIVIDUAL LEVELS ####
  
#These analyses and models serve only to study general trends, and include all samples from all species, and for now ignore unbalanced sampling.
#More detailed models follow in step 6 below.
  
  #We handle different sample sizes by plot by running bootstraps of equal sample sizes for all plots, and taking mean and 95% CI of the results.

  #Define a new dataframe to collect all results.
  data_byPlot_allSpecies_collected<-NA
  
  #Define sample size as lowest number of individuals per plot for either diet or microbiome.
  ss_plot<-min(c(min(table(diet.ph@sam_data$LOCATION_PLOT)),min(table(microbiome.ph@sam_data$LOCATION_PLOT))))
  
  #Run 1000 bootstraps with equal sample sizes/plot.
  for(i in 1:1000){
    print(i)
    
    #First for microbiome data.
    microbiome_byPlot_allSpecies.ph<-microbiome.ph
    #Create a vector of samples to include when we take equal sample sizes per plot.
    samples<-NULL
    for(j in 1:length(table(microbiome_byPlot_allSpecies.ph@sam_data$LOCATION_PLOT))){
      samples<-c(samples,sample(which(microbiome_byPlot_allSpecies.ph@sam_data$LOCATION_PLOT==names(table(microbiome_byPlot_allSpecies.ph@sam_data$LOCATION_PLOT))[j]), ss_plot))
    }
    #Subset to create equal sample sizes by plot (but not by species!)
    microbiome_byPlot_allSpecies.ph<-prune_samples(sample_names(microbiome_byPlot_allSpecies.ph)[samples], microbiome_byPlot_allSpecies.ph)
    #Check that we have correctly subsetted to equal sample size by plot.
    table(microbiome_byPlot_allSpecies.ph@sam_data$LOCATION_PLOT)
    #Merge ALL samples by plot.
    microbiome_byPlot_allSpecies.ph<-phyloseq::merge_samples(microbiome_byPlot_allSpecies.ph, group=microbiome_byPlot_allSpecies.ph@sam_data$LOCATION_PLOT, fun = sum)
    #And update sample data in PhyloSeq objects.
    microbiome_byPlot_allSpecies.ph@sam_data$LOCATION<-sapply(strsplit(sample_names(microbiome_byPlot_allSpecies.ph),"_"),'[',1)
    microbiome_byPlot_allSpecies.ph@sam_data$PLOT<-sapply(strsplit(sample_names(microbiome_byPlot_allSpecies.ph),"_"),'[',2)
    #Prune data to exclude taxa with zero data left.
    microbiome_byPlot_allSpecies.ph<-prune_taxa(colSums(microbiome_byPlot_allSpecies.ph@otu_table@.Data)>0, microbiome_byPlot_allSpecies.ph)

    #Second for diet data.
    diet_byPlot_allSpecies.ph<-diet.ph
    #Create a vector of samples to include when we take equal sample sizes per plot.
    samples<-NULL
    for(j in 1:length(table(diet_byPlot_allSpecies.ph@sam_data$LOCATION_PLOT))){
      samples<-c(samples,sample(which(diet_byPlot_allSpecies.ph@sam_data$LOCATION_PLOT==names(table(diet_byPlot_allSpecies.ph@sam_data$LOCATION_PLOT))[j]), ss_plot))
    }
    #Subset to create equal sample sizes by plot (but not by species!)
    diet_byPlot_allSpecies.ph<-prune_samples(sample_names(diet_byPlot_allSpecies.ph)[samples], diet_byPlot_allSpecies.ph)
    #Check that we have correctly subsetted to equal sample size by plot.
    table(diet_byPlot_allSpecies.ph@sam_data$LOCATION_PLOT)
    #Merge ALL samples by plot.
    diet_byPlot_allSpecies.ph<-phyloseq::merge_samples(diet_byPlot_allSpecies.ph, group=diet_byPlot_allSpecies.ph@sam_data$LOCATION_PLOT, fun = sum)
    #And update sample data in PhyloSeq objects.
    diet_byPlot_allSpecies.ph@sam_data$LOCATION<-sapply(strsplit(sample_names(diet_byPlot_allSpecies.ph),"_"),'[',1)
    diet_byPlot_allSpecies.ph@sam_data$PLOT<-sapply(strsplit(sample_names(diet_byPlot_allSpecies.ph),"_"),'[',2)
    #Prune data to exclude taxa with zero data left.
    diet_byPlot_allSpecies.ph<-prune_taxa(colSums(diet_byPlot_allSpecies.ph@otu_table@.Data)>0, diet_byPlot_allSpecies.ph)

    #Aggregate data.
    microbiome_byPlot_allSpecies.ss<-data.frame(reshape(aggregate(value~samples+variable,plot_richness(microbiome_byPlot_allSpecies.ph,measures = c("Observed","Chao1","Shannon","Simpson","InvSimpson"))$data,mean),idvar = "samples",timevar = "variable",direction = "wide"))
    diet_byPlot_allSpecies.ss<-data.frame(reshape(aggregate(value~samples+variable,plot_richness(diet_byPlot_allSpecies.ph,measures = c("Observed","Chao1","Shannon","Simpson","InvSimpson"))$data,mean),idvar = "samples",timevar = "variable",direction = "wide"))
    community.shells.ss #was already calculated above
    community.live.ss #was already calculated above

    #Add Shannon evenness, which also is not by default in PhyloSeq, but defined as 1/D/ln(S) (Magurran & McGill, 2011, p 57).
    microbiome_byPlot_allSpecies.ss$ShannonEvenness<-1/ifelse(microbiome_byPlot_allSpecies.ss$value.Shannon==0,NA,microbiome_byPlot_allSpecies.ss$value.Shannon)/log(microbiome_byPlot_allSpecies.ss$value.Observed)
    diet_byPlot_allSpecies.ss$ShannonEvenness<-1/ifelse(diet_byPlot_allSpecies.ss$value.Shannon==0,NA,diet_byPlot_allSpecies.ss$value.Shannon)/log(diet_byPlot_allSpecies.ss$value.Observed)

    #Add Faith's PD, which is not by default included in a PhyloSeq function, but can be calculted using the picante package.
    microbiome_byPlot_allSpecies.ss<-merge.data.frame(microbiome_byPlot_allSpecies.ss,picante::pd(samp = (microbiome_byPlot_allSpecies.ph@otu_table@.Data), tree = microbiome_byPlot_allSpecies.ph@phy_tree, include.root = FALSE), by.x = 1, by.y = 0)
    diet_byPlot_allSpecies.ss<-merge.data.frame(diet_byPlot_allSpecies.ss,picante::pd(samp = (diet_byPlot_allSpecies.ph@otu_table@.Data), tree = diet_byPlot_allSpecies.ph@phy_tree, include.root = FALSE), by.x = 1, by.y = 0)

    #Gather these results in a single data.frame. Use merging to combine data from different data.frames.
    data_byPlot_allSpecies<-merge.data.frame(microbiome_byPlot_allSpecies.ss,diet_byPlot_allSpecies.ss,by = 1, suffixes = c(".micr",".diet"), all = T)

    #Add further data.
    data_byPlot_allSpecies<-merge.data.frame(data_byPlot_allSpecies,community.shells.ss, by=1)
    data_byPlot_allSpecies<-merge.data.frame(data_byPlot_allSpecies,community.live.ss, by=1, suffixes = c(".comm.shells",".comm.live"))
    colnames(data_byPlot_allSpecies)<-ifelse(substr(colnames(data_byPlot_allSpecies),1,6)=="value.",substr(colnames(data_byPlot_allSpecies),7,100),colnames(data_byPlot_allSpecies))
    data_byPlot_allSpecies<-merge.data.frame(data_byPlot_allSpecies,environment[,c(1:3,9:18)], by = 1)

    #Add the results from run i to the results from previous runs.
    if(i==1) {
      data_byPlot_allSpecies_collected<-data_byPlot_allSpecies
    } else {
      data_byPlot_allSpecies_collected<-rbind(data_byPlot_allSpecies_collected,data_byPlot_allSpecies)
    }
  }
  
  #Summarize the results.
  data_byPlot_allSpecies_summary<-as.data.frame(data_byPlot_allSpecies_collected %>%
                                                  group_by(samples) %>%
                                                  summarise(Shannon.micr.mean = mean(Shannon.micr, na.rm=T),
                                                            Shannon.micr.lowerCI = quantile(Shannon.micr, probs = 0.025, na.rm=T),
                                                            Shannon.micr.upperCI = quantile(Shannon.micr, probs = 0.975, na.rm=T),
                                                            PD.micr.mean = mean(PD.micr, na.rm=T),
                                                            PD.micr.lowerCI = quantile(PD.micr, probs = 0.025, na.rm=T),
                                                            PD.micr.upperCI = quantile(PD.micr, probs = 0.975, na.rm=T),
                                                            Chao1.micr.mean = mean(Chao1.micr, na.rm=T),
                                                            Chao1.micr.lowerCI = quantile(Chao1.micr, probs = 0.025, na.rm=T),
                                                            Chao1.micr.upperCI = quantile(Chao1.micr, probs = 0.975, na.rm=T),
                                                            Shannon.diet.mean = mean(Shannon.diet, na.rm=T),
                                                            Shannon.diet.lowerCI = quantile(Shannon.diet, probs = 0.025, na.rm=T),
                                                            Shannon.diet.upperCI = quantile(Shannon.diet, probs = 0.975, na.rm=T),
                                                            PD.diet.mean = mean(PD.diet, na.rm=T),
                                                            PD.diet.lowerCI = quantile(PD.diet, probs = 0.025, na.rm=T),
                                                            PD.diet.upperCI = quantile(PD.diet, probs = 0.975, na.rm=T),
                                                            Chao1.diet.mean = mean(Chao1.diet, na.rm=T),
                                                            Chao1.diet.lowerCI = quantile(Chao1.diet, probs = 0.025, na.rm=T),
                                                            Chao1.diet.upperCI = quantile(Chao1.diet, probs = 0.975, na.rm=T),
                                                            Shannon.comm.live = mean(Shannon.comm.live, na.rm=T),
                                                            PD.comm.live = mean(PD.comm.live, na.rm=T),
                                                            Chao1.comm.live = mean(Chao1.comm.live, na.rm=T),
                                                            Shannon.comm.shells = mean(Shannon.comm.shells, na.rm=T),
                                                            PD.comm.shells = mean(PD.comm.shells, na.rm=T),
                                                            Chao1.comm.shells = mean(Chao1.comm.shells, na.rm=T)))
  
  
  
### STEP 5: MANTEL TESTS OF DIET AND MICROBIOME DISTANCES ####

  #Because we will try to correlate microbiome and diet, we are now only interested in samples for which we have data on both.
  intersect<-unlist(intersect(sample_names(microbiome.ph),sample_names(diet.ph)))
  #The number of samples for which we have both data, is:
  length(intersect)
  
  #Calculate pairwise diet and microbiome distances based on UniFrac and weighted UniFrac metrics.
  #Note that Bray-Curtis, Jaccard, and similar distance metrics not incorporating a tree distance, are useless: Because
  #diets and microbiomes between individuals (i.e. samples) are so different, there is rarely much overlap, i.e. distances are virtually always unity.
  diet_distances_allSpecies_wunifrac<-distance(prune_taxa(rowSums(prune_samples(intersect, diet.ph)@otu_table@.Data)>0,prune_samples(intersect, diet.ph)), method="wunifrac", type="samples")
  diet_distances_allSpecies_unifrac<-distance(prune_taxa(rowSums(prune_samples(intersect, diet.ph)@otu_table@.Data)>0,prune_samples(intersect, diet.ph)), method="unifrac", type="samples")
  microbiome_distances_allSpecies_wunifrac<-distance(prune_taxa(rowSums(prune_samples(intersect, microbiome.ph)@otu_table@.Data)>0,prune_samples(intersect, microbiome.ph)), method="wunifrac", type="samples")
  microbiome_distances_allSpecies_unifrac<-distance(prune_taxa(rowSums(prune_samples(intersect, microbiome.ph)@otu_table@.Data)>0,prune_samples(intersect, microbiome.ph)), method="unifrac", type="samples")
  
  #Visually study the results.
  plot(diet_distances_allSpecies_unifrac[1:length(diet_distances_allSpecies_unifrac)], microbiome_distances_allSpecies_unifrac[1:length(microbiome_distances_allSpecies_unifrac)])
  plot(diet_distances_allSpecies_wunifrac[1:length(diet_distances_allSpecies_wunifrac)], microbiome_distances_allSpecies_wunifrac[1:length(microbiome_distances_allSpecies_wunifrac)])
  
  #Now we can run a Mantel test. We do this first for all samples and all species.
  (mantel_diet_microbiome_allSpecies_unifrac<-mantel(diet_distances_allSpecies_unifrac, microbiome_distances_allSpecies_unifrac, method="spearman", permutations=999, na.rm=T))
  (mantel_diet_microbiome_allSpecies_wunifrac<-mantel(diet_distances_allSpecies_wunifrac, microbiome_distances_allSpecies_wunifrac, method="spearman", permutations=999, na.rm=T))
  
  #And run for each of the three target species separately.
  #For A. jagori.
  intersect_A.jagori<-unlist(intersect(sample_names(prune_samples(microbiome.ph@sam_data$GENUS_SPECIES=="A_jagori", microbiome.ph)),
                                      sample_names(prune_samples(diet.ph@sam_data$GENUS_SPECIES=="A_jagori", diet.ph))))
  diet_distances_A.jagori_wunifrac<-distance(prune_taxa(rowSums(prune_samples(intersect_A.jagori, diet.ph)@otu_table@.Data)>0,prune_samples(intersect_A.jagori, diet.ph)), method="wunifrac", type="samples")
  diet_distances_A.jagori_unifrac<-distance(prune_taxa(rowSums(prune_samples(intersect_A.jagori, diet.ph)@otu_table@.Data)>0,prune_samples(intersect_A.jagori, diet.ph)), method="unifrac", type="samples")
  microbiome_distances_A.jagori_wunifrac<-distance(prune_taxa(rowSums(prune_samples(intersect_A.jagori, microbiome.ph)@otu_table@.Data)>0,prune_samples(intersect_A.jagori, microbiome.ph)), method="wunifrac", type="samples")
  microbiome_distances_A.jagori_unifrac<-distance(prune_taxa(rowSums(prune_samples(intersect_A.jagori, microbiome.ph)@otu_table@.Data)>0,prune_samples(intersect_A.jagori, microbiome.ph)), method="unifrac", type="samples")
  plot(diet_distances_A.jagori_unifrac[1:length(diet_distances_A.jagori_unifrac)], microbiome_distances_A.jagori_unifrac[1:length(microbiome_distances_A.jagori_unifrac)])
  plot(diet_distances_A.jagori_wunifrac[1:length(diet_distances_A.jagori_wunifrac)], microbiome_distances_A.jagori_wunifrac[1:length(microbiome_distances_A.jagori_wunifrac)])
  (mantel_diet_microbiome_A.jagori_unifrac<-mantel(diet_distances_A.jagori_unifrac, microbiome_distances_A.jagori_unifrac, method="spearman", permutations=999, na.rm=T))
  (mantel_diet_microbiome_A.jagori_wunifrac<-mantel(diet_distances_A.jagori_wunifrac, microbiome_distances_A.jagori_wunifrac, method="spearman", permutations=999, na.rm=T))
  
  #For G. similis s.l.
  intersect_G.similis<-unlist(intersect(sample_names(prune_samples(microbiome.ph@sam_data$GENUS_SPECIES=="G_similis", microbiome.ph)),
                                       sample_names(prune_samples(diet.ph@sam_data$GENUS_SPECIES=="G_similis", diet.ph))))
  diet_distances_G.similis_wunifrac<-distance(prune_taxa(rowSums(prune_samples(intersect_G.similis, diet.ph)@otu_table@.Data)>0,prune_samples(intersect_G.similis, diet.ph)), method="wunifrac", type="samples")
  diet_distances_G.similis_unifrac<-distance(prune_taxa(rowSums(prune_samples(intersect_G.similis, diet.ph)@otu_table@.Data)>0,prune_samples(intersect_G.similis, diet.ph)), method="unifrac", type="samples")
  microbiome_distances_G.similis_wunifrac<-distance(prune_taxa(rowSums(prune_samples(intersect_G.similis, microbiome.ph)@otu_table@.Data)>0,prune_samples(intersect_G.similis, microbiome.ph)), method="wunifrac", type="samples")
  microbiome_distances_G.similis_unifrac<-distance(prune_taxa(rowSums(prune_samples(intersect_G.similis, microbiome.ph)@otu_table@.Data)>0,prune_samples(intersect_G.similis, microbiome.ph)), method="unifrac", type="samples")
  plot(diet_distances_G.similis_unifrac[1:length(diet_distances_G.similis_unifrac)], microbiome_distances_G.similis_unifrac[1:length(microbiome_distances_G.similis_unifrac)])
  plot(diet_distances_G.similis_wunifrac[1:length(diet_distances_G.similis_wunifrac)], microbiome_distances_G.similis_wunifrac[1:length(microbiome_distances_G.similis_wunifrac)])
  (mantel_diet_microbiome_G.similis_unifrac<-mantel(diet_distances_G.similis_unifrac, microbiome_distances_G.similis_unifrac, method="spearman", permutations=999, na.rm=T))
  (mantel_diet_microbiome_G.similis_wunifrac<-mantel(diet_distances_G.similis_wunifrac, microbiome_distances_G.similis_wunifrac, method="spearman", permutations=999, na.rm=T))
  
  #For P. concinnum.
  intersect_P.concinnum<-unlist(intersect(sample_names(prune_samples(microbiome.ph@sam_data$GENUS_SPECIES=="P_concinnum", microbiome.ph)),
                                       sample_names(prune_samples(diet.ph@sam_data$GENUS_SPECIES=="P_concinnum", diet.ph))))
  diet_distances_P.concinnum_wunifrac<-distance(prune_taxa(rowSums(prune_samples(intersect_P.concinnum, diet.ph)@otu_table@.Data)>0,prune_samples(intersect_P.concinnum, diet.ph)), method="wunifrac", type="samples")
  diet_distances_P.concinnum_unifrac<-distance(prune_taxa(rowSums(prune_samples(intersect_P.concinnum, diet.ph)@otu_table@.Data)>0,prune_samples(intersect_P.concinnum, diet.ph)), method="unifrac", type="samples")
  microbiome_distances_P.concinnum_wunifrac<-distance(prune_taxa(rowSums(prune_samples(intersect_P.concinnum, microbiome.ph)@otu_table@.Data)>0,prune_samples(intersect_P.concinnum, microbiome.ph)), method="wunifrac", type="samples")
  microbiome_distances_P.concinnum_unifrac<-distance(prune_taxa(rowSums(prune_samples(intersect_P.concinnum, microbiome.ph)@otu_table@.Data)>0,prune_samples(intersect_P.concinnum, microbiome.ph)), method="unifrac", type="samples")
  plot(diet_distances_P.concinnum_unifrac[1:length(diet_distances_P.concinnum_unifrac)], microbiome_distances_P.concinnum_unifrac[1:length(microbiome_distances_P.concinnum_unifrac)])
  plot(diet_distances_P.concinnum_wunifrac[1:length(diet_distances_P.concinnum_wunifrac)], microbiome_distances_P.concinnum_wunifrac[1:length(microbiome_distances_P.concinnum_wunifrac)])
  (mantel_diet_microbiome_P.concinnum_unifrac<-mantel(diet_distances_P.concinnum_unifrac, microbiome_distances_P.concinnum_unifrac, method="spearman", permutations=999, na.rm=T))
  (mantel_diet_microbiome_P.concinnum_wunifrac<-mantel(diet_distances_P.concinnum_wunifrac, microbiome_distances_P.concinnum_wunifrac, method="spearman", permutations=999, na.rm=T))

  #Repeat the above at a by-plot level.
  #First re-create plot-level diet and microbiome phyloseq objects.
  diet_byPlot_allSpecies.ph<-merge_samples(diet.ph, group = diet.ph@sam_data$LOCATION_PLOT,  fun = sum)
  microbiome_byPlot_allSpecies.ph<-merge_samples(microbiome.ph, group = microbiome.ph@sam_data$LOCATION_PLOT,  fun = sum)
  
  intersect_byPlot_allSpecies<-unlist(intersect(sample_names(diet_byPlot_allSpecies.ph),sample_names(microbiome_byPlot_allSpecies.ph)))
  diet_distances_byPlot_allSpecies_wunifrac<-distance(prune_taxa(colSums(prune_samples(intersect_byPlot_allSpecies, diet_byPlot_allSpecies.ph)@otu_table@.Data)>0,prune_samples(intersect_byPlot_allSpecies, diet_byPlot_allSpecies.ph)), method="wunifrac", type="samples")
  diet_distances_byPlot_allSpecies_unifrac<-distance(prune_taxa(colSums(prune_samples(intersect_byPlot_allSpecies, diet_byPlot_allSpecies.ph)@otu_table@.Data)>0,prune_samples(intersect_byPlot_allSpecies, diet_byPlot_allSpecies.ph)), method="unifrac", type="samples")
  microbiome_distances_byPlot_allSpecies_wunifrac<-distance(prune_taxa(colSums(prune_samples(intersect_byPlot_allSpecies, microbiome_byPlot_allSpecies.ph)@otu_table@.Data)>0,prune_samples(intersect_byPlot_allSpecies, microbiome_byPlot_allSpecies.ph)), method="wunifrac", type="samples")
  microbiome_distances_byPlot_allSpecies_unifrac<-distance(prune_taxa(colSums(prune_samples(intersect_byPlot_allSpecies, microbiome_byPlot_allSpecies.ph)@otu_table@.Data)>0,prune_samples(intersect_byPlot_allSpecies, microbiome_byPlot_allSpecies.ph)), method="unifrac", type="samples")
  
  #Visually study the results by plot.
  plot(diet_distances_byPlot_allSpecies_unifrac[1:length(diet_distances_byPlot_allSpecies_unifrac)], microbiome_distances_byPlot_allSpecies_unifrac[1:length(microbiome_distances_byPlot_allSpecies_unifrac)])
  plot(diet_distances_byPlot_allSpecies_wunifrac[1:length(diet_distances_byPlot_allSpecies_wunifrac)], microbiome_distances_byPlot_allSpecies_wunifrac[1:length(microbiome_distances_byPlot_allSpecies_wunifrac)])
  
  #Now we can run a Mantel test by plot. We do this first for all samples and all species.
  (mantel_diet_microbiome_byPlot_allSpecies_unifrac<-mantel(diet_distances_byPlot_allSpecies_unifrac, microbiome_distances_byPlot_allSpecies_unifrac, method="spearman", permutations=999, na.rm=T))
  (mantel_diet_microbiome_byPlot_allSpecies_wunifrac<-mantel(diet_distances_byPlot_allSpecies_wunifrac, microbiome_distances_byPlot_allSpecies_wunifrac, method="spearman", permutations=999, na.rm=T))
  
  #And again we can do this per species.
  #For A. jagori.
  diet_byPlot_A.jagori.ph<-merge_samples(prune_samples(diet.ph@sam_data$GENUS_SPECIES=="A_jagori", diet.ph), group = prune_samples(diet.ph@sam_data$GENUS_SPECIES=="A_jagori", diet.ph)@sam_data$LOCATION_PLOT,  fun = sum)
  microbiome_byPlot_A.jagori.ph<-merge_samples(prune_samples(microbiome.ph@sam_data$GENUS_SPECIES=="A_jagori", microbiome.ph), group = prune_samples(microbiome.ph@sam_data$GENUS_SPECIES=="A_jagori", microbiome.ph)@sam_data$LOCATION_PLOT,  fun = sum)
  intersect_byPlot_A.jagori<-unlist(intersect(sample_names(diet_byPlot_A.jagori.ph),sample_names(microbiome_byPlot_A.jagori.ph)))
  diet_distances_byPlot_A.jagori_wunifrac<-distance(prune_taxa(colSums(prune_samples(intersect_byPlot_A.jagori, diet_byPlot_A.jagori.ph)@otu_table@.Data)>0,prune_samples(intersect_byPlot_A.jagori, diet_byPlot_A.jagori.ph)), method="wunifrac", type="samples")
  diet_distances_byPlot_A.jagori_unifrac<-distance(prune_taxa(colSums(prune_samples(intersect_byPlot_A.jagori, diet_byPlot_A.jagori.ph)@otu_table@.Data)>0,prune_samples(intersect_byPlot_A.jagori, diet_byPlot_A.jagori.ph)), method="unifrac", type="samples")
  microbiome_distances_byPlot_A.jagori_wunifrac<-distance(prune_taxa(colSums(prune_samples(intersect_byPlot_A.jagori, microbiome_byPlot_A.jagori.ph)@otu_table@.Data)>0,prune_samples(intersect_byPlot_A.jagori, microbiome_byPlot_A.jagori.ph)), method="wunifrac", type="samples")
  microbiome_distances_byPlot_A.jagori_unifrac<-distance(prune_taxa(colSums(prune_samples(intersect_byPlot_A.jagori, microbiome_byPlot_A.jagori.ph)@otu_table@.Data)>0,prune_samples(intersect_byPlot_A.jagori, microbiome_byPlot_A.jagori.ph)), method="unifrac", type="samples")
  plot(diet_distances_byPlot_A.jagori_unifrac[1:length(diet_distances_byPlot_A.jagori_unifrac)], microbiome_distances_byPlot_A.jagori_unifrac[1:length(microbiome_distances_byPlot_A.jagori_unifrac)])
  plot(diet_distances_byPlot_A.jagori_wunifrac[1:length(diet_distances_byPlot_A.jagori_wunifrac)], microbiome_distances_byPlot_A.jagori_wunifrac[1:length(microbiome_distances_byPlot_A.jagori_wunifrac)])
  (mantel_diet_microbiome_byPlot_A.jagori_unifrac<-mantel(diet_distances_byPlot_A.jagori_unifrac, microbiome_distances_byPlot_A.jagori_unifrac, method="spearman", permutations=999, na.rm=T))
  (mantel_diet_microbiome_byPlot_A.jagori_wunifrac<-mantel(diet_distances_byPlot_A.jagori_wunifrac, microbiome_distances_byPlot_A.jagori_wunifrac, method="spearman", permutations=999, na.rm=T))
  
  #For G. similis s.l.
  diet_byPlot_G.similis.ph<-merge_samples(prune_samples(diet.ph@sam_data$GENUS_SPECIES=="G_similis", diet.ph), group = prune_samples(diet.ph@sam_data$GENUS_SPECIES=="G_similis", diet.ph)@sam_data$LOCATION_PLOT,  fun = sum)
  microbiome_byPlot_G.similis.ph<-merge_samples(prune_samples(microbiome.ph@sam_data$GENUS_SPECIES=="G_similis", microbiome.ph), group = prune_samples(microbiome.ph@sam_data$GENUS_SPECIES=="G_similis", microbiome.ph)@sam_data$LOCATION_PLOT,  fun = sum)
  intersect_byPlot_G.similis<-unlist(intersect(sample_names(diet_byPlot_G.similis.ph),sample_names(microbiome_byPlot_G.similis.ph)))
  diet_distances_byPlot_G.similis_wunifrac<-distance(prune_taxa(colSums(prune_samples(intersect_byPlot_G.similis, diet_byPlot_G.similis.ph)@otu_table@.Data)>0,prune_samples(intersect_byPlot_G.similis, diet_byPlot_G.similis.ph)), method="wunifrac", type="samples")
  diet_distances_byPlot_G.similis_unifrac<-distance(prune_taxa(colSums(prune_samples(intersect_byPlot_G.similis, diet_byPlot_G.similis.ph)@otu_table@.Data)>0,prune_samples(intersect_byPlot_G.similis, diet_byPlot_G.similis.ph)), method="unifrac", type="samples")
  microbiome_distances_byPlot_G.similis_wunifrac<-distance(prune_taxa(colSums(prune_samples(intersect_byPlot_G.similis, microbiome_byPlot_G.similis.ph)@otu_table@.Data)>0,prune_samples(intersect_byPlot_G.similis, microbiome_byPlot_G.similis.ph)), method="wunifrac", type="samples")
  microbiome_distances_byPlot_G.similis_unifrac<-distance(prune_taxa(colSums(prune_samples(intersect_byPlot_G.similis, microbiome_byPlot_G.similis.ph)@otu_table@.Data)>0,prune_samples(intersect_byPlot_G.similis, microbiome_byPlot_G.similis.ph)), method="unifrac", type="samples")
  plot(diet_distances_byPlot_G.similis_unifrac[1:length(diet_distances_byPlot_G.similis_unifrac)], microbiome_distances_byPlot_G.similis_unifrac[1:length(microbiome_distances_byPlot_G.similis_unifrac)])
  plot(diet_distances_byPlot_G.similis_wunifrac[1:length(diet_distances_byPlot_G.similis_wunifrac)], microbiome_distances_byPlot_G.similis_wunifrac[1:length(microbiome_distances_byPlot_G.similis_wunifrac)])
  (mantel_diet_microbiome_byPlot_G.similis_unifrac<-mantel(diet_distances_byPlot_G.similis_unifrac, microbiome_distances_byPlot_G.similis_unifrac, method="spearman", permutations=999, na.rm=T))
  (mantel_diet_microbiome_byPlot_G.similis_wunifrac<-mantel(diet_distances_byPlot_G.similis_wunifrac, microbiome_distances_byPlot_G.similis_wunifrac, method="spearman", permutations=999, na.rm=T))
  
  #For P. concinnum.
  diet_byPlot_P.concinnum.ph<-merge_samples(prune_samples(diet.ph@sam_data$GENUS_SPECIES=="P_concinnum", diet.ph), group = prune_samples(diet.ph@sam_data$GENUS_SPECIES=="P_concinnum", diet.ph)@sam_data$LOCATION_PLOT,  fun = sum)
  microbiome_byPlot_P.concinnum.ph<-merge_samples(prune_samples(microbiome.ph@sam_data$GENUS_SPECIES=="P_concinnum", microbiome.ph), group = prune_samples(microbiome.ph@sam_data$GENUS_SPECIES=="P_concinnum", microbiome.ph)@sam_data$LOCATION_PLOT,  fun = sum)
  intersect_byPlot_P.concinnum<-unlist(intersect(sample_names(diet_byPlot_P.concinnum.ph),sample_names(microbiome_byPlot_P.concinnum.ph)))
  diet_distances_byPlot_P.concinnum_wunifrac<-distance(prune_taxa(colSums(prune_samples(intersect_byPlot_P.concinnum, diet_byPlot_P.concinnum.ph)@otu_table@.Data)>0,prune_samples(intersect_byPlot_P.concinnum, diet_byPlot_P.concinnum.ph)), method="wunifrac", type="samples")
  diet_distances_byPlot_P.concinnum_unifrac<-distance(prune_taxa(colSums(prune_samples(intersect_byPlot_P.concinnum, diet_byPlot_P.concinnum.ph)@otu_table@.Data)>0,prune_samples(intersect_byPlot_P.concinnum, diet_byPlot_P.concinnum.ph)), method="unifrac", type="samples")
  microbiome_distances_byPlot_P.concinnum_wunifrac<-distance(prune_taxa(colSums(prune_samples(intersect_byPlot_P.concinnum, microbiome_byPlot_P.concinnum.ph)@otu_table@.Data)>0,prune_samples(intersect_byPlot_P.concinnum, microbiome_byPlot_P.concinnum.ph)), method="wunifrac", type="samples")
  microbiome_distances_byPlot_P.concinnum_unifrac<-distance(prune_taxa(colSums(prune_samples(intersect_byPlot_P.concinnum, microbiome_byPlot_P.concinnum.ph)@otu_table@.Data)>0,prune_samples(intersect_byPlot_P.concinnum, microbiome_byPlot_P.concinnum.ph)), method="unifrac", type="samples")
  plot(diet_distances_byPlot_P.concinnum_unifrac[1:length(diet_distances_byPlot_P.concinnum_unifrac)], microbiome_distances_byPlot_P.concinnum_unifrac[1:length(microbiome_distances_byPlot_P.concinnum_unifrac)])
  plot(diet_distances_byPlot_P.concinnum_wunifrac[1:length(diet_distances_byPlot_P.concinnum_wunifrac)], microbiome_distances_byPlot_P.concinnum_wunifrac[1:length(microbiome_distances_byPlot_P.concinnum_wunifrac)])
  (mantel_diet_microbiome_byPlot_P.concinnum_unifrac<-mantel(diet_distances_byPlot_P.concinnum_unifrac, microbiome_distances_byPlot_P.concinnum_unifrac, method="spearman", permutations=999, na.rm=T))
  (mantel_diet_microbiome_byPlot_P.concinnum_wunifrac<-mantel(diet_distances_byPlot_P.concinnum_wunifrac, microbiome_distances_byPlot_P.concinnum_wunifrac, method="spearman", permutations=999, na.rm=T))
  
  
#### STEP 6: (GENERALIZED) LINEAR MIXED MODELS OF MICROBIOME AND DIET VS. HOST COMMUNITY, SPECIES, AND LOCATION ####

#Having collected multiple individuals from each species per plot, we include the plot as a random effect in each of the following models.

#(A) Test for correlation between diet and community Shannon diversities, with influence of the species and location.
  
  #We check the response variable against several likely distributions to learn which one fits the data best.
  #Note that we take out smaples for which Shannon.diet==0, or otherwise no distribution fits the data properly.
  descdist(data$Shannon.diet[is.na(data$Shannon.diet)==F & data$Shannon.diet>0], discrete = FALSE, boot = 1000)
  Shannon.diet_distributions<-list(fitdistrplus::fitdist(data$Shannon.diet[is.na(data$Shannon.diet)==F & data$Shannon.diet>0], "weibull"),
                                   fitdistrplus::fitdist(data$Shannon.diet[is.na(data$Shannon.diet)==F & data$Shannon.diet>0], "gamma"),
                                   fitdistrplus::fitdist(data$Shannon.diet[is.na(data$Shannon.diet)==F & data$Shannon.diet>0], "lnorm"),
                                   fitdistrplus::fitdist(data$Shannon.diet[is.na(data$Shannon.diet)==F & data$Shannon.diet>0], "logis"),
                                   fitdistrplus::fitdist(data$Shannon.diet[is.na(data$Shannon.diet)==F & data$Shannon.diet>0], "norm"))
  gofstat(Shannon.diet_distributions)
  #And this can be checked graphically, too.
  denscomp(Shannon.diet_distributions)
  
  #Based on the AIC, we choose the family of the normal distribution to create a complete model.
  model.Shannon.diet1<-glmmTMB(Shannon.diet ~ Shannon.comm.shells * GENUS_SPECIES + (1|LOCATION_PLOT), 
                               data=data[data$GENUS_SPECIES %in% c("A_jagori","P_concinnum","G_similis") & is.na(data$Shannon.diet)==F & data$Shannon.diet>0,],
                               family = gaussian(link="identity"))
  summary(model.Shannon.diet1)
  #We see that none of the species differences, host community Shannon diversity, or interactions, are significant, except for different responses between A. jagori and P. concinnum.
  
  #We are primarily interested in possible differences in the response of each species and need the complete model to study these. 
  #(For more on this method, see https://cran.r-project.org/web/packages/emmeans/vignettes/interactions.html)
  (model.Shannon.diet1.contrast.GENUS_SPECIES<-emtrends(model.Shannon.diet1, pairwise~GENUS_SPECIES, var="Shannon.comm.shells"))
  #And we check whether the interactions -per species- are significant.
  (summary(model.Shannon.diet1.contrast.GENUS_SPECIES, infer=c(TRUE,TRUE),null=0))
  #After Tukey adjusted pairwise comparisons, we see that indeed there's a significant difference in response between A. jagori and P. concinnum.
  #This can be shown graphically as follows.
  emmip(model.Shannon.diet1, GENUS_SPECIES~Shannon.comm.shells, cov.reduce=range)
  
  #Let's compare this complete model to simpler, nested models.
  model.Shannon.diet2<-update(model.Shannon.diet1, .~. -Shannon.comm.shells:GENUS_SPECIES)
  model.Shannon.diet3<-update(model.Shannon.diet2, .~. -Shannon.comm.shells)
  model.Shannon.diet4<-update(model.Shannon.diet2, .~. -GENUS_SPECIES)
  model.Shannon.diet5<-update(model.Shannon.diet2, .~. -GENUS_SPECIES -Shannon.comm.shells)
  #We compare these models and find that model.Shannon.diet3, excluding the effect of the Shannon.comm.shells and interactions, performs best, following AIC.
  anova(model.Shannon.diet1,model.Shannon.diet2,model.Shannon.diet3,model.Shannon.diet4,model.Shannon.diet5)
  summary(model.Shannon.diet3)
  #Which shows that there are significant species differences, though not in the response to the host community Shannon diversity.
  
  #We continue by modelling the effect of location on the different species in species-specific models.
  #Because not all species are present in all locations, we cannot study one overall model of both species and location as fixed effects.
  #(E.g. A. jagori does not occur on Batangan, and as such we have no data for this species-location combination.)
  #We continue with the normal distribution for the response data as determined above for the all-species model.
  #(We understand these subsets of the data should follow, by definition, the same distribution as the overall dataset.)
  
  #For A. jagori (Locations with <10 samples removed, as well as Tomanggong 2).
    model.Shannon.A_jag.diet1<-glmmTMB(Shannon.diet ~ Shannon.comm.shells * LOCATION + (1|LOCATION_PLOT), 
                                       data=data[data$GENUS_SPECIES=="A_jagori" & is.na(data$Shannon.diet)==F & data$Shannon.diet>0 & data$LOCATION %in% names(table(data[data$GENUS_SPECIES=="A_jagori",]$LOCATION)[(table(data[data$GENUS_SPECIES=="A_jagori",]$LOCATION)>31)]),],
                                       family = gaussian(link="identity"))
    summary(model.Shannon.A_jag.diet1)
    #And check for significance of contrasts between locations using the complete model.
    (model.Shannon.A_jag.diet1.contrast.LOCATION<-emtrends(model.Shannon.A_jag.diet1, pairwise~LOCATION, var="Shannon.comm.shells"))
    emmip(model.Shannon.A_jag.diet1, LOCATION~Shannon.comm.shells, cov.reduce=range)
    #Let's compare this complete model to simpler, nested models.
    model.Shannon.A_jag.diet2<-update(model.Shannon.A_jag.diet1, .~. -Shannon.comm.shells:LOCATION)
    model.Shannon.A_jag.diet3<-update(model.Shannon.A_jag.diet2, .~. -Shannon.comm.shells)
    model.Shannon.A_jag.diet4<-update(model.Shannon.A_jag.diet2, .~. -LOCATION)
    model.Shannon.A_jag.diet5<-update(model.Shannon.A_jag.diet2, .~. -LOCATION -Shannon.comm.shells)
    #We compare these models and find that model.Shannon.A_jag.diet5, the empty model, performs best, following AIC.
    #Second best model is model 4.
    anova(model.Shannon.A_jag.diet1,model.Shannon.A_jag.diet2,model.Shannon.A_jag.diet3,model.Shannon.A_jag.diet4,model.Shannon.A_jag.diet5)
    summary(model.Shannon.A_jag.diet4)
    
  #For G. similis s.l. (Note here two locations with too few data had to be removed before could make this model converge).
    model.Shannon.G_sim.diet1<-glmmTMB(Shannon.diet ~ Shannon.comm.shells * LOCATION + (1|LOCATION_PLOT), 
                                       data=data[data$GENUS_SPECIES=="G_similis" & is.na(data$Shannon.diet)==F & data$Shannon.diet>0 & data$LOCATION %in% names(table(data[data$GENUS_SPECIES=="G_similis",]$LOCATION)[(table(data[data$GENUS_SPECIES=="G_similis",]$LOCATION)>16)]),],
                                       family = gaussian(link="identity"))
    summary(model.Shannon.G_sim.diet1)
    #And check for significance of contrasts between locations using the complete model.
    (model.Shannon.G_sim.diet1.contrast.LOCATION<-emtrends(model.Shannon.G_sim.diet1, pairwise~LOCATION, var="Shannon.comm.shells"))
    emmip(model.Shannon.G_sim.diet1, LOCATION~Shannon.comm.shells, cov.reduce=range)
    #Let's compare this complete model to simpler, nested models.
    model.Shannon.G_sim.diet2<-update(model.Shannon.G_sim.diet1, .~. -Shannon.comm.shells:LOCATION)
    model.Shannon.G_sim.diet3<-update(model.Shannon.G_sim.diet2, .~. -Shannon.comm.shells)
    model.Shannon.G_sim.diet4<-update(model.Shannon.G_sim.diet2, .~. -LOCATION)
    model.Shannon.G_sim.diet5<-update(model.Shannon.G_sim.diet2, .~. -LOCATION -Shannon.comm.shells)
    #We compare these models and find confirmation that model.Shannon.G_sim.diet5, excluding any of the fixed effects, performs best, following AIC.
    anova(model.Shannon.G_sim.diet1,model.Shannon.G_sim.diet2,model.Shannon.G_sim.diet3,model.Shannon.G_sim.diet4,model.Shannon.G_sim.diet5)
    #This indicates none of the fixed effects explain the diet Shannon diversity significantly.
    summary(model.Shannon.G_sim.diet5)
    
  #For P. concinnum.
    #Locations with <10 samples removed.
    model.Shannon.P_con.diet1<-glmmTMB(Shannon.diet ~ Shannon.comm.shells * LOCATION + (1|LOCATION_PLOT), 
                                       data=data[data$GENUS_SPECIES=="P_concinnum" & is.na(data$Shannon.diet)==F & data$Shannon.diet>0 & data$LOCATION %in% names(table(data[data$GENUS_SPECIES=="P_concinnum",]$LOCATION)[(table(data[data$GENUS_SPECIES=="P_concinnum",]$LOCATION)>9)]),],
                                       family = gaussian(link="identity"))
    summary(model.Shannon.P_con.diet1)
    #And check for significance of contrasts between locations using the complete model.
    (model.Shannon.P_con.diet1.contrast.LOCATION<-emtrends(model.Shannon.P_con.diet1, pairwise~LOCATION, var="Shannon.comm.shells"))
    emmip(model.Shannon.P_con.diet1, LOCATION~Shannon.comm.shells, cov.reduce=range)
    #Trends contrast stronlgy between locations, but none are significant.
    #Let's compare this complete model to simpler, nested models.
    model.Shannon.P_con.diet2<-update(model.Shannon.P_con.diet1, .~. -Shannon.comm.shells:LOCATION)
    model.Shannon.P_con.diet3<-update(model.Shannon.P_con.diet2, .~. -Shannon.comm.shells)
    model.Shannon.P_con.diet4<-update(model.Shannon.P_con.diet2, .~. -LOCATION)
    model.Shannon.P_con.diet5<-update(model.Shannon.P_con.diet2, .~. -LOCATION -Shannon.comm.shells)
    #We compare these models and find that model.Shannon.P_con.diet3, excluding host community diversity and interaction effects, performs best, following AIC.
    anova(model.Shannon.P_con.diet1,model.Shannon.P_con.diet2,model.Shannon.P_con.diet3,model.Shannon.P_con.diet4,model.Shannon.P_con.diet5)
    summary(model.Shannon.P_con.diet3)
  
#(B) Test for correlation between diet and community Faith's phylogenetic diversities (PD), with influence of the species and location.
  
  #We check the response variable against several likely distributions to learn which one fits the data best.
  descdist(data$PD.diet[is.na(data$PD.diet)==F & data$PD.diet>0], discrete = FALSE, boot = 1000)
  PD.diet_distributions<-list(fitdistrplus::fitdist(data$PD.diet[is.na(data$PD.diet)==F & data$PD.diet>0], "weibull"),
                              fitdistrplus::fitdist(data$PD.diet[is.na(data$PD.diet)==F & data$PD.diet>0], "gamma"),
                              fitdistrplus::fitdist(data$PD.diet[is.na(data$PD.diet)==F & data$PD.diet>0], "lnorm"),
                              fitdistrplus::fitdist(data$PD.diet[is.na(data$PD.diet)==F & data$PD.diet>0], "logis"),
                              fitdistrplus::fitdist(data$PD.diet[is.na(data$PD.diet)==F & data$PD.diet>0], "norm"))
  gofstat(PD.diet_distributions)
  #And this can be checked graphically, too.
  denscomp(PD.diet_distributions)
  
  #Based on the AIC, we choose a lognormal distribution to create a complete model (which is the second best fit, but the best fit to result in a stable model).
  model.PD.diet1<-glmmTMB(PD.diet ~ PD.comm.shells * GENUS_SPECIES + (1|LOCATION_PLOT), 
                          data=data[data$GENUS_SPECIES %in% c("A_jagori","P_concinnum","G_similis") & is.na(data$PD.diet)==F & data$PD.diet>0,],
                          family = gaussian(link="log"))
  summary(model.PD.diet1)
  #We see here that response to PD in the diet of G. similis and P. concinnum is significantly different from that of A. jagori.
  
  #We are primarily interested in possible differences in the response of each species. 
  (model.PD.diet1.contrast.GENUS_SPECIES<-emtrends(model.PD.diet1, pairwise~GENUS_SPECIES, var="PD.comm.shells"))
  #Contrast in the response between A. jagori and G. similis are not significant, whereas the other two contrasts are.
  #And we check whether the interactions -per species- are significant.
  (summary(model.PD.diet1.contrast.GENUS_SPECIES, infer=c(TRUE,TRUE),null=0))
  #This can be shown graphically as follows.
  emmip(model.PD.diet1, GENUS_SPECIES~PD.comm.shells, cov.reduce=range)
  
  #Let's compare this complete model to simpler, nested models.
  model.PD.diet2<-update(model.PD.diet1, .~. -PD.comm.shells:GENUS_SPECIES)
  model.PD.diet3<-update(model.PD.diet2, .~. -PD.comm.shells)
  model.PD.diet4<-update(model.PD.diet2, .~. -GENUS_SPECIES)
  model.PD.diet5<-update(model.PD.diet2, .~. -GENUS_SPECIES -PD.comm.shells)
  #We compare these models and find confirmation that model 1, including the interaction effect, performs best, following AIC.
  anova(model.PD.diet1,model.PD.diet2,model.PD.diet3,model.PD.diet4,model.PD.diet5)
  
  #We continue by modelling the effect of location on the different species in species-specific models.
  #Because not all species are present in all locations, we cannot study one overall model with both species and location as fixed effects.
  #(E.g. A. jagori does not occur on Batangan, and as such we have no data for this species-location combination.)
  #We continue with the gamma distribution for the response data as determined above for the all-species model.
  #(We understand these subsets of the data should follow, by definition, the same distribution as the overall dataset.)
  
  #For A. jagori (Locations with <10 samples removed, and Tomanggong 2).
    model.PD.A_jag.diet1<-glmmTMB(PD.diet ~ PD.comm.shells * LOCATION + (1|LOCATION_PLOT), 
                                  data=data[data$GENUS_SPECIES=="A_jagori" & is.na(data$PD.diet)==F & data$PD.diet>0 & data$LOCATION %in% names(table(data[data$GENUS_SPECIES=="A_jagori",]$LOCATION)[(table(data[data$GENUS_SPECIES=="A_jagori",]$LOCATION)>31)]),],
                                  family = gaussian(link="identity"))
    summary(model.PD.A_jag.diet1)
    #And check for significance of contrasts between locations using the complete model.
    (model.PD.A_jag.diet1.contrast.LOCATION<-emtrends(model.PD.A_jag.diet1, pairwise~LOCATION, var="PD.comm.shells"))
    emmip(model.PD.A_jag.diet1, LOCATION~PD.comm.shells, cov.reduce=range)
    #Let's compare this complete model to simpler, nested models.
    model.PD.A_jag.diet2<-update(model.PD.A_jag.diet1, .~. -PD.comm.shells:LOCATION)
    model.PD.A_jag.diet3<-update(model.PD.A_jag.diet2, .~. -PD.comm.shells)
    model.PD.A_jag.diet4<-update(model.PD.A_jag.diet2, .~. -LOCATION)
    model.PD.A_jag.diet5<-update(model.PD.A_jag.diet2, .~. -LOCATION -PD.comm.shells)
    #We compare these models and find that model.PD.A_jag.diet4, excluding fixed effect location or interactions, performs best, following AIC.
    anova(model.PD.A_jag.diet1,model.PD.A_jag.diet2,model.PD.A_jag.diet3,model.PD.A_jag.diet4,model.PD.A_jag.diet5)
    summary(model.PD.A_jag.diet4)
    
  #For G. similis s.l. (Note here two locations with too few data had to be removed before could make this model converge).
    model.PD.G_sim.diet1<-glmmTMB(PD.diet ~ PD.comm.shells * LOCATION + (1|LOCATION_PLOT), 
                                  data=data[data$GENUS_SPECIES=="G_similis" & is.na(data$PD.diet)==F & data$PD.diet>0 & data$LOCATION %in% names(table(data[data$GENUS_SPECIES=="G_similis",]$LOCATION)[(table(data[data$GENUS_SPECIES=="G_similis",]$LOCATION)>16)]),],
                                  family = gaussian(link="identity"))
    summary(model.PD.G_sim.diet1)
    #And check for significance of contrasts between locations using the complete model.
    (model.PD.G_sim.diet1.contrast.LOCATION<-emtrends(model.PD.G_sim.diet1, pairwise~LOCATION, var="PD.comm.shells"))
    emmip(model.PD.G_sim.diet1, LOCATION~PD.comm.shells, cov.reduce=range)
    #Let's compare this complete model to simpler, nested models.
    model.PD.G_sim.diet2<-update(model.PD.G_sim.diet1, .~. -PD.comm.shells:LOCATION)
    model.PD.G_sim.diet3<-update(model.PD.G_sim.diet2, .~. -PD.comm.shells)
    model.PD.G_sim.diet4<-update(model.PD.G_sim.diet2, .~. -LOCATION)
    model.PD.G_sim.diet5<-update(model.PD.G_sim.diet2, .~. -LOCATION -PD.comm.shells)
    #We compare these models and find confirmation that model 4, excluding the effects of  location and interactions, performs best, following AIC.
    anova(model.PD.G_sim.diet1,model.PD.G_sim.diet2,model.PD.G_sim.diet3,model.PD.G_sim.diet4,model.PD.G_sim.diet5)
    summary(model.PD.G_sim.diet4)
  
  #For P. concinnum.
    #Locations with <10 samples removed.
    model.PD.P_con.diet1<-glmmTMB(PD.diet ~ PD.comm.shells * LOCATION + (1|LOCATION_PLOT), 
                                  data=data[data$GENUS_SPECIES=="P_concinnum" & is.na(data$PD.diet)==F & data$PD.diet>0 & data$LOCATION %in% names(table(data[data$GENUS_SPECIES=="P_concinnum",]$LOCATION)[(table(data[data$GENUS_SPECIES=="P_concinnum",]$LOCATION)>9)]),],
                                  family = gaussian(link="identity"))
    summary(model.PD.P_con.diet1)
    #And check for significance of contrasts between locations using the complete model.
    (model.PD.P_con.diet1.contrast.LOCATION<-emtrends(model.PD.P_con.diet1, pairwise~LOCATION, var="PD.comm.shells"))
    emmip(model.PD.P_con.diet1, LOCATION~PD.comm.shells, cov.reduce=range)
    #Although response for P. coninnum differ strongly between locations, these contrasts are often not significant.
    #Let's compare this complete model to simpler, nested models.
    model.PD.P_con.diet2<-update(model.PD.P_con.diet1, .~. -PD.comm.shells:LOCATION)
    model.PD.P_con.diet3<-update(model.PD.P_con.diet2, .~. -PD.comm.shells)
    model.PD.P_con.diet4<-update(model.PD.P_con.diet2, .~. -LOCATION)
    model.PD.P_con.diet5<-update(model.PD.P_con.diet2, .~. -LOCATION -PD.comm.shells)
    #We compare these models and find that model.PD.P_con.diet5, excluding any of our fixed effects, performs best, following AIC.
    anova(model.PD.P_con.diet1,model.PD.P_con.diet2,model.PD.P_con.diet3,model.PD.P_con.diet4,model.PD.P_con.diet5)
    #This shows that none of the included fixed effects has any influence on the response variable.
    summary(model.PD.P_con.diet5)
  
#(C) Test for correlation between diet and community Chao1 richnesses, with influence of the species as fixed effect, the location as random effect.
  
  #We check the response variable against several likely distributions to learn which one fits the data best.
  descdist(data$Chao1.diet[is.na(data$Chao1.diet)==F & data$Chao1.diet>0], discrete = FALSE, boot = 1000)
  Chao1.diet_distributions<-list(fitdistrplus::fitdist(data$Chao1.diet[is.na(data$Chao1.diet)==F & data$Chao1.diet>0], "weibull"),
                                 fitdistrplus::fitdist(data$Chao1.diet[is.na(data$Chao1.diet)==F & data$Chao1.diet>0], "gamma"),
                                 fitdistrplus::fitdist(data$Chao1.diet[is.na(data$Chao1.diet)==F & data$Chao1.diet>0], "lnorm"),
                                 fitdistrplus::fitdist(data$Chao1.diet[is.na(data$Chao1.diet)==F & data$Chao1.diet>0], "logis"),
                                 fitdistrplus::fitdist(data$Chao1.diet[is.na(data$Chao1.diet)==F & data$Chao1.diet>0], "norm"))
  gofstat(Chao1.diet_distributions)
  #And this can be checked graphically, too.
  denscomp(Chao1.diet_distributions)
  
  #Based on the AIC, we choose the family of the lognormal distribution.
  model.Chao1.diet1<-glmmTMB(Chao1.diet ~ Chao1.comm.shells * GENUS_SPECIES + (1|LOCATION_PLOT), 
                             data=data[data$GENUS_SPECIES %in% c("A_jagori","P_concinnum","G_similis") & is.na(data$Chao1.diet)==F & data$Chao1.diet>0,],
                             family = gaussian(link="log"))
  summary(model.Chao1.diet1)
  
  #We are primarily interested in possible differences in the response of each species. 
  (model.Chao1.diet1.contrast.GENUS_SPECIES<-emtrends(model.Chao1.diet1, pairwise~GENUS_SPECIES, var="Chao1.comm.shells"))
  #Only contrast in the response between A. jagori and P. concinnum is significant, the other two are not.
  #And we check whether the interactions -per species- are significant.
  (summary(model.Chao1.diet1.contrast.GENUS_SPECIES, infer=c(TRUE,TRUE),null=0))
  #This can be shown graphically as follows.
  emmip(model.Chao1.diet1, GENUS_SPECIES~Chao1.comm.shells, cov.reduce=range)
  
  #Let's study simpler, nested models by dropping terms.
  model.Chao1.diet2<-update(model.Chao1.diet1, .~. -Chao1.comm.shells:GENUS_SPECIES)
  model.Chao1.diet3<-update(model.Chao1.diet2, .~. -Chao1.comm.shells)
  model.Chao1.diet4<-update(model.Chao1.diet2, .~. -GENUS_SPECIES)
  model.Chao1.diet5<-update(model.Chao1.diet2, .~. -GENUS_SPECIES -Chao1.comm.shells)
  #We compare the other models and learn that model.Chao1.diet1, the complete model, scores best, based on AIC.
  anova(model.Chao1.diet1,model.Chao1.diet2,model.Chao1.diet3,model.Chao1.diet4,model.Chao1.diet5)
  
  #We continue by modelling the effect of location on the different species in species-specific models.
  #Because not all species are present in all locations, we cannot study one overall model of both species and location as fixed effects.
  #(E.g. A. jagori does not occur on Batangan, and as such we have no data for this species-location combination.)
  #We continue with the lognormal distribution for the response data as determined above for the all-species model.
  #(We understand these subsets of the data should follow, by definition, the same distribution as the overall dataset.)
  
  #For A. jagori (Locations with <10 samples removed).
    model.Chao1.A_jag.diet1<-glmmTMB(Chao1.diet ~ Chao1.comm.shells * LOCATION + (1|LOCATION_PLOT), 
                                     data=data[data$GENUS_SPECIES=="A_jagori" & is.na(data$Chao1.diet)==F & data$Chao1.diet>0 & data$LOCATION %in% names(table(data[data$GENUS_SPECIES=="A_jagori",]$LOCATION)[(table(data[data$GENUS_SPECIES=="A_jagori",]$LOCATION)>9)]),],
                                     family = gaussian(link="log"))
    summary(model.Chao1.A_jag.diet1)
    #And check for significance of contrasts between locations using the complete model.
    (model.Chao1.A_jag.diet1.contrast.LOCATION<-emtrends(model.Chao1.A_jag.diet1, pairwise~LOCATION, var="Chao1.comm.shells"))
    emmip(model.Chao1.A_jag.diet1, LOCATION~Chao1.comm.shells, cov.reduce=range)
    #Let's compare this complete model to simpler, nested models.
    model.Chao1.A_jag.diet2<-update(model.Chao1.A_jag.diet1, .~. -Chao1.comm.shells:LOCATION)
    model.Chao1.A_jag.diet3<-update(model.Chao1.A_jag.diet2, .~. -Chao1.comm.shells)
    model.Chao1.A_jag.diet4<-update(model.Chao1.A_jag.diet2, .~. -LOCATION)
    model.Chao1.A_jag.diet5<-update(model.Chao1.A_jag.diet2, .~. -LOCATION -Chao1.comm.shells)
    #We compare these models and find that model.Chao1.A_jag.diet1, the complete model, performs best, following AIC.
    anova(model.Chao1.A_jag.diet1,model.Chao1.A_jag.diet2,model.Chao1.A_jag.diet3,model.Chao1.A_jag.diet4,model.Chao1.A_jag.diet5)
    
  #For G. similis s.l. (Note here two locations with too few samples had to be removed before could make this model converge).
    model.Chao1.G_sim.diet1<-glmmTMB(Chao1.diet ~ Chao1.comm.shells * LOCATION + (1|LOCATION_PLOT), 
                                     data=data[data$GENUS_SPECIES=="G_similis" & is.na(data$Chao1.diet)==F & data$Chao1.diet>0 & data$LOCATION %in% names(table(data[data$GENUS_SPECIES=="G_similis",]$LOCATION)[(table(data[data$GENUS_SPECIES=="G_similis",]$LOCATION)>16)]),],
                                     family = gaussian(link="log"))
    summary(model.Chao1.G_sim.diet1)
    #And check for significance of contrasts between locations using the complete model.
    (model.Chao1.G_sim.diet1.contrast.LOCATION<-emtrends(model.Chao1.G_sim.diet1, pairwise~LOCATION, var="Chao1.comm.shells"))
    emmip(model.Chao1.G_sim.diet1, LOCATION~Chao1.comm.shells, cov.reduce=range)
    #Let's compare this complete model to simpler, nested models.
    model.Chao1.G_sim.diet2<-update(model.Chao1.G_sim.diet1, .~. -Chao1.comm.shells:LOCATION)
    model.Chao1.G_sim.diet3<-update(model.Chao1.G_sim.diet2, .~. -Chao1.comm.shells)
    model.Chao1.G_sim.diet4<-update(model.Chao1.G_sim.diet2, .~. -LOCATION)
    model.Chao1.G_sim.diet5<-update(model.Chao1.G_sim.diet2, .~. -LOCATION -Chao1.comm.shells)
    #We compare these models and find confirmation that the complete model performs best, based on AIC (but about equal to the empty model!).
    anova(model.Chao1.G_sim.diet1,model.Chao1.G_sim.diet2,model.Chao1.G_sim.diet3,model.Chao1.G_sim.diet4,model.Chao1.G_sim.diet5)
    
  #For P. concinnum. (Locations with <10 samples removed.)
    model.Chao1.P_con.diet1<-glmmTMB(Chao1.diet ~ Chao1.comm.shells * LOCATION + (1|LOCATION_PLOT), 
                                     data=data[data$GENUS_SPECIES=="P_concinnum" & is.na(data$Chao1.diet)==F & data$Chao1.diet>0 & data$LOCATION %in% names(table(data[data$GENUS_SPECIES=="P_concinnum",]$LOCATION)[(table(data[data$GENUS_SPECIES=="P_concinnum",]$LOCATION)>9)]),],
                                     family = gaussian(link="log"))
    summary(model.Chao1.P_con.diet1)
    #And check for significance of contrasts between locations using the complete model.
    (model.Chao1.P_con.diet1.contrast.LOCATION<-emtrends(model.Chao1.P_con.diet1, pairwise~LOCATION, var="Chao1.comm.shells"))
    emmip(model.Chao1.P_con.diet1, LOCATION~Chao1.comm.shells, cov.reduce=range)
    #Let's compare this complete model to simpler, nested models.
    model.Chao1.P_con.diet2<-update(model.Chao1.P_con.diet1, .~. -Chao1.comm.shells:LOCATION)
    model.Chao1.P_con.diet3<-update(model.Chao1.P_con.diet2, .~. -Chao1.comm.shells)
    model.Chao1.P_con.diet4<-update(model.Chao1.P_con.diet2, .~. -LOCATION)
    model.Chao1.P_con.diet5<-update(model.Chao1.P_con.diet2, .~. -LOCATION -Chao1.comm.shells)
    #We compare these models and find that model.Chao1.P_con.diet3, excluding effect of Cha01.comm.shells and interaction of the fixed effects, performs best, following AIC.
    anova(model.Chao1.P_con.diet1,model.Chao1.P_con.diet2,model.Chao1.P_con.diet3,model.Chao1.P_con.diet4,model.Chao1.P_con.diet5)
    summary(model.Chao1.P_con.diet3)
    
  
#(D) Test for correlation between microbiome and community Shannon diversities, with influence of the species and location.
  
  #We check the response variable against several likely distributions to learn which one fits the data best.
  descdist(data$Shannon.micr[is.na(data$Shannon.micr)==F], discrete = FALSE, boot = 1000)
  Shannon.micr_distributions<-list(fitdistrplus::fitdist(data$Shannon.micr[is.na(data$Shannon.micr)==F], "weibull"),
                                   fitdistrplus::fitdist(data$Shannon.micr[is.na(data$Shannon.micr)==F], "gamma"),
                                   fitdistrplus::fitdist(data$Shannon.micr[is.na(data$Shannon.micr)==F], "lnorm"),
                                   fitdistrplus::fitdist(data$Shannon.micr[is.na(data$Shannon.micr)==F], "logis"),
                                   fitdistrplus::fitdist(data$Shannon.micr[is.na(data$Shannon.micr)==F], "norm"))
  gofstat(Shannon.micr_distributions)
  #And this can be checked graphically, too.
  denscomp(Shannon.micr_distributions)
  
  #Based on the AIC, a gamma model scores best, but results in errors; hence, we choose the second best family of the lognormal distribution to create a complete model.
  model.Shannon.micr1<-glmmTMB(Shannon.micr ~ Shannon.comm.shells * GENUS_SPECIES + (1|LOCATION_PLOT), 
                               data=data[data$GENUS_SPECIES %in% c("A_jagori","P_concinnum","G_similis") & is.na(data$Shannon.micr)==F,],
                               family = gaussian(link="log"))
  summary(model.Shannon.micr1)
  #We see here that  diversity in the microbiome of G. similis is significantly differently influenced by diversity in the community
  #than found in A. jagori; the difference in response for P. concinnum is not significant (i.e. same response, or same slope).
  
  #We are primarily interested in possible differences in the response of each species and need the complete model to study these. 
  #(For more on this method, see https://cran.r-project.org/web/packages/emmeans/vignettes/interactions.html)
  (model.Shannon.micr1.contrast.GENUS_SPECIES<-emtrends(model.Shannon.micr1, pairwise~GENUS_SPECIES, var="Shannon.comm.shells"))
  #After Tukey adjusted pairwise comparisons, we see that the contrast in the response between G. similis and P. concinnum is almost significant.
  #And we check whether the interactions -per species- are significant.
  (summary(model.Shannon.micr1.contrast.GENUS_SPECIES, infer=c(TRUE,TRUE),null=0))
  #This can be shown graphically as follows.
  emmip(model.Shannon.micr1, GENUS_SPECIES~Shannon.comm.shells, cov.reduce=range)
  
  #Let's compare this complete model to simpler, nested models.
  model.Shannon.micr2<-update(model.Shannon.micr1, .~. -Shannon.comm.shells:GENUS_SPECIES)
  model.Shannon.micr3<-update(model.Shannon.micr2, .~. -Shannon.comm.shells)
  model.Shannon.micr4<-update(model.Shannon.micr2, .~. -GENUS_SPECIES)
  model.Shannon.micr5<-update(model.Shannon.micr2, .~. -GENUS_SPECIES -Shannon.comm.shells)
  #We compare these models and find confirmation that model 1, including the effect of the Shannon.comm.shells performs best, following AIC.
  anova(model.Shannon.micr1,model.Shannon.micr2,model.Shannon.micr3,model.Shannon.micr4,model.Shannon.micr5)
  
  #We continue by modelling the effect of location on the different species in species-specific models.
  #Because not all species are present in all locations, we cannot study one overall model of both species and location as fixed effects.
  #(E.g. A. jagori does not occur on Batangan, and as such we have no data for this species-location combination.)
  #We continue with the lognormal distribution for the response data as determined above for the all-species model.
  #(We understand these subsets of the data should follow, by definition, the same distribution as the overall dataset.)
  
  #For A. jagori (Locations with <10 samples removed).
    model.Shannon.A_jag.micr1<-glmmTMB(Shannon.micr ~ Shannon.comm.shells * LOCATION + (1|LOCATION_PLOT), 
                                       data=data[data$GENUS_SPECIES=="A_jagori" & is.na(data$Shannon.micr)==F & data$LOCATION %in% names(table(data[data$GENUS_SPECIES=="A_jagori",]$LOCATION)[(table(data[data$GENUS_SPECIES=="A_jagori",]$LOCATION)>9)]),],
                                       family = gaussian(link="log"))
    summary(model.Shannon.A_jag.micr1)
    #And check for significance of contrasts between locations using the complete model.
    (model.Shannon.A_jag.micr1.contrast.LOCATION<-emtrends(model.Shannon.A_jag.micr1, pairwise~LOCATION, var="Shannon.comm.shells"))
    emmip(model.Shannon.A_jag.micr1, LOCATION~Shannon.comm.shells, cov.reduce=range)
    #Let's compare this complete model to simpler, nested models.
    model.Shannon.A_jag.micr2<-update(model.Shannon.A_jag.micr1, .~. -Shannon.comm.shells:LOCATION)
    model.Shannon.A_jag.micr3<-update(model.Shannon.A_jag.micr2, .~. -Shannon.comm.shells)
    model.Shannon.A_jag.micr4<-update(model.Shannon.A_jag.micr2, .~. -LOCATION)
    model.Shannon.A_jag.micr5<-update(model.Shannon.A_jag.micr2, .~. -LOCATION -Shannon.comm.shells)
    #We compare these models and find confirmation that model 1, including the effect of the Shannon.comm.shells performs best, following AIC.
    anova(model.Shannon.A_jag.micr1,model.Shannon.A_jag.micr2,model.Shannon.A_jag.micr3,model.Shannon.A_jag.micr4,model.Shannon.A_jag.micr5)
  
  #For G. similis s.l. (Note here two locations with too few data had to be removed before could make this model converge).
    model.Shannon.G_sim.micr1<-glmmTMB(Shannon.micr ~ Shannon.comm.shells * LOCATION + (1|LOCATION_PLOT), 
                                       data=data[data$GENUS_SPECIES=="G_similis" & is.na(data$Shannon.micr)==F & data$LOCATION %in% names(table(data[data$GENUS_SPECIES=="G_similis",]$LOCATION)[(table(data[data$GENUS_SPECIES=="G_similis",]$LOCATION)>16)]),],
                                       family = gaussian(link="log"))
    summary(model.Shannon.G_sim.micr1)
    #And check for significance of contrasts between locations using the complete model.
    (model.Shannon.G_sim.micr1.contrast.LOCATION<-emtrends(model.Shannon.G_sim.micr1, pairwise~LOCATION, var="Shannon.comm.shells"))
    emmip(model.Shannon.G_sim.micr1, LOCATION~Shannon.comm.shells, cov.reduce=range)
    #Let's compare this complete model to simpler, nested models.
    model.Shannon.G_sim.micr2<-update(model.Shannon.G_sim.micr1, .~. -Shannon.comm.shells:LOCATION)
    model.Shannon.G_sim.micr3<-update(model.Shannon.G_sim.micr2, .~. -Shannon.comm.shells)
    model.Shannon.G_sim.micr4<-update(model.Shannon.G_sim.micr2, .~. -LOCATION)
    model.Shannon.G_sim.micr5<-update(model.Shannon.G_sim.micr2, .~. -LOCATION -Shannon.comm.shells)
    #We compare these models and find that model 1, the complete model, performs best, following AIC. (But model 3 almost as good!)
    anova(model.Shannon.G_sim.micr1,model.Shannon.G_sim.micr2,model.Shannon.G_sim.micr3,model.Shannon.G_sim.micr4,model.Shannon.G_sim.micr5)
  
  #For P. concinnum.
    #Locations with <10 samples removed.
    model.Shannon.P_con.micr1<-glmmTMB(Shannon.micr ~ Shannon.comm.shells * LOCATION + (1|LOCATION_PLOT), 
                                       data=data[data$GENUS_SPECIES=="P_concinnum" & is.na(data$Shannon.micr)==F & data$LOCATION %in% names(table(data[data$GENUS_SPECIES=="P_concinnum",]$LOCATION)[(table(data[data$GENUS_SPECIES=="P_concinnum",]$LOCATION)>9)]),],
                                       family = gaussian(link="log"))
    summary(model.Shannon.P_con.micr1)#And check for significance of contrasts between locations using the complete model.
    #And check for significance of contrasts between locations using the complete model.
    (model.Shannon.P_con.micr1.contrast.LOCATION<-emtrends(model.Shannon.P_con.micr1, pairwise~LOCATION, var="Shannon.comm.shells"))
    emmip(model.Shannon.P_con.micr1, LOCATION~Shannon.comm.shells, cov.reduce=range)
    #Trends contrast stronlgy between locations, but none are significant.
    #Let's compare this complete model to simpler, nested models.
    model.Shannon.P_con.micr2<-update(model.Shannon.P_con.micr1, .~. -Shannon.comm.shells:LOCATION)
    model.Shannon.P_con.micr3<-update(model.Shannon.P_con.micr2, .~. -Shannon.comm.shells)
    model.Shannon.P_con.micr4<-update(model.Shannon.P_con.micr2, .~. -LOCATION)
    model.Shannon.P_con.micr5<-update(model.Shannon.P_con.micr2, .~. -LOCATION -Shannon.comm.shells)
    #We compare these models and find confirmation that model 3, excluding effects of Shannon.comm.shells, significantly performs best, following AIC.
    anova(model.Shannon.P_con.micr1,model.Shannon.P_con.micr2,model.Shannon.P_con.micr3,model.Shannon.P_con.micr4,model.Shannon.P_con.micr5)
    #Which shows that for P. concinnum, the second model scores best on AIC, i.e. interaction of the fixed effects can be removed.
    summary(model.Shannon.P_con.micr2)

#(E) Test for correlation between microbiome and community Faith's phylogenetic diversities (PD), with influence of the species and location.
  
  #We check the response variable against several likely distributions to learn which one fits the data best.
  descdist(data$PD.micr[is.na(data$PD.micr)==F], discrete = FALSE, boot = 1000)
  PD.micr_distributions<-list(fitdistrplus::fitdist(data$PD.micr[is.na(data$PD.micr)==F], "weibull"),
                              fitdistrplus::fitdist(data$PD.micr[is.na(data$PD.micr)==F], "gamma"),
                              fitdistrplus::fitdist(data$PD.micr[is.na(data$PD.micr)==F], "lnorm"),
                              fitdistrplus::fitdist(data$PD.micr[is.na(data$PD.micr)==F], "logis"),
                              fitdistrplus::fitdist(data$PD.micr[is.na(data$PD.micr)==F], "norm"))
  gofstat(PD.micr_distributions)
  #And this can be checked graphically, too.
  denscomp(PD.micr_distributions)
  
  #Based on the AIC, we choose a gamma distribution to create a complete model.
  model.PD.micr1<-glmmTMB(PD.micr ~ PD.comm.shells * GENUS_SPECIES + (1|LOCATION_PLOT), 
                          data=data[data$GENUS_SPECIES %in% c("A_jagori","P_concinnum","G_similis") & is.na(data$PD.micr)==F,],
                          family = Gamma(link="identity"))
  summary(model.PD.micr1)
  #The PD of the host community influences the PD of the microbiome of P. concinnum significantly different from that of A. jagori.
  
  #We are primarily interested in possible differences in the response of each species. 
  (model.PD.micr1.contrast.GENUS_SPECIES<-emtrends(model.PD.micr1, pairwise~GENUS_SPECIES, var="PD.comm.shells"))
  #Contrast in the response between A. jagori and G. similis are not significant, whereas the other two contrasts are.
  #And we check whether the interactions -per species- are significant.
  (summary(model.PD.micr1.contrast.GENUS_SPECIES, infer=c(TRUE,TRUE),null=0))
  #This can be shown graphically as follows.
  emmip(model.PD.micr1, GENUS_SPECIES~PD.comm.shells, cov.reduce=range)
  
  #Let's compare this complete model to simpler, nested models.
  model.PD.micr2<-update(model.PD.micr1, .~. -PD.comm.shells:GENUS_SPECIES)
  model.PD.micr3<-update(model.PD.micr2, .~. -PD.comm.shells)
  model.PD.micr4<-update(model.PD.micr2, .~. -GENUS_SPECIES)
  model.PD.micr5<-update(model.PD.micr2, .~. -GENUS_SPECIES -PD.comm.shells)
  #We compare these models and find confirmation that model 1, including the interaction effect, performs best, following AIC.
  anova(model.PD.micr1,model.PD.micr2,model.PD.micr3,model.PD.micr4,model.PD.micr5)
  
  #We continue by modelling the effect of location on the different species in species-specific models.
  #Because not all species are present in all locations, we cannot study one overall model with both species and location as fixed effects.
  #(E.g. A. jagori does not occur on Batangan, and as such we have no data for this species-location combination.)
  #We continue with the gamma distribution for the response data as determined above for the all-species model.
  #(We understand these subsets of the data should follow, by definition, the same distribution as the overall dataset.)
  
  #For A. jagori (Locations with <10 samples removed).
    model.PD.A_jag.micr1<-glmmTMB(PD.micr ~ PD.comm.shells * LOCATION + (1|LOCATION_PLOT), 
                                  data=data[data$GENUS_SPECIES=="A_jagori" & is.na(data$PD.micr)==F & data$LOCATION %in% names(table(data[data$GENUS_SPECIES=="A_jagori",]$LOCATION)[(table(data[data$GENUS_SPECIES=="A_jagori",]$LOCATION)>9)]),],
                                  family = Gamma(link="identity"))
    summary(model.PD.A_jag.micr1)
    #And check for significance of contrasts between locations using the complete model.
    (model.PD.A_jag.micr1.contrast.LOCATION<-emtrends(model.PD.A_jag.micr1, pairwise~LOCATION, var="PD.comm.shells"))
    emmip(model.PD.A_jag.micr1, LOCATION~PD.comm.shells, cov.reduce=range)
    #Let's compare this complete model to simpler, nested models.
    model.PD.A_jag.micr2<-update(model.PD.A_jag.micr1, .~. -PD.comm.shells:LOCATION) #Results in an error and not considered below!
    model.PD.A_jag.micr3<-update(model.PD.A_jag.micr2, .~. -PD.comm.shells)
    model.PD.A_jag.micr4<-update(model.PD.A_jag.micr2, .~. -LOCATION)
    model.PD.A_jag.micr5<-update(model.PD.A_jag.micr2, .~. -LOCATION -PD.comm.shells)
    #We compare these models and find confirmation that model 1, including the effect of the PD.comm.shells performs best, following AIC.
    anova(model.PD.A_jag.micr1,model.PD.A_jag.micr3,model.PD.A_jag.micr4,model.PD.A_jag.micr5)
    #Which shows that for A. jagori, the full model scores best on AIC.
    
  #For G. similis s.l. (Note here two locations with too few data had to be removed before could make this model converge).
    model.PD.G_sim.micr1<-glmmTMB(PD.micr ~ PD.comm.shells * LOCATION + (1|LOCATION_PLOT), 
                                  data=data[data$GENUS_SPECIES=="G_similis" & is.na(data$PD.micr)==F & data$LOCATION %in% names(table(data[data$GENUS_SPECIES=="G_similis",]$LOCATION)[(table(data[data$GENUS_SPECIES=="G_similis",]$LOCATION)>16)]),],
                                  family = Gamma(link="identity"))
    summary(model.PD.G_sim.micr1)
    #And check for significance of contrasts between locations using the complete model.
    (model.PD.G_sim.micr1.contrast.LOCATION<-emtrends(model.PD.G_sim.micr1, pairwise~LOCATION, var="PD.comm.shells"))
    emmip(model.PD.G_sim.micr1, LOCATION~PD.comm.shells, cov.reduce=range)
    #Let's compare this complete model to simpler, nested models.
    model.PD.G_sim.micr2<-update(model.PD.G_sim.micr1, .~. -PD.comm.shells:LOCATION)
    model.PD.G_sim.micr3<-update(model.PD.G_sim.micr2, .~. -PD.comm.shells)
    model.PD.G_sim.micr4<-update(model.PD.G_sim.micr2, .~. -LOCATION)
    model.PD.G_sim.micr5<-update(model.PD.G_sim.micr2, .~. -LOCATION -PD.comm.shells) #Results in an error and not considered below!
    #We compare these models and find confirmation that model 1, including the effect of the PD.comm.shells and interactions, performs best, following AIC.
    anova(model.PD.G_sim.micr1,model.PD.G_sim.micr2,model.PD.G_sim.micr3,model.PD.G_sim.micr4)
    
  #For P. concinnum.
    #Locations with <10 samples removed.
    model.PD.P_con.micr1<-glmmTMB(PD.micr ~ PD.comm.shells * LOCATION + (1|LOCATION_PLOT), 
                                  data=data[data$GENUS_SPECIES=="P_concinnum" &  is.na(data$PD.micr)==F & data$LOCATION %in% names(table(data[data$GENUS_SPECIES=="P_concinnum",]$LOCATION)[(table(data[data$GENUS_SPECIES=="P_concinnum",]$LOCATION)>9)]),],
                                  family = Gamma(link="identity"))
    summary(model.PD.P_con.micr1)
    #And check for significance of contrasts between locations using the complete model.
    (model.PD.P_con.micr1.contrast.LOCATION<-emtrends(model.PD.P_con.micr1, pairwise~LOCATION, var="PD.comm.shells"))
    emmip(model.PD.P_con.micr1, LOCATION~PD.comm.shells, cov.reduce=range)
    #Although response for P. coninnum differ strongly between locations, these contrasts are often not significant.
    #Let's compare this complete model to simpler, nested models.
    model.PD.P_con.micr2<-update(model.PD.P_con.micr1, .~. -PD.comm.shells:LOCATION)
    model.PD.P_con.micr3<-update(model.PD.P_con.micr2, .~. -PD.comm.shells)
    model.PD.P_con.micr4<-update(model.PD.P_con.micr2, .~. -LOCATION)
    model.PD.P_con.micr5<-update(model.PD.P_con.micr2, .~. -LOCATION -PD.comm.shells)
    #We compare these models and find confirmation that model 1, including the effect of the PD.comm.shells performs best, following AIC.
    anova(model.PD.P_con.micr1,model.PD.P_con.micr2,model.PD.P_con.micr3,model.PD.P_con.micr4,model.PD.P_con.micr5)
    #Which shows that for P. concinnum, again, the first, complete model scores best on AIC.

#(F) Test for correlation between microbiome and community Chao1 richnesses, with influence of the species as fixed effect, the location as random effect.
  
  #We check the response variable against several likely distributions to learn which one fits the data best.
  descdist(data$Chao1.micr[is.na(data$Chao1.micr)==F], discrete = FALSE, boot = 1000)
  Chao1.micr_distributions<-list(fitdistrplus::fitdist(data$Chao1.micr[is.na(data$Chao1.micr)==F], "weibull"),
                                 fitdistrplus::fitdist(data$Chao1.micr[is.na(data$Chao1.micr)==F], "gamma"),
                                 fitdistrplus::fitdist(data$Chao1.micr[is.na(data$Chao1.micr)==F], "lnorm"),
                                 fitdistrplus::fitdist(data$Chao1.micr[is.na(data$Chao1.micr)==F], "logis"),
                                 fitdistrplus::fitdist(data$Chao1.micr[is.na(data$Chao1.micr)==F], "norm"))
  gofstat(Chao1.micr_distributions)
  #And this can be checked graphically, too.
  denscomp(Chao1.micr_distributions)
  
  #Based on the AIC, we find a best fit for the family of the lognormal distribution, but this results in errors. Hence, we take the second best fit, a gamma distribition.
  model.Chao1.micr1<-glmmTMB(Chao1.micr ~ Chao1.comm.shells * GENUS_SPECIES + (1|LOCATION_PLOT), 
                             data=data[data$GENUS_SPECIES %in% c("A_jagori","P_concinnum","G_similis") & is.na(data$Chao1.micr)==F,],
                             family = Gamma(link="identity"))
  summary(model.Chao1.micr1)
  
  #We are primarily interested in possible differences in the response of each species. 
  (model.Chao1.micr1.contrast.GENUS_SPECIES<-emtrends(model.Chao1.micr1, pairwise~GENUS_SPECIES, var="Chao1.comm.shells"))
  #Only contrast in the response between G. similis and P. concinnum are significant.
  #And we check whether the interactions -per species- are significant.
  (summary(model.Chao1.micr1.contrast.GENUS_SPECIES, infer=c(TRUE,TRUE),null=0))
  #This can be shown graphically as follows.
  emmip(model.Chao1.micr1, GENUS_SPECIES~Chao1.comm.shells, cov.reduce=range)
  
  #Let's study simpler, nested models by dropping terms.
  model.Chao1.micr2<-update(model.Chao1.micr1, .~. -Chao1.comm.shells:GENUS_SPECIES) #Results in an error and not considered below!
  model.Chao1.micr3<-update(model.Chao1.micr2, .~. -Chao1.comm.shells)
  model.Chao1.micr4<-update(model.Chao1.micr2, .~. -GENUS_SPECIES) #Results in an error and not considered below!
  model.Chao1.micr5<-update(model.Chao1.micr2, .~. -GENUS_SPECIES -Chao1.comm.shells)
  #We compare the other models.
  anova(model.Chao1.micr1,model.Chao1.micr3,model.Chao1.micr5)
  #The complete model has the lowest AIC and is chosen: included effects and interactions are thus important.
  
  #We continue by modelling the effect of location on the different species in species-specific models.
  #Because not all species are present in all locations, we cannot study one overall model of both species and location as fixed effects.
  #(E.g. A. jagori does not occur on Batangan, and as such we have no data for this species-location combination.)
  #We continue with the lognormal distribution for the response data as determined above for the all-species model.
  #(We understand these subsets of the data should follow, by definition, the same distribution as the overall dataset.)
  
  #For A. jagori (Locations with <10 samples removed).
    model.Chao1.A_jag.micr1<-glmmTMB(Chao1.micr ~ Chao1.comm.shells * LOCATION + (1|LOCATION_PLOT), 
                                     data=data[data$GENUS_SPECIES=="A_jagori" & is.na(data$Chao1.micr)==F & data$LOCATION %in% names(table(data[data$GENUS_SPECIES=="A_jagori",]$LOCATION)[(table(data[data$GENUS_SPECIES=="A_jagori",]$LOCATION)>9)]),],
                                     family = gaussian(link="log"))
    summary(model.Chao1.A_jag.micr1)
    #And check for significance of contrasts between locations using the complete model.
    (model.Chao1.A_jag.micr1.contrast.LOCATION<-emtrends(model.Chao1.A_jag.micr1, pairwise~LOCATION, var="Chao1.comm.shells"))
    emmip(model.Chao1.A_jag.micr1, LOCATION~Chao1.comm.shells, cov.reduce=range)
    #Let's compare this complete model to simpler, nested models.
    model.Chao1.A_jag.micr2<-update(model.Chao1.A_jag.micr1, .~. -Chao1.comm.shells:LOCATION)
    model.Chao1.A_jag.micr3<-update(model.Chao1.A_jag.micr2, .~. -Chao1.comm.shells)
    model.Chao1.A_jag.micr4<-update(model.Chao1.A_jag.micr2, .~. -LOCATION)
    model.Chao1.A_jag.micr5<-update(model.Chao1.A_jag.micr2, .~. -LOCATION -Chao1.comm.shells)
    #We compare these models and find confirmation that model 1, including the effect of the Chao1.comm.shells performs best, following AIC.
    anova(model.Chao1.A_jag.micr1,model.Chao1.A_jag.micr2,model.Chao1.A_jag.micr3,model.Chao1.A_jag.micr4,model.Chao1.A_jag.micr5)
    #Which shows that for A. jagori, the model without interaction and influence of host community richness scores best on AIC.
    summary(model.Chao1.A_jag.micr3)
    
  #For G. similis s.l. (Note here two locations with too few samples had to be removed before could make this model converge).
    model.Chao1.G_sim.micr1<-glmmTMB(Chao1.micr ~ Chao1.comm.shells * LOCATION + (1|LOCATION_PLOT), 
                                     data=data[data$GENUS_SPECIES=="G_similis" & is.na(data$Chao1.micr)==F & data$LOCATION %in% names(table(data[data$GENUS_SPECIES=="G_similis",]$LOCATION)[(table(data[data$GENUS_SPECIES=="G_similis",]$LOCATION)>16)]),],
                                     family = gaussian(link="log"))
    summary(model.Chao1.G_sim.micr1)
    #And check for significance of contrasts between locations using the complete model.
    (model.Chao1.G_sim.micr1.contrast.LOCATION<-emtrends(model.Chao1.G_sim.micr1, pairwise~LOCATION, var="Chao1.comm.shells"))
    emmip(model.Chao1.G_sim.micr1, LOCATION~Chao1.comm.shells, cov.reduce=range)
    #Let's compare this complete model to simpler, nested models.
    model.Chao1.G_sim.micr2<-update(model.Chao1.G_sim.micr1, .~. -Chao1.comm.shells:LOCATION)
    model.Chao1.G_sim.micr3<-update(model.Chao1.G_sim.micr2, .~. -Chao1.comm.shells)
    model.Chao1.G_sim.micr4<-update(model.Chao1.G_sim.micr2, .~. -LOCATION)
    model.Chao1.G_sim.micr5<-update(model.Chao1.G_sim.micr2, .~. -LOCATION -Chao1.comm.shells)
    #We compare these models and find confirmation that model 4, which does not include effect of location, performs best.
    anova(model.Chao1.G_sim.micr1,model.Chao1.G_sim.micr2,model.Chao1.G_sim.micr3,model.Chao1.G_sim.micr4,model.Chao1.G_sim.micr5)
    #Which shows that also for G. similis, there is no significant influence (by location) of Chao1.comm.shells.
    summary(model.Chao1.G_sim.micr4)
    
  #For P. concinnum.
    #Locations with <10 samples removed.
    #NOTE: this model does not converge using either lognormal or Gamma distribition (the first and second best fit to the data).
    #Hence, a normal distibution is used instead.
    model.Chao1.P_con.micr1<-glmmTMB(Chao1.micr ~ Chao1.comm.shells * LOCATION + (1|LOCATION_PLOT), 
                                     data=data[data$GENUS_SPECIES=="P_concinnum" & is.na(data$Chao1.micr)==F & data$LOCATION %in% names(table(data[data$GENUS_SPECIES=="P_concinnum",]$LOCATION)[(table(data[data$GENUS_SPECIES=="P_concinnum",]$LOCATION)>9)]),],
                                     family = gaussian(link="identity"))
    summary(model.Chao1.P_con.micr1)
    #And check for significance of contrasts between locations using the complete model.
    (model.Chao1.P_con.micr1.contrast.LOCATION<-emtrends(model.Chao1.P_con.micr1, pairwise~LOCATION, var="Chao1.comm.shells"))
    emmip(model.Chao1.P_con.micr1, LOCATION~Chao1.comm.shells, cov.reduce=range)
    #Let's compare this complete model to simpler, nested models.
    model.Chao1.P_con.micr2<-update(model.Chao1.P_con.micr1, .~. -Chao1.comm.shells:LOCATION)
    model.Chao1.P_con.micr3<-update(model.Chao1.P_con.micr2, .~. -Chao1.comm.shells)
    model.Chao1.P_con.micr4<-update(model.Chao1.P_con.micr2, .~. -LOCATION)
    model.Chao1.P_con.micr5<-update(model.Chao1.P_con.micr2, .~. -LOCATION -Chao1.comm.shells)
    #We compare these models and find confirmation that model 1, including the effect of the Chao1.comm.shells performs best, following AIC.
    anova(model.Chao1.P_con.micr1,model.Chao1.P_con.micr2,model.Chao1.P_con.micr3,model.Chao1.P_con.micr4,model.Chao1.P_con.micr5)
    #Which shows that for P. concinnum, the complete model scores best on AIC.



#### STEP 7: PLS-PM PATH MODELLING ####
  #Following Sanchez (2013); reference to specific chapters and paragraphs given in comments.
  
  #Because we will try to correlate microbiome and diet, we are now only interested in samples for which we have data on both.
  intersect<-unlist(intersect(sample_names(microbiome.ph),sample_names(diet.ph)))
  #The number of samples for which we have both data, is:
  length(intersect)
  
  #STEP 7A: CORE MODEL.
  #In this first model, we compare microbiome and diet data to the time-independent community data, i.e. data from shells.
  #Here, we assume that host community composition, microbiome and diet do not change with the timescale of a decade, and are not under the influence of environmental factors.
  #Note in all cases below that when bootstrapping is involved, values as published may differ from results from rerunning the code!
  
  #The inner model, defined by three latent variables and their antipicated relations, is as follows.
  consumer<-c(0,0,0)
  microbiome<-c(1,0,0)
  diet<-c(1,1,0)
  plspm_path<-rbind(consumer,microbiome,diet)
  colnames(plspm_path)<-rownames(plspm_path)
  plspm_path
  
  #And view plot as a check.
  innerplot(plspm_path)
  
  #Associate indicators (i.e. metric data representing the microbiome, diet, and consumer) with the latent variables.
  plspm_blocks<-list(c(25,26,28,29),c(9,10,12,13),c(17,18,20,21))
  plspm_modes<-rep("A",length(plspm_blocks))
  plspm_scaling<-lapply(plspm_blocks, function(x) rep("NUM",length(x)))
  
  #Run the model.
  plspm_model_core<-plspm(Data = data[data$SAMPLE_ID %in% intersect,],
                          path_matrix = plspm_path,
                          blocks = plspm_blocks, 
                          modes = plspm_modes,
                          scaling = plspm_scaling)
  
  #Model assessment.
  #Check for unidimensionality (§4.3.2).
  plspm_model_core$unidim
  #Not all C.alpha and DG.rho>0.7; first check the loadings of the outer model!
  
  #Plot the loadings to see what happens in each case.
  plot(plspm_model_core, what="loadings")
  
  #Shannon evenness appears inversely loaded w.r.t. the other variables and we better to take the negative.
  data$ShannonEvenness.micr<-ifelse(data$ShannonEvenness.micr>0,-1*data$ShannonEvenness.micr,data$ShannonEvenness.micr)
  data$ShannonEvenness.diet<-ifelse(data$ShannonEvenness.diet>0,-1*data$ShannonEvenness.diet,data$ShannonEvenness.diet)
  data$ShannonEvenness.comm.shells<-ifelse(data$ShannonEvenness.comm.shells>0,-1*data$ShannonEvenness.comm.shells,data$ShannonEvenness.comm.shells)
  data$ShannonEvenness.comm.live<-ifelse(data$ShannonEvenness.comm.live>0,-1*data$ShannonEvenness.comm.live,data$ShannonEvenness.comm.live)
  
  #Rerun the model.
  plspm_model_core<-plspm(Data = data[data$SAMPLE_ID %in% intersect,],
                          path_matrix = plspm_path,
                          blocks = plspm_blocks, 
                          modes = plspm_modes,
                          scaling = plspm_scaling)
  
  #Model assessment.
  #Check for unidimensionality (§4.3.2).
  plspm_model_core$unidim
  #All C.alpha and DG.rho>0.7 thus okay; all eig.1st > 1, and most eig.2nd < 1.
  
  #Plot the loadings to see what happens in each case.
  plot(plspm_model_core, what="loadings")
  #All loadings are now of the same (positive) sign.
  #Although some loadings are weak (< 0.7) we leave the associated variables (PD.comm.shells, ShannonEvenness.diet) in for the sake of completeness.
  
  #Check the loading & communality; values should be >0.7 and >0.5, resp. (§4.3.3).
  plspm_model_core$outer_model
  #Loadings are > 0.7 and communalities > 0.5, in general, which looks good.
  
  #Check cross-loadings (§4.3.4).
  plspm_model_core$crossloadings
  #This looks okay: loadings for own LV are always much higher than for others. 
  
  #Inspect coefficient of determination (R2) (§4.5.1).
  plspm_model_core$inner_summary
  #R2 values indicate te amount of variance in each endogenous LV explained by the exogenous LVs, which here is low.
  
  #Inspect redundancy (§4.5.2).
  plspm_model_core$inner_summary
  #The consumer explains 3.4% of the variance in the microbiome; the community and microbiome together explain 2.5% of the diet.
  
  #Inspect Goodness of Fit (GoF) of the model (§4.5.3).
  plspm_model_core$gof
  #The "prediction power" of the model is 17.2%. 
  #This is low, which indicates low support for the full model, even though individuals parts can still be valuable and informative.
  #(Values of 70% and over are considered "good", but there is no single objective threshold and models cannot simply be compared based on this value!).
  
  #To get an idea of the significance of our results, we shall rerun the analysis 999 times, using bootstraps of 300 random samples.
  plspm_model_core_complete_shells<-list()
  #Rerun the model and save the results.
  for (n in 1:999){
    #Run the model.
    plspm_model_core_complete_shells<-c(plspm_model_core_complete_shells,
                             list(plspm(Data = data[sample(which(data$SAMPLE_ID %in% intersect),300),],
                                        path_matrix = plspm_path,
                                        blocks = plspm_blocks, 
                                        modes = plspm_modes,
                                        scaling = plspm_scaling)))
  }
  #We collect the results for the mean, 2.5% and 97.5% quantiles.
  X<-lapply(plspm_model_core_complete_shells, function(x) x$path_coefs)
  Y<-do.call(cbind, X)
  Y<-array(Y, dim=c(dim(X[[1]]), length(X)))
  mean<-apply(Y, c(1, 2), mean)  
  quantile0.025<-apply(Y, c(1, 2), function(x) quantile(x,0.025))  
  quantile0.975<-apply(Y, c(1, 2), function(x) quantile(x,0.975))  
  rownames(mean)=rownames(quantile0.025)=rownames(quantile0.975)<-rownames(plspm_model_core_complete_shells[[1]]$path_coefs)
  colnames(mean)=colnames(quantile0.025)=colnames(quantile0.975)<-colnames(plspm_model_core_complete_shells[[1]]$path_coefs)
  #And put these results in a long format dataframe for ease of tabulating later.
  plspm_model_core_complete_shells_results<-melt(mean)
  plspm_model_core_complete_shells_results$paths<-paste0(plspm_model_core_complete_shells_results[,2], " -> ",plspm_model_core_complete_shells_results[,1])
  plspm_model_core_complete_shells_results<-cbind(plspm_model_core_complete_shells_results,melt(quantile0.025)[,3],melt(quantile0.975)[,3])
  #Re-order; remove zero entries of paths not considered.
  plspm_model_core_complete_shells_results<-plspm_model_core_complete_shells_results[plspm_model_core_complete_shells_results[,3]!=0,c(4,3,5,6)]
  colnames(plspm_model_core_complete_shells_results)<-c("paths","Mean.Boot","perc.025","perc.975")
  plspm_model_core_complete_shells_results
  #And the average and quantiles for the gof are as follows.
  plspm_model_core_complete_shells_gof<-NULL
  plspm_model_core_complete_shells_gof$mean<-mean(unlist(lapply(plspm_model_core_complete_shells, function(x) x$gof)))
  plspm_model_core_complete_shells_gof$quantile0.025<-quantile(unlist(lapply(plspm_model_core_complete_shells, function(x) x$gof)),0.025)
  plspm_model_core_complete_shells_gof$quantile0.975<-quantile(unlist(lapply(plspm_model_core_complete_shells, function(x) x$gof)),0.975)
  #Stay clean!
  rm(X, Y, mean, quantile0.025, quantile0.975)
  
  #The species are not distributed evenly over the dataset, and it may be possible that some species (mainly P. concinnum) determine much of the pattern we find.
  #We normalize our dataset by subsetting to equal sample numbers per species.
  #We use only species for which we have many (>=100) samples, then rerun the analysis 999 times with an evenly distributed sampling of 100 ind./species.
  table(data$GENUS_SPECIES[data$SAMPLE_ID %in% intersect])
  #So, these are 3 species, i.e. the target species:
  (species<-names(table(data$GENUS_SPECIES[data$SAMPLE_ID %in% intersect])[(table(data$GENUS_SPECIES[data$SAMPLE_ID %in% intersect])>=100)]))
  
  plspm_model_core_norm_shells<-list()
  #Rerun the model and save the results.
  for (n in 1:999){
    #Define a subset of samples from these species, based on sampling 10 ind./species.
    subset<-NULL
    for (i in 1:length(species)){subset<-c(subset,sample(which(data[data$SAMPLE_ID %in% intersect, ]$GENUS_SPECIES==species[i]),100))}
    table(data[data$SAMPLE_ID %in% intersect, ]$GENUS_SPECIES[subset])
    #Run the model.
    plspm_model_core_norm_shells<-c(plspm_model_core_norm_shells,
                                    list(plspm(Data = data[data$SAMPLE_ID %in% intersect,][subset,],
                                               path_matrix = plspm_path,
                                               blocks = plspm_blocks, 
                                               modes = plspm_modes,
                                               scaling = plspm_scaling)))
  }
  #We collect the results for the mean, 2.5% and 97.5% quantiles.
  X<-lapply(plspm_model_core_norm_shells, function(x) x$path_coefs)
  Y<-do.call(cbind, X)
  Y<-array(Y, dim=c(dim(X[[1]]), length(X)))
  mean<-apply(Y, c(1, 2), mean)  
  quantile0.025<-apply(Y, c(1, 2), function(x) quantile(x,0.025))  
  quantile0.975<-apply(Y, c(1, 2), function(x) quantile(x,0.975))  
  rownames(mean)=rownames(quantile0.025)=rownames(quantile0.975)<-rownames(plspm_model_core_norm_shells[[1]]$path_coefs)
  colnames(mean)=colnames(quantile0.025)=colnames(quantile0.975)<-colnames(plspm_model_core_norm_shells[[1]]$path_coefs)
  #And put these results in a long format dataframe for ease of tabulating later.
  plspm_model_core_norm_shells_results<-melt(mean)
  plspm_model_core_norm_shells_results$paths<-paste0(plspm_model_core_norm_shells_results[,2], " -> ",plspm_model_core_norm_shells_results[,1])
  plspm_model_core_norm_shells_results<-cbind(plspm_model_core_norm_shells_results,melt(quantile0.025)[,3],melt(quantile0.975)[,3])
  #Re-order; remove zero entries of paths not considered.
  plspm_model_core_norm_shells_results<-plspm_model_core_norm_shells_results[plspm_model_core_norm_shells_results[,3]!=0,c(4,3,5,6)]
  colnames(plspm_model_core_norm_shells_results)<-c("paths","Mean.Boot","perc.025","perc.975")
  plspm_model_core_norm_shells_results
  #And the average and quantiles for the gof are as follows.
  plspm_model_core_norm_shells_gof<-NULL
  plspm_model_core_norm_shells_gof$mean<-mean(unlist(lapply(plspm_model_core_norm_shells, function(x) x$gof)))
  plspm_model_core_norm_shells_gof$quantile0.025<-quantile(unlist(lapply(plspm_model_core_norm_shells, function(x) x$gof)),0.025)
  plspm_model_core_norm_shells_gof$quantile0.975<-quantile(unlist(lapply(plspm_model_core_norm_shells, function(x) x$gof)),0.975)
  #Stay clean!
  rm(X, Y, mean, quantile0.025, quantile0.975)
  
  #So far, we have chosen to study the represent the communty of consumers by the dataset of shells found from the soil.
  #It may be true that the live snails collected for diet and microbiome analyses are in fact a better proxy, so let's study these, too.
  
  #Update associated indicators (i.e. metric data representing the microbiome, diet, and consumer) with the latent variables.
  plspm_blocks<-list(c(33,34,36,37),c(9,10,12,13),c(17,18,20,21))
  plspm_modes<-rep("A",length(plspm_blocks))
  plspm_scaling<-lapply(plspm_blocks, function(x) rep("NUM",length(x)))
  
  plspm_model_core_complete_live<-list()
  #Rerun the model and save the results.
  for (n in 1:999){
    #Run the model.
    plspm_model_core_complete_live<-c(plspm_model_core_complete_live,
                                        list(plspm(Data = data[sample(which(data$SAMPLE_ID %in% intersect),300),],
                                                   path_matrix = plspm_path,
                                                   blocks = plspm_blocks, 
                                                   modes = plspm_modes,
                                                   scaling = plspm_scaling)))
  }
  #We collect the results for the mean, 2.5% and 97.5% quantiles.
  X<-lapply(plspm_model_core_complete_live, function(x) x$path_coefs)
  Y<-do.call(cbind, X)
  Y<-array(Y, dim=c(dim(X[[1]]), length(X)))
  mean<-apply(Y, c(1, 2), mean)  
  quantile0.025<-apply(Y, c(1, 2), function(x) quantile(x,0.025))  
  quantile0.975<-apply(Y, c(1, 2), function(x) quantile(x,0.975))  
  rownames(mean)=rownames(quantile0.025)=rownames(quantile0.975)<-rownames(plspm_model_core_complete_live[[1]]$path_coefs)
  colnames(mean)=colnames(quantile0.025)=colnames(quantile0.975)<-colnames(plspm_model_core_complete_live[[1]]$path_coefs)
  #And put these results in a long format dataframe for ease of tabulating later.
  plspm_model_core_complete_live_results<-melt(mean)
  plspm_model_core_complete_live_results$paths<-paste0(plspm_model_core_complete_live_results[,2], " -> ",plspm_model_core_complete_live_results[,1])
  plspm_model_core_complete_live_results<-cbind(plspm_model_core_complete_live_results,melt(quantile0.025)[,3],melt(quantile0.975)[,3])
  #Re-order; remove zero entries of paths not considered.
  plspm_model_core_complete_live_results<-plspm_model_core_complete_live_results[plspm_model_core_complete_live_results[,3]!=0,c(4,3,5,6)]
  colnames(plspm_model_core_complete_live_results)<-c("paths","Mean.Boot","perc.025","perc.975")
  plspm_model_core_complete_live_results
  #And the average and quantiles for the gof are as follows.
  plspm_model_core_complete_live_gof<-NULL
  plspm_model_core_complete_live_gof$mean<-mean(unlist(lapply(plspm_model_core_complete_live, function(x) x$gof)))
  plspm_model_core_complete_live_gof$quantile0.025<-quantile(unlist(lapply(plspm_model_core_complete_live, function(x) x$gof)),0.025)
  plspm_model_core_complete_live_gof$quantile0.975<-quantile(unlist(lapply(plspm_model_core_complete_live, function(x) x$gof)),0.975)
  #Stay clean!
  rm(X, Y, mean, quantile0.025, quantile0.975)
  
  #And also for the normalized dataset.
  plspm_model_core_norm_live<-list()
  #Rerun the model and save the results.
  for (n in 1:999){
    #Define a subset of samples from these species, based on sampling 10 ind./species.
    subset<-NULL
    for (i in 1:length(species)){subset<-c(subset,sample(which(data[data$SAMPLE_ID %in% intersect, ]$GENUS_SPECIES==species[i]),100))}
    table(data[data$SAMPLE_ID %in% intersect, ]$GENUS_SPECIES[subset])
    #Run the model.
    plspm_model_core_norm_live<-c(plspm_model_core_norm_live,
                                    list(plspm(Data = data[data$SAMPLE_ID %in% intersect,][subset,],
                                               path_matrix = plspm_path,
                                               blocks = plspm_blocks, 
                                               modes = plspm_modes,
                                               scaling = plspm_scaling)))
  }
  #We collect the results for the mean, 2.5% and 97.5% quantiles.
  X<-lapply(plspm_model_core_norm_live, function(x) x$path_coefs)
  Y<-do.call(cbind, X)
  Y<-array(Y, dim=c(dim(X[[1]]), length(X)))
  mean<-apply(Y, c(1, 2), mean)  
  quantile0.025<-apply(Y, c(1, 2), function(x) quantile(x,0.025))  
  quantile0.975<-apply(Y, c(1, 2), function(x) quantile(x,0.975))  
  rownames(mean)=rownames(quantile0.025)=rownames(quantile0.975)<-rownames(plspm_model_core_norm_live[[1]]$path_coefs)
  colnames(mean)=colnames(quantile0.025)=colnames(quantile0.975)<-colnames(plspm_model_core_norm_live[[1]]$path_coefs)
  #And put these results in a long format dataframe for ease of tabulating later.
  plspm_model_core_norm_live_results<-melt(mean)
  plspm_model_core_norm_live_results$paths<-paste0(plspm_model_core_norm_live_results[,2], " -> ",plspm_model_core_norm_live_results[,1])
  plspm_model_core_norm_live_results<-cbind(plspm_model_core_norm_live_results,melt(quantile0.025)[,3],melt(quantile0.975)[,3])
  #Re-order; remove zero entries of paths not considered.
  plspm_model_core_norm_live_results<-plspm_model_core_norm_live_results[plspm_model_core_norm_live_results[,3]!=0,c(4,3,5,6)]
  colnames(plspm_model_core_norm_live_results)<-c("paths","Mean.Boot","perc.025","perc.975")
  plspm_model_core_norm_live_results
  #And the average and quantiles for the gof are as follows.
  plspm_model_core_norm_live_gof<-NULL
  plspm_model_core_norm_live_gof$mean<-mean(unlist(lapply(plspm_model_core_norm_live, function(x) x$gof)))
  plspm_model_core_norm_live_gof$quantile0.025<-quantile(unlist(lapply(plspm_model_core_norm_live, function(x) x$gof)),0.025)
  plspm_model_core_norm_live_gof$quantile0.975<-quantile(unlist(lapply(plspm_model_core_norm_live, function(x) x$gof)),0.975)
  #Stay clean!
  rm(X, Y, mean, quantile0.025, quantile0.975)
  
  #STEP 7B: FULL MODEL.
  #In this model, we add environmental factors to the core model of STEP 7A: geology, flooding, anthropogenic activity, and humidity.
  #Because some of these are time-dependent (especially humidity), we also add community data based on live snails to the core model, to see whether these data correlate 
  #to the environmental factors better than community data based on shells, which we understand is less time-dependent (build-up over maybe a decade possible).
  
  #The inner model, defined by ten latent variables and their antipicated relations, is as follows.
  river.distance<-c(0,0,0,0,0,0,0,0,0)
  cave.distance<-c(0,0,0,0,0,0,0,0,0)
  island.size<-c(0,0,0,0,0,0,0,0,0)
  next.outcrop.distance<-c(0,0,0,0,0,0,0,0,0)
  anthropogenic.distance<-c(0,0,0,0,0,0,0,0,0)
  humidity<-c(0,0,0,0,0,0,0,0,0)
  consumer<-c(1,1,1,1,1,1,0,0,0)
  microbiome<-c(1,1,1,1,1,1,1,0,0)
  diet<-c(1,1,1,1,1,1,1,1,0)
  plspm_path<-rbind(river.distance,cave.distance,island.size,next.outcrop.distance,anthropogenic.distance,humidity,consumer,microbiome,diet)
  colnames(plspm_path)<-rownames(plspm_path)
  plspm_path
  
  #And view plot as a check.
  innerplot(plspm_path)
  
  #Associate indicators (i.e. metric data representing the microbiome, diet, and community) with the latent variables.
  plspm_blocks<-list(39:40,41,42,43,44:45,46:47,c(25,26,28,29),c(9,10,12,13),c(17,18,20,21))
  plspm_modes<-rep("A",length(plspm_blocks))
  plspm_scaling<-lapply(plspm_blocks, function(x) rep("NUM",length(x)))
  
  #Run the model.
  plspm_model_full<-plspm(Data = data[data$SAMPLE_ID %in% intersect,],
                          path_matrix = plspm_path,
                          blocks = plspm_blocks, 
                          modes = plspm_modes,
                          scaling = plspm_scaling)
  
  #Model assessment.
  #Check for unidimensionality (§4.3.2).
  plspm_model_full$unidim
  #Not all C.alpha and DG.rho>0.7; check the loadings of the outer model.
  
  #Plot the loadings to see what happens in each case.
  plot(plspm_model_full, what="loadings")
  
  #"Humidity level" and "altitude" appear inversely loaded w.r.t. the other variables and we need to take the negative.
  #We change the sign of the other construct of the LV, which helps interpretation of the results later (i.e. decrease in time.since.rain will correspond to increase in humidity).
  data$time.since.rain<-ifelse(data$time.since.rain>0,-1*data$time.since.rain,data$time.since.rain)
  data$altitude<-ifelse(data$altitude>0,-1*data$altitude,data$altitude)
  
  #Rerun the model.
  plspm_model_full<-plspm(Data = data[data$SAMPLE_ID %in% intersect,],
                          path_matrix = plspm_path,
                          blocks = plspm_blocks, 
                          modes = plspm_modes,
                          scaling = plspm_scaling)
  
  #Model assessment.
  #Check for unidimensionality (§4.3.2).
  plspm_model_full$unidim
  #Most C.alpha and all DG.rho > 0.7, thus okay; all eig.1st > 1, and most eig.2nd < 1.
  
  #Plot the loadings to see what happens in each case.
  plot(plspm_model_full, what="loadings")
  #All loadings are now of the same (positive) sign.
  #Although some loadings are weak (< 0.7) we leave the associated variables (altitude, PD.comm.shells, ShannonEvenness.diet) in for the sake of completeness.
  
  #Check the loading & communality; values should be >0.7 and >0.5, resp. (§4.3.3).
  plspm_model_full$outer_model
  #Loadings are > 0.7 with only few expections, communalities > 0.5, which looks good.
  
  #Check cross-loadings (§4.3.4).
  plspm_model_full$crossloadings
  #This looks okay: loadings for own LV are always much higher than for others. 
  
  #Inspect coefficient of determination (R2) (§4.5.1) & redundancy (§4.5.2).
  plspm_model_full$inner_summary
  #R2 values indicate the amount of variance in each LV explained by the independent LV.
  #We see here that more variation can be explained than in the core model, e.g. 32% in the community of shells.
  #Of the microbiome and diet, 12% and 9% can be explained by the exogenous LVs only.
  
  #Inspect Goodness of Fit (GoF) of the model (§4.5.3).
  plspm_model_full$gof
  #The "prediction power" of the model is now 37%.
  
  #Again, to get an idea of the significance of our results, we shall rerun the analysis 999 times, using bootstraps of 300 random samples.
  plspm_model_full_complete_shells<-list()
  #Rerun the model and save the results.
  for (n in 1:999){
    #Run the model.
    plspm_model_full_complete_shells<-c(plspm_model_full_complete_shells,
                                 list(plspm(Data = data[sample(which(data$SAMPLE_ID %in% intersect),300),],
                                            path_matrix = plspm_path,
                                            blocks = plspm_blocks, 
                                            modes = plspm_modes,
                                            scaling = plspm_scaling)))
  }
  #We collect the results for the mean, 2.5% and 97.5% quantiles.
  X<-lapply(plspm_model_full_complete_shells, function(x) x$path_coefs)
  Y<-do.call(cbind, X)
  Y<-array(Y, dim=c(dim(X[[1]]), length(X)))
  mean<-apply(Y, c(1, 2), mean)  
  quantile0.025<-apply(Y, c(1, 2), function(x) quantile(x,0.025))  
  quantile0.975<-apply(Y, c(1, 2), function(x) quantile(x,0.975))  
  rownames(mean)=rownames(quantile0.025)=rownames(quantile0.975)<-rownames(plspm_model_full_complete_shells[[1]]$path_coefs)
  colnames(mean)=colnames(quantile0.025)=colnames(quantile0.975)<-colnames(plspm_model_full_complete_shells[[1]]$path_coefs)
  #And put these results in a long format dataframe for ease of tabulating later.
  plspm_model_full_complete_shells_results<-melt(mean)
  plspm_model_full_complete_shells_results$paths<-paste0(plspm_model_full_complete_shells_results[,2], " -> ",plspm_model_full_complete_shells_results[,1])
  plspm_model_full_complete_shells_results<-cbind(plspm_model_full_complete_shells_results,melt(quantile0.025)[,3],melt(quantile0.975)[,3])
  #Re-order; remove zero entries of paths not considered.
  plspm_model_full_complete_shells_results<-plspm_model_full_complete_shells_results[plspm_model_full_complete_shells_results[,3]!=0,c(4,3,5,6)]
  colnames(plspm_model_full_complete_shells_results)<-c("paths","Mean.Boot","perc.025","perc.975")
  plspm_model_full_complete_shells_results
  #And the average and quantiles for the gof are as follows.
  plspm_model_full_complete_shells_gof<-NULL
  plspm_model_full_complete_shells_gof$mean<-mean(unlist(lapply(plspm_model_full_complete_shells, function(x) x$gof)))
  plspm_model_full_complete_shells_gof$quantile0.025<-quantile(unlist(lapply(plspm_model_full_complete_shells, function(x) x$gof)),0.025)
  plspm_model_full_complete_shells_gof$quantile0.975<-quantile(unlist(lapply(plspm_model_full_complete_shells, function(x) x$gof)),0.975)
  #Stay clean!
  rm(X, Y, mean, quantile0.025, quantile0.975)
  
  #Once again, to correct for uneven sampling, we normalize the data and rerun the above model.
  plspm_model_full_norm_shells<-list()
  #Rerun the model and save the results.
  for (n in 1:999){
    #Define a subset of samples from these species, based on sampling 10 ind./species.
    subset<-NULL
    for (i in 1:length(species)){subset<-c(subset,sample(which(data[data$SAMPLE_ID %in% intersect, ]$GENUS_SPECIES==species[i]),100))}
    table(data$GENUS_SPECIES[subset])
    #Run the model.
    plspm_model_full_norm_shells<-c(plspm_model_full_norm_shells,
                             list(plspm(Data = data[data$SAMPLE_ID %in% intersect,][subset,],
                                        path_matrix = plspm_path,
                                        blocks = plspm_blocks, 
                                        modes = plspm_modes,
                                        scaling = plspm_scaling)))
  }
  #We collect the results for the mean, 2.5% and 97.5% quantiles.
  X<-lapply(plspm_model_full_norm_shells, function(x) x$path_coefs)
  Y<-do.call(cbind, X)
  Y<-array(Y, dim=c(dim(X[[1]]), length(X)))
  mean<-apply(Y, c(1, 2), mean)  
  quantile0.025<-apply(Y, c(1, 2), function(x) quantile(x,0.025))  
  quantile0.975<-apply(Y, c(1, 2), function(x) quantile(x,0.975))  
  rownames(mean)=rownames(quantile0.025)=rownames(quantile0.975)<-rownames(plspm_model_full_norm_shells[[1]]$path_coefs)
  colnames(mean)=colnames(quantile0.025)=colnames(quantile0.975)<-colnames(plspm_model_full_norm_shells[[1]]$path_coefs)
  #And put these results in a long format dataframe for ease of tabulating later.
  plspm_model_full_norm_shells_results<-melt(mean)
  plspm_model_full_norm_shells_results$paths<-paste0(plspm_model_full_norm_shells_results[,2], " -> ",plspm_model_full_norm_shells_results[,1])
  plspm_model_full_norm_shells_results<-cbind(plspm_model_full_norm_shells_results,melt(quantile0.025)[,3],melt(quantile0.975)[,3])
  #Re-order; remove zero entries of paths not considered.
  plspm_model_full_norm_shells_results<-plspm_model_full_norm_shells_results[plspm_model_full_norm_shells_results[,3]!=0,c(4,3,5,6)]
  colnames(plspm_model_full_norm_shells_results)<-c("paths","Mean.Boot","perc.025","perc.975")
  plspm_model_full_norm_shells_results
  #And the average and quantiles for the gof are as follows.
  plspm_model_full_norm_shells_gof<-NULL
  plspm_model_full_norm_shells_gof$mean<-mean(unlist(lapply(plspm_model_full_norm_shells, function(x) x$gof)))
  plspm_model_full_norm_shells_gof$quantile0.025<-quantile(unlist(lapply(plspm_model_full_norm_shells, function(x) x$gof)),0.025)
  plspm_model_full_norm_shells_gof$quantile0.975<-quantile(unlist(lapply(plspm_model_full_norm_shells, function(x) x$gof)),0.975)
  #Stay clean!
  rm(X, Y, mean, quantile0.025, quantile0.975)
  
  #And as for the core models, we rerun these full models with live snail data as a proxy for the consumer community instead.
  
  #Associate indicators (i.e. metric data representing the microbiome, diet, and community) with the latent variables.
  plspm_blocks<-list(39:40,41,42,43,44:45,46:47,c(33,34,36,37),c(9,10,12,13),c(17,18,20,21))
  plspm_modes<-rep("A",length(plspm_blocks))
  plspm_scaling<-lapply(plspm_blocks, function(x) rep("NUM",length(x)))
  
  #Again, to get an idea of the significance of our results, we shall rerun the analysis 999 times, using bootstraps of 300 random samples.
  plspm_model_full_complete_live<-list()
  #Rerun the model and save the results.
  for (n in 1:999){
    #Run the model.
    plspm_model_full_complete_live<-c(plspm_model_full_complete_live,
                                        list(plspm(Data = data[sample(which(data$SAMPLE_ID %in% intersect),300),],
                                                   path_matrix = plspm_path,
                                                   blocks = plspm_blocks, 
                                                   modes = plspm_modes,
                                                   scaling = plspm_scaling)))
  }
  #We collect the results for the mean, 2.5% and 97.5% quantiles.
  X<-lapply(plspm_model_full_complete_live, function(x) x$path_coefs)
  Y<-do.call(cbind, X)
  Y<-array(Y, dim=c(dim(X[[1]]), length(X)))
  mean<-apply(Y, c(1, 2), mean)  
  quantile0.025<-apply(Y, c(1, 2), function(x) quantile(x,0.025))  
  quantile0.975<-apply(Y, c(1, 2), function(x) quantile(x,0.975))  
  rownames(mean)=rownames(quantile0.025)=rownames(quantile0.975)<-rownames(plspm_model_full_complete_live[[1]]$path_coefs)
  colnames(mean)=colnames(quantile0.025)=colnames(quantile0.975)<-colnames(plspm_model_full_complete_live[[1]]$path_coefs)
  #And put these results in a long format dataframe for ease of tabulating later.
  plspm_model_full_complete_live_results<-melt(mean)
  plspm_model_full_complete_live_results$paths<-paste0(plspm_model_full_complete_live_results[,2], " -> ",plspm_model_full_complete_live_results[,1])
  plspm_model_full_complete_live_results<-cbind(plspm_model_full_complete_live_results,melt(quantile0.025)[,3],melt(quantile0.975)[,3])
  #Re-order; remove zero entries of paths not considered.
  plspm_model_full_complete_live_results<-plspm_model_full_complete_live_results[plspm_model_full_complete_live_results[,3]!=0,c(4,3,5,6)]
  colnames(plspm_model_full_complete_live_results)<-c("paths","Mean.Boot","perc.025","perc.975")
  plspm_model_full_complete_live_results
  #And the average and quantiles for the gof are as follows.
  plspm_model_full_complete_live_gof<-NULL
  plspm_model_full_complete_live_gof$mean<-mean(unlist(lapply(plspm_model_full_complete_live, function(x) x$gof)))
  plspm_model_full_complete_live_gof$quantile0.025<-quantile(unlist(lapply(plspm_model_full_complete_live, function(x) x$gof)),0.025)
  plspm_model_full_complete_live_gof$quantile0.975<-quantile(unlist(lapply(plspm_model_full_complete_live, function(x) x$gof)),0.975)
  #Stay clean!
  rm(X, Y, mean, quantile0.025, quantile0.975)
  
  #Once again, to correct for uneven sampling, we normalize the data and rerun the above model.
  plspm_model_full_norm_live<-list()
  #Rerun the model and save the results.
  for (n in 1:999){
    #Define a subset of samples from these species, based on sampling 10 ind./species.
    subset<-NULL
    for (i in 1:length(species)){subset<-c(subset,sample(which(data[data$SAMPLE_ID %in% intersect, ]$GENUS_SPECIES==species[i]),100))}
    table(data$GENUS_SPECIES[subset])
    #Run the model.
    plspm_model_full_norm_live<-c(plspm_model_full_norm_live,
                                    list(plspm(Data = data[data$SAMPLE_ID %in% intersect,][subset,],
                                               path_matrix = plspm_path,
                                               blocks = plspm_blocks, 
                                               modes = plspm_modes,
                                               scaling = plspm_scaling)))
  }
  #We collect the results for the mean, 2.5% and 97.5% quantiles.
  X<-lapply(plspm_model_full_norm_live, function(x) x$path_coefs)
  Y<-do.call(cbind, X)
  Y<-array(Y, dim=c(dim(X[[1]]), length(X)))
  mean<-apply(Y, c(1, 2), mean)  
  quantile0.025<-apply(Y, c(1, 2), function(x) quantile(x,0.025))  
  quantile0.975<-apply(Y, c(1, 2), function(x) quantile(x,0.975))  
  rownames(mean)=rownames(quantile0.025)=rownames(quantile0.975)<-rownames(plspm_model_full_norm_live[[1]]$path_coefs)
  colnames(mean)=colnames(quantile0.025)=colnames(quantile0.975)<-colnames(plspm_model_full_norm_live[[1]]$path_coefs)
  #And put these results in a long format dataframe for ease of tabulating later.
  plspm_model_full_norm_live_results<-melt(mean)
  plspm_model_full_norm_live_results$paths<-paste0(plspm_model_full_norm_live_results[,2], " -> ",plspm_model_full_norm_live_results[,1])
  plspm_model_full_norm_live_results<-cbind(plspm_model_full_norm_live_results,melt(quantile0.025)[,3],melt(quantile0.975)[,3])
  #Re-order; remove zero entries of paths not considered.
  plspm_model_full_norm_live_results<-plspm_model_full_norm_live_results[plspm_model_full_norm_live_results[,3]!=0,c(4,3,5,6)]
  colnames(plspm_model_full_norm_live_results)<-c("paths","Mean.Boot","perc.025","perc.975")
  plspm_model_full_norm_live_results
  #And the average and quantiles for the gof are as follows.
  plspm_model_full_norm_live_gof<-NULL
  plspm_model_full_norm_live_gof$mean<-mean(unlist(lapply(plspm_model_full_norm_live, function(x) x$gof)))
  plspm_model_full_norm_live_gof$quantile0.025<-quantile(unlist(lapply(plspm_model_full_norm_live, function(x) x$gof)),0.025)
  plspm_model_full_norm_live_gof$quantile0.975<-quantile(unlist(lapply(plspm_model_full_norm_live, function(x) x$gof)),0.975)
  #Stay clean!
  rm(X, Y, mean, quantile0.025, quantile0.975)



#### STEP 8: SAVE ALL OF THE CREATED OBJECTS FOR SHARING WITH PUBLICATION ####
save(list=ls(), file="saved_R_objects/Hendriks_Bisschop_et_al_2019.rda")


  