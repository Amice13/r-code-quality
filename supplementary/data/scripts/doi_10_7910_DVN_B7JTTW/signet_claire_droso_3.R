library(signet)
library(graphite)
library(org.Dm.eg.db)
library(UniProt.ws)
library(magrittr)


########################################################databases
#drosophila
#take correspondance flybase to uniprot
columns(org.Dm.eg.db)
flybase <- Rkeys(org.Dm.egFLYBASECG)
fly_to_uni<-select(org.Dm.eg.db, flybase, "UNIPROT","FLYBASECG")

######################################################## prepare correspondance transcript/uniprot/flybase
library(dplyr)
library(splitstackshape)
library(data.table)
#in Bash
#cat 04_annotation/transcriptome.hits | awk -F"|" '$1=$1' OFS="\t" | cut -f1,2 > 04_annotation/transcriptome_hits.formatted
#cat 05_results/transcriptome_annotation_table.tsv | cut -f1,2 > 05_results/transcripts_annot_uniprot_swissprot.txt
transcript_annot<-read.table("transcriptome_hits_Dmel.formatted", header=F)
head(transcript_annot)
transcript_uniprot<-transcript_annot[,c(1,3)]
colnames(transcript_uniprot)<-c("id_transcript","uniprot")
head(transcript_uniprot)

#format
annot_split = cSplit(transcript_uniprot, "uniprot", sep = " ")
terms = colnames(select(annot_split, contains("uniprot")))
annot_long = melt(annot_split,measure.vars=terms, id.vars = "id_transcript", na.rm=TRUE)
head(annot_long)
annot_ready = as.data.frame(annot_long[,c(1, 3)])
head(annot_ready)
tail(annot_ready)
write.table(annot_ready, "transcripts_annot_uniprot_Dmel_simple.txt", sep="\t", quote=F, row.names=F)

#load the list of transcript with flybase annotation (from transcriptome folder)
annot_simple_flybase<-read.table("correspondance_transcript_uniprot_flybase.txt", header=T)
head(annot_simple_flybase)
#load the position of the genes
genome_annotation<-read.table("genome_annotation_emma_corrected.bed")
head(genome_annotation)
colnames(genome_annotation)<-c("LG","start","stop","id_transcript")

annot_simple_flybase_position<-left_join(annot_simple_flybase,genome_annotation)
head(annot_simple_flybase_position)

write.table(annot_simple_flybase_position, "transcript_flybase_in_genome.txt", row.names=F, quote=F, sep="\t")


#annotate transcript with flybase id
annot_simple<-read.table("transcripts_annot_uniprot_Dmel_simple.txt", header=T)
head(annot_simple)
colnames(annot_simple)[2]<-"UNIPROT"

annot_simple_flybase<-inner_join(annot_simple, fly_to_uni)
head(annot_simple_flybase)
dim(annot_simple)
dim(fly_to_uni)
dim(annot_simple_flybase)
write.table(annot_simple_flybase, "correspondance_transcript_uniprot_flybase.txt", row.names=F, quote=F, sep="\t")

#############################################annotate the RDA and simplify it
#annotate the RDA
DErda<-read.table("GENO_SEX_scores_blind_Adults.txt", header=T)
DErda$id_transcript<-row.names(DErda)
head(DErda)
dim(DErda)
DErda_annot_fly<-inner_join(DErda, annot_simple_flybase)
head(DErda_annot_fly)
dim(DErda_annot_fly)
DErda_annot_uniprot<-inner_join(DErda, annot_simple)
head(DErda_annot_uniprot)

#average the score by flybase gene
DErda_annot_fly_unique<-as.data.frame(DErda_annot_fly %>% group_by(FLYBASECG)%>%summarise(RDA1_mean=mean(RDA1), RDA2_mean=mean(RDA2)))
DErda_annot_fly_unique$z_geno<-scale(DErda_annot_fly_unique$RDA2_mean, center = TRUE, scale = TRUE)
DErda_annot_fly_unique$z_geno_opp<--scale(DErda_annot_fly_unique$RDA2_mean, center = TRUE, scale = TRUE)
DErda_annot_fly_unique$z_sex<-scale(DErda_annot_fly_unique$RDA1_mean, center = TRUE, scale = TRUE)
DErda_annot_fly_unique$z_sex_opp<-scale(DErda_annot_fly_unique$RDA1_mean, center = TRUE, scale = TRUE)
head(DErda_annot_fly_unique)
dim(DErda)
dim(DErda_annot_fly_unique)
write.table(DErda_annot_fly_unique, "DErda_annot_droso_fly.txt", row.names=F, sep="\t", quote=F)


#average the score by uniprot gene
DErda_annot_uniprot_unique<-as.data.frame(DErda_annot_uniprot %>% group_by(UNIPROT)%>%summarise(RDA1_mean=mean(RDA1), RDA2_mean=mean(RDA2)))
DErda_annot_uniprot_unique$z_geno<-scale(DErda_annot_uniprot_unique$RDA2_mean, center = TRUE, scale = TRUE)
DErda_annot_uniprot_unique$z_geno_opp<--scale(DErda_annot_uniprot_unique$RDA2_mean, center = TRUE, scale = TRUE)
DErda_annot_uniprot_unique$z_sex<-scale(DErda_annot_uniprot_unique$RDA1_mean, center = TRUE, scale = TRUE)
DErda_annot_uniprot_unique$z_sex_opp<-scale(DErda_annot_uniprot_unique$RDA1_mean, center = TRUE, scale = TRUE)
head(DErda_annot_uniprot_unique)
dim(DErda)
dim(DErda_annot_uniprot_unique)
write.table(DErda_annot_uniprot_unique, "DErda_annot_droso_uniprot.txt", row.names=F, sep="\t", quote=F)

#annotate the RDA with geno only
DErda<-read.table("GENO_scores_blind_Larvae_3G.txt", header=T)
DErda$id_transcript<-row.names(DErda)
head(DErda)
DErda_annot_fly<-inner_join(DErda, annot_simple_flybase)
head(DErda_annot_fly)
DErda_annot_uniprot<-inner_join(DErda, annot_simple)
head(DErda_annot_uniprot)
#average the score by flybase gene
DErda_annot_fly_unique<-as.data.frame(DErda_annot_fly %>% group_by(FLYBASECG)%>%summarise(RDA1_mean=mean(RDA1)))
DErda_annot_fly_unique$z_geno<-scale(DErda_annot_fly_unique$RDA1_mean, center = TRUE, scale = TRUE)
DErda_annot_fly_unique$z_geno_opp<--scale(DErda_annot_fly_unique$RDA1_mean, center = TRUE, scale = TRUE)
head(DErda_annot_fly_unique)
dim(DErda)
dim(DErda_annot_fly_unique)
write.table(DErda_annot_fly_unique, "DErda_annot_droso_fly_larvae_3G.txt", row.names=F, sep="\t", quote=F)
#average the score by uniprot gene
DErda_annot_uniprot_unique<-as.data.frame(DErda_annot_uniprot %>% group_by(UNIPROT)%>%summarise(RDA1_mean=mean(RDA1)))
DErda_annot_uniprot_unique$z_geno<-scale(DErda_annot_uniprot_unique$RDA1_mean, center = TRUE, scale = TRUE)
DErda_annot_uniprot_unique$z_geno_opp<--scale(DErda_annot_uniprot_unique$RDA1_mean, center = TRUE, scale = TRUE)
head(DErda_annot_uniprot_unique)
dim(DErda)
dim(DErda_annot_uniprot_unique)
write.table(DErda_annot_uniprot_unique, "DErda_annot_droso_uniprot_larvae_3G.txt", row.names=F, sep="\t", quote=F)



#pick a unique transcript? I don't know how it choses which one to keep
#DErda_annot_fly_unique<-DErda_annot_fly %>% distinct(id_transcript,.keep_all = TRUE)


######################################################### GRAPHITE
pathwayDatabases() #to check pathways and species available

#########KEGG
paths_droso <- pathways("dmelanogaster", "kegg")
pathway_droso <- lapply(paths_droso, pathwayGraph) #apply the conversion to graph to the list of "pathways" object (graphNEL graph)
#this is to remove the empty pathways that make a bug
dim_pathway<-vector(length=length(pathway_droso))
for (i in 1:length(pathway_droso)){ dim_pathway[i]<-length(pathway_droso[[i]]@edgeL)}
pathway_droso_ok <- lapply(paths_droso[dim_pathway>0], pathwayGraph) #apply the conversion to graph to the list of "pathways" object (graphNEL graph)

######### REACTOME
paths_droso_reactome <- pathways("dmelanogaster", "reactome")
pathway_droso_reactome <- lapply(paths_droso_reactome, pathwayGraph) #apply the conversion to graph to the list of "pathways" object (graphNEL graph)
length(pathway_droso_reactome)
#this is to remove the empty pathways that make a bug
dim_pathway<-vector(length=length(pathway_droso_reactome))
for (i in 1:length(pathway_droso_reactome)){ dim_pathway[i]<-length(pathway_droso_reactome[[i]]@edgeL)}
pathway_droso_reactome_ok <- lapply(paths_droso_reactome[dim_pathway>0], pathwayGraph) #apply the conversion to graph to the list of "pathways" object (graphNEL graph)

#pathbank looks like a bigger database but many paathways are redundant
#paths_droso_pathbank <- pathways("dmelanogaster", "pathbank")
#pathway_droso_pathbank <- lapply(paths_droso_pathbank, pathwayGraph) #apply the conversion to graph to the list of "pathways" object (graphNEL graph)
#length(pathway_droso_pathbank)

#############################signet
#read input ready for flybase/KEGG
DErda_annot<-read.table("DErda_annot_droso_fly.txt", header=T)
head(DErda_annot)
signet_droso <- subset(DErda_annot, select=c("FLYBASECG", "z_geno"))
signet_droso[,1]<-paste0("FLYBASECG:",signet_droso[,1])
head(signet_droso)
# Apply the simulated annealing algorithm on pathways of your choice
HSS_droso <- searchSubnet(pathway_droso_ok, 
                          signet_droso, 
                          iterations = 1000)

# Generate the null distribution of the subnetworks scores
null_droso <- nullDist(pathway_droso_ok,
                       signet_droso,
                       n = 1000)

#read input ready for uniprot/reactome
DErda_annot<-read.table("DErda_annot_droso_uniprot.txt", header=T)
head(DErda_annot)
signet_droso <- subset(DErda_annot, select=c("UNIPROT", "z_geno"))
signet_droso[,1]<-paste0("UNIPROT:",signet_droso[,1])
head(signet_droso)
hist(signet_droso[,2], breaks=100)
mean(signet_droso[,2])
sd(signet_droso[,2])

# Apply the simulated annealing algorithm on pathways of your choice
HSS_droso <- searchSubnet(pathway_droso_reactome_ok, 
                          signet_droso, 
                         iterations = 1000)

# Generate the null distribution of the subnetworks scores
null_droso <- nullDist(pathway_droso_reactome_ok,
                       signet_droso,
                      n = 1000)


###############analyse output
HSS_test_droso <- testSubnet(HSS_droso, null_droso)

tab_droso <- summary(HSS_test_droso)
head(tab_droso)
tab_droso$pathway_id<-as.character(row.names(tab_droso))
tab_droso$p.val<-as.numeric(as.character(tab_droso$p.val))
tab_droso[which(tab_droso$p.val<=0.05),]
tab_droso[which(tab_droso$p.val<=0.10),]
tabp0.10<-tab_droso[which(tab_droso$p.val<=0.10),] %>% distinct(subnet.genes,.keep_all = TRUE)
tabp0.10[,1:5]
#tab_droso[which(tab_droso$pvalue>=0.95),]
hist(signet_droso$z_geno, breaks=100)
hist(null_droso, breaks=100)
min(null_droso, na.rm=T)
hist(as.numeric(as.character(tab_droso$subnet.score)), breaks=100)
min(as.numeric(as.character(tab_droso$subnet.score)), na.rm=T )


tabp0.025<-tab_droso[which(tab_droso$p.val<=0.025),] %>% distinct(subnet.genes,.keep_all = TRUE)
tabp0.025[,1:5]


tab_droso2<-tab_droso %>% distinct(subnet.genes,.keep_all = TRUE)
hist(as.numeric(as.character(tab_droso2$p.val)), breaks=100)
dim(tab_droso)
dim(tab_droso2)
#write the summary table
write.table(tab_droso2,
            file = "signet_geno_negatif_droso_reactome_unique.txt",
            sep = "\t",
            quote = FALSE,
            row.names = TRUE)

#export all pathwyaw in .xgmml format
writeXGMML(HSS_test_droso , filename = "z_geno_reactome_0.25.xgmml", threshold = 0.025)

#export significant pathway one by one
for (i in 1: dim(tabp0.025)[1])
{
  j<-as.numeric(tabp0.025$pathway_id[i])
  plot(HSS_test_droso [[j]]) 
  writeXGMML(HSS_test_droso[[j]] , filename = paste0("z_geno_reactome_0.25_pathway",tabp0.025$pathway_id[i],".xgmml"))
  
}
