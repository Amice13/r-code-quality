library(DESeq2)
library(vegan)
library(dplyr)
library(magrittr)
library(splitstackshape)
library(data.table)



#############DEseq
matrix = read.table("adult2.counts.matrix")
samples = read.table("adults2.txt", sep="\t", header=TRUE)
head(samples)



#calculate total nb of reads
adults_new = matrix %>% mutate(Total = dplyr::select(., P10F:P9FD) %>% rowSums(na.rm = TRUE))
rownames(adults_new) = rownames(matrix)

#keep only transcript with >10 reads
adults_new = subset(adults_new, adults_new[ , 18] > 10, drop=FALSE ) 
adults_new = adults_new[,1:17]
matrix = as.matrix(adults_new)
matrix2 = round(matrix)
head(matrix)

#transformation of data
dds_obj_blind <- DESeqDataSetFromMatrix(countData = matrix2,
                                        colData = samples,
                                        design = ~ 1)
dds_blind = DESeq(dds_obj_blind)
rld_blind <- rlog(dds_blind, blind=TRUE)
rld2_blind <- assay(rld_blind)

###############RDA
#prepare factors
info_factor<-as.data.frame(cbind(as.numeric(as.factor(samples$Genotype)),as.numeric(as.factor(samples$Sex))))
colnames(info_factor)<-c("geno","sex")
head(info_factor)

#prepare matrix
expr_mat_trans<-t(rld2_blind)

#have a look at PCA
expr_pca<-prcomp(expr_mat_trans)
plot(expr_pca$x[,1], expr_pca$x[,2],pch=info_factor$sex,col=info_factor$geno )

#rda geno & sex
GENO_SEXrda<-rda(expr_mat_trans~info_factor$geno+info_factor$sex)
plot(GENO_SEXrda, scaling=3) 
points(GENO_SEXrda, display="sites", pch=info_factor$sex, cex=1.3, col=info_factor$geno, scaling=3)
RsquareAdj(GENO_SEXrda)
anova.cca(GENO_SEXrda, parallel=getOption("mc.cores"))
anova(GENO_SEXrda, step=1000, by="margin", parallel=getOption("mc.cores"))

GENO_SEX_scores<-scores(GENO_SEXrda, choices=c(1,2), display="species") 

write.table(GENO_SEX_scores, "GENO_SEX_scores_blind_Adults.txt", sep="\t", quote=F)

#rda geno males
expr_mat_trans_M<-expr_mat_trans[samples$Sex=="Male",]
info_factor_M<-info_factor[samples$Sex=="Male",]

GENO_Mrda<-rda(expr_mat_trans_M~info_factor_M$geno)
plot(GENO_Mrda, scaling=3) 
points(GENO_Mrda, display="sites", pch=info_factor_M$sex, cex=1.3, col=info_factor_M$geno, scaling=3)
RsquareAdj(GENO_Mrda)
anova.cca(GENO_Mrda, parallel=getOption("mc.cores"))
anova(GENO_Mrda, step=1000, by="margin", parallel=getOption("mc.cores"))

GENO_M_scores<-scores(GENO_Mrda, choices=c(1,2), display="species") 
write.table(GENO_M_scores, "GENO_scores_blind_M.txt", sep="\t", quote=F)

#rda geno females
expr_mat_trans_F<-expr_mat_trans[samples$Sex=="Female",]
info_factor_F<-info_factor[samples$Sex=="Female",]

GENO_Frda<-rda(expr_mat_trans_F~info_factor_F$geno)
plot(GENO_Frda, scaling=3) 
points(GENO_Frda, display="sites", pch=info_factor_F$sex, cex=1.3, col=info_factor_F$geno, scaling=3)
RsquareAdj(GENO_Frda)
anova.cca(GENO_Frda, parallel=getOption("mc.cores"))
anova(GENO_Frda, step=1000, by="margin", parallel=getOption("mc.cores"))

GENO_F_scores<-scores(GENO_Frda, choices=c(1,2), display="species") 
write.table(GENO_F_scores, "GENO_scores_blind_F.txt", sep="\t", quote=F)

####larvae
matrix_L = read.table("larvae.counts.matrix")[,c(1:10,12:24,26:28)]
samples_L = read.table("larvae.txt", sep="\t", header=TRUE)[c(1:10,12:24,26:28),]
head(samples_L)
larvae_new = matrix_L %>% mutate(Total = dplyr::select(., L1:YBB2) %>% rowSums(na.rm = TRUE))
rownames(larvae_new) = rownames(matrix_L)

#keep only transcript with >10 reads
larvae_new = subset(larvae_new, larvae_new[ , 27] > 10, drop=FALSE ) 
larvae_new = larvae_new[,1:26]
matrix_L = as.matrix(larvae_new)
matrix_L2 = round(matrix_L)
head(matrix_L)

#transformation of data
dds_obj_blind_L <- DESeqDataSetFromMatrix(countData = matrix_L2,
                                        colData = samples_L,
                                        design = ~ 1)
dds_blind_L = DESeq(dds_obj_blind_L)
rld_blind_L <- rlog(dds_blind_L, blind=TRUE)
rld2_blind_L <- assay(rld_blind_L)

###############RDA
#prepare matrix #have a look at PCA
expr_mat_trans_L<-t(rld2_blind_L)
expr_pca_L<-prcomp(expr_mat_trans_L)
plot(expr_pca_L$x[,1], expr_pca_L$x[,2],col=as.numeric(as.factor(samples_L$Genotype)))

#rda geno on larvae
GENO_rda_L<-rda(expr_mat_trans_L~as.numeric(as.factor(samples_L$Genotype)))
plot(GENO_rda_L, scaling=3) 
points(GENO_rda_L, display="sites", cex=1.3, col=as.numeric(as.factor(samples_L$Genotype)), scaling=3)
RsquareAdj(GENO_rda_L)
anova.cca(GENO_rda_L, parallel=getOption("mc.cores"))
anova(GENO_rda_L, step=1000, by="margin", parallel=getOption("mc.cores"))

GENO_scores_L<-scores(GENO_rda_L, choices=c(1,2), display="species") 
write.table(GENO_scores_L, "GENO_scores_blind_Larvae_3G.txt", sep="\t", quote=F)

#rda geno on larvae
samples_L$Genotype<-as.factor(samples_L$Genotype)
GENO_rda_L_AB<-rda(expr_mat_trans_L[samples_L$Genotype!="AA",]~samples_L$Genotype[samples_L$Genotype!="AA"])
plot(GENO_rda_L_AB, scaling=3) 
points(GENO_rda_L_AB, display="sites", cex=1.3, col=as.numeric(samples_L$Genotype[samples_L$Genotype!="AA"]), scaling=3)
RsquareAdj(GENO_rda_L_AB)
anova.cca(GENO_rda_L_AB, parallel=getOption("mc.cores"))
anova(GENO_rda_L_AB, step=1000, by="margin", parallel=getOption("mc.cores"))

GENO_scores_L_AB<-scores(GENO_rda_L_AB, choices=c(1,2), display="species") 
write.table(GENO_scores_L_AB, "GENO_scores_blind_Larvae_AB_BB.txt", sep="\t", quote=F)




#rda geno on larvae
GENO_rda_L_HOMO<-rda(expr_mat_trans_L[samples_L$Genotype!="AB",]~samples_L$Genotype[samples_L$Genotype!="AB"])
plot(GENO_rda_L_HOMO, scaling=3) 
points(GENO_rda_L_HOMO, display="sites", cex=1.3, col=as.numeric(samples_L$Genotype[samples_L$Genotype!="AB"]), scaling=3)
RsquareAdj(GENO_rda_L_HOMO)
anova.cca(GENO_rda_L_HOMO, parallel=getOption("mc.cores"))
anova(GENO_rda_L_HOMO, step=1000, by="margin", parallel=getOption("mc.cores"))

GENO_scores_L_HOMO<-scores(GENO_rda_L_HOMO, choices=c(1,2), display="species") 
write.table(GENO_scores_L_HOMO, "GENO_scores_blind_Larvae_AA_BB.txt", sep="\t", quote=F)





###########DEseq true analysis

#DEseq accounting for factors
dds_obj_factor <- DESeqDataSetFromMatrix(countData = matrix2,
                                        colData = samples,
                                        design = ~ Sex + Genotype + Sex*Genotype)
dds_factor = DESeq(dds_obj_factor)

head(dds_factor)
res_geno <- as.data.frame(results(dds_factor, name="Genotype_BB_vs_AA", alpha=0.05))
res_geno$id_transcript<-row.names(res_geno)
head(res_geno)
write.table(res_geno, "DE_geno.txt", sep="\t", quote=F, row.names=F)

res_sex <- as.data.frame(results(dds_factor, name="Sex_Male_vs_Female", alpha=0.05))
res_sex$id_transcript<-row.names(res_sex)
head(res_sex)
write.table(res_sex, "DE_sex.txt", sep="\t", quote=F, row.names=F)


############### Prepare file for signet
#in Bash
#cat 04_annotation/transcriptome.hits | awk -F"|" '$1=$1' OFS="\t" | cut -f1,2 > 04_annotation/transcriptome_hits.formatted
#cat 05_results/transcriptome_annotation_table.tsv | cut -f1,2 > 05_results/transcripts_annot_uniprot_swissprot.txt
#transcript_annot<-read.table("../transcripts_annot_uniprot_swissprot.txt", sep="\t", header=T)
transcript_annot<-read.table("transcriptome_hits_Dmel.formatted", header=F)
head(transcript_annot)
transcript_uniprot<-transcript_annot[,c(1,3)]
colnames(transcript_uniprot)<-c("id_transcript","uniprot")
head(transcript_uniprot)

annot_split = cSplit(transcript_uniprot, "uniprot", sep = " ")
terms = colnames(select(annot_split, contains("uniprot")))
annot_long = melt(annot_split,measure.vars=terms, id.vars = "id_transcript", na.rm=TRUE)
head(annot_long)
annot_ready = as.data.frame(annot_long[,c(1, 3)])
head(annot_ready)
tail(annot_ready)
#write.table(annot_ready, "../transcripts_annot_uniprot_swissprot_simple.txt", sep="\t", quote=F, row.names=F)
write.table(annot_ready, "transcripts_annot_uniprot_Dmel_simple.txt", sep="\t", quote=F, row.names=F)

