############################################
### Code for Building from the Brain #####

# This code loads the CBMA brain maps used in the 
# main text and replicates all statistics and figures 
# except for the 3D brain renderings (Figure 1A, Figure 3)

# Replication code for the 3D brain renderings is provided 
# separately as .py scripts to run on MRIcroGL


# Table of Contents
# 1. Meta-Analytic Brain Maps (load, binarize)
# 2. Reference Parcels (load, helper functions)
# 3. Parcellate CBMA maps for analysis
# 4. Statistical Tests of Assumption 1
# 5. Statistical Tests of Assumption 2
# 6. Replicate Figures 1B, 1C, 2A, 2B, 2C 

# Clear the environment
gc()
rm(list = ls())

# For reproducibility, use groundhog to load packages
library(groundhog)
pkgs = c("oro.nifti", "neurobase", "dplyr", "ggplot2")
groundhog.library(pkgs, "2024-04-22")

# Set the working directory - Code designed to run from a single folder
setwd()
# setwd("C:/Users/mlw/Dropbox (Personal)/Projects_Berkeley/CBMA_IO/ReproVersion_IO")


#############################################
# 1. Meta-Analytic Brain Maps ####
# Original Neurovault collection information is provided
# in the main text

## Tan et al.: 2mm iso, 91 x 109 x 91 ####
clust_loss <- readnii("Monetary_loss_P005_1000_ALE.nii")
# Available at: https://neurovault.org/images/776023/
clust_pain <- readnii("Physical_pain_P005_1000_ALE.nii")
# Available at: https://neurovault.org/images/776025/

## Bartra et al.: 2mm iso, 91 x 109 x 91 ####
# Negative subjective value (Bartra et al, Figure 3, panel B)
kda_nsv <- readnii("I_NEG_kda10_testStat_mean_cMass_corrp.nii")
# Available at: https://neurovault.org/images/13348/
# NOTE: the corrp map corresponds to the clusters reported in the original paper (tstat thresh does not)

## Schurz et al.: 2mm iso 91 x 109 x 91  ####
# Rational Action Intention Inference
sch_RA <- readnii("T_RA_thresh.nii")
# Available at: https://neurovault.org/images/500564/

# Strategic Games
sch_GA <- readnii("T_GA_thresh.nii")
# Available at: https://neurovault.org/images/500562/

# Trait Inference
sch_TR <- readnii("T_TR_thresh.nii")
# Available at: https://neurovault.org/images/500566/

# False belief task
sch_FB <- readnii("T_FB_thresh.nii")
# Available at: https://neurovault.org/images/500561/

# Reasoning about emotional states and consequences
sch_RE <- readnii("E_RE_thresh.nii")
# Available at: https://neurovault.org/images/500558/

# Sharing pain and emotional states
sch_SH <- readnii("E_SH_thresh.nii")
# Available at: https://neurovault.org/images/500560/


## Fehlbaum et al.: 2mm iso 91 x 109 x 91 ####
# use adult data only, most strict cluster forming criteria
feh_ale <- readnii("Adults_strict_C05_1k_ALE.nii.gz")
# Avalailable at: https://neurovault.org/images/510208/

## Create binary masks for all CMBAs ####

# Pain (Reported)
bin_pain <- ifelse(clust_pain > 0, 1, 0)
n_pain <- sum(c(bin_pain))
# Active voxels:
n_pain #3052

# Monetary Loss (Reported)
bin_loss <- ifelse(clust_loss > 0, 1, 0)
n_loss <- sum(c(bin_loss))
# Active voxels:
n_loss #1118

# Negative Subjective value
bin_nsv <- ifelse(kda_nsv > 0, 1, 0)
n_nsv <- sum(c(bin_nsv))
# Active voxels:
n_nsv #5941

# Rational action inference
bin_RA <- ifelse(sch_RA > 0, 1, 0)
n_RA <- sum(c(bin_RA))
# Active voxels:
n_RA #12810

# Strategic games
bin_GA <- ifelse(sch_GA > 0, 1, 0)
n_GA <- sum(c(bin_GA))
# Active voxels:
n_GA #9958

# Inferences about traits
bin_TR <- ifelse(sch_TR > 0, 1, 0)
n_TR <- sum(c(bin_TR))
# Active voxels:
n_TR #15655

# False Belief task (inferences about beliefs/knowledge)
bin_FB <- ifelse(sch_FB > 0, 1, 0)
n_FB <- sum(c(bin_FB))
# Active voxels:
n_FB #23622

# Emotion inference
bin_RE <- ifelse(sch_RE > 0, 1, 0)
n_RE <- sum(c(bin_RE))
# Active voxels:
n_RE #15018

# Sharing Pain and Emotions
bin_SH <- ifelse(sch_SH > 0, 1, 0)
n_SH <- sum(c(bin_SH))
# Active voxels:
n_SH #7395

# Mentalizing Meta
bin_ment <- ifelse(feh_ale > 0, 1, 0)
n_ment <- sum(c(bin_ment))
n_ment #11556



#############################################

#############################################
# 2. Reference Parcels and Helper Functions  ####

## de la Vega et al: k50 funcational parcellation ####
# Load original mask
k50_msk <- readnii("k50_2mm.nii.gz")
# Available from: https://neurovault.org/images/395092/

k50_msk_rd <- round(k50_msk)
# ROI Names from Neurovault - manually corrected for errors by MLW
k50_key <- readxl::read_xlsx("k50_2mm_key.xlsx")
k50_rois <- k50_key[-1,] #drop the row of zeros
#head(k50_rois)
k50_names <- c(k50_rois$Use_parcel) #make char vector of names

## Helper functions ####
### Function for Extracting k50 voxel counts ####

# takes in any mask and returns  
# the count of non-zero voxels in each k50 parcel
k50.nz.fun <- function(x){
  # x is the mask (cluster, binary or ale)
  # output vector is length 50
  k50_n_vox <- c()
  for(i in 1:50){
    use_msk <- k50_msk_rd == i
    use_bin <- ifelse(use_msk > 0, 1, 0)
    vox_msk <- use_bin * c(x)
    k50_n_vox[i] <- sum(vox_msk > 0)
  }
  names(k50_n_vox) <- k50_names
  return(k50_n_vox)
}

### Function for Running RSA in parcels - Pearson's Phi ####
k50.rsa.fun <- function(df, ale1, ale2, parcels){
  # dataframe, names of the columns, parcel vector
  df_use <- dplyr::select(df, all_of(ale1), all_of(ale2), Mask)
  # output vectors:
  cor_r <- c()
  cor_p <- c()
  # parcel names
  msk_name <- c()
  for(i in 1:length(parcels)){
    msk_idx <- parcels[i]
    msk_name[i] <- k50_names[msk_idx]
    df_parc <- dplyr::filter(df_use, Mask == parcels[i])
    # columns by index
    x <- df_parc[,1]
    y <- df_parc[,2]
    cov_xy <- cor.test(x, y, method = "pearson")
    cor_r[i] <- cov_xy$estimate
    cor_p[i] <- cov_xy$p.value
  }
  out.df <- data.frame(ALE_1 = ale1,
                       ALE_2 = ale2,
                       Mask = msk_name,
                       Corr = round(cor_r, 4),
                       P_val = round(cor_p, 4))
  return(out.df)
}



### Function for Calculating mean RSAs ####
ra.avgs.fun <- function(x){
  m <- c()
  ale <- c()
  for(i in 1:length(bin_x)){
    ale[i] <- bin_x[i]
    x.ale <- dplyr::filter(x, ALE_2 == bin_x[i])
    m[i] <- mean(x.ale$Corr_zero)
  }
  x.df <- data.frame(ALE_2 = ale,
                     Mean = m,
                     Network = x$Network[1])
  return(x.df)
}

#############################################

#############################################
# 3. Parcellate CBMA Maps ####

# Note: these can take a little bit to run on a regular laptop
# Also includes a check of the coverage loss between the k50
# map and the CBMA map.  Generally very high (94-99%)

## Pain ####
pain_bin_k50 <- k50.nz.fun(bin_pain) 
sum(pain_bin_k50)/n_pain #99.3% coverage

## Monetary Loss ####
loss_bin_k50 <- k50.nz.fun(bin_loss) 
sum(loss_bin_k50)/n_loss #98.8% coverage

## Negative Subjective Value ####
nsv_bin_k50 <- k50.nz.fun(bin_nsv) 
sum(nsv_bin_k50)/n_nsv #99.2% coverage


## Mentalizing (6 Maps) ####
RA_bin_k50 <- k50.nz.fun(bin_RA) 
sum(RA_bin_k50)/n_RA #98.6% coverage

GA_bin_k50 <- k50.nz.fun(bin_GA) 
sum(GA_bin_k50)/n_GA #97.5% coverage

TR_bin_k50 <- k50.nz.fun(bin_TR) 
sum(TR_bin_k50)/n_TR #94.6% coverage

FB_bin_k50 <- k50.nz.fun(bin_FB) 
sum(FB_bin_k50)/n_FB #93.6% coverage

RE_bin_k50 <- k50.nz.fun(bin_RE) 
sum(RE_bin_k50)/n_RE #96.1% coverage

SH_bin_k50 <- k50.nz.fun(bin_SH) 
sum(SH_bin_k50)/n_SH #98.3% coverage


#############################################

#############################################
# 4. Statistical Tests of Assumption 1  ####

## Pain vs. Monetary Loss ####
### Set up Parcels ####
# make a dataframe 
ppml_k50_df <- data.frame(Parcel = k50_names,
                         PP = pain_bin_k50,
                         ML = loss_bin_k50)
rownames(ppml_k50_df) <- NULL
dim(ppml_k50_df)
# 50 x 3

# Add columns for uniqueness and overlap
ppml_k50_df$PPML <- ifelse(ppml_k50_df$PP > 0 & 
                             ppml_k50_df$ML > 0, 1, 0)
ppml_k50_df$PP_only <- ifelse(ppml_k50_df$PP > 0 &
                                ppml_k50_df$PPML == 0, 1, 0)
ppml_k50_df$ML_only <- ifelse(ppml_k50_df$ML > 0 &
                                ppml_k50_df$PPML == 0, 1, 0)
# insert tracker for all zeros
ppml_k50_none <- rowSums(ppml_k50_df[,-1])
ppml_k50_df$none <- ifelse(ppml_k50_none > 0, 0, 1)

# shares - note, the denominator is the full n, not adjusted 
# for the voxels that fall outside the k50 mask, which is a very
# small number and doesn't impact the conclusions
ppml_k50_df$PP_share <- ppml_k50_df$PP/n_pain 
ppml_k50_df$ML_share <- ppml_k50_df$ML/n_loss

### Parcel Distributions (Reported) ####
# Get column counts: 
ppml_parc_n <- apply(ppml_k50_df[,-1], 2, function(x) sum(x > 0))

# Total number of parcels included in each CBMA:
ppml_parc_n[c("PP", "ML")]
#PP ML 
#18  9

# Unique number 
ppml_parc_n[c("PP_only", "ML_only")]
#PP_only ML_only 
#13       4

# Generate dataframe to use for analysis
use_ppml_k50_df <- subset(ppml_k50_df, none == 0)
# 22 valid parcels

### Functional Similarity: Pain vs. Monetary Loss (Reported) ####

# Correlation of distribution following method in Tan et al.
cor.test(use_ppml_k50_df$PP_share, 
         use_ppml_k50_df$ML_share, 
         method = "spearman")
#S = 2266, p-value = 0.2077
#-0.2795284
# not signif correlated and not positively correlated in any case


### Representational Similarity: Pain vs. Monetary Loss ####

#### Prepare the data ####
# Build a dataframe of voxels within all 22 masks
ppml_rsa_df <- data.frame(Mask = c(k50_msk_rd),
                       bin_pain = c(bin_pain),
                       bin_loss = c(bin_loss))
# get all non-zero voxels
bin_ppml_sums <- ppml_rsa_df$bin_pain + ppml_rsa_df$bin_loss
ppml_rsa_df$nonzero <- ifelse(bin_ppml_sums > 0, 1, 0)
#sum(ppml_rsa_df$nonzero) #4563

# Names of all parcels per CBMA
pp_parcels <- as.character(subset(ppml_k50_df, PP > 0)$Parcel)
ml_parcels <- as.character(subset(ppml_k50_df, ML > 0)$Parcel)

# Unique parcels
pp_unique <- setdiff(pp_parcels, ml_parcels) 
length(pp_unique)
# 13/18 is correct
ml_unique <- setdiff(ml_parcels, pp_parcels)
length(ml_unique)
# 4/9 is correct 

# Overlapping parcels per pair
ppml_parcels <- intersect(pp_parcels, ml_parcels)
length(ppml_parcels)
# 5 is correct

# Create vector of numeric values for filtering voxel-wise data
ppml_num <- strsplit(ppml_parcels, "_")
ppml_num <- lapply(ppml_num, function(x)  x[[1]])
ppml_num <- as.numeric(unlist(ppml_num))

#### RSA Results (Reported) ####

# NOTE: Bonferroni corrected thresholds
ppml_thresh <- 0.05/length(ppml_num)
ppml_thresh
# 0.01 is correct

# PP vs ML RSA
ppml_rsa <- k50.rsa.fun(df = ppml_rsa_df,
                        ale1 = "bin_pain",
                        ale2 = "bin_loss",
                        parcels = ppml_num)
ppml_rsa$P_critical <- ppml_thresh
ppml_rsa
# P-value smaller than critical value:
# neg: aMPFC (1), 
# pos: dLPFC_SMA (24)


## Negative Subjective Value vs. Monetary Loss ####
### Set up Parcels ####
# make a dataframe
mlnsv_k50_df <- data.frame(Parcel = k50_names,
                        ML = loss_bin_k50,
                        NSV = nsv_bin_k50)
rownames(mlnsv_k50_df) <- NULL
head(mlnsv_k50_df)

# Add columns for uniqueness and overlap
mlnsv_k50_df$MLNSV <- ifelse(mlnsv_k50_df$ML > 0 & 
                               mlnsv_k50_df$NSV > 0, 1, 0)
mlnsv_k50_df$ML_only <- ifelse(mlnsv_k50_df$ML > 0 &
                                 mlnsv_k50_df$MLNSV == 0, 1, 0)
mlnsv_k50_df$NSV_only <- ifelse(mlnsv_k50_df$NSV > 0 &
                                  mlnsv_k50_df$MLNSV == 0, 1, 0)
# insert tracker for all zeros
mlnsv_k50_none <- rowSums(mlnsv_k50_df[,-1])
mlnsv_k50_df$none <- ifelse(mlnsv_k50_none > 0, 0, 1)

# shares
mlnsv_k50_df$ML_share <- mlnsv_k50_df$ML/n_loss
mlnsv_k50_df$NSV_share <- mlnsv_k50_df$NSV/n_nsv


### Parcel Distributions (Reported) ####
# Get column counts
mlnsv_parc_n <- apply(mlnsv_k50_df[,-1], 2, function(x) sum(x > 0))

# Total number of parcels included in each CBMA:
mlnsv_parc_n[c("ML", "NSV")]
#ML NSV
#9 27 

# Unique number
mlnsv_parc_n[c("ML_only", "NSV_only")]
#ML_only NSV_only 
#1       19

# Generate dataframe to use for analysis
use_mlnsv_k50_df <- subset(mlnsv_k50_df, none == 0)
# 28 valid parcels

### Functional Similarity: NSV vs. Monetary Loss (Reported) ####

# Correlation of distribution
cor.test(use_mlnsv_k50_df$ML_share, 
         use_mlnsv_k50_df$NSV_share, 
         method = "spearman")
#S = 3239.2, p-value = 0.5652
#rho = 0.1135144 
# not correlated

### Representational Similarity: NSV vs. Monetary Loss ####
#### Prepare the data ####
# Build a dataframe of voxels within all 28 masks
mlnsv_rsa_df <- data.frame(Mask = c(k50_msk_rd),
                        bin_loss = c(bin_loss),
                        bin_nsv = c(bin_nsv))

mlnsv_sums <- mlnsv_rsa_df$bin_loss + mlnsv_rsa_df$bin_nsv
mlnsv_rsa_df$nonzero <- ifelse(mlnsv_sums > 0, 1, 0)
#sum(mlnsv_rsa_df$nonzero) #6635

# Get nsv parcel names
nsv_parcels <- as.character(subset(mlnsv_k50_df, NSV > 0)$Parcel)

# Overlapping parcel
mlnsv_parcels <- intersect(ml_parcels, nsv_parcels)
length(mlnsv_parcels)
# 8 is correct

# Create vector of numeric values for filtering voxel-wise data
mlnsv_num <- strsplit(mlnsv_parcels, "_")
mlnsv_num <- lapply(mlnsv_num, function(x)  x[[1]])
mlnsv_num <- as.numeric(unlist(mlnsv_num))
mlnsv_num
# 8 is correct

#### RSA Results (Reported) ####

# NOTE: Bonferroni corrected thresholds
mlnsv_thresh <- 0.05/length(mlnsv_num)
mlnsv_thresh
# 0.00625 is correct

# ML vs NSV RSA: 
mlnsv_rsa <- k50.rsa.fun(df = mlnsv_rsa_df,
                         ale1 = "bin_loss",
                         ale2 = "bin_nsv",
                         parcels = mlnsv_num)
mlnsv_rsa$P_critical <- mlnsv_thresh
mlnsv_rsa
# weak positive corr: AI, IFG (19, 38)
# weak neg corr: aMPFC (1)
#############################################

#############################################
# 5. Statistical Tests of Assumption 2 #### 

# Note: Rational Action Intention Inference is the reference CBMA
# against which the others are compared

## Set Up Parcels ####
sch_k50_df <- data.frame(Parcel = k50_names,
                         RA = RA_bin_k50,
                         GA = GA_bin_k50,
                         TR = TR_bin_k50,
                         FB = FB_bin_k50,
                         RE = RE_bin_k50,
                         SH = SH_bin_k50)
rownames(sch_k50_df) <- NULL
head(sch_k50_df)

# shares
sch_k50_df$RA_share <- sch_k50_df$RA/n_RA
sch_k50_df$GA_share <- sch_k50_df$GA/n_GA
sch_k50_df$TR_share <- sch_k50_df$TR/n_TR
sch_k50_df$FB_share <- sch_k50_df$FB/n_FB
sch_k50_df$RE_share <- sch_k50_df$RE/n_RE
sch_k50_df$SH_share <- sch_k50_df$SH/n_SH

## Functional Similarity (Reported) ####
### RA vs. GA ####
ravga <- dplyr::filter(sch_k50_df, RA > 0 | GA > 0)
length(unique(ravga$Parcel)) #33
sum(ravga$RA > 0) #30
sum(ravga$GA > 0) #18

# Correlation of distribution
ct_ravga <- cor.test(ravga$RA_share, 
                     ravga$GA_share, 
                     method = "spearman")
ct_ravga
#S = 4574.9, p-value = 0.1871
#r = 0.2354803 
# not signif correlated

### RA vs. TR ####
ravtr <- dplyr::filter(sch_k50_df, RA > 0 | TR > 0)
length(unique(ravtr$Parcel)) #32
sum(ravtr$RA > 0) #30
sum(ravtr$TR > 0) #21

# Correlation of distribution
ct_ravtr <- cor.test(ravtr$RA_share, 
                     ravtr$TR_share, 
                     method = "spearman")
ct_ravtr
#S = 2379.5, p-value = 0.0007767
#0.5638755 

### RA vs. FB ####
ravfb <- dplyr::filter(sch_k50_df, RA > 0 | FB > 0)
length(unique(ravfb$Parcel)) #37
sum(ravfb$RA > 0) #30
sum(ravfb$FB > 0) #29

# Correlation of distribution
ct_ravfb <- cor.test(ravfb$RA_share, 
                     ravfb$FB_share, 
                     method = "spearman")
ct_ravfb
#S = 3584.7, p-value = 0.0001968
#0.5750755 

### RA vs. RE ####
ravre <- dplyr::filter(sch_k50_df, RA > 0 | RE > 0)
length(unique(ravre$Parcel)) #36
sum(ravre$RA > 0) #30
sum(ravre$RE > 0) #30

# Correlation of distribution
ct_ravre <- cor.test(ravre$RA_share, 
                     ravre$RE_share, 
                     method = "spearman")
ct_ravre
#S = 3014.8, p-value = 7.3e-05
#0.6119982 


### RA vs. SH ####
ravsh <- dplyr::filter(sch_k50_df, RA > 0 | SH > 0)
length(unique(ravsh$Parcel)) #37
sum(ravsh$RA > 0) #30
sum(ravsh$SH > 0) #25

# Correlation of distribution
ct_ravsh <- cor.test(ravsh$RA_share, 
                     ravsh$SH_share, 
                     method = "spearman")
ct_ravsh
#S = 7184.4, p-value = 0.3809
#0.1483596 


## RSA: Mentalizing vs. Non-Mentalizing Regions ####
### Prepare the data ####
# Create a dataframe
inf_rsa_df <- data.frame(Mask = c(k50_msk_rd),
                            bin_RA = c(bin_RA),
                            bin_GA = c(bin_GA),
                            bin_TR = c(bin_TR),
                            bin_FB = c(bin_FB),
                            bin_SH = c(bin_SH),
                            bin_RE = c(bin_RE))
# Add a column for the binary indicator of the mentalizing mask
inf_rsa_df$bin_ment <- c(bin_ment)
# Add a column to count how many positive voxels appear across maps
inf_rsa_df$count_msk <- rowSums(inf_rsa_df[,c(2:7)])

# Filter to Fehlbaum's mentalizing clusters
ment_rsa_df <- dplyr::filter(inf_rsa_df, bin_ment > 0)

# Get parcels
ment_rois <- unique(ment_rsa_df$Mask)
# drop 0
ment_rois <- setdiff(ment_rois, c(0))
length(ment_rois) #20

### Overlapping Voxel Count (Reported) ####
# For every Area (incl. 0), counts how many times each 
# voxel shows up in Mentalizing exercises
# 0 = no map shows activation; 1 = 1 map shows activation, etc.
# Overlapping is defined as 2 or more maps showing activity

vox_ovlp_tab <- table(inf_rsa_df$Mask, inf_rsa_df$count_msk)
vox_ovlp_df <- as.data.frame.matrix(vox_ovlp_tab)
colnames(vox_ovlp_df) <- paste("N_Ovlp_", c(0:6), sep = "")
vox_ovlp_df$Mask <- c(0:50)
# drop first row which is Area 0 (i.e. outside the brain)
vox_ovlp_df <- dplyr::filter(vox_ovlp_df, Mask != 0)
# Then drop non-overlapping regions (>5 vox overlap)
# This will automatically drop all Areas with only 1 map
vox_ovlp_df2 <- dplyr::filter(vox_ovlp_df, N_Ovlp_2 > 0)
dim(vox_ovlp_df2)
# N regions with overlap = 34

# N voxels with overlapping activation
nvox_ovlp <- sum(colSums(vox_ovlp_df2[,c(3:7)]))
nvox_ovlp
#21453

# identify which regions included in mentalizing meta
ovlp_ment <- intersect(vox_ovlp_df2$Mask, ment_rois)
length(ovlp_ment) #18 regions with overlaps are in mentalizing areas

# N voxels with overlaps in mentalizing areas
vox_ment_df <- dplyr::filter(vox_ovlp_df2, Mask %in% ovlp_ment)
nvox_ment <-  sum(colSums(vox_ment_df[,c(3:7)]))
nvox_ment
#18951

# Share of Voxels with some overlap that are in mentalizing regions
nvox_ment/nvox_ovlp
# 0.883373

### RA vs. Functionally Similar Tasks in Mentalizing ROIs ####
# Cut-point for RA activity (no region with fewer than 1% voxels)
share_cut <- 0.01

# Identify RA regions above cutpoint 
ra_cut <- dplyr::filter(sch_k50_df, RA_share > share_cut)
ra_rois <- as.character(ra_cut$Parcel)
length(ra_rois)
# Note: total active regions = 30
# At 0.01, N regions = 19
# But there's not much point in RSA in a region 
# with a tiny number of active voxels
# Switching to a 0.005 thresholds adds 4 ROIs but all 
# conclusions remain the same

# Identify RA mentalizing regions and non-mentalizing regions
ra_rois_num <- strsplit(ra_rois, "_")
ra_rois_num <- lapply(ra_rois_num, function(x)  x[[1]])
ra_rois_num <- as.numeric(unlist(ra_rois_num))

ra_ment <- intersect(ra_rois_num, ment_rois)
length(ra_ment) #12
ra_other <- setdiff(ra_rois_num,  ra_ment)
length(ra_other) #7

#### RA vs. TR ####
ravtr_m_rsa <- k50.rsa.fun(df = inf_rsa_df,
                           ale1 = "bin_RA",
                           ale2 = "bin_TR",
                           parcels = ra_ment)
ravtr_m_rsa

ravtr_o_rsa <- k50.rsa.fun(df = inf_rsa_df,
                           ale1 = "bin_RA",
                           ale2 = "bin_TR",
                           parcels = ra_other)
ravtr_o_rsa

#### RA vs. FB ####
ravfb_m_rsa <- k50.rsa.fun(df = inf_rsa_df,
                           ale1 = "bin_RA",
                           ale2 = "bin_FB",
                           parcels = ra_ment)
ravfb_m_rsa

ravfb_o_rsa <- k50.rsa.fun(df = inf_rsa_df,
                           ale1 = "bin_RA",
                           ale2 = "bin_FB",
                           parcels = ra_other)
ravfb_o_rsa

#### RA vs. RE ####
ravre_m_rsa <- k50.rsa.fun(df = inf_rsa_df,
                           ale1 = "bin_RA",
                           ale2 = "bin_RE",
                           parcels = ra_ment)
ravre_m_rsa

ravre_o_rsa <- k50.rsa.fun(df = inf_rsa_df,
                           ale1 = "bin_RA",
                           ale2 = "bin_RE",
                           parcels = ra_other)
ravre_o_rsa

#### Avg. RSA by Region Type (Reported) ####

# Note: 0 inserted instead of NA because "NA" reflects
# where the comparison map had no significant activity.  Correlation
# in these regions is treated as 0 ("uncorrelated") rather than 
# a negative value ("anticorrelated") to be more conservative on
# the resulting averages

# Set up dataframes
ra_ment_df <- rbind(ravtr_m_rsa, ravfb_m_rsa, ravre_m_rsa)
ra_ment_df$Network <- "Mentalizing"
ra_ment_df$Corr_zero <- ifelse(is.na(ra_ment_df$Corr), 0, 
                               ra_ment_df$Corr)
head(ra_ment_df)

ra_other_df <- rbind(ravtr_o_rsa, ravfb_o_rsa, ravre_o_rsa)
ra_other_df$Network <- "Other"
ra_other_df$Corr_zero <- ifelse(is.na(ra_other_df$Corr), 0, 
                                ra_other_df$Corr)
head(ra_other_df)

# Generate averages
bin_x <- c("bin_TR", "bin_FB", "bin_RE")

ra_ment_avgs <- ra.avgs.fun(ra_ment_df)
ra_ment_avgs
mean(ra_ment_avgs$Mean) #0.3997694

ra_other_avgs <- ra.avgs.fun(ra_other_df)
ra_other_avgs
mean(ra_other_avgs$Mean) #0.1791143

#############################################

#############################################
# 6. Figures  ####

### Fig. 1B: Functional Similarity Line Chart (Pain vs Loss) ####

# Point-line chart inputs
ppvml_plot <- use_ppml_k50_df[, c("Parcel", "PP_share", "ML_share")]
ppvml_nr <- length(ppvml_plot$Parcel)
# Pull Parcel number
ppvml_plot$Parcel2 <- sapply(stringr::str_split(ppvml_plot$Parcel,"_"),'[[',1)
ppvml_plot$Area <- paste("Area", ppvml_plot$Parcel2, sep= " ")
ppvml_plot <- ppvml_plot[order(as.numeric(ppvml_plot$Parcel2)),]

ppvml_plot <- tidyr::pivot_longer(ppvml_plot, 
                                  cols = c("PP_share", "ML_share"),
                                  names_to = "Task",
                                  values_to = "Share")
head(ppvml_plot)

# Transform Area and Task to factors
ppvml_plot$Area <- as.factor(ppvml_plot$Area)
ppvml_plot$Task <- factor(ppvml_plot$Task, 
                          levels = c("ML_share", "PP_share"),
                          labels = c("Monetary Loss", "Physical Harm"))
#ppvml_plot$Parcel <- as.character(ppvml_plot$Parcel)

# Plot
ppvml_fsim_gg <- ggplot(ppvml_plot,
                    aes(x = reorder(Area, as.numeric(Parcel2)),
                        y = Share*100,
                        group = Task,
                        color = Task)) + 
  geom_point(size = 4) + 
  geom_line(size = 2) +
  scale_color_manual(values = c("darkgrey", "black")) +
  labs(title = "Functional Similiarity: Physical Harm vs. Montetary Loss",
       subtitle = "22 brain areas active for one or more tasks",
       y = "Share of Active Voxels (%)") + 
  annotate("text", 
           label = "Spearman's rho = -0.28",
           size = 8,
           x = 6, y=55) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(angle = 45,
                                 hjust = 1),
        title = element_text(size = 18))
ppvml_fsim_gg


### Fig. 1C: Representational Similarity Bar Chart (Pain vs Loss) ####

rsim_df <- ppml_rsa
rsim_df$P_thresh <- ifelse(rsim_df$P_val < 0.01, "p < 0.01", "p > 0.01")
rsim_df$P_thresh <- factor(rsim_df$P_thresh)
# Match Areas from Similarity figure
rsim_df$Area <- rsim_df$Mask
rsim_df$Area <- factor(rsim_df$Area, 
                     levels = c("1_aMPFC", "15_white-matter", 
                                "19_aIns", "24_dMCC_dLPFC_SMA", "38_Ins_IFGorb"),
                     labels = c("Area 1", "Area 15",
                                "Area 19", "Area 24", "Area 38"))
rsim_df

ppvml_rsim_gg <- ggplot(rsim_df,
                    aes(x = reorder(Area, -Corr),
                        y = Corr)) + 
  geom_col(aes(fill = P_thresh), width = 0.8, color = "black") + 
  scale_fill_grey(start = 0.1, end = 0.9) +
  labs(title = "Representational Similarity: Physical Harm vs. Montetary Loss",
       subtitle = "5 brain areas activated across both meta-analyses",
       y = "Pearson's phi") + 
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 14),
        title = element_text(size = 18))
ppvml_rsim_gg




### Fig. 2A: Functional Similarity Line Chart (Mentalizing) ####

# Inferences data
inf_nz <- dplyr::select(sch_k50_df, Parcel, RA_share, GA_share,
                           TR_share, FB_share, RE_share, SH_share)
inf_nz$Zeros <- inf_nz$RA_share + inf_nz$GA_share + 
  inf_nz$TR_share + inf_nz$FB_share + inf_nz$RE_share + 
  inf_nz$SH_share
inf_nz <- dplyr::filter(inf_nz, Zeros > 0.01) #32 for 0.001; #32 for 0.01
inf_rois <- length(inf_nz$Parcel)
head(inf_nz)

# Pull Parcel number
inf_nz$Parcel2 <- sapply(stringr::str_split(inf_nz$Parcel,"_"),'[[',1)
inf_nz$Area <- paste("Area", inf_nz$Parcel2, sep= " ")
inf_nz <- inf_nz[order(as.numeric(inf_nz$Parcel2)),]

inf_parcel_plot <- tidyr::pivot_longer(inf_nz, 
                                      cols = c("RA_share", "GA_share",
                                               "TR_share", "FB_share",
                                               "RE_share", "SH_share"),
                                      names_to = "Task",
                                      values_to = "Share")
head(inf_parcel_plot)

# Transform Region and Task to factors
inf_parcel_plot$Task <- factor(inf_parcel_plot$Task, 
                              levels = c("RA_share", "GA_share", "SH_share",
                                         "TR_share", "FB_share", "RE_share"),
                              labels = c("Infer Intentions", 
                                         "Strategic Gameplay",
                                         "Simulate Internal State",
                                         "Infer Trait",
                                         "Infer Factual Belief",
                                         "Infer Emotional Response"))

# Plot
inf_fsim_lines_gg <- ggplot(inf_parcel_plot,
                       aes(x = reorder(Area, as.numeric(Parcel2)),
                           y = Share*100,
                           group = Task,
                           color = Task,
                           linetype = Task)) + 
  geom_point(size = 3) + 
  geom_line(size = 1.5) +
  scale_color_manual(values = c("black", "#969696", "#cccccc", "#636363", "#969696", "#cccccc")) +
  scale_linetype_manual(values = c(rep("solid",3), rep("dotted",3))) +
  labs(title = "Functional Similarity of Mentalizing Activities",
       subtitle = "32 brain areas with at least 1% combined activation",
       y = "Share of Active Voxels (%)") + 
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.key.width = unit(1.8, "cm"),
        legend.spacing.y = unit(2.0, "cm"),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        title = element_text(size = 18))
inf_fsim_lines_gg

# Note: Boxes in Fig 2A were added manually
# Find ROIs (n = 17):
region_area <- dplyr::select(inf_parcel_plot, Parcel2, Area)
region_area <- dplyr::distinct(region_area)
region_area$Fehlb_ROI <- ifelse(region_area$Parcel2 %in% ment_rois, 1, 0)
print(subset(region_area, Fehlb_ROI == 1)$Area)

### Fig. 2B: Functional Similarity Bar Chart (Mentalizing) ####

# Make a dataframe from inference func similarity test results
inf_cor_df <- data.frame(Task = c("GA", "TR", "FB", "RE", "SH"),
                    Corr = c(ct_ravga$estimate, ct_ravtr$estimate,
                             ct_ravfb$estimate, ct_ravre$estimate,
                             ct_ravsh$estimate),
                    Pval =  c(ct_ravga$p.value, ct_ravtr$p.value,
                              ct_ravfb$p.value, ct_ravre$p.value,
                              ct_ravsh$p.value))
# Tidy
inf_cor_df$Corr <- round(inf_cor_df$Corr, 3)
inf_cor_df$Pval <- round(inf_cor_df$Pval, 4)
inf_cor_df$Pthresh <- ifelse(inf_cor_df$Pval < 0.001, "p < 0.001", 
                        ifelse(inf_cor_df$Pval <= 0.05, "p < 0.05", "p > 0.05"))
inf_cor_df$Pthresh <- factor(inf_cor_df$Pthresh)
inf_cor_df$Task <- factor(inf_cor_df$Task, 
                     levels = c("GA", "TR", "FB", "RE", "SH"),
                     labels = c("Strategic\nGameplay",
                                "Trait\nInference",
                                "Factual\nBelief\nInference",
                                "Emotional\nResponse\nInference",
                                "Internal\nState\nSimulation"))
inf_cor_df

inf_fsim_bars_gg <- ggplot(inf_cor_df,
                    aes(x = reorder(Task, -Corr),
                        y = Corr)) + 
  geom_col(aes(fill = Pthresh), width = 0.8, color = "black") + 
  scale_fill_grey(start = 0.1, end = 0.9) +
  ylim(0,1) +
  labs(title = "Functional Similarity: Correlation \nwith Intention Inference",
       subtitle = "Evaluated across areas with at least \n1% active voxels for Intention Inference",
       y = "Spearman's rho") + 
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 14),
        title = element_text(size = 18))
inf_fsim_bars_gg


### Fig. 2C: Representational Similarity (Mentalizing) ####

# combine the mean correlations
inf_rs_3 <- rbind(ra_ment_avgs, ra_other_avgs)
inf_rs_3$ALE_2 <- factor(inf_rs_3$ALE_2, 
                         levels=c("bin_FB", "bin_RE", "bin_TR"),
                         labels=c("Factual Belief \nInference",
                                  "Emotional Response \nInference",
                                  "Trait \nInference"))
inf_rs_3$Network <- factor(inf_rs_3$Network, 
                         levels=c("Mentalizing", "Other"),
                         labels=c("Mentalizing Areas",
                                  "Other Areas"))

inf_rsim_bars_gg <- ggplot(inf_rs_3,
                      aes(x = ALE_2,
                          y = Mean,
                          fill = Network)) + 
  geom_col(position="dodge", width = 0.8, color = "black") +
  scale_fill_grey(start = 0.1, end = 0.9) +
  ylim(0,0.5) +
  labs(title = "Representational Similarity: Correlation with \nIntention Inference",
       subtitle = "Evaluated across areas with at least 1% of active voxels \nfor Intention Inference",
       y = "Avg. Pearson's phi") + 
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 14),
        title = element_text(size = 18))
inf_rsim_bars_gg


