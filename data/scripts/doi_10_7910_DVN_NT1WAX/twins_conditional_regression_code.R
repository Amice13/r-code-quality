library(tidyverse)
library(DMRcate)
library(survival)
library(IlluminaHumanMethylationEPICanno.ilm10b4.hg19)

##Set working directory to save output files
setwd("/path/to/output_directory")

###Load raw beta and outcomes files (clinical variables)
###Beta file rownames should be subject IDs (ID), column names are array CpGs (probeID)
beta_file <- readRDS("/path/to/beta_file.rds")
###outcomes file should include ALL case status (case_status), twin pair number (Pair_ID) EPIC array plate/chip, sex, and nucleated cell deconvolution variables (IDOL method)
outcomes <- readRDS("/path/to/outcome_file.rds")

###Ensure beta file and outcomes contain the same subjects and same subject order
betas_update <- beta_file[row.names(beta_file)%in%outcomes$ID,]

###Omit non-CpG probes from analysis
colIND <- grep("cg",colnames(betas_update))
betas_update_1 <- betas_update[,colIND]

###Transform beta values to Mvalues (with adequate transposing)
Mvalues_prepped <-betas_update_1%>%
  t()%>%
  logit2()%>%
  t()%>%
  as.data.frame()

###Ensure colums are finite
Mvalues_prepped_1 <- Mvalues_prepped[ ,is.finite(colSums(Mvalues_prepped))]

###Wrapper for EWAS output
beta_pval <- function(x){
  data.frame(estimate=summary(x)$coefficients[1,1],exp=summary(x)$coefficents[1,2],se=summary(x)$coefficients[1,3],z_stat=summary(x)$coefficients[1,4],p_val=summary(x)$coefficients[2,5])
}

###Prepare annotation file
data("IlluminaHumanMethylationEPICanno.ilm10b4.hg19")
anno       <- IlluminaHumanMethylationEPICanno.ilm10b4.hg19 %>%
  getAnnotation %>%
  as.data.frame%>%
  rownames_to_column(var="cpg")

###Set Regressors
regressors<-names(Mvalues_prepped)

##Combine dataframes for analysis
Mvalues <- bind_cols(outcomes, Mvalues_prepped)

###Run conditional regression model
ewas <- lapply(regressors,function(cpg){c(data.frame(cpg_name=cpg),summary(
  clogit(case_status ~ Mvalues[,cpg] + chip + CD4T + CD8T + Bcell + NK + Gran + Mono + nRBC + cluster(Pair_ID), data = Mvalues, method = 'approximate'))$coefficients[1,1:6])})

###Annotate EWAS results and sort by P-value
ewas <- ewas%>%
  bind_rows%>%
  left_join(anno, by=c("cpg_name"="cpg"))

###Save file
write_rds(ewas, "EWAS_conditional_regression_output.rds")


