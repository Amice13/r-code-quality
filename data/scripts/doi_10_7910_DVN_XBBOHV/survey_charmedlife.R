
rm(list=ls())

# set curcomp to directory of folder holding rep_charmedlife
curcomp <- c("C:/Users/iosgood/Dropbox/CRsurvey/code/rep_charmedlife/"); 

# load packages
library(plyr)

#########################
## Load and merge data ##
#########################

cr <- read.csv(paste(curcomp, "survey/crsurvey_full.csv", sep = ""), header = TRUE)
cr$obsnum <- 1:nrow(cr) 
cur <- cr

# load in and merge the isic codes
isic <- read.csv(paste(curcomp, "survey/repondents_isic.csv", sep = ""), header = TRUE)
isic[,c("isic","isic_2","isic_3","isic_4")] <- apply(isic[,c("isic","isic_2","isic_3","isic_4")], 2, gsub, pattern = "\\.00", replacement = "") 
isic[,c("isic","isic_2","isic_3","isic_4")] <- apply(isic[,c("isic","isic_2","isic_3","isic_4")], 2, gsub, pattern = "\\.99", replacement = "") 
isic[,c("isic","isic_2","isic_3","isic_4")] <- apply(isic[,c("isic","isic_2","isic_3","isic_4")], 2, gsub, pattern = "\\.01", replacement = "") 

# isic codes begininng with 0
baddies <- apply(isic[,c("isic","isic_2","isic_3","isic_4")], 2, nchar) < 4 & apply(isic[,c("isic","isic_2","isic_3","isic_4")], 2, is.na) == FALSE
isic[,c("isic","isic_2","isic_3","isic_4")][baddies] <- paste("0", isic[,c("isic","isic_2","isic_3","isic_4")][baddies], sep = "")

# merge isic codes with crsurvey
cur <- join(cur, isic, by = "V1")
names(cur)[(ncol(cur)-3):ncol(cur)] <- c("isic","isic_2","isic_3","isic_4")
names(cur)[names(cur) == "proccode.x"] <- "proccode"

# merge in the covariates
covs <- read.csv(paste(curcomp, "covariates/covariates_charmedlife.csv", sep = ""), header = TRUE)
covs <- covs[,c("V1", "main_imp", "main_exp", "all_imp", "all_exp",   
  "rcaimp2", "ca2", "diff",
  "nyear", "totexp", "avgannexp", "nmarkets", "nprod", "avgmfn",
  "indrank", "indrank2", "indrank3", "avgannexps", "totexps", "nmarket")]
cur <- merge(cur, covs, by = "V1", all.x = TRUE)

# reorder
cur <- cur[cur$obsnum,]

# responded twice to the survey: this list records the response (of two) to be dropped b/c they are highly incomplete or second
resptwice <- c("R_3lKP5vVopq27Q5T", "R_ewwjPKYf1mQtVkx", "R_8wVpt534HFyvbG5","R_7QwtntdSZ7ONCaV",
  "R_787WpeOkIatHWkZ", "R_8umeWuf3qISiJcV", "R_85MeGXniJgeyA6h", "R_0Sy2CDRAw2ddHql", "R_5pDS87UuBf5ekU5",
  "R_9YwTFMe01hCQIDP","R_38WL2rp1ocK9c1v","R_0qEMbRZcn0W3gMd","R_3UdWL2nJMFa61hP","R_ctKLc9LF3hANh4x")
# resptwice %in% cur$V1 # should be all trues

# cut out firms that responded twice by removing second reponse or mainly incomplete response
cur <- cur[cur$V1 %in% resptwice == FALSE,]

##########################
## Define new variables ##
##########################

# create variable for the version of the survey
cur$wave <- as.numeric(substr(cur$V9, 1, 2) %in% c("5/","6/"))

# does respondent produce a tradable good (ag/mining/mnftr)
cur$amm <- amm <- substr(cur$isic, 1, 1) %in% c(0,1,2,3); 

# two variables for whether firm ever exports
cur$exporter <- cur$proccode != ""; cur$exporter[is.na(cur$exporter)] <- 0 ; cur$exporter <- as.numeric(cur$exporter)
cur$exporter2 <- as.numeric(cur$geosales_3 > 0 | cur$geosales_2 > 0)

# reorder recip so that higher numbers are greater benefits from trade
cur$recip2 <- abs(as.numeric(cur$recip) - 6)
# create factor version of recip2 for ordinal logit models
cur$recip3 <- factor(cur$recip2, levels = c(1,2,3,4,5))

# collapse responses on disagreements within industry variables
cur$hetero2 <- NA; cur$hetero2[cur$hetero %in% c(1,2)] <- 0; cur$hetero2[cur$hetero %in% c(4,3)] <- 1; 
cur$hetero2[cur$heterodk %in% c(1,2)] <- 0; cur$hetero2[cur$heterodk %in% c(3,4)] <- 1;
cur$asshet2 <- NA; cur$asshet2[cur$asshet %in% c(1,2)] <- 0; cur$asshet2[cur$asshet %in% c(4,3)] <- 1; 
cur$asshet2[cur$asshetdk %in% c(1,2)] <- 0; cur$asshet2[cur$asshetdk %in% c(3,4)] <- 1;

# renumber political activity variables so that a 0 refers to no activity, 1 to activity
polact <- NA; polact[cur$polac == 1] <- 1; polact[cur$polac == 2] <- 0; cur$polact <- polact
trdpolact <- NA; trdpolact[cur$polactr_1 == 1] <- 1; trdpolact[cur$polactr_1 == 2] <- 0; cur$trdpolact <- trdpolact
trdpolact2 <- NA; trdpolact2[cur$polactr_1 == 1] <- 1; trdpolact2[cur$polact == 0] <- 0; cur$trdpolact2 <- trdpolact2

# change up ordering of imports so that 0 means does not import, 1 means imports
import <- cur$import; import[cur$import == 2] <- 0; cur$import <- import

# reorder the preference intensity variables so that 5 means is more important (or likely), 1 is less
cur$trdpolimpt <- 6 - cur$prefint1
cur$trdseekinfo <- 6 - cur$prefint2

# turn proddif into measure of substitutability, so higher numbers means less differentiated
cur$proddif <- 5 - cur$proddif

# turn fp1 measure into a 0 (rather than 2) if firm has no foreign prod facilities
cur$fp1[cur$fp1==2] <- 0

# construct new variable which is average of support for other responses on trade_ question
cur$tradesup <- apply(cur[,c("trade_1", "trade_2", "trade_3", "trade_4", "trade_5", "trade_6", "trade_7", "trade_8")], 1, mean, na.rm = TRUE) 
cur$tradesup[is.nan(cur$tradesup)] <- NA

# number of respondents per two-digit ISIC code
cur$isic2dig <- substr(cur$isic, 1, 2)
isic2digs <- unique(cur$isic2dig)
cur$nfirmisic2 <- NA
for(i in isic2digs){
  nfirms <- sum(cur$isic2dig == i, na.rm = TRUE)
  cur$nfirmisic2[cur$isic2dig == i] <- nfirms
}

########################
## Produce final data ##
########################

vars <- c("V1","amm","isic","indrank","indrank2","indrank3","wave",
  "exporter","avgannexps","totexps","nmarkets",
  "recip3","hetero2","togeth","asshet2",
  "trdpolimpt","trdseekinfo","polact","trdpolact",
  "diff","fp1","import","proddif",
  "main_imp","main_exp","all_imp","all_exp","ca2",
  "disagg_1","disagg_2","effic_1","prod_1","exporter2",
  "wtolib","tradesup","rcaimp2") 
crs <- cur[,vars]

write.csv(crs, paste(curcomp, "repdata_charmedlife.csv", sep = ""), row.names = FALSE)




