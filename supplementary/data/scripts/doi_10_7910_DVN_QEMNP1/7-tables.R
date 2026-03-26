#########################################################
# Policy Diffusion: The Issue-Definition Stage
# Fabrizio Gilardi, Charles R. Shipan & Bruno Wueest
# Make tables
# 2019-11-18
#########################################################


rm(list = ls())
setwd("/Users/fgilardi/Downloads/AJPS-final/")

library(stm)
library(texreg)
library(dplyr)
library(xtable)
library(readxl)

# sessionInfo()
# R version 3.6.1 (2019-07-05)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS Catalina 10.15.1

# Matrix products: default
# BLAS:   /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRblas.0.dylib
# LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib

# locale:
# [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     

# other attached packages:
# [1] readxl_1.3.1   xtable_1.8-4   dplyr_0.8.3    texreg_1.36.23 stm_1.3.4     

# loaded via a namespace (and not attached):
#  [1] Rcpp_1.0.3        lattice_0.20-38   crayon_1.3.4      assertthat_0.2.1  cellranger_1.1.0 
#  [6] grid_3.6.1        R6_2.4.1          magrittr_1.5      pillar_1.4.2      rlang_0.4.1      
# [11] data.table_1.12.6 Matrix_1.2-17     glue_1.3.1        purrr_0.3.3       compiler_3.6.1   
# [16] pkgconfig_2.0.3   tidyselect_0.2.5  tibble_2.1.3 



# Table A.1 (Appendix A.1, Corpus description) --------------------------------------------------------

# get the list of folders (corresponding to the 49 newspapers) in the repo
folders <- list.dirs("./corpus", full.names = TRUE, recursive = FALSE)

# loop over folders
tableA1 <- data.frame(Newspaper = NA, Articles = NA, Paragraphs = NA, Filtered = NA)
for (f in folders) {
  # extract newspaper name
  name <- gsub("./corpus/", "", f)
  # get the list of paragraphs (each saved in files with the ID as filename)
  fnParagraphs <- list.files(paste0(f, "/pars"), full.names = T, recursive = F)
  # extract year and article IDs from the paragraph filenames
  fnParagraphs <- data.frame(year=sapply(strsplit(fnParagraphs, "-"), "[[", 2),
                             path = fnParagraphs,
                             article = sapply(strsplit(fnParagraphs, "/"), "[[", 5),
                             stringsAsFactors = F)
  fnParagraphs$article <- gsub("-[[:digit:]]+.xml$", " ", fnParagraphs$article)
  # exclude paragraphs before 1995 and after 2013
  fnParagraphs <- subset(fnParagraphs, year < 2014)
  fnParagraphs <- subset(fnParagraphs, year > 1995)
  # count number of paragraphs and articles
  nParagraphs <- length(fnParagraphs$path)
  nArticles <- length(unique(fnParagraphs$article))
  # get list of relevant paragraphs
  fnRelevant <- read.csv(paste0("relevant/", name, "_relevant.txt"), header = FALSE, stringsAsFactors = FALSE)
  fnRelevant <- data.frame(year = sapply(strsplit(fnRelevant[,1], "-"), "[[", 2),
                           path = fnRelevant[,1],
                           stringsAsFactors = FALSE)
  fnRelevant <- subset(fnRelevant, year < 2014)
  fnRelevant <- subset(fnRelevant, year > 1995)
  # count relevant paragraphs 
  nRelevant <- length(fnRelevant$path)
  # add all counts to the table
  tableA1 <- rbind(tableA1, c(name, nArticles, nParagraphs, nRelevant))
}
tableA1 <- tableA1[-1,]


# tidy up table for printing
# colnames(tableA1) <- c("Newspaper", "Articles", "Paragraphs", "Filtered")
tableA1$Articles <- as.numeric(tableA1$Articles)
tableA1$Paragraphs <- as.numeric(tableA1$Paragraphs)
tableA1$Filtered <- as.numeric(tableA1$Filtered)

# Sum numbers for St. Petersburg Times and Tampa Bay Times
# (same newspaper, changed name during the research period)
tableA1$Articles[tableA1$Newspaper == "TampaBayTimes"] <- tableA1$Articles[tableA1$Newspaper == "StPetersburgTimes"] +
                                                              tableA1$Articles[tableA1$Newspaper == "TampaBayTimes"]
tableA1$Paragraphs[tableA1$Newspaper == "TampaBayTimes"] <- tableA1$Paragraphs[tableA1$Newspaper == "StPetersburgTimes"] +
                                                              tableA1$Paragraphs[tableA1$Newspaper == "TampaBayTimes"]
tableA1$Filtered[tableA1$Newspaper == "TampaBayTimes"] <- tableA1$Filtered[tableA1$Newspaper == "StPetersburgTimes"] +
                                                              tableA1$Filtered[tableA1$Newspaper == "TampaBayTimes"]
tableA1 <- tableA1[tableA1$Newspaper != "StPetersburgTimes",]


# add state
tableA1 <- tableA1 %>% mutate(State=recode(Newspaper, `AlbuquerqueJournal`="NM",
  `ArgusLeader`="SD", `ArizonaRepublic`="AZ", `AtlantaJournal`="GA",
  `AustinAmericanStatesman`="TX", `TheBaltimoreSun2`="MD", `TheBillingsGazette`="MT",
  `BirminghamNews`="AL", `BismarckTribune2`="ND", `BostonGlobe`="MA",
  `TheBurlingtonFreePress`="VT", `CharlestonGazette`="WV", `ChicagoTribune`="IL",
  `TheClarionLedger`="MS", `CourierJournal`="KY", `DailyNewsNewYork`="NY",
  `DaytonDailyNews`="OH", `DemokratGazette`="AR", `DenverPost`="CO",
  `DesMoinesRegister`="IA", `DeseretNewsSaltLakeCity`="UT", `DetroitFreePress`="MI",
  `HartfordCourant`="CT", `HonoluluAdvertiser`="HI", `IdahoFallsPostRegister`="ID",
  `IndianapolisStar`="IN", `MilwaukeeJournalSentinel`="WI", `LasVegasReviewJournal`="NV",
  `LosAngelesTimes`="CA", `TheNewsJournal`="DE", `Record`="NJ",       
  `DailyOklahoman`="OK", `OmahaWorldHerald`="NE", `TheOregonian`="OR",
  `PhiladelphiaInquirer`="PA", `PortlandPressHeraldMaine`="ME",
  `PostAndCourier`="SC", `ProvidenceJournal`="RI", `RichmondTimesDispatch`="VA",
  `TheSeattleTimes`="WA", `StLouisPostDispatchMissouri`="MI", `StarTribuneMinneapolis`="MN",
  `TampaBayTimes`="FL", `TheTennessean`="TN",
  `TheTimesPicayune`="LA", `TopekaCapitalJournal`="KS", `TheUnionLeader`="NH",
  `WilmingtonStarNews`="NC", `WyomingTribuneEagle`="WY"
))

# recode newspaper names
tableA1 <- tableA1 %>% mutate(Newspaper=recode(Newspaper,
  `AlbuquerqueJournal`="Albuquerque Journal", `ArgusLeader`="Argus Leader",
  `ArizonaRepublic`="Arizona Republic", `AtlantaJournal`="Atlanta Journal-Constitution",
  `AustinAmericanStatesman`="Austin American-Statesman", `TheBaltimoreSun2`="Baltimore Sun",
  `TheBillingsGazette`="Billings Gazette", `BirminghamNews`="Birmingham News",
  `BismarckTribune2`="Bismarck Tribune", `BostonGlobe`="Boston Globe",
  `TheBurlingtonFreePress`="Burlington Free Press", `CharlestonGazette`="Charleston Gazette-Mail",
  `ChicagoTribune`="Chicago Tribune", `TheClarionLedger`="Clarion Ledger",
  `CourierJournal`="Courier Journal", `DailyNewsNewYork`="Daily News",
  `DaytonDailyNews`="Dayton Daily News", `DemokratGazette`="Democrat-Gazette",
  `DenverPost`="Denver Post", `DesMoinesRegister`="Des Moines Register",
  `DeseretNewsSaltLakeCity`="Deseret News", `DetroitFreePress`="Detroit Free Press",
  `HartfordCourant`="Hartford Courant", `HonoluluAdvertiser`="Honolulu Star-Advertiser",
  `IdahoFallsPostRegister`="Idaho Falls Post Register", `IndianapolisStar`="Indianapolis Star",
  `MilwaukeeJournalSentinel`="Journal Sentinel", `LasVegasReviewJournal`="Las Vegas Review-Journal",
  `LosAngelesTimes`="Los Angeles Times",
  `TheNewsJournal`="News Journal", `Record`="North Jersey Record",       
  `DailyOklahoman`="Oklahoman", `OmahaWorldHerald`="Omaha World-Herald",
  `TheOregonian`="Oregonian", `PhiladelphiaInquirer`="Philadelphia Inquirer",
  `PortlandPressHeraldMaine`="Portland Press Herald", `PostAndCourier`="Post and Courier",
  `ProvidenceJournal`="Providence Journal", `RichmondTimesDispatch`="Richmond Times-Dispatch",
  `TheSeattleTimes`="Seattle Times", `StLouisPostDispatchMissouri`="St.Louis Post-Dispatch",
  `StarTribuneMinneapolis`="Star Tribune",         
  `TampaBayTimes`="Tampa Bay Times", `TheTennessean`="Tennessean",
  `TheTimesPicayune`="Times-Picayune", `TopekaCapitalJournal`="Topeka Capital-Journal",
  `TheUnionLeader`="Union Leader", 
  `WilmingtonStarNews`="Wilmington Star-News", `WyomingTribuneEagle`="Wyoming Tribune Eagle"
  ))

# reorder columns
tableA1 <- tableA1[,c("Newspaper", "State", "Articles", "Paragraphs", "Filtered")]

tableA1 <- tableA1 %>% filter(!(Newspaper %in% c("NewYorkTimes", "USATODAY", "WallStreetJournal", "WashingtonPost")))

# add totals
tableA1 <- rbind(tableA1, c("Total", "", sum(tableA1$Articles), sum(tableA1$Paragraphs), sum(tableA1$Filtered)))

# format numerics
tableA1$Articles <- prettyNum(tableA1$Articles, big.mark=",")
tableA1$Paragraphs <- prettyNum(tableA1$Paragraphs, big.mark=",")
tableA1$Filtered <- prettyNum(tableA1$Filtered, big.mark=",")

print(tableA1)
table_A1 <- xtable(tableA1, type = "latex")
print(table_A1, file = "./tables/Table-A1.tex", include.rownames = FALSE)


# Table A.2 (Appendix A.1, Corpus description) --------------------------------------------------------

# read the crowd coding judgements
crowd_anno <- read.csv('./crowdcoding/f892245.csv')

# aggregate to average crowd judgement
crowd_mean <- crowd_anno %>%
  group_by(id) %>%
  summarise(judgement = sum(relevant, na.rm = T), count = n())
crowd_mean$judgement <- crowd_mean$judgement / crowd_mean$count

# the test paragraphs were annotated much more than five times, so we 
# exclude them for the evaluation
crowd_mean <- crowd_mean[crowd_mean$count < 6, ]

# load and attach manual evaluation of crowdd coding
crowd_mean$eval <- NA
level <- c("04", "06", "08")
for (l in level) {
  tmp <- read_excel(paste0('./crowdcoding/eval_', l, '.xlsx'))
  tmp$text <- NULL
  crowd_mean <- merge(crowd_mean, tmp, by = "id", all.x = T)
  crowd_mean$eval[!is.na(crowd_mean$relevant)] <- crowd_mean$relevant[!is.na(crowd_mean$relevant)]
  crowd_mean$relevant <- NULL
}

# make crosstable of judgements and evaluations
tableA2 <- as.data.frame(table(crowd_mean$judgement, crowd_mean$eval, useNA = "always"))
tableA2 <- tableA2[!is.na(tableA2$Var1),]

# reformat table
tableA2 <- data.frame(average.crowd.coder.judgement = tableA2$Var1[1:6],
                      N.evaluated.as.relevant = tableA2$Freq[7:12],
                      N.evaluated.as.not.relevant = tableA2$Freq[1:6],
                      clear.cases = tableA2$Freq[13:18])
tableA2$N.evaluated.as.not.relevant[tableA2$average.crowd.coder.judgement %in% c(0, 0.2)] <- tableA2$clear.cases[tableA2$average.crowd.coder.judgement %in% c(0, 0.2)]
tableA2$N.evaluated.as.relevant[tableA2$average.crowd.coder.judgement %in% c(1)] <- tableA2$clear.cases[tableA2$average.crowd.coder.judgement %in% c(1)]
tableA2$clear.cases <- NULL

# add totals
tableA2$N.overall <- tableA2$N.evaluated.as.relevant + tableA2$N.evaluated.as.not.relevant
tableA2 <- rbind(tableA2, c(NA, sum(tableA2$N.evaluated.as.relevant),
                            sum(tableA2$N.evaluated.as.not.relevant),
                            sum(tableA2$N.overall)))

# tidy up for printing
tableA2$N.evaluated.as.relevant <- prettyNum(tableA2$N.evaluated.as.relevant, big.mark=",")
tableA2$N.evaluated.as.not.relevant <- prettyNum(tableA2$N.evaluated.as.not.relevant, big.mark=",")
tableA2$N.overall <- prettyNum(tableA2$N.overall, big.mark=",")
tableA2$average.crowd.coder.judgement <- as.character(tableA2$average.crowd.coder.judgement)
tableA2$average.crowd.coder.judgement[7] <- "total"
tableA2$N.evaluated.as.relevant[1:2] <- c("-", "-")
tableA2$N.evaluated.as.not.relevant[6] <- "-"

print(tableA2)
table_A2 <- xtable(tableA2, type = "latex")
print(table_A2, file = "./tables/Table-A2.tex", include.rownames = FALSE)


# Table A.3 (Appendix A.4, Evaluation of the support vector classification filter) --------------------------------------------------------

# Prerequisites
# 0. install dependencies
#   A. Xcode
#      a. open Terminal
#      b. enter <xcode-select --install>
#   B. X11
#      a. download and launch https://dl.bintray.com/xquartz/downloads/XQuartz-2.7.11.dmg
#      b. run the pkg installer
#   C. Homebrew
#      a. open Terminal
#      b. enter </usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)">
#   D. python 3.7.:
#      a. open Terminal
#      b. enter <brew install python3>
# 2. install python modules 
#   A. open Terminal
#   B. enter:
#      <pip3 install 'pandas==0.25.3'>
#      <pip3 install 'nltk==3.4.5'>
#      <pip3 install 'numpy==1.17.3'>
#      <pip3 install 'scikit-learn==0.19.2'>
#      <pip3 install 'joblib==0.14.0'>
#      <pip3 install 'lxml==4.3.3'>
# 3. install nltk packages
#   A. open Terminal
#   B. enter <python3>
#   C. enter:
#      <import nltk>
#      <nltk.download('stopwords')>
#      <nltk.download('punkt')>
# 4. get full path of the right Python interpreter 
#   A. open Terminal
#   B. enter <python3>
#   C. enter:
#      <import sys>
#      <print(sys.executable)>
#   D. copy path and replace if necessary

# define path to Python interpreter
# pythonPath <- "/Library/Frameworks/Python.framework/Versions/3.7/bin/python3" 
pythonPath <- "/usr/local/opt/python/bin/python3.7"

# run the suppport vector machine classifier on the paragraphs for which we have
# crowdcoding annotations
system2(pythonPath, args="./evaluation/classification/clf_evaluation.py")

# load and display evaluation data
clf <- read.csv("./evaluation/classification/classification_evaluation.txt", header = F)
clf$V1 <- gsub('avg / total', 'Average', clf$V1)
clf$V1 <- gsub('[[:space:]]+', ' ', clf$V1)
clf$V1 <- gsub('^[[:space:]]|[[:space:]]$', '', clf$V1)
clf$V1[1] <- paste0(' ', clf$V1[1])
clf <- data.frame(do.call(rbind, strsplit(clf$V1, " ", fixed=TRUE)), stringsAsFactors = F)
clf$X4 <- NULL
clf[2,1] <- "Irrelevant"
clf[3,1] <- "Relevant"
clf <- clf[-c(1),]
colnames(clf) <- c("", "Precision", "Recall", "N.held-out.set")

print(clf)
table_A3 <- xtable(clf, type = "latex")
print(table_A3, file = "./tables/Table-A3.tex", include.rownames = FALSE)

# Table C.1 (Appendix C.5) --------------------------------------------------------

# Load data

load("./output/STM_model_network.RData") #STM
load("./corpus/corpus_txt.RData") # corpus

# Combine the document-topic loadings (theta) with metadata to create a data.table

dt <- make.dt(STM, meta = corpus$meta)

# Compute Herfindahl index

dt$hhi <- NA

for(i in 1:nrow(dt)){
	t <- 100*dt[i,2:13]
	hhi <- sum(t^2)
	dt$hhi[i] <- hhi
}

# Estimate regression

out <- lm(hhi ~ Policy_Spatial_Lag_Network_Pcent, data = dt)
summary(out)

table_C1 <- texreg(out)
print(table_C1, file = "./tables/Table-C1.tex", include.rownames = FALSE)


# Table D.1 (Appendix D, Extrapolation of diffusion networks) --------------------------------------------------------

# Load extrapolation data
load("./covariates/extrapolation/one_out_results.RData")
load("./covariates/extrapolation/two_out_results.RData")
load("./covariates/extrapolation/three_out_results.RData")
load("./covariates/extrapolation/four_out_results.RData")
outs <- list(one_output, two_output, three_output, four_output)

# define best models and thresholds

best_models <- c(19, 130, 107, 107)
thresholds <- c(13, 6, 9, 5)

# extract numbers of the evaluation

precrec <- matrix(NA, nrow = 4, ncol = 3)
colnames(precrec) <- c("Precision", "Recall", "F1.score")
rownames(precrec) <- as.character(c(2010:2013))
for(i in 1:4) {
  j <- best_models[i]
  precrec[i, c(1:2)] <- outs[[i]][[j]]$pr[, thresholds[i]]
  precrec[i, 3] <- outs[[i]][[j]]$f1[thresholds[i]]
}

# print table

print(round(precrec, 2))
table_D1 <- xtable(round(precrec, 2), type = "latex")
print(table_D1, file = "./tables/Table-D1.tex", include.rownames = FALSE)

