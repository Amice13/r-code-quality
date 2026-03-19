setwd("~/Research/debian_underprod/quality_metameta")
source('lib-00-utils.R')
source('data_ingest_and_clean.R') #
dataPath="~/Dropbox/Apps/Overleaf/Software Quality Metareview/knitr_rdata/"

primaryInitial <- 870 + 198 + 2216 + 5867 + 765
snowballInitial.1 <- 227
snowballInitial.2 <- 87
snowballInitial.3 <- 4

primaryDedup <- 7656
snowballDedup.1 <- 106
snowballDedup.2 <- 66 - 18
snowballDedup.3 <- 1

#in primary, automatic doesn't apply
snowballAuto.1  <- snowballDedup.1 - 13
snowballAuto.2 <- snowballDedup.2 - 11
snowballAuto.3 <- snowballDedup.3

primaryScreenTitle <- 627
snowballTitle.1 <- snowballAuto.1 - 5
snowballTitle.2 <- snowballAuto.2 - 1
snowballTitle.3 <- snowballDedup.3

primaryScreenAbstract <- 194
snowballAbstract.1 <- snowballTitle.1 - 8
snowballAbstract.2 <- snowballTitle.2 - 3
snowballAbstract.3 <- snowballTitle.3

#not reporting primary screen title and conclusions, that turned into fulltext skim in reality

primaryScreenFulltext <- 125 - 11 - 13 - 38 - 6 -3 #includes everything eliminated for being bealls, conf paper, closer read, graylit. last 3 here and the -1 below are those 4 elim for not having a quality def/analysis
snowballFulltext.1 <- snowballAbstract.1 - 61 - 1 
snowballFulltext.2 <- snowballAbstract.2 - 30
snowballFulltext.3 <- snowballAbstract.3 - 1

#no access issues in the end
primaryFinal <- primaryScreenFulltext 
snowballFinal.1 <- snowballFulltext.1 
snowballFinal.2 <- snowballFulltext.2 
snowballFinal.3 <- snowballFulltext.3


initialDedupCount <- primaryDedup + snowballDedup.1 + snowballDedup.2 + snowballDedup.3
finalCount <- primaryFinal + snowballFinal.1 + snowballFinal.2 + snowballFinal.3

#synthesis counting
methodMap <- list()
methodMap[["byStudy"]] <- length(cleanDF$by_study[cleanDF$by_study=="1"])
methodMap[["byTheme"]] <- length(cleanDF$theme[cleanDF$theme=="1"])
methodMap[["byVote"]] <- length(cleanDF$vote_count[cleanDF$vote_count=="1"])
methodMap[["trueMeta"]] <- length(cleanDF$true_quant[cleanDF$true_quant=="1"])

#reproducible research nose-counting
repoMap <- list()
repoMap[["refs"]] <- length(cleanDF$data_avail[cleanDF$data_avail=="references"])
repoMap[["allData"]] <- length(cleanDF$data_avail[cleanDF$data_avail=="all data"])
repoMap[["gone"]] <- length(cleanDF$data_avail[cleanDF$data_avail=="external-gone"])
repoMap[["none"]] <- length(cleanDF$data_avail[cleanDF$data_avail=="no"])

#secondary method counting
secMethMap <- list()
secMethMap[["SLR"]] <- length(cleanDF$method[cleanDF$method=="SLR"])
secMethMap[["map"]] <- length(cleanDF$method[cleanDF$method=="map"])
secMethMap[["meta"]] <- length(cleanDF$method[cleanDF$method=="meta-analysis"])
secMethMap[["other"]] <- length(cleanDF$method[cleanDF$method=="other"])

#bucket counting

bucketMap <- list()
bucketMap[["smell"]] <- length(cleanDF$bucket[cleanDF$bucket=="Bad-smell"])
bucketMap[["holistic"]] <- length(cleanDF$bucket[cleanDF$bucket=="Holistic"])
#bucketMap[["other"]] <- length(cleanDF$bucket[cleanDF$bucket=="Other"])
#print("This should be zero:")
#print(bucketMap[["other"]])
bucketMap[["longitudinal"]] <- length(cleanDF$bucket[cleanDF$bucket=="Longitudinal"])
bucketMap[["dependable"]] <- length(cleanDF$bucket[cleanDF$bucket=="Dependability"])
#bucketMap[["method"]] <- length(cleanDF$bucket[cleanDF$bucket=="Method-driven"])
#print("this should also be zero:")
#print(bucketMap[["method"]])
bucketMap[["prediction"]] <- length(cleanDF$bucket[cleanDF$bucket=="Defect-Prediction"])

#let's make some little tables 

library(texreg)
library(kableExtra)
library(tidyverse)
library(xtable)


gT <- table(str_to_title(cleanDF$journal_or_conf))
gT <- data.frame(gT[order(gT, decreasing=T)])
colnames(gT) <- c("Venue", "Count")

con <- textConnection("venTex", "w") #how we save R objects to strings
sink(con, split=TRUE, type="output")
#kable(gT, "latex", col.names=c("Type", "Count"), align=c('l','r'), booktabs='T', linesep="")
print(xtable(gT), comment=FALSE, only.contents=TRUE, include.rownames = FALSE, type="latex", hline.after=0)
sink()
close(con);rm(con)

vT <- table(cleanDF$venue)
vT <- vT[order(vT, decreasing=T)]
vT <- data.frame(subset(data.frame(vT), Freq > 1))
colnames(vT) <- c("Venue", "Count")

con2 <- textConnection("topTex", "w") #how we save R objects to strings
sink(con2, split=TRUE, type="output")
#kable(vT, "latex", col.names=c("Venue", "Count"), align=c('z','r'), booktabs='T', linesep="")
print(xtable(vT), comment=FALSE, only.contents=TRUE, include.rownames = FALSE, type="latex", hline.after=0)
sink()
close(con2);rm(con2)

tT <- table(cleanDF$niceBucket)
tT <- tT[order(tT, decreasing=T)]
tT <- data.frame(tT)
tT$prop <- paste0(round(tT$Freq/finalCount * 100), "%") 
colnames(tT) <- c("Theme", "Count", "Proportion")

con3 <- textConnection("themeTex", "w") #how we save R objects to strings
sink(con3, split=TRUE, type="output")
#kable(tT, "latex", col.names=c("Theme", "Count", "Proportion"), align=c('l','r', 'r'), booktabs='T', linesep="")
print(xtable(tT), comment=FALSE, only.contents=TRUE, include.rownames = FALSE, type="latex", hline.after=0)
sink()
close(con3);rm(con3)

#cross-category tabulation


#lT <- as.data.frame(table(cleanDF$niceBucket, cleanDF$design_arch, cleanDF$single_product, cleanDF$multi_product, cleanDF$org_or_community))
#(table(row.names=cleanDF$niceBucket, cleanDF$design_arch, cleanDF$single_product, cleanDF$multi_product, cleanDF$org_or_community))
#lT <- as.data.frame(table(row.names=cleanDF$niceBucket, cleanDF$design_arch, cleanDF$single_product, cleanDF$multi_product, cleanDF$org_or_community))

#library(reshape2)

#recast(cleanDF, cleanDF$niceBucket ~ cleanDF$design_arch + cleanDF$single_product + cleanDF$multi_product +
#         cleanDF$org_or_community, id=cleanDF$niceBucket, fun=sum)
#recast(cleanDF, cleanDF$niceBucket ~ cleanDF$design_arch + cleanDF$single_product, margins="niceBucket")



#xtabs(niceBucket ~ design_arch + single_product + multi_product + org_or_community, data = lT)
#ftable(xtabs(~ Var1 + Var2 + Var3 + Var4 + Var5, data = lT))

#unique(cleanDF$niceBucket)
#hammerDF <- data.frame(row.names=c(unique(cleanDF$niceBucket)), (c("Design", "Single Product", "Multiple Products", "Organization")))
#n = length(unique(cleanDF$niceBucket))

library(tidyverse)
designDF <- subset(cleanDF, design_arch==1) 
designSum <- c(
               sum(designDF$bucket=="Bad-smell"),         
               sum(designDF$bucket=="Holistic"), 
               sum(designDF$bucket=="Longitudinal"),
               sum(designDF$bucket=="Defect-Prediction"), 
 #              sum(designDF$bucket=="Method-driven"),
               sum(designDF$bucket=="Dependability"))
 #              sum(designDF$bucket=="Other"))

singleDF <- subset(cleanDF, single_product==1) 
singleSum <- c(
               sum(singleDF$bucket=="Bad-smell"),         
               sum(singleDF$bucket=="Holistic"), 
               sum(singleDF$bucket=="Longitudinal"),
               sum(singleDF$bucket=="Defect-Prediction"), 
               #sum(singleDF$bucket=="Method-driven"),
               sum(singleDF$bucket=="Dependability"))
#               sum(singleDF$bucket=="Other"))

multiDF <- subset(cleanDF, multi_product==1) 
multiSum <- c(
               sum(multiDF$bucket=="Bad-smell"),         
               sum(multiDF$bucket=="Holistic"), 
               sum(multiDF$bucket=="Longitudinal"),
               sum(multiDF$bucket=="Defect-Prediction"), 
#               sum(multiDF$bucket=="Method-driven"),
               sum(multiDF$bucket=="Dependability"))
#               sum(multiDF$bucket=="Other"))

orgDF <- subset(cleanDF, org_or_community==1) 
orgSum <- c( 
               sum(orgDF$bucket=="Bad-smell"),         
               sum(orgDF$bucket=="Holistic"), 
               sum(orgDF$bucket=="Longitudinal"), #i.e. maintainability
               sum(orgDF$bucket=="Defect-Prediction"), #i.e. structural
#               sum(orgDF$bucket=="Method-driven"),
               sum(orgDF$bucket=="Dependability"))
#               sum(orgDF$bucket=="Other"))

#hammerDF <- data.frame(Category = character(), "Design"=integer(), "Single Product"=integer(), 
#                       "Multiple Products"=integer(), "Organization"=integer())

themeOrder <- c(
                "Quality is Heuristic",
                "Quality is Holistic", 
                "Quality is Maintainability",
                "Quality is Structural",
                "Quality is Dependability"
  )

hammerDF <- data.frame(Category = themeOrder, "Design"=designSum, "Single Product"=singleSum, 
                       "Multiple Products"=multiSum, "Organization"=orgSum)

colnames(hammerDF) <-  c("", "Design", "Product", "Multi. Product", "Dev. Group") 

con4 <- textConnection("layerTex", "w") #how we save R objects to strings
sink(con4, split=TRUE, type="output")
print(xtable(hammerDF), comment=FALSE, only.contents=TRUE, include.rownames = FALSE, type="latex", hline.after=0)
sink()
close(con4);rm(con4)

##this is not a nice way to do this
temp <- as.data.table(table(cleanDF$journal_or_conf))
nConference = temp[V1=='conference']$N
nJournal = temp[V1=='journal']$N
nSymposium = temp[V1=='symposium']$N
nWorking = temp[V1=='working conference']$N
nWorkshop = temp[V1=='workshop']$N




r <- list()
remember(bucketMap)
remember(themeTex)
remember(layerTex)
remember(secMethMap)
remember(methodMap)
remember(repoMap)
remember(venTex)
remember(topTex)
remember(primaryInitial) 
remember(primaryDedup)
remember(primaryScreenTitle)
remember(primaryScreenAbstract)
remember(primaryScreenFulltext)
remember(primaryFinal)
remember(snowballInitial.1)
remember(snowballDedup.1)
remember(snowballAuto.1)
remember(snowballTitle.1)
remember(snowballAbstract.1)
remember(snowballFulltext.1)
remember(snowballFinal.1)
remember(snowballInitial.2)
remember(snowballDedup.2)
remember(snowballAuto.2)
remember(snowballTitle.2)
remember(snowballAbstract.2)
remember(snowballFulltext.2)
remember(snowballFinal.2)
remember(snowballInitial.3)
remember(snowballDedup.3)
remember(snowballAuto.3)
remember(snowballTitle.3)
remember(snowballAbstract.3)
remember(snowballFulltext.3)
remember(snowballFinal.3) 
remember(initialDedupCount)
remember(finalCount)
remember(nJournal) 
remember(nConference)
remember(nSymposium)
remember(nWorking) 
remember(nWorkshop)


save(r, file=paste0(dataPath, "knitr_data.RData"), version=2)

