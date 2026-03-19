################################################################################
### Script to create collapsed version of Weimar dataset
### Merge GESIS units to create time-constant units (TCUs) for Weimar period
### Aggregate both Wahldaten and Sozialdaten from GESIS
### 1246 units to 687 units
################################################################################

library(foreign)

gesis <- read.dta("data_weimar/raw_data/ZA8013_Wahldaten.dta")
nrow(gesis)
gesis$agglvl <- as.character(as.vector(gesis$agglvl))
gesis$name <- as.character(as.vector(gesis$name))
### drop gemeinde from GESIS data
gesis <- gesis[gesis$agglvl!="GEMEINDEN AB 2000 E." & gesis$agglvl!="RESTKREISE (GEM.< 20",]
nrow(gesis) #1246 kreise
### set missing values to NAs
gesis[gesis=="-9"] <- NA

gesis.soc <- read.dta("data_weimar/raw_data/ZA8013_Sozialdaten.dta")
nrow(gesis.soc)
gesis.soc$agglvl <- as.character(as.vector(gesis.soc$agglvl))
gesis.soc$name <- as.character(as.vector(gesis.soc$name))
### drop gemeinde from GESIS data
gesis.soc <- gesis.soc[gesis.soc$agglvl!="GEMEINDEN AB 2000 E." & gesis.soc$agglvl!="RESTKREISE (GEM.< 20",]
nrow(gesis.soc) #1246 kreise
### set missing values to NAs
gesis.soc[gesis.soc=="-9"] <- NA

# check if units in GESIS and GESIS Sozialdaten are identical
identical(gesis$krnr,gesis.soc$krnr)

###########################
### create aggregated data - NOT YET UPDATED
###########################
# load crosswalks of GESIS units to aggregated units
load("data_weimar/crosswalks/King-to-GESIS.RData")

length(gesisKRNRList)

# define which columns of data we want to pull
dataToGetGESIS <- c(colnames(gesis)[(which(colnames(gesis)=="n245pop")):(which(colnames(gesis)=="n333x"))])
dataToGetGESIS.Soc <- c(colnames(gesis.soc)[(which(colnames(gesis.soc)=="c25wohn")):(which(colnames(gesis.soc)=="c33lv"))])

# create holder for aggregated data
aggWeimarData <-  as.data.frame(matrix(data=NA, ncol=(length(dataToGetGESIS)+length(dataToGetGESIS.Soc)), nrow=length(gesisKRNRList))) 
colnames(aggWeimarData) <- c(dataToGetGESIS,dataToGetGESIS.Soc)
rownames(aggWeimarData) <- names(gesisKRNRList)

# iterate over data frame and sum corresponding data from GESIS to create aggregate units
for(l in 1:length(gesisKRNRList)){
		aggKRNR.l <- names(gesisKRNRList)[l]
		gesis.temp <- as.matrix(gesis[which(gesis$krnr %in% gesisKRNRList[[l]] ==TRUE),dataToGetGESIS])
		gesis.soc.temp <- as.matrix(gesis.soc[which(gesis.soc$krnr %in% gesisKRNRList[[l]] ==TRUE),dataToGetGESIS.Soc])
		vote.sums <- colSums(gesis.temp,na.rm=TRUE)
		soc.sums <- colSums(gesis.soc.temp, na.rm=TRUE)
		aggWeimarData[l,] <- c(vote.sums,soc.sums)
	}

### save aggregate data
save(aggWeimarData, file="data_weimar/aggregated_data/aggWeimarData.RData")

