library(reshape2)

#### Load GESIS to Weimar TCU crosswalk
load("/data_weimar/crosswalks/King-to-GESIS.RData")
# gesisKRNRList

#### Load GESIS to DR1924 map crosswalk
load("data_weimar/crosswalks/GESIS-to-DR1924.RData")
# mapGESISList & mapGESISListDF

### we need to create a crosswalk between the TCU and the map
# iterate over gesisKRNR and keep only those GESIS codes that are also on the map (since not all GESIS codes have population in 1924)

# melt gesisKRNRList
gesisKRNRDF <- stack(gesisKRNRList)
colnames(gesisKRNRDF) <- c("gesisKRNR","aggKRNR")

# check if all KRNR codes from map are in gesisKRNRList
which(mapGESISListDF$gesisKRNR %in% gesisKRNRDF$gesisKRNR ==FALSE)

# extract GESIS units that are on map
mapGESISAggCW<- gesisKRNRDF[which(gesisKRNRDF$gesisKRNR %in% mapGESISListDF$gesisKRNR),]
length(unique(mapGESISAggCW$aggKRNR)) # 687 total aggregated units (matches the total, so no units lots by this subset)

### assign map codes to mapGESISAggCW
row.names(mapGESISListDF) <- mapGESISListDF$gesisKRNR
mapGESISAggCW$mapID <- mapGESISListDF[as.character(mapGESISAggCW$gesisKRNR),"mapID"]

### 3 columns in mapGESISAggCW now:
# gesisKRNR
# aggKRNR
# mapID

mapGESISAggCW$aggKRNR <- as.character(as.vector(mapGESISAggCW$aggKRNR))

### check if there are aggKRNR mapped to multiple mapIDs:
sum(sapply(as.list(by(mapGESISAggCW,mapGESISAggCW$aggKRNR,function(x) unique(x[,"mapID"]))), function(x) length(x)>1))
# 216 aggKRNRs consist of multiple mapIDs
# in this case, we want to aggregate mapIDs to match the aggKRNRs since map is more disaggregated than the TCUs 

### are there multiple aggKRNRs mapped to mapIDs?
### this would be a problem because would have to aggregate aggKRNRs together further to match map (or find map with finer units)
mapID.AggKRNR.List<- as.list(by(mapGESISAggCW,mapGESISAggCW$mapID,function(x) unique(x[,"aggKRNR"])))
mapID.AggKRNR.List[sapply(mapID.AggKRNR.List,function(x) length(x)>1)]
# none!

### save file
save(mapGESISAggCW, file="data_weimar/crosswalks/TCU-to-DR1924.RData")