################################################################################################################################################
### Updated on January 24, 2018.
### merge in Weimar units (weighted) and imperial Data
################################################################################################################################################

matches <- read.csv("merge_1890const_weimar/intersected_shapefile/dr1897const-dr1924agg-intersect.csv", stringsAsFactors=FALSE) 


matches <- matches[matches$AREA>1,] #914 by 120

### start by analyzing which 1897 constituencies got split over several districts
detect_splits <- function(df){
	constituencies <- as.list(rep(NA,length(unique(df$constID))))
	names(constituencies) <- unique(df$constID)


	for(r in 1:nrow(df)){
		# original ID
		id1897 <- df[r,"constID"]
		# place Weimar code in proper place in original list
		constituencies[[as.character(id1897)]] <- as.vector(na.omit(append(constituencies[[as.character(id1897)]],df[r,"aggKRNR"])))
			}
	return(constituencies)
	}
	
weimarByConstList <- detect_splits(matches) # 355 constituencies


###### load map
library(maptools)
dr1897Const <- readShapePoly("merge_1890const_weimar/intersected_shapefile/dr1897_const.shp")
dim(dr1897Const)# 389   1

dr1897Const$constID <- as.character(as.vector(dr1897Const$constID))

row.names(dr1897Const) <- dr1897Const$constID

##################################################################################################
#### Weighting to create Weimar data for constituencies
##################################################################################################


length(unique(matches$aggKRNR)) #687 unique ones
total.areas <- stack(by(matches,matches$aggKRNR,function(x) sum(x[,"AREA"])))  #sum up areas where IDs are the same
dim(total.areas) #687 by  2
colnames(total.areas) <- c("area","krnr")
total.areas$krnr <- as.character(as.vector(total.areas$krnr))

conv.matrix <- matrix(data=0,nrow=length(total.areas$krnr),ncol=length(unique(matches$constID)))
rownames(conv.matrix) <- total.areas$krnr
colnames(conv.matrix) <- unique(matches$constID)

for(r in 1:nrow(matches)){
	id1897 <- as.character(matches[r,"constID"])
	id1924 <- as.character(matches[r,"aggKRNR"])
	area.r <- matches[r,"AREA"]
	conv.matrix[id1924,id1897] <- area.r/total.areas[total.areas$krnr==id1924,"area"]
}

# load Weimar data
load("data_weimar/aggregated_data/aggWeimarData.RData") 


names(aggWeimarData) 
dim(aggWeimarData) #Data created by Helen is 692 by 204
aggWeimarData$n206gs #There are a few zeros
aggWeimarData$n245gs #There are a few zeros, but in different places than in previous row

aggWeimarData$aggKRNR #NULL?

aggWeimarRO<- aggWeimarData[rownames(conv.matrix),1:ncol(aggWeimarData)] #ORIGINAL aggWeimarRO<- aggWeimarData[rownames(conv.matrix),3:ncol(aggWeimarData)]
for(c in 1:ncol(aggWeimarRO)){
	aggWeimarRO[,c] <- as.numeric(aggWeimarRO[,c])
}

constData<- matrix(data=NA,nrow=ncol(conv.matrix),ncol=ncol(aggWeimarRO))
rownames(constData) <- colnames(conv.matrix)
colnames(constData) <- colnames(aggWeimarRO)

#Calculate values based on % overlap
for(c in 1:ncol(conv.matrix)){
	constData[c,] <- colSums(conv.matrix[,c] * aggWeimarRO) #data is in aggWeimarRO; overlap in conv.matrix
}
constData <- as.data.frame(constData)


data.to.merge <- constData[row.names(dr1897Const),]
data.to.merge$constID <- row.names(dr1897Const)
row.names(data.to.merge) <- row.names(dr1897Const)

#dr1897Const only has one column
dr1897Const.Weighted <- SpatialPolygonsDataFrame(dr1897Const, data.to.merge)
dr1897Const.Weighted$Weimar <- 0 #New row - Weimar
dr1897Const.Weighted$Weimar[!is.na(dr1897Const.Weighted$n245pop)] <- 1 #is this column missing (it was #1 in the original file)?
dim(dr1897Const.Weighted)#389 360
names(dr1897Const.Weighted)
which(dr1897Const.Weighted$n206pop==0) #Before there were fize zeros. Using Helen's aggregation, there are none.  
dr1897Const.Weighted$n245pop


save(dr1897Const.Weighted,file="const1897-Weimar-weighted.RData") #added Version VC3 on January 24, 2018

