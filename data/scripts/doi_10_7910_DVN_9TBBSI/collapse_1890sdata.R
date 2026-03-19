################################################################################################
################################################################################################
### Script to collapse 1890s data to electoral constituency level
################################################################################################
################################################################################################

################################
### Crosswalk
################################

fullData<- read.csv("data_1890s/master/Full Dataset of German Kreise, 1890s.csv",stringsAsFactors=FALSE)

dataSub <- fullData[,c("Const.Code..auto.","Const.Name..auto.","codelandgini1895","dr_ab_1897mapcode","Unit.of.Analysis")]

################################
### some tests on the data
################################
dataSub[is.na(dataSub$codelandgini1895),] # all NA land gini codes are NA's elsewhere

dataSub[!is.na(dataSub$codelandgini1895) & is.na(dataSub$dr_ab_1897mapcode),] 
# some land gini codes have NA values for map (but it's because they're assigned to other codes that don't have NAs)

dataSub[!is.na(dataSub$codelandgini1895) & is.na(dataSub$Const.Code..auto.),] 
# all land gini codes have constituencies

### create a list of land gini by constituency
landcodeByConstituency <- as.list(by(dataSub,dataSub$Const.Code..auto.,function(x) unique(x[,"codelandgini1895"])))

# let's make sure there are no duplicates of a landgini code being assigned to multiple constituencies -- no duplicates!
length(stack(landcodeByConstituency)$values)
length(unique(stack(landcodeByConstituency)$values))
# this means that constituencies FULLY nest the land gini codes -- this means that we can just combine the 1897 map to constituency codes

### for landcodeByConstituency, names are constituencies and list values are code numbers corresponding to statistical units for landholding data

# merge constituency hamburg (372.1) with GEEST-MARSCH (373) b/c map combines Hamburg with surrounding units

landcodeByConstituency[["373"]] <- c(landcodeByConstituency[["373"]],landcodeByConstituency[["372.1"]])
landcodeByConstituency <- landcodeByConstituency[-which(names(landcodeByConstituency)=="372.1")]


#### Define function to collapse data
collapseByCode <- function(c){
	sub <- fullData[fullData$codelandgini1895 %in% c, ]
	
	### Religion data
	religion.data <- colSums(sub[,c("protestant","catholic")],na.rm=TRUE)
	  
	### Land gini
	total.num.farms <- sum(sub[,c("Number.of.Farms")],na.rm=TRUE)
	num.farms <- colSums(sub[,c("X0.small", "X0.1.small", "X2.small", "X5.small","X20.small","X50.small","X1ha","X2ha","X3ha","X4ha","X5ha","X10ha","X20ha","X50ha","X100ha","X200ha","X500ha","X1000ha")],na.rm=TRUE)
	num.farms.share <- num.farms/total.num.farms
	 
	total.farm.area <- sum(sub[,c("landtotal")],na.rm=TRUE)
	farm.area <- colSums(sub[,c("land0small","land0.1.small","land2.small","land5.small", "land20.small" ,"land50.small","land1ha","land2ha", "land3ha","land4ha", "land5ha", "land10ha","land20ha","land50ha","land100ha","land200ha","land500ha","land1000ha")],na.rm=TRUE)
	cum.area.share <- cumsum(farm.area)/total.farm.area
	 
	rect.cum.area <- cum.area.share*num.farms.share
	rect.cum.area.sum <- sum(rect.cum.area)
	 
	area.share <- farm.area/total.farm.area
	 
	rect.area <- 0.5*area.share*num.farms.share
	rect.area.sum <- sum(rect.area)
	 
	area.lower.curve <- rect.cum.area.sum-rect.area.sum
	area.between <- 0.5-area.lower.curve
	gini <- area.between/0.5
	return(c(gini,religion.data))
	}
	
	##### end of function to collapse data
	
constData <- t(sapply(landcodeByConstituency,function(x) collapseByCode(x)))
colnames(constData) <- c("landgini","prot1900","cat1900")

#### save data
save(constData, file="constituency_1890s/collapsed_data/collapsed1890s_data.RData")
