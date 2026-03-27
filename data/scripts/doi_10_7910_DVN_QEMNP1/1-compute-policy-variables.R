######################################################
# Policy Diffusion: The Issue-Definition Stage
# Fabrizio Gilardi, Charles R. Shipan & Bruno Wueest
# Compute policy variables
# 2019-11-18
######################################################


rm(list = ls())

setwd("/Users/fgilardi/Downloads/AJPS-final/")

library(spdep)
library(maps)
library(maptools)
library(Hmisc)
library(lubridate)

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
#  [1] lubridate_1.7.4 Hmisc_4.3-0     ggplot2_3.2.1   Formula_1.2-3   survival_3.1-7 
#  [6] lattice_0.20-38 maptools_0.9-8  maps_3.3.0      spdep_1.1-3     sf_0.8-0       
# [11] spData_0.3.2    sp_1.3-2       

# loaded via a namespace (and not attached):
#  [1] Rcpp_1.0.3          deldir_0.1-23       class_7.3-15        gtools_3.8.1       
#  [5] assertthat_0.2.1    digest_0.6.22       R6_2.4.1            backports_1.1.5    
#  [9] acepack_1.4.1       coda_0.19-3         e1071_1.7-2         pillar_1.4.2       
# [13] rlang_0.4.1         lazyeval_0.2.2      data.table_1.12.6   rstudioapi_0.10    
# [17] gdata_2.18.0        gmodels_2.18.1      rpart_4.1-15        Matrix_1.2-17      
# [21] checkmate_1.9.4     splines_3.6.1       stringr_1.4.0       foreign_0.8-72     
# [25] htmlwidgets_1.5.1   munsell_0.5.0       compiler_3.6.1      xfun_0.11          
# [29] pkgconfig_2.0.3     base64enc_0.1-3     htmltools_0.4.0     nnet_7.3-12        
# [33] tidyselect_0.2.5    tibble_2.1.3        gridExtra_2.3       htmlTable_1.13.2   
# [37] expm_0.999-4        crayon_1.3.4        dplyr_0.8.3         withr_2.1.2        
# [41] MASS_7.3-51.4       grid_3.6.1          nlme_3.1-142        gtable_0.3.0       
# [45] DBI_1.0.0           magrittr_1.5        units_0.6-5         scales_1.0.0       
# [49] KernSmooth_2.23-16  stringi_1.4.3       LearnBayes_2.15.1   latticeExtra_0.6-28
# [53] boot_1.3-23         RColorBrewer_1.1-2  tools_3.6.1         glue_1.3.1         
# [57] purrr_0.3.3         colorspace_1.4-1    cluster_2.1.0       classInt_0.4-2     
# [61] knitr_1.26  


## W NEIGHBORS

# Adjacency matrices (http://www.biostat.umn.edu/~brad/data/Areal.R)
# Create an adjacency matrix for the states in the US
usa.state <- map(database = "state", fill = TRUE, plot = FALSE)
state.ID <- sapply(strsplit(usa.state$names, ":"), function(x) x[1])
usa.poly <- map2SpatialPolygons(usa.state, IDs=state.ID)
usa.nb <- poly2nb(usa.poly)
usa.adj.mat <- nb2mat(usa.nb, style="B")

# Write the 0-1 adjacency matrix
W <- usa.adj.mat
W[(W > 0)] <- 1

# Change state names and reorder rows and columns
states_49 <- read.csv("./covariates/raw-data/US-states-names-abbreviations-49.csv")
rownames(W) <- colnames(W) <- states_49[,2]

W <- W[order(rownames(W)),order(colnames(W))]

# Add AK and HI
W <- rbind(rbind(0, W[1:10,]), rbind(0, W[11:49,]))
rownames(W)[c(1,12)] <- c("AK","HI")

W <- cbind(cbind(0, W[,1:10]), cbind(0, W[,11:49]))
colnames(W)[c(1,12)] <- c("AK","HI")

# Row standardize W
Ws <- W
for(i in 1:nrow(W)){
	Ws[i,] <- W[i,]/sum(W[i,])
}
Ws[c(1,12),] <- 0

## W ALL OTHER STATES

W_all <- W
W_all[W_all == 0] <- 1
diag(W_all) <- 0

# Row standardize
Ws_all <- W_all
for(i in 1:nrow(W_all)){
	Ws_all[i,] <- W_all[i,]/sum(W_all[i,])
}


## ADOPTION DATASETS

# Mayatech dataset
d <- read.csv("./covariates/raw-data/Smokefree-Air-Policy-Analysis-1994-2014.csv", as.is=TRUE)
d <- d[-c(1072:1076),-ncol(d)]
d <- data.frame(d)

d <- d[order(d$State),]

unique(d$State) == colnames(W)
unique(d$State) == rownames(W)


#### ENACTED ###########################################

# Reformat dates

d$Restaurants_Enacted_Date <- strptime(d$Restaurants_Enacted_Date, format="%d/%m/%y")
d$Restaurants_Enacted_Date <- strftime(d$Restaurants_Enacted_Date, usetz=FALSE)

d$Bars_Enacted_Date[d$Bars_Enacted_Date=="prior to 1994"] <- c("31/12/1993")
d$Bars_Enacted_Date[d$Bars_Enacted_Date=="9/27/1/1996"] <- c("9/27/1996")
d$Bars_Enacted_Date[d$Bars_Enacted_Date=="9/271996"] <- c("9/27/1996")
d$Bars_Enacted_Date <- strptime(d$Bars_Enacted_Date, format="%d/%m/%y")
d$Bars_Enacted_Date <- strftime(d$Bars_Enacted_Date, usetz=FALSE)

d$Government_Worksites_Enacted_Date <- strptime(d$Government_Worksites_Enacted_Date, format="%d/%m/%y")
d$Government_Worksites_Enacted_Date <- strftime(d$Government_Worksites_Enacted_Date, usetz=FALSE)

d$Private_Worksites_Enacted_Date <- strptime(d$Private_Worksites_Enacted_Date, format="%d/%m/%y")
d$Private_Worksites_Enacted_Date <- strftime(d$Private_Worksites_Enacted_Date, usetz=FALSE)

d$Hotels_Motels_Enacted_Date <- strptime(d$Hotels_Motels_Enacted_Date, format="%d/%m/%y")
d$Hotels_Motels_Enacted_Date <- strftime(d$Hotels_Motels_Enacted_Date, usetz=FALSE)

d$Indoor_Arenas_Enacted_Date <- strptime(d$Indoor_Arenas_Enacted_Date, format="%d/%m/%y")
d$Indoor_Arenas_Enacted_Date <- strftime(d$Indoor_Arenas_Enacted_Date, usetz=FALSE)

d$Malls_Enacted_Date <- strptime(d$Malls_Enacted_Date, format="%d/%m/%y")
d$Malls_Enacted_Date <- strftime(d$Malls_Enacted_Date, usetz=FALSE)


# Extract only policies we need (separately for each policy b/c of NAs that screw up date computations)

rest <- data.frame(d$State, d$Restaurants_Enacted_Date, d$Restaurants)
names(rest) <- c("State", "Date", "Policy")
rest <- rest[!is.na(rest$Date),]
rest$Date <- as.Date(rest$Date, format="%Y-%m-%d", tz = "", usetz == FALSE)

bars <- data.frame(d$State, d$Bars_Enacted_Date, d$Bars)
names(bars) <- c("State", "Date", "Policy")
bars <- bars[!is.na(bars$Date),]
bars$Date <- as.Date(bars$Date, format="%Y-%m-%d")

govwork <- data.frame(d$State, d$Government_Worksites_Enacted_Date, d$Government_Worksites)
names(govwork) <- c("State", "Date", "Policy")
govwork <- govwork[!is.na(govwork$Date),]
govwork$Date <- as.Date(govwork$Date, format="%Y-%m-%d")

privwork <- data.frame(d$State, d$Private_Worksites_Enacted_Date, d$Private_Worksites)
names(privwork) <- c("State", "Date", "Policy")
privwork <- privwork[!is.na(privwork$Date),]
privwork$Date <- as.Date(privwork$Date, format="%Y-%m-%d")

hotels <- data.frame(d$State, d$Hotels_Motels_Enacted_Date, d$Hotels_Motels)
names(hotels) <- c("State", "Date", "Policy")
hotels <- hotels[!is.na(hotels$Date),]
hotels$Date <- as.Date(hotels$Date, format="%Y-%m-%d")

arenas <- data.frame(d$State, d$Indoor_Arenas_Enacted_Date, d$Indoor_Arenas)
names(arenas) <- c("State", "Date", "Policy")
arenas <- arenas[!is.na(arenas$Date),]
arenas$Date <- as.Date(arenas$Date, format="%Y-%m-%d")

malls <- data.frame(d$State, d$Malls_Enacted_Date, d$Malls)
names(malls) <- c("State", "Date", "Policy")
malls <- malls[!is.na(malls$Date),]
malls$Date <- as.Date(malls$Date, format="%Y-%m-%d")


# Make state-year-month dataset

yrs <- unique(d$Year)
sts <- unique(d$State)
ym <- length(yrs)*12

d2 <- as.data.frame(rep(NA, length(yrs)*12*length(sts)))

d2$State <- NA
j <- 1
for(i in 1:length(sts)){
	d2$State[j:(j + ym - 1)] <- rep(sts[i], ym)
	j <- j + ym
}
unique(d2$State)

d2$State_Full <- NA
d2$State_Full[d2$State=="AK"] <- c("Alaska")
d2$State_Full[d2$State=="AL"] <- c("Alabama")
d2$State_Full[d2$State=="AR"] <- c("Arkansas")
d2$State_Full[d2$State=="AZ"] <- c("Arizona")
d2$State_Full[d2$State=="CA"] <- c("California")
d2$State_Full[d2$State=="CO"] <- c("Colorado")
d2$State_Full[d2$State=="CT"] <- c("Connecticut")
d2$State_Full[d2$State=="DC"] <- c("District of Columbia")
d2$State_Full[d2$State=="DE"] <- c("Delaware")
d2$State_Full[d2$State=="FL"] <- c("Florida")
d2$State_Full[d2$State=="GA"] <- c("Georgia")
d2$State_Full[d2$State=="HI"] <- c("Hawaii")
d2$State_Full[d2$State=="IA"] <- c("Iowa")
d2$State_Full[d2$State=="ID"] <- c("Idaho")
d2$State_Full[d2$State=="IL"] <- c("Illinois")
d2$State_Full[d2$State=="IN"] <- c("Indiana")
d2$State_Full[d2$State=="KS"] <- c("Kansas")
d2$State_Full[d2$State=="KY"] <- c("Kentucky")
d2$State_Full[d2$State=="LA"] <- c("Louisiana")
d2$State_Full[d2$State=="MA"] <- c("Massachusetts")
d2$State_Full[d2$State=="MD"] <- c("Maryland")
d2$State_Full[d2$State=="ME"] <- c("Maine")
d2$State_Full[d2$State=="MI"] <- c("Michigan")
d2$State_Full[d2$State=="MN"] <- c("Minnesota")
d2$State_Full[d2$State=="MO"] <- c("Missouri")
d2$State_Full[d2$State=="MS"] <- c("Mississippi")
d2$State_Full[d2$State=="MT"] <- c("Montana")
d2$State_Full[d2$State=="NC"] <- c("North Carolina")
d2$State_Full[d2$State=="ND"] <- c("North Dakota")
d2$State_Full[d2$State=="NE"] <- c("Nebraska")
d2$State_Full[d2$State=="NH"] <- c("New Hampshire")
d2$State_Full[d2$State=="NJ"] <- c("New Jersey")
d2$State_Full[d2$State=="NM"] <- c("New Mexico")
d2$State_Full[d2$State=="NV"] <- c("Nevada")
d2$State_Full[d2$State=="NY"] <- c("New York")
d2$State_Full[d2$State=="OH"] <- c("Ohio")
d2$State_Full[d2$State=="OK"] <- c("Oklahoma")
d2$State_Full[d2$State=="OR"] <- c("Oregon")
d2$State_Full[d2$State=="PA"] <- c("Pennsylvania")
d2$State_Full[d2$State=="RI"] <- c("Rhode Island")
d2$State_Full[d2$State=="SC"] <- c("South Carolina")
d2$State_Full[d2$State=="SD"] <- c("South Dakota")
d2$State_Full[d2$State=="TN"] <- c("Tennessee")
d2$State_Full[d2$State=="TX"] <- c("Texas")
d2$State_Full[d2$State=="UT"] <- c("Utah")
d2$State_Full[d2$State=="VA"] <- c("Virginia")
d2$State_Full[d2$State=="VT"] <- c("Vermont")
d2$State_Full[d2$State=="WA"] <- c("Washington")
d2$State_Full[d2$State=="WI"] <- c("Wisconsin")
d2$State_Full[d2$State=="WV"] <- c("West Virginia")
d2$State_Full[d2$State=="WY"] <- c("Wyoming")


d2$Year <- NA
yrs2 <- c()
j <- 1
for(i in 1:length(yrs)){
	yrs2[j:(j + 12 - 1)] <- rep(yrs[i], 12)	
	j <- j + 12
}
d2$Year <- rep(yrs2, length(sts))

d2$Month <- rep(seq(1:12), length(yrs)*length(sts))

d2 <- d2[,-1]

dts <- seq(from = as.Date("1994-01-01"), to = as.Date("2014-12-01"), by = "1 month")
d2$Date <- rep(dts, length(sts))
d2$Date <- as.Date(d2$Date, tz = "", usetz = FALSE)
date_min <- min(d2$Date)
date_max <- max(d2$Date)


# Policies in state itself

d2$Restaurants <- 0
d2$Bars <- 0
d2$Government_Worksites <- 0
d2$Private_Worksites <- 0
d2$Hotels <- 0
d2$Indoor_Arenas <- 0 
d2$Malls <- 0

### http://stackoverflow.com/questions/1995933/number-of-months-between-two-dates

# turn a date into a 'monthnumber' relative to an origin
monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01")); lt$year*12 + lt$mon } 

# compute a month difference as a difference between two monnb's
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }

# test
mondf(d2$Date[1], d2$Date[3]) == 2 # TRUE


### THESE VARIABLES MEASURE MONTHS IN WHICH LAWS WERE ENACTED

## Restaurants

d2$Restaurants_Policy <- 0
sts_rest <- unique(rest$State)
rest$Date <- as.Date(trunc.POSIXt(rest$Date, units = c("months")), tz = "", usetz = FALSE)
for(i in 1:length(sts_rest)){
	dts <- unique(rest$Date[rest$State == sts_rest[i]])
	dts <- as.Date(trunc.POSIXt(dts, units = c("months")), tz = "", usetz = FALSE)
	for(j in 1:length(dts)){
		p <- rest$Policy[rest$State == sts_rest[i] & rest$Date == dts[j]] # This codes the policy (1-2-3)
		if(length(p) == 1){
			d2$Restaurants_Policy[d2$State == sts_rest[i] & d2$Date == dts[j]] <- p 
		}
		else if(length(p) > 1){
			d2$Restaurants_Policy[d2$State == sts_rest[i] & d2$Date == dts[j]] <- max(p) 
		}
	}
}
# This dichotomizes the policy
d2$Restaurants_Enacted <- NA
d2$Restaurants_Enacted[d2$Restaurants_Policy > 0] <- 1
d2$Restaurants_Enacted[d2$Restaurants_Policy == 0] <- 0
summary(d2$Restaurants_Enacted)

# After enactment

d2$Restaurants_After_Enactment <- 0
for(i in 1:length(sts_rest)){

	date <- d2$Date[d2$Restaurants_Enacted == 1 & d2$State == sts_rest[i]]

	if(length(date) > 0){
		first_enactment <- min(date)
		d2$Restaurants_After_Enactment[d2$Date >= first_enactment & d2$State == sts_rest[i]] <- 1
	}
}

stopifnot(length(d2$Restaurants_After_Enactment[is.na(d2$Restaurants_After_Enactment) == TRUE]) == 0)


## Bars

d2$Bars_Policy <- 0
sts_bars <- unique(bars$State)
bars$Date <- as.Date(trunc.POSIXt(bars$Date, units = c("months")), tz = "", usetz = FALSE)
for(i in 1:length(sts_bars)){
	dts <- unique(bars$Date[bars$State == sts_bars[i]])
	dts <- as.Date(trunc.POSIXt(dts, units = c("months")), tz = "", usetz = FALSE)
	for(j in 1:length(dts)){
		p <- bars$Policy[bars$State == sts_bars[i] & bars$Date == dts[j]] # This codes the policy (1-2-3)
		if(length(p) == 1){
			d2$Bars_Policy[d2$State == sts_bars[i] & d2$Date == dts[j]] <- p 
		}
		else if(length(p) > 1){
			d2$Bars_Policy[d2$State == sts_bars[i] & d2$Date == dts[j]] <- max(p) 
		}
	}
}
# This dichotomizes the policy
d2$Bars_Enacted <- NA
d2$Bars_Enacted[d2$Bars_Policy > 0] <- 1
d2$Bars_Enacted[d2$Bars_Policy == 0] <- 0
summary(d2$Bars_Enacted)

# After enactment

d2$Bars_After_Enactment <- 0

for(i in 1:length(sts_rest)){

	date <- d2$Date[d2$Bars_Enacted == 1 & d2$State == sts_rest[i]]

	if(length(date) > 0){
		first_enactment <- min(date)
		d2$Bars_After_Enactment[d2$Date >= first_enactment & d2$State == sts_rest[i]] <- 1
	}
}

stopifnot(length(d2$Bars_After_Enactment[is.na(d2$Bars_After_Enactment) == TRUE]) == 0)


## Government worksites

d2$Government_Worksites_Policy <- 0
sts_govwork <- unique(govwork$State)
govwork$Date <- as.Date(trunc.POSIXt(govwork$Date, units = c("months")), tz = "", usetz = FALSE)
for(i in 1:length(sts_govwork)){
	dts <- unique(govwork$Date[govwork$State == sts_govwork[i]])
	dts <- as.Date(trunc.POSIXt(dts, units = c("months")), tz = "", usetz = FALSE)
	for(j in 1:length(dts)){
		p <- govwork$Policy[govwork$State == sts_govwork[i] & govwork$Date == dts[j]] # This codes the policy (1-2-3)
		if(length(p) == 1){
			d2$Government_Worksites_Policy[d2$State == sts_govwork[i] & d2$Date == dts[j]] <- p 
		}
		else if(length(p) > 1){
			d2$Government_Worksites_Policy[d2$State == sts_govwork[i] & d2$Date == dts[j]] <- max(p) 
		}
	}
}
# This dichotomizes the policy
d2$Government_Worksites_Enacted <- NA
d2$Government_Worksites_Enacted[d2$Government_Worksites_Policy > 0] <- 1
d2$Government_Worksites_Enacted[d2$Government_Worksites_Policy == 0] <- 0
summary(d2$Government_Worksites_Enacted)

# After enactment

d2$Government_Worksites_After_Enactment <- 0

for(i in 1:length(sts_rest)){

	date <- d2$Date[d2$Government_Worksites_Enacted == 1 & d2$State == sts_rest[i]]

	if(length(date) > 0){

		first_enactment <- min(date)
		d2$Government_Worksites_After_Enactment[d2$Date >= first_enactment & d2$State == sts_rest[i]] <- 1

	}
}

stopifnot(length(d2$Government_Worksites_After_Enactment[is.na(d2$Government_Worksites_After_Enactment) == TRUE]) == 0)


## Private worksites

d2$Private_Worksites_Policy <- 0
sts_privwork <- unique(privwork$State)
privwork$Date <- as.Date(trunc.POSIXt(privwork$Date, units = c("months")), tz = "", usetz = FALSE)
for(i in 1:length(sts_privwork)){
	dts <- unique(privwork$Date[privwork$State == sts_privwork[i]])
	dts <- as.Date(trunc.POSIXt(dts, units = c("months")), tz = "", usetz = FALSE)
	for(j in 1:length(dts)){
		p <- privwork$Policy[privwork$State == sts_privwork[i] & privwork$Date == dts[j]] # This codes the policy (1-2-3)
		if(length(p) == 1){
			d2$Private_Worksites_Policy[d2$State == sts_privwork[i] & d2$Date == dts[j]] <- p 
		}
		else if(length(p) > 1){
			d2$Private_Worksites_Policy[d2$State == sts_privwork[i] & d2$Date == dts[j]] <- max(p) 
		}
	}
}
# This dichotomizes the policy
d2$Private_Worksites_Enacted <- NA
d2$Private_Worksites_Enacted[d2$Private_Worksites_Policy > 0] <- 1
d2$Private_Worksites_Enacted[d2$Private_Worksites_Policy == 0] <- 0
summary(d2$Private_Worksites_Enacted)

# After enactment

d2$Private_Worksites_After_Enactment <- 0

for(i in 1:length(sts_rest)){

	date <- d2$Date[d2$Private_Worksites_Enacted == 1 & d2$State == sts_rest[i]]

	if(length(date) > 0){

		first_enactment <- min(date)
		d2$Private_Worksites_After_Enactment[d2$Date >= first_enactment & d2$State == sts_rest[i]] <- 1
	
	}
}

stopifnot(length(d2$Private_Worksites_After_Enactment[is.na(d2$Private_Worksites_After_Enactment) == TRUE]) == 0)


## Hotels

d2$Hotels_Policy <- 0
sts_hotels <- unique(hotels$State)
hotels$Date <- as.Date(trunc.POSIXt(hotels$Date, units = c("months")), tz = "", usetz = FALSE)
for(i in 1:length(sts_hotels)){
	dts <- unique(hotels$Date[hotels$State == sts_hotels[i]])
	dts <- as.Date(trunc.POSIXt(dts, units = c("months")), tz = "", usetz = FALSE)
	for(j in 1:length(dts)){
		p <- hotels$Policy[hotels$State == sts_hotels[i] & hotels$Date == dts[j]] # This codes the policy (1-2-3)
		if(length(p) == 1){
			d2$Hotels_Policy[d2$State == sts_hotels[i] & d2$Date == dts[j]] <- p 
		}
		else if(length(p) > 1){
			d2$Hotels_Policy[d2$State == sts_hotels[i] & d2$Date == dts[j]] <- max(p) 
		}
	}
}
# This dichotomizes the policy
d2$Hotels_Enacted <- NA
d2$Hotels_Enacted[d2$Hotels_Policy > 0] <- 1
d2$Hotels_Enacted[d2$Hotels_Policy == 0] <- 0
summary(d2$Hotels_Enacted)

# After enactment

d2$Hotels_After_Enactment <- 0

for(i in 1:length(sts_rest)){

	date <- d2$Date[d2$Hotels_Enacted == 1 & d2$State == sts_rest[i]]

	if(length(date) > 0){

		first_enactment <- min(date)
		d2$Hotels_After_Enactment[d2$Date >= first_enactment & d2$State == sts_rest[i]] <- 1

	}
}

stopifnot(length(d2$Hotels_After_Enactment[is.na(d2$Hotels_After_Enactment) == TRUE]) == 0)


## Indoor arenas

d2$Indoor_Arenas_Policy <- 0
sts_arenas <- unique(arenas$State)
arenas$Date <- as.Date(trunc.POSIXt(arenas$Date, units = c("months")), tz = "", usetz = FALSE)
for(i in 1:length(sts_arenas)){
	dts <- unique(arenas$Date[arenas$State == sts_arenas[i]])
	dts <- as.Date(trunc.POSIXt(dts, units = c("months")), tz = "", usetz = FALSE)
	for(j in 1:length(dts)){
		p <- arenas$Policy[arenas$State == sts_arenas[i] & arenas$Date == dts[j]] # This codes the policy (1-2-3)
		if(length(p) == 1){
			d2$Indoor_Arenas_Policy[d2$State == sts_arenas[i] & d2$Date == dts[j]] <- p 
		}
		else if(length(p) > 1){
			d2$Indoor_Arenas_Policy[d2$State == sts_arenas[i] & d2$Date == dts[j]] <- max(p) 
		}
	}
}
# This dichotomizes the policy
d2$Indoor_Arenas_Enacted <- NA
d2$Indoor_Arenas_Enacted[d2$Indoor_Arenas_Policy > 0] <- 1
d2$Indoor_Arenas_Enacted[d2$Indoor_Arenas_Policy == 0] <- 0
summary(d2$Indoor_Arenas_Enacted)

# After enactment

d2$Indoor_Arenas_After_Enactment <- 0

for(i in 1:length(sts_rest)){

	date <- d2$Date[d2$Indoor_Arenas_Enacted == 1 & d2$State == sts_rest[i]]

	if(length(date) > 0){

		first_enactment <- min(date)
		d2$Indoor_Arenas_After_Enactment[d2$Date >= first_enactment & d2$State == sts_rest[i]] <- 1

	}
}

stopifnot(length(d2$Indoor_Arenas_After_Enactment[is.na(d2$Indoor_Arenas_After_Enactment) == TRUE]) == 0)


## Malls

d2$Malls_Policy <- 0
sts_malls <- unique(malls$State)
malls$Date <- as.Date(trunc.POSIXt(malls$Date, units = c("months")), tz = "", usetz = FALSE)
for(i in 1:length(sts_malls)){
	dts <- unique(malls$Date[malls$State == sts_malls[i]])
	dts <- as.Date(trunc.POSIXt(dts, units = c("months")), tz = "", usetz = FALSE)
	for(j in 1:length(dts)){
		p <- malls$Policy[malls$State == sts_malls[i] & malls$Date == dts[j]] # This codes the policy (1-2-3)
		if(length(p) == 1){
			d2$Malls_Policy[d2$State == sts_malls[i] & d2$Date == dts[j]] <- p 
		}
		else if(length(p) > 1){
			d2$Malls_Policy[d2$State == sts_malls[i] & d2$Date == dts[j]] <- max(p) 
		}
	}
}
# This dichotomizes the policy
d2$Malls_Enacted <- NA
d2$Malls_Enacted[d2$Malls_Policy > 0] <- 1
d2$Malls_Enacted[d2$Malls_Policy == 0] <- 0
summary(d2$Malls_Enacted)

# After enactment

d2$Malls_After_Enactment <- 0
for(i in 1:length(sts_rest)){

	date <- d2$Date[d2$Malls_Enacted == 1 & d2$State == sts_rest[i]]

	if(length(date) > 0){

		first_enactment <- min(date)
		d2$Malls_After_Enactment[d2$Date >= first_enactment & d2$State == sts_rest[i]] <- 1

	}
}

stopifnot(length(d2$Malls_After_Enactment[is.na(d2$Malls_After_Enactment) == TRUE]) == 0)


# AGGREGATE POLICIES

## Month of enactment: 1 during month in which any of the policies were enacted

d2$Policy_Enacted_This_Month <- NA
d2$Policy_Enacted_This_Month <- d2$Restaurants_Enacted + d2$Bars_Enacted + d2$Government_Worksites_Enacted + d2$Private_Worksites_Enacted + d2$Hotels_Enacted + d2$Indoor_Arenas_Enacted + d2$Malls_Enacted
d2$Policy_Enacted_This_Month[d2$Policy_Enacted_This_Month > 0] <- 1


# NUMBER OF MONTHS BEFORE / AFTER ENACTMENT

## Max number of months, for those states that never adopt
d2$Policy_Months_Before_After_Enacted <- NA
d2$Policy_Months_Before_After_Enacted[d2$Policy_Enacted == 1] <- 0

e <- c()
for(i in 1:length(sts)){
	e[i] <- length(sort(unique(d2$Date[d2$Policy_Enacted_This_Month == 1 & d2$State == sts[i]]))) # Get number of dates of enactment
}
e

for(i in 1:length(sts)){

	u <- sort(unique(d2$Date[d2$Policy_Enacted_This_Month == 1 & d2$State == sts[i]])) # Get dates of enactment

	if(length(u) == 0){ # If no enactment
		d2$Policy_Months_Before_After_Enacted[d2$State == sts[i]] <- -mondf(min(d2$Date), max(d2$Date))
	}

	else if(length(u) == 1){ # If only one enactment
		t <- mondf(date_min, u) # difference between enactment date and minimum date
		d2$Policy_Months_Before_After_Enacted[d2$Date >= date_min & d2$Date < u & d2$State == sts[i]] <- -rev(seq(1,t)) # negative counter for dates before enactment

		t <- mondf(u, date_max)
		d2$Policy_Months_Before_After_Enacted[d2$Date > u & d2$Date <= date_max & d2$State == sts[i]] <- seq(1,t) # positive counter for dates before enactment
	}

	else if(length(u) > 1){ # If more than one enactment
		midpoints <- c()
		for(j in 1:(length(u)-1)){ # Compute midpoints between enactments
			midpoints[j] <- u[j] + floor((u[j + 1] - u[j]) / 2)
		}
		
		midpoints <- sort(c(date_min, midpoints, date_max))

		for(j in 1:(length(u))){

			t <- mondf(midpoints[j], u[j]) # compute difference between midpoints and enactment dates
			s <- seq(1,t)

			l_before <- length(d2$Policy_Months_Before_After_Enacted[d2$Date >= midpoints[j] & d2$Date < u[j] & d2$State == sts[i]])
			counter_before <- -rev(s[1:l_before])

			d2$Policy_Months_Before_After_Enacted[d2$Date >= midpoints[j] & d2$Date < u[j] & d2$State == sts[i]] <- counter_before

			t <- mondf(u[j], midpoints[j+1])
			s <- seq(1,t)

			l_after <- length(d2$Policy_Months_Before_After_Enacted[d2$Date > u[j] & d2$Date <= midpoints[j+1] & d2$State == sts[i]])
			counter_after <- s[1:l_after]

			d2$Policy_Months_Before_After_Enacted[d2$Date > u[j] & d2$Date <= midpoints[j+1] & d2$State == sts[i]] <- counter_after

		}
	}
}

summary(d2$Policy_Months_Before_After_Enacted)

# SHARE OF POLICIES PRESENT IN A STATE

d2$Policy_Share <- (d2$Restaurants_After_Enactment + d2$Bars_After_Enactment + d2$Government_Worksites_After_Enactment + d2$Private_Worksites_After_Enactment + d2$Indoor_Arenas_After_Enactment + d2$Hotels_After_Enactment + d2$Malls_After_Enactment) / 7


#### SPATIAL LAGS ####################################

## Spatial lags (neighbors + all + diffusion network)

d4 <-  d2[order(d2$Date),]
d4$Policy_Spatial_Lag_Neighbors <- NA
d4$Policy_Spatial_Lag_All <- NA
d4$Policy_Spatial_Lag_Network_Pcent <- NA
unique_dates <- unique(d4$Date)

d4 <-  subset(d4, State != "DC") # Remove DC, not in Desmarais et al.
dts2 <- as.numeric(format(unique_dates,"%Y"))


# Neighbors

Ws2 <-  Ws
Ws2 <- Ws2[-8, -8]

for(i in 1:length(unique_dates)){
	sl <- Ws2 %*% d4$Policy_Share[d4$Date==unique_dates[i]] 
	d4$Policy_Spatial_Lag_Neighbors[d4$Date==unique_dates[i]] <- sl
}

# All other states

Ws2_all <-  Ws_all
Ws2_all <- Ws2_all[-8, -8]

for(i in 1:length(unique_dates)){
	sl <- Ws2_all %*% d4$Policy_Share[d4$Date==unique_dates[i]] 
	d4$Policy_Spatial_Lag_All[d4$Date==unique_dates[i]] <- sl
}


# Network

for(i in 1:length(yrs)){

	dts3 <- unique_dates[dts2 == yrs[i]]

	Wd <-  as.matrix(read.csv(paste("./covariates/raw-data/W_", yrs[i], ".csv", sep="")))
	Wds <-  Wd
	# Row-standardize
	for(r in 1:nrow(Wd)){
		if(sum(Wd[r,]) > 0){
			Wds[r,] <- Wd[r,]/sum(Wd[r,])
		}
	}

	for(i in 1:length(dts3)){

		sl <- Wds %*% d4$Policy_Share[d4$Date==dts3[i]] 
		d4$Policy_Spatial_Lag_Network_Pcent[d4$Date==dts3[i]] <- sl

		sl2 <- Wd %*% d4$Policy_Share[d4$Date==dts3[i]]
		d4$Policy_Spatial_Lag_Network_Count[d4$Date==dts3[i]] <- sl2

	}

}

d4 <- d4[order(d4$State),]

write.csv(d4, "./covariates/raw-data/smokefree-policies-with-spatial-lags.csv", row.names = FALSE)


