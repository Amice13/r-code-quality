
## This file makes shows how the main data file (data_combined.rda) was assembled.

rm(list=ls(all=TRUE))

library(foreign)
library(xlsx)
library(DataCombine)


set.seed(123)

# load the data with QPR scores for all party-party dyads for all surveys
load("data/coopdata/coopdata_pol-pol.rda")


# load the data from Fortunato et al (2018) that contain parties' left-right CMP scores, voters' average perceived left-right positions, and voters' perceived left-right positions according to their SOPHIA measure
sophia <- read.dta("data/data_aux/sophia/placeGov.dta")

sophia$month <- as.character(sophia$month)
sophia$month <- ifelse(nchar(sophia$month)==1, paste0("0", sophia$month), sophia$month)

# keep only 2002 to 2014 
sophia <- sophia[sophia$year>=2002 & sophia$year<=2014,]

# keep only countries where there is QPR data
countries <- c("austria", "belgium", "denmark", "finland", "france", "germany", "greece", "ireland", "italy", "netherlands", "portugal", "spain", "uk")
sophia <- sophia[sophia$country %in% countries,]

# create additional date variables
sophia$year.month <- paste0(sophia$year, "_", sophia$month)
sophia$year.month.country <- paste0(sophia$year, "_", sophia$month, "_", sophia$country)


# keep only relevant variables
sophia <- subset(sophia, select=c("year.month.country", "country", "survey", "cmp", "survey", "average", "averageSd", "averageSe", "deviation", paste0("draw", 1:1000), "cmpPoint", "cmpSe", "cmpDate", "cmpPointLag", "cmpSeLag", "cmpDateLag", "coalition", "seatShare", "cabinet", "pm", "mean", "sd"))


# merge (using CMP identifier, for party 1, then for party 2 in the dyad)
colnames(sophia) <- c("year.month.country", "country", "survey", "cmpcode.1", "survey.1", "lr.avg.1", "lr.avg.sd.1", "lr.avg.se.1", "lr.dev.1", paste0("lr.sophia.1.", 1:1000), "lr.cmp.1", "lr.cmp.se.1", "cmp.date.1", "lr.cmp.lag.1", "lr.cmp.lag.se.1", "cmp.date.lag.1", "coalition.1", "seatshare.1", "cabinet.1", "pm.1", "lr.sophia.mean.1", "lr.sophia.sd.1")
data <- merge(data, sophia, by=c("year.month.country", "cmpcode.1"), all.x=T)


colnames(sophia) <- c("year.month.country", "country", "survey", "cmpcode.2", "survey.2", "lr.avg.2", "lr.avg.sd.2", "lr.avg.se.2", "lr.dev.2", paste0("lr.sophia.2.", 1:1000), "lr.cmp.2", "lr.cmp.se.2", "cmp.date.2", "lr.cmp.lag.2", "lr.cmp.lag.se.2", "cmp.date.lag.2", "coalition.2", "seatshare.2", "cabinet.2", "pm.2", "lr.sophia.mean.2", "lr.sophia.sd.2")
data <- merge(data, sophia, by=c("year.month.country", "cmpcode.2"), all.x=T)


# delete duplicates and missing data
data <- data[data$duplicate==0,]
data <- data[is.na(data$lr.avg.1)==F & is.na(data$lr.avg.2)==F,]



# difference in average perceptions (mean)
data$lr.avg.diff.mean <- abs(data$lr.avg.1 - data$lr.avg.2)
# difference in CMP positions (mean)
data$lr.cmp.diff.mean <- abs(data$lr.cmp.1 - data$lr.cmp.2)
# difference in Sophia positions (mean)
data$lr.sophia.diff.mean <- abs(data$lr.sophia.mean.1 - data$lr.sophia.mean.2)


# create 1000 draws for CMP differences
d1 <- data.frame(data$lr.cmp.1, data$lr.cmp.se.1)
d1draw <- NULL
for(i in 1:dim(d1)[1]){
	add <- rnorm(1000, d1[i,1], d1[i,2])
	d1draw <- rbind(d1draw, add)
}

d2 <- data.frame(data$lr.cmp.2, data$lr.cmp.se.2)
d2draw <- NULL
for(i in 1:dim(d2)[1]){
	add <- rnorm(1000, d2[i,1], d2[i,2])
	d2draw <- rbind(d2draw, add)
}

d3 <- abs(d1draw-d2draw)
colnames(d3) <- paste0("lr.cmp.diff.", 1:1000)

data <- cbind(data, d3)


# create 1000 draws for perception differences (average)
d1 <- data.frame(data$lr.avg.1, data$lr.avg.sd.1)
d1draw <- NULL
for(i in 1:dim(d1)[1]){
	add <- rnorm(1000, d1[i,1], d1[i,2])
	d1draw <- rbind(d1draw, add)
}

d2 <- data.frame(data$lr.avg.2, data$lr.avg.sd.2)
d2draw <- NULL
for(i in 1:dim(d2)[1]){
	add <- rnorm(1000, d2[i,1], d2[i,2])
	d2draw <- rbind(d2draw, add)
}

d3 <- abs(d1draw-d2draw)
colnames(d3) <- paste0("lr.avg.diff.", 1:1000)

data <- cbind(data, d3)


# create differences in sophia positions (1000 draws)
d1 <- data[,paste0("lr.sophia.1.", 1:1000)]
d2 <- data[,paste0("lr.sophia.2.", 1:1000)]

d3 <- abs(d1-d2)
colnames(d3) <- paste0("lr.sophia.diff.", 1:1000)

data <- cbind(data, d3)


# create coalition and opposition variables
data$coalition <- ifelse(data$cabinet.1==1 & data$cabinet.2==1, 1, 0)
data$opposition <- ifelse(data$cabinet.1==0 & data$cabinet.2==0, 1, 0)


# merge in data on election years
elecdata <- read.csv("data/data_aux/elections/elections.csv")

elecdata$country <- as.character(tolower(elecdata$country))
elecdata$country <- ifelse(elecdata$country=="united kingdom", "uk", elecdata$country)

data$elec <- NA

for(i in 1:dim(data)[1]){
	usegovdata <- elecdata[elecdata$country==as.character(data$country[i]) & as.character(elecdata$year)==as.character(data$year[i]),]
	if(dim(usegovdata)[1]==0) { data$elec[i] <- 0}
	if(dim(usegovdata)[1]>0) { data$elec[i] <- 1}
}



# keep only variables needed for analysis
data <- subset(data, select=c("dyadid", "year.month.country", "country", "year", "month", "cmpcode.1", "id.1", "name.1", "type.1", "cmpcode.2", "id.2", "name.2", "type.2", "coalition", "opposition", "elec", "qpr.mean", "lr.avg.diff.mean", "lr.cmp.diff.mean",  paste0("qpr.", 1:1000), paste0("lr.cmp.diff.", 1:1000), paste0("lr.avg.diff.", 1:1000), paste0("lr.sophia.diff.", 1:1000)))


save(data, file="data/final/data_combined.rda")


