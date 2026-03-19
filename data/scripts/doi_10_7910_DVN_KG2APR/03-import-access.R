library(data.table)
library(reshape)


#######################################################################
##### Import Internet Access Data ######
#######################################################################
## survey data at the district level.
## there is a separate col for each access type and each survey
acc <- read.csv(file="access.csv", sep=",", stringsAsFactors=FALSE)

gov <- grep("ADMIN1_EN|District", colnames(acc), value=TRUE)
internet <- grep("Mobile|3G|2G", colnames(acc), value=TRUE)

## Data in wide format - find all cols that refer to 2G, 3G and Mobile Phone access
int.name <- c("_X3G", "_X2G.GPRS", "_Mobile.Phones")
int.list <- NULL
## reshape data so we have one col for each access type
for (i in int.name) {
   int <- grep(i, colnames(acc), value="TRUE")
   int <- melt(acc[,c("District", "ADMIN1_EN", int)], id=c("District", "ADMIN1_EN"))
   int$variable <- gsub(i, "", int$variable)
   names(int) <- c("District", "gov","time", i)
   if (is.null(int.list)) {
      int.list <- int
   } else {
      int.list <- merge(int.list, int, by=c("District", "gov", "time"), all=TRUE)
   }
}
colnames(int.list) <- gsub("_", "", colnames(int.list))
int.list$time <- as.numeric(gsub("X", "", int.list$time))
int.list <- int.list[order(int.list$time),]
int.list <- int.list[order(int.list$District),]

## survey-date2w.Rdata maps each survey to the days it covers -> then data is aggregated to bi-weekly level
load("survey-date2w.Rdata")

## create a dataset at the day-governorate level that gives us Internet access at the daily level for each governorate, based on the survey data we have.
day.gov.access <- data.table(merge(int.list, survey.times, by.x="time", by.y="survey.period", all=T))


#######################################################################
##### Aggregate to two-week level for each governorate ######
#######################################################################
## aggregate to two week level, using the mean for each 2 week period.
## access by 2 week period is therefore weighted by daily access levels 
access.date2w <- day.gov.access[!is.na(District), list(X3G=mean(X3G, na.rm=T),
                                       X2G.GPRS=mean(X2G.GPRS, na.rm=T),
                                       Mobile.Phones=mean(Mobile.Phones, na.rm=T)),
                                by=list(gov=gov, date2w=date2w)]


## rename governorates so that they have the same names as in viol data
access.date2w[, gov := as.character(gov)]
access.date2w[, gov := ifelse(gov=="Al-Hasakeh", "Al-Hasaka", gov)]
access.date2w[, gov := ifelse(gov=="Ar-Raqqa", "Ar-Raqqah", gov)]
access.date2w[, gov := ifelse(gov=="As-Sweida", "As-Suwayda", gov)]
access.date2w[, gov := ifelse(gov=="Dar'a", "Daraa", gov)]
access.date2w[, gov := ifelse(gov=="Idleb", "Idlib", gov)]
access.date2w[, gov := ifelse(gov=="Lattakia", "Latakia", gov)]
access.date2w[, gov := ifelse(gov=="Tartous", "Tartus", gov)]
access.date2w[, gov := ifelse(gov=="Deir-ez-Zor", "Deir ez-Zor", gov)]



#######################################################################
##### Dealing with missing values  ######
#######################################################################
### interpolating missing access values
access.date2w <- access.date2w[order(date2w),]
access.date2w <- access.date2w[order(gov),]

## find missing values
NAs <- which(is.na(access.date2w$X3G))

##missing values
access.date2w[NAs,]

## value at t-1
NAs.pre <- NAs-1
# value at t+1
NAs.post <- NAs+1

## interpolate for 3G, 2G, and Mobile Phones
access.date2w[NAs, Mobile.Phones := rowMeans(cbind(access.date2w[NAs.pre, Mobile.Phones],
                            access.date2w[NAs.post, Mobile.Phones]), na.rm=T)]
access.date2w[NAs, X3G := rowMeans(cbind(access.date2w[NAs.pre, X3G], access.date2w[NAs.post, X3G]), na.rm=T)]
access.date2w[NAs, X2G.GPRS := rowMeans(cbind(access.date2w[NAs.pre, X2G.GPRS], access.date2w[NAs.post, X2G.GPRS]),
                                        na.rm=T)]



write.csv(access.date2w, file="date2w-access.csv", row.names=FALSE)




