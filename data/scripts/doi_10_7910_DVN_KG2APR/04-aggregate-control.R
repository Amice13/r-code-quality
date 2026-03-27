library(data.table)
library(zoo)
library(tidyr)
library(dplyr)

#######################################################################
##### Import Control data  ######
#######################################################################
## Data from the Carter Center is geolocated
data <- read.csv("terr_control.csv", sep=",", stringsAsFactors=FALSE)

str(data)
table(data$group)
table(data$date)

## clean up lat and long
data$LatLongMGRSGridLongitude <- gsub("\\°", "", data$LatLongMGRSGridLongitude)
data$LatLongMGRSGridLongitude <- gsub("\\|.*","", data$LatLongMGRSGridLongitude)
data$LatLongMGRSGridLongitude <- trimws(data$LatLongMGRSGridLongitude)

data$LatLongMGRSGridLatitude <- gsub("\\°", "", data$LatLongMGRSGridLatitude)
data$LatLongMGRSGridLatitude <- gsub("\\|.*","", data$LatLongMGRSGridLatitude)
data$LatLongMGRSGridLatitude <- trimws(data$LatLongMGRSGridLatitude)


## clean up governorate names
data$governorate[grep("Al-Hasakah", data$governorate)] <- "Al-Hasaka"
data$governorate[grep("Aleppo", data$governorate)] <- "Aleppo"
data$governorate[grep("Ar-Raqqa", data$governorate)] <- "Ar-Raqqah"
data$governorate[grep("As-Suweida", data$governorate)] <- "As-Suwayda"
data$governorate[grep("^Damascus", data$governorate)] <- "Damascus"
data$governorate[grep("Rural Damascus", data$governorate)] <- "Rural Damascus"
data$governorate[grep("Daraa", data$governorate)] <- "Daraa"
data$governorate[grep("Deir Ezzor", data$governorate)] <- "Deir ez-Zor"
data$governorate[grep("Hama", data$governorate)] <- "Hama"
data$governorate[grep("Homs", data$governorate)] <- "Homs"
data$governorate[grep("Idleb", data$governorate)] <- "Idlib"
data$governorate[grep("Latakia", data$governorate)] <- "Latakia"
data$governorate[grep("Quneitra", data$governorate)] <- "Quneitra"
data$governorate[grep("Tartous", data$governorate)] <- "Tartus"

table(data$governorate, useNA="always")


## unique lat + long combination

data$latlong <- paste(data$LatLongMGRSGridLongitude, data$LatLongMGRSGridLatitude, sep=":") 
length(unique(data$latlong))

## make sure there are no geo-location/date duplicates 
data <- data[!duplicated(data[c("latlong","date")]),]

## map latlong to governorates
geo.gov <- data[, c("latlong", "governorate")]
geo.gov <- geo.gov[!duplicated(geo.gov),]
data$date <- as.Date(data$date)
data$governorate <- data$LatLongMGRSGridLongitude <- data$LatLongMGRSGridLatitude <- NULL

## dataframe that covers full study period - create geoloc observation for every time point 
load("survey-date2w.Rdata")
survey.times <- unique(as.Date(survey.times$date2w))
control.geoloc <- expand.grid(latlong = unique(data$latlong), date=survey.times)

## add governorate names for each geolocation
control.geoloc <- merge(control.geoloc, geo.gov, by="latlong", all.x=TRUE)

## merge with control data 
control.geoloc <- merge(control.geoloc, data, by=c("date", "latlong"), all.x=TRUE)

## sort by date and location
control.geoloc <- control.geoloc[order(control.geoloc$date),]
control.geoloc <- control.geoloc[order(control.geoloc$latlong),]

## fill in missing values with previous control group (If Carter Center logs no change the value would be missing)
control.geoloc <- group_by(control.geoloc,latlong) %>% tidyr::fill(group, .direction=c("down")) %>% as.data.frame()
## for remaining missing vlalues fill with first control group we know of 
control.geoloc <- group_by(control.geoloc,latlong) %>% tidyr::fill(group, .direction=c("up")) %>% as.data.frame()

control.geoloc <- data.table(control.geoloc)
control.geoloc[, cnt := 1]
agg <- control.geoloc[, list(sum=sum(cnt)), by=list(governorate, group, date2w=as.character(date))]
agg <- agg[order(date2w),]
agg <- agg[order(group),]
agg <- agg[order(governorate),]

agg[, sum := ifelse(is.na(sum), 0, sum)]
## reshape to wide format
control <- reshape(agg, idvar=c("date2w", "governorate"), timevar="group", direction="wide")

## replace NA with 0
control <- control[,lapply(.SD, function(x) { ifelse(is.na(x), 0, x) } )]

## number of communities by gov and date
control[, total := rowSums(.SD, na.rm = TRUE), .SDcols = grep("sum", names(control))] 



### convert number of communities to percentage
control <-  control[, list(governorate, date2w,
                           perc.Government= 100*round(sum.Government/total, digits=2),
                           `perc.Islamic State`= 100*round(`sum.Islamic State`/total, digits=2),
                           perc.Kurds= 100*round(sum.Kurds/total, digits=2),
                           perc.Opposition= 100*round(sum.Opposition/total, digits=2),
                           total)]

#######################################################################
##### Compute control parameters  ######
#######################################################################

### for each conflict side: change (win/loose/constant)
control <- control[order(governorate, date2w),]
setkey(control, governorate)

control[, Government.diff:=c(NA,diff(perc.Government)),by=governorate]
control[, IS.diff:=c(NA,diff(`perc.Islamic State`)),by=governorate]
control[, Opposition.diff:=c(NA,diff(perc.Opposition)),by=governorate]
control[, Kurds.diff:=c(NA,diff(perc.Kurds)),by=governorate]

## recode
control <- as.data.frame(control)
control[, grep("diff", names(control))] <- apply(control[, grep("diff", names(control))], 2, 
                                       function(x) ifelse(x<0, -1, x))

control[, grep("diff", names(control))] <- apply(control[, grep("diff", names(control))], 2, 
                                       function(x) ifelse(x>0, 1, x))

control[, grep("diff", names(control))] <- apply(control[, grep("diff", names(control))], 2, 
                                       function(x) ifelse(x==0, "constant", x))

control[, grep("diff", names(control))] <- apply(control[, grep("diff", names(control))], 2, 
                                       function(x) ifelse(x==1, "gain", x))

control[, grep("diff", names(control))] <- apply(control[, grep("diff", names(control))], 2, 
                                       function(x) ifelse(x==-1, "loss", x))


## some groups are not active in certain regions.

control$Kurds.diff <- ifelse(control$governorate!="Al-Hasaka" &
                          control$governorate!="Aleppo" &
                          control$governorate!="Ar-Raqqah" &
                          control$governorate!="Idlib", "not present", control$Kurds.diff)

control$IS.diff <- ifelse(control$governorate=="Latakia" |
                       control$governorate=="Quneitra" |
                       control$governorate=="Rural Damascus"|
                       control$governorate=="Tartus", "not present", control$IS.diff)
## rename percentage controlled by IS 
control$perc.IS <- control[["perc.Islamic State"]]

### who has majority of communities under control?
control$control <- apply(control[, grep("perc", colnames(control))], 1, function(x)  names(which.max(x)))

control$control <- gsub("perc.", "", control$control)
## and what percentage?
control$perc.control <- apply(control[, grep("perc", colnames(control))], 1, function(x) max(x))
### if control is below 60% control is contested
control$control <- ifelse(control$perc.control<61, "contested", control$control)
### alternative measure: if control is below 70% control is contested
control$control70 <- ifelse(control$perc.control<71, "contested", control$control)

control$gov <- control$governorate
control$governorate <- NULL
write.csv(control, file="date2w-control.csv", row.names=FALSE)



