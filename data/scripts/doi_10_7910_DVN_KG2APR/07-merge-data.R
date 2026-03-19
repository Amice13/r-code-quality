library(data.table)

## violence estimates
load("estimates-dga-date2w.Rdata")   
data$date2w <- as.character(data$date2w)
data$gov <- as.character(data$gov)

## control
control.date2w <- read.csv("date2w-control.csv", stringsAsFactors=FALSE, sep=",")
control.date2w$gov <- gsub(" ", ".", control.date2w$gov)

## internet access
access.date2w <- read.csv("date2w-access.csv", stringsAsFactors=FALSE, sep=",")
access.date2w$gov <- gsub(" ", ".", access.date2w$gov)

data <- Reduce(function(x, y) merge(x, y, all=T, 
    by=c("gov", "date2w")), list(data, access.date2w, control.date2w), accumulate=F)


### replace NAs with 0 in the death counts
counts <- grep("untarg|targ|sum", names(data))
data[counts] <- apply(data[counts], 2, function(x) ifelse(is.na(x), 0, x))


### EPR
epr <- read.csv(file="EPR-Syria-bygov.csv", stringsAsFactors=FALSE)

### 2011 population data
pop <- read.csv(file="syr_pop_2011.csv", stringsAsFactors=FALSE)
pop <- pop[, c("gov", "pop11")]
pop$pop11.log <- log(pop$pop11)

### 2011 unemployment data
unemp <- read.csv(file="SY_unemployment.csv", stringsAsFactors=FALSE)
unemp <- unemp[, c("gov", "unemp_perc_2011")]

demo.stats = Reduce(function(...) merge(..., by=c("gov"), all=TRUE), list(unemp, pop, epr))

data <- merge(data, demo.stats, by="gov", all=T)


### refactor control
data$control <- factor(data$control, levels=c("contested", "Government", "Islamic State",
                                            "Kurds", "Opposition"))
data$control70 <- factor(data$control70, levels=c("contested", "Government", "Islamic State",
                                            "Kurds", "Opposition"))

## invert internet scale (so that low numbers are low access, high numbers high access)
data$Mobile.Phones <- 5-data$Mobile.Phones
data$X3G <- 5-data$X3G
data$X2G.GPRS <- 5-data$X2G.GPRS


## create a binary variable: when internet is full 1, otherwise 0
data <- data[order(data$date2w),]
data <- data[order(data$gov),]

binary.internet <- function(data, varname){
   tmp <- data.table(data[, c("gov","date2w", varname)])
   tmp$Binary <- round(tmp[[varname]])
   tmp$Binary <- ifelse(tmp$Binary>1,1,0)
   ## lagged (access at t-1)
   tmp[, l.Binary := shift(.SD, type='lag'), by = gov, .SDcols="Binary"]
   ## lagged internet access
   tmp[, l.varname := shift(.SD, type='lag'), by = gov, .SDcols=`varname`]
   
   tmp <-  as.data.frame(tmp[, list(Binary, l.Binary, l.varname)])
   
   return(tmp)
}

data[, c("X3G.d", "l.X3G.d", 'l.X3G')]  <-
   binary.internet(data, "X3G")

data[, c("X2G.d", "l.X2G.d", "l.X2G")] <-
   binary.internet(data, "X2G.GPRS")

data[, c("Mobile.Phones.d", "l.Mobile.Phones.d", "l.Mobile.Phones")] <-
   binary.internet(data, "Mobile.Phones")


#### violence measures used
## targeted violence
data$targeted <- data$nhat.targ
## overall violence (targeted + untargeted)
data$total <- data$nhat.targ+data$nhat.untarg
## log overall violence (.5 added to deal with zeros)
data$total.log <- log(data$total + 0.5)

save(data, file="analysis-data.Rdata")

