
setwd("~/Dropbox/Home/Projects/TexasRadiosondeData")

# First 4 lines of file
ACM00078861  17.1170  -61.7830   10.0    COOLIDGE FIELD (UA)            1947 1993  13896
AEM00041217  24.4333   54.6500   16.0    ABU DHABI INTERNATIONAL AIRPOR 1983 2021  37655
AEXUAE05467  25.2500   55.3700    4.0    SHARJAH                        1935 1942   2477
AFM00040911  36.7000   67.2000  378.0    MAZAR-I-SHARIF                 2010 2014   2179

locs<-c(0, 12, 21,31,38,72,77,82,89) 
widths<- diff( locs)
stationInfo<- read.fwf("igra2-station-list.txt",widths =widths,
                       stringsAsFactors = FALSE)

stats( stationInfo  )
countryID<- substr(stationInfo[,1], 1,2)
indUS<-  which( countryID =="US")
infoUS<- stationInfo[indUS,]
plot( infoUS[,3], infoUS[,2],cex=.5)
world(  col="green", add=TRUE)
US()
points( infoUS[,3], infoUS[,2],cex=.5)

TXRegion<- cbind( 
  c(  -89.51488,-101.12853),
   c( 25.47732,  31.90862)
)
indTX<-  
         (-89  > infoUS[,3]) &  (-102 < infoUS[,3]) &
         ( 25  <  infoUS[,2]) &  (  35   >  infoUS[,2])
               
indYR<- (infoUS[,7] > 2020) & indTX

infoUS[ indYR,]

infoUS[ indYR,1]

look<- scan("USM00072249-data.txt", what="a", sep="\r")
indRecord0<- which(substr(look,1,1) =="#")
# get the dates

tmp<- look[ indRecord0 ]
#USM00072249 1970 12 31 12
firstLine<- read.table(   text = substr( tmp,1,27),
                            stringsAsFactors = FALSE,
                                  comment="")
ind<- firstLine[,5] ==99 # some hours are missing
firstLine[ind,5]<- 0

dateTime<- paste0( firstLine[,2],"/",
                   firstLine[,3],"/",
                   firstLine[,4], " ",
                   firstLine[,5] )

dateTime<- ymd_h( dateTime)

ind<- which( (dateTime >= ymd("2017/01/01")) &
                (dateTime <= ymd("2018/01/01")))
  
indRecord<- indRecord0[ind]
i1<- min(ind)
i2<- max(ind)  
a1<- indRecord0[i1]
a2<- indRecord0[i2] # this is first line of next sonde so subtract 1
a2<- a2 -1 
# check  should be next sonde
look[a2+1]
date0<- dateTime[ind]

# the right subset of raw data 
rawData0<- look[a1:a2 ]



#Standard pressure level (for levels at 1000, 925, 850,
#                         700, 500, 400, 300, 250, 200, 150, 100, 70, 50, 30, 
#                        20, 10, 7, 5, 3, 2, and 1 hPa
#    

indexRD<- which(substr(rawData0,1,1) =="#")


nRecords<- length( indexRD)
library( lubridate)

RD<- NULL
for( k in 1 : nRecords ){
  cat( k, " ")
   firstLine<- read.table(   text = rawData0[ indexRD[k] ],
                           stringsAsFactors = FALSE,
                           comment="")
   dateTime<- paste0( firstLine[2],"/",
                     firstLine[3],"/",
                     firstLine[4], " ",
                     firstLine[5] )
   dateTime<- ymd_h( dateTime)
   if( dateTime != date0[k]){
     stop("Houston we have a problem")
   }
  if( k < nRecords){
  iRec<- (indexRD[k] + 1) : (indexRD[k+1] - 1)
  }
  else{
    iRec<- (indexRD[k] + 1) : length(rawData0)
  }
  
  rawSonde<- rawData0[iRec]
  rawSonde<- gsub("-9999","  NA" , rawSonde )
  rawSonde<- gsub("-8888","  NA" , rawSonde )
  
  work<- read.table( text = rawSonde,
                      stringsAsFactors = FALSE)
  
  # grab only mandatory levels
  ind<- which( work[,1] <20)
  work<- work[ind,]
  work<- work[,3:9]
  colnames(work)<- c("pressure","height","temp","RH",
                     "DPDP","windDir", "windSpeed" )
  # fix up columns
  work$pressure<- as.numeric(work$pressure) / 10
  # fix up column 5 (omit letter)
  if( is.character(work$height)){
    work$height <-  as.numeric(gsub("B", "", work$height ))
  }
  if( is.character(work$temp)){
    work$temp <- as.numeric( gsub("B","", work$temp ))/10
  }
  work$windSpeed<- work$windSpeed / 10
  work<- data.frame( dateTime= rep( dateTime, nrow(work)), 
                     work)
  RD<- rbind( RD, work)
 
}

cat( fill=TRUE)
FortWorth<- RD
ind<- FortWorth$pressure == 9250

plot( (FortWorth$dateTime[ind]), FortWorth$windSpeed[ind],
      type="l")
ind<- FortWorth$pressure == 500

lines(FortWorth$dateTime[ind], FortWorth$windSpeed[ind], col="orange" )


