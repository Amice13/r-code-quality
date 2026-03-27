
setwd("~/Dropbox/Home/Projects/TexasRadiosondeData")
library( lubridate)
# USM00072240 Lake Charles 
look<- scan("USM00072251-data.txt", what="a", sep="\r")
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

StdP<- c(1000, 925, 850,
          700, 500, 400, 300,
          250, 200, 150, 100, 
          70, 50, 30, 20, 10, 7)
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
  work$pressure<- as.numeric(work$pressure) / 100
  # fix up columns ,omit letter B,  these are quality flags
  if( is.character(work$height)){
    work$height <-  as.numeric(gsub("B", "", work$height ))
  }
  if( is.character(work$temp)){
    work$temp <- as.numeric( gsub("B","", work$temp ))/10
  }
  work$windSpeed<- work$windSpeed / 10
  work$RH<- work$RH/10
  
  work$DPDP<- work$DPDP/10
  
  indP<- match( work$pressure,StdP )
  
  work2<- data.frame( matrix(NA, ncol=7, nrow= length(StdP) ))
  work2[,1]<- StdP
  work2[indP, ] <- work[1:nrow(work),]
  colnames( work2)<- colnames( work)
  work2<- data.frame( dateTime= rep( dateTime, nrow(work2)), 
                     work2)

  
  RD<- rbind( RD, work2)
  
}

cat( fill=TRUE)
CorpusCristi<- RD
save( CorpusCristi, file="CorpusCristi.rda")
ind<- CorpusCristi$pressure == 925

plot( (CorpusCristi$dateTime[ind]), CorpusCristi$windSpeed[ind],
      type="l")
ind<- CorpusCristi$pressure == 500

lines(CorpusCristi$dateTime[ind], CorpusCristi$windSpeed[ind], col="orange" )

plot(  RD$temp,RD$height)
plot(  RD$height, RD$pressure)
