#R
#chamber analysis guatavita
#from 2021 to 2024

library(ggplot2)
library(RColorBrewer)
library(reshape)
library(lubridate)

std <- function(x) sd(x)/sqrt(length(x))


pcdir<-"C:/Users/jubenavides/Dropbox/papers/Guatavita fluxes/stats"
macdir<-"/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/Guatavita fluxes/stats"
setwd(macdir)

precip<-read.csv("Compilado_ST2.csv",header=TRUE)

bd2021<-read.csv("bd2021.csv",header=TRUE,sep=",",dec=".")
bd2022<-read.csv("bd2022.csv",header=TRUE,sep=",",dec=".")
bd2023<-read.csv("bd2023.csv",header=TRUE,sep=",",dec=".")
bd2024<-read.csv("bd2024.csv",header=TRUE,sep=",",dec=".")
bd2025<-read.csv("bd2025.csv",header=TRUE,sep=",",dec=".")


wt<-read.csv("wtd_martos_21_25.csv",header=TRUE,sep=",",dec=".")

#wt$WTD_Fu2<-as.numeric(wt$WTD_Fu)

#### unir las bases de datos
bd_martos_chambers<- rbind(bd2021, bd2022, bd2023, bd2024, bd2025)

#reemplazar nombre para no usar Date
names(bd_martos_chambers)[3]<-"date2"

#crear codigo para identificar registros duplicados entre años
bd_martos_chambers$code<-paste(bd_martos_chambers$Site,bd_martos_chambers$Plot,bd_martos_chambers$date2,
                                bd_martos_chambers$Time,bd_martos_chambers$Day.Night,sep="_")
#borrar duplicados

bd_martos_chambers2<-bd_martos_chambers[order(c(bd_martos_chambers$code,bd_martos_chambers$CO2.Flux.umolm.2s.1.)),]

#bd_martos_chambers3<-bd_martos_chambers2[!duplicated(bd_martos_chambers2$code),]
#reformatear fecahs para que quedan todas en el mismo formato

a<-grepl("/",substr(bd_martos_chambers2$date2, nchar(bd_martos_chambers2$date2)-3, nchar(bd_martos_chambers2$date2)))



bd_martos_chambers2$Date2<-as.Date(ifelse(a,as.Date(bd_martos_chambers2$date2,"%d/%m/%y"),
            as.Date(bd_martos_chambers2$date2,"%d/%m/%Y")), origin = "1970-01-01") 
bd_martos_chambers2$code2<-paste(bd_martos_chambers2$Site,bd_martos_chambers2$Plot,bd_martos_chambers2$Date2,sep="_")

#lo mismo para el archivo de WT
b<-grepl("/",substr(wt$Date, nchar(wt$Date)-3, nchar(wt$Date)))

wt$Date2<-as.Date(ifelse(b,as.Date(wt$Date,"%m/%d/%y"),as.Date(wt$Date,"%m/%d/%Y")), origin = "1970-01-01") 

wt$code2<-paste(wt$Site,wt$Plot,wt$Date2,sep="_")

bd_martos_chambers3<-merge(bd_martos_chambers2,wt,by="code2",all=TRUE)


fluxes<-bd_martos_chambers3
fluxes$Site<-ifelse(is.na(fluxes$Site.x),fluxes$Site.y,fluxes$Site.x)

fluxes$Site2<-ifelse(fluxes$Site=="S1_new","S1",fluxes$Site)
fluxes$Site2<-ifelse(fluxes$Site=="S1_old","S1",fluxes$Site2)


fluxes$Plot<-ifelse(is.na(fluxes$Plot.x),fluxes$Plot.y,fluxes$Plot.x)

fluxes$Date2<-as.Date(ifelse(is.na(fluxes$Date2.x),fluxes$Date2.y,fluxes$Date2.x),origin = "1970-01-01") 

fluxes2<-fluxes[order(fluxes$Date2),]


precip$lengthcell<-nchar(precip$TIMESTAMP)
precip$TIMESTAMP2<-NA
precip$position2<-NA

size1<-dim(precip)[1]

for(i in seq(1,size1)){
 
    a<-precip[i,]
    positions<-unlist(gregexpr('/', a$TIMESTAMP))
    precip$position2[i]<-positions[2]
    }

precip$TIMESTAMP2<- ifelse(precip$lengthcell<15,paste(substr(precip$TIMESTAMP, 1,  precip$position2-1),"/20",
      substr(precip$TIMESTAMP, precip$position2+1, precip$lengthcell),sep=""),    precip$TIMESTAMP)


precip$time2<-as.POSIXct(precip$TIMESTAMP2,format="%d/%m/%Y %H:%M")
precip$Date2<-as.Date(precip$time2)

precip2<-aggregate(Rain_mm_Tot~Date2, data=precip,FUN=sum)
precip2<-precip2[order(precip2$Date2),]

precip2$months<-month(precip2$Date2)
precip2$years<-year(precip2$Date2)


precip3<-aggregate(Rain_mm_Tot~months+years,precip2,FUN=sum )
precip3$date2<-paste(precip3$years,precip3$months,"1",sep="-") 
precip3$date2<-as.Date(precip3$date2,format="%Y-%m-%d")

plot(precip3$date2,precip3$Rain_mm_Tot)
lines(precip3$date2,precip3$Rain_mm_Tot)

barplot(precip3$Rain_mm_Tot,names.arg= precip3$months, col="blue")

size<-dim(fluxes2)[1]

fluxes2$precip1<-NA
fluxes2$precip5<-NA
fluxes2$precip10<-NA
fluxes2$precip30<-NA

fluxes2<-subset(fluxes2,Date2>as.Date("2021-04-28"))
#fluxes2<-subset(fluxes2,Date2<as.Date("2025-01-01"))


#loop to estimate previous rain for each well
for(i in seq(1,size)){

a<-precip2[precip2$Date2 > fluxes2$Date2[i]-30 & precip2$Date2 < fluxes2$Date2[i], ]
  fluxes2$precip1[i]<-a$Rain_mm_Tot[length(a$Rain_mm_Tot)]
  fluxes2$precip5[i]<-sum(a$Rain_mm_Tot[length(a$Rain_mm_Tot)-5:length(a$Rain_mm_Tot)])
  fluxes2$precip10[i]<-sum(a$Rain_mm_Tot[length(a$Rain_mm_Tot)-10:length(a$Rain_mm_Tot)])
  fluxes2$precip30[i]<-sum(a$Rain_mm_Tot[1:length(a$Rain_mm_Tot)])
}


plot(fluxes2$precip30,fluxes2$WTD_Fu,na.rm=TRUE)

fluxes2$Wcode<-paste(fluxes2$Site2,fluxes2$Plot,sep="_")
wellcode<-unique(fluxes2$Wcode)

pred.watertable<-data.frame(Wcode="A",WaterTable3=0,reps=0,precip1=0,precip5=0,precip10=0,precip30=0)
pred.watertable<-pred.watertable[-1,]


for(i in wellcode){
 
   a<-subset(fluxes2,Wcode==i) 
    if(sum(a$WTD_Fu,na.rm=TRUE)>0){
      lmprec<-lm(WTD_Fu~precip1+precip5+precip10+precip30,data=a)      
      b<-data.frame(Wcode=a$Wcode,WaterTable3=predict(lmprec,a),reps=length(a[,1]),
            precip1=a$precip1,precip5=a$precip5,precip10=a$precip10,precip30=a$precip30)
      pred.watertable<-rbind(pred.watertable,b)    
    } else{
      c<-data.frame(Wcode=a$Wcode,WaterTable3=NA,reps=0,
          precip1=a$precip1,precip5=a$precip5,precip10=a$precip10,precip30=a$precip30)
      pred.watertable<-rbind(pred.watertable,c)
    }

  }


pred.watertable$WaterTable3<-ifelse(pred.watertable$WaterTable3>100,0,pred.watertable$WaterTable3)
pred.watertable$WaterTable3<-ifelse(pred.watertable$WaterTable3< -10,NA,pred.watertable$WaterTable3)

plot(pred.watertable$precip30,pred.watertable$WaterTable3,na.rm=TRUE)
points(fluxes2$precip30,fluxes2$WTD_Fu,col="red",na.rm=TRUE)


lmprec<-lm(WTD_Fu~precip1+precip5+precip10+precip30,data=fluxes2)
pred.wattab<-predict(lmprec,fluxes2)
pred.watertable$WaterTable3<-ifelse(is.na(pred.watertable$WaterTable3),pred.wattab,pred.watertable$WaterTable3)

#only one record of WT for each day for each well
plot(pred.wattab,pred.watertable$WaterTable3)

pred.watertable2<-aggregate(WaterTable3 ~ Wcode ,pred.watertable,FUN=mean)

fluxes3<-merge(fluxes2,pred.watertable2,by="Wcode",all=TRUE)

plot(fluxes3$WTD_Fu,fluxes3$WaterTable3,na.rm=TRUE)
fluxes3$WaterTable2<-ifelse(is.na(fluxes3$WTD_Fu),fluxes3$WaterTable3,fluxes3$WTD_Fu)


fluxes3<-fluxes3[order(fluxes3$Date2),]



write.csv(fluxes3,"fluxes4.csv")







