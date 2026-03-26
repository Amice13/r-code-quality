## get country data
setwd("C:\\Users\\zjw461\\Dropbox\\Maternal Global Model\\Papers\\typologies\\Dataverse") ## change directory
df<-read.csv("countryEsts.csv",as.is=T)

## calculate clusters (k-means)

#set initial cluster means to ensure stable results
centers<-matrix(nrow=7,ncol=2)
centers[1,]<-c(1140,0.06)
centers[2,]<-c(660,0.03)
centers[3,]<-c(400,0.02)
centers[4,]<-c(270,0.01)
centers[5,]<-c(160,0.005)
centers[6,]<-c(80,0.002)
centers[7,]<-c(30,0.0005)

set.seed(999) #set seed
test<-kmeans(cbind(df$MMR_2022,df$LTR_2022), centers, iter.max=100)

df$cluster<-test$cluster #get country cluster

### plot clusters (Figure 1a)
plot(sqrt(df$MMR_2022),df$LTR_2022*100,ylab="Lifetime Risk of Maternal Death (%)",xlab="MMR",xaxt='n',
     col=rgb(0,0,0,0),ylim=c(0,6.5))
vals<-c(10,25,50,100,200,300,500,750,1000)
axis(1,at=sqrt(vals),labels=vals)
addCluster<-function(c, curCol, curShade, curLbl){
  rows<-df$cluster==c
  points(sqrt(df$MMR_2022[rows]), df$LTR_2022[rows]*100, col=curCol, pch=16)
  minX<-min(sqrt(df$MMR_2022[rows]))
  maxX<-max(sqrt(df$MMR_2022[rows]))
  minY<-min(df$LTR_2022[rows]*100)
  maxY<-max(df$LTR_2022[rows]*100)
  rect(minX,minY,maxX,maxY,col=curShade,border=curCol)
  text((minX+maxX)/2,maxY,labels=curLbl,col=curCol,pos=3)
}

addCluster(1, rgb(215/255,48/255,39/255), rgb(215/255,48/255,39/255,0.1),' A')
addCluster(2, rgb(252/255,141/255,89/255), rgb(252/255,141/255,89/255,0.1), 'B')
addCluster(3, rgb(254/255,224/255,144/255), rgb(254/255,224/255,144/255,0.1), 'C')
addCluster(4, rgb(235/255,235/255,191/255), rgb(235/255,235/255,191/255,0.1), 'D')
addCluster(5, rgb(184/255,203/255,208/255), rgb(184/255,203/255,208/255, 0,0.1), 'E')
addCluster(6, rgb(145/255,191/255,219/255), rgb(145/255,191/255,219/255,0.1), 'F')
addCluster(7, rgb(69/255,117/255,180/255), rgb(69/255,117/255,180/255,0.1), 'G')

### map clusters (Figure 1b)
library(maps)
library(mapdata)

mapList<-subset(df,select=c("name","ISOCode"))
mapList$name[mapList$ISOCode=="CIV"]<-"Ivory Coast"
mapList$name[mapList$ISOCode=="TUR"]<-"Turkey"

colnames(mapList)<-c("name","Code")
#Recode to match list of regions
mapList$name[mapList$name=="Antigua and Barbuda"]<-"Antigua|Barbuda"
mapList$name[mapList$name=="Bolivia (Plurinational State of)"]<-"Bolivia"
mapList$name[mapList$name=="Brunei Darussalam"]<-"Brunei"
mapList$name[mapList$name=="Cabo Verde"]<-"Cape Verde"
mapList$name[mapList$name=="Congo"]<-"Republic of Congo"
mapList$name[mapList$name=="C?te d'Ivoire"]<-"Ivory Coast"
mapList$name[mapList$name=="Czechia"]<-"Czech Republic"
mapList$name[mapList$name=="France"]<-"France|French Guiana|Reunion"
mapList$name[mapList$name=="Iran (Islamic Republic of)"]<-"Iran"
mapList$name[mapList$name=="Dem. People's Republic of Korea"]<-"North Korea"
mapList$name[mapList$name=="Republic of Korea"]<-"South Korea"
mapList$name[mapList$name=="Lao People's Democratic Republic"]<-"Laos"
mapList$name[mapList$name=="Micronesia (Fed. States of)"]<-"Micronesia"
mapList$name[mapList$name=="Republic of Moldova"]<-"Moldova"
mapList$name[mapList$name=="Morocco"]<-"Morocco|Western Sahara"
mapList$name[mapList$name=="Niger"]<-"Niger\\b"
mapList$name[mapList$name=="Russian Federation"]<-"Russia"
mapList$name[mapList$name=="Saint Kitts and Nevis"]<-"Saint Kitts|Nevis"
mapList$name[mapList$name=="Saint Lucia"]<-"Saint Lucia"
mapList$name[mapList$name=="Saint Vincent and the Grenadines"]<-"Saint Vincent|Grenadines"
mapList$name[mapList$name=="Eswatini"]<-"Swaziland"
mapList$name[mapList$name=="United Republic of Tanzania"]<-"Tanzania"
mapList$name[mapList$name=="Syrian Arab Republic"]<-"Syria"
mapList$name[mapList$name=="Trinidad and Tobago"]<-"Trinidad|Tobago"
mapList$name[mapList$name=="T?rkiye"]<-"Turkey"
mapList$name[mapList$name=="Tuvalu"]<-"Kiribati" #Redirect
mapList$name[mapList$name=="United Kingdom"]<-"UK:"
mapList$name[mapList$name=="United States of America"]<-"USA"
mapList$name[mapList$name=="Venezuela (Bolivarian Republic of)"]<-"Venezuela"
mapList$name[mapList$name=="Viet Nam"]<-"Vietnam"
mapList$name[mapList$name=="State of Palestine"]<-"Palestine"

clusterCols<-c(rgb(215/255,48/255,39/255),
               rgb(252/255,141/255,89/255),
               rgb(254/255,224/255,144/255),
               rgb(235/255,235/255,191/255),
               rgb(184/255,203/255,208/255),
               rgb(145/255,191/255,219/255),
               rgb(69/255,117/255,180/255))

map("world",mapList$name)
for(c in 1:nrow(df)){
  curName=mapList$name[mapList$Code==df$ISOCode[c]]
  shade=clusterCols[df$cluster[c]]
  map("world",curName,fill=T,add=T,col=shade)
}
par(xpd=T)
legend("bottom",ncol=7,inset=-0.15,c("A","B","C","D","E","F","G"),fill=c(rgb(215/255,48/255,39/255),
                                                                         rgb(252/255,141/255,89/255),
                                                                         rgb(254/255,224/255,144/255),
                                                                         rgb(235/255,235/255,191/255),
                                                                         rgb(184/255,203/255,208/255),
                                                                         rgb(145/255,191/255,219/255),
                                                                         rgb(69/255,117/255,180/255)))

### calculate cluster means (Table 2)
curTable<-data.frame(matrix(nrow=7,ncol=12))
colnames(curTable)<-c("group","mmr","ltr","matDeaths","tfr","cpr","facDel","cSection",
                      "impact_fp","impact_commLink","impact_facBirths","impact_quality")
curTable$group<-c("A","B","C","D","E","F","G")
for(i in 1:7){ #for each group
  rows<-df$cluster==i
  curTable$mmr[i]<-round(weighted.mean(df$MMR_2022[rows],df$popSize_2022[rows]))
  curTable$ltr[i]<-round(weighted.mean(df$LTR_2022[rows]*100,df$popSize_2022[rows]),2)
  curTable$matDeaths[i]<-round(sum(df$matDeaths_2022[rows]),-2)
  curTable$tfr[i]<-round(weighted.mean(df$TFR_2022[rows],df$popSize_2022[rows]),2)
  curTable$cpr[i]<-round(weighted.mean(df$CPR_2022[rows]*100,df$popSize_2022[rows]),1)
  curTable$facDel[i]<-round(weighted.mean(df$facDel_2022[rows]*100,df$popSize_2022[rows]),1)
  curTable$cSection[i]<-round(weighted.mean(df$cSection_2022[rows]*100,df$popSize_2022[rows]),1)
  #policy impacts
  curTable$impact_fp[i]<-round(weighted.mean(df$impact_fp[rows],df$popSize_2022[rows],na.rm=T),1)
  curTable$impact_commLink[i]<-round(weighted.mean(df$impact_commLink[rows],df$popSize_2022[rows],na.rm=T),1)
  curTable$impact_facBirths[i]<-round(weighted.mean(df$impact_facBirths[rows],df$popSize_2022[rows],na.rm=T),1)
  curTable$impact_quality[i]<-round(weighted.mean(df$impact_quality[rows],df$popSize_2022[rows],na.rm=T),1)
}




