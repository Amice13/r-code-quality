#Figures for Macro-Micro Paper
#Thomas J. Leeper
#November 27, 2011

pa = read.csv("pa-question-data.csv")

library("car")

#Create date-formatted index (to be used for axis labels)
dateaxis=as.data.frame(matrix(nrow=144,ncol=3))
colnames(dateaxis)=c("month","yearmonth","axismonth")
i=0;j=1
while (i<144) {
  for(j in 1:12){
  x=i+j
  dateaxis[x,1]=x
    if(x<=120) dateaxis[x,2]=paste("200",i/12,"",month.abb[j],sep="")
    if(x>120) dateaxis[x,2]=paste("20",i/12,"",month.abb[j],sep="")
    if(x<=120) dateaxis[x,3]=paste("200",i/12,"-",month.abb[j],sep="")
    if(x>120) dateaxis[x,3]=paste("20",i/12,"-",month.abb[j],sep="")
  }
  i=i+12
}


################################################################################

#PA iPoll Questions
for(i in 1:dim(pa)[1]){
  x=as.character(pa[i,3])
  pa$yearmonth[i]=paste(substr(x,nchar(x)-3,nchar(x)),substr(x,nchar(x)-7,nchar(x)-5),sep="")
}


#create standardized "month" variable using dateaxis
for(i in 1:dim(pa)[1]){
  pa$month[i]=dateaxis[dateaxis$yearmonth==pa$yearmonth[i],1]
}



################################################################################


#New York Times Search
#Search Criteria:
#"patriot act" AND SECTION(SECTION A) AND NOT SECTION(Editorial Desk)
#2001 to 2011
#New York Times, The

nyt.read=readLines(file("New York Times Articles.txt"))

nyt.dates=NA
i=2; j=1
while(i<1888){
  nyt.dates[j]=nyt.read[i]
  i=i+4
  j=j+1
}

#separate date, month, and year
dates=matrix(nrow=472,ncol=3)
date.temp=NA
for(i in 1:length(nyt.dates)){
  date.temp=strsplit(nyt.dates[i],"Times, ")[[1]][2]
  date.temp=strsplit(date.temp,"day")[[1]][1]
  dates[i,]=strsplit(date.temp," ")[[1]][1:3]
}

#combined date, month, and year
dates.bind=NA
#dates.format=NA
for(i in 1:dim(dates)[1]){
  dates.bind[i]=substr(paste(dates[i,3],dates[i,1],sep=""),1,7) #recombine in logical order
  #dates.bind[i]=substr(dates.bind[i],1,nchar(dates.bind[i])-1) #delete trailing comma
  #dates.format[i]=as.Date(substr(paste(tolower(dates[i,3]),dates[i,1],sep=""),1,7),"%Y%b")    #convert to standard date format
}
#as.Date(substr(paste(dates[i,3],tolower(dates[i,1]),sep=""),1,7),"%Y%b")

nyt=as.data.frame(dates.bind)
colnames(nyt)="yearmonth"
for(i in 1:dim(nyt)[1]){
  nyt$month[i]=dateaxis[dateaxis$yearmonth==nyt$yearmonth[i],1]
}



################################################################################
#Plotting

#Create 6-month interval axis labels
axislabels=matrix(nrow=23,ncol=2)
i=6; j=1
while (i<max(dateaxis[,1])){
  axislabels[j,1]=dateaxis[i,3]
  axislabels[j,2]=i
  i=i+6; j=j+1
}
axislabels=as.data.frame(axislabels)
names(axislabels)=c("yearmonth","month")
axislabels$month=as.numeric(as.character(axislabels$month))

#Create the comparative density plot
pdf("patriot-act.pdf",width=7.5,height=4)
#jpeg("patriot-act.jpg",width=3000,height=1600,res=300)

#significant dates
##sort(table(pa$month))
##sort(table(nyt$month))
#month==23: October 2001 initial passage; first mention in NYT
#month==43: First iPoll Question
#month==50: First peak of iPoll Questions in Feb 2004 (11)
#month==67: July 2005 reauthorization
#month==68: Second largest peak of iPoll Questions in August 2005 (38)
#month==72: Max NYT mentions in December 2005 (30)
#month==122: temporary reauthorization
#month==137: May 2011 Reauthorization


#nyt mentions
#plot(density(pa$month,bw=2),type="l",col="gray50",lwd=3,xaxt="n",yaxt="n",xlab="",ylab="",xlim=c(18,140),ylim=c(0,.09),main="PATRIOT Act Questions and Mentions",sub="") #plot density of nyt mentions
hist(pa$month,seq(1,144,by=3),freq=FALSE,col="gray50",border="white",lwd=3,xaxt="n",yaxt="n",xlab="",ylab="Monthly Survey Questions",xlim=c(18,140),ylim=c(0,.09),main="",sub="") #plot density of nyt mentions; main="PATRIOT Act Survey Questions and Mentions in New York Times"
#text(100,.03,"Survey Questions",col="gray50",font=2)
axis(1,axislabels$month,axislabels$yearmonth,tick=FALSE,las=2,cex=.1)
axis(2,seq(0,.1,length.out=9),seq(0,40,by=5),tick=FALSE,las=2,cex=.1,col="gray50")

#survey questions
lines(density(nyt$month,bw=1.5),type="l",col="black",lwd=3)  #plot PA mentions
#text(90,.077,"PATRIOT Act",col="blue",font=2)
text(110,.015,"NYT Mentions",col="black",font=2)


lines(c(75,85),c(.035,.045),lwd=1)
text(80,.047,"Monthly NYT Mentions = 30",cex=0.5,pos=4,col="black",font=1)

#text(23,.037,"Initial Authorization",cex=0.5,srt=90,pos=2,col="gray50",font=1)
#text(42.5,.045,"First Survey in Field",cex=0.5,srt=90,pos=2,col="gray50",font=1)
#text(63,.047,"Reauthorization",cex=0.5,srt=90,pos=4,col="gray50",font=1)
#text(64,.047,"Monthly Questions = 38",cex=0.5,srt=90,pos=4,col="gray50",font=1)
#text(136,.037,"Reauthorization",cex=0.5,srt=90,pos=2,col="gray50",font=1)

dev.off()



