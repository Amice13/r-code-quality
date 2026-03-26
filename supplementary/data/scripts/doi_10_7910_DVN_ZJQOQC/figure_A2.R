#remove(list=ls())
source("code/build.R")


  
#suppressMessages({

##Figure A2 Left panel 
pdf("figure_output/Figure_A2_Left.pdf",8,6)
hist_data=hist(data$age, breaks=c(15,20,25,30,35,40,45,50,55,60,65), plot=FALSE)
hist_data$counts=hist_data$counts/sum(hist_data$counts)*100
plot(hist_data, type="n",main="Age Distribution among Recruits",xlab="Age Group",ylab="Percentage",xlim=c(15,65),ylim=c(0,35),axes=F,cex.main=1.5,cex.lab=1.5)

grid()
for(i in 1:10){
  rect(hist_data$breaks[i], 0, hist_data$breaks[i+1],hist_data$counts[i],border=F,col="dark grey")
}

axis(1, seq(15,65,by=5),lwd=.75,labels = rep("",11))
axis(1, seq(15,65,by=5),labels =c("15","20","25","30","35","40","45","50","55","60","65"),line=0,cex.axis=1.5,tick=F)

axis(2,seq(0,35,by=5),lwd=.75,cex.axis=1.5)

dev.off()






age<-read.csv("data/britain_population1931.csv")
male<-age[age$gender=="male",]
male_adults<-subset(male,age_group %in% c("15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64"))
sums_male<-unlist(tapply(male_adults$population_1931,male_adults$age_group,sum))
print(sums_male)

total<-age[age$gender=="total",]
total_adults<-subset(total,age_group %in% c("15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64"))
sums_total<-tapply(total_adults$population_1931,total_adults$age_group,sum)
print(sums_total)


##Figure A2 Right panel
pdf("figure_output/Figure_A2_Right.pdf",8,6)
hist_male<-sums_male/sum(sums_male)*100

plot(hist_male, type="n",main="Age Distribution of British Male Adults (15-64, 1931)",xlab="Age Group",ylab="Percentage",xlim=c(15,65),ylim=c(0,35),axes=F,cex.main=1.5,cex.lab=1.5)

grid()
for(i in 1:10){
  rect(hist_data$breaks[i], 0, hist_data$breaks[i+1],hist_male[i],border=F,col="dark grey")
}

axis(1, seq(15,65,by=5),lwd=.75,labels = rep("",11))
axis(1, seq(15,65,by=5),labels =c("15","20","25","30","35","40","45","50","55","60","65"),line=0,cex.axis=1.5,tick=F)

axis(2,seq(0,35,by=5),lwd=.75,cex.axis=1.5)
dev.off()


#  })



