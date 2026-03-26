#remove(list=ls())
source("code/build.R")

#suppressMessages({
  
tb1<-c(0,table(data$Ordered_Occ))
names(tb1)<-seq(1,10)

pdf("figure_output/Figure_A1.pdf",8,6)

plot(1:10,tb1,type="n",axes=F,xlab="",ylab="",xlim=c(0,11))
grid()
for(i in 2:10){
  rect(names(tb1)[i-1],0, names(tb1)[i],tb1[i],border=F,col="dark grey")
}

axis(1, seq(-.5,10.5,by=1),labels =c("","","Higher", "Lower","Employers", 
                                     "Managers","Clerical","Foremen",
                                     "Skilled","Semi-Skilled","Unskilled",""),cex.axis=.5,tick=F,line=-.5)

axis(1, seq(-.5,10.5,by=1),labels =c("","","Professional", "Professional","Proprietors", 
                                     "Administrators","Workers","Supervisors",
                                     "Manual","Manual","Manual",""),cex.axis=.5,tick=F,line=-.15)

axis(1, seq(1.5,9.5,by=1),labels =c("I","II","II","II","III","III","III","IV","V"),line=.5,cex.axis=.75,tick=F)
axis(1,at=.5,label="Social Class:",line=.5,tick=F,cex.axis=.5)
axis(1, seq(1.5,9.5,by=1),labels =c("IA","IB","IIA","IIB","III","IV","V","VI","VII"),line=1,cex.axis=.75,tick=F)
axis(1,at=.5,label="Occupational Staus:",line=1,tick=F,cex.axis=.5)
axis(1, seq(1.5,9.5,by=1),lwd=.5,labels = rep("",9))
axis(1, c(-1.5,12.5),lwd=.5,labels = rep("",2))
axis(2,seq(0,1500,by=250))

dev.off()

#})