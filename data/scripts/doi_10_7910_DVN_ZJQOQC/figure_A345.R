
#remove(list=ls())
source("code/build.R")

#suppressMessages({
  


####Figure A3

pdf("figure_output/Figure_A3.pdf",12,6)

par(mfrow=c(1,2))
par(mar=c(3,5,4,1))

ft1<-summary(feols(comm~as.factor(age)-1,data[data$Occupation!="",]))

unq_age<-unique(sort(data$age))

plot(unq_age, coef(ft1),ylim=c(-1,2),type="n",axes=F,ylab=expression(alpha[age]),xlab="Age",main="No Controls")
grid()

for(i in 1:length(unq_age)){
  
  lines(c(unq_age[i], unq_age[i]) , 
        c(coef(ft1)[i]+ sqrt(diag(vcov(ft1)))[i]*1.96, coef(ft1)[i]- sqrt(diag(vcov(ft1)))[i]*1.96),
        lwd=2,col="dark grey"
  )
  
  
  lines( c(unq_age[i]+.2, unq_age[i]-.2 ),   
         c(coef(ft1)[i]+ sqrt(diag(vcov(ft1)))[i]*1.96, coef(ft1)[i]+ sqrt(diag(vcov(ft1)))[i]*1.96 ),
         lwd=2,col="dark grey"
  )
  
  
  lines( c(unq_age[i]+.2, unq_age[i]-.2 ),   
         c(coef(ft1)[i]- sqrt(diag(vcov(ft1)))[i]*1.96, coef(ft1)[i]- sqrt(diag(vcov(ft1)))[i]*1.96 ),
         lwd=2,col="dark grey"
  ) 
  
}


points(unq_age, coef(ft1),pch=16,col="dark grey")

axis(1,seq(15,75,by=5));axis(2)



ft2<-summary(feols(comm~as.factor(age)-1|Class+arrival+country+woman,data[data$Occupation!="",]))

unq_age<-unq_age[unq_age %in% c(48,58,62)==F]


plot(unq_age, coef(ft2),ylim=c(-1,2),type="n",axes=F,ylab=expression(alpha[age]),xlab="Age",main="With Controls")
grid()

for(i in 1:length(unq_age)){
  
  lines(c(unq_age[i], unq_age[i]) , 
        c(coef(ft2)[i]+ sqrt(diag(vcov(ft2)))[i]*1.96, coef(ft2)[i]- sqrt(diag(vcov(ft2)))[i]*1.96),
        lwd=2,col="dark grey"
  )
  
  
  lines( c(unq_age[i]+.2, unq_age[i]-.2 ),   
         c(coef(ft2)[i]+ sqrt(diag(vcov(ft2)))[i]*1.96, coef(ft2)[i]+ sqrt(diag(vcov(ft2)))[i]*1.96 ),
         lwd=2,col="dark grey"
  )
  
  
  lines( c(unq_age[i]+.2, unq_age[i]-.2 ),   
         c(coef(ft2)[i]- sqrt(diag(vcov(ft2)))[i]*1.96, coef(ft2)[i]- sqrt(diag(vcov(ft2)))[i]*1.96 ),
         lwd=2,col="dark grey"
  ) 
  
}


points(unq_age, coef(ft2),pch=16,col="dark grey")

axis(1,seq(15,75,by=5));axis(2)

dev.off()





####Figure A4
pdf("figure_output/Figure_A4.pdf",12,6)

par(mfrow=c(1,2))
par(mar=c(3,5,4,1))

ft1<-summary(feols(deserter~as.factor(age)-1,data[data$Occupation!="",]))

unq_age<-unique(sort(data$age))

plot(unq_age, coef(ft1),ylim=c(-1,2),type="n",axes=F,ylab=expression(alpha[age]),xlab="Age",main="No Controls")
grid()

for(i in 1:length(unq_age)){
  
  lines(c(unq_age[i], unq_age[i]) , 
        c(coef(ft1)[i]+ sqrt(diag(vcov(ft1)))[i]*1.96, coef(ft1)[i]- sqrt(diag(vcov(ft1)))[i]*1.96),
        lwd=2,col="dark grey"
  )
  
  
  lines( c(unq_age[i]+.2, unq_age[i]-.2 ),   
         c(coef(ft1)[i]+ sqrt(diag(vcov(ft1)))[i]*1.96, coef(ft1)[i]+ sqrt(diag(vcov(ft1)))[i]*1.96 ),
         lwd=2,col="dark grey"
  )
  
  
  lines( c(unq_age[i]+.2, unq_age[i]-.2 ),   
         c(coef(ft1)[i]- sqrt(diag(vcov(ft1)))[i]*1.96, coef(ft1)[i]- sqrt(diag(vcov(ft1)))[i]*1.96 ),
         lwd=2,col="dark grey"
  ) 
  
}


points(unq_age, coef(ft1),pch=16,col="dark grey")

axis(1,seq(15,75,by=5));axis(2)



ft2<-summary(feols(deserter~as.factor(age)-1|Class+arrival+country+woman,data[data$Occupation!="",]))

unq_age<-unq_age[unq_age %in% c(48,58,62)==F]


plot(unq_age, coef(ft2),ylim=c(-1,2),type="n",axes=F,ylab=expression(alpha[age]),xlab="Age",main="With Controls")
grid()

for(i in 1:length(unq_age)){
  
  lines(c(unq_age[i], unq_age[i]) , 
        c(coef(ft2)[i]+ sqrt(diag(vcov(ft2)))[i]*1.96, coef(ft2)[i]- sqrt(diag(vcov(ft2)))[i]*1.96),
        lwd=2,col="dark grey"
  )
  
  
  lines( c(unq_age[i]+.2, unq_age[i]-.2 ),   
         c(coef(ft2)[i]+ sqrt(diag(vcov(ft2)))[i]*1.96, coef(ft2)[i]+ sqrt(diag(vcov(ft2)))[i]*1.96 ),
         lwd=2,col="dark grey"
  )
  
  
  lines( c(unq_age[i]+.2, unq_age[i]-.2 ),   
         c(coef(ft2)[i]- sqrt(diag(vcov(ft2)))[i]*1.96, coef(ft2)[i]- sqrt(diag(vcov(ft2)))[i]*1.96 ),
         lwd=2,col="dark grey"
  ) 
  
}


points(unq_age, coef(ft2),pch=16,col="dark grey")

axis(1,seq(15,75,by=5));axis(2)

dev.off()






###Figure A5
pdf("figure_output/Figure_A5.pdf",12,6)

par(mfrow=c(1,2))
par(mar=c(3,5,4,1))

ft1<-summary(feols(killed~as.factor(age)-1,data[data$Occupation!="",]))

unq_age<-unique(sort(data$age))

plot(unq_age, coef(ft1),ylim=c(-1,2),type="n",axes=F,ylab=expression(alpha[age]),xlab="Age",main="No Controls")
grid()

for(i in 1:length(unq_age)){
  
  lines(c(unq_age[i], unq_age[i]) , 
        c(coef(ft1)[i]+ sqrt(diag(vcov(ft1)))[i]*1.96, coef(ft1)[i]- sqrt(diag(vcov(ft1)))[i]*1.96),
        lwd=2,col="dark grey"
  )
  
  
  lines( c(unq_age[i]+.2, unq_age[i]-.2 ),   
         c(coef(ft1)[i]+ sqrt(diag(vcov(ft1)))[i]*1.96, coef(ft1)[i]+ sqrt(diag(vcov(ft1)))[i]*1.96 ),
         lwd=2,col="dark grey"
  )
  
  
  lines( c(unq_age[i]+.2, unq_age[i]-.2 ),   
         c(coef(ft1)[i]- sqrt(diag(vcov(ft1)))[i]*1.96, coef(ft1)[i]- sqrt(diag(vcov(ft1)))[i]*1.96 ),
         lwd=2,col="dark grey"
  ) 
  
}


points(unq_age, coef(ft1),pch=16,col="dark grey")

axis(1,seq(15,75,by=5));axis(2)



ft2<-summary(feols(killed~as.factor(age)-1|Class+arrival+country+woman,data[data$Occupation!="",]))

unq_age<-unq_age[unq_age %in% c(48,58,62)==F]


plot(unq_age, coef(ft2),ylim=c(-1,2),type="n",axes=F,ylab=expression(alpha[age]),xlab="Age",main="With Controls")
grid()

for(i in 1:length(unq_age)){
  
  lines(c(unq_age[i], unq_age[i]) , 
        c(coef(ft2)[i]+ sqrt(diag(vcov(ft2)))[i]*1.96, coef(ft2)[i]- sqrt(diag(vcov(ft2)))[i]*1.96),
        lwd=2,col="dark grey"
  )
  
  
  lines( c(unq_age[i]+.2, unq_age[i]-.2 ),   
         c(coef(ft2)[i]+ sqrt(diag(vcov(ft2)))[i]*1.96, coef(ft2)[i]+ sqrt(diag(vcov(ft2)))[i]*1.96 ),
         lwd=2,col="dark grey"
  )
  
  
  lines( c(unq_age[i]+.2, unq_age[i]-.2 ),   
         c(coef(ft2)[i]- sqrt(diag(vcov(ft2)))[i]*1.96, coef(ft2)[i]- sqrt(diag(vcov(ft2)))[i]*1.96 ),
         lwd=2,col="dark grey"
  ) 
  
}


points(unq_age, coef(ft2),pch=16,col="dark grey")

axis(1,seq(15,75,by=5));axis(2)

dev.off()






#})

