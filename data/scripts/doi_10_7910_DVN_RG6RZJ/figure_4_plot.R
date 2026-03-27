
setwd("~/Dropbox/io_endogenous_replication/main_text_do/figures/figure_4/")




quant_out<-read.csv("quantile_reg_output.csv",header=T)


ln_hi<-ln_lo<-coef_hi<-coef_lo<-coefs<-ln_coefs<-rep(NA,9)

for(i in 1:9){

ln_coefs[i]<-as.numeric(as.character(quant_out[1,i]))

coefs[i]<-as.numeric(as.character(quant_out[3,i]))


ln_lo[i]<-as.numeric(strsplit(as.character(quant_out[2,i]), ",")[[1]][1])
ln_hi[i]<-as.numeric(strsplit(as.character(quant_out[2,i]), ",")[[1]][2])


coef_lo[i]<-as.numeric(strsplit(as.character(quant_out[4,i]), ",")[[1]][1])
coef_hi[i]<-as.numeric(strsplit(as.character(quant_out[4,i]), ",")[[1]][2])





}



plot(1:9,coefs,cex=.75,pch=16,axes=F,type="n",ylab=expression(beta),xlab="Quantile")

grid()

points(1:9,coefs,cex=.75,pch=16)


for(i in 1:9){
  
  
 lines(c(i,i),c(coef_hi[i],coef_lo[i]),lwd=2) 
  
  
}


axis(1,at=1:9,labels=seq(.1,.9,by=.1))
axis(2,seq(.6,2.2,by=.2))

mtext("Quantile Regression Coefficients",3,cex=1.5,font=2)
mtext("Outcome: Urban Population",3,cex=1,line=-1)









plot(1:9,ln_coefs,cex=.75,pch=16,axes=F,type="n",ylab=expression(beta),xlab="Quantile",ylim=c(.88,1.1))

grid()

points(1:9,ln_coefs,cex=.75,pch=16)


for(i in 1:9){
  
  
  lines(c(i,i),c(ln_hi[i],ln_lo[i]),lwd=2) 
  
  
}


axis(1,at=1:9,labels=seq(.1,.9,by=.1))
axis(2,seq(.8,1.1,by=.05))

mtext("Quantile Regression Coefficients",3,cex=1.5,font=2)
mtext("Outcome: log(Urban Population)",3,cex=1,line=-1)
