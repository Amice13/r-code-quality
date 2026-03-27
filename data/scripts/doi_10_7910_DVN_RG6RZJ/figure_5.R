

ye<-c(-1.172031,-.8920766,-.8430985,-.9565566,0)

U_lo<-rep(NA,7)

U_hi<-rep(NA,7)
U_super_hi<-rep(NA,7)

U_lo[1:2]<-c(log(1+.001),log(8+.001))

U_hi[1:2]<-c(log(12+.001),log(19+.001))

U_super_hi[1:2]<-c(log(23+.001),log(30+.001))



for(i in 3:7){
  
U_lo[i]<-  1.344861 + .7710488*U_lo[i-1]+ .245438*U_lo[i-2] +ye[i-2]   
U_hi[i]<-  1.344861 + .7710488*U_hi[i-1]+ .245438*U_hi[i-2] +ye[i-2]   
U_super_hi[i]<-1.344861 + .7710488*U_super_hi[i-1]+ .245438*U_super_hi[i-2] +ye[i-2]
}

plot(seq(1200,1800,by=100), exp(U_super_hi),ylab="Predicted Urban Population",xlab="Year",ylim=c(0,550),type="n",axes=F)

grid()

lines(seq(1200,1800,by=100), exp(U_super_hi),lwd=2)

points(seq(1200,1800,by=100), exp(U_super_hi),pch=19,col="blue")

lines(seq(1200,1800,by=100), exp(U_hi),lwd=2)

points(seq(1200,1800,by=100), exp(U_hi),pch=19,col="red")

lines(seq(1200,1800,by=100), exp(U_lo),lwd=2)

points(seq(1200,1800,by=100), exp(U_lo),pch=19,col="yellow")


axis(1);axis(2)


