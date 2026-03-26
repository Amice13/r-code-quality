
setwd("~/Dropbox/io_endogenous_replication/main_text_do/figures/figure_3/")


data_12_18<-read.csv("data_12_18.csv")

data_12_15<-read.csv("data_12_15.csv")




plot(data_12_15$U_x,data_12_15$U_y,pch=16,col="grey",type="n",axes=F, xlab="Urban Population - 1200",ylab="Urban Population - 1500")

grid()

points(data_12_15$U_x,data_12_15$U_y,pch=16,col="grey")

axis(1);axis(2)


abline(lm(U_y~U_x,data=data_12_15),lwd=2,col="red")


b_hat<-round(coef(lm(U_y~U_x,data=data_12_15))[2],2)
se_hat<-round(sqrt(vcov(lm(U_y~U_x,data=data_12_15))  )[2,2],3)

text( 20, 420  , expression(hat(beta) == 1.70))
text( 20, 400  , "        (0.05)")


########################


plot(data_12_18$U_x,data_12_18$U_y,pch=16,col="grey",type="n",axes=F, xlab="Urban Population - 1200",ylab="Urban Population - 1800")

grid()

points(data_12_18$U_x,data_12_18$U_y,pch=16,col="grey")

axis(1);axis(2)


abline(lm(U_y~U_x,data=data_12_18),lwd=2,col="red")


b_hat<-round(coef(lm(U_y~U_x,data=data_12_18))[2],2)
se_hat<-round(sqrt(vcov(lm(U_y~U_x,data=data_12_18))  )[2,2],3)

text( 20, 1040  , expression(hat(beta) == 3.99))
text( 20, 990  , "      (0.20)")




##########################


plot(data_12_15$U_y,data_12_18$U_y,pch=16,col="grey",type="n",axes=F, xlab="Urban Population - 1500",ylab="Urban Population - 1800")

grid()

points(data_12_15$U_y,data_12_18$U_y,pch=16,col="grey")

axis(1);axis(2)


abline(lm(data_12_18$U_y~data_12_15$U_y),lwd=2,col="red")



b_hat<-round(coef(lm(data_12_18$U_y~data_12_15$U_y,data=data_12_18))[2],2)
se_hat<-round(sqrt(vcov(lm(data_12_18$U_y~data_12_15$U_y,data=data_12_18))  )[2,2],3)

text( 20, 1040  , expression(hat(beta) == 2.33))
text( 20, 990  , "      (0.08)")

