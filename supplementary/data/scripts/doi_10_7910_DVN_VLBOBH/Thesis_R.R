# < Yilin Deng >
# QMSS 5999 Thesis

# Load data set
setwd("/Users/user/Desktop/thesis data")
library("xlsx")
data <- read.xlsx ("Thesis Data WB.xlsx",sheetName = "Work Data", header = T, na.strings="?" )
#data=as.matrix(data)
attach(data)

#Summary statistics
summary(data)
sd(cred.private)
#correlations:
c0<-data.frame(pc.growth,mkt.cap,tradable.cap,turnover,value.traded,cred.private)
cor(c0)

# install.packages("sandwich")
library("sandwich")


# 5-year rolling time windows to abstract from business cycle effect
# calculate moving averages
growth.ma5<-vector(length=17)
# X_stock 
mkt.cap.ma5<-vector(length=17)
turnover.ma5<-vector(length=17)
value.traded.ma5<-vector(length=17)
vol.ma5<-vector(length=17)
tradable.ma5<-vector(length=17)
# X_bank
credit.ma5<-vector(length=17)
  
for (i in 1:17) {
  # use averages reported to World Bank Databank
  growth.ma5[i] = (pc.growth[i]+pc.growth[i+1]+pc.growth[i+2]+pc.growth[i+3]+pc.growth[i+4])/5

  mkt.cap.ma5[i]=(mkt.cap[i]+mkt.cap[i+1]+mkt.cap[i+2]+mkt.cap[i+3]+mkt.cap[i+4])/5
  turnover.ma5[i]=(turnover[i]+turnover[i+1]+turnover[i+2]+turnover[i+3]+turnover[i+4])/5
  value.traded.ma5[i]=(value.traded[i]+value.traded[i+1]+value.traded[i+2]+value.traded[i+3]+value.traded[i+4])/5
  vol.ma5[i]=(p.vol[i]+p.vol[i+1]+p.vol[i+2]+p.vol[i+3]+p.vol[i+4])/5
  
  credit.ma5[i]=(cred.private[i]+cred.private[i+1]+cred.private[i+2]+cred.private[i+3]+cred.private[i+4])/5
}


for (i in 3:17) {
  tradable.ma5[i]=(tradable.cap[i]+tradable.cap[i+1]+tradable.cap[i+2]+tradable.cap[i+3]+tradable.cap[i+4])/5
}

###### Contemporaneous OLS ###########

ols = lm(growth.ma5[3:17] ~ tradable.ma5[3:17] + value.traded.ma5[3:17] +turnover.ma5[3:17]  + credit.ma5[3:17] )
summary(ols)

######  2SLS  ################
# use IV and perform 2SLS
# 1st stage LS
s1.cap.5 = lm(mkt.cap.ma5 ~ mkt.cap[1:17])
summary(s1.cap.5)
iv5.cap=s1.cap.5$fitted.values

s1.turnover.5= lm(turnover.ma5 ~ turnover[1:17])
summary(s1.turnover.5)
iv5.turn=s1.turnover.5$fitted.values

s1.val.5 = lm(value.traded.ma5 ~ value.traded[1:17])
summary(s1.val.5)
iv5.val=s1.val.5$fitted.values

s1.cred.5 = lm(credit.ma5 ~ cred.private[1:17])
summary(s1.cred.5)
iv5.cred=s1.cred.5$fitted.values

s1.tradable = lm(tradable.ma5 ~ tradable.cap[3:17])
summary(s1.tradable)
iv5.trad=s1.tradable$fitted.values

# s1.vol.5 = lm(vol.ma5 ~ p.vol[1:17])
# summary(s1.vol.5)
# iv5.vol=s1.vol.5$fitted.values

# 2nd stage LS
stage2.ls1 = lm(growth.ma5 ~ iv5.val + iv5.cap +iv5.turn + iv5.cred )
summary(stage2.ls1)
ls.res=resid(stage2.ls1)
# plot residuals against time to test for serial correlation
plot(year[1:17],ls.res,ylab="res",xlab="year",main="residual plot 1")

# compute Newey West estimator 
NW1 = NeweyWest(stage2.ls1)
#coef(stage2.ls1)
q1 = coef(stage2.ls1)/sqrt(diag(NW1))
p1 = pnorm(q1,mean=0,sd=1)
p1

# tradable shares
stage2.ls2 = lm(growth.ma5[3:17] ~ iv5.val[3:17] + iv5.trad[3:17] +iv5.turn[3:17] + iv5.cred[3:17])
summary(stage2.ls2)
ls.res2=resid(stage2.ls2)

# compute Newey West estimator 
NW2 = NeweyWest(stage2.ls2)
#coef(stage2.ls1)
q2 = coef(stage2.ls2)/sqrt(diag(NW2))
p2 = pnorm(q2,mean=0,sd=1)
p2

# IV diagnostic tests

#install.packages("AER")
library(AER)

iv.fin <- ivreg(growth.ma5[3:17] ~ tradable.ma5[3:17] + turnover.ma5[3:17] + value.traded.ma5[3:17]  + credit.ma5[3:17]
                |tradable.cap[3:17] + turnover.ma5[3:17] + value.traded[3:17]+ cred.private[3:17])
summary(iv.fin,diagnostic=T)




