# Complementary Method: Test mutiple Arima Models and chose the one with the lowest AIC

aic<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(ts_A_TotTraffic,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic[p+1,q+1]<-aic.p.q
  }
}
aic # this is just a test of the code

#########################################

aic_A_TotTraffic<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(tsstationary_A_TotTraffic,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic_A_TotTraffic[p+1,q+1]<-aic.p.q
  }
}
aic_A_TotTraffic

####

aic_B_TotTraffic<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(tsstationary_B_TotTraffic,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic_B_TotTraffic[p+1,q+1]<-aic.p.q
  }
}
aic_B_TotTraffic

####

aic_C_TotTraffic<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(tsstationary_C_TotTraffic,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic_C_TotTraffic[p+1,q+1]<-aic.p.q
  }
}
aic_C_TotTraffic

####

aic_D_TotTraffic<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(tsstationary_D_TotTraffic,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic_D_TotTraffic[p+1,q+1]<-aic.p.q
  }
}
aic_D_TotTraffic


write.csv(aic_A_TotTraffic,"results/AIC/x_TotTraffic/AIC_A_TotTraffic.csv")
write.csv(aic_B_TotTraffic,"results/AIC/x_TotTraffic/AIC_B_TotTraffic.csv")
write.csv(aic_C_TotTraffic,"results/AIC/x_TotTraffic/AIC_C_TotTraffic.csv")
write.csv(aic_D_TotTraffic,"results/AIC/x_TotTraffic/AIC_D_TotTraffic.csv")

rm(a.p.q, aic,aic.p.q,p,q)
