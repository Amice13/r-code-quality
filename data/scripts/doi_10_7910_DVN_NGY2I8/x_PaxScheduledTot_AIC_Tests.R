# Complementary Method: Test mutiple Arima Models and chose the one with the lowest AIC

aic<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(ts_A_PaxScheduledTot,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic[p+1,q+1]<-aic.p.q
  }
}
aic # this is just a test of the code

#########################################

aic_A_PaxScheduledTot<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(tsstationary_A_PaxScheduledTot,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic_A_PaxScheduledTot[p+1,q+1]<-aic.p.q
  }
}
aic_A_PaxScheduledTot

####

aic_B_PaxScheduledTot<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(tsstationary_B_PaxScheduledTot,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic_B_PaxScheduledTot[p+1,q+1]<-aic.p.q
  }
}
aic_B_PaxScheduledTot

####

aic_C_PaxScheduledTot<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(tsstationary_C_PaxScheduledTot,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic_C_PaxScheduledTot[p+1,q+1]<-aic.p.q
  }
}
aic_C_PaxScheduledTot

####

aic_D_PaxScheduledTot<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(tsstationary_D_PaxScheduledTot,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic_D_PaxScheduledTot[p+1,q+1]<-aic.p.q
  }
}
aic_D_PaxScheduledTot


write.csv(aic_A_PaxScheduledTot,"results/AIC/x_PaxScheduledTot/AIC_A_PaxScheduledTot.csv")
write.csv(aic_B_PaxScheduledTot,"results/AIC/x_PaxScheduledTot/AIC_B_PaxScheduledTot.csv")
write.csv(aic_C_PaxScheduledTot,"results/AIC/x_PaxScheduledTot/AIC_C_PaxScheduledTot.csv")
write.csv(aic_D_PaxScheduledTot,"results/AIC/x_PaxScheduledTot/AIC_D_PaxScheduledTot.csv")

rm(a.p.q, aic,aic.p.q,p,q)
