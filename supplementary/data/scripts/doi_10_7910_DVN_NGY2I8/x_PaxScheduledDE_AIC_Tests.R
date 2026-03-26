# Complementary Method: Test mutiple Arima Models and chose the one with the lowest AIC

aic<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(ts_A_PaxScheduledDE,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic[p+1,q+1]<-aic.p.q
  }
}
aic # this is just a test of the code

#########################################

aic_A_PaxScheduledDE<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(tsstationary_A_PaxScheduledDE,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic_A_PaxScheduledDE[p+1,q+1]<-aic.p.q
  }
}
aic_A_PaxScheduledDE

####

aic_B_PaxScheduledDE<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(tsstationary_B_PaxScheduledDE,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic_B_PaxScheduledDE[p+1,q+1]<-aic.p.q
  }
}
aic_B_PaxScheduledDE

####

aic_C_PaxScheduledDE<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(tsstationary_C_PaxScheduledDE,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic_C_PaxScheduledDE[p+1,q+1]<-aic.p.q
  }
}
aic_C_PaxScheduledDE

####

aic_D_PaxScheduledDE<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(tsstationary_D_PaxScheduledDE,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic_D_PaxScheduledDE[p+1,q+1]<-aic.p.q
  }
}
aic_D_PaxScheduledDE


write.csv(aic_A_PaxScheduledDE,"results/AIC/x_PaxScheduledDE/AIC_A_PaxScheduledDE.csv")
write.csv(aic_B_PaxScheduledDE,"results/AIC/x_PaxScheduledDE/AIC_B_PaxScheduledDE.csv")
write.csv(aic_C_PaxScheduledDE,"results/AIC/x_PaxScheduledDE/AIC_C_PaxScheduledDE.csv")
write.csv(aic_D_PaxScheduledDE,"results/AIC/x_PaxScheduledDE/AIC_D_PaxScheduledDE.csv")

rm(a.p.q, aic,aic.p.q,p,q)
