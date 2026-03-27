# Complementary Method: Test mutiple Arima Models and chose the one with the lowest AIC

aic<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(ts_A_PaxScheduledEurope,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic[p+1,q+1]<-aic.p.q
  }
}
aic # this is just a test of the code

#########################################

aic_A_PaxScheduledEurope<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(tsstationary_A_PaxScheduledEurope,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic_A_PaxScheduledEurope[p+1,q+1]<-aic.p.q
  }
}
aic_A_PaxScheduledEurope

####

aic_B_PaxScheduledEurope<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(tsstationary_B_PaxScheduledEurope,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic_B_PaxScheduledEurope[p+1,q+1]<-aic.p.q
  }
}
aic_B_PaxScheduledEurope

####

aic_C_PaxScheduledEurope<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(tsstationary_C_PaxScheduledEurope,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic_C_PaxScheduledEurope[p+1,q+1]<-aic.p.q
  }
}
aic_C_PaxScheduledEurope

####

aic_D_PaxScheduledEurope<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(tsstationary_D_PaxScheduledEurope,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic_D_PaxScheduledEurope[p+1,q+1]<-aic.p.q
  }
}
aic_D_PaxScheduledEurope


write.csv(aic_A_PaxScheduledEurope,"results/AIC/x_PaxScheduledEurope/AIC_A_PaxScheduledEurope.csv")
write.csv(aic_B_PaxScheduledEurope,"results/AIC/x_PaxScheduledEurope/AIC_B_PaxScheduledEurope.csv")
write.csv(aic_C_PaxScheduledEurope,"results/AIC/x_PaxScheduledEurope/AIC_C_PaxScheduledEurope.csv")
write.csv(aic_D_PaxScheduledEurope,"results/AIC/x_PaxScheduledEurope/AIC_D_PaxScheduledEurope.csv")

rm(a.p.q, aic,aic.p.q,p,q)
