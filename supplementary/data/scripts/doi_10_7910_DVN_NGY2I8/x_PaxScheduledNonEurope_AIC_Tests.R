# Complementary Method: Test mutiple Arima Models and chose the one with the lowest AIC

aic<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(ts_A_PaxScheduledNonEurope,order=c(p,1,q))
    aic.p.q<-a.p.q$aic
    aic[p+1,q+1]<-aic.p.q
  }
}
aic # this is just a test of the code

#########################################

aic_A_PaxScheduledNonEurope<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(tsstationary_A_PaxScheduledNonEurope,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic_A_PaxScheduledNonEurope[p+1,q+1]<-aic.p.q
  }
}
aic_A_PaxScheduledNonEurope

####

aic_B_PaxScheduledNonEurope<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(tsstationary_B_PaxScheduledNonEurope,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic_B_PaxScheduledNonEurope[p+1,q+1]<-aic.p.q
  }
}
aic_B_PaxScheduledNonEurope

####

aic_C_PaxScheduledNonEurope<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(tsstationary_C_PaxScheduledNonEurope,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic_C_PaxScheduledNonEurope[p+1,q+1]<-aic.p.q
  }
}
aic_C_PaxScheduledNonEurope

####

aic_D_PaxScheduledNonEurope<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(tsstationary_D_PaxScheduledNonEurope,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic_D_PaxScheduledNonEurope[p+1,q+1]<-aic.p.q
  }
}
aic_D_PaxScheduledNonEurope


write.csv(aic_A_PaxScheduledNonEurope,"results/AIC/x_PaxScheduledNonEurope/AIC_A_PaxScheduledNonEurope.csv")
write.csv(aic_B_PaxScheduledNonEurope,"results/AIC/x_PaxScheduledNonEurope/AIC_B_PaxScheduledNonEurope.csv")
write.csv(aic_C_PaxScheduledNonEurope,"results/AIC/x_PaxScheduledNonEurope/AIC_C_PaxScheduledNonEurope.csv")
write.csv(aic_D_PaxScheduledNonEurope,"results/AIC/x_PaxScheduledNonEurope/AIC_D_PaxScheduledNonEurope.csv")

rm(a.p.q, aic,aic.p.q,p,q)
