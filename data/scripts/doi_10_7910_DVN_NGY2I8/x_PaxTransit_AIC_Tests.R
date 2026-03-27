# Complementary Method: Test mutiple Arima Models and chose the one with the lowest AIC

aic<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(ts_A_PaxTransit,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic[p+1,q+1]<-aic.p.q
  }
}
aic # this is just a test of the code

#########################################

aic_A_PaxTransit<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(tsstationary_A_PaxTransit,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic_A_PaxTransit[p+1,q+1]<-aic.p.q
  }
}
aic_A_PaxTransit

####

aic_B_PaxTransit<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(tsstationary_B_PaxTransit,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic_B_PaxTransit[p+1,q+1]<-aic.p.q
  }
}
aic_B_PaxTransit

####

aic_C_PaxTransit<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(tsstationary_C_PaxTransit,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic_C_PaxTransit[p+1,q+1]<-aic.p.q
  }
}
aic_C_PaxTransit

####

aic_D_PaxTransit<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(tsstationary_D_PaxTransit,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic_D_PaxTransit[p+1,q+1]<-aic.p.q
  }
}
aic_D_PaxTransit


write.csv(aic_A_PaxTransit,"results/AIC/x_PaxTransit/AIC_A_PaxTransit.csv")
write.csv(aic_B_PaxTransit,"results/AIC/x_PaxTransit/AIC_B_PaxTransit.csv")
write.csv(aic_C_PaxTransit,"results/AIC/x_PaxTransit/AIC_C_PaxTransit.csv")
write.csv(aic_D_PaxTransit,"results/AIC/x_PaxTransit/AIC_D_PaxTransit.csv")

rm(a.p.q, aic,aic.p.q,p,q)
