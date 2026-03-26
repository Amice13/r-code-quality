# Complementary Method: Test mutiple Arima Models and chose the one with the lowest AIC

aic<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(ts_A_PaxTerminalTot,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic[p+1,q+1]<-aic.p.q
  }
}
aic # this is just a test of the code

#########################################

aic_A_PaxTerminalTot<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(tsstationary_A_PaxTerminalTot,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic_A_PaxTerminalTot[p+1,q+1]<-aic.p.q
  }
}
aic_A_PaxTerminalTot

####

aic_B_PaxTerminalTot<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(tsstationary_B_PaxTerminalTot,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic_B_PaxTerminalTot[p+1,q+1]<-aic.p.q
  }
}
aic_B_PaxTerminalTot

####

aic_C_PaxTerminalTot<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(tsstationary_C_PaxTerminalTot,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic_C_PaxTerminalTot[p+1,q+1]<-aic.p.q
  }
}
aic_C_PaxTerminalTot

####

aic_D_PaxTerminalTot<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(tsstationary_D_PaxTerminalTot,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic_D_PaxTerminalTot[p+1,q+1]<-aic.p.q
  }
}
aic_D_PaxTerminalTot


write.csv(aic_A_PaxTerminalTot,"results/AIC/x_PaxTerminalTot/AIC_A_PaxTerminalTot.csv")
write.csv(aic_B_PaxTerminalTot,"results/AIC/x_PaxTerminalTot/AIC_B_PaxTerminalTot.csv")
write.csv(aic_C_PaxTerminalTot,"results/AIC/x_PaxTerminalTot/AIC_C_PaxTerminalTot.csv")
write.csv(aic_D_PaxTerminalTot,"results/AIC/x_PaxTerminalTot/AIC_D_PaxTerminalTot.csv")

rm(a.p.q, aic,aic.p.q,p,q)
