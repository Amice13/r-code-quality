# Complementary Method: Test mutiple Arima Models and chose the one with the lowest AIC

aic<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(ts_A_OtherCommercTraffic,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic[p+1,q+1]<-aic.p.q
  }
}
aic # this is just a test of the code

#########################################

aic_A_OtherCommercTraffic<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(tsstationary_A_OtherCommercTraffic,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic_A_OtherCommercTraffic[p+1,q+1]<-aic.p.q
  }
}
aic_A_OtherCommercTraffic

####

# aic_B_OtherCommercTraffic<-matrix(NA,6,6)
# for(p in 0:5)
# {
#   for(q in 0:5)
#   {
#     a.p.q<-arima(tsstationary_B_OtherCommercTraffic,order=c(p,1,q), method = "ML")
#     aic.p.q<-a.p.q$aic
#     aic_B_OtherCommercTraffic[p+1,q+1]<-aic.p.q
#   }
# }
# aic_B_OtherCommercTraffic

####

aic_C_OtherCommercTraffic<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(tsstationary_C_OtherCommercTraffic,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic_C_OtherCommercTraffic[p+1,q+1]<-aic.p.q
  }
}
aic_C_OtherCommercTraffic

####

aic_D_OtherCommercTraffic<-matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q<-arima(tsstationary_D_OtherCommercTraffic,order=c(p,1,q), method = "ML")
    aic.p.q<-a.p.q$aic
    aic_D_OtherCommercTraffic[p+1,q+1]<-aic.p.q
  }
}
aic_D_OtherCommercTraffic


write.csv(aic_A_OtherCommercTraffic,"results/AIC/x_OtherCommercTraffic/AIC_A_OtherCommercTraffic.csv")
# write.csv(aic_B_OtherCommercTraffic,"results/AIC/x_OtherCommercTraffic/AIC_B_OtherCommercTraffic.csv")
write.csv(aic_C_OtherCommercTraffic,"results/AIC/x_OtherCommercTraffic/AIC_C_OtherCommercTraffic.csv")
write.csv(aic_D_OtherCommercTraffic,"results/AIC/x_OtherCommercTraffic/AIC_D_OtherCommercTraffic.csv")

rm(a.p.q, aic,aic.p.q,p,q)
