setwd(pathTab)

coef.Names.dur <- c("DV$_{lag}$","Number of troops in country$_{100,000}$","Number of troops in grid$_{10,000}$","Duration of peacekeepers in grid","Casualties in grid$_{1,000}$","Spatial Lag number of troops in grid","Redeployment period","Peace time in grid","Peace time in country","Number of troops in grid$_{10,000} \times$ Duration of peacekeepers in grid")


model.Names <- c("Uncalibrated Mean","Calibrated Mean","Uncalibrated Max")

setwd(pathTab)
texreg(list(model.fe.2meanNoCalib.dur,model.fe.2mean.dur,model.fe.2max.dur),use.packages=FALSE,dcolumn=TRUE,custom.coef.names=coef.Names.dur,custom.model.names=model.Names,stars = c(0.01, 0.05, 0.1),digits=4,scalebox=0.7,caption="Fixed effects models with full and matched samples. Outcome variable: Nightlight emissions. Unit of analysis is a grid-year. All models include year and grid fixed effects as well as lagged dependent variable ",label="table:coefficientsSplitDur",caption.above=TRUE,file="table_A8.tex")








