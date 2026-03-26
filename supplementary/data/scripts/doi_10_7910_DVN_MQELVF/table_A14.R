setwd(pathTab)

coef.Names <- c("DV$_{lag}$","Number of troops in country$_{100,000}$","Number of troops in grid$_{10,000}$","Years after peacekeepers left grid","Casualties in grid$_{1000}$","Spatial Lag number of troops in grid","Redeployment period","Peace time in grid","Peace time in country","Duration of peacekeepers in grid","Sq. duration of peacekeepers in grid","Number of troops in grid$_{10,000}$$\times$Duration of peacekeepers in grid","Number of troops in grid$_{10,000}$$\times$Sq. duration of peacekeepers in grid")

model.Names <- c("Uncalibrated Mean","Calibrated Mean","Uncalibrated Max")

texreg(list(model.fe.2meanNoCalib.int,model.fe.2mean.int,model.fe.2max.int),use.packages=FALSE,dcolumn=TRUE,custom.coef.names=coef.Names,custom.model.names=model.Names,stars = c(0.01, 0.05, 0.1),digits=4,scalebox=0.7,caption="Fixed effects models with matched samples. Outcome variable: Nightlight emissions. Unit of analysis is a grid-year. All models include year and grid fixed effects as well as lagged dependent variable. Models investigate the interaction between duration of peacekeepers in a grid and their size.",label="table:coefficientsIntDuration",caption.above=TRUE,file="table_A14.tex")




