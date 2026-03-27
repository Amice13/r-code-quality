
setwd(pathTab)
coef.Names.after <- c("DV$_{lag}$","Number of troops in country$_{100,000}$","Years after peacekeepers left grid","Casualties in grid$_{1,000}$","Spatial Lag number of troops in grid","Peace time in grid","Peace time in country","Max Number of troops in grid","Sq. years after peacekeepers left grid","Cube. years after peacekeepers left grid")

model.Names <- c("Uncalibrated Mean","Calibrated Mean","Uncalibrated Max")

texreg(list(model.fe.2meanNoCalib.after,model.fe.2mean.after,model.fe.2max.after),use.packages=FALSE,dcolumn=TRUE,custom.coef.names=coef.Names.after,custom.model.names=model.Names,stars = c(0.01, 0.05, 0.1),digits=4,scalebox=0.7,caption="Fixed effects models with full and matched samples. Outcome variable: Nightlight emissions. Unit of analysis is a grid-year. All models include year and grid fixed effects as well as lagged dependent variable ",label="table:coefficientsSplitAfter",caption.above=TRUE,file="table_A12.tex")














