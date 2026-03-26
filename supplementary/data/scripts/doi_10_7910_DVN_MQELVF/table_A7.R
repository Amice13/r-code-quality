setwd(pathTab)

#All unmatched models including present, duration, count
model.Names <- c("Full Sample","Full Sample","Full Sample","Full Sample","Full Sample","Full Sample","Full Sample","Full Sample","Full Sample")


coef.Names <- c("DV$_{lag}$","Number of troops in country$_{100,000}$","Casualties in grid$_{1000}$","Spatial Lag number of troops in grid","Redeployment period","Peace time in grid","Peace time in country","Troops present","Troops withdrawn","Years after peacekeepers left grid","Duration of peacekeepers in grid","Sq. duration of peacekeepers in grid","Number of troops in grid$_{10,000}$")


texreg(list(model.fe.1meanNoCalib.present,model.fe.1mean.present,model.fe.1max.present,model.fe.1meanNoCalibNP,model.fe.1meanNP,model.fe.1maxNP,model.fe.1meanNoCalib,model.fe.1mean,model.fe.1max),use.packages=FALSE,dcolumn=TRUE,custom.coef.names=coef.Names,custom.model.names=model.Names,stars = c(0.01, 0.05, 0.1),digits=4,scalebox=0.7,caption="Fixed effects models with full and matched samples. Outcome variable: Nightlight emissions. Unit of analysis is a grid-year. All models include year and grid fixed effects as well as lagged dependent variable ",label="table:coefficientsAllinOneNoMatch",caption.above=TRUE,file="table_A7.tex")


