setwd(pathTab)

model.Names <- c("Full Sample","Matched Sample","Full Sample","Matched Sample","Full Sample","Matched Sample")


coef.Names <- c("DV$_{lag}$","Number of troops in country$_{100,000}$","Number of troops in grid$_{10,000}$","Years after peacekeepers left grid","Duration of peacekeepers in grid","Casualties in grid$_{1000}$","Spatial Lag number of troops in grid","Sq. duration of peacekeepers in grid","Redeployment period","Peace time in grid","Peace time in country","Sq. years after peacekeepers left grid","Cube. years after peacekeepers left grid")


texreg(list(model.fe.1meanNoCalib.after.sq,model.fe.2meanNoCalib.after.sq,model.fe.1mean.after.sq,model.fe.2mean.after.sq,model.fe.1max.after.sq,model.fe.2max.after.sq),use.packages=FALSE,dcolumn=TRUE,custom.coef.names=coef.Names,custom.model.names=model.Names,stars = c(0.01, 0.05, 0.1),digits=4,scalebox=0.7,caption="Fixed effects models with full and matched samples. Outcome variable: Nightlight emissions. Unit of analysis is a grid-year. All models include year and grid fixed effects as well as lagged dependent variable ",label="table:coefficientsAll.after.sq",caption.above=TRUE,file="table_A11.tex")

