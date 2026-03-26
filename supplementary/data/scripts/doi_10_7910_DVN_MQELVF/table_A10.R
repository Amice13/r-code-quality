setwd(pathTab)

model.Names <- c("Full Sample","Matched Sample","Full Sample","Matched Sample","Full Sample","Matched Sample")

coef.Names <- c("Difference$t_{0}-t_{-1}$","Number of troops in country$_{100,000}$","Number of troops in grid$_{10,000}$","Years after peacekeepers left grid","Duration of peacekeepers in grid","Casualties in grid$_{1000}$","Spatial Lag number of troops in grid","Sq. duration of peacekeepers in grid","Redeployment period","Peace time in grid","Peace time in country")


texreg(list(model.fe.1meanNoCalib.lr,model.fe.2meanNoCalib.lr,model.fe.1mean.lr,model.fe.2mean.lr,model.fe.1max.lr,model.fe.2max.lr),use.packages=FALSE,dcolumn=TRUE,custom.coef.names=coef.Names,custom.model.names=model.Names,stars = c(0.01, 0.05, 0.1),digits=4,scalebox=0.7,caption="Long-run effects. Fixed effects models with full and matched samples. Outcome variable: Nightlight emissions. Unit of analysis is a grid-year. All models include year and grid fixed effects as well as lagged dependent variable ",label="table:coefficientsAllLR",caption.above=TRUE,file="table_A10.tex")


