
setwd(pathTab)

model.Names <- c("Baseline Model","Redeployment Interaction")

coef.Names <- c("DV$_{lag}$","Number of troops in country$_{100,000}$","Years after peacekeepers left grid","Duration of peacekeepers in grid","Casualties in grid$_{1000}$","Spatial Lag number of troops in grid","Sq. duration of peacekeepers in grid","Redeployment period","Peace time in grid","Peace time in country","RP$\times$Number of troops in grid$_{10,000}","RP$\times$Years after peacekeepers left grid","RP$\times$Duration of peacekeepers in grid","RP$\times$Sq. duration of peacekeepers in grid","Number of troops in grid$_{10,000}$")


texreg(list(model.fe.2max,model.fe.2max.deploy),use.packages=FALSE,dcolumn=TRUE,custom.coef.names=coef.Names,custom.model.names=model.Names,stars = c(0.01, 0.05, 0.1),digits=4,scalebox=0.7,caption="Fixed effects models with matched samples. Outcome variable: Maximum nightlight emissions in PRIO-GRID. Unit of analysis is a grid-year. All models include year and grid fixed effects as well as lagged dependent variable. First model pertains to the baseline model, while the second model includes interactions with the deployment period of UN peacekeepers in a grid. This is to assess possible heterogenous treatment effects",label="table:coefficientsMaxDeploy",caption.above=TRUE,file="table_A13.tex")

