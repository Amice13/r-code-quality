#during model - duration treatment 

#models with matching weights
matched.table.data.dur2$lag.dv = matched.table.data.dur2$nlights_mean.l1
model.matchduration.mean.dur = plm (nlights_mean ~ lag.dv+No.troops.country+No.troops+duringUN.period.spell+
                                    best.ged+No.troops_sp+redeploy.period+time.since.last.best.gid+time.since.last.best.country,
                                    data=matched.table.data.dur2,index=c("gid","year"),model="within",effect="twoways",weights = w)
summary(model.matchduration.mean.dur)

matched.table.data.dur2$lag.dv = matched.table.data.dur2$nlights_calib_mean.l1_orig
model.matchduration.calib.dur = plm (nlights_calib_mean_orig ~ lag.dv+No.troops.country+No.troops+duringUN.period.spell+
                                     best.ged+No.troops_sp+redeploy.period+time.since.last.best.gid+time.since.last.best.country,
                                     data=matched.table.data.dur2,index=c("gid","year"),model="within",effect="twoways",weights = w)
summary(model.matchduration.calib.dur)

matched.table.data.dur2$lag.dv = matched.table.data.dur2$nlights_max.l1
model.matchduration.max.dur = plm (nlights_max ~ lag.dv+No.troops.country+No.troops+duringUN.period.spell+best.ged+
                                   No.troops_sp+redeploy.period+time.since.last.best.gid+time.since.last.best.country,
                                   data=matched.table.data.dur2,index=c("gid","year"),model="within",effect="twoways",weights = w)
summary(model.matchduration.max.dur)


#tables

setwd(pathTab)

coef.Names.dur <- c("DV$_{lag}$","Number of troops in country$_{100,000}$","Number of troops in grid$_{10,000}$","Duration of peacekeepers in grid","Casualties in grid$_{1,000}$","Spatial Lag number of troops in grid","Redeployment period","Peace time in grid","Peace time in country")

model.Names <- c("Uncalibrated Mean","Calibrated Mean","Uncalibrated Max")

texreg(list(model.matchduration.mean.dur,model.matchduration.calib.dur,model.matchduration.max.dur),use.packages=FALSE,dcolumn=TRUE,custom.coef.names=coef.Names.dur,custom.model.names=model.Names,stars = c(0.01, 0.05, 0.1),digits=4,scalebox=0.7,caption="Fixed effects models with matched samples (Deployment duration treatment). Outcome variable: Nightlight emissions. Unit of analysis is a grid-year. All models include year and grid fixed effects as well as lagged dependent variable ",label="table:coefficientsSplitDurationDur",caption.above=TRUE,file="table_A16.tex")


