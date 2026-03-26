#Matched during
matched.table.data.dur$lag.dv <- matched.table.data.dur$nlights_max.l1
model.fe.2max.dur <- plm(nlights_max~lag.dv+No.troops.country+No.troops+best.ged+No.troops_sp 
+redeploy.period+time.since.last.best.gid+time.since.last.best.country+duringUN.period.spell+I(No.troops*duringUN.period.spell)
,data=matched.table.data.dur,index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.2max.dur)

model.fe <- model.fe.2max.dur
setwd(pathR)
source('graphmatchedMaxDur.R', chdir = TRUE)


matched.table.data.dur$lag.dv <- matched.table.data.dur$nlights_calib_mean.l1_orig
model.fe.2mean.dur <- plm(nlights_calib_mean_orig~lag.dv+No.troops.country+No.troops+best.ged+No.troops_sp
+redeploy.period+time.since.last.best.gid+time.since.last.best.country+duringUN.period.spell+I(No.troops*duringUN.period.spell)
,data=matched.table.data.dur,index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.2mean.dur)

model.fe <- model.fe.2mean.dur
setwd(pathR)
source('graphmatchedCMeanDur.R', chdir = TRUE)

matched.table.data.dur$lag.dv <- matched.table.data.dur$nlights_mean.l1
model.fe.2meanNoCalib.dur <- plm(nlights_mean~lag.dv+No.troops.country+No.troops+best.ged+No.troops_sp
+redeploy.period+time.since.last.best.gid+time.since.last.best.country+duringUN.period.spell+I(No.troops*duringUN.period.spell)
,data=matched.table.data.dur,index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.2meanNoCalib.dur)

model.fe <- model.fe.2meanNoCalib.dur
setwd(pathR)
source('graphmatchedNoCalibMeanDur.R', chdir = TRUE)



#Matched after
matched.table.data.after$lag.dv <- matched.table.data.after$nlights_max.l1
model.fe.2max.after <- plm(nlights_max~lag.dv+No.troops.country+afterUN.period.spell+best.ged+No.troops_sp 
+time.since.last.best.gid+time.since.last.best.country+I(max.No.troops*afterUN.period.spell),data=matched.table.data.after,index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.2max.after)


model.fe <- model.fe.2max.after
setwd(pathR)
source('graphmatchedMaxAfter.R', chdir = TRUE)

matched.table.data.after$lag.dv <- matched.table.data.after$nlights_calib_mean.l1_orig
model.fe.2mean.after <- plm(nlights_calib_mean_orig~lag.dv+No.troops.country+afterUN.period.spell+best.ged+No.troops_sp
+time.since.last.best.gid+time.since.last.best.country+I(max.No.troops*afterUN.period.spell),data=matched.table.data.after,index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.2mean.after)

model.fe <- model.fe.2mean.after
setwd(pathR)
source('graphmatchedCMeanAfter.R', chdir = TRUE)

matched.table.data.after$lag.dv <- matched.table.data.after$nlights_mean.l1
model.fe.2meanNoCalib.after <- plm(nlights_mean~lag.dv+No.troops.country+afterUN.period.spell+best.ged+No.troops_sp 
+time.since.last.best.gid+time.since.last.best.country+I(max.No.troops*afterUN.period.spell),data=matched.table.data.after,index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.2meanNoCalib.after)

model.fe <- model.fe.2meanNoCalib.after
setwd(pathR)
source('graphmatchedNoCalibMeanAfter.R', chdir = TRUE)














