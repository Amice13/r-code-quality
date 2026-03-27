#######			
table.data$lag.dv <- table.data$nlights_max.l1

model.fe.2max <- plm(nlights_max~lag.dv+No.troops.country+No.troops+best.ged+No.troops_sp
+redeploy.period+time.since.last.best.gid+time.since.last.best.country+afterUN.period.spell+duringUN.period.spell+I(duringUN.period.spell^2),data=table.data[mat2$matched==TRUE,],index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.2max)

model.fe <- model.fe.2max
source('graphmatchedMax.R', chdir = TRUE)

model.fe.2maxNP <- plm(nlights_max~lag.dv+No.troops.country+best.ged+No.troops_sp
+redeploy.period+time.since.last.best.gid+time.since.last.best.country+afterUN.period.spell+duringUN.period.spell+I(duringUN.period.spell^2),data=table.data[mat2$matched==TRUE,],index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.2maxNP)			
			
setwd(pathR)
model.fe <- model.fe.2maxNP
source('graphmatchedMaxNT.R', chdir = TRUE)		
			
			

#######	
table.data$lag.dv <- table.data$nlights_calib_mean.l1_orig

model.fe.2mean <- plm(nlights_calib_mean_orig~lag.dv+No.troops.country+No.troops+best.ged+No.troops_sp +redeploy.period+time.since.last.best.gid+time.since.last.best.country+afterUN.period.spell+duringUN.period.spell+I(duringUN.period.spell^2)  
,data=table.data[mat2$matched==TRUE,],index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.2mean)

model.fe <- model.fe.2mean
setwd(pathR)
source('graphmatchedCMean.R', chdir = TRUE)

model.fe.2meanNP <- plm(nlights_calib_mean_orig~lag.dv+No.troops.country+best.ged+No.troops_sp+redeploy.period+time.since.last.best.gid+time.since.last.best.country +afterUN.period.spell+duringUN.period.spell+I(duringUN.period.spell^2) 
,data=table.data[mat2$matched==TRUE,],index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.2meanNP)
			
model.fe <- model.fe.2meanNP
setwd(pathR)
source('graphmatchedCMeanNT.R', chdir = TRUE)		
			
			
			
#######	
table.data$lag.dv <- table.data$nlights_mean.l1

model.fe.2meanNoCalib <- plm(nlights_mean~lag.dv+No.troops.country+No.troops+best.ged+No.troops_sp 
+redeploy.period+time.since.last.best.gid+time.since.last.best.country+afterUN.period.spell+duringUN.period.spell+I(duringUN.period.spell^2),data=table.data[mat2$matched==TRUE,],index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.2meanNoCalib)

model.fe <- model.fe.2meanNoCalib
setwd(pathR)
source('graphmatchedNoCalibMean.R', chdir = TRUE)

model.fe.2meanNoCalibNP <- plm(nlights_mean~lag.dv+No.troops.country+best.ged+No.troops_sp 
+redeploy.period+time.since.last.best.gid+time.since.last.best.country+afterUN.period.spell+duringUN.period.spell+I(duringUN.period.spell^2),data=table.data[mat2$matched==TRUE,],index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.2meanNoCalibNP)
			
model.fe <- model.fe.2meanNoCalibNP
setwd(pathR)
source('graphmatchedNoCalibMeanNT.R', chdir = TRUE)
		
			
















