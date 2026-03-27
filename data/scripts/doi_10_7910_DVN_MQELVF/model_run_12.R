setwd(pathData)
load("unGridAll.rda")


model.data <- model.frame(un.yes~capdist+nlights_max+nlights_max.l1+nlights_mean+nlights_mean.l1+nlights_calib_mean_orig+nlights_calib_mean.l1_orig+pop_gpw_sum+best.ged.beforeUN+ttime_mean+best.ged+light.1994+No.troops_sp+No.troops.country+No.troops+afterUN.period.spell+duringUN.period.spell+year+gid+redeploy.period+time.since.last.best.gid+time.since.last.best.country,data=un.data.final[un.data.final$year<=2012 & un.data.final$year>1994,])


model.data <- model.data %>%ungroup()


mat <- cem(treatment= "un.yes", data =model.data , drop =c("nlights_max","nlights_max.l1","nlights_mean","nlights_mean.l1","nlights_calib_mean_orig","nlights_calib_mean.l1_orig","afterUN.period.spell","No.troops_sp","duringUN.period.spell","No.troops.country", "No.troops","year","country_name","gid","MissionID","best.ged","redeploy.period","time.since.last.best.gid","time.since.last.best.country"),keep.all=TRUE)
	table(mat$matched)
		mat2 <- k2k(mat, model.data , "euclidean", 1) 	
			table(mat2$matched)	


model.fe.2max <- plm(nlights_max~nlights_max.l1+No.troops.country+No.troops+afterUN.period.spell+duringUN.period.spell+best.ged+No.troops_sp+redeploy.period+time.since.last.best.gid+time.since.last.best.country +I(duringUN.period.spell^2)
,data=model.data[mat2$matched==TRUE,],index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.2max)

model.fe <- model.fe.2max

