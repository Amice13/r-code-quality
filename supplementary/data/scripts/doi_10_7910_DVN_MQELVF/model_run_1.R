setwd(pathData)
load("unGridAll.rda")


#Model Frame


model.data <- model.frame(un.yes~capdist+nlights_max+nlights_max.l1+nlights_mean+nlights_mean.l1+nlights_calib_mean_orig+nlights_calib_mean.l1_orig+pop_gpw_sum+best.ged.beforeUN+ttime_mean+best.ged+light.1994+No.troops_sp+No.troops.country+No.troops+afterUN.period.spell+duringUN.period.spell+year+gid++redeploy.period+time.since.last.best.gid+time.since.last.best.country,data=un.data.final[un.data.final$year<=2012 & un.data.final$year>1994,])


model.data <- model.data %>%ungroup()


mat <- cem(treatment= "un.yes", data =model.data , drop =c("nlights_sd","nlights_sd.l1","nlights_max_st","nlights_max_st.l1","nlights_max","nlights_max.l1","nlights_mean","nlights_mean.l1","nlights_calib_mean_orig","nlights_calib_mean.l1_orig","afterUN.period.spell","No.troops_sp","duringUN.period.spell","No.troops.country", "No.troops","year","country_name","gid","MissionID","best.ged","redeploy.period","time.since.last.best.gid","time.since.last.best.country"),keep.all=TRUE)
	table(mat$matched)
		mat2 <- k2k(mat, model.data , "euclidean", 1) 	
			table(mat2$matched)	


temp.data <- model.data[mat2$matched==TRUE,]
save(model.data,file="modelDatav7.rda")
save(temp.data,file="modelDataMatchedv7.rda")


table.data <- model.data
	table.data$No.troops.country <- table.data$No.troops.country/100000
		table.data$No.troops <- table.data$No.troops/10000
			table.data$best.ged <- table.data$best.ged/1000
				table.data$troops.present <- ifelse(table.data$No.troops>0,1,0)
					table.data$troops.withdraw <- ifelse(table.data$afterUN.period.spell>0,1,0)


######################################################	
#Unmatched
######################################################	

# nlights_max ###############################
table.data$lag.dv <- table.data$nlights_max.l1

model.fe.1max <- plm(nlights_max~lag.dv+No.troops.country+best.ged+No.troops_sp
+redeploy.period+time.since.last.best.gid+time.since.last.best.country+afterUN.period.spell+duringUN.period.spell+I(duringUN.period.spell^2)+No.troops,data=table.data,index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.1max)

model.fe.1maxNP <- plm(nlights_max~lag.dv+No.troops.country+best.ged+No.troops_sp
+redeploy.period+time.since.last.best.gid+time.since.last.best.country+afterUN.period.spell+duringUN.period.spell+I(duringUN.period.spell^2),data=table.data,index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.1maxNP)

model.fe.1max.present <- plm(nlights_max~lag.dv+No.troops.country+best.ged+No.troops_sp
+redeploy.period+time.since.last.best.gid+time.since.last.best.country+troops.present+troops.withdraw,data=table.data,index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.1max.present)

model.fe.1max.after.sq <- plm(nlights_max~lag.dv+No.troops.country+No.troops+afterUN.period.spell+duringUN.period.spell+best.ged+No.troops_sp +I(duringUN.period.spell^2)
+redeploy.period+time.since.last.best.gid+time.since.last.best.country+I(afterUN.period.spell^2)+I(afterUN.period.spell^3),data=table.data,index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.1max.after.sq)

table.data$diff <- table.data$nlights_max-table.data$lag.dv

model.fe.1max.lr <- plm(nlights_max~ diff+No.troops.country+No.troops+afterUN.period.spell+duringUN.period.spell+best.ged+No.troops_sp +I(duringUN.period.spell^2) +redeploy.period+time.since.last.best.gid+time.since.last.best.country | lag.dv+No.troops.country+No.troops+afterUN.period.spell+duringUN.period.spell+best.ged+No.troops_sp +I(duringUN.period.spell^2)
+redeploy.period+time.since.last.best.gid+time.since.last.best.country,data=table.data,index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.1max.lr)


# nlights_calib_mean_orig ###############################
table.data$lag.dv <- table.data$nlights_calib_mean.l1_orig

model.fe.1mean <- plm(nlights_calib_mean_orig~lag.dv+No.troops.country+best.ged+No.troops_sp
+redeploy.period+time.since.last.best.gid+time.since.last.best.country+afterUN.period.spell+duringUN.period.spell+I(duringUN.period.spell^2)+No.troops,data=table.data,index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.1mean)

model.fe.1meanNP <- plm(nlights_calib_mean_orig~lag.dv+No.troops.country+best.ged+No.troops_sp
+redeploy.period+time.since.last.best.gid+time.since.last.best.country+afterUN.period.spell+duringUN.period.spell+I(duringUN.period.spell^2),data=table.data,index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.1meanNP)

model.fe.1mean.present <- plm(nlights_calib_mean_orig~lag.dv+No.troops.country+best.ged+No.troops_sp
+redeploy.period+time.since.last.best.gid+time.since.last.best.country+troops.present+troops.withdraw,data=table.data,index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.1mean.present)

model.fe.1mean.after.sq <- plm(nlights_calib_mean_orig~lag.dv+No.troops.country+No.troops+afterUN.period.spell+duringUN.period.spell+best.ged+No.troops_sp +I(duringUN.period.spell^2)
+redeploy.period+time.since.last.best.gid+time.since.last.best.country+I(afterUN.period.spell^2)+I(afterUN.period.spell^3),data=table.data,index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.1mean.after.sq)

table.data$diff <- table.data$nlights_calib_mean_orig-table.data$lag.dv

model.fe.1mean.lr <- plm(nlights_calib_mean_orig~ diff+No.troops.country+No.troops+afterUN.period.spell+duringUN.period.spell+best.ged+No.troops_sp +I(duringUN.period.spell^2) +redeploy.period+time.since.last.best.gid+time.since.last.best.country | lag.dv+No.troops.country+No.troops+afterUN.period.spell+duringUN.period.spell+best.ged+No.troops_sp +I(duringUN.period.spell^2)
+redeploy.period+time.since.last.best.gid+time.since.last.best.country,data=table.data,index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.1mean.lr)

#nlights_mean ###############################
table.data$lag.dv <- table.data$nlights_mean.l1

model.fe.1meanNoCalib <- plm(nlights_mean~lag.dv+No.troops.country+best.ged+No.troops_sp
+redeploy.period+time.since.last.best.gid+time.since.last.best.country+afterUN.period.spell+duringUN.period.spell+I(duringUN.period.spell^2)+No.troops,data=table.data,index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.1meanNoCalib)

model.fe.1meanNoCalibNP <- plm(nlights_mean~lag.dv+No.troops.country+best.ged+No.troops_sp
+redeploy.period+time.since.last.best.gid+time.since.last.best.country+afterUN.period.spell+duringUN.period.spell+I(duringUN.period.spell^2),data=table.data,index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.1meanNoCalibNP)

model.fe.1meanNoCalib.present <- plm(nlights_mean~lag.dv+No.troops.country+best.ged+No.troops_sp
+redeploy.period+time.since.last.best.gid+time.since.last.best.country+troops.present+troops.withdraw,data=table.data,index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.1meanNoCalib.present)

model.fe.1meanNoCalib.after.sq <- plm(nlights_mean~lag.dv+No.troops.country+No.troops+afterUN.period.spell+duringUN.period.spell+best.ged+No.troops_sp +I(duringUN.period.spell^2)
+redeploy.period+time.since.last.best.gid+time.since.last.best.country+I(afterUN.period.spell^2)+I(afterUN.period.spell^3),data=table.data,index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.1meanNoCalib.after.sq)

table.data$diff <- table.data$nlights_mean-table.data$lag.dv

model.fe.1meanNoCalib.lr <- plm(nlights_mean~ diff+No.troops.country+No.troops+afterUN.period.spell+duringUN.period.spell+best.ged+No.troops_sp +I(duringUN.period.spell^2)+redeploy.period+time.since.last.best.gid+time.since.last.best.country | lag.dv+No.troops.country+No.troops+afterUN.period.spell+duringUN.period.spell+best.ged+No.troops_sp +I(duringUN.period.spell^2) +redeploy.period+time.since.last.best.gid+time.since.last.best.country
,data=table.data,index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.1meanNoCalib.lr)


######################################################
#Matched
######################################################	


#nlights_max ###############################
table.data$lag.dv <- table.data$nlights_max.l1

model.fe.2max <- plm(nlights_max~lag.dv+No.troops.country+best.ged+No.troops_sp
+redeploy.period+time.since.last.best.gid+time.since.last.best.country+afterUN.period.spell+duringUN.period.spell+I(duringUN.period.spell^2)+No.troops,data=table.data[mat2$matched==TRUE,],index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.2max)

model.fe.2maxNP <- plm(nlights_max~lag.dv+No.troops.country+best.ged+No.troops_sp
+redeploy.period+time.since.last.best.gid+time.since.last.best.country+afterUN.period.spell+duringUN.period.spell+I(duringUN.period.spell^2),data=table.data[mat2$matched==TRUE,],index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.2maxNP)

model.fe.2max.present <- plm(nlights_max~lag.dv+No.troops.country+best.ged+No.troops_sp
+redeploy.period+time.since.last.best.gid+time.since.last.best.country+troops.present+troops.withdraw,data=table.data[mat2$matched==TRUE,],index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.2max.present)

model.fe.2max.after.sq <- plm(nlights_max~lag.dv+No.troops.country+No.troops+afterUN.period.spell+duringUN.period.spell+best.ged+No.troops_sp +I(duringUN.period.spell^2)
+redeploy.period+time.since.last.best.gid+time.since.last.best.country+I(afterUN.period.spell^2)+I(afterUN.period.spell^3),data=table.data[mat2$matched==TRUE,],index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.2max.after.sq)

model.fe.2max.int <- plm(nlights_max~lag.dv+No.troops.country+No.troops+afterUN.period.spell+best.ged+No.troops_sp 
+redeploy.period+time.since.last.best.gid+time.since.last.best.country+duringUN.period.spell+I(duringUN.period.spell^2)+I(No.troops*duringUN.period.spell)+I(No.troops*duringUN.period.spell^2),data=table.data[mat2$matched==TRUE,],index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.2max.int)

# Robustness: Interaction with deployment period
#nlights_max

model.fe.2max.deploy <- plm(nlights_max~lag.dv+No.troops.country+No.troops+afterUN.period.spell+duringUN.period.spell+best.ged+No.troops_sp +I(duringUN.period.spell^2)
+redeploy.period+time.since.last.best.gid+time.since.last.best.country+I(redeploy.period*No.troops)+I(redeploy.period*afterUN.period.spell)+I(redeploy.period*duringUN.period.spell)++I(redeploy.period*(duringUN.period.spell^2)),data=table.data[mat2$matched==TRUE,],index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.2max.deploy)

table.data$diff <- table.data$nlights_max-table.data$lag.dv

model.fe.2max.lr <- plm(nlights_max~ diff+No.troops.country+No.troops+afterUN.period.spell+duringUN.period.spell+best.ged+No.troops_sp +I(duringUN.period.spell^2) +redeploy.period+time.since.last.best.gid+time.since.last.best.country| lag.dv+No.troops.country+No.troops+afterUN.period.spell+duringUN.period.spell+best.ged+No.troops_sp +I(duringUN.period.spell^2)
+redeploy.period+time.since.last.best.gid+time.since.last.best.country,data=table.data[mat2$matched==TRUE,],index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.2max.lr)

# nlights_calib_mean_orig ###############################
table.data$lag.dv <- table.data$nlights_calib_mean.l1_orig

model.fe.2mean <- plm(nlights_calib_mean_orig~lag.dv+No.troops.country+best.ged+No.troops_sp +redeploy.period+time.since.last.best.gid+time.since.last.best.country+afterUN.period.spell+duringUN.period.spell+I(duringUN.period.spell^2) +No.troops 
,data=table.data[mat2$matched==TRUE,],index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.2mean)

model.fe.2meanNP <- plm(nlights_calib_mean_orig~lag.dv+No.troops.country+best.ged+No.troops_sp+redeploy.period+time.since.last.best.gid+time.since.last.best.country +afterUN.period.spell+duringUN.period.spell+I(duringUN.period.spell^2) 
,data=table.data[mat2$matched==TRUE,],index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.2meanNP)

model.fe.2mean.present <- plm(nlights_calib_mean_orig~lag.dv+No.troops.country+best.ged+No.troops_sp+redeploy.period+time.since.last.best.gid+time.since.last.best.country +troops.present+troops.withdraw
,data=table.data[mat2$matched==TRUE,],index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.2mean.present)

model.fe.2mean.after.sq <- plm(nlights_calib_mean_orig~lag.dv+No.troops.country+No.troops+afterUN.period.spell+duringUN.period.spell+best.ged+No.troops_sp +I(duringUN.period.spell^2) +redeploy.period+time.since.last.best.gid+time.since.last.best.country +I(afterUN.period.spell^2)+I(afterUN.period.spell^3)
,data=table.data[mat2$matched==TRUE,],index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.2mean.after.sq)

model.fe.2mean.int <- plm(nlights_calib_mean_orig~lag.dv+No.troops.country+No.troops+afterUN.period.spell+best.ged+No.troops_sp 
+redeploy.period+time.since.last.best.gid+time.since.last.best.country+duringUN.period.spell+I(duringUN.period.spell^2)+I(No.troops*duringUN.period.spell)+I(No.troops*duringUN.period.spell^2),data=table.data[mat2$matched==TRUE,],index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.2mean.int)

table.data$diff <- table.data$nlights_calib_mean_orig-table.data$lag.dv

model.fe.2mean.lr <- plm(nlights_calib_mean_orig~ diff+No.troops.country+No.troops+afterUN.period.spell+duringUN.period.spell+best.ged+No.troops_sp +I(duringUN.period.spell^2) +redeploy.period+time.since.last.best.gid+time.since.last.best.country | lag.dv+No.troops.country+No.troops+afterUN.period.spell+duringUN.period.spell+best.ged+No.troops_sp +I(duringUN.period.spell^2)
+redeploy.period+time.since.last.best.gid+time.since.last.best.country,data=table.data[mat2$matched==TRUE,],index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.2mean.lr)

#nlights_mean ###############################
table.data$lag.dv <- table.data$nlights_mean.l1

model.fe.2meanNoCalib <- plm(nlights_mean~lag.dv+No.troops.country+best.ged+No.troops_sp 
+redeploy.period+time.since.last.best.gid+time.since.last.best.country+afterUN.period.spell+duringUN.period.spell+I(duringUN.period.spell^2)+No.troops,data=table.data[mat2$matched==TRUE,],index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.2meanNoCalib)

model.fe.2meanNoCalibNP <- plm(nlights_mean~lag.dv+No.troops.country+best.ged+No.troops_sp 
+redeploy.period+time.since.last.best.gid+time.since.last.best.country+afterUN.period.spell+duringUN.period.spell+I(duringUN.period.spell^2),data=table.data[mat2$matched==TRUE,],index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.2meanNoCalibNP)

model.fe.2meanNoCalib.present <- plm(nlights_mean~lag.dv+No.troops.country+best.ged+No.troops_sp
+redeploy.period+time.since.last.best.gid+time.since.last.best.country+troops.present+troops.withdraw,data=table.data[mat2$matched==TRUE,],index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.2meanNoCalib.present)

model.fe.2meanNoCalib.after.sq <- plm(nlights_mean~lag.dv+No.troops.country+No.troops+afterUN.period.spell+duringUN.period.spell+best.ged+No.troops_sp +I(duringUN.period.spell^2)
+redeploy.period+time.since.last.best.gid+time.since.last.best.country+I(afterUN.period.spell^2)+I(afterUN.period.spell^3),data=table.data[mat2$matched==TRUE,],index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.2meanNoCalib.after.sq)

model.fe.2meanNoCalib.int <- plm(nlights_mean~lag.dv+No.troops.country+No.troops+afterUN.period.spell+best.ged+No.troops_sp
+redeploy.period+time.since.last.best.gid+time.since.last.best.country+duringUN.period.spell+I(duringUN.period.spell^2)+I(No.troops*duringUN.period.spell)+I(No.troops*duringUN.period.spell^2),data=table.data[mat2$matched==TRUE,],index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.2meanNoCalib.int)

table.data$diff <- table.data$nlights_mean-table.data$lag.dv

model.fe.2meanNoCalib.lr <- plm(nlights_mean~ diff+No.troops.country+No.troops+afterUN.period.spell+duringUN.period.spell+best.ged+No.troops_sp +I(duringUN.period.spell^2)+redeploy.period+time.since.last.best.gid+time.since.last.best.country | lag.dv+No.troops.country+No.troops+afterUN.period.spell+duringUN.period.spell+best.ged+No.troops_sp +I(duringUN.period.spell^2) +redeploy.period+time.since.last.best.gid+time.since.last.best.country
,data=table.data[mat2$matched==TRUE,],index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.2meanNoCalib.lr)




