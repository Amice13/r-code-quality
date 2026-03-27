setwd(pathData)

load("unGridAll.rda")

un.data.final <- un.data.final%>%
					dplyr::group_by(gid) %>%
						dplyr::mutate(max.UN.duration = max(duringUN.period))

un.data.final <- un.data.final%>%
					dplyr::group_by(gid) %>%
						dplyr::mutate(max.redeploy.period = max(redeploy.period))

un.data.final <- un.data.final%>%
					dplyr::group_by(gid) %>%
						dplyr::mutate(max.No.troops = max(No.troops))



###########################
# Matched Model estimation
###########################


model.data <- model.frame(un.yes~max.UN.duration++capdist+nlights_max+nlights_max.l1+nlights_mean+nlights_mean.l1+nlights_calib_mean_orig+nlights_calib_mean.l1_orig+pop_gpw_sum+best.ged.beforeUN+ttime_mean+best.ged+light.1994+No.troops_sp+No.troops.country+No.troops+afterUN.period.spell+duringUN.period.spell+year+country_name+gid+MissionID+redeploy.period+max.redeploy.period+time.since.last.best.gid+time.since.last.best.country+max.No.troops,data=un.data.final[un.data.final$year<=2012 & un.data.final$year>1994,])


model.data <- model.data %>%ungroup()


## Matching for the duration models

#model.data$un.yes <- 0
#model.data$un.yes[model.data$duringUN.period.spell>0] <- 1

mat <- cem(treatment= "un.yes", data =model.data[model.data$duringUN.period.spell==0 & model.data$afterUN.period.spell==0|model.data$duringUN.period.spell==1 & model.data$afterUN.period.spell==0,] , drop =c("nlights_max","nlights_max.l1","nlights_mean","nlights_mean.l1","nlights_calib_mean_orig","nlights_calib_mean.l1_orig","afterUN.period.spell","max.UN.duration","No.troops_sp","duringUN.period.spell","No.troops.country","No.troops","year","country_name","gid","MissionID","best.ged","redeploy.period","max.redeploy.period","time.since.last.best.gid","time.since.last.best.country","max.No.troops"),keep.all=TRUE)
	table(mat$matched)
		mat2 <- k2k(mat, model.data , "euclidean", 1) 	
			table(mat2$matched)	
			
data.match.dur <- model.data[model.data$duringUN.period.spell==0 & model.data$afterUN.period.spell==0|model.data$duringUN.period.spell==1 & model.data$afterUN.period.spell==0,]
data.matched.dur <- data.match.dur[mat2$matched==TRUE,]
gid.match.dur <- data.match.dur$gid[mat2$matched==TRUE]


## Matching for the after models
#model.data$un.yes <- 0
#model.data$un.yes[model.data$afterUN.period.spell>0] <- 1


mat <- cem(treatment= "un.yes", data =model.data[model.data$afterUN.period.spell==1|model.data$duringUN.period.spell==0 & model.data$afterUN.period.spell==0 & model.data$duringUN.period.spell==0,] , drop =c("nlights_max","nlights_max.l1","nlights_mean","nlights_mean.l1","nlights_calib_mean_orig","nlights_calib_mean.l1_orig","afterUN.period.spell","max.UN.duration","No.troops_sp","duringUN.period.spell","No.troops.country", "count_project","count_project.l1","No.troops","country_name","year","gid","MissionID","best.ged","redeploy.period","max.redeploy.period","time.since.last.best.gid","time.since.last.best.country","max.No.troops"),keep.all=TRUE) #matching in year as well
	table(mat$matched)
		mat2 <- k2k(mat, model.data , "euclidean", 1) 	
			table(mat2$matched)	
			
data.match.after <- model.data[model.data$afterUN.period.spell==1|model.data$duringUN.period.spell==0 & model.data$afterUN.period.spell==0 & model.data$duringUN.period.spell==0,]
data.matched.after <- data.match.after[mat2$matched==TRUE,]
gid.match.after <- data.match.after$gid[mat2$matched==TRUE]



table.data <- model.data
table.data$No.troops.country <- table.data$No.troops.country/100000
table.data$No.troops <- table.data$No.troops/10000
table.data$best.ged <- table.data$best.ged/1000
matched.table.data.dur <- table.data[which(table.data$gid %in% gid.match.dur),]
	matched.table.data.dur <- matched.table.data.dur[matched.table.data.dur$afterUN.period.spell==0,]
	


table.data <- model.data
table.data$No.troops.country <- table.data$No.troops.country/100000
table.data$No.troops <- table.data$No.troops/10000
table.data$best.ged <- table.data$best.ged/1000
matched.table.data.after <- table.data[which(table.data$gid %in% gid.match.after),]
	matched.table.data.after <- matched.table.data.after[matched.table.data.after$duringUN.period.spell==0,]
	
	
	
	
	
