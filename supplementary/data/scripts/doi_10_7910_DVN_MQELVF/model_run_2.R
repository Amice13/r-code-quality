setwd(pathData)

# data
load("unGridAll.rda")

# new vars ####

#max
un.data.final <- un.data.final%>%
  dplyr::group_by(gid) %>%
  dplyr::mutate(max.UN.duration = max(duringUN.period))

un.data.final <- un.data.final%>%
  dplyr::group_by(gid) %>%
  dplyr::mutate(max.redeploy.period = max(redeploy.period))

un.data.final <- un.data.final%>%
  dplyr::group_by(gid) %>%
  dplyr::mutate(max.No.troops = max(No.troops))


#size categorical treatment

un.data.final$deploymentsize.cat = case_when(un.data.final$No.troops ==  0 ~ 1,
                                          un.data.final$No.troops > 0 & un.data.final$No.troops < 500 ~ 2,
                                          un.data.final$No.troops >= 500 & un.data.final$No.troops < 1000 ~ 3,
                                          T ~ 4)

table(un.data.final$deploymentsize.cat)
un.data.final$deploymentsize.cat = as.factor(un.data.final$deploymentsize.cat)


#duration categorical

un.data.final$duringUN.period.spell.cat = case_when(un.data.final$duringUN.period.spell ==  0 ~ 1,
                                                 un.data.final$duringUN.period.spell > 0 & un.data.final$duringUN.period.spell <= 3 ~ 2,
                                                 un.data.final$duringUN.period.spell >= 4 & un.data.final$duringUN.period.spell <= 6 ~ 3,
                                                 T ~ 4)

table(un.data.final$duringUN.period.spell)
table(un.data.final$duringUN.period.spell.cat)
un.data.final$duringUN.period.spell.cat = as.factor(un.data.final$duringUN.period.spell.cat)


########################
#### size treatment ####
########################

#### during model ####

model.data <- model.frame(deploymentsize.cat~max.UN.duration++capdist+nlights_max+nlights_max.l1+nlights_mean+nlights_mean.l1+nlights_calib_mean_orig+nlights_calib_mean.l1_orig+pop_gpw_sum+best.ged.beforeUN+ttime_mean+best.ged+light.1994+No.troops_sp+No.troops.country+No.troops+afterUN.period.spell+duringUN.period.spell+year+country_name+gid+MissionID+redeploy.period+max.redeploy.period+time.since.last.best.gid+time.since.last.best.country+max.No.troops,data=un.data.final[un.data.final$year<=2012 & un.data.final$year>1994,])

#matching 

mat <- cem(treatment= "deploymentsize.cat", data =model.data[model.data$duringUN.period.spell==0 & model.data$afterUN.period.spell==0|model.data$duringUN.period.spell>0 & model.data$afterUN.period.spell==0,] , drop =c("nlights_max","nlights_max.l1","nlights_mean","nlights_mean.l1","nlights_calib_mean_orig","nlights_calib_mean.l1_orig","afterUN.period.spell","max.UN.duration","No.troops_sp","duringUN.period.spell","No.troops.country","No.troops","year","country_name","gid","MissionID","best.ged","redeploy.period","max.redeploy.period","time.since.last.best.gid","time.since.last.best.country","max.No.troops"),keep.all=TRUE)
table(mat$matched)
# mat2 <- k2k(mat, model.data , "euclidean", 1)
# table(mat2$matched)

data.match.dur <- model.data[model.data$duringUN.period.spell==0 & model.data$afterUN.period.spell==0|model.data$duringUN.period.spell>0 & model.data$afterUN.period.spell==0,]
data.matched.dur <- data.match.dur[mat$matched==TRUE,]
data.matched.dur$w <- mat$w[mat$matched==TRUE]

matched.table.data.dur <- data.matched.dur
matched.table.data.dur$No.troops.country <- matched.table.data.dur$No.troops.country/100000
matched.table.data.dur$No.troops <- matched.table.data.dur$No.troops/10000
matched.table.data.dur$best.ged <- matched.table.data.dur$best.ged/1000


############################
#### duration treatment ####
############################

#### during model ####

model.data <- model.frame(duringUN.period.spell.cat~max.UN.duration+capdist+nlights_max+nlights_max.l1+nlights_mean+nlights_mean.l1+nlights_calib_mean_orig+nlights_calib_mean.l1_orig+pop_gpw_sum+best.ged.beforeUN+ttime_mean+best.ged+light.1994+No.troops_sp+No.troops.country+No.troops+afterUN.period.spell+duringUN.period.spell+year+country_name+gid++MissionID+redeploy.period+max.redeploy.period+time.since.last.best.gid+time.since.last.best.country+max.No.troops,data=un.data.final[un.data.final$year<=2012 & un.data.final$year>1994,])

#matching

mat <- cem(treatment= "duringUN.period.spell.cat", data =model.data[model.data$duringUN.period.spell==0 & model.data$afterUN.period.spell==0|model.data$duringUN.period.spell>0 & model.data$afterUN.period.spell==0,] , drop =c("nlights_max","nlights_max.l1","nlights_mean","nlights_mean.l1","nlights_calib_mean_orig","nlights_calib_mean.l1_orig","afterUN.period.spell","max.UN.duration","No.troops_sp","duringUN.period.spell","No.troops.country","No.troops","year","country_name","gid","MissionID","best.ged","redeploy.period","max.redeploy.period","time.since.last.best.gid","time.since.last.best.country","max.No.troops"),keep.all=TRUE)
table(mat$matched)
# mat2 <- k2k(mat, model.data , "euclidean", 1)
# table(mat2$matched)

data.match.dur2 <- model.data[model.data$duringUN.period.spell==0 & model.data$afterUN.period.spell==0|model.data$duringUN.period.spell>0 & model.data$afterUN.period.spell==0,]
data.matched.dur2 <- data.match.dur2[mat$matched==TRUE,]
data.matched.dur2$w <- mat$w[mat$matched==TRUE]

matched.table.data.dur2 <- data.matched.dur2
matched.table.data.dur2$No.troops.country <- matched.table.data.dur2$No.troops.country/100000
matched.table.data.dur2$No.troops <- matched.table.data.dur2$No.troops/10000
matched.table.data.dur2$best.ged <- matched.table.data.dur2$best.ged/1000
