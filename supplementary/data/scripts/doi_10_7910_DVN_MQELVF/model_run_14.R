setwd(pathData)
load("unGridAll.rda")

#new variables

# max
un.data.final <- un.data.final%>%
  dplyr::group_by(gid) %>%
  dplyr::mutate(max.UN.duration = max(duringUN.period))

un.data.final <- un.data.final%>%
  dplyr::group_by(gid) %>%
  dplyr::mutate(max.redeploy.period = max(redeploy.period))

un.data.final <- un.data.final%>%
  dplyr::group_by(gid) %>%
  dplyr::mutate(max.No.troops = max(No.troops))

#un status 

un.data.final$un.status = case_when(
  un.data.final$duringUN > 0 ~ "2", 
  un.data.final$afterUN > 0 ~ "3",
  T ~ "1") %>% factor(levels = c("1", "2", "3"))

table(un.data.final$un.status)

#select 
model.data <- model.frame(un.status~un.yes+max.UN.duration+capdist+nlights_max+nlights_max.l1+nlights_mean+nlights_mean.l1+nlights_calib_mean_orig+nlights_calib_mean.l1_orig+pop_gpw_sum+best.ged.beforeUN+ttime_mean+best.ged+light.1994+No.troops_sp+No.troops.country+No.troops+afterUN.period.spell+duringUN.period.spell+year+country_name+gid+MissionID+redeploy.period+max.redeploy.period+time.since.last.best.gid+time.since.last.best.country+max.No.troops,data=un.data.final[un.data.final$year<=2012 & un.data.final$year>1994,])
model.data$MissionID = NULL


table.data <- model.data
table.data$No.troops.country <- table.data$No.troops.country/100000
table.data$best.ged <- table.data$best.ged/1000


#matching

mat <- cem(treatment= "un.yes", data = model.data , drop =c("nlights_max","nlights_max.l1","nlights_mean",
                                                               "nlights_mean.l1","nlights_calib_mean_orig","nlights_calib_mean.l1_orig",
                                                               "afterUN.period.spell","max.UN.duration","No.troops_sp","duringUN.period.spell",
                                                               "No.troops.country","No.troops",
                                                               "year","country_name","gid","MissionID","best.ged",
                                                               "redeploy.period","max.redeploy.period","time.since.last.best.gid","time.since.last.best.country",
                                                               "max.No.troops","un.status"), keep.all=TRUE)
table(mat$matched)
mat2 <- k2k(mat, model.data , "euclidean", 1)
table(mat2$matched)

data.match <- model.data
data.matched <- data.match[mat2$matched==TRUE,]
data.matched$w <- mat2$w[mat2$matched==TRUE]
table(data.matched$un.status)

# data.match <- model.data
# data.matched <- data.match[mat$matched==TRUE,]
# data.matched$w <- mat$w[mat$matched==TRUE]
# table(data.matched$un.status)

matched.table.data <- data.matched
matched.table.data$No.troops.country <- matched.table.data$No.troops.country/100000
matched.table.data$best.ged <- matched.table.data$best.ged/1000


