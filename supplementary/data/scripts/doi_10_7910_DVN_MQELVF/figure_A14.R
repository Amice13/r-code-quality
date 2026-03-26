
#### Panel a)

####################  
# After
#################### 

model.fe.2max <- plm(nlights_max~nlights_max.l1+No.troops.country+No.troops+best.ged+No.troops_sp
+redeploy.period+time.since.last.best.gid+time.since.last.best.country+afterUN.period.spell+duringUN.period.spell+I(duringUN.period.spell^2),data=table.data[mat2$matched==TRUE,],index=c("gid","year"),model="within",effect="twoways")
summary(model.fe.2max)

model.fe <- model.fe.2max
 
values <- apply(model.data[,c("nlights_max.l1","No.troops.country","No.troops","best.ged","No.troops_sp","redeploy.period","time.since.last.best.gid","time.since.last.best.country","afterUN.period.spell","duringUN.period.spell")],2,FUN=mean)
	values[length(values)+1] <- values["duringUN.period.spell"]^2
		values <- rbind(values,values,values,values,values,values,values,values,values,values)
			colnames(values)[dim(values)[2]] <- "duringUN.period.spell.2"
				values[,"afterUN.period.spell"] <- seq(from=min(model.data$afterUN.period.spell),to=max(model.data$afterUN.period.spell),length.out=10)

setwd(pathR)
	source('sim_helper.R', chdir = TRUE)

					data.2$Var2 <- rep(seq(from=min(model.data$afterUN.period.spell),to=max(model.data$afterUN.period.spell),length.out=10),each=7)


n.p1 <- ggplot(data.2[data.2$Var1=="2.5%"|data.2$Var1=="97.5%"|data.2$Var1=="50%",], aes(x=Var2, y=value,fill=as.factor(Var1)))+
    geom_line(aes(linetype=Var1))+scale_linetype_manual(values=c("dashed","solid" ,"dashed"))+xlab("Years")+ ylab("Maximum Nightlight in Grid")+
    theme(legend.position="none")

setwd(pathFig)
ggsave("predMatchAfterMax.pdf",n.p1,width = 9, height = 9, units = c("cm"))

#### Panel b)



####################  
# After
####################  

model.fe.2max.after.sq <- plm(nlights_max~nlights_max.l1+No.troops.country+No.troops+afterUN.period.spell+duringUN.period.spell+best.ged+No.troops_sp 
+redeploy.period+time.since.last.best.gid+time.since.last.best.country+I(duringUN.period.spell^2)+I(afterUN.period.spell^2)+I(afterUN.period.spell^3),data=table.data[mat2$matched==TRUE,],index=c("gid","year"),model="within",effect="twoways")

model.fe <- model.fe.2max.after.sq

values <- apply(model.data[,c("nlights_max.l1","No.troops.country","No.troops","afterUN.period.spell","duringUN.period.spell","best.ged","No.troops_sp","redeploy.period","time.since.last.best.gid","time.since.last.best.country")],2,FUN=mean)
	values[length(values)+1] <- values["duringUN.period.spell"]^2
		values <- rbind(values,values,values,values,values,values,values,values,values,values)
			colnames(values)[dim(values)[2]] <- "duringUN.period.spell.2"
				values[,"afterUN.period.spell"] <- seq(from=min(model.data$afterUN.period.spell),to=max(model.data$afterUN.period.spell),length.out=10)
					values <- cbind(values,values[,"afterUN.period.spell"]^2)
						colnames(values)[dim(values)[2]] <- "afterUN.period.spell.2"
							values <- cbind(values,values[,"afterUN.period.spell"]^3)
								colnames(values)[dim(values)[2]] <- "afterUN.period.spell.3"

setwd(pathR)
	source('sim_helper.R', chdir = TRUE)

					data.2$Var2 <- rep(seq(from=min(model.data$afterUN.period.spell),to=max(model.data$afterUN.period.spell),length.out=10),each=7)


n.p1 <- ggplot(data.2[data.2$Var1=="2.5%"|data.2$Var1=="97.5%"|data.2$Var1=="50%",], aes(x=Var2, y=value,fill=as.factor(Var1)))+
    geom_line(aes(linetype=Var1))+scale_linetype_manual(values=c("dashed","solid" ,"dashed"))+xlab("Years")+ ylab("Maximum Nightlight in Grid")+
    theme(legend.position="none")


setwd(pathFig)
ggsave("predMatchAfterMaxCube.pdf",n.p1,width = 9, height = 9, units = c("cm"))


#### Panel c)



####################  
# After
####################  

setwd(pathData)

load("unGridAllV7.rda")

un.data.final <- un.data.final%>%
					dplyr::group_by(gid) %>%
						dplyr::mutate(max.UN.duration = max(duringUN.period))

un.data.final <- un.data.final%>%
					dplyr::group_by(gid) %>%
						dplyr::mutate(max.redeploy.period = max(redeploy.period))

un.data.final <- un.data.final%>%
					dplyr::group_by(gid) %>%
						dplyr::mutate(max.No.troops = max(No.troops))


#Model Frame
model.data <- model.frame(nlights_calib_mean~nlights_sd+max.UN.duration+nlights_sd.l1+nlights_sd_st+nlights_sd_st.l1+nlights_max_st+nlights_max_st.l1+nlights_max+nlights_mean+nlights_max.l1+nlights_mean.l1++nlights_mean_st+nlights_mean_st.l1+pop_gpw_sum+No.troops.country+beforeUN+No.troops+afterUN.period+MissionID+capdist+excluded+ttime_mean+best.ged+duringUN.period+afterUN+year+duringUN+gid+notroops.cummax+No.troops_sp+notroops.cummax_sp+beforeUN_sp+duringUN_sp+afterUN_sp+country_name+count_project+count_project.l1+nlights_calib_mean.l1+duringUN.period.spell+afterUN.period.spell+un.yes+nlights_calib_mean_orig+nlights_calib_mean.l1_orig+nlights_calib_meanNC+nlights_calib_meanNC.l1+redeploy.period+max.redeploy.period+time.since.last.best.gid+time.since.last.best.country+max.No.troops,data=un.data.final[un.data.final$year<=2012 & un.data.final$year>1994,])


###########################
# Matched Model estimation
###########################


model.data <- model.frame(un.yes~max.UN.duration+nlights_sd+nlights_sd.l1+notroops.cummax+nlights_max_st+nlights_max_st.l1+capdist+nlights_max+nlights_max.l1+nlights_mean+nlights_mean.l1+nlights_calib_mean_orig+nlights_calib_mean.l1_orig+pop_gpw_sum+best.ged.beforeUN+ttime_mean+best.ged+light.1994+No.troops_sp+nlights_calib_mean+No.troops.country+No.troops+afterUN.period.spell+duringUN.period.spell+year+count_project+count_project.l1+country_name+gid+nlights_calib_mean.l1+MissionID+redeploy.period+max.redeploy.period+time.since.last.best.gid+time.since.last.best.country+max.No.troops,data=un.data.final[un.data.final$year<=2012 & un.data.final$year>1994,])


model.data <- model.data %>%ungroup()


## Matching for the after models
#model.data$un.yes <- 0
#model.data$un.yes[model.data$afterUN.period.spell>0] <- 1


mat <- cem(treatment= "un.yes", data =model.data[model.data$afterUN.period.spell==1|model.data$duringUN.period.spell==0 & model.data$afterUN.period.spell==0 & model.data$duringUN.period.spell==0,] , drop =c("nlights_sd","nlights_sd.l1","nlights_max_st","notroops.cummax","nlights_max_st.l1","nlights_max","nlights_max.l1","nlights_mean","nlights_mean.l1","nlights_calib_mean_orig","nlights_calib_mean.l1_orig","afterUN.period.spell","max.UN.duration","No.troops_sp","duringUN.period.spell","No.troops.country", "count_project","count_project.l1","No.troops","nlights_calib_mean","country_name","year","gid","nlights_calib_mean.l1","MissionID","best.ged","redeploy.period","max.redeploy.period","time.since.last.best.gid","time.since.last.best.country","max.No.troops"),keep.all=TRUE) #matching in year as well
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
matched.table.data.after <- table.data[which(table.data$gid %in% gid.match.after),]
matched.table.data.after <- matched.table.data.after[matched.table.data.after$duringUN.period.spell==0,]
	
	
#Matched after
matched.table.data.after$lag.dv <- matched.table.data.after$nlights_max.l1
model.fe.2max.after <- plm(nlights_max~lag.dv+No.troops.country+afterUN.period.spell+best.ged+No.troops_sp 
+time.since.last.best.gid+time.since.last.best.country+I(max.No.troops*afterUN.period.spell),data=matched.table.data.after,index=c("gid","year"),model="within",effect="twoways")

model.fe <- model.fe.2max.after



values <- apply(matched.table.data.after[,c("lag.dv","No.troops.country","afterUN.period.spell","best.ged","No.troops_sp","time.since.last.best.gid","time.since.last.best.country","max.No.troops")],2,FUN=mean)
		values <- rbind(values,values,values,values,values,values,values,values,values,values)
				values[,"afterUN.period.spell"] <- seq(from=min(matched.table.data.after$afterUN.period.spell),to=max(matched.table.data.after$afterUN.period.spell),length.out=10)
					values[,"max.No.troops"] <- values[,"max.No.troops"] * values[,"afterUN.period.spell"]


setwd(pathR)
	source('sim_helper.R', chdir = TRUE)

					data.2$Var2 <- rep(seq(from=min(matched.table.data.after$afterUN.period.spell),to=max(matched.table.data.after$afterUN.period.spell),length.out=10),each=7)


n.p1 <- ggplot(data.2[data.2$Var1=="2.5%"|data.2$Var1=="97.5%"|data.2$Var1=="50%",], aes(x=Var2, y=value,fill=as.factor(Var1)))+
    geom_line(aes(linetype=Var1))+scale_linetype_manual(values=c("dashed","solid" ,"dashed"))+xlab("Years")+ ylab("Maximum Nightlight in Grid")+
    theme(legend.position="none")



setwd(pathFig)
ggsave("predMatchAfterMaxAfter.pdf",n.p1,width = 9, height = 9, units = c("cm"))





#### Panel d)

####################  
# After
####################  


#Matched after


matched.table.data.after$lag.dv <- matched.table.data.after$nlights_max.l1
model.fe.2max.after <- plm(nlights_max~lag.dv+No.troops.country+afterUN.period.spell+best.ged+No.troops_sp 
+time.since.last.best.gid+time.since.last.best.country+I(max.No.troops*afterUN.period.spell)+I(afterUN.period.spell^2)+I(afterUN.period.spell^3),data=matched.table.data.after,index=c("gid","year"),model="within",effect="twoways")

model.fe <- model.fe.2max.after




values <- apply(matched.table.data.after[,c("lag.dv","No.troops.country","afterUN.period.spell","best.ged","No.troops_sp","time.since.last.best.gid","time.since.last.best.country","max.No.troops")],2,FUN=mean)
		values <- rbind(values,values,values,values,values,values,values,values,values,values)
				values[,"afterUN.period.spell"] <- seq(from=min(matched.table.data.after$afterUN.period.spell),to=max(matched.table.data.after$afterUN.period.spell),length.out=10)
					values[,"max.No.troops"] <- values[,"max.No.troops"] * values[,"afterUN.period.spell"]
					values <- cbind(values,values[,"afterUN.period.spell"]^2)
						colnames(values)[dim(values)[2]] <- "afterUN.period.spell.2"
							values <- cbind(values,values[,"afterUN.period.spell"]^3)
								colnames(values)[dim(values)[2]] <- "afterUN.period.spell.3"

setwd(pathR)
	source('sim_helper.R', chdir = TRUE)

					data.2$Var2 <- rep(seq(from=min(matched.table.data.after$afterUN.period.spell),to=max(matched.table.data.after$afterUN.period.spell),length.out=10),each=7)


n.p1 <- ggplot(data.2[data.2$Var1=="2.5%"|data.2$Var1=="97.5%"|data.2$Var1=="50%",], aes(x=Var2, y=value,fill=as.factor(Var1)))+
    geom_line(aes(linetype=Var1))+scale_linetype_manual(values=c("dashed","solid" ,"dashed"))+xlab("Years")+ ylab("Maximum Nightlight in Grid")+
    theme(legend.position="none")




setwd(pathFig)
ggsave("predMatchAfterMaxAfterCube.pdf",n.p1,width = 9, height = 9, units = c("cm"))



