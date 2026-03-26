
######################################################
#Matched
######################################################	

setwd(pathFig)
results.list <- list()

#nlights_max ###############################
table.data$lag.dv <- table.data$nlights_max.l1

results.list[[1]] <- plm(nlights_max~lag.dv+No.troops.country+factor(afterUN.period.spell)+factor(duringUN.period.spell)+best.ged+No.troops_sp
+redeploy.period+time.since.last.best.gid+time.since.last.best.country,data=table.data[mat2$matched==TRUE,],index=c("gid","year"),model="within",effect="twoways")


results.list[[4]] <- plm(nlights_max~lag.dv+No.troops.country+factor(afterUN.period.spell)+factor(duringUN.period.spell)+best.ged+No.troops_sp
+redeploy.period+time.since.last.best.gid+time.since.last.best.country +No.troops,data=table.data[mat2$matched==TRUE,],index=c("gid","year"),model="within",effect="twoways")



# nlights_calib_mean_orig ###############################
table.data$lag.dv <- table.data$nlights_calib_mean.l1_orig

results.list[[2]] <- plm(nlights_calib_mean_orig~lag.dv+No.troops.country+factor(afterUN.period.spell)+factor(duringUN.period.spell)+best.ged+No.troops_sp+redeploy.period+time.since.last.best.gid+time.since.last.best.country
,data=table.data[mat2$matched==TRUE,],index=c("gid","year"),model="within",effect="twoways")


results.list[[5]] <- plm(nlights_calib_mean_orig~lag.dv+No.troops.country+factor(afterUN.period.spell)+factor(duringUN.period.spell)+best.ged+No.troops_sp+redeploy.period+time.since.last.best.gid+time.since.last.best.country + No.troops
,data=table.data[mat2$matched==TRUE,],index=c("gid","year"),model="within",effect="twoways")



#nlights_mean ###############################
table.data$lag.dv <- table.data$nlights_mean.l1

results.list[[3]] <- plm(nlights_mean~lag.dv+No.troops.country+factor(afterUN.period.spell)+factor(duringUN.period.spell)+best.ged+No.troops_sp+redeploy.period+time.since.last.best.gid+time.since.last.best.country
,data=table.data[mat2$matched==TRUE,],index=c("gid","year"),model="within",effect="twoways")


results.list[[6]] <- plm(nlights_mean~lag.dv+No.troops.country+factor(afterUN.period.spell)+factor(duringUN.period.spell)+best.ged+No.troops_sp+redeploy.period+time.since.last.best.gid+time.since.last.best.country + No.troops
,data=table.data[mat2$matched==TRUE,],index=c("gid","year"),model="within",effect="twoways")



model.names <- c("night_max","night_calib","night_mean")

for(i in 1:3){
coeff.after <- data.frame(summary(results.list[[i]])[1]$coefficients[3:19,1:2])
coeff.dur <- data.frame(summary(results.list[[i]])[1]$coefficients[20:31,1:2])
	coeff.after$time <- 1:dim(coeff.after)[1]
	coeff.dur$time <- 1:dim(coeff.dur)[1]
		coeff.after$lb_CI_95 <- coeff.after$Estimate-1.96*coeff.after$Std..Error
		coeff.dur$lb_CI_95 <- coeff.dur$Estimate-1.96*coeff.dur$Std..Error
			coeff.after$up_CI_95 <- coeff.after$Estimate+1.96*coeff.after$Std..Error
			coeff.dur$up_CI_95 <- coeff.dur$Estimate+1.96*coeff.dur$Std..Error

p1 <- ggplot(coeff.after, aes(x=time, y=Estimate, ymin = lb_CI_95, ymax = up_CI_95)) +
		#ylim(-0.25,0.5)+
		geom_pointrange()+
			labs(title = "",
             x = "Time since Deployment",
             y = "Estimated Effect")+ theme_bw() + geom_hline(yintercept=0,linetype="dashed")
             
p2 <- ggplot(coeff.dur, aes(x=time, y=Estimate, ymin = lb_CI_95, ymax = up_CI_95)) +
		#ylim(-0.25,0.5)+
		geom_pointrange()+
			labs(title = "",
             x = "Time since Withdrawal",
             y = "Estimated Effect")+ theme_bw() + geom_hline(yintercept=0,linetype="dashed")
              
  setwd(pathData)         
            fileAfter <- paste(model.names[i],"Figure_A10_After.pdf",sep="")
            	fileDur <- paste(model.names[i],"Figure_A10_Dur.pdf",sep="")
ggsave(fileAfter,p1,width = 10*1.63, height = 10, units = c("cm"))
ggsave(fileDur,p2,width = 10*1.63, height = 10, units = c("cm"))
}



























