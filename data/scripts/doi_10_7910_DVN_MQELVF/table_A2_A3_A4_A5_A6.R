#Table A2 Unmatched Sample

coef.Names <- c("Nightlights Mean","Calibrated Nightlights Mean","Nightlights Max","Number of troops in country","Number of troops in grid","Years after peacekeepers left grid","Duration of peacekeepers in grid","Casualties in grid","Spatial Lag number of troops in grid","Redeployment period","Peace time in grid","Peace time in country")

df <- model.data[,c("nlights_mean","nlights_calib_mean_orig","nlights_max","No.troops.country","No.troops","afterUN.period.spell","duringUN.period.spell","best.ged","No.troops_sp","redeploy.period","time.since.last.best.gid","time.since.last.best.country")]

stargazer(df,mean.sd = TRUE,iqr=FALSE,omit.summary.stat = c("p25", "p75"),covariate.labels=coef.Names,label="sum:tab:1",title="Summary statistics for variables in unmatched sample")

#Table A3 Matched Sample

df <- temp.data[,c("nlights_mean","nlights_calib_mean_orig","nlights_max","No.troops.country","No.troops","afterUN.period.spell","duringUN.period.spell","best.ged","No.troops_sp","redeploy.period","time.since.last.best.gid","time.since.last.best.country")]

stargazer(df,mean.sd = TRUE,iqr=FALSE,omit.summary.stat = c("p25", "p75"),covariate.labels=coef.Names,label="sum:tab:2",title="Summary statistics for variables in matched sample")


#Table A4 Sample grids that experienced UN peacekeeping for variables in matched sample
df <- temp.data[model.data$un.yes==1,c("nlights_mean","nlights_calib_mean_orig","nlights_max","No.troops.country","No.troops","afterUN.period.spell","duringUN.period.spell","best.ged","No.troops_sp","redeploy.period","time.since.last.best.gid","time.since.last.best.country")]

	stargazer(df,mean.sd = TRUE,iqr=FALSE,omit.summary.stat = c("p25", "p75"),covariate.labels=coef.Names,label="sum:tab:3",title="Summary statistics for grids that experienced UN peacekeeping for variables in matched sample")
	

#Table A5 Sample grids without UN peacekeeping for variables in matched sample	
df <- temp.data[model.data$un.yes==0,c("nlights_mean","nlights_calib_mean_orig","nlights_max","No.troops.country","No.troops","afterUN.period.spell","duringUN.period.spell","best.ged","No.troops_sp","redeploy.period","time.since.last.best.gid","time.since.last.best.country")]
	
	stargazer(df,mean.sd = TRUE,iqr=FALSE,omit.summary.stat = c("p25", "p75"),covariate.labels=coef.Names,label="sum:tab:4",title="Summary statistics for grids without UN peacekeeping for variables in matched sample")
	
	

#Table A6 Variables used for matching
coef.Names <- c("Capital Distance","Population in 1990 (Gridded Population of the World)","Number of casualties before UN","Travel time to the next urban center","Nighlight emissions in 1994 (calibrated)")

df <- model.data[,c("capdist","pop_gpw_sum","best.ged.beforeUN","ttime_mean","light.1994")]

stargazer(df,mean.sd = TRUE,iqr=FALSE,omit.summary.stat = c("p25", "p75"),covariate.labels=coef.Names,label="sum:tab:5",title="Summary statistics for variables used for matching")






