#Full and Matched models for simple models

#models without matching 

#basic model without matching + controls and two-way fixed effects

table.data$lag.dv = table.data$nlights_mean.l1
m_mean2 = plm (nlights_mean ~ lag.dv + un.status + No.troops.country + best.ged + No.troops_sp   + redeploy.period + time.since.last.best.gid + time.since.last.best.country,  data=table.data,index=c("gid","year"),model="within",effect="twoways")

table.data$lag.dv = table.data$nlights_max.l1
m_max2 = plm (nlights_max ~ lag.dv + un.status + No.troops.country + best.ged + No.troops_sp  + redeploy.period + time.since.last.best.gid + time.since.last.best.country, data=table.data,index=c("gid","year"),model="within",effect="twoways")

table.data$lag.dv = table.data$nlights_calib_mean.l1_orig
m_orig2 = plm (nlights_calib_mean_orig ~ lag.dv + un.status + No.troops.country + best.ged + No.troops_sp   +  redeploy.period +time.since.last.best.gid + time.since.last.best.country,  data=table.data,index=c("gid","year"),model="within",effect="twoways")

#models with matching  

#basic model WITH matching + controls and two-way fixed effects

matched.table.data$lag.dv = matched.table.data$nlights_mean.l1
m_mean_matched = plm (nlights_mean ~ lag.dv + un.status + No.troops.country + best.ged + No.troops_sp + redeploy.period + time.since.last.best.gid + time.since.last.best.country,  data=matched.table.data,index=c("gid","year"),model="within",effect="twoways", weights = w)


matched.table.data$lag.dv = matched.table.data$nlights_max.l1
m_max_matched = plm (nlights_max ~ lag.dv + un.status + No.troops.country + best.ged + No.troops_sp + redeploy.period + time.since.last.best.gid + time.since.last.best.country, data=matched.table.data,index=c("gid","year"),model="within",effect="twoways", weights = w)
summary(m_max_matched)

matched.table.data$lag.dv = matched.table.data$nlights_calib_mean.l1_orig
m_orig_matched = plm (nlights_calib_mean_orig ~ lag.dv + un.status + No.troops.country + best.ged + No.troops_sp + redeploy.period + time.since.last.best.gid + time.since.last.best.country,  data=matched.table.data,index=c("gid","year"),model="within",effect="twoways", weights = w)
summary(m_orig_matched)




#figures - MAX ####
#before_never 
values1 = apply(data.matched[,c("nlights_max.l1", "No.troops.country","best.ged","No.troops_sp", "redeploy.period", "time.since.last.best.gid", "time.since.last.best.country")],2,FUN=mean)
values1["un.status2"] = 0
values1["un.status3"] = 0
values1 = values1[c("nlights_max.l1", "un.status2" , "un.status3", "No.troops.country","best.ged","No.troops_sp", "redeploy.period", "time.since.last.best.gid", "time.since.last.best.country")]

#during
values2 = apply(data.matched[,c("nlights_max.l1", "No.troops.country","best.ged","No.troops_sp", "redeploy.period", "time.since.last.best.gid", "time.since.last.best.country")],2,FUN=mean)
values2["un.status2"] = 1
values2["un.status3"] = 0
values2 = values2[c("nlights_max.l1", "un.status2" , "un.status3", "No.troops.country","best.ged","No.troops_sp", "redeploy.period", "time.since.last.best.gid", "time.since.last.best.country")]

#after
values3 = apply(data.matched[,c("nlights_max.l1", "No.troops.country","best.ged","No.troops_sp", "redeploy.period", "time.since.last.best.gid", "time.since.last.best.country")],2,FUN=mean)
values3["un.status2"] = 0
values3["un.status3"] = 1
values3 = values3[c("nlights_max.l1", "un.status2" , "un.status3", "No.troops.country","best.ged","No.troops_sp", "redeploy.period", "time.since.last.best.gid", "time.since.last.best.country")]

#modified sim helper 
model_pred = plm (nlights_max ~ nlights_max.l1 + un.status + No.troops.country + best.ged + No.troops_sp + redeploy.period + time.since.last.best.gid + time.since.last.best.country, data=data.matched,index=c("gid","year"),model="within",effect="twoways", weights = w)
summary(model_pred)

betas <- coef(model_pred)
varCov <- vcov(model_pred)
reps <- 1000
yhat <- matrix(NA,reps,3) 

for(k in 1:reps){
  yhat[k,1] <- values1 %*% MASS::mvrnorm(1,betas,varCov)
  yhat[k,2] <- values2 %*% MASS::mvrnorm(1,betas,varCov)
  yhat[k,3] <- values3 %*% MASS::mvrnorm(1,betas,varCov)
}

#preds
before.never = melt(quantile(yhat[,1], c(0.025, 0.5, 0.975))) 
before.never$pred = row.names(before.never)
before.never = before.never %>% spread(pred, value) %>% rename ()
before.never$label = "Before/Never"
colnames(before.never) = c("low", "medium", "high", "label")

during  = melt(quantile(yhat[,2], c(0.025, 0.5, 0.975)))
during$pred = row.names(during)
during = during %>% spread(pred, value) %>% rename ()
during$label = "Present"
colnames(during) = c("low", "medium", "high", "label")

after = melt(quantile(yhat[,3], c(0.025, 0.5, 0.975)))
after$pred = row.names(after)
after = after %>% spread(pred, value) %>% rename ()
after$label = "Withdrawn"
colnames(after) = c("low", "medium", "high", "label")

pred = rbind(before.never, during, after)
pred$label = as.factor(pred$label) %>% factor(levels = c("Before/Never", "Present", "Withdrawn"))



# ggsave("predUNstatusMax_uncat.pdf",p1,width = 9, height = 9, units = c("cm"))

#first differences
during_before.never = melt(quantile(yhat[,2] - yhat[,1], c(0.025, 0.5, 0.975)))#during - before
during_before.never$fd  = row.names(during_before.never)
during_before.never = during_before.never %>% spread(fd, value) %>% rename ()
during_before.never$label = "Present vs.\nBefore/Never"
colnames(during_before.never) = c("low", "medium", "high", "label")

after_before.never = melt(quantile(yhat[,3] - yhat[,1], c(0.025, 0.5, 0.975))) #after - before
after_before.never$fd  = row.names(after_before.never)
after_before.never = after_before.never %>% spread(fd, value) %>% rename ()
after_before.never$label = "Withdrawn vs.\nBefore/Never"
colnames(after_before.never) = c("low", "medium", "high", "label")

during_after = melt(quantile(yhat[,3] - yhat[,2], c(0.025, 0.5, 0.975))) #after - during 
during_after$fd  = row.names(during_after)
during_after = during_after %>% spread(fd, value) %>% rename ()
during_after$label = "Withdrawn vs.\nPresent"
colnames(during_after) = c("low", "medium", "high", "label")

fd = rbind(during_before.never, after_before.never, during_after)
fd$label = as.factor(fd$label) %>% factor(levels = c("Present vs.\nBefore/Never", "Withdrawn vs.\nBefore/Never", "Withdrawn vs.\nPresent"))

fdmax = ggplot(fd, aes(label, medium, ymin = low, ymax = high)) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "gray10") + ylim (-1, 1.5) + 
  geom_point() + geom_pointrange(color = "gray10")+xlab("")+ ylab("Difference in Maximum Nightlight in Grid")+
  
  theme(legend.position="none") 


setwd(pathFig)
ggsave("max_firstdiff.pdf",fdmax,width = 9, height = 9, units = c("cm"))

#figures - MEAN ####
#before_never 
values1 = apply(data.matched[,c("nlights_mean.l1", "No.troops.country","best.ged","No.troops_sp", "redeploy.period", "time.since.last.best.gid", "time.since.last.best.country")],2,FUN=mean)
values1["un.status2"] = 0
values1["un.status3"] = 0
values1 = values1[c("nlights_mean.l1", "un.status2" , "un.status3", "No.troops.country","best.ged","No.troops_sp", "redeploy.period", "time.since.last.best.gid", "time.since.last.best.country")]

#during
values2 = apply(data.matched[,c("nlights_mean.l1", "No.troops.country","best.ged","No.troops_sp", "redeploy.period", "time.since.last.best.gid", "time.since.last.best.country")],2,FUN=mean)
values2["un.status2"] = 1
values2["un.status3"] = 0
values2 = values2[c("nlights_mean.l1", "un.status2" , "un.status3", "No.troops.country","best.ged","No.troops_sp", "redeploy.period", "time.since.last.best.gid", "time.since.last.best.country")]

#after
values3 = apply(data.matched[,c("nlights_mean.l1", "No.troops.country","best.ged","No.troops_sp", "redeploy.period", "time.since.last.best.gid", "time.since.last.best.country")],2,FUN=mean)
values3["un.status2"] = 0
values3["un.status3"] = 1
values3 = values3[c("nlights_mean.l1", "un.status2" , "un.status3", "No.troops.country","best.ged","No.troops_sp", "redeploy.period", "time.since.last.best.gid", "time.since.last.best.country")]

#modified sim helper 
model_pred = plm (nlights_mean ~ nlights_mean.l1 + un.status + No.troops.country + best.ged + No.troops_sp + redeploy.period + time.since.last.best.gid + time.since.last.best.country, data=data.matched,index=c("gid","year"),model="within",effect="twoways", weights = w)
summary(model_pred)

betas <- coef(model_pred)
varCov <- vcov(model_pred)
reps <- 1000
yhat <- matrix(NA,reps,3) 

for(k in 1:reps){
  yhat[k,1] <- values1 %*% MASS::mvrnorm(1,betas,varCov)
  yhat[k,2] <- values2 %*% MASS::mvrnorm(1,betas,varCov)
  yhat[k,3] <- values3 %*% MASS::mvrnorm(1,betas,varCov)
}

#preds
before.never = melt(quantile(yhat[,1], c(0.025, 0.5, 0.975))) 
before.never$pred = row.names(before.never)
before.never = before.never %>% spread(pred, value) %>% rename ()
before.never$label = "Before/Never"
colnames(before.never) = c("low", "medium", "high", "label")

during  = melt(quantile(yhat[,2], c(0.025, 0.5, 0.975)))
during$pred = row.names(during)
during = during %>% spread(pred, value) %>% rename ()
during$label = "Present"
colnames(during) = c("low", "medium", "high", "label")

after = melt(quantile(yhat[,3], c(0.025, 0.5, 0.975)))
after$pred = row.names(after)
after = after %>% spread(pred, value) %>% rename ()
after$label = "Withdrawn"
colnames(after) = c("low", "medium", "high", "label")

pred = rbind(before.never, during, after)
pred$label = as.factor(pred$label) %>% factor(levels = c("Before/Never", "Present", "Withdrawn"))


# ggsave("predUNstatusMean_uncat.pdf",p1,width = 9, height = 9, units = c("cm"))

#first differences
during_before.never = melt(quantile(yhat[,2] - yhat[,1], c(0.025, 0.5, 0.975)))#during - before
during_before.never$fd  = row.names(during_before.never)
during_before.never = during_before.never %>% spread(fd, value) %>% rename ()
during_before.never$label = "Present vs.\nBefore/Never"
colnames(during_before.never) = c("low", "medium", "high", "label")

after_before.never = melt(quantile(yhat[,3] - yhat[,1], c(0.025, 0.5, 0.975))) #after - before
after_before.never$fd  = row.names(after_before.never)
after_before.never = after_before.never %>% spread(fd, value) %>% rename ()
after_before.never$label = "Withdrawn vs.\nBefore/Never"
colnames(after_before.never) = c("low", "medium", "high", "label")

during_after = melt(quantile(yhat[,3] - yhat[,2], c(0.025, 0.5, 0.975))) #after - during 
during_after$fd  = row.names(during_after)
during_after = during_after %>% spread(fd, value) %>% rename ()
during_after$label = "Withdrawn vs.\nPresent"
colnames(during_after) = c("low", "medium", "high", "label")

fd = rbind(during_before.never, after_before.never, during_after)
fd$label = as.factor(fd$label) %>% factor(levels = c("Present vs.\nBefore/Never", "Withdrawn vs.\nBefore/Never", "Withdrawn vs.\nPresent"))

fdmean = ggplot(fd, aes(label, medium, ymin = low, ymax = high)) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "gray10") + ylim (-0.03, 0.03) +
  geom_point() + geom_pointrange(color = "gray10")+xlab("")+ ylab("Difference in Mean Nightlight in Grid")+
  
  theme(legend.position="none") 


setwd(pathFig)
ggsave("mean_firstdiff.pdf",fdmean,width = 9, height = 9, units = c("cm"))


#figures - CALIB ####
#before_never 
values1 = apply(data.matched[,c("nlights_calib_mean.l1_orig", "No.troops.country","best.ged","No.troops_sp", "redeploy.period", "time.since.last.best.gid", "time.since.last.best.country")],2,FUN=mean)
values1["un.status2"] = 0
values1["un.status3"] = 0
values1 = values1[c("nlights_calib_mean.l1_orig", "un.status2" , "un.status3", "No.troops.country","best.ged","No.troops_sp", "redeploy.period", "time.since.last.best.gid", "time.since.last.best.country")]

#during
values2 = apply(data.matched[,c("nlights_calib_mean.l1_orig", "No.troops.country","best.ged","No.troops_sp", "redeploy.period", "time.since.last.best.gid", "time.since.last.best.country")],2,FUN=mean)
values2["un.status2"] = 1
values2["un.status3"] = 0
values2 = values2[c("nlights_calib_mean.l1_orig", "un.status2" , "un.status3", "No.troops.country","best.ged","No.troops_sp", "redeploy.period", "time.since.last.best.gid", "time.since.last.best.country")]

#after
values3 = apply(data.matched[,c("nlights_calib_mean.l1_orig", "No.troops.country","best.ged","No.troops_sp", "redeploy.period", "time.since.last.best.gid", "time.since.last.best.country")],2,FUN=mean)
values3["un.status2"] = 0
values3["un.status3"] = 1
values3 = values3[c("nlights_calib_mean.l1_orig", "un.status2" , "un.status3", "No.troops.country","best.ged","No.troops_sp", "redeploy.period", "time.since.last.best.gid", "time.since.last.best.country")]

#modified sim helper 
model_pred = plm (nlights_calib_mean_orig ~ nlights_calib_mean.l1_orig + un.status + No.troops.country + best.ged + No.troops_sp + redeploy.period + time.since.last.best.gid + time.since.last.best.country, data=data.matched,index=c("gid","year"),model="within",effect="twoways", weights = w)
summary(model_pred)

betas <- coef(model_pred)
varCov <- vcov(model_pred)
reps <- 1000
yhat <- matrix(NA,reps,3) 

for(k in 1:reps){
  yhat[k,1] <- values1 %*% MASS::mvrnorm(1,betas,varCov)
  yhat[k,2] <- values2 %*% MASS::mvrnorm(1,betas,varCov)
  yhat[k,3] <- values3 %*% MASS::mvrnorm(1,betas,varCov)
}

#preds
before.never = melt(quantile(yhat[,1], c(0.025, 0.5, 0.975))) 
before.never$pred = row.names(before.never)
before.never = before.never %>% spread(pred, value) %>% rename ()
before.never$label = "Before/Never"
colnames(before.never) = c("low", "medium", "high", "label")

during  = melt(quantile(yhat[,2], c(0.025, 0.5, 0.975)))
during$pred = row.names(during)
during = during %>% spread(pred, value) %>% rename ()
during$label = "Present"
colnames(during) = c("low", "medium", "high", "label")

after = melt(quantile(yhat[,3], c(0.025, 0.5, 0.975)))
after$pred = row.names(after)
after = after %>% spread(pred, value) %>% rename ()
after$label = "Withdrawn"
colnames(after) = c("low", "medium", "high", "label")

pred = rbind(before.never, during, after)
pred$label = as.factor(pred$label) %>% factor(levels = c("Before/Never", "Present", "Withdrawn"))


# ggsave("predUNstatusCalib_uncat.pdf",p1,width = 9, height = 9, units = c("cm"))

#first differences
during_before.never = melt(quantile(yhat[,2] - yhat[,1], c(0.025, 0.5, 0.975)))#during - before
during_before.never$fd  = row.names(during_before.never)
during_before.never = during_before.never %>% spread(fd, value) %>% rename ()
during_before.never$label = "Present vs.\nBefore/Never"
colnames(during_before.never) = c("low", "medium", "high", "label")

after_before.never = melt(quantile(yhat[,3] - yhat[,1], c(0.025, 0.5, 0.975))) #after - before
after_before.never$fd  = row.names(after_before.never)
after_before.never = after_before.never %>% spread(fd, value) %>% rename ()
after_before.never$label = "Withdrawn vs.\nBefore/Never"
colnames(after_before.never) = c("low", "medium", "high", "label")

during_after = melt(quantile(yhat[,3] - yhat[,2], c(0.025, 0.5, 0.975))) #after - during 
during_after$fd  = row.names(during_after)
during_after = during_after %>% spread(fd, value) %>% rename ()
during_after$label = "Withdrawn vs.\nPresent"
colnames(during_after) = c("low", "medium", "high", "label")

fd = rbind(during_before.never, after_before.never, during_after)
fd$label = as.factor(fd$label) %>% factor(levels = c("Present vs.\nBefore/Never", "Withdrawn vs.\nBefore/Never", "Withdrawn vs.\nPresent"))

fdcalib = ggplot(fd, aes(label, medium, ymin = low, ymax = high)) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "gray10") + ylim(-0.001, 0.001) +
  geom_point() + geom_pointrange(color = "gray10")+xlab("")+ ylab("Difference in Mean Calibrated\nNightlight in Grid") +
 
  theme(legend.position="none") 


setwd(pathFig)
ggsave("calib_firstdiff.pdf",fdcalib,width = 9, height = 9, units = c("cm"))

# ggsave("firstdiff_combined_unyes.pdf", gridExtra::grid.arrange(fdmean, fdcalib, fdmax), width = 6, height = 7, units = c("in"))

