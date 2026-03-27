# Libraries
library(weights)

# Read in the data
load("Data_prepostcombined.Rdata")

##################
#### Check correlation
#####################

cor.test(pre$status_quo_1,pre$status_quo_2)
cor.test(post$status_quo_1,post$status_quo_2)

cor.test(pre$directional_1,pre$directional_2)
cor.test(post$directional_1,post$directional_2)

cor.test(pre$self_1,pre$self_2)
cor.test(post$self_1,post$self_2)

##################
#### Create data objects
#####################

# Create an object with only high-attention respondents 
combined_sub <- subset(combined, attention_check.x=="2" & attention_check.y=="2")

# Shape the datasets into long format
combined_long <- reshape(combined, varying = list(c("status_quo_1.x", "status_quo_1.y"),c("status_quo_2.x", "status_quo_2.y"),c("directional_1.x","directional_1.y"),c("directional_2.x","directional_2.y"),c("self_1.x","self_1.y"),c("self_2.x","self_2.y"),c("status_quo_mean.x","status_quo_mean.y"),c("directional_mean.x","directional_mean.y"),c("self_mean.x","self_mean.y"),c("thermo.x","thermo.y")),v.name=c("status_quo_1","status_quo_2","directional_1","directional_2","self_1","self_2","status_quo_mean","directional_mean","self_mean","thermo","date"), timevar="post", direction="long", idvar="PID",times=c(0,1))

combined_long_sub <- subset(combined_long, attention_check.x=="2" & attention_check.y=="2")

############################
###### Results for norm perception
###########################

set.seed(1120)

ttest_sq_w <- wtd.t.test(combined$status_quo_mean.y-combined$status_quo_mean.x, weight=combined$weights*combined$weights_pst_census,bootse=F)
mean(combined$status_quo_mean.x); mean(combined$status_quo_mean.y,na.rm=T)
ttest_dir_w <- wtd.t.test(combined$directional_mean.y-combined$directional_mean.x, weight=combined$weights*combined$weights_pst_census,bootse=F)
mean(combined$directional_mean.x); mean(combined$directional_mean.y,na.rm=T)

lm_sq_w <- lm(status_quo_mean~post+hh_income + religiosity + age + male + education + married, data=combined_long,weights=weights * weights_pst_census)
lm_dir_w <- lm(directional_mean~post+hh_income + religiosity + age + male + education + married, data=combined_long,weights=weights * weights_pst_census)

ttest_sq_w_sub <- wtd.t.test(combined_sub$status_quo_mean.y-combined_sub$status_quo_mean.x, weight=combined_sub$weights*combined_sub$weights_pst_census,bootse=F)
mean(combined_sub$status_quo_mean.x); mean(combined_sub$status_quo_mean.y)
ttest_dir_w_sub <- wtd.t.test(combined_sub$directional_mean.y-combined_sub$directional_mean.x, weight=combined_sub$weights*combined_sub$weights_pst_census,bootse=F)
mean(combined_sub$directional_mean.x); mean(combined_sub$directional_mean.y)

lm_sq_w_sub <- lm(status_quo_mean~post+hh_income + religiosity + age + male + education + married, data=combined_long_sub,weights=weights * weights_pst_census)
lm_dir_w_sub <- lm(directional_mean~post+hh_income + religiosity + age + male + education + married, data=combined_long_sub,weights=weights * weights_pst_census)

############################
###### Figure 1
###########################

pdf("plot_norm_perception_pstcensus.pdf",width=8,height=6)
par(mfrow=c(2,2),mai=c(0.6,0.6806,0.3,0.3486))

diff_sq_w <- ttest_sq_w$additional[1]
ttest_se_sq_w <- ttest_sq_w$additional[4]

coef_sq_w <- summary(lm_sq_w)$coef[2,1]
se_sq_w <- summary(lm_sq_w)$coef[2,2]

diff_sq_w_sub <- ttest_sq_w_sub$additional[1]
ttest_se_sq_w_sub <- ttest_sq_w_sub$additional[4]

coef_sq_w_sub <- summary(lm_sq_w_sub)$coef[2,1]
se_sq_w_sub <- summary(lm_sq_w_sub)$coef[2,2]

plot(x=c(),y=c(),ylim=c(0,2),xlim=c(0,0.6), ylab="Effect of referendum", main="Status quo perception (full sample)", xaxt="n", xlab="",cex.lab=1.2)
points(0.15,diff_sq_w,pch=16)
lines(x=c(0.15,0.15),y=c(diff_sq_w-1.96*ttest_se_sq_w,diff_sq_w+1.96*ttest_se_sq_w),lwd=2)
points(0.45,coef_sq_w,pch=16)
lines(x=c(0.45,0.45),y=c(coef_sq_w-1.96*se_sq_w,coef_sq_w+1.96*se_sq_w),lwd=2)
abline(h=0,lty="dashed")
mtext("T-test", side=1, at=0.15, line=1,cex=0.9)
mtext("Multiple regression", side=1, at=0.45, line=1,cex=0.9)

plot(x=c(),y=c(),ylim=c(0,2),xlim=c(0,0.6), ylab="Effect of referendum", main="Status quo perception (subsample)", xaxt="n", xlab="",cex.lab=1.2)
points(0.15,diff_sq_w_sub,pch=16)
lines(x=c(0.15,0.15),y=c(diff_sq_w_sub-1.96*ttest_se_sq_w_sub,diff_sq_w_sub+1.96*ttest_se_sq_w_sub),lwd=2)
points(0.45,coef_sq_w_sub,pch=16)
lines(x=c(0.45,0.45),y=c(coef_sq_w_sub-1.96*se_sq_w_sub,coef_sq_w_sub+1.96*se_sq_w_sub),lwd=2)
abline(h=0,lty="dashed")
mtext("T-test", side=1, at=0.15, line=1,cex=0.9)
mtext("Multiple regression", side=1, at=0.45, line=1,cex=0.9)

diff_dir_w <- ttest_dir_w$additional[1]
ttest_se_dir_w <- ttest_dir_w$additional[4]

coef_dir_w <- summary(lm_dir_w)$coef[2,1]
se_dir_w <- summary(lm_dir_w)$coef[2,2]

diff_dir_w_sub <- ttest_dir_w_sub$additional[1]
ttest_se_dir_w_sub <- ttest_dir_w_sub$additional[4]

coef_dir_w_sub <- summary(lm_dir_w_sub)$coef[2,1]
se_dir_w_sub <- summary(lm_dir_w_sub)$coef[2,2]

plot(x=c(),y=c(),ylim=c(0,2),xlim=c(0,0.6), ylab="Effect of referendum", main="Directional perception (full sample)", xaxt="n", xlab="",cex.lab=1.2)
points(0.15,diff_dir_w,pch=16)
lines(x=c(0.15,0.15),y=c(diff_dir_w-1.96*ttest_se_dir_w,diff_dir_w+1.96*ttest_se_dir_w),lwd=2)
points(0.45,coef_dir_w,pch=16)
lines(x=c(0.45,0.45),y=c(coef_dir_w-1.96*se_dir_w,coef_dir_w+1.96*se_dir_w),lwd=2)
abline(h=0,lty="dashed")
mtext("T-test", side=1, at=0.15, line=1,cex=0.9)
mtext("Multiple regression", side=1, at=0.45, line=1,cex=0.9)

plot(x=c(),y=c(),ylim=c(0,2),xlim=c(0,0.6), ylab="Effect of referendum", main="Directional perception (subsample)", xaxt="n", xlab="",cex.lab=1.2)
points(0.15,diff_dir_w_sub,pch=16)
lines(x=c(0.15,0.15),y=c(diff_dir_w_sub-1.96*ttest_se_dir_w_sub,diff_dir_w_sub+1.96*ttest_se_dir_w_sub),lwd=2)
points(0.45,coef_dir_w_sub,pch=16)
lines(x=c(0.45,0.45),y=c(coef_dir_w_sub-1.96*se_dir_w_sub,coef_dir_w_sub+1.96*se_dir_w_sub),lwd=2)
abline(h=0,lty="dashed")
mtext("T-test", side=1, at=0.15, line=1,cex=0.9)
mtext("Multiple regression", side=1, at=0.45, line=1,cex=0.9)

dev.off()

############################
###### Results for personal opinion
###########################

ttest_self_w <- wtd.t.test(combined$self_mean.y-combined$self_mean.x, weight=combined$weights*combined$weights_pst_census,bootse=F)
mean(combined$self_mean.x); mean(combined$self_mean.y,na.rm=T)
ttest_thermo_w <- wtd.t.test(combined$thermo.y-combined$thermo.x, weight=combined$weights*combined$weights_pst_census,bootse=F)
mean(combined$thermo.x); mean(combined$thermo.y,na.rm=T)

lm_self_w <- lm(self_mean~post+hh_income + religiosity + age + male + education + married, data=combined_long,weights=weights * weights_pst_census)
lm_thermo_w <- lm(thermo~post+hh_income + religiosity + age + male + education + married, data=combined_long,weights=weights * weights_pst_census)

ttest_self_w_sub <- wtd.t.test(combined_sub$self_mean.y-combined_sub$self_mean.x, weight=combined_sub$weights*combined_sub$weights_pst_census,bootse=F)
mean(combined_sub$self_mean.x); mean(combined_sub$self_mean.y)
ttest_thermo_w_sub <- wtd.t.test(combined_sub$thermo.y-combined_sub$thermo.x, weight=combined_sub$weights*combined_sub$weights_pst_census,bootse=F)
mean(combined_sub$thermo.x); mean(combined_sub$thermo.y)

lm_self_w_sub <- lm(self_mean~post+hh_income + religiosity + age + male + education + married, data=combined_long_sub,weights=weights * weights_pst_census)
lm_thermo_w_sub <- lm(thermo~post+hh_income + religiosity + age + male + education + married, data=combined_long_sub,weights=weights * weights_pst_census)

############################
###### Figure 2
###########################

pdf("plot_personal_opinion_pstcensus.pdf",width=8,height=6)
par(mfrow=c(2,2),mai=c(0.6,0.6806,0.3,0.3486))

diff_self_w <- ttest_self_w$additional[1]
ttest_se_self_w <- ttest_self_w$additional[4]

coef_self_w <- summary(lm_self_w)$coef[2,1]
se_self_w <- summary(lm_self_w)$coef[2,2]

diff_self_w_sub <- ttest_self_w_sub$additional[1]
ttest_se_self_w_sub <- ttest_self_w_sub$additional[4]

coef_self_w_sub <- summary(lm_self_w_sub)$coef[2,1]
se_self_w_sub <- summary(lm_self_w_sub)$coef[2,2]

plot(x=c(),y=c(),ylim=c(-1,1),xlim=c(0,0.6), ylab="Effect of referendum", main="Personal position (full sample)", xaxt="n", xlab="",cex.lab=1.2)
points(0.15,diff_self_w,pch=16)
lines(x=c(0.15,0.15),y=c(diff_self_w-1.96*ttest_se_self_w,diff_self_w+1.96*ttest_se_self_w),lwd=2)
points(0.45,coef_self_w,pch=16)
lines(x=c(0.45,0.45),y=c(coef_self_w-1.96*se_self_w,coef_self_w+1.96*se_self_w),lwd=2)
abline(h=0,lty="dashed")
mtext("T-test", side=1, at=0.15, line=1,cex=0.9)
mtext("Multiple regression", side=1, at=0.45, line=1,cex=0.9)

plot(x=c(),y=c(),ylim=c(-1,1),xlim=c(0,0.6), ylab="Effect of referendum", main="Personal position (subsample)", xaxt="n", xlab="",cex.lab=1.2)
points(0.15,diff_self_w_sub,pch=16)
lines(x=c(0.15,0.15),y=c(diff_self_w_sub-1.96*ttest_se_self_w_sub,diff_self_w_sub+1.96*ttest_se_self_w_sub),lwd=2)
points(0.45,coef_self_w_sub,pch=16)
lines(x=c(0.45,0.45),y=c(coef_self_w_sub-1.96*se_self_w_sub,coef_self_w_sub+1.96*se_self_w_sub),lwd=2)
abline(h=0,lty="dashed")
mtext("T-test", side=1, at=0.15, line=1,cex=0.9)
mtext("Multiple regression", side=1, at=0.45, line=1,cex=0.9)

diff_thermo_w <- ttest_thermo_w$additional[1]
ttest_se_thermo_w <- ttest_thermo_w$additional[4]

coef_thermo_w <- summary(lm_thermo_w)$coef[2,1]
se_thermo_w <- summary(lm_thermo_w)$coef[2,2]

diff_thermo_w_sub <- ttest_thermo_w_sub$additional[1]
ttest_se_thermo_w_sub <- ttest_thermo_w_sub$additional[4]

coef_thermo_w_sub <- summary(lm_thermo_w_sub)$coef[2,1]
se_thermo_w_sub <- summary(lm_thermo_w_sub)$coef[2,2]

plot(x=c(),y=c(),ylim=c(-5,7),xlim=c(0,0.6), ylab="Effect of referendum", main="Thermometer (full sample)", xaxt="n", xlab="",cex.lab=1.2)
points(0.15,diff_thermo_w,pch=16)
lines(x=c(0.15,0.15),y=c(diff_thermo_w-1.96*ttest_se_thermo_w,diff_thermo_w+1.96*ttest_se_thermo_w),lwd=2)
points(0.45,coef_thermo_w,pch=16)
lines(x=c(0.45,0.45),y=c(coef_thermo_w-1.96*se_thermo_w,coef_thermo_w+1.96*se_thermo_w),lwd=2)
abline(h=0,lty="dashed")
mtext("T-test", side=1, at=0.15, line=1,cex=0.9)
mtext("Multiple regression", side=1, at=0.45, line=1,cex=0.9)

plot(x=c(),y=c(),ylim=c(-5,7),xlim=c(0,0.6), ylab="Effect of referendum", main="Thermometer (subsample)", xaxt="n", xlab="",cex.lab=1.2)
points(0.15,diff_thermo_w_sub,pch=16)
lines(x=c(0.15,0.15),y=c(diff_thermo_w_sub-1.96*ttest_se_thermo_w_sub,diff_thermo_w_sub+1.96*ttest_se_thermo_w_sub),lwd=2)
points(0.45,coef_thermo_w_sub,pch=16)
lines(x=c(0.45,0.45),y=c(coef_thermo_w_sub-1.96*se_thermo_w_sub,coef_thermo_w_sub+1.96*se_thermo_w_sub),lwd=2)
abline(h=0,lty="dashed")
mtext("T-test", side=1, at=0.15, line=1,cex=0.9)
mtext("Multiple regression", side=1, at=0.45, line=1,cex=0.9)

dev.off()