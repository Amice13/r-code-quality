#####################################################################################################################################
# APPENDIX Replication File: "Reducing Bias in Citizens' Perception of Crime Rates: Evidence from a Field Experiment on Burglary Prevalence"
# Authors: Martin Vinæs Larsen & Asmus Leth Olsen
# The Journal of Politics
#####################################################################################################################################

# Run the code in the R file "3_data management" before running this file.  

################################################################## 
################################################################## 
######### SUMMARIZING DATA FOR PLACEBO ANALYSIS WITH UNEMPLOYMENT
################################################################## 
################################################################## 

stat_w1 <-df %>%
             filter(stat==1)   %>%
             summarize(avg_t = mean(p_trend_w1, na.rm = TRUE),
                       avg_l_ex = mean(p_level_w1_exact, na.rm = TRUE),
                       avg_l_pm = mean(p_level_w1_pm2, na.rm = TRUE),
                       n_t = length(p_trend_w1),
                       n_l_ex = length(p_level_w1_exact),
                       n_l_pm = length(p_level_w1_pm2)
                       )     %>%
             mutate(days=c("0_pre"))

stat_w2 <-df %>%
             filter(stat==1)   %>%
             group_by(days)   %>%
             summarize(avg_t = mean(p_trend_w2, na.rm = TRUE),
                       avg_l_ex = mean(p_level_w2_exact, na.rm = TRUE),
                       avg_l_pm = mean(p_level_w2_pm2, na.rm = TRUE),
                       n_t = length(p_trend_w2),
                       n_l_ex = length(p_level_w2_exact),
                       n_l_pm = length(p_level_w2_pm2)
                       ) 

stats <-bind_rows(stat_w1,stat_w2)

stats <- stats %>% mutate(
                          avg_t_std1.96 = 1.96*(c(sqrt((avg_t*(100-avg_t)/n_t)))),
                          avg_t_lower = avg_t-avg_t_std1.96,
                          avg_t_upper = avg_t+avg_t_std1.96,
                          avg_l_ex_std1.96 = 1.96*(c(sqrt((avg_l_ex*(100-avg_l_ex)/n_l_ex)))),
                          avg_l_ex_lower = avg_l_ex-avg_l_ex_std1.96,
                          avg_l_ex_upper = avg_l_ex+avg_l_ex_std1.96,
                          avg_l_pm_std1.96 = 1.96*(c(sqrt((avg_l_pm*(100-avg_l_pm)/n_l_pm)))),
                          avg_l_pm_lower = avg_l_pm-avg_l_pm_std1.96,
                          avg_l_pm_upper = avg_l_pm+avg_l_pm_std1.96
                          )

nonstat_w1 <-df %>%
             filter(stat==0)   %>%
             summarize(avg_t = mean(p_trend_w1, na.rm = TRUE),
                       avg_l_ex = mean(p_level_w1_exact, na.rm = TRUE),
                       avg_l_pm = mean(p_level_w1_pm2, na.rm = TRUE),
                       n_t = length(p_trend_w1),
                       n_l_ex = length(p_level_w1_exact),
                       n_l_pm = length(p_level_w1_pm2)
                       ) %>%
             mutate(days=c("0_pre"))

nonstat_w2 <-df %>%
             filter(stat==0)   %>%
             group_by(days)   %>%
             summarize(avg_t = mean(p_trend_w2, na.rm = TRUE),
                       avg_l_ex = mean(p_level_w2_exact, na.rm = TRUE),
                       avg_l_pm = mean(p_level_w2_pm2, na.rm = TRUE),
                       n_t = length(p_trend_w2),
                       n_l_ex = length(p_level_w2_exact),
                       n_l_pm = length(p_level_w2_pm2)
                       ) 

nonstats <-bind_rows(nonstat_w1,nonstat_w2)

nonstats <- nonstats %>% mutate(
                          avg_t_std1.96 = 1.96*(c(sqrt((avg_t*(100-avg_t)/n_t)))),
                          avg_t_lower = avg_t-avg_t_std1.96,
                          avg_t_upper = avg_t+avg_t_std1.96,
                          avg_l_ex_std1.96 = 1.96*(c(sqrt((avg_l_ex*(100-avg_l_ex)/n_l_ex)))),
                          avg_l_ex_lower = avg_l_ex-avg_l_ex_std1.96,
                          avg_l_ex_upper = avg_l_ex+avg_l_ex_std1.96,
                          avg_l_pm_std1.96 = 1.96*(c(sqrt((avg_l_pm*(100-avg_l_pm)/n_l_pm)))),
                          avg_l_pm_lower = avg_l_pm-avg_l_pm_std1.96,
                          avg_l_pm_upper = avg_l_pm+avg_l_pm_std1.96
                          )

## data for sub plots C1--C3
unemp_below_w1 <-df %>%
             filter(local_unemp==1)   %>%
             group_by(.dots=c("stat"))   %>%
             summarize(avg_r = mean(p_relative_w1, na.rm = TRUE),
                       n_r = length(p_relative_w1)
                       )  %>%
             mutate(days=c("0_pre","0_pre"))
unemp_below_w2 <-df %>%
             filter(local_unemp==1)   %>%
             group_by(.dots=c("stat","days"))   %>%
             summarize(avg_r = mean(p_relative_w2, na.rm = TRUE),
                       n_r = length(p_relative_w2)
                       ) 
unemp_below <- bind_rows(unemp_below_w1,unemp_below_w2)
unemp_below <-  arrange(unemp_below,stat,days)
unemp_below <- unemp_below %>% mutate(
                          avg_r_std1.96 = 1.96*(c(sqrt((avg_r*(100-avg_r)/n_r)))),
                          avg_r_lower = avg_r-avg_r_std1.96,
                          avg_r_upper = avg_r+avg_r_std1.96
                          )

unemp_average_w1 <-df %>%
             filter(local_unemp==2)   %>%
             group_by(.dots=c("stat"))   %>%
             summarize(avg_r = mean(p_relative_w1, na.rm = TRUE),
                       n_r = length(p_relative_w1)
                       )  %>%
              mutate(days=c("0_pre","0_pre"))
unemp_average_w2  <-df %>%
             filter(local_unemp==2)   %>%
             group_by(.dots=c("stat","days"))   %>%
             summarize(avg_r = mean(p_relative_w2, na.rm = TRUE),
                       n_r = length(p_relative_w2)
                       ) 
unemp_average <- bind_rows(unemp_average_w1,unemp_average_w2)
unemp_average <- arrange(unemp_average,stat,days)
unemp_average <- unemp_average %>% mutate(
                          avg_r_std1.96 = 1.96*(c(sqrt((avg_r*(100-avg_r)/n_r)))),
                          avg_r_lower = avg_r-avg_r_std1.96,
                          avg_r_upper = avg_r+avg_r_std1.96
                          )

unemp_above_w1 <-df %>%
             filter(local_unemp==3)   %>%
             group_by(.dots=c("stat"))   %>%
             summarize(avg_r = mean(p_relative_w1, na.rm = TRUE),
                       n_r = length(p_relative_w1)
                       )  %>%
              mutate(days=c("0_pre","0_pre"))
unemp_above_w2  <-df %>%
             filter(local_unemp==3)   %>%
             group_by(.dots=c("stat","days"))   %>%
             summarize(avg_r = mean(p_relative_w2, na.rm = TRUE),
                       n_r = length(p_relative_w2)
                       ) 
unemp_above   <- bind_rows(unemp_above_w1,unemp_above_w2)
unemp_above   <- arrange(unemp_above,stat,days)
unemp_above <- unemp_above %>% mutate(
                          avg_r_std1.96 = 1.96*(c(sqrt((avg_r*(100-avg_r)/n_r)))),
                          avg_r_lower = avg_r-avg_r_std1.96,
                          avg_r_upper = avg_r+avg_r_std1.96
                          )

################################################################## 
################################################################## 
######### APPENDIX B: DESCRIPTIVE STATISTICS, BALANCE, AND ATTRITION       
################################################################## 
################################################################## 

###Table B1: Descriptive Statistics

descriptivdf <- df %>%  select(
                        c_trend_w1,c_trend_w2,c_level_w1_exact,c_level_w2_exact,c_level_w1_pm2,c_level_w2_pm2,c_relative_w1,c_relative_w2,
                        female,age,burg_fear,pol_intr
                        ) 

descriptives<-c(
"DV, wave 1: Trend (pct. correct)",
"DV, wave 2: Trend (pct. correct)",
"DV, wave 1: Level exact (pct. correct)",	
"DV, wave 2: Level exact (pct. correct)",
"DV, wave 1: Level +/- 2 pp (pct. correct)",
"DV, wave 2: Level +/- 2 pp (pct. correct)", 
"DV, wave 1: Relative (pct. correct)",
"DV, wave 2: Relative (pct. correct)",
"Females (share)",		  		
"Age	(years)",	
"Fear of burglary (1-7)", 
"Interest in local politics (1-4)") 

stargazer(descriptivdf,summary=T,covariate.labels = descriptives,
          font.size = "normalsize",title = "Descriptive statistics",
          label = "tab:ds",out = "table_descript.tex",digits = 2)  

#####Table B2: Balance test across treatments, created manually from these analysis:
#females
round(tapply(df$female,df$stat,mean)*100,1)
round(tapply(df$female,df$stat,sd)*100,1)
t.test(df$female~df$stat)

#age
round(tapply(df$age,df$stat,mean),1)
round(tapply(df$age,df$stat,sd),1)
t.test(df$age~df$stat)

#fear
round(tapply(df$burg_fear,df$stat,mean),1)
round(tapply(df$burg_fear,df$stat,sd),1)
t.test(df$burg_fear~df$stat)

#interest in local politics
round(tapply(df$pol_intr,df$stat,mean),1)
round(tapply(df$pol_intr,df$stat,sd),1)
t.test(df$pol_intr~df$stat)

#attrition
100-round(tapply(df_full$w2_resp,df_full$stat,mean)*100,1)
round(tapply(df_full$w2_resp,df_full$stat,sd)*100,1)
t.test(df_full$w2_resp~df_full$stat)

#####Table B3: Balance test across time, created manually from these analysis:

#gender
round(tapply(df$female,df$days,mean)*100,1)
summary(lm(I(female)*100~days, data=df))

#age
round(tapply(df$age,df$days,mean),1)
summary(lm(I(age)*100~days, data=df))

#fear of burglary
round(tapply(df$burg_fear,df$days,mean),1)
summary(lm(burg_fear~days, data=df))

#interest in local politics
round(tapply(df$pol_intr,df$days,mean),1)
summary(lm(pol_intr~days, data=df))

#attrition
100-round(tapply(df_full$w2_resp,df_full$days,mean)*100,1)
summary(lm(I(w2_resp)*100~days, data=df_full))

################################################################## 
################################################################## 
######### APPENDIX C: UNEMPLOYMENT PLACEBO                            
################################################################## 
################################################################## 

#set own path to save Figure as a EPS-file
#postscript("figure3_placebo_outcome.eps", width=12, height=16, horizontal=F, paper="special", pointsize = 20) 

layout.matrix <- matrix(c(1:6), nrow = 6, ncol = 1)
layout(mat = layout.matrix, heights = c(3, 1.4, 1.65, 1.2, 1.2, 1.35)) 

#########A. Correct Response: Declining Trend in Unemployment
par(mar=c(4,3.4,2.5,2.2), xpd=T)
plot(stats$avg_t, axes=F, xlab="", ylab="", xlim=c(1,4), ylim=c(70,85), type="n") 
axis(2,at=c(70,75,80,85),labels=c("70%","75","80","85%"), line=0, cex.axis=1, tck = -.04) # lwd=2, cex.axis=1.5
axis(1,at=c(1:4),las=1.5,labels=c("Pre-intervention","7-12 days after","13-18 days after","19-25 days after"),line=0,cex.axis=1.25,tck=-.04, lwd=1, lwd.ticks=1)
title(main="A. Correct Response: Declining Trend in Unemployment", adj = 0, cex.main=1.5, font.main=1)
legend(y=88,x=2.98,legend=c("Statistics Leaflet",
                         "Non-statistics Leaflet or Placebo"),pch=c(19,21), 
                          cex=1.4,pt.bg=c("black","white"), pt.cex=2)
text(1.15,80,"Leaflet Intervention", font=1, srt=90, cex=1)
segments(1.1,70,1.1,85,lty=2)

#stat treatment
lines(c(1:4)+.025,stats$avg_t)
arrows(c(1:4)+.025,stats$avg_t_lower,c(1:4)+.025,stats$avg_t_upper,length=.10,lwd=1, pch=3, angle=90, code=0) #eller code=0
points(c(1:4)+.025,stats$avg_t,pch=21,col="black",bg="black",cex=2, lwd=1)

#non-stat treatment + placebo
lines(c(1:4),nonstats$avg_t)
arrows(c(1:4),nonstats$avg_t_lower,c(1:4),nonstats$avg_t_upper,length=.10,lwd=1, pch=3, angle=90, code=0) #eller code=0
points(c(1:4),nonstats$avg_t,pch=21,col="black",bg="white",cex=2, lwd=1)

#########B1. Correct Response: FivePercent Unemployment Rate (+/- 2%-points)
par(mar=c(1,3.4,1.25,2.2), xpd=F)
plot(stats$avg_l_pm, axes=F, xlab="", ylab="", xlim=c(1,4), ylim=c(45,60), type="n")
axis(2,at=c(45,50,55,60),labels=c("45%","50","55","60%"), line=0, cex.axis=1, tck = -.09) 
title(main="B1. Correct Response: Five Percent Unemployment Rate (+/- 2% Points)", adj = 0, cex.main=1.5, font.main=1)
segments(1.1,45,1.1,60,lty=2)

#stat treatment
lines(c(1:4)+.025,stats$avg_l_pm)
arrows(c(1:4)+.025,stats$avg_l_pm_lower,c(1:4)+.025,stats$avg_l_pm_upper,length=.10,lwd=1, pch=3, angle=90, code=0) #eller code=0
points(c(1:4)+.025,stats$avg_l_pm,pch=21,col="black",bg="black",cex=2, lwd=1)

#non-stat treatment + placebo
lines(c(1:4),nonstats$avg_l_pm)
arrows(c(1:4),nonstats$avg_l_pm_lower,c(1:4),nonstats$avg_l_pm_upper,length=.10,lwd=1, pch=3, angle=90, code=0) #eller code=0
points(c(1:4),nonstats$avg_l_pm,pch=21,col="black",bg="white",cex=2, lwd=1)

#########B2. Five Percent Unemployment Rate (Exact)
par(mar=c(4,3.4,1.25,2.2), xpd=F)
plot(stats$avg_l_ex, axes=F, xlab="", ylab="", xlim=c(1,4), ylim=c(15,25), type="n") 
axis(2,at=c(15,20,25),labels=c("15%","20","25%"), line=0, cex.axis=1, tck = -.095) 
axis(1,at=c(1:4),las=1.5,labels=c("Pre-intervention","7-12 days after","13-18 days after","19-25 days after"),line=0,cex.axis=1.25,tck=-.09, lwd=1, lwd.ticks=1)
title(main="B2. Five Percent Unemployment Rate (Exact)", adj = 0, cex.main=1.5, font.main=1)
segments(1.1,15,1.1,25,lty=2)

#stat treatment
lines(c(1:4)+.025,stats$avg_l_ex)
arrows(c(1:4)+.025,stats$avg_l_ex_lower,c(1:4)+.025,stats$avg_l_ex_upper,length=.10,lwd=1, pch=3, angle=90, code=0) #eller code=0
points(c(1:4)+.025,stats$avg_l_ex,pch=21,col="black",bg="black",cex=2, lwd=1)

#non-stat treatment + placebo
lines(c(1:4),nonstats$avg_l_ex)
arrows(c(1:4),nonstats$avg_l_ex_lower,c(1:4),nonstats$avg_l_ex_upper,length=.10,lwd=1, pch=3, angle=90, code=0) #eller code=0
points(c(1:4),nonstats$avg_l_ex,pch=21,col="black",bg="white",cex=2, lwd=1)

#########C1. Correct Response: Municipal Unemployment Rate is Above the National Average
par(mar=c(1,3.4,1.25,2.2), xpd=F)
plot(unemp_above$avg_r, axes=F, xlab="", ylab="", xlim=c(1,4), ylim=c(20,40), type="n") 
axis(2,at=c(20,30,40),labels=c("20%","30","40%"), line=0, cex.axis=1, tck = -.10)
title(main="C1. Correct Response: Municipal Unemployment Rate is Above the National Average", adj = 0, cex.main=1.5, font.main=1)
segments(1.1,20,1.1,50,lty=2)

#stat treatment
lines(c(1:4)+.025,unemp_above$avg_r[5:8])
arrows(c(1:4)+.025,unemp_above$avg_r_lower[5:8],c(1:4)+.025,unemp_above$avg_r_upper[5:8],length=.10,lwd=1, pch=3, angle=90, code=0) #eller code=0
points(c(1:4)+.025,unemp_above$avg_r[5:8],pch=21,col="black",bg="black",cex=2, lwd=1)

#non stat treatment
lines(c(1:4),unemp_above$avg_r[1:4])
arrows(c(1:4),unemp_above$avg_r_lower[1:4],c(1:4),unemp_above$avg_r_upper[1:4],length=.10,lwd=1, pch=3, angle=90, code=0) #eller code=0
points(c(1:4),unemp_above$avg_r[1:4],pch=21,col="black",bg="white",cex=2, lwd=1)

#########C2. Municipal Unemployment Rate is at the National Average
par(mar=c(1,3.4,1.25,2.2), xpd=F)
plot(unemp_average$avg_r, axes=F, xlab="", ylab="", xlim=c(1,4), ylim=c(40,60), type="n") 
axis(2,at=c(40,50,60),labels=c("40%","50","60%"), line=0, cex.axis=1, tck = -.10)
title(main="C2. Municipal Unemployment Rate is at the National Average", adj = 0, cex.main=1.5, font.main=1)
segments(1.1,40,1.1,70,lty=2)

#stat treatment
lines(c(1:4)+.025,unemp_average$avg_r[5:8])
arrows(c(1:4)+.025,unemp_average$avg_r_lower[5:8],c(1:4)+.025,unemp_average$avg_r_upper[5:8],length=.10,lwd=1, pch=3, angle=90, code=0) #eller code=0
points(c(1:4)+.025,unemp_average$avg_r[5:8],pch=21,col="black",bg="black",cex=2, lwd=1)

#non stat treatment
lines(c(1:4),unemp_average$avg_r[1:4])
arrows(c(1:4),unemp_average$avg_r_lower[1:4],c(1:4),unemp_average$avg_r_upper[1:4],length=.10,lwd=1, pch=3, angle=90, code=0) #eller code=0
points(c(1:4),unemp_average$avg_r[1:4],pch=21,col="black",bg="white",cex=2, lwd=1)
unemp_below
#########C3. Municipal Unemployment Rate is Below the National Average
par(mar=c(2,3.4,1.25,2.2), xpd=F)
plot(unemp_below$avg_r, axes=F, xlab="", ylab="", xlim=c(1,4), ylim=c(40,60), type="n")
axis(1,at=c(1:4),las=1.5,labels=c("Pre-intervention","7-12 days after","13-18 days after","19-25 days after"),line=0,cex.axis=1.25,tck=-.10, lwd=1, lwd.ticks=1)
axis(2,at=c(40,50,60),labels=c("40%","50","60%"), line=0, cex.axis=1, tck = -.10)
title(main="C3. Municipal Unemployment Rate is Below the National Average", adj = 0, cex.main=1.5, font.main=1)
segments(1.1,20,1.1,60,lty=2)

#stat treatment
lines(c(1:4)+.025,unemp_below$avg_r[5:8])
arrows(c(1:4)+.025,unemp_below$avg_r_lower[5:8],c(1:4)+.025,unemp_below$avg_r_upper[5:8],length=.10,lwd=1, pch=3, angle=90, code=0) #eller code=0
points(c(1:4)+.025,unemp_below$avg_r[5:8],pch=21,col="black",bg="black",cex=2, lwd=1)

#non stat treatment
lines(c(1:4),unemp_below$avg_r[1:4])
arrows(c(1:4),unemp_below$avg_r_lower[1:4],c(1:4),unemp_below$avg_r_upper[1:4],length=.10,lwd=1, pch=3, angle=90, code=0) #eller code=0
points(c(1:4),unemp_below$avg_r[1:4],pch=21,col="black",bg="white",cex=2, lwd=1)

####Y-axis
mtext(text="Percent Correct Responses",side=2,line=-1,outer=TRUE,cex=.8,adj=.13) #bottom
mtext(text="Percent Correct Responses",side=2,line=-1,outer=TRUE,cex=.8,adj=.565) #middle
mtext(text="Percent Correct Responses",side=2,line=-1,outer=TRUE,cex=.8,adj=.935) #top

#dev.off()

################################################################## 
################################################################## 
######### APPENDIX E: AVERAGE TREATMNT EFFECT PLOT                          
################################################################## 
################################################################## 

a_0 <-lm(c_trend_w1~stat, data=df)
a_1 <-lm(c_trend_w2~stat, data=df,subset=days=="1_days7_12")
a_2 <-lm(c_trend_w2~stat, data=df,subset=days=="2_days13_18")
a_3 <-lm(c_trend_w2~stat, data=df,subset=days=="3_days19_25")
a_ate <- c(a_0$coefficients[2],a_1$coefficients[2],a_2$coefficients[2],a_3$coefficients[2])
a_cis <- c(coef(summary(a_0))[, "Std. Error"][2],coef(summary(a_1))[, "Std. Error"][2],
           coef(summary(a_2))[, "Std. Error"][2],coef(summary(a_3))[, "Std. Error"][2])*1.96

b1_0<-lm(c_level_w1_pm2~stat, data=df)
b1_1<-lm(c_level_w2_pm2~stat, data=df,subset=days=="1_days7_12")
b1_2<-lm(c_level_w2_pm2~stat, data=df,subset=days=="2_days13_18")
b1_3<-lm(c_level_w2_pm2~stat, data=df,subset=days=="3_days19_25")

b1_ate <- c(b1_0$coefficients[2],b1_1$coefficients[2],b1_2$coefficients[2],b1_3$coefficients[2])
b1_cis <- c(coef(summary(b1_0))[, "Std. Error"][2],coef(summary(b1_1))[, "Std. Error"][2],
           coef(summary(b1_2))[, "Std. Error"][2],coef(summary(b1_3))[, "Std. Error"][2])*1.96

b2_0<-lm(c_level_w1_exact~stat, data=df)
b2_1<-lm(c_level_w2_exact~stat, data=df,subset=days=="1_days7_12")
b2_2<-lm(c_level_w2_exact~stat, data=df,subset=days=="2_days13_18")
b2_3<-lm(c_level_w2_exact~stat, data=df,subset=days=="3_days19_25")

b2_ate <- c(b2_0$coefficients[2],b2_1$coefficients[2],b2_2$coefficients[2],b2_3$coefficients[2])
b2_cis <- c(coef(summary(b2_0))[, "Std. Error"][2],coef(summary(b2_1))[, "Std. Error"][2],
           coef(summary(b2_2))[, "Std. Error"][2],coef(summary(b2_3))[, "Std. Error"][2])*1.96

c1_0<-lm(c_relative_w1~stat, data=df, subset=local_crime==3)
c1_1<-lm(c_relative_w2~stat, data=df,subset=days=="1_days7_12" & local_crime==3)
c1_2<-lm(c_relative_w2~stat, data=df,subset=days=="2_days13_18" & local_crime==3)
c1_3<-lm(c_relative_w2~stat, data=df,subset=days=="3_days19_25" & local_crime==3)

c1_ate <- c(c1_0$coefficients[2],c1_1$coefficients[2],c1_2$coefficients[2],c1_3$coefficients[2])
c1_cis <- c(coef(summary(c1_0))[, "Std. Error"][2],coef(summary(c1_1))[, "Std. Error"][2],
           coef(summary(c1_2))[, "Std. Error"][2],coef(summary(c1_3))[, "Std. Error"][2])*1.96

c2_0<-lm(c_relative_w1~stat, data=df, subset=local_crime==2)
c2_1<-lm(c_relative_w2~stat, data=df,subset=days=="1_days7_12" & local_crime==2)
c2_2<-lm(c_relative_w2~stat, data=df,subset=days=="2_days13_18" & local_crime==2)
c2_3<-lm(c_relative_w2~stat, data=df,subset=days=="3_days19_25" & local_crime==2)

c2_ate <- c(c2_0$coefficients[2],c2_1$coefficients[2],c2_2$coefficients[2],c2_3$coefficients[2])
c2_cis <- c(coef(summary(c2_0))[, "Std. Error"][2],coef(summary(c2_1))[, "Std. Error"][2],
           coef(summary(c2_2))[, "Std. Error"][2],coef(summary(c2_3))[, "Std. Error"][2])*1.96

c3_0<-lm(c_relative_w1~stat, data=df, subset=local_crime==1)
c3_1<-lm(c_relative_w2~stat, data=df,subset=days=="1_days7_12" & local_crime==1)
c3_2<-lm(c_relative_w2~stat, data=df,subset=days=="2_days13_18" & local_crime==1)
c3_3<-lm(c_relative_w2~stat, data=df,subset=days=="3_days19_25" & local_crime==1)

c3_ate <- c(c3_0$coefficients[2],c3_1$coefficients[2],c3_2$coefficients[2],c3_3$coefficients[2])
c3_cis <- c(coef(summary(c3_0))[, "Std. Error"][2],coef(summary(c3_1))[, "Std. Error"][2],
           coef(summary(c3_2))[, "Std. Error"][2],coef(summary(c3_3))[, "Std. Error"][2])*1.96

#########A. Correct Response: Declining Trend in Burglaries
#set own path to save Figure as a EPS-file
#postscript("figure3_ate.eps", width=12, height=16, horizontal=F, paper="special", pointsize = 20) 

layout.matrix <- matrix(c(1:6), nrow = 6, ncol = 1)
layout(mat = layout.matrix,heights = c(3, 1.4, 1.65, 1.2, 1.2, 1.35)) 

par(mar=c(4,3.4,2.5,2.2), xpd=T)
plot(a_ate, axes=F, xlab="", ylab="", xlim=c(1,4), ylim=c(-5,20), type="n") 
axis(2,at=c(-5,0,5,10,15,20),labels=c("-5pp","0","5","10","15","20pp"), line=0, cex.axis=1, tck = -.04)
axis(1,at=c(1:4),las=1.5,labels=c("Pre-intervention","7-12 days after","13-18 days after","19-25 days after"),line=0,cex.axis=1.25,tck=-.04, lwd=1, lwd.ticks=1)
title(main="A. Correct Response: Declining Trend in Burglaries", adj = 0, cex.main=1.5, font.main=1)
legend(y=25,x=2.98,legend=c("Average treatment effects"),pch=c(19),cex=1.4,pt.bg=c("black"), pt.cex=2)

text(1.15,12,"Leaflet Intervention", font=1, srt=90, cex=1.25)
segments(1.1,-5,1.1,20,lty=2)
segments(.9,0,4,0,lty=2)

lines(c(1,2,3,4),a_ate)
arrows(c(1,2,3,4),a_ate-a_cis,c(1,2,3,4),a_ate+a_cis,length=.10,lwd=1, pch=3, angle=90, code=0) #eller code=0
points(c(1,2,3,4),a_ate,pch=21,col="black",bg="black",cex=1.5, lwd=1)

#########B1. Correct Response: Nine Percent Burglary Rate (+/- 2%-points)
par(mar=c(1,3.4,1.25,2.2), xpd=F)
plot(b1_ate, axes=F, xlab="", ylab="", xlim=c(1,4), ylim=c(-6,9), type="n") 
axis(2,at=c(20,25,30,34),labels=c("20%","25","30","35%"), line=0, cex.axis=1, tck = -.09) 
axis(2,at=c(-6,0,9),labels=c("-6 pp","0","9 pp"), line=0, cex.axis=1, tck = -.08) 
title(main="B1. Correct Response: Nine Percent Burglary Rate (+/- 2% Points)", adj = 0, cex.main=1.5, font.main=1)

segments(1.1,-6,1.1,9,lty=2)
segments(.9,0,4,0,lty=2)

lines(c(1,2,3,4),b1_ate)
arrows(c(1,2,3,4),b1_ate-b1_cis,c(1,2,3,4),b1_ate+b1_cis,length=.10,lwd=1, pch=3, angle=90, code=0) #eller code=0
points(c(1,2,3,4),b1_ate,pch=21,col="black",bg="black",cex=1.5, lwd=1)

#########B2. Nine Percent Burglary Rate (Exact)
par(mar=c(4,3.4,1.25,2.2), xpd=F)
plot(b2_ate, axes=F, xlab="", ylab="", xlim=c(1,4), ylim=c(-2,3), type="n")
axis(2,at=c(-2,0,3),labels=c("-2 pp","0","3 pp"), line=0, cex.axis=1, tck = -.10) 
axis(1,at=c(1:4),las=1.5,labels=c("Pre-intervention","7-12 days after","13-18 days after","19-25 days after"),line=0,cex.axis=1.25,tck=-.09, lwd=1, lwd.ticks=1)
title(main="B2. Nine Percent Burglary Rate (Exact)", adj = 0, cex.main=1.5, font.main=1)

segments(1.1,-2,1.1,3,lty=2)
segments(.9,0,4,0,lty=2)

lines(c(1,2,3,4),b2_ate)
arrows(c(1,2,3,4),b2_ate-b2_cis,c(1,2,3,4),b2_ate+b2_cis,length=.10,lwd=1, pch=3, angle=90, code=0) #eller code=0
points(c(1,2,3,4),b2_ate,pch=21,col="black",bg="black",cex=1.5, lwd=1)

#########C1. Correct Response: Municipal Burglary Rate is Above the National Average
par(mar=c(1,3.4,1.25,2.2), xpd=F)
plot(c1_ate, axes=F, xlab="", ylab="", xlim=c(1,4), ylim=c(-10,15), type="n") 
axis(2,at=c(-10,0,15),labels=c("-10pp","0","15pp"), line=0, cex.axis=1, tck = -.10) 
title(main="C1. Correct Response: Municipal Burglary Rate is Above the National Average", adj = 0, cex.main=1.5, font.main=1)

segments(1.1,-15,1.1,25,lty=2)
segments(.9,0,4,0,lty=2)

lines(c(1,2,3,4),c1_ate)
arrows(c(1,2,3,4),c1_ate-c1_cis,c(1,2,3,4),c1_ate+c1_cis,length=.10,lwd=1, pch=3, angle=90, code=0) #eller code=0
points(c(1,2,3,4),c1_ate,pch=21,col="black",bg="black",cex=1.5, lwd=1)

#########C2. Municipal Burglary Rate is at the National Average
par(mar=c(1,3.4,1.25,2.2), xpd=F)
plot(c2_ate, axes=F, xlab="", ylab="", xlim=c(1,4), ylim=c(-15,10), type="n") # border = NA
axis(2,at=c(-15,0,10),labels=c("-15pp","0","10pp"), line=0, cex.axis=1, tck = -.11) 
title(main="C2. Municipal Burglary Rate is at the National Average", adj = 0, cex.main=1.5, font.main=1)

segments(1.1,-15,1.1,25,lty=2)
segments(.9,0,4,0,lty=2)

lines(c(1,2,3,4),c2_ate)
arrows(c(1,2,3,4),c2_ate-c2_cis,c(1,2,3,4),c2_ate+c2_cis,length=.10,lwd=1, pch=3, angle=90, code=0) #eller code=0
points(c(1,2,3,4),c2_ate,pch=21,col="black",bg="black",cex=1.5, lwd=1)

#########C3. Municipal Burglary Rate is Below the National Average
par(mar=c(2,3.4,1.25,2.2), xpd=F)
plot(c3_ate, axes=F, xlab="", ylab="", xlim=c(1,4), ylim=c(-5,30), type="n") 
axis(1,at=c(1:4),las=1.5,labels=c("Pre-intervention","7-12 days after","13-18 days after","19-25 days after"),line=0,cex.axis=1.25,tck=-.10, lwd=1, lwd.ticks=1)
axis(2,at=c(-5,0,30),labels=c("-5pp","0","30pp"), line=0, cex.axis=1, tck = -.10) 
title(main="C3. Municipal Burglary Rate is Below the National Average", adj = 0, cex.main=1.5, font.main=1)

segments(1.1,-15,1.1,25,lty=2)
segments(.9,0,4,0,lty=2)

lines(c(1,2,3,4),c3_ate)
arrows(c(1,2,3,4),c3_ate-c3_cis,c(1,2,3,4),c3_ate+c3_cis,length=.10,lwd=1, pch=3, angle=90, code=0) #eller code=0
points(c(1,2,3,4),c3_ate,pch=21,col="black",bg="black",cex=1.5, lwd=1)

####Y-axis
mtext(text="%-point Change in Correct Responses",side=2,line=-1,outer=TRUE,cex=.8,adj=.11) #bottom y-axis
mtext(text="%-point Change in Correct Responses",side=2,line=-1,outer=TRUE,cex=.8,adj=.565) #middle y-axis
mtext(text="%-point Change in Correct Responses",side=2,line=-1,outer=TRUE,cex=.8,adj=.945) #top y-axis

#dev.off()


###Table D1: Average Treatment Effects and Treatment effects on the Treated (TOT), manually made from these analysis:

#Trend, ATE, lower and upper 95%-CI, and TOT 
round(a_ate,1); round(a_ate,1)-round(a_cis,1); round(a_ate,1)+round(a_cis,1); round(a_ate/.46,1)

#Level (+/- 2), ATE, lower and upper 95%-CI, and TOT 
round(b1_ate,1); round(b1_ate,1)-round(b1_cis,1); round(b1_ate,1)+round(b1_cis,1); round(b1_ate/.46,1)

#Level (exact), ATE, lower and upper 95%-CI, and TOT 
round(b2_ate,1); round(b2_ate,1)-round(b2_cis,1); round(b2_ate,1)+round(b2_cis,1); round(b2_ate/.46,1)

#Relative (above), ATE, lower and upper 95%-CI, and TOT 
round(c1_ate,1); round(c1_ate,1)-round(c1_cis,1); round(c1_ate,1)+round(c1_cis,1); round(c1_ate/.46,1)

#Relative (average), ATE, lower and upper 95%-CI, and TOT 
round(c2_ate,1); round(c2_ate,1)-round(c2_cis,1); round(c2_ate,1)+round(c2_cis,1); round(c2_ate/.46,1)

#Relative (below), ATE, lower and upper 95%-CI, and TOT 
round(c3_ate,1); round(c3_ate,1)-round(c3_cis,1); round(c3_ate,1)+round(c3_cis,1); round(c3_ate/.46,1)


####################################################################################################
####################################################################################################
#######APPENDIX E: RECREATING THE RESULTS USING LOGISTIC REGRESSION MODELS                             
####################################################################################################
####################################################################################################

####Table E1: pre treatment survey
m_trends_w0 <- glm(I(c_trend_w1/100)~stat+female+age+factor(education)+factor(income)+factor(region),family=binomial(link="logit"), data=df)
m_levels_2pp_w0 <- glm(I(c_level_w1_pm2/100)~stat+female+age+factor(education)+factor(income)+factor(region),family=binomial(link="logit"), data=df)
m_levels_exact_w0 <- glm(I(c_level_w1_exact/100)~stat+female+age+factor(education)+factor(income)+factor(region),family=binomial(link="logit"), data=df)
m_below_w0 <- glm(I(c_relative_w1/100)~stat+female+age+factor(education)+factor(income)+factor(region),family=binomial(link="logit"),subset=local_crime==3,data=df)
m_average_w0 <- glm(I(c_relative_w1/100)~stat+female+age+factor(education)+factor(income)+factor(region),family=binomial(link="logit"),subset=local_crime==2,data=df)
m_above_w0 <- glm(I(c_relative_w1/100)~stat+female+age+factor(education)+factor(income)+factor(region),family=binomial(link="logit"),subset=local_crime==1,data=df)

logits_w0 = stargazer(m_trends_w0,m_levels_2pp_w0,m_levels_exact_w0,m_above_w0,m_average_w0,m_below_w0,   
    summary = F, digits = 2, 
    title = "Pre-intervention: Controlling for pre-treatment variables (Logistic regression)",
    style = "qje",
    no.space = TRUE,
    font.size = "footnotesize",
    covariate.labels = c(
      "Statistics leaftlet",
      "Female",
      "Age (years)",
      "Vocational training (ref: high school)",
      "--Short-cycle tertiary",
      "--Medium-cycle tertiary",
      "--Long-cycle tertiary",
      "--Other",
      "Income: 150K-249K (ref: $<$150K)",
      "--250K-349K",
      "--350K-499K",
      "--500K-599K",
      "--600K-699K",
      "--700K-799K",
      "--800K-",
      "--Do not want to report",
      "Region: M. Jutland (ref: n. Jutland)",
      "--Southern Denmark",
      "--Zealand",
      "--Capital",
      "Intercept"),
    column_labels = c("Trend","Level: +/-2", "Level: Exact", "Above avg.", "avg.", "Below avg."),
    dep.var.labels = c("","","",""),
   star.cutoffs=c(0.05, 0.01),
    notes="Logit coefficients with standard errors. $^{*}$p$<$.05; $^{**}$p$<$.01",
    notes.append=F,
    column_sep.width = "-5pt",
    out="logits_w0.tex",
    label="logits_w0"
)

#####Table E2: days 7--12
m_trends_w1 <- glm(I(c_trend_w2/100)~stat+female+age+factor(education)+factor(income)+factor(region),family=binomial(link="logit"),subset=days=="1_days7_12",data=df)
m_levels_2pp_w1 <- glm(I(c_level_w2_pm2/100)~stat+female+age+factor(education)+factor(income)+factor(region),family=binomial(link="logit"),subset=days=="1_days7_12",data=df)
m_levels_exact_w1 <- glm(I(c_level_w2_exact/100)~stat+female+age+factor(education)+factor(income)+factor(region),family=binomial(link="logit"),subset=days=="1_days7_12",data=df)
m_below_w1 <- glm(I(c_relative_w2/100)~stat+female+age+factor(education)+factor(income)+factor(region),family=binomial(link="logit"),subset=local_crime==3 & days=="1_days7_12",data=df)
m_average_w1 <- glm(I(c_relative_w2/100)~stat+female+age+factor(education)+factor(income)+factor(region),family=binomial(link="logit"),subset=local_crime==2 & days=="1_days7_12",data=df)
m_above_w1 <- glm(I(c_relative_w2/100)~stat+female+age+factor(education)+factor(income)+factor(region),family=binomial(link="logit"),subset=local_crime==1 & days=="1_days7_12",data=df)

logits_w1 = stargazer(m_trends_w1,m_levels_2pp_w1,m_levels_exact_w1,m_above_w1,m_average_w1,m_below_w1,   
    summary = F, digits = 2, 
    title = "Days 7-12: Controlling for pre-treatment variables (Logistic regression)",
    style = "qje",
    no.space = TRUE,
    font.size = "footnotesize",
    covariate.labels = c(
      "Statistics leaftlet",
      "Female",
      "Age (years)",
      "Vocational training (ref: high school)",
      "--Short-cycle tertiary",
      "--Medium-cycle tertiary",
      "--Long-cycle tertiary",
      "--Other",
      "Income: 150K-249K (ref: $<$150K)",
      "--250K-349K",
      "--350K-499K",
      "--500K-599K",
      "--600K-699K",
      "--700K-799K",
      "--800K-",
      "--Do not want to report",
      "Region: M. Jutland (ref: n. Jutland)",
      "--Southern Denmark",
      "--Zealand",
      "--Capital",
      "Intercept"),
    column_labels = c("Trend","Level: +/-2", "Level: Exact", "Above avg.", "avg.", "Below avg."),
    dep.var.labels = c("","","",""),
   star.cutoffs=c(0.05, 0.01),
    notes="Logit coefficients with standard errors. $^{*}$p$<$.05; $^{**}$p$<$.01",
    notes.append=F,
    column_sep.width = "-5pt",
    out="logits_w1.tex",
    label="logits_w1"
)

#####Table E3: days 13--18
m_trends_w2 <- glm(I(c_trend_w2/100)~stat+female+age+factor(education)+factor(income)+factor(region),family=binomial(link="logit"),subset=days=="2_days13_18",data=df)
m_levels_2pp_w2 <- glm(I(c_level_w2_pm2/100)~stat+female+age+factor(education)+factor(income)+factor(region),family=binomial(link="logit"),subset=days=="2_days13_18",data=df)
m_levels_exact_w2 <- glm(I(c_level_w2_exact/100)~stat+female+age+factor(education)+factor(income)+factor(region),family=binomial(link="logit"),subset=days=="2_days13_18",data=df)
m_below_w2 <- glm(I(c_relative_w2/100)~stat+female+age+factor(education)+factor(income)+factor(region),family=binomial(link="logit"),subset=local_crime==3 & days=="2_days13_18",data=df)
m_average_w2 <- glm(I(c_relative_w2/100)~stat+female+age+factor(education)+factor(income)+factor(region),family=binomial(link="logit"),subset=local_crime==2 & days=="2_days13_18",data=df)
m_above_w2 <- glm(I(c_relative_w2/100)~stat+female+age+factor(education)+factor(income)+factor(region),family=binomial(link="logit"),subset=local_crime==1 & days=="2_days13_18",data=df)

logits_w2 = stargazer(m_trends_w2,m_levels_2pp_w2,m_levels_exact_w2,m_above_w2,m_average_w2,m_below_w2,   
    summary = F, digits = 2, 
    title = "Days 13-18: Controlling for pre-treatment variables (Logistic regression)",
    style = "qje",
    no.space = TRUE,
    font.size = "footnotesize",
    covariate.labels = c(
      "Statistics leaftlet",
      "Female",
      "Age (years)",
      "Vocational training (ref: high school)",
      "--Short-cycle tertiary",
      "--Medium-cycle tertiary",
      "--Long-cycle tertiary",
      "--Other",
      "Income: 150K-249K (ref: $<$150K)",
      "--250K-349K",
      "--350K-499K",
      "--500K-599K",
      "--600K-699K",
      "--700K-799K",
      "--800K-",
      "--Do not want to report",
      "Region: M. Jutland (ref: n. Jutland)",
      "--Southern Denmark",
      "--Zealand",
      "--Capital",
      "Intercept"),
    column_labels = c("Trend","Level: +/-2", "Level: Exact", "Above avg.", "avg.", "Below avg."),
    dep.var.labels = c("","","",""),
   star.cutoffs=c(0.05, 0.01),
    notes="Logit coefficients with standard errors. $^{*}$p$<$.05; $^{**}$p$<$.01",
    notes.append=F,
    column_sep.width = "-5pt",
    out="logits_w2.tex",
    label="logits_w2"
)

#####Table E3: days 19--25
m_trends_w3 <- glm(I(c_trend_w2/100)~stat+female+age+factor(education)+factor(income)+factor(region),family=binomial(link="logit"),subset=days=="3_days19_25",data=df)
m_levels_2pp_w3 <- glm(I(c_level_w2_pm2/100)~stat+female+age+factor(education)+factor(income)+factor(region),family=binomial(link="logit"),subset=days=="3_days19_25",data=df)
m_levels_exact_w3 <- glm(I(c_level_w2_exact/100)~stat+female+age+factor(education)+factor(income)+factor(region),family=binomial(link="logit"),subset=days=="3_days19_25",data=df)
m_below_w3 <- glm(I(c_relative_w2/100)~stat+female+age+factor(education)+factor(income)+factor(region),family=binomial(link="logit"),subset=local_crime==3 & days=="3_days19_25",data=df)
m_average_w3 <- glm(I(c_relative_w2/100)~stat+female+age+factor(education)+factor(income)+factor(region),family=binomial(link="logit"),subset=local_crime==2 & days=="3_days19_25",data=df)
m_above_w3 <- glm(I(c_relative_w2/100)~stat+female+age+factor(education)+factor(income)+factor(region),family=binomial(link="logit"),subset=local_crime==1 & days=="3_days19_25",data=df)

logits_w3 = stargazer(m_trends_w3,m_levels_2pp_w3,m_levels_exact_w3,m_above_w3,m_average_w3,m_below_w3,   
    summary = F, digits = 2, 
    title = "Days 19-25: Controlling for pre-treatment variables (Logistic regression)",
    style = "qje",
    no.space = TRUE,
    font.size = "footnotesize",
    covariate.labels = c(
      "Statistics leaftlet",
      "Female",
      "Age (years)",
      "Vocational training (ref: high school)",
      "--Short-cycle tertiary",
      "--Medium-cycle tertiary",
      "--Long-cycle tertiary",
      "--Other",
      "Income: 150K-249K (ref: $<$150K)",
      "--250K-349K",
      "--350K-499K",
      "--500K-599K",
      "--600K-699K",
      "--700K-799K",
      "--800K-",
      "--Do not want to report",
      "Region: M. Jutland (ref: n. Jutland)",
      "--Southern Denmark",
      "--Zealand",
      "--Capital",
      "Intercept"),
    column_labels = c("Trend","Level: +/-2", "Level: Exact", "Above avg.", "avg.", "Below avg."),
    dep.var.labels = c("","","",""),
   star.cutoffs=c(0.05, 0.01),
    notes="Logit coefficients with standard errors. $^{*}$p$<$.05; $^{**}$p$<$.01",
    notes.append=F,
    column_sep.width = "-5pt",
    out="logits_w3.tex",
    label="logits_w3"
)

###################################################################################
###################################################################################
######APPPENDIX F: INDIVIDUAL FLYER EFFECTS FLYER EFFECTS
###################################################################################
###################################################################################

#stat
stat_w1 <-df %>% filter(stat==1)   %>%
             summarize(avg_t = mean(c_trend_w1, na.rm = TRUE),
                       n_t = length(c_trend_w1)
                       )     %>%
             mutate(days=c("0_pre"))
stat_w2 <-df %>% filter(stat==1)  %>% group_by(days)   %>%
             summarize(avg_t = mean(c_trend_w2, na.rm = TRUE),
                       n_t = length(c_trend_w2)) 
stats <-bind_rows(stat_w1,stat_w2)
stats <- stats %>% mutate(avg_t.std1.96 = 1.96*(c(sqrt((avg_t*(100-avg_t)/n_t)))),
                          avg_t_lower = avg_t-avg_t.std1.96,
                          avg_t_upper = avg_t+avg_t.std1.96)

#nonstat without placebo
nonstat_w1 <-df %>% filter(nonstat_t==1)   %>%
             summarize(avg_t = mean(c_trend_w1, na.rm = TRUE),
                       n_t = length(c_trend_w1)
                       )     %>%
             mutate(days=c("0_pre"))
nonstat_w2 <-df %>% filter(nonstat_t==1)  %>% group_by(days)   %>%
             summarize(avg_t = mean(c_trend_w2, na.rm = TRUE),
                       n_t = length(c_trend_w2)) 
nonstats <-bind_rows(nonstat_w1,nonstat_w2)
nonstats <- nonstats %>% mutate(avg_t.std1.96 = 1.96*(c(sqrt((avg_t*(100-avg_t)/n_t)))),
                          avg_t_lower = avg_t-avg_t.std1.96,
                          avg_t_upper = avg_t+avg_t.std1.96)

#placebo
placebo_w1 <-df %>% filter(placebo==1)   %>%
             summarize(avg_t = mean(c_trend_w1, na.rm = TRUE),
                       n_t = length(c_trend_w1)
                       )     %>%
             mutate(days=c("0_pre"))
placebo_w2 <-df %>% filter(placebo==1)  %>% group_by(days)   %>%
             summarize(avg_t = mean(c_trend_w2, na.rm = TRUE),
                       n_t = length(c_trend_w2)) 
placebo <-bind_rows(placebo_w1,placebo_w2)
placebo <- placebo %>% mutate(avg_t.std1.96 = 1.96*(c(sqrt((avg_t*(100-avg_t)/n_t)))),
                          avg_t_lower = avg_t-avg_t.std1.96,
                          avg_t_upper = avg_t+avg_t.std1.96)

#########F1
#set own path to save Figure as a EPS-file
#postscript("trends_w_placebo.eps", width=14, height=6, horizontal=F, paper="special", pointsize = 16) 

par(mar=c(2,3.5,1,2.2), xpd=F)

plot(stats$avg_t, axes=F, xlab="", ylab="", xlim=c(1,4), ylim=c(35,65), type="n") 
axis(2,at=c(35,40,45,50,55,60,65),labels=c("35%","40","45","50","55","60","65%"), line=0, cex.axis=1.25, tck = -.04) # lwd=2, cex.axis=1.5
axis(1,at=c(1:4),las=1.5,labels=c("Pre-intervention","7-12 days after","13-18 days after","19-25 days after"),line=0,cex.axis=1.25,tck=-.04, lwd=1, lwd.ticks=1)
title(main="Correct Response: Declining Trend in Burglaries", adj = 0, cex.main=1.5, font.main=1)
legend(y=66,x=3.15,legend=c("Statistics Leaflet",
                         "Non-statistics Leaflet",
                         "Placebo"),pch=c(21,21,4), 
                          cex=1.4,pt.bg=c("black","white","black"), pt.cex=2)
text(1.15,55,"Leaflet Intervention", font=1, srt=90, cex=1.25)
segments(1.1,35,1.1,65,lty=2)
mtext("Percent Correct Responses", side=2, line=2.5, cex=1.25)

#stat treatment
lines(c(1:4)+.025,stats$avg_t)
arrows(c(1:4)+.025,stats$avg_t_lower,c(1:4)+.025,stats$avg_t_upper,length=.10,lwd=1, pch=3, angle=90, code=0) 
points(c(1:4)+.025,stats$avg_t,pch=21,col="black",bg="black",cex=2, lwd=1)

#non-stat treatment
lines(c(1:4),nonstats$avg_t)
arrows(c(1:4),nonstats$avg_t_lower,c(1:4),nonstats$avg_t_upper,length=.10,lwd=1, pch=3, angle=90, code=0) 
points(c(1:4),nonstats$avg_t,pch=21,col="black",bg="white",cex=2, lwd=1)

#placebo
lines(c(1:4)-.025,placebo$avg_t)
arrows(c(1:4)-.025,placebo$avg_t_lower,c(1:4)-.025,placebo$avg_t_upper,length=.10,lwd=1, pch=3, angle=90, code=0) 
points(c(1:4)-.025,placebo$avg_t,pch=4,col="black",cex=2, lwd=1)
#dev.off()

#########################F2

pre.means0 <- as.vector(tapply(df$c_trend_w1,df$leaflet,mean))
pre.means <-c(pre.means0[1],pre.means0[2],pre.means0[5],pre.means0[3],pre.means0[4],pre.means0[6],pre.means0[7])
pre.n0     <- table(df$leaflet)
pre.n     <-c(pre.n0[1],pre.n0[2],pre.n0[5],pre.n0[3],pre.n0[4],pre.n0[6],pre.n0[7])
post.means0 <- as.vector(tapply(df$c_trend_w2,df$leaflet,mean))
post.means <-c(post.means0[1],post.means0[2],post.means0[5],post.means0[3],post.means0[4],post.means0[6],post.means0[7])
post.n0 <- table(df$leaflet)
post.n <- c(pre.n0[1],pre.n0[2],pre.n0[5],pre.n0[3],pre.n0[4],pre.n0[6],pre.n0[7])

#set own path to save Figure as a EPS-file
#postscript("trends_flyer.eps",width=14, height=6, horizontal=F, paper="special", pointsize = 16)

par(mar=c(2.5,5.25,1,0), xpd=F)
plot(pre.means,pch=16,ylim=c(35, 60),xlim=c(1, 13.5), xlab="",ylab="",axes=F, col=c(NA, NA, NA), type="n")

axis(1,at=c(1.125,3.125,5.125,7.125,9.125,11.125,13.125),las=1.5, 
     labels=NA,cex.axis=1.5, tck = -.03, lwd=1, lwd.ticks=1, line=0)
axis(1,at=c(1.125,3.125,5.125,7.125,9.125,11.125,13.125),las=1.5, 
     labels=c("Statistics 1","Statistics 2", "Statistics 3",
              "Non-statistics 1", "Non-statistics 2"
              ,"Non-statistics 3", "Placebo"),
     cex.axis=1, tck =0, lwd=0, lwd.ticks=0, line=0)
axis(2, at=c(35,40,45,50,55,60),labels=c("35%","40","45","50","55","60%"), line=1.75, cex.axis=1.25, tck = -.04) # lwd=2, cex.axis=1.5
mtext("Percent Correct Responses", side=2, line=4.2, cex=1.25)

title(main="Correct Response: Declining Trend in Burglaries", adj = 0, cex.main=1.5, font.main=1)
abline(v=6.125,lty=2)
text(4.8,59,"Statistics leaflets")
text(7.75,59,"Non-statistics leaflets")

std1.96.pre<-1.96*(c(sqrt((pre.means*(100-pre.means)/pre.n))))
std1.96.post<-1.96*(c(sqrt((post.means*(100-post.means)/post.n))))

lower <- pre.means-std1.96.pre
upper <- pre.means+std1.96.pre
arrows(c(1,3,5,7,9,11,13),lower,c(1,3,5,7,9,11,13),upper,length=.10,lwd=1, pch=3, angle=90, code=0) #eller code=0

lower <- post.means-std1.96.post
upper <- post.means+std1.96.post
arrows(c(1.25,3.25,5.25,7.25,9.25,11.25,13.25),lower,c(1.25,3.25,5.25,7.25,9.25,11.25,13.25),upper,length=.10,lwd=1, pch=3, angle=90, code=0) #eller code=0

points(c(1,3,5,7,9,11,13),pre.means,pch=21,col="black",bg="white",cex=2, lwd=1)
points(c(1.25,3.25,5.25,7.25,9.25,11.25,13.25),post.means,pch=19,col="black",bg="white",cex=2, lwd=1)

legend(y=60,x=11,legend=c("Pre-treatment","Post-treatment"),pch=c(21,19), cex=1.4,col=c("black"),bg=c("white","black"), pt.cex=2.0)
#dev.off()

##############################################################
###############################################################
#APPENDIX G: EFFECTS CONDITIONAL ON INTEREST IN LOCAL POLITICS            
##############################################################
##############################################################

##### Correct Response: Declining Trend in Burglaries -- given different levels of political interest

#set own path to save Figure as a EPS-file
#postscript("polinterest.eps",width=14, height=8, horizontal=F, paper="special", pointsize = 20) 

a_0_h <-lm(c_trend_w1~stat, data=df,subset=pol_intr<=2)
a_1_h <-lm(c_trend_w2~stat, data=df,subset=days=="1_days7_12" & pol_intr<=2)
a_2_h <-lm(c_trend_w2~stat, data=df,subset=days=="2_days13_18" & pol_intr<=2)
a_3_h <-lm(c_trend_w2~stat, data=df,subset=days=="3_days19_25" & pol_intr<=2)
a_ate_h <- c(a_0_h$coefficients[2],a_1_h$coefficients[2],a_2_h$coefficients[2],a_3_h$coefficients[2])
a_cis_h <- c(coef(summary(a_0_h))[, "Std. Error"][2],coef(summary(a_1_h))[, "Std. Error"][2],
           coef(summary(a_2_h))[, "Std. Error"][2],coef(summary(a_3_h))[, "Std. Error"][2])*1.96

a_0_l <-lm(c_trend_w1~stat, data=df,subset=pol_intr>2)
a_1_l <-lm(c_trend_w2~stat, data=df,subset=days=="1_days7_12" & pol_intr>2)
a_2_l <-lm(c_trend_w2~stat, data=df,subset=days=="2_days13_18" & pol_intr>2)
a_3_l <-lm(c_trend_w2~stat, data=df,subset=days=="3_days19_25" & pol_intr>2)
a_ate_l <- c(a_0_l$coefficients[2],a_1_l$coefficients[2],a_2_l$coefficients[2],a_3_l$coefficients[2])
a_cis_l <- c(coef(summary(a_0_l))[, "Std. Error"][2],coef(summary(a_1_l))[, "Std. Error"][2],
           coef(summary(a_2_l))[, "Std. Error"][2],coef(summary(a_3_l))[, "Std. Error"][2])*1.96

par(mar=c(2,3.4,2.5,3), xpd=T)
plot(a_ate_h, axes=F, xlab="", ylab="", xlim=c(1,4), ylim=c(-5,25), type="n") 
axis(2,at=c(-5,0,5,10,15,20,25),labels=c("-5pp","0","5","10","15","20","25pp"), line=0, cex.axis=1, tck = -.03)
axis(1,at=c(1:4),las=1.5,labels=c("Pre-intervention","7-12 days after","13-18 days after","19-25 days after"),line=0,cex.axis=1,tck=-.03, lwd=1, lwd.ticks=1)
title(main="Correct Response: Declining Trend in Burglaries", adj = 0, cex.main=1.25, font.main=1)
legend(y=26.5,x=2.8,legend=c("ATE for those HIGH in political interest", "ATE for those LOW in political interest"),pch=c(21,21),cex=1,pt.bg=c("black","white"), pt.cex=2)
mtext("%-point Change in Correct Responses", side=2, line=2, cex=1)

text(1.15,17.5,"Leaflet Intervention", font=1, srt=90, cex=1)
segments(1.1,-5,1.1,25,lty=2)
segments(.9,0,4,0,lty=2)

lines(c(1,2,3,4),a_ate_h)
lines(c(1,2,3,4)+.025,a_ate_l)
arrows(c(1,2,3,4),a_ate_h-a_cis_h,c(1,2,3,4),a_ate_h+a_cis_h,length=.10,lwd=1, pch=3, angle=90, code=0) #eller code=0
arrows(c(1,2,3,4)+.025,a_ate_l-a_cis_l,c(1,2,3,4)+.025,a_ate_l+a_cis_l,length=.10,lwd=1, pch=3, angle=90, code=0) #eller code=0
points(c(1,2,3,4),a_ate_h,pch=21,col="black",bg="black",cex=1.5, lwd=1)
points(c(1,2,3,4)+.025,a_ate_l,pch=21,col="black",bg="white",cex=1.5, lwd=1)

#dev.off()

#differences in treatment effects for those high and low in political interest
pre_int<-lm(c_trend_w1~stat*I(pol_intr<=2),data=df)
days7_12<-lm(c_trend_w2~stat*I(pol_intr<=2),data=df,subset=days=="1_days7_12")
days13_18<-lm(c_trend_w2~stat*I(pol_intr<=2),data=df,subset=days=="2_days13_18")
days19_25<-lm(c_trend_w2~stat*I(pol_intr<=2),data=df,subset=days=="3_days19_25")

polinterest = stargazer(pre_int,days7_12,days13_18,days19_25,   
    summary = F, digits = 2, 
    title = "Declining Trend in Burglaries (Interaction with Level of Political Interest)",
    style = "qje",
    no.space = TRUE,
    font.size = "footnotesize",
    covariate.labels = c(
      "Statistics leaftlet",
      "High political interest",
      "Interaction",
      "Intercept"),
    column_labels = c("Pre intervention","After 7 to 12 days","After 13 to 18 days","After 19 to 25 days"),
    dep.var.labels = c("","","",""),
   star.cutoffs=c(0.05, 0.01),
    notes="OLS coefficients with standard errors. $^{*}$p$<$.05; $^{**}$p$<$.01",
    notes.append=F,
    column_sep.width = "-5pt",
    out="polinterest.tex",
    label="polinterest"
)

##############################################################
###############################################################
#THE END
##############################################################
##############################################################