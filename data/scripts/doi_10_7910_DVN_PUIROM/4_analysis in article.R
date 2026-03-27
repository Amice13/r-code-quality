#####################################################################################################################################
#####################################################################################################################################
# Replication File: "Reducing Bias in Citizens' Perception of Crime Rates: 															#
# Evidence from a Field Experiment on Burglary Prevalence" 																			#
# Authors: Martin Vinæs Larsen & Asmus Leth Olsen                                                                                   #
# The Journal of Politics                                                                                                           #
#####################################################################################################################################
#####################################################################################################################################

# Run the code in the R file "3_data management" before running this file.   

################################################################## 
################################################################## 
######### SUMMARIZING DATA FOR FRIGURE 3 
################################################################## 
################################################################## 

#data for sub plots A, B1, B2.
stat_w1 <-df %>%
             filter(stat==1)   %>%
             summarize(avg_t = mean(c_trend_w1, na.rm = TRUE),
                       avg_l_ex = mean(c_level_w1_exact, na.rm = TRUE),
                       avg_l_pm = mean(c_level_w1_pm2, na.rm = TRUE),
                       n_t = length(c_trend_w1),
                       n_l_ex = length(c_level_w1_exact),
                       n_l_pm = length(c_level_w1_pm2)
                       )     %>%
             mutate(days=c("0_pre"))
stat_w2 <-df %>%
             filter(stat==1)   %>%
             group_by(days)   %>%
             summarize(avg_t = mean(c_trend_w2, na.rm = TRUE),
                       avg_l_ex = mean(c_level_w2_exact, na.rm = TRUE),
                       avg_l_pm = mean(c_level_w2_pm2, na.rm = TRUE),
                       n_t = length(c_trend_w2),
                       n_l_ex = length(c_level_w2_exact),
                       n_l_pm = length(c_level_w2_pm2)
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
             summarize(avg_t = mean(c_trend_w1, na.rm = TRUE),
                       avg_l_ex = mean(c_level_w1_exact, na.rm = TRUE),
                       avg_l_pm = mean(c_level_w1_pm2, na.rm = TRUE),
                       n_t = length(c_trend_w1),
                       n_l_ex = length(c_level_w1_exact),
                       n_l_pm = length(c_level_w1_pm2)
                       ) %>%
             mutate(days=c("0_pre"))
nonstat_w2 <-df %>%
             filter(stat==0)   %>%
             group_by(days)   %>%
             summarize(avg_t = mean(c_trend_w2, na.rm = TRUE),
                       avg_l_ex = mean(c_level_w2_exact, na.rm = TRUE),
                       avg_l_pm = mean(c_level_w2_pm2, na.rm = TRUE),
                       n_t = length(c_trend_w2),
                       n_l_ex = length(c_level_w2_exact),
                       n_l_pm = length(c_level_w2_pm2)
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
crime_below_w1 <-df %>%
             filter(local_crime==1)   %>%
             group_by(.dots=c("stat"))   %>%
             summarize(avg_r = mean(c_relative_w1, na.rm = TRUE),
                       n_r = length(c_relative_w1)
                       )  %>%
             mutate(days=c("0_pre","0_pre"))
crime_below_w2 <-df %>%
             filter(local_crime==1)   %>%
             group_by(.dots=c("stat","days"))   %>%
             summarize(avg_r = mean(c_relative_w2, na.rm = TRUE),
                       n_r = length(c_relative_w2)
                       ) 
crime_below <- bind_rows(crime_below_w1,crime_below_w2)
crime_below <- arrange(crime_below,stat,days)
crime_below <- crime_below %>% mutate(
                          avg_r_std1.96 = 1.96*(c(sqrt((avg_r*(100-avg_r)/n_r)))),
                          avg_r_lower = avg_r-avg_r_std1.96,
                          avg_r_upper = avg_r+avg_r_std1.96
                          )
crime_average_w1 <-df %>%
             filter(local_crime==2)   %>%
             group_by(.dots=c("stat"))   %>%
             summarize(avg_r = mean(c_relative_w1, na.rm = TRUE),
                       n_r = length(c_relative_w1)
                       )  %>%
             mutate(days=c("0_pre","0_pre"))
crime_average_w2  <-df %>%
             filter(local_crime==2)   %>%
             group_by(.dots=c("stat","days"))   %>%
             summarize(avg_r = mean(c_relative_w2, na.rm = TRUE),
                       n_r = length(c_relative_w2)
                       ) 
crime_average <- bind_rows(crime_average_w1,crime_average_w2)
crime_average <- arrange(crime_average,stat,days)
crime_average <- crime_average %>% mutate(
                          avg_r_std1.96 = 1.96*(c(sqrt((avg_r*(100-avg_r)/n_r)))),
                          avg_r_lower = avg_r-avg_r_std1.96,
                          avg_r_upper = avg_r+avg_r_std1.96
                          )
crime_above_w1 <-df %>%
             filter(local_crime==3)   %>%
             group_by(.dots=c("stat"))   %>%
             summarize(avg_r = mean(c_relative_w1, na.rm = TRUE),
                       n_r = length(c_relative_w1)
                       )  %>%
             mutate(days=c("0_pre","0_pre"))
crime_above_w2  <-df %>%
             filter(local_crime==3)   %>%
             group_by(.dots=c("stat","days"))   %>%
             summarize(avg_r = mean(c_relative_w2, na.rm = TRUE),
                       n_r = length(c_relative_w2)
                       ) 
crime_above   <- bind_rows(crime_above_w1,crime_above_w2)
crime_above   <- arrange(crime_above,stat,days)
crime_above <- crime_above %>% mutate(
                          avg_r_std1.96 = 1.96*(c(sqrt((avg_r*(100-avg_r)/n_r)))),
                          avg_r_lower = avg_r-avg_r_std1.96,
                          avg_r_upper = avg_r+avg_r_std1.96
                          )

################################################################## 
################################################################## 
######### FACTS REPORTED IN THE MANUSCRIPT
################################################################## 
################################################################## 

#Participants in wave 1
length(df_full$id)

#mininum age of 30 years or more
min(df$age)

#subjects getting the statistical leaftlet (treatment)
sum(df_full$stat)
sum(df$stat)
round(sum(df$stat)/sum(df_full$stat)*100,1)
round(mean(df$stat)*100,0)

#subjects getting a leaflet on burglaries but with no statistical information on burglary rates
sum(df_full$nonstat_t)
sum(df$nonstat_t)
round(sum(df$nonstat_t)/sum(df_full$nonstat_t)*100,1)
round(mean(df$nonstat_t)*100,0)

#subjects getting a placebo leaflet unrelated to the topic of burglaries
sum(df_full$placebo)
sum(df$placebo)
round(sum(df$placebo)/sum(df_full$placebo)*100,1)
round(mean(df$placebo)*100,0)

#350 subjects invited per day for 19 days to participate in Wave 2
table(as.Date(df_full$date, format="%d-%m-%Y"))
length(table(as.Date(df_full$date, format="%d-%m-%Y")))

#response rate in wave 2
round(length(df$id)/length(df_full$id)*100,1)

#Actual responses per day
table(as.Date(df$date, format="%d-%m-%Y"))

#response rate with one 1 day of the invite
round(mean(df$days_to_resp<=1)*100,0)

#reponse rate with in five days of the invite
round(mean(df$days_to_resp<=5)*100,0)

#Participants in wave 1 and wave 2
length(df$id)

#Participants in each recoded time period in W2
table(df$days)

#Participants in municipalities with below, average or above average crime
table(df$local_crime)

#percent stating to have received a leaflet from Trygfonden by mail during the last month?
round(mean(df$got_leaflet)*100,0)

################################################################## 
################################################################## 
######### FIGURE 3
################################################################## 
################################################################## 

#set own path to save Figure 3 as a EPS-file
#postscript("figure3.eps", width=12, height=16, horizontal=F, paper="special", pointsize = 20) 

layout.matrix <- matrix(c(1:6), nrow = 6, ncol = 1)
layout(mat = layout.matrix, heights = c(3, 1.4, 1.65, 1.2, 1.2, 1.35)) 

#########A. Correct Response: Declining Trend in Burglaries
par(mar=c(4,3.4,2.5,2.2), xpd=T)
plot(stats$avg_t, axes=F, xlab="", ylab="", xlim=c(1,4), ylim=c(35,65), type="n") 
axis(2,at=c(35,40,45,50,55,60,65),labels=c("35%","40","45","50","55","60","65%"), line=0, cex.axis=1, tck = -.04) # lwd=2, cex.axis=1.5
axis(1,at=c(1:4),las=1.5,labels=c("Pre-intervention","7-12 days after","13-18 days after","19-25 days after"),line=0,cex.axis=1.25,tck=-.04, lwd=1, lwd.ticks=1)
title(main="A. Correct Response: Declining Trend in Burglaries", adj = 0, cex.main=1.5, font.main=1)
legend(y=71,x=2.98,legend=c("Statistics Leaflet",
                         "Non-statistics Leaflet or Placebo"),pch=c(19,21), 
                          cex=1.4,pt.bg=c("black","white"), pt.cex=2)
text(1.15,55,"Leaflet Intervention", font=1, srt=90, cex=1.25)
segments(1.1,35,1.1,65,lty=2)

#stat treatment
lines(c(1:4)+.025,stats$avg_t)
arrows(c(1:4)+.025,stats$avg_t_lower,c(1:4)+.025,stats$avg_t_upper,length=.10,lwd=1, pch=3, angle=90, code=0) #eller code=0
points(c(1:4)+.025,stats$avg_t,pch=21,col="black",bg="black",cex=2, lwd=1)

#non-stat treatment + placebo
lines(c(1:4),nonstats$avg_t)
arrows(c(1:4),nonstats$avg_t_lower,c(1:4),nonstats$avg_t_upper,length=.10,lwd=1, pch=3, angle=90, code=0) #eller code=0
points(c(1:4),nonstats$avg_t,pch=21,col="black",bg="white",cex=2, lwd=1)

#########B1. Correct Response: Nine Percent Burglary Rate (+/- 2%-points)
par(mar=c(1,3.4,1.25,2.2), xpd=F)
plot(stats$avg_l_pm, axes=F, xlab="", ylab="", xlim=c(1,4), ylim=c(20,35), type="n")
axis(2,at=c(20,25,30,34),labels=c("20%","25","30","35%"), line=0, cex.axis=1, tck = -.09) 
title(main="B1. Correct Response: Nine Percent Burglary Rate (+/- 2% Points)", adj = 0, cex.main=1.5, font.main=1)
segments(1.1,20,1.1,35,lty=2)

#stat treatment
lines(c(1:4)+.025,stats$avg_l_pm)
arrows(c(1:4)+.025,stats$avg_l_pm_lower,c(1:4)+.025,stats$avg_l_pm_upper,length=.10,lwd=1, pch=3, angle=90, code=0) #eller code=0
points(c(1:4)+.025,stats$avg_l_pm,pch=21,col="black",bg="black",cex=2, lwd=1)

#non-stat treatment + placebo
lines(c(1:4),nonstats$avg_l_pm)
arrows(c(1:4),nonstats$avg_l_pm_lower,c(1:4),nonstats$avg_l_pm_upper,length=.10,lwd=1, pch=3, angle=90, code=0) #eller code=0
points(c(1:4),nonstats$avg_l_pm,pch=21,col="black",bg="white",cex=2, lwd=1)

#########B2. Nine Percent Burglary Rate (Exact)
par(mar=c(4,3.4,1.25,2.2), xpd=F)
plot(stats$avg_l_ex, axes=F, xlab="", ylab="", xlim=c(1,4), ylim=c(0,3), type="n") 
axis(2,at=c(0,1,2,3),labels=c("0%","1","2","3%"), line=0, cex.axis=1, tck = -.095) 
axis(1,at=c(1:4),las=1.5,labels=c("Pre-intervention","7-12 days after","13-18 days after","19-25 days after"),line=0,cex.axis=1.25,tck=-.09, lwd=1, lwd.ticks=1)
title(main="B2. Nine Percent Burglary Rate (Exact)", adj = 0, cex.main=1.5, font.main=1)
segments(1.1,0,1.1,3,lty=2)

#stat treatment
lines(c(1:4)+.025,stats$avg_l_ex)
arrows(c(1:4)+.025,stats$avg_l_ex_lower,c(1:4)+.025,stats$avg_l_ex_upper,length=.10,lwd=1, pch=3, angle=90, code=0) #eller code=0
points(c(1:4)+.025,stats$avg_l_ex,pch=21,col="black",bg="black",cex=2, lwd=1)

#non-stat treatment + placebo
lines(c(1:4),nonstats$avg_l_ex)
arrows(c(1:4),nonstats$avg_l_ex_lower,c(1:4),nonstats$avg_l_ex_upper,length=.10,lwd=1, pch=3, angle=90, code=0) #eller code=0
points(c(1:4),nonstats$avg_l_ex,pch=21,col="black",bg="white",cex=2, lwd=1)

#########C1. Correct Response: Municipal Burglary Rate is Above the National Average
par(mar=c(1,3.4,1.25,2.2), xpd=F)
plot(crime_above$avg_r, axes=F, xlab="", ylab="", xlim=c(1,4), ylim=c(20,50), type="n") 
axis(2,at=c(20,30,40,50),labels=c("20%","30","40","50%"), line=0, cex.axis=1, tck = -.10)
title(main="C1. Correct Response: Municipal Burglary Rate is Above the National Average", adj = 0, cex.main=1.5, font.main=1)
segments(1.1,20,1.1,50,lty=2)

#stat treatment
lines(c(1:4)+.025,crime_above$avg_r[5:8])
arrows(c(1:4)+.025,crime_above$avg_r_lower[5:8],c(1:4)+.025,crime_above$avg_r_upper[5:8],length=.10,lwd=1, pch=3, angle=90, code=0) #eller code=0
points(c(1:4)+.025,crime_above$avg_r[5:8],pch=21,col="black",bg="black",cex=2, lwd=1)

#non stat treatment
lines(c(1:4),crime_above$avg_r[1:4])
arrows(c(1:4),crime_above$avg_r_lower[1:4],c(1:4),crime_above$avg_r_upper[1:4],length=.10,lwd=1, pch=3, angle=90, code=0) #eller code=0
points(c(1:4),crime_above$avg_r[1:4],pch=21,col="black",bg="white",cex=2, lwd=1)

#########C2. Municipal Burglary Rate is at the National Average
par(mar=c(1,3.4,1.25,2.2), xpd=F)
plot(crime_average$avg_r, axes=F, xlab="", ylab="", xlim=c(1,4), ylim=c(40,70), type="n") 
axis(2,at=c(40,50,60,70),labels=c("40%","50","60","70%"), line=0, cex.axis=1, tck = -.10)
title(main="C2. Municipal Burglary Rate is at the National Average", adj = 0, cex.main=1.5, font.main=1)
segments(1.1,40,1.1,70,lty=2)

#stat treatment
lines(c(1:4)+.025,crime_average$avg_r[5:8])
arrows(c(1:4)+.025,crime_average$avg_r_lower[5:8],c(1:4)+.025,crime_average$avg_r_upper[5:8],length=.10,lwd=1, pch=3, angle=90, code=0) #eller code=0
points(c(1:4)+.025,crime_average$avg_r[5:8],pch=21,col="black",bg="black",cex=2, lwd=1)

#non stat treatment
lines(c(1:4),crime_average$avg_r[1:4])
arrows(c(1:4),crime_average$avg_r_lower[1:4],c(1:4),crime_average$avg_r_upper[1:4],length=.10,lwd=1, pch=3, angle=90, code=0) #eller code=0
points(c(1:4),crime_average$avg_r[1:4],pch=21,col="black",bg="white",cex=2, lwd=1)

#########C3. Municipal Burglary Rate is Below the National Average
par(mar=c(2,3.4,1.25,2.2), xpd=F)
plot(crime_below$avg_r, axes=F, xlab="", ylab="", xlim=c(1,4), ylim=c(20,60), type="n")
axis(1,at=c(1:4),las=1.5,labels=c("Pre-intervention","7-12 days after","13-18 days after","19-25 days after"),line=0,cex.axis=1.25,tck=-.10, lwd=1, lwd.ticks=1)
axis(2,at=c(20,30,40,50,60),labels=c("20%","30","40","50","60%"), line=0, cex.axis=1, tck = -.10)
title(main="C3. Municipal Burglary Rate is Below the National Average", adj = 0, cex.main=1.5, font.main=1)
segments(1.1,20,1.1,60,lty=2)

#stat treatment
lines(c(1:4)+.025,crime_below$avg_r[5:8])
arrows(c(1:4)+.025,crime_below$avg_r_lower[5:8],c(1:4)+.025,crime_below$avg_r_upper[5:8],length=.10,lwd=1, pch=3, angle=90, code=0) #eller code=0
points(c(1:4)+.025,crime_below$avg_r[5:8],pch=21,col="black",bg="black",cex=2, lwd=1)

#non stat treatment
lines(c(1:4),crime_below$avg_r[1:4])
arrows(c(1:4),crime_below$avg_r_lower[1:4],c(1:4),crime_below$avg_r_upper[1:4],length=.10,lwd=1, pch=3, angle=90, code=0) #eller code=0
points(c(1:4),crime_below$avg_r[1:4],pch=21,col="black",bg="white",cex=2, lwd=1)

####Y-axis
mtext(text="Percent Correct Responses",side=2,line=-1,outer=TRUE,cex=.8,adj=.13) #bottom y-axis
mtext(text="Percent Correct Responses",side=2,line=-1,outer=TRUE,cex=.8,adj=.565) #middle y-axis
mtext(text="Percent Correct Responses",side=2,line=-1,outer=TRUE,cex=.8,adj=.935) #top y-axis

#dev.off()

################################################################## 
################################################################## 
######### ANALYSIS
################################################################## 
################################################################## 

#########A. Correct Response: Declining Trend in Burglaries
t.test(df$c_trend_w1)
summary(lm(c_trend_w1~1, data=df))
summary(lm(c_trend_w1~stat, data=df))
summary(lm(c_trend_w2~stat, data=df))
summary(lm(c_trend_w2~stat, data=df,subset=days=="1_days7_12"))
summary(lm(c_trend_w2~stat, data=df,subset=days=="2_days13_18"))
summary(lm(c_trend_w2~stat, data=df,subset=days=="3_days19_25"))

#differences between time periods after the intervention
summary(lm(c_trend_w2~stat*days,data=df,subset=days!="1_days7_12"))
summary(lm(c_trend_w2~stat*days,data=df,subset=days!="2_days13_18"))
summary(lm(c_trend_w2~stat*days,data=df,subset=days!="3_days19_25"))

#########B1. Correct Response: Nine Percent Burglary Rate (+/- 2%-points)
t.test(df$c_level_w1_pm2)
summary(lm(c_level_w1_pm2~1, data=df))
summary(lm(c_level_w1_pm2~stat, data=df))
summary(lm(c_level_w2_pm2~stat, data=df))
summary(lm(c_level_w2_pm2~stat, data=df,subset=days=="1_days7_12"))
summary(lm(c_level_w2_pm2~stat, data=df,subset=days=="2_days13_18"))
summary(lm(c_level_w2_pm2~stat, data=df,subset=days=="3_days19_25"))

#########B2. Nine Percent Burglary Rate (Exact)
t.test(df$c_level_w1_exact)
summary(lm(c_level_w1_exact~1, data=df))
summary(lm(c_level_w1_exact~stat, data=df))
summary(lm(c_level_w2_exact~stat, data=df))
summary(lm(c_level_w2_exact~stat, data=df,subset=days=="1_days7_12"))
summary(lm(c_level_w2_exact~stat, data=df,subset=days=="2_days13_18"))
summary(lm(c_level_w2_exact~stat, data=df,subset=days=="3_days19_25"))

#########C1. Correct Response: Municipal Burglary Rate is Above the National Average
t.test(df$c_relative_w1)
summary(lm(c_relative_w1~1, data=df, subset=local_crime==3))
summary(lm(c_relative_w1~stat, data=df, subset=local_crime==3))
summary(lm(c_relative_w2~stat, data=df, subset=local_crime==3))
summary(lm(c_relative_w2~stat, data=df,subset=days=="1_days7_12" & local_crime==3))
summary(lm(c_relative_w2~stat, data=df,subset=days=="2_days13_18" & local_crime==3))
summary(lm(c_relative_w2~stat, data=df,subset=days=="3_days19_25" & local_crime==3))

#########C2. Municipal Burglary Rate is at the National Average
t.test(df$c_relative_w1)
summary(lm(c_relative_w1~1, data=df, subset=local_crime==2))
summary(lm(c_relative_w1~stat, data=df, subset=local_crime==2))
summary(lm(c_relative_w2~stat, data=df, subset=local_crime==2))
summary(lm(c_relative_w2~stat, data=df,subset=days=="1_days7_12" & local_crime==2))
summary(lm(c_relative_w2~stat, data=df,subset=days=="2_days13_18" & local_crime==2))
summary(lm(c_relative_w2~stat, data=df,subset=days=="3_days19_25" & local_crime==2))

#########C3. Municipal Burglary Rate is Below the National Average
t.test(df$c_relative_w1)
summary(lm(c_relative_w1~1, data=df, subset=local_crime==1))
summary(lm(c_relative_w1~stat, data=df, subset=local_crime==1))
summary(lm(c_relative_w2~stat, data=df, subset=local_crime==1))
summary(lm(c_relative_w2~stat, data=df,subset=days=="1_days7_12" & local_crime==1))
summary(lm(c_relative_w2~stat, data=df,subset=days=="2_days13_18" & local_crime==1))
summary(lm(c_relative_w2~stat, data=df,subset=days=="3_days19_25" & local_crime==1))

############################################
############################################
######################THE END
############################################
############################################
