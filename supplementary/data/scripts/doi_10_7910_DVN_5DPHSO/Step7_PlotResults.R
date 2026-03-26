#Creating plots and figures from bootstrap results

#Change the working directory as needed
setwd("/Users/djd78/Box/Belief Networks/Replication Materials/Results")

rm(list=ls())

library(ggplot2)
library(grid)
library(gmodels)

load(file="yearlist.saved")

reps = 5000 #number of bootstrap replications
###############################################
#Code for generating Figures 6 and 7 

#Density 
measure = "Dens" 

#load in data 
load(file = paste(measure, "_full.saved", sep="")) #baseline condition
measure_f = get(measure)
load(file = paste(measure, "_intermed.saved", sep="")) #ideology-removed condition
measure_i = get(measure)
load(file = paste(measure, "_partial.saved", sep="")) #ideology-controlled condition
measure_p = get(measure)

#Plot yearly fitted means
p = data.frame(yearlist) #set up data frame for values to plot 
p$mean_f = NA
p$mean_i = NA
p$mean_p = NA
for(i in 1:length(yearlist)) { #find yearly means across bootstrap replications
  p$mean_f[i]=mean(measure_f[i,2:(reps+1)])
  p$mean_i[i]=mean(measure_i[i,2:(reps+1)])
  p$mean_p[i]=mean(measure_p[i,2:(reps+1)])
}

p$mean_f=loess(p$mean_f~p$yearlist)$fitted #find loess fits of time trend 
p$mean_i=loess(p$mean_i~p$yearlist)$fitted
p$mean_p=loess(p$mean_p~p$yearlist)$fitted

g1=ggplot(p, aes(x=yearlist, y=mean_f))+
  geom_line(aes(x=yearlist, y=mean_f), colour="black",  size=1.5)+
  geom_line(aes(x=yearlist, y=mean_i), colour="black", linetype="dashed",  size=1.5)+
  geom_line(aes(x=yearlist, y=mean_p), colour="black", linetype="dotted",  size=1.5)+
  xlab("Year")+
  ylab("")+
  scale_y_continuous(limits=c(.02,.05))+
  labs(title="A - Density")+
  theme(plot.title=element_text(hjust=0), axis.title.y = element_text(vjust=1), panel.background=element_rect(fill="white", colour="black"), legend.position="none", title=element_text(face="bold", size=26), axis.text=element_text(size=22, face="bold", colour="black")) 
g1

#fit linear trends 
for(i in 1:reps) {
  measure_f[,i+1]=lm(measure_f[,i+1]~measure_f[,1])$fitted.values
  measure_i[,i+1]=lm(measure_i[,i+1]~measure_i[,1])$fitted.values
  measure_p[,i+1]=lm(measure_p[,i+1]~measure_p[,1])$fitted.values
}

#bootstrapped diffs
diff_f_dens = ((measure_f[31,2:(reps+1)] - measure_f[1,2:(reps+1)])/measure_f[1,2:(reps+1)])*100
diff_i_dens = ((measure_i[31,2:(reps+1)] - measure_i[1,2:(reps+1)])/measure_i[1,2:(reps+1)])*100
diff_p_dens = ((measure_p[31,2:(reps+1)] - measure_p[1,2:(reps+1)])/measure_p[1,2:(reps+1)])*100

#Plot mean changes 
m1=ggplot()+
  geom_errorbarh(aes(y=3, xmin=quantile(diff_f_dens, probs = c(.025, .975))[1], xmax =quantile(diff_f_dens, probs = c(.025, .975))[2] ), height=.1, size = 1.25)+
  geom_errorbarh(aes(y=3, xmin=quantile(diff_f_dens, probs = c(.05, .95))[1], xmax =quantile(diff_f_dens, probs = c(.05, .95))[2] ), height=.2, size = 1.25)+
  geom_point(aes(y=3, x=mean(diff_f_dens)))+
  geom_errorbarh(aes(y=2, xmin=quantile(diff_i_dens, probs = c(.025, .975))[1], xmax =quantile(diff_i_dens, probs = c(.025, .975))[2] ), height=.1, size = 1.25)+
  geom_errorbarh(aes(y=2, xmin=quantile(diff_i_dens, probs = c(.05, .95))[1], xmax =quantile(diff_i_dens, probs = c(.05, .95))[2] ), height=.2, size = 1.25)+
  geom_point(aes(y=2, x=mean(diff_i_dens)))+
  geom_errorbarh(aes(y=1, xmin=quantile(diff_p_dens, probs = c(.025, .975))[1], xmax =quantile(diff_p_dens, probs = c(.025, .975))[2] ), height=.1, size = 1.25)+
  geom_errorbarh(aes(y=1, xmin=quantile(diff_p_dens, probs = c(.05, .95))[1], xmax =quantile(diff_p_dens, probs = c(.05, .95))[2] ), height=.2, size = 1.25)+
  geom_point(aes(y=1, x=mean(diff_p_dens)))+
  geom_vline(xintercept=0, linetype="dashed")+
  scale_x_continuous(limits=c(-50, 50)) +
  xlab("Percent Change")+
  ylab("")+
  labs(title="A - Density")+
  scale_y_continuous(breaks=c(1,2,3), labels=c("1" ="Ideology-Controlled", "2"="Ideology-Removed", "3"="Ideology-Included"))+
  theme(plot.title=element_text(hjust=0), axis.title.y = element_text(vjust=1), panel.background=element_rect(fill="white", colour="black"), legend.position="none", title=element_text(face="bold", size=30), axis.text=element_text(size=26, face="bold", colour="black")) 
m1

#Number of modules 
measure = "Numcom" 

#load in data 
load(file = paste(measure, "_full.saved", sep=""))
measure_f = get(measure)
load(file = paste(measure, "_intermed.saved", sep=""))
measure_i = get(measure)
load(file = paste(measure, "_partial.saved", sep=""))
measure_p = get(measure)

#Plot yearly fitted means
p2 = data.frame(yearlist) #set up data frame for values to plot 
p2$mean_f = NA
p2$mean_i = NA
p2$mean_p = NA
for(i in 1:length(yearlist)) {
  p2$mean_f[i]=mean(measure_f[i,2:(reps+1)])
  p2$mean_i[i]=mean(measure_i[i,2:(reps+1)])
  p2$mean_p[i]=mean(measure_p[i,2:(reps+1)])
}

p2$mean_f=loess(p2$mean_f~p2$yearlist)$fitted
p2$mean_i=loess(p2$mean_i~p2$yearlist)$fitted
p2$mean_p=loess(p2$mean_p~p2$yearlist)$fitted

g2=ggplot(p2, aes(x=yearlist, y=mean_f))+
  geom_line(aes(x=yearlist, y=mean_f), colour="black",  size=1.5)+
  geom_line(aes(x=yearlist, y=mean_i), colour="black", linetype="dashed",  size=1.5)+
  geom_line(aes(x=yearlist, y=mean_p), colour="black", linetype="dotted",  size=1.5)+
  xlab("Year")+
  ylab("")+
  scale_y_continuous(limits=c(10,18))+
  labs(title="B - Number of Modules")+
  theme(plot.title=element_text(hjust=0), axis.title.y = element_text(vjust=1), panel.background=element_rect(fill="white", colour="black"), legend.position="none", title=element_text(face="bold", size=26), axis.text=element_text(size=22, face="bold", colour="black")) 
g2

#fit linear trends
for(i in 1:reps) {
  measure_f[,i+1]=lm(measure_f[,i+1]~measure_f[,1])$fitted.values
  measure_i[,i+1]=lm(measure_i[,i+1]~measure_i[,1])$fitted.values
  measure_p[,i+1]=lm(measure_p[,i+1]~measure_p[,1])$fitted.values
}

#bootstrapped diffs
diff_f_num = ((measure_f[31,2:(reps+1)] - measure_f[1,2:(reps+1)])/measure_f[1,2:(reps+1)])*100
diff_i_num = ((measure_i[31,2:(reps+1)] - measure_i[1,2:(reps+1)])/measure_i[1,2:(reps+1)])*100
diff_p_num = ((measure_p[31,2:(reps+1)] - measure_p[1,2:(reps+1)])/measure_p[1,2:(reps+1)])*100

#Plot mean changes 
m2=ggplot()+
  geom_errorbarh(aes(y=3, xmin=quantile(diff_f_num, probs = c(.025, .975))[1], xmax =quantile(diff_f_num, probs = c(.025, .975))[2] ), height=.1, size = 1.25)+
  geom_errorbarh(aes(y=3, xmin=quantile(diff_f_num, probs = c(.05, .95))[1], xmax =quantile(diff_f_num, probs = c(.05, .95))[2] ), height=.2, size = 1.25)+
  geom_point(aes(y=3, x=mean(diff_f_num)))+
  geom_errorbarh(aes(y=2, xmin=quantile(diff_i_num, probs = c(.025, .975))[1], xmax =quantile(diff_i_num, probs = c(.025, .975))[2] ), height=.1, size = 1.25)+
  geom_errorbarh(aes(y=2, xmin=quantile(diff_i_num, probs = c(.05, .95))[1], xmax =quantile(diff_i_num, probs = c(.05, .95))[2] ), height=.2, size = 1.25)+
  geom_point(aes(y=2, x=mean(diff_i_num)))+
  geom_errorbarh(aes(y=1, xmin=quantile(diff_p_num, probs = c(.025, .975))[1], xmax =quantile(diff_p_num, probs = c(.025, .975))[2] ), height=.1, size = 1.25)+
  geom_errorbarh(aes(y=1, xmin=quantile(diff_p_num, probs = c(.05, .95))[1], xmax =quantile(diff_p_num, probs = c(.05, .95))[2] ), height=.2, size = 1.25)+
  geom_point(aes(y=1, x=mean(diff_p_num)))+
  geom_vline(xintercept=0, linetype="dashed")+
  scale_x_continuous(limits=c(-50, 50)) +
  xlab("Percent Change")+
  ylab("")+
  labs(title="B - Number of Modules")+
  scale_y_continuous(breaks=c(1,2,3), labels=c("1" ="Ideology-Controlled", "2"="Ideology-Removed", "3"="Ideology-Included"))+
  theme(plot.title=element_text(hjust=0), axis.title.y = element_text(vjust=1), panel.background=element_rect(fill="white", colour="black"), legend.position="none", title=element_text(face="bold", size=30), axis.text=element_text(size=26, face="bold", colour="black")) 
m2

#Modularity 
measure = "Mod" 

#load in data 
load(file = paste(measure, "_full.saved", sep=""))
measure_f = get(measure)
load(file = paste(measure, "_intermed.saved", sep=""))
measure_i = get(measure)
load(file = paste(measure, "_partial.saved", sep=""))
measure_p = get(measure)

#Plot yearly fitted means
p3 = data.frame(yearlist) #set up data frame for values to plot 
p3$mean_f = NA
p3$mean_i = NA
p3$mean_p = NA
for(i in 1:length(yearlist)) {
  p3$mean_f[i]=mean(measure_f[i,2:(reps+1)])
  p3$mean_i[i]=mean(measure_i[i,2:(reps+1)])
  p3$mean_p[i]=mean(measure_p[i,2:(reps+1)])
}

p3$mean_f=loess(p3$mean_f~p3$yearlist)$fitted
p3$mean_i=loess(p3$mean_i~p3$yearlist)$fitted
p3$mean_p=loess(p3$mean_p~p3$yearlist)$fitted

g3=ggplot(p3, aes(x=yearlist, y=mean_f))+
  geom_line(aes(x=yearlist, y=mean_f), colour="black",  size=1.5)+
  geom_line(aes(x=yearlist, y=mean_i), colour="black", linetype="dashed",  size=1.5)+
  geom_line(aes(x=yearlist, y=mean_p), colour="black", linetype="dotted",  size=1.5)+
  xlab("Year")+
  ylab("")+
  scale_y_continuous(limits=c(.10,.15))+
  labs(title="C - Modularity")+
  theme(plot.title=element_text(hjust=0), axis.title.y = element_text(vjust=1), panel.background=element_rect(fill="white", colour="black"), legend.position="none", title=element_text(face="bold", size=26), axis.text=element_text(size=22, face="bold", colour="black")) 
g3

#fit linear trends
for(i in 1:reps) {
  measure_f[,i+1]=lm(measure_f[,i+1]~measure_f[,1])$fitted.values
  measure_i[,i+1]=lm(measure_i[,i+1]~measure_i[,1])$fitted.values
  measure_p[,i+1]=lm(measure_p[,i+1]~measure_p[,1])$fitted.values
}

#bootstrapped diffs
diff_f_mod = ((measure_f[31,2:(reps+1)] - measure_f[1,2:(reps+1)])/measure_f[1,2:(reps+1)])*100
diff_i_mod = ((measure_i[31,2:(reps+1)] - measure_i[1,2:(reps+1)])/measure_i[1,2:(reps+1)])*100
diff_p_mod = ((measure_p[31,2:(reps+1)] - measure_p[1,2:(reps+1)])/measure_p[1,2:(reps+1)])*100

#Plot mean changes 
m3=ggplot()+
  geom_errorbarh(aes(y=3, xmin=quantile(diff_f_mod, probs = c(.025, .975))[1], xmax =quantile(diff_f_mod, probs = c(.025, .975))[2] ), height=.1, size = 1.25)+
  geom_errorbarh(aes(y=3, xmin=quantile(diff_f_mod, probs = c(.05, .95))[1], xmax =quantile(diff_f_mod, probs = c(.05, .95))[2] ), height=.2, size = 1.25)+
  geom_point(aes(y=3, x=mean(diff_f_mod)))+
  geom_errorbarh(aes(y=2, xmin=quantile(diff_i_mod, probs = c(.025, .975))[1], xmax =quantile(diff_i_mod, probs = c(.025, .975))[2] ), height=.1, size = 1.25)+
  geom_errorbarh(aes(y=2, xmin=quantile(diff_i_mod, probs = c(.05, .95))[1], xmax =quantile(diff_i_mod, probs = c(.05, .95))[2] ), height=.2, size = 1.25)+
  geom_point(aes(y=2, x=mean(diff_i_mod)))+
  geom_errorbarh(aes(y=1, xmin=quantile(diff_p_mod, probs = c(.025, .975))[1], xmax =quantile(diff_p_mod, probs = c(.025, .975))[2] ), height=.1, size = 1.25)+
  geom_errorbarh(aes(y=1, xmin=quantile(diff_p_mod, probs = c(.05, .95))[1], xmax =quantile(diff_p_mod, probs = c(.05, .95))[2] ), height=.2, size = 1.25)+
  geom_point(aes(y=1, x=mean(diff_p_mod)))+
  geom_vline(xintercept=0, linetype="dashed")+
  scale_x_continuous(limits=c(-50, 50)) +
  xlab("Percent Change")+
  ylab("")+
  labs(title="C - Modularity")+
  scale_y_continuous(breaks=c(1,2,3), labels=c("1" ="Ideology-Controlled", "2"="Ideology-Removed", "3"="Ideology-Included"))+
  theme(plot.title=element_text(hjust=0), axis.title.y = element_text(vjust=1), panel.background=element_rect(fill="white", colour="black"), legend.position="none", title=element_text(face="bold", size=30), axis.text=element_text(size=26, face="bold", colour="black")) 
m3

#% in Largest Mod 
measure = "Large" 

#load in data 
load(file = paste(measure, "_full.saved", sep=""))
measure_f = get(measure)
load(file = paste(measure, "_intermed.saved", sep=""))
measure_i = get(measure)
load(file = paste(measure, "_partial.saved", sep=""))
measure_p = get(measure)

#Plot yearly fitted means
p4 = data.frame(yearlist) #set up data frame for values to plot 
p4$mean_f = NA
p4$mean_i = NA
p4$mean_p = NA
for(i in 1:length(yearlist)) {
  p4$mean_f[i]=mean(measure_f[i,2:(reps+1)])
  p4$mean_i[i]=mean(measure_i[i,2:(reps+1)])
  p4$mean_p[i]=mean(measure_p[i,2:(reps+1)])
}

p4$mean_f=loess(p4$mean_f~p4$yearlist)$fitted
p4$mean_i=loess(p4$mean_i~p4$yearlist)$fitted
p4$mean_p=loess(p4$mean_p~p4$yearlist)$fitted

g4=ggplot(p4, aes(x=yearlist, y=mean_f))+
  geom_line(aes(x=yearlist, y=mean_f*100), colour="black",  size=1.5)+
  geom_line(aes(x=yearlist, y=mean_i*100), colour="black", linetype="dashed",  size=1.5)+
  geom_line(aes(x=yearlist, y=mean_p*100), colour="black", linetype="dotted",  size=1.5)+
  xlab("Year")+
  ylab("")+
  scale_y_continuous(limits=c(25, 50))+
  labs(title="D - % in Top Module")+
  theme(plot.title=element_text(hjust=0), axis.title.y = element_text(vjust=1), panel.background=element_rect(fill="white", colour="black"), legend.position="none", title=element_text(face="bold", size=26), axis.text=element_text(size=22, face="bold", colour="black")) 
g4

#fit linear trends
for(i in 1:reps) {
  measure_f[,i+1]=lm(measure_f[,i+1]~measure_f[,1])$fitted.values
  measure_i[,i+1]=lm(measure_i[,i+1]~measure_i[,1])$fitted.values
  measure_p[,i+1]=lm(measure_p[,i+1]~measure_p[,1])$fitted.values
}

#bootstrapped diffs
diff_f_large = ((measure_f[31,2:(reps+1)] - measure_f[1,2:(reps+1)])/measure_f[1,2:(reps+1)])*100
diff_i_large = ((measure_i[31,2:(reps+1)] - measure_i[1,2:(reps+1)])/measure_i[1,2:(reps+1)])*100
diff_p_large = ((measure_p[31,2:(reps+1)] - measure_p[1,2:(reps+1)])/measure_p[1,2:(reps+1)])*100

#Plot mean changes 
m4=ggplot()+
  geom_errorbarh(aes(y=3, xmin=quantile(diff_f_large, probs = c(.025, .975))[1], xmax =quantile(diff_f_large, probs = c(.025, .975))[2] ), height=.1, size = 1.25)+
  geom_errorbarh(aes(y=3, xmin=quantile(diff_f_large, probs = c(.05, .95))[1], xmax =quantile(diff_f_large, probs = c(.05, .95))[2] ), height=.2, size = 1.25)+
  geom_point(aes(y=3, x=mean(diff_f_large)))+
  geom_errorbarh(aes(y=2, xmin=quantile(diff_i_large, probs = c(.025, .975))[1], xmax =quantile(diff_i_large, probs = c(.025, .975))[2] ), height=.1, size = 1.25)+
  geom_errorbarh(aes(y=2, xmin=quantile(diff_i_large, probs = c(.05, .95))[1], xmax =quantile(diff_i_large, probs = c(.05, .95))[2] ), height=.2, size = 1.25)+
  geom_point(aes(y=2, x=mean(diff_i_large)))+
  geom_errorbarh(aes(y=1, xmin=quantile(diff_p_large, probs = c(.025, .975))[1], xmax =quantile(diff_p_large, probs = c(.025, .975))[2] ), height=.1, size = 1.25)+
  geom_errorbarh(aes(y=1, xmin=quantile(diff_p_large, probs = c(.05, .95))[1], xmax =quantile(diff_p_large, probs = c(.05, .95))[2] ), height=.2, size = 1.25)+
  geom_point(aes(y=1, x=mean(diff_p_large)))+
  geom_vline(xintercept=0, linetype="dashed")+
  scale_x_continuous(limits=c(-50, 50)) +
  xlab("Percent Change")+
  ylab("")+
  labs(title="D - % in Top Module")+
  scale_y_continuous(breaks=c(1,2,3), labels=c("1" ="Ideology-Controlled", "2"="Ideology-Removed", "3"="Ideology-Included"))+
  theme(plot.title=element_text(hjust=0), axis.title.y = element_text(vjust=1), panel.background=element_rect(fill="white", colour="black"), legend.position="none", title=element_text(face="bold", size=30), axis.text=element_text(size=26, face="bold", colour="black")) 
m4

#% Nodes in Largest Two 
measure = "Large2" 

#load in data 
load(file = paste(measure, "_full.saved", sep=""))
measure_f = get(measure)
load(file = paste(measure, "_intermed.saved", sep=""))
measure_i = get(measure)
load(file = paste(measure, "_partial.saved", sep=""))
measure_p = get(measure)

#Plot yearly fitted means
p5 = data.frame(yearlist) #set up data frame for values to plot 
p5$mean_f = NA
p5$mean_i = NA
p5$mean_p = NA
for(i in 1:length(yearlist)) {
  p5$mean_f[i]=mean(measure_f[i,2:(reps+1)])
  p5$mean_i[i]=mean(measure_i[i,2:(reps+1)])
  p5$mean_p[i]=mean(measure_p[i,2:(reps+1)])
}

p5$mean_f=loess(p5$mean_f~p5$yearlist)$fitted
p5$mean_i=loess(p5$mean_i~p5$yearlist)$fitted
p5$mean_p=loess(p5$mean_p~p5$yearlist)$fitted

g5=ggplot(p5, aes(x=yearlist, y=mean_f))+
  geom_line(aes(x=yearlist, y=mean_f*100), colour="black",  size=1.5)+
  geom_line(aes(x=yearlist, y=mean_i*100), colour="black", linetype="dashed",  size=1.5)+
  geom_line(aes(x=yearlist, y=mean_p*100), colour="black", linetype="dotted",  size=1.5)+
  xlab("Year")+
  ylab("")+
  scale_y_continuous(limits=c(50, 75))+
  labs(title="E - % in Top Two Modules")+
  theme(plot.title=element_text(hjust=0), axis.title.y = element_text(vjust=1), panel.background=element_rect(fill="white", colour="black"), legend.position="none", title=element_text(face="bold", size=26), axis.text=element_text(size=22, face="bold", colour="black")) 
g5

#fit linear trends
for(i in 1:reps) {
  measure_f[,i+1]=lm(measure_f[,i+1]~measure_f[,1])$fitted.values
  measure_i[,i+1]=lm(measure_i[,i+1]~measure_i[,1])$fitted.values
  measure_p[,i+1]=lm(measure_p[,i+1]~measure_p[,1])$fitted.values
}

#bootstrapped diffs
diff_f_large2 = ((measure_f[31,2:(reps+1)] - measure_f[1,2:(reps+1)])/measure_f[1,2:(reps+1)])*100
diff_i_large2 = ((measure_i[31,2:(reps+1)] - measure_i[1,2:(reps+1)])/measure_i[1,2:(reps+1)])*100
diff_p_large2 = ((measure_p[31,2:(reps+1)] - measure_p[1,2:(reps+1)])/measure_p[1,2:(reps+1)])*100

#Plot mean changes 
m5=ggplot()+
  geom_errorbarh(aes(y=3, xmin=quantile(diff_f_large2, probs = c(.025, .975))[1], xmax =quantile(diff_f_large2, probs = c(.025, .975))[2] ), height=.1, size = 1.25)+
  geom_errorbarh(aes(y=3, xmin=quantile(diff_f_large2, probs = c(.05, .95))[1], xmax =quantile(diff_f_large2, probs = c(.05, .95))[2] ), height=.2, size = 1.25)+
  geom_point(aes(y=3, x=mean(diff_f_large2)))+
  geom_errorbarh(aes(y=2, xmin=quantile(diff_i_large2, probs = c(.025, .975))[1], xmax =quantile(diff_i_large2, probs = c(.025, .975))[2] ), height=.1, size = 1.25)+
  geom_errorbarh(aes(y=2, xmin=quantile(diff_i_large2, probs = c(.05, .95))[1], xmax =quantile(diff_i_large2, probs = c(.05, .95))[2] ), height=.2, size = 1.25)+
  geom_point(aes(y=2, x=mean(diff_i_large2)))+
  geom_errorbarh(aes(y=1, xmin=quantile(diff_p_large2, probs = c(.025, .975))[1], xmax =quantile(diff_p_large2, probs = c(.025, .975))[2] ), height=.1, size = 1.25)+
  geom_errorbarh(aes(y=1, xmin=quantile(diff_p_large2, probs = c(.05, .95))[1], xmax =quantile(diff_p_large2, probs = c(.05, .95))[2] ), height=.2, size = 1.25)+
  geom_point(aes(y=1, x=mean(diff_p_large2)))+
  geom_vline(xintercept=0, linetype="dashed")+
  scale_x_continuous(limits=c(-50, 50)) +
  xlab("Percent Change")+
  ylab("")+
  labs(title="E - % in Top Two Modules")+
  scale_y_continuous(breaks=c(1,2,3), labels=c("1" ="Ideology-Controlled", "2"="Ideology-Removed", "3"="Ideology-Included"))+
  theme(plot.title=element_text(hjust=0), axis.title.y = element_text(vjust=1), panel.background=element_rect(fill="white", colour="black"), legend.position="none", title=element_text(face="bold", size=30), axis.text=element_text(size=26, face="bold", colour="black")) 
m5

#Concentration 
measure = "Rosen" 

#load in data 
load(file = paste(measure, "_full.saved", sep=""))
measure_f = get(measure)
load(file = paste(measure, "_intermed.saved", sep=""))
measure_i = get(measure)
load(file = paste(measure, "_partial.saved", sep=""))
measure_p = get(measure)

#Plot yearly fitted means
p6 = data.frame(yearlist) #set up data frame for values to plot 
p6$mean_f = NA
p6$mean_i = NA
p6$mean_p = NA
for(i in 1:length(yearlist)) {
  p6$mean_f[i]=mean(measure_f[i,2:(reps+1)])
  p6$mean_i[i]=mean(measure_i[i,2:(reps+1)])
  p6$mean_p[i]=mean(measure_p[i,2:(reps+1)])
}

p6$mean_f=loess(p6$mean_f~p6$yearlist)$fitted
p6$mean_i=loess(p6$mean_i~p6$yearlist)$fitted
p6$mean_p=loess(p6$mean_p~p6$yearlist)$fitted

g6=ggplot(p6, aes(x=yearlist, y=mean_f))+
  geom_line(aes(x=yearlist, y=mean_f), colour="black", size=1.5)+
  geom_line(aes(x=yearlist, y=mean_i), colour="black", linetype="dashed", size=1.5)+
  geom_line(aes(x=yearlist, y=mean_p), colour="black", linetype="dotted", size=1.5)+
  xlab("Year")+
  ylab("")+
  scale_y_continuous(limits=c(.15,.3))+
  labs(title="F - Module Concentration")+
  theme(plot.title=element_text(hjust=0), axis.title.y = element_text(vjust=1), panel.background=element_rect(fill="white", colour="black"), legend.position="none", title=element_text(face="bold", size=26), axis.text=element_text(size=22, face="bold", colour="black")) 
g6

#fit linear trends
for(i in 1:reps) {
  measure_f[,i+1]=lm(measure_f[,i+1]~measure_f[,1])$fitted.values
  measure_i[,i+1]=lm(measure_i[,i+1]~measure_i[,1])$fitted.values
  measure_p[,i+1]=lm(measure_p[,i+1]~measure_p[,1])$fitted.values
}

#bootstrapped diffs
diff_f_conc = ((measure_f[31,2:(reps+1)] - measure_f[1,2:(reps+1)])/measure_f[1,2:(reps+1)])*100
diff_i_conc = ((measure_i[31,2:(reps+1)] - measure_i[1,2:(reps+1)])/measure_i[1,2:(reps+1)])*100
diff_p_conc = ((measure_p[31,2:(reps+1)] - measure_p[1,2:(reps+1)])/measure_p[1,2:(reps+1)])*100

#Plot mean changes 
m6=ggplot()+
  geom_errorbarh(aes(y=3, xmin=quantile(diff_f_conc, probs = c(.025, .975))[1], xmax =quantile(diff_f_conc, probs = c(.025, .975))[2] ), height=.1, size=1.25)+
  geom_errorbarh(aes(y=3, xmin=quantile(diff_f_conc, probs = c(.05, .95))[1], xmax =quantile(diff_f_conc, probs = c(.05, .95))[2] ), height=.2, size=1.25)+
  geom_point(aes(y=3, x=mean(diff_f_conc)))+
  geom_errorbarh(aes(y=2, xmin=quantile(diff_i_conc, probs = c(.025, .975))[1], xmax =quantile(diff_i_conc, probs = c(.025, .975))[2] ), height=.1, size=1.25)+
  geom_errorbarh(aes(y=2, xmin=quantile(diff_i_conc, probs = c(.05, .95))[1], xmax =quantile(diff_i_conc, probs = c(.05, .95))[2] ), height=.2, size=1.25)+
  geom_point(aes(y=2, x=mean(diff_i_conc)))+
  geom_errorbarh(aes(y=1, xmin=quantile(diff_p_conc, probs = c(.025, .975))[1], xmax =quantile(diff_p_conc, probs = c(.025, .975))[2] ), height=.1, size=1.25)+
  geom_errorbarh(aes(y=1, xmin=quantile(diff_p_conc, probs = c(.05, .95))[1], xmax =quantile(diff_p_conc, probs = c(.05, .95))[2] ), height=.2, size=1.25)+
  geom_point(aes(y=1, x=mean(diff_p_conc)))+
  geom_vline(xintercept=0, linetype="dashed")+
  scale_x_continuous(limits=c(-50, 50)) +
  xlab("Percent Change")+
  ylab("")+
  labs(title="F - Module Concentration")+
  scale_y_continuous(breaks=c(1,2,3), labels=c("1" ="Ideology-Controlled", "2"="Ideology-Removed", "3"="Ideology-Included"))+
  theme(plot.title=element_text(hjust=0), axis.title.y = element_text(vjust=1), panel.background=element_rect(fill="white", colour="black"), legend.position="none", title=element_text(face="bold", size=30), axis.text=element_text(size=26, face="bold", colour="black")) 
m6

vplayout = function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

#CHANGE FILE PATHS 

mypath=file.path("/Users", "djd78", "Box", "Belief Networks", "Figures", "trends.tiff")
tiff(file=mypath, width=1250, height=1500)
grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 2)))
print(g1, vp=vplayout(1,1))
print(g2, vp=vplayout(1,2))
print(g3, vp=vplayout(2,1))
print(g4, vp=vplayout(2,2))
print(g5, vp=vplayout(3,1))
print(g6, vp=vplayout(3,2))
dev.off()

mypath=file.path("/Users", "djd78", "Box", "Belief Networks", "Figures", "sig_tests.tiff")
tiff(file=mypath, width=1500, height=2000)
grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 2)))
print(m1, vp=vplayout(1,1))
print(m2, vp=vplayout(1,2))
print(m3, vp=vplayout(2,1))
print(m4, vp=vplayout(2,2))
print(m5, vp=vplayout(3,1))
print(m6, vp=vplayout(3,2))
dev.off()
