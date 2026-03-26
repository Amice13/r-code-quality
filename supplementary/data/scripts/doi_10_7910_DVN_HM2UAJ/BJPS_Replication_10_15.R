##################
###REPLICATION FOR
#Cilizoglu and Estancona
#'Hide and Seek: Offshore Financial Centers and Targeted Sanctions'
#BJPS
#2025

###clear workspace and load necessary packages

rm(list=ls(all=TRUE))
library(foreign)
library(tidyr)
library(dplyr)
library(readr)
library(car)
library(xtable)
library(splitstackshape)
library(DataCombine)
library(RColorBrewer)
library(stringr)
library(anytime)
library(sampleSelection)
library(vdemdata)
library(stargazer)
library(marginaleffects)
library(prediction)
library(ggplot2)

#import offshore datasets 

setwd("/")

#######all countries

all_full_new<-read.csv("all_full_monthly_RR_5.csv")
o_full_new<-read.csv("o_full_monthly_RR_5.csv")
pan_full_new<-read.csv("pan_full_monthly_RR_5.csv")
par_full_new<-read.csv("par_full_monthly_RR_5.csv")

######'limited origin' countries

lim_all<-read.csv("lim_all_RR.csv")
lim_o<-read.csv("lim_o_RR.csv")
lim_pan<-read.csv("lim_pan_RR.csv")
lim_par<-read.csv("lim_par_RR.csv")



##########################################################################
#####################################################################
#########################################################################

####Necessary Data Cleaning/Adjustments

#####add increase_tax variable

all_full_new$increase_tax<-ifelse(all_full_new$change_tax>0,1,0)
all_full_new$increase_repress<-ifelse(all_full_new$change_v2csreprss>0,1,0)
all_full_new$increase_antimv<-ifelse(all_full_new$change_v2csantimv>0,1,0)

o_full_new$increase_tax<-ifelse(o_full_new$change_tax>0,1,0)
o_full_new$increase_repress<-ifelse(o_full_new$change_v2csreprss>0,1,0)
o_full_new$increase_antimv<-ifelse(o_full_new$change_v2csantimv>0,1,0)

pan_full_new$increase_tax<-ifelse(pan_full_new$change_tax>0,1,0)
pan_full_new$increase_repress<-ifelse(pan_full_new$change_v2csreprss>0,1,0)
pan_full_new$increase_antimv<-ifelse(pan_full_new$change_v2csantimv>0,1,0)

par_full_new$increase_tax<-ifelse(par_full_new$change_tax>0,1,0)
par_full_new$increase_repress<-ifelse(par_full_new$change_v2csreprss>0,1,0)
par_full_new$increase_antimv<-ifelse(par_full_new$change_v2csantimv>0,1,0)

######alternate all_full dataset that's just an rbind of all the other datasets

all_full_bind<-rbind(o_full_new, pan_full_new, par_full_new)
all_full_bind<-unique(all_full_bind)

all_full_bind<-arrange(all_full_bind, wdicode_o, Year, Month)

#########create limited rbind 
lim_bind<-rbind(lim_o, lim_pan, lim_par)
lim_bind<-unique(lim_bind)

#######create increase variables in lim data 
lim_all$increase_tax<-ifelse(lim_all$change_tax>0,1,0)
lim_all$increase_repress<-ifelse(lim_all$change_v2csreprss>0,1,0)
lim_all$increase_antimv<-ifelse(lim_all$change_v2csantimv>0,1,0)

lim_bind$increase_tax<-ifelse(lim_bind$change_tax>0,1,0)
lim_bind$increase_repress<-ifelse(lim_bind$change_v2csreprss>0,1,0)
lim_bind$increase_antimv<-ifelse(lim_bind$change_v2csantimv>0,1,0)

lim_o$increase_tax<-ifelse(lim_o$change_tax>0,1,0)
lim_o$increase_repress<-ifelse(lim_o$change_v2csreprss>0,1,0)
lim_o$increase_antimv<-ifelse(lim_o$change_v2csantimv>0,1,0)

lim_pan$increase_tax<-ifelse(lim_pan$change_tax>0,1,0)
lim_pan$increase_repress<-ifelse(lim_pan$change_v2csreprss>0,1,0)
lim_pan$increase_antimv<-ifelse(lim_pan$change_v2csantimv>0,1,0)

lim_par$increase_tax<-ifelse(lim_par$change_tax>0,1,0)
lim_par$increase_repress<-ifelse(lim_par$change_v2csreprss>0,1,0)
lim_par$increase_antimv<-ifelse(lim_par$change_v2csantimv>0,1,0)

#######################################################
########################################

################
####Main Models (in-text), Tables 1 and 2
################


m23_t <- heckit(
  selection = offshore_d ~lag_all_added+increase_tax+v2csantimv_ord+lag_el_diff+lag_monthly_brent,
  outcome = n_ofc_g23_nona ~ lag_all_added+lag_el_diff+increase_tax+v2csantimv_ord,
  method = "2step",
  data = all_full_bind
)
summary(m23_t)

m1_t <- heckit(
  selection = offshore_d ~lag_all_added+increase_tax+v2csantimv_ord+lag_el_diff+lag_monthly_brent,
  outcome = n_ofc_g1_nona ~ lag_all_added+lag_el_diff+increase_tax+v2csantimv_ord,
  method = "2step",
  data = all_full_bind
)
summary(m1_t)

m23_o_t <- heckit(
  selection = offshore_d ~lag_all_added+increase_tax+v2csantimv_ord+lag_el_diff+lag_monthly_brent,
  outcome = n_ofc_g23_nona ~ lag_all_added+lag_el_diff+increase_tax+v2csantimv_ord,
  method = "2step",
  data = o_full_new
)
summary(m23_o_t)

m1_o_t <- heckit(
  selection = offshore_d ~lag_all_added+increase_tax+v2csantimv_ord+lag_el_diff+lag_monthly_brent,
  outcome = n_ofc_g1_nona ~ lag_all_added+lag_el_diff+increase_tax+v2csantimv_ord,
  method = "2step",
  data = o_full_new
)
summary(m1_o_t)

m23_pan_t <- heckit(
  selection = offshore_d ~lag_all_added+increase_tax+v2csantimv_ord+lag_el_diff+lag_monthly_brent,
  outcome = n_ofc_g23_nona ~ lag_all_added+lag_el_diff+increase_tax+v2csantimv_ord,
  method = "2step",
  data = pan_full_new
)
summary(m23_pan_t)

m1_pan_t <- heckit(
  selection = offshore_d ~lag_all_added+increase_tax+v2csantimv_ord+lag_el_diff+lag_monthly_brent,
  outcome = n_ofc_g1_nona ~ lag_all_added+lag_el_diff+increase_tax+v2csantimv_ord,
  method = "2step",
  data = pan_full_new
)
summary(m1_pan_t)

m23_par_t <- heckit(
  selection = offshore_d ~lag_all_added+increase_tax+v2csantimv_ord+lag_el_diff+lag_monthly_brent,
  outcome = n_ofc_g23_nona ~ lag_all_added+lag_el_diff+increase_tax+v2csantimv_ord,
  method = "2step",
  data = par_full_new
)
summary(m23_par_t)

m1_par_t <- heckit(
  selection = offshore_d ~lag_all_added+increase_tax+v2csantimv_ord+lag_el_diff+lag_monthly_brent,
  outcome = n_ofc_g1_nona ~ lag_all_added+lag_el_diff+increase_tax+v2csantimv_ord,
  method = "2step",
  data = par_full_new
)
summary(m1_par_t)

#####Table 1
#Low Supervision
#manually combine selection and outcome results
stargazer(m23_t, m23_o_t, m23_pan_t, m23_par_t, selection.equation=TRUE, digits=3) 
stargazer(m23_t, m23_o_t, m23_pan_t, m23_par_t, digits=3) 


###Table 2
#High Supervision
#manually combine selection and outcome results
stargazer(m1_t, m1_o_t, m1_pan_t, m1_par_t, selection.equation=TRUE, digits=3) #need to combine these
stargazer(m1_t, m1_o_t, m1_pan_t, m1_par_t, digits=3)

##########################################
####################In-Text Plots (Figures 1, 2, 3, 4) 
##########note - plots will take a LONG time to run!

########Figure 1 - All Data 
test_added_all<-seq(from=min(na.omit(all_full_bind$lag_all_added)), to=max(na.omit(all_full_bind$lag_all_added)), length.out=335)
test_tax_all<-rep(median(na.omit(all_full_bind$increase_tax)), times=335)
test_v2csantimv_all<-rep(median(na.omit(all_full_bind$v2csantimv_ord)), times=335)
test_diff_all<-rep(median(na.omit(all_full_bind$lag_el_diff)), times=335)
test_brent_all<-rep(median(na.omit(all_full_bind$lag_monthly_brent)), times=335)

plotdat_all<-as.data.frame(cbind(test_added_all, test_tax_all, test_v2csantimv_all, test_diff_all,
                                 test_brent_all))

colnames(plotdat_all)<-c("lag_all_added", "increase_tax", "v2csantimv_ord", "lag_el_diff", "lag_monthly_brent")

td1_all<-predictions(m1_t, newdata=plotdat_all, part="outcome", type="unconditional")|>inferences(
  method = "boot", sim="balanced")

colnames(td1_all)[c(2,8,9,10)]<-c("estimate_td1", "std.error_td1", "conf.low_td1", "conf.high_td1")

td2_all<-predictions(m23_t, newdata=plotdat_all, part="outcome", type="unconditional")|>inferences(
  method = "boot", sim="balanced")

colnames(td2_all)[c(2,8,9,10)]<-c("estimate_td2", "std.error_td2", "conf.low_td2", "conf.high_td2")
td2_all<-td2_all[,c(2,8,9,10)]

plotdat_all_2<-as.data.frame(cbind(td1_all, td2_all))
plotdat_all_2<-plotdat_all_2[-c(334,335),]

plotdat_all_2 <- mutate_all(plotdat_all_2, function(x) as.numeric(as.character(x)))

#create plot
pdf("all_data.pdf")
ggplot(plotdat_all_2)+ 
  labs(x="Number of Sanctions Added", y="Predicted Offshore Transactions")+
  geom_line(aes(x=lag_all_added, y=plotdat_all_2[,11], color="Low Supervision"),linetype="solid")+
  geom_line(aes(x=lag_all_added, y=plotdat_all_2[,2], color="High Supervision"),linetype="solid")+
  geom_ribbon(aes(x=lag_all_added, ymin=plotdat_all_2[,13], ymax=plotdat_all_2[,14]), alpha=.3, fill="firebrick1")+
  geom_ribbon(aes(x=lag_all_added, ymin=plotdat_all_2[,9], ymax=plotdat_all_2[,10]), alpha=.3, fill="royalblue")+
  theme_set(theme_bw())+
  theme(legend.title=element_text(size=20), legend.position="bottom", panel.grid.minor.x=element_blank(),  panel.grid.minor.y=element_blank())+
  scale_fill_manual(values=c("royalblue", "firebrick"), name="")+
  scale_color_manual(values=c("royalblue","firebrick"), name="")+
  geom_rug(data=all_full_bind, aes(x = lag_all_added,y = NULL))
dev.off()


#################Figure 2 - Offshore papers

test_added_o<-seq(from=min(na.omit(o_full_new$lag_all_added)), to=max(na.omit(o_full_new$lag_all_added)), length.out=335)
test_tax_o<-rep(median(na.omit(o_full_new$increase_tax)), times=335)
test_v2csantimv_o<-rep(median(na.omit(o_full_new$v2csantimv_ord)), times=335)
test_diff_o<-rep(median(na.omit(o_full_new$lag_el_diff)), times=335)
test_brent_o<-rep(median(na.omit(o_full_new$lag_monthly_brent)), times=335)

plotdat_o<-as.data.frame(cbind(test_added_o, test_tax_o, test_v2csantimv_o, test_diff_o,
                               test_brent_o))

colnames(plotdat_o)<-c("lag_all_added", "increase_tax", "v2csantimv_ord", "lag_el_diff", "lag_monthly_brent")

td1_o<-predictions(m1_o_t, newdata=plotdat_o, part="outcome", type="unconditional")|>inferences(
  method = "boot", sim="balanced")

colnames(td1_o)[c(2,8,9,10)]<-c("estimate_td1", "std.error_td1", "conf.low_td1", "conf.high_td1")

td2_o<-predictions(m23_o_t, newdata=plotdat_o, part="outcome", type="unconditional")|>inferences(
  method = "boot", sim="balanced")

colnames(td2_o)[c(2,8,9,10)]<-c("estimate_td2", "std.error_td2", "conf.low_td2", "conf.high_td2")
td2_o<-td2_o[,c(2,8,9,10)]

plotdat_o_2<-as.data.frame(cbind(td1_o, td2_o))
plotdat_o_2<-plotdat_o_2[-c(334,335),]

plotdat_o_2 <- mutate_all(plotdat_o_2, function(x) as.numeric(as.character(x)))



#create plot
pdf("offshore_papers.pdf")
ggplot(plotdat_o_2)+ 
  labs(x="Number of Sanctions Added", y="Predicted Offshore Transactions")+
  geom_line(aes(x=lag_all_added, y=plotdat_o_2[,11], color="Low Supervision"),linetype="solid")+
  geom_line(aes(x=lag_all_added, y=plotdat_o_2[,2], color="High Supervision"),linetype="solid")+
  geom_ribbon(aes(x=lag_all_added, ymin=plotdat_o_2[,13], ymax=plotdat_o_2[,14]), alpha=.3, fill="firebrick1")+
  geom_ribbon(aes(x=lag_all_added, ymin=plotdat_o_2[,9], ymax=plotdat_o_2[,10]), alpha=.3, fill="royalblue")+
  theme_set(theme_bw())+
  theme(legend.title=element_text(size=20), legend.position="bottom", panel.grid.minor.x=element_blank(),  panel.grid.minor.y=element_blank())+
  scale_fill_manual(values=c("royalblue", "firebrick"), name="")+
  scale_color_manual(values=c("royalblue","firebrick"), name="")+
  geom_rug(data=o_full_new, aes(x = lag_all_added,y = NULL))
dev.off()

###################Figure 3 - Panama papers

m1_pan_t$coefficients
#increase_tax, transdiff_o, tax, v2x_rule, v2csantimv_ord, lag_el_diff, lag_monthly_brent, lag_all_added

test_added_pan<-seq(from=min(na.omit(pan_full_new$lag_all_added)), to=max(na.omit(pan_full_new$lag_all_added)), length.out=335)
test_tax_pan<-rep(median(na.omit(pan_full_new$increase_tax)), times=335)
test_v2csantimv_pan<-rep(median(na.omit(pan_full_new$v2csantimv_ord)), times=335)
test_diff_pan<-rep(median(na.omit(pan_full_new$lag_el_diff)), times=335)
test_brent_pan<-rep(median(na.omit(pan_full_new$lag_monthly_brent)), times=335)

plotdat_pan<-as.data.frame(cbind(test_added_pan, test_tax_pan, test_v2csantimv_pan, test_diff_pan,
                                 test_brent_pan))

colnames(plotdat_pan)<-c("lag_all_added", "increase_tax", "v2csantimv_ord", "lag_el_diff", "lag_monthly_brent")

###########generate predictions and conf int for each group 

td1_pan<-predictions(m1_pan_t, newdata=plotdat_pan, part="outcome", type="unconditional")|>inferences(
  method = "boot", sim="balanced")

colnames(td1_pan)[c(2,8,9,10)]<-c("estimate_td1", "std.error_td1", "conf.low_td1", "conf.high_td1")

td2_pan<-predictions(m23_pan_t, newdata=plotdat_pan, part="outcome", type="unconditional")|>inferences(
  method = "boot", sim="balanced")

colnames(td2_pan)[c(2,8,9,10)]<-c("estimate_td2", "std.error_td2", "conf.low_td2", "conf.high_td2")
td2_pan<-td2_pan[,c(2,8,9,10)]

plotdat_pan_2<-as.data.frame(cbind(td1_pan, td2_pan))
plotdat_pan_2<-plotdat_pan_2[-c(334,335),]

plotdat_pan_2 <- mutate_all(plotdat_pan_2, function(x) as.numeric(as.character(x)))

#create plot
pdf("panama_papers.pdf")
ggplot(plotdat_pan_2)+ 
  labs(x="Number of Sanctions Added", y="Predicted Offshore Transactions")+
  geom_line(aes(x=lag_all_added, y=plotdat_pan_2[,11], color="Low Supervision"),linetype="solid")+
  geom_line(aes(x=lag_all_added, y=plotdat_pan_2[,2], color="High Supervision"),linetype="solid")+
  geom_ribbon(aes(x=lag_all_added, ymin=plotdat_pan_2[,13], ymax=plotdat_pan_2[,14]), alpha=.3, fill="firebrick1")+
  geom_ribbon(aes(x=lag_all_added, ymin=plotdat_pan_2[,9], ymax=plotdat_pan_2[,10]), alpha=.3, fill="royalblue")+
  theme_set(theme_bw())+
  theme(legend.title=element_text(size=20), legend.position="bottom", panel.grid.minor.x=element_blank(),  panel.grid.minor.y=element_blank())+
  scale_fill_manual(values=c("royalblue", "firebrick"), name="")+
  scale_color_manual(values=c("royalblue","firebrick"), name="")+
  geom_rug(data=pan_full_new, aes(x = lag_all_added,y = NULL))
dev.off()

###################Figure 4 - Paradise papers

test_added_par<-seq(from=min(na.omit(par_full_new$lag_all_added)), to=max(na.omit(par_full_new$lag_all_added)), length.out=335)
test_tax_par<-rep(median(na.omit(par_full_new$increase_tax)), times=335)
test_v2csantimv_par<-rep(median(na.omit(par_full_new$v2csantimv_ord)), times=335)
test_diff_par<-rep(median(na.omit(par_full_new$lag_el_diff)), times=335)
test_brent_par<-rep(median(na.omit(par_full_new$lag_monthly_brent)), times=335)

plotdat_par<-as.data.frame(cbind(test_added_par, test_tax_par, test_v2csantimv_par, test_diff_par,
                                 test_brent_par))

colnames(plotdat_par)<-c("lag_all_added", "increase_tax", "v2csantimv_ord", "lag_el_diff", "lag_monthly_brent")

td1_par<-predictions(m1_par_t, newdata=plotdat_par, part="outcome", type="unconditional")|>inferences(
  method = "boot", sim="balanced")

colnames(td1_par)[c(2,8,9,10)]<-c("estimate_td1", "std.error_td1", "conf.low_td1", "conf.high_td1")

td2_par<-predictions(m23_par_t, newdata=plotdat_par, part="outcome", type="unconditional")|>inferences(
  method = "boot", sim="balanced")

colnames(td2_par)[c(2,8,9,10)]<-c("estimate_td2", "std.error_td2", "conf.low_td2", "conf.high_td2")
td2_par<-td2_par[,c(2,8,9,10)]

plotdat_par_2<-as.data.frame(cbind(td1_par, td2_par))
plotdat_par_2<-plotdat_par_2[-c(334,335),]

plotdat_par_2 <- mutate_all(plotdat_par_2, function(x) as.numeric(as.character(x)))


#create plot
pdf("paradise_papers.pdf")
ggplot(plotdat_par_2)+ 
  labs(x="Number of Sanctions Added", y="Predicted Offshore Transactions")+
  geom_line(aes(x=lag_all_added, y=plotdat_par_2[,11], color="Low Supervision"),linetype="solid")+
  geom_line(aes(x=lag_all_added, y=plotdat_par_2[,2], color="High Supervision"),linetype="solid")+
  geom_ribbon(aes(x=lag_all_added, ymin=plotdat_par_2[,13], ymax=plotdat_par_2[,14]), alpha=.3, fill="firebrick1")+
  geom_ribbon(aes(x=lag_all_added, ymin=plotdat_par_2[,9], ymax=plotdat_par_2[,10]), alpha=.3, fill="royalblue")+
  theme_set(theme_bw())+
  theme(legend.title=element_text(size=20), legend.position="bottom", panel.grid.minor.x=element_blank(),  panel.grid.minor.y=element_blank())+
  scale_fill_manual(values=c("royalblue", "firebrick"), name="")+
  scale_color_manual(values=c("royalblue","firebrick"), name="")+
  geom_rug(data=par_full_new, aes(x = lag_all_added,y = NULL))
dev.off()



###################################
################Appendix Tables 
###################################

###Appendix Table A1
#Country-level push factors

r2_a<-lm(ofc_all~increase_tax+increase_antimv+as.factor(wdicode_o)+as.factor(Year), data=all_full_new)
summary(r2_a) 

r2_23<-lm(n_ofc_g23_nona~increase_tax+increase_antimv+as.factor(wdicode_o)+as.factor(Year), data=all_full_new)
summary(r2_23) 

r2_1<-lm(n_ofc_g1_nona~increase_tax+increase_antimv+as.factor(wdicode_o)+as.factor(Year), data=all_full_new)
summary(r2_1) 

#reporting (Table A1): 
stargazer(r2_a, r2_23, r2_1, digits=3, keep=c(1,2,3))

########Appendix Table A2
#Country -level push factors, limited sample

all_full_no_added_SDN<-all_full_new[which(all_full_new$all_added==0),]

r2_n<-lm(ofc_all~increase_tax+increase_antimv+as.factor(wdicode_o)+as.factor(Year), data=all_full_no_added_SDN)
summary(r2_n) 

r2_n_23<-lm(n_ofc_g23_nona~increase_tax+increase_antimv+as.factor(wdicode_o)+as.factor(Year), data=all_full_no_added_SDN)
summary(r2_n_23) 

r2_n_1<-lm(n_ofc_g1_nona~increase_tax+increase_antimv+as.factor(wdicode_o)+as.factor(Year), data=all_full_no_added_SDN)
summary(r2_n_1) 

##reporting (Table A2):
stargazer(r2_n, r2_n_23, r2_n_1, digits=3, keep=c(1,2,3))

#######Alternate Rule of Law Measure
##########Appendix models A3 and A4

m23_x <- heckit(
  selection = offshore_d ~lag_all_added+v2x_rule+lag_el_diff+lag_monthly_brent,
  outcome = n_ofc_g23_nona ~ lag_all_added+lag_el_diff+v2x_rule,
  method = "2step",
  data = all_full_bind
)
summary(m23_x)

m1_x <- heckit(
  selection = offshore_d ~lag_all_added+v2x_rule+lag_el_diff+lag_monthly_brent,
  outcome = n_ofc_g1_nona ~ lag_all_added+lag_el_diff+v2x_rule,
  method = "2step",
  data = all_full_bind
)
summary(m1_x)

m23_o_x <- heckit(
  selection = offshore_d ~lag_all_added+v2x_rule+lag_el_diff+lag_monthly_brent,
  outcome = n_ofc_g23_nona ~ lag_all_added+lag_el_diff+v2x_rule,
  method = "2step",
  data = o_full_new
)
summary(m23_o_x)

m1_o_x <- heckit(
  selection = offshore_d ~lag_all_added+v2x_rule+lag_el_diff+lag_monthly_brent,
  outcome = n_ofc_g1_nona ~ lag_all_added+lag_el_diff+v2x_rule,
  method = "2step",
  data = o_full_new
)
summary(m1_o_x)

m23_pan_x <- heckit(
  selection = offshore_d ~lag_all_added+v2x_rule+lag_el_diff+lag_monthly_brent,
  outcome = n_ofc_g23_nona ~ lag_all_added+lag_el_diff+v2x_rule,
  method = "2step",
  data = pan_full_new
)
summary(m23_pan_x)

m1_pan_x <- heckit(
  selection = offshore_d ~lag_all_added+v2x_rule+lag_el_diff+lag_monthly_brent,
  outcome = n_ofc_g1_nona ~ lag_all_added+lag_el_diff+v2x_rule,
  method = "2step",
  data = pan_full_new
)
summary(m1_pan_x)

m23_par_x <- heckit(
  selection = offshore_d ~lag_all_added+v2x_rule+lag_el_diff+lag_monthly_brent,
  outcome = n_ofc_g23_nona ~ lag_all_added+lag_el_diff+v2x_rule,
  method = "2step",
  data = par_full_new
)
summary(m23_par_x)

m1_par_x <- heckit(
  selection = offshore_d ~lag_all_added+v2x_rule+lag_el_diff+lag_monthly_brent,
  outcome = n_ofc_g1_nona ~ lag_all_added+lag_el_diff+v2x_rule,
  method = "2step",
  data = par_full_new
)
summary(m1_par_x)


#Appendix Table A3
#Low supervision
#manually combine results tables
stargazer(m23_x, m23_o_x, m23_pan_x, m23_par_x, selection.equation=TRUE, digits=3) #need to combine these
stargazer(m23_x, m23_o_x, m23_pan_x, m23_par_x, digits=3)

#Appendix Table A4
#High supervision
#manually combine results tables
stargazer(m1_x, m1_o_x, m1_pan_x, m1_par_x, selection.equation=TRUE, digits=3) #need to combine these
stargazer(m1_x, m1_o_x, m1_pan_x, m1_par_x, digits=3)

###################
########Appendix Tables A5 and A6
###limited origin countries 

m23_t_lim <- heckit(
  selection = offshore_d ~lag_all_added+increase_tax+v2csantimv_ord+lag_el_diff+lag_monthly_brent,
  outcome = n_ofc_g23_nona ~ lag_all_added+lag_el_diff+increase_tax+v2csantimv_ord,
  method = "2step",
  data = lim_all
)
summary(m23_t_lim)

m1_t_lim <- heckit(
  selection = offshore_d ~lag_all_added+increase_tax+v2csantimv_ord+lag_el_diff+lag_monthly_brent,
  outcome = n_ofc_g1_nona ~ lag_all_added+lag_el_diff+increase_tax+v2csantimv_ord,
  method = "2step",
  data = lim_all
)
summary(m1_t_lim)

m23_o_t_lim <- heckit(
  selection = offshore_d ~lag_all_added+increase_tax+v2csantimv_ord+lag_el_diff+lag_monthly_brent,
  outcome = n_ofc_g23_nona ~ lag_all_added+lag_el_diff+increase_tax+v2csantimv_ord,
  method = "2step",
  data = lim_o
)
summary(m23_o_t_lim)

m1_o_t_lim <- heckit(
  selection = offshore_d ~lag_all_added+increase_tax+v2csantimv_ord+lag_el_diff+lag_monthly_brent,
  outcome = n_ofc_g1_nona ~ lag_all_added+lag_el_diff+increase_tax+v2csantimv_ord,
  method = "2step",
  data = lim_o
)
summary(m1_o_t_lim)

m23_pan_t_lim <- heckit(
  selection = offshore_d ~lag_all_added+increase_tax+v2csantimv_ord+lag_el_diff+lag_monthly_brent,
  outcome = n_ofc_g23_nona ~ lag_all_added+lag_el_diff+increase_tax+v2csantimv_ord,
  method = "2step",
  data = lim_pan
)
summary(m23_pan_t_lim)

m1_pan_t_lim <- heckit(
  selection = offshore_d ~lag_all_added+increase_tax+v2csantimv_ord+lag_el_diff+lag_monthly_brent,
  outcome = n_ofc_g1_nona ~ lag_all_added+lag_el_diff+increase_tax+v2csantimv_ord,
  method = "2step",
  data = lim_pan
)
summary(m1_pan_t_lim)

m23_par_t_lim <- heckit(
  selection = offshore_d ~lag_all_added+increase_tax+v2csantimv_ord+lag_el_diff+lag_monthly_brent,
  outcome = n_ofc_g23_nona ~ lag_all_added+lag_el_diff+increase_tax+v2csantimv_ord,
  method = "2step",
  data = lim_par
)
summary(m23_par_t_lim)

m1_par_t_lim <- heckit(
  selection = offshore_d ~lag_all_added+increase_tax+v2csantimv_ord+lag_el_diff+lag_monthly_brent,
  outcome = n_ofc_g1_nona ~ lag_all_added+lag_el_diff+increase_tax+v2csantimv_ord,
  method = "2step",
  data = lim_par
)
summary(m1_par_t_lim)

#####Table A5
#Low Supervision
#manually combine results tables
stargazer(m23_t_lim, m23_o_t_lim, m23_pan_t_lim, m23_par_t_lim, selection.equation=TRUE, digits=3) #need to combine these
stargazer(m23_t_lim, m23_o_t_lim, m23_pan_t_lim, m23_par_t_lim, digits=3)

#Table A6
#High Supervision
#manually combine results tables 
stargazer(m1_t_lim, m1_o_t_lim, m1_pan_t_lim, m1_par_t_lim, selection.equation=TRUE, digits=3) #need to combine these
stargazer(m1_t_lim, m1_o_t_lim, m1_pan_t_lim, m1_par_t_lim, digits=3)

#################
##########Appendix Tables A7 and A8 - Main Models, excluding Russia

all_no_r<-all_full_bind[which(all_full_bind$wdicode_o!="RUS"),]
o_no_r<-o_full_new[which(o_full_new$wdicode_o!="RUS"),]
par_no_r<-par_full_new[which(par_full_new$wdicode_o!="RUS"),]
pan_no_r<-pan_full_new[which(pan_full_new$wdicode_o!="RUS"),]

m23_nr <- heckit(
  selection = offshore_d ~lag_all_added+increase_tax+v2csantimv_ord+lag_el_diff+lag_monthly_brent,
  outcome = n_ofc_g23_nona ~ lag_all_added+lag_el_diff+increase_tax+v2csantimv_ord,
  method = "2step",
  data = all_no_r
)
summary(m23_nr)

m1_nr <- heckit(
  selection = offshore_d ~lag_all_added+increase_tax+v2csantimv_ord+lag_el_diff+lag_monthly_brent,
  outcome = n_ofc_g1_nona ~ lag_all_added+lag_el_diff+increase_tax+v2csantimv_ord,
  method = "2step",
  data = all_no_r
)
summary(m1_nr)

m23_o_nr <- heckit(
  selection = offshore_d ~lag_all_added+increase_tax+v2csantimv_ord+lag_el_diff+lag_monthly_brent,
  outcome = n_ofc_g23_nona ~ lag_all_added+lag_el_diff+increase_tax+v2csantimv_ord,
  method = "2step",
  data = o_no_r
)
summary(m23_o_nr)

m1_o_nr <- heckit(
  selection = offshore_d ~lag_all_added+increase_tax+v2csantimv_ord+lag_el_diff+lag_monthly_brent,
  outcome = n_ofc_g1_nona ~ lag_all_added+lag_el_diff+increase_tax+v2csantimv_ord,
  method = "2step",
  data = o_no_r
)
summary(m1_o_nr)

m23_pan_nr <- heckit(
  selection = offshore_d ~lag_all_added+increase_tax+v2csantimv_ord+lag_el_diff+lag_monthly_brent,
  outcome = n_ofc_g23_nona ~ lag_all_added+lag_el_diff+increase_tax+v2csantimv_ord,
  method = "2step",
  data = pan_no_r
)
summary(m23_pan_nr)

m1_pan_nr <- heckit(
  selection = offshore_d ~lag_all_added+increase_tax+v2csantimv_ord+lag_el_diff+lag_monthly_brent,
  outcome = n_ofc_g1_nona ~ lag_all_added+lag_el_diff+increase_tax+v2csantimv_ord,
  method = "2step",
  data = pan_no_r
)
summary(m1_pan_nr)

m23_par_nr <- heckit(
  selection = offshore_d ~lag_all_added+increase_tax+v2csantimv_ord+lag_el_diff+lag_monthly_brent,
  outcome = n_ofc_g23_nona ~ lag_all_added+lag_el_diff+increase_tax+v2csantimv_ord,
  method = "2step",
  data = par_no_r
)
summary(m23_par_nr)

m1_par_nr <- heckit(
  selection = offshore_d ~lag_all_added+increase_tax+v2csantimv_ord+lag_el_diff+lag_monthly_brent,
  outcome = n_ofc_g1_nona ~ lag_all_added+lag_el_diff+increase_tax+v2csantimv_ord,
  method = "2step",
  data = par_no_r
)
summary(m1_par_nr)

#Appendix Table A7
#Low Supervision
#manually combine results tables
stargazer(m23_nr, m23_o_nr, m23_pan_nr, m23_par_nr, selection.equation=TRUE, digits=3) #need to combine these
stargazer(m23_nr, m23_o_nr, m23_pan_nr, m23_par_nr, digits=3)

#Appendix Table A8
#High Supervision
#manually combine results tables
stargazer(m1_nr, m1_o_nr, m1_pan_nr, m1_par_nr, selection.equation=TRUE, digits=3) #need to combine these
stargazer(m1_nr, m1_o_nr, m1_pan_nr, m1_par_nr, digits=3)

################
######Appendix models A9 and A10 - including capital controls

m1_all_bind_k <- heckit(
  selection = offshore_d ~lag_all_added+v2csantimv_ord+lag_el_diff+lag_monthly_brent+lag_ka,
  outcome = n_ofc_g1_nona ~ lag_all_added+lag_el_diff+v2csantimv_ord+lag_ka,
  method = "2step",
  data = all_full_bind
)
summary(m1_all_bind_k)

m23_all_bind_k <- heckit(
  selection = offshore_d ~lag_all_added+v2csantimv_ord+lag_el_diff+lag_monthly_brent+lag_ka,
  outcome = n_ofc_g23_nona ~ lag_all_added+lag_el_diff+v2csantimv_ord+lag_ka,
  method = "2step",
  data = all_full_bind
)
summary(m23_all_bind_k)

m1_o_k <- heckit(
  selection = offshore_d ~lag_all_added+v2csantimv_ord+lag_el_diff+lag_monthly_brent+lag_ka,
  outcome = n_ofc_g1_nona ~ lag_all_added+lag_el_diff+v2csantimv_ord+lag_ka,
  method = "2step",
  data = o_full_new
)
summary(m1_o_k)

m23_o_k <- heckit(
  selection = offshore_d ~lag_all_added+v2csantimv_ord+lag_el_diff+lag_monthly_brent+lag_ka,
  outcome = n_ofc_g23_nona ~ lag_all_added+lag_el_diff+v2csantimv_ord+lag_ka,
  method = "2step",
  data = o_full_new
)
summary(m23_o_k)

m1_pan_k <- heckit(
  selection = offshore_d ~lag_all_added+v2csantimv_ord+lag_el_diff+lag_monthly_brent+lag_ka,
  outcome = n_ofc_g1_nona ~ lag_all_added+lag_el_diff+v2csantimv_ord+lag_ka,
  method = "2step",
  data = pan_full_new
)
summary(m1_pan_k)

m23_pan_k <- heckit(
  selection = offshore_d ~lag_all_added+v2csantimv_ord+lag_el_diff+lag_monthly_brent+lag_ka,
  outcome = n_ofc_g23_nona ~ lag_all_added+lag_el_diff+v2csantimv_ord+lag_ka,
  method = "2step",
  data = pan_full_new
)
summary(m23_pan_k)

m1_par_k <- heckit(
  selection = offshore_d ~lag_all_added+v2csantimv_ord+lag_el_diff+lag_monthly_brent+lag_ka,
  outcome = n_ofc_g1_nona ~ lag_all_added+lag_el_diff+v2csantimv_ord+lag_ka,
  method = "2step",
  data = par_full_new
)
summary(m1_par_k)

m23_par_k <- heckit(
  selection = offshore_d ~lag_all_added+v2csantimv_ord+lag_el_diff+lag_monthly_brent+lag_ka,
  outcome = n_ofc_g23_nona ~ lag_all_added+lag_el_diff+v2csantimv_ord+lag_ka,
  method = "2step",
  data = par_full_new
)
summary(m23_par_k)

###Reporting - Appendix Table A9
#Low Supervision
#manually combine tables
stargazer(m23_all_bind_k, m23_o_k, m23_pan_k, m23_par_k, selection.equation=TRUE, digits=3) #need to combine these
stargazer(m23_all_bind_k, m23_o_k, m23_pan_k, m23_par_k, digits=3)

#Reporting - Appendix Table A10
#High Supervision
#manually combine tables 
stargazer(m1_all_bind_k, m1_o_k, m1_pan_k, m1_par_k, selection.equation=TRUE, digits=3) #need to combine these
stargazer(m1_all_bind_k, m1_o_k, m1_pan_k, m1_par_k, digits=3)

#############
##########Appendix Table A11 - OLS models, country fixed effects

all_full_bind$n_ofc_g23<-all_full_bind$n_ofc_g2+all_full_bind$n_ofc_g3

outcome_m1<-lm(n_ofc_g23_nona~lag_all_added+increase_tax+v2csantimv_ord+lag_el_diff+lag_monthly_brent+as.factor(wdicode_o), data=all_full_bind)
summary(outcome_m1)

outcome_m3<-lm(n_ofc_g1_nona~lag_all_added+increase_tax+v2csantimv_ord+lag_el_diff+lag_monthly_brent+as.factor(wdicode_o), data=all_full_bind)
summary(outcome_m3)

#####Reporting, Table A11
stargazer(outcome_m1, outcome_m3, digits=3, omit="wdicode_o")




############################################


