### comparing the performance of the classifiers
source("./tools/toolbox.R")
load("./data/us_dfmn.rdata")
gc()
library(quanteda)

## to do:
load("models_new/nb_dfm_nn.rdata")
load("models_new/nb_nn.rdata")

# figure 6
dfg_tw<-dfm_group(dfc,group=docvars(dfc,"partyweekwin"),fill=T)

me<-docvars(dfg_tw)


dfg_ts1<-dfm_match(dfg_tw,features=featnames(dfg_ind_nn))
me_ts_p<-docvars(dfg_ts1)
p1<-predict(nb4,dfg_ts1,type = "logposterior")
me_ts_p$theta_nb_nn<-rescale_nominate(p1[,1]/p1[,2])


pddates<-as.Date(unique(docvars(dfc,"primary_date")),format="%d.%m.%Y")

m5<-me_ts_p

pcols<-c("blue","red")
m5$week<-as.Date(m5$week)



pdf("appendix/figures/figure6.pdf",width = 8,height = 12)
plot(m5$week,m5$theta_nb_nn,col=pcols[as.factor(m5$party)],pch=c(1,19)[as.factor(m5$winner)],ylab="Ideology",xlab="2020")
for(i in 1:length(pddates)){
  abline(v=pddates[i],col="grey")
}
abline(v=as.Date("2020-11-03"),col="red")
dev.off()




dfg_ts<-dfm_group(dfc,group=docvars(dfc,"partytimewin"),fill=T)
dfg_ts<-dfm_subset(dfg_ts,docvars(dfg_ts,"ttp")>-30)



## Naive Bayes 
load("models_new/nb_dfm_nn.rdata")
load("models_new/nb_nn.rdata")

dfg_ts1<-dfm_match(dfg_ts,features=featnames(dfg_ind))
p1<-predict(nb1,dfg_ts1,type = "logposterior")
me_ts<-docvars(dfg_ts1)
me_ts$winner<-factor(me_ts$winner,labels = c("Loser","Winner"))
me_ts$party<-as.factor(me_ts$party)

me_ts$count<-rowSums(dfg_ts1)

me_ts$theta_nb<-rescale_nominate(p1[,1]/p1[,2])





load("models_new/nb_at.rdata")
load("models_new/nb_dfm_at.rdata")

dfg_ts1<-dfm_match(dfg_ts,features=featnames(dfg_ind_at))

p1<-predict(nb2,dfg_ts1,type = "logposterior")
me_ts$theta_nb_at<-rescale_nominate(p1[,1]/p1[,2])

load("models_new/nb_dfm_nat.rdata")
load("models_new/nb_nat.rdata")
dfg_ts1<-dfm_match(dfg_ts,features=featnames(dfg_ind_nat))

p1<-predict(nb3,dfg_ts1,type = "logposterior")
me_ts$theta_nb_nat<-rescale_nominate(p1[,1]/p1[,2])
plot_ts(me_ts,"theta_nb_nat")


load("models_new/nb_dfm_nn.rdata")
load("models_new/nb_nn.rdata")
dfg_ts1<-dfm_match(dfg_ts,features=featnames(dfg_ind_nn))

p1<-predict(nb4,dfg_ts1,type = "logposterior")
me_ts$theta_nb_nn<-rescale_nominate(p1[,1]/p1[,2])




## change to -1 / 1 scaling 



pdf(paste0("appendix/figures/figure9.pdf"),width=14,height = 12)
par(mfrow=c(4,1))
plot_ts(me_ts,"theta_nb")
plot_ts(me_ts,"theta_nb_at")
plot_ts(me_ts,"theta_nb_nat")
plot_ts(me_ts,"theta_nb_nn")
dev.off()

load("models_new/mod_cnn.rdata")

p1<-pr_mod(m3,dfg_ts)
me_ts$theta_cnat<-p1$score

me_ts$winner<-factor(me_ts$winner,labels=c("Loser","Winner"))


save(me_ts,file="data/compare_measures_ts.rdata")








##################
#################
#################
#################
##################
################
#### Uncentered timeline

### The names of the dates are german WHY


dfg_ts<-dfm_group(dfc,group=docvars(dfc,"partyweekwin"),fill=T)
nrow(me_ts)


load("models_new/nb_all.rdata")
load("models_new/nb_dfm.rdata")

dfg_ts1<-dfm_match(dfg_ts,features=featnames(dfg_ind))
p1<-predict(nb1,dfg_ts1,type = "logposterior")
me_ts2<-docvars(dfg_ts1)

me_ts2$theta_nb<-rescale_nominate(p1[,1]/p1[,2])



load("models_new/nb_at.rdata")
load("models_new/nb_dfm_at.rdata")

dfg_ts1<-dfm_match(dfg_ts,features=featnames(dfg_ind_at))

p1<-predict(nb2,dfg_ts1,type = "logposterior")
me_ts2$theta_nb_at<-rescale_nominate(p1[,1]/p1[,2])






load("models_new/nb_dfm_nat.rdata")
load("models_new/nb_nat.rdata")
dfg_ts1<-dfm_match(dfg_ts,features=featnames(dfg_ind_nat))

p1<-predict(nb3,dfg_ts1,type = "logposterior")
me_ts2$theta_nb_nat<-rescale_nominate(p1[,1]/p1[,2])


load("models_new/nb_dfm_nn.rdata")
load("models_new/nb_nn.rdata")
dfg_ts1<-dfm_match(dfg_ts,features=featnames(dfg_ind_nn))

p1<-predict(nb4,dfg_ts1,type = "logposterior")
me_ts2$theta_nb_nn<-rescale_nominate(p1[,1]/p1[,2])


load("models_new/mod_cnn.rdata")

p1<-pr_mod(m3,dfg_ts)
me_ts2$theta_cnat<-p1$score

me_ts2$winner<-factor(me_ts2$winner,labels=c("Loser","Winner"))

#pdf(paste0("appendix/figures/figure9.pdf"),width=12,height = 12)
#par(mfrow=c(2,1))
#plot_ts(me_ts2,"theta_cnat",time = "week")
#plot_ts(me_ts2,"theta_nb_nn",time = "week")
#plot_ts(me_ts2,"theta_cnat",time = "week")
#plot_ts(me_ts2,"theta_nb_nn",time = "week")
#dev.off()



save(me_ts,file="data/compare_measures_tso.rdata")

load("data/compare_measures_tso.rdata")



#pdf("appendix/figures/figure7.pdf",width=20,height = 12)
#par(mfrow=c(4,1))
#plot_ts(me_ts2,"theta_nb",time = "week")
#plot_ts(me_ts2,"theta_nb_at",time = "week")
#plot_ts(me_ts2,"theta_nb_nat",time = "week")
#plot_ts(me_ts2,"theta_nb_nn",time = "week")
#dev.off()


pdf("appendix/figures/figure7.pdf",width=8,height = 12)
plot_ts(me_ts2,"theta_nb_nn",time = "week")
dev.off()


me_ts3<-me_ts2[me_ts2$week>as.POSIXct("2019-03-01"),]


pdf("appendix/figures/figure8.pdf",width = 8,height = 14)
par(mfrow=c(2,1))
plot_ts(me_ts,"theta_nb_nn")
plot_ts(me_ts,"theta_cnat")
dev.off()


##################
#################
#################
#################
##################
################
