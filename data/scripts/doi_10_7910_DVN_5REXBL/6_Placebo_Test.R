#### PLACEBO timeline
Sys.setenv(TZ='Europe/London')
source("./tools/toolbox.R")

load("./data/us_dfmn.rdata")

tim<-as.POSIXct(docvars(dfc,"created_at"))
docvars(dfc,"time")<-tim
docvars(dfc,"partyplacwin")<-paste0(docvars(dfc,"party"),"-",docvars(dfc,"ttp_plac"),"-",docvars(dfc,"winner"))


prim<-as.POSIXct(docvars(dfc,"primary_date"),format = "%d.%m.%Y")

df<-docvars(dfc)
nom<-unique(df$fullname)
p1<-unique(prim)
df$placebo_date<-df$created_at
for(i in 1:length(nom)){
  plac<-sample(prim,1)
  df[df$fullname==nom[i],"placebo_date"]<-rep(as.POSIXct(plac),nrow(df[df$fullname==nom[i],]))
  
}
df$placebo_date

t2<-df[df$fullname==nom[1],]

t2$primary_date
t2$placebo_date

docvars(dfc,"placebo_date")<-df$placebo_date

docvars(dfc,"ttp_plac")<-round(difftime(tim,plac,unit="weeks"),0)
docvars(dfc,"partyplacwin")<-paste0(docvars(dfc,"party"),"-",docvars(dfc,"ttp_plac"),"-",docvars(dfc,"winner"))


dfg_ts<-dfm_group(dfc,group=docvars(dfc,"partyplacwin"),fill=T)




load("models_new/nb_dfm_nn.rdata")
load("models_new/nb_nn.rdata")
dfg_ts1<-dfm_match(dfg_ts,features=featnames(dfg_ind_nn))
me_ts_p<-docvars(dfg_ts1)
p1<-predict(nb4,dfg_ts1,type = "logposterior")
me_ts_p$theta_nb_nn<-rescale_nominate(p1[,1]/p1[,2])


load("models_new/mod_cnn.rdata")

p1<-pr_mod(m3,dfg_ts1)
me_ts_p$theta_cnat<-p1$score

me_ts_p$winner<-factor(me_ts_p$winner,labels=c("Loser","Winner"))


me_ts_p<-me_ts_p[me_ts_p$ttp_plac<25,]
me_ts_p<-me_ts_p[me_ts_p$ttp_plac>-25,]

#me_ts_p$ttp_plac<-me_ts_p$ttp_plac*-1

## Plotting Placebo Test

pdf("appendix/figures/figure7.pdf",width = 10,height = 8)
plot_ts(me_ts_p,var = "theta_nb_nn",time="ttp_plac")
dev.off()


plot_ts(me_ts_p,var = "theta_cnat",time="ttp_plac")

me<-me_ts_p

me$dep<-me$theta
me$dep<-me$theta_cnat
me$interven<-me$ttp_plac>0

me$winner<-factor(me$winner,labels=c("Loser","Winner"))
me$loser<-relevel(me$winner,ref = "Winner")


checkm1_d<-lm(dep~interven+loser+loser*interven,data=me[me$party=="D",])
summary(checkm1_d)


checkm1_d<-lm(dep~ttp_plac+interven+ttp_plac*interven+loser+loser*interven+loser*ttp_plac+loser*ttp_plac*interven,data=me[me$party=="D",])
summary(checkm1_d)



summary(checkm1_d)

level_order<-c("Time to Pseudo Primary (T\\textsubscript{t})","After Pseudo Primary (X\\textsubscript{t})","Loser (Z\\textsubscript{i})","TTP:After Pseudo Primary (X\\textsubscript{t}T\\textsubscript{t})","After Pseudo Primary: Loser (Z\\textsubscript{i}T\\textsubscript{t}) ","TTP: Loser","TTP:After Pseudo Primary: Loser (Z\\textsubscript{i}X\\textsubscript{t}T\\textsubscript{t})",'Intercept')

stargazer(checkm1_d,type="latex",out ="./appendix/tables/table5.tex",covariate.labels = level_order,dep.var.labels = "Position",label = "its_pseudo",title = "ITS Results: Placebo Dates",column.labels = c("Democrats","Republicans"),style = "apsr")

