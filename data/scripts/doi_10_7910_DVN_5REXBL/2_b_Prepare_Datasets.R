## This script will create the datasets for analysis




print("Importing data and trained models")
source("./tools/toolbox.R")
load("./data/us_dfmn.rdata")

length(unique(docvars(dfc,"fullname")))

gc()
load("models_new/nb_dfm_nn.rdata")
load("models_new/nb_nn.rdata")
## Import all the logposteriors of the 400 straps
load("bootstrap/strap_nb_nn.rdata")
gc()



### Count the tweets

df<-docvars(dfc)

df$polit<-ifelse(df$pol=="policy",1,0)
pol<-aggregate(polit~party+ttp+winner,data=df,FUN="mean")

pol2<-aggregate(polit~party+ttp+winner,data=df,FUN="length")

pol$party<-as.factor(pol$party)
pol$winner<-as.factor(pol$winner)
pol$n<-pol2$polit

pol3<-aggregate(polit~party+ttp+winner,data=df,FUN="sum")

pol$poltweets<-pol3$polit

rm(df)

# Create  a subset of policy Tweets
dfc2<-dfm_subset(dfc,docvars(dfc,"pol")=="policy")

print("Aggregating...")

# Aggregate both datasets, the Policy only and the Full set, 
dfg_ts<-dfm_group(dfc,group=docvars(dfc,"partytimewin"),fill=T)
dfg_ts2<-dfm_group(dfc2,group=docvars(dfc2,"partytimewin"),fill=T)


gc()
# all

print("Start Predicting Main Analysis Set...")
# Predict them both with the NB Model
dfg_ts<-dfm_match(dfg_ts,features=featnames(dfg_ind_nn))
dfg_ts2<-dfm_match(dfg_ts2,features=featnames(dfg_ind_nn))


print("Bootstrapping...")
ps<-predict_strap(t5,dfg_ts)
hist(ps[1,])
# We compute the mean, and the standard deviation of the posterior density
me<-docvars(dfg_ts)


print("Create Theta...")


me$theta<-rowMeans(ps)
me$theta_se<-apply(ps,MARGIN = 1,"sd")




dfg_ts<-dfm_match(dfg_ts2,features=featnames(dfg_ind_nn))


ps<-predict_strap(t5,dfg_ts)
hist(ps[1,])
# We compute the mean, and the standard deviation of the posterior density

me$theta_r<-rowMeans(ps)
me$theta_r_se<-apply(ps,MARGIN = 1,"sd")


me$winner<-as.factor(me$winner)


me<-merge(me,pol,by=c("ttp","winner","party"))


print("Save to file for analysis")

save(me,file="data/main_analysis.rdata")

me<-me[me$ttp>=-25,]
me<-me[me$ttp<=25,]







print("Step 2: two-stage Individual Analysis")


## Two-stage individual Analysis 


dft<-dfm_subset(dfc,docvars(dfc,"ttp")<10)
dft<-dfm_subset(dft,docvars(dft,"ttp")>-10)

df<-docvars(dft)
docvars(dft,"dup")<-duplicated(df$status_id)

table(docvars(dft,"second_name"))
docvars(dft,"uncontested")<-ifelse(docvars(dft,"second_name")=="none none",1,0)



docvars(dft,"before")<-docvars(dft,"ttp")<0
docvars(dft,"fullname_before")<-paste(docvars(dft,"fullname"),docvars(dft,"before"))


dfg_tw<-dfm_group(dft,group=docvars(dft,"fullname_before"),fill=T)


print("Predicting...")


## Predicting Two Stage Data
dfg_tw1<-dfm_match(dfg_tw,features=featnames(dfg_ind_nn))

dfg_ts<-dfg_tw
t2<-t5
i<-2

print("Bootstrapping Errors...")

ps<-predict_strap(t5,dfg_tw1)
hist(ps[1,])

# We compute the mean, and the standard deviation of the posterior density

me<-docvars(dfg_tw)
me$theta<-rowMeans(ps)
me$theta_se<-apply(ps,MARGIN = 1,"sd")

#quanteda.textmodels::textplot_scale1d(w2, groups = docvars(dfg_tw,"party"))




m5<-me

before<-m5[m5$before==T,]
after<-m5[m5$before==F,]



after<-after[,c("fullname","theta","theta_se")]
names(after)[2:3]<-paste0(names(after)[2:3],"_after")

full<-merge(before,after,by="fullname")


print("Compute and Normalize Theta...")

full$theta<-log(full$theta)*20
full$theta_se<-full$theta_se*20


full$theta_after<-log(full$theta_after)*20
full$theta_after_se<-full$theta_after*20



print("Compute Movement...")

full$move<-full$theta_after-full$theta



er<-2
full$move3<-ifelse((full$theta_after-er*full$theta_se_after)>(full$theta+er*full$theta_se),1,0)
full$move3<-ifelse((full$theta_after+er*full$theta_se_after)<(full$theta-er*full$theta_se),-1,full$move3)
table(full$move3)

er<-1
full$move1<-ifelse((full$theta_after-er*full$theta_se_after)>(full$theta+er*full$theta_se),1,0)
full$move1<-ifelse((full$theta_after+er*full$theta_se_after)<(full$theta-er*full$theta_se),-1,full$move1)

table(full$move3)

print("Save for Individual Robustness Analysis")

save(full,file="./data/individual_diff.rdata")

####

load("data/main_analysis.rdata")
me<-me[me$ttp>=-25,]
me<-me[me$ttp<=25,]

names(me)

me$position<-log(me$theta)
me$position_policy<-log(me$theta_r)

me$position_20<-me$position*20
me$position_policy<-me$position_policy*20

me$panel<-NA
me$panel<-ifelse(me$party=="R" & me$winner==0,0,me$panel)
me$panel<-ifelse(me$party=="R" & me$winner==1,1,me$panel)
me$panel<-ifelse(me$party=="D" & me$winner==0,10,me$panel)
me$panel<-ifelse(me$party=="D" & me$winner==1,11,me$panel)

me$democractic<-ifelse(me$party=="D",1,0)

haven::write_dta(me,"./data/fuller.dta")






