##### ind
#setwd("..")



print("Loading and Processing Data...")
gc()
load("./data/validation_dfm.rdata")
source("./tools/toolbox.R")
library(quanteda.textmodels)
dfc2<-dfm_subset(dfc2,docvars(dfc2,"date")>as.Date("2020-01-01"))

dfc2<-dfm_subset(dfc2,docvars(dfc2,"bioname")!="VAN DREW, Jefferson")



strap<-F
names(docvars(dfc2))
#dfg_ind<-dfm_group(dfc,group=docvars(dfc,"fullname"),fill=T)
dfg_ind2<-dfm_group(dfc2,group=docvars(dfc2,"bioname"),fill=T)


print("Loading Done...Starting Predictions")


dfg_ts<-dfg_ind2


load("models_new/nb_all.rdata")
load("models_new/nb_dfm.rdata")


dfg_ts1<-dfm_match(dfg_ts,features=featnames(dfg_ind))
p1<-predict(nb1,dfg_ts1,type = "logposterior")
me_ts<-docvars(dfg_ts1)

me_ts$theta_nb<-p1[,1]/p1[,2]

load("models_new/nb_at.rdata")
load("models_new/nb_dfm_at.rdata")

dfg_ts1<-dfm_match(dfg_ts,features=featnames(dfg_ind_at))

print("Making Predictions...")

p1<-predict(nb2,dfg_ts1,type = "logposterior")
me_ts$theta_nb_at<-p1[,1]/p1[,2]

load("models_new/nb_dfm_nat.rdata")
load("models_new/nb_nat.rdata")
dfg_ts1<-dfm_match(dfg_ts,features=featnames(dfg_ind_nat))

p1<-predict(nb3,dfg_ts1,type = "logposterior")
me_ts$theta_nb_nat<-p1[,1]/p1[,2]

print("Making Predictions.......")


load("models_new/nb_dfm_nn.rdata")
load("models_new/nb_nn.rdata")
dfg_ts1<-dfm_match(dfg_ts,features=featnames(dfg_ind_nn))

p1<-predict(nb4,dfg_ts1,type = "logposterior")
me_ts$theta_nb_nn<-p1[,1]/p1[,2]

m6<-me_ts





if(strap==T){
print("Compute Boostrapped Predictions")  
  load("bootstrap/strap_nb_nat.rdata")
  dfg_ts1<-dfm_match(dfg_ts,features=featnames(dfg_ind_nat))
  ps<-predict_strap(t4,dfg_ts1)
  m6$theta<-rowMeans(ps)
  m6$theta_se<-apply(ps,MARGIN = 1,"sd")
  
  m6$theta<-log(m6$theta)*20
  m6$theta_se<-m6$theta_se*20
}else{m6$mean<-NA
  m6$theta_error<-NA}

#### Clean up dataset 

m6$count<-rowSums(dfg_ind2)
m6$dim1<-m6$nominate_dim1
m6<-m6[!is.na(m6$dim1),]
m6$party_code<-ifelse(m6$party_code==100,"D","R")
m6$party_code[grepl("sanders",m6$bioname,ignore.case = T)]<-"D"
m6$party_code[grepl("KING, Angus",m6$bioname,ignore.case = T)]<-"D"
m6$party<-as.factor(m6$party_code)



m6$theta_nb<-rescale_nominate(m6$theta_nb)
m6$theta_nb_at<-rescale_nominate(m6$theta_nb_at)
m6$theta_nb_nat<-rescale_nominate(m6$theta_nb_nat)
m6$theta_nb_nn<-rescale_nominate(m6$theta_nb_nn)


save(m6,file="data/compare_measures_rc.rdata")




load("data/compare_measures_rc.rdata")
m6$party_code[grepl("KING, Angus",m6$bioname,ignore.case = T)]

R<-m6[m6$party=="R",]


m6<-m6[m6$count>1000,]

vip<-c("GOSAR, Paul",
       "OCASIO-CORTEZ, Alexandria",
       "Ilhan_Omar","OMAR, Ilhan",
       "TLAIB, Rashida",
       "SPANBERGER, Abigail",
       "JORDAN, Jim",
       "KATKO, John",
       "KINZINGER, Adam",
       "UPTON, Frederick Stephen",
       "Andy_Biggs","BIGGS, Andrew S.",
       "Lauren_Boebert",
       "CHENEY, Liz","Liz_Cheney",
       "STEFANIK, Elise M",
       "SCALISE, Steve",
       "MCCARTHY, Kevin",
       "JAYAPAL, Pramila","Pramila_Jayapal" ,
       "Mondaire_Jones" ,
       "WASSERMAN SCHULTZ, Debbie",
       "CUELLAR, Henry",
       "CLYBURN, James Enos",
       "HOYER, Steny Hamilton",
       "PELOSI, Nancy")

plot_rc(m6,var="theta_nb_nn",var2=NULL,var3="nominate_dim1",vip=vip,ci = 3)


pdf(paste0("figures/figure1.pdf"),width = 12,height = 12)
plot_rc(m6,"theta_nb_nn",vip=vip,pcols=c("light blue","coral"), borders = FALSE)
dev.off()


#save(m6,file="./data/validation_individual.rdata")
#load("./data/validation_individual.rdata")

R<-m6[m6$party=="R",]
plot(R$nominate_dim1,R$theta_nb)

#m6$theta_nb_nn<-rescale_nominate(m6$theta_nb_nn)
m6$party_code[grepl("KING, Angus",m6$bioname,ignore.case = T)]<-"D"
m6$party<-as.factor(m6$party_code)

load("models_new/nb_nn.rdata")
load("models_new/nb_dfm_nn.rdata")

plot_rc(m6,var="theta_nb_nn",var2=NULL,var3="nominate_dim1",ci = 3,pcols=c("light blue","coral"))
plot_rc(m6,var="theta_nb",var2=NULL,var3="nominate_dim1",ci = 3,pcols=c("light blue","coral"))
plot_rc(m6,var="theta_nb_nat",var2=NULL,var3="nominate_dim1",ci = 3,pcols=c("light blue","coral"))

m6$party<-as.factor(m6$party_code)


plot_rc(m6,var="theta_nb_nn",var2=NULL,var3="nominate_dim1",ci = 3,pcols=c("light blue","coral"))


pdf(paste0("figures/figure2.pdf"),width = 12,height = 12)
par(mar = c(3, 3, 3, 7)) # Set the margin on all sides to 2
plot_ws(m6,var ="theta_nb_nn", nb4,dfm = dfg_ind_nn,limit=1000,n=160,pcols=c("light blue","coral"),sz = .8,borders=F,span = 2.4)
dev.off()


## export terms 

## Create figure 6

print("Plotting Figure 6 to Appendix")

pdf(paste0("appendix/figures/figure5.pdf"),width = 12,height = 12)
par(mfrow=c(2,2))
plot_rc(m6,"theta_nb",vip=vip,pcols=c("light blue","coral"))
plot_rc(m6,"theta_nb_at",vip=vip,pcols=c("light blue","coral"))
plot_rc(m6,"theta_nb_nat",vip=vip,pcols=c("light blue","coral"))
plot_rc(m6,"theta_nb_nn",vip=vip,pcols=c("light blue","coral"))
dev.off()


#print("Plotting Figure 7 to Appendix")

#if(strap==T){
#  pdf(paste0("appendix/figures/figure_x.pdf"),width = 12,height = 12)
#plot_rc(m6,var="mean",var2="theta_se",vip=NULL,ci = 3)
  ## add the terms here 
#  dev.off()
#}



ex1<-read.csv("data/expert_scores.csv",header = F, fileEncoding = 'latin1')
names(ex1)<-c("name","party_code","state","type","xscore")
ex1<-ex1[ex1$type=="Senator",]

nom<-strsplit(ex1$name," ")
ex1$lastname<-tolower(sapply(nom,"[[",2))


m7<-m6[m6$chamber=="Senate",]

nom<-strsplit(m7$bioname,",")
m7$lastname<-tolower(sapply(nom,"[[",1))


m8<-merge(m7,ex1,by=c("lastname","party_code"))
m8<-m8[m8$lastname!="scott",]


pdf(paste0("appendix/figures/figure10.pdf"),width = 12,height = 12)
plot_rc(m8,var="theta_nb_nn",var2=NULL,var3="xscore",ci = 3,lab2 = "Expert Scores",borders = F,pcols = c("blue","red"))
dev.off()


plot_rc(m8,var="theta_nb",var2=NULL,var3="xscore",ci = 3,lab2 = "Expert Scores",borders = F,pcols = c("blue","red"))
