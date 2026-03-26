gc()

source("./tools/toolbox.R")
# import the dfm
library("quanteda")
library(quanteda.textmodels)

strap<-T
####
load("./data/us_dfmn.rdata")

dfc<-dfm_subset(dfc,docvars(dfc,"date")>as.Date("2020-01-01"))

dfc<-dfm_trim(dfc,min_termfreq = 0.05,max_termfreq = 0.99,termfreq_type = "quantile",verbose=T)
dfc<-dfm_trim(dfc,min_termfreq = 4,termfreq_type = "count",verbose=T)

gc()

# build over-time party data set to maximize the main dimension
#dfg_tw<-dfm_group(dfc,group=docvars(dfc,"partyweekwin"),fill=T)
dfg_tw<-dfm_group(dfc,group=docvars(dfc,"partyweek"),fill=T)

dfg_tw_at<-dfm_select(dfg_tw,pattern = "@*",selection = "keep")
dfg_tw_nat<-dfm_select(dfg_tw,pattern = "@*",selection = "remove")
dfg_tw_nn<-dfm_select(dfg_tw_nat,pattern = "#*",selection = "remove")
gc()
# export into a format to do CCA


# export into a format to do CCA
m<-convert(dfg_tw_nn,to='data.frame')
me<-docvars(dfg_tw_nn)
names(me)
#rm(dfg_tw)
gc()

rownames(m)<-m[,1]
m<-m[,-1]
m[,1]
minm<-20
count<-rowSums(m)
me$count<-count
sel<-count>minm
m<-m[count>minm,]
me<-me[sel,]
me$count<-rowSums(m)

v1<-vegan::cca(m,Z=as.factor(me$week))


## extract the terms of the model
terms<-as.data.frame(v1$CA$v)
scores<-terms[,1]
# create a wordscore style object
names(scores)<-rownames(terms)
m3<-list()
m3$wordscores<-scores
class(m3)<-c("textmodel_wordscores","textmodel","list")
save(m3,file="models_new/mod_cnn.rdata")





#### nb
#load("./data/us_dfmn.rdata")


source("./tools/toolbox.R")
# import the dfm
library("quanteda")
library(quanteda.textmodels)

strap<-F
load("./data/us_dfmn.rdata")


dfg_tw2<-dfm_group(dfc,docvars(dfc,"fullname"))
#dfg_tw2<-dfm_subset(dfg_tw2,!is.na(docvars(dfg_tw2,"dim1")))
dfg_ind<-dfg_tw2
dfg_ind_at<-dfm_select(dfg_tw2,pattern = "@*",selection = "keep")
dfg_ind_nat<-dfm_select(dfg_tw2,pattern = "@*",selection = "remove")
dfg_ind_nn<-dfm_select(dfg_ind_nat,pattern = "#*",selection = "remove")





scores<-function(df,i=2,j=3){
  if(ncol(df)<i){df<-cbind(df,rep(0,i))}
  
  if(nrow(df)<i){df<-rbind(df,rep(0,i))}
  
  acc<-sum(diag(df))/sum(df)
  
  prec<-df[i,i]/sum(df[,i])
  rec<-df[i,i]/sum(df[i,])
  
  if(acc==1){prec<-1
  rec<-1}
  
  f1=2*prec*rec/ (prec+rec)
  return(c(round(acc,j),round(prec,j),round(rec,j),round(f1,j)))
}

docvars(dfg_ind_nn,"index")<-1:ndoc(dfg_ind_nn)

allscores<-c()

for(i in 1:10){
s1<-sample(ndoc(dfg_ind_nn),0.7*ndoc(dfg_ind_nn))

nn_t<-dfm_subset(dfg_ind_nn,
                 docvars(dfg_ind_nn,"index") %in% s1)
nn_v<-dfm_subset(dfg_ind_nn,!docvars(dfg_ind_nn,"index")%in% s1)

nb1a<-quanteda.textmodels::textmodel_nb(nn_t,docvars(nn_t,"party"))
a2<-predict(nb1a,nn_v)

allscores<-rbind(allscores,scores(table(docvars(nn_v,"party"),a2))
)
}

allscores<-as.data.frame(allscores)
names(allscores)<-c("Accuracy","Precision","Recall","F1")

stargazer(allscores,summary = F,out = "appendix/tables/table4.tex",type = "latex",label="cross-validation",title="10-Fold Cross-Validation")

nb1<-quanteda.textmodels::textmodel_nb(dfg_ind,docvars(dfg_ind,"party"))
nb2<-quanteda.textmodels::textmodel_nb(dfg_ind_at,docvars(dfg_ind_at,"party"))
nb3<-quanteda.textmodels::textmodel_nb(dfg_ind_nat,docvars(dfg_ind_nat,"party"))
nb4<-quanteda.textmodels::textmodel_nb(dfg_ind_nn,docvars(dfg_ind_nn,"party"))




save(nb1,file="models_new/nb_all.rdata")
save(nb2,file="models_new/nb_at.rdata")
save(nb3,file="models_new/nb_nat.rdata")
save(nb4,file="models_new/nb_nn.rdata")

save(dfg_ind,file="models_new/nb_dfm.rdata")
save(dfg_ind_at,file="models_new/nb_dfm_at.rdata")
save(dfg_ind_nat,file="models_new/nb_dfm_nat.rdata")
save(dfg_ind_nn,file="models_new/nb_dfm_nn.rdata")




i<-2
docvars(dfg_ind,"id")<-1:ndoc(dfg_ind)
strap<-T
if(strap==T){
t2<-matrix(NA,nrow=nfeat(dfg_ind),ncol=400)

for(i in 1:400){
  samp<-sample(docvars(dfg_ind,"id"),0.9*length(docvars(dfg_ind,"id")))  
  dfg_ind_sub<-dfm_subset(dfg_ind,subset = docvars(dfg_ind,"id")%in%samp)
  nb<-quanteda.textmodels::textmodel_nb(dfg_ind_sub,docvars(dfg_ind_sub,"party"))
  
  nb<-as.data.frame(t(log(nb$param)))
  t2[,i]<-nb[,1]/nb[,2]
}

t2<-as.data.frame(t2)
rownames(t2)<-colnames(nb1$param)


save(t2,file="./bootstrap/strap_nb.rdata")

rm(t2)
gc()
t3<-matrix(NA,nrow=nfeat(dfg_ind_at),ncol=400)

docvars(dfg_ind_at,"id")<-1:ndoc(dfg_ind_at)

for(i in 1:400){
  samp<-sample(docvars(dfg_ind_at,"id"),0.9*length(docvars(dfg_ind_at,"id")))  
  dfg_ind_sub<-dfm_subset(dfg_ind_at,subset = docvars(dfg_ind_at,"id")%in%samp)
  nb<-quanteda.textmodels::textmodel_nb(dfg_ind_sub,docvars(dfg_ind_sub,"party"))
  
  nb<-as.data.frame(t(log(nb$param)))
  t3[,i]<-nb[,1]/nb[,2]
}
t3<-as.data.frame(t3)
rownames(t3)<-colnames(nb2$param)


save(t3,file="bootstrap/strap_nb_at.rdata")

rm(t3)
gc()

docvars(dfg_ind_nat,"id")<-1:ndoc(dfg_ind_nat)

t4<-matrix(NA,nrow=nfeat(dfg_ind_nat),ncol=400)

for(i in 1:400){
  samp<-sample(docvars(dfg_ind_nat,"id"),0.9*length(docvars(dfg_ind_nat,"id")))  
  dfg_ind_sub<-dfm_subset(dfg_ind_nat,subset = docvars(dfg_ind_nat,"id")%in%samp)
  nb<-quanteda.textmodels::textmodel_nb(dfg_ind_sub,docvars(dfg_ind_sub,"party"))
  
  nb<-as.data.frame(t(log(nb$param)))
  t4[,i]<-nb[,1]/nb[,2]
}

t4<-as.data.frame(t4)
rownames(t4)<-colnames(nb3$param)
save(t4,file="bootstrap/strap_nb_nat.rdata")
rm(t4)
gc()


docvars(dfg_ind_nn,"id")<-1:ndoc(dfg_ind_nn)
t5<-matrix(NA,nrow=nfeat(dfg_ind_nn),ncol=400)
for(i in 1:400){
  samp<-sample(docvars(dfg_ind_nn,"id"),0.9*length(docvars(dfg_ind_nn,"id")))  
  dfg_ind_sub<-dfm_subset(dfg_ind_nn,subset = docvars(dfg_ind_nn,"id")%in%samp)
  nb<-quanteda.textmodels::textmodel_nb(dfg_ind_sub,docvars(dfg_ind_sub,"party"))
  
  nb<-as.data.frame(t(log(nb$param)))
  t5[,i]<-nb[,1]/nb[,2]
}


t5<-as.data.frame(t5)
rownames(t5)<-colnames(nb4$param)

save(t5,file="bootstrap/strap_nb_nn.rdata")



}






