

library(foreign)

 
## Note: "goodsites" are the locations where there was nearly full compliance with the assigned seating, and these are
## the sites we use for the results in the paper.  We get similar results from the "badsites" but for simplicity we 
## only use the goodsite data.  This is described in the paper.  The goodsites are:
## Humboldt, Riverside,Sacramento, and San Luis Obispo


### FOR ALL GOODSITES!!!!!!!!!!!!!!!

dataset<-read.dta("disagreement_raw_goodsites.dta")


# create the matrix of table assignments

TableAdj=matrix(data=0, nrow=length(dataset$sitetable), ncol=length(dataset$sitetable))
for (i in 1:length(dataset$sitetable)) {
for (j in 1:length(dataset$sitetable)) {
if (i!=j) { 
if (dataset$sitetable[i]==dataset$sitetable[j]) {
TableAdj[i,j]<-1
}}}}


# optional: normalize it to W

TableW=matrix(data=0, nrow=length(dataset$sitetable), ncol=length(dataset$sitetable))
for (i in 1:length(dataset$sitetable)) {
for (j in 1:length(dataset$sitetable)) {
TableW[i,j]<-TableAdj[i,j]/sum(TableAdj[i,])
}}


#### Create Vector of table mappings to read into Congdon programs:

TableMap<-list(map=NULL)
for (i in 1:length(dataset$sitetable)) {
for (j in 1:length(dataset$sitetable)) {
if (TableAdj[i,j]==1) TableMap<-list(map=c(TableMap$map, j))
}}

TableC<-list(C=0)
nadj<-0
for (i in 1:length(dataset$sitetable)) {
nadj<-sum(TableAdj[i,])+nadj
TableC<-list(C=c(TableC$C, nadj))
}

one<-rep(1, length(dataset$sitetable))
numTableNeigh<-sum(as.numeric(t(TableAdj%*%one)))

pre.missing.count<-as.numeric(is.na(dataset$sicko)) +
as.numeric(is.na(dataset$pre_change_stateprogram)) + 
as.numeric(is.na(dataset$pre_change_limitgovt)) + 
as.numeric(is.na(dataset$pre_agr_gonetoofar)) + 
as.numeric(is.na(dataset$pre_libconideo)) + 
as.numeric(is.na(dataset$pre_partyid))

pre.is.missing<-rep(0, length(dataset$sitetable))
pre.is.missing[pre.missing.count==6]<-1

pst.missing.count<-as.numeric(is.na(dataset$pst_agr_moreinformed)) +
as.numeric(is.na(dataset$pst_agr_pplrespectful)) +
as.numeric(is.na(dataset$pst_agr_othersheardviews)) + 
as.numeric(is.na(dataset$pst_agr_unbiasedmeeting)) + 
as.numeric(is.na(dataset$pst_agr_reasonablepts)) + 
as.numeric(is.na(dataset$pst_agr_oppspeak)) + 
as.numeric(is.na(dataset$pst_agr_civicduty)) + 
as.numeric(is.na(dataset$pst_agr_funtoday)) + 
as.numeric(is.na(dataset$pst_agr_viewchanged)) + 
as.numeric(is.na(dataset$pst_agr_votingresults)) + 
as.numeric(is.na(dataset$pst_agr_ldrsincorporate)) + 
as.numeric(is.na(dataset$pst_agr_participateagain)) 

pst.is.missing<-rep(0, length(dataset$sitetable))
pst.is.missing[pst.missing.count==12]<-1



## create variables 

data<-list(N=length(dataset$sitetable), NN=as.numeric(numTableNeigh), 
map=as.numeric(TableMap$map), C=as.numeric(TableC$C), 
pst.agr.moreinformed=as.numeric(dataset$pst_agr_moreinformed), 
pst.agr.pplrespectful=as.numeric(dataset$pst_agr_pplrespectful), 
pst.agr.othersheardviews=as.numeric(dataset$pst_agr_othersheardviews), 
pst.agr.unbiasedmeeting=as.numeric(dataset$pst_agr_unbiasedmeeting), 
pst.agr.reasonablepts=as.numeric(dataset$pst_agr_reasonablepts), 
pst.agr.oppspeak=as.numeric(dataset$pst_agr_oppspeak), 
pst.agr.civicduty=as.numeric(dataset$pst_agr_civicduty), 
pst.agr.funtoday=as.numeric(dataset$pst_agr_funtoday), 
pst.agr.viewchanged=as.numeric(dataset$pst_agr_viewchanged), 
pst.agr.votingresults=as.numeric(dataset$pst_agr_votingresults), 
pst.agr.ldrsincorporate=as.numeric(dataset$pst_agr_ldrsincorporate), 
pst.agr.participateagain=as.numeric(dataset$pst_agr_participateagain), 
pre.change.stateprogram=as.numeric(dataset$pre_change_stateprogram), 
pre.change.limitgovt=as.numeric(dataset$pre_change_limitgovt), 
sicko=as.numeric(dataset$sicko), 
pre.agr.gonetoofar=as.numeric(dataset$pre_agr_gonetoofar), 
pre.libconideo=as.numeric(dataset$pre_libconideo), 
pre.partyid=as.numeric(dataset$pre_partyid),
riverside=as.numeric(dataset$riverside),
sacto=as.numeric(dataset$sacto),
obispo=as.numeric(dataset$obispo),
smalltable=as.numeric(dataset$smalltable),
bigtable=as.numeric(dataset$bigtable),
pre.is.missing=pre.is.missing
# pst.is.missing=pst.is.missing,
)
dput(data, file="disagreement_data_goodsites.txt", control=NULL)

save.image("C:\\Users\\Kevin\\Documents\\Kevin\\disagreement\\DisagreementFinal\\dataset_goodsites.RData")



### FOR ALL BADSITES!!!!!!!!!!!!!!!

dataset<-read.dta("disagreement_raw_badsites.dta")


# create the matrix of table assignments

TableAdj=matrix(data=0, nrow=length(dataset$sitetable), ncol=length(dataset$sitetable))
for (i in 1:length(dataset$sitetable)) {
for (j in 1:length(dataset$sitetable)) {
if (i!=j) { 
if (dataset$sitetable[i]==dataset$sitetable[j]) {
TableAdj[i,j]<-1
}}}}


# optional: normalize it to W

TableW=matrix(data=0, nrow=length(dataset$sitetable), ncol=length(dataset$sitetable))
for (i in 1:length(dataset$sitetable)) {
for (j in 1:length(dataset$sitetable)) {
TableW[i,j]<-TableAdj[i,j]/sum(TableAdj[i,])
}}


#### Create Vector of table mappings to read into Congdon programs:

TableMap<-list(map=NULL)
for (i in 1:length(dataset$sitetable)) {
for (j in 1:length(dataset$sitetable)) {
if (TableAdj[i,j]==1) TableMap<-list(map=c(TableMap$map, j))
}}

TableC<-list(C=0)
nadj<-0
for (i in 1:length(dataset$sitetable)) {
nadj<-sum(TableAdj[i,])+nadj
TableC<-list(C=c(TableC$C, nadj))
}

one<-rep(1, length(dataset$sitetable))
numTableNeigh<-sum(as.numeric(t(TableAdj%*%one)))

pre.missing.count<-as.numeric(is.na(dataset$sicko)) +
as.numeric(is.na(dataset$pre_change_stateprogram)) + 
as.numeric(is.na(dataset$pre_change_limitgovt)) + 
as.numeric(is.na(dataset$pre_agr_gonetoofar)) + 
as.numeric(is.na(dataset$pre_libconideo)) + 
as.numeric(is.na(dataset$pre_partyid))

pre.is.missing<-rep(0, length(dataset$sitetable))
pre.is.missing[pre.missing.count==6]<-1

pst.missing.count<-as.numeric(is.na(dataset$pst_agr_moreinformed)) +
as.numeric(is.na(dataset$pst_agr_pplrespectful)) +
as.numeric(is.na(dataset$pst_agr_othersheardviews)) + 
as.numeric(is.na(dataset$pst_agr_unbiasedmeeting)) + 
as.numeric(is.na(dataset$pst_agr_reasonablepts)) + 
as.numeric(is.na(dataset$pst_agr_oppspeak)) + 
as.numeric(is.na(dataset$pst_agr_civicduty)) + 
as.numeric(is.na(dataset$pst_agr_funtoday)) + 
as.numeric(is.na(dataset$pst_agr_viewchanged)) + 
as.numeric(is.na(dataset$pst_agr_votingresults)) + 
as.numeric(is.na(dataset$pst_agr_ldrsincorporate)) + 
as.numeric(is.na(dataset$pst_agr_participateagain)) 

pst.is.missing<-rep(0, length(dataset$sitetable))
pst.is.missing[pst.missing.count==12]<-1



## create variables 

data<-list(N=length(dataset$sitetable), NN=as.numeric(numTableNeigh), 
map=as.numeric(TableMap$map), C=as.numeric(TableC$C), 
pst.agr.moreinformed=as.numeric(dataset$pst_agr_moreinformed), 
pst.agr.pplrespectful=as.numeric(dataset$pst_agr_pplrespectful), 
pst.agr.othersheardviews=as.numeric(dataset$pst_agr_othersheardviews), 
pst.agr.unbiasedmeeting=as.numeric(dataset$pst_agr_unbiasedmeeting), 
pst.agr.reasonablepts=as.numeric(dataset$pst_agr_reasonablepts), 
pst.agr.oppspeak=as.numeric(dataset$pst_agr_oppspeak), 
pst.agr.civicduty=as.numeric(dataset$pst_agr_civicduty), 
pst.agr.funtoday=as.numeric(dataset$pst_agr_funtoday), 
pst.agr.viewchanged=as.numeric(dataset$pst_agr_viewchanged), 
pst.agr.votingresults=as.numeric(dataset$pst_agr_votingresults), 
pst.agr.ldrsincorporate=as.numeric(dataset$pst_agr_ldrsincorporate), 
pst.agr.participateagain=as.numeric(dataset$pst_agr_participateagain), 
pre.change.stateprogram=as.numeric(dataset$pre_change_stateprogram), 
pre.change.limitgovt=as.numeric(dataset$pre_change_limitgovt), 
sicko=as.numeric(dataset$sicko), 
pre.agr.gonetoofar=as.numeric(dataset$pre_agr_gonetoofar), 
pre.libconideo=as.numeric(dataset$pre_libconideo), 
pre.partyid=as.numeric(dataset$pre_partyid),
sandiego=as.numeric(dataset$sandiego),
losangeles=as.numeric(dataset$losangeles),
fresno=as.numeric(dataset$fresno),
pre.is.missing=pre.is.missing,
smalltable=as.numeric(dataset$smalltable)
# pst.is.missing=pst.is.missing,
)
dput(data, file="disagreement_data_badsites.txt", control=NULL)

save.image("C:\\Users\\Kevin\\Documents\\Kevin\\disagreement\\DisagreementFinal\\dataset_badsites.RData")



## create inits

inits<-list(
eta_3=rnorm(length(dataset$sitetable)), 
lambda_1=runif(8),
lambda_2=runif(4),
lambda_3=runif(6),
k11_c1=-1, 
k11_c2=-.5, 
k11_c3=.5, 
k11_c4=1, 
k12_c1=-1, 
k12_c2=-.5, 
k12_c3=.5, 
k12_c4=1, 
k13_c1=-1, 
k13_c2=-.5, 
k13_c3=.5, 
k13_c4=1, 
k14_c1=-1, 
k14_c2=-.5, 
k14_c3=.5, 
k14_c4=1, 
k15_c1=-1, 
k15_c2=-.5, 
k15_c3=.5, 
k15_c4=1, 
k16_c1=-1, 
k16_c2=-.5, 
k16_c3=.5, 
k16_c4=1, 
k17_c1=-1, 
k17_c2=-.5, 
k17_c3=.5, 
k17_c4=1, 
k18_c1=-1, 
k18_c2=-.5, 
k18_c3=.5, 
k18_c4=1, 
k21_c1=-1, 
k21_c2=-.5, 
k21_c3=-.5, 
k21_c4=1, 
k22_c1=-1, 
k22_c2=-.5, 
k22_c3=.5, 
k22_c4=1, 
k23_c1=-1, 
k23_c2=-.5, 
k23_c3=.5, 
k23_c4=1, 
k24_c1=-1, 
k24_c2=-.5, 
k24_c3=.5, 
k24_c4=1, 
k31_c1=-1, 
k31_c2=-.5, 
k31_c3=.5, 
k31_c4=1, 
k32_c1=-1, 
k32_c2=-.5, 
k32_c3=.5, 
k32_c4=1, 
k33_c1=-1, 
k34_c2=-.5, 
k34_c3=.5, 
k34_c4=1, 
k35_c1=-1, 
k35_c2=-.5, 
k35_c3=.5, 
k35_c4=1, 
k36_c1=-1, 
k36_c2=-.5, 
k36_c3=.5, 
k36_c4=1, 
rho.eta=0.5,
g1.small=rnorm(1),
g1.big=rnorm(1),
g2.small=rnorm(1),
g2.big=rnorm(1),
g1.sitea=rnorm(1),
g1.siteb=rnorm(1),
g1.sitec=rnorm(1),
g2.sitea=rnorm(1),
g2.siteb=rnorm(1),
g2.sitec=rnorm(1),
g3.sitea=rnorm(1),
g3.siteb=rnorm(1),
g3.sitec=rnorm(1),
g1=rnorm(2),
g2=rnorm(2),
g3=rnorm(2),
g4=rnorm(2)
)
dput(inits, file="disagreement_inits3_goodsites.txt", control=NULL)



