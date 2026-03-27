# Data
load("dataset_2012.RData")

df<-subset(dataset, select=c("country.name", "ActMeetMun",
"country.name", "ActMeetMun", "ActContMun",  "ActContAut" ,                           
"ActMeetImp" , "ActSolveProb" ,  "ActMeetPty" ,  "ActContGvt",                            
"ActProtest" , "ActBlock" ,  "ActVoted",  "ActSign" ,                              
"ActShare" , "education" ,  "female", "age", "relative.incomeproxy" ,
"victim", "corrupt", "spatial.distance" ,
"compulsory.voting", "gdp.pc", "rule.of.lw", "enp", "social.spending"))                       
df<-na.exclude(df)


Y <- cbind(df$ActMeetMun, df$ActContMun, df$ActContAut,
		 df$ActContGvt, df$ActSolveProb, 
		df$ActMeetImp, df$ActMeetPty, df$ActVoted, 
		df$ActShare, df$ActSign, df$ActProtest, df$ActBlock)
colnames(Y)<-c("ActMeetMun", "ActContMun", "ActContAut", "ActContGvt",
		"ActSolveProb", "ActMeetImp", "ActMeetPty", "ActVoted",
		"ActShare", "ActSign", "ActProtest", "ActBlock")

Country<-as.numeric(factor(df$country.name), levels=unique(df$country.name))
		

Matrix.Country<-matrix(0,nrow(Y),length(unique(Country)))           
for (i in 1:length(unique(Country))) Matrix.Country[Country==i,i]<-1



library(Rcpp)
library(RcppArmadillo)
library(RcppGSL)

Sys.setenv("OMP_NUM_THREADS"="1")
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")



sourceCpp("LATAMsingleclass.cpp")


Results<-mcmc_probit (Y=Y, 
I=as.matrix(Matrix.Country),
tablecountry=table(Country),
alphastart=matrix(0,ncol(Y),ncol(Matrix.Country)),
alphastart_conv= matrix(0,ncol(Y),ncol(Matrix.Country)),
alphastart_unconv=matrix(0,ncol(Y),ncol(Matrix.Country)),
mcmc =150000,
burn=50000,
thin=10,
chains=3
 )



save(Results, file="Results_SingleclassModel")

