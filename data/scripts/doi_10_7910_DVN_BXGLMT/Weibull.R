## LOAD DATA 
df<-read.csv("Dataset.csv")

df<-subset(df, select=c("votes.centered",
			"country",
			"case_id",
			"merged", "dissolved",
			"log.partyyears",
			"logseats.centered",
		 "government",
			 "EU_parl",
		 "reg_gov",
		 "finance_party_qualify",
		 "efNPP",
	 "elec_coalition", 
	 "indPOL",
	"election_year_dummy",
	"distinctiveness",
	 "insider", 
	 "organisation", 
	 "newfamfirst",
	"OriginNew",
	"post.election.year",	
	"pre.election.year", 
	"seat.product"
	 ))

df<-na.exclude(df)



## DATA PREPARATION
country<-as.numeric(factor(df$country), levels=unique(df$country))
party<-as.numeric(factor(df$case_id), levels=unique(df$case_id))
num.country<-length(unique(country))
num.party<-length(unique(party))


# 1= survive, 2=merge, 3=dissolve
Y<-cbind(df$merged, df$dissolved)
Y<-cbind(ifelse(rowSums(Y)==0,1,0),Y)
y<-c(sapply(1:nrow(Y), function(i) which(Y[i,]==1)))
n<-length(y)         

Matrix.Country<-matrix(0,n,num.country)
for (i in 1:num.country) Matrix.Country[country==i,i]<-1

Matrix.Party<-matrix(0,n,num.party)
for (i in 1:num.party) Matrix.Party[party==i,i]<-1


Matrix.Outcome<-matrix(0, n, 3)
for (i in 1:3) Matrix.Outcome[y==i,i]<-1


df$OriginNew.newfamfirst<-df$OriginNew*df$newfamfirst


X<-cbind(1,  df$log.partyyears, 
	 df$insider, I(df$insider*df$logpartyyears), 
	 df$organisation, I(df$organisation*df$logpartyyears),
       df$newfamfirst,I(df$newfamfirst*df$logpartyyears),
	 df$OriginNew,I(df$OriginNew*df$logpartyyears),
	df$OriginNew.newfamfirst, I(df$OriginNew.newfamfirst*df$logpartyyears),
	df$votes.centered, 
	 df$logseats.centered, 	 df$government,
	 df$EU_parl,
	 df$reg_gov,  
	 df$finance_party_qualify,
	 (df$efNPP-mean(df$efNPP))/(2*sd(df$efNPP)),
	 df$elec_coalition, 
	 (df$indPOL-mean(df$indPOL))/(2*sd(df$indPOL)),
	 (df$seat.product-mean(df$seat.product))/(2*sd(df$seat.product)),
	df$election_year_dummy,
	df$post.election.year,	
	df$pre.election.year, 
	df$distinctiveness)


Matrix.Cov.Beta<-solve(diag(ncol(X))+(6/(pi^2))*crossprod(X))


## LOAD THE PACKAGES REQUIRED FOR RCPP AND SOURCE THE .CPP FILE
library(Rcpp)
library(RcppArmadillo)
sessionInfo()
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
sourceCpp("Weibull.cpp")

## RUN THE MCMC ALGORITHM AND STORE PARAMETER DRAWS
out<-mcmc_logit_index (y=matrix(y), 
X=as.matrix(X),
I_party=as.matrix(Matrix.Party),
I_country=as.matrix(Matrix.Country),
I_outcome=as.matrix(Matrix.Outcome),
Cov_Beta=as.matrix(Matrix.Cov.Beta),
beta2start=matrix(0, ncol(X)),
beta3start=matrix(0, ncol(X)),
reffect_partystart=matrix(0,ncol(Matrix.Party),2),
reffect_countrystart=matrix(0,ncol(Matrix.Country),2),
densitybeta=rep(0,ncol(X)),
DiagWish=diag(2),
mcmc=350000,
burn=150000,
thin=10,
chains=3
 )



## STORE THE PARAMETER DRAWS
save(out, file="Estimates_Weibull")








