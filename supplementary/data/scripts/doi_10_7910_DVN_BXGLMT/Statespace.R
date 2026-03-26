## LOAD DATA 
df<-read.csv("Dataset.csv")

df<-subset(df, select=c("votes.centered",
			"country",
			"case_id",
			"merged", "dissolved",
			"year_exist",
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

duration<-as.numeric(factor(df$year_exist), levels=unique(df$year_exist))
num.duration<-length(unique(duration))

Matrix.Duration<-matrix(0,n,num.duration)
for (i in 1:num.duration) Matrix.Duration[duration==i,i]<-1

Matrix.Outcome<-matrix(0, n, 3)
for (i in 1:3) Matrix.Outcome[y==i,i]<-1



W<-cbind(df$votes.centered, 
	 df$logseats.centered,  
	 df$government,
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

X<-cbind(1,  
	 df$insider, 
	 df$organisation, 
        df$newfamfirst,
	 df$OriginNew,
	I(df$OriginNew*df$newfamfirst)
	 )



Matrix.Cov.Beta<-solve(diag(ncol(W))+(6/(pi^2))*crossprod(W))

Matrix.Index.Durations<-matrix(0, length(unique(party)), num.duration)
table.durations<-c()

for (i in 1:num.duration) {

Matrix.Index.Durations[,i]<-c(sort(which(duration==i))-1, rep(0,length(unique(party))-length(sort(which(duration==i)))))
table.durations<-c(table.durations, length(sort(which(duration==i)))-1)

}

Matrix.ColIndex.Party<-matrix(0, length(unique(party)), num.duration)
table.durations.colparty<-c()
for (i in 1:num.duration) {

Matrix.ColIndex.Party[,i]<-c(sort(party[which(duration==i)])-1,rep(0,length(unique(party))-length(sort(party[which(duration==i)]))))
table.durations.colparty<-c(table.durations.colparty, length(sort(party[which(duration==i)]))-1)
}


Matrix.ColIndex.Country<-matrix(0, length(unique(country)), num.duration)
table.durations.colcountry<-c()
for (i in 1:num.duration) {

Matrix.ColIndex.Country[,i]<-c(sort(unique(country[which(duration==i)]))-1,rep(0,length(unique(country))-length(sort(unique(country[which(duration==i)])))))
table.durations.colcountry<-c(table.durations.colcountry, length(sort(unique(country[which(duration==i)])))-1)
}


## LOAD THE PACKAGES REQUIRED FOR RCPP AND SOURCE THE .CPP FILE
library(Rcpp)
library(RcppArmadillo)
sessionInfo()
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
sourceCpp("Statespace.cpp")



## RUN THE MCMC ALGORITHM AND STORE PARAMETER DRAWS
out<-mcmc_logit_index (y=matrix(y), 
X=as.matrix(X),
W=as.matrix(W),
I_party=as.matrix(Matrix.Party),
I_country=as.matrix(Matrix.Country),
I_duration=as.matrix(Matrix.Duration),
I_index_duration=as.matrix(Matrix.Index.Durations),
table_duration=c(table.durations),
I_colindex_party=as.matrix(Matrix.ColIndex.Party),
table_duration_colparty=c(table.durations.colparty),
I_colindex_country=as.matrix(Matrix.ColIndex.Country),
table_duration_colcountry=c(table.durations.colcountry),
I_outcome=as.matrix(Matrix.Outcome),
Cov_Beta=as.matrix(Matrix.Cov.Beta),
alphastart=matrix(0,2*ncol(X), num.duration),
beta2start=matrix(0, ncol(W)),
beta3start=matrix(0, ncol(W)),
reffect_partystart=matrix(0,ncol(Matrix.Party),2),
reffect_countrystart=matrix(0,ncol(Matrix.Country),2),
densitybeta=rep(0,ncol(W)),
DiagWish=diag(2),
icol0=0,
icol1=1,
icol2=2,
mcmc=350000,
burn=150000,
thin=10,
chains=3
 )



## STORE THE PARAMETER DRAWS
save(out, file="Estimates_Statespace")
 









