#File Name: IdeoRatings_estimation.R
#Data: ideo_ratings.csv, ideo_prior.csv, names.csv, dept.dta, acr.dta
#Purpose: Generate informed and baseline estimates of agency ideology

#Note: There are multiple file paths below that need to be changed to the working directory you are using
#before running the code.

require(runjags)
library(coda)
library(readstata13)

##################################
#Load files ####
##################################

ideo<-read.csv(file="C:/Users/mdr/Dropbox/IdeologyPaper/JOP Dataverse/ideo_ratings.csv") #load ratings
prior<-read.csv(file="C:/Users/mdr/Dropbox/IdeologyPaper/JOP Dataverse/ideo_prior.csv",stringsAsFactors = F) #load priors
names<-read.csv(file="C:/Users/mdr/Dropbox/IdeologyPaper/JOP Dataverse/ideo_names.csv",stringsAsFactors = F) #load mapping of agency names

##################################
#Assemble ratings data ####
##################################

for(i in 1:ncol(ideo)){
  ideo[,i][ideo[,i]==99]<-NA #recode Don't knows to missing
}
table(ideo$ideo_omb) #no 99's

#Get ideo rating with raters that rated 3+ agecncies and agencies with 5+ ratings

#Remove raters that didn't rate at least 3 items
for(i in 1:nrow(ideo)){# get number of ratings per rater
  ideo[i,169]<-length(ideo[i,-169][is.na(ideo[i,-169])==F]) #get number of nonmissing, exclude last column of number of ratings
}

table(ideo$ideo_n_ratings) #Could have more than 8 ratings from combinations of paper and online surveys;

#Keep raters 3+
ideo<-ideo[which(ideo$ideo_n_ratings>=3),]

table(ideo$ideo_n_ratings) #limited to those that rated 3+ items

ideo$ideo_n_ratings<-NULL #remove number of ratings per rater, no longer needed

#turn ideo data.frame into a matrix and transpose it so that rows are agencies and columns are raters
ideo<-as.data.frame(t(ideo))

rownames(ideo)<-names[,1]

ideo$r_per_agency<-NA

for(i in 1:nrow(ideo)){# get ideo ratings per agency
  ideo$r_per_agency[i]<-length(ideo[i,][is.na(ideo[i,])==F])
}

View(ideo[ncol(ideo)])

prior<-prior[which(ideo$r_per_agency>=5),] #remove agencies from prior list

ideo<-ideo[which(ideo$r_per_agency>=5),] #keep only agencies with 5+ ratings

prior$Agency==rownames(ideo) #true (false for cabinet departments due to (All))

View(ideo[ncol(ideo)])

ideo$r_per_agency<-NULL #remove ratings per agency

s<-as.matrix(ideo)

#Check to be sure each rater still has 3 ratings

t<-vector(length=ncol(s))

for(i in 1:ncol(s)){# get number of ratings per rater
  t[i]<-length(s[,i][is.na(s[,i])==F]) #get number of nonmissing
}

table(t) #2 raters with 2 ratings

s<-s[,which(t>2)] #remove them

#Check ratings per agency again

t<-vector(length=nrow(s))

for(i in 1:nrow(s)){# get ratings per agency
  t[i]<-length(s[i,][is.na(s[i,])==F])
}

table(t) #all agencies have at least 5 ratings

rm(t)

#create vectors of ratings
y<-NA
a<-NA
r<-NA

for(i in 1:nrow(s)){
  for(j in 1:ncol(s)){
    if(is.na(s[i,j])==F){ #skip if there is no rating
      y<-c(y,s[i,j]) #vector of ratings
      a<-c(a,i) # agency index
      r<-c(r,j) # rater index
    }
  }
}

#remove the first element; it's NA due to creation of vector above;
y<-y[-1]
a<-a[-1]
r<-r[-1]

x<-cbind(y,a,r)

#relabel columns of s so rater index is sequential and raters can be matched to beta coefficients
t<-1:ncol(s)
colnames(s)<-t
rm(t)

N<-length(y) #number of ratings
A<-length(unique(a)) #number of agencies
R<-length(unique(r)) #number of raters
###################################
#Assemble data for JAGS #####
###################################

forJags<-list(y=y,N=N,a=a,A=A,r=r,R=R,u=as.vector(prior$mean),pre=as.vector(prior$pre))

forJags.dump<-dump.format(forJags)

#################################
#create jags model ####
#################################
model<-"model{
  for(i in 1:N){ #loop over N ratings
    mu[i]<-beta[r[i],1]+beta[r[i],2]*xi[a[i]] #a indexes agencies, r indexes raters
    y[i] ~ dnorm(mu[i],tau[a[i]]) 
  }
  
  # prior latent agency ideology
  for(j in 1:A){
    xi[j] ~ dnorm(u[j],pre[j])
  }
  
 
  for(j in 1:A){     
    # Assume all raters are equally precise raters of the same agency (same measurement error variance)
    sigma[j] ~ dunif(0,10) #standard deviation
    tau[j] <- pow(sigma[j],-2) #convert to precision
  }
  
  #priors for the rater parameters for R raters
  for(k in 1:R){
    ## intercepts and slopes
    beta[k,1] ~ dnorm(0,0.01)
    beta[k,2] ~ dnorm(0,0.01)
  }
}"

#########################
#Execute model run ####
#########################
#Note: Need to set random number generator seeds when using parallel cores or warning is given
#and seeds are added to inits by runjags
foo.inits<-list(list(xi=rep.int(1,times=165),.RNG.name="base::Mersenne-Twister",.RNG.seed=1),
                list(xi=rep.int(5,times=165),.RNG.name="base::Mersenne-Twister",.RNG.seed=5))

setwd("G:/Data/Ratings/Ideology/Informed/") #set wd for jagsfiles

foo<-run.jags(model=model,monitor=c("beta","sigma","xi"),
              data=forJags.dump,inits=foo.inits,n.chains=2,adapt=10000,burnin=3000000,
              sample=4000,thin=500, method="parallel",keep.jags.files=T)

#save as an MCMC list
sample<-as.mcmc.list(foo)

##########################################
#Save model ####
##########################################
date<-date()
date_f<-paste(substr(date,5,7),substr(date,9,10),substr(date,21,24),sep="_")
file_path<-paste("G:/Data/Ratings/Ideology/Informed/IdeoInfModel",date_f,".RData",sep="")
save.image(file=file_path)
rm(date,date_f,file_path)

##########################################
#evaluate betas ####
##########################################
b1.size<-effectiveSize(sample[,1:R])
quantile(b1.size)

b2.size<-effectiveSize(sample[,(R+1):(2*R)])
quantile(b2.size)


pdf(file="G:/Data/Ratings/Ideology/Informed/trace_beta.pdf")
traceplot(sample[,1:R]) #beta1
traceplot(sample[,(R+1):(2*R)]) #beta2
dev.off()

pdf(file="G:/Data/Ratings/Ideology/Informed/dens_beta.pdf")
densplot(sample[,1:R]) #beta1
densplot(sample[,(R+1):(2*R)]) #beta2
dev.off()

b1.auto<-autocorr.diag(sample[,1:R],lags=c(1:4)) #beta1
quantile(b1.auto[1,]) #500
quantile(b1.auto[2,]) #1,000
quantile(b1.auto[3,]) #1,500
quantile(b1.auto[4,]) #2,000

b2.auto<-autocorr.diag(sample[,(R+1):(2*R)],lags=c(1:4)) #beta2
quantile(b2.auto[1,]) #500
quantile(b2.auto[2,]) #1,000
quantile(b2.auto[3,]) #1,500
quantile(b2.auto[4,]) #2,000

b1.gk<-geweke.diag(sample[,1:R]) #beta1
table(ifelse(abs(b1.gk[[1]][[1]])>=1.96,1,0))
table(ifelse(abs(b1.gk[[2]][[1]])>=1.96,1,0))

b2.gk<-geweke.diag(sample[,(R+1):(2*R)]) #beta2
table(ifelse(abs(b2.gk[[1]][[1]])>=1.96,1,0))
table(ifelse(abs(b2.gk[[2]][[1]])>=1.96,1,0))

#b2 at 0.025 > 0
hpd.025<-apply(as.matrix(sample[,(R+1):(2*R)]),2,quantile,probs=0.025)
table(ifelse(hpd.025>=0,1,0))

#b2 at 0.05 > 0
hpd.05<-apply(as.matrix(sample[,(R+1):(2*R)]),2,quantile,probs=0.05)
table(ifelse(hpd.05>=0,1,0))

#b2 at 0.10 > 0
hpd.1<-apply(as.matrix(sample[,(R+1):(2*R)]),2,quantile,probs=0.1)
table(ifelse(hpd.1>=0,1,0))

b1.gl<-gelman.diag(sample[,1:R],autoburnin=F)
quantile(b1.gl[[1]][,1]) #estimate
quantile(b1.gl[[1]][,2]) #upper bound
sort(b1.gl[[1]][,1])
sort(b1.gl[[1]][,2])

b2.gl<-gelman.diag(sample[,(R+1):(2*R)],autoburnin=F)
quantile(b2.gl[[1]][,1]) #estimate
quantile(b2.gl[[1]][,2]) #upper bound
sort(b2.gl[[1]][,1])
sort(b2.gl[[1]][,2])

##########################################
#evaluate xi ####
##########################################

xi.size<-effectiveSize(sample[,(2*R+A+1):(2*R+2*A)])
quantile(xi.size)

pdf(file="G:/Data/Ratings/Ideology/Informed/trace_xi.pdf")
traceplot(sample[,(2*R+A+1):(2*R+2*A)])
dev.off()

pdf(file="G:/Data/Ratings/Ideology/Informed/dens_xi.pdf")
densplot(sample[,(2*R+A+1):(2*R+2*A)])
dev.off()

xi.auto<-autocorr.diag(sample[,(2*R+A+1):(2*R+2*A)],lags=c(1:10))
quantile(xi.auto[1,]) #500
quantile(xi.auto[2,]) #1000
quantile(xi.auto[3,]) #1500
quantile(xi.auto[4,]) #2000
quantile(xi.auto[6,]) #3000
quantile(xi.auto[8,]) #4000
quantile(xi.auto[10,]) #5000

xi.gk<-geweke.diag(sample[,(2*R+A+1):(2*R+2*A)])
table(ifelse(abs(xi.gk[[1]][[1]])>=1.96,1,0))
table(ifelse(abs(xi.gk[[2]][[1]])>=1.96,1,0))
sort(abs(xi.gk[[1]][[1]]))
sort(abs(xi.gk[[2]][[1]]))

#Latent traits that are suggested to have not converged are different across chains
xi.gk[[1]][[1]][which(abs(xi.gk[[1]][[1]])>=1.96)]
xi.gk[[2]][[1]][which(abs(xi.gk[[2]][[1]])>=1.96)]

xi.gk<-geweke.diag(sample[,(2*R+A+1):(2*R+2*A)],frac1=0.2,frac2=0.5)
table(ifelse(abs(xi.gk[[1]][[1]])>=1.96,1,0))
table(ifelse(abs(xi.gk[[2]][[1]])>=1.96,1,0))

xi.gk<-geweke.diag(sample[,(2*R+A+1):(2*R+2*A)],frac1=0.3,frac2=0.5)
table(ifelse(abs(xi.gk[[1]][[1]])>=1.96,1,0))
table(ifelse(abs(xi.gk[[2]][[1]])>=1.96,1,0))

#Check difference in means across chains
compare<-cbind(apply(sample[[1]][,(2*R+A+1):(2*R+2*A)],2,mean),
               apply(sample[[2]][,(2*R+A+1):(2*R+2*A)],2,mean))

compare<-cbind(compare,round(compare[,1]-compare[,2],digits=4))

rm(compare)

#Chains are slow mixing causing geweke diagnostic to suggest no convergence for certain xi, 
#but gelman-rubin is near on for all xi suggesting convergence

xi.gl<-gelman.diag(sample[,(2*R+A+1):(2*R+2*A)],autoburnin = F)
quantile(xi.gl[[1]][,1]) #estimate
quantile(xi.gl[[1]][,2]) #upper bound
sort(xi.gl[[1]][,1])
sort(xi.gl[[1]][,2])

######################################
#scale xi to create local identification ####
######################################

xi<-as.matrix(sample[,(2*R+A+1):(2*R+2*A)])

nrow(xi) #correct dimensions
ncol(xi)

xi.local<-matrix(NA,nrow=nrow(xi),ncol=ncol(xi))

colnames(xi.local)<-colnames(xi)

xi.mean<-mean(xi) #store mean and sd to reduce computing time

xi.sd<-sd(xi)

for(i in 1:nrow(xi)){
  for(j in 1:ncol(xi)){
    xi.local[i,j]<-(xi[i,j]-xi.mean)/xi.sd
    print(j)
  }
}

xi.sum<-matrix(NA,nrow=A,ncol=5)
rownames(xi.sum)<-rownames(s)
xi.sum[,5]<-rownames(s)


for(i in 1:ncol(xi)){
  xi.sum[i,1]<-mean(xi.local[,i])
  xi.sum[i,2]<-sd(xi.local[,i])
  xi.sum[i,3]<-quantile(xi.local[,i],probs=c(0.025))
  xi.sum[i,4]<-quantile(xi.local[,i],probs=c(0.975))
}

#prepare for export ####


colnames(xi.sum)<-c("ideo_rating","ideo_sd","ideo_lb","ideo_ub","agency")
xi.sum<-as.data.frame(xi.sum,stringsAsFactors=F)

xi.sum$ideo_rating<-as.numeric(xi.sum$ideo_rating)
xi.sum$ideo_sd<-as.numeric(xi.sum$ideo_sd)
xi.sum$ideo_lb<-as.numeric(xi.sum$ideo_lb)
xi.sum$ideo_ub<-as.numeric(xi.sum$ideo_ub)

#recode offices to match SFGSII variable
xi.sum$agency[xi.sum$agency=="Federal Reserve"]<-"Board of Governors of the Federal Reserve System"
xi.sum$agency[xi.sum$agency=="Immigration and Customs Enforcement"]<-"United States Immigration and Customs Enforcement"
xi.sum$agency[xi.sum$agency=="Citizenship and Immigration Services"]<-"United States Citizenship and Immigration Services"
xi.sum$agency[xi.sum$agency=="Customs and Border Protection"]<-"United States Customs and Border Protection"

xi.sum$agency<-sub("Offices and Bureaus within ", "", xi.sum$agency, fixed=T)

##########################################
#save file and prepare for merging with baseline ####
##########################################

#add dept
dpt<-read.dta13(file="G:/Data/dept.dta",convert.factors = F)

out<-merge(xi.sum,dpt,all.x=T,by.x="agency",by.y="office")
table(is.na(out$dept)) #only cab departments not matched
rm(dpt)

out$dept1<-grepl("Department of",out$dept,fixed=T)

out$dept[out$dept1==F]<-NA
out$dept1<-NULL

#merge in acronyms
acr<-read.dta13(file="G:/Data/acronyms.dta",convert.factors=F)

out<-merge(out,acr,by="agency",all.x=T)
out$gov_man<-NULL
rm(acr)

out$agency[out$agency=="Economic Growth, Energy and the Environment"]<-"Economic Growth, Energy, and the Environment"

#save estimates
ideo_est<-out
ideo_est$dept[is.na(ideo_est$dept)==T]<-"" #save.dta13 will not accept NA in strings
save.dta13(ideo_est,file="G:/Data/Ratings/Ideology/Informed/informed_agency_ideology_ratings.dta",version=117) 

rm(out,xi,xi.local,xi.sum,ideo_est,foo,foo.inits,forJags,forJags.dump,model,sample,
   b1.size,b2.size,b1.auto,b2.auto,b1.gk,b2.gk,b1.gl,b2.gl,hpd.025,hpd.05,hpd.1,
   xi.size,xi.auto,xi.gk,xi.gl,xi.mean,xi.sd)

############################
#Baseline estimates####
#########################################
#Assemble baseline data for JAGS #####
#########################################

forJags.base<-list(y=y,N=N,a=a,A=A,r=r,R=R)
forJags.base.dump<-dump.format(forJags.base)

#####################################
#create baseline jags model ####
#####################################
model.baseline<-"model{
  for(i in 1:N){ #loop over N ratings
    mu[i]<-beta[r[i],1]+beta[r[i],2]*xi[a[i]] #a indexes agencies, r indexes raters
    y[i] ~ dnorm(mu[i],tau[a[i]]) 
  }
  
  # prior latent agency ideology
  for(j in 1:A){
    xi[j] ~ dnorm(0,1)
  }
  
 
  for(j in 1:A){     
    # Assume all raters are equally precise raters of the same agency (same measurement error variance)
    sigma[j] ~ dunif(0,10) #standard deviation
    tau[j] <- pow(sigma[j],-2) #convert to precision
  }
  
  #priors for the rater parameters for R raters
  for(k in 1:R){
    ## intercepts and slopes
    beta[k,1] ~ dnorm(0,0.01)
    beta[k,2] ~ dnorm(0,0.01)
  }
}"

#####################################
#Execute baseline model run ####
#####################################
#Note: Need to set random number generator seeds when using parallel cores or warning is given
#and seeds are added to inits by runjags
foo.inits<-list(list(xi=rep.int(1,times=165),.RNG.name="base::Mersenne-Twister",.RNG.seed=1),
                list(xi=rep.int(5,times=165),.RNG.name="base::Mersenne-Twister",.RNG.seed=5))

setwd("G:/Data/Ratings/Ideology/Baseline/") #set wd for jagsfiles

foo.base<-run.jags(model=model.baseline,monitor=c("beta","sigma","xi"),
              data=forJags.base.dump,inits=foo.inits,n.chains=2,adapt=10000,burnin=3000000,
              sample=4000,thin=500, method="parallel",keep.jags.files=T)

#save as an MCMC list
base.sample<-as.mcmc.list(foo.base)

##########################################
#Save model ####
##########################################
date<-date()
date_f<-paste(substr(date,5,7),substr(date,9,10),substr(date,21,24),sep="_")
file_path<-paste("G:/Data/Ratings/Ideology/Baseline/IdeoBaseModel",date_f,".RData",sep="")
save.image(file=file_path)
rm(date,date_f,file_path)

##########################################
#check direction of scale ####
##########################################
#Check DOD
mean(base.sample[[1]][,(2*R+A+10)])
mean(base.sample[[2]][,(2*R+A+10)]) #DOD is negative; will need to flip scale

##########################################
#evaluate beta1 ####
##########################################

b1.size<-effectiveSize(base.sample[,1:R])
quantile(b1.size)

pdf(file="G:/Data/Ratings/Ideology/Baseline/trace_beta1.pdf")
traceplot(base.sample[,1:R]) #beta1
dev.off()

pdf(file="G:/Data/Ratings/Ideology/Baseline/dens_beta1.pdf")
densplot(base.sample[,1:R]) #beta1
dev.off()  #note: N=8,000 not 4,000, obs on x axis only show chain 1; density reflects both chains;

b1.auto<-autocorr.diag(base.sample[,1:R],lags=c(1:4)) #beta1
quantile(b1.auto[1,]) #500
quantile(b1.auto[2,]) #1,000
quantile(b1.auto[3,]) #1,500
quantile(b1.auto[4,]) #2,000

b1.gk<-geweke.diag(base.sample[,1:R]) #beta1
table(ifelse(abs(b1.gk[[1]][[1]])>=1.96,1,0))
table(ifelse(abs(b1.gk[[2]][[1]])>=1.96,1,0))

b1.gl<-gelman.diag(base.sample[,1:R],autoburnin=F)
quantile(b1.gl[[1]][,1]) #estimate
quantile(b1.gl[[1]][,2]) #upper bound
sort(b1.gl[[1]][,1])
sort(b1.gl[[1]][,2])

##########################################
#evaluate beta2 ####
##########################################

b2.size<-effectiveSize(base.sample[,(R+1):(2*R)])
quantile(b2.size)

pdf(file="G:/Data/Ratings/Ideology/Baseline/trace_beta2.pdf")
traceplot(base.sample[,(R+1):(2*R)]) #beta2
dev.off()

pdf(file="G:/Data/Ratings/Ideology/Baseline/dens_beta2.pdf")
densplot(base.sample[,(R+1):(2*R)]) #beta2
dev.off()

b2.auto<-autocorr.diag(base.sample[,(R+1):(2*R)],lags=c(1:4)) #beta2
quantile(b2.auto[1,]) #500
quantile(b2.auto[2,]) #1,000
quantile(b2.auto[3,]) #1,500
quantile(b2.auto[4,]) #2,000


b2.gk<-geweke.diag(base.sample[,(R+1):(2*R)]) #beta2
table(ifelse(abs(b2.gk[[1]][[1]])>=1.96,1,0))
table(ifelse(abs(b2.gk[[2]][[1]])>=1.96,1,0))

#b2 at 0.025 > 0
hpd.025<-apply(as.matrix(base.sample[,(R+1):(2*R)]),2,quantile,probs=0.025)
table(ifelse(hpd.025>=0,1,0))

#b2 at 0.05 > 0
hpd.05<-apply(as.matrix(base.sample[,(R+1):(2*R)]),2,quantile,probs=0.05)
table(ifelse(hpd.05>=0,1,0))

#b2 at 0.10 > 0
hpd.1<-apply(as.matrix(base.sample[,(R+1):(2*R)]),2,quantile,probs=0.1)
table(ifelse(hpd.1>=0,1,0))

b2.gl<-gelman.diag(base.sample[,(R+1):(2*R)],autoburnin=F)
quantile(b2.gl[[1]][,1]) #estimate
quantile(b2.gl[[1]][,2]) #upper bound
sort(b2.gl[[1]][,1])
sort(b2.gl[[1]][,2])

##########################################
#evaluate xi ####
##########################################

xi.size<-effectiveSize(base.sample[,(2*R+A+1):(2*R+2*A)])
quantile(xi.size)

pdf(file="G:/Data/Ratings/Ideology/Baseline/trace_xi.pdf")
traceplot(base.sample[,(2*R+A+1):(2*R+2*A)])
dev.off()

pdf(file="G:/Data/Ratings/Ideology/Baseline/dens_xi.pdf")
densplot(base.sample[,(2*R+A+1):(2*R+2*A)])
dev.off()

xi.auto<-autocorr.diag(base.sample[,(2*R+A+1):(2*R+2*A)],lags=c(1:10))
quantile(xi.auto[1,]) #500
quantile(xi.auto[2,]) #1000
quantile(xi.auto[3,]) #1500
quantile(xi.auto[4,]) #2000
quantile(xi.auto[6,]) #3000
quantile(xi.auto[8,]) #4000
quantile(xi.auto[10,]) #5000

xi.gk<-geweke.diag(base.sample[,(2*R+A+1):(2*R+2*A)],frac1=0.2,frac2=0.5)
table(ifelse(abs(xi.gk[[1]][[1]])>=1.96,1,0))
table(ifelse(abs(xi.gk[[2]][[1]])>=1.96,1,0))
sort(abs(xi.gk[[1]][[1]]))
sort(abs(xi.gk[[2]][[1]]))

#Chains are slow mixing, depending on portions of chain select, z value are quite different
#Geweke diagnostic of chain one suggests non-convergence, 
#but gelman diagnositic suggests convergence between chains

#Compare means of each latent trait to see if means are different across chains

compare<-cbind(apply(base.sample[[1]][,(2*R+A+1):(2*R+2*A)],2,mean),
               apply(base.sample[[2]][,(2*R+A+1):(2*R+2*A)],2,mean))

compare<-cbind(compare,round(compare[,1]-compare[,2],digits=4))

rm(compare)
#no substantive difference in means of latent traits across chains

#All estimates near 1, indicates convergence
xi.gl<-gelman.diag(base.sample[,(2*R+A+1):(2*R+2*A)],autoburnin = F)
quantile(xi.gl[[1]][,1]) #estimate
quantile(xi.gl[[1]][,2]) #upper bound
sort(xi.gl[[1]][,1])
sort(xi.gl[[1]][,2])

##########################################
#flip scale ####
##########################################

mean(base.sample[[1]][,(2*R+A+10)]) #confirm change
mean(base.sample[[2]][,(2*R+A+10)])

######################################
#scale xi to create local identification ####
######################################

xi<-as.matrix(base.sample[,(2*R+A+1):(2*R+2*A)])

nrow(xi) #correct dimensions
ncol(xi)

#flip the scale
mean(base.sample[[1]][,(2*R+A+10)])
mean(base.sample[[2]][,(2*R+A+10)])

xi<-xi*-1
mean(xi[1:4000,10])
mean(xi[4001:8000,10]) #confirm change

xi.local<-matrix(NA,nrow=nrow(xi),ncol=ncol(xi))

colnames(xi.local)<-colnames(xi)

xi.mean<-mean(xi) #store mean and sd to reduce computing time

xi.sd<-sd(xi)

for(i in 1:nrow(xi)){
  for(j in 1:ncol(xi)){
    xi.local[i,j]<-(xi[i,j]-xi.mean)/xi.sd
    print(j)
  }
}

xi.sum<-matrix(NA,nrow=A,ncol=5)
rownames(xi.sum)<-rownames(s)
xi.sum[,5]<-rownames(s)


for(i in 1:ncol(xi)){
  xi.sum[i,1]<-mean(xi.local[,i])
  xi.sum[i,2]<-sd(xi.local[,i])
  xi.sum[i,3]<-quantile(xi.local[,i],probs=c(0.025))
  xi.sum[i,4]<-quantile(xi.local[,i],probs=c(0.975))
}

#prepare for export to stata ####
colnames(xi.sum)<-c("base_ideo_rating","base_ideo_sd","base_ideo_lb","base_ideo_ub","agency")
xi.sum<-as.data.frame(xi.sum,stringsAsFactors=F)

xi.sum$base_ideo_rating<-as.numeric(xi.sum$base_ideo_rating)
xi.sum$base_ideo_sd<-as.numeric(xi.sum$base_ideo_sd)
xi.sum$base_ideo_lb<-as.numeric(xi.sum$base_ideo_lb)
xi.sum$base_ideo_ub<-as.numeric(xi.sum$base_ideo_ub)

#recode offices to match SFGSII variable
xi.sum$agency[xi.sum$agency=="Federal Reserve"]<-"Board of Governors of the Federal Reserve System"
xi.sum$agency[xi.sum$agency=="Immigration and Customs Enforcement"]<-"United States Immigration and Customs Enforcement"
xi.sum$agency[xi.sum$agency=="Citizenship and Immigration Services"]<-"United States Citizenship and Immigration Services"
xi.sum$agency[xi.sum$agency=="Customs and Border Protection"]<-"United States Customs and Border Protection"
xi.sum$agency[xi.sum$agency=="Economic Growth, Energy and the Environment"]<-"Economic Growth, Energy, and the Environment"

xi.sum$agency<-sub("Offices and Bureaus within ", "", xi.sum$agency, fixed=T)
xi.sum$agency[xi.sum$agency=="Economic Growth, Energy and the Environment"]<-"Economic Growth, Energy, and the Environment"

##########################################
#merge with informed estimates and export ####
##########################################
ideo_est_base<-xi.sum

#save estimates
save.dta13(ideo_est_base,file="G:/Data/Ratings/Ideology/Baseline/baseline_agency_ideology_ratings.dta",version=117) #save as a stata 13 file

ideo_est<-read.dta13(file="G:/Data/Ratings/Ideology/Informed/informed_agency_ideology_ratings.dta",
                     convert.factors = F)

ideo_exp<-merge(ideo_est,ideo_est_base,by="agency")

#Finalize names
ideo_exp$agency[ideo_exp$agency=="Air Force"]<-"Department of the Air Force"
ideo_exp$agency[ideo_exp$agency=="Army"]<-"Department of the Army"
ideo_exp$agency[ideo_exp$agency=="Navy"]<-"Department of the Navy"
ideo_exp$agency[ideo_exp$agency=="Board of Governors of the Federal Reserve System" ]<-"The Federal Reserve"

ideo_exp<-ideo_exp[order(ideo_exp$agency),]

save.dta13(ideo_exp,file="C:/Users/mdr/Dropbox/IdeologyPaper/WebData/rcl_ideology_estimates.dta",version=117) #save as a stata 13 file
write.csv(ideo_exp,file="C:/Users/mdr/Dropbox/IdeologyPaper/WebData/rcl_ideology_estimates.csv",row.names=F)

