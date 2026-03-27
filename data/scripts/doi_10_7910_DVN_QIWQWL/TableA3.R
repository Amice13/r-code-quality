rm(list=ls(all=TRUE))

data 		<- read.delim("GCspending.txt",header=TRUE)	# Read into memory a tab delimited file and assign it to a data frame ds
attach(data)

data.lim<-data[municipality!="Belo Horizonte",]
detach(data)
attach(data.lim)


### Outcome 1 is the difference in the proportion of spending dedicated to the municipality ###

	Y1 	<- GC.spending.62/sum(GC.spending.62) - GC.spending.58/sum(GC.spending.58)

### Scale to percentage points 
	
	Y1	<- Y1*100 
	
### Outcome 2 is the inflation-adjusted difference in spending (see note) ###
	Y2 <- (GC.spending.62/3.699763) - GC.spending.58

### Scale to ten thousands of cruzieros ###
	Y2 <- Y2/10000
	
### Explanatory variable of interest is deviation of GC's actual votes from his prior belief (in hundreds of votes) ###
	D <- (GC.votes.58 - GC.expvotes.58)/100
	

	
### full set of controls ###
	X.full <- data.frame(GC.votes.58/100, GC.votes.54/100, PSD.mayor.58,PSD.voteshare.58,PSD.mayor.54,PSD.voteshare.54,log(regvoters.60),pliterate.60,log(population.60),log(area),prunningwater.60,pelectricity.60,pradio.60,p.fridge.60,landgini.60,log(avgsizeland.60),pfarmworkers.60,pindustrial.60)
	
## All variables together ##
	
	study.dat<-data.frame(Y1,Y2,D,X.full)
	
N.obs 		<- function(x){length(x)-sum(is.na(x))}
mean.obs	<- function(x){mean(x,na.rm=T)}
sd.obs		<- function(x){sd(x,na.rm=T)}
min.obs		<- function(x){min(x,na.rm=T)}
max.obs		<- function(x){max(x,na.rm=T)}



N 		<- 	apply(study.dat,2,N.obs)
Avg 	<- 	apply(study.dat,2,mean.obs)
StDev	<-	apply(study.dat,2,sd.obs)
minimum	<-	apply(study.dat,2,min.obs)
maximum	<-	apply(study.dat,2,max.obs)

dstats<-round(cbind(N,Avg,StDev,minimum,maximum),3)
dstats 	