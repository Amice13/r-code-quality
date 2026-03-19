rm(list=ls(all=TRUE))


data 		<- read.delim("GCspending.txt",header=TRUE)	# Read into memory a tab delimited file and assign it to a data frame ds
attach(data)

### Outcome is difference in Capanema's votes across elections (in hundreds of votes) ###
	Y <- (GC.votes.58 - GC.votes.54)/100

### Treatment is the inflation-adjusted difference in spending (see note) ###
	T <- (GC.spending.62/3.699763) - GC.spending.58
### Scaled to ten thousand cruzeiros ###
	T <- T/10000

### core controls ###

	X.red <-data.frame(GC.votes.54/100,PSD.mayor.54,log(regvoters.60))

### limited set of controls ###
	X.lim  <- data.frame(X.red,GC.total.share.54,PSD.voteshare.54,pliterate.60)

### full set of controls ###
X.full <- data.frame(X.lim,log(population.60),log(area),prunningwater.60,pelectricity.60,pradio.60,p.fridge.60,landgini.60,log(avgsizeland.60),pfarmworkers.60,pindustrial.60)

data.nc<-data.frame(Y,T)
data.nc<-data.nc[municipality!="Belo Horizonte",]
data.nc<-na.omit(data.nc)
ols.nc <- lm(Y~T,data=data.nc)
summary(ols.nc)
N.nc<-nrow(data.nc)



data.red<-data.frame(Y,T,X.red)
data.red<-data.red[municipality!="Belo Horizonte",]
data.red<-na.omit(data.red)
ols.red <- lm(Y~.,data=data.red)
summary(ols.red)
N.red<-nrow(data.red)


data.lim<-data.frame(Y,T,X.lim)
data.lim<-data.lim[municipality!="Belo Horizonte",]
data.lim<-na.omit(data.lim)
ols.lim<- lm(Y~.,data=data.lim)
summary(ols.lim)
N.lim<-nrow(data.lim)

data.full<-data.frame(Y,T,X.full)
data.full<-data.full[municipality!="Belo Horizonte",]
data.full<-na.omit(data.full)
ols.full<- lm(Y~.,data=data.full)
summary(ols.full)
N.full<-nrow(data.full)