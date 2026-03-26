rm(list=ls(all=TRUE))


data 		<- read.delim("GCspending.txt",header=TRUE)	# Read into memory a tab delimited file and assign it to a data frame ds
attach(data)

### Outcome is the difference in the proportion of total spending ###
	Y <- GC.spending.62/sum(GC.spending.62) - GC.spending.58/sum(GC.spending.58)

### Scale to percentage points 
	
	Y	<- Y*100

### Explanatory variable of interest is deviation of GC's actual votes from his prior belief (in hundreds of votes) ###
	D <- (GC.votes.58 - GC.expvotes.58)/100

	D.votes54 <- D*GC.votes.54/100

### core controls ###

	X.red <-data.frame(GC.votes.58/100,log(regvoters.60))


### limited set of controls ###
	X.lim  <- data.frame(GC.votes.58/100, PSD.mayor.58,PSD.voteshare.58,log(regvoters.60),pliterate.60)

### full set of controls ###
	X.full <- data.frame(GC.votes.58/100, PSD.mayor.58,PSD.voteshare.58,PSD.mayor.54,PSD.voteshare.54,log(regvoters.60),pliterate.60,log(population.60),log(area),prunningwater.60,pelectricity.60,pradio.60,p.fridge.60,landgini.60,log(avgsizeland.60),pfarmworkers.60,pindustrial.60)

data.red<-data.frame(Y,D,GC.votes.54/100,D.votes54,X.red)
data.red<-data.red[municipality!="Belo Horizonte"&broker.contact.58==1,]
data.red<-na.omit(data.red)
ols.red <- lm(Y~.,data=data.red)
summary(ols.red)
N.red<-nrow(data.red)

data.lim<-data.frame(Y,D,GC.votes.54/100,D.votes54,X.lim)
data.lim<-data.lim[municipality!="Belo Horizonte"&broker.contact.58==1,]
data.lim<-na.omit(data.lim)
ols.lim<- lm(Y~.,data=data.lim)
summary(ols.lim)
N.lim<-nrow(data.lim)

data.full<-data.frame(Y,D,GC.votes.54/100,D.votes54,X.full)
data.full<-data.full[municipality!="Belo Horizonte"&broker.contact.58==1,]
data.full<-na.omit(data.full)
ols.full<- lm(Y~.,data=data.full)
summary(ols.full)
N.full<-nrow(data.full)


