rm(list=ls(all=TRUE))

data 		<- read.delim("GCspending.txt",header=TRUE)	# Read into memory a tab delimited file and assign it to a data frame ds
attach(data)

### Outcome is the difference in the proportion of spending dedicated to the municipality ###

	Y 	<- GC.spending.62/sum(GC.spending.62) - GC.spending.58/sum(GC.spending.58)

### Scale to percentage points 
	
	Y	<- Y*100 



### Explanatory variable of interest is deviation of GC's actual votes from his prior belief (in hundreds of votes) ###
	D <- (GC.votes.58 - GC.expvotes.58)/100


### core controls ###

	X.red <-data.frame(GC.votes.58/100,log(regvoters.60))


### limited set of controls ###
	X.lim  <- data.frame(GC.votes.58/100, PSD.mayor.58,PSD.voteshare.58,log(regvoters.60),pliterate.60)



data.red<-data.frame(Y,D,X.red)
data.red<-data.red[municipality!="Belo Horizonte"&GC.spending.58>0,]
data.red<-na.omit(data.red)
ols.red <- lm(Y~.,data=data.red)
summary(ols.red)
N.red<-nrow(data.red)


data.lim<-data.frame(Y,D,X.lim)
data.lim<-data.lim[municipality!="Belo Horizonte"&GC.spending.58>0,]
data.lim<-na.omit(data.lim)
ols.lim<- lm(Y~.,data=data.lim)
summary(ols.lim)
N.lim<-nrow(data.lim)




