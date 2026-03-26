rm(list=ls(all=TRUE))

data 		<- read.delim("GCspending.txt",header=TRUE)	# Read into memory a tab delimited file and assign it to a data frame ds
attach(data)

### Outcome is the difference in the proportion of spending dedicated to the municipality ###

	Y 	<- GC.spending.62/sum(GC.spending.62) - GC.spending.58/sum(GC.spending.58)

### Scale to percentage points 
	
	Y	<- Y*100 



### Explanatory variable of interest is deviation of GC's actual votes from his prior belief (in hundreds of votes) ###
	D <- (GC.votes.58 - GC.expvotes.58)/100

### limited set of controls ###

	X.lim1  <- data.frame(GC.votes.58/100)
	X.lim2  <- data.frame((GC.votes.58-GC.votes.54)/100)
	X.lim3  <- data.frame(GC.total.share.58)
	X.lim4  <- data.frame(GC.total.share.58-GC.total.share.54)
	X.lim5  <- data.frame(GC.PSD.share.58)
	X.lim6  <- data.frame(GC.PSD.share.58-GC.PSD.share.54)
	X.lim7  <- data.frame(X.lim1,X.lim2,X.lim3,X.lim4,X.lim5,X.lim6)

data.lim1<-na.omit(data.frame(Y,D,X.lim1))
data.lim1<-data.lim1[municipality!="Belo Horizonte",]
ols.lim1<- lm(Y~.,data=data.lim1)
summary(ols.lim1)
N.lim1<-nrow(data.lim1)

data.lim2<-na.omit(data.frame(Y,D,X.lim2))
data.lim2<-data.lim2[municipality!="Belo Horizonte",]
ols.lim2<- lm(Y~.,data=data.lim2)
summary(ols.lim2)
N.lim2<-nrow(data.lim2)

data.lim3<-na.omit(data.frame(Y,D,X.lim3))
data.lim3<-data.lim3[municipality!="Belo Horizonte",]
ols.lim3<- lm(Y~.,data=data.lim3)
summary(ols.lim3)
N.lim3<-nrow(data.lim3)

data.lim4<-na.omit(data.frame(Y,D,X.lim4))
data.lim4<-data.lim4[municipality!="Belo Horizonte",]
ols.lim4<- lm(Y~.,data=data.lim4)
summary(ols.lim4)
N.lim4<-nrow(data.lim4)

data.lim5<-na.omit(data.frame(Y,D,X.lim5))
data.lim5<-data.lim5[municipality!="Belo Horizonte",]
ols.lim5<- lm(Y~.,data=data.lim5)
summary(ols.lim5)
N.lim5<-nrow(data.lim5)

data.lim6<-na.omit(data.frame(Y,D,X.lim6))
data.lim6<-data.lim6[municipality!="Belo Horizonte",]
ols.lim6<- lm(Y~.,data=data.lim6)
summary(ols.lim6)
N.lim6<-nrow(data.lim6)


data.lim7<-na.omit(data.frame(Y,D,X.lim7))
data.lim7<-data.lim7[municipality!="Belo Horizonte",]
ols.lim7<- lm(Y~.,data=data.lim7)
summary(ols.lim7)
N.lim7<-nrow(data.lim7)



