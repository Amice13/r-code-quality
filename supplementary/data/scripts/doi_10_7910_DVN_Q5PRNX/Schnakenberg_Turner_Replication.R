#uncomment to install packages

#install.packages("foreign")
#install.packages("Hmisc")
library(foreign)
library(Hmisc)

#uncomment this to set your working directory, replacing "~/mypath/" with your working directory
#mywd <- "~/mypath/"
#setwd(mywd)

data <- readRDS("Schnakenberg_Turner_Replication_Data.rds")
data <- data[!is.na(data$donor_id),]


opp <- as.integer(data$peragsen < data$per2agchal)
data$opp <- opp

donor <- as.integer(data$total_donation > 0)
data$donor <- donor

data$categories <- NA
data$categories[data$investor2==0 & data$matchcommf==0] <- 1
data$categories[data$investor2==1 & data$matchcommf==0] <- 2
data$categories[data$investor2==0 & data$matchcommf==1] <- 3
data$categories[data$investor2==1 & data$matchcommf==1] <- 4

dataopp <- data[data$opp==1,]
dataally <- data[data$opp==0,]
oppprop <- aggregate(dataopp$donor, by=list(dataopp$categories), FUN=mean, na.rm=T)
allyprop <- aggregate(dataally$donor, by=list(dataally$categories), FUN=mean, na.rm=T)
oppallyrat <- oppprop$x/allyprop$x

set.seed(1234) 
opppropmat <- matrix(NA, nrow=500, ncol=4)
allypropmat <- matrix(NA, nrow=500, ncol=4)
oppallyratmat <- matrix(NA, nrow=500, ncol=4)
for(k in 1:500){
  bootsamp <- sample(x=unique(data$donor_id), size=length(unique(data$donor_id)), replace=TRUE)
  bootrows <- numeric(0)
  for(i in bootsamp){ bootrows <- append(bootrows, which(data$donor_id==i))}
  bootdata <- data[bootrows,]
  bootdataopp <- bootdata[bootdata$opp==1,]
  bootdataally <- bootdata[bootdata$opp==0,]
  opppropmat[k,] <- aggregate(bootdataopp$donor, by=list(bootdataopp$categories), FUN=mean, na.rm=T)$x
  allypropmat[k,] <- aggregate(bootdataally$donor, by=list(bootdataally$categories), FUN=mean, na.rm=T)$x
  oppallyratmat[k,] <- opppropmat[k,]/allypropmat[k,]
}



####################################################################################################################
# FIGURE 5 (TOP)
####################################################################################################################

plot(1:4, oppprop$x, axes=F, pch=19, xlab="", ylab="Pr[donate]", ylim=c(0, .12))
points(1:4, allyprop$x, pch=23)
segments(x0=1:4, x1=1:4, y0=apply(opppropmat, 2, quantile, probs=.05), y1=apply(opppropmat, 2, quantile, probs=.95))
segments(x0=1:4, x1=1:4, y0=apply(allypropmat, 2, quantile, probs=.05), y1=apply(allypropmat, 2, quantile, probs=.95))
grid()
axis(1, at=1:4, labels=c("Non-investor \n + non-match", "Investor \n + non-match", "Non-investor \n + match", "Investor \n + match"), padj=1)
axis(2, at=c(.00, .02, .04, .06, .08, .1, .12))
legend(1, .1, legend=c("Allies", "Opponents"), pch=c(23, 19))


####################################################################################################################
# FIGURE 5 (BOTTOM)
####################################################################################################################

par(mar=c(8,11,1,1), las=1)
plot(1:4, oppallyrat,  axes=F, pch=19, xlab="", ylab=" ", ylim=c(0, .6))
grid()
axis(1, at=1:4, labels=c("Non-investor \n + non-match", "Investor \n + non-match", "Non-investor \n + match", "Investor \n + match"), padj=1)
axis(2, at=c(.00, .1, .2, .3, .4, .5, .6))
segments(x0=1:4, x1=1:4, y0=apply(oppallyratmat, 2, quantile, probs=.05), y1=apply(oppallyratmat, 2, quantile, probs=.95))
mtext(expression(frac("Pr[donate | opponent]", "Pr[donate | ally]")), side=2, adj=1.3)

#support for the significance claim in first paragraph of section C.2
quantile(oppallyratmat[,4] - oppallyratmat[,1], probs=c(.05, .95))


#create donor-level dataset
percentopp <- numeric(length(unique(data$donor_id)))
isinvestor <- numeric(length(unique(data$donor_id)))
j <- 1
for(i in unique(data$donor_id)){
  percentopp[j] <- mean(data$opp[data$donor_id==i], na.rm=T)
  isinvestor[j] <- mean(data$investor2[data$donor_id==i], na.rm=T)
  j <- j+1
}
donordata <- data.frame(unique(data$donor_id), percentopp, isinvestor)
donordata <- na.omit(donordata)


####################################################################################################################
# FIGURE 7 
####################################################################################################################
boxplot(percentopp ~ isinvestor, data=donordata, notch=TRUE, col="grey", axes=F, ylab="Percent of donations to opponents")
grid()
axis(1, at=c(1, 2), labels=c("Non-Investor", "Investor"), lty=0)
axis(2, at=c(0, .2, .4, .6, .8, 1))

####################################################################################################################
# FIGURE 6
####################################################################################################################
hist(percentopp, col="grey", xlab="Percent of donations to opponents", main="")

#claim in first paragraph of C.3 that "around 23 percent of respondents donated to opponents more than 80 percent of the time they donated to campaigns"
mean(as.integer(percentopp > .8), na.rm=T)

