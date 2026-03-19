##################################################################################################
#Ms: Selective Attention: The United Nations Security Council and Armed Conflict
#Authors: Magnus Lundgren & Mark Klamberg
#Date: August 2022
#Software: Stata 17
#Hardware: Mac
#Data available via Harvard Dataverse
##################################################################################################

# NOTE: Main models are fitted in Stata. See separate do-file. 


# Read in data 

data<-read.csv("~/Dropbox/lundgren_klamberg_bjps.csv")

# Figure 1

ggplot(data, mapping=aes(x=year , y= reorder(location, start_year),fill=agenda)) + geom_tile(color="black") +  scale_fill_gradient(name = "", low = "#FFFFFF", high = "#012345") + xlab(label = "") + ylab(label = "") +theme_bw() +facet_grid(region~.,space="free",scales="free") +guides(fill=guide_legend(title="Number of meetings"))


# Figure 2

par(mfrow=c(1,1))
temp<-aggregate(agenda_binary~year,data,mean)
temp<-temp[2:31,]
temp[2:31,3]<-aggregate(resolution_binary~year,data,mean)[2:31,2]
plot(temp[,1],temp[,2], type="l", ylim=c(0,.6),xlab="Year",ylab="Proportion of conflict countries on UNSC agenda",lwd=2)


# Figure 5

temp<-data[data$location=="Yemen",]
test<-data.frame(year=1989:2019)
t<-merge(test,temp,by="year",all.x=T)
t$bd_best[is.na(t$bd_best)]<-0
t$agenda[is.na(t$agenda)]<-0
par(mar=c(5.1,4.1,4.1,6.1))
x<-barplot(t$agenda)
barplot(t$bd_best/1000,axes=F,ylim=c(0,15),names=t$year,cex.names = .8, main="Yemen")
lines(x[,1],t$agenda,type="l",lwd=2)
axis(4,at=c(0,2,4,6,8,10,12,14), cex.axis=.8) 
axis(2,at=c(0,2,4,6,8), cex.axis=.8) 
mtext("Annual battle deaths (1,000s)", side=2, cex=.8,line=2.5,at=4)
mtext("Annual UNSC meetings", side=4, cex=.8,line=2.5,at=7)
legend("topleft",c("Battle deaths             ", "UNSC meetings           "), fill=c("darkgrey","black"),cex=.7,bty="n")


