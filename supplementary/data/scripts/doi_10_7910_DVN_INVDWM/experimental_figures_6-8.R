
#########################################
### PRODUCING FIGURES FROM EXPERIMENT ###
#########################################

rm(list=ls())
setwd("/Users/samtrachtman/Dropbox/ACA Study/Projects/Public Private Field Experiment/APSR Production/replication materials/submission_data/experiment/")

library(foreign)
library(lmtest)
library(sandwich)
library(plyr)
library(readstata13)

joint.data = read.dta13("dig_ph_append.dta")

#functions for balance plots 
check = function(x, tr){
  r=rep(NA,3)
  r[1] = mean(x[tr==1], na.rm=TRUE)
  r[2] = mean(x[tr==0], na.rm=TRUE)
  r[3]=t.test(x[tr==1], x[tr==0], na.rm=T)$p.value
  return(r)
}

plot.pval <- function(results, title=NULL, legend,legendx=0.15,legendy=2.2, textsize=0.9, parcex=0.8, at1=-0.35, at2=-0.15, at3=-0.9,xlim1=-0.85) {
  
  # set values of different parameters
  xlim = c(xlim1,1); pchset = c(21,24,22,23); pchcolset = c("blue","yellow","red","darkgreen")
  
  # set margins and letter size
  par(cex=parcex, mai = c(0.5, 0.35, 1.1, 0.35))
  
  # set number of rows 
  ny = nrow(results)
  
  # create the empty figure
  if(!is.null(title))  plot(x=NULL,axes=F, xlim=xlim, ylim=c(1,ny),xlab="",ylab="", main=title)
  if(is.null(title))   plot(x=NULL,axes=F, xlim=xlim, ylim=c(1,ny),xlab="",ylab="")
  
  # add the 0, 0.05 and 0.1 vertical lines
  abline(v=c(0,0.05,0.1),lty=c(1,4,4), lwd=c(1,2,2))
  axis(side=1,at=c(0,0.05,0.1,1),tick=TRUE, las=2, cex.axis=0.7)
  
  # add labels on top of the three areas of the graph
  axis(side=3,at=at1,labels="Mean\n.gov",tick=FALSE, padj=0.5,cex.axis=textsize)
  axis(side=3,at=at2,labels="Mean\nsherpa",tick=FALSE, padj=0.5,cex.axis=textsize)
  axis(side=3,at=0.5,labels="P-values",tick=FALSE, padj=0.5,cex.axis=textsize)
  
  # Fill the figure with the information which is inside the 'results' matrix
  # First, add the p-values as points
  for(i in 4:ncol(results)) points(results[,i],ny:1, pch = pchset[i-4+1], col = pchcolset[i-4+1], bg = pchcolset[i-4+1])
  
  # Second, add each variable name and the means for treated and control
  for(i in 1:ny) {
    text(at3,ny-i+1,results[i,1],adj = 0,cex=textsize) # variable name
    text(at1,ny-i+1,results[i,2], cex=textsize)        # treatment mean
    text(at2,ny-i+1,results[i,3], cex=textsize)        # control mean
  }
  
  # Add dotted horizontal lines every two variables to make it prettier
  for(i in seq(2,by=2,length.out=floor((ny-1)/2))) abline(h = i+0.5, lty = 3)
  
  # Add legend
  #if(legend) legend(x=legendx, y=legendy, c("t-test"), pch=pchset, pt.bg = pchcolset, cex=0.8)
}

#setwd("/Users/samtrachtman/Dropbox/ACA Study/Projects/Public Private Field Experiment/APSR Production/submission_results/experiment/")


################
###FIGURE 4 ###
################

all.est0 = t.test(joint.data$uptake[joint.data$tr_gov == 0])$estimate
all.clow0 = t.test(joint.data$uptake[joint.data$tr_gov == 0])$conf[[1]]
all.chigh0 = t.test(joint.data$uptake[joint.data$tr_gov == 0])$conf[[2]]

all.est1 = t.test(joint.data$uptake[joint.data$tr_gov == 1])$estimate
all.clow1 = t.test(joint.data$uptake[joint.data$tr_gov == 1])$conf[[1]]
all.chigh1 = t.test(joint.data$uptake[joint.data$tr_gov == 1])$conf[[2]]

dem.est0 = t.test(joint.data$uptake[joint.data$party3 == 1 & joint.data$tr_gov == 0])$estimate
dem.clow0 = t.test(joint.data$uptake[joint.data$party3 == 1 & joint.data$tr_gov == 0])$conf[[1]]
dem.chigh0 = t.test(joint.data$uptake[joint.data$party3 == 1 & joint.data$tr_gov == 0])$conf[[2]]

dem.est1 = t.test(joint.data$uptake[joint.data$party3 == 1 & joint.data$tr_gov == 1])$estimate
dem.clow1 = t.test(joint.data$uptake[joint.data$party3 == 1 & joint.data$tr_gov == 1])$conf[[1]]
dem.chigh1 = t.test(joint.data$uptake[joint.data$party3 == 1 & joint.data$tr_gov == 1])$conf[[2]]

ind.est0 = t.test(joint.data$uptake[joint.data$party3 == 2 & joint.data$tr_gov == 0])$estimate
ind.clow0 = t.test(joint.data$uptake[joint.data$party3 == 2 & joint.data$tr_gov == 0])$conf[[1]]
ind.chigh0 = t.test(joint.data$uptake[joint.data$party3 == 2 & joint.data$tr_gov == 0])$conf[[2]]

ind.est1 = t.test(joint.data$uptake[joint.data$party3 == 2 & joint.data$tr_gov == 1])$estimate
ind.clow1 = t.test(joint.data$uptake[joint.data$party3 == 2 & joint.data$tr_gov == 1])$conf[[1]]
ind.chigh1 = t.test(joint.data$uptake[joint.data$party3 == 2 & joint.data$tr_gov == 1])$conf[[2]]

t.test(joint.data$uptake[joint.data$party3 == 3 & joint.data$tr_gov == 0], 
       joint.data$uptake[joint.data$party3 == 1 & joint.data$tr_gov == 1])

rep.est0 = t.test(joint.data$uptake[joint.data$party3 == 3 & joint.data$tr_gov == 0])$estimate
rep.clow0 = t.test(joint.data$uptake[joint.data$party3 == 3 & joint.data$tr_gov == 0])$conf[[1]]
rep.chigh0 = t.test(joint.data$uptake[joint.data$party3 == 3 & joint.data$tr_gov == 0])$conf[[2]]

rep.est1 = t.test(joint.data$uptake[joint.data$party3 == 3 & joint.data$tr_gov == 1])$estimate
rep.clow1 = t.test(joint.data$uptake[joint.data$party3 == 3 & joint.data$tr_gov == 1])$conf[[1]]
rep.chigh1 = t.test(joint.data$uptake[joint.data$party3 == 3 & joint.data$tr_gov == 1])$conf[[2]]

a = sum(!is.na(joint.data$party3 == 1))
b = sum(joint.data$party3 == 1, na.rm=T)
c = sum(joint.data$party3 == 2, na.rm=T)
d = sum(joint.data$party3 == 3, na.rm=T)

#plot
x = c(.9, 1.1, 1.9, 2.1, 2.9, 3.1, 3.9, 4.1)
value = c(all.est0, all.est1, dem.est0, dem.est1, ind.est0, ind.est1, rep.est0, rep.est1)
lower.conf = c(all.clow0, all.clow1, dem.clow0, dem.clow1, ind.clow0, ind.clow1, rep.clow0, rep.clow1)
upper.conf = c(all.chigh0, all.chigh1, dem.chigh0, dem.chigh1, ind.chigh0, ind.chigh1, rep.chigh0, rep.chigh1)
treat = c("HealthSherpa.com", "Healthcare.gov", "HealthSherpa.com", "Healthcare.gov","HealthSherpa.com", "Healthcare.gov","HealthSherpa.com", "Healthcare.gov")
plotdata = data.frame(value, lower.conf, upper.conf, x, treat)

library(ggplot2)
tiff(filename = "figure4.tif")
ggplot(plotdata, aes(x = x, y = value))+
  geom_point(size=3, aes(shape=as.factor(treat))) +
  geom_linerange(aes(ymin=lower.conf, ymax=upper.conf)) + 
  scale_x_continuous(breaks = 1:4, labels = c("All Respondents\nn = 1837", "Democrats\nn = 678", "Independents\nn = 394", "Republicans\nn = 178"))+
  scale_y_continuous(labels = scales::percent)+
  labs(x = "", y = "Percent Enrolling") + 
  theme_bw()+
  theme(legend.position=c(.14,.91), legend.title =  element_blank()) 
dev.off()



################
### FIGURE 5 ###
################

all.est0 = t.test(joint.data$uptake[joint.data$tr_gov == 0])$estimate
all.clow0 = t.test(joint.data$uptake[joint.data$tr_gov == 0])$conf[[1]]
all.chigh0 = t.test(joint.data$uptake[joint.data$tr_gov == 0])$conf[[2]]

all.est1 = t.test(joint.data$uptake[joint.data$tr_gov == 1])$estimate
all.clow1 = t.test(joint.data$uptake[joint.data$tr_gov == 1])$conf[[1]]
all.chigh1 = t.test(joint.data$uptake[joint.data$tr_gov == 1])$conf[[2]]

dem.est0 = t.test(joint.data$uptake[joint.data$ideology3 == 1 & joint.data$tr_gov == 0])$estimate
dem.clow0 = t.test(joint.data$uptake[joint.data$ideology3 == 1 & joint.data$tr_gov == 0])$conf[[1]]
dem.chigh0 = t.test(joint.data$uptake[joint.data$ideology3 == 1 & joint.data$tr_gov == 0])$conf[[2]]

dem.est1 = t.test(joint.data$uptake[joint.data$ideology3 == 1 & joint.data$tr_gov == 1])$estimate
dem.clow1 = t.test(joint.data$uptake[joint.data$ideology3 == 1 & joint.data$tr_gov == 1])$conf[[1]]
dem.chigh1 = t.test(joint.data$uptake[joint.data$ideology3 == 1 & joint.data$tr_gov == 1])$conf[[2]]

ind.est0 = t.test(joint.data$uptake[joint.data$ideology3 == 2 & joint.data$tr_gov == 0])$estimate
ind.clow0 = t.test(joint.data$uptake[joint.data$ideology3 == 2 & joint.data$tr_gov == 0])$conf[[1]]
ind.chigh0 = t.test(joint.data$uptake[joint.data$ideology3 == 2 & joint.data$tr_gov == 0])$conf[[2]]

ind.est1 = t.test(joint.data$uptake[joint.data$ideology3 == 2 & joint.data$tr_gov == 1])$estimate
ind.clow1 = t.test(joint.data$uptake[joint.data$ideology3 == 2 & joint.data$tr_gov == 1])$conf[[1]]
ind.chigh1 = t.test(joint.data$uptake[joint.data$ideology3 == 2 & joint.data$tr_gov == 1])$conf[[2]]

rep.est0 = t.test(joint.data$uptake[joint.data$ideology3 == 3 & joint.data$tr_gov == 0])$estimate
rep.clow0 = t.test(joint.data$uptake[joint.data$ideology3 == 3 & joint.data$tr_gov == 0])$conf[[1]]
rep.chigh0 = t.test(joint.data$uptake[joint.data$ideology3 == 3 & joint.data$tr_gov == 0])$conf[[2]]

rep.est1 = t.test(joint.data$uptake[joint.data$ideology3 == 3 & joint.data$tr_gov == 1])$estimate
rep.clow1 = t.test(joint.data$uptake[joint.data$ideology3 == 3 & joint.data$tr_gov == 1])$conf[[1]]
rep.chigh1 = t.test(joint.data$uptake[joint.data$ideology3 == 3 & joint.data$tr_gov == 1])$conf[[2]]

a = sum(!is.na(joint.data$ideology3 == 1))
b = sum(joint.data$ideology3 == 1, na.rm=T)
c = sum(joint.data$ideology3 == 2, na.rm=T)
d = sum(joint.data$ideology3 == 3, na.rm=T)

#plot
x = c(.9, 1.1, 1.9, 2.1, 2.9, 3.1)
value = c(dem.est0, dem.est1, ind.est0, ind.est1, rep.est0, rep.est1)
lower.conf = c(dem.clow0, dem.clow1, ind.clow0, ind.clow1, rep.clow0, rep.clow1)
upper.conf = c(dem.chigh0, dem.chigh1, ind.chigh0, ind.chigh1, rep.chigh0, rep.chigh1)
treat = c("HealthSherpa.com", "Healthcare.gov","HealthSherpa.com", "Healthcare.gov","HealthSherpa.com", "Healthcare.gov")
plotdata = data.frame(value, lower.conf, upper.conf, x, treat)

library(ggplot2)
tiff(filename = "figure5.tif")
ggplot(plotdata, aes(x = x, y = value))+
  geom_point(size=3, aes(shape=as.factor(treat))) +
  geom_linerange(aes(ymin=lower.conf, ymax=upper.conf)) + 
  scale_x_continuous(breaks = 1:3, labels = c("Liberals\nn = 126", "Moderates\nn = 185", "Conservatives\nn = 120"))+
  scale_y_continuous(labels = scales::percent)+
  labs(x = "", y = "Percent Enrolling") + 
  theme_bw()+
  theme(legend.position=c(.14,.91), legend.title =  element_blank()) 
dev.off()



################
### FIGURE A6 ###
################
  varnames = c("College", "White", "Gender","Age","Income", "Party", "Government waste", "Government regulation")
  X = cbind(joint.data$college, joint.data$white,joint.data$gender1,joint.data$age2,joint.data$income, joint.data$party3, joint.data$pro_gov, joint.data$pro_reg)
  tr = joint.data$tr_gov
  tab=t(apply(X,2,check, tr=tr))
  options(scipen=2)
  tab = round(tab, 2)
  tab = cbind(varnames, tab)
  tiff(filename = "figureA6.tif")
  plot.pval(results=tab, title="Balance Across Treatment and Control", legend=TRUE,legendx=0.15,legendy=2.5, textsize=0.9, parcex=0.8, at1=-0.35, at2=-0.15, at3=-0.9,xlim1=-0.85)
  dev.off()

################
###FIGURE A7 ###
################
  joint.data.rep = joint.data[joint.data$party3 == 3,]
  joint.data.rep = joint.data.rep[!is.na(joint.data.rep$tr_gov),]
  varnames = c("College", "White", "Gender","Age","Income", "Government waste", "Government regulation")
  X = cbind(joint.data.rep$college, joint.data.rep$white,joint.data.rep$gender1,joint.data.rep$age2,joint.data.rep$income, joint.data.rep$pro_gov, joint.data.rep$pro_reg)
  tr = joint.data.rep$tr_gov
  tab=t(apply(X,2,check, tr=tr))
  options(scipen=2)
  tab = round(tab, 2)
  tab = cbind(varnames, tab)
  tiff(filename = "balance_within_reps.tif")
  plot.pval(results=tab, title="Balance Within Republicans", legend=TRUE,legendx=0.15,legendy=2.5, textsize=0.9, parcex=0.8, at1=-0.35, at2=-0.15, at3=-0.9,xlim1=-0.85)
  dev.off()




