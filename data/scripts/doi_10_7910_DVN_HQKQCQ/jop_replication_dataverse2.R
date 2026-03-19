rm(list=ls())
library(foreign)
library(stringr)
library(rms)
library(pBrackets)
library(xtable)


as.n<-as.numeric
as.c<-as.character
as.f<-as.formula
len<-length

##Enter the file path to your working directory inside the quotes below, including final backslash, e.g.: "~/Dropbox/Selective Exposure Experiment/replication/"
wd<-""
setwd(wd)


##load custom functions
source("reg.results.function.R")
source("pred.function.R")

##load conjoint estimation data

##if .Rdata file use:
file.name<-paste(wd, "se.conjoint2.Rdata",sep="")
load(file.name)
dim(conj.dta)

# # ##if .tab file use:
# file.name<-paste(wd, "se.conjoint2.tab",sep="")
# conj.dta<-read.table(file.name, header=T, sep="\t")
# head(conj.dta)
# dim(conj.dta)

##load wide survey data with demos
#if .Rdata file use:
file.name<-paste(wd, "dd_se2.Rdata",sep="")
load(file.name)
dd<-d.survey
dim(dd)



# ##if .tab file use:
# file.name<-paste(wd, "dd_se2.tab",sep="")
# dd<-read.table(file.name, header=T, sep="\t")
# head(dd)
# dim(dd)


##distribution of partisans
table(conj.dta $dem, exclude=NULL) ##conjoint data
table(dd$dem, exclude=NULL) ##demographic data
table(conj.dta $rep, exclude=NULL)
table(dd$rep, exclude=NULL)


##what percentage of sample are members of at least one Affected Public?
groups<-c("female","smoker","senior","student","hcip","lose.weight")

##code indicator for belonging to at least one affected public
dd$ap<-0
conj.dta$ap<-0
for(i in 1:length(groups)){
	
	dd$ap[dd[,groups[i]]==1]<-1
    conj.dta$ap[conj.dta[,groups[i]]==1]<-1	
}

mean(dd$ap) ### 96% of sample belongs to at least one AP
mean(conj.dta$ap)
dim(dd[dd$ap==1,])##1013 members of aps
dim(conj.dta[conj.dta$ap==1,])[1]/24 ##1013 members of aps





###################################################################
###################################################################
###################################################################
###################################################################
###################### TABLE 1: Demographics of Sample
###################################################################
###################################################################
###################################################################

##Load population-weighted demographics from 2012 CCES

##if .Rdata use:
load("demos.cces2.Rdata")

##it .tab data use:
# demos.cces<-read.table("demos.cces2.tab", header=T, sep="\t")
# head(demos.cces)
# dim(demos.cces)

n.dems<-class(demos.cces$dems.cces)
demos.cces.dems<-c(demos.cces$dems.cces,rep(NA,3),21040)
demos.cces.reps<-c(demos.cces$reps.cces,rep(NA,3),15751)

var<-c("Female","Non-Hispanic White","Non-Hispanic Black","Hispanic/Latino","Other Race","Age (years)","Median HH Income ($1,000s)", "Has B.A.","Student","Senior (Over 55)", "Smoker","Trying to Lose Weight","Uninsured/Health Care Worker","N" )

len(var)
len(demos.cces.dems)
#"Inc. <$29k","Inc. $30-$39k","Inc. $40-$49k","Inc. $50-59k","Inc. $60-69k","Inc. $70-$79k","Inc. $80-$99k","Inc. $100-$119k","Inc. Over $120k", 


dd.dem<-subset(dd, dd$dem==1)
dd.rep<-subset(dd, dd$rep==1)

means.dems<-c( mean(dd.dem$female,na.rm=TRUE),  mean(dd.dem$white,na.rm=TRUE), mean(dd.dem$black,na.rm=TRUE),mean(dd.dem$latino,na.rm=TRUE),mean(dd.dem$other.race,na.rm=TRUE),mean(dd.dem$age, na.rm=TRUE),median(dd.dem$income, na.rm=TRUE),mean(dd.dem$BA,na.rm=TRUE), mean(dd.dem$student,na.rm=TRUE),mean(dd.dem$senior,na.rm=TRUE),mean(dd.dem$smoker,na.rm=TRUE),mean(dd.dem$lose.weight,na.rm=TRUE),mean(dd.dem$hcip,na.rm=TRUE),dim(dd.dem)[1])

means.reps<-c( mean(dd.rep$female,na.rm=TRUE),  mean(dd.rep$white,na.rm=TRUE), mean(dd.rep$black,na.rm=TRUE),mean(dd.rep$latino,na.rm=TRUE),mean(dd.rep$other.race,na.rm=TRUE),mean(dd.rep$age, na.rm=TRUE),median(dd.rep$income, na.rm=TRUE),mean(dd.rep$BA,na.rm=TRUE), mean(dd.rep$student,na.rm=TRUE),mean(dd.rep$senior,na.rm=TRUE),mean(dd.rep$smoker,na.rm=TRUE),mean(dd.rep$lose.weight,na.rm=TRUE),mean(dd.rep$hcip,na.rm=TRUE),dim(dd.rep)[1])


len(var)
len(means.dems)
len(means.reps)
len(demos.cces.reps)

demos<-cbind.data.frame(means.dems,demos.cces.dems,  means.reps, demos.cces.reps)
rownames(demos)<-var
demos

##for LaTex version:
xtable(demos, digits=c(2,2,2,2,2))








###################################################################
###################################################################
###################################################################
###################################################################
###################### TABLE 3: Treatment Effects by Subgroup
###################################################################
###################################################################
###################################################################


##### Make vectors of Affected Public variable names and corresponding relevant topic headlines

topics<-c("fem","smoking","socialsec","studentdebt","obamacare","weight")
groups<-c("female","smoker","senior","student","hcip","lose.weight")
group.names<-c("Women","Smokers","Seniors","Students","Uninsured/Health Care Workers","Trying to Lose Weight")
covars<-c("relevant","friend.source","unfriend.source")
coef.names<-c("Intercept","Relevant Topic","Friendly Source","Unfriendly Source")

##estimate treatment effects within 6 Affected Publics and report results in table
out<-reg.results(data=conj.dta, dv="response",covars=covars, groups=groups, group.names=group.names, topics=topics, interaction=F, coef.names=coef.names)
out

##print table for LaTex
xtable(out[[1]])




##############################
##############################
##############################
##############################
##############################
##############################
## FIGURE 2: PREDICTED PROBABILITES BY SUBGROUP
##############################
##############################
##############################
##############################
##############################
##############################




topics<-c("fem","smoking","socialsec","studentdebt","obamacare","weight")
groups<-c("female","smoker","senior","student","hcip","lose.weight")


results.ols<-pred(data=conj.dta,  groups=groups, topics=topics, model.type=c("ols"))

##see results for women
results.ols[[1]]

est<-NA
lb<-NA
ub<-NA

group.labels<-NA

for(i in 1:nrow(results.ols[[1]])){
temp1<-NA
temp2<-NA
temp3<-NA
group<-NA

for(j in 1:length(results.ols)){
	temp1[j]<-results.ols[[j]]$est[i]
	temp2[j]<- results.ols[[j]]$lb[i]
	temp3[j]<- results.ols[[j]]$ub[i]
	group[j]<-names(results.ols)[[j]]
	
}

est<-c(est, temp1)
lb<-c(lb, temp2)
ub<-c(ub, temp3)
group.labels<-c(group.labels, group)

}


est <- est[!is.na(est)]
lb <- lb[!is.na(lb)]
ub <- ub[!is.na(ub)]
group.labels <- group.labels[!is.na(group.labels)]

shape <- rep(c(0,1,2,4,5,6),6)
type<-c(rep("irrel",length(est)/2), rep("rel",length(est)/2) )
col<-I(est>=.5)
col[col==T]<-"black"
col[col==F]<-"grey"


res<-cbind.data.frame(est=est, lb=lb, ub=ub, shape=shape, col=col, type=type, group=group.labels)
res$source.type<-NA
res$source.type<-rep(c(rep("1unfriend",6),rep("2neutral",6), rep("3friend",6)),2)

res$shape2<-NA
res$shape2[res$type=="irrel"]<-17
res$shape2[res$type=="rel"]<-19

res$col2<-NA
res$col2[res$type=="irrel"]<-"grey"
res$col2[res$type=="rel"]<-"black"


res<-res[order(res$shape,  res$source.type, res$type),]
res

y.axis<-length(res$est):1


pdf(paste(wd,"pred.probs3.pdf",sep=""), width=, height=)

par(mar=c(4,6,4,6))
plot(res$est, length(res$est):1, pch=19, axes=F, xlim=c(min(res$lb), max(res$ub)), xlab="Probability of Selection", col=res$col2, ylab="", main="Relevant Topics Are Selected Often, \n Regardless of Source Type")
abline(v=.5, lty=2)

segments(res$lb,length(res$est):1, res$ub, length(res$est):1, col=res$col2)
axis(1, at=seq(-1,1, by=.1))
axis(4, at=seq(max(y.axis)-.5, 1.5, by=-2), labels=rep(c("Unfriendly Source","Neutral Source","Friendly Source"), 6), las=2, tck=-.02, cex.axis=.7)
axis(4, at=c((max(y.axis)+1):-5), tck=0, labels=F)

#axis(4, at=-5:50, labels=F, tck=0)  
axis(2, at=-5:40, labels=F, tck=0)
abline(h=seq(max(y.axis)-1.5, 2.5, by=-2), col="grey")

#abline(h=(length(res$est)/2)+.5, lty=1)
axis(2, at=seq(3.5, 33.5, by=6), labels=rev(c("Women","Smokers","Seniors","Students","Uninsured/
HC Worker","Trying to 
Lose Weight")) , las=2) 
legend("topright",pch=c(19,19,NA, NA), legend=c("Irrelevant Topic","Relevant Topic"), cex=.98, bg="white", col=c("grey","black") , lty=c(rep(1,2)) , lwd=c(rep(1,2)))

abline(h=seq(6.5, 30.5, by=6), col="black")

points(res$est, length(res$est):1, pch=19, col=res$col2)

dev.off()










###################################################################
###################################################################
###################################################################
###################################################################
###################### TABLE 4: Interaction Models
###################################################################
###################################################################
###################################################################


topics<-c("fem","smoking","socialsec","studentdebt","obamacare","weight")
groups<-c("female","smoker","senior","student","hcip","lose.weight")
group.names<-c("Women","Smokers","Seniors","Students","Uninsured/Health Care Workers","Trying to Lose Weight")
covars<-c("relevant","friend.source","unfriend.source")
coef.names<-c("Intercept","Relevant Topic","Friendly Source","Unfriendly Source","Relevant * Friendly","Relevant * Unfriendly")


out<-reg.results(data=conj.dta, dv="response",covars=covars, groups=groups, group.names=group.names, topics=topics, interaction=T, coef.names=coef.names)
out

##print table for LaTex
xtable(out[[1]])

###################################################################
###################################################################
###################################################################
###################################################################
###################### ONLINE APPENDIX
###################################################################
###################################################################
###################################################################





###################################################################
###################################################################
###################################################################
##### FIGURE A1: HEADLINE VERSION ROBUSTNESS CHECK
###################################################################
###################################################################
###################################################################

##data for headline version robustness check
conj.dta$head2<-as.c(conj.dta$head)
unique(conj.dta$head2)


placebo<-as.data.frame(matrix(nrow=len(unique(conj.dta$head2)),ncol=6))
colnames(placebo)<-c("head","version","mean","se","lb","ub")
placebo$head<-unique(conj.dta$head2)
placebo<-placebo[order(placebo$head),]
placebo$version<-c("abortion1","equalpay1","equalpay2","celeb1","celeb2","celeb3",
"socialsec1","socialsec2",
"socialsec3","christie1","christie2","nfl1","nfl2","nfl3","abortion2",
"abortion3","christie3","student1","obama1","obama2","equalpay2","smoking1",
"obama3","breast1","smoking2","breast2","breast3","smoking3","student2",
"student3",
"weight1","weight2","weight3")
placebo<-placebo[order(placebo$version),]
placebo$topic<-c(rep("abortion",3),rep("breast",3),rep("celeb",3),rep("christie",3),rep("equalpay",3),rep("nfl",3),rep("obamacare",3),rep("smoking",3),rep("socialsec",3),rep("studentdebt",3),rep("weight",3))
placebo$topic.mean<-NA
placebo

heads<-as.c(unique(placebo$head))
omitted<- heads[1]

##fill in means for each version
model<-ols(conj.dta$response~factor(conj.dta$head2, levels= heads), data=conj.dta,model=TRUE, x=TRUE, y=TRUE)
clust<-robcov(model, conj.dta$respid)
model2<-lm(response~factor(conj.dta$head2, levels= heads), data=conj.dta)
df<-model2$df.residual

##get means
	placebo$mean[placebo$head=="Abortion restrictions reinstated by federal court"]<-as.n(clust$coefficients[1])##firt get intercept(omitted condition)
	placebo$mean[2:nrow(placebo)]<-as.n(clust$coefficients[2:nrow(placebo)])+as.n(clust$coefficients[1])##get other means

ses<-NA

##grab largest SE
	se1<-as.n(sqrt(clust$var["Intercept","Intercept"]))
	se2<-as.n(sqrt(vcov(model)["Intercept","Intercept"]))
if(se1>se2){ses[1]<-se1}
if(se1<se2){ses[1]<-se2}


placebo$se[placebo$head=="Abortion restrictions reinstated by federal court"]<-ses[1]

for (i in 2:nrow(placebo)){
	
	se1<-as.n(sqrt(clust$var["Intercept","Intercept"]+clust$var[i,i]+2*(clust$var["Intercept",i]) ) ) 
	se2<-as.n(sqrt(vcov(model)["Intercept","Intercept"]+vcov(model)[i,i]+2*(vcov(model)["Intercept",i]) ) ) 

##grab largest SE	
if(se1>se2){ses[i]<-se1}
if(se1<se2){ses[i]<-se2}
	
	placebo$se[i]<-ses[i]
	
	
	}
	
placebo$lb<-placebo$mean-as.n(qt(.975, df))*placebo$se
placebo$ub<-placebo$mean+as.n(qt(.975, df))*placebo$se
placebo


topic.means<-NA
topics<-c("abortion","breast","equalpay","obamacare","studentdebt","nfl","weight","celeb","christie","smoking","socialsec")
for ( i in 1:len(topics)){
	test<-t.test(conj.dta$response[conj.dta[,topics[i]]==1])
	topic.means[i]<-as.n(test$estimate)
	}
	
for (i in 1:len(placebo[,1])){	
for (j in 1:len(topics)){	
	if(placebo$topic[i]==topics[j] ){
	placebo$topic.mean[i]<-topic.means[j]}
	}}

placebo<-placebo[order(placebo$topic.mean,decreasing=TRUE),]
topic2<-c("Breast Cancer","Equal Pay Act","Social Security","Abortion Restirctions","Student Debt","Smoking","Obamacare","Christie","Weight Loss Tips","Celebrity Gossip","NFL")

##plot these
coef.vec <- as.n(as.character( placebo$mean))
lb.vec <- as.n(as.character( placebo$lb))
ub.vec <- as.n(as.character(placebo$ub))
y.axis <- c(length(coef.vec):1)

len(coef.vec)==len(lb.vec)
len(lb.vec)==len(ub.vec)
len(coef.vec)==len(y.axis)

file.name<-paste(wd,"aggregation.test.pdf",sep="")
pdf(file.name, paper='special', height=6, width=6)
par(mar=c(5, 8, 2.5, 6))
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Rate of Selection", ylab = "",pch=19, cex = .7,
xlim=c(.2,.9), xaxs = "i", main = "Rate of Selection for Each Headline Version", col="black") 
segments(lb.vec, y.axis, ub.vec, y.axis, lwd =  .6, col="black")
axis(1, at = seq(0,1,.1), labels = seq(0,1,.1) , tick = T, tck= -.02, cex.axis = .8, mgp = c(2,.7,0)) 
axis(2, at=c(-4,y.axis), label = FALSE, las = 1, tick = T, 
mgp = c(2,1,0),tck=0, cex.axis = .7) 
axis(2, at=seq(2,max(y.axis)-1,3), label = FALSE, las = 1, tick = T, 
mgp = c(2,1,0),tck=-.01, cex.axis = .7) 
abline(h=seq(3.5,max(y.axis)-2.5,3),lwd=1, col="gray")
#abline(h=c(2.5,4.5),lwd=1, col="black")
#mtext(placebo$topic[seq(1,len(placebo$topic)-2,3)], side=2, at=seq(max(y.axis)-1,2,-3),las=2,cex=.8, line=1)
mtext(topic2, side=2, at=seq(max(y.axis)-1,2,-3),las=2,cex=.8, line=1)
dev.off()




###################################################################
###################################################################
###################################################################
##### FIGURE A2: Effects of individual headlines relative to relevant headline, by Affected Public 
###################################################################
###################################################################
###################################################################


##women
results<-as.data.frame(matrix(nrow=8,ncol=6))

colnames(results)<-c("group","test","effect","se","lb","ub")


results$test<-c( "social.sec" ,  "student.debt"   ,    "nfl"   ,       "christie"  ,   "celeb"     ,   "smoking"  ,    "obamacare" ,   "weight"   )


conj.dta$head3<-as.character(conj.dta$head3)

conj.dta$head4<-NA
conj.dta$head4[conj.dta$fem==1]<-"fem"
conj.dta$head4[conj.dta$fem!=1]<-conj.dta$head3[conj.dta$fem!=1]
table(conj.dta$head4)

##women
##subset to one subgroup at a time
sub<-subset(conj.dta, conj.dta[,"female"]==1 )
head(sub)

results$group<-rep("female", nrow(results)) ##label the group in results


model<-ols(response~factor(head4, levels=c( "fem","social.sec" ,  "student.debt"   ,    "nfl"   ,       "christie"  ,   "celeb"     ,   "smoking"  ,    "obamacare" ,   "weight"  ))+ unfriend.source + friend.source, data=sub,model=TRUE, x=TRUE, y=TRUE)
model
clust<-robcov(model, sub$respid)
clust
##get effects

for(j in 1:length(results$test)){
	
results$effect[j]<-as.n(clust$coefficients[paste("head4=",results$test[j],sep="")])

results$se[j]<-as.n(sqrt(clust$var[paste("head4=",results$test[j],sep=""),paste("head4=",results$test[j],sep="")]))

}




##get CIs
results$lb<-as.n(results$effect - 1.96*results$se)
results$ub<-as.n(results$effect + 1.96*results$se)

results.fem<-results





###smokers

results<-as.data.frame(matrix(nrow=10,ncol=6))

colnames(results)<-c("group","test","effect","se","lb","ub")


results$test<-c( "abortion","breast","equal.pay", "social.sec" ,  "student.debt"   ,    "nfl"   ,       "christie"  ,   "celeb"     ,      "obamacare" ,   "weight"   )

length(c( "abortion","breast","equal.pay", "social.sec" ,  "student.debt"   ,    "nfl"   ,       "christie"  ,   "celeb"     ,      "obamacare" ,   "weight"   ))==10

##smokers
##subset to one subgroup at a time
sub<-subset(conj.dta, conj.dta[,"smoker"]==1 )
head(sub)

results$group<-rep("smoker", nrow(results)) ##label the group in results


model<-ols(response~factor(head3, levels=c("smoking", "abortion","breast","equal.pay", "social.sec" ,  "student.debt"   ,    "nfl"   ,       "christie"  ,   "celeb"     ,      "obamacare" ,   "weight"   ))+ unfriend.source + friend.source, data=sub,model=TRUE, x=TRUE, y=TRUE)
clust<-robcov(model, sub$respid)
clust
##get effects

for(j in 1:length(results$test)){
	
results$effect[j]<-as.n(clust$coefficients[paste("head3=",results$test[j],sep="")])

results$se[j]<-as.n(sqrt(clust$var[paste("head3=",results$test[j],sep=""),paste("head3=",results$test[j],sep="")]))

}


##get CIs
results$lb<-as.n(results$effect - 1.96*results$se)
results$ub<-as.n(results$effect + 1.96*results$se)

results.smoker<-results



###seniors
results<-as.data.frame(matrix(nrow=10,ncol=6))

colnames(results)<-c("group","test","effect","se","lb","ub")


results$test<-c( "smoking","abortion","breast","equal.pay",  "student.debt"   ,    "nfl"   ,       "christie"  ,   "celeb"     ,      "obamacare" ,   "weight"   )

length(c( "smoking","abortion","breast","equal.pay",  "student.debt"   ,    "nfl"   ,       "christie"  ,   "celeb"     ,      "obamacare" ,   "weight"   ))==10

##subset to one subgroup at a time
sub<-subset(conj.dta, conj.dta[,"senior"]==1 )
head(sub)

results$group<-rep("senior", nrow(results)) ##label the group in results


model<-ols(response~factor(head3, levels=c("social.sec", "abortion","breast","equal.pay", "smoking" ,  "student.debt"   ,    "nfl"   ,       "christie"  ,   "celeb"     ,      "obamacare" ,   "weight"   ))+ unfriend.source + friend.source, data=sub,model=TRUE, x=TRUE, y=TRUE)
clust<-robcov(model, sub$respid)
clust
##get effects

for(j in 1:length(results$test)){
	
results$effect[j]<-as.n(clust$coefficients[paste("head3=",results$test[j],sep="")])

results$se[j]<-as.n(sqrt(clust$var[paste("head3=",results$test[j],sep=""),paste("head3=",results$test[j],sep="")]))

}


##get CIs
results$lb<-as.n(results$effect - 1.96*results$se)
results$ub<-as.n(results$effect + 1.96*results$se)
results.senior<-results



###students
results<-as.data.frame(matrix(nrow=10,ncol=6))

colnames(results)<-c("group","test","effect","se","lb","ub")


results$test<-c( "social.sec","abortion","breast","equal.pay", "nfl"   ,  "smoking",     "christie"  ,   "celeb"     ,      "obamacare" ,   "weight"   )

length(c( "social.sec","abortion","breast","equal.pay", "nfl"   ,  "smoking",     "christie"  ,   "celeb"     ,      "obamacare" ,   "weight"   ))==10

##subset to one subgroup at a time
sub<-subset(conj.dta, conj.dta[,"student"]==1 )
head(sub)

results$group<-rep("student", nrow(results)) ##label the group in results


model<-ols(response~factor(head3, levels=c("student.debt","social.sec", "abortion","breast","equal.pay", "smoking" ,    "nfl"   ,       "christie"  ,   "celeb"     ,      "obamacare" ,   "weight"   ))+ unfriend.source + friend.source, data=sub,model=TRUE, x=TRUE, y=TRUE)
clust<-robcov(model, sub$respid)
clust
##get effects

for(j in 1:length(results$test)){
	
results$effect[j]<-as.n(clust$coefficients[paste("head3=",results$test[j],sep="")])

results$se[j]<-as.n(sqrt(clust$var[paste("head3=",results$test[j],sep=""),paste("head3=",results$test[j],sep="")]))

}


##get CIs
results$lb<-as.n(results$effect - 1.96*results$se)
results$ub<-as.n(results$effect + 1.96*results$se)
results.student<-results




###hcip

results<-as.data.frame(matrix(nrow=10,ncol=6))

colnames(results)<-c("group","test","effect","se","lb","ub")


results$test<-c("social.sec","abortion","breast","equal.pay", "nfl"   ,"student.debt",  "smoking",     "christie"  ,   "celeb"     ,         "weight"   )

length(c("social.sec","abortion","breast","equal.pay", "nfl"   ,"student.debt",  "smoking",     "christie"  ,   "celeb"     ,         "weight"   ))==10

##subset to one subgroup at a time
sub<-subset(conj.dta, conj.dta[,"hcip"]==1 )
head(sub)

results$group<-rep("hcip", nrow(results)) ##label the group in results


model<-ols(response~factor(head3, levels=c("obamacare","social.sec", "abortion","breast","equal.pay", "smoking" ,    "nfl"   ,       "christie"  ,   "celeb"     ,        "weight","student.debt"   ))+ unfriend.source + friend.source, data=sub,model=TRUE, x=TRUE, y=TRUE)
clust<-robcov(model, sub$respid)
clust
##get effects

for(j in 1:length(results$test)){
	
results$effect[j]<-as.n(clust$coefficients[paste("head3=",results$test[j],sep="")])

results$se[j]<-as.n(sqrt(clust$var[paste("head3=",results$test[j],sep=""),paste("head3=",results$test[j],sep="")]))

}


##get CIs
results$lb<-as.n(results$effect - 1.96*results$se)
results$ub<-as.n(results$effect + 1.96*results$se)

results.hcip<-results



### weight

results<-as.data.frame(matrix(nrow=10,ncol=6))

colnames(results)<-c("group","test","effect","se","lb","ub")


results$test<-c("obamacare","social.sec","abortion","breast","equal.pay", "nfl"   ,"student.debt",  "smoking",     "christie"  ,   "celeb"           )

length(c("obamacare","social.sec","abortion","breast","equal.pay", "nfl"   ,"student.debt",  "smoking",     "christie"  ,   "celeb"           ))==10

##subset to one subgroup at a time
sub<-subset(conj.dta, conj.dta[,"lose.weight"]==1 )
head(sub)

results$group<-rep("lose.weight", nrow(results)) ##label the group in results


model<-ols(response~factor(head3, levels=c("weight","social.sec", "abortion","breast","equal.pay", "smoking" ,    "nfl"   ,       "christie"  ,   "celeb"     ,        "obamacare","student.debt"   ))+ unfriend.source + friend.source, data=sub,model=TRUE, x=TRUE, y=TRUE)
clust<-robcov(model, sub$respid)
clust
##get effects

for(j in 1:length(results$test)){
	
results$effect[j]<-as.n(clust$coefficients[paste("head3=",results$test[j],sep="")])

results$se[j]<-as.n(sqrt(clust$var[paste("head3=",results$test[j],sep=""),paste("head3=",results$test[j],sep="")]))

}


##get CIs
results$lb<-as.n(results$effect - 1.96*results$se)
results$ub<-as.n(results$effect + 1.96*results$se)

results.weight<-results


res<-list(results.fem,results.smoker,results.senior,results.student,results.hcip,results.weight)

for(i in 1:length(res)){
	
	res[[i]]<-res[[i]][order(res[[i]]$effect),]
	res[[i]]$head<-NA
	res[[i]]$head[res[[i]]$test=="obamacare"]<-"Obamacare"
	res[[i]]$head[res[[i]]$test=="student.debt"]<-"Student Debt"
	res[[i]]$head[res[[i]]$test=="social.sec"]<-"Social Security"
	res[[i]]$head[res[[i]]$test=="nfl"]<-"NFL"
	res[[i]]$head[res[[i]]$test=="christie"]<-"Christie"
	res[[i]]$head[res[[i]]$test=="celeb"]<-"Celebrity Gossip"
	res[[i]]$head[res[[i]]$test=="fem"]<-"Women's Issue"
	res[[i]]$head[res[[i]]$test=="abortion"]<-"Abortion"
	res[[i]]$head[res[[i]]$test=="equal.pay"]<-"Equal Pay"
	res[[i]]$head[res[[i]]$test=="breast"]<-"Breast Cancer"
	res[[i]]$head[res[[i]]$test=="smoking"]<-"Smoking Study"
	res[[i]]$head[res[[i]]$test=="weight"]<-"Weight Loss Tips"
	
}

group.names<-c("Women","Smokers","Seniors","Students","Uninsured/
HC Workers","Trying to
Lose Weight")

topic.names<-c("Women's Issue","Smoking Study","Social Security","Student Debt","Obamacare","Weight Loss Tips")


pdf(paste(wd,"headline.diffs.pdf",sep=""), width=7, height=6)

par(mfrow=c(3,2), mar=c(4,8,4,4))




for(i in 1:length(topic.names)){
	
	if(group.names[i]=="Women"){
	
plot(res[[i]]$effect,8:1, pch=19, xlim=c(min(res[[i]]$lb), max(res[[i]]$ub) ), ylab="",xlab=paste("Effect Relative to ",topic.names[i],sep=""), axes=F, main=group.names[i])
segments(res[[i]]$lb, 8:1, res[[i]]$ub, 8:1)
abline(v=0, lty=2)
axis(1, at=seq(-1,1,by=.1), cex.axis=.8)
axis(2, at=8:1, labels=res[[i]]$head, las=2,cex.axis=.8)	
	
}
	
	if(group.names[i]!="Seniors" & group.names[i]!="Women"){
plot(res[[i]]$effect,10:1, pch=19, xlim=c(min(res[[i]]$lb), max(res[[i]]$ub) ), ylab="",xlab=paste("Effect Relative to ",topic.names[i],sep=""), axes=F, main=group.names[i])
segments(res[[i]]$lb, 10:1, res[[i]]$ub, 10:1)
abline(v=0, lty=2)
axis(1, at=seq(-1,1,by=.1), cex.axis=.8)
axis(2, at=10:1, labels=res[[i]]$head, las=2,cex.axis=.8)}


	if(group.names[i]=="Seniors"){
plot(res[[i]]$effect,10:1, pch=19, xlim=c(min(res[[i]]$lb), .05 ), ylab="",xlab=paste("Effect Relative to ",topic.names[i],sep=""), axes=F, main=group.names[i])
segments(res[[i]]$lb, 10:1, res[[i]]$ub, 10:1)
abline(v=0, lty=2)
axis(1, at=seq(-1,1,by=.1), cex.axis=.8)
axis(2, at=10:1, labels=res[[i]]$head, las=2, cex.axis=.8)}

}
dev.off()






###################################################################
###################################################################
###################################################################
###################################################################
####### Figure A3: MTurk Pre-Test results
###################################################################
###################################################################
###################################################################
###################################################################

##Ratings of partisan bias for news outlets

##if data is in .Rdata format use:
load("pre_test2.Rdata")
head(dd2)

##if data is in .tab format use:
#dd2<-read.table("pre_test2.tab", header=T, sep="\t")


head(dd2)
dim(dd2)

##Estimate means
networks<-c("fox","cnn","msnbc","huffpo","drudge","nyt","cbs","abc","nbc","reuters","bbc","ap","usa","fin.times")

networks2<-c("Fox News","CNN","MSNBC","Huffington Post","Drudge Report","New York Times","CBS","ABC","NBC","Reuters","BBC","AP","USA Today","Financial Times")

source.results<-as.data.frame(matrix(nrow=len(networks), ncol=6))
source.results[,1]<-networks
colnames(source.results)<-c("source","mean","lb","ub","party","col")
source.results$party<-"all"
source.results$col<-"black"

source.results.dem<-as.data.frame(matrix(nrow=len(networks), ncol=6))
source.results.dem[,1]<-networks
colnames(source.results.dem)<-c("source","mean","lb","ub","party","col")
source.results.dem$party<-"dem"
source.results.dem$col<-"blue"

source.results.rep<-as.data.frame(matrix(nrow=len(networks), ncol=6))
source.results.rep[,1]<-networks
colnames(source.results.rep)<-c("source","mean","lb","ub","party","col")
source.results.rep$party<-"rep"
source.results.rep$col<-"red"

for (i in 1:len(networks)){
test<-t.test(dd2[,networks[i]], na.rm=TRUE)
source.results[source.results$source==networks[i],"mean"]<-as.n(test$estimate)
source.results[source.results$source==networks[i],"lb"]<-as.n(test$conf.int[1])
source.results[source.results$source==networks[i],"ub"]<-as.n(test$conf.int[2])

test.dem<-t.test(dd2[dd2$dem==1,networks[i]], na.rm=TRUE)
source.results.dem[source.results$source==networks[i],"mean"]<-as.n(test.dem$estimate)
source.results.dem[source.results$source==networks[i],"lb"]<-as.n(test.dem$conf.int[1])
source.results.dem[source.results$source==networks[i],"ub"]<-as.n(test.dem$conf.int[2])

test.rep<-t.test(dd2[dd2$rep==1,networks[i]], na.rm=TRUE)
source.results.rep[source.results$source==networks[i],"mean"]<-as.n(test.rep$estimate)
source.results.rep[source.results$source==networks[i],"lb"]<-as.n(test.rep$conf.int[1])
source.results.rep[source.results$source==networks[i],"ub"]<-as.n(test.rep$conf.int[2])

}

source.results$source<-networks2
source.results.dem$source<-networks2
source.results.rep$source<-networks2


source.results<-source.results[order(source.results$mean, decreasing=TRUE),]

source.results$order<-seq(1,len(networks),1)

order.frame<-cbind.data.frame(source.results$order, source.results$source)
colnames(order.frame)<-c("order","source")
order.frame

source.results.dem<-merge(source.results.dem,order.frame,by="source")
source.results.dem

source.results.rep<-merge(source.results.rep,order.frame,by="source")
source.results.rep


source<-rbind.data.frame(source.results,
source.results.dem,
source.results.rep)

source<-source[order(source$order),]
source$shape[source$party=="all"]<-19
source$shape[source$party=="dem"]<-15
source$shape[source$party=="rep"]<-17

####graph results
##make graphical parameters
coef.vec<-source[,"mean"]
lb.vec<- source[,"lb"]
ub.vec<- source[,"ub"]
y.axis <- c(length(coef.vec):1)

file.name<-paste(wd,"source.pretest.pdf",sep="")

pdf(file=file.name, width = 7, height=7)
par(mar=c(5, 7, 5,5))
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Perceived Partisan Slant", ylab = "", pch = source$shape, cex = .7, xlim = c(0,100),   main = "Perceived Partisan Slant of Major News Outlets", col=source$col)
segments(50, 0, 50, max(y.axis),lty=2, lwd =  .7)
segments(lb.vec, y.axis, ub.vec, y.axis, lwd =  .7, col=source$col)
axis(1, at = seq(0,100,10), labels = T, tick = T, tck=-0.03, cex.axis = .8, mgp = c(2,.7,0)) 
axis(2, at = seq(max(y.axis),-10,-1), label =F, las = 1, tick = T, mgp = c(2,1,0),tck=-0, cex.axis = .8) 
axis(2, at =seq(2,max(y.axis)-1,3), label =F, las = 1, tick = T, mgp = c(2,1,0),tck=-.01, cex.axis = .8) 
abline(h=seq(max(y.axis)-2.5,3.5,-3), lty=1, lwd=1, col="gray")
mtext(rev(source$source[source$party=="all"]),side=2, at=seq(2,max(y.axis)-1,3),line=.34, las=2, cex=.9)
mtext(c("Favor Democrats","Favors Neither Party","Favor Republicans"),side=1, at=c(0,50,100),line=1.6, las=1, cex=.9)
legend("bottomright", c("All","Dems","Reps"),pch=source$shape, cex=1,  col=c("black","blue","red"), bg="white")
dev.off()






###################################################################
###################################################################
####### TABLES A4 and A5: Core Results by Party
###################################################################
###################################################################


topics<-c("fem","smoking","socialsec","studentdebt","obamacare","weight")
groups<-c("female","smoker","senior","student","hcip","lose.weight")
group.names<-c("Women","Smokers","Seniors","Students","Uninsured/Health Care Workers","Trying to Lose Weight")
covars<-c("relevant","friend.source","unfriend.source")
coef.names<-c("Intercept","Relevant Topic","Friendly Source","Unfriendly Source")

##Democrats
out<-reg.results(data=conj.dta[conj.dta$dem==1, ], dv="response",covars=covars, groups=groups, group.names=group.names, topics=topics, interaction=F, coef.names=coef.names)
xtable(out[[1]])


##Republicans
out<-reg.results(data=conj.dta[conj.dta$rep==1, ], dv="response",covars=covars, groups=groups, group.names=group.names, topics=topics, interaction=F, coef.names=coef.names)
xtable(out[[1]])


###################################################################
###################################################################
####### TABLES A6 and A7: Interaction Results by Party
###################################################################
###################################################################


topics<-c("fem","smoking","socialsec","studentdebt","obamacare","weight")
groups<-c("female","smoker","senior","student","hcip","lose.weight")
group.names<-c("Women","Smokers","Seniors","Students","Uninsured/Health Care Workers","Trying to Lose Weight")
covars<-c("relevant","friend.source","unfriend.source")
coef.names<-c("Intercept","Relevant Topic","Friendly Source","Unfriendly Source", "Relevant*Friendly","Relevant*Unfriendly")

##Democrats
out<-reg.results(data=conj.dta[conj.dta$dem==1, ], dv="response",covars=covars, groups=groups, group.names=group.names, topics=topics, interaction=T, coef.names=coef.names)
xtable(out[[1]])


##Republicans
out<-reg.results(data=conj.dta[conj.dta$rep==1, ], dv="response",covars=covars, groups=groups, group.names=group.names, topics=topics, interaction=T, coef.names=coef.names)
xtable(out[[1]])





 

###################################################################
###################################################################
###################################################################
###################################################################
###################### Figures A4 and A5: Mean Rates of Selection by Party
###################################################################
###################################################################
###################################################################




##Democrats
results.ols<-pred(data=conj.dta[conj.dta$dem==1,],  groups=groups, topics=topics, model.type=c("ols"))



est<-NA
lb<-NA
ub<-NA

group.labels<-NA

for(i in 1:nrow(results.ols[[1]])){



temp1<-NA
temp2<-NA
temp3<-NA

group<-NA

for(j in 1:length(results.ols)){

	
	temp1[j]<-results.ols[[j]]$est[i]
	temp2[j]<- results.ols[[j]]$lb[i]
	temp3[j]<- results.ols[[j]]$ub[i]
	group[j]<-names(results.ols)[[j]]
	
}

est<-c(est, temp1)
lb<-c(lb, temp2)
ub<-c(ub, temp3)
group.labels<-c(group.labels, group)

}


est <- est[!is.na(est)]
lb <- lb[!is.na(lb)]
ub <- ub[!is.na(ub)]
group.labels <- group.labels[!is.na(group.labels)]

shape <- rep(c(0,1,2,4,5,6),6)
type<-c(rep("irrel",length(est)/2), rep("rel",length(est)/2) )
col<-I(est>=.5)
col[col==T]<-"black"
col[col==F]<-"grey"


res<-cbind.data.frame(est=est, lb=lb, ub=ub, shape=shape, col=col, type=type, group=group.labels)
res$source.type<-NA
res$source.type<-rep(c(rep("1unfriend",6),rep("2neutral",6), rep("3friend",6)),2)

res$shape2<-NA
res$shape2[res$type=="irrel"]<-17
res$shape2[res$type=="rel"]<-19

res$col2<-NA
res$col2[res$type=="irrel"]<-"grey"
res$col2[res$type=="rel"]<-"black"


res<-res[order(res$shape,  res$source.type, res$type),]
res

y.axis<-length(res$est):1


pdf(paste(wd,"pred.probs3.dems.pdf",sep=""), width=, height=)

par(mar=c(4,6,4,6))
plot(res$est, length(res$est):1, pch=19, axes=F, xlim=c(min(res$lb), max(res$ub)), xlab="Probability of Selection", col=res$col2, ylab="", main="Relevant Topics Are Selected Often, \n Regardless of Source Type (Democrats)")
abline(v=.5, lty=2)

segments(res$lb,length(res$est):1, res$ub, length(res$est):1, col=res$col2)
axis(1, at=seq(-1,1, by=.1))
axis(4, at=seq(max(y.axis)-.5, 1.5, by=-2), labels=rep(c("Unfriendly Source","Neutral Source","Friendly Source"), 6), las=2, tck=-.02, cex.axis=.7)
axis(4, at=c((max(y.axis)+1):-5), tck=0, labels=F)

#axis(4, at=-5:50, labels=F, tck=0)  
axis(2, at=-5:40, labels=F, tck=0)
abline(h=seq(max(y.axis)-1.5, 2.5, by=-2), col="grey")

#abline(h=(length(res$est)/2)+.5, lty=1)
axis(2, at=seq(3.5, 33.5, by=6), labels=rev(c("Women","Smokers","Seniors","Students","Uninsured/
HC Worker","Trying to 
Lose Weight")) , las=2) 
legend("topright",pch=c(19,19,NA, NA), legend=c("Irrelevant Topic","Relevant Topic"), cex=.98, bg="white", col=c("grey","black") , lty=c(rep(1,2)) , lwd=c(rep(1,2)))

abline(h=seq(6.5, 30.5, by=6), col="black")

points(res$est, length(res$est):1, pch=19, col=res$col2)

dev.off()



##Republicans
results.ols<-pred(data=conj.dta[conj.dta$rep==1,],  groups=groups, topics=topics, model.type=c("ols"))



est<-NA
lb<-NA
ub<-NA

group.labels<-NA

for(i in 1:nrow(results.ols[[1]])){



temp1<-NA
temp2<-NA
temp3<-NA

group<-NA

for(j in 1:length(results.ols)){

	
	temp1[j]<-results.ols[[j]]$est[i]
	temp2[j]<- results.ols[[j]]$lb[i]
	temp3[j]<- results.ols[[j]]$ub[i]
	group[j]<-names(results.ols)[[j]]
	
}

est<-c(est, temp1)
lb<-c(lb, temp2)
ub<-c(ub, temp3)
group.labels<-c(group.labels, group)

}


est <- est[!is.na(est)]
lb <- lb[!is.na(lb)]
ub <- ub[!is.na(ub)]
group.labels <- group.labels[!is.na(group.labels)]

shape <- rep(c(0,1,2,4,5,6),6)
type<-c(rep("irrel",length(est)/2), rep("rel",length(est)/2) )
col<-I(est>=.5)
col[col==T]<-"black"
col[col==F]<-"grey"


res<-cbind.data.frame(est=est, lb=lb, ub=ub, shape=shape, col=col, type=type, group=group.labels)
res$source.type<-NA
res$source.type<-rep(c(rep("1unfriend",6),rep("2neutral",6), rep("3friend",6)),2)

res$shape2<-NA
res$shape2[res$type=="irrel"]<-17
res$shape2[res$type=="rel"]<-19

res$col2<-NA
res$col2[res$type=="irrel"]<-"grey"
res$col2[res$type=="rel"]<-"black"


res<-res[order(res$shape,  res$source.type, res$type),]
res

y.axis<-length(res$est):1


pdf(paste(wd,"pred.probs3.reps.pdf",sep=""), width=, height=)

par(mar=c(4,6,4,6))
plot(res$est, length(res$est):1, pch=19, axes=F, xlim=c(min(res$lb), max(res$ub)), xlab="Probability of Selection", col=res$col2, ylab="", main="Relevant Topics Are Selected Often, \n Regardless of Source Type (Republicans)")
abline(v=.5, lty=2)

segments(res$lb,length(res$est):1, res$ub, length(res$est):1, col=res$col2)
axis(1, at=seq(-1,1, by=.1))
axis(4, at=seq(max(y.axis)-.5, 1.5, by=-2), labels=rep(c("Unfriendly Source","Neutral Source","Friendly Source"), 6), las=2, tck=-.02, cex.axis=.7)
axis(4, at=c((max(y.axis)+1):-5), tck=0, labels=F)

#axis(4, at=-5:50, labels=F, tck=0)  
axis(2, at=-5:40, labels=F, tck=0)
abline(h=seq(max(y.axis)-1.5, 2.5, by=-2), col="grey")

#abline(h=(length(res$est)/2)+.5, lty=1)
axis(2, at=seq(3.5, 33.5, by=6), labels=rev(c("Women","Smokers","Seniors","Students","Uninsured/
HC Worker","Trying to 
Lose Weight")) , las=2) 
legend("topright",pch=c(19,19,NA, NA), legend=c("Irrelevant Topic","Relevant Topic"), cex=.98, bg="white", col=c("grey","black") , lty=c(rep(1,2)) , lwd=c(rep(1,2)))

abline(h=seq(6.5, 30.5, by=6), col="black")

points(res$est, length(res$est):1, pch=19, col=res$col2)

dev.off()









###################################################################
###################################################################
###################################################################
###################################################################
###################### FIGURE A6: Diff in Diff Mechanism Check
###################################################################
###################################################################
###################################################################


dim(conj.dta)
conj.test<-conj.dta
dim(conj.test)
dim(conj.dta)

topics<-c("fem","smoking","socialsec","studentdebt","obamacare","weight")
groups<-c("female","smoker","senior","student","hcip", "lose.weight")



model<-ols(response ~ fem + female + fem*female , data=conj.test,model=TRUE, x=TRUE, y=TRUE)
clust<-robcov(model, conj.test$respid)
clust


##get diff in diff
wom.dd<-as.n(clust$coefficients["fem * female"])
##get SE
##check manually
d1<-t.test(conj.test$response[conj.test$fem==1 & conj.test$female==0], conj.test$response[conj.test$fem==0 & conj.test$female==0] )##men
d2<-t.test(conj.test$response[conj.test$fem==1 & conj.test$female==1], conj.test$response[conj.test$fem==0 & conj.test$female==1] )##women
diff1<-d1$estimate[1] - d1$estimate[2] ##effect for men
diff2<-d2$estimate[1] - d2$estimate[2] ##effect for women
wom.dd
diff2-diff1##get same result

## Load function to extract diff in diffs across groups ...
source("did_function.R")

dd.results<-did(data=conj.dta, groups=groups, topics=topics)

dd.results$topic<-c("Women/Men","Smokers/Others","Seniors/Others","Students/Others","Uninsured or HC Workers
/ Others","Trying to Lose Weight/
Others")

##plot these results
coef.vec <- as.n(as.character( dd.results$did))
lb.vec <- as.n(as.character( dd.results$lb))
ub.vec <- as.n(as.character(dd.results$ub))
y.axis <- c(length(coef.vec):1)

len(coef.vec)==len(lb.vec)
len(lb.vec)==len(ub.vec)
len(coef.vec)==len(y.axis)

file.name<-paste(wd,"did.pooled2.pdf",sep="")
pdf(file.name, paper='special', height=4, width=6)


par(mar=c(5, 10, 3.5, 6))
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Difference-in-Difference", ylab = "",pch=19, cex = .7,
xlim=c(min(dd.results$lb), max(dd.results$ub)), xaxs = "i", main = "Affected Public Members Respond More Favorably 
to Relevant Content than their Counterparts ", col="black", cex.main=.85) 
segments(lb.vec, y.axis, ub.vec, y.axis, lwd =  .6, col="black")
abline(v=0, lty=2)
axis(1, at = seq(-1,1,.1), labels = seq(-1,1,.1) , tick = T, tck= -.02, cex.axis = .8, mgp = c(2,.7,0)) 
axis(2, at=c(-4,y.axis), label = FALSE, las = 1, tick = T, 
mgp = c(2,1,0),tck=0, cex.axis = .7) 
axis(2, at=seq(1,max(y.axis),1), label = FALSE, las = 1, tick = T, 
mgp = c(2,1,0),tck=-.01, cex.axis = .7) 
abline(h=seq(1.5,max(y.axis)-.5,1),lwd=1, col="gray")
mtext(dd.results$topic, side=2, at=seq(max(y.axis),1,-1),las=2,cex=.8, line=1)


dev.off()





###################################################################
###################################################################
####### FIGURE A7: Differences in Treatment Effects, Bootstrap Analysis
###################################################################
###################################################################

##This procedure takes a while...
##UN_COMMENT THE FOLLOWING CODE TO PERFORM THE BOOTSTRAP, OR PICK UP BELOW THE COMMENTED CODE TO LOAD BOOTSTRAP RESULTS AND CONSTRUCT FIGURE A7.

##make storage object

# 




# # topics<-c("fem","smoking","socialsec","studentdebt","obamacare","weight")
# groups<-c("female","smoker","senior","student","hcip","lose.weight")


# rel.min.unfriend<-NA
# rel.min.friend<-NA


# results<-cbind.data.frame(rel.min.unfriend=rel.min.unfriend, rel.min.friend=rel.min.friend)

# results.diff<-list(results,results,results,results,results,results)
# names(results.diff)<-c(groups)

# file.name=paste(wd, "results.diff_rep.Rdata",sep="")
# set.seed(31415)


# for(i in 1:length(results.diff)){


	
# sub<-subset(conj.dta, conj.dta[,groups[i]]==1 )
# ids<-unique(sub$respid)


# for(j in 1:1000){



# ids2<-sample(ids,size=length(ids),replace=T)

# rows<-rownames(sub[sub$respid==ids2[1],])

# #get relevant row numbers
# for(k in 2:length(ids2)){
	
	# rows.new<-rownames(sub[sub$respid==ids2[k],c("respid","head")])
	# rows<-c(rows,rows.new)
	
# }


# sub2<-sub[rows,c(topics[i],"unfriend.source","friend.source","response")]
# # head(sub2)
# # dim(sub2)
# # dim(sub)

# model2<-lm(response~sub2[,topics[i]] + unfriend.source + friend.source, data=sub2)

	
	# #differences
	# results.diff[[i]][j,"rel.min.unfriend"]<-abs(model2$coefficients["sub2[, topics[i]]"])-abs(model2$coefficients["unfriend.source"])
	# results.diff[[i]][j,"rel.min.friend"]<-abs(model2$coefficients["sub2[, topics[i]]"])-abs(model2$coefficients["friend.source"])

	
# }
# save(results.diff,file=file.name)
# print(i)
# }

# mean(results.diff[["hcip"]][,1])




# # ######

# ###change comparison group for source effects
# ##make storage object


# topics<-c("fem","smoking","socialsec","studentdebt","obamacare","weight")
# groups<-c("female","smoker","senior","student","hcip","lose.weight")

# rel.min.unfriend<-NA
# rel.min.friend<-NA


# results<-cbind.data.frame(rel.min.unfriend=rel.min.unfriend)

# results.diff<-list(results,results,results,results,results,results)
# names(results.diff)<-c(groups)

# file.name=paste(wd, "results.diff2.Rdata",sep="")
# set.seed(31415)

# for(i in 1:length(results.diff)){

# sub<-subset(conj.dta, conj.dta[,groups[i]]==1 )
# ids<-unique(sub$respid)

# for(j in 1:1000){


# ids2<-sample(ids,size=length(ids),replace=T)

# rows<-as.numeric(rownames(sub[sub$respid==ids2[1],]))

# ##get relevant row numbers
# for(k in 2:length(ids2)){
	
	# rows.new<-as.numeric(rownames(sub[sub$respid==ids2[k],c("respid","head")]))
	# rows<-c(rows,rows.new)
	
# }

# sub2<-sub[as.character(rows),c(topics[i],"unfriend.source","usa","response")]
# head(sub2)

# model2<-lm(response~sub2[,topics[i]] + unfriend.source+usa , data=sub2)

	
	# ##differences
	# results.diff[[i]][j,"rel.min.unfriend"]<-abs(model2$coefficients["sub2[, topics[i]]"])-abs(model2$coefficients["unfriend.source"])

	
# }
# save(results.diff,file=file.name)
# print(i)
# }





# # 
# ####make table of results from first set of tests
results.diff<-NA
load(paste(wd, "results.diff_rep.Rdata",sep=""))

diff.unfriend<-NA
diff.friend<-NA
lb.unfriend<-NA
ub.unfriend<-NA
lb.friend<-NA
ub.friend<-NA

for(i in 1:length(results.diff)){
	
	diff.unfriend[i]<-mean(results.diff[[i]][,"rel.min.unfriend"])
	diff.friend[i]<-mean(results.diff[[i]][,"rel.min.friend"])
	
	lb.unfriend[i]<-quantile(results.diff[[i]][,"rel.min.unfriend"], probs=.025)
	ub.unfriend[i]<-quantile(results.diff[[i]][,"rel.min.unfriend"], probs=.975)
	
	lb.friend[i]<-quantile(results.diff[[i]][,"rel.min.friend"], probs=.025)
	ub.friend[i]<-quantile(results.diff[[i]][,"rel.min.friend"], probs=.975)
		
	
}

results<-cbind.data.frame(group=groups,unfriend=diff.unfriend,friend=diff.friend,lb.unfriend=lb.unfriend,ub.unfriend=ub.unfriend,lb.friend=lb.friend,ub.friend=ub.friend)
results
results$test<-"rel.to.neutral"



####same thing for second set of tests
results.diff<-NA
load(paste(wd, "results.diff2.Rdata",sep=""))
head(results.diff[[1]])
dim(results.diff[[1]])


diff.unfriend<-NA
lb.unfriend<-NA
ub.unfriend<-NA


for(i in 1:length(results.diff)){
	
	diff.unfriend[i]<-mean(results.diff[[i]][,"rel.min.unfriend"])
	lb.unfriend[i]<-quantile(results.diff[[i]][,"rel.min.unfriend"], probs=.025)
	ub.unfriend[i]<-quantile(results.diff[[i]][,"rel.min.unfriend"], probs=.975)
		
	
}

results2<-na.omit(cbind.data.frame(group=groups,unfriend=diff.unfriend,lb.unfriend=lb.unfriend,ub.unfriend=ub.unfriend))
results2$test<-"rel.to.outparty"
results2$lb.friend<-NA
results2$ub.friend<-NA
results2$friend<-NA


results
results2<-results2[,c("group","unfriend","friend","lb.unfriend","ub.unfriend","lb.friend","ub.friend","test")]
results2


results.all<-rbind.data.frame(results, results2)
results.all<-results.all[order(results.all$group, decreasing=F),]
results.all

groups<-as.character(unique(results.all$group))

##get results for women
effect<-c(results.all$unfriend[results.all$group==groups[1] & results.all$test=="rel.to.neutral"],results.all$unfriend[results.all$group==groups[1]& results.all$test=="rel.to.outparty"], results.all$friend[results.all$group==groups[1]& results.all$test=="rel.to.neutral"]) 

lb<-c(   results.all$lb.unfriend[results.all$group==groups[1] & results.all$test=="rel.to.neutral"],results.all$lb.unfriend[results.all$group==groups[1]& results.all$test=="rel.to.outparty"], results.all$lb.friend[results.all$group==groups[1]& results.all$test=="rel.to.neutral"])

ub<-c(   results.all$ub.unfriend[results.all$group==groups[1] & results.all$test=="rel.to.neutral"],results.all$ub.unfriend[results.all$group==groups[1]& results.all$test=="rel.to.outparty"], results.all$ub.friend[results.all$group==groups[1]& results.all$test=="rel.to.neutral"])

group<-rep(groups[1],3)

test<-c("neutral.unfriend","outparty.unfriend","neutral.friend")
d<-cbind.data.frame(effect,lb,ub,group,test)

##fill in rest of groups
for(i in 2:length(groups)){
	
effect<-c(results.all$unfriend[results.all$group==groups[i] & results.all$test=="rel.to.neutral"],results.all$unfriend[results.all$group==groups[i]& results.all$test=="rel.to.outparty"], results.all$friend[results.all$group==groups[i]& results.all$test=="rel.to.neutral"]) 

lb<-c(   results.all$lb.unfriend[results.all$group==groups[i] & results.all$test=="rel.to.neutral"],results.all$lb.unfriend[results.all$group==groups[i]& results.all$test=="rel.to.outparty"], results.all$lb.friend[results.all$group==groups[i]& results.all$test=="rel.to.neutral"])

ub<-c(   results.all$ub.unfriend[results.all$group==groups[i] & results.all$test=="rel.to.neutral"],results.all$ub.unfriend[results.all$group==groups[i]& results.all$test=="rel.to.outparty"], results.all$ub.friend[results.all$group==groups[i]& results.all$test=="rel.to.neutral"])

group<-rep(groups[i],3)

test<-c("neutral.unfriend","outparty.unfriend","neutral.friend")
new<-cbind.data.frame(effect,lb,ub,group,test)
d<-rbind.data.frame(d,new)
	
}
d[,1:3]<-d[,1:3]*100
d$group<-as.character(d$group)
d


d$group[d$group=="female"]<-"Women"
d$group[d$group=="hcip"]<-"Uninsured/
Health Care Workers"
d$group[d$group=="lose.weight"]<-"Trying to Lose Weight"
d$group[d$group=="senior"]<-"Seniors (Over 55)"
d$group[d$group=="student"]<-"Students"
d$group[d$group=="smoker"]<-"Smokers"

d$order<-NA
d$order[d$group=="Women"]<-2
d$order[d$group=="Smokers"]<-3
d$order[d$group=="Seniors (Over 55)"]<-4
d$order[d$group=="Students"]<-5
d$order[d$group=="Uninsured/
Health Care Workers"]<-6
d$order[d$group=="Trying to Lose Weight"]<-7
d<-d[order(d$order,decreasing=F),]

d$shape<-NA
d$shape[d$test=="neutral.unfriend"]<-19
d$shape[d$test=="outparty.unfriend"]<-15
d$shape[d$test=="neutral.friend"]<-17

head(d)

d2<-cbind.data.frame(round(d[,1:3],digits=3), d[,4:ncol(d)])
d2test<-as.c(d2$test)
d2$test2<-NA
d2$test2[d2$test=="neutral.unfriend"]<-"Topic - Unfriendly Source (a)"
d2$test2[d2$test=="outparty.unfriend"]<-"Topic - Unfriendly Source (b)"
d2$test2[d2$test=="neutral.friend"]<-"Topic - Friendly Source"
d2<-d2[,c(1:4,8)]
d2<-d2[,c("test2","group","effect","lb","ub")]
colnames(d2)<-c("Test","Subgroup","Difference
(Percentage Points)","95% L.B.","95% U.B.")

library(xtable)
xtable(d2)

cats<-unique(d$group)
head(d)

pdf(paste(wd,"boot.effects.pdf",sep=""))
par(mar=c(4,9.5,4,4))
y.axis<-length(d$effect):1
plot(d$effect, y.axis, pch=d$shape, xlim=c(min(d$lb),max(d$ub)+2), yaxt="n",ylab="",main="Differences Between Topic and Source Effects",xlab="Difference (Percentage Points)")
abline(v=0,lty=2)
segments(d$lb,y.axis,d$ub,y.axis)
mtext(side=2, cats, at=seq(max(y.axis-1),2,by=-3),las=2,line=1)
axis(side=2, at=seq(max(y.axis-1),2,by=-3),tick=.5,labels=F)
abline(h=seq(3.5,17.5,by=3),col="grey")
text(y=max(y.axis),x=d$ub[1]+8,"Topic minus Unfriendly Source (a)" ,cex=.6)
text(y=max(y.axis)-1,x=d$ub[2]+8,"Topic minus Unfriendly Source (b)" ,cex=.6)
text(y=max(y.axis)-2,x=d$ub[3]+7,"Topic minus Friendly Source" ,cex=.6)
dev.off()








###################################################################
###################################################################
####### Tables A8, A9 and A10: IDEOLOGICAL PARTISANS AND NEWS AVOIDERS
###################################################################
###################################################################




topics<-c("fem","smoking","socialsec","studentdebt","obamacare","weight")
groups<-c("female","smoker","senior","student","hcip","lose.weight")
group.names<-c("Women","Smokers","Seniors","Students","Uninsured/Health Care Workers","Trying to Lose Weight")
covars<-c("relevant","friend.source","unfriend.source")
coef.names<-c("Intercept","Relevant Topic","Friendly Source","Unfriendly Source")

##Liberal Democrats
out<-reg.results(data=conj.dta[conj.dta$libdem==1, ], dv="response",covars=covars, groups=groups, group.names=group.names, topics=topics, interaction=F, coef.names=coef.names)
xtable(out[[1]])

##Conservative Republicans
out<-reg.results(data=conj.dta[conj.dta$conrep==1, ], dv="response",covars=covars, groups=groups, group.names=group.names, topics=topics, interaction=F, coef.names=coef.names)
xtable(out[[1]])

##News Consumers Only
out<-reg.results(data=conj.dta[conj.dta$no.news.std==0, ], dv="response",covars=covars, groups=groups, group.names=group.names, topics=topics, interaction=F, coef.names=coef.names)
out
xtable(out[[1]])






###################################################################
###################################################################
####### FIGURE A8: CHECKING CONJOINT ASSUMPTIONS
###################################################################
###################################################################




groups<-c("female","smoker","senior","student","hcip","lose.weight")
topics<-c("fem","smoking","socialsec","studentdebt","obamacare","weight")

##code pooled relevance treatment
conj.dta$relevant<-NA
conj.dta$relevant[conj.dta$smoker==1 & conj.dta$smoking==0 ]<-0
conj.dta$relevant[conj.dta$female==1 & conj.dta$fem==0]<-0
conj.dta$relevant[conj.dta$senior==1 & conj.dta$socialsec==0]<-0
conj.dta$relevant[conj.dta$student==1 & conj.dta$studentdebt==0]<-0
conj.dta$relevant[conj.dta$lose.weight==1 & conj.dta$weight==0]<-0
conj.dta$relevant[conj.dta$hcip==1 & conj.dta$obamacare==0]<-0


conj.dta$relevant[conj.dta$smoker==1 & conj.dta$smoking==1]<-1
conj.dta$relevant[conj.dta$female==1 & conj.dta$fem==1]<-1
conj.dta$relevant[conj.dta$senior==1 & conj.dta$socialsec==1]<-1
conj.dta$relevant[conj.dta$student==1 & conj.dta$studentdebt==1]<-1
conj.dta$relevant[conj.dta$lose.weight==1 & conj.dta$weight==1]<-1
conj.dta$relevant[conj.dta$hcip==1 & conj.dta$obamacare==1]<-1
table(conj.dta$relevant)
mean(conj.dta$relevant, na.rm=T) ###30.7% of observations got relevance treatment


######CHECK WHETHER NUMBER OF THE CHOICE TASK MATTERS 
###code indicator for just first choice
conj.dta$choice.1<-0
conj.dta$choice.1[conj.dta$choice.num==1]<-1
table(conj.dta$choice.1)


##test whether choice.1 responses are different with interaction
model<-ols(response~ relevant+unfriend.source+friend.source + choice.1 + relevant*choice.1 + unfriend.source*choice.1 + friend.source*choice.1 +female+smoker+senior+student+hcip+lose.weight , data=conj.dta,model=TRUE, x=TRUE, y=TRUE)
clust<-robcov(model, conj.dta[,"respid"])
model
clust 


#######effects over choice tasks
results.points<-matrix(nrow=3, ncol=12)
rownames(results.points)<-c("Topic Relevance","Politically Unfriendly Source","Politically Friendly Source")
colnames(results.points)<-seq(1,12,1)

results.ses<-matrix(nrow=3, ncol=12)
rownames(results.ses)<-c("Topic Relevance","Politically Unfriendly Source","Politically Friendly Source")
colnames(results.ses)<-seq(1,12,1)


results.dfs<-matrix(nrow=3, ncol=12)
rownames(results.dfs)<-c("Topic Relevance","Politically Unfriendly Source","Politically Friendly Source")
colnames(results.dfs)<-seq(1,12,1)

for(i in 1:len(unique(conj.dta$choice.num)) ){
model<-ols(response~ relevant+unfriend.source+friend.source+female+smoker+senior+student+hcip+lose.weight, data=conj.dta[conj.dta$choice.num==i,],model=TRUE, x=TRUE, y=TRUE)
clust<-robcov(model, conj.dta[conj.dta$choice.num==i,"respid"])
model2<-lm(response~ relevant+unfriend.source+friend.source +female+smoker+senior+student+hcip+lose.weight , data=conj.dta[conj.dta$choice.num==i,])
df<-model2$df.residual

results.points["Topic Relevance",i]<-as.n(clust$coefficients[2])
results.points["Politically Unfriendly Source",i]<-as.n(clust$coefficients[3])
results.points["Politically Friendly Source",i]<-as.n(clust$coefficients[4])

results.ses["Topic Relevance",i]<-as.n(clust$var[2,2])
results.ses["Politically Unfriendly Source",i]<-as.n(clust$var[3,3])
results.ses["Politically Friendly Source",i]<-as.n(clust$var[4,4])

results.dfs["Topic Relevance",i]<-df
results.dfs["Politically Unfriendly Source",i]<-df
results.dfs["Politically Friendly Source",i]<-df
	
	}


file.name<-paste(wd,"choice.num.pdf",sep="")
pdf(file.name, paper='special', height=6, width=6)

plot(seq(1,12,1), results.points["Topic Relevance",], type="l", lty=1, ylim=c(-.2,.25), frame.plot=FALSE, xlab="Choice Task Number",ylab="Estimated Effect on Selection", main="Treatment Effects by Choice Task Number", axes=FALSE)
points(seq(1,12,1), results.points["Politically Unfriendly Source",], type="l",lty=2)
points(seq(1,12,1), results.points["Politically Friendly Source",], type="l",lty=3)


axis(1, at=seq(1,12,1), labels=T, tick=T)
axis(2, at=seq(-.2,.3,.1), labels=T, tick=T, las=2)
legend("bottomright", c("Relevant Topic","Politically Unfriendly Source","Politically Friendly Source"),lty=c(1,2,3), cex=.7)

dev.off()









###################################################################
###################################################################
####### Post-Treatment Bias Check
###################################################################
###################################################################


demos<-c("female","smoker","senior","student","hcip","lose.weight","rep")
treat<-c("fox", "msnbc","usa", "equalpay","breast","abortion","socialsec","smoking","studentdebt","weight","christie","obamacare","celeb","nfl")

coefs<-list( rep(NA, len(treat)),  rep(NA, len(treat)), rep(NA, len(treat)),rep(NA, len(treat)),rep(NA, len(treat)), rep(NA, len(treat)), rep(NA, len(treat)) )
names(coefs)<-demos
for(i in 1:length(coefs)){names(coefs[[i]])<-treat}

ses<-list( rep(NA, len(treat)),  rep(NA, len(treat)), rep(NA, len(treat)),rep(NA, len(treat)),rep(NA, len(treat)), rep(NA, len(treat)), rep(NA, len(treat)))
names(ses)<-demos
for(i in 1:length(ses)){names(ses[[i]])<-treat}


names(coefs)<-demos
names(ses)<-demos


for (i in 1:len(demos)){
for (j in 1:len(treat)){

model<-ols(conj.dta[,demos[i]] ~ conj.dta[,treat[j]] , data=conj.dta, model=TRUE, x=TRUE, y=TRUE)
clust<-robcov(model, conj.dta$respid)
model2<-lm(conj.dta[,demos[i]] ~ conj.dta[,treat[j]] , data=conj.dta)



coefs[[i]][j]<-as.n(clust$coefficients)[2]


ses[[i]][j]<-as.n(sqrt(clust$var[2,2]))

}
}
coefs##coefficients
ses##standard errors

##whats the largest treatment effect on any variable?
for(i in 1:len(coefs)){print(max(abs(coefs[[i]])))}


sig<-list( rep(NA, len(treat)),  rep(NA, len(treat)), rep(NA, len(treat)),rep(NA, len(treat)),rep(NA, len(treat)), rep(NA, len(treat)), rep(NA, len(treat)) )
names(sig)<-demos
for(i in 1:length(sig)){names(sig[[i]])<-treat}

for(i in 1:len(demos)){
for(j in 1:len(treat)){
	
	lb<-coefs[[i]][j]-1.96*ses[[i]][j]
	ub<-coefs[[i]][j]+1.96*ses[[i]][j]
	
	##code as stat sig based on whether 95% CI crosses 0
	if(lb*ub>0){ sig[[i]][j]<-TRUE   }
	if(lb*ub<0){ sig[[i]][j]<-FALSE   }
	
	}}


sig

demos<-c("female","smoker","senior","student","hcip","lose.weight","dem")
treat<-c("fox", "msnbc","usa", "equalpay","breast","abortion","socialsec","smoking","studentdebt","weight","christie","obamacare","celeb","nfl")


##what portion of tests have stat sig results?
num.sig<-0
for(i in 1:len(demos)){
for(j in 1:len(treat)){

if (sig[[i]][j]==TRUE){num.sig<-num.sig+1}
	}}
	
	
num.sig
num.sig/(len(treat)*len(demos))

for(i in 1:length(sig)){
	
	print(coefs[[i]][sig[[i]]==T])
	print(ses[[i]][sig[[i]]==T])
	
}



###################################################################
###################################################################
####### TABLE A11: ALTERNATE CODING SCHEMES
###################################################################
###################################################################

##code alternate indicators
conj.dta$senior2<-0
conj.dta$senior2[conj.dta$age>60]<-1
table(conj.dta$senior2)

conj.dta$senior3<-0
conj.dta$senior3[conj.dta$age>65]<-1


conj.dta$student2<-I( conj.dta$BA==1 & conj.dta$age<=25)
conj.dta$student3<-I( conj.dta$BA==1 & conj.dta$age<=30)

##1 cig or more
conj.dta$smoker2<-I(conj.dta$smoker2==1)
##2-4 packs
conj.dta$smoker3<-I( conj.dta$smoker3==1)

##uninsured only
conj.dta$hcip2<-I(conj.dta$uninsured==1)
##health car workers only
conj.dta$hcip3<-I(conj.dta$hc.worker==1)


topics<-c(rep("socialsec",3), rep("studentdebt",3), rep("smoking",3), rep("obamacare",3))
groups<-c("senior","senior2","senior3","student","student2","student3","smoker","smoker2","smoker3","hcip","hcip2","hcip3")
group.names<-c("Over 55","Over 60","Over 65","Current Students","B.A./<=25","B.A./<=30", ">=1Pack",">=1Cig","2-4Packs","HCIP","UN","HC")
covars<-c("relevant","friend.source","unfriend.source")
coef.names<-c("Intercept","Relevant Topic","Friendly Source","Unfriendly Source")


out<-reg.results(data=conj.dta, dv="response",covars=covars, groups=groups, group.names=group.names, topics=topics, interaction=F, coef.names=coef.names)

xtable(out[[1]])



###################################################################
###################################################################
####### TABLE A12: M-TURK REPLICATION
###################################################################
###################################################################
rm(list="conj.dta")##remove SSI data


##if data is in .Rdata format
file.name<-paste(wd, "se.conjoint.turk2.Rdata", sep="")
load(file.name)
dim(conj.dta)


##if data is in .tab format
# file.name<-paste(wd, "se.conjoint.turk2.tab", sep="")
# conj.dta<-read.table(file.name, header=T, sep="\t")
# dim(conj.dta)


##load custom functions
source("reg.results.function.R")
source("pred.function.R")


topics<-c("fem","smoking","socialsec","studentdebt","obamacare","weight")
groups<-c("female","smoker","senior","student","hcip","lose.weight")
group.names<-c("Women","Smokers","Seniors","Students","Uninsured/Health Care Workers","Trying to Lose Weight")
covars<-c("relevant","friend.source","unfriend.source")
coef.names<-c("Intercept","Relevant Topic","Friendly Source","Unfriendly Source")




out<-reg.results(data=conj.dta, dv="response",covars=covars, groups=groups, group.names=group.names, topics=topics, interaction=F, coef.names=coef.names)
out

##print table for Latex
xtable(out[[1]])


########################
########################
########################
###Table A13: Interaction Models
########################
########################
########################

topics<-c("fem","smoking","socialsec","studentdebt","obamacare","weight")
groups<-c("female","smoker","senior","student","hcip","lose.weight")
group.names<-c("Women","Smokers","Seniors","Students","Uninsured/Health Care Workers","Trying to Lose Weight")
covars<-c("relevant","friend.source","unfriend.source")
coef.names<-c("Intercept","Relevant Topic","Friendly Source","Unfriendly Source","Relevant * Friendly","Relevant * Unfriendly")


out<-reg.results(data=conj.dta, dv="response",covars=covars, groups=groups, group.names=group.names, topics=topics, interaction=T, coef.names=coef.names)
out

library(xtable)
xtable(out[[1]])


########################
########################
########################
###Figure A9: Mean rates of consumption
########################
########################
########################


results.ols<-pred(data=conj.dta,  groups=groups, topics=topics, model.type=c("ols"))


est<-NA
lb<-NA
ub<-NA

for(i in 1:nrow(results.ols[[1]])){



temp1<-NA
temp2<-NA
temp3<-NA

for(j in 1:length(results.ols)){

	
	temp1[j]<-results.ols[[j]]$est[i]
	temp2[j]<- results.ols[[j]]$lb[i]
	temp3[j]<- results.ols[[j]]$ub[i]
	
	
}

est<-c(est, temp1)
lb<-c(lb, temp2)
ub<-c(ub, temp3)

}


est <- est[!is.na(est)]
lb <- lb[!is.na(lb)]
ub <- ub[!is.na(ub)]
shape <- rep(c(0,1,2,4,5,6),6)
type<-c(rep("irrel",length(est)/2), rep("rel",length(est)/2) )
col<-I(est>=.5)
col[col==T]<-"black"
col[col==F]<-"grey"


res<-cbind.data.frame(est=est, lb=lb, ub=ub, shape=shape, col=col, type=type)
res$source.type<-NA
res$source.type<-rep(c(rep("1unfriend",6),rep("2neutral",6), rep("3friend",6)),2)

res$shape2<-NA
res$shape2[res$type=="irrel"]<-17
res$shape2[res$type=="rel"]<-19

res$col2<-NA
res$col2[res$type=="irrel"]<-"grey"
res$col2[res$type=="rel"]<-"black"


res<-res[order(res$shape,  res$source.type, res$type),]
res

y.axis<-length(res$est):1


pdf(paste(wd,"pred.probs2.turk.pdf",sep=""), width=, height=)

par(mar=c(4,6,4,6))
plot(res$est, length(res$est):1, pch=19, axes=F, xlim=c(min(res$lb), max(res$ub)), xlab="Probability of Selection", col=res$col2, ylab="", main="Relevant Topics Are Selected Often, \n Regardless of Source Type")
abline(v=.5, lty=2)

segments(res$lb,length(res$est):1, res$ub, length(res$est):1, col=res$col2)
axis(1, at=seq(-1,1, by=.1))
axis(4, at=seq(max(y.axis)-.5, 1.5, by=-2), labels=rep(c("Unfriendly Source","Neutral Source","Friendly Source"), 6), las=2, tck=-.02, cex.axis=.7)
axis(4, at=c((max(y.axis)+1):-5), tck=0, labels=F)

#axis(4, at=-5:50, labels=F, tck=0)  
axis(2, at=-5:40, labels=F, tck=0)
abline(h=seq(max(y.axis)-1.5, 2.5, by=-2), col="grey")

#abline(h=(length(res$est)/2)+.5, lty=1)
axis(2, at=seq(3.5, 33.5, by=6), labels=rev(c("Women","Smokers","Seniors","Students","Uninsured/
HC Worker","Trying to 
Lose Weight")) , las=2) 
legend("topright",pch=c(19,19,NA, NA), legend=c("Irrelevant Topic","Relevant Topic"), cex=.98, bg="white", col=c("grey","black") , lty=c(rep(1,2)) , lwd=c(rep(1,2)))

abline(h=seq(6.5, 30.5, by=6), col="black")

points(res$est, length(res$est):1, pch=19, col=res$col2)

dev.off()


















