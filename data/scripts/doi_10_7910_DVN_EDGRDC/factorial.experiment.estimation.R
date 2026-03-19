###################################################
###################################################
###################################################
###################################################
###################################################
###################################################
####### COMMUNITY PREFERENCE FACTORIAL DESIGN
###################################################
###################################################
###################################################
###################################################
###################################################
###################################################

cat("Begin COMMUNITY PREFERENCE FACTORIAL DESIGN\n")


load("dd.final.Rdata")
dd<-dd.final
dim(dd)


## Community preference design
####
####
###COMM PREF items: create single response vector (0's and 1's) and accompanying treatment factor vector
#####
####

as.n<-as.numeric
as.c<-as.character

table(dd$comm.pref.arm)
## 1st response variable, dichotomous

##test to see everyone only got 1 of 9 conditions
dd$comm.resp.test<-paste(dd$rp.will.prefa1,dd$rp.will.prefa2,dd$rp.will.prefa3, dd$rp.will.prefa4,dd$rp.will.prefa5,dd$rp.will.prefa6,dd$rp.will.prefa7,dd$rp.will.prefa8,dd$rp.will.prefa9, sep="")

table(dd$comm.resp.test)
dd$comm.resp<-as.n(as.c(dd$comm.resp.test))
table(dd$comm.resp)

##second answer to comm pref items
dd$comm.resp2.test<-paste(dd$rp.attr.prefa1,dd$rp.attr.prefa2,dd$rp.attr.prefa3, dd$rp.attr.prefa4,dd$rp.attr.prefa5,dd$rp.attr.prefa6,dd$rp.attr.prefa7,dd$rp.attr.prefa8,dd$rp.attr.prefa9, sep="")

table(dd$comm.resp2.test) 

dd$comm.resp2<-as.n(as.c(dd$comm.resp2.test))


#####
#####
### MEANS for Comm. Pref. 
#####
#####

as.n<-as.numeric
as.c<-as.character
len<-length

###
comm.treat.arms<-c("Comm Pref A1","Comm Pref A2","Comm Pref A3","Comm Pref A4","Comm Pref A5","Comm Pref A6","Comm Pref A7","Comm Pref A8","Comm Pref A9")
comm.treat.arms2<-c("Control/Control","Control/Dem","Control/Rep","White/Rep","White/Control","White/Dem","Minority/Rep","Minority/Control","Minority/Dem")

comm.results.combined<-list(NA)

comm.results.data<-list(all=dd, dems=subset(dd, dd$partywlean=="dem"), reps=subset(dd, dd$partywlean=="rep"))

##COMM PREF ITEMS - whole sample
for (j in 1:3){
t.mean<-NA
t.lb<-NA
t.ub<-NA
t.stat<-NA
t2.mean<-NA
t2.lb<-NA
t2.ub<-NA
t2.stat<-NA
for (i in 1:len(comm.treat.arms)){
	t1<-t.test((comm.results.data[[j]]$comm.resp[comm.results.data[[j]]$comm.pref.arm==comm.treat.arms[i]]), na.rm=TRUE )
	t2<-t.test((comm.results.data[[j]]$comm.resp2[comm.results.data[[j]]$comm.pref.arm==comm.treat.arms[i]]), na.rm=TRUE )
	t.mean[i]<-as.n(t1$estimate)
    t.lb[i] <-t1$conf.int[1]
	t.ub[i] <-t1$conf.int[2]
    t.stat[i] <-as.n(t1$statistic)
    t2.mean[i]<-as.n(t2$estimate)
    t2.lb[i] <-t2$conf.int[1]
	t2.ub[i] <-t2$conf.int[2]
    t2.stat[i] <-as.n(t2$statistic)
    treat.index<-c(1:9)
    }
if(j==1){
	label<-rep("All",9)
	all.1<-cbind.data.frame(label, treat.index,comm.treat.arms2, t.mean, t.lb, t.ub, t2.mean, t2.lb, t2.ub)
	colnames(all.1)<-c("Sample","Treat Index","TreatArm","Mean1", "LB95", "UB95","Mean 2", "LB95.2", "UB95.2" )
		comm.results.combined[[j]]<-all}
if(j==2){label<-rep("Dem",9)
	dems<-cbind.data.frame(label, treat.index,comm.treat.arms2, t.mean, t.lb, t.ub, t2.mean, t2.lb, t2.ub)
	colnames(dems)<-c("Sample","Treat Index","TreatArm","Mean1", "LB95", "UB95","Mean 2", "LB95.2", "UB95.2" ) 
	comm.results.combined[[j]]<-dems}
if(j==3){label<-rep("Rep",9)
	reps<-cbind.data.frame(label, treat.index,comm.treat.arms2, t.mean, t.lb, t.ub, t2.mean, t2.lb, t2.ub)
	colnames(reps)<-c("Sample","Treat Index","TreatArm","Mean1", "LB95", "UB95","Mean 2", "LB95.2", "UB95.2")  
	comm.results.combined[[j]]<-reps}
	}

##save the sorted table 
file.name<-paste(paste(output, "comm.pref.results.Rdata",sep=""))
save(comm.results.combined, file=file.name,row.names=T)



##order results for graphing
comm.results.combined[[2]]$sort.num<-seq(2,2*len(comm.results.combined[[2]][,1]), by=2)
comm.results.combined[[3]]$sort.num<-seq(1,2*len(comm.results.combined[[2]][,1])-1, by=2)
graph.data<-rbind.data.frame(comm.results.combined[[2]], comm.results.combined[[3]])
graph.data<-graph.data[order(graph.data$sort.num), ]
graph.data

##
##NO party info
##
graph.data$new.order<-NA
graph.data$new.order[graph.data$TreatArm=="Control/Control" & graph.data$Sample=="Rep"]<-1
graph.data$new.order[graph.data$TreatArm=="Control/Control" & graph.data$Sample=="Dem"]<-2
graph.data$new.order[graph.data$TreatArm=="Minority/Control" & graph.data$Sample=="Rep"]<-3
graph.data$new.order[graph.data$TreatArm=="Minority/Control" & graph.data$Sample=="Dem"]<-4
graph.data$new.order[graph.data$TreatArm=="White/Control" & graph.data$Sample=="Rep"]<-5
graph.data$new.order[graph.data$TreatArm=="White/Control" & graph.data$Sample=="Dem"]<-6

###
##OWN PARTY
###
##no race/own party
graph.data$new.order[graph.data$TreatArm=="Control/Rep" & graph.data$Sample=="Rep"]<-7
graph.data$new.order[graph.data$TreatArm=="Control/Dem" & graph.data$Sample=="Dem"]<-8
##70% white/own party
graph.data$new.order[graph.data$TreatArm=="Minority/Rep" & graph.data$Sample=="Rep"]<-9
graph.data$new.order[graph.data$TreatArm=="Minority/Dem" & graph.data$Sample=="Dem"]<-10
##96% white/own party
graph.data$new.order[graph.data$TreatArm=="White/Rep" & graph.data$Sample=="Rep"]<-11
graph.data$new.order[graph.data$TreatArm=="White/Dem" & graph.data$Sample=="Dem"]<-12

###
## OUT PARTY
##

#none/out party
graph.data$new.order[graph.data$TreatArm=="Control/Dem" & graph.data$Sample=="Rep"]<-13
graph.data$new.order[graph.data$TreatArm=="Control/Rep" & graph.data$Sample=="Dem"]<-14
##minority/out party
graph.data$new.order[graph.data$TreatArm=="Minority/Dem" & graph.data$Sample=="Rep"]<-15
graph.data$new.order[graph.data$TreatArm=="Minority/Rep" & graph.data$Sample=="Dem"]<-16
#white/out-party
graph.data$new.order[graph.data$TreatArm=="White/Dem" & graph.data$Sample=="Rep"]<-17
graph.data$new.order[graph.data$TreatArm=="White/Rep" & graph.data$Sample=="Dem"]<-18

graph.data

graph.data<-graph.data[order(graph.data$new.order), ]
comm.graph.data<-graph.data

file.name<-paste(output, "comm.graph.data.Rdata",sep="")
save(comm.graph.data, file=file.name,row.names=T)



file.name<-paste(output, "comm.graph.data.Rdata",sep="")

load(file.name)

##display the data that is to be plotted
comm.graph.data

graph.data<-comm.graph.data

##make diff shapes for dems and reps
graph.data$points<-NA
graph.data$points[graph.data$Sample=="Rep"]<-19
graph.data$points[graph.data$Sample=="Dem"]<-17



##make graphical parameters
coef.vec<-as.n(as.c(graph.data[,"Mean1"]))
lb.vec<- as.n(as.c(graph.data[,"LB95"]))
ub.vec<- as.n(as.c(graph.data[,"UB95"]))
#y.axis <- c(length(coef.vec):1)
y.1<-seq(length(coef.vec)/2, 1, -1)
y.2<-seq((length(coef.vec)/2)+.25, 1.25,-1)
y.axis<-c(y.1, y.2)
y.axis<-y.axis[order(y.axis, decreasing=TRUE)] 
y.axis      

var.names<-c("No Race Info" , "70% White","96% White" ,"No Race Info" ,"70% White" ,"96% White"  ,"No Race Info",     "70% White", "96% White")

file.name<-paste(output,"comm1.pdf",sep="")


##calculate the mean response for dems and reps (combined)respondents in the control condition

mean.control<-((graph.data$Mean1[graph.data$TreatArm=="Control/Control" &graph.data$Sample=="Rep" ]) + (graph.data$Mean1[graph.data$TreatArm=="Control/Control" &graph.data$Sample=="Dem" ]))/2

hard.divides<-c(6.575,3.575) 
gray.divides<-seq(2.575,8.575,by=1)
y.ticks<-seq(1.075,9.075,by=1)  

##make the commpref figure, saving as a pdf
pdf(file=file.name, height=4.5)
par(mar=c(5, 10, 5,5))
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Mean Willingness to Move to Community (0-1)", ylab = "", pch = graph.data$points, cex = .7, xlim = c(min(lb.vec),max(ub.vec)),   main = "Partisan Composition Strongly Affects Evaluations
of Desirable Communities", col=c("red","blue") )
segments(lb.vec, y.axis, ub.vec, y.axis, lwd =  .7, col=c("red","blue"))
segments(mean.control,0,mean.control, 18, lwd =  .7, lty=2)
axis(1, at = seq(0,1,.1), labels = T, tick = T, tck=-0.03, cex.axis = .8, mgp = c(2,.7,0)) 
axis(2, at = seq(18,0,-1), label =F, las = 1, tick = T, mgp = c(2,1,0),tck=-0, cex.axis = .8) 
axis(2, at =hard.divides, label =F, las = 1, tick = T, mgp = c(2,1,0),tck=-.4, cex.axis = .8)
axis(2, at =y.ticks, label =F, las = 1, tick = T, mgp = c(2,1,0),tck=-.01, cex.axis = .8) 
abline(h=gray.divides, lty=1, lwd=1, col="gray")
segments(0,1.575,.883, 1.575, lwd =  1, lty=1, col="gray")

abline(h=hard.divides, lty=1, lwd=1, col="black")
mtext(var.names,side=2, at=seq(9.075,1.075,by=-1),line=.2, las=2, cex=.9)
mtext(c("No  
Party 
Info &: ","70% 
Own 
Party &: ","70% 
Other 
Party &: "),side=2, at=c((max(y.axis)+hard.divides[1])/2, (hard.divides[1]+hard.divides[2])/2, hard.divides[2]/2), las=2,line=5.5, cex=.9)
legend("bottomright", legend=c("Republicans","Democrats"), cex=.6, col=c("red","blue"), pch=graph.data$points)

dev.off()



cat("End COMMUNITY PREFERENCE FACTORIAL DESIGN\n")
