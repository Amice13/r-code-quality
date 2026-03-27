
library(RItools)
library(mediation)
survey <- read.delim(file="Master data.txt", , sep = ",", header=T)
survey2<- as.data.frame(subset(survey, gov_statement==0))

#Manuscript Figure 1

argentina<-subset(survey2,argentina==1)
india<-subset(survey2,india==1)
israel<-subset(survey2,israel==1)

results<-matrix(ncol=3,nrow=3)
x<-t.test(india$approve~india$opp_viol)
results[1,1]<-x$estimate[1]-x$estimate[2]
results[1,2]<-x$conf.int[1]
results[1,3]<-x$conf.int[2]
x<-t.test(israel$approve~israel$opp_viol)
results[2,1]<-x$estimate[1]-x$estimate[2]
results[2,2]<-x$conf.int[1]
results[2,3]<-x$conf.int[2]
x<-t.test(argentina$approve~argentina$opp_viol)
results[3,1]<-x$estimate[1]-x$estimate[2]
results[3,2]<-x$conf.int[1]
results[3,3]<-x$conf.int[2]
results<-results*-100

modellabels1<-c("India","Israel","Argentina")

y<-c(3,2,1)

plotname<-paste("figure1.jpg")
jpeg(filename = plotname, width = 500, height = 350,
     units = "px", pointsize = 12, quality = 100, bg = "white",
     res = NA, restoreConsole = TRUE)

par(mai=c(1.5,1.5,1,0.5),oma=c(0,0,0,0),cex=1.4,cex.axis=1,mgp=c(1,1,0))

plot(results[,1],y,ylab="",yaxt="n",xlab="",
     xlim=c(-20,20),cex=1,pch=16)
lines(c(0,0),c(0,dim(results)[2]+1),lty="dotted")
for(i in 1:3) lines(results[i,2:3],c(y[i],y[i]))
axis(2,labels=modellabels1, at=c(3,2,1),las=1,font=1,tick=FALSE,ps=5,cex.axis=1.2)
mtext("% Change in Government Approval",side=1,line=2.2,font=1,cex=1.5)
mtext("95% Confidence Intervals",side=1,line=3.5,font=1,cex=1.2)
mtext("Opposition Violence",side=3,line=1.25,font=1,cex=1.5)

dev.off()

#Manuscript Figure 2

argentina<-subset(survey2,argentina==1)
india<-subset(survey2,india==1)
israel<-subset(survey2,israel==1)

results<-matrix(ncol=3,nrow=3)
x<-t.test(india$approve~india$gov_viol)
results[1,1]<-x$estimate[1]-x$estimate[2]
results[1,2]<-x$conf.int[1]
results[1,3]<-x$conf.int[2]
x<-t.test(israel$approve~israel$gov_viol)
results[2,1]<-x$estimate[1]-x$estimate[2]
results[2,2]<-x$conf.int[1]
results[2,3]<-x$conf.int[2]
x<-t.test(argentina$approve~argentina$gov_viol)
results[3,1]<-x$estimate[1]-x$estimate[2]
results[3,2]<-x$conf.int[1]
results[3,3]<-x$conf.int[2]
results<-results*-100


modellabels1<-c("India","Israel","Argentina")

y<-c(3,2,1)

plotname<-paste("figure2.jpg")
jpeg(filename = plotname, width = 500, height = 350,
     units = "px", pointsize = 12, quality = 100, bg = "white",
     res = NA, restoreConsole = TRUE)

par(mai=c(1.5,1.5,1,0.5),oma=c(0,0,0,0),cex=1.4,cex.axis=1,mgp=c(1,1,0))

plot(results[,1],y,ylab="",yaxt="n",xlab="",
     xlim=c(-20,20),cex=1,pch=16)
lines(c(0,0),c(0,dim(results)[2]+1),lty="dotted")
for(i in 1:3) lines(results[i,2:3],c(y[i],y[i]))
axis(2,labels=modellabels1, at=c(3,2,1),las=1,font=1,tick=FALSE,ps=5,cex.axis=1.2)
mtext("% Change in Government Approval",side=1,line=2.2,font=1,cex=1.5)
mtext("95% Confidence Intervals",side=1,line=3.5,font=1,cex=1.2)
mtext("Government Violence",side=3,line=1.25,font=1,cex=1.5)

dev.off()

#Manuscript Figure 3

argentina<-subset(survey2,argentina==1)
india<-subset(survey2,india==1)
israel<-subset(survey2,israel==1)

results<-matrix(ncol=3,nrow=3)
x<-t.test(india$approve~india$treaty)
results[1,1]<-x$estimate[1]-x$estimate[2]
results[1,2]<-x$conf.int[1]
results[1,3]<-x$conf.int[2]
x<-t.test(israel$approve~israel$treaty)
results[2,1]<-x$estimate[1]-x$estimate[2]
results[2,2]<-x$conf.int[1]
results[2,3]<-x$conf.int[2]
x<-t.test(argentina$approve~argentina$treaty)
results[3,1]<-x$estimate[1]-x$estimate[2]
results[3,2]<-x$conf.int[1]
results[3,3]<-x$conf.int[2]
results<-results*-100


modellabels1<-c("India","Israel","Argentina")

y<-c(3,2,1)

plotname<-paste("figure3.jpg")
jpeg(filename = plotname, width = 500, height = 350,
     units = "px", pointsize = 12, quality = 100, bg = "white",
     res = NA, restoreConsole = TRUE)

par(mai=c(1.5,1.5,1,0.5),oma=c(0,0,0,0),cex=1.4,cex.axis=1,mgp=c(1,1,0))

plot(results[,1],y,ylab="",yaxt="n",xlab="",
     xlim=c(-20,20),cex=1,pch=16)
lines(c(0,0),c(0,dim(results)[2]+1),lty="dotted")
for(i in 1:3) lines(results[i,2:3],c(y[i],y[i]))
axis(2,labels=modellabels1, at=c(3,2,1),las=1,font=1,tick=FALSE,ps=5,cex.axis=1.2)
mtext("% Change in Government Approval",side=1,line=2.2,font=1,cex=1.5)
mtext("95% Confidence Intervals",side=1,line=3.5,font=1,cex=1.2)
mtext("International Law",side=3,line=1.25,font=1,cex=1.5)

dev.off()

#Manuscript Figure 4

india_nv<-subset(survey2,india==1& opp_viol==0)
india_v<-subset(survey2,india==1& opp_viol==1)
israel_nv<-subset(survey2,israel==1& opp_viol==0)
israel_v<-subset(survey2,israel==1& opp_viol==1)
argentina_nv<-subset(survey2,argentina==1 & opp_viol==0)
argentina_v<-subset(survey2,argentina==1 & opp_viol==1)

results<-matrix(ncol=3,nrow=6)
x<-t.test(india_nv$approve~india_nv$gov_viol)
results[1,1]<-x$estimate[1]-x$estimate[2]
results[1,2]<-x$conf.int[1]
results[1,3]<-x$conf.int[2]
x<-t.test(india_v$approve~india_v$gov_viol)
results[2,1]<-x$estimate[1]-x$estimate[2]
results[2,2]<-x$conf.int[1]
results[2,3]<-x$conf.int[2]
x<-t.test(israel_nv$approve~israel_nv$gov_viol)
results[3,1]<-x$estimate[1]-x$estimate[2]
results[3,2]<-x$conf.int[1]
results[3,3]<-x$conf.int[2]
x<-t.test(israel_v$approve~israel_v$gov_viol)
results[4,1]<-x$estimate[1]-x$estimate[2]
results[4,2]<-x$conf.int[1]
results[4,3]<-x$conf.int[2]
x<-t.test(argentina_nv$approve~argentina_nv$gov_viol)
results[5,1]<-x$estimate[1]-x$estimate[2]
results[5,2]<-x$conf.int[1]
results[5,3]<-x$conf.int[2]
x<-t.test(argentina_v$approve~argentina_v$gov_viol)
results[6,1]<-x$estimate[1]-x$estimate[2]
results[6,2]<-x$conf.int[1]
results[6,3]<-x$conf.int[2]
results<-results*-100

modellabels1<-c("Opposition Non-violent","Opposition Violent")
y<-c(2,1)

plotname<-paste("figure4.jpg")
jpeg(filename = plotname, width = 800, height = 300,
     units = "px", pointsize = 12, quality = 100, bg = "white",
     res = NA, restoreConsole = TRUE)

par(mai=c(1.5,0.5,0.5,0.5),oma=c(0,12,0,0),cex=1,cex.axis=1,mgp=c(1,1,0))

par(mfrow=c(1,3))

plot(results[1:2,1],y,ylab="",yaxt="n",xlab="",
     xlim=c(-20,20),cex=1,pch=16)
lines(c(0,0),c(0,dim(results)[2]+1),lty="dotted")
for(i in 1:2) lines(results[i,2:3],c(y[i],y[i]))
axis(2,labels=modellabels1, at=c(2,1),las=1,font=1,tick=FALSE,ps=5,cex.axis=1.5)
mtext("% Change in Government Approval",side=1,line=3,font=1,cex=1)
mtext("95% Confidence Intervals",side=1,line=4.5,font=1,cex=1)
mtext("India",side=3,line=.9,font=1,cex=1.2)

plot(results[3:4,1],y,ylab="",yaxt="n",xlab="",
     xlim=c(-20,20),cex=1,pch=16)
lines(c(0,0),c(0,dim(results)[2]+1),lty="dotted")
for(i in 3:4) lines(results[i,2:3],c(y[i-2],y[i-2]))
mtext("% Change in Government Approval",side=1,line=3,font=1,cex=1)
mtext("95% Confidence Intervals",side=1,line=4.5,font=1,cex=1)
mtext("Israel",side=3,line=.9,font=1,cex=1.2)

plot(results[5:6,1],y,ylab="",yaxt="n",xlab="",
     xlim=c(-20,20),cex=1,pch=16)
lines(c(0,0),c(0,dim(results)[2]+1),lty="dotted")
for(i in 5:6) lines(results[i,2:3],c(y[i-4],y[i-4]))
mtext("% Change in Government Approval",side=1,line=3,font=1,cex=1)
mtext("95% Confidence Intervals",side=1,line=4.5,font=1,cex=1)
mtext("Argentina",side=3,line=.9,font=1,cex=1.2)
dev.off()

#Manuscript Figure 5


india_nv_nv<-subset(survey2,india==1& opp_viol==0 &gov_viol==0)
india_nv_v<-subset(survey2,india==1& opp_viol==0&gov_viol==1)
india_v_nv<-subset(survey2,india==1& opp_viol==1 &gov_viol==0)
india_v_v<-subset(survey2,india==1& opp_viol==1&gov_viol==1)

israel_nv_nv<-subset(survey2,israel==1& opp_viol==0 &gov_viol==0)
israel_nv_v<-subset(survey2,israel==1& opp_viol==0&gov_viol==1)
israel_v_nv<-subset(survey2,israel==1& opp_viol==1 &gov_viol==0)
israel_v_v<-subset(survey2,israel==1& opp_viol==1&gov_viol==1)

argentina_nv_nv<-subset(survey2,argentina==1& opp_viol==0 &gov_viol==0)
argentina_nv_v<-subset(survey2,argentina==1& opp_viol==0&gov_viol==1)
argentina_v_nv<-subset(survey2,argentina==1& opp_viol==1 &gov_viol==0)
argentina_v_v<-subset(survey2,argentina==1& opp_viol==1&gov_viol==1)

results<-matrix(ncol=3,nrow=12)
x<-t.test(india_nv_nv$approve~india_nv_nv$treaty)
results[1,1]<-x$estimate[1]-x$estimate[2]
results[1,2]<-x$conf.int[1]
results[1,3]<-x$conf.int[2]
x<-t.test(india_nv_v$approve~india_nv_v$treaty)
results[2,1]<-x$estimate[1]-x$estimate[2]
results[2,2]<-x$conf.int[1]
results[2,3]<-x$conf.int[2]
x<-t.test(india_v_nv$approve~india_v_nv$treaty)
results[3,1]<-x$estimate[1]-x$estimate[2]
results[3,2]<-x$conf.int[1]
results[3,3]<-x$conf.int[2]
x<-t.test(india_v_v$approve~india_v_v$treaty)
results[4,1]<-x$estimate[1]-x$estimate[2]
results[4,2]<-x$conf.int[1]
results[4,3]<-x$conf.int[2]
x<-t.test(israel_nv_nv$approve~israel_nv_nv$treaty)
results[5,1]<-x$estimate[1]-x$estimate[2]
results[5,2]<-x$conf.int[1]
results[5,3]<-x$conf.int[2]
x<-t.test(israel_nv_v$approve~israel_nv_v$treaty)
results[6,1]<-x$estimate[1]-x$estimate[2]
results[6,2]<-x$conf.int[1]
results[6,3]<-x$conf.int[2]
x<-t.test(israel_v_nv$approve~israel_v_nv$treaty)
results[7,1]<-x$estimate[1]-x$estimate[2]
results[7,2]<-x$conf.int[1]
results[7,3]<-x$conf.int[2]
x<-t.test(israel_v_v$approve~israel_v_v$treaty)
results[8,1]<-x$estimate[1]-x$estimate[2]
results[8,2]<-x$conf.int[1]
results[8,3]<-x$conf.int[2]
x<-t.test(argentina_nv_nv$approve~argentina_nv_nv$treaty)
results[9,1]<-x$estimate[1]-x$estimate[2]
results[9,2]<-x$conf.int[1]
results[9,3]<-x$conf.int[2]
x<-t.test(argentina_nv_v$approve~argentina_nv_v$treaty)
results[10,1]<-x$estimate[1]-x$estimate[2]
results[10,2]<-x$conf.int[1]
results[10,3]<-x$conf.int[2]
x<-t.test(argentina_v_nv$approve~argentina_v_nv$treaty)
results[11,1]<-x$estimate[1]-x$estimate[2]
results[11,2]<-x$conf.int[1]
results[11,3]<-x$conf.int[2]
x<-t.test(argentina_v_v$approve~argentina_v_v$treaty)
results[12,1]<-x$estimate[1]-x$estimate[2]
results[12,2]<-x$conf.int[1]
results[12,3]<-x$conf.int[2]
results<-results*-100

modellabels1<-c("Both Non-Violent","Opp. Non-Violent/Gov. Violent","Opp. Violent/Gov. Non-Violence","Both Violent")

y<-c(4,3,2,1)

plotname<-paste("figure5.jpg")
jpeg(filename = plotname, width = 800, height = 400,
     units = "px", pointsize = 12, quality = 100, bg = "white",
     res = NA, restoreConsole = TRUE)

par(mai=c(1.5,0.5,0.5,0.5),oma=c(0,18,0,0),cex=1,cex.axis=1,mgp=c(1,1,0))

par(mfrow=c(1,3))

plot(results[1:4,1],y,ylab="",yaxt="n",xlab="",
     xlim=c(-20,20),cex=1.8,pch=16)
lines(c(0,0),c(0,dim(results)[2]+2),lty="dotted")
for(i in 1:4) lines(results[i,2:3],c(y[i],y[i]),lwd=2)
axis(2,labels=modellabels1, at=c(4,3,2,1),las=1,font=1,tick=FALSE,ps=5,cex.axis=1.5)
mtext("India",side=3,line=2,font=1)
mtext("% Change in Government Approval",side=1,line=3,font=1,cex=1)
mtext("95% Confidence Intervals",side=1,line=4.5,font=1,cex=1)

plot(results[5:8,1],y,ylab="",yaxt="n",xlab="",
     xlim=c(-20,20),cex=1.8,pch=16)
lines(c(0,0),c(0,dim(results)[2]+2),lty="dotted")
for(i in 5:8) lines(results[i,2:3],c(y[i-4],y[i-4]),lwd=2)
mtext("Israel",side=3,line=2,font=1)
mtext("% Change in Government Approval",side=1,line=3,font=1,cex=1)
mtext("95% Confidence Intervals",side=1,line=4.5,font=1,cex=1)

plot(results[9:12,1],y,ylab="",yaxt="n",xlab="",
     xlim=c(-20,20),cex=1.8,pch=16)
lines(c(0,0),c(0,dim(results)[2]+2),lty="dotted")
for(i in 9:12) lines(results[i,2:3],c(y[i-8],y[i-8]),lwd=2)
mtext("Argentina",side=3,line=2,font=1)
mtext("% Change in Government Approval",side=1,line=3,font=1,cex=1)
mtext("95% Confidence Intervals",side=1,line=4.5,font=1,cex=1)
dev.off()

#Appendix Figure 1

argentina<-subset(survey2,argentina==1)
india<-subset(survey2,india==1)
israel<-subset(survey2,israel==1)

male<-argentina$male
counts <- table(male)
percentage_arg<-counts/length(male)*100

male<-india$male
counts <- table(male)
percentage_ind<-counts/length(male)*100

male<-israel$male
counts <- table(male)
percentage_isr<-counts/length(male)*100


data<-c(percentage_ind[2],percentage_isr[2],percentage_arg[2])

plotname<-paste("figureA1.jpg")
jpeg(filename = plotname, width = 750, height = 350,
     units = "px", pointsize = 12, quality = 100, bg = "white",
     res = NA, restoreConsole = TRUE)

barplot(data, main="",
  	ylab="Percentage of Male Respondents",names.arg=c("India","Israel","Argentina"))

dev.off()

#Appendix Figure 2

india<-subset(survey2,india==1)

age<-india$agecat
counts <- table(age)
percentage<-counts/length(age)*100


plotname<-paste("figureA2.jpg")
jpeg(filename = plotname, width = 750, height = 350,
     units = "px", pointsize = 12, quality = 100, bg = "white",
     res = NA, restoreConsole = TRUE)

barplot(percentage, main="",
  	ylab="Percentage of Respondents",names.arg=c("18-24","25-34","35-44","45-54","55-64","65+"))

dev.off()


#Appendix Figure 3

israel<-subset(survey2,israel==1)

age<-israel$agecat
counts <- table(age)
percentage<-counts/length(age)*100


plotname<-paste("figureA3.jpg")
jpeg(filename = plotname, width = 750, height = 350,
     units = "px", pointsize = 12, quality = 100, bg = "white",
     res = NA, restoreConsole = TRUE)

barplot(percentage, main="",
  	ylab="Percentage of Respondents",names.arg=c("18-24","25-34","35-44","45-54","55-64"))

dev.off()

#Appendix Figure 4

argentina<-subset(survey2,argentina==1)

age<-argentina$agecat
counts <- table(age)
percentage<-counts/length(age)*100


plotname<-paste("figureA4.jpg")
jpeg(filename = plotname, width = 750, height = 350,
     units = "px", pointsize = 12, quality = 100, bg = "white",
     res = NA, restoreConsole = TRUE)

barplot(percentage, main="",
  	ylab="Percentage of Respondents",names.arg=c("18-24","25-34","35-44","45-54","55-64","65+"))

dev.off()

#Appendix Figure 5

india<-subset(survey2,india==1)
id<-india$ideology
counts <- table(id)
percentage<-counts/length(id)*100


plotname<-paste("figureA5.jpg")
jpeg(filename = plotname, width = 800, height = 350,
     units = "px", pointsize = 12, quality = 100, bg = "white",
     res = NA, restoreConsole = TRUE)

barplot(percentage, main="",
  	ylab="Percentage of Respondents",names.arg=c("Extremely Liberal","Liberal","Slightly Liberal","Moderate","Slightly Cons.","Conservative","Extremely Cons."))
dev.off()

#Appendix Figure 6

israel<-subset(survey2,israel==1)
id<-israel$ideology
counts <- table(id)
percentage<-counts/length(id)*100


plotname<-paste("figureA6.jpg")
jpeg(filename = plotname, width = 800, height = 350,
     units = "px", pointsize = 12, quality = 100, bg = "white",
     res = NA, restoreConsole = TRUE)

barplot(percentage, main="",
  	ylab="Percentage of Respondents",names.arg=c("Extremely Liberal","Liberal","Slightly Liberal","Moderate","Slightly Cons.","Conservative","Extremely Cons."))
dev.off()

#Appendix Figure 7

india<-subset(survey2,india==1)

ed<-india$educat
counts <- table(ed)
percentage<-counts/length(ed)*100


plotname<-paste("figureA7.jpg")
jpeg(filename = plotname, width = 750, height = 350,
     units = "px", pointsize = 12, quality = 100, bg = "white",
     res = NA, restoreConsole = TRUE)

barplot(percentage, main="",
  	ylab="Percentage of Respondents",names.arg=c("Less than high school","High School","Some college","Bachelor's or higher"))

dev.off()

#Appendix Figure 8

israel<-subset(survey2,israel==1)
ed<-israel$educat
counts <- table(ed)
percentage<-counts/length(ed)*100


plotname<-paste("figureA8.jpg")
jpeg(filename = plotname, width = 750, height = 350,
     units = "px", pointsize = 12, quality = 100, bg = "white",
     res = NA, restoreConsole = TRUE)

barplot(percentage, main="",
  	ylab="Percentage of Respondents",names.arg=c("Less than high school","High School","Some college","Bachelor's or higher"))

dev.off()

#Appendix Figure 9

arg<-subset(survey2,argentina==1)
ed<-arg$educat
counts <- table(ed)
percentage<-counts/length(ed)*100


plotname<-paste("figureA9.jpg")
jpeg(filename = plotname, width = 750, height = 350,
     units = "px", pointsize = 12, quality = 100, bg = "white",
     res = NA, restoreConsole = TRUE)

barplot(percentage, main="",
  	ylab="Percentage of Respondents",names.arg=c("Less than high school","High School","Some college","Bachelor's or higher"))

dev.off()

#Appendix Figure 10

india<-subset(survey2,india==1)
approval<-india$approve
counts <- table(approval)
percentage<-counts/length(approval)*100

plotname<-paste("figureA10.jpg")
jpeg(filename = plotname, width = 750, height = 350,
     units = "px", pointsize = 12, quality = 100, bg = "white",
     res = NA, restoreConsole = TRUE)

barplot(percentage, main="",
  	ylab="Percentage of Respondents",names.arg=c("strongly disapprove","somewhat disapprove","neither","somewhat approve","strongly approve"))

dev.off()

#Appendix Figure 11

israel<-subset(survey2,israel==1)
approval<-israel$approve
counts <- table(approval)
percentage<-counts/length(approval)*100


plotname<-paste("figureA11.jpg")
jpeg(filename = plotname, width = 750, height = 350,
     units = "px", pointsize = 12, quality = 100, bg = "white",
     res = NA, restoreConsole = TRUE)

barplot(percentage, main="",
  	ylab="Percentage of Respondents",names.arg=c("strongly disapprove","somewhat disapprove","neither","somewhat approve","strongly approve"))

dev.off()

#Appendix Figure 12

argentina<-subset(survey2,argentina==1)
approval<-argentina$approve
counts <- table(approval)
percentage<-counts/length(approval)*100


plotname<-paste("figureA12.jpg")
jpeg(filename = plotname, width = 750, height = 350,
     units = "px", pointsize = 12, quality = 100, bg = "white",
     res = NA, restoreConsole = TRUE)

barplot(percentage, main="",
  	ylab="Percentage of Respondents",names.arg=c("strongly disapprove","somewhat disapprove","neither","somewhat approve","strongly approve"))

dev.off()

#Appendix Figure 13

set.seed(12345)

india<-subset(survey2,india==1)

s<-100
attach(india)

model.m <- lm(security ~ opp_viol + agecat + male + educat + ideology7 + polint + activist, data=india)
model.y <- lm(approve ~ opp_viol + security + agecat + male + educat + ideology7 + polint + activist, data=india)
out1 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="opp_viol", mediator="security")

summary(out1)

model.m <- lm(competence ~ opp_viol + agecat + male + educat + ideology7 + polint + activist, data=india)
model.y <- lm(approve ~ opp_viol + competence + agecat + male + educat+ ideology7 + polint + activist, data=india)
out2 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="opp_viol", mediator="competence")

summary(out2)

model.m <- lm(reputation ~ opp_viol + agecat + male + educat + ideology7 + polint + activist, data=india)
model.y <- lm(approve ~ opp_viol + reputation + agecat + male + educat+ ideology7 + polint + activist, data=india)
out3 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="opp_viol", mediator="reputation")

summary(out3)

model.m <- glm(morality ~ opp_viol + agecat + male + educat + ideology7 + polint + activist, data=india,family=binomial(link="logit"))
model.y <- lm(approve ~ opp_viol + morality + agecat + male + educat+ ideology7 + polint + activist, data=india)
out4 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="opp_viol", mediator="morality")

summary(out4)

model.m <- glm(threat ~ opp_viol + agecat + male + educat + ideology7 + polint + activist, data=india,family=binomial(link="logit"))
model.y <- lm(approve ~ opp_viol + threat + agecat + male + educat+ ideology7 + polint + activist, data=india)
out5 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="opp_viol", mediator="threat")

summary(out5)

india<-array(dim=c(3,5))
india[1,1]<-summary(out1)$d1
india[2,1]<-summary(out1)$d1.ci[1]
india[3,1]<-summary(out1)$d1.ci[2]

india[1,2]<-summary(out2)$d1
india[2,2]<-summary(out2)$d1.ci[1]
india[3,2]<-summary(out2)$d1.ci[2]

india[1,3]<-summary(out3)$d1
india[2,3]<-summary(out3)$d1.ci[1]
india[3,3]<-summary(out3)$d1.ci[2]

india[1,4]<-summary(out4)$d1
india[2,4]<-summary(out4)$d1.ci[1]
india[3,4]<-summary(out4)$d1.ci[2]

india[1,5]<-summary(out5)$d1
india[2,5]<-summary(out5)$d1.ci[1]
india[3,5]<-summary(out5)$d1.ci[2]

israel<-subset(survey2,israel==1)

s<-100
attach(israel)

model.m <- lm(security ~ opp_viol + agecat + male + educat + ideology7 + polint + activist, data=israel)
model.y <- lm(approve ~ opp_viol + security + agecat + male + educat + ideology7 + polint+ activist, data=israel)
out1 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="opp_viol", mediator="security")

summary(out1)

model.m <- lm(competence ~ opp_viol + agecat + male + educat + ideology7 + polint+ activist, data=israel)
model.y <- lm(approve ~ opp_viol + competence + agecat + male + educat+ ideology7 + polint+ activist, data=israel)
out2 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="opp_viol", mediator="competence")

summary(out2)

model.m <- lm(reputation ~ opp_viol + agecat + male + educat + ideology7 + polint+ activist, data=israel)
model.y <- lm(approve ~ opp_viol + reputation + agecat + male + educat+ ideology7 + polint+ activist, data=israel)
out3 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="opp_viol", mediator="reputation")

summary(out3)

model.m <- glm(morality ~ opp_viol + agecat + male + educat + ideology7 + polint+ activist, data=israel,family=binomial(link="logit"))
model.y <- lm(approve ~ opp_viol + morality + agecat + male + educat+ ideology7 + polint+ activist, data=israel)
out4 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="opp_viol", mediator="morality")

summary(out4)

model.m <- glm(threat ~ opp_viol + agecat + male + educat + ideology7 + polint+ activist, data=israel,family=binomial(link="logit"))
model.y <- lm(approve ~ opp_viol + threat + agecat + male + educat+ ideology7 + polint+ activist, data=israel)
out5 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="opp_viol", mediator="threat")

summary(out5)

israel<-array(dim=c(3,5))
israel[1,1]<-summary(out1)$d1
israel[2,1]<-summary(out1)$d1.ci[1]
israel[3,1]<-summary(out1)$d1.ci[2]

israel[1,2]<-summary(out2)$d1
israel[2,2]<-summary(out2)$d1.ci[1]
israel[3,2]<-summary(out2)$d1.ci[2]

israel[1,3]<-summary(out3)$d1
israel[2,3]<-summary(out3)$d1.ci[1]
israel[3,3]<-summary(out3)$d1.ci[2]

israel[1,4]<-summary(out4)$d1
israel[2,4]<-summary(out4)$d1.ci[1]
israel[3,4]<-summary(out4)$d1.ci[2]

israel[1,5]<-summary(out5)$d1
israel[2,5]<-summary(out5)$d1.ci[1]
israel[3,5]<-summary(out5)$d1.ci[2]

argentina<-subset(survey2,argentina==1)

argentina$security[is.na(argentina$security)] <- 100
argentina$approve[is.na(argentina$approve)] <- 100
argentina$competence[is.na(argentina$competence)] <- 100
argentina$reputation[is.na(argentina$reputation)] <- 100

s<-100

argentina_a<-subset(argentina,security<100)
argentina_a<-subset(argentina_a,approve<100)
attach(argentina_a)

model.m <- lm(security ~ opp_viol + agecat + male + educat + activist, data=argentina_a)
model.y <- lm(approve ~ opp_viol + security + agecat + male + educat + activist, data=argentina_a)
out1 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="opp_viol", mediator="security")

summary(out1)

argentina_b<-subset(argentina,competence<100)
argentina_b<-subset(argentina_b,approve<100)
attach(argentina_b)

model.m <- lm(competence ~ opp_viol + agecat + male + educat + activist , data=argentina_b)
model.y <- lm(approve ~ opp_viol + competence + agecat + male + educat + activist, data=argentina_b)
out2 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="opp_viol", mediator="competence")

summary(out2)

argentina_c<-subset(argentina,reputation<100)
argentina_c<-subset(argentina_c,approve<100)
attach(argentina_c)

model.m <- lm(reputation ~ opp_viol + agecat + male + educat + activist, data=argentina_c)
model.y <- lm(approve ~ opp_viol + reputation + agecat + male + educat + activist, data=argentina_c)
out3 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="opp_viol", mediator="reputation")

summary(out3)

argentina<-array(dim=c(3,5))
argentina[1,1]<-summary(out1)$d1
argentina[2,1]<-summary(out1)$d1.ci[1]
argentina[3,1]<-summary(out1)$d1.ci[2]

argentina[1,2]<-summary(out2)$d1
argentina[2,2]<-summary(out2)$d1.ci[1]
argentina[3,2]<-summary(out2)$d1.ci[2]

argentina[1,3]<-summary(out3)$d1
argentina[2,3]<-summary(out3)$d1.ci[1]
argentina[3,3]<-summary(out3)$d1.ci[2]


india<-india*100
argentina<-argentina*100
israel<-israel*100

modellabels1<-c("Security","Competence","Reputation","Morality", "Threat")

y<-c(5,4,3,2,1)

plotname<-paste("figureA13.jpg")
jpeg(filename = plotname, width = 1100, height = 400,
     units = "px", pointsize = 12, quality = 100, bg = "white",
     res = NA, restoreConsole = TRUE)

par(mai=c(0.5,1.5,1,0.5),oma=c(0.5,0,0,0),cex=1.2,cex.axis=1.2,mgp=c(1,1,0))

par(mfrow=c(1,3))

plot(india[1,],y,ylab="",yaxt="n",xlab="",
     xlim=c(-10,10),cex=2.5,pch=16)
lines(c(0,0),c(0,dim(india)[2]+1),lty="dotted")
for(i in 1:5) lines(india[2:3,i],c(y[i],y[i]),lwd=2)
axis(2,labels=modellabels1, at=c(5,4,3,2,1),las=1,font=1,tick=FALSE,ps=5,cex.axis=2)
mtext("India",side=3,line=.5,font=1,cex=1.5)
mtext("95% Confidence Intervals",side=1,line=3,font=1,cex=1.2)

plot(israel[1,],y,ylab="",yaxt="n",xlab="",
     xlim=c(-10,10),cex=2.5,pch=16)
lines(c(0,0),c(0,dim(israel)[2]+1),lty="dotted")
for(i in 1:5) lines(israel[2:3,i],c(y[i],y[i]),lwd=2)
axis(2,labels=modellabels1, at=c(5,4,3,2,1),las=1,font=1,tick=FALSE,ps=5,cex.axis=2)
mtext("Israel",side=3,line=.5,font=1,cex=1.5)
mtext("95% Confidence Intervals",side=1,line=3,font=1,cex=1.2)

plot(argentina[1,],y,ylab="",yaxt="n",xlab="",
     xlim=c(-10,10),cex=2.5,pch=16)
lines(c(0,0),c(0,dim(argentina)[2]+1),lty="dotted")
for(i in 1:3) lines(argentina[2:3,i],c(y[i],y[i]),lwd=2)
axis(2,labels=modellabels1[1:3], at=c(5,4,3),las=1,font=1,tick=FALSE,ps=5,cex.axis=2)
mtext("Argentina",side=3,line=.5,font=1,cex=1.5)
mtext("95% Confidence Intervals",side=1,line=3,font=1,cex=1.2)

mtext("Average Causal Mediation Effects",outer=TRUE,side=3,line=-1.5,font=1,cex=1.5)
mtext("Opposition Violence",outer=TRUE,side=3,line=-4,font=1,cex=1.5)

dev.off()

#Appendix Figure 14

india<-subset(survey2,india==1)

s<-100
attach(india)

model.m <- lm(security ~ treaty + agecat + male + educat + ideology7 + polint + activist, data=india)
model.y <- lm(approve ~ treaty + security + agecat + male + educat + ideology7 + polint + activist, data=india)
out1 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="treaty", mediator="security")

summary(out1)

model.m <- lm(competence ~ treaty + agecat + male + educat + ideology7 + polint + activist, data=india)
model.y <- lm(approve ~ treaty + competence + agecat + male + educat+ ideology7 + polint + activist, data=india)
out2 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="treaty", mediator="competence")

summary(out2)

model.m <- lm(reputation ~ treaty + agecat + male + educat + ideology7 + polint + activist, data=india)
model.y <- lm(approve ~ treaty + reputation + agecat + male + educat+ ideology7 + polint + activist, data=india)
out3 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="treaty", mediator="reputation")

summary(out3)

model.m <- glm(morality ~ treaty + agecat + male + educat + ideology7 + polint + activist, data=india,family=binomial(link="logit"))
model.y <- lm(approve ~ treaty + morality + agecat + male + educat+ ideology7 + polint + activist, data=india)
out4 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="treaty", mediator="morality")

summary(out4)

model.m <- glm(threat ~ treaty + agecat + male + educat + ideology7 + polint + activist, data=india,family=binomial(link="logit"))
model.y <- lm(approve ~ treaty + threat + agecat + male + educat+ ideology7 + polint + activist, data=india)
out5 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="treaty", mediator="threat")

summary(out5)

indiares<-array(dim=c(3,5))
indiares[1,1]<-summary(out1)$d1
indiares[2,1]<-summary(out1)$d1.ci[1]
indiares[3,1]<-summary(out1)$d1.ci[2]

indiares[1,2]<-summary(out2)$d1
indiares[2,2]<-summary(out2)$d1.ci[1]
indiares[3,2]<-summary(out2)$d1.ci[2]

indiares[1,3]<-summary(out3)$d1
indiares[2,3]<-summary(out3)$d1.ci[1]
indiares[3,3]<-summary(out3)$d1.ci[2]

indiares[1,4]<-summary(out4)$d1
indiares[2,4]<-summary(out4)$d1.ci[1]
indiares[3,4]<-summary(out4)$d1.ci[2]

indiares[1,5]<-summary(out5)$d1
indiares[2,5]<-summary(out5)$d1.ci[1]
indiares[3,5]<-summary(out5)$d1.ci[2]

israel<-subset(survey2,israel==1)

s<-100
attach(israel)

model.m <- lm(security ~ treaty + agecat + male + educat + ideology7 + polint + activist, data=israel)
model.y <- lm(approve ~ treaty + security + agecat + male + educat + ideology7 + polint+ activist, data=israel)
out1 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="treaty", mediator="security")

summary(out1)

model.m <- lm(competence ~ treaty + agecat + male + educat + ideology7 + polint+ activist, data=israel)
model.y <- lm(approve ~ treaty + competence + agecat + male + educat+ ideology7 + polint+ activist, data=israel)
out2 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="treaty", mediator="competence")

summary(out2)

model.m <- lm(reputation ~ treaty + agecat + male + educat + ideology7 + polint+ activist, data=israel)
model.y <- lm(approve ~ treaty + reputation + agecat + male + educat+ ideology7 + polint+ activist, data=israel)
out3 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="treaty", mediator="reputation")

summary(out3)

model.m <- glm(morality ~ treaty + agecat + male + educat + ideology7 + polint+ activist, data=israel,family=binomial(link="logit"))
model.y <- lm(approve ~ treaty + morality + agecat + male + educat+ ideology7 + polint+ activist, data=israel)
out4 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="treaty", mediator="morality")

summary(out4)

model.m <- glm(threat2 ~ treaty + agecat + male + educat + ideology7 + polint+ activist, data=israel,family=binomial(link="logit"))
model.y <- lm(approve ~ treaty + threat + agecat + male + educat+ ideology7 + polint+ activist, data=israel)
out5 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="treaty", mediator="threat")

summary(out5)

israelres<-array(dim=c(3,5))
israelres[1,1]<-summary(out1)$d1
israelres[2,1]<-summary(out1)$d1.ci[1]
israelres[3,1]<-summary(out1)$d1.ci[2]

israelres[1,2]<-summary(out2)$d1
israelres[2,2]<-summary(out2)$d1.ci[1]
israelres[3,2]<-summary(out2)$d1.ci[2]

israelres[1,3]<-summary(out3)$d1
israelres[2,3]<-summary(out3)$d1.ci[1]
israelres[3,3]<-summary(out3)$d1.ci[2]

israelres[1,4]<-summary(out4)$d1
israelres[2,4]<-summary(out4)$d1.ci[1]
israelres[3,4]<-summary(out4)$d1.ci[2]

israelres[1,5]<-summary(out5)$d1
israelres[2,5]<-summary(out5)$d1.ci[1]
israelres[3,5]<-summary(out5)$d1.ci[2]


indiares<-indiares*100
israelres<-israelres*100


modellabels1<-c("Security","Competence","Reputation","Morality", "Threat")

y<-c(5,4,3,2,1)

plotname<-paste("figureA14.jpg")
jpeg(filename = plotname, width = 1100, height = 600,
     units = "px", pointsize = 12, quality = 100, bg = "white",
     res = NA, restoreConsole = TRUE)


par(mai=c(0.8,1.5,1,0.5),oma=c(0,0.7,0,0),cex=1.2,cex.axis=0.8,mgp=c(1,1,0))

par(mfrow=c(1,2))

plot(indiares[1,],y,ylab="",yaxt="n",xlab="",
     xlim=c(-10,10),cex=1.8,pch=16)
lines(c(0,0),c(0,dim(indiares)[2]+1),lty="dotted")
for(i in 1:5) lines(indiares[2:3,i],c(y[i],y[i]),lwd=2)
axis(2,labels=modellabels1, at=c(5,4,3,2,1),las=1,font=1,tick=FALSE,ps=5,cex.axis=1.5)
mtext("India",side=3,line=.5,font=1,cex=1.5)
mtext("95% Confidence Intervals",side=1,line=2,font=1,cex=1.2)


plot(israelres[1,],y,ylab="",yaxt="n",xlab="",
     xlim=c(-10,10),cex=1.8,pch=16)
lines(c(0,0),c(0,dim(israelres)[2]+1),lty="dotted")
for(i in 1:5) lines(israelres[2:3,i],c(y[i],y[i]),lwd=2)
axis(2,labels=modellabels1, at=c(5,4,3,2,1),las=1,font=1,tick=FALSE,ps=5,cex.axis=1.5)
mtext("Israel",side=3,line=.5,font=1,cex=1.5)
mtext("95% Confidence Intervals",side=1,line=2,font=1,cex=1.2)

mtext("Average Causal Mediation Effects",outer=TRUE,side=3,line=-1.5,font=1,cex=1.5)
mtext("International Law",outer=TRUE,side=3,line=-3,font=1,cex=1.5)
dev.off()

#Appendix Figure 15


india<-subset(survey2,india==1)

s<-100
attach(india)


model.m <- lm(security ~ opp_viol + agecat + male + educat + ideology7 + polint + activist, data=india)
model.y <- lm(approve ~ opp_viol + security + competence + reputation + morality + threat+ agecat + male + educat + ideology7 + polint + activist, data=india)
out1 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="opp_viol", mediator="security")

summary(out1)


model.m <- lm(competence ~ opp_viol + agecat + male + educat + ideology7 + polint + activist, data=india)
model.y <- lm(approve ~ opp_viol + security + competence + reputation + morality + threat + agecat + male + educat+ ideology7 + polint + activist, data=india)
out2 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="opp_viol", mediator="competence")

summary(out2)


model.m <- lm(reputation ~ opp_viol + agecat + male + educat + ideology7 + polint + activist, data=india)
model.y <- lm(approve ~ opp_viol + security + competence + reputation + morality + threat + agecat + male + educat+ ideology7 + polint + activist, data=india)
out3 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="opp_viol", mediator="reputation")

summary(out3)


model.m <- glm(morality ~ opp_viol + agecat + male + educat + ideology7 + polint + activist, data=india,family=binomial(link="logit"))
model.y <- lm(approve ~ opp_viol + security + competence + reputation + morality + threat + agecat + male + educat+ ideology7 + polint + activist, data=india)
out4 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="opp_viol", mediator="morality")

summary(out4)


model.m <- glm(threat ~ opp_viol + agecat + male + educat + ideology7 + polint + activist, data=india,family=binomial(link="logit"))
model.y <- lm(approve ~ opp_viol + security + competence + reputation + morality + threat + agecat + male + educat+ ideology7 + polint + activist, data=india)
out5 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="opp_viol", mediator="threat")

summary(out5)



india<-array(dim=c(3,5))
india[1,1]<-summary(out1)$d1
india[2,1]<-summary(out1)$d1.ci[1]
india[3,1]<-summary(out1)$d1.ci[2]

india[1,2]<-summary(out2)$d1
india[2,2]<-summary(out2)$d1.ci[1]
india[3,2]<-summary(out2)$d1.ci[2]

india[1,3]<-summary(out3)$d1
india[2,3]<-summary(out3)$d1.ci[1]
india[3,3]<-summary(out3)$d1.ci[2]

india[1,4]<-summary(out4)$d1
india[2,4]<-summary(out4)$d1.ci[1]
india[3,4]<-summary(out4)$d1.ci[2]

india[1,5]<-summary(out5)$d1
india[2,5]<-summary(out5)$d1.ci[1]
india[3,5]<-summary(out5)$d1.ci[2]

israel<-subset(survey2,israel==1)

israel$security[is.na(israel$security)] <- 100
israel$approve[is.na(israel$approve)] <- 100
israel$competence[is.na(israel$competence)] <- 100
israel$reputation[is.na(israel$reputation)] <- 100
israel$morality[is.na(israel$morality)] <- 100
israel$threat[is.na(israel$threat)] <- 100

s<-100

israel_a<-subset(israel,security<100)
israel_a<-subset(israel_a,approve<100)
israel_a<-subset(israel_a,competence<100)
israel_a<-subset(israel_a,reputation<100)
israel_a<-subset(israel_a,morality<100)
israel_a<-subset(israel_a,threat<100)
attach(israel_a)


model.m <- lm(security ~ opp_viol + agecat + male + educat + ideology7 + polint + activist, data=israel_a)
model.y <- lm(approve ~ opp_viol+ security + competence + reputation + morality + threat + agecat + male + educat + ideology7 + polint+ activist, data=israel_a)
out1 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="opp_viol", mediator="security")

summary(out1)


model.m <- lm(competence ~ opp_viol + agecat + male + educat + ideology7 + polint+ activist, data=israel_a)
model.y <- lm(approve ~ opp_viol+ security + competence + reputation + morality + threat + agecat + male + educat+ ideology7 + polint+ activist, data=israel_a)
out2 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="opp_viol", mediator="competence")

summary(out2)


model.m <- lm(reputation ~ opp_viol + agecat + male + educat + ideology7 + polint+ activist, data=israel_a)
model.y <- lm(approve ~ opp_viol+ security + competence + reputation + morality + threat + agecat + male + educat+ ideology7 + polint+ activist, data=israel_a)
out3 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="opp_viol", mediator="reputation")

summary(out3)


model.m <- glm(morality ~ opp_viol + agecat + male + educat + ideology7 + polint+ activist, data=israel_a,family=binomial(link="logit"))
model.y <- lm(approve ~ opp_viol + security + competence + reputation + morality + threat + agecat + male + educat+ ideology7 + polint+ activist, data=israel_a)
out4 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="opp_viol", mediator="morality")

summary(out4)


model.m <- glm(threat ~ opp_viol + agecat + male + educat + ideology7 + polint+ activist, data=israel_a,family=binomial(link="logit"))
model.y <- lm(approve ~ opp_viol + security + competence + reputation + morality + threat + agecat + male + educat+ ideology7 + polint+ activist, data=israel_a)
out5 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="opp_viol", mediator="threat")

summary(out5)


israel<-array(dim=c(3,5))
israel[1,1]<-summary(out1)$d1
israel[2,1]<-summary(out1)$d1.ci[1]
israel[3,1]<-summary(out1)$d1.ci[2]

israel[1,2]<-summary(out2)$d1
israel[2,2]<-summary(out2)$d1.ci[1]
israel[3,2]<-summary(out2)$d1.ci[2]

israel[1,3]<-summary(out3)$d1
israel[2,3]<-summary(out3)$d1.ci[1]
israel[3,3]<-summary(out3)$d1.ci[2]

israel[1,4]<-summary(out4)$d1
israel[2,4]<-summary(out4)$d1.ci[1]
israel[3,4]<-summary(out4)$d1.ci[2]

israel[1,5]<-summary(out5)$d1
israel[2,5]<-summary(out5)$d1.ci[1]
israel[3,5]<-summary(out5)$d1.ci[2]




argentina<-subset(survey2,argentina==1)


argentina$security[is.na(argentina$security)] <- 100
argentina$approve[is.na(argentina$approve)] <- 100
argentina$competence[is.na(argentina$competence)] <- 100
argentina$reputation[is.na(argentina$reputation)] <- 100

s<-100


argentina_a<-subset(argentina,security<100)
argentina_a<-subset(argentina_a,approve<100)
argentina_a<-subset(argentina_a,competence<100)
argentina_a<-subset(argentina_a,reputation<100)
attach(argentina_a)


model.m <- lm(security ~ opp_viol + agecat + male + educat + activist, data=argentina_a)
model.y <- lm(approve ~ opp_viol + security + competence + reputation + agecat + male+ educat + activist, data=argentina_a)
out1 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="opp_viol", mediator="security")

summary(out1)


model.m <- lm(competence ~ opp_viol + agecat + male + educat + activist , data=argentina_a)
model.y <- lm(approve ~ opp_viol + security + competence + reputation + agecat + male + educat + activist, data=argentina_a)
out2 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="opp_viol", mediator="competence")

summary(out2)


model.m <- lm(reputation ~ opp_viol + agecat + male + educat + activist , data=argentina_a)
model.y <- lm(approve ~ opp_viol + security + competence + reputation + agecat + male + educat + activist, data=argentina_a)
out3 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="opp_viol", mediator="reputation")

summary(out3)



argentina<-array(dim=c(3,5))
argentina[1,1]<-summary(out1)$d1
argentina[2,1]<-summary(out1)$d1.ci[1]
argentina[3,1]<-summary(out1)$d1.ci[2]

argentina[1,2]<-summary(out2)$d1
argentina[2,2]<-summary(out2)$d1.ci[1]
argentina[3,2]<-summary(out2)$d1.ci[2]

argentina[1,3]<-summary(out3)$d1
argentina[2,3]<-summary(out3)$d1.ci[1]
argentina[3,3]<-summary(out3)$d1.ci[2]


israel<-israel*100
india<-india*100
argentina<-argentina*100

modellabels1<-c("Security","Competence","Reputation","Morality", "Threat")

y<-c(5,4,3,2,1)

plotname<-paste("figureA15.jpg")
jpeg(filename = plotname, width = 1100, height = 900,
     units = "px", pointsize = 12, quality = 100, bg = "white",
     res = NA, restoreConsole = TRUE)


par(mai=c(0.5,1.5,1,0.5),oma=c(1,1.5,0,0),cex=1.2,cex.axis=1.2,mgp=c(1,1,0))

par(mfrow=c(2,2))

plot(india[1,],y,ylab="",yaxt="n",xlab="",
     xlim=c(-5,5),cex=2.5,pch=16)
lines(c(0,0),c(0,dim(india)[2]+1),lty="dotted")
for(i in 1:5) lines(india[2:3,i],c(y[i],y[i]),lwd=2)
axis(2,labels=modellabels1, at=c(5,4,3,2,1),las=1,font=1,tick=FALSE,ps=5,cex.axis=2)
mtext("India",side=3,line=.5,font=1,cex=1.5)
mtext("95% Confidence Intervals",side=1,line=3,font=1,cex=1.2)


plot(israel[1,],y,ylab="",yaxt="n",xlab="",
     xlim=c(-5,5),cex=2.5,pch=16)
lines(c(0,0),c(0,dim(israel)[2]+1),lty="dotted")
for(i in 1:5) lines(israel[2:3,i],c(y[i],y[i]),lwd=2)
axis(2,labels=modellabels1, at=c(5,4,3,2,1),las=1,font=1,tick=FALSE,ps=5,cex.axis=2)
mtext("Israel",side=3,line=.5,font=1,cex=1.5)
mtext("95% Confidence Intervals",side=1,line=3,font=1,cex=1.2)


plot(argentina[1,],y,ylab="",yaxt="n",xlab="",
     xlim=c(-5,5),cex=2.5,pch=16)
lines(c(0,0),c(0,dim(argentina)[2]+1),lty="dotted")
for(i in 1:3) lines(argentina[2:3,i],c(y[i],y[i]),lwd=2)
axis(2,labels=modellabels1[1:3], at=c(5,4,3),las=1,font=1,tick=FALSE,ps=5,cex.axis=2)
mtext("Argentina",side=3,line=.5,font=1,cex=1.5)
mtext("95% Confidence Intervals",side=1,line=3,font=1,cex=1.2)

dev.off()

#Appendix Figure 16


india<-subset(survey2,india==1)

s<-100
attach(india)


model.m <- lm(security ~ treaty + agecat + male + educat + ideology7 + polint + activist, data=india)
model.y <- lm(approve ~ treaty + security + competence + reputation + morality + threat + agecat + male + educat + ideology7 + polint + activist, data=india)
out1 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="treaty", mediator="security")

summary(out1)

model.m <- lm(competence ~ treaty + agecat + male + educat + ideology7 + polint + activist, data=india)
model.y <- lm(approve ~ treaty + security + competence + reputation + morality + threat + agecat + male + educat+ ideology7 + polint + activist, data=india)
out2 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="treaty", mediator="competence")

summary(out2)

model.m <- lm(reputation ~ treaty + agecat + male + educat + ideology7 + polint + activist, data=india)
model.y <- lm(approve ~ treaty + security + competence + reputation + morality + threat + agecat + male + educat+ ideology7 + polint + activist, data=india)
out3 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="treaty", mediator="reputation")

summary(out3)

model.m <- glm(morality ~ treaty + agecat + male + educat + ideology7 + polint + activist, data=india,family=binomial(link="logit"))
model.y <- lm(approve ~ treaty + security + competence + reputation + morality + threat + agecat + male + educat+ ideology7 + polint + activist, data=india)
out4 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="treaty", mediator="morality")

summary(out4)

model.m <- glm(threat ~ treaty + agecat + male + educat + ideology7 + polint + activist, data=india,family=binomial(link="logit"))
model.y <- lm(approve ~ treaty + security + competence + reputation + morality + threat + agecat + male + educat+ ideology7 + polint + activist, data=india)
out5 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="treaty", mediator="threat")

summary(out5)

india<-array(dim=c(3,5))
india[1,1]<-summary(out1)$d1
india[2,1]<-summary(out1)$d1.ci[1]
india[3,1]<-summary(out1)$d1.ci[2]

india[1,2]<-summary(out2)$d1
india[2,2]<-summary(out2)$d1.ci[1]
india[3,2]<-summary(out2)$d1.ci[2]

india[1,3]<-summary(out3)$d1
india[2,3]<-summary(out3)$d1.ci[1]
india[3,3]<-summary(out3)$d1.ci[2]

india[1,4]<-summary(out4)$d1
india[2,4]<-summary(out4)$d1.ci[1]
india[3,4]<-summary(out4)$d1.ci[2]

india[1,5]<-summary(out5)$d1
india[2,5]<-summary(out5)$d1.ci[1]
india[3,5]<-summary(out5)$d1.ci[2]

israel<-subset(survey2,israel==1)

israel$security[is.na(israel$security)] <- 100
israel$approve[is.na(israel$approve)] <- 100
israel$competence[is.na(israel$competence)] <- 100
israel$reputation[is.na(israel$reputation)] <- 100
israel$morality[is.na(israel$morality)] <- 100
israel$threat[is.na(israel$threat)] <- 100

s<-100

israel_a<-subset(israel,security<100)
israel_a<-subset(israel_a,approve<100)
israel_a<-subset(israel_a,competence<100)
israel_a<-subset(israel_a,reputation<100)
israel_a<-subset(israel_a,morality<100)
israel_a<-subset(israel_a,threat<100)
attach(israel_a)

model.m <- lm(security ~ treaty + agecat + male + educat + ideology7 + polint + activist, data=israel_a)
model.y <- lm(approve ~ treaty + security + competence + reputation + morality + threat + agecat + male + educat + ideology7 + polint+ activist, data=israel_a)
out1 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="treaty", mediator="security")

summary(out1)


model.m <- lm(competence ~ treaty + agecat + male + educat + ideology7 + polint+ activist, data=israel_a)
model.y <- lm(approve ~ treaty + security + competence + reputation + morality + threat + agecat + male + educat+ ideology7 + polint+ activist, data=israel_a)
out2 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="treaty", mediator="competence")

summary(out2)


model.m <- lm(reputation ~ treaty + agecat + male + educat + ideology7 + polint+ activist, data=israel_a)
model.y <- lm(approve ~ treaty + security + competence + reputation + morality + threat + agecat + male + educat+ ideology7 + polint+ activist, data=israel_a)
out3 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="treaty", mediator="reputation")

summary(out3)


model.m <- glm(morality ~ treaty + agecat + male + educat + ideology7 + polint+ activist, data=israel_a,family=binomial(link="logit"))
model.y <- lm(approve ~ treaty+ security + competence + reputation + morality + threat + agecat + male + educat+ ideology7 + polint+ activist, data=israel_a)
out4 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="treaty", mediator="morality")

summary(out4)


model.m <- glm(threat ~ treaty + agecat + male + educat + ideology7 + polint+ activist, data=israel_a,family=binomial(link="logit"))
model.y <- lm(approve ~ treaty + security + competence + reputation + morality + threat + agecat + male + educat+ ideology7 + polint+ activist, data=israel_a)
out5 <- mediate(model.m, model.y, sims=s, boot=TRUE, treat="treaty", mediator="threat")

summary(out5)


israel<-array(dim=c(3,5))
israel[1,1]<-summary(out1)$d1
israel[2,1]<-summary(out1)$d1.ci[1]
israel[3,1]<-summary(out1)$d1.ci[2]

israel[1,2]<-summary(out2)$d1
israel[2,2]<-summary(out2)$d1.ci[1]
israel[3,2]<-summary(out2)$d1.ci[2]

israel[1,3]<-summary(out3)$d1
israel[2,3]<-summary(out3)$d1.ci[1]
israel[3,3]<-summary(out3)$d1.ci[2]

israel[1,4]<-summary(out4)$d1
israel[2,4]<-summary(out4)$d1.ci[1]
israel[3,4]<-summary(out4)$d1.ci[2]

israel[1,5]<-summary(out5)$d1
israel[2,5]<-summary(out5)$d1.ci[1]
israel[3,5]<-summary(out5)$d1.ci[2]

israel<-israel*100
india<-india*100


modellabels1<-c("Security","Competence","Reputation","Morality", "Threat")

y<-c(5,4,3,2,1)

plotname<-paste("figureA16.jpg")
jpeg(filename = plotname, width = 800, height = 500,
     units = "px", pointsize = 12, quality = 100, bg = "white",
     res = NA, restoreConsole = TRUE)

par(mai=c(0.8,1.5,1,0.5),oma=c(0,0.7,0,0),cex=1.2,cex.axis=0.8,mgp=c(1,1,0))

par(mfrow=c(1,2))

plot(india[1,],y,ylab="",yaxt="n",xlab="",
     xlim=c(-5,5),cex=1.8,pch=16)
lines(c(0,0),c(0,dim(india)[2]+1),lty="dotted")
for(i in 1:5) lines(india[2:3,i],c(y[i],y[i]),lwd=2)
axis(2,labels=modellabels1, at=c(5,4,3,2,1),las=1,font=1,tick=FALSE,ps=5,cex.axis=1.5)
mtext("India",side=3,line=.5,font=1,cex=1.5)
mtext("95% Confidence Intervals",side=1,line=2,font=1,cex=1.2)


plot(israel[1,],y,ylab="",yaxt="n",xlab="",
     xlim=c(-5,5),cex=1.8,pch=16)
lines(c(0,0),c(0,dim(israel)[2]+1),lty="dotted")
for(i in 1:5) lines(israel[2:3,i],c(y[i],y[i]),lwd=2)
axis(2,labels=modellabels1, at=c(5,4,3,2,1),las=1,font=1,tick=FALSE,ps=5,cex.axis=1.5)
mtext("Israel",side=3,line=.5,font=1,cex=1.5)
mtext("95% Confidence Intervals",side=1,line=2,font=1,cex=1.2)
dev.off()

# Appendix Figure 17

argentina<-subset(survey,argentina==1)
india<-subset(survey,india==1)
israel<-subset(survey,israel==1)

results<-matrix(ncol=3,nrow=3)
x<-t.test(india$approve~india$gov_statement)
results[1,1]<-x$estimate[1]-x$estimate[2]
results[1,2]<-x$conf.int[1]
results[1,3]<-x$conf.int[2]
x<-t.test(israel$approve~israel$gov_statement)
results[2,1]<-x$estimate[1]-x$estimate[2]
results[2,2]<-x$conf.int[1]
results[2,3]<-x$conf.int[2]
x<-t.test(argentina$approve~argentina$gov_statement)
results[3,1]<-x$estimate[1]-x$estimate[2]
results[3,2]<-x$conf.int[1]
results[3,3]<-x$conf.int[2]
results<-results*-100


modellabels1<-c("India","Israel","Argentina")

y<-c(3,2,1)

plotname<-paste("figureA17.jpg")
jpeg(filename = plotname, width = 500, height = 350,
     units = "px", pointsize = 12, quality = 100, bg = "white",
     res = NA, restoreConsole = TRUE)

par(mai=c(1.5,1.5,1,0.5),oma=c(0,0,0,0),cex=1.4,cex.axis=1,mgp=c(1,1,0))

plot(results[,1],y,ylab="",yaxt="n",xlab="",
     xlim=c(-20,40),cex=1,pch=16)
lines(c(0,0),c(0,dim(results)[2]+1),lty="dotted")
for(i in 1:3) lines(results[i,2:3],c(y[i],y[i]))
axis(2,labels=modellabels1, at=c(3,2,1),las=1,font=1,tick=FALSE,ps=5,cex.axis=1.2)
mtext("% Change in Government Approval",side=1,line=2.2,font=1,cex=1.5)
mtext("95% Confidence Intervals",side=1,line=3.5,font=1,cex=1.2)
mtext("Government Statement",side=3,line=1.25,font=1,cex=1.5)

dev.off()


