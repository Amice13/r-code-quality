rm(list=ls())
load("data/results.mat.Rdata")
load("data/threshold.Rdata")
load("data/confirmation.Rdata")
load("data/sens.mat.Rdata")

library(RColorBrewer)
library(lattice)
library(ggplot2)
library(ggtern)
library(colorspace)
library(grid)
library(shape)
source("R.code/wrap_sentence.R")

ypos=seq(0.0)
bw.value <- 3
dens.strip.width <- 3.25
cex.talk.value <- 0.9
cex.label <- 0.9
scale <- 1
var.list <- c("longterm.quit","adol.longterm.smokers","ya.longterm.smokers")
ymin <- min(apply(results.mat[,var.list],2,quantile,probs=0.025)/scale)
ymax <- max(apply(results.mat[,var.list],2,quantile,probs=0.975)/scale)
x.pos <- seq(1,11,5)
text.width <- 16
adol.ages <- as.character(12:17)
ya.ages <- as.character(18:29)
adult.ages <- as.character(seq(25,65,5))
adult.ages.lab <- c("25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69")

print.numeric <- function(x, digits = 0) formatC(x, digits = digits, big.mark=",", format = "d")
print.numeric2 <- function(x, digits = 1) formatC(x, digits = digits, big.mark=",", format = "f")

pdf("figures/figure2.pdf",width=8.5,height=5.5)
par (mfrow=c(1,1),mgp=c(2.75,1,0)*0.55,mar=c(1.6,3.5,0.5,2)*1.6,omi=c(0.2,0.5,0.4,0), tcl=-0.25,bg="white", cex=cex.talk.value,cex.main=cex.talk.value)
plot(0,0,ylim=c(1.1*ymin,ymax),bty="n",axes=FALSE,xlab=NA,ylab=NA,pch=NA,xlim=c(-0.5,x.pos[length(x.pos)]+1))
abline(h=0,col="grey",lwd=1)
for (i in 1:length(var.list)) {
    points(x.pos[i],mean(results.mat[,var.list[i]]/scale),pch=19,cex=1.5)
    lines(rep(x.pos[i],2),quantile(results.mat[,var.list[i]]/scale,probs=c(0.025,0.975)),lwd=2)
    text(rep(x.pos[i],1),mean(results.mat[,var.list[i]]/scale),
         paste(print.numeric(mean(results.mat[,var.list[i]]))),pos=c(4),cex=cex.label)
    text(rep(x.pos[i],2),quantile(results.mat[,var.list[i]]/scale,probs=c(0.025,0.975)),
         paste(print.numeric(quantile(results.mat[,var.list[i]],probs=c(0.025,0.975)),0)),pos=c(4,4),cex=cex.label)
}
axis(2,las=1,at=seq(-60000,150000,20000),paste(formatC(seq(-60000,150000,20000),big.mark=",",format="f",digits=0)),cex.axis=1)
axis(1,at=x.pos,line=2,tick=FALSE,paste(c(wrap_sentence("Addt'l Current Adult Cig. Smokers Who Quit For >=7 Years",text.width),
                                          wrap_sentence("Addt'l Adol. Who Initiate Cig. Smoking and Eventually Become Daily Cig. Smokers at Age 35-39",text.width),
                                          wrap_sentence("Addt'l Young Adults Who Initiate Cig. Smoking and Eventually Become Daily Cig. Smokers at Age 35-39",text.width)))
     ,cex.axis=cex.label)
mtext("Number of Persons",side=2,line=-1,outer=TRUE,at=1/2,cex=cex.talk.value)
dev.off()

scale <- 10^6
tmp <- apply(results.mat[,c("adol.life.years","ya.life.years","adult.cig.quit.life.years")],1,sum)
ymin <- quantile(tmp,probs=0.01)
ymax <- max(quantile(tmp,probs=0.985),0)
dens.strip.width <- 0.5
pdf("figures/figure3.pdf",width=5.5,height=5.5)
par (mfrow=c(1,1),mgp=c(2.75,1,0)*0.55,mar=c(1.6,3.5,0.5,2)*1.6,omi=c(0.2,0.5,0.4,0), tcl=-0.25,bg="white", cex=cex.talk.value,cex.main=cex.talk.value)
plot(0,0,ylim=c(ymin,ymax),bty="n",axes=FALSE,xlab=NA,ylab=NA,pch=NA,xlim=c(-1,1))
abline(h=0,col="grey",lwd=1)
#denstrip(tmp,at=0,width=1.5,horiz=FALSE,colmax="darkmagenta")
points(0,mean(tmp),pch=19,cex=1.5)
lines(rep(0,2),quantile(tmp,probs=c(0.025,0.975)),lwd=2)
text(rep(0,1),mean(tmp),
     paste(print.numeric(mean(tmp))),pos=4,cex=cex.label)
text(rep(0,2),quantile(tmp,probs=c(0.025,0.975))+c(0,0),pos=4,
     paste(print.numeric(quantile(tmp,probs=c(0.025,0.975)))),cex=cex.label)
axis(2,las=1,at=seq(-2.2*10^6,0*10^6,2*10^5),paste(formatC(seq(-2.2*10^6,0*10^6,2*10^5),big.mark =",",format="f",digits=0)),cex.axis=1)
mtext("Total Number of Years of Life Gained",side=2,line=-0.5,outer=TRUE,at=1/2,cex=cex.talk.value)
dev.off()

pdf("figures/figure4.pdf",width=8.5,height=8.5)
par (mfrow=c(2,2),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.1,1.75,2)*1.6,omi=c(0.2,0.5,0.1,0), tcl=-0.25,bg="white", cex=cex.talk.value,cex.main=cex.talk.value)

plot(colnames(sens.cig.quit.mat), 100*apply(sens.cig.quit.mat, 2, function(x) length(which(x>0))/length(x)), xlab="Relative Risk", ylab=NA, xaxt="n", yaxt="n", las=1, pch=NA, col="darkgrey", bty="l", ylim=c(0,100))
tmp <- lowess(colnames(sens.cig.quit.mat), 100*apply(sens.cig.quit.mat, 2, function(x) length(which(x>0))/length(x)), f=0.2)
grid(lty=1)
lines(tmp$x, tmp$y,lwd=2)
axis(1, at=seq(1,4,0.5))
axis(2, at=seq(0,100,10),las=1)
title ("A. Adj. Odds Ratio of Six-Month Cessation, \nE-Cigarette Use vs. Non-E-Cigarette Use", font.main=1)

plot(colnames(sens.current.cig.ecig.increase.mat), 100*apply(sens.current.cig.ecig.increase.mat, 2, function(x) length(which(x>0))/length(x)), xlab="% Increase", ylab=NA, xaxt="n", yaxt="n", las=1, pch=NA, col="darkgrey", bty="l", ylim=c(0,100))
tmp <- lowess(colnames(sens.current.cig.ecig.increase.mat), 100*apply(sens.current.cig.ecig.increase.mat, 2, function(x) length(which(x>0))/length(x)), f=0.33)
grid(lty=1)
lines(tmp$x, tmp$y,lwd=2)
axis(1, at=seq(1,5.5,1), paste(seq(0,450,100)))
axis(2, at=seq(0,100,10),las=1)
title ("B. Current E-Cigarette Use Among Cigarette\nSmokers Who Have Tried to Quit in Past Year", font.main=1)

plot(colnames(sens.cig.init.mat), 100*apply(sens.cig.init.mat, 2, function(x) length(which(x>0))/length(x)), xlab="Odds Ratio", ylab=NA, xaxt="n", yaxt="n", las=1, pch=NA, col="darkgrey", bty="l", ylim=c(0,100))
tmp <- lowess(colnames(sens.cig.init.mat), 100*apply(sens.cig.init.mat, 2, function(x) length(which(x>0))/length(x)), f=0.2)
grid(lty=1)
lines(tmp$x, tmp$y,lwd=2)
axis(1, at=seq(0.5,3.5,0.5))
axis(2, at=seq(0,100,10),las=1)
title ("C. Adj. Odds Ratio of Cigarette Smoking Initiation,\nEver E-Cigarette Use vs. Never E-Cigarette Use", font.main=1)

plot(colnames(sens.ever.tried.cig.never.tried.cig.reduction.mat), 100*apply(sens.ever.tried.cig.never.tried.cig.reduction.mat, 2, function(x) length(which(x>0))/length(x)), xlab="% Reduction", ylab=NA, yaxt="n", xaxt="n", las=1, pch=NA, col="darkgrey", bty="l", ylim=c(0,100))
tmp <- lowess(colnames(sens.ever.tried.cig.never.tried.cig.reduction.mat), 100*apply(sens.ever.tried.cig.never.tried.cig.reduction.mat, 2, function(x) length(which(x>0))/length(x)), f=0.33)
grid(lty=1)
lines(tmp$x, tmp$y,lwd=2)
axis(1, at=seq(0,1,0.20), paste(seq(0,100,20)))
axis(2, at=seq(0,100,10),las=1)
title ("D. Ever E-Cigarette Use \nAmong Never Cigarette Smokers", font.main=1)

mtext("Prob. Positive Total # Yrs. of Life Gained",side=2,line=-22,outer=TRUE,at=c(1,3)/4,cex=1)
mtext("Prob. Positive Total # Yrs. of Life Gained",side=2,line=0,outer=TRUE,at=c(1,3)/4,cex=1)
dev.off()

scale <- 10^6
tmp <- as.data.frame(cbind(cut(harm.reduction, seq(0, 1, 0.10)), apply(sens.mat9[,4:6], 1, sum)))
names(tmp) <- c("harm.reduction", "life.years")
tmp2 <- by(tmp$life.years, tmp$harm.reduction, function(x) c(mean(x), quantile(x, probs=c(0.025, 0.975))))
ymin <- min(unlist(tmp2))
ymax <- 0
harm.reduction.list <- seq(5,100,10)
harm.reduction.list.labels <- c("0-10%", "10-20%",  "20-30%",  "30-40%",  "40-50%",  "50-60%",  "60-70%",  "70-80%",  "80-90%",  "90-100%")
pdf("figures/figure5.pdf",width=11.5,height=5.75)
par (mfrow=c(1,1),mgp=c(2.75,1,0)*0.55,mar=c(1.6,3.5,0.5,2)*1.6,omi=c(0.2,0.5,0.1,0), tcl=-0.25,bg="white", font=1, cex=cex.talk.value,cex.main=cex.talk.value)
plot(0,0,ylim=c(ymin,ymax),bty="n",axes=FALSE,xlab=NA,ylab=NA,pch=NA,xlim=c(0,max(harm.reduction.list)))
abline(h=seq(-3*10^6, 0, 2.5*10^5), col="grey", lty=1)
for (i in 1:length(tmp2)) {
  points(harm.reduction.list[i],tmp2[[i]][1],pch=19,cex=1.5)
  lines(rep(harm.reduction.list[i],2),tmp2[[i]][2:3], lwd=2)
}
axis(1, at=harm.reduction.list, paste(harm.reduction.list.labels), outer=TRUE, line=-2.5)
axis(2, las=1, at=seq(-3*10^6, 0, 2.5*10^5), paste(formatC(seq(-3*10^6, 0, 2.5*10^5), big.mark = ",", digits=0, format="f")), line=-0)
mtext("Total Number of Years of Life Gained",side=2,line=-0.0,outer=TRUE,at=1/2,cex=cex.talk.value)
mtext("Relative Harm of E-Cigarette Use Compared to Cigarette Smoking",side=1,line=-0.0,outer=TRUE,at=1/2,cex=cex.talk.value)
dev.off()

############################
#sensitivity analysis table#
############################
ff.fxn <- function(x)
  paste(formatC(mean(apply(x[,4:6],1,sum)),big.mark = ",", digits=0, format="f"), " (", formatC(quantile(apply(x[,4:6],1,sum),probs=0.025), big.mark = ",", digits=0, format="f"), " to ", formatC(quantile(apply(x[,4:6],1,sum),probs=0.975), big.mark = ",", digits=0, format="f"), ")", sep="")
ff.fxn2 <- function(x)
  paste(formatC(exp(x[1]), digits=2, format="f"), " (", formatC(exp(x[1]-qnorm(0.975)*x[2]), digits=2, format="f"), " to ", formatC(exp(x[1]+qnorm(0.975)*x[2]), digits=2, format="f"), ")", sep="")
ff.fxn3 <- function(x)
  paste(formatC((x[1]), digits=2, format="f"), " (", formatC((x[1]-qnorm(0.975)*x[2]), digits=2, format="f"), " to ", formatC((x[1]+qnorm(0.975)*x[2]), digits=2, format="f"), ")", sep="")
rr.to.or.fxn <- function(rr, p) (rr*(1-p))/(1-rr*p)
ff.fxn4 <- function(x, p)
  paste(formatC(rr.to.or.fxn(x[1], p), digits=2, format="f"), " (", formatC(rr.to.or.fxn(x[1]-qnorm(0.975)*x[2], p), digits=2, format="f"), " to ", formatC(rr.to.or.fxn(x[1]+qnorm(0.975)*x[2], p), digits=2, format="f"), ")", sep="")

sens.table <- rbind(c("Base Case", ff.fxn3(cig.quit.or.pt.est.se), ff.fxn(results.mat)),
                  c("Bullen et al.", ff.fxn4(cig.quit.or.pt.est.se.sens1, 17/295), ff.fxn(sens.mat1)),
                  c("Base Case", ff.fxn2(ecig.cig.or.pt.est.se), ff.fxn(results.mat)),
                  c("10% Reduction", ff.fxn2(ecig.cig.or.pt.est.se.sens3), ff.fxn(sens.mat3)),
                  c("20% Reduction", ff.fxn2(ecig.cig.or.pt.est.se.sens4), ff.fxn(sens.mat4)),
                  c("Base Case", "Age-Group Specific", ff.fxn(results.mat)),
                  c("10% Increase", "Age-Group Specific", ff.fxn(sens.mat5)),
                  c("20% Increase", "Age-Group Specific", ff.fxn(sens.mat6)),
                  c("Base Case", "Age Specific", ff.fxn(results.mat)),
                  c("10% Decrease", "Age Specific", ff.fxn(sens.mat7)),
                  c("20% Decrease", "Age Specific", ff.fxn(sens.mat8)))
colnames(sens.table) <- c("Scenario","Parameter Pt. Est. (95% CI)", "Years of Life Gained (95% CI)")  
rownames(sens.table) <- c("Relative Risk of Cigarette Smoking Cessation", NA, 
                          "Adjusted Odds Ratio of Cigarette Smoking Initiation", NA, NA, 
                          "Prevalence of Current E-Cigarette Use among Current Cigarette Smokers Who Tried to Quit Within the Past Year", NA, NA, 
                          "Prevalence of Ever E-Cigarette Use Among Never Cigarette Smokers", NA, NA)
write.csv(sens.table, file="tables/sens.table.csv")

####################
#abstract & results#
####################
N <- nrow(results.mat)
sink("text/results.txt")
print(paste(date()))
cat("\n")
print(paste("iterations:",formatC(N,big.mark = ",",format="d")))
cat("\n")
print(paste("adult quit.  mean of simulation:",formatC(apply(results.mat,2,mean)["adult.cig.quit.life.years"],digits=0,big.mark=",",format="d"),"and point estimate:",formatC(sum(apply(adult.quit.table2,1,prod)),digits=0,big.mark=",",format="d")))
print(paste("adult quit: ",round(100*(abs(apply(results.mat,2,mean)["adult.cig.quit.life.years"])-abs(sum(apply(adult.quit.table2,1,prod))))/abs(sum(apply(adult.quit.table2,1,prod))),2),"% difference between point estimate and mean of simulation",sep=""))
print(paste("adol int.  mean of simulation:",formatC(abs(apply(results.mat,2,mean)["adol.life.years"]),digits=0,big.mark=",",format="d"),"and point estimate:",formatC(abs(sum(apply(adol.table2,1,prod))),digits=0,big.mark=",",format="d")))
print(paste("adol init: ",round(100*((apply(results.mat,2,mean)["adol.life.years"]+sum(apply(adol.table2,1,prod)))/sum(apply(adol.table2,1,prod))),2),"% difference between point estimate and mean of simulation",sep=""))
print(paste("adol int.  mean of simulation:",formatC(abs(apply(results.mat,2,mean)["ya.life.years"]),digits=0,big.mark=",",format="d"),"and point estimate:",formatC(abs(sum(apply(ya.table2,1,prod))),digits=0,big.mark=",",format="d")))
print(paste("ya init: ",round(100*((apply(results.mat,2,mean)["ya.life.years"]+sum(apply(ya.table2,1,prod)))/sum(apply(ya.table2,1,prod))),2),"% difference between point estimate and mean of simulation",sep=""))
cat("\n")

print(paste("Results: The model estimated that ",
            formatC(1*mean(results.mat$longterm.quit),big.mark=",",format="d"),
            " additional current cigarette smoking adults aged 25-69 (95% CI: ",
            formatC(quantile(results.mat$longterm.quit,probs=0.025),big.mark=",",format="d"),
            " to ",
            formatC(quantile(results.mat$longterm.quit,probs=0.975),big.mark=",",format="d"),
            ") ",
            "would quit smoking in 2015 and remain continually abstinent from smoking for ≥7 years through the use of e-cigarettes in 2014.",
            "  The model also estimated ",
            formatC(mean(results.mat$adol.longterm.smokers + results.mat$ya.longterm.smokers),big.mark=",",format="d"),
            " additional never-cigarette smoking adolescents aged 12-17 and young adults aged 18-29 (95% CI: ",
            formatC(quantile(results.mat$adol.longterm.smokers + results.mat$ya.longterm.smokers,probs=0.025),big.mark=",",format="d"),
            " to ",
            formatC(quantile(results.mat$adol.longterm.smokers + results.mat$ya.longterm.smokers,probs=0.975),big.mark=",",format="d"),
            "), would initiate cigarette smoking in 2015 and eventually become daily cigarette smokers at age 35-39 through the use of e-cigarettes in 2014.  ",
            "Overall, the model estimated that e-cigarette use in 2014 would lead to ",
            formatC(-1*mean(apply(results.mat[,4:6],1,sum)),big.mark=",",format="d"),
            " years of life lost (95% CI: ",
            formatC(-1*quantile(apply(results.mat[,4:6],1,sum),probs=0.975),big.mark=",",format="d"),
            " to ",
            formatC(-1*quantile(apply(results.mat[,4:6],1,sum),probs=0.025),big.mark=",",format="d"),
            ").",
            sep=""))
cat("\n")

#########
#results#
#########
print(paste("In 2014, ",
            formatC(sum(popl5[adult.ages] * current.pt.est.se[adult.ages,2] * tried.quit.pt.est.se[adult.ages,2] * current.cig.ecig.pt.est.se[adult.ages,2]),big.mark = ",", format="d"),
            " current adult cigarette smokers who had attempted to quit smoking in the past year had also currently used e-cigarettes.  ",
            "Additionally, ",
            formatC(sum(popl[adol.ages]*never.tried.cig.pt.est.se[adol.ages,2]*ever.tried.ecig.never.tried.cig.pt.est.se[,2])+
                      sum(popl[ya.ages] * c(never.tried.cig.pt.est.se[as.character(18:26),2],rep(never.tried.cig.pt.est.se[as.character(26),2],3)) * never.cig.ecig.pt.est.se[,3]),big.mark = ",", format="d"),
            " never-cigarette smoking adolescents and young adults had ever used e-cigarettes.",sep=""))
cat("\n")

print(paste("The model estimated that ",
            formatC(mean(results.mat$longterm.quit),big.mark=",",format="d"),
            " additional current cigarette smoking adults (95% CI: ",
            formatC(quantile(results.mat$longterm.quit,probs=0.025),big.mark=",",format="d"),
            " to ",
            formatC(quantile(results.mat$longterm.quit,probs=0.975),big.mark=",",format="d"),
            ") who currently used e-cigarettes in 2014 would quit smoking in 2015 and remain continually abstinent from smoking for ≥7 years using e-cigarettes, compared to those who did not currently use e-cigarettes (Figure 2).  ",
            "The model also estimated that an additional ",
            formatC(mean(results.mat$adol.longterm.smokers + results.mat$ya.longterm.smokers),big.mark=",",format="d"),
            " never-cigarette smoking adolescents and young adults in 2014 (95% CI: ",
            formatC(quantile(results.mat$adol.longterm.smokers + results.mat$ya.longterm.smokers,probs=0.025),big.mark=",",format="d"),
            " to ",
            formatC(quantile(results.mat$adol.longterm.smokers + results.mat$ya.longterm.smokers,probs=0.975),big.mark=",",format="d"),
            ") who had ever used e-cigarettes would initiate cigarette smoking in 2015 and eventually become daily cigarette smokers at age 35-39, compared to those who had never used e-cigarettes.",
            sep=""))
cat("\n")

print(paste("The model estimated that the ",
            formatC(mean(results.mat$longterm.quit),big.mark=",",format="d"),
            " additional long-term quitters would gain ",
            formatC(mean(results.mat$adult.cig.quit.life.years),big.mark=",",format="d"),
            " years of life (95% CI: ",
            formatC(quantile(results.mat$adult.cig.quit.life.years,probs=0.025),big.mark=",",format="d"),
            " to ",
            formatC(quantile(results.mat$adult.cig.quit.life.years,probs=0.975),big.mark=",",format="d"),
            ").  ",
            "The model also estimated the additional ",
            formatC(mean(results.mat$adol.longterm.smokers + results.mat$ya.longterm.smokers),big.mark=",",format="d"),

            " adolescent and young adult cigarette smoking initiators who eventually become daily cigarette smokers at age 35-39 will lose ",
            formatC(-1*mean(results.mat$adol.life.years + results.mat$ya.life.years),big.mark=",",format="d"),
            " years of life (95% CI: ",
            formatC(-1*quantile(results.mat$adol.life.years+results.mat$ya.life.years,probs=0.975),big.mark=",",format="d"),
            " to ",
            formatC(-1*quantile(results.mat$adol.life.years+results.mat$ya.life.years,probs=0.025),big.mark=",",format="d"),
            ").  ",
            "Thus, considering all population subgroups, the model estimated that e-cigarette use in 2014 would lead to ",
            formatC(-1*mean(apply(results.mat[,4:6],1,sum)),big.mark=",",format="d"),
            " years of life lost (95% CI: ",
            formatC(-1*quantile(apply(results.mat[,4:6],1,sum),probs=0.975),big.mark=",",format="d"),
            " to ",
            formatC(-1*quantile(apply(results.mat[,4:6],1,sum),probs=0.025),big.mark=",",format="d"),
            "; Figure 3).",
            sep=""))

cat("\n")

print(paste("The model estimated that e-cigarette use in 2014 would lead to ",
            formatC(mean(apply(sens.mat1[,4:6],1,sum)),big.mark = ",", digits=0, format="f"), 
            " years of life lost (95% CI: ",
            formatC(quantile(apply(sens.mat1[,4:6],1,sum),probs=0.025), big.mark = ",", digits=0, format="f"), 
            " to ",
            formatC(quantile(apply(sens.mat1[,4:6],1,sum),probs=0.975), big.mark = ",", digits=0, format="f"), 
            ") and under the relative risk of smoking cessation estimated by Bullen et al. (Table 2).  ",  
            "Our results were sensitive to the adjusted odds of cigarette smoking initiation; the model estimated that e-cigarette use in 2014 would lead to ",
            formatC(mean(apply(sens.mat3[,4:6],1,sum)),big.mark = ",", digits=0, format="f"), 
            " years of life lost (95% CI: ",
            formatC(quantile(apply(sens.mat3[,4:6],1,sum),probs=0.025), big.mark = ",", digits=0, format="f"), 
            " to ",
            formatC(quantile(apply(sens.mat3[,4:6],1,sum),probs=0.975), big.mark = ",", digits=0, format="f"), 
            ") and ",
            formatC(mean(apply(sens.mat4[,4:6],1,sum)),big.mark = ",", digits=0, format="f"), 
            " years of life lost (95% CI: ",
            formatC(quantile(apply(sens.mat4[,4:6],1,sum),probs=0.025), big.mark = ",", digits=0, format="f"), 
            " to ",
            formatC(quantile(apply(sens.mat4[,4:6],1,sum),probs=0.975), big.mark = ",", digits=0, format="f"),
            ") if the adjusted odds ratio of cigarette smoking initiation decreased by 10% and 20%, respectively.  ",
            "Our results were also sensitive to the prevalence of current e-cigarette use among current cigarette smokers who tried quitting within the past year and ever e-cigarette use and never cigarette smokers.  ",
            "Finally, we varied the health risks of e-cigarette use as a percentage of the risk associated with cigarette smoking. The total number of years of life lost increased as the relative harm of e-cigarette use, compared to cigarette smoking, grew (Figure 5).  The model estimated that e-cigarette use in 2014 would lead to ",
            by(tmp$life.years, tmp$harm.reduction, function(x) paste(formatC(mean(x),big.mark = ",", digits=0, format="f")))[2],
            " years of life lost (95% CI: ", 
            by(tmp$life.years, tmp$harm.reduction, function(x) paste(formatC(quantile(x, probs=0.025),big.mark = ",", digits=0, format="f")))[2],
            " to ",
            by(tmp$life.years, tmp$harm.reduction, function(x) paste(formatC(quantile(x, probs=0.975),big.mark = ",", digits=0, format="f")))[2], 
            ") and ",
            by(tmp$life.years, tmp$harm.reduction, function(x) paste(formatC(mean(x),big.mark = ",", digits=0, format="f")))[5],
            " years of life lost (95% CI: ", 
            by(tmp$life.years, tmp$harm.reduction, function(x) paste(formatC(quantile(x, probs=0.025),big.mark = ",", digits=0, format="f")))[5],
            " to ",
            by(tmp$life.years, tmp$harm.reduction, function(x) paste(formatC(quantile(x, probs=0.975),big.mark = ",", digits=0, format="f")))[5], 
            ") if the health risks of e-cigarette use were 10%-20% (i.e., 80%-90% safer) and 40%-50% (i.e., 50%-60% safer) of the risks of cigarette smoking, respectively.",
            sep=""))

cat("\n")

print(paste("The probability of a positive total number of years of life gained increased with the relative risk of smoking cessation: ",
            formatC(100*apply(sens.cig.quit.mat, 2, function(x) length(which(x>0))/length(x))[which(colnames(sens.cig.quit.mat)=="2")], digits=1, format="f"),
            "%, ",
            formatC(100*apply(sens.cig.quit.mat, 2, function(x) length(which(x>0))/length(x))[which(colnames(sens.cig.quit.mat)=="2.5")], digits=1, format="f"),
            "%, and ",
            formatC(100*apply(sens.cig.quit.mat, 2, function(x) length(which(x>0))/length(x))[which(colnames(sens.cig.quit.mat)=="3")], digits=1, format="f"),
            "% as the relative risk increased to 2.0, 2.5, and 3.0, respectively (Figure 4, Panel A).  ",
          "The probability also increased with higher prevalence of current e-cigarette use among current cigarette smokers (Figure 4, Panel B).  ",
          "Conversely, the probability increased to ",
          formatC(100*apply(sens.cig.init.mat, 2, function(x) length(which(x>0))/length(x))[which(colnames(sens.cig.init.mat)=="3")], digits=1, format="f"),
          "%, ",
          formatC(100*apply(sens.cig.init.mat, 2, function(x) length(which(x>0))/length(x))[which(colnames(sens.cig.init.mat)=="2")], digits=1, format="f"),
          "%, and ",
          formatC(100*apply(sens.cig.init.mat, 2, function(x) length(which(x>0))/length(x))[which(colnames(sens.cig.init.mat)=="1")], digits=1, format="f"),
          "% as the adjusted odds ratio decreased to 3.0, 2.0, and 1.0, respectively (Figure 4, Panel C).  ",
          "Finally, the probability increased with lower prevalence of ever e-cigarette use among never cigarette smokers (Figure 4, Panel D).",
          sep=""))
sink()