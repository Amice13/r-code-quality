# Andrew Healy, Katrina Kosec, and Cecilia Hyunjung Mo
# March 31, 2017
# Replication File for American Political Science Review
# To create Figure 2 and Figure A.1

library(ggplot2)
#Set Pathway
setwd("")

d <- read.table("figpov.txt",header=T)
d$pe <- d$c1
d$se <- d$c2
d$lb <- d$pe- 1.64*d$se
d$ub <- d$pe+ 1.64*d$se
d$group <- factor(d$c3,levels=c(5,4,3,2,1),
                  labels=c("Government Confidence Index","Overall Satisfaction with Government","Government Satisfaction with Government Community Services","Satisfaction with Government Security Services","Support of Political System")[5:1])

f1 = ggplot(data = d, aes(x = group, y = pe, width=0.5,ymin = lb, ymax = ub))
f1 = f1 + geom_pointrange(size=0.5, position = position_dodge(width=.5))
f1 = f1 + scale_shape_manual(values=c(22,22))  
f1 = f1 + scale_fill_manual(values=c("black","black"))
f1 = f1 + coord_flip()
f1 = f1 + geom_hline(yintercept=0, linetype="longdash", size=0.5)
f1 = f1 + theme(axis.text=element_text(size=8), axis.title=element_text(size=10,face="bold"))
f1 = f1 + ylab("Poverty Prime Treatment Effect") + xlab("Dependent Variable") 
print(f1)

d <- read.table("figinterac.txt",header=T)
d$pe <- d$c1
d$se <- d$c2
d$lb <- d$pe- 1.64*d$se
d$ub <- d$pe+ 1.64*d$se
d$group <- factor(d$c3,levels=c(5,4,3,2,1),
                  labels=c("Government Confidence Index","Overall Satisfaction with Government","Government Satisfaction with Government Community Services","Satisfaction with Government Security Services","Support of Political System")[5:1])

f1 = ggplot(data = d, aes(x = group, y = pe, width=0.5,ymin = lb, ymax = ub))
f1 = f1 + geom_pointrange(size=0.5, position = position_dodge(width=.5))
f1 = f1 + scale_shape_manual(values=c(22,22))  
f1 = f1 + scale_fill_manual(values=c("black","black"))
f1 = f1 + coord_flip()
f1 = f1 + geom_hline(yintercept=0, linetype="longdash", size=0.5)
f1 = f1 + theme(axis.text=element_text(size=8), axis.title=element_text(size=10,face="bold"))
f1 = f1 + ylab("Mobility + Poverty Primes X Aspiration Level") + xlab("Dependent Variable") 
print(f1)
