library(scales)
library(ggplot2)
library(data.table)
library(xtable)

## ggplot theme for all graphs
theme.arg <-  theme(panel.background=element_blank(),
                    panel.border=element_rect(colour = "black", fill=NA),
                    axis.text=element_text(color="black"))
label.size <- 12

## analysis data
load(file="analysis-data.Rdata")
data <- data.table(data)

#######################################################################
##### Table A8. Yearly percentage of territory (by governorate) under control by each faction
#######################################################################
data$year <- as.factor(substr(data$date2w, 1,4))
control.agg <- data[, list(Government = round(mean(perc.Government), digits=2),
                           Opposition = round(mean(perc.Opposition), digits=2),
                           Kurds = round(mean(perc.Kurds), digits=2),
                           IS = round(mean(perc.IS), digits=2)), by=list(year)]

print(xtable(control.agg, caption="Yearly percentage of territory (by governorate) under control by each faction"), file="TableA8_control-by-year.tex", include.rownames=FALSE)



#######################################################################
##### Figure 2. Percentage of violence that is targeted, by type of control.
#######################################################################
viol.cntrl <- data[, list(untarg=sum(nhat.untarg), targ=sum(nhat.targ)), by=list(control)]

viol.cntrl[, perc.targ := targ/untarg*100]
viol.cntrl <- viol.cntrl[order(perc.targ),]
viol.cntrl$control <- factor(viol.cntrl$control, levels=c("Islamic State", "Opposition", "contested", "Kurds", "Government"))
plot <- ggplot(data=viol.cntrl) +
   geom_bar(aes(x=control, y=perc.targ), stat="identity",
            width=0.5, fill="grey") +
   geom_text(aes(x=control, y=perc.targ-2,
                 label=paste(round(perc.targ, digits=0), "%")),
             vjust=0) +
   coord_flip() +
   ylab("% targeted violence") + xlab("controlled by:") + theme.arg +
   theme(axis.text=element_text(size=label.size))
plot
ggsave(plot=plot, file="Figure2_perc-targ-by-control.pdf", width=4, height=3)


#######################################################################
##### Figure A5. Average percentage of government control, by governorate.
#######################################################################

### control by governorate
gov.control <- data[,list(mean.gvt.perc=mean(perc.Government)), by=list(gov)]
gov.control <- gov.control[order(mean.gvt.perc, decreasing=TRUE),]
gov.control$gov <- factor(gov.control$gov, levels=unique(gov.control$gov))

   
plot <- ggplot(data=gov.control) +
   geom_bar(aes(x=gov, y=mean.gvt.perc), stat="identity",
            width=0.5, fill="grey") +
   coord_flip() +
   ylab("% Government control (average)") + xlab("") + theme.arg+
   theme(axis.text=element_text(size=label.size))
plot
ggsave(plot=plot, file="Figurea5_perc-control-by-gov.pdf", width=5, height=4)


#######################################################################
##### Figure 1. Network (mobile phones, 3G, and 2G) accessibility by Syrian governorate, June 2013 - April 2015.
#######################################################################

## governorates with on average more than 80% govt control
fullgov <- as.character(gov.control$gov[gov.control$mean.gvt.perc>80])
## governorates with on average less than 20% govt control
nogov <- as.character(gov.control$gov[gov.control$mean.gvt.perc<20])


## Internet access by governorate:
full.access <- data[,list(Mobile.Phones, X3G, X2G.GPRS, date2w, gov)]

full.access <- melt(full.access, id=c("date2w","gov"))
full.access$control <- NA
full.access$control[full.access$gov %in% fullgov] <- 1
full.access$control[full.access$gov %in% nogov] <- 0
full.access <- full.access[]
access.plot <- ggplot() +
   facet_wrap(~ gov, scales="fixed", nrow=4, ncol=4) +
   # geom_rect(data=full.access[!is.na(control),], aes(fill=as.factor(control)), alpha=.02, xmin = -Inf,xmax = Inf,
   #           ymin = -Inf,ymax = Inf)+
   geom_line(data=full.access, aes(x=as.Date(date2w), y=value, linetype=factor(variable)), size=.4) +
   scale_x_date(breaks = date_breaks("3 months"),
                labels = date_format("%b-%y")) +
   scale_linetype_manual("", labels=c("Mobile Phones", "3G", "2G"), values = c(1,2,3)) +
   scale_fill_manual("",values=c("slategray1", "lightpink"), guide=FALSE)+
   scale_y_continuous(breaks=c(1,2,3,4), labels=c("none", "sporadic", "often", "full")) +
   ylab("") + xlab('') +
   theme.arg + theme(legend.position="bottom", 
                     legend.text=element_text(size=label.size),
                     axis.text.x=element_text(angle=90, hjust=.5, vjust=.5, size=label.size),
                     axis.text.y=element_text(size=label.size),
                     strip.text.x = element_text(size = 14))
access.plot
ggsave(file="Figure1_access-gov-internet.pdf", plot=access.plot, width=10, height=8)



#######################################################################
##### Figure A10. Targeted and untargeted violence, observed and estimated counts.
#######################################################################
sum.targ <- data[, list(
    Nk=sum(Nk.targ[!is.na(hi.targ)]),
    m0=sum(nhat.targ[!is.na(hi.targ)])-sum(Nk.targ[!is.na(hi.targ)]),
    hi=sum(hi.targ, na.rm=T),
    lo=sum(lo.targ, na.rm=T),
    type="targ")]


sum.untarg <- data[, list(
    Nk=sum(Nk.untarg[!is.na(hi.untarg)]),
    m0=sum(nhat.untarg[!is.na(hi.untarg)])-sum(Nk.untarg[!is.na(hi.untarg)]),
    hi=sum(hi.untarg, na.rm=T),
    lo=sum(lo.untarg, na.rm=T),
   type="untarg")]

### some information on reported and estimated killings, by type
# print(paste("number of over (observed + estimated) targeted killings:", (sum.targ$Nk + sum.targ$m0)))
# print(paste("number of over (observed + estimated) untargeted killings", (sum.untarg$Nk + sum.untarg$m0)))
# print(paste("percentage of targeted killings of overall killings - observed data",
#     round(sum.targ$Nk/(sum.targ$Nk+sum.untarg$Nk)*100, digits=2), "%"))
# print(paste("percentage of targeted killings of overall killings - observerd + estimated data", round((sum.targ$Nk+sum.targ$m0)/(sum.targ$Nk+sum.targ$m0+sum.untarg$Nk+sum.untarg$m0)*100, digits=2), "%"))
# print(paste("percentage of targeted killings that weren't reported'", round(sum.targ$m0 / (sum.targ$m0 + sum.targ$Nk)*100, digits=2), "%"))
# print(paste("percentage of untargeted killings that weren't reported'", round(sum.untarg$m0 / (sum.untarg$m0 + sum.untarg$Nk)*100, digits=2), "%"))

sum.df <- rbind(sum.targ, sum.untarg)
sum.df <- melt(sum.df, id=c("hi", "lo", "type"))
sum.df[, variable := factor(variable, levels=c("m0", "Nk"))]
sum.df <- sum.df[order(sum.df$type),]

plot <- ggplot(sum.df, aes(x=factor(type), y=value, fill=variable,
                      ymin=lo, ymax=hi)) +
     geom_bar(stat="identity", width=.5) +
     geom_errorbar(colour="black", width=.05, size=.2)+
         scale_fill_manual("",  values=c("darkgrey","lightgrey"),
                           labels=c("Estimated", "Observed")) +
                               scale_y_continuous(labels = comma) +
                                   scale_x_discrete("",labels=c("Targeted", "Untargeted"))+
    xlab("") + ylab("Classified Records") +
    theme.arg +
    theme(axis.ticks.x=element_blank(),
          axis.text=element_text(size=label.size),
          legend.text=element_text(size=label.size))
plot
ggsave(plot=plot, file="FigureA10_summary-rec-class_estimate.pdf", height=5, width=5)


#######################################################################
##### Figure A11. Targeted and untargeted violence, observed and estimated counts, over time.
#######################################################################
sum.targ <- data[, list(
    Nk=sum(Nk.targ[!is.na(hi.targ)]),
    m0=sum(nhat.targ[!is.na(hi.targ)])-sum(Nk.targ[!is.na(hi.targ)]),
    hi=sum(hi.targ, na.rm=T),
    lo=sum(lo.targ, na.rm=T),
    type="Targeted Killings"), by = list(date2w)]

sum.untarg <- data[, list(
    Nk=sum(Nk.untarg[!is.na(hi.untarg)]),
    m0=sum(nhat.untarg[!is.na(hi.untarg)])-sum(Nk.untarg[!is.na(hi.untarg)]),
    hi=sum(hi.untarg, na.rm=T),
    lo=sum(lo.untarg, na.rm=T),
    type="Untargeted Killings"), by = list(date2w)] 

sum.df <- rbind(sum.targ, sum.untarg)
sum.df <- melt(sum.df, id=c("hi", "lo", "type", "date2w"))
sum.df[, variable := factor(variable, levels=c("m0", "Nk"))]
sum.df <- sum.df[order(sum.df$type),]

plot <- ggplot(sum.df, aes(x=as.Date(date2w), y=value, fill=variable)) +
   geom_bar(stat="identity") +
   geom_errorbar(data=sum.df[variable=="m0",], aes(ymin=lo, ymax=hi), colour="black", width=.05, size=.2)+
   facet_wrap(~type, scales="free", nrow=2)+
         scale_fill_manual("",  values=c("darkgrey","lightgrey"),
                           labels=c("Estimated", "Observed")) +
   scale_x_date(breaks = date_breaks("2 months"),
                labels = date_format("%b-%y")) +
   scale_y_continuous(labels = comma) +
   xlab("") + ylab("") +
   theme.arg + theme(legend.position="bottom",
                     axis.text.y=element_text(size=label.size),
                     legend.text=element_text(size=label.size))

plot
ggsave(plot=plot, file="FigureA11_summary-rec-class_estimate-date2w.pdf", height=6, width=8)


#######################################################################
##### Figure A9. Number of words describing each individual record of killing
##### by classified type of violence and governorate.word count by viol type
#######################################################################
load("classified_data-xgboost.RData")
data$code <- data$xgboost.code

data <- data.table(data)
data[, num.words := vapply(strsplit(all.text, "\\W+"), length, integer(1))]

## summary(data[xgboost.label=="is_untargeted", num.words])
## summary(data[xgboost.label=="is_targeted", num.words])
plot.data <- data[xgboost.label %in% c("is_untargeted", "is_targeted"),
                  list(xgboost.label, num.words, governorate)]

plot <- ggplot() +
   geom_boxplot(data=plot.data, aes(x=xgboost.label, y=num.words), width=.4) +
   facet_wrap(~governorate)+
   xlab("Classified violence type") +
   ylab("Number of words per unique record of killing") +
   scale_x_discrete(label = c("Targeted", "Untargeted")) + 
  theme.arg + theme(strip.text.x = element_text(size = label.size))
plot
ggsave(plot=plot, file="FigureA9_words-by-violcat.pdf", width=6, height=6)


#######################################################################
##### Figure A8. Histogram of probabilities for each record being either targeted or untargeted.
##### Y-axis cut at 10,000.
#######################################################################

probs <- data[xgboost.label!= "is_other" ,
              list(xgboost.label, is_targeted)]

plot <- ggplot(probs) +
   geom_histogram(aes(x=is_targeted, fill=xgboost.label), binwidth=.01)+
   scale_fill_manual("Classified as", values=c("black", "grey"), labels=c("Targeted", "Untargeted"))+
   coord_cartesian(ylim=c(0,10000))+
   ylab("Individual records") + 
   xlab("Probability of incident being targeted (based on xgboost classifier)")+  
  theme.arg + theme(legend.text=element_text(size=label.size),
                    legend.position="bottom",
                    axis.text=element_text(size=label.size))
plot
ggsave(plot=plot, file="FigureA8_xgboot-probs.pdf")

## Descriptives mentioned in the appendix
## number of cases where the probability of being targeted is smaller than 95%, and the probability of not being targeted is more than 5% is 1981 records
# table(probs$is_targeted>0.05 & probs$is_targeted < .95)
## that is ~ 3.1%
# 1981 / nrow(probs)

## how many cases had no information at all? -- 162
# nrow(data[data$all.text==" ",])
## what were they assigned?
# table(data$code[data$all.text==" "]) # all untargeted
## 3695 records with only one word descriptor, majority of them where "shooting", also all coded as untargeted killings. when we have no further information, assume a shooting is not targeted.
# length(data$all.text[data$num.words==1])
# table(data$all.text[data$num.words==1])

