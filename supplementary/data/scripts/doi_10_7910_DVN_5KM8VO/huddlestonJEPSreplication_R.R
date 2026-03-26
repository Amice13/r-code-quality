sink(file = "huddlestonJEPSlogR.txt")

setwd("INSERT WORKING DIRECTORY HERE")
#setwd("C:/Users/Joey Huddleston/Dropbox/JEPS")


library(ggplot2)
library(foreign)
library(dplyr)
library(plyr)
library(scales)
library(grid)

#
#
# Begin code for Figure 1
# Effect of vignette on expected cost
# 
# Import data, use Oct. 2015 round, exclude Partial Intervention scenario
costt <- read.dta("JEPSr.dta", convert.factors = T)%>%  
  filter(!is.na(vignette) & round==1 & !is.na(cost) & vignette<4) %>% 
  dplyr::select(cost, vignette)
costt$vignette<-as.factor(costt$vignette)
levels(costt$vignette) <- c("Empty Threat", "Stay Out", "Follow Through")

# This function extracts mean and CIs from t-tests, by vignette
compare_means <- function(costt,cols_detail, col_to_eval){
  costf<- costt[, c(cols_detail, col_to_eval)]
  vignettecost <- ddply(costf
                        , .(vignette)
                        , function(x){
                          mean = t.test(x[,col_to_eval])$estimate
                          lower = t.test(x[,col_to_eval])$conf.int[1]
                          upper = t.test(x[,col_to_eval])$conf.int[2]
                          data.frame(mean, lower, upper)
                        }
  )
  return(vignettecost)
}
costt$cost <- as.numeric(costt$cost)

# This generates the dataframe for the plot code for Figure 1
vignettecost<-compare_means(costt, 'vignette', 'cost')  
vignettecost
vignettecost$vignette = factor(vignettecost$vignette,
                               levels(vignettecost$vignette)[c(2,1,3)])
# This produces the plot in Figure 1
costexpplot <- ggplot(data = vignettecost,aes(x=vignette,y=mean,ymin=lower,ymax=upper,
                                           fill=vignette))+
  geom_bar(stat="identity", position="dodge")+
  geom_errorbar(width = .1, position = position_dodge(.9))+
  scale_y_continuous(limits=c(0,7), breaks=1:7, labels=c("0-50", "50-99", "100-499 (3)", "500-1999 (4)",
                                                         "2000-4999 (5)", "5000-10000", "More than 10000"))+
  coord_cartesian(ylim=c(3,5))+
  scale_fill_manual(values=c("#cccc99", "darkolivegreen4", "#444422"))+
  theme_bw()+
  guides(fill=F)+
  theme(axis.line.x = element_line(color="black", size = .5),
        axis.line.y = element_line(color="black", size = .5),
        panel.border = element_rect(color="black", size = .5),
        panel.grid.major.x = element_blank(),
        legend.position="none")+
  geom_hline(yintercept=0)+
  geom_hline(data=subset(vignettecost, vignette=="Empty Threat"),
             aes(yintercept=upper), linetype=3,color="black",
             alpha=.2)+
  geom_hline(data=subset(vignettecost, vignette=="Empty Threat"),
             aes(yintercept=lower), linetype=3, color="black",
             alpha=.2)+
  ylab("Average Expected Casualty Level")+
  xlab("Vignette")
costexpplot
ggsave(filename ='costexp.png',plot = costexpplot,path = "figures",
       width = 4.5, height=3, units="in")

# 
# Begin code for Figure 2
#
# t.test and graph for casualty prime
#
# Import the data from the Stata file
# Use only October 2015 round
simpac <- read.dta("JEPSr.dta", convert.factors = F)
costnumstayemp<-simpac %>% filter(round==1 & !is.na(vignette) & !is.na(approve))
levels(costnumstayemp$vignette)<-c("1"="Empty Threat", "2"="Stay Out", "3"="Follow Through")
levels(costnumstayemp$vignette)
cost <- read.dta("costfirstgraph_r.dta", convert.factors = F)
# Separate into frames for each vignette, 2=stay out, 1=empty threat, 3=follow through
costs <- cost %>% filter(vignette==2)
coste <- cost %>% filter(vignette==1)
costf <- cost %>% filter(vignette==3)

# T-tests for stay, empty, and follow, resp.
coststaytest <- t.test(approve~costfirst, costs)
costemptest <- t.test(approve~costfirst, coste)
costfoltest <- t.test(approve~costfirst, costf)
#
# Statistics for Figure 2, and Table 2 in appendix
#
coststaytest 
costemptest 
costfoltest

print(coststaytest$estimate[1] - coststaytest$estimate[2])
# Extract statistics from t-test, put into 3 dataframes to be used in figure
# Specifically, this generates the text of the statistics for annotation in the figure
cs <- as.data.frame(as.list(cbind(coststaytest$p.value,
                                  coststaytest$estimate[2] - coststaytest$estimate[1])))
ce <- as.data.frame(as.list(cbind(costemptest$p.value,
                                  costemptest$estimate[2] - costemptest$estimate[1])))
cf <- as.data.frame(as.list(cbind(costfoltest$p.value,
                                  costfoltest$estimate[2] - costfoltest$estimate[1])))
colnames(cs)<- c("pvalue","diff")
colnames(ce)<- c("pvalue","diff")
colnames(cf)<- c("pvalue","diff")

#
# This function extracts the means and CIs from t-tests, by vignette
# 
compare_means <- function(costs,cols_detail, col_to_eval){
  test<- costs[, c(cols_detail, col_to_eval)]
costsplot <- ddply(test
                     , .(costfirst)
                     , function(x){
                       mean = t.test(x[,col_to_eval])$estimate
                       lower = t.test(x[,col_to_eval])$conf.int[1]
                       upper = t.test(x[,col_to_eval])$conf.int[2]
                       diff = t.test(x[,col_to_eval])$d
                       data.frame(mean, lower, upper)
                     }
  )
  return(costsplot)
}
# Create dataframe for each vignette
costsplot<-cbind(rbind(cs,cs),compare_means(costs, 'costfirst', 'approve'))  
costeplot<-cbind(rbind(ce,ce),compare_means(coste, 'costfirst', 'approve')) 
costfplot<-cbind(rbind(cf,cf),compare_means(costf, 'costfirst', 'approve'))
costsplot$vignette <- "Stay Out"
costeplot$vignette <- "Empty Threat"
costfplot$vignette <- "Follow Through"
# Merge dataframes into one
# This dataframe is used for the plot itself
costplot <- rbind(costeplot, costsplot, costfplot)
costplot$cost <- as.factor(costsplot$costfirst)
levels(costnumstayemp$vignette)<-c("0"="Approve \nFirst", "1"="Cost \nFirst")
costplot$vignette<-as.factor(costplot$vignette)
levels(costplot$vignette)
costplot$vignette = factor(costplot$vignette,levels(costplot$vignette)[c(1,3,2)])



# This generates the plot for Figure 2
costplotcompare <- ggplot(data = costplot,
                          aes(x=cost,y=mean,ymin=lower,ymax=upper,fill=cost))+
  geom_bar(stat="identity", position="dodge")+
  scale_y_continuous(limits=c(0,7), breaks=1:7, 
                     labels=c("Strong Disapprove (1)","Some Disapprove (2)", 
                              "Lean Disapprove (3)","Neutral (4)", "Lean Approve (5)",
                              "Approve (6)", "Strong Approve (7)"))+
  scale_x_discrete(labels=c("0"="Approve\nFirst", "1"="Casualties\nFirst"))+
  coord_cartesian(ylim=c(2,5.25))+
  scale_fill_manual(values=c("#cccc99", "darkolivegreen4"),
                    labels=c("Empty Threat", "Follow Through"))+
  theme_bw()+
  guides(fill=F)+
  theme(axis.line.x = element_line(color="black", size = .5),
        axis.line.y = element_line(color="black", size = .5),
        panel.border = element_rect(color="black", size = .5),
        panel.grid.major.x = element_blank(),
        strip.background = element_blank())+
  ylab("Average Approval Response")+
  xlab("Question Order")+
  geom_text(data=subset(costplot, costfirst==1 & vignette!="Follow Through"),
            aes(label=round(diff, digits=2), y=mean+.35, x=1.75, 
                family="Arial"), size=3)+
  geom_text(data=subset(costplot, costfirst==1 & vignette!="Follow Through"),
            aes(label="Effect:", y=mean+.35, x=1.75, family="Arial", hjust=1.58), size=3)+
  geom_text(data=subset(costplot, costfirst==1 & vignette!="Follow Through"),
            aes(label=round(pvalue, digits=2), y=mean+.15, x=1.75, family="Arial"), size=3)+
  geom_text(data=subset(costplot, costfirst==1 & vignette!="Follow Through"),
            aes(label="p:", y=mean+.15, x=1.75, family="Arial", hjust=3), size=3)+
  geom_text(data=subset(costplot, costfirst==0 & vignette=="Follow Through"),
            aes(label=round(diff, digits=2), y=mean+.35, x=1.75, family="Arial"), size=3)+
  geom_text(data=subset(costplot, costfirst==0 & vignette=="Follow Through"),
            aes(label="Effect:", y=mean+.35, x=1.75, family="Arial", hjust=1.58), size=3)+
  geom_text(data=subset(costplot, costfirst==0 & vignette=="Follow Through"),
            aes(label=round(pvalue, digits=2), y=mean+.15, x=1.75, family="Arial"), size=3)+
  geom_text(data=subset(costplot, costfirst==0 & vignette=="Follow Through"),
            aes(label="p:", y=mean+.15, x=1.75, family="Arial", hjust=3), size=3)+
  facet_wrap(~vignette, ncol=3)
costplotcompare
ggsave(filename ='costplot.png',plot = costplotcompare,path = "figures",
       width = 8, height=3, units="in")



#
# Begin code for Figure 3
#
# This takes the dataframe of predicted values from the Stata file
# and generates the plot in Figure 3
# 
# Import dataframe of predicted values from logit model
syrgraph<-read.dta("likertapprovepredictgraphcontrol.dta", convert.factors = F)
# Generate the plot for Figure 3
audplotcontrol <- ggplot(syrgraph, x=order())+ theme_bw()+
  geom_bar(aes(x=(order-.2), y=emp, fill="emp", color=NULL), stat="identity", width=.4)+
  geom_bar(aes(x=(order+.2), y=fol, fill="fol", color=NULL), stat="identity", width=.4)+
  xlab("\nPredicted Approval Rating") + ylab("Change with Casualties Prompt") +
  scale_x_continuous(breaks=c(1:7), labels=c("Strongly \n Disapprove", "Disapprove", 
                                             "Lean \n Disapprove", "Neutral", "Lean \n Approve", 
                                             "Approve", "Strongly \n Approve"))+
  scale_y_continuous(breaks=c(-.06,-.04,-.02, 0 , .02, .04), 
                     labels=c("-6%","-4%","-2%","0%","+2%","+4%"))+ 
  geom_hline(yintercept = c(0), color="#000000", size=.4)+
  scale_fill_manual(values=c("#cccc99", "darkolivegreen4"), 
                    labels=c("Empty Threat", "Follow Through"))+
  theme(axis.text.x = element_text(size=12, angle=0, vjust=.7),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=16, vjust=-.5),
        axis.title.y = element_text(size=16, vjust=.5),
        legend.position=c(.5,.2),
        legend.box="horizontal",
        legend.background=element_blank(),
        legend.key = element_blank()) +
  labs(fill="Treatment Group")
audplotcontrol
ggsave(filename ='audplotcontrol.png',plot = audplotcontrol, path = "figures",
       width = 8, height=5, units="in")


# APPENDIX
#
# Begin code for Figure 4 in appendix
# Effect of reputation prompt on approval
# Same basic code as that which produces Figure 2
#
# Import dataframe generated in Stata
rep <- read.dta("repfirstgraph_r.dta", convert.factors = F)
reps<- rep %>% filter(vignette==2)
repe <- rep %>% filter(vignette==1)

repstaytest <- t.test(approve~repfirst, reps)
repemptest <- t.test(approve~repfirst, repe)

#
# T-test for Figure 4 in appendix
#

repstaytest
repemptest
print(repstaytest$estimate[1] - repstaytest$estimate[2])
rs <- as.data.frame(as.list(cbind(repstaytest$p.value,
                                  repstaytest$estimate[2] - repstaytest$estimate[1])))
re <- as.data.frame(as.list(cbind(repemptest$p.value,
                                  repemptest$estimate[2] - repemptest$estimate[1])))
colnames(rs)<- c("pvalue","diff")
colnames(re)<- c("pvalue","diff")
table(reps$repfirst)
table(repe$repfirst)

#
# This function extracts the means and CIs from t-tests, by vignette
# 
compare_means <- function(repfirst,cols_detail, col_to_eval){
  test<- reps[, c(cols_detail, col_to_eval)]
  
  repdat <- ddply(test
                  , .(repfirst)
                  , function(x){
                    mean = t.test(x[,col_to_eval])$estimate
                    lower = t.test(x[,col_to_eval])$conf.int[1]
                    upper = t.test(x[,col_to_eval])$conf.int[2]
                    data.frame(mean, lower, upper)
                  }
  )
  return(repdat)
}
compare_means2 <- function(repfirst,cols_detail, col_to_eval){
  test<- repe[, c(cols_detail, col_to_eval)]
  repdat <- ddply(test
                  , .(repfirst)
                  , function(x){
                    mean = t.test(x[,col_to_eval])$estimate
                    lower = t.test(x[,col_to_eval])$conf.int[1]
                    upper = t.test(x[,col_to_eval])$conf.int[2]
                    data.frame(mean, lower, upper)
                  }
  )
  return(repdat)
}
#
# This combines all dataframes into one frame used for Figure 4
#
repsdata<-cbind(rbind(rs,rs),compare_means(reps, 'repfirst', 'approve'))
repedata<-cbind(rbind(re,re),compare_means2(repe, 'repfirst', 'approve'))
repsdata$vignette <- "Stay Out"
repedata$vignette <- "Empty Threat"

repdata <- rbind(repsdata, repedata)
repdata$repfirst <- as.factor(repdata$repfirst)

#
# This generates the plot for Figure 4 in appendix
#
repplot <- ggplot(data = repdata,
                  aes(x=repfirst,y=mean,ymin=lower,ymax=upper,fill=repfirst))+
  geom_bar(stat="identity", position="dodge")+
  scale_y_continuous(limits=c(0,7), breaks=1:7, 
                     labels=c("Strong Disapprove (1)","Some Disapprove (2)", 
                              "Lean Disapprove (3)","Neutral (4)", "Lean Approve (5)",
                              "Approve (6)", "Strong Approve (7)"))+
  scale_x_discrete(labels=c("0"="Approve\nFirst", "1"="Reputation\nFirst"))+
  coord_cartesian(ylim=c(2.5,5))+
  scale_fill_manual(values=c("#cccc99", "darkolivegreen4"),
                    labels=c("Empty Threat", "Follow Through"))+
  theme_bw()+
  guides(fill=F)+
  theme(axis.line.x = element_line(color="black", size = .5),
        axis.line.y = element_line(color="black", size = .5),
        panel.border = element_rect(color="black", size = .5),
        panel.grid.major.x = element_blank(),
        strip.background = element_blank())+
  ylab("Average Approval Response")+
  xlab("Question Order")+
  geom_text(data=subset(repdata, repfirst==0),
            aes(label=round(diff, digits=2), y=mean+.51, x=1.75, family="Arial"), size=3)+
  geom_text(data=subset(repdata, repfirst==0),
            aes(label="Effect:", y=mean+.51, x=1.75, family="Arial", hjust=1.58), size=3)+
  geom_text(data=subset(repdata, repfirst==0),
            aes(label=round(pvalue, digits=2), y=mean+.35, x=1.75, family="Arial"), size=3)+
  geom_text(data=subset(repdata, repfirst==0),
            aes(label="p:", y=mean+.35, x=1.75, family="Arial", hjust=3), size=3)+
  facet_wrap(~vignette, ncol=3)
repplot

ggsave(filename ='repplot.png',plot = repplot,path = "figures",
       width = 5, height=3, units="in")


