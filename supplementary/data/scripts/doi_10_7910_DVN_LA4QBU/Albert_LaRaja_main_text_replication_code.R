########################################################################################
# Replication code for "Who Should Decide the Party's Nominee?" (Party Politics, 2020)
# This file includes replication code for all analyses in the MAIN TEXT
########################################################################################
#If packages not installed, uncomment the following line and run before loading packages
#install.packages(c("ggplot2","gridExtra","nnet","effects","stargazer","reshape2"))
library(ggplot2)
library(gridExtra)
library(nnet)
library(effects)
library(stargazer)
library(reshape2)

###########
#Load Data#
###########
#set PATH to location of .RData file on your computer (uncomment and run to set working directory)
#setwd("PATH")
load("cces18_data.RData")

#Contains 2 objects:
#1) x: complete 2018 CCES data, including original survey items (unweighted)
#2) x.weight: complete 2018 CCES data, including original survey items (weighted)


#########################
#Create Figures & Tables#
#########################
#(follows order of presentation in main text)

###
#Figure 1: Distribution of Influence Points
#independent voters
a<-ggplot(x, aes(x=iv_points))+ geom_histogram( binwidth=5, color = "black",fill="grey") +
  geom_vline(aes(xintercept=mean(iv_points, na.rm=T)), color="red",linetype="longdash")+
  ggtitle("Independent Voters")   + ylim(0,650) + 
  xlab("")+ylab("Number of Respondents")
#party voters
b<-ggplot(x, aes(x=pv_points))+ geom_histogram( binwidth=5, color = "black",fill="grey") +
  geom_vline(aes(xintercept=mean(pv_points, na.rm=T)), color="red",linetype="longdash")+
  ggtitle("Registered Party Voters")   + ylim(0,650) +
  xlab("")+ylab("")
#party leaders
c<-ggplot(x, aes(x=pl_points))+ geom_histogram( binwidth=5, color = "black",fill="grey") +
  geom_vline(aes(xintercept=mean(pl_points, na.rm=T)), color="red",linetype="longdash")+
  ggtitle("Party Leaders")   + ylim(0,650) +
  xlab("Points")+ylab("Number of Respondents")
#nonparty experts
d<-ggplot(x, aes(x=ne_points))+ geom_histogram( binwidth=5, color = "black",fill="grey") +
  geom_vline(aes(xintercept=mean(ne_points, na.rm=T)), color="red",linetype="longdash")+
  ggtitle("Nonparty Experts")   + ylim(0,650) +
  xlab("Points")+ylab("")

#arrange on 2x2 grid:
grid.arrange(a,b,c,d, top=textGrob("Distribution of Points Allocated to...", gp=gpar(fontsize=14,font=3)), nrow=2)

#summary statistics:
summary(x$iv_points) #independent voters
summary(x$pv_points) #party voters
summary(x$pl_points) #party leaders
summary(x$ne_points) #nonparty experts


###
#Figure 2: Category sizes, all respondents and primary voters
#find weighted percentages for all respondents
all <- prop.table(svytable(~cluster, x.weight))*100

#combine with weighted percentages for primary voters only (prim_vote_model = 1)
prim_sub <- subset(x.weight, x.weight$variables$prim_vote_model==1)
prim <- prop.table(svytable(~cluster, prim_sub))*100

#create dataframe to plot
percdat <- data.frame(sample=c("All Respondents","All Respondents","All Respondents","Primary Voters","Primary Voters","Primary Voters"),
                      cluster=c("Elitist","Pluralist","Populist","Elitist","Pluralist","Populist"), perc = c(all, prim))
percdat

#plot
ggplot(percdat, aes(y=perc, x = cluster, fill=sample)) + labs(fill="")+ scale_fill_grey(start=0, end=.6)+
  geom_bar(position="dodge", stat="identity") + xlab("") + ylab("Percent") + theme(legend.position="bottom")


###
#Table 1: Political and demographic characteristics by cluster

#create empty dataframe
data <- data.frame(populist = NA, pluralist = NA, elitist = NA, total = NA)

#subset weighted data to populist, pluralist, elitist samples
pop <- subset(x.weight, x.weight$variables$cluster=="Populist")
plur <- subset(x.weight, x.weight$variables$cluster=="Pluralist")
eli <- subset(x.weight, x.weight$variables$cluster=="Elitist")

#Find percentages within each cluster (and run chi-squared test)
#Party ID
data[1:7,1] <- prop.table(svytable(~pid7, pop))*100
data[1:7,2] <- prop.table(svytable(~pid7, plur))*100
data[1:7,3] <- prop.table(svytable(~pid7, eli))*100
data[1:7,4] <- prop.table(svytable(~pid7, x.weight))*100

#chi-squared test
chisq.test(svytable(~pid7 + cluster, x.weight))

#Ideology
data[8:12,1] <- prop.table(svytable(~ideo5, pop))*100
data[8:12,2] <- prop.table(svytable(~ideo5, plur))*100
data[8:12,3] <- prop.table(svytable(~ideo5, eli))*100
data[8:12,4] <- prop.table(svytable(~ideo5, x.weight))*100

#chi-squared test
chisq.test(svytable(~ideo5 + cluster, x.weight))

#Gender (female)
data[13,1] <- prop.table(svytable(~gender, pop))[[1]]*100
data[13,2] <- prop.table(svytable(~gender, plur))[[1]]*100
data[13,3] <- prop.table(svytable(~gender, eli))[[1]]*100
data[13,4] <- prop.table(svytable(~gender, x.weight))[[1]]*100

#chi-squared test
chisq.test(svytable(~gender + cluster, x.weight))

#Race (white)
data[14,1] <- prop.table(svytable(~race, pop))[[8]]*100
data[14,2] <- prop.table(svytable(~race, plur))[[8]]*100
data[14,3] <- prop.table(svytable(~race, eli))[[8]]*100
data[14,4] <- prop.table(svytable(~race, x.weight))[[8]]*100

#chi-squared test
chisq.test(svytable(~race + cluster, x.weight))

#Mean Age
data[15,1] <- svymean(~age, pop)[[1]]
data[15,2] <- svymean(~age, plur)[[1]]
data[15,3] <- svymean(~age, eli)[[1]]
data[15,4] <- svymean(~age, x.weight)[[1]]

#chi-squared test
chisq.test(svytable(~age + cluster, x.weight))

#Primary Voter?
data[16,1] <- prop.table(svytable(~vote, pop))[[2]]*100
data[16,2] <- prop.table(svytable(~vote, plur))[[2]]*100
data[16,3] <- prop.table(svytable(~vote, eli))[[2]]*100
data[16,4] <- prop.table(svytable(~vote, x.weight))[[2]]*100

#chi-squared test
chisq.test(svytable(~vote + cluster, x.weight))

#General Voter?
data[17,1] <- prop.table(svytable(~gen_vote, pop))[[2]]*100
data[17,2] <- prop.table(svytable(~gen_vote, plur))[[2]]*100
data[17,3] <- prop.table(svytable(~gen_vote, eli))[[2]]*100
data[17,4] <- prop.table(svytable(~gen_vote, x.weight))[[2]]*100

#chi-squared test
chisq.test(svytable(~gen_vote + cluster, x.weight))

#Campaign Donor?
data[18,1] <- prop.table(svytable(~CC18_417a_6, pop))[[2]]*100
data[18,2] <- prop.table(svytable(~CC18_417a_6, plur))[[2]]*100
data[18,3] <- prop.table(svytable(~CC18_417a_6, eli))[[2]]*100
data[18,4] <- prop.table(svytable(~CC18_417a_6, x.weight))[[2]]*100

#chi-squared test
chisq.test(svytable(~CC18_417a_6 + cluster, x.weight))

#News Interest (follow some or most of the time)
data[19,1] <- prop.table(svytable(~newsint, pop))[[3]]*100 + prop.table(svytable(~newsint, pop))[[5]]*100
data[19,2] <- prop.table(svytable(~newsint, plur))[[3]]*100 + prop.table(svytable(~newsint, plur))[[5]]*100
data[19,3] <- prop.table(svytable(~newsint, eli))[[3]]*100 + prop.table(svytable(~newsint, eli))[[5]]*100
data[19,4] <- prop.table(svytable(~newsint, x.weight))[[3]]*100 + prop.table(svytable(~newsint, x.weight))[[5]]*100

#chi-squared test
chisq.test(svytable(~newsint + cluster, x.weight))

#Above Average Political Knowledge?
data[20,1] <- prop.table(svytable(~above_average_know, pop))[[2]]*100
data[20,2] <- prop.table(svytable(~above_average_know, plur))[[2]]*100
data[20,3] <- prop.table(svytable(~above_average_know, eli))[[2]]*100
data[20,4] <- prop.table(svytable(~above_average_know, x.weight))[[2]]*100

#chi-squared test
chisq.test(svytable(~above_average_know + cluster, x.weight))

#format final data
data <- data %>% mutate_if(is.numeric, round, 0) #round numbers
#add row names
rownames(data)<-c("Strong Democrat","Democrat","Democratic Leaner","Indepenent","Republican Leaner",
                  "Republican","Strong Republican","Very Liberal","Liberal","Moderate","Conservative",
                  "Very Conservative","Female","White","Age (Mean)","Primary Voter","General Voter","Campaign Donor",
                  "Follow News (Most or Some of the Time)","Above Average Knowledge")


###
#Table 2: Predicting support for primary influencers

#Model 1 (no party perception variables)
m1 <- multinom(cluster_reg ~ dem + rep + pid_strength_model + very_lib + lib + con + very_con  +
                   prim_vote_model + activist_score + interest + knowledge_full + educ_model+ female_model + minority_model + age + closed, data=x)
#print results
stargazer(m1, type="text", covariate.labels = c("Democrat","Republican","PID Strength","Very Liberal","Liberal","Conservative","Very Conservative",
                                                "Primary Voter","Activist Score","Political Interest","Political Knowledge","Education",
                                                "Female","Racial Minority","Age","Close Primary","Constant"))

#Model 2 (including party perception variables)
m2 <- multinom(cluster_reg ~ percept + percept*dem + percept*rep + dem + rep + pid_strength_model + very_lib + lib + con + very_con  +
                   prim_vote_model + activist_score + interest + knowledge_full + educ_model+ female_model + minority_model + age + closed, data=x)
#print results
stargazer(m2, type="text", covariate.labels = c("Party Perception", "Democrat","Republican","PID Strength","Very Liberal","Liberal","Conservative","Very Conservative",
                                                "Primary Voter","Activist Score","Political Interest","Political Knowledge","Education",
                                                "Female","Racial Minority","Age","Close Primary","Party Perception x Democrat", "Party Perception x Republican","Constant"))


###
#Figure 3: Predicted Probabilities
#NOTE: see "rr" and "dd" objects for precise predicted probability estimates (referenced in main text)
#Get interaction effects
eff<-allEffects(m2)

#Republicans
r<- as.data.frame(eff$`percept:rep`)
r <- subset(r, r$rep == 1)

rr<- melt(r[,c(1,3:5)],id.vars="percept") #get estimates
rrr <- melt(r[,c(1,9:11)],id.vars="percept") #get standard errors
rr$se <- rrr$value
rr$variable <- str_replace(rr$variable,"prob.","")
#plot
rplot<-ggplot(rr, aes(x=percept,y=value))+geom_line(aes(linetype=variable))+ggtitle("Republican Respondents")+
  geom_ribbon(data=rr,aes(ymin=value-se, ymax=value+se,linetype=variable),alpha=0.3,show.legend=F)+
  xlab("")+ylab("")+scale_fill_grey() +
  scale_x_continuous(limits =c(-4,5),breaks=c(-4,-3,-2,-1,0,1,2,3,4,5),labels=c("More\nExtreme","","","","Same\nIdeology","","","","","Less\nExtreme"))+
  labs(linetype="")

#Democrats
d<-as.data.frame(eff$`percept:dem`)
d <- subset(d, d$dem == 1)

dd<- melt(d[,c(1,3:5)],id.vars="percept") #get estimates
ddd <- melt(d[,c(1,9:11)],id.vars="percept") #get standard errors
dd$se <- ddd$value
dd$variable <- str_replace(dd$variable,"prob.","")

dplot<-ggplot(dd, aes(x=percept,y=value))+geom_line(aes(linetype=variable))+ggtitle("Democratic Respondents")+
  geom_ribbon(data=dd,aes(ymin=value-se, ymax=value+se,linetype=variable),alpha=0.3,show.legend=F)+
  xlab("")+ylab("")+scale_fill_grey() +
  scale_x_continuous(limits =c(-4,5),breaks=c(-4,-3,-2,-1,0,1,2,3,4,5),labels=c("More\nExtreme","","","","Same\nIdeology","","","","","Less\nExtreme"))+
  labs(linetype="")

#arrange
full_plot<-ggarrange(dplot,rplot, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
annotate_figure(full_plot,bottom=text_grob("Perception of Party Ideology Relative to Respondent Self-Placement",vjust=-4))


###
#Figure 4 - Political outcomes by cluster and party
#separate democrat/republican populists
dem_pop<-subset(pop, pop$variables$pid3=="Democrat"& pop$variables$prim_vote_model==1)
rep_pop<-subset(pop, pop$variables$pid3=="Republican" & pop$variables$prim_vote_model==1)

#separate democrat/republican pluralists
dem_plur<-subset(plur, plur$variables$pid3=="Democrat"& plur$variables$prim_vote_model==1)
rep_plur<-subset(plur, plur$variables$pid3=="Republican" & plur$variables$prim_vote_model==1)

#separate democrat/republican rank and file
dems <- subset(x.weight,x.weight$variables$pid3=="Democrat" & x.weight$variables$prim_vote_model==1)
reps <- subset(x.weight,x.weight$variables$pid3=="Republican" & x.weight$variables$prim_vote_model==1)

#create empty dataframe for attitudes variables
att <- data.frame(matrix(ncol=6,nrow=5, dimnames=list(c("Own Beliefs","Likely to Win","Prevent Leaders","Be Involved","Don't Compromise"),
                                                       c("Democratic Populists","Republican Populists","Democratic Pluralists","Republican Pluralists","Democrats","Republicans"))))

#Democratic Populist attitudes (% somewhat/strongly agreeing)
att[1,1] <- prop.table(svytable(~own_belief, dem_pop))[[4]]*100 + prop.table(svytable(~own_belief, dem_pop))[[5]]*100 #reflect own beliefs
att[2,1]<- prop.table(svytable(~likely_win, dem_pop))[[4]]*100 + prop.table(svytable(~likely_win, dem_pop))[[5]]*100 #likely to win
att[3,1]<- prop.table(svytable(~prevent_leaders, dem_pop))[[4]]*100 + prop.table(svytable(~prevent_leaders, dem_pop))[[5]]*100 #prevent leaders
att[4,1]<-prop.table(svytable(~be_involved, dem_pop))[[4]]*100 + prop.table(svytable(~be_involved, dem_pop))[[5]]*100 #be involved
att[5,1] <- prop.table(svytable(~UMA311b, dem_pop))[[4]]*100 + prop.table(svytable(~UMA311b, dem_pop))[[2]]*100 #should not compromise if it means going against beliefs

#Republican Populist attitudes (% somewhat/strongly agreeing)
att[1,2] <- prop.table(svytable(~own_belief, rep_pop))[[4]]*100 + prop.table(svytable(~own_belief, rep_pop))[[5]]*100 #reflect own beliefs
att[2,2]<- prop.table(svytable(~likely_win, rep_pop))[[4]]*100 + prop.table(svytable(~likely_win, rep_pop))[[5]]*100 #likely to win
att[3,2]<- prop.table(svytable(~prevent_leaders, rep_pop))[[4]]*100 + prop.table(svytable(~prevent_leaders, rep_pop))[[5]]*100 #prevent leaders
att[4,2]<-prop.table(svytable(~be_involved, rep_pop))[[4]]*100 + prop.table(svytable(~be_involved, rep_pop))[[5]]*100 #be involved
att[5,2] <- prop.table(svytable(~UMA311b, rep_pop))[[4]]*100 + prop.table(svytable(~UMA311b, rep_pop))[[2]]*100 #should not compromise if it means going against beliefs

#Democratic Pluralist attitudes (% somewhat/strongly agreeing)
att[1,3] <- prop.table(svytable(~own_belief, dem_plur))[[4]]*100 + prop.table(svytable(~own_belief, dem_plur))[[5]]*100 #reflect own beliefs
att[2,3]<- prop.table(svytable(~likely_win, dem_plur))[[4]]*100 + prop.table(svytable(~likely_win, dem_plur))[[5]]*100 #likely to win
att[3,3]<- prop.table(svytable(~prevent_leaders, dem_plur))[[4]]*100 + prop.table(svytable(~prevent_leaders, dem_plur))[[5]]*100 #prevent leaders
att[4,3]<-prop.table(svytable(~be_involved, dem_plur))[[4]]*100 + prop.table(svytable(~be_involved, dem_plur))[[5]]*100 #be involved
att[5,3] <- prop.table(svytable(~UMA311b, dem_plur))[[4]]*100 + prop.table(svytable(~UMA311b, dem_plur))[[2]]*100 #should not compromise if it means going against beliefs

#Republican Pluralist attitudes (% somewhat/strongly agreeing)
att[1,4] <- prop.table(svytable(~own_belief, rep_plur))[[4]]*100 + prop.table(svytable(~own_belief, rep_plur))[[5]]*100 #reflect own beliefs
att[2,4]<- prop.table(svytable(~likely_win, rep_plur))[[4]]*100 + prop.table(svytable(~likely_win, rep_plur))[[5]]*100 #likely to win
att[3,4]<- prop.table(svytable(~prevent_leaders, rep_plur))[[4]]*100 + prop.table(svytable(~prevent_leaders, rep_plur))[[5]]*100 #prevent leaders
att[4,4]<-prop.table(svytable(~be_involved, rep_plur))[[4]]*100 + prop.table(svytable(~be_involved, rep_plur))[[5]]*100 #be involved
att[5,4] <- prop.table(svytable(~UMA311b, rep_plur))[[4]]*100 + prop.table(svytable(~UMA311b, rep_plur))[[2]]*100 #should not compromise if it means going against beliefs

#Democratic Rank-and-File attitudes (% somewhat/strongly agreeing)
att[1,5] <- prop.table(svytable(~own_belief, dems))[[4]]*100 + prop.table(svytable(~own_belief, dems))[[5]]*100 #reflect own beliefs
att[2,5]<- prop.table(svytable(~likely_win, dems))[[4]]*100 + prop.table(svytable(~likely_win, dems))[[5]]*100 #likely to win
att[3,5]<- prop.table(svytable(~prevent_leaders, dems))[[4]]*100 + prop.table(svytable(~prevent_leaders, dems))[[5]]*100 #prevent leaders
att[4,5]<-prop.table(svytable(~be_involved, dems))[[4]]*100 + prop.table(svytable(~be_involved, dems))[[5]]*100 #be involved
att[5,5] <- prop.table(svytable(~UMA311b, dems))[[4]]*100 + prop.table(svytable(~UMA311b, dems))[[2]]*100 #should not compromise if it means going against beliefs

#Republican Rank-and-File attitudes (% somewhat/strongly agreeing)
att[1,6] <- prop.table(svytable(~own_belief, reps))[[4]]*100 + prop.table(svytable(~own_belief, reps))[[5]]*100 #reflect own beliefs
att[2,6]<- prop.table(svytable(~likely_win, reps))[[4]]*100 + prop.table(svytable(~likely_win, reps))[[5]]*100 #likely to win
att[3,6]<- prop.table(svytable(~prevent_leaders, reps))[[4]]*100 + prop.table(svytable(~prevent_leaders, reps))[[5]]*100 #prevent leaders
att[4,6]<-prop.table(svytable(~be_involved, reps))[[4]]*100 + prop.table(svytable(~be_involved, reps))[[5]]*100 #be involved
att[5,6] <- prop.table(svytable(~UMA311b, reps))[[4]]*100 + prop.table(svytable(~UMA311b, reps))[[2]]*100 #should not compromise if it means going against beliefs

#reformat for plotting
att$Var <- factor(rownames(att),levels=c("Own Beliefs","Likely to Win","Prevent Leaders","Be Involved","Don't Compromise"))
att2<-melt(att, id.vars="Var")
att2$party <- substr(att2$variable,1,3)
att2$party <- ifelse(att2$party=="Dem","Democrats","Republicans")
att2$cluster <- factor(att2$variable, levels=c("Democratic.Pluralists", "Republican.Pluralists","Democratic.Populists", "Republican.Populists","Democrats","Republicans") ,
                       labels=c("Pluralists","Pluralists","Populists","Populists","Rank-and-File","Rank-and-File"))

#plot
ggplot(att2, aes(y=as.numeric(as.character(value)), x = Var, fill=cluster)) + labs(fill="")+scale_fill_manual(values=c("grey72","grey59","grey36"))+ylim(0,100)+
  geom_bar(position="dodge", stat="identity") + facet_wrap(~party)+
  xlab("") + ylab("Percent Agreeing or Strongly Agreeing") + theme(legend.position="bottom", axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_text(position=position_dodge(width=1),aes(y=(as.numeric(as.character(value))+2.5), label=paste0("",round(as.numeric(as.character(value)),0),"%")), size=3)







