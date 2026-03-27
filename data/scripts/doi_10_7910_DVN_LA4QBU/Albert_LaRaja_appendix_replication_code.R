########################################################################################
# Replication code for "Who Should Decide the Party's Nominee?" (Party Politics, 2020)
# This file includes replication code for all analyses in the SUPPLEMENTAL APPENDIX
########################################################################################
#If packages not installed, uncomment the following line and run before loading packages
#install.packages(c("ggplot2","gridExtra","systemfit"))
library(ggplot2)
library(gridExtra)
library(systemfit)


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
#(follows order of presentation in SI)

###
#Figure A1: Points allocations to independents and registered partisans by respondents' state primary type
x$closed_primaries_cat <- ifelse(x$closed_primaries==1, "Closed Primaries", "Open Primaries")

#NOTE: to use state *presidential* primary rules, run this line and replace "closed_primaries_cat" with "closed_primaries_pres_cat" in lines 38-45
#closed_primaries_pres_cat <- ifelse(x$open_primaries_pres==1,"Open Primaries","Closed Primaries")

#Independents
a1<-ggplot(x, aes(x=iv_points, fill=closed_primaries_cat, group=closed_primaries_cat))+
  stat_bin(aes(y=..density..),position="dodge") + theme(legend.title = element_blank(), legend.position = "bottom", axis.title.x = element_text(size=10))+
  xlab('Points Allocated to "Independent Voters and Others\nWho Are Not Enrolled With A Major Party"')+ ylab("Density")

#Registered partisans
b1<-ggplot(x, aes(x=pv_points, fill=closed_primaries_cat, group=closed_primaries_cat))+
  stat_bin(aes(y=..density..),position="dodge") + theme(legend.title = element_blank(), legend.position = "bottom", axis.title.x = element_text(size=10))+
  xlab('Points Allocated to "Voters Who Register With\nThat Particular Party"')+ ylab("")

#plot
grid.arrange(a1,b1, top=textGrob("Point Allocations by Respondent's State Primary Type", gp=gpar(fontsize=14,font=3)), nrow=1)


#Wilcoxon ranked-sum (or Mann-Whitney) test to check if distributions differ
#Might expect those w/ closed primaries to be less supportive of independent voters (who are not allowed), more supportive of party voters
#Independent voters: testing whether x (open primaries) shifted to right of (i.e. more supportive/greater (alternative = "g") than) y (closed primaries)
wilcox.test(iv_points ~ closed_primaries, data=x, alternative = "g")
#p = 0.2733 -> we cannot reject the null hypothesis, that the population distributions of ind voters is greater for those in open primary states
#Party voters: testing whether x (open primaries) shifted to left of (i.e. less supportive/lower (alternative = "l") than) y (closed primaries)
wilcox.test(pv_points ~ closed_primaries, data=x, alternative= "l")
#p = 0.1728 -> we cannot reject the null hypothesis, that the population distributions of party voters is less for those in open primary states


###
#Figure A2 - Point Allocations by Respondent Party Identification
a<-ggplot(subset(x, x$pid3 %in% c("Democrat","Republican")), aes(x=UMA308a_1, fill=pid3, group=pid3))+ stat_bin(aes(y=..density..), binwidth=5, position="dodge") +
  geom_vline(aes(xintercept=aggregate(x$UMA308a_1 ~ x$pid3, FUN="mean")[5,2]), color="#F8766D",linetype="longdash")+
  geom_vline(aes(xintercept=aggregate(x$UMA308a_1 ~ x$pid3, FUN="mean")[1,2]), color="#00BFC4",linetype="longdash")+
  ggtitle("Independent Voters")   + scale_fill_discrete(name="") +
  xlab("")+ylab("Proportion of Respondents,\nby Party") + ylim(0,0.08)
b<-ggplot(subset(x, x$pid3 %in% c("Democrat","Republican")), aes(x=UMA308b_1, fill=pid3, group=pid3))+ stat_bin(aes(y=..density..), binwidth=5, position="dodge") +
  geom_vline(aes(xintercept=aggregate(x$UMA308b_1 ~ x$pid3, FUN="mean")[5,2]), color="#F8766D",linetype="longdash")+
  geom_vline(aes(xintercept=aggregate(x$UMA308b_1 ~ x$pid3, FUN="mean")[1,2]), color="#00BFC4",linetype="longdash")+
  ggtitle("Registered Party Voters")   + scale_fill_discrete(name="") +
  xlab("")+ylab("")+ ylim(0,0.08)
c<-ggplot(subset(x, x$pid3 %in% c("Democrat","Republican")), aes(x=UMA308c_1, fill=pid3, group=pid3))+ stat_bin(aes(y=..density..), binwidth=5, position="dodge") +
  geom_vline(aes(xintercept=aggregate(x$UMA308c_1 ~ x$pid3, FUN="mean")[5,2]), color="#F8766D",linetype="longdash")+
  geom_vline(aes(xintercept=aggregate(x$UMA308c_1 ~ x$pid3, FUN="mean")[1,2]), color="#00BFC4",linetype="longdash")+
  ggtitle("Party Leaders")   + scale_fill_discrete(name="") +
  xlab("Points")+ylab("Proportion of Respondents,\nby Party")+ ylim(0,0.08)
d<-ggplot(subset(x, x$pid3 %in% c("Democrat","Republican")), aes(x=UMA308d_1, fill=pid3, group=pid3))+ stat_bin(aes(y=..density..), binwidth=5, position="dodge") +
  geom_vline(aes(xintercept=aggregate(x$UMA308d_1 ~ x$pid3, FUN="mean")[5,2]), color="#F8766D",linetype="longdash")+
  geom_vline(aes(xintercept=aggregate(x$UMA308d_1 ~ x$pid3, FUN="mean")[1,2]), color="#00BFC4",linetype="longdash")+
  ggtitle("Nonparty Experts") + scale_fill_discrete(name="") +
  xlab("Points")+ylab("")+ ylim(0,0.08)

ggarrange(a, b, c, d, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")



###
#Table A1: Seemingly Unrelated Regression Analysis
attach(x)

#party voters
m1_pv <- pv_points ~ dem + rep +  pid_strength_model + very_lib + lib + con + very_con  +
  prim_vote_model + activist_score + interest + knowledge_full + educ_model+ female_model + minority_model + age + closed
#independent voters
m1_iv <- iv_points ~ dem + rep + pid_strength_model + very_lib + lib + con + very_con  +
  prim_vote_model + activist_score + interest + knowledge_full + educ_model+ female_model + minority_model + age + closed
#party leaders
m1_pl <- pl_points ~ dem + rep + pid_strength_model + very_lib + lib + con + very_con  +
  prim_vote_model + activist_score + interest + knowledge_full + educ_model+ female_model + minority_model + age + closed
#nonparty experts
m1_ne <- ne_points ~ dem + rep + pid_strength_model + very_lib + lib + con + very_con  +
  prim_vote_model + activist_score + interest + knowledge_full + educ_model+ female_model + minority_model + age + closed

#run SUR model
fitsur<- systemfit(list(pv = m1_pv, iv = m1_iv, pl = m1_pl, ne = m1_ne), data=x)

#print results
summary(fitsur)



