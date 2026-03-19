###################################################################################################################################################################################
# Script: Dual Citizenship Acceptance and Immigrant Naturalisation Propensity in the Netherlands: The Relevance of Origin- and Destination-Country Rules                          #
# Floris Peters & Maarten Vink                                                                                                                                                    #
# Published in R. Bauböck & M. Haller (Eds.), Dual citizenship and naturalisation: global, comparative and Austrian perspectives (pp. 121-140). Austrian Academy of Science Press #
###################################################################################################################################################################################


################
# Introduction #
################

# This file provides the syntax used to create all the tables and figures in the paper. 
# The dataset contains sensitive, micro level information. As such, for privacy reasons the data is only available to individuals employed at or affiliated to Statistics Netherlands. 
# The dataset can be found at the following location on the network of Statistics Netherlands: \\cbsp.nl\Productie\Projecten\SAL\209253UM_FP_SEC1\Werk\Floris\MiLifeStatus\AASP_dual_citizenship 

#######################################################################################################################
#######################################################################################################################

#############
# Variables #
#############

# ID
# (Individual identification number)

# YEAR
# (Observation year)

# START
# (Time vector - start of a given time period)

# STOP
# (Time vector - end of a given time period)

# EVENT
# (Acquiring Dutch citizenship)
# [0] The event does not occur during this time period; 
# [1] The event occurs at the end of this time period

# NATURALISED
# (Naturalised - time-varying)
# [0] Not naturalised; 
# [1] Naturalised

# DUALCIT
# (Dual citizenship toleration in the origin country)
# [0] Country does not exist on January 1 of reference year; 
# [110] Automatic loss of citizenship of origin country; 
# [111] Automatic loss of citizenship of origin country; State is Party to Chapter 1, Strasbourg  Convention;
# [112] Automatic loss of citizenship of origin country; State is Party to Chapter 1, Strasbourg Convention and Second Protocol; 
# [210] No automatic loss of citizenship of origin country, but renunciation of citizenship of origin country is possible;
# [211] No automatic loss of citizenship, but renunciation is possible; State is Party to Chapter 1, Strasbourg Convention; 
# [212] Same as 210, but including the Second Protocol; 
# [220] Same as 210, but automatic loss for persons who have acquired citizenship of origin country by naturalization; 
# [310] No automatic loss of citizenship of origin country, and renunciation of citizenship of origin country is not possible;
# [320] Same as 310, but automatic loss for persons who have acquired citizenship of origin country by naturalization; 
# [330] Same as 310, but renunciation is possible only for those who have acquired citizenship of origin by naturalization;
# [999] Rules on loss of citizenship of origin country are unknown

# DUALCITREC
# (Dual citizenship toleration in the origin country in categories)
# [0] automatic loss dual nationality; 
# [1] No automatic loss and can renounce; 
# [2] cannot renounce

# IMMIGRATIONYEAR
# (Year of first immigration to the Netherlands)

# GENDER
# [1] Male; 
# [2] Female

# AGEARRIVAL
# (Age at the moment of migration)

# AGECAT 
# (Age at the moment of migration in categories) 
# [1] 18-30 years; 
# [2] 31-40 years; 
# [3] 41-50 years; 
# [4] 51+ years

# PARTNER
# [1] No partner; 
# [2] Foreign born foreign partner; 
# [3] Foreign born Dutch partner; 
# [4] Native Dutch partner

# CHILD
# (Children in the household in categories)
# [0] no children <18 in household; 
# [1] Children <18 in household

# EDUCATION
# [0] missing; 
# [1] Low education; 
# [2] Middle education; 
# [3] High education

# DEVELOPMENT
# (Human Development Index (HDI) score origin country)

# DEVELOPMENTREC_M
# (Human Development Index (HDI) score origin country: low/high by median)
# [1] Low;
# [2] High

# EU
# [0] Not EU country of origin; 
# [1] EU country of origin

# REGION
# (Region of origin, categorical)
# [1] EU;
# [2] non-EU Europe, North America and Australia;
# [3] South America;
# [4] Africa;
# [5] Asia;
# [6] Middle-East

#######################################################################################################################
#######################################################################################################################

#load packages
library(ggplot2)
library(MASS)
library(dotwhisker)
library(dplyr)
library(lattice)
library(gridExtra)
library(grid)

##############     
# Figure 6.1 #
##############  

#year vector
year_6.1 <- rbind(1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,
              2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,
              2010,2011,2012,2013,2014,2015,2016,2017,2018)

#fee vector
rate_6.1 <- rbind(12216,28215,35450,42191,48804,70450,81528,58503,
              57854,60667,48232,45066,44080,27881,25259,27076,
              28277,29985,27729,29045,25452,27795,30250,25274,
              32071,27358,28077,27232,27421)

#combine in dataframe
dataset_6.1 <- data.frame(year_6.1,rate_6.1)

#plot fees over time
plot6.1 <- ggplot(data=dataset_6.1, aes(x=year_6.1, y=rate_6.1)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +
  scale_x_continuous(breaks=c(1990,1995,2000,2005,2010,2015)) +
  xlab("") +
  ylab("")
plot6.1


##############     
# Figure 6.3 #
############## 

#dual citizenship policy vector
y_6.3 <- rbind(31.60,49.42,18.98)

#labels vector
y_6.3_label <- rbind(49.42,18.98,31.60)

#legend vector
z_6.3 <- rbind("Automatic loss", "No automatic loss &\ncan renounce", "Cannot renounce")

#combine in dataframe
data_6.3 <- data.frame(y_6.3, z_6.3, y_6.3_label)
data_6.3 <- data_6.3 %>%
  arrange(desc(z_6.3)) %>%
  mutate(lab.ypos = cumsum(y_6.3) - 0.5*y_6.3)

#plot proportions citizenship policies
plot6.3 <- ggplot(data_6.3, aes(x = "", y = y_6.3, fill = z_6.3)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  geom_text(aes(y = lab.ypos), label = y_6.3_label) +
  theme_void() +
  theme(legend.title = element_blank())
plot6.3 <- plot6.3 + scale_fill_manual(values=c("gray35", "gray60", "gray85"))
plot6.3


##############     
# Figure 6.4 #
############## 

#model labels vector
term_6.4 <- rbind("Main model (Obs = 865,224)","EU origin country","EU (Obs = 221,785)","no EU (Obs = 643,439)",
              "HDI origin country","High HDI (Obs = 451,117)","Low HDI (Obs = 414,107)")

#coefficients vector
estimate_6.4 <- rbind(1.120,NA,1.235,1.086,NA,1.256,1.069)

#standard errors vector
std.error_6.4 <- rbind(0.012,NA,0.015,0.012,NA,0.013,0.010)

#combine in dataframe
dataset_6.4 <- data.frame(term_6.4,estimate_6.4,std.error_6.4)
dataset_6.4$term_6.4 <- factor(dataset_6.4$term_6.4, levels = unique(dataset_6.4$term_6.4))

#plot cox regression results
plot6.4 <- dwplot(dataset_6.4, dot_args = list(size = 3, colour = "black"), whisker_args = list(size = 1.5, colour = "black")) +
  geom_vline(xintercept = 1, colour = "grey60", linetype = 5, size = 1.05) +
  xlim(0.7,1.3) +
  theme_bw() +
  xlab("Impact does not lose or cannot renounce original citizenship for propensity to naturalise") + ylab("") +
  theme(axis.text.y = element_text(face = c('plain','plain', 'bold', 'plain', 'plain','bold','bold'),
  size = 10),
  legend.position = "none")
plot6.4


##############     
# Figure 6.5 #
############## 

#years since migration vector
term_6.5 <- rbind(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)

#hazard ratio vector
estimate_6.5 <- rbind(1.12,1.102863,1.09296,1.085988,1.08061,1.076236,1.072552,1.069371,1.066572,
                  1.064075,1.061822,1.059768,1.057883,1.056141,1.054521,1.053008,
                  1.051589,1.050252)

#upper confidence interval vector
upper_6.5 <- rbind(1.132,1.119022,1.111551,1.106305,1.102267,1.098987,1.096227,1.093847,
               1.091756,1.089891,1.088209,1.086678,1.085273,1.083975,1.082769,1.081643,
               1.080588,1.079595)

#lower confidence interval vector
lower_6.5 <- rbind(1.108,1.086704,1.074368,1.06567,1.058953,1.053486,1.048876,1.044894,1.041389,
               1.03826,1.035434,1.032859,1.030493,1.028306,1.026273,1.024372,1.022589,1.02091)

#combine in dataframe
dataset_6.5 <- data.frame(term_6.5,estimate_6.5,lower_6.5,upper_6.5)

#plot hazard ratio by years since migration
plot6.5 <- ggplot(data=dataset_6.5, aes(x=term_6.5, y=estimate_6.5)) + 
  geom_line(size = 1.3, colour = "black") +
  ylim(0.95,1.25) +
  theme_bw()+
  theme(legend.position = "none",
        axis.title.x = element_text(size = 13),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 13),
        axis.text.y = element_text(size = 10),
        panel.border = element_blank()) +
  geom_hline(yintercept = 1, colour = "grey60", size = 0.8) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)) +
  xlab("years since migration") + ylab("hazard ratio")
plot6.5 <- plot6.5 + geom_ribbon(aes(ymin=lower_6.5, ymax=upper_6.5),alpha=0.1, linetype = "dashed", 
             size = 1.3, colour = "black")
plot6.5


##############     
# Figure 6.6 #
############## 

#years since migration vector
term_6.6_1 <- rbind(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,
              1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)

#hazard ratio vector
estimate_6.6_1 <- rbind(1.086,1.074682,1.068117,1.063483,1.059902,1.056986,1.054526,1.0524,1.050528,
                  1.048857,1.047347,1.045971,1.044706,1.043537,1.042449,1.041433,1.040479,1.03958,
                  1.235,1.200545,1.180837,1.167051,1.156468,1.147893,1.140692,1.134491,1.129049,
                  1.124204,1.119838,1.115867,1.112227,1.1089,1.105749,1.10284,1.100114,1.09755)

#upper confidence interval vector
upper_6.6_1 <- rbind(1.097,1.090535,1.086807,1.084187,1.082168,1.080528,1.079147,1.077956,1.076909,
               1.075975,1.075132,1.074365,1.073661,1.07301,1.072405,1.071841,1.071311,1.070813,
               1.25,1.219704,1.202429,1.190368,1.181125,1.173643,1.167367,1.161968,1.157233,
               1.153019,1.149225,1.145777,1.142617,1.139702,1.136997,1.134475,1.132113,1.129892)

#lower confidence interval vector
lower_6.6_1 <- rbind(1.075,1.05883,1.049427,1.042779,1.037636,1.033443,1.029905,1.026844,1.024148,
               1.021739,1.019562,1.017576,1.015751,1.014063,1.012493,1.011025,1.009646,1.008348,
               1.22,1.181386,1.159245,1.143733,1.131811,1.122142,1.114016,1.107014,1.100866,
               1.095388,1.090451,1.085958,1.081838,1.078033,1.074501,1.071204,1.068114,1.065208)

#labels vector
group_var_6.6_1 <- rbind("non-EU / low-HDI","non-EU / low-HDI","non-EU / low-HDI","non-EU / low-HDI","non-EU / low-HDI",
                   "non-EU / low-HDI","non-EU / low-HDI","non-EU / low-HDI","non-EU / low-HDI",
                   "non-EU / low-HDI","non-EU / low-HDI","non-EU / low-HDI","non-EU / low-HDI","non-EU / low-HDI",
                   "non-EU / low-HDI","non-EU / low-HDI","non-EU / low-HDI","non-EU / low-HDI",
                   "EU / high-HDI","EU / high-HDI","EU / high-HDI","EU / high-HDI","EU / high-HDI","EU / high-HDI",
                   "EU / high-HDI","EU / high-HDI","EU / high-HDI",
                   "EU / high-HDI","EU / high-HDI","EU / high-HDI","EU / high-HDI","EU / high-HDI","EU / high-HDI",
                   "EU / high-HDI","EU / high-HDI","EU / high-HDI")

#combine in dataframe
dataset_6.6_1 <- data.frame(term_6.6_1,estimate_6.6_1,lower_6.6_1,upper_6.6_1,group_var_6.6_1)

#plot hazard ratio by years since migration
plot6.6_1 <- ggplot(data=dataset_6.6_1, aes(x=term_6.6_1, y=estimate_6.6_1, group = group_var_6.6_1))+ 
  geom_line(size = 1.3, linetype = c("dashed","dashed","dashed","dashed","dashed",
                                     "dashed","dashed","dashed","dashed",
                                     "dashed","dashed","dashed","dashed","dashed",
                                     "dashed","dashed","dashed","dashed",
                                     "solid","solid","solid","solid","solid",
                                     "solid","solid","solid","solid",
                                     "solid","solid","solid","solid","solid",
                                     "solid","solid","solid","solid"),
            aes(group=group_var_6.6_2, colour=group_var_6.6_1)) +
  ylim(0.95,1.3) +
  theme_bw()+
  theme(legend.position = "none",
        axis.title.x = element_text(size = 13),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 13),
        axis.text.y = element_text(size = 10),
        panel.border = element_blank()) +
  geom_hline(yintercept = 1, colour = "grey60", size = 0.8) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)) +
  xlab("years since migration") + ylab("hazard ratio")
plot6.6_1 <- plot_6.6_1 + scale_colour_grey(end = 0)
plot6.6_1 <- plot_6.6_1  + geom_ribbon(aes(ymin=lower, ymax=upper),alpha=0.1, 
                                linetype = c("dotted","dotted","dotted","dotted","dotted",
                                             "dotted","dotted","dotted","dotted",
                                             "dotted","dotted","dotted","dotted","dotted",
                                             "dotted","dotted","dotted","dotted",
                                             "twodash","twodash","twodash","twodash","twodash",
                                             "twodash","twodash","twodash","twodash",
                                             "twodash","twodash","twodash","twodash","twodash",
                                             "twodash","twodash","twodash","twodash"), 
                                size = 1.3,
                                colour=c("black","black","black","black","black",
                                         "black","black","black","black",
                                         "black","black","black","black","black",
                                         "black","black","black","black",
                                         "black","black","black","black","black",
                                         "black","black","black","black",
                                         "black","black","black","black","black",
                                         "black","black","black","black"))
plot6.6_1

#years since migration vector
term_6.6_2 <- rbind(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,
              1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)

#hazard ratio vector
estimate_6.6_2 <- rbind(1.069,1.063065,1.059608,1.057163,1.05527,1.053725,1.052422,1.051293,1.050299,
                  1.049411,1.048608,1.047875,1.047202,1.046578,1.045999,1.045457,1.044948,1.044468,
                  1.256,1.227123,1.21054,1.19891,1.189966,1.182708,1.176605,1.171345,1.166725,
                  1.162607,1.158894,1.155516,1.152416,1.1496,1.146895,1.144414,1.142088,1.1399)

#upper confidence interval vector
upper_6.6_2 <- rbind(1.08,1.078224,1.0772,1.076481,1.075926,1.075476,1.075097,1.07477,1.074483,1.074226,
               1.073995,1.073785,1.073591,1.073413,1.073247,1.073092,1.072947,1.07281,
               1.27,1.245975,1.23223,1.222614,1.215232,1.20925,1.204227,1.199901,1.196105,
               1.192725,1.18968,1.18691,1.184371,1.182027,1.179852,1.177822,1.175921,1.174133)

#lower confidence interval vector
lower_6.6_2 <- rbind(1.058,1.047906,1.042017,1.037845,1.034613,1.031975,1.029746,1.027817,1.026116,
               1.024595,1.02322,1.021966,1.020812,1.019744,1.01875,1.017821,1.016948,1.016126,
               1.242,1.208271,1.188849,1.175206,1.1647,1.156165,1.148984,1.142789,1.137344,
               1.132489,1.128109,1.124121,1.120461,1.11708,1.113939,1.111006,1.108256,1.105667)

#labels vector
group_var_6.6_2 <- rbind("non-EU / low-HDI","non-EU / low-HDI","non-EU / low-HDI","non-EU / low-HDI","non-EU / low-HDI",
                   "non-EU / low-HDI","non-EU / low-HDI","non-EU / low-HDI","non-EU / low-HDI",
                   "non-EU / low-HDI","non-EU / low-HDI","non-EU / low-HDI","non-EU / low-HDI","non-EU / low-HDI",
                   "non-EU / low-HDI","non-EU / low-HDI","non-EU / low-HDI","non-EU / low-HDI",
                   "EU / high-HDI","EU / high-HDI","EU / high-HDI","EU / high-HDI","EU / high-HDI","EU / high-HDI",
                   "EU / high-HDI","EU / high-HDI","EU / high-HDI",
                   "EU / high-HDI","EU / high-HDI","EU / high-HDI","EU / high-HDI","EU / high-HDI","EU / high-HDI",
                   "EU / high-HDI","EU / high-HDI","EU / high-HDI")

#combine in dataframe
dataset_6.6_2 <- data.frame(term_6.6_2,estimate_6.6_2,lower_6.6_2,upper_6.6_2,group_var_6.6_2)

#plot hazard ratio by years since migration
plot6.6_2 <- ggplot(data=dataset0, aes(x=term_6.6_2, y=estimate_6.6_2, group = group_var_6.6_2))+ 
  geom_line(size = 1.3, linetype = c("dashed","dashed","dashed","dashed","dashed",
                                     "dashed","dashed","dashed","dashed",
                                     "dashed","dashed","dashed","dashed","dashed",
                                     "dashed","dashed","dashed","dashed",
                                     "solid","solid","solid","solid","solid",
                                     "solid","solid","solid","solid",
                                     "solid","solid","solid","solid","solid",
                                     "solid","solid","solid","solid"),
            aes(group=group_var_6.6_2, colour=group_var_6.6_2)) +
  ylim(0.95,1.3) +
  theme_bw()+
  theme(legend.position = "none",
        axis.title.x = element_text(size = 13),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 13),
        axis.text.y = element_text(size = 10),
        panel.border = element_blank()) +
  geom_hline(yintercept = 1, colour = "grey60", size = 0.8) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)) +
  xlab("years since migration") + ylab("")
plot6.6_2 <- plot6.6_2 + scale_colour_grey(end = 0)
plot6.6_2 <- plot6.6_2 + geom_ribbon(aes(ymin=lower, ymax=upper),alpha=0.1, 
                               linetype = c("dotted","dotted","dotted","dotted","dotted",
                                            "dotted","dotted","dotted","dotted",
                                            "dotted","dotted","dotted","dotted","dotted",
                                            "dotted","dotted","dotted","dotted",
                                            "twodash","twodash","twodash","twodash","twodash",
                                            "twodash","twodash","twodash","twodash",
                                            "twodash","twodash","twodash","twodash","twodash",
                                            "twodash","twodash","twodash","twodash"), 
                               size = 1.3,
                               colour=c("black","black","black","black","black",
                                        "black","black","black","black",
                                        "black","black","black","black","black",
                                        "black","black","black","black",
                                        "black","black","black","black","black",
                                        "black","black","black","black",
                                        "black","black","black","black","black",
                                        "black","black","black","black"))

plot6.6_2

#combine plots
plot6.6 <- grid.arrange(plot6.6_1, plot6.6_2, ncol=2)
plot6.6


#############     
# Table 6.1 #
#############

#load dataset
dataset_main <- read.csv(file.choose(),header=T,sep=";")

#calculate percentages
prop.table(dataset_main$NATURALISED)
prop.table(dataset_main$GENDER)
prop.table(dataset_main$AGECAT)
prop.table(dataset_main$PARTNER)
prop.table(dataset_main$CHILD)
prop.table(dataset_main$EDUCATION)
prop.table(dataset_main$DUALCITREC)
prop.table(dataset_main$EU)
prop.table(dataset_main$REGION)
prop.table(dataset_main$DEVELOPMENTREC_M)
prop.table(dataset_main$IMMIGRATIONYEAR)

#number of observations/individuals
length(dataset_main$id)
length(unique(dataset_main$id))


#############     
# Table 6.2 #
#############

#load dataset
dataset_main <- read.csv(file.choose(),header=T,sep=";")

#create subsamples
dataset_EU <- subset(dataset_main, EU == 1)
dataset_nonEU <- subset(dataset_main, EU == 0)
dataset_highHDI <- subset(dataset_main, DEVELOPMENTREC_M == 2)
dataset_lowHDI <- subset(dataset_main, EU == 1)

#compute the survival functions
dataset_main$surv_main <- Surv(dataset_main$START, dataset_main$STOP, dataset_main$EVENT)
dataset_EU$surv_EU <- Surv(dataset_EU$START, dataset_EU$STOP, dataset_EU$EVENT)
dataset_nonEU$surv_nonEU <- Surv(dataset_nonEU$START, dataset_nonEU$STOP, dataset_nonEU$EVENT)
dataset_highHDI$surv_highHDI <- Surv(dataset_highHDI$START, dataset_highHDI$STOP, dataset_highHDI$EVENT)
dataset_lowHDI$surv_lowHDI <- Surv(dataset_lowHDI$START, dataset_lowHDI$STOP, dataset_lowHDI$EVENT)

#cox regression models
result_main <- coxph(surv_main ~ as.factor(DUALCITREC) + GENDER + as.factor(AGECAT) + as.factor(PARTNER) + CHILD + as.factor(EDUCATION) 
                     + EU + as.factor(REGION) + as.factor(IMMIGRATIONYEAR), data = dataset_main)
result_EU <- coxph(surv_EU ~ as.factor(DUALCITREC) + GENDER + as.factor(AGECAT) + as.factor(PARTNER) + CHILD + as.factor(EDUCATION) 
                     + as.factor(REGION) + as.factor(IMMIGRATIONYEAR), data = dataset_EU)
result_nonEU <- coxph(surv_nonEU ~ as.factor(DUALCITREC) + GENDER + as.factor(AGECAT) + as.factor(PARTNER) + CHILD + as.factor(EDUCATION) 
                     + as.factor(REGION) + as.factor(IMMIGRATIONYEAR), data = dataset_nonEU)
result_highHDI <- coxph(surv_highHDI ~ as.factor(DUALCITREC) + GENDER + as.factor(AGECAT) + as.factor(PARTNER) + CHILD + as.factor(EDUCATION) 
                     + EU + as.factor(REGION) + as.factor(IMMIGRATIONYEAR), data = dataset_highHDI)
result_lowHDI <- coxph(surv_lowHDI ~ as.factor(DUALCITREC) + GENDER + as.factor(AGECAT) + as.factor(PARTNER) + CHILD + as.factor(EDUCATION) 
                     + EU + as.factor(REGION) + as.factor(IMMIGRATIONYEAR), data = dataset_lowHDI)

