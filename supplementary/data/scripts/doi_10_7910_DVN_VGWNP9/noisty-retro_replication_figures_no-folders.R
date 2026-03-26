#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
##  TITLE: R script to create figures in paper and appendix
##  PROJECT: Noisy Retrospection: The Effect of Party Control on Policy Outcomes
##  AUTHORS: Adam M. Dynes and John B. Holbein
##  DATE: September 2, 2019
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################


#########################################################################################################
#########################################################################################################
##  NOTE: To run this, you must replace the following phrase in the script below with the folder 
##        directory where the folders and files are located on your computer.
##        
##        PHRASE TO REPLACE W/ DIRECTORY: "[INSERT WORKING DIRECTORY HERE]"
##
#########################################################################################################
#########################################################################################################





rm(list=ls())

library(ggplot2)
library(xlsx)
library(foreign)
library(rdd)


setwd("[INSERT WORKING DIRECTORY HERE")


####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
##
##
##     FIGURES IN PAPER, SEPARATE FROM APPENDIX FIGURES 
##
##
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################




## FIGURE 1
################## Bivariate Comparisons


bivariate<-read.dta("bivariate.dta")

attach(bivariate) 

plot1<-ggplot(bivariate, aes(y=coef, x=depvar)) + 
  geom_point(data=bivariate, colour="black", size=5) +
  geom_errorbar(aes(ymin=coef-1.96*stderr, ymax=coef+1.96*stderr), colour="black", width=0, size=2) +
  geom_errorbar(aes(ymin=coef-1.64*stderr, ymax=coef+1.64*stderr), colour="black", width=0, size=4) +
  facet_wrap(~var, nrow=2)+
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x="", y="Bivariate Estimate (std.)", title="") +
  theme(text = element_text(size=40)) + geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed") +
  theme(axis.text.x = element_text(angle=90, hjust = 1)) 
plot1
ggsave(plot1, file="bivariate.pdf", width=20, height=12, scale=2)

detach(bivariate)


## FIGURE 2
######################## Diff-in-Diff, Each Chamber Individually 


# 4 years downstream
## FIGURE 2
all3<-read.dta("unified_diff_diff_l_3_single_effects.dta")

attach(all3) 

pd <- position_dodge(width=0.62)  

color.names <- c("#000000", "#737373", "#bdbdbd")

plot1<-ggplot(all3, aes(y=coef,  x=depvar))+ 
  geom_hline(aes(yintercept=0.36), colour="grey8", linetype="dashed", size=2) +
  geom_hline(aes(yintercept=-0.36), colour="grey8", linetype="dashed", size=2) +  
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed", size=2) +
  geom_point(data=all3, size=6,  aes(colour = factor(Chamber)), position = pd) +
  geom_errorbar(data=all3, aes(ymin=coef-1.96*stderr, ymax= coef+1.96*stderr, colour = factor(Chamber)), position = pd, width=0, size=1) +
  geom_errorbar(data=all3, aes(ymin=coef-1.64*stderr, ymax= coef+1.64*stderr, colour = factor(Chamber)), position = pd, width=0, size=3)+
  facet_wrap(~type, scales = "free", nrow=2)  +
  scale_colour_manual(values=color.names) + theme_bw() + theme(panel.grid.major = element_blank(), 
                                                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.title = element_blank()) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  coord_flip() +
  labs(x="", y="Diff-Diff Effect of Democratic Control (std.)", title="") +
  theme(text = element_text(size=30)) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_y_continuous(limits=c(-0.4, 0.4)) +
  scale_x_discrete(breaks=c("birthrate","edattendrate","robrate", "newimmig", "nofelons", "hsdiploma", "res_energy_price", "bus_energy_consum_pc", 
                            "vepvotingrate", "vcrimerate", "healthspendpc", "housing_prices_quar", "real_pc_inc_quar", "gsppcap", "state_cpi_bfh", 
                            "unemployment", "co2emissions", "Top1_adj", "murderrate", "pop_growth", "propcrimerate", "valueofagsect", 
                            "divorcerate", "Top01_adj", "cartheftrate", "abortionrate", "raperate", "firms"),
                   labels=c("Births", "School Attend", "Robberies", "New Immigrants", "Felons Ineligible", "HS Grad", 
                            "Energy Price", "Energy Consume", "Turnout (VEP)", "Violent Crimes", "Health Spend", "Housing Prices", 
                            "Income", "GSP", "CPI", "Unemployment", "Co2 Emissions", "Income Top 1%", 
                            "Murders", "Pop. Growth", "Property Crimes", "Agriculture", "Divorces", "Income Top 0.1%", "Car Thefts", 
                            "Abortions", "Rapes", "Number of Businesses"))
plot1 
ggsave(plot1, file="diff_diff_all_t3.pdf", width=16, height=11, scale=2)

detach(all3)


## FIGURE 3
################# Unified Control: D vs R

diffdiffall<-read.dta("unified_diff_diff_t1_t3.dta")
attach(diffdiffall) 

color.names<-c("black", "grey70")

pd <- position_dodge(width=0.57)  

plot1<-ggplot(diffdiffall, aes(y=coef,  x=reorder(depvar, rank))) +
  geom_hline(aes(yintercept=0.36), colour="grey8", linetype="dashed", size=2) +
  geom_hline(aes(yintercept=-0.36), colour="grey8", linetype="dashed", size=2) +  
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed", size=2) +
  geom_point(data=diffdiffall, size=7,  aes(colour = factor(year)), position = pd) +
  geom_errorbar(data=diffdiffall, aes(ymin=coef-1.96*stderr, ymax= coef+1.96*stderr, colour = factor(year)), position = pd, width=0, size=1) +
  geom_errorbar(data=diffdiffall, aes(ymin=coef-1.64*stderr, ymax= coef+1.64*stderr, colour = factor(year)), position = pd, width=0, size=3) +
  scale_colour_manual(values=color.names) + theme_bw() +theme(panel.grid.major = element_blank(), 
                                                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  theme(legend.title = element_blank()) +
  labs(x="", y="Effect of Unified Democraticic Control (std.)", title="") +
  theme(text = element_text(size=30)) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_y_continuous(limits=c(-0.5, 0.5)) +
  scale_x_discrete(breaks=c("birthrate","edattendrate","robrate", "newimmig", "nofelons", "hsdiploma", "res_energy_price", "bus_energy_consum_pc", 
                            "vepvotingrate", "vcrimerate", "healthspendpc", "housing_prices_quar", "real_pc_inc_quar", "gsppcap", "state_cpi_bfh", 
                            "unemployment", "co2emissions", "Top1_adj", "murderrate", "pop_growth", "propcrimerate", "valueofagsect", 
                            "divorcerate", "Top01_adj", "cartheftrate", "abortionrate", "raperate", "firms"),
                   labels=c("Births", "School Attend", "Robberies", "New Immigrants", "Felons Ineligible", "HS Grad", 
                            "Energy Price", "Energy Consume", "Turnout (VEP)", "Violent Crimes", "Health Spend", "Housing Prices", 
                            "Income", "GSP", "CPI", "Unemployment", "Co2 Emissions", "Income Top 1%", 
                            "Murders", "Pop. Growth", "Property Crimes", "Agriculture", "Divorces", "Income Top 0.1%", "Car Thefts", 
                            "Abortions", "Rapes", "Number of Businesses"))

plot1 

ggsave(plot1, file="diffdifall_unified_t1_t3.pdf", width=10.5, height=6, scale=2)

detach(diffdiffall)


## FIGURE 4
################# Diff Diff--persistent control 
diffdiffall<-read.dta("unified_by_years_in_power_l_1_l_3.dta")
attach(diffdiffall) 

pd <- position_dodge(width=0.57)  

color.names<-c("black", "grey70")

pd <- position_dodge(width=0.57)  

plot1<-ggplot(diffdiffall, aes(y=coef,  x=reorder(depvar, rank))) + 
  geom_hline(aes(yintercept=0.36), colour="grey8", linetype="dashed", size=2) +
  geom_hline(aes(yintercept=-0.36), colour="grey8", linetype="dashed", size=2) +    
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed", size=2) +
  geom_point(data=diffdiffall, size=7,  aes(colour = factor(year)), position = pd) +
  geom_errorbar(data=diffdiffall, aes(ymin=coef-1.96*stderr, ymax= coef+1.96*stderr, colour = factor(year)), position = pd, width=0, size=1) +
  geom_errorbar(data=diffdiffall, aes(ymin=coef-1.64*stderr, ymax= coef+1.64*stderr, colour = factor(year)), position = pd, width=0, size=3) +
  scale_colour_manual(values=color.names) + theme_bw() +theme(panel.grid.major = element_blank(), 
                                                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  theme(legend.title = element_blank()) +
  labs(x="", y="Effect of Persistent Unified Democraticic Control (std.)", title="") +
  theme(text = element_text(size=30)) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_y_continuous(limits=c(-0.5, 0.5)) +
  scale_x_discrete(breaks=c("birthrate","edattendrate","robrate", "newimmig", "nofelons", "hsdiploma", "res_energy_price", "bus_energy_consum_pc", 
                            "vepvotingrate", "vcrimerate", "healthspendpc", "housing_prices_quar", "real_pc_inc_quar", "gsppcap", "state_cpi_bfh", 
                            "unemployment", "co2emissions", "Top1_adj", "murderrate", "pop_growth", "propcrimerate", "valueofagsect", 
                            "divorcerate", "Top01_adj", "cartheftrate", "abortionrate", "raperate", "firms"),
                   labels=c("Births", "School Attend", "Robberies", "New Immigrants", "Felons Ineligible", "HS Grad", 
                            "Energy Price", "Energy Consume", "Turnout (VEP)", "Violent Crimes", "Health Spend", "Housing Prices", 
                            "Income", "GSP", "CPI", "Unemployment", "Co2 Emissions", "Income Top 1%", 
                            "Murders", "Pop. Growth", "Property Crimes", "Agriculture", "Divorces", "Income Top 0.1%", "Car Thefts", 
                            "Abortions", "Rapes", "Number of Businesses"))

plot1 

ggsave(plot1, file="diffdifall_unified_persistent_way_2.pdf", width=10.5, height=8, scale=2)

detach(diffdiffall)

## FIGURE 5
##################### RDD + FE 


# 2 years downstream
all3<-read.dta("rdd_diffdiff_l_3_single_effects.dta")

attach(all3) 

pd <- position_dodge(width=0.62)  

color.names <- c("#000000", "#737373", "#bdbdbd")

plot1<-ggplot(all3, aes(y=coef,  x=depvar))+ 
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed", size=2) +
  geom_hline(aes(yintercept=0.36), colour="grey8", linetype="dashed", size=2) +
  geom_hline(aes(yintercept=-0.36), colour="grey8", linetype="dashed", size=2) +
  geom_point(data=all3, size=6,  aes(colour = factor(Chamber)), position = pd) +
  geom_errorbar(data=all3, aes(ymin=coef-1.96*stderr, ymax= coef+1.96*stderr, colour = factor(Chamber)), position = pd, width=0, size=1) +
  geom_errorbar(data=all3, aes(ymin=coef-1.64*stderr, ymax= coef+1.64*stderr, colour = factor(Chamber)), position = pd, width=0, size=3)+
  facet_wrap(~type, scales = "free", nrow=2)  +
  scale_colour_manual(values=color.names) + theme_bw() + theme(panel.grid.major = element_blank(), 
                                                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.title = element_blank()) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  coord_flip() +
  labs(x="", y="RDD Effect of Democratic Control (std.)", title="") +
  theme(text = element_text(size=30)) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_y_continuous(limits=c(-0.5, 0.5)) +
  scale_x_discrete(breaks=c("birthrate","edattendrate","robrate", "newimmig", "nofelons", "hsdiploma", "res_energy_price", "bus_energy_consum_pc", 
                            "vepvotingrate", "vcrimerate", "healthspendpc", "housing_prices_quar", "real_pc_inc_quar", "gsppcap", "state_cpi_bfh", 
                            "unemployment", "co2emissions", "Top1_adj", "murderrate", "pop_growth", "propcrimerate", "valueofagsect", 
                            "divorcerate", "Top01_adj", "cartheftrate", "abortionrate", "raperate", "firms"),
                   labels=c("Births", "School Attend", "Robberies", "New Immigrants", "Felons Ineligible", "HS Grad", 
                            "Energy Price", "Energy Consume", "Turnout (VEP)", "Violent Crimes", "Health Spend", "Housing Prices", 
                            "Income", "GSP", "CPI", "Unemployment", "Co2 Emissions", "Income Top 1%", 
                            "Murders", "Pop. Growth", "Property Crimes", "Agriculture", "Divorces", "Income Top 0.1%", "Car Thefts", 
                            "Abortions", "Rapes", "Number of Businesses"))
plot1 
ggsave(plot1, file="rdd_diffdiff_l_3_single_effects.pdf", width=16, height=11, scale=2)

detach(all3) 



####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
##
##
##     ALL FIGURES IN PAPER AND APPENDIX 
##
##
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################



## FIGURE 1 (IN PAPER)
################## Bivariate Comparisons




bivariate<-read.dta("bivariate.dta")

attach(bivariate) 

plot1<-ggplot(bivariate, aes(y=coef, x=depvar)) + 
  geom_point(data=bivariate, colour="black", size=5) +
  geom_errorbar(aes(ymin=coef-1.96*stderr, ymax=coef+1.96*stderr), colour="black", width=0, size=2) +
  geom_errorbar(aes(ymin=coef-1.64*stderr, ymax=coef+1.64*stderr), colour="black", width=0, size=4) +
  facet_wrap(~var, nrow=2)+
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x="", y="Bivariate Estimate (std.)", title="") +
  theme(text = element_text(size=40)) + geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed") +
  theme(axis.text.x = element_text(angle=90, hjust = 1)) 
plot1
ggsave(plot1, file="bivariate.pdf", width=20, height=12, scale=2)

detach(bivariate) 

################# Bivariate Comparisons, Alternative Specification
## FIGURE A20

bivariate<-read.dta("bivariate23chambers.dta")

attach(bivariate) 

plot1<-ggplot(bivariate, aes(y=coef, x=depvar)) + 
  geom_point(data=bivariate, colour="black", size=5) +
  geom_errorbar(aes(ymin=coef-1.96*stderr, ymax=coef+1.96*stderr), colour="black", width=0, size=2) +
  geom_errorbar(aes(ymin=coef-1.64*stderr, ymax=coef+1.64*stderr), colour="black", width=0, size=4) +
  facet_wrap(~var, nrow=3)+
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x="", y="Bivariate Estimate (std.)", title="") +
  theme(text = element_text(size=40)) + geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed") +
  theme(axis.text.x = element_text(angle=90, hjust = 1)) 
plot1
ggsave(plot1, file="bivariate23chambers.pdf", width=12, height=12, scale=2)

detach(bivariate)

##########################################################################################  
##########################################################################################  
########################################################################################## 
##########################################################################################  
##########################################################################################  
##########################################################################################   
##
## Diff Diff Graphs
##
##########################################################################################  
##########################################################################################  
##########################################################################################  
##########################################################################################  
##########################################################################################  
########################################################################################## 





##########################################################################################  
##########################################################################################  
######################### Two Way FE   ################################################### 
##########################################################################################  
##########################################################################################  
########################################################################################## 




######### Lag
all3<-read.dta("unified_diff_diff_two_way_lag.dta")

attach(all3) 

pd <- position_dodge(width=0.62)  

color.names <- c("#000000", "#737373", "#bdbdbd")

plot1<-ggplot(all3, aes(y=coef,  x=depvar))+ 
  geom_hline(aes(yintercept=0.36), colour="grey8", linetype="dashed", size=2) +
  geom_hline(aes(yintercept=-0.36), colour="grey8", linetype="dashed", size=2) +
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed", size=2) +
  geom_point(data=all3, size=6,  aes(colour = factor(Chamber)), position = pd) +
  geom_errorbar(data=all3, aes(ymin=coef-1.96*stderr, ymax= coef+1.96*stderr, colour = factor(Chamber)), position = pd, width=0, size=1) +
  geom_errorbar(data=all3, aes(ymin=coef-1.64*stderr, ymax= coef+1.64*stderr, colour = factor(Chamber)), position = pd, width=0, size=3)+
  facet_wrap(~type, scales = "free", nrow=2)  +
  scale_colour_manual(values=color.names) + theme_bw() + theme(panel.grid.major = element_blank(), 
                                                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.title = element_blank()) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  coord_flip() +
  labs(x="", y="Diff-Diff Effect of Democratic Control (std.)", title="") +
  theme(text = element_text(size=30)) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_y_continuous(limits=c(-0.7, 0.7)) +
  scale_x_discrete(breaks=c("birthrate","edattendrate","robrate", "newimmig", "nofelons", "hsdiploma", "res_energy_price", "bus_energy_consum_pc", 
                            "vepvotingrate", "vcrimerate", "healthspendpc", "housing_prices_quar", "real_pc_inc_quar", "gsppcap", "state_cpi_bfh", 
                            "unemployment", "co2emissions", "Top1_adj", "murderrate", "pop_growth", "propcrimerate", "valueofagsect", 
                            "divorcerate", "Top01_adj", "cartheftrate", "abortionrate", "raperate", "firms"),
                   labels=c("Births", "School Attend", "Robberies", "New Immigrants", "Felons Ineligible", "HS Grad", 
                            "Energy Price", "Energy Consume", "Turnout (VEP)", "Violent Crimes", "Health Spend", "Housing Prices", 
                            "Income", "GSP", "CPI", "Unemployment", "Co2 Emissions", "Income Top 1%", 
                            "Murders", "Pop. Growth", "Property Crimes", "Agriculture", "Divorces", "Income Top 0.1%", "Car Thefts", 
                            "Abortions", "Rapes", "Number of Businesses"))
plot1 
ggsave(plot1, file="diff_diff_all_twoway_lag.pdf", width=16, height=11, scale=2)

detach(all3) 


######### 2 years
all3<-read.dta("unified_diff_diff_l_1_single_effects_two_way.dta")

attach(all3) 

pd <- position_dodge(width=0.62)  

color.names <- c("#000000", "#737373", "#bdbdbd")

plot1<-ggplot(all3, aes(y=coef,  x=depvar))+ 
  geom_hline(aes(yintercept=0.36), colour="grey8", linetype="dashed", size=2) +
  geom_hline(aes(yintercept=-0.36), colour="grey8", linetype="dashed", size=2) +
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed", size=2) +
  geom_point(data=all3, size=6,  aes(colour = factor(Chamber)), position = pd) +
  geom_errorbar(data=all3, aes(ymin=coef-1.96*stderr, ymax= coef+1.96*stderr, colour = factor(Chamber)), position = pd, width=0, size=1) +
  geom_errorbar(data=all3, aes(ymin=coef-1.64*stderr, ymax= coef+1.64*stderr, colour = factor(Chamber)), position = pd, width=0, size=3)+
  facet_wrap(~type, scales = "free", nrow=2)  +
  scale_colour_manual(values=color.names) + theme_bw() + theme(panel.grid.major = element_blank(), 
                                                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.title = element_blank()) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  coord_flip() +
  labs(x="", y="Diff-Diff Effect of Democratic Control (std.)", title="") +
  theme(text = element_text(size=30)) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_y_continuous(limits=c(-0.7, 0.7)) +
  scale_x_discrete(breaks=c("birthrate","edattendrate","robrate", "newimmig", "nofelons", "hsdiploma", "res_energy_price", "bus_energy_consum_pc", 
                            "vepvotingrate", "vcrimerate", "healthspendpc", "housing_prices_quar", "real_pc_inc_quar", "gsppcap", "state_cpi_bfh", 
                            "unemployment", "co2emissions", "Top1_adj", "murderrate", "pop_growth", "propcrimerate", "valueofagsect", 
                            "divorcerate", "Top01_adj", "cartheftrate", "abortionrate", "raperate", "firms"),
                   labels=c("Births", "School Attend", "Robberies", "New Immigrants", "Felons Ineligible", "HS Grad", 
                            "Energy Price", "Energy Consume", "Turnout (VEP)", "Violent Crimes", "Health Spend", "Housing Prices", 
                            "Income", "GSP", "CPI", "Unemployment", "Co2 Emissions", "Income Top 1%", 
                            "Murders", "Pop. Growth", "Property Crimes", "Agriculture", "Divorces", "Income Top 0.1%", "Car Thefts", 
                            "Abortions", "Rapes", "Number of Businesses"))
plot1 
ggsave(plot1, file="diff_diff_all_twoway_l_1.pdf", width=16, height=11, scale=2)

detach(all3) 


######### 4 years
all3<-read.dta("unified_diff_diff_l_3_single_effects_two_way.dta")

attach(all3) 

pd <- position_dodge(width=0.62)  

color.names <- c("#000000", "#737373", "#bdbdbd")

plot1<-ggplot(all3, aes(y=coef,  x=depvar))+ 
  geom_hline(aes(yintercept=0.36), colour="grey8", linetype="dashed", size=2) +
  geom_hline(aes(yintercept=-0.36), colour="grey8", linetype="dashed", size=2) +
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed", size=2) +
  geom_point(data=all3, size=6,  aes(colour = factor(Chamber)), position = pd) +
  geom_errorbar(data=all3, aes(ymin=coef-1.96*stderr, ymax= coef+1.96*stderr, colour = factor(Chamber)), position = pd, width=0, size=1) +
  geom_errorbar(data=all3, aes(ymin=coef-1.64*stderr, ymax= coef+1.64*stderr, colour = factor(Chamber)), position = pd, width=0, size=3)+
  facet_wrap(~type, scales = "free", nrow=2)  +
  scale_colour_manual(values=color.names) + theme_bw() + theme(panel.grid.major = element_blank(), 
                                                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.title = element_blank()) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  coord_flip() +
  labs(x="", y="Diff-Diff Effect of Democratic Control (std.)", title="") +
  theme(text = element_text(size=30)) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_y_continuous(limits=c(-0.7, 0.7)) +
  scale_x_discrete(breaks=c("birthrate","edattendrate","robrate", "newimmig", "nofelons", "hsdiploma", "res_energy_price", "bus_energy_consum_pc", 
                            "vepvotingrate", "vcrimerate", "healthspendpc", "housing_prices_quar", "real_pc_inc_quar", "gsppcap", "state_cpi_bfh", 
                            "unemployment", "co2emissions", "Top1_adj", "murderrate", "pop_growth", "propcrimerate", "valueofagsect", 
                            "divorcerate", "Top01_adj", "cartheftrate", "abortionrate", "raperate", "firms"),
                   labels=c("Births", "School Attend", "Robberies", "New Immigrants", "Felons Ineligible", "HS Grad", 
                            "Energy Price", "Energy Consume", "Turnout (VEP)", "Violent Crimes", "Health Spend", "Housing Prices", 
                            "Income", "GSP", "CPI", "Unemployment", "Co2 Emissions", "Income Top 1%", 
                            "Murders", "Pop. Growth", "Property Crimes", "Agriculture", "Divorces", "Income Top 0.1%", "Car Thefts", 
                            "Abortions", "Rapes", "Number of Businesses"))
plot1 
ggsave(plot1, file="diff_diff_all_twoway_l_3.pdf", width=16, height=11, scale=2)

detach(all3) 




##########################################################################################  
##########################################################################################  
##########################################################################################   
######################### WITH STATE SPECIFIC TIME TRENDS   ############################## 
##########################################################################################  
##########################################################################################  
########################################################################################## 




######################## Each Chamber Individually Lag Treatment #######################

all3<-read.dta("unified_diff_diff_lag.dta")

attach(all3) 

pd <- position_dodge(width=0.62)  

color.names <- c("#000000", "#737373", "#bdbdbd")

plot1<-ggplot(all3, aes(y=coef,  x=depvar))+ 
  geom_hline(aes(yintercept=0.36), colour="grey8", linetype="dashed", size=2) +
  geom_hline(aes(yintercept=-0.36), colour="grey8", linetype="dashed", size=2) +
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed", size=2) +
  geom_point(data=all3, size=6,  aes(colour = factor(Chamber)), position = pd) +
  geom_errorbar(data=all3, aes(ymin=coef-1.96*stderr, ymax= coef+1.96*stderr, colour = factor(Chamber)), position = pd, width=0, size=1) +
  geom_errorbar(data=all3, aes(ymin=coef-1.64*stderr, ymax= coef+1.64*stderr, colour = factor(Chamber)), position = pd, width=0, size=3)+
  facet_wrap(~type, scales = "free", nrow=2)  +
  scale_colour_manual(values=color.names) + theme_bw() + theme(panel.grid.major = element_blank(), 
                                                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.title = element_blank()) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  coord_flip() +
  labs(x="", y="Diff-Diff Effect of Democratic Control (std.)", title="") +
  theme(text = element_text(size=30)) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_y_continuous(limits=c(-0.4, 0.4)) +
  scale_x_discrete(breaks=c("birthrate","edattendrate","robrate", "newimmig", "nofelons", "hsdiploma", "res_energy_price", "bus_energy_consum_pc", 
                            "vepvotingrate", "vcrimerate", "healthspendpc", "housing_prices_quar", "real_pc_inc_quar", "gsppcap", "state_cpi_bfh", 
                            "unemployment", "co2emissions", "Top1_adj", "murderrate", "pop_growth", "propcrimerate", "valueofagsect", 
                            "divorcerate", "Top01_adj", "cartheftrate", "abortionrate", "raperate", "firms"),
                   labels=c("Births", "School Attend", "Robberies", "New Immigrants", "Felons Ineligible", "HS Grad", 
                            "Energy Price", "Energy Consume", "Turnout (VEP)", "Violent Crimes", "Health Spend", "Housing Prices", 
                            "Income", "GSP", "CPI", "Unemployment", "Co2 Emissions", "Income Top 1%", 
                            "Murders", "Pop. Growth", "Property Crimes", "Agriculture", "Divorces", "Income Top 0.1%", "Car Thefts", 
                            "Abortions", "Rapes", "Number of Businesses"))
plot1 
ggsave(plot1, file="diff_diff_all_lag.pdf", width=16, height=11, scale=2)

detach(all3) 



######################## Each Chamber Individually (Figure 2 + Appendix Equivalent)

# 2 years downstream
all3<-read.dta("unified_diff_diff_l_1_single_effects.dta")

attach(all3) 

pd <- position_dodge(width=0.62)  

color.names <- c("#000000", "#737373", "#bdbdbd")

plot1<-ggplot(all3, aes(y=coef,  x=depvar))+ 
  geom_hline(aes(yintercept=0.36), colour="grey8", linetype="dashed", size=2) +
  geom_hline(aes(yintercept=-0.36), colour="grey8", linetype="dashed", size=2) +
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed", size=2) +
  geom_point(data=all3, size=6,  aes(colour = factor(Chamber)), position = pd) +
  geom_errorbar(data=all3, aes(ymin=coef-1.96*stderr, ymax= coef+1.96*stderr, colour = factor(Chamber)), position = pd, width=0, size=1) +
  geom_errorbar(data=all3, aes(ymin=coef-1.64*stderr, ymax= coef+1.64*stderr, colour = factor(Chamber)), position = pd, width=0, size=3)+
  facet_wrap(~type, scales = "free", nrow=2)  +
  scale_colour_manual(values=color.names) + theme_bw() + theme(panel.grid.major = element_blank(), 
                                                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.title = element_blank()) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  coord_flip() +
  labs(x="", y="Diff-Diff Effect of Democratic Control (std.)", title="") +
  theme(text = element_text(size=30)) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_y_continuous(limits=c(-0.4, 0.4)) +
  scale_x_discrete(breaks=c("birthrate","edattendrate","robrate", "newimmig", "nofelons", "hsdiploma", "res_energy_price", "bus_energy_consum_pc", 
                            "vepvotingrate", "vcrimerate", "healthspendpc", "housing_prices_quar", "real_pc_inc_quar", "gsppcap", "state_cpi_bfh", 
                            "unemployment", "co2emissions", "Top1_adj", "murderrate", "pop_growth", "propcrimerate", "valueofagsect", 
                            "divorcerate", "Top01_adj", "cartheftrate", "abortionrate", "raperate", "firms"),
                   labels=c("Births", "School Attend", "Robberies", "New Immigrants", "Felons Ineligible", "HS Grad", 
                            "Energy Price", "Energy Consume", "Turnout (VEP)", "Violent Crimes", "Health Spend", "Housing Prices", 
                            "Income", "GSP", "CPI", "Unemployment", "Co2 Emissions", "Income Top 1%", 
                            "Murders", "Pop. Growth", "Property Crimes", "Agriculture", "Divorces", "Income Top 0.1%", "Car Thefts", 
                            "Abortions", "Rapes", "Number of Businesses"))
plot1 
ggsave(plot1, file="diff_diff_all_t1.pdf", width=16, height=11, scale=2)

detach(all3) 


## FIGURE 2 (IN PAPER)
# 4 years downstream

all3<-read.dta("unified_diff_diff_l_3_single_effects.dta")

attach(all3) 

pd <- position_dodge(width=0.62)  

color.names <- c("#000000", "#737373", "#bdbdbd")

plot1<-ggplot(all3, aes(y=coef,  x=depvar))+ 
  geom_hline(aes(yintercept=0.36), colour="grey8", linetype="dashed", size=2) +
  geom_hline(aes(yintercept=-0.36), colour="grey8", linetype="dashed", size=2) +  
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed", size=2) +
  geom_point(data=all3, size=6,  aes(colour = factor(Chamber)), position = pd) +
  geom_errorbar(data=all3, aes(ymin=coef-1.96*stderr, ymax= coef+1.96*stderr, colour = factor(Chamber)), position = pd, width=0, size=1) +
  geom_errorbar(data=all3, aes(ymin=coef-1.64*stderr, ymax= coef+1.64*stderr, colour = factor(Chamber)), position = pd, width=0, size=3)+
  facet_wrap(~type, scales = "free", nrow=2)  +
  scale_colour_manual(values=color.names) + theme_bw() + theme(panel.grid.major = element_blank(), 
                                                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.title = element_blank()) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  coord_flip() +
  labs(x="", y="Diff-Diff Effect of Democratic Control (std.)", title="") +
  theme(text = element_text(size=30)) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_y_continuous(limits=c(-0.4, 0.4)) +
  scale_x_discrete(breaks=c("birthrate","edattendrate","robrate", "newimmig", "nofelons", "hsdiploma", "res_energy_price", "bus_energy_consum_pc", 
                            "vepvotingrate", "vcrimerate", "healthspendpc", "housing_prices_quar", "real_pc_inc_quar", "gsppcap", "state_cpi_bfh", 
                            "unemployment", "co2emissions", "Top1_adj", "murderrate", "pop_growth", "propcrimerate", "valueofagsect", 
                            "divorcerate", "Top01_adj", "cartheftrate", "abortionrate", "raperate", "firms"),
                   labels=c("Births", "School Attend", "Robberies", "New Immigrants", "Felons Ineligible", "HS Grad", 
                            "Energy Price", "Energy Consume", "Turnout (VEP)", "Violent Crimes", "Health Spend", "Housing Prices", 
                            "Income", "GSP", "CPI", "Unemployment", "Co2 Emissions", "Income Top 1%", 
                            "Murders", "Pop. Growth", "Property Crimes", "Agriculture", "Divorces", "Income Top 0.1%", "Car Thefts", 
                            "Abortions", "Rapes", "Number of Businesses"))
plot1 
ggsave(plot1, file="diff_diff_all_t3.pdf", width=16, height=11, scale=2)

detach(all3) 


## FIGURE 3 (IN PAPER)
################# Combined: D vs R 

diffdiffall<-read.dta("unified_diff_diff_t1_t3.dta")
attach(diffdiffall) 

color.names<-c("black", "grey70")

pd <- position_dodge(width=0.57)  

plot1<-ggplot(diffdiffall, aes(y=coef,  x=reorder(depvar, rank))) +
  geom_hline(aes(yintercept=0.36), colour="grey8", linetype="dashed", size=2) +
  geom_hline(aes(yintercept=-0.36), colour="grey8", linetype="dashed", size=2) +  
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed", size=2) +
  geom_point(data=diffdiffall, size=7,  aes(colour = factor(year)), position = pd) +
  geom_errorbar(data=diffdiffall, aes(ymin=coef-1.96*stderr, ymax= coef+1.96*stderr, colour = factor(year)), position = pd, width=0, size=1) +
  geom_errorbar(data=diffdiffall, aes(ymin=coef-1.64*stderr, ymax= coef+1.64*stderr, colour = factor(year)), position = pd, width=0, size=3) +
  scale_colour_manual(values=color.names) + theme_bw() +theme(panel.grid.major = element_blank(), 
                                                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  theme(legend.title = element_blank()) +
  labs(x="", y="Effect of Unified Democraticic Control (std.)", title="") +
  theme(text = element_text(size=30)) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_y_continuous(limits=c(-0.5, 0.5)) +
  scale_x_discrete(breaks=c("birthrate","edattendrate","robrate", "newimmig", "nofelons", "hsdiploma", "res_energy_price", "bus_energy_consum_pc", 
                            "vepvotingrate", "vcrimerate", "healthspendpc", "housing_prices_quar", "real_pc_inc_quar", "gsppcap", "state_cpi_bfh", 
                            "unemployment", "co2emissions", "Top1_adj", "murderrate", "pop_growth", "propcrimerate", "valueofagsect", 
                            "divorcerate", "Top01_adj", "cartheftrate", "abortionrate", "raperate", "firms"),
                   labels=c("Births", "School Attend", "Robberies", "New Immigrants", "Felons Ineligible", "HS Grad", 
                            "Energy Price", "Energy Consume", "Turnout (VEP)", "Violent Crimes", "Health Spend", "Housing Prices", 
                            "Income", "GSP", "CPI", "Unemployment", "Co2 Emissions", "Income Top 1%", 
                            "Murders", "Pop. Growth", "Property Crimes", "Agriculture", "Divorces", "Income Top 0.1%", "Car Thefts", 
                            "Abortions", "Rapes", "Number of Businesses"))

plot1 

ggsave(plot1, file="diffdifall_unified_t1_t3.pdf", width=10.5, height=6, scale=2)

detach(diffdiffall) 


################# Combined: Divided vs R (In Appendix)

diffdiffall<-read.dta("unified_diff_diff_t1_t3_divided.dta")
attach(diffdiffall) 

color.names<-c("black", "chartreuse4")

pd <- position_dodge(width=0.57)  

plot1<-ggplot(diffdiffall, aes(y=coef,  x=reorder(depvar, rank))) + 
  geom_hline(aes(yintercept=0.36), colour="grey8", linetype="dashed", size=2) +
  geom_hline(aes(yintercept=-0.36), colour="grey8", linetype="dashed", size=2) +  
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed", size=2) +
  geom_point(data=diffdiffall, size=6,  aes(colour = factor(year)), position = pd) +
  geom_errorbar(data=diffdiffall, aes(ymin=coef-1.96*stderr, ymax= coef+1.96*stderr, colour = factor(year)), position = pd, width=0, size=1) +
  geom_errorbar(data=diffdiffall, aes(ymin=coef-1.64*stderr, ymax= coef+1.64*stderr, colour = factor(year)), position = pd, width=0, size=3) +
  scale_colour_manual(values=color.names) + theme_bw() +theme(panel.grid.major = element_blank(), 
                                                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  theme(legend.title = element_blank()) +
  labs(x="", y="Effect of Divided Control vs. Unified Republican (std.)", title="") +
  theme(text = element_text(size=22)) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_y_continuous(limits=c(-0.5, 0.5)) +
  scale_x_discrete(breaks=c("birthrate","edattendrate","robrate", "newimmig", "nofelons", "hsdiploma", "res_energy_price", "bus_energy_consum_pc", 
                            "vepvotingrate", "vcrimerate", "healthspendpc", "housing_prices_quar", "real_pc_inc_quar", "gsppcap", "state_cpi_bfh", 
                            "unemployment", "co2emissions", "Top1_adj", "murderrate", "pop_growth", "propcrimerate", "valueofagsect", 
                            "divorcerate", "Top01_adj", "cartheftrate", "abortionrate", "raperate", "firms"),
                   labels=c("Births", "School Attend", "Robberies", "New Immigrants", "Felons Ineligible", "HS Grad", 
                            "Energy Price", "Energy Consume", "Turnout (VEP)", "Violent Crimes", "Health Spend", "Housing Prices", 
                            "Income", "GSP", "CPI", "Unemployment", "Co2 Emissions", "Income Top 1%", 
                            "Murders", "Pop. Growth", "Property Crimes", "Agriculture", "Divorces", "Income Top 0.1%", "Car Thefts", 
                            "Abortions", "Rapes", "Number of Businesses"))

plot1 
ggsave(plot1, file="diffdifall_t1_t3_divided_vs_r.pdf", width=10.5, height=6, scale=2)

detach(diffdiffall) 


################# Combined: Divided vs D (In Appendix)

diffdiffall<-read.dta("unified_diff_diff_t1_t3_d_vs_divided.dta")
attach(diffdiffall) 

color.names<-c("black", "chartreuse4")

pd <- position_dodge(width=0.57)  

plot1<-ggplot(diffdiffall, aes(y=coef,  x=reorder(depvar, rank))) + 
  geom_hline(aes(yintercept=0.36), colour="grey8", linetype="dashed", size=2) +
  geom_hline(aes(yintercept=-0.36), colour="grey8", linetype="dashed", size=2) +  
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed", size=2) +
  geom_point(data=diffdiffall, size=6,  aes(colour = factor(year)), position = pd) +
  geom_errorbar(data=diffdiffall, aes(ymin=coef-1.96*stderr, ymax= coef+1.96*stderr, colour = factor(year)), position = pd, width=0, size=1) +
  geom_errorbar(data=diffdiffall, aes(ymin=coef-1.64*stderr, ymax= coef+1.64*stderr, colour = factor(year)), position = pd, width=0, size=3) +
  scale_colour_manual(values=color.names) + theme_bw() +theme(panel.grid.major = element_blank(), 
                                                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  theme(legend.title = element_blank()) +
  labs(x="", y="Effect of Unified Democratic vs. Divided Control (std.)", title="") +
  theme(text = element_text(size=24)) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_y_continuous(limits=c(-0.5, 0.5)) +
  scale_x_discrete(breaks=c("birthrate","edattendrate","robrate", "newimmig", "nofelons", "hsdiploma", "res_energy_price", "bus_energy_consum_pc", 
                            "vepvotingrate", "vcrimerate", "healthspendpc", "housing_prices_quar", "real_pc_inc_quar", "gsppcap", "state_cpi_bfh", 
                            "unemployment", "co2emissions", "Top1_adj", "murderrate", "pop_growth", "propcrimerate", "valueofagsect", 
                            "divorcerate", "Top01_adj", "cartheftrate", "abortionrate", "raperate", "firms"),
                   labels=c("Births", "School Attend", "Robberies", "New Immigrants", "Felons Ineligible", "HS Grad", 
                            "Energy Price", "Energy Consume", "Turnout (VEP)", "Violent Crimes", "Health Spend", "Housing Prices", 
                            "Income", "GSP", "CPI", "Unemployment", "Co2 Emissions", "Income Top 1%", 
                            "Murders", "Pop. Growth", "Property Crimes", "Agriculture", "Divorces", "Income Top 0.1%", "Car Thefts", 
                            "Abortions", "Rapes", "Number of Businesses"))

plot1 

ggsave(plot1, file="diffdifall_t1_t3_unified_d_vs_divided.pdf", width=10.5, height=6, scale=2)

detach(diffdiffall) 


## FIGURE 4 (IN PAPER)
################# Diff Diff--persistent control (way 2--years in power)

diffdiffall<-read.dta("unified_by_years_in_power_l_1_l_3.dta")
attach(diffdiffall) 

pd <- position_dodge(width=0.57)  

color.names<-c("black", "grey70")

pd <- position_dodge(width=0.57)  

plot1<-ggplot(diffdiffall, aes(y=coef,  x=reorder(depvar, rank))) + 
  geom_hline(aes(yintercept=0.36), colour="grey8", linetype="dashed", size=2) +
  geom_hline(aes(yintercept=-0.36), colour="grey8", linetype="dashed", size=2) +    
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed", size=2) +
  geom_point(data=diffdiffall, size=7,  aes(colour = factor(year)), position = pd) +
  geom_errorbar(data=diffdiffall, aes(ymin=coef-1.96*stderr, ymax= coef+1.96*stderr, colour = factor(year)), position = pd, width=0, size=1) +
  geom_errorbar(data=diffdiffall, aes(ymin=coef-1.64*stderr, ymax= coef+1.64*stderr, colour = factor(year)), position = pd, width=0, size=3) +
  scale_colour_manual(values=color.names) + theme_bw() +theme(panel.grid.major = element_blank(), 
                                                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  theme(legend.title = element_blank()) +
  labs(x="", y="Effect of Persistent Unified Democraticic Control (std.)", title="") +
  theme(text = element_text(size=30)) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_y_continuous(limits=c(-0.5, 0.5)) +
  scale_x_discrete(breaks=c("birthrate","edattendrate","robrate", "newimmig", "nofelons", "hsdiploma", "res_energy_price", "bus_energy_consum_pc", 
                            "vepvotingrate", "vcrimerate", "healthspendpc", "housing_prices_quar", "real_pc_inc_quar", "gsppcap", "state_cpi_bfh", 
                            "unemployment", "co2emissions", "Top1_adj", "murderrate", "pop_growth", "propcrimerate", "valueofagsect", 
                            "divorcerate", "Top01_adj", "cartheftrate", "abortionrate", "raperate", "firms"),
                   labels=c("Births", "School Attend", "Robberies", "New Immigrants", "Felons Ineligible", "HS Grad", 
                            "Energy Price", "Energy Consume", "Turnout (VEP)", "Violent Crimes", "Health Spend", "Housing Prices", 
                            "Income", "GSP", "CPI", "Unemployment", "Co2 Emissions", "Income Top 1%", 
                            "Murders", "Pop. Growth", "Property Crimes", "Agriculture", "Divorces", "Income Top 0.1%", "Car Thefts", 
                            "Abortions", "Rapes", "Number of Businesses"))

plot1 

ggsave(plot1, file="diffdifall_unified_persistent_way_2.pdf", width=10.5, height=8, scale=2)

detach(diffdiffall) 


################# Combined: Unified D vs R, BY YEAR (In Appendix)

diffdiffall<-read.dta("unified_diff_diff_t1_t3_d_vs_divided_late.dta")
attach(diffdiffall) 

color.names<-c("black", "chartreuse4")

pd <- position_dodge(width=0.57)  

plot1<-ggplot(diffdiffall, aes(y=coef,  x=reorder(depvar, rank))) + 
  geom_hline(aes(yintercept=0.36), colour="grey8", linetype="dashed", size=2) +
  geom_hline(aes(yintercept=-0.36), colour="grey8", linetype="dashed", size=2) +  
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed", size=2) +
  geom_point(data=diffdiffall, size=6,  aes(colour = factor(year)), position = pd) +
  geom_errorbar(data=diffdiffall, aes(ymin=coef-1.96*stderr, ymax= coef+1.96*stderr, colour = factor(year)), position = pd, width=0, size=1) +
  geom_errorbar(data=diffdiffall, aes(ymin=coef-1.64*stderr, ymax= coef+1.64*stderr, colour = factor(year)), position = pd, width=0, size=3) +
  scale_colour_manual(values=color.names) + theme_bw() +theme(panel.grid.major = element_blank(), 
                                                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  theme(legend.title = element_blank()) +
  labs(x="", y="Effect of Unified Democratic by Year (std.)", title="") +
  theme(text = element_text(size=30)) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_y_continuous(limits=c(-0.4, 0.4)) +
  scale_x_discrete(breaks=c("birthrate","edattendrate","robrate", "newimmig", "nofelons", "hsdiploma", "res_energy_price", "bus_energy_consum_pc", 
                            "vepvotingrate", "vcrimerate", "healthspendpc", "housing_prices_quar", "real_pc_inc_quar", "gsppcap", "state_cpi_bfh", 
                            "unemployment", "co2emissions", "Top1_adj", "murderrate", "pop_growth", "propcrimerate", "valueofagsect", 
                            "divorcerate", "Top01_adj", "cartheftrate", "abortionrate", "raperate", "firms"),
                   labels=c("Births", "School Attend", "Robberies", "New Immigrants", "Felons Ineligible", "HS Grad", 
                            "Energy Price", "Energy Consume", "Turnout (VEP)", "Violent Crimes", "Health Spend", "Housing Prices", 
                            "Income", "GSP", "CPI", "Unemployment", "Co2 Emissions", "Income Top 1%", 
                            "Murders", "Pop. Growth", "Property Crimes", "Agriculture", "Divorces", "Income Top 0.1%", "Car Thefts", 
                            "Abortions", "Rapes", "Number of Businesses"))

plot1 

ggsave(plot1, file="unified_diff_diff_t1_t3_d_vs_divided_late.pdf", width=10.5, height=6, scale=2)

detach(diffdiffall) 


################# Diff Diff--persistent control (way 1---triple interaction)


diffdiffall<-read.dta("unified_diff_diff_dynamic.dta")
attach(diffdiffall) 

pd <- position_dodge(width=0.57)  

plot1<-ggplot(diffdiffall, aes(y=coef,  x=reorder(depvar, rank))) + 
  geom_hline(aes(yintercept=0.36), colour="grey8", linetype="dashed", size=2) +
  geom_hline(aes(yintercept=-0.36), colour="grey8", linetype="dashed", size=2) +    
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed", size=2) +
  geom_point(data=diffdiffall, size=6,  colour = "black", position = pd) +
  geom_errorbar(data=diffdiffall, aes(ymin=coef-1.96*stderr, ymax= coef+1.96*stderr, colour = "black"), position = pd, width=0, size=1) +
  geom_errorbar(data=diffdiffall, aes(ymin=coef-1.64*stderr, ymax= coef+1.64*stderr, colour = "black"), position = pd, width=0, size=3) +
  scale_colour_manual(values=color.names) + theme_bw() +theme(panel.grid.major = element_blank(), 
                                                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.title = element_blank()) +
  theme(legend.position="none") + 
  scale_y_continuous(limits=c(-1, 1)) +
  labs(x="", y="", title="") +
  theme(text = element_text(size=30)) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_x_discrete(breaks=c("birthrate","edattendrate","robrate", "newimmig", "nofelons", "hsdiploma", "res_energy_price", "bus_energy_consum_pc", 
                            "vepvotingrate", "vcrimerate", "healthspendpc", "housing_prices_quar", "real_pc_inc_quar", "gsppcap", "state_cpi_bfh", 
                            "unemployment", "co2emissions", "Top1_adj", "murderrate", "pop_growth", "propcrimerate", "valueofagsect", 
                            "divorcerate", "Top01_adj", "cartheftrate", "abortionrate", "raperate", "firms"),
                   labels=c("Births", "School Attend", "Robberies", "New Immigrants", "Felons Ineligible", "HS Grad", 
                            "Energy Price", "Energy Consume", "Turnout (VEP)", "Violent Crimes", "Health Spend", "Housing Prices", 
                            "Income", "GSP", "CPI", "Unemployment", "Co2 Emissions", "Income Top 1%", 
                            "Murders", "Pop. Growth", "Property Crimes", "Agriculture", "Divorces", "Income Top 0.1%", "Car Thefts", 
                            "Abortions", "Rapes", "Number of Businesses"))

plot1 

ggsave(plot1, file="diffdifall_unified_persistent.pdf", width=10.5, height=8, scale=2)

detach(diffdiffall) 



################# Diff Diff--composite outcomes



# 2 years downstream
all3<-read.dta("unified_diff_diff_all8_single_effects_composite_2.dta")

attach(all3) 

pd <- position_dodge(width=0.62)  

color.names <- c("#000000", "#737373", "#bdbdbd")

plot1<-ggplot(all3, aes(y=coef,  x=Chamber))+ 
  geom_hline(aes(yintercept=0.36), colour="grey8", linetype="dashed", size=2) +
  geom_hline(aes(yintercept=-0.36), colour="grey8", linetype="dashed", size=2) +
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed", size=2) +
  geom_point(data=all3, size=6,  aes(colour = factor(Chamber)), position = pd) +
  geom_errorbar(data=all3, aes(ymin=coef-1.96*stderr, ymax= coef+1.96*stderr, colour = factor(Chamber)), position = pd, width=0, size=1) +
  geom_errorbar(data=all3, aes(ymin=coef-1.64*stderr, ymax= coef+1.64*stderr, colour = factor(Chamber)), position = pd, width=0, size=3)+
  facet_wrap(~depvar, scales = "free", nrow=2)  +
  scale_colour_manual(values=color.names) + theme_bw() + theme(panel.grid.major = element_blank(), 
                                                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.title = element_blank()) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  coord_flip() +
  labs(x="", y="Diff-Diff Effect of Democratic Control (std.)", title="") +
  theme(text = element_text(size=40)) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_y_continuous(limits=c(-0.4, 0.4)) +
  scale_x_discrete(breaks=c("birthrate","edattendrate","robrate", "newimmig", "nofelons", "hsdiploma", "res_energy_price", "bus_energy_consum_pc", 
                            "vepvotingrate", "vcrimerate", "healthspendpc", "housing_prices_quar", "real_pc_inc_quar", "gsppcap", "state_cpi_bfh", 
                            "unemployment", "co2emissions", "Top1_adj", "murderrate", "pop_growth", "propcrimerate", "valueofagsect", 
                            "divorcerate", "Top01_adj", "cartheftrate", "abortionrate", "raperate", "firms"),
                   labels=c("Births", "School Attend", "Robberies", "New Immigrants", "Felons Ineligible", "HS Grad", 
                            "Energy Price", "Energy Consume", "Turnout (VEP)", "Violent Crimes", "Health Spend", "Housing Prices", 
                            "Income", "GSP", "CPI", "Unemployment", "Co2 Emissions", "Income Top 1%", 
                            "Murders", "Pop. Growth", "Property Crimes", "Agriculture", "Divorces", "Income Top 0.1%", "Car Thefts", 
                            "Abortions", "Rapes", "Number of Businesses"))
plot1 
ggsave(plot1, file="diff_diff_all_t1_composite.pdf", width=16, height=11, scale=2)

detach(all3) 


# 4 years downstream
all3<-read.dta("unified_diff_diff_all8_single_effects_composite_4.dta")

attach(all3) 

pd <- position_dodge(width=0.62)  

color.names <- c("#000000", "#737373", "#bdbdbd")

plot1<-ggplot(all3, aes(y=coef,  x=Chamber))+ 
  geom_hline(aes(yintercept=0.36), colour="grey8", linetype="dashed", size=2) +
  geom_hline(aes(yintercept=-0.36), colour="grey8", linetype="dashed", size=2) +  
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed", size=2) +
  geom_point(data=all3, size=6,  aes(colour = factor(Chamber)), position = pd) +
  geom_errorbar(data=all3, aes(ymin=coef-1.96*stderr, ymax= coef+1.96*stderr, colour = factor(Chamber)), position = pd, width=0, size=1) +
  geom_errorbar(data=all3, aes(ymin=coef-1.64*stderr, ymax= coef+1.64*stderr, colour = factor(Chamber)), position = pd, width=0, size=3)+
  facet_wrap(~depvar, scales = "free", nrow=2)  +
  scale_colour_manual(values=color.names) + theme_bw() + theme(panel.grid.major = element_blank(), 
                                                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.title = element_blank()) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  coord_flip() +
  labs(x="", y="Diff-Diff Effect of Democratic Control (std.)", title="") +
  theme(text = element_text(size=40)) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_y_continuous(limits=c(-0.4, 0.4)) +
  scale_x_discrete(breaks=c("birthrate","edattendrate","robrate", "newimmig", "nofelons", "hsdiploma", "res_energy_price", "bus_energy_consum_pc", 
                            "vepvotingrate", "vcrimerate", "healthspendpc", "housing_prices_quar", "real_pc_inc_quar", "gsppcap", "state_cpi_bfh", 
                            "unemployment", "co2emissions", "Top1_adj", "murderrate", "pop_growth", "propcrimerate", "valueofagsect", 
                            "divorcerate", "Top01_adj", "cartheftrate", "abortionrate", "raperate", "firms"),
                   labels=c("Births", "School Attend", "Robberies", "New Immigrants", "Felons Ineligible", "HS Grad", 
                            "Energy Price", "Energy Consume", "Turnout (VEP)", "Violent Crimes", "Health Spend", "Housing Prices", 
                            "Income", "GSP", "CPI", "Unemployment", "Co2 Emissions", "Income Top 1%", 
                            "Murders", "Pop. Growth", "Property Crimes", "Agriculture", "Divorces", "Income Top 0.1%", "Car Thefts", 
                            "Abortions", "Rapes", "Number of Businesses"))
plot1 
ggsave(plot1, file="diff_diff_all_t3_composite.pdf", width=16, height=11, scale=2)

detach(all3) 


################# Diff Diff--composite outcomes (unified D v R)



diffdiffall<-read.dta("unified_diff_diff_t1_t3_d_vs_r_composite.dta")
attach(diffdiffall) 

color.names<-c("black", "chartreuse4")

pd <- position_dodge(width=0.57)  

plot1<-ggplot(diffdiffall, aes(y=coef,  x=depvar)) + 
  geom_hline(aes(yintercept=0.36), colour="grey8", linetype="dashed", size=2) +
  geom_hline(aes(yintercept=-0.36), colour="grey8", linetype="dashed", size=2) +  
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed", size=2) +
  geom_point(data=diffdiffall, size=6,  aes(colour = factor(year)), position = pd) +
  geom_errorbar(data=diffdiffall, aes(ymin=coef-1.96*stderr, ymax= coef+1.96*stderr, colour = factor(year)), position = pd, width=0, size=1) +
  geom_errorbar(data=diffdiffall, aes(ymin=coef-1.64*stderr, ymax= coef+1.64*stderr, colour = factor(year)), position = pd, width=0, size=3) +
  scale_colour_manual(values=color.names) + theme_bw() +theme(panel.grid.major = element_blank(), 
                                                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  theme(legend.title = element_blank()) +
  labs(x="", y="Effect of Unified Democratic (std.)", title="") +
  theme(text = element_text(size=30)) + 
  scale_y_continuous(limits=c(-0.4, 0.4)) 

plot1 

ggsave(plot1, file="unified_diff_diff_t1_t3_d_vs_r_composite.pdf", width=8, height=5, scale=2)

detach(diffdiffall) 


##################### Diff Diff -- change in DV

# 2 years downstream
all3<-read.dta("change_dv_diff_dff.dta")

attach(all3) 

pd <- position_dodge(width=0.62)  

color.names <- c("#000000", "#737373", "#bdbdbd")

plot1<-ggplot(all3, aes(y=coef,  x=depvar))+ 
  geom_hline(aes(yintercept=0.36), colour="grey8", linetype="dashed", size=2) +
  geom_hline(aes(yintercept=-0.36), colour="grey8", linetype="dashed", size=2) +
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed", size=2) +
  geom_point(data=all3, size=6,  aes(colour = factor(Chamber)), position = pd) +
  geom_errorbar(data=all3, aes(ymin=coef-1.96*stderr, ymax= coef+1.96*stderr, colour = factor(Chamber)), position = pd, width=0, size=1) +
  geom_errorbar(data=all3, aes(ymin=coef-1.64*stderr, ymax= coef+1.64*stderr, colour = factor(Chamber)), position = pd, width=0, size=3)+
  facet_wrap(~type, scales = "free", nrow=2)  +
  scale_colour_manual(values=color.names) + theme_bw() + theme(panel.grid.major = element_blank(), 
                                                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.title = element_blank()) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  coord_flip() +
  labs(x="", y="Diff-Diff Effect of Democratic Control (std.)", title="") +
  theme(text = element_text(size=30)) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_y_continuous(limits=c(-0.4, 0.4)) +
  scale_x_discrete(breaks=c("birthrate","edattendrate","robrate", "newimmig", "nofelons", "hsdiploma", "res_energy_price", "bus_energy_consum_pc", 
                            "vepvotingrate", "vcrimerate", "healthspendpc", "housing_prices_quar", "real_pc_inc_quar", "gsppcap", "state_cpi_bfh", 
                            "unemployment", "co2emissions", "Top1_adj", "murderrate", "pop_growth", "propcrimerate", "valueofagsect", 
                            "divorcerate", "Top01_adj", "cartheftrate", "abortionrate", "raperate", "firms"),
                   labels=c("Births", "School Attend", "Robberies", "New Immigrants", "Felons Ineligible", "HS Grad", 
                            "Energy Price", "Energy Consume", "Turnout (VEP)", "Violent Crimes", "Health Spend", "Housing Prices", 
                            "Income", "GSP", "CPI", "Unemployment", "Co2 Emissions", "Income Top 1%", 
                            "Murders", "Pop. Growth", "Property Crimes", "Agriculture", "Divorces", "Income Top 0.1%", "Car Thefts", 
                            "Abortions", "Rapes", "Number of Businesses"))
plot1 
ggsave(plot1, file="diff_diff_all_t0_change.pdf", width=16, height=11, scale=2)

detach(all3)



################# Unified Effects--Additional Outcomes

diffdiffall<-read.dta("unified_diff_diff_single_effects_extra_dvs_4_years.dta")
attach(diffdiffall) 

color.names<-c("#cccccc", "#969696", "#636363", "#252525")

pd <- position_dodge(width=0.57)  

plot1<-ggplot(diffdiffall, aes(y=coef,  x=depvar)) + 
  geom_hline(aes(yintercept=0.36), colour="grey8", linetype="dashed", size=2) +
  geom_hline(aes(yintercept=-0.36), colour="grey8", linetype="dashed", size=2) +  
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed", size=2) +
  geom_point(data=diffdiffall, size=6,  aes(colour = factor(year)), position = pd) +
  geom_errorbar(data=diffdiffall, aes(ymin=coef-1.96*stderr, ymax= coef+1.96*stderr, colour = factor(year)), position = pd, width=0, size=1) +
  geom_errorbar(data=diffdiffall, aes(ymin=coef-1.64*stderr, ymax= coef+1.64*stderr, colour = factor(year)), position = pd, width=0, size=3) +
  scale_colour_manual(values=color.names) + theme_bw() +theme(panel.grid.major = element_blank(), 
                                                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  theme(legend.title = element_blank()) +
  labs(x="", y="Effect of Unified Democratic (std.)", title="") +
  theme(text = element_text(size=35)) + 
  scale_y_continuous(limits=c(-1, 1))  +
  theme(axis.text.x = element_text(angle=45, hjust = 1)) 

plot1 

ggsave(plot1, file="unified_diff_diff_t1_t3_other_outcomes.pdf", width=15, height=8, scale=2)


detach(diffdiffall) 



################# 
diffdiffall<-read.dta("unified_diff_diff_t1.dta")
attach(diffdiffall) 

plot1<-ggplot(diffdiffall, aes(y=coef,  x=reorder(depvar, rank)))+ 
  geom_point(data=diffdiffall, colour="black", size=7, position=position_dodge(width=0.6)) +
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed") +
  geom_errorbar(aes(ymin=coef-1.96*stderr, ymax=coef+1.96*stderr), colour="black", width=0, size=2) +
  geom_errorbar(aes(ymin=coef-1.64*stderr, ymax=coef+1.64*stderr), colour="black", width=0, size=4) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x="", y="Effect of Unified Democraticic Control (std.)", title="") +
  theme(text = element_text(size=30)) + 
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  scale_y_continuous(limits=c(-0.5, 0.5)) +
  scale_x_discrete(breaks=c("birthrate","edattendrate","robrate", "newimmig", "nofelons", "hsdiploma", "res_energy_price", "bus_energy_consum_pc", 
                            "vepvotingrate", "vcrimerate", "healthspendpc", "housing_prices_quar", "real_pc_inc_quar", "gsppcap", "state_cpi_bfh", 
                            "unemployment", "co2emissions", "Top1_adj", "murderrate", "pop_growth", "propcrimerate", "valueofagsect", 
                            "divorcerate", "Top01_adj", "cartheftrate", "abortionrate", "raperate"),
                   labels=c("Births", "School Attend", "Robberies", "New Immigrants", "Felons Ineligible", "HS Grad", 
                            "Energy Price", "Energy Consume", "Turnout (VEP)", "Violent Crimes", "Health Spend", "Housing Prices", 
                            "Income", "GSP", "CPI", "Unemployment", "Co2 Emissions", "Income Top 1%", 
                            "Murders", "Pop. Growth", "Property Crimes", "Agriculture", "Divorces", "Income Top 0.1%", "Car Thefts", 
                            "Abortions", "Rapes"))

plot1
ggsave(plot1, file="diffdifall_unified_t1.pdf", width=10, height=6, scale=2)

detach(diffdiffall) 


################# 
diffdiffall<-read.dta("unified_diff_diff_l_3_single_effects.dta")
attach(diffdiffall) 

plot1<-ggplot(diffdiffall, aes(y=coef,  x=depvar))+ 
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed") +
  geom_point(data=diffdiffall, colour="black", size=7, position=position_dodge(width=0.6)) +
  geom_errorbar(aes(ymin=coef-1.96*stderr, ymax=coef+1.96*stderr), colour="black", width=0, size=2) +
  geom_errorbar(aes(ymin=coef-1.64*stderr, ymax=coef+1.64*stderr), colour="black", width=0, size=4) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x="", y="Effect of Unified Democraticic Control (std.)", title="") +
  theme(text = element_text(size=30)) + 
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  scale_y_continuous(limits=c(-0.5, 0.5)) +
  scale_x_discrete(breaks=c("birthrate","edattendrate","robrate", "newimmig", "nofelons", "hsdiploma", "res_energy_price", "bus_energy_consum_pc", 
                            "vepvotingrate", "vcrimerate", "healthspendpc", "housing_prices_quar", "real_pc_inc_quar", "gsppcap", "state_cpi_bfh", 
                            "unemployment", "co2emissions", "Top1_adj", "murderrate", "pop_growth", "propcrimerate", "valueofagsect", 
                            "divorcerate", "Top01_adj", "cartheftrate", "abortionrate", "raperate"),
                   labels=c("Births", "School Attend", "Robberies", "New Immigrants", "Felons Ineligible", "HS Grad", 
                            "Energy Price", "Energy Consume", "Turnout (VEP)", "Violent Crimes", "Health Spend", "Housing Prices", 
                            "Income", "GSP", "CPI", "Unemployment", "Co2 Emissions", "Income Top 1%", 
                            "Murders", "Pop. Growth", "Property Crimes", "Agriculture", "Divorces", "Income Top 0.1%", "Car Thefts", 
                            "Abortions", "Rapes"))

plot1
ggsave(plot1, file="diffdifall_unified_t3.pdf", width=10, height=6, scale=2)

detach(diffdiffall) 


################# Observational Comparisons Unified
diffdiffall<-read.dta("unified_diff_diff_t1_t3_nofe.dta")
attach(diffdiffall) 

color.names<-c("black", "chartreuse4")

pd <- position_dodge(width=0.57)  

plot1<-ggplot(diffdiffall, aes(y=coef,  x=reorder(depvar, rank))) + 
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed", size=2) +
  geom_point(data=diffdiffall, size=6,  aes(colour = factor(year)), position = pd) +
  geom_errorbar(data=diffdiffall, aes(ymin=coef-1.96*stderr, ymax= coef+1.96*stderr, colour = factor(year)), position = pd, width=0, size=1) +
  geom_errorbar(data=diffdiffall, aes(ymin=coef-1.64*stderr, ymax= coef+1.64*stderr, colour = factor(year)), position = pd, width=0, size=3) +
  scale_colour_manual(values=color.names) + theme_bw() +theme(panel.grid.major = element_blank(), 
                                                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  theme(legend.title = element_blank()) +
  labs(x="", y="Effect of Unified Democraticic Control (std.)", title="") +
  theme(text = element_text(size=30)) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_y_continuous(limits=c(-5, 5)) +
  scale_x_discrete(breaks=c("birthrate","edattendrate","robrate", "newimmig", "nofelons", "hsdiploma", "res_energy_price", "bus_energy_consum_pc", 
                            "vepvotingrate", "vcrimerate", "healthspendpc", "housing_prices_quar", "real_pc_inc_quar", "gsppcap", "state_cpi_bfh", 
                            "unemployment", "co2emissions", "Top1_adj", "murderrate", "pop_growth", "propcrimerate", "valueofagsect", 
                            "divorcerate", "Top01_adj", "cartheftrate", "abortionrate", "raperate"),
                   labels=c("Births", "School Attend", "Robberies", "New Immigrants", "Felons Ineligible", "HS Grad", 
                            "Energy Price", "Energy Consume", "Turnout (VEP)", "Violent Crimes", "Health Spend", "Housing Prices", 
                            "Income", "GSP", "CPI", "Unemployment", "Co2 Emissions", "Income Top 1%", 
                            "Murders", "Pop. Growth", "Property Crimes", "Agriculture", "Divorces", "Income Top 0.1%", "Car Thefts", 
                            "Abortions", "Rapes"))

plot1 

ggsave(plot1, file="observational_unified_t1_t3.pdf", width=10.5, height=6, scale=2)

detach(diffdiffall) 




##########################################################################################  
##########################################################################################  
########################################################################################## 
##########################################################################################  
##########################################################################################  
##########################################################################################  
##
##                       RDD Graphs
##
##########################################################################################  
##########################################################################################  
########################################################################################## 
##########################################################################################  
##########################################################################################  
########################################################################################## 






########################################################################################## 
##########################################################################################  
########################################################################################## 
##
## Inidividual Cut-Offs
##
########################################################################################## 
##########################################################################################  
########################################################################################## 






################# All 2 years downstream--Appendix

all3<-read.dta("rdd_t1.dta")

attach(all3) 

pd <- position_dodge(width=0.62)  

color.names <- c("#000000", "#737373", "#bdbdbd")

plot1<-ggplot(all3, aes(y=coef,  x=depvar))+ 
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed", size=2) +
  # geom_hline(aes(yintercept=0.36), colour="grey8", linetype="dashed", size=2) +
  #  geom_hline(aes(yintercept=-0.36), colour="grey8", linetype="dashed", size=2) +
  geom_point(data=all3, size=6,  aes(colour = factor(chamber)), position = pd) +
  geom_errorbar(data=all3, aes(ymin=coef-1.96*stderr, ymax= coef+1.96*stderr, colour = factor(chamber)), position = pd, width=0, size=1) +
  geom_errorbar(data=all3, aes(ymin=coef-1.64*stderr, ymax= coef+1.64*stderr, colour = factor(chamber)), position = pd, width=0, size=3)+
  facet_wrap(~type, scales = "free", nrow=2)   +
  scale_colour_manual(values=color.names) + theme_bw() + theme(panel.grid.major = element_blank(), 
                                                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.title = element_blank()) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  coord_flip() +
  labs(x="", y="RDD Effect of Democrat Control (std.)", title="") +
  theme(text = element_text(size=30)) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_y_continuous(limits=c(-3, 3)) +
  scale_x_discrete(breaks=c("s_birthrate_l_1","s_edattendrate_l_1","s_robrate_l_1", "s_newimmig_l_1", "s_nofelons_l_1", "s_hsdiploma_l_1", "s_res_energy_price_l_1", "s_bus_energy_consum_pc_l_1", 
                            "s_vepvotingrate_l_1", "s_vcrimerate_l_1", "s_healthspendpc_l_1", "s_housing_prices_quar_l_1", "s_real_pc_inc_quar_l_1", "s_gsppcap_l_1", "s_state_cpi_bfh_l_1", 
                            "s_unemployment_l_1", "s_co2emissions_l_1", "s_Top1_adj_l_1", "s_murderrate_l_1", "s_pop_growth_l_1", "s_propcrimerate_l_1", "s_valueofagsect_l_1", 
                            "s_divorcerate_l_1", "s_Top01_adj_l_1", "s_cartheftrate_l_1", "s_abortionrate_l_1", "s_raperate_l_1", "firms_l_1"),
                   labels=c("Births", "School Attend", "Robberies", "New Immigrants", "Felons Ineligible", "HS Grad", 
                            "Energy Price", "Energy Consume", "Turnout (VEP)", "Violent Crimes", "Health Spend", "Housing Prices", 
                            "Income", "GSP", "CPI", "Unemployment", "Co2 Emissions", "Income Top 1%", 
                            "Murders", "Pop. Growth", "Property Crimes", "Agriculture", "Divorces", "Income Top 0.1%", "Car Thefts", 
                            "Abortions", "Rapes", "Number of Businesses"))
plot1 

ggsave(plot1, file="rdd_all_t1.pdf", width=16, height=10, scale=2)

detach(all3) 


################ All 4 years downstream

all3<-read.dta("rdd_t3.dta")

attach(all3) 

pd <- position_dodge(width=0.62)  

color.names <- c("#000000", "#737373", "#bdbdbd")

plot1<-ggplot(all3, aes(y=coef,  x=depvar))+ 
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed", size=2) +
  geom_point(data=all3, size=6,  aes(colour = factor(chamber)), position = pd) +
  geom_errorbar(data=all3, aes(ymin=coef-1.96*stderr, ymax= coef+1.96*stderr, colour = factor(chamber)), position = pd, width=0, size=1) +
  geom_errorbar(data=all3, aes(ymin=coef-1.64*stderr, ymax= coef+1.64*stderr, colour = factor(chamber)), position = pd, width=0, size=3)+
  facet_wrap(~type, scales = "free", nrow=2)  +
  scale_colour_manual(values=color.names) + theme_bw() + theme(panel.grid.major = element_blank(), 
                                                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.title = element_blank()) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  coord_flip() +
  labs(x="", y="RDD Effect of Democrat Control (std.)", title="") +
  theme(text = element_text(size=30)) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_y_continuous(limits=c(-3, 3)) +
  scale_x_discrete(breaks=c("birthrate","edattendrate","robrate", "newimmig", "nofelons", "hsdiploma", "res_energy_price", "bus_energy_consum_pc", 
                            "vepvotingrate", "vcrimerate", "healthspendpc", "housing_prices_quar", "real_pc_inc_quar", "gsppcap", "state_cpi_bfh", 
                            "unemployment", "co2emissions", "Top1_adj", "murderrate", "pop_growth", "propcrimerate", "valueofagsect", 
                            "divorcerate", "Top01_adj", "cartheftrate", "abortionrate", "raperate", "firms"),
                   labels=c("Births", "School Attend", "Robberies", "New Immigrants", "Felons Ineligible", "HS Grad", 
                            "Energy Price", "Energy Consume", "Turnout (VEP)", "Violent Crimes", "Health Spend", "Housing Prices", 
                            "Income", "GSP", "CPI", "Unemployment", "Co2 Emissions", "Income Top 1%", 
                            "Murders", "Pop. Growth", "Property Crimes", "Agriculture", "Divorces", "Income Top 0.1%", "Car Thefts", 
                            "Abortions", "Rapes", "Number of Businesses"))
plot1 

ggsave(plot1, file="rdd_all_t3.pdf", width=16, height=10, scale=2)

detach(all3) 


############ All Individual Cutoff Optimal Bandwidth--Appendix

rm(list=ls())

diffdiffall<-read.dta("rdd_t1_t3.dta")
attach(diffdiffall) 

color.names<-c("black", "chartreuse4")

pd <- position_dodge(width=0.57)  

plot1<-ggplot(diffdiffall, aes(y=coef,  x=reorder(depvarcommon, rank))) +
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed", size=2) +
  geom_point(data=diffdiffall, size=6,  aes(colour = factor(year)), position = pd) +
  geom_errorbar(data=diffdiffall, aes(ymin=coef-1.96*stderr, ymax= coef+1.96*stderr, colour = factor(year)), position = pd, width=0, size=1) +
  geom_errorbar(data=diffdiffall, aes(ymin=coef-1.64*stderr, ymax= coef+1.64*stderr, colour = factor(year)), position = pd, width=0, size=3) +
  scale_colour_manual(values=color.names) + theme_bw() +theme(panel.grid.major = element_blank(), 
                                                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  facet_wrap(~chamber, scales = "fixed", nrow=3)  +
  theme(legend.position="bottom", legend.direction="horizontal") +
  theme(legend.title = element_blank()) +
  labs(x="", y="Effect of Democraticic Control (std.)", title="") +
  theme(text = element_text(size=35)) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_y_continuous(limits=c(-2.75, 2.75)) +
  scale_x_discrete(breaks=c("birthrate","edattendrate","robrate", "newimmig", "nofelons", "hsdiploma", "res_energy_price", "bus_energy_consum_pc", 
                            "vepvotingrate", "vcrimerate", "healthspendpc", "housing_prices_quar", "real_pc_inc_quar", "gsppcap", "state_cpi_bfh", 
                            "unemployment", "co2emissions", "Top1_adj", "murderrate", "pop_growth", "propcrimerate", "valueofagsect", 
                            "divorcerate", "Top01_adj", "cartheftrate", "abortionrate", "raperate", "firms"),
                   labels=c("Births", "School Attend", "Robberies", "New Immigrants", "Felons Ineligible", "HS Grad", 
                            "Energy Price", "Energy Consume", "Turnout (VEP)", "Violent Crimes", "Health Spend", "Housing Prices", 
                            "Income", "GSP", "CPI", "Unemployment", "Co2 Emissions", "Income Top 1%", 
                            "Murders", "Pop. Growth", "Property Crimes", "Agriculture", "Divorces", "Income Top 0.1%", "Car Thefts", 
                            "Abortions", "Rapes", "Number of Businesses"))

plot1 

ggsave(plot1, file="rdd_t1_t3_optimalBW.pdf", width=19, height=12, scale=2)

detach(diffdiffall) 


############ All Individual Cutoff Full Bandwidth--Appendix

rm(list=ls())

diffdiffall<-read.dta("rdd_t1_t3_fullBW.dta")
attach(diffdiffall) 

color.names<-c("black", "chartreuse4")

pd <- position_dodge(width=0.57)  

plot1<-ggplot(diffdiffall, aes(y=coef,  x=reorder(depvarcommon, rank))) +
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed", size=2) +
  geom_point(data=diffdiffall, size=6,  aes(colour = factor(year)), position = pd) +
  geom_errorbar(data=diffdiffall, aes(ymin=coef-1.96*stderr, ymax= coef+1.96*stderr, colour = factor(year)), position = pd, width=0, size=1) +
  geom_errorbar(data=diffdiffall, aes(ymin=coef-1.64*stderr, ymax= coef+1.64*stderr, colour = factor(year)), position = pd, width=0, size=3) +
  scale_colour_manual(values=color.names) + theme_bw() +theme(panel.grid.major = element_blank(), 
                                                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  facet_wrap(~Chamber, scales = "fixed", nrow=3)  +
  theme(legend.position="bottom", legend.direction="horizontal") +
  theme(legend.title = element_blank()) +
  labs(x="", y="Effect of Democraticic Control (std.)", title="") +
  theme(text = element_text(size=35)) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_y_continuous(limits=c(-1.5, 1.5)) +
  scale_x_discrete(breaks=c("s_birthrate","s_edattendrate","s_robrate", "s_newimmig", "s_nofelons", "s_hsdiploma", "s_res_energy_price", "s_bus_energy_consum_pc", 
                            "s_vepvotingrate", "s_vcrimerate", "s_healthspendpc", "s_housing_prices_quar", "s_real_pc_inc_quar", "s_gsppcap", "s_state_cpi_bfh", 
                            "s_unemployment", "s_co2emissions", "s_Top1_adj", "s_murderrate", "s_pop_growth", "s_propcrimerate", "s_valueofagsect", 
                            "s_divorcerate", "s_Top01_adj", "s_cartheftrate", "s_abortionrate", "s_raperate", "s_firms"),
                   labels=c("Births", "School Attend", "Robberies", "New Immigrants", "Felons Ineligible", "HS Grad", 
                            "Energy Price", "Energy Consume", "Turnout (VEP)", "Violent Crimes", "Health Spend", "Housing Prices", 
                            "Income", "GSP", "CPI", "Unemployment", "Co2 Emissions", "Income Top 1%", 
                            "Murders", "Pop. Growth", "Property Crimes", "Agriculture", "Divorces", "Income Top 0.1%", "Car Thefts", 
                            "Abortions", "Rapes", "Number of Businesses"))

plot1 

ggsave(plot1, file="rdd_t1_t3_fullBW.pdf", width=19, height=12, scale=2)

detach(diffdiffall) 



################# Combined: D vs R/Div (Appendix)

rddall<-read.dta("rdd_unified_t1_t3_dem.dta")
attach(rddall) 

color.names<-c("black", "chartreuse4")

pd <- position_dodge(width=0.57)  

plot1<-ggplot(rddall, aes(y=coef,  x=reorder(depvar, rank))) + 
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed", size=2) +
  geom_point(data=rddall, size=6,  aes(colour = factor(year)), position = pd) +
  geom_errorbar(data=rddall, aes(ymin=coef-1.96*stderr, ymax= coef+1.96*stderr, colour = factor(year)), position = pd, width=0, size=1) +
  geom_errorbar(data=rddall, aes(ymin=coef-1.64*stderr, ymax= coef+1.64*stderr, colour = factor(year)), position = pd, width=0, size=3) +
  scale_colour_manual(values=color.names) + theme_bw() +theme(panel.grid.major = element_blank(), 
                                                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  theme(legend.title = element_blank()) +
  labs(x="", y="Effect of Unified Democrat Control (std.)", title="") +
  theme(text = element_text(size=30)) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_x_discrete(breaks=c("birthrate","edattendrate","robrate", "newimmig", "nofelons", "hsdiploma", "res_energy_price", "bus_energy_consum_pc", 
                            "vepvotingrate", "vcrimerate", "healthspendpc", "housing_prices_quar", "real_pc_inc_quar", "gsppcap", "state_cpi_bfh", 
                            "unemployment", "co2emissions", "Top1_adj", "murderrate", "pop_growth", "propcrimerate", "valueofagsect", 
                            "divorcerate", "Top01_adj", "cartheftrate", "abortionrate", "raperate"),
                   labels=c("Births", "School Attend", "Robberies", "New Immigrants", "Felons Ineligible", "HS Grad", 
                            "Energy Price", "Energy Consume", "Turnout (VEP)", "Violent Crimes", "Health Spend", "Housing Prices", 
                            "Income", "GSP", "CPI", "Unemployment", "Co2 Emissions", "Income Top 1%", 
                            "Murders", "Pop. Growth", "Property Crimes", "Agriculture", "Divorces", "Income Top 0.1%", "Car Thefts", 
                            "Abortions", "Rapes"))

plot1 

ggsave(plot1, file="rdd_unified_t1_t3_dem.pdf", width=10.5, height=6, scale=2)

detach(rddall) 



################# Combined: R vs D/Div (Appendix)

rddall<-read.dta("rdd_unified_t1_t3_rep.dta")
attach(rddall) 

color.names<-c("black", "chartreuse4")

pd <- position_dodge(width=0.57)  

plot1<-ggplot(rddall, aes(y=coef,  x=reorder(depvar, rank))) + 
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed", size=2) +
  geom_point(data=rddall, size=6,  aes(colour = factor(year)), position = pd) +
  geom_errorbar(data=rddall, aes(ymin=coef-1.96*stderr, ymax= coef+1.96*stderr, colour = factor(year)), position = pd, width=0, size=1) +
  geom_errorbar(data=rddall, aes(ymin=coef-1.64*stderr, ymax= coef+1.64*stderr, colour = factor(year)), position = pd, width=0, size=3) +
  scale_colour_manual(values=color.names) + theme_bw() +theme(panel.grid.major = element_blank(), 
                                                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  theme(legend.title = element_blank()) +
  labs(x="", y="Effect of Unified Republican Control (std.)", title="") +
  theme(text = element_text(size=30)) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_x_discrete(breaks=c("birthrate","edattendrate","robrate", "newimmig", "nofelons", "hsdiploma", "res_energy_price", "bus_energy_consum_pc", 
                            "vepvotingrate", "vcrimerate", "healthspendpc", "housing_prices_quar", "real_pc_inc_quar", "gsppcap", "state_cpi_bfh", 
                            "unemployment", "co2emissions", "Top1_adj", "murderrate", "pop_growth", "propcrimerate", "valueofagsect", 
                            "divorcerate", "Top01_adj", "cartheftrate", "abortionrate", "raperate"),
                   labels=c("Births", "School Attend", "Robberies", "New Immigrants", "Felons Ineligible", "HS Grad", 
                            "Energy Price", "Energy Consume", "Turnout (VEP)", "Violent Crimes", "Health Spend", "Housing Prices", 
                            "Income", "GSP", "CPI", "Unemployment", "Co2 Emissions", "Income Top 1%", 
                            "Murders", "Pop. Growth", "Property Crimes", "Agriculture", "Divorces", "Income Top 0.1%", "Car Thefts", 
                            "Abortions", "Rapes"))

plot1 

ggsave(plot1, file="rdd_unified_t1_t3_rep.pdf", width=10.5, height=6, scale=2)

detach(rddall) 


##################### RDD -- change in DV

# 2 years downstream
all3<-read.dta("rdd_t0_change.dta")

attach(all3) 

pd <- position_dodge(width=0.62)  

color.names <- c("#000000", "#737373", "#bdbdbd")

plot1<-ggplot(all3, aes(y=coef,  x=depvar))+ 
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed", size=2) +
  geom_point(data=all3, size=6,  aes(colour = factor(chamber)), position = pd) +
  geom_errorbar(data=all3, aes(ymin=coef-1.96*stderr, ymax= coef+1.96*stderr, colour = factor(chamber)), position = pd, width=0, size=1) +
  geom_errorbar(data=all3, aes(ymin=coef-1.64*stderr, ymax= coef+1.64*stderr, colour = factor(chamber)), position = pd, width=0, size=3)+
  facet_wrap(~type, scales = "free", nrow=2)  +
  scale_colour_manual(values=color.names) + theme_bw() + theme(panel.grid.major = element_blank(), 
                                                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.title = element_blank()) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  coord_flip() +
  labs(x="", y="RDD Effect of Democratic Control (std.)", title="") +
  theme(text = element_text(size=30)) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_y_continuous(limits=c(-1.22, 1.34)) +
  scale_x_discrete(breaks=c("d_birthrate","d_edattendrate","d_robrate", "d_newimmig", "d_nofelons", "d_hsdiploma", "d_res_energy_price", "d_bus_energy_consum_pc", 
                            "d_vepvotingrate", "d_vcrimerate", "d_healthspendpc", "d_housing_prices_quar", "d_real_pc_inc_quar", "d_gsppcap", "d_state_cpi_bfh", 
                            "d_unemployment", "d_co2emissions", "d_Top1_adj", "d_murderrate", "d_pop_growth", "d_propcrimerate", "d_valueofagsect", 
                            "d_divorcerate", "d_Top01_adj", "d_cartheftrate", "d_abortionrate", "d_raperate", "d_firms"),
                   labels=c("Births", "School Attend", "Robberies", "New Immigrants", "Felons Ineligible", "HS Grad", 
                            "Energy Price", "Energy Consume", "Turnout (VEP)", "Violent Crimes", "Health Spend", "Housing Prices", 
                            "Income", "GSP", "CPI", "Unemployment", "Co2 Emissions", "Income Top 1%", 
                            "Murders", "Pop. Growth", "Property Crimes", "Agriculture", "Divorces", "Income Top 0.1%", "Car Thefts", 
                            "Abortions", "Rapes", "Number of Businesses"))
plot1 
ggsave(plot1, file="rdd_all_t0_change.pdf", width=16, height=11, scale=2)


detach(all3) 




## FIGURE 5 (IN PAPER)
##################### RDD + FE 

# 2 years downstream
all3<-read.dta("rdd_diffdiff_l_3_single_effects.dta")

attach(all3) 

pd <- position_dodge(width=0.62)  

color.names <- c("#000000", "#737373", "#bdbdbd")

plot1<-ggplot(all3, aes(y=coef,  x=depvar))+ 
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed", size=2) +
  geom_hline(aes(yintercept=0.36), colour="grey8", linetype="dashed", size=2) +
  geom_hline(aes(yintercept=-0.36), colour="grey8", linetype="dashed", size=2) +
  geom_point(data=all3, size=6,  aes(colour = factor(Chamber)), position = pd) +
  geom_errorbar(data=all3, aes(ymin=coef-1.96*stderr, ymax= coef+1.96*stderr, colour = factor(Chamber)), position = pd, width=0, size=1) +
  geom_errorbar(data=all3, aes(ymin=coef-1.64*stderr, ymax= coef+1.64*stderr, colour = factor(Chamber)), position = pd, width=0, size=3)+
  facet_wrap(~type, scales = "free", nrow=2)  +
  scale_colour_manual(values=color.names) + theme_bw() + theme(panel.grid.major = element_blank(), 
                                                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.title = element_blank()) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  coord_flip() +
  labs(x="", y="RDD Effect of Democratic Control (std.)", title="") +
  theme(text = element_text(size=30)) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_y_continuous(limits=c(-0.5, 0.5)) +
  scale_x_discrete(breaks=c("birthrate","edattendrate","robrate", "newimmig", "nofelons", "hsdiploma", "res_energy_price", "bus_energy_consum_pc", 
                            "vepvotingrate", "vcrimerate", "healthspendpc", "housing_prices_quar", "real_pc_inc_quar", "gsppcap", "state_cpi_bfh", 
                            "unemployment", "co2emissions", "Top1_adj", "murderrate", "pop_growth", "propcrimerate", "valueofagsect", 
                            "divorcerate", "Top01_adj", "cartheftrate", "abortionrate", "raperate", "firms"),
                   labels=c("Births", "School Attend", "Robberies", "New Immigrants", "Felons Ineligible", "HS Grad", 
                            "Energy Price", "Energy Consume", "Turnout (VEP)", "Violent Crimes", "Health Spend", "Housing Prices", 
                            "Income", "GSP", "CPI", "Unemployment", "Co2 Emissions", "Income Top 1%", 
                            "Murders", "Pop. Growth", "Property Crimes", "Agriculture", "Divorces", "Income Top 0.1%", "Car Thefts", 
                            "Abortions", "Rapes", "Number of Businesses"))
plot1 
ggsave(plot1, file="rdd_diffdiff_l_3_single_effects.pdf", width=16, height=11, scale=2)


detach(all3) 





########################################################################################## 
##########################################################################################  
##########################################################################################  
##
## Hall et al. Running Variable
##
########################################################################################## 
##########################################################################################  
##########################################################################################




################# T +31 (pared)
variousrv<-read.dta("rdd_hall_seat_share_l_3.dta")

################# 
attach(variousrv) 

plot1<-ggplot(variousrv, aes(y=coef, x=specification )) + 
  geom_point(data=variousrv, colour="black", size=10) +
  geom_errorbar(data=variousrv, aes(ymin=coef-1.96*stderr, ymax= coef+1.96*stderr), width=0, size=3) +
  geom_errorbar(data=variousrv, aes(ymin=coef-1.64*stderr, ymax= coef+1.64*stderr), width=0, size=5) +
  facet_wrap(~depvar,  nrow=4)+
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x="", y="RDD Estimate (std.)", title="") +
  theme(text = element_text(size=35)) + geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed") +
  theme(axis.text.x = element_text(angle=90, hjust = 1))  + scale_y_continuous(limits=c(-2.3, 1.1))
plot1
ggsave(plot1, file="variousrv.pdf", width=20, height=13, scale=2)

detach(variousrv) 



################# with FE
variousrv<-read.dta("rdd_diffdiff_hall_seatshare.dta")

attach(variousrv) 

plot1<-ggplot(variousrv, aes(y=coef, x=specification )) + 
  geom_point(data=variousrv, colour="black", size=10) +
  geom_errorbar(data=variousrv, aes(ymin=coef-1.96*stderr, ymax= coef+1.96*stderr), width=0, size=3) +
  geom_errorbar(data=variousrv, aes(ymin=coef-1.64*stderr, ymax= coef+1.64*stderr), width=0, size=5) +
  facet_wrap(~depvar,  nrow=4)+
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x="", y="RDD + Diff/Diff Estimate (std.)", title="") +
  theme(text = element_text(size=35)) + geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed") +
  theme(axis.text.x = element_text(angle=90, hjust = 1))  + scale_y_continuous(limits=c(-0.5, 0.5))
plot1

ggsave(plot1, file="variousrv_with_fe.pdf", width=20, height=13, scale=2)

detach(variousrv) 



########################################################################################## 
##########################################################################################  
##########################################################################################  
##
## McCrary Sorting Test
##
########################################################################################## 
##########################################################################################  
##########################################################################################






library(foreign)
library(rdd)

overall <- read.dta("state_legislative_data_final_just_rv.dta")
attach(overall)


pdf("governor_mccrary.pdf")
DCdensity<-DCdensity(gov_dem_distance_l_p, 0)
abline(v=0,col="red",lty=2)
title(xlab="Proximity to Democratic Governor", ylab="Density")
dev.off()


pdf("legislative_house_mccrary.pdf")
DCdensity<-DCdensity(distance_majority_house, 0)
abline(v=0,col="red",lty=2)
title(xlab="Proximity to Democratic Majority in House", ylab="Density")
dev.off()

pdf("legislative_senate_mccrary.pdf")
DCdensity<-DCdensity(distance_majority_sen, 0)
abline(v=0,col="red",lty=2)
title(xlab="Proximity to Democratic Majority in Senate", ylab="Density")
dev.off()

### Hall 1
pdf("legislative_house_mccrary_hall_1.pdf")
DCdensity<-DCdensity(rv_i, 0)
abline(v=0,col="red",lty=2)
title(xlab="Proximity to Democratic Majority in House", ylab="Density")
dev.off()
DCdensity

### Hall 2
pdf("legislative_house_mccrary_hall_2.pdf")
DCdensity<-DCdensity(rve_i, 0)
abline(v=0,col="red",lty=2)
title(xlab="Proximity to Democratic Majority in House", ylab="Density")
dev.off()
DCdensity

### Hall 3
pdf("legislative_house_mccrary_hall_3.pdf")
DCdensity<-DCdensity(rvu_i, 0)
abline(v=0,col="red",lty=2)
title(xlab="Proximity to Democratic Majority in House", ylab="Density")
dev.off()
DCdensity


detach(overall)


