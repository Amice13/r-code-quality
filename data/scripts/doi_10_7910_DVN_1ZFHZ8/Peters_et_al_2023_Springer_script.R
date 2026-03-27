##############################################################################################################################################
# Script: Becoming Dutch at what cost? Increasing application fees and naturalisation rates of EU immigrants in the Netherlands              #
# Floris Peters, Swantje Falcke & Maarten Vink                                                                                               #
# Published in R. Barbulescu, S. W. Goodman, & L. Pedroza (Eds.), Revising the Integration-Citizenship Nexus in Europe (pp. 37-54). Springer #                                      
##############################################################################################################################################


################
# Introduction #
################

# This file provides the syntax used to create all the tables and figures in the paper. 
# The dataset contains sensitive, micro level information. As such, for privacy reasons the data is only available to individuals employed at or affiliated to Statistics Netherlands. 
# The dataset can be found at the following location on the network of Statistics Netherlands: \\cbsp.nl\Productie\Projecten\SAL\209253UM_FP_SEC1\Werk\Floris\MiLifeStatus\Springer_fees  

#######################################################################################################################
#######################################################################################################################

#############
# Variables #
#############

# ID
# (Individual identification number)

# YEAR
# (Observation year)

# NATURALISED
# (Naturalised - time-varying)
# [0] Not naturalised; 
# [1] Naturalised

# POST_2010
# (observation year >= 2010)
# [0] No; 
# [1] Yes

# HH_INCOME
# (Household income)

# HH_INCOME_MODAL
# (Household income below/above modal)
# [0] No; 
# [1] Yes

# GENDER
# [1] Male; 
# [2] Female

# YSM
# (Years since migration)

# YSM2
# (Years since migration, squared)

# AGEARRIVAL
# (Age at the moment of migration)

# AGEARRIVAL2
# (Age at the moment of migration, squared)

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

# DUALCITREC
# (Dual citizenship toleration in the origin country in categories)
# [0] no; 
# [1] yes

# MUNICIPALITY
# (Identification code municipality of residence)

# ORIGIN
# (Identification code origin country)

# IMMIGRATIONYEAR
# (Year of first immigration to the Netherlands)

#######################################################################################################################
#######################################################################################################################


#Upload dataframes for separate figures
dataset_fees <- read.csv(file.choose(),header=T,sep=";")
dataset_nat_rate <- read.csv(file.choose(),header=T,sep=";")
dataset_fe_regr_results <- read.csv(file.choose(),header=T,sep=";")
dataset_did_regr_results_years <- read.csv(file.choose(),header=T,sep=";")
dataset_did_regr_results_main <- read.csv(file.choose(),header=T,sep=";")

#load packages
library(ggplot2)
library(MASS)
library(dotwhisker)
library(dplyr)
library(lattice)
library(gridExtra)
library(grid)


##############     
# Figure 3.1 #
##############  

plot3.1 <- ggplot(data=dataset_fees, aes(x=year_3.1, y=fees_3.1)) +
  geom_bar(stat="identity") +
  theme_bw() +     
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12)) +   
  scale_x_continuous(breaks=c(2000,2005,2010,2015,2020)) +
  xlab("") +
  ylab("Application fee")
plot3.1


##############     
# Figure 3.2 #
##############  

plot3.2 <- ggplot(data=dataset_nat_rate, aes(x = year_3.2, y = nat_rate, group = modal_income)) +       
  geom_line(aes(linetype = group), size = 1) +                                                      
  theme_bw() +                                                                                      
  theme(legend.position = "right") +                                                                 
  theme(axis.text = element_text(size = 12),                                                        
        axis.title = element_text(size = 12)) +
  geom_text(aes(label = round(nat_rate, 2)),
            vjust = "inward", hjust = "inward",
            show.legend = FALSE) +
  geom_vline(xintercept = 0, colour = "grey60",                                    
             linetype = "dotted", size = 1) +   
  scale_x_continuous(breaks = c(2007,2008,2009,2010,2011,2012,2013,2014)) +                                                       
  scale_y_continuous(limits = c(20,50), breaks = seq(20,50, by = 10)) +                                
  xlab("") + 
  ylab("Naturalisation rate")                                                                    
plot3.2


##############     
# Figure 3.3 #
##############  

plot3.3 <- dwplot(data = dataset_fe_regr_results, 
             dot_args = list(size = 2, color = "black"),  
  whisker_args = list(size = 1, color = "black")) +     
  xlim(-8,2) +                                                     
  theme_bw() +                                                           
  geom_vline(xintercept = 0, colour = "grey60",                                    
             linetype = "dotted", size = 1) +                            
  theme(legend.title = element_blank()) +
  xlab("Difference in naturalisation rate of 2010-2014 compared to 2007-2009 (percentage points)") +
  ylab("")  
plot3.3


##############     
# Figure 3.4 #
##############  

#plot results per year
plot3.4_1 <- dwplot(data = dataset_did_regr_results_years, 
                  dot_args = list(size = 2, color = "black"),  
                  whisker_args = list(size = 1, color = "black")) +     
  xlim(-5,2.5) +                                                     
  theme_bw() +                                                           
  geom_vline(xintercept = 2010, colour = "grey60",                                    
             linetype = "dotted", size = 1) +  
  geom_hline(xintercept = 0, colour = "grey60",                                    
             linetype = "solid", size = 1) +
  theme(legend.title = element_blank()) +
  xlab("Difference in naturalisation rate between migrants from below/above model income households (percentage points)") +
  ylab("") +
  coord_flip()
plot3.4_1

#plot aggregate results
plot3.4_2 <- dwplot(data = dataset_did_regr_results_main, 
                    dot_args = list(size = 2, color = "black"),  
                    whisker_args = list(size = 1, color = "black")) +     
  xlim(-5,2.5) +                                                     
  theme_bw() +                                                           
  geom_vline(xintercept = 2010, colour = "grey60",                                    
             linetype = "dotted", size = 1) +  
  geom_hline(xintercept = 0, colour = "grey60",                                    
             linetype = "solid", size = 1) +
  theme(legend.title = element_blank()) +
  xlab("") +
  ylab("") +
  coord_flip()
plot3.4_2

#combine plots 4.3_1 and 3.4_2
plot3.4 <- grid.arrange(plot3.4_1,plot3.4_2,
                         layout_matrix = rbind(c(1,2)))
plot3.4


#############     
# Table 3.2 #
############# 

#upload dataset regressions
main_dataset <- read.csv(file.choose(),header=T,sep=";")

#difference-in-difference regression
result3.2_main <- lm(NATURALISATION ~ POST_2010 + GENDER + YSM + YSM2 + AGEARRIVAL + AGEARRIVAL2 +
                     as.factor(PARTNER) + CHILD + EMPLOYMENT + HH_INCOME + as.factor(EDUCATION) +
                     as.factor(DUALCITREC) + as.factor(MUNICIPALITY) + as.factor(ORIGIN) + as.factor(YEAR),
                     data = main_dataset, cluster = ID)

#parallel trend assumption
result3.2_year <- lm(NATURALISATION ~ as.factor(YEAR):as.factor(HH_INCOME_MODAL) + 
                     GENDER + YSM + YSM2 + AGEARRIVAL + AGEARRIVAL2 +
                     as.factor(PARTNER) + CHILD + EMPLOYMENT + HH_INCOME + as.factor(EDUCATION) +
                     as.factor(DUALCITREC) + as.factor(MUNICIPALITY) + as.factor(ORIGIN),
                     data = main_dataset, cluster = ID)
length(dataset_main$ID)
length(unique(dataset_main$ID))


#############     
# Table 3.3 #
############# 

#municipality and origin country fixed-effects regression
result3.3 <- lm(NATURALISATION ~ POST_2010:HH_INCOME_MODAL + GENDER + YSM + YSM2 + AGEARRIVAL + AGEARRIVAL2 +
                as.factor(PARTNER) + CHILD + EMPLOYMENT + HH_INCOME + as.factor(EDUCATION) +
                as.factor(DUALCITREC) + as.factor(MUNICIPALITY) + as.factor(ORIGIN),
                data = main_dataset, cluster = ID)
length(dataset_main$ID)
length(unique(dataset_main$ID))
