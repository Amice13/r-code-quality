## Micro-foundation of Quest for Status: Testing StatusPerception and Multilateral Use of Force
## Yuji Masumura and Atsushi Tago




# Setting Up --------------------------------------------------------------
rm(list = ls())

library(tidyverse)
library(ggplot2)
library(summarytools)
library(stargazer)
library(cobalt)
library(tableone)
library(patchwork)

# Loading data ------------------------------------------------------------
data <- read_csv("preprocessed_data.csv")

# Descriptive Statistics --------------------------------------------------
descr(data)
data %>% select(DV1, DV2, DV3, DV4, DV5, DV6, DV7, DV8, DV9, DV10, DV11, DV12,knowledge, check2,
                education, gender, interest, leader_sup, hawk_dove, priming_1, priming_2, priming_3, age_group) %>% 
  freq(cumul = FALSE,headings = FALSE)


# Main Analysis 1 DV1 (Figure 1)-----------------------------------------------------------

## Figure 1
##graph of pooled two groups (treatment and control, pooled)
ggplot(data = data %>% filter(groups==1|groups==2|groups==3|groups==4),  aes(x=DV1, y= ..count../sum(..count..)))+
  geom_bar(position = "dodge")+
  ylim(0,0.5)+
  coord_flip()+
  labs(title="Treatment")+
  scale_x_discrete(limits=c("Do not want to answer", "Do not know", "Do not support", "Support"))+
  ylab("Percentage")+
  theme_classic()+
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = 0, hjust = -0.25, size = 5.5)+
  theme(axis.title.y = element_blank(), text = element_text(size = 13), axis.text = element_text(size = 15))+
  
  
  ggplot(data =data %>% filter(groups==5|groups==6), aes(x=DV1, y= ..count../sum(..count..)))+
  geom_bar(position = "dodge")+
  ylim(0,0.5)+
  coord_flip()+
  labs(title="Control")+
  scale_x_discrete(limits=c("Do not want to answer", "Do not know", "Do not support", "Support"))+
  ylab("Percentage")+
  theme_classic()+
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = 0, hjust=-0.25, size = 5.5)+
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(),
        text = element_text(size = 13), axis.text = element_text(size = 15))


# Main Analysis 2 DV2 (Figure 2, 3)-----------------------------------------------------

## Figure 2
## graphs of the descriptive statistics of the four group (UN treatment, US treatment, UN control, US control)
ggplot(data =data %>% filter(groups==1|groups==3),  aes(x=DV2, y= ..count../sum(..count..)))+
  geom_bar(position = "dodge")+
  ylim(0,0.7)+
  coord_flip()+
  labs(title="UN Treatment")+
  scale_x_discrete(limits=c("Have an impact of decreasing status", "Do not have an impact", "Have an impact of increasing status"))+
  ylab("Percentage")+
  theme_classic()+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),4)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = 0, hjust = -0.1, size = 4.5)+
  theme(axis.title.y = element_blank(), text = element_text(size = 13),axis.text = element_text(size = 15))+
  
  
  ggplot(data =data %>% filter(groups==2|groups==4), aes(x=DV2 ,y= ..count../sum(..count..)))+
  geom_bar(position = "dodge")+
  ylim(0,0.7)+
  coord_flip()+
  labs(title="US Treatment")+
  scale_x_discrete(limits=c("Have an impact of decreasing status", "Do not have an impact", "Have an impact of increasing status"))+
  ylab("Percentage")+
  theme_classic()+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),4)),
                y= ((..count..)/sum(..count..))), stat="count",
          vjust = 0, hjust=-0.1, size =4.5)+
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), 
     text = element_text(size = 13), axis.text = element_text(size = 15))+
  
  
  ggplot(data =data %>% filter(groups==5), aes(x=DV2 ,y= ..count../sum(..count..)))+
  geom_bar(position = "dodge")+
  ylim(0,0.7)+
  coord_flip()+
  labs(title="UN Coontrol")+
  scale_x_discrete(limits=c("Have an impact of decreasing status", "Do not have an impact", "Have an impact of increasing status"))+
  ylab("Percentage")+
  theme_classic()+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),4)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = 0, hjust=-0.1, size = 4.5)+
  theme(axis.title.y = element_blank(), text = element_text(size = 13), axis.text = element_text(size = 15))+
  
  ggplot(data =data %>% filter(groups==6), aes(x=DV2 ,y= ..count../sum(..count..)))+
  geom_bar(position = "dodge")+
  ylim(0,0.7)+
  coord_flip()+
  labs(title="US Control")+
  scale_x_discrete(limits=c("Have an impact of decreasing status", "Do not have an impact", "Have an impact of increasing status"))+
  ylab("Percentage")+
  theme_classic()+
  annotate("text", x=3,   y=0.24, label="17%", size = 4.5)+ 
  annotate("text", x=2,   y=0.65, label="58%", size = 4.5)+
  annotate("text", x=1,   y=0.32, label="25%", size = 4.5)+
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(),
       text = element_text(size = 13),  axis.text = element_text(size = 15))



##Prop.test for Figure 3

## increasing status
### UN
T_UNimp_suc <- nrow(data %>% filter((groups==1|groups==3) & DV2=="Have an impact of increasing status"))
T_UNimp_total <- nrow(data %>% filter(groups==1|groups==3))

C_UNimp_suc <- nrow(data %>% filter(groups==5 & DV2=="Have an impact of increasing status"))
C_UNimp_total <- nrow(data %>% filter(groups==5))

UNimp_test <- prop.test(c(T_UNimp_suc, C_UNimp_suc ), c(T_UNimp_total, C_UNimp_total))

### US
T_USimp_suc <- nrow(data %>% filter((groups==2|groups==4) & DV2=="Have an impact of increasing status"))
T_USimp_total <- nrow(data %>% filter((groups==2|groups==4)))

C_USimp_suc <- nrow(data %>% filter(groups==6 & DV2=="Have an impact of increasing status"))
C_USimp_total <- nrow(data %>% filter(groups==6))

USimp_test <- prop.test(c(T_USimp_suc, C_USimp_suc ), c(T_USimp_total, C_USimp_total))
USimp_test



## decreasining
### UN
T_UNdeimp_suc <- nrow(data %>% filter((groups==1|groups==3) & DV2=="Have an impact of decreasing status"))
T_UNdeimp_total <- nrow(data %>% filter(groups==1|groups==3))

C_UNdeimp_suc <- nrow(data %>% filter(groups==5 & DV2=="Have an impact of decreasing status"))
C_UNdeimp_total <- nrow(data %>% filter(groups==5))

UNdeimp_test <- prop.test(c(T_UNdeimp_suc, C_UNdeimp_suc ), c(T_UNdeimp_total, C_UNdeimp_total))
UNdeimp_test

### US
T_USdeimp_suc <- nrow(data %>% filter((groups==2|groups==4) & DV2=="Have an impact of decreasing status"))
T_USdeimp_total <- nrow(data %>% filter(groups==2|groups==4))

C_USdeimp_suc <- nrow(data %>% filter(groups==6 & DV2=="Have an impact of decreasing status"))
C_USdeimp_total <- nrow(data %>% filter(groups==6))

USdeimp_test <- prop.test(c(T_USdeimp_suc, C_USdeimp_suc ), c(T_USdeimp_total, C_USdeimp_total))
USdeimp_test



## no change
###UN
T_UNnoimp_suc <- nrow(data %>% filter((groups==1|groups==3) & DV2=="Do not have an impact"))
T_UNnoimp_total <- nrow(data %>% filter((groups==1|groups==3)))

C_UNnoimp_suc <- nrow(data %>% filter(groups==5 & DV2=="Do not have an impact"))
C_UNnoimp_total <- nrow(data %>% filter(groups==5))

UNnoimp_test <- prop.test(c(T_UNnoimp_suc, C_UNnoimp_suc ), c(T_UNnoimp_total, C_UNnoimp_total))
UNnoimp_test

### US
T_USnoimp_suc <- nrow(data %>% filter((groups==2|groups==4) & DV2=="Do not have an impact"))
T_USnoimp_total <- nrow(data %>% filter(groups==2|groups==4))

C_USnoimp_suc <- nrow(data %>% filter(groups==6 & DV2=="Do not have an impact"))
C_USnoimp_total <- nrow(data %>% filter(groups==6))

USnoimp_test <- prop.test(c(T_USnoimp_suc, C_USnoimp_suc ), c(T_USnoimp_total, C_USnoimp_total))
USnoimp_test

## Figure 3
impact <- c("Expect Status Increase", "Expect Status Increase","Expect No Status Change","Expect No Status Change",  "Expect Status Decrease", "Expect Status Decrease")
UNUS <- c("UN Peacekeeping", "US Coalition", "UN Peacekeeping", "US Coalition","UN Peacekeeping", "US Coalition")
conf_up <- c(UNimp_test$conf.int[2], USimp_test$conf.int[2], UNnoimp_test$conf.int[2], USnoimp_test$conf.int[2], UNdeimp_test$conf.int[2], USdeimp_test$conf.int[2])
conf_dw <- c(UNimp_test$conf.int[1], USimp_test$conf.int[1], UNnoimp_test$conf.int[1], USnoimp_test$conf.int[1], UNdeimp_test$conf.int[1], USdeimp_test$conf.int[1])
estimate <- c(UNimp_test$estimate[1] - UNimp_test$estimate[2], USimp_test$estimate[1] - USimp_test$estimate[2], 
              UNnoimp_test$estimate[1] - UNnoimp_test$estimate[2], USnoimp_test$estimate[1] - USnoimp_test$estimate[2],
              UNdeimp_test$estimate[1] - UNdeimp_test$estimate[2], USdeimp_test$estimate[1] - USdeimp_test$estimate[2])
sig <- c(1,1,0,0,1,1)

names(estimate) <- NULL

df <- data_frame(impact, UNUS, conf_up = conf_up, conf_dw = conf_dw, estimate = estimate, sig = as.factor(sig))


ggplot(df, aes(x=impact, y=estimate, shape = UNUS, color=sig))+
  geom_point(position = position_dodge(width = 0.3), size=2.5)+
  geom_errorbar(aes(ymin= conf_dw, ymax = conf_up), width=0.1,  position = position_dodge(width = 0.3))+
  scale_x_discrete(limits=c("Expect Status Decrease", "Expect No Status Change", "Expect Status Increase"), 　　labels=c("Expect  \nStatus  \nDecrease", "Expect   \nNo Status\n Change   ", "Expect  \nStatus  \nIncrease"))+
  geom_hline(yintercept=0, linetype="dashed")+
  labs(shape = "Type of use of force", y= "Difference bet. treatment (\"join\") and control (\"not join\")", x="")+
  theme_classic()+
  theme(legend.position = c(0.8, 0.2), text = element_text(size = 16), axis.text = element_text(size = 17))+
  coord_flip()+
  scale_color_manual(values = c("grey", "black"))+
  guides(color = FALSE, shape =guide_legend(reverse = TRUE))+
  annotate("text", x=2.8,   y=df$estimate[1], label="0.20", size = 5) +
  annotate("text", x=3.2,   y=df$estimate[2], label="0.15", size = 5) +
  annotate("text", x=1.8,   y=df$estimate[3], label="-0.04", size = 5) +
  annotate("text", x=2.2,   y=df$estimate[4], label="-0.04", size = 5) +
  annotate("text", x=0.8,   y=df$estimate[5], label="-0.16", size = 5) +
  annotate("text", x=1.2,   y=df$estimate[6], label="-0.11", size = 5) 


# Main Analysis 3 Stay and Withdraw (Figure 4)---------------------------------------

## prop.test for Figure 4

## increasing status
##UN
T_UNimp_suc_11 <- nrow(data %>% filter(groups==1 & DV11=="Have an impact of increasing status"))
T_UNimp_total_11 <- nrow(data %>% filter(groups==1))

C_UNimp_suc_11 <- nrow(data %>% filter(groups==3  & DV11=="Have an impact of increasing status"))
C_UNimp_total_11 <- nrow(data %>% filter(groups==3))

UNimp_test_11 <- prop.test(c(T_UNimp_suc_11, C_UNimp_suc_11 ), c(T_UNimp_total_11, C_UNimp_total_11))
UNimp_test_11

##US
T_USimp_suc_11 <- nrow(data %>% filter(groups==2 & DV11=="Have an impact of increasing status"))
T_USimp_total_11 <- nrow(data %>% filter(groups==2))

C_USimp_suc_11 <- nrow(data %>% filter(groups==4 & DV11=="Have an impact of increasing status"))
C_USimp_total_11 <- nrow(data %>% filter(groups==4))

USimp_test_11 <- prop.test(c(T_USimp_suc_11, C_USimp_suc_11 ), c(T_USimp_total_11, C_USimp_total_11))
USimp_test_11

## decreasing
## UN
T_UNdeimp_suc_11 <- nrow(data %>% filter(groups==1 & DV11=="Have an impact of decreasing status"))
T_UNdeimp_total_11 <- nrow(data %>% filter(groups==1))

C_UNdeimp_suc_11 <- nrow(data %>% filter(groups==3 & DV11=="Have an impact of decreasing status"))
C_UNdeimp_total_11 <- nrow(data %>% filter(groups==3))

UNdeimp_test_11 <- prop.test(c(T_UNdeimp_suc_11, C_UNdeimp_suc_11 ), c(T_UNdeimp_total_11, C_UNdeimp_total_11))
UNdeimp_test_11

## US
T_USdeimp_suc_11 <- nrow(data %>% filter(groups==2 & DV11=="Have an impact of decreasing status"))
T_USdeimp_total_11 <- nrow(data %>% filter(groups==2))

C_USdeimp_suc_11 <- nrow(data %>% filter(groups==4 & DV11=="Have an impact of decreasing status"))
C_USdeimp_total_11 <- nrow(data %>% filter(groups==4))

USdeimp_test_11 <- prop.test(c(T_USdeimp_suc_11, C_USdeimp_suc_11 ), c(T_USdeimp_total_11, C_USdeimp_total_11))
USdeimp_test_11

## no change
##UN
T_UNnoimp_suc_11 <- nrow(data %>% filter(groups==1 & DV11=="Do not have an impact"))
T_UNnoimp_total_11 <- nrow(data %>% filter(groups==1))

C_UNnoimp_suc_11 <- nrow(data %>% filter(groups==3 & DV11=="Do not have an impact"))
C_UNnoimp_total_11 <- nrow(data %>% filter(groups==3))

UNnoimp_test_11 <- prop.test(c(T_UNnoimp_suc_11, C_UNnoimp_suc_11 ), c(T_UNnoimp_total_11, C_UNnoimp_total_11))
UNnoimp_test_11

## US
T_USnoimp_suc_11 <- nrow(data %>% filter(groups==2 & DV11=="Do not have an impact"))
T_USnoimp_total_11 <- nrow(data %>% filter(groups==2))

C_USnoimp_suc_11 <- nrow(data %>% filter(groups==4 & DV11=="Do not have an impact"))
C_USnoimp_total_11 <- nrow(data %>% filter(groups==4))

USnoimp_test_11 <- prop.test(c(T_USnoimp_suc_11, C_USnoimp_suc_11 ), c(T_USnoimp_total_11, C_USnoimp_total_11))
USnoimp_test_11

#Figure 4
##Visualizing the previous results

impact <- c("Expect Status Increase", "Expect Status Increase",
            "Expect No Status Change","Expect No Status Change",
            "Expect Status Decrease", "Expect Status Decrease")

UNUS <- c("UN Peacekeeping", "US Coalition", "UN Peacekeeping", "US Coalition","UN Peacekeeping", "US Coalition")

conf_up <- c(UNimp_test_11$conf.int[2], USimp_test_11$conf.int[2],
             UNnoimp_test_11$conf.int[2], USnoimp_test_11$conf.int[2],
             UNdeimp_test_11$conf.int[2], USdeimp_test_11$conf.int[2])

conf_dw <- c(UNimp_test_11$conf.int[1], USimp_test_11$conf.int[1],
             UNnoimp_test_11$conf.int[1], USnoimp_test_11$conf.int[1], 
             UNdeimp_test_11$conf.int[1], USdeimp_test_11$conf.int[1])


estimate <- c(UNimp_test_11$estimate[1] - UNimp_test_11$estimate[2],
              USimp_test_11$estimate[1] - USimp_test_11$estimate[2], 
              UNnoimp_test_11$estimate[1] - UNnoimp_test_11$estimate[2],
              USnoimp_test_11$estimate[1] - USnoimp_test_11$estimate[2],
              UNdeimp_test_11$estimate[1] - UNdeimp_test_11$estimate[2],
              USdeimp_test_11$estimate[1] - USdeimp_test_11$estimate[2])

sig <- c(0,0,0,0,0,0)

names(estimate) <- NULL

df <- data_frame(impact, UNUS, conf_up, conf_dw, estimate, sig = as.factor(sig))


ggplot(df, aes(x=impact, y=estimate, shape = UNUS, color=sig))+
  geom_point(position = position_dodge(width = 0.3), size=2.5)+
  geom_errorbar(aes(ymin= conf_dw, ymax = conf_up), width=0.1,  position = position_dodge(width = 0.3))+
  scale_x_discrete(limits=c("Expect Status Decrease", "Expect No Status Change", "Expect Status Increase"), 　　labels=c("Expect  \nStatus  \nDecrease", "Expect   \nNo Status\n Change   ", "Expect  \nStatus  \nIncrease"))+
  geom_hline(yintercept=0, linetype="dashed")+
  labs(shape = "Type of use of force", y= "Difference bet. continuation and withdrawal", x="")+
  theme_classic()+
  theme(legend.position = c(0.8, 0.2),text = element_text(size = 16), axis.text = element_text(size = 17))+
  coord_flip()+
  scale_color_manual(values = c("black"))+
  guides(color = FALSE, shape =guide_legend(reverse = TRUE))+
  annotate("text", x=2.8,   y=df$estimate[1], label="0.16", size = 5) +
  annotate("text", x=3.2,   y=df$estimate[2], label="0.20", size = 5) +
  annotate("text", x=1.8,   y=df$estimate[3], label="-0.09", size = 5) +
  annotate("text", x=2.2,   y=df$estimate[4], label="-0.10", size = 5) +
  annotate("text", x=0.8,   y=df$estimate[5], label="-0.07", size = 5) +
  annotate("text", x=1.2,   y=df$estimate[6], label="-0.10", size = 5) 


# Main Analysis 4 SDO and DV2, DV11(Figure 5, 6)----------------------------------------------------

#Prop Test for Figure 5

#subsetting: high, low, and middle SDO
data_high <-  data %>% filter(SDO_mean > mean(data$SDO_mean) + sd(data$SDO_mean))

data_low <- data %>% filter(SDO_mean < mean(SDO_mean) - sd(SDO_mean))

data_middle <- data %>% filter(SDO_mean <= mean(SDO_mean) + sd(SDO_mean) & 
                                 SDO_mean >= mean(SDO_mean) - sd(SDO_mean))



## high SDO, DV2
## increasing status
T_imp_suc_hi <- nrow(data_high %>% filter((groups==1|groups==2|groups==3|groups==4) & DV2=="Have an impact of increasing status"))
T_imp_total_hi <- nrow(data_high %>% filter((groups==1|groups==2|groups==3|groups==4)))

C_imp_suc_hi <- nrow(data_high %>% filter((groups==5|groups==6) & DV2=="Have an impact of increasing status"))
C_imp_total_hi <- nrow(data_high %>% filter(groups==5|groups==6))

imp_test_hi <- prop.test(c(T_imp_suc_hi, C_imp_suc_hi ), c(T_imp_total_hi, C_imp_total_hi))
imp_test_hi




## has no impact on status
T_noimp_suc_hi <- nrow(data_high %>% filter((groups==1|groups==2|groups==3|groups==4)  & DV2=="Do not have an impact"))
T_noimp_total_hi <- nrow(data_high %>% filter(groups==1|groups==2|groups==3|groups==4))

C_noimp_suc_hi <- nrow(data_high %>% filter((groups==5|groups==6) & DV2=="Do not have an impact"))
C_noimp_total_hi <- nrow(data_high %>% filter(groups==5|groups==6))

noimp_test_hi <- prop.test(c(T_noimp_suc_hi, C_noimp_suc_hi ), c(T_noimp_total_hi, C_noimp_total_hi))
noimp_test_hi



## decreasing status
T_deimp_suc_hi <- nrow(data_high %>% filter((groups==1|groups==2|groups==3|groups==4)  & DV2=="Have an impact of decreasing status"))
T_deimp_total_hi <- nrow(data_high %>% filter((groups==1|groups==2|groups==3|groups==4)))

C_deimp_suc_hi <- nrow(data_high %>% filter((groups==5|groups==6) & DV2=="Have an impact of decreasing status"))
C_deimp_total_hi <- nrow(data_high %>% filter(groups==5|groups==6))

deimp_test_hi <- prop.test(c(T_deimp_suc_hi, C_deimp_suc_hi ), c(T_deimp_total_hi, C_deimp_total_hi))
deimp_test_hi



## middle SDO, DV2
## increasing status
T_imp_suc_mi <- nrow(data_middle %>% filter((groups==1|groups==2|groups==3|groups==4) & DV2=="Have an impact of increasing status"))
T_imp_total_mi <- nrow(data_middle %>% filter(groups==1|groups==2|groups==3|groups==4))

C_imp_suc_mi <- nrow(data_middle %>% filter((groups==5|groups==6) & DV2=="Have an impact of increasing status"))
C_imp_total_mi <- nrow(data_middle %>% filter(groups==5|groups==6))

imp_test_mi <- prop.test(c(T_imp_suc_mi, C_imp_suc_mi ), c(T_imp_total_mi, C_imp_total_mi))
imp_test_mi



## has no impact on status
T_noimp_suc_mi <- nrow(data_middle %>% filter((groups==1|groups==2|groups==3|groups==4)  & DV2=="Do not have an impact"))
T_noimp_total_mi <- nrow(data_middle %>% filter((groups==1|groups==2|groups==3|groups==4) ))

C_noimp_suc_mi <- nrow(data_middle %>% filter((groups==5|groups==6) & DV2=="Do not have an impact"))
C_noimp_total_mi <- nrow(data_middle %>% filter(groups==5|groups==6))

noimp_test_mi <- prop.test(c(T_noimp_suc_mi, C_noimp_suc_mi ), c(T_noimp_total_mi, C_noimp_total_mi))
noimp_test_mi


## decreasing status
T_deimp_suc_mi <- nrow(data_middle %>% filter((groups==1|groups==2|groups==3|groups==4)  & DV2=="Have an impact of decreasing status"))
T_deimp_total_mi <- nrow(data_middle %>% filter((groups==1|groups==2|groups==3|groups==4) ))

C_deimp_suc_mi <- nrow(data_middle %>% filter((groups==5|groups==6) & DV2=="Have an impact of decreasing status"))
C_deimp_total_mi <- nrow(data_middle %>% filter(groups==5|groups==6))

deimp_test_mi <- prop.test(c(T_deimp_suc_mi, C_deimp_suc_mi ), c(T_deimp_total_mi, C_deimp_total_mi))
deimp_test_mi


## low SDO, DV2
## increasing status
T_imp_suc_lo <- nrow(data_low %>% filter((groups==1|groups==2|groups==3|groups==4)  & DV2=="Have an impact of increasing status"))
T_imp_total_lo <- nrow(data_low %>% filter((groups==1|groups==2|groups==3|groups==4) ))

C_imp_suc_lo <- nrow(data_low %>% filter((groups==5|groups==6) & DV2=="Have an impact of increasing status"))
C_imp_total_lo <- nrow(data_low %>% filter(groups==5|groups==6))

imp_test_lo <- prop.test(c(T_imp_suc_lo, C_imp_suc_lo ), c(T_imp_total_lo, C_imp_total_lo))
imp_test_lo



## has no impact on status
T_noimp_suc_lo <- nrow(data_low %>% filter((groups==1|groups==2|groups==3|groups==4)  & DV2=="Do not have an impact"))
T_noimp_total_lo <- nrow(data_low %>% filter((groups==1|groups==2|groups==3|groups==4) ))

C_noimp_suc_lo <- nrow(data_low %>% filter((groups==5|groups==6) & DV2=="Do not have an impact"))
C_noimp_total_lo <- nrow(data_low %>% filter(groups==5|groups==6))

noimp_test_lo <- prop.test(c(T_noimp_suc_lo, C_noimp_suc_lo), c(T_noimp_total_lo, C_noimp_total_lo))
noimp_test_lo


## decreasing status
T_deimp_suc_lo <- nrow(data_low %>% filter((groups==1|groups==2|groups==3|groups==4 ) & DV2=="Have an impact of decreasing status"))
T_deimp_total_lo <- nrow(data_low %>% filter((groups==1|groups==2|groups==3|groups==4) ))

C_deimp_suc_lo <- nrow(data_low %>% filter((groups==5|groups==6) & DV2=="Have an impact of decreasing status"))
C_deimp_total_lo <- nrow(data_low %>% filter(groups==5|groups==6))

deimp_test_lo <- prop.test(c(T_deimp_suc_lo, C_deimp_suc_lo ), c(T_deimp_total_lo, C_deimp_total_lo))
deimp_test_lo


## Figure 5
##Visualizing the previous results

impact <- c("Expect Status Increase", "Expect Status Increase", "Expect Status Increase",
            "Expect No Status Change","Expect No Status Change", "Expect No Status Change",
            "Expect Status Decrease", "Expect Status Decrease","Expect Status Decrease")

SDO <- c("High SDO", "Middle SDO", "Low SDO", "High SDO", "Middle SDO", "Low SDO","High SDO", "Middle SDO", "Low SDO")

conf_up <- c(  imp_test_hi$conf.int[2],     imp_test_mi$conf.int[2],   imp_test_lo$conf.int[2],
               noimp_test_hi$conf.int[2], noimp_test_mi$conf.int[2],   noimp_test_lo$conf.int[2],
               deimp_test_hi$conf.int[2], deimp_test_mi$conf.int[2],   deimp_test_lo$conf.int[2])

conf_dw <- c(  imp_test_hi$conf.int[1],   imp_test_mi$conf.int[1],     imp_test_lo$conf.int[1],
               noimp_test_hi$conf.int[1], noimp_test_mi$conf.int[1],   noimp_test_lo$conf.int[1],
               deimp_test_hi$conf.int[1], deimp_test_mi$conf.int[1],   deimp_test_lo$conf.int[1])

estimate <- c(imp_test_hi$estimate[1] - imp_test_hi$estimate[2],
              imp_test_mi$estimate[1] - imp_test_mi$estimate[2],
              imp_test_lo$estimate[1] - imp_test_lo$estimate[2],
              noimp_test_hi$estimate[1] - noimp_test_hi$estimate[2], 
              noimp_test_mi$estimate[1] - noimp_test_mi$estimate[2],
              noimp_test_lo$estimate[1] - noimp_test_lo$estimate[2],
              deimp_test_hi$estimate[1] - deimp_test_hi$estimate[2],
              deimp_test_mi$estimate[1] - deimp_test_mi$estimate[2],
              deimp_test_lo$estimate[1] - deimp_test_lo$estimate[2])

sig <- c(1,1,1,0,0,0, 1, 1, 1)

names(estimate) <- NULL

df <- data_frame(impact, SDO, conf_up, conf_dw, estimate, sig = as.factor(sig))

df$SDO <- factor(df$SDO, levels = c("Low SDO", "Middle SDO", "High SDO"))


ggplot(df, aes(x=impact, y=estimate, shape = SDO, color=sig))+
  geom_point(position = position_dodge(width = 0.6), size=2.5)+
  geom_errorbar(aes(ymin= conf_dw, ymax = conf_up), width=0.1,  position = position_dodge(width = 0.6))+
  scale_x_discrete(limits=c("Expect Status Decrease", "Expect No Status Change", "Expect Status Increase"), 　　labels=c("Expect  \nStatus  \nDecrease", "Expect   \nNo Status\n Change   ", "Expect  \nStatus  \nIncrease"))+
  geom_hline(yintercept=0, linetype="dashed")+
  labs(shape = "SDO", y= "Difference bet. treatment (\"join\") and control (\"not join\")", x="")+
  theme_classic()+
  theme(legend.position = c(0.8, 0.2),text = element_text(size = 16), axis.text = element_text(size = 17))+
  coord_flip()+
  scale_color_manual(values = c("grey", "black"))+
  guides(color = FALSE, shape =guide_legend(reverse = TRUE))+ 
  annotate("text", x=3.35,   y=df$estimate[1], label="0.40", size = 5) +
  annotate("text", x=3.15,   y=df$estimate[2], label="0.15", size = 5) +
  annotate("text", x=2.65,   y=df$estimate[3], label="0.19", size = 5) +
  annotate("text", x=2.35,   y=df$estimate[4], label="-0.11", size = 5) +
  annotate("text", x=2.1,   y=df$estimate[5]-0.02, label="-0.02", size = 5) +
  annotate("text", x=1.65,   y=df$estimate[6], label="-0.06", size = 5)+
  annotate("text", x=1.35,   y=df$estimate[7], label="-0.29", size = 5) +
  annotate("text", x=1.12,   y=df$estimate[8], label="-0.12", size = 5) +
  annotate("text", x=0.65,   y=df$estimate[9], label="-0.13", size = 5)


## Prop. test for Figure 6
#High SDO
## increasing status
##UN
T_imp_suc_11_hi <- nrow(data_high %>% filter((groups==1| groups==2) & DV11=="Have an impact of increasing status"))
T_imp_total_11_hi <- nrow(data_high %>% filter(groups==1 | groups==2))

C_imp_suc_11_hi <- nrow(data_high %>% filter((groups==3 | groups==4)  & DV11=="Have an impact of increasing status"))
C_imp_total_11_hi <- nrow(data_high %>% filter(groups==3| groups==4))

imp_test_11_hi <- prop.test(c(T_imp_suc_11_hi, C_imp_suc_11_hi ), c(T_imp_total_11_hi, C_imp_total_11_hi))
imp_test_11_hi

## prop.test for each treatment status

## no change in status
T_noimp_suc_11_hi <- nrow(data_high %>% filter((groups==1| groups==2) & DV11=="Do not have an impact"))
T_noimp_total_11_hi <- nrow(data_high %>% filter(groups==1| groups==2))

C_noimp_suc_11_hi <- nrow(data_high %>% filter((groups==3| groups==4)  & DV11=="Do not have an impact"))
C_noimp_total_11_hi <- nrow(data_high %>% filter(groups==3| groups==4))

noimp_test_11_hi <- prop.test(c(T_noimp_suc_11_hi, C_noimp_suc_11_hi ), c(T_noimp_total_11_hi, C_noimp_total_11_hi))
noimp_test_11_hi


## decreasing status
T_deimp_suc_11_hi <- nrow(data_high %>% filter((groups==1| groups==2) & DV11=="Have an impact of decreasing status"))
T_deimp_total_11_hi <- nrow(data_high %>% filter(groups==1| groups==2))

C_deimp_suc_11_hi <- nrow(data_high %>% filter((groups==3| groups==4)  & DV11=="Have an impact of decreasing status"))
C_deimp_total_11_hi <- nrow(data_high %>% filter(groups==3| groups==4))

deimp_test_11_hi <- prop.test(c(T_deimp_suc_11_hi, C_deimp_suc_11_hi ), c(T_deimp_total_11_hi, C_deimp_total_11_hi))
deimp_test_11_hi


## increasing status
##UN
T_imp_suc_11_mi <- nrow(data_middle %>% filter((groups==1| groups==2) & DV11=="Have an impact of increasing status"))
T_imp_total_11_mi <- nrow(data_middle %>% filter(groups==1| groups==2))

C_imp_suc_11_mi <- nrow(data_middle %>% filter((groups==3| groups==4)  & DV11=="Have an impact of increasing status"))
C_imp_total_11_mi <- nrow(data_middle %>% filter(groups==3| groups==4))

imp_test_11_mi <- prop.test(c(T_imp_suc_11_mi, C_imp_suc_11_mi ), c(T_imp_total_11_mi, C_imp_total_11_mi))
imp_test_11_mi


## no change in status
T_noimp_suc_11_mi <- nrow(data_middle %>% filter((groups==1| groups==2) & DV11=="Do not have an impact"))
T_noimp_total_11_mi <- nrow(data_middle %>% filter(groups==1| groups==2))

C_noimp_suc_11_mi <- nrow(data_middle %>% filter((groups==3| groups==4)  & DV11=="Do not have an impact"))
C_noimp_total_11_mi <- nrow(data_middle %>% filter(groups==3| groups==4))

noimp_test_11_mi <- prop.test(c(T_noimp_suc_11_mi, C_noimp_suc_11_mi ), c(T_noimp_total_11_mi, C_noimp_total_11_mi))
noimp_test_11_mi


## decreasing status
T_deimp_suc_11_mi <- nrow(data_middle %>% filter((groups==1| groups==2) & DV11=="Have an impact of decreasing status"))
T_deimp_total_11_mi <- nrow(data_middle %>% filter(groups==1| groups==2))

C_deimp_suc_11_mi <- nrow(data_middle %>% filter((groups==3| groups==4)  & DV11=="Have an impact of decreasing status"))
C_deimp_total_11_mi <- nrow(data_middle %>% filter(groups==3| groups==4))

deimp_test_11_mi <- prop.test(c(T_deimp_suc_11_mi, C_deimp_suc_11_mi ), c(T_deimp_total_11_mi, C_deimp_total_11_mi))
deimp_test_11_mi


#Low SDO
## increasing status
T_imp_suc_11_lo <- nrow(data_low %>% filter((groups==1| groups==2) & DV11=="Have an impact of increasing status"))
T_imp_total_11_lo <- nrow(data_low %>% filter(groups==1| groups==2))

C_imp_suc_11_lo <- nrow(data_low %>% filter((groups==3| groups==4)  & DV11=="Have an impact of increasing status"))
C_imp_total_11_lo <- nrow(data_low %>% filter(groups==3| groups==4))

imp_test_11_lo <- prop.test(c(T_imp_suc_11_lo, C_imp_suc_11_lo ), c(T_imp_total_11_lo, C_imp_total_11_lo))
imp_test_11_lo

## no change in status
T_noimp_suc_11_lo <- nrow(data_low %>% filter((groups==1| groups==2) & DV11=="Do not have an impact"))
T_noimp_total_11_lo <- nrow(data_low %>% filter(groups==1| groups==2))

C_noimp_suc_11_lo <- nrow(data_low %>% filter((groups==3| groups==4)  & DV11=="Do not have an impact"))
C_noimp_total_11_lo <- nrow(data_low %>% filter(groups==3| groups==4))

noimp_test_11_lo <- prop.test(c(T_noimp_suc_11_lo, C_noimp_suc_11_lo ), c(T_noimp_total_11_lo, C_noimp_total_11_lo))
noimp_test_11_lo

## decreasing status
T_deimp_suc_11_lo <- nrow(data_low %>% filter((groups==1| groups==2) & DV11=="Have an impact of decreasing status"))
T_deimp_total_11_lo <- nrow(data_low %>% filter(groups==1| groups==2))

C_deimp_suc_11_lo <- nrow(data_low %>% filter((groups==3| groups==4)  & DV11=="Have an impact of decreasing status"))
C_deimp_total_11_lo <- nrow(data_low %>% filter(groups==3| groups==4))

deimp_test_11_lo <- prop.test(c(T_deimp_suc_11_lo, C_deimp_suc_11_lo ), c(T_deimp_total_11_lo, C_deimp_total_11_lo))
deimp_test_11_lo

#Figure 6
##Visualizing the previous results

impact <- c("Expect Status Increase", "Expect Status Increase", "Expect Status Increase",
            "Expect No Status Change","Expect No Status Change", "Expect No Status Change",
            "Expect Status Decrease", "Expect Status Decrease","Expect Status Decrease")

SDO <- c("High SDO", "Middle SDO", "Low SDO", "High SDO", "Middle SDO", "Low SDO","High SDO", "Middle SDO", "Low SDO")

conf_up <- c(  imp_test_11_hi$conf.int[2],     imp_test_11_mi$conf.int[2],   imp_test_11_lo$conf.int[2],
               noimp_test_11_hi$conf.int[2], noimp_test_11_mi$conf.int[2],   noimp_test_11_lo$conf.int[2],
               deimp_test_11_hi$conf.int[2], deimp_test_11_mi$conf.int[2],   deimp_test_11_lo$conf.int[2])

conf_dw <- c(  imp_test_11_hi$conf.int[1],   imp_test_11_mi$conf.int[1],     imp_test_11_lo$conf.int[1],
               noimp_test_11_hi$conf.int[1], noimp_test_11_mi$conf.int[1],   noimp_test_11_lo$conf.int[1],
               deimp_test_11_hi$conf.int[1], deimp_test_11_mi$conf.int[1],   deimp_test_11_lo$conf.int[1])

estimate <- c(imp_test_11_hi$estimate[1] - imp_test_11_hi$estimate[2],
              imp_test_11_mi$estimate[1] - imp_test_11_mi$estimate[2],
              imp_test_11_lo$estimate[1] - imp_test_11_lo$estimate[2],
              noimp_test_11_hi$estimate[1] - noimp_test_11_hi$estimate[2], 
              noimp_test_11_mi$estimate[1] - noimp_test_11_mi$estimate[2],
              noimp_test_11_lo$estimate[1] - noimp_test_11_lo$estimate[2],
              deimp_test_11_hi$estimate[1] - deimp_test_11_hi$estimate[2],
              deimp_test_11_mi$estimate[1] - deimp_test_11_mi$estimate[2],
              deimp_test_11_lo$estimate[1] - deimp_test_11_lo$estimate[2])

sig <- c(1,1,1,0,1,1, 1, 1, 0)

names(estimate) <- NULL

df <- data_frame(impact, SDO, conf_up, conf_dw, estimate, sig = as.factor(sig))

df$SDO <- factor(df$SDO, levels = c("Low SDO", "Middle SDO", "High SDO"))


ggplot(df, aes(x=impact, y=estimate, shape = SDO, color=sig))+
  geom_point(position = position_dodge(width = 0.6), size=2.5)+
  geom_errorbar(aes(ymin= conf_dw, ymax = conf_up), width=0.1,  position = position_dodge(width = 0.6))+
  scale_x_discrete(limits=c("Expect Status Decrease", "Expect No Status Change", "Expect Status Increase"), 　　labels=c("Expect  \nStatus  \nDecrease", "Expect   \nNo Status\n Change   ", "Expect  \nStatus  \nIncrease"))+
  geom_hline(yintercept=0, linetype="dashed")+
  labs(shape = "SDO", y= "Difference bet. continuation and withdrawal", x="")+
  theme_classic()+
  theme(legend.position = c(0.8, 0.2), text = element_text(size = 16), axis.text = element_text(size = 17))+
  coord_flip()+
  scale_color_manual(values = c("grey", "black"))+
  guides(color = FALSE, shape =guide_legend(reverse = TRUE))+ 
  annotate("text", x=3.35,   y=df$estimate[1], label="0.34", size = 5) +
  annotate("text", x=3.1,   y=df$estimate[2], label="0.17", size = 5) +
  annotate("text", x=2.9,   y=df$estimate[3], label="0.15", size = 5) +
  annotate("text", x=2.35,   y=df$estimate[4], label="-0.03", size = 5) +
  annotate("text", x=2.11,   y=df$estimate[5], label="-0.09", size = 5) +
  annotate("text", x=1.9,   y=df$estimate[6], label="-0.14", size = 5)+
  annotate("text", x=1.35,   y=df$estimate[7], label="-0.31", size = 5) +
  annotate("text", x=1.12,   y=df$estimate[8], label="-0.08", size = 5) +
  annotate("text", x=0.9,   y=df$estimate[9]-0.03, label="-0.01", size = 5)





# Appendix ----------------------------------------------------------------

# Figure 8
## graphs of the four group (UN treatment, US treatment, UN control, and US control)
ggplot(data =data %>% filter(groups==1|groups==3),  aes(x=DV1,y= ..count../sum(..count..)))+
  geom_bar(position = "dodge")+
  ylim(0,0.5)+
  coord_flip()+
  labs(title="UN Treatment")+
  scale_x_discrete(limits=c("Do not want to answer", "Do not know", "Do not support", "Support"))+
  ylab("Percentage")+
  theme_classic()+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),2)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = 0, hjust = -0.25, size = 4.5)+
  theme(axis.title.y = element_blank(),text = element_text(size = 13), axis.text = element_text(size = 15))+
  
  
  ggplot(data =data %>% filter(groups==2|groups==4), aes(x=DV1,y= ..count../sum(..count..)))+
  geom_bar(position = "dodge")+
  ylim(0,0.5)+
  coord_flip()+
  labs(title="US Treatment")+
  scale_x_discrete(limits=c("Do not want to answer", "Do not know", "Do not support", "Support"))+
  ylab("Percentage")+
  theme_classic()+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),2)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = 0, hjust=-0.25, size = 4.5)+
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(),
        text = element_text(size = 13), axis.text = element_text(size = 15))+
  
  
  ggplot(data =data %>% filter(groups==5), aes(x=DV1,y= ..count../sum(..count..)))+
  geom_bar(position = "dodge")+
  ylim(0,0.5)+
  coord_flip()+
  labs(title="UN Coontrol")+
  scale_x_discrete(limits=c("Do not want to answer", "Do not know", "Do not support", "Support"))+
  ylab("Percentage")+
  theme_classic()+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),2)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = 0, hjust=-0.25, size = 4.5)+
  theme(axis.title.y = element_blank(),text = element_text(size = 13), axis.text = element_text(size = 15))+
  
  ggplot(data =data %>% filter(groups==6), aes(x=DV1,y= ..count../sum(..count..)))+
  geom_bar(position = "dodge")+
  ylim(0,0.5)+
  coord_flip()+
  labs(title="US Control")+
  scale_x_discrete(limits=c("Do not want to answer", "Do not know", "Do not support", "Support"))+
  ylab("Percentage")+
  theme_classic()+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),2)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = 0, hjust=-0.25, size = 4.5)+
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(),
        text = element_text(size = 13), axis.text = element_text(size = 15))


## descriptive statistics of SDO and DV2

## Figure 9
#High SDO
ggplot(data =data_high %>% filter(groups==1|groups==3|groups==2|groups==4),  aes(x=DV2, y= ..count../sum(..count..)))+
  geom_bar(position = "dodge")+
  ylim(0,0.75)+
  coord_flip()+
  labs(title="High SDO/ Treatment")+
  scale_x_discrete(limits=c("Have an impact of decreasing status", "Do not have an impact", "Have an impact of increasing status"))+
  ylab("Percentage")+
  theme_classic()+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),4)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = 0, hjust = -0.1, size = 5)+
  theme(axis.title.y = element_blank(), text = element_text(size = 13), 
        axis.text = element_text(size = 14))+
  
  
  ggplot(data =data_high %>% filter(groups==5|groups==6), aes(x=DV2 ,y= ..count../sum(..count..)))+
  geom_bar(position = "dodge")+
  ylim(0,0.75)+
  coord_flip()+
  labs(title="High SDO/ Control")+
  scale_x_discrete(limits=c("Have an impact of decreasing status", "Do not have an impact", "Have an impact of increasing status"))+
  ylab("Percentage")+
  theme_classic()+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),4)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = 0, hjust=-0.1, size = 5)+
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(),text = element_text(size = 13), 
        axis.text = element_text(size = 14))

## Figure 10
#Middle SDO
ggplot(data =data_middle %>% filter(groups==1|groups==3|groups==2|groups==4),  aes(x=DV2, y= ..count../sum(..count..)))+
  geom_bar(position = "dodge")+
  ylim(0,0.75)+
  coord_flip()+
  labs(title="Middle SDO/ Treatment")+
  scale_x_discrete(limits=c("Have an impact of decreasing status", "Do not have an impact", "Have an impact of increasing status"))+
  ylab("Percentage")+
  theme_classic()+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),4)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = 0, hjust = -0.1, size = 5)+
  theme(axis.title.y = element_blank(), text = element_text(size = 13), 
        axis.text = element_text(size = 14))+
  
  
  ggplot(data =data_middle %>% filter(groups==5|groups==6), aes(x=DV2 ,y= ..count../sum(..count..)))+
  geom_bar(position = "dodge")+
  ylim(0,0.75)+
  coord_flip()+
  labs(title="Middle SDO/ Control")+
  scale_x_discrete(limits=c("Have an impact of decreasing status", "Do not have an impact", "Have an impact of increasing status"))+
  ylab("Percentage")+
  theme_classic()+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),4)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = 0, hjust=-0.1, size = 5)+
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), text = element_text(size = 13), 
        axis.text = element_text(size = 14))


##Figure 11
#Low SDO
ggplot(data =data_low %>% filter(groups==1|groups==3|groups==2|groups==4),  aes(x=DV2, y= ..count../sum(..count..)))+
  geom_bar(position = "dodge")+
  ylim(0,0.75)+
  coord_flip()+
  labs(title="Low SDO/ Treatment")+
  scale_x_discrete(limits=c("Have an impact of decreasing status", "Do not have an impact", "Have an impact of increasing status"))+
  ylab("Percentage")+
  theme_classic()+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),4)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = 0, hjust = -0.1, size = 5)+
  theme(axis.title.y = element_blank(), text = element_text(size = 13), 
        axis.text = element_text(size = 14))+
  
  
  ggplot(data =data_low %>% filter(groups==5|groups==6), aes(x=DV2 ,y= ..count../sum(..count..)))+
  geom_bar(position = "dodge")+
  ylim(0,0.75)+
  coord_flip()+
  labs(title="Low SDO/ Control")+
  scale_x_discrete(limits=c("Have an impact of decreasing status", "Do not have an impact", "Have an impact of increasing status"))+
  ylab("Percentage")+
  theme_classic()+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),4)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = 0, hjust=-0.1, size = 5)+
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(),text = element_text(size = 13), 
        axis.text = element_text(size = 14))



## descriptive statistics of SDO and DV11

## Figure 12
## descriptive statistics of continuation and withdraw
ggplot(data =data %>% filter(groups==1),  aes(x=DV11, y= ..count../sum(..count..)))+
  geom_bar(position = "dodge")+
  ylim(0,0.75)+
  coord_flip()+
  labs(title="UN Continuation")+
  scale_x_discrete(limits=c("Have an impact of decreasing status", "Do not have an impact", "Have an impact of increasing status"))+
  ylab("Percentage")+
  theme_classic()+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),4)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = 0, hjust = -0.1)+
  theme(axis.title.y = element_blank(), text = element_text(size = 13), axis.text = element_text(size = 13))+
  
  
  ggplot(data =data %>% filter(groups==2), aes(x=DV11, y= ..count../sum(..count..)))+
  geom_bar(position = "dodge")+
  ylim(0,0.75)+
  coord_flip()+
  labs(title="US Continuation")+
  scale_x_discrete(limits=c("Have an impact of decreasing status", "Do not have an impact", "Have an impact of increasing status"))+
  ylab("Percentage")+
  theme_classic()+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),4)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = 0, hjust=-0.1)+
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(),
        text = element_text(size = 13), axis.text = element_text(size = 13))+
  
  
  ggplot(data =data %>% filter(groups==3), aes(x=DV11, y= ..count../sum(..count..)))+
  geom_bar(position = "dodge")+
  ylim(0,0.75)+
  coord_flip()+
  labs(title="UN Withdraw")+
  scale_x_discrete(limits=c("Have an impact of decreasing status", "Do not have an impact", "Have an impact of increasing status"))+
  ylab("Percentage")+
  theme_classic()+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),4)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = 0, hjust=-0.1)+
  theme(axis.title.y = element_blank(), text = element_text(size = 13), axis.text = element_text(size = 13))+
  
  ggplot(data =data %>% filter(groups==4), aes(x=DV11, y= ..count../sum(..count..)))+
  geom_bar(position = "dodge")+
  ylim(0,0.75)+
  coord_flip()+
  labs(title="US Withdraw")+
  scale_x_discrete(limits=c("Have an impact of decreasing status", "Do not have an impact", "Have an impact of increasing status"))+
  ylab("Percentage")+
  theme_classic()+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),3)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = 0, hjust=-0.1)+
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), 
        text = element_text(size = 13), axis.text = element_text(size = 13))



#high SDO
## Figure 13
ggplot(data =data_high %>% filter(groups==1|groups==2),  aes(x=DV11, y= ..count../sum(..count..)))+
  geom_bar(position = "dodge")+
  ylim(0,0.75)+
  coord_flip()+
  labs(title="High SDO, Continuation")+
  scale_x_discrete(limits=c("Have an impact of decreasing status", "Do not have an impact", "Have an impact of increasing status"))+
  ylab("Percentage")+
  theme_classic()+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),4)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = 0, hjust = -0.1)+
  theme(axis.title.y = element_blank(),text = element_text(size = 13), 
        axis.text = element_text(size = 14))+
  
  
  ggplot(data =data_high %>% filter(groups==3|groups==4), aes(x=DV11 ,y= ..count../sum(..count..)))+
  geom_bar(position = "dodge")+
  ylim(0,0.75)+
  coord_flip()+
  labs(title="High SDO, Withdraw")+
  scale_x_discrete(limits=c("Have an impact of decreasing status", "Do not have an impact", "Have an impact of increasing status"))+
  ylab("Percentage")+
  theme_classic()+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),4)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = 0, hjust=-0.1)+
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), text = element_text(size = 13), 
        axis.text = element_text(size = 14))


#Middle SDO
## Figure 14
ggplot(data =data_middle %>% filter(groups==1|groups==2),  aes(x=DV11, y= ..count../sum(..count..)))+
  geom_bar(position = "dodge")+
  ylim(0,0.75)+
  coord_flip()+
  labs(title="Middle SDO, Continuation")+
  scale_x_discrete(limits=c("Have an impact of decreasing status", "Do not have an impact", "Have an impact of increasing status"))+
  ylab("Percentage")+
  theme_classic()+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),4)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = 0, hjust = -0.1)+
  theme(axis.title.y = element_blank(), text = element_text(size = 13), 
        axis.text = element_text(size = 14))+
  
  
  ggplot(data =data_middle %>% filter(groups==3|groups==4), aes(x=DV11 ,y= ..count../sum(..count..)))+
  geom_bar(position = "dodge")+
  ylim(0,0.75)+
  coord_flip()+
  labs(title="Middle SDO, Withdraw")+
  scale_x_discrete(limits=c("Have an impact of decreasing status", "Do not have an impact", "Have an impact of increasing status"))+
  ylab("Percentage")+
  theme_classic()+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),4)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = 0, hjust=-0.1)+
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), text = element_text(size = 13), 
        axis.text = element_text(size = 14))

#Low SDO
## Figure 15
ggplot(data =data_low %>% filter(groups==1|groups==2),  aes(x=DV11, y= ..count../sum(..count..)))+
  geom_bar(position = "dodge")+
  ylim(0,0.75)+
  coord_flip()+
  labs(title="Low SDO, Continuation")+
  scale_x_discrete(limits=c("Have an impact of decreasing status", "Do not have an impact", "Have an impact of increasing status"))+
  ylab("Percentage")+
  theme_classic()+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),4)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = 0, hjust = -0.1)+
  theme(axis.title.y = element_blank(), text = element_text(size = 13), 
        axis.text = element_text(size = 14))+
  
  
  ggplot(data =data_low %>% filter(groups==3|groups==4), aes(x=DV11 ,y= ..count../sum(..count..)))+
  geom_bar(position = "dodge")+
  ylim(0,0.75)+
  coord_flip()+
  labs(title="Low SDO, Withdraw")+
  scale_x_discrete(limits=c("Have an impact of decreasing status", "Do not have an impact", "Have an impact of increasing status"))+
  ylab("Percentage")+
  theme_classic()+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),4)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = 0, hjust=-0.1)+
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), text = element_text(size = 13), 
        axis.text = element_text(size = 14))




# Balance Check (Figure 16)-----------------------------------------------------------


## transforming discrete variables into dummy variables
data <- data %>% mutate(gender_male = if_else(gender=="female", 1, 0),
                        gender_female= if_else(gender=="male", 1, 0),
                        
                         education_graduate_graduated = if_else(education=="Graduated from a graduate school", 1, 0),
                         education_graduate_current = if_else(education=="Studying at a graduate school", 1, 0),
                         education_four_graduated = if_else(education=="Graduated from a four-year college", 1, 0),
                         education_four_current = if_else(education=="Studying at a four-year college", 1, 0),
                         education_two = if_else(education=="Two-year college", 1, 0),
                         education_high = if_else(education=="High school", 1, 0),
                         education_elem_juni = if_else(education=="Elementary school or Junior high school", 1, 0),
                        
                        interest_a_lot = if_else(interest == "A lot of attention",1, 0),
                        interest_some = if_else(interest == "Some attention",1, 0),
                        interest_limited = if_else(interest == "Limited attention",1, 0),
                        interest_no = if_else(interest == "No attention" ,1, 0),
                        
                        leader_sup_str_app = if_else(leader_sup == "Strongly Approve", 1, 0),
                        leader_sup_som_app = if_else(leader_sup == "Somewhat Approve", 1, 0),
                        leader_sup_str_dis = if_else(leader_sup == "Somewhat Disapprove", 1, 0),
                        leader_sup_som_dis = if_else(leader_sup == "Strongly Disapprove", 1, 0),
                        
                        hawk_dove_hawk = if_else(hawk_dove == "Hawk", 1,0),
                        hawk_dove_dove = if_else(hawk_dove == "Dove", 1,0),
                        hawk_dove_donotknow = if_else(hawk_dove == "Do not know", 1,0)
                        )


# Defining functions for SMD
##standardized mean difference for discrete variables
 SMD_dis <- function(treat, cont){
  mean_t <- mean(treat)
  mean_c <- mean(cont)
  SMD <- (mean_t - mean_c)/sqrt((mean_t*(1-mean_c) + mean_c*(1-mean_t)) /2) 
  return(SMD)
 }
 
##standardized mean difference for continuous variables
SMD_con <- function(treat, cont){
   mean_t <- mean(treat)
   mean_c <- mean(cont)
   SMD <- (mean_t - mean_c)/sqrt((var(treat)+var(cont))/2) 
   return(SMD)
}

#female
female01 <- SMD_dis(data$gender_female[data$groups==1|data$groups==2], data$gender_female[data$groups == 5]) 
female02 <- SMD_dis(data$gender_female[data$groups==3|data$groups==4], data$gender_female[data$groups == 6])
female11 <-SMD_dis(data$gender_female[data$groups==1], data$gender_female[data$groups==2])
female12 <-SMD_dis(data$gender_female[data$groups==3], data$gender_female[data$groups==4])

#male
male01 <- SMD_dis(data$gender_male[data$groups==1|data$groups==2], data$gender_male[data$groups == 5]) 
male02 <- SMD_dis(data$gender_male[data$groups==3|data$groups==4], data$gender_male[data$groups == 6])
male11 <- SMD_dis(data$gender_male[data$groups==1], data$gender_male[data$groups==2])
male12 <- SMD_dis(data$gender_male[data$groups==3], data$gender_male[data$groups==4])

#age
age01 <- SMD_con(data$age[data$groups==1|data$groups==2], data$age[data$groups == 5]) 
age02 <- SMD_con(data$age[data$groups==3|data$groups==4], data$age[data$groups == 6])
age11 <- SMD_con(data$age[data$groups==1], data$age[data$groups==2])
age12 <- SMD_con(data$age[data$groups==3], data$age[data$groups==4])

#liberal
liberal01 <- SMD_con(data$liberal[data$groups==1|data$groups==2] %>% na.omit(), data$liberal[data$groups == 5] %>% na.omit()) 
liberal02 <- SMD_con(data$liberal[data$groups==3|data$groups==4] %>% na.omit(), data$liberal[data$groups == 6] %>% na.omit())
liberal11 <- SMD_con(data$liberal[data$groups==1] %>% na.omit(), data$liberal[data$groups==2] %>% na.omit())
liberal12 <- SMD_con(data$liberal[data$groups==3] %>% na.omit(), data$liberal[data$groups==4] %>% na.omit())

#education_graduate_graduated
education_graduate_graduated01 <- SMD_dis(data$education_graduate_graduated[data$groups==1|data$groups==2], data$education_graduate_graduated[data$groups == 5]) 
education_graduate_graduated02 <- SMD_dis(data$education_graduate_graduated[data$groups==3|data$groups==4], data$education_graduate_graduated[data$groups == 6])
education_graduate_graduated11 <- SMD_dis(data$education_graduate_graduated[data$groups==1], data$education_graduate_graduated[data$groups==2])
education_graduate_graduated12 <- SMD_dis(data$education_graduate_graduated[data$groups==3], data$education_graduate_graduated[data$groups==4])

#education_graduate_current
education_graduate_current01 <- SMD_dis(data$education_graduate_current[data$groups==1|data$groups==2], data$education_graduate_current[data$groups == 5]) 
education_graduate_current02 <- SMD_dis(data$education_graduate_current[data$groups==3|data$groups==4], data$education_graduate_current[data$groups == 6])
education_graduate_current11 <- SMD_dis(data$education_graduate_current[data$groups==1], data$education_graduate_current[data$groups==2])
education_graduate_current12 <- SMD_dis(data$education_graduate_current[data$groups==3], data$education_graduate_current[data$groups==4])

#education_four_graduated
education_four_graduated01 <- SMD_dis(data$education_four_graduated[data$groups==1|data$groups==2], data$education_four_graduated[data$groups==5]) 
education_four_graduated02 <- SMD_dis(data$education_four_graduated[data$groups==3|data$groups==4], data$education_four_graduated[data$groups==6])
education_four_graduated11 <- SMD_dis(data$education_four_graduated[data$groups==1], data$education_four_graduated[data$groups==2])
education_four_graduated12 <- SMD_dis(data$education_four_graduated[data$groups==3], data$education_four_graduated[data$groups==4])

#education_four_current
education_four_current01 <- SMD_dis(data$education_four_current[data$groups==1|data$groups==2], data$education_four_current[data$groups==5]) 
education_four_current02 <- SMD_dis(data$education_four_current[data$groups==3|data$groups==4], data$education_four_current[data$groups==6])
education_four_current11 <- SMD_dis(data$education_four_current[data$groups==1], data$education_four_current[data$groups==2])
education_four_current12 <- SMD_dis(data$education_four_current[data$groups==3], data$education_four_current[data$groups==4])

#education_two
education_two01 <- SMD_dis(data$education_two[data$groups==1|data$groups==2], data$education_two[data$groups==5]) 
education_two02 <- SMD_dis(data$education_two[data$groups==3|data$groups==4], data$education_two[data$groups==6])
education_two11 <- SMD_dis(data$education_two[data$groups==1], data$education_two[data$groups==2])
education_two12 <- SMD_dis(data$education_two[data$groups==3], data$education_two[data$groups==4])

#education_high
education_high01 <- SMD_dis(data$education_high[data$groups==1|data$groups==2], data$education_high[data$groups==5]) 
education_high02 <- SMD_dis(data$education_high[data$groups==3|data$groups==4], data$education_high[data$groups==6])
education_high11 <- SMD_dis(data$education_high[data$groups==1], data$education_high[data$groups==2])
education_high12 <- SMD_dis(data$education_high[data$groups==3], data$education_high[data$groups==4])

#education_juni
education_juni01 <- SMD_dis(data$education_elem_juni[data$groups==1|data$groups==2], data$education_elem_juni[data$groups==5]) 
education_juni02 <- SMD_dis(data$education_elem_juni[data$groups==3|data$groups==4], data$education_elem_juni[data$groups==6])
education_juni11 <- SMD_dis(data$education_elem_juni[data$groups==1], data$education_elem_juni[data$groups==2])
education_juni12 <- SMD_dis(data$education_elem_juni[data$groups==3], data$education_elem_juni[data$groups==4])

#SDO_mean
SDO01 <- SMD_con(data$SDO_mean[data$groups==1|data$groups==2], data$SDO_mean[data$groups==5]) 
SDO02 <- SMD_con(data$SDO_mean[data$groups==3|data$groups==4], data$SDO_mean[data$groups==6])
SDO11 <- SMD_con(data$SDO_mean[data$groups==1], data$SDO_mean[data$groups==2])
SDO12 <- SMD_con(data$SDO_mean[data$groups==3], data$SDO_mean[data$groups==4])

#interest_a_lot
interest_a_lot01 <- SMD_dis(data$interest_a_lot[data$groups==1|data$groups==2], data$interest_a_lot[data$groups==5]) 
interest_a_lot02 <- SMD_dis(data$interest_a_lot[data$groups==3|data$groups==4], data$interest_a_lot[data$groups==6])
interest_a_lot11 <- SMD_dis(data$interest_a_lot[data$groups==1], data$interest_a_lot[data$groups==2])
interest_a_lot12 <- SMD_dis(data$interest_a_lot[data$groups==3], data$interest_a_lot[data$groups==4])

#interest_some
interest_some01 <- SMD_dis(data$interest_some[data$groups==1|data$groups==2], data$interest_some[data$groups==5]) 
interest_some02 <- SMD_dis(data$interest_some[data$groups==3|data$groups==4], data$interest_some[data$groups==6])
interest_some11 <- SMD_dis(data$interest_some[data$groups==1], data$interest_some[data$groups==2])
interest_some12 <- SMD_dis(data$interest_some[data$groups==3], data$interest_some[data$groups==4])

#interest_limited
interest_limited01 <- SMD_dis(data$interest_limited[data$groups==1|data$groups==2], data$interest_limited[data$groups==5]) 
interest_limited02 <- SMD_dis(data$interest_limited[data$groups==3|data$groups==4], data$interest_limited[data$groups==6])
interest_limited11 <- SMD_dis(data$interest_limited[data$groups==1], data$interest_limited[data$groups==2])
interest_limited12 <- SMD_dis(data$interest_limited[data$groups==3], data$interest_limited[data$groups==4])


#interest_no
interest_no01 <- SMD_dis(data$interest_no[data$groups==1|data$groups==2], data$interest_no[data$groups==5]) 
interest_no02 <- SMD_dis(data$interest_no[data$groups==3|data$groups==4], data$interest_no[data$groups==6])
interest_no11 <- SMD_dis(data$interest_no[data$groups==1], data$interest_no[data$groups==2])
interest_no12 <- SMD_dis(data$interest_no[data$groups==3], data$interest_no[data$groups==4])

#hawk_dove_hawk
hawk_dove_hawk01 <- SMD_dis(data$hawk_dove_hawk[data$groups==1|data$groups==2], data$hawk_dove_hawk[data$groups==5]) 
hawk_dove_hawk02 <- SMD_dis(data$hawk_dove_hawk[data$groups==3|data$groups==4], data$hawk_dove_hawk[data$groups==6])
hawk_dove_hawk11 <- SMD_dis(data$hawk_dove_hawk[data$groups==1], data$hawk_dove_hawk[data$groups==2])
hawk_dove_hawk12 <- SMD_dis(data$hawk_dove_hawk[data$groups==3], data$hawk_dove_hawk[data$groups==4])

#hawk_dove_dove
hawk_dove_dove01 <- SMD_dis(data$hawk_dove_dove[data$groups==1|data$groups==2], data$hawk_dove_dove[data$groups==5]) 
hawk_dove_dove02 <- SMD_dis(data$hawk_dove_dove[data$groups==3|data$groups==4], data$hawk_dove_dove[data$groups==6])
hawk_dove_dove11 <- SMD_dis(data$hawk_dove_dove[data$groups==1], data$hawk_dove_dove[data$groups==2])
hawk_dove_dove12 <- SMD_dis(data$hawk_dove_dove[data$groups==3], data$hawk_dove_dove[data$groups==4])

#hawk_dove_do not know
hawk_dove_donotknow01 <- SMD_dis(data$hawk_dove_donotknow[data$groups==1|data$groups==2], data$hawk_dove_donotknow[data$groups==5]) 
hawk_dove_donotknow02 <- SMD_dis(data$hawk_dove_donotknow[data$groups==3|data$groups==4], data$hawk_dove_donotknow[data$groups==6])
hawk_dove_donotknow11 <- SMD_dis(data$hawk_dove_donotknow[data$groups==1], data$hawk_dove_donotknow[data$groups==2])
hawk_dove_donotknow12 <- SMD_dis(data$hawk_dove_donotknow[data$groups==3], data$hawk_dove_donotknow[data$groups==4])

#fav_China
fav_China01 <- SMD_con(data$fav_China[data$groups==1|data$groups==2], data$fav_China[data$groups==5]) 
fav_China02 <- SMD_con(data$fav_China[data$groups==3|data$groups==4], data$fav_China[data$groups==6])
fav_China11 <- SMD_con(data$fav_China[data$groups==1], data$fav_China[data$groups==2])
fav_China12 <- SMD_con(data$fav_China[data$groups==3], data$fav_China[data$groups==4])

#fav_France
fav_France01 <- SMD_con(data$fav_France[data$groups==1|data$groups==2], data$fav_France[data$groups==5]) 
fav_France02 <- SMD_con(data$fav_France[data$groups==3|data$groups==4], data$fav_France[data$groups==6])
fav_France11 <- SMD_con(data$fav_France[data$groups==1], data$fav_France[data$groups==2])
fav_France12 <- SMD_con(data$fav_France[data$groups==3], data$fav_France[data$groups==4])

#fav_Korea
fav_Korea01 <- SMD_con(data$fav_Korea[data$groups==1|data$groups==2], data$fav_Korea[data$groups==5]) 
fav_Korea02 <- SMD_con(data$fav_Korea[data$groups==3|data$groups==4], data$fav_Korea[data$groups==6])
fav_Korea11 <- SMD_con(data$fav_Korea[data$groups==1], data$fav_Korea[data$groups==2])
fav_Korea12 <- SMD_con(data$fav_Korea[data$groups==3], data$fav_Korea[data$groups==4])

#fav_Russia
fav_Russia01 <- SMD_con(data$fav_Russia[data$groups==1|data$groups==2], data$fav_Russia[data$groups==5]) 
fav_Russia02 <- SMD_con(data$fav_Russia[data$groups==3|data$groups==4], data$fav_Russia[data$groups==6])
fav_Russia11 <- SMD_con(data$fav_Russia[data$groups==1], data$fav_Russia[data$groups==2])
fav_Russia12 <- SMD_con(data$fav_Russia[data$groups==3], data$fav_Russia[data$groups==4])

#fav_UK
fav_UK01 <- SMD_con(data$fav_UK[data$groups==1|data$groups==2], data$fav_UK[data$groups==5]) 
fav_UK02 <- SMD_con(data$fav_UK[data$groups==3|data$groups==4], data$fav_UK[data$groups==6])
fav_UK11 <- SMD_con(data$fav_UK[data$groups==1], data$fav_UK[data$groups==2])
fav_UK12 <- SMD_con(data$fav_UK[data$groups==3], data$fav_UK[data$groups==4])

#fav_US
fav_US01 <- SMD_con(data$fav_US[data$groups==1|data$groups==2], data$fav_US[data$groups==5]) 
fav_US02 <- SMD_con(data$fav_US[data$groups==3|data$groups==4], data$fav_US[data$groups==6])
fav_US11 <- SMD_con(data$fav_US[data$groups==1], data$fav_US[data$groups==2])
fav_US12 <- SMD_con(data$fav_US[data$groups==3], data$fav_US[data$groups==4])

#fav_UN
fav_UN01 <- SMD_con(data$fav_UN[data$groups==1|data$groups==2], data$fav_UN[data$groups==5]) 
fav_UN02 <- SMD_con(data$fav_UN[data$groups==3|data$groups==4], data$fav_UN[data$groups==6])
fav_UN11 <- SMD_con(data$fav_UN[data$groups==1], data$fav_UN[data$groups==2])
fav_UN12 <- SMD_con(data$fav_UN[data$groups==3], data$fav_UN[data$groups==4])

#fav_WHO
fav_WHO01 <- SMD_con(data$fav_WHO[data$groups==1|data$groups==2], data$fav_WHO[data$groups==5]) 
fav_WHO02 <- SMD_con(data$fav_WHO[data$groups==3|data$groups==4], data$fav_WHO[data$groups==6])
fav_WHO11 <- SMD_con(data$fav_WHO[data$groups==1], data$fav_WHO[data$groups==2])
fav_WHO12 <- SMD_con(data$fav_WHO[data$groups==3], data$fav_WHO[data$groups==4])


##visualization

SMD <- c(female01, female02, female11, female12, male01, male02, male11, male12, age01, age02, age11, age12, liberal01, liberal02, liberal11, liberal12,
         education_graduate_graduated01, education_graduate_graduated02, education_graduate_graduated11, education_graduate_graduated12,
         education_graduate_current01, education_graduate_current02, education_graduate_current11, education_graduate_current12, 
         education_four_graduated01, education_four_graduated02, education_four_graduated11, education_four_graduated12,
         education_four_current01, education_four_current02, education_four_current11, education_four_current12,
         education_two01, education_two02, education_two11, education_two12, education_high01, education_high02, education_high11, education_high12,
         education_juni01, education_juni02, education_juni11, education_juni12, SDO01, SDO02, SDO11, SDO12,interest_a_lot01, interest_a_lot02, 
         interest_a_lot11, interest_a_lot12, interest_some01, interest_some02,interest_some11, interest_some12, interest_limited01, interest_limited02, interest_limited11, interest_limited12,
         interest_no01, interest_no02, interest_no11, interest_no12, hawk_dove_hawk01, hawk_dove_hawk02, hawk_dove_hawk11, hawk_dove_hawk12, 
         hawk_dove_dove01, hawk_dove_dove02, hawk_dove_dove11, hawk_dove_dove12, hawk_dove_donotknow01, hawk_dove_donotknow02, hawk_dove_donotknow11, hawk_dove_donotknow12,
         fav_China01, fav_China02, fav_China11, fav_China12, fav_France01,fav_France02, fav_France11, fav_France12, fav_Korea01, fav_Korea02, fav_Korea11, fav_Korea12,
         fav_Russia01, fav_Russia02, fav_Russia11, fav_Russia12, fav_UK01, fav_UK02, fav_UK11, fav_UK12, fav_US01, fav_US02, fav_US11, fav_US12,
         fav_UN01,fav_UN02, fav_UN11, fav_UN12,fav_WHO01, fav_WHO02, fav_WHO11, fav_WHO12)

covariates <- c("female", "female", "female","female", "male", "male", "male",  "male", "age", "age", "age", "age", "liberal", "liberal", "liberal", "liberal",
                "education (grad school graduated)", "education (grad school graduated)", "education (grad school graduated)", "education (grad school graduated)",
                "education (grad school current)", "education (grad school current)", "education (grad school current)", "education (grad school current)",
                "education (undergrad graduated)", "education (undergrad graduated)", "education (undergrad graduated)", "education (undergrad graduated)",
                "education (undergrad current)", "education (undergrad current)", "education (undergrad current)", "education (undergrad current)",
                "education (community college)", "education (community college)", "education (community college)", "education (community college)",
                "education (high school)",  "education (high school)", "education (high school)", "education (high school)",
                "education (junior high shcool)", "education (junior high shcool)", "education (junior high shcool)", "education (junior high shcool)",
                "SDO mean", "SDO mean", "SDO mean", "SDO mean",
                "interest in politics (a lot)",  "interest in politics (a lot)",  "interest in politics (a lot)", "interest in politics (a lot)",
                "interest in politics (some)", "interest in politics (some)", "interest in politics (some)", "interest in politics (some)",
                "interest in politics (limited)", "interest in politics (limited)", "interest in politics (limited)", "interest in politics (limited)",
                "interest in politics (no)",  "interest in politics (no)",  "interest in politics (no)", "interest in politics (no)", 
                "Hawk or Dove (Hawk)",  "Hawk or Dove (Hawk)",  "Hawk or Dove (Hawk)","Hawk or Dove (Hawk)",
                "Hawk or Dove (Dove)", "Hawk or Dove (Dove)", "Hawk or Dove (Dove)", "Hawk or Dove (Dove)", 
                "Hawk or Dove (Do not know)", "Hawk or Dove (Do not know)", "Hawk or Dove (Do not know)", "Hawk or Dove (Do not know)", 
                "Favorability (China)",  "Favorability (China)",  "Favorability (China)","Favorability (China)",
                "Favorability (France)", "Favorability (France)", "Favorability (France)","Favorability (France)",
                "Favorability (Russia)", "Favorability (Russia)", "Favorability (Russia)", "Favorability (Russia)", 
                "Favorability (Korea)", "Favorability (Korea)", "Favorability (Korea)", "Favorability (Korea)",
                "Favorability (UK)", "Favorability (UK)", "Favorability (UK)", "Favorability (UK)",
                "Favorability (US)", "Favorability (US)", "Favorability (US)", "Favorability (US)", 
                "Favorability (UN)", "Favorability (UN)", "Favorability (UN)","Favorability (UN)",
                "Favorability (WHO)", "Favorability (WHO)", "Favorability (WHO)","Favorability (WHO)")

group <-as.factor(rep(1:4, 27))


SMDs <- data_frame(SMD, covariates, group)

## Figure 16
ggplot(SMDs, aes(x= covariates, y=SMD, color = group))+
  geom_point(alpha=0.6)+
  scale_y_continuous(breaks=c(-0.5,-0.25,-0.1, 0, 0.1,0.25,0.5), limits = c(-0.5,0.5))+
  scale_x_discrete(limits=c("Favorability (China)",  "Favorability (France)", "Favorability (Russia)", "Favorability (Korea)", 
                            "Favorability (UK)", "Favorability (US)",
                            "Favorability (WHO)", "Favorability (UN)",
                            "Hawk or Dove (Do not know)", "Hawk or Dove (Dove)",
                            "Hawk or Dove (Hawk)", "interest in politics (no)", "interest in politics (limited)",
                            "interest in politics (some)", "interest in politics (a lot)", 
                            "education (junior high shcool)",   "education (high school)",
                            "education (community college)", "education (undergrad current)", 
                            "education (undergrad graduated)", "education (grad school current)",
                            "education (grad school graduated)","SDO mean", "liberal", "age", "male","female"), 　
                   labels=c("Favorability (China)",  "Favorability (France)", "Favorability (Russia)", "Favorability (Korea)", 
                            "Favorability (UK)", "Favorability (US)",
                            "Favorability (WHO)", "Favorability (UN)",
                            "Hawk or Dove (Do not know)", "Hawk or Dove (Dove)",
                            "Hawk or Dove (Hawk)", "interest in politics (no)", "interest in politics (limited)",
                            "interest in politics (some)", "interest in politics (a lot)", 
                            "education (junior high shcool)",   "education (high school)",
                            "education (community college)", "education (undergrad attending)", 
                            "education (undergrad graduated)", "education (grad school attending)",
                            "education (grad school graduated)","SDO mean", "liberal", "age", "male","female"))+
  scale_color_discrete(labels = c("Diff. bet. UN Treatment and Control", "Diff. bet. US Treatment and Control", "Diff. bet. UN Withdrawal and Continuation", "Diff. bet. US Withdrawal and Continuation" ))+
  labs(title=, y = "Standerdized Difference")+
  theme_bw()+
  geom_hline(yintercept=0.1, linetype="dotted")+
  geom_hline(yintercept=-0.1, linetype="dotted")+
  geom_hline(yintercept=0.25, linetype="dashed")+
  geom_hline(yintercept=-0.25, linetype="dashed")+
  theme(legend.position = c(0.8, 0.1), axis.text.y=element_text(size=12))+
  coord_flip()



# priming effect (Figure 17, 18, 19) ----------------------------------------------------------


data <- data %>% mutate(priming_cha = case_when(priming ==1 ~ "Included",
                                            priming ==0 ~ "Not Included"))

data$priming_cha <- as.factor(data$priming_cha)

##Figure 17
##graph of pooled two groups (treatment and control, pooled)
ggplot(data = data %>% filter(groups==1|groups==2|groups==3|groups==4),
       aes(x=DV1, y= ..count../sum(..count..), fill = factor(priming_cha, levels =c("Not Included","Included") )))+
  geom_bar(position = "dodge")+
  ylim(0,0.25)+
  coord_flip()+
  labs(title="Treatment")+
  scale_x_discrete(limits=c("Do not want to answer", "Do not know", "Do not support", "Support"))+
  ylab("Percentage")+
  theme_classic()+
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = 0, hjust = -0.25, position = position_dodge(0.9), size = 4.5)+
  theme(axis.title.y = element_blank(),text = element_text(size = 13), 
        axis.text = element_text(size = 15), legend.position = 'none') + 
  
  
  ggplot(data =data %>% filter(groups==5|groups==6), 
         aes(x=DV1, y= ..count../sum(..count..), fill = factor(priming_cha, levels =c("Not Included","Included"))))+
  geom_bar(position = "dodge")+
  ylim(0,0.25)+
  coord_flip()+
  labs(title="Control")+
  scale_x_discrete(limits=c("Do not want to answer", "Do not know", "Do not support", "Support"))+
  ylab("Percentage")+
  theme_classic()+
  scale_color_discrete(labels = c("Not Included", "Included"))+
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y= ((..count..)/sum(..count..))), stat="count",
             hjust=-0.25, position = position_dodge(0.9), size = 4.5)+
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), 
        text = element_text(size = 13), axis.text = element_text(size = 15), legend.position=c(0.8,0.1))+
  labs(fill = "Priming")+
  guides(fill = guide_legend(reverse = TRUE))



##Figure 18
ggplot(data =data %>% filter(groups==1|groups==3),  
       aes(x=DV2, y= ..count../sum(..count..), fill = factor(priming_cha, levels =c("Not Included","Included"))))+
  geom_bar(position = "dodge")+
  ylim(0,0.4)+
  coord_flip()+
  labs(title="UN Treatment")+
  scale_x_discrete(limits=c("Have an impact of decreasing status", "Do not have an impact", "Have an impact of increasing status"))+
  ylab("Percentage")+
  theme_classic()+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),4)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = 0, hjust = -0.1,  position = position_dodge(0.9))+
  theme(axis.title.y = element_blank(), legend.position = 'none',
         text = element_text(size = 13), axis.text = element_text(size = 13))+
  
  
  ggplot(data =data %>% filter(groups==2|groups==4), 
         aes(x=DV2 ,y= ..count../sum(..count..), fill = factor(priming_cha, levels =c("Not Included","Included"))))+
  geom_bar(position = "dodge")+
  ylim(0,0.4)+
  coord_flip()+
  labs(title="US Treatment")+
  scale_x_discrete(limits=c("Have an impact of decreasing status", "Do not have an impact", "Have an impact of increasing status"))+
  ylab("Percentage")+
  theme_classic()+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),4)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = 0, hjust=-0.1, position = position_dodge(0.9))+
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), legend.position = 'none',
         text = element_text(size = 13), axis.text = element_text(size = 13))+
  
  
  ggplot(data =data %>% filter(groups==5), 
         aes(x=DV2 ,y= ..count../sum(..count..), fill =  factor(priming_cha, levels =c("Not Included","Included")) ))+
  geom_bar(position = "dodge")+
  ylim(0,0.4)+
  coord_flip()+
  labs(title="UN Coontrol")+
  scale_x_discrete(limits=c("Have an impact of decreasing status", "Do not have an impact", "Have an impact of increasing status"))+
  ylab("Percentage")+
  theme_classic()+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),4)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = 0, hjust=-0.1, position = position_dodge(0.9))+
  theme(axis.title.y = element_blank(), legend.position = 'none',  
        text = element_text(size = 13), axis.text = element_text(size = 13))+
  
  ggplot(data =data %>% filter(groups==6), 
         aes(x=DV2 ,y= ..count../sum(..count..), fill = factor(priming_cha, levels =c("Not Included","Included") )))+
  geom_bar(position = "dodge")+
  ylim(0,0.4)+
  coord_flip()+
  labs(title="US Control")+
  scale_x_discrete(limits=c("Have an impact of decreasing status", "Do not have an impact", "Have an impact of increasing status"))+
  ylab("Percentage")+
  theme_classic()+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),3)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = 0, hjust=-0.1, position = position_dodge(0.9))+
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(),
        text = element_text(size = 13), axis.text = element_text(size = 13))+
  labs(fill = "Priming")+
  guides(fill = guide_legend(reverse = TRUE))



## Figure 19
## priming check: descriptive statistics of stay and withdraw
ggplot(data =data %>% filter(groups==1),
       aes(x=DV11, y= ..count../sum(..count..),  fill = factor(priming_cha, levels =c("Not Included","Included")) ))+
  geom_bar(position = "dodge")+
  ylim(0,0.45)+
  coord_flip()+
  labs(title="UN Continuation")+
  scale_x_discrete(limits=c("Have an impact of decreasing status", "Do not have an impact", "Have an impact of increasing status"))+
  ylab("Percentage")+
  theme_classic()+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),4)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = 0, hjust = -0.1, position = position_dodge(0.9))+
  theme(axis.title.y = element_blank(), text = element_text(size = 13), 
        axis.text = element_text(size = 13), legend.position = 'none')+
  
  
  ggplot(data =data %>% filter(groups==2),
         aes(x=DV11, y= ..count../sum(..count..), fill = factor(priming_cha, levels =c("Not Included","Included"))))+
  geom_bar(position = "dodge")+
  ylim(0,0.45)+
  coord_flip()+
  labs(title="US Continuation")+
  scale_x_discrete(limits=c("Have an impact of decreasing status", "Do not have an impact", "Have an impact of increasing status"))+
  ylab("Percentage")+
  theme_classic()+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),4)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = 0, hjust=-0.1, position = position_dodge(0.9))+
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), 
        text = element_text(size = 13), axis.text = element_text(size = 13), legend.position = 'none')+
  
  
  ggplot(data =data %>% filter(groups==3), 
         aes(x=DV11, y= ..count../sum(..count..), fill = factor(priming_cha, levels =c("Not Included","Included"))))+
  geom_bar(position = "dodge")+
  ylim(0,0.45)+
  coord_flip()+
  labs(title="UN Withdraw")+
  scale_x_discrete(limits=c("Have an impact of decreasing status", "Do not have an impact", "Have an impact of increasing status"))+
  ylab("Percentage")+
  theme_classic()+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),4)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = 0, hjust=-0.1, position = position_dodge(0.9))+
  theme(axis.title.y = element_blank(),  text = element_text(size = 13), 
        axis.text = element_text(size = 13), legend.position = 'none')+
  
  ggplot(data =data %>% filter(groups==4), 
         aes(x=DV11, y= ..count../sum(..count..), fill = factor(priming_cha, levels =c("Not Included","Included"))))+
  geom_bar(position = "dodge")+
  ylim(0,0.45)+
  coord_flip()+
  labs(title="US Withdraw")+
  scale_x_discrete(limits=c("Have an impact of decreasing status", "Do not have an impact", "Have an impact of increasing status"))+
  ylab("Percentage")+
  theme_classic()+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),3)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = 0, hjust=-0.1, position = position_dodge(0.9))+
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), 
        text = element_text(size = 13), axis.text = element_text(size = 13))+
  labs(fill = "Priming")+
  guides(fill = guide_legend(reverse = TRUE))



  