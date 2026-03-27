## Paper replication code ##

library(foreign)
library(ggplot2)
library(doBy)
library(coefplot)
library(tidyverse)
library(tidyr)
library(dplyr)
library(optimx)
library(DescTools)
library(sjPlot)
library(lme4)
library(lmtest)
library(sandwich)
library(xtable)
library(reshape2)
library(gtools)
library(stargazer)
require(gridExtra)
library(stringr)
library(patchwork)
library(effects)
library(ggeffects)


load("Nigeria_COVIDmisinfo.RData")



### Figure 1 ###

b.barplot <- ng.long %>% 
  group_by(veracity, neutral) %>%
  summarize(
    mean = mean(belief),
    se = sd(belief) / sqrt(n()),
    ## new CI - using BinomCI
    lower = BinomCI(x = sum(belief==1,na.rm=T), n = length(belief))[,2],
    upper = BinomCI(x = sum(belief==1,na.rm=T), n = length(belief))[,3]
  ) %>% 
  ggplot(aes(veracity, mean, fill=factor(neutral), label = scales::percent(mean))) + 
  scale_fill_manual(values=c("#E69F00", "#56B4E9"), labels =c("Any emotion","Neutral"), name = "") +
  geom_bar(stat="identity",position=position_dodge(.7),width=.7)+
  geom_text(position = position_dodge(width = .7),    # move to center of bars
            vjust = -4,    # nudge above top of bar
            size = 3) +
  geom_errorbar(aes(ymin=lower, ymax=upper, 
                    group=factor(neutral),
                    width=.2),
                position=position_dodge(.7))+
  scale_y_continuous(labels=scales::percent,limits = c(0,1)) +
  ylab("% Believe headline")+
  xlab("Headline Veracity")+
  ggtitle("Belief")+
  theme(legend.title = element_blank())+
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        panel.border = element_rect(colour = "gray50", fill=NA, size=.11))

c.barplot <- ng.long %>% 
  group_by(veracity, neutral) %>%
  summarize(mean = mean(click, na.rm=T),
            se = sd(click, na.rm=T) / sqrt(n()),
            #lower = mean - se * qnorm(.975),
            #upper = mean + se * qnorm(.975)) %>%
            ## new CI - using BinomCI
            lower = BinomCI(x = sum(click==1,na.rm=T), n = length(click))[,2],
            upper = BinomCI(x = sum(click==1,na.rm=T), n = length(click))[,3]
  ) %>% 
  ggplot(aes(veracity, mean, fill=factor(neutral), label = scales::percent(mean))) + 
  scale_fill_manual(values=c("#E69F00", "#56B4E9"), labels =c("Any emotion","Neutral"), name = "") +
  geom_bar(stat="identity",position=position_dodge(.7),width=.7)+
  geom_text(position = position_dodge(width = .7),    # move to center of bars
            vjust = -4,      # nudge above top of bar
            size = 3) +
  scale_y_continuous(labels=scales::percent,limits = c(0,1)) +
  geom_errorbar(aes(ymin=lower, ymax=upper, 
                    group=factor(neutral),
                    width=.2),
                position=position_dodge(.7))+
  ylab("% Click headline")+
  xlab("Headline Veracity")+
  ggtitle("Click")+
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        panel.border = element_rect(colour = "gray50", fill=NA, size=.11))

s.barplot <- ng.long %>% 
  group_by(veracity, neutral) %>%
  summarize(mean = mean(share, na.rm=T),
            se = sd(share, na.rm=T) / sqrt(n()),
            #lower = mean - se * qnorm(.975),
            #upper = mean + se * qnorm(.975)) %>%
            ## new CI - using BinomCI
            lower = BinomCI(x = sum(share==1,na.rm=T), n = length(share))[,2],
            upper = BinomCI(x = sum(share==1,na.rm=T), n = length(share))[,3]
  ) %>% 
  ggplot(aes(veracity, mean, fill=factor(neutral), label = scales::percent(mean))) + 
  scale_fill_manual(values=c("#E69F00", "#56B4E9"), labels =c("Any emotion","Neutral"), name = "") +
  scale_y_continuous(labels=scales::percent,limits = c(0,1)) +
  geom_col(position=position_dodge(.7), width=.7)+
  geom_errorbar(aes(ymin=lower, ymax=upper, 
                    group=factor(neutral),
                    width=.2),
                position=position_dodge(.7))+
  geom_text(position = position_dodge(width = .7),    # move to center of bars
            vjust = -4,      # nudge above top of bar
            size = 3) +
  ylab("% Share headline")+
  xlab("Headline Veracity")+
  ggtitle("Share")+
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        panel.border = element_rect(colour = "gray50", fill=NA, size=.11))

grid.arrange(b.barplot , c.barplot , s.barplot, ncol=3)




### Figure 2 ###

## belief ~ happiness x veracity
bhappy <- ng.long %>% 
  group_by(veracity, happy) %>%
  summarize(
    mean = mean(belief),
    se = sd(belief) / sqrt(n()),
    ## new CI - using BinomCI
    lower = BinomCI(x = sum(belief==1,na.rm=T), n = length(belief))[,2],
    upper = BinomCI(x = sum(belief==1,na.rm=T), n = length(belief))[,3]
  ) %>% 
  ggplot(aes(veracity, mean, fill=factor(happy), label = scales::percent(mean))) + 
  scale_fill_manual(values=c("#E69F00", "#56B4E9"), labels =c("Not happy","Happy"), name = "") +
  geom_bar(stat="identity",position=position_dodge(.9))+
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -1.5,    # nudge above top of bar
            size = 3) +
  geom_errorbar(aes(ymin=lower, ymax=upper, 
                    group=factor(happy),
                    width=.2),
                position=position_dodge(.9))+
  scale_y_continuous(labels=scales::percent,limits = c(0,1)) +
  ylab("% Believe headline")+
  xlab("Headline Veracity")+
  ggtitle("Happiness")+
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        panel.border = element_rect(colour = "gray50", fill=NA, size=.11))


## belief ~ surprise x veracity
bsurprise <- ng.long %>% 
  group_by(veracity, surprise) %>%
  summarize(
    mean = mean(belief),
    se = sd(belief) / sqrt(n()),
    ## new CI - using BinomCI
    lower = BinomCI(x = sum(belief==1,na.rm=T), n = length(belief))[,2],
    upper = BinomCI(x = sum(belief==1,na.rm=T), n = length(belief))[,3]
  ) %>% 
  ggplot(aes(veracity, mean, fill=factor(surprise), label = scales::percent(mean))) + 
  scale_fill_manual(values=c("#E69F00", "#56B4E9"), labels =c("Not surprised","Surprised"), name = "") +
  geom_bar(stat="identity",position=position_dodge(.9))+
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -1.5,    # nudge above top of bar
            size = 3) +
  geom_errorbar(aes(ymin=lower, ymax=upper, 
                    group=factor(surprise),
                    width=.2),
                position=position_dodge(.9))+
  scale_y_continuous(labels=scales::percent,limits = c(0,1)) +
  ylab("% Believe headline")+
  xlab("Headline Veracity")+
  ggtitle("Surprise")+
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        panel.border = element_rect(colour = "gray50", fill=NA, size=.11))


grid.arrange(bhappy, bsurprise, ncol=2)


### Figure 3 ###

# function to standardize coefficients
std <- function(x){
  (x-mean(x, na.rm=T))/sd(x, na.rm=T)
}

vs <- c("crt_index","age","gender","edu","urban","APC_party","voted","religiosity","someuse","discern")

ng.uni.sd <- as.data.frame(apply(ng.uni[,c(vs)], 2, std))

crt2.std <- lm(discern ~ crt_index  +  age + gender + edu + urban + APC_party + voted + religiosity + someuse, data=ng.uni.sd)

names(crt2.std$coefficients)[c(2:10)] <- c("CRT","age","male","education","urban","APC","voted","religiosity","social media use")

cplot <- coefplot(crt2.std, title = "Correlates of discernment", pointSize = 3, innerCI = 1, outerCI = 2, lwdInner = 1, lwdOuter = .5, xlab = "Effect Size (standard deviations)", ylab = "Variable", cex = 1.8, intercept=FALSE, sort = "magnitude") +
  theme(text = element_text(size=18)) 

cplot
