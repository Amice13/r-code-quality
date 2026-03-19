##########################################################################
# Figure A2: Republicanos and the Organizational Structure of the UCKG
###########################################################################

rm(list = ls())
options(scipen=999)
library(here)
library(dplyr)
library(ggplot2)

load(here('data','prb_iurd.Rda'))

data$branch_presence <- as.factor(data$UCKG2011)
levels(data$branch_presence) <- c("No UCKG", "UCKG")

pd <- position_dodge(0.1)


# Party Offices -----------------------------------------------------------

table(data$prb_partyoffice2012)

sum <- data %>% group_by(branch_presence) %>% summarize(
  mean = mean(prb_partyoffice2012, na.rm = T),
  se = sd(prb_partyoffice2012, na.rm = T)/sqrt(n()),
  lower = mean - 1.96* se,
  upper = mean + 1.96* se
  
)

ggplot(sum, aes(x=branch_presence, y=mean)) +
  facet_wrap(~ "Party Offices") + 
  geom_bar(position=pd, stat="identity", width=0.45, alpha=0.9) +
  geom_errorbar(aes(ymin = lower, ymax = upper, width=.1)) + 
  ggtitle(" ") +
  scale_fill_brewer(palette = "Set1") + 
  xlab("") +
  ylab("% of Municipalities with\na Republicanos Party Office") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.y=element_text(colour = "gray30", size=11), axis.ticks.y=element_blank()) +
  theme(axis.text.x=element_text(colour = "gray30", size=11), axis.ticks.x=element_line(colour="gray30"))


# Party Affiliation -------------------------------------------------------

data$fil_rep2012_sh <- data$fil_rep2012_sh*100

sum <- data %>% group_by(branch_presence) %>% summarize(
  mean = mean(fil_rep2012_sh, na.rm = T),
  se = sd(fil_rep2012_sh, na.rm = T)/sqrt(n()),
  lower = mean - 1.96* se,
  upper = mean + 1.96* se
  
)

ggplot(sum, aes(x=branch_presence, y=mean)) +
  facet_wrap(~ "Party Affiliation") + 
  geom_bar(position=pd, stat="identity", width=0.45, alpha=0.9) +
  geom_errorbar(aes(ymin = lower, ymax = upper, width=.1)) + 
  ggtitle(" ") +
  scale_fill_brewer(palette = "Set1") + 
  xlab("") +
  ylab("% Over Affiliates to All Parties") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.y=element_text(colour = "gray30", size=11), axis.ticks.y=element_blank()) +
  theme(axis.text.x=element_text(colour = "gray30", size=11), axis.ticks.x=element_line(colour="gray30"))



# Mayoral Candidates ------------------------------------------------------

sum <- data %>% group_by(branch_presence) %>% summarize(
  mean = mean(is_running_mayor_prb_2012, na.rm = T),
  se = sd(is_running_mayor_prb_2012, na.rm = T)/sqrt(n()),
  lower = mean - 1.96* se,
  upper = mean + 1.96* se
  
)

ggplot(sum, aes(x=branch_presence, y=mean)) +
  facet_wrap(~ "Mayoral Candidates") + 
  geom_bar(position=pd, stat="identity", width=0.45, alpha=0.9) +
  geom_errorbar(aes(ymin = lower, ymax = upper, width=.1)) + 
  ggtitle(" ") +
  scale_fill_brewer(palette = "Set1") + 
  xlab("") +
  ylab("% of Municipalities with\na Republicanos Mayoral Candidate") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.y=element_text(colour = "gray30", size=11), axis.ticks.y=element_blank()) +
  theme(axis.text.x=element_text(colour = "gray30", size=11), axis.ticks.x=element_line(colour="gray30"))



# Campaign Spending  ------------------------------------------------------

sum <- data %>% group_by(branch_presence) %>% summarize(
  mean = mean(spendpervoter, na.rm = T),
  se = sd(spendpervoter, na.rm = T)/sqrt(n()),
  lower = mean - 1.96* se,
  upper = mean + 1.96* se
  
)

ggplot(sum, aes(x=branch_presence, y=mean)) +
  facet_wrap(~ "Campaign Spending") + 
  geom_bar(position=pd, stat="identity", width=0.45, alpha=0.9) +
  geom_errorbar(aes(ymin = lower, ymax = upper, width=.1)) + 
  ggtitle(" ") +
  scale_fill_brewer(palette = "Set1") + 
  xlab("") +
  ylab("Campaign Spending\n(in Reais per Voter)") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.y=element_text(colour = "gray30", size=11), axis.ticks.y=element_blank()) +
  theme(axis.text.x=element_text(colour = "gray30", size=11), axis.ticks.x=element_line(colour="gray30"))



