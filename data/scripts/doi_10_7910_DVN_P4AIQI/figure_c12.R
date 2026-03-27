# ----------------------------------------------------------------------------
# INFO  
# ----------------------------------------------------------------------------

# Article: The Electoral Politics of Immigration and Crime
# Author(s): Jeyhun Alizade
# Code to replicate Figure C.12

# ----------------------------------------------------------------------------
# SETUP
# ----------------------------------------------------------------------------

# clear environment
rm(list=ls())

# install and load necessary packages
# install.packages("ggplot2")
library(ggplot2)

# set working directory (change working directory to where the replication folder is saved on your computer)
setwd("/Users/jeyhunalizade/Dropbox/immigration crime project/ajps_replication/replication_files/")

# load data set
dat = read.csv("data/conjoint.csv", sep=",")


# ----------------------------------------------------------------------------
# DATA PREPARATION
# ----------------------------------------------------------------------------

# stand-in variable to sum over
dat$one = 1

# aggregagte sum of response categories by party voted for
cats_tab = aggregate(one ~ cult_predis + voted_last, data=dat, FUN=sum)

# aggregate sum of parties voted for
parties_tab = aggregate(one ~ voted_last, data=dat, FUN=sum)

# merge the two tables
tab = merge(cats_tab, parties_tab, by="voted_last")

# generate the percentage share of respondents answering with a given response category, by party voted for in last election
tab$share = (tab$one.x / tab$one.y)*100

# round percentage share
tab$share = round(tab$share)

# recode response category variable into character
tab$cult_predis[tab$cult_predis==1] = "Disagree\nstrongly"
tab$cult_predis[tab$cult_predis==2] = "Disagree"
tab$cult_predis[tab$cult_predis==3] = "Agree"
tab$cult_predis[tab$cult_predis==4] = "Strongly\nagree"

# recode into factor and relevel response category variable
tab$cult_predis = factor(tab$cult_predis, levels=c("Disagree\nstrongly", "Disagree", "Agree", "Strongly\nagree"))

# relabel and relevel party variable
tab$voted_last[tab$voted_last=="CDU"] = "CDU/CSU"
tab$voted_last[tab$voted_last=="Gruene"] = "Greens"
tab$voted_last = factor(tab$voted_last, levels=c("Linke", "Greens", "SPD", "FDP", "CDU/CSU", "AfD"))


# ----------------------------------------------------------------------------
# PLOTTING
# ----------------------------------------------------------------------------

# create Figure C.12

ggplot(data=tab, aes(x=cult_predis, y=share)) + geom_bar(stat="identity") + facet_wrap(~voted_last) +
  theme_bw() +
  scale_y_continuous(breaks=seq(0, 100, 20), limits = c(0,100), ) +
  theme(text=element_text(size=14), axis.text.x=element_text(angle = -45, hjust = 0)) +
  ylab("Percent") + xlab("Response category") +
  geom_text(aes(label = round(share)), vjust = -0.5)

# save figure C.12

ggsave("figures/figure_c12.pdf", width = 7, height = 5)


# ----------------------------------------------------------------------------
# NUMBERS REPORTED ON PAGE 25
# ----------------------------------------------------------------------------

# percent of respondents who agree or strongly agree with the statement that immigrants from certain cultures are predisposed toward crime
print(sum(dat$cult_predis>=3)/nrow(dat)*100)

# percent of Green voters who agree or strongly agree with the statement
print(sum(dat$cult_predis[dat$voted_last=="Gruene" & !is.na(dat$voted_last)]>=3)/nrow(dat[dat$voted_last=="Gruene" & !is.na(dat$voted_last),])*100)

# percent of SPD voters who gree or strongly agree with the statement
print(sum(dat$cult_predis[dat$voted_last=="SPD" & !is.na(dat$voted_last)]>=3)/nrow(dat[dat$voted_last=="SPD" & !is.na(dat$voted_last),])*100)

# percent of Die Linke voters who gree or strongly agree with the statement
sum(dat$cult_predis[dat$voted_last=="Linke" & !is.na(dat$voted_last)]>=3)/nrow(dat[dat$voted_last=="Linke" & !is.na(dat$voted_last),])*100

