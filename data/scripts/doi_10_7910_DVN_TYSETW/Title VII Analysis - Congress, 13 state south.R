rm(list = ls())

library(car)
library(sfsmisc)
library(plm)
library(foreign)
library(mrpdata)
library(mrp)
library(arm)
library(ggplot2)
library(gcookbook)
library(reshape)
library(reshape2)
library(maps)
library(mapproj)
library(plyr)
library(gridBase)
library(doBy)
library(base)
library(xlsx)
library(Hmisc)

##NOTE: This is a robustness check, redefining the south as the 13 states identified by the CQ Almanac in 1964 as being part of the "conservative coalition"

###################
#LOAD AND EDIT DATA
###################

#Borrow MRP dataset for regional info
load("mrp.regions.Rda")  
mrp.regions$region[mrp.regions$state=="DE"] <- "Northeast"  
mrp.regions$region[mrp.regions$state=="MD"] <- "Northeast"  
mrp.regions$region[mrp.regions$state=="WV"] <- "Midwest"  

#Load House ERA sponsorship dataset, add region
data_erahouse <- read.csv("ERA Introductions, 1923-1964 House.csv")
data_erahouse <- rename(data_erahouse, c(SponsorState="state"))
data_erahouse <- join(data_erahouse, mrp.regions, by="state")

#Load Senate ERA sponsorship dataset, add region
data_erasen <- read.csv("ERA Introductions, 1923-1964 SENATE.csv")
data_erasen <- rename(data_erasen, c(SponsorState="state"))
data_erasen <- join(data_erasen, mrp.regions, by="state")

########################
#ERA SPONSORSHIP GRAPHS
########################

#Sponsorship in House by Year, Party, and Region - data
sponsors_pry_house <- melt(xtabs(~region+SponsorParty+year, data=data_erahouse))
sponsors_pry_house <- subset(sponsors_pry_house, !(region=="DC"))
sponsors_pry_house <- subset(sponsors_pry_house, !(SponsorParty=="ID"))
sponsors_pry_house <- droplevels(sponsors_pry_house)
for(i in c(names(sponsors_pry_house))){
  sponsors_pry_house[,i] <- recode(sponsors_pry_house[,i],"'' = NA")
}
sponsors_pry_house <-na.omit(sponsors_pry_house)

#Sponsorship in Senate by year, party, and region - data
sponsors_pry_sen <- melt(xtabs(~region+SponsorParty+year, data=data_erasen))
sponsors_pry_sen <- subset(sponsors_pry_sen, !(region=="DC"))
sponsors_pry_sen <- subset(sponsors_pry_sen, !(SponsorParty=="ID"))
sponsors_pry_sen <- droplevels(sponsors_pry_sen)
for(i in c(names(sponsors_pry_sen))){
  sponsors_pry_sen[,i] <- recode(sponsors_pry_sen[,i],"'' = NA")
}
sponsors_pry_sen <-na.omit(sponsors_pry_sen)

#Combined graph for House and Senate by year

sponsors_pry_house$chamber <- "House"
sponsors_pry_sen$chamber <- "Senate"
sponsors_pry_combined <- rbind(sponsors_pry_house, sponsors_pry_sen)

#Summary graph: sponsorship in Congress by region and party, 1923-1964

sponsors_pr <- melt(xtabs(~region+SponsorParty, data=data_erahouse))
for(i in c(names(sponsors_pr))){
  sponsors_pr[,i] <- recode(sponsors_pr[,i],"'' = NA")
}
sponsors_pr <- na.omit(sponsors_pr)
sponsors_pr <- subset(sponsors_pr, !(region=="DC"))
sponsors_pr <- subset(sponsors_pr, !(SponsorParty=="ID"))

sponsors_pr_sen <- melt(xtabs(~region+SponsorParty, data=data_erasen))
for(i in c(names(sponsors_pr_sen))){
  sponsors_pr_sen[,i] <- recode(sponsors_pr_sen[,i],"'' = NA")
}
sponsors_pr_sen <- na.omit(sponsors_pr_sen)
sponsors_pr_sen <- subset(sponsors_pr_sen, !(region=="DC"))
sponsors_pr_sen <- subset(sponsors_pr_sen, !(SponsorParty=="ID"))

sponsors_pr_sen$chamber <- "Senate"
sponsors_pr$chamber <- "House"
sponsors_pr_combined <- rbind(sponsors_pr, sponsors_pr_sen)

pdf("era_cpr_bw_13.pdf")
ggplot(sponsors_pr_combined, aes(x=region, y=value, fill=SponsorParty))+
  geom_bar(position="dodge", stat="identity") + 
  ggtitle("Total Sponsorships (13 State South)\n") + 
  #ggtitle("ERA Sponsorships in Congress by Party and Region, 1923-1964 (13 State South)\n") + 
  labs(x = "", y = "# of sponsorships", fill="Party") + 
  scale_fill_manual(values=c(R="grey65", D="grey25")
  )  +
  facet_grid(chamber~.) + 
  guides(fill=FALSE)+
  theme_bw()+
  theme(plot.title=element_text(size=14, hjust = 0.5), axis.text=element_text(size=12), axis.title=element_text(size=12), strip.text=element_text(size=12))

dev.off()


#################################################
#TOTAL DELEGATION SIZE BY PARTY, REGION, AND YEAR
#################################################

#How many R and D were there in total in each year in the House?

Hdata <- read.dta("HL01113D21_BSSE_12.DTA")
Hdata$chamber <- "House"
Hdata <- subset(Hdata, select=c("cong", "idno", "state", "party", "name", "chamber"))
Hdata=Hdata[which(Hdata$cong>67 & Hdata$cong<89),]

Sdata <- read.dta("SL01113D21_BSSE_12.DTA")
Sdata$chamber <- "Senate"
Sdata <- subset(Sdata, select=c("cong", "idno", "state", "party", "name", "chamber"))
Sdata=Sdata[which(Sdata$cong>67 & Sdata$cong<89),]

MCdata <- rbind(Sdata, Hdata)
converter <- read.dta("state_converter.dta")
converter <- rename(converter, c(icpsr="state"))

MCdata <-join(MCdata, converter, by="state")
MCdata <- rename(MCdata, c(state="icpsr"))
MCdata <- rename(MCdata, c(abbr="state"))
MCdata <- join(MCdata, mrp.regions, by="state")

MC_totals <- melt(xtabs(~region+cong+party+chamber, data=MCdata))
MC_totals=MC_totals[which(MC_totals$region!="DC"),]
MC_totals=MC_totals[which(MC_totals$party==100 | MC_totals$party==200),]

MC_totals$year[MC_totals$cong==68] <- 1924
MC_totals$year[MC_totals$cong==69] <- 1926
MC_totals$year[MC_totals$cong==70] <- 1928
MC_totals$year[MC_totals$cong==71] <- 1930
MC_totals$year[MC_totals$cong==72] <- 1932
MC_totals$year[MC_totals$cong==73] <- 1934
MC_totals$year[MC_totals$cong==74] <- 1936
MC_totals$year[MC_totals$cong==75] <- 1938
MC_totals$year[MC_totals$cong==76] <- 1940
MC_totals$year[MC_totals$cong==77] <- 1942
MC_totals$year[MC_totals$cong==78] <- 1944
MC_totals$year[MC_totals$cong==79] <- 1946
MC_totals$year[MC_totals$cong==80] <- 1948
MC_totals$year[MC_totals$cong==81] <- 1950
MC_totals$year[MC_totals$cong==82] <- 1952
MC_totals$year[MC_totals$cong==83] <- 1954
MC_totals$year[MC_totals$cong==84] <- 1956
MC_totals$year[MC_totals$cong==85] <- 1958
MC_totals$year[MC_totals$cong==86] <- 1960
MC_totals$year[MC_totals$cong==87] <- 1962
MC_totals$year[MC_totals$cong==88] <- 1964

temp <- MC_totals
temp$year[temp$cong==68] <- 1923
temp$year[temp$cong==69] <- 1925
temp$year[temp$cong==70] <- 1927
temp$year[temp$cong==71] <- 1929
temp$year[temp$cong==72] <- 1931
temp$year[temp$cong==73] <- 1933
temp$year[temp$cong==74] <- 1935
temp$year[temp$cong==75] <- 1937
temp$year[temp$cong==76] <- 1939
temp$year[temp$cong==77] <- 1941
temp$year[temp$cong==78] <- 1943
temp$year[temp$cong==79] <- 1945
temp$year[temp$cong==80] <- 1947
temp$year[temp$cong==81] <- 1949
temp$year[temp$cong==82] <- 1951
temp$year[temp$cong==83] <- 1953
temp$year[temp$cong==84] <- 1955
temp$year[temp$cong==85] <- 1957
temp$year[temp$cong==86] <- 1959
temp$year[temp$cong==87] <- 1961
temp$year[temp$cong==88] <- 1963

MC_totals <- rbind(temp, MC_totals)
MC_totals <- rename(MC_totals, c(value="no_MCs"))
MC_totals$party[MC_totals$party==100] <- "D"
MC_totals$party[MC_totals$party==200] <- "R"
MC_totals$party <- as.factor(MC_totals$party)
MC_totals <- subset(MC_totals, !(region=="DC"))
MC_totals <- droplevels(MC_totals)

#######################################################################
#GRAPH PERCENTAGE OF EACH PARTY-REGION DELEGATION THAT SPONSORED AN ERA
#######################################################################

full <- as.data.frame(rep(1923:1964, each=16))
colnames(full) <- c("year")
full$party <- rep(c("D", "R"), each=8)
full$region <- rep(c("Midwest", "Northeast", "South", "West"))
full$chamber <- rep(c("House", "Senate"), each=4)
full$chamber <- as.factor(full$chamber)
full$region <- as.factor(full$region)
full$party <- as.factor(full$party)

full_h <- subset(full[which(full$chamber=="House"), ])
full_s <- subset(full[which(full$chamber=="Senate"), ])
MC_totals_h <- subset(MC_totals[which(MC_totals$chamber=="House"), ])
MC_totals_s <- subset(MC_totals[which(MC_totals$chamber=="Senate"), ])

#Average delegation size over the whole period by party and region

avg_del <- aggregate(no_MCs~region+party+chamber, MC_totals, mean)
avg_del <- rename(avg_del, c(no_MCs="avg_delegation"))

temp <- sponsors_pr_combined
temp <- rename(temp, c(SponsorParty="party"))

pct_graph_agg <- merge(temp, avg_del, by=c("party", "region", "chamber"))
pct_graph_agg <- rename(pct_graph_agg, c(value="tot_sponsorships"))

#Total number of seats over the whole period by party and region

tot_del <- aggregate(no_MCs~region+party+chamber, MC_totals, FUN=sum)
tot_del <- rename(tot_del, c(no_MCs="tot_delegation"))
pct_graph_agg <- merge(pct_graph_agg, tot_del, by=c("party", "region", "chamber"))
pct_graph_agg$perseat <- (pct_graph_agg$tot_sponsorships/pct_graph_agg$tot_delegation)

pdf("era_perseat_13.pdf")
ggplot(pct_graph_agg, aes(x=region, y=perseat, fill=party))+
  geom_bar(position="dodge", stat="identity") + 
  ggtitle("Sponsorships Per Capita (13 State South)\n") + 
  labs(x = "", y = "# of sponsorships per seat", fill="Party") + 
  scale_fill_manual(values=c(R="grey65", D="grey25")
  )  +
  facet_grid(chamber~.) + 
  theme_bw()+
  theme(plot.title=element_text(size=14, hjust = 0.5), axis.text=element_text(size=12), axis.title=element_text(size=12), strip.text=element_text(size=12))

dev.off()


####################
#DISCHARGE PETITIONS
####################

dp <- read.csv("ERA discharge petition 81st congress.csv", 1)

converter <- read.dta("state_converter.dta")
converter <- rename(converter, c(icpsr="state"))

dp <-join(dp, converter, by="state")
dp <- rename(dp, c(state="icpsr"))
dp <- rename(dp, c(abbr="state"))
dp <- join(dp, mrp.regions, by="state")
dp=dp[which(dp$dp26!="NA"),]

dp_graph <- melt(xtabs(~region+party, data=dp))
dp_graph=dp_graph[which(dp_graph$region!="DC"),]
dp_graph=dp_graph[which(dp_graph$party==100 | dp_graph$party==200),]
dp_graph$partyname[dp_graph$party==100] <- "D"
dp_graph$partyname[dp_graph$party==200] <- "R"

pdf("discharge_bw_13.pdf", width=7, height=4)
ggplot(dp_graph, aes(x=region, y=value, fill=partyname)) +
  geom_bar(position="dodge", stat="identity") + 
  ggtitle("Discharge Petition Signatures (13 State South)\n") + 
  labs(x = "", y = "# of signatures\n", fill="Party") + 
  scale_fill_manual(values=c(R="grey65", D="grey25")) +
  theme_bw()+
  theme(plot.title=element_text(size=14, hjust = 0.5), axis.text=element_text(size=12), axis.title=element_text(size=12), strip.text=element_text(size=12))
dev.off()

#Graph signatories as a percentage of party-region delegation

Hdata <- read.dta("HL01113D21_BSSE_12.DTA")
Hdata_discharge=Hdata[which(Hdata$cong==81),]

converter <- read.dta("state_converter.dta")
converter <- rename(converter, c(icpsr="state"))

Hdata_discharge <-join(Hdata_discharge, converter, by="state")
Hdata_discharge <- rename(Hdata_discharge, c(state="icpsr"))
Hdata_discharge <- rename(Hdata_discharge, c(abbr="state"))
Hdata_discharge <- join(Hdata_discharge, mrp.regions, by="state")

Discharge_totals <- melt(xtabs(~region+party, data=Hdata_discharge))
Discharge_totals=Discharge_totals[which(Discharge_totals$region!="DC"),]
Discharge_totals=Discharge_totals[which(Discharge_totals$party==100 | Discharge_totals$party==200),]

dp_graph_pct <- merge(Discharge_totals, dp_graph, by=c("region", "party"))
dp_graph_pct$pct <- (dp_graph_pct$value.y/dp_graph_pct$value.x)*100

pdf("discharge_pct_bw_13.pdf", width=7, height=4)
ggplot(dp_graph_pct, aes(x=region, y=pct, fill=partyname)) +
  geom_bar(position="dodge", stat="identity") + 
  ggtitle("Percent of Delegation Signing Discharge Petition (13 State South)\n") + 
  labs(x = "", y = "% of delegation signing petition \n", fill="Party") + 
  scale_fill_manual(values=c(R="grey65", D="grey25")) +
  theme_bw()+
  theme(plot.title=element_text(size=14, hjust = 0.5), axis.text=element_text(size=12), axis.title=element_text(size=12), strip.text=element_text(size=12))

dev.off()



###################
#ROLL CALL VOTES
###################

rollcalls_sen <- read.dta("sen_votes_use.dta")
rollcalls_sen <- merge(rollcalls_sen, mrp.regions, by="state")
rollcalls_sen <- rename(rollcalls_sen, c(partyregion="partyregion_17"))
rollcalls_sen$partyregion[rollcalls_sen$party==200] <- "Republican" 
rollcalls_sen$partyregion[rollcalls_sen$party==100 & rollcalls_sen$region=="South"] <- "Southern Democrat" 
rollcalls_sen$partyregion[rollcalls_sen$party==100 & rollcalls_sen$region!="South"] <- "Non-southern Democrat" 

#S.J.Res. 61
sjres61 <-subset(rollcalls_sen, select=c(partyregion, votesen_sjres61_f))
sjres61 <-na.omit(sjres61)
sjres61_ps <- melt(xtabs(~partyregion+votesen_sjres61_f, data=sjres61))

for(i in c(names(sjres61_ps))){
  sjres61_ps[,i] <- recode(sjres61_ps[,i],"'.' = NA")
}

sjres61_ps <-na.omit(sjres61_ps)

sjres61_ps <- dcast(sjres61_ps, partyregion ~ votesen_sjres61_f, value.var="value")
sjres61_ps <- rename(sjres61_ps, c("0"="nay"))
sjres61_ps <- rename(sjres61_ps, c("1"="yay"))
sjres61_ps$pctyay <- sjres61_ps$yay/(sjres61_ps$yay + sjres61_ps$nay)
sjres61_ps$bill <- "SJRes61 (1946)"
sjres61_ps$year <- 1946
sjres61_ps <- subset(sjres61_ps, select=c(partyregion, pctyay, year, bill))

#S.J.Res. 25
sjres25 <-subset(rollcalls_sen, select=c(partyregion, votesen_sjres25_f))
sjres25 <-na.omit(sjres25)
sjres25_ps <- melt(xtabs(~partyregion+votesen_sjres25_f, data=sjres25))

sjres25_ps <- dcast(sjres25_ps, partyregion ~ votesen_sjres25_f, value.var="value")
sjres25_ps <- rename(sjres25_ps, c("0"="nay"))
sjres25_ps <- rename(sjres25_ps, c("1"="yay"))
sjres25_ps$pctyay <- sjres25_ps$yay/(sjres25_ps$yay + sjres25_ps$nay)
sjres25_ps$bill <- "SJRes25 (1950)"
sjres25_ps$year <- 1950
sjres25_ps <- subset(sjres25_ps, select=c(partyregion, pctyay, year, bill))

#S.J.Res. 25 Hayden Amendment
sjres25hayden <-subset(rollcalls_sen, select=c(partyregion, votesen_sjres25_haydenamend_f))           
sjres25hayden <-na.omit(sjres25hayden)
sjres25hayden_ps <- melt(xtabs(~partyregion+votesen_sjres25_haydenamend_f, data=sjres25hayden))
sjres25hayden_ps <- dcast(sjres25hayden_ps, partyregion ~ votesen_sjres25_haydenamend_f, value.var="value")
sjres25hayden_ps <- rename(sjres25hayden_ps, c("0"="nay"))
sjres25hayden_ps <- rename(sjres25hayden_ps, c("1"="yay"))
sjres25hayden_ps$pctyay <- sjres25hayden_ps$yay/(sjres25hayden_ps$yay + sjres25hayden_ps$nay)
sjres25hayden_ps$bill <- "SJRes25 Hayden Amendment (1950)"
sjres25hayden_ps$year <- 1950
sjres25hayden_ps <- subset(sjres25hayden_ps, select=c(partyregion, bill, year, pctyay))

#S.J.Res. 25 Kefauver Amendment
sjres25kefauver <-subset(rollcalls_sen, select=c(partyregion, votesen_sjres25_kefauver_f))           
sjres25kefauver <-na.omit(sjres25kefauver)
sjres25kefauver_ps <- melt(xtabs(~partyregion+votesen_sjres25_kefauver_f, data=sjres25kefauver))
sjres25kefauver_ps <- dcast(sjres25kefauver_ps, partyregion ~ votesen_sjres25_kefauver_f, value.var="value")
sjres25kefauver_ps <- rename(sjres25kefauver_ps, c("0"="nay"))
sjres25kefauver_ps <- rename(sjres25kefauver_ps, c("1"="yay"))
sjres25kefauver_ps$pctyay <- sjres25kefauver_ps$yay/(sjres25kefauver_ps$yay + sjres25kefauver_ps$nay)
sjres25kefauver_ps$bill <- "SJRes25 Kefauver Amendment (1950)"
sjres25kefauver_ps$year <- 1950
sjres25kefauver_ps <- subset(sjres25kefauver_ps, select=c(partyregion, bill, year, pctyay))

#S.J.Res. 49
sjres49 <-subset(rollcalls_sen, select=c(partyregion, votesen_sjres49_f))
sjres49 <-na.omit(sjres49)
sjres49_ps <- melt(xtabs(~partyregion+votesen_sjres49_f, data=sjres49))

sjres49_ps <- dcast(sjres49_ps, partyregion ~ votesen_sjres49_f, value.var="value")
sjres49_ps <- rename(sjres49_ps, c("0"="nay"))
sjres49_ps <- rename(sjres49_ps, c("1"="yay"))
sjres49_ps$pctyay <- sjres49_ps$yay/(sjres49_ps$yay + sjres49_ps$nay)
sjres49_ps$bill <- "SJRes49 (1953)"
sjres49_ps$year <- 1953
sjres49_ps <- subset(sjres49_ps, select=c(partyregion, pctyay, year, bill))

#S.J.Res. 49 Hayden Amendment
sjres49hayden <-subset(rollcalls_sen, select=c(partyregion, votesen_sjres49_haydenamend_f))           
sjres49hayden <-na.omit(sjres49hayden)
sjres49hayden_ps <- melt(xtabs(~partyregion+votesen_sjres49_haydenamend_f, data=sjres49hayden))
sjres49hayden_ps <- dcast(sjres49hayden_ps, partyregion ~ votesen_sjres49_haydenamend_f, value.var="value")
sjres49hayden_ps <- rename(sjres49hayden_ps, c("0"="nay"))
sjres49hayden_ps <- rename(sjres49hayden_ps, c("1"="yay"))
sjres49hayden_ps$pctyay <- sjres49hayden_ps$yay/(sjres49hayden_ps$yay + sjres49hayden_ps$nay)
sjres49hayden_ps$bill <- "SJRes49 Hayden Amendment (1953)"
sjres49hayden_ps$year <- 1953
sjres49hayden_ps <- subset(sjres49hayden_ps, select=c(partyregion, bill, year, pctyay))

#Combine

combined <- rbind(sjres61_ps, sjres25_ps, sjres25hayden_ps, sjres25kefauver_ps, sjres49_ps,  sjres49hayden_ps)

combined <- within(combined,{
  bill <- factor(bill, levels=c("SJRes61 (1946)", "SJRes25 (1950)", "SJRes25 Hayden Amendment (1950)", "SJRes25 Kefauver Amendment (1950)", "SJRes49 (1953)", "SJRes49 Hayden Amendment (1953)"))
})

combined$label[combined$partyregion=="Republican"]  <- "R"
combined$label[combined$partyregion=="Non-southern Democrat"]  <- "SD"
combined$label[combined$partyregion=="Southern Democrat"]  <- "NSD"

combined <- within(combined,{
  label <- factor(label, levels=c("NSD", "SD", "R"))
})


pdf("Roll Call Votes bw 13.pdf")
ggplot(combined, aes(x=label, y=pctyay))+
  geom_bar(position="dodge", stat="identity") + 
  ggtitle("Roll Call Voting Coalitions on ERA Bills and Amendments (13 State South)\n") + 
  labs(x = "Party/Region", y = "% Supporting ERA\n") + 
  facet_wrap(~bill, ncol=2) +
  theme_bw()
dev.off()



