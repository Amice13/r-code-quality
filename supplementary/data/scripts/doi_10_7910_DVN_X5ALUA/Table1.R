## Fraga and Miller (2021) "Who Does Voter ID Keep from Voting?" ##

## Table 1 and associated statistics ##

require(data.table)
require(dplyr)

load('FragaMiller_JOP_ReplicationData.RData')

fragamiller_jopdata$RIDbin <- 0
fragamiller_jopdata$RIDbin[fragamiller_jopdata$RID > 0] <- 1

fragamiller_jopdata$NoRID <- abs(1-fragamiller_jopdata$RID)

# Percent of 2016 voters with no race information (0.7%)
nrow(subset(fragamiller_jopdata, is.na(pred_whi) & Vote2016 == 1)) / nrow(subset(fragamiller_jopdata, Vote2016 == 1))
# Percent of RID filers with no race information (1.14, plus unmatched RIDs = approx 1.2%)
nrow(subset(fragamiller_jopdata, is.na(pred_whi) & Vote2016 == 1 & RIDbin == 1)) / nrow(subset(fragamiller_jopdata, Vote2016 == 1 & RIDbin == 1))

##################################################################
## Table 1: Racial Composition of RID versus no RID 2016 Voters ##

tab1 <- fragamiller_jopdata %>%
	filter(Vote2016 == 1) %>%
	summarize(NWhite_RID = sum(pred_whi*RID, na.rm=TRUE), NWhite_NoRID = sum(pred_whi*NoRID, na.rm=TRUE), NBlack_RID = sum(pred_bla*RID, na.rm=TRUE), NBlack_NoRID = sum(pred_bla*NoRID, na.rm=TRUE), NLatino_RID = sum(pred_his*RID, na.rm=TRUE), NLatino_NoRID = sum(pred_his*NoRID, na.rm=TRUE), NAsian_RID = sum(pred_asi*RID, na.rm=TRUE), NAsian_NoRID = sum(pred_asi*NoRID, na.rm=TRUE), NOther_RID = sum(pred_oth*RID, na.rm=TRUE), NOther_NoRID = sum(pred_oth*NoRID, na.rm=TRUE))

tab1 <- melt(tab1)
tab1$RID <- 1
tab1$RID[grepl("NoRID", tab1$variable)] <- 0
tab1$variable <- sub("_RID","", tab1$variable)
tab1$variable <- sub("_NoRID","", tab1$variable)
tab1 <- dcast(tab1, RID ~ variable, value.var="value", fun.aggregate=sum)

tab1 <- tab1 %>%
	group_by(RID) %>%
	mutate(PWhite = NWhite/(NWhite + NBlack + NLatino + NAsian + NOther), PBlack = NBlack/(NWhite + NBlack + NLatino + NAsian + NOther), PLatino = NLatino/(NWhite + NBlack + NLatino + NAsian + NOther), PAsian = NAsian/(NWhite + NBlack + NLatino + NAsian + NOther), POther = NOther/(NWhite + NBlack + NLatino + NAsian + NOther))
tab1

############################
## t-tests of differences ##

fragamiller_jopdata$pred_min <- fragamiller_jopdata$pred_bla + fragamiller_jopdata$pred_his + fragamiller_jopdata$pred_asi + fragamiller_jopdata$pred_oth

t.test(pred_bla ~ RIDbin, data=subset(fragamiller_jopdata, Vote2016 == 1))
t.test(pred_his ~ RIDbin, data=subset(fragamiller_jopdata, Vote2016 == 1))
t.test(pred_asi ~ RIDbin, data=subset(fragamiller_jopdata, Vote2016 == 1))
t.test(pred_min ~ RIDbin, data=subset(fragamiller_jopdata, Vote2016 == 1))

summary(lm(RID ~ pred_bla, data=subset(fragamiller_jopdata, Vote2016 == 1)))
summary(lm(RID ~ pred_his, data=subset(fragamiller_jopdata, Vote2016 == 1)))
summary(lm(RID ~ pred_asi, data=subset(fragamiller_jopdata, Vote2016 == 1)))
summary(lm(RID ~ pred_min, data=subset(fragamiller_jopdata, Vote2016 == 1)))