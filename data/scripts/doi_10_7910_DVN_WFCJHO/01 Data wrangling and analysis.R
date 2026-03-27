### This sciprt replicates the analyses reported in Toshkov and Romeijn (2021) in Electoral Studies
### 'How to estimate the policy preferences of party supporters: Disaggregating data from voting advice applications versus modeling survey responses'
### Last update: 4 October 2021 by Dimiter Toshkov

# 00 Libraries, functions and working directory ------------------------------
library(data.table)
library(car)
library(plyr)
library(reshape2)
library(RColorBrewer)
library(tidyverse)
library(htmlTable)
library(epiR)
##prepare functions which ignore the NAs
sum.na<-function (x) {sum(x, na.rm=T)}
mean.na<-function (x) {mean(x, na.rm=T)}
median.na<-function (x) {median(x, na.rm=T)}
sd.na<-function (x) {sd(x, na.rm=T)}

# 01 Load and arrange the Dutch and German survey data ----------------------------------------
# This data file containts the aggregated and MRP-ed survey data from NL and DE
surveys<-read.table("./data/surveys_data.txt", sep='\t', dec='.', header=T)
surveys.nl = filter (surveys, country=='NL') # Dutch numbers only
surveys.de = filter (surveys, country=='DE') # Germany numbers only

# Reverse coding of adoption issue in Germany
surveys.de$mrppred[surveys.de$vaa_issue_name=='adoption'] = 1 - surveys.de$mrppred[surveys.de$vaa_issue_name=='adoption']
surveys.de$mrppredmin50[surveys.de$vaa_issue_name=='adoption'] = 1 - surveys.de$mrppredmin50[surveys.de$vaa_issue_name=='adoption']
surveys.de$mrppredplus50[surveys.de$vaa_issue_name=='adoption'] = 1 - surveys.de$mrppredplus50[surveys.de$vaa_issue_name=='adoption']
surveys.de$disaggregation<- ifelse (surveys.de$vaa_issue_name=='adoption', 1-surveys.de$disaggregation, surveys.de$disaggregation)
surveys.de$disaggregationdk<- ifelse (surveys.de$vaa_issue_name=='adoption', 1-surveys.de$disaggregationdk, surveys.de$disaggregationdk)

# 02a Load and arrange the German VAA data ---------------------------------------------------------
de <- read.csv (file='./data/vaa_de_2013.csv', row.names = NULL)[,-1]

# This data file is a result from an export from the raw VAA data (which is not publicly available) using the following syntax
# Data reference: Krouwel, A., Eckert, T. Fanto, S. (2013) Kieskompas (Election Compass) Voting Advice Application Data for the 2013 German Federal Election. Kieskompas: Amsterdam.

# de <- fread("./data/de2013election.csv")
# de <- de %>%
#   filter (duration>100, duration<(60*30), country_code=="DE", de$language_id==1) %>%
#   mutate (votefed = car::recode(extra_question_9,  "144='CDU/CSU'; 145='SPD';146='FDP'; 
#                    147='Die Linke';148='Die Gruene';149='Piraten'; 150='NPD'; else=NA"), #last Lander vote
#           vote2009 = car::recode(extra_question_8, "134='CDU/CSU'; 135='SPD';136='FDP';137='Die Linke';138='Die Gruenen';139='Piraten';
#                     140='NPD'; else=NA"),
#           issue13  = car::recode(answer_28,  "-1=NA"), ##create new variables for the issues with the proper numbers and recode -1s to NAs
#           issue10  = car::recode(answer_1,  "-1=NA"),
#           issue18 = car::recode(answer_32, "-1=NA"),
#           issue30 = car::recode(answer_18, "-1=NA"),
#           issue1 = car::recode(answer_10, "-1=NA")) %>%
#   select (votefed, vote2009, starts_with('issue'))  
# write.csv(de,"./data/vaa_de_2013.csv")

# define the party supporter in various ways
de$supporter1a<-ifelse(is.na(de$vote2009)==F, 1, 0) #create a variable for weak supporters
de$supporter2a<-ifelse(de$vote2009==de$votefed,1,0) #strong supporters only

# subset the data to the issue positions and party affiliation variables only
de2 <- de %>% filter(supporter1a==1) %>% select (-supporter1a, -supporter2a, -votefed) #filter the dataset by type of supporters

### This block below aggregates the data at the party level
party.nas    <-ddply(de2,.(vote2009) , numcolwise(.fun = function(x) sum(is.na(x)))) # calculate party-issue level NAs
party.neutral<-ddply(de2,.(vote2009) , numcolwise(.fun = function(x) sum.na(x==3))) # calculate party-issue level neutral responses
party.minus2 <-ddply(de2,.(vote2009) , numcolwise(.fun = function(x) sum.na(x==1))) # calculate party-issue level very neg responses
party.minus1 <-ddply(de2,.(vote2009) , numcolwise(.fun = function(x) sum.na(x==2))) # calculate party-issue level neg responses
party.plus1  <-ddply(de2,.(vote2009) , numcolwise(.fun = function(x) sum.na(x==4))) # calculate party-issue level pos responses
party.plus2  <-ddply(de2,.(vote2009) , numcolwise(.fun = function(x) sum.na(x==5))) # calculate party-issue level very pos responses
party.all    <-ddply(de2,.(vote2009) , numcolwise (.fun = function(x) sum.na(is.na(x)==F))) # calculate party-issue level non NA responses

party.support1<-(party.plus1[-1]+party.plus2[-1])/(party.all[-1]-party.neutral[-1]) # support from all with opinion
party.support2<-(party.plus1[-1]+party.plus2[-1])/(party.all[-1]+party.nas[-1]) # support from all, including NAs

party.support1$vote2009<-party.all$vote2009
party.support2$vote2009<-party.all$vote2009

party.me1.50<-party.all # calculate margins of error; start with a copied table just to get the labels
party.me2.50<-party.all # calculate margins of error; start with a copied table just to get the labels

party.me1.50[,-1]<-qnorm(.75)*(sqrt(party.support1[,-6]*(1-party.support1[,-6])/(party.all[,-1]-party.neutral[,-1])))
party.me2.50[,-1]<-qnorm(.75)*(sqrt(party.support2[,-6]*(1-party.support2[,-6])/(party.all[,-1]+party.nas[,-1])))

m1<-melt(party.support1, "vote2009") #melt the means matrix
m1$index<-paste(m1$vote2009, m1$variable, sep=".") #create an index variable
colnames(m1)<-c("party","issue","support1","index") #rename the columns

m2<-melt(party.support2, "vote2009") #melt the means matrix
m2$index<-paste(m2$vote2009, m2$variable, sep=".") #create an index variable
colnames(m2)<-c("party","issue","support2","index") #rename the columns

m3<-melt(party.me1.50, "vote2009") #melt the means matrix
m3$index<-paste(m3$vote2009, m3$variable, sep=".") #create an index variable
colnames(m3)<-c("party","issue","me1.50","index") #rename the columns

m4<-melt(party.me2.50, "vote2009") #melt the means matrix
m4$index<-paste(m4$vote2009, m4$variable, sep=".") #create an index variable
colnames(m4)<-c("party","issue","me2.50","index") #rename the columns

dm<-merge(m1,m2[,3:4], by="index")
dm<-merge(dm,m3[,3:4], by="index")
dm<-merge(dm,m4[,3:4], by="index")
# 03a Merge German survey and VAA data ----------------------------------------
### Merge everything together
dm<-merge(dm, surveys.de, by="index")
# 04a Demographic overview of VAA users (Table 1a) -----------------------------------------

### Some tabulations for summary stats
td.p<-summary(as.factor(de$vote2009)) # respondents per party (2010 vote)
td.p <- data.frame(td.p[-c(5,6,8)]) # exclude NAs
td.p[,2] = rownames(td.p)

html.dv.table = htmlTable::htmlTable(td.p, rnames = FALSE,
                                     col.columns = c('white','lightgrey'),
                                     tfoot = 'Note:',
                                     align = c(rep('c', 13)),
                                     align.header = c(rep('c', 13)),
                                     header = c('N', 'party'),
                                     caption = "Table 1a. Number of VAA party supporters in Germany.")

print(html.dv.table, type = "html", file = './tables/Table_1a_de_usersperparty.html')
# 05a Correctional analysis I DE, party supporters based on retrospective voting (Table 2) -------------------------------------------------------------

# Pearson's correlations
cor(dm$support1, dm$mrppred) # VAA support from those with opinion (excl. neutral) and MRP estimates
cor(dm$support2, dm$mrppreddk) # VAA support from all and MRP estimates

cor(dm$mrppred, dm$disaggregation) # MRP estimates and disaggregation
cor(dm$support1, dm$disaggregation) # VAA estimates (those with opinion) and disaggregation
cor(dm$support2, dm$disaggregationdk) # VAA estimates (all) and disaggregation

# Lin's concordance coefficients
epi.ccc(dm$support1, dm$mrppred, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est # VAA support from those with opinion (excl. neutral) and MRP estimates
epi.ccc(dm$support2, dm$mrppreddk, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est # VAA support from all and MRP estimates

epi.ccc(dm$support1, dm$disaggregation, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est # VAA estimates (those with opinion) and disaggregation
epi.ccc(dm$support2, dm$disaggregationdk, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est # VAA estimates (all) and disaggregation

# Correlations and MAR per issue 
out1 <- dm %>%
  group_by ( vaa_issue_name ) %>%
  summarize ( cor = round (cor (support1, mrppred), 2),
              lcc = round (epi.ccc(support1, mrppred, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est, 2),
              mae = round (mean (abs(support1 - mrppred)),2)*100,
              cor.dk = round (cor (support2, mrppreddk), 2),
              lcc.dk = round (epi.ccc(support2, mrppreddk, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est, 2),
              mae.dk = round (mean (abs(support2 - mrppreddk)),2)*100
  )

out2 <- dm %>%
  group_by ( partyname ) %>%
  summarize ( cor = round (cor (support1, mrppred), 2),
              lcc = round (epi.ccc(support1, mrppred, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est, 2),
              mae = round (mean (abs(support1 - mrppred)),2)*100,
              cor.dk = round (cor (support2, mrppreddk), 2),
              lcc.dk = round (epi.ccc(support2, mrppreddk, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est, 2),
              mae.dk = round (mean (abs(support2 - mrppreddk)),2)*100
  )
colnames(out2)[1] <- colnames(out1)[1]  
out12 = bind_rows(out1, out2)

html.dv.table = htmlTable::htmlTable(out12, rnames = FALSE,
                                     col.columns = c('white','lightgrey'),
                                     tfoot = 'Note: Mean absolute errors are in percentage points.',
                                     align = c('l','r','r','r','r','r','r'),
                                     align.header = c('l','c','c','c','c','c','c'),
                                     header = c('Policy issue / Party group','COR','LCC','MAE','COR (with DK)','LCC (with DK)','MAE (with DK)'),
                                     caption = "Table 2. Comparing the two sets of party support estimates per policy issue and per party group in Germany.")

print(html.dv.table, type = "html", file = './tables/Table_2_de_cor_mae.html')


# Correlations and MAR per issue and party, disaggregated
out1 <- dm %>%
  group_by ( vaa_issue_name ) %>%
  summarize ( cor = round (cor (support1, disaggregation), 2),
              lcc = round (epi.ccc(support1, disaggregation, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est, 2),
              mae = round (mean (abs(support1 - disaggregation)),2)*100,
              
              cor.dk = round (cor (support2, disaggregationdk), 2),
              lcc.dk = round (epi.ccc(support2, disaggregationdk, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est, 2),
              mae.dk = round (mean (abs(support2 - disaggregationdk)),2)*100
  )

out2 <- dm %>%
  group_by ( partyname ) %>%
  summarize ( cor = round (cor (support1, disaggregation), 2),
              lcc = round (epi.ccc(support1, disaggregation, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est, 2),
              mae = round (mean (abs(support1 - disaggregation)),2)*100,
              
              cor.dk = round (cor (support2, disaggregationdk), 2),
              lcc.dk = round (epi.ccc(support2, disaggregationdk, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est, 2),
              mae.dk = round (mean (abs(support2 - disaggregationdk)),2)*100
  )

colnames(out2)[1] <- colnames(out1)[1]  
out12 = bind_rows(out1, out2)

html.dv.table = htmlTable::htmlTable(out12, rnames = FALSE,
                                     col.columns = c('white','lightgrey'),
                                     tfoot = 'Note: Mean absolute errors are in percentage points.',
                                     align = c('l','r','r','r','r', 'r', 'r'),
                                     align.header = c('l','c','c','c','c', 'c', 'c'),
                                     header = c('Policy issue / Party group','Correlation','LCC','Mean abs. error','Correlation (with DK)','LCC (with DK)','Mean abs. error (with DK)'),
                                     caption = "Table A4. Comparing VAA-based on disaggreagted party support estimates per policy issue and per party group in Germany.")

print(html.dv.table, type = "html", file = './tables/Table_A4_de_cor_mae_disaggregate.html')

# correlation mismatch and party size
out22 <- left_join(out2, party.all, by=c('vaa_issue_name'='vote2009'))
cor(out22$cor, out22$issue10)
cor(out22$lcc, out22$issue10)

# correlation mismatch and extreme positions
out3 <- dm %>%
  group_by ( vaa_issue_name ) %>%
  summarize (overall.support = mean(support1) )

out3 <- left_join(dm, out3) %>% 
  mutate (extreme = abs(support1 - overall.support))

out4 <- out3 %>%
  group_by ( party ) %>%
  summarize (mean.extreme = mean(extreme) )

#correlation mismatch and exreme positions
out23 <- left_join(out2, out4, by=c('vaa_issue_name'='party'))
cor(out23$cor, out23$mean.extreme)
cor(out23$lcc, out23$mean.extreme)
# 06a Figure 1 DE: All pairs in one figure ---------------------------------
palette(brewer.pal(n = 5, name = "Dark2"))
png('./figures/Figure_1_corr_de.png', width=3000, height=2000, res=300) #turn off the lables for png
par(mfrow=(c(1,2)))
par(mar=c(5,5,4,1))
plot(dm$support1, dm$mrppred, col=dm$issue, pch=16, cex=2, ylim=c(0,1), xlim=c(0,1), main="Share of support\n(from all strong supporters with opinion)", xlab="VAA estimates", ylab="Share of support: MRP-ed survey estimates",
     cex.lab=1.5)
text(paste0("Correlation=",round(cor(dm$support1, dm$mrppred),2)), x=0.8, y= 0.01)
lines(abline(0,1, lwd=2))
lines(abline(lm(mrppred~support1, data=dm), col="red", lwd=2))

plot(dm$support2, dm$mrppred, col=dm$issue, pch=16, cex=2, ylim=c(0,1), xlim=c(0,1), main="Share of support\n(from all strong supporters)", xlab ="VAA estimates", ylab="Share of support: MRP-ed survey estimates",
     cex.lab=1.5)
text(paste0("Correlation=",round(cor(dm$support2, dm$mrppred),2)), x=0.8, y= 0.01)
lines(abline(0,1, lwd=2))
lines(abline(lm(mrppreddk~support2, data=dm), col="red", lwd=2))
dev.off()

# 07a Figures A5.1 and A5.2 DE: Dotplot figures (those with opinion and all responses) ---------------------------------
png('./figures/FA5_1_dotplot_de.png', width=3000, height=4000, res=300)
par(mfrow=(c(1,1)))
dm<-dm[order(dm$support1),]
par(mar=c(5,12,1,1))
plot(x=rep(-1,100), y=1:100, xlim=c(0,1), xaxt="n", yaxt="n", pch=16, col="red", xlab="Estimates of policy positions of party supporters (share support). Red = VAA; Blue = MRP-ed survey", ylab="", cex.lab=1.2)
points(x=dm$support1, y=seq(2,100,4), col="red", pch=16, cex=0.75)
points(x=dm$mrppred, y=seq(3,100,4), col="blue", pch=16, cex=0.75)
segments(x0=(dm$support1-dm$me1.50), x1=(dm$support1+dm$me1.50),y0=seq(2,100,4), col="red" )
segments(x0=(dm$mrppredmin50), x1=(dm$mrppredplus50),   y0=seq(3,100,4), col="blue" )
axis(1,at=seq(0, 1, 0.25), labels=seq(0, 1, 0.25), cex.axis=1.2)
axis(2,at=seq(2.5,100, 4), labels=dm$index.name, cex.axis=1.2, col.axis="grey28", las=1)
segments(x0=rep(0,60), x1=rep(1,60), y0=seq(0.3,101,4) , col="lightgrey", lwd=.5)
dev.off()

png('./figures/FA5_2_dotplot2_de.png', width=3000, height=4000, res=300)
par(mfrow=(c(1,1)))
dm<-dm[order(dm$support2),]
par(mar=c(5,12,1,1))
plot(x=rep(-1,100), y=1:100, xlim=c(0,1), xaxt="n", yaxt="n", pch=16, col="red", xlab="Estimates of policy positions of party supporters (share support). Red = VAA; Blue = MRP-ed survey", ylab="", cex.lab=1.2)
points(x=dm$support2, y=seq(2,100,4), col="red", pch=16, cex=0.75)
points(x=dm$mrppreddk, y=seq(3,100,4), col="blue", pch=16, cex=0.75)
segments(x0=(dm$support2-dm$me2.50), x1=(dm$support2+ dm$me2.50),y0=seq(2,100,4), col="red" )
segments(x0=(dm$mrppreddkmin50), x1=(dm$mrppreddkplus50),   y0=seq(3,100,4), col="blue" )
axis(1,at=seq(0, 1, 0.25), labels=seq(0, 1, 0.25), cex.axis=1.2)
axis(2,at=seq(2.5,100, 4), labels=dm$index.name, cex.axis=1.2, col.axis="grey28", las=1)
segments(x0=rep(0,60), x1=rep(1,60), y0=seq(0.3,101,4) , col="lightgrey", lwd=.5)
dev.off()

# 08a Figures A5.3 and A5.4, A5.5 and A5.6 DE: Dotplot figures per isssue and per party ---------------------------------
png('./figures/FA5_3_dotplot_issue_de.png', width=3000, height=2000, res=300)
par(mfrow=(c(2,3)))
u<-unique(dm$issue)
for(i in 1:length(u)){
  dm.sub<-subset(dm, dm$issue==u[i])
  dm.sub<-dm.sub[order(dm.sub$support1),]
  par(mar=c(3,6,3,1))
  plot(x=rep(-1,20), y=1:20, xlim=c(0,1), xaxt="n", yaxt="n", pch=16, col="red", ylab="", main=dm.sub$vaa_issue_name[1], cex.main=2)
  points(x=dm.sub$support1, y=seq(2,20,4), col="red", pch=16, cex=1)
  points(x=dm.sub$mrppred, y=seq(3,20,4), col="blue", pch=16, cex=1)
  segments(x0=(dm.sub$support1-dm.sub$me1.50), x1=(dm.sub$support1+dm.sub$me1.50),y0=seq(2,20,4), col="red" )
  segments(x0=(dm.sub$mrppredmin50), x1=(dm.sub$mrppredplus50),   y0=seq(3,20,4), col="blue" )
  axis(1,at=seq(0, 1, 0.25), labels=seq(0, 1, 0.25), cex.axis=1.2)
  axis(2,at=seq(2.5,20, 4), labels=dm.sub$party, cex.axis=1.2, col.axis="black", las=1)
  segments(x0=rep(0,20), x1=rep(1,20), y0=seq(0.3,21,4) , col="grey", lwd=.5)
}
dev.off()

png('./figures/FA5_4_dotplot_issue2_de.png', width=3000, height=2000, res=300)
par(mfrow=c(2,3))
u<-unique(dm$issue)
for(i in 1:length(u)){
  dm.sub<-subset(dm, dm$issue==u[i])
  dm.sub<-dm.sub[order(dm.sub$support2),]
  par(mar=c(3,6,3,1))
  plot(x=rep(-1,20), y=1:20, xlim=c(0,1), xaxt="n", yaxt="n", pch=16, col="red", ylab="", main=dm.sub$vaa_issue_name[1], cex.main=2)
  points(x=dm.sub$support2, y=seq(2,20,4), col="red", pch=16, cex=1)
  points(x=dm.sub$mrppreddk, y=seq(3,20,4), col="blue", pch=16, cex=1)
  segments(x0=(dm.sub$support2-dm.sub$me2.50), x1=(dm.sub$support2+dm.sub$me2.50),y0=seq(2,20,4), col="red" )
  segments(x0=(dm.sub$mrppreddkmin50), x1=(dm.sub$mrppreddkplus50),   y0=seq(3,20,4), col="blue" )
  axis(1,at=seq(0, 1, 0.25), labels=seq(0, 1, 0.25), cex.axis=1.2)
  axis(2,at=seq(2.5,20, 4), labels=dm.sub$party, cex.axis=1.2, col.axis="black", las=1)
  segments(x0=rep(0,20), x1=rep(1,20), y0=seq(0.3,21,4) , col="lightgrey", lwd=.5)
}
dev.off()

png('./figures/FA5_5_dotplot_party_de.png', width=3000, height=2000, res=300)
par(mfrow=(c(2,3)))
u<-unique(dm$party)
for(i in 1:length(u)){
  dm.sub<-subset(dm, dm$party==u[i])
  dm.sub<-dm.sub[order(dm.sub$support1),]
  par(mar=c(3,6,3,1))
  plot(x=rep(-1,20), y=1:20, xlim=c(0,1), xaxt="n", yaxt="n", pch=16, col="red", ylab="", main=dm.sub$party[1], cex.main=2)
  points(x=dm.sub$support1, y=seq(2,20,4), col="red", pch=16, cex=1)
  points(x=dm.sub$mrppred, y=seq(3,20,4), col="blue", pch=16, cex=1)
  segments(x0=(dm.sub$support1-dm.sub$me1.50), x1=(dm.sub$support1+dm.sub$me1.50),y0=seq(2,20,4), col="red" )
  segments(x0=(dm.sub$mrppredmin50), x1=(dm.sub$mrppredplus50),   y0=seq(3,20,4), col="blue" )
  axis(1,at=seq(0, 1, 0.25), labels=seq(0, 1, 0.25), cex.axis=1.2)
  axis(2,at=seq(2.5,20, 4), labels=dm.sub$vaa_issue_name, cex.axis=1.2, col.axis="black", las=1)
  segments(x0=rep(0,20), x1=rep(1,24), y0=seq(0.3,21,4) , col="grey", lwd=.5)
}
dev.off()

png('./figures/FA5_6_dotplot2_party_de.png', width=3000, height=2000, res=300)
par(mfrow=c(2,3))
u<-unique(dm$party)
for(i in 1:length(u)){
  dm.sub<-subset(dm, dm$party==u[i])
  dm.sub<-dm.sub[order(dm.sub$support2),]
  par(mar=c(3,6,3,1))
  plot(x=rep(-1,20), y=1:20, xlim=c(0,1), xaxt="n", yaxt="n", pch=16, col="red", ylab="", main=dm.sub$party[1], cex.main=2)
  points(x=dm.sub$support2, y=seq(2,20,4), col="red", pch=16, cex=1)
  points(x=dm.sub$mrppreddk, y=seq(3,20,4), col="blue", pch=16, cex=1)
  segments(x0=(dm.sub$support2-dm.sub$me2.50), x1=(dm.sub$support2+dm.sub$me2.50),y0=seq(2,20,4), col="red" )
  segments(x0=(dm.sub$mrppreddkmin50), x1=(dm.sub$mrppreddkplus50),   y0=seq(3,20,4), col="blue" )
  axis(1,at=seq(0, 1, 0.25), labels=seq(0, 1, 0.25), cex.axis=1.2,)
  axis(2,at=seq(2.5,20, 4), labels=dm.sub$vaa_issue_name, cex.axis=1.2, col.axis="black", las=1)
  segments(x0=rep(0,20), x1=rep(1,24), y0=seq(0.3,21,4) , col="lightgrey", lwd=.5)
}
dev.off()



# 09a Analysis DE based on strong supporters ----------------------------------
de2 <- de %>% filter(supporter2a==1) %>% select (-supporter2a, -supporter1a, -votefed) #filter the dataset by type of supporters: STRONG supporters only

### This block below aggregates the data at the party level
party.nas    <-ddply(de2,.(vote2009) , numcolwise(.fun = function(x) sum(is.na(x)))) # calculate party-issue level NAs
party.neutral<-ddply(de2,.(vote2009) , numcolwise(.fun = function(x) sum.na(x==3))) # calculate party-issue level neutral responses
party.minus2 <-ddply(de2,.(vote2009) , numcolwise(.fun = function(x) sum.na(x==1))) # calculate party-issue level very neg responses
party.minus1 <-ddply(de2,.(vote2009) , numcolwise(.fun = function(x) sum.na(x==2))) # calculate party-issue level neg responses
party.plus1  <-ddply(de2,.(vote2009) , numcolwise(.fun = function(x) sum.na(x==4))) # calculate party-issue level pos responses
party.plus2  <-ddply(de2,.(vote2009) , numcolwise(.fun = function(x) sum.na(x==5))) # calculate party-issue level very pos responses
party.all    <-ddply(de2,.(vote2009) , numcolwise (.fun = function(x) sum.na(is.na(x)==F))) # calculate party-issue level non NA responses

party.support1<-(party.plus1[-1]+party.plus2[-1])/(party.all[-1]-party.neutral[-1]) # support from all with opinion
party.support2<-(party.plus1[-1]+party.plus2[-1])/(party.all[-1]+party.nas[-1]) # support from all, including NAs

party.support1$vote2009<-party.all$vote2009
party.support2$vote2009<-party.all$vote2009

party.me1.50<-party.all # calculate margins of error; start with a copied table just to get the labels
party.me2.50<-party.all # calculate margins of error; start with a copied table just to get the labels

party.me1.50[,-1]<-qnorm(.75)*(sqrt(party.support1[,-6]*(1-party.support1[,-6])/(party.all[,-1]-party.neutral[,-1])))
party.me2.50[,-1]<-qnorm(.75)*(sqrt(party.support2[,-6]*(1-party.support2[,-6])/(party.all[,-1]+party.nas[,-1])))

m1<-melt(party.support1, "vote2009") #melt the means matrix
m1$index<-paste(m1$vote2009, m1$variable, sep=".") #create an index variable
colnames(m1)<-c("party","issue","support1","index") #rename the columns

m2<-melt(party.support2, "vote2009") #melt the means matrix
m2$index<-paste(m2$vote2009, m2$variable, sep=".") #create an index variable
colnames(m2)<-c("party","issue","support2","index") #rename the columns

m3<-melt(party.me1.50, "vote2009") #melt the means matrix
m3$index<-paste(m3$vote2009, m3$variable, sep=".") #create an index variable
colnames(m3)<-c("party","issue","me1.50","index") #rename the columns

m4<-melt(party.me2.50, "vote2009") #melt the means matrix
m4$index<-paste(m4$vote2009, m4$variable, sep=".") #create an index variable
colnames(m4)<-c("party","issue","me2.50","index") #rename the columns

dm<-merge(m1,m2[,3:4], by="index")
dm<-merge(dm,m3[,3:4], by="index")
dm<-merge(dm,m4[,3:4], by="index")

dm<-merge(dm, surveys.de, by="index")

# Pearson's correlations
cor(dm$support1, dm$mrppred) # VAA support from those with opinion (excl. neutral) and MRP estimates
cor(dm$support2, dm$mrppreddk) # VAA support from all and MRP estimates

cor(dm$support1, dm$disaggregation) # VAA estimates (those with opinion) and disaggregation
cor(dm$support2, dm$disaggregationdk) # VAA estimates (all) and disaggregation

# Lin's concordance coefficients
epi.ccc(dm$support1, dm$mrppred, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est # VAA support from those with opinion (excl. neutral) and MRP estimates
epi.ccc(dm$support2, dm$mrppreddk, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est # VAA support from all and MRP estimates

epi.ccc(dm$support1, dm$disaggregation, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est # VAA estimates (those with opinion) and disaggregation
epi.ccc(dm$support2, dm$disaggregationdk, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est # VAA estimates (all) and disaggregation

# Correlations and MAR per issue 
out1 <- dm %>%
  group_by ( vaa_issue_name ) %>%
  summarize ( cor = round (cor (support1, mrppred), 2),
              lcc = round (epi.ccc(support1, mrppred, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est, 2),
              mae = round (mean (abs(support1 - mrppred)),2)*100,
              cor.dk = round (cor (support2, mrppreddk), 2),
              lcc.dk = round (epi.ccc(support2, mrppreddk, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est, 2),
              mae.dk = round (mean (abs(support2 - mrppreddk)),2)*100
  )

out2 <- dm %>%
  group_by ( partyname ) %>%
  summarize ( cor = round (cor (support1, mrppred), 2),
              lcc = round (epi.ccc(support1, mrppred, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est, 2),
              mae = round (mean (abs(support1 - mrppred)),2)*100,
              cor.dk = round (cor (support2, mrppreddk), 2),
              lcc.dk = round (epi.ccc(support2, mrppreddk, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est, 2),
              mae.dk = round (mean (abs(support2 - mrppreddk)),2)*100
  )
colnames(out2)[1] <- colnames(out1)[1]  
out12 = bind_rows(out1, out2)

html.dv.table = htmlTable::htmlTable(out12, rnames = FALSE,
                                     col.columns = c('white','lightgrey'),
                                     tfoot = 'Note: Mean absolute errors are in percentage points.',
                                     align = c('l','r','r','r','r','r','r'),
                                     align.header = c('l','c','c','c','c','c','c'),
                                     header = c('Policy issue / Party group','COR','LCC','MAE','COR (with DK)','LCC (with DK)','MAE (with DK)'),
                                     caption = "Alt Table A2. Comparing the two sets of STRONG party support estimates per policy issue and per party group in Germany.")

print(html.dv.table, type = "html", file = './tables/Alt_Table_2_de_cor_mae.html')



# 10a Figure 1 DE(but strong supporters): All pairs in one figure (Part 7.1 of the Appendix) ---------------------------------
palette(brewer.pal(n = 5, name = "Dark2"))
png('./figures/FA7_1_corr_de.png', width=3000, height=2000, res=300) #turn off the lables for png
par(mfrow=(c(1,2)))
par(mar=c(5,5,4,1))
plot(dm$support1, dm$mrppred, col=dm$issue, pch=16, cex=2, ylim=c(0,1), xlim=c(0,1), main="Share of support\n(from all strong supporters with opinion)", xlab="VAA estimates", ylab="Share of support: MRP-ed survey estimates",
     cex.lab=1.5)
text(paste0("Correlation=",round(cor(dm$support1, dm$mrppred),2)), x=0.8, y= 0.01)
lines(abline(0,1, lwd=2))
lines(abline(lm(mrppred~support1, data=dm), col="red", lwd=2))

plot(dm$support2, dm$mrppred, col=dm$issue, pch=16, cex=2, ylim=c(0,1), xlim=c(0,1), main="Share of support\n(from all strong supporters)", xlab ="VAA estimates", ylab="Share of support: MRP-ed survey estimates",
     cex.lab=1.5)
text(paste0("Correlation=",round(cor(dm$support2, dm$mrppred),2)), x=0.8, y= 0.01)
lines(abline(0,1, lwd=2))
lines(abline(lm(mrppreddk~support2, data=dm), col="red", lwd=2))
dev.off()


# 02b Load and arrange the Dutch VAA data ----------------------------------------
tk <- read.csv (file='./data/vaa_nl_2012.csv', row.names = NULL)

# This data file is a result from an export from the raw VAA data (which is not publicly available) using the following syntax
# Data reference: Krouwel, A., Ceulemans, S., Koedam, J., van Heck, S. Boiten, M., van Dieren, L. (2012) Kieskompas (Election Compass) Voting Advice Application Data for the 2012 General Dutch Election. Kieskompas: Amsterdam.

#tk <- fread("./data/TK2012 anonymous 2 (2).csv")
# tk <- tk %>%
#   filter (duration>100, duration<(60*30), country_code=="NL") %>%
#   mutate (vote2012 = car::recode(extra_question_5, "98='PvdA'; 99='VVD';100='CDA'; 101='CU';102='GL';103='SP';
#                     104='D66';105='SGP';106='PvdD';107='PVV'; 108='50PLUS';211='Pirates';212='Anders'; 373='WeetNiet'"),
#           vote2010 = car::recode(extra_question_6, "110='PvdA'; 112='VVD';109='CDA'; 115='CU';114='GL';111='SP';
#                     116='D66';117='SGP';118='PvdD';113='PVV'; 120='WeetNietMeer';121='nietGest'; 122='mochtNiet';119='Anders1'"),
#           supporter = ifelse (vote2010 == vote2012, 1, 0),
#           edu = car::recode(opleiding, "1='4.high'; 2='4.high'; 3='3.midhigh'; 4='3.midhigh'; 5='2.midlow'; 6='2.midlow'; 195='1.lower'; 196 = NA "),
#           sex = ifelse(vrouw==1, 'woman', 'man'),
#           age_cat3 = ifelse(Age<35, 'young', ifelse(Age<65, 'midage', 'old')),
#           edu_cat3 = ifelse(edu=='4.high', 'high_edu', ifelse(edu=='3.midhigh', 'mid_edu', 'low_edu')),
#           index = paste(sex, age_cat3, edu_cat3, sep='.'),
#           pindex = paste(vote2010, sex, age_cat3, edu_cat3, sep='.'),
#           issue4  = car::recode(answer_6,  "-1=NA"), ##create new variables for the issues with the proper numbers and recode -1s to NAs
#           issue7  = car::recode(answer_9,  "-1=NA"),
#           issue17 = car::recode(answer_27, "-1=NA"),
#           issue20 = car::recode(answer_30, "-1=NA"),
#           issue27 = car::recode(answer_39, "-1=NA")) %>%
#   select (vote2010, vote2012, supporter, starts_with('issue'), edu, edu_cat3, vrouw, sex, Age, Age_Cat, age_cat3, index, pindex)  

# Subset for calculating positions and analysis (supporters based on retrospective vote) 
tk2 <- filter (tk, vote2010=="PvdA" | vote2010=="CDA" | vote2010=="CU"  | 
                 vote2010=="D66"  | vote2010=="SP"  | vote2010=="GL"  |
                 vote2010=="SGP"  | vote2010=="PvdD"| vote2010=="PVV" | vote2010=="VVD") %>% 
  select (vote2010, starts_with('issue'))

### This block below aggregates the data at the party level
party.nas    <-ddply(tk2,.(vote2010) , numcolwise(.fun = function(x) sum(is.na(x)))) # calculate party-issue level NAs
party.neutral<-ddply(tk2,.(vote2010) , numcolwise(.fun = function(x) sum.na(x==3))) # calculate party-issue level neutral responses
party.minus2 <-ddply(tk2,.(vote2010) , numcolwise(.fun = function(x) sum.na(x==1))) # calculate party-issue level very neg responses
party.minus1 <-ddply(tk2,.(vote2010) , numcolwise(.fun = function(x) sum.na(x==2))) # calculate party-issue level neg responses
party.plus1  <-ddply(tk2,.(vote2010) , numcolwise(.fun = function(x) sum.na(x==4))) # calculate party-issue level pos responses
party.plus2  <-ddply(tk2,.(vote2010) , numcolwise(.fun = function(x) sum.na(x==5))) # calculate party-issue level very pos responses
party.all    <-ddply(tk2,.(vote2010) , numcolwise (.fun = function(x) sum.na(is.na(x)==F))) # calculate party-issue level non NA responses

party.support1<-(party.plus1[-1]+party.plus2[-1])/(party.all[-1]-party.neutral[-1]) # support from all with opinion
party.support2<-(party.plus1[-1]+party.plus2[-1])/(party.all[-1]+party.nas[-1]) # support from all, including NAs

party.support1$vote2010<-party.all$vote2010 # attach party names
party.support2$vote2010<-party.all$vote2010 # attach party names

party.me1.50<-party.all # calculate margins of error; start with a copied table just to get the labels
party.me2.50<-party.all # calculate margins of error; start with a copied table just to get the labels

party.me1.50[,-1]<-qnorm(.75)*(sqrt(party.support1[,-6]*(1-party.support1[,-6])/(party.all[,-1]-party.neutral[,-1])))
party.me2.50[,-1]<-qnorm(.75)*(sqrt(party.support2[,-6]*(1-party.support2[,-6])/(party.all[,-1]+party.nas[,-1])))

m1<-melt(party.support1, "vote2010") # melt the means matrix
m1$index<-paste(m1$vote2010, m1$variable, sep=".") # create an index variable
colnames(m1)<-c("party","issue","support1","index") # rename the columns

m2<-melt(party.support2, "vote2010") # melt the means matrix
m2$index<-paste(m2$vote2010, m2$variable, sep=".") # create an index variable
colnames(m2)<-c("party","issue","support2","index") # rename the columns

m3<-melt(party.me1.50, "vote2010") # melt the means matrix
m3$index<-paste(m3$vote2010, m3$variable, sep=".") # create an index variable
colnames(m3)<-c("party","issue","me1.50","index") # rename the columns

m4<-melt(party.me2.50, "vote2010") # melt the means matrix
m4$index<-paste(m4$vote2010, m4$variable, sep=".") # create an index variable
colnames(m4)<-c("party","issue","me2.50","index") # rename the columns

m<-merge(m1,m2[,3:4], by="index")
m<-merge(m,m3[,3:4], by="index")
m<-merge(m,m4[,3:4], by="index")

# 03b Merge Dutch survey and VAA data ----------------------------------------
m<-merge(m, surveys.nl, by="index")
m.s <- m %>% filter (party!='CU', party!='SGP',party!='PvdD') #subset excluding small parties

# 04a Descriptive demographic table
# 04b Demographic overview of VAA users (Table A3, Table 1b) -----------------------------------------
tk2 <- filter (tk, vote2010=="PvdA" | vote2010=="CDA" | vote2010=="CU"  |
                 vote2010=="D66"  | vote2010=="SP"  | vote2010=="GL"  |
                 vote2010=="SGP"  | vote2010=="PvdD"| vote2010=="PVV" | vote2010=="VVD") 

t1 <- round(prop.table(table(tk2$vote2010, tk2$edu),1),2)
t2 <- round(prop.table(table(tk2$vote2010, tk2$sex),1),2)
t3 <- round(prop.table(table(tk2$vote2010, tk2$Age_Cat),1),2)

t.all <- data.frame(cbind(t1, t2, t3))
t.all[,12] = rownames(t1)

html.dv.table = htmlTable::htmlTable(t.all, rnames = FALSE,
                                     col.columns = c('white','lightgrey'),
                                     tfoot = 'Note:',
                                     align = c(rep('c', 13)),
                                     align.header = c(rep('c', 13)),
                                     header = c('low.edu','midlow.edu','midhigh.edu','high.edu', 'Men','Women','18-24','25-34','35-49','50-64','65+', 'party'),
                                     caption = "Table A3. Demographic profile of the VAA party supporters in The Netherlands.")

print(html.dv.table, type = "html", file = './tables/Table_A3_nl_demogr.html')

### Some tabulations for summary stats
summary(as.factor(tk$vote2012)) # respondents per party (2012 vote)
t.p<-summary(as.factor(tk$vote2010)) # respondents per party (2010 vote)
table(tk$supporter, tk$vote2012) # strong supporters (support same party at both elections)
table(tk$supporter, tk$vote2010)
t.p <- data.frame(t.p[-c(1,6,7,14,15)]) # exclude NAs
t.p[,2] = rownames(t.p)

html.dv.table = htmlTable::htmlTable(t.p, rnames = FALSE,
                                     col.columns = c('white','lightgrey'),
                                     tfoot = 'Note:',
                                     align = c(rep('c', 13)),
                                     align.header = c(rep('c', 13)),
                                     header = c('N', 'party'),
                                     caption = "Table 1b. Number of VAA party supporters in The Netherlands.")

print(html.dv.table, type = "html", file = './tables/Table_1b_nl_usersperparty.html')
# 05b Correctional analysis I NL, party supporters based on retrospective voting (Table 3) -------------------------------------------------------------

# Pearson's correlations
cor(m$support1, m$mrppred) # VAA support from those with opinion (excl. neutral) and MRP estimates
cor(m$support2, m$mrppreddk) # VAA support from all and MRP estimates

cor(m$mrppred, m$disaggregation) # MRP estimates and disaggregation
cor(m$support1, m$disaggregation) # VAA estimates (those with opinion) and disaggregation
cor(m$support2, m$disaggregationdk) # VAA estimates (all) and disaggregation

# Lin's concordance coefficients
epi.ccc(m$support1, m$mrppred, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est # VAA support from those with opinion (excl. neutral) and MRP estimates
epi.ccc(m$support2, m$mrppreddk, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est # VAA support from all and MRP estimates

epi.ccc(m$support1, m$disaggregation, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est # VAA estimates (those with opinion) and disaggregation
epi.ccc(m$support2, m$disaggregationdk, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est # VAA estimates (all) and disaggregation

# excluding small parties
cor(m.s$support1, m.s$mrppred) # VAA support from those with opinion (excl. neutral) and MRP estimates
cor(m.s$support2, m.s$mrppreddk) # VAA support from all and MRP estimates
epi.ccc(m.s$support1, m.s$mrppred, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est # VAA support from those with opinion (excl. neutral) and MRP estimates
epi.ccc(m.s$support2, m.s$mrppreddk, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est # VAA support from all and MRP estimates

# Correlations and MAR per issue 
out1 <- m %>%
  group_by ( vaa_issue_name ) %>%
  summarize ( cor = round (cor (support1, mrppred), 2),
              lcc = round (epi.ccc(support1, mrppred, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est, 2),
              mae = round (mean (abs(support1 - mrppred)),2)*100,
              
              cor.dk = round (cor (support2, mrppreddk), 2),
              lcc.dk = round (epi.ccc(support2, mrppreddk, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est, 2),
              mae.dk = round (mean (abs(support2 - mrppreddk)),2)*100
  )

# Correlations and MAR per party
out2 <- m %>%
  group_by ( partyname ) %>%
  summarize ( cor = round (cor (support1, mrppred), 2),
              lcc = round (epi.ccc(support1, mrppred, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est, 2),
              mae = round (mean (abs(support1 - mrppred)),2)*100,
              
              cor.dk = round (cor (support2, mrppreddk), 2),
              lcc.dk = round (epi.ccc(support2, mrppreddk, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est, 2),
              mae.dk = round (mean (abs(support2 - mrppreddk)),2)*100
  )

# Merge the two
colnames(out2)[1] <- colnames(out1)[1]  
out12 = bind_rows(out1, out2)

# Table 3, main text
html.dv.table = htmlTable::htmlTable(out12, rnames = FALSE,
                                     col.columns = c('white','lightgrey'),
                                     tfoot = 'Note: Mean absolute errors are in percentage points.',
                                     align = c('l','r','r','r','r', 'r', 'r'),
                                     align.header = c('l','c','c','c','c', 'c', 'c'),
                                     header = c('Policy issue / Party group','Correlation','LCC','Mean abs. error','Correlation (with DK)','LCC (with DK)','Mean abs. error (with DK)'),
                                     caption = "Table 3. Comparing the two sets of party support estimates per policy issue and per party group in The Netherlands.")

print(html.dv.table, type = "html", file = './tables/Table_3_nl_cor_mae.html')

# Correlation mismatch and party size
out22 <- left_join(out2, party.all, by=c('vaa_issue_name'='vote2010'))
cor(out22$cor, out22$issue7)
cor(out22$lcc, out22$issue7)

# correlation mismatch and extreme positions
out3 <- m %>%
  group_by ( vaa_issue_name ) %>%
  summarize (overall.support = mean(support1) )
out3  

out3 <- left_join(m, out3) %>% 
  mutate (extreme = abs(support1 - overall.support))

out4 <- out3 %>%
  group_by ( party ) %>%
  summarize (mean.extreme = mean(extreme) )
out4  

#correlation mismatch and exreme positions
out23 <- left_join(out2, out4, by=c('vaa_issue_name'='party'))
cor(out23$cor, out23$mean.extreme)
cor(out23$lcc, out23$mean.extreme)

# Correlations and MAR per issue and party, with disaggregated survey responses
out1 <- m %>%
  group_by ( vaa_issue_name ) %>%
  summarize ( cor = round (cor (support1, disaggregation), 2),
              lcc = round (epi.ccc(support1, disaggregation, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est, 2),
              mae = round (mean (abs(support1 - disaggregation)),2)*100,
              
              cor.dk = round (cor (support2, disaggregationdk), 2),
              lcc.dk = round (epi.ccc(support2, disaggregationdk, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est, 2),
              mae.dk = round (mean (abs(support2 - disaggregationdk)),2)*100
  )
out2 <- m %>%
  group_by ( partyname ) %>%
  summarize ( cor = round (cor (support1, disaggregation), 2),
              lcc = round (epi.ccc(support1, disaggregation, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est, 2),
              mae = round (mean (abs(support1 - disaggregation)),2)*100,
              
              cor.dk = round (cor (support2, disaggregationdk), 2),
              lcc.dk = round (epi.ccc(support2, disaggregationdk, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est, 2),
              mae.dk = round (mean (abs(support2 - disaggregationdk)),2)*100
  )
colnames(out2)[1] <- colnames(out1)[1]  
out12 = bind_rows(out1, out2)

html.dv.table = htmlTable::htmlTable(out12, rnames = FALSE,
                                     col.columns = c('white','lightgrey'),
                                     tfoot = 'Note: Mean absolute errors are in percentage points.',
                                     align = c('l','r','r','r','r', 'r', 'r'),
                                     align.header = c('l','c','c','c','c', 'c', 'c'),
                                     header = c('Policy issue / Party group','Correlation','LCC','Mean abs. error','Correlation (with DK)','LCC (with DK)','Mean abs. error (with DK)'),
                                     caption = "Table A5. Comparing VAA-based and DISAGGREGATED party support estimates per policy issue and per party group in The Netherlands.")

print(html.dv.table, type = "html", file = './tables/Table_A5_nl_cor_mae_disaggregate.html')


# 06b Figure 2 NL: All pairs in one figure -------------------------------
palette(brewer.pal(n = 5, name = "Dark2"))
png('./figures/Figure_2_corr_nl.png', width=3000, height=2000, res=300) #turn off the lables for png
par(mfrow=(c(1,2)))
par(mar=c(5,5,4,1))
plot(m$support1, m$mrppred, col=m$issue, pch=16, cex=2, ylim=c(0,1), xlim=c(0,1), cex.lab=1.5,
     main="Share of support (from all with opinion)", xlab = 'VAA estimates', ylab="Share of support: MRP-ed survey estimates")
text(paste0("Correlation = ",round(cor(m$support1, m$mrppred),2)), x=0.8, y= 0.01)
lines(abline(0,1, lwd=2))
lines(abline(lm(mrppred~support1, data=m), col="red", lwd=2))

plot(m$support2, m$mrppreddk, col=m$issue, pch=16, cex=2, ylim=c(0,1), xlim=c(0,1), cex.lab=1.5, 
     main="Share of support (from all recrods)", xlab = 'VAA estimates', ylab="Share of support: MRP-ed survey estimates")
text(paste0("Correlation = ",round(cor(m$support2, m$mrppreddk),2)), x=0.8, y= 0.01)
lines(abline(0,1, lwd=2))
lines(abline(lm(mrppreddk~support2, data=m), col="red", lwd=2))
dev.off()
# 07b Figures A6.1 and A6.2 NL: Dotplot figures (those with opinion and all responses) -------------------------------
png('./figures/FA6_1_dotplot_withopinion_nl.png', width=3000, height=4000, res=300)
par(mfrow=(c(1,1)))
m<-m[order(m$support1),]
par(mar=c(5,10,1,1))
plot(x=rep(-1,200), y=1:200, xlim=c(0,1), xaxt="n", yaxt="n", pch=16, col="red", xlab="Estimates of policy positions of party supporters (share support). Red = VAA; Blue = MRP-ed survey.\n Empty diamonds show the disaggregated survey means.", ylab="")
points(x=m$support1, y=seq(2,200,4), col="red", pch=16, cex=0.75)
points(x=m$mrppred, y=seq(3,200,4), col="blue", pch=16, cex=0.75)
#points(x=m$disaggregation, y=seq(2.5,200,4), col="darkgreen", pch=5, cex=0.75)
segments(x0=(m$support1-m$me1.50), x1=(m$support1+m$me1.50),y0=seq(2,200,4), col="red" )
segments(x0=(m$mrppredmin50), x1=(m$mrppredplus50),   y0=seq(3,200,4), col="blue" )

axis(1,at=seq(0, 1, 0.25), labels=seq(0, 1, 0.25))
axis(2,at=seq(2.5,200, 4), labels=m$index.name, cex.axis=.7, col.axis="grey28", las=1)
segments(x0=rep(0,50), x1=rep(1,50), y0=seq(0.3,201,4) , col="lightgrey", lwd=.5)
dev.off()

png('./figures/FA6_2_dotplot_allresponses_nl.png', width=3000, height=4000, res=300)
par(mfrow=(c(1,1)))
m<-m[order(m$support2),]
par(mar=c(5,10,1,1))
plot(x=rep(-1,200), y=1:200, xlim=c(0,1), xaxt="n", yaxt="n", pch=16, col="red", xlab="Estimates of policy positions of party supporters (share support). Red = VAA; Blue = MRP-ed survey.\n Empty diamonds show the disaggregated survey means.", ylab="")
points(x=m$support2, y=seq(2,200,4), col="red", pch=16, cex=0.75)
points(x=m$mrppreddk, y=seq(3,200,4), col="blue", pch=16, cex=0.75)
#points(x=m$disaggregationdk, y=seq(2.5,200,4), col="darkgreen", pch=5, cex=0.75)
segments(x0=(m$support2-m$me2.50), x1=(m$support2+m$me2.50),y0=seq(2,200,4), col="red" )
segments(x0=(m$mrppreddkmin50), x1=(m$mrppreddkplus50),   y0=seq(3,200,4), col="blue" )
axis(1,at=seq(0, 1, 0.25), labels=seq(0, 1, 0.25))
axis(2,at=seq(2.5,200, 4), labels=m$index.name, cex.axis=.7, col.axis="grey28", las=1)
segments(x0=rep(0,50), x1=rep(1,50), y0=seq(0.3,201,4) , col="lightgrey", lwd=.5)
dev.off()

# 08b Figure A6.3, A6.4, A6.5 and A6.6: Dotplot figures per isssue and per party-------------------------------
png('./figures/FA6_3_dotplot_withopinion_issue_nl.png', width=3000, height=2000, res=300)
par(mfrow=c(2,3))
u<-unique(m$issue)
for(i in 1:length(u)){
  m.sub<-subset(m, m$issue==u[i])
  m.sub<-m.sub[order(m.sub$support1),]
  par(mar=c(3,5,3,1))
  plot(x=rep(-1,40), y=1:40, xlim=c(0,1), xaxt="n", yaxt="n", pch=16, col="red", xlab="", ylab="", main=m.sub$vaa_issue_name[1], cex.main=2)
  points(x=m.sub$support1, y=seq(2,40,4), col="red", pch=16, cex=1)
  points(x=m.sub$mrppred, y=seq(3,40,4), col="blue", pch=16, cex=1)
  #points(x=m.sub$disaggregation, y=seq(3,40,4), col="darkgreen", pch=5, cex=1)
  segments(x0=(m.sub$support1-m.sub$me1.50), x1=(m.sub$support1+m.sub$me1.50),y0=seq(2,40,4), col="red" )
  segments(x0=(m.sub$mrppredmin50), x1=(m.sub$mrppredplus50),   y0=seq(3,40,4), col="blue" )
  axis(1,at=seq(0, 1, 0.25), labels=seq(0, 1, 0.25), cex.axis = 1.5)
  axis(2,at=seq(2.5,40, 4), labels=m.sub$party, cex.axis=1.5, col.axis="black", las=1)
  segments(x0=rep(0,40), x1=rep(1,40), y0=seq(0.3,41,4) , col="lightgrey", lwd=.5)
}
dev.off()

png('./figures/FA6_4_dotplot_allrecords_issue_nl.png', width=3000, height=2000, res=300)
par(mfrow=c(2,3))
u<-unique(m$issue)
for(i in 1:length(u)){
  m.sub<-subset(m, m$issue==u[i])
  m.sub<-m.sub[order(m.sub$support2),]
  par(mar=c(3,5,3,1))
  plot(x=rep(-1,40), y=1:40, xlim=c(0,1), xaxt="n", yaxt="n", pch=16, col="red", ylab="", main=m.sub$vaa_issue_name[1], cex.main= 2)
  points(x=m.sub$support2, y=seq(2,40,4), col="red", pch=16, cex=1)
  points(x=m.sub$mrppreddk, y=seq(3,40,4), col="blue", pch=16, cex=1)
  segments(x0=(m.sub$support2-m.sub$me2.50), x1=(m.sub$support2+m.sub$me2.50),y0=seq(2,40,4), col="red" )
  segments(x0=(m.sub$mrppreddkmin50), x1=(m.sub$mrppreddkplus50),   y0=seq(3,40,4), col="blue" )
  axis(1,at=seq(0, 1, 0.25), labels=seq(0, 1, 0.25), cex.axis = 1.5)
  axis(2,at=seq(2.5,40, 4), labels=m.sub$party, cex.axis = 1.5, col.axis="black", las=1)
  segments(x0=rep(0,40), x1=rep(1,40), y0=seq(0.3,41,4) , col="lightgrey", lwd=.5)
}
dev.off()

png('./figures/FA6_5_dotplot_withopinion_party_nl.png', width=3000, height=3000, res=300)
par(mfrow=c(4,3))
u<-unique(m$party)
for(i in 1:length(u)){
  m.sub<-subset(m, m$party==u[i])
  m.sub<-m.sub[order(m.sub$support1),]
  par(mar=c(3,7,3,1))
  plot(x=rep(-1,20), y=1:20, xlim=c(0,1), xaxt="n", yaxt="n", pch=16, col="red", ylab="", main=m.sub$party[1], cex.main=2)
  points(x=m.sub$support1, y=seq(2,20,4), col="red", pch=16, cex=1)
  points(x=m.sub$mrppred, y=seq(3,20,4), col="blue", pch=16, cex=1)
  segments(x0=(m.sub$support1-m.sub$me1.50), x1=(m.sub$support1+m.sub$me1.50),y0=seq(2,20,4), col="red" )
  segments(x0=(m.sub$mrppredmin50), x1=(m.sub$mrppredplus50),   y0=seq(3,20,4), col="blue" )
  axis(1,at=seq(0, 1, 0.25), labels=seq(0, 1, 0.25), cex.axis = 1.2)
  axis(2,at=seq(2.5,20, 4), labels=m.sub$vaa_issue_name, cex.axis = 1.2, col.axis="black", las=1)
  segments(x0=rep(0,20), x1=rep(1,24), y0=seq(0.3,21,4) , col="lightgrey", lwd=.5)
}
dev.off()

png('./figures/FA6_6_dotplot_allrecords_issue_nl.png', width=3000, height=3000, res=300)
par(mfrow=c(4,3))
u<-unique(m$party)
for(i in 1:length(u)){
  m.sub<-subset(m, m$party==u[i])
  m.sub<-m.sub[order(m.sub$support2),]
  par(mar=c(3,7,3,1))
  plot(x=rep(-1,20), y=1:20, xlim=c(0,1), xaxt="n", yaxt="n", pch=16, col="red", ylab="", main=m.sub$party[1], cex.main=2)
  points(x=m.sub$support2, y=seq(2,20,4), col="red", pch=16, cex=1)
  points(x=m.sub$mrppreddk, y=seq(3,20,4), col="blue", pch=16, cex=1)
  segments(x0=(m.sub$support2-m.sub$me2.50), x1=(m.sub$support2+m.sub$me2.50),y0=seq(2,20,4), col="red" )
  segments(x0=(m.sub$mrppreddkmin50), x1=(m.sub$mrppreddkplus50),   y0=seq(3,20,4), col="blue" )
  axis(1,at=seq(0, 1, 0.25), labels=seq(0, 1, 0.25), cex.axis = 1.2)
  axis(2,at=seq(2.5,20, 4), labels=m.sub$vaa_issue_name, cex.axis=1.2, col.axis="black", las=1)
  segments(x0=rep(0,20), x1=rep(1,24), y0=seq(0.3,21,4) , col="lightgrey", lwd=.5)
}
dev.off()
# 09b Analysis NL based on weighted data ----------------------------------
# First load the demographic distribution of party supporters from the LISS data
liss<-read.csv(file='./data/liss_props.csv', header=TRUE, sep=',')
row.names(liss) <- liss[,1]
liss<-liss[,2:11]

# Note: LISS data can be obtained from https://www.dataarchive.lissdata.nl/ after registration 
# The code below shows how the demographic proportions of party supporters are computed
# liss_pol <- haven::read_sav('./data/liss/cv12e_EN_10p.sav')
# liss_gen <- haven::read_sav('./data/liss/avars_201112_EN_20p.sav')
# liss <- left_join (liss_pol, liss_gen, by='nomem_encr')
# liss <- liss %>%
#   filter (leeftijd > 17) %>%
#   mutate (age = leeftijd,
#           age_cat = lftdcat,
#           age_cat3 = ifelse(age<35, 'young', ifelse(age<65, 'midage', 'old')),
#           edu = dplyr::recode (as.character(oplzon), '1'='1.lower','2'='2.midlow','3'='2.midlow','4'='3.midhigh', '5'='3.midhigh', '6'='4.high', .default=NA_character_),
#           edu_cat3 = ifelse(edu=='4.high', 'high_edu', ifelse(edu=='3.midhigh', 'mid_edu', 'low_edu')),
#           sex = ifelse(geslacht==2, 'woman', 'man'),
#           vote2010 = dplyr::recode(as.character(cv12e169), '1' = 'VVD', '2' ='PvdA', '3'='PVV', '4'='CDA', '5'='SP', '6'='D66','7'='GL', '8'='CU', '9'='SGP', '10'='PvdD', .default = NA_character_),
#           index = paste(sex, age_cat3, edu_cat3, sep='.'),
#           pindex = paste(vote2010, sex, age_cat3, edu_cat3, sep='.')
#   ) %>%
#   select (age, age_cat, age_cat3, edu, edu_cat3, sex, vote2010, index) %>%
#   filter (is.na(edu_cat3)==F)
# liss_props<-round(prop.table(table(liss$index, liss$vote2010), 2), 2)
# write.csv(liss_props, file='./data/liss_props.csv')

# filter the main data frame to exclude NAs
tkn <- tk %>%
  filter (vote2010=="PvdA" | vote2010=="CDA" | vote2010=="CU"  | 
            vote2010=="D66"  | vote2010=="SP"  | vote2010=="GL"  |
            vote2010=="SGP"  | vote2010=="PvdD"| vote2010=="PVV" | vote2010=="VVD") %>%
  filter (is.na(age_cat3)==F,is.na(edu_cat3)==F, is.na(sex)==F)

# get the table of proportions from the VAA data
vaa_props <- round(prop.table(table(tkn$index, tkn$vote2010), 2), 2)

# now divide the LISS by the VAA to get the weights
w <- as.table(as.matrix(liss / vaa_props))

# adjust the data frame
w <- melt(data.frame(w)) %>%
  mutate (pindex = paste(Var2, Var1, sep='.')) %>%
  mutate_if(is.numeric, list(~na_if(., Inf))) %>%
  select (pindex, value)

# attach back to data
tkn <- left_join (tkn, w, by='pindex')

# compute weighted support
out <- data.table(matrix(nrow=10, ncol=6, NA))
names(out) <- c('vote2010','issue4','issue7','issue17','issue20','issue27')
out$vote2010 <- unique(tkn$vote2010)

tkn <- as.data.table(tkn)

for (i in 1:10){
  tt<-subset (tkn, tkn$vote2010==unique(tkn$vote2010)[i]) 
  out$issue4[i] <- (sum(tt[issue4==4, 'value'], na.rm=TRUE) + sum(tt[tt$issue4==5, 'value'], na.rm=TRUE)) /
    (sum(tt[is.na(tt$issue4)!=T, 'value'], na.rm=TRUE) - sum(tt[tt$issue4==3, 'value'], na.rm=TRUE))
  out$issue7[i] <- (sum(tt[issue7==4, 'value'], na.rm=TRUE) + sum(tt[tt$issue7==5, 'value'], na.rm=TRUE)) /
    (sum(tt[is.na(tt$issue7)!=T, 'value'], na.rm=TRUE) - sum(tt[tt$issue7==3, 'value'], na.rm=TRUE))
  out$issue17[i] <- (sum(tt[issue17==4, 'value'], na.rm=TRUE) + sum(tt[tt$issue17==5, 'value'], na.rm=TRUE)) /
    (sum(tt[is.na(tt$issue17)!=T, 'value'], na.rm=TRUE) - sum(tt[tt$issue17==3, 'value'], na.rm=TRUE))
  out$issue20[i] <- (sum(tt[issue20==4, 'value'], na.rm=TRUE) + sum(tt[tt$issue20==5, 'value'], na.rm=TRUE)) /
    (sum(tt[is.na(tt$issue20)!=T, 'value'], na.rm=TRUE) - sum(tt[tt$issue20==3, 'value'], na.rm=TRUE))
  out$issue27[i] <- (sum(tt[issue27==4, 'value'], na.rm=TRUE) + sum(tt[tt$issue27==5, 'value'], na.rm=TRUE)) /
    (sum(tt[is.na(tt$issue27)!=T, 'value'], na.rm=TRUE) - sum(tt[tt$issue27==3, 'value'], na.rm=TRUE))
}

out2 <- data.table(matrix(nrow=10, ncol=6, NA))
names(out2) <- c('vote2010','issue4','issue7','issue17','issue20','issue27')
out2$vote2010 <- unique(tkn$vote2010)

for (i in 1:10){
  tt<-subset (tkn, tkn$vote2010==unique(tkn$vote2010)[i]) 
  out2$issue4[i] <- (sum(tt[issue4==4, 'value'], na.rm=TRUE) + sum(tt[tt$issue4==5, 'value'], na.rm=TRUE)) /
    sum(tt[is.na(tt$issue4)!=T, 'value'], na.rm=TRUE)
  out2$issue7[i] <- (sum(tt[issue7==4, 'value'], na.rm=TRUE) + sum(tt[tt$issue7==5, 'value'], na.rm=TRUE)) /
    sum(tt[is.na(tt$issue4)!=T, 'value'], na.rm=TRUE)
  out2$issue17[i] <- (sum(tt[issue17==4, 'value'], na.rm=TRUE) + sum(tt[tt$issue17==5, 'value'], na.rm=TRUE)) /
    sum(tt[is.na(tt$issue4)!=T, 'value'], na.rm=TRUE)
  out2$issue20[i] <- (sum(tt[issue20==4, 'value'], na.rm=TRUE) + sum(tt[tt$issue20==5, 'value'], na.rm=TRUE)) /
    sum(tt[is.na(tt$issue4)!=T, 'value'], na.rm=TRUE)
  out2$issue27[i] <- (sum(tt[issue27==4, 'value'], na.rm=TRUE) + sum(tt[tt$issue27==5, 'value'], na.rm=TRUE)) /
    sum(tt[is.na(tt$issue4)!=T, 'value'], na.rm=TRUE)
}

m1<-melt(out, "vote2010") # melt the means matrix
m1$index<-paste(m1$vote2010, m1$variable, sep=".") # create an index variable
colnames(m1)<-c("party","issue","support1","index") # rename the columns

m2<-melt(out2, "vote2010") # melt the means matrix
m2$index<-paste(m2$vote2010, m2$variable, sep=".") # create an index variable
colnames(m2)<-c("party","issue","support2","index") # rename the columns

mw<-merge(m1,m2[,3:4], by="index") # merge the two

# Merge everything together
mw<-merge(mw, surveys.nl, by="index") # merge with the survey data 

# Compute correlations and concordance coef-s
cor(mw$support1, mw$mrppred) # Weighted VAA support from those with opinion (excl. neutral) and MRP estimates
cor(mw$support2, mw$mrppreddk) # Weighted VAA support from all and MRP estimates

cor(mw$support1, mw$disaggregation) # Weighted VAA support from those with opinion (excl. neutral) and disaggregation
cor(mw$support2, mw$disaggregationdk)  # Weighted VAA support from all and disaggregation

epi.ccc(mw$support1, mw$mrppred, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est # Weighted VAA support from those with opinion (excl. neutral) and MRP estimates
epi.ccc(mw$support2, mw$mrppreddk, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est # Weighted VAA support from all and MRP estimates

epi.ccc(mw$support1, mw$disaggregation, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est # Weighted VAA support from those with opinion (excl. neutral) and disaggregation
epi.ccc(mw$support2, mw$disaggregationdk, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est # Weighted VAA support from all and disaggregation

# Correlations and MAR per issue 
out1 <- mw %>%
  group_by ( vaa_issue_name ) %>%
  summarize ( cor = round (cor (support1, mrppred), 2),
              lcc = round (epi.ccc(support1, mrppred, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est, 2),
              mae = round (mean (abs(support1 - mrppred)),2)*100,
              
              cor.dk = round (cor (support2, mrppreddk), 2),
              lcc.dk = round (epi.ccc(support2, mrppreddk, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est, 2),
              mae.dk = round (mean (abs(support2 - mrppreddk)),2)*100
  )

out2 <- mw %>%
  group_by ( partyname ) %>%
  summarize ( cor = round (cor (support1, mrppred), 2),
              lcc = round (epi.ccc(support1, mrppred, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est, 2),
              mae = round (mean (abs(support1 - mrppred)),2)*100,
              
              cor.dk = round (cor (support2, mrppreddk), 2),
              lcc.dk = round (epi.ccc(support2, mrppreddk, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est, 2),
              mae.dk = round (mean (abs(support2 - mrppreddk)),2)*100
  )
colnames(out2)[1] <- colnames(out1)[1]  
out12w = bind_rows(out1, out2)

html.dv.table = htmlTable::htmlTable(out12w, rnames = FALSE,
                                     col.columns = c('white','lightgrey'),
                                     tfoot = 'Note: Mean absolute errors are in percentage points.',
                                     align = c('l','r','r','r','r', 'r', 'r'),
                                     align.header = c('l','c','c','c','c', 'c', 'c'),
                                     header = c('Policy issue / Party group','Correlation','LCC','Mean abs. error','Correlation (with DK)','LCC (with DK)','Mean abs. error (with DK)'),
                                     caption = "Table A6. Comparing modelled sets of WEIGHTED party support estimates per policy issue and per party group in The Netherlands.")

print(html.dv.table, type = "html", file = './tables/Table_A6_nl_cor_mae_weights.html')



# Where are the improvements form the weighting procedure? 
out12w$eval.cor = out12w$cor - out12$cor
out12w$eval.lcc = out12w$lcc - out12$lcc
out12w$eval.mae = out12w$mae - out12$mae

out12w

# 10b Figure 2 NL (but weighted VAA data): All pairs in one figure (Part 9 of Appendix) -------------------------------
palette(brewer.pal(n = 5, name = "Dark2"))
png('./figures/FA9_1_corr_nl_weights.png', width=3000, height=2000, res=300) #turn off the lables for png
par(mfrow=(c(1,2)))
par(mar=c(5,5,4,1))
plot(mw$support1, mw$mrppred, col=mw$issue, pch=16, cex=2, ylim=c(0,1), xlim=c(0,1), cex.lab=1.5,
     main="Share of support (from all with opinion)", xlab = 'Weighted VAA estimates', ylab="Share of support: MRP-ed survey estimates")
text(paste0("Correlation = ",round(cor(mw$support1, mw$mrppred),2)), x=0.8, y= 0.01)
lines(abline(0,1, lwd=2))
lines(abline(lm(mrppred~support1, data=mw), col="red", lwd=2))

plot(mw$support2, mw$mrppreddk, col=mw$issue, pch=16, cex=2, ylim=c(0,1), xlim=c(0,1), cex.lab=1.5, 
     main="Share of support (from all recrods)", xlab = 'Weighted VAA estimates', ylab="Share of support: MRP-ed survey estimates")
text(paste0("Correlation = ",round(cor(mw$support2, mw$mrppreddk),2)), x=0.8, y= 0.01)
lines(abline(0,1, lwd=2))
lines(abline(lm(mrppreddk~support2, data=mw), col="red", lwd=2))
dev.off()

# 11b Analysis NL based on strong supporters ----------------------------------
  
### For the analysis of strong supporters, the data needs to be filtered first
### Filter by type of supporter
tk2 <- tk %>% 
  filter (supporter == 1) %>% # filter strong supporters only
  select (vote2010, starts_with('issue')) 

## This block below aggregates the data at the party level
party.nas    <-ddply(tk2,.(vote2010) , numcolwise(.fun = function(x) sum(is.na(x)))) #calculate party-issue level NAs
party.neutral<-ddply(tk2,.(vote2010) , numcolwise(.fun = function(x) sum.na(x==3))) # calculate party-issue level neutral responses
party.minus2 <-ddply(tk2,.(vote2010) , numcolwise(.fun = function(x) sum.na(x==1))) # calculate party-issue level very neg responses
party.minus1 <-ddply(tk2,.(vote2010) , numcolwise(.fun = function(x) sum.na(x==2))) # calculate party-issue level neg responses
party.plus1  <-ddply(tk2,.(vote2010) , numcolwise(.fun = function(x) sum.na(x==4))) # calculate party-issue level pos responses
party.plus2  <-ddply(tk2,.(vote2010) , numcolwise(.fun = function(x) sum.na(x==5))) # calculate party-issue level very pos responses
party.all    <-ddply(tk2,.(vote2010) , numcolwise (.fun = function(x) sum.na(is.na(x)==F))) #calculate party-issue level non NA responses

party.support1<-(party.plus1[-1]+party.plus2[-1])/(party.all[-1]-party.neutral[-1]) #support from all with opinion
party.support2<-(party.plus1[-1]+party.plus2[-1])/(party.all[-1]+party.nas[-1]) #support from all, including NAs

party.support1$vote2010<-party.all$vote2010
party.support2$vote2010<-party.all$vote2010

party.me1.50<-party.all #calculate margins of error; start with a copied table just to get the labels
party.me2.50<-party.all #calculate margins of error; start with a copied table just to get the labels

party.me1.50[,-1]<-qnorm(.75)*(sqrt(party.support1[,-6]*(1-party.support1[,-6])/(party.all[,-1]-party.neutral[,-1])))
party.me2.50[,-1]<-qnorm(.75)*(sqrt(party.support2[,-6]*(1-party.support2[,-6])/(party.all[,-1]+party.nas[,-1])))

m1<-melt(party.support1, "vote2010") #melt the means matrix
m1$index<-paste(m1$vote2010, m1$variable, sep=".") #create an index variable
colnames(m1)<-c("party","issue","support1","index") #rename the columns

m2<-melt(party.support2, "vote2010") #melt the means matrix
m2$index<-paste(m2$vote2010, m2$variable, sep=".") #create an index variable
colnames(m2)<-c("party","issue","support2","index") #rename the columns

m3<-melt(party.me1.50, "vote2010") #melt the means matrix
m3$index<-paste(m3$vote2010, m3$variable, sep=".") #create an index variable
colnames(m3)<-c("party","issue","me1.50","index") #rename the columns

m4<-melt(party.me2.50, "vote2010") #melt the means matrix
m4$index<-paste(m4$vote2010, m4$variable, sep=".") #create an index variable
colnames(m4)<-c("party","issue","me2.50","index") #rename the columns

mstrong<-merge(m1,m2[,3:4], by="index")
mstrong<-merge(mstrong,m3[,3:4], by="index")
mstrong<-merge(mstrong,m4[,3:4], by="index")

### Merge everything together
mstrong<-merge(mstrong, surveys.nl, by="index")

### Correlations
cor(mstrong$support1, mstrong$mrppred) 
cor(mstrong$support2, mstrong$mrppreddk) 
cor(mstrong$support1, mstrong$disaggregation) 
cor(mstrong$support2, mstrong$disaggregationdk) 

epi.ccc(mstrong$support1, mstrong$mrppred, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est 
epi.ccc(mstrong$support2, mstrong$mrppreddk, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est 
epi.ccc(mstrong$support1, mstrong$disaggregation, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est 
epi.ccc(mstrong$support2, mstrong$disaggregationdk, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE)$rho$est
# 12b Figure 2 (but strong supporters): All pairs in one figure (Part 7.2 of the Appendix) -------------------------------
palette(brewer.pal(n = 5, name = "Dark2"))
png('./figures/FA7_2_corr_nl_strong.png', width=3000, height=2000, res=300) #turn off the lables for png
par(mfrow=(c(1,2)))
par(mar=c(5,5,4,1))
plot(mstrong$support1, mstrong$mrppred, col=mstrong$issue, pch=16, cex=2, ylim=c(0,1), xlim=c(0,1), cex.lab=1.5,
     main="Share of support\n(from all strong supporters with opinion)", xlab = 'VAA estimates', ylab="Share of support: MRP-ed survey estimates")
text(paste0("Correlation = ",round(cor(mstrong$support1, mstrong$mrppred),2)), x=0.8, y= 0.01)
lines(abline(0,1, lwd=2))
lines(abline(lm(mrppred~support1, data=mstrong), col="red", lwd=2))

plot(mstrong$support2, mstrong$mrppreddk, col=mstrong$issue, pch=16, cex=2, ylim=c(0,1), xlim=c(0,1), cex.lab=1.5, 
     main="Share of support\n(from all recrods of strong supporters)", xlab = 'VAA estimates', ylab="Share of support: MRP-ed survey estimates")
text(paste0("Correlation = ",round(cor(mstrong$support2, mstrong$mrppreddk),2)), x=0.8, y= 0.01)
lines(abline(0,1, lwd=2))
lines(abline(lm(mrppreddk~support2, data=mstrong), col="red", lwd=2))
dev.off()



# THE END -----------------------------------------------------------------
writeLines(capture.output(sessionInfo()), "sessionInfo.txt")




