###################################################
###################################################
###################################################
###################################################
###################################################
###################################################
##### OBSERVED MOVING BEHAVIOR OF SURVEY RESPONDENTS
###################################################
###################################################
###################################################
###################################################
###################################################
###################################################


cat("Begin OBSERVED MOVING BEHAVIOR OF SURVEY RESPONDENTS\n")


load(file="dd.final.Rdata")
dd<-dd.final
dd<-dd[dd$satislife!="",] 


load("zip_apsa_imputed22.Rdata")


dd$knowlastzip<-dd$lastzip
dd$lastzip<-dd$lastzip.text
table(nchar(dd$lastzip.text))

dd$zip5yrs<-dd$lastzip.text
dd$zip5yrs[dd$zip==""]<-NA

table(nchar(dd$zipnew))
table(nchar(dd$zip5yrs))
table(nchar(zip$zipnew))

##get mccain support in current zip
dim(dd)
dd<-merge(dd, zip[,c("zipnew","pctr082")], by="zipnew",all.x=T)
dim(dd)

zip$pctr082.prev<-zip$pctr082

##get mccain support in previous zip
dim(dd)
dd<-merge(dd, zip[,c("zipnew","pctr082.prev")], by.x="zip5yrs",by.y="zipnew",all.x=T)
dim(dd)

head(dd)


##Code relevant zip code variables
dd$pctd08zipnow<-1-dd$pctr082
dd$pctd08zip5yr<-1-dd$pctr082.prev
dd$pctr08zipchg<-dd$pctd08zipnow-dd$pctd08zip5yr


##Estimation


codestr<-c("", "& dmt$white==\'1\'", "& dmt$single==0", "& dmt$single==1", "& dmt$age<=35", "& dmt$age>=35 & dmt$age<=65","& dmt$age>65", "& dmt$income%in%c(\'1.30kless\',\'2.30to39k\')", 
"& dmt$income%in% c(\'7.80to89k\', \'8.90to99k\', \'9.100to119k\', \'10.120kplus\')", "& (dmt$libdem==1|dmt$conrep==1)", "& dmt$partystrength==1", "& dmt$kidshome==1", "&dmt$thrm.urbanites>=75", "&dmt$thrm.rurals>=75","&dmt$same.msa==0","&dmt$same.msa==1")


filestr<-c("", ".white", ".age18to35", ".age35to60", ".incomeunder40k", ".incomeover80k", ".sortedpartisan", ".haskids", ".prourbanite", ".prorurals")
titlestr<-c("All", "Whites Only", "Not Single","Single", "Respondents Age 18-35", "Respondents Age 35-65", "Respondents Age Over 65", "Income Under $40,000", "Income $80,000 and Up", 
"Ideological Partisans", "Strong Partisans", "Children at Home", "Urban People Therm >75", "Rural People Therm >75","Moved Outside MSA","Moved Within MSA")

## Save average sorting for each group.
d.avgnow<-d.avg5yr<-r.avgnow<-r.avg5yr<-rep(NA, length(codestr))
d.avgchg<-r.avgchg<-rep(NA, length(codestr))
d.cilo<-d.cihi<-r.cilo<-r.cihi<-rep(NA, length(codestr))
d.cilo.now<-d.cihi.now<-r.cilo.now<-r.cihi.now<-rep(NA, length(codestr))
d.cilo.5yr<-d.cihi.5yr<-r.cilo.5yr<-r.cihi.5yr<-rep(NA, length(codestr))

dm<-dd[dd$zip5yr!=dd$zip&!is.na(dd$zip5yr),]
dm.dem<-dm[dm$party=="dem",]
dm.rep<-dm[dm$party=="rep",]

dm$partystrength<-as.numeric(dm$partystrengthd=="strong"|dm$partystrengthr=="strong")
table(dm$partystrength)


for(i in 1:length(codestr)){
  dmt<-dm
  dmt$pctd08zipchg<-dmt$pctd08zipnow-dmt$pctd08zip5yr
  dmt.dem<-eval(parse(text=paste("dmt[dmt$party==\'dem\'", codestr[i], ",]", sep="")))
  dmt.rep<-eval(parse(text=paste("dmt[dmt$party==\'rep\'", codestr[i], ",]", sep="")))  
  d.avgnow[i]<-mean(dmt.dem$pctd08zipnow, na.rm=T)
  r.avgnow[i]<-mean(dmt.rep$pctd08zipnow, na.rm=T)
  d.avg5yr[i]<-mean(dmt.dem$pctd08zip5yr, na.rm=T)
  r.avg5yr[i]<-mean(dmt.rep$pctd08zip5yr, na.rm=T)
  d.avgchg[i]<-mean(dmt.dem$pctd08zipchg, na.rm=T)
  r.avgchg[i]<-mean(dmt.rep$pctd08zipchg, na.rm=T)                                                                                          
  d.cilo[i]<-t.test(dmt.dem$pctd08zipchg)$conf.int[1]
  d.cihi[i]<-t.test(dmt.dem$pctd08zipchg)$conf.int[2]
  r.cilo[i]<-t.test(dmt.rep$pctd08zipchg)$conf.int[1]
  r.cihi[i]<-t.test(dmt.rep$pctd08zipchg)$conf.int[2]
  d.cilo.now[i]<-t.test(dmt.dem$pctd08zipnow)$conf.int[1]
  d.cihi.now[i]<-t.test(dmt.dem$pctd08zipnow)$conf.int[2]
  r.cilo.now[i]<-t.test(dmt.rep$pctd08zipnow)$conf.int[1]
  r.cihi.now[i]<-t.test(dmt.rep$pctd08zipnow)$conf.int[2]
  d.cilo.5yr[i]<-t.test(dmt.dem$pctd08zip5yr)$conf.int[1]
  d.cihi.5yr[i]<-t.test(dmt.dem$pctd08zip5yr)$conf.int[2]
  r.cilo.5yr[i]<-t.test(dmt.rep$pctd08zip5yr)$conf.int[1]
  r.cihi.5yr[i]<-t.test(dmt.rep$pctd08zip5yr)$conf.int[2]
}


# Mean change in partisan context, by % Dem, % Republican
grps<-data.frame(grp=codestr, d.chg=d.avgchg, d.cilo=d.cilo, d.cihi=d.cihi, 
r.chg=r.avgchg, r.cilo=r.cilo, r.cihi=r.cihi, d.avgnow=d.avgnow, d.cilo.now=d.cilo.now, d.cihi.now=d.cihi.now, 
r.avgnow=r.avgnow, r.cilo.now=r.cilo.now, r.cihi.now=r.cihi.now, d.avg5yr=d.avgnow, d.cilo.5yr=d.cilo.5yr, d.cihi.5yr=d.cihi.5yr, 
r.avg5yr=r.avgnow, r.cilo.5yr=r.cilo.5yr, r.cihi.5yr=r.cihi.5yr, 
title=titlestr)
grps$diffr<-grps$d.chg-grps$r.chg 
grps$rownum<-seq(nrow(grps), 1)    

## Plotting the change in zip code % Democratic among movers.


##GENERATE FIGURE 6 in manuscript

pdf(paste(output,"movingbehavior.pdf",sep=""), paper='special', height=6, width=6)
par(mar=c(5,12,3,1))
plot(grps$rownum-0.1~grps$d.chg, xlab = "Change in Zip Code Proportion Democratic \n (Among Movers)", ylab = "", main = "Americans Are Not Sorting and \nMost Partisan Subgroups are Mixing",
yaxt="n", las=1, xlim=c(min(grps[,2:7]), max(grps[,2:7])), pch="", col="blue", ylim=c(0.5,max(grps$rownum)+0.7)) 
abline(v=0,lty=2, col="grey")
points(grps$rownum-0.1~grps$d.chg, pch=17, col="blue")
segments(grps$d.cilo, grps$rownum-0.1, grps$d.cihi, grps$rownum-0.1, lwd =.6, col="blue")
points(grps$rownum+0.1~grps$r.chg, pch=19, col="red")
segments(grps$r.cilo, grps$rownum+0.1, grps$r.cihi, grps$rownum+0.1, lwd =.6, col="red")
axis(2, at = grps$rownum, labels = grps$title, las = 1, tick = T, mgp = c(2,1,0), mar=c(2, 12, 2, 2), tck=0, cex.axis = 1.1) 
legend("bottomright", legend=c("Republicans","Democrats"), cex=.7, col=c("red","blue"), pch=c(19,17))
dev.off()





##repeat including those who moved within zipcode

rm(list=ls()[ls()!="wd"&ls()!="output"])
## Libraries
library(car)
library(fossil)
library(maptools)
library(sp)
library(foreign)
library(zipcode)
library(ggplot2)
library(reshape2)
library(plotrix)
library(gplots)
library(xtable)



load(file=("dd.final.Rdata"))
dd<-dd.final
dd<-dd[dd$satislife!="",] 


load("zip_apsa_imputed22.Rdata")


dd$knowlastzip<-dd$lastzip
dd$lastzip<-dd$lastzip.text
table(nchar(dd$lastzip.text))

dd$zip5yrs<-dd$lastzip.text
dd$zip5yrs[dd$zip==""]<-NA

table(nchar(dd$zipnew))
table(nchar(dd$zip5yrs))
table(nchar(zip$zipnew))

##get mccain support in current zip
dim(dd)
dd<-merge(dd, zip[,c("zipnew","pctr082")], by="zipnew",all.x=T)
dim(dd)

zip$pctr082.prev<-zip$pctr082

##get mccain support in previous zip
dim(dd)
dd<-merge(dd, zip[,c("zipnew","pctr082.prev")], by.x="zip5yrs",by.y="zipnew",all.x=T)
dim(dd)

head(dd)


##Code relevant zip code variables
dd$pctd08zipnow<-1-dd$pctr082
dd$pctd08zip5yr<-1-dd$pctr082.prev
dd$pctr08zipchg<-dd$pctd08zipnow-dd$pctd08zip5yr


##Estimation


codestr<-c("", "& dmt$white==\'1\'", "& dmt$single==0", "& dmt$single==1", "& dmt$age<=35", "& dmt$age>=35 & dmt$age<=65","& dmt$age>65", "& dmt$income%in%c(\'1.30kless\',\'2.30to39k\')", 
"& dmt$income%in% c(\'7.80to89k\', \'8.90to99k\', \'9.100to119k\', \'10.120kplus\')", "& (dmt$libdem==1|dmt$conrep==1)", "& dmt$partystrength==1", "& dmt$kidshome==1", "&dmt$thrm.urbanites>=75", "&dmt$thrm.rurals>=75","&dmt$same.msa==0","&dmt$same.msa==1")


filestr<-c("", ".white", ".age18to35", ".age35to60", ".incomeunder40k", ".incomeover80k", ".sortedpartisan", ".haskids", ".prourbanite", ".prorurals")
titlestr<-c("All", "Whites Only", "Not Single","Single", "Respondents Age 18-35", "Respondents Age 35-65", "Respondents Age Over 65", "Income Under $40,000", "Income $80,000 and Up", 
"Ideological Partisans", "Strong Partisans", "Children at Home", "Urban People Therm >75", "Rural People Therm >75","Moved Outside MSA","Moved Within MSA")

## Save average sorting for each group.
d.avgnow<-d.avg5yr<-r.avgnow<-r.avg5yr<-rep(NA, length(codestr))
d.avgchg<-r.avgchg<-rep(NA, length(codestr))
d.cilo<-d.cihi<-r.cilo<-r.cihi<-rep(NA, length(codestr))
d.cilo.now<-d.cihi.now<-r.cilo.now<-r.cihi.now<-rep(NA, length(codestr))
d.cilo.5yr<-d.cihi.5yr<-r.cilo.5yr<-r.cihi.5yr<-rep(NA, length(codestr))

dm<-dd[!is.na(dd$zip5yr),]
dm.dem<-dm[dm$party=="dem",]
dm.rep<-dm[dm$party=="rep",]

dm$partystrength<-as.numeric(dm$partystrengthd=="strong"|dm$partystrengthr=="strong")
table(dm$partystrength)


for(i in 1:length(codestr)){
  dmt<-dm
  dmt$pctd08zipchg<-dmt$pctd08zipnow-dmt$pctd08zip5yr
  dmt.dem<-eval(parse(text=paste("dmt[dmt$party==\'dem\'", codestr[i], ",]", sep="")))
  dmt.rep<-eval(parse(text=paste("dmt[dmt$party==\'rep\'", codestr[i], ",]", sep="")))  
  d.avgnow[i]<-mean(dmt.dem$pctd08zipnow, na.rm=T)
  r.avgnow[i]<-mean(dmt.rep$pctd08zipnow, na.rm=T)
  d.avg5yr[i]<-mean(dmt.dem$pctd08zip5yr, na.rm=T)
  r.avg5yr[i]<-mean(dmt.rep$pctd08zip5yr, na.rm=T)
  d.avgchg[i]<-mean(dmt.dem$pctd08zipchg, na.rm=T)
  r.avgchg[i]<-mean(dmt.rep$pctd08zipchg, na.rm=T)                                                                                          
  d.cilo[i]<-t.test(dmt.dem$pctd08zipchg)$conf.int[1]
  d.cihi[i]<-t.test(dmt.dem$pctd08zipchg)$conf.int[2]
  r.cilo[i]<-t.test(dmt.rep$pctd08zipchg)$conf.int[1]
  r.cihi[i]<-t.test(dmt.rep$pctd08zipchg)$conf.int[2]
  d.cilo.now[i]<-t.test(dmt.dem$pctd08zipnow)$conf.int[1]
  d.cihi.now[i]<-t.test(dmt.dem$pctd08zipnow)$conf.int[2]
  r.cilo.now[i]<-t.test(dmt.rep$pctd08zipnow)$conf.int[1]
  r.cihi.now[i]<-t.test(dmt.rep$pctd08zipnow)$conf.int[2]
  d.cilo.5yr[i]<-t.test(dmt.dem$pctd08zip5yr)$conf.int[1]
  d.cihi.5yr[i]<-t.test(dmt.dem$pctd08zip5yr)$conf.int[2]
  r.cilo.5yr[i]<-t.test(dmt.rep$pctd08zip5yr)$conf.int[1]
  r.cihi.5yr[i]<-t.test(dmt.rep$pctd08zip5yr)$conf.int[2]
}


# Mean change in partisan context, by % Dem, % Republican
grps<-data.frame(grp=codestr, d.chg=d.avgchg, d.cilo=d.cilo, d.cihi=d.cihi, 
r.chg=r.avgchg, r.cilo=r.cilo, r.cihi=r.cihi, d.avgnow=d.avgnow, d.cilo.now=d.cilo.now, d.cihi.now=d.cihi.now, 
r.avgnow=r.avgnow, r.cilo.now=r.cilo.now, r.cihi.now=r.cihi.now, d.avg5yr=d.avgnow, d.cilo.5yr=d.cilo.5yr, d.cihi.5yr=d.cihi.5yr, 
r.avg5yr=r.avgnow, r.cilo.5yr=r.cilo.5yr, r.cihi.5yr=r.cihi.5yr, 
title=titlestr)
grps$diffr<-grps$d.chg-grps$r.chg 
grps$rownum<-seq(nrow(grps), 1)    




pdf(paste(output,"movingbehavior_samezip.pdf",sep=""), paper='special', height=6, width=7)
par(mar=c(5,12,3,1))
plot(grps$rownum-0.1~grps$d.chg, xlab = "Change in Zip Code Proportion Democratic \n (Among Movers)", ylab = "", main = "Americans Are Not Sorting and \nMost Partisan Subgroups are Mixing",
yaxt="n", las=1, xlim=c(min(grps[,2:7]), max(grps[,2:7])), pch="", col="blue", ylim=c(0.5,max(grps$rownum)+0.7)) 
abline(v=0,lty=2, col="grey")
points(grps$rownum-0.1~grps$d.chg, pch=17, col="blue")
segments(grps$d.cilo, grps$rownum-0.1, grps$d.cihi, grps$rownum-0.1, lwd =.6, col="blue")
points(grps$rownum+0.1~grps$r.chg, pch=19, col="red")
segments(grps$r.cilo, grps$rownum+0.1, grps$r.cihi, grps$rownum+0.1, lwd =.6, col="red")
axis(2, at = grps$rownum, labels = grps$title, las = 1, tick = T, mgp = c(2,1,0), mar=c(2, 12, 2, 2), tck=0, cex.axis = 1.1) 
legend("bottomright", legend=c("Republicans","Democrats"), cex=.7, col=c("red","blue"), pch=c(19,17))
dev.off()



cat("End OBSERVED MOVING BEHAVIOR OF SURVEY RESPONDENTS\n")

