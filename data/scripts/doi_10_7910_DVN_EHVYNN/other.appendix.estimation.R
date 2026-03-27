

#######################################
####SCREEN ANALYSIS
#####
#######################################

cat("Begin Other Appendix Materials Analysis\n")


load("dd.final.Rdata")
dd<-dd.final
head(dd)
#####
#### Run balance tests based on screen items

table(dd$post.screen1)##multiple choice item correct=1; else 0
table(dd$post.screen2.new)##those who enetred at least one character =1; else 0

##balance vars

balance.vars<-cbind.data.frame("Age"=dd$age,  "Income"=dd$income.num,  "Education"=dd$educ2, "Female"=dd$female, "Democrat"=dd$democrat,"Ideology"=dd$ideol, "Interest in Politics"=dd$likepol, "Native English Speaker"=dd$nativeeng, "White"=dd$white2, "Black"=dd$black2, "Hispanic"=dd$hisp2, "Homeowner"=dd$ownrent2,  "Moved in Last 5 years"=dd$homelast5,"Has Kids"=dd$kidshome, "City Resident"=dd$city, "Daily Commute Time"=dd$commuteminwork, "Total Survey Time"=dd$time, post.screen1=dd$post.screen1, post.screen2=dd$post.screen2.new, partywlean=dd$partywlean)

head(balance.vars)
dim(balance.vars)


for (i in seq(1,ncol(balance.vars)-1, by=1)){
	balance.vars[,i]<-as.numeric(as.character(balance.vars[,i]))
	}


head(balance.vars)


##########
##########
##DESCRIPTIVE STATS
##########
##########
##########



balance.vars.party<-subset(balance.vars, balance.vars$partywlean!="ind" & balance.vars$partywlean!="oth" & !is.na(balance.vars$partywlean))

balance.vars.screened<-subset(balance.vars, balance.vars$post.screen1==1&post.screen2==1)

balance.vars.unscreened<-subset(balance.vars, balance.vars$post.screen1!=1|post.screen2!=1)

balance.vars.screened.party<-subset(balance.vars, balance.vars$post.screen1==1&post.screen2==1 & 
balance.vars$partywlean!="ind" & balance.vars$partywlean!="oth" & !is.na(balance.vars$partywlean) )

balance.vars.unscreened.party<-subset(balance.vars.unscreened, balance.vars.unscreened$partywlean!="ind" & balance.vars.unscreened$partywlean!="oth" & !is.na(balance.vars.unscreened$partywlean) )

dim(balance.vars)##full data
dim(balance.vars.party)##partisan sample
dim(balance.vars.screened.party)##partisan sample correct on screens
dim(balance.vars.unscreened.party)##partisan sample incorrect on at least one screen



dim(balance.vars.screened.party)

n.full<-NA
n.party<-NA
n.party.screened<-NA
n.party.unscreened<-NA
mean.full<-NA
mean.party<-NA
mean.party.screened<-NA
mean.party.unscreened<-NA
t.stad.diff.parties<-NA



####
##all respondents who answered first screen
###
as.n<-as.numeric
as.c<-as.character
for (i in seq(1,ncol(balance.vars)-3, by=1)){
	n.full[i]<-as.n(table(is.na(balance.vars[,i]))[1])
	n.party[i]<-as.n(table(is.na(balance.vars.party[,i]))[1])
	n.party.screened[i]<-as.n(table(is.na(balance.vars.screened.party[,i]))[1])
	n.party.unscreened[i]<-as.n(table(is.na(balance.vars.unscreened.party[,i]))[1])

	mean.full[i]<-mean(balance.vars[,i], na.rm=TRUE)
	mean.party[i]<-mean(balance.vars.party[,i], na.rm=TRUE)
	mean.party.screened[i]<-mean(balance.vars.screened.party[,i], na.rm=TRUE)
	mean.party.unscreened[i]<-mean(balance.vars.unscreened.party[,i], na.rm=TRUE)
	
	t.stad.diff.parties[i]<-(t.test(balance.vars.screened.party[,i],balance.vars.unscreened.party[,i], na.rm=TRUE ))$statistic
	
	}

variable<-names(balance.vars[,seq(1,ncol(balance.vars)-3, by=1)])

des.stats<-cbind.data.frame(variable=variable, n.party, n.party.screened, n.party.unscreened, mean.party, mean.party.screened, mean.party.unscreened, t.stad.diff.parties)
rownames(des.stats)<-des.stats$variable
des.stats<-des.stats[,-1]
library(xtable)

##TABLE 1 in Online Appendix

cat("Table 1 in Online Appendix\n")
print(xtable(des.stats, digits=c(0,0,0,0,2,2,2,2)))











##########ANES ESTIMATION
as.n<-as.numeric
as.c<-as.character

#####
######
#### ANALYSIS
######
#####

load("dd.final.Rdata")
dd<-dd.final
head(dd)

load("anes.12.regions.Rdata")
head(anes.12)

####make data frames
ourdata<-cbind.data.frame("Age"=dd$age, "Education"=dd$educ.years, "Female"=dd$female, "Married"=dd$married, "Homeowner"=dd$ownrent2, "Ideology"=dd$ideol, "Interest in Politics"=dd$likepol,"Voted (2012)"=dd$vote,  "Non-Hispanic White"=dd$white2, "Non-Hispanic Black"=dd$black2, "Hispanic"=dd$hisp2, "Northeast"=dd$northeast, "Midwest"=dd$midwest, "South"=dd$south, "West"=dd$west, "Democrat"=dd$democrat, "Republican"=dd$republican)


anes.2012.data<-cbind.data.frame("Age"=anes.12$age, "Education"=anes.12$educ.years, "Female"=anes.12$female, "Married"=anes.12$married, "Homeowner"=anes.12$ownrent2, "Ideology"=anes.12$ideol,"Interest in Politics"=anes.12$likepol,"Voted (2012)"=anes.12$vote,  "Non-Hispanic White"=anes.12$white,"Non-Hispanic Black"=anes.12$black, "Hispanic"= anes.12$hisp,  "Northeast"=anes.12$northeast, "Midwest"=anes.12$midwest, "South"=anes.12$south, "West"=anes.12$west, "Democrat"=anes.12$democrat, "Republican"=anes.12$republican )

table(names(ourdata)==names(anes.2012.data))##these should match

##make subsets
ourdata.dems<-subset(ourdata, ourdata$Democrat==1)
ourdata.reps<-subset(ourdata, ourdata$Republican==1)
anes.2012.data.dems<-subset(anes.2012.data,anes.2012.data$Democrat==1 )
anes.2012.data.reps<-subset(anes.2012.data,anes.2012.data$Republican==1 )



mean.diff.data<-list(ourdata.dems= ourdata.dems, ourdata.reps=ourdata.reps, anes.2012.data.dems=anes.2012.data.dems, anes.2012.data.reps=anes.2012.data.reps)
names(mean.diff.data)

#######
######


##make results data frame
N.ourdata<-rep(NA, ncol(ourdata)-2)
N.anes.2012<-rep(NA, ncol(ourdata)-2)


mean.ourdata<-rep(NA, ncol(ourdata)-2)
mean.anes2012<-rep(NA, ncol(ourdata)-2)

mean.diff<-rep(NA, ncol(ourdata)-2)
mean.diff.lb<-rep(NA, ncol(ourdata)-2)
mean.diff.ub<-rep(NA, ncol(ourdata)-2)
t.stat.diff<-rep(NA, ncol(ourdata)-2)

results<-cbind.data.frame(variable=names(ourdata[, 1:(ncol(ourdata)-2)]),N.ourdata=N.ourdata,N.anes.2012=N.anes.2012, mean.ourdata=mean.ourdata,mean.anes2012=mean.anes2012, mean.diff=mean.diff, t.stat.diff=t.stat.diff)

results

results.parties<-list(results.dems=results, results.reps=results)


for (i in 1:(ncol(ourdata)-2)){
##dems
results.parties$results.dems$N.ourdata[i]<-as.n(table(is.na(mean.diff.data$ourdata.dems[,i]))[1])
results.parties$results.dems$N.anes.2012[i]<-as.n(table(is.na(mean.diff.data$anes.2012.data.dems[,i]))[1])

a<-t.test(mean.diff.data$ourdata.dems[,i],mean.diff.data$anes.2012.data.dems[,i], na.rm=TRUE)
results.parties$results.dems$mean.ourdata[i]<-as.n(a$estimate[1])
results.parties$results.dems$mean.anes2012[i]<-as.n(a$estimate[2])
results.parties$results.dems$mean.diff[i]<-as.n(a$estimate[1])-as.n(a$estimate[2])
#results.parties$results.dems$mean.diff.lb[i]<-as.n(a$conf.int[1])
#results.parties$results.dems$mean.diff.ub[i]<-as.n(a$conf.int[2])
results.parties$results.dems$t.stat.diff[i]<-as.n(a$statistic)


##reps

results.parties$results.reps$N.ourdata[i]<-as.n(table(is.na(mean.diff.data$ourdata.reps[,i]))[1])
results.parties$results.reps$N.anes.2012[i]<-as.n(table(is.na(mean.diff.data$anes.2012.data.reps[,i]))[1])


b<-t.test(mean.diff.data$ourdata.reps[,i],mean.diff.data$anes.2012.data.reps[,i], na.rm=TRUE)
results.parties$results.reps$mean.ourdata[i]<-as.n(b$estimate[1])
results.parties$results.reps$mean.anes2012[i]<-as.n(b$estimate[2])
results.parties$results.reps$mean.diff[i]<-as.n(b$estimate[1])-as.n(b$estimate[2])
#results.parties$results.reps$mean.diff.lb[i]<-as.n(b$conf.int[1])
#results.parties$results.reps$mean.diff.ub[i]<-as.n(b$conf.int[2])
results.parties$results.reps$t.stat.diff[i]<-as.n(b$statistic)
}
anes.compare<-results.parties

##drop unnecessary columns and make table to include in appendix
head(anes.compare[[1]])

##TABLE 2 in Online Appendix
cat("Table 2 in Online Appendix\n")
print(xtable(anes.compare$results.dems, digits=c(1,0,0,0,2,2,2,2) ),include.rownames=FALSE)

##TABLE 3 in Online Appendix
cat("Table 3 in Online Appendix\n")
print(xtable(anes.compare$results.reps, digits=c(1,0,0,0,2,2,2,2) ),include.rownames=FALSE)

file.name<-paste(output, "anes.compare.R", sep="")
save(anes.compare, file=file.name)

file.name<-paste(output,"anes.compare.csv", sep="")
write.csv(anes.compare,file=file.name)






#################
################
######
######CORRELATION MATRICES
#################
###################


####code feeling thermometers


###Obama
##our data: 
table(dd$thrm.obama)
##anes.12 
table(anes.12$ftpo_pres)
anes.12$thrm.obama<-anes.12$ftpo_pres<-as.n(as.c(anes.12$ftpo_pres))
anes.12$thrm.obama[anes.12$thrm.obama<0]<-NA
table(anes.12$thrm.obama)



##unions
##our data
table(dd$thrm.unions)
##anes.12
anes.12$thrm.unions<-anes.12$ftgr_unions<-as.n(as.c(anes.12$ftgr_unions))
anes.12$thrm.unions[anes.12$thrm.unions<0]<-NA
table(anes.12$thrm.unions)

## corporate execs/big business
##our data: "corporate execs"
table(dd$thrm.execs)
##anes.12 "big business"
anes.12$thrm.execs<-anes.12$ftgr_bigbus<-as.n(as.c(anes.12$ftgr_bigbus))
anes.12$thrm.execs[anes.12$thrm.execs<0]<-NA
table(anes.12$thrm.execs)

##atheists
##our data:
table(dd$thrm.atheists)
##anes.12
anes.12$thrm.atheists<-anes.12$ftgr_atheists<-as.n(as.c(anes.12$ftgr_atheists))
anes.12$thrm.atheists[anes.12$thrm.atheists<0]<-NA
table( anes.12$thrm.atheists)

##GOP (conservatives)
##our data: GOP
table(dd$trhm.gop)
##conservatives (instead of GOP)
anes.12$thrm.gop<-anes.12$ftgr_cons<-as.n(as.c(anes.12$ftgr_cons))
anes.12$thrm.gop[anes.12$thrm.gop<0]<-NA
table(anes.12$thrm.gop)

##Dem party (liberals)
#our data: Dem Party
table(dd$thrm.dem)
##anes.12 (liberals)
anes.12$thrm.dem<-anes.12$ftgr_liberals<-as.n(as.c(anes.12$ftgr_liberals))
anes.12$thrm.dem[anes.12$thrm.dem<0]<-NA
table(anes.12$thrm.dem)


ourdata.cor<-cbind.data.frame(Atheists=dd$thrm.atheists, Execs=dd$thrm.execs, Unions=dd$thrm.unions, Obama=dd$thrm.obama)

ourdata.cor.matrix<-as.matrix(cor(ourdata.cor, use='complete.obs'))


cat("Table 4 in Online Appendix\n")
xtable(ourdata.cor.matrix, digits=3)


anes.12.cor.data<-cbind.data.frame( Atheists=anes.12$thrm.atheists, BigBusiness=anes.12$thrm.execs, Unions=anes.12$thrm.unions, Obama=anes.12$thrm.obama)


anes.12.cor.matrix<-as.matrix(cor(anes.12.cor.data, use='complete.obs'))

##Table 5 in Online Appendix
cat("Table 5 in Online Appendix\n")
xtable(anes.12.cor.matrix, digits=3)


cat("End\n")

