


library(RItools)
library(foreign)

dataset<-read.dta("disagreement_balance.dta")


sink("BalanceTest10152013.txt")

results<-xBalance(tsdx~inddisagreex+sicko+democrat_pre+independent_pre+educ_pre+liberal_pre+moderate_pre+gender_pre+
homeowner_pre+raceblack_pre+racehisp_pre+raceasian_pre+employed_pre+
insured_pre+healthstatus_pre+majorill_pre+vote2006_pre+ideology, 
strata=as.factor(dataset$site[dataset$goodsite==1]), data=dataset[dataset$goodsite==1,], 
report=c("std.diffs", "adj.means", "adj.mean.diffs", "z.scores", "chisquare.test"))

cat("goodsites", "\n")
print(results)

results<-xBalance(tsdx~inddisagreex+sicko+democrat_pre+independent_pre+educ_pre+liberal_pre+moderate_pre+gender_pre+
homeowner_pre+raceblack_pre+racehisp_pre+raceasian_pre+employed_pre+
insured_pre+healthstatus_pre+majorill_pre+vote2006_pre+ideology, 
strata=as.factor(dataset$site[dataset$goodsite==0]), data=dataset[dataset$goodsite==0,], 
report=c("std.diffs", "adj.means", "adj.mean.diffs", "z.scores", "chisquare.test"))

cat("not goodsites", "\n")
print(results)

sink()

