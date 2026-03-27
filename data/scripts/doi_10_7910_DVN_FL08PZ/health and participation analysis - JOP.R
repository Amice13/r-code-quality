#####################################Replication files for "How Different Forms of Health Matter to Political Participation" by Burden, Fletcher, Herd, Jones, and Moynihan in the Journal of Politics"
#This file organizes the code used to run the analysis
#The full analysis requires a dataset that needs approval from the WLS team
#For a public version of the dataset (and instructions on obtaining the full data) see: http://www.ssc.wisc.edu/wlsresearch/data/

#Working directories will need to be set appropriately throughout

#########final analysis

###set up data
##get small version of Catalist that has only the variables we use
##if working with the public data, some of the variable names may need to be changed
##consult the documentation from the WLS
source("set up data from private.R")

source("functions.R")
library(stargazer)
plot <- FALSE

###tables


##################################TABLE A1
##Descriptives
##create walking speed in meters per second
data$walk_speed <- 2.5/data$walk_speed_raw

desc <- c("cat08","cat10","cat12","give08","give10","give12",
	"cog04","cog10","iq","hui04","hui10","walk_speed",
	"walk_speed_mis","age","networth04_10","educ04","educDad2",
	"educMom2","parentsInc57","male")
d.table <- array(NA, c(length(desc),3))
rownames(d.table) <- desc
for (j in 1:length(desc)) {
	d.table[j,1] <- round(mean(data[[desc[j]]], na.rm=TRUE),2)
	d.table[j,2] <- round(min(data[[desc[j]]], na.rm=TRUE),2)
	d.table[j,3] <- round(max(data[[desc[j]]], na.rm=TRUE),2)
}
d.table

##Basic control variables

eqn <- y ~ iq + zAge + zAge2 + networth04_10 + educ04 + educDad2 + educMom2 +
	parentsInc57 + male + fl + mn + ca + il + az + tx + colo + other_state


#################Voting
source("voting analysis.R")


###########################TABLES 1 & A2 
###output the results
writeLines("Voting Results\n\n", "results/voting.txt")
write(c("Summary Table\n\n", voting.summary.table), 
	file = "results/voting.txt", append=TRUE)
write(c("\n\nSummary Table Regressions\n\nCognition Results\n\n", cog.summary),
	file = "results/voting.txt", append = TRUE)
write(c("\n\nHUI Results\n\n", hui.summary),
	file = "results/voting.txt", append = TRUE)
write(c("\n\nWalking Speed\n\n", walk.summary),
	file = "results/voting.txt", append = TRUE)

write(c("\n\nAppendix Regressions (full sample)\n\n",
	voting.appendix.full), file = "results/voting.txt", append = TRUE)
write(c("\n\nAppendix Regressions (restricted sample)\n\n",
	voting.appendix.restricted), file = "results/voting.txt", append = TRUE)
write(c("\n\nBaseline models (without health)\n\n",
	baseline), file = "results/voting.txt", append = TRUE)



#################Campaign Giving TABLES 2 and A3
source("contribution analysis.R")

writeLines("Contribution Results\n\n", "results/contributions.txt")
write(c("Summary Table\n\n", contr.summary.table), 
	file = "results/contributions.txt", append=TRUE)
write(c("\n\nSummary Table Regressions\n\nCognition Results\n\n", ccog.summary),
	file = "results/contributions.txt", append = TRUE)
write(c("\n\nHUI Results\n\n", chui.summary),
	file = "results/contributions.txt", append = TRUE)
write(c("\n\nWalking Speed\n\n", cwalk.summary),
	file = "results/contributions.txt", append = TRUE)

write(c("\n\nAppendix Regressions (full sample)\n\n",
	contr.appendix.full), file = "results/contributions.txt", append = TRUE)
write(c("\n\nAppendix Regressions (restricted sample)\n\n",
	contr.appendix.restricted), file = "results/contributions.txt", append = TRUE)

write(c("\n\nBaseline models (without health)\n\n",
	baseline), file = "results/contributions.txt", append = TRUE)

write(c("\n\nOther models\n\n",
	other.models), file = "results/contributions.txt", append = TRUE)
write(c("\n",logLik(res[[3]])), file = "results/contributions.txt", 
	append = TRUE)


#################Persuasion  TABLE A4
source("persuasion analysis.R")

writeLines("Persuasion Results\n\n", "results/persuasion.txt")
write(c("Summary Table\n\n", polgrp.summary.table), 
	file = "results/polgroups.txt", append=TRUE)
write(c("\n\nAppendix Regressions (full sample)\n\n", polgrp.appendix.full),
	file = "results/persuasion.txt", append = TRUE)
write(c("\n\nAppendix Regressions (restricted sample)\n\n", 
	polgrp.appendix.restricted),
	file = "results/persuasion.txt", append = TRUE)

#################Self-reported voting TABLE A5
source("vote (self-report) analysis.R")

writeLines("Self-reported voting Results\n\n", "results/self_vote.txt")
write(c("Summary Table\n\n", polgrp.summary.table), 
	file = "results/self_vote.txt", append=TRUE)
write(c("\n\nAppendix Regressions (full sample)\n\n", polgrp.appendix.full),
	file = "results/self_vote.txt", append = TRUE)
write(c("\n\nAppendix Regressions (restricted sample)\n\n", 
	polgrp.appendix.restricted),
	file = "results/self_vote.txt", append = TRUE)


##############################Plots  FIGURE 1
data$y <- data$cat12

####see the "plot.health" function in the "functions.R" script for details
if (plot) plot.health(eqn, health = c("educ04", "cog10", "hui10"),
	xlab = "Change in Probability of Voting", year = "2012", 
	adj = .2, hlab = c("Education", "Cognition", "HUI"), bs = 500)
eqn <- y ~ cog04 + hui04 + iq + zAge + zAge2 + networth04_10 + educ04 + educDad + educMom +
	parentsInc57 + male + fl + mn + ca + il + az + tx + co + other_state
data$y <- data$cat08
if (plot) plot.health(eqn, health = c("educ04", "cog04", "hui04"),
	hlab = c("", "", ""), year = "2008", bs = 500, add = TRUE)
data$y <- data$cat10
if (plot) plot.health(eqn, health = c("educ04", "cog04", "hui04"),
	add = TRUE, year = "2010", adj = .1, bs = 500)

#################################FIGURE 2
data$y <- data$give12
if (plot) plot.health(eqn, health = c("educ04", "cog10", "hui10"),
	xlab = "Change in Probability of Contributing", year = "2012", 
	adj = .2, hlab = c("Education", "Cognition", "HUI"), bs = 500)
eqn <- y ~ cog04 + hui04 + iq + zAge + zAge2 + networth04_10 + educ04 + educDad + educMom +
	parentsInc57 + male + fl + mn + ca + il + az + tx + co + other_state
data$y <- data$give08
if (plot) plot.health(eqn, health = c("educ04", "cog04", "hui04"),
	hlab = c("", "", ""), year = "2008", bs = 500, add = TRUE)
data$y <- data$give10
if (plot) plot.health(eqn, health = c("educ04", "cog04", "hui04"),
	add = TRUE, year = "2010", adj = .1, bs = 500)



