
###################################### Contents:

#### Preliminaries
## set working directory
## load libraries
## load data
## cleaning and creating variables

#### Analyses and figures from main paper
## figure 2
## analyses underpinning figure 3 
## figure 3
## tables 5, 8, 9 from appendix

#### Analyses and figures from appendix
## Appendix table 3: covariate balance across treatment and control
## Appendix table 4: OLS 3-item knowledge index regression, including differential attrition
## Appendix figure 2: Correctness cutoff sensitivity figure
## Appendix table 6: OLS 4-item knowledge, including differential attrition
## Appendix figure 3: Treatment and recall
## Appendix table 7: what moved?
## Appendix table 10: motivated reasoning differential treatment effects (attitudes)
## Appendix table 11: motivated reasoning differential treatment effects (preferences)

###################################### Preliminaries

## Set working directory
# Set this directory to the file which includes the replication data
setwd("~/Dropbox/AviEthanJakeLucy/Results/replication_data")

## Load  libraries
library(foreign)
library(MASS)
library(stargazer)

## Load data, recode to generate new vars with correct direction
load("bphf_replication.RData")

bphf$waste_w1_pos = 4 - bphf$waste_w1
bphf$waste_w2_pos = 4 - bphf$waste_w2
bphf$taxyoupay_w1_pos = 6 - bphf$taxyoupay_w1
bphf$taxyoupay_w2_pos = 6 - bphf$taxyoupay_w2
bphf$bot5th_w1_pos = 6 - bphf$bot5th_w1
bphf$bot5th_w2_pos = 6 - bphf$bot5th_w2

## ordered versions of outcome variables
bphf$knowledgeindex_w2_3item.f = as.ordered(bphf$knowledgeindex_w2_3item)
bphf$fair_w2.f = as.ordered(bphf$fair_w2)
bphf$trust_w2.f = as.ordered(bphf$trust_w2)
bphf$value_w2.f = as.ordered(bphf$value_w2)
bphf$avoid_w2.f = as.ordered(bphf$avoid_w2)
bphf$waste_w2_pos.f = as.ordered(bphf$waste_w2_pos)
bphf$taxyoupay_w2_pos.f = as.ordered(bphf$taxyoupay_w2_pos)
bphf$top5th_w2.f = as.ordered(bphf$top5th_w2)
bphf$bot5th_w2_pos.f = as.ordered(bphf$bot5th_w2_pos)

bphf$educationfactor = as.ordered(bphf$educationfactor)

###################################### Replication of figures and analyses presented in main paper:
########### Figure 2 from the paper:

## simple averages:
before.mu = c(mean(bphf$knowledgeindex_w1_3item[which(bphf$treatment == 0)], na.rm = T), mean(bphf$knowledgeindex_w1_3item[which(bphf$treatment == 1)], na.rm = T))
after.mu = c(mean(bphf$knowledgeindex_w2_3item[which(bphf$treatment == 0)], na.rm = T), mean(bphf$knowledgeindex_w2_3item[which(bphf$treatment == 1)], na.rm = T))

## confidence intervals of these means
before.s <- c(sd(bphf$knowledgeindex_w1_3item[which(bphf$treatment == 0)], na.rm = T), sd(bphf$knowledgeindex_w1_3item[which(bphf$treatment == 1)], na.rm = T))
after.s <- c(sd(bphf$knowledgeindex_w2_3item[which(bphf$treatment == 0)], na.rm = T), sd(bphf$knowledgeindex_w2_3item[which(bphf$treatment == 1)], na.rm = T))

before.n <- c(length(na.omit(bphf$knowledgeindex_w1_3item[which(bphf$treatment == 0)])), length(na.omit(bphf$knowledgeindex_w1_3item[which(bphf$treatment == 1)])))
after.n <- c(length(na.omit(bphf$knowledgeindex_w2_3item[which(bphf$treatment == 0)])), length(na.omit(bphf$knowledgeindex_w2_3item[which(bphf$treatment == 1)])))

before.errors <- qnorm(0.975)*before.s/sqrt(before.n)
after.errors <- qnorm(0.975)*after.s/sqrt(after.n)
 
before.upper <- before.mu + before.errors
before.lower <- before.mu - before.errors

after.upper <- after.mu + after.errors
after.lower <- after.mu - after.errors

#pdf("Figure1.pdf")

par( mar = c(4, 4, 2, 2) + 0.1,
     tck=0.01,
     mgp=c(2,0.5,0))

x_wave <- c(0.25, 1.25)
x_eps <- 0.05

b2 = matrix(c(x_wave[1] - x_eps, x_wave[1] + x_eps, 
              x_wave[2] - x_eps, x_wave[2] + x_eps), byrow = F, ncol = 2)


plot(b2, c(before.mu, after.mu), type = "n", ylim = c(0, 1.1), xlim = c(0, 1.75), xaxt = "n", ylab = "Average Number of Correct Responses", xlab = "",
     bty = "n", xaxs = "i", cex.axis = 1.25, cex.lab = 1.25)

segments(x0 = b2, x1 = b2, y0 = c(before.lower, after.lower), y1 = c(before.upper, after.upper), lwd = 1.5)
segments(x0 = b2[,1], x1 = b2[,2], y0 = before.mu, y1 = after.mu, lty = 3, col = c(1, "grey50"))


points(c(b2), c(before.mu, after.mu), pch = c(21, 23), cex = 1.75, bg = c("white", "black"))
axis(side = 1, at = x_wave, labels = c("Before\nreceipts", "After\nreceipts"), padj = 0.6, cex.axis = 1.25)

text(x_wave[2] + 0.15, after.mu[2], "Treatment", cex = 1.25, adj = c(0, NA))
text(x_wave[2] + 0.15, after.mu[1], "Control", cex = 1.25, adj = c(0, NA))

#dev.off()

###### Regression specifications for figure 3 in the main paper (and tables 5, 8, 9 in the online appendix)

know.o.cha = polr(knowledgeindex_w2_3item.f ~ treatment + knowledgeindex_w1_3item +age +female +white +conservative +labour +libdem +fulltime +region2 +region3 + region4 + region5 +region6 +educationfactor +income1 + income2 + income3 + income4 + income5 + income6 + income7 + income8 + income9 +income10 + income11 + income12 + income13 + income14 +income15, data = bphf, method = "logistic", Hess = T) 

know.o.cha.en = polr(knowledgeindex_w2_3item.f ~ treatment + knowledgeindex_w1_3item +age +female +white +conservative +labour +libdem +fulltime +region2 +region3 + region4 + region5 +region6 +educationfactor +income1 + income2 + income3 + income4 + income5 + income6 + income7 + income8 + income9 +income10 + income11 + income12 + income13 + income14 +income15, data = bphf, method = "logistic", Hess = T) 


## attitudes regressions
fair.o.cha = polr(fair_w2.f ~ treatment + fair_w1 +age +female +white +conservative +labour +libdem +fulltime +region2 +region3 + region4 + region5 +region6 +educationfactor +income1 + income2 + income3 + income4 + income5 + income6 + income7 + income8 + income9 +income10 + income11 + income12 + income13 + income14 +income15, data = bphf, Hess = T, method = "logistic") 

trust.o.cha = polr(trust_w2.f ~ treatment + trust_w1 +age +female +white +conservative +labour +libdem +fulltime +region2 +region3 + region4 + region5 +region6 +educationfactor +income1 + income2 + income3 + income4 + income5 + income6 + income7 + income8 + income9 +income10 + income11 + income12 + income13 + income14 +income15, data = bphf, Hess = T, method = "logistic") 

value.o.cha = polr(value_w2.f ~ treatment + value_w1 +age +female +white +conservative +labour +libdem +fulltime +region2 +region3 + region4 + region5 +region6 +educationfactor +income1 + income2 + income3 + income4 + income5 + income6 + income7 + income8 + income9 +income10 + income11 + income12 + income13 + income14 +income15, data = bphf, Hess = T, method = "logistic") 

avoid.o.cha = polr(avoid_w2.f ~ treatment + avoid_w1 +age +female +white +conservative +labour +libdem +fulltime +region2 +region3 + region4 + region5 +region6 +educationfactor +income1 + income2 + income3 + income4 + income5 + income6 + income7 + income8 + income9 +income10 + income11 + income12 + income13 + income14 +income15, data = bphf, Hess = T, method = "logistic") 

waste.o.cha = polr(waste_w2_pos.f ~ treatment + waste_w1_pos +age +female +white +conservative +labour +libdem +fulltime +region2 +region3 + region4 + region5 +region6 +educationfactor +income1 + income2 + income3 + income4 + income5 + income6 + income7 + income8 + income9 +income10 + income11 + income12 + income13 + income14 +income15, data = bphf, Hess = T, method = "logistic") 

## policy preference regressions

taxyoupay.o.cha = polr(taxyoupay_w2_pos.f ~ treatment + taxyoupay_w1_pos +age +female +white +conservative +labour +libdem +fulltime +region2 +region3 + region4 + region5 +region6 +educationfactor +income1 + income2 + income3 + income4 + income5 + income6 + income7 + income8 + income9 +income10 + income11 + income12 + income13 + income14 +income15, data = bphf, Hess = T, method = "logistic") 

top5th.o.cha = polr(top5th_w2.f ~ treatment + top5th_w1+age +female +white +conservative +labour +libdem +fulltime +region2 +region3 + region4 + region5 +region6 +educationfactor +income1 + income2 + income3 + income4 + income5 + income6 + income7 + income8 + income9 +income10 + income11 + income12 + income13 + income14 +income15, data = bphf, Hess = T, method = "logistic") 


bot5th.o.cha = polr(bot5th_w2_pos.f ~ treatment + bot5th_w1_pos +age +female +white +conservative +labour +libdem +fulltime +region2 +region3 + region4 + region5 +region6 +educationfactor +income1 + income2 + income3 + income4 + income5 + income6 + income7 + income8 + income9 +income10 + income11 + income12 + income13 + income14 +income15, data = bphf, method = "logistic", Hess = T) 

### Creating figure 3:

treatment.effects.o.cha = c(know.o.cha$coefficients["treatment"],
		fair.o.cha$coefficients["treatment"],
		trust.o.cha$coefficients["treatment"],
		value.o.cha$coefficients["treatment"],
		avoid.o.cha$coefficients["treatment"],
		waste.o.cha$coefficients["treatment"],
		taxyoupay.o.cha$coefficients["treatment"],
		top5th.o.cha$coefficients["treatment"],
		bot5th.o.cha$coefficients["treatment"])
treatment.effects.o.cha.order = treatment.effects.o.cha[order(treatment.effects.o.cha)]


conf.int.o.cha = rbind(
	confint(know.o.cha, "treatment"),
	confint(fair.o.cha, "treatment"),
	confint(trust.o.cha, "treatment"),
	confint(value.o.cha, "treatment"),
	confint(avoid.o.cha, "treatment"),
	confint(waste.o.cha, "treatment"),
	confint(taxyoupay.o.cha, "treatment"),
	confint(top5th.o.cha, "treatment"),
	confint(bot5th.o.cha, "treatment"))
	
rownames(conf.int.o.cha) = c("Knowledge", "Fairness", "Trust", "Value", "Avoidance", "Waste", "Own tax", "Tax on rich", "Tax on poor")

treatment.effect.o.ch.types = as.data.frame(treatment.effects.o.cha)
treatment.effect.o.ch.types$type = c(3, 2, 2, 2, 2, 2, 1,1,1)
ordertype.o.ch = order(treatment.effect.o.ch.types[,2], treatment.effect.o.ch.types[,1])


par(mar = c(4, 8, 1, 1)+0.2)
plot(x = c(-0.4, 0.4), y = c(0, length(treatment.effects.o.cha)+3), col = "transparent", yaxt = "n", ylab = "", xlab = "Coefficient on treatment")
abline(v = 0, lty = 3)

points(x = c(treatment.effects.o.cha[ordertype.o.ch][1:3], NA, treatment.effects.o.cha[ordertype.o.ch][4:8], NA, treatment.effects.o.cha[ordertype.o.ch][9]) , y = c(1:(length(treatment.effects.o.cha)+2)), pch = 18)

segments(x0 = c(conf.int.o.cha[ordertype.o.ch[c(1:3)],1], NA, conf.int.o.cha[ordertype.o.ch[c(4:8)],1], NA, conf.int.o.cha[ordertype.o.ch[9],1]), x1 = c(conf.int.o.cha[ordertype.o.ch[c(1:3)],2], NA, conf.int.o.cha[ordertype.o.ch[c(4:8)],2], NA, conf.int.o.cha[ordertype.o.ch[9],2]), y0 = c(1:(length(treatment.effects.o.cha)+2)), y1 = c(1:(length(treatment.effects.o.cha)+2)))

ordered.names = rownames(conf.int.o.cha)[ordertype.o.ch]

mtext(ordered.names[1:3], side = 2, las = 2, at = c(1:3), line = 1)
mtext(ordered.names[4:8], side = 2, las = 2, at = c(5:9), line = 1)
mtext("3-item index", side = 2, las = 2, at = 11, line = 1)
mtext("Knowledge", side = 2, las = 2, at = 12, adj = 0, line = 7, font = 2)
mtext("Attitudes", side = 2, las = 2, at = 10, adj = 0, line = 7, font = 2)
mtext("Preferences", side = 2, las = 2, at = 4, adj = 0, line = 7, font = 2)

### Appendix table 5:
stargazer(know.o.cha, style = "ajps", omit = c("income", "region"), order = c("treatment", "age", "female", "white", "conservative", "labour", "libdem","fulltime", "education", "w1"), covariate.labels= c("Treatment", "Age", "Female", "White", "Conservative", "Labour", "Liberal Democrat", "Working full time", "Education scale", "Wave 1 knowledge"), type = "text", dep.var.labels = "3-item knowledge index")

### Appendix table 8:
stargazer(fair.o.cha, trust.o.cha, value.o.cha, avoid.o.cha, waste.o.cha, style = "ajps", omit = c("income", "region"), order = c("treatment", "age", "female", "white", "conservative", "labour", "libdem","fulltime", "education", "w1"), covariate.labels= c("Treatment", "Age", "Female", "White", "Conservative", "Labour", "Liberal Democrat", "Working full time", "Education scale", "Wave 1 fairness", "Wave 1 trust", "Wave 1 value", "Wave 1 avoidance", "Wave 1 waste"), dep.var.labels = c("Fairness", "Trust", "Value", "Avoidance", "Waste"), type = "text")

### Appendix table 9: 
stargazer(taxyoupay.o.cha, top5th.o.cha, bot5th.o.cha, style = "ajps", omit = c("income", "region"), order = c("treatment", "age", "female", "white", "conservative", "labour", "libdem","fulltime", "education", "w1"), covariate.labels= c("Treatment", "Age", "Female", "White", "Conservative", "Labour", "Liberal Democrat", "Working full time", "Education scale", "Wave 1 own tax", "Wave 1 tax on rich", "Wave 1 tax on poor"), dep.var.labels = c("Own tax", "Tax on rich", "Tax on poor"), type = "text")

############################ Replication of tables and figures from the online appendix

#### Appendix table 3: covariate balance across treatment and control
## create excluded region dummy:
bphf$region1 = as.numeric(bphf$region2 == 0 & bphf$region3 == 0 & bphf$region4 == 0 & bphf$region5 == 0 & bphf$region6 == 0)
varnames = c("age", "female", "conservative", "white", "labour", "libdem", "fulltime", "educationfactor", "region1", "region2", "region3", "region4", "region5", "region6" )

mean.control = rep(NA, length(varnames))
mean.treat = rep(NA, length(varnames))

for(i in 1:length(varnames)){
	mean.control[i] = mean(bphf[which(bphf$treatment == 0) , varnames[i]], na.rm = T)
	mean.treat[i] = mean(bphf[which(bphf$treatment == 1) , varnames[i]], na.rm = T)
}

t.test.t = rep(NA, length(varnames))
for(i in 1:length(varnames)){
	temp.t = t.test(bphf[which(bphf$treatment == 0) , varnames[i]], bphf[which(bphf$treatment == 1) , varnames[i]])
	t.test.t[i] = temp.t$statistic
}

ks.test.stat = rep(NA, length(varnames))
for(i in 1:length(varnames)){
	temp.ks = ks.test(bphf[which(bphf$treatment == 0) , varnames[i]], bphf[which(bphf$treatment == 1) , varnames[i]])
	ks.test.stat[i] = temp.ks$p.value
}

## create table 3:

at3 = cbind(mean.control, mean.treat, t.test.t)
at3 = round(at3, digits = 2)
rownames(at3) = varnames
at3

#### Appendix table 4: OLS 3-item knowledge index regression, including differential attrition
final3a = lm(knowledgeindex_w2_3item ~ treatment +age +female +white +conservative +labour +libdem +fulltime +region2 +region3 + region4 + region5 +region6 +educationfactor +income1 + income2 + income3 + income4 + income5 + income6 + income7 + income8 + income9 +income10 + income11 + income12 + income13 + income14 +income15, data = bphf) 

final3b = lm(knowledgeindex_w2_3item ~ treatment +age +female +white +conservative +labour +libdem +fulltime +region2 +region3 + region4 + region5 +region6 +educationfactor +income1 + income2 + income3 + income4 + income5 + income6 + income7 + income8 + income9 +income10 + income11 + income12 + income13 + income14 +income15 +knowledgeindex_w1_3item, data = bphf) 

final3b.att = lm(knowledgeindex_w2_3item ~ treatment +age +female +white +conservative +labour +libdem +fulltime +region2 +region3 + region4 + region5 +region6 +educationfactor +income1 + income2 + income3 + income4 + income5 + income6 + income7 + income8 + income9 +income10 + income11 + income12 + income13 + income14 +income15 +knowledgeindex_w1_3item, data = bphf, weights = wt) 

stargazer(final3a, final3b, final3b.att, style = "ajps", omit = c("income", "region"), order = c("treatment", "age", "female", "white", "conservative", "labour", "libdem","fulltime", "education", "w1"), covariate.labels= c("Treatment", "Age", "Female", "White", "Conservative", "Labour", "Liberal Democrat", "Working full time", "Education scale", "Wave 1 knowledge"), dep.var.labels = "Wave 2 knowledge index", type = "text")

#### Appendix figure 2: Correctness cutoff sensitivity figure
knowledge.cutoff = function(overseas, defense, health, alpha){ # where knowledge, overseas and health are vectors of responses; alpha is the cutoff distance for correctness
	## create 'tolerance' cutoffs for each response
	overseas.tol = 0.8 + alpha # because so close to zero, asymmetric
	if(alpha < 10.8){			# small alpha, defense bounds are symmetric
	defense.tol.upper = 5.4 + alpha/2
	defense.tol.lower = 5.4 - alpha/2
	} 
	else if (alpha >= 10.8){
	defense.tol.lower = 0
	defense.tol.upper = alpha
	}
	if(alpha < 36.8){
	health.tol.lower = 18.4 - alpha/2
	health.tol.upper = 18.4 + alpha/2
	}
	else if (alpha > 36.8){
	health.tol.lower = 0
	health.tol.upper = alpha
	}
	
	## create individual item correctness vectors
	overseas.right = as.numeric(overseas < overseas.tol)
	overseas.right[which(is.na(overseas.right)==T)] = 0
	defense.right = as.numeric(defense.tol.lower < defense & defense < defense.tol.upper)
	defense.right[which(is.na(defense.right)==T)] = 0
	health.right = as.numeric(health.tol.lower < health & health < health.tol.upper)
	health.right[which(is.na(health.right)==T)] = 0

	## sum to index
	sens_knowledgeindex = apply(cbind(overseas.right, defense.right, health.right), 1, sum)
	
	#return(cbind(overseas.right, defense.right, health.right, sens_knowledgeindex))  ## use this line if you want to return individual item correctness at changing correct levels as well as the full index
	return(sens_knowledgeindex)
}

replicate10_w1 = knowledge.cutoff(overseas = bphf$knowoverseas_w1, defense = bphf$knowdefense_w1, health = bphf$knowhealth_w1, alpha = 10)

## now create these knowledge indices for w1 and w2, for a bunch of alpha levels
usealpha = seq(from = 0.01, to = 20, length = 100)

usedata = bphf[ , which(names(bphf) %in% c("knowledgeindex_w2_3item", "knowoverseas_w1", "knowoverseas_w2", "knowdefense_w1", "knowdefense_w2", "knowhealth_w1", "knowhealth_w2","treatment", "age", "female", "white", "conservative", "labour", "libdem", "fulltime", "region2", "region3", "region4", "region5", "region6", "educationfactor", "income1", "income2", "income3", "income4", "income5", "income6", "income7", "income8", "income9", "income10", "income11", "income12", "income13", "income14", "income15"))]

# create lists to store results
cutoff.sensitivity.results = list()

run.cutoff.sensitivity = function(alpha){
	attach(usedata)
	dv = knowledge.cutoff(knowoverseas_w2, knowdefense_w2, knowhealth_w2, alpha)
	dv[which(is.na(knowledgeindex_w2_3item)==T)] = NA # this is sketchy but I can't figure out how NAs were dealt with in the original assignments to the knowledge index
	wave1 = knowledge.cutoff(knowoverseas_w1, knowdefense_w1, knowhealth_w1, alpha)
	reg.a = lm(dv ~ treatment +age +female +white +conservative +labour +libdem +fulltime +region2 +region3 + region4 + region5 +region6 +educationfactor +income1 + income2 + income3 + income4 + income5 + income6 + income7 + income8 + income9 +income10 + income11 + income12 + income13 + income14 +income15)
	reg.b = lm(dv ~ treatment +age +female +white +conservative +labour +libdem +fulltime +region2 +region3 + region4 + region5 +region6 +educationfactor +income1 + income2 + income3 + income4 + income5 + income6 + income7 + income8 + income9 +income10 + income11 + income12 + income13 + income14 +income15 + wave1)
	detach(usedata)
	return(list(reg.a, reg.b))
}

for(i in 1:length(usealpha)){
	cutoff.sensitivity.results[[i]] = run.cutoff.sensitivity(usealpha[i])
}

###### pull out coefficients
betas.a = rep(NA, length(usealpha))
betas.b = rep(NA, length(usealpha))
for(i in 1:length(usealpha)){
betas.a[i] = cutoff.sensitivity.results[[i]][[1]]$coefficients[2]
betas.b[i] = cutoff.sensitivity.results[[i]][[2]]$coefficients[2]
}
###### pull out/create confidence intervals
interval.a = matrix(NA, length(usealpha), 2)
interval.b = matrix(NA, length(usealpha), 2)
for(i in 1:length(usealpha)){
interval.a[i,] = confint(cutoff.sensitivity.results[[i]][[1]], "treatment")
interval.b[i,] = confint(cutoff.sensitivity.results[[i]][[2]], "treatment")
}


par(mar = c(4,4,1,1)+0.2)
plot(usealpha, betas.b, type = "p", ylim = c(-0.2, 0.25), ylab = "Treatment effect estimate", xlab = "'Correct' cutoff", pch = 18, col = "transparent")
segments(x0 = usealpha, x1 = usealpha, y0 = interval.b[,1], y1 = interval.b[,2], col = "grey60")
points(usealpha, betas.b, pch = 18,cex = 0.7)
abline(h = 0)
legend(x=0, y=-0.1, legend = c("Treatment effect estimates", "95% confidence intervals"), bty = "n", pch = c(18, NA), lty = c(NA, 1), col = c(1, "grey60"), cex = 0.8)

#### Appendix table 6: OLS 4-item knowledge, including differential attrition
final4a = lm(knowledgeindex_w2_missing ~ treatment +age +female +white +conservative +labour +libdem +fulltime +region2 +region3 + region4 + region5 +region6 +educationfactor +income1 + income2 + income3 + income4 + income5 + income6 + income7 + income8 + income9 +income10 + income11 + income12 + income13 + income14 +income15, data = bphf) 

final4b = lm(knowledgeindex_w2_missing ~ treatment +age +female +white +conservative +labour +libdem +fulltime +region2 +region3 + region4 + region5 +region6 +educationfactor +income1 + income2 + income3 + income4 + income5 + income6 + income7 + income8 + income9 +income10 + income11 + income12 + income13 + income14 +income15 +knowledgeindex_w1_missing, data = bphf) 

final4b.att = lm(knowledgeindex_w2_missing ~ treatment +age +female +white +conservative +labour +libdem +fulltime +region2 +region3 + region4 + region5 +region6 +educationfactor +income1 + income2 + income3 + income4 + income5 + income6 + income7 + income8 + income9 +income10 + income11 + income12 + income13 + income14 +income15 +knowledgeindex_w1_missing, data = bphf, weights = wt) 

stargazer(final4a, final4b, final4b.att, style = "ajps", omit = c("income", "region"), order = c("treatment", "age", "female", "white", "conservative", "labour", "libdem","fulltime", "education", "w1"), covariate.labels= c("Treatment", "Age", "Female", "White", "Conservative", "Labour", "Liberal Democrat", "Working full time", "Education scale", "Wave 1 knowledge"), dep.var.labels = "Wave 2 knowledge index, 4-item", type = "text")

#### Appendix figure 3: Treatment and recall
# knowledge index for those in treatment who recall
k3tr = mean(bphf$knowledgeindex_w2_3item[which(bphf$recall == 1 & bphf$treatment == 1)], na.rm = T)
k3tn = mean(bphf$knowledgeindex_w2_3item[which(bphf$recall == 0 & bphf$treatment == 1)], na.rm = T)
k3cr = mean(bphf$knowledgeindex_w2_3item[which(bphf$recall == 1 & bphf$treatment == 0)], na.rm = T)
k3cn = mean(bphf$knowledgeindex_w2_3item[which(bphf$recall == 0 & bphf$treatment == 0)], na.rm = T)


ttr= t.test(bphf$knowledgeindex_w2_3item[which(bphf$recall == 1 & bphf$treatment == 1)])
ttn= t.test(bphf$knowledgeindex_w2_3item[which(bphf$recall == 0 & bphf$treatment == 1)])
tcr = t.test(bphf$knowledgeindex_w2_3item[which(bphf$recall == 1 & bphf$treatment == 0)])
tcn = t.test(bphf$knowledgeindex_w2_3item[which(bphf$recall == 0 & bphf$treatment == 0)])

par(mar = c(4, 14, 1, 2)+0.2)
plot(c(k3tr, k3tn, k3cr, k3cn), c(1,2,3,4), yaxt = "n", pch = 18, ylim = c(0.5,4.5), xlim = c(0, 1.5), xlab = "Average number of correct responses", ylab = "", col = c(1,"grey60", 1, "grey60"))
segments(x0 = ttr$conf.int[1], x1 = ttr$conf.int[2], y0 = 1, y1 = 1)
segments(x0 = ttn$conf.int[1], x1 = ttn$conf.int[2], y0 = 2, y1 = 2, col = "grey60")
segments(x0 = tcr$conf.int[1], x1 = tcr$conf.int[2], y0 = 3, y1 = 3)
segments(x0 = tcn$conf.int[1], x1 = tcn$conf.int[2], y0 = 4, y1 = 4, col = "grey60")
mtext("Treatment group", 2, 0.5, las = 2, at = 1.5)
mtext("Control group", 2, 0.5, las = 2, at = 3.5)
par(xpd = T)

legend(-.4, 0.1, bty = "n", pch = 18, lwd = 1, col = c("grey60", 1), legend = c("Does not remember receipt", "Remembers receipt"))
## you may need to reposition the legend for it to appear, depending on the size and dimensions of the graphics window you want to see

#### Appendix table 7: what moved?
overseasa = glm(correctoverseas_w2 ~ treatment +age +female +white +conservative +labour +libdem +fulltime +region2 +region3 + region4 + region5 +region6 +educationfactor +income1 + income2 + income3 + income4 + income5 + income6 + income7 + income8 + income9 +income10 + income11 + income12 + income13 + income14 +income15, data = bphf, family= binomial(link = "logit")) 
overseasb = glm(correctoverseas_w2 ~ treatment +age +female +white +conservative +labour +libdem +fulltime +region2 +region3 + region4 + region5 +region6 +educationfactor +income1 + income2 + income3 + income4 + income5 + income6 + income7 + income8 + income9 +income10 + income11 + income12 + income13 + income14 +income15 + correctoverseas_w1, data = bphf, family= binomial(link = "logit")) 

defensea = glm(correctdefense_w2 ~ treatment +age +female +white +conservative +labour +libdem +fulltime +region2 +region3 + region4 + region5 +region6 +educationfactor +income1 + income2 + income3 + income4 + income5 + income6 + income7 + income8 + income9 +income10 + income11 + income12 + income13 + income14 +income15, data = bphf, family= binomial(link = "logit")) 
defenseb = glm(correctdefense_w2 ~ treatment +age +female +white +conservative +labour +libdem +fulltime +region2 +region3 + region4 + region5 +region6 +educationfactor +income1 + income2 + income3 + income4 + income5 + income6 + income7 + income8 + income9 +income10 + income11 + income12 + income13 + income14 +income15 + correctdefense_w1, data = bphf, family= binomial(link = "logit")) 

healtha = glm(correcthealth_w2 ~ treatment +age +female +white +conservative +labour +libdem +fulltime +region2 +region3 + region4 + region5 +region6 +educationfactor +income1 + income2 + income3 + income4 + income5 + income6 + income7 + income8 + income9 +income10 + income11 + income12 + income13 + income14 +income15, data = bphf, family= binomial(link = "logit")) 
healthb = glm(correcthealth_w2 ~ treatment +age +female +white +conservative +labour +libdem +fulltime +region2 +region3 + region4 + region5 +region6 +educationfactor +income1 + income2 + income3 + income4 + income5 + income6 + income7 + income8 + income9 +income10 + income11 + income12 + income13 + income14 +income15 + correcthealth_w1, data = bphf, family= binomial(link = "logit")) 

stargazer(overseasa, overseasb, defensea, defenseb, healtha, healthb, omit = c( "age", "female", "white", "conservative", "labour", "libdem", "fulltime", "region2", "region3", "region4", "region5", "region6", "educationfactor", "income1", "income2", "income3", "income4", "income5", "income6", "income7", "income8", "income9", "income10", "income11", "income12", "income13", "income14", "income15", "Constant", "_w1"), covariate.labels = c("Treatment"), style = "ajps", type = "text", dep.var.labels = c("Overseas aid", "Defense", "Health"))

#### Appendix table 10: motivated reasoning differential treatment effects (attitudes)

bphf$partyid_w1 = as.character(levels(bphf$Wave1profile_partyid))[bphf$Wave1profile_partyid]

bphf$partyid_w1[which(bphf$Wave1profile_partyid == "Don't know"  |  bphf$Wave1profile_partyid == "No don\xe4\xf3\xbbt think of myself as any of these")] = "Don't know and none"

bphf$partyid_w1 = as.factor(bphf$partyid_w1)

know.o.cha.mr = polr(knowledgeindex_w2_3item.f ~ treatment + knowledgeindex_w1_3item +age +female +white +conservative +labour +libdem + treatment:conservative +treatment:labour +treatment:libdem +fulltime +region2 +region3 + region4 + region5 +region6 +educationfactor +income1 + income2 + income3 + income4 + income5 + income6 + income7 + income8 + income9 +income10 + income11 + income12 + income13 + income14 +income15, data = bphf, method = "logistic", Hess = T) 

trust.o.cha.mr = polr(trust_w2.f  ~ treatment +trust_w1 +age +female +white +conservative +labour +libdem + treatment:conservative +treatment:labour +treatment:libdem +fulltime +region2 +region3 + region4 + region5 +region6 +educationfactor +income1 + income2 + income3 + income4 + income5 + income6 + income7 + income8 + income9 +income10 + income11 + income12 + income13 + income14 +income15, data = bphf, method = "logistic", Hess = T) 

fair.o.cha.mr = polr(fair_w2.f ~ treatment + fair_w1 +age +female +white +conservative +labour +libdem + treatment:conservative +treatment:labour +treatment:libdem +fulltime +region2 +region3 + region4 + region5 +region6 +educationfactor +income1 + income2 + income3 + income4 + income5 + income6 + income7 + income8 + income9 +income10 + income11 + income12 + income13 + income14 +income15, data = bphf, method = "logistic", Hess = T) 

value.o.cha.mr = polr(value_w2.f ~ treatment + value_w1 +age +female +white +conservative +labour +libdem + treatment:conservative +treatment:labour +treatment:libdem +fulltime +region2 +region3 + region4 + region5 +region6 +educationfactor +income1 + income2 + income3 + income4 + income5 + income6 + income7 + income8 + income9 +income10 + income11 + income12 + income13 + income14 +income15, data = bphf, method = "logistic", Hess = T) 

avoid.o.cha.mr = polr(avoid_w2.f ~ treatment + avoid_w1 +age +female +white +conservative +labour +libdem + treatment:conservative +treatment:labour +treatment:libdem +fulltime +region2 +region3 + region4 + region5 +region6 +educationfactor +income1 + income2 + income3 + income4 + income5 + income6 + income7 + income8 + income9 +income10 + income11 + income12 + income13 + income14 +income15, data = bphf, method = "logistic", Hess = T) 

waste.o.cha.mr = polr(waste_w2_pos.f ~ treatment + avoid_w2 +age +female +white +conservative +labour +libdem + treatment:conservative +treatment:labour +treatment:libdem +fulltime +region2 +region3 + region4 + region5 +region6 +educationfactor +income1 + income2 + income3 + income4 + income5 + income6 + income7 + income8 + income9 +income10 + income11 + income12 + income13 + income14 +income15, data = bphf, method = "logistic", Hess = T) 

## table 10:
stargazer(trust.o.cha.mr, fair.o.cha.mr, value.o.cha.mr, avoid.o.cha.mr, waste.o.cha.mr, omit = c("income", "education", "region", "age", "white", "fulltime","female", "trust", "fair", "value", "avoid"), dep.var.labels = c("Trust", "Fairness", "Value", "Avoidance", "Waste"), style = "ajps", covariate.labels = c("Treatment", "Treatment:Conservative", "Treatment:Labour", "Treatment:Liberal Democrat", "Conservative", "Labour", "Liberal Democrat"), order = c("treatment", "cons", "labour", "libdem"), type = "text")

#### Appendix table 11: motivated reasoning differential treatment effects (preferences)

tax.o.cha.mr = polr(taxyoupay_w2_pos.f ~ treatment + taxyoupay_w1_pos +age +female +white +conservative +labour +libdem + treatment:conservative +treatment:labour +treatment:libdem +fulltime +region2 +region3 + region4 + region5 +region6 +educationfactor +income1 + income2 + income3 + income4 + income5 + income6 + income7 + income8 + income9 +income10 + income11 + income12 + income13 + income14 +income15, data = bphf, method = "logistic", Hess = T) 

top5th.o.cha.mr = polr(top5th_w2.f ~ treatment + top5th_w1 +age +female +white +conservative +labour +libdem + treatment:conservative +treatment:labour +treatment:libdem +fulltime +region2 +region3 + region4 + region5 +region6 +educationfactor +income1 + income2 + income3 + income4 + income5 + income6 + income7 + income8 + income9 +income10 + income11 + income12 + income13 + income14 +income15, data = bphf, method = "logistic", Hess = T) 

bot5th.o.cha.mr = polr(bot5th_w2_pos.f ~ treatment + bot5th_w1_pos +age +female +white +conservative +labour +libdem + treatment:conservative +treatment:labour +treatment:libdem +fulltime +region2 +region3 + region4 + region5 +region6 +educationfactor +income1 + income2 + income3 + income4 + income5 + income6 + income7 + income8 + income9 +income10 + income11 + income12 + income13 + income14 +income15, data = bphf, method = "logistic", Hess = T) 

## table 11
stargazer(tax.o.cha.mr, top5th.o.cha.mr, bot5th.o.cha.mr, omit = c("income", "education", "region", "age", "white", "fulltime","female", "tax", "5th"), dep.var.labels = c("Own tax", "Tax on rich", "Tax on poor"), style = "ajps", covariate.labels = c("Treatment", "Treatment:Conservative", "Treatment:Labour", "Treatment:Liberal Democrat", "Conservative", "Labour", "Liberal Democrat"), order = c("treatment", "cons", "labour", "libdem"), type = "text")


