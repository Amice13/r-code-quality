##
#
# Reproducibility material for 'Democracy for the Youth? The Impact of Mock Elections on Voting Age Attitudes'
#
# Erik Gahner Larsen, Klaus Levinsen, Ulrik Kjaer
#
# This file creates the tables and figures in the paper
#
##

# Load packages
library("ggplot2")
library("grid")
library("gridExtra")
library("Zelig") 
library("ZeligMultilevel")
library("ordinal")
library("stargazer")

# Load data
me.survey <- read.csv("me-survey.csv")
me.context <- read.csv("me-context.csv")

# Test for differences between municipalities in turnout and mayor ideology
summary(lm(valgdelt~mem, data=me.context))
summary(lm(leftborg~mem, data=me.context))

# Merge the two datasets to the dataframe valgret
valgret <- merge(me.survey, me.context, by="komnr")

# Create variable with interaction between period and mock election
valgret$mepmem <- valgret$mep*valgret$mem

# Create variable with information on the ratio between turnout in mock elections and turnout in real elections
valgret$valgrel <- valgret$mockdelt/valgret$valgdelt

# Figure 1: Distribution of dependent variable
hist.2009 <- ggplot(valgret[valgret$year == 2009,], aes(x=valgret)) +
  geom_histogram(binwidth=.5, aes(y = (..count..)/sum(..count..)), alpha=.5) +
  scale_x_continuous("2009", breaks = c(1:5), labels = c("Totally\ndisagree","Somewhat\ndisagree","Either/or","Somewhat\nagree","Totally\nagree")) +
  scale_y_continuous("Percent", limits=c(0, .7), breaks = c(0,.1,.2,.3,.4,.5,.6,.7), labels = c("0","10","20","30","40","50","60","70")) +
  theme_minimal()

hist.2013 <- ggplot(valgret[valgret$year == 2013,], aes(x=valgret)) +
  geom_histogram(binwidth=.5, aes(y = (..count..)/sum(..count..)), alpha=.5) +
  scale_x_continuous("2013", breaks = c(1:5), labels = c("Totally\ndisagree","Somewhat\ndisagree","Either/or","Somewhat\nagree","Totally\nagree")) +
  scale_y_continuous("Percent", limits=c(0, .7), breaks = c(0,.1,.2,.3,.4,.5,.6,.7), labels = c("0","10","20","30","40","50","60","70")) +
  theme_minimal()

## Save as .png
png('figure1.png', height=4, width=9, units="in",res=700)
grid.arrange(hist.2009, hist.2013, ncol=2)
dev.off()

## Save as .tiff
tiff('figure1.tiff', height=4, width=9, units="in",res=300)
grid.arrange(hist.2009, hist.2013, ncol=2)
dev.off()

## Estimate regression models
regression.1.ord <- clmm(as.factor(valgret) ~ mep + mem + mepmem + (1|komnr), data = valgret, Hess=TRUE)
regression.2.ord <- clmm(as.factor(valgret) ~ mep + mem + mepmem + male + as.factor(age.cat) + highschool + ideology + (1|komnr), data = valgret, Hess=TRUE)
regression.3.ord <- clmm(as.factor(valgret) ~ mep + mem + mepmem + male + as.factor(age.cat) + highschool + ideology + as.factor(party) + leftborg + child + (1|komnr), data = valgret, Hess=TRUE)
regression.1 <- zelig(valgret.d ~ mep + mem + mepmem + tag(1 | komnr), data = valgret, model = "logit.mixed")
regression.2 <- zelig(valgret.d ~ mep + mem + mepmem + male + as.factor(age.cat) + highschool + ideology + tag(1 | komnr), data = valgret, model = "logit.mixed")
regression.3 <- zelig(valgret.d ~ mep + mem + mepmem + male + as.factor(age.cat) + highschool + ideology + as.factor(party) + leftborg + child + tag(1 | komnr), data = valgret, model = "logit.mixed")

## All information reported in Table 1
summary(regression.1.ord)
summary(regression.2.ord)
summary(regression.3.ord)
summary(regression.1)
summary(regression.2)
summary(regression.3)

# Exclude observations below the age of 22
regression.age <- zelig(valgret.d ~ mep + mem + mepmem + tag(1 | komnr), data = valgret[valgret$age > 22,], model = "logit.mixed")
summary(regression.age)

# Estimate predicted probabilities
x.mem.0 <- setx(regression.3, mep = 1, mem = 0, mepmem = 0, male=1, age.cat=0, highschool=1, child=0, leftborg=1, ideology=5, party=1)
x.mem.1 <- setx(regression.3, mep = 1, mem = 1, mepmem = 1, male=1, age.cat=0, highschool=1, child=0, leftborg=1, ideology=5, party=1)
x.mep.0 <- setx(regression.3, mep = 0, mem = 0, mepmem = 0, male=1, age.cat=0, highschool=1, child=0, leftborg=1, ideology=5, party=1)
x.mep.1 <- setx(regression.3, mep = 0, mem = 1, mepmem = 0, male=1, age.cat=0, highschool=1, child=0, leftborg=1, ideology=5, party=1)

## Simulations (10000 simulations)
results.mem <- sim(regression.1, x = x.mem.0, x1 = x.mem.1, num=10000)
results.mep <- sim(regression.1, x = x.mep.0, x1 = x.mep.1, num=10000)

## Save predicted probabilities
prediction.mem.0 <- results.mem[4]$stats$`Expected Values: E(Y|X)`
prediction.mem.1 <- results.mem[4]$stats$`Expected Values: E(Y|X1)`
prediction.mep.0 <- results.mep[4]$stats$`Expected Values: E(Y|X)`
prediction.mep.1 <- results.mep[4]$stats$`Expected Values: E(Y|X1)`

## Create data frames with the predicted probabilities
d = data.frame(
  x  = c(2,2,1,1),
  y  = c(prediction.mep.0[1], prediction.mep.1[1], prediction.mem.0[1], prediction.mem.1[1]),
  sd = c(prediction.mep.0[2], prediction.mep.1[2], prediction.mem.0[2], prediction.mem.1[2]),
  g = c("Control","MEM","Control","MEM")
)

d2 = data.frame(
  x  = c(2,1), 
  y  = c(prediction.mep.0[1], prediction.mem.0[1]),
  sd = c(prediction.mep.0[2], prediction.mem.0[2]), 
  g = c("Control","MEM")
)

# Create Figure 2: The effect of mock elections (png)
png('figure2.png', height=4, width=5, units="in", res=600)
ggplot(d, aes(x=x, y=y, colour=g, group=g)) +
  geom_point(shape=21, fill="white") + 
  geom_errorbar(width=.05, aes(ymin=y-sd, ymax=y+sd)) +
  geom_errorbar(width=.05, aes(ymin=y-sd, ymax=y+sd), colour="red", data=d2) +
  geom_point(shape=21, size=3, fill="white") +
  scale_x_discrete(name="Mock election period", limits=c("Yes (2009)","No (2013)")) +
  scale_y_continuous(name="Pr(More positive voting age attitude)") +
  coord_cartesian(ylim=c(0,.4), xlim=c(0.9,2.1)) +
  theme_classic() +
  theme(legend.title=element_blank()) 
dev.off()

# Create Figure 2: The effect of mock elections (tiff)
tiff('figure2.tiff', height=4, width=5, units="in", res=300) 
ggplot(d, aes(x=x, y=y, colour=g, group=g)) +
  geom_point(shape=21, fill="white") + 
  geom_errorbar(width=.05, aes(ymin=y-sd, ymax=y+sd)) +
  geom_errorbar(width=.05, aes(ymin=y-sd, ymax=y+sd), colour="red", data=d2) +
  geom_point(shape=21, size=3, fill="white") +
  scale_x_discrete(name="Mock election period", limits=c("Yes (2009)","No (2013)")) +
  scale_y_continuous(name="Pr(More positive voting age attitude)") +
  coord_cartesian(ylim=c(0,.4), xlim=c(0.9,2.1)) +
  theme_classic() +
  theme(legend.title=element_blank()) 
dev.off()

## Estimate predicted probabilities for ideology
regression.ideology <- zelig(valgret.d ~ mep + male + age.cat.1 + age.cat.2 + age.cat.3 + age.cat.4 + age.cat.5 + highschool + ideology + leftborg + child + party.2 + party.3 + party.4 + party.5 + party.6 + party.7 + party.8 + party.9 +tag(1 | komnr), data = na.omit(valgret), model = "logit.mixed")

x.ideology.0 <- setx(regression.ideology, ideology=0)
x.ideology.1 <- setx(regression.ideology, ideology=1)
x.ideology.2 <- setx(regression.ideology, ideology=2)
x.ideology.3 <- setx(regression.ideology, ideology=3)
x.ideology.4 <- setx(regression.ideology, ideology=4)
x.ideology.5 <- setx(regression.ideology, ideology=5)
x.ideology.6 <- setx(regression.ideology, ideology=6)
x.ideology.7 <- setx(regression.ideology, ideology=7)
x.ideology.8 <- setx(regression.ideology, ideology=8)
x.ideology.9 <- setx(regression.ideology, ideology=9)
x.ideology.10 <- setx(regression.ideology, ideology=10)
x.ideology.cov <- setx(regression.ideology, mep = 0, male=1, age.cat.1=0, age.cat.2=0, age.cat.3=0, age.cat.4=0, age.cat.5=0, highschool=1, child=0, leftborg=1, party.2=0, party.3=0, party.4=0, party.5=0, party.6=0, party.7=0, party.8=0, party.9=0)

results.ideology.0 <- sim(regression.ideology, x = x.ideology.0, x1= x.ideology.cov, num=10000)
results.ideology.1 <- sim(regression.ideology, x = x.ideology.1, x1= x.ideology.cov, num=10000)
results.ideology.2 <- sim(regression.ideology, x = x.ideology.2, x1= x.ideology.cov, num=10000)
results.ideology.3 <- sim(regression.ideology, x = x.ideology.3, x1= x.ideology.cov, num=10000)
results.ideology.4 <- sim(regression.ideology, x = x.ideology.4, x1= x.ideology.cov, num=10000)
results.ideology.5 <- sim(regression.ideology, x = x.ideology.5, x1= x.ideology.cov, num=10000)
results.ideology.6 <- sim(regression.ideology, x = x.ideology.6, x1= x.ideology.cov, num=10000)
results.ideology.7 <- sim(regression.ideology, x = x.ideology.7, x1= x.ideology.cov, num=10000)
results.ideology.8 <- sim(regression.ideology, x = x.ideology.8, x1= x.ideology.cov, num=10000)
results.ideology.9 <- sim(regression.ideology, x = x.ideology.9, x1= x.ideology.cov, num=10000)
results.ideology.10 <- sim(regression.ideology, x = x.ideology.10, x1= x.ideology.cov, num=10000)

prediction.ideology.0 <- results.ideology.0[4]$stats$`Expected Values: E(Y|X)`
prediction.ideology.1 <- results.ideology.1[4]$stats$`Expected Values: E(Y|X)`
prediction.ideology.2 <- results.ideology.2[4]$stats$`Expected Values: E(Y|X)`
prediction.ideology.3 <- results.ideology.3[4]$stats$`Expected Values: E(Y|X)`
prediction.ideology.4 <- results.ideology.4[4]$stats$`Expected Values: E(Y|X)`
prediction.ideology.5 <- results.ideology.5[4]$stats$`Expected Values: E(Y|X)`
prediction.ideology.6 <- results.ideology.6[4]$stats$`Expected Values: E(Y|X)`
prediction.ideology.7 <- results.ideology.7[4]$stats$`Expected Values: E(Y|X)`
prediction.ideology.8 <- results.ideology.8[4]$stats$`Expected Values: E(Y|X)`
prediction.ideology.9 <- results.ideology.9[4]$stats$`Expected Values: E(Y|X)`
prediction.ideology.10 <- results.ideology.10[4]$stats$`Expected Values: E(Y|X)`

## Create data frame
pd <- position_dodge(.1)
d3 <- data.frame(
  ideology = c(0:10),
  y = c(prediction.ideology.0[1],prediction.ideology.1[1],prediction.ideology.2[1],prediction.ideology.3[1],prediction.ideology.4[1],prediction.ideology.5[1],prediction.ideology.6[1],prediction.ideology.7[1],prediction.ideology.8[1],prediction.ideology.9[1],prediction.ideology.10[1]),
  sd = c(prediction.ideology.0[2],prediction.ideology.1[2],prediction.ideology.2[2],prediction.ideology.3[2],prediction.ideology.4[2],prediction.ideology.5[2],prediction.ideology.6[2],prediction.ideology.7[2],prediction.ideology.8[2],prediction.ideology.9[2],prediction.ideology.10[2])
)

# Create Figure 3: Predicted probabilities on ideology (png)
png('figure3.png', height=5, width=6, units="in",res=600)
ggplot(d3, aes(x=ideology, y=y)) + 
  geom_smooth(aes(ymin = y-sd, ymax = y+sd), stat="identity") +
  geom_point(colour="blue", alpha=.5, position=pd, size=3) +
  ylab("Pr(More positive voting age attitude)") +
  xlab("Ideology") +
  scale_x_continuous(limits=c(0, 10),breaks=c(0:10), labels=c("Left","","","","","","","","","","Right")) +
  theme_classic() +
  coord_cartesian(ylim=c(0,.7))
dev.off()

# Create Figure 3: Predicted probabilities on ideology (tiff)
tiff('figure3.tiff', height=5, width=6, units="in",res=300) 
ggplot(d3, aes(x=ideology, y=y)) + 
  geom_smooth(aes(ymin = y-sd, ymax = y+sd), stat="identity") +
  geom_point(colour="blue", alpha=.5, position=pd, size=3) +
  ylab("Pr(More positive voting age attitude)") +
  xlab("Ideology") +
  scale_x_continuous(limits=c(0, 10),breaks=c(0:10), labels=c("Left","","","","","","","","","","Right")) +
  theme_classic() +
  coord_cartesian(ylim=c(0,.7))
dev.off()

# Heterogeneous effects
valgret$het.mean <- mean(valgret[valgret$mem == 1,]$mockdelt, na.rm=T)
valgret$het.rel.mean <- mean(valgret[valgret$mem == 1,]$valgrel, na.rm=T)
valgret$ideology.mean <- mean(valgret[valgret$mem == 1,]$ideology, na.rm=T)

## Estimate regressions
regression.het <- zelig(valgret.d ~ mep*I(mockdelt-het.mean) + tag(1 | komnr), data = valgret[valgret$mem == 1,], model = "logit.mixed")
regression.het.rel <- zelig(valgret.d ~ mep*I(valgrel-het.rel.mean) + tag(1 | komnr), data = valgret[valgret$mem == 1,], model = "logit.mixed")
regression.child <- zelig(valgret.d ~ mep*child + tag(1 | komnr), data = valgret[valgret$mem == 1,], model = "logit.mixed")
regression.ide <- zelig(valgret.d ~ mep*I(ideology-ideology.mean) + tag(1 | komnr), data = valgret[valgret$mem == 1,], model = "logit.mixed")

## Results reported in Table 2
summary(regression.het)
summary(regression.het.rel)
summary(regression.child)
summary(regression.ide)

## Descriptive statistics, Online Appendix
writeLines(capture.output(
  stargazer(valgret[valgret$year == 2009,][c("mem","valgret","valgret.d","male","age","highschool","ideology", "child")],
            covariate.labels = c("Mock election municipality", "Voting age attitude", "Voting age attitude (binary)", "Male","Age","High school","Ideology", "Child"), 
            title = "Summary statistics, 2009",
            summary = TRUE,
            type="html")
), "desc_stat_2009.htm")

writeLines(capture.output(
  stargazer(valgret[valgret$year == 2013,][c("mem","valgret","valgret.d","male","age","highschool","ideology", "child")],
            covariate.labels = c("Mock election municipality", "Voting age attitude", "Voting age attitude (binary)", "Male","Age","High school","Ideology", "Child"), 
            title = "Summary statistics, 2013",
            summary = TRUE,
            type="html")
), "desc_stat_2013.htm")

# Create sessionInfo.txt
writeLines(capture.output(sessionInfo()), "sessionInfo.txt")