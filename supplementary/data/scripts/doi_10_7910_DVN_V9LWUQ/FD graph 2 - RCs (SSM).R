# Replication File for Figure 2 of "The Influence of Religious-Political 
# Sophistication on U.S. Public Opinion" (Eric R. Schmidt)
# Effect of RPS and Church Attendance on Roman Catholic Support for 
# Same-Sex Marriage 
#-------------------------------------------------------------------------------
rm(list=ls())
setwd("C:/Users/Eric/Dropbox/- Active Projects/Religious Knowledge/Final Revisions - PB Submission/Replication Files/First Difference Graphs")
library(foreign)
our.data<-read.csv("replication-data.csv")
names(our.data)
#------------
# Subset: Evangelical Protestants and Roman Catholics
evan.cath = subset(our.data, mainline != 1 & ambiguous==0)
#------------
set.seed(47408) # This is my zip code...
#------------
# Add the interaction terms in the RPS model to the dataset - this helps Zelig out
evan.cath$interaction1 = evan.cath$partyid*evan.cath$secular.knowledge
evan.cath$interaction2 = evan.cath$church*evan.cath$bible.collapsed
evan.cath$interaction3 = evan.cath$church*evan.cath$secular.knowledge
evan.cath$interaction4 = evan.cath$church*evan.cath$rps.ssm
#------------
# Call up Zelig
library(Zelig)
#------------
# Run the RPS model
mylogit = zelig(data=evan.cath, ssm ~ age + female + black + education.collapsed 
              + income.collapsed + ideology.collapsed
              + romancatholic + partyid + secular.knowledge
              + interaction1 + church + bible.collapsed + interaction2
              + interaction3 + rps.ssm + interaction4, model = "logit")
summary(mylogit)

x.out = setx(mylogit) # Store the central tendencies for the variables
z.out = sim(mylogit, x = x.out) # Begin the simulation process
#------------
# Designate what the control variables should be set to for the simulations 
# There will be different central tendencies depending on the variable
age.1 = mean(evan.cath$age[evan.cath$tradition=="Roman Catholic Church"],
             na.rm=T)
female.1 = median(evan.cath$female[evan.cath$tradition=="Roman Catholic Church"], 
                  na.rm=T)
black.1 = median(evan.cath$black[evan.cath$tradition=="Roman Catholic Church"], 
                 na.rm=T)
education.1 = mean(evan.cath$education.collapsed[evan.cath$tradition=="Roman Catholic Church"], 
                   na.rm=T)
income.1 = mean(evan.cath$income.collapsed[evan.cath$tradition=="Roman Catholic Church"], 
                na.rm=T)
ideology.1 = mean(evan.cath$ideology.collapsed[evan.cath$tradition=="Roman Catholic Church"], 
                  na.rm=T)
pid.1 = mean(evan.cath$partyid[evan.cath$tradition=="Roman Catholic Church"], 
             na.rm=T)
secular.knowledge.1 = mean(evan.cath$secular.knowledge[evan.cath$tradition=="Roman Catholic Church"], 
                           na.rm=T)
bible.1 = mean(evan.cath$bible.collapsed[evan.cath$tradition=="Roman Catholic Church"], 
               na.rm=T)
#-------------------------------------------------------------------------------
# EFFECT OF RPS FOR PEOPLE THAT 'NEVER' ATTEND CHURCH (CHURCH = 0) 
#------------
# Church = 0; RPS = 0
x.low = setx(z.out, 
             age = age.1,
             female = female.1, 
             black = black.1,
             education.collapsed = education.1,
             income.collapsed = income.1,
             ideology.collapsed = ideology.1,
             romancatholic = 1,
             partyid = pid.1,
             secular.knowledge = secular.knowledge.1,
             interaction1 = pid.1*secular.knowledge.1,
             church = 0, 
             bible.collapsed = bible.1,
             interaction2 = 0*bible.1,
             interaction3 = 0*secular.knowledge.1,
             rps.ssm = 0,
             interaction4 = 0*0)
#------------
# Church = 0, RPS = 1 
x.high = setx(z.out, 
             age = age.1,
             female = female.1, 
             black = black.1,
             education.collapsed = education.1,
             income.collapsed = income.1,
             ideology.collapsed = ideology.1,
             romancatholic = 1,
             partyid = pid.1,
             secular.knowledge = secular.knowledge.1,
             interaction1 = pid.1*secular.knowledge.1,
             church = 0, 
             bible.collapsed = bible.1,
             interaction2 = 0*bible.1,
             interaction3 = 0*secular.knowledge.1,
             rps.ssm = 1,
             interaction4 = 0*1)
#------------
# Simulate first difference
s.out = sim(z.out, x = x.low, x1 = x.high)
summary(s.out)
#------------
# Extract the quantities of interest:
LB.1 = as.numeric(quantile(s.out$getqi(qi = "fd", xvalue = "x1") , 0.025))
UB.1 = as.numeric(quantile(s.out$getqi(qi = "fd", xvalue = "x1") , 0.975)) 
PE.1 = as.numeric(quantile(s.out$getqi(qi = "fd", xvalue = "x1") , 0.50))
#-------------------------------------------------------------------------------
# EFFECT OF RPS FOR PEOPLE THAT 'SELDOM' ATTEND CHURCH (CHURCH = 1)
#------------
# Church = 1, RPS = 0
x.low = setx(z.out, 
             age = age.1,
             female = female.1, 
             black = black.1,
             education.collapsed = education.1,
             income.collapsed = income.1,
             ideology.collapsed = ideology.1,
             romancatholic = 1,
             partyid = pid.1,
             secular.knowledge = secular.knowledge.1,
             interaction1 = pid.1*secular.knowledge.1,
             church = 1, 
             bible.collapsed = bible.1,
             interaction2 = 1*bible.1,
             interaction3 = 1*secular.knowledge.1,
             rps.ssm = 0,
             interaction4 = 1*0)
#------------
# Church = 1, RPS = 1
x.high = setx(z.out, 
              age = age.1,
              female = female.1, 
              black = black.1,
              education.collapsed = education.1,
              income.collapsed = income.1,
              ideology.collapsed = ideology.1,
              romancatholic = 1,
              partyid = pid.1,
              secular.knowledge = secular.knowledge.1,
              interaction1 = pid.1*secular.knowledge.1,
              church = 1, 
              bible.collapsed = bible.1,
              interaction2 = 1*bible.1,
              interaction3 = 1*secular.knowledge.1,
              rps.ssm = 1,
              interaction4 = 1*1)
#------------
# Simulate first difference
s.out = sim(z.out, x = x.low, x1 = x.high)
summary(s.out)
#------------
# Extract the quantities of interest:
LB.2 = as.numeric(quantile(s.out$getqi(qi = "fd", xvalue = "x1") , 0.025))
UB.2 = as.numeric(quantile(s.out$getqi(qi = "fd", xvalue = "x1") , 0.975)) 
PE.2 = as.numeric(quantile(s.out$getqi(qi = "fd", xvalue = "x1") , 0.50))
#-------------------------------------------------------------------------------
# EFFECT OF RPS FOR PEOPLE THAT ATTEND CHURCH 'A FEW TIMES A YEAR' (CHURCH = 2)
#------------
# Church = 2, RPS = 0
x.low = setx(z.out, 
             age = age.1,
             female = female.1, 
             black = black.1,
             education.collapsed = education.1,
             income.collapsed = income.1,
             ideology.collapsed = ideology.1,
             romancatholic = 1,
             partyid = pid.1,
             secular.knowledge = secular.knowledge.1,
             interaction1 = pid.1*secular.knowledge.1,
             church = 2, 
             bible.collapsed = bible.1,
             interaction2 = 2*bible.1,
             interaction3 = 2*secular.knowledge.1,
             rps.ssm = 0,
             interaction4 = 2*0)
#------------
# Church = 2, RPS = 1
x.high = setx(z.out, 
              age = age.1,
              female = female.1, 
              black = black.1,
              education.collapsed = education.1,
              income.collapsed = income.1,
              ideology.collapsed = ideology.1,
              romancatholic = 1,
              partyid = pid.1,
              secular.knowledge = secular.knowledge.1,
              interaction1 = pid.1*secular.knowledge.1,
              church = 2, 
              bible.collapsed = bible.1,
              interaction2 = 2*bible.1,
              interaction3 = 2*secular.knowledge.1,
              rps.ssm = 1,
              interaction4 = 2*1)
#------------
# Simulate first difference
s.out = sim(z.out, x = x.low, x1 = x.high)
summary(s.out)
#------------
# Extract the quantities of interest:
LB.3 = as.numeric(quantile(s.out$getqi(qi = "fd", xvalue = "x1") , 0.025))
UB.3 = as.numeric(quantile(s.out$getqi(qi = "fd", xvalue = "x1") , 0.975)) 
PE.3 = as.numeric(quantile(s.out$getqi(qi = "fd", xvalue = "x1") , 0.50))
#-------------------------------------------------------------------------------
# EFFECT OF RPS FOR PEOPLE THAT ATTEND CHURCH 'ONCE OR TWICE A MONTH' (CHURCH = 3)
#------------
# Church = 3, RPS = 0
x.low = setx(z.out, 
             age = age.1,
             female = female.1, 
             black = black.1,
             education.collapsed = education.1,
             income.collapsed = income.1,
             ideology.collapsed = ideology.1,
             romancatholic = 1,
             partyid = pid.1,
             secular.knowledge = secular.knowledge.1,
             interaction1 = pid.1*secular.knowledge.1,
             church = 3, 
             bible.collapsed = bible.1,
             interaction2 = 3*bible.1,
             interaction3 = 3*secular.knowledge.1,
             rps.ssm = 0,
             interaction4 = 3*0)
#------------
# Church = 3, RPS = 1
x.high = setx(z.out, 
              age = age.1,
              female = female.1, 
              black = black.1,
              education.collapsed = education.1,
              income.collapsed = income.1,
              ideology.collapsed = ideology.1,
              romancatholic = 1,
              partyid = pid.1,
              secular.knowledge = secular.knowledge.1,
              interaction1 = pid.1*secular.knowledge.1,
              church = 3, 
              bible.collapsed = bible.1,
              interaction2 = 3*bible.1,
              interaction3 = 3*secular.knowledge.1,
              rps.ssm = 1,
              interaction4 = 3*1)
#------------
# Simulate first difference
s.out = sim(z.out, x = x.low, x1 = x.high)
summary(s.out)
#------------
# Extract the quantities of interest:
LB.4 = as.numeric(quantile(s.out$getqi(qi = "fd", xvalue = "x1") , 0.025))
UB.4 = as.numeric(quantile(s.out$getqi(qi = "fd", xvalue = "x1") , 0.975)) 
PE.4 = as.numeric(quantile(s.out$getqi(qi = "fd", xvalue = "x1") , 0.50))
#-------------------------------------------------------------------------------
# EFFECT OF RPS FOR PEOPLE THAT ATTEND CHURCH 'WEEKLY' (CHURCH = 4)
#------------
# Church = 4, RPS = 0
x.low = setx(z.out, 
             age = age.1,
             female = female.1, 
             black = black.1,
             education.collapsed = education.1,
             income.collapsed = income.1,
             ideology.collapsed = ideology.1,
             romancatholic = 1,
             partyid = pid.1,
             secular.knowledge = secular.knowledge.1,
             interaction1 = pid.1*secular.knowledge.1,
             church = 4, 
             bible.collapsed = bible.1,
             interaction2 = 4*bible.1,
             interaction3 = 4*secular.knowledge.1,
             rps.ssm = 0,
             interaction4 = 4*0)
#------------
# Church = 4, RPS = 1
x.high = setx(z.out, 
              age = age.1,
              female = female.1, 
              black = black.1,
              education.collapsed = education.1,
              income.collapsed = income.1,
              ideology.collapsed = ideology.1,
              romancatholic = 1,
              partyid = pid.1,
              secular.knowledge = secular.knowledge.1,
              interaction1 = pid.1*secular.knowledge.1,
              church = 4, 
              bible.collapsed = bible.1,
              interaction2 = 4*bible.1,
              interaction3 = 4*secular.knowledge.1,
              rps.ssm = 1,
              interaction4 = 4*1)
#------------
# Simulate first difference
s.out = sim(z.out, x = x.low, x1 = x.high)
summary(s.out)
#------------
# Extract the quantities of interest:
LB.5 = as.numeric(quantile(s.out$getqi(qi = "fd", xvalue = "x1") , 0.025))
UB.5 = as.numeric(quantile(s.out$getqi(qi = "fd", xvalue = "x1") , 0.975)) 
PE.5 = as.numeric(quantile(s.out$getqi(qi = "fd", xvalue = "x1") , 0.50))
#-------------------------------------------------------------------------------
# EFFECT OF RPS FOR PEOPLE THAT ATTEND CHURCH 'MORE THAN ONCE WEEKLY' (CHURCH = 5)
#------------
# Church = 5, RPS = 0
x.low = setx(z.out, 
             age = age.1,
             female = female.1, 
             black = black.1,
             education.collapsed = education.1,
             income.collapsed = income.1,
             ideology.collapsed = ideology.1,
             romancatholic = 1,
             partyid = pid.1,
             secular.knowledge = secular.knowledge.1,
             interaction1 = pid.1*secular.knowledge.1,
             church = 5, 
             bible.collapsed = bible.1,
             interaction2 = 5*bible.1,
             interaction3 = 5*secular.knowledge.1,
             rps.ssm = 0,
             interaction4 = 5*0)
#------------
# Church = 5, RPS = 1
x.high = setx(z.out, 
              age = age.1,
              female = female.1, 
              black = black.1,
              education.collapsed = education.1,
              income.collapsed = income.1,
              ideology.collapsed = ideology.1,
              romancatholic = 1,
              partyid = pid.1,
              secular.knowledge = secular.knowledge.1,
              interaction1 = pid.1*secular.knowledge.1,
              church = 5, 
              bible.collapsed = bible.1,
              interaction2 = 5*bible.1,
              interaction3 = 5*secular.knowledge.1,
              rps.ssm = 1,
              interaction4 = 5*1)
#------------
# Simulate first difference
s.out = sim(z.out, x = x.low, x1 = x.high)
summary(s.out)
#------------
# Extract the quantities of interest:
LB.6 = as.numeric(quantile(s.out$getqi(qi = "fd", xvalue = "x1") , 0.025))
UB.6 = as.numeric(quantile(s.out$getqi(qi = "fd", xvalue = "x1") , 0.975)) 
PE.6 = as.numeric(quantile(s.out$getqi(qi = "fd", xvalue = "x1") , 0.50))
#-------------------------------------------------------------------------------
# FIRST DIFFERENCE GRAPH
#------------
# Create vectors with the simulated CI bounds and point estimates
UL = c(UB.1, UB.2, UB.3, UB.4, UB.5, UB.6)
LL = c(LB.1, LB.2, LB.3, LB.4, LB.5, LB.6)
PredictedProb = c(PE.1, PE.2, PE.3, PE.4, PE.5, PE.6)
PredictedProb
# Also create a vector containing the values that the 'church' variable takes on
church = c(0:5)
#------------
# Create data frame with the upper and lower bounds of the confidence intervals,
# the predicted probabilities, and the new 'church' object to correspond with the values
newdata = data.frame(UL, LL, PredictedProb, church)
#------------
# Call up ggplot2 
library(ggplot2)
#------------
# Use ggplot2 to graph the effect of RPS at each level of church attendance,
# based on the Zelig simulated first difference
our.plot = ggplot(newdata, aes(church, PredictedProb, ymin = LL, ymax = UL)) +
  geom_pointrange(size=1) +
  ylab("Change in Probability") +
  # ggtitle("Roman Catholics: Change in Probability of Supporting \n Same-Sex Marriage when Respondent Knows Church Opposition") +
  theme_bw() +
  xlab("Frequency of Church Attendance")
our.plot
our.plot = our.plot + scale_x_continuous(breaks = 0:5, labels=c("Never", "Seldom",
                                                                "A few times \n a year",
                                                                "Once or twice \n a month",
                                                                "Every week",
                                                                "More than \n once weekly"))
our.plot = our.plot + geom_hline(yintercept = 0, lty = 2)
our.plot
#------------
# Save the file to the working directory 
ggsave(plot = our.plot, file = "Fig2.eps", height = 234, width = 177, units = "mm")
#-------------------------------------------------------------------------------
