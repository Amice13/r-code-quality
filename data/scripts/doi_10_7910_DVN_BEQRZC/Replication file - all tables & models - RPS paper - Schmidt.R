# Replication File for "The Influence of Religious-Political Sophistication
# on U.S. Public Opinion" (Eric R. Schmidt)
#-------------------------------------------------------------------------------
rm(list=ls())
setwd("C:/Users/Eric/Dropbox/- Active Projects/Religious Knowledge/Final Revisions - PB Submission/Replication Files/Main Replication Files")
library(foreign)

# This is the main replication file to use for the tables that appear 
# in the article. 
our.data = read.csv("replication-data.csv") 
names(our.data)
# Note: lines 14-24 enable subsetting the data into 
# 1) mainline Protestants, and 2) evangelical Protestants and Roman Catholics.
# The published article includes only respondents with unambiguous affiliation,
# but I mention in a footnote that the results do not substantively change
# when we include ambiguous cases based on born-again status and/or ethnicity. 
# This script allows users to analyze the data with or without the ambiguous 
# cases. Please note, though, that to replicate the findings in the published 
# article, you should run the models only on the unambiguous cases 
# (i.e. the subsetted data will specify that ambiguous==0)

# For each model, I have NOT commented out the specification that is reported 
# in the final version. But the code does include a variety of additional 
# alternative specifications that are referenced in footnotes; these are commented
# out. 
#-------------------------------------------------------------------------------
# Mainline Protestants subset
# Sample of unambiguous cases 
mainliners = subset(our.data, mainline == 1 & ambiguous == 0) 
# Sample including ambiguous cases: 
# mainliners = subset(our.data, mainline == 1) 

# Catholics & Evangelicals subset
# Sample of unambiguous cases
evan.cath = subset(our.data, mainline != 1 & ambiguous==0)
# Sample including ambiguous cases 
# evan.cath = subset(our.data, mainline != 1)
#-------------------------------------------------------------------------------




#-------------------------------------------------------------------------------
# Table 1: SSM (Evangelical Protestants and Roman Catholics) 
#------------
# Secular Model

# Model using the collapsed control variables (shown in paper)
model1 = glm(ssm ~ age + female + black + education.collapsed 
             + income.collapsed + ideology.collapsed + romancatholic 
             + partyid + secular.knowledge + partyid*secular.knowledge, 
             data=evan.cath, family="binomial")
summary(model1)

# Model using the uncollapsed control variables (referenced but not shown) 
# model1 = glm(ssm ~ age + female + black + education 
#             + income + ideology + romancatholic 
#             + partyid + secular.knowledge + partyid*secular.knowledge, 
#             data=evan.cath, family="binomial")
# summary(model1)
#------------
# Naive Model

# Model using the collapsed control variables (shown in paper)
model2 = glm(ssm ~ age + female + black + education.collapsed 
             + income.collapsed + ideology.collapsed + romancatholic 
             + partyid + secular.knowledge + partyid*secular.knowledge 
             + church + bible.collapsed + church*bible.collapsed, 
             data=evan.cath, family="binomial")
summary(model2)

# Model using the uncollapsed control variables (referenced but not shown)
# model2 = glm(ssm ~ age + female + black + education 
#             + income + ideology + romancatholic 
#             + partyid + secular.knowledge + partyid*secular.knowledge 
#             + church + bible + church*bible, 
#             data=evan.cath, family="binomial")
# summary(model2)
#------------
# "Culture Wars" Model 

# Model using the collapsed control variables (shown in paper)
model3 = glm(ssm ~ age + female + black + education.collapsed 
             + income.collapsed + ideology.collapsed + romancatholic 
             + partyid + secular.knowledge + partyid*secular.knowledge
             + church + bible.collapsed + church*bible.collapsed 
             + church*secular.knowledge, 
             data=evan.cath, family="binomial")
summary(model3)

# Model using the uncollapsed control variables (referenced but not shown)
# model3 = glm(ssm ~ age + female + black + education 
#             + income + ideology + romancatholic 
#             + partyid + secular.knowledge + partyid*secular.knowledge
#             + church + bible + church*bible 
#             + church*secular.knowledge, 
#             data=evan.cath, family="binomial")
# summary(model3)
#------------
# RPS Model

# Model using the collapsed control variables (shown in paper)
model4 = glm(ssm ~ age + female + black + education.collapsed 
             + income.collapsed + ideology.collapsed + romancatholic
             + partyid + secular.knowledge + partyid*secular.knowledge
             + church + bible.collapsed + church*bible.collapsed 
             + church*secular.knowledge + rps.ssm + church*rps.ssm, 
             data=evan.cath, family="binomial")
summary(model4)

# Model using the uncollapsed control variables (referenced but not shown)
# model4 = glm(ssm ~ age + female + black + education 
#             + income + ideology + romancatholic
#             + partyid + secular.knowledge + partyid*secular.knowledge
#             + church + bible + church*bible 
#             + church*secular.knowledge + rps.ssm + church*rps.ssm, 
#             data=evan.cath, family="binomial")
# summary(model4)

# Model excluding income and ideology, with collapsed control variables 
# (referenced but not shown)
# model4 = glm(ssm ~ age + female + black + education.collapsed 
#             + romancatholic + partyid + secular.knowledge 
#             + partyid*secular.knowledge + church + bible.collapsed 
#             + church*bible.collapsed + church*secular.knowledge 
#             + rps.ssm + church*rps.ssm, data=evan.cath, family="binomial")
# summary(model4)

# Model excluding income and ideology, with uncollapsed control variables 
# (referenced but not shown)
# model4 = glm(ssm ~ age + female + black + education 
#             + romancatholic + partyid + secular.knowledge 
#             + partyid*secular.knowledge + church + bible 
#             + church*bible + church*secular.knowledge 
#             + rps.ssm + church*rps.ssm, data=evan.cath, 
#             family="binomial")
# summary(model4)
#-------------------------------------------------------------------------------





#-------------------------------------------------------------------------------
# Table 2: Abortion (Evangelical Protestants and Roman Catholics) 
#------------
# Secular Model

# Model using the collapsed control variables (shown in paper)
model1 = glm(abortion ~ age + female + black + education.collapsed 
             + income.collapsed + ideology.collapsed + romancatholic 
             + partyid + secular.knowledge + partyid*secular.knowledge, 
             data=evan.cath, family="binomial")
summary(model1)

# Model using the uncollapsed control variables (referenced but not shown)
# model1 = glm(abortion ~ age + female + black + education 
#             + income + ideology + romancatholic 
#             + partyid + secular.knowledge + partyid*secular.knowledge, 
#             data=evan.cath, family="binomial")
# summary(model1)
#------------
# Naive Model

# Model using the collapsed control variables (shown in paper)
model2 = glm(abortion ~ age + female + black + education.collapsed 
             + income.collapsed + ideology.collapsed + romancatholic 
             + partyid + secular.knowledge + partyid*secular.knowledge
             + church + bible.collapsed + church*bible.collapsed, 
             data=evan.cath, family="binomial")
summary(model2)

# Model using the uncollapsed control variables (referenced but not shown)
# model2 = glm(abortion ~ age + female + black + education 
#             + income + ideology + romancatholic 
#             + partyid + secular.knowledge + partyid*secular.knowledge
#             + church + bible + church*bible, 
#             data=evan.cath, family="binomial")
# summary(model2)
#------------
# "Culture Wars" Model

# Model using the collapsed control variables (shown in paper)
model3 = glm(abortion ~ age + female + black + education.collapsed 
             + income.collapsed + ideology.collapsed + romancatholic 
             + partyid + secular.knowledge + partyid*secular.knowledge
             + church + bible.collapsed + church*bible.collapsed
             + church*secular.knowledge, data=evan.cath, family="binomial")
summary(model3)

# Model using the uncollapsed control variables (referenced but not shown)
# model3 = glm(abortion ~ age + female + black + education 
#             + income + ideology + romancatholic 
#             + partyid + secular.knowledge + partyid*secular.knowledge
#             + church + bible + church*bible
#             + church*secular.knowledge, data=evan.cath, family="binomial")
# summary(model3)
#------------
# RPS Model

# Model using the collapsed control variables (shown in paper)
model4 = glm(abortion ~ age + female + black + education.collapsed 
             + income.collapsed + ideology.collapsed + romancatholic 
             + partyid + secular.knowledge + partyid*secular.knowledge
             + church + bible.collapsed + church*bible.collapsed
             + church*secular.knowledge + rps.abortion + church*rps.abortion, 
             data=evan.cath, family="binomial")
summary(model4)

# Model using the uncollapsed control variables (referenced but not shown)
# model4 = glm(abortion ~ age + female + black + education 
#             + income + ideology + romancatholic 
#             + partyid + secular.knowledge + partyid*secular.knowledge
#             + church + bible + church*bible
#             + church*secular.knowledge + rps.abortion + church*rps.abortion, 
#             data=evan.cath, family="binomial")
# summary(model4)

# RPS model excluding income and ideology, with collapsed control variables 
# (referenced but not shown) 
# model4 = glm(abortion ~ age + female + black + education.collapsed 
#             + romancatholic + partyid + secular.knowledge 
#             + partyid*secular.knowledge + church + bible.collapsed 
#             + church*bible.collapsed + church*secular.knowledge 
#             + rps.abortion + church*rps.abortion,  data=evan.cath, 
#             family="binomial")
# summary(model4)

# RPS model excluding income and ideology, with uncollapsed control variables 
# (referenced but not shown)
# model4 = glm(abortion ~ age + female + black + education 
#             + romancatholic + partyid + secular.knowledge 
#             + partyid*secular.knowledge + church + bible 
#             + church*bible + church*secular.knowledge 
#             + rps.abortion + church*rps.abortion, data=evan.cath, 
#             family="binomial")
# summary(model4)
#-------------------------------------------------------------------------------





#-------------------------------------------------------------------------------
# Table 3: Cross-Over Issue Constraint (Evangelical Protestants and Roman Catholics) 
#------------
# Predicting SSM Attitudes Using Abortion RPS  

# Model using the collapsed control variables (shown in paper)
model1 = glm(ssm ~ age + female + black + education.collapsed 
             + income.collapsed + ideology.collapsed + romancatholic 
             + partyid + secular.knowledge + partyid*secular.knowledge
             + church + bible.collapsed + church*bible.collapsed 
             + church*secular.knowledge + rps.abortion
             + church*rps.abortion, data=evan.cath, family="binomial")
summary(model1)

# Model using the uncollapsed control variables (referenced but not shown)
# model1 = glm(ssm ~ age + female + black + education 
#              + income + ideology + romancatholic 
#              + partyid + secular.knowledge + partyid*secular.knowledge
#              + church + bible + church*bible 
#              + church*secular.knowledge + rps.abortion
#              + church*rps.abortion, data=evan.cath, family="binomial")
# summary(model1)
#------------
# Predicting Abortion Attitudes Using SSM RPS 

# Model using the collapsed control variables (shown in paper)
model2 = glm(abortion ~ age + female + black + education.collapsed 
             + income.collapsed + ideology.collapsed + romancatholic 
             + partyid + secular.knowledge + partyid*secular.knowledge
             + church + bible.collapsed + church*bible.collapsed 
             + church*secular.knowledge + rps.ssm
             + church*rps.ssm, data=evan.cath, family="binomial")
summary(model2)

# Model using the uncollapsed control variables (referenced but not shown)
# model2 = glm(abortion ~ age + female + black + education 
#              + income + ideology + romancatholic 
#              + partyid + secular.knowledge + partyid*secular.knowledge
#              + church + bible + church*bible 
#              + church*secular.knowledge + rps.ssm
#              + church*rps.ssm, data=evan.cath, family="binomial")
# summary(model2)
#-------------------------------------------------------------------------------





#-------------------------------------------------------------------------------
# Table 4: SSM (Mainline Protestants)  
#------------
# Secular Model 

# Model using the collapsed control variables, no demographic controls (shown in paper)
model1 = glm(ssm ~ partyid + secular.knowledge + partyid*secular.knowledge, 
             data=mainliners, family="binomial")
summary(model1)

# Model using the uncollapsed control variables, all demographic controls 
# (referenced but not shown)
# model1 = glm(ssm ~ age + female + black + education 
#             + income + ideology + partyid + secular.knowledge 
#             + partyid*secular.knowledge, data=mainliners, family="binomial")
# summary(model1)

# Model using the collapsed control variables, all demographic controls 
# (referenced but not shown)
# model1 = glm(ssm ~ age + female + black + education.collapsed 
#             + income.collapsed + ideology.collapsed + partyid + secular.knowledge 
#             + partyid*secular.knowledge, data=mainliners, family="binomial")
# summary(model1)
#------------
# Naive Model

# Model using the collapsed control variables, no demographic controls (shown in paper)
model2 = glm(ssm ~ partyid + secular.knowledge 
             + partyid*secular.knowledge + church + bible.collapsed 
             + church*bible.collapsed, data=mainliners, family="binomial")
summary(model2)

# Model using the uncollapsed control variables, all demographic controls 
# (referenced but not shown)
# model2 = glm(ssm ~ age + female + black + education 
#             + income + ideology + partyid + secular.knowledge 
#             + partyid*secular.knowledge + church + bible 
#             + church*bible, data=mainliners, family="binomial")
# summary(model2)

# Model using the collapsed control variables, all demographic controls 
# (referenced but not shown)
# model2 = glm(ssm ~ age + female + black + education.collapsed 
#             + income.collapsed + ideology.collapsed + partyid + secular.knowledge 
#             + partyid*secular.knowledge + church + bible.collapsed 
#             + church*bible.collapsed, data=mainliners, family="binomial")
# summary(model2)
#------------
# "Culture Wars" Model

# Model using the collapsed control variables, no demographic controls (shown in paper)
model3 = glm(ssm ~ partyid + secular.knowledge 
             + partyid*secular.knowledge + church + bible.collapsed 
             + church*bible.collapsed + church*secular.knowledge, 
             data=mainliners, family="binomial")
summary(model3)

# Model using the uncollapsed control variables, all demographic controls 
# (referenced but not shown)
# model3 = glm(ssm ~ age + female + black + education 
#             + income + ideology + partyid + secular.knowledge 
#             + partyid*secular.knowledge + church + bible 
#             + church*bible + church*secular.knowledge, 
#             data=mainliners, family="binomial")
# summary(model3)

# Model using the collapsed control variables, all demographic controls 
# (referenced but not shown)
# model3 = glm(ssm ~ age + female + black + education.collapsed 
#             + income.collapsed + ideology.collapsed + partyid + secular.knowledge 
#             + partyid*secular.knowledge + church + bible.collapsed 
#             + church*bible.collapsed + church*secular.knowledge, 
#             data=mainliners, family="binomial")
# summary(model3)
#------------
# RPS Model

# Model using the collapsed control variables, no demographic controls (shown in paper)
model4 = glm(ssm ~ partyid + secular.knowledge 
             + partyid*secular.knowledge + church + bible.collapsed 
             + church*bible.collapsed + church*secular.knowledge 
             + rps.ssm + church*rps.ssm, 
             data=mainliners, family="binomial")
summary(model4)

# Model using the uncollapsed control variables, all demographic controls 
# (referenced but not shown)
# model4 = glm(ssm ~ age + female + black + education 
#             + income + ideology + partyid + secular.knowledge 
#             + partyid*secular.knowledge + church + bible 
#             + church*bible + church*secular.knowledge 
#             + rps.ssm + church*rps.ssm, 
#             data=mainliners, family="binomial")
# summary(model4)

# Model using the collapsed control variables, all demographic controls 
# (referenced but not shown)
# model4 = glm(ssm ~ age + female + black + education.collapsed 
#             + income.collapsed + ideology.collapsed + partyid + secular.knowledge 
#             + partyid*secular.knowledge + church + bible.collapsed 
#             + church*bible.collapsed + church*secular.knowledge 
#             + rps.ssm + church*rps.ssm, 
#             data=mainliners, family="binomial")
# summary(model4)
#-------------------------------------------------------------------------------





#-------------------------------------------------------------------------------
# Table 5: Abortion (Mainline Protestants) 
#------------
# Secular Model 

# Model using the collapsed control variables, no demographic controls 
# (shown in paper)
model1 = glm(abortion ~ partyid + secular.knowledge 
             + partyid*secular.knowledge, data=mainliners, family="binomial")
summary(model1)

# Model using the uncollapsed control variables, all demographic controls 
# (referenced but not shown)
# model1 = glm(abortion ~ age + female + black + education 
#             + income + ideology + partyid + secular.knowledge 
#             + partyid*secular.knowledge, data=mainliners, family="binomial")
# summary(model1)

# Model using the collapsed control variables, all demographic controls 
# (referenced but not shown)
# model1 = glm(abortion ~ age + female + black + education.collapsed 
#             + income.collapsed + ideology.collapsed + partyid + secular.knowledge 
#             + partyid*secular.knowledge, data=mainliners, family="binomial")
# summary(model1)
#------------
# Naive Model

# Model using the collapsed control variables, no demographic controls (shown in paper)
model2 = glm(abortion ~ partyid + secular.knowledge 
             + partyid*secular.knowledge + church + bible.collapsed 
             + church*bible.collapsed, data=mainliners, family="binomial")
summary(model2)

# Model using the uncollapsed control variables, all demographic controls (referenced but not shown)
# model2 = glm(abortion ~ age + female + black + education 
#             + income + ideology + partyid + secular.knowledge 
#             + partyid*secular.knowledge + church + bible 
#             + church*bible, data=mainliners, family="binomial")
# summary(model2)

# Model using the collapsed control variables, all demographic controls 
# (referenced but not shown)
# model2 = glm(abortion ~ age + female + black + education.collapsed 
#             + income.collapsed + ideology.collapsed + partyid + secular.knowledge 
#             + partyid*secular.knowledge + church + bible.collapsed 
#             + church*bible.collapsed, data=mainliners, family="binomial")
# summary(model2)
#------------
# "Culture Wars" Model 

# Model using the collapsed control variables, no demographic controls (shown in paper)
model3 = glm(abortion ~ partyid + secular.knowledge 
             + partyid*secular.knowledge + church + bible.collapsed 
             + church*bible.collapsed + church*secular.knowledge, 
             data=mainliners, family="binomial")
summary(model3)

# Model using the uncollapsed control variables, all demographic controls 
# (referenced but not shown)
# model3 = glm(abortion ~ age + female + black + education 
#             + income + ideology + partyid + secular.knowledge 
#             + partyid*secular.knowledge + church + bible 
#             + church*bible + church*secular.knowledge, 
#             data=mainliners, family="binomial")
# summary(model3)

# Model using the collapsed control variables, all demographic controls 
# (referenced but not shown)
# model3 = glm(abortion ~ age + female + black + education.collapsed 
#             + income.collapsed + ideology.collapsed + partyid + secular.knowledge 
#             + partyid*secular.knowledge + church + bible.collapsed 
#             + church*bible.collapsed + church*secular.knowledge, 
#             data=mainliners, family="binomial")
# summary(model3)
#------------
# RPS Model

# Model using the collapsed control variables, no demographic controls 
# (shown in paper)
model4 = glm(abortion ~ partyid + secular.knowledge 
             + partyid*secular.knowledge + church + bible.collapsed 
             + church*bible.collapsed + church*secular.knowledge 
             + rps.abortion + church*rps.abortion, 
             data=mainliners, family="binomial")
summary(model4)

# Model using the uncollapsed control variables, all demographic controls 
# (referenced but not shown)
# model4 = glm(abortion ~ age + female + black + education 
#             + income + ideology + partyid + secular.knowledge 
#             + partyid*secular.knowledge + church + bible 
#             + church*bible + church*secular.knowledge 
#             + rps.abortion + church*rps.abortion, 
#             data=mainliners, family="binomial")
# summary(model4)

# Model using the collapsed control variables, all demographic controls 
# (referenced but not shown)
# model4 = glm(abortion ~ age + female + black + education.collapsed 
#             + income.collapsed + ideology.collapsed + partyid + secular.knowledge 
#             + partyid*secular.knowledge + church + bible.collapsed 
#             + church*bible.collapsed + church*secular.knowledge 
#             + rps.abortion + church*rps.abortion, 
#             data=mainliners, family="binomial")
# summary(model4)
#-------------------------------------------------------------------------------





#-------------------------------------------------------------------------------
# RPS Reports and Personal Attitudes 
#------------ 
# Reports of Church Teaching (SSM)
# Take the 'Not Available' out
church.reports.ssm = recode(our.data$church.ssm, 
                            "'Not Available' = NA")
table(church.reports.ssm)

# Reports of Church Teaching (Abortion)
# Take the 'Not Available' out
church.reports.abortion = recode(our.data$church.abortion, 
                                 "'Not Available' = NA")
#------------
# Table 6 
# RPS answer (SSM) and support/opposition to SSM
table1 = prop.table(table(church.reports.ssm[our.data$ambiguous==0], our.data$ssm[our.data$ambiguous==0]), 2)
round(table1, digits = 2)
# Column Totals, RPS (SSM) Answer and SSM Position
table1 = table(church.reports.ssm[our.data$ambiguous==0], our.data$ssm[our.data$ambiguous==0])
sum(table1[, 1]) # Cell counts
sum(table1[, 2]) # Cell counts
#------------
# Table 7 
# RPS answer (abortion) and support/opposition 
# to unrestricted abortion rights
table1 = prop.table(table(church.reports.abortion[our.data$ambiguous==0], 
                          our.data$abortion[our.data$ambiguous==0]), 2)
round(table1, digits = 2)
# Column Totals, RPS (Abortion) Answer and Abortion Position
table1 = table(church.reports.abortion[our.data$ambiguous==0], 
               our.data$abortion[our.data$ambiguous==0])
sum(table1[, 1]) # Cell counts
sum(table1[, 2]) # Cell counts
#------------
# Footnote 17
# RPS answer (abortion) and pro-choice response 
# to 'Standard GOP Exceptions' question
table1 = prop.table(table(church.reports.abortion[our.data$ambiguous==0], 
                          our.data$gop.exceptions[our.data$ambiguous==0]), 2)
round(table1, digits = 2)
# We see that 47 percent of those that favor only allowing abortion 
# in case of rape, incest, or life-of-mother exceptions also say that 
# their church takes that position. 
#-------------------------------------------------------------------------------





#-------------------------------------------------------------------------------
# Table 8: RPS Models (Incorrect RPS) (Evangelical Protestants and Roman Catholics)
#------------
# 'Incorrect' RPS Model (Same-Sex Marriage)

# Model using the collapsed control variables (shown in paper)
inc.ssm = glm(ssm ~ age + female + black + education.collapsed 
              + income.collapsed + ideology.collapsed + romancatholic 
              + partyid + secular.knowledge + partyid*secular.knowledge
              + church + bible.collapsed + church*bible.collapsed 
              + church*secular.knowledge + incorrect.rps.ssm 
              + church*incorrect.rps.ssm, data=evan.cath, family="binomial")
summary(inc.ssm)

# Model using the uncollapsed control variables (referenced but not shown)
# inc.ssm = glm(ssm ~ age + female + black + education
#              + income + ideology + romancatholic 
#              + partyid + secular.knowledge + partyid*secular.knowledge
#              + church + bible + church*bible 
#              + church*secular.knowledge + incorrect.rps.ssm 
#              + church*incorrect.rps.ssm, data=evan.cath, family="binomial")
# summary(inc.ssm)
#------------
# 'Incorrect' RPS Model (Abortion)

# Model using the collapsed control variables (shown in paper)
inc.abortion = glm(abortion ~ age + female + black + education.collapsed 
                   + income.collapsed + ideology.collapsed + romancatholic 
                   + partyid + secular.knowledge + partyid*secular.knowledge
                   + church + bible.collapsed + church*bible.collapsed 
                   + church*secular.knowledge + incorrect.rps.abortion 
                   + church*incorrect.rps.abortion, 
                   data=evan.cath, family="binomial")
summary(inc.abortion)

# Model using the uncollapsed control variables (referenced but not shown)
# inc.abortion = glm(abortion ~ age + female + black + education 
#                  + income + ideology + romancatholic 
#                  + partyid + secular.knowledge + partyid*secular.knowledge
#                  + church + bible + church*bible 
#                  + church*secular.knowledge + incorrect.rps.abortion 
#                  + church*incorrect.rps.abortion, 
#                  data=evan.cath, family="binomial")
# summary(inc.abortion)
#-------------------------------------------------------------------------------





#-------------------------------------------------------------------------------
# Other statistics and correlations
#------------
# Two different new datasets - one with Catholics, one with Evangelicals
catholics = subset(our.data, romancatholic==1 & ambiguous==0)
evangelicals = subset(our.data, romancatholic==0 & mainline==0 & ambiguous==0)
#------------
# What percent of Roman Catholics knew their church's position on SSM?
table1 = prop.table(table(catholics$rps.ssm))
round(table1, digits=3)*100 
#------------
# What percent of evangelical Protestants knew their church's position on SSM?
table1 = prop.table(table(evangelicals$rps.ssm))
round(table1, digits=3)*100
#------------
# What percent of Roman Catholics knew their church's position on abortion?
table1 = prop.table(table(catholics$rps.abortion))
round(table1, digits=3)*100
#------------
# What percent of evangelical Protestants knew their church's position on abortion?
table1 = prop.table(table(evangelicals$rps.abortion))
round(table1, digits=3)*100
#------------
# Correlation between RPS (Abortion) and RPS (SSM): Catholics
library(psych)
X1 = catholics$rps.abortion
X2 = catholics$rps.ssm
data = data.frame(X1, X2)
tetrachoric(x = data, y=NULL, correct=TRUE, smooth=TRUE, global=TRUE, weight=NULL, na.rm=TRUE,
            delete=TRUE) 
#------------
# Correlation between RPS (Abortion) and RPS (SSM): Evangelicals
X1 = evangelicals$rps.abortion
X2 = evangelicals$rps.ssm
data = data.frame(X1, X2)
tetrachoric(x = data, y=NULL, correct=TRUE, smooth=TRUE, global=TRUE, weight=NULL, na.rm=TRUE,
            delete=TRUE) 
#------------
# Correlation between RPS (Abortion) and Biblical Literacy: Catholics
X1 = catholics$rps.abortion
X2 = catholics$bible
biserial(X2, X1) 
#------------
# Correlation between RPS (Abortion) and Biblical Literacy: Evangelicals
X1 = evangelicals$rps.abortion
X2 = evangelicals$bible
biserial(X2, X1) 
#------------
# Correlation between RPS (SSM) and Biblical Literacy: Catholics
X1 = catholics$rps.ssm
X2 = catholics$bible
biserial(X2, X1) 
#------------
# Correlation between RPS (SSM) and Biblical Literacy: Evangelicals
X1 = evangelicals$rps.ssm
X2 = evangelicals$bible
biserial(X2, X1) 
#------------
# Catholics' performance on the biblical literacy test
mean(catholics$bible, na.rm=T) 
sd(catholics$bible, na.rm=T) 
table(catholics$bible)
#------------
# Evangelicals' performance on the biblical literacy test
mean(evangelicals$bible, na.rm=T) 
sd(evangelicals$bible, na.rm=T)
table(evangelicals$bible)
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Footnote 13: RPS Models (Evangelical Protestants and Roman Catholics) 
# for other more specific abortion attitudes (models referenced but not shown)
#------------
# Pro-choice position: rape, incest, life-of-mother exceptions (referenced but not shown)
# model4 = glm(gop.exceptions ~ age + female + black + education.collapsed 
#             + income.collapsed + ideology.collapsed + romancatholic 
#             + partyid + secular.knowledge + partyid*secular.knowledge
#             + church + bible.collapsed + church*bible.collapsed
#             + church*secular.knowledge + rps.abortion + church*rps.abortion, 
#             data=evan.cath, family="binomial")
# summary(model4)
#------------
# Pro-choice position: banning abortion after 20 weeks (referenced but not shown)
# model4 = glm(week.20 ~ age + female + black + education.collapsed 
#             + income.collapsed + ideology.collapsed + romancatholic 
#             + partyid + secular.knowledge + partyid*secular.knowledge
#             + church + bible.collapsed + church*bible.collapsed
#             + church*secular.knowledge + rps.abortion + church*rps.abortion, 
#             data=evan.cath, family="binomial")
# summary(model4)
#------------
# Pro-choice position: employers and insurance coverage (referenced but not shown)
# model4 = glm(insurance ~ age + female + black + education.collapsed 
#             + income.collapsed + ideology.collapsed + romancatholic 
#             + partyid + secular.knowledge + partyid*secular.knowledge
#             + church + bible.collapsed + church*bible.collapsed
#             + church*secular.knowledge + rps.abortion + church*rps.abortion, 
#             data=evan.cath, family="binomial")
# summary(model4)
#------------
# Pro-choice position: federal funds used for abortion (referenced but not shown)
# model4 = glm(federal.funds ~ age + female + black + education.collapsed 
#             + income.collapsed + ideology.collapsed + romancatholic 
#             + partyid + secular.knowledge + partyid*secular.knowledge
#             + church + bible.collapsed + church*bible.collapsed
#             + church*secular.knowledge + rps.abortion + church*rps.abortion, 
#             data=evan.cath, family="binomial")
# summary(model4)
#------------
# Count of Total Pro-Choice Attitudes (referenced but not shown)
# model4 = glm(pro.choice.count ~ age + female + black + education.collapsed 
#             + income.collapsed + ideology.collapsed + romancatholic 
#             + partyid + secular.knowledge + partyid*secular.knowledge
#             + church + bible.collapsed + church*bible.collapsed
#             + church*secular.knowledge + rps.abortion + church*rps.abortion, 
#             data=evan.cath, family="poisson")
# summary(model4)
#-------------------------------------------------------------------------------




