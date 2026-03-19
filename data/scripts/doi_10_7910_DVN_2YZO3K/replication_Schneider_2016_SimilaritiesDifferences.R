##########################################################################
##                                                                      ##
##  Title  : Replication material for "Real Differences and             ##
##           Overlooked Similarities", Comparative Political Studies    ##
##  Author : Carsten Q. Schneider                                       ##
##  Script : Ioana Elena Oana and Carsten Q. Schneider                  ##
##  Version: 30/01/2016                                                 ##
##                                                                      ##
##########################################################################

rm(list=ls())

library(QCA); library(SetMethods)


# ----------------------
# Babe Ruth Example - producing Table 1: Home Runs, Babe Ruth, 1927
# -----------------------

# In 1927 Babe Ruth hit a record of 60 home runs out of 192 hits. 
# In the same season there were around 14352 hits in the entire league
# (made by the top 100 players) out of which around 654 were home runs
# and 13698 were not.
# We can now generate data reproducing these numbers:

# First, we create a variable recording if a hit was a home run or not:

home<-c(rep(1, 654),rep(0, 13698))

# Second, we create a variable recording if Babe Ruth was the one
# making the hit or not:

babe<-ifelse (home==1, c(rep(1,60),rep(0,594)), c(rep(1, 132), rep(0,13566)))

# Putting the two variables in a dataframe:

df.babe<-data.frame (babe, home)

# Saving the dataframe as a .csv file:

write.csv(df.babe, "babe.csv")

# we can cross-tabulate the two variables:

ct<-table(df.babe$babe,df.babe$home);
ct

# we can get the sum scores:

rs <- margin.table(ct, margin = 1) # for the rows
rs
cs <- margin.table(ct, margin = 2) # for the columns
cs
ts <- margin.table(ct, margin = NULL) # for the total
ts

# and bind them to the table:

csbabe <- rbind(ct, cs)
csbabe <- cbind(csbabe, c(rs,ts))

# give labels:

rownames(csbabe) <- c("not-babe", "babe", "Tot.")
colnames(csbabe) <- c("not-home", "home", "Tot.")

csbabe

# -------------------
# Necessity and Sufficiency Tests
# ------------------

# Parameters of fit for being Babe Ruth as a necessary condition for hitting home runs:
# We use function QCAfit from the SetMethods package

QCAfit(df.babe$babe,df.babe$home, necessity=TRUE)

# And at not being Babe Ruth as a necessary condition for hitting home runs:

QCAfit(1-df.babe$babe,df.babe$home, necessity=TRUE)

# Just to show how STM is taking care of skewed set membership scores
# when assessing set relations, look at the following test:
# Is not being Babe Ruth perhaps necessary for not-hitting home runs?
# Note that ~X is a very big set and ~Y is as well (most players do not 
# hit home runs)

# ~X <- ~Y
QCAfit(1-df.babe$babe,1-df.babe$home, necessity=TRUE)

# We see that both consistency (.99) and coverage (.96) are very high
# However, RoN is very low (.244)
# Hence, STM scholars would not claim that ~X <- ~Y.

# ------------------
# Table 2: Necessity and Sufficiency Tests
# ------------------

# Summary all tests of NECESSITY  -        test passed
QCAfit(1-df.babe$babe,df.babe$home, necessity=TRUE)     #no
QCAfit(1-df.babe$babe,1-df.babe$home, necessity=TRUE)   #no
QCAfit(df.babe$babe,1-df.babe$home, necessity=TRUE)     #no
QCAfit(df.babe$babe,df.babe$home, necessity=TRUE)       #no

# Tests of SUFFICIENCY
QCAfit(1-df.babe$babe,df.babe$home, necessity=FALSE)    #no
QCAfit(1-df.babe$babe,1-df.babe$home, necessity=FALSE)  #yes
QCAfit(df.babe$babe,1-df.babe$home, necessity=FALSE)    #no
QCAfit(df.babe$babe,df.babe$home, necessity=FALSE)      #no

# SUMMARY:
# of all relations between X and Y, STM would find empirical
# support only for the claim that not begin Babe Ruth is
# sufficient for not hitting a home run
# Consistency is very high and about 99% if all cases are 
# covered (all except for the bats by Babe Ruth that were not home runs)

# -----------------
#### Functions proposed by Paine:
# ------------------

## Function for consistency necessity:

paine <- function(x,y) 
{a<-sum(x==1&y==1)
b<-sum(x==1&y==0)
c<-sum(x==0&y==1)
d<-sum(x==0&y==0)
xy<-(a/(a+b))/((a/(a+b))+(c/(c+d)))
nxy<-(c/(c+d))/((c/(c+d))+(a/(a+b)))
xny<-(b/(a+b))/((b/(a+b))+(d/(c+d)))
nxny<-(d/(c+d))/((d/(c+d))+(b/(a+b)))
param<-c(xy,nxy,xny,nxny)
rel<- c("x necessary for y", "~x necessary for y", "x necessary for ~y", "~x necessary for ~y")
nec <- data.frame(rel,param)

return(nec)
}

paine(df.babe$babe,df.babe$home)

## Function for sufficiency:

# Unlike for necessity, Paine does not propose an entirely new measure for consistency sufficiency.
# He, instead, discusses a bivariate regression coefficient combines information from all four cells
# by combining the two consistency sufficiency scores for both the prensence and the absence of the condition.
# This implies that one gets one and the same coefficient for all sufficiency relations between X, Y, and their
# negations.

painesuf <- function(x,y) 
{a<-sum(x==1&y==1)
b<-sum(x==1&y==0)
c<-sum(x==0&y==1)
d<-sum(x==0&y==0)

reg.coef.<-((a/(a+b))-(c/(c+d)))
rel.suf.<-"x or ~x necessary for y or ~y"
suf<- data.frame(rel.suf., reg.coef.)


return(suf)

}

painesuf(df.babe$babe,df.babe$home)

