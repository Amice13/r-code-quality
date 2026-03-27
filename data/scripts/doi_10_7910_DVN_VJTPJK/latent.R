

## Replication file for Van der Maat, Eelco. "Genocidal Consolidation: Final Solutions to Elite Rivalry," International Organization

## R Replication file of analysis I

## Presented in:
## Table 4 in the paper
## Tables A.4 and A.10 of Appendix C and G

## for effect estimations, Paper Table 3, and Appendix Table A.3
## see stata replication file: "analysisI.do"

# set working directory
setwd("/Users/eelco/Dropbox/Academic/Journal submissions/IO 2019/Final Manuscript/IO_GC_replication")


# Before running models, run the following functions:
# 1) run functions.R
source("functions.R")

# 2) run two-stage probit.R
source("two-stage probit.R")

library(foreign)


# read data files
Data <- read.dta("onset_data_R.dta")
Data.ACER <- read.dta("onset_data_R_ACER.dta")


# attach data for ease of analysis
attach(Data)
X1 <- cbind(lgdppcl, lpopl, polity, guerrilla)
X2 <- cbind(lgdppcl, lpopl, polity, guerrilla, tenure, Banks_purge, newleader2)
X  <- cbind(lgdppcl, lpopl, polity, guerrilla, tenure, Banks_purge, newleader2)


y1 <- genonset
y1a <- cgvonset
y2 <- coup_risk

# Table 4, column I — also in Stata
X1a <- cbind(altcoup_risk,lgdppcl,lpopl, polity, guerrilla)
mod1<-probit.clust(X=X1a,y=y1,group=ccode)
summary.probit.nr(mod1)

# Table 4, column II — also in Stata
mod1a<-probit.clust(X=X1a,y=y1a,group=ccode)
summary.probit.nr(mod1a)

# Table 4, column III and IV
# latent genocidal consolidation model; country-clustered SE
lat.gc<-probit.ts(X=X,X1=X1,y1=y1,X2=X2,y2=y2,group=ccode)
# summary
summary.probit.ts(lat.gc)

# Table 4, column III and V 
# latent counter-guerrilla mass violence model; country-clustered SE
lat.cgmv<-probit.ts(X=X,X1=X1,y1=y1a,X2=X2,y2=y2,group=ccode)
# summary
summary.probit.ts(lat.cgmv)

# don't forget to detach
detach()

# number of obs.
nrow(Data)

################################
###                          ###
###  Appendix G:             ###
###  Horizontal Inequality   ###
###                          ###
################################


attach(Data)

X1 <- cbind(lgdppcl, lpopl, polity, guerrilla, discriminate)
X2 <- cbind(lgdppcl, lpopl, polity, guerrilla, discriminate, tenure, Banks_purge, newleader2)
X  <- cbind(lgdppcl, lpopl, polity, guerrilla, discriminate, tenure, Banks_purge, newleader2)


y1 <- genonset
y1a <- cgvonset
y2 <- coup_risk

# Table A.9, column I
X1a <- cbind(altcoup_risk,lgdppcl,lpopl, polity, discriminate, guerrilla)
mod1<-probit.clust(X=X1a,y=y1,group=ccode)
summary.probit.nr(mod1)

# Table A.9, column II
mod1a<-probit.clust(X=X1a,y=y1a,group=ccode)
summary.probit.nr(mod1a)

# Table A.9, column III and IV
# latent genocidal consolidation model; country-clustered SE
lat.gc<-probit.ts(X=X,X1=X1,y1=y1,X2=X2,y2=y2,group=ccode)
# summary
summary.probit.ts(lat.gc)

# Table A.9, column III and V 
# latent counter-guerrilla mass violence model; country-clustered SE
lat.cgmv<-probit.ts(X=X,X1=X1,y1=y1a,X2=X2,y2=y2,group=ccode)
# summary
summary.probit.ts(lat.cgmv)


detach()

nrow(Data)

################################
###                          ###
###  Appendix C:             ###
###  Alt. specs              ###
###                          ###
################################

#
# With ACER Data, Table A.3 Cols. I, II, III
#

attach(na.omit(Data.ACER))

X1 <- cbind(lgdppcl, lpopl, polity, guerrilla)
X2 <- cbind(lgdppcl, lpopl, polity, guerrilla, tenure, purge, newleader2)
X  <- cbind(lgdppcl, lpopl, polity, guerrilla, tenure, purge, newleader2)

y1 <- genonset
y1a <- cgvonset
y2 <- coup_risk



# Table A.3 Cols. I and II
lat.gc<-probit.ts(X=X,X1=X1,y1=y1,X2=X2,y2=y2,group=ccode)
# summary
summary.probit.ts(lat.gc)

# Table A.3 Cols. I and III
lat.cgmv<-probit.ts(X=X,X1=X1,y1=y1a,X2=X2,y2=y2,group=ccode)
# summary
summary.probit.ts(lat.cgmv)

detach()

# nr. obs
nrow(na.omit(Data.ACER))


#
# With Civil Conflict, Table A.3 Cols. IV, V, VI
#

attach(Data)
X1 <- cbind(lgdppcl, lpopl, polity, conflict)
X2 <- cbind(lgdppcl, lpopl, polity, conflict, tenure, Banks_purge, newleader2)
X  <- cbind(lgdppcl, lpopl, polity, conflict, tenure, Banks_purge, newleader2)
      
y1 <- genonset
y1a <- cgvonset
y2 <- coup_risk    


# Table A.3 Cols. IV and V
lat.gc<-probit.ts(X=X,X1=X1,y1=y1,X2=X2,y2=y2,group=ccode)
#model.acv
summary.probit.ts(lat.gc)

# Table A.3 Cols. IV and VI      
lat.cgmv<-probit.ts(X=X,X1=X1,y1=y1a,X2=X2,y2=y2,group=ccode)
#model.cgv
summary.probit.ts(lat.cgmv)
      
detach()
nrow(Data) 
     
#
# With Coup in first stage, Table A.3 Cols. VII, VIII, IX
#

attach(Data)
y1 <- genonset
y1a <- cgvonset
y2a <- coup

X1 <- cbind(lgdppcl, lpopl, polity, guerrilla)
X2 <- cbind(lgdppcl, lpopl, polity, guerrilla, tenure, Banks_purge, newleader2)
X  <- cbind(lgdppcl, lpopl, polity, guerrilla, tenure, Banks_purge, newleader2)


# Table A.3 Cols. VII and VIII
lat.gc<-probit.ts(X=X,X1=X1,y1=y1,X2=X2,y2=y2a,group=ccode)
#summary
summary.probit.ts(lat.gc)

# Table A.3 Cols. VII and IX
lat.cgmv<-probit.ts(X=X,X1=X1,y1=y1a,X2=X2,y2=y2a,group=ccode)
#model.cgv
summary.probit.ts(lat.cgmv)
detach()


