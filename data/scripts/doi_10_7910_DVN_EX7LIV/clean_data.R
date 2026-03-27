## CR Conrad, DW Hill & WH Moore
## To run batch, from shell prompt: R CMD BATCH filename.R

setwd("/Users/danielhill/Documents/torture/JPR R&R/empirics/replication")
library(foreign)

## Read in data, get what we need, remove negative values for CIRI stuff, Assign missing PWT Grade lowest value, drop obs w/ missing data
dat<-read.dta("ConMooTortTypesData14Apr15.dta")
tort.dat<-data.frame(dat$cowcode, dat$year, dat$scarring, dat$stealth, dat$unstated, dat$democracy, dat$linzerstatonJI, dat$linzerstatonPOSTSD, dat$civilwar, log(dat$wdi_gdpc), log(dat$wdi_pop), dat$logHROs, dat$speech, dat$majoritarian, dat$PR, dat$mixed)
colnames(tort.dat)<-c("cowcode", "year", "scar", "stealth", "unstated", "dem", "ji", "ji.sd", "cwar", "gdpc", "pop", "hros", "speech","majoritarian","pr","mixed")

sf.dat<-read.dta(file="lagged_FS_hrscores.dta")
sf.dat<-sf.dat[sf.dat$year>1994,]
sf.dat$todrop<-ifelse(sf.dat$cowcode>666&sf.dat$cowcode<667,1,0)
sf.dat<-sf.dat[sf.dat$todrop==0,]

check.dat<-merge(tort.dat,sf.dat,by=c("cowcode","year"),all.x=T,all.y=T)

check.dat$speech<-ifelse(check.dat$speech<0,NA,check.dat$speech)
nrow(check.dat)
write.csv(check.dat,file="estimation_sample.csv",row.names=F)

