

library(foreign)
library(benford.analysis)
library(dplyr)
library(CircStats)

usdata <-read.dta(file="covidusmort.dta",convert.dates=TRUE, convert.factors=TRUE, missing.type=FALSE,
                 convert.underscore=FALSE,warn.missing.labels=TRUE)

attach(usdata)

summary(usdata)



usdata$submission_date<-as.Date(usdata$submission_date,format="%m/%d/%Y")
class(usdata$submission_date)

nobs<-length(usdata$submission_date)
nobs

# subset the data so its the first wave as defined in the NY AG report
# march 7 2020 to aug 4 2020

usmortwv1 <- usdata[usdata$submission_date>as.Date("2020-03-06") & usdata$submission_date<as.Date("2020-08-05"), ]


nobs<-length(usmortwv1$new_death)
nobs

head(usmortwv1,n=10)
tail(usmortwv1,n=10)


usmortwv1_ny <- usmortwv1[ usmortwv1$state=="NY",]
usmortwv1_nyc <- usmortwv1[ usmortwv1$state=="NYC",]
usmortwv1_nyall <- usmortwv1[ usmortwv1$state=="NY"|usmortwv1$state=="NYC",]

nobs<-length(usmortwv1_ny$new_death)
nobs

head(usmortwv1_ny,n=10)
tail(usmortwv1_ny,n=20)


usmortwv1_ak <- usmortwv1[ usmortwv1$state=="AK",]
usmortwv1_al <- usmortwv1[ usmortwv1$state=="AL",]
usmortwv1_ar <- usmortwv1[ usmortwv1$state=="AR",]
usmortwv1_az <- usmortwv1[ usmortwv1$state=="AZ",]

usmortwv1_ca <- usmortwv1[ usmortwv1$state=="CA",]
usmortwv1_co <- usmortwv1[ usmortwv1$state=="CO",]
usmortwv1_ct <- usmortwv1[ usmortwv1$state=="CT",]

usmortwv1_dc <- usmortwv1[ usmortwv1$state=="DC",]
usmortwv1_de <- usmortwv1[ usmortwv1$state=="DE",]

usmortwv1_fl <- usmortwv1[ usmortwv1$state=="FL",]
usmortwv1_fsm <- usmortwv1[ usmortwv1$state=="FSM",]

usmortwv1_ga <- usmortwv1[ usmortwv1$state=="GA",]
usmortwv1_gu <- usmortwv1[ usmortwv1$state=="GU",]

usmortwv1_hi <- usmortwv1[ usmortwv1$state=="HI",]

usmortwv1_ia <- usmortwv1[ usmortwv1$state=="IA",]
usmortwv1_id <- usmortwv1[ usmortwv1$state=="ID",]
usmortwv1_il <- usmortwv1[ usmortwv1$state=="IL",]
usmortwv1_in <- usmortwv1[ usmortwv1$state=="IN",]

usmortwv1_ks <- usmortwv1[ usmortwv1$state=="KS",]
usmortwv1_ky <- usmortwv1[ usmortwv1$state=="KY",]

usmortwv1_la <- usmortwv1[ usmortwv1$state=="LA",]

usmortwv1_ma <- usmortwv1[ usmortwv1$state=="MA",]
usmortwv1_md <- usmortwv1[ usmortwv1$state=="MD",]
usmortwv1_me <- usmortwv1[ usmortwv1$state=="ME",]
usmortwv1_mi <- usmortwv1[ usmortwv1$state=="MI",]
usmortwv1_mn <- usmortwv1[ usmortwv1$state=="MN",]
usmortwv1_mo <- usmortwv1[ usmortwv1$state=="MO",]
usmortwv1_mp <- usmortwv1[ usmortwv1$state=="MP",]
usmortwv1_ms <- usmortwv1[ usmortwv1$state=="MS",]
usmortwv1_mt <- usmortwv1[ usmortwv1$state=="MT",]

usmortwv1_nc <- usmortwv1[ usmortwv1$state=="NC",]
usmortwv1_nd <- usmortwv1[ usmortwv1$state=="ND",]
usmortwv1_ne <- usmortwv1[ usmortwv1$state=="NE",]
usmortwv1_nh <- usmortwv1[ usmortwv1$state=="NH",]
usmortwv1_nj <- usmortwv1[ usmortwv1$state=="NJ",]
usmortwv1_nm <- usmortwv1[ usmortwv1$state=="NM",]
usmortwv1_nv <- usmortwv1[ usmortwv1$state=="NV",]

usmortwv1_oh <- usmortwv1[ usmortwv1$state=="OH",]
usmortwv1_ok <- usmortwv1[ usmortwv1$state=="OK",]
usmortwv1_or <- usmortwv1[ usmortwv1$state=="OR",]

usmortwv1_pa <- usmortwv1[ usmortwv1$state=="PA",]
usmortwv1_pr <- usmortwv1[ usmortwv1$state=="PR",]
usmortwv1_pw <- usmortwv1[ usmortwv1$state=="PW",]

usmortwv1_ri <- usmortwv1[ usmortwv1$state=="RI",]
usmortwv1_rmi <- usmortwv1[ usmortwv1$state=="RMI",]

usmortwv1_sc <- usmortwv1[ usmortwv1$state=="SC",]
usmortwv1_sd <- usmortwv1[ usmortwv1$state=="SD",]

usmortwv1_tn <- usmortwv1[ usmortwv1$state=="TN",]
usmortwv1_tx <- usmortwv1[ usmortwv1$state=="TX",]

usmortwv1_ut <- usmortwv1[ usmortwv1$state=="UT",]

usmortwv1_va <- usmortwv1[ usmortwv1$state=="VA",]
usmortwv1_vi <- usmortwv1[ usmortwv1$state=="VI",]
usmortwv1_vt<- usmortwv1[ usmortwv1$state=="VT",]

usmortwv1_wa <- usmortwv1[ usmortwv1$state=="WA",]
usmortwv1_wi <- usmortwv1[ usmortwv1$state=="WI",]
usmortwv1_wv <- usmortwv1[ usmortwv1$state=="WV",]
usmortwv1_wy <- usmortwv1[ usmortwv1$state=="WY",]

# benford law test
# ALL US
benford_cases<-benford(usmortwv1$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))
# test the df
nobs<-length(usmortwv1$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat


# NY
benford_cases<-benford(usmortwv1_ny$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))
# test the df
nobs<-length(usmortwv1_ny$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
# kuiper test 
kuiper(usmortwv1_ny$new_death,alpha=0.1)

# NYC
benford_cases<-benford(usmortwv1_nyc$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))
nobs<-length(usmortwv1_nyc$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
# kuiper test 
kuiper(usmortwv1_nyc$new_death,alpha=0.01)


# NY all
benford_cases<-benford(usmortwv1_nyall$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))



# ak
benford_cases<-benford(usmortwv1_ak$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

nobs<-length(usmortwv1_ak$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
kuiper(usmortwv1_ak$new_death)
                                                                                                             
# AL
benford_cases<-benford(usmortwv1_al$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

nobs<-length(usmortwv1_al$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
kuiper(usmortwv1_al$new_death)



# AR
benford_cases<-benford(usmortwv1_ar$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_ar$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
kuiper(usmortwv1_ar$new_death)



# Az
benford_cases<-benford(usmortwv1_az$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_az$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
kuiper(usmortwv1_az$new_death)



# CA
benford_cases<-benford(usmortwv1_ca$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))


#test the df
nobs<-length(usmortwv1_ca$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
kuiper(usmortwv1_ca$new_death)



# CO
benford_cases<-benford(usmortwv1_co$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_co$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
kuiper(usmortwv1_co$new_death)






# CT
benford_cases<-benford(usmortwv1_ct$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))
#test the df
nobs<-length(usmortwv1_ct$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
kuiper(usmortwv1_ct$new_death)





# de
benford_cases<-benford(usmortwv1_de$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_de$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
kuiper(usmortwv1_de$new_death)


# dc
benford_cases<-benford(usmortwv1_dc$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))


#test the df
nobs<-length(usmortwv1_dc$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
kuiper(usmortwv1_dc$new_death)



# fl
benford_cases<-benford(usmortwv1_fl$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_fl$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
kuiper(usmortwv1_fl$new_death)


# ga
benford_cases<-benford(usmortwv1_ga$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_ga$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
kuiper(usmortwv1_ga$new_death)



# HI
benford_cases<-benford(usmortwv1_hi$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_hi$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
kuiper(usmortwv1_hi$new_death)

#ID
benford_cases<-benford(usmortwv1_id$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))


#test the df
nobs<-length(usmortwv1_id$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat

kuiper(usmortwv1_id$new_death)

#IL
benford_cases<-benford(usmortwv1_il$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))


#test the df
nobs<-length(usmortwv1_il$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
kuiper(usmortwv1_il$new_death)


#IN
benford_cases<-benford(usmortwv1_in$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_in$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat

kuiper(usmortwv1_in$new_death)


# IA
benford_cases<-benford(usmortwv1_ia$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_ia$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat

kuiper(usmortwv1_ia$new_death)

# KS
benford_cases<-benford(usmortwv1_ks$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_ks$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
kuiper(usmortwv1_ks$new_death)



# KY
benford_cases<-benford(usmortwv1_ky$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_ky$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
kuiper(usmortwv1_ky$new_death)



# la
benford_cases<-benford(usmortwv1_la$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_la$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
kuiper(usmortwv1_la$new_death)



# ME
benford_cases<-benford(usmortwv1_me$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_me$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
kuiper(usmortwv1_me$new_death)



# MD
benford_cases<-benford(usmortwv1_md$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))
#test the df
nobs<-length(usmortwv1_md$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
kuiper(usmortwv1_md$new_death)


# MA
benford_cases<-benford(usmortwv1_ma$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_ma$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
kuiper(usmortwv1_ma$new_death)



# MI
benford_cases<-benford(usmortwv1_mi$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_mi$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
kuiper(usmortwv1_mi$new_death)


#MN
benford_cases<-benford(usmortwv1_mn$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_mn$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
kuiper(usmortwv1_mn$new_death)


# MS
benford_cases<-benford(usmortwv1_ms$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_ms$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
kuiper(usmortwv1_ms$new_death)


#MO
benford_cases<-benford(usmortwv1_mo$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_mo$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat

kuiper(usmortwv1_md$new_death)

# MT
benford_cases<-benford(usmortwv1_mt$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_mt$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
kuiper(usmortwv1_mt$new_death)


# NE
benford_cases<-benford(usmortwv1_ne$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_ne$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
kuiper(usmortwv1_ne$new_death)



# NV
benford_cases<-benford(usmortwv1_nv$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_nv$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat

kuiper(usmortwv1_nv$new_death)


# NH
benford_cases<-benford(usmortwv1_nh$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_nh$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat

kuiper(usmortwv1_nh$new_death)



# NJ
benford_cases<-benford(usmortwv1_nj$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))


#test the df
nobs<-length(usmortwv1_nj$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
kuiper(usmortwv1_nj$new_death)


# nm
benford_cases<-benford(usmortwv1_nm$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))


#test the df
nobs<-length(usmortwv1_nm$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
kuiper(usmortwv1_nm$new_death)


# nc
benford_cases<-benford(usmortwv1_nc$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_nc$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat

kuiper(usmortwv1_nc$new_death)

# nd
benford_cases<-benford(usmortwv1_nd$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_nd$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
kuiper(usmortwv1_nd$new_death)




# oh
benford_cases<-benford(usmortwv1_oh$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_oh$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat

kuiper(usmortwv1_oh$new_death)


# ok
benford_cases<-benford(usmortwv1_ok$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_ok$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
kuiper(usmortwv1_ok$new_death)


# or
benford_cases<-benford(usmortwv1_or$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_or$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
kuiper(usmortwv1_or$new_death)


# PA
benford_cases<-benford(usmortwv1_pa$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_pa$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat

kuiper(usmortwv1_pa$new_death)


# Ri
benford_cases<-benford(usmortwv1_ri$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_ri$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
kuiper(usmortwv1_ri$new_death)


# SC
benford_cases<-benford(usmortwv1_sc$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_sc$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat

kuiper(usmortwv1_sc$new_death)


# SD
benford_cases<-benford(usmortwv1_sd$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_sd$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat

kuiper(usmortwv1_sd$new_death)


 
# Tn
benford_cases<-benford(usmortwv1_tn$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_tn$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat

kuiper(usmortwv1_tn$new_death)




# TX
benford_cases<-benford(usmortwv1_tx$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_tx$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
kuiper(usmortwv1_tx$new_death)


# UT 
benford_cases<-benford(usmortwv1_ut$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_ut$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
kuiper(usmortwv1_ut$new_death)



# VA
benford_cases<-benford(usmortwv1_va$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_va$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat

kuiper(usmortwv1_va$new_death)


#VT

benford_cases<-benford(usmortwv1_vt$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_vt$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
kuiper(usmortwv1_vt$new_death)



# WA
benford_cases<-benford(usmortwv1_wa$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_wa$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat

kuiper(usmortwv1_wa$new_death)


# WV
benford_cases<-benford(usmortwv1_wv$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_wv$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
kuiper(usmortwv1_wv$new_death)


# WI
benford_cases<-benford(usmortwv1_wi$new_death,number.of.digits = 1)
benford_cases
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))

#test the df
nobs<-length(usmortwv1_wi$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
kuiper(usmortwv1_wi$new_death)


# WY
benford_cases<-benford(usmortwv1_wy$new_death,number.of.digits = 1)
benford_cases 
format(benford_cases[["bfd"]]$data.dist*100, digits = 2)
plot(benford_cases,except=c("summation", "mantissa", "abs diff", "ex summation"))


#test the df
nobs<-length(usmortwv1_wy$new_death)
nobs
sd_df=0.638253/sqrt(nobs)
teststat<-dfactor(benford_cases)/sd_df
teststat
kuiper(usmortwv1_wy$new_death)


