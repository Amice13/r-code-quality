setwd("/users/danhop/Dropbox/ImmigrationInnumeracy/immiginfo/replication/")
library(foreign)
dta3 <- read.dta("cces2006immig.dta")
dta3$GROUPN <- 99

### guess
dta3$GROUPN[dta3$immcond=="immig, no info"] <- 3
### correct
dta3$GROUPN[dta3$immcond=="immig, info"] <- 4

dta3$IMMSERVN <- NA
dta3$IMMSERVN[dta3$immserv=="0 generally take out more"] <- 0
dta3$IMMSERVN[dta3$immserv=="1"] <- 1
dta3$IMMSERVN[dta3$immserv=="2"] <- 2
dta3$IMMSERVN[dta3$immserv=="3"] <- 3
dta3$IMMSERVN[dta3$immserv=="4"] <- 4
dta3$IMMSERVN[dta3$immserv=="5"] <- 5
dta3$IMMSERVN[dta3$immserv=="6"] <- 6
dta3$IMMSERVN[dta3$immserv=="7"] <- 7
dta3$IMMSERVN[dta3$immserv=="8"] <- 8
dta3$IMMSERVN[dta3$immserv=="9"] <- 9
dta3$IMMSERVN[dta3$immserv=="10 generally put in more"] <- 10

dta3$IMMCULTN <- NA
dta3$IMMCULTN[dta3$immcult=="0 cultural life undermined"] <- 0
dta3$IMMCULTN[dta3$immcult=="1"] <- 1
dta3$IMMCULTN[dta3$immcult=="2"] <- 2
dta3$IMMCULTN[dta3$immcult=="3"] <- 3
dta3$IMMCULTN[dta3$immcult=="4"] <- 4
dta3$IMMCULTN[dta3$immcult=="5"] <- 5
dta3$IMMCULTN[dta3$immcult=="6"] <- 6
dta3$IMMCULTN[dta3$immcult=="7"] <- 7
dta3$IMMCULTN[dta3$immcult=="8"] <- 8
dta3$IMMCULTN[dta3$immcult=="9"] <- 9
dta3$IMMCULTN[dta3$immcult=="10 cultural life enriched"] <- 10

dta3$IMMCRIMEN <- NA
dta3$IMMCRIMEN[dta3$immcrime=="0 made worse"] <- 0
dta3$IMMCRIMEN[dta3$immcrime=="1"] <- 1
dta3$IMMCRIMEN[dta3$immcrime=="2"] <- 2
dta3$IMMCRIMEN[dta3$immcrime=="3"] <- 3
dta3$IMMCRIMEN[dta3$immcrime=="4"] <- 4
dta3$IMMCRIMEN[dta3$immcrime=="5"] <- 5
dta3$IMMCRIMEN[dta3$immcrime=="6"] <- 6
dta3$IMMCRIMEN[dta3$immcrime=="7"] <- 7
dta3$IMMCRIMEN[dta3$immcrime=="8"] <- 8
dta3$IMMCRIMEN[dta3$immcrime=="9"] <- 9
dta3$IMMCRIMEN[dta3$immcrime=="10 made better"] <- 10

dta3$LETIN <- NA
dta3$LETIN[dta3$immlevel=="increased a lot"] <- 5
dta3$LETIN[dta3$immlevel=="increased a little"] <- 4
dta3$LETIN[dta3$immlevel=="left same"] <- 3
dta3$LETIN[dta3$immlevel=="decreased a little"] <- 2
dta3$LETIN[dta3$immlevel=="decreased a lot"] <- 1

dta3$ILLPROB <- NA
dta3$ILLPROB[dta3$illimmprob=="very"] <- 3
dta3$ILLPROB[dta3$illimmprob=="somewhat"] <- 2 
dta3$ILLPROB[dta3$illimmprob=="not too"] <- 1
dta3$ILLPROB[dta3$illimmprob=="not at all"] <- 0

dta3$ILLPROB2 <- dta3$ILLPROB/max(dta3$ILLPROB,na.rm=T)

dta3$IMMPOLICY <- NA
dta3$IMMPOLICY[dta3$immpolicy=="enforcement only"] <- 1
dta3$IMMPOLICY[dta3$immpolicy=="enforcement and guestworker"] <- 0

dta3$TOUGHEN <- NA
dta3$TOUGHEN[dta3$toughen=="support"] <- 1
dta3$TOUGHEN[dta3$toughen=="oppose"] <- 0

dta3$GUESTWORK <- NA
dta3$GUESTWORK[dta3$guestwork=="support"] <- 0
dta3$GUESTWORK[dta3$guestwork=="oppose"] <- 1

dta3$ILLCONS <- NA
dta3$ILLCONS[dta3$illimmconsq=="cost too much" & dta3$illimmconsq2=="strongly"] <- 1
dta3$ILLCONS[dta3$illimmconsq=="cost too much" & dta3$illimmconsq2=="not so strongly"] <- .666
dta3$ILLCONS[dta3$illimmconsq=="become productive" & dta3$illimmconsq2=="not so strongly"] <- .333
dta3$ILLCONS[dta3$illimmconsq=="become productive" & dta3$illimmconsq2=="strongly"] <- 0

dta3$ILLINDEX <- dta3$IMMPOLICY+dta3$ILLPROB2+dta3$TOUGHEN+dta3$ILLCONS+dta3$GUESTWORK
dta3$ILLINFO <- 1*(dta3$immcond=="illegal, info")

dta31 <- dta3[! dta3$race=="hispanic",]

dta31$INDEX <- NA
dta31$SURVEY <- "PMX06"
dta31$GROUPN2 <- dta31$GROUPN

dta31$LETINR <- NA
dta31$LETINR[dta31$LETIN==5] <-1
dta31$LETINR[dta31$LETIN==4] <-2
dta31$LETINR[dta31$LETIN==3] <-3
dta31$LETINR[dta31$LETIN==2] <-4
dta31$LETINR[dta31$LETIN==1] <-5

dta31$EDYEARS <- NA
dta31$EDYEARS[dta31$educ=="no hs"] <- 8
dta31$EDYEARS[dta31$educ=="high school graduate"] <- 12
dta31$EDYEARS[dta31$educ=="some college"] <- 13
dta31$EDYEARS[dta31$educ=="2-year"] <- 14
dta31$EDYEARS[dta31$educ=="4-year"] <- 16
dta31$EDYEARS[dta31$educ=="post-grad"] <- 19

dta31$PID <- NA
dta31$PID[dta31$pid7=="strong democrat"] <- 1
dta31$PID[dta31$pid7=="weak democrat"] <- 2
dta31$PID[dta31$pid7=="lean democrat"] <- 3
dta31$PID[dta31$pid7 %in% c("independent","not sure")] <- 4
dta31$PID[dta31$pid7=="lean republican"] <- 5
dta31$PID[dta31$pid7=="weak republican"] <- 6
dta31$PID[dta31$pid7=="strong republican"] <- 7

dta31$IDEO <- NA
dta31$IDEO[dta31$ideo5=="very liberal"] <- 2
dta31$IDEO[dta31$ideo5=="liberal"] <- 3
dta31$IDEO[dta31$ideo5 %in% c("moderate","not sure")] <- 4
dta31$IDEO[dta31$ideo5=="conservative"] <- 5
dta31$IDEO[dta31$ideo5=="very conservative"] <- 6

dta31$EMPLOYED <- 1*(dta31$employ %in% c("full-time","part-time"))

dta31$BLACK <- 1*(dta31$race=="black")

dta31$NATGUESS <- NA
dta31$NATGUESS <- dta31$noimbro 

#######
## load original data (with geo-codes)

#load("tessoriginal091708.Rdata")
### subset necessary variables
#vars <- c("G_DAT","THREAT2","HISP","LETIN3","TAKEJOBS","PARTYID7","PPINCIMP","Q10")
#dtsub <- subset(dt,select=vars)
### save subset of interest
#save(dtsub,file="tess-subset-03202018.Rdata")

load("/users/danhop/Dropbox/ImmigrationInnumeracy/immiginfo/replication/tess-subset-03202018.Rdata")
dt <- dtsub
dt$THREATEN <- dt$THREAT2

dt$NATGUESS <- NA
dt$NATGUESS[dt$G_DAT=="G3"] <- dt$Q10[dt$G_DAT=="G3"]

dt$LOCGUESS <- NA
dt$LOCGUESS[dt$G_DAT=="G4"] <- dt$Q10[dt$G_DAT=="G4"]

### zip code measure of pc. immigrant in 2000
#xloc <- cbind(dt$LOCGUESS,dt$ZPCIMM00)
#write.csv(xloc,"locaguesses08.csv",row.names=F)

dt$GROUPN <- 99
#### 1 = natl info
dt$GROUPN[dt$G_DAT=="G1"] <- 1
#### 3 = natl guess
dt$GROUPN[dt$G_DAT=="G3"] <- 3

dt$GROUPL <- 99
#### 2 = local info
dt$GROUPL[dt$G_DAT=="G2"] <- 2
#### 4 = local guess
dt$GROUPL[dt$G_DAT=="G4"] <- 4

dt2 <- dt[dt$HISP==0,]

dt2$LETINR <- NA
dt2$LETINR[dt2$LETIN3==1] <- 5
dt2$LETINR[dt2$LETIN3==2] <- 4
dt2$LETINR[dt2$LETIN3==3] <- 3
dt2$LETINR[dt2$LETIN3==4] <- 2
dt2$LETINR[dt2$LETIN3==5] <- 1

dt2$INDEX <- dt2$TAKEJOBS+dt2$THREATEN+dt2$LETINR

dt2$PID <- NA
dt2$PID[dt2$PARTYID7=="Strong Republican"] <- 7
dt2$PID[dt2$PARTYID7=="Not Strong Republican"] <- 6
dt2$PID[dt2$PARTYID7=="Leans Republican"] <- 5
dt2$PID[dt2$PARTYID7=="Undecided/Independent/Other"] <- 4
dt2$PID[dt2$PARTYID7=="Leans Democrat"] <- 3
dt2$PID[dt2$PARTYID7=="Not Strong Democrat"] <- 2
dt2$PID[dt2$PARTYID7=="Strong Democrat"] <- 1

dt2$INCOME <- NA
dt2$INCOME[dt2$PPINCIMP=="Less than $5,000"] <- 2.5
dt2$INCOME[dt2$PPINCIMP=="$5,000 to $7,499"] <- 6.25
dt2$INCOME[dt2$PPINCIMP=="$7,500 to $9,999"] <- 8.75
dt2$INCOME[dt2$PPINCIMP=="$10,000 to $12,499"] <- 11.25
dt2$INCOME[dt2$PPINCIMP=="$12,500 to $14,999"] <- 12.75
dt2$INCOME[dt2$PPINCIMP=="$15,000 to $19,999"] <- 17.5
dt2$INCOME[dt2$PPINCIMP=="$20,000 to $24,999"] <- 22.5
dt2$INCOME[dt2$PPINCIMP=="$25,000 to $29,999"] <- 27.5
dt2$INCOME[dt2$PPINCIMP=="$30,000 to $34,999"] <- 32.5
dt2$INCOME[dt2$PPINCIMP=="$35,000 to $39,999"] <- 37.5

dt2$INCOME[dt2$PPINCIMP=="$40,000 to $49,999"] <- 45
dt2$INCOME[dt2$PPINCIMP=="$50,000 to $59,999"] <- 55
dt2$INCOME[dt2$PPINCIMP=="$60,000 to $74,999"] <- 67.5
dt2$INCOME[dt2$PPINCIMP=="$75,000 to $84,999"] <- 80
dt2$INCOME[dt2$PPINCIMP=="$85,000 to $99,999"] <- 92.5
dt2$INCOME[dt2$PPINCIMP=="$100,000 to $124,999"] <- 125.5
dt2$INCOME[dt2$PPINCIMP=="$125,000 to $149,999"] <- 137.5
dt2$INCOME[dt2$PPINCIMP=="$150,000 to $174,999"] <- 162.5
dt2$INCOME[dt2$PPINCIMP=="$175,000 or more"] <- 250

######## load 2010 KN data
dta <- read.spss("Georgetown_ImmigrationQuickview_Client.sav",to.data.frame=T)

dta$INCOME <- NA
dta$INCOME[dta$PPINCIMP=="Less than $5,000"] <- 2.5
dta$INCOME[dta$PPINCIMP=="$5,000 to $7,499"] <- 6.25
dta$INCOME[dta$PPINCIMP=="$7,500 to $9,999"] <- 8.75
dta$INCOME[dta$PPINCIMP=="$10,000 to $12,499"] <- 11.25
dta$INCOME[dta$PPINCIMP=="$12,500 to $14,999"] <- 12.75
dta$INCOME[dta$PPINCIMP=="$15,000 to $19,999"] <- 17.5
dta$INCOME[dta$PPINCIMP=="$20,000 to $24,999"] <- 22.5
dta$INCOME[dta$PPINCIMP=="$25,000 to $29,999"] <- 27.5
dta$INCOME[dta$PPINCIMP=="$30,000 to $34,999"] <- 32.5
dta$INCOME[dta$PPINCIMP=="$35,000 to $39,999"] <- 37.5

dta$INCOME[dta$PPINCIMP=="$40,000 to $49,999"] <- 45
dta$INCOME[dta$PPINCIMP=="$50,000 to $59,999"] <- 55
dta$INCOME[dta$PPINCIMP=="$60,000 to $74,999"] <- 67.5
dta$INCOME[dta$PPINCIMP=="$75,000 to $84,999"] <- 80
dta$INCOME[dta$PPINCIMP=="$85,000 to $99,999"] <- 92.5
dta$INCOME[dta$PPINCIMP=="$100,000 to $124,999"] <- 125.5
dta$INCOME[dta$PPINCIMP=="$125,000 to $149,999"] <- 137.5
dta$INCOME[dta$PPINCIMP=="$150,000 to $174,999"] <- 162.5
dta$INCOME[dta$PPINCIMP=="$175,000 or more"] <- 250

dta$LETIN <- NA
dta$LETIN[dta$Q4=="Increased a lot"] <- 5
dta$LETIN[dta$Q4=="Increased a little"] <- 4
#dta$LETIN[dta$Q4=="Left the same" | dta$Q4=="I'm not sure"] <- 3
dta$LETIN[dta$Q4=="Left the same"] <- 3
dta$LETIN[dta$Q4=="Decreased a little"] <- 2
dta$LETIN[dta$Q4=="Decreased a lot"] <- 1

dta$LETINR <- NA
dta$LETINR[dta$Q4=="Increased a lot"] <- 1
dta$LETINR[dta$Q4=="Increased a little"] <- 2
#dta$LETINR[dta$Q4=="Left the same" | dta$Q4=="I'm not sure"] <- 3
dta$LETINR[dta$Q4=="Left the same"] <- 3
dta$LETINR[dta$Q4=="Decreased a little"] <- 4
dta$LETINR[dta$Q4=="Decreased a lot"] <- 5

dta$DK <- (dta$Q4=="I'm not sure")*1

dta$TAKEJOBS <- NA
dta$TAKEJOBS[dta$Q5=="Extremely likely"] <- 4
dta$TAKEJOBS[dta$Q5=="Very likely"] <- 3
dta$TAKEJOBS[dta$Q5=="Somewhat likely"] <- 2
dta$TAKEJOBS[dta$Q5=="Not at all likely"] <- 1

dta$THREATEN <- NA
dta$THREATEN[dta$Q6=="Extremely likely"] <- 4
dta$THREATEN[dta$Q6=="Very likely"] <- 3
dta$THREATEN[dta$Q6=="Somewhat likely"] <- 2
dta$THREATEN[dta$Q6=="Not at all likely"] <- 1

dta$NATGUESS <- as.numeric(as.character(dta$Q2))

dta$GROUPN <- as.numeric(dta$GROUP) 
cor(dta$THREATEN,dta$TAKEJOBS,use="pairwise.complete.obs")
cor(dta$THREATEN,dta$LETIN,use="pairwise.complete.obs")

dta$INDEX <- dta$THREATEN+dta$TAKEJOBS+dta$LETINR

dta$BLACK <- 0
dta$BLACK[dta$PPETHM=="Black, Non-Hispanic"] <- 1

dta$MALE <- 0
dta$MALE[dta$PPGENDER=="Male"] <- 1

dta$PID <- NA
dta$PID[dta$xparty7=="Strong Republican"] <- 7
dta$PID[dta$xparty7=="Not Strong Republican"] <- 6
dta$PID[dta$xparty7=="Leans Republican"] <- 5
dta$PID[dta$xparty7=="Undecided/Independent/Other"] <- 4
dta$PID[dta$xparty7=="Leans Democrat"] <- 3
dta$PID[dta$xparty7=="Not Strong Democrat"] <- 2
dta$PID[dta$xparty7=="Strong Democrat"] <- 1

dta$EDYEARS <- NA
dta$EDYEARS[dta$PPEDUC=="No formal education"] <- 0 
dta$EDYEARS[dta$PPEDUC=="5th or 6th grade"] <- 6
dta$EDYEARS[dta$PPEDUC=="7th or 8th grade"] <- 8
dta$EDYEARS[dta$PPEDUC=="9th grade"] <- 9
dta$EDYEARS[dta$PPEDUC=="10th grade"] <- 10
dta$EDYEARS[dta$PPEDUC=="11th grade"] <- 11
dta$EDYEARS[dta$PPEDUC=="12th grade NO DIPLOMA"] <- 11.5
dta$EDYEARS[dta$PPEDUC=="HIGH SCHOOL GRADUATE - high school DIPLOMA or the equivalent (GED)"] <- 12
dta$EDYEARS[dta$PPEDUC=="Some college, no degree"] <- 13
dta$EDYEARS[dta$PPEDUC=="Associate degree"] <- 14
dta$EDYEARS[dta$PPEDUC=="Bachelors degree"] <- 16
dta$EDYEARS[dta$PPEDUC=="Masters degree"] <- 18
dta$EDYEARS[dta$PPEDUC=="Professional or Doctorate degree"] <- 20

dta$GROUPN2 <- NA
dta$GROUPN2[dta$GROUP=="Group C (Information)"] <- 1
dta$GROUPN2[dta$GROUP=="Group A (CONTROL)"] <- 2
dta$GROUPN2[dta$GROUP=="Group B (Guess)"] <- 3
dta$GROUPN2[dta$GROUP=="Group D (Correction)"] <- 4

dta$EMPLOYED <- 1*(dta$PPWORK %in% c("Working - as a paid employee","Working - self-employed"))

dta$GUESSQ <- cut(dta$NATGUESS,breaks=quantile(dta$NATGUESS,c(0,.25,.5,.75,1),na.rm=T),labels=c(1,2,3,4))

### interaction with guess
dta$CORRECT <- 1*(dta$GROUPN2==4)
dta2 <- dta[! dta$PPETHM=="Hispanic",]

######
######

dta10 <- read.dta("cces2010immig.dta")

dta10$LETIN <- NA
dta10$LETIN[dta10$gwu014=="increased a lot"] <- 5
dta10$LETIN[dta10$gwu014=="increased a little"] <- 4
#dta$LETIN[dta10$gwu014=="left the same" | dta10$gwu014=="i don’t know"] <- 3
dta10$LETIN[dta10$gwu014=="left the same"] <- 3
dta10$LETIN[dta10$gwu014=="decreased a little"] <- 2
dta10$LETIN[dta10$gwu014=="decreased a lot"] <- 1

dta10$LETINR <- NA
dta10$LETINR[dta10$gwu014=="increased a lot"] <- 1
dta10$LETINR[dta10$gwu014=="increased a little"] <- 2
#dta$LETINR[dta10$gwu014=="Left the same" | dta$Q4=="I'm not sure"] <- 3
dta10$LETINR[dta10$gwu014=="left the same"] <- 3
dta10$LETINR[dta10$gwu014=="decreased a little"] <- 4
dta10$LETINR[dta10$gwu014=="decreased a lot"] <- 5

dta$DK <- (dta$Q4=="I'm not sure")*1

dta10$TAKEJOBS <- NA
dta10$TAKEJOBS[dta10$gwu015=="extremely likely"] <- 4
dta10$TAKEJOBS[dta10$gwu015=="very likely"] <- 3
dta10$TAKEJOBS[dta10$gwu015=="somewhat likely"] <- 2
dta10$TAKEJOBS[dta10$gwu015=="not at all likely"] <- 1

dta10$THREATEN <- NA
dta10$THREATEN[dta10$gwu016=="extremely likely"] <- 4
dta10$THREATEN[dta10$gwu016=="very likely"] <- 3
dta10$THREATEN[dta10$gwu016=="somewhat likely"] <- 2
dta10$THREATEN[dta10$gwu016=="not at all likely"] <- 1

dta10$INDEX <- dta10$THREATEN+dta10$LETINR+dta10$TAKEJOBS

dta10$GROUPN <- as.numeric(dta10$gwu011) 

dta10$GROUPN2 <- NA
dta10$GROUPN2[dta10$gwu011=="information"] <- 1
dta10$GROUPN2[dta10$gwu011=="control"] <- 2
dta10$GROUPN2[dta10$gwu011=="guessing"] <- 3
dta10$GROUPN2[dta10$gwu011=="correcting"] <- 4

dta10$NATGUESS <- NA
dta10$NATGUESS[dta10$gwu012 < 101 & ! dta10$gwu012 %in% c(NA)] <- dta10$gwu012[dta10$gwu012 < 101 & ! dta10$gwu012 %in% c(NA)]

dta10$BLACK <- 1*(dta10$race=="black")

dta10$PID <- NA
dta10$PID[dta10$pid7=="strong democrat"] <- 1
dta10$PID[dta10$pid7=="not very strong democrat"] <- 2
dta10$PID[dta10$pid7=="lean democrat"] <- 3
dta10$PID[dta10$pid7=="independent"] <- 4
dta10$PID[dta10$pid7=="lean republican"] <- 5
dta10$PID[dta10$pid7=="not very strong republican"] <- 6
dta10$PID[dta10$pid7=="strong republican"] <- 7

dta10$EDYEARS <- NA
dta10$EDYEARS[dta10$educ=="no hs"] <- 8
dta10$EDYEARS[dta10$educ=="high school graduate"] <- 12
dta10$EDYEARS[dta10$educ=="some college"] <- 13
dta10$EDYEARS[dta10$educ=="2-year"] <- 14
dta10$EDYEARS[dta10$educ=="4-year"] <- 16
dta10$EDYEARS[dta10$educ=="post-grad"] <- 19

dta10$IDEO <- NA
dta10$IDEO[dta10$ideo7=="very liberal"] <- 1
dta10$IDEO[dta10$ideo7=="liberal"] <- 2
dta10$IDEO[dta10$ideo7=="somewhat liberal"] <- 3
dta10$IDEO[dta10$ideo7=="middle of the road"] <- 4
dta10$IDEO[dta10$ideo7=="somewhat conservative"] <- 5
dta10$IDEO[dta10$ideo7=="conservative"] <- 6
dta10$IDEO[dta10$ideo7=="very conservative"] <- 7
#dta10$IDEO[dta10$ideo7==""] <- 

dta10$EMPLOYED <- 1*(dta10$employ %in% c("full-time","part-time"))

dta10$GUESSQ <- cut(dta10$NATGUESS,breaks=quantile(dta$NATGUESS,c(0,.25,.5,.75,1),na.rm=T),labels=c(1,2,3,4))
dta10$CORRECT <- 1*(dta10$GROUPN2==4)
dta10$MALE <- 1*(dta10$gender=="male")

dta20 <- dta10[! dta10$race=="hispanic",]

#####
##### treatment

dt2$GROUPN2 <- NA
#info
dt2$GROUPN2[dt2$G_DAT=="G1"] <- 1
#guess
dt2$GROUPN2[dt2$G_DAT=="G3"] <- 3

dt2$GUESS <- 1*(dt2$GROUPN2==3)
dt2$INFO <- 1*(dt2$GROUPN2==1)

dta31$GUESS <- NA
dta31$GUESS[dta31$GROUPN2==3] <- 1
dta31$GUESS[dta31$GROUPN2==4] <- 0

dta31$CORRECT <- NA
dta31$CORRECT[dta31$GROUPN2==3] <- 0
dta31$CORRECT[dta31$GROUPN2==4] <- 1

dta2$GUESS <- NA
dta2$GUESS[dta2$GROUPN2==3] <- 1
dta2$GUESS[! dta2$GROUPN2==3] <- 0

dta2$CORRECT <- NA
dta2$CORRECT[dta2$GROUPN2==4] <- 1
dta2$CORRECT[! dta2$GROUPN2==4] <- 0

dta2$CONTROL <- NA
dta2$CONTROL[dta2$GROUPN2==2] <- 1
dta2$CONTROL[! dta2$GROUPN2==2] <- 0

dta2$INFO <- NA
dta2$INFO[dta2$GROUPN2==1] <- 1
dta2$INFO[! dta2$GROUPN2==1] <- 0

#####
dta20$GUESS <- NA
dta20$GUESS[dta20$GROUPN2==3] <- 1
dta20$GUESS[! dta20$GROUPN2==3] <- 0

dta20$CORRECT <- NA
dta20$CORRECT[dta20$GROUPN2==4] <- 1
dta20$CORRECT[! dta20$GROUPN2==4] <- 0

dta20$CONTROL <- NA
dta20$CONTROL[dta20$GROUPN2==2] <- 1
dta20$CONTROL[! dta20$GROUPN2==2] <- 0

dta20$INFO <- NA
dta20$INFO[dta20$GROUPN2==1] <- 1
dta20$INFO[! dta20$GROUPN2==1] <- 0

##### LOAD MC DATA
dtaMC <- read.csv("/users/danhop/Dropbox/ImmigrationInnumeracy/immiginfo/replication/MCdata.csv")

dtaMC$CONTROL <- 1*(dtaMC$group=="A")
dtaMC$GUESS <- 1*(dtaMC$group=="B")
dtaMC$INFO <- 1*(dtaMC$group=="C")
dtaMC$CORRECT <- 1*(dtaMC$group=="D")

dtaMC$PID <- NA
dtaMC$PID[dtaMC$xpid3==1] <- 1
dtaMC$PID[dtaMC$xpid3==2] <- 2
dtaMC$PID[dtaMC$xpid3==3] <- 3

dtaMC$LETINR <- NA
dtaMC$LETINR[dtaMC$EXP4==1] <- 1
dtaMC$LETINR[dtaMC$EXP4==2] <- 2
dtaMC$LETINR[dtaMC$EXP4==3] <- 3
dtaMC$LETINR[dtaMC$EXP4==4] <- 4
dtaMC$LETINR[dtaMC$EXP4==5] <- 5

dtaMC$NUMBERPOST <- NA
dtaMC$NUMBERPOST <- as.numeric(as.character(dtaMC$EXP7))
dtaMC$NUMBERPOST[dtaMC$EXP7==",70"] <- 70
dtaMC$NUMBERPOST[dtaMC$EXP7==",14"] <- 14

dtaMC$THREATEN <- NA
dtaMC$THREATEN[dtaMC$EXP6==1] <- 4
dtaMC$THREATEN[dtaMC$EXP6==2] <- 3
dtaMC$THREATEN[dtaMC$EXP6==3] <- 2
dtaMC$THREATEN[dtaMC$EXP6==4] <- 1

dtaMC$TAKEJOBS <- NA
dtaMC$TAKEJOBS[dtaMC$EXP5==1] <- 4
dtaMC$TAKEJOBS[dtaMC$EXP5==2] <- 3
dtaMC$TAKEJOBS[dtaMC$EXP5==3] <- 2
dtaMC$TAKEJOBS[dtaMC$EXP5==4] <- 1

dtaMC$INDEX <- dtaMC$TAKEJOBS+dtaMC$THREATEN+dtaMC$LETINR

