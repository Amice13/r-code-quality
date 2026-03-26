#### djh
#### 2/14/2022

library(xtable)

#### load data
### data obtained via ICPSR, National Asian American Survey, 37380-0001-Data
setwd("~/Dropbox/PDparty/rcode/replication/")
load("37380-0001-Data.rda")

### no, it doesn't seem right
#dtaa$WEIGHT <- NA
#dtaa$WEIGHT[dtaa$NWEIGHTNATIVITY < 40] <- dtaa$NWEIGHTNATIVITY[dtaa$NWEIGHTNATIVITY < 40]
#summary(dtaa$WEIGHT)

### weights NWEIGHTNATIVITY
dtaa <- da37380.0001
dtaa$NY.W1 <- 1*(dtaa$RSTATE=="NY               ")
dtaa$CA.W1 <- 1*(dtaa$RSTATE=="CA               ")
dtaa$FL.W1 <- 1*(dtaa$RSTATE=="FL               ")
dtaa$TX.W1 <- 1*(dtaa$RSTATE=="TX               ")

dtaa$ASIAN.W1 <- 1*(dtaa$RACE=="(1) Asian American")
dtaa$HISPANIC.W1 <- 1*(dtaa$RACE=="(6) Latino")

dtaa$CITIZEN.W1 <- 1*(dtaa$CITIZEN=="(1) Citizen")

dtaa$CHINESE.W1 <- 1*(dtaa$RETHNIC=="(03) Chinese")
dtaa$JAPANESE.W1 <- 1*(dtaa$RETHNIC=="(07) Japanese")
dtaa$INDIAN.W1 <- 1*(dtaa$RETHNIC=="(06) Indian")
dtaa$FILIPINO.W1 <- 1*(dtaa$RETHNIC=="(04) Filipino")
dtaa$KOREAN.W1 <- 1*(dtaa$RETHNIC=="(08) Korean")
dtaa$CAMBODIAN.W1 <- 1*(dtaa$RETHNIC=="(02) Cambodian")

dtaa$MEXICAN.W1 <- 1*(dtaa$RETHNIC=="(41) Mexican")
dtaa$CUBAN.W1 <- 1*(dtaa$RETHNIC=="(47) Cuban")
dtaa$PUERTORICAN.W1 <- 1*(dtaa$RETHNIC=="(57) Puerto Rican")

dtaa$FEMALE.W1 <- 1*(dtaa$S7=="(2) Female")
dtaa$BORNUS.W1 <- 1*(dtaa$S9=="(1) United States")

dtaa$EDYEARS.W1 <- NA
dtaa$EDYEARS.W1[dtaa$S8=="(1) No schooling completed"] <- 0
dtaa$EDYEARS.W1[dtaa$S8=="(2) Some schooling, no high school degree/GED"] <- 8
dtaa$EDYEARS.W1[dtaa$S8=="(3) High school degree/GED"] <- 12
dtaa$EDYEARS.W1[dtaa$S8=="(4) Some college, but no degree"] <- 14
dtaa$EDYEARS.W1[dtaa$S8=="(5) College degree or Bachelor's degree"] <- 16
dtaa$EDYEARS.W1[dtaa$S8=="(6) Graduate or Professional degree"] <- 19

dtaa$ENGLISH.W1 <- 1*(dtaa$INTLANGUAGE %in% c("",NA))

dtaa$NONVOTE16 <- 1*(dtaa$Q2_4=="(2) No")

dtaa$INCOME.W1 <- NA
dtaa$INCOME.W1[dtaa$Q10_15=="(1) Up to $20,000"] <- 10
dtaa$INCOME.W1[dtaa$Q10_15=="(2) $20,000 to $50,000"] <- 35
dtaa$INCOME.W1[dtaa$Q10_15=="(3) $50,000 to $75,000"] <- 62.5
dtaa$INCOME.W1[dtaa$Q10_15=="(4) $75,000 to $100,000"] <- 87.5
dtaa$INCOME.W1[dtaa$Q10_15=="(5) $100,000 to $125,000"] <- 112.5
dtaa$INCOME.W1[dtaa$Q10_15=="(6) $125,000 to $250,000"] <- 187.5
dtaa$INCOME.W1[dtaa$Q10_15=="(7) $250,000 and over"] <- 250

dtaa$AGE.W1 <- 2016-dtaa$Q10_18
dtaa$AGE.W1[dtaa$Q10_18 %in% c(8888,9999)] <- NA

dtaa$PID7 <- NA
dtaa$PID7[dtaa$Q10_0B=="(1) Strong Republican"] <- 7
dtaa$PID7[dtaa$Q10_0B=="(2) Not very strong Republican"] <- 6
dtaa$PID7[dtaa$Q10_0D=="(1) Closer to the Republican Party" | 
            dtaa$Q10_0E=="(1) Closer to the Republican Party"] <- 5

dtaa$PID7[dtaa$Q10_0D=="(3) Closer to neither party" | 
            dtaa$Q10_0E=="(3) Closer to neither party"] <- 4


dtaa$PID7[dtaa$Q10_0D=="(2) Closer to the Democratic party" | 
            dtaa$Q10_0E=="(2) Closer to the Democratic party"] <- 3
dtaa$PID7[dtaa$Q10_0C=="(2) Not very strong Democrat"] <- 2
dtaa$PID7[dtaa$Q10_0C=="(1) Strong Democrat"] <- 1

vars <- c("CITIZEN.W1","BORNUS.W1",
          "ASIAN.W1","JAPANESE.W1","CHINESE.W1","INDIAN.W1","FILIPINO.W1",
          "EDYEARS.W1","FEMALE.W1","ENGLISH.W1","INCOME.W1","AGE.W1","NONVOTE16",
          "CA.W1","TX.W1","NY.W1","FL.W1","PID7")

dta.asn <- dtaa[dtaa$ASIAN.W1==1,]
dta.asn.ne <- dtaa[dtaa$ENGLISH.W1==0 & dtaa$ASIAN.W1==1,]
dta.asn.eng <- dtaa[dtaa$ENGLISH.W1==1 & dtaa$ASIAN.W1==1,]

rmat <- matrix(NA,length(vars),6)
for(i in 1:length(vars)){
  txt <- paste("hold <- dta.asn$",vars[i],sep="")
  eval(parse(text=txt))
  rmat[i,1] <- mean(hold,na.rm=T)
  rmat[i,2] <- sd(hold,na.rm=T)
  
  txt <- paste("hold <- dta.asn.ne$",vars[i],sep="")
  eval(parse(text=txt))
  rmat[i,3] <- mean(hold,na.rm=T)
  rmat[i,4] <- sd(hold,na.rm=T)
  
  txt <- paste("hold <- dta.asn.eng$",vars[i],sep="")
  eval(parse(text=txt))
  rmat[i,5] <- mean(hold,na.rm=T)
  rmat[i,6] <- sd(hold,na.rm=T)
  
  
  # rmat[i,3] <- mean(hold[! dta.asn$duration.W2 %in% c(NA)],na.rm=T)
  # rmat[i,4] <- sd(hold[! dta.asn$duration.W2 %in% c(NA)],na.rm=T)
  
  # rmat[i,5] <- mean(hold[! dta.asn$duration.W3 %in% c(NA)],na.rm=T)
  # rmat[i,6] <- sd(hold[! dta.asn$duration.W3 %in% c(NA)],na.rm=T)
}
rownames(rmat) <- vars
round(rmat,digits=2)
xtable(rmat,digits=c(0,rep(3,6)))