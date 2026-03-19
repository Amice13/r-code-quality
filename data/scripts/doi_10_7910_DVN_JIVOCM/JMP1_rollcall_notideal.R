#install.packages("tidyr")
library(pscl)
library(rjags)
library(readr)
library(dplyr)
library(tidyr)
library(Rvoteview)
library(wnominate)
library(ggplot2)
library(readxl)

legis <- read_csv("~/Documents/Active Projects/Disseration/Data/Ideal Points/Hall_members.csv")
#legis <- subset(legis, subset=(chamber == "House")) 
bills <- read_csv("~/Documents/Active Projects/Disseration/Data/Ideal Points/Hall_rollcalls.csv")
bank_data_ <- read_csv("~/Documents/Active Projects/Disseration/Data/bank-data.csv")
cap <- read_excel("~/Documents/Active Projects/Disseration/Data/Ideal Points/Roll Call Data_Econ Reg_Full.xlsx")

fin81 <- read_csv("~/Documents/Active Projects/Disseration/Data/District Data/fin81.csv")
fin82 <- read_csv("~/Documents/Active Projects/Disseration/Data/District Data/fin82.csv")
fin83 <- read_csv("~/Documents/Active Projects/Disseration/Data/District Data/fin83.csv")
fin84 <- read_csv("~/Documents/Active Projects/Disseration/Data/District Data/fin84.csv")
fin85 <- read_csv("~/Documents/Active Projects/Disseration/Data/District Data/fin85.csv")
fin86 <- read_csv("~/Documents/Active Projects/Disseration/Data/District Data/fin86.csv")
fin87 <- read_csv("~/Documents/Active Projects/Disseration/Data/District Data/fin87.csv")
fin88 <- read_csv("~/Documents/Active Projects/Disseration/Data/District Data/fin88.csv")
fin89 <- read_csv("~/Documents/Active Projects/Disseration/Data/District Data/fin89.csv")
fin90 <- read_csv("~/Documents/Active Projects/Disseration/Data/District Data/fin90.csv")
fin91 <- read_csv("~/Documents/Active Projects/Disseration/Data/District Data/fin91.csv")
fin92 <- read_csv("~/Documents/Active Projects/Disseration/Data/District Data/fin92.csv")
fin93 <- read_csv("~/Documents/Active Projects/Disseration/Data/District Data/fin93.csv")
fin94 <- read_excel("~/Documents/Active Projects/Disseration/Data/District Data/fin94.xls")
fin95 <- read_excel("~/Documents/Active Projects/Disseration/Data/District Data/fin95.xls")
fin96 <- read_excel("~/Documents/Active Projects/Disseration/Data/District Data/fin96.xls")
fin97 <- read_excel("~/Documents/Active Projects/Disseration/Data/District Data/fin97.xls")
fin98 <- read_csv("~/Documents/Active Projects/Disseration/Data/District Data/fin98.csv")
fin99 <- read_csv("~/Documents/Active Projects/Disseration/Data/District Data/fin99.csv")
fin101 <- read_csv("~/Documents/Active Projects/Disseration/Data/District Data/fin101.csv")
fin102 <- read_csv("~/Documents/Active Projects/Disseration/Data/District Data/fin102.csv")



fin81$congress <- 81
fin82$congress <- 82
fin83$congress <- 83
fin84$congress <- 84
fin85$congress <- 85
fin86$congress <- 86
fin87$congress <- 87
fin88$congress <- 88
fin89$congress <- 89
fin90$congress <- 90
fin91$congress <- 91
fin92$congress <- 92
fin93$congress <- 93
fin94$congress <- 94
fin95$congress <- 95
fin96$congress <- 96
fin97$congress <- 97
fin98$congress <- 98
fin99$congress <- 99
fin101$congress <- 101
fin102$congress <- 102

fin81 <- subset(fin81, select=c(STATE, CD, congress, BLACK, CONSTRCT, FARMER, RURLFARM, 
                                TRANSPRT, UNION, URBAN, LANDSQMI, BLUCLLR ))
fin81$BANK <- ""
fin81$AGE65 <- ""
fin81$FINANCE <- ""

fin82 <- subset(fin82, select=c(STATE, CD, congress, BLACK, CONSTRCT, FARMER, RURLFARM, 
                                TRANSPRT, UNION, URBAN, LANDSQMI, BLUCLLR))
fin82$BANK <- ""
fin82$AGE65 <- ""
fin82$FINANCE <- ""
fin88$FINANCE <- ""
fin89$FINANCE <- ""
fin90$FINANCE <- ""
fin91$FINANCE <- ""
fin92$FINANCE <- ""

fin83 <- subset(fin83, select=c(STATE, CD, congress, BLACK, CONSTRCT, FARMER, RURLFARM, 
                                TRANSPRT, UNION, URBAN, LANDSQMI,BLUCLLR, BANK, AGE65, FINANCE ))
fin84 <- subset(fin84, select=c(STATE, CD, congress, BLACK, CONSTRCT, FARMER, RURLFARM, 
                                TRANSPRT, UNION, URBAN, LANDSQMI, BLUCLLR, BANK, AGE65, FINANCE  ))
fin85 <- subset(fin85, select=c(STATE, CD, congress, BLACK, CONSTRCT, FARMER, RURLFARM, 
                                TRANSPRT, UNION, URBAN, LANDSQMI, BLUCLLR, BANK, AGE65, FINANCE  ))
fin86 <- subset(fin86, select=c(STATE, CD, congress, BLACK, CONSTRCT, FARMER, RURLFARM, 
                                TRANSPRT, UNION, URBAN, LANDSQMI,BLUCLLR, BANK, AGE65, FINANCE  ))
fin87 <- subset(fin87, select=c(STATE, CD, congress, BLACK, CONSTRCT, FARMER, RURLFARM, 
                                TRANSPRT, UNION, URBAN, LANDSQMI, BLUCLLR, BANK, AGE65, FINANCE  ))
fin88 <- subset(fin88, select=c(STATE, CD, congress, BLACK, CONSTRCT, FARMER, RURLFARM, 
                                TRANSPRT, UNION, URBAN, LANDSQMI, BLUCLLR, BANK, AGE65, FINANCE  ))
fin89 <- subset(fin89, select=c(STATE, CD, congress, BLACK, CONSTRCT, FARMER, RURLFARM, 
                                TRANSPRT, UNION, URBAN, LANDSQMI, BLUCLLR, BANK, AGE65, FINANCE  ))
fin90 <- subset(fin90, select=c(STATE, CD, congress, BLACK, CONSTRCT, FARMER, RURLFARM, 
                                TRANSPRT, UNION, URBAN, LANDSQMI, BLUCLLR, BANK, AGE65, FINANCE ))
fin91 <- subset(fin91, select=c(STATE, CD, congress, BLACK, CONSTRCT, FARMER, RURLFARM, 
                                TRANSPRT, UNION, URBAN, LANDSQMI, BLUCLLR, BANK, AGE65, FINANCE  ))
fin92 <- subset(fin92, select=c(STATE, CD, congress, BLACK, CONSTRCT, FARMER, RURLFARM, 
                                TRANSPRT, UNION, URBAN, LANDSQMI, BLUCLLR, BANK, AGE65, FINANCE  ))
fin93 <- subset(fin93, select=c(STATE, CD, congress, BLACK, CONSTRCT, FARMER, RURLFARM, 
                                TRANSPRT, UNION, URBAN, LANDSQMI, BLUCLLR, BANK, AGE65, FINANCE  ))
fin94 <- subset(fin94, select=c(STATE, CD, congress, BLACK, CONSTRCT, FARMER, RURLFARM, 
                                TRANSPRT, UNION, URBAN, LANDSQMI, BLUCLLR, BANK, AGE65, FINANCE  ))
fin95 <- subset(fin95, select=c(STATE, CD, congress, BLACK, CONSTRCT, FARMER, RURLFARM, 
                                TRANSPRT, UNION, URBAN, LANDSQMI,BLUCLLR, BANK, AGE65, FINANCE  ))
fin96 <- subset(fin96, select=c(STATE, CD, congress, BLACK, CONSTRCT, FARMER, RURLFARM, 
                                TRANSPRT, UNION, URBAN, LANDSQMI, BLUCLLR, BANK, AGE65, FINANCE  ))
fin97 <- subset(fin97, select=c(STATE, CD, congress, BLACK, CONSTRCT, FARMER, RURLFARM, 
                                TRANSPRT, UNION, URBAN, LANDSQMI,BLUCLLR, BANK, AGE65, FINANCE  ))
fin98 <- subset(fin98, select=c(STATE, CD, congress, BLACK, CONSTRCT, FARMER, RURLFARM, 
                                TRANSPRT, UNION, URBAN, LANDSQMI, BLUCLLR, BANK, AGE65, FINANCE  ))
fin99 <- subset(fin99, select=c(STATE, CD, congress, BLACK, CONSTRCT, FARMER, RURLFARM, 
                                TRANSPRT, UNION, URBAN, LANDSQMI, BLUCLLR, BANK, AGE65, FINANCE  ))
fin101 <- subset(fin101, select=c(STATE, CD, congress, BLACK, CONSTRCT, FARMER, RURLFARM, 
                                TRANSPRT, UNION, URBAN, LANDSQMI, BLUCLLR, BANK, AGE65, FINANCE  ))
fin102 <- subset(fin102, select=c(STATE, CD, congress, BLACK, CONSTRCT, FARMER, RURLFARM, 
                                TRANSPRT, UNION, URBAN, LANDSQMI, BLUCLLR, BANK, AGE65, FINANCE  ))

adler <- rbind( fin81,  fin82)
adler <- rbind( adler,  fin83)
adler <- rbind( adler,  fin84)
adler <- rbind( adler,  fin85)
adler <- rbind( adler,  fin86)
adler <- rbind( adler,  fin87)
adler <- rbind( adler,  fin88)
adler <- rbind( adler,  fin89)
adler <- rbind( adler,  fin90)
adler <- rbind( adler,  fin91)
adler <- rbind( adler,  fin92)
adler <- rbind( adler,  fin93)
adler <- rbind( adler,  fin94)
adler <- rbind( adler,  fin95)
adler <- rbind( adler,  fin96)
adler <- rbind( adler,  fin97)
adler <- rbind( adler,  fin98)
adler <- rbind( adler,  fin99)
adler <- rbind( adler,  fin101)
adler <- rbind( adler,  fin102)

#adler$CD[adler$CD==1] <- "01"
#adler$CD[adler$CD==2] <- "02"
#adler$CD[adler$CD==3] <- "03"
#adler$CD[adler$CD==4] <- "04"
#adler$CD[adler$CD==5] <- "05"
#adler$CD[adler$CD==6] <- "06"
#adler$CD[adler$CD==7] <- "07"
#adler$CD[adler$CD==8] <- "08"
#adler$CD[adler$CD==9] <- "09"


legis$st <- 0
legis$st[legis$state_abbrev=="AL"] <- 1
legis$st[legis$state_abbrev=="AK"] <- 2
legis$st[legis$state_abbrev=="AZ"] <- 3
legis$st[legis$state_abbrev=="AR"] <- 4
legis$st[legis$state_abbrev=="CA"] <- 5
legis$st[legis$state_abbrev=="CO"] <- 6
legis$st[legis$state_abbrev=="CT"] <- 7
legis$st[legis$state_abbrev=="DE"] <- 8
legis$st[legis$state_abbrev=="FL"] <- 9
legis$st[legis$state_abbrev=="GA"] <- 10
legis$st[legis$state_abbrev=="HI"] <- 11
legis$st[legis$state_abbrev=="ID"] <- 12
legis$st[legis$state_abbrev=="IL"] <- 13
legis$st[legis$state_abbrev=="IN"] <- 14
legis$st[legis$state_abbrev=="IA"] <- 15
legis$st[legis$state_abbrev=="KS"] <- 16
legis$st[legis$state_abbrev=="KY"] <- 17
legis$st[legis$state_abbrev=="LA"] <- 18
legis$st[legis$state_abbrev=="ME"] <- 19
legis$st[legis$state_abbrev=="MD"] <- 20
legis$st[legis$state_abbrev=="MA"] <- 21
legis$st[legis$state_abbrev=="MI"] <- 22
legis$st[legis$state_abbrev=="MN"] <- 23
legis$st[legis$state_abbrev=="MS"] <- 24
legis$st[legis$state_abbrev=="MO"] <- 25
legis$st[legis$state_abbrev=="MT"] <- 26
legis$st[legis$state_abbrev=="NE"] <- 27
legis$st[legis$state_abbrev=="NV"] <- 28
legis$st[legis$state_abbrev=="NH"] <- 29
legis$st[legis$state_abbrev=="NJ"] <- 30
legis$st[legis$state_abbrev=="NM"] <- 31
legis$st[legis$state_abbrev=="NY"] <- 32
legis$st[legis$state_abbrev=="NC"] <- 33
legis$st[legis$state_abbrev=="ND"] <- 34
legis$st[legis$state_abbrev=="OH"] <- 35
legis$st[legis$state_abbrev=="OK"] <- 36
legis$st[legis$state_abbrev=="OR"] <- 37
legis$st[legis$state_abbrev=="PA"] <- 38
legis$st[legis$state_abbrev=="RI"] <- 39
legis$st[legis$state_abbrev=="SC"] <- 40
legis$st[legis$state_abbrev=="SD"] <- 41
legis$st[legis$state_abbrev=="TN"] <- 42
legis$st[legis$state_abbrev=="TX"] <- 43
legis$st[legis$state_abbrev=="UT"] <- 44
legis$st[legis$state_abbrev=="VT"] <- 45
legis$st[legis$state_abbrev=="VA"] <- 46
legis$st[legis$state_abbrev=="WA"] <- 47
legis$st[legis$state_abbrev=="WV"] <- 48
legis$st[legis$state_abbrev=="WI"] <- 49
legis$st[legis$state_abbrev=="WY"] <- 50
legis$stcd <- as.numeric(paste(legis$st, legis$district_code, sep = ""))


bank_data_$st <- 0
bank_data_$st[bank_data_$STNAME=="Alabama"] <- 1
bank_data_$st[bank_data_$STNAME=="Alaska"] <- 2
bank_data_$st[bank_data_$STNAME=="Arizona"] <- 3
bank_data_$st[bank_data_$STNAME=="Arkansas"] <- 4
bank_data_$st[bank_data_$STNAME=="California"] <- 5
bank_data_$st[bank_data_$STNAME=="Colorado"] <- 6
bank_data_$st[bank_data_$STNAME=="Connecticut"] <- 7
bank_data_$st[bank_data_$STNAME=="Delaware"] <- 8
bank_data_$st[bank_data_$STNAME=="Florida"] <- 9
bank_data_$st[bank_data_$STNAME=="Georgia"] <- 10
bank_data_$st[bank_data_$STNAME=="Hawaii"] <- 11
bank_data_$st[bank_data_$STNAME=="Idaho"] <- 12
bank_data_$st[bank_data_$STNAME=="Illinois"] <- 13
bank_data_$st[bank_data_$STNAME=="Indiana"] <- 14
bank_data_$st[bank_data_$STNAME=="Iowa"] <- 15
bank_data_$st[bank_data_$STNAME=="Kansas"] <- 16
bank_data_$st[bank_data_$STNAME=="Kentucky"] <- 17
bank_data_$st[bank_data_$STNAME=="Louisiana"] <- 18
bank_data_$st[bank_data_$STNAME=="Maine"] <- 19
bank_data_$st[bank_data_$STNAME=="Maryland"] <- 20
bank_data_$st[bank_data_$STNAME=="Massachusetts"] <- 21
bank_data_$st[bank_data_$STNAME=="Michigan"] <- 22
bank_data_$st[bank_data_$STNAME=="Minnesota"] <- 23
bank_data_$st[bank_data_$STNAME=="Mississippi"] <- 24
bank_data_$st[bank_data_$STNAME=="Missouri"] <- 25
bank_data_$st[bank_data_$STNAME=="Montana"] <- 26
bank_data_$st[bank_data_$STNAME=="Nebraska"] <- 27
bank_data_$st[bank_data_$STNAME=="Nevada"] <- 28
bank_data_$st[bank_data_$STNAME=="New Hampshire"] <- 29
bank_data_$st[bank_data_$STNAME=="New Jersey"] <- 30
bank_data_$st[bank_data_$STNAME=="New Mexico"] <- 31
bank_data_$st[bank_data_$STNAME=="New York"] <- 32
bank_data_$st[bank_data_$STNAME=="North Carolina"] <- 33
bank_data_$st[bank_data_$STNAME=="North Dakota"] <- 34
bank_data_$st[bank_data_$STNAME=="Ohio"] <- 35
bank_data_$st[bank_data_$STNAME=="Oklahoma"] <- 36
bank_data_$st[bank_data_$STNAME=="Oregon"] <- 37
bank_data_$st[bank_data_$STNAME=="Pennsylvania"] <- 38
bank_data_$st[bank_data_$STNAME=="Rhode Island"] <- 39
bank_data_$st[bank_data_$STNAME=="South Carolina"] <- 40
bank_data_$st[bank_data_$STNAME=="South Dakota"] <- 41
bank_data_$st[bank_data_$STNAME=="Tennessee"] <- 42
bank_data_$st[bank_data_$STNAME=="Texas"] <- 43
bank_data_$st[bank_data_$STNAME=="Utah"] <- 44
bank_data_$st[bank_data_$STNAME=="Vermont"] <- 45
bank_data_$st[bank_data_$STNAME=="Virginia"] <- 46
bank_data_$st[bank_data_$STNAME=="Washington"] <- 47
bank_data_$st[bank_data_$STNAME=="West Virginia"] <- 48
bank_data_$st[bank_data_$STNAME=="Wisconsin"] <- 49
bank_data_$st[bank_data_$STNAME=="Wyoming"] <- 50
bank_data_ <- subset(bank_data_, subset=(!st==0))
bank_data_$year <- bank_data_$YEAR

legis$year <- 1786 + 2*legis$congress  
legis <- subset(legis, subset=(year>1940))
legis <- merge(legis, bank_data_, by = c("st", "year"))


h80 <- read_csv("~/Documents/Active Projects/Disseration/Data/Ideal Points/Votes/H080_votes.csv")
h81 <- read_csv("~/Documents/Active Projects/Disseration/Data/Ideal Points/Votes/H081_votes.csv")
h82 <- read_csv("~/Documents/Active Projects/Disseration/Data/Ideal Points/Votes/H082_votes.csv")
h83 <- read_csv("~/Documents/Active Projects/Disseration/Data/Ideal Points/Votes/H083_votes.csv")
h84 <- read_csv("~/Documents/Active Projects/Disseration/Data/Ideal Points/Votes/H084_votes.csv")
h85 <- read_csv("~/Documents/Active Projects/Disseration/Data/Ideal Points/Votes/H085_votes.csv")
h86 <- read_csv("~/Documents/Active Projects/Disseration/Data/Ideal Points/Votes/H086_votes.csv")
h87 <- read_csv("~/Documents/Active Projects/Disseration/Data/Ideal Points/Votes/H087_votes.csv")
h88 <- read_csv("~/Documents/Active Projects/Disseration/Data/Ideal Points/Votes/H088_votes.csv")
h89 <- read_csv("~/Documents/Active Projects/Disseration/Data/Ideal Points/Votes/H089_votes.csv")
h90 <- read_csv("~/Documents/Active Projects/Disseration/Data/Ideal Points/Votes/H090_votes.csv")
h91 <- read_csv("~/Documents/Active Projects/Disseration/Data/Ideal Points/Votes/H091_votes.csv")
h92 <- read_csv("~/Documents/Active Projects/Disseration/Data/Ideal Points/Votes/H092_votes.csv")
h93 <- read_csv("~/Documents/Active Projects/Disseration/Data/Ideal Points/Votes/H093_votes.csv")
h94 <- read_csv("~/Documents/Active Projects/Disseration/Data/Ideal Points/Votes/H094_votes.csv")
h95 <- read_csv("~/Documents/Active Projects/Disseration/Data/Ideal Points/Votes/H095_votes.csv")
h96 <- read_csv("~/Documents/Active Projects/Disseration/Data/Ideal Points/Votes/H096_votes.csv")
h97 <- read_csv("~/Documents/Active Projects/Disseration/Data/Ideal Points/Votes/H097_votes.csv")
h98 <- read_csv("~/Documents/Active Projects/Disseration/Data/Ideal Points/Votes/H098_votes.csv")
h99 <- read_csv("~/Documents/Active Projects/Disseration/Data/Ideal Points/Votes/H099_votes.csv")
h100 <- read_csv("~/Documents/Active Projects/Disseration/Data/Ideal Points/Votes/H100_votes.csv")
h101 <- read_csv("~/Documents/Active Projects/Disseration/Data/Ideal Points/Votes/H101_votes.csv")
h102 <- read_csv("~/Documents/Active Projects/Disseration/Data/Ideal Points/Votes/H102_votes.csv")
h103 <- read_csv("~/Documents/Active Projects/Disseration/Data/Ideal Points/Votes/H103_votes.csv")
h104 <- read_csv("~/Documents/Active Projects/Disseration/Data/Ideal Points/Votes/H104_votes.csv")
h105 <- read_csv("~/Documents/Active Projects/Disseration/Data/Ideal Points/Votes/H105_votes.csv")
h106 <- read_csv("~/Documents/Active Projects/Disseration/Data/Ideal Points/Votes/H106_votes.csv")


votes <- rbind( h94,  h95)
votes <- rbind( votes,  h89)
votes <- rbind( votes,  h90)
votes <- rbind( votes,  h91)
votes <- rbind( votes,  h92)
votes <- rbind( votes,  h93)
votes <- rbind( votes,  h96)
votes <- rbind( votes,  h97)
votes <- rbind( votes,  h98)
votes <- rbind( votes,  h80)
votes <- rbind( votes,  h81)
votes <- rbind( votes,  h82)
votes <- rbind( votes,  h83)
votes <- rbind( votes,  h84)
votes <- rbind( votes,  h85)
votes <- rbind( votes,  h86)
votes <- rbind( votes,  h87)
votes <- rbind( votes,  h88)
votes <- rbind( votes,  h99)
votes <- rbind( votes,  h100)
votes <- rbind( votes,  h101)
votes <- rbind( votes,  h102)
votes <- rbind( votes,  h103)
votes <- rbind( votes,  h104)
votes <- rbind( votes,  h105)
votes <- rbind( votes,  h106)

cap <- subset(cap, subset=(filter_House==1))
cap <- subset(cap, subset=(subtopic==1501|subtopic==1502))
cap$rollnumber <- cap$rc_count
cap$congress <- cap$cong
cap <- subset(cap, subset=(congress>80))
cap <- subset(cap, subset=(congress<103))
cap$reg <- (  cap$`conglomereation` +
                  cap$`rate/wage controls, quotas, license (competition from below) ; interstate, bank branching constraints`+
                  cap$`constrain markets and financial products`)
cap <- subset(cap, subset=(!reg==0))


votes <- merge(votes, cap, by = c("congress", "rollnumber"))

votes$vote[votes$cast_code==1 |votes$cast_code==2 | votes$cast_code==3] <- 1
votes$vote[votes$cast_code==4 |votes$cast_code==5 | votes$cast_code==6] <- 0
votes$vote[votes$cast_code==7 |votes$cast_code==8 | votes$cast_code==9] <- NA


votes$deregvote[votes$reg>0 & votes$vote==1] <- 1 
votes$deregvote[votes$reg<0 & votes$vote==0] <- 1 
votes$deregvote[votes$reg>0 & votes$vote==0] <- 0 
votes$deregvote[votes$reg<0 & votes$vote==1] <- 0
votes$deregvote[votes$reg==0] <- NA
votes <- subset(votes, subset=(!reg==0))

votes$deregvote[is.na(votes$deregvote)] <- NA
votes <- votes[!is.na(votes$rollnumber),]

legis3 <- subset(legis, select=c(icpsr, party_code,  congress))
votes <- merge(votes, legis3, by=c("icpsr", "congress"), all.y =TRUE)

deregvote <- aggregate(deregvote ~ party_code + congress + icpsr , data= votes, FUN = mean)
legis <- merge(legis, deregvote, by=c("icpsr", "congress", "party_code"), all.y =TRUE)


pre94th <- subset(legis, subset=(congress < 94)) 
pre94th <- subset(pre94th, subset=(congress > 88)) 
pre94th <- subset(pre94th, subset=(chamber == "House")) 
pre94th$pre94 <- 1
pre94th <- subset(pre94th, select=c("icpsr", "pre94"))
pre94th <- pre94th[!duplicated(pre94th[c("icpsr")]),]

legis <- subset(legis, subset=(congress < 103)) 
legis <- subset(legis, subset=(congress > 80)) 
legis <- merge(legis, pre94th, by="icpsr", all.x = TRUE)
legis$pre94[is.na(legis$pre94)] <- 0


dem <- subset(legis, subset=(party_code==100))
dem <- subset(dem, subset=(congress<103))
dem <- subset(dem, subset=(congress>80))


dem <- merge(dem, adler, by.x=c("state_abbrev", "district_code", "congress"), 
             by.y=c("STATE", "CD", "congress"), all.x=TRUE)

dem$BANK <- as.numeric(dem$BANK)
dem$AGE65 <- as.numeric(dem$AGE65)
dem$FINANCE <- as.numeric(dem$FINANCE)

table(cap$congress)

dem$avg_deposit <- dem$DEP / dem$BANKS

dem$dummy81 <- dem$congress
dem$dummy81[dem$dummy81==81] <- 1
dem$dummy81[!dem$dummy81==1] <- 0

dem$dummy84 <- dem$congress
dem$dummy84[dem$dummy84==84] <- 1
dem$dummy84[!dem$dummy84==1] <- 0

dem$dummy89 <- dem$congress
dem$dummy89[dem$dummy89==89] <- 1
dem$dummy89[!dem$dummy89==1] <- 0

dem$dummy90 <- dem$congress
dem$dummy90[dem$dummy90==90] <- 1
dem$dummy90[!dem$dummy90==1] <- 0

dem$dummy91 <- dem$congress
dem$dummy91[dem$dummy91==91] <- 1
dem$dummy91[!dem$dummy91==1] <- 0


dem$dummy93 <- dem$congress
dem$dummy93[dem$dummy93==93] <- 1
dem$dummy93[!dem$dummy93==1] <- 0


dem$dummy94 <- dem$congress
dem$dummy94[dem$dummy94==94] <- 1
dem$dummy94[!dem$dummy94==1] <- 0


dem$dummy95 <- dem$congress
dem$dummy95[dem$dummy95==95] <- 1
dem$dummy95[!dem$dummy95==1] <- 0


dem$dummy96 <- dem$congress
dem$dummy96[dem$dummy96==96] <- 1
dem$dummy96[!dem$dummy96==1] <- 0


dem$dummy97 <- dem$congress
dem$dummy97[dem$dummy97==97] <- 1
dem$dummy97[!dem$dummy97==1] <- 0

dem$dummy100 <- dem$congress
dem$dummy100[dem$dummy100==100] <- 1
dem$dummy100[!dem$dummy100==1] <- 0

dem$dummy101 <- dem$congress
dem$dummy101[dem$dummy101==101] <- 1
dem$dummy101[!dem$dummy101==1] <- 0

dem$dummy102 <- dem$congress
dem$dummy102[dem$dummy102==102] <- 1
dem$dummy102[!dem$dummy102==1] <- 0

dem$deregvote <- dem$deregvote *100
dem$avg_deposit <- dem$avg_deposit/10000
dem$BLUCLLR <- dem$BLUCLLR/10000
dem$CONSTRCT <- dem$CONSTRCT/10000
dem$URBAN <- dem$URBAN/10000
dem$RURLFARM <- dem$RURLFARM/10000
dem$BLACK <- dem$BLACK/10000


dem_prereform <- subset(dem, congress<94)
dem_postreform <- subset(dem, congress>93)

pre <- lm(deregvote ~  avg_deposit + BLUCLLR + CONSTRCT + URBAN + RURLFARM+
            BLACK + nominate_dim1 +
            dummy84 + dummy89 + dummy90 + dummy91 +dummy93 ,  data=dem_prereform)
summary(pre)

post <- lm(deregvote ~  avg_deposit + BLUCLLR+ CONSTRCT+ URBAN + RURLFARM+
            BLACK + nominate_dim1 +
             dummy95 + dummy96 + dummy97 + dummy100+ dummy101 +dummy102 + pre94,  data=dem_postreform)
summary(post)


m81 <- lm(deregvote ~  avg_deposit+ UNION+ BLUCLLR+ CONSTRCT+ URBAN + RURLFARM+
            BLACK + nominate_dim1 ,  subset=(congress==81), data=dem)
summary(m81)


m89 <- lm(deregvote ~  avg_deposit + UNION+ BLUCLLR+ CONSTRCT+ URBAN + RURLFARM+
            BLACK  + nominate_dim1,  subset=(congress==89), data=dem)
summary(m89)

m91 <- lm(deregvote ~  avg_deposit + UNION  + CONSTRCT+  UNION+RURLFARM+
          BLUCLLR + BLACK +URBAN+  AGE65 +nominate_dim1,  subset=(congress==91), data=dem)
summary(m91)

m93 <- lm(deregvote ~  avg_deposit + UNION+CONSTRCT+RURLFARM+
          BLUCLLR + BLACK + URBAN+  AGE65 + nominate_dim1,  subset=(congress==93), data=dem)
summary(m93)

m94 <- lm(deregvote ~   avg_deposit +  UNION+RURLFARM+
          BLUCLLR+ BLACK +URBAN+  AGE65 + nominate_dim1,  subset=(congress==94), data=dem)
summary(m94)

m95 <- lm(deregvote ~  `Avg Deposit`  + UNION+ RURLFARM+
          BLUCLLR + BLACK +URBAN+  AGE65 ,  subset=(congress==95), data=dem)
summary(m95)

m96 <- lm(deregvote ~  avg_deposit  + UNION+ RURLFARM+
            BLUCLLR + BLACK +URBAN+  AGE65 +nominate_dim1,  subset=(congress==96), data=dem)
summary(m96)

m101 <- lm(deregvote ~  `Avg Deposit`  +  UNION +RURLFARM+
          BLUCLLR + BLACK +URBAN+  AGE65 ,  subset=(congress==101), data=dem)
summary(t)

m102 <- lm(deregvote ~  `Avg Deposit`  +   UNION+RURLFARM+
          BLUCLLR + BLACK + URBAN+ AGE65 ,  subset=(congress==102), data=dem)
summary(m102)

library(dotwhisker)
library(dplyr)
dwplot(list(m81, m89, m91, m93))
dwplot(list(m95, m96, m101, m102))


sd(dem$deregvote)

library(ggplot2)
library(repmis)
library(mgcv)
library(Hmisc)

g0 <- ggplot(dem,aes(x=pre94,y=deregvote))
g_mean<-g0+stat_summary(fun.y=mean,geom="point")
g_mean
g_mean+stat_summary(fun.data=mean_cl_normal,geom="errorbar")


t <- lm(deregvote ~ first_primary, data=dem)
summary(t)
#need to create first_primary estimates using primary data from the 50/60s by state 

t <- lm(deregvote ~ nominate_dim1, data=dem)
summary(t)

t <- lm(deregvote ~ nominate_dim2, data=dem)
summary(t)

t <- lm(deregvote ~ pre94 + nominate_dim1 + nominate_dim2, data=dem)
summary(t)

t <- lm(nominate_dim2 ~ pre94, data=dem)
summary(t)
sd(dem$nominate_dim2)


t <- lm(nominate_dim1 ~ pre94, data=dem)
summary(t)
sd(dem$nominate_dim1)

reg <- lm(deregvote ~ pre94 +  CONSTRCT + FINANCE + RURLFARM +
            UNION + LANDSQMI + URBAN + COAST + BLACK + BANK + AGE65, data=dem)

reg <- lm(deregvote ~ first_primary, data=dem)
summary(reg)

reg <- lm(deregvote ~ first_primary + BANK + CONSTRCT + UNION  +
            RURLFARM  + BLACK  + AGE65, data=dem)
summary(reg)

dem$pre94 <- as.factor(dem$pre94)
ggplot(dem, aes(x=nominate_dim1, y=nominate_dim2, color=pre94)) + geom_point()

plot(dem$nominate_dim1, dem$deregvote)
plot(dem$nominate_dim2, dem$deregvote)

sd(dem$deregvote)
sd(dem$first_primary)


###before deregulation and party change
votes2 <- rbind( h89,  h90)
votes2 <- rbind( votes2,  h91)
votes2 <- rbind( votes2,  h92)
votes2 <- rbind( votes2,  h93)

votes2 <- merge(votes2, cap, by = c("congress", "rollnumber"))

votes2_bank <- subset(votes2, subset=(billcoded==1 ))

votes2_bank$vote[votes2_bank$cast_code==1 |votes2_bank$cast_code==2 | votes2_bank$cast_code==3] <- 1
votes2_bank$vote[votes2_bank$cast_code==4 |votes2_bank$cast_code==5 | votes2_bank$cast_code==6] <- -1
votes2_bank$vote[votes2_bank$cast_code==7 |votes2_bank$cast_code==8 | votes2_bank$cast_code==9] <- 0

votes2_bank$deregvote[votes2_bank$prior>0 & votes2_bank$vote==1] <- 1 
votes2_bank$deregvote[votes2_bank$prior<0 & votes2_bank$vote==-1] <- 1 
votes2_bank$deregvote[votes2_bank$prior>0 & votes2_bank$vote==-1] <- -1 
votes2_bank$deregvote[votes2_bank$prior<0 & votes2_bank$vote==1] <- -1
votes2_bank$deregvote[votes2_bank$vote==0] <- 0
votes2_bank$deregvote[is.na(votes2_bank$deregvote)] <- 0

deregvote2 <- aggregate(deregvote ~ icpsr, data= votes2_bank, FUN = mean)

legis <- read_csv("~/Documents/Active Projects/Disseration/Data/Ideal Points/Hall_members.csv")
legis3 <- subset(legis, subset=(chamber == "House")) 
legis3 <- subset(legis3, subset=(congress < 94)) 
legis3 <- subset(legis3, subset=(congress > 88))
legis3 <- subset(legis3, select=c("bioname", "icpsr", "party_code", "state_icpsr", "state_abbrev", "district_code"))
legis3 <- legis3[!duplicated(legis3[c("icpsr")]),]

legis3 <- merge(legis3, deregvote2, by="icpsr", all.y=TRUE)
dem2 <- subset(legis3, subset=(party_code==100))

fin90 <- read_csv("~/Documents/Active Projects/Disseration/Data/District Data/fin90.csv")

dem2 <- merge(dem2, fin90, by.x=c("state_abbrev", "district_code"), 
             by.y=c("STATE", "CD"), all.x=TRUE)

reg <- lm(deregvote ~  CONSTRCT + FINANCE + RURLFARM +
            UNION + URBAN + CITY + COAST + BLACK + BANK + AGE65, data=dem2)
summary(reg) 