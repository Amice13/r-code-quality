### Dan Hopkins
### 11/22/2022
### Replication file

### load libraries
library(ggplot2)
library(scales) # for muted function
library(xtable)
library(reshape2)
library(texreg)

### new function
calculate.vec <- function(M,vec){
  hold <- c()
  N <- length(vec)
  for(i in 1:M){
    hold[i] <- mean(sample(vec,
                           size=N,replace=T),na.rm=T)
  }
  return(quantile(hold,c(0.025,0.975)))
}    
### must reset working directory
setwd("/users/danhop/Dropbox/PDparty/rcode/replication/")

### load data
#load("~/Dropbox/PDparty/rcode/replication/merged-PD-waves-01212021.Rdata")
#colnames(dtam2sub)

load("pd-allwaves-replication1.Rdata")

dtam2 <- dtam2sub
rm(dtam2sub)

dtam2$TRUMPVCB.W1 <- 0
dtam2$TRUMPVCB.W1[dtam2$VP5_A.W1 %in% c("Donald Trump, the Republican")] <- 1
dtam2$TRUMPVCB.W1[dtam2$VP5_A.W1 %in% c("Hillary Clinton, the Democrat")] <- -1

dtam2$TRUMPVCB.W2 <- NA

dtam2$TRUMPVCB.W2[dtam2$DOV_VP5_A.W2=="2" & dtam2$VP5_A.W2=="Donald Trump, the Republican/Hillary Clinton, the Democrat"] <- -1
dtam2$TRUMPVCB.W2[dtam2$DOV_VP5_A.W2=="1" & dtam2$VP5_A.W2=="Donald Trump, the Republican/Hillary Clinton, the Democrat"] <- 1

dtam2$TRUMPVCB.W2[dtam2$DOV_VP5_A.W2=="1" & 
                    dtam2$VP5_A.W2=="Hillary Clinton, the Democrat/Donald Trump, the Republican"] <- -1
dtam2$TRUMPVCB.W2[dtam2$DOV_VP5_A.W2=="2" & 
                    dtam2$VP5_A.W2=="Hillary Clinton, the Democrat/Donald Trump, the Republican"] <- 1
dtam2$TRUMPVCB.W2[dtam2$VP5_A.W2 %in% c("Gary Johnson, the Libertarian","Jill Stein, the Green Party candidate",
                                        "Other (SPECIFY)","Would not vote for President")] <- 0


dtam2$PANETHNOTIMP.W3 <- NA
dtam2$PANETHNOTIMP.W3[dtam2$Q12_1.W3=="Strongly disagree"] <- 4
dtam2$PANETHNOTIMP.W3[dtam2$Q12_1.W3=="Somewhat disagree"] <- 3
dtam2$PANETHNOTIMP.W3[dtam2$Q12_1.W3=="Somewhat agree"] <- 2
dtam2$PANETHNOTIMP.W3[dtam2$Q12_1.W3=="Strongly agree"] <- 1

dtam2$PANETHCENTRAL.W3 <- NA
dtam2$PANETHCENTRAL.W3[dtam2$Q12_3.W3=="Strongly disagree"] <- 1
dtam2$PANETHCENTRAL.W3[dtam2$Q12_3.W3=="Somewhat disagree"] <- 2
dtam2$PANETHCENTRAL.W3[dtam2$Q12_3.W3=="Somewhat agree"] <- 3
dtam2$PANETHCENTRAL.W3[dtam2$Q12_3.W3=="Strongly agree"] <- 4

dtam2$PANETHIDX.W3 <- dtam2$PANETHCENTRAL.W3 + dtam2$PANETHNOTIMP.W3

# cor(dtam2$PANETHNOTIMP.W1,dtam2$PANETHNOTIMP.W3,use="pairwise.complete.obs")
# cor(dtam2$PANETHIDX.W1,dtam2$PANETHIDX.W3,use="pairwise.complete.obs")

dtam2$NATORNOTIMP.W3 <- NA
dtam2$NATORNOTIMP.W3[dtam2$Q13_1.W3=="Strongly disagree"] <- 4
dtam2$NATORNOTIMP.W3[dtam2$Q13_1.W3=="Somewhat disagree"] <- 3
dtam2$NATORNOTIMP.W3[dtam2$Q13_1.W3=="Somewhat agree"] <- 2
dtam2$NATORNOTIMP.W3[dtam2$Q13_1.W3=="Strongly agree"] <- 1

dtam2$NATORCENTRAL.W3 <- NA
dtam2$NATORCENTRAL.W3[dtam2$Q13_2.W3=="Strongly disagree"] <- 1
dtam2$NATORCENTRAL.W3[dtam2$Q13_2.W3=="Somewhat disagree"] <- 2
dtam2$NATORCENTRAL.W3[dtam2$Q13_2.W3=="Somewhat agree"] <- 3
dtam2$NATORCENTRAL.W3[dtam2$Q13_2.W3=="Strongly agree"] <- 4

dtam2$NATORIDX.W3 <- dtam2$NATORCENTRAL.W3 + dtam2$NATORNOTIMP.W3

dtam2$POLKNOW1.W1 <- NA
dtam2$POLKNOW1.W1[dtam2$Q15.W1=="Two-thirds"] <- 1
dtam2$POLKNOW1.W1[dtam2$Q15.W1 %in% c("One half plus one vote","Refused","Three-fifths","Three quarters")] <- 0

dtam2$POLKNOW2.W1 <- NA
dtam2$POLKNOW2.W1[dtam2$Q16.W1 %in% c("Refused",0:20)] <- 0
dtam2$POLKNOW2.W1[dtam2$Q16.W1=="6"] <- 1

dtam2$POLKNOWIDX.W1 <- dtam2$POLKNOW1.W1+dtam2$POLKNOW2.W1

dtam2$PATHWAY1 <- NA
dtam2$PATHWAY1[dtam2$Q9_1.W1=="Return illegal immigrants to their native countries"] <- 1
dtam2$PATHWAY1[dtam2$Q9_1.W1=="2"] <- 2
dtam2$PATHWAY1[dtam2$Q9_1.W1=="3"] <- 3
dtam2$PATHWAY1[dtam2$Q9_1.W1=="4"] <- 4
dtam2$PATHWAY1[dtam2$Q9_1.W1=="5"] <- 5
dtam2$PATHWAY1[dtam2$Q9_1.W1=="6"] <- 6
#dtam2$PATHWAY1[dtam2$Q9_1.W1=="Return illegal immigrants to their native countries"] <- 1
dtam2$PATHWAY1[dtam2$Q9_1.W1=="Create a pathway to U.S. citizenship for illegal immigrants"] <- 7

dtam2$PATHWAY3 <- NA

dtam2$PATHWAY3[dtam2$RE23_A.W3 %in% c("Return illegal immigrants to their native countries") & 
                 ! dtam2$RE23_A.W3 %in% c(NA)] <- 1
dtam2$PATHWAY3[dtam2$RE23_A.W3 %in% c("") & ! dtam2$RE23_A.W3 %in% c(NA)] <- 2
dtam2$PATHWAY3[dtam2$RE23_A.W3 %in% c("_duplicated_3") & ! dtam2$RE23_A.W3 %in% c(NA)] <- 3
dtam2$PATHWAY3[dtam2$RE23_A.W3 %in% c("_duplicated_4") & ! dtam2$RE23_A.W3 %in% c(NA)] <- 4
dtam2$PATHWAY3[dtam2$RE23_A.W3 %in% c("_duplicated_5") & ! dtam2$RE23_A.W3 %in% c(NA)] <- 5
dtam2$PATHWAY3[dtam2$RE23_A.W3 %in% c("_duplicated_6") & ! dtam2$RE23_A.W3 %in% c(NA)] <- 6
dtam2$PATHWAY3[dtam2$RE23_A.W3 %in% c("Create a pathway to U.S. citizenship for illegal immigrants") & 
                 ! dtam2$RE23_A.W3 %in% c(NA)] <- 7

#### employment measures
dtam2$UNEMPLOYED.W1 <- 1*(dtam2$PPWORK.W1 %in% c("Not working - on temporary layoff from a job",
                                                 "Not working - looking for work","Not working - other"))
dtam2$UNEMPLOYED.W3 <- 1*(dtam2$PPWORK.W3 %in% c("Not working - on temporary layoff from a job",
                                                 "Not working - looking for work","Not working - other"))
dtam2$RETIRED.W1 <- 1*(dtam2$PPWORK.W1 %in% c("Not working - retired"))
dtam2$EMPLOYED.W1 <- 1*(dtam2$PPWORK.W1 %in% c("Working - as a paid employee",
                                               "Working - self-employed"))

dtam2$EMPLOYED.W3 <- 1*(dtam2$PPWORK.W3 %in% c("Working - as a paid employee",
                                               "Working - self-employed"))


dtam2$IDEO.W1 <- NA
dtam2$IDEO.W1[dtam2$xIdeo.W1=="Extremely liberal"] <- 1
dtam2$IDEO.W1[dtam2$xIdeo.W1=="Liberal"] <- 2
dtam2$IDEO.W1[dtam2$xIdeo.W1=="Slightly liberal"] <- 3
dtam2$IDEO.W1[dtam2$xIdeo.W1=="Moderate, middle of the road"] <- 4
dtam2$IDEO.W1[dtam2$xIdeo.W1=="Slightly conservative"] <- 5
dtam2$IDEO.W1[dtam2$xIdeo.W1=="Conservative"] <- 6
dtam2$IDEO.W1[dtam2$xIdeo.W1=="Extremely conservative"] <- 7

### employment changes
dtam2$BECOME.EMPLOYED <- 1*(dtam2$EMPLOYED.W1==0 & dtam2$EMPLOYED.W3==1) 
dtam2$BECOME.UNEMPLOYED <- 1*(dtam2$UNEMPLOYED.W1==0 & dtam2$UNEMPLOYED.W3==1) 
table(dtam2$BECOME.UNEMPLOYED)
table(dtam2$BECOME.EMPLOYED)

dtam2$AGE.W1 <- as.numeric(as.character(dtam2$PPAGE.W1))/100
dtam2$INCOMER.W1 <- dtam2$INCOME.W1/1000

dtam2$PID.D31 <- dtam2$PID3-dtam2$PIDPRE.W1

table(dtam2$PID.D31)
table(dtam2$PID.D31[dtam2$SPANISH.W1==1])
sum(table(dtam2$PID.D31[dtam2$SPANISH.W1==1]))

### FATHER
dtam2$FATHERBORNUS <- NA
dtam2$FATHERBORNUS[dtam2$Xphi1401.W1=="United States"] <- 1
dtam2$FATHERBORNUS[dtam2$Xphi1401.W1=="Another country"] <- 0

table(dtam2$FATHERBORNUS)

dtam2$MOTHERBORNUS <- NA
dtam2$MOTHERBORNUS[dtam2$Xphi1402.W1=="United States"] <- 1
dtam2$MOTHERBORNUS[dtam2$Xphi1402.W1=="Another country"] <- 0

### variable only exists for Hispanic respondents
dtam2$SECONDGEN.W1 <- 0
dtam2$SECONDGEN.W1[dtam2$Xphi1401.W1=="United States" | dtam2$Xphi1402.W1=="United States"] <- 0.5
dtam2$SECONDGEN.W1[dtam2$Xphi1401.W1=="United States" & dtam2$Xphi1402.W1=="United States"] <- 1

dtam2$PID.ABS.D <- abs(dtam2$PID3-dtam2$PIDPRE.W1)


dtam2$ACAFAVOR.W3 <- NA
dtam2$ACAFAVOR.W3[dtam2$DH2.W3=="Very favorable"] <- 4
dtam2$ACAFAVOR.W3[dtam2$DH2.W3=="Somewhat favorable"] <- 3
dtam2$ACAFAVOR.W3[dtam2$DH2.W3=="Somewhat unfavorable"] <- 2
dtam2$ACAFAVOR.W3[dtam2$DH2.W3=="Very unfavorable"] <- 1

dtam2$PANETHNOTIMP.W3 <- NA
dtam2$PANETHNOTIMP.W3[dtam2$Q12_1.W3=="Strongly disagree"] <- 4
dtam2$PANETHNOTIMP.W3[dtam2$Q12_1.W3=="Somewhat disagree"] <- 3
dtam2$PANETHNOTIMP.W3[dtam2$Q12_1.W3=="Somewhat agree"] <- 2
dtam2$PANETHNOTIMP.W3[dtam2$Q12_1.W3=="Strongly agree"] <- 1

dtam2$PANETHCENTRAL.W3 <- NA
dtam2$PANETHCENTRAL.W3[dtam2$Q12_3.W3=="Strongly disagree"] <- 1
dtam2$PANETHCENTRAL.W3[dtam2$Q12_3.W3=="Somewhat disagree"] <- 2
dtam2$PANETHCENTRAL.W3[dtam2$Q12_3.W3=="Somewhat agree"] <- 3
dtam2$PANETHCENTRAL.W3[dtam2$Q12_3.W3=="Strongly agree"] <- 4

dtam2$PANETHIDX.W3 <- dtam2$PANETHCENTRAL.W3 + dtam2$PANETHNOTIMP.W3

# cor(dtam2$PANETHNOTIMP.W1,dtam2$PANETHNOTIMP.W3,use="pairwise.complete.obs")
# cor(dtam2$PANETHIDX.W1,dtam2$PANETHIDX.W3,use="pairwise.complete.obs")

dtam2$NATORNOTIMP.W3 <- NA
dtam2$NATORNOTIMP.W3[dtam2$Q13_1.W3=="Strongly disagree"] <- 4
dtam2$NATORNOTIMP.W3[dtam2$Q13_1.W3=="Somewhat disagree"] <- 3
dtam2$NATORNOTIMP.W3[dtam2$Q13_1.W3=="Somewhat agree"] <- 2
dtam2$NATORNOTIMP.W3[dtam2$Q13_1.W3=="Strongly agree"] <- 1

dtam2$NATORCENTRAL.W3 <- NA
dtam2$NATORCENTRAL.W3[dtam2$Q13_2.W3=="Strongly disagree"] <- 1
dtam2$NATORCENTRAL.W3[dtam2$Q13_2.W3=="Somewhat disagree"] <- 2
dtam2$NATORCENTRAL.W3[dtam2$Q13_2.W3=="Somewhat agree"] <- 3
dtam2$NATORCENTRAL.W3[dtam2$Q13_2.W3=="Strongly agree"] <- 4

dtam2$NATORIDX.W3 <- dtam2$NATORCENTRAL.W3 + dtam2$NATORNOTIMP.W3

dtam2$POLKNOW1.W1 <- NA
dtam2$POLKNOW1.W1[dtam2$Q15.W1=="Two-thirds"] <- 1
dtam2$POLKNOW1.W1[dtam2$Q15.W1 %in% c("One half plus one vote","Refused","Three-fifths","Three quarters")] <- 0

dtam2$POLKNOW2.W1 <- NA
dtam2$POLKNOW2.W1[dtam2$Q16.W1 %in% c("Refused",0:20)] <- 0
dtam2$POLKNOW2.W1[dtam2$Q16.W1=="6"] <- 1

dtam2$POLKNOWIDX.W1 <- dtam2$POLKNOW1.W1+dtam2$POLKNOW2.W1

dtam2$PATHWAY1 <- NA
dtam2$PATHWAY1[dtam2$Q9_1.W1=="Return illegal immigrants to their native countries"] <- 1
dtam2$PATHWAY1[dtam2$Q9_1.W1=="2"] <- 2
dtam2$PATHWAY1[dtam2$Q9_1.W1=="3"] <- 3
dtam2$PATHWAY1[dtam2$Q9_1.W1=="4"] <- 4
dtam2$PATHWAY1[dtam2$Q9_1.W1=="5"] <- 5
dtam2$PATHWAY1[dtam2$Q9_1.W1=="6"] <- 6
#dtam2$PATHWAY1[dtam2$Q9_1.W1=="Return illegal immigrants to their native countries"] <- 1
dtam2$PATHWAY1[dtam2$Q9_1.W1=="Create a pathway to U.S. citizenship for illegal immigrants"] <- 7

dtam2$PATHWAY3 <- NA

dtam2$PATHWAY3[dtam2$RE23_A.W3 %in% c("Return illegal immigrants to their native countries") & 
                 ! dtam2$RE23_A.W3 %in% c(NA)] <- 1
dtam2$PATHWAY3[dtam2$RE23_A.W3 %in% c("") & ! dtam2$RE23_A.W3 %in% c(NA)] <- 2
dtam2$PATHWAY3[dtam2$RE23_A.W3 %in% c("_duplicated_3") & ! dtam2$RE23_A.W3 %in% c(NA)] <- 3
dtam2$PATHWAY3[dtam2$RE23_A.W3 %in% c("_duplicated_4") & ! dtam2$RE23_A.W3 %in% c(NA)] <- 4
dtam2$PATHWAY3[dtam2$RE23_A.W3 %in% c("_duplicated_5") & ! dtam2$RE23_A.W3 %in% c(NA)] <- 5
dtam2$PATHWAY3[dtam2$RE23_A.W3 %in% c("_duplicated_6") & ! dtam2$RE23_A.W3 %in% c(NA)] <- 6
dtam2$PATHWAY3[dtam2$RE23_A.W3 %in% c("Create a pathway to U.S. citizenship for illegal immigrants") & 
                 ! dtam2$RE23_A.W3 %in% c(NA)] <- 7

#### employment measures
dtam2$UNEMPLOYED.W1 <- 1*(dtam2$PPWORK.W1 %in% c("Not working - on temporary layoff from a job",
                                                 "Not working - looking for work","Not working - other"))
dtam2$UNEMPLOYED.W3 <- 1*(dtam2$PPWORK.W3 %in% c("Not working - on temporary layoff from a job",
                                                 "Not working - looking for work","Not working - other"))
dtam2$RETIRED.W1 <- 1*(dtam2$PPWORK.W1 %in% c("Not working - retired"))
dtam2$EMPLOYED.W1 <- 1*(dtam2$PPWORK.W1 %in% c("Working - as a paid employee",
                                               "Working - self-employed"))

dtam2$EMPLOYED.W3 <- 1*(dtam2$PPWORK.W3 %in% c("Working - as a paid employee",
                                               "Working - self-employed"))


dtam2$IDEO.W1 <- NA
dtam2$IDEO.W1[dtam2$xIdeo.W1=="Extremely liberal"] <- 1
dtam2$IDEO.W1[dtam2$xIdeo.W1=="Liberal"] <- 2
dtam2$IDEO.W1[dtam2$xIdeo.W1=="Slightly liberal"] <- 3
dtam2$IDEO.W1[dtam2$xIdeo.W1=="Moderate, middle of the road"] <- 4
dtam2$IDEO.W1[dtam2$xIdeo.W1=="Slightly conservative"] <- 5
dtam2$IDEO.W1[dtam2$xIdeo.W1=="Conservative"] <- 6
dtam2$IDEO.W1[dtam2$xIdeo.W1=="Extremely conservative"] <- 7

### employment changes
dtam2$BECOME.EMPLOYED <- 1*(dtam2$EMPLOYED.W1==0 & dtam2$EMPLOYED.W3==1) 
dtam2$BECOME.UNEMPLOYED <- 1*(dtam2$UNEMPLOYED.W1==0 & dtam2$UNEMPLOYED.W3==1) 
table(dtam2$BECOME.UNEMPLOYED)
table(dtam2$BECOME.EMPLOYED)

dtam2$AGE.W1 <- as.numeric(as.character(dtam2$PPAGE.W1))/100
dtam2$INCOMER.W1 <- dtam2$INCOME.W1/1000

dtam2$PID.D31 <- dtam2$PID3-dtam2$PIDPRE.W1

dtam2$FATHERBORNUS <- NA
dtam2$FATHERBORNUS[dtam2$Xphi1401.W1=="United States"] <- 1
dtam2$FATHERBORNUS[dtam2$Xphi1401.W1=="Another country"] <- 0

table(dtam2$FATHERBORNUS)

dtam2$MOTHERBORNUS <- NA
dtam2$MOTHERBORNUS[dtam2$Xphi1402.W1=="United States"] <- 1
dtam2$MOTHERBORNUS[dtam2$Xphi1402.W1=="Another country"] <- 0

### variable only exists for Hispanic respondents
dtam2$SECONDGEN.W1 <- 0
dtam2$SECONDGEN.W1[dtam2$Xphi1401.W1=="United States" | dtam2$Xphi1402.W1=="United States"] <- 0.5
dtam2$SECONDGEN.W1[dtam2$Xphi1401.W1=="United States" & dtam2$Xphi1402.W1=="United States"] <- 1

dtam2$PID.ABS.D <- abs(dtam2$PID3-dtam2$PIDPRE.W1)


dtam2$ACAFAVOR.W3 <- NA
dtam2$ACAFAVOR.W3[dtam2$DH2.W3=="Very favorable"] <- 4
dtam2$ACAFAVOR.W3[dtam2$DH2.W3=="Somewhat favorable"] <- 3
dtam2$ACAFAVOR.W3[dtam2$DH2.W3=="Somewhat unfavorable"] <- 2
dtam2$ACAFAVOR.W3[dtam2$DH2.W3=="Very unfavorable"] <- 1

dtam2$RANKNATOR.W3 <- NA
dtam2$RANKNATOR.W3[dtam2$NEW2_6.W3=="1"] <- 1
dtam2$RANKNATOR.W3[dtam2$NEW2_6.W3=="2"] <- 2
dtam2$RANKNATOR.W3[dtam2$NEW2_6.W3=="3"] <- 3
dtam2$RANKNATOR.W3[dtam2$NEW2_6.W3=="4"] <- 4
dtam2$RANKNATOR.W3[dtam2$NEW2_6.W3=="5"] <- 5
dtam2$RANKNATOR.W3[dtam2$NEW2_6.W3=="6"] <- 6
dtam2$RANKNATOR.W3[dtam2$NEW2_6.W3=="7"] <- 7
dtam2$RANKNATOR.W3[dtam2$NEW2_6.W3=="8"] <- 8

dtam2$RANKPANETH.W3 <- NA
dtam2$RANKPANETH.W3[dtam2$NEW2_7.W3=="1"] <- 1
dtam2$RANKPANETH.W3[dtam2$NEW2_7.W3=="2"] <- 2
dtam2$RANKPANETH.W3[dtam2$NEW2_7.W3=="3"] <- 3
dtam2$RANKPANETH.W3[dtam2$NEW2_7.W3=="4"] <- 4
dtam2$RANKPANETH.W3[dtam2$NEW2_7.W3=="5"] <- 5
dtam2$RANKPANETH.W3[dtam2$NEW2_7.W3=="6"] <- 6
dtam2$RANKPANETH.W3[dtam2$NEW2_7.W3=="7"] <- 7
dtam2$RANKPANETH.W3[dtam2$NEW2_7.W3=="8"] <- 8

dtam2$RANKAMER.W3 <- NA
dtam2$RANKAMER.W3[dtam2$NEW2_8.W3=="1"] <- 1
dtam2$RANKAMER.W3[dtam2$NEW2_8.W3=="2"] <- 2
dtam2$RANKAMER.W3[dtam2$NEW2_8.W3=="3"] <- 3
dtam2$RANKAMER.W3[dtam2$NEW2_8.W3=="4"] <- 4
dtam2$RANKAMER.W3[dtam2$NEW2_8.W3=="5"] <- 5
dtam2$RANKAMER.W3[dtam2$NEW2_8.W3=="6"] <- 6
dtam2$RANKAMER.W3[dtam2$NEW2_8.W3=="7"] <- 7
dtam2$RANKAMER.W3[dtam2$NEW2_8.W3=="8"] <- 8


dtam2$RANKPARTY.W3 <- NA
dtam2$RANKPARTY.W3[dtam2$NEW2_4.W3=="1"] <- 1
dtam2$RANKPARTY.W3[dtam2$NEW2_4.W3=="2"] <- 2
dtam2$RANKPARTY.W3[dtam2$NEW2_4.W3=="3"] <- 3
dtam2$RANKPARTY.W3[dtam2$NEW2_4.W3=="4"] <- 4
dtam2$RANKPARTY.W3[dtam2$NEW2_4.W3=="5"] <- 5
dtam2$RANKPARTY.W3[dtam2$NEW2_4.W3=="6"] <- 6
dtam2$RANKPARTY.W3[dtam2$NEW2_4.W3=="7"] <- 7
dtam2$RANKPARTY.W3[dtam2$NEW2_4.W3=="8"] <- 8


#####
dtam2$RANKNATOR.W2 <- NA
dtam2$RANKNATOR.W2[dtam2$NEW2_6.W2=="1"] <- 1
dtam2$RANKNATOR.W2[dtam2$NEW2_6.W2=="2"] <- 2
dtam2$RANKNATOR.W2[dtam2$NEW2_6.W2=="3"] <- 3
dtam2$RANKNATOR.W2[dtam2$NEW2_6.W2=="4"] <- 4
dtam2$RANKNATOR.W2[dtam2$NEW2_6.W2=="5"] <- 5
dtam2$RANKNATOR.W2[dtam2$NEW2_6.W2=="6"] <- 6
dtam2$RANKNATOR.W2[dtam2$NEW2_6.W2=="7"] <- 7
dtam2$RANKNATOR.W2[dtam2$NEW2_6.W2=="8"] <- 8

dtam2$RANKPANETH.W2 <- NA
dtam2$RANKPANETH.W2[dtam2$NEW2_7.W2=="1"] <- 1
dtam2$RANKPANETH.W2[dtam2$NEW2_7.W2=="2"] <- 2
dtam2$RANKPANETH.W2[dtam2$NEW2_7.W2=="3"] <- 3
dtam2$RANKPANETH.W2[dtam2$NEW2_7.W2=="4"] <- 4
dtam2$RANKPANETH.W2[dtam2$NEW2_7.W2=="5"] <- 5
dtam2$RANKPANETH.W2[dtam2$NEW2_7.W2=="6"] <- 6
dtam2$RANKPANETH.W2[dtam2$NEW2_7.W2=="7"] <- 7
dtam2$RANKPANETH.W2[dtam2$NEW2_7.W2=="8"] <- 8

dtam2$RANKAMER.W2 <- NA
dtam2$RANKAMER.W2[dtam2$NEW2_8.W2=="1"] <- 1
dtam2$RANKAMER.W2[dtam2$NEW2_8.W2=="2"] <- 2
dtam2$RANKAMER.W2[dtam2$NEW2_8.W2=="3"] <- 3
dtam2$RANKAMER.W2[dtam2$NEW2_8.W2=="4"] <- 4
dtam2$RANKAMER.W2[dtam2$NEW2_8.W2=="5"] <- 5
dtam2$RANKAMER.W2[dtam2$NEW2_8.W2=="6"] <- 6
dtam2$RANKAMER.W2[dtam2$NEW2_8.W2=="7"] <- 7
dtam2$RANKAMER.W2[dtam2$NEW2_8.W2=="8"] <- 8

dtam2$RANKPARTY.W2 <- NA
dtam2$RANKPARTY.W2[dtam2$NEW2_4.W2=="1"] <- 1
dtam2$RANKPARTY.W2[dtam2$NEW2_4.W2=="2"] <- 2
dtam2$RANKPARTY.W2[dtam2$NEW2_4.W2=="3"] <- 3
dtam2$RANKPARTY.W2[dtam2$NEW2_4.W2=="4"] <- 4
dtam2$RANKPARTY.W2[dtam2$NEW2_4.W2=="5"] <- 5
dtam2$RANKPARTY.W2[dtam2$NEW2_4.W2=="6"] <- 6
dtam2$RANKPARTY.W2[dtam2$NEW2_4.W2=="7"] <- 7
dtam2$RANKPARTY.W2[dtam2$NEW2_4.W2=="8"] <- 8

dtam2$PDJOB.W3 <- NA
dtam2$PDJOB.W3[dtam2$Q6_1.W3=="Never"] <- 0
dtam2$PDJOB.W3[dtam2$Q6_1.W3=="Once"] <- 1
dtam2$PDJOB.W3[dtam2$Q6_1.W3=="Sometimes"] <- 2
dtam2$PDJOB.W3[dtam2$Q6_1.W3=="Occasionally"] <- 3
dtam2$PDJOB.W3[dtam2$Q6_1.W3=="Frequently"] <- 4

dtam2$PDPOLICE.W3 <- NA
dtam2$PDPOLICE.W3[dtam2$Q6_2.W3=="Never"] <- 0
dtam2$PDPOLICE.W3[dtam2$Q6_2.W3=="Once"] <- 1
dtam2$PDPOLICE.W3[dtam2$Q6_2.W3=="Sometimes"] <- 2
dtam2$PDPOLICE.W3[dtam2$Q6_2.W3=="Occasionally"] <- 3
dtam2$PDPOLICE.W3[dtam2$Q6_2.W3=="Frequently"] <- 4

dtam2$PDRENT.W3 <- NA
dtam2$PDRENT.W3[dtam2$Q6_3.W3=="Never"] <- 0
dtam2$PDRENT.W3[dtam2$Q6_3.W3=="Once"] <- 1
dtam2$PDRENT.W3[dtam2$Q6_3.W3=="Sometimes"] <- 2
dtam2$PDRENT.W3[dtam2$Q6_3.W3=="Occasionally"] <- 3
dtam2$PDRENT.W3[dtam2$Q6_3.W3=="Frequently"] <- 4

dtam2$PDSTORE.W3 <- NA
dtam2$PDSTORE.W3[dtam2$Q6_4.W3=="Never"] <- 0
dtam2$PDSTORE.W3[dtam2$Q6_4.W3=="Once"] <- 1
dtam2$PDSTORE.W3[dtam2$Q6_4.W3=="Sometimes"] <- 2
dtam2$PDSTORE.W3[dtam2$Q6_4.W3=="Occasionally"] <- 3
dtam2$PDSTORE.W3[dtam2$Q6_4.W3=="Frequently"] <- 4

dtam2$PDINDEX.W3 <- dtam2$PDJOB.W3+dtam2$PDPOLICE.W3+dtam2$PDRENT.W3+dtam2$PDSTORE.W3

#~/Dropbox/PDparty/rcode/replication/
dta.can <- read.csv("candidate_data_sep.csv")
#colnames(dta.can)[7] <- "DistrictNum"

dta.can$DistrictNum <- NA
dta.can$DistrictNum <- gsub("[[:space:]]", "", dta.can$District.Num)
dta.can$DistrictNum <- gsub("\xa0", "", dta.can$District.Num)

#sum(dta12$DistrictNum %in% dta.can$DistrictNum)

dtam2$DistrictNum <- paste(dtam2$PPSTATEN.W1,"-",dtam2$XCID.W3,sep="")

sum(dtam2$DistrictNum %in% dta.can$DistrictNum)
dtam3 <- merge(dtam2,dta.can,by=c("DistrictNum"),all.x=T)

dtam3$GOPREPVD18B <- NA
dtam3$GOPREPVD18B[dtam3$VP13.W3=="[Candidate_1 name], the [Candidate_1 party]" 
                  & dtam3$Candidate.1.Party=="Democratic"  & ! dtam3$Candidate.1.Party %in% c("NA")] <- -1
dtam3$GOPREPVD18B[dtam3$VP13.W3=="[Candidate_1 name], the [Candidate_1 party]" 
                  & dtam3$Candidate.1.Party=="Republican"  & ! dtam3$Candidate.1.Party %in% c("NA")] <- 1

dtam3$GOPREPVD18B[dtam3$VP13.W3=="[Candidate_2 name], the [Candidate_2 party]" & dtam3$Candidate.2.Party=="Democratic" & ! dtam3$Candidate.2.Party %in% c("NA")] <- -1
dtam3$GOPREPVD18B[dtam3$VP13.W3=="[Candidate_2 name], the [Candidate_2 party]" & dtam3$Candidate.2.Party=="Republican" & ! dtam3$Candidate.2.Party %in% c("NA")] <- 1

dtam3$GOPREPVD18B[dtam3$VP13a.W3=="Democratic candidate" & ! dtam3$VP13a.W3 %in% c(NA)] <- -1
dtam3$GOPREPVD18B[dtam3$VP13a.W3=="Republican candidate" & ! dtam3$VP13a.W3 %in% c(NA)] <- 1
dtam3$GOPREPVD18B[dtam3$VP13.W3 %in% c("Someone else","Don’t know","Refused")] <- 0

#### FIGURE 2 top panels ----
##### Asian Americans

t1p <- table(dtam2$PID3[dtam2$HISPANIC.W1==0]-dtam2$PIDPRE[dtam2$HISPANIC.W1==0])

(17+161+27)/sum(t1p)
#[1] 0.8951965

tt1 <- table(dtam2$PIDPRE[dtam2$HISPANIC.W1==0],dtam2$PID3[dtam2$HISPANIC.W1==0])

sum(tt1[4,])/sum(tt1)
sum(tt1[,4])/sum(tt1)

sum(apply(tt1[3:5,],1,sum))/sum(tt1)
sum(apply(tt1[,3:5],2,sum))/sum(tt1)

sum(tt1)
#[1] 229
sum(diag(tt1))
#[1] 161
sum(diag(tt1))/sum(tt1)
#[1] 0.7030568

tt2 <- round(prop.table(tt1),digits=3)
colnames(tt2) <- rownames(tt2) <- c("SD","WD","ID","I","IR","WR","SR")

#rownames(tt2) <- c("Dem 16","Neither 16","GOP 16")
#colnames(tt2) <- c("Dem 18","Neither 18","GOP 18")

co=melt(tt2)
head(co)
colnames(co) <- c("Wave1","Wave3","value")

#pdf("partisan-transition-asn-06152022.pdf")

ggplot(co, aes(Wave1, Wave3)) + # x and y axes => Var1 and Var2
  geom_tile(aes(fill = value)) + # background colours are mapped according to the value column
  #theme_gray(base_size=24) +
  geom_text(aes(fill = value, label = round(value, 3)),size=4) + # write the values
  scale_fill_gradient2(low = "#F2F2F2", 
                       #mid = "#B7B7B7", 
                       high = "#7F7F7F")+#, 
  #midpoint = mean(co$value)) + # determine the colour
  #scale_colour_grey()+theme_bw()+
  theme(panel.grid.major.x=element_blank(), #no gridlines
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), # background=white
        axis.text.x = element_text(angle=60, hjust = 1,vjust=1,size = 16,face = "bold"),
        axis.title.x = element_text(size=18,face="bold"),
        plot.title = element_text(size=20,face="bold"),
        axis.text.y = element_text(size = 16,face = "bold"),
        axis.title.y = element_text(size=18,face="bold"))+
  ggtitle("Partisan Transitions: As. Americans") + 
  theme(legend.title=element_text(face="bold", size=12)) + 
  scale_x_discrete(name="2016") +
  scale_y_discrete(name="2018") +
  labs(fill="Share of As. Americans")

dev.off()

tt1 <- table(dtam2$PIDPRE[dtam2$HISPANIC.W1==1],dtam2$PID3[dtam2$HISPANIC.W1==1])
sum(tt1)
#[1] 220
sum(diag(tt1))
#124
sum(diag(tt1))/sum(tt1)
#[1] 0.5636364

sum(apply(tt1[3:5,],1,sum))/sum(tt1)
sum(apply(tt1[,3:5],2,sum))/sum(tt1)

###
sum(apply(tt1[3:5,],1,sum))/sum(tt1)
sum(apply(tt1[,3:5],2,sum))/sum(tt1)

sum(tt1[4,])/sum(tt1)
sum(tt1[,4])/sum(tt1)


t1p <- table(dtam2$PID3[dtam2$HISPANIC.W1==1]-dtam2$PIDPRE[dtam2$HISPANIC.W1==1])
(23+124+36)/sum(t1p)

tt2 <- round(prop.table(tt1),digits=3)
colnames(tt2) <- rownames(tt2) <- c("SD","WD","ID","I","IR","WR","SR")

#rownames(tt2) <- c("Dem 16","Neither 16","GOP 16")
#colnames(tt2) <- c("Dem 18","Neither 18","GOP 18")

co=melt(tt2)
head(co)
colnames(co) <- c("Wave1","Wave3","value")

#pdf("partisan-transition-hsp-06152022.pdf")

ggplot(co, aes(Wave1, Wave3)) + # x and y axes => Var1 and Var2
  geom_tile(aes(fill = value)) + # background colours are mapped according to the value column
  #theme_gray(base_size=24) +
  geom_text(aes(fill = value, label = round(value, 3)),size=4) + # write the values
  scale_fill_gradient2(low = "#F2F2F2", 
                       #mid = "#B7B7B7", 
                       high = "#7F7F7F")+#, 
  #midpoint = mean(co$value)) + # determine the colour
  #scale_colour_grey()+theme_bw()+
  theme(panel.grid.major.x=element_blank(), #no gridlines
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), # background=white
        axis.text.x = element_text(angle=60, hjust = 1,vjust=1,size = 16,face = "bold"),
        axis.title.x = element_text(size=18,face="bold"),
        plot.title = element_text(size=20,face="bold"),
        axis.text.y = element_text(size = 16,face = "bold"),
        axis.title.y = element_text(size=18,face="bold"))+
  ggtitle("Partisan Transitions: Latinos") + 
  theme(legend.title=element_text(face="bold", size=12)) + 
  scale_x_discrete(name="2016") +
  scale_y_discrete(name="2018") +
  labs(fill="Share of Latinos")

#dev.off()


#### pid over time figure (FIGURE 1) ----
ubmat <- lbmat <- rmat <- matrix(NA,3,3)
rmat[1,1] <- mean(dtam2$PIDPRE.W1[dtam2$HISPANIC.W1==1 & dtam2$SPANISH.W1==1 & ! dtam2$PID3 %in% c(NA)],na.rm=T)
rmat[1,2] <- mean(dtam2$PID2[dtam2$HISPANIC.W1==1 & dtam2$SPANISH.W1==1 & ! dtam2$PID3 %in% c(NA)],na.rm=T)
rmat[1,3] <- mean(dtam2$PID3[dtam2$HISPANIC.W1==1 & dtam2$SPANISH.W1==1 & ! dtam2$PID3 %in% c(NA)],na.rm=T)

length(na.omit(dtam2$PID3[dtam2$HISPANIC.W1==1 & dtam2$SPANISH.W1==1 & ! dtam2$PID3 %in% c(NA)]))

out1 <- calculate.vec(M=10000,vec=dtam2$PIDPRE.W1[dtam2$HISPANIC.W1==1 & dtam2$SPANISH.W1==1 & ! dtam2$PID3 %in% c(NA)])
out2 <- calculate.vec(M=10000,vec=dtam2$PID2[dtam2$HISPANIC.W1==1 & dtam2$SPANISH.W1==1 & ! dtam2$PID3 %in% c(NA)])
out3 <- calculate.vec(M=10000,vec=dtam2$PID3[dtam2$HISPANIC.W1==1 & dtam2$SPANISH.W1==1 & ! dtam2$PID3 %in% c(NA)])

lbmat[1,1] <-out1[1]
ubmat[1,1] <-out1[2]

lbmat[1,2] <-out2[1]
ubmat[1,2] <-out2[2]

lbmat[1,3] <-out3[1]
ubmat[1,3] <-out3[2]

rmat[2,1] <- mean(dtam2$PIDPRE.W1[dtam2$HISPANIC.W1==1 & dtam2$SPANISH.W1==0 & ! dtam2$PID3 %in% c(NA)],na.rm=T)
rmat[2,2] <- mean(dtam2$PID2[dtam2$HISPANIC.W1==1 & dtam2$SPANISH.W1==0 & ! dtam2$PID3 %in% c(NA)],na.rm=T)
rmat[2,3] <- mean(dtam2$PID3[dtam2$HISPANIC.W1==1 & dtam2$SPANISH.W1==0 & ! dtam2$PID3 %in% c(NA)],na.rm=T)

length(na.omit(dtam2$PID3[dtam2$HISPANIC.W1==1 & dtam2$SPANISH.W1==0 & ! dtam2$PID3 %in% c(NA)]))

out1b <- calculate.vec(M=10000,vec=dtam2$PIDPRE.W1[dtam2$HISPANIC.W1==1 & dtam2$SPANISH.W1==0 & ! dtam2$PID3 %in% c(NA)])
out2b <- calculate.vec(M=10000,vec=dtam2$PID2[dtam2$HISPANIC.W1==1 & dtam2$SPANISH.W1==0 & ! dtam2$PID3 %in% c(NA)])
out3b <- calculate.vec(M=10000,vec=dtam2$PID3[dtam2$HISPANIC.W1==1 & dtam2$SPANISH.W1==0 & ! dtam2$PID3 %in% c(NA)])

lbmat[2,1] <-out1b[1]
ubmat[2,1] <-out1b[2]

lbmat[2,2] <-out2b[1]
ubmat[2,2] <-out2b[2]

lbmat[2,3] <-out3b[1]
ubmat[2,3] <-out3b[2]

rmat[3,1] <- mean(dtam2$PIDPRE.W1[dtam2$HISPANIC.W1==0 & dtam2$SPANISH.W1==0 & ! dtam2$PID3 %in% c(NA)],na.rm=T)
rmat[3,2] <- mean(dtam2$PID2[dtam2$HISPANIC.W1==0 & dtam2$SPANISH.W1==0 & ! dtam2$PID3 %in% c(NA)],na.rm=T)
rmat[3,3] <- mean(dtam2$PID3[dtam2$HISPANIC.W1==0 & dtam2$SPANISH.W1==0 & ! dtam2$PID3 %in% c(NA)],na.rm=T)

out1c <- calculate.vec(M=10000,vec=dtam2$PIDPRE.W1[dtam2$HISPANIC.W1==0 & dtam2$SPANISH.W1==0 & ! dtam2$PID3 %in% c(NA)])
out2c <- calculate.vec(M=10000,vec=dtam2$PID2[dtam2$HISPANIC.W1==0 & dtam2$SPANISH.W1==0 & ! dtam2$PID3 %in% c(NA)])
out3c <- calculate.vec(M=10000,vec=dtam2$PID3[dtam2$HISPANIC.W1==0 & dtam2$SPANISH.W1==0 & ! dtam2$PID3 %in% c(NA)])

lbmat[3,1] <-out1c[1]
ubmat[3,1] <-out1c[2]

lbmat[3,2] <-out2c[1]
ubmat[3,2] <-out2c[2]

lbmat[3,3] <-out3c[1]
ubmat[3,3] <-out3c[2]

length(na.omit(dtam2$PID3[dtam2$HISPANIC.W1==0 & dtam2$SPANISH.W1==0 & ! dtam2$PID3 %in% c(NA)]))

#jpeg("/users/danhop/Dropbox/PDparty/figures/pid-by-group-01212021.jpg",
#     unit="in",height=6,width=6,res=300)#,width=1440)
#pdf("~/Dropbox/PDparty/figures/pid-by-group-02182022.pdf")
plot(y=rmat[1,],x=c(2016.33333,2016.833333,2018.83333),xlim=c(2016,2019),
     xaxt="n",xlab="",lty=1,pch=16,type="b",ylim=c(1,7),yaxt="n",
     ylab="Party ID",cex.lab=1.25,cex.axis=1.5)
par(new=T)
plot(y=rmat[2,],x=c(2016.33333,2016.833333,2018.83333)+.1,xlim=c(2016,2019),
     xaxt="n",xlab="",lty=2,pch=17,type="b",ylim=c(1,7),
     ylab="",cex.lab=1.25,cex.axis=1.5,yaxt="n")
par(new=T)
plot(y=rmat[3,],x=c(2016.33333,2016.833333,2018.83333)-.1,xlim=c(2016,2019),
     xaxt="n",xlab="",lty=3,pch=18,type="b",ylim=c(1,7),
     ylab="",cex.lab=1.25,cex.axis=1.5,yaxt="n")
axis(side=2,at=1:7,labels=c("SD","WD","ID","I","IR","WR","SR"),cex.axis=1.5)
axis(side=1,at=c(2016,2017,2018,2019),labels=c(2016,2017,2018,2019),cex.axis=1.5)
text(x=2017,y=3.75,"Asian Americans (English)",cex.axis=1.5,cex=1.5)
text(x=2017,y=2.75,"Latinos (English)",cex.axis=1.5,cex=1.5)
text(x=2017,y=1.85,"Latinos (Spanish)",cex.axis=1.5,cex=1.5)
for(i in 1:3){
  if(i==1){
    jitter <- 0
  }
  if(i==2){
    jitter <- .1
  }
  if(i==3){
    jitter <- -.1
  }
  lines(x=c(2016.33333,2016.33333)+jitter,y=c(lbmat[i,1],ubmat[i,1]),lty=2)
  lines(x=c(2016.833333,2016.833333)+jitter,y=c(lbmat[i,2],ubmat[i,2]),lty=2)
  lines(x=c(2018.83333,2018.83333)+jitter,y=c(lbmat[i,3],ubmat[i,3]),lty=2)
}
dev.off()

#### trump ft figure FIGURE 1----

### changes driven by outliers?
M <- 10000
holdvec <- c()
for(i in 1:M){
  n2 <- length(dtam2$FTTRUMP.W2[dtam2$HISPANIC.W1==1 & dtam2$SPANISH.W1==1 & ! dtam2$FTTRUMP.W3 %in% c(NA)])
  rs2 <- sample(dtam2$FTTRUMP.W2[dtam2$HISPANIC.W1==1 & dtam2$SPANISH.W1==1 & ! dtam2$FTTRUMP.W3 %in% c(NA)],
                size=n2,replace=T)
  
  n3 <- length(dtam2$FTTRUMP.W3[dtam2$HISPANIC.W1==1 & dtam2$SPANISH.W1==1 & ! dtam2$FTTRUMP.W3 %in% c(NA)])
  rs3 <- sample(dtam2$FTTRUMP.W3[dtam2$HISPANIC.W1==1 & dtam2$SPANISH.W1==1 & ! dtam2$FTTRUMP.W3 %in% c(NA)],
                size=n3,replace=T)
  
  holdvec[i] <- median(rs3)-median(rs2)
}

ubmat <- lbmat <- rmat <- matrix(NA,3,3)
rmat[1,1] <- mean(dtam2$FTTRUMP.W1[dtam2$HISPANIC.W1==1 & dtam2$SPANISH.W1==1 & ! dtam2$FTTRUMP.W3 %in% c(NA)],
                  na.rm=T)
rmat[1,2] <- mean(dtam2$FTTRUMP.W2[dtam2$HISPANIC.W1==1 & dtam2$SPANISH.W1==1 & ! dtam2$FTTRUMP.W3 %in% c(NA)],
                  na.rm=T)
rmat[1,3] <- mean(dtam2$FTTRUMP.W3[dtam2$HISPANIC.W1==1 & dtam2$SPANISH.W1==1 & ! dtam2$FTTRUMP.W3 %in% c(NA)],
                  na.rm=T)

out1 <- calculate.vec(M=10000,vec=dtam2$FTTRUMP.W1[dtam2$HISPANIC.W1==1 & dtam2$SPANISH.W1==1 & ! dtam2$FTTRUMP.W3 %in% c(NA)])
out2 <- calculate.vec(M=10000,vec=dtam2$FTTRUMP.W2[dtam2$HISPANIC.W1==1 & dtam2$SPANISH.W1==1 & ! dtam2$FTTRUMP.W3 %in% c(NA)])
out3 <- calculate.vec(M=10000,vec=dtam2$FTTRUMP.W3[dtam2$HISPANIC.W1==1 & dtam2$SPANISH.W1==1 & ! dtam2$FTTRUMP.W3 %in% c(NA)])

lbmat[1,1] <-out1[1]
ubmat[1,1] <-out1[2]

lbmat[1,2] <-out2[1]
ubmat[1,2] <-out2[2]

lbmat[1,3] <-out3[1]
ubmat[1,3] <-out3[2]

rmat[2,1] <- mean(dtam2$FTTRUMP.W1[dtam2$HISPANIC.W1==1 & dtam2$SPANISH.W1==0 & ! dtam2$FTTRUMP.W3 %in% c(NA)],na.rm=T)
rmat[2,2] <- mean(dtam2$FTTRUMP.W2[dtam2$HISPANIC.W1==1 & dtam2$SPANISH.W1==0 & ! dtam2$FTTRUMP.W3 %in% c(NA)],na.rm=T)
rmat[2,3] <- mean(dtam2$FTTRUMP.W3[dtam2$HISPANIC.W1==1 & dtam2$SPANISH.W1==0 & ! dtam2$FTTRUMP.W3 %in% c(NA)],na.rm=T)
length(dtam2$FTTRUMP.W3[dtam2$HISPANIC.W1==1 & dtam2$SPANISH.W1==0 & ! dtam2$FTTRUMP.W3 %in% c(NA)])

out1b <- calculate.vec(M=10000,vec=dtam2$FTTRUMP.W1[dtam2$HISPANIC.W1==1 & dtam2$SPANISH.W1==0 & ! dtam2$FTTRUMP.W3 %in% c(NA)])
out2b <- calculate.vec(M=10000,vec=dtam2$FTTRUMP.W2[dtam2$HISPANIC.W1==1 & dtam2$SPANISH.W1==0 & ! dtam2$FTTRUMP.W3 %in% c(NA)])
out3b <- calculate.vec(M=10000,vec=dtam2$FTTRUMP.W3[dtam2$HISPANIC.W1==1 & dtam2$SPANISH.W1==0 & ! dtam2$FTTRUMP.W3 %in% c(NA)])

lbmat[2,1] <-out1b[1]
ubmat[2,1] <-out1b[2]

lbmat[2,2] <-out2b[1]
ubmat[2,2] <-out2b[2]

lbmat[2,3] <-out3b[1]
ubmat[2,3] <-out3b[2]

rmat[3,1] <- mean(dtam2$FTTRUMP.W1[dtam2$HISPANIC.W1==0 & dtam2$SPANISH.W1==0 & ! dtam2$FTTRUMP.W3 %in% c(NA)],na.rm=T)
rmat[3,2] <- mean(dtam2$FTTRUMP.W2[dtam2$HISPANIC.W1==0 & dtam2$SPANISH.W1==0 & ! dtam2$FTTRUMP.W3 %in% c(NA)],na.rm=T)
rmat[3,3] <- mean(dtam2$FTTRUMP.W3[dtam2$HISPANIC.W1==0 & dtam2$SPANISH.W1==0 & ! dtam2$FTTRUMP.W3 %in% c(NA)],na.rm=T)
length(dtam2$FTTRUMP.W3[dtam2$HISPANIC.W1==0 & dtam2$SPANISH.W1==0 & ! dtam2$FTTRUMP.W3 %in% c(NA)])

out1c <- calculate.vec(M=10000,vec=dtam2$FTTRUMP.W1[dtam2$HISPANIC.W1==0 & dtam2$SPANISH.W1==0 & ! dtam2$FTTRUMP.W3 %in% c(NA)])
out2c <- calculate.vec(M=10000,vec=dtam2$FTTRUMP.W2[dtam2$HISPANIC.W1==0 & dtam2$SPANISH.W1==0 & ! dtam2$FTTRUMP.W3 %in% c(NA)])
out3c <- calculate.vec(M=10000,vec=dtam2$FTTRUMP.W3[dtam2$HISPANIC.W1==0 & dtam2$SPANISH.W1==0 & ! dtam2$FTTRUMP.W3 %in% c(NA)])

lbmat[3,1] <-out1c[1]
ubmat[3,1] <-out1c[2]

lbmat[3,2] <-out2c[1]
ubmat[3,2] <-out2c[2]

lbmat[3,3] <-out3c[1]
ubmat[3,3] <-out3c[2]

#pdf("~/Dropbox/PDparty/figures/trump-ft-by-group-02012022.pdf")
plot(y=rmat[1,],x=c(2016.33333,2016.833333,2018.83333),xlim=c(2016,2019),
     xaxt="n",xlab="",lty=1,pch=16,type="b",ylim=c(0,50),
     ylab="Trump Feeling Thermometer",cex.axis=1.5,cex.lab=1.3)
par(new=T)
plot(y=rmat[2,],x=c(2016.33333,2016.833333,2018.83333)+.1,xlim=c(2016,2019),
     xaxt="n",xlab="",lty=2,pch=17,type="b",ylim=c(0,50),
     ylab="",cex.axis=1.5,cex.lab=1.3)
par(new=T)
plot(y=rmat[3,],x=c(2016.33333,2016.833333,2018.83333)-.1,xlim=c(2016,2019),
     xaxt="n",xlab="",lty=3,pch=18,type="b",ylim=c(0,50),
     ylab="",cex.axis=1.5,cex.lab=1.3)
axis(side=1,at=c(2016,2017,2018,2019),labels=c(2016,2017,2018,2019),cex.axis=1.5)
text(x=2017,y=33,"Asian Americans (English)",cex=1.5)
text(x=2016.5,y=20.5,"Latinos (English)",cex=1.5)
text(x=2017.5,y=11,"Latinos (Spanish)",cex=1.5)

for(i in 1:3){
  if(i==1){
    jitter <- 0
  }
  if(i==2){
    jitter <- .1
  }
  if(i==3){
    jitter <- -.1
  }
  lines(x=c(2016.33333,2016.33333)+jitter,y=c(lbmat[i,1],ubmat[i,1]),lty=2)
  lines(x=c(2016.833333,2016.833333)+jitter,y=c(lbmat[i,2],ubmat[i,2]),lty=2)
  lines(x=c(2018.83333,2018.83333)+jitter,y=c(lbmat[i,3],ubmat[i,3]),lty=2)
}

dev.off()


####### party transitions FIGURE 3 ----


t1 <- table(dtam3$TRUMPVCB.W1[dtam3$HISPANIC.W1==1],dtam3$GOPREPVD18B[dtam3$HISPANIC.W1==1])
tt3 <- t1/sum(t1)
sum(t1)

#co=melt(tt3)
#head(co)

lab1 <- c("Dem","Undec","GOP")
colnames(tt3) <- rownames(tt3) <- lab1

co=melt(tt3)
head(co)
colnames(co) <- c("Wave1","Wave3","value")

#pdf("voting-transition-hsp-02022022.pdf")
ggplot(co, aes(Wave1, Wave3)) + # x and y axes => Var1 and Var2
  geom_tile(aes(fill = value)) + # background colours are mapped according to the value column
  #theme_gray(base_size=24) +
  geom_text(aes(fill = value, label = round(value, 2)),size=12) + # write the values
  scale_fill_gradient2(low = "#F2F2F2", 
                       #mid = "#B7B7B7", 
                       high = "#7F7F7F")+#, 
  #midpoint = mean(co$value)) + # determine the colour
  #scale_colour_grey()+theme_bw()+
  theme(panel.grid.major.x=element_blank(), #no gridlines
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), # background=white
        axis.text.x = element_text(angle=60, hjust = 1,vjust=1,size = 16,face = "bold"),
        axis.title.x = element_text(size=18,face="bold"),
        plot.title = element_text(size=20,face="bold"),
        axis.text.y = element_text(size = 16,face = "bold"),
        axis.title.y = element_text(size=18,face="bold"))+
  ggtitle("Vote Choice 2016-18, Latinos") + 
  theme(legend.title=element_text(face="bold", size=12)) + 
  scale_x_discrete(name="2016") +
  scale_y_discrete(name="2018") +
  labs(fill="Share of Latinos")
dev.off()

#### voting transitions Asian Americans
t1 <- table(dtam3$TRUMPVCB.W1[dtam3$HISPANIC.W1==0],
            dtam3$GOPREPVD18B[dtam3$HISPANIC.W1==0])
tt3 <- t1/sum(t1)
rownames(tt3) <- c("Dem 16","Neither 16","GOP 16")
colnames(tt3) <- c("Dem 18","Neither 18","GOP 18")


co=melt(tt3)
head(co)
colnames(co) <- c("Wave1","Wave3","value")

#pdf("voting-transition-asn-02022022.pdf")

ggplot(co, aes(Wave1, Wave3)) + # x and y axes => Var1 and Var2
  geom_tile(aes(fill = value)) + # background colours are mapped according to the value column
  #theme_gray(base_size=24) +
  geom_text(aes(fill = value, label = round(value, 2)),size=12) + # write the values
  scale_fill_gradient2(low = "#F2F2F2", 
                       #mid = "#B7B7B7", 
                       high = "#7F7F7F")+#, 
  #midpoint = mean(co$value)) + # determine the colour
  #scale_colour_grey()+theme_bw()+
  theme(panel.grid.major.x=element_blank(), #no gridlines
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), # background=white
        axis.text.x = element_text(angle=60, hjust = 1,vjust=1,size = 16,face = "bold"),
        axis.title.x = element_text(size=18,face="bold"),
        plot.title = element_text(size=20,face="bold"),
        axis.text.y = element_text(size = 16,face = "bold"),
        axis.title.y = element_text(size=18,face="bold"))+
  ggtitle("Vote Choice 2016-18, As. Americans") + 
  theme(legend.title=element_text(face="bold", size=12)) + 
  scale_x_discrete(name="2016") +
  scale_y_discrete(name="2018") +
  labs(fill="Share of As. Americans")
dev.off()


#### regression models ----
#### TABLE 1

lout1a.hisp <- lm(I(PID3-PIDPRE.W1)~ EDYEARS.W1+SPANISH.W1+
                    FEMALE.W1+AGE.W1+log(INCOME.W1)+BORNUS.W1+CITIZEN.W1+MEXICAN.W1+PUERTORICAN.W1,
                  data=dtam2[dtam2$HISPANIC.W1==1,])
summary(lout1a.hisp)

lout1a.asian <- lm(I(PID3-PIDPRE.W1)~ EDYEARS.W1+
                     FEMALE.W1+AGE.W1+log(INCOME.W1)+BORNUS.W1+CITIZEN.W1+CHINESE.W1+FILIPINO.W1,
                   data=dtam2[dtam2$HISPANIC.W1==0,])
summary(lout1a.asian)

texreg(list(lout1a.hisp,lout1a.asian),digits=3,stars=0.05)

lout1b.hisp <- lm(I(PID3-PIDPRE.W1)~ EDYEARS.W1+SPANISH.W1+
                    FEMALE.W1+AGE.W1+log(INCOME.W1)+BORNUS.W1+CITIZEN.W1+MEXICAN.W1+PUERTORICAN.W1+PATHWAY1+PANETHIDX.W1,
                  data=dtam2[dtam2$HISPANIC.W1==1,])
summary(lout1b.hisp)

lout1b.asian <- lm(I(PID3-PIDPRE.W1)~ EDYEARS.W1+
                     FEMALE.W1+AGE.W1+log(INCOME.W1)+BORNUS.W1+CITIZEN.W1+CHINESE.W1+FILIPINO.W1+PATHWAY1+PANETHIDX.W1,
                   data=dtam2[dtam2$HISPANIC.W1==0,])
summary(lout1b.asian)

lout1c.hisp <- lm(I(PID3-PIDPRE.W1)~ EDYEARS.W1+SPANISH.W1+
                    FEMALE.W1+AGE.W1+log(INCOME.W1)+BORNUS.W1+CITIZEN.W1+MEXICAN.W1+PUERTORICAN.W1+PATHWAY1+NATORIDX.W1,
                  data=dtam2[dtam2$HISPANIC.W1==1,])
summary(lout1c.hisp)

lout1c.asian <- lm(I(PID3-PIDPRE.W1)~ EDYEARS.W1+
                     FEMALE.W1+AGE.W1+log(INCOME.W1)+BORNUS.W1+CITIZEN.W1+CHINESE.W1+FILIPINO.W1+PATHWAY1+NATORIDX.W1,
                   data=dtam2[dtam2$HISPANIC.W1==0,])
summary(lout1c.asian)

texreg(list(lout1a.asian,lout1b.asian,lout1a.hisp,lout1b.hisp),digits=3,stars=0.05,
       custom.model.names = c("Asian Am","Asian Am","Latino","Latino"))

### TABLE 2
lout1c <- lm(PIDPRE.W1 ~ PANETHIDX.W1+EDYEARS.W1+SPANISH.W1+
               FEMALE.W1+AGE.W1+INCOMER.W1+BORNUS.W1+CITIZEN.W1,data=dtam2[! dtam2$PID3 %in% c(NA) & dtam2$HISPANIC.W1==1,])
lout3c <- lm(PID3 ~ PANETHIDX.W3+EDYEARS.W1+SPANISH.W1+
               FEMALE.W1+AGE.W1+INCOMER.W1+BORNUS.W1+CITIZEN.W1,data=dtam2[! dtam2$PID3 %in% c(NA) & dtam2$HISPANIC.W1==1,])
lout1d <- lm(PIDPRE.W1 ~ PANETHIDX.W1+EDYEARS.W1+
               FEMALE.W1+AGE.W1+INCOMER.W1+BORNUS.W1+CITIZEN.W1,data=dtam2[! dtam2$PID3 %in% c(NA) & dtam2$HISPANIC.W1==0,])
lout3d <- lm(PID3 ~ PANETHIDX.W3+EDYEARS.W1+
               FEMALE.W1+AGE.W1+INCOMER.W1+BORNUS.W1+CITIZEN.W1,data=dtam2[! dtam2$PID3 %in% c(NA) & dtam2$HISPANIC.W1==0,])

texreg(list(lout1d,lout3d,lout1c,lout3c),digits=3,stars=0.05,
       custom.model.names = c("Asian Am","Asian Am","Latinos","Latinos"))

#### cross-lagged regression models ----
#### TABLE 3
lout.pid <- lm(PID3 ~ PANETHIDX.W1+PIDPRE.W1,data=dtam2)
lout.pan <- lm(PANETHIDX.W3 ~ PIDPRE.W1+PANETHIDX.W1,data=dtam2)

texreg(list(lout.pid,lout.pan),stars=0.05,digits=3,
       custom.model.names=c("Party ID","Panethnic Index"))

#### TABLE A4
rmat <- matrix(NA,4,3)
rmat[1,1] <- mean(dtam2$RANKPANETH.W2[! dtam2$RANKPANETH.W3 %in% c(NA)],na.rm=T)
rmat[1,2] <- mean(dtam2$RANKPANETH.W3[! dtam2$RANKPANETH.W3 %in% c(NA)],na.rm=T)
tout <- t.test(dtam2$RANKPANETH.W2-dtam2$RANKPANETH.W3)
rmat[1,3] <- tout$p.value

rmat[2,1] <- mean(dtam2$RANKNATOR.W2[! dtam2$RANKNATOR.W3 %in% c(NA)],na.rm=T)
rmat[2,2] <- mean(dtam2$RANKNATOR.W3[! dtam2$RANKNATOR.W3 %in% c(NA)],na.rm=T)
tout2 <- t.test(dtam2$RANKNATOR.W2-dtam2$RANKNATOR.W3)
rmat[2,3] <- tout2$p.value

rmat[3,1] <- mean(dtam2$RANKAMER.W2[! dtam2$RANKAMER.W3 %in% c(NA)],na.rm=T)
rmat[3,2] <- mean(dtam2$RANKAMER.W3[! dtam2$RANKAMER.W3 %in% c(NA)],na.rm=T)
tout2 <- t.test(dtam2$RANKAMER.W2-dtam2$RANKAMER.W3)
rmat[3,3] <- tout2$p.value

rmat[4,1] <- mean(dtam2$RANKPARTY.W2[! dtam2$RANKPARTY.W3 %in% c(NA)],na.rm=T)
rmat[4,2] <- mean(dtam2$RANKPARTY.W3[! dtam2$RANKPARTY.W3 %in% c(NA)],na.rm=T)
tout3 <- t.test(dtam2$RANKPARTY.W2-dtam2$RANKPARTY.W3)
rmat[4,3] <- tout3$p.value

rownames(rmat) <- c("Pan-Ethnic","Nat'l Origin","American","Party")
colnames(rmat) <- c("Fall 2016","Fall 2018","P-value")
xtable(rmat,digits=3)

### TABLE A5
#### model variation ---
lout <- lm(I(PID.ABS.D>0) ~ BECOME.EMPLOYED+BECOME.UNEMPLOYED+EDYEARS.W1+HISPANIC.W1+SPANISH.W1+
             FEMALE.W1+AGE.W1+INCOME.W1+EMPLOYED.W1+UNEMPLOYED.W1+RETIRED.W1+BORNUS.W1,data=dtam2)
summary(lout)
texreg(lout,stars=0.05)

### FIGURE A4
#### asian americans

p1 <- mean(dtam2$PANETHIDX.W1[! dtam2$PANETHIDX.W3 %in% c(NA) & dtam2$HISPANIC.W1==0],na.rm=T)
p3 <- mean(dtam2$PANETHIDX.W3[! dtam2$PANETHIDX.W1 %in% c(NA) & dtam2$HISPANIC.W1==0],na.rm=T)

tout1 <- t.test(dtam2$PANETHIDX.W1[! dtam2$PANETHIDX.W3 %in% c(NA) & dtam2$HISPANIC.W1==0])
tout3 <- t.test(dtam2$PANETHIDX.W3[! dtam2$PANETHIDX.W3 %in% c(NA) & dtam2$HISPANIC.W1==0])

y <- c(p3,p1)
dta <- as.data.frame(y)
dta$x <- c("2018, Fall","2016, Spring")
dta$lb <- c(tout3$conf.int[1],tout1$conf.int[1])
dta$ub <- c(tout3$conf.int[2],tout1$conf.int[2])

#pdf("panethind-asn-05032021.pdf")
ggplot(dta) +
  #
  geom_bar( aes(x=x, y=y), stat="identity", fill="skyblue", alpha=0.5)+
  ylim(0,8)+
  geom_errorbar(aes(x=x,ymin=lb,ymax=ub), width=0.4, colour="orange", alpha=0.9, size=1.3)+
  ggtitle("Attachment to Pan-ethnic Identity: As. Americans")+
  xlab("Survey Wave")+
  ylab("Score (2-8)")
#  theme(text = element_text(size=rel(3.35)))
dev.off()  


#### latinos 
p1 <- mean(dtam2$PANETHIDX.W1[! dtam2$PANETHIDX.W3 %in% c(NA) & dtam2$HISPANIC.W1==1],na.rm=T)
p3 <- mean(dtam2$PANETHIDX.W3[! dtam2$PANETHIDX.W1 %in% c(NA) & dtam2$HISPANIC.W1==1],na.rm=T)

tout1 <- t.test(dtam2$PANETHIDX.W1[! dtam2$PANETHIDX.W3 %in% c(NA) & dtam2$HISPANIC.W1==1])
tout3 <- t.test(dtam2$PANETHIDX.W3[! dtam2$PANETHIDX.W3 %in% c(NA) & dtam2$HISPANIC.W1==1])

y <- c(p1,p3)
dta <- as.data.frame(y)
dta$x <- c("2016, Spring","2018, Fall")
dta$lb <- c(tout1$conf.int[1],tout3$conf.int[1])
dta$ub <- c(tout1$conf.int[2],tout3$conf.int[2])

#pdf("panethind-lat-04282021.pdf")
ggplot(dta) +
  geom_bar( aes(x=x, y=y), stat="identity", fill="skyblue", alpha=0.5)+
  ylim(0,8)+
  geom_errorbar(aes(x=x,ymin=lb,ymax=ub), width=0.4, colour="orange", alpha=0.9, size=1.3)+
  ggtitle("Attachment to Pan-ethnic Identity: Latinos")+
  xlab("Survey Wave")+
  ylab("Score (2-8)")
#  theme(text = element_text(size=rel(3.35)))
dev.off()  

#### APPENDIX TABLE A6 ----

lout2 <- lm(PID3 ~ as.factor(PIDPRE.W1)+PATHWAY1+PANETHIDX.W1+EDYEARS.W1+HISPANIC.W1+SPANISH.W1+
              FEMALE.W1+AGE.W1+INCOMER.W1,data=dtam2)
summary(lout2)
lout3 <- lm(PID3 ~ as.factor(PIDPRE.W1)+PATHWAY1*HISPANIC.W1+PANETHIDX.W1+EDYEARS.W1+HISPANIC.W1+SPANISH.W1+
              FEMALE.W1+AGE.W1+INCOMER.W1,data=dtam2)
summary(lout3)

lout2ftr <- lm(FTREPS.W3 ~ FTREPS.W1+PATHWAY1+PANETHIDX.W1+EDYEARS.W1+HISPANIC.W1+SPANISH.W1+
                 FEMALE.W1+AGE.W1+INCOMER.W1,data=dtam2)
summary(lout2ftr)
lout2ftd <- lm(FTDEMS.W3 ~ FTDEMS.W1+PATHWAY1+PANETHIDX.W1+EDYEARS.W1+HISPANIC.W1+SPANISH.W1+
                 FEMALE.W1+AGE.W1+INCOMER.W1,data=dtam2)
summary(lout2ftd)

### generates t:overtime
texreg(list(lout2,lout3,lout2ftr,lout2ftd))

### TABLE A7
lout1 <- lm(PIDPRE.W1 ~ PDINDEX.W1+HISPANIC.W1+SPANISH.W1+CITIZEN.W1+BORNUS.W1+EDYEARS.W1+FEMALE.W1+AGE.W1,data=dtam2[! dtam2$PDINDEX.W3 %in% c(NA),])
lout3 <- lm(PID3 ~ PDINDEX.W3+HISPANIC.W1+SPANISH.W1+CITIZEN.W1+BORNUS.W1+EDYEARS.W1+FEMALE.W1+AGE.W1,data=dtam2)

texreg(list(lout1,lout3),stars=0.05,custom.model.names=c("Spring '16","Fall '18"),digits=3)

#### TABLE A9

lout1 <- lm(PIDPRE.W1 ~ PDINDEX.W1+HISPANIC.W1+SPANISH.W1+CITIZEN.W1+BORNUS.W1+EDYEARS.W1+FEMALE.W1+AGE.W1,data=dtam2[! dtam2$PDINDEX.W3 %in% c(NA),])
lout3 <- lm(PID3 ~ PDINDEX.W3+HISPANIC.W1+SPANISH.W1+CITIZEN.W1+BORNUS.W1+EDYEARS.W1+FEMALE.W1+AGE.W1,data=dtam2)

texreg(list(lout1,lout3),stars=0.05,custom.model.names=c("Spring '16","Fall '18"),digits=3)

#### ### cross-lagged models first gen Appendix A10
lout.pid1 <- lm(PID3 ~ PANETHIDX.W1+PIDPRE.W1,data=dtam2[dtam2$BORNUS.W1==0,])
lout.pan1 <- lm(PANETHIDX.W3 ~ PIDPRE.W1+PANETHIDX.W1,data=dtam2[dtam2$BORNUS.W1==0,])

texreg(list(lout.pid1,lout.pan1),stars=0.05,digits=3,
       custom.model.names=c("Party ID","Panethnic Index"))

#### Appendix Table A8
lout8c <- lm(PIDPRE.W1 ~ NATORIDX.W1+EDYEARS.W1+SPANISH.W1+
               FEMALE.W1+AGE.W1+INCOMER.W1+BORNUS.W1+CITIZEN.W1,data=dtam2[! dtam2$PID3 %in% c(NA) & dtam2$HISPANIC.W1==1,])
lout9c <- lm(PID3 ~ NATORIDX.W3+EDYEARS.W1+SPANISH.W1+
               FEMALE.W1+AGE.W1+INCOMER.W1+BORNUS.W1+CITIZEN.W1,data=dtam2[! dtam2$PID3 %in% c(NA) & dtam2$HISPANIC.W1==1,])

lout8a <- lm(PIDPRE.W1 ~ NATORIDX.W1+EDYEARS.W1+
               FEMALE.W1+AGE.W1+INCOMER.W1+BORNUS.W1+CITIZEN.W1,data=dtam2[! dtam2$PID3 %in% c(NA) & dtam2$HISPANIC.W1==0,])
lout9a <- lm(PID3 ~ NATORIDX.W3+EDYEARS.W1+
               FEMALE.W1+AGE.W1+INCOMER.W1+BORNUS.W1+CITIZEN.W1,data=dtam2[! dtam2$PID3 %in% c(NA) & dtam2$HISPANIC.W1==0,])

texreg(list(lout8a,lout9a,lout8c,lout9c),stars=0.05,digits=3)


#### EXPERIMENT

######
library(foreign)
library(texreg)

#dta <- read.spss("/users/danielhopkins/Dropbox/PDParty/survey/VanderbiltUniversity_Latinos and Asians 2015_Main_Client.sav",to.data.frame=T)
#dta <- read.spss("/users/danhop/Dropbox/PDParty/survey/VanderbiltUniversity_Latinos and Asians 2015_Main_Client.sav",to.data.frame=T)

load("VanderbiltUniversity_Latinos and Asians 2015_Main_Client_replication.Rdata")
dta <- dtasub
rm(dtasub)

dta$OPTIN <- 1*(dta$offpanel=="Off-panel")
dta$SPANISH <- 1*(dta$XSPANISH=="Spanish")
#  43% of Hispanics respond in Spanish
#[1] 0.4292683

dta$INCOME <- NA
dta$INCOME[dta$PPINCIMP=="Less than $5,000"] <- 2.5
dta$INCOME[dta$PPINCIMP=="$5,000 to $7,499"] <- 6.25
dta$INCOME[dta$PPINCIMP=="$7,500 to $9,999"] <- 8.75
dta$INCOME[dta$PPINCIMP=="$10,000 to $12,499"] <- 11.25
dta$INCOME[dta$PPINCIMP=="$12,500 to $14,999"] <- 13.75
dta$INCOME[dta$PPINCIMP=="$15,000 to $19,999"] <- 17.5
dta$INCOME[dta$PPINCIMP=="$20,000 to $24,999"] <- 22.5
dta$INCOME[dta$PPINCIMP=="$25,000 to $29,999"] <- 27.5
dta$INCOME[dta$PPINCIMP=="$30,000 to $34,999"] <- 32.5
dta$INCOME[dta$PPINCIMP=="$35,000 to $39,999"] <- 37.5
dta$INCOME[dta$PPINCIMP=="$40,000 to $49,999"] <- 45
dta$INCOME[dta$PPINCIMP=="$50,000 to $59,999"] <- 55.0
dta$INCOME[dta$PPINCIMP=="$60,000 to $74,999"] <- 67.5
dta$INCOME[dta$PPINCIMP=="$75,000 to $84,999"] <- 80
dta$INCOME[dta$PPINCIMP=="$85,000 to $99,999"] <- 92.5
dta$INCOME[dta$PPINCIMP=="$100,000 to $124,999"] <- 112.5
dta$INCOME[dta$PPINCIMP=="$125,000 to $149,999"] <- 137.5
dta$INCOME[dta$PPINCIMP=="$150,000 to $174,999"] <- 162.5
dta$INCOME[dta$PPINCIMP=="$175,000 or more"] <- 250

dta$HISPANIC <- 1*(! dta$DOV_ASAM=="Asian")
dta$ASIAN <- 1*(dta$DOV_ASAM=="Asian")
dta$CHINESE <- 1*(dta$Q11_Asians=="China")
dta$INDIAN <- 1*(dta$Q11_Asians=="India")
dta$FILIPINO <- 1*(dta$Q11_Asians=="Philippines")
dta$JAPANESE <- 1*(dta$Q11_Asians=="Japan")

dta$CUBAN <- 1*(dta$Q11_Latinos=="Cuba")
dta$PUERTORICAN <- 1*(dta$Q11_Latinos=="Puerto Rico")
dta$MEXICAN <- 1*(dta$Q11_Latinos=="Mexico")

####
dta$ARTICLE <- 1*(dta$PDMANIPULATION1=="TREATMENT ARTICLE")
dta$SAWTRUMP <- 1*(dta$PDMANIPULATION2=="TREATMENT VIDEO")

dta$PDMANIPULATION <- 1*(dta$PDMANIPULATION3=="Ask PD battery first")

dta$FTREPS <- as.numeric(dta$Q2_1)
dta$FTREPS[dta$Q2_1==-1] <- NA
dta$FTDEMS <- as.numeric(dta$Q2_2)
dta$FTDEMS[dta$Q2_2==-1] <- NA

dta$FTREPSDEMS <- dta$FTREPS-dta$FTDEMS
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-101.00  -50.00  -13.00  -19.43    1.00  100.00 

dta$FTIN <- as.numeric(dta$Q2_7)
dta$FTIN[dta$Q2_7==-1] <- NA

dta$FTBLKS <- as.numeric(dta$Q2_9)
dta$FTBLKS[dta$Q2_9==-1] <- NA

dta$FTWHTS <- as.numeric(dta$Q2_8)
dta$FTWHTS[dta$Q2_8==-1] <- NA

dta$FTOUT <- apply(cbind(dta$FTBLKS,dta$BTWHTS),1,mean)

dta$FTCLINTON <- as.numeric(dta$Q2_4)
dta$FTTRUMP <- as.numeric(dta$Q2_6)

dta$FTTRUMPCLINTON <- dta$FTTRUMP-dta$FTCLINTON
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-101.00  -66.00  -34.50  -29.26    0.00  100.00 

dta$FIGHTHIS <- NA
dta$FIGHTHIS[dta$Q3_5=="Democratic Party"]  <- 5
dta$FIGHTHIS[dta$Q3_5=="4"]  <- 4
dta$FIGHTHIS[dta$Q3_5=="Both equally"]  <- 3
dta$FIGHTHIS[dta$Q3_5=="2"]  <- 2
dta$FIGHTHIS[dta$Q3_5=="Republican Party"]  <- 1

dta$FIGHTASN <- NA
dta$FIGHTASN[dta$Q3_9=="Democratic Party"]  <- 5
dta$FIGHTASN[dta$Q3_9=="4"]  <- 4
dta$FIGHTASN[dta$Q3_9=="Both equally"]  <- 3
dta$FIGHTASN[dta$Q3_9=="2"]  <- 2
dta$FIGHTASN[dta$Q3_9=="Republican Party"]  <- 1

dta$FIGHTIN <- NA
dta$FIGHTIN[dta$ASIAN==1] <- dta$FIGHTASN[dta$ASIAN==1]
dta$FIGHTIN[dta$ASIAN==0] <- dta$FIGHTHIS[dta$ASIAN==0]

dta$FIGHTRICH <- NA
dta$FIGHTRICH[dta$Q3_3=="Democratic Party"]  <- 5
dta$FIGHTRICH[dta$Q3_3=="4"]  <- 4
dta$FIGHTRICH[dta$Q3_3=="Both equally"]  <- 3
dta$FIGHTRICH[dta$Q3_3=="2"]  <- 2
dta$FIGHTRICH[dta$Q3_3=="Republican Party"]  <- 1

dta$FIGHTREL <- NA
dta$FIGHTREL[dta$Q3_2=="Democratic Party"]  <- 5
dta$FIGHTREL[dta$Q3_2=="4"]  <- 4
dta$FIGHTREL[dta$Q3_2=="Both equally"]  <- 3
dta$FIGHTREL[dta$Q3_2=="2"]  <- 2
dta$FIGHTREL[dta$Q3_2=="Republican Party"]  <- 1

dta$FIGHTTAX <- NA
dta$FIGHTTAX[dta$Q3_7=="Democratic Party"]  <- 5
dta$FIGHTTAX[dta$Q3_7=="4"]  <- 4
dta$FIGHTTAX[dta$Q3_7=="Both equally"]  <- 3
dta$FIGHTTAX[dta$Q3_7=="2"]  <- 2
dta$FIGHTTAX[dta$Q3_7=="Republican Party"]  <- 1

dta$PIDPRE <- NA
dta$PIDPRE[dta$xParty7=="Strong Republican"] <- 7
dta$PIDPRE[dta$xParty7=="Not Strong Republican"] <- 6
dta$PIDPRE[dta$xParty7=="Leans Republican"] <- 5
dta$PIDPRE[dta$xParty7=="Undecided/Independent/Other"] <- 4
dta$PIDPRE[dta$xParty7=="Leans Democrat"] <- 3
dta$PIDPRE[dta$xParty7=="Not Strong Democrat"] <- 2 
dta$PIDPRE[dta$xParty7=="Strong Democrat"] <- 1

dta$CLINTONVT <- 1*(dta$VP5_A=="Hillary Clinton, the Democrat")
dta$TRUMPVC <- 1*(dta$VP5_A=="Donald Trump, the Republican")
dta$NOVOTECT <- 1*(dta$VP5_A=="Would not vote for President")
dta$OTHERVOTECT <- 1*(dta$VP5_A=="Other (SPECIFY)")

dta$TRUMPVC10B <- NA
dta$TRUMPVC10B[dta$VP5_A=="Hillary Clinton, the Democrat"] <- -1
dta$TRUMPVC10B[dta$VP5_A %in% c("Other (SPECIFY)","Would not vote for President","Refused")] <- 0
dta$TRUMPVC10B[dta$VP5_A=="Donald Trump, the Republican"] <- 1

dta$RUBIOVC10B <- NA
dta$RUBIOVC10B[dta$VP5_B=="Hillary Clinton, the Democrat"] <- -1
dta$RUBIOVC10B[dta$VP5_B %in% c("Other (SPECIFY)","Would not vote for President","Refused")] <- 0
dta$RUBIOVC10B[dta$VP5_B=="Marco Rubio, the Republican"] <- 1

dta$CRUZVC10B <- NA
dta$CRUZVC10B[dta$VP5_C=="Hillary Clinton, the Democrat"] <- -1
dta$CRUZVC10B[dta$VP5_C %in% c("Other (SPECIFY)","Would not vote for President","Refused")] <- 0
dta$CRUZVC10B[dta$VP5_C=="Ted Cruz, the Republican"] <- 1

dta$CLINTONVR <- 1*(dta$VP5_B=="Hillary Clinton, the Democrat")
dta$CLINTONVC <- 1*(dta$VP5_C=="Hillary Clinton, the Democrat")


dta$FEMALE <- 1*(dta$PPGENDER=="Female")

dta$EDYEARS <- NA
dta$EDYEARS[dta$PPEDUC=="No formal education"] <- 0
dta$EDYEARS[dta$PPEDUC=="1st, 2nd, 3rd, or 4th grade"] <- 4
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

dta$PDJOB <- NA
dta$PDJOB[dta$Q6_1=="Never"] <- 0
dta$PDJOB[dta$Q6_1=="Once"] <- 1
dta$PDJOB[dta$Q6_1=="Sometimes"] <- 2
dta$PDJOB[dta$Q6_1=="Occasionally"] <- 3
dta$PDJOB[dta$Q6_1=="Frequently"] <- 4

dta$PDPOLICE <- NA
dta$PDPOLICE[dta$Q6_2=="Never"] <- 0
dta$PDPOLICE[dta$Q6_2=="Once"] <- 1
dta$PDPOLICE[dta$Q6_2=="Sometimes"] <- 2
dta$PDPOLICE[dta$Q6_2=="Occasionally"] <- 3
dta$PDPOLICE[dta$Q6_2=="Frequently"] <- 4

dta$PDRENT <- NA
dta$PDRENT[dta$Q6_3=="Never"] <- 0
dta$PDRENT[dta$Q6_3=="Once"] <- 1
dta$PDRENT[dta$Q6_3=="Sometimes"] <- 2
dta$PDRENT[dta$Q6_3=="Occasionally"] <- 3
dta$PDRENT[dta$Q6_3=="Frequently"] <- 4

dta$PDSTORE <- NA
dta$PDSTORE[dta$Q6_4=="Never"] <- 0
dta$PDSTORE[dta$Q6_4=="Once"] <- 1
dta$PDSTORE[dta$Q6_4=="Sometimes"] <- 2
dta$PDSTORE[dta$Q6_4=="Occasionally"] <- 3
dta$PDSTORE[dta$Q6_4=="Frequently"] <- 4

dta$CITIZEN <- 0
dta$CITIZEN[dta$XPP20197=="Yes"] <- 1

dta$BORNUS <- 0
dta$BORNUS[dta$XPP20198=="Born a U.S. citizen"] <- 1

dta$GROUPDIS <- NA
dta$GROUPDIS[dta$Q10_2=="Strongly agree"] <- 6
dta$GROUPDIS[dta$Q10_2=="Mostly agree"] <- 5
dta$GROUPDIS[dta$Q10_2=="Somewhat agree"] <- 4
dta$GROUPDIS[dta$Q10_2=="Somewhat disagree"] <- 3
dta$GROUPDIS[dta$Q10_2=="Mostly disagree"] <- 2
dta$GROUPDIS[dta$Q10_2=="Strongly disagree"] <- 1

dta$PERSONDIS <- NA
dta$PERSONDIS[dta$Q10_3=="Strongly agree"] <- 6
dta$PERSONDIS[dta$Q10_3=="Mostly agree"] <- 5
dta$PERSONDIS[dta$Q10_3=="Somewhat agree"] <- 4
dta$PERSONDIS[dta$Q10_3=="Somewhat disagree"] <- 3
dta$PERSONDIS[dta$Q10_3=="Mostly disagree"] <- 2
dta$PERSONDIS[dta$Q10_3=="Strongly disagree"] <- 1

dta$PDINDEX <- dta$PDSTORE+dta$PDRENT+dta$PDPOLICE+dta$PDJOB
dta$SOUTH <- 1*(dta$PPSTATEN %in% c("AR","AL","FL","GA","NC","SC","TX","TN","MS","LA","VA"))

dta$ASKRUBIO <- 1*(dta$CLINTONVR %in% c(0,1))

dta$CLINTONVCR <- NA
dta$CLINTONVCR[dta$ASKRUBIO==0]  <- dta$CLINTONVC[dta$ASKRUBIO==0] 
dta$CLINTONVCR[dta$ASKRUBIO==1]  <- dta$CLINTONVR[dta$ASKRUBIO==1] 

dta$PANETHNOTIMP <- NA
dta$PANETHNOTIMP[dta$Q12_1=="Strongly disagree"] <- 4
dta$PANETHNOTIMP[dta$Q12_1=="Somewhat disagree"] <- 3
dta$PANETHNOTIMP[dta$Q12_1=="Somewhat agree"] <- 2
dta$PANETHNOTIMP[dta$Q12_1=="Strongly agree"] <- 1

dta$PANETHCENTRAL <- NA
dta$PANETHCENTRAL[dta$Q12_3=="Strongly disagree"] <- 1
dta$PANETHCENTRAL[dta$Q12_3=="Somewhat disagree"] <- 2
dta$PANETHCENTRAL[dta$Q12_3=="Somewhat agree"] <- 3
dta$PANETHCENTRAL[dta$Q12_3=="Strongly agree"] <- 4

dta$PANETHIDX <- dta$PANETHNOTIMP+dta$PANETHCENTRAL

dta$NATORNOTIMP <- NA
dta$NATORNOTIMP[dta$Q13_1=="Strongly disagree"] <- 4
dta$NATORNOTIMP[dta$Q13_1=="Somewhat disagree"] <- 3
dta$NATORNOTIMP[dta$Q13_1=="Somewhat agree"] <- 2
dta$NATORNOTIMP[dta$Q13_1=="Strongly agree"] <- 1

dta$NATORCENTRAL <- NA
dta$NATORCENTRAL[dta$Q13_2=="Strongly disagree"] <- 1
dta$NATORCENTRAL[dta$Q13_2=="Somewhat disagree"] <- 2
dta$NATORCENTRAL[dta$Q13_2=="Somewhat agree"] <- 3
dta$NATORCENTRAL[dta$Q13_2=="Strongly agree"] <- 4

dta$NATORIDX <- dta$NATORCENTRAL+dta$NATORNOTIMP

dta$AMERNOTIMP <- NA
dta$AMERNOTIMP[dta$Q14_2=="Strongly agree"] <- 1
dta$AMERNOTIMP[dta$Q14_2=="Somewhat agree"] <- 2
dta$AMERNOTIMP[dta$Q14_2=="Somewhat disagree"] <- 3
dta$AMERNOTIMP[dta$Q14_2=="Strongly disagree"] <- 4

dta$AMERCENTRAL <- NA
dta$AMERCENTRAL[dta$Q14_3=="Strongly agree"] <- 4
dta$AMERCENTRAL[dta$Q14_3=="Somewhat agree"] <- 3
dta$AMERCENTRAL[dta$Q14_3=="Somewhat disagree"] <- 2
dta$AMERCENTRAL[dta$Q14_3=="Strongly disagree"] <- 1

dta$AMERIDX <- dta$AMERCENTRAL+dta$AMERNOTIMP

dta$HASBA <- (dta$EDYEARS > 15)*1

dta$KOREAN <- 1*(dta$Q11_Asians=="Korea")

loutall2ct <- lm(TRUMPVC10B ~ SAWTRUMP+ARTICLE+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$OPTIN==0,])
summary(loutall2ct)

loutasian2ct <- lm(TRUMPVC10B ~ SAWTRUMP+ARTICLE+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==1 & dta$OPTIN==0,])
summary(loutasian2ct)

loutlatino2ct <- lm(TRUMPVC10B ~ SAWTRUMP+ARTICLE+SPANISH+MEXICAN+CUBAN+PUERTORICAN+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==0,])
summary(loutlatino2ct)

loutall2cr <- lm(RUBIOVC10B ~ SAWTRUMP+ARTICLE+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$OPTIN==0,])
summary(loutall2cr)

loutasian2cr <- lm(RUBIOVC10B ~ SAWTRUMP+ARTICLE+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==1 & dta$OPTIN==0,])
summary(loutasian2cr)

loutlatino2cr <- lm(RUBIOVC10B ~ SAWTRUMP+ARTICLE+SPANISH+MEXICAN+CUBAN+PUERTORICAN+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==0,])
summary(loutlatino2cr)

loutall2cc <- lm(CRUZVC10B ~ SAWTRUMP+ARTICLE+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$OPTIN==0,])
summary(loutall2cc)

loutasian2cc <- lm(CRUZVC10B ~ SAWTRUMP+ARTICLE+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==1 & dta$OPTIN==0,])
summary(loutasian2cc)

loutlatino2cc <- lm(CRUZVC10B ~ SAWTRUMP+ARTICLE+SPANISH+MEXICAN+CUBAN+PUERTORICAN+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==0,])
summary(loutlatino2cc)

models1 <- rev(c("loutall2ct","loutasian2ct","loutlatino2ct"))
models2 <- rev(c("loutall2cr","loutasian2cr","loutlatino2cr"))
models3 <- rev(c("loutall2cc","loutasian2cc","loutlatino2cc"))
#models4 <- rev(c("loutall2in","loutasian2in","loutlatino2in"))

outcomes <- rev(c("Cruz vs. Clinton","Rubio vs. Clinton","Trump vs. Clinton"))

pch.options <- c(16,17,18,19)
groups <- rev(c("All Respondents","Asian Americans","Latinos"))

#pdf("/users/danhop/Dropbox/PDparty/survey/exp2-video0-candidarteresultsjoint0721217.pdf")
plot(1:19,type="n",ylab="",ylim=c(0.5,14.5),xlim=c(-.5,.5),yaxt="n",xlab="Support for Republican",
     main="Effect of Trump Video",cex.main=1.5,cex.lab=1.25)

k <- -1
### loop over 3 dependent variables
for(j in 1:3){
  k <- k+2
  txt10 <- paste("models <- models",j,sep="")
  eval(parse(text=txt10))
  for(i in 1:length(models)){
    txt <- paste("lout <- ",models[i])
    eval(parse(text=txt))
    points(x=summary(lout)$coef[2,1],y=k,pch=pch.options[i])	
    lines(x=c(summary(lout)$coef[2,1]-summary(lout)$coef[2,2]*1.96,summary(lout)$coef[2,1]+summary(lout)$coef[2,2]*1.96),y=c(k,k),pch=2)	
    #text(x=summary(lout)$coef[2,1],y=k+.25,lbs[i])
    lines(x=c(summary(lout)$coef[2,1]-summary(lout)$coef[2,2],summary(lout)$coef[2,1]+summary(lout)$coef[2,2]),y=c(k,k),pch=2,lwd=2)	
    if(j==3){
      #text(groups[i],x=(summary(lout)$coef[2,1]-summary(lout)$coef[2,2]*1.96-2),y=k)
      text(groups[i],x=0.35,y=k)
      
    }
    k <- k+1
  }
}
abline(v=0,lty=2)
#dev.off()

text(y=3.95,x=0,outcomes[1],cex=1.4)
text(y=8.95,x=0,outcomes[2],cex=1.4)
text(y=13.95,x=0,outcomes[3],cex=1.4)
#text(y=18.95,x=0,outcomes[4],cex=1.4)

dev.off()

##### experiment -- party perceptions figure ----

loutall2in <- lm(FIGHTIN ~ SAWTRUMP+ARTICLE+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$OPTIN==0,])
#summary(loutall2)

loutasian2in <- lm(FIGHTIN ~ SAWTRUMP+ARTICLE+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==1 & dta$OPTIN==0,])
#ARTICLE                                 -0.1093311  0.0636464  -1.718  0.08630 .  

loutoptin2in <- lm(FIGHTIN ~ SAWTRUMP+ARTICLE+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==1 & dta$OPTIN==1,])
#SAWTRUMP                                -0.09931    0.09921  -1.001 0.318075    
#ARTICLE                                 -0.10987    0.09966  -1.102 0.271680   

loutlatino2in <- lm(FIGHTIN ~ SAWTRUMP+ARTICLE+SPANISH+MEXICAN+CUBAN+PUERTORICAN+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==0,])
summary(loutlatino2in)

loutall2rich <- lm(FIGHTRICH ~ SAWTRUMP+ARTICLE+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$OPTIN==0,])
summary(loutall2rich)

loutasian2rich <- lm(FIGHTRICH ~ SAWTRUMP+ARTICLE+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==1 & dta$OPTIN==0,])
summary(loutasian2rich)

loutlatino2rich <- lm(FIGHTRICH ~ SAWTRUMP+ARTICLE+SPANISH+MEXICAN+CUBAN+PUERTORICAN+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==0,])
summary(loutlatino2rich)

loutall2tax <- lm(FIGHTTAX ~ SAWTRUMP+ARTICLE+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$OPTIN==0,])
summary(loutall2tax)

loutasian2tax <- lm(FIGHTTAX ~ SAWTRUMP+ARTICLE+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==1 & dta$OPTIN==0,])
summary(loutasian2tax)

loutlatino2tax <- lm(FIGHTTAX ~ SAWTRUMP+ARTICLE+SPANISH+MEXICAN+CUBAN+PUERTORICAN+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==0,])
summary(loutlatino2tax)

loutall2rel <- lm(FIGHTREL ~ SAWTRUMP+ARTICLE+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$OPTIN==0,])
summary(loutall2rel)

loutasian2rel <- lm(FIGHTREL ~ SAWTRUMP+ARTICLE+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==1 & dta$OPTIN==0,])
summary(loutasian2rel)

loutlatino2rel <- lm(FIGHTREL ~ SAWTRUMP+ARTICLE+SPANISH+MEXICAN+CUBAN+PUERTORICAN+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==0,])
summary(loutlatino2rel)

models1 <- rev(c("loutall2rich","loutasian2rich","loutlatino2rich"))
models3 <- rev(c("loutall2rel","loutasian2rel","loutlatino2rel"))
models2 <- rev(c("loutall2tax","loutasian2tax","loutlatino2tax"))
models4 <- rev(c("loutall2in","loutasian2in","loutlatino2in"))

outcomes <- rev(c("Fights for My Group","Fights for Religious","Fights to Cut Taxes","Fights for Rich"))

pch.options <- c(16,17,18,19)
groups <- rev(c("All Respondents","Asian Americans","Latinos"))

#pdf("exp2-video0mainresultsjoint05032021.pdf")
plot(1:19,type="n",ylab="",ylim=c(0.5,19.5),xlim=c(-.5,.5),yaxt="n",xlab="Describes GOP             Describes Dems",
     main="Effect of Trump Video",cex.main=1.5,cex.lab=1.25)

k <- -1
### loop over 3 dependent variables
for(j in 1:4){
  k <- k+2
  txt10 <- paste("models <- models",j,sep="")
  eval(parse(text=txt10))
  for(i in 1:length(models)){
    txt <- paste("lout <- ",models[i])
    eval(parse(text=txt))
    points(x=summary(lout)$coef[2,1],y=k,pch=pch.options[i])	
    lines(x=c(summary(lout)$coef[2,1]-summary(lout)$coef[2,2]*1.96,summary(lout)$coef[2,1]+summary(lout)$coef[2,2]*1.96),y=c(k,k),pch=2)	
    #text(x=summary(lout)$coef[2,1],y=k+.25,lbs[i])
    lines(x=c(summary(lout)$coef[2,1]-summary(lout)$coef[2,2],summary(lout)$coef[2,1]+summary(lout)$coef[2,2]),y=c(k,k),pch=2,lwd=2)	
    if(j==4){
      #text(groups[i],x=(summary(lout)$coef[2,1]-summary(lout)$coef[2,2]*1.96-2),y=k)
      text(groups[i],x=0.35,y=k)
      
    }
    k <- k+1
  }
}
abline(v=0,lty=2)
#dev.off()

text(y=3.95,x=0,outcomes[1],cex=1.4)
text(y=8.95,x=0,outcomes[2],cex=1.4)
text(y=13.95,x=0,outcomes[3],cex=1.4)
text(y=18.95,x=0,outcomes[4],cex=1.4)

dev.off()

##### FIGURE A7
dta$KNOWOVER <- 0
dta$KNOWOVER[dta$Q15=="Two-thirds"] <- 1

dta$KNOWSEN <- 0
dta$KNOWSEN[dta$Q16=="6"] <- 1

dta$KNOWLEDGE <- dta$KNOWSEN+dta$KNOWOVER

###### FIGURE A7-A

loutall2ctknow <- lm(TRUMPVC10B ~ SAWTRUMP+ARTICLE+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+
                       as.factor(PIDPRE),data=dta[dta$OPTIN==0 & dta$KNOWLEDGE==0,])
summary(loutall2ctknow)

loutasian2ctknow <- lm(TRUMPVC10B ~ SAWTRUMP+ARTICLE+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+
                         BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==1 & dta$OPTIN==0 & dta$KNOWLEDGE==0,])
summary(loutasian2ctknow)

loutlatino2ctknow <- lm(TRUMPVC10B ~ SAWTRUMP+ARTICLE+SPANISH+MEXICAN+CUBAN+PUERTORICAN+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+
                          BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==0 & dta$KNOWLEDGE==0,])
summary(loutlatino2ctknow)

loutall2crknow <- lm(RUBIOVC10B ~ SAWTRUMP+ARTICLE+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+
                       as.factor(PIDPRE),data=dta[dta$OPTIN==0  & dta$KNOWLEDGE==0,])
summary(loutall2crknow)

loutasian2crknow <- lm(RUBIOVC10B ~ SAWTRUMP+ARTICLE+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+
                         BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==1 & dta$OPTIN==0 & dta$KNOWLEDGE==0,])
summary(loutasian2crknow)

loutlatino2crknow <- lm(RUBIOVC10B ~ SAWTRUMP+ARTICLE+SPANISH+MEXICAN+CUBAN+PUERTORICAN+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+
                          BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==0 & dta$KNOWLEDGE==0,])
summary(loutlatino2crknow)


loutall2ccknow <- lm(CRUZVC10B ~ SAWTRUMP+ARTICLE+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+
                       as.factor(PIDPRE),data=dta[dta$OPTIN==0 & dta$KNOWLEDGE==0,])
summary(loutall2ccknow)

loutasian2ccknow <- lm(CRUZVC10B ~ SAWTRUMP+ARTICLE+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+
                         BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==1 & dta$OPTIN==0 & dta$KNOWLEDGE==0,])
summary(loutasian2ccknow)

loutlatino2ccknow <- lm(CRUZVC10B ~ SAWTRUMP+ARTICLE+SPANISH+MEXICAN+CUBAN+PUERTORICAN+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+
                          BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==0 & dta$KNOWLEDGE==0,])
summary(loutlatino2ccknow)

models1 <- rev(c("loutall2ctknow","loutasian2ctknow","loutlatino2ctknow"))
models2 <- rev(c("loutall2crknow","loutasian2crknow","loutlatino2crknow"))
models3 <- rev(c("loutall2ccknow","loutasian2ccknow","loutlatino2ccknow"))
#models4 <- rev(c("loutall2in","loutasian2in","loutlatino2in"))

outcomes <- rev(c("Cruz vs. Clinton","Rubio vs. Clinton","Trump vs. Clinton"))

pch.options <- c(16,17,18,19)
groups <- rev(c("All Respondents","Asian Americans","Latinos"))

#pdf("exp2-video0-candidarteresultsjoint02182022knowledge.pdf")
plot(1:19,type="n",ylab="",ylim=c(0.5,14.5),xlim=c(-.5,.5),yaxt="n",xlab="Support for Republican: Low-Knowledge Respondents",
     main="Effect of Trump Video",cex.main=1.5,cex.lab=1.25)

k <- -1
### loop over 3 dependent variables
for(j in 1:3){
  k <- k+2
  txt10 <- paste("models <- models",j,sep="")
  eval(parse(text=txt10))
  for(i in 1:length(models)){
    txt <- paste("lout <- ",models[i])
    eval(parse(text=txt))
    points(x=summary(lout)$coef[2,1],y=k,pch=pch.options[i])	
    lines(x=c(summary(lout)$coef[2,1]-summary(lout)$coef[2,2]*1.96,summary(lout)$coef[2,1]+summary(lout)$coef[2,2]*1.96),y=c(k,k),pch=2)	
    #text(x=summary(lout)$coef[2,1],y=k+.25,lbs[i])
    lines(x=c(summary(lout)$coef[2,1]-summary(lout)$coef[2,2],summary(lout)$coef[2,1]+summary(lout)$coef[2,2]),y=c(k,k),pch=2,lwd=2)	
    if(j==3){
      #text(groups[i],x=(summary(lout)$coef[2,1]-summary(lout)$coef[2,2]*1.96-2),y=k)
      text(groups[i],x=-0.35,y=k)
      
    }
    k <- k+1
  }
}
abline(v=0,lty=2)
#dev.off()

text(y=3.95,x=0,outcomes[1],cex=1.4)
text(y=8.95,x=0,outcomes[2],cex=1.4)
text(y=13.95,x=0,outcomes[3],cex=1.4)
#text(y=18.95,x=0,outcomes[4],cex=1.4)

#dev.off()

####### FIGURE A7-B 
loutall2inknow <- lm(FIGHTIN ~ SAWTRUMP+ARTICLE+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+
                       as.factor(PIDPRE),data=dta[dta$OPTIN==0 & dta$KNOWLEDGE==0,])

loutasian2inknow<- lm(FIGHTIN ~ SAWTRUMP+ARTICLE+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+
                        as.factor(PIDPRE),data=dta[dta$OPTIN==0 & dta$KNOWLEDGE==0 & dta$ASIAN==1,])
summary(loutasian2inknow)
loutlatino2inknow<- lm(FIGHTIN ~ SAWTRUMP+ARTICLE+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+
                         as.factor(PIDPRE),data=dta[dta$OPTIN==0 & dta$KNOWLEDGE==0 & dta$ASIAN==0,])
summary(loutlatino2inknow)


loutall2rich <- lm(FIGHTRICH ~ SAWTRUMP+ARTICLE+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$OPTIN==0,])
summary(loutall2rich)

loutasian2rich <- lm(FIGHTRICH ~ SAWTRUMP+ARTICLE+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==1 & dta$OPTIN==0,])
summary(loutasian2rich)

loutlatino2rich <- lm(FIGHTRICH ~ SAWTRUMP+ARTICLE+SPANISH+MEXICAN+CUBAN+PUERTORICAN+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==0,])
summary(loutlatino2rich)


loutall2richknow <- lm(FIGHTRICH ~ SAWTRUMP+ARTICLE+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+
                         BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$OPTIN==0 & dta$KNOWLEDGE==0,])
summary(loutall2richknow)

loutasian2richknow <- lm(FIGHTRICH ~ SAWTRUMP+ARTICLE+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+
                           BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==1 & dta$OPTIN==0  & dta$KNOWLEDGE==0,])
summary(loutasian2richknow)

loutlatino2richknow <- lm(FIGHTRICH ~ SAWTRUMP+ARTICLE+SPANISH+MEXICAN+CUBAN+PUERTORICAN+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+
                            BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==0  & dta$KNOWLEDGE==0,])
summary(loutlatino2richknow)

loutall2tax <- lm(FIGHTTAX ~ SAWTRUMP+ARTICLE+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$OPTIN==0,])
summary(loutall2tax)

loutasian2tax <- lm(FIGHTTAX ~ SAWTRUMP+ARTICLE+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==1 & dta$OPTIN==0,])
summary(loutasian2tax)

loutlatino2tax <- lm(FIGHTTAX ~ SAWTRUMP+ARTICLE+SPANISH+MEXICAN+CUBAN+PUERTORICAN+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==0,])
summary(loutlatino2tax)

loutall2taxknow <- lm(FIGHTTAX ~ SAWTRUMP+ARTICLE+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+
                        as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$OPTIN==0 & dta$KNOWLEDGE==0,])
summary(loutall2taxknow)

loutasian2taxknow <- lm(FIGHTTAX ~ SAWTRUMP+ARTICLE+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+FEMALE+as.factor(ppagect4)+
                          CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==1 & dta$OPTIN==0  & dta$KNOWLEDGE==0,])
summary(loutasian2taxknow)

loutlatino2taxknow <- lm(FIGHTTAX ~ SAWTRUMP+ARTICLE+SPANISH+MEXICAN+CUBAN+PUERTORICAN+EDYEARS+FEMALE+as.factor(ppagect4)+
                           CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==0  & dta$KNOWLEDGE==0,])
summary(loutlatino2taxknow)

loutall2rel <- lm(FIGHTREL ~ SAWTRUMP+ARTICLE+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$OPTIN==0,])
summary(loutall2rel)

loutasian2rel <- lm(FIGHTREL ~ SAWTRUMP+ARTICLE+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==1 & dta$OPTIN==0,])
summary(loutasian2rel)

loutlatino2rel <- lm(FIGHTREL ~ SAWTRUMP+ARTICLE+SPANISH+MEXICAN+CUBAN+PUERTORICAN+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==0,])
summary(loutlatino2rel)


loutall2relknow <- lm(FIGHTREL ~ SAWTRUMP+ARTICLE+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+
                        as.factor(PIDPRE),data=dta[dta$OPTIN==0 & dta$KNOWLEDGE==0,])
summary(loutall2relknow)

loutasian2relknow <- lm(FIGHTREL ~ SAWTRUMP+ARTICLE+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+
                          as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==1 & dta$OPTIN==0 & dta$KNOWLEDGE==0,])
summary(loutasian2relknow)

loutlatino2relknow <- lm(FIGHTREL ~ SAWTRUMP+ARTICLE+SPANISH+MEXICAN+CUBAN+PUERTORICAN+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+
                           as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==0 & dta$KNOWLEDGE==0,])
summary(loutlatino2relknow)



models1 <- rev(c("loutall2richknow","loutasian2richknow","loutlatino2richknow"))
models3 <- rev(c("loutall2relknow","loutasian2relknow","loutlatino2relknow"))
models2 <- rev(c("loutall2taxknow","loutasian2taxknow","loutlatino2taxknow"))
models4 <- rev(c("loutall2inknow","loutasian2inknow","loutlatino2inknow"))

outcomes <- rev(c("Fights for My Group","Fights for Religious","Fights to Cut Taxes","Fights for Rich"))

pch.options <- c(16,17,18,19)
groups <- rev(c("All Respondents","Asian Americans","Latinos"))

#pdf("exp2-video0mainresultsjoint02182022lowknowledge.pdf")
plot(1:19,type="n",ylab="",ylim=c(0.5,19.5),xlim=c(-.5,.5),yaxt="n",xlab="Descibes GOP             Describes Dems \n Low-Knowledge Respondents",
     main="Effect of Trump Video",cex.main=1.5,cex.lab=1.25)

k <- -1
### loop over 3 dependent variables
for(j in 1:4){
  k <- k+2
  txt10 <- paste("models <- models",j,sep="")
  eval(parse(text=txt10))
  for(i in 1:length(models)){
    txt <- paste("lout <- ",models[i])
    eval(parse(text=txt))
    points(x=summary(lout)$coef[2,1],y=k,pch=pch.options[i])	
    lines(x=c(summary(lout)$coef[2,1]-summary(lout)$coef[2,2]*1.96,summary(lout)$coef[2,1]+summary(lout)$coef[2,2]*1.96),y=c(k,k),pch=2)	
    #text(x=summary(lout)$coef[2,1],y=k+.25,lbs[i])
    lines(x=c(summary(lout)$coef[2,1]-summary(lout)$coef[2,2],summary(lout)$coef[2,1]+summary(lout)$coef[2,2]),y=c(k,k),pch=2,lwd=2)	
    if(j==4){
      #text(groups[i],x=(summary(lout)$coef[2,1]-summary(lout)$coef[2,2]*1.96-2),y=k)
      text(groups[i],x=0.365,y=k)
      
    }
    k <- k+1
  }
}
abline(v=0,lty=2)
#dev.off()

text(y=3.95,x=0,outcomes[1],cex=1.4)
text(y=8.95,x=0,outcomes[2],cex=1.4)
text(y=13.95,x=0,outcomes[3],cex=1.4)
text(y=18.95,x=0,outcomes[4],cex=1.4)

#dev.off()

#### DESCR STATS AND WEIGHTS ----
#### Figure A-2

dtam2$CA.W1 <- 1*(dtam2$PPSTATEN.W1=="CA")
dtam2$TX.W1 <- 1*(dtam2$PPSTATEN.W1=="TX")
dtam2$NY.W1 <- 1*(dtam2$PPSTATEN.W1=="NY")
dtam2$FL.W1 <- 1*(dtam2$PPSTATEN.W1=="FL")

dtam2$AGE.W1 <- as.numeric(as.character(dtam2$PPAGE.W1))

dtam2$WAVE3 <- 1*(! dtam2$duration.W3 %in% c(NA))

dta.asn <- dtam2[dtam2$ASIAN==1 & dtam2$OPTIN==0,]
dta.his <- dtam2[dtam2$ASIAN==0 & dtam2$OPTIN==0,]

vars <- c("CITIZEN.W1","BORNUS.W1",
          "ASIAN.W1","JAPANESE.W1","CHINESE.W1","INDIAN.W1","FILIPINO.W1",
          "EDYEARS.W1","FEMALE.W1","INCOME.W1","AGE.W1","NONVOTE16",
          "CA.W1","TX.W1","NY.W1","FL.W1")
rmat <- matrix(NA,length(vars),6)
for(i in 1:length(vars)){
  txt <- paste("hold <- dta.asn$",vars[i],sep="")
  eval(parse(text=txt))
  rmat[i,1] <- mean(hold,na.rm=T)
  rmat[i,2] <- sd(hold,na.rm=T)
  
  rmat[i,3] <- mean(hold[! dta.asn$duration.W2 %in% c(NA)],na.rm=T)
  rmat[i,4] <- sd(hold[! dta.asn$duration.W2 %in% c(NA)],na.rm=T)
  
  rmat[i,5] <- mean(hold[! dta.asn$duration.W3 %in% c(NA)],na.rm=T)
  rmat[i,6] <- sd(hold[! dta.asn$duration.W3 %in% c(NA)],na.rm=T)
}
rownames(rmat) <- vars
round(rmat,digits=2)
### TABLE A-1
xtable(rmat,digits=c(0,rep(3,6)))

### TABLE A-2

vars <- c("CITIZEN.W1","BORNUS.W1",
          "HISPANIC.W1","SPANISH.W1","MEXICAN.W1","CUBAN.W1",
          "PUERTORICAN.W1",
          "EDYEARS.W1","FEMALE.W1","INCOME.W1","AGE.W1","NONVOTE16",
          "CA.W1","TX.W1","NY.W1","FL.W1")
rmat <- matrix(NA,length(vars),6)
for(i in 1:length(vars)){
  txt <- paste("hold <- dta.his$",vars[i],sep="")
  eval(parse(text=txt))
  rmat[i,1] <- mean(hold,na.rm=T)
  rmat[i,2] <- sd(hold,na.rm=T)
  
  rmat[i,3] <- mean(hold[! dta.his$duration.W2 %in% c(NA)],na.rm=T)
  rmat[i,4] <- sd(hold[! dta.his$duration.W2 %in% c(NA)],na.rm=T)
  
  rmat[i,5] <- mean(hold[! dta.his$duration.W3 %in% c(NA)],na.rm=T)
  rmat[i,6] <- sd(hold[! dta.his$duration.W3 %in% c(NA)],na.rm=T)
}
rownames(rmat) <- vars
round(rmat,digits=2)
xtable(rmat,digits=c(0,rep(3,6)))

#### FIGURE A8-MANIPULATION
dta$SAW <- 1
dta$SAW[dta$S1=="No" & ! dta$S1 %in% c("",NA)] <- 0
dta$SAW[dta$S1=="Refused" & ! dta$S1 %in% c("",NA)] <- 0

dta$SAW[dta$Timer2 < 20 | dta$Timer2 > 120] <- 0

dtahis <- dta[dta$ASIAN==0,]
dtaasn <- dta[dta$ASIAN==1 & dta$OPTIN==0,]
dtaspn <- dta[dta$ASIAN==0 & dta$SPANISH==1,]

#####
loutsaw2ct <- lm(TRUMPVC10B ~ SAWTRUMP+ARTICLE+SPANISH+EDYEARS+FEMALE+
                   as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+
                   as.factor(PIDPRE),data=dta[dta$OPTIN==0 & dta$SAW==1,])
summary(loutsaw2ct)

loutsawasian2ct <- lm(TRUMPVC10B ~ SAWTRUMP+ARTICLE+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+
                        FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+
                        as.factor(PIDPRE),data=dta[dta$ASIAN==1 & dta$OPTIN==0 & dta$SAW==1,])
summary(loutasian2ct)

loutsawlatino2ct <- lm(TRUMPVC10B ~ SAWTRUMP+ARTICLE+SPANISH+MEXICAN+CUBAN+PUERTORICAN+
                         EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+
                         as.factor(PIDPRE),data=dta[dta$ASIAN==0 & dta$SAW==1,])
summary(loutlatino2ct)

loutsaw2cr <- lm(RUBIOVC10B ~ SAWTRUMP+ARTICLE+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+
                   BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$OPTIN==0 & dta$SAW==1,])
summary(loutall2cr)

loutsawasian2cr <- lm(RUBIOVC10B ~ SAWTRUMP+ARTICLE+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+FEMALE+
                        as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+
                        as.factor(PIDPRE),data=dta[dta$ASIAN==1 & dta$OPTIN==0 & dta$SAW==1,])
summary(loutasian2cr)

loutsawlatino2cr <- lm(RUBIOVC10B ~ SAWTRUMP+ARTICLE+SPANISH+MEXICAN+CUBAN+PUERTORICAN+
                         EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+
                         as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==0 & dta$SAW==1,])
summary(loutlatino2cr)

loutsaw2cc <- lm(CRUZVC10B ~ SAWTRUMP+ARTICLE+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+
                   CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$OPTIN==0 & dta$SAW==1,])
summary(loutall2cc)

loutsawasian2cc <- lm(CRUZVC10B ~ SAWTRUMP+ARTICLE+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+FEMALE+
                        as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),
                      data=dta[dta$ASIAN==1 & dta$OPTIN==0 & dta$SAW==1,])
summary(loutsawasian2cc)

loutsawlatino2cc <- lm(CRUZVC10B ~ SAWTRUMP+ARTICLE+SPANISH+MEXICAN+CUBAN+
                         PUERTORICAN+EDYEARS+FEMALE+as.factor(ppagect4)+
                         CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),
                       data=dta[dta$ASIAN==0 & dta$SAW==1,])
summary(loutlatino2cc)

models1 <- rev(c("loutsaw2ct","loutsawasian2ct","loutsawlatino2ct"))
models2 <- rev(c("loutsaw2cr","loutsawasian2cr","loutsawlatino2cr"))
models3 <- rev(c("loutsaw2cc","loutsawasian2cc","loutsawlatino2cc"))
#models4 <- rev(c("loutall2in","loutasian2in","loutlatino2in"))

outcomes <- rev(c("Cruz vs. Clinton","Rubio vs. Clinton","Trump vs. Clinton"))

pch.options <- c(16,17,18,19)
groups <- rev(c("All Respondents","Asian Americans","Latinos"))

#pdf("exp2-video0-saw-candidarteresultsjoint06092022.pdf")
plot(1:19,type="n",ylab="",ylim=c(0.5,14.5),xlim=c(-.5,.5),yaxt="n",xlab="Support for Republican",
     main="Effect of Trump Video",cex.main=1.5,cex.lab=1.25)

k <- -1
### loop over 3 dependent variables
for(j in 1:3){
  k <- k+2
  txt10 <- paste("models <- models",j,sep="")
  eval(parse(text=txt10))
  for(i in 1:length(models)){
    txt <- paste("lout <- ",models[i])
    eval(parse(text=txt))
    points(x=summary(lout)$coef[2,1],y=k,pch=pch.options[i])	
    lines(x=c(summary(lout)$coef[2,1]-summary(lout)$coef[2,2]*1.96,summary(lout)$coef[2,1]+summary(lout)$coef[2,2]*1.96),y=c(k,k),pch=2)	
    #text(x=summary(lout)$coef[2,1],y=k+.25,lbs[i])
    lines(x=c(summary(lout)$coef[2,1]-summary(lout)$coef[2,2],summary(lout)$coef[2,1]+summary(lout)$coef[2,2]),y=c(k,k),pch=2,lwd=2)	
    if(j==3){
      #text(groups[i],x=(summary(lout)$coef[2,1]-summary(lout)$coef[2,2]*1.96-2),y=k)
      text(groups[i],x=0.35,y=k)
      
    }
    k <- k+1
  }
}
abline(v=0,lty=2)

text(y=3.95,x=0,outcomes[1],cex=1.4)
text(y=8.95,x=0,outcomes[2],cex=1.4)
text(y=13.95,x=0,outcomes[3],cex=1.4)
#text(y=18.95,x=0,outcomes[4],cex=1.4)

#dev.off()

loutsaw2in <- lm(FIGHTIN ~ SAWTRUMP+ARTICLE+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+
                   as.factor(PIDPRE),data=dta[dta$OPTIN==0 & dta$SAW==1,])
summary(loutsaw2in)

loutsawasian2in <- lm(FIGHTIN ~ SAWTRUMP+ARTICLE+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+
                        BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==1 & dta$OPTIN==0 & dta$SAW==1,])

loutsawoptin2in <- lm(FIGHTIN ~ SAWTRUMP+ARTICLE+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+
                        BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==1 & dta$OPTIN==1 & dta$SAW==1,])

loutsawlatino2in <- lm(FIGHTIN ~ SAWTRUMP+ARTICLE+SPANISH+MEXICAN+CUBAN+PUERTORICAN+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+
                         BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==0 & dta$SAW==1,])
summary(loutsawlatino2in)

loutsaw2rich <- lm(FIGHTRICH ~ SAWTRUMP+ARTICLE+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+
                     CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$OPTIN==0 & dta$SAW==1,])
summary(loutall2rich)

loutsawasian2rich <- lm(FIGHTRICH ~ SAWTRUMP+ARTICLE+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+FEMALE+
                          as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),
                        data=dta[dta$ASIAN==1 & dta$OPTIN==0 & dta$SAW==1,])
summary(loutasian2rich)

loutsawlatino2rich <- lm(FIGHTRICH ~ SAWTRUMP+ARTICLE+SPANISH+MEXICAN+CUBAN+PUERTORICAN+
                           EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+
                           as.factor(PIDPRE),data=dta[dta$ASIAN==0 & dta$SAW==1,])
summary(loutlatino2rich)


loutsaw2tax <- lm(FIGHTTAX ~ SAWTRUMP+ARTICLE+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+
                    CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),
                  data=dta[dta$OPTIN==0 & dta$SAW==1,])
summary(loutall2tax)

loutsawasian2tax <- lm(FIGHTTAX ~ SAWTRUMP+ARTICLE+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+
                         FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+
                         as.factor(PIDPRE),data=dta[dta$ASIAN==1 & dta$OPTIN==0 & dta$SAW==1,])
summary(loutasian2tax)

loutsawlatino2tax <- lm(FIGHTTAX ~ SAWTRUMP+ARTICLE+SPANISH+MEXICAN+CUBAN+PUERTORICAN+EDYEARS+
                          FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+
                          as.factor(PIDPRE),data=dta[dta$ASIAN==0 & dta$SAW==1,])
summary(loutlatino2tax)


loutsaw2rel <- lm(FIGHTREL ~ SAWTRUMP+ARTICLE+SPANISH+EDYEARS+FEMALE+
                    as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+
                    as.factor(PIDPRE),data=dta[dta$OPTIN==0 & dta$SAW==1,])
summary(loutall2rel)

loutsawasian2rel <- lm(FIGHTREL ~ SAWTRUMP+ARTICLE+JAPANESE+CHINESE+INDIAN+FILIPINO+
                         EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+
                         as.factor(PIDPRE),data=dta[dta$ASIAN==1 & dta$OPTIN==0 & dta$SAW==1,])
summary(loutasian2rel)

loutsawlatino2rel <- lm(FIGHTREL ~ SAWTRUMP+ARTICLE+SPANISH+MEXICAN+CUBAN+PUERTORICAN+
                          EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+
                          as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$ASIAN==0 & dta$SAW==1,])
summary(loutlatino2rel)


models1 <- rev(c("loutsaw2rich","loutsawasian2rich","loutsawlatino2rich"))
models3 <- rev(c("loutsaw2rel","loutsawasian2rel","loutsawlatino2rel"))
models2 <- rev(c("loutsaw2tax","loutsawasian2tax","loutsawlatino2tax"))
models4 <- rev(c("loutsaw2in","loutsawasian2in","loutsawlatino2in"))

outcomes <- rev(c("Fights for My Group","Fights for Religious","Fights to Cut Taxes","Fights for Rich"))

pch.options <- c(16,17,18,19)
groups <- rev(c("All Respondents","Asian Americans","Latinos"))

#pdf("exp2-video-saw-resultsjoint06092022.pdf")
plot(1:19,type="n",ylab="",ylim=c(0.5,19.5),xlim=c(-.5,.5),yaxt="n",xlab="Descibes GOP             Describes Dems",
     main="Effect of Trump Video",cex.main=1.5,cex.lab=1.25)

k <- -1
### loop over 3 dependent variables
for(j in 1:4){
  k <- k+2
  txt10 <- paste("models <- models",j,sep="")
  eval(parse(text=txt10))
  for(i in 1:length(models)){
    txt <- paste("lout <- ",models[i])
    eval(parse(text=txt))
    points(x=summary(lout)$coef[2,1],y=k,pch=pch.options[i])	
    lines(x=c(summary(lout)$coef[2,1]-summary(lout)$coef[2,2]*1.96,summary(lout)$coef[2,1]+summary(lout)$coef[2,2]*1.96),y=c(k,k),pch=2)	
    #text(x=summary(lout)$coef[2,1],y=k+.25,lbs[i])
    lines(x=c(summary(lout)$coef[2,1]-summary(lout)$coef[2,2],summary(lout)$coef[2,1]+summary(lout)$coef[2,2]),y=c(k,k),pch=2,lwd=2)	
    if(j==4){
      #text(groups[i],x=(summary(lout)$coef[2,1]-summary(lout)$coef[2,2]*1.96-2),y=k)
      text(groups[i],x=0.35,y=k)
      
    }
    k <- k+1
  }
}
abline(v=0,lty=2)
#dev.off()

text(y=3.95,x=0,outcomes[1],cex=1.4)
text(y=8.95,x=0,outcomes[2],cex=1.4)
text(y=13.95,x=0,outcomes[3],cex=1.4)
text(y=18.95,x=0,outcomes[4],cex=1.4)

#dev.off()

### FIGURE A9: OPEN-ENDED

dta.ppw1.eng <- read.csv("PDSurvey-wave1-party-Coding-062216EZ.csv")
dta.ppw1.spn <- read.csv("oecodespanish-7-1-2016-DM.csv")

### standardize column names
colnames(dta.ppw1.spn)[which(colnames(dta.ppw1.spn)=="NEGDEM")] <- "NEGATIVED"
colnames(dta.ppw1.spn)[which(colnames(dta.ppw1.spn)=="NEGREP")] <- "NEGATIVER"
colnames(dta.ppw1.spn)[which(colnames(dta.ppw1.spn)=="POSDEM")] <- "POSITIVED"
colnames(dta.ppw1.spn)[which(colnames(dta.ppw1.spn)=="POSREP")] <- "POSITIVER"

cn.both <- colnames(dta.ppw1.eng)[colnames(dta.ppw1.eng) %in% colnames(dta.ppw1.spn)]

dta.sub1 <- subset(dta.ppw1.eng,select=cn.both)
dta.sub2 <- subset(dta.ppw1.spn,select=cn.both)

sum(dta.sub1$CaseID %in% dta.sub2$CaseID)

identical(colnames(dta.sub1),colnames(dta.sub2))

dta.sub <- rbind(dta.sub1,dta.sub2)

colnames(dta)[1] <- "CaseID"
dta.oe <- merge(dta,dta.sub,by=c("CaseID"))
###


lout1allnd <- lm(I(NEGATIVED) ~ ARTICLE+SAWTRUMP+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP),data=dta.oe[dta.oe$OPTIN==0,])
summary(lout1allnd)

lout1allnd <- lm(I(NEGATIVED) ~ ARTICLE+SAWTRUMP+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP),data=dta.oe[dta.oe$OPTIN==0,])
summary(lout1allnd)

lout1asiannd <- lm(I(NEGATIVED) ~ ARTICLE+SAWTRUMP+OPTIN+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP),data=dta.oe[dta.oe$ASIAN==1 & dta.oe$OPTIN==0,])
summary(lout1asiannd)

lout1optinnd <- lm(I(NEGATIVED) ~ ARTICLE+SAWTRUMP+OPTIN+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP),data=dta.oe[dta.oe$ASIAN==1 & dta.oe$OPTIN==1,])
summary(lout1optinnd)

lout1latinond <- lm(I(NEGATIVED) ~ ARTICLE+SAWTRUMP+SPANISH+MEXICAN+CUBAN+PUERTORICAN+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP),data=dta.oe[dta.oe$ASIAN==0,])
summary(lout1latinond)


lout1allnr <- lm(I(NEGATIVER) ~ ARTICLE+SAWTRUMP+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP),data=dta.oe[dta.oe$OPTIN==0,])
summary(lout1allnr)

lout1asiannr <- lm(I(NEGATIVER) ~ ARTICLE+SAWTRUMP+OPTIN+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP),data=dta.oe[dta.oe$ASIAN==1 & dta.oe$OPTIN==0,])
summary(lout1asiannr)

lout1optinnr <- lm(I(NEGATIVER) ~ ARTICLE+SAWTRUMP+OPTIN+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP),data=dta.oe[dta.oe$ASIAN==1 & dta.oe$OPTIN==1,])
summary(lout1optinnr)

lout1latinonr <- lm(I(NEGATIVER) ~ ARTICLE+SAWTRUMP+SPANISH+MEXICAN+CUBAN+PUERTORICAN+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP),data=dta.oe[dta.oe$ASIAN==0,])
summary(lout1latinonr)



lout1allpd <- lm(I(POSITIVED) ~ ARTICLE+SAWTRUMP+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP),data=dta.oe[dta.oe$OPTIN==0,])
summary(lout1allpd)

lout1asianpd <- lm(I(POSITIVED) ~ ARTICLE+SAWTRUMP+OPTIN+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP),data=dta.oe[dta.oe$ASIAN==1 & dta.oe$OPTIN==0,])
summary(lout1asianpd)

lout1optinpd <- lm(I(POSITIVED) ~ ARTICLE+SAWTRUMP+OPTIN+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP),data=dta.oe[dta.oe$ASIAN==1 & dta.oe$OPTIN==1,])
summary(lout1optinpd)

lout1latinopd <- lm(I(POSITIVED) ~ ARTICLE+SAWTRUMP+SPANISH+MEXICAN+CUBAN+PUERTORICAN+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP),data=dta.oe[dta.oe$ASIAN==0,])
summary(lout1latinopd)


lout1allpr <- lm(I(POSITIVER) ~ ARTICLE+SAWTRUMP+SPANISH+EDYEARS+FEMALE+
                   as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP),
                 data=dta.oe[dta.oe$OPTIN==0,])
summary(lout1allpr)

lout1asianpr <- lm(I(POSITIVER) ~ ARTICLE+SAWTRUMP+OPTIN+JAPANESE+CHINESE+
                     INDIAN+FILIPINO+EDYEARS+FEMALE+as.factor(ppagect4)+
                     CITIZEN+BORNUS+as.factor(PPINCIMP),
                   data=dta.oe[dta.oe$ASIAN==1 & dta.oe$OPTIN==0,])
summary(lout1asianpr)

lout1optinpr <- lm(I(POSITIVER) ~ ARTICLE+SAWTRUMP+OPTIN+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP),data=dta.oe[dta.oe$ASIAN==1 & dta.oe$OPTIN==1,])
summary(lout1optinpr)

lout1latinopr <- lm(I(POSITIVER) ~ ARTICLE+SAWTRUMP+SPANISH+MEXICAN+CUBAN+PUERTORICAN+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP),data=dta.oe[dta.oe$ASIAN==0,])
summary(lout1latinopr)

#models <- rev(c("lout1alldr","lout1asiandr","lout1latinodr"))
#lbs <- rev(c("All","Asian Americans","Latinos"))

models4 <- rev(c("lout1allnr","lout1asiannr","lout1latinonr"))
models3 <- rev(c("lout1allnd","lout1asiannd","lout1latinond"))
models2 <- rev(c("lout1allpr","lout1asianpr","lout1latinopr"))
models1 <- rev(c("lout1allpd","lout1asianpd","lout1latinopd"))

outcomes <- c("Positive Dems","Positive GOP","Negative Dems","Negative GOP")

pch.options <- c(16,17,18)
groups <- rev(c("All Respondents","Asian Americans","Latinos"))

#pdf("exp2oeresults06102022.pdf")
plot(1:14,type="n",ylab="",ylim=c(0.5,19.5),xlim=c(-.18,.18),yaxt="n",
     xlab="Change in Party Mentions",
     main="Effect of Trump Video",cex.main=1.5,cex.lab=1.25)

k <- -1
### loop over 3 dependent variables
for(j in 1:4){
  k <- k+2
  txt10 <- paste("models <- models",j,sep="")
  eval(parse(text=txt10))
  for(i in 1:length(models)){
    txt <- paste("lout <- ",models[i])
    eval(parse(text=txt))
    points(x=summary(lout)$coef[3,1],y=k,pch=pch.options[i])	
    lines(x=c(summary(lout)$coef[3,1]-summary(lout)$coef[3,2]*1.96,summary(lout)$coef[3,1]+summary(lout)$coef[3,2]*1.96),y=c(k,k),pch=2)	
    #text(x=summary(lout)$coef[2,1],y=k+.25,lbs[i])
    lines(x=c(summary(lout)$coef[3,1]-summary(lout)$coef[2,2],summary(lout)$coef[3,1]+summary(lout)$coef[3,2]),y=c(k,k),pch=2,lwd=2)	
    if(j==4){
      #text(groups[i],x=(summary(lout)$coef[2,1]-summary(lout)$coef[2,2]*1.96-2),y=k)
      text(groups[i],x=.14,y=k)
      
    }
    k <- k+1
  }
}
abline(v=0,lty=2)
#dev.off()

text(y=3.95,x=0,outcomes[1],cex=1.4)
text(y=8.95,x=0,outcomes[2],cex=1.4)
text(y=13.95,x=0,outcomes[3],cex=1.4)
text(y=18.95,x=0,outcomes[4],cex=1.4)

dev.off()


#### MEXICAN SUBSET TABLE A11-A12
loutmex2ct <- lm(TRUMPVC10B ~ SAWTRUMP+ARTICLE+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+
                   BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$OPTIN==0 & dta$MEXICAN==1,])
summary(loutmex2ct)

loutmex2cr <- lm(RUBIOVC10B ~ SAWTRUMP+ARTICLE+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+
                   BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$OPTIN==0 & dta$MEXICAN==1,])
summary(loutall2cr)

loutmex2cc <- lm(CRUZVC10B ~ SAWTRUMP+ARTICLE+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+
                   CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),
                 data=dta[dta$OPTIN==0 & dta$MEXICAN==1,])

texreg(list(loutmex2ct,loutmex2cr,loutmex2cc),stars=0.05,digits=3,
       custom.model.names=c("vs. Trump","vs. Rubio","vs. Cruz"))

loutmex2fightin <- lm(FIGHTIN ~ SAWTRUMP+ARTICLE+SPANISH+EDYEARS+FEMALE+
                        as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+
                        as.factor(PIDPRE),data=dta[dta$OPTIN==0 & dta$MEXICAN==1,])


loutmex2fightrich <- lm(FIGHTRICH ~ SAWTRUMP+ARTICLE+SPANISH+EDYEARS+FEMALE+
                          as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+
                          as.factor(PIDPRE),data=dta[dta$OPTIN==0 & dta$MEXICAN==1,])

loutmex2fighttax <- lm(FIGHTTAX ~ SAWTRUMP+ARTICLE+SPANISH+MEXICAN+CUBAN+PUERTORICAN+
                         EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+
                         as.factor(PIDPRE),data=dta[dta$ASIAN==0 & dta$MEXICAN==1,])

loutmex2fightrel <- lm(FIGHTTAX ~ SAWTRUMP+ARTICLE+SPANISH+MEXICAN+CUBAN+PUERTORICAN+
                         EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP)+
                         as.factor(PIDPRE),data=dta[dta$ASIAN==0 & dta$MEXICAN==1,])

texreg(list(loutmex2fightin,loutmex2fightrich,loutmex2fighttax,loutmex2fightrel),
       stars=0.05,digits=3,custom.model.names=c("Fights In-group","Fights Rich",
                                                "Fights Taxes","Fights Religious"))

loutmex2cr <- lm(RUBIOVC10B ~ SAWTRUMP+ARTICLE+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+
                   BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),data=dta[dta$OPTIN==0 & dta$MEXICAN==1,])
summary(loutall2cr)

loutmex2cc <- lm(CRUZVC10B ~ SAWTRUMP+ARTICLE+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+
                   CITIZEN+BORNUS+as.factor(PPINCIMP)+as.factor(PIDPRE),
                 data=dta[dta$OPTIN==0 & dta$MEXICAN==1,])

texreg(list(loutmex2ct,loutmex2cr,loutmex2cc),stars=0.05,digits=3,
       custom.model.names=c("vs. Trump","vs. Rubio","vs. Cruz"))

#### weights
gout <- glm(I(WAVE3==0) ~ CITIZEN.W1+BORNUS.W1+JAPANESE.W1+CHINESE.W1+INDIAN.W1+FILIPINO.W1+
              EDYEARS.W1+FEMALE.W1+INCOME.W1+AGE.W1+NONVOTE16,
            family=binomial(link="logit"),data=dta.asn)

p1 <- predict(gout,type="response")
dta.asn$newweight <- p1

dta.asn$newweight2 <- NA
dta.asn$newweight2[dta.asn$WAVE3==0 | dta.asn$PID3 %in% c(NA)] <- 0

bottom <- sum(dta.asn$newweight[dta.asn$WAVE3==1 & ! dta.asn$PID3 %in% c(NA)])
top <- sum(dta.asn$WAVE3[! dta.asn$PID3 %in% c(NA)])

dta.asn$newweight2[dta.asn$WAVE3==1 & ! dta.asn$PID3 %in% c(NA)] <- 
  dta.asn$newweight[dta.asn$WAVE3==1 & ! dta.asn$PID3 %in% c(NA)]*(top/bottom)

dta.asn.sub <- dta.asn[! dta.asn$PIDPRE.W1 %in% c(NA) & ! dta.asn$PID3 %in% c(NA),]

ttw <- matrix(0,7,7)
for(i in 1:7){
  for(j in 1:7){
    ttw[i,j] <- weighted.mean(1*(dta.asn.sub$PIDPRE.W1==i & dta.asn.sub$PID3==j),
                              w=dta.asn.sub$newweight2)
    
    #sum(dta.asn.sub$PIDPRE.W1==i & dta.asn.sub$PID3==j,na.rm=T)#*
    #sum(dta.asn.sub$newweight2[dta.asn.sub$PIDPRE.W1==i & dta.asn.sub$PID3==j],na.rm=T)
  }
}
sum(diag(ttw/sum(ttw)))

#[1] 0.7030568

tt2 <- round(prop.table(ttw),digits=3)
colnames(tt2) <- rownames(tt2) <- c("SD","WD","ID","I","IR","WR","SR")

#rownames(tt2) <- c("Dem 16","Neither 16","GOP 16")
#colnames(tt2) <- c("Dem 18","Neither 18","GOP 18")

co=melt(tt2)
head(co)
colnames(co) <- c("Wave1","Wave3","value")

pdf("partisan-transition-asn-weights-02172022.pdf")

ggplot(co, aes(Wave1, Wave3)) + # x and y axes => Var1 and Var2
  geom_tile(aes(fill = value)) + # background colours are mapped according to the value column
  #theme_gray(base_size=24) +
  geom_text(aes(fill = value, label = round(value, 2)),size=6) + # write the values
  scale_fill_gradient2(low = "#F2F2F2", 
                       #mid = "#B7B7B7", 
                       high = "#7F7F7F")+#, 
  #midpoint = mean(co$value)) + # determine the colour
  #scale_colour_grey()+theme_bw()+
  theme(panel.grid.major.x=element_blank(), #no gridlines
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), # background=white
        axis.text.x = element_text(angle=60, hjust = 1,vjust=1,size = 16,face = "bold"),
        axis.title.x = element_text(size=18,face="bold"),
        plot.title = element_text(size=20,face="bold"),
        axis.text.y = element_text(size = 16,face = "bold"),
        axis.title.y = element_text(size=18,face="bold"))+
  ggtitle("Weighted Partisan Transitions: \n As. Americans") + 
  theme(legend.title=element_text(face="bold", size=12)) + 
  scale_x_discrete(name="2016") +
  scale_y_discrete(name="2018") +
  labs(fill="Share of As. Americans")

dev.off()


#### weights latinos ----

gout <- glm(I(WAVE3==0) ~ CITIZEN.W1+BORNUS.W1+SPANISH.W1+MEXICAN.W1+PUERTORICAN.W1+CUBAN.W1+
              EDYEARS.W1+FEMALE.W1+INCOME.W1+AGE.W1+NONVOTE16,
            family=binomial(link="logit"),data=dta.his)

p1 <- predict(gout,type="response")
dta.his$newweight <- p1

dta.his$newweight2 <- NA
dta.his$newweight2[dta.his$WAVE3==0 | dta.his$PID3 %in% c(NA)] <- 0

bottom <- sum(dta.his$newweight[dta.his$WAVE3==1 & ! dta.his$PID3 %in% c(NA)])
top <- sum(dta.his$WAVE3[! dta.his$PID3 %in% c(NA)])

dta.his$newweight2[dta.his$WAVE3==1 & ! dta.his$PID3 %in% c(NA)] <- 
  dta.his$newweight[dta.his$WAVE3==1 & ! dta.his$PID3 %in% c(NA)]*(top/bottom)

dta.his.sub <- dta.his[! dta.his$PIDPRE.W1 %in% c(NA) & ! dta.his$PID3 %in% c(NA),]

ttc <- ttw2 <- matrix(0,7,7)
for(i in 1:7){
  for(j in 1:7){
    ttw2[i,j] <- weighted.mean(1*(dta.his.sub$PIDPRE.W1==i & dta.his.sub$PID3==j),
                               w=dta.his.sub$newweight2)
    ttc[i,j] <- sum(dta.his.sub$PIDPRE.W1==i & dta.his.sub$PID3==j)
    #sum(dta.asn.sub$PIDPRE.W1==i & dta.asn.sub$PID3==j,na.rm=T)#*
    #sum(dta.asn.sub$newweight2[dta.asn.sub$PIDPRE.W1==i & dta.asn.sub$PID3==j],na.rm=T)
  }
}
sum(diag(ttc/sum(ttc)))
#[1] 0.5636364

sum(diag(ttw2/sum(ttw2)))
#[1] 0.5578494

tt2 <- round(prop.table(ttw2),digits=3)
colnames(tt2) <- rownames(tt2) <- c("SD","WD","ID","I","IR","WR","SR")

#rownames(tt2) <- c("Dem 16","Neither 16","GOP 16")
#colnames(tt2) <- c("Dem 18","Neither 18","GOP 18")

co=melt(tt2)
head(co)
colnames(co) <- c("Wave1","Wave3","value")

#pdf("partisan-transition-his-weights-02172022.pdf")

ggplot(co, aes(Wave1, Wave3)) + # x and y axes => Var1 and Var2
  geom_tile(aes(fill = value)) + # background colours are mapped according to the value column
  #theme_gray(base_size=24) +
  geom_text(aes(fill = value, label = round(value, 2)),size=6) + # write the values
  scale_fill_gradient2(low = "#F2F2F2", 
                       #mid = "#B7B7B7", 
                       high = "#7F7F7F")+#, 
  #midpoint = mean(co$value)) + # determine the colour
  #scale_colour_grey()+theme_bw()+
  theme(panel.grid.major.x=element_blank(), #no gridlines
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), # background=white
        axis.text.x = element_text(angle=60, hjust = 1,vjust=1,size = 16,face = "bold"),
        axis.title.x = element_text(size=18,face="bold"),
        plot.title = element_text(size=20,face="bold"),
        axis.text.y = element_text(size = 16,face = "bold"),
        axis.title.y = element_text(size=18,face="bold"))+
  ggtitle("Weighted Partisan Transitions: \n Latinos") + 
  theme(legend.title=element_text(face="bold", size=12)) + 
  scale_x_discrete(name="2016") +
  scale_y_discrete(name="2018") +
  labs(fill="Share of Latinos")

dev.off()




