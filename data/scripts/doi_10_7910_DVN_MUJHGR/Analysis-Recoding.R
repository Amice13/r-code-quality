#########################
### VARIABLE RECODING ###
#########################

# SAMPLE VARIABLES

#MERGE Facebook and Craigslist into 'Ads' sample
table(mydat$Sample)
mydat$Sample <- recode(mydat$Sample, "1=1;2=2;3=3;4=4;5=5;6=6;7=6;else=NA")
table(mydat$Sample)
#Sample without Ads group
mydat$Sample2 <- recode(mydat$Sample, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")
table(mydat$Sample2)

#Subsamples by experimental sample
tess <- subset(mydat, Sample==1)
exit <- subset(mydat, Sample==2)
stud <- subset(mydat, Sample==3)
staff <- subset(mydat, Sample==4)
mturk <- subset(mydat, Sample==5)
craig <- subset(mydat, Sample==6)
.SampleNames <- c("National","Exit Poll", "Student", "Staff", "Mturk", "Ads")


# COVARIATES

#Party, 7pt
#table(mydat$Party)
mydat$PartyR <- recode(mydat$Party,"1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
#table(mydat$PartyR)
#Party, 3pt, leaners treated as partisans. 1= Dem, 2=Indep, 3 = Repub
mydat$party3 <- recode(mydat$PartyR,"1:3=1; 4=2; 5:7=3")
#table(mydat$party3)
#table(mydat$party3, mydat$Sample)
prop.table(table(mydat$party3, mydat$Sample), 2)

#Ideology
#table(mydat$Ideo)
mydat$IdeoR <- recode(mydat$Ideo, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
#table(mydat$IdeoR)
mydat$Ideo3 <- recode(mydat$IdeoR, "1=1; 2=1;3=1;4=2;5=3; 6=3; 7=3; else=NA")
#table(mydat$Ideo3)
#table(mydat$Ideo3, mydat$Sample)
round(prop.table(table(mydat$Ideo3, mydat$Sample),2)*100,1) # prop lib/con

#Age
#table(mydat$Age)
mydat$AgeR <- recode(mydat$Age,"1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
#table(mydat$AgeR)
#table(mydat$ppage)
mean(mydat$ppage, na.rm=TRUE)
mydat$AgeR[is.na(mydat$AgeR)] <- recode(mydat$ppage[is.na(mydat$AgeR)], "18:24 =2; 25:34=3; 35:50=4; 51:65=5; 65:89=6; else=NA")
#table(mydat$AgeR)
#table(mydat$AgeR, mydat$Sample)
with(mydat, tapply(AgeR,Sample,median,na.rm=TRUE)) # median by sample
with(mydat[mydat$Sample==1,],median(ppage,na.rm=TRUE))

#Sex
table(mydat$Sex)
mydat$SexR <- recode(mydat$Sex, "1=1; 2=2; else=NA")
#table(mydat$SexR)
#table(mydat$SexR,mydat$Sample)
round(prop.table(table(mydat$SexR,mydat$Sample),2)*100,1) # gender by sample


#Race would need some recoding
table(mydat$ppethm)
#table(exit$Race)
#table(stud$Race)
#table(staff$Race)
#table(mturk$Race)
#table(craig$Race)

#Interest
table(mydat$Interest)
mydat$InterestR <- recode(mydat$Interest, "1=1; 2=2; 3=3; 4=4; else=NA")
table(mydat$InterestR)
#tess$InterestR <- recode(tess$InterestR, "1=4; 2=3; 3=2; 4=1; else=NA")
#mean(tess$InterestR, na.rm=TRUE)
#mean(stud$InterestR, na.rm=TRUE)
#mean(staff$InterestR, na.rm=TRUE)
#mean(mturk$InterestR, na.rm=TRUE)
#mean(craig$InterestR, na.rm=TRUE)

table(mydat$Interest7)
mydat$Interest7R <- recode(mydat$Interest7, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
mean(mydat$Interest7, na.rm=TRUE)
#6.140473
(6.140473/7)*4



# OUTCOME VARIABLES

#Loan Variables
#table(mydat$LoanSupp)
mydat$LoanSuppR <- recode(mydat$LoanSupp,"1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
#table(mydat$LoanSuppR)
mydat$LoanSuppR <- as.numeric(mydat$LoanSuppR)
mydat$LoanGroupNum <- as.numeric(mydat$LoanGroup)

# Loan Strength Variables
mydat$LoanCert <- recode(mydat$LoanCert, "-99=1;-1=1",as.factor.result=FALSE)
#table(mydat$LoanCert)
mydat$LoanImp <- recode(mydat$LoanImp, "-99=1;-1=1",as.factor.result=FALSE)
#table(mydat$LoanImp)


#DREAM Variables. I create some new variables to help with subsetting and tests later.
#table(mydat$DREAMSupp)
mydat$DREAMSuppR <- recode(mydat$DREAMSupp, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA", as.numeric=TRUE)
#table(mydat$DREAMGroup)
mydat$DREAMGroup1V5 <- recode(mydat$DREAMGroup, "1=1;5=5; else=NA",as.numeric=FALSE)
#table(mydat$DREAMGroup1V5)
mydat$DREAMGroup2V5 <- recode(mydat$DREAMGroup, "2=2;5=5; else=NA",as.numeric=FALSE)
#table(mydat$DREAMGroup2V5)
mydat$DREAMGroup3V5 <- recode(mydat$DREAMGroup, "3=3;5=5; else=NA",as.numeric=FALSE)
#table(mydat$DREAMGroup3V5)
mydat$DREAMGroup4V5 <- recode(mydat$DREAMGroup, "4=4;5=5; else=NA",as.numeric=FALSE)
#table(mydat$DREAMGroup4V5)
mydat$DREAMCertR <- recode(mydat$DREAMCert,  "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA", as.numeric=TRUE)
mydat$DREAMImpR <- recode(mydat$DREAMImp, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA", as.numeric=TRUE)

#Rally variables
#table(mydat$RallyAllow)
mydat$RallyAllowR <- recode(mydat$RallyAllow, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA", as.numeric=TRUE)
#table(mydat$RallyAllowR)
#table(mydat$RallyGroup)
mydat$RallyGroupTC <- recode(mydat$RallyGroup, "1=1; 2=2; 3=1; 4=2; else=NA")
#table(mydat$RallyGroupTC)
mydat$RallyGroupLTLC <- recode(mydat$RallyGroup, "1=1; 2=2; else=NA")
#table(mydat$RallyGroupLTLC)
mydat$RallyGroupDTDC <- recode(mydat$RallyGroup, "3=3; 4=4; else=NA")
#table(mydat$RallyGroupDTDC)
mydat$RallyGroupLTDT <- recode(mydat$RallyGroup, "1=1; 3=3; else=NA")
#table(mydat$RallyGroupLTDT)
mydat$RallyGroupLCDC <- recode(mydat$RallyGroup, "2=2; 4=4; else=NA")
#table(mydat$RallyGroupLCDC)
mydat$RallyCertR <- recode(mydat$RallyCert,"1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA", as.numeric=TRUE)
mydat$RallyImpR <- recode(mydat$RallyImp,"1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA", as.numeric=TRUE)


