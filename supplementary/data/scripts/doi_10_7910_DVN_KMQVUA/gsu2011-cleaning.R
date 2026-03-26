###############################################################################
## RECODING
###############################################################################

library("foreign")
library("car")
data2 <- read.dta("Fall2011data.dta")
#data <- na.omit(data1)

data1 <- subset(data2,select=c(Q115,Q117,Q119,Q121,Q123_0,Q125,Q127,Q129_0,Q131_0,Q133,Q135_0,Q137,Q139_0,Q141_0,Q7,Q8,Q9,Q10,Q291,Q13,Q14,Q261,Q458))
data <- data1

# KNOWLEDGE QUESTIONS
#Q101 # PRESIDENTIAL TERMS (OPEN-ENDED)
#Q102 # LENGTH OF US SENATOR'S TERM (OPEN-ENDED)
#Q103 # NUMBER OF SENATORS FROM EACH STATE (OPEN-ENDED)
#Q104 # LENGTH OF US REP'S TERM (OPEN-ENDED)
#Q105 # NAME OF UK PRIME MINISTER (OPEN-ENDED)
#Q100_0 # CURRENT SPEAKER OF US HOUSE (CLOSED-ENDED, 'John Boehner')
#Q104_0 # REPUBLICAN CONTROL OF CONGRESS (CLOSED-ENDED, 'The House of Representatives only')
#Q108 # FOCUS OF WI PROTESTS (CLOSED-ENDED, 'Union rights for public employees')
##Q112 # STATE IN WHICH DOZENS OF WILD ANIMALS WERE RELEASED (CLOSED-ENDED, 'Ohio') NOT USED!!!
#Q116 # COUNTRY ACCUSED OF PLOTTING TO KILL SAUDI ARABIAN DIPLOMAT (CLOSED-ENDED, 'Iran')

data$know1 <- 0
data$know1[grep("2",data2$Q101,fixed=TRUE)] <- 1
data$know1[grep("two",data2$Q101,fixed=TRUE)] <- 1
data$know1[grep("Two",data2$Q101,fixed=TRUE)] <- 1
data$know1[grep("twice",data2$Q101,fixed=TRUE)] <- 1
data$know1[grep("Twice",data2$Q101,fixed=TRUE)] <- 1
data$know2 <- recode(data2$Q102,"'6'=1;'06'=1;else=0",as.factor.result=FALSE)
data$know3 <- recode(data2$Q103,"'2'=1;else=0",as.factor.result=FALSE)
data$know4 <- recode(data2$Q104,"'2'=1;else=0",as.factor.result=FALSE)
data$know5 <- 0
data$know5[grep("davi",data2$Q105,fixed=TRUE)] <- 1
data$know5[grep("Davi",data2$Q105,fixed=TRUE)] <- 1
data$know5[grep("DAVI",data2$Q105,fixed=TRUE)] <- 1
data$know5[grep("cam",data2$Q105,fixed=TRUE)] <- 1
data$know5[grep("CAM",data2$Q105,fixed=TRUE)] <- 1
data$know6 <- recode(data2$Q100_0,"'John Boehner'=1;else=0",as.factor.result=FALSE)
data$know7 <- recode(data2$Q104_0,"'The House of Representatives only'=1;else=0",as.factor.result=FALSE)
data$know8 <- recode(data2$Q108,"'Union rights for public employees'=1;else=0",as.factor.result=FALSE)
data$know9 <- recode(data2$Q116,"'Iran'=1;else=0",as.factor.result=FALSE)

data$knowledge <- with(data, know1 + know2 + know3 + know4 + know5 + know6 + know7 + know8 + know9)
data$knowbin <- recode(data$knowledge,"0:4=0;5:9=1")

#Condition Numbers:
##1: Control/NoNorm-NoSci (Q115)
##2: Norm-NoSci (Q117)
##3: NoNorm-Sci (Q119)
##4: Norm-Sci (Q121)
##5: NoNorm-PolSci (Q123_0)
##6: Norm-PolSci (Q125)

data$Q115=recode(data$Q115,"1=1;else=0")
data$Q117=recode(data$Q117,"1=1;else=0")
data$Q119=recode(data$Q119,"1=1;else=0")
data$Q121=recode(data$Q121,"1=1;else=0")
data$Q123_0=recode(data$Q123_0,"1=1;else=0")
data$Q125=recode(data$Q125,"1=1;else=0")

data$Condition=NA
for(i in 1:dim(data)[1]) {
    if(data[i,]$Q115==1) data$Condition[i]=1
    else if(data[i,]$Q117==1) data$Condition[i]=2
    else if(data[i,]$Q119==1) data$Condition[i]=3
    else if(data[i,]$Q121==1) data$Condition[i]=4
    else if(data[i,]$Q123_0==1) data$Condition[i]=5
    else if(data[i,]$Q125==1) data$Condition[i]=6
    else end
}

#DVs
data$Increase = recode(data$Q127,"'Strongly Disagree'=-1;'Moderately Disagree'=-.66;'Slightly Disagree'=-.33;'Neither Agree nor Disagree'=0;'Slightly Agree'=.33;'Moderately Agree'=.66;'Strongly Agree'=1;else=NA",as.factor.result=FALSE)
data$Decrease = recode(data$Q129_0,"'Strongly Disagree'=-1;'Moderately Disagree'=-.66;'Slightly Disagree'=-.33;'Neither Agree nor Disagree'=0;'Slightly Agree'=.33;'Moderately Agree'=.66;'Strongly Agree'=1;else=NA",as.factor.result=FALSE)
data$PersonalAction = recode(data$Q131_0,"'extremely unwilling'=-1;'moderately unwilling'=-.66;'somewhat unwilling'=-.33;'neither willing nor unwilling'=0;'somewhat willing'=.33;'moderately willing'=.66;'extremely willing'=1;else=NA",as.factor.result=FALSE)
data$EmissionCap = recode(data$Q133,"'Strongly Oppose'=-1;'Moderately Oppose'=-.66;'Slightly Oppose'=-.33;'Neither Oppose nor Support'=0;'Slightly Support'=.33;'Moderately Support'=.66;'Strongly Support'=1;else=NA",as.factor.result=FALSE)
data$Email = recode(data$Q135_0,"'Yes'=1;'No'=0;else=NA",as.factor.result=FALSE)
data$GWBelief = recode(data$Q137,"'Strongly Disagree'=-1;'Moderately Disagree'=-.66;'Slightly Disagree'=-.33;'Neither Agree nor Disagree'=0;'Slightly Agree'=.33;'Moderately Agree'=.66;'Strongly Agree'=1;else=NA",as.factor.result=FALSE)
data$GWHuman = recode(data$Q139_0,"'definitely human induced'=1;'very likely human induced'=.66;'probably human induced'=.33;'neither human nor naturally induced'=0;'probably naturally induced'=-.33;'very likely naturally induced'=-.66;'definitely naturally induced'=-1;else=NA",as.factor.result=FALSE)
data$AdaptPrevent = recode(data$Q141_0,"'Only prevention'=-1;'mostly prevention, some adaptation'=-.5;'equal amounts of prevention and adaptation'=0;'mostly adaptation, some prevention'=.5;'Only adaptation'=1;else=NA",as.factor.result=FALSE)

#Study indicator variable (Study 1 = MTurk; Study 2 = GSU)
data$study=rep(2,dim(data)[1])

#Covariates:
data$partyid = recode(data$Q7,"'Democratic Party'=1;'Republican Party'=-1;'Independent'=0;'Something else'=0;else=0",as.factor.result=FALSE)
    temp1 = recode(data$Q8,"'Not very strong Democrat'=.66;else=0",as.factor.result=FALSE)
    temp2 = recode(data$Q9,"'Not very strong Republican'=-.66;else=0",as.factor.result=FALSE)
    temp3 = recode(data$Q10,"'Closer to the Democratic Party'=.33;else=0",as.factor.result=FALSE)
    temp4 = recode(data$Q10,"'Closer to the Republican Party'=-.33;else=0",as.factor.result=FALSE)
for(i in 1:length(data$partyid)){
    if(!temp1[i]==0) data$partyid[i] = temp1[i]
    else if(!temp2[i]==0) data$partyid[i] = temp2[i]
    else if(!temp3[i]==0) data$partyid[i] = temp3[i]
    else if(!temp4[i]==0) data$partyid[i] = temp4[i]
}
data$partyidimp = recode(data$Q291,"'Extremely important'=1;'Extremely unimportant'=-1;'Neither Important nor Unimportant'=0;'Somewhat Important'=.33;'Somewhat Unimportant'=-.33;'Very Important'=.66;'Very Unimportant'=-.66;else=0",as.factor.result=FALSE)
data$sex = recode(data$Q13,"'Male'=0;'Female'=1;else=NA",as.factor.result=FALSE)
#data$race = recode(data$Q16_,"",as.factor.result=FALSE)
data$age = 2011-as.numeric(recode(as.character(data$Q14),"'1970 or earlier'=1970"))
data$ideology = recode(data$Q261,"'Very conservative'=-1;'Somewhat conservative'=-.66;'Slightly conservative'=-.33;'Slightly liberal'=.33;'Somewhat liberal'=.66;'Very liberal'=1;else=0",as.factor.result=FALSE)


#Attention Check:
##Should be 1 for Q385_2 AND Q385_6
##Should be 0 for all other Q385_ (1-9, except 2 and 6)
data$attentive=NA
data2$Q385_1=recode(data2$Q385_1,"1=1;else=0")
data2$Q385_2=recode(data2$Q385_2,"1=1;else=0")
data2$Q385_3=recode(data2$Q385_3,"1=1;else=0")
data2$Q385_4=recode(data2$Q385_4,"1=1;else=0")
data2$Q385_5=recode(data2$Q385_5,"1=1;else=0")
data2$Q385_6=recode(data2$Q385_6,"1=1;else=0")
data2$Q385_7=recode(data2$Q385_7,"1=1;else=0")
data2$Q385_8=recode(data2$Q385_8,"1=1;else=0")
data2$Q385_9=recode(data2$Q385_9,"1=1;else=0")

for (i in 1:dim(data)[1]){
    if(data2$Q385_2[i]==1 & data2$Q385_6[i]==1) data$attentive[i]=1
    else data$attentive[i]=0
    if(data2$Q385_1[i]==1) data$attentive[i]==0
    if(data2$Q385_3[i]==1) data$attentive[i]==0
    if(data2$Q385_4[i]==1) data$attentive[i]==0
    if(data2$Q385_5[i]==1) data$attentive[i]==0
    if(data2$Q385_7[i]==1) data$attentive[i]==0
    if(data2$Q385_8[i]==1) data$attentive[i]==0
    if(data2$Q385_9[i]==1) data$attentive[i]==0
}


#Create working dataset
data <- subset(data,
               select=c(Condition,Increase,Decrease,PersonalAction,EmissionCap,Email,GWBelief,GWHuman,AdaptPrevent,
                        partyid,partyidimp,sex,age,ideology,attentive,study,knowledge,knowbin))

## CLEAN DATA

## There are 51 completely missing observations; delete them
data = subset(data,!is.na(data$Condition))

## treatment group missing data imputation
for(i in 1:dim(data)[1]){
    temp.data = subset(data, data$Condition==data$Condition[i])
    if(is.na(data$Increase[i]))
        data$Increase[i] = sample(temp.data$Increase,1)
    if(is.na(data$Decrease[i]))
        data$Decrease[i] = sample(temp.data$Decrease,1)
    if(is.na(data$PersonalAction[i]))
        data$PersonalAction[i] = sample(temp.data$PersonalAction,1)
    if(is.na(data$EmissionCap[i]))
        data$EmissionCap[i] = sample(temp.data$EmissionCap,1)
    if(is.na(data$Email[i]))
        data$Email[i] = sample(temp.data$Email,1)
    if(is.na(data$GWBelief[i]))
        data$GWBelief[i] = sample(temp.data$GWBelief,1)
    if(is.na(data$GWHuman[i]))
        data$GWHuman[i] = sample(temp.data$GWHuman,1)
    if(is.na(data$AdaptPrevent[i]))
        data$AdaptPrevent[i] = sample(temp.data$AdaptPrevent,1)    
}

# WRITE OUTPUT
write.csv(data,"gsu2011-data.csv")
