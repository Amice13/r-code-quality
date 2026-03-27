library(dplyr)
library(car)
library(ggplot2)
library(scales)
library(knitr)
library(reshape)

# Read the data file
df <- read.csv("~<PATH>eng_Raw3.csv")

# First clean the England data file
# add timer variables
rtime<- df %>% select(starts_with("RTime"))
less<-apply(rtime, 1, function(x)sum(x<2.1))

# add no opinion variables
ans<- df %>% select(starts_with("Ans"))
rep_test <- data.frame(ans,df$IP.Address,df$SupQ_8,df$SupQ_9)
noOpinions<-apply(ans, 1, function(x)sum(x==6))

# function for successive answers
findMaxSuccessiveSameAnswers <- function(userAns){
  maxLength<-1;
  curLength<-1;
  prevAns<--1;
  for (i in 1:length(userAns)){
    if(prevAns==userAns[i]){
      curLength<-curLength+1;
      if(curLength>maxLength){
        maxLength<-curLength;
      }
    }else{
      curLength<-1;
    }
    prevAns=userAns[i];
  }
  maxLength;
}

# create variable to identify likely repeat users (rep_users). 
rep_user <- duplicated(rep_test)

#  Checking repeat users function
df0=df %>%
  select(IP.Address,SupQ_8,SupQ_9,starts_with("Ans"))
df0<-unique(df0)


# create sameAns variable
sameAns<-apply(ans, 1, findMaxSuccessiveSameAnswers)


# clean dataset
df1 <-df %>% select(Record.ID, IP.Address, Total.Time,Mobile,Attempt,X.Placement,Y.Placement,Z.Placement,starts_with("SL1_"),starts_with("SL2_"),starts_with("SL3_"),starts_with("Ans"), contains("Sup"))
df1<-data.frame(cbind(df1,less,noOpinions,sameAns,rep_user))
df1<-filter(df1, Mobile==0, Record.ID<128992, sameAns<11,noOpinions<15,less<3,Total.Time>=120,Total.Time<=5000,rep_user==FALSE)

# Weed out any remaining duplicates

df_cut <- df1
df_cut$Record.ID <- NULL
df1 <- cbind(df1,duplicated(df_cut))
colnames(df1)[[78]] <- "dup"
df1 <- df1[df1$dup==FALSE,]
df1$dup <- NULL

#Remove entries in which users declared themselves ineligible to vote (for reasons other than age) and where they declared a probably spurious age
df1 <- df1[is.na(df1$SupQ_6)|df1$SupQ_6==1|df1$SupQ_6==29,]
detach("package:dplyr", unload=TRUE)
library(plyr)
df1$SupQ_9 <- mapvalues(df1$SupQ_9, from = c(NA,98), to = c(0,0))
england <- df1[(df1$SupQ_9>=1920|df1$SupQ_9==0),]

#Remove entries in which no value has been assigned to age, education, gender or previous vote

england <- england[!is.na(england$SupQ_2)&!is.na(england$SupQ_8)&!is.na(england$SupQ_9)&!is.na(england$SupQ_10)&!is.na(england$SupQ_11),]

england <- rename(england, c(Ans_1="q1", Ans_2="q2", Ans_3="q3", Ans_4="q4", Ans_5="q5", Ans_6="q6", Ans_7="q7", Ans_8="q8", Ans_9="q9", Ans_10="q10", Ans_11="q11", Ans_12="q12", Ans_13="q13", Ans_14="q14", Ans_15="q15", Ans_16="q16", Ans_17="q17", Ans_18="q18", Ans_19="q19", Ans_20="q20", Ans_21="q21", Ans_22="q22", Ans_23="q23", Ans_24="q24", Ans_25="q25", Ans_26="q26", Ans_27="q27", Ans_28="q28", Ans_29="q29", Ans_30="q30"))
# Recode item variables from 1...5 to 4 ...0
attach(england)
qus <- data.frame(q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,q11,q12,q13,q14,q15,q16,q17,q18,q19,q20,q21,q22,q23,q24,q25,q26,q27,q28,q29,q30)
qus  <- lapply(qus, FUN = function(x) recode(x, "1=4;2=3;3=2;4=1;5=0;6=NA"))
sqs <- data.frame(SupQ_1,SupQ_2,SupQ_3,SupQ_4,SupQ_5,SupQ_6,SupQ_7,SupQ_8,SupQ_9,SupQ_10,SupQ_11,SupQ_12,SupQ_13,SupQ_14,SupQ_15,SupQ_16,SupQ_17,SupQ_18,SupQ_19,SupQ_20)
england <- data.frame(england[,1:23],qus,sqs)

# add reversed items
qurevs <- lapply(qus, FUN = function(x) recode(x, "0=4;1=3;2=2;3=1;4=0;NA=NA"))
qurevs <- rename(qurevs, c(q1="q1_rev", q2="q2_rev", q3="q3_rev", q4="q4_rev", q5="q5_rev", q6="q6_rev", q7="q7_rev", q8="q8_rev", q9="q9_rev", q10="q10_rev", q11="q11_rev", q12="q12_rev", q13="q13_rev", q14="q14_rev", q15="q15_rev", q16="q16_rev", q17="q17_rev", q18="q18_rev", q19="q19_rev", q20="q20_rev", q21="q21_rev", q22="q22_rev", q23="q23_rev", q24="q24_rev", q25="q25_rev", q26="q26_rev", q27="q27_rev", q28="q28_rev", q29="q29_rev", q30="q30_rev"))
england <- data.frame(england,qurevs)

england$age <- 2013 - england$SupQ_9
england$age <- recode(england$age,"2013=999")

# Consider undecideds
england$vot <- recode(england$SupQ_2,"11='CONS'; 12='LAB'; 13='LD'; 14='GRN'; 15='UKIP'; 16=NA; 17=NA; 18='Undec'; 19=NA; 20=NA")

england$sex <- recode(england$SupQ_8,"95='Female'; 96='Male'; 97=NA")
england$pol <- recode(england$SupQ_11,"197='interest'; 198='interest'; 199='LowInterest'; 200='LowInterest'; 201=NA")

england$joint[(england$SupQ_10==195|england$SupQ_10==196)&england$age<54&england$age>17] <- "degreeLow"
england$joint[(england$SupQ_10>190&england$SupQ_10<195)&england$age<54&england$age>17] <- "noDegreeLow"
england$joint[(england$SupQ_10==195|england$SupQ_10==196)&england$age>53&england$age!=999] <- "degreeHigh"
england$joint[(england$SupQ_10>190&england$SupQ_10<195)&england$age>53&england$age!=999] <- "noDegreeHigh"

england <- england[!is.na(england$vot)&!is.na(england$sex)&!is.na(england$pol)&!is.na(england$joint),]
