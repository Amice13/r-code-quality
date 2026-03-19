#####Study 2: ANES vote data from GPT-3 and the ANES#####
##Last updated: 19 December 2022

####Libraries and working directories####
library(plyr)
library(psych)
library(correlation)
library(here)
library(rstudioapi)
library(xtable)
options(scipen=9)

###To set the path for loading the data:

stub <- function() {}
thisPath <- function() {
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  if (length(grep("^-f$", cmdArgs)) > 0) {
    # R console option
    normalizePath(dirname(cmdArgs[grep("^-f", cmdArgs) + 1]))[1]
  } else if (length(grep("^--file=", cmdArgs)) > 0) {
    # Rscript/R console option
    scriptPath <- normalizePath(dirname(sub("^--file=", "", cmdArgs[grep("^--file=", cmdArgs)])))[1]
  } else if (Sys.getenv("RSTUDIO") == "1") {
    if (rstudioapi::isAvailable(version_needed=NULL,child_ok=FALSE)) {
      # RStudio interactive
      dirname(rstudioapi::getActiveDocumentContext()$path)
    } else if (is.null(knitr::current_input(dir = TRUE)) == FALSE) {
      # Knit
      knitr::current_input(dir = TRUE)
    } else {
      # R markdown on RStudio
      getwd()
    }
  } else if (is.null(attr(stub, "srcref")) == FALSE) {
    # 'source'd via R console
    dirname(normalizePath(attr(attr(stub, "srcref"), "srcfile")$filename))
  } else {
    stop("Cannot find file path")
  }
}

#Now to import the data; the following files contain the survey data from the ANES (with original variable names from the ANES) along with corresponding predictions from GPT-3 for each respondent

####2012####
anes2012=read.csv(here::here(thisPath(),"Study2_Data","full_results_2012_2.csv"))
names(anes2012)

#Give intuitive names
anes2012=plyr::rename(anes2012, replace=c("presvote2012_x" = "anes2012_vote", "dem_raceeth_x"="race", 
                            "discuss_disc"="discuss_politics", "libcpre_self"="ideology",
                            "pid_x"="party_id", "relig_church"="attend_church",
                            "dem_age_r_x"="age", "gender_respondent_x"="gender", "paprofile_interestpolit"="interest",
                            "patriot_flag"="flag", "sample_stfips"="state"))

#Create a binary vote for the anes2012
anes2012$vote_romney[anes2012$anes2012_vote==2]=1
anes2012$vote_romney[anes2012$anes2012_vote==1]=0

#We can also turn GPT-3's predictions into something binary
anes2012$gpt3_vote[anes2012$p_romney>0.5]=1
anes2012$gpt3_vote[anes2012$p_romney<0.5]=0

#Make all negative values on the anes2012 variables missing
anes2012$race[anes2012$race<0]=NA
anes2012$discuss_politics[anes2012$discuss_politics<0]=NA
anes2012$ideology[anes2012$ideology<0]=NA
anes2012$party_id[anes2012$party_id<0]=NA
anes2012$attend_church[anes2012$attend_church<0]=NA
anes2012$age[anes2012$age<0]=NA
anes2012$gender[anes2012$gender<0]=NA
anes2012$interest[anes2012$interest<0]=NA
anes2012$flag[anes2012$flag<0]=NA
anes2012$state[anes2012$state<0]=NA

####Let's restrict just to those for whom we have voting data
anes20122=anes2012[!is.na(anes2012$vote_romney),]


####2016####
anes=read.csv(here::here(thisPath(),"Study2_Data","full_results_2016_2.csv"))
names(anes)

#Give intuitive names
anes=plyr::rename(anes, replace=c("V162062x" = "anes_vote", "V161310x"="race", 
                            "V162174"="discuss_politics", "V161126"="ideology",
                            "V161158x"="party_id", "V161244"="attend_church",
                            "V161267"="age", "V161342"="gender", "V162256"="interest",
                            "V162125x"="flag", "V161010d"="state"))

#Create a binary vote for trump or clinton version for the ANES
anes$vote_trump[anes$anes_vote==2]=1
anes$vote_trump[anes$anes_vote==1]=0
mean(anes$p_trump[!is.na(anes$vote_trump)], na.rm=T)

#We can also turn GPT-3's predictions into something binary
anes$gpt3_vote[anes$p_trump>0.5]=1
anes$gpt3_vote[anes$p_trump<0.5]=0

#Make all negative values on the anes variables missing
anes$race[anes$race<0]=NA
anes$discuss_politics[anes$discuss_politics<0]=NA
anes$ideology[anes$ideology<0]=NA
anes$party_id[anes$party_id<0]=NA
anes$attend_church[anes$attend_church<0]=NA
anes$age[anes$age<0]=NA
anes$gender[anes$gender<0]=NA
anes$interest[anes$interest<0]=NA
anes$flag[anes$flag<0]=NA
anes$state[anes$state<0]=NA

##Restrict just to those for whom we have voting data
anes2=anes[!is.na(anes$vote_trump),]

####2020####
anes2020=read.csv(here::here(thisPath(),"Study2_Data","full_results_2020_2.csv"))
names(anes2020)

#Give intuitive names
anes2020=plyr::rename(anes2020, replace=c("V202110x" = "anes2020_vote", "V201549x"="race", 
                                    "V202022"="discuss_politics", "V201200"="ideology",
                                    "V201231x"="party_id", "V201452"="attend_church",
                                    "V201507x"="age", "V201600"="gender", "V202406"="interest"))
#Create a binary vote for the anes2020
anes2020$vote_trump[anes2020$anes2020_vote==2]=1
anes2020$vote_trump[anes2020$anes2020_vote==1]=0

#We can also turn GPT-3's predictions into something binary
anes2020$gpt3_vote[anes2020$p_trump>0.5]=1
anes2020$gpt3_vote[anes2020$p_trump<0.5]=0

####Let's restrict just to those for whom we have voting data
anes20202=anes2020[!is.na(anes2020$vote_trump),]

####Results in the order presented in the text####
####Overall voting####
##2012
prop.test(x=c(mean(anes2012$vote_romney, na.rm=T)*nrow(anes2012[!is.na(anes2012$vote_romney),]), mean(anes2012$p_romney, na.rm=T)*nrow(anes2012[!is.na(anes2012$vote_romney),])), n=c(nrow(anes2012[!is.na(anes2012$vote_romney),]), nrow(anes2012[!is.na(anes2012$vote_romney),])))
##2016
prop.test(x=c(mean(anes$vote_trump, na.rm=T)*nrow(anes[!is.na(anes$vote_trump),]), mean(anes$p_trump[!is.na(anes$vote_trump)], na.rm=T)*nrow(anes[!is.na(anes$vote_trump),])), n=c(nrow(anes[!is.na(anes$vote_trump),]), nrow(anes[!is.na(anes$vote_trump),])))
##2020
prop.test(x=c(mean(anes2020$vote_trump, na.rm=T)*nrow(anes2020[!is.na(anes2020$vote_trump),]), mean(anes2020$p_trump[!is.na(anes2020$vote_trump)], na.rm=T)*nrow(anes2020[!is.na(anes2020$vote_trump),])), n=c(nrow(anes2020[!is.na(anes2020$vote_trump),]), nrow(anes2020[!is.na(anes2020$vote_trump),])))

####Empty table that we will fill in for Table 1####
table1=matrix(nrow=23, ncol=6, byrow=T,
            dimnames = list(c("Whole sample", "Men", "Women",
                              "Strong partisans", "Weak partisans",
                              "Leaners", "Independents", "Conservatives",
                              "Moderates", "Liberals", "Whites", "Blacks",
                              "Hispanics", "Attends church", "Doesn't attend church",
                              "High interest in politics", "Low interest in politics",
                              "Discusses politics", "Doesn't discuss politics",
                              "18 to 30 years old", "31 to 45 years old",
                              "46 to 60 years old", "Over 60"), 
                            c("2012 Tetra", "2012 Prop. Agree", "2016 Tetra",
                              "2016 Prop. Agree", "2020 Tetra.", "2020 Prop. Agree")))

#####2012 detailed results for Table 1####
#Whole sample
temp=anes20122
table1[1,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
table1[1,2]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)

#Men
temp=anes20122[anes20122$gender==1,]
table1[2,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
table1[2,2]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)

#Women
temp=anes20122[anes20122$gender==2,]
table1[3,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
table1[3,2]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)

#Strong partisans
temp=anes20122[anes20122$party_id==7|anes20122$party_id==1,]
table1[4,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
table1[4,2]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)

#Weak partisans
temp=anes20122[anes20122$party_id==6|anes20122$party_id==2,]
table1[5,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
table1[5,2]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)

#Leaners
temp=anes20122[anes20122$party_id==5|anes20122$party_id==3,]
table1[6,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
table1[6,2]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)

#Independents
temp=anes20122[anes20122$party_id==4,]
table1[7,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
table1[7,2]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)

#Conservatives
temp=anes20122[anes20122$ideology>4,]
table1[8,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
table1[8,2]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)

#Moderates
temp=anes20122[anes20122$ideology==4,]
table1[9,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
table1[9,2]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)

#Liberals
temp=anes20122[anes20122$ideology<4,]
table1[10,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
table1[10,2]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)

#Whites
temp=anes20122[anes20122$race==1,]
table1[11,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
table1[11,2]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)

#Blacks
temp=anes20122[anes20122$race==2,]
table1[12,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
table1[12,2]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)

#Hispanics
temp=anes20122[anes20122$race==5,]
table1[13,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
table1[13,2]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)

#Attend church
temp=anes20122[anes20122$attend_church==1,]
table1[14,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
table1[14,2]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)

#Doesn't attend church
temp=anes20122[anes20122$attend_church==2,]
table1[15,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
table1[15,2]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)

#Interest in politics (high)
temp=anes20122[anes20122$interest==1,]
table1[16,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
table1[16,2]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)

#Interest in politics (low)
temp=anes20122[anes20122$interest==4,]
table1[17,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
table1[17,2]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)

#Discuss politics
temp=anes20122[anes20122$discuss_politics==1,]
table1[18,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
table1[18,2]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)

#Discuss politics (not)
temp=anes20122[anes20122$discuss_politics==2,]
table1[19,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
table1[19,2]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)

#Age (18-30)
temp=anes20122[anes20122$age>=18&anes20122$age<31,]
table1[20,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
table1[20,2]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)

#Age (31-45)
temp=anes20122[anes20122$age>=31&anes20122$age<46,]
table1[21,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
table1[21,2]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)

#Age (46-60)
temp=anes20122[anes20122$age>=46&anes20122$age<61,]
table1[22,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
table1[22,2]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)

#Age (Over 60)
temp=anes20122[anes20122$age>=61,]
table1[23,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
table1[23,2]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)

####2016 results for Table 1####
#Whole sample
temp=anes2
table1[1,3]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[1,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Men
temp=anes2[anes2$gender==1,]
table1[2,3]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[2,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Women
temp=anes2[anes2$gender==2,]
table1[3,3]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[3,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Strong partisans
temp=anes2[anes2$party_id==7|anes2$party_id==1,]
table1[4,3]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[4,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Weak partisans
temp=anes2[anes2$party_id==6|anes2$party_id==2,]
table1[5,3]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[5,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Leaners
temp=anes2[anes2$party_id==5|anes2$party_id==3,]
table1[6,3]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[6,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Independents
temp=anes2[anes2$party_id==4,]
table1[7,3]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[7,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Conservatives
temp=anes2[anes2$ideology>4,]
table1[8,3]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[8,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Moderates
temp=anes2[anes2$ideology==4,]
table1[9,3]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[9,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Liberals
temp=anes2[anes2$ideology<4,]
table1[10,3]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[10,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Whites
temp=anes2[anes2$race==1,]
table1[11,3]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[11,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Blacks
temp=anes2[anes2$race==2,]
table1[12,3]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[12,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Hispanics
temp=anes2[anes2$race==5,]
table1[13,3]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[13,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Attend church
temp=anes2[anes2$attend_church==1,]
table1[14,3]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[14,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Doesn't attend church
temp=anes2[anes2$attend_church==2,]
table1[15,3]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[15,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Interest in politics (high)
temp=anes2[anes2$interest==1,]
table1[16,3]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[16,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Interest in politics (low)
temp=anes2[anes2$interest==4,]
table1[17,3]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[17,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Discuss politics
temp=anes2[anes2$discuss_politics==1,]
table1[18,3]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[18,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Discuss politics (not)
temp=anes2[anes2$discuss_politics==2,]
table1[19,3]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[19,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Age (18-30)
temp=anes2[anes2$age>=18&anes2$age<31,]
table1[20,3]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[20,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Age (31-45)
temp=anes2[anes2$age>=31&anes2$age<46,]
table1[21,3]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[21,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Age (46-60)
temp=anes2[anes2$age>=46&anes2$age<61,]
table1[22,3]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[22,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Age (Over 60)
temp=anes2[anes2$age>=61,]
table1[23,3]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[23,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

####2020 results for Table 1####
#Whole sample
temp=anes20202
table1[1,5]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[1,6]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Men
temp=anes20202[anes20202$gender==1,]
table1[2,5]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[2,6]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Women
temp=anes20202[anes20202$gender==2,]
table1[3,5]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[3,6]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Strong partisans
temp=anes20202[anes20202$party_id==7|anes20202$party_id==1,]
table1[4,5]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[4,6]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Weak partisans
temp=anes20202[anes20202$party_id==6|anes20202$party_id==2,]
table1[5,5]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[5,6]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Leaners
temp=anes20202[anes20202$party_id==5|anes20202$party_id==3,]
table1[6,5]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[6,6]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Independents
temp=anes20202[anes20202$party_id==4,]
table1[7,5]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[7,6]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Conservatives
temp=anes20202[anes20202$ideology>4,]
table1[8,5]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[8,6]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Moderates
temp=anes20202[anes20202$ideology==4,]
table1[9,5]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[9,6]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Liberals
temp=anes20202[anes20202$ideology<4,]
table1[10,5]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[10,6]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Whites
temp=anes20202[anes20202$race==1,]
table1[11,5]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[11,6]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Blacks
temp=anes20202[anes20202$race==2,]
table1[12,5]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[12,6]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Hispanics
temp=anes20202[anes20202$race==5,]
table1[13,5]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[13,6]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Attend church
temp=anes20202[anes20202$attend_church==1,]
table1[14,5]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[14,6]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Doesn't attend church
temp=anes20202[anes20202$attend_church==2,]
table1[15,5]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[15,6]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Interest in politics (high)
temp=anes20202[anes20202$interest==1,]
table1[16,5]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[16,6]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Interest in politics (low)
temp=anes20202[anes20202$interest==4,]
table1[17,5]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[17,6]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Discuss politics
temp=anes20202[anes20202$discuss_politics==1,]
table1[18,5]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[18,6]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Discuss politics (not)
temp=anes20202[anes20202$discuss_politics==2,]
table1[19,5]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[19,6]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Age (18-30)
temp=anes20202[anes20202$age>=18&anes20202$age<31,]
table1[20,5]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[20,6]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Age (31-45)
temp=anes20202[anes20202$age>=31&anes20202$age<46,]
table1[21,5]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[21,6]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Age (46-60)
temp=anes20202[anes20202$age>=46&anes20202$age<61,]
table1[22,5]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[22,6]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Age (Over 60)
temp=anes20202[anes20202$age>=61,]
table1[23,5]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
table1[23,6]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

####Table 1, finalized####
table1_out <- xtable(table1)
print.xtable(table1_out, type="latex", size="\\tiny", file=here::here(thisPath(),"Tables","Main_Table1.tex"))

####Online Appendix####
##These commands replicate the numbers presented in the Online Appendix.

###Appendix Table 7: 2012 results####
##Empty table
tablea.7=matrix(nrow=29, ncol=4, byrow=T,
              dimnames = list(c("Whole sample", "Men", "Women",
                                "Strong partisans", "Weak partisans",
                                "Leaners", "Independents", "Conservatives",
                                "Moderates", "Liberals", "Whites", "Blacks",
                                "Hispanics", "Attends church", "Doesn't attend church",
                                "High interest in politics", "Low interest in politics",
                                "Discusses politics", "Doesn't discuss politics",
                                "18 to 30 years old", "31 to 45 years old",
                                "46 to 60 years old", "Over 60", "Californians",
                                "Texans", "New Yorkers", "Ohioans", "Arizonans", "Wisconsins"), 
                              c("Tetrachoric correlation", "Cohen's Kappa", "ICC", "Prop. agreement")))

#Whole sample
temp=anes20122
tablea.7[1,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
tablea.7[1,4]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)
tablea.7[1,2]=round(cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))$kappa, digits=2)
tablea.7[1,3]=round(min(ICC(cbind(temp$vote_romney, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Men
temp=anes20122[anes20122$gender==1,]
tablea.7[2,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
tablea.7[2,4]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)
tablea.7[2,2]=round(cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))$kappa, digits=2)
tablea.7[2,3]=round(min(ICC(cbind(temp$vote_romney, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Women
temp=anes20122[anes20122$gender==2,]
tablea.7[3,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
tablea.7[3,4]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)
tablea.7[3,2]=round(cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))$kappa, digits=2)
tablea.7[3,3]=round(min(ICC(cbind(temp$vote_romney, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Strong partisans
temp=anes20122[anes20122$party_id==7|anes20122$party_id==1,]
tablea.7[4,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
tablea.7[4,4]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)
tablea.7[4,2]=round(cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))$kappa, digits=2)
tablea.7[4,3]=round(min(ICC(cbind(temp$vote_romney, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Weak partisans
temp=anes20122[anes20122$party_id==6|anes20122$party_id==2,]
tablea.7[5,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
tablea.7[5,4]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)
tablea.7[5,2]=round(cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))$kappa, digits=2)
tablea.7[5,3]=round(min(ICC(cbind(temp$vote_romney, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Leaners
temp=anes20122[anes20122$party_id==5|anes20122$party_id==3,]
tablea.7[6,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
tablea.7[6,4]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)
tablea.7[6,2]=round(cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))$kappa, digits=2)
tablea.7[6,3]=round(min(ICC(cbind(temp$vote_romney, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Independents
temp=anes20122[anes20122$party_id==4,]
tablea.7[7,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
tablea.7[7,4]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)
tablea.7[7,2]=round(cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))$kappa, digits=2)
tablea.7[7,3]=round(min(ICC(cbind(temp$vote_romney, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Conservatives
temp=anes20122[anes20122$ideology>4,]
tablea.7[8,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
tablea.7[8,4]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)
tablea.7[8,2]=round(cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))$kappa, digits=2)
tablea.7[8,3]=round(min(ICC(cbind(temp$vote_romney, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Moderates
temp=anes20122[anes20122$ideology==4,]
tablea.7[9,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
tablea.7[9,4]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)
tablea.7[9,2]=round(cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))$kappa, digits=2)
tablea.7[9,3]=round(min(ICC(cbind(temp$vote_romney, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Liberals
temp=anes20122[anes20122$ideology<4,]
tablea.7[10,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
tablea.7[10,4]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)
tablea.7[10,2]=round(cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))$kappa, digits=2)
tablea.7[10,3]=round(min(ICC(cbind(temp$vote_romney, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Whites
temp=anes20122[anes20122$race==1,]
tablea.7[11,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
tablea.7[11,4]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)
tablea.7[11,2]=round(cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))$kappa, digits=2)
tablea.7[11,3]=round(min(ICC(cbind(temp$vote_romney, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Blacks
temp=anes20122[anes20122$race==2,]
tablea.7[12,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
tablea.7[12,4]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)
tablea.7[12,2]=round(cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))$kappa, digits=2)
tablea.7[12,3]=round(min(ICC(cbind(temp$vote_romney, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Hispanics
temp=anes20122[anes20122$race==5,]
tablea.7[13,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
tablea.7[13,4]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)
tablea.7[13,2]=round(cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))$kappa, digits=2)
tablea.7[13,3]=round(min(ICC(cbind(temp$vote_romney, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Attend church
temp=anes20122[anes20122$attend_church==1,]
tablea.7[14,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
tablea.7[14,4]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)
tablea.7[14,2]=round(cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))$kappa, digits=2)
tablea.7[14,3]=round(min(ICC(cbind(temp$vote_romney, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Doesn't attend church
temp=anes20122[anes20122$attend_church==2,]
tablea.7[15,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
tablea.7[15,4]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)
tablea.7[15,2]=round(cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))$kappa, digits=2)
tablea.7[15,3]=round(min(ICC(cbind(temp$vote_romney, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Interest in politics (high)
temp=anes20122[anes20122$interest==1,]
tablea.7[16,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
tablea.7[16,4]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)
tablea.7[16,2]=round(cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))$kappa, digits=2)
tablea.7[16,3]=round(min(ICC(cbind(temp$vote_romney, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Interest in politics (low)
temp=anes20122[anes20122$interest==4,]
tablea.7[17,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
tablea.7[17,4]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)
tablea.7[17,2]=round(cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))$kappa, digits=2)
tablea.7[17,3]=round(min(ICC(cbind(temp$vote_romney, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Discuss politics
temp=anes20122[anes20122$discuss_politics==1,]
tablea.7[18,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
tablea.7[18,4]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)
tablea.7[18,2]=round(cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))$kappa, digits=2)
tablea.7[18,3]=round(min(ICC(cbind(temp$vote_romney, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Discuss politics (not)
temp=anes20122[anes20122$discuss_politics==2,]
tablea.7[19,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
tablea.7[19,4]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)
tablea.7[19,2]=round(cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))$kappa, digits=2)
tablea.7[19,3]=round(min(ICC(cbind(temp$vote_romney, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Age (18-30)
temp=anes20122[anes20122$age>=18&anes20122$age<31,]
tablea.7[20,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
tablea.7[20,4]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)
tablea.7[20,2]=round(cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))$kappa, digits=2)
tablea.7[20,3]=round(min(ICC(cbind(temp$vote_romney, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Age (31-45)
temp=anes20122[anes20122$age>=31&anes20122$age<46,]
tablea.7[21,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
tablea.7[21,4]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)
tablea.7[21,2]=round(cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))$kappa, digits=2)
tablea.7[21,3]=round(min(ICC(cbind(temp$vote_romney, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Age (46-60)
temp=anes20122[anes20122$age>=46&anes20122$age<61,]
tablea.7[22,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
tablea.7[22,4]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)
tablea.7[22,2]=round(cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))$kappa, digits=2)
tablea.7[22,3]=round(min(ICC(cbind(temp$vote_romney, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Age (Over 60)
temp=anes20122[anes20122$age>=61,]
tablea.7[23,1]=round(tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))$rho[2], digits=2)
tablea.7[23,4]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)
tablea.7[23,2]=round(cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))$kappa, digits=2)
tablea.7[23,3]=round(min(ICC(cbind(temp$vote_romney, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#States:
#CA
temp=anes20122[anes20122$state==6,]
tablea.7[24,1]=round(tetrachoric(table(temp$vote_romney, temp$gpt3_vote))$rho[1], digits=2)
tablea.7[24,2]=round(cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))$kappa, digits=2)
tablea.7[24,3]=round(min(ICC(cbind(temp$vote_romney, temp$gpt3_vote))$results$ICC[4:6]), digits=2)
tablea.7[24,4]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)

#Texas
temp=anes20122[anes20122$state==48,]
tablea.7[25,1]=round(tetrachoric(table(temp$vote_romney, temp$gpt3_vote))$rho[1], digits=2)
tablea.7[25,2]=round(cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))$kappa, digits=2)
tablea.7[25,3]=round(min(ICC(cbind(temp$vote_romney, temp$gpt3_vote))$results$ICC[4:6]), digits=2)
tablea.7[25,4]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)

#NY
temp=anes20122[anes20122$state==36,]
tablea.7[26,1]=round(tetrachoric(table(temp$vote_romney, temp$gpt3_vote))$rho[1], digits=2) 
tablea.7[26,2]=round(cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))$kappa, digits=2)
tablea.7[26,3]=round(min(ICC(cbind(temp$vote_romney, temp$gpt3_vote))$results$ICC[4:6]), digits=2)
tablea.7[26,4]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)

#Ohio
temp=anes20122[anes20122$state==39,]
tablea.7[27,1]=round(tetrachoric(table(temp$vote_romney, temp$gpt3_vote))$rho[1], digits=2)
tablea.7[27,2]=round(cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))$kappa, digits=2)
tablea.7[27,3]=round(min(ICC(cbind(temp$vote_romney, temp$gpt3_vote))$results$ICC[4:6]), digits=2)
tablea.7[27,4]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)

#Arizona
temp=anes20122[anes20122$state==4,]
tablea.7[28,1]=round(tetrachoric(table(temp$vote_romney, temp$gpt3_vote))$rho[1], digits=2)
tablea.7[28,2]=round(cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))$kappa, digits=2)
tablea.7[28,3]=round(min(ICC(cbind(temp$vote_romney, temp$gpt3_vote))$results$ICC[4:6]), digits=2)
tablea.7[28,4]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)

#WI
temp=anes20122[anes20122$state==55,]
tablea.7[29,1]=round(tetrachoric(table(temp$vote_romney, temp$gpt3_vote))$rho[1], digits=2)
tablea.7[29,2]=round(cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))$kappa, digits=2)
tablea.7[29,3]=round(min(ICC(cbind(temp$vote_romney, temp$gpt3_vote))$results$ICC[4:6]), digits=2)
tablea.7[29,4]=round(prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1], digits=2)

###Completed table###
table7a_out <- xtable(tablea.7)
print.xtable(table7a_out, type="latex", size="\\small", file=here::here(thisPath(),"Tables","Appendix_Table7.tex"))

####Appendix Table 8: 2016 results####
##Empty table:
tablea.8=matrix(nrow=29, ncol=4, byrow=T,
                dimnames = list(c("Whole sample", "Men", "Women",
                                  "Strong partisans", "Weak partisans",
                                  "Leaners", "Independents", "Conservatives",
                                  "Moderates", "Liberals", "Whites", "Blacks",
                                  "Hispanics", "Attends church", "Doesn't attend church",
                                  "High interest in politics", "Low interest in politics",
                                  "Discusses politics", "Doesn't discuss politics",
                                  "18 to 30 years old", "31 to 45 years old",
                                  "46 to 60 years old", "Over 60", "Californians",
                                  "Texans", "New Yorkers", "Ohioans", "Arizonans", "Wisconsins"), 
                                c("Tetrachoric correlation", "Cohen's Kappa", "ICC", "Prop. agreement")))
#Whole sample
temp=anes2
tablea.8[1,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.8[1,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.8[1,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.8[1,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Men
temp=anes2[anes2$gender==1,]
tablea.8[2,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.8[2,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.8[2,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.8[2,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Women
temp=anes2[anes2$gender==2,]
tablea.8[3,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.8[3,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.8[3,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.8[3,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Strong partisans
temp=anes2[anes2$party_id==7|anes2$party_id==1,]
tablea.8[4,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.8[4,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.8[4,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.8[4,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Weak partisans
temp=anes2[anes2$party_id==6|anes2$party_id==2,]
tablea.8[5,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.8[5,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.8[5,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.8[5,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Leaners
temp=anes2[anes2$party_id==5|anes2$party_id==3,]
tablea.8[6,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.8[6,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.8[6,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.8[6,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Independents
temp=anes2[anes2$party_id==4,]
tablea.8[7,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.8[7,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.8[7,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.8[7,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Conservatives
temp=anes2[anes2$ideology>4,]
tablea.8[8,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.8[8,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.8[8,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.8[8,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Moderates
temp=anes2[anes2$ideology==4,]
tablea.8[9,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.8[9,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.8[9,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.8[9,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Liberals
temp=anes2[anes2$ideology<4,]
tablea.8[10,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.8[10,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.8[10,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.8[10,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Whites
temp=anes2[anes2$race==1,]
tablea.8[11,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.8[11,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.8[11,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.8[11,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Blacks
temp=anes2[anes2$race==2,]
tablea.8[12,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.8[12,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.8[12,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.8[12,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Hispanics
temp=anes2[anes2$race==5,]
tablea.8[13,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.8[13,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.8[13,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.8[13,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Attend church
temp=anes2[anes2$attend_church==1,]
tablea.8[14,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.8[14,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.8[14,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.8[14,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Doesn't attend church
temp=anes2[anes2$attend_church==2,]
tablea.8[15,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.8[15,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.8[15,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.8[15,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Interest in politics (high)
temp=anes2[anes2$interest==1,]
tablea.8[16,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.8[16,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.8[16,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.8[16,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Interest in politics (low)
temp=anes2[anes2$interest==4,]
tablea.8[17,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.8[17,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.8[17,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.8[17,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Discuss politics
temp=anes2[anes2$discuss_politics==1,]
tablea.8[18,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.8[18,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.8[18,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.8[18,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Discuss politics (not)
temp=anes2[anes2$discuss_politics==2,]
tablea.8[19,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.8[19,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.8[19,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.8[19,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Age (18-30)
temp=anes2[anes2$age>=18&anes2$age<31,]
tablea.8[20,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.8[20,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.8[20,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.8[20,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Age (31-45)
temp=anes2[anes2$age>=31&anes2$age<46,]
tablea.8[21,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.8[21,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.8[21,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.8[21,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Age (46-60)
temp=anes2[anes2$age>=46&anes2$age<61,]
tablea.8[22,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.8[22,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.8[22,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.8[22,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Age (Over 60)
temp=anes2[anes2$age>=61,]
tablea.8[23,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.8[23,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.8[23,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.8[23,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#States:
#CA
temp=anes2[anes2$state==6,]
tablea.8[24,1]=round(tetrachoric(table(temp$vote_trump, temp$gpt3_vote))$rho[1], digits=2)
tablea.8[24,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.8[24,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)
tablea.8[24,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Texas
temp=anes2[anes2$state==48,]
tablea.8[25,1]=round(tetrachoric(table(temp$vote_trump, temp$gpt3_vote))$rho[1], digits=2)
tablea.8[25,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.8[25,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)
tablea.8[25,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#NY
temp=anes2[anes2$state==36,]
tablea.8[26,1]=round(tetrachoric(table(temp$vote_trump, temp$gpt3_vote))$rho[1], digits=2) 
tablea.8[26,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.8[26,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)
tablea.8[26,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Ohio
temp=anes2[anes2$state==39,]
tablea.8[27,1]=round(tetrachoric(table(temp$vote_trump, temp$gpt3_vote))$rho[1], digits=2)
tablea.8[27,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.8[27,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)
tablea.8[27,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Arizona
temp=anes2[anes2$state==4,]
tablea.8[28,1]=round(tetrachoric(table(temp$vote_trump, temp$gpt3_vote))$rho[1], digits=2)
tablea.8[28,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.8[28,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)
tablea.8[28,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#WI
temp=anes2[anes2$state==55,]
tablea.8[29,1]=round(tetrachoric(table(temp$vote_trump, temp$gpt3_vote))$rho[1], digits=2)
tablea.8[29,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.8[29,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)
tablea.8[29,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)

#Final table:
table8a_out <- xtable(tablea.8)
print.xtable(table8a_out, type="latex", size="\\small", file=here::here(thisPath(),"Tables","Appendix_Table8.tex"))

####Appendix Table 9: 2020 results####
#Empty table
tablea.9=matrix(nrow=23, ncol=4, byrow=T,
                dimnames = list(c("Whole sample", "Men", "Women",
                                  "Strong partisans", "Weak partisans",
                                  "Leaners", "Independents", "Conservatives",
                                  "Moderates", "Liberals", "Whites", "Blacks",
                                  "Hispanics", "Attends church", "Doesn't attend church",
                                  "High interest in politics", "Low interest in politics",
                                  "Discusses politics", "Doesn't discuss politics",
                                  "18 to 30 years old", "31 to 45 years old",
                                  "46 to 60 years old", "Over 60"), 
                                c("Tetrachoric correlation", "Cohen's Kappa", "ICC", "Prop. agreement")))
#Whole sample
temp=anes20202
tablea.9[1,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.9[1,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.9[1,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.9[1,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Men
temp=anes20202[anes20202$gender==1,]
tablea.9[2,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.9[2,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.9[2,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.9[2,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Women
temp=anes20202[anes20202$gender==2,]
tablea.9[3,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.9[3,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.9[3,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.9[3,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Strong partisans
temp=anes20202[anes20202$party_id==7|anes20202$party_id==1,]
tablea.9[4,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.9[4,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.9[4,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.9[4,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Weak partisans
temp=anes20202[anes20202$party_id==6|anes20202$party_id==2,]
tablea.9[5,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.9[5,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.9[5,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.9[5,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Leaners
temp=anes20202[anes20202$party_id==5|anes20202$party_id==3,]
tablea.9[6,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.9[6,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.9[6,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.9[6,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Independents
temp=anes20202[anes20202$party_id==4,]
tablea.9[7,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.9[7,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.9[7,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.9[7,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Conservatives
temp=anes20202[anes20202$ideology>4,]
tablea.9[8,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.9[8,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.9[8,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.9[8,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Moderates
temp=anes20202[anes20202$ideology==4,]
tablea.9[9,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.9[9,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.9[9,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.9[9,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Liberals
temp=anes20202[anes20202$ideology<4,]
tablea.9[10,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.9[10,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.9[10,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.9[10,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Whites
temp=anes20202[anes20202$race==1,]
tablea.9[11,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.9[11,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.9[11,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.9[11,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Blacks
temp=anes20202[anes20202$race==2,]
tablea.9[12,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.9[12,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.9[12,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.9[12,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Hispanics
temp=anes20202[anes20202$race==5,]
tablea.9[13,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.9[13,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.9[13,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.9[13,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Attend church
temp=anes20202[anes20202$attend_church==1,]
tablea.9[14,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.9[14,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.9[14,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.9[14,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Doesn't attend church
temp=anes20202[anes20202$attend_church==2,]
tablea.9[15,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.9[15,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.9[15,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.9[15,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Interest in politics (high)
temp=anes20202[anes20202$interest==1,]
tablea.9[16,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.9[16,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.9[16,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.9[16,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Interest in politics (low)
temp=anes20202[anes20202$interest==4,]
tablea.9[17,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.9[17,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.9[17,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.9[17,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Discuss politics
temp=anes20202[anes20202$discuss_politics==1,]
tablea.9[18,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.9[18,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.9[18,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.9[18,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Discuss politics (not)
temp=anes20202[anes20202$discuss_politics==2,]
tablea.9[19,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.9[19,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.9[19,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.9[19,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Age (18-30)
temp=anes20202[anes20202$age>=18&anes20202$age<31,]
tablea.9[20,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.9[20,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.9[20,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.9[20,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Age (31-45)
temp=anes20202[anes20202$age>=31&anes20202$age<46,]
tablea.9[21,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.9[21,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.9[21,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.9[21,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Age (46-60)
temp=anes20202[anes20202$age>=46&anes20202$age<61,]
tablea.9[22,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.9[22,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.9[22,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.9[22,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#Age (Over 60)
temp=anes20202[anes20202$age>=61,]
tablea.9[23,1]=round(tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))$rho[2], digits=2)
tablea.9[23,4]=round(prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1], digits=2)
tablea.9[23,2]=round(cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))$kappa, digits=2)
tablea.9[23,3]=round(min(ICC(cbind(temp$vote_trump, temp$gpt3_vote))$results$ICC[4:6]), digits=2)

#finished table
table9a_out <- xtable(tablea.9)
print.xtable(table9a_out, type="latex", size="\\small", file=here::here(thisPath(),"Tables","Appendix_Table9.tex"))