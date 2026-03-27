#This means you need to run the script tittled "Summer Project Final ANES R-Script.R"

#IN this script you will assagin variables that indicate datasets
#Then you stack the data, but only variables that are included in our model

##creating categorical variable that ID's each dataset
table(mydata$mode)
mydata2$mode<-"MTurk"
table(mydata2$mode)
mydata$dataset<-NA
mydata2$dataset<-NA
mydata$dataset[mydata$mode=="1. FTF (face-to-face) mode"] <-1
mydata$dataset[mydata$mode=="2. Internet mode"]<-2
mydata2$dataset<-3
table(mydata$dataset)
table(mydata2$dataset)

##Creating 3 sets of dummies
##creating dummy for MTurk and Int; Mturk=1 internet=0 ftf=NA
mydata$turkint<-NA
mydata2$turkint<-NA
mydata$turkint[mydata$mode=="1. FTF (face-to-face) mode"] <-NA
mydata$turkint[mydata$mode=="2. Internet mode"]<-0
mydata2$turkint<-1
table(mydata$turkint)
table(mydata2$turkint)


##creating dummy for MTurk and FTF; Mturk=1 internet=NA ftf=0
mydata$turkftf<-NA
mydata2$turkftf<-NA
mydata$turkftf[mydata$mode=="1. FTF (face-to-face) mode"] <-0
mydata$turkftf[mydata$mode=="2. Internet mode"]<-NA
mydata2$turkftf<-1
table(mydata$turkftf)
table(mydata2$turkftf)

##Creating 3 sets of dummies
##creating dummy for int and FTF; Mturk=NA internet=1 ftf=0
mydata$intftf<-NA
mydata2$intftf<-NA
mydata$intftf[mydata$mode=="1. FTF (face-to-face) mode"] <-0
mydata$intftf[mydata$mode=="2. Internet mode"]<-1
mydata2$intftf<-NA
table(mydata$intftf)
table(mydata2$intftf)
table(mydata2$mode)
     
##Taking variables we need from each dataset
##Taking needed variables from ##2012 data
subset1<-c("PID","ideo","econ","social","extraversion","agreeableness","conscientiousness","emostab","openness",
           "escale" ,"mtscale","rrscale","ascale","age","edu","male","inc","religiosity","mode","turkint","turkftf","intftf" ) ##"mydata$","mydata"
head(subset1)
newdata <- mydata2[subset1]

subset2<-c("PID","ideology","econ","social","extraversion","agreeableness","conscientiousness","emostab","openness",
           "escale" ,"mtscale","rrscale","ascale","age","edu","male","inc","religiosity","mode","turkint","turkftf","intftf" ) ##"mydata$","mydata"
head(subset2)
newdata2 <- mydata[subset2]

##renaming old variables
names(newdata2) <- c("PID","ideo","econ","social","extraversion","agreeableness","conscientiousness","emostab","openness",
                     "escale" ,"mtscale","rrscale","ascale","age","edu","male","inc","religiosity","mode","turkint","turkftf","intftf")

##merging data
mydata3<-rbind(newdata, newdata2)

##exporting data to excel
write.xlsx(mydata2, "C:/Users/Ryan/Downloads/stackedmturkanes.xlsx")
#exporting data as dta
write.dta(mydata2, "C:/Users/Ryan/Downloads/stackedmturkanes2.dta")

rm(list=ls())
##open data
mydata3 <- read.dta("C:/Users/Ryan/Downloads/stackedmturkanes2.dta", 1)

#turning mode into stored values
table(mydata3$mode)
mydata3$ftf<-NA
mydata3$ftf[mydata3$mode=="1. FTF (face-to-face) mode"]<-1
table(mydata3$ftf)

mydata3$int<-NA
mydata3$int[mydata3$mode=="2. Internet mode"]<-1
mydata3$int

mydata3$mturk<-NA
mydata3$mturk[mydata3$mode=="MTurk"]<-1
mydata3$mturk

ftf1=mydata3$ftf ==1
int1=mydata3$int ==1
mturk=mydata3$mturk==1

#running regressions
#ideo + big 5
#mturk
reg1001<-lm(mydata3$ideo~ mydata3$extraversion +mydata3$agreeableness+mydata3$conscientiousness+mydata3$emostab+
             mydata3$openness+mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity, subset=mturk)
#ftf
reg3101<-lm(mydata3$ideo~ mydata3$extraversion +mydata3$agreeableness+mydata3$conscientiousness+mydata3$emostab+
              mydata3$openness+mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity, subset=ftf1)
#Internet
reg401<-lm(mydata3$ideo~ mydata3$extraversion +mydata3$agreeableness+mydata3$conscientiousness+mydata3$emostab+
              mydata3$openness+mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity, subset=int1)
#PID + big 5
#Mturk
reg1<-lm(mydata3$PID~ mydata3$extraversion +mydata3$agreeableness+mydata3$conscientiousness+mydata3$emostab+
           mydata3$openness+mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity, subset=mturk)
#FTF
reg11<-lm(mydata3$PID~ mydata3$extraversion +mydata3$agreeableness+mydata3$conscientiousness+mydata3$emostab+
            mydata3$openness+mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity, subset=ftf1)

#Internet
reg21<-lm(mydata3$PID~ mydata3$extraversion +mydata3$agreeableness+mydata3$conscientiousness+mydata3$emostab+
            mydata3$openness+mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity, subset=int1)


stargazer(reg3101,reg401, reg1001,reg11,reg21,reg1, type="latex", title="Regression Results",
          covariate.labels=c( "Extraversion", "Agreeableness","Conscientiousness",
            "Emotional Stability", "Openness","Age","Education", "Male","Income","Religiosity"), align=TRUE, 
          star.cutoffs = c(0.05, 0.01, 0.001))

#Regressing values
#Ideo(7-point sclae) = values + demographics
#mturk
reg7<-lm(mydata3$ideo~ mydata3$escale +mydata3$mtscale+mydata3$rrscale+mydata3$ascale+
           mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity, subset=mturk)

#FTF
reg71<-lm(mydata3$ideo~ mydata3$escale +mydata3$mtscale+mydata3$rrscale+mydata3$ascale+
            mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity, subset=ftf1)

#Internet
reg81<-lm(mydata3$ideo~ mydata3$escale +mydata3$mtscale+mydata3$rrscale+mydata3$ascale+
            mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity, subset=int1)
#PID(7-point scale)=values + demographics
#Mturk
reg5<-lm(mydata3$PID~ mydata3$escale +mydata3$mtscale+mydata3$rrscale+mydata3$ascale+
           mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity, subset=mturk)

#FTF
reg51<-lm(mydata3$PID~ mydata3$escale +mydata3$mtscale+mydata3$rrscale+mydata3$ascale+
            mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity, subset=ftf1)
#Internet
reg61<-lm(mydata3$PID~ mydata3$escale +mydata3$mtscale+mydata3$rrscale+mydata3$ascale+
            mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity, subset=int1)

stargazer(reg71,reg81, reg7,reg51,reg61,reg5, type="latex", title="Regression Results",
          covariate.labels=c( "Equalitarianism","Moral Traditionalism","Racial Resentment",
                              "Authoritarianism","Age","Education", "Male","Income","Religiosity"), align=TRUE, 
          star.cutoffs = c(0.05, 0.01, 0.001))

#Econ index = TIPI + demographics 
#MTurk
reg10<-lm(mydata3$econ~ mydata3$extraversion +mydata3$agreeableness+mydata3$conscientiousness+mydata3$emostab+
            mydata3$openness+mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity, subset=mturk)
#ANES TS 2012 FTF
reg101<-lm(mydata3$econ~ mydata3$extraversion +mydata3$agreeableness+mydata3$conscientiousness+mydata3$emostab+
             mydata3$openness+mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity, subset=ftf1)
#ANES TS 2012 INT
reg121<-lm(mydata3$econ~ mydata3$extraversion +mydata3$agreeableness+mydata3$conscientiousness+mydata3$emostab+
             mydata3$openness+mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity, subset=int1)


#social index = TIPI + demographics
#MTurk
reg1111<-lm(mydata3$social~ mydata3$extraversion +mydata3$agreeableness+mydata3$conscientiousness+mydata3$emostab+
              mydata3$openness+mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity, subset=mturk)
#ANES TS 2012 FTF
reg111<-lm(mydata3$social~ mydata3$extraversion +mydata3$agreeableness+mydata3$conscientiousness+mydata3$emostab+
             mydata3$openness+mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity, subset=ftf1)
#ANES TS 2012 INT
reg131<-lm(mydata3$social~ mydata3$extraversion +mydata3$agreeableness+mydata3$conscientiousness+mydata3$emostab+
             mydata3$openness+mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity, subset=int1)

stargazer(reg111,reg131, reg1111,reg101,reg121,reg10, type="latex", title="Regression Results",
          covariate.labels=c( "Extraversion", "Agreeableness","Conscientiousness",
                              "Emotional Stability", "Openness","Age","Education", "Male","Income","Religiosity"), align=TRUE, 
          star.cutoffs = c(0.05, 0.01, 0.001))

#Econ index=values + demographics
#MTurk
reg12<-lm(mydata3$econ~ mydata3$escale +mydata3$mtscale+mydata3$rrscale+mydata3$ascale+
            mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity, subset=mturk)

# FTF
reg141<-lm(mydata3$econ~ mydata3$escale +mydata3$mtscale+mydata3$rrscale+mydata3$ascale+
             mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity, subset=ftf1)

# INT
reg161<-lm(mydata3$econ~ mydata3$escale +mydata3$mtscale+mydata3$rrscale+mydata3$ascale+
             mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity, subset=int1)

#social index = values + demographics
#MTurk
reg13<-lm(mydata3$social~ mydata3$escale +mydata3$mtscale+mydata3$rrscale+mydata3$ascale+
            mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity, subset=mturk)
# FTF
reg151<-lm(mydata3$social~ mydata3$escale +mydata3$mtscale+mydata3$rrscale+mydata3$ascale+
             mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity, subset=ftf1)
# INT
reg171<-lm(mydata3$social~ mydata3$escale +mydata3$mtscale+mydata3$rrscale+mydata3$ascale+
             mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity, subset=int1)

stargazer(reg151,reg171, reg13,reg141,reg161,reg12, type="text", title="Regression Results",
          covariate.labels=c( "Equalitarianism","Moral Traditionalism","Racial Resentment",
                              "Authoritarianism","Age","Education", "Male","Income","Religiosity"), align=TRUE, 
          star.cutoffs = c(0.05, 0.01, 0.001))

stargazer(reg52, type="text", title="Scales", 
          covariate.labels=c( "Extraversion", "Agreeableness","Conscientiousness",
                              "Emotional StabilitY","Openness","turkint","Age","Education", "Male","Income","Religiosity",
                              "Extraversion*turkint", "Agreeableness*turkint","Conscientiousness*turkint",
                              "Emotional Stability*turkint", "Openness*turkint"), align=TRUE, 
          star.cutoffs = c(0.05, 0.01, 0.001))

##regressions with interractions
#mturk and int dummy turkint
#ideo + big 5
#mturk
reg52<-lm(mydata3$ideo~ mydata3$extraversion +mydata3$agreeableness+mydata3$conscientiousness+mydata3$emostab+
              mydata3$openness+mydata3$turkint+mydata3$extraversion*mydata3$turkint +mydata3$agreeableness*mydata3$turkint+
            mydata3$conscientiousness*mydata3$turkint+mydata3$emostab*mydata3$turkint+
            mydata3$openness*mydata3$turkint+mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity)

summary(reg52)
#ftf
reg53<-lm(mydata3$ideo~ mydata3$extraversion +mydata3$agreeableness+mydata3$conscientiousness+mydata3$emostab+
              mydata3$openness+mydata3$turkftf+mydata3$extraversion*mydata3$turkftf +mydata3$agreeableness*mydata3$turkftf+mydata3$conscientiousness*mydata3$turkftf+
            mydata3$emostab*mydata3$turkftf+
            mydata3$openness*mydata3$turkftf+mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity)
summary(reg53)
#Internet
reg54<-lm(mydata3$ideo~ mydata3$extraversion +mydata3$agreeableness+mydata3$conscientiousness+mydata3$emostab+
             mydata3$openness+mydata3$intftf+mydata3$extraversion*mydata3$intftf +mydata3$agreeableness*mydata3$intftf+
            mydata3$conscientiousness*mydata3$intftf+mydata3$emostab*mydata3$intftf+
            mydata3$openness*mydata3$intftf+mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity)
summary(reg54)

#PID + big 5
#Mturk
reg55<-lm(mydata3$PID~ mydata3$extraversion +mydata3$agreeableness+mydata3$conscientiousness+mydata3$emostab+
            mydata3$openness+mydata3$turkint+mydata3$extraversion*mydata3$turkint +mydata3$agreeableness*mydata3$turkint+
            mydata3$conscientiousness*mydata3$turkint+mydata3$emostab*mydata3$turkint+
            mydata3$openness*mydata3$turkint+mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity)
summary(reg55)
#FTF
reg56<-lm(mydata3$PID~ mydata3$extraversion +mydata3$agreeableness+mydata3$conscientiousness+mydata3$emostab+
            mydata3$openness+mydata3$turkftf+mydata3$extraversion*mydata3$turkftf +mydata3$agreeableness*mydata3$turkftf+mydata3$conscientiousness*mydata3$turkftf+
            mydata3$emostab*mydata3$turkftf+
            mydata3$openness*mydata3$turkftf+mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity)
summary(reg56)

#Internet
reg57<-lm(mydata3$PID~ mydata3$extraversion +mydata3$agreeableness+mydata3$conscientiousness+mydata3$emostab+
            mydata3$openness+mydata3$intftf+mydata3$extraversion*mydata3$intftf +mydata3$agreeableness*mydata3$intftf+
            mydata3$conscientiousness*mydata3$intftf+mydata3$emostab*mydata3$intftf+
            mydata3$openness*mydata3$intftf+mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity)
summary(reg57)

stargazer(reg53, reg54, reg52, reg56,reg57,reg55,type="text", title="Big 5 with interactions",  align=TRUE, 
          star.cutoffs = c(0.05, 0.01, 0.001),omit.stat=c("adj.rsq","f","ser"))

#Regressing values with interactions
#Ideo(7-point sclae) = values + demographics
#mturk
reg58<-lm(mydata3$ideo~ mydata3$escale +mydata3$mtscale+mydata3$rrscale+mydata3$ascale+
           mydata3$turkint+mydata3$escale*mydata3$turkint +mydata3$mtscale*mydata3$turkint+mydata3$rrscale*mydata3$turkint+mydata3$ascale*mydata3$turkint+
           mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity)

summary(reg58)
#FTF
reg59<-lm(mydata3$ideo~ mydata3$escale +mydata3$mtscale+mydata3$rrscale+mydata3$ascale+
            mydata3$turkftf+mydata3$escale*mydata3$turkftf +mydata3$mtscale*mydata3$turkftf+mydata3$rrscale*mydata3$turkftf+mydata3$ascale*mydata3$turkftf+
            mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity)
summary(reg59)

#Internet
reg60<-lm(mydata3$ideo~ mydata3$escale +mydata3$mtscale+mydata3$rrscale+mydata3$ascale+
            mydata3$intftf+mydata3$escale*mydata3$intftf +mydata3$mtscale*mydata3$intftf+mydata3$rrscale*mydata3$intftf+mydata3$ascale*mydata3$intftf+
            mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity)
summary(reg60)
#PID(7-point scale)=values + demographics
#Mturk
reg62<-lm(mydata3$PID~ mydata3$escale +mydata3$mtscale+mydata3$rrscale+mydata3$ascale+
           mydata3$turkint+mydata3$escale*mydata3$turkint +mydata3$mtscale*mydata3$turkint+mydata3$rrscale*mydata3$turkint+mydata3$ascale*mydata3$turkint+
           mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity)

summary(reg62)
#FTF
reg63<-lm(mydata3$PID~ mydata3$escale +mydata3$mtscale+mydata3$rrscale+mydata3$ascale+
            mydata3$turkftf+mydata3$escale*mydata3$turkftf +mydata3$mtscale*mydata3$turkftf+mydata3$rrscale*mydata3$turkftf+mydata3$ascale*mydata3$turkftf+
            mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity)
summary(reg63)

#Internet
reg64<-lm(mydata3$PID~ mydata3$escale +mydata3$mtscale+mydata3$rrscale+mydata3$ascale+
            mydata3$intftf+mydata3$escale*mydata3$intftf +mydata3$mtscale*mydata3$intftf+mydata3$rrscale*mydata3$intftf+mydata3$ascale*mydata3$intftf+
            mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity)
summary(reg64)
stargazer(reg59, reg60, reg58, reg63,reg64,reg62,type="text", title="Big 5 with interactions",  align=TRUE, 
          star.cutoffs = c(0.05, 0.01, 0.001),omit.stat=c("adj.rsq","f","ser"))

#social + big 5
#mturk
reg52<-lm(mydata3$social~ mydata3$extraversion +mydata3$agreeableness+mydata3$conscientiousness+mydata3$emostab+
            mydata3$openness+mydata3$turkint+mydata3$extraversion*mydata3$turkint +mydata3$agreeableness*mydata3$turkint+
            mydata3$conscientiousness*mydata3$turkint+mydata3$emostab*mydata3$turkint+
            mydata3$openness*mydata3$turkint+mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity)

summary(reg52)
#ftf
reg53<-lm(mydata3$social~ mydata3$extraversion +mydata3$agreeableness+mydata3$conscientiousness+mydata3$emostab+
            mydata3$openness+mydata3$turkftf+mydata3$extraversion*mydata3$turkftf +mydata3$agreeableness*mydata3$turkftf+mydata3$conscientiousness*mydata3$turkftf+
            mydata3$emostab*mydata3$turkftf+
            mydata3$openness*mydata3$turkftf+mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity)
summary(reg53)
#Internet
reg54<-lm(mydata3$social~ mydata3$extraversion +mydata3$agreeableness+mydata3$conscientiousness+mydata3$emostab+
            mydata3$openness+mydata3$intftf+mydata3$extraversion*mydata3$intftf +mydata3$agreeableness*mydata3$intftf+
            mydata3$conscientiousness*mydata3$intftf+mydata3$emostab*mydata3$intftf+
            mydata3$openness*mydata3$intftf+mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity)
summary(reg54)

#econ + big 5
#Mturk
reg55<-lm(mydata3$econ~ mydata3$extraversion +mydata3$agreeableness+mydata3$conscientiousness+mydata3$emostab+
            mydata3$openness+mydata3$turkint+mydata3$extraversion*mydata3$turkint +mydata3$agreeableness*mydata3$turkint+
            mydata3$conscientiousness*mydata3$turkint+mydata3$emostab*mydata3$turkint+
            mydata3$openness*mydata3$turkint+mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity)
summary(reg55)
#FTF
reg56<-lm(mydata3$econ~ mydata3$extraversion +mydata3$agreeableness+mydata3$conscientiousness+mydata3$emostab+
            mydata3$openness+mydata3$turkftf+mydata3$extraversion*mydata3$turkftf +mydata3$agreeableness*mydata3$turkftf+mydata3$conscientiousness*mydata3$turkftf+
            mydata3$emostab*mydata3$turkftf+
            mydata3$openness*mydata3$turkftf+mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity)
summary(reg56)

#Internet
reg57<-lm(mydata3$econ~ mydata3$extraversion +mydata3$agreeableness+mydata3$conscientiousness+mydata3$emostab+
            mydata3$openness+mydata3$intftf+mydata3$extraversion*mydata3$intftf +mydata3$agreeableness*mydata3$intftf+
            mydata3$conscientiousness*mydata3$intftf+mydata3$emostab*mydata3$intftf+
            mydata3$openness*mydata3$intftf+mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity)
summary(reg57)

stargazer(reg53, reg54, reg52, reg56,reg57,reg55,type="text", title="Big 5 with interactions",  align=TRUE, 
          star.cutoffs = c(0.05, 0.01, 0.001),omit.stat=c("adj.rsq","f","ser"))

#social = values + demographics
#mturk
reg58<-lm(mydata3$social~ mydata3$escale +mydata3$mtscale+mydata3$rrscale+mydata3$ascale+
            mydata3$turkint+mydata3$escale*mydata3$turkint +mydata3$mtscale*mydata3$turkint+mydata3$rrscale*mydata3$turkint+mydata3$ascale*mydata3$turkint+
            mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity)

summary(reg58)
#FTF
reg59<-lm(mydata3$social~ mydata3$escale +mydata3$mtscale+mydata3$rrscale+mydata3$ascale+
            mydata3$turkftf+mydata3$escale*mydata3$turkftf +mydata3$mtscale*mydata3$turkftf+mydata3$rrscale*mydata3$turkftf+mydata3$ascale*mydata3$turkftf+
            mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity)

summary(reg59)
#Internet
reg60<-lm(mydata3$social~ mydata3$escale +mydata3$mtscale+mydata3$rrscale+mydata3$ascale+
            mydata3$intftf+mydata3$escale*mydata3$intftf +mydata3$mtscale*mydata3$intftf+mydata3$rrscale*mydata3$intftf+mydata3$ascale*mydata3$intftf+
            mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity)
summary(reg60)
#econ=values + demographics
#Mturk
reg62<-lm(mydata3$econ~ mydata3$escale +mydata3$mtscale+mydata3$rrscale+mydata3$ascale+
            mydata3$turkint+mydata3$escale*mydata3$turkint +mydata3$mtscale*mydata3$turkint+mydata3$rrscale*mydata3$turkint+mydata3$ascale*mydata3$turkint+
            mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity)

summary(reg62)
#FTF
reg63<-lm(mydata3$econ~ mydata3$escale +mydata3$mtscale+mydata3$rrscale+mydata3$ascale+
            mydata3$turkftf+mydata3$escale*mydata3$turkftf +mydata3$mtscale*mydata3$turkftf+mydata3$rrscale*mydata3$turkftf+mydata3$ascale*mydata3$turkftf+
            mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity)
summary(reg63)
#Internet
reg64<-lm(mydata3$econ~ mydata3$escale +mydata3$mtscale+mydata3$rrscale+mydata3$ascale+
            mydata3$intftf+mydata3$escale*mydata3$intftf +mydata3$mtscale*mydata3$intftf+mydata3$rrscale*mydata3$intftf+mydata3$ascale*mydata3$intftf+
            mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity)
summary(reg64)

stargazer(reg59, reg60, reg58, reg63,reg64,reg62,type="text", title="Big 5 with interactions",  align=TRUE, 
          star.cutoffs = c(0.05, 0.01, 0.001),omit.stat=c("adj.rsq","f","ser"))

#demographics

table(mydata3[ftf1,]$male)
table(mydata3[int1,]$male)
table(mydata3[mturk,]$male)

mean(mydata3[ftf1,]$age, na.rm = TRUE)
mean(mydata3[int1,]$age, na.rm = TRUE)
mean(mydata3[mturk,]$age, na.rm = TRUE)

table(mydata3[ftf1,]$edu)
table(mydata3[int1,]$edu)
table(mydata3[mturk,]$edu)

mean(mydata3[ftf1,]$inc, na.rm = TRUE)
mean(mydata3[int1,]$inc, na.rm = TRUE)
mean(mydata3[mturk,]$inc, na.rm = TRUE)

mean(mydata3[ftf1,]$religiosity, na.rm = TRUE)
mean(mydata3[int1,]$religiosity, na.rm = TRUE)
mean(mydata3[mturk,]$religiosity, na.rm = TRUE)

table(mydata3[ftf1,]$PID)
table(mydata3[int1,]$PID)
table(mydata3[mturk,]$PID)

mean(mydata3[ftf1,]$econ, na.rm = TRUE)
mean(mydata3[int1,]$econ, na.rm = TRUE)
mean(mydata3[mturk,]$econ, na.rm = TRUE)

mean(mydata3[ftf1,]$social, na.rm = TRUE)
mean(mydata3[int1,]$social, na.rm = TRUE)
mean(mydata3[mturk,]$social, na.rm = TRUE)

#t-tests and correlations
R333=mydata3[mturk,]$PID ==7
R222=mydata3[mturk,]$PID ==6
R111=mydata3[mturk,]$PID ==5
I111=mydata3[mturk,]$PID ==4
D111=mydata3[mturk,]$PID ==3
D222=mydata3[mturk,]$PID ==2
D333=mydata3[mturk,]$PID ==1


C333=mydata3[mturk,]$ideo ==7
C222=mydata3[mturk,]$ideo ==6
C111=mydata3[mturk,]$ideo ==5
M111=mydata3[mturk,]$ideo ==4
L111=mydata3[mturk,]$ideo ==3
L222=mydata3[mturk,]$ideo ==2
L333=mydata3[mturk,]$ideo ==1

R3=mydata3[ftf1,]$PID ==7
R2=mydata3[ftf1,]$PID ==6
R1=mydata3[ftf1,]$PID ==5
I1=mydata3[ftf1,]$PID ==4
D1=mydata3[ftf1,]$PID ==3
D2=mydata3[ftf1,]$PID ==2
D3=mydata3[ftf1,]$PID ==1


C3=mydata3[ftf1,]$ideo ==7
C2=mydata3[ftf1,]$ideo ==6
C1=mydata3[ftf1,]$ideo ==5
M1=mydata3[ftf1,]$ideo ==4
L1=mydata3[ftf1,]$ideo ==3
L2=mydata3[ftf1,]$ideo ==2
L3=mydata3[ftf1,]$ideo ==1

R33=mydata3[int1,]$PID ==7
R22=mydata3[int1,]$PID ==6
R11=mydata3[int1,]$PID ==5
I11=mydata3[int1,]$PID ==4
D11=mydata3[int1,]$PID ==3
D22=mydata3[int1,]$PID ==2
D33=mydata3[int1,]$PID ==1


C33=mydata3[int1,]$ideo ==7
C22=mydata3[int1,]$ideo ==6
C11=mydata3[int1,]$ideo ==5
M11=mydata3[int1,]$ideo ==4
L11=mydata3[int1,]$ideo ==3
L22=mydata3[int1,]$ideo ==2
L33=mydata3[int1,]$ideo ==1

#EXTRAVERSION
cor.test(mydata3[ftf1,]$extraversion, mydata3$ideo)
cor.test(mydata3[int1,]$extraversion, mydata3$ideo)
cor.test(mydata3[mturk,]$extraversion, mydata3$ideo)
cor.test(mydata3[ftf1,]$extraversion, mydata3$PID)
cor.test(mydata3[int1,]$extraversion, mydata3$PID)
cor.test(mydata3[mturk,]$extraversion, mydata3$PID)
t.test(mydata3[C3,]$extraversion,mydata3[C33,]$extraversion)
t.test(mydata3[C3,]$extraversion,mydata3[C333,]$extraversion)
t.test(mydata3[C33,]$extraversion,mydata3[C333,]$extraversion)
t.test(mydata3[C2,]$extraversion,mydata3[C22,]$extraversion)
t.test(mydata3[C2,]$extraversion,mydata3[C222,]$extraversion)
t.test(mydata3[C22,]$extraversion,mydata3[C222,]$extraversion)
t.test(mydata3[C1,]$extraversion,mydata3[C11,]$extraversion)
t.test(mydata3[C1,]$extraversion,mydata3[C111,]$extraversion)
t.test(mydata3[C11,]$extraversion,mydata3[C111,]$extraversion)
t.test(mydata3[M1,]$extraversion,mydata3[M11,]$extraversion)
t.test(mydata3[M1,]$extraversion,mydata3[M111,]$extraversion)
t.test(mydata3[M11,]$extraversion,mydata3[M111,]$extraversion)
t.test(mydata3[L3,]$extraversion,mydata3[L33,]$extraversion)
t.test(mydata3[L3,]$extraversion,mydata3[L333,]$extraversion)
t.test(mydata3[L33,]$extraversion,mydata3[L333,]$extraversion)
t.test(mydata3[L2,]$extraversion,mydata3[L22,]$extraversion)
t.test(mydata3[L2,]$extraversion,mydata3[L222,]$extraversion)
t.test(mydata3[L22,]$extraversion,mydata3[L222,]$extraversion)
t.test(mydata3[L1,]$extraversion,mydata3[L11,]$extraversion)
t.test(mydata3[L1,]$extraversion,mydata3[L111,]$extraversion)
t.test(mydata3[L11,]$extraversion,mydata3[L111,]$extraversion)

#mydata3$agreeableness
cor.test(mydata3[ftf1,]$agreeableness, mydata3$ideo)
cor.test(mydata3[int1,]$agreeableness, mydata3$ideo)
cor.test(mydata3[mturk,]$agreeableness, mydata3$ideo)
cor.test(mydata3[ftf1,]$agreeableness, mydata3$PID)
cor.test(mydata3[int1,]$agreeableness, mydata3$PID)
cor.test(mydata3[mturk,]$agreeableness, mydata3$PID)
t.test(mydata3[C3,]$agreeableness,mydata3[C33,]$agreeableness)
t.test(mydata3[C3,]$agreeableness,mydata3[C333,]$agreeableness)
t.test(mydata3[C33,]$agreeableness,mydata3[C333,]$agreeableness)
t.test(mydata3[C2,]$agreeableness,mydata3[C22,]$agreeableness)
t.test(mydata3[C2,]$agreeableness,mydata3[C222,]$agreeableness)
t.test(mydata3[C22,]$agreeableness,mydata3[C222,]$agreeableness)
t.test(mydata3[C1,]$agreeableness,mydata3[C11,]$agreeableness)
t.test(mydata3[C1,]$agreeableness,mydata3[C111,]$agreeableness)
t.test(mydata3[C11,]$agreeableness,mydata3[C111,]$agreeableness)
t.test(mydata3[M1,]$agreeableness,mydata3[M11,]$agreeableness)
t.test(mydata3[M1,]$agreeableness,mydata3[M111,]$agreeableness)
t.test(mydata3[M11,]$agreeableness,mydata3[M111,]$agreeableness)
t.test(mydata3[L3,]$agreeableness,mydata3[L33,]$agreeableness)
t.test(mydata3[L3,]$agreeableness,mydata3[L333,]$agreeableness)
t.test(mydata3[L33,]$agreeableness,mydata3[L333,]$agreeableness)
t.test(mydata3[L2,]$agreeableness,mydata3[L22,]$agreeableness)
t.test(mydata3[L2,]$agreeableness,mydata3[L222,]$agreeableness)
t.test(mydata3[L22,]$agreeableness,mydata3[L222,]$agreeableness)
t.test(mydata3[L1,]$agreeableness,mydata3[L11,]$agreeableness)
t.test(mydata3[L1,]$agreeableness,mydata3[L111,]$agreeableness)
t.test(mydata3[L11,]$agreeableness,mydata3[L111,]$agreeableness)

#mydata3$conscientiousness
cor.test(mydata3[ftf1,]$conscientiousness, mydata3$ideo)
cor.test(mydata3[int1,]$conscientiousness, mydata3$ideo)
cor.test(mydata3[mturk,]$conscientiousness, mydata3$ideo)
cor.test(mydata3[ftf1,]$conscientiousness, mydata3$PID)
cor.test(mydata3[int1,]$conscientiousness, mydata3$PID)
cor.test(mydata3[mturk,]$conscientiousness, mydata3$PID)
t.test(mydata3[C3,]$conscientiousness,mydata3[C33,]$conscientiousness)
t.test(mydata3[C3,]$conscientiousness,mydata3[C333,]$conscientiousness)
t.test(mydata3[C33,]$conscientiousness,mydata3[C333,]$conscientiousness)
t.test(mydata3[C2,]$conscientiousness,mydata3[C22,]$conscientiousness)
t.test(mydata3[C2,]$conscientiousness,mydata3[C222,]$conscientiousness)
t.test(mydata3[C22,]$conscientiousness,mydata3[C222,]$conscientiousness)
t.test(mydata3[C1,]$conscientiousness,mydata3[C11,]$conscientiousness)
t.test(mydata3[C1,]$conscientiousness,mydata3[C111,]$conscientiousness)
t.test(mydata3[C11,]$conscientiousness,mydata3[C111,]$conscientiousness)
t.test(mydata3[M1,]$conscientiousness,mydata3[M11,]$conscientiousness)
t.test(mydata3[M1,]$conscientiousness,mydata3[M111,]$conscientiousness)
t.test(mydata3[M11,]$conscientiousness,mydata3[M111,]$conscientiousness)
t.test(mydata3[L3,]$conscientiousness,mydata3[L33,]$conscientiousness)
t.test(mydata3[L3,]$conscientiousness,mydata3[L333,]$conscientiousness)
t.test(mydata3[L33,]$conscientiousness,mydata3[L333,]$conscientiousness)
t.test(mydata3[L2,]$conscientiousness,mydata3[L22,]$conscientiousness)
t.test(mydata3[L2,]$conscientiousness,mydata3[L222,]$conscientiousness)
t.test(mydata3[L22,]$conscientiousness,mydata3[L222,]$conscientiousness)
t.test(mydata3[L1,]$conscientiousness,mydata3[L11,]$conscientiousness)
t.test(mydata3[L1,]$conscientiousness,mydata3[L111,]$conscientiousness)
t.test(mydata3[L11,]$conscientiousness,mydata3[L111,]$conscientiousness)

#mydata3$emostab
cor.test(mydata3[ftf1,]$emostab, mydata3$ideo)
cor.test(mydata3[int1,]$emostab, mydata3$ideo)
cor.test(mydata3[mturk,]$emostab, mydata3$ideo)
cor.test(mydata3[ftf1,]$emostab, mydata3$PID)
cor.test(mydata3[int1,]$emostab, mydata3$PID)
cor.test(mydata3[mturk,]$emostab, mydata3$PID)
t.test(mydata3[C3,]$emostab,mydata3[C33,]$emostab)
t.test(mydata3[C3,]$emostab,mydata3[C333,]$emostab)
t.test(mydata3[C33,]$emostab,mydata3[C333,]$emostab)
t.test(mydata3[C2,]$emostab,mydata3[C22,]$emostab)
t.test(mydata3[C2,]$emostab,mydata3[C222,]$emostab)
t.test(mydata3[C22,]$emostab,mydata3[C222,]$emostab)
t.test(mydata3[C1,]$emostab,mydata3[C11,]$emostab)
t.test(mydata3[C1,]$emostab,mydata3[C111,]$emostab)
t.test(mydata3[C11,]$emostab,mydata3[C111,]$emostab)
t.test(mydata3[M1,]$emostab,mydata3[M11,]$emostab)
t.test(mydata3[M1,]$emostab,mydata3[M111,]$emostab)
t.test(mydata3[M11,]$emostab,mydata3[M111,]$emostab)
t.test(mydata3[L3,]$emostab,mydata3[L33,]$emostab)
t.test(mydata3[L3,]$emostab,mydata3[L333,]$emostab)
t.test(mydata3[L33,]$emostab,mydata3[L333,]$emostab)
t.test(mydata3[L2,]$emostab,mydata3[L22,]$emostab)
t.test(mydata3[L2,]$emostab,mydata3[L222,]$emostab)
t.test(mydata3[L22,]$emostab,mydata3[L222,]$emostab)
t.test(mydata3[L1,]$emostab,mydata3[L11,]$emostab)
t.test(mydata3[L1,]$emostab,mydata3[L111,]$emostab)
t.test(mydata3[L11,]$emostab,mydata3[L111,]$emostab)

#mydata3$openness
cor.test(mydata3[ftf1,]$openness, mydata3$ideo)
cor.test(mydata3[int1,]$openness, mydata3$ideo)
cor.test(mydata3[mturk,]$openness, mydata3$ideo)
cor.test(mydata3[ftf1,]$openness, mydata3$PID)
cor.test(mydata3[int1,]$openness, mydata3$PID)
cor.test(mydata3[mturk,]$openness, mydata3$PID)
t.test(mydata3[C3,]$openness,mydata3[C33,]$openness)
t.test(mydata3[C3,]$openness,mydata3[C333,]$openness)
t.test(mydata3[C33,]$openness,mydata3[C333,]$openness)
t.test(mydata3[C2,]$openness,mydata3[C22,]$openness)
t.test(mydata3[C2,]$openness,mydata3[C222,]$openness)
t.test(mydata3[C22,]$openness,mydata3[C222,]$openness)
t.test(mydata3[C1,]$openness,mydata3[C11,]$openness)
t.test(mydata3[C1,]$openness,mydata3[C111,]$openness)
t.test(mydata3[C11,]$openness,mydata3[C111,]$openness)
t.test(mydata3[M1,]$openness,mydata3[M11,]$openness)
t.test(mydata3[M1,]$openness,mydata3[M111,]$openness)
t.test(mydata3[M11,]$openness,mydata3[M111,]$openness)
t.test(mydata3[L3,]$openness,mydata3[L33,]$openness)
t.test(mydata3[L3,]$openness,mydata3[L333,]$openness)
t.test(mydata3[L33,]$openness,mydata3[L333,]$openness)
t.test(mydata3[L2,]$openness,mydata3[L22,]$openness)
t.test(mydata3[L2,]$openness,mydata3[L222,]$openness)
t.test(mydata3[L22,]$openness,mydata3[L222,]$openness)
t.test(mydata3[L1,]$openness,mydata3[L11,]$openness)
t.test(mydata3[L1,]$openness,mydata3[L111,]$openness)
t.test(mydata3[L11,]$openness,mydata3[L111,]$openness)

#mydata3$escale
cor.test(mydata3[ftf1,]$escale, mydata3$ideo)
cor.test(mydata3[int1,]$escale, mydata3$ideo)
cor.test(mydata3[mturk,]$escale, mydata3$ideo)
cor.test(mydata3[ftf1,]$escale, mydata3$PID)
cor.test(mydata3[int1,]$escale, mydata3$PID)
cor.test(mydata3[mturk,]$escale, mydata3$PID)
t.test(mydata3[C3,]$escale,mydata3[C33,]$escale)
t.test(mydata3[C3,]$escale,mydata3[C333,]$escale)
t.test(mydata3[C33,]$escale,mydata3[C333,]$escale)
t.test(mydata3[C2,]$escale,mydata3[C22,]$escale)
t.test(mydata3[C2,]$escale,mydata3[C222,]$escale)
t.test(mydata3[C22,]$escale,mydata3[C222,]$escale)
t.test(mydata3[C1,]$escale,mydata3[C11,]$escale)
t.test(mydata3[C1,]$escale,mydata3[C111,]$escale)
t.test(mydata3[C11,]$escale,mydata3[C111,]$escale)
t.test(mydata3[M1,]$escale,mydata3[M11,]$escale)
t.test(mydata3[M1,]$escale,mydata3[M111,]$escale)
t.test(mydata3[M11,]$escale,mydata3[M111,]$escale)
t.test(mydata3[L3,]$escale,mydata3[L33,]$escale)
t.test(mydata3[L3,]$escale,mydata3[L333,]$escale)
t.test(mydata3[L33,]$escale,mydata3[L333,]$escale)
t.test(mydata3[L2,]$escale,mydata3[L22,]$escale)
t.test(mydata3[L2,]$escale,mydata3[L222,]$escale)
t.test(mydata3[L22,]$escale,mydata3[L222,]$escale)
t.test(mydata3[L1,]$escale,mydata3[L11,]$escale)
t.test(mydata3[L1,]$escale,mydata3[L111,]$escale)
t.test(mydata3[L11,]$escale,mydata3[L111,]$escale)

#mydata3$mtscale
cor.test(mydata3[ftf1,]$mtscale, mydata3$ideo)
cor.test(mydata3[int1,]$mtscale, mydata3$ideo)
cor.test(mydata3[mturk,]$mtscale, mydata3$ideo)
cor.test(mydata3[ftf1,]$mtscale, mydata3$PID)
cor.test(mydata3[int1,]$mtscale, mydata3$PID)
cor.test(mydata3[mturk,]$mtscale, mydata3$PID)
t.test(mydata3[C3,]$mtscale,mydata3[C33,]$mtscale)
t.test(mydata3[C3,]$mtscale,mydata3[C333,]$mtscale)
t.test(mydata3[C33,]$mtscale,mydata3[C333,]$mtscale)
t.test(mydata3[C2,]$mtscale,mydata3[C22,]$mtscale)
t.test(mydata3[C2,]$mtscale,mydata3[C222,]$mtscale)
t.test(mydata3[C22,]$mtscale,mydata3[C222,]$mtscale)
t.test(mydata3[C1,]$mtscale,mydata3[C11,]$mtscale)
t.test(mydata3[C1,]$mtscale,mydata3[C111,]$mtscale)
t.test(mydata3[C11,]$mtscale,mydata3[C111,]$mtscale)
t.test(mydata3[M1,]$mtscale,mydata3[M11,]$mtscale)
t.test(mydata3[M1,]$mtscale,mydata3[M111,]$mtscale)
t.test(mydata3[M11,]$mtscale,mydata3[M111,]$mtscale)
t.test(mydata3[L3,]$mtscale,mydata3[L33,]$mtscale)
t.test(mydata3[L3,]$mtscale,mydata3[L333,]$mtscale)
t.test(mydata3[L33,]$mtscale,mydata3[L333,]$mtscale)
t.test(mydata3[L2,]$mtscale,mydata3[L22,]$mtscale)
t.test(mydata3[L2,]$mtscale,mydata3[L222,]$mtscale)
t.test(mydata3[L22,]$mtscale,mydata3[L222,]$mtscale)
t.test(mydata3[L1,]$mtscale,mydata3[L11,]$mtscale)
t.test(mydata3[L1,]$mtscale,mydata3[L111,]$mtscale)
t.test(mydata3[L11,]$mtscale,mydata3[L111,]$mtscale)

#mydata3$rrscale
cor.test(mydata3[ftf1,]$rrscale, mydata3$ideo)
cor.test(mydata3[int1,]$rrscale, mydata3$ideo)
cor.test(mydata3[mturk,]$rrscale, mydata3$ideo)
cor.test(mydata3[ftf1,]$rrscale, mydata3$PID)
cor.test(mydata3[int1,]$rrscale, mydata3$PID)
cor.test(mydata3[mturk,]$rrscale, mydata3$PID)
t.test(mydata3[C3,]$rrscale,mydata3[C33,]$rrscale)
t.test(mydata3[C3,]$rrscale,mydata3[C333,]$rrscale)
t.test(mydata3[C33,]$rrscale,mydata3[C333,]$rrscale)
t.test(mydata3[C2,]$rrscale,mydata3[C22,]$rrscale)
t.test(mydata3[C2,]$rrscale,mydata3[C222,]$rrscale)
t.test(mydata3[C22,]$rrscale,mydata3[C222,]$rrscale)
t.test(mydata3[C1,]$rrscale,mydata3[C11,]$rrscale)
t.test(mydata3[C1,]$rrscale,mydata3[C111,]$rrscale)
t.test(mydata3[C11,]$rrscale,mydata3[C111,]$rrscale)
t.test(mydata3[M1,]$rrscale,mydata3[M11,]$rrscale)
t.test(mydata3[M1,]$rrscale,mydata3[M111,]$rrscale)
t.test(mydata3[M11,]$rrscale,mydata3[M111,]$rrscale)
t.test(mydata3[L3,]$rrscale,mydata3[L33,]$rrscale)
t.test(mydata3[L3,]$rrscale,mydata3[L333,]$rrscale)
t.test(mydata3[L33,]$rrscale,mydata3[L333,]$rrscale)
t.test(mydata3[L2,]$rrscale,mydata3[L22,]$rrscale)
t.test(mydata3[L2,]$rrscale,mydata3[L222,]$rrscale)
t.test(mydata3[L22,]$rrscale,mydata3[L222,]$rrscale)
t.test(mydata3[L1,]$rrscale,mydata3[L11,]$rrscale)
t.test(mydata3[L1,]$rrscale,mydata3[L111,]$rrscale)
t.test(mydata3[L11,]$rrscale,mydata3[L111,]$rrscale)

#mydata3$ascale
cor.test(mydata3[ftf1,]$ascale, mydata3$ideo)
cor.test(mydata3[int1,]$ascale, mydata3$ideo)
cor.test(mydata3[mturk,]$ascale, mydata3$ideo)
cor.test(mydata3[ftf1,]$ascale, mydata3$PID)
cor.test(mydata3[int1,]$ascale, mydata3$PID)
cor.test(mydata3[mturk,]$ascale, mydata3$PID)
t.test(mydata3[C3,]$ascale,mydata3[C33,]$ascale)
t.test(mydata3[C3,]$ascale,mydata3[C333,]$ascale)
t.test(mydata3[C33,]$ascale,mydata3[C333,]$ascale)
t.test(mydata3[C2,]$ascale,mydata3[C22,]$ascale)
t.test(mydata3[C2,]$ascale,mydata3[C222,]$ascale)
t.test(mydata3[C22,]$ascale,mydata3[C222,]$ascale)
t.test(mydata3[C1,]$ascale,mydata3[C11,]$ascale)
t.test(mydata3[C1,]$ascale,mydata3[C111,]$ascale)
t.test(mydata3[C11,]$ascale,mydata3[C111,]$ascale)
t.test(mydata3[M1,]$ascale,mydata3[M11,]$ascale)
t.test(mydata3[M1,]$ascale,mydata3[M111,]$ascale)
t.test(mydata3[M11,]$ascale,mydata3[M111,]$ascale)
t.test(mydata3[L3,]$ascale,mydata3[L33,]$ascale)
t.test(mydata3[L3,]$ascale,mydata3[L333,]$ascale)
t.test(mydata3[L33,]$ascale,mydata3[L333,]$ascale)
t.test(mydata3[L2,]$ascale,mydata3[L22,]$ascale)
t.test(mydata3[L2,]$ascale,mydata3[L222,]$ascale)
t.test(mydata3[L22,]$ascale,mydata3[L222,]$ascale)
t.test(mydata3[L1,]$ascale,mydata3[L11,]$ascale)
t.test(mydata3[L1,]$ascale,mydata3[L111,]$ascale)
t.test(mydata3[L11,]$ascale,mydata3[L111,]$ascale)

#mydata3$econ
cor.test(mydata3[ftf1,]$econ, mydata3$ideo)
cor.test(mydata3[int1,]$econ, mydata3$ideo)
cor.test(mydata3[mturk,]$econ, mydata3$ideo)
cor.test(mydata3[ftf1,]$econ, mydata3$PID)
cor.test(mydata3[int1,]$econ, mydata3$PID)
cor.test(mydata3[mturk,]$econ, mydata3$PID)
t.test(mydata3[C3,]$econ,mydata3[C33,]$econ)
t.test(mydata3[C3,]$econ,mydata3[C333,]$econ)
t.test(mydata3[C33,]$econ,mydata3[C333,]$econ)
t.test(mydata3[C2,]$econ,mydata3[C22,]$econ)
t.test(mydata3[C2,]$econ,mydata3[C222,]$econ)
t.test(mydata3[C22,]$econ,mydata3[C222,]$econ)
t.test(mydata3[C1,]$econ,mydata3[C11,]$econ)
t.test(mydata3[C1,]$econ,mydata3[C111,]$econ)
t.test(mydata3[C11,]$econ,mydata3[C111,]$econ)
t.test(mydata3[M1,]$econ,mydata3[M11,]$econ)
t.test(mydata3[M1,]$econ,mydata3[M111,]$econ)
t.test(mydata3[M11,]$econ,mydata3[M111,]$econ)
t.test(mydata3[L3,]$econ,mydata3[L33,]$econ)
t.test(mydata3[L3,]$econ,mydata3[L333,]$econ)
t.test(mydata3[L33,]$econ,mydata3[L333,]$econ)
t.test(mydata3[L2,]$econ,mydata3[L22,]$econ)
t.test(mydata3[L2,]$econ,mydata3[L222,]$econ)
t.test(mydata3[L22,]$econ,mydata3[L222,]$econ)
t.test(mydata3[L1,]$econ,mydata3[L11,]$econ)
t.test(mydata3[L1,]$econ,mydata3[L111,]$econ)
t.test(mydata3[L11,]$econ,mydata3[L111,]$econ)

#mydata3$social
cor.test(mydata3[ftf1,]$social, mydata3$ideo)
cor.test(mydata3[int1,]$social, mydata3$ideo)
cor.test(mydata3[mturk,]$social, mydata3$ideo)
cor.test(mydata3[ftf1,]$social, mydata3$PID)
cor.test(mydata3[int1,]$social, mydata3$PID)
cor.test(mydata3[mturk,]$social, mydata3$PID)
t.test(mydata3[C3,]$social,mydata3[C33,]$social)
t.test(mydata3[C3,]$social,mydata3[C333,]$social)
t.test(mydata3[C33,]$social,mydata3[C333,]$social)
t.test(mydata3[C2,]$social,mydata3[C22,]$social)
t.test(mydata3[C2,]$social,mydata3[C222,]$social)
t.test(mydata3[C22,]$social,mydata3[C222,]$social)
t.test(mydata3[C1,]$social,mydata3[C11,]$social)
t.test(mydata3[C1,]$social,mydata3[C111,]$social)
t.test(mydata3[C11,]$social,mydata3[C111,]$social)
t.test(mydata3[M1,]$social,mydata3[M11,]$social)
t.test(mydata3[M1,]$social,mydata3[M111,]$social)
t.test(mydata3[M11,]$social,mydata3[M111,]$social)
t.test(mydata3[L3,]$social,mydata3[L33,]$social)
t.test(mydata3[L3,]$social,mydata3[L333,]$social)
t.test(mydata3[L33,]$social,mydata3[L333,]$social)
t.test(mydata3[L2,]$social,mydata3[L22,]$social)
t.test(mydata3[L2,]$social,mydata3[L222,]$social)
t.test(mydata3[L22,]$social,mydata3[L222,]$social)
t.test(mydata3[L1,]$social,mydata3[L11,]$social)
t.test(mydata3[L1,]$social,mydata3[L111,]$social)
t.test(mydata3[L11,]$social,mydata3[L111,]$social)

#looking at ascale by age
plot(mydata3$age, mydata3$ascale)
abline(lm(mydata3[mturk,]$ascale~mydata3[mturk,]$age), col="red")
abline(lm(mydata3[ftf1,]$ascale~mydata3[ftf1,]$age), col="blue")
abline(lm(mydata3[int1,]$ascale~mydata3[int1,]$age), col="green")


table(mydata3$mode)
mydata3<-mydata3[!(mydata3$mode=="NA"),]



ggplot(mydata3, aes(linetype=mode, age, ascale))+
  stat_smooth(method="lm", se=TRUE, color="black", level = 0.95)+ggtitle("OLS: Authoritarianism Scale ~ Age")+
  ylim(0,1) +
  xlab("Age (0 = Youngest - 1 = Oldest)")+ylab("Level of Authoritarianism (0 = Low - 1 = High)")+
  theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))

#making Value regressions by survey dummies
#making new mode dummies
table(mydata3$mode)
mydata3$anesftf<-NA
mydata3$anesftf[mydata3$mode=="1. FTF (face-to-face) mode"]<-1
mydata3$anesftf[mydata3$mode=="2. Internet mode"]<-0
mydata3$anesftf[mydata3$mode=="MTurk"]<-0
table(mydata3$anesftf)

mydata3$anesweb<-NA
mydata3$anesweb[mydata3$mode=="1. FTF (face-to-face) mode"]<-0
mydata3$anesweb[mydata3$mode=="2. Internet mode"]<-1
mydata3$anesweb[mydata3$mode=="MTurk"]<-0
table(mydata3$anesweb)

table(mydata3$ideo)
mydata3$liberals<-NA
mydata3$liberals[mydata3$ideo=="1"]<-1
mydata3$liberals[mydata3$ideo=="2"]<-1
mydata3$liberals[mydata3$ideo=="3"]<-1
liberals=mydata3$liberals ==1

reg585<-lm(mydata3$ascale~ mydata3$anesftf+mydata3$anesweb,subset=liberals)
reg586<-lm(mydata3$ascale~ mydata3$anesftf+mydata3$anesweb+mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity,subset=liberals)


reg587<-lm(mydata3$rrscale~ mydata3$anesftf+mydata3$anesweb,subset=liberals)
reg588<-lm(mydata3$rrscale~ mydata3$anesftf+mydata3$anesweb+mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity,subset=liberals)

reg589<-lm(mydata3$mtscale~ mydata3$anesftf+mydata3$anesweb,subset=liberals)
reg590<-lm(mydata3$mtscale~ mydata3$anesftf+mydata3$anesweb+mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity,subset=liberals)

reg591<-lm(mydata3$escale~ mydata3$anesftf+mydata3$anesweb,subset=liberals)
reg592<-lm(mydata3$escale~ mydata3$anesftf+mydata3$anesweb+mydata3$age+mydata3$edu+mydata3$male+mydata3$inc+mydata3$religiosity,subset=liberals)


stargazer(reg585,reg586,reg587,reg588,reg589,reg590,reg591,reg592, type="latex", title="Regression Results",
          covariate.labels=c( "ANES FTF","ANES Web","Age","Education", "Male","Income","Religiosity"), align=TRUE, 
          star.cutoffs = c(0.05, 0.01, 0.001))


#Getting values for Cohan's D table
se1 =matrix(c(std.error(mydata3[mturk,]$openness)*1.96,std.error(mydata3[mturk,]$conscientiousness)*1.96,std.error(mydata3[mturk,]$extraversion)*1.96,
             std.error(mydata3[mturk,]$agreeableness)*1.96,std.error(mydata3[mturk,]$emostab)*1.96,std.error(mydata3[mturk,]$mtscale)*1.96,
             std.error(mydata3[mturk,]$rrscale)*1.96,std.error(mydata3[mturk,]$escale)*1.96,std.error(mydata3[mturk,]$ascale)*1.96
             ))
Means1 =c(mean(mydata3[mturk,]$openness, na.rm=TRUE),mean(mydata3[mturk,]$conscientiousness, na.rm=TRUE),mean(mydata3[mturk,]$extraversion, na.rm=TRUE),
         mean(mydata3[mturk,]$agreeableness, na.rm=TRUE),mean(mydata3[mturk,]$emostab, na.rm=TRUE),mean(mydata3[mturk,]$mtscale, na.rm=TRUE),
         mean(mydata3[mturk,]$rrscale, na.rm=TRUE),mean(mydata3[mturk,]$escale, na.rm=TRUE),mean(mydata3[mturk,]$ascale, na.rm=TRUE)
         )
se1
Means1

se2 =matrix(c(std.error(mydata3[int1,]$openness)*1.96,std.error(mydata3[int1,]$conscientiousness)*1.96,std.error(mydata3[int1,]$extraversion)*1.96,
              std.error(mydata3[int1,]$agreeableness)*1.96,std.error(mydata3[int1,]$emostab)*1.96,std.error(mydata3[int1,]$mtscale)*1.96,
              std.error(mydata3[int1,]$rrscale)*1.96,std.error(mydata3[int1,]$escale)*1.96,std.error(mydata3[int1,]$ascale)*1.96
))
Means2 =c(mean(mydata3[int1,]$openness, na.rm=TRUE),mean(mydata3[int1,]$conscientiousness, na.rm=TRUE),mean(mydata3[int1,]$extraversion, na.rm=TRUE),
          mean(mydata3[int1,]$agreeableness, na.rm=TRUE),mean(mydata3[int1,]$emostab, na.rm=TRUE),mean(mydata3[int1,]$mtscale, na.rm=TRUE),
          mean(mydata3[int1,]$rrscale, na.rm=TRUE),mean(mydata3[int1,]$escale, na.rm=TRUE),mean(mydata3[int1,]$ascale, na.rm=TRUE)
)
se2
Means2

se3 =matrix(c(std.error(mydata3[ftf1,]$openness)*1.96,std.error(mydata3[ftf1,]$conscientiousness)*1.96,std.error(mydata3[ftf1,]$extraversion)*1.96,
              std.error(mydata3[ftf1,]$agreeableness)*1.96,std.error(mydata3[ftf1,]$emostab)*1.96,std.error(mydata3[ftf1,]$mtscale)*1.96,
              std.error(mydata3[ftf1,]$rrscale)*1.96,std.error(mydata3[ftf1,]$escale)*1.96,std.error(mydata3[ftf1,]$ascale)*1.96
))
Means3 =c(mean(mydata3[ftf1,]$openness, na.rm=TRUE),mean(mydata3[ftf1,]$conscientiousness, na.rm=TRUE),mean(mydata3[ftf1,]$extraversion, na.rm=TRUE),
          mean(mydata3[ftf1,]$agreeableness, na.rm=TRUE),mean(mydata3[ftf1,]$emostab, na.rm=TRUE),mean(mydata3[ftf1,]$mtscale, na.rm=TRUE),
          mean(mydata3[ftf1,]$rrscale, na.rm=TRUE),mean(mydata3[ftf1,]$escale, na.rm=TRUE),mean(mydata3[ftf1,]$ascale, na.rm=TRUE)
)
se3
Means3


sd1 =c(sd(mydata3[mturk,]$openness, na.rm=TRUE),sd(mydata3[mturk,]$conscientiousness, na.rm=TRUE),sd(mydata3[mturk,]$extraversion, na.rm=TRUE),
          sd(mydata3[mturk,]$agreeableness, na.rm=TRUE),sd(mydata3[mturk,]$emostab, na.rm=TRUE),sd(mydata3[mturk,]$mtscale, na.rm=TRUE),
          sd(mydata3[mturk,]$rrscale, na.rm=TRUE),sd(mydata3[mturk,]$escale, na.rm=TRUE),sd(mydata3[mturk,]$ascale, na.rm=TRUE)
)

sd1


sd2=c(sd(mydata3[int1,]$openness, na.rm=TRUE),sd(mydata3[int1,]$conscientiousness, na.rm=TRUE),sd(mydata3[int1,]$extraversion, na.rm=TRUE),
          sd(mydata3[int1,]$agreeableness, na.rm=TRUE),sd(mydata3[int1,]$emostab, na.rm=TRUE),sd(mydata3[int1,]$mtscale, na.rm=TRUE),
          sd(mydata3[int1,]$rrscale, na.rm=TRUE),sd(mydata3[int1,]$escale, na.rm=TRUE),sd(mydata3[int1,]$ascale, na.rm=TRUE)
)
sd2

sd3 =c(sd(mydata3[ftf1,]$openness, na.rm=TRUE),sd(mydata3[ftf1,]$conscientiousness, na.rm=TRUE),sd(mydata3[ftf1,]$extraversion, na.rm=TRUE),
          sd(mydata3[ftf1,]$agreeableness, na.rm=TRUE),sd(mydata3[ftf1,]$emostab, na.rm=TRUE),sd(mydata3[ftf1,]$mtscale, na.rm=TRUE),
          sd(mydata3[ftf1,]$rrscale, na.rm=TRUE),sd(mydata3[ftf1,]$escale, na.rm=TRUE),sd(mydata3[ftf1,]$ascale, na.rm=TRUE)
)
sd3

t.test(mydata3[mturk,]$openness,mydata3[int1,]$openness)
t.test(mydata3[mturk,]$openness,mydata3[ftf1,]$openness)
t.test(mydata3[int1,]$openness,mydata3[ftf1,]$openness)

t.test(mydata3[mturk,]$conscientiousness,mydata3[int1,]$conscientiousness)
t.test(mydata3[mturk,]$conscientiousness,mydata3[ftf1,]$conscientiousness)
t.test(mydata3[int1,]$conscientiousness,mydata3[ftf1,]$conscientiousness)

t.test(mydata3[mturk,]$extraversion,mydata3[int1,]$extraversion)
t.test(mydata3[mturk,]$extraversion,mydata3[ftf1,]$extraversion)
t.test(mydata3[int1,]$extraversion,mydata3[ftf1,]$extraversion)

t.test(mydata3[mturk,]$agreeableness,mydata3[int1,]$agreeableness)
t.test(mydata3[mturk,]$agreeableness,mydata3[ftf1,]$agreeableness)
t.test(mydata3[int1,]$agreeableness,mydata3[ftf1,]$agreeableness)

t.test(mydata3[mturk,]$emostab,mydata3[int1,]$emostab)
t.test(mydata3[mturk,]$emostab,mydata3[ftf1,]$emostab)
t.test(mydata3[int1,]$emostab,mydata3[ftf1,]$emostab)

t.test(mydata3[mturk,]$mtscale,mydata3[int1,]$mtscale)
t.test(mydata3[mturk,]$mtscale,mydata3[ftf1,]$mtscale)
t.test(mydata3[int1,]$mtscale,mydata3[ftf1,]$mtscale)

t.test(mydata3[mturk,]$rrscale,mydata3[int1,]$rrscale)
t.test(mydata3[mturk,]$rrscale,mydata3[ftf1,]$rrscale)
t.test(mydata3[int1,]$rrscale,mydata3[ftf1,]$rrscale)

t.test(mydata3[mturk,]$escale,mydata3[int1,]$escale)
t.test(mydata3[mturk,]$escale,mydata3[ftf1,]$escale)
t.test(mydata3[int1,]$escale,mydata3[ftf1,]$escale)

t.test(mydata3[mturk,]$ascale,mydata3[int1,]$ascale)
t.test(mydata3[mturk,]$ascale,mydata3[ftf1,]$ascale)
t.test(mydata3[int1,]$ascale,mydata3[ftf1,]$ascale)

