#making dataframe 
df <- matrix(c(3558,8144,24985,234057), ncol=2)
#test expected value
pchi2<-chisq.test(df,correct = FALSE)
#expected value
pchi2$expected
pchi2

#
library(epiDisplay)
cci(3558,8144,24985,234057)

#example 2
df <- matrix(c(102,1360,548,6232,1964,26481,14352,219705), ncol=2)
#test expected value
pchi2<-chisq.test(df,correct = FALSE)
#expected value
pchi2$expected
pchi2

#compare each paire
cci(1964,26481,14352,219705)
cci(548,6232,14352,219705)
cci(102,1360,14352,219705)

#use logistic regression
library(epiDisplay)
outcome<-c(1,1,1,1,0,0,0,0)	
var<-c(3,2,1,0,3,2,1,0)
freq<-c(102,548,1964,14352,1360,6232,26481,219705)
df<-data.frame(outcome,var,freq)
df$var<-as.factor(df$var)
mylogit <- glm(outcome ~ var, 
               data = df, family = "binomial",weights =freq )
logistic.display(mylogit)

#Fisher
dat2 <- matrix(c(52,2,3,16), ncol=2)
pchi2<-chisq.test(dat2,correct = FALSE)
pchi2
fisher.test(dat2)
library(epiDisplay)
cci(52,2,3,16)

#mantel hanzeal
library(epiDisplay)
tbl <- array(c(2,13,21,177,11,9,4,32),
             dim=c(2,2,2),
             dimnames=list(histdm=c("Yes","No"),
                           retiodm=c("Case","Control"),
                           duradm=c("0-6yrs",">=6")))
mhor(mhtable=tbl,decomal=4,graph = TRUE, design = "case control")


#ANSWER
cci(13,4987,7,9993)
cci(2,23,5,30)

library(epiDisplay)
tbl <- array(c(120,111,80,155,161,117,130,124),
             dim=c(2,2,2),
             dimnames=list(PassiveSmoke=c("Yes","No"),
                           Outcome=c("Case","Control"),
                           STRATUM=c("Smoke","Non-Smoke")))

mhor(mhtable=tbl,decomal=4,graph = TRUE, design = "case control")

