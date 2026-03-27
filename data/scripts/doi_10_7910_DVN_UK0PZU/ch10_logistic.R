setwd("...")
library(epiDisplay)
#impaort data
df<-read.csv("logistic.csv")
summ(df)

#data management
tab1(df$insti,graph = FALSE)
df$insti<-factor(df$insti,levels=c("1","2"),labels=c("private","public"))
tab1(df$insti,graph = FALSE)

tab1(df$age,graph = FALSE)
df$age<-factor(df$age,levels=c("0","1"),labels=c("<35yrs",">= 35"))
tab1(df$age,graph = FALSE)

tab1(df$income,graph = FALSE)
df$income<-factor(df$income,levels=c("0","1"),labels=c("<= 2000",">2000"))
tab1(df$income,graph = FALSE)

tab1(df$parity,graph = FALSE)
df$parity<-factor(df$parity,levels=c("1","2","3"),
                     labels=c("First","Second",">=3"))
tab1(df$parity,graph = FALSE)

tab1(df$prevcs,graph = FALSE)
df$prevcs<-factor(df$prevcs,levels=c("1","2"),
                     labels=c("no","yes"))
tab1(df$prevcs,graph = FALSE)

tab1(df$abort,graph = FALSE)
df$abort<-factor(df$abort,levels=c("1","2"),
                     labels=c("no","yes"))
tab1(df$abort,graph = FALSE)

tab1(df$ancfu,graph = FALSE)
df$ancfu<-factor(df$ancfu,levels=c("1","2"),
                    labels=c("no","yes"))
tab1(df$ancfu,graph = FALSE)

tab1(df$ga,graph = FALSE)
df$ga<-factor(df$ga,levels=c("1","2","3","4"),
                    labels=c("<=36","37-39","40-42",">=43"))
tab1(df$ga,graph = FALSE)

tab1(df$deliv,graph = FALSE)
df$deliv<-factor(df$deliv,levels=c("0","1"),
                    labels=c("Vaginal","Cesarean"))
tab1(df$deliv,graph = FALSE)

#univarible analysis
cc(df$deliv,df$insti)
cc(df$deliv,df$age)
cc(df$deliv,df$income)
cc(df$deliv,df$parity)
cc(df$deliv,df$prevcs)
cc(df$deliv,df$abort)
cc(df$deliv,df$ga)
cc(df$deliv,df$ancfu)

tb1<-tableStack(vars = c(insti,age,income,parity,prevcs,abort,ancfu,ga),by=deliv,
                total.column = TRUE,dataFrame =df)
tb1


#logistic
m1<-glm(deliv~insti+prevcs+ancfu+ga,family =binomial(link = "logit"),data=df)
summary(m1)

#delete ancfu
m2<-glm(deliv~insti+prevcs+ga,family =binomial(link = "logit"),data=df)
lrtest(m2,m1)
summary(m2)

logistic.display(m2)

library(ResourceSelection)
df$deliv<-as.numeric(df$deliv)
df$deliv<-ifelse(df$deliv==2,1,0)
tab1(df$deliv)

hoslem.test(df$deliv, fitted(m2))
