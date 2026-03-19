####################################################
#### Replication file for the online appendix   ####
#### for "Pigeonholing Partisans" in Political  ####
#### Behavior. Created 3.17.2018 by the authors ####
####################################################
library(stm)
library(readstata13)
library(tm)
library(Hmisc)
library(dplyr)
library(Rtsne)
library(geometry)
library(stargazer)
library(sandwich)
library(reshape)
library(ggplot2)
library(stringr)
library(betareg)
#These lines are for robust standard errors in the later OLS modeling commands
library(sandwich)
library(foreign)
if (!require("RCurl")) {
  install.packages("RCurl", dependencies = TRUE)
  library(RCurl)
}
url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(ulr_robust, ssl.verifypeer = FALSE)),
     envir=.GlobalEnv)
####Read in the data####
setwd("")
data<- read.dta13("Pigeonholing partisans.dta")
# MAKING VARIABLES NUMERIC
data$Hisp2 <- ifelse(data$Hisp == "Not Hispanic", 0,1)
# REMOVING CASES WITH MISSING DATA (stm won't work otherwise)
data2 <- data[complete.cases(data$Inc),]
data3 <- data2[complete.cases(data2$Gender),]
data4 <- data3[complete.cases(data3$WHITE),]
data5 <- data4[complete.cases(data4$Interest),]
data6 <- data5[complete.cases(data5$sample),]
data7 <- data6[complete.cases(data6$Hisp2),]
data8 <- data7[complete.cases(data7$Age),]
data9 <- data8[complete.cases(data8$PK_Scale),]
data10 <- data9[complete.cases(data9$PID7),]
data11 <- data10[complete.cases(data10$Education2),]
data12 <- data11[which(data11$Inc>0),]

####Detailed results of the topic modeling process####
#These commands were used in the model searching for Democrats:
#PREPPING TEXT for the stm analyses
processedD <- textProcessor(data12$GenD, metadata = data12)
outD <- prepDocuments(processedD$documents, processedD$vocab,
                      processedD$meta, lower.thresh = 5)
docsD <- outD$documents
vocabD <- outD$vocab
metaD <- outD$meta
#Initial model search
GenDSearch2 <- searchK(outD$documents, outD$vocab, K = c(5,10,15,20,25,35,45,55,75),
                       prevalence =~ Inc + Gender + WHITE + Interest + Education2 + 
                         Hisp2 + Age + PK_Scale + PID7 + as.factor(sample), data = metaD, LDAbeta=FALSE)
knitr::kable(GenDSearch2$results)
plot(GenDSearch2)
#Additional search, narrowing the window
GenDSearch3 <- searchK(outD$documents, outD$vocab, K = c(5:20),
                       prevalence =~ Inc + Gender + WHITE + Interest + Education2 + 
                         Hisp2 + Age + PK_Scale + PID7 + as.factor(sample), data = metaD, LDAbeta=FALSE)
knitr::kable(GenDSearch3$results)
plot(GenDSearch3)
## SAGE Model selection, with 10 and 15 topics
GenDSelect10SAGE <- selectModel(outD$documents, outD$vocab, K = 10,
                                prevalence =~ Inc + Gender + WHITE + Interest + Education2 + 
                                  Hisp2 + Age + PK_Scale + PID7 + as.factor(sample), max.em.its = 500,
                                data = outD$meta, runs = 30, seed = 1, LDAbeta=FALSE)

GenDSelect15SAGE <- selectModel(outD$documents, outD$vocab, K = 15,
                                prevalence =~ Inc + Gender + WHITE + Interest + Education2 + 
                                  Hisp2 + Age + PK_Scale + PID7 + as.factor(sample), max.em.its = 500,
                                data = outD$meta, runs = 30, seed = 1, LDAbeta=FALSE)
#Final model:
GenDChoice <- GenDSelect10SAGE$runout[[5]]

#These commands were used in the model searching for Republicans:
#PREPPING TEXT for the stm analyses
processedR <- textProcessor(data12$GenR, metadata = data12)
outR <- prepDocuments(processedR$documents, processedR$vocab,
                      processedR$meta, lower.thresh = 5)
docsR <- outR$documents
vocabR <- outR$vocab
metaR <- outR$meta

# MODEL SEARCH ACROSS NUMBER OF TOPICS
GenRSearch <- searchK(outR$documents, outR$vocab, K = c(5,10,15,20,25,35,45,55,75),
                      prevalence =~ Inc + Gender + WHITE + Interest + Education2 + 
                        Hisp2 + Age + PK_Scale + PID7 + as.factor(sample), data = metaR)
knitr::kable(GenRSearch$results)
plot(GenRSearch)

# second search model adding argument: LDAbeta=FALSE to account for short documents
GenRSearch2 <- searchK(outR$documents, outR$vocab, K = c(5,10,15,20,25,35,45,55,75),
                       prevalence =~ Inc + Gender + WHITE + Interest + Education2 + 
                         Hisp2 + Age + PK_Scale + PID7 + as.factor(sample), data = metaR, LDAbeta=FALSE)
knitr::kable(GenRSearch2$results)
plot(GenRSearch2)

# third search, narrowing the window
GenRSearch3 <- searchK(outR$documents, outR$vocab, K = c(5:20),
                       prevalence =~ Inc + Gender + WHITE + Interest + Education2 + 
                         Hisp2 + Age + PK_Scale + PID7 + as.factor(sample), data = metaR, LDAbeta=FALSE)
knitr::kable(GenRSearch3$results)
plot(GenRSearch3)

## SAGE Model selection, with 8, 9, and 10 topics
GenRSelect8SAGE <- selectModel(outR$documents, outR$vocab, K = 8,
                               prevalence =~ Inc + Gender + WHITE + Interest + Education2 + 
                                 Hisp2 + Age + PK_Scale + PID7 + as.factor(sample), max.em.its = 500,
                               data = outR$meta, runs = 30, seed = 1, LDAbeta=FALSE)

GenRSelect9SAGE <- selectModel(outR$documents, outR$vocab, K = 9,
                               prevalence =~ Inc + Gender + WHITE + Interest + Education2 + 
                                 Hisp2 + Age + PK_Scale + PID7 + as.factor(sample), max.em.its = 500,
                               data = outR$meta, runs = 30, seed = 1, LDAbeta=FALSE)
#Model selection
GenRSelect10SAGE <- selectModel(outR$documents, outR$vocab, K = 10,
                                prevalence =~ Inc + Gender + WHITE + Interest + Education2 + 
                                  Hisp2 + Age + PK_Scale + PID7 + as.factor(sample), max.em.its = 500,
                                data = outR$meta, runs = 30, seed = 1, LDAbeta=FALSE)

### Selected model
GenRChoice <- GenRSelect10SAGE$runout[[5]]

### Finding Exemplar Documents for Each Topic ###

thought1D <- findThoughts(GenDChoice, texts=metaD$GenD, n=10, topics=1)$docs[[1]]
plotQuote(thought1D, main="Dem Topic 1")

thought2D <- findThoughts(GenDChoice, texts=metaD$GenD, n=10, topics=2)$docs[[1]]
plotQuote(thought2D, main="Dem Topic 2")

thought3D <- findThoughts(GenDChoice, texts=metaD$GenD, n=10, topics=3)$docs[[1]]
plotQuote(thought3D, main="Dem Topic 3")

thought4D <- findThoughts(GenDChoice, texts=metaD$GenD, n=10, topics=4)$docs[[1]]
plotQuote(thought4D, main="Dem Topic 4")

thought5D <- findThoughts(GenDChoice, texts=metaD$GenD, n=10, topics=5)$docs[[1]]
plotQuote(thought5D, main="Dem Topic 5")

thought6D <- findThoughts(GenDChoice, texts=metaD$GenD, n=10, topics=6)$docs[[1]]
plotQuote(thought6D, main="Dem Topic 6")

thought7D <- findThoughts(GenDChoice, texts=metaD$GenD, n=10, topics=7)$docs[[1]]
plotQuote(thought7D, main="Dem Topic 7")

thought8D <- findThoughts(GenDChoice, texts=metaD$GenD, n=10, topics=8)$docs[[1]]
plotQuote(thought8D, main="Dem Topic 8")

thought9D <- findThoughts(GenDChoice, texts=metaD$GenD, n=10, topics=9)$docs[[1]]
plotQuote(thought9D, main="Dem Topic 9")

thought10D <- findThoughts(GenDChoice, texts=metaD$GenD, n=10, topics=10)$docs[[1]]
plotQuote(thought10D, main="Dem Topic 10")

thought1R <- findThoughts(GenRChoice, texts=metaR$GenR, n=10, topics=1)$docs[[1]]
plotQuote(thought1R, main="Rep Topic 1")

thought2R <- findThoughts(GenRChoice, texts=metaR$GenR, n=10, topics=2)$docs[[1]]
plotQuote(thought2R, main="Rep Topic 2")

thought3R <- findThoughts(GenRChoice, texts=metaR$GenR, n=10, topics=3)$docs[[1]]
plotQuote(thought3R, main="Rep Topic 3")

thought4R <- findThoughts(GenRChoice, texts=metaR$GenR, n=10, topics=4)$docs[[1]]
plotQuote(thought4R, main="Rep Topic 4")

thought5R <- findThoughts(GenRChoice, texts=metaR$GenR, n=10, topics=5)$docs[[1]]
plotQuote(thought5R, main="Rep Topic 5")

thought6R <- findThoughts(GenRChoice, texts=metaR$GenR, n=10, topics=6)$docs[[1]]
plotQuote(thought6R, main="Rep Topic 6")

thought7R <- findThoughts(GenRChoice, texts=metaR$GenR, n=10, topics=7)$docs[[1]]
plotQuote(thought7R, main="Rep Topic 7")

thought8R <- findThoughts(GenRChoice, texts=metaR$GenR, n=10, topics=8)$docs[[1]]
plotQuote(thought8R, main="Rep Topic 8")

thought9R <- findThoughts(GenRChoice, texts=metaR$GenR, n=10, topics=9)$docs[[1]]
plotQuote(thought9R, main="Rep Topic 9")

thought10R <- findThoughts(GenRChoice, texts=metaR$GenR, n=10, topics=10)$docs[[1]]
plotQuote(thought10R, main="Rep Topic 10")


###Table A.1: Demographic table####
table(data$Gender, data$sample) #1=Female
227/(227+207)
160/(106+160)
374/(339+374)
table(data$PID3_p, data$sample) #1 is Dem, 2 is Ind, 3 is Rep
328/(328+28+81)
28/(328+28+81)
81/(328+28+81)
147/(147+44+77)
44/(147+44+77)
77/(147+44+77)
317/(317+130+259)
130/(317+130+259)
259/(317+130+259)
table(data$Ideo, data$sample)
(46+155+93)/(46+155+93+76+44+21+2)
(76)/(46+155+93+76+44+21+2)
(44+21+2)/(46+155+93+76+44+21+2)
(34+67+32)/(34+67+32+64+21+37+14)
(64)/(34+67+32+64+21+37+14)
(21+37+14)/(34+67+32+64+21+37+14)
(33+111+75)/(33+111+75+216+101+119+31)
(216)/(33+111+75+216+101+119+31)
(101+119+31)/(33+111+75+216+101+119+31)
table(data$WHITE, data$sample) #White=1
158/(274+158)
39/(229+39)
138/(566+138)
median(data$Inc[data$sample=="Subject pool"], na.rm=T) #7 corresponds to b/w 150 and 200K 
median(data$Inc[data$sample=="MTurk"], na.rm=T) #4 corresponds to b/w 50 and 75K for this sample
median(data$Inc[data$sample=="National"], na.rm=T) #4 corresponds to b/w 50 and 75K
median(data$Education2[data$sample=="Subject pool"], na.rm=T) #3 Some college 
median(data$Education2[data$sample=="MTurk"], na.rm=T) #4 corresponds to 2 year college degree 
median(data$Education2[data$sample=="National"], na.rm=T) #5 corresponds to Bachelor's degree
median(data$Interest[data$sample=="Subject pool"], na.rm=T) #3 Some college 
median(data$Interest[data$sample=="MTurk"], na.rm=T) #4 corresponds to 2 year college degree 
median(data$Interest[data$sample=="National"], na.rm=T) #5 corresponds to Bachelor's degree

####For tables A.2-A.5, we need the long data again####
long=read.dta13("Pigeonholing partisans_long.dta")
####Table A.2####
sort(table(long$GenR_recode[long$sample==2]),decreasing=TRUE)[1:11]
##Blank comes in here at about the number 9 slot
sort(table(long$GenR_recode[long$sample==1]),decreasing=TRUE)[1:11]
##Blank comes in here at about the number 3 slot
sort(table(long$GenR_recode[long$sample==0]),decreasing=TRUE)[1:11]
##Blank comes in here at about the number 1 slot

####Table A.3####
sort(table(long$GenD_recode[long$sample==2]),decreasing=TRUE)[1:11]
##Blank comes in here at about the number 8 slot
sort(table(long$GenD_recode[long$sample==1]),decreasing=TRUE)[1:11]
##Blank comes in here at about the number 2 slot
sort(table(long$GenD_recode[long$sample==0]),decreasing=TRUE)[1:11]
##Blank comes in here at about the number 1 slot

####Table A.4####
sort(table(long$GenD_recode[long$sample==0&long$PID3==1]),decreasing=TRUE)[1:11]
##Blank comes in here at about the number 1 slot
sort(table(long$GenD_recode[long$sample==0&long$PID3==2]),decreasing=TRUE)[1:11]
##Blank comes in here at about the number 1 slot
sort(table(long$GenD_recode[long$sample==0&long$PID3==3]),decreasing=TRUE)[1:11]
##Blank comes in here at about the number 1 slot

####Table A.5####
sort(table(long$GenR_recode[long$sample==0&long$PID3==1]),decreasing=TRUE)[1:11]
##Blank comes in here at about the number 1 slot
sort(table(long$GenR_recode[long$sample==0&long$PID3==2]),decreasing=TRUE)[1:11]
##Blank comes in here at about the number 1 slot
sort(table(long$GenR_recode[long$sample==0&long$PID3==3]),decreasing=TRUE)[1:11]
##Blank comes in here at about the number 1 slot

####Table A.6####
#Models
m.issue=lm(issgro~Inc + Gender + WHITE + Interest + Education2 + Hisp2 + Age+ 
             PK_Scale + PID7 + as.factor(sample), data=newdata)
summary(m.issue, robust=T)
m.trait=lm(trait~Inc + Gender + WHITE + Interest + Education2 + Hisp2 + Age+ 
             PK_Scale + PID7 + as.factor(sample), data=newdata)
summary(m.trait, robust=T)
m.ambig=lm(ambig~Inc + Gender + WHITE + Interest + Education2 + Hisp2 + Age+ 
             PK_Scale + PID7 + as.factor(sample), data=newdata)
summary(m.ambig, robust=T)

m.issue2=lm(issgro2~Inc + Gender + WHITE + Interest + Education2 + Hisp2 + Age+ 
              PK_Scale + PID7 + as.factor(sample), data=newdata)
summary(m.issue2, robust=T)
m.trait2=lm(trait2~Inc + Gender + WHITE + Interest + Education2 + Hisp2 + Age+ 
              PK_Scale + PID7 + as.factor(sample), data=newdata)
summary(m.trait2, robust=T)
m.ambig2=lm(ambig2~Inc + Gender + WHITE + Interest + Education2 + Hisp2 + Age+ 
              PK_Scale + PID7 + as.factor(sample), data=newdata)
summary(m.ambig2, robust=T)
m.blank=lm(blank~Inc + Gender + WHITE + Interest + Education2 + Hisp2 + Age+ 
             PK_Scale + PID7 + as.factor(sample), data=newdata)
summary(m.blank, robust=T)

setwd("C:/Users/busby/Box Sync/Partisan Stereotypes/Data and Analysis files/Analyses and figures/Topic OLS Tables")

se1=sqrt(diag(vcovHC(m.issue, type = "HC1")))
se2=sqrt(diag(vcovHC(m.issue2, type = "HC1")))
se3=sqrt(diag(vcovHC(m.trait, type = "HC1")))
se4=sqrt(diag(vcovHC(m.trait2, type = "HC1")))
se5=sqrt(diag(vcovHC(m.ambig, type = "HC1")))
se6=sqrt(diag(vcovHC(m.ambig2, type = "HC1")))
se7=sqrt(diag(vcovHC(m.blank, type = "HC1")))


stargazer(m.issue, m.issue2, m.trait, m.trait2, m.ambig, m.ambig2, m.blank,
          covariate.labels = c("Intercept","Income","Gender","White", "Interest","Education", 
                               "Hispanic/Latin@", "Age", "Political Knowledge Scale", "Party ID (7 point)", 
                               "MTurk Sample", "Student sample"),
          type="html",
          style = "apsr",
          label="tab:study1",
          title="",
          intercept.bottom = FALSE, 
          intercept.top=TRUE,
          se=list(se1, se2, se3, se4, se5, se6, se7),
          omit.stat="f",
          digits=3, 
          star.cutoffs=c(.05,.01,.001),
          out="Predict_topics.htm"
)

####Table A.7####
##Democratic topics
prepD <- estimateEffect(1:10 ~ Inc + Gender + WHITE + Interest + Education2 + 
                          Hisp2 + Age + PK_Scale + PID7 + as.factor(sample), GenDChoice,
                        meta = outD$meta, uncertainty = "Global")
labels10 <- c('Topic1','Topic2','Topic3','Topic4','Topic5','Topic6','Topic7',
              'Topic8','Topic9','Topic10')
summary(prepD)


##Republican Topics
prepR <- estimateEffect(1:10 ~ Inc + Gender + WHITE + Interest + Education2 + 
                          Hisp2 + Age + PK_Scale + PID7 + as.factor(sample), GenRChoice,
                        meta = outR$meta, uncertainty = "Global")
labels10 <- c('Topic1','Topic2','Topic3','Topic4','Topic5','Topic6','Topic7',
              'Topic8','Topic9','Topic10')
summary(prepR)


####Table A.8####
FT=lm(FT_diff~ issgro + trait + ambig + Inc + Gender + WHITE + Interest + Education2 + 
        Hisp2 + Age + PK_Scale + PID7+issues+GenR_vmean+GenD_vmean, data=newdata2)
summary(FT, robust=T)

#Issue extremity
ext=lm(Ext_diff~ issgro + trait + ambig + Inc + Gender + WHITE + Interest + Education2 
       + Hisp2 + Age + PK_Scale + PID7+issues+GenR_vmean+GenD_vmean, data=newdata2)
summary(ext, robust=T)

#Ideology:
ideo=lm(Ideo_diff~ issgro + trait + ambig + Inc + Gender + WHITE + Interest + Education2 
        + Hisp2 + Age + PK_Scale + PID7+issues+GenR_vmean+GenD_vmean, data=newdata2)
summary(ideo, robust=T)

#Issue extremity in Congress
ext_c=lm(Ext_Congress_diff~ issgro + trait +ambig + Inc + Gender + WHITE + Interest + Education2 
         + Hisp2 + Age + PK_Scale + PID7+issues+GenR_vmean+GenD_vmean, data=newdata2)
summary(ext_c, robust=T)

#Ideology in Congress
ideo_c=lm(Ideo_Congress_diff~ issgro + trait + ambig + Inc + Gender + WHITE + Interest + Education2 
          + Hisp2 + Age + PK_Scale + PID7+issues+GenR_vmean+GenD_vmean, data=newdata2)
summary(ideo_c, robust=T)

#Ideology (self)
ideo_f=lm(Ideo_f~ issgro + trait + ambig + Inc + Gender + WHITE + Interest + Education2 + 
            Hisp2 + Age + PK_Scale + PID7+issues+GenR_vmean+GenD_vmean, data=newdata2)
summary(ideo_f, robust=T)

#Let's make some nice tables:
#This is so the table contains robust SEs
se1=sqrt(diag(vcovHC(FT, type = "HC1")))
se4=sqrt(diag(vcovHC(ideo_f, type = "HC1")))
se5=sqrt(diag(vcovHC(ideo, type = "HC1")))
se6=sqrt(diag(vcovHC(ideo_c, type = "HC1")))
se7=sqrt(diag(vcovHC(ext, type = "HC1")))
se8=sqrt(diag(vcovHC(ext_c, type = "HC1")))

stargazer(ideo_f, ideo, ideo_c, ext, ext_c, FT,
          covariate.labels = c("Intercept (No response)","Issues and Groups","Traits","Ambiguous ", "Income","Gender", 
                               "White", "Political Interest", "Education", "Hispanic", "Age", 
                               "Political Knowledge Scale", "Party ID (7 point)", "Answered issues domain", 
                               "Rep stereotype valence", "Dem stereotype valence"),
          type="html",
          style = "apsr",
          label="tab:study1",
          title="",
          intercept.bottom = FALSE, 
          intercept.top=TRUE,
          se=list(se4, se5, se6, se7, se8, se1),
          omit.stat="f",
          digits=3, 
          star.cutoffs=c(.05,.01,.001),
          out="Table A.8.htm"
)

####Figure 1 with ambiguous topics####
#Version 1
coef=c(-0.061, -0.001, 0.008, 0.053,
       -0.012, 0.016, -0.003, -0.001,
       -0.020, -0.012, -0.003, 0.035) 
se= c(0.006, 0.003, 0.002, 0.003,
      0.004, 0.002, 0.001, 0.002,
      0.007, 0.003, 0.002, 0.003
)

t1=cbind(coef, se)
t1=data.frame(t1)
t1$names=c("Knowledge", "Knowledge", "Knowledge", "Knowledge",
           "Interest", "Interest", "Interest", "Interest",
           "Education", "Education", "Education", "Education")
t1$names=factor(t1$names, levels=c("Education", "Interest", "Knowledge"))
t1$outcome=c("Don't know", "Traits", "Issues/Groups", "Ambiguous",
             "Don't know", "Traits", "Issues/Groups", "Ambiguous",
             "Don't know", "Traits", "Issues/Groups", "Ambiguous")
t1$outcome=factor(t1$outcome, levels=c("Traits", "Issues/Groups", "Ambiguous", "Don't know"))
#These commands do the actual graphing
plot1=ggplot(data = t1, aes(x=outcome, y=coef, ymin=I(coef-(1.96*se)), ymax=I(coef+(1.96*se))))+
  geom_pointrange(color="blue") +
  geom_hline(yintercept=0, size=1, linetype="dashed", color="red") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y= "Change in proportion of this kind of topic", 
       x="Topic type")

plot1=plot1+facet_grid(.~names)+theme(strip.text.x = element_text(size = 15))+theme(strip.background = element_blank())

#Flips it, simply because I like the other orientation better.
plot1+coord_flip()

####Additional robustness checks####
#Self-placed ideology as an ordered model:
library(MASS)
#More strict coding
ideo_f=polr(as.ordered(Ideo_f)~ issgro + trait + ambig + Inc + Gender + WHITE + Interest + Education2 + 
              Hisp2 + Age + PK_Scale + PID7+issues+GenR_vmean+GenD_vmean, data=newdata2, method="logistic", Hess=TRUE)
summary(ideo_f)
#More lenient coding
ideo_f=polr(as.ordered(Ideo_f)~ issgro2 + trait2 + ambig2 + Inc + Gender + WHITE + Interest + Education2 + 
              Hisp2 + Age + PK_Scale + PID7+issues+GenR_vmean+GenD_vmean, data=newdata2, method="logistic", Hess=TRUE)
summary(ideo_f)