library(tidyverse)
library(reshape2)
library(readstata13)

setwd("")
options(scipen=999)
#########################################################################

## FIRST ANALYSIS (FIG 1 IN PAPER: YEARS SINCE CHANGE) ##

## FUNCTION TO GET INFO NEEDED ON CHANGE ##
myvalues <- function(clasch,amount){
  if(amount==1){
    myvals <- c(mean(a$ideol_ch[a$class_ch==clasch & a$lastclass==2 & a$tclass_ch==clasch],na.rm=T),
                sd(a$ideol_ch[a$class_ch==clasch & a$lastclass==2 & a$tclass_ch==clasch],na.rm=T),
                length(a$ideol_ch[a$class_ch==clasch & a$lastclass==2 & !is.na(a$ideol_ch) & a$tclass_ch==clasch])
    )
    return(myvals)} else if(amount==2){
      myvals <- c(mean(a$ideol_ch[a$class_ch==clasch & a$lastclass==2 & a$tclass_ch==0 & a$tclass_ch2==clasch],na.rm=T),
                  sd(a$ideol_ch[a$class_ch==clasch & a$lastclass==2 & a$tclass_ch==0 & a$tclass_ch2==clasch],na.rm=T),
                  length(a$ideol_ch[a$class_ch==clasch & a$lastclass==2 & !is.na(a$ideol_ch) & a$tclass_ch==0 & a$tclass_ch2==clasch])
      )
      return(myvals)} else{
        myvals <- c(mean(a$ideol_ch[a$class_ch==clasch & a$lastclass==2 & a$tclass_ch==0 & a$tclass_ch2==0 & a$tclass_ch3==clasch],na.rm=T),
                    sd(a$ideol_ch[a$class_ch==clasch & a$lastclass==2 & a$tclass_ch==0 & a$tclass_ch2==0 & a$tclass_ch3==clasch],na.rm=T),
                    length(a$ideol_ch[a$class_ch==clasch & a$lastclass==2 & !is.na(a$ideol_ch) & a$tclass_ch==0 & a$tclass_ch2==0 & a$tclass_ch3==clasch])
        )
        return(myvals)}
}

## GENDER EQUALITY ##
a <- read.dta13("final_sustained_gender_forR.dta",convert.factors=F)

# values for changes of 1-3 years
dataset <- as.data.frame(rbind(
  myvalues(2,1),myvalues(1,1),myvalues(2,2),
  myvalues(1,2),c(NA,NA,NA),myvalues(1,3)
))

# values for no change
dataset <- as.data.frame(rbind(
  c(
    mean(a$ideol_ch[a$class_ch==0 & a$lastclass==2],na.rm=T),
    sd(a$ideol_ch[a$class_ch==0 & a$lastclass==2],na.rm=T),
    length(a$ideol_ch[a$class_ch==0 & a$lastclass==2 & !is.na(a$ideol_ch)])
  ),
  dataset))

colnames(dataset) <- c("delta","sd","n")

## HOMOSEXUALITY ##
a <- read.dta13("final_sustained_homosex_forR.dta",convert.factors=F)

# values for no change
dataset <- as.data.frame(rbind(
  dataset,c(
    mean(a$ideol_ch[a$class_ch==0 & a$lastclass==2],na.rm=T),
    sd(a$ideol_ch[a$class_ch==0 & a$lastclass==2],na.rm=T),
    length(a$ideol_ch[a$class_ch==0 & a$lastclass==2 & !is.na(a$ideol_ch)])
  )))

dataset2 <- as.data.frame(rbind(
  myvalues(2,1),myvalues(1,1),myvalues(2,2),
  myvalues(1,2),c(NA,NA,NA),c(NA,NA,NA)
))
colnames(dataset2) <- c("delta","sd","n")

dataset <- as.data.frame(rbind(dataset,dataset2))



## ECONOMIC IDEOLOGY ##
a <- read.dta13("final_sustained_econ_forR.dta",convert.factors=F)

# values for no change
dataset <- as.data.frame(rbind(
  dataset,c(
    mean(a$ideol_ch[a$class_ch==0 & a$lastclass==2],na.rm=T),
    sd(a$ideol_ch[a$class_ch==0 & a$lastclass==2],na.rm=T),
    length(a$ideol_ch[a$class_ch==0 & a$lastclass==2 & !is.na(a$ideol_ch)])
  )))

dataset3 <- as.data.frame(rbind(
  myvalues(2,1),myvalues(1,1),myvalues(2,2),
  myvalues(1,2),myvalues(2,3),myvalues(1,3)
))
colnames(dataset3) <- c("delta","sd","n")

dataset <- as.data.frame(rbind(dataset,dataset3))



# colnames(dataset) <- c("delta","sd","n")

## REST OF DATASET ##

dataset$change <- rep(c("No change",rep("Up, One Year",2),rep("Up, Two Years",2),
                        rep("Up, Three Years",2)),3)
dataset$change1 <- rep(factor(c(4,3,3,2,2,1,1)),3)
dataset$amount <- rep(c("No Change",rep(c("Two Classes","One Class"),3)),3)
dataset$amount1 <- rep(factor(c(1,2,3,2,3,2,3)),3)

dataset$delta <- as.numeric(as.character(dataset$delta)) 
dataset$sd <- as.numeric(as.character(dataset$sd)) 
dataset$n <- as.numeric(as.character(dataset$n)) 

dataset$se <- dataset$sd/sqrt(dataset$n)
dataset$upper <- dataset$delta + (1.96*dataset$se)
dataset$lower <- dataset$delta - (1.96*dataset$se)

dataset$variable <- c(rep("Gender Equality",7),rep("Homosexuality",7),rep("Economic Ideology",7))


png(file="figure1.jpg",units="in",height=4,width=9,res=500)
ggplot(data = dataset, aes(x = factor(change1), y = delta, ymin = lower, 
                           ymax = upper, shape=amount1)) +
  facet_wrap(~variable) +
  geom_point(position = position_dodge(width = 0.3),size=2) +
  geom_errorbar(position = position_dodge(width = 0.3), width = 0.1) +
  coord_flip() +
  scale_x_discrete(name="",labels=c("Upward Move, 2-3 Years Previously","Upward Move, 1-2 Years Previously",
                                    "Upward Move, 0-1 Years Previously","No Change")) + 
  scale_y_continuous(name="Amount of Ideological Change") +
  geom_hline(aes(yintercept=0),size=0.5) +
  theme(axis.text.x = element_text(size =11,colour="black"))+
  theme(axis.text.y = element_text(size = 8,colour="black")) +
  scale_colour_discrete(name  ="Mobility",
                        labels=c("No Change","Up Two Classes","Up One Class")) +
  scale_shape_discrete(name  ="Mobility",
                       labels=c("No Change","Up Two Classes","Up One Class")) +
  theme(legend.title=element_text(size=8)) +
  theme(axis.title=element_text(size=9))
dev.off()



#########################################################

## SECOND ANALYSIS (IN APPENDIX): MORE THAN ONE WAVE ##

myvalues <- function(sustup,ideolch){
  myvals <- c(mean(ideolch[sustup==1],na.rm=T),
              sd(ideolch[sustup==1],na.rm=T),
              length(ideolch[sustup==1 &!is.na(sustup) & !is.na(ideolch)])
  )
  return(myvals)
}

## GENDER EQUALITY ##
  a <- read.dta13("final_sustained_gender_forR2.dta",convert.factors=F)
  
  myvalues(a$sustup,a$ideol_ch)

  dataset <- as.data.frame(rbind(
    myvalues(a$sustnoch,a$ideol_ch_prev),
    myvalues(a$sustup2,a$ideol_ch_prev),
    myvalues(a$sustup,a$ideol_ch_prev),
    myvalues(a$sustnoch,a$ideol_ch),
    myvalues(a$sustup2,a$ideol_ch),
    myvalues(a$sustup,a$ideol_ch)
  ))
  
  colnames(dataset) <- c("delta","sd","n")
  
## ECONOMIC IDEOLOGY ##
  a <- read.dta13("final_sustained_econ_forR2.dta",convert.factors=F)
  
  myvalues(a$sustup,a$ideol_ch)
  
  dataset2 <- as.data.frame(rbind(
    myvalues(a$sustnoch,a$ideol_ch_prev),
    myvalues(a$sustup2,a$ideol_ch_prev),
    myvalues(a$sustup,a$ideol_ch_prev),
    myvalues(a$sustnoch,a$ideol_ch),
    myvalues(a$sustup2,a$ideol_ch),
    myvalues(a$sustup,a$ideol_ch)
  ))
  
  colnames(dataset2) <- c("delta","sd","n")
  
  dataset <- as.data.frame(rbind(dataset,dataset2))
  
  dataset$change <- rep(c(rep("After one Wave",3),rep("After Two Waves",3)),2)
  dataset$change1 <- factor(rep(c(1,1,1,2,2,2),2))
  dataset$amount <- rep(c(rep(c("No Change","Up Two Classes","Up One Class"),2)),2)
  dataset$amount1 <- factor(rep(c(1,2,3,1,2,3),2))
  
  dataset$delta <- as.numeric(as.character(dataset$delta)) 
  dataset$sd <- as.numeric(as.character(dataset$sd)) 
  dataset$n <- as.numeric(as.character(dataset$n)) 
  
  dataset$se <- dataset$sd/sqrt(dataset$n)
  dataset$upper <- dataset$delta + (1.96*dataset$se)
  dataset$lower <- dataset$delta - (1.96*dataset$se)
  
  dataset$variable <- c(rep("Gender Equality",6),rep("Economic Ideology",6))
  
 
  png(file="figurea2.jpg",units="in",height=5,width=8,res=500)
  ggplot(data = dataset, aes(x = factor(change1), y = delta, ymin = lower, 
                             ymax = upper, colour = amount1, shape=amount1)) +
    facet_wrap(~variable) +
    geom_point(position = position_dodge(width = 0.3),size=1.5) +
    geom_errorbar(position = position_dodge(width = 0.3), width = 0.1) +
    coord_flip() +
    scale_x_discrete(name="Timing of Ideology Change",labels=c("After Two Waves","After 1 Wave")) + 
    scale_y_continuous(name="Amount of Ideological Change") +
    geom_hline(aes(yintercept=0),size=0.5) +
    theme(axis.text.x = element_text(size =9,colour="black"))+
    theme(axis.text.y = element_text(size = 8,colour="black")) +
    scale_colour_discrete(name  ="Mobility",
                          labels=c("No Change","Up Two Classes","Up One Class")) +
    scale_shape_discrete(name  ="Mobility",
                         labels=c("No Change","Up Two Classes","Up One Class")) +
    theme(legend.title=element_text(size=8)) +
    theme(axis.title=element_text(size=9))
  dev.off()
#########################################################################

