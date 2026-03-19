library(tidyverse)
library(reshape2)
library(readstata13)

setwd("")
a <- read.dta13("paralleltrends_forR.dta",convert.factors=F)

########################################

Wave <- rep(rep(c(1,3,5,7,10,14,17),3),5)

period <- factor(c(rep(3,21),
                 rep(2,21),
                 rep(1,21),
                 rep(5,21),
                 rep(4,21)
                 ))

groups <- rep(c(rep("Up Two",7),rep("Up One",7),rep("No Change",7)),5)

mymean <- function(wave,var){
  themean <- mean(a$eclr[a$wave==wave & var==1],na.rm=T)
  return(themean)
}

eclr <- c(NA,NA,c(mymean(5,a$sustup2_5710),mymean(7,a$sustup2_5710),mymean(10,a$sustup2_5710)),#5-7-10
          NA,NA,NA,NA,
          c(mymean(5,a$sustup_5710),mymean(7,a$sustup_5710),mymean(10,a$sustup_5710)),
          NA,NA,NA,NA,
          c(mymean(5,a$sustnoch_5710),mymean(7,a$sustnoch_5710),mymean(10,a$sustnoch_5710)),NA,NA,

          NA,c(mymean(3,a$sustup2_357),mymean(5,a$sustup2_357),mymean(7,a$sustup2_357)),  #3-5-7
          NA,NA,NA,NA,
          c(mymean(3,a$sustup_357),mymean(5,a$sustup_357),mymean(7,a$sustup_357)),
          NA,NA,NA,NA,
          c(mymean(3,a$sustnoch_357),mymean(5,a$sustnoch_357),mymean(7,a$sustnoch_357)),
          NA,NA,NA,   
          
          c(mymean(1,a$sustup2_135),mymean(3,a$sustup2_135),mymean(5,a$sustup2_135)),# 1-3-5
          NA,NA,NA,NA,
          c(mymean(1,a$sustup_135),mymean(3,a$sustup_135),mymean(5,a$sustup_135)),
          NA,NA,NA,NA,
          c(mymean(1,a$sustnoch_135),mymean(3,a$sustnoch_135),mymean(5,a$sustnoch_135)),
          NA,NA,NA,NA, 
          
          NA,NA,NA,NA,c(mymean(10,a$sustup2_101417),mymean(14,a$sustup2_101417),mymean(17,a$sustup2_101417)),#10-14-17
          NA,NA,NA,NA,c(mymean(10,a$sustup_101417),mymean(14,a$sustup_101417),mymean(17,a$sustup_101417)), 
          NA,NA,NA,NA,c(mymean(10,a$sustnoch_101417),mymean(14,a$sustnoch_101417),mymean(17,a$sustnoch_101417)),
          
          NA,NA,NA,c(mymean(7,a$sustup2_71014),mymean(10,a$sustup2_71014),mymean(14,a$sustup2_71014)),NA, #7-10-14
          NA,NA,NA,c(mymean(7,a$sustup_71014),mymean(10,a$sustup_71014),mymean(14,a$sustup_71014)),NA, 
          NA,NA,NA,c(mymean(7,a$sustnoch_71014),mymean(10,a$sustnoch_71014),mymean(14,a$sustnoch_71014)),NA
        )  



dataset <- as.data.frame(cbind(Wave,period,groups,eclr))  
dataset$eclr <- as.numeric(as.character(dataset$eclr))
dataset$Wave <- as.numeric(as.character(dataset$Wave))
levels(dataset$period) <- c("Waves 1-3-5","Waves 3-5-7","Waves 5-7-10","Waves 7-10-14","Waves 10-14-17")
vline.dat <- data.frame(period=unique(dataset$period),vl=c(7,5,3,14,10))

png(file="figure2.jpg",units="in",height=6,width=8,res=500)
ggplot(dataset,aes(y=eclr,x=Wave)) +
  geom_line(size=1.1,aes(linetype=groups)) +
  facet_wrap(period~.) +
  geom_vline(aes(xintercept=vl),data=vline.dat) +
  scale_x_continuous(breaks=c(1,3,5,7,9,11,13,15,17)) +
  scale_colour_discrete(name="Extent \nof Mobility") +
  scale_y_continuous(name="Mean Economic Ideology")+
  labs(caption="Note: Black vertical lines indicate period in which class mobility begins.\n All plots follow the same individuals over time") +
  scale_linetype_manual(values=c("solid","dotted","dashed"))
dev.off()



         
         
