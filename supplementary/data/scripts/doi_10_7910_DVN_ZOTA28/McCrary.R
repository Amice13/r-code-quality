#Open R packages for this code to run 

Sys.setenv(LANG='en')
setwd("D:/Data/workdata/707116/SIDS_RDD/R scripts")

need <- c('tidyverse','glue','readstata13','stringi','stargazer',
          'ggplot2','readxl','dplyr', 'lubridate', 'lfe','rddensity',
          'rdrobust', 'rdd')

lapply(need,require, character.only=TRUE)

rm(list=ls())
##

mortality.full <- read.dta13(file="../SIDS_analysis_final_full.dta") 

mortality.full$dif2 <- as.numeric(mortality.full$birthday - dmy("31-12-1992"))

t1 <- DCdensity(mortality.full$dif2, 
          cutpoint = 0, 
          ext.out = TRUE, 
          bw=1799, bin=1)

mortality.agg <- mortality.full %>%
  group_by(dif) %>%
  summarise(freq = n())



g1 <- mortality.agg %>%
      filter(abs(dif) < 54) %>%
      ggplot(aes(x=dif,y=freq))+
      geom_bar(stat="identity") + 
      theme_classic() + 
       ylim(c(0,8000)) +
      labs(x="Born after December 1991", y="Frequency") + 
      theme(axis.title = element_text(size=15)) + 
      theme(text=element_text(size=16, family="Georgia"))


mo1 <- loess(data=mortality.agg[mortality.agg$dif<=0,], 
             formula = freq ~ dif, degree=2, span=1)


mo2 <- loess(data=mortality.agg[mortality.agg$dif>0,], 
             formula = freq ~ dif, degree=2, span=1)

predict.mo1 <- predict(mo1,se=TRUE, newdata=mortality.agg)
predict.mo2 <- predict(mo2,se=TRUE, newdata=mortality.agg)

mortality.agg <- mortality.agg %>%
  mutate(yhat1 = predict.mo1$fit, 
         yhat2 = predict.mo2$fit, 
         ub1 = predict.mo1$fit + 1.96*predict.mo1$se.fit, 
         lb1 = predict.mo1$fit - 1.96*predict.mo1$se.fit, 
         ub2 = predict.mo2$fit + 1.96*predict.mo2$se.fit, 
         lb2 = predict.mo2$fit - 1.96*predict.mo2$se.fit, 
  )

g2 <- ggplot(data=mortality.agg) + 
  geom_vline(xintercept=0, linetype="dashed",color="black") + 
  geom_point(aes(x=dif,y=freq), colour='#636363', size=2) + 
  geom_line(aes(x=dif,y=yhat1), size=1, alpha=1,color='#000000') + 
  geom_line(aes(x=dif,y=yhat2), size=1, alpha=1,color='#000000') + 
  geom_line(aes(x=dif,y=ub1),colour='gray50', linetype='solid') + 
  geom_line(aes(x=dif,y=ub2),colour='gray50', linetype='solid') + 
  geom_line(aes(x=dif,y=lb1),colour='gray50', linetype='solid') + 
  geom_line(aes(x=dif,y=lb2),colour='gray50', linetype='solid') + 
  theme_classic() + 
  ylim(c(0,8000)) +
  labs(x="Born after December 1991", y="Frequency") + 
  theme(axis.title = element_text(size=15)) + 
  theme(text=element_text(size=16, family="Georgia"))

g3 <-  gridExtra::arrangeGrob(g1,g2)

ggsave('mc_crary.eps', g3, width=15,heigh=10)


mc.test.sim <- function(x){
  
  mc.test <- DCdensity(mortality.full$dif, 
                       cutpoint = x, 
                       ext.out = TRUE, 
                       bw=54, bin=1) 
  p.val <- mc.test$p
  return(p.val)
}

out1 <- lapply(seq(-100,100), mc.test.sim)

out2 <- do.call(rbind.data.frame,out1) %>%
  rename(pval=1) %>%
  mutate(dif=seq(-100,100)) %>%
  mutate(sig = case_when(pval<0.05 ~ 1, TRUE ~ 0))


round(mean(out2$sig),2)
