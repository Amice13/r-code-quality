rm(list=ls())
library(tidytext)
library(tidyr)
library(dplyr)
library(forcats)
library(ggplot2)

#Set working directory (change this to your local directory)
setwd('~/Dropbox/cm_project2/KenwickMaxey_JCR2024_replication/')
load('survey2.RData')

##############################################################################
###Analysis of Word Frequency (tf-idf)
##############################################################################

#Transform data to long for analysis
responses <- pivot_longer(text,!id, names_to = "treatment",values_to = "text")
responses$treatment <- as.factor(responses$treatment)
responses <- responses[!responses$text=="",]

#generate counts for tf-idf
responses_count <- responses %>% 
  unnest_tokens(word, text) %>% 
  count(treatment,word,sort=T) 
total_words <- responses_count %>% 
  group_by(treatment) %>% 
  summarize(total=sum(n))
responses_count <- left_join(responses_count,total_words)

#drop a missing assignment
responses_count <- responses_count[!is.na(responses_count$treatment),] 

#generate metrics
responses_count_t <- responses_count %>% 
  bind_tf_idf(word,treatment,n)

#object for plotting
test <- responses_count_t %>%
  group_by(treatment) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup()  %>% 
  arrange(treatment,tf_idf)

####
#Fig A3
####

pdf("figures/figA3.pdf",height=9,width=10)
par(mfrow=c(2,2),mar=c(4,8,3,3))
barplot(test$tf_idf[test$treatment=="dom_loss"],
        horiz=T, xlim=c(0,0.0055),
        names=test$word[test$treatment=="dom_loss"],
        las=1,main="Domestic Loss")
mtext(side = 1, "tf-idf", line =2.4,cex=1) 
barplot(test$tf_idf[test$treatment=="dom_retention"],
        horiz=T, xlim=c(0,0.0055),
        names=test$word[test$treatment=="dom_retention"],
        las=1,main="Domestic Retention")
mtext(side = 1, "tf-idf", line =2.4,cex=1) 
barplot(test$tf_idf[test$treatment=="intl_loss"],
        horiz=T, xlim=c(0,0.0055),
        names=test$word[test$treatment=="intl_loss"],
        las=1,main="International Loss")
mtext(side = 1, "tf-idf", line =2.4,cex=1) 
barplot(test$tf_idf[test$treatment=="intl_retention"],
        horiz=T, xlim=c(0,0.0055),
        names=test$word[test$treatment=="intl_retention"],
        las=1,main="International Retention")
mtext(side = 1, "tf-idf", line =2.4,cex=1) 
dev.off()

##############################################################################
###Analysis of Sentiment
##############################################################################

#Generate bing sentiment scores
sent <- responses %>% 
  unnest_tokens(word, text) %>% 
  inner_join(get_sentiments("bing")) %>%
  count(id,treatment, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

#bootstrapped difference in means
set.seed(1989)
sims <- 2000
bootResults <- matrix(NA, nrow=sims, ncol=2)
bootMeans <- matrix(NA, nrow=sims, ncol=4)
for(ii in 1:sims){
  resample<-dplyr::sample_n(sent,size=nrow(text),replace=T)
  dom_loss <- mean(resample$sentiment[resample$treatment=="dom_loss"],na.rm=T)
  dom_retention <- mean(resample$sentiment[resample$treatment=="dom_retention"],na.rm=T)
  intl_loss <- mean(resample$sentiment[resample$treatment=="intl_loss"],na.rm=T)
  intl_retention <- mean(resample$sentiment[resample$treatment=="intl_retention"],na.rm=T)
  
  bootMeans[ii,1]<-dom_loss
  bootMeans[ii,2]<-dom_retention
  bootMeans[ii,3]<-intl_loss
  bootMeans[ii,4]<-intl_retention

  bootResults[ii,1]<- dom_loss - dom_retention
  bootResults[ii,2]<- intl_loss - intl_retention

}
#function for generating p-values
pvalue<-function(boots){
  min(1 - length(which(boots <= 0))/length(boots),
      1 - length(which(boots >= 0))/length(boots))*2
}

#Generate scores for NRC sentiment dictionary
nrc_dict <- get_sentiments("nrc")

sent_nrc <- responses %>% 
  unnest_tokens(word, text) %>% 
  inner_join(nrc_dict,relationship = "many-to-many") %>%
  count(id,treatment, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

#Bootstrap differences in means
set.seed(1989)
sims <- 2000
bootResults_dom <- matrix(NA, nrow=sims, ncol=9)
bootResults_intl <- matrix(NA, nrow=sims, ncol=9)

sent_nrc$positive <- NULL #Since captured in Bing sentiment
sent_nrc$negative <- NULL #Since captured in Bing sentiment
sent_nrc$id <- NULL

colnames(bootResults_dom) <- names(sent_nrc)[2:10]
colnames(bootResults_intl) <- names(sent_nrc)[2:10]

for(ii in 1:sims){
  resample<-dplyr::sample_n(sent_nrc,size=nrow(sent_nrc),replace=T)
  
  dom_loss <- subset(resample,treatment=="dom_loss")
  dom_ret <- subset(resample,treatment=="dom_retention")
  intl_loss <- subset(resample,treatment=="intl_loss")
  intl_ret <- subset(resample,treatment=="intl_retention")
  
  dom_loss_means <- apply(dom_loss[,2:10],2,mean)
  dom_ret_means <- apply(dom_ret[,2:10],2,mean)
  intl_loss_means <- apply(intl_loss[,2:10],2,mean)
  intl_ret_means <- apply(intl_ret[,2:10],2,mean)
  
  bootResults_dom[ii,]<- dom_loss_means - dom_ret_means
  bootResults_intl[ii,]<- intl_loss_means - intl_ret_means
  
}
####
#fig A4
####
pdf("figures/figA4.pdf",height=6.3,width=10.6)
par(mfrow=c(1,2),mar=c(5,10,3,3))
plot(NULL,
     xlim = (c(-1,1)), 
     ylim = c(10,95), 
     axes = F, xlab = NA, ylab = NA) 
abline(v=0,lty=1,col="orangered",lwd=1.4)
for(ii in 1:9){
  abline(h=ii*10,las=1,col="gray80")
  polygon(density(bootResults_intl[,ii],bw="sj")$x,density(bootResults_intl[,ii],bw="sj")$y+10*ii ,col=alpha("#a6bddb",.8),border="black",lwd=1.5)  
  if(pvalue(bootResults_intl[,ii])>0.001){
      text(1, ii*10+1,  
           bquote(p==.(round(pvalue(bootResults_intl[,ii]) ,3)))
           ,cex=1,col="black",pos=2)
    } else{
      text(1, ii*10+1,  
           "p<0.001"
           ,cex=1,col="black",pos=2) }
  }
axis(side=1, las=1,cex.axis=1,mgp=c(3,.8,0))
title("International Primes", cex.main=1)
mtext("Effect of Loss Treatment on Sentiment", side = 1, line=1.8,cex=.85) 
axis(2,at=seq(10,90,10),labels=colnames(bootResults_intl),las=2)

plot(NULL,
     xlim = (c(-1,1)), 
     ylim = c(10,95), 
     axes = F, xlab = NA, ylab = NA) 
abline(v=0,lty=1,col="orangered",lwd=1.4)
for(ii in 1:9){
  abline(h=ii*10,las=1,col="gray80")
  polygon(density(bootResults_dom[,ii],bw="sj")$x,density(bootResults_dom[,ii],bw="sj")$y+10*ii ,col=alpha("#a6bddb",.8),border="black",lwd=1.5)  
  if(pvalue(bootResults_dom[,ii])>0.001){
    text(1, ii*10+1,  
         bquote(p==.(round(pvalue(bootResults_dom[,ii]) ,3)))
         ,cex=1,col="black",pos=2)
  } else{
    text(1, ii*10+1,  
         "p<0.001"
         ,cex=1,col="black",pos=2) }
}
axis(side=1, las=1,cex.axis=1,mgp=c(3,.8,0))
title("Domestic Primes", cex.main=1)
mtext("Effect of Loss Treatment on Sentiment", side = 1, line=1.8,cex=.85) 
axis(2,at=seq(10,90,10),labels=colnames(bootResults_dom),las=2)
dev.off()
