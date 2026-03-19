#set working directory
setwd("C:/Users/user/Desktop/quora_climate_change")



#Loading necessary packages
library(stm) #structural topic modeling package
library(MASS)#package for negative binomial regression model
library(rms)#package for vif test
library(glmmADMB)
library(lme4)

#load the data
data<-read.csv("quora_data.csv")

#extract the answer texts
answer_text<-as.character(data$answer_text)

#load the stopword list
cstop<-readLines("stopwords.txt")


#preprocessing 
answer_text=gsub("^ ","",answer_text)
answer_text=gsub("[^a-zA-Z]"," ",answer_text)
answer_text=gsub("\""," ",answer_text)
answer_text=gsub("\'","",answer_text)
answer_text=gsub("\\/","",answer_text)
answer_text=gsub("?","",answer_text)
processed<-textProcessor(answer_text, metadata=data,stem=TRUE,language="en",customstopwords = cstop)
out<-prepDocuments(processed$documents,processed$vocab,processed$meta)
docs<-out$documents
vocab<-out$vocab
meta<-out$meta

#structural topic modeling processes with four, eight, ten and twelve topics
stm_4<-stm(out$documents,out$vocab,K=4,max.em.its = 300,data=out$meta,seed=2013)
stm_8<-stm(out$documents,out$vocab,K=8,max.em.its = 300,data=out$meta,seed=2013)
stm_10<-stm(out$documents,out$vocab,K=10,max.em.its = 300,data=out$meta,seed=2013)
stm_12<-stm(out$documents,out$vocab,K=12,max.em.its = 300,data=out$meta,seed=2013)

#printing the 15 most frequent terms of the four models
plot.STM(stm_4,type="labels",n=15)
plot.STM(stm_8,type="labels",n=15)
plot.STM(stm_10,type="labels",n=15)
plot.STM(stm_12,type="labels",n=15)

#printing topic proportions in the whole corpus, for each model
theta_4<-stm_4$theta
proportion_4<-colSums(theta_4)
proportion_4<-proportion_4/10393#the whole corpus contains 10393 articles

theta_8<-stm_8$theta
proportion_8<-colSums(theta_8)
proportion_8<-proportion_8/10393#the whole corpus contains 10393 articles

theta_10<-stm_10$theta
proportion_10<-colSums(theta_10)
proportion_10<-proportion_10/10393#the whole corpus contains 10393 articles

theta_12<-stm_12$theta
proportion_12<-colSums(theta_12)
proportion_12<-proportion_12/10393#the whole corpus contains 10393 articles

#regression modeling

#auxiliary features
auxiliary_data<-cbind(meta$upvote_number,meta$a_followers,meta$answer_length,meta$question_follower,meta$image_number,meta$cadays)

#transformation of textual features into dummy variable
theta_10_<-theta_10

for (i in 1:10)
{
  #theta_10_[,i]=log(theta_10[,i])
  theta_10_[,i][which(theta_10[,i]>=0.2)]<-1
  theta_10_[,i][which(theta_10[,i]<0.2)]<-0
  
}

theta_10__<-theta_10
for (i in 1:10)
{
  theta_10__[,i]=log(theta_10[,i])

}

year<-meta$answer_date
year<-as.character(year)
year2<-gsub("/\\w+/\\w+","",year)

#consolidation of auxiliary features and textual features
data_regression<-cbind(theta_10_,auxiliary_data)
data_regression<-data.frame(data_regression)
names(data_regression)<-c("topic1","topic2","topic3","topic4","topic5","topic6","topic7","topic8","topic9","topic10","upvote_number","author_followers","answer_length","question_followers","image_number","existing_days")
data_regression<-cbind(data_regression,year2)



data_regression_interaction<-cbind(theta_10__,auxiliary_data)
data_regression_interaction<-data.frame(data_regression_interaction)
names(data_regression_interaction)<-c("topic1","topic2","topic3","topic4","topic5","topic6","topic7","topic8","topic9","topic10","upvote_number","author_followers","answer_length","question_followers","image_number","existing_days")


data_regression_interaction<-cbind(data_regression_interaction,year2)


#printing regression modeling results
summary(poisson.model<-glm(upvote_number~topic1+topic2+topic3+topic4+topic5+topic6+topic7+topic8+topic9+topic10+ 
                 scale(author_followers)+  scale(answer_length)+scale(image_number)+scale(question_followers)+
                 offset(log(data_regression$existing_days+1)),data=data_regression, family=poisson))

summary(nb.model<-glm.nb(upvote_number~topic1+topic2+topic3+topic4+topic5+topic6+topic7+topic8+topic9+topic10+ 
                           scale(author_followers)+  scale(answer_length)+scale(image_number)+scale(question_followers)+
                           offset(log(data_regression$existing_days+1)),data=data_regression))

summary(nb2.model<-glm.nb(upvote_number~topic1+topic2+topic3+topic4+topic5+topic6+topic7+topic8+topic9+topic10+ 
                           scale(author_followers)+  scale(answer_length)+scale(image_number)+scale(question_followers)+
                           offset(log(data_regression$existing_days+1)),data=data_regression_interaction))




summary(nb3.model<-glm.nb(upvote_number~topic1+topic2+topic3+topic4+topic5+topic6+topic7+topic8+topic9+topic10+ 
                           log(author_followers+1)+  log(answer_length)+image_number+log(question_followers+1)+
                           log(data_regression$existing_days+1)
                            #year2-1
                          ,data=data_regression))

summary(nb4.model<-glm.nb(upvote_number~topic1+topic2+topic3+topic4+topic5+topic6+topic7+topic8+topic9+topic10+ 
                            log(author_followers+1)+  log(answer_length)+image_number+log(question_followers+1)+
                            log(data_regression$existing_days+1)
                          #year2-1
                          ,data=data_regression_interaction))


nb_fix <- glmer.nb(upvote_number~#topic1+topic2+topic3+topic4+topic5+topic6+topic7+topic8+topic9+topic10+ 
                                  log(author_followers+1)+  log(answer_length)+image_number+log(question_followers+1)+
                                  (1|year2),
                                  data=data_regression_interaction,verbose=TRUE)
summary(nb_fix)


#printing measures of fit
#log-likelihood
logLik(poisson.model)
logLik(nb.model)

#AIC
AIC(poisson.model)
AIC(nb.model)

#BIC
BIC(poisson.model)
BIC(nb.model)

#printing vif for multicollinearity test
vif(poisson.model)
vif(nb2.model)
