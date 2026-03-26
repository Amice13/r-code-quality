
##############
# Any Agency
##############

#main formula + outputs
f.a<- agency~ factor(Publication) + spellcorrect + N + fulltext_count + fulltext_neg_sent_score + fulltext_pos_sent_score + factor(Article_dummies) + factor(Year) + factor(dayofweek)
outputa<-matrix(data=NA,nrow=ntopic,ncol=3)
fullresultsa<-c()

#run analysis
for (i in 1:ntopic){
  topic.star <-paste0('topic', i)
  model<-update(f.a,paste("~ . +", topic.star))
  results <- glm(model, family = binomial(link = 'logit'), data= agency_files)
  outputa[i, ] <-c(topic.star, as.numeric(summary(results)$coefficients[, 1][topic.star]), as.numeric(summary(results)$coefficients[, 2][topic.star]))
  fullresultsa[[i]]<-results
}

#save output for visualization
outputa<-as_tibble(outputa)
colnames(outputa)<-c("topic", "coef", "se")
outputa$se<-as.numeric(outputa$se)
outputa$coef<-as.numeric(outputa$coef)

##############
#Mental states
##############

#main formula + outputs
f.m<- dummy_mental_recoded~ factor(Publication) + spellcorrect + N + fulltext_count + fulltext_neg_sent_score + fulltext_pos_sent_score + factor(Article_dummies) + factor(Year) + factor(dayofweek)
outputm<-matrix(data=NA,nrow=ntopic,ncol=3)
fullresultsm<-c()

#loop thorugh model
for (i in 1:ntopic){
  topic.star <-paste0('topic', i)
  model<-update(f.m,paste("~ . +", topic.star))
  results <- glm(model, family = binomial(link = 'logit'), data= agency_files)
  outputm[i, ] <-c(topic.star, as.numeric(summary(results)$coefficients[, 1][topic.star]), as.numeric(summary(results)$coefficients[, 2][topic.star]))
  fullresultsm[[i]]<-results
  
}

#prepare data for visualization
outputm<-as_tibble(outputm)
colnames(outputm)<-c("topic", "coef", "se")
outputm$se<-as.numeric(outputm$se)
outputm$coef<-as.numeric(outputm$coef)

##############
#Communication
##############

#main formula + outputs
f.c<- dummy_comm_recoded~ factor(Publication) + spellcorrect + N + fulltext_count + fulltext_neg_sent_score + fulltext_pos_sent_score +  factor(Article_dummies) + factor(Year) + factor(dayofweek)
outputc<-matrix(data=NA,nrow=ntopic,ncol=3)
fullresultsc<-c()

#loop through
for (i in 1:ntopic){
  topic.star <-paste0('topic', i)
  model<-update(f.c,paste("~ . +", topic.star))
  results <- glm(model, family = binomial(link = 'logit'), data= agency_files)
  outputc[i, ] <-c(topic.star, as.numeric(summary(results)$coefficients[, 1][topic.star]), as.numeric(summary(results)$coefficients[, 2][topic.star]))
  fullresultsc[[i]]<-results
  
}

#save
outputc<-as_tibble(outputc)
colnames(outputc)<-c("topic", "coef", "se")
outputc$se<-as.numeric(outputc$se)
outputc$coef<-as.numeric(outputc$coef)


#######################
#Combine + Visualize
#######################

topics<- paste0("topic", seq(1:ntopic))
outputc$model<- "Speech verb"
outputm$model<- "Intentionality verb"
outputa$model<- "Any agentic verb"

combined_output<-rbind(outputm, outputc, outputa)

combined_output<- combined_output %>%
  mutate(term = recode(topic, `topic1` = "Business opinion", 
                       `topic2` = "International news", 
                       `topic3` = "Business outlook",
                       `topic4` = "Oil + gas",
                       `topic5` = "Politics",
                       `topic6` = "Banking",
                       `topic7` = "Stocks",
                       `topic8` = "Regulation",
                       `topic9` = "Local organizations",
                       `topic10` = "Railroads",
                       `topic11` = "Courts",
                       `topic12` = "Labor")) 

ordered_topic<- combined_output %>% 
  filter(model == "Any agentic verb") %>%
  arrange(-coef)%>%
  pull(term)

combined_output %>%
  mutate(term = ordered(term, levels = rev(ordered_topic)))%>%
  ggplot(aes(x= term, y= coef, shape = model, color = model))+
  geom_point(position = position_dodge(0.5))+
  geom_errorbar(aes(ymin = coef- 2*se, ymax = coef+2*se), width = 0, position = position_dodge(0.5) )+
  coord_flip()+
  geom_hline(yintercept=0, color = "black", linetype = 2)+
  ylab("")+
  xlab("Coefficient estimate")+
  scale_shape_manual(breaks = c("Speech verb", "Intentionality verb", "Any agentic verb"), values=c(15,16, 17) )+
  scale_colour_grey(breaks = c("Speech verb", "Intentionality verb", "Any agentic verb"), start = .1, end = .8 )+
  theme_minimal()+
  theme(legend.title = element_blank(), legend.position="right", 
        legend.background =element_blank(),
        legend.text=element_text(size=8),
        legend.key.size = unit(0.5, "cm"),
        axis.title.x =  element_text(size =8))


