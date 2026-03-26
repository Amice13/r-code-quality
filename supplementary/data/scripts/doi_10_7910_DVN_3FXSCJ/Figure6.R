#Set up lists to store output
results_list<- c()
outputlist<-c()

#Main model
main.model<- dummy_comm_recoded~ factor(Publication) +N +spellcorrect +fulltext_count + fulltext_pos_sent_score +fulltext_neg_sent_score +  factor(Article_dummies) + factor(Year) + factor(dayofweek) + Year*keytopic

#Iterate over topics
for (i in 1:ntopic){
  topic.star <-paste0('topic', i)
  datastar<- agency_files %>% mutate(keytopic = eval(as.name(topic.star)))
  results <- glm(main.model, family = binomial(link = 'logit'), data= datastar)
  mylist <- list(Years=seq(1890,1934), keytopic=c(1))
  margins<-emmip(results,keytopic~Year,at=mylist, CIs=TRUE, type = "response", plotit=FALSE)
  margins$topic<- topic.star
  outputlist[[i]] <- margins
  results_list[[i]] <- results
}

#combine outputs
output_combined<- bind_rows(outputlist, .id = "column_label")

#order by coefficient size
ordered_topic<- output_combined %>% 
  group_by(topic)%>%
  summarize(max_coef = max(yvar))%>%
  arrange(desc(max_coef))%>%
  pull(topic)

#Plot

output_combined%>%
  mutate(topic = factor(topic, levels = ordered_topic))%>%
  mutate(term = recode(topic, `topic1` = "Business opinion", 
                       `topic2` = "International news", 
                       `topic3` = "Business outlook",
                       `topic4` = "Oil + gas",
                       `topic5` = "Politics",
                       `topic6` = "Banking",
                       `topic7` = "Stocks",
                       `topic8` = "Regulation",
                       `topic9` = "Local\n organizations",
                       `topic10` = "Railroads",
                       `topic11` = "Courts",
                       `topic12` = "Labor")) %>%
  ggplot(aes(x=Year, y=yvar))+
  geom_point(size =.5)+
  #geom_smooth(method = "lm", color = "blue")+
  geom_errorbar(aes(ymin =LCL, ymax = UCL), width =0)+
  geom_hline(yintercept=0, color = "black")+
  theme_minimal()+
  ylab("Predicted probability")+
  xlab("")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45, hjust =1))+
  facet_wrap(~term, nrow=2)
