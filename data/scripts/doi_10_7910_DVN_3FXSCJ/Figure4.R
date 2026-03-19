#number of topics
ntopic = 12

#word scores
td_beta <- tidy(stm12)

td_beta<-td_beta %>%
  filter(!term == "")

#get frex words
labels<- labelTopics(stm12, seq(1,ntopic), n=7)

df <- data.frame(matrix(unlist(labels$frex), nrow=7, byrow=T))

df<-as_tibble(df)

df1 <-df %>%
  gather(key = "topic", value = "term")%>%
  mutate(topic = str_remove(topic, "X")) %>%
  mutate(topic = as.numeric(topic))

##########
#Visualize
##########

df2<-df1 %>% 
  left_join(as_tibble(td_beta), by = c('topic', 'term') ) %>%
  mutate(topic = recode(topic, `1` = "Business opinion", 
                        `2` = "International news", 
                        `3` = "Business outlook",
                        `4` = "Oil + gas",
                        `5` = "Politics",
                        `6` = "Banking",
                        `7` = "Stocks",
                        `8` = "Regulation",
                        `9` = "Local organizations",
                        `10` = "Railroads",
                        `11` = "Courts",
                        `12` = "Labor")) %>%
  mutate(topic = factor(topic, levels = c("Banking", "Business outlook", "Oil + gas", "Railroads", "Stocks", "Courts", "Labor","Politics","Regulation", "Business opinion", "International news", "Local organizations")))%>%
  mutate(topiccolor = ifelse(topic %in%  c("Labor", "Regulation", "Courts", "Politics"), 0, ifelse(topic %in% c("Local organizations", "International news", "Business opinion"), -1, 2)))


df2 %>%
  group_by(topic, term) %>%  
  arrange(desc(beta)) %>%
  ungroup() %>%
  mutate(term = factor(paste(term, topic, sep = "__"), levels = rev(paste(term, topic, sep = "__")))) %>% 
  ggplot(aes(term, beta, fill = as.factor(topiccolor))) +
  geom_col(alpha = 0.8, show.legend = FALSE, color = "black", size =.2) +
  facet_wrap(~ topic, scales = "free") +
  #scale_fill_manual(values = c("white", "lightgrey", "grey", "darkgrey", "black"))+
  theme_minimal()+
  coord_flip() +
  ylab("Frequency")+
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x))+
  scale_fill_grey(start = 1, end = 0)+
  labs(x = NULL, y = expression(beta))+
  theme(axis.text.x=element_text(angle=45, hjust =1))
