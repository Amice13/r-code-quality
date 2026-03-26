#All entities: Action
agency_entities %>%
  filter(Deptype =="nsubj")%>%
  mutate(subject_action = paste(Entity_lemma, lemma, sep = "_"))%>%
  count(subject_action) %>%    
  mutate(prop = prop.table(n))%>%
  arrange(-n)%>%
  top_n(15)

#Named entities: Action
agency_entities %>%
  filter(Deptype =="nsubj" & NamedEntity ==1)%>%
  mutate(subject_action = paste(Entity_lemma, lemma, sep = "_"))%>%
  count(subject_action) %>%    
  mutate(prop = prop.table(n))%>%
  arrange(-n)%>%
  top_n(15)

#Passive action
agency_entities %>%
  filter(Deptype =="nsubj:pass")%>%
  mutate(subject_action = paste(Entity_lemma, Verb, sep = "_"))%>%
  count(subject_action) %>%    
  mutate(prop = prop.table(n))%>%
  arrange(-n)%>%
  top_n(15)

#All Intentionality
agency_entities %>%
  filter(Deptype =="nsubj" &  mental_recoded ==1)%>%
  mutate(subject_action = paste(Entity_lemma, lemma, sep = "_"))%>%
  count(subject_action) %>%    
  mutate(prop = prop.table(n))%>%
  arrange(-n)%>%
  top_n(15)

#All Speech
agency_entities %>%
  filter(Deptype =="nsubj" &  comm_recoded ==1)%>%
  mutate(subject_action = paste(Entity_lemma, lemma, sep = "_"))%>%
  count(subject_action) %>%    
  mutate(prop = prop.table(n))%>%
  arrange(-n)%>%
  top_n(15)

