
#############################################################
#Panel A: Count of articles with any active or passive action 
############################################################

summary<-agency_entities %>%
  group_by(Pubname, Year, Deptype)%>%
  summarize(N = n(), .groups = 'drop') %>%
  mutate(Deptype = recode(Deptype, `nsubj` = "Active subject",
                          `nsubj:pass` = "Passive subject"))

p1<- summary %>%
  ggplot(aes(x=Year, y = N, group = Pubname)) +
  geom_line(aes(linetype = Pubname))+
  ylab("Count, Active and Passive Subjects")+
  xlab("")+
  scale_fill_manual(values = c("darkgrey", "black"))+
  scale_y_continuous(label=comma)+
  theme_minimal()+
  facet_grid(~Deptype)+
  theme(axis.text.x=element_text(angle=45, hjust =1),
        axis.title.y = element_text(size = 9))


###############################################################
#Panel B: Percent of articles with any intentionality or communication 
##############################################################

#Summarize by Article
articles <- agency_entities %>%
  filter(Deptype == "nsubj")%>%
  group_by(Year, Pubname, Filename)%>%
  select(Filename, Pubname, Year, mental_recoded, comm_recoded)%>%
  summarise_each(funs(max(., na.rm=T)))

#articles a year with either
articles_long <- articles %>%
  gather(variable, value, mental_recoded:comm_recoded)

articles_with_agency<- articles_long %>%
  group_by(Year, Pubname, variable)%>%
  summarise(mean = mean(value, na.rm =T), 
            se = sd(value)/sqrt(length(value)))%>%
  mutate(variable = recode(variable, `comm_recoded` = "Organizational speech",
                           `mental_recoded` = "Organizational Intentionality"))

p2<-articles_with_agency %>%
  ggplot(aes(x=Year, y = mean, group = Pubname)) +
  geom_line(aes(linetype=Pubname))+
  geom_ribbon(aes(ymin=mean- se,  ymax=mean+ se), alpha = .3) +
  facet_grid(~variable, scales = "free_y")+
  ylab("Verb Frequency (%)")+
  xlab("")+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x=element_text(angle=45, hjust =1), 
        axis.title.y = element_text(size = 9))


############
#Combine
##############

plot.row<- plot_grid(p1 +theme(legend.position="none"),
                     p2 + theme(legend.position="none"),
                     labels = c('A', 'B'), 
                     align = "vh")

legend <-get_legend(p2) 

p<- plot_grid(plot.row, legend, ncol=1,
              rel_heights = c(1, .1))

p
