summary<-agency_files %>%
  group_by(Pubname, Year)%>%
  summarize(N = n(), .groups = 'drop')

#Figure 1
summary %>%
  ggplot(aes(x=Year, y = N, group = Pubname)) +
  geom_col(aes(fill = Pubname))+
  ylab("Article Count")+
  xlab("")+
  scale_fill_manual(values = c("darkgrey", "black"))+
  scale_y_continuous(label=comma)+
  theme_minimal()+
  theme(legend.title = element_blank(), legend.position = "bottom")