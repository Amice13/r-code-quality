
####### Homophily + TimeShocks 
timeshockMCsum = readRDS(file="../Data/H+TSummary.rds")

ggplot(timeshockMCsum, aes(timeseries.length, mean, group=timeseries.length)) +
  geom_boxplot(size=0.4, outlier.size=0.15) +
  ggtitle("Data with Time shock")+
  xlab("Length of Timeseries") +
  ylab("Contagion Signal") +
  ylim(-0.15,0.15) +
  theme_light()

ggsave("../Plots/Appendix-Fig 2b.png") 

##### Contagion signal vs Network densities
allnet.summary = readRDS("../Data/allnetSummary.Rds")

ggplot(allnet.summary, aes(net.den,mean, group=net.den)) +
  geom_boxplot(size=0.4, outlier.size=0.15) +
  xlab("Network Density") +
  ylab("Contagion Signal") +
  ggtitle("Network Density Effect on Contagion Signal")+
  theme_light()

ggsave("../Plots/Appendix-Fig 2c.png") 