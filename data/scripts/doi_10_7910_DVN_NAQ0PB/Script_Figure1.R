
#################################################################################################################
# Replication file for "Group Appeals of Parties in Times of Economic and Identity Conflicts and Realignment" 
# published in Political Studies
# by Simon Stuckelberger and Anke Tresch 
#################################################################################################################


rm(list=ls())

#install not yet installed packages and load all packages 
pacman::p_load(dplyr, ggplot2, ggthemes)


# Figure 1 ----------------------------------------------------------------

load("data_Figure1.Rdata")


pdf("Figure1.pdf")
data_Figure1 %>%
  ggplot(data=., aes(x=reorder(Group,-N), y=N,fill=Type)) +
  geom_bar(position=position_stack(),stat="identity",width=0.75) + theme_hc() +
  ggtitle("") + labs(x="",y="Number of group appeals",fill="")+
  theme(axis.ticks = element_blank()) + scale_fill_grey(breaks=c("Positive group appeals","Negative group appeals")) + scale_y_continuous(breaks=seq(0,600,100),limits=c(0,600)) + 
  theme(legend.position="bottom",legend.margin=margin(t=-1,unit='cm'),legend.text=element_text(size=9)) + theme(axis.text.x = element_text(angle=45,size=9,hjust=1,vjust=1.2)) +
  facet_grid(.~Category,scales="free",space="free") + theme(text=element_text(family="serif"))+ theme(axis.title.y = element_text(margin = margin(r = 10)))
dev.off()





