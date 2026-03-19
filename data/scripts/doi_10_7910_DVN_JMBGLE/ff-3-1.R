require(ggplot2)

load('data/actions-topic-year.Rda')

# FIGURE 3.1: Executive action is everywhere. 
tiles_ua = ggplot() + geom_tile(data=act_dt, aes(x=year, y=topic, fill=Action),colour='black') + 
  scale_x_continuous(position = "bottom",breaks=c(1989,1993,1997,2001,2005,2009,2013,2017)) + 
  scale_fill_manual(values = c("#d4d4d4", "#000000","#7f7f7f", "#000000")) +
  theme_bw() + theme(panel.grid.major = element_blank(),panel.border = element_blank(),legend.position="none",panel.background = element_blank(),
                     axis.title.y=element_blank(), axis.title.x=element_blank(),
                     axis.text.x=element_text(colour="black"),
                     axis.text.y=element_text(colour="black")) + 
  annotate("text", x=1990.5, y=4, label= "Bush", angle=35, alpha=1, color = 'white') +
  annotate("text", x=1996.5, y=4, label= "Clinton", angle=35, alpha=1, color = 'white') +
  annotate("text", x=2004.5, y=4, label= "W. Bush", angle=35, alpha=1, color = 'white') +
  annotate("text", x=2012.5, y=4, label= "Obama", angle=35, alpha=1, color = 'white') +
  annotate("text", x=2018.5, y=4, label= "Trump", angle=35, alpha=1, color = 'white')
# -----------------------------------------------
