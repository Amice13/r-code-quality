require(ggplot2)

load('type-topic-year.Rda')
load('type-topic-year.Rda')


# Figure 3.2: Executive action is diverse, but not consistently going up. 
types_ua = ggplot() + geom_tile(data=type_dt, aes(x=year, y=type_consolidated, fill=party,alpha=log(action+1)),colour='black') + 
  scale_x_continuous(position = "bottom",breaks=c(1989,1993,1997,2001,2005,2009,2013,2017)) + 
  scale_fill_manual(values=c("#000000","#000000")) +
  theme_bw() + theme(panel.grid.major = element_blank(),panel.border = element_blank(),legend.position="none",panel.background = element_blank(),axis.title.y=element_blank(),
                     axis.title.x=element_blank(),axis.text.x=element_text(colour="black"),
                     axis.text.y=element_text(colour="black"))

line_ua = ggplot() + geom_line(data=total, aes(x=year, y=action, color='black')) + 
  scale_x_continuous(position = "bottom",breaks=c(1989,1993,1997,2001,2005,2009,2013,2017)) + 
  scale_color_manual(values='black') +
  theme_bw() + theme(panel.grid.major = element_blank(),panel.border = element_blank(),legend.position="none",panel.background = element_blank(),
                     axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(colour="black"),
                     axis.text.y=element_text(colour="black"))
# -----------------------------------------------
