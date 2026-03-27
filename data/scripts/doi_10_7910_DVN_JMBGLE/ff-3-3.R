require(ggplot2)

load('preamble-type-year.Rda')

# Figure 3.3: Less formal presidential directives have more symbolic language. 
line_preamble = ggplot() + 
  geom_rect(mapping=aes(xmin=1989, xmax=1992, ymin=0, ymax=550, fill='R',alpha='1'))+
  geom_rect(mapping=aes(xmin=1993, xmax=2000, ymin=0, ymax=550, fill='D',alpha='1'))+
  geom_rect(mapping=aes(xmin=2001, xmax=2008, ymin=0, ymax=550, fill='R',alpha='1'))+
  geom_rect(mapping=aes(xmin=2009, xmax=2016, ymin=0, ymax=550, fill='D',alpha='1'))+
  geom_rect(mapping=aes(xmin=2017, xmax=2020, ymin=0, ymax=550, fill='R',alpha='1'))+
  annotate("text", x=1990.7, y=500, label= "Bush",  alpha=1, color = 'white') +
  annotate("text", x=1997, y=500, label= "Clinton", alpha=1, color = 'white') +
  annotate("text", x=2004.5, y=500, label= "W. Bush", alpha=1, color = 'white') +
  annotate("text", x=2012.5, y=500, label= "Obama", alpha=1, color = 'white') +
  annotate("text", x=2018.5, y=500, label= "Trump", alpha=1, color = 'white') +
  geom_line(data=by_year, aes(x=year, y=preamble_words, color=type)) + 
  scale_x_continuous(position = "bottom",breaks=c(1989,1993,1997,2001,2005,2009,2013,2017)) + 
  scale_color_manual(values=c('black','white')) + 
  scale_fill_manual(values=c("#7f7f7f","#7f7f7f")) + scale_alpha_manual(values = c(.8,.8)) +
  theme_bw() + theme(panel.grid.major = element_blank(),panel.border = element_blank(),
                     legend.position="none",panel.background = element_blank(),axis.title.y=element_blank(), axis.title.x=element_blank(),
                     axis.text.x=element_text(colour="black"),
                     axis.text.y=element_text(colour="black"))
# -----------------------------------------------
