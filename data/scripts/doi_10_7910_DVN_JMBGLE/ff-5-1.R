require(ggplot2)

load('data/discretion-raters.Rda')

# FIGURE 5.1: The expectations gap is larger for domestic affairs. 
gap = ggplot(ranks,aes(x=Rank,y=Topic)) + 
  geom_point(aes(colour=Rater)) + 
  geom_line(colour='#dddddd') +
  theme_bw() + xlab('Power Rank') + ylab('') +
  scale_colour_manual(values=c('#000000','#7f7f7f')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),legend.title=element_blank(),legend.position='none',
        axis.text.x=element_text(colour="black"),
        axis.text.y=element_text(colour="black"))
# -----------------------------------------------
