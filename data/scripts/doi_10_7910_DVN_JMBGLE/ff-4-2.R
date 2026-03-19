require(ggplot2)

load('mde-term-months.Rda')

# Figure 4.2: Less effectual orders peak near the presidential election. 
ff_cycles = ggplot(term_months, aes(x=term_month, y=prop_mde, size=log(action_count))) + 
  geom_point(alpha=0.5) + geom_smooth(span=0.4,colour='#000000',fill='#dddddd') + scale_size_continuous(range=c(1,5)) +
  scale_x_continuous(position = "bottom",breaks=c(6,12,18,24,30,36,42,48)) +
  annotate("text", x=19.8, y=0.3, label= "Midterms") +
  annotate("text", x=43.1, y=0.3, label= "Presidential") +
  geom_segment(aes(x = 23, y = 0.285, xend = 23, yend = 0.31),size=.7,colour='#000000') +
  geom_segment(aes(x = 47, y = 0.285, xend = 47, yend = 0.31),size=.7,colour='#000000') +
  xlab("Month in Term") + ylab("Prop. with Minimal Direct Effects") +
  theme_bw() + theme(panel.grid.major = element_blank(),panel.border = element_blank(),legend.position="none",panel.background = element_blank(),
                     axis.text.x=element_text(colour="black"),
                     axis.text.y=element_text(colour="black"))
# -----------------------------------------------

load('executive-actions-1989-2021.Rda')

# Results In-text: mde, time in term, and discretion
logitmfx(mde~lame_duck+subj_discretion+near_preselect,data=ua,clustervar1='topic_consolidated')
logitmfx(mde~president+lame_duck+subj_discretion+near_preselect,data=ua,clustervar1='topic_consolidated')
logitmfx(mde~president+topic_consolidated+lame_duck+subj_discretion+near_preselect,data=ua,clustervar1='topic_consolidated')
# -----------------------------------------------


