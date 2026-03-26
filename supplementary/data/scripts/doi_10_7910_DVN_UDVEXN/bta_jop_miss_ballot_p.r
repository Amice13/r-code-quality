##########
#### Assessing Ballot Structure and Split Ticket Voting
#### Barnes, Tchintian, Alles
#### Journal of Politics, forthcoming.
##########

#### updated: 06/25/2016

### Plot
### Data

ranef.store <- merge(unique(subset(mod.dat, select=c(party.id,party.name))), ranef.store, by="party.id")

party.labels <- merge(data.frame(party.id=unique(ranef.store$party.id)),
                      subset(parties, select=c(party.number,fig.label)),
                      by.x="party.id", by.y="party.number", all.x=T)


### Plot
### Title

plot.title <- "Robustness Tests to Detect the Prevalence of Missing Ballots"
plot.subtitle <- "Salta District (2011). Linear mixed-effects model: Nearest Neighbor and Thiessen Polygons."


### Plot
### Script

p <- ggplot(ranef.store, aes(as.factor(party.id), coef))

# Chart region
p <- p + theme_bw()
p <- p + theme(panel.background=element_rect(fill="#F0F0F0")) +
  theme(plot.background=element_rect(fill="#F0F0F0")) +
  theme(panel.border=element_rect(colour="#F0F0F0"))
p <- p + theme(panel.border=element_blank())

# Grid format
p <- p + theme(panel.grid.major.y=element_line(colour="#D0D0D0",size=.75),
               panel.grid.minor.y=element_blank(),
               panel.grid.major.x=element_blank())

## Axis
p <- p + scale_y_continuous(minor_breaks=0,breaks=seq(-30,30,10),limits=c(-30,30))
p <- p + theme(axis.ticks=element_blank())
p <- p + ylab("Distribution of Voting Booths' Random Intercepts") + xlab("Party Number")

p <- p + theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=12,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=12,colour="#535353",face="bold",vjust=-.5))

p <- p + theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=16))

## Content
p <- p + geom_label(data=party.labels, aes(x=as.factor(party.id), label=fig.label), 
                    y=30, colour="#535353", fontface="bold", fill="#F0F0F0")
p <- p + geom_boxplot(fill="#F0F0F0", outlier.colour = "red", outlier.shape = 1)
p <- p + facet_grid(gis.method ~ .)

p <- p + ggtitle(bquote(atop(bold(.(plot.title)), atop(.(plot.subtitle), ""))))


#### Storing
#### Plot

filename <- "BTA JOP Figure F1"

ggsave(p, file=paste(main.dir,"Results","Robustness Tests", paste(filename, "png", sep="."), sep="/"),
       width=12, height=8, units="in", dpi=800)



rm(p, ranef.store, plot.title, plot.subtitle, filename)
