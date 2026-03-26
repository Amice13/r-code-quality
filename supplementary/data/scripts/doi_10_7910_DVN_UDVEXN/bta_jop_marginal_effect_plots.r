##########
#### Assessing Ballot Structure and Split Ticket Voting
#### Barnes, Tchintian, Alles
#### Journal of Politics, forthcoming.
##########

#### updated: 06/25/2016

#### Marginal Effects ####
####     Figure 3     ####


#### Plot
#### Data

num.p <- max(summaryBy(count~ year + match.method + gis.method,
                     data = data.frame(pooled.pred, count = 1),
                     FUN = sum, keep.names = T)$count)

plot.dat <- c()

for(i in 1:length(unique(pooled.pred$year))){
  for(j in 1:length(unique(pooled.pred$match.method))){
    for(k in 1:length(unique(pooled.pred$gis.method))){
      
      tmp <- orderBy(~mean, data=subset(pooled.pred, year==unique(pooled.pred$year)[i] & 
                                        match.method==unique(pooled.pred$match.method)[j] &
                                        gis.method==unique(pooled.pred$gis.method)[k]))

      plot.dat <- data.frame(rbind(plot.dat, data.frame(ranking=seq(1,num.p,length.out=nrow(tmp)), 
                                                      tmp)), row.names=NULL) ; rm(tmp)
      
    }
  
  }
  
} ; rm(i,j,k)


### Year
### Label

plot.dat$election.year <- with(plot.dat, ifelse(year==2007, yes="No Implementation", no=NA))
plot.dat$election.year <- with(plot.dat, ifelse(year==2011, yes="Partial Implementation", no=election.year))
plot.dat$election.year <- with(plot.dat, ifelse(year==2015, yes="Full Implementation", no=election.year))

plot.dat$election.year <- with(plot.dat, paste(year, "Election:", election.year))


### Party Label
### Position

plot.dat <- data.frame(plot.dat,with(plot.dat, data.frame(y.pos=ifelse(mean<0, yes=ci.lo.95-.025, no=ci.up.95+.025),
                          hj=ifelse(mean<0, yes=1, no=0))))

plot.dat <- orderBy(~year + gis.method + match.method + ranking, data = plot.dat)


### Plot
### Title

plot.title <- "Marginal Effect of E-Voting on the Share of Split Ballots, by Party and Election Year"
plot.subtitle <- "Salta, 2007-2015. Coarsened Exact Matching, using on Thiessen Polygons (95% CI)"


#### Plot
#### Script

p <- ggplot(subset(plot.dat, gis.method=="Thiessen Polygon" & match.method=="cem"),
               aes(x=ranking, y=mean))

# Chart region
p <- p + theme_bw()
p <- p + theme(panel.background=element_rect(fill="#F0F0F0")) +
  theme(plot.background=element_rect(fill="#F0F0F0")) +
  theme(panel.border=element_rect(colour="#F0F0F0"))

# Grid format
p <- p + theme(panel.grid.major.x=element_line(colour="#D0D0D0",size=.5),
               panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
  scale_x_continuous(minor_breaks=0,limits=c(0,num.p)) +
  scale_y_continuous(minor_breaks=0,limits=c(-.6,.6), breaks=c(seq(from=-1, to=1, by=.25)[!seq(from=-1, to=1, by=.25)%in%c(0)]))
  
# Drop legend
p <- p + theme(legend.position="none")

# Axis labels, and title
p <- p + ylab("Level of Ballot Splitting") + xlab("") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_blank()) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
  theme(axis.ticks=element_blank())

p <- p + theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=16))

# Y axis, and x=0
p <- p + geom_vline(xintercept=0,size=1.2,colour="#535353")
p <- p + geom_hline(yintercept=0, linetype="dashed",size=.75,colour="#535353")

# Party Labels
p <- p + geom_text(aes(label=party.label, y=y.pos, x=ranking, hjust=hj), size=3.25)

# Plot margins
p <- p + theme(plot.margin = unit(c(1, 1, .7, .7), "cm"))

# Facet Grid
p <- p + facet_grid(.~election.year) + coord_flip() +
  theme(strip.background = element_blank()) + 
  theme(strip.text = element_text(size=14,colour="#535353",face="bold",vjust=.75))

# Plot content
p <- p + geom_pointrange(aes(ymin=ci.lo.95, ymax=ci.up.95), shape=".", size=1.00)
p <- p + geom_point(size=4, colour="#F8766D")

p <- p + ggtitle(bquote(atop(bold(.(plot.title)), atop(.(plot.subtitle), ""))))


#### Storing
#### Plot

filename <- "BTA JOP Figure 3"

ggsave(p, file=paste(graph.dir,paste(filename,"png",sep="."),sep="/"), 
       width=12, height=8, units="in", dpi=800)




#### Marginal Effects ####
####    Figure E1     ####


#### Plot
#### Data

num.p <- max(summaryBy(count ~ year + match.method + gis.method,
                       data = data.frame(pooled.pred, count = 1),
                       FUN = sum, keep.names = T)$count)

plot.dat<-c()
for(i in 1:length(unique(pooled.pred$year))){
  for(j in 1:length(unique(pooled.pred$match.method))){
    
    tmp <- subset(pooled.pred, year == unique(pooled.pred$year)[i] & 
                  match.method == unique(pooled.pred$match.method)[j])
    
    tmp <-merge(tmp, data.frame(ranking=seq(1,num.p,length.out=length(unique(tmp$party.number))),
                               orderBy(~mean.gis+party.number, 
                                       data=summaryBy(mean~party.number, FUN=mean, na.rm=T, 
                                                      keep.names=T, var.names="mean.gis", data=tmp))),
               by="party.number", all.x=T)
    
    tmp <- orderBy(~ranking+party.number+gis.method, data=tmp)
    
    plot.dat <- data.frame(rbind(plot.dat, tmp), row.names=NULL) ; rm(tmp)
    
  }
  
} ; rm(i,j)


### Year
### Label

plot.dat$election.year <- with(plot.dat, ifelse(year==2007, yes="No Implementation", no=NA))
plot.dat$election.year <- with(plot.dat, ifelse(year==2011, yes="Partial Implementation", no=election.year))
plot.dat$election.year <- with(plot.dat, ifelse(year==2015, yes="Full Implementation", no=election.year))

plot.dat$election.year <- with(plot.dat, paste(year, "Election:", election.year))


### Party Label
### Position

plot.dat <- merge(plot.dat,merge(summaryBy(ci.lo.95~year+match.method+party.number,
                                         FUN=min, data=plot.dat,keep.names=T, var.names="lo.pos"),
                               summaryBy(ci.up.95~year+match.method+party.number,
                                         FUN=max, data=plot.dat,keep.names=T, var.names="hi.pos"),
                               by=c("year","match.method","party.number"),all=T),
                by=c("year","match.method","party.number"),all.x=T)

plot.dat <- data.frame(plot.dat, with(plot.dat, data.frame(y.pos=ifelse(mean.gis<0, 
                                                                     yes=lo.pos-.025, 
                                                                     no=hi.pos+.025),
                                                        hj=ifelse(mean.gis<0, yes=1, no=0))))
plot.dat <- plot.dat[!colnames(plot.dat) %in% c("lo.pos","hi.pos") ]


### Matching
### Label

plot.dat$match.method <- with(plot.dat, ifelse(match.method=="psm", 
                                               yes="Propensity Score Matching", 
                                               no="Coarsened Exact Matching"))

plot.dat <- orderBy(~year + gis.method + match.method + ranking, data = plot.dat)


### Plot
### Title

plot.title <- "Marginal Effect of E-Voting on the Share of Split Ballots, by Party and Election Year"
plot.subtitle <- "Salta, 2007-2015. Alternative GIS Techniques and Matching Approaches (95% CI)"


#### Plot
#### Script

p <- ggplot(plot.dat, aes(x=ranking, y=mean, group=gis.method))

# Chart region
p <- p + theme_bw()
p <- p + theme(panel.background=element_rect(fill="#F0F0F0")) +
  theme(plot.background=element_rect(fill="#F0F0F0")) +
  theme(panel.border=element_rect(colour="#F0F0F0"))

# Grid format
p <- p + theme(panel.grid.major.x=element_line(colour="#D0D0D0",size=.5),
               panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
  scale_x_continuous(minor_breaks=0,limits=c(0,num.p+1)) +
  scale_y_continuous(minor_breaks=0,limits=c(-.675,.675), breaks=c(seq(from=-1, to=1, by=.25)[!seq(from=-1, to=1, by=.25)%in%c(0)]))

# Plot legend
p <- p + theme(legend.background=element_rect(fill="#F0F0F0"),
               legend.title=element_blank(),
               legend.key=element_rect(fill="#F0F0F0", colour = NA),
               legend.position=c(0.025,0.925))

# Axis labels, and title
p <- p + ylab("Level of Ballot Splitting") + xlab("") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_blank()) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
  theme(axis.ticks=element_blank())

p <- p + theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=16))

# Y axis, and x=0
p <- p + geom_vline(xintercept=0,size=1.2,colour="#535353")
p <- p + geom_hline(yintercept=0, linetype="dashed",size=.75,colour="#535353")

# Party Labels
p <- p + geom_text(aes(label=party.label, y=y.pos, x=ranking, hjust=hj), size=3.25)

# Plot margins
p <- p + theme(plot.margin = unit(c(1, 1, .7, .7), "cm"))

# Facet Grid
p <- p + facet_grid(match.method~election.year) + coord_flip() +
  theme(strip.background = element_blank()) + 
  theme(strip.text = element_text(size=14,colour="#535353",face="bold",vjust=.75))

# Plot content
p <- p + geom_pointrange(aes(ymin=ci.lo.95, ymax=ci.up.95),
                         position=position_dodge(width=.35), 
                         shape=".", size=1.00)
p <- p + geom_point(aes(colour=gis.method),size=4, position=position_dodge(width=.35))

p <- p + ggtitle(bquote(atop(bold(.(plot.title)), atop(.(plot.subtitle), ""))))


#### Storing
#### Plot

filename <- "BTA JOP Figure E1"

ggsave(p, file=paste(graph.dir,paste(filename,"png",sep="."),sep="/"), 
       width=12, height=8, units="in", dpi=800)



rm(plot.dat, plot.title, plot.subtitle, p, num.p, filename)

