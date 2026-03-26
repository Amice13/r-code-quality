##########
#### Assessing Ballot Structure and Split Ticket Voting
#### Barnes, Tchintian, Alles
#### Journal of Politics, forthcoming.
##########

#### updated: 06/25/2016

#### Building
#### Graphs

for(i in 1:length(unique(prect.matched.data$year))){
  
  #### Plot
  #### Data
  
  loop.dat <- subset(prect.matched.data, matching.method == "cem" & gis.method == "Thiessen Polygon")
  loop.dat <- unique(subset(loop.dat, select=c(precinct, year, split.share, electronic.device)))  
  loop.dat <- subset(loop.dat, year == unique(prect.matched.data$year)[i])
  loop.dat <- data.frame(rank=seq(1,nrow(loop.dat)), orderBy(~split.share, data=loop.dat), row.names=NULL)
  
  loop.dat$device.label <- ifelse(loop.dat$electronic.device==1, yes="Electronic Ballot", no="Paper Ballot")
  loop.dat$device.label <- paste(loop.dat$device.label, ", in 2011", sep="")
  
  
  #### Plot
  #### Title
  
  plot.title <- "Aggregated Share of Split Ballots, by Precinct and Type of Ballot"
  plot.subtitle <- unique(with(loop.dat, ifelse(year==2007, yes="No Implementation", 
                                                ifelse(year==2011, yes="Partial Implementation", 
                                              ifelse(year==2015, yes="Full Implementation", NA)))))
  plot.subtitle <- paste(unique(loop.dat$year), "Election:", plot.subtitle)
  
  
  ### Plot
  ### Script
  
  p <- ggplot(loop.dat, aes(x = rank, y = split.share, fill = device.label))
  
  # Chart region
  p <- p + theme_bw()
  p <- p + theme(panel.background=element_rect(fill="#F0F0F0")) +
    theme(plot.background=element_rect(fill="#F0F0F0")) +
    theme(panel.border=element_rect(colour="#F0F0F0"))
  
  # Grid format
  p <- p + theme(panel.grid.major.y = element_line(colour="#D0D0D0",size=.5),
                 panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_continuous(breaks=loop.dat$rank, labels=loop.dat$precinct) +
    scale_y_continuous(limits=c(0, 15), breaks=c(seq(from=0, to=100, by=2.5)))
  
  # Plot colors
  p <- p + scale_fill_manual(values=plot.col)
  
  # Plot legend
  p <- p + theme(legend.background=element_rect(fill="#F0F0F0"),
                 legend.title=element_blank(),
                 legend.key=element_rect(fill="#F0F0F0", colour = NA),
                 legend.position=c(.150, .725),
                 legend.text=element_text(size=12, colour="#535353", face="bold"))
  
  # Axis labels, and title
  p <- p + ylab("Share of Split Ballots") + xlab("Electoral Precinct") +
    theme(axis.text.x=element_text(size=11, colour="#535353")) +
    theme(axis.text.y=element_text(size=11, colour="#535353", face="bold")) +
    theme(axis.title.y=element_text(size=11, colour="#535353", face="bold", vjust=1.5)) +
    theme(axis.title.x=element_text(size=11, colour="#535353", face="bold", vjust=-.5)) +
    theme(axis.ticks=element_blank())
  
  p <- p + theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=16))
 
  
  # Plot content
  p <- p + geom_bar(stat="identity", width=.7, alpha=.85)
  #p <- p + ggtitle(bquote(atop(bold(.(plot.title)), atop(.(plot.subtitle), ""))))
  
  # x=0
  p <- p + geom_hline(yintercept=0, size=1.2, colour="#535353")
  
  
  #### Storing
  #### Plot
  
  filename <- paste("BTA JOP Figure 2-",unique(loop.dat$year),sep="")
  
  ggsave(p, file=paste(graph.dir,paste(filename,"png",sep="."),sep="/"), 
         width=12, height=8, units="in", dpi=800)
  

  rm(loop.dat, p, filename, plot.title, plot.subtitle)
  
}

rm(i)

