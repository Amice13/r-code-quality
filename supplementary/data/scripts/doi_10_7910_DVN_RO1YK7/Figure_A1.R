#####################################################################
## Replication Code for: Ansolabehere, Fraga, and Schaffner (2021) ################################
## Figure A1: CPS Six State Turnout by Race w/Confidence Interval and Catalist + Census estimate ##
## Hur and Achen (2013) correction applied ########################################################
###################################################################################################

#install.packages("ggplot2")
#install.packages("scales")
#install.packages("ggrepel")

require(ggplot2)
require(scales)
require(ggrepel)

data <- read.csv("CPS_Turnout_2008-2018_HurAchen.csv", stringsAsFactors=FALSE)

data_sixstates <- subset(data, State == "SixStates")

cat <- read.csv("Catalist_Turnout_2008-2018.csv", stringsAsFactors=FALSE)
cat <- subset(cat, State == "SixStates")

displayprefs <-  theme_bw() + theme(panel.grid.minor=element_blank(), 
	panel.grid.major.y=element_blank(), 
	panel.grid.major.x=element_blank(),
	panel.border=element_blank(),
	axis.text.x=element_text(family="Helvetica", size=12, color="black", face="bold"),
	axis.title.x=element_text(family="Helvetica", size=11, color="gray40"),
	axis.title.y=element_text(family="Helvetica", size=12, color="gray40", vjust=1.1),
	axis.text.y=element_text(family="Helvetica", size=12, color="black"),
	axis.line=element_line(color="black", size=0.5),
	axis.ticks.x=element_line(color="black", size=1),
	plot.caption=element_text(color="gray40"),
	plot.subtitle=element_text(family="Helvetica", size=8, vjust=0),
	plot.title=element_text(family="Helvetica", size=14, vjust=0, face="bold"),
	legend.text=element_text(family="Helvetica", size=12, color="black"),
	legend.position="none",
	legend.title=element_blank())
	
## Figure A1a: White Turnout, CPS with Hur and Achen (2013) correction vs Catalist

data_white <- subset(data_sixstates, Race == "White")
cat_white <- subset(cat, Race == "White")
	
figA1a <- ggplot(data= data_white, aes(x=Year, y=Turnout))
figA1a <- figA1a + geom_point(data=cat_white, aes(y=Turnout, x=Year), color="gray60", size=2.5)
figA1a <- figA1a + geom_pointrange(aes(ymin=Turnout_low, ymax=Turnout_upp), color="black", size=1.25, fatten=1.5)
figA1a <- figA1a + scale_y_continuous("White Voter Turnout", labels=percent_format(1), limits=c(0.35,0.7), breaks=seq(0.4,0.7,0.1))
figA1a <- figA1a + scale_x_continuous(name=NULL, limits=c(2008,2019), breaks=seq(2008,2018,2))

labels_white <- rbind(cbind(subset(data_white, select=c(Year, Turnout))), subset(cat_white, select=c(Year, Turnout)))
labels_white$set <- "cps"
labels_white$set[7:12] <- "cat"
figA1a <- figA1a + geom_text_repel(data=labels_white, aes(y=Turnout, x=Year, label=percent_format(0.1)(Turnout), color=set), nudge_x=0.75, direction="y", min.segment.length=Inf)
figA1a <- figA1a + scale_color_manual(values=c("gray60","black"))

figA1a <- figA1a + displayprefs

quartz(width=5.5,height=2.75, type="pdf",file="figA1a.pdf")
figA1a
dev.off()

## Figure A1b: Black Turnout, CPS with Hur and Achen (2013) correction vs Catalist

data_black <- subset(data_sixstates, Race == "Black")
cat_black <- subset(cat, Race == "Black")
	
figA1b <- ggplot(data= data_black, aes(x=Year, y=Turnout))
figA1b <- figA1b + geom_point(data=cat_black, aes(y=Turnout, x=Year), color="gray60", size=2.5)
figA1b <- figA1b + geom_pointrange(aes(ymin=Turnout_low, ymax=Turnout_upp), color="black", size=1.25, fatten=1.5)
figA1b <- figA1b + scale_y_continuous("Black Voter Turnout", labels=percent_format(1), limits=c(0.25,0.7), breaks=seq(0.3,0.7,0.1))
figA1b <- figA1b + scale_x_continuous(name=NULL, limits=c(2008,2019), breaks=seq(2008,2018,2))

labels_black <- rbind(cbind(subset(data_black, select=c(Year, Turnout))), subset(cat_black, select=c(Year, Turnout)))
labels_black$set <- "cps"
labels_black$set[7:12] <- "cat"
figA1b <- figA1b + geom_text_repel(data=labels_black, aes(y=Turnout, x=Year, label=percent_format(0.1)(Turnout), color=set), nudge_x=0.75, direction="y", min.segment.length=Inf)
figA1b <- figA1b + scale_color_manual(values=c("gray60","black"))

figA1b <- figA1b + displayprefs

quartz(width=5.5,height=2.75, type="pdf",file="figA1b.pdf")
figA1b
dev.off()

## Figure A1c: Hispanic Turnout, CPS with Hur and Achen (2013) correction vs Catalist

data_hispanic <- subset(data_sixstates, Race == "Hispanic")
cat_hispanic <- subset(cat, Race == "Hispanic")
	
figA1c <- ggplot(data= data_hispanic, aes(x=Year, y=Turnout))
figA1c <- figA1c + geom_point(data=cat_hispanic, aes(y=Turnout, x=Year), color="gray60", size=2.5)
figA1c <- figA1c + geom_pointrange(aes(ymin=Turnout_low, ymax=Turnout_upp), color="black", size=1.25, fatten=1.5)
figA1c <- figA1c + scale_y_continuous("Hispanic Voter Turnout", labels=percent_format(1), limits=c(0.175,0.7), breaks=seq(0.2,0.7,0.1))
figA1c <- figA1c + scale_x_continuous(name=NULL, limits=c(2008,2019), breaks=seq(2008,2018,2))

labels_hispanic <- rbind(cbind(subset(data_hispanic, select=c(Year, Turnout))), subset(cat_hispanic, select=c(Year, Turnout)))
labels_hispanic$set <- "cps"
labels_hispanic$set[7:12] <- "cat"
figA1c <- figA1c + geom_text_repel(data=labels_hispanic, aes(y=Turnout, x=Year, label=percent_format(0.1)(Turnout), color=set), nudge_x=0.75, direction="y", min.segment.length=Inf)
figA1c <- figA1c + scale_color_manual(values=c("gray60","black"))

figA1c <- figA1c + displayprefs

quartz(width=5.5,height=2.75, type="pdf",file="figA1c.pdf")
figA1c
dev.off()