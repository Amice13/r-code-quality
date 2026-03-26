#####################################################################
## Replication Code for: Ansolabehere, Fraga, and Schaffner (2021) ####################################
## Figure A1: CPS Six State Proportions by Race w/Confidence Interval and Catalist + Census estimate ##
## Hur and Achen (2013) correction applied ############################################################
#######################################################################################################

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
	
## Figure A2a: White Proportion, CPS with Hur and Achen (2013) correction vs Catalist

data_white <- subset(data_sixstates, Race == "White")
cat_white <- subset(cat, Race == "White")
	
figA2a <- ggplot(data= data_white, aes(x=Year, y=Prop_Voters))
figA2a <- figA2a + geom_point(data=cat_white, aes(y=Prop_Voters, x=Year), color="gray60", size=2.5)
figA2a <- figA2a + geom_pointrange(aes(ymin=Prop_Voters_low, ymax=Prop_Voters_upp), color="black", size=1.25, fatten=1.5)
figA2a <- figA2a + scale_y_continuous("White Share of Voters", labels=percent_format(1), limits=c(0.65,0.751), breaks=seq(0.65,0.75,0.05))
figA2a <- figA2a + scale_x_continuous(name=NULL, limits=c(2008,2019), breaks=seq(2008,2018,2))

labels_white <- rbind(cbind(subset(data_white, select=c(Year, Prop_Voters))), subset(cat_white, select=c(Year, Prop_Voters)))
labels_white$set <- "cps"
labels_white$set[7:12] <- "cat"
figA2a <- figA2a + geom_text_repel(data=labels_white, aes(y=Prop_Voters, x=Year, label=percent_format(0.1)(Prop_Voters), color=set), nudge_x=0.75, direction="y")
figA2a <- figA2a + scale_color_manual(values=c("gray60","black"))

figA2a <- figA2a + displayprefs

quartz(width=5.5,height=2.75, type="pdf",file="figA2a.pdf")
figA2a
dev.off()

## Figure A2b: Black Prop_Voters, CPS with Hur and Achen (2013) correction vs Catalist

data_black <- subset(data_sixstates, Race == "Black")
cat_black <- subset(cat, Race == "Black")
	
figA2b <- ggplot(data= data_black, aes(x=Year, y=Prop_Voters))
figA2b <- figA2b + geom_point(data=cat_black, aes(y=Prop_Voters, x=Year), color="gray60", size=2.5)
figA2b <- figA2b + geom_pointrange(aes(ymin=Prop_Voters_low, ymax=Prop_Voters_upp), color="black", size=1.25, fatten=1.5)
figA2b <- figA2b + scale_y_continuous("Black Share of Voters", labels=percent_format(1), limits=c(0.15,0.251), breaks=seq(0.15,0.25,0.05))
figA2b <- figA2b + scale_x_continuous(name=NULL, limits=c(2008,2019), breaks=seq(2008,2018,2))

labels_black <- rbind(cbind(subset(data_black, select=c(Year, Prop_Voters))), subset(cat_black, select=c(Year, Prop_Voters)))
labels_black$set <- "cps"
labels_black$set[7:12] <- "cat"
figA2b <- figA2b + geom_text_repel(data=labels_black, aes(y=Prop_Voters, x=Year, label=percent_format(0.1)(Prop_Voters), color=set), nudge_x=0.75, direction="y")
figA2b <- figA2b + scale_color_manual(values=c("gray60","black"))

figA2b <- figA2b + displayprefs

quartz(width=5.5,height=2.75, type="pdf",file="figA2b.pdf")
figA2b
dev.off()

## Figure A2c: Hispanic Prop_Voters, CPS with Hur and Achen (2013) correction vs Catalist

data_hispanic <- subset(data_sixstates, Race == "Hispanic")
cat_hispanic <- subset(cat, Race == "Hispanic")
	
figA2c <- ggplot(data= data_hispanic, aes(x=Year, y=Prop_Voters))
figA2c <- figA2c + geom_point(data=cat_hispanic, aes(y=Prop_Voters, x=Year), color="gray60", size=2.5)
figA2c <- figA2c + geom_pointrange(aes(ymin=Prop_Voters_low, ymax=Prop_Voters_upp), color="black", size=1.25, fatten=1.5)
figA2c <- figA2c + scale_y_continuous("Hispanic Share of Voters", labels=percent_format(1), limits=c(0,0.101), breaks=seq(0,0.1,0.05))
figA2c <- figA2c + scale_x_continuous(name=NULL, limits=c(2008,2019), breaks=seq(2008,2018,2))

labels_hispanic <- rbind(cbind(subset(data_hispanic, select=c(Year, Prop_Voters))), subset(cat_hispanic, select=c(Year, Prop_Voters)))
labels_hispanic$set <- "cps"
labels_hispanic$set[7:12] <- "cat"
figA2c <- figA2c + geom_text_repel(data=labels_hispanic, aes(y=Prop_Voters, x=Year, label=percent_format(0.1)(Prop_Voters), color=set), nudge_x=0.75, direction="y")
figA2c <- figA2c + scale_color_manual(values=c("gray60","black"))

figA2c <- figA2c + displayprefs

quartz(width=5.5,height=2.75, type="pdf",file="figA2c.pdf")
figA2c
dev.off()