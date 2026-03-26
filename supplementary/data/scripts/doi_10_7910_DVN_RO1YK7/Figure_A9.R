#####################################################################
## Replication Code for: Ansolabehere, Fraga, and Schaffner (2021) #################################
## Figure A.9: CPS Hispanic Turnout by State w/Confidence Interval and Catalist + Census estimate ##
####################################################################################################

#install.packages("ggplot2")
#install.packages("scales")
#install.packages("ggrepel")

require(ggplot2)
require(scales)
require(ggrepel)

data <- read.csv("CPS_Turnout_2008-2018.csv", stringsAsFactors=FALSE)

cat <- read.csv("Catalist_Turnout_2008-2018.csv", stringsAsFactors=FALSE)

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
	
## AL ##
data_HispanicAL <- subset(data, Race == "Hispanic" & State == "AL")
cat_HispanicAL <- subset(cat, Race == "Hispanic" & State == "AL")
	
fig2a <- ggplot(data= data_HispanicAL, aes(x=Year, y=Turnout))
fig2a <- fig2a + geom_point(data=cat_HispanicAL, aes(y=Turnout, x=Year), color="gray60", size=2.5)
fig2a <- fig2a + geom_point(color="black", size=2.5)
fig2a <- fig2a + scale_y_continuous("Hispanic Voter Turnout", labels=percent_format(1), limits=c(0,0.75), breaks=seq(0,0.7,0.1))
fig2a <- fig2a + scale_x_continuous(name=NULL, limits=c(2008,2019), breaks=seq(2008,2018,2))

labels_HispanicAL <- rbind(cbind(subset(data_HispanicAL, select=c(Year, Turnout))), subset(cat_HispanicAL, select=c(Year, Turnout)))
labels_HispanicAL$set <- "cps"
labels_HispanicAL$set[7:12] <- "cat"
fig2a <- fig2a + geom_text_repel(data=labels_HispanicAL, aes(y=Turnout, x=Year, label=percent_format(0.1)(Turnout), color=set), nudge_x=0.75, direction="y")
fig2a <- fig2a + scale_color_manual(values=c("gray60","black"))

fig2a <- fig2a + displayprefs

quartz(width=5.5,height=2.75, type="pdf",file="figA9_AL.pdf")
fig2a
dev.off()

## FL ##
data_HispanicFL <- subset(data, Race == "Hispanic" & State == "FL")
cat_HispanicFL <- subset(cat, Race == "Hispanic" & State == "FL")
	
fig2a <- ggplot(data= data_HispanicFL, aes(x=Year, y=Turnout))
fig2a <- fig2a + geom_point(data=cat_HispanicFL, aes(y=Turnout, x=Year), color="gray60", size=2.5)
fig2a <- fig2a + geom_point(color="black", size=2.5)
fig2a <- fig2a + scale_y_continuous("Hispanic Voter Turnout", labels=percent_format(1), limits=c(0,0.75), breaks=seq(0,0.7,0.1))
fig2a <- fig2a + scale_x_continuous(name=NULL, limits=c(2008,2019), breaks=seq(2008,2018,2))

labels_HispanicFL <- rbind(cbind(subset(data_HispanicFL, select=c(Year, Turnout))), subset(cat_HispanicFL, select=c(Year, Turnout)))
labels_HispanicFL$set <- "cps"
labels_HispanicFL$set[7:12] <- "cat"
fig2a <- fig2a + geom_text_repel(data=labels_HispanicFL, aes(y=Turnout, x=Year, label=percent_format(0.1)(Turnout), color=set), nudge_x=0.75, direction="y")
fig2a <- fig2a + scale_color_manual(values=c("gray60","black"))

fig2a <- fig2a + displayprefs

quartz(width=5.5,height=2.75, type="pdf",file="figA9_FL.pdf")
fig2a
dev.off()

## GA ##
data_HispanicGA <- subset(data, Race == "Hispanic" & State == "GA")
cat_HispanicGA <- subset(cat, Race == "Hispanic" & State == "GA")
	
fig2a <- ggplot(data= data_HispanicGA, aes(x=Year, y=Turnout))
fig2a <- fig2a + geom_point(data=cat_HispanicGA, aes(y=Turnout, x=Year), color="gray60", size=2.5)
fig2a <- fig2a + geom_point(color="black", size=2.5)
fig2a <- fig2a + scale_y_continuous("Hispanic Voter Turnout", labels=percent_format(1), limits=c(0,0.75), breaks=seq(0,0.7,0.1))
fig2a <- fig2a + scale_x_continuous(name=NULL, limits=c(2008,2019), breaks=seq(2008,2018,2))

labels_HispanicGA <- rbind(cbind(subset(data_HispanicGA, select=c(Year, Turnout))), subset(cat_HispanicGA, select=c(Year, Turnout)))
labels_HispanicGA$set <- "cps"
labels_HispanicGA$set[7:12] <- "cat"
fig2a <- fig2a + geom_text_repel(data=labels_HispanicGA, aes(y=Turnout, x=Year, label=percent_format(0.1)(Turnout), color=set), nudge_x=0.75, direction="y")
fig2a <- fig2a + scale_color_manual(values=c("gray60","black"))

fig2a <- fig2a + displayprefs

quartz(width=5.5,height=2.75, type="pdf",file="figA9_GA.pdf")
fig2a
dev.off()

## LA ##
data_HispanicLA <- subset(data, Race == "Hispanic" & State == "LA")
cat_HispanicLA <- subset(cat, Race == "Hispanic" & State == "LA")
	
fig2a <- ggplot(data= data_HispanicLA, aes(x=Year, y=Turnout))
fig2a <- fig2a + geom_point(data=cat_HispanicLA, aes(y=Turnout, x=Year), color="gray60", size=2.5)
fig2a <- fig2a + geom_point(color="black", size=2.5)
fig2a <- fig2a + scale_y_continuous("Hispanic Voter Turnout", labels=percent_format(1), limits=c(0,0.75), breaks=seq(0,0.7,0.1))
fig2a <- fig2a + scale_x_continuous(name=NULL, limits=c(2008,2019), breaks=seq(2008,2018,2))

labels_HispanicLA <- rbind(cbind(subset(data_HispanicLA, select=c(Year, Turnout))), subset(cat_HispanicLA, select=c(Year, Turnout)))
labels_HispanicLA$set <- "cps"
labels_HispanicLA$set[7:12] <- "cat"
fig2a <- fig2a + geom_text_repel(data=labels_HispanicLA, aes(y=Turnout, x=Year, label=percent_format(0.1)(Turnout), color=set), nudge_x=0.75, direction="y")
fig2a <- fig2a + scale_color_manual(values=c("gray60","black"))

fig2a <- fig2a + displayprefs

quartz(width=5.5,height=2.75, type="pdf",file="figA9_LA.pdf")
fig2a
dev.off()

## NC ##
data_HispanicNC <- subset(data, Race == "Hispanic" & State == "NC")
cat_HispanicNC <- subset(cat, Race == "Hispanic" & State == "NC")
	
fig2a <- ggplot(data= data_HispanicNC, aes(x=Year, y=Turnout))
fig2a <- fig2a + geom_point(data=cat_HispanicNC, aes(y=Turnout, x=Year), color="gray60", size=2.5)
fig2a <- fig2a + geom_point(color="black", size=2.5)
fig2a <- fig2a + scale_y_continuous("Hispanic Voter Turnout", labels=percent_format(1), limits=c(0,0.75), breaks=seq(0,0.7,0.1))
fig2a <- fig2a + scale_x_continuous(name=NULL, limits=c(2008,2019), breaks=seq(2008,2018,2))

labels_HispanicNC <- rbind(cbind(subset(data_HispanicNC, select=c(Year, Turnout))), subset(cat_HispanicNC, select=c(Year, Turnout)))
labels_HispanicNC$set <- "cps"
labels_HispanicNC$set[7:12] <- "cat"
fig2a <- fig2a + geom_text_repel(data=labels_HispanicNC, aes(y=Turnout, x=Year, label=percent_format(0.1)(Turnout), color=set), nudge_x=0.75, direction="y")
fig2a <- fig2a + scale_color_manual(values=c("gray60","black"))

fig2a <- fig2a + displayprefs

quartz(width=5.5,height=2.75, type="pdf",file="figA9_NC.pdf")
fig2a
dev.off()

## SC ##
data_HispanicSC <- subset(data, Race == "Hispanic" & State == "SC")
cat_HispanicSC <- subset(cat, Race == "Hispanic" & State == "SC")
	
fig2a <- ggplot(data= data_HispanicSC, aes(x=Year, y=Turnout))
fig2a <- fig2a + geom_point(data=cat_HispanicSC, aes(y=Turnout, x=Year), color="gray60", size=2.5)
fig2a <- fig2a + geom_point(color="black", size=2.5)
fig2a <- fig2a + scale_y_continuous("Hispanic Voter Turnout", labels=percent_format(1), limits=c(0,0.75), breaks=seq(0,0.7,0.1))
fig2a <- fig2a + scale_x_continuous(name=NULL, limits=c(2008,2019), breaks=seq(2008,2018,2))

labels_HispanicSC <- rbind(cbind(subset(data_HispanicSC, select=c(Year, Turnout))), subset(cat_HispanicSC, select=c(Year, Turnout)))
labels_HispanicSC$set <- "cps"
labels_HispanicSC$set[7:12] <- "cat"
fig2a <- fig2a + geom_text_repel(data=labels_HispanicSC, aes(y=Turnout, x=Year, label=percent_format(0.1)(Turnout), color=set), nudge_x=0.75, direction="y")
fig2a <- fig2a + scale_color_manual(values=c("gray60","black"))

fig2a <- fig2a + displayprefs

quartz(width=5.5,height=2.75, type="pdf",file="figA9_SC.pdf")
fig2a
dev.off()