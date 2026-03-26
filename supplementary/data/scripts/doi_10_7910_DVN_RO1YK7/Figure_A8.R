#####################################################################
## Replication Code for: Ansolabehere, Fraga, and Schaffner (2021) #################################
## Figure A.8: CPS Black Proportion by State w/Confidence Interval and Catalist + Census estimate ##
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
data_BlackAL <- subset(data, Race == "Black" & State == "AL")
cat_BlackAL <- subset(cat, Race == "Black" & State == "AL")
labels_BlackAL <- rbind(cbind(subset(data_BlackAL, select=c(Year, Prop_Voters))), subset(cat_BlackAL, select=c(Year, Prop_Voters)))
labels_BlackAL$set <- "cps"
labels_BlackAL$set[7:12] <- "cat"
	
fig3a <- ggplot(data=data_BlackAL, aes(x=Year, y=Prop_Voters))
fig3a <- fig3a + geom_point(data=cat_BlackAL, aes(y=Prop_Voters, x=Year), color="gray60", size=2.5)
fig3a <- fig3a + geom_point(color="black", size=2.5)
fig3a <- fig3a + scale_y_continuous("Black Share of Voters", labels=percent_format(1), limits=c(0.15,0.3), breaks=seq(0.15,0.3,0.05))
fig3a <- fig3a + scale_x_continuous(name=NULL, limits=c(2008,2019), breaks=seq(2008,2018,2))
fig3a <- fig3a + geom_text_repel(data=labels_BlackAL, aes(y=Prop_Voters, x=Year, label=percent_format(0.1)(Prop_Voters), color=set), nudge_x=0.75, direction="y")
fig3a <- fig3a + scale_color_manual(values=c("gray60","Black"))
fig3a <- fig3a + displayprefs

quartz(width=5.5,height=2.75, type="pdf",file="figA8_AL.pdf")
fig3a
dev.off()

## FL ##
data_BlackFL <- subset(data, Race == "Black" & State == "FL")
cat_BlackFL <- subset(cat, Race == "Black" & State == "FL")
labels_BlackFL <- rbind(cbind(subset(data_BlackFL, select=c(Year, Prop_Voters))), subset(cat_BlackFL, select=c(Year, Prop_Voters)))
labels_BlackFL$set <- "cps"
labels_BlackFL$set[7:12] <- "cat"
	
fig3a <- ggplot(data=data_BlackFL, aes(x=Year, y=Prop_Voters))
fig3a <- fig3a + geom_point(data=cat_BlackFL, aes(y=Prop_Voters, x=Year), color="gray60", size=2.5)
fig3a <- fig3a + geom_point(color="black", size=2.5)
fig3a <- fig3a + scale_y_continuous("Black Share of Voters", labels=percent_format(1), limits=c(0.1,0.25), breaks=seq(0.1,0.25,0.05))
fig3a <- fig3a + scale_x_continuous(name=NULL, limits=c(2008,2019), breaks=seq(2008,2018,2))
fig3a <- fig3a + geom_text_repel(data=labels_BlackFL, aes(y=Prop_Voters, x=Year, label=percent_format(0.1)(Prop_Voters), color=set), nudge_x=0.75, direction="y")
fig3a <- fig3a + scale_color_manual(values=c("gray60","Black"))
fig3a <- fig3a + displayprefs

quartz(width=5.5,height=2.75, type="pdf",file="figA8_FL.pdf")
fig3a
dev.off()

## GA ##
data_BlackGA <- subset(data, Race == "Black" & State == "GA")
cat_BlackGA <- subset(cat, Race == "Black" & State == "GA")
labels_BlackGA <- rbind(cbind(subset(data_BlackGA, select=c(Year, Prop_Voters))), subset(cat_BlackGA, select=c(Year, Prop_Voters)))
labels_BlackGA$set <- "cps"
labels_BlackGA$set[7:12] <- "cat"
	
fig3a <- ggplot(data=data_BlackGA, aes(x=Year, y=Prop_Voters))
fig3a <- fig3a + geom_point(data=cat_BlackGA, aes(y=Prop_Voters, x=Year), color="gray60", size=2.5)
fig3a <- fig3a + geom_point(color="black", size=2.5)
fig3a <- fig3a + scale_y_continuous("Black Share of Voters", labels=percent_format(1), limits=c(0.2,0.35), breaks=seq(0.2,0.35,0.05))
fig3a <- fig3a + scale_x_continuous(name=NULL, limits=c(2008,2019), breaks=seq(2008,2018,2))
fig3a <- fig3a + geom_text_repel(data=labels_BlackGA, aes(y=Prop_Voters, x=Year, label=percent_format(0.1)(Prop_Voters), color=set), nudge_x=0.75, direction="y")
fig3a <- fig3a + scale_color_manual(values=c("gray60","Black"))
fig3a <- fig3a + displayprefs

quartz(width=5.5,height=2.75, type="pdf",file="figA8_GA.pdf")
fig3a
dev.off()

## LA ##
data_BlackLA <- subset(data, Race == "Black" & State == "LA")
cat_BlackLA <- subset(cat, Race == "Black" & State == "LA")
labels_BlackLA <- rbind(cbind(subset(data_BlackLA, select=c(Year, Prop_Voters))), subset(cat_BlackLA, select=c(Year, Prop_Voters)))
labels_BlackLA$set <- "cps"
labels_BlackLA$set[7:12] <- "cat"
	
fig3a <- ggplot(data=data_BlackLA, aes(x=Year, y=Prop_Voters))
fig3a <- fig3a + geom_point(data=cat_BlackLA, aes(y=Prop_Voters, x=Year), color="gray60", size=2.5)
fig3a <- fig3a + geom_point(color="black", size=2.5)
fig3a <- fig3a + scale_y_continuous("Black Share of Voters", labels=percent_format(1), limits=c(0.2,0.35), breaks=seq(0.2,0.35,0.05))
fig3a <- fig3a + scale_x_continuous(name=NULL, limits=c(2008,2019), breaks=seq(2008,2018,2))
fig3a <- fig3a + geom_text_repel(data=labels_BlackLA, aes(y=Prop_Voters, x=Year, label=percent_format(0.1)(Prop_Voters), color=set), nudge_x=0.75, direction="y")
fig3a <- fig3a + scale_color_manual(values=c("gray60","Black"))
fig3a <- fig3a + displayprefs

quartz(width=5.5,height=2.75, type="pdf",file="figA8_LA.pdf")
fig3a
dev.off()

## NC ##
data_BlackNC <- subset(data, Race == "Black" & State == "NC")
cat_BlackNC <- subset(cat, Race == "Black" & State == "NC")
labels_BlackNC <- rbind(cbind(subset(data_BlackNC, select=c(Year, Prop_Voters))), subset(cat_BlackNC, select=c(Year, Prop_Voters)))
labels_BlackNC$set <- "cps"
labels_BlackNC$set[7:12] <- "cat"
	
fig3a <- ggplot(data=data_BlackNC, aes(x=Year, y=Prop_Voters))
fig3a <- fig3a + geom_point(data=cat_BlackNC, aes(y=Prop_Voters, x=Year), color="gray60", size=2.5)
fig3a <- fig3a + geom_point(color="black", size=2.5)
fig3a <- fig3a + scale_y_continuous("Black Share of Voters", labels=percent_format(1), limits=c(0.2,0.35), breaks=seq(0.2,0.35,0.05))
fig3a <- fig3a + scale_x_continuous(name=NULL, limits=c(2008,2019), breaks=seq(2008,2018,2))
fig3a <- fig3a + geom_text_repel(data=labels_BlackNC, aes(y=Prop_Voters, x=Year, label=percent_format(0.1)(Prop_Voters), color=set), nudge_x=0.75, direction="y")
fig3a <- fig3a + scale_color_manual(values=c("gray60","Black"))
fig3a <- fig3a + displayprefs

quartz(width=5.5,height=2.75, type="pdf",file="figA8_NC.pdf")
fig3a
dev.off()

## SC ##
data_BlackSC <- subset(data, Race == "Black" & State == "SC")
cat_BlackSC <- subset(cat, Race == "Black" & State == "SC")
labels_BlackSC <- rbind(cbind(subset(data_BlackSC, select=c(Year, Prop_Voters))), subset(cat_BlackSC, select=c(Year, Prop_Voters)))
labels_BlackSC$set <- "cps"
labels_BlackSC$set[7:12] <- "cat"
	
fig3a <- ggplot(data=data_BlackSC, aes(x=Year, y=Prop_Voters))
fig3a <- fig3a + geom_point(data=cat_BlackSC, aes(y=Prop_Voters, x=Year), color="gray60", size=2.5)
fig3a <- fig3a + geom_point(color="black", size=2.5)
fig3a <- fig3a + scale_y_continuous("Black Share of Voters", labels=percent_format(1), limits=c(0.2,0.35), breaks=seq(0.2,0.35,0.05))
fig3a <- fig3a + scale_x_continuous(name=NULL, limits=c(2008,2019), breaks=seq(2008,2018,2))
fig3a <- fig3a + geom_text_repel(data=labels_BlackSC, aes(y=Prop_Voters, x=Year, label=percent_format(0.1)(Prop_Voters), color=set), nudge_x=0.75, direction="y")
fig3a <- fig3a + scale_color_manual(values=c("gray60","Black"))
fig3a <- fig3a + displayprefs

quartz(width=5.5,height=2.75, type="pdf",file="figA8_SC.pdf")
fig3a
dev.off()