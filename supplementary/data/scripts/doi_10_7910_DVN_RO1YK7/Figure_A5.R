#####################################################################
## Replication Code for: Ansolabehere, Fraga, and Schaffner (2021) ##############################
## Figure A.5: CPS White Turnout by State w/Confidence Interval and Catalist + Census estimate ##
#################################################################################################

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
data_WhiteAL <- subset(data, Race == "White" & State == "AL")
cat_WhiteAL <- subset(cat, Race == "White" & State == "AL")
	
fig2a <- ggplot(data= data_WhiteAL, aes(x=Year, y=Turnout))
fig2a <- fig2a + geom_point(data=cat_WhiteAL, aes(y=Turnout, x=Year), color="gray60", size=2.5)
fig2a <- fig2a + geom_point(color="black", size=2.5)
fig2a <- fig2a + scale_y_continuous("White Voter Turnout", labels=percent_format(1), limits=c(0.33,0.775), breaks=seq(0.4,0.7,0.1))
fig2a <- fig2a + scale_x_continuous(name=NULL, limits=c(2008,2019), breaks=seq(2008,2018,2))

labels_WhiteAL <- rbind(cbind(subset(data_WhiteAL, select=c(Year, Turnout))), subset(cat_WhiteAL, select=c(Year, Turnout)))
labels_WhiteAL$set <- "cps"
labels_WhiteAL$set[7:12] <- "cat"
fig2a <- fig2a + geom_text_repel(data=labels_WhiteAL, aes(y=Turnout, x=Year, label=percent_format(0.1)(Turnout), color=set), nudge_x=0.75, direction="y")
fig2a <- fig2a + scale_color_manual(values=c("gray60","black"))

fig2a <- fig2a + displayprefs

quartz(width=5.5,height=2.75, type="pdf",file="figA5_AL.pdf")
fig2a
dev.off()

## FL ##
data_WhiteFL <- subset(data, Race == "White" & State == "FL")
cat_WhiteFL <- subset(cat, Race == "White" & State == "FL")
	
fig2a <- ggplot(data= data_WhiteFL, aes(x=Year, y=Turnout))
fig2a <- fig2a + geom_point(data=cat_WhiteFL, aes(y=Turnout, x=Year), color="gray60", size=2.5)
fig2a <- fig2a + geom_point(color="black", size=2.5)
fig2a <- fig2a + scale_y_continuous("White Voter Turnout", labels=percent_format(1), limits=c(0.33,0.775), breaks=seq(0.4,0.7,0.1))
fig2a <- fig2a + scale_x_continuous(name=NULL, limits=c(2008,2019), breaks=seq(2008,2018,2))

labels_WhiteFL <- rbind(cbind(subset(data_WhiteFL, select=c(Year, Turnout))), subset(cat_WhiteFL, select=c(Year, Turnout)))
labels_WhiteFL$set <- "cps"
labels_WhiteFL$set[7:12] <- "cat"
fig2a <- fig2a + geom_text_repel(data=labels_WhiteFL, aes(y=Turnout, x=Year, label=percent_format(0.1)(Turnout), color=set), nudge_x=0.75, direction="y")
fig2a <- fig2a + scale_color_manual(values=c("gray60","black"))

fig2a <- fig2a + displayprefs

quartz(width=5.5,height=2.75, type="pdf",file="figA5_FL.pdf")
fig2a
dev.off()

## GA ##
data_WhiteGA <- subset(data, Race == "White" & State == "GA")
cat_WhiteGA <- subset(cat, Race == "White" & State == "GA")
	
fig2a <- ggplot(data= data_WhiteGA, aes(x=Year, y=Turnout))
fig2a <- fig2a + geom_point(data=cat_WhiteGA, aes(y=Turnout, x=Year), color="gray60", size=2.5)
fig2a <- fig2a + geom_point(color="black", size=2.5)
fig2a <- fig2a + scale_y_continuous("White Voter Turnout", labels=percent_format(1), limits=c(0.33,0.775), breaks=seq(0.4,0.7,0.1))
fig2a <- fig2a + scale_x_continuous(name=NULL, limits=c(2008,2019), breaks=seq(2008,2018,2))

labels_WhiteGA <- rbind(cbind(subset(data_WhiteGA, select=c(Year, Turnout))), subset(cat_WhiteGA, select=c(Year, Turnout)))
labels_WhiteGA$set <- "cps"
labels_WhiteGA$set[7:12] <- "cat"
fig2a <- fig2a + geom_text_repel(data=labels_WhiteGA, aes(y=Turnout, x=Year, label=percent_format(0.1)(Turnout), color=set), nudge_x=0.75, direction="y")
fig2a <- fig2a + scale_color_manual(values=c("gray60","black"))

fig2a <- fig2a + displayprefs

quartz(width=5.5,height=2.75, type="pdf",file="figA5_GA.pdf")
fig2a
dev.off()

## LA ##
data_WhiteLA <- subset(data, Race == "White" & State == "LA")
cat_WhiteLA <- subset(cat, Race == "White" & State == "LA")
	
fig2a <- ggplot(data= data_WhiteLA, aes(x=Year, y=Turnout))
fig2a <- fig2a + geom_point(data=cat_WhiteLA, aes(y=Turnout, x=Year), color="gray60", size=2.5)
fig2a <- fig2a + geom_point(color="black", size=2.5)
fig2a <- fig2a + scale_y_continuous("White Voter Turnout", labels=percent_format(1), limits=c(0.33,0.775), breaks=seq(0.4,0.7,0.1))
fig2a <- fig2a + scale_x_continuous(name=NULL, limits=c(2008,2019), breaks=seq(2008,2018,2))

labels_WhiteLA <- rbind(cbind(subset(data_WhiteLA, select=c(Year, Turnout))), subset(cat_WhiteLA, select=c(Year, Turnout)))
labels_WhiteLA$set <- "cps"
labels_WhiteLA$set[7:12] <- "cat"
fig2a <- fig2a + geom_text_repel(data=labels_WhiteLA, aes(y=Turnout, x=Year, label=percent_format(0.1)(Turnout), color=set), nudge_x=0.75, direction="y")
fig2a <- fig2a + scale_color_manual(values=c("gray60","black"))

fig2a <- fig2a + displayprefs

quartz(width=5.5,height=2.75, type="pdf",file="figA5_LA.pdf")
fig2a
dev.off()

## NC ##
data_WhiteNC <- subset(data, Race == "White" & State == "NC")
cat_WhiteNC <- subset(cat, Race == "White" & State == "NC")
	
fig2a <- ggplot(data= data_WhiteNC, aes(x=Year, y=Turnout))
fig2a <- fig2a + geom_point(data=cat_WhiteNC, aes(y=Turnout, x=Year), color="gray60", size=2.5)
fig2a <- fig2a + geom_point(color="black", size=2.5)
fig2a <- fig2a + scale_y_continuous("White Voter Turnout", labels=percent_format(1), limits=c(0.33,0.775), breaks=seq(0.4,0.7,0.1))
fig2a <- fig2a + scale_x_continuous(name=NULL, limits=c(2008,2019), breaks=seq(2008,2018,2))

labels_WhiteNC <- rbind(cbind(subset(data_WhiteNC, select=c(Year, Turnout))), subset(cat_WhiteNC, select=c(Year, Turnout)))
labels_WhiteNC$set <- "cps"
labels_WhiteNC$set[7:12] <- "cat"
fig2a <- fig2a + geom_text_repel(data=labels_WhiteNC, aes(y=Turnout, x=Year, label=percent_format(0.1)(Turnout), color=set), nudge_x=0.75, direction="y")
fig2a <- fig2a + scale_color_manual(values=c("gray60","black"))

fig2a <- fig2a + displayprefs

quartz(width=5.5,height=2.75, type="pdf",file="figA5_NC.pdf")
fig2a
dev.off()

## SC ##
data_WhiteSC <- subset(data, Race == "White" & State == "SC")
cat_WhiteSC <- subset(cat, Race == "White" & State == "SC")
	
fig2a <- ggplot(data= data_WhiteSC, aes(x=Year, y=Turnout))
fig2a <- fig2a + geom_point(data=cat_WhiteSC, aes(y=Turnout, x=Year), color="gray60", size=2.5)
fig2a <- fig2a + geom_point(color="black", size=2.5)
fig2a <- fig2a + scale_y_continuous("White Voter Turnout", labels=percent_format(1), limits=c(0.33,0.775), breaks=seq(0.4,0.7,0.1))
fig2a <- fig2a + scale_x_continuous(name=NULL, limits=c(2008,2019), breaks=seq(2008,2018,2))

labels_WhiteSC <- rbind(cbind(subset(data_WhiteSC, select=c(Year, Turnout))), subset(cat_WhiteSC, select=c(Year, Turnout)))
labels_WhiteSC$set <- "cps"
labels_WhiteSC$set[7:12] <- "cat"
fig2a <- fig2a + geom_text_repel(data=labels_WhiteSC, aes(y=Turnout, x=Year, label=percent_format(0.1)(Turnout), color=set), nudge_x=0.75, direction="y")
fig2a <- fig2a + scale_color_manual(values=c("gray60","black"))

fig2a <- fig2a + displayprefs

quartz(width=5.5,height=2.75, type="pdf",file="figA5_SC.pdf")
fig2a
dev.off()