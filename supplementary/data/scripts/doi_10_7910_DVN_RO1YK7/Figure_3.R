#####################################################################
## Replication Code for: Ansolabehere, Fraga, and Schaffner (2021) ###################################
## Figure 3: CPS Six State Proportions by Race w/Confidence Interval and Catalist + Census estimate ##
######################################################################################################

#install.packages("ggplot2")
#install.packages("scales")
#install.packages("ggrepel")

require(ggplot2)
require(scales)
require(ggrepel)

data <- read.csv("CPS_Turnout_2008-2018.csv", stringsAsFactors=FALSE)

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

## Figure 3a: White Proportion, CPS vs Catalist

data_white <- subset(data_sixstates, Race == "White")
cat_white <- subset(cat, Race == "White")
	
fig3a <- ggplot(data= data_white, aes(x=Year, y=Prop_Voters))
fig3a <- fig3a + geom_point(data=cat_white, aes(y=Prop_Voters, x=Year), color="gray60", size=2.5)
fig3a <- fig3a + geom_pointrange(aes(ymin=Prop_Voters_low, ymax=Prop_Voters_upp), color="black", size=1.25, fatten=1.5)
fig3a <- fig3a + scale_y_continuous("White Share of Voters", labels=percent_format(1), limits=c(0.65,0.751), breaks=seq(0.65,0.75,0.05))
fig3a <- fig3a + scale_x_continuous(name=NULL, limits=c(2008,2019), breaks=seq(2008,2018,2))

labels_white <- rbind(cbind(subset(data_white, select=c(Year, Prop_Voters))), subset(cat_white, select=c(Year, Prop_Voters)))
labels_white$set <- "cps"
labels_white$set[7:12] <- "cat"
fig3a <- fig3a + geom_text_repel(data=labels_white, aes(y=Prop_Voters, x=Year, label=percent_format(0.1)(Prop_Voters), color=set), nudge_x=0.75, direction="y")
fig3a <- fig3a + scale_color_manual(values=c("gray60","black"))

fig3a <- fig3a + displayprefs

quartz(title=width=5.5,height=2.75, type="pdf",file="fig3a.pdf")
fig3a
dev.off()

## Figure 3b: Black Prop_Voters, CPS vs Catalist

data_black <- subset(data_sixstates, Race == "Black")
cat_black <- subset(cat, Race == "Black")
	
fig3b <- ggplot(data= data_black, aes(x=Year, y=Prop_Voters))
fig3b <- fig3b + geom_point(data=cat_black, aes(y=Prop_Voters, x=Year), color="gray60", size=2.5)
fig3b <- fig3b + geom_pointrange(aes(ymin=Prop_Voters_low, ymax=Prop_Voters_upp), color="black", size=1.25, fatten=1.5)
fig3b <- fig3b + scale_y_continuous("Black Share of Voters", labels=percent_format(1), limits=c(0.15,0.251), breaks=seq(0.15,0.25,0.05))
fig3b <- fig3b + scale_x_continuous(name=NULL, limits=c(2008,2019), breaks=seq(2008,2018,2))

labels_black <- rbind(cbind(subset(data_black, select=c(Year, Prop_Voters))), subset(cat_black, select=c(Year, Prop_Voters)))
labels_black$set <- "cps"
labels_black$set[7:12] <- "cat"
fig3b <- fig3b + geom_text_repel(data=labels_black, aes(y=Prop_Voters, x=Year, label=percent_format(0.1)(Prop_Voters), color=set), nudge_x=0.75, direction="y")
fig3b <- fig3b + scale_color_manual(values=c("gray60","black"))

fig3b <- fig3b + displayprefs

quartz(width=5.5,height=2.75, type="pdf",file="fig3b.pdf")
fig3b
dev.off()

## Figure 3c: Hispanic Prop_Voters, CPS vs Catalist

data_hispanic <- subset(data_sixstates, Race == "Hispanic")
cat_hispanic <- subset(cat, Race == "Hispanic")
	
fig3c <- ggplot(data= data_hispanic, aes(x=Year, y=Prop_Voters))
fig3c <- fig3c + geom_point(data=cat_hispanic, aes(y=Prop_Voters, x=Year), color="gray60", size=2.5)
fig3c <- fig3c + geom_pointrange(aes(ymin=Prop_Voters_low, ymax=Prop_Voters_upp), color="black", size=1.25, fatten=1.5)
fig3c <- fig3c + scale_y_continuous("Hispanic Share of Voters", labels=percent_format(1), limits=c(0,0.101), breaks=seq(0,0.1,0.05))
fig3c <- fig3c + scale_x_continuous(name=NULL, limits=c(2008,2019), breaks=seq(2008,2018,2))

labels_hispanic <- rbind(cbind(subset(data_hispanic, select=c(Year, Prop_Voters))), subset(cat_hispanic, select=c(Year, Prop_Voters)))
labels_hispanic$set <- "cps"
labels_hispanic$set[7:12] <- "cat"
fig3c <- fig3c + geom_text_repel(data=labels_hispanic, aes(y=Prop_Voters, x=Year, label=percent_format(0.1)(Prop_Voters), color=set), nudge_x=0.75, direction="y")
fig3c <- fig3c + scale_color_manual(values=c("gray60","black"))

fig3c <- fig3c + displayprefs

quartz(width=5.5,height=2.75, type="pdf",file="fig3c.pdf")
fig3c
dev.off()