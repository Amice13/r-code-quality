#####################################################################
## Replication Code for: Ansolabehere, Fraga, and Schaffner (2021) #########################################
## Figure A.4: CPS Turnout by State w/Confidence Interval and line indicating McDonald's turnout estimate ##
############################################################################################################

#install.packages("ggplot2")
#install.packages("scales")
#install.packages("ggrepel")

require(ggplot2)
require(scales)
require(ggrepel)

## Figure A.4: Turnout by State in each of the Six Southern States, compared to McDonald estimates

data <- read.csv("CPS_Turnout_2008-2018.csv", stringsAsFactors=FALSE)

mcd <- read.csv("McDonaldTurnout_2019-07-20.csv", stringsAsFactors=FALSE)

prison <- read.csv("Prisoners_2006_2018.csv", stringsAsFactors=FALSE)
mcd <- merge(mcd, prison, all.x=TRUE, sort=FALSE, by.x=c("Year","State"), by.y=c("YEAR","State_Name"))

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

## Alabama ##
data_south <- subset(data, State == "AL" & Race == "Total")
mcd_south <- subset(mcd, Year >= 2008 & State == "Alabama")
mcd_south$Voters[is.na(mcd_south$Voters)] <- mcd_south$VotersHighest[is.na(mcd_south$Voters)]
mcd_south$Turnout <- mcd_south$Voters/(mcd_south$CVAP-mcd_south$Total)

fig1b <- ggplot(data= data_south, aes(x=Year, y=Turnout))
fig1b <- fig1b + geom_point(data=mcd_south, aes(y=Turnout, x=Year), color="gray60", size=2.5)
fig1b <- fig1b + geom_pointrange(aes(ymin=Turnout_low, ymax=Turnout_upp), color="black", size=1.25, fatten=1.5)
fig1b <- fig1b + scale_y_continuous("Voter Turnout", labels=percent_format(1), limits=c(0.325,0.7), breaks=seq(0.4,0.7,0.1))
fig1b <- fig1b + scale_x_continuous(name=NULL, limits=c(2008,2019), breaks=seq(2008,2018,2))

labels_south <- rbind(cbind(subset(data_south, select=c(Year, Turnout))), subset(mcd_south, select=c(Year, Turnout)))
labels_south$set <- "cps"
labels_south$set[7:12] <- "mcd"
fig1b <- fig1b + geom_text_repel(data=labels_south, aes(y=Turnout, x=Year, label=percent_format(0.1)(Turnout), color=set), nudge_x=0.75, direction="y")
fig1b <- fig1b + scale_color_manual(values=c("black","gray60"))

fig1b <- fig1b + displayprefs

quartz(width=5.5,height=2.75, type="pdf",file="figA4_AL.pdf")
fig1b
dev.off()

## Florida ##
data_south <- subset(data, State == "FL" & Race == "Total")
mcd_south <- subset(mcd, Year >= 2008 & State == "Florida")
mcd_south$Voters[is.na(mcd_south$Voters)] <- mcd_south$VotersHighest[is.na(mcd_south$Voters)]
mcd_south$Turnout <- mcd_south$Voters/(mcd_south$CVAP-mcd_south$Total)

fig1b <- ggplot(data= data_south, aes(x=Year, y=Turnout))
fig1b <- fig1b + geom_point(data=mcd_south, aes(y=Turnout, x=Year), color="gray60", size=2.5)
fig1b <- fig1b + geom_pointrange(aes(ymin=Turnout_low, ymax=Turnout_upp), color="black", size=1.25, fatten=1.5)
fig1b <- fig1b + scale_y_continuous("Voter Turnout", labels=percent_format(1), limits=c(0.35,0.7), breaks=seq(0.4,0.7,0.1))
fig1b <- fig1b + scale_x_continuous(name=NULL, limits=c(2008,2019), breaks=seq(2008,2018,2))

labels_south <- rbind(cbind(subset(data_south, select=c(Year, Turnout))), subset(mcd_south, select=c(Year, Turnout)))
labels_south$set <- "cps"
labels_south$set[7:12] <- "mcd"
fig1b <- fig1b + geom_text_repel(data=labels_south, aes(y=Turnout, x=Year, label=percent_format(0.1)(Turnout), color=set), nudge_x=0.75, direction="y")
fig1b <- fig1b + scale_color_manual(values=c("black","gray60"))

fig1b <- fig1b + displayprefs

quartz(width=5.5,height=2.75, type="pdf",file="figA4_FL.pdf")
fig1b
dev.off()

## Louisiana ##
data_south <- subset(data, State == "LA" & Race == "Total")
mcd_south <- subset(mcd, Year >= 2008 & State == "Louisiana")
mcd_south$Voters[is.na(mcd_south$Voters)] <- mcd_south$VotersHighest[is.na(mcd_south$Voters)]
mcd_south$Turnout <- mcd_south$Voters/(mcd_south$CVAP-mcd_south$Total)

fig1b <- ggplot(data= data_south, aes(x=Year, y=Turnout))
fig1b <- fig1b + geom_point(data=mcd_south, aes(y=Turnout, x=Year), color="gray60", size=2.5)
fig1b <- fig1b + geom_pointrange(aes(ymin=Turnout_low, ymax=Turnout_upp), color="black", size=1.25, fatten=1.5)
fig1b <- fig1b + scale_y_continuous("Voter Turnout", labels=percent_format(1), limits=c(0.35,0.75), breaks=seq(0.4,0.7,0.1))
fig1b <- fig1b + scale_x_continuous(name=NULL, limits=c(2008,2019), breaks=seq(2008,2018,2))

labels_south <- rbind(cbind(subset(data_south, select=c(Year, Turnout))), subset(mcd_south, select=c(Year, Turnout)))
labels_south$set <- "cps"
labels_south$set[7:12] <- "mcd"
fig1b <- fig1b + geom_text_repel(data=labels_south, aes(y=Turnout, x=Year, label=percent_format(0.1)(Turnout), color=set), nudge_x=0.75, direction="y")
fig1b <- fig1b + scale_color_manual(values=c("black","gray60"))

fig1b <- fig1b + displayprefs

quartz(width=5.5,height=2.75, type="pdf",file="figA4_LA.pdf")
fig1b
dev.off()

## Georgia ##
data_south <- subset(data, State == "GA" & Race == "Total")
mcd_south <- subset(mcd, Year >= 2008 & State == "Georgia")
mcd_south$Voters[is.na(mcd_south$Voters)] <- mcd_south$VotersHighest[is.na(mcd_south$Voters)]
mcd_south$Turnout <- mcd_south$Voters/(mcd_south$CVAP-mcd_south$Total)

fig1b <- ggplot(data= data_south, aes(x=Year, y=Turnout))
fig1b <- fig1b + geom_point(data=mcd_south, aes(y=Turnout, x=Year), color="gray60", size=2.5)
fig1b <- fig1b + geom_pointrange(aes(ymin=Turnout_low, ymax=Turnout_upp), color="black", size=1.25, fatten=1.5)
fig1b <- fig1b + scale_y_continuous("Voter Turnout", labels=percent_format(1), limits=c(0.35,0.7), breaks=seq(0.4,0.7,0.1))
fig1b <- fig1b + scale_x_continuous(name=NULL, limits=c(2008,2019), breaks=seq(2008,2018,2))

labels_south <- rbind(cbind(subset(data_south, select=c(Year, Turnout))), subset(mcd_south, select=c(Year, Turnout)))
labels_south$set <- "cps"
labels_south$set[7:12] <- "mcd"
fig1b <- fig1b + geom_text_repel(data=labels_south, aes(y=Turnout, x=Year, label=percent_format(0.1)(Turnout), color=set), nudge_x=0.75, direction="y")
fig1b <- fig1b + scale_color_manual(values=c("black","gray60"))

fig1b <- fig1b + displayprefs

quartz(width=5.5,height=2.75, type="pdf",file="figA4_GA.pdf")
fig1b
dev.off()

## North Carolina ##
data_south <- subset(data, State == "NC" & Race == "Total")
mcd_south <- subset(mcd, Year >= 2008 & State == "North Carolina")
mcd_south$Voters[is.na(mcd_south$Voters)] <- mcd_south$VotersHighest[is.na(mcd_south$Voters)]
mcd_south$Turnout <- mcd_south$Voters/(mcd_south$CVAP-mcd_south$Total)

fig1b <- ggplot(data= data_south, aes(x=Year, y=Turnout))
fig1b <- fig1b + geom_point(data=mcd_south, aes(y=Turnout, x=Year), color="gray60", size=2.5)
fig1b <- fig1b + geom_pointrange(aes(ymin=Turnout_low, ymax=Turnout_upp), color="black", size=1.25, fatten=1.5)
fig1b <- fig1b + scale_y_continuous("Voter Turnout", labels=percent_format(1), limits=c(0.35,0.725), breaks=seq(0.4,0.7,0.1))
fig1b <- fig1b + scale_x_continuous(name=NULL, limits=c(2008,2019), breaks=seq(2008,2018,2))

labels_south <- rbind(cbind(subset(data_south, select=c(Year, Turnout))), subset(mcd_south, select=c(Year, Turnout)))
labels_south$set <- "cps"
labels_south$set[7:12] <- "mcd"
fig1b <- fig1b + geom_text_repel(data=labels_south, aes(y=Turnout, x=Year, label=percent_format(0.1)(Turnout), color=set), nudge_x=0.75, direction="y")
fig1b <- fig1b + scale_color_manual(values=c("black","gray60"))

fig1b <- fig1b + displayprefs

quartz(width=5.5,height=2.75, type="pdf",file="figA4_NC.pdf")
fig1b
dev.off()

## South Carolina ##
data_south <- subset(data, State == "SC" & Race == "Total")
mcd_south <- subset(mcd, Year >= 2008 & State == "South Carolina")
mcd_south$Voters[is.na(mcd_south$Voters)] <- mcd_south$VotersHighest[is.na(mcd_south$Voters)]
mcd_south$Turnout <- mcd_south$Voters/(mcd_south$CVAP-mcd_south$Total)

fig1b <- ggplot(data= data_south, aes(x=Year, y=Turnout))
fig1b <- fig1b + geom_point(data=mcd_south, aes(y=Turnout, x=Year), color="gray60", size=2.5)
fig1b <- fig1b + geom_pointrange(aes(ymin=Turnout_low, ymax=Turnout_upp), color="black", size=1.25, fatten=1.5)
fig1b <- fig1b + scale_y_continuous("Voter Turnout", labels=percent_format(1), limits=c(0.325,0.7), breaks=seq(0.4,0.7,0.1))
fig1b <- fig1b + scale_x_continuous(name=NULL, limits=c(2008,2019), breaks=seq(2008,2018,2))

labels_south <- rbind(cbind(subset(data_south, select=c(Year, Turnout))), subset(mcd_south, select=c(Year, Turnout)))
labels_south$set <- "cps"
labels_south$set[7:12] <- "mcd"
fig1b <- fig1b + geom_text_repel(data=labels_south, aes(y=Turnout, x=Year, label=percent_format(0.1)(Turnout), color=set), nudge_x=0.75, direction="y")
fig1b <- fig1b + scale_color_manual(values=c("black","gray60"))

fig1b <- fig1b + displayprefs

quartz(width=5.5,height=2.75, type="pdf",file="figA4_SC.pdf")
fig1b
dev.off()