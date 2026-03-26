#####################################################################
## Replication Code for: Ansolabehere, Fraga, and Schaffner (2021) ####################################
## Figure 1: CPS Total Turnout w/Confidence Interval and line indicating McDonald's turnout estimate ##
#######################################################################################################

#install.packages("ggplot2")
#install.packages("scales")
#install.packages("ggrepel")

require(ggplot2)
require(scales)
require(ggrepel)

data <- read.csv("CPS_Turnout_2008-2018.csv", stringsAsFactors=FALSE)

data_national <- subset(data, State == "National")

prison <- read.csv("Prisoners_2006_2018_National.csv", stringsAsFactors=FALSE)
prison$STATE <- "United States"

mcd <- read.csv("McDonaldTurnout_2019-07-20.csv", stringsAsFactors=FALSE)
mcd <- merge(mcd, prison, all.x=TRUE, sort=FALSE, by.x=c("Year","State"), by.y=c("YEAR","STATE"))
mcd$Turnout <- mcd$Voters/(mcd$CVAP-mcd$Total)
mcd_national <- subset(mcd, Year >= 2008 & State == "United States")
mcd_national <- subset(mcd_national, select=c(Year, Turnout))

# Figure 1a: National Turnout

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

labels_national <- rbind(cbind(subset(data_national, select=c(Year, Turnout))), subset(mcd_national, select=c(Year, Turnout)))
labels_national$set <- "cps"
labels_national$set[7:12] <- "mcd"

fig1a <- ggplot(data= data_national, aes(x=Year, y=Turnout))
fig1a <- fig1a + geom_point(data=mcd_national, aes(y=Turnout, x=Year), color="gray60", size=2.5)
fig1a <- fig1a + geom_pointrange(aes(ymin=Turnout_low, ymax=Turnout_upp), color="black", size=1.25, fatten=1.5)
fig1a <- fig1a + scale_y_continuous("Voter Turnout", labels=percent_format(1), limits=c(0.35,0.7), breaks=seq(0.4,0.7,0.1))
fig1a <- fig1a + scale_x_continuous(name=NULL, limits=c(2008,2019), breaks=seq(2008,2018,2))
fig1a <- fig1a + geom_text_repel(data=labels_national, aes(y=Turnout, x=Year, label=percent_format(0.1)(Turnout), color=set), nudge_x=0.75, direction="y")
fig1a <- fig1a + scale_color_manual(values=c("black","gray60"))
fig1a <- fig1a + displayprefs

quartz(width=5.5,height=2.75, type="pdf",file="fig1a.pdf")
fig1a
dev.off()

## Figure 1b: Six State Turnout

data_south <- subset(data, State == "SixStates" & Race == "Total")

mcd_south <- subset(mcd, State %in% c("Lousiana","Alabama","Georgia","Florida","South Carolina","North Carolina") & Year >= 2008)
mcd_south$Voters[is.na(mcd_south$Voters)] <- mcd_south$VotersHighest[is.na(mcd_south$Voters)]
mcd_south <- mcd_south %>%
	group_by(Year) %>%
	summarize(Voters = sum(Voters), CVAP = sum(CVAP), Prison = sum(Prison))
mcd_south$Turnout <- mcd_south$Voters/(mcd_south$CVAP - mcd_south$Prison)
mcd_south <- as.data.frame(mcd_south)

labels_south <- rbind(cbind(subset(data_south, select=c(Year, Turnout))), subset(mcd_south, select=c(Year, Turnout)))
labels_south$set <- "cps"
labels_south$set[7:12] <- "mcd"

fig1b <- ggplot(data= data_south, aes(x=Year, y=Turnout))
fig1b <- fig1b + geom_point(data=mcd_south, aes(y=Turnout, x=Year), color="gray60", size=2.5)
fig1b <- fig1b + geom_pointrange(aes(ymin=Turnout_low, ymax=Turnout_upp), color="black", size=1.25, fatten=1.5)
fig1b <- fig1b + scale_y_continuous("Voter Turnout", labels=percent_format(1), limits=c(0.35,0.7), breaks=seq(0.4,0.7,0.1))
fig1b <- fig1b + scale_x_continuous(name=NULL, limits=c(2008,2019), breaks=seq(2008,2018,2))
fig1b <- fig1b + geom_text_repel(data=labels_south, aes(y=Turnout, x=Year, label=percent_format(0.1)(Turnout), color=set), nudge_x=0.75, direction="y")
fig1b <- fig1b + scale_color_manual(values=c("black","gray60"))
fig1b <- fig1b + displayprefs

quartz(width=5.5,height=2.75, type="pdf",file="fig1b.pdf")
fig1b
dev.off()