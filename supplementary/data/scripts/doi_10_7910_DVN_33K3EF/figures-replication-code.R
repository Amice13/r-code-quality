## If the ggplot and foreign packages are not already installed, un-comment the following line:

# install.packages(c("ggplot2","foreign"))

library(ggplot2)
library(foreign)

##############
## Figure 1 ##
##############

upu2000 <- read.dta("UPU2000.dta")

country.hist.gray <- ggplot(data=upu2000, aes(log(upu2000$upu_totalpo_ipo))) +
      geom_histogram(fill="#bdbdbd", alpha=0.75,color="black",bins=40)  + 
	theme_bw() +
      labs(x="Number of post offices (logged)", y="Observations") +
	scale_x_continuous(breaks=seq(0,12, by = 3))

cairo_pdf(file = "country-hist-gray.pdf", width = 7, height = 6.5, pointsize = 14, family = "Minion Pro")
country.hist.gray
dev.off()



################
## Figure A.1 ##
################

library(rworldmap)
library(RColorBrewer)
library(classInt)
library(rgdal)

###################
## Figure A.1(a) ##
## data coverage ##
###################

data_a1a <- read.csv("data coverage.csv")
data_a1a <- data.frame(data_a1a)

sPDF <- getMap()[-which(getMap()$ADMIN=='Antarctica'),]
sPDF <- spTransform(sPDF, CRS=CRS("+proj=robin +ellps=WGS84"))
sPDF2 <- joinData2Map(data_a1b, nameMap = sPDF, nameJoinColumn="code")

classInt <- classIntervals(sPDF2$years, n=7, style="fixed",fixedBreaks=c(0,37,68,86.5,101.5,120.5,129.5,200))
catMethod <- classInt$brks

par(oma=c(0,0,0,0),mar=c(0,0,0,0))
mapCountryData(sPDF2, nameColumnToPlot="years", addLegend=FALSE,colourPalette=c('#feedde','#fdd0a2','#fdae6b','#fd8d3c','#f16913','#d94801','#8c2d04'),
		catMethod=catMethod,missingCountryCol='lightgrey',borderCol = "lightgrey",
		oceanCol='lightblue',mapTitle="")


########################
## Figure A.1(b)      ##
## POs per 100k, 2000 ##
########################

rm(list=ls(all=TRUE))

data_a1b <- read.csv("per_capita_2000.csv")
data_a1b <- data.frame(data_a1b)
sPDF <- getMap()[-which(getMap()$ADMIN=='Antarctica'),]
sPDF <- spTransform(sPDF, CRS=CRS("+proj=robin +ellps=WGS84"))
sPDF3 <- joinData2Map(data_a1b, nameMap = sPDF, nameJoinColumn="code")

classInt <- classIntervals(sPDF3$per100k, n=9, style="fixed",fixedBreaks=c(0, 1.3, 2.8, 5, 8, 12, 20, 28, 36, 20000000))
catMethod <- classInt$brks

orange <- c('#fff5eb','#fee6ce','#fdd0a2','#fdae6b','#fd8d3c','#f16913','#d94801','#a63603','#7f2704')

par(oma=c(0,0,0,0),mar=c(0,0,0,0))
mapCountryData(sPDF3, nameColumnToPlot="per100k", addLegend=FALSE,colourPalette=orange,
		catMethod=catMethod,missingCountryCol='black',borderCol = "lightgrey",
		oceanCol='lightblue',mapTitle="")


################
## Figure A.2 ##
################

rm(list=ls(all=TRUE))

library(ggplot2)
library(foreign)

dataf1 <- read.dta("data-facet1.dta")

dataf1$country_name[dataf1$country_name=="Congo_Democratic Republic of"] <- "Congo"
dataf1$country_name[dataf1$country_name=="Congo_Republic of the"] <- "Congo, Republic of"
dataf1$country_name[dataf1$country_name=="Korea_North"] <- "Korea (North)"
dataf1$country_name[dataf1$country_name=="Korea_South"] <- "Korea (South)"

facet1 <- ggplot() + geom_line(data = dataf1, aes(x = year, y=postpc),stat="identity") +
   facet_wrap(~country_name,scales="free_y",ncol=7) +
 theme(strip.text = element_text(size = 7), panel.spacing = unit(.5, 'pt'))  + theme_bw(base_size=9) +
labs(x="", y="Post offices per 100,000") + theme(strip.text = element_text(size = 7)) +
scale_y_continuous(breaks = scales::pretty_breaks(3), limits = c(0, NA)) 

cairo_pdf(file = "facet1.pdf", width = 12, height = 8, pointsize = 14, family = "Minion Pro")
facet1
dev.off()

################
## Figure A.3 ##
################

dataf2 <- read.dta("data-facet2.dta")

dataf2$country_name[dataf2$country_name=="Vietnam_Democratic Republic of"] <- "Vietnam (North)"
dataf2$country_name[dataf2$country_name=="Vietnam_Republic of"] <- "Vietnam (South)"

facet2 <- ggplot() + geom_line(data = dataf2, aes(x = year, y=postpc),stat="identity") +
   facet_wrap(~country_name,scales="free_y",ncol=7) +
 theme(strip.text = element_text(size = 7), panel.spacing = unit(.5, 'pt'))  + theme_bw(base_size=9) +
labs(x="", y="Post offices per 100,000") + theme(strip.text = element_text(size = 7)) +
scale_y_continuous(breaks = scales::pretty_breaks(3), limits = c(0, NA)) 

cairo_pdf(file = "facet2.pdf", width = 12, height = 8, pointsize = 14, family = "Minion Pro")
facet2
dev.off()

################
## Figure A.4 ##
################

rm(list=ls(all=TRUE))

modata <- read.dta("money_orders.dta")

mo.hist <- ggplot(data=modata, aes(modata$ln_mo)) +
      geom_histogram(fill="#c0392b", alpha=0.75,color="black")  + 
	theme_bw() +
      labs(x="Number of domestic money orders issued (logged)", y="Observations") 

cairo_pdf(file = "mo-hist.pdf", width = 6, height = 6.5, pointsize = 14, family = "Minion Pro")
mo.hist
dev.off()


################
## Figure A.5 ##
################

year.hist <- ggplot(data = modata, aes(x = ln_mo)) +
  geom_histogram(fill="#c0392b", alpha=0.75,color="black",size=.25) +
  facet_wrap(~year) + theme_bw() + labs(x="Number of domestic money orders issued (logged)", y="Observations") 

cairo_pdf(file = "year-hist.pdf", width = 6, height = 6.5, pointsize = 14, family = "Minion Pro")
year.hist
dev.off()


################
## Figure A.6 ##
################

p5 <- ggplot(modata, aes(x=ln_po, y=ln_mo)) +
    geom_point(col="steelblue1",alpha=1/3,size=3)  +
	geom_smooth(method=lm,col="darkblue",size=2,linetype="dashed") +
	geom_smooth(method=loess,col="violetred1",size=2) + 
	theme_bw() + 
labs(x="Post offices (logged)",y="Money order transactions (logged)") +   theme(axis.ticks = element_blank()) +
 theme(axis.title.x = element_text(size=10)) 

cairo_pdf(file = "po-mo.pdf", width = 6, height = 6.5, pointsize = 14, family = "Minion Pro")
p5
dev.off()



