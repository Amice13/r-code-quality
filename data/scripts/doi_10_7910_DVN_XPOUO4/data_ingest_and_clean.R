
#####SET THE BELOW GLOBAL
figPath = "~/Dropbox/Apps/Overleaf/Software Quality Metareview/figures/"

### This script:
### 1 - loads up the papers meta-meta review data
### 2 - drops unneeded columns
### 
### Associated with quality_metadownload.py
###

### 1 - loads up the papers meta-meta review data

library(data.table)

#filenames and locations

datadir = "data/"
filename = "quality_dataset.tsv"

load.sheet <- function (f) {
  name_and_path <- paste(datadir, "/", f, sep="")
 # colNames <- read.csv(name_and_path, skip=2, nrows=1, sep='\t')
  #d.tmp <- read.csv(name_and_path, header=FALSE, stringsAsFactors = FALSE, skip=3, sep='\t')
  d.tmp <- read.csv(name_and_path, header=TRUE, stringsAsFactors = FALSE, sep='\t')
  return(d.tmp)
}

df <- load.sheet(filename)

#Header
#1:10 id	source	title	journal_or_conf	venue	date	facet	range	rigor	qty_papers	
#11:20 method	data_avail	by_study	theme	vote_count	true_quant	other	qual_model	bucket	design_arch	
#21:23 single_product	multi_product	org_or_community	

recodeList = c(13:16, 20:23)

recode_yn <- function(df) {
  for (i in recodeList)  {
    df[,i] <- ifelse(df[,i] == "yes", 1, ifelse(df[,i] == "no", 0, NA))
  }
  return(df)
}

cleanDF <- recode_yn(df)
cleanDF$sid <- paste0("S", cleanDF$id, ", n = ", cleanDF$qty_papers) 

cleanDF$niceBucket <- ifelse(cleanDF$bucket=="Bad-smell", "Quality is Heuristic", 
             ifelse(cleanDF$bucket== "Defect-Prediction", "Quality is Structural", 
             ifelse(cleanDF$bucket=="Dependability", "Quality is Dependability", 
             ifelse(cleanDF$bucket=="Holistic", "Quality is Holistic", 
             ifelse(cleanDF$bucket=="Longitudinal", "Quality is Maintainability", NA))))) 

#set order
cleanDF$niceBucket <- ordered(cleanDF$niceBucket, levels=c('Quality is Heuristic', 'Quality is Holistic', 'Quality is Maintainability', 'Quality is Structural', 'Quality is Dependability'))

missing = cleanDF[is.na(cleanDF)]

library(tidyr)
#errors -- why is it here? maybe errors when value is NA?
cleanDF <- cleanDF %>% separate(col=range, sep="-", into=c('startYear', 'endYear'))

cleanDF$qty_papers <- as.integer(cleanDF$qty_papers)
cleanDF$date <- as.integer(cleanDF$date)
cleanDF$startYear <- as.integer(cleanDF$startYear)
cleanDF$endYear <- as.integer(cleanDF$endYear)
cleanDF <- cleanDF[!is.na(cleanDF$id),]

#make a plot of review duration and paper quantity

cleanDF <- cleanDF[order(as.numeric(cleanDF$date)),]
library('ggplot2')
library('ggrepel')
a <- ggplot(cleanDF, aes(x=date, y=qty_papers))
timePlot <- a + 
  scale_linetype_identity() +
  geom_segment(data=cleanDF, aes(x=startYear, xend=endYear, 
                                   y=qty_papers, yend=qty_papers), size=1, alpha=.4) +
  geom_segment(data=cleanDF, aes(x=endYear, xend=date, 
                                   y=qty_papers, yend=qty_papers, linetype="dotted")) +
  geom_rug(sides="b", alpha=.15, size=3, color="blue") +
  scale_y_log10() +
  labs(y="Number of Papers Reviewed", x="Date Range of Studies Summarized - - Publication Year") +
  theme(panel.background = element_blank(),
        axis.text.x = element_text(size=8,angle=0,hjust=.5,vjust=.5,face="plain"),
        legend.position = "none",
        panel.grid = element_blank(),
        axis.line = element_line(color = "black")) +
  geom_point(aes(x=date, y=qty_papers), fill="blue", shape=21, color="blue") +
  #geom_text_repel(aes(label=id), hjust=0, vjust=0, segment.size=0.2, size=2, point.padding=NA)
  facet_wrap( ~ bucket, ncol=2)
timePlot

png(paste0(figPath, "timePlot.png"), width=9, height=6, units='in', res=300)
timePlot
dev.off()
timePlot

vTitle = "Publication Venue"
pubPlot <- ggplot(cleanDF, aes(x=startYear)) +
  scale_linetype_identity() +
  scale_y_discrete() +
  geom_segment(data=cleanDF, aes(x=startYear, xend=endYear, color=journal_or_conf,
                                   y=qty_papers, yend=qty_papers), size=3, alpha=.3) +
  theme(panel.background = element_blank(),
        axis.text.x = element_text(size=8,angle=0,hjust=.5,vjust=.5,face="plain"),
        legend.position = "right",
        panel.grid = element_blank(),
        axis.line = element_line(color = "black")) +
  #labs(y="Number of Studies", x="Date Range of Studies Summarized, by Dominant Theme", color=vTitle) +
  labs(x="Date Range of Studies Summarized, by Venue", color=vTitle) +
  geom_point(aes(x=date, y=qty_papers), fill="lightblue", shape=21, color="blue") +
  geom_text_repel(aes(label=sid, y=qty_papers, x=date), hjust=0, vjust=0, segment.size=0.2, size=2, point.padding=NA) +
  #facet_wrap( ~ bucket, ncol=1, labeller=labeller(bucket = fLabels))
  facet_wrap( ~ niceBucket, ncol=1)

pubPlot

vTitle = "Theme"

pubPlot2 <- ggplot(cleanDF, aes(x=startYear, y=as.character(id))) +
  scale_linetype_identity() +
  scale_y_discrete(breaks=cleanDF$id, labels=cleanDF$sid) +
  geom_segment(data=cleanDF, aes(x=startYear, xend=endYear, color=niceBucket, y=id, yend=id))+#, size=3, alpha=.3) +
  labs(x="Date Range of Studies Summarized", y="Study Number and Size", color=vTitle) +
  geom_point(aes(x=date), fill="lightblue", shape=21, color="blue") +
  theme_bw() +
  theme(legend.position="bottom")

pubPlot2

yearList <- unique(cleanDF$startYear)
trialDF <- cleanDF
#this is done previously
#idea: ids are all wild order, so need to level them according to their year
#trialDF$id <- factor(trialDF$id, levels=trialDF$id[sort(yearList)])
trialDF$niceBucket <- factor(trialDF$niceBucket, levels=c(
  "Quality is Heuristic"                   ,
  "Quality is Holistic"                     ,
  "Quality is Maintainability"          ,
  "Quality is Structural" ,
  "Quality is Dependability"               
))
#trialDF$id <- factor(trialDF$id, levels=trialDF$id[order(trialDF$niceBucket, trialDF$startYear, decreasing=TRUE)])
trialDF$id <- factor(trialDF$id, levels=trialDF$id[order(trialDF$niceBucket, trialDF$date, decreasing=TRUE)])

  
#trialDF <- trialDF[order(trialDF$niceBucket, trialDF$startYear, decreasing=TRUE),]
pubPlot3 <- ggplot(trialDF, aes(x=startYear, y=as.character(id))) +
  scale_linetype_identity() +
  scale_y_discrete(breaks=trialDF$id, labels=trialDF$sid) +
  geom_segment(data=trialDF, aes(x=startYear, xend=endYear, color=niceBucket, y=id, yend=id))+#, size=3, alpha=.3) +
  labs(x="Date Range of Studies Summarized", y="Study Number and Size", color=vTitle) +
  geom_point(aes(x=date), fill="lightblue", shape=21, color="blue", size=1) +
  theme_bw() +
  theme(legend.position="bottom") +
  #theme(axis.text.y=element_text(size=6, margin(20, 20, 20, 20, unit="pt")))
  theme(axis.text.y=element_text(size=6))

pubPlot3

png(paste0(figPath, "pubPlot.png"), width=9, height=6, units='in', res=300)
pubPlot3
dev.off()
pubPlot3

trialDF$id <- factor(trialDF$id, levels=trialDF$id[order(trialDF$niceBucket, trialDF$startYear, decreasing=TRUE)])
pubPlot4 <- ggplot(trialDF, aes(x=startYear, y=as.character(id))) +
  scale_linetype_identity() +
  scale_y_discrete(breaks=trialDF$id, labels=trialDF$sid) +
  geom_segment(data=trialDF, aes(x=startYear, xend=endYear, color=niceBucket, y=id, yend=id))+#, size=3, alpha=.3) +
  labs(x="Date Range of Studies Summarized", y="Study Number and Size", color=vTitle) +
  geom_point(aes(x=date), fill="lightblue", shape=21, color="blue", size=1) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position="bottom") +
  #theme(axis.text.y=element_text(size=6, margin(20, 20, 20, 20, unit="pt")))
  theme(axis.text.y=element_text(size=6))

pubPlot4

png(paste0(figPath, "pubPlotRetrospective.png"), width=9, height=7, units='in', res=300)
pubPlot4
dev.off()
pubPlot4


#make a plot of articles per year
library('ggplot2')
b <- ggplot(cleanDF, aes(x=date))
  b + geom_histogram()

  
#make a plot of method type; data needs to be clean for this to be sensible
library('ggplot2')
c <- ggplot(cleanDF, aes(x=method))
  c + geom_histogram(stat="count")


