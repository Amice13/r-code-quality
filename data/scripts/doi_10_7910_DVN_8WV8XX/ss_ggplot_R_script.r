#plotting duration and no. of follows for each focal offspring in each behaviour
#written by T Revathe

rm(list=ls())

#read data
xdata=read.csv(file="ss_ggplot.txt", header=T,
                 stringsAsFactors=T, sep="\t")
str(xdata)
names(xdata)

library(ggplot2)
library(viridis)

#plotting no. of follows for each indiviudal in each behaviour
#------------------------------------------
# Set the size (width and height) in inches

dev.new()
options(repr.plot.width=16, repr.plot.height=10)

plot <-ggplot(xdata, aes(fill=offspringID, y=no_of_follows, x=behaviour_code)) + 
		geom_bar(position="stack", stat="identity", color = "black")+ #,show.legend = FALSE
			scale_fill_viridis(option = "D",discrete = TRUE) +
				labs(x = "Maternal Behaviour", y = "Total number of follows")+
					theme_bw() +
						scale_y_continuous(
						limits = c(0, 900),
						breaks = seq(0, 900, by = 100),
						labels = seq(0, 900, by = 100))+
							theme(
							panel.grid.major.x = element_blank(),
							panel.grid.major.y = element_blank(),
							panel.grid.minor = element_blank(),
							axis.line.x = element_line(),
							axis.line.y = element_line(),
							axis.text=element_text(size=12),
							axis.title=element_text(size=14),
							legend.text=element_text(size=12),
							legend.title=element_text(size=14))
  
#plotting total duration of follows for each individual in each behaviour
#------------------------------------------------------------------------

follow_dur=aggregate(x=xdata$duration_of_follows, by=list(behaviour=xdata$behaviour_code), FUN=sum) 
max_follow_dur=max(follow_dur$x) #10251.48


# Set the size (width and height) in inches
dev.new()
options(repr.plot.width=16, repr.plot.height=10)

ggplot(xdata, aes(fill=offspringID, y=duration_of_follows, x=behaviour_code)) + 
		geom_bar(position="stack", stat="identity", color = "black")+
			scale_fill_viridis(option = "D",discrete = TRUE) +
				labs(x = "Maternal Behaviour", y = "Total duration of follows")+
					theme_bw() +
						scale_y_continuous(
						limits = c(0, 8000),
						breaks = seq(0, 8000, by = 1000),
						labels = seq(0, 8000, by = 1000))+
							theme(
							panel.grid.major.x = element_blank(),
							panel.grid.major.y = element_blank(),
							panel.grid.minor = element_blank(),
							axis.line.x = element_line(),
							axis.line.y = element_line(),
							axis.text=element_text(size=12),
							axis.title=element_text(size=14),
							legend.text=element_text(size=12),
							legend.title=element_text(size=14))


