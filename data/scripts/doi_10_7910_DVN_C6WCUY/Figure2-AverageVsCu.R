library(ggplot2)
cu.gdp <- read.csv(file = "Figure2-AverageVsCuData.csv", header = TRUE, stringsAsFactors = FALSE)

###draw the x-,y-axis
require(grid)
tick.x=data.frame(x=c(-5,-4,-3,-2,-1,1,2,3,4,5,6),
             y=c(0,0,0,0,0,0,0,0,0,0,0),
             vx=c(-5,-4,-3,-2,-1,1,2,3,4,5,6),
             vy=c(-0.25,-0.25,-0.25,-0.25,-0.25, -0.25,-0.25,-0.25,-0.25,-0.25,-0.25))
tick.y=data.frame(x=c(0,0,0,0,0,0),
             y=c(-15,-10,-5,5,10,15),
             vx=c(-0.05,-0.05,-0.05,-0.05, -0.05,-0.05),
             vy=c(-15,-10,-5,5,10,15))

###create a new column that contains that maximum cu value of each country
max.cu <- with(cu.gdp, tapply(cu.gdp$cu, cu.gdp$iso, max, na.rm = TRUE))
cu.gdp$max.cu <- max.cu[match(cu.gdp$iso,names(max.cu))]

###create a dataframe that contains the avg. value of CU
avg.cu <- subset(cu.gdp, year == 2010)


ggplot() + geom_segment(aes(x=x, y=y, xend=vx, yend=vy), size=0.05, color="black", data=tick.x) +
  geom_hline(yintercept = 0, size = 0.05) +
  geom_vline(xintercept = 0, size = 0.05) +
  geom_text(data= tick.x, aes(x, y, label=c(-5,-4,-3,-2,-1,1,2,3,4,5,6)), size=2, vjust=2, hjust=0.5) +
  geom_text(data= tick.y, aes(x, y, label=c(-15,-10,-5,5,10,15)), size=2, vjust=0.5, hjust=1.8) +
  geom_point(data = cu.gdp, aes(x = avg, y = cu), pch = 21, color = "darkgrey", size = 2, fill = "white") +
  geom_point(data = avg.cu, aes(x = avg, y = avg), pch = 15, size = 1.6) +
  labs(y="Current Account/GDP", x="Average Current Account/GDP 1950-2015") +
  geom_text(data= cu.gdp, aes(avg, max.cu, label=iso),size=2.4, vjust=-2, hjust=0.5) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title=element_text(size = 12,face="bold"))



ggsave(file = "Fig2-AverageVsCu.pdf", height = 6, width = 8.1)
