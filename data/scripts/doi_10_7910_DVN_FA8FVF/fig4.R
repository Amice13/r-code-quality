print("Running fig4.R...")

load("../results/fig4_data.RData")

library(ggplot2)
library(metafolio)
library(ggalt)
set.seed(02138)

r1.1 = read.csv("../data/rankings_resp7.csv")
r1.2 = read.csv("../data/rankings_resp7b.csv")

r2.1 = read.csv("../data/rankings_resp1.csv")
r2.2 = read.csv("../data/rankings_resp1b.csv")

r3.1 = read.csv("../data/rankings_resp3.csv")
r3.2 = read.csv("../data/rankings_resp3b.csv")


## Note: the scripts that generate these data (mturker_rankings_followup.R) is protected by IRB
temp = merge(r1.1, r1.2, by="V1")
plot1 <- ggplot(temp, aes(x=rank.x, y=rank.y)) + xlab("T1") + ylab("T2") + geom_point(size=1) +
  geom_abline(intercept=0, slope=1) + coord_fixed() +
  scale_x_discrete(limit = c(1,25,50,75, 100), labels=c(1,25,50,75,100)) +
  scale_y_discrete(limit = c(1,25,50,75, 100), labels=c(1,25,50,75,100)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size=35),
        axis.title.x = element_text(size=35),
        axis.text.y = element_text(size=35),
        axis.title.y = element_text(size=35),
        legend.position = "none",
        axis.line.x = element_line(color="black", size = .5),
        axis.line.y = element_line(color="black", size = .5),
        plot.margin=unit(c(1.2,2,1.2,1.2),"cm")) +
  ggplot2::annotate("text", label="paste(rho, \" = .90\")", parse = TRUE, x = 75, y=25, col="red", parse=T, size=14)

temp = merge(r2.1, r2.2, by="V1")
plot2 <- ggplot(temp, aes(x=rank.x, y=rank.y)) + xlab("T1") + ylab("T2") + geom_point(size=1) +
  geom_abline(intercept=0, slope=1) + coord_fixed() +
  scale_x_discrete(limit = c(1,25,50,75, 100), labels=c(1,25,50,75,100)) +
  scale_y_discrete(limit = c(1,25,50,75, 100), labels=c(1,25,50,75,100)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size=35),
        axis.title.x = element_text(size=35),
        axis.text.y = element_text(size=35),
        axis.title.y = element_text(size=35),
        legend.position = "none",
        axis.line.x = element_line(color="black", size = .5),
        axis.line.y = element_line(color="black", size = .5),
        plot.margin=unit(c(1.2,2,1.2,1.2),"cm")) +
  ggplot2::annotate("text", label="rho == 0.92", x = 75, y=25, col="red", parse=T, size=14)


temp = merge(r3.1, r3.2, by="V1")
plot3 <- ggplot(temp, aes(x=rank.x, y=rank.y)) + xlab("T1") + ylab("T2") + geom_point(size=1) +
  geom_abline(intercept=0, slope=1) + coord_fixed() +
  scale_x_discrete(limit = c(1,25,50,75, 100), labels=c(1,25,50,75,100)) +
  scale_y_discrete(limit = c(1,25,50,75, 100), labels=c(1,25,50,75,100)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size=35),
        axis.title.x = element_text(size=35),
        axis.text.y = element_text(size=35),
        axis.title.y = element_text(size=35),
        legend.position = "none",
        axis.line.x = element_line(color="black", size = .5),
        axis.line.y = element_line(color="black", size = .5),
        plot.margin=unit(c(1.2,1.2,1.2,2),"cm")) +
  ggplot2::annotate("text", label="rho == 0.84", x = 75, y=25, col="red", parse=T, size=14)




gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

rand = replicate(1000, cor(sample(1:100), sample(1:100)))
df = data.frame(val = c(intracoder[-2], rand), source = c(rep("Truth", length(intracoder)-1), rep("Random", 1000)))
plot4 <- ggplot(df, aes(x=val, fill=source)) + geom_bkde(alpha = 0.5, bandwidth=.05, truncate=T, color="black") +
  scale_fill_manual(values=c(NA, gg_color_hue(3)[1]))+
  scale_x_continuous(limits=c(-.6,1)) +
  scale_y_continuous(limits = c(0,6), expand = c(0,0)) +
  geom_segment(x=-0.5, y=0, xend=1, yend=0) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size=35),
        axis.title.x = element_text(size=35),
        axis.line.x = element_blank(),
        axis.text.y=element_blank(),
        plot.margin=unit(c(1,1.2,1.2,2),"cm")) +
  xlab("Intracoder Correlation") + ylab("") +
  annotate("text", x=.56, y=4.5, label= "Ranking", col=gg_color_hue(3)[1], size=14) + 
  annotate("text", x = -.31, y=3.8, label = "Random", col="black", size=14)
plot4


### Multiplot function as defined by the R Cookbook:
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



png(filename = "../results/fig4.png", width=1500, height=1500, bg = "white", res=100, units="px")
multiplot(plot1,plot3,plot2,plot4, cols=2)
dev.off()