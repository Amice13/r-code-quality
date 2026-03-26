print("Runing fig3.R...")

load("../results/fig3_data.RData")

library(ggplot2)
library(metafolio)
library(ggalt)

plot1 <- ggplot(time1, aes(x=rank_1, y=rank_1.4)) + xlab("Ranker 1") + ylab("Ranker 5") + geom_point(size=1) +
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
        axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        plot.margin=unit(c(1,1.2,1.2,2),"cm")) +
  ggplot2::annotate("text", label="rho == 0.77", x = 75, y=25, col="red", parse=T, size=14)

plot2 <- ggplot(time1, aes(x=rank_1.2, y=rank_1.5)) + xlab("Ranker 3") + ylab("Ranker 6") + geom_point(size=1) +
  geom_abline(intercept=0, slope=1) +coord_fixed() +
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
        axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        plot.margin=unit(c(1,1.2,1.2,2),"cm")) +
  ggplot2::annotate("text", label="rho == 0.81", x = 75, y=25, col="red", parse=T, size=14)


plot3 <- ggplot(time1, aes(x=rank_1.1, y=rank_1.3)) + xlab("Ranker 2") + ylab("Ranker 4") + geom_point(size=1) +
  geom_abline(intercept=0, slope=1) +coord_fixed() +
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
        axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        plot.margin=unit(c(1,1.2,1.2,2),"cm")) +
  ggplot2::annotate("text", label="paste(rho, \" = .70\")", x = 75, y=25, col="red", parse=T, size=14)




gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


#plot4 <- ggplot(df, aes(x=val, fill=source)) + geom_density(alpha=0.5, bw=0.04, kernel="gaussian", adjust=.5) +
plot4 <- ggplot(df, aes(x=val, fill=source)) + geom_bkde(alpha = 0.5, bandwidth=0.05, truncate=T, color="black") +
  scale_fill_manual(values=c(NA, gg_color_hue(3)[1]))+
  scale_x_continuous(limits=c(-.6,1)) +
  scale_y_continuous(limits =c(0,6), expand = c(0,0)) +
  geom_segment(x=-0.5, y=0, xend=1, yend=0) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size=35),
        axis.title.x = element_text(size=35),
        legend.position = "none",
        axis.ticks.y=element_blank(),
        axis.line.x = element_blank(),
        axis.text.y=element_blank(),
        plot.margin=unit(c(1,1.2,1.2,2),"cm")) +
  xlab("Intercoder Correlation") + ylab("") +
  annotate("text", x=.58, y=4.5, label= "Ranking", col=gg_color_hue(3)[1], size=14) + 
  annotate("text", x = -.34, y=2.2, label = "Random", col="black", size=14)




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




png(filename = "../results/fig3.png", width=1000, height=1000, bg = "white", units="px")
multiplot(plot1,plot3,plot2,plot4, cols=2)
dev.off()
