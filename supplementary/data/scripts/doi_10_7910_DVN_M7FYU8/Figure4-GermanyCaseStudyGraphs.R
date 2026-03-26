require(ggplot2)
library(grid)
GermanyCurrentDataGraphs <- read.csv("Figure4-GermanyCaseStudyData.csv")
GermanPlots1 <-ggplot (data=GermanyCurrentDataGraphs, aes(x=year, y=cu_trade_gdp),
          shape=cond)+geom_line(colour="black", linetype="solid")+
          scale_y_continuous(name = "Trade balance (% of GDP)", labels = scales::number_format(accuracy = 0.1)) +
          scale_x_continuous(name = "Year", limits = c(1975, 2015)) + 
          theme_bw(base_size = 15) +
#          ggtitle("Trade Balance") +
          geom_vline(xintercept = 1991, linetype = "longdash")
GermanPlots1
ggsave(file="GermanPlotTradeBalance2015.pdf", width=10, height=3)
GermanPlots2 <-ggplot (data=GermanyCurrentDataGraphs, aes(x=year, y=relativePriceLevel),
                      shape=cond)+geom_line(colour="black", linetype="solid")+
  scale_y_continuous(name = "Reer, USA in 2011 = 1", labels = scales::number_format(accuracy = 0.1)) +
  scale_x_continuous(name = "Year", limits = c(1975, 2015)) +
  theme_bw(base_size = 15) +
#  ggtitle("Real Effective Exchange Rate")+
  geom_vline(xintercept = 1991, linetype = "longdash") +
#  annotate("segment", x = 1995, xend = 2008, y = 0.75, yend = 0.75, colour = "black",
#           arrow=arrow(ends="both", angle = 90, length=unit(.2,"cm"))) +
annotate("rect", xmin=1995, xmax = 2008, ymin = 0, ymax = 1.50, alpha = 0.1, fill="grey30") +
 annotate(geom="text", x=2001.5, y=0.7, label="Wage moderation phase", size = 4.5)
GermanPlots2
ggsave(file="GermanPlotReer2015.pdf", width=10, height=3)
