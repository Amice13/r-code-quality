### Script to generate the theoretical graph in the PS paper

setwd("")


#Generate first the data
df <- data.frame(x = c(0, 5, 10, 15, 20), y = c(0, 3.5, 7.5, 11.5, 15.5), 
                 z = c(8, 10, 12, 14, 16))
#plot
library("ggplot2")
library("gridExtra")

#png("graph_expectations.png", width = 800, height = 600)
exp <- ggplot(df, aes(x)) + 
  geom_line(aes(y = y, colour=6), linetype = 2) + 
  geom_line(aes(y = z, colour=6)) +
  ylab("Probability to protest") +
  scale_x_continuous(breaks=c(0, 5, 10, 15, 20, 25),
                     labels = c("Good economy", "", "", "", "Economic crisis        ", "")) +
  theme_bw(base_size = 18) +
    theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line=element_line()) +
      annotate("text", x = 10, y = 13, label = "High resources") +
  annotate("text", x = 16, y = 10, label = "Low resources") 
ggsave(exp, file="graph_expectations.png", scale=1.2)
exp
#dev.off()



#http://stackoverflow.com/questions/17562379/line-plot-of-multiple-variables-in-r
