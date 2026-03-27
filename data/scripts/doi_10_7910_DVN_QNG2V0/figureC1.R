sink(file="figureC1_log.txt")

# Install and load libraries

install.packages('ggplot2')
install.packages('farver')
install.packages('gridExtra')

library(ggplot2)
library(farver)
library(gridExtra)

# Set the Working Directory
#setwd("")

# Load data
load("tscs.RData")

# Aggregate the # of states controlled by each party in each year
v$Number.of.States <- 1
legi_control <- aggregate(Number.of.States ~ year + legi_control, v, sum)
names(legi_control) <- c("year", "Party", "Number.of.States")


# Aggregate the # of states controlled trifecta in each year
tri_control <- aggregate(Number.of.States ~ year + trifecta, v, sum)
names(tri_control) <- c("year", "Party", "Number.of.States")
tri_control <- tri_control[tri_control$Party!="Divided",]

############################
# Plots
############################

s_plot <- ggplot(legi_control, aes(x = year, y = Number.of.States, color=Party)) +
  geom_line() +
  scale_color_manual(values=c("Blue", "Red", "purple1")) +
  labs(title = "Party Control of State Legislatures",
       x = "",
       y = "Number of States", 
       col="Party") +
  theme_bw() +
  scale_y_continuous(limits=c(0,40)) +
  #scale_x_discrete(limits=c("1995-1999", "2015-2019")) +
  theme(legend.position="none") +   theme(axis.title.x = element_blank()) + 
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1), 
        axis.title=element_text(size=12), 
        plot.title = element_text(size = 14)) 

s_plot

# Trifectas 

s_plot2 <- ggplot(tri_control, aes(x = year, y = Number.of.States, color=Party)) +
  geom_line() +
  scale_color_manual(values=c("Blue", "Red"), labels=c("Democrat", "Republican")) +
  labs(title = "Trifectas in State Governments",
       x = "",
       y = "Number of States", 
       col="Party") +
  theme_bw() +
  scale_y_continuous(limits=c(0,40)) +
  #scale_x_continuous(limits=c(1992,2020)) +
  #scale_x_discrete(limits=c("1995-1999", "2015-2019")) +
  theme(legend.position="none") +   theme(axis.title.x = element_blank()) + 
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1), 
        axis.title=element_text(size=12), 
        plot.title = element_text(size = 14))
s_plot2

stateControl <- grid.arrange(grobs = list(s_plot, s_plot2),
                             ncol = 2, nrow = 1)
ggsave("stateControl1.pdf", stateControl, width = 9.02, height = 4.46)

