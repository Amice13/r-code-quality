###
#Figure 4
#Figure 4 Change in the Likelihood of Engaging in High and Low-Cost Protesting When Moving from a Country with Minimum to Maximum Level of Unemployment 
##

setwd("")

library("ggplot2")
library("ggthemes")
library("readxl")
library("plyr")


#Open dataset
data <- read_excel("figure4.xlsx")

#a few tweaks before proceeding
library("plyr")
data$variable <- factor(data$variable)
data$demonstration <- factor(data$demonstration)
data$demonstration <- factor(data$demonstration,
                    levels = c(0,1),
                    labels = c("Low-cost protesting","High-cost protesting")) 


data$variable2 <- ordered(data$variable, 
                          levels=c("Extreme right *", "Right", "Centre", "Left", 
                                   "Extreme left", "Female *", "Male", "Less than 30",
                                   "From 30 to 44", "From 45 to 64", "65 and more *",
                                   "Low income *", "Medium income", "High income",
                                   "Low education *", "Medium and high education"))

data$variable2 <- factor(data$variable2, levels = rev(levels(data$variable2)))

myColors <- c("black", "gray")

#graph time!

#GRAPH A
#subset data
data_demo <- data[ which(data$demonstration=="High-cost protesting"), ]

ggplot(data_demo, aes(x=variable2), stat = "identity") +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_pointrange(aes(x = variable2, y = beta, ymin = low95, ymax = upper95),  
                  lwd = 1/2, position = position_dodge(.3),
                  shape = 21, fill = "WHITE", stat = "identity") +
  geom_linerange(aes(x = variable2, ymin = low95 ,
                     ymax = upper95),
                 lwd = 1, position = position_dodge(.3), stat = "identity") +
  ylab("Coefficient + 95% CI") +
  scale_color_manual(values=myColors) +
  coord_flip() + 
  theme_bw() +
  theme(legend.title=element_blank(), 
        legend.position=c(0.86,0.05),
        legend.key = element_rect(fill = "transparent", 
                                  colour = "transparent"),
        axis.title.y=element_blank(), 
        text = element_text(size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_text(size=rel(.85))) +
  geom_vline(xintercept=5.5, color="grey60") +
  annotate("text", x=1, y=-0.04, label="Education", 
           size=5, color="grey60", hjust=0) +
  geom_vline(xintercept=2.5, color="grey60") +
  annotate("text", x=3, y=-0.04, label="Income", 
           size=5, color="grey60", hjust=0) +
  geom_vline(xintercept=5.5, color="grey60") +
  annotate("text", x=6, y=-0.04, label="Age", 
           size=5, color="grey60", hjust=0) +
  geom_vline(xintercept=9.5, color="grey60") +
  annotate("text", x=10, y=-0.04, label="Gender", 
           size=5, color="grey60", hjust=0) +
  geom_vline(xintercept=11.5, color="grey60") +
  annotate("text", x=12, y=-0.04, label="Ideology", 
           size=5, color="grey60", hjust=0) 
#ggsave("coefficients_highcost.png", width = 30, height = 20, units="cm", scale=1.5)

#Graph B

#subset data
data_low <- data[ which(data$demonstration=="Low-cost protesting"), ]

ggplot(data_low, aes(x=variable2), stat = "identity") +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_pointrange(aes(x = variable2, y = beta, ymin = low95, ymax = upper95),  
                  lwd = 1/2, position = position_dodge(.3),
                  shape = 21, fill = "WHITE", stat = "identity") +
  geom_linerange(aes(x = variable2, ymin = low95 ,
                     ymax = upper95),
                 lwd = 1, position = position_dodge(.3), stat = "identity") +
  ylab("Coefficient + 95% CI") +
  scale_color_manual(values=myColors) +
  coord_flip() + 
  theme_bw() +
  theme(legend.title=element_blank(), 
        legend.position=c(0.86,0.05),
        legend.key = element_rect(fill = "transparent", 
                                  colour = "transparent"),
        axis.title.y=element_blank(), 
        text = element_text(size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_text(size=rel(.85))) +
  geom_vline(xintercept=5.5, color="grey60") +
  annotate("text", x=1, y=-0.04, label="Education", 
           size=5, color="grey60", hjust=0) +
  geom_vline(xintercept=2.5, color="grey60") +
  annotate("text", x=3, y=-0.04, label="Income", 
           size=5, color="grey60", hjust=0) +
  geom_vline(xintercept=5.5, color="grey60") +
  annotate("text", x=6, y=-0.04, label="Age", 
           size=5, color="grey60", hjust=0) +
  geom_vline(xintercept=9.5, color="grey60") +
  annotate("text", x=10, y=-0.04, label="Gender", 
           size=5, color="grey60", hjust=0) +
  geom_vline(xintercept=11.5, color="grey60") +
  annotate("text", x=12, y=-0.04, label="Ideology", 
           size=5, color="grey60", hjust=0) 
#ggsave("coefficients_lowcost.png", width = 30, height = 20, units="cm", scale=1.5)


