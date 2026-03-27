setwd("/Replication Materials/")
library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_bw())
library(readstata13)

XP1 <- read.dta13("replication-data-experiment1.dta")
XP1 <- XP1[XP1$white == 1 ,]

names(XP1)


head(XP1)
summary(XP1)
XP1$source <- factor(XP1$source , levels = 0:2, labels = c("None", "CNN", "Fox\nNews"))
XP1$sgtRace  <- factor(XP1$sgtRace , levels = 0:4, labels = c("None", "White", "Black", "Latino", "Middle Eastern" ))
 
#Table 2, Models:
summary(lm(dvConvict ~ factor(sgtRace)*factor(source), data = XP1))
summary(lm(dvJustified ~ factor(sgtRace)*factor(source), data = XP1))





toPlot3 <- XP1 %>% group_by(sgtRace, source) %>% summarize(MM = mean(dvConvict , na.rm=TRUE),
                                                          NN = length(dvConvict ),
                                                          SD = sd(dvConvict , na.rm=TRUE),
                                                          SE = SD/sqrt(NN),
                                                          LOW = MM +qt(.025, df = NN)*SE ,
                                                          HIGH = MM -qt(.025, df = NN)*SE ,
                                                          VV = "Convict"  )
toPlot3 <- toPlot3[complete.cases(toPlot3),]
toPlot3
 

figure2 <- qplot(data = toPlot3, x = source, y = MM, geom = "pointrange", colour = factor(VV),
            ymin = LOW, ymax = HIGH, ylim = 0:1, ylab = "Agree that Sgt.\nShould Be Convicted" , xlab = "Source" 
) + guides(colour = "none")  + facet_wrap(facets = ~sgtRace, nrow = 1)
figure2 <- figure2 + geom_hline(yintercept =  0.6362, lty = 2, colour = "grey40") +scale_colour_manual(values = c("dodgerblue4"  ))


figure2
getwd()
ggsave(figure2, file = "replication-finalFigure2.png", dpi = 1200)
#######################################################

names(XP2)
XP2 <- read.dta13("replication-data-experiment2.dta")
XP2 <- XP2[XP2$white == 1 & XP2$manipcheck == 2  ,]



head(XP2)
summary(XP2) 
XP2$newsSource <- factor(XP2$newsSource  , levels = 0:2, labels = c("None", "CNN", "Fox\nNews"))
XP2$sgtRace  <- factor(XP2$sgtRace , levels = 0:4, labels = c("None", "White", "Black", "Latino", "Middle Eastern" ))


#Table 3, Models:
summary(lm(dvconvict01     ~ factor(sgtRace)*factor(newsSource), data = XP2))
summary(lm(dvjustified01   ~ factor(sgtRace)*factor(newsSource), data = XP2))




toPlot3 <- XP2 %>% group_by(sgtRace, newsSource) %>% summarize(MM = mean(dvjustified01  , na.rm=TRUE),
                                                           NN = length(dvjustified01  ),
                                                           SD = sd(dvjustified01  , na.rm=TRUE),
                                                           SE = SD/sqrt(NN),
                                                           LOW = MM +qt(.025, df = NN)*SE ,
                                                           HIGH = MM -qt(.025, df = NN)*SE ,
                                                           VV = "Justified"  )
toPlot3 <- toPlot3[complete.cases(toPlot3),]
toPlot3


figure3 <- qplot(data = toPlot3, x = newsSource, y = MM, geom = "pointrange", colour = factor(VV),
            ymin = LOW, ymax = HIGH, ylim = 0:1, ylab = "Agree that Sgt.\nWas Justified" , xlab = "Source" 
) + guides(colour = "none")  + facet_wrap(facets = ~sgtRace, nrow = 1)
figure3 <- figure3 + geom_hline(yintercept =  0.703, lty = 2, colour = "grey40") +scale_colour_manual(values = c("dodgerblue4"  ))


figure3

ggsave(figure3, file = "replication-finalFigure3.png", dpi = 1200)