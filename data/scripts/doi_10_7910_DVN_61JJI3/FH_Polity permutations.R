### Multiple Measurements, Elusive Agreement, and Unstable Outcomes in the Study of Regime Change
### Combinations of Freedom House & Polity subcomponent indicators
### Hans Lueders and Ellen Lust
### September 2017


rm(list=ls())
library(Rmisc)
library(ggplot2)
library(ggthemes)
library(foreign)
library(plyr)


# Freedom House subscore-level --------------------------------------------
# load data
fh.subscores <- read.csv("FH_subscores_all years.csv")
fh.subscores$pr.sum <- fh.subscores$A + fh.subscores$B + fh.subscores$C
fh.subscores$cl.sum <- fh.subscores$D + fh.subscores$E + fh.subscores$F + fh.subscores$G

# Political Rights
fh.pr <- seq(from=0, to= 12, by=1)
fh.pr <- as.data.frame(rep(fh.pr, 17))

fh.pr[,2] <- c(rep(0,13), rep(1,13), rep(2,13), rep(3,13), rep(4,13), rep(5,13), 
               rep(6,13), rep(6,13), rep(8,13), rep(9,13), rep(10,13), rep(11,13),
               rep(12,13), rep(13,13), rep(14,13), rep(15,13), rep(16,13))

fh.pr <- rbind(fh.pr, fh.pr, fh.pr, fh.pr, fh.pr, fh.pr, fh.pr, fh.pr, fh.pr,
               fh.pr, fh.pr, fh.pr, fh.pr)
fh.pr[,3] <- c(rep(0,221), rep(1,221), rep(2,221), rep(3,221), rep(4,221), rep(5,221), 
               rep(6,221), rep(6,221), rep(7,221), rep(9,221), rep(10,221), rep(11,221),
               rep(12,221))

fh.pr[,4] <- fh.pr[,1] + fh.pr[,2] + fh.pr[,3]
colnames(fh.pr) <- c("A", "B", "C", "sum")

fh.pr$score <- ifelse(fh.pr$sum>=0 & fh.pr$sum<=5, 7, 
                      ifelse(fh.pr$sum>=6 & fh.pr$sum<=11, 6,
                             ifelse(fh.pr$sum>=12 & fh.pr$sum<=16, 5,
                                    ifelse(fh.pr$sum>=18 & fh.pr$sum<=23, 4,
                                           ifelse(fh.pr$sum>=24 & fh.pr$sum<=29, 3,
                                                  ifelse(fh.pr$sum>=30 & fh.pr$sum<=35,2,1))))))


# Civil Liberties
fh.cl <- seq(from=0, to=16, by=1)
fh.cl <- as.data.frame(rep(fh.cl, 13))

fh.cl[,2] <- c(rep(0,17), rep(1,17), rep(2,17), rep(3,17), rep(4,17), rep(5,17), 
               rep(6,17), rep(6,17), rep(8,17), rep(9,17), rep(10,17), rep(11,17),
               rep(12,17))

fh.cl <- rbind(fh.cl, fh.cl, fh.cl, fh.cl, fh.cl, fh.cl, fh.cl, fh.cl, fh.cl, 
               fh.cl, fh.cl, fh.cl, fh.cl, fh.cl, fh.cl, fh.cl, fh.cl)
fh.cl[,3] <- c(rep(0,221), rep(1,221), rep(2,221), rep(3,221), rep(4,221), rep(5,221), 
               rep(6,221), rep(6,221), rep(7,221), rep(9,221), rep(10,221), rep(11,221),
               rep(12,221), rep(13,221), rep(14,221), rep(15,221), rep(16,221))

fh.cl <- rbind(fh.cl, fh.cl, fh.cl, fh.cl, fh.cl, fh.cl, fh.cl, fh.cl, fh.cl, 
               fh.cl, fh.cl, fh.cl, fh.cl, fh.cl, fh.cl, fh.cl, fh.cl)
fh.cl[,4] <- c(rep(0,3757), rep(1,3757), rep(2,3757), rep(3,3757), rep(4,3757), rep(5,3757), 
               rep(6,3757), rep(7,3757), rep(8,3757), rep(9,3757), rep(10,3757), rep(11,3757), 
               rep(12,3757), rep(13,3757), rep(14,3757), rep(15,3757), rep(16,3757))

fh.cl[,5] <- fh.cl[,1] + fh.cl[,2] + fh.cl[,3] + fh.cl[,4]
colnames(fh.cl) <- c("D", "E", "F", "G", "sum")


fh.cl$score <- ifelse(fh.cl$sum>=0 & fh.cl$sum<=7, 7, 
                      ifelse(fh.cl$sum>=8 & fh.cl$sum<=16, 6,
                             ifelse(fh.cl$sum>=17 & fh.cl$sum<=25, 5,
                                    ifelse(fh.cl$sum>=26 & fh.cl$sum<=34, 4,
                                           ifelse(fh.cl$sum>=35 & fh.cl$sum<=43, 3,
                                                  ifelse(fh.cl$sum>=44 & fh.cl$sum<=52,2,1))))))


## Empirical distributions
# For PR score:
fh.subscores$pr.score <- ifelse(fh.subscores$pr.sum>=0 & fh.subscores$pr.sum<=5, 7, 
                                ifelse(fh.subscores$pr.sum>=6 & fh.subscores$pr.sum<=11, 6,
                                       ifelse(fh.subscores$pr.sum>=12 & fh.subscores$pr.sum<=16, 5,
                                              ifelse(fh.subscores$pr.sum>=18 & fh.subscores$pr.sum<=23, 4,
                                                     ifelse(fh.subscores$pr.sum>=24 & fh.subscores$pr.sum<=29, 3,
                                                            ifelse(fh.subscores$pr.sum>=30 & fh.subscores$pr.sum<=35,2,1))))))

count.pr.emp <- count(count(fh.subscores[,c("A", "B", "C", "pr.sum")])[,4])
count.pr.score.emp <- count(count(fh.subscores[,c("A", "B", "C", "pr.score")])[,4])


# For CL score:
fh.subscores$cl.score <- ifelse(fh.subscores$cl.sum>=0 & fh.subscores$cl.sum<=7, 7, 
                                ifelse(fh.subscores$cl.sum>=8 & fh.subscores$cl.sum<=16, 6,
                                       ifelse(fh.subscores$cl.sum>=17 & fh.subscores$cl.sum<=25, 5,
                                              ifelse(fh.subscores$cl.sum>=26 & fh.subscores$cl.sum<=34, 4,
                                                     ifelse(fh.subscores$cl.sum>=35 & fh.subscores$cl.sum<=43, 3,
                                                            ifelse(fh.subscores$cl.sum>=44 & fh.subscores$cl.sum<=52,2,1))))))

count.cl.emp <- count(count(fh.subscores[,c("D", "E", "F", "G", "cl.sum")])[,5])
count.cl.score.emp <- count(count(fh.subscores[,c("D", "E", "F", "G", "cl.score")])[,5])


# Plots
# First, histogram for Political Rights subscore
pr.plot <- ggplot() + 
  geom_histogram(data=fh.pr, aes(sum), binwidth=1, fill="gray75", col="gray20") +
  geom_segment(data=count.pr.emp, aes(x=x , xend=x, y=1, yend=freq), 
               col="gray30", size = 2.5, lineend = "butt") +
  geom_vline(xintercept=c(5.5, 11.5, 17.5, 23.5, 29.5, 35.5), col="red", alpha=0.8) +   
  theme_bw() + 
  xlab("Political Rights Subscore") + ylab("Number of Combinations (log)") + 
  ggtitle("Political Rights") + 
  theme(axis.title=element_text(size=13,face="bold")) +
  theme(plot.title = element_text(size=16, face="bold", colour="black",
                                  hjust=.5)) +  
  scale_x_continuous(breaks = seq(0,40,10),
                     labels = c("0", "10", "20", "30", "40"),
                     lim = c(-0.5,40.5))  +
  scale_y_log10(breaks=c(1, 2, 5, 10, 25, 50, 100)) +
  annotate("text", x = 2.5, y = 0.8, label = "7", col="red", size=4) +
  annotate("text", x = 8.5, y = 0.8, label = "6", col="red", size=4) +
  annotate("text", x = 14.5, y = 0.8, label = "5", col="red", size=4) +
  annotate("text", x = 20.5, y = 0.8, label = "4", col="red", size=4) +
  annotate("text", x = 26.5, y = 0.8, label = "3", col="red", size=4) +  
  annotate("text", x = 32.5, y = 0.8, label = "2", col="red", size=4) +  
  annotate("text", x = 38, y = 0.8, label = "1", col="red", size=4)    

# Second, histogram for PR index
pr.score.plot <- ggplot() + 
  geom_histogram(data=fh.pr, aes(score), binwidth=1, fill="gray75", col="gray20") +
  geom_segment(data=count.pr.score.emp, aes(x=x , xend=x, y=1, yend=freq), 
               col="gray30", size = 12, lineend = "butt") +
  theme_bw() + 
  xlab("Political Rights Index") + ylab("Number of Combinations (log)") + 
  theme(axis.title=element_text(size=13,face="bold")) +
  theme(plot.title = element_text(size=16, face="bold", colour="black",
                                  hjust=.5)) +  
  scale_x_continuous(lim=c(0.5,7.5),
                     breaks=seq(1,7,1)) + 
  scale_y_log10(breaks=c(1,2,5,10,25,50,100,250,500,1000))


# Third, histogram for Civil liberties subscore
cl.plot <- ggplot() + 
  geom_histogram(data=fh.cl, aes(sum), binwidth=1, fill="gray75", col="gray20") +
  geom_segment(data=count.cl.emp, aes(x=x , xend=x, y=1, yend=freq), 
               col="gray30", size = 1.5, lineend = "butt") +
  geom_vline(xintercept=c(7.5, 16.5, 25.5, 34.5, 43.5, 52.5), col="red", alpha=0.8) + 
  theme_bw() + 
  xlab("Civil Liberties Subscore") + ylab("Number of Combinations (log)") + 
  ggtitle("Civil Liberties") + 
  theme(axis.title=element_text(size=13,face="bold")) +
  theme(plot.title = element_text(size=16, face="bold", colour="black",
                                  hjust=.5)) +  
  scale_x_continuous(breaks = seq(0,60,10),
                     labels = c("0", "10", "20", "30", "40", "50", "60"),
                     lim=c(-0.5,60.5))  +
  scale_y_log10(breaks=c(1, 2, 5, 10, 25, 50, 100, 250, 500, 1000, 2000)) +
  annotate("text", x = 3, y = 0.8, label = "7", col="red", size=4) +
  annotate("text", x = 12, y = 0.8, label = "6", col="red", size=4) +
  annotate("text", x = 21, y = 0.8, label = "5", col="red", size=4) +
  annotate("text", x = 30, y = 0.8, label = "4", col="red", size=4) +
  annotate("text", x = 39, y = 0.8, label = "3", col="red", size=4) +  
  annotate("text", x = 48, y = 0.8, label = "2", col="red", size=4) +  
  annotate("text", x = 57, y = 0.8, label = "1", col="red", size=4) 


# Fourth, histogram for CL index
cl.score.plot <- ggplot() + 
  geom_histogram(data=fh.cl, aes(score), binwidth=1, fill="gray75", col="gray20") +
  geom_segment(data=count.cl.score.emp, aes(x=x , xend=x, y=1, yend=freq), 
               col="gray30", size = 12, lineend = "butt") +
  theme_bw() + 
  xlab("Civil Liberties Index") + ylab("Number of Combinations (log)") + 
  theme(axis.title=element_text(size=13,face="bold")) +
  theme(plot.title = element_text(size=16, face="bold", colour="black",
                                  hjust=.5)) +  
  scale_x_continuous(lim=c(0.5,7.5),
                     breaks=seq(1,7,1)) + 
  scale_y_log10(breaks=c(1, 10, 50, 100, 250, 500, 1000, 2000, 5000, 10000, 20000))


# combining all four plots
pdf("combinations_fh_subscores.pdf", width=12, height=9)
multiplot(pr.plot, pr.score.plot, cl.plot, cl.score.plot, cols=2)
dev.off()



# Polity ------------------------------------------------------------------


# Let's first code the number of different combinations
polity <- seq(from=1, to=3, by=1)
polity <- c(polity, polity, polity, polity)
polity <- as.data.frame(polity)
colnames(polity) <- "xrcomp"

polity$xropen <- c(rep(1,3), rep(2,3), rep(3,3), rep(4,3)) 

polity <- rbind(polity, polity, polity, polity, polity, polity, polity)
polity$xconst <- c(rep(1,12), rep(2,12), rep(3,12), rep(4,12), rep(5,12), rep(6,12), rep(7,12))

polity <- rbind(polity, polity, polity, polity, polity)
polity$parreg <- c(rep(1,84), rep(2,84), rep(3,84), rep(4, 84), rep(5,84))

polity <- rbind(polity, polity, polity, polity, polity)
polity$parcomp <- c(rep(1,420), rep(2,420), rep(3,420), rep(4,420), rep(5,420))  
polity$parcomp <- ifelse(polity$parreg==1, 0, polity$parcomp)
  
  
# next,we can assign the democracy and autocracy scores, in turn
polity$dem <- ifelse(polity$xrcomp==3, 2, ifelse(polity$xrcomp==2,1,0))
polity$aut <- ifelse(polity$xrcomp==1,2,0)

polity$dem <- ifelse(polity$xropen==3 | polity$xropen==4, polity$dem+1, polity$dem)
polity$aut <- ifelse(polity$xropen==1 | polity$xropen==2, polity$aut+1, polity$aut)

polity$dem <- ifelse(polity$xconst==7, polity$dem+4, ifelse(polity$xconst==6, polity$dem+3,
                                                            ifelse(polity$xconst==5, polity$dem+2,
                                                                   ifelse(polity$xconst==4, polity$dem+1, polity$dem))))
polity$aut <- ifelse(polity$xconst==1, polity$aut+3, ifelse(polity$xconst==2, polity$aut+2,
                                                            ifelse(polity$xconst==1, polity$aut+1, polity$aut)))

polity$aut <- ifelse(polity$parreg==4, polity$aut+2, ifelse(polity$parreg==3, polity$aut+1, polity$aut))

polity$dem <- ifelse(polity$parcomp==5, polity$dem+3, ifelse(polity$parcomp==4, polity$dem+2,
                                                             ifelse(polity$parcomp==3, polity$dem+1, polity$dem)))
polity$aut <- ifelse(polity$parcomp==1, polity$aut+2, ifelse(polity$parcomp==2, polity$aut==1, polity$aut))


polity$diff <- polity$dem - polity$aut


# now, let's count the unique number of combinations that are in the data
polity.data <- read.csv("politydata.csv")
data.count <- polity.data[,c("democ", "autoc", "polity2", "xrcomp", "xropen", "xconst", "parreg", "parcomp")]
data.count <- subset(data.count, data.count$aut>=0)

# for democracy score:
count.dem.emp <- count(count(data.count[,-c(2,3)])[,1])

# for autocracy score:
count.aut.emp <- count(count(data.count[,-c(1,3)])[,1])

# for polity score:
count.polity.emp <- count(count(data.count[,-c(1,2)])[,1])



### plots
# First, histogram for dem score
polity1 <- ggplot() + 
  geom_histogram(data=polity, aes(dem), binwidth=1, fill="gray75", col="gray20") +
  geom_segment(data=count.dem.emp, aes(x=x , xend=x, y=1, yend=freq+1), 
               col="gray30", size = 10, lineend = "butt") +
  theme_bw() + 
  xlab("Democracy Score") + ylab("Number of Combinations (log)") + 
  ggtitle("Democracy Score") + 
  theme(axis.title=element_text(size=13,face="bold")) +
  theme(plot.title = element_text(size=16, face="bold", colour="black",
                                  hjust=.5)) +  
  scale_x_continuous(breaks = seq(0,10,2),
                     labels = c("0", "2", "4", "6", "8", "10"),
                     lim=c(-0.5,10.5)) +
  scale_y_log10(breaks=c(1,3,6,11,26,51,101,201,501),
                labels=c("0","2","5","10","25","50","100","200","500"),
                lim=c(0.9,501))


# Second, histogram for aut score
count.aut.emp[11,2] <- 1
polity2 <- ggplot() + 
  geom_histogram(data=polity, aes(aut), binwidth=1, fill="gray75", col="gray20") +
  geom_segment(data=count.aut.emp, aes(x=x , xend=x, y=1, yend=freq+1), 
               col="gray30", size = 10, lineend = "butt") +  
  theme_bw() + 
  xlab("Autocracy Score") + ylab("Number of Combinations (log)") + 
  ggtitle("Autocracy Score") + 
  theme(axis.title=element_text(size=13,face="bold")) +
  theme(plot.title = element_text(size=16, face="bold", colour="black",
                                  hjust=.5)) +  
  scale_x_continuous(breaks = seq(0,10,2),
                     labels = c("0", "2", "4", "6", "8", "10"),
                     lim=c(-0.5,10.5)) +
  scale_y_log10(breaks=c(1,3,6,11,26,51,101,201,501),
                labels=c("0","2","5","10","25","50","100","200","500"),
                lim=c(0.9,501))



# Third, combined polity2 score
count.polity.emp[1,2] <- 1
count.polity.emp[2,2] <- 3

polity3 <- ggplot(data=polity, aes(diff)) + 
  geom_histogram(data=polity, aes(diff), binwidth=1, fill="gray75", col="gray20") +
  geom_segment(data=count.polity.emp, aes(x=x , xend=x, y=1, yend=freq+1), 
               col="gray30", size = 10, lineend = "butt") +  
  geom_vline(xintercept=c(-5.5, 5.5), col="red", alpha=0.8) +   
  theme_bw() + 
  xlab("Democracy - Autocracy") + ylab("Number of Combinations (log)") + 
  ggtitle("polity2 Score") + 
  theme(axis.title=element_text(size=13,face="bold")) +
  theme(plot.title = element_text(size=16, face="bold", colour="black",
                                  hjust=.5)) +  
  scale_x_continuous(breaks = seq(-10,10,5),
                     labels = c("-10", "-5", "0", "5", "10"),
                     lim=c(-10,11))  +
  scale_y_log10(breaks=c(1,3,6,11,26,51,101,201,501),
                labels=c("0", "2","5","10","25","50","100","200","500"),
                lim=c(0.85,501)) +
  annotate("text", x = -8, y = .85, label = "Autocracy", col="red", size=4) +
  annotate("text", x = 0, y = .85, label = "Anocracy", col="red", size=4) + 
  annotate("text", x = 8, y = .85, label = "Democracy", col="red", size=4)  


# combining the plots
layout <- matrix(c(1, 2, 3, 3), nrow = 2, byrow = TRUE)
pdf("combinations_polity.pdf", width=12, height=9)
multiplot(polity1, polity2, polity3, layout = layout)
dev.off()



