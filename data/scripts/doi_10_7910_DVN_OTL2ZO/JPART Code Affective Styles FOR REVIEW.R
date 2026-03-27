###############################################################################
#
#         
#          Accountability and Affective Styles in Administrative Reporting 
#
#          Paper for JPART, R&R Code for Reviewers
#
###############################################################################

# clear memory
rm(list=ls()) 

# load packages to use
library(foreign)
library(quanteda)
library(readtext)
library(changepoint)
library(readr)
library(tidyr)
library(ggplot2)
library(dplyr)

#Create UNRWA Main Text Corpus
setwd("~/Desktop/R Data Folder/UNRWAReportsWOletters")
UNRWA.main <- readtext("*.txt")
UNRWA.main.corpus <- corpus(UNRWA.main)

#Create UNRWA Main Text Dataframe with punctuation, hyphens, and numbers removed
UNRWA.main.dfm <- dfm(UNRWA.main.corpus)
UNRWA.main.dfm <- dfm(tokens(UNRWA.main.corpus, remove_punct=TRUE, remove_hyphens=TRUE, remove_numbers=TRUE, remove_symbols=TRUE))


#Create AdCom Letter Corpus
setwd("~/Desktop/R Data Folder/UNRWAAdComLetter")
UNRWA.adcom <- readtext("*.txt")
UNRWA.adcom.corpus <- corpus(UNRWA.adcom)

#Create UNRWA Adcom Text Dataframe with punctuation, hyphens, and numbers removed
UNRWA.adcom.dfm <- dfm(UNRWA.adcom.corpus)
UNRWA.adcom.dfm <- dfm(tokens(UNRWA.adcom.corpus, remove_punct=TRUE, remove_hyphens=TRUE, remove_numbers=TRUE, remove_symbols=TRUE) )

#Word Count for Main Dataframe and AdCom Dataframe (numbers removed)
WordCount.main<-ntoken(UNRWA.main.dfm)
WordCount.adcom<-ntoken(UNRWA.adcom.dfm)

#Remove Words from LSD Dictionary (Positive)
LSD_subsetpos <- data_dictionary_LSD2015$positive
LSD_subsetpos <- LSD_subsetpos[LSD_subsetpos!="united*"]
LSD_subsetpos <- LSD_subsetpos[LSD_subsetpos!="unite*"]
LSD_subsetpos <- LSD_subsetpos[LSD_subsetpos!="resolut*"]
LSD_subsetpos <- LSD_subsetpos[LSD_subsetpos!="special"]
LSD_subsetpos <- LSD_subsetpos[LSD_subsetpos!="well"]

#Remove Words from LSD Dictionary (Negative)
LSD_subsetneg <- data_dictionary_LSD2015$negative
LSD_subsetneg <- LSD_subsetneg[LSD_subsetneg!="divisi*"]

##Recreate Dictionary Without Removed Words
data_dictionary_LSD2015_edit<-data_dictionary_LSD2015
data_dictionary_LSD2015_edit$positive<-LSD_subsetpos
data_dictionary_LSD2015_edit$negative<-LSD_subsetneg

#Apply the LSD dictionary to the Main Dataframe and the AdCom Dataframe
sentiment.main.LSD<-dfm_lookup(UNRWA.main.dfm, data_dictionary_LSD2015_edit)
sentiment.main.LSD.frame<-convert(sentiment.main.LSD, to = "data.frame")

sentiment.adcom.LSD<-dfm_lookup(UNRWA.adcom.dfm, data_dictionary_LSD2015_edit)
sentiment.adcom.LSD.frame<-convert(sentiment.adcom.LSD, to = "data.frame")

#Replace the first column with years of publication 
#Main starts with 1951 while AdCom starts with 1960

colnames(sentiment.main.LSD.frame)[1] <- "year"
colnames(sentiment.adcom.LSD.frame)[1] <- "year"

sentiment.main.LSD.frame$year<-c(1951:(1950+nrow(sentiment.main.LSD.frame)))
sentiment.adcom.LSD.frame$year<-c(1960:(1959+nrow(sentiment.adcom.LSD.frame)))

#Calculate Negative and Positive Sentiment Shares for Main and AdCom (normalized on Word Count)

sentiment.main.LSD.frame$wordcount<-WordCount.main
sentiment.adcom.LSD.frame$wordcount<-WordCount.adcom

sentiment.main.LSD.frame$negshare<-sentiment.main.LSD.frame$negative/sentiment.main.LSD.frame$wordcount
sentiment.adcom.LSD.frame$negshare<-sentiment.adcom.LSD.frame$negative/sentiment.adcom.LSD.frame$wordcount

sentiment.main.LSD.frame$posshare<-sentiment.main.LSD.frame$positive/sentiment.main.LSD.frame$wordcount
sentiment.adcom.LSD.frame$posshare<-sentiment.adcom.LSD.frame$positive/sentiment.adcom.LSD.frame$wordcount

sentiment.main.LSD.frame$totalshare<-sentiment.main.LSD.frame$negshare+sentiment.main.LSD.frame$posshare
sentiment.adcom.LSD.frame$totalshare<-sentiment.adcom.LSD.frame$negshare+sentiment.adcom.LSD.frame$posshare

##Analysis

#Correlation between negative and positive sentiment shares in the main corpus until 1986 (negative correlation)
sentiment.main.cor1986<-cor.test(sentiment.main.LSD.frame$negshare[1:36],sentiment.main.LSD.frame$posshare[1:36], method=c("pearson"))

#Correlation between negative and positive sentiment shares in the main corpus from 1994 (positive correlation)
sentiment.main.cor1994<-cor.test(sentiment.main.LSD.frame$negshare[44:70],sentiment.main.LSD.frame$posshare[44:70], method=c("pearson"))

#Correlation between negative and positive sentiment shares between Main and AdCom (starting 1960)
sentiment.main.adcom.negative<-cor.test(sentiment.main.LSD.frame$negshare[10:70],sentiment.adcom.LSD.frame$negshare, method=c("pearson"))
sentiment.main.adcom.positive<-cor.test(sentiment.main.LSD.frame$posshare[10:70],sentiment.adcom.LSD.frame$posshare, method=c("pearson"))

#Calculate difference in means between negative and positive sentiment shares in Main and AdCom starting 1960
sentiment.main.adcom.negative.ttest<-t.test(sentiment.main.LSD.frame$negshare[10:70], sentiment.adcom.LSD.frame$negshare, paired = TRUE, alternative = "l")
sentiment.main.adcom.positive.ttest<-t.test(sentiment.main.LSD.frame$posshare[10:70], sentiment.adcom.LSD.frame$posshare, paired = TRUE, alternative = "l")

#Calculate change-points, used in Figure 4, Figure 5, Figure 6 (*100 for percentages)
sentiment.main.total.changepoint <- cpt.meanvar(sentiment.main.LSD.frame$totalshare*100,penalty="Manual",pen.value="3*log(n)",method="BinSeg",Q=30,class=TRUE,test.stat = "Normal") 
sentiment.main.pos.changepoint <- cpt.meanvar(sentiment.main.LSD.frame$posshare*100,penalty="Manual",pen.value="3*log(n)",method="BinSeg",Q=30,class=TRUE,test.stat = "Normal") 
sentiment.main.neg.changepoint <- cpt.meanvar(sentiment.main.LSD.frame$negshare*100,penalty="Manual",pen.value="3*log(n)",method="BinSeg",Q=30,class=TRUE,test.stat = "Normal") 

#Calculate change-points, used in Annex 6
sentiment.main.total.changepoint6 <- cpt.meanvar(sentiment.main.LSD.frame$totalshare*100,penalty="Manual",pen.value="6*log(n)",method="BinSeg",Q=30,class=TRUE,test.stat = "Normal") 
sentiment.main.pos.changepoint6 <- cpt.meanvar(sentiment.main.LSD.frame$posshare*100,penalty="Manual",pen.value="6*log(n)",method="BinSeg",Q=30,class=TRUE,test.stat = "Normal") 
sentiment.main.neg.changepoint6 <- cpt.meanvar(sentiment.main.LSD.frame$negshare*100,penalty="Manual",pen.value="6*log(n)",method="BinSeg",Q=30,class=TRUE,test.stat = "Normal") 

sentiment.main.total.changepoint12 <- cpt.meanvar(sentiment.main.LSD.frame$totalshare*100,penalty="Manual",pen.value="12*log(n)",method="BinSeg",Q=30,class=TRUE,test.stat = "Normal") 
sentiment.main.pos.changepoint12 <- cpt.meanvar(sentiment.main.LSD.frame$posshare*100,penalty="Manual",pen.value="12*log(n)",method="BinSeg",Q=30,class=TRUE,test.stat = "Normal") 
sentiment.main.neg.changepoint12 <- cpt.meanvar(sentiment.main.LSD.frame$negshare*100,penalty="Manual",pen.value="12*log(n)",method="BinSeg",Q=30,class=TRUE,test.stat = "Normal") 


#Plotting

#Simple plot changepoint (does not translate to ggplot2, therefore refined manual plotting below)
#Replace variable in plot() and ylab as needed for Figures 4-6, Annex 6

plot(sentiment.main.total.changepoint,
     ylim = c(0, 10),
     type = "l",
     cpt.col = "black",
     ylab= "Total sentiment share",
     cpt.width = 2,
     xaxt='n'
)
axis(1, at=1:70, labels=1951:2020, cex.axis=1, las=3)


###Refined Plotting###

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Figure 2 - Plot Sentiment Shares of main text
# sentiment.main.LSD.frame$negshare, $posshare, $totalshare, 1951-2020

plotdata.main <- gather(sentiment.main.LSD.frame, "Type", "Share", c(7:9))

## sentiment in main text (pos, neg, total)
p1 <- ggplot(plotdata.main, aes(x=(year), y=Share*100)) +
  geom_line(aes(colour=Type), size=1.3) +
  scale_colour_grey(start=0.8, end=0.2,labels=c("Negative terms", "Positive terms", "Both")) +
  #scale_x_continuous(breaks = round(seq(min(plotdata.main$year), max(plotdata.main$year), by = 1),1)) +
  scale_x_continuous(breaks = c(seq(min(plotdata.main$year), max(plotdata.main$year), by = 1)),expand = c(0, 0)) +
  #    scale_x_continuous(name="Year", breaks = seq(1951, 2020, 1)) + 
  ylab("Percentage share") +
  xlab("Year") +
  ggtitle("Sentiment in main text of UNRWA annual reports") +
  ylim(0,10) +
  theme(
    plot.title = element_text(hjust=0.5, size=14, face="bold"),
    #legend.position="none",
    axis.text.x= element_text(angle=90, vjust=0.5, size=8),
    axis.text.y  = element_text(size=8),
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="white")) 

p1



# Refined Plots Change Point Analysis. Manual transfer from simple plots above.

#Figure 3: Total Affect Main & Plot

pc <- data.frame(sentiment.main.LSD.frame$year, sentiment.main.LSD.frame$totalshare*100)
pc$no <- seq(1:nrow(pc))
keep <- as.numeric(cpts(sentiment.main.total.changepoint))
pc$changepoint <- ifelse(pc$no %in% keep, 1, 0)
pc$mean <- ifelse(pc$no<28, mean(pc$sentiment.main.LSD.frame.totalshare...100[pc$no<28]), NA)
pc$mean <- ifelse(pc$no>27 & pc$no<31,
                  mean(pc$sentiment.main.LSD.frame.totalshare...100[pc$no>27 & pc$no<31]),
                  pc$mean)
pc$mean <- ifelse(pc$no>30 & pc$no<36,
                  mean(pc$sentiment.main.LSD.frame.totalshare...100[pc$no>30 & pc$no<36]),
                  pc$mean)
pc$mean <- ifelse(pc$no>35 & pc$no<44,
                  mean(pc$sentiment.main.LSD.frame.totalshare...100[pc$no>35 & pc$no<44]),
                  pc$mean)
pc$mean <- ifelse(pc$no>43 & pc$no<57,
                  mean(pc$sentiment.main.LSD.frame.totalshare...100[pc$no>43 & pc$no<57]),
                  pc$mean)
pc$mean <- ifelse(pc$no>56 & pc$no<64,
                  mean(pc$sentiment.main.LSD.frame.totalshare...100[pc$no>56 & pc$no<64]),
                  pc$mean)
pc$mean <- ifelse(pc$no>63 & pc$no<71,
                  mean(pc$sentiment.main.LSD.frame.totalshare...100[pc$no>63 & pc$no<71]),
                  pc$mean)
pc$changepointcount <- 1
pc$changepointcount <- ifelse(pc$no %in% 28:30, 2, pc$changepointcount)
pc$changepointcount <- ifelse(pc$no %in% 31:35, 3, pc$changepointcount)
pc$changepointcount <- ifelse(pc$no %in% 36:43, 4, pc$changepointcount)
pc$changepointcount <- ifelse(pc$no %in% 44:56, 5, pc$changepointcount)
pc$changepointcount <- ifelse(pc$no %in% 57:63, 6, pc$changepointcount)
pc$changepointcount <- ifelse(pc$no %in% 64:70, 7, pc$changepointcount)

pc <- pc %>% spread(changepointcount, mean)


p6 <- ggplot(pc) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=sentiment.main.LSD.frame.totalshare...100), size=0.7) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`1`), size=1.3) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`2`), size=1.3) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`3`), size=1.3) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`4`), size=1.3) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`5`), size=1.3) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`6`), size=1.3) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`7`), size=1.3) +
  scale_colour_manual(values=cbPalette[3:4]) +
  scale_x_continuous(breaks = c(seq(1951, max(plotdata.main$year), by = 1)),expand = c(0, 0)) +
  ylab("Total Sentiment Share") +
  xlab("Year") +
  #  ggtitle("Share of positive sentiment in AdCom letter and main report") +
  ylim(0,10) +
  #scale_y_continuous(breaks = c(seq(1:12))) +
  theme(
    #  plot.title = element_text(hjust=0.5, size=14, face="bold"),
    # legend.position="none",
    axis.text.x= element_text(angle=90, vjust=0.5, size=8),
    axis.text.y  = element_text(size=8),
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="white" ))

p6


# Figure 4: Change Point Analysis Negative Affect Main & Plot

pc2 <- data.frame(sentiment.main.LSD.frame$year, sentiment.main.LSD.frame$negshare*100)
pc2$no <- seq(1:nrow(pc2))
keep2 <- as.numeric(cpts(sentiment.main.neg.changepoint))
pc2$changepoint <- ifelse(pc2$no %in% keep2, 1, 0)
pc2$mean <- ifelse(pc2$no<17, mean(pc2$sentiment.main.LSD.frame.negshare...100[pc2$no<17]), NA)
pc2$mean <- ifelse(pc2$no>16 & pc2$no<32,
                   mean(pc2$sentiment.main.LSD.frame.negshare...100[pc2$no>16 & pc2$no<32]),
                   pc2$mean)
pc2$mean <- ifelse(pc2$no>31 & pc2$no<38,
                   mean(pc2$sentiment.main.LSD.frame.negshare...100[pc2$no>31 & pc2$no<38]),
                   pc2$mean)
pc2$mean <- ifelse(pc2$no>37 & pc2$no<44,
                   mean(pc2$sentiment.main.LSD.frame.negshare...100[pc2$no>37 & pc2$no<44]),
                   pc2$mean)
pc2$mean <- ifelse(pc2$no>43 & pc2$no<52,
                   mean(pc2$sentiment.main.LSD.frame.negshare...100[pc2$no>43 & pc2$no<52]),
                   pc2$mean)
pc2$mean <- ifelse(pc2$no>51 & pc2$no<57,
                   mean(pc2$sentiment.main.LSD.frame.negshare...100[pc2$no>51 & pc2$no<57]),
                   pc2$mean)
pc2$mean <- ifelse(pc2$no>56 & pc2$no<59,
                   mean(pc2$sentiment.main.LSD.frame.negshare...100[pc2$no>56 & pc2$no<59]),
                   pc2$mean)
pc2$mean <- ifelse(pc2$no>58 & pc2$no<63,
                   mean(pc2$sentiment.main.LSD.frame.negshare...100[pc2$no>58 & pc2$no<63]),
                   pc2$mean)
pc2$mean <- ifelse(pc2$no>62 & pc2$no<71,
                   mean(pc2$sentiment.main.LSD.frame.negshare...100[pc2$no>62 & pc2$no<71]),
                   pc2$mean)
pc2$changepointcount <- 1
pc2$changepointcount <- ifelse(pc2$no %in% 17:31, 2, pc2$changepointcount)
pc2$changepointcount <- ifelse(pc2$no %in% 32:37, 3, pc2$changepointcount)
pc2$changepointcount <- ifelse(pc2$no %in% 38:43, 4, pc2$changepointcount)
pc2$changepointcount <- ifelse(pc2$no %in% 44:51, 5, pc2$changepointcount)
pc2$changepointcount <- ifelse(pc2$no %in% 52:56, 6, pc2$changepointcount)
pc2$changepointcount <- ifelse(pc2$no %in% 57:58, 7, pc2$changepointcount)
pc2$changepointcount <- ifelse(pc2$no %in% 59:62, 8, pc2$changepointcount)
pc2$changepointcount <- ifelse(pc2$no %in% 63:70, 9, pc2$changepointcount)

pc2 <- pc2 %>% spread(changepointcount, mean)


p7 <- ggplot(pc2) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=sentiment.main.LSD.frame.negshare...100), size=0.7) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`1`), size=1.3) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`2`), size=1.3) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`3`), size=1.3) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`4`), size=1.3) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`5`), size=1.3) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`6`), size=1.3) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`7`), size=1.3) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`8`), size=1.3) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`9`), size=1.3) +
  scale_colour_manual(values=cbPalette[3:4]) +
  scale_x_continuous(breaks = c(seq(1951, max(plotdata.main$year), by = 1)),expand = c(0, 0)) +
  ylab("Negative Sentiment Share") +
  xlab("Year") +
  #  ggtitle("Share of positive sentiment in AdCom letter and main report") +
  ylim(0,10) +
  #scale_y_continuous(breaks = c(seq(1:12))) +
  theme(
    #  plot.title = element_text(hjust=0.5, size=14, face="bold"),
    # legend.position="none",
    axis.text.x= element_text(angle=90, vjust=0.5, size=8),
    axis.text.y  = element_text(size=8),
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="white" ))

p7


# Figure 5: Change Point Analysis Positive Affect Main & Plot

pc4 <- data.frame(sentiment.main.LSD.frame$year, sentiment.main.LSD.frame$posshare*100)
pc4$no <- seq(1:nrow(pc4))
keep3 <- as.numeric(cpts(sentiment.main.pos.changepoint))
pc4$changepoint <- ifelse(pc4$no %in% keep3, 1, 0)
pc4$mean <- ifelse(pc4$no<18, mean(pc4$sentiment.main.LSD.frame.posshare...100[pc4$no<18]), NA)
pc4$mean <- ifelse(pc4$no>17 & pc4$no<36,
                   mean(pc4$sentiment.main.LSD.frame.posshare...100[pc4$no>17 & pc4$no<36]),
                   pc4$mean)
pc4$mean <- ifelse(pc4$no>35 & pc4$no<40,
                   mean(pc4$sentiment.main.LSD.frame.posshare...100[pc4$no>35 & pc4$no<40]),
                   pc4$mean)
pc4$mean <- ifelse(pc4$no>39 & pc4$no<44,
                   mean(pc4$sentiment.main.LSD.frame.posshare...100[pc4$no>39 & pc4$no<44]),
                   pc4$mean)
pc4$mean <- ifelse(pc4$no>43 & pc4$no<60,
                   mean(pc4$sentiment.main.LSD.frame.posshare...100[pc4$no>43 & pc4$no<60]),
                   pc4$mean)
pc4$mean <- ifelse(pc4$no>59 & pc4$no<71,
                   mean(pc4$sentiment.main.LSD.frame.posshare...100[pc4$no>59 & pc4$no<71]),
                   pc4$mean)
pc4$changepointcount <- 1
pc4$changepointcount <- ifelse(pc4$no %in% 18:35, 2, pc4$changepointcount)
pc4$changepointcount <- ifelse(pc4$no %in% 36:39, 3, pc4$changepointcount)
pc4$changepointcount <- ifelse(pc4$no %in% 40:43, 4, pc4$changepointcount)
pc4$changepointcount <- ifelse(pc4$no %in% 44:59, 5, pc4$changepointcount)
pc4$changepointcount <- ifelse(pc4$no %in% 60:70, 6, pc4$changepointcount)

pc4 <- pc4 %>% spread(changepointcount, mean)


p8 <- ggplot(pc4) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=sentiment.main.LSD.frame.posshare...100), size=0.7) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`1`), size=1.3) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`2`), size=1.3) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`3`), size=1.3) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`4`), size=1.3) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`5`), size=1.3) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`6`), size=1.3) +
  
  scale_colour_manual(values=cbPalette[3:4]) +
  scale_x_continuous(breaks = c(seq(1951, max(plotdata.main$year), by = 1)),expand = c(0, 0)) +
  ylab("Positive Sentiment Share") +
  xlab("Year") +
  #  ggtitle("Share of positive sentiment in AdCom letter and main report") +
  ylim(0,10) +
  #scale_y_continuous(breaks = c(seq(1:12))) +
  theme(
    #  plot.title = element_text(hjust=0.5, size=14, face="bold"),
    # legend.position="none",
    axis.text.x= element_text(angle=90, vjust=0.5, size=8),
    axis.text.y  = element_text(size=8),
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="white" ))

p8



# Figures 8 and 9: Compare negative and positive sentiment shares in Main and Adcom

# Prepare Plot Sentiment Shares of AdCom text
# sentiment.adcom.LSD.frame$negshare, $posshare, #totalshare, 1960-2020

plotdata.adcom <- gather(sentiment.adcom.LSD.frame, "Type", "Share", c(7:9))
plotdata.adcomyears <- data.frame("year"=rep(1951:1959, each=3))
plotdata.adcomtype <- data.frame("Type" = rep(c("negshare", "posshare", "totalshare"), times=9))
plotdata.adcomadds <- data.frame(cbind(plotdata.adcomyears, plotdata.adcomtype))
plotdata.adcom <- full_join(plotdata.adcom, plotdata.adcomadds, by=c("year", "Type"))


## Figure (not used in R&R) sentiment in adcom text (pos, neg, total)
#p5 <- ggplot(plotdata.adcom, aes(x=(year), y=Share*100)) +
#  geom_line(aes(colour=Type), size=1.3) +
#  scale_colour_grey(start=0.8, end=0.2,labels=c("Negative terms", "Positive terms", "Both")) +
#scale_linetype_manual(values=c("dotdash", "dashed", "solid"))+
#  scale_colour_manual(values=cbPalette, name= "Share of:", labels=c("Negative terms", "Positive terms", "Both")) +
#scale_x_continuous(breaks = round(seq(min(plotdata.main$year), max(plotdata.main$year), by = 1),1)) +
#  scale_x_continuous(breaks = c(seq(1951, max(plotdata.main$year), by = 1)),expand = c(0, 0)) +
#    scale_x_continuous(name="Year", breaks = seq(1951, 2020, 1)) + 
#  ylab("Percentage share") +
#  xlab("Year") +
#  ggtitle("Sentiment in text of AdCom letters accompanying UNRWA annual reports") +
#  ylim(0,16) +
#  theme(
#    plot.title = element_text(hjust=0.5, size=14, face="bold"),
#    #legend.position="none",
#    axis.text.x= element_text(angle=90, vjust=0.5, size=8),
#    axis.text.y  = element_text(size=8),
#    panel.grid.major.x = element_blank() ,
#    panel.grid.major.y = element_line( size=.1, color="white")) 

#p5

#Create joint variable for Main and Adcom for plotting
sentiment.both <- full_join(sentiment.main.LSD.frame, sentiment.adcom.LSD.frame,
                            by=c("year"))
sentiment.both <- sentiment.both %>% 
  rename(
    negshare_main = negshare.x,
    posshare_main = posshare.x,
    totalshare_main = totalshare.x,
    negshare_adcom = negshare.y,
    posshare_adcom = posshare.y,
    totalshare_adcom = totalshare.y
    
  )

#In the plots below, Select c(7,15) for negative; c(8,16) for positive, c(9,17) for total

# Figure 8: Plot Negative Sentiment Shares of main and adcom (note different vector length for $year)
plotdata.negative <- gather(sentiment.both, "Type", "ShareNegative", c(7,15))

p2 <- ggplot(plotdata.negative, aes(x=(year), y=ShareNegative*100)) +
  geom_line(aes(colour=Type), size=1.3) +
  #scale_colour_manual(values=cbPalette[3:4], name= "Negative share in:", labels=c("AdCom letter", "Main report")) +
  scale_colour_grey(start=0.8, end=0.5, name= "Negative share in:", labels=c("AdCom letter", "Main report")) +
  scale_x_continuous(breaks = c(seq(1951, max(plotdata.main$year), by = 1)),expand = c(0, 0)) +
  ylab("Percentage share") +
  xlab("Year") +
  ggtitle("Share of negative sentiment in AdCom letter and main report") +
  ylim(0,10) +
  theme(
    plot.title = element_text(hjust=0.5, size=14, face="bold"),
    # legend.position="none",
    axis.text.x= element_text(angle=90, vjust=0.5, size=8),
    axis.text.y  = element_text(size=8),
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="white" ))

p2


# Figure 9: Plot Positive Sentiment Shares of main and adcom (note different vector length for $year)
plotdata.positive <- gather(sentiment.both, "Type", "SharePositive", c(8,16))

p3 <- ggplot(plotdata.positive, aes(x=(year), y=SharePositive*100)) +
  geom_line(aes(colour=Type), size=1.3) +
  #scale_colour_manual(values=cbPalette[3:4], name= "Positive share in:", labels=c("AdCom letter", "Main report")) +
  scale_colour_grey(start=0.8, end=0.5, name= "Positive share in:", labels=c("AdCom letter", "Main report")) +
  scale_x_continuous(breaks = c(seq(1951, max(plotdata.main$year), by = 1)),expand = c(0, 0)) +
  ylab("Percentage share") +
  xlab("Year") +
  ggtitle("Share of positive sentiment in AdCom letter and main report") +
  ylim(0,10) +
  theme(
    plot.title = element_text(hjust=0.5, size=14, face="bold"),
    # legend.position="none",
    axis.text.x= element_text(angle=90, vjust=0.5, size=8),
    axis.text.y  = element_text(size=8),
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="white" ))

p3

# Not used: Plot Total Sentiment Shares of main and adcom (note different vector length for $year)
#plotdata.total <- gather(sentiment.both, "Type", "ShareTotal", c(9,17))

#p4 <- ggplot(plotdata.positive, aes(x=(year), y=ShareTotal*100)) +
#  geom_line(aes(colour=Type), size=1.3) +
#scale_colour_manual(values=cbPalette[3:4], name= "Total share in:", labels=c("AdCom letter", "Main report")) +
#  scale_colour_grey(start=0.8, end=0.5, name= "Total share in:", labels=c("AdCom letter", "Main report")) +
#  scale_x_continuous(breaks = c(seq(1951, max(plotdata.main$year), by = 1)),expand = c(0, 0)) +
#  ylab("Percentage share") +
#  xlab("Year") +
#  ggtitle("Share of total sentiment in AdCom letter and main report") +
#  ylim(0,12) +
#  theme(
#    plot.title = element_text(hjust=0.5, size=14, face="bold"),
# legend.position="none",
#    axis.text.x= element_text(angle=90, vjust=0.5, size=8),
#    axis.text.y  = element_text(size=8),
#    panel.grid.major.x = element_blank() ,
#    panel.grid.major.y = element_line( size=.1, color="white" ))

#p4


#Annex 1 - Plot UNRWA annual report length (without numbers)

#Figure 10: UNRWA main word count
plotdata.main <- gather(sentiment.main.LSD.frame, "Type", "wordcount", c(6))

p10 <- ggplot(plotdata.main, aes(x=(year), y=wordcount)) +
  geom_line(aes(colour=Type), size=1.3) +
  scale_colour_grey(start=0.8, end=0.2) +
  #scale_x_continuous(breaks = round(seq(min(plotdata.main$year), max(plotdata.main$year), by = 1),1)) +
  scale_x_continuous(breaks = c(seq(min(plotdata.main$year), max(plotdata.main$year), by = 1)),expand = c(0, 0)) +
  #    scale_x_continuous(name="Year", breaks = seq(1951, 2020, 1)) + 
  ylab("N° of words (without numbers)") +
  xlab("Year") +
  ggtitle("Length of UNRWA annual reports") +
  ylim(0,50000) +
  theme(
    plot.title = element_text(hjust=0.5, size=14, face="bold"),
    legend.position="none",
    axis.text.x= element_text(angle=90, vjust=0.5, size=8),
    axis.text.y  = element_text(size=8),
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="white")) 

p10

#Figure 11: AdCom Word Count
plotdata.main <- gather(sentiment.adcom.LSD.frame, "Type", "wordcount", c(6))

p11 <- ggplot(plotdata.main, aes(x=(year), y=wordcount)) +
  geom_line(aes(colour=Type), size=1.3) +
  scale_colour_grey(start=0.8, end=0.2) +
  #scale_x_continuous(breaks = round(seq(min(plotdata.main$year), max(plotdata.main$year), by = 1),1)) +
  scale_x_continuous(breaks = c(seq(min(plotdata.main$year), max(plotdata.main$year), by = 1)),expand = c(0, 0)) +
  #    scale_x_continuous(name="Year", breaks = seq(1951, 2020, 1)) + 
  ylab("N° of words (without numbers)") +
  xlab("Year") +
  ggtitle("Length of AdCom letters") +
  ylim(0,2500) +
  theme(
    plot.title = element_text(hjust=0.5, size=14, face="bold"),
    legend.position="none",
    axis.text.x= element_text(angle=90, vjust=0.5, size=8),
    axis.text.y  = element_text(size=8),
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="white")) 

p11



#Annex 2 - Plot Flesch readability

UNRWA_Readability <- textstat_readability(UNRWA.main.corpus, "Flesch")
colnames(UNRWA_Readability)[1] <- "year"
UNRWA_Readability$year<-c(1951:(1950+nrow(UNRWA_Readability)))

#Figure 12: Readability of UNRWA annual reports

p12 <- ggplot(UNRWA_Readability, aes(x=(year), y=Flesch)) +
  geom_line(aes(colour='gray'), size=1.3) +
  scale_colour_grey(start=0.8, end=0.2) +
  #scale_x_continuous(breaks = round(seq(min(plotdata.main$year), max(plotdata.main$year), by = 1),1)) +
  scale_x_continuous(breaks = c(seq(min(UNRWA_Readability$year), max(UNRWA_Readability$year), by = 1)),expand = c(0, 0)) +
  #    scale_x_continuous(name="Year", breaks = seq(1951, 2020, 1)) + 
  ylab("Flesch Reading Ease (higher=easier)") +
  xlab("Year") +
  ggtitle("Readability of UNRWA annual reports") +
  ylim(0,50) +
  theme(
    plot.title = element_text(hjust=0.5, size=14, face="bold"),
    legend.position="none",
    axis.text.x= element_text(angle=90, vjust=0.5, size=8),
    axis.text.y  = element_text(size=8),
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="white")) 

p12



#Annex 4 - Create Tables (neg,pos) with LSD Counts of Individual Words
dictionary.LSD<-dictionary(data_dictionary_LSD2015)

dict_dfm_results.negUNRWA <- dfm(UNRWA.main.dfm, select = dictionary.LSD$negative, verbose = FALSE)
dict_dfm_results.posUNRWA <- dfm(UNRWA.main.dfm, select = dictionary.LSD$positive, verbose = FALSE)

dict_dfm_results.negUNRWA <- convert(dict_dfm_results.negUNRWA, to = "data.frame")
dict_dfm_results.posUNRWA <- convert(dict_dfm_results.posUNRWA, to = "data.frame")

dict_dfm_results.negUNRWA$document <- paste0((dict_dfm_results.negUNRWA$document),"main")
dict_dfm_results.negUNRWAtr <- t((dict_dfm_results.negUNRWA))
colnames(dict_dfm_results.negUNRWAtr) =dict_dfm_results.negUNRWAtr[1, ]
dict_dfm_results.negUNRWAtr = dict_dfm_results.negUNRWAtr[-1, ]
words <- rownames(dict_dfm_results.negUNRWAtr) 
dict_dfm_results.negUNRWAtr <- cbind(words, dict_dfm_results.negUNRWAtr)
rownames(dict_dfm_results.negUNRWAtr) <- c()

dict_dfm_results.posUNRWA$document <- paste0((dict_dfm_results.posUNRWA$document),"main")
dict_dfm_results.posUNRWAtr <- t((dict_dfm_results.posUNRWA))
colnames(dict_dfm_results.posUNRWAtr) =dict_dfm_results.posUNRWAtr[1, ]
dict_dfm_results.posUNRWAtr = dict_dfm_results.posUNRWAtr[-1, ]
words <- rownames(dict_dfm_results.posUNRWAtr) 
dict_dfm_results.posUNRWAtr <- cbind(words, dict_dfm_results.posUNRWAtr)
rownames(dict_dfm_results.posUNRWAtr) <- c()



#Annex 6 - Refined Plots for Change Points with alternative penalty values


#Figure A1. Overall sentiment, 6*log(n)
pc5 <- data.frame(sentiment.main.LSD.frame$year, sentiment.main.LSD.frame$totalshare*100)
pc5$no <- seq(1:nrow(pc5))
keep4 <- as.numeric(cpts(sentiment.main.total.changepoint6))
pc5$changepoint <- ifelse(pc5$no %in% keep4, 1, 0)
pc5$mean <- ifelse(pc5$no<36, mean(pc5$sentiment.main.LSD.frame.totalshare...100[pc5$no<36]), NA)
pc5$mean <- ifelse(pc5$no>35 & pc5$no<57,
                   mean(pc5$sentiment.main.LSD.frame.totalshare...100[pc5$no>35 & pc5$no<57]),
                   pc5$mean)
pc5$mean <- ifelse(pc5$no>56 & pc5$no<71,
                   mean(pc5$sentiment.main.LSD.frame.totalshare...100[pc5$no>56 & pc5$no<71]),
                   pc5$mean)

pc5$changepointcount <- 1
pc5$changepointcount <- ifelse(pc5$no %in% 36:56, 2, pc5$changepointcount)
pc5$changepointcount <- ifelse(pc5$no %in% 57:70, 3, pc5$changepointcount)

pc5 <- pc5 %>% spread(changepointcount, mean)


p20 <- ggplot(pc5) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=sentiment.main.LSD.frame.totalshare...100), size=0.7) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`1`), size=1.3) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`2`), size=1.3) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`3`), size=1.3) +
  
  scale_colour_manual(values=cbPalette[3:4]) +
  scale_x_continuous(breaks = c(seq(1951, max(plotdata.main$year), by = 1)),expand = c(0, 0)) +
  ylab("Total Sentiment Share") +
  xlab("Year") +
  ggtitle("Overall sentiment in UNRWA annual reports (changepoints, penality 6*log(n)") +
  ylim(0,10) +
  #scale_y_continuous(breaks = c(seq(1:12))) +
  theme(
    #  plot.title = element_text(hjust=0.5, size=14, face="bold"),
    # legend.position="none",
    axis.text.x= element_text(angle=90, vjust=0.5, size=8),
    axis.text.y  = element_text(size=8),
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="white" ))

p20

#Figure A2. Overall sentiment, 12*log(n)
pc5 <- data.frame(sentiment.main.LSD.frame$year, sentiment.main.LSD.frame$totalshare*100)
pc5$no <- seq(1:nrow(pc5))
keep5 <- as.numeric(cpts(sentiment.main.total.changepoint12))
pc5$changepoint <- ifelse(pc5$no %in% keep5, 1, 0)
pc5$mean <- ifelse(pc5$no<36, mean(pc5$sentiment.main.LSD.frame.totalshare...100[pc5$no<36]), NA)
pc5$mean <- ifelse(pc5$no>35 & pc5$no<71,
                   mean(pc5$sentiment.main.LSD.frame.totalshare...100[pc5$no>35 & pc5$no<71]),
                   pc5$mean)

pc5$changepointcount <- 1
pc5$changepointcount <- ifelse(pc5$no %in% 36:70, 2, pc5$changepointcount)

pc5 <- pc5 %>% spread(changepointcount, mean)


p21 <- ggplot(pc5) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=sentiment.main.LSD.frame.totalshare...100), size=0.7) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`1`), size=1.3) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`2`), size=1.3) +
  
  scale_colour_manual(values=cbPalette[3:4]) +
  scale_x_continuous(breaks = c(seq(1951, max(plotdata.main$year), by = 1)),expand = c(0, 0)) +
  ylab("Total Sentiment Share") +
  xlab("Year") +
  ggtitle("Overall sentiment in UNRWA annual reports (changepoints, penality 12*log(n)") +
  ylim(0,10) +
  #scale_y_continuous(breaks = c(seq(1:12))) +
  theme(
    #  plot.title = element_text(hjust=0.5, size=14, face="bold"),
    # legend.position="none",
    axis.text.x= element_text(angle=90, vjust=0.5, size=8),
    axis.text.y  = element_text(size=8),
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="white" ))

p21


#Figure A3. Positive sentiment, 6*log(n)
pc5 <- data.frame(sentiment.main.LSD.frame$year, sentiment.main.LSD.frame$posshare*100)
pc5$no <- seq(1:nrow(pc5))
keep6 <- as.numeric(cpts(sentiment.main.pos.changepoint6))
pc5$changepoint <- ifelse(pc5$no %in% keep6, 1, 0)
pc5$mean <- ifelse(pc5$no<44, mean(pc5$sentiment.main.LSD.frame.posshare...100[pc5$no<44]), NA)
pc5$mean <- ifelse(pc5$no>43 & pc5$no<71,
                   mean(pc5$sentiment.main.LSD.frame.posshare...100[pc5$no>43 & pc5$no<71]),
                   pc5$mean)

pc5$changepointcount <- 1
pc5$changepointcount <- ifelse(pc5$no %in% 44:70, 2, pc5$changepointcount)

pc5 <- pc5 %>% spread(changepointcount, mean)


p22 <- ggplot(pc5) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=sentiment.main.LSD.frame.posshare...100), size=0.7) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`1`), size=1.3) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`2`), size=1.3) +
  
  scale_colour_manual(values=cbPalette[3:4]) +
  scale_x_continuous(breaks = c(seq(1951, max(plotdata.main$year), by = 1)),expand = c(0, 0)) +
  ylab("Positive Sentiment Share") +
  xlab("Year") +
  ggtitle("Positive sentiment in UNRWA annual reports (changepoints, penality 6*log(n)") +
  ylim(0,10) +
  #scale_y_continuous(breaks = c(seq(1:12))) +
  theme(
    #  plot.title = element_text(hjust=0.5, size=14, face="bold"),
    # legend.position="none",
    axis.text.x= element_text(angle=90, vjust=0.5, size=8),
    axis.text.y  = element_text(size=8),
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="white" ))

p22

#Figure A4. Positive sentiment, 12*log(n)
pc5 <- data.frame(sentiment.main.LSD.frame$year, sentiment.main.LSD.frame$posshare*100)
pc5$no <- seq(1:nrow(pc5))
keep7 <- as.numeric(cpts(sentiment.main.pos.changepoint12))
pc5$changepoint <- ifelse(pc5$no %in% keep7, 1, 0)
pc5$mean <- ifelse(pc5$no<71, mean(pc5$sentiment.main.LSD.frame.posshare...100[pc5$no<71]), NA)

pc5$changepointcount <- 1

pc5 <- pc5 %>% spread(changepointcount, mean)


p23 <- ggplot(pc5) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=sentiment.main.LSD.frame.posshare...100), size=0.7) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`1`), size=1.3) +
  
  scale_colour_manual(values=cbPalette[3:4]) +
  scale_x_continuous(breaks = c(seq(1951, max(plotdata.main$year), by = 1)),expand = c(0, 0)) +
  ylab("Positive Sentiment Share") +
  xlab("Year") +
  ggtitle("Positive sentiment in UNRWA annual reports (changepoints, penality 12*log(n)") +
  ylim(0,10) +
  #scale_y_continuous(breaks = c(seq(1:12))) +
  theme(
    #  plot.title = element_text(hjust=0.5, size=14, face="bold"),
    # legend.position="none",
    axis.text.x= element_text(angle=90, vjust=0.5, size=8),
    axis.text.y  = element_text(size=8),
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="white" ))

p23



#Figure A5. Negative sentiment, 6*log(n)
pc5 <- data.frame(sentiment.main.LSD.frame$year, sentiment.main.LSD.frame$negshare*100)
pc5$no <- seq(1:nrow(pc5))
keep8 <- as.numeric(cpts(sentiment.main.neg.changepoint6))
pc5$changepoint <- ifelse(pc5$no %in% keep8, 1, 0)
pc5$mean <- ifelse(pc5$no<17, mean(pc5$sentiment.main.LSD.frame.negshare...100[pc5$no<17]), NA)
pc5$mean <- ifelse(pc5$no>16 & pc5$no<32,
                   mean(pc5$sentiment.main.LSD.frame.negshare...100[pc5$no>16 & pc5$no<32]),
                   pc5$mean)
pc5$mean <- ifelse(pc5$no>31 & pc5$no<59,
                   mean(pc5$sentiment.main.LSD.frame.negshare...100[pc5$no>31 & pc5$no<59]),
                   pc5$mean)
pc5$mean <- ifelse(pc5$no>58 & pc5$no<63,
                   mean(pc5$sentiment.main.LSD.frame.negshare...100[pc5$no>58 & pc5$no<63]),
                   pc5$mean)
pc5$mean <- ifelse(pc5$no>62 & pc5$no<71,
                   mean(pc5$sentiment.main.LSD.frame.negshare...100[pc5$no>62 & pc5$no<71]),
                   pc5$mean)

pc5$changepointcount <- 1
pc5$changepointcount <- ifelse(pc5$no %in% 17:31, 2, pc5$changepointcount)
pc5$changepointcount <- ifelse(pc5$no %in% 32:58, 3, pc5$changepointcount)
pc5$changepointcount <- ifelse(pc5$no %in% 59:62, 4, pc5$changepointcount)
pc5$changepointcount <- ifelse(pc5$no %in% 63:70, 5, pc5$changepointcount)

pc5 <- pc5 %>% spread(changepointcount, mean)


p24 <- ggplot(pc5) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=sentiment.main.LSD.frame.negshare...100), size=0.7) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`1`), size=1.3) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`2`), size=1.3) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`3`), size=1.3) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`4`), size=1.3) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`5`), size=1.3) +
  
  scale_colour_manual(values=cbPalette[3:4]) +
  scale_x_continuous(breaks = c(seq(1951, max(plotdata.main$year), by = 1)),expand = c(0, 0)) +
  ylab("Negative Sentiment Share") +
  xlab("Year") +
  ggtitle("Negative sentiment in UNRWA annual reports (changepoints, penality 6*log(n)") +
  ylim(0,10) +
  #scale_y_continuous(breaks = c(seq(1:12))) +
  theme(
    #  plot.title = element_text(hjust=0.5, size=14, face="bold"),
    # legend.position="none",
    axis.text.x= element_text(angle=90, vjust=0.5, size=8),
    axis.text.y  = element_text(size=8),
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="white" ))

p24

#Figure A6. Negative sentiment, 12*log(n)
pc5 <- data.frame(sentiment.main.LSD.frame$year, sentiment.main.LSD.frame$negshare*100)
pc5$no <- seq(1:nrow(pc5))
keep9 <- as.numeric(cpts(sentiment.main.neg.changepoint12))
pc5$changepoint <- ifelse(pc5$no %in% keep9, 1, 0)
pc5$mean <- ifelse(pc5$no<32, mean(pc5$sentiment.main.LSD.frame.negshare...100[pc5$no<32]), NA)
pc5$mean <- ifelse(pc5$no>31 & pc5$no<71,
                   mean(pc5$sentiment.main.LSD.frame.negshare...100[pc5$no>31 & pc5$no<71]),
                   pc5$mean)

pc5$changepointcount <- 1
pc5$changepointcount <- ifelse(pc5$no %in% 32:70, 2, pc5$changepointcount)

pc5 <- pc5 %>% spread(changepointcount, mean)


p25 <- ggplot(pc5) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=sentiment.main.LSD.frame.negshare...100), size=0.7) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`1`), size=1.3) +
  geom_line(aes(x=(sentiment.main.LSD.frame.year), y=`2`), size=1.3) +
  
  scale_colour_manual(values=cbPalette[3:4]) +
  scale_x_continuous(breaks = c(seq(1951, max(plotdata.main$year), by = 1)),expand = c(0, 0)) +
  ylab("Negative Sentiment Share") +
  xlab("Year") +
  ggtitle("Negative sentiment in UNRWA annual reports (changepoints, penality 12*log(n)") +
  ylim(0,10) +
  #scale_y_continuous(breaks = c(seq(1:12))) +
  theme(
    #  plot.title = element_text(hjust=0.5, size=14, face="bold"),
    # legend.position="none",
    axis.text.x= element_text(angle=90, vjust=0.5, size=8),
    axis.text.y  = element_text(size=8),
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="white" ))

p25
