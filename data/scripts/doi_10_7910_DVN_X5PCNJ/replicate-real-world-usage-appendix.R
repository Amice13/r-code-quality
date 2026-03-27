library(tidyverse); library(readxl)

######################################
# Table A-1 (Senator Press Releases) # 
######################################
load("senator-nominee-press.RData")

# Proportion of releases that reference each principle
round(length(which(psrm.nominee.releases.df$activism == 1))/length(psrm.nominee.releases.df$senator),3) # Activism
round(length(which(psrm.nominee.releases.df$actual.phrases == 1))/length(psrm.nominee.releases.df$senator),3) # Constitutional phrases
round(length(which(psrm.nominee.releases.df$federalism == 1))/length(psrm.nominee.releases.df$senator),3) # Federalism
round(length(which(psrm.nominee.releases.df$foreign.law == 1))/length(psrm.nominee.releases.df$senator),3) # Foreign Law
round(length(which(psrm.nominee.releases.df$living.constitution == 1))/length(psrm.nominee.releases.df$senator),3) # Living Constitution
round(length(which(psrm.nominee.releases.df$original.intent == 1))/length(psrm.nominee.releases.df$senator),3) # Original Intent
round(length(which(psrm.nominee.releases.df$precedent == 1))/length(psrm.nominee.releases.df$senator),3) # Precedent
round(length(which(psrm.nominee.releases.df$strict.construction == 1))/length(psrm.nominee.releases.df$senator),3) # Strict Construction
round(length(which(psrm.nominee.releases.df$constitutional.text == 1))/length(psrm.nominee.releases.df$senator),3) # Textualism

# Proportion of releases that reference at least one principle
psrm.nominee.releases.df %>%
  mutate(any_principle = original.intent + 
           precedent +
           foreign.law +
           activism +
           federalism +
           constitutional.text +
           actual.phrases +
           living.constitution +
           strict.construction,
         any_principle = ifelse(any_principle > 0, 1, 0)) %>%
  pull(any_principle) %>%
  mean

#########################################
# Figure A-1 (Network News Transcripts) # 
#########################################
load("network-news-nominee.RData")

news_filter_nomination <- psrm.network.news.df %>% 
  mutate(any_principle = 1 * ((original.intent + precedent + foreign.law + activism +
                                 federalism + constitutional.text + actual.phrases + living.constitution + strict.construction) > 0)) %>% 
  group_by(nomination) %>% 
  dplyr::summarize(any_principle_prop = mean(any_principle), any_principle_n = sum(any_principle), n = n())

pdf("figure-a-1.pdf")
news_filter_nomination %>%
  ggplot(aes(y = any_principle_prop, x = nomination)) +
  geom_col() +
  theme_classic() +
  xlab("Nomination") +
  ylab("Proportion of Segments Discussing Principles") +
  ylim(0,.3) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

psrm.network.news.df %>% 
  mutate(any_principle = 1 * ((original.intent + precedent + foreign.law + activism +
                                 federalism + constitutional.text + actual.phrases + living.constitution + strict.construction) > 0)) %>%
  pull(any_principle) %>%
  mean()

####################################################
# Figure A-2 (Senate Judiciary Committee Hearings) # 
####################################################
# Downloading and reading in the Collins and Ringhand SJC data, hosted at: https://blogs.umass.edu/pmcollins/data/ ("The U.S. Supreme Court Confirmation Hearing Database")
# For more information, see: https://stackoverflow.com/questions/3053833/using-r-to-download-zipped-data-file-extract-and-import-data
# For temporarily storing downloaded files
temporary <- tempfile()
temporary2 <- tempfile()
# Downloading the zip file
download.file(url="https://www.dropbox.com/s/jf178f4t5wch7nh/SCCH%20Database.zip?dl=1", temporary)
# Unzipping and reading in the hearings data
unzip(zipfile = temporary, exdir = temporary2)
hearings <- read_xlsx(file.path(temporary2, "/SCCH Database/SCCH Database/7-25-16 SCCHD 1939-2010.XLSX"))
# Removing the temporary files
unlink(c(temporary, temporary2))

# Issue 102 is philosophy; look at mentions by party
partytable <- table(hearings$subis[hearings$issue == 102], hearings$senpar[hearings$issue == 102])
# Naming rows and columns
row.names(partytable) <- c("General","Restraint/Activism","Intent/Meaning","Living Constitution","Text as Interpretive","Precedent","SOP")
colnames(partytable) <- c("Republicans","Democrats")
# Making the table a percentage of all the party's questions they asked in the hearings
percentagetable <- cbind(round(partytable[,1]/length(which(hearings$senpar == 0)), 4), round(partytable[,2]/length(which(hearings$senpar == 1)), 4))
# Adding a total percentage (for all principles questions); can be calculated by apply(percentagetable, 2, sum)
percentagetable <- rbind(c(0.1564,0.1052), percentagetable)
# Naming rows and columns
row.names(percentagetable)[1] <- "All"
colnames(percentagetable) <- c("Republicans","Democrats")

# Barplot of R/D philosophy discussion rates by topic
pdf("figure-a-2.pdf")
b1 <- barplot(t(percentagetable), main="",
              xlab="Category of Philosophy Question", ylab="Proportion of All Questions Asked by Party", col=c("red","blue"),
              legend = colnames(percentagetable), beside=TRUE, xaxt="n", ylim=c(0,.2))
text(cex=0.8, rownames(percentagetable), x=b1[1,], y=-.008, srt=20, las=2, xpd=T)
dev.off()
