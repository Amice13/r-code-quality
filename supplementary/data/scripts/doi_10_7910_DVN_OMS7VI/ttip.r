#### Basic information ####
# R script to replicate the results reported in manuscript:
# Of principal(s') interest? A disaggregated, multiple principals' approach to Commission discretion
# Markus Gastinger (TU Dresden) and Johan Adriaensen (CERiM)
# published in JCMS: Journal of Common Market Studies, doi: 10.1111/jcms.12801

#### Header ####
rm(list=ls())
cat("\014")
library(dplyr)
library(Hmisc)
library(ggplot2)
library(reshape2)

#### Load datasets ####
ds.survey <-  read.csv2(file = "survey.csv", stringsAsFactors = FALSE)
ds.directives <- read.csv2(file = "negotiating_directives.csv", stringsAsFactors = FALSE)
ds.questions <- read.csv2(file = "written_questions.csv", stringsAsFactors = FALSE)
ds.resolutions <- read.csv2(file = "EP_resolutions.csv", stringsAsFactors = FALSE)
ds.advisory <- read.csv2(file = "advisory_group.csv", stringsAsFactors = FALSE)
ds.media <- read.csv2(file = "media_germany.csv", stringsAsFactors = FALSE)
colnames(ds.directives)[3] <- "negotiating.directives"
ds.media <- ds.media[,-4]

#### Transform datasets #### 
ds.directives <- ds.directives %>% 
  replace(is.na(.), 0)

ds.questions <- ds.questions %>% 
  replace(is.na(.), 0) %>%
  mutate(written.questions = rowSums(.[3:445]))
ds.questions <- ds.questions[,c(1,2,446)]

ds.advisory <- ds.advisory %>% 
  replace(is.na(.), 0) %>%
  mutate(advisory.group = rowSums(.[3:31]))
ds.advisory <- ds.advisory[,c(1,2,32)]

ds <- full_join(ds.directives, ds.questions, by = "id")
ds <- full_join(ds, ds.resolutions, by = "id")
ds <- full_join(ds, ds.advisory, by = "id")
ds <- full_join(ds, ds.media, by = "id")
ds <- ds[,c(1,2,3,5,7,8,10,12)]
colnames(ds)[2] <- "chapter"

ds <- full_join(ds, ds.survey, by = "chapter")

#### Prepare Table 1 ####
market.access <- c("Services", "Trade in goods", "Trade in Goods and Customs Duties", "Agriculture", "Wine and spirits", "Public Procurement","Rules of Origin")
regulatory.cooperation <- c("Chemicals","Cosmetics","Engineering","Food Safety and Animal and Plant Health (SPS)","Information and Communication Technology (ICT)","Medical Devices","Pesticides","Pharmaceuticals","Regulatory Coherence","Technical Barriers to Trade (TBTs)","Textiles","Vehicles")
rules <- c("Competition","Customs and Trade Facilitation (CTF)","Energy and Raw Materials (ERMs)","Government-Government Dispute Settlement (GGDS)","Intellectual Property (IP) and Geographical Indications (GIs)","Investment","Small and Medium-Sized Enterprises (SMEs)","Sustainable Development")
ds$basket <- as.character(ifelse(ds$chapter %in% market.access, "Market Access", ifelse(ds$chapter %in% regulatory.cooperation, "Regulatory Cooperation", "Rules")))

ds <- ds[,c(1,2,13,11,3,12,4,5,6,10,7,8)]

interest.labels <- c("L", "RL", "RH", "H") # L - Low, RL - Rather low, RH - Rather high, H - High

ds.pub <- ds[,-1]
ds.pub$council <- cut(ds.pub$council, c(1, 2.5, 4, 5.5, 7), labels=interest.labels, include.lowest=TRUE)
ds.pub$EP <- cut(ds.pub$EP, c(1, 2.5, 4, 5.5, 7), labels=interest.labels, include.lowest=TRUE)
ds.pub$citizens <- cut(ds.pub$citizens, c(1, 2.5, 4, 5.5, 7), labels=interest.labels, include.lowest=TRUE)
ds.pub$negotiating.directives <- cut(ds.pub$negotiating.directives, breaks = 4, labels=interest.labels, include.lowest=TRUE)
ds.pub$written.questions <- cut(ds.pub$written.questions, breaks = 4, labels=interest.labels, include.lowest=TRUE)
ds.pub$resolution.2013 <- cut(ds.pub$resolution.2013, breaks = 4, labels=interest.labels, include.lowest=TRUE)
ds.pub$resolution.2015 <- cut(ds.pub$resolution.2015, breaks = 4, labels=interest.labels, include.lowest=TRUE)
ds.pub$advisory.group <- cut(ds.pub$advisory.group, breaks = 4, labels=interest.labels, include.lowest=TRUE)
ds.pub$media.study <- cut(ds.pub$media.study, breaks = 4, labels=interest.labels, include.lowest=TRUE)

colnames(ds.pub) <- c("Chapter", "Basket", "Council", "CND", "EP", "WQ", "R13", "R15", "CS", "AG", "MS")
ds.pub[ds.pub=="Market Access"] <- "MA"
ds.pub[ds.pub=="Regulatory Cooperation"] <- "RCOO"
ds.pub[ds.pub=="Trade in Goods and Customs Duties"] <- "TGCD"
ds.pub[ds.pub=="Public Procurement"] <- "PP"
ds.pub[ds.pub=="Food Safety and Animal and Plant Health (SPS)"] <- "SPS"
ds.pub[ds.pub=="Information and Communication Technology (ICT)"] <- "ICT"
ds.pub[ds.pub=="Technical Barriers to Trade (TBTs)"] <- "TBTs"
ds.pub[ds.pub=="Customs and Trade Facilitation (CTF)"] <- "CTF"
ds.pub[ds.pub=="Energy and Raw Materials (ERMs)"] <- "ERMs"
ds.pub[ds.pub=="Government-Government Dispute Settlement (GGDS)"] <- "GGDS"
ds.pub[ds.pub=="Intellectual Property (IP) and Geographical Indications (GIs)"] <- "IP&GIs"
ds.pub[ds.pub=="Regulatory Coherence"] <- "RCOH"
ds.pub[ds.pub=="Small and Medium-Sized Enterprises (SMEs)"] <- "SMEs"
ds.pub[ds.pub=="Sustainable Development"] <- "SD"
ds.pub <- sapply(ds.pub, as.character)
ds.pub[is.na(ds.pub)] <- "."
ds.pub

#### Prepare Table 3A (included in online Appendix) ####
ds.mx <- as.matrix(ds[,4:12])
cormatrix <- rcorr(ds.mx, type="spearman")
cordata.tmp1 <- melt(cormatrix$r)
cordata.tmp2 <- melt(cormatrix$P)
cordata.tmp2[is.na(cordata.tmp2)] <- 1
colnames(cordata.tmp1) <- c("Var1","Var2","r")
colnames(cordata.tmp2) <- c("Var1","Var2","p")
cordata <- merge(cordata.tmp1, cordata.tmp2, by=c("Var1","Var2"))
cordata$stars <- cut(cordata$p, breaks=c(-Inf, 0.01, 0.05, Inf), label=c("**", "*", ""))
rm(ds.mx,cormatrix,cordata.tmp1,cordata.tmp2)

ggplot(cordata, aes(x=Var1, y=Var2)) + 
  geom_tile(data=cordata, aes(fill=r), color="black") +
  geom_text(aes(fill=cordata$r, label = round(cordata$r, 2))) +
  geom_text(aes(label=stars), color="black", size=5, vjust=1.6) +
  scale_fill_gradient(low="white", high="black", name="rho") +
  theme_minimal() + theme(axis.text.x = element_text(angle=25, hjust=1),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()
        )

sink("sessionInfo.txt")
sessionInfo()
sink()