#script written by Parmit singh
# it makes a plot of distribution of retroviral integration sites and genomic features tabulated into % value along with gene length.
#load ggplot2 package#
library(ggplot2)

#input files are the data table(s). Each file contains two columns: distance from the start and the end of the Refseq geens and the % of total integration sites.
#Refseq genes are divided into 10 equal segments.
#replace the "path of input file" in read.table by the path of input file in your local computer.
#the script can plot any number of inputs by copying and pasting read.table
int1 <- read.table("/Users/parmitsingh/parmit/manuscripts/GD/transcription_map/transcription_map_for2023data/map_genes/transcription_map/ORF/ORF_1.txt",header = FALSE, sep="\t")
View(int1)
colnames(int1)<-c("range","WT_percentage")
View(int1)
####
int2 <- read.table("/Users/parmitsingh/parmit/manuscripts/GD/transcription_map/transcription_map_for2023data/map_genes/transcription_map/ORF/ORF_2.txt",header = FALSE, sep="\t")
View(int2)
colnames(int2)<-c("range","LKO_percentage")
View(int2)
#####
int3 <- read.table("/Users/parmitsingh/parmit/manuscripts/GD/transcription_map/transcription_map_for2023data/map_genes/transcription_map/ORF/ORF_3.txt",header = FALSE, sep="\t")
View(int3)
colnames(int3)<-c("range","CKO_percentage")
View(int3)
######
int4 <- read.table("/Users/parmitsingh/parmit/manuscripts/GD/transcription_map/transcription_map_for2023data/map_genes/transcription_map/ORF/ORF_4.txt",header = FALSE, sep="\t")
View(int4)
colnames(int4)<-c("range","DKO_percentage")
View(int4)
##
int5 <- read.table("/Users/parmitsingh/parmit/manuscripts/GD/transcription_map/transcription_map_for2023data/map_genes/transcription_map/ORF/ORF_5.txt",header = FALSE, sep="\t")
View(int5)
colnames(int5)<-c("range","five_percentage")
View(int5)
####
int6 <- read.table("/Users/parmitsingh/parmit/manuscripts/GD/transcription_map/transcription_map_for2023data/map_genes/transcription_map/ORF/ORF_6.txt",header = FALSE, sep="\t")
View(int6)
colnames(int6)<-c("range","six_percentage")
View(int6)
#####
int7 <- read.table("/Users/parmitsingh/parmit/manuscripts/GD/transcription_map/transcription_map_for2023data/map_genes/transcription_map/ORF/ORF_7.txt",header = FALSE, sep="\t")
View(int7)
colnames(int7)<-c("range","seven_percentage")
View(int7)
######
int8 <- read.table("/Users/parmitsingh/parmit/manuscripts/GD/transcription_map/transcription_map_for2023data/map_genes/transcription_map/ORF/ORF_8.txt",header = FALSE, sep="\t")
View(int8)
colnames(int8)<-c("range","eight_percentage")
View(int8)
###
int9 <- read.table("/Users/parmitsingh/parmit/manuscripts/GD/transcription_map/transcription_map_for2023data/map_genes/transcription_map/ORF/ORF_11.txt",header = FALSE, sep="\t")
View(int9)
colnames(int9)<-c("range","eleven_percentage")
View(int9)
int10 <- read.table("/Users/parmitsingh/parmit/manuscripts/GD/transcription_map/transcription_map_for2023data/map_genes/transcription_map/ORF/ORF_random_sites_AVRII_NheI_SpeI_BamH1.txt",header = FALSE, sep="\t")
View(int10)
colnames(int10)<-c("range","RICU3_percentage")
View(int10)
##
intRIC <- read.table("/Users/parmitsingh/parmit/Input_for_bioinformatics_analysis/MRC_wen/transcription_map/ORF_random_sites_bglII_mseI.txt",header = FALSE, sep="\t")
View(intRIC)
colnames(intRIC)<-c("range","RIC_percentage")
View(intRIC)

#####
cdf=merge(int1, int2,by="range", all=TRUE)
View(cdf)
cdf1=merge(cdf, int3,by="range", all=TRUE)
View(cdf1)
cdf2=merge(cdf1, int4,by="range", all=TRUE)
View(cdf2)
###
cdf3=merge(cdf2, int5,by="range", all=TRUE)
View(cdf3)
cdf4=merge(cdf3, int6,by="range", all=TRUE)
View(cdf4)
cdf5=merge(cdf4, int7,by="range", all=TRUE)
View(cdf5)
cdf7=merge(cdf5, int8,by="range", all=TRUE)
View(cdf7)
cdf8=merge(cdf7, int9,by="range", all=TRUE)
View(cdf8)
cdf9=merge(cdf8, int10,by="range", all=TRUE)
View(cdf9)
####
cdf6=merge(cdf9,intRIC,by="range",all=TRUE)
View(cdf6)

###added 
cdf6[is.na(cdf6)]<-0
#View(cdf6)
#### from here to correct the density
str(cdf)

cdf6.2 <- cdf6
cdf6.2 <- cdf6[3201:3210,]
#r=c(3201,3204,3208,3212,3216,3220)
#cdf6.2<-cdf6[r,]
cdf6.2$range<-(c(10,20,30,40,50,60,70,80,90,100))
View(cdf6.2)
#cdf6.2$density <- as.numeric(cdf6.2$density)
cdf6.2$range <- as.numeric(cdf6.2$range)
#plotting
gp<-ggplot(cdf6.2, aes(x=range, y=RICU3_percentage, color="RIC_U3"))+geom_area( fill="lightgrey")+theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.border = element_blank(),text=element_text(size=24, face ="bold"), legend.title=element_blank(), axis.line = element_line(size = 1, colour = "black")) +
  labs(x = "% of gene length", y = "% of Integration")+ggtitle("HIV-1")+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0),limits = c(0,10))+
  geom_smooth(aes(x=range, y=CKO_percentage, color="Set KO1"),se=FALSE)+
  geom_smooth(aes(x=range, y=LKO_percentage, color="WT2"),se=FALSE)+
  geom_smooth(aes(x=range, y=DKO_percentage, color="Set KO2"),se=FALSE)+
  geom_smooth(aes(x=range, y=WT_percentage, color="WT1"),se=FALSE)+
  geom_smooth(aes(x=range, y=five_percentage, color="ML WT1"),se=FALSE)+
  geom_smooth(aes(x=range, y=six_percentage, color="ML WT2"),se=FALSE)+
  geom_smooth(aes(x=range, y=seven_percentage, color="ML KO1"),se=FALSE)+
  geom_smooth(aes(x=range, y=eight_percentage, color="ML KO2"),se=FALSE)+
  geom_smooth(aes(x=range, y=eleven_percentage, color="eleven"),se=FALSE)+
  
  scale_color_manual(values = c("WT1"="black",  "WT2"="red", "Set KO1"="green", "Set KO2"="blue","five"="brown","six"="magenta", "seven"="cyan","eight"="coral","eleven"="violet", "RIC_U3"="grey"))+guides(color=guide_legend(override.aes=list(fill=NA))) 

show(gp)







