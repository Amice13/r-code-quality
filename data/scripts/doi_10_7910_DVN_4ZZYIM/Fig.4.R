# Plotting lineages by county # Figure 3
# Clear any data in memory, load necessary packages and set working directory

rm(list=ls())
library(tidyverse); library(stringr); library(lubridate); library(scales); library(RColorBrewer)
library(wesanderson); library(artyfarty); library(janitor); library(ggpubr); library(phylotools)

setwd("~/Dropbox/COVID-19/SECONDWAVE/revisedcode/Figures/Fig.4/")
min <- as.Date("2020-02-28")
max <- as.Date("2021-02-28")



#1. Figure 4A
#Import all required data
load("~/Dropbox/COVID-19/SECONDWAVE/revisedcode/Data/Lineages_dta_11Aug2021.RData")
names(Lineages_dta)

#Identify lineages to show by assessing distribution: If Frequency is >5 OR Variant of Interest OR varaint of Concern
#lineage_distribution <- tabyl(county_lineage_dta, Lineage)%>%
#  rename(Freq=n)%>%
#  filter(Freq>5 |Lineage=="B.1.1.7"|Lineage=="B.1.525"|Lineage=="A.23")

major_lineages <- c("A","A.23","A.23.1","B","B.1","B.1.1","B.1.1.33","B.1.1.7","B.1.333",
                    "B.1.351","B.1.525","B.1.530","B.1.535","B.1.549","B.1.596.1","B.1.612","N.8")


minor_lineages <-c("A.25","AL.1","B.1.1.1","B.1.13","B.1.153","B.1.157","B.1.160","B.1.177","B.1.179",
                   "B.1.222","B.1.229","B.1.281","B.1.393","B.1.551","B.1.558","B.1.593","B.39","B.4","B.4.7")

county_lineages_dta <- Lineages_dta%>%
  mutate(Count=1)%>%
  mutate(Lineage=as.character(Lineage))%>%
  mutate(key_lineages=ifelse(Lineage%in%major_lineages, Lineage, "Other Kenya"))%>%
  mutate(key_lineages=factor(key_lineages, levels=c("A","A.23","A.23.1","B","B.1","B.1.1","B.1.1.33","B.1.1.7","B.1.333",
                                                    "B.1.351","B.1.525","B.1.530","B.1.535","B.1.549","B.1.596.1","B.1.612","N.8", "Other Kenya")))

plot_color=c("#000000","#C0C0C0","#696969","#FF0000","#F2D2BD",
             "#800000","#C9A9A6","#00FF00","#008000","#00FFFF","#BA6B57", "#FFA500","#9933FF",
             "#A4C639","#0000FF","#CCCCFF","#FF00FF","#55ACEE")

#1. Plot weekly pango lineage distribution by county across the study period

# Plot weekly pango lineage distribution by county across the study period
plot1 <- ggplot(county_lineages_dta, aes(x=Forthnight, y=Count,fill=key_lineages))+
  stat_summary(fun=sum, geom="bar", position = "fill")+
  scale_fill_manual(values=plot_color)+
  labs(y="Fraction of genomes", x="")+
  theme_scientific()+
  scale_x_date(breaks ="2 month", date_minor_breaks="1 month", labels = date_format("%b %Y"), limits = c(min,max))+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 11, angle=0),
        axis.text.y = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size = 12, face="bold"),
        #legend.position = c(0.75, 0.750),
        #legend.position = "bottom",
        legend.key.size = unit(0.25, "cm"),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.text = element_text(size = 10),
        legend.title =element_text(size = 10),
        legend.box.background = element_blank())+
  guides(fill=guide_legend(ncol=1, title = "Lineage"), size=T)
pdf("~/Dropbox/COVID-19/SECONDWAVE/revisedcode/Figures/Fig.4/Fig 4A.pdf", width = 7, height = 3)
print(plot1)
dev.off()

#2. Figure 4B

#Load the Africa lineages dataset, FOR PANELS B and C
my_dta <- read.csv("~/Dropbox/COVID-19/SECONDWAVE/revisedcode/Data/All_Africa_lineage_report_11Aug2021.csv", 
                   sep=",", stringsAsFactors = F, header=T)

str(my_dta)

Africa_rawdta <- my_dta%>%
  separate(taxon, into=c("id", "gisaid_no", "date"), remove=F, extra="warn", sep="\\|")%>%
  separate(id, into=c("virus", "country", "sample_id", "year", "other"), remove=F, extra="warn", sep="/")%>%
  mutate(country =recode(country, SouthAfrica="South_Africa", BurkinaFaso="Burkina_Faso"))%>%
  filter(country!="Kenya")

Africa_dta <-Africa_rawdta%>%
  mutate(collectiondate=as.Date(date, format="%Y-%m-%d"))%>%
  filter(collectiondate>min &collectiondate<max)%>%
  mutate(Monthly =as.Date(cut(collectiondate, breaks = "month")))%>%
  mutate(Weekly =as.Date(cut(collectiondate, breaks = "week", start.on.Monday=FALSE)))%>%
  mutate(Biweekly =as.Date(cut(collectiondate, breaks = "2 weeks", start.on.Monday=FALSE)))%>%
  filter(collectiondate>as.Date("2020-03-16") & collectiondate<as.Date("2021-02-27"))%>%
  filter(lineage!="None")


#Panel C, Eastern Africa lineages plot
plot_color_EA=c("#000000","#C0C0C0","#696969","#FF0000","#F2D2BD","#800000","#00FF00",
             "#008000","#00FFFF","#BA6B57", "#9933FF","#55ACEE","#FFD400",
             "#BFFF00","#0F4D92")

EA_major_lineages <- c("A","A.23","A.23.1","B","B.1","B.1.1","B.1.1.33","B.1.1.7","B.1.333",
                       "B.1.351","B.1.525","B.1.530","B.1.535","B.1.549","B.1.596.1","B.1.612","N.8", "B.1.351.2", "B.1.380")



Other_Kenya <-c("A.25","AL.1","B.1.1.1","B.1.13","B.1.153","B.1.157","B.1.160","B.1.177","B.1.179",
                   "B.1.222","B.1.229","B.1.281","B.1.393","B.1.551","B.1.558","B.1.593","B.39","B.4","B.4.7")

EA_noted_lineages <- append(EA_major_lineages, Other_Kenya)


Eastern_Africa_dta<-Africa_dta%>%
  filter(country=="Uganda"|country=="Rwanda"|country=="Ethiopia"|
           country=="Comoros"|country=="Madagascar"|country== "Mozambique"|country=="Reunion"|
           country=="Malawi"|country=="Zimbabwe"|country=="Zambia")%>%
  mutate(Pango_Lineage=ifelse(lineage%in%EA_noted_lineages, lineage, "Absent Kenya"))%>%
  mutate(Pango_Lineage=ifelse(Pango_Lineage%in%Other_Kenya, "Other Kenya", Pango_Lineage))%>%
  mutate(Pango_Lineage=factor(Pango_Lineage, levels=c("A","A.23","A.23.1","B","B.1","B.1.1","B.1.1.7","B.1.333",
                                                    "B.1.351","B.1.525","B.1.535","Other Kenya", "B.1.351.2", "B.1.380", "Absent Kenya")))
EA_Lineages <- tabyl(Eastern_Africa_dta, lineage)
plot2 <- Eastern_Africa_dta%>%
  select(collectiondate, Biweekly, Weekly, Pango_Lineage)%>%
  mutate(Count=1)%>%
  arrange(collectiondate)%>%
  group_by(Pango_Lineage)%>%
  ggplot(aes(x=Biweekly, y=Count,fill=Pango_Lineage))+
  stat_summary(fun=sum, geom="bar", position = "fill")+
  scale_fill_manual(values=plot_color_EA)+
  labs(y="Fraction of genomes", x="")+
  theme_scientific()+
  scale_x_date(breaks ="2 month", date_minor_breaks="1 month", labels = date_format("%b %Y"),limits = c(min,max))+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 11, angle=0),
        axis.text.y = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size = 12, face="bold"),
        legend.key.size = unit(0.25, "cm"),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.text = element_text(size = 10),
        legend.title =element_text(size = 10),
        legend.box.background = element_blank())+
  guides(fill=guide_legend(ncol=1, title = "Lineage"), size=T)

pdf("~/Dropbox/COVID-19/SECONDWAVE/revisedcode/Figures/Fig.4/Fig.4B.pdf", width = 7, height = 3)
print(plot2)
dev.off()

#4. Africa lineages plot
#Panel C, Africa lineages plot
plot_color_Africa=c("#000000","#C0C0C0","#696969","#FF0000","#F2D2BD","#800000","#00FF00","#008000","#00FFFF","#BA6B57","#9933FF","#CCCCFF","#55ACEE","#FFD400", "#B9EDE0","#0F4D92")

names(Africa_dta)

Africa_Freq <-tabyl(Africa_dta, lineage)

Africa_major_lineages <- c("A","A.23","A.23.1","B","B.1","B.1.1","B.1.1.33","B.1.1.7","B.1.333",
                       "B.1.351","B.1.525","B.1.530","B.1.535","B.1.549","B.1.596.1","B.1.612","N.8", "B.1.351.2", "B.1.1.448")

Other_Kenya <-c("A.25","AL.1","B.1.1.1","B.1.13","B.1.153","B.1.157","B.1.160","B.1.177","B.1.179",
                "B.1.222","B.1.229","B.1.281","B.1.393","B.1.551","B.1.558","B.1.593","B.39","B.4","B.4.7")

Africa_noted_lineages <- append(Africa_major_lineages, Other_Kenya)


Africa_lineages_dta<-Africa_dta%>%
  mutate(Pango_Lineage=ifelse(lineage%in%Africa_noted_lineages, lineage, "Absent Kenya"))%>%
  mutate(Pango_Lineage=ifelse(Pango_Lineage%in%Other_Kenya, "Other Kenya", Pango_Lineage))%>%
  mutate(Pango_Lineage=factor(Pango_Lineage, levels=c("A","A.23","A.23.1","B","B.1","B.1.1","B.1.1.7","B.1.333",
                                                      "B.1.351","B.1.525","B.1.535","B.1.612", "Other Kenya", "B.1.351.2", "B.1.1.448", "Absent Kenya")))






plot3 <- Africa_lineages_dta%>%
  select(collectiondate, Biweekly, Weekly, Pango_Lineage)%>%
  mutate(Count=1)%>%
  arrange(collectiondate)%>%
  group_by(Pango_Lineage)%>%
  ggplot(aes(x=Biweekly, y=Count,fill=Pango_Lineage))+
  stat_summary(fun=sum, geom="bar", position = "fill")+
  scale_fill_manual(values=plot_color_Africa)+
  labs(y="Fraction of genomes", x="")+
  theme_scientific()+
  scale_x_date(breaks ="2 month", date_minor_breaks="1 month", labels = date_format("%b %Y"),limits = c(min,max))+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 11, angle=0),
        axis.text.y = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size = 12, face="bold"),
        legend.key.size = unit(0.25, "cm"),
        legend.spacing.x = unit(0.2, 'cm'),
        legend.spacing.y = unit(0.2, 'cm'),
        legend.text = element_text(size = 11),
        legend.title =element_text(size = 11),
        legend.box.background = element_blank())+
  guides(fill=guide_legend(ncol=1, title = "Lineage"), size=T)

pdf("~/Dropbox/COVID-19/SECONDWAVE/revisedcode/Figures/Fig.4/Fig.4C.pdf", width = 7, height = 3)
print(plot3)
dev.off()




#global, global sub-sampled data, panel D
global_dta <- as_tibble(read.csv("~/Dropbox/COVID-19/SECONDWAVE/revisedcode/Data/subsampled_global/lineage_report.csv"))%>%
  distinct(taxon, .keep_all=T)

str(global_dta)


Lineages_dta<- global_dta%>%
  separate(taxon, into=c("id", "date_collection","date_submission"), remove=F, sep="\\|")%>%
  separate(id, into=c("virus", "country","sample_id", "year"), remove=F, sep="\\/")%>%
  mutate(country=trimws(country, which = c("both", "left", "right"), whitespace = "[ \t\r\n]"))%>%
  mutate(collectiondate=as.Date(date_collection, format="%Y-%m-%d"))%>%
  mutate(Weekly =as.Date(cut(collectiondate, breaks = "week", start.on.Monday=FALSE)))%>%
  mutate(Biweekly =as.Date(cut(collectiondate, breaks = "2 weeks", start.on.Monday=FALSE)))%>%
  rename(Lineage=lineage)%>%
  filter(Lineage!="None")%>%
  filter(country!="Kenya")
  
Global_Freq <-tabyl(Lineages_dta, Lineage)
Country_Freq <-tabyl(Lineages_dta, country)


Global_major_lineages <- c("A","A.23","A.23.1","B","B.1","B.1.1","B.1.1.33","B.1.1.7","B.1.333",
                           "B.1.351","B.1.525","B.1.530","B.1.535","B.1.549","B.1.596.1","B.1.612","N.8", "D.2", "B.1.2")

Other_Kenya <-c("A.25","AL.1","B.1.1.1","B.1.13","B.1.153","B.1.157","B.1.160","B.1.177","B.1.179",
                "B.1.222","B.1.229","B.1.281","B.1.393","B.1.551","B.1.558","B.1.593","B.39","B.4","B.4.7")

Global_noted_lineages <- append(Africa_major_lineages, Other_Kenya)


Global_Lineages_dta <- Lineages_dta%>%
  mutate(Pango_Lineage=ifelse(Lineage%in%Global_major_lineages, Lineage, "Absent Kenya"))%>%
  mutate(Pango_Lineage=ifelse(Pango_Lineage%in%Other_Kenya, "Other Kenya", Pango_Lineage))%>%
  mutate(month=format(collectiondate, format="%Y-%m"))

tabyl(Global_Lineages_dta, Pango_Lineage)


plot_color_global=c("#000000","#C0C0C0","#696969","#FF0000","#F2D2BD",
                    "#800000","#C9A9A6","#00FF00","#008000","#00FFFF","#BA6B57","#FFA500","#9933FF", "#0000FF","#CCCCFF","#F1948A","#82E0AA","#0F4D92")

plot4 <- Global_Lineages_dta %>%
  mutate(Pango_Lineage=factor(Pango_Lineage, levels=c("A","A.23","A.23.1","B","B.1","B.1.1","B.1.1.33","B.1.1.7","B.1.333",
                                                          "B.1.351","B.1.525","B.1.530","B.1.535","B.1.596.1","B.1.612", "Other Kenya", "D.2", "B.1.2", "Absent Kenya")))%>%
  select(collectiondate, Biweekly, Pango_Lineage)%>%
  mutate(Count=1)%>%
  arrange(collectiondate)%>%
  group_by(Pango_Lineage)%>%
  ggplot(aes(x=Biweekly, y=Count,fill=Pango_Lineage))+
  stat_summary(fun=sum, geom="bar", position = "fill")+
  scale_fill_manual(values=plot_color_global)+
  labs(y="Fraction of genomes", x="")+
  theme_scientific()+
  scale_x_date(breaks ="2 month", date_minor_breaks="1 month", labels = date_format("%b %Y"), limits = c(min,max))+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 11, angle=0),
        axis.text.y = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size = 12, face="bold"),
        legend.key.size = unit(0.25, "cm"),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.text = element_text(size = 10),
        legend.title =element_text(size = 10),
        legend.box.background = element_blank())+
  guides(fill=guide_legend(ncol=1, title = "Lineage"), size=T)

pdf("~/Dropbox/COVID-19/SECONDWAVE/revisedcode/Figures/Fig.4/Fig.4D.pdf", width = 7, height = 3)
print(plot4)
dev.off()


################# Select  samples to include as background in the trees.....##############


global_seq <- as_tibble(read.csv("~/Dropbox/COVID-19/SECONDWAVE/revisedcode/Data/subsampled_global/subsampled_wave2_info_details.csv"))

global_seq_2 <-global_seq%>%
  rename(taxon=seq_id)

global_seq_lineages <-merge(global_seq_2,Global_Lineages_dta, by=c("taxon"))%>%
  filter(Ns<5980)%>%
  filter(Lineage!="")%>%
  distinct(taxon, .keep_all=T)%>%
  mutate(taxa_name=paste(Lineage, country, sample_id, collectiondate, sep="|"))

Kenya_lineages <-c("B.1", "B.1.1", "B.1.333", "B.1.1.33", "N.8", "B.1.535", "B.1.530", "B.1.549", "B.1.596.1")


subset_lineages<- global_seq_lineages%>%
  filter(Lineage%in%Kenya_lineages)

tabyl(subset_lineages, Lineage)


export_fasta <-global_seq_lineages%>%
  filter(Lineage=="B.1.535")%>%
  select(taxa_name, actualseq)%>%
  rename(seq.name=taxa_name, seq.text=actualseq)

  
setwd("~/Dropbox/COVID-19/SECONDWAVE/revisedcode/Data/Lineages/")  
dat2fasta(export_fasta, outfile = "Global.B.1.535.fasta")  
  
write.csv(global_seq_lineages, file = "subsample_one_12Aug21.csv", row.names = F, na="")


pdf("Fig.4.pdf", width = 12, height = 12, family = "Helvetica")

ggarrange(plot1, plot2, plot3, plot4, labels=c("","", "",""),label.x=0.1, label.y=0.90, ncol=1, nrow=4,
          font.label = list(size =7, face = "bold", color ="black"))
dev.off()

# distribution of globally selected sequences
tabyl(Global_Lineages_dta,month)%>%summarise(range=range(n), mean =mean(n), median=median(n))

# count the number of countries included
tabyl(Global_Lineages_dta,country)%>%mutate(present=1)%>%summarise(count=sum(present))

