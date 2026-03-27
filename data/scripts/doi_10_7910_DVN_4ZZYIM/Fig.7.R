#plot mutations
#install.packages("Hmisc")

#prepare analysis environment
rm(list=ls())
library(tidyverse); library(janitor); library(data.table);library(artyfarty); library(Hmisc)

#Target lineages to plot for Figure 7
list_exlude <- scan("~/Dropbox/COVID-19/SECONDWAVE/revisedcode/Figures/Fig.7/exclude.txt",  what="", sep="\n") 
lineages_of_interest <- c("B.1", "B.1.1", "B.1.1.33", "N.8", "B.1.333","B.1.535", "B.1.530", "B.1.549", "B.1.596.1")
Kenya <-c("Mombasa", "Kilifi", "Kwale", "Taita Taveta", "Tana River", "Lamu")
global_dta <- as.data.frame(data.table::fread("~/Dropbox/COVID-19/SECONDWAVE/revisedcode/Data/nextclade/Global/nextclade/nextclade.tsv", fill=TRUE))%>%
  select(seqName, clade, totalSubstitutions, totalAminoacidSubstitutions)


coast_dta<- as.data.frame(data.table::fread("~/Dropbox/COVID-19/SECONDWAVE/revisedcode/Data/nextclade/Kenya/nextclade/nextclade.tsv", fill=TRUE))%>%
  select(seqName, clade, totalSubstitutions, totalAminoacidSubstitutions)

others_dta<- as.data.frame(data.table::fread("~/Dropbox/COVID-19/SECONDWAVE/revisedcode/Data/nextclade/others/nextclade/nextclade.tsv", fill=TRUE))%>%
  select(seqName, clade, totalSubstitutions, totalAminoacidSubstitutions)


full_dta <-rbind(global_dta, coast_dta, others_dta)%>%
  distinct(seqName, .keep_all=TRUE)%>%
  separate(seqName, into=c("Lineage", "Country", "Sample_id", "datecollection"), sep="\\|", remove=FALSE)%>%
  filter(Lineage%in%lineages_of_interest)%>%
  mutate(source=ifelse(str_detect(seqName, pattern="Mombasa|Kilifi|Kwale|Taita Taveta|Tana River|Lamu"), "Coast", "Global"))%>%
  mutate(Lineage=factor(Lineage, levels=c("B.1","B.1.1", "B.1.333", "B.1.1.33",  "B.1.530", "B.1.535", "B.1.549", "B.1.596.1",  "N.8")))%>%
  filter(!seqName%in%list_exlude)

plot1 <-ggplot(data=full_dta, aes(x=Lineage, y=totalSubstitutions))+
  stat_summary(aes(color=source),
               fun.data = "mean_sdl", 
               fun.args = list(mult=1), 
               geom="pointrange", 
               position = position_dodge(width = 0.9))+
  labs(x="", y="Nucleotide Substitutions")+
  theme_scientific()+
  scale_color_manual(values=pal("flat")[c(1,4)])+
  scale_y_continuous(limits = c(0,30), minor_breaks = seq(0,30, 2.5), breaks = seq(0, 30, 5))+
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle=90, vjust = 0.5),
        strip.text.x = element_text(size=10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 10, face="bold"),
        legend.position = c(0.15,0.8),
        legend.key.size = unit(0.35, "cm"),
        legend.spacing.x = unit(0.20, 'cm'),
        legend.spacing.y = unit(0.20, 'cm'),
        legend.text = element_text(size = 10),
        legend.title =element_text(size = 10),
        strip.background = element_rect(fill="white", color = "white"),
        panel.spacing.x = unit(1.2,"lines"),
        legend.box.background = element_blank())+
  guides(color=guide_legend(nrow=4, title = "Source",reverse = TRUE), size=T)
pdf("~/Dropbox/COVID-19/SECONDWAVE/revisedcode/Figures/Fig.7/Fig.7A.pdf", width =5, height=3)
print(plot1)
dev.off()



plot2 <-ggplot(data=full_dta, aes(x=Lineage, y=totalAminoacidSubstitutions))+
  stat_summary(aes(color=source),
               fun.data = "mean_sdl", 
               fun.args = list(mult=1), 
               geom="pointrange", 
               position = position_dodge(width = 0.9))+
  labs(x="", y="Amino acid changes")+
  theme_scientific()+
  scale_color_manual(values=pal("flat")[c(1,4)])+
  scale_y_continuous(limits = c(0,30), minor_breaks = seq(0,30, 2.5), breaks = seq(0, 30, 5))+
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle=90, vjust = 0.5),
        strip.text.x = element_text(size=10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 10, face="bold"),
        legend.position = c(0.15,0.8),
        legend.key.size = unit(0.35, "cm"),
        legend.spacing.x = unit(0.20, 'cm'),
        legend.spacing.y = unit(0.20, 'cm'),
        legend.text = element_text(size = 10),
        legend.title =element_text(size = 10),
        strip.background = element_rect(fill="white", color = "white"),
        panel.spacing.x = unit(1.2,"lines"),
        legend.box.background = element_blank())+
  guides(color=guide_legend(nrow=4, title = "Source", reverse = TRUE), size=T)
pdf("~/Dropbox/COVID-19/SECONDWAVE/revisedcode/Figures/Fig.7/Fig.7B.pdf", width =5, height=3)
print(plot2)
dev.off()



