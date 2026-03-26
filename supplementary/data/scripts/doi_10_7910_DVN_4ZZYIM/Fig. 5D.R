#Code for plotting pairwise distances
#Written by:  Dr Nyaigoti Agoti

# First import all necessary libraries
rm(list=ls())
library(tidyverse); library(janitor); library(lubridate); library(artyfarty); library(rstatix);
#library(data.table)


setwd("/Users/cnyaigoti/Dropbox/COVID-19/SECONDWAVE/revisedcode/Data/nextclade/")


#coast_dta <- read.csv("./Kenya/nextclade/nextclade.csv")

coast_dta <- as.data.frame(data.table::fread("./Kenya/nextclade/nextclade.tsv"))%>%
  select(seqName, totalAminoacidSubstitutions)%>%
  mutate(source="Coast")

summary(coast_dta$totalAminoacidSubstitutions)

head(coast_dta)

global_dta <- as.data.frame(data.table::fread("./Global/nextclade/nextclade.tsv"))%>%
  select(seqName, totalAminoacidSubstitutions)%>%
  filter(totalAminoacidSubstitutions <80)%>%
  mutate(source="Global")

summary(global_dta$totalAminoacidSubstitutions)

plot1 <- bind_rows(coast_dta,global_dta )%>%
  mutate(source=recode(source, "Coast"="Coastal Kenya"))%>%
  ggplot(aes(x=source, y=totalAminoacidSubstitutions))+
  geom_boxplot()+
  geom_violin(aes(fill=source))+
  scale_fill_manual(values=c("white", "grey"))+
  labs(x="", y="Number of amino acid changes")+
  theme_scientific()+
  scale_y_continuous(minor_breaks = seq(0 , 50, 5), breaks = seq(0, 50, 10))+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none",
        legend.key.size = unit(0.45, "cm"),
        legend.spacing.x = unit(0.5, 'cm'),
        legend.text = element_text(size = 14),
        legend.title =element_blank(),
        legend.box.background = element_blank())+
  guides(color = FALSE)

pdf("Fig 5d.pdf", width = 3.5, height = 4.5, family = "Helvetica")
print(plot1)
dev.off()
print ("I need Madondo!")

plot1
print ("I need Madondo!")
