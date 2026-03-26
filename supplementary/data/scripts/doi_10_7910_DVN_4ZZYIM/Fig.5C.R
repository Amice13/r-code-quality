#Code for plotting pairwise distances
#Written by:  Dr Nyaigoti Agoti

# First import all necessary libraries
rm(list=ls())
library(tidyverse); library(janitor); library(lubridate); library(artyfarty); library(rstatix)


setwd("/Users/cnyaigoti/Dropbox/COVID-19/SECONDWAVE/revisedcode/Data/pairsnp/")


coast_dta <- read.csv("Kenya_seq.csv", stringsAsFactors = F, sep = ",", header=T)
pairwise_dta <- as.matrix(coast_dta, rownames=TRUE)%>%
  replace_upper_triangle(by=NA, diagonal=TRUE)%>%
  pivot_longer((2:738), names_to="seq_id", values_to="nt_diff")%>%
  filter(!is.na(nt_diff))%>%
  mutate(source="Coast")

summary(pairwise_dta$nt_diff)

global_dta <- read.csv("Global_seq.csv", stringsAsFactors = F, sep = ",", header=T)
pairwise2_dta <- as.matrix(global_dta, rownames=TRUE)%>%
  replace_upper_triangle(by=NA, diagonal=TRUE)%>%
  pivot_longer((2:3971), names_to="seq_id", values_to="nt_diff")%>%
  filter(!is.na(nt_diff))%>%
  mutate(source="Global")


summary(pairwise2_dta$nt_diff)

all_dta <- bind_rows(pairwise_dta,pairwise2_dta)

plot1 <- all_dta%>%
  mutate(source=recode(source, "Coast"="Coastal Kenya"))%>%
  ggplot(aes(x=source, y=nt_diff))+
  geom_boxplot()+
  geom_violin(aes(fill=source))+
  scale_fill_manual(values=c("white", "grey"))+
  labs(x="", y="Number of nt pairwise differences")+
  theme_scientific()+
  scale_y_continuous(minor_breaks = seq(0 , 80, 5), breaks = seq(0, 80, 10))+
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

pdf("Fig 5c.pdf", width = 3.5, height = 4.5, family = "Helvetica")
print(plot1)
dev.off()
print ("I need Madondo!")

plot1
print ("I need Madondo!")

t.test(data=all_dta, nt_diff ~ source)
