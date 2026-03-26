
rm(list=ls())
library(tidyverse);library(scales); library(lubridate); library(artyfarty)


#specify working directory
setwd("~/Dropbox/COVID-19/SECONDWAVE/revisedcode/Figures/Fig.2/")


coast_MoH_dta <- read.csv("~/Dropbox/COVID-19/SECONDWAVE/revisedcode/Data/coastal_counties_smoothed.csv", header = T, sep=",", stringsAsFactors = T)
names(coast_MoH_dta)


coast_MoH_dta<-coast_MoH_dta%>%
  pivot_longer(cols=c("Mombasa", "Kilifi", "Kwale", "TaitaTaveta", "TanaRiver", "Lamu"), names_to="County", values_to="Cases")%>%
  mutate(Date=as.Date(date, "%d-%b-%y"))%>%
  mutate(County=factor(County, levels=c("Mombasa", "Kilifi", "Kwale", "TaitaTaveta", "TanaRiver", "Lamu")))

color_coast <- c("#FF0000","#00FF00","#00FFFF",'#FFA500','#0000FF',"#FF00FF")# 

###########################################################################################################
# Plot cases observed in the different coastal Kenya counties
plot1 <- ggplot(coast_MoH_dta, aes(x=Date, y=Cases))+
  geom_line(aes(color=County), size=0.6)+
  scale_color_manual(values = color_coast)+
  labs(y="Positive tests/million people", x="Month in 2020/21")+
  theme_scientific()+
  scale_y_continuous(limits = c(0,140), minor_breaks=seq(0, 140, 15), breaks=seq(0, 140, 30))+
  scale_x_date(breaks ="2 month", date_minor_breaks="2 month", labels = date_format("%b"))+
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle =0),
        axis.text.y = element_text(size = 10),
        strip.text.x = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size = 11, face="bold"),
        legend.position = c(0.2, 0.70),
        legend.key.size = unit(0.25, "cm"),
        legend.spacing.x = unit(0.25, 'cm'),
        legend.spacing.y = unit(0.25, 'cm'),
        legend.text = element_text(size = 10),
        legend.title =element_text(size = 10),
        legend.background=element_rect(fill = alpha("white", 0)))+
  guides(color=guide_legend(ncol=1, title = "County"), size=T)
pdf("Fig.2A.pdf", width = 4, height =4, family = "Helvetica")
print(plot1)
dev.off()

#Testing at KWTRP
###########################################################################################################
kwtrp_test_dta <-read.csv("~/Dropbox/COVID-19/SECONDWAVE/revisedcode/Data/daily_summaries_26_Feb_2020.csv", sep=",", header = TRUE, stringsAsFactors = TRUE)%>%
  mutate(datetested=as.Date(datetested, format="%Y-%m-%d"))%>%
  filter(datetested< as.Date("2021-02-28", format= "%Y-%m-%d"))%>%
  mutate(daily_coast_pos=(daily_pos_mombasa+daily_pos_kilifi+daily_pos_kwale+daily_pos_taita_taveta+daily_pos_tana_river+daily_pos_lamu))%>%
  mutate(daily_coast_tests=(daily_mombasa+daily_kilifi+daily_kwale+daily_taita_taveta+daily_tana_river+daily_lamu))%>%
  select(datetested,daily_lamu,daily_coast_tests,daily_coast_pos)%>%
  mutate(Monthly =as.Date(cut(datetested,breaks="month")))%>%
  group_by(Monthly)%>%
  summarise(monthly_coast_tests=sum(daily_coast_tests), monthly_coast_pos=sum(daily_coast_pos))%>%
  rename(Tests_done=monthly_coast_tests, Positive_tests=monthly_coast_pos)

names(kwtrp_test_dta)
############################################################################################################
# Figure showing tests done at KWTRP

plot2 <-kwtrp_test_dta%>%
  pivot_longer(cols=c("Tests_done", "Positive_tests"), names_to="tests", values_to="count")%>%
  mutate(tests=recode(tests, Tests_done="Tests done", Positive_tests="Positive tests"))%>%
  mutate(tests=factor(tests, levels=c("Tests done", "Positive tests")))%>%
  ggplot(aes(x=Monthly, y= count))+
  geom_col(aes(fill=tests), position = "dodge", color="black", width = 14)+
  labs(y="Count of tests", x="Month in 2020/21")+
  theme_scientific()+
  scale_x_date(breaks ="2 month", date_minor_breaks="1 month", labels = date_format("%b"))+
  scale_y_continuous(limits = c(0, 18000), minor_breaks = seq(0,18000, 1000), breaks = seq(0, 18000, 2000))+
  scale_fill_manual(values=c( "white","black"))+
  theme(axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.text.x = element_text(size = 10, angle=0),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 12, face="bold"),
        legend.position = c(0.50, 0.90),
        legend.key.size = unit(0.20, "cm"),
        legend.spacing.x = unit(0.20, 'cm'),
        legend.spacing.y = unit(0.10, 'cm'),
        legend.text = element_text(size = 10),
        legend.title =element_text(size = 10),
        legend.background=element_rect(fill = alpha("white", 0)))+
  guides(fill=guide_legend(ncol=2, title = "Key"), size=T)

pdf("Fig.2B.pdf", width = 4, height = 4, family = "Helvetica")
print(plot2)
dev.off()

#Number of samples sequenced across the different month
###########################################################################################################

sequenced_prop <- read.csv("~/Dropbox/COVID-19/SECONDWAVE/revisedcode/Data/monthly_sequencing_2.csv", header = T, sep=",", stringsAsFactors = T)%>%
  mutate(Monthly=as.Date(Monthly, format="%m/%d/%y"))%>%
  ggplot(aes(x=Monthly, y=Sequenced_prop))+
  geom_col(fill="white", color="black", width = 12)+
  labs(y="Proportion positives sequenced", x="Month in 2020/21")+
  theme_scientific()+
  scale_x_date(breaks ="2 month", date_minor_breaks="1 month", labels = date_format("%b"))+
  scale_y_continuous(limits = c(0, 100), minor_breaks = seq(0,100, 5), breaks = seq(0, 100, 10))+
  theme(axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.text.x = element_text(size = 10, angle=0),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 11, face="bold"),
        legend.position = c(0.75, 0.90),
        legend.key.size = unit(0.25, "cm"),
        legend.spacing.x = unit(0.25, 'cm'),
        legend.spacing.y = unit(0.25, 'cm'),
        legend.text = element_text(size = 10),
        legend.title =element_text(size = 10),
        legend.box.background = element_blank())

pdf("Fig.2C.pdf", width = 4, height = 4, family = "Helvetica")
print(sequenced_prop)
dev.off()


counties <- c("Mombasa", "Kilifi", "Kwale", "Taita Taveta", "Tana River", "Lamu")
no_sequenced <-c(367, 119, 68, 139, 11, 43)
county_sequenced <-data.frame(counties, no_sequenced)
