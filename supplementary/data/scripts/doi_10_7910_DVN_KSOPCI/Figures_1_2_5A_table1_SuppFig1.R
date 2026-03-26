


setwd("~/Documents/MSc_project/hrvSocialScales/codebook/")
rm(list=ls())

#required packages
require("pacman")
pacman::p_load(data.table,tidyverse, RColorBrewer, grid, magrittr, hrbrthemes, ggridges, beeswarm, 
               ggbeeswarm, lubridate,viridis, ggridges, patchwork, reshape2) 

# Figure 1 ####
# Note
### Figures 1A-C are created using ArcGIS 

### Figure 1 D ####

# Plot to show individual studies time span 
## create dataset
study_period <- data.table(c("School", "KHDSS", "Households", "Countrywide", "KCH"), 
                           c("2017-05-01", "2015-12-01", "2009-12-01", "2014-01-01", "2002-01-01",
                             "2018-04-06", "2016-11-30", "2010-05-01", "2014-12-01", "2018-12-01"))


study_period$V2 <- as.Date(study_period$V2)
time_order <- c("KCH", "School", "KHDSS", "Countrywide", "Households")


fig_1D <- ggplot(data=study_period) +
  geom_point(aes(y=factor(V1, levels = time_order), x=V2), shape = 15, size =2)+
  geom_line(aes(y=factor(V1, levels = time_order), x=V2)) +
  theme_bw()+
  theme(axis.title.y = element_blank(),
        axis.text = element_text(size = 12))+
  labs(x = "Year")

fig_1D


# Figure 2 #######

# Importing data 
mydata <- read.csv("./data/anonym_metadata.csv", fileEncoding="UTF-8-BOM", header = TRUE, sep = ",") %>%
  dplyr::select(pseudo_id, date_collect, type, species, scale, study, site, site2)

# date
mydata$date_collect <- as.Date(mydata$date_collect, "%d/%m/%Y")
mydata <-  filter(mydata, !between(date_collect, as.Date("2017-02-01"), as.Date("2017-05-01"))) #drop school pilot study samples (not included in primary manuscript)
mydata$days<-as.Date(cut(mydata$date_collect,breaks = "day"))
mydata$date2<-as.Date(cut(mydata$date_collect,breaks = "2 weeks",start.on.monday = FALSE))
mydata$date3<-as.Date(cut(mydata$date_collect,breaks = "1 month",start.on.monday = FALSE))

# tidy up
  # use county names in place of hospital name for countrywide study
  # rectify Dadaab spelling
mydata <- mydata %>% 
  mutate(
    site = case_when(
      scale == "country" & site == "KNH" ~ "Nairobi",
      scale == "country" & site == "Kibera" ~ "Nairobi",
      scale == "country" & site == "Daadab" ~ "Dadaab", # correct spelling
      scale == "country" & site == "KCH" ~ "Kilifi",
      TRUE ~ site),
    study = case_when(
      (scale == "country" & study == "KCH") ~ "country",
      TRUE ~ study)) %>%
  mutate(across(c(study, scale)                                 
                ,recode,
                "hh" = "HH",
                "school" = "School",
                "country" = "Country",
                "hdss" = "KHDSS"))




# Figure 02.A-  barplot of genotype frequencies (all data)
neworder = c("HH","Country", "KHDSS" , "School")  #reorder based on increasing scale
mydata <- arrange(mutate(mydata,scale=factor(scale,levels=neworder)),scale)

# Note: scale vs study.
  # study is respective study for a given sample
  # scale implies the 4 'main' studies in this analysis. It doesnt have KCH as an option  
      #e.g KCH samples during the household study read 'hh' at scale level

## Figure 2 
### Figure 2A #######
fig_2A <- mydata  %>%
ggplot(.) +
  geom_bar(stat = "count", aes(x = type, fill=study)) +
  coord_flip() + scale_y_continuous(name="Count") +
  scale_x_discrete(name="RV type") +
  scale_fill_manual(values=c("KCH"="#E69F00"),name = "Key", limits = "KCH", na.value = "cyan4")+
  # scale_fill_manual(values=c("Country"="cyan4", "HH"="cyan4","KCH"="#E69F00",
  #                            'KHDSS'="cyan4", "School"="cyan4"),name = "Key", limits = "KCH")+
  theme_bw()+
  theme(strip.background = element_blank(),
        axis.text.x = element_text(color="black", size=14),
        axis.text.y = element_text(color="black", size=10),
        axis.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.position = c(0.94, 0.97))+
  facet_wrap(~scale, ncol = 4)


# Data summary 
table(mydata$scale)
#selection='hdss'    # iterate selection through c('hh', 'hdss', 'country', 'school')

## filter data based on scale 
  # Remember: study vs scale. e.g KCH during HH study will read HH at scale but KCH at study. 
filter_per_social_scale <- function(selection){
  not_merged_with_KCH <-mydata[mydata$scale==selection,] %>% filter(! site == "KCH")
  merged_with_KCH <- mydata[mydata$scale==selection,]
  out <- list(not_merged_with_KCH,  merged_with_KCH)
  return(out)
}


## Jaccard Similarity Index
jaccard <- function(selection) {
  # filter data including contemporaneous kch data
  merged_with_KCH <- filter_per_social_scale(selection)[[2]]
  # get sets (unique members of a list)
  a <- unique(merged_with_KCH[!grepl("^KCH", merged_with_KCH$study),]$type)
  b <- unique(merged_with_KCH[grepl("^KCH", merged_with_KCH$study),]$type)
  # find intersect
  intersection = length(intersect(a, b))
  # then union
  union = length(a) + length(b) - intersection
  # calculate index
  jaccard_index = intersection/union
  # output
  print(paste('The Jaccard similarity index between KCH and', selection, 'is:', jaccard_index))
}

jaccard("HH")
jaccard("KHDSS")
jaccard("School")

## Frequent types (top 10 types) 
ten_most_frequent_types <- function(selection) {
  # filter data excluding contemporaneous kch data
  not_merged_with_KCH <- filter_per_social_scale(selection)[[1]]
  merged_with_KCH <- filter_per_social_scale(selection)[[2]]
  # count frequency of each type
  typeCount<-as.data.frame(table(not_merged_with_KCH[['type']]))
  typeCount<-typeCount[order(typeCount[['Freq']]),] 
  typeCount<-typeCount[!grepl("unassigned2", typeCount$Var1),] # drop unknown types
  
  # top 10
  typeCount10<-tail(typeCount,n=10)
  typeCount10<-typeCount10 %>% dplyr::rename(type = Var1)
  typeCount10[['topTypes']] <- typeCount10[['type']]
  
  # merge with bigger dataset
  freqTypes <- merge(merged_with_KCH, typeCount10, by="type", all.x = TRUE)
  freqTypesOnly<- freqTypes %>% filter(!is.na(topTypes)) 
  freqTypes[['topTypes']]<-as.character(freqTypes[['topTypes']])
  freqTypes[['topTypes']][is.na(freqTypes$topTypes)] <- "Other"
  return(freqTypes)
}


### Figure 2B-E #######
## Plot
plot_2B_E <- function(selection){
  #colors
  color_list = c("KCH" = "#E69F00")
  color_list[[selection]] = 'cyan4'
  #plot
  output_plot = ten_most_frequent_types(selection) %>%
    ggplot(., aes_string(x = 'days', y= 'topTypes', fill = 'study')) +
      geom_density_ridges2(bandwidth=10,alpha = 0.5, scale = 2)+
      scale_x_date(date_labels = "%b-%Y") + #+ #limits = as.Date(c('2019-12-01','2021-06-01')
      theme_bw()+
      theme(axis.text = element_text(size=12),
            axis.title = element_text(size = 13),
            legend.position = "top")+
      ylab("RV type") + xlab("Date") +
      scale_fill_manual(values= color_list,name = "Key") +
      #the code line below is only for the school setting - to show breaks in sampling during the holidays
      geom_vline(xintercept = as.Date(c("2017-08-01", "2017-08-30", "2017-10-30", "2018-01-08")), color="gray40", linetype="dashed")+
      ggtitle(selection) 
  return(output_plot)
}


fig_2B <- plot_2B_E("HH") +
  ggtitle("Households")
fig_2C <- plot_2B_E("School")
fig_2D <- plot_2B_E("KHDSS")
fig_2E <- plot_2B_E("Country") +
  theme(legend.position = "none")

# combine
fig_B_E <- fig_2B/fig_2C/fig_2D/fig_2E
fig2<- fig_2A + fig_B_E + plot_layout(ncol = 2, widths = c(2, 1))
fig2


#Supplementary 1 -  epidemic curves resolved to location/county level#####
# Supplementary Fig 1A and B

#order locations based on proximity to each other
hdss_order = c("Junju", "Pingilikani", "Chasimba", "Jaribuni", "Mavueni",
               "KCH", "Mtondia", "Sokoke", "Ngerenya", "Matsangoni" ) 
#order counties based on proximity to each other
country_order = c("Siaya", "Kakamega", "Kisumu","Kakuma", "Nakuru", "Nairobi",
                  "Nyeri", "Dadaab","Kilifi", "Mombasa") 

# function
resolved_epidemic_curves <- function(selection, proximity_order){
  # frequent types
  freqTypes <- ten_most_frequent_types(selection)
  freqTypes <- arrange(mutate(freqTypes,site=factor(site, levels=proximity_order)),site)
  output_plot <- freqTypes %>%
    ggplot(., aes(x = date3, y=site, fill= site)) +
    geom_density_ridges2(bandwidth=10,alpha = 0.5, scale = 2)+
    scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y") +
    theme_bw()+ylab("Density")+ xlab(NULL)+ 
    theme(axis.text = element_text(size=8),
          axis.title = element_text(size = 12),
          legend.position = 'none',
          strip.background = element_rect(colour=NA, fill=NA))+
    guides(fill=guide_legend(nrow = 1))+
    facet_wrap(vars(topTypes))+
    scale_fill_viridis(discrete = TRUE, option = "D",name = "Site")+
    ggtitle(selection)
  return(output_plot)
}

# plot 
plot_a <- resolved_epidemic_curves(selection ='KHDSS', proximity_order=hdss_order)

plot_b <- resolved_epidemic_curves(selection ='Country', proximity_order=country_order)

#merge
library(patchwork)
supp_1 <- plot_a/plot_b
supp_1 + plot_annotation(tag_levels = 'A')


# Figure 5A #######

dis<-read.csv("./data/overall_genetic_distances_per_type.csv")
head(dis)

dis %>% 
  group_by(scale) %>% 
  dplyr::summarise(n= max(distance)*100)

neworder2 = c("Households",  "School", "KHDSS","Country")  #reorder based on increasing scale
dis <- arrange(mutate(dis,scale=factor(scale,levels=neworder2)),scale)

#plot
boxplot(distance ~ scale, data = dis,
        outline = FALSE,
        xlab = "", ylab = "")
abline(h=seq(0,0.08,0.01), lty=3, col="gray")
mtext(side=2, line=2.2, "Overall mean distance", col="black", font=1, cex=1.2)
beeswarm(distance ~ scale, data = dis,
         pch = 16, add=T)

# Table 1 #######
 # extract frequent types (top 10) per study
frequent_types_in_hh<- data.frame(table(mydata$type, mydata$study)) %>% 
  spread(., key = Var2, value = Freq) %>% 
  slice_max(HH, n=10)

frequent_types_in_school<- data.frame(table(mydata$type, mydata$study)) %>% 
  spread(., key = Var2, value = Freq) %>% 
  slice_max(School, n=10)

frequent_types_in_hdss<- data.frame(table(mydata$type, mydata$study)) %>% 
  spread(., key = Var2, value = Freq) %>% 
  slice_max(KHDSS, n=10)

frequent_types_in_country<- data.frame(table(mydata$type, mydata$study)) %>% 
  spread(., key = Var2, value = Freq) %>% 
  slice_max(Country, n=10)

freq_types_all <- rbind(frequent_types_in_hh, frequent_types_in_country, frequent_types_in_hdss,
                        frequent_types_in_school)
# total sequences per scale
df2 <- data.frame(table(mydata$study))

#percentages per study
table_1 <- freq_types_all %>%
  mutate(.,
         country_percent = round(Country*100/803, digits=2),
         hdss_percent = round(KHDSS*100/613, digits=2),
         hh_percent = round(HH*100/481, digits=2),
         school_percent = round(School*100/256, digits=2)
         )


  
  