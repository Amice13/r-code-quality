#setting my working directory
setwd("C:/Users/Rocco/Desktop/My Files/Luce/merced_assessment/Data")

#loading required packages for manipulation and graphing
library(dplyr)
library(ggplot2)

#read in complete historical assessment data
data <- read.csv(file = "assessment_data.csv")
data <- as.data.frame(data)
data[is.na(data)] = 0
data

#Split data by year
split <- split(data, data$year)

#Assign each split to a new object by year
data_1920 <- split$`1920`
data_1925 <- split$`1925`
data_1930 <- split$`1930`


#Aggregate entries by block, remove blocks higher than 160 and write new csv
agg_1920 <- aggregate(list(val_soldier=data_1920$ex_soldier, 
                      val_lot=data_1920$val_lot, 
                      val_improve=data_1920$val_imp, 
                      val_total=data_1920$val_total), 
                 by = list(city_block=data_1920$plat_block), sum)
agg_1920$year <- 1920
agg_1920<-agg_1920[!(agg_1920$city_block > 160),]

write.csv(agg_1920, file = "output_1920.csv")

#Aggregate entries by block, remove blocks higher than 160 and write new csv
agg_1925 <- aggregate(list(val_soldier=data_1925$ex_soldier, 
                           val_lot=data_1925$val_lot, 
                           val_improve=data_1925$val_imp, 
                           val_total=data_1925$val_total), 
                      by = list(city_block=data_1925$plat_block), sum)
agg_1925$year <- 1925
agg_1925<-agg_1925[!(agg_1925$city_block > 160),]

write.csv(agg_1925, file = "output_1925.csv")

#Aggregate entries by block, remove blocks higher than 160 and write new csv
agg_1930 <- aggregate(list(val_soldier=data_1930$ex_soldier, 
                           val_lot=data_1930$val_lot, 
                           val_improve=data_1930$val_imp, 
                           val_total=data_1930$val_total), 
                      by = list(city_block=data_1930$plat_block), sum)
agg_1930$year <- 1930
agg_1930<-agg_1930[!(agg_1930$city_block > 160),]

write.csv(agg_1930, file = "output_1930.csv")

#remerge aggregated data and add new id column for export
data_remerge <- rbind(agg_1920,agg_1925,agg_1930)
data_remerge$id <- 1:nrow(data_remerge) 

data_remerge <- data_remerge %>%
  select("id", everything())

write.csv(data_remerge, file = "output_merge.csv")

#Read in historic building survey data and investigate data
arch <- read.csv(file = "historic_home_stock.csv")
summary(arch)
head(arch)
hist(arch$year)
#Plot histogram for year built
ggplot(arch)+
  geom_histogram(aes(arch$year))

#Plot number of styles 
ggplot(arch)+
  geom_bar(aes(arch$style_desc))+
  theme(axis.text.x = element_text(angle=45, hjust = 1))

#Count per style
arch_count <- count(arch, style_desc)

#Plot bar plot for style count but in order
ggplot(arch_count, aes(x = reorder(style_desc, -n), y = n)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  ylab("Architecture Style") +
  xlab("Count") +
  ggtitle("Relative Number of Styles for All of Downtown")

#barplot(arch$style)
#rolls <- read.csv(file = "sfr_all.csv")
#rolls <- rolls[!is.na(rolls$year),]
#hist(rolls$year)

#Read in styles extracted for John Muir Neighborhood
johnmuir_arch <- read.csv(file = "john_muir_styles.csv")
unique(johnmuir_arch$style_desc)

#Count styles for John Muir and plot
johnmuir_arch <- count(johnmuir_arch, style_desc)
ggplot(johnmuir_arch, aes(x = reorder(style_desc, -n), y = n)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  ylab("Architecture Style") +
  xlab("Count") +
  ggtitle("Relative Number of Styles for Muir Woods")

#Read in modern data for (SFR) single-family residences
sfr <- read.csv(file = "sfr_all.csv")
#Check out year historgram from this data (ended up not using this)
hist(sfr$year)

#population statistics
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)

#read in population information
pop <- read.csv(file = "merced_population.csv")
early_pop <- subset(pop, year <= 1950) 

#Plot line graph and text
early_pop %>% 
  ggplot(aes(x=year, y=city_pop)) +
  geom_line(color="#69b3a2", size = 1) +
  ylim(0,20000) +
  geom_hline(yintercept=0, color="orange", size=.5) +
  xlim(1870,1950)+
  xlab("Year")+
  ylab("Population")+
  ggtitle("Population Growth In Merced's Early Years")+
  annotate(geom="text", x=1910, y=7000, 
           label="1920s sees a spike\n in population growth")+
  annotate(geom="point", x=1920, y=3974, size=10, shape=21, fill="transparent") +
  theme_ipsum()


