### Data Quality and Sampling Bias of Mapping German Fiction in Translation Dataset ###
##Author: Lisa Teichmann
##Date: 19 January 2024
##Comment: This script includes various data quality and summary statistics for the "Mapping German fiction in translation" dataset

#Load packages
install.packages(c("ggplot2", "dplyr", "gridExtra", "stringr", "readxl"))
library(ggplot2)
library(dplyr)
library(gridExtra)
library(stringr)
library(readxl)
library(ggplot2)

#Import master tables extracted from DNB from pre-processing script for all years
alldnb_2021<-read.csv("Data/dnb_transdata_210415/alldnb_2021_2.csv", header=T,row.names=NULL,sep=",")
alldnb_2023<-read.csv("Data/dnb_transdata_220523/alldnb_2023_220523.csv", header=T,row.names=NULL,sep=",")

### 1. Inconsistencies
#Consistency = the percentage of values that match across records (DNB, VIAF, Index Translationum)
# The raw data is restricted and can therefore not be shared here
# see the section "Inconsistencies across databases: the DNB catalogue compared to ONB, VIAF, and IT"

### 2. Sampling bias
#	Precision Assessment
#False positives = what has been labeled a translation and is not
#False negatives = what has not been labeled a translation but is

###assess false positives for titles published in 2020
rs_2020_alldnb <- read.csv("Results/dnb-datashop_ger_2020_precision.csv", header=T,row.names=NULL,sep=";")
sum(rs_alldnb$precision,na.rm=TRUE)

###create a random sample of 100 records from the 2020 dataset
# rs_alldnb <- sample_n(alldnb, 100)
# write.csv(rs_alldnb, file="dnb_transdata_190117/alldnb_random_sample_precision_ch2.0.csv")
# manually annotate editions that are NOT translations by adding the value "1" to a new column called "precision"
rs_alldnb <- read.csv("Results/alldnb_random_sample_precision_ch2.0.csv", header=T,row.names=NULL,sep="\t")
# count 
sum(rs_alldnb$precision,na.rm=TRUE)
# there are only two editions which are not translations in the sample dataset

### 3. Summary statistics: Compare datasets from different extraction dates

## Plot overall distribution of titlesums per year to shows sample bias due to cataloguing practices
# calculate titlesums per year
titlesums_2021 <- as.data.frame(table(alldnb_2021$year))
titlesums_2023 <- as.data.frame(table(alldnb_2023$year))
# save
write.csv(titlesums_2021, "Results/alldnb_2021_titlesums_peryear.csv")
write.csv(titlesums_2023, "Results/alldnb_2023_titlesums_peryear.csv")

## Barplot for title distributions per year (extracted: 2021)
prettytheme <- element_text(face = "italic", color = "#10627a", size = 9)

ggplot(titlesums_2021[titlesums_2021$Freq>1, ], aes(x=Var1, y=Freq)) + 
  geom_bar(stat="identity", fill = "pink") + xlab("Year") + ylab("Total Titles") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + xlab("Total Titles per Year (1980-2020)") + theme(axis.text.x = prettytheme) + ylim(0, 2000)

ggsave("Figures/alldnb_2021_titlesums_peryear.png")

## Barplot for title distributions per year (extracted: 2023)

ggplot(titlesums_2023, aes(x=Var1, y=Freq)) + 
  geom_bar(stat="identity", fill = "pink") + xlab("Year") + ylab("Total Titles") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + xlab("Total Titles per Year (1980-2023)") + theme(axis.text.x = prettytheme) + ylim(0, 2000)

ggsave("Figures/alldnb_2023_titlesums_peryear.png")

###Percentage of translation from all fiction titles in DNB catalogue per year (for 2021 dataset)
totaltitles_2021 <- read.csv(file="Results/alldnb_2021_titles_fiction_nonfiction_percentage.csv", header = T)
totaltitles_2021$perc_title_sum <- (totaltitles_2021$trans_sum / totaltitles_2021$title_sum)*100

#scatterplot of percentages (extracted: 2021)

ggplot(totaltitles_2021, aes((x=year),y=perc_title_sum)) +
  geom_point() + 
  geom_text(label=totaltitles_2021$year)+coord_flip()+ xlab("Year")+ylab("Percentage")+
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5)) + xlab("Percentages of German Fiction Translations per Year in the DNB \nn=755082 titles (fiction only) for data extracted in April 2021") + theme(axis.text.x = prettytheme)+scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+theme_bw()

ggsave("Figures/alldnb_2021_titles_fiction_nonfiction_percentage.png")

###Percentage of translation from all fiction titles in DNB catalogue per year (for 2023 dataset)

totaltitles_2023 <- read.csv(file="Results/alldnb_2023_titles_fiction_nonfiction_percentage.csv", header = T)
totaltitles_2023$perc_title_sum <- (totaltitles_2023$trans_sum / totaltitles_2023$title_sum)*100

#scatterplot of percentages

ggplot(totaltitles_2023, aes((x=year),y=perc_title_sum)) +
  geom_point() + 
  geom_text(label=totaltitles_2023$year)+coord_flip()+ xlab("Year")+ylab("Percentage")+
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5)) + xlab("Percentages of German Fiction Translations per Year in the DNB \nn=1102480 titles (fiction only) for data extracted in May 2023") + theme(axis.text.x = prettytheme)+scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+theme_bw()

ggsave("Figures/alldnb_2023_titles_fiction_nonfiction_percentage.png")

### The following data quality measures are only for the 2021 dataset

### 4. Completeness
#Completeness = #Of NA’s for each variable /total of values

creator_perc <- (sum(alldnb_2021$creator=="")/nrow(alldnb_2021))*100
#2331/35972 = 6.5% missing values of creator
publisher_perc <- (sum(alldnb_2021$publisher=="")/nrow(alldnb_2021))*100
#2331/35972 = 6.8% missing values of publisher
country_perc <- (sum(alldnb_2021$country=="")/nrow(alldnb_2021))*100
#2693/35972 = 7.5% missing values of country
isbn_perc <- (sum(alldnb_2021$ISBN=="")/nrow(alldnb_2021))*100
#2286/35972 missing ISBN
volume_perc <- (sum(alldnb_2021$volume=="")/nrow(alldnb_2021))*100
#33506 missing volume
edition_perc <- (sum(alldnb_2021$edition=="")/nrow(alldnb_2021))*100
#21692 missing edition
dimension_perc <- (sum(alldnb_2021$dimension=="")/nrow(alldnb_2021))*100
#2800 missing dimension (7.7%)
format_perc <- (sum(alldnb_2021$format=="")/nrow(alldnb_2021))*100
#missing format (0.1%)
price_perc <- (sum(alldnb_2021$binding.price=="")/nrow(alldnb_2021))*100
#1315/35972 (3.6%) missing binding price
coltitle_perc <- (sum(alldnb_2021$collective.title=="")/nrow(alldnb_2021))*100
#18091 missing collective title
subhead_perc <- (sum(alldnb_2021$subject.headings=="")/nrow(alldnb_2021))*100
#29580 missing subject headings
unititle_perc <- (sum(alldnb_2021$uniform.title=="")/nrow(alldnb_2021))*100
#8591/35972 or 23% of all titles have a missing original title
lang_perc <- (sum(alldnb_2021$language=="")/nrow(alldnb_2021))*100
year_perc <- (sum(alldnb_2021$year=="")/nrow(alldnb_2021))*100
#no missing values for language and year

### Create new DF with percentages of missing values per variable
na_perc <- data.frame(category = c("creator", "publisher", "country", "ISBN", "volume", "edition", "dimension", "format", "binding price", "collective title", "subject heading", "uniform title"),
                     percentage_nas = c(creator_perc, publisher_perc, country_perc, isbn_perc, volume_perc, edition_perc, dimension_perc,format_perc, price_perc, coltitle_perc, subhead_perc, unititle_perc)
)

write.csv(na_perc, file="Results/alldnb_na_perc_ch2.0.csv")

###Plot percentages of NA
ggplot(na_perc,aes(x= reorder(category,-percentage_nas),percentage_nas))+geom_bar(stat ="identity")+ xlab("Category") + ylab("Percentage of Missing Values")+ labs(title = "Completeness in the DNB translation dataset", subtitle = "Percentages across categories") +coord_flip()+theme_bw()
ggsave("Figures/figure_ch1.0_dnball_completeness_v2.png")

### 5. Variable accuracy
#Accuracy = ratio of data to errors
##author and translator NA's

##author name
#split column to separate author from translator in the field "creator"
authortranslators <- as.data.frame(str_split_fixed(alldnb_2021$creator, ";", 3))

#rename columns
names(authortranslators)[1] <- "author"
names(authortranslators)[2] <- "translator"
names(authortranslators)[3] <- "additional"

##clean "[Verfasser]", [Ubersetzer], [Illustrator], [Gefeierter],  [Herausgeber],  [Verfasser eines Vorworts],  [Mitwirkender],  [Buchgestalter],  [Erzähler],  [Verfasser eines Geleitworts],  [Drucker], Künstler
authortranslators$author <- str_replace_all(authortranslators$author,"Verfasser|Übersetzer|Illustrator|Gefeierter|Herausgeber|Verfasser eines Vorworts|Mitwirkender|Buchgestalter|Erzähler|Verfasser eines Geleitworts|Drucker|Künstler", "")
authortranslators$author <- gsub("\\[|\\]", "", authortranslators$author)
##remove empty spaces
authortranslators$author <- gsub(" ", "", authortranslators$author)

#check for author name ambiguaty
View(table(authortranslators$author))
nrow(table(authortranslators$author))

## count unique author names
length(unique(authortranslators$author))
#finding: returns 4963 unique author names, which are coherent

##Add author, translator columns to original DF
alldnb_2021_authortrans <- cbind(alldnb_2021, authortranslators)
##missing values for author
(sum(alldnb_2021_authortrans$author=="")/nrow(alldnb_2021_authortrans))*100
#6.4% of all titles have missing values for author

##missing values for translator
(sum(alldnb_2021_authortrans$translator=="")/nrow(alldnb_2021_authortrans))*100
#48.9% of all titles have missing values for translator

## save table
write.csv(alldnb_2021_authortrans,"dnb_transdata_210415/alldnb_2021_2_authortrans.csv")

# ###################### Additional ########################################
# 
# #Check for accuracy in titles
# #the column title includes both, the translated and original title and oftentimes just includes the edition (Bd. or a number) NEED TO BE EXCLUDED!
# #the column uniform.title is the original German title
# 
# View(table(alldnb$title))
# View(as.data.frame(unique(alldnb$title)))
# length((unique(alldnb$title)))
# 
# View(table(alldnb$uniform.title))
# View(as.data.frame(unique(alldnb$uniform.title)))
# length((unique(alldnb$title)))
# 
# #publisher
# 
# View(table(alldnb$publisher))
# 
# #see if all publishers have place and if yes how consistent place names are
# 
# publisher_place <- as.data.frame(str_split_fixed(alldnb$publisher, ":", 2))
# names(publisher_place)[1] <- "place"
# names(publisher_place)[2] <- "publisher"
# write.csv(publisher_place, file = "alldnb_publisherplace_ch2.0_v2.csv")
# 
# View(table(publisher_place$place))
# 
# #format
# 
# View(table(alldnb$format))
# 
# #subset to check if both page number and dimensions are included
# 
# format_compl <- as.data.frame(str_split_fixed(alldnb$format, ";", 2))
# View(table(format_compl$V2))
# 
# format_grams <- as.data.frame(str_split_fixed(format_compl$V2, ",", 2))
# View(table(format_grams$V2))
# 
# #binding prize contains binding type and price
# 
# binding_price <- as.data.frame(str_split_fixed(alldnb$binding.price, ":", 2))
# View(table(binding_price$V2))
