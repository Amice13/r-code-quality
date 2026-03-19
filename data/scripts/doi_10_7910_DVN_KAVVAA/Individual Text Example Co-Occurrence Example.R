library(data.table)
library(cooccur)
library(dplyr)
library(tidyverse)
library(tibble)

#This is a sample script for generating co-occurrence tables for individual texts. It replicates much of the code from
#the corpus-wide co-occurrence analysis.The key difference is that running co-occurrence on smaller data sets requires 
#built in error-handling, as small sets cause problems for cooccur. 
#Selecting the specific family connection for co-occurrence(intra-familial, inter-familial, extra-familial, and general population)
#requires creating both an abundance matrix (all occurrence) and a co-occurrence matrix (present/absent) for families. 
#Each column required for analysis can then be tested for specific conditions in the abundance and co-occurrence matrix.

#Intra-Familial (within a family) Column sum abundance matrix > 1 AND column sum co-occurrence matrix == 1. 
#At least two members of the same family are present, but no other families are.

#Inter-Familial (between families) Column sum co-occurrence matrix >1
#There are at least two families present.

#Extra-familial (only one family member) Column sum abundance matrix == 1
#There is only one member of a family present.

#General Population (no family present) Column sum abundance matrix < 1






# Read File
df.all <- read.csv(file = 'DYSampleData.csv', stringsAsFactors = FALSE)

#This creates two tables populated with -1 for each value. When there are no significant co-occurrences
#the cooccur function throws an error. If the -1 value remains unchanged, it indicates that that particular text has
#no significant co-occurrences.

df.all$PubDate <- as.Date(paste("19",substring(df.all$publication_date, 7,8),
                        "-", substring(df.all$publication_date,4,5),
                        "-", substring(df.all$publication_date,1,2), sep = ""))


df.all <-df.all[order(df.all$PubDate),]

full.summary <- data.frame(unique(df.all$SourceTextTitle))
full.summary$Date <- as.Date("0001-01-01")

full.summary$Positive <- -1
full.summary$Negative <- -1
full.summary$Co_Occurrences <- -1
full.summary$Random <- -1
full.summary$Unclassifiable <- -1
full.summary$NonRandom <- -1
full.summary$Sites <- -1
full.summary$Species <- -1
full.summary.absent <- full.summary

j<-1


#This loops through all of the texts and populates both the tables.

for (i in unique(df.all$SourceTextCode))
{
  
  code <- i
  
  df <- subset(df.all, SourceTextCode == code)
  df.date <- df$PubDate[1]
  
  full.summary[j,2] <- df.date
  
  #Filter out Mentioned Values
  df <- df %>% filter(PresentMentioned != 'Mentioned')
  
  #Set Family Factor to "None" for NA Values
  df$Family[which(df$Family=="")] <- "None"
  df[grepl("Bundren", df$CharacterName), "Family"] <-"Bundren"
  df[grepl("De Spain", df$CharacterName, ignore.case = TRUE), "Family"] <-"De Spain"
 
  #There are very few Asian characters in Faulkner. As they do not register in co-occurrence analysis, they
  #are removed.
  df <- df %>% filter(Race != 'Asian')
  
  
  #Delete any event with any of the groups. 
  #As it is not possible to quantify groups, any events including groups have been removed. Groups appear in roughly 
  #20% of all events are not insubstantial. At the moment, there is no way to account for them yet.
  
  df.deletegroups <- df %>% 
    filter(Gender == 'Multi Gender Group'| 
             Class == 'MultiClass Group'| 
             Race == 'Multiracial Group')%>%
    select("EventKey")
  df <- df %>% anti_join(df.deletegroups, by = 'EventKey')
  
  
   #Consolidate Yeoman and Lower Class
  #There are too few characters in the Yeoman class to register for analysis. These have been moved to "Lower Class"
  
  df$Class[df$Class == 'Yeoman'] <- 'Lower Class'
  
  #Consolidate all Native Americans into one class.
  #The divisions between Native Americans are too granular to register in the analysis.
  #Therefore these have been consolidated.
  
  df$Class[df$Class == 'Indian Chief'] <- 'Tribal Member'
  df$Class[df$Class == 'Indian Tribal Leader'] <- 'Tribal Member'
  df$Class[df$Class == 'Indian Tribal Member'] <- 'Tribal Member'
  df$Class[df$Class=='Enslaved Black'] <- 'Enslaved'
  df$Class[df$Class=='Free Black'] <- 'Free'
  
  df$Race <- as.factor(df$Race)
  df$Class <- as.factor(df$Class)
  df$Gender <-as.factor(df$Gender) 
  df$EventKey <- as.factor(df$EventKey)
  
  
  #Cast Family and Event abundance matrix
  #Get the number of co-occurrences between families at events
  df.family <- df
  df.family <- setDT(df.family)
  #due to the deprecation of the reshape2 dependency in data.table, the data frame has to be manually coerced.
  df.family.abundance <- dcast(df.family, Family~EventKey, length)  
  df.family.abundance <- df.family.abundance %>% remove_rownames %>% column_to_rownames(var="Family")
  
  #Create co-occurrence matrix
  df.family.cooccur <- as.data.frame(with(df.family, table (Family,EventKey))>0L) +0L
  
  
  #Generate Race, Class, Gender co-occurrence matrix
  #Turn characters into species by concatenating them and cast a data frame of race, class, and gender co-occurrence
  #by events.
  
  df.rcg <- df
  df.rcg$RaceClassGender <- NA
  df.rcg$RaceClassGender <- paste(df.rcg$Race, df.rcg$Class, df.rcg$Gender, sep = " ")
  df.rcg.cnt <- as.data.frame(with(df.rcg, table(RaceClassGender, EventKey)) > 0L) +0L
  
  
  #Delete None row
  row.names.remove<- c("None")
  df.family.abundance <- df.family.abundance[!(row.names(df.family.abundance) %in% row.names.remove),]
  df.family.cooccur <- df.family.cooccur[!(row.names(df.family.cooccur) %in% row.names.remove),]
 
  #Cast table with values from race class gender and where there is no inter-familial co-occurrence
  #but there is intra-familial co-occurrence.
  df.family.inter <- as.data.frame (df.rcg.cnt[1:nrow(df.rcg.cnt),colSums(df.family.cooccur) == 1 & colSums(df.family.abundance) > 1])
  
  
  #Generate Co-occurance data variable using cooccur package. This checks to see what elements within in the data set
  #appear at the same event and then calculates the probability of that happening given the other data.
  #These are computationally expensive.
  
  if (ncol(df.family.inter)<2){
    full.summary[j,5] <- "No Family Co-Occurrence"
    full.summary[j,3:10]
    j<- j+1
    next
  }
  try(
    {
      df.rcg.family.present.cooccur <- cooccur(mat = df.family.inter,
                                               type="spp_site",
                                               thresh=TRUE,
                                               spp_names = TRUE,
                                               true_rand_classifier = 0.1, prob = "comb",
                                               site_mask = NULL, only_effects = FALSE,
                                               eff_standard = TRUE, eff_matrix = TRUE)
      
      
      
      cooccur.present <- df.rcg.family.present.cooccur[3:8] 
      cooccur.present$sites <- NA
      cooccur.present$species <- NA
      cooccur.present$sites <- df.rcg.family.present.cooccur$sites[1]
      cooccur.present$species <-df.rcg.family.present.cooccur$species
      full.summary[j,3:10]<-cooccur.present
    }
  )

  j <-j +1
  
}
lgd <- format(Sys.time(), "%Y%m%d%H%M")
version <- paste("Total_Interfamilial Race and Gender",lgd,j,".csv", sep="_")
write.csv(full.summary, version)

