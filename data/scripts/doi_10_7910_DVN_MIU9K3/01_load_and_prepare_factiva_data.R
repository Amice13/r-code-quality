#!/usr/bin/Rscript

# Replication code for:
# Bailo, F., & Vromen, A. (2016). 
# Hybrid social and news media protest events: from #MarchinMarch to #BusttheBudget in Australia, 
# Information, Communication & Society, 1–20. https://doi.org/10.1080/1369118X.2016.1252410

# 01_load_and_prepare_factiva_data.R
## Updated: 2016-11-11 15:34:17 AEDT

setwd('~/Documents/replication_packages/ics_2016')

# Prepare Factiva data
library(XML)
library(plyr)
# March Australia
doc <- xmlParse("factiva_marchaus.xml")
df1 <- xmlToDataFrame(doc)
doc <- xmlParse("guardian_marchaus.xml")
df1 <- rbind(df1, xmlToDataFrame(doc))
# Remove duplicates
df1 <- unique(df1)

# Bust the Budget
doc <- xmlTreeParse("factiva_bustbudget.xml", useInternalNodes = TRUE)
df2 <- xmlToDataFrame(doc)
doc <- xmlTreeParse("guardian_bustbudget.xml", useInternalNodes = TRUE)
drop <- c("unkw")
df2 <- rbind(df2[,!(names(df2) %in% drop)], xmlToDataFrame(doc))

recodeMediaGroup <- function (str) {
  if (grepl("News Limited", str)) {return("News Corp")}
  if (grepl("Fairfax", str)) {return("Fairfax")}
  if (grepl("APN Newspapers", str)) {return("APN Newspapers")}
  if (grepl("Canberra Times ", str)) {return("Canberra Times")}
  if (grepl("Associated Newspapers ", str)) {return("Associated Newspapers")}
  if (grepl("McPherson ", str)) {return("McPherson")}
  if (grepl("Guardian ", str)) {return("Guardian")}
  else {return("Other")}
}

df1_publication <- data.frame(table(df1$publication))
df1_publication <- df1_publication[with(df1_publication, order(-Freq)), ]
colnames(df1_publication) <- c("Publication", "Frequency")
df1_mediagroup <-  data.frame(prop.table(table(sapply(df1$copyright, recodeMediaGroup))))
df1_mediagroup <-  df1_mediagroup[with(df1_mediagroup, order(-Freq)), ]
colnames(df1_mediagroup) <- c("Group", "Frequency")

df2_publication <- data.frame(table(df2$publication))
df2_publication <- df2_publication[with(df2_publication, order(-Freq)), ]
colnames(df2_publication) <- c("Publication", "Frequency")
df2_mediagroup <-  data.frame(prop.table(table(sapply(df2$copyright, recodeMediaGroup))))
df2_mediagroup <-  df2_mediagroup[with(df2_mediagroup, order(-Freq)), ]
colnames(df2_mediagroup) <- c("Group", "Frequency (%)")

df1$search <- factor("March Australia", levels=c("March Australia","Bust the Budget"))
df2$search <- factor("Bust the Budget", levels=c("March Australia","Bust the Budget"))
binded <- rbind(df1[,intersect(names(df1),names(df2))],df2[intersect(names(df1),names(df2))])
cols_to_keep <- c("date", "title", "author", "publication", "publication_id",
                  "page", "words", "copyright", "language", 
                  "factiva_id", "search")

newspaper_articles_df <- rbind(df1[,cols_to_keep], df2[,cols_to_keep])
save(newspaper_articles_df, file='newspaper_articles_df.RData')
