rm(list = ls())

# set working directory to YOUR PATH TO REPLICATION MATERIALS
setwd('~/PATH TO REPLICATION MATERIALS/gz_replication_materials/')

## load required packages
library(data.table)
library(xtable)

## load data
load('./data/media/articles_word_frequency.rds')

### Table D1. Top 160 words in subset of in-sample articles
# compute table with word frequency
freq_table <- words[,.(frequency=sum(value)),by=(word)]
freq_table <- freq_table[grepl("\\d", word)==FALSE]
freq_table <- freq_table[order(-frequency)]
freq_table[,row:=1:.N]

tfreq <- data.table(cbind(freq_table[(row %in% c(1:40))],
                          freq_table[(row %in% c(41:80))],
                          freq_table[(row %in% c(81:120))],
                          freq_table[(row %in% c(121:160))]))

# print table
print(xtable(tfreq[, c(1,2,4,5,7,8,10,11)],
                     digits = rep(0,9),
                     caption = "Top 160 words in relevant articles subset",
                     label = "table:table_D1",),
      include.rownames=FALSE,
      caption.placement = "top",
      file='./output/tables/tableD1.tex')
