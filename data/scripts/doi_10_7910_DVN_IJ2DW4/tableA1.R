rm(list = ls())

# set working directory to YOUR PATH TO REPLICATION MATERIALS
setwd('~/PATH TO REPLICATION MATERIALS/gz_replication_materials/')

## load required packages
library(data.table)
library(xtable)

## load data
load('./data/crime/hate_crime.RDS')
load('./data/attitudes/attitudes.rds')
sentiment_3_categories <- readRDS('./data/media/media_3_classification_categories.RDS')

### Table A.1. Summary Statistics
# compute left-leaning party and winner candidate indicator
hate_crimes_w_at_3m[, party_left:=ifelse(party_political_position=='left',1,0)]
hate_crimes_w_at_3m[,winner:=ifelse(victory_margin>0,1,0)]

# vector of candidate and constituency characteristics
baseline <- c('incumbent', 'female', 'party_left',
              'ethnicity_minority',
              'religion_minority',
              'density',
              'young',
              'Single (never married or never registered a same-sex civil partnership)_%' ,
              'deprivation1', 'deprivation2', 'deprivation3', 'deprivation4',
              'grade_ab', 'gradea_c1' ,'grade_c2', 'grade_de',
              'Highest level of qualification: Level 1 qualifications_%' ,
              'Highest level of qualification: Level 2 qualifications_%' ,
              'Highest level of qualification: Level 3 qualifications_%' ,
              'Highest level of qualification: Level 4 qualifications and above_%' ,
              'Economically Inactive_%' , 'Economically active: Full-time student_%' ,
              'Economically active: In employment_%' , 'Economically active: Unemployed_%' ,
              'Living rent free_%' , 'Owned_%' , 'Private rented_%' , 'Social rented_%' ,
              'No people in household have English as a main language (English or Welsh in Wales)_%',
              'At least one but not all people aged 16 and over in household have English as a main language (English or Welsh in Wales)_%',
              'No people aged 16 and over in household but at least one person aged 3 to 15 has English as a main language (English or Welsh in Wales)_%', 
              'Other EU_%' , 'Other countries_%' ,
              'arrived_before1960', 'arrived_1960_1990' , 'arrived_1990_2011',
              'far_right2010')

# labels of vector of candidate and constituency characteristics
baseline_labels <- c('incumbent candidate', 'female candidate', 'left party candidate',
                     '% ethnic minority',
                     '% non-dominant religion',
                     'population density',
                     '% young',
                     '% single' ,
                     '% deprivation level 1', '% deprivation level 2', '% deprivation level 3', '% deprivation level 4',
                     '% social grade ab', '% social grade c1' ,'% social grade c2', '% social grade de',
                     '% level 1 qualifications' ,
                     '% level 2 qualifications' ,
                     '% level 3 qualifications' ,
                     '% level 4+ qualifications' ,
                     '% economically inactive',
                     '% economically active: students' ,
                     '% economically active: employed',
                     '% economically active: unemployed',
                     '% tenure: rent free' , '% tenure: owned' , '% tenure: private rented' , '% tenure: social rented',
                     '% English main language: none',
                     '% English main language: one > 16',
                     '% English main language: one < 16', 
                     '% immigrants: EU' , '% immigrants: non-EU' ,
                     '% immigrant arrival < 1960', '% immigrant arrival 1960-1990' , '% immigrant arrival 1990-2011',
                     '% vote far-right')

# compute summary statistics for hate crime rate 3 months from election, candidate and constituency characteristics
mean <- sapply(hate_crimes_w_at_3m[, c('crime_rate', 'victory_margin', 'winner', baseline), with=F], mean, na.rm=TRUE)
min <- sapply(hate_crimes_w_at_3m[, c('crime_rate', 'victory_margin', 'winner', baseline), with=F], min, na.rm=TRUE)
max <- sapply(hate_crimes_w_at_3m[, c('crime_rate', 'victory_margin', 'winner', baseline), with=F], max, na.rm=TRUE)
sd <- sapply(hate_crimes_w_at_3m[, c('crime_rate', 'victory_margin', 'winner', baseline), with=F], sd, na.rm=TRUE)

stats_table <- data.frame(variable=c('crime rate', 'victory margin', 'winner', baseline_labels),
                          mean = unname(mean),
                          sd = unname(sd),
                          min = unname(min),
                          max = unname(max))

# compute summary statistics for post-election attitudinal outcome
bes_rdd <- bes_rdd[grepl('^(E|W)', ons_id) & white==1]
mean <- sapply(bes_rdd[, 'j05', with=F], mean, na.rm=TRUE)
min <- sapply(bes_rdd[, 'j05', with=F], min, na.rm=TRUE)
max <- sapply(bes_rdd[, 'j05', with=F], max, na.rm=TRUE)
sd <- sapply(bes_rdd[, 'j05', with=F], sd, na.rm=TRUE)

attitudes_table <- data.frame(variable=c('inclusionary attitudes'),
                              mean = unname(mean),
                              sd = unname(sd),
                              min = unname(min),
                              max = unname(max))

# compute summary statistics for media tone about a candidate's ethnic group 3 months from election
sentiment_3_categories <- sentiment_3_categories[(region_name!='Scotland' & !is.na(first_generation) & as.numeric(as.character(period))==3)]
mean <- sapply(sentiment_3_categories[, c('prop_negative', 'prop_neutral', 'prop_positive'), with=F], mean, na.rm=TRUE)
min <- sapply(sentiment_3_categories[, c('prop_negative', 'prop_neutral', 'prop_positive'), with=F], min, na.rm=TRUE)
max <- sapply(sentiment_3_categories[, c('prop_negative', 'prop_neutral', 'prop_positive'), with=F], max, na.rm=TRUE)
sd <- sapply(sentiment_3_categories[, c('prop_negative', 'prop_neutral', 'prop_positive'), with=F], sd, na.rm=TRUE)

media_table <- data.frame(variable=c('% negative mentions', '% neutral mentions', '% positive mentions'),
                          mean = unname(mean),
                          sd = unname(sd),
                          min = unname(min),
                          max = unname(max))

stats_summary <- rbind(media_table,attitudes_table,stats_table)

# save table
print(xtable(stats_summary,
             align = "llcccc",       
             caption = "Summary Statistics",
             label = "table:table_A1",
             digits = c(0,0,2,2,2,2)),
      include.rownames=FALSE,
      caption.placement = "top",
      timestamp = NULL,
      file='./output/tables/tableA1.tex')
