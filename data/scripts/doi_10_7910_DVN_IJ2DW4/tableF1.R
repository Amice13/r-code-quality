rm(list = ls())

# set working directory to YOUR PATH TO REPLICATION MATERIALS
setwd('~/PATH TO REPLICATION MATERIALS/gz_replication_materials/')

## load required packages
library(data.table)
library(xtable)

## load data
candidates <- readRDS('./data/candidates/minority_candidates_2010_2019.RDS')
demographic_const <- readRDS('./data/constituencies/demographic.RDS')

### Table F1. Selection of constituencies into the sample
## define vector of constituency characteristics
baseline <- c('ethnicity_minority',
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

## define labels of vector of constituency characteristics
baseline_labels <- c('share ethnic minority',
                     'share non-dominant religion',
                     'population density',
                     'share young',
                     'share single' ,
                     'share deprivation level 1', 'share deprivation level 2', 'share deprivation level 3', 'share deprivation level 4',
                     'share social grade ab', 'share social gradea c1' ,'share social grade c2', 'share social grade de',
                     'share level 1 qualifications' ,
                     'share level 2 qualifications' ,
                     'share level 3 qualifications' ,
                     'share level 4+ qualifications' ,
                     'share economically inactive',
                     'share economically active: students' ,
                     'share economically active: employed',
                     'share economically active: unemployed',
                     'share tenure: rent free' , 'share tenure: owned' , 'share tenure: private rented' , 'share tenure: social rented',
                     'share English main language: none',
                     'share English main language: one > 16',
                     'share English main language: one < 16', 
                     'share immigrants: EU' , 'share immigrants: non-EU' ,
                     'share immigrant arrival < 1960', 'share immigrant arrival 1960-1990' , 'share immigrant arrival 1990-2011',
                     'share vote far-right 2010')
## compute table
election <- rep(c(2010,2015,2017,2019), each=nrow(demographic_const))

data_stats <- unique(merge(cbind(do.call("rbind", replicate(4, demographic_const, simplify = FALSE)),election), candidates,
                           by.x = c('geogcode','election'), by.y =c('ons_id','election'), all.x = TRUE), by=c('election', 'geogcode'))
data_stats[,in_sample:=ifelse(!is.na(victory_margin),1,0)]

summary_stats <- cbind(baseline_labels, data_stats[, .(lapply(.SD, mean, na.rm=TRUE), lapply(.SD, sd, na.rm=TRUE)), ,
                                                   .SDcols=baseline],
                       data_stats[in_sample==1, .(lapply(.SD, mean, na.rm=TRUE), lapply(.SD, sd, na.rm=TRUE)), ,
                                  .SDcols=baseline])

names(summary_stats) <- c('variable', 'mean', 'sd','mean', 'sd')
summary_stats <- rbind(summary_stats,
                       data.frame(variable ='N constituency-election',
                                  mean=data_stats[,.N], sd=data_stats[,.N], mean=data_stats[in_sample==1,.N],
                                  sd=data_stats[in_sample==1,.N]), use.names=FALSE)

# create footnotes for table
comment <- list(pos = list(0), command = NULL)
comment$pos[[1]] <- c(nrow(summary_stats))
comment$command <- c(paste("\\hline\n",
                           "{\\footnotesize Notes: Shows descriptive statistics for all constituencies, and constituencies in our sample. Our sample is selected by dropping constituencies where ethnic minority candidates do not stand for Parliament. The unit of observation is a constituency-election year.}\n", sep = ""))

## print table
print(xtable(summary_stats,
             align = "llcccc",       
             caption = "Selection of Constituencies Into the Sample",
             label = "table:table_F1",
             digits = c(0,0,3,3,3,3)),
      include.rownames=FALSE,
      caption.placement = "top",
      add.to.row = comment,
      timestamp = NULL,
      file='./output/tables/tableF1.tex')
