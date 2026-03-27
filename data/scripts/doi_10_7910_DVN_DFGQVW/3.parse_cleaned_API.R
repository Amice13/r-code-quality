rm(list=ls())

# Install packages if needed
# install.packages(c('readxl', 'lubridate', 'dplyr', 'haven', 'stringr'))
library(readxl); library(lubridate); 
library(dplyr); library(haven); library(stringr)

Sys.setenv(TZ = "UTC")

# Easy conversion for dates between Japanese era and common era
date.convert <- suppressWarnings(read_excel('metadata_for_analysis//Gengo-Seireki.xlsx'))
date.convert$gengo <- gsub(date.convert$gengo, pattern='年', replacement = '')
date.convert$seireki <- gsub(date.convert$seireki, pattern='年', replacement = '')

unicode <- cbind(sort(unique(unlist(str_split(date.convert$seireki, pattern='')))), c(0:9))

date.convert[,]<- apply(date.convert, MARGIN = 2, FUN=function(j){sapply(str_split(j, pattern=''),
  FUN=function(i){paste0(ifelse(i %in% unicode[,1], unicode[,2][match(i, unicode[,1])],i), collapse='')}
)})
date.convert$seireki <- as.numeric(date.convert$seireki)
stopifnot(all(!is.na(date.convert$seireki)))


cabinet.data <- suppressWarnings(read_excel('metadata_for_analysis/JAPAN-MP-CABINETS-1947-2017.xlsx'))
cabinet.data$cab_date <- str_replace(cabinet.data$cab_date, pattern='昭(?=[0-9])', replacement='昭和')
fmt.date <- do.call('rbind', str_split(cabinet.data$cab_date, pattern='年|月|日'))
fmt.date[,1] <- date.convert$seireki[match(fmt.date[,1], date.convert$gengo)]

cabinet.data$fmt_date <- ymd(apply(fmt.date[,1:3], MARGIN = 1, FUN=function(j){paste(j, collapse='-')}))

# Checksums on accurate date conversions
stopifnot(all(fmt.date[,4] == ""))
stopifnot(all(!is.na(fmt.date)))
stopifnot(all(!is.na(cabinet.data$fmt_date)))

unique.cabinets <- unique(cabinet.data[c('fmt_date', 'cab_name')])
unique.cabinets <- arrange(unique.cabinets, fmt_date)

unique.cabinets <- rbind(unique.cabinets, data.frame(
  fmt_date = ymd(c('2016-08-03', '2017-08-03', '2017-11-1', '2018-10-02', '2019-9-11')),
  cab_name = c('Abe 3.2', 'Abe 3.3', 'Abe 4', 'Abe 4.1', 'Abe 4.2')
))

unique.cabinets$start_date <- unique.cabinets$fmt_date
unique.cabinets$start_date[1] <- ymd('1947-05-20')
unique.cabinets$end_date <- c(unique.cabinets$fmt_date[-1]-days(1), as.Date('2020-09-16'))
unique.cabinets$interval <- interval(unique.cabinets$start_date, unique.cabinets$end_date)

end_date <- max(unique.cabinets$end_date)

chamber_list <- list('syugiin' = "衆議院",
     'sangiin' = "参議院",
     'bicameral' = "両院")

write <- lapply(unique.cabinets$cab_name, FUN=function(x){0})
names(write) <- unique.cabinets$cab_name

write <- lapply(names(chamber_list), FUN=function(i){write})
names(write) <- names(chamber_list)

count.checksum <- count.summary <- data.frame()

for (d in 1:203){
  
  print(d)
  
  data_d <- suppressWarnings(read_excel(paste0('api_parsed_clean/data_session', d, '.xlsx')))

  if (!(all(data_d$chamber %in% unlist(chamber_list)))){
    stop('.')
  }
  
  # Remove the meetings without "X" that are not actual meetings.
  stopifnot(all(is.na(data_d %>% filter(!grepl(issueID, pattern='X')) %>% pull(speech))))
  data_d <- data_d %>% filter(grepl(issueID, pattern='X'))
  
  #Parse the date
  data_d$fmt_date <- ymd(data_d$date)
  if (sum(is.na(data_d$date)) > 0){stop("Missing dates")}
  
  if (any(as.numeric(data_d$fmt_date - end_date) > 0)){
    warning(paste0('Pruning after ', end_date)) 
    data_d <- data_d %>% filter(fmt_date - end_date < 0)
    if (nrow(data_d) == 0){
      warning(paste0('Skipping ', d))
    }
    next
  }
  # Take all of the unique calendar dates, 
  # Find the cabinet with which they match.
  ses.dates <- unique(data_d$fmt_date)
  ses.cabinets <- sapply(ses.dates, FUN=function(x){
    o <- unique.cabinets$cab_name[x %within% unique.cabinets$interval]; 
    if (length(o) != 1){stop('DATE ERROR')}; 
    return(o)
  })
  print('Cabinets Found')
  print(unique(ses.cabinets))
  # Add the cabinet identifier to each observation
  if (length(unique(ses.cabinets)) == 1){
    data_d$cabinet <- ses.cabinets[1]    
  }else{
    data_d$cabinet <- ses.cabinets[sapply(data_d$fmt_date, FUN=function(x){which(ses.dates == x)})]
  }
  #Add information about the number of characters, length, and sentences
  data_d$length_characters <- nchar(gsub(data_d$speech, pattern='\\p{P}|\\p{Z}|@', replacement = '', perl = TRUE))
  data_d$length_sentences <- str_count(data_d$speech, pattern='。')
  data_d$length_lines <- data_d$last_line - data_d$first_line + 1
  # Save out the data, loop over the cabinets
  for (cab in unique(ses.cabinets)){
    cab_d <- data_d %>% filter(cabinet == cab) #Take the data in the Diet Session#
    for (chamber in unique(cab_d$chamber)){#Loop over each chamber 
      sub.cab <- cab_d[which(cab_d$chamber == chamber),]
      ascii_chamber <- names(which(unlist(chamber_list) == chamber))
      fmt.cab <- paste('parsed_api_cabinet/', ascii_chamber, '_' ,
                       tolower(gsub(make.names(cab), pattern='\\.', replacement = '_')), '.RDS', sep='')
      # If the file/chamber already exists, then read the existing RDS file
      # and append. Otherwise, turn 'write' to equal one for the future.
      if (write[[ascii_chamber]][[cab]] == 1){
        all.data <- readRDS(fmt.cab)
        all.data <- rbind(all.data, sub.cab)
      }else{
        all.data <- sub.cab; 
        write[[ascii_chamber]][[cab]] <- 1
      }
      saveRDS(all.data, file = fmt.cab, compress = T)
      rm(all.data)
      
    }
  }

  checksum_number <- data_d %>%
    group_by(chamber, cabinet) %>% tally()
  
  count.checksum <- bind_rows(count.checksum, checksum_number %>% mutate(session = d))
  speaking.volume <- data_d %>% group_by(full_name, cabinet, chamber) %>% 
    summarise(n = n(), suffixes = paste(unique(suffix), collapse='~'), 
              characters = sum(length_characters), lines = sum(length_lines), .groups = 'drop') %>%
    mutate(session = d)
  count.summary <- bind_rows(count.summary, speaking.volume)
  
}


agg.checksum <- count.checksum %>% as_tibble %>%
  group_by(chamber, cabinet) %>% tally(wt = n)

for (v in 1:nrow(agg.checksum)){
  print(v)
  chamber_v <- names(which(chamber_list == agg.checksum$chamber[v]))
  cabinet_v <- tolower(gsub(make.names(agg.checksum$cabinet[v]), pattern='\\.', replacement = '_'))
  data_v <- readRDS(paste0('parsed_api_cabinet/', chamber_v, '_', cabinet_v, '.RDS'))
  stopifnot(agg.checksum$n[v] == nrow(data_v))
}

print('DONE LOOPING')  
count.summary <- count.summary %>% group_by(full_name, chamber, cabinet) %>% 
  summarise(n = sum(n), characters = sum(characters), 
    lines = sum(lines), 
    suffixes = paste(unique(suffixes), collapse='~'))

count.summary$suffixes <- sapply(str_split(count.summary$suffixes, pattern = '~'), FUN=function(i){paste(unique(i), collapse = '~')})

saveRDS(count.summary, file = paste('diagnostic_data/aggregate_committee_count.rds'))


