rm(list=ls())

# Install if necessary
# install.packages(c('foreign', 'readxl', 'lubridate', 'reshape2',
#                    'dplyr', 'stringr', 'purrr', 'ggplot2', 'glue',
#                    'readr', 'plyr'))
library(foreign)
library(readxl); library(lubridate); library(reshape2)
library(dplyr); library(stringr)
library(purrr)
library(ggplot2)
library(glue); library(readr)
# Note that "plyr" is required but is called via :: vs loading
# to avoid issues with dplyr

options(dplyr.summarise.inform = FALSE)

Sys.setenv(TZ = "UTC")
###Load the list of HOC members.
hoc.list <- data.frame(read_excel('metadata_for_analysis/HOC_members.xlsx', col_types = 'text'), stringsAsFactors = F)
hoc.names <- gsub(hoc.list$name, pattern='\u3000', replacement = '')
hoc.party <- gsub(hoc.list$party, pattern='', replacement = '')
hoc.party.id <- as.numeric(hoc.list$party_id)

hoc.data <- data.frame(name = hoc.names, party = hoc.party, party_id = hoc.party.id, stringsAsFactors = F)
extra.hoc <- which(hoc.list$alternatename != '')
extra.hoc <- data.frame(party = hoc.list$party[extra.hoc],  
                        party_id =as.numeric(hoc.list$`party_id`)[extra.hoc],
                        name = gsub(hoc.list$alternatename[extra.hoc], pattern='\u3000', replacement = ''), 
                        stringsAsFactors = FALSE)
hoc.data <- rbind(hoc.data, extra.hoc)


#Integrate DS's manual corrections.
manual.corrections <- read_excel('metadata_for_analysis/Spelling_Issues_Oct_2018.xlsx')
manual.corrections <- subset(manual.corrections, !is.na(correction))

extra.hoc <- subset(manual.corrections, grepl(notes, pattern='HoC'))
extra.hoc <- unique(extra.hoc[,c('full_name', 'correction', 'notes')])
extra.hoc <- data.frame(name = extra.hoc$full_name, hoc.data[match(extra.hoc$correction, hoc.data$name),])
extra.hoc <- extra.hoc[names(extra.hoc) != 'name.1']
hoc.data <- rbind(hoc.data, extra.hoc)

hor.alternative.names <- subset(manual.corrections, !grepl(notes, pattern='HoC'))

# A reformatted version of Reed-Smith that we use
# to get party, ministerial status
cabinet.data <- suppressWarnings(read_excel('metadata_for_analysis/JAPAN-MP-CABINETS-1947-2017.xlsx'))
cabinet.data$cab_date <- str_replace(cabinet.data$cab_date, pattern='昭(?=[0-9])', replacement='昭和')

# Easy conversion for dates between Japanese era and common era
date.convert <- read_excel('metadata_for_analysis/Gengo-Seireki.xlsx')
date.convert$gengo <- gsub(date.convert$gengo, pattern='年', replacement = '')
date.convert$seireki <- gsub(date.convert$seireki, pattern='年', replacement = '')

unicode <- cbind(sort(unique(unlist(str_split(date.convert$seireki, pattern='')))), c(0:9))

date.convert[,]<- apply(date.convert, MARGIN = 2, FUN=function(j){sapply(str_split(j, pattern=''),
    FUN=function(i){paste0(ifelse(i %in% unicode[,1], unicode[,2][match(i, unicode[,1])],i), collapse='')}
)})
date.convert$seireki <- as.numeric(date.convert$seireki)
stopifnot(all(!is.na(date.convert$seireki)))

fmt.date <- do.call('rbind', str_split(cabinet.data$cab_date, pattern='年|月|日'))
fmt.date[,1] <- date.convert$seireki[match(fmt.date[,1], date.convert$gengo)]
cabinet.data$fmt_date <- ymd(apply(fmt.date[,1:3], MARGIN = 1, FUN=function(j){paste(j, collapse='-')}))

# Checksums on accurate date conversions
stopifnot(all(fmt.date[,4] == ""))
stopifnot(all(!is.na(fmt.date)))
stopifnot(all(!is.na(cabinet.data$fmt_date)))

cabinet.data$fmt_name <- gsub(cabinet.data$cab_name, pattern='\\.', replacement = ' ')
cabinet.data$simple.name <- gsub(gsub(tolower(cabinet.data$cab_name), pattern='\\.| ', replacement='_'), pattern='_', replacement = ' ')
#Get a list of the dates that the cabinets span.
unique.cabinets <- unique(cabinet.data[c('fmt_date', 'cab_name', 'cab_code')])
unique.cabinets <- arrange(unique.cabinets, fmt_date)
unique.cabinets$start_date <- unique.cabinets$fmt_date
unique.cabinets$start_date[1] <- ymd('1947-05-20')
unique.cabinets$end_date <- c(unique.cabinets$fmt_date[-1]-days(1), as.Date('2016-12-31'))
unique.cabinets$interval <- interval(unique.cabinets$start_date, unique.cabinets$end_date)

print(head(unique.cabinets))

# Load our dictionary of mapping suffixes to top level categories
# This will allow us to deal with many rare cases. 
# See the Appendix of the paper for the most common ones.
suffix.dictionary <- read_excel('metadata_for_analysis/Diet_Suffixes.xlsx', sheet = 'suffixes_for_MPs')
suffix.dictionary$role <- suffix.dictionary$type

override.suffix <- read_excel('metadata_for_analysis/Diet_Suffixes.xlsx', sheet = 'override_suffix')
override.suffix <- override.suffix$suffix

governing.parties <- cabinet.data %>% group_by(cab_name,  simple.name, party_jp, party_id) %>% summarize(cabappt = max(cabappt)) %>% filter(cabappt > 0)

standing.hor <- read_excel('metadata_for_analysis/Standing_committees.xlsx', sheet = 'House of Representatives')
standing.hor.periods <- ymd(sapply(str_split(names(standing.hor)[-1], pattern='[年月日]'), FUN=function(i){
  paste(i[-4], collapse='-')
}))
standing.hor <- apply(standing.hor[-1:-2,-1], MARGIN = 2, FUN=function(i){data.frame(committee.name = i, english.name = standing.hor$`Date of change`[-1:-2], stringsAsFactors = F)})
standing.hor.periods <- interval(c(standing.hor.periods), c(standing.hor.periods[-1] - 1, ymd('2100-01-01')))


standing.hoc <- read_excel('metadata_for_analysis/Standing_committees.xlsx', sheet = 'House of Councillors')
standing.hoc.periods <- ymd(sapply(str_split(names(standing.hoc)[-1], pattern='[年月日]'), FUN=function(i){
  paste(i[-4], collapse='-')
}))
standing.hoc <- apply(standing.hoc[-1:-2,-1], MARGIN = 2, FUN=function(i){data.frame(committee.name = i, english.name = standing.hoc$`Date of change`[-1:-2], stringsAsFactors = F)})
standing.hoc.periods <- interval(c(standing.hoc.periods), c(standing.hoc.periods[-1] - 1, ymd('2100-01-01')))


key.jurisdictions <- union(standing.hoc[[1]]$english.name, standing.hor[[1]]$english.name)


store.disagree.unclear <- store.disagree.nonMP <- data.frame()
store.disagree.ministerial <- store.markov <- data.frame()
invalid.corrections <- store.witness <- data.frame()
store.duo <- store.markov.mp <- data.frame()
store.MP <- store.special <- store.mr.correction <- data.frame()
store.pm <- bureaucrat.summary <- store.all <- data.frame()

# Code is *NOT* set up for bicameral meetings (ryoin) 
# More work is needed if you want to use that data

for (chamber in c('syugiin', 'sangiin')){
  #Loop over chambers
  message(chamber)

  file.paths <- paste0('parsed_api_cabinet/', chamber, '_', 
      tolower(gsub(make.names(unique.cabinets$cab_name), 
        pattern='\\.', replacement = '_')), '.RDS')
  file.paths <- file.paths[1:96]
  #Loop over all cabinets

  for (d in file.paths){
    
    print(d)
    all.data <- readRDS(d)
    
    # Confirm all having meeting URLs.
    stopifnot(sum(is.na(all.data$attempted_url)) == 0)
    
    # Remove plenary debates
    all.data <- all.data %>% filter(committee != "本会議")
    ##################
    ###Collapsing Standing Committee Names to Avoid Artificial Name Changes
    ###See Appendix for Discussion
    ##################
    if (chamber == 'syugiin'){
      period <- sapply(ymd(all.data$date), FUN=function(i){which(i %within% standing.hor.periods)}) 
      standing.d <- standing.hor
    }else if (chamber == 'sangiin'){
      period <- sapply(ymd(all.data$date), FUN=function(i){which(i %within% standing.hoc.periods)}) 
      standing.d <- standing.hoc
    }else{
      # Not set up for bicameral.
      stop('Invalid Chamber')
    }
    if (class(period) != 'integer'){
      stop()
    }
    
    split.name <- split(all.data$committee, period)
    transf.name <- mapply(as.numeric(names(split.name)), split.name, SIMPLIFY = FALSE, FUN=function(a,b){
      names.a <- standing.d[[a]]
      m.b <- match(b, names.a$committee.name)
      changed.names <- ifelse(is.na(m.b), b, names.a$english.name[m.b])
      return(changed.names)
    })
    transf.name <- unsplit(transf.name, period)
    all.data$committee.original <- all.data$committee
    all.data$committee <- transf.name
    #########################
    # Correct some typos.
    # See Appendix for discussion.
    #########################

    for (v in c('^.政府委員$', '^.説明員$', '^..説明員$', '^.参考人$', '^.政府委員$')){
      all.data$suffix[grepl(all.data$suffix,pattern=v)] <- gsub(v, pattern='[\\^\\.\\$]', replacement = '')
    }
    #Select all obs with missing suffixes
    missing.suffix <- with(all.data, which(is.na(suffix) & hansard_name != 'Attendance Information'))
    if (length(missing.suffix) != 0){
      #Split based on space. See if any matches the 
      extracted.suffix <- sapply(str_split(all.data$hansard_name[missing.suffix], pattern='\\p{Pe}|\\p{Ps}'), FUN=function(i){
        if (length(i) == 1){
          return(NA)
        }else{
          return(i[1])
        }
      })
      extracted.suffix <- as.character(suffix.dictionary$suffix[match(extracted.suffix, suffix.dictionary$suffix)])
      if (!all(is.na(extracted.suffix))){
        all.data$suffix[missing.suffix] <- extracted.suffix
      }
    }
    
    stopifnot(sum(all.data$suffix %in% c('ERROR')) == 0)

    if (chamber != 'ryoin'){
      #Exclude headings.
      all.data <- subset(all.data, !(suffix %in% c('ERROR')) & 
        !(hansard_name %in% c('Attendance Information', 'Closing Information'))
      )
    }else{
      #In the joint meetings, ordinary members seem to not be given suffixes.
      #So don't exclude those speeches here. Need to CHECK THIS.
      all.data <- subset(all.data, !(suffix %in% 'ERROR'))      
    }
    #Match the suffix to the named ones.
    all.data$speaker.type <- suffix.dictionary$role[match(all.data$suffix, suffix.dictionary$suffix)]
    #If not found, classify as "other". SEE REPLACEMENT FOR KNOWN MPS LATER.
    all.data$speaker.type[is.na(all.data$speaker.type)] <- 'other'
    #Clean up the cabinet name
    fmt.name <- gsub(d, pattern='parsed_api_cabinet.*(sangiin|syugiin|ryoin)_|(\\.Rbin|\\.RDS)', replacement = '')
    fmt.name <- gsub(fmt.name, pattern='_', replacement = ' ')
    #Check name formatting.
    if (nrow(subset(cabinet.data, tolower(fmt_name) == fmt.name)) == 0){stop('Name formatting')}
    if (chamber %in% c('syugiin', 'sangiin')){
      ###Subset the HOR data and merge in to get a list of MPs, names and positions.
      sub.HOR <- subset(cabinet.data, simple.name == fmt.name)
      
      #Merge in the corrections.
      sub.alt.HOR <- subset(hor.alternative.names, cabinet == sub.HOR$cab_name[1])
      if (nrow(sub.alt.HOR) > 0){
        correct.entry <- match(sub.alt.HOR$correction, sub.HOR$name_jp)
        if (any(is.na(correct.entry))){
          invalid.corrections <- rbind(invalid.corrections, sub.alt.HOR[which(is.na(correct.entry)),])
          message('Invalid Corrections')
          print(sub.alt.HOR[which(is.na(correct.entry)),])
          sub.alt.HOR <- sub.alt.HOR[-which(is.na(correct.entry)),]
          correct.entry <- match(sub.alt.HOR$correction, sub.HOR$name_jp)
          if (any(is.na(correct.entry))){stop('???')}
        }
        corrected.HOR <- sub.HOR[correct.entry,]
        corrected.HOR$name_jp <- sub.alt.HOR$full_name
        sub.HOR <- rbind(sub.HOR, corrected.HOR)
      }
      sub.HOR$name_jp <- gsub(sub.HOR$name_jp, pattern='[0-9]+$', replacement = '') 
      sub.HOR <- sub.HOR[,c('name_jp', 'cabappt', 'junior_appt', 'party_jp', 'party_id', 'pid')]
      sub.HOR$chamber <- 'syugiin'
      #We do not have the HOC in a cabinet formatted form so we just match directly.
      sub.HOC <- data.frame(chamber = 'sangiin', name_jp = hoc.data$name, party_jp = hoc.data$party, party_id = hoc.data$party_id, cabappt = -1, junior_appt = -1)
      sub.HOC$pid <- NA
      #Combine the chambers.
      sub.BICAMERAL <- rbind(sub.HOR, sub.HOC)
      #Match against the full data.
      matched.BICAMERAL <- sub.BICAMERAL[match(all.data$full_name, sub.BICAMERAL$name_jp), ]
      matched.BICAMERAL$governing.party <- as.numeric(matched.BICAMERAL$party_id %in% subset(governing.parties, simple.name == fmt.name)$party_id)
      #Is the entry in Reed-Smith (EITHER chamber)?
      all.data$in.ds <- !is.na(matched.BICAMERAL$name_jp)
      all.data$pid <- matched.BICAMERAL$pid
      all.data$ds.name <- matched.BICAMERAL$name_jp
      all.data$ds.party <- matched.BICAMERAL$party_jp
      all.data$matched.chamber <- matched.BICAMERAL$chamber
      
      matched.BICAMERAL$mp.type <- ifelse(matched.BICAMERAL$cabappt == 1 | matched.BICAMERAL$junior_appt == 1, 
                                          ifelse(matched.BICAMERAL$cabappt == 1, 3,2), 
                                          matched.BICAMERAL$governing.party)
      
      all.data[c('mp.type', 'jappt', 'cappt')] <- matched.BICAMERAL[c('mp.type', 'junior_appt', 'cabappt')]
      
      #Confirm there are no duplicate MPs in the bicameral data.
      duplicate.check <- unique(all.data$full_name)
      duplicate.check <- sapply(duplicate.check, FUN=function(i){sum(i %in% sub.BICAMERAL$name_jp)})
      duplicate.check <- duplicate.check[which(duplicate.check > 1)]
      if (length(duplicate.check) > 0){stop('DUPlICATES')}
      #Print the distribution of MPs, should heavily favor the actual chamber...
      print(table(matched.BICAMERAL$chamber))
      
      #Final imputation.
      all.data$speaker.type[which(all.data$in.ds & is.na(all.data$suffix))] <- 'ordinarymember'
      all.data$suffix[which(all.data$in.ds & is.na(all.data$suffix))] <- 'imputed.OM'
    }else{
      stop('Check and confirm code for bicameral (ryoin)')
      in.HOC <- !is.na(match(all.data$full_name, hoc.names))      
      in.HOR <- !is.na(match(all.data$full_name, subset(cabinet.data, tolower(fmt_name) == fmt.name)$name_jp))      
      in.either <- in.HOR | in.HOC
      all.data$in.ds <- in.either
      ##Manually build in ordinary members.
      all.data$speaker.type[which(in.either & is.na(all.data$suffix))] <- 'ordinarymember'
      all.data$suffix[which(in.either & is.na(all.data$suffix))] <- 'imputed.OM'
    }
    if (any(grepl(all.data$speaker.type,pattern='disambig'))){
      which.disambig <- grep(all.data$speaker.type, pattern='disambig')
      split.names <- do.call('rbind', str_split(all.data$speaker.type[which.disambig], pattern='\\.'))[,-1]
      disambig.type <- ifelse(all.data$in.ds[which.disambig], split.names[,1], split.names[,2])
      print('Results of Disambiguation')
      print(table(all.data$speaker.type[which.disambig], disambig.type))
      all.data$speaker.type[which.disambig] <- disambig.type
    }
    
    #If they're a government member AND in Smith-Reed, then parliamentary secretary.
    #Otherwise bureaucrat    
    #     all.data$speaker.type[all.data$suffix %in% disambiguate.labels & all.data$in.ds == TRUE] <- 'parliamentarysec'
    #     all.data$speaker.type[all.data$suffix %in% disambiguate.labels & all.data$in.ds == FALSE] <- 'bureaucrat'
    
    #     ##LAST DITCH IMPUTATION of ALL "MR" no suffix.ri
    incorrect.mr <- all.data[which(grepl(all.data$hansard_name, pattern='君$') & (all.data$speaker.type == 'other')),] %>% group_by(hansard_name, suffix, full_name) %>% tally()
    
    if (nrow(incorrect.mr) != 0){
      incorrect.mr <- incorrect.mr %>% ungroup()
      incorrect.mr$chamber <- chamber
      incorrect.mr$d <- d
      store.mr.correction <- rbind(store.mr.correction, incorrect.mr)
    }
    all.data$speaker.type[which(grepl(all.data$hansard_name, pattern='君$') & (all.data$speaker.type == 'other'))] <- 'ordinarymember'
    
    print(table(all.data$speaker.type, all.data$mp.type, useNA ='always'))
    
    witness <- subset(all.data, speaker.type == 'other')
    if (nrow(witness) != 0){
      witness <- witness %>% group_by(full_name, suffix) %>% summarize(n = n()) %>% ungroup()
      witness$chamber <- chamber
      witness$d <- d
      store.witness <- rbind(store.witness, witness)
    }
    
    
    disagreement.nonMP <- arrange(subset(all.data, speaker.type %in% c('bureaucrat', 'other') & !is.na(mp.type)) %>% 
        group_by(suffix) %>% tally(), desc(n))
    #select all ministerial categories
    disagreement.ministerial <- subset(all.data, 
        !(speaker.type %in% c('bureaucrat', 'other', 'ordinarymember', 'committee.chair')) & !(mp.type %in% c(2,3))) %>% 
      group_by(full_name, matched.chamber, suffix) %>% tally()    
    
    disagreement.unclear <- subset(all.data, !(speaker.type %in% c('bureaucrat', 'other')) & is.na(mp.type)) %>% group_by(full_name) %>% tally()
    
    all.data$markov.id <- NA
    #If bureaucrat or other in Diet Record, but MP in Reed-Smith, use Reed Smith:
    corrected.bureaucrats <- which(all.data$speaker.type %in% c('bureaucrat', 'other') & !is.na(all.data$mp.type))
    if (sum(all.data$suffix[corrected.bureaucrats] %in% override.suffix) > 0){
      corrected.bureaucrats <- corrected.bureaucrats[!(all.data$suffix[corrected.bureaucrats] %in% override.suffix)] 
      if (sum(all.data$suffix[corrected.bureaucrats] %in% override.suffix) > 0){
        stop()
      }
    }
    ##########################################
    # Take the speaker categories
    # and simplify them for the main analysis
    ##########################################
    all.data$speaker.type[corrected.bureaucrats] <- 'ordinarymember'
    #If Diet Record denotes minister, classify as minister *over* Reed-Smith.    
    all.data$markov.id[grepl(all.data$speaker.type, pattern='^minister$|^pm')] <- 'minister'
    all.data$markov.id[grepl(all.data$speaker.type, pattern='^viceminister|parliamentary')] <- 'junior.minister'
    all.data$markov.id[grepl(all.data$speaker.type, pattern='^committee.chair')] <- 'committee.chair'
    #Classify speakers into types based on Reed-Smith
    all.data$markov.id[all.data$speaker.type == 'bureaucrat'] <- 'bureaucrat'
    all.data$markov.id[all.data$speaker.type == 'other'] <- 'other'
    all.data$markov.id[all.data$speaker.type == 'ordinarymember' & all.data$mp.type == 0] <- 'opposition'
    all.data$markov.id[all.data$speaker.type == 'ordinarymember' & all.data$mp.type == 1] <- 'gov.backbench'
    all.data$markov.id[all.data$speaker.type == 'ordinarymember' & all.data$mp.type == 2] <- 'junior.minister'
    all.data$markov.id[all.data$speaker.type == 'ordinarymember' & all.data$mp.type == 3] <- 'minister'
    all.data$markov.id[all.data$speaker.type == 'ordinarymember' & is.na(all.data$mp.type)] <- 'other'
    
    #####MARKOV TRANSITION
    all.data$chr.length <- stringr::str_length(all.data$speech)  
    all.data$chr.length[is.na(all.data$chr.length)] <- 0
    
    all.data$has_question <- grepl(all.data$speech, pattern='(か。|か？|？|か\\.|か\\?)')
    all.data$has_responsibility <- grepl(all.data$speech, pattern='責任')
    all.data$has_minister <- grepl(all.data$speech, pattern = '大臣')
    
    print('Percent with ? and resp')
    print(colMeans(all.data[,c('has_question', 'has_responsibility', 'has_minister')]))
    
    if (chamber %in% c('syugiin', 'sangiin')){      
      if (sum(is.na(all.data$markov.id)) > 0){stop('Markov IDs in complete')}
      # Call via :: to avoid issues with overwriting functions
      triple.matrix <- plyr::ddply(all.data, ~ committee + attempted_url, .fun=function(x){
        x <- arrange(x, speechOrder)
        mp.order <- x$markov.id
        triple <- cbind(mp.order, dplyr::lead(mp.order, 1), dplyr::lead(mp.order, 2))
        triple <- apply(triple, MARGIN = 1, FUN = paste0, collapse =' - ')        
        
        o <- t(bind_rows(tapply(x$chr.length, triple, FUN=function(i){
          data.frame(c(sum(i), length(i)))})))
        o <- data.frame(o)
        o$triple <- rownames(o)
        rownames(o) <- NULL
        
        hr <- data.frame(table(triple[x$has_responsibility]), stringsAsFactors = F)
        hq <- data.frame(table(triple[x$has_question]), stringsAsFactors = F)
        hm <- data.frame(table(triple[x$has_minister]), stringsAsFactors = F)
        if (nrow(hr) > 0){
          names(hr) <- c('triple', 'has_responsibility')
          o <- full_join(o, hr, by = 'triple')          
        }else{
          o$has_responsibility <- 0
        }
        if (nrow(hq) > 0){
          names(hq) <- c('triple', 'has_question')
          o <- full_join(o, hq, by = 'triple')          
        }else{
          o$has_question <- 0
        }
        if (nrow(hm) > 0){
          names(hm) <- c('triple', 'has_minister')
          o <- full_join(o, hm, by = 'triple')          
        }else{
          o$has_minister <- 0
        }
        return(o)
      })
      
      triple.matrix[c('first_speaker', 'second_speaker', 'third_speaker')] <- do.call('rbind', str_split(triple.matrix$triple, pattern=' - '))
      triple.matrix$has_question[is.na(triple.matrix$has_question)] <- 0
      triple.matrix$has_minister[is.na(triple.matrix$has_minister)] <- 0
      triple.matrix$has_responsibility[is.na(triple.matrix$has_responsibility)] <- 0
      triple.matrix <- triple.matrix %>% group_by(committee, triple, first_speaker, second_speaker, third_speaker) %>% 
        summarize(count = sum(X2), characters = sum(X1), has_responsibility = sum(has_responsibility),
                  has_question = sum(has_question), has_minister = sum(has_minister))
      
      stopifnot(sum(triple.matrix$has_minister) == sum(all.data$has_minister))      
      stopifnot(sum(triple.matrix$has_responsibility) == sum(all.data$has_responsibility))
      stopifnot(sum(triple.matrix$has_question) == sum(all.data$has_question))
      
      # Call via :: to avoid issues with overwriting functions
      mp.level <- plyr::ddply(all.data, ~ committee + attempted_url, .fun=function(x){
        x <- arrange(x, speechOrder)
        mp.order <- x$markov.id
        triple <- cbind(mp.order, dplyr::lead(mp.order, 1), dplyr::lead(mp.order, 2))
        triple <- apply(triple, MARGIN = 1, FUN = paste0, collapse =' - ')        
        x$triple <- triple
        out <- x %>% group_by(pid, ds.name, ds.party, speaker.type, triple) %>%
          summarize(n_speeches = length(chr.length), n_char = sum(chr.length), .groups = 'drop') %>% ungroup
        return(out)
      })    
      mp.level[c('first_speaker', 'second_speaker', 'third_speaker')] <- do.call('rbind', str_split(mp.level$triple, pattern=' - '))
      mp.level <- mp.level %>% group_by(committee, triple, pid, ds.name, ds.party, speaker.type, 
                                        first_speaker, second_speaker, third_speaker) %>% summarize(count = sum(n_speeches), characters = sum(n_char))
    }else{
      stop('Set up triples for ryoin')
    }
    all.data$old.speaker.type <- all.data$speaker.type
    all.data$speaker.type <- all.data$markov.id
    #Collapse *back* for the simpler analysis.
    all.data$speaker.type[all.data$speaker.type %in% c('opposition', 'gov.backbench', 'unclear')] <- 'ordinarymember'
    all.data$speaker.type[all.data$old.speaker.type %in% c('pm', 'parliamentarysec', 'viceminister')] <- all.data$old.speaker.type[all.data$old.speaker.type %in% c('pm', 'parliamentarysec', 'viceminister')]
    
    #Tabulate the # and length of speeches by speaker type, meeting date, and suffix.
    suffix <- all.data %>% group_by(committee, attempted_url, speaker.type, cabinet) %>% summarise(n = n(), 
      characters = sum(length_characters), 
      lines = sum(length_lines),
      sentences = sum(length_sentences)
    )
    #We need to add in zeros for a speaker type that never appeared in a meeting.    
    impute.zeros <- expand.grid('speaker.type' = unique(suffix.dictionary$role),
                                'committee' = unique(suffix$committee), 'cabinet' = unique(suffix$cabinet))
    impute.zeros[c('n', 'characters', 'lines', 'sentences', 'attempted_url')] <- 0
    impute.zeros$attempted_url <- 'Imputed: NONE'
    #Build a full dataframe of zeros and bid them to the suffixes
    suffix <- bind_rows(suffix, impute.zeros)
    #-1 on the n.meetings to adjust for the fact that everyone is given one extra.
    collapsed.suffix <- suffix %>% group_by(committee, cabinet, speaker.type) %>% summarise(n.meetings = n()-1, 
                                                                                            n.speeches = sum(n), n.characters = sum(characters), n.lines = sum(lines), n.sentences = sum(sentences))
    n.meetings <- suffix %>% group_by(committee, cabinet) %>% summarise(unique.meetings = length(unique(attempted_url)), 
                                                                        total.speeches = sum(n), total.characters = sum(characters), 
                                                                        total.lines = sum(lines), total.sentences = sum(sentences))
    #Create an "all minister" category. Subset the data to all ministers.
    am <- subset(collapsed.suffix, speaker.type %in% c('parliamentarysec', 'minister', 'pm', 'viceminister'))
    #Collapse all of the variables.
    am <- am %>% 
      group_by(committee, cabinet) %>%
      summarize(across(-'speaker.type', sum))
    am$speaker.type <- 'allminister'
    #Add to the main data.
    collapsed.suffix <- rbind(collapsed.suffix, am)
    #Create a "all JM" category.
    am <- subset(collapsed.suffix, speaker.type %in% c('parliamentarysec', 'viceminister'))[names(collapsed.suffix) != 'speaker.type'] %>% 
      group_by(committee, cabinet) %>% summarise_all(~ sum(.))
    am$speaker.type <- 'juniorminister'
    collapsed.suffix <- rbind(collapsed.suffix, am)
    #Merge in committee metadata, how many speeches/etc. occured.
    collapsed.suffix <- merge(collapsed.suffix, n.meetings, by =c('committee', 'cabinet'), all.x = T)
    collapsed.suffix$chamber <- chamber
    #Bind into the storage data.frame
    bureaucrat.summary <- rbind(bureaucrat.summary, collapsed.suffix)
    
    #Store all of the suffix information.
    ss <- all.data %>% 
      group_by(speaker.type, old.speaker.type, suffix) %>% 
      tally() %>% data.frame
    ss$name <- collapsed.suffix$cabinet[1]
    ss$chamber <- chamber
    store.all <- rbind(store.all, ss)
    
    check_duo <- all.data %>% group_by(committee, attempted_url) %>%
      summarize(
        
        minister = sum(speaker.type %in% c('minister', 'pm')),
        junior.minister = sum(speaker.type %in% c('viceminister', 'parliamentarysec', 'juniorminister')),
        bureaucrat = sum(speaker.type == 'bureaucrat'))
    duo_minister <- check_duo %>%
      mutate(type = (minister > 0) + (bureaucrat > 0) * 2) %>%
      group_by(committee, type) %>%
      tally()
    duo_jm <- check_duo %>%
      mutate(type = ((junior.minister + minister) > 0) + (bureaucrat > 0) * 2) %>%
      group_by(committee, type) %>%
      tally()
    duo_minister$type <- c('neither', 'minister_only', 'bureaucrat_only', 'both')[1 + duo_minister$type]
    duo_jm$type <- c('neither', 'minister_only', 'bureaucrat_only', 'both')[1 + duo_jm$type]
    duo_minister <- duo_minister %>% dcast(committee ~ type, value.var = 'n', fill = 0)
    duo_jm <- duo_jm %>% dcast(committee ~ type, value.var = 'n', fill = 0)
    
    duo_double <- bind_rows(
      duo_minister %>% mutate(type = 'minister'),
      duo_jm %>% mutate(type = 'include_junior')
    )
    duo_double$chamber <- chamber
    duo_double$cabinet <- collapsed.suffix$cabinet[1]      
    store.duo <- bind_rows(store.duo, duo_double)
    
    if (chamber %in% c('syugiin', 'sangiin')){
      triple.matrix$chamber <- chamber
      triple.matrix$cabinet <- collapsed.suffix$cabinet[1]
      
      mp.level$chamber <- chamber
      mp.level$cabinet <- collapsed.suffix$cabinet[1]
      
      if (nrow(disagreement.nonMP) != 0){
        disagreement.nonMP$cabinet <- collapsed.suffix$cabinet[1]
        disagreement.nonMP$chamber <- chamber        
      }
      if (nrow(disagreement.ministerial) != 0){
        disagreement.ministerial$cabinet <- collapsed.suffix$cabinet[1]
        disagreement.ministerial$chamber <- chamber
      }
      if (nrow(disagreement.unclear) != 0){
        disagreement.unclear$cabinet <- collapsed.suffix$cabinet[1]
        disagreement.unclear$chamber <- chamber
      }
      
      mp.data <- all.data %>% filter(!is.na(pid)) %>% 
        group_by(pid, committee) %>% 
        summarize(pr.cab = mean(mp.type == 3),
                  pr.junior = mean(mp.type == 2),
                  pr.gov = mean(mp.type == 1),
                  party_jp = unique(ds.party), speeches = n(), characters = sum(length_characters, na.rm=T))

      store.markov <- rbind(store.markov, triple.matrix %>% ungroup())
      store.markov.mp <- rbind(store.markov.mp, mp.level %>% ungroup())
      store.disagree.nonMP <- rbind(store.disagree.nonMP, disagreement.nonMP %>% ungroup())
      store.disagree.ministerial <- rbind(store.disagree.ministerial, disagreement.ministerial %>% ungroup())
      store.disagree.unclear <- rbind(store.disagree.unclear, disagreement.unclear %>% ungroup())
      
    } 
    
  }
}


store.markov$cabinet_number <- unique.cabinets$cab_code[match(store.markov$cabinet, unique.cabinets$cab_name)]
store.markov$budget <- store.markov$committee == "Budget"
store.markov$all.standing <- store.markov$committee %in% key.jurisdictions
store.markov$committee.type <- c('Other',"Standing",'Budget')[1+with(store.markov, all.standing + budget)]

store.duo$cabinet_number <- unique.cabinets$cab_code[match(store.duo$cabinet, unique.cabinets$cab_name)]
store.duo$budget <- store.duo$committee == "Budget"
store.duo$all.standing <- store.duo$committee %in% key.jurisdictions
store.duo$committee.type <- c('Other',"Standing",'Budget')[1+with(store.duo, all.standing + budget)]

store.markov.mp$cabinet_number <- unique.cabinets$cab_code[match(store.markov.mp$cabinet, unique.cabinets$cab_name)]
store.markov.mp$all.standing <- store.markov.mp$committee %in% key.jurisdictions
store.markov.mp$budget <- store.markov.mp$committee == "Budget"
store.markov.mp$committee.type <- c('Other',"Standing",'Budget')[1+with(store.markov.mp, all.standing + budget)]

saveRDS(store.markov, '../replication_data/final_data/markov_output.RDS')
saveRDS(store.duo, '../replication_data/final_data/duo_participation.RDS')
saveRDS(store.markov.mp, '../replication_data/final_data/panel_mp_markov.RDS')

# Select top fifty suffixes across chamber for SI of RPG paper.
top_fifty <- store.all %>% group_by(chamber, suffix) %>%
  summarize(total_n = sum(n))  %>%
  group_by(chamber) %>% top_n(n=50,wt=total_n)
top_fifty <- left_join(top_fifty,  store.all %>%
    group_by(chamber, suffix, speaker.type) %>%
    summarize(n = sum(n)), by = c('chamber', 'suffix'))
  
saveRDS(top_fifty, '../replication_data/final_data/top_50.RDS')

# Save out various diagnostic data
# This was used building our spelling corrections
# to address the bulk of common issues of people who were
# mis-identified.

store.mr.correction <- store.mr.correction %>% group_by(hansard_name, suffix, full_name) %>% summarize(total = sum(n))
write_excel_csv(arrange(store.mr.correction, desc(total)), file =  'diagnostic_data/corrected_mr.csv')

store.disagree.nonMP <- arrange(store.disagree.nonMP %>% group_by(chamber, suffix) %>% summarize(n = sum(n)), desc(n))
write_excel_csv(store.disagree.nonMP, file =  'diagnostic_data/suffixes_for_MPs_classifed_as_nonMPs.csv')

store.disagree.ministerial <- arrange(store.disagree.ministerial %>% group_by(full_name, cabinet, suffix) %>% summarize(n = sum(n)))
largest.disagreements <- arrange(store.disagree.ministerial %>% group_by(full_name) %>% summarize(n = sum(n)), n)$full_name
store.disagree.ministerial <- store.disagree.ministerial %>% arrange(desc(match(full_name, largest.disagreements)))
write_excel_csv(store.disagree.ministerial, file =  'diagnostic_data/suffixes_for_ministers_not_classifed_in_DietRecord.csv')

store.disagree.unclear <- arrange(store.disagree.unclear %>% group_by(full_name, cabinet) %>% summarize(n = sum(n)), desc(n))
largest.disagreements <- arrange(store.disagree.unclear %>% group_by(full_name) %>% summarize(n = sum(n)), n)$full_name
store.disagree.unclear <- store.disagree.unclear %>% arrange(desc(match(full_name, largest.disagreements)))

store.disagree.unclear$ever.in.RS <- store.disagree.unclear$full_name %in% cabinet.data$name_jp
store.disagree.unclear$manual.nonMP <-manual.corrections$correct_nonmp[match(store.disagree.unclear$full_name, manual.corrections$full_name)]

write_excel_csv(store.disagree.unclear, file =  'diagnostic_data/spelling_issues.csv')
write_excel_csv(invalid.corrections, file =  'diagnostic_data/invalid_corrections.csv')

