##Create the relevant periods for the analysis.
periodize.data <- function(scheme.id = 1, limit.scheme = TRUE){
  if (scheme.id == 1){
    #Law Passage
    alt.period.scheme <- list(1:43, 44:58, 59:66, 67:84, 85:92, 93:96)        
  }else if (scheme.id == 2){
    #Law Passage (Alt)
    alt.period.scheme <- list(1:43, 44:58, 59:64, 65:84, 85:92, 93:96)        
  }else if (scheme.id == 3){
    #Law Enactment
    alt.period.scheme <- list(1:43, 44:63, 64:71, 72:84, 85:92, 93:96)        
  }else if (scheme.id == 4){
    # Extended Historical Period
    alt.period.scheme <- list(1:11, 12:58, 59:66, 67:84, 85:92, 93:96)        
  }else if(scheme.id == 5){
    # Exclude "Unstable" Cabinets: Exclude 59 and 67 as "intermediate"
    # Exclude Abe 1.1, Mori 2.1 and Koizumi 3 b/c very short: (81,  71, 78, respectively)
    alt.period.scheme <- list(1:43, 44:58, 60:66, setdiff(68:84, c(71, 78, 81)), 
                              85:92, 93:96)
  }else{stop('Invalid scheme id')}
  if (length(setdiff(1:96, unlist(alt.period.scheme))) != 0){
    if (scheme.id != 5){
      stop('invalid scheme')
    }else{
      print(setdiff(1:96, unlist(alt.period.scheme)))
    }
  }
  if (limit.scheme){
    alt.period.scheme <- c(alt.period.scheme[1:3], list(unlist(alt.period.scheme[4:6])))
    scheme.labels <- c('Pre-1955', '1955 System', 'Reform 1', 'Reform 2')
    
    names(alt.period.scheme) <- scheme.labels
    
    #Collapse R2, DPJ, post-DPJ into one.
  }else{
    scheme.labels <- c('Pre-1955', '1955 System', 'Reform 1', 'Reform 2', 'DPJ', 'Post-DPJ')
    names(alt.period.scheme) <- scheme.labels
  }
  
  fmt.period.scheme <- bind_rows(lapply(alt.period.scheme, data.frame), .id = 'period') 
  names(fmt.period.scheme)[2] <- 'cabinet_number'
  return(list(period = fmt.period.scheme, labels = scheme.labels))
}

collapse.data.by.scheme <- function(data, outcome, scheme.id, limit.scheme){
  outcome <- enquo(outcome)
  period.mapping <- periodize.data(scheme.id = scheme.id, limit.scheme = limit.scheme)
  
  if ('period' %in% colnames(data)){
    message('overwriting period variable with new scheme')
    print(split(period.mapping$period$cabinet_number, period.mapping$period$period))
  }
  
  data$period <- period.mapping$period$period[match(data$cabinet_number, period.mapping$period$cabinet_number)]
  data$period <- factor(data$period, levels = period.mapping$labels, labels = period.mapping$labels)
  #Collapse meetings by cabinet/committee/chamber/speaker type
  data <- data %>% group_by(committee, first_speaker, second_speaker, 
      chamber, cabinet, cabinet_number, budget, period, committee.type) %>%
    summarize(count =sum(!! outcome ))
  #Propogate through "structural" zeros.
  data <- dcast(data, formula = ... ~ first_speaker + second_speaker, value.var = 'count', fill = 0)
  #Melt back down.
  data <- melt(data, id.vars = names(data)[1:8])
  data[,c('first_speaker','second_speaker')] <- do.call('rbind', str_split(data$variable, pattern='_'))
  #Add in totals for first speaker
  data <- full_join(data, data %>% group_by(committee,chamber,first_speaker,cabinet_number) %>% summarize(total = sum(value)) %>% ungroup(), 
                             by =c('committee','chamber','first_speaker', 'cabinet_number'))
  #Add in totals for all speeches.
  data <- full_join(data, data %>% group_by(committee,chamber,cabinet_number) %>% summarize(total.committee = sum(value)) %>% ungroup(), 
                             by =c('committee','chamber','cabinet_number'))
  return(data)
}

prep.data.activity <- function(data, speaker){
  #Prepare data for "activity" regression.
  data <- subset(data, first_speaker == speaker)
  #Collapse over second/third speaker to get all speeches for first speaker
  data  <- data %>% group_by(chamber, committee, cabinet, cabinet_number, 
    budget, period, committee.type, first_speaker) %>% 
    summarize(value = sum(value), total.committee = unique(total.committee))
  #Get the proportion over all committee activity.
  data$pr.activity <- with(data, value/total.committee)
  data <- arrange(data, committee, cabinet_number)
  return(data)
}

fmt.regression <- function(obj, se.type = 'normal'){  
  require(lmtest); require(plm); require(sandwich)
  if (se.type == 'normal'){
    obj.vcov <- vcov(obj)
  }else if (se.type == 'robust'){
    if (class(obj)[1] == 'plm'){
      obj.vcov <- vcovHC(obj, cluster = 'group')        
    }else if (class(obj)[1] == 'lm'){
      obj.vcov <- vcovHC(obj)
    }
  }else{stop('Invalid SE type')}
  #Parse summary into data.frame
  ct.obj <- coeftest(obj, obj.vcov)
  ct.obj <- unclass(ct.obj) %>% data.frame
  names(ct.obj) <- c('est', 'se', 't', 'pr')
  ct.obj$variable <- rownames(ct.obj)
  rownames(ct.obj) <- NULL
  ct.obj$se.type <- se.type
  return(ct.obj)
}


clean.columns <- function(data){
                  #c('Pre-1955', '1955 System', 'Reform 1', 'Reform 2', 'DPJ', 'Post-DPJ')
  abbrev.period <- c('Pre-LDP', 'Pre-R', 'R1', 'R2', 'DPJ', 'Post-DPJ')
  
  if ('variable' %in% colnames(data)){
    data$fmt.name <- gsub(data$variable, pattern='^period', replacement = '')
    data$fmt.name <- gsub(data$fmt.name, pattern='Reform ', replacement = 'R')
    data$fmt.name <- gsub(data$fmt.name, pattern='1955 System', replacement = 'Pre-R')
    data$fmt.name <- factor(data$fmt.name, levels = abbrev.period, ordered = TRUE)
  }
  if ('period' %in% colnames(data)){
    data$fmt_period <- gsub(data$period, pattern='^period', replacement = '')
    data$fmt_period <- gsub(data$fmt_period, pattern='Reform ', replacement = 'R')
    data$fmt_period <- gsub(data$fmt_period, pattern='1955 System', replacement = 'Pre-R')
    data$fmt_period <- factor(data$fmt_period, levels = abbrev.period, ordered = TRUE)
  }
  # if ('post.reform' %in% colnames(data)){
  #   data$fmt.post <- c('Pre-Reform', 'Post-Reform')[data$post.reform + 1]
  #   data$fmt.post <- factor(data$fmt.post, levels = c('Pre-Reform', 'Post-Reform'), ordered = TRUE)
  # }
  if ('chamber' %in% colnames(data)){
    data$fmt.chamber <- factor(data$chamber, 
      levels = c('syugiin', 'sangiin','ryoin'), 
      labels = c('House of Representatives', 'House of Councillors', 'Joint Committee'), ordered = TRUE)    
  }
  for (v in c('first_speaker', 'second_speaker')){
    if (v %in% colnames(data)){
      data[[paste0('fmt_', v)]] <- factor(data[[v]], 
        levels = c('bureaucrat', 'minister', 'junior.minister', 'committee.chair', 'gov.backbench', 'opposition'), 
        labels = c('Bureaucrat', 'Minister', 'Junior\nMinister', 'Committee\nChair', 'Government\nMP', 'Opposition\nMP'), ordered = TRUE)        
    }
  }
  return(data)
}
