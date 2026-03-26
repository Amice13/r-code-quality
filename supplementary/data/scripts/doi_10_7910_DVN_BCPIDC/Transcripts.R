rm(list = ls())

################################################################################

# Packages

library('xml2')
library('stringi')
library('stringr')
library('tidyverse')
library('piecemaker')

################################################################################

# This function extracts the relevant information from the XML files and formats
# it for our data frame

extract <- function(node){
  
  if (xml_name(node) == 'FloorLanguage') {
    
    Affiliation <- 'Language'
    
    Content <- paste(xml_attr(node, 'language'), collapse = ' ')
    
  } else if (xml_name(node) != 'Intervention') {
    
    Affiliation <- xml_name(node)
    
    Content <- paste(xml_text(node), collapse = ' ')
    
  } else {
    
    Affiliation <- paste(xml_text(xml_find_all(node, 'PersonSpeaking/Affiliation')), 
                         collapse = ' ')
    
    Content <- paste(xml_text(xml_find_all(node, 'Content')), collapse = ' ')
    
  }
  
  return(c(Affiliation, Content))
  
}

# This function introduces spaces after some punctuation marks

spaces <- function(text){
  
  text <- gsub("\\.(?=[A-Za-z])", ". ", text, perl = TRUE)
  
  text <- gsub(",(?=[A-Za-z])", ", ", text, perl = TRUE)
  
  text <- gsub("\\?(?=[A-Za-z])", "? ", text, perl = TRUE)

  text <- gsub("!(?=[A-Za-z])", "! ", text, perl = TRUE)
  
  text <- gsub(":(?=[A-Za-z])", ": ", text, perl = TRUE)
  
  text <- gsub(";(?=[A-Za-z])", "; ", text, perl = TRUE)
  
  return(text)
  
}

################################################################################

# This vector contains the number of meetings in each session of our period of study

vec <- c('391' = 175,
         '392' = 117,
         '401' = 13,
         '402' = 128,
         '403' = 149,
         '411' = 272,
         '412' = 235,
         '421' = 438,
         '431' = 45,
         '432' = 124)

df_English <- NULL

df_French <- NULL

# For each meeting of each session in our period of study

for (session in names(vec)) {
  
  for (meeting in 1:vec[session]){
    
    # We upload the Hansard in XML format from the House of Commons' website
    
    hansard_English <- read_xml(paste('https://www.ourcommons.ca/Content/House/', 
                                      session, '/Debates/', sprintf('%03d', meeting), 
                                      '/HAN', sprintf('%03d', meeting), '-E.XML', sep = ''))
    
    # We extract the parts of the Hansard relative to QP
    
    QP <- xml_find_all(hansard_English, 
                       'HansardBody/OrderOfBusiness[contains(translate(OrderOfBusinessTitle, \'ABCDEFGHIJKLMNOPQRSTUVWXYZ\', \'abcdefghijklmnopqrstuvwxyz\'), \'oral\') and contains(translate(OrderOfBusinessTitle, \'ABCDEFGHIJKLMNOPQRSTUVWXYZ\', \'abcdefghijklmnopqrstuvwxyz\'), \'questions\')]')
    
    # Filter out all interventions containing a motion as they are out of order in QP
    
    QP <- xml_find_all(QP, 
                       './*[not(descendant::*[contains(ProceduralText, \'Motion\')])]')
    
    # We extract the transcripts of interventions, but also stamps related to the
    # language in which speeches are pronounced and headers (that will allow us to
    # filter out procedures speeches)
    
    QP <- xml_find_all(QP, 
                       './/Intervention | .//FloorLanguage | .//SubjectOfBusinessTitle | .//SubjectOfBusinessQualifier | .//CatchLine')
    
    # Extract the relevant information using the function defined above
    
    z <- unlist(lapply(QP, extract))
    
    if (!is.null(z)){
      
      z <- matrix(z, ncol = 2, byrow = TRUE) %>%
        as.data.frame()
      
      names(z) <- c('Speaker', 'Text')
      
      # To infer the language in which an intervention was pronounced, we keep 
      # track of the 'stamps' inserted in the Hansard and, in particular, look 
      # at the value of the previous stamp
      
      z <- z %>%
        mutate(Language = ifelse(Speaker == 'Language', Text, NA)) %>%
        fill(Language) %>%
        mutate(Session = session,
               Meeting = meeting)
      
      df_English <- rbind(df_English, z)
      
    }
    
    # Repeat identical steps for French
    
    hansard_French <- read_xml(paste('https://www.ourcommons.ca/Content/House/', 
                                     session, '/Debates/', sprintf('%03d', meeting), 
                                     '/HAN', sprintf('%03d', meeting), '-F.XML', sep = ''))
    
    QP <- xml_find_all(hansard_French, 
                       'HansardBody/OrderOfBusiness[contains(translate(OrderOfBusinessTitle, \'ABCDEFGHIJKLMNOPQRSTUVWXYZ\', \'abcdefghijklmnopqrstuvwxyz\'), \'questions\') and contains(translate(OrderOfBusinessTitle, \'ABCDEFGHIJKLMNOPQRSTUVWXYZ\', \'abcdefghijklmnopqrstuvwxyz\'), \'orales\')]')
    
    QP <- xml_find_all(QP, 
                       './*[not(descendant::*[contains(ProceduralText, \'Motion\')])]')
    
    QP <- xml_find_all(QP, 
                       './/Intervention | .//FloorLanguage | .//SubjectOfBusinessTitle | .//SubjectOfBusinessQualifier | .//CatchLine')
    
    z <- unlist(lapply(QP, extract))
    
    if (!is.null(z)){
      
      z <- matrix(z, ncol = 2, byrow = TRUE) %>%
        as.data.frame() 
      
      names(z) <- c('Speaker', 'Text')
      
      z <- z %>%
        mutate(Language = ifelse(Speaker == 'Language', Text, NA)) %>%
        fill(Language) %>%
        mutate(Session = session,
               Meeting = meeting)
      
      df_French <- rbind(df_French, z)
      
      # Display Progress
      
      cat(paste('\r', session, meeting, '/', vec[session]))
      
    }
    
  }
  
}

# The difficulty is that some interventions not proper to QP may have slipped in
# our data set

# To fix this problem:
# 1. We manually filter out some procedural speeches (based on headers we have identified)
# 2. We filter out interventions from the Speaker (or Chair)
# 3. We filter out interventions containing more than 200 words as they are unlikely 
#    to have been pronounced during QP

vec_English <- c('Privilege',
                 'Points of Order',
                 'Point of Order',
                 'Speaker\'s Ruling',
                 'Speaker\'s Statement',
                 'Presence in Gallery',
                 'Presence in the Gallery',
                 'Electoral Return',
                 'Request for Emergency Debate',
                 'Message from the Senate',
                 'Private Members\' Business',
                 'Tributes',
                 'Tribute',
                 'Business of Supply',
                 'Government Orders',
                 'Statements by Members',
                 'Royal Assent',
                 'Resignation of Member',
                 'Notice of Motion',
                 'Notice of Closure Motion',
                 'Notice of time allocation motion',
                 'Vacancies',
                 'Vacancy',
                 'Ways and Means',
                 'Broadcasting of House Proceedings',
                 'Michael Ferguson')

vec_English_bis <- c('Oral Questions',
                     'Business of the House',
                     'Committees of the House',
                     'First Nations Financial Transparency Act',
                     '150th Anniversary of the First Meeting of Parliament',
                     'Official Report')

df_English <- df_English %>%
  mutate(SubjectOfBusinessTitle = ifelse(Speaker == 'SubjectOfBusinessTitle', 
                                         Text, 
                                         NA),
         CatchLine = ifelse(Speaker == 'CatchLine', 
                            Text, 
                            NA),
         SubjectOfBusinessQualifier = ifelse(Speaker == 'SubjectOfBusinessQualifier', 
                                             Text, 
                                             NA)) %>%
  mutate_at(vars(SubjectOfBusinessTitle, CatchLine, SubjectOfBusinessQualifier), 
            str_squish) %>%
  group_by(Session, Meeting) %>%
  mutate(SubjectOfBusinessTitle = make.unique(SubjectOfBusinessTitle),
         SubjectOfBusinessTitle = ifelse(grepl('NA', 
                                               SubjectOfBusinessTitle), 
                                         NA, 
                                         SubjectOfBusinessTitle)) %>%
  fill(SubjectOfBusinessTitle) %>%
  ungroup() %>%
  mutate(Indicator = grepl(paste(tolower(vec_English), 
                                 collapse = '|'), 
                           tolower(SubjectOfBusinessTitle)) &
           (SubjectOfBusinessTitle != 'Members\' Franking Privileges')) %>%
  group_by(Session, Meeting, SubjectOfBusinessTitle) %>%
  mutate(Indicator = as.integer((Indicator|
                                  any(grepl(paste(tolower(vec_English), 
                                                  collapse = '|'),
                                            tolower(CatchLine)))|
           any(grepl(paste(tolower(vec_English), 
                           collapse = '|'),
                     tolower(SubjectOfBusinessQualifier)))) &
             (SubjectOfBusinessTitle != 'Members\' Franking Privileges'))) %>%
  ungroup() %>%
  group_by(Session, Meeting) %>%
  mutate(Indicator_cumsum = cumsum(Indicator),
         Check = Indicator_cumsum > 0 & Indicator == 0,
         Language = ifelse(Speaker == 'Language', Text, NA)) %>%
  fill(Language) %>%
  ungroup() %>%
  filter(!grepl(tolower(paste(vec_English_bis, 
                              collapse = '|')), 
                tolower(SubjectOfBusinessTitle)) & 
           (Indicator_cumsum == 0) &
           !str_detect(Speaker, 'Le Président') & 
           !str_detect(Speaker, 'Speaker') &
           !str_detect(Speaker, 'The Chair') &
           !grepl('Language|SubjectOfBusinessTitle|SubjectOfBusinessQualifier|CatchLine', 
                  Speaker) &
           !str_detect(tolower(Text), 'unanimous consent') &
           !str_detect(tolower(Text), 'point of order')) %>%
  select(-c('SubjectOfBusinessTitle', 
            'CatchLine', 
            'SubjectOfBusinessQualifier',
            'Indicator',
            'Indicator_cumsum',
            'Check')) %>%
  mutate(Text = sapply(Text, FUN = spaces),
         N_Words = stri_count_words(Text)) %>%
  filter(N_Words < 200) %>%
  mutate(ID = row_number())

# Save the resulting data frame

save(df_English, file = 'Data\df_English.Rdata')

# Repeat identical steps for French

vec_French <- c('Recours au Règlement',
                'Privilège',
                'Privilèges',
                'Décision de la présidence',
                'Message du Sénat',
                'Présence à la tribune',
                'Travaux des subsides',
                'Hommage',
                'Hommages',
                'Initiatives ministérielles',
                'déclarations des députés',
                'Avis de motion',
                'démission d\'un député',
                'Sanction royale',
                'initiatives parlementaires',
                'Vacance',
                'Démission de députés',
                'Avis de motion de clôture',
                'Vacance de siège',
                'Déclaration de la présidence',
                'voies et moyens',
                'Demande de débat d\'urgence',
                'démission d\'une députée',
                'compte de campagne électorale',
                'Gurbax Singh Sohi',
                'télédiffusion des délibérations de la Chambre',
                'Michael Ferguson')

vec_French_bis <- c('Loi sur la transparence financière des Premières Nations',
                    'travaux de la Chambre',
                    'comités de la Chambre',
                    'Le 150e anniversaire de la première séance du Parlement',
                    'questions orales') 

df_French <- df_French %>%
  mutate(SubjectOfBusinessTitle = ifelse(Speaker == 'SubjectOfBusinessTitle', 
                                         Text, 
                                         NA),
         CatchLine = ifelse(Speaker == 'CatchLine', 
                            Text, 
                            NA),
         SubjectOfBusinessQualifier = ifelse(Speaker == 'SubjectOfBusinessQualifier', 
                                             Text, 
                                             NA)) %>%
  mutate_at(vars(Speaker, SubjectOfBusinessTitle, CatchLine, SubjectOfBusinessQualifier), 
            str_squish) %>%
  group_by(Session, Meeting) %>%
  mutate(SubjectOfBusinessTitle = make.unique(SubjectOfBusinessTitle),
         SubjectOfBusinessTitle = ifelse(grepl('NA', 
                                               SubjectOfBusinessTitle), 
                                         NA, 
                                         SubjectOfBusinessTitle)) %>%
  fill(SubjectOfBusinessTitle) %>%
  ungroup() %>%
  mutate(Indicator = grepl(paste(tolower(vec_French), 
                                 collapse = '|'), 
                           tolower(SubjectOfBusinessTitle)) &
           (SubjectOfBusinessTitle != 'Les privilèges de franchise postale des députés')) %>%
  group_by(Session, Meeting, SubjectOfBusinessTitle) %>%
  mutate(Indicator = as.integer((Indicator|
                                   any(grepl(paste(tolower(vec_French), 
                                                   collapse = '|'),
                                             tolower(CatchLine)))|
                                   any(grepl(paste(tolower(vec_French), 
                                                   collapse = '|'),
                                             tolower(SubjectOfBusinessQualifier)))) &
                                  (SubjectOfBusinessTitle != 'Les privilèges de franchise postale des députés'))) %>%
  ungroup() %>%
  group_by(Session, Meeting) %>%
  mutate(Indicator_cumsum = cumsum(Indicator),
         Check = Indicator_cumsum > 0 & Indicator == 0,
         Language = ifelse(Speaker == 'Language', Text, NA)) %>%
  fill(Language) %>%
  ungroup() %>%
  filter(!grepl(tolower(paste(vec_French_bis, 
                              collapse = '|')), 
                tolower(SubjectOfBusinessTitle)) & 
           (Indicator_cumsum == 0) &
           !str_detect(Speaker, 'Le Président') & 
           !str_detect(Speaker, 'The Speaker') & 
           !str_detect(Speaker, 'Le président') & 
           !str_detect(Speaker, 'vice-président') & 
           !str_detect(Speaker, 'président suppléant') &
           !grepl('Language|SubjectOfBusinessTitle|SubjectOfBusinessQualifier|CatchLine', 
                  Speaker) &
           !str_detect(tolower(Text), 'consentement unanime') &
           !str_detect(tolower(Text), 'rappel au règlement') &
           !str_detect(tolower(Text), 'j\'invoque le Règlement')) %>%
  select(-c('SubjectOfBusinessTitle', 
            'CatchLine', 
            'SubjectOfBusinessQualifier',
            'Indicator',
            'Indicator_cumsum',
            'Check')) %>%
  mutate(Text = sapply(Text, FUN = spaces),
         N_Words = stri_count_words(Text)) %>%
  filter(N_Words < 200) %>%
  mutate(ID = row_number())

save(df_French, file = 'Data\df_French.Rdata')
