rm(list = ls())

################################################################################

# Packages

library('xml2')
library('tidyverse')
library('zoo')

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

df_Date <- NULL

# For each meeting of each session in our period of study

for (session in names(vec)) {
  
  for (meeting in 1:vec[session]){
    
    # We upload the Hansard in XML format from the House of Commons' website
    
    hansard <- read_xml(paste('https://www.ourcommons.ca/Content/House/', 
                              session, '/Debates/', sprintf('%03d', meeting), 
                              '/HAN', sprintf('%03d', meeting), '-E.XML', sep = ''))
    
    # We extract the date of the meeting from the XML file
    
    text <- xml_text(xml_find_all(hansard, 
                                  'ExtractedInformation/ExtractedItem[@Name = \'HeaderDate\']'))
    
    # We add it to our data frame
    
    df_Date <- rbind(df_Date, c('Session' = session, 
                                'Meeting' = meeting, 
                                'Date' = text))
    
    # Display Progress
    
    cat(paste('\r', session, meeting, '/', vec[session]))
      
  }
  
}

df_Date <- as.data.frame(df_Date) %>%
  mutate(Date = as.Date(Date, format = '%B %d, %Y'),
         Session = as.numeric(Session),
         Meeting = as.numeric(Meeting))

save(df_Date, file = 'Data\df_Date.Rdata')
