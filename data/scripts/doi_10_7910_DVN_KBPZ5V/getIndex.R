#' Get index of filings from SEC Edgar
#'
#' This function extracts the index file of all SEC filings from
#' SEC edgar, given a year range, quarter selection and form type
#' 
#' @param years range of years for index
#' @param quarter selection from 1-4 for quarters of index
#' @param form_types vector of form types to be included index
#' @return dataframe of filing index

require(tidyverse)

getIndex <- function(years, quarter, form_types){
  
  # Create temporary directory for filing
  
  dir.create("./temp_filing_dir")
  
  # initialzing list to keep filing data
  
  filing_index <- list()
  
  for (year in years){
    
    for (quarter in 1:4){
      
      # constructing link to filing
      
      file_link <- paste("https://www.sec.gov/Archives/edgar/full-index/", year, "/QTR", quarter, "/master.gz", sep = "")
      
      # download zipped file
      
      utils::download.file(file_link, destfile = "./temp_filing_dir/master.idx", quiet = TRUE)
      
      # clean data from file
      
      filing_data <- cleanData(path_to_file = "./temp_filing_dir/master.idx")
      
      # filter for filing type of interest, adding to list of filing indices
      
      filing_index[[paste(year, quarter, sep = "_")]] <- filing_data[filing_data$form.type %in% form_types,]
      
    }
  }
  
  # Delete temporary directory
  
  unlink("./temp_filing_dir", recursive = TRUE)
  
  # Closing open file connections
  
  closeAllConnections()
  
  ## state will be the state abbreviation
  
  inc_links <- bind_rows(filing_index)
  
  return(inc_links)
  
}