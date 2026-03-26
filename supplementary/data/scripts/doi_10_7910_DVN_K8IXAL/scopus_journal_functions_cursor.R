# Nexsys - Academic abstracts text analysis
# Brian Boyle
# Created at: 2023-02-07
# Last updated: 2023-02-07


# Custom functions for collecting academic publication data using the Elsiver Scopus API

## Resources
# Scopus website: https://www.elsevier.com/en-gb/solutions/scopus
# Scopus developer website: https://dev.elsevier.com/
# Scopus search API documentation : https://dev.elsevier.com/documentation/SCOPUSSearchAPI.wadl
# API key set up instructions: https://cran.r-project.org/web/packages/rscopus/vignettes/api_key.html#:~:text=Steps%20to%20get%20API%20key&text=You%20need%20to%20provide%20a,Click%20%E2%80%9CCreate%20API%20Key%E2%80%9D.
# rscopus package information: https://cran.r-project.org/web/packages/rscopus/rscopus.pdf
# Scopus journal search: https://www.scopus.com/sources.uri?zone=TopNavBar&origin=searchbasic

## Set up
# Creating an account via your academic institution will allow you to create an API key
# This can only be used when connected to your institution's network, so either on campus, or when using a university VPN


## Notes
# Maximum number of entries that can be returned from API requests
# view = 'standard', 200
# view = 'complete, 25
# The complete view contains the abstract text for articles



# Packages ----------------------------------------------------------------
library(here) # CRAN v1.0.1
library(rscopus) # CRAN v0.6.6
library(dplyr) # CRAN v1.1.4
library(data.table) # CRAN v1.16.0
library(purrr) # CRAN v1.0.2




# 01. See how many articles are returned when searching a specific journal and time period --------------------------

## Example use:
# Journal_date_results(issn = NA, data = NA)

## issn
# String containing target journal's issn number.
# If multiple issn's are included, the function will loop through each in turn.

# date
# String containing date range to search for each journal (by year).
# Can be single year, or year range separated by '-'.

# issn and date arguments must be of the same length.

# journal_date_results(issn = c('1540-5907', '1540-5907', '0261-3794'),
#                      date = c('2022-2023', '2008', '2003-2005'))
#



journal_date_results <- function(issn = NA, date = NA,
                                 api_key = rscopus::get_api_key()) {
  # Create search queries for API call
  journal_query <- as.list(paste0("issn(", issn, ")"))

  # If a single date is given, but multiple journals
  # Use same date for all journal requests
  if (length(date) < length(issn) & length(date) == 1) {
    date <- rep(date, length(issn))
  }

  # Empty list to store output
  api_output_list <- list()

  results_total <- NULL
  journal_name <- NULL

  # Loop through each query
  for (i in seq_along(date)) {
    # Run first API call, save output and check total number of search results
    api_output_list[[i]] <-
      rscopus::generic_elsevier_api(
        query = journal_query[[i]],
        type = "search",
        search_type = "scopus",
        date = date[i],
        max_count = Inf,
        view = "COMPLETE",
        api_key = rscopus::get_api_key(),
        verbose = F
      )


    # Total results matching search query

    if (is.null(api_output_list[[i]]$content$`search-results`$entry[[1]]$`prism:publicationName`)) {
      journal_name <- c(journal_name, NA)
    } else {
      journal_name <- c(journal_name, api_output_list[[i]]$content$`search-results`$entry[[1]]$`prism:publicationName`)
    }

    results_total <- c(results_total, as.numeric(api_output_list[[i]]$content$`search-results`$`opensearch:totalResults`))

    print(paste("Journal", i, journal_query[[i]]))
  }

  api_output <-
    data.frame(
      journal_name = journal_name,
      issn = issn,
      date = date,
      results_total = results_total
    )

  return(api_output)
}






# 02.00 Collect article information for a specific journal by year -----------------

# First we construct the appropriate API call, then make the first request.
# If there are more search results than the maximum request limit, repeat process until
# all search results are returned.
# Output will be a list, with each list item containing information for a single article, with sub-items in xml format

# Rate limits
# Scopus search rate limit information: https://dev.elsevier.com/api_key_settings.html
# Max requests per week   = 20,000
# Max requests per second = 9

# 200 items (articles) returned per request on 'standard' search
# 25 items (articles) returned per request on 'complete' search
# Different values returned by each search type can be seen here: https://dev.elsevier.com/sc_search_views.html

# To collect abstract text for articles we need to use 'complete' search.
# Citation information is also included in the narrower 'standard' search.




#  02.01 Get first journal query ----------------------------------------------

# Run first API call, store output
# Check number of search results, if multiple calls needed, update counter

journal_date_search <- function(issn = NA, date = NA,
                                api_key = rscopus::get_api_key(),
                                search_type = "COMPLETE") {
  # Create search query for API call
  journal_query <- paste0("issn(", issn, ")")

  # Empty list to store output from API calls
  # Create counter for number of articles scraped
  api_output <- list()
  results_start <- 0

  # Run first API call, save output and check total number of search results
  api_output[[1]] <-
    rscopus::generic_elsevier_api(
      query = journal_query,
      type = "search",
      search_type = "scopus",
      cursor = "*",
      date = date,
      max_count = Inf,
      view = search_type,
      api_key = rscopus::get_api_key(),
      verbose = F
    )


  if (!is.null(api_output[[1]]$content$`search-results`$entry[[1]]$error)) {
    entries <- NA
    return(entries)
  } else {
    # Total results matching search query
    results_total <-
      as.numeric(api_output[[1]]$content$`search-results`$`opensearch:totalResults`)

    # Add collected articles to counter
    results_captured <-
      results_start + api_output[[1]]$content$`search-results`$entry |>
      length()

    # Name of journal
    journal_name <- api_output[[1]]$content$`search-results`$entry[[1]]$`prism:publicationName`


    # 02.02 If needed, run additional requests -----------------------------------


    # If  results have all been collected
    if (results_captured >= results_total) {
      # Print progress
      cat("Journal: ", journal_name, "\n",
        "  Years: ", date, "\n",
        "Total search results: ", results_total, "\n", "Articles collected ", results_captured, "/",
        length(api_output[[1]]$content$`search-results`$entry),
        sep = ""
      )
    }

    # If more results are still to be collected
    if (results_captured < results_total) {
      # Print progress
      cat("Journal: ", journal_name, "\n",
        "  Years: ", date, "\n",
        "Total search results: ", results_total, "\n", "Articles collected ", results_captured, "/",
        results_total, "\n",
        sep = ""
      )


      i <- 1
      # Loop through as many API requests as needed

      while (results_captured < results_total) {
        i <- i + 1

        api_output[[i]] <-
          rscopus::generic_elsevier_api(
            query = journal_query,
            type = "search",
            search_type = "scopus",
            # Extract code for next page of results from the previous call's output
            cursor = api_output[[i - 1]]$content$`search-results`$cursor$`@next`,
            date = date,
            max_count = Inf,
            view = "COMPLETE",
            api_key = rscopus::get_api_key(),
            verbose = F
          )
        # Update counter (current value + new results collected)
        results_captured <-
          results_captured + length(api_output[[i]]$content$`search-results`$entry)

        # Rest for .5 seconds between calls
        Sys.sleep(.5)

        # Print progress
        cat("Articles collected ", results_captured, "/", results_total, "\n", sep = "")
      }
    }

    # From the raw API output, the main content is in the nested item called 'entries'


    get_entries <- function(raw_api_results) {
      raw_api_results[["content"]][["search-results"]][["entry"]]
    }

    # Unnest and bind the lists together so each list element == 1 article
    entries <-
      api_output |>
      lapply(get_entries) |>
      purrr::reduce(c)


    return(entries)
  } # else statement for error message
}










# 03.00 Parse XML output -----------------------------------------------------

# Convert the API output from a list of articles in XML format, to a dataframe with a row for each article.
# For values containing a list of multiple items (e.g. author/affiliation information), paste values together separated by '|'
# So for an article with multiple authors, the 'author' column would look like 'author1name | author2name'

# Within each article entry are list items that we want to convert into a row in a dataframe
# To do this we need to unlist any items that have length > 1

# The 'link' item contains urls that link to the article search queries.
# The relevant article ID information is already catpure by other fields, so this can be removed.

# 'Affiliation', 'author-count', and 'author' can contain multiple values,
# so these will be collapsed into single values that will fit in a dataframe cell.

# Any empty values with length == 0 will be removed.



parse_scopus_output <- function(entries = NA) {
  # 03.01 Remove links and empty items --------------------------------------


  # Parse xml
  remove_item <- function(entry) {
    # Remove links
    entry$link <- NULL

    # Get length of each list element
    item_len <- entry |>
      lapply(length) |>
      unlist()

    # Which elements have length < 1
    k <- which(item_len == 0) |> as.numeric()

    # Remove these from the list
    entry[k] <- NULL

    return(entry)
  }

  # Apply function over each article (list item)
  entries <-
    entries |>
    lapply(remove_item)


  # Now we have removed the unwanted links, and elements with length 0
  # Now we need to extract and collapse the author and affiliation information




  # 03.02 Extract author info  ----------------------

  # Extract author data for each entry in the API output

  author_data_list <-
    entries |>
    lapply("[[", "author")

  # Apply function across each article entry
  author_data <-
    author_data_list |>
    lapply(unlist)

  # author_data = list of length(entries), each list item contains character strings containing author information
  # string names will be repeated across each author
  # e.g. authname will be included once for each author, so we can use this to collapse information on all authors across each piece of information

  # Collapse author information for selected variable
  auth_combine <- function(auth_dat, var) {
    paste0(
      auth_dat[auth_dat |>
        names() |>
        grep(var, x = _)],
      collapse = " | "
    )
  }

  # Apply author info extraction for all articles, across selected variables

  author_count <- lapply(author_data_list, length) |>
    unlist()

  author_id <-
    lapply(author_data, auth_combine, var = "authid") |>
    unlist()

  author_name <-
    lapply(author_data, auth_combine, var = "authname") |>
    unlist()

  author_surname <-
    lapply(author_data, auth_combine, var = "surname") |>
    unlist()

  author_given_name <-
    lapply(author_data, auth_combine, var = "given-name") |> unlist()

  author_initials <-
    lapply(author_data, auth_combine, var = "initials") |>
    unlist()

  # Combine author information into dataframe that can be merged in with tidy output
  author_df <-
    cbind(
      author_count, author_id, author_name,
      author_surname, author_given_name, author_initials
    )




  # 03.03 Extract affiliation info  ----------------------
  # Repeat same process for author affiliation data

  # Extract author data for each entry in the API output
  affil_data_list <-
    entries |>
    lapply("[[", "affiliation")


  affil_data <-
    affil_data_list |>
    lapply(unlist)

  # Can reuse author function to extract affiliation info
  auth_combine <- function(auth_dat, var) {
    paste0(
      auth_dat[auth_dat |>
        names() |>
        grep(var, x = _)],
      collapse = " | "
    )
  }

  # Apply affiliation info extraction for all articles, across selected variables

  affil_id <-
    lapply(affil_data, auth_combine, var = "afid") |>
    unlist()

  affil_name <-
    lapply(affil_data, auth_combine, var = "affilname") |>
    unlist()


  affil_city <-
    lapply(affil_data, auth_combine, var = "affiliation-city") |>
    unlist()


  affil_country <-
    lapply(affil_data, auth_combine, var = "affiliation-country") |>
    unlist()

  affil_df <-
    cbind(affil_id, affil_name, affil_city, affil_country)





  # 03.04 Clean and bind data ----------------------


  # Remove author and affiliation list items, bind all entries,
  # and add in author and affiliation dataframes.

  # Function to remove unwanted article features
  remove_auth_affil <- function(data_input) {
    data_input[["author"]] <- NULL
    data_input[["author-count"]] <- NULL
    data_input[["affiliation"]] <- NULL

    return(data_input)
  }

  # Apply function across each article
  entries <- entries |>
    lapply(remove_auth_affil) |>
    data.table::rbindlist(fill = T)

  # Remove unwanted columns (api call link, repeated open acces info)
  entries <- entries[, -c("@_fa", "openaccessFlag", "freetoread", "freetoreadLabel")]

  # Bind with author and affiliation data
  entries_df <- cbind(author_df, affil_df, entries)
}

# 03.05 Clean and reorder dataframe -----------------------------------------




tidy_output <- function(entries_df) {
  # If keywords column not present, add in empty column
  if (!"authkeywords" %in% names(entries_df)) {
    entries_df$authkeywords <- NA
  }

  if (!"prism:doi" %in% names(entries_df)) {
    entries_df$`prism:doi` <- NA
  }

  if (!"prism:issn" %in% names(entries_df)) {
    entries_df$`prism:issn` <- NA
  }

  if (!"prism:eIssn" %in% names(entries_df)) {
    entries_df$`prism:eIssn` <- NA
  }

  if (!"prism:pageRange" %in% names(entries_df)) {
    entries_df$`prism:pageRange` <- NA
  }

  if (!"prism:issueIdentifier" %in% names(entries_df)) {
    entries_df$`prism:issueIdentifier` <- NA
  }

  if (!"fund-sponsor" %in% names(entries_df)) {
    entries_df$`fund-sponsor` <- NA
  }

  if (!"fund-acr" %in% names(entries_df)) {
    entries_df$`fund-acr` <- NA
  }

  #   if(!'freetoreadLabel' %in% names(entries_df)){
  #    entries_df$`freetoreadLabel` <- NA
  #   }
  #
  #   if(!'freetoread' %in% names(entries_df)){
  #    entries_df$`freetoread` <- NA
  # }


  entries_tidy <-
    entries_df |>
    dplyr::mutate(
      article_year = stringr::str_sub(`prism:coverDate`, 1, 4),
      api_call_date = Sys.Date()
    ) |>
    dplyr::select(
      journal_name = "prism:publicationName",
      article_cover_date = "prism:coverDate",
      journal_vol = "prism:volume",
      journal_iss = "prism:issueIdentifier",
      journal_pages = "prism:pageRange",
      article_title = "dc:title",
      article_submit_author = "dc:creator",
      article_abstract = "dc:description",
      article_keywords = "authkeywords",
      article_display_date = "prism:coverDisplayDate",
      article_year,
      article_doi = "prism:doi",
      article_cite_count = "citedby-count",
      article_url = "prism:url",
      article_scopus_id = "dc:identifier",
      article_eid = "eid",
      journal_issn = "prism:issn",
      journal_eissn = "prism:eIssn",
      "author_count",
      "author_id",
      "author_name",
      "author_surname",
      "author_given_name",
      "author_initials",
      "affil_id",
      "affil_name",
      "affil_city",
      "affil_country",
      publication_type_agg = "prism:aggregationType",
      publication_type_sub_code = "subtype",
      publication_type_sub = "subtypeDescription",
      source_id = "source-id",
      fund_no = "fund-no",
      open_access = "openaccess",
      fund_sponsor = "fund-sponsor",
      fund_short = "fund-acr",
      api_call_date
    )


  return(entries_tidy)
}


# Export output -----------------------------------------------------------


save_entries <- function(dataset = NA,
                         path = "./",
                         abbrev = NA,
                         overwrite = F) {
  # Split article data by year of publication
  entries_by_year <- dataset |>
    dplyr::group_split(article_year)

  # Extract and clean journal name, and use this to create a new folder in the filepath
  journal_nam <- str_replace_all(unique(tolower(entries_by_year[[1]]$journal_name)), "[^\\w\\s]", " ") |>
    str_replace_all("  ", " ") |>
    str_replace_all(" ", "_")

  folder_path <- file.path(path, journal_nam)[1]

  # If journal folder directory does not exist, create it
  if (dir.exists(folder_path) == F) {
    dir.create(folder_path)
  }


  # For each journal-year, save output as a single .rds file
  for (i in seq_along(entries_by_year)) {
    ## Create filepath for output
    # If journal abbreviation set, use that for file naming
    if (is.na(abbrev)) {
      output_path <-
        paste0(
          folder_path, "/",
          journal_nam, "_s",
          unique(entries_by_year[[i]]$source_id), "_y",
          unique(entries_by_year[[i]]$article_year), ".rds"
        )
    }

    # If not, use full journal name for file naming
    if (!is.na(abbrev)) {
      output_path <-
        paste0(
          folder_path, "/",
          tolower(abbrev), "_s",
          unique(entries_by_year[[i]]$source_id), "_y",
          unique(entries_by_year[[i]]$article_year), ".rds"
        )
    }





    ## Export data after checking whether file already exists
    # If file already exists, and overwrite == T, save output
    if (file.exists(output_path) & overwrite == T) {
      saveRDS(entries_by_year[[i]], output_path)
    }

    # If file already exists, and overwrite == F, print_message
    if (file.exists(output_path) & overwrite == F) {
      cat(
        "File already exists:", "\n",
        output_path, "\n",
        "To automatically overwrite with new data, set overwrite == T in function call", "\n"
      )
    }

    # If file does not exist, save output to filepath
    if (file.exists(output_path) == F) {
      saveRDS(entries_by_year[[i]], output_path)
    }
  }
}







# 05.00 Get filepaths for importing selected journal-years ----------------



target_paths <- function(journals, years, jdir = here("data", "journal_year")) {
  # Get filepaths for all journal-year files
  journal_year_paths_all <-
    paste0(
      jdir, "/",
      list.files(jdir, recursive = T)
    )

  # Get index for files that match search criteria
  k <-
    which(grepl(paste0(journals, collapse = "|"), journal_year_paths_all) &
      grepl(paste0("y", years, collapse = "|"), journal_year_paths_all))

  # Select correct paths and return output
  return(journal_year_paths_all[k])
}
