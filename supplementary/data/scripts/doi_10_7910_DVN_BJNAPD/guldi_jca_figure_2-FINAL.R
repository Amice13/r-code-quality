################################
## FIGURE 2 & Various Tables
#
# Keyword Count Visualizations to accompany
# Jo Guldi, "Critical Search," Journal of Cultural Analytics (2018)
#
# This code uses a longer set of keywords derived from the Oxford English Dictionary.
# It generates tables listing the debates that have the highest counts
# of particular keywords.
# It also plots the most significant keywords over time.

library(data.table)
library(foreach)
library(dplyr)
library(tidyr)
library(tidytext)
library(gridExtra)
library(grid)
library(ggplot2)
library(lubridate)
library(itertools)
library(grid)
library(gtable)
library(doParallel)
library(ggrepel)

# slower machines may change these parameters to downsample the data
firstyear <- 1803
lastyear <- 1911

# folder locations -- modify to match your own file locations
datafolder <- "~/Box Sync/#learningtocode/data/hansard_data"
vizfolder <- "~/Box Sync/#learningtocode/visualizations"
wordcount_visualizations <-
  "~/Box Sync/#learningtocode/visualizations/wordcount_tables_fig_2"
wordcount_data <-
  "~/Box Sync/#learningtocode/data/wordcount_tables_fig_2"

##### read in hansard
print("reading in hansard...")

##### the data file in question has already been stemmed for better matching
setwd(datafolder)
hansard <-
  read.csv(
    "member_names_mispelled_words.tsv",
    sep = "\t",
    header = FALSE,
    stringsAsFactors = FALSE
  )
print("hansard read.")

##### clean up the variable names
hansard2 <- hansard
names(hansard2) <-
  c("row_number",
    "id_number",
    "speechdate",
    "debate",
    "speaker",
    "contituency",
    "text") #rename the columns
hansard2$text <-
  as.character(hansard2$text) %>%  # make the text column to be of the class character (instead of factor)
  gsub("\\-", " ", .) #replace hyphens with spaces (so that we don't get into trouble with "fee simple"
hansard2$debate <-
  as.character(hansard2$debate) # make the text column to be of the class character (instead of factor)
hansard2$speechdate <- ymd(hansard2$speechdate)
hansard2$year <- year(hansard2$speechdate)
hansard2 <- hansard2 %>%
  filter(year >= firstyear) %>%
  filter(year <= lastyear)

##### list the keywords that will comprise the search terms
inhabitation_words <-
  c(
    "acredale",
    "allotment",
    "almsland",
    "apanage",
    "cammandery",
    "comonage",
    "commonty",
    "depopulation",
    "dispossess",
    "esplees",
    "feud",
    "gavelkind",
    "gwely",
    "householdership",
    "lotment",
    "mese",
    "mivvy",
    "occupance",
    "occupancy",
    "pendicle",
    "poffle",
    "pollam",
    "severalty",
    "solidate",
    "sorning",
    "squat",
    "squatment",
    "squatter",
    "squatting",
    "tenancy",
    "thanage",
    "thaneland",
    "allodial",
    "almoign",
    "bookland",
    "burgage",
    "copyhold",
    "croft",
    "crofter",
    "crofting",
    "demesne",
    "domesday",
    "evict",
    "evictee",
    "evicting",
    "evicted",
    "evicter",
    "eviction",
    "evictor",
    "feudee",
    "frankalmoign",
    "freehold",
    "freeholding",
    "freelage",
    "gavelkind",
    "homager",
    "landhold",
    "landholder",
    "landholding",
    "landlordism",
    "leasehold",
    "leaseless",
    "leaser",
    "lessee",
    "lesseeship",
    "lifehold",
    "liferent",
    "liferenter",
    "liferentrix",
    "livier",
    "manorialize",
    "manurable",
    "manurer",
    "mesnalty",
    "metayage",
    "metayer",
    "ouster",
    "parage",
    "pendicle",
    "pendicler",
    "rent",
    "rentage",
    "rentaller",
    "renter",
    "roturier",
    "roture",
    "rundale",
    "rundaled",
    "runrig",
    "runrigged",
    "ryoti",
    "ryotwar",
    "ryotwari",
    "seisin",
    "socage",
    "socager",
    "socman",
    "sokeman",
    "sokemanry",
    "steelbow",
    "subaltern",
    "subfeu",
    "subfeudatory",
    "sublesseee",
    "subtacksman",
    "subtenancy",
    "subtenant",
    "subtenure",
    "subvassal",
    "subvassalage",
    "swain",
    "tacksman",
    "tanistic",
    "tanistry",
    "tenancy",
    "tenant",
    "tenanted",
    "tenantable",
    "tenantry",
    "tenantship",
    "tenement",
    "tenemental",
    "tenementary",
    "tenementer",
    "tenure",
    "tenurial",
    "tenurially",
    "termon",
    "termoner",
    "termor",
    "thanage",
    "thaneland",
    "udal",
    "udaller",
    "underlessee",
    "unfeuded",
    "unleased",
    "unlet",
    "unlettable",
    "urbarial",
    "undertenant",
    "undertenure",
    "venville",
    "villar",
    "villeinage",
    "wardatar",
    "zemindar",
    "zemindarship",
    "zemindary",
    "zamandar",
    "zamandary",
    "absentee",
    "absenteeism",
    "absenteeship",
    "ejectment",
    "enclosure",
    "inclosure"
  )


#### set up a parallel backend for faster processing
cores = detectCores() - 1#
cl <-
  makeCluster(cores, outfile = "speaker-entropy.txt") #not to overload your computer
registerDoParallel(cl)


##### this loop cycles through each term in the list
for (i in 1:length(inhabitation_words)) {
  term1 <- inhabitation_words[i]
  
  # find just the debates where term1 appears
  hansard3 <- hansard2 %>%
    filter(grepl(term1, text))
  
  print("unpacking hansard into words... ")
  
  if (nrow(hansard3) > 0) {
    # tokenize the debates into a dataframe of individual words
    hansard_words <- foreach(
      m = isplitRows(hansard3, chunks = cores),
      .combine = 'rbind',
      .packages = 'tidytext'
    ) %dopar% {
      unnest_tokens(m, word, text, token = "words")
    }
    
    print("debates have been tokenized")
    
    # filter for the appearances of term1
    term_words <- hansard_words %>%
      filter(grepl(term1, word))
    
    # count the appearances of term1
    term_wordcount <-
      term_words %>% group_by(debate, year, speechdate) %>%
      summarise(wordcount = n()) %>%
      ungroup()
    
    
    # select just the top appearances for documentation in the table
    top_term_wordcount <- term_wordcount %>%
      top_n(10, wt = wordcount) %>%
      arrange(desc(wordcount))
    
    # generate visual tables listing the debates with the highest count for term1
    setwd(wordcount_visualizations)
    table <- tableGrob(top_term_wordcount)
    title <-
      textGrob(
        paste0(
          "Debates With Highest Wordcounts for '",
          stringr::str_to_title(term1),
          "'"
        ),
        gp = gpar(fontsize = 20)
      )
    padding <- unit(0.8, "line")
    footnote <- textGrob(
      "",
      x = 0,
      hjust = 0,
      gp = gpar(fontface = "italic")
    )
    table <- gtable_add_rows(table,
                             heights = grobHeight(title) + padding,
                             pos = 0)
    table <- gtable_add_rows(table,
                             heights = grobHeight(footnote) + padding)
    table <- gtable_add_grob(
      table,
      list(title, footnote),
      t = c(1, nrow(table)),
      l = c(1, 2),
      r = ncol(table)
    )
    g <- grid.newpage()
    g <- grid.draw(table)
    
    # save the tables
    setwd(wordcount_visualizations)
    jpg(
      paste0("top-debates-per-term-",
             term1,
             ".jpg"),
      height = 11,
      width = 8.5,
      paper = "letter"
    )
    print(table)
    grid.draw(table)
    dev.off()
    
    
    # save the wordcounts for later use
    setwd(wordcount_data)
    write.csv2(top_term_wordcount,
               paste0("top-debates-containing-term-", term1, ".csv"))
    
  }
}


inhabitation_bigrams <-
  c(
    "forc purchas",
    "emin domain",
    "displac famil",
    "remov tenant",
    "tenant remov",
    "fee simple",
    "communal land",
    "common land",
    "commun land",
    "compound householder",
    "compound household",
    "ground rent",
    "thane land",
    "field book",
    "land registry",
    "land occupier",
    "land occcupi",
    "land tenure",
    "life holder",
    "life tenancy",
    "life tenant",
    "life rent",
    "life renter",
    "tenant paravail",
    "rack rent",
    "rack renter",
    "rack renting",
    "rack rented",
    "squatter right",
    "squatter's right",
    "squatter's rights",
    "sub fief",
    "tenant sted",
    "torrens system",
    "fee simple",
    "frank ferm",
    "acre dale",
    "ground rent",
    "udal land"
  )

# the same loop -- this time counting two-word phrases
for (i in 1:length(inhabitation_bigrams)) {
  bigram1 <- inhabitation_bigrams[i]
  
  # Search for debates that contain bigram1
  hansard3 <- hansard2 %>%
    filter(year >= firstyear) %>%
    filter(year <= lastyear) %>%
    filter(stringr::str_detect(text, bigram1))
  
  print("unpacking hansard into words... ")
  
  # If anything was returned, tokenize those debates
  if (nrow(hansard3) > 0) {
    # search for ngrams in parallel
    hansard_words <- foreach(
      m = isplitRows(hansard3, chunks = cores),
      .combine = 'rbind',
      .packages = 'tidytext'
    ) %dopar% {
      unnest_tokens(m, bigram, text, token = "ngrams", n = 2)
    }
    
    print("debates have been tokenized")
    
    # find the appearances of term1
    term_words <- hansard_words %>%
      filter(grepl(bigram1, bigram))
    
    # count the appearances of term1
    term_wordcount <-
      term_words %>% group_by(debate, year, speechdate) %>%
      summarise(wordcount = n()) %>%
      ungroup()
    
    
    # select just the top appearances for documentation in the table
    top_term_wordcount <- term_wordcount %>%
      top_n(10, wt = wordcount) %>%
      arrange(desc(wordcount))
    
    
    # Generate visual tables listing the debates with the highest count for term1
    setwd(wordcount_visualizations)
    table <- tableGrob(top_term_wordcount)
    title <-
      textGrob(
        paste0(
          "Debates With Highest Wordcounts for '",
          stringr::str_to_title(bigram1),
          "'"
        ),
        gp = gpar(fontsize = 20)
      )
    padding <- unit(0.8, "line")
    footnote <- textGrob(
      "",
      x = 0,
      hjust = 0,
      gp = gpar(fontface = "italic")
    )
    table <- gtable_add_rows(table,
                             heights = grobHeight(title) + padding,
                             pos = 0)
    table <- gtable_add_rows(table,
                             heights = grobHeight(footnote) + padding)
    table <- gtable_add_grob(
      table,
      list(title, footnote),
      t = c(1, nrow(table)),
      l = c(1, 2),
      r = ncol(table)
    )
    g <- grid.newpage()
    g <- grid.draw(table)
    
    # Save the tables
    setwd(wordcount_visualizations)
    jpg(
      paste0("top-debates-per-term-", bigram1, ".jpg"),
      height = 11,
      width = 8.5,
      paper = "letter"
    )
    print(table)
    grid.draw(table)
    dev.off()
    
    # save the wordcounts for later use
    setwd(wordcount_data)
    write.csv2(top_term_wordcount,
               paste0("top-debates-containing-term-", bigram1, ".csv"))
    
    
  }
}

#### Trigrams
inhabitation_trigrams <-
  c("fixity of tenure", "tenant at will", "tenant for life")

# the same loop -- this time counting two-word phrases
for (i in 1:length(inhabitation_trigrams)) {
  trigram1 <- inhabitation_trigrams[i]
  
  hansard3 <- hansard2 %>%
    filter(year >= firstyear) %>%
    filter(year <= lastyear) %>%
    filter(stringr::str_detect(text, trigram1))
  
  print("unpacking hansard into words... ")
  
  if (nrow(hansard3) > 0) {
    # search for ngrams in parallel
    hansard_words <- foreach(
      m = isplitRows(hansard3, chunks = cores),
      .combine = 'rbind',
      .packages = 'tidytext'
    ) %dopar% {
      unnest_tokens(m, trigram, text, token = "ngrams", n = 3)
    }
    
    print("debates have been tokenized")
    
    # find the appearances of term1
    term_words <- hansard_words %>%
      filter(grepl(trigram1, trigram))
    
    # count the appearances of term1
    term_wordcount <-
      term_words %>% group_by(debate, year, speechdate) %>%
      summarise(wordcount = n()) %>%
      ungroup()
    
    # select just the top appearances for documentation in the table
    top_term_wordcount <- term_wordcount %>%
      top_n(10, wt = wordcount) %>%
      arrange(desc(wordcount))
    
    # generate visual tables listing the debates with the highest count for term1
    setwd(wordcount_data)
    table <- tableGrob(top_term_wordcount)
    title <-
      textGrob(
        paste0(
          "Debates With Highest Wordcounts for '",
          stringr::str_to_title(trigram1),
          "'"
        ),
        gp = gpar(fontsize = 20)
      )
    padding <- unit(0.8, "line")
    footnote <- textGrob(
      "",
      x = 0,
      hjust = 0,
      gp = gpar(fontface = "italic")
    )
    table <- gtable_add_rows(table,
                             heights = grobHeight(title) + padding,
                             pos = 0)
    table <- gtable_add_rows(table,
                             heights = grobHeight(footnote) + padding)
    table <- gtable_add_grob(
      table,
      list(title, footnote),
      t = c(1, nrow(table)),
      l = c(1, 2),
      r = ncol(table)
    )
    g <- grid.newpage()
    g <- grid.draw(table)
    
    # save the tables
    jpg(
      paste0("top-debates-per-term-", trigram1, ".jpg"),
      height = 11,
      width = 8.5,
      paper = "letter"
    )
    print(table)
    grid.draw(table)
    dev.off()
    
    # save a .csv file of term counts for later aggregate visualization
    setwd(wordcount_data)
    write.csv2(top_term_wordcount,
               paste0("top-debates-containing-term-", trigram1, ".csv"))
    
  }
}


#### combine counts of single-tokens and n-grams
all_terms <-
  c(inhabitation_words,
    inhabitation_bigrams,
    inhabitation_trigrams)

# create an empty dataframe and read in term counts saved as .csv's
all_terms_wordcount <- data.frame()
setwd(wordcount_data)

for (i in 1:length(all_terms)) {
  term1 <- all_terms[i]
  if (file.exists(paste0("top-debates-containing-term-", term1, ".csv"))) {
    term1_wordcount <-
      read.table(paste0("top-debates-containing-term-", term1, ".csv")) %>%
      mutate(term = term1)
    all_terms_wordcount <- all_terms_wordcount %>%
      bind_rows(term1_wordcount)
  }
}

term_count <- all_terms_wordcount %>%
  group_by(term, year) %>%
  summarize(count = sum(wordcount))

#### Gather words_per_year data for all relevant years
# so as to count keywords as a percentage of all words spoken that year

# Tokenize all of Hansard
all_hansard_words <- foreach(
  m = isplitRows(hansard2, chunks = cores),
  .combine = 'rbind',
  .packages = 'tidytext'
) %dopar% {
  unnest_tokens(m, trigram, text, token = "ngrams", n = 3)
}

# Count the words for each year in Hansard
all_hansard_wordcounts_by_year <- all_hansard_words %>%
  group_by(year) %>%
  summarise(words_per_yr = n()) %>%
  select(year, words_per_yr)
  
# Calculate the word count as a proportion of all words spoken in a single year
term_proportions <- term_count %>%
  left_join(all_hansard_wordcounts_by_year,  by = "year") %>%
  mutate(proportion = count / words_per_yr)

# Just the top 15 terms
top_terms2 <- term_proportions %>%
  group_by(term) %>%
  summarize(total = sum(proportion)) %>%
  top_n(15) %>%
  left_join(term_proportions) %>%
  mutate(term = toupper(term))

# Create a list of the first appearance of each term; this will be used to annotate the graph
subs2 <- top_terms2 %>%
  mutate(period = year - year %% 10) %>% # floor a year to the nearest n
  group_by(term, period) %>%
  mutate(is_max = (proportion == max(proportion))) %>%
  ungroup() %>%
  filter(is_max == TRUE) %>%
  group_by(term, period) %>%
  mutate(is_first = (year == min(year))) %>%
  ungroup() %>%
  filter(is_first == TRUE) %>%
  distinct() %>%
  select(year, term, proportion) %>%
  mutate(label = TRUE)

top_terms2 <- top_terms2 %>%
  left_join(subs2)

top_terms2$label[is.na(top_terms2$label)] <- FALSE


# a function to help label the Y axis
number_ticks <- function(n) {
  function(limits)
    pretty(limits, n)
}

# Plot FIGURE 2 -- a graph of word counts over time for each keyword
top_terms2 %>% ggplot(aes(
  x = year,
  y = proportion,
  label = ifelse(label == TRUE, term, ""),
  fill = term
)) +
  geom_col() +
  scale_fill_viridis(discrete = TRUE, option = "A") +
  scale_x_continuous(breaks = number_ticks(10)) +
  scale_y_continuous(
    breaks = number_ticks(5),
    labels = function(x)
      paste0(scales::comma(x), "%")  # Spell out exponent and add percent sign
  ) +
  geom_label_repel(
    color = "white",
    fontface = "bold",
    force = 3,
    position = position_stack(vjust = 0.5)
  ) +
  guides(color = FALSE, fill = FALSE) +
  labs(
    y = "word count as a proportion of all words spoken that year",
    x = "Year",
    title = "Property Over Time: Count of OED-Suggested Keywords in Debate Text",
    subtitle = "Searching within the Top 10 Debates Most Relevant to Each Term, Hansard's Parliamentary Debates of Great Britain, 1803-1911",
    caption = "labels are placed at the year of the maximum count for each term per 30-year period"
  )

setwd(vizfolder)
ggsave("figure_2.jpg")
