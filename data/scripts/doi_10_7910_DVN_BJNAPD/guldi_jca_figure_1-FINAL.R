################################
## FIGURE 1 & Various Tables
#
# Keyword Count Visualizations to accompany
# Jo Guldi, "Critical Search," Journal of Cultural Analytics (2018)
#
# This code generates tables listing the debates that have the highest counts
# of particular keywords.
# It also plots the keywords over time.

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
library(parallel)
library(doParallel)
library(scales)
library(viridis)

# slower machines may change these parameters to downsample the data
firstyear <- 1803
lastyear <- 1911

# folder locations -- modify to match your own file locations
datafolder <- "~/Box Sync/#learningtocode/data/hansard_data"
vizfolder <- "~/Box Sync/#learningtocode/visualizations"
wordcount_visualizations <-
  "~/Box Sync/#learningtocode/visualizations/wordcount_tables_fig_1"
wordcount_data <-
  "~/Box Sync/#learningtocode/data/wordcount_tables_fig_1"

##### read in Hansard
print("reading in hansard...")
setwd(datafolder)
hansard <-
  read.csv(
    "member_names_mispelled_words.tsv",
    sep = "\t",
    header = FALSE,
    stringsAsFactors = FALSE
  )
print("hansard read.")

# create names for variables
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
  as.character(hansard2$text) # make the text column to be of the class character (instead of factor)
hansard2$debate <-
  as.character(hansard2$debate) # make the text column to be of the class character (instead of factor)
hansard2$speechdate <- ymd(hansard2$speechdate)
hansard2$year <- year(hansard2$speechdate)

# In this exercise, we are just looking at the words in the titles.
# We therefore select the "debate" variable (the title of the debate)
all_titles <- hansard2 %>%
  select(debate, year, speechdate) %>%
  group_by(year, speechdate) %>% # find all unique debate titles within each year
  distinct(debate) %>%
  filter(!nchar(debate) > 200) %>% # get rid of errors where the debate title was skipped for the text
  mutate(debate = toupper(debate)) # make everything upper case

# Figure 1 searches for keywords supplied by the author.
terms <-
  c(
    "croft",
    "ejectment",
    "enclosure",
    "estate",
    "evict",
    "inclosure",
    "landlord",
    "landown",
    "propert",
    " rent",
    "tenant",
    "tenure"
  )
terms <- toupper(terms)

# We create an empty data frame and fill it with information about the counts of words
# per year for each term.  The loop cycles through the keywords in the list "terms,"
# counting each one and drawing a table naming the top debates.
all_year_counts <- data.frame()

for (i in 1:length(terms)) {
  term1 <- terms[i]
  
  # Find the debates with term1 in the title
  debate_titles <- all_titles %>%
    filter(grepl(term1, debate))
  
  # Calculate the periodization of each debate related to term1: for for each unique
  # debate title containing term1, when was the first debate of that title and the last?
  # Over how many days was that title debated?
  debate_length <- debate_titles %>%
    group_by(debate) %>%
    mutate(begins = min(speechdate)) %>%
    mutate(ends = max(speechdate)) %>%
    mutate(length = ends - begins) %>%
    distinct(debate, begins, ends, length)
  
  # Calculate the episode count: for each unique debate title containing term1,
  # on how many unique dates did that debate take place?
  episode_count <- debate_titles %>%
    group_by(debate) %>%
    summarise(episodes = n()) %>%
    ungroup() %>%
    filter(episodes > 1) %>%
    arrange(desc(episodes)) %>%
    rename(times_debated = episodes) %>%
    left_join(debate_length, by = "debate")
  
  # Tally the year count: in what years were there debates with term1 in the title?
  year_count <- debate_titles %>%
    group_by(year) %>%
    summarise(debates_per_year = n()) %>%
    arrange(year) %>%
    mutate(term = term1)
  
  # Add the year_count for term1 to the ongoing list of years in which each term
  # was debated.
  all_year_counts <- all_year_counts %>%
    bind_rows(year_count)
  
  # Making tables for the above tallies.
  num_years <- nrow(episode_count)
  if (num_years > 10) {
    #  The tables only show the top 10 debate titles for term1.
    episode_count <- episode_count %>%
      top_n(10, wt = times_debated)
  }
  if (num_years > 1) {
    # skip if there are no occurrences
    mytheme <-
      ttheme_default(
        base_size = 10,
        base_colour = "black",
        base_family = "",
        parse = FALSE,
        padding = unit(c(4, 4), "mm")
      )
    
    # Parameters for a table showing the episode count.
    table <- tableGrob(episode_count, theme = mytheme)
    title <-
      textGrob(
        paste0(
          "Frequent Subjects Whose Title Includes the Term '",
          stringr::str_to_title(term1),
          "'"
        ),
        gp = gpar(fontsize = 12)
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
    
    # adjusting table width to fit all the words
    missed <-
      convertWidth(sum(table$widths), "in", valueOnly = TRUE) -
      convertWidth(grobWidth(title), "in", valueOnly = TRUE)
    if (missed < 0)
      table$widths <-
      table$widths + unit(abs(missed) / ncol(table), "in")
    
    # Drawing the table.
    g <- grid.newpage()
    g <- grid.draw(table)
    
    setwd(wordcount_visualizations)
    
    # Saving the Table.
    h = grid::convertHeight(sum(table$heights), "in", TRUE)
    w = grid::convertWidth(sum(table$widths), "in", TRUE)
    png(
      paste0("top-debates-per-term-", term1, ".png"),
      res = 300,
      height = h,
      width = w,
      units = "in"
    )
    print(table)
    grid.draw(table)
    dev.off()
    
    # Your data drive goes here
    setwd(wordcount_data)
    
    # Saving the data as a .csv for reading later.
    write.csv2(debate_titles,
               paste0("debate-titles-containing-term-", term1, ".csv"))
  }
  
}


#### Create a master term count.  Read in year_count for all terms.

# Your data drive goes here
setwd(wordcount_data)

# Filter out any badly labeled data.
all_year_counts2 <- all_year_counts %>% filter(year < 1911)

# Count the number of debates per year in Hansard overall so that we can
all_title_words <-
  unnest_tokens(all_titles, term, debate, token = "words")

all_title_wordcounts <- all_title_words %>%
  ungroup() %>%
  select(year, term) %>%
  count(year) %>%
  ungroup()

# Calculate the appearance of each term per year as a percentage.
all_year_counts3 <- all_year_counts2 %>%
  left_join(all_title_wordcounts) %>%
  mutate(proportion = debates_per_year / n) %>%
  mutate(term = stringr::str_trim(term))


# Create a list of the first appearance of each term; this will be used to annotate the graph
subs1 <- all_year_counts3 %>%
  mutate(period = year - year %% 30) %>% # floor a year to the nearest n
  group_by(term, period) %>%
  mutate(is_max = (proportion == max(proportion))) %>%
  ungroup() %>%
  filter(is_max == TRUE) %>%
  group_by(term, period) %>%
  mutate(is_first = (year == min(year))) %>%
  ungroup() %>%
  filter(is_first == TRUE)


# a function to help label the Y axis
number_ticks <- function(n) {
  function(limits)
    pretty(limits, n)
}


# Create FIGURE 1.
ggplot(all_year_counts3, aes(x = year, y = proportion,
                             fill = term)) +
  geom_col() +
  scale_fill_viridis(discrete = TRUE, option = "A") +
  scale_x_continuous(breaks = number_ticks(10)) +
  scale_y_continuous(
    breaks = number_ticks(5),
    labels = function(x)
      paste0(scales::comma(x), "%")
  ) + # Add percent sign
  guides(fill = FALSE) +
  geom_label_repel(data = subs1,
                   aes(label = term),
                   color = "white",
                   fontface = "bold") +
  labs(
    y = "count of terms in debate titles (as proportion of all title words for each year)",
    title = "Property Over Time: Count of Author-Generated Keywords in Debate Titles",
    subtitle = "Searching Hansard's Parliamentary Debates of Great Britain, 1803-1911",
    caption = "labels are placed at the year of the maximum count for each term per each 30 year segment"
  )


setwd(vizfolder)
ggsave("figure_1.jpg")
