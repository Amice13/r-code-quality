# Code for cleaning DS2 Synthese Data for revisions. 

library(dplyr)
library(tm)
library(stringr)


setwd("C:/Users/mad4/Desktop/synthese_journals")

# journals = read.csv("journals_filtered.csv")

ocr1 = readtext("C:/Users/mdick/Desktop/ocr_1")
ocr2 = readtext("C:/Users/mdick/Desktop/ocr_2")
ocr3 = readtext("C:/Users/mdick/Desktop/ocr_3")
ocr4 = readtext("C:/Users/mdick/Desktop/ocr_4")

# stop_words = read.csv("stop_words.csv")
# stem_terms = read.csv("stem_terms.csv")


# -----------------------------------------------

# Remove the 'X' column - not needed

 # journals$X = NULL

# -------------------------------------------------

# lower the text.x column (fulltext)

# journals$text.x = tolower(journals$text.x)

# glimpse(journals)

# -------------------------------------------------

# Remove Stop Words

# journals$text.x = removeWords(journals$text.x, stop_words$stop_words)

# journals$text.x[1]

# ---------------------------------------------------

# Load in clean, un-stemmed data: 

journals = read.csv("ready_for_stems.csv")

 
journals$text.x[1]

# ------------------------------------------------------

# Remove (most) HTML from text.x

# journals$text.x = str_remove_all(journals$text.x, "</page>")

# journals$text.x = str_remove_all(journals$text.x, "<page>")

# journals$text.x = str_remove_all(journals$text.x, "</plain_text>")

# journals$text.x = str_remove_all(journals$text.x, "<plain_text>")

# journals$text.x = str_remove_all(journals$text.x, regex('<page sequence=\"\\d\">'))

# journals$text.x = str_remove_all(journals$text.x, regex('<page sequence=\"\\d\\d\">'))

# journals$text.x = str_remove_all(journals$text.x, regex('<page sequence=\"\\d\\d\\d\">'))

# journals$text.x[1]

# --------------------------------------------------------------------------------

# Write a file for the stops_html_rem data

#write.csv(journals, "stops_html_rem_not_stem.csv")



# -------------------------------------------------------

# Root words to possibly stem (We may not need to stem these words): 



journals$text.x = journals$text.x %>% 
  str_replace_all("prove" , "proves")

journals$text.x = journals$text.x %>% 
  str_replace_all("proved" , "proves")

journals$text.x = journals$text.x %>% 
  str_replace_all("proving" , "proves")
 
journals$text.x = journals$text.x %>% 
 str_replace_all("follow" , "follows")

journals$text.x = journals$text.x %>% 
  str_replace_all("followed" , "follows")

journals$text.x = journals$text.x %>% 
  str_replace_all("following" , "follows")

journals$text.x = journals$text.x %>% 
 str_replace_all("infers" , "infer")

journals$text.x = journals$text.x %>% 
  str_replace_all("inferred" , "infer")

journals$text.x = journals$text.x %>% 
  str_replace_all("inferring" , "infer")

journals$text.x = journals$text.x %>% 
  str_replace_all("concludes", "conclude")

journals$text.x = journals$text.x %>% 
  str_replace_all("concluded", "conclude")

journals$text.x = journals$text.x %>% 
  str_replace_all("concluding", "conclude")

# ----------------------------------------------

# Anchor Terms to Stem:

journals$text.x = journals$text.x %>% 
  str_replace_all("necessary" , "necessarily")

journals$text.x = journals$text.x %>% 
  str_replace_all("certain" , "certainly")

journals$text.x = journals$text.x %>% 
  str_replace_all("definite" , "definitely")

journals$text.x = journals$text.x %>% 
  str_replace_all("absolute" , "absolutely")

journals$text.x = journals$text.x %>% 
  str_replace_all("probable" , "probably")

journals$text.x = journals$text.x %>% 
  str_replace_all("improbably" , "improbable")

journals$text.x = journals$text.x %>% 
  str_replace_all("likelihood" , "likely")

journals$text.x = journals$text.x %>%  
  str_replace_all("unlikelihood" , "unlikely")

journals$text.x = journals$text.x %>% 
  str_replace_all("accounts" , "account")

journals$text.x = journals$text.x %>% 
  str_replace_all("accounted" , "account")

journals$text.x = journals$text.x %>% 
  str_replace_all("accounting" , "account")

journals$text.x = journals$text.x %>% 
  str_replace_all("accountable" , "account")

journals$text.x = journals$text.x %>% 
  str_replace_all(" explians " , "explain")

journals$text.x = journals$text.x %>% 
  str_replace_all("explained" , "explain")

journals$text.x = journals$text.x %>% 
  str_replace_all("explaining" , "explain")

journals$text.x = journals$text.x %>% 
  str_replace_all("explainable" , "explain")

journals$text.x = journals$text.x %>% 
  str_replace_all("makes" , "make")

journals$text.x = journals$text.x %>% 
  str_replace_all("making" , "make")

journals$text.x = journals$text.x %>% 
  str_replace_all("made" , "make")

# ------------------------------------------------

journals = read.csv("ready_to_search.csv")

# ------------------------------------------

# Other Words - may not have any stemmed words
# Patterns must be an exact match - so multi-token anchors like "best explains"
# must appear together: 

# test_string = "This theory best explains it all"

# str_detect(test_string, "best explains") # TRUE 

# test_string_2 = "this theary explains it all, it is the best solution"

# str_detect(test_string_2, "best explains") # FALSE

# ----------------------------------------------------

test_stem = str_detect(journals$text.x, "certain")

test_stem_convert = as.character(test_stem) %>% 
  str_replace("TRUE", "1") %>% 
  str_replace("FALSE", "0") %>%
  as.numeric()

test_stem_true = cbind(journals, test_stem_convert)

test_stem_filtered = test_stem_true %>% 
  filter(test_stem_convert == 1)

glimpse(test_stem_filtered)

# ----------------------------------------

# Testing the str_replace_all() function

# important: if a word is a part of another word, it will return TRUE

test_1 = c("makes", "makes", "made", "make", "making", " tulmakes") # T, F, F, F, T

str_detect(test_1, "makes")

test_replace = str_replace(test_1, "makes|made|making", "make")
test_replace

str_detect(test_replace, "makes")


test_2 = "While they were explaining the facts, that they hand't explained it at all!"

test_replace_2 = str_replace_all(test_2, " explaining ", "explain")

test_replace_2

# -----------------------------------------

# Tests to format data

# Remove Stop Words from the fulltext (text.x) - TEST

# sw = stopwords("en")

# write.csv(sw, "stop_words.csv")

# test_1 = "She hadn't thought it out before. All such things were less important"

# test_remove = removeWords(test_1, stopwords("en"))

# test_remove

# --------------------------------------------------------

# Experiment with stem_strings() and lemmatize_strings() -Test

# stemmed = stem_strings(stem_terms$indicator_terms)

# lemmed = lemmatize_strings(stem_terms$indicator_terms)


# stemmed
# lemmed # Neither is  a great option

# -----------------------------------------------------------

# Replace stems by hand - Test:

# ex_test = c("explains", "explained", "explaining")

# explain = ex_test %>% 
#  str_replace_all("explains|explained|explaining", "explain")

# explain

# -----------------------------------------------------------

# Remove html tags from text

# test_string = '<page sequence=\"13\">'

# test_rem = str_remove_all(test_string, '<page sequence=\"\\d\\d\">')
# test_rem

# ------------------------------------------------------------------

# Extract article_title from metadata column: 

journals$text.y[1]

article_test = journals$text.y %>% 
  str_extract(">(.*?)</article-title")

head(article_test)



article_title = article_test %>% 
  str_remove_all("</article-title") %>% 
  str_remove_all(">")

article_title = data.frame(article_title, stringsAsFactors = FALSE)

head(article_title, n = 100 )

# Bind the article_title column to the journals object

journals = cbind(journals, article_title)

glimpse(journals)

# Save the file: stops_html_article_title_not_stem

# write.csv(journals, "ready_for_stems.csv")

# ----------------------------------------------------------------

