#' ---
#' title: "Legislating Landlords: Private Interests, Issue Emphasis, and Policy Positions (LSQ)"
#' subtitle: "02a_data_prep.R"
#' author: "Authors: Stefan Mueller and Jihed Ncib"
#' date: "Note: Code compiled successfully on `r format(Sys.time(), '%d %B %Y')`"
#' ---

# script to merge and harmonise the text corpora and datasets on legislators
# and their interests

# If the code does not run, one or more packages may have been 
# updated, which may result in errors or conflicts. You can solve this issue
# by installing the package version listed above or by using the 
# groundhog package:
# after installing groundhog using install.packages("groundhog")
# change library(name_of_package) to
# groundhog::groundhog.library(name_of_package, date = "2024-05-06")
# Instead of adjusting the library() function for each package, 
# you can adjust them at all once using the
# the following syntax:
# groundhog.library("library('pkgA')
#                   library('pkgB')
#                   library('pkgC')", date = "2024-05-06")
# More details are available at: https://groundhogr.com/using/

library(dplyr) # CRAN v1.1.2
library(arrow) # CRAN v14.0.0.2

training = read_parquet("data_dontshare/training.parquet")

train = training[sample(nrow(training), 600, replace = FALSE),]
training = training %>% filter(!doc_id %in% train$doc_id)
eval = training[sample(nrow(training), 200, replace = FALSE),]
test = training %>% filter(!doc_id %in% eval$doc_id)

# write_parquet(train, "data_dontshare/train.parquet")
# write_parquet(eval, "data_dontshare/eval.parquet")
# write_parquet(test, "data_dontshare/test.parquet")
