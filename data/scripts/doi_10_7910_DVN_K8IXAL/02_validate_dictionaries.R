#' ---
#' title: "Catalysts for progress? Mapping policy insights from energy research (ERSS)"
#' subtitle: "02_validate_dictionaries.R"
#' author: "Authors: Brian Boyle, Yen-Chieh Liao, Sarah King, Robin Rauner, and Stefan Müller"
#' ---

# load required R packages
library(quanteda) # CRAN v4.1.0
library(tidyverse) # CRAN v2.0.0
library(rio) # CRAN v1.2.2
library(here) # CRAN v1.0.1
library(mltest) # CRAN v1.0.1
library(tidycomm) # CRAN v0.4.1
library(xtable) # CRAN v1.8-4


# If the code does not run, one or more packages may have been
# updated, which may result in errors or conflicts. You can solve this issue
# by installing the package version listed above or by using the
# groundhog package:
# after installing groundhog using install.packages("groundhog")
# change library(name_of_package) to
# groundhog::groundhog.library(name_of_package, date = "2025-01-01")
# Instead of adjusting the library() function for each package,
# you can adjust them at all once using the
# the following syntax:
# groundhog.library("library('pkgA')
#                   library('pkgB')
#                   library('pkgC')", date = "2025-01-01")
# More details are available at: https://groundhogr.com/using/

# Note: This script requires data containing the text of abstracts.
# Due to Scopus' data-sharing policies, these files cannot be shared publicly.
# Please contact us if you would like to reproduce this script.

# Load custom functions for dictionary optimization
source("dictionary_functions.r")


# Import and prepare data (please contact authors for access to data)
dat_abstracts <- import("data_dontshare/data_abstracts_annotated_ground_truth_dontshare.csv")

dat_validation <- import("data_dontshare/data_abstracts_annotated_held_out_dontshare.csv")

dat_terms <- import("policy_terms.csv")

keyness_terms <- import("keyness_terms_n100.csv")

# Convert to corpus
corp_abstracts <- corpus(dat_abstracts)
corp_validate <- corpus(dat_validation)


# Create a dictionary of validated terms
# Step 1 - Test policy-relevant terms
initial_terms <- dat_terms$term_policy_explicit[
  dat_terms$term_policy_explicit != "" & !is.na(
    dat_terms$term_policy_explicit
  )
]

candidate_terms <- dat_terms$term_policy_related

result_relevant <- optimize_dictionary(
  initial_terms, candidate_terms, corp_abstracts
)
result_relevant
evaluate_dictionary(result_relevant$optimized_dict, corp_abstracts)

# Step 2 - Test keyness terms
best_terms <- unlist(result_relevant$optimized_dict)

key_terms <- keyness_terms$term

result_key <- optimize_dictionary(best_terms, key_terms, corp_abstracts)
result_key
evaluate_dictionary(result_key$optimized_dict, corp_abstracts)


# Prepare output
# Create a dictionary with only explicit policy terms
dict_policy <- dictionary(list(
  policy_statement_dict = initial_terms
))

# Table of dictionary terms
tab_dict_terms <- data.frame(
  Dictionaries = c(
    "Dict 1 (explicit policy terms)",
    paste(dict_policy$policy_statement_dict, collapse = ", "),
    "Dict 2 (+ policy-relevant terms)",
    paste(unlist(result_relevant$optimized_dict), collapse = ", "),
    "Dict 3 (+ keyness terms)",
    paste(unlist(result_key$optimized_dict), collapse = ", ")
  )
)
tab_dict_terms

tab_a2 <- xtable(
  tab_dict_terms,
  caption = "List of dictionary terms",
  label = "tab:dict_terms",
  align = rep("p{12cm}", ncol(tab_dict_terms) + 1)
)
tab_a2

print(tab_a2,
  file = "tab_A2.tex",
  type = "latex", booktabs = TRUE,
  include.rownames = FALSE,
  caption.placement = "top"
)

# The .tex file of Table A2 should be manually edited as follows:
# remove "Dictionaries \\ \midrule"
# add "\midrule" to separate Dict 1, Dict 2, and Dict 3

print(tab_a2,
  file = "tab_A2.html",
  type = "html",
  include.rownames = FALSE,
  include.colnames = FALSE,
  caption.placement = "top"
)

# Performance metrics tables
# Ground truth set
tab_ground_truth <- make_table(
  dict_policy, result_relevant, result_key, corp_abstracts
)
tab_ground_truth

# Inspect table which will be saved as tab_A3.tex and tab_A3.html
tab_ground_truth

tab_a3 <- xtable(tab_ground_truth,
  caption = "Dictionary performance on ground truth set (n=400)",
  label = "tab:ground_truth",
  digits = 2,
  align = rep("r", ncol(tab_ground_truth) + 1)
)

print(tab_a3,
  file = "tab_A3.tex",
  type = "latex", booktabs = TRUE,
  caption.placement = "top"
)

print(tab_a3,
  file = "tab_A3.html",
  type = "html",
  caption.placement = "top"
)

# Held-out validation set
tab_held_out <- make_table(
  dict_policy, result_relevant, result_key, corp_validate
)

# Inspect table which will be saved as tab_A4.tex and tab_A4.html
tab_held_out

tab_a4 <- xtable(tab_held_out,
  caption = "Dictionary performance on held-out set (n=200)",
  label = "tab:held_out",
  digits = 2,
  align = rep("r", ncol(tab_ground_truth) + 1)
)


print(tab_a4,
  file = "tab_A4.tex",
  type = "latex", booktabs = TRUE,
  caption.placement = "top"
)

print(tab_a4,
  file = "tab_A4.html",
  type = "html",
  caption.placement = "top"
)


## Print sessionInfo() in Markdown html report
sessionInfo()
