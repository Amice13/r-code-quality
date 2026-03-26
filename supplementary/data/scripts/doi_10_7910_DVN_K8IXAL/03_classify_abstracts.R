#' ---
#' title: "Catalysts for progress? Mapping policy insights from energy research (ERSS)"
#' subtitle: "03_classify_abstracts.R"
#' author: "Authors: Brian Boyle, Yen-Chieh Liao, Sarah King, Robin Rauner, and Stefan Müller"
#' ---

# load required R packages
library(quanteda) # CRAN v4.1.0
library(tidyverse) # CRAN v2.0.0
library(rio) # CRAN v1.2.2


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

# Function to apply dictionary analysis
apply_dictionary <- function(dictionary, data) {
  # Convert to corpus
  corp_abstracts <- corpus(data)

  # Tokenize and process
  toks <- tokens(corp_abstracts,
    remove_numbers = TRUE,
    remove_punct = TRUE,
    split_hyphens = FALSE,
    padding = TRUE
  ) |>
    tokens_tolower()

  # Apply policy dictionary
  dfmat <- tokens_lookup(toks, dictionary = dictionary) |> dfm()
  dfmat_weighted <- dfm_weight(dfmat, scheme = c("boolean"))
  dat_dict <- quanteda::convert(dfmat_weighted, to = "data.frame") |>
    cbind(docvars(dfmat_weighted))

  # Merge with text
  dat_dict_text <- dat_dict |>
    left_join(data |> select(c(article_scopus_id, text)),
      by = "article_scopus_id"
    )

  return(dat_dict_text)
}

# Import and prepare data
# Text corpus: 461,894 abstracts
dat_abstracts <- readRDS("data_dontshare/data_scopus_1_100_2010_2023_dontshare.rds") |>
  rename(text = article_abstract) |>
  filter(!is.na(text))

# get number of rows and columns
dim(dat_abstracts)

energy_terms <- list(c(
  "climate change", "climate crisis", "global warming",
  "net zero", "climate neutral",
  "co2", "emissions", "renewable*", "energy",
  "carbon*", "ecological", "footprint*",
  "environment*"
))

policy_terms <- list(c(
  "policy", "policies", "policymak*", "policy-mak*", "regulator",
  "regulators", "legislat*", "government*", "politic*", "subsid*",
  "agenc*", "national", "social", "tax*", "decision-mak*"
))


# Create dictionaries
dict_energy <- dictionary(list(energy_relevant = energy_terms))

dict_energy

dict_policy <- dictionary(list(policy_statement = policy_terms))

dict_policy

# Apply energy dictionary
dat_dict_energy <- apply_dictionary(dict_energy, dat_abstracts)

# Inspect classified data
table(dat_dict_energy$energy_relevant) # 270,537 energy-relevant abstracts

# Subset data
dat_energy_subset <- dat_dict_energy |>
  filter(energy_relevant == 1)

# Check that subset contains 270,537 rows
dim(dat_energy_subset)

# Apply policy dictionary to energy subset
dat_dict_policy <- apply_dictionary(dict_policy, dat_energy_subset)

# Inspect classified data
table(dat_dict_policy$policy_statement) # 40,468 policy-relevant abstracts
round(mean(dat_dict_policy$policy_statement), 2) * 100 # or %15 of abstracts
names(dat_dict_policy)

# Save classified data with abstract text
saveRDS(
  dat_dict_policy,
  "data_dontshare/data_scopus_1_100_2010_2023_classified_dontshare.rds"
)

names(dat_dict_policy)

# Keep only columns used for analysis, and comply with data-sharing requirements
dat_dict_policy_public <- dat_dict_policy |>
  select(
    doc_id, policy_statement, energy_relevant,
    journal_name,
    journal_vol,
    journal_issn,
    journal_pages,
    article_title,
    article_year,
    article_doi,
    article_scopus_id,
    author_count,
    author_name,
    affil_country,
    starts_with("publication_type"),
    fund_sponsor,
    rank,
    asjc_code_first,
    asjc_cat_first, asjc_area_code,
    asjc_area, asjc_field,
    api_call_date
  )

names(dat_dict_policy_public)

# Save classified data without abstract text
saveRDS(
  dat_dict_policy_public,
  "data_scopus_1_100_2010_2023_classified.rds"
)


## Print sessionInfo() in Markdown html report
sessionInfo()
