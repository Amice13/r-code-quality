#' ---
#' This script returns the statistics mentioned in the manuscript of:
#' title: "Catalysts for progress? Mapping policy insights from energy research (ERSS)"
#' subtitle: "06_check_stats.R"
#' author: "Authors: Brian Boyle, Yen-Chieh Liao, Sarah King, Robin Rauner, and Stefan Müller"
#' ---

# Note: This script requires data containing the text of abstracts.
# Due to Scopus' data-sharing policies, these files cannot be shared publicly.
# Please contact us if you would like to reproduce this script.

# load required R packages
library(quanteda) # CRAN v4.1.0
library(tidyverse) # CRAN v2.0.0
library(rio) # CRAN v1.2.2
library(newsmap) # CRAN v0.9.0
library(tidycomm) # CRAN v0.4.1
library(mltest) # CRAN v1.0.1


# load pre-filtering corpus
dat_corpus <- import("data_dontshare/data_scopus_1_100_2010_2023_dontshare.rds")
dim(dat_corpus) # 461,894 rows

# load classified abstracts
dat_classified <- import(
    "data_dontshare/data_scopus_1_100_2010_2023_classified_dontshare.rds"
)
dim(dat_classified) # 270,537 rows


## Percentage of abstracts containing policy-relevant statements
mean(dat_classified$policy_statement) # 0.149584
sum(dat_classified$policy_statement) / nrow(dat_classified) # 0.149584


## Average policy relevance by region
# Create author continent variables
dict_newsmap <- newsmap::data_dictionary_newsmap_en

# Create new objects for the continent dictionaries
# Inspect and remove abbreviations and short names that could return mismatches

africa <- dict_newsmap[1] |>
    unlist() |>
    as.character()
africa <- africa[!africa %in% c("sa")]
africa <- c(africa, "cote d'ivoire")

americas <- dict_newsmap[2] |>
    unlist() |>
    as.character()
americas <- americas[!americas %in% c("us", "lima", "rio")]

asia <- dict_newsmap[3] |>
    unlist() |>
    as.character()
asia <- asia[!asia %in% c("omani*", "oman")]

europe <- dict_newsmap[4] |>
    unlist() |>
    as.character()
europe <- europe[!europe %in% c("uk", "rome", "riga", "pole*")]
europe <- c(europe, "faroe islands")

oceania <- dict_newsmap[5] |>
    unlist() |>
    as.character()
oceania <- oceania[!oceania %in% c("nz", "oz", "apia")]




dat_classified_region <- dat_classified |>
    mutate(
        africa = as.numeric(grepl(paste0(africa, collapse = "|"),
                                  dat_classified$affil_country,
            ignore.case = TRUE
        )),
        americas = as.numeric(grepl(paste0(americas, collapse = "|"),
                                    dat_classified$affil_country,
            ignore.case = TRUE
        )),
        asia = as.numeric(grepl(paste0(asia, collapse = "|"),
                                dat_classified$affil_country,
            ignore.case = TRUE
        )),
        europe = as.numeric(grepl(paste0(europe, collapse = "|"),
                                  dat_classified$affil_country,
            ignore.case = TRUE
        )),
        oceania = as.numeric(grepl(paste0(oceania, collapse = "|"),
                                   dat_classified$affil_country,
            ignore.case = TRUE
        ))
    )


# Manually run additional search for africa dictionary
# Require exact case sensitive match for 'Oman' to avoid false positives
dat_classified_region$africa[str_detect(dat_classified_region$affil_country, "Oman")] <- 1


# Number of continents all article authors affiliated with
dat_classified_region <- dat_classified_region |>
    mutate(
        n_continents = rowSums(
            dat_classified_region[, c("africa", "americas", "asia", "europe", "oceania")]
        ),
        affil_continent = case_when(
            n_continents > 1 ~ "Intercontinental",
            africa == 1 ~ "Africa",
            americas == 1 ~ "Americas",
            asia == 1 ~ "Asia",
            europe == 1 ~ "Europe",
            oceania == 1 ~ "Oceania"
        )
    )

policy_region <- dat_classified_region |>
    group_by(affil_continent) |>
    summarise(
        mean_policy = round(mean(policy_statement) * 100),
        n = n()
    )
policy_region

## check for missing values
dat_classified_region |>
    filter(is.na(affil_continent)) |>
    nrow()


## Get the fraction of articles from Oceania
region_counts <- dat_classified_region |>
    filter(!is.na(affil_continent)) |>
    count(affil_continent) |>
    mutate(percentage = round(n / sum(n) * 100, 2))
region_counts


## Get abstract-full text coding agreement
# load full text codings
dat_fulltexts <- import("data_dontshare/data_fulltexts_annotated_dontshare.csv")
dim(dat_fulltexts) # 50

# cross-table of hand-coding for abstracts and full texts
table(
    abstract = dat_fulltexts$policy_statement_abstract,
    `full text` = dat_fulltexts$policy_statement_fulltext
)

# percentage of true positives & true negatives
45 / nrow(dat_fulltexts) # 0.9


## Check the prevalence of abstracts that mention "climate" keywords
# filter for abstracts that contain climate keywords
climate_keywords <- c("climate change", "climate crisis", "global warming")

dat_climate <- dat_classified |>
    filter(str_detect(tolower(text), paste(climate_keywords, collapse = "|")))
print(head(dat_climate))
dim(dat_climate) # 12,969 rows

round(nrow(dat_climate) / nrow(dat_classified) * 100, 0) # get the percentage


## Check the prevalence of abstracts that mention "policy"/"policies"
policy_keywords <- c("policy", "policies")

dat_policy <- dat_classified |>
    filter(str_detect(tolower(text), paste(policy_keywords, collapse = "|")))
print(head(dat_policy))
dim(dat_policy) # 22,900 rows

round(nrow(dat_policy) / nrow(dat_classified) * 100) # get the percentage


## Check inter-coder reliability of double-coded set
# load gold-standard set
dat_annotated <- import("data_dontshare/data_abstracts_annotated_ground_truth_dontshare.csv")

# convert to long format
dat_annotated_long <- dat_annotated |>
    pivot_longer(
        cols = starts_with("policy_statement"),
        names_to = "coder",
        values_to = "policy_statement"
    )
print(head(dat_annotated_long))
unique(dat_annotated_long$coder)

# calculate inter-coder reliability
icr_metrics <- dat_annotated_long |>
    test_icr(article_scopus_id, coder, policy_statement)

icr_metrics$Krippendorffs_Alpha # 0.7507738


## Check the classification performance metrics
# load the held-out validation set
dat_validation <- import("data_dontshare/data_abstracts_annotated_held_out_dontshare.csv")

# create policy dictionary
policy_terms <- list(c(
    "policy", "policies", "policymak*", "policy-mak*", "regulator",
    "regulators", "legislat*", "government*", "politic*", "subsid*",
    "agenc*", "national", "social", "tax*", "decision-mak*"
))

dict_policy <- dictionary(list(policy_statement = policy_terms))


# function to apply dictionary analysis
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

# apply dictionary to gold standard and validation sets
dat_gold_dict <- apply_dictionary(dict_policy, dat_annotated)

dat_validation_dict <- apply_dictionary(dict_policy, dat_validation)

# calculate performance metrics
ml_gold <- ml_test(
    predicted = dat_gold_dict$policy_statement,
    true = dat_gold_dict$true_policy_statement
)

ml_validation <- ml_test(
    predicted = dat_validation_dict$policy_statement,
    true = dat_validation_dict$true_policy_statement
)

round(mean(ml_gold$F1), 2) # 0.9
round(mean(ml_validation$F1), 2) # 0.86

# convert to long format
dat_gold_long <- dat_gold_dict |>
    pivot_longer(
        cols = c("true_policy_statement", "policy_statement"),
        names_to = "coder",
        values_to = "coded_policy"
    )

dat_validation_long <- dat_validation_dict |>
    pivot_longer(
        cols = c("true_policy_statement", "policy_statement"),
        names_to = "coder",
        values_to = "coded_policy"
    )

# calculate inter-coder reliability metrics
icr_metrics_gold <- dat_gold_long |>
    test_icr(article_scopus_id, coder, coded_policy)

icr_metrics_validation <- dat_validation_long |>
    test_icr(article_scopus_id, coder, coded_policy)

round(icr_metrics_gold$Krippendorffs_Alpha, 2) # 0.81
round(icr_metrics_validation$Krippendorffs_Alpha, 2) # 0.73


## Check number of policy_statement == 1
sum(dat_classified$policy_statement) # 40,468


## Check percentage of Asia in Intercontinental
prop_asia <- dat_classified_region |>
    filter(affil_continent == "Intercontinental" & asia == 1) |>
    nrow() / nrow(
        dat_classified_region[
            dat_classified_region$affil_continent == "Intercontinental",
        ]
    )
prop_asia # 77%


## Check the proportion of journals that began publishing after 2013
# load journal statistics
journal_stats <- import("journal_aims.csv")

# extract years from journal_coverage
journal_stats <- journal_stats |>
    separate(journal_coverage,
        into = paste0("year_", 1:4), sep = "[,\\-]", fill = "right"
    ) |>
    mutate(year_1 = as.numeric(year_1))

# get the number of journals where year_1 > 2013
n_journals_after_2013 <- journal_stats |>
    filter(year_1 > 2013) |>
    nrow()

n_journals_after_2013 / nrow(journal_stats) # 0.33


## Check policy relevance in journals with policy in aims and scope
journal_policy_stats <- dat_classified |>
    group_by(journal_name) |>
    summarise(
        policy_relevance = round(mean(policy_statement), 2) * 100
    )

# merge journal_aims with classified corpus
journal_policy_stats <- journal_policy_stats |>
    mutate(journal_name_lower = tolower(journal_name)) |>
    left_join(
        journal_stats |>
            select(c(journal_name_lower, journal_policy_aims)),
        by = "journal_name_lower"
    )

prop_low_policy_relevance <- journal_policy_stats |>
    filter(journal_policy_aims == 1 & policy_relevance < 15) |>
    nrow() / nrow(
        journal_policy_stats[journal_policy_stats$journal_policy_aims == 1, ]
    )

prop_low_policy_relevance # 0.3513514

# 38 journals have policy in aims and scope
table(journal_policy_stats$journal_policy_aims)

# inspect policy in aims and scope subset
journal_policy_stats_1 <- journal_policy_stats |>
    filter(journal_policy_aims == 1)
journal_policy_stats_1


## Check the range in yearly publications for Oceania
region_counts_year <- dat_classified_region |>
    filter(!is.na(affil_continent)) |>
    count(affil_continent, article_year) |>
    filter(affil_continent == "Oceania") |>
    arrange(n)
region_counts_year
