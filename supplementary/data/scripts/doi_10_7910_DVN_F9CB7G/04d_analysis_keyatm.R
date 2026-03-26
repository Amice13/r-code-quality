#' ---
#' title: "Legislating Landlords: Private Interests, Issue Emphasis, and Policy Positions (LSQ)"
#' subtitle: "04d_analysis_keyatm.R"
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



# note: parts of the analysis take very long with quanteda v.4.0.2
# We recommend installing quanteda 3.3.1 for this analysis

# library(remotes)
# install_version("quanteda", version = "3.3.1", repos = "http://cran.us.r-project.org")


library(tidyverse)           # CRAN v2.0.0
library(quanteda)            # CRAN v3.3.1 # do not use quanteda v4.0.2
library(quanteda.textstats)  # CRAN v0.96.3
library(quanteda.textmodels) # CRAN v0.9.6
library(arrow)               # CRAN v14.0.0.2
library(clarify)             # CRAN v0.2.0
library(fixest)              # CRAN v0.11.2
library(xtable)              # CRAN v1.8-4
library(scales)              # CRAN v1.2.1
library(keyATM)              # CRAN v0.5.0

source("function_theme_base.R")

## prepare scaling analysis
# read parquet file
dat_analysis <- read_parquet("data_dontshare/data_analysis_classified_housing.parquet")

# make housing_bert an integer
dat_analysis <- dat_analysis |> 
    mutate(housing_bert = dplyr::recode(housing_bert, "Housing" = 1, 
                                        "Non-housing" = 0)) |> 
    mutate(housing_bert = as.integer(housing_bert))

table(dat_analysis$housing_bert)


dat_analysis <- dat_analysis |> 
    mutate(age = as.integer(year) - as.integer(birth)) |> 
    mutate(seniority_years = as.integer(seniority) / 365) 


dat_analysis <- dat_analysis |> 
    mutate(cycle = case_when(
        year %in% 2011:2015 ~ "2013-2016",
        year %in% 2016:2019 ~ "2016-2020",
        year %in% 2020:2025 ~ "2020-2022"
    ))


# create doc_id

dat_analysis <- dat_analysis |> 
    group_by(mpname, year, type) |> 
    mutate(document_id = paste(mpname,  year, type, 1:n())) |> 
    select(document_id, everything())



# subset data

dat_analysis$year <- as.integer(dat_analysis$year)

dat_analysis_subset <- filter(dat_analysis, year %in% 2016:2022) |> 
    group_by(year, harmonised) |> 
    mutate(count_texts = n()) |> 
    filter(count_texts >= 2) |>  # keep only TDs with at least two texts per year 
    ungroup()

# assign housing ministers to reference texts (=+1)
# https://en.wikipedia.org/wiki/Minister_for_Housing,_Local_Government_and_Heritage


dat_analysis_subset <- dat_analysis_subset |> 
    mutate(housing_pos = ifelse(harmonised == "murphy eoghan" & year %in% 2016:2019, 1,
                                ifelse(harmonised == "coveney simon" & year %in% 2016:2019, 1, 
                                       housing_pos)))


dat_analysis_subset <- dat_analysis_subset |> 
    mutate(housing_pos = ifelse(harmonised == "o'brien darragh" & year > 2019, 1, housing_pos))

dat_reftexts <- dat_analysis_subset |> 
    filter(!is.na(housing_pos)) |> 
    dplyr::select(harmonised, candidate, constituency_name, party, housing_pos) |> 
    unique()

table(dat_analysis_subset$housing_pos,
      dat_analysis_subset$year)

# filter only housing content
dat_analysis_subset_housing <- dat_analysis_subset |> 
    filter(housing_bert == 1) 

# number of housing-related tweets and questions
nrow(dat_analysis_subset_housing)

table(dat_analysis_subset_housing$harmonised)

table(dat_analysis_subset_housing$housing_pos, useNA = "always")

dat_ref_texts <- dat_analysis_subset |> 
    filter(housing_bert == 1) |> 
    filter(!is.na(housing_pos)) |> 
    group_by(year, harmonised, housing_pos) |> 
    count()

dat_ref_texts

# 15 people (not catherine bryne) + both housing minsters
length(unique(dat_ref_texts$harmonised))

# also include the ministers of housing


## get list of all names (first names and surnames)

dat_harmonised <- readRDS("harmonisedlegislator.rds")

dat_harmonised <- dat_harmonised |> 
    mutate(candidate = str_squish(candidate))


dat_names_td <- dat_harmonised |> 
    corpus(text_field = "candidate") |> 
    tokens(what = "word3") |> 
    dfm() |> 
    dfm_select(min_nchar = 2) |> 
    textstat_frequency()

names_td <- unique(dat_names_td$feature)

head(names_td)

set.seed(245)

toks_coll <- dat_analysis_subset_housing |> 
    filter(!is.na(ownership_2)) |>  # only TDs with ownership data
    filter(housing_bert == 1) |> # select only tweets related to housing
    corpus() |> 
    corpus_sample(size = 5000) |> # select 5,000 documents to check collocations
    tokens(remove_punct = TRUE,
           remove_numbers = TRUE,
           remove_symbols = TRUE,
           remove_url = TRUE, padding = TRUE,
           what = "word3") |> 
    tokens_remove(pattern = stopwords("en"),
                  padding = TRUE)


tstat_col <- textstat_collocations(toks_coll, 
                                   min_count = 3, size = 2) |> 
    subset(z > 3)

# key atm model

# create corpus of housing content
corp_sampled <- dat_analysis_subset_housing |> 
    filter(!is.na(ownership_2)) |>  # only TDs with ownership data
    filter(housing_bert == 1) |> # select only tweets related to housing
    corpus() 

# create dfm for keyATM model
dfmat_keyatm <- corp_sampled |> 
    tokens(remove_punct = TRUE,
           remove_numbers = TRUE,
           remove_symbols = TRUE,
           remove_url = TRUE, padding = TRUE,
           what  = "word3") |>
    tokens_remove(pattern = names_td) |> # remove firstnames and surnames of TDs
    tokens_remove(pattern = phrase(c(
        "Sinn F*in",
        "Fianna F*il",
        "Fine Gael",
        "Social Dem*",
        "Labour",
        "FF", 
        "FG", 
        "SF",
        "Green P*",
        "taoiseach*",
        "minister*"
    ))) |> 
    tokens_compound(pattern = c("emergency accomodation",
                                "housing first",
                                "first time buyer",
                                "first home",
                                "cost rental",
                                "new homes",
                                "first time buyer",
                                "housing for all",
                                "housing crisis",
                                "housing emergency",
                                "public housing",
                                "social housing",
                                "new home",
                                "vacant homes",
                                "emergency response",
                                "waiting list",
                                "human right",
                                "real estate")) |> 
    tokens_compound(pattern = phrase(tstat_col)) |> 
    tokens_remove(pattern = c("asked", "ask", "deputy",
                              "tabular_form",
                              "tabular form",
                              "statement",
                              "make",
                              "matter", 
                              "dubw",
                              "plans",
                              "housing",
                              "heritage",
                              "planning",
                              "number",
                              "local_government")) |> # housing appears in almost all topics
    tokens_remove(pattern = stopwords("en"), padding = TRUE) |> 
    dfm() |> 
    dfm_keep(min_nchar = 3) |> 
    dfm_remove(pattern = "@*") |> # remove mentions of other accounts
    dfm_remove(pattern = "#*") |> # remove hashtags
    dfm_remove(pattern = "amp") |> 
    dfm_remove(pattern = "0001f600") |> 
    dfm_remove(pattern = c("0*", "1*", "2*",
                           "3*", "4*",
                           "5*",
                           "6*",
                           "7*",
                           "8*",
                           "9*")) |> 
    dfm_keep(min_nchar = 4)


dfmat_keyatm_trim <- dfm(dfmat_keyatm) |>
    dfm_trim(min_termfreq = 5, min_docfreq = 2)

# remove empty documents (ntoken = 0)
dfmat_keyatm_trim_no0 <- dfm_subset(dfmat_keyatm_trim, ntoken(dfmat_keyatm_trim) > 0)

ndoc(dfmat_keyatm_trim_no0)

keyATM_docs <- keyATM_read(texts = dfmat_keyatm_trim_no0)

summary(keyATM_docs)

## Use this to get the seedwords for the keyATM model from the spreadsheet

# Original string
original_string <- "social housing, public housing, low_income"

# Splitting the string into individual elements
elements <- unlist(strsplit(original_string, ", "))


get_keywords <- function(x, data = df) {
    unlist(strsplit(as.character(df[df$Topic == x, "Keywords"]), ", "))
    
}

df <- read_csv("topics_seedwords_housing.csv")


dat_keywords <- df |> 
    filter(Topic %in% c("Social Housing",
                        "Homelessness",
                        "Homeownership",
                        "Renting",
                        "Cost",
                        "Buildings and Investment",
                        "Achievements",
                        "Failures",
                        "Supply",
                        "Demand")) |> 
    mutate(Topic = dplyr::recode(Topic,
                                 "Buildings and Investment" = "Buildings")) |> 
    arrange(Topic) |> 
    select(Topic, Keywords)



# print table
print(
    xtable(dat_keywords,
           digits = 1,
           caption = "Topic labels and keywords used for keyATM model.",
           label = "tab:topic_labels",
           align = c(
               "p{0.03\\textwidth}",
               "p{0.15\\textwidth}",
               "p{0.7\\textwidth}"
           )
    ),
    type = "latex",
    digits = 1,
    size = "footnotesize",    
    file = "tab_a16.tex",
    include.rownames = FALSE,
    caption.placement = "top"
)

print(
    xtable(dat_keywords,
           digits = 1,
           caption = "Topic labels and keywords used for keyATM model.",
           label = "tab:topic_labels",
           align = c(
               "p{0.03\\textwidth}",
               "p{0.15\\textwidth}",
               "p{0.7\\textwidth}"
           )
    ),
    type = "html",
    digits = 1,
    size = "footnotesize",    
    file = "tab_a16.html",
    include.rownames = FALSE,
    caption.placement = "top"
)

table(df$Topic)

# get keywords and topics for keyATM topics
keywords <- list(
    Social_Housing = get_keywords(x = "Social Housing"),
    Homelessness = get_keywords(x = "Homelessness"),
    Homeownership = get_keywords(x = "Homeownership"),
    Renting = get_keywords(x = "Renting"),
    Cost = get_keywords(x = "Cost"),
    Buildings = get_keywords(x = "Buildings and Investment"),
    Achievements = get_keywords(x = "Achievements"),
    Failures = get_keywords(x = "Failures"),
    Supply = get_keywords(x = "Supply"),
    Demand = get_keywords(x = "Demand")
)



vars_selected <- docvars(dfmat_keyatm_trim_no0) |> 
    dplyr::select(ownership_2, gov_opp)

# https://keyatm.github.io/keyATM/articles/pkgdown_files/keyATM_cov.html

# run base keyATM topic model
out <- keyATM(
    docs = keyATM_docs,
    no_keyword_topics = 2,
    keywords = keywords,
    model = "base",
    options = list(seed = 250)
)


# get top words for each topic
top_words(out) 

dat_topwords <- top_words(out, n = 6) |> 
    as.data.frame() |> 
    gather(topic, terms) |> 
    mutate(topic = sub("^[0-9]+_", "", topic)) |> 
    mutate(terms = str_remove_all(terms, " \\[[^a-zA-Z0-9-\\d]\\]")) |> 
    mutate(terms = str_remove_all(terms, " \\[10\\]")) |> 
    group_by(topic) |> 
    summarise(terms = paste(terms, collapse = ", "))


# extract thetas
thetas <- as.data.frame(out$theta)

# remove numbers at start of column names
names(thetas) <- sapply(names(thetas), function(name) {
    if (grepl("^[0-9]+_", name)) {
        return(sub("^[0-9]+_", "", name))
    }
    return(name)
})

docvars_trimmed_dfm <- docvars(dfmat_keyatm_trim_no0)

nrow(thetas)

nrow(docvars_trimmed_dfm)


# bind thetas with docvars of trimmed dfm
dat_analysis_keyatm <- bind_cols(thetas, docvars_trimmed_dfm)

topics <- colnames(thetas)

# remove Other_1 and Other_2 from character vector topics
topics <- topics[!grepl("Other", topics)]


dat_analysis_keyatm$ownership_2 <- relevel(factor(dat_analysis_keyatm$ownership_2), 
                                           ref = "Not Homeowner or Landlord")


dat_analysis_keyatm$gov_opp <- relevel(factor(dat_analysis_keyatm$gov_opp), 
                                       ref = "Opposition")


# empty data frame to store coefficients
dat_coefs_all <- data.frame()

list_models_topics <- list()


for (i in topics) {
    print(i)
    
    # predict topic prevalence conditionally on 
    # ownership and government opposition
    # add year fixed-effects
    dat_analysis_keyatm$year <- as.factor(dat_analysis_keyatm$year)
    lm_topic <- feols(
        as.formula(paste(i, "~ ownership_2  + gov_opp + dublin_dummy + perc_change_mean + year + type")),
        data = dat_analysis_keyatm,
        cluster = "mpname")
    
    # store regression models
    list_models_topics[[i]] <- lm_topic
    
    
    dat_tidy <- broom::tidy(lm_topic) |> 
        mutate(topic = i) |> 
        filter(str_detect(term, "ownership_2|gov_opp"))
    
    dat_coefs_all <- bind_rows(dat_tidy, dat_coefs_all)    
    
}


dat_coefs_all_merged <- dat_coefs_all |> 
    filter(!str_detect(topic, "her_")) |> 
    left_join(dat_topwords, by = c("topic" = "topic")) |> 
    mutate(topic = str_replace_all(topic, "_", " ")) |> 
    mutate(topic_terms = paste0(topic, "\n (", terms, ")")) |> 
    mutate(top_words = paste0("Top words: ", terms)) |> 
    mutate(term = gsub("gov_opp|ownership_2", "", term)) |> 
    mutate(term = str_remove_all(term, "Homeowner or "))


order_fd <- dat_coefs_all_merged |> 
    select(estimate, top_words, topic, term) |> 
    filter(term == "Government") |> 
    arrange(-estimate)

# order in ascending order based on government variable
dat_coefs_all_merged$top_words <- factor(dat_coefs_all_merged$top_words,
                                         levels = order_fd$top_words)


ggplot(filter(dat_coefs_all_merged, !str_detect(term, "year")),
       aes(x = estimate, y = top_words,
           xmin = estimate - 1.96 * std.error,
           xmax = estimate + 1.96 * std.error)) +
    geom_vline(xintercept = 0, colour = "red",
               linetype = "dashed") +
    geom_label(aes(label = topic), nudge_y = 0.3,
               fill = "white",
               label.size = NA, size = 4) +
    geom_linerange(linewidth = 1) +
    geom_point(size = 3) +
    scale_y_discrete(labels = label_wrap(40)) +
    facet_wrap(~term) +
    labs(x = "Percentage Point Difference in Topic Prevalence", y = NULL) +
    theme(axis.text.y = element_text(colour = "grey30",
                                     size = 10),
          axis.ticks.y = element_blank())
ggsave("fig_08.pdf",
       width = 9, height = 6)
ggsave("fig_08.eps",
       width = 9, height = 6)



# empty data frame to store coefficients
dat_coefs_all_int <- data.frame()
fd_combined <- data.frame()
list_int_models_topics <- list()


for (i in topics) {
    print(i)
    
    # predict topic prevalence conditionally on 
    # ownership and government opposition
    # add year fixed-effects
    dat_analysis_keyatm$year <- as.factor(dat_analysis_keyatm$year)
    
    # get subset of variables that are not na and in regression below
    dat_analysis_keyatm_subset <- dat_analysis_keyatm |> 
        dplyr::select(i, ownership_2, mpname, perc_change_mean, 
                      gov_opp, dublin_dummy,
                      seniority_years, year, type) |> 
        drop_na()
    
    lm_topic <- feols( # make sure to add interaction
        as.formula(paste(i,  "~ ownership_2  * gov_opp + dublin_dummy + perc_change_mean + year + type")),
        data = dat_analysis_keyatm_subset,
        cluster = "mpname")
    
    # store regression models
    list_int_models_topics[[i]] <- lm_topic
    
    
    dat_tidy <- broom::tidy(lm_topic) |> 
        mutate(topic = i) |> 
        filter(str_detect(term, "ownership_2|gov_opp|year"))
    
    dat_coefs_all_int <- bind_rows(dat_tidy, dat_coefs_all_int)    
    
    # run first difference analysis here
    #https://iqss.github.io/clarify/reference/sim_setx.html
    
    sim <- sim(lm_topic, n = 1000,
               vcov = sandwich::vcovCL(lm_topic, 
                                       cluster = ~ mpname))
    
    fd_opp_raw <- sim_setx(
        sim, 
        x = data.frame(ownership_2 = "Not Homeowner or Landlord", 
                       gov_opp = "Opposition"),
        x1 = data.frame(ownership_2 = "Homeowner or Landlord", 
                        gov_opp = "Opposition"),
        verbose = FALSE)
    
    print(fd_opp_raw)
    
    fd_opp <- fd_opp_raw |> 
        as.data.frame() |> 
        mutate(topic = i) |> 
        group_by(topic) |> 
        summarise(estimate = mean(FD),
                  conf.low = quantile(FD, 0.025),
                  conf.high = quantile(FD, 0.975)) |> 
        mutate(model = "Opposition:\nLandlord vs. No Landlord")
    
    fd_gov_raw <- sim_setx(
        sim, 
        x = list(ownership_2 = "Not Homeowner or Landlord", 
                 gov_opp = "Government"),
        x1 = list(ownership_2 = "Homeowner or Landlord", 
                  gov_opp = "Government"),
        verbose = TRUE)
    
    print(fd_gov_raw)
    
    fd_gov <- fd_gov_raw |> 
        as.data.frame() |> 
        mutate(topic = i) |> 
        group_by(topic) |> 
        summarise(estimate = mean(FD),
                  conf.low = quantile(FD, 0.025),
                  conf.high = quantile(FD, 0.975)) |> 
        mutate(model = "Government:\nLandlord vs. No Landlord")
    
    fd_all <- bind_rows(fd_opp, fd_gov)
    
    fd_combined <- bind_rows(fd_all, fd_combined)    
}


# plot fd_combined

fd_combined_clean <- fd_combined |> 
    filter(!str_detect(topic, "her_")) |> 
    left_join(dat_topwords, by = c("topic" = "topic")) |> 
    mutate(topic = str_replace_all(topic, "_", " ")) |> 
    mutate(topic_terms = paste0(topic, "\n (", terms, ")")) |> 
    mutate(top_words = paste0("Top words: ", terms))


# order in ascending order based on government variable
fd_combined_clean$top_words <- factor(fd_combined_clean$top_words,
                                      levels = order_fd$top_words)


# Figure A10 ----

ggplot(fd_combined_clean,
       aes(x = estimate, y = top_words,
           xmin = conf.low,
           xmax = conf.high)) +
    geom_vline(xintercept = 0, colour = "red",
               linetype = "dashed") +
    geom_label(aes(label = topic), nudge_y = 0.3,
               fill = "white",
               label.size = NA, size = 4) +
    geom_linerange(linewidth = 1) +
    geom_point(size = 3) +
    scale_y_discrete(labels = label_wrap(40)) +
    facet_wrap(~model) +
    geom_segment(aes(x = 0.02, 
                     y = 0.55, xend = 0.12,
                     yend = 0.55),
                 linewidth = 0.5,
                 arrow = arrow(length = unit(0.2, "cm")),
                 colour = "grey50") +
    geom_segment(aes(x = -0.02, xend = -0.12,
                     y = 0.55, 
                     yend = 0.55),
                 linewidth = 0.5,
                 arrow = arrow(length = unit(0.2, "cm")),
                 colour = "grey50") +
    scale_x_continuous(limits = c(-0.14, 0.12)) +
    annotate("text", x = 0.02, y = 0.8,
             size = 3.5, hjust = 0,
             label = "Landlords higher",
             colour = "grey50") +
    annotate("text", x = -0.02, y = 0.8,
             size = 3.5, hjust = 1,
             label = "Landlords lower",
             colour = "grey50") +
    xlab("First Difference: Landlord-No Landlord") +
    ylab(NULL) +
    theme(axis.text.y = element_text(colour = "grey30",
                                     size = 10),
          axis.ticks.y = element_blank())
ggsave("fig_a11.pdf",
       width = 9, height = 6)
ggsave("fig_a11.eps",
       width = 9, height = 6)

