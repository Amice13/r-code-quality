#' ---
#' title: "Legislating Landlords: Private Interests, Issue Emphasis, and Policy Positions (LSQ)"
#' subtitle: "04c_analysis_positions.R"
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


library(tidyverse)           # CRAN v2.0.0
library(quanteda)            # CRAN v3.3.1 ## do not use quanteda v4.0.2
library(quanteda.textstats)  # CRAN v0.96.3
library(quanteda.textmodels) # CRAN v0.9.6
library(estimatr)            # CRAN v1.0.0
library(marginaleffects)     # CRAN v0.18.0
library(texreg)              # CRAN v1.38.6
library(arrow)               # CRAN v14.0.0.2
library(clarify)             # CRAN v0.2.0
library(fixest)              # CRAN v0.11.2
library(xtable)              # CRAN v1.8-4
library(scales)              # CRAN v1.2.1
library(LSX)                 # CRAN v1.3.1
library(cowplot)             # CRAN v1.1.1

# load custom ggplot2 scheme
source("function_theme_base.R")

## prepare scaling analysis
# read parquet file
dat_analysis <- read_parquet("data_dontshare/data_analysis_classified_housing.parquet")

# load sentiment predictions
dat_sent <- read_parquet("data_dontshare/housing_content_sentiment_cardiff.parquet")


dat_analysis |> 
    filter(housing_bert == "Housing") |> 
    nrow()


# make housing_bert an integer
dat_analysis <- dat_analysis |> 
    mutate(housing_bert = dplyr::recode(housing_bert, "Housing" = 1, 
                                        "Non-housing" = 0)) |> 
    mutate(housing_bert = as.integer(housing_bert))

table(dat_analysis$housing_bert)


dat_analysis |> 
    filter(housing_bert == 1) |> 
    nrow()

dat_analysis |> 
    filter(housing_bert == 1) |> 
    filter(!is.na(ownership_2)) |> 
    nrow()

overview <- dat_analysis |> 
    filter(housing_bert == 1) |> 
    filter(!is.na(ownership_2)) 


table(overview$type)


table(dat_analysis$housing_bert,
      dat_analysis$year)



dat_analysis <- dat_analysis |> 
    mutate(age = as.integer(year) - as.integer(birth)) |> 
    mutate(seniority_years = as.integer(seniority) / 365) 


dat_analysis <- dat_analysis |> 
    mutate(cycle = case_when(
        year %in% 2011:2015 ~ "2013-2016",
        year %in% 2016:2019 ~ "2016-2020",
        year %in% 2020:2025 ~ "2020-2022"
    ))


head(dat_analysis$document_id)
tail(dat_analysis$document_id)

dat_analysis_count <- dat_analysis |> 
    group_by(mpname, ownership_2, gov_opp, year) |> 
    count() 



# get list of all names (first names and surnames)
dat_harmonised <- readRDS("harmonisedlegislator.rds")

dat_harmonised <- dat_harmonised |> 
    mutate(candidate = str_squish(candidate))


# subset data

dat_analysis$year <- as.integer(dat_analysis$year)

dat_analysis_subset <- filter(dat_analysis, year %in% 2016:2022) |> 
    group_by(year, harmonised) |> 
    mutate(count_texts = n()) |> 
    filter(count_texts >= 2) |>  # keep only TDs with at least two texts per year 
    ungroup()


# get overview of reference texts (actors)
dat_ref_cycles <- dat_analysis_subset |> 
    filter(!is.na(housing_pos)) |> 
    mutate(term = str_replace_all(term, "-", "--")) |> 
    group_by(term, candidate, party, gov_opp) |> 
    summarise(Score = as.integer(min(housing_pos)), 
              `Texts (Housing)` = as.integer(n())) |> 
    arrange(term, Score, party, candidate) |> 
    rename(Party = party, Name = candidate,
           `Gov/Opp` = gov_opp,
           Term = term) |> 
    mutate(Name = str_to_title(Name)) |> 
    mutate(Name = dplyr::recode(Name, "Eoin O Broin" = "Eoin Ó Broin",
                                "Darragh O'brien" = "Darragh O'Brien",
                                "Paul Mcauliffe" = "Paul McAuliffe",
                                "Cian O'callaghan" = "Cian O'Callaghan"))


print(xtable(dat_ref_cycles,
             caption.placement = "top",
             digits = 1,
             label = "tab:refscores",
             caption = "Politicians used for reference scores in Wordscores analysis"),
      include.rownames = FALSE,
      format.args = list(big.mark = ",", decimal.mark = "."),
      type = "latex",
      digits = 1,
      caption.placement = "top",
      size = "footnotesize",
      file = "tab_a08.tex")

print(xtable(dat_ref_cycles,
             caption.placement = "top",
             digits = 1,
             label = "tab:refscores",
             caption = "Politicians used for reference scores in Wordscores analysis"),
      include.rownames = FALSE,
      format.args = list(big.mark = ",", decimal.mark = "."),
      type = "html",
      digits = 1,
      caption.placement = "top",
      size = "footnotesize",
      file = "tab_a08.html")


dat_names_td <- dat_harmonised |> 
    corpus(text_field = "candidate") |> 
    tokens(what = "word3") |> 
    dfm() |> 
    dfm_select(min_nchar = 2) |> 
    textstat_frequency()

names_td <- unique(dat_names_td$feature)

head(names_td)


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

# run collocation analysis for compounding
tstat_col <- textstat_collocations(toks_coll, 
                                   min_count = 3, size = 2) |> 
    subset(z > 3)



# keyness analysis - housing vs not housing

corp_keyness <- corpus(dat_analysis)


# note: parts of the analysis take very long with quanteda v.4.0.2
# We recommend installing quanteda 3.3.1 for this analysis

# library(remotes)
# install_version("quanteda", version = "3.3.1", repos = "http://cran.us.r-project.org")

# list of terms to compound
compound <- c("social hous*",
              "public hous*",
              "affordable hous*",
              "property tax*",
              "planning per*",
              "fianna fáil*",
              "fine gael*",
              "sinn féin*",
              "local government*",
              "housing cris*",
              "housing emerg*",
              "local authorit*")


toks_keyness <- corp_keyness |>
    tokens(remove_punct = TRUE, 
           remove_symbols = TRUE,
           remove_numbers = TRUE,
           what = "word3") 

toks_keyness <- toks_keyness |>
    tokens_compound(pattern = phrase(c("fianna fáil*",
                                       "fine gael*",
                                       "sinn féin*"))) |>
    tokens_compound(pattern = phrase(compound)) |> 
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
        "minister*",
        "broin", "eoin"
    ))) |> 
    tokens_remove(pattern = names_td) |> # remove firstnames and surnames of TDs
    tokens_compound(pattern = phrase(tstat_col)) |> 
    tokens_remove(pattern = c(stopwords("en"), "amp")) |> 
    tokens_remove(pattern = c("#*", "042-9751802", "@*")) |>  # remove hashtags and meaningless features
    tokens_remove(pattern = c("http*", "www*"))  # remove URLs


# get two types of documents for loop
tweets_qs <- unique(dat_analysis$type)

# empty data frame to store keyness results
dat_keyness_words <- data.frame()

for (i in tweets_qs) {
    
    cat("Keyness analysis for", i, "\n")
    
    # create dfm and group by measure i
    toks_keyness_subset <- toks_keyness |>
        tokens_subset(type == i)
    
    dfmat_keyness <- toks_keyness_subset |> 
        dfm() |>
        dfm_keep(min_nchar = 2) |> 
        dfm_group(groups = toks_keyness_subset$housing_bert) # group by housing prediction
    
    # run keyness analysis
    keyness <- textstat_keyness(dfmat_keyness, target = "1")
    
    # create variable indicating the category
    keyness$type <- i
    
    # bind data frame
    dat_keyness_words <- bind_rows(keyness, dat_keyness_words)
}


# get the top-100 terms per measure
dat_keyness_words_max_100 <- dat_keyness_words |>
    group_by(type) |>
    mutate(rank = 1:n()) |>
    filter(rank <= 100)


# bind terms for each measure (unit of observation is not the measure, rather than the term)
dat_keyness_words_max_100_tab <- dat_keyness_words_max_100 |>
    rename(Source = type) |> 
    group_by(Source) |>
    arrange(-chi2) |> 
    summarise(Words = paste(feature, collapse = ", ")) |> 
    ungroup() 


# print table
print(
    xtable(dat_keyness_words_max_100_tab,
           digits = 1,
           caption = "Keyness analysis for parliamentary questions and tweets about housing compared with texts not classified as housing policy. Table lists the 100 features with the highest keynesss (chi-squared) values.",
           label = "tab:keyness_housing",
           align = c(
               "p{0.03\\textwidth}",
               "p{0.15\\textwidth}",
               "p{0.7\\textwidth}"
           )
    ),
    type = "latex",
    digits = 1,
    size = "footnotesize",    
    add.to.row = list(pos=list(1), command = "\\hdashline \n"),
    file = "tab_a04.tex",
    include.rownames = FALSE,
    caption.placement = "top"
)


print(
    xtable(dat_keyness_words_max_100_tab,
           digits = 1,
           caption = "Keyness analysis for parliamentary questions and tweets about housing compared with texts not classified as housing policy. Table lists the 100 features with the highest keynesss (chi-squared) values.",
           label = "tab:keyness_housing",
           align = c(
               "p{0.03\\textwidth}",
               "p{0.15\\textwidth}",
               "p{0.7\\textwidth}"
           )
    ),
    type = "html",
    digits = 1,
    size = "footnotesize",    
    add.to.row = list(pos=list(1), command = "\\hdashline \n"),
    file = "tab_a04.html",
    include.rownames = FALSE,
    caption.placement = "top"
)


# get the top-25 terms per measure
dat_keyness_words_max_25 <- dat_keyness_words |>
    group_by(type) |>
    mutate(rank = 1:n()) |>
    filter(rank <= 25)


# bind terms for each measure (unit of observation is not the measure, rather than the term)
dat_keyness_words_max_25_tab <- dat_keyness_words_max_25 |>
    rename(Source = type) |> 
    group_by(Source) |>
    arrange(-chi2) |> 
    summarise(Words = paste(feature, collapse = ", ")) |> 
    ungroup() 


# print table
print(
    xtable(dat_keyness_words_max_25_tab,
           digits = 1,
           caption = "Keyness analysis for parliamentary questions and tweets about housing compared with texts not classified as housing policy. Table lists the 25 features with the highest keynesss (chi-squared) values.",
           label = "tab:keyness_housing_small",
           align = c(
               "p{0.03\\textwidth}",
               "p{0.15\\textwidth}",
               "p{0.7\\textwidth}"
           )
    ),
    type = "latex",
    digits = 1,
    size = "footnotesize",    
    add.to.row = list(pos=list(1), command = "\\hdashline \n"),
    file = "tab_01.tex",
    include.rownames = FALSE,
    caption.placement = "top"
)

print(
    xtable(dat_keyness_words_max_25_tab,
           digits = 1,
           caption = "Keyness analysis for parliamentary questions and tweets about housing compared with texts not classified as housing policy. Table lists the 25 features with the highest keynesss (chi-squared) values.",
           label = "tab:keyness_housing_small",
           align = c(
               "p{0.03\\textwidth}",
               "p{0.15\\textwidth}",
               "p{0.7\\textwidth}"
           )
    ),
    type = "html",
    digits = 1,
    size = "footnotesize",    
    add.to.row = list(pos=list(1), command = "\\hdashline \n"),
    file = "tab_01.html",
    include.rownames = FALSE,
    caption.placement = "top"
)



# keyness again for full dataset (not distinguishing between tweets and questions)

dfmat_keyness_all <- toks_keyness |> 
    dfm() |>
    dfm_keep(min_nchar = 2) |> 
    dfm_group(groups = toks_keyness$housing_bert) # group by housing prediction

# run keyness analysis
keyness_all <- textstat_keyness(dfmat_keyness_all, target = "1")

# get the top-100 terms per measure
dat_keyness_words_all_max_100 <- keyness_all |>
    arrange(-chi2) |> 
    mutate(rank = 1:n()) |>
    filter(rank <= 100)


# bind terms for each measure (unit of observation is not the measure, rather than the term)
dat_keyness_words_all_max_100_tab <- dat_keyness_words_all_max_100 |>
    summarise(Words = paste(feature, collapse = ", ")) |> 
    ungroup() 


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



# set up LSS
dfmat_lss <- dat_analysis_subset_housing |> 
    filter(!is.na(ownership_2)) |>  # only TDs with ownership data
    filter(housing_bert == 1) |> # select only tweets related to housing
    corpus() |> 
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
        "minister*",
        "broin", "eoin"
    ))) |> 
    tokens_compound(pattern = phrase(tstat_col)) |> 
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


dict_lss <- quanteda::dictionary(list(
    positive = c("deliver",
                 "investment",
                 "affordable", "achievement",
                 "expansion"),
    negative = c("homeless",
                 "emergency", "eviction", "crisis",
                 "social_housing", "housing_crisis")
))


seeds_lss <- as.seedwords(dict_lss)

# print seed words
seeds_lss

# run LSS model
lss <- textmodel_lss(dfmat_lss, seeds = seeds_lss,
                     k = 300, 
                     cache = FALSE, 
                     include_data = TRUE, 
                     group_data = TRUE)



frequency <- lss$frequency[names(lss$beta)] # fix for < v1.1.4

dat_lss_neg_pos <- data.frame(word = names(lss$beta), 
                          beta = lss$beta,
                          freq = frequency,
                          stringsAsFactors = FALSE) |> 
    filter(freq > 10)


# select most positive words
dat_lss_neg_pos_top <- top_n(dat_lss_neg_pos, n = 100,
                         wt = beta) |> 
    mutate(type = "Highest LSS Scores")

head(dat_lss_neg_pos_top)


# select most negative words
dat_lss_neg_pos_bottom <- top_n(dat_lss_neg_pos, n = -100,
                            wt = beta) |> 
    mutate(type = "Lowest LSS Scores") |> 
    arrange(beta) 

head(dat_lss_neg_pos_bottom)


# bind terms for each measure (unit of observation is not the measure, rather than the term)
dat_lss_posneg <- bind_rows(dat_lss_neg_pos_top, dat_lss_neg_pos_bottom) |> 
    group_by(type) |>
    summarise(Terms = paste(word, collapse = ", ")) |> 
    ungroup() |> 
    rename(Dimension = type)

# reverse order of negative and positive words word table
dat_lss_posneg <- dat_lss_posneg |>  map_df(rev)



# print table
print(
    xtable(dat_lss_posneg,
           digits = 1,
           caption = "Terms with the 100 lowest (negative) and highest (positive) LSS word scores in texts about housing.",
           label = "tab:lss_pos_neg",
           align = c(
               "p{0.03\\textwidth}",
               "p{0.2\\textwidth}",
               "p{0.7\\textwidth}"
           )
    ),
    type = "latex",
    digits = 1,
    add.to.row = list(pos=list(1), command="\\hdashline \n"),
    size = "footnotesize",
    file = "tab_a06.tex",
    include.rownames = FALSE,
    caption.placement = "top"
)

print(
    xtable(dat_lss_posneg,
           digits = 1,
           caption = "Terms with the 100 lowest (negative) and highest (positive) LSS word scores in texts about housing.",
           label = "tab:lss_pos_neg",
           align = c(
               "p{0.03\\textwidth}",
               "p{0.2\\textwidth}",
               "p{0.7\\textwidth}"
           )
    ),
    type = "html",
    digits = 1,
    add.to.row = list(pos=list(1), command="\\hdashline \n"),
    size = "footnotesize",
    file = "tab_a06.html",
    include.rownames = FALSE,
    caption.placement = "top"
)



# select most positive words
dat_neg_pos_lss_top_small <- top_n(dat_lss_neg_pos, n = 25,
                         wt = beta) |> 
    mutate(type = "Highest LSS Scores")

# select most negative words
dat_neg_pos_lss_bottom_small <- top_n(dat_lss_neg_pos, n = -25,
                            wt = beta) |> 
    mutate(type = "Lowest LSS Scores") |> 
    arrange(beta) 



# bind terms for each measure (unit of observation is not the measure, rather than the term)
dat_lss_posneg_small <- bind_rows(dat_neg_pos_lss_top_small, dat_neg_pos_lss_bottom_small) |> 
    group_by(type) |>
    summarise(Terms = paste(word, collapse = ", ")) |> 
    ungroup() |> 
    rename(Dimension = type)

# reverse order of negative and positive words word table
dat_lss_posneg_small <- dat_lss_posneg_small |>  map_df(rev)


# print table
print(
    xtable(dat_lss_posneg_small,
           digits = 1,
           caption = "Terms with the 25 lowest (negative) and highest (positive) LSS word scores in texts about housing.",
           label = "tab:lss_pos_neg_small",
           align = c(
               "p{0.03\\textwidth}",
               "p{0.2\\textwidth}",
               "p{0.7\\textwidth}"
           )
    ),
    type = "latex",
    digits = 1,
    add.to.row = list(pos=list(1), command="\\hdashline \n"),
    size = "footnotesize",
    file = "tab_03.tex",
    include.rownames = FALSE,
    caption.placement = "top"
)

print(
    xtable(dat_lss_posneg_small,
           digits = 1,
           caption = "Terms with the 25 lowest (negative) and highest (positive) LSS word scores in texts about housing.",
           label = "tab:lss_pos_neg_small",
           align = c(
               "p{0.03\\textwidth}",
               "p{0.2\\textwidth}",
               "p{0.7\\textwidth}"
           )
    ),
    type = "html",
    digits = 1,
    add.to.row = list(pos=list(1), command="\\hdashline \n"),
    size = "footnotesize",
    file = "tab_03.html",
    include.rownames = FALSE,
    caption.placement = "top"
)


# Predict all texts
dat_lss <- docvars(lss$dat)
dat_lss$lss <- predict(lss)


dat_pol <- dat_lss |> 
    group_by(gov_opp, mpname, year) |> 
    summarise(mean_lss = mean(lss))


# store scalingestimates for each year
dat_scalingestimates <- data.frame()

dat_wordscores_words <- data.frame()

years <- 2016:2022

for (i in years) {
    
    dat_tweets_year <- filter(dat_analysis_subset_housing,
                              year == i) 
    
    dat_tweets_n <- dat_tweets_year |> 
        filter(!is.na(ownership_2)) |>  # only TDs with ownership data
        filter(housing_bert == 1) |> # select only tweets related to housing
        group_by(year, harmonised) |> 
        summarise(n_texts_housing = n())
    
    cat("Scaling analysis for", i, "\n")
    
    dfmat_grouped <- dat_tweets_year |> 
        filter(!is.na(ownership_2)) |>  # only TDs with ownership data
        filter(housing_bert == 1) |> # select only tweets related to housing
        corpus() |> 
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
            "broin",
            "eoin",
            "minister*"
        ))) |> 
        tokens_compound(pattern = phrase(tstat_col)) |> 
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
        dfm_group(groups = harmonised) |> 
        dfm_trim(min_termfreq = 5, min_docfreq = 5) 
    
    tmod_ws <- textmodel_wordscores(
        dfmat_grouped,
        y = dfmat_grouped$housing_pos)
    
    dat_ws_words <- data.frame(
        word_score = tmod_ws$wordscores)
    
    dat_ws_words$feature <- rownames(dat_ws_words)
    dat_ws_words$year <- i
    rownames(dat_ws_words) <- NULL
    
    
    dat_wordscores_words <- bind_rows(dat_wordscores_words,
                                      dat_ws_words)

    
    pred_ws_year <- predict(tmod_ws, 
                            se.fit = TRUE,
                            interval = "confidence",
                            level = 0.95)
    
    dat_pred_ws <- data.frame(
        estimate_wordscores = pred_ws_year$fit,
        estimate_wordscores_se = pred_ws_year$se.fit
    )
    
    dat_pred_ws$harmonised <- rownames(dat_pred_ws)
    
    # get docvars for year
    docvars_grouped <- docvars(dfmat_grouped)
    
    dat_tweets_sum <- dat_tweets_year |> 
        filter(housing_bert == 1) |> 
        dplyr::select(party, party_recoded, party_broad,
                      cycle, ownership_2,
                      ownership_3,
                      dublin_dummy, age, seniority_years,
                      propnum,
                      candidate, harmonised, year, housing_pos,
                      mean_price, mean_price,
                      perc_change_mean, perc_change_median) |> 
        unique()
    
    dat_pred_ws <- left_join(dat_pred_ws, 
                             docvars_grouped) |> 
        left_join(dat_tweets_sum)
    
    dat_pred_ws$year_type <- as.character(i)
    
    dat_pred_ws <- left_join(dat_pred_ws, dat_tweets_n)
    dat_scalingestimates <- bind_rows(dat_pred_ws, 
                                      dat_scalingestimates)
}


# get average LSS score per year

dat_lss_year_mp <- dat_lss |> 
    group_by(mpname, year) |> 
    summarise(mean_lss = mean(lss, na.rm = TRUE),
              se_lss = sd(lss, na.rm = TRUE) / sqrt(n()))

# join Wordscores with LSS scores
dat_lss_ws <- left_join(dat_scalingestimates,
                        dat_lss_year_mp,
                        by = c("mpname", "year"))


# combine sentiment predictions with housing content

dat_housing <- filter(dat_analysis, housing_bert == 1)

nrow(dat_housing)

dat_housing$text_classified <- dat_housing$text
dat_housing$mpname_py <- dat_housing$mpname

dat_housing_sent <- bind_cols(dat_sent, dat_housing)

dat_housing_sent <- left_join(dat_sent, dat_housing)
# keyness for positive and negative sentdat_housing# keyness for positive and negative sentences

corp_posneg <- corpus(dat_housing_sent)

# list of terms to compound
compound <- c("social hous*",
              "public hous*",
              "affordable hous*",
              "property tax*",
              "planning per*",
              "fianna fáil*",
              "fianna fail*",
              "fine gael*",
              "sinn féin*",
              "local government*",
              "fulture fund*",
              "housing cris*",
              "housing emerg*",
              "local authorit*")


toks_posneg <- corp_posneg |>
    tokens(remove_punct = TRUE, 
           remove_symbols = TRUE,
           remove_numbers = TRUE,
           what = "word3") |>
    tokens_remove(pattern = c("broin", "eoin", "minister")) |> 
    tokens_remove(pattern = names_td) |> # remove firstnames and surnames of TDs
    tokens_compound(pattern = phrase(c("fianna fáil*",
                                       "fine gael*",
                                       "sinn féin*"))) |>
    tokens_compound(pattern = phrase(compound)) |> 
    tokens_remove(pattern = c(stopwords("en"), "amp")) |> 
    tokens_remove(pattern = c("#*", "042-9751802")) |>  # remove hashtags and meaningless features
    tokens_remove(pattern = c("http*", "www*"))  # remove URLs


# exclude neutral documents
toks_posneg_keyness <- toks_posneg |>
    tokens_subset(sentiment_cardiff != "neutral")

dfmat_posneg_keyness <- toks_posneg_keyness |> 
    dfm() |>
    dfm_keep(min_nchar = 2) |> 
    dfm_group(groups = toks_posneg_keyness$sentiment_cardiff) # group by housing prediction

# run keyness analysis
keyness_posneg <- textstat_keyness(dfmat_posneg_keyness, target = "positive")


# get the top-50 terms per measure
dat_keyness_pos <- keyness_posneg |> 
    arrange(-chi2) |>
    mutate(rank = 1:n()) |>
    filter(rank <= 100) |> 
    mutate(sentiment = "Positive")


dat_keyness_neg <- keyness_posneg |> 
    arrange(chi2) |>
    mutate(rank = 1:n()) |>
    filter(rank <= 100) |> 
    mutate(sentiment = "Negative")



# bind terms for each measure (unit of observation is not the measure, rather than the term)
dat_keyness_posneg <- bind_rows(dat_keyness_neg, dat_keyness_pos) |> 
    group_by(sentiment) |>
    arrange(-chi2) |> 
    summarise(Terms = paste(feature, collapse = ", ")) |> 
    ungroup() |> 
    rename(Sentiment = sentiment)


# print table
print(
    xtable(dat_keyness_posneg,
           digits = 1,
           caption = "Keyness analysis of features in texts classified as positive and negative in documents about housing. Table lists the 100 features with the lowest (negative) and highest (positive) keynesss (chi-squared) values.",
           label = "tab:keyness_posneg",
           align = c(
               "p{0.03\\textwidth}",
               "p{0.2\\textwidth}",
               "p{0.7\\textwidth}"
           )
    ),
    type = "latex",
    digits = 1,
    add.to.row = list(pos=list(1), command = "\\hdashline \n"),
    size = "footnotesize",
    file = "tab_a07.tex",
    include.rownames = FALSE,
    caption.placement = "top"
)

print(
    xtable(dat_keyness_posneg,
           digits = 1,
           caption = "Keyness analysis of features in texts classified as positive and negative in documents about housing. Table lists the 100 features with the lowest (negative) and highest (positive) keynesss (chi-squared) values.",
           label = "tab:keyness_posneg",
           align = c(
               "p{0.03\\textwidth}",
               "p{0.2\\textwidth}",
               "p{0.7\\textwidth}"
           )
    ),
    type = "html",
    digits = 1,
    add.to.row = list(pos=list(1), command = "\\hdashline \n"),
    size = "footnotesize",
    file = "tab_a07.html",
    include.rownames = FALSE,
    caption.placement = "top"
)


# get the top-25 terms per measure
dat_keyness_pos_small <- keyness_posneg |> 
    arrange(-chi2) |>
    mutate(rank = 1:n()) |>
    filter(rank <= 25) |> 
    mutate(sentiment = "Positive")


dat_keyness_neg_small <- keyness_posneg |> 
    arrange(chi2) |>
    mutate(rank = 1:n()) |>
    filter(rank <= 25) |> 
    mutate(sentiment = "Negative")



# bind terms for each measure (unit of observation is not the measure, rather than the term)
dat_keyness_posneg_small <- bind_rows(dat_keyness_neg_small, dat_keyness_pos_small) |> 
    group_by(sentiment) |>
    arrange(-chi2) |> 
    summarise(Terms = paste(feature, collapse = ", ")) |> 
    ungroup() |> 
    rename(Sentiment = sentiment)



# print table
print(
    xtable(dat_keyness_posneg_small,
           digits = 1,
           caption = "Keyness analysis of features in texts classified as positive and negative in documents about housing. Table lists the 25 features with the lowest (negative) and highest (positive) keynesss (chi-squared) values.",
           label = "tab:keyness_posneg_small",
           align = c(
               "p{0.03\\textwidth}",
               "p{0.1\\textwidth}",
               "p{0.7\\textwidth}"
           )
    ),
    type = "latex",
    digits = 1,
    add.to.row = list(pos=list(1), command="\\hdashline \n"),
    size = "footnotesize",
    file = "tab_04.tex",
    include.rownames = FALSE,
    caption.placement = "top"
)
print(
    xtable(dat_keyness_posneg_small,
           digits = 1,
           caption = "Keyness analysis of features in texts classified as positive and negative in documents about housing. Table lists the 25 features with the lowest (negative) and highest (positive) keynesss (chi-squared) values.",
           label = "tab:keyness_posneg_small",
           align = c(
               "p{0.03\\textwidth}",
               "p{0.1\\textwidth}",
               "p{0.7\\textwidth}"
           )
    ),
    type = "html",
    digits = 1,
    add.to.row = list(pos=list(1), command="\\hdashline \n"),
    size = "footnotesize",
    file = "tab_04.html",
    include.rownames = FALSE,
    caption.placement = "top"
)


# get average positive score per year
dat_sentiment_mean <- dat_housing_sent |> 
    group_by(mpname, year) |> 
    mutate(sentiment_cardiff = dplyr::recode(sentiment_cardiff,
                                             "positive" = 1,
                                             "negative" = -1,
                                             "neutral" = 0)) |> 
    summarise(mean_sent_cardiff = mean(sentiment_cardiff))


# merge all datasets
dat_lss_ws_sent <- left_join(dat_lss_ws,
                             dat_sentiment_mean,
                             by = c("mpname", "year"))



nrow(dat_scalingestimates)
nrow(dat_lss_ws)

cor.test(dat_lss_ws$mean_lss,
         dat_lss_ws$estimate_wordscores.fit)


rescale_variable <- function(x) {
    mean_x <- mean(x, na.rm = TRUE)
    sd_x <- sd(x, na.rm = TRUE)
    (x - mean_x) / sd_x
}

# standardise variables (mean of 0 and sd of 1)
dat_lss_ws_sent_stand <- dat_lss_ws_sent |> 
    ungroup() |> 
    mutate(estimate_wordscores.fit = rescale_variable(estimate_wordscores.fit),
           mean_lss = rescale_variable(mean_lss),
           mean_sent_cardiff = rescale_variable(mean_sent_cardiff))



# make sure mean is 0 and sd is 1
summary(dat_lss_ws_sent_stand$estimate_wordscores.fit)
sd(dat_lss_ws_sent_stand$estimate_wordscores.fit)

summary(dat_lss_ws_sent_stand$mean_sent_cardiff)
sd(dat_lss_ws_sent_stand$mean_sent_cardiff)

dat_lss_ws_sent_stand$ownership_2 <- relevel(factor(dat_lss_ws_sent_stand$ownership_2),
                                             ref = "Not Homeowner or Landlord")


dat_lss_ws_sent_stand$year <- factor(dat_lss_ws_sent_stand$year)



dat_lss_ws_sent_stand$dublin_dummy <- factor(
    dat_lss_ws_sent_stand$dublin_dummy,
    levels = c("Other", "Dublin"))

# run models
dat_lss_ws_sent_stand_change <- dat_lss_ws_sent_stand |> 
    mutate(gov_opp = factor(gov_opp, levels = c("Government", "Opposition")))


lm_ws <- fixest::feols(estimate_wordscores.fit ~ 
                           gov_opp + ownership_2 + dublin_dummy + seniority_years  + perc_change_mean | year,
                       cluster = "mpname", 
                       data = dat_lss_ws_sent_stand_change)

lm_lss <- fixest::feols(mean_lss ~ 
                            gov_opp + ownership_2 + dublin_dummy + seniority_years + perc_change_mean | year, 
                        cluster = "mpname",
                        data = dat_lss_ws_sent_stand_change)

lm_sent_cardiff <- fixest::feols(mean_sent_cardiff ~ 
                                     gov_opp + ownership_2 + dublin_dummy + seniority_years + perc_change_mean| year, 
                                 cluster = "mpname", 
                                 data = dat_lss_ws_sent_stand_change)

# run plot for per_change_mean


# function to plot predicted values
plot_pred_continuous <- function(data) {

    ggplot(data = data,
           aes(x = perc_change_mean, y = estimate,
               ymin = estimate - 1.96 * std.error,
               ymax = estimate + 1.96 * std.error)) +
        geom_ribbon(fill = "grey80") +
        geom_line(group = 1) +
        geom_rug(data = dat_lss_ws_sent_stand_change, 
                 aes(x = perc_change_mean),
                 sides = "b",
                 alpha = 0.6,
                 inherit.aes = FALSE) +
        xlab("Percentage Change\nof Average Property Price")

}


p_ws_change <- plot_predictions(lm_ws,
                 condition = "perc_change_mean",
                 draw = TRUE) +
    geom_rug(data = dat_lss_ws_sent_stand_change, 
             aes(x = perc_change_mean),
             sides = "b",
             alpha = 0.4,
             colour = "grey50",
             inherit.aes = FALSE) +
    xlab("Percentage Change\nof Average Property Price") +
    scale_y_continuous(limits = c(-1, 0.5)) +
    labs(y = "Predicted Wordscores Position",
         title = "(a) Wordscores")



p_lss_change <- plot_predictions(lm_lss,
                 condition = "perc_change_mean",
                 draw = TRUE) +
    geom_rug(data = dat_lss_ws_sent_stand_change, 
             aes(x = perc_change_mean),
             sides = "b",
             alpha = 0.4,
             colour = "grey50",
             inherit.aes = FALSE) +
    xlab("Percentage Change\nof Average Property Price") +  
    scale_y_continuous(limits = c(-1, 0.5)) +
    labs(y = "Predicted LSS Score",
         title = "(b) Latent Semantic Scaling")

p_sent_change <- plot_predictions(lm_sent_cardiff,
                 condition = "perc_change_mean",
                 draw = TRUE) +
    geom_rug(data = dat_lss_ws_sent_stand_change, 
             aes(x = perc_change_mean),
             sides = "b",
             alpha = 0.4,
             colour = "grey50",
             inherit.aes = FALSE) +
    xlab("Percentage Change\nof Average Property Price") +  
    scale_y_continuous(limits = c(-1, 0.5)) +
    labs(y = "Predicted Sentiment",
         title = "(c) Sentiment")


# Figure A07
plot_grid(p_ws_change, p_lss_change, p_sent_change,
          nrow = 1)
ggsave("fig_a07.pdf",
       width = 10, height = 5)
ggsave("fig_a07.eps",
       width = 10, height = 5)


# run interaction models

lm_ws_int <- fixest::feols(estimate_wordscores.fit ~  ownership_2 * gov_opp +
                               dublin_dummy + seniority_years + perc_change_mean| year,
                           cluster = "mpname", 
                           data = dat_lss_ws_sent_stand)

lm_lss_int <- fixest::feols(mean_lss ~  ownership_2 * gov_opp + 
                                dublin_dummy + seniority_years +perc_change_mean | year, 
                            cluster = "mpname", 
                            data = dat_lss_ws_sent_stand)


lm_sent_cardiff_int <- feols(mean_sent_cardiff ~ ownership_2  * gov_opp +  
                                 dublin_dummy + seniority_years + perc_change_mean | year, 
                             cluster = "mpname", 
                             data = dat_lss_ws_sent_stand)




screenreg(list(lm_ws,
               lm_lss,
               lm_sent_cardiff,
               lm_ws_int,
               lm_lss_int,
               lm_sent_cardiff_int))


coef_names <- list(
    "ownership_2Homeowner or Landlord" = "Landlord (ref.: No Landlord)",
    "gov_oppOpposition" = "Opposition (ref.: Government)",
    "dublin_dummyDublin" = "Dublin Constituencies",
    "seniority_years" = "Seniority (Years)",
    "perc_change_mean" = "Change in Property Prices",
    "ownership_2Homeowner or Landlord:gov_oppOpposition" = "Landlord $\\times$ Opposition"
)

# save as table
texreg(list(lm_ws,
            lm_lss,
            lm_sent_cardiff,
            lm_ws_int,
            lm_lss_int,
            lm_sent_cardiff_int),
       include.var = TRUE,
       include.adjrs = TRUE,
       include.proj.stats = FALSE,
       include.pseudors = FALSE,
       custom.coef.map = coef_names,
       fontsize = "footnotesize",
       caption.above = TRUE,
       custom.gof.names = c(
           "Num. obs",
           "Fixed Effects: Year",
           "R$^2$", "Adj. R$^2$"
       ),
       label = "tab:reg_main_positions",
       caption = "Predicting positions on housing policy. Robust standard errors, clustered by politician, in parentheses.",
       custom.model.names = c("M1: WS", 
                              "M2: LSS",
                              "M3: Sent.",
                              "M4: WS", 
                              "M5: LSS",
                              "M6: Sent."),
       file = "tab_06.tex")

htmlreg(list(lm_ws,
            lm_lss,
            lm_sent_cardiff,
            lm_ws_int,
            lm_lss_int,
            lm_sent_cardiff_int),
       include.var = TRUE,
       include.adjrs = TRUE,
       include.proj.stats = FALSE,
       include.pseudors = FALSE,
       custom.coef.map = coef_names,
       fontsize = "footnotesize",
       caption.above = TRUE,
       custom.gof.names = c(
           "Num. obs",
           "Fixed Effects: Year",
           "R$^2$", "Adj. R$^2$"
       ),
       label = "tab:reg_main_positions",
       caption = "Predicting positions on housing policy. Robust standard errors, clustered by politician, in parentheses.",
       custom.model.names = c("M1: WS", 
                              "M2: LSS",
                              "M3: Sent.",
                              "M4: WS", 
                              "M5: LSS",
                              "M6: Sent."),
       file = "tab_06.html")

       
# function for first differences

first_diff <- function(x) {
    # run first difference analysis here
    
    #https://iqss.github.io/clarify/reference/sim_setx.html
    
    sim_model <- clarify::sim(x, n = 1000,
                              vcov = sandwich::vcovCL(x, 
                                                      cluster = ~ mpname))
    
    fd_opp_raw <- sim_setx(
        sim_model,
        x = data.frame(ownership_2 = "Not Homeowner or Landlord",
                       gov_opp = "Opposition"),
        x1 = data.frame(ownership_2 = "Homeowner or Landlord",
                        gov_opp = "Opposition"),
        verbose = FALSE)
    
    print(fd_opp_raw)
    
    fd_opp <- fd_opp_raw |>
        as.data.frame() |>
        summarise(estimate = mean(FD),
                  conf.low = quantile(FD, 0.025),
                  conf.high = quantile(FD, 0.975)) |>
        mutate(model = "Opposition:\nLandlord vs. No Landlord")
    
    fd_gov_raw <- sim_setx(
        sim_model,
        x = list(ownership_2 = "Not Homeowner or Landlord",
                 gov_opp = "Government"),
        x1 = list(ownership_2 = "Homeowner or Landlord",
                  gov_opp = "Government"),
        verbose = TRUE)
    
    print(fd_gov_raw)
    
    fd_gov <- fd_gov_raw |>
        as.data.frame() |>
        summarise(estimate = mean(FD),
                  conf.low = quantile(FD, 0.025),
                  conf.high = quantile(FD, 0.975)) |>
        mutate(model = "Government:\nLandlord vs. No Landlord")
    
    fd_both <- bind_rows(fd_opp, fd_gov)
    
    return(fd_both)
}



# run split sample: gov/opp


lm_ws_gov <- fixest::feols(estimate_wordscores.fit ~ 
                           ownership_2 + dublin_dummy + seniority_years  + perc_change_mean | year,
                       cluster = "mpname", 
                       data = filter(dat_lss_ws_sent_stand_change,
                                     gov_opp == "Government"))

lm_ws_opp <- update(lm_ws_gov,
                    data = filter(dat_lss_ws_sent_stand_change,
                                         gov_opp == "Opposition"))


lm_lss_gov <- fixest::feols(mean_lss ~ 
                               ownership_2 + dublin_dummy + seniority_years  + perc_change_mean | year,
                           cluster = "mpname", 
                           data = filter(dat_lss_ws_sent_stand_change,
                                         gov_opp == "Government"))

lm_lss_opp <- update(lm_lss_gov,
                    data = filter(dat_lss_ws_sent_stand_change,
                                  gov_opp == "Opposition"))


lm_sentiment_gov <- fixest::feols(mean_sent_cardiff ~ 
                                ownership_2 + dublin_dummy + seniority_years  + perc_change_mean | year,
                            cluster = "mpname", 
                            data = filter(dat_lss_ws_sent_stand_change,
                                          gov_opp == "Government"))

lm_sentiment_opp <- update(lm_sentiment_gov,
                      data = filter(dat_lss_ws_sent_stand_change,
                                    gov_opp == "Opposition"))

# save as table

screenreg(list(lm_ws_gov,
            lm_ws_opp, 
            lm_lss_gov, 
            lm_lss_opp,
            lm_sentiment_gov,
            lm_sentiment_opp))


texreg(list(lm_ws_gov,
            lm_ws_opp, 
            lm_lss_gov, 
            lm_lss_opp,
            lm_sentiment_gov,
            lm_sentiment_opp),
       include.var = TRUE,
       include.adjrs = TRUE,
       include.proj.stats = FALSE,
       include.pseudors = FALSE,
       custom.coef.map = coef_names,
       custom.gof.names = c(
           "Num. obs",
           "Fixed Effects: Year",
           "R$^2$", "Adj. R$^2$"
       ),
       fontsize = "scriptsize",
       caption.above = TRUE,
       label = "tab:reg_main_positions_splitsample",
       caption = "Predicting positions on housing policy. Models 1, 3, and 5 are limited to government TDs; Models 2, 4, and 6 are limited to opposition TDs. Robust standard errors, clustered by politician, in parentheses.",
       custom.model.names = c("M1: WS (G)", 
                              "M2: WS (O)",
                              "M3: LSS (G)",
                              "M4: LSS (O)",
                              "M5: S (G)",
                              "M6: S (O)"),
       file = "tab_a10.tex")


htmlreg(list(lm_ws_gov,
            lm_ws_opp, 
            lm_lss_gov, 
            lm_lss_opp,
            lm_sentiment_gov,
            lm_sentiment_opp),
       include.var = TRUE,
       include.adjrs = TRUE,
       include.proj.stats = FALSE,
       include.pseudors = FALSE,
       custom.coef.map = coef_names,
       custom.gof.names = c(
           "Num. obs",
           "Fixed Effects: Year",
           "R$^2$", "Adj. R$^2$"
       ),
       fontsize = "scriptsize",
       caption.above = TRUE,
       label = "tab:reg_main_positions_splitsample",
       caption = "Predicting positions on housing policy. Models 1, 3, and 5 are limited to government TDs; Models 2, 4, and 6 are limited to opposition TDs. Robust standard errors, clustered by politician, in parentheses.",
       custom.model.names = c("M1: WS (G)", 
                              "M2: WS (O)",
                              "M3: LSS (G)",
                              "M4: LSS (O)",
                              "M5: S (G)",
                              "M6: S (O)"),
       file = "tab_a10.html")


# get subset of variables that are not na and in regression below
dat_lss_ws_sent_stand_fd <- dat_lss_ws_sent_stand |> 
    dplyr::select(estimate_wordscores.fit,
                  mean_lss,
                  mean_sent_cardiff,cycle,
                  ownership_2, mpname, perc_change_mean, 
                  gov_opp, dublin_dummy,
                  seniority_years, year) |> 
    drop_na()


lm_ws_int_fd <- fixest::feols(estimate_wordscores.fit ~ gov_opp * ownership_2 +
                                  dublin_dummy + seniority_years + perc_change_mean + year,
                              cluster = "mpname", 
                              data = dat_lss_ws_sent_stand_fd)

lm_lss_int_fd <- fixest::feols(mean_lss ~ gov_opp * ownership_2 + 
                                   dublin_dummy + seniority_years + perc_change_mean + year, 
                               cluster = "mpname", 
                               data = dat_lss_ws_sent_stand_fd)


lm_sent_cardiff_int_fd <- feols(mean_sent_cardiff ~ gov_opp * ownership_2 +  
                                    dublin_dummy + seniority_years + perc_change_mean + year, 
                                cluster = "mpname", 
                                data = dat_lss_ws_sent_stand_fd)


fd_sent_cardiff <- first_diff(x = lm_sent_cardiff_int_fd) |> 
    mutate(dv = "Sentiment")

fd_lss <- first_diff(x = lm_lss_int_fd) |> 
    mutate(dv = "Latent Semantic Scaling")

fd_wordscores <- first_diff(x = lm_ws_int_fd) |> 
    mutate(dv = "Wordscores")

# combine into single data frame
dat_fd_fullsample <- bind_rows(fd_wordscores,
                               fd_lss,
                               fd_sent_cardiff) |> 
    mutate(dv = factor(dv, levels = c("Wordscores", 
                                      "Latent Semantic Scaling",
                                      "Sentiment"))) |> 
    mutate(year = "Full Sample")


# for each cycle
dat_fd_cycles <- data.frame()

# get cycles
cycles <- unique(dat_lss_ws_sent_stand$cycle)

cycles

for (i in cycles) {
    
    dat_cycle <- filter(dat_lss_ws_sent_stand_fd, cycle == i)
    lm_ws_int_year <- fixest::feols(estimate_wordscores.fit ~ gov_opp * ownership_2 +
                                        dublin_dummy + seniority_years,
                                    cluster = "mpname", data = dat_cycle)
    
    lm_lss_int_year <- fixest::feols(mean_lss ~ gov_opp * ownership_2 +
                                         dublin_dummy + seniority_years, 
                                     cluster = "mpname", data = dat_cycle)
    
    lm_sent_cardiff_int_year <- feols(mean_sent_cardiff ~ gov_opp * ownership_2 +
                                          dublin_dummy + seniority_years, 
                                      cluster = "mpname", data = dat_cycle)
    
    
    fd_sent_cardiff <- first_diff(x = lm_sent_cardiff_int_year) |> 
        mutate(dv = "Sentiment")
    
    fd_lss <- first_diff(x = lm_lss_int_year) |> 
        mutate(dv = "Latent Semantic Scaling")
    
    fd_wordscores <- first_diff(x = lm_ws_int_year) |> 
        mutate(dv = "Wordscores")
    
    fd_combined <- bind_rows(fd_wordscores,
                             fd_lss,
                             fd_sent_cardiff) |> 
        mutate(year = i)
    
    dat_fd_cycles <- bind_rows(dat_fd_cycles, fd_combined)
    
}


dat_fd_cycles <- dat_fd_cycles |>
    mutate(sample = "Cycle-Specific Sample")


dat_fd_all <- bind_rows(dat_fd_fullsample,
                        dat_fd_cycles) |> 
    mutate(type_dummy = ifelse(year == "Full Sample", TRUE, FALSE)) |> 
    mutate(year = factor(year, levels = c("Full Sample",
                                          "2016-2020",
                                          "2020-2022"))) |> 
    mutate(dv = dplyr::recode(dv, "Latent Semantic Scaling" = "Latent\nSemantic Scaling")) |> 
    mutate(dv = factor(dv, levels = c("Wordscores", 
                                      "Latent\nSemantic Scaling",
                                      "Sentiment"))) 


ggplot(dat_fd_all,
       aes(x = estimate, y = fct_rev(factor(year)),
           xmin = conf.low,
           xmax = conf.high,
           colour = type_dummy,
           shape = type_dummy)) +
    geom_vline(xintercept = 0, linetype = "dashed",
               colour = "red") +
    geom_linerange(linewidth = 1) +
    geom_point(size = 4) +
    scale_y_discrete(labels = label_wrap(30)) +
    facet_grid(dv~model) +
    scale_x_continuous(breaks = c(seq(-0.8, 0.8, 0.4)),
                       limits = c(-0.8, 0.8)) +
    annotate("text", x = -0.05, y = 0.7,
             size = 3.5,
             hjust = 1, colour = "grey50",
             label = "Landlords lower") +
    annotate("text", x = 0.05, y = 0.7,
             size = 3.5,
             hjust = 0, colour = "grey50",
             label = "Landlords higher") +
    annotate("segment",
             x = -0.05, xend = -0.55,
             y = 0.5, yend = 0.5, colour = "grey50",
             linewidth = 0.5,
             arrow = arrow(angle = 25,
                           length = unit(0.2, "cm"))) +
    annotate("segment",
             x = 0.05, xend = 0.55,
             y = 0.5, yend = 0.5, colour = "grey50",
             linewidth = 0.5,
             arrow = arrow(angle = 25,
                           length = unit(0.2, "cm"))) +
    scale_color_manual(values = c("grey30", "black")) +
    scale_shape_manual(values = c(16, 15)) +
    theme(legend.position = "none") +
    ylab(NULL) +
    xlab("First Difference in Standard Deviations: Landlord-No Landlord")
ggsave("fig_07.pdf",
       width = 9, height = 6.5)
ggsave("fig_07.eps",
       width = 9, height = 6.5)





# robustness: run models with age continuous,
# age categorical, and number of times elected as proxies
# for personal interests


dat_lss_ws_sent_stand <- dat_lss_ws_sent_stand |> 
    mutate(age_group = case_when(
        age %in% 18:34 ~ "18-34",
        age %in% 35:44 ~ "35-44",
        age %in% 45:54 ~ "45-54",
        age %in% 55:64 ~ "55-64", 
        age > 64 ~ "65+"
    ))


lm_ws_age <- feols(
    estimate_wordscores.fit ~ gov_opp +
        dublin_dummy +  
        perc_change_mean +
        age_group | year,
    cluster = "harmonised",
    data = dat_lss_ws_sent_stand
)

lm_lss_age <- feols(
    mean_lss ~ gov_opp +
        dublin_dummy +  
        perc_change_mean +
        age_group | year,
    cluster = "harmonised",
    data = dat_lss_ws_sent_stand
)


lm_sent_age <- feols(
    mean_sent_cardiff ~ gov_opp +
        dublin_dummy +  
        perc_change_mean +
        age_group | year,
    cluster = "harmonised",
    data = dat_lss_ws_sent_stand
)


texreg(list(lm_ws_age,
            lm_lss_age,
            lm_sent_age),
       include.var = FALSE,
       custom.coef.names = c(
           "Opposition (ref.: Government)",
           "Dublin Constituencies",
           "Change in Property Prices",
           "Age Group: 35-44 (ref.: 18-34)",
           "Age Group: 45-54",
           "Age Group: 55-64",
           "Age Group: 65+"),
       include.var = TRUE,
       include.adjrs = TRUE,
       include.proj.stats = FALSE,
       include.pseudors = FALSE,
       custom.gof.names = c(
           "Num. obs",
           "Fixed Effects: Year",
           "R$^2$", "Adj. R$^2$"
       ),
       fontsize = "footnotesize",
       caption.above = TRUE,
       label = "tab:reg_main_positions_age",
       caption = "Predicting positions on housing policy. Robust standard errors, clustered by politician, in parentheses.",
       custom.model.names = c("M1 (WS)", "M2 (LSS)", "M3 (Sent.)"),
       file = "tab_a14.tex")

htmlreg(list(lm_ws_age,
            lm_lss_age,
            lm_sent_age),
       include.var = FALSE,
       custom.coef.names = c(
           "Opposition (ref.: Government)",
           "Dublin Constituencies",
           "Change in Property Prices",
           "Age Group: 35-44 (ref.: 18-34)",
           "Age Group: 45-54",
           "Age Group: 55-64",
           "Age Group: 65+"),
       include.var = TRUE,
       include.adjrs = TRUE,
       include.proj.stats = FALSE,
       include.pseudors = FALSE,
       custom.gof.names = c(
           "Num. obs",
           "Fixed Effects: Year",
           "R$^2$", "Adj. R$^2$"
       ),
       fontsize = "footnotesize",
       caption.above = TRUE,
       label = "tab:reg_main_positions_age",
       caption = "Predicting positions on housing policy. Robust standard errors, clustered by politician, in parentheses.",
       custom.model.names = c("M1 (WS)", "M2 (LSS)", "M3 (Sent.)"),
       file = "tab_a14.html")

pred_age_ws <- plot_predictions(lm_ws_age, 
                             condition = "age_group",
                             draw = FALSE)

pred_age_lss <- plot_predictions(lm_lss_age, 
                                condition = "age_group",
                                draw = FALSE)

pred_age_sent <- plot_predictions(lm_sent_age, 
                                 condition = "age_group",
                                 draw = FALSE)


p_age_ws <- ggplot(pred_age_ws, aes(x = age_group,
                              y = estimate,
                              ymin = conf.low,
                              ymax = conf.high)) +
    geom_point(size = 3) +
    geom_linerange(linewidth = 0.8) +
    scale_y_continuous(limits = c(-1.5, 0.5)) +
    labs(x = "Age Group", y = "Predicted Salience of Housing",
         title = "(a) Wordscores") +
    theme(axis.text.x = element_text(angle = 90))
p_age_ws

p_age_lss <- ggplot(pred_age_lss, aes(x = age_group,
                                    y = estimate,
                                    ymin = conf.low,
                                    ymax = conf.high)) +
    geom_point(size = 3) +
    geom_linerange(linewidth = 0.8) +
    scale_y_continuous(limits = c(-1.5, 0.5)) +
    labs(x = "Age Group", y = "Predicted Salience of Housing",
         title = "(b) Latent\nSemantic Scaling") +
    theme(axis.text.x = element_text(angle = 90))

p_age_lss


p_age_sent <- ggplot(pred_age_sent, aes(x = age_group,
                                      y = estimate,
                                      ymin = conf.low,
                                      ymax = conf.high)) +
    geom_point(size = 3) +
    geom_linerange(linewidth = 0.8) +
    scale_y_continuous(limits = c(-1.5, 0.5)) +
    labs(x = "Age Group", y = "Predicted Salience of Housing",
         title = "(c) Sentiment") +
    theme(axis.text.x = element_text(angle = 90))

p_age_sent



# Figure A09
plot_grid(p_age_ws, p_age_lss, p_age_sent, nrow = 1,
          align = "vh")
ggsave("fig_a09.pdf",
       width = 9, height = 5)
ggsave("fig_a09.eps",
       width = 9, height = 5)



# get words with lowest scores by year
dat_scores_lowest <- dat_wordscores_words |> 
    group_by(feature) |>
    summarise(mean_score = mean(word_score),
              n_years = n())  |> 
    filter(mean_score > -1 & n_years > 1) |> 
    slice_min(order_by = mean_score, n = 100) |> 
    mutate(type = "Lowest Word Scores")


# get words with highest scores by year
dat_scores_highest <- dat_wordscores_words |> 
    group_by(feature) |>
    summarise(mean_score = mean(word_score),
              n_years = n())  |> 
    filter(mean_score < 1 & n_years > 1) |> 
    slice_min(order_by = -mean_score, n = 100) |> 
    mutate(type = "Highest Word Scores")


# bind terms for each measure (unit of observation is not the measure, rather than the term)
dat_ws_lowhigh <- bind_rows(dat_scores_lowest, dat_scores_highest) |> 
    group_by(type) |>
    summarise(Terms = paste(feature, collapse = ", ")) |> 
    ungroup() |> 
    rename(Dimension = type)

dat_ws_lowhigh <- dat_ws_lowhigh |>  map_df(rev)

# print table
print(
    xtable(dat_ws_lowhigh,
           digits = 1,
           caption = "Terms with the 100 lowest (negative) and highest (positive) word scores in texts about housing.",
           label = "tab:ws_lowhigh",
           align = c(
               "p{0.03\\textwidth}",
               "p{0.2\\textwidth}",
               "p{0.7\\textwidth}"
           )
    ),
    type = "latex",
    digits = 1,
    add.to.row = list(pos=list(1), command="\\hdashline \n"),
    size = "footnotesize",
    file = "tab_a05.tex",
    include.rownames = FALSE,
    caption.placement = "top"
)

print(
    xtable(dat_ws_lowhigh,
           digits = 1,
           caption = "Terms with the 100 lowest (negative) and highest (positive) word scores in texts about housing.",
           label = "tab:ws_lowhigh",
           align = c(
               "p{0.03\\textwidth}",
               "p{0.2\\textwidth}",
               "p{0.7\\textwidth}"
           )
    ),
    type = "html",
    digits = 1,
    add.to.row = list(pos=list(1), command="\\hdashline \n"),
    size = "footnotesize",
    file = "tab_a05.html",
    include.rownames = FALSE,
    caption.placement = "top"
)


# get words with lowest scores by year
dat_scores_lowest_small <- dat_wordscores_words |> 
    group_by(feature) |>
    summarise(mean_score = mean(word_score),
              n_years = n())  |> 
    filter(mean_score > -1 & n_years > 1) |> 
    slice_min(order_by = mean_score, n = 25) |> 
    mutate(type = "Lowest Word Scores")


# get words with highest scores by year
dat_scores_highest_small <- dat_wordscores_words |> 
    group_by(feature) |>
    summarise(mean_score = mean(word_score),
              n_years = n())  |> 
    filter(mean_score < 1 & n_years > 1) |> 
    slice_min(order_by = -mean_score, n = 25) |> 
    mutate(type = "Highest Word Scores")


# bind terms for each measure (unit of observation is not the measure, rather than the term)
dat_ws_lowhigh_small <- bind_rows(dat_scores_lowest_small, dat_scores_highest_small) |> 
    group_by(type) |>
    summarise(Terms = paste(feature, collapse = ", ")) |> 
    ungroup() |> 
    rename(Dimension = type)

dat_ws_lowhigh_small <- dat_ws_lowhigh_small |>  map_df(rev)

# print table
print(
    xtable(dat_ws_lowhigh_small,
           digits = 1,
           caption = "Terms with the 25 lowest (negative) and highest (positive) word scores in texts about housing.",
           label = "tab:ws_lowhigh_small",
           align = c(
               "p{0.03\\textwidth}",
               "p{0.2\\textwidth}",
               "p{0.7\\textwidth}"
           )
    ),
    type = "latex",
    digits = 1,
    add.to.row = list(pos=list(1), command="\\hdashline \n"),
    size = "footnotesize",
    file = "tab_02.tex",
    include.rownames = FALSE,
    caption.placement = "top"
)

print(
    xtable(dat_ws_lowhigh_small,
           digits = 1,
           caption = "Terms with the 25 lowest (negative) and highest (positive) word scores in texts about housing.",
           label = "tab:ws_lowhigh_small",
           align = c(
               "p{0.03\\textwidth}",
               "p{0.2\\textwidth}",
               "p{0.7\\textwidth}"
           )
    ),
    type = "html",
    digits = 1,
    add.to.row = list(pos=list(1), command="\\hdashline \n"),
    size = "footnotesize",
    file = "tab_02.html",
    include.rownames = FALSE,
    caption.placement = "top"
)


dat_feats <- bind_rows(dat_scores_highest, dat_scores_lowest)

dat_feats <- data.frame(
    Lowest = paste0(dat_scores_lowest$feature, " (", round(dat_scores_lowest$mean_score, 3), ")"),
    Highest = paste0(dat_scores_highest$feature, " (", round(dat_scores_highest$mean_score, 3), ")")
    
)

dat_scalingestimates <- dat_scalingestimates |> 
    mutate(age_group = case_when(
        age %in% 18:34 ~ "18-34",
        age %in% 35:44 ~ "35-44",
        age %in% 45:54 ~ "45-54",
        age %in% 55:64 ~ "55-64", 
        age > 64 ~ "65+"
    ))





lm_sent_cardiff_int_year <- feols(mean_sent_cardiff ~ gov_opp * ownership_2 +
                                      dublin_dummy + seniority_years, 
                                  cluster = "mpname", data = dat_cycle)





dat_lss_ws_sent_stand <- dat_lss_ws_sent_stand |> 
    mutate(propnum_recoded = ifelse(propnum >= 4, "4+", propnum)) |> 
    mutate(propnum_recoded = as.factor(propnum_recoded)) |> 
    mutate(propnum_dummy = factor(ifelse(propnum > 0 , 1, 0)))

dat_lss_ws_sent_stand$ownership_3 <- relevel(factor(dat_lss_ws_sent_stand$ownership_3),
                                             ref = "Not Homeowner or Landlord")



dat_models_all_scaling <- data.frame()

variables <- c("propnum_recoded",
               "ownership_2",
               "ownership_3")



for (i in variables) {
    
    cat("Running models using", i, "\n")
    
    lm_ws <- feols(
        as.formula(paste0("estimate_wordscores.fit ~ ",
                          i, " + gov_opp +
            dublin_dummy + seniority_years | year")), 
        cluster = "mpname",
        data = dat_lss_ws_sent_stand) |> 
        broom::tidy() |> 
        mutate(model = "Wordscores",
               variable = i)
    
    
    lm_lss <- feols(
        as.formula(paste0("mean_lss ~ ",
                          i, " + gov_opp +
            dublin_dummy + seniority_years | year")), 
        cluster = "mpname",
        data = dat_lss_ws_sent_stand) |> 
        broom::tidy() |> 
        mutate(model = "Latent Semantic Scaling",
               variable = i)
    
    lm_sent <- feols(
        as.formula(paste0("mean_sent_cardiff ~ ",
                          i, " + gov_opp +
            dublin_dummy + seniority_years | year")), 
        cluster = "mpname",
        data = dat_lss_ws_sent_stand) |> 
        broom::tidy() |> 
        mutate(model = "Sentiment",
               variable = i)
    
    lm_all_var <- bind_rows(lm_sent, lm_lss, lm_ws)
    
    dat_models_all_scaling <- bind_rows(lm_all_var, dat_models_all_scaling)
    
}


dat_models_all_scaling |> 
    filter(str_detect(term, "propnum|ownership")) |>
    mutate(term = gsub(c("ownership_2|ownership_3|propnum_recoded"), "", term)) |>
    mutate(variable = dplyr::recode(
        variable, "ownership_2" = "2 Categories",
        "ownership_3" = "3 Categories",
        "propnum_dummy" = "Register:\nAt Least One Entry",
        "propnum_recoded" = "Register:\nNumber of Entries",
    )) |> 
    mutate(term = dplyr::recode(term, "Homeowner or Landlord" = "Landlord or Multiple Properties\n(ref.: Other)")) |> 
    mutate(term = dplyr::recode(term, "Homeowner Only" = "Multiple Properties")) |> 
    mutate(term = dplyr::recode(term, "Landlord" = "Landlord with Rental Income\n(ref.: Other)")) |> 
    mutate(term = dplyr::recode(term, "1" = "1 (ref.: 0)")) |> 
    mutate(model = str_replace_all(model, "Latent ", "Latent\n")) |> 
    mutate(model = factor(model, levels = c("Wordscores", "Latent\nSemantic Scaling", "Sentiment"))) |> 
    ggplot(aes(x = forcats::fct_rev(term), y = estimate,
               ymin = estimate - 1.96 * std.error, 
               ymax = estimate  + 1.96 * std.error)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
    geom_point(size = 3.5) +
    geom_linerange(aes(ymin = estimate - 1.96 * std.error,
                       ymax = estimate + 1.96 * std.error),
                   linewidth = 0.8) +
    coord_flip() +
    facet_grid(variable~model, space = "free", scales = "free_y") +
    labs(x = "Indicator of Private Interests", 
         y = "Coefficient on Housing Positions") +
    theme(strip.text.y = element_text(angle = 0, hjust = 0, size = 13),
          strip.text.x = element_text(size = 13))
ggsave("fig_05.pdf",
       width = 9.25, height = 5)
ggsave("fig_05.eps",
       width = 9.25, height = 5)


# recode factor levels
dat_scalingestimates$dublin_dummy <- relevel(factor(dat_scalingestimates$dublin_dummy),
                                             ref = "Other")



dat_scalingestimates <- dat_scalingestimates |> 
    mutate(party_broad = factor(party_broad, 
                                levels = c("Fine Gael",
                                           "Fianna Fáil",
                                           "Other/Independents",
                                           "Green Party and Left Bloc",
                                           "Sinn Féin")))


dat_scalingestimates <- dat_scalingestimates |> 
    mutate(ref_text = ifelse(!is.na(housing_pos), "Reference Texts", "Other Texts")) 


dat_scalingestimates_density <- dat_scalingestimates |> 
    mutate(gov_opp = fct_rev(gov_opp))



# plot for sentiment, wordscores, and lss
# use standardised measures (mean 0, sd = 1)
dat_scores_long <- dat_lss_ws_sent_stand |> 
    select(estimate_wordscores.fit,
           mean_lss,
           mean_sent_cardiff, year, gov_opp) |> 
    gather(measure, value, -c(year, gov_opp)) |> 
    mutate(measure = dplyr::recode(measure, 
                                   "estimate_wordscores.fit" = "Wordscores",
                                   "mean_lss" = "Latent Semantic Scaling",
                                   "mean_sent_cardiff" = "Sentiment")) |> 
    mutate(measure = factor(measure, levels = c(
        "Wordscores", "Latent Semantic Scaling", "Sentiment"
    )))


ggplot(dat_scores_long,
       aes(x = value,
           colour = gov_opp,
           linetype = gov_opp,
           fill = gov_opp)) +
    geom_density(alpha = 0.3, size = 0.4) +
    geom_rug(alpha = 0.4) +
    scale_colour_manual(values = c("black", "grey60")) +
    scale_fill_manual(values = c("black", "grey60")) +
    facet_grid(year~measure, scales = "free")  +
    labs(x = "Estimates",
         y = NULL) +
    scale_linetype_manual(values=c("dashed", "solid"))+
    #scale_x_continuous(breaks = c(seq(-0.4, 0.6, 0.2))) +
    theme(legend.title = element_blank(),
          strip.text.y = element_text(angle = 0, hjust = 0),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank())
ggsave("fig_a05.pdf",
       width = 9, height = 10)

ggsave("fig_a05.eps",
       width = 9, height = 10)

dat_scores_long$gov_opp <- factor(dat_scores_long$gov_opp, 
                                  levels = c("Opposition",
                                             "Government"))

ggplot(dat_scores_long,
       aes(x = value,
           colour = gov_opp,
           linetype = gov_opp,
           fill = gov_opp)) +
    geom_density(alpha = 0.3, size = 0.4) +
    geom_rug(alpha = 0.4) +
    scale_colour_manual(values = c("grey60", "black")) +
    scale_fill_manual(values = c("grey60", "black")) +
    facet_wrap(~measure, scales = "free")  + 
    labs(x = "Estimates",
         y = NULL) +
    scale_linetype_manual(values = c("solid", "dashed"))+
    theme(legend.title = element_blank(),
          strip.text.y = element_text(angle = 0, hjust = 0),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          legend.position = c(0.24, 0.83))
ggsave("fig_02.pdf",
       width = 9, height = 3)
ggsave("fig_02.eps",
       width = 9, height = 3)


# compare changes in positions after becoming landlord
dat_compare_changes_all <- dat_lss_ws_sent_stand |> 
    arrange(candidate, year) |> 
    group_by(candidate) |> 
    mutate(ownership_2_lag = lag(ownership_2)) |> 
    mutate(change_became_owner = ifelse(ownership_2 == "Homeowner or Landlord" & ownership_2_lag == "Not Homeowner or Landlord", 1, 0)) |> 
    dplyr::select(candidate, year, party_recoded, ownership_2,
                  change_became_owner, gov_opp,
                  mean_lss, mean_sent_cardiff, estimate_wordscores.fit) |>
    gather(measure, position, -c(candidate, year, gov_opp,
                                 party_recoded, ownership_2,
                                 change_became_owner)) |>
    arrange(measure, candidate, year) |> 
    group_by(measure, candidate) |> 
    mutate(lag_position = lag(position)) |> 
    mutate(after_minus_before = position - lag_position) 




table(dat_compare_changes_all$change_became_owner)

# select first time they became owner
dat_compare_changes_first <- dat_compare_changes_all |> 
    filter(change_became_owner == 1) |> 
    arrange(measure, candidate, year) |>
    group_by(measure, candidate) |> 
    slice(1) 

length(unique(dat_compare_changes_first$candidate))

dat_analysis_change_clean <- dat_compare_changes_first |> 
    mutate(name_clean = str_to_title(candidate)) |> 
    mutate(name_clean = str_replace_all(name_clean, "O'r", "O'R")) |> 
    mutate(name_clean = str_replace_all(name_clean, "O'd", "O'D")) |> 
    mutate(name_clean = str_replace_all(name_clean, "O's", "O'S")) |> 
    mutate(name_clean = str_replace_all(name_clean, "Mcgrath", "McGrath")) |> 
    mutate(name_clean = paste0(name_clean, " (", year, ")")) |> 
    filter(!is.na(after_minus_before)) |> 
    mutate(measure = dplyr::recode(measure,
                                   "mean_lss" = "Latent\nSemantic Scaling",
                                   "mean_sent_cardiff" = "Sentiment",
                                   "estimate_wordscores.fit" = "Wordscores")) |> 
    mutate(measure = factor(measure, levels = c("Wordscores",
                                                "Latent\nSemantic Scaling",
                                                "Sentiment"))) 




dat_avg_pos <- dat_analysis_change_clean |> 
    group_by(measure) |> 
    summarise(
        mean = mean(after_minus_before, na.rm = TRUE), # Calculate mean
        sd = sd(after_minus_before, na.rm = TRUE), # Calculate standard deviation
        n = n(), # Count observations
        se = sd / sqrt(n) # Calculate Standard Error
    ) %>%
    mutate(
        ci_lower = mean - (1.96 * se), # Calculate lower bound of the 95% CI
        ci_upper = mean + (1.96 * se) # Calculate upper bound of the 95% CI
    ) |> 
    mutate(label = paste0("Mean: ",
                          sprintf("%.2f", round(mean, 2)), 
                          "\n95% CI:\n[", sprintf("%.2f", round(ci_lower, 2)), ", ",
                          sprintf("%.2f", round(ci_upper, 2)), "]"))


ggplot(dat_analysis_change_clean, 
       aes(x = after_minus_before,
           colour = gov_opp, shape = gov_opp,
           y = fct_rev(name_clean))) +
    facet_wrap(~measure) +
    geom_segment(data = dat_analysis_change_clean,
                 aes(x = 0, y = fct_rev(name_clean), 
                     xend = after_minus_before,
                     yend = fct_rev(name_clean))) +
    geom_point(data = dat_analysis_change_clean,
               size = 3) +
    geom_vline(data = dat_avg_pos, 
               aes(xintercept =  mean),
               colour = "red") +
    geom_vline(data = dat_avg_pos,
               aes(xintercept = ci_lower),
               colour = "red", linetype = "dashed") +
    geom_vline(data = dat_avg_pos,
               aes(xintercept = ci_upper),
               colour = "red", linetype = "dashed") +
    geom_text(data = dat_avg_pos, aes(label = label,
                                  x = -1.7, y = 25),
              colour = "red", size = 3.5,
              inherit.aes = FALSE, hjust = 0.5) +
    scale_shape_manual(values = c(16, 15)) +
    scale_colour_manual(values = c("black", "grey50")) +

    labs(x = "Difference in Standardised Positions on Housing\nAfter Becoming Landlord",
         y  = NULL) +
    ggplot2::annotate("text", x = 0.6, y = 1.2,
                      hjust = 0, colour = "grey20",
                      size = 4,
                      label = "Increase ") +
    ggplot2::annotate("text", x = -0.6 , y = 1.2,
                      hjust = 1, colour = "grey20",
                      size = 4,
                      label = "Decrease") +
    ggplot2::annotate("segment",
                      x = 0.5, xend = 2.5,
                      y = 0.7, yend = 0.7, colour = "grey20",
                      size = 0.5,
                      arrow = arrow(angle = 25,
                                    length = unit(0.3, "cm"))) +
    scale_x_continuous(limits = c(-3, 3)) +
    annotate("segment",
             x = -0.5, xend = -2.5,
             y = 0.7, yend = 0.7, colour = "grey20",
             size = 0.5,
             arrow = arrow(angle = 25,
                           length = unit(0.3, "cm"))) +
    theme(legend.title = element_blank())
ggsave("fig_a13.pdf",
       width = 9, height = 8)
ggsave("fig_a13.eps",
       width = 9, height = 8)

