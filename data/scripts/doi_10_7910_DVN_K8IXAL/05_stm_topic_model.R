#' ---
#' title: "Catalysts for progress? Mapping policy insights from energy research (ERSS)"
#' subtitle: "05_stm_topic_model.R"
#' author: "Authors: Brian Boyle, Yen-Chieh Liao, Sarah King, Robin Rauner, and Stefan Müller"
#' ---

# load required R packages
library(tidyverse) # CRAN v2.0.0
library(here) # CRAN v1.0.1
library(quanteda) # CRAN v4.1.0
library(quanteda.textstats) # CRAN v0.97.2
library(stm) # CRAN v1.3.7
library(furrr) # CRAN v0.3.1
library(patchwork) # CRAN v1.2.0
plan(multisession)
library(xtable) # CRAN v1.8-4
library(Hmisc) # CRAN v5.1.3

# load custom ggplot2 scheme
source("function_theme_base.R")

# If the code does not run, one or more packages may have been
# updated, which may result in errors or conflicts. You can solve this issue
# by installing the package version listed above or by using the
# groundhog package:
# after installing groundhog using install.packages("groundhog")
# change library(name_of_package) to
# groundhog::groundhog.library(name_of_package, date = "2024-01-31")
# Instead of adjusting the library() function for each package,
# you can adjust them at all once using the
# the following syntax:
# groundhog.library("library('pkgA')
#                   library('pkgB')
#                   library('pkgC')", date = "2024-01-31")
# More details are available at: https://groundhogr.com/using/

# Note: This script requires data containing the text of abstracts.
# Due to Scopus' data-sharing policies, these files cannot be shared publicly.
# Please contact us if you would like to reproduce this script.

# Model diagnostics
# Workflow
# Abstracts collected for top 100 ranked energy journals by SJR rankings
# Filtered to remove non-journal articles, and those containing valid doi and author info
# Second filter applied to identify climate and energy publications, based on keywords

# Gives us 270,537 article abstracts

# Import data
# Dictionary classification used to identify policy statements within abstracts
abs_coded <- readRDS("data_dontshare/data_scopus_1_100_2010_2023_classified_dontshare.rds")

# 40,468 abstracts contain a policy statement
# 15% of corpus
table(abs_coded$policy_statement)
mean(abs_coded$policy_statement)

theme_set(theme_baser())

# Filter only abstracts flagged as containing policy statements
abs_ps <- abs_coded |>
    filter(policy_statement == 1) |>
    select(journal_name, rank, article_cover_date, article_title,
        abstract_text = text, article_keywords,
        source_id, publication_type_sub, journal_issn,
        article_scopus_id, article_year, article_doi
    )

# Remove original object with all abstracts, as only modelling policy statement ones
rm(abs_coded)

## Approaches for exploring sub-topics of policy statements
# Use the author attached keywords for each article
# Use the abstract text in topic models

# 1. Create corpus and DFM


# Text preprocessing

# Create a dictionary of common and uninformative words in academic abstracts

stopwords_academic <-
    c(
        "model", "models", "using", "find", "findings", "can", "data", "paper",
        "results", "study", "used", "use", "may", "approach", "analysis", "also", "based",
        "found", "article", "method", "however", "due", "elsevier", "ltd", "rights",
        "reserved", "effect", "causality", "all", "total", "methods", "many",
        "proposed", "different", "within", "two", "show", "this paper"
    )

# Create corpus

corp_abs <- abs_ps |>
    corpus(text_field = "abstract_text")

# Run pre-processing steps and convert to tokens object

tokens_abs <- tokens(corp_abs,
    remove_numbers = TRUE,
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_separators = TRUE,
    remove_url = TRUE
) |>
    tokens_tolower() |>
    tokens_remove(c(stopwords("english"), stopwords_academic)) |>
    tokens_select(min_nchar = 3)

# Check for collocations, i.e. multi-word tokens, based on corpus text
set.seed(235)
tokens_colloc <- tokens_abs |>
    tokens_sample(size = 5000) |>
    textstat_collocations(size = 2, min_count = 5, tolower = TRUE)

# select top 200 multi-word expressions to collocate
top_cols <- tokens_colloc[1:200, ]

# Add collocations into tokens dataset
toks_abs_cols <- tokens_compound(tokens_abs, pattern = top_cols)

# Create document feature matrix from tokens object
dfm_abs <- dfm(toks_abs_cols) |>
    dfm_trim(min_termfreq = 5, min_docfreq = 1)

dfm_3y <- dfm_subset(dfm_abs, article_year %in% c(2010, 2015, 2020))

# Prepare dfm for topic model, set metadata

# Filter DFM to keep on 3 years

# We can run topic models with different numbers of topics
# and compare model evaluation metrics
# Set 'k_test' object to TRUE to run
# Note: warning message "Dropped 27,306 zero-count feature(s)"
# will appear because the subsetted dfm does not contain many of the
# features from the full (=all years) dfm

n_topics <- c(5, 10, 15, 20)

stm_ntest <- tibble(K = n_topics) |>
    mutate(topic_model = future_map(K, ~ stm(dfm_3y, K = ., verbose = FALSE, seed = 135),
        .options = furrr_options(seed = TRUE)
    ))

# Extract evaluation metrics from each model
heldout <- make.heldout(dfm_3y, seed = 235)

set.seed(25)
stm_eval <- stm_ntest |>
    mutate(
        exclusivity = map(topic_model, exclusivity),
        semantic_coherence = map(topic_model, semanticCoherence, dfm_3y),
        eval_heldout = map(topic_model, eval.heldout, heldout$missing),
        residual = map(topic_model, checkResiduals, dfm_3y),
        bound = map_dbl(topic_model, function(x) max(x$convergence$bound)),
        lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
        lbound = bound + lfact,
        iterations = map_dbl(topic_model, function(x) length(x$convergence$bound))
    )

# 03 Model evaluation and inspection

# Figure A6
# Plot model evaluation statistics across range of topics

ntopics_eval_plot <- stm_eval |>
    transmute(K,
        `Lower bound` = lbound,
        Residuals = map_dbl(residual, "dispersion"),
        `Semantic coherence` = map_dbl(semantic_coherence, mean),
        `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")
    ) |>
    gather(Metric, Value, -K) |>
    ggplot(aes(K, Value)) +
    geom_vline(xintercept = 10, linetype = "dashed") +
    geom_line(linewidth = 1.5, alpha = 0.7, show.legend = FALSE) +
    facet_wrap(~Metric, scales = "free_y") +
    labs(
        x = "K (Number of Topics)",
        y = NULL,
        title = "(a) Model Diagnostics by Number of Topics"
    )

# Compare exclusivity vs coherence for specific numbers of topics
ibm_five <- c("#648FFF", "#785EF0", "#DC267F", "#FE6100", "#FFB000")

ntopics_excl_sem_plot <- stm_eval |>
    select(K, exclusivity, semantic_coherence) |>
    filter(K %in% n_topics) |>
    unnest(cols = c(exclusivity, semantic_coherence)) |>
    mutate(K = as.factor(K)) |>
    ggplot(aes(semantic_coherence, exclusivity, shape = K, color = K)) +
    scale_colour_manual(values = ibm_five) +
    geom_point(size = 3) +
    labs(
        x = "Semantic Coherence",
        y = "Exclusivity",
        colour = "Number of Topics",
        shape = "Number of Topics",
        title = "Comparing Exclusivity and Semantic Coherence"
    )

topic_eval_plot <- ntopics_eval_plot / ntopics_excl_sem_plot

topic_eval_plot
ggsave("fig_A6.png", topic_eval_plot, width = 9, height = 11, dpi = 300)
ggsave("fig_A6.pdf", topic_eval_plot, width = 9, height = 11)
ggsave("fig_A6.eps", topic_eval_plot, width = 9, height = 11)


# Prepare dfm for topic model, set metadata

dfm_abs_md <- quanteda::convert(dfm_abs, to = "stm")
dfm_abs_md$meta <- docvars(dfm_abs)

# Check number of documents
ndoc(dfm_abs)

# Run STM model with 10 topics (will take time to run)

# Run model
stm_md_10 <- stm(
    documents = dfm_abs_md$documents,
    vocab = dfm_abs_md$vocab,
    K = 10, # Number of topics
    data = dfm_abs_md$meta,
    seed = 123,
    verbose = TRUE
)

# inspect words for each document
plot(stm_md_10, n = 6)

# transform to data frame
doc_topic_df <- make.dt(stm_md_10, meta = dfm_abs_md$meta)

# get averages of each topic
dtp <- doc_topic_df |>
    select(starts_with("Topic")) |>
    colMeans()

# 2.1 Topic topwords and labels

topic_topwords <- stm_md_10 |>
    labelTopics(
        topics = NULL,
        n = stm_md_10$settings$dim$K,
        frexweight = .5
    )

# Topwords Frex
topwords_frex <- topic_topwords$frex |>
    t() |>
    as.data.frame() |>
    as.list() |>
    lapply(paste, collapse = ", ")

names(topwords_frex) <- NULL

# Topwords Highest prob
topwords_hp <- topic_topwords$prob |>
    t() |>
    as.data.frame() |>
    as.list() |>
    lapply(paste, collapse = ", ")

names(topwords_hp) <- NULL

df_f <- data.frame(topic = 1:10, Topwords = paste("(FREX)", unlist(topwords_frex)))
df_h <- data.frame(topic = 1:10, Topwords = paste("(Highest)", unlist(topwords_hp)))

# print for manual labeling
df_h

topic_labs <- data.frame(
    topic = paste0("Topic", 1:10),
    lab = c(
        "Emissions",
        "Economy",
        "Implementation",
        "Biomass",
        "Sustainability",
        "Household Energy",
        "Consumers",
        "Ecology",
        "Energy Systems",
        "Renewable Energy"
    )
)

df_h <- tibble(
    Topic = topic_labs$lab,
    `Topic prevalence` = round(dtp, 4),
    Topwords = unlist(topwords_hp)
) |>
    arrange(-dtp)

df_h_print <- df_h |>
    mutate(`Topic prevalence` = scales::percent(`Topic prevalence`, accuracy = 0.1)) |>
    rename(`Words with highest probabilities` = Topwords)

# Table 2
print(
    xtable(df_h_print,
        label = "tab:topwords",
        align = c(
            "p{0.05\\textwidth}",
            "p{0.2\\textwidth}",
            "p{0.15\\textwidth}",
            "p{0.5\\textwidth}"
        ),
        caption = "STM topic labels and associated topwords in policy-relevant article abstracts"
    ),
    type = "latex",
    table.placement = "!h",
    caption.placement = "top",
    file = "tab_02.tex",
    size = "footnotesize",
    include.rownames = FALSE
)

print(
    xtable(df_h_print,
        caption = "STM topic labels and associated topwords in policy-relevant article abstracts"
    ),
    type = "html",
    table.placement = "!h",
    caption.placement = "top",
    file = "tab_02.html",
    size = "footnotesize",
    include.rownames = FALSE
)

lab_order <- df_h |>
    arrange(-`Topic prevalence`) |>
    pull(Topic)

# Top Documents
docs <- findThoughts(stm_md_10,
    n = 25,
    text = abs_ps$abstract_text
)

# Get doc DOIs
docs_df <- docs$docs |>
    as.data.frame() |>
    t() |>
    as.data.frame()
docs_df$topic <- gsub("\\.", "", rownames(docs_df))
rownames(docs_df) <- NULL

docs_df <- docs_df |>
    left_join(topic_labs, by = "topic")

docs_df <- docs_df |>
    select(topic, lab, everything())

# docs_df <- docs_df |>
#   select(topic, lab, doc1 = V1, doc2 = V2, doc3 = V3)

# Get docs index
index_df <- docs$index |>
    as.data.frame() |>
    t() |>
    as.data.frame()
index_df$topic <- gsub("\\.", "", rownames(index_df))
rownames(index_df) <- NULL

index_df <- index_df |>
    left_join(topic_labs, by = "topic")

index_df <- index_df |>
    select(topic, lab, everything()) |>
    pivot_longer(starts_with("V"), names_to = "doc", values_to = "index")

index_df$scopus_id <- abs_ps$article_scopus_id[index_df$index]
index_df$abstract_text <- abs_ps$abstract_text[index_df$index]
index_df$article_doi <- abs_ps$article_doi[index_df$index]

# save abstracts with high topic proportions for manual validation
write.csv(index_df, "data_dontshare/topdocs_expanded.csv", row.names = FALSE)

topdocs <- index_df |>
    mutate(
        topic = str_replace_all(topic, "Topic", "Topic "),
        doc = str_replace_all(doc, "doc", "Doc ")
    ) |>
    mutate(
        topic_lab = paste0(topic, ": ", lab),
        doc_lab = paste0(doc, ": ", article_doi),
        text = paste0("\n\n", topic_lab, "\n", doc_lab, "\n", abstract_text)
    )

topdocs |>
    # filter(topic == 'Topic 1') |>
    pull(text) |>
    cat()

ids <- docs$index

ids_df <- ids |>
    as.data.frame() |>
    pivot_longer(everything(), names_to = "topic", values_to = "index")

ids_df$scopus_id <- abs_ps$article_scopus_id[ids_df$index]
ids_df$abstract_text <- abs_ps$abstract_text[ids_df$index]
ids_df$article_doi <- abs_ps$article_doi[ids_df$index]

# 2.2 Mean Doc-topic proportions

dty <- doc_topic_df |>
    group_by(article_year) |>
    summarise(
        t1 = mean(Topic1),
        t2 = mean(Topic2),
        t3 = mean(Topic3),
        t4 = mean(Topic4),
        t5 = mean(Topic5),
        t6 = mean(Topic6),
        t7 = mean(Topic7),
        t8 = mean(Topic8),
        t9 = mean(Topic9),
        t10 = mean(Topic10)
    ) |>
    pivot_longer(starts_with("t"), names_to = "topic", values_to = "mean")

dty <- dty |>
    left_join(topic_labs, by = "topic") |>
    mutate(lab_fac = factor(lab, levels = lab_order))


# 2.3 Mean Doc-topic proportions over time

mean_dtp <- data.frame(
    mean = dtp,
    topic = names(dtp)
) |>
    left_join(topic_labs, by = "topic") |>
    mutate(lab_fac = factor(lab, levels = lab_order))


dtp_df <- doc_topic_df |>
    select(docnum, article_year, starts_with("Topic")) |>
    pivot_longer(starts_with("Topic"), names_to = "topic", values_to = "dtp")

dtp_df <- dtp_df |>
    left_join(topic_labs, by = "topic") |>
    mutate(lab_fac = factor(lab, levels = lab_order))

dat_avgs_dpt <- dtp_df |>
    group_by(article_year, lab_fac) |>
    summarise(dtp = mean(dtp)) |>
    ungroup()

# filter topics mentioned in paper
dat_avgs_dpt |>
    filter(lab_fac %in% c("Renewable Energy", "Implementation")) |>
    filter(article_year %in% c(2010, 2023))


table(dtp_df$lab_fac)

# Figure 4
dtp_year_plot <- dtp_df |>
    ggplot(aes(x = article_year, y = dtp, group = lab_fac)) +
    stat_summary(fun.data = "mean_cl_normal", geom = "ribbon", fill = "grey80") +
    stat_summary(fun = mean, geom = "point") +
    stat_summary(fun = mean, geom = "line") +
    geom_hline(data = mean_dtp, aes(yintercept = mean), linetype = "dashed") +
    coord_cartesian(ylim = c(0, 0.25)) + # "zoom" into plot
    scale_x_discrete(breaks = c(2011, 2013, 2015, 2017, 2019, 2021, 2023)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    facet_wrap(~lab_fac,
        nrow = 2, scales = "free_x",
        labeller = label_wrap_gen(width = 20)
    ) +
    labs(x = "", y = "Mean Document-Topic Prevalence per Year\n(and 95% Confidence Intervals)") +
    theme(
        axis.text.x = element_text(angle = 90),
        strip.text.x = element_text(size = 13)
    )
dtp_year_plot
ggsave(dtp_year_plot, filename = "fig_04.png", width = 9.5, height = 7, dpi = 300)
ggsave(dtp_year_plot, filename = "fig_04.pdf", width = 9.5, height = 7)
ggsave(dtp_year_plot, filename = "fig_04.eps", width = 9.5, height = 7)


## Print sessionInfo() in Markdown html report
sessionInfo()
