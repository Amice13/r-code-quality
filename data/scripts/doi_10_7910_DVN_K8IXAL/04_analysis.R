#' ---
#' title: "Catalysts for progress? Mapping policy insights from energy research (ERSS)"
#' subtitle: "04_analysis.R"
#' author: "Authors: Brian Boyle, Yen-Chieh Liao, Sarah King, Robin Rauner, and Stefan Müller"
#' ---

# load required R packages
library(tidyverse) # CRAN v2.0.0
library(rio) # CRAN v1.2.2
library(here) # CRAN v1.0.1
library(newsmap) # CRAN v0.9.0
library(quanteda) # CRAN v4.1.0
library(quanteda.textstats) # CRAN v0.97.2
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

# load custom ggplot2 scheme
source("function_theme_base.R")

# Import and prepare data for analysis
# Binary classification for policy relevance
dat_classified <- readRDS("data_scopus_1_100_2010_2023_classified.rds") |>
    mutate(journal_name_lower = tolower(journal_name))

# check out variable names
names(dat_classified)

# Text of classified abstracts for keyness analysis 
# (cannot be shared as it contains abstract texts)
dat_dontshare <- readRDS(
    "data_dontshare/data_scopus_1_100_2010_2023_classified_dontshare.rds"
)

# Journal characteristics + policy in aims and scope variable
journal_aims <- import("journal_aims.csv")

# Merge data
dat_merged <- dat_classified |>
    left_join(
        journal_aims |>
            select(starts_with("journal")),
        by = "journal_name_lower"
    )

# Create author continent variables
dict_newsmap <- newsmap::data_dictionary_newsmap_en


# Create new objects for the continent dictionaries
# Inspect and remove abbreviations and short names that could return mismatches

africa <- dict_newsmap[1] |>
    unlist() |>
    as.character()
africa <- africa[!africa %in% c('sa')]
africa <- c(africa, "cote d'ivoire")

americas <- dict_newsmap[2] |>
    unlist() |>
    as.character() 
americas <- americas[!americas %in% c('us', 'lima', 'rio')]

asia <- dict_newsmap[3] |>
    unlist() |>
    as.character()
asia <- asia[!asia %in% c('omani*', 'oman')]

europe <- dict_newsmap[4] |>
    unlist() |>
    as.character() 
europe <- europe[!europe %in% c('uk', 'rome', 'riga', 'pole*')]
europe <- c(europe, "faroe islands")

oceania <- dict_newsmap[5] |>
    unlist() |>
    as.character()
oceania <- oceania[!oceania %in% c('nz', 'oz', 'apia')]




# Apply dictionaries to identify matches for each continent

dat_merged <- dat_merged |>
    mutate(
        africa = as.numeric(grepl(paste0(africa, collapse = "|"),
                                  dat_merged$affil_country,
                                  ignore.case = TRUE
        )),
        americas = as.numeric(grepl(paste0(americas, collapse = "|"),
                                    dat_merged$affil_country,
                                    ignore.case = TRUE
        )),
        asia = as.numeric(grepl(paste0(asia, collapse = "|"),
                                dat_merged$affil_country,
                                ignore.case = TRUE
        )),
        europe = as.numeric(grepl(paste0(europe, collapse = "|"),
                                  dat_merged$affil_country,
                                  ignore.case = TRUE
        )),
        oceania = as.numeric(grepl(paste0(oceania, collapse = "|"),
                                   dat_merged$affil_country,
                                   ignore.case = TRUE
        ))
    )

# Manually run additional search for africa dictionary
# Require exact case sensitive match for 'Oman' to avoid false positives
dat_merged$africa[str_detect(dat_merged$affil_country, 'Oman')] <- 1


# Number of continents all article authors affiliated with
dat_merged <- dat_merged |>
    mutate(
        n_continents = rowSums(
            dat_merged[, c("africa", "americas", "asia", "europe", "oceania")]
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

# Check for NAs in affil_continent

dat_merged |> 
    filter(is.na(affil_continent)) |> 
    nrow()

# Intercontinental only - spotcheck
dat_merged |>
    filter(affil_continent == 'Intercontinental') |>
    select(affil_country, africa:affil_continent) |> 
    head()

# Calculate percentage of Asia in Intercontinental
n_intercontinental_asia <- dat_merged |>
    filter(affil_continent == "Intercontinental" & asia == 1) |>
    nrow()

n_interncontinental <- dat_merged |>
    filter(affil_continent == "Intercontinental") |>
    nrow()

n_intercontinental_asia / n_interncontinental * 100 # 77%

# Keyness analysis
# quanteda::quanteda_options(threads = 22)

# tokenize corpus (cannot be shared publicly)
toks <- dat_dontshare |>
    corpus() |>
    tokens()

# identify multiword expressions (size: 2, 3, 4)
# set seed and only use small sample
set.seed(23)
mwes <- toks |>
    tokens_sample(size = 10000) |> # sample 1000 abstracts to speed up collocation analysis
    tokens(remove_punct = TRUE) |>
    tokens_keep(min_nchar = 2) |>
    tokens_remove(pattern = stopwords("en"), padding = TRUE) |>
    textstat_collocations(size = 2:4, min_count = 10)

# create document feature matrix after compounding multiword expressions
# then group dfm in to policy_recommendation = 1/0
dfmat <- toks |>
    tokens_keep(min_nchar = 2) |>
    tokens_compound(pattern = mwes) |>
    dfm() |>
    dfm_remove(pattern = c("to", "how", "we")) |>
    dfm_group(groups = policy_statement)

# conduct keyness analysis and give positive values to
# words in the target category = 1
tstat_key <- textstat_keyness(dfmat, target = "1") |>
    arrange(-chi2) # arrange by chi2

# get the top most frequent words and phrases
top_total <- 100

tstat_key_top <- tstat_key[1:top_total, ]

topwords <- tstat_key_top |>
    mutate(feature = str_replace_all(feature, "_", " ")) |>
    summarise(Words = paste(feature, collapse = ", "))


# inspect object
topwords

# Main text tables and figures
# Table 1: Top 100 most predictive terms policy-relevant abstracts
print(
    xtable(topwords,
           label = "tab:keyness",
           align = c(
               "p{0.05\\textwidth}",
               "p{0.9\\textwidth}"
           ),
           caption = "The top 100 most predictive terms in abstracts classified as policy relevant"
    ),
    type = "latex",
    table.placement = "!h",
    caption.placement = "top",
    booktabs = TRUE,
    file = "tab_01.tex",
    size = "footnotesize",
    include.rownames = FALSE
)

print(
    xtable(topwords,
           label = "tab:keyness",
           align = c(
               "p{0.05\\textwidth}",
               "p{0.9\\textwidth}"
           ),
           caption = "The top 100 most predictive terms in abstracts classified as policy relevant"
    ),
    type = "html",
    table.placement = "!h",
    caption.placement = "top",
    file = "tab_01.html",
    size = "footnotesize",
    include.rownames = FALSE
)


# Figure 1: Number of Publications by Year and Author Continent
tableau_ten <- c(
  "#4E79A7", "#F28E2B", "#E15759",
  "#76B7B2", "#59A14F", "#EDC948"
)

fig_pubs_year_cont <- dat_merged |>
    filter(!is.na(affil_continent)) |>
    count(affil_continent, article_year) |>
    mutate(cont_fac = factor(affil_continent,
                             levels = c(
                                 "Asia",
                                 "Intercontinental",
                                 "Europe",
                                 "Americas",
                                 "Africa",
                                 "Oceania"
                             ), ordered = TRUE
    )) |>
    ggplot(aes(x = as.numeric(article_year), y = n)) +
    labs(x = "", y = "Number of Publications", fill = NULL) +
    scale_x_continuous(
        limits = c(2010, 2023),
        breaks = seq(2010, 2023, by = 2),
        labels = seq(2010, 2023, by = 2)
    ) +
    scale_y_continuous(
        limits = c(0, 40000), breaks = seq(0, 40000, by = 10000),
        labels = scales::comma_format()
    ) +
    geom_area(aes(fill = cont_fac), alpha = 1) +
    geom_area(aes(colour = cont_fac), alpha = 0) +
    scale_fill_manual(values = tableau_ten) +
    guides(fill = guide_legend(nrow = 1)) +
    scale_colour_manual(values = tableau_ten, guide = "none") +
    theme(legend.position = "bottom")
fig_pubs_year_cont
ggsave("fig_01.png", fig_pubs_year_cont, width = 9, height = 5, dpi = 300)
ggsave("fig_01.pdf", fig_pubs_year_cont, width = 9, height = 5)
ggsave("fig_01.eps", fig_pubs_year_cont, width = 9, height = 5)


# Figure 2: Prevalence of PR Across Journals by Aims and Scope
# Plot PR across journals by aims and scope
dat_journal_stats <- dat_merged |>
    group_by(
        journal_name, journal_rank,
        journal_policy_aims, journal_categories, journal_areas
    ) |>
    summarise(
        mean_policy = mean(policy_statement, na.rm = TRUE),
        n = n(),
        sd_policy = sd(policy_statement, na.rm = TRUE),
        se = sd_policy / sqrt(n),
        ci_lower = mean_policy - 1.96 * se,
        ci_upper = mean_policy + 1.96 * se
    ) |>
    mutate(journal_policy_aims = factor(journal_policy_aims)) |>
    mutate(journal_name = str_replace_all(journal_name, "Gcb", "GCB")) |>
    mutate(journal_name = str_replace_all(journal_name, "Jphys", "JPhys")) |>
    mutate(journal_name = str_replace_all(journal_name, " Of ", " of ")) |>
    mutate(journal_name = str_replace_all(journal_name, " And", " and")) |>
    mutate(journal_name = str_replace_all(journal_name, " And", " and")) |>
    mutate(journal_name = str_replace_all(journal_name, " On ", " on ")) |>
    mutate(journal_name = str_replace_all(journal_name, "Acs", "ACS")) |>
    mutate(journal_name = str_replace_all(journal_name, "Ieee", "IEEE")) |> 
    mutate(journal_name = str_replace_all(journal_name, " For ", " for ")) |> 
    mutate(journal_name = str_replace_all(journal_name, "Csee", "CSEE")) |> 
    mutate(journal_name = str_replace_all(journal_name, "Rrl", "RRL")) |> 
    mutate(journal_name = str_replace_all(journal_name, "Spe ", "SPE ")) |> 
    mutate(journal_name = str_replace_all(journal_name, "Energy Research and Social Science", "Energy Research & Social Science")) |> 
    mutate(journal_name = str_replace_all(journal_name, "Energy For Sustainable Development", "Energy for Sustainable Development")) |> 
    mutate(journal_name = str_replace_all(journal_name, "Chemsuschem", "ChemSusChem")) |> 
    mutate(journal_name = str_replace_all(journal_name, "Current Opinion In Chemical Engineering", "Current Opinion in Chemical Engineering"))  
    
# Subset journals with policy in aims and scope
journal_stats_subset <- dat_journal_stats |>
    filter(journal_policy_aims == 1) |>
    arrange(desc(mean_policy))

journal_aims_below <- journal_stats_subset |>
    filter(mean_policy < mean(dat_merged$policy_statement))

journal_aims_above <- journal_stats_subset |>
    filter(mean_policy > mean(dat_merged$policy_statement))

pct_below_mean <- sum(
    journal_stats_subset$mean_policy < mean(dat_merged$policy_statement)
) / nrow(journal_stats_subset) * 100

# select top 50 journals

dat_journal_stats_t50 <- dat_journal_stats |> 
    arrange(-mean_policy) |> # arrange by prevalence 
    head(50) # get first 50 observations

ggplot(
    dat_journal_stats_t50,
    aes(
        x = mean_policy, xmin = ci_lower, xmax = ci_upper,
        y = reorder(journal_name, mean_policy),
        fill = journal_policy_aims
    )
) +
    geom_linerange() +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    geom_point(size = 3, shape = 21, color = "black") +
    # geom_vline(aes(xintercept = mean(dat_merged$policy_statement)),
    #   linetype = "dashed"
    # ) +
    labs(
        x = "Mean Policy Relevance\n(and 95% Confidence Intervals)",
        fill = "'Policy' in Aims and Scope", y = NULL
    ) +
    scale_fill_manual(values = c("white", "black"), labels = c("No", "Yes")) +
    theme(panel.grid.major.x = element_line(), 
          panel.grid.major.y = element_line(),
          axis.text.x = element_text(hjust = 0.85),
          axis.ticks.y = element_blank())
ggsave("fig_02.png", height = 12, width = 9, dpi = 300)
ggsave("fig_02.pdf", height = 12, width = 9)
ggsave("fig_02.eps", height = 12, width = 9)


# Figure 3: Prevalence of PR over Time Across Author Continents
# Calculate summaries with 95% confidence intervals
author_cont_year <- dat_merged |>
    filter(!is.na(affil_continent)) |>
    group_by(affil_continent, article_year) |>
    summarise(
        mean_policy = mean(policy_statement, na.rm = TRUE),
        n = n(),
        sd_policy = sd(policy_statement, na.rm = TRUE)
    ) |>
    mutate(
        se = sd_policy / sqrt(n),
        ci_lower = mean_policy - qt(0.975, df = n - 1) * se,
        ci_upper = mean_policy + qt(0.975, df = n - 1) * se
    )

author_cont_year$affil_continent <- factor(
    author_cont_year$affil_continent,
    levels = c(
        "Asia", "Africa", "Americas", "Europe", "Oceania", "Intercontinental"
    )
)

dat_avgs <- dat_merged |>
    filter(!is.na(affil_continent)) |>
    group_by(affil_continent) |>
    summarise(
        mean_policy = mean(policy_statement, na.rm = TRUE)
    ) |> 
    mutate(affil_continent = factor(affil_continent, 
                                    levels = c(
                                        "Asia", "Africa", "Americas", "Europe", "Oceania", "Intercontinental")))

# overall averages (full sample)
dat_avgs


fig_policy_year_cont <- ggplot(
    author_cont_year, #filter(author_cont_year, affil_continent != "Intercontinental"),
    aes(x = as.numeric(article_year), y = mean_policy)
) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "grey80") +
    geom_point() +
    geom_line() +
    facet_wrap(~affil_continent, nrow = 1) +
    labs(y = "Mean Policy Relevance per Year\n(and 95% Confidence Intervals)"
    ) +
    scale_y_continuous(
        limits = c(0, 0.37),
        labels = scales::percent_format(accuracy = 1)
    ) +
    geom_hline(data = dat_avgs, # filter(dat_avgs, affil_continent != "Intercontinental"),
               aes(yintercept = mean_policy), linetype = "dashed") +
    scale_x_continuous(
        limits = c(2010, 2023),
        breaks = seq(2011, 2023, by = 2),
        labels = seq(2011, 2023, by = 2)
    ) +
    theme(
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none",
        strip.text.x = element_text(size = 13)
    )
fig_policy_year_cont
ggsave("fig_03.png", fig_policy_year_cont, width = 9.5, height = 4, dpi = 300)
ggsave("fig_03.pdf", fig_policy_year_cont, width = 9.5, height = 4)
ggsave("fig_03.eps", fig_policy_year_cont, width = 9.5, height = 4)


# Appendix tables and figures ---------------------------

# Table A1: Distribution of Policy Relevance Across Journals
journal_policy_stats <- dat_merged |>
    mutate(journal_policy_aims = dplyr::recode(journal_policy_aims, "0" = "No", "1" = "Yes")) |> 
    group_by(journal_name, journal_policy_aims) |>
    summarise(
        `Total Abstracts` = n(),
        `Policy Mention` = sum(policy_statement),
        `% Policy` = mean(policy_statement)
    ) |>
    mutate(`% Policy` = scales::percent(`% Policy`, accuracy = 0.1)) |> 
    mutate(journal_name = str_replace_all(journal_name, "Gcb", "GCB")) |>
    mutate(journal_name = str_replace_all(journal_name, "Jphys", "JPhys")) |>
    mutate(journal_name = str_replace_all(journal_name, " Of ", " of ")) |>
    mutate(journal_name = str_replace_all(journal_name, " And", " and")) |>
    mutate(journal_name = str_replace_all(journal_name, " And", " and")) |>
    mutate(journal_name = str_replace_all(journal_name, " On ", " on ")) |>
    mutate(journal_name = str_replace_all(journal_name, "Acs", "ACS")) |>
    mutate(journal_name = str_replace_all(journal_name, "Ieee", "IEEE")) |> 
    mutate(journal_name = str_replace_all(journal_name, " For ", " for ")) |> 
    mutate(journal_name = str_replace_all(journal_name, "Csee", "CSEE")) |> 
    mutate(journal_name = str_replace_all(journal_name, "Rrl", "RRL")) |> 
    mutate(journal_name = str_replace_all(journal_name, "Spe ", "SPE ")) |> 
    mutate(journal_name = str_replace_all(journal_name, "Topics In Power", "Topics in Power")) |> 
        mutate(journal_name = str_replace_all(journal_name, "Energy Research and Social Science", "Energy Research & Social Science")) |> 
    mutate(journal_name = str_replace_all(journal_name, "Energy For Sustainable Development", "Energy for Sustainable Development")) |> 
    mutate(journal_name = str_replace_all(journal_name, "Chemsuschem", "ChemSusChem")) |> 
    mutate(journal_name = str_replace_all(journal_name, "Current Opinion In Chemical Engineering", "Current Opinion in Chemical Engineering")) |> 
    rename(`Journal Name` = journal_name,
           `Policy in Aims` = journal_policy_aims) 

print(head(journal_policy_stats, 10))

# Generate the LaTeX longtable output
print(
    xtable(journal_policy_stats,
           digits = 0,
           label = "tab:policy_stats",
           align = c(
               "p{0.05\\textwidth}",
               "p{0.55\\textwidth}",
               "p{0.06\\textwidth}",
               "p{0.065\\textwidth}",
               "p{0.07\\textwidth}",
               "p{0.07\\textwidth}"
           ),
           caption = "Prevalence of policy relevance across journals"
    ),
    type = "latex",
    size = "scriptsize",
    format.args=list(big.mark=","),
    caption.placement = "top",
    include.rownames = FALSE,
    tabular.environment = "longtable",
    file = "tab_A1.tex",
    booktabs = TRUE
)

print(
    xtable(journal_policy_stats,
           digits = 0,
           label = "tab:policy_stats",
           caption = "Prevalence of policy relevance across journals"
    ),
    type = "html",
    size = "scriptsize",
    caption.placement = "top",
    include.rownames = FALSE,
    tabular.environment = "longtable",
    file = "tab_A1.html",
    booktabs = TRUE
)


# Figure A1: Prevalence of Policy Relevance Over Time
# Plot mean policy relevance by year with CIs
# Calculate the overall average of policy_statement
average_policy_statement <- mean(dat_classified$policy_statement)

ggplot(dat_classified, aes(x = factor(article_year), y = policy_statement)) +
    stat_summary(
        fun.data = mean_cl_normal,
        linewidth = 0.8, size = 0.5
    ) +
    geom_hline(
        yintercept = average_policy_statement,
        linetype = "dashed"
    ) +
    coord_cartesian(ylim = c(0, 0.2)) +
    scale_y_continuous(
        labels = scales::percent_format(accuracy = 1),
        breaks = c(seq(0, 0.5, 0.05))
    ) +
    labs(
        y = "Mean Policy Relevance\n(and 95% Confidence Intervals)"
    ) +
    theme(
        axis.title.x = element_blank()
    )
ggsave("fig_A1.png", width = 9, height = 5, dpi = 300)
ggsave("fig_A1.pdf", width = 9, height = 5)
ggsave("fig_A1.eps", width = 9, height = 5)


# Figure A2: Prevalence of PR Across Author Continent
ggplot(
    dat_merged |> filter(!is.na(affil_continent)),
    aes(
        x = policy_statement,
        y = reorder(affil_continent, policy_statement, FUN = mean)
    )
) +
    stat_summary(
        fun.data = mean_cl_normal,
        linewidth = 0.8, size = 0.5
    ) +
    coord_cartesian(xlim = c(0, 0.4)) +
    scale_x_continuous(
        labels = scales::percent_format(accuracy = 1),
        breaks = c(seq(0, 0.4, 0.05))
    ) +
    labs(
        x = "Mean Policy Relevance (and 95% Confidence Intervals)"
    ) +
    theme(
        axis.title.y = element_blank()
    )
ggsave("fig_A2.png", width = 9, height = 4, dpi = 300)
ggsave("fig_A2.pdf", width = 9, height = 4)
ggsave("fig_A2.eps", width = 9, height = 4)


# Figure A3: Prevalence of PR Across Top 10 Funding Agencies
# Plot PR by funding sponsor
funder <- as.data.frame(table(dat_merged$fund_sponsor)) |>
    arrange(desc(Freq))

funder_stats <- dat_merged |>
    group_by(fund_sponsor) |>
    summarise(
        count = n(),
        policy_mention = as.integer(sum(policy_statement)),
        mean_policy = round(mean(policy_statement), 2)
    ) |>
    arrange(desc(count))
print(head(funder_stats, 10))

# Plot PR by funder with CIs
# Plot PR by funder with CIs
ggplot(
    dat_merged |>
        filter(!is.na(fund_sponsor)) |>
        count(fund_sponsor, sort = TRUE) |>
        top_n(10, n) |>
        inner_join(dat_merged, by = "fund_sponsor"),
    aes(
        x = policy_statement,
        y = reorder(fund_sponsor, policy_statement, FUN = mean),
        color = fund_sponsor
    )
) +
    stat_summary(
        fun.data = mean_cl_normal,
        linewidth = 1, size = 0.8
    ) +
    labs(
        x = "Mean Policy Relevance\n(and 95% Confidence Intervals)"
    ) +
    theme(
        axis.title.y = element_blank()
    ) +
    scale_x_continuous(
        labels = scales::percent_format(accuracy = 1),
        breaks = c(seq(0, 0.3, 0.05))
    ) +
    coord_cartesian(xlim = c(0, 0.3)) +
    scale_y_discrete(labels = scales::label_wrap(30)) +
    scale_color_manual(
        name = "Continent",
        breaks = c(
            "National Natural Science Foundation of China",
            "National Science Foundation",
            "European Commission"
        ),
        values = c(
            "European Commission" = "#E15759",
            "Engineering and Physical Sciences Research Council" = "#E15759",
            "Horizon 2020 Framework Programme" = "#E15759",
            "National Science Foundation" = "#76B7B2",
            "Japan Society for the Promotion of Science" = "#4E79A7",
            "U.S. Department of Energy" = "#76B7B2",
            "Natural Sciences and Engineering Research Council of Canada" = "#76B7B2",
            "National Key Research and Development Program of China" = "#4E79A7",
         "National Natural Science Foundation of China" = "#4E79A7",
            "Ministry of Science, ICT and Future Planning" = "#4E79A7"
        ),
        labels = c(
            "European Commission" = "Europe",
            "National Science Foundation" = "Americas",
            "National Natural Science Foundation of China" = "Asia"
        )
    )
ggsave("fig_A3.png", width = 9, height = 8, dpi = 300)
ggsave("fig_A3.pdf", width = 9, height = 8)
ggsave("fig_A3.eps", width = 9, height = 8)


# Figure A4: Prevalence of Polic-Relevant Abstracts by Journal Aims and Scope
# Compare means of policy in aims and scope == 1 and == 0
dat_merged_p <- dat_merged |>
    mutate(journal_policy_aims = ifelse(
        journal_policy_aims == "0", "Policy Not Mentioned in Aims and Scope",
        "Policy Mentioned in Aims and Scope"
    ))

# Calculate the mean of policy_statement by aims and scope
aims_means <- dat_merged_p |>
    group_by(journal_policy_aims) |>
    summarise(mean_policy = mean(policy_statement))
print(aims_means)

# Plot mean policy relevance by year with CIs with facet for aims & scope
ggplot(
    dat_merged_p,
    aes(x = factor(article_year), y = policy_statement)
) +
    stat_summary(
        fun.data = mean_cl_normal,
        linewidth = 0.8, size = 0.3
    ) +
    coord_cartesian(ylim = c(0, 0.4)) +
    scale_y_continuous(
        labels = scales::percent_format(accuracy = 1),
        breaks = c(seq(0, 0.5, 0.05))
    ) +
    geom_hline(
        data = aims_means, colour = "darkblue",
        aes(yintercept = mean_policy),
        linetype = "dashed"
    ) +
    geom_text(
        colour = "darkblue",
        data = aims_means, hjust = 0,
        aes(
            x = 0.5, y = mean_policy + 0.015,
            label = paste0(round(100 * mean_policy, 1), "%")
        )
    ) +
    facet_wrap(~journal_policy_aims) +
    labs(
        x = "Year",
        y = "Mean Policy Relevance\n(and 95% Confidence Intervals)"
    ) +
    theme(
        axis.text.x = element_text(angle = 90)
    )
ggsave("fig_A4.png", width = 9.5, height = 5, dpi = 300)
ggsave("fig_A4.pdf", width = 9.5, height = 5)
ggsave("fig_A4.eps", width = 9.5, height = 5)


# Table A5: Prevalence of PR by Research Area and Author Location
dat_asia_disc <- dat_merged |>
    filter(!is.na(affil_continent)) |>
    mutate(asia_dummy = ifelse(affil_continent == "Asia", "Asia", "Other")) |>
    group_by(asjc_area, asia_dummy) |>
    summarise(
        mean_policy = mean(policy_statement, na.rm = TRUE),
        n = n(),
        sd_policy = sd(policy_statement, na.rm = TRUE)
    ) |>
    mutate(
        se = sd_policy / sqrt(n),
        ci_lower = mean_policy - qt(0.975, df = n - 1) * se,
        ci_upper = mean_policy + qt(0.975, df = n - 1) * se
    )

dat_asia_disc_wide <- dat_asia_disc |>
    mutate(mean_policy = paste0(round(100 * mean_policy, 1), "%")) |>
    select(
        Discipline = asjc_area, Location = asia_dummy,
        `Policy Relevance` = mean_policy
    ) |>
    spread(Location, `Policy Relevance`)

# Generate the LaTeX output
print(
    xtable(dat_asia_disc_wide,
           digits = 0,
           label = "tab:asia_other",
           align = c(
               "p{0.05\\textwidth}",
               "p{0.4\\textwidth}",
               "p{0.15\\textwidth}",
               "p{0.15\\textwidth}"
           ),
           caption = "Prevalence of policy relevance in abstracts by research area and location of authors"
    ),
    type = "latex",
    size = "footnotesize",
    caption.placement = "top",
    include.rownames = FALSE,
    file = "tab_A5.tex",
    booktabs = TRUE
)

print(
    xtable(dat_asia_disc_wide,
           digits = 0,
           label = "tab:asia_other",
           align = c(
               "p{0.05\\textwidth}",
               "p{0.4\\textwidth}",
               "p{0.15\\textwidth}",
               "p{0.15\\textwidth}"
           ),
           caption = "Prevalence of policy relevance in abstracts by research area and location of authors"
    ),
    type = "html",
    size = "footnotesize",
    caption.placement = "top",
    include.rownames = FALSE,
    file = "tab_A5.html",
    booktabs = TRUE
)

# Figure A5: Policy Relevance and Journal Rank
ggplot(
    dat_journal_stats,
    aes(x = journal_rank, y = mean_policy)
) +
    geom_point(size = 2) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_x_continuous(breaks = c(seq(1, 100, 5))) +
    geom_smooth(method = "lm") +
    labs(
        x = "Journal Rank",
        y = "Mean Policy Relevance"
    )
ggsave("fig_A5.png", width = 9, height = 5, dpi = 300)
ggsave("fig_A5.pdf", width = 9, height = 5)
ggsave("fig_A5.eps", width = 9, height = 5)


## Print sessionInfo() in Markdown report
sessionInfo()
