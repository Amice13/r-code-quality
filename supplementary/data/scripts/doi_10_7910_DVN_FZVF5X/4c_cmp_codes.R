library(lubridate)
library(ggplot2)
library(arrow)
library(here)
library(ggbeeswarm)
library(tidyr)
library(dplyr)
library(stringr)
library(ggcorrplot)
library(reshape2)
library(data.table)
library(lme4)
library(jtools)
library(kableExtra)
library(reticulate)
library(stats)
library(pscl)


# basic data loading
source_python("scripts/utils_analysis.py")

cmp_comparison_results <- data.frame(
    code = integer(),
    language = character(),
    method = character(),
    r2_mcfadden = double()
)
cmp_comparison_results2 <- data.frame(
    code = integer(),
    language = character(),
    method = character(),
    r2_mcfadden = double()
)

meta_data <- as.data.table(read_feather(here(
    "data", "for_graphs",
    "regression", "scored_data.feather"
)))
setDT(meta_data)
poles <- list("virtue" = 603, "vice" = 604)
iso_languages <- c("es", "en", "de", "nl")

# Multilingual data
methods <- c("ccr_multi_to_multi", "ccr_multi_to_en", "ddr", "mfd")
for (method in methods) {
    print(method)
    if (method == "ddr") {
        multilingual <- load_ddr(which = "merged", aggregate = FALSE, )
        monolingual <- load_ddr(which = "mono", aggregate = FALSE, )
        setDT(multilingual)
        setDT(monolingual)
    } else if (grepl("ccr", method)) {
        string_of_file_name <- substring(method, 5) # [4:]
        multilingual <- load_ccr(which = string_of_file_name, aggregate = FALSE, )
        setDT(multilingual)
    } else if (method == "mfd") {
        multilingual <- load_mfd(which = "merged", aggregate = FALSE)
        setDT(multilingual)
    }

    data_merged <- merge(meta_data, multilingual, by = "id_for_project", all.x = TRUE)

    # for DDR, we also need the english data, which is in the monolingual dataframe
    if (method == "ddr") {
        data_merged <- merge(data_merged, monolingual, by = "id_for_project", all.x = TRUE)
    }
    # setnames(dt1, c("iso_language.x"), c("iso_language"))
    data_merged$cmp_603 <- ifelse(data_merged$cmp_code == "603", TRUE, FALSE)
    data_merged$cmp_604 <- ifelse(data_merged$cmp_code == "604", TRUE, FALSE)

    for (language in iso_languages) {
        if ((language == "en") & (method == "ddr")) {
            string_of_column <- "ddr_all_en_"
        } else if ((language == "en") & (method == "mfd")) {
            string_of_column <- "english_anchor_"
        } else {
            string_of_column <- paste0(method, "_")
        }
        for (pole in names(poles)) {
            # data_subsetted <- subset(data_merged, language_iso.x == language)
            formular_pasted <- as.formula(paste0("cmp_", poles[[pole]], " ~ ", string_of_column, "sanctity.", pole, " + ", string_of_column, "authority.", pole))
            result <- glm(formular_pasted,
                family = "binomial",
                data = data_merged[eval(data_merged$language_iso == language), ]
            )
            cmp_comparison_results <- cmp_comparison_results %>% add_row(
                code = poles[[pole]],
                language = language,
                method = method,
                r2_mcfadden = pR2(result)["McFadden"]
            )
        }
    }
}

# English translations
methods <- c("ccr_en_to_en", "ddr", "mfd", "emfd", "moralbert")
for (method in methods) {
    print(method)
    if (method == "ddr") {
        monolingual <- load_ddr(which = "mono", aggregate = FALSE)
        setDT(monolingual)
    } else if (method == "ccr_en_to_en") {
        monolingual <- load_ccr(which = "en_to_en", aggregate = FALSE)
        setDT(monolingual)
    } else if (method == "ccr_multi_to_en") {
        string_of_file_name <- substring(method, 5) # [4:]
        monolingual <- load_ccr(which = string_of_file_name, aggregate = FALSE)
        setDT(monolingual)
    } else if (method == "mfd") {
        monolingual <- load_mfd(which = "mono", aggregate = FALSE)[["mfd"]]
        setDT(monolingual)
    } else if (method == "emfd") {
        monolingual <- load_mfd(which = "mono", aggregate = FALSE)[["emfd"]]
        setDT(monolingual)
    } else if (method == "moralbert") {
        monolingual <- load_moral_bert(aggregate = FALSE)
        setDT(monolingual)
    }

    data_merged <- merge(meta_data, monolingual, by = "id_for_project", all.x = TRUE)

    # setnames(dt1, c("iso_language.x"), c("iso_language"))
    data_merged$cmp_603 <- ifelse(data_merged$cmp_code == "603", TRUE, FALSE)
    data_merged$cmp_604 <- ifelse(data_merged$cmp_code == "604", TRUE, FALSE)

    # print(language)
    if (method == "ddr") {
        string_of_column <- "ddr_all_en_"
    } else if (method == "mfd") {
        string_of_column <- "mfd_"
    } else if (method == "emfd") {
        string_of_column <- "emfd_"
    } else if (method == "ccr_en_to_en") {
        string_of_column <- "ccr_en_to_en_"
    } else if (method == "ccr_multi_to_en") {
        string_of_column <- "ccr_multi_to_en_"
    } else if (method == "moralbert") {
        string_of_column <- "moralbert_"
    }
    for (pole in names(poles)) {
        # data_subsetted <- subset(data_merged, language_iso.x == language)
        formular_pasted <- as.formula(paste0("cmp_", poles[[pole]], " ~ ", string_of_column, "sanctity.", pole, " + ", string_of_column, "authority.", pole))
        result <- glm(formular_pasted,
            family = "binomial",
            data = data_merged
        )
        cmp_comparison_results2 <- cmp_comparison_results2 %>% add_row(
            code = poles[[pole]],
            language = "English translation",
            method = method,
            r2_mcfadden = pR2(result)["McFadden"]
        )
    }
}

# export LaTEX table

cmp_comparison_results |>
    bind_rows(cmp_comparison_results2) |>
    mutate(
        code = case_match(
            code,
            603 ~ "positive",
            604 ~ "negative"
        ),
        language = case_match(
            language,
            "es" ~ "Spanish",
            "nl" ~ "Dutch",
            "de" ~ "German",
            "en" ~ "English",
            "English translation" ~ "English translation"
        ),
        method = case_match(
            method,
            "ccr_multi_to_multi" ~ "CCR (multi. ref, multi. embedding)",
            c("ccr_multi_to_en") ~ "CCR (english ref, multi. embedding)",
            "ccr_en_to_en" ~ "CCR (english ref, engl. embedding)",
            "moralbert" ~ "MoralBERT",
            "ddr" ~ "DDR",
            "mfd" ~ "MFD2",
            "emfd" ~ "eMFD",
        ),
        r2_mcfadden = round(r2_mcfadden, digits = 4)
    ) |>
    rename(
        "Traditional Morality (MARPOR)" = code,
        "Original language of manifesto" = language,
        "Method" = method,
        "Pseudo R2" = r2_mcfadden
    ) |>
    kable(
        format = "latex", booktabs = TRUE, longtable = T,
        caption = "Pseudo R2 for a logistic regression, predicting the MARPOR Codes 603/604 on quasi sentences with the respective Sanctity and Authority dimensions",
    ) |>
    collapse_rows(columns = 1:3) |>
    save_kable("graphs/tables/cmp_log_regression.tex")
