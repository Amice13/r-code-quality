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
library(countrycode)
library(readr)
library(texreg)
library(reticulate)
## First Step: gathering and uniting all meta-data

partyfacts <- as.data.table(read.csv(here("data", "external", "partyfacts-external-parties.csv")))

parlgov_data <- as.data.table(read.csv(here("data", "external", "view_cabinet.csv")))
parlgov_data <- parlgov_data[, .(election_date, cabinet_party, party_id, start_date)]
parlgov_data[, election_date := as.Date(election_date, format = "%Y-%m-%d")]
parlgov_data[, start_date := as.Date(start_date, format = "%Y-%m-%d")]
parlgov_data[, cabinet_party := as.logical(cabinet_party)]

setkey(parlgov_data, party_id)


partyfacts_parlgov <- partyfacts[dataset_key == "parlgov", .(partyfacts_id, dataset_party_id)]
partyfacts_parlgov[, dataset_party_id := as.integer(dataset_party_id)]
setkey(partyfacts_parlgov, dataset_party_id)

parlgov_data <- parlgov_data[partyfacts_parlgov, nomatch = 0]
parlgov_data$party_id <- NULL
setkey(parlgov_data, partyfacts_id)

regression_data <- as.data.table(read_feather(here("data", "for_graphs", "regression", "scored_data.feather")))
regression_data <- unique(regression_data, by = "manifesto_id")
regression_data[, party := as.integer(party)]
setkey(regression_data, party)

# regression_data[, date := as.integer(date)]
regression_data[, manifesto_year := as.Date(paste(date, "01", sep = ""), "%Y%m%d")]

partyfacts_manifesto <- partyfacts[dataset_key == "manifesto", .(partyfacts_id, dataset_party_id)]
partyfacts_manifesto[, dataset_party_id := as.integer(dataset_party_id)]
setkey(partyfacts_manifesto, dataset_party_id)

regression_data <- partyfacts_manifesto[regression_data, nomatch = 0]
regression_data$dataset_party_id <- NULL
setkey(regression_data, partyfacts_id)

partyfacts_ches <- partyfacts[dataset_key == "ches", .(partyfacts_id, dataset_party_id)]
partyfacts_ches[, dataset_party_id := as.integer(dataset_party_id)]

setkey(partyfacts_ches, partyfacts_id)

regression_nearly <- regression_data[partyfacts_ches, nomatch = 0]

ches_data <- as.data.table(read.csv(here("data", "external", "ches.csv")))
ches_data[, ches_id := as.integer(party_id)]
ches_data <- ches_data[, .(year, ches_id, galtan)]

ches_data[, ches_year := as.Date(paste(year, "0101"), format = "%Y%m%d")]
ches_data[, roll_year := ches_year]

ches_data <- ches_data[, .(ches_id, galtan, ches_year, roll_year)]
setkey(ches_data, ches_id, roll_year)

regression_nearly[, roll_year := manifesto_year]
setnames(regression_nearly, c("dataset_party_id"), c("ches_id"))
setkey(regression_nearly, ches_id, roll_year)
nrow(regression_nearly)


regression_plus_galtan <- ches_data[regression_nearly, roll = -Inf]

# filter out manifestos past 2020 because there is no CHES after
regression_plus_galtan <- regression_plus_galtan[!is.na(ches_year)]


setkey(regression_plus_galtan, partyfacts_id, roll_year)
parlgov_data[, start_date_2 := start_date]
setkey(parlgov_data, partyfacts_id, start_date)

regression_with_govt <- parlgov_data[regression_plus_galtan, roll = Inf]
regression_with_govt <- regression_with_govt[!is.na(cabinet_party)]


regression_with_govt[, diff_year := ches_year - manifesto_year]

filtered_data <- regression_with_govt[diff_year < 2 * 366, ]
filtered_data[, year := year(manifesto_year)]
nrow(filtered_data)
# View(test2[diff_year < 2*365, .(partyname, ches_year, manifesto_year)])

write_feather(filtered_data, here("data", "for_graphs", "regression", "current_working_data.feather"))



## More prep

source_python("scripts/utils_analysis.py")

regression_prepared <- as.data.table(read_feather(here("data", "for_graphs", "regression", "current_working_data.feather")))
poles <- c("virtue", "vice")
dimensions <- c("care", "fairness", "loyalty", "authority", "sanctity")
overall_results <- data.frame(
    foundation = character(),
    pole = character(),
    method = character(),
    language = character(),
    coef = double(),
    ci_low = double(),
    ci_high = double()
)
overall_results2 <- data.frame(
    foundation = character(),
    pole = character(),
    method = character(),
    language = character(),
    coef = double(),
    ci_low = double(),
    ci_high = double()
)
iso_languages <- c("es", "en", "de", "nl")
language_mapping <- list("en" = "English", "nl" = "Dutch", "es" = "Spanish", "de" = "German")
tool_mapping <- list("mfd" = "MFD", "mfd2" = "MFD2", "emfd" = "eMFD", "ddr" = "DDR", "ccr_multi_to_multi" = "CCR (multi ref/embed)", "ccr_multi_to_en" = "CCR (engl ref, multi embed)", "ccr_en_to_en" = "CCR (engl ref/embed)", "moralbert" = "MoralBERT")


## Monolingual and tables

methods <- c("ccr_en_to_en", "ddr", "mfd", "mfd2", "emfd", "moralbert")
for (method in methods) {
    print(method)
    if (method == "ddr") {
        monolingual <- load_ddr(which = "mono", explicit_manifesto_id = TRUE)
        setDT(monolingual)
    } else if (method == "ccr_en_to_en") {
        monolingual <- load_ccr(which = "en_to_en", explicit_manifesto_id = TRUE)
        setDT(monolingual)
    } else if (method == "ccr_multi_to_en") {
        string_of_file_name <- substring(method, 5) # [4:]
        monolingual <- load_ccr(which = string_of_file_name, explicit_manifesto_id = TRUE)
        setDT(monolingual)
    } else if (method == "mfd") {
        monolingual <- load_mfd(which = "mono", explicit_manifesto_id = TRUE)[["mfd"]]
        setDT(monolingual)
    } else if (method == "mfd2") {
        monolingual <- load_mfd(which = "mono", explicit_manifesto_id = TRUE)[["mfd2"]]
        setDT(monolingual)
    } else if (method == "emfd") {
        monolingual <- load_mfd(which = "mono", explicit_manifesto_id = TRUE)[["emfd"]]
        setDT(monolingual)
    } else if (method == "moralbert") {
        monolingual <- load_moral_bert(explicit_manifesto_id = TRUE)
        setDT(monolingual)
    }

    data_merged <- merge(regression_prepared, monolingual, by = "manifesto_id")

    # print(language)
    if (method == "ddr") {
        string_of_column <- "ddr_all_en_"
    } else if (method == "mfd2") {
        string_of_column <- "mfd2_"
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
    for (pole in poles) {
        list_of_dimensional_results <- c()
        for (dimension in dimensions) {
            dependent <- paste0(dimension, ".", pole)
            form_for_model <- paste0(string_of_column, dependent, "~ galtan + year  + cabinet_party + (1| countryname)")
            model <- lmer(as.formula(form_for_model), data = data_merged)

            #     # standardise model to get coefficients
            standardised_model <- datawizard::standardise(model)

            list_of_dimensional_results <- c(list_of_dimensional_results, standardised_model)

            if (performance::check_singularity(standardised_model)) {
                overall_results <- overall_results %>% add_row(
                    foundation = dimension,
                    pole = pole,
                    method = method,
                    coef = NA,
                    ci_low = NA,
                    ci_high = NA
                )
            } else {
                galtan_coef <- fixef(standardised_model)["galtan"]
                confidence_intervals <- confint(standardised_model, method = "Wald")
                overall_results <- overall_results %>% add_row(
                    foundation = dimension,
                    pole = pole,
                    method = method,
                    language = "English Translation",
                    coef = as.double(galtan_coef),
                    ci_low = confidence_intervals["galtan", ]["2.5 %"],
                    ci_high = confidence_intervals["galtan", ]["97.5 %"]
                )
            }
        }
        texreg(list_of_dimensional_results,
            dcolumn = TRUE, booktabs = TRUE, single.row = TRUE,
            custom.model.names = str_to_title(dimensions),
            custom.coef.map = list(
                "galtan" = "GAL-TAN (CHES)", "year" = "Year",
                "cabinet_partyTRUE" = "Government Participation", "countryname" = "Country"
            ),
            custom.gof.names = c(
                NA, NA, NA, "Num. manifestos", "Num. groups: countries",
                "Var: Country (Intercept)", NA
            ),
            caption = paste0(
                "Regression table for all English-translated manifestos, scored with ",
                tool_mapping[method], " for the dimension ", str_to_title(pole), "."
            ),
            use.packages = FALSE,
            file = paste0("graphs/tables/regression_appendix/translated_", method, "_", pole, ".tex"),
            label = paste0("tab:app_reg_trans_", method, "_", pole)
        )
    }
}

write_feather(overall_results, "graphs/main_article/figure5_data.feather")

# Plot Monolingual

overall_results |>
    mutate(
        Foundation = stringr::str_to_title(foundation),
        Pole = stringr::str_to_title(pole),
        Method = case_match(
            method,
            c("ddr") ~ "DDR",
            "ccr_en_to_en" ~ "CCR (engl ref/embed)",
            "moralbert" ~ "MoralBERT",
            "mfd2" ~ "MFD2",
            "mfd" ~ "MFD",
            "emfd" ~ "eMFD"
        )
    ) |>
    mutate(
        Foundation = factor(Foundation, levels = c("Care", "Fairness", "Loyalty", "Authority", "Sanctity")),
        Pole = factor(Pole, levels = c("Virtue", "Vice")),
        Method = factor(Method, levels = tool_mapping)
    ) |>
    ggplot(aes(
        x = coef,
        y = reorder(Foundation, desc(Foundation)),
        colour = Method
    )) +
    geom_point(size = 3, position = position_dodge(width = 0.5)) +
    geom_linerange(aes(xmin = ci_low, xmax = ci_high), position = position_dodge(width = 0.5)) +
    geom_vline(xintercept = 0, linetype = "dotted", linewidth = 1) +
    facet_wrap(~Pole) +
    xlab("Standardised Regression Coefficient of GAL-TAN CHES score (Wald CI)") +
    ylab("Foundation")
ggsave(here("graphs", "regression", "regression_plot_mono.pdf"), width = 10, height = 10)

## Multilingual tables

methods <- c("ccr_multi_to_multi", "ccr_multi_to_en", "ddr", "mfd")
for (method in methods) {
    print(method)
    if (method == "ddr") {
        multilingual <- load_ddr(which = "merged", aggregate = TRUE, explicit_manifesto_id = TRUE)
        monolingual <- load_ddr(which = "mono", aggregate = TRUE, explicit_manifesto_id = TRUE)
        setDT(multilingual)
        setDT(monolingual)
    } else if (grepl("ccr", method)) {
        string_of_file_name <- substring(method, 5) # [4:]
        multilingual <- load_ccr(which = string_of_file_name, aggregate = TRUE, explicit_manifesto_id = TRUE)
        setDT(multilingual)
    } else if (method == "mfd") {
        multilingual <- load_mfd(which = "merged", aggregate = TRUE, explicit_manifesto_id = TRUE)
        setDT(multilingual)
    }

    data_merged <- merge(regression_prepared, multilingual, by = "manifesto_id", all.x = TRUE)

    # for DDR, we also need the english data, which is in the monolingual dataframe
    if (method == "ddr") {
        data_merged <- merge(data_merged, monolingual, by = "manifesto_id", all.x = TRUE)
    }

    for (language in iso_languages) {
        # print(language)
        if ((language == "en") & (method == "ddr")) {
            string_of_column <- "ddr_all_en_"
        } else if ((language == "en") & (method == "mfd")) {
            string_of_column <- "english_anchor_"
        } else {
            string_of_column <- paste0(method, "_")
        }
        for (pole in poles) {
            list_of_dimensional_results <- list()
            if ((language == "es") & (pole == "vice") & (method == "ccr_multi_to_multi")) {
                dimension_names <- c("care", "fairness", "authority", "sanctity")
            } else {
                dimension_names <- c("care", "fairness", "loyalty", "authority", "sanctity")
            }
            for (dimension in dimensions) {
                # spanish vignettes for CCR vice don't include some dimensions
                if ((language == "es") & (dimension == "loyalty") & (pole == "vice") & (method == "ccr_multi_to_multi")) {
                    next
                }
                dependent <- paste0(dimension, ".", pole)
                number_of_grouping_factors <- length(unique(data_merged[eval(data_merged$language_iso == language), countryname]))
                if (number_of_grouping_factors < 2) {
                    not_grouped <- TRUE
                } else {
                    not_grouped <- FALSE
                }
                if (not_grouped) {
                    form_for_model <- paste0(string_of_column, dependent, " ~ galtan + year + cabinet_party")
                    model <- lm(as.formula(form_for_model),
                        data = data_merged[eval(data_merged$language_iso == language), ]
                    )
                } else {
                    form_for_model <- paste0(string_of_column, dependent, " ~ galtan + year  + cabinet_party + (1| countryname)")
                    model <- lmer(as.formula(form_for_model),
                        data = data_merged[eval(data_merged$language_iso == language), ]
                    )
                }


                #     test_model <- lmer(as.formula(form_for_model), data = data_merged)
                #     # standardise model to get coefficients
                standardised_model <- datawizard::standardise(model)

                # for some reason, appending to a list works for lmer models, but nto lm models, so we need to do it like this
                list_of_dimensional_results[[length(list_of_dimensional_results) + 1]] <- datawizard::standardise(model)
                if (performance::check_singularity(standardised_model)) {
                    overall_results <- overall_results %>% add_row(
                        foundation = dimension,
                        pole = pole,
                        method = method,
                        coef = NA,
                        ci_low = NA,
                        ci_high = NA
                    )
                } else {
                    if (not_grouped) {
                        galtan_coef <- coef(standardised_model)["galtan"]
                    } else {
                        galtan_coef <- fixef(standardised_model)["galtan"]
                    }
                    confidence_intervals <- confint(standardised_model, method = "Wald")
                    overall_results2 <- overall_results2 %>% add_row(
                        foundation = dimension,
                        pole = pole,
                        method = method,
                        language = language,
                        coef = as.double(galtan_coef),
                        ci_low = confidence_intervals["galtan", ]["2.5 %"],
                        ci_high = confidence_intervals["galtan", ]["97.5 %"]
                    )
                }
            }
            if (not_grouped) {
                custom.coef.map <- list(
                    "galtan" = "GAL-TAN (CHES)", "year" = "Year",
                    "cabinet_partyTRUE" = "Government Participation"
                )
                custom.gof.names <- c(
                    NA, NA, "Num. manifestos"
                )
            } else {
                custom.coef.map <- list(
                    "galtan" = "GAL-TAN (CHES)", "year" = "Year",
                    "cabinet_partyTRUE" = "Government Participation", "countryname" = "Country"
                )
                custom.gof.names <- c(
                    NA, NA, NA, "Num. manifestos", "Num. groups: countries",
                    "Var: Country (Intercept)", NA
                )
            }
            texreg(list_of_dimensional_results,
                dcolumn = TRUE, booktabs = TRUE, single.row = TRUE,
                custom.model.names = str_to_title(dimension_names),
                custom.coef.map = custom.coef.map,
                custom.gof.names = custom.gof.names,
                caption = paste0(
                    "Regression table for all manifestos in ", language_mapping[language], ", scored with ",
                    tool_mapping[method], " for the dimension ", str_to_title(pole), "."
                ),
                use.packages = FALSE,
                file = paste0("graphs/tables/regression_appendix/", language, "_", method, "_", pole, ".tex"),
                label = paste0("tab:app_reg_", language, "_", method, "_", pole)
            )
        }
    }
}


# Plots separated by language

for (language_here in iso_languages) {
    overall_results2 |>
        filter(language == language_here) |>
        mutate(
            Foundation = stringr::str_to_title(foundation),
            Pole = stringr::str_to_title(pole),
            Method = case_match(
                method,
                c("ddr") ~ "DDR",
                "ccr_multi_to_multi" ~ "CCR (multi ref/embed)",
                "ccr_multi_to_en" ~ "CCR (engl ref, multi embed)",
                "mfd" ~ "MFD",
                "emfd" ~ "eMFD"
            )
        ) |>
        mutate(
            Foundation = factor(Foundation, levels = c("Care", "Fairness", "Loyalty", "Authority", "Sanctity")),
            Pole = factor(Pole, levels = c("Virtue", "Vice")),
            Method = factor(Method, levels = tool_mapping)
        ) |>
        ggplot(aes(
            x = coef,
            y = reorder(Foundation, desc(Foundation)),
            colour = Method
        )) +
        geom_point(size = 3, position = position_dodge(width = 0.5)) +
        geom_linerange(aes(xmin = ci_low, xmax = ci_high), position = position_dodge(width = 0.5)) +
        geom_vline(xintercept = 0, linetype = "dotted", linewidth = 1) +
        facet_wrap(~Pole) +
        xlab(paste0(language_mapping[language_here], " manifestos: Standardised Regression Coefficient of GAL-TAN CHES score (Wald CI)")) +
        ylab("Foundation")
    ggsave(here("graphs", "regression", paste0("regression_plot_multi", language_here, ".pdf")), width = 8, height = 10)
}

write_feather(overall_results2, "graphs/main_article/figure6_data.feather")

## Plots separated by dimension
for (dimension in dimensions) {
    overall_results2 |>
        filter(foundation == dimension) |>
        mutate(
            Language = case_match(
                language,
                "en" ~ "English",
                "nl" ~ "Dutch",
                "es" ~ "Spanish",
                "de" ~ "German"
            ),
            Pole = stringr::str_to_title(pole),
            Method = case_match(
                method,
                c("ddr") ~ "DDR",
                "ccr_multi_to_multi" ~ "CCR (multi ref/embed)",
                "ccr_multi_to_en" ~ "CCR (engl ref, multi embed)",
                "mfd" ~ "MFD",
                "emfd" ~ "eMFD"
            )
        ) |>
        mutate(
            Language = factor(Language, levels = c("Dutch", "English", "German", "Spanish")),
            Pole = factor(Pole, levels = c("Virtue", "Vice")),
            Method = factor(Method, levels = tool_mapping)
        ) |>
        ggplot(aes(
            x = coef,
            y = reorder(Language, desc(Language)),
            colour = Method
        )) +
        geom_point(size = 3, position = position_dodge(width = 0.5)) +
        geom_linerange(aes(xmin = ci_low, xmax = ci_high), position = position_dodge(width = 0.5)) +
        geom_vline(xintercept = 0, linetype = "dotted", linewidth = 1) +
        facet_wrap(~Pole) +
        xlab(paste0(str_to_title(dimension), " found., Standardised Regression Coefficient of GAL-TAN CHES score (Wald CI)")) +
        ylab("Language")
    ggsave(here("graphs", "regression", paste0("regression_plot_foundations_multi_", dimension, ".pdf")), width = 8, height = 10)
}


## Table for appendix
data <- read_feather(here("data", "for_graphs", "regression", "current_working_data.feather"))

unique(data[, c("countryname", "partyname", "manifesto_year", "ches_year")]) |>
    mutate(
        ches_year = year(ches_year),
        manifesto_year = year(manifesto_year)
    ) |>
    arrange(desc(countryname), desc(partyname)) |>
    rename(
        "Country" = countryname,
        "Party" = partyname,
        "Year (Manifesto)" = manifesto_year,
        "Year (CHES)" = ches_year
    ) |>
    kable(
        format = "latex", booktabs = TRUE, longtable = T,
        caption = "Manifestos included in the regression analysis",
    ) |>
    collapse_rows(columns = 1:2) |>
    kable_styling(latex_options = c("repeat_header")) |>
    save_kable("graphs/tables/regression_descriptives.tex")
