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

retrieve_long_dataset <- function(normalised = TRUE) {
    methods <- c("ccr", "emfd", "ddr", "mfd2", "mfd", "moralbert")
    methods_pretty_names <- c("MFD", "MFD2", "eMFD", "DDR", "CCR", "MoralBERT")
    dimensions <- c("Care", "Fairness", "Loyalty", "Authority", "Sanctity")
    full_data <- data.frame()
    if (normalised) {
        normalised_string <- "_normalised"
    } else {
        normalised_string <- ""
    }
    for (method_name in methods) {
        temp_data <- read_feather(here(
            "data", "for_graphs", "same_method",
            paste0("data_", method_name, normalised_string, ".feather")
        ))
        temp_data$method <- method_name
        full_data <- rbind(full_data, temp_data)
    }
    long_full_data <- full_data |>
        pivot_longer(
            cols = matches("[a-z]*\\.(vice|virtue)"),
            names_to = c("foundation", "pole"),
            names_pattern = "([a-z]*).([a-z]*)",
            values_to = "score"
        ) |>
        mutate(
            foundation = str_to_title(foundation),
            pole = str_to_title(pole)
        ) |>
        mutate(
            foundation = factor(foundation, levels = dimensions),
            method = case_match(
                method,
                "mfd2" ~ "MFD2",
                "mfd" ~ "MFD",
                "ccr" ~ "CCR",
                "emfd" ~ "eMFD",
                "ddr" ~ "DDR",
                "moralbert" ~ "MoralBERT"
            )
        ) |>
        mutate(
            method = factor(method, levels = methods_pretty_names)
        )
    return(long_full_data)
}

get_significant_stars <- function(value_to_test) {
    # print(value_to_test)
    value_to_test <- as.numeric(value_to_test)
    # for self-correlations
    if (value_to_test < 0.001) {
        value_to_replace <- "***"
    } else if (value_to_test < 0.01) {
        value_to_replace <- "**"
    } else if (value_to_test < 0.05) {
        value_to_replace <- "*"
    } else {
        value_to_replace <- ""
    }
    return(value_to_replace)
}

density_plots <- function() {
    methods <- c("ccr", "emfd", "ddr", "mfd2", "mfd", "moralbert")
    methods_pretty_names <- c("MFD", "MFD2", "eMFD", "DDR", "CCR", "MoralBERT")
    dimensions <- c("Care", "Fairness", "Loyalty", "Authority", "Sanctity")
    long_full_data <- retrieve_long_dataset()

    long_full_data |> ggplot(aes(
        y = score,
        fill = pole,
        x = foundation
    )) +
        geom_violin(scale = "area") +
        labs(
            x = "Foundation",
            y = "Morality Score",
            fill = "Pole"
        ) +
        facet_wrap(~method, ncol = 2)

    ggsave(here("graphs", "density", paste0("density_", "all", "_normalised.png")),
        width = 9, height = 8, dpi = 600,
        type = "cairo"
    )
}

density_plots_alt <- function() {
    methods <- c("ccr", "emfd", "ddr", "mfd2", "mfd", "moralbert")
    methods_pretty_names <- c("MFD", "MFD2", "eMFD", "DDR", "CCR", "MoralBERT")
    dimensions <- c("Care", "Fairness", "Loyalty", "Authority", "Sanctity")
    long_full_data <- retrieve_long_dataset()

    long_full_data |>
        ggplot(aes(
            y = score,
            fill = pole,
            x = method
        )) +
        geom_violin(scale = "area") +
        # geom_boxplot() +
        labs(
            x = "Method",
            y = "Morality Score",
            fill = "Pole"
        ) +
        facet_wrap(~foundation, ncol = 1)
    ggsave(here("graphs", "density", paste0("density_alt_", "all", "_normalised.png")),
        width = 9, height = 11, dpi = 600,
        type = "cairo"
    )
}

descriptive_statistics_table_app <- function() {
    methods <- c("ccr", "emfd", "ddr", "mfd2", "mfd", "moralbert")
    methods_pretty_names <- c("MFD", "MFD2", "eMFD", "DDR", "CCR", "MoralBERT")
    long_full_data <- retrieve_long_dataset(normalised = FALSE)

    long_full_data |>
        rename(
            Pole = pole,
            Foundation = foundation,
            Method = method
        ) |>
        group_by(Method, Pole, Foundation) |>
        summarize(
            "Mean" = mean(score),
            "SD" = sd(score),
            "Coefficient of Variation" = sd(score) / mean(score),
        ) |>
        kable(
            format = "latex", digits = 2, booktabs = TRUE, longtable = TRUE,
            caption = "Descriptives of different measurement instruments (not normalised)", label = "tab:descriptives_mean_sd"
        ) |>
        collapse_rows(columns = 1:3) |>
        kable_styling(latex_options = c("repeat_header")) |>
        save_kable("graphs/tables/scoring_descriptive.tex")
}

correlations_same_method <- function() {
    methods <- c("mfd", "mfd2", "ccr", "emfd", "ddr", "moralbert")
    methods_pretty_names <- c("MFD", "MFD2", "eMFD", "DDR", "CCR", "MoralBERT")
    poles <- c("virtue", "vice")
    dimensions <- c("Care", "Fairness", "Loyalty", "Authority", "Sanctity")
    correlation_measurements <- c("kendall", "spearman")
    levels_for_corr <- c()
    # rearrange the foundations as is typical in literature
    for (pole in poles) {
        for (dimension in dimensions) {
            levels_for_corr <- c(levels_for_corr, paste0(dimension, " (", stringr::str_to_title(pole), ")"))
        }
    }

    for (correlation_measure in correlation_measurements) {
        full_data <- data.frame()
        full_data_pv <- data.frame()
        for (method_name in methods) {
            temp_data <- read_feather(here("data", "for_graphs", "same_method", paste0(
                "correlation_", method_name, "_", correlation_measure,
                ".feather"
            )))
            temp_data_pv <- read_feather(here("data", "for_graphs", "same_method", paste0("correlation_", method_name, "_pvalues__", correlation_measure, ".feather")))
            temp_data$method <- method_name
            temp_data_pv$method <- method_name
            full_data <- rbind(full_data, temp_data)
            full_data_pv <- rbind(full_data_pv, temp_data_pv)
        }

        full_data_pv <- sapply(full_data_pv, as.character)
        for (i in seq_len(nrow(full_data_pv))) {
            for (j in seq(from = 2, to = ncol(full_data_pv) - 1)) {
                # value_to_test <- full_data_pv[i, j]
                if (full_data_pv[, "index"][i] == colnames(full_data_pv)[j]) {
                    full_data_pv[i, j] <- ""
                } else {
                    full_data_pv[i, j] <- get_significant_stars(full_data_pv[i, j])
                }
            }
        }
        full_data_pv <- as.data.frame(full_data_pv)

        transform_long_to_wide <- function(one_df) {
            all_correlations <- data.frame()
            for (method in methods) {
                melted_cormat <- reshape2::melt(one_df[full_data$method == method, ][1:ncol(one_df) - 1], id.vars = "index")
                melted_cormat$variable <- as.character(melted_cormat$variable)
                # i apologise for an ugly for loop
                for (i in 1:nrow(melted_cormat)) {
                    melted_cormat[i, ]$variable <- paste0(stringr::str_to_title(str_split(melted_cormat[i, ]$variable, "\\.")[[1]][1]), " (", stringr::str_to_title(str_split(melted_cormat[i, ]$variable, "\\.")[[1]][2]), ")")
                    melted_cormat[i, ]$index <- paste0(stringr::str_to_title(str_split(melted_cormat[i, ]$index, "\\.")[[1]][1]), " (", stringr::str_to_title(str_split(melted_cormat[i, ]$index, "\\.")[[1]][2]), ")")
                }

                melted_cormat$method <- method
                all_correlations <- rbind(all_correlations, melted_cormat)
            }
            return(all_correlations)
        }

        all_correlations <- transform_long_to_wide(full_data)
        all_correlations_pv <- transform_long_to_wide(full_data_pv)
        display_correlations <- all_correlations_pv |>
            rename(significance = value) |>
            right_join(all_correlations, by = c("index", "variable", "method")) |>
            rowwise() |>
            mutate(
                display_text = paste0(round(value, digits = 1), " ", significance),
                index = factor(index, levels = levels_for_corr),
                variable = factor(variable, levels = levels_for_corr),
                method = case_match(
                    method,
                    "mfd2" ~ "MFD2",
                    "mfd" ~ "MFD",
                    "ccr" ~ "CCR",
                    "emfd" ~ "eMFD",
                    "ddr" ~ "DDR",
                    "moralbert" ~ "MoralBERT"
                )
            ) |>
            mutate(
                method = factor(method, levels = methods_pretty_names)
            )

        if (correlation_measure == "kendall") {
            write_feather(display_correlations, "graphs/main_article/figure2_data.feather")
        }

        corr_plot <- ggplot(data = display_correlations, aes(
            x = factor(index), y = factor(reorder(variable, desc(variable))),
            # aes(reorder(index, desc(as.character(index))),
            #    reorder(variable, desc(as.character(variable))),
            fill = value
        )) +
            geom_tile(color = "white") +
            scale_fill_gradient2( # low = "blue", high = "red", mid = "white",
                midpoint = 0, limit = c(-1, 1), space = "Lab",
                name = "Correlation"
            ) +
            labs(
                x = "", y = ""
            ) +
            theme(axis.text.x = element_text(
                angle = 45, vjust = 1,
                hjust = 1
            )) +
            coord_fixed() +
            facet_wrap(~method, labeller = labeller(method = c(
                `mfd2` = "MFD2",
                `ccr` = "CCR",
                `emfd` = "eMFD",
                `ddr` = "DDR",
                `moralbert` = "MoralBERT"
            )), ncol = 2) +
            geom_text(aes(index, variable, label = display_text), size = 2.8)

        ggsave(here("graphs", "same_method", paste0("correlation_all_", correlation_measure, ".pdf")),
            width = 11, height = 14
        )
    }
}

correlations_same_category <- function() {
    methods <- c("mfd", "mfd2", "ccr", "emfd", "ddr", "moralbert")
    methods_pretty_names <- c("MFD", "MFD2", "eMFD", "DDR", "CCR", "MoralBERT")

    poles <- c("virtue", "vice")
    dimensions <- c("Care", "Fairness", "Loyalty", "Authority", "Sanctity")
    correlation_measurements <- c("kendall", "spearman")
    for (correlation_measure in correlation_measurements) {
        full_data <- data.frame()
        for (pole in poles) {
            temp_data <- read_feather(here("data", "for_graphs", "same_category", paste0(
                "mean_correlation_", pole, "_", correlation_measure,
                ".feather"
            )))
            temp_data$pole <- pole
            full_data <- rbind(full_data, temp_data)
        }
        correlations <- reshape2::melt(full_data) |>
            mutate(
                index = case_match(
                    index,
                    "mfd" ~ "MFD",
                    "mfd2" ~ "MFD2",
                    "emfd" ~ "eMFD",
                    "ddr_all_en" ~ "DDR",
                    "ccr_en_to_en" ~ "CCR",
                    "moralbert" ~ "MoralBERT"
                ),
                variable = case_match(
                    variable,
                    "mfd" ~ "MFD",
                    "mfd2" ~ "MFD2",
                    "emfd" ~ "eMFD",
                    "ddr_all_en" ~ "DDR",
                    "ccr_en_to_en" ~ "CCR",
                    "moralbert" ~ "MoralBERT"
                ),
                pole = str_to_title(pole)
            ) |>
            mutate(
                index = factor(index, levels = rev(methods_pretty_names), order = TRUE),
                variable = factor(variable, levels = rev(methods_pretty_names), order = TRUE)
            )

        if (correlation_measure == "kendall") {
            write_feather(correlations, "graphs/main_article/figure3_data.feather")
        }

        corplot <- ggplot(data = correlations, aes(index, variable,
            fill = value
        )) +
            geom_tile(color = "white") +
            scale_fill_gradient2( # low = "blue", high = "red", mid = "white",
                midpoint = 0, limit = c(-1, 1), space = "Lab",
                name = "Correlation"
            ) +
            labs(
                x = "", y = ""
            ) +
            theme(axis.text.x = element_text(
                angle = 45, vjust = 1,
                hjust = 1
            )) +
            coord_fixed() +
            facet_wrap(~pole) +
            geom_text(aes(index, variable, label = round(value, digits = 2)), size = 3.5) +
            scale_x_discrete(drop = FALSE) + # this is somehow necessary for the right factor order, don't know why
            scale_y_discrete(drop = FALSE)

        ggsave(here("graphs", "same_category", paste0("same_category_all_", correlation_measure, ".pdf")),
            width = 12, height = 12
        )
    }
}


multilingual_graphic <- function() {
    methods <- c("mfd", "ccr_multi_to_en", "ccr_multi_to_multi", "ddr")
    poles <- c("virtue", "vice")
    dimensions <- c("Care", "Fairness", "Loyalty", "Authority", "Sanctity")
    correlation_measurements <- c("kendall", "spearman")
    for (correlation_measure in correlation_measurements) {
        full_data <- data.frame()
        for (method_name in methods) {
            temp_data <- read_feather(here(
                "data", "for_graphs", "multi",
                paste0("correlations_", method_name, "_", correlation_measure, ".feather")
            ))
            temp_data$method <- method_name
            full_data <- rbind(full_data, temp_data)
        }

        full_data <- full_data |>
            mutate(
                foundation = stringr::str_to_title(foundation),
                pole = stringr::str_to_title(pole),
                language = case_match(
                    language,
                    "es" ~ "Spanish",
                    "nl" ~ "Dutch",
                    "de" ~ "German",
                )
            ) |>
            mutate(
                foundation = factor(foundation, levels = c("Care", "Fairness", "Loyalty", "Authority", "Sanctity")),
                pole = factor(pole, levels = c("Virtue", "Vice"))
            )


        full_data |>
            mutate(
                "method" = case_match(
                    method,
                    "mfd" ~ "MFD",
                    "ccr_multi_to_en" ~ "CCR (engl reference, multi. embedding )",
                    "ccr_multi_to_multi" ~ "CCR (multi. reference, multi. embeding)",
                    "ddr" ~ "DDR"
                ),
                correlation = round(
                    correlation,
                    digits = 2
                ),
                pvalue = case_when(
                    pvalue < 0.001 ~ "<.001",
                    .default = as.character(round(pvalue, digits = 3))
                )
            ) |>
            rename_with(str_to_title, !starts_with("pvalue")) |>
            select(Method, Language, Foundation, Pole, Correlation, pvalue) |>
            arrange(Method, Language, Foundation, Pole, ) |>
            na.omit() |>
            kable(
                format = "latex", booktabs = TRUE, longtable = T, label = paste0("multilingual_appendix_", correlation_measure),
                caption = paste0("Correlations and p-values for multilingual data, (", str_to_title(correlation_measure), ")")
            ) |>
            collapse_rows(columns = 1:3, longtable_clean_cut = FALSE) |>
            kable_styling(latex_options = c("repeat_header")) |>
            save_kable(paste0("graphs/tables/multilingual_correlations_", correlation_measure, ".tex"))

        if (correlation_measure == "kendall") {
            write_feather(full_data, "graphs/main_article/figure4_data.feather")
        }
        full_data |> ggplot(aes(
            x = foundation,
            y = correlation,
            shape = pole,
            colour = language
        )) +
            geom_beeswarm(cex = 3, size = 2) +
            labs(
                x = "Foundation",
                y = "Correlation",
                color = "Language",
                shape = "Pole"
            ) +
            facet_wrap(~method, labeller = labeller(method = c(
                `mfd` = "MFD",
                `ccr_multi_to_en` = "CCR (engl reference, multi. embedding )",
                `ccr_multi_to_multi` = "CCR (multi. reference, multi. embeding)",
                `ddr` = "DDR"
            )))
        ggsave(here("graphs", "multi", paste0("correlations_", "all_", correlation_measure, ".pdf")), width = 8, height = 5)
    }
}
