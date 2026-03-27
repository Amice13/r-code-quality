library(arrow)
library(data.table)
library(dplyr)
library(kableExtra)

source("scripts/utils_figures.R")

# First create all figures for appendix
# as well as saving intermediate results
# for this script
density_plots()
density_plots_alt()
descriptive_statistics_table_app()
correlations_same_method()
correlations_same_category()
multilingual_graphic()


## Table 1
data <- read_feather("data/source/manifesto_corpus.feather")
setDT(data)
data[, .(unique_manifesto_count = uniqueN(manifesto_id)), by = language_iso] |>
    mutate(language_iso = case_match(
        language_iso,
        "es" ~ "Spanish",
        "nl" ~ "Dutch",
        "de" ~ "German",
        "en" ~ "English"
    )) |>
    rename(Language = language_iso, "No of Manifestos" = unique_manifesto_count) |>
    kable(
        format = "latex", digits = 2, booktabs = TRUE,
        caption = "Descriptives for each language", label = "tab:no_of_manifestos"
    ) |>
    save_kable("graphs/main_article/table_1.tex")

## Table 2
data[, .(unique_manifesto_count = uniqueN(manifesto_id)), by = countryname] |>
    arrange(desc(unique_manifesto_count)) |>
    slice_max(n = 10, order_by = unique_manifesto_count) |>
    rename(Country = countryname, "No of Manifestos" = unique_manifesto_count) |>
    kable(
        format = "latex", digits = 2, booktabs = TRUE,
        caption = "Descriptives for the top ten countries", label = "tab:documents_per_country"
    ) |>
    save_kable("graphs/main_article/table_2.tex")


## Figure 1
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

ggsave(here("graphs", "main_article", "figure1.png"),
    width = 9, height = 8, dpi = 600,
    type = "cairo"
)

## Figure 2

figure_2_data <- read_feather("graphs/main_article/figure2_data.feather")

corr_plot <- ggplot(data = figure_2_data, aes(
    x = factor(index), y = factor(reorder(variable, desc(variable))),
    fill = value
)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
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

ggsave(here("graphs", "main_article", "figure2.pdf"),
    width = 11, height = 14
)

## Figure 3

figure_3_data <- read_feather("graphs/main_article/figure3_data.feather")


corplot <- ggplot(data = figure_3_data, aes(index, variable,
    fill = value
)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
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

ggsave(here("graphs", "main_article", "figure3.pdf"),
    width = 12, height = 12
)

## Figure 4
figure_4_data <- read_feather("graphs/main_article/figure4_data.feather")

figure_4_data |> ggplot(aes(
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
ggsave(here("graphs", "main_article", "figure4.pdf"), width = 8, height = 5)


## Figure 5

figure_5_data <- read_feather("graphs/main_article/figure5_data.feather")

tool_mapping <- list(
    "mfd" = "MFD", "mfd2" = "MFD2", "emfd" = "eMFD", "ddr" = "DDR",
    "ccr_multi_to_multi" = "CCR (multi ref/embed)",
    "ccr_multi_to_en" = "CCR (engl ref, multi embed)",
    "ccr_en_to_en" = "CCR (engl ref/embed)", "moralbert" = "MoralBERT"
)


figure_5_data |>
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
        Foundation = factor(Foundation,
            levels = c("Care", "Fairness", "Loyalty", "Authority", "Sanctity")
        ),
        Pole = factor(Pole, levels = c("Virtue", "Vice")),
        Method = factor(Method, levels = tool_mapping)
    ) |>
    ggplot(aes(
        x = coef,
        y = reorder(Foundation, desc(Foundation)),
        colour = Method
    )) +
    geom_point(size = 3, position = position_dodge(width = 0.5)) +
    geom_linerange(aes(xmin = ci_low, xmax = ci_high),
        position = position_dodge(width = 0.5)
    ) +
    geom_vline(xintercept = 0, linetype = "dotted", linewidth = 1) +
    facet_wrap(~Pole) +
    xlab("Standardised Regression Coefficient of GAL-TAN CHES score (Wald CI)") +
    ylab("Foundation")

ggsave(here("graphs", "main_article", "figure5.pdf"), width = 10, height = 10)

## Figure 6
figure_6_data <- read_feather("graphs/main_article/figure6_data.feather")

figure_6_data |>
    filter(foundation == "care") |>
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
        Language = factor(Language,
            levels = c("Dutch", "English", "German", "Spanish")
        ),
        Pole = factor(Pole, levels = c("Virtue", "Vice")),
        Method = factor(Method, levels = tool_mapping)
    ) |>
    ggplot(aes(
        x = coef,
        y = reorder(Language, desc(Language)),
        colour = Method
    )) +
    geom_point(size = 3, position = position_dodge(width = 0.5)) +
    geom_linerange(aes(xmin = ci_low, xmax = ci_high),
        position = position_dodge(width = 0.5)
    ) +
    geom_vline(xintercept = 0, linetype = "dotted", linewidth = 1) +
    facet_wrap(~Pole) +
    xlab(
        "Care found., Standardised Regression Coefficient of GAL-TAN CHES score (Wald CI)"
    ) +
    ylab("Language")
ggsave(here("graphs", "main_article", "figure6.pdf"), width = 8, height = 10)
