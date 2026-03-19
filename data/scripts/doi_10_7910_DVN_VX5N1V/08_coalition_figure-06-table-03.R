library(tidyverse)
library(patchwork)
library(fs)
library(haven)
library(wacolors)
library(sf)
library(ggsflabel)
library(ggtext)
library(glue)
library(kableExtra)


cd_shp <- read_rds("data/shp_cd.rds") # from donnermap
st_shp <- read_rds("data/shp_st.rds")


# Data -----
cc_cd <- read_csv(path("data/mrp-ests_by-cd-race.csv"))


# Coalition computation -----
compute_coal <- function(tbl) {
    tbl |>
    summarize(
        black_alone_D = sum((race == "Black")*(1 - p_mrp_twway)*N) / sum(N),
        hisp_alone_D  = sum((race == "Hispanic")*(p_mrp_twway < 0.5)*(1 - p_mrp_twway)*N) / sum(N),
        white_alone_D = sum((race == "White")*(1 - p_mrp_twway)*N) / sum(N),
        other_alone_D = sum((race == "Other")*(1 - p_mrp_twway)*N) / sum(N),
        black_hisp_D  = sum((race %in% c("Hispanic", "Black"))*(p_mrp_twway < 0.5)*(1 - p_mrp_twway)*N) / sum(N),
        bho_D  = sum((!race %in% c("White"))*(p_mrp_twway < 0.5)*(1 - p_mrp_twway)*N) / sum(N),
        white_other_D = sum((race %in% c("White", "Other"))*(1 - p_mrp_twway)*N) / sum(N),
        white_alone_R = sum((race == "White")*(p_mrp_twway)*N) / sum(N),
        hisp_alone_R = sum((race == "Hispanic")*(p_mrp_twway)*N) / sum(N),
        black_hisp_R = sum((race %in% c("Hispanic", "Black"))*(p_mrp_twway)*N) / sum(N),
        bho_R = sum((!race %in% "White")*(p_mrp_twway)*N) / sum(N),
        .groups = "drop"
    ) |>
        mutate(
            coalition = case_when(
                white_alone_R > 0.5 ~ "White alone Republican",
                hisp_alone_R > 0.5 ~ "Hispanic alone Republican",
                black_hisp_R > 0.5 ~ "Black and Hispanic Republican",
                bho_R > 0.5 ~ "Black + Hispanic + Other Republican",
                white_alone_D > 0.5 ~ "White alone Democrat",
                black_alone_D > 0.5 ~ "Black alone Democrat",
                hisp_alone_D > 0.5  ~ "Hispanic alone Democrat",
                black_hisp_D > 0.5  ~ "Black + Hispanic Democrat",
                bho_D > 0.5  ~ "Black + Hispanic + Other Democrat",
                TRUE ~ NA_character_
            )
        )
}


cd_coal <- cc_cd |>
    group_by(year, cd, pct_trump) |>
    compute_coal()

# xtabs ----
lbl_f <- function(x) str_remove_all(x, "\\s(Democrat|Republican)")
cd_16 <- cd_coal |>
    mutate(winner = ifelse(pct_trump > 0.5, "R", "D"),
           coalition = lbl_f(coalition)) |>
    filter(year == 2016) |>
    count(winner, coalition)

xtabs_coal <- cd_16 |>
    pivot_wider(id_cols = coalition,
                names_from = c(winner),
                values_from = n, values_fill = 0) |>
    rename(type = coalition) |>
    mutate(type = recode_factor(
        type,
        "White alone" = "White voters alone",
        "Black alone" = "Black voters alone",
        "Hispanic alone" = "Hispanic voters alone",
        "Black + Hispanic" = "Black and Hispanic voters",
        "Black + Hispanic + Other" = "Black, Hispanic, and Other non-White voters",
        .missing = "White and non-White"
    )) |>
    arrange(type) |>
    add_row(type = "Total", D = sum(cd_16$n[cd_16$winner == "D"]), R = sum(cd_16$n[cd_16$winner == "R"])) |>
    kbl(booktabs = TRUE,
        align = c("r", rep("c", 4)),
        format = "latex",
        linesep = "",
        col.names = c("Racial Group Composition for Sufficient for a Majority", "Democratic CDs", "Republican CDs")) |>
        row_spec(6, hline_after = TRUE)



# Figure map  -----
#' Make map of coalition
fig_coal <- function(cd_data = cd_coal, yr = 2016, other_as_nonwhite = FALSE) {

    candD <- recode(yr, `2016` = "Clinton", `2020` = "Biden")
    candR <- recode(yr, `2016` = "Trump", `2020` = "Trump")

    if (isTRUE(other_as_nonwhite)) {
        minority_lang <- "Black, Hispanic, and Other"
    }

    if (isFALSE(other_as_nonwhite)) {
        cd_data <- cd_data |>
            mutate(coalition = replace(coalition, str_detect(coalition, "Black.*Hispanic.*Other"), NA_character_)) |>
            select(-matches("^bho_"))
        minority_lang <- "Black and Hispanic"
    }

    gg_cd <- cd_shp |>
        left_join(cd_data) |>
        filter(year == yr) |>
        mutate(coal_R = replace(coalition, !str_detect(coalition, "Republican"), NA_character_),
               coal_D = replace(coalition, !str_detect(coalition, "Democrat"), NA_character_),
               winner = ifelse(pct_trump > 0.5, candR, candD),
               crossover_D = ifelse(is.na(coalition) & (winner == candD), "Crossover", NA_character_),
               crossover_R = ifelse(is.na(coalition) & (winner == candR), "Crossover", NA_character_)
        )

    # Base plot ---
    gg_base <- gg_cd |>
        ggplot() +
        geom_sf(size = 0.1, alpha = 0.5, color = "white") +
        geom_sf(data = st_shp, size = 0.3, fill = "transparent") +
        ggthemes::theme_map() +
        theme(plot.title = element_markdown(
            hjust = 0.5,
            margin = margin(t = 5, b = -25)),
            legend.position = c(0.75, 0.15))

    # R
    gg_R_W <- gg_base +
        geom_sf(data = filter(gg_cd, coalition == "White alone Republican"),
                aes(fill = coal_R),
                size = 0.1) +
        scale_fill_manual(values = c(
            `White alone Republican` = "#D48792"),
            labels = lbl_f,
            na.value = "white") +
        labs(fill = NULL,
             title = glue("**{candR}** won with<br>**White** voters alone"))

    gg_R_M <- gg_base +
        geom_sf(data = filter(gg_cd, str_detect(coalition, "(Black|Hispanic).*Republican")),
                aes(fill = coal_R),
                size = 0.1) +
        labs(fill = NULL,
             title = glue("**{candR}** won with<br>**{minority_lang}** voters alone"))

    gg_D_W <- gg_base +
        geom_sf(data = filter(gg_cd, coalition == "White alone Democrat"),
                aes(fill = coal_D),
                size = 0.1) +
        scale_fill_manual(values = c(
            "White alone Democrat" = "#D48792"),
            labels = lbl_f,
            na.value = "white") +
        geom_sf_label_repel(
            data = filter(gg_cd, coalition == "White alone Democrat"),
            aes(label = cd, fill = coal_D),
            color = "white",
            label.size = NA,
            alpha = 1,
            show.legend = FALSE,
            size = 1.5
        ) +
        labs(fill = NULL,
             title = glue("**{candD}** won with<br>**White** voters alone"))

    gg_D_M <- gg_base +
        geom_sf(data = filter(gg_cd, str_detect(coalition, "(Black|Hispanic).*Democrat")),
                aes(fill = coal_D),
                size = 0.1) +
        scale_fill_manual(values = c(
            "Black alone Democrat" = "#465177",
            "Hispanic alone Democrat" = "#A4BADF",
            "Black + Hispanic Democrat" = "#516F25", # "#E4C22B",
            "Black + Hispanic + Other Democrat" = "#EFC519"),
            labels = lbl_f,
            na.value = "white") +
        geom_sf_label_repel(
            data = filter(gg_cd, str_detect(coalition, "(Black|Hispanic).*Democrat")),
            aes(label = cd, fill = coal_D),
            color = "white",
            label.size = NA,
            alpha = 0.9,
            show.legend = FALSE,
            max.overlaps = 20,
            size = 1.5
        ) +
        labs(fill = NULL,
             title = glue("**{candD}** won with<br>**{minority_lang}** voters alone")) +
        theme(legend.position = c(0.775, 0.05))

    gg_cross_D <- gg_base +
        geom_sf(data = filter(gg_cd, !is.na(crossover_D)),
                aes(fill = crossover_D),
                size = 0.1) +
        scale_fill_manual(
            values = c(`Crossover` = "#CC7810"),
            na.value = "white") +
        labs(fill = NULL,
             title = glue("**{candD}** won, but needs<br>both Whites and non-Whites"))

    gg_cross_R <- gg_base +
        geom_sf(data = filter(gg_cd, !is.na(crossover_R)),
                aes(fill = crossover_R),
                size = 0.1) +
        scale_fill_manual(
            values = c(`Crossover` = "#CC7810"),
            na.value = "white") +
        labs(fill = NULL,
             title = glue("**{candR}** won, but needs<br>both Whites and non-Whites"))

    gg_D_W + gg_R_W +
        gg_D_M + gg_R_M +
        gg_cross_D + gg_cross_R +
        plot_layout(design =
                    "AB
                     CD
                     EF")
}


# Save -------
# Saves to Figure 6 and Table 3
gg16 <- fig_coal(yr = 2016, other_as_nonwhite = TRUE)
ggsave("figures/coalition_by-cd_2016.pdf", gg16, w = 9, h = 10)

gg20 <- fig_coal(yr = 2020, other_as_nonwhite = TRUE)
ggsave("figures/coalition_by-cd_2020.pdf", gg20, w = 9, h = 10)

xtabs_coal |>
    write_lines("tables/coalition_counts_2016.tex")
