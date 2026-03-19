library(sf)
library(janitor)
library(tidyverse)
library(scales)
library(patchwork)
library(haven)

# Data ------
est_long <- read_dta("data/cces_by-agg-level.dta") |>
    mutate(race = fct_relevel(haven::as_factor(race), "White", "Black", "Hispanic"),
           division = as_factor(division),
           region = as_factor(region)) |>
    mutate(race = fct_recode(race, "non-White" = "All Non-Whites"),
           race = recode_factor(race, White = "White Voters", Hispanic = "Hispanic Voters", Black = "Black Voters"))

cd_shp <- read_rds("data/shp_cd.rds") # from donnermap
st_shp <- read_rds("data/shp_st.rds")

# Functions ---
## Make map of RV
gg_rv <- function(data,
                  fillvar = p_mrp_twway,
                  pal = "YlOrRd",
                  lim = c(0, 1),
                  brk = seq(0.10, 0.90, by = 0.10), ut = "%",
                  facet_race = TRUE,
                  dir = -1) {
    fillvar <- enquo(fillvar)

    gg <- data |>
        ggplot(aes(fill = !!fillvar)) +
        geom_sf(color = "white", size = 0.1) +
        scale_fill_fermenter(
            palette = pal,
            limits = lim,
            breaks = brk,
            labels = scales::percent_format(accuracy = 1, suffix = ut),
            direction = dir) +
        ggthemes::theme_map()

    if (facet_race)
        gg <- gg + facet_wrap(~ race, nrow = 1)

    return(gg)
}

#' Make map of levels
gg_cd_make <- function(tbl = left_join(cd_shp, cc_cd, by = c("cd")),
                       race_regex = c("White|Hispanic|Black"),
                       legend_tit = "Trump's Two-Party Vote",
                       legend_pos = c(0.25, -0.2),
                       legend_keyheight = 0.5,
                       strip_size = 13) {

    gg_cd <- cd_shp |>
        left_join(cc_cd, by = c("cd")) |>
        filter(str_detect(race, race_regex)) |>
        ggplot(aes(fill = p_mrp_twway)) +
        facet_wrap(~ race, nrow = 1) +
        geom_sf(color = "white", size = 0.1) +
        scale_fill_fermenter(
            palette = "PiYG",
            limits = c(0, 1),
            breaks = seq(0.10, 0.90, by = 0.10),
            labels = scales::percent_format(accuracy = 1),
            direction = -1) +
        ggthemes::theme_map()


    gg_cd +
        labs(fill = legend_tit) +
        theme(legend.position = legend_pos,
              legend.direction = "horizontal",
              legend.key.width = unit(0.5, "in"),
              legend.key.height = unit(legend_keyheight, "in"),
              strip.text = element_text(
                  size = strip_size,
                  face = "bold",
                  margin = margin(l = -10)),
              strip.background = element_blank())
}



# Create datasets -----
rpv_df <- est_long |>
    mutate(race = str_remove_all(race, " Voters")) |>
    filter(race %in% c("White", "non-White"),
           level == "cd") |>
    pivot_wider(id_cols = c(year, cd),
                values_from = p_mrp_twway, names_from = race) |>
    janitor::clean_names() |>
    mutate(rpv = white - non_white)

rpv_race <- est_long |>
    mutate(race = str_remove_all(race, " Voters")) |>
    filter(race %in% c("White", "Black", "Hispanic"),
           level == "cd") |>
    pivot_wider(id_cols = c(year, cd),
                values_from = p_mrp_twway, names_from = race) |>
    janitor::clean_names() |>
    mutate(rpv_wb = white - black,
           rpv_wh = white - hispanic)

# Findings ---
## CD diff by race
gg_gap <- cd_shp |>
    left_join(rpv_df, by = "cd") |>
    filter(year == 2016) |>
    gg_rv(fillvar = rpv,
          facet_race = FALSE,
          brk = sort(c(0.05, seq(0.10, 0.60, by = 0.10), 0.65)),
          lim = c(0, 0.7),
          ut = "pp",
          pal = "YlOrRd",
          dir = 1) +
    geom_sf(data = st_shp, fill = "transparent", size = 0.1) +
    labs(fill = "Racial Gap in Trump vote") +
    theme(legend.position = c(0.74, 0.05))

## Levels, not differences
cc_cd <- est_long |>
    filter(year == 2016, level == "cd") |>
    filter(str_detect(race, "(Black|Hispanic|^White)"))


# Save ----

#' Saves Figure 3a
ggsave("figures/rpv_cd.pdf", gg_gap, w = 5.8, h = 4.2)

#' Saves Figure 4
ggsave("figures/2016-vote-by-race_cd.pdf",
       plot = gg_cd_make(),
       w = 10, h = 3.5)
