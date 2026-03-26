library(tidyverse)
library(donnermap)
library(sf)
library(patchwork)

cc_df <- read_csv("data/mrp-ests_by-cd-race_asian.csv") |>
  mutate(st = str_sub(cd, 1, 2))


# sub -----
tbl_sub <- cc_df |>
  filter(
    st %in% c("NJ", "CA", "HI", "WA"),
    race %in% c("White", "Asian", "Hispanic", "Black", "All"),
    year == 2016) |>
  mutate(race = fct_relevel(race, "White", "Asian", "Black", "Hispanic")) |>
  group_by(cd) |>
  mutate(frac = N / sum(N)) |>
  ungroup()
#
# wide_R <- tbl_sub |>
#     pivot_wider(id_cols = cd, names_from = race, values_from = p_mrp_est, names_sort = TRUE, names_prefix = "R_")
cds_aapi <- tbl_sub |>
  pivot_wider(id_cols = cd, names_from = race, values_from = frac, names_sort = TRUE, names_prefix = "N_") |>
  filter(N_Asian > 0.10) |>
  pull(cd)

map_aapi_st <- function(races, data = cc_df,
                        states = c("NJ", "CA", "HI", "WA"),
                        color_cds = cds_aapi,
                        shp = cd_shp) {

  cc_sel <- filter(data, st %in% states, race %in% races)

  cc_shp <- shp |>
    inner_join(cc_sel, by = "cd") |>
    mutate(p_mrp_ggfix = replace(p_mrp_ggfix, !cd %in% color_cds, NA_real_))

  gg_all <- ggplot(cc_shp) +
    geom_sf(color = "gray90", aes(fill = p_mrp_ggfix), size = 0.1) +
    scale_fill_fermenter(
      palette = "PiYG",
      limits = c(0, 1),
      breaks = seq(0.10, 0.90, by = 0.10),
      labels = scales::percent_format(accuracy = 1, suffix = "%"),
      na.value = "white",
      direction = -1) +
    ggthemes::theme_map()

  gg_west <- gg_all %+% filter(cc_shp, st %in% c("WA", "CA", "HI")) +
    labs(title = races) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  gg_east <- gg_all %+% filter(cc_shp, st %in% c("NJ")) +
    guides(fill = FALSE)

  gg_west +
    inset_element(gg_east, left = 0.48, right = 0.75, bottom = 0.4, top = 1)
}

# Save to Figure B01
map_aapi_st("White") +
  map_aapi_st("Black") +
  map_aapi_st("Hispanic") +
  map_aapi_st("Asian") +
  plot_layout(nrow = 1, guides = "collect") &
  theme(legend.position = "bottom",
        legend.key.width = unit(0.5, "in")) &
  labs(fill = "Trump Vote\nwithin Race")

