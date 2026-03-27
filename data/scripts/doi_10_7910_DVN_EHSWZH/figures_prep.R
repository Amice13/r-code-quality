#################################################################################
### load needed packages
#################################################################################

library(readxl)
library(tigris)
library(sf)
library(tidyverse)
library(ggsn)
library(cowplot)
library(ggthemes)
library(ggrepel)
library(ggnewscale)
library(biscale)

#################################################################################
### define functions
#################################################################################

## polygon from bounding box
bbox_poly <- function(x) {sf::st_as_sf(sf::st_as_sfc(x))}

## set CRS to UTM zone 11
utm11 <- function(x) {sf::st_transform(x, 32611)}

## add Black/Latino and Asian/White axes to plots
axes <- function(
          gg, # ggplot object to add axes to
          h = TRUE, # add horizontal axes?
          v = TRUE, # add vertical axes?
          ls = 0.2, # line size
          lt_minor = "solid", # line type for within-axis lines
          ls_mult = 3L # ratio btwn major/minor line sizes
) {
     if (v) {
          gg <- gg +
               geom_vline(xintercept = 12.5, linetype = "solid",
                          size = ls * ls_mult) +
               geom_vline(xintercept = 11.5, linetype = lt_minor, size = ls) +
               geom_vline(xintercept = 5.5, linetype = "solid",
                          size = ls * ls_mult) +
               geom_vline(xintercept = 1.5, linetype = lt_minor, size = ls) +
               geom_vline(xintercept = 0.5, linetype = "solid",
                          size = ls * ls_mult)
     }
     if (h) {
          gg <- gg +
               geom_hline(yintercept = 12.5, linetype = "solid",
                          size = ls * ls_mult) +
               geom_hline(yintercept = 11.5, linetype = lt_minor, size = ls) +
               geom_hline(yintercept = 7.5, linetype = "solid",
                          size = ls * ls_mult) +
               geom_hline(yintercept = 1.5, linetype = lt_minor, size = ls) +
               geom_hline(yintercept = 0.5, linetype = "solid",
                          size = ls * ls_mult)
     }
     return(gg)
}

## manually change locations of points in an sf object
update_point <- function(
          sf_obj, # sf spatial object containing points
          id_var, # character string of ID variable
          id_list, # list of IDs for observations that will be manually changed
          new_point_list # list of new values for the points
) {
     if (!identical(length(id_list), length(new_point_list))) {
          stop("Unequal lengths of `id_list` and `new_point_list.`")
     } 
     for (i in 1:length(id_list)) {
          sf::st_geometry(sf_obj[sf_obj[[id_var]] == id_list[[i]], ]) <- 
               sf::st_sfc(sf::st_point(new_point_list[[i]]))
     }
     return(sf_obj)
}

## base function for making choropleth maps of LA Metro
plot_metro <- function(
          sdf, # sf spatial object containing variable to be mapped
          ramp_var, # name of the variable to be mapped
          color_scale, # geom or name of the color scale to be used
          spec_list, # list containing scales, colors, and other specifications
          fig_num = NULL, # deprecated; kept for compatibility w/ old versions
          city_names_layer = NULL, # geom with city names to map, if any
          city_colors_layer = NULL # geom with colors for city name text, if any
) {
     # X/Y anchor points for compass
     cx <- 368672.2
     cy <- 3725489
     # X/Y anchor points for distance scale bar
     sx <- 385249.5
     sy <- 3716924
     # see `s` list definitions starting at line 294 for info on layers/geoms
     g <- ggplot() +
          geom_sf(data = st_union(s$pacific),
                  mapping = aes(fill = "Pacific Ocean"),
                  color = spec_list$ocean) +
          scale_fill_manual(values = spec_list$ocean,
                            guide = guide_legend(title = element_blank(),
                                                 order = 6)) +
          new_scale_fill() +
          geom_sf(data = s$ca, color = spec_list$land,
                  mapping = aes(
                       fill = "Non-urbanized, outside\nmetro, or missing data")
          ) +
          scale_fill_manual(values = spec_list$land,
                            guide = guide_legend(title = element_blank(),
                                                 order = 5)) +
          geom_sf(data = s$extent, fill = spec_list$land,
                  color = spec_list$land) +
          new_scale_color() +
          new_scale_fill() +
          geom_sf(data = sdf,
                  mapping = aes_string(fill = ramp_var, color = ramp_var)) +
          color_scale +
          new_scale_color() +
          geom_sf(data = s$la, fill = NA,
                  mapping = aes(color = "LA City boundaries"), size = 0.3,
                  key_glyph = draw_key_vline) +
          scale_color_manual(values = spec_list$la,
                             guide = guide_legend(
                                  override.aes = list(size = 0.6),
                                  title = element_blank(),
                                  order = 3)) +
          new_scale_color() +
          geom_sf(data = s$regions, key_glyph = draw_key_vline, fill = NA,
                  mapping = aes(color = "Region boundaries"), size = 1/7,
                  linetype = 3) +
          scale_color_manual(values = spec_list$region,
                             guide = guide_legend(title = element_blank(),
                                                  override.aes =
                                                       list(size = 0.3),
                                                  order = 4)) +
          new_scale_color() +
          geom_sf(data = s$cnty, mapping = aes(color = "County boundaries"),
                  fill = NA, size = 0.6, key_glyph = draw_key_vline) +
          scale_color_manual(values = spec_list$cnty,
                             guide = guide_legend(title = element_blank(),
                                                  order = 2,
                                                  override.aes =
                                                       list(size = 0.9))) +
          geom_sf(data = bbox_poly(s$bb), color = spec_list$bg, fill = NA,
                  size = 0.8) +
          (if (!is.null(city_colors_layer)) {new_scale_color()} else {NULL}) +
          city_names_layer +
          city_colors_layer +
          geom_sf_text(data = s$regions_point, mapping = aes(label = name3),
                       color = spec_list$region_lab,
                       fontface = "bold", size = 4/3, lineheight = 0.75) +
          scalebar(x.min = s$bb$xmin, x.max = s$bb$xmax,
                   y.min = s$bb$ymin, y.max = s$bb$ymax,
                   dist = 5, dist_unit = "mi", transform = FALSE,
                   anchor = c(x = sx, y = sy), height = 0.015,
                   st.size = 1.5) +
          north(x.min = s$bb$xmin, x.max = s$bb$xmax,
                y.min = s$bb$ymin, y.max = s$bb$ymax,
                anchor = c(x = cx, y = cy),
                scale = 0.125, symbol = 16) +
          theme_map() +
          theme(legend.direction = "horizontal",
                legend.background = element_rect(fill = spec_list$legbox,
                                                 color = spec_list$legbox),
                legend.box.background = element_rect(fill = spec_list$legbox,
                                                     color = NA),
                legend.key = element_rect(fill = spec_list$legbox,
                                          color = spec_list$legbox),
                legend.key.height = unit(0.125, "in"),
                legend.key.width = unit(0.125, "in"),
                legend.spacing.y = unit(0.01, "in"),
                legend.text = element_text(size = 4.5),
                legend.title = element_text(size = 5.5))
     return(g)
}

## base function for plotting panels of Figure 4
plot_f4 <- function(
          .data, # data frame containing exposure/isolation data
          x_var = "lab2", # name of x variable
          y_var = "lab1", # name of y variable
          fill_var = "z", # name of variable to visualize
          diag_var = "d", # name of variable indicating location on diagonal
          leg_pos = "none", # position of legend
          fill_lab = NULL, # text label for fill_var
          fill_guide = NULL # colorbar guide info for fill_var
) {
     x <- .data[[x_var]]
     y <- factor(.data[[y_var]], levels = rev(levels(x)))
     f <- .data[[fill_var]]
     d <- .data[[diag_var]]
     gg1 <- ggplot() +
          geom_tile(aes(x, y, fill = f, color = f)) +
          coord_equal() +
          scale_fill_viridis_b(option = "cividis", 
                               limits = c(-3, 3), breaks = c(-2, -1, 1, 2),
                               guide = fill_guide, 
                               aesthetics = c("fill", "color")) +
          theme_minimal() +
          theme(legend.position = leg_pos,
                legend.direction = "horizontal",
                legend.text = element_text(size = 5.75),
                legend.title = element_text(size = 6.5),
                axis.text.x = element_text(
                     angle = 90, vjust = 0.5, hjust = 1, size = 5.5
                ),
                axis.text.y = element_text(size = 5.5),
                axis.ticks = element_blank(),
                axis.title = element_text(size = 6),
                panel.grid = element_blank()) +
          labs(fill = fill_lab, color = fill_lab,
               y = "Exposure of", x = "Exposure to", title = "")
     gg2 <- axes(gg1, ls = 0.25, ls_mult = 2L) +
          new_scale_color() +
          geom_tile(aes(x, y, color = d), fill = NA, 
                    linetype = "dotted", size = 0.175) +
          scale_color_manual(values = c("transparent", "white"), guide = NULL)
     return(gg2)
}

## prep segregation data for use in `plot_f4` function
exp_iso_prep <- function(.data) {
     .data %>%
          right_join(d$cw_lab, by = c("g1" = "abb")) %>%
          right_join(d$cw_lab, by = c("g2" = "abb")) %>%
          select(g1, lab1 = lab.x, g2, lab2 = lab.y, z) %>%
          mutate(across(starts_with("lab"),
                        ~ factor(., levels = levels(d$cw_lab$lab))),
                 d = if_else(lab1 == lab2, TRUE, FALSE)) %>%
          arrange(lab1, lab2)
}

### year for Census Bureau data
yr <- 2019L

#################################################################################
### non-spatial data
#################################################################################

## list containing all non-spatial data objects
d <- list()

## full group crosswalk from glossaries workbook
d$cw_id <- read_excel("data/glossaries.xlsx", sheet = "Groups (Full)", 
                      col_types = c("text", "skip", "text", "text")) %>%
     filter(year == "2015-19") %>%
     distinct() %>%
     dplyr::select(-year)

## group crosswalk for main analysis groups only from glossaries workbook
d$cw_lab <- read_excel("data/glossaries.xlsx", 
                       sheet = "Groups (Segregation Analyses)", 
                       col_types = c("text", "text")) %>%
     mutate(across(.fns = ~ factor(., levels = .)))

## data containing SES neighborhood quartiles by group
d$ses_group <- read_csv("data/clean/ses_quartiles.csv", col_types = "ff_d") %>%
     right_join(d$cw_lab)

## data containing SES index by tract
d$ses_tract <- read_csv("data/clean/ses_tract.csv", col_types = "cd")

## get tract-level group data
source("scripts/get_groups_tract.R")

## filter tract-level group data for LA Metro, 2015-19 data
d$long <- ct %>%
     filter(cbsa_name == "Los Angeles CA" & year == "2015-19") %>%
     select(-c(year, cbsa_name))

## segregation indices
d$ind_raw <- read_csv("data/clean/segregation_indices.csv", 
                      col_types = "ccccddd") %>%
     drop_na(value)

## exposure/isolation index data for all metros
d$exp_iso <- d$ind_raw %>%
     filter(index == "exposure") %>%
     filter(cbsa_name == "Los Angeles CA") %>%
     exp_iso_prep()

## exposure/isolation index data for immigrant gateways only
# laod immigrant gateway crosswalk
d$exp_iso_ig <- read_csv("data/clean/gateways.csv", col_types = "fc__l") %>%
     inner_join(d$ind_raw) %>%
     filter(imm_gate == TRUE & year == "2015-19" & index == "exposure") %>%
     group_by(g1, g2) %>%
     mutate(z = as.numeric(scale(value))) %>%
     ungroup() %>%
     filter(cbsa_name == "Los Angeles CA") %>%
     exp_iso_prep()

rm(ct)
gc()

#################################################################################
### spatial data (map layers)
#################################################################################

## list containing all spatial data objects
s <- list()

## LA Urbanized Area
s$ua <- urban_areas(year = yr) %>%
     filter(GEOID10 == "51445") %>%
     select(geometry) %>%
     utm11()

## LA Metro
s$msa <- core_based_statistical_areas(year = yr) %>%
     filter(GEOID == "31080") %>%
     select(geometry) %>%
     utm11()

## extent of metro-level maps
# map parts of LA Metro in LA Urbanized Area only
s$extent <- st_intersection(s$msa, s$ua) %>%
     st_collection_extract("POLYGON")
## boundary box for metro-level maps
s$bb <- st_bbox(s$extent)

## LA neighbrohood polygons from Mapping LA Project
s$nbhd <- st_read("data/raw/la-county-neighborhoods-v6.geojson") %>%
     utm11() %>%
     mutate(name = case_when(
          name == "East Los Angeles" ~ "East LA",
          name == "Downtown" ~ "Downtown LA",
          TRUE ~ name))

## city names selected to map
s$cities <- suppressWarnings(st_centroid(s$nbhd)) %>%
     filter(name %in% c("Anaheim", "Santa Ana", "Glendale", "Pasadena",
                        "El Monte", "Pomona", "Long Beach", "Beverly Hills",
                        "Burbank", "Encino", "Malibu", "Santa Monica",
                        "Downtown LA", "Van Nuys", "Pacoima", "East LA",
                        "Culver City", "Inglewood", "Compton", "Hollywood",
                        "Torrance", "Monterey Park", "Carson", "Hawthorne",
                        "Whittier", "Downey", "Norwalk", "Ladera Heights",
                        "Baldwin Hills/Crenshaw", "Historic South-Central",
                        "Arcadia", "West Covina", "Irvine", "South Gate",
                        "Chinatown", "Alhambra", "Rowland Heights",
                        "West Covina", "Cerritos", "Diamond Bar",  "Seal Beach",
                        "Altadena", "Wilmington",  "Mid-City", "Fullerton",
                        "Yorba Linda", "Huntington Beach")) %>%
     mutate(name2 = case_when(
          name == "Ladera Heights" ~ " \n \n Ladera\nHts.",
          name == "Historic South-Central" ~ 
               " \nHistoric\nSouth\nCentral",
          name == "Baldwin Hills/Crenshaw" ~ "     Cren-\n     shaw",
          name == "Alhambra" ~ "     Alhambra",
          name == "Downtown LA" ~ "Down-\ntown",
          name == "Chinatown" ~ "China-\ntown",
          name == "Hollywood" ~ "Holly-\nwood",
          name == "Inglewood" ~ "Ingle-\nwood",
          name == "Long Beach" ~ " \n \n \n Long\nBeach",
          name %in% c("Downey", "Pomona", "Glendale", "Irvine", "East LA") ~
               paste0(" \n ", name),
          name %in% c("Torrance", "Pasadena") ~
               paste0(" \n \n ", name),
          name == "Malibu" ~ " \n \n \n Malibu",
          name %in% c("Santa Monica", "Beverly Hills", "West Covina", 
                      "South Gate", "Hacienda Heights", "Rowland Heights", 
                      "Yorba Linda", "West Covina", "Seal Beach",
                      "Monterey Park", "Diamond Bar", "Culver City",
                      "Huntington Beach") ~
               str_replace_all(name, " ", "\n"),
          TRUE ~ name))

## LA regions polygons from Mapping LA Project
s$regions <- st_read("data/raw/la-county-regions-v6.geojson") %>%
     utm11() %>%
     st_crop(s$bb)

## region names selected to map
s$regions_point <- suppressWarnings(st_centroid(s$regions)) %>%
     mutate(name2 = str_remove_all(name, "\\."),
            name3 = case_when(
                 substr(name, 1, 4) == "San " ~ name,
                 name == "Northwest County" ~ "",
                 name == "North County" ~ "North Orange County",
                 name == "South County" ~ "South\nOrange\nCounty",
                 name == "Eastside" ~ "East-\nside",
                 name == "Westside" ~ "West-\nside",
                 name == "Beach Cities" ~ "Beach\nCities",
                 name == "Pomona Valley" ~ "Pomona\nValley",
                 name == "South Bay" ~ "South\nBay",
                 name == "Northeast L.A." ~ "NE\nLA",
                 name == "South L.A." ~ "South\nLA",
                 TRUE ~ name2)) %>%
     update_point("name",
                  c("South Bay", "Southeast", "San Gabriel Valley",
                    "Central L.A.", "Westside", "Verdugos",
                    "South County"), 
                  list(c(375125, 3749002),
                       c(397000, 3759143),
                       c(417577, 3774995),
                       c(378899, 3769980),
                       c(365545.669112131, 3769475.23272904),
                       c(387320, 3780303),
                       c(431336.933671128, 3732034)))

## LA City boundaries
s$la <- places("CA", year = yr) %>%
     filter(GEOID == "0644000") %>%
     select(geometry) %>%
     utm11() %>%
     st_intersection(s$extent) %>%
     st_collection_extract("POLYGON")

## bodies of water
s$water <- map_dfr(c("Orange", "Los Angeles"),
                   ~ area_water(state = "CA", county = ., year = yr)) %>%
     select(mtfcc = MTFCC, geometry) %>%
     utm11() %>%
     st_crop(s$bb)

## Pacific Ocean
s$pacific <- "data/raw/PC_PROTLMT.shp"
# unzip shapefile if necessary
if (!file.exists(s$pacific)) {
     unzip(str_replace(s$pacific, "shp", "zip"), exdir = "data/raw")
     walk(list.files("data/raw/data/raw", full.names = TRUE), 
          ~ file.copy(., "data/raw"))
     unlink("data/raw/data", TRUE)
}
s$pacific <- st_read(s$pacific) %>%
     utm11() %>%
     st_union() %>%
     st_crop(s$bb)

## CA land boundaries
s$ca <- states(year = yr) %>%
     select(geometry) %>%
     utm11() %>%
     st_crop(s$bb) %>%
     st_difference(st_union(filter(s$water, mtfcc == "H2053")))

## LA County
s$cnty <- counties("CA", year = yr) %>%
     utm11() %>%
     st_crop(s$bb) %>%
     st_intersection(s$ca)

## 2010 census tracts
s$ct <- tracts("CA", c("Los Angeles", "Orange"), FALSE, yr) %>%
     select(ct_id = GEOID, geometry) %>%
     utm11() %>%
     st_intersection(s$extent) %>%
     st_collection_extract("POLYGON") %>%
     st_crop(s$bb) %>%
     st_collection_extract("POLYGON") 

## population centers from LISA Analysis
s$pc <- read_csv("data/clean/population_centers.csv", col_types = "cllll") %>%
     pivot_longer(latino:white) %>%
     mutate(name = str_to_title(name),
            panel = if_else(name %in% c("Latino", "Black"), 
                            "Black/Latino",
                            "Asian/White")) %>%
     filter(value == TRUE) %>%
     distinct(ct_id, name, value, .keep_all = TRUE) %>%
     group_by(ct_id, panel) %>%
     summarize(pop_cen = factor(paste0(sort(name), collapse = "/"),
                                levels = c("Black", "Black/Latino", "Latino",
                                           "Asian", "Asian/White",  "White")
     )) %>%
     right_join(expand_grid(ct_id = sort(unique(s$ct$ct_id)),
                            panel = c("Asian/White", "Black/Latino"))) %>%
     left_join(s$ct) %>%
     st_as_sf() %>%
     mutate(panel = factor(panel,
                           levels = c("Black/Latino", "Asian/White"))) %>%
     arrange(ct_id, panel)

## combine city/region/county boundaries
s$boundaries <- map2_dfr(
     c("la", "regions", "cnty"),
     c("LA City\nboundaries", "Region\nboundaries", "County\nboundaries"),
     ~ mutate(select(s[[.x]], geometry), lyr = .y)) %>%
     mutate(lyr = factor(lyr, levels = c("LA City\nboundaries",
                                         "Region\nboundaries",
                                         "County\nboundaries"))) %>%
     st_cast("MULTILINESTRING") %>%
     st_difference(st_cast(bbox_poly(s$bb), "LINESTRING")) 

## freeways
s$hwy <- primary_roads(yr) %>%
     utm11() %>%
     st_crop(s$bb) 

gc()