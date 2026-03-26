#################################################################################
### load data & functions for making figures
#################################################################################

source("scripts/figures_prep.R")

#################################################################################
### Figure 1
#################################################################################

fig1 <- axes(
     ggplot() +
          geom_bar(
               data = d$ses_group,
               mapping = aes(
                    fill = factor(-as.integer(ses4)),
                    y = lab,
                    x = -pr_grp_est
               ),
               size = 2,
               position = "fill",
               stat = "identity"
          ) +
          scale_y_discrete(limits = rev) +
          scale_x_continuous(
               breaks = seq(0,-1,-0.25),
               labels = c(100, 75, 50, 25, 0)
          ) +
          scale_fill_viridis_d(
               option = "rocket",
               labels = c("Bottom", "Third", "Second", "Top")
          ) +
          theme_minimal() +
          theme(
               legend.position = "bottom",
               legend.direction = "horizontal",
               legend.text = element_text(size = 5.75),
               legend.title = element_text(size = 6.5),
               axis.text = element_text(size = 5.75),
               axis.title = element_text(size = 6),
               panel.grid = element_blank()
          ) +
          guides(
               fill = guide_legend(
                    keywidth = unit(0.5, "in"),
                    title.position = "top",
                    label.position = "bottom"
               )
          ) +
          labs(fill = "Neighborhood socioeconomic quartile",
               y = NULL, x = "Percent of group population"),
     v = FALSE
)

fig1

#################################################################################
### Figure 2
#################################################################################

fig2 <- list()

fig2$df <- s$ct %>%
     left_join(d$ses_tract) %>%
     drop_na(ses)

fig2$spec <- list(
     ramp = "rocket",
     ocean = "lightblue1",
     land = "gray40",
     la = "gray60",
     region = "gray94",
     region_lab = "gray94",
     bg = "white",
     legbox = "gray78",
     cnty = "gray100",
     city = c("gray23", "gray88"),
     limits = c(min(fig2$df$ses) - 0.02, max(fig2$df$ses) + 0.02),
     ticks = quantile(fig2$df$ses, seq(0, 1, 1 / 4), na.rm = TRUE)
)

fig2$cities <- s$cities %>%
     mutate(color = if_else(
          name %in% c("Encino", "Glendale", "Culver City",
                      "Malibu", "Beverly Hills", "Yorba Linda"),
          "dark",
          "light"
     ))

fig2$fig <- plot_metro(
     sdf = fig2$df,
     ramp_var = "ses",
     fig_num = 2,
     color_scale = scale_color_viridis_c(
          option = fig2$spec$ramp,
          direction = -1,
          aesthetics = c("fill", "color"),
          guide = guide_colorbar(
               order = 1,
               reverse = TRUE,
               barwidth = 5,
               barheight = 0.5,
               title.position = "top",
               title.hjust = 0.5,
               title = 
                    "Socioeconomic composition\n(percentiles, low to high SES)"
          ),
          breaks = fig2$spec$ticks,
          labels = rev(seq(0, 100, 25)),
          limits = fig2$spec$limits
     ),
     city_names_layer = geom_sf_text(
          data = fig2$cities,
          mapping = aes(label = toupper(name2), color = color),
          show.legend = FALSE,
          size = 0.8,
          lineheight = 2 / 3,
          fontface = "bold"
     ),
     city_colors_layer = scale_color_manual(values = fig2$spec$city),
     spec_list = fig2$spec
)

fig2$fig

#################################################################################
### Figure 3
#################################################################################

fig3 <- list()

fig3$spec <- list(
     fills = c("#F0F921FF", "#FCA338FF", "#DE6065FF",
               "#0D0887FF", "#6E00A8FF", "#B52F8CFF"),
     ocean = fig2$spec$ocean,
     land = fig2$spec$land,
     boundaries = "gray63",
     la = "gray52",
     text = fig2$spec$region,
     bg = fig2$spec$bg
)

fig3$fig <- ggplot() +
     geom_sf(
          data = st_union(s$pacific),
          mapping = aes(color = "Pacific\nOcean"),
          fill = fig3$spec$ocean
     ) +
     scale_color_manual(values = fig3$spec$ocean,
                        guide = guide_legend(order = 3, title = NULL)) +
     geom_sf(
          data = s$ca,
          color = fig3$spec$land,
          fill = fig3$spec$land
     ) +
     geom_sf(
          data = st_union(s$extent),
          color = fig3$spec$land,
          fill = fig3$spec$land
     ) +
     new_scale_fill() +
     new_scale_color() +
     geom_sf(data = s$pc,
             mapping = aes(color = pop_cen, fill = pop_cen)) +
     scale_fill_manual(
          values = fig3$spec$fills,
          na.value = fig3$spec$land,
          aesthetics = c("color", "fill"),
          guide = guide_legend(
               order = 1,
               title = "Population centers",
               label.position = "bottom",
               title.position = "top",
               byrow = TRUE,
               nrow = 2,
               keywidth = unit(0.35, "in")
          ),
          na.translate = FALSE,
          labels = c("Black", "Overlap", "Latino",
                     "Asian", "Overlap", "White")
     ) +
     geom_sf(
          data = s$boundaries,
          color = fig3$spec$boundaries,
          key_glyph = draw_key_vline,
          mapping = aes(size = lyr, linetype = lyr)
     ) +
     scale_linetype_manual(
          values = c("solid", "dotted", "solid"),
          guide =
               guide_legend(
                    order = 2,
                    title = NULL,
                    byrow = TRUE,
                    nrow = 3,
                    override.aes = list(size = c(0.8, 0.4, 1.6))
               )
     ) +
     scale_size_manual(values = c(0.275, 0.125, 0.525), guide = "none") +
     geom_sf_text(
          data = s$regions_point,
          mapping = aes(label = name3),
          color = fig3$spec$text,
          fontface = "bold",
          size = 1.25,
          lineheight = 2 / 3
     ) +
     geom_sf_text(
          data = s$cities,
          mapping = aes(label = toupper(name2)),
          show.legend = FALSE,
          size = 0.8,
          fontface = "bold",
          color = fig3$spec$text,
          lineheight = 0.6
     ) +
     facet_wrap( ~ panel, nrow = 2, strip.position = "top") +
     theme_map() +
     theme(
          strip.background = element_blank(),
          strip.text = element_text(
               angle = 0,
               size = 7,
               face = "bold",
               margin = margin(b = 0, t = 0.1, unit = "in")
          ),
          panel.spacing.y = unit(-0.1, "in"),
          panel.spacing.x = unit(-0.2, "in"),
          legend.position = "bottom",
          legend.justification = "center",
          legend.title.align = 0.5,
          legend.key.height = unit(0.15, "in"),
          legend.key.width = unit(0.15, "in"),
          legend.text = element_text(size = 4.75),
          legend.title = element_text(size = 5.75),
          legend.key = element_rect(fill = fig3$spec$land, color = "white")
     ) +
     scalebar(
          data = s$pc,
          location = "bottomleft",
          dist = 5,
          dist_unit = "mi",
          transform = FALSE,
          height = 0.015,
          st.size = 1.5,
          facet.var = "panel",
          facet.lev = "Asian/White"
     )

fig3$fig

#################################################################################
### Figure 4
#################################################################################

fig4 <- list()

fig4$exp_imm <- plot_f4(d$exp_iso_ig)

fig4$exp_met <- plot_f4(d$exp_iso)

fig4$leg <- get_legend(
     plot_f4(
          d$exp_iso,
          fill_guide = guide_colorbar(
               barwidth = 10,
               barheight = 0.75,
               hjust = 0.5,
               title.position = "top",
               title.hjust = 0.5,
               axis.ticks = NULL
          ),
          fill_lab = "Spatial exposure\n(standard deviations from mean)",
          leg_pos = "bottom"
     )
)

fig4$fig <- plot_grid(
     fig4$exp_met,
     fig4$exp_imm,
     fig4$leg,
     nrow = 3,
     labels = c("Compared to all US metropolitan areas",
                "  Compared to 50 immigrant gateways  ",
                NULL),
     label_size = 7,
     label_y = 0.975,
     hjust = c(-2 / 3, -2 / 3, NULL),
     rel_heights = c(10, 10, 2),
     align = "hv",
     axis = "lrtb"
)

fig4$fig

#################################################################################
### Figure 6
#################################################################################

fig6 <- list()

fig6$bb <- s$nbhd %>%
     filter(
          str_detect(metadata, "south-la") |
               name %in% c("Compton", "Ladera Heights", "Downton", "Pico-Union")
     ) %>%
     st_bbox()

fig6$bbp <- bbox_poly(fig6$bb)

fig6$focus <- s$nbhd %>%
     filter(str_detect(metadata, "south-la"))

fig6$bg <- st_crop(s$nbhd, fig6$bb) %>%
     mutate(
          name2 = case_when(
               name %in% c(
                    "Palms",
                    "Redondo Beach",
                    "Hermosa Beach",
                    "Rancho Dominguez",
                    "Pico-Robertson",
                    "Beverlywood"
               ) ~ "",
               name == "Athens" ~ " \n \nAthens",
               name == "East LA" ~ " \n \nEast LA",
               name %in% c(
                    "South Park",
                    "Baldwin Hills/Crenshaw",
                    "Pico-Union",
                    "Huntington Park",
                    "West Adams",
                    "Vermont Square",
                    "Mid-City",
                    "Long Beach",
                    "Hyde Park",
                    "Grammercy Park",
                    "Walnut Park"
               ) ~ name,
               str_detect(name, "\\/") ~ str_replace_all(name, "\\/", "\\/\n"),
               str_detect(name, "-") ~ str_replace_all(name, "-", "-\n"),
               str_detect(name, " ") ~ str_replace_all(name, " ", "\n"),
               TRUE ~ name
          )
     )

fig6$hwy <- st_crop(s$hwy, fig6$bb)

fig6$borders <- "gray85"
fig6$shade <- "gray95"
fig6$lab <- "gray10"
fig6$hwy_clr <- c("gray60", "gray100")

fig6$fig <- ggplot() +
     geom_sf(
          data = fig6$focus,
          color = fig6$borders,
          size = 0.3,
          mapping = aes(fill = "South LA communities")
     ) +
     scale_fill_manual(values = fig6$shade,
                       guide = guide_legend(title = NULL, order = 1)) +
     geom_sf(
          data = st_crop(s$la, fig6$bb),
          mapping = aes(linetype = "LA City boundaries"),
          key_glyph = draw_key_vline,
          fill = NA,
          color = fig6$lab,
          size = 0.1
     ) +
     scale_linetype_manual(values = "solid",
                           guide = guide_legend(
                                title = NULL,
                                order = 2,
                                override.aes =
                                     list(size = 0.2)
                           )) +
     geom_sf(
          data = fig6$bbp,
          fill = NA,
          color = "white",
          size = 0.25
     ) +
     geom_sf(
          data = fig6$hwy,
          size = 0.5,
          mapping = aes(color = "Interstate highways"),
          key_glyph = draw_key_vline
     ) +
     scale_color_manual(
          values = fig6$hwy_clr[[1]],
          guide = guide_legend(
               title = NULL,
               order = 3,
               override.aes = list(size = 1)
          )
     ) +
     geom_text_repel(
          data = fig6$bg,
          mapping = aes(label = name2, geometry = geometry),
          size = 1,
          fontface = "bold",
          stat = "sf_coordinates",
          box.padding = unit(0, "in"),
          force_pull = 55,
          force = 0.01
     ) +
     scalebar(
          data = fig6$bg,
          location = "bottomright",
          dist = 2,
          dist_unit = "mi",
          transform = FALSE,
          height = 0.015,
          st.size = 1.5
     ) +
     north(
          data = fig6$bg,
          location = "topleft",
          symbol = 3,
          scale = 0.06
     ) +
     theme_map() +
     theme(
          legend.direction = "horizontal",
          legend.background = element_rect(fill = NA, color = NA),
          legend.box.background = element_rect(color = "gray97",
                                               fill = "white"),
          legend.key.height = unit(0.1, "in"),
          legend.key.width = unit(0.1, "in"),
          legend.spacing.y = unit(0.005, "in"),
          legend.text = element_text(size = 4.25),
          legend.title = element_text(size = 5.25)
     )

fig6$fig

#################################################################################
### Figure 8
#################################################################################

fig8 <- list()

fig8$bb <- s$nbhd %>%
     filter(str_detect(metadata, "san-gabriel-valley") |
                 name == "Chinatown") %>%
     st_bbox()

fig8$bbp <- bbox_poly(fig8$bb)

fig8$focus <- s$nbhd %>%
     filter(str_detect(metadata, "san-gabriel-valley"))

fig8$bg <- st_crop(s$nbhd, fig8$bb) %>%
     mutate(name2 = case_when(
          name %in% c("Walnut Park", "Bell Gardens", "Glassell Park",
                      "Mount Washington", "Elysian Valley", "Paramount",
                      "Huntington Park", "Bell Gardens", 
                      "La Canada Flintridge") ~ "",
          TRUE ~ name
     ))

fig8$hwy <- st_crop(s$hwy, fig8$bb)

fig8$borders <- "gray85"
fig8$shade <- "gray95"
fig8$lab <- "gray10"
fig8$hwy_clr <- c("gray60", "gray100")

fig8$fig <- ggplot() +
     geom_sf(
          data = fig8$focus,
          color = fig8$borders,
          size = 0.3,
          mapping = aes(fill = "SGV communities")
     ) +
     scale_fill_manual(values = fig8$shade,
                       guide = guide_legend(title = NULL, order = 1)) +
     geom_sf(
          data = st_crop(s$la, fig8$bb),
          mapping = aes(linetype = "LA City boundaries"),
          key_glyph = draw_key_vline,
          fill = NA,
          color = fig8$lab,
          size = 0.1
     ) +
     scale_linetype_manual(values = "solid",
                           guide = guide_legend(
                                title = NULL,
                                order = 2,
                                override.aes =
                                     list(size = 0.2)
                           )) +
     geom_sf(
          data = fig8$bbp,
          fill = NA,
          color = "white",
          size = 0.25
     ) +
     geom_sf(
          data = fig8$hwy,
          size = 0.5,
          mapping = aes(color = "Interstate highways"),
          key_glyph = draw_key_vline
     ) +
     scale_color_manual(
          values = fig8$hwy_clr[[1]],
          guide = guide_legend(
               title = NULL,
               order = 3,
               override.aes = list(size = 1)
          )
     ) +
     geom_text_repel(
          data = fig8$bg,
          mapping = aes(label = name2, geometry = geometry),
          size = 1,
          fontface = "bold",
          stat = "sf_coordinates",
          box.padding = unit(0, "in"),
          force_pull = 55,
          force = 0.01
     ) +
     scalebar(
          data = fig8$bg,
          location = "bottomright",
          dist = 2,
          dist_unit = "mi",
          transform = FALSE,
          height = 0.015,
          st.size = 1.5
     ) +
     north(
          data = fig8$bg,
          location = "topleft",
          symbol = 3,
          scale = 0.06
     ) +
     theme_map() +
     theme(
          legend.direction = "horizontal",
          legend.background = element_rect(fill = NA, color = NA),
          legend.box.background = element_rect(color = "gray97",
                                               fill = "white"),
          legend.key.height = unit(0.1, "in"),
          legend.key.width = unit(0.1, "in"),
          legend.spacing.y = unit(0.005, "in"),
          legend.text = element_text(size = 4.25),
          legend.title = element_text(size = 5.25)
     )

fig8$fig

#################################################################################
### Figure 7
#################################################################################

fig7 <- list()

fig7$spec <- fig2$spec
fig7$spec$ramp <- "mako"
fig7$spec$region <- "gray32"
fig7$spec$region_lab <- fig7$spec$city <- "gray18"
fig7$spec$la <- "gray60"

fig7$df <- d$long %>%
     filter(abb %in% c("pop", "fb024", "fb050", "fb103")) %>%
     pivot_wider(names_from = abb, values_from = est) %>%
     mutate(chinese = (fb024 + fb050 + fb103) / pop) %>%
     right_join(s$ct) %>%
     st_as_sf()

fig7$fig <- plot_metro(
     sdf = fig7$df,
     ramp_var = "chinese",
     spec_list = fig7$spec,
     fig_num = 7,
     color_scale = scale_color_viridis_c(
          option = fig7$spec$ramp,
          direction = -1,
          aesthetics = c("fill", "color"),
          guide = guide_colorbar(
               order = 1,
               barwidth = 5,
               barheight = 0.5,
               title.position = "top",
               title.hjust = 0.5,
               title = "Chinese immigrants"
          ),
          limits = c(0, 1),
          breaks = seq(0, 1, 0.25),
          labels = scales::percent_format(1L)
     ),
     city_names_layer = geom_sf_text(
          data = s$cities,
          mapping = aes(label = toupper(name2)),
          show.legend = FALSE,
          size = 0.8,
          lineheight = 2 / 3,
          fontface = "bold",
          color = fig7$spec$city
     )
)

fig7$fig

#################################################################################
### Figure 5
#################################################################################

fig5 <- list()

fig5$spec <- fig7$spec
fig5$spec$city <- c(fig5$spec$city, fig2$spec$city[[2]])

fig5$cities <- s$cities %>%
     mutate(color = if_else(
          name %in% c("Inglewood", "Ladera Heights", "Baldwin Hills/Crenshaw"),
          "light",
          "dark"
     ))

fig5$df <- d$long %>%
     filter(abb %in% c("pop", "nb_b")) %>%
     pivot_wider(names_from = abb, values_from = est) %>%
     mutate(black = nb_b / pop) %>%
     right_join(s$ct) %>%
     st_as_sf()

fig5$fig <- plot_metro(
     sdf = fig5$df,
     ramp_var = "black",
     spec_list = fig5$spec,
     fig_num = 5,
     color_scale = scale_color_viridis_c(
          option = fig5$spec$ramp,
          direction = -1,
          aesthetics = c("fill", "color"),
          guide = guide_colorbar(
               order = 1,
               barwidth = 5,
               barheight = 0.5,
               title.position = "top",
               title.hjust = 0.5,
               title = "Black, US-born"
          ),
          limits = c(-0.01, 1.01),
          breaks = seq(0, 1, 0.25),
          labels = scales::percent_format(1L)
     ),
     city_names_layer = geom_sf_text(
          data = fig5$cities,
          mapping = aes(label = toupper(name2), color = color),
          show.legend = FALSE,
          size = 0.8,
          lineheight = 2 / 3,
          fontface = "bold"
     ),
     city_colors_layer = scale_color_manual(values = fig5$spec$city)
)

fig5$fig