### descriptives: railway validation
library(here)
library(sf)
library(tidyverse)
library(cshapes)
library(nngeo)

sf_use_s2(FALSE)

### load segment data
rail.df <- readRDS(here("analysis_data","analysis_rails_df.rds"))
segments.ls <- readRDS(here("analysis_data","segments_sf_ls_tv.rds"))
rails.sf <- st_read(here("analysis_data", "RShapes.geojson"))
habsburg.sf <- here("analysis_data","habsburg_railways") %>% 
  list.files(full.names = T) %>% 
  map(st_read) %>% 
  map(~ .x[!st_is_empty(.x), ]) %>% 
  map(st_union) %>% 
  map(st_as_sf) %>% 
  map(st_make_valid) %>% 
  bind_rows() %>% 
  mutate(year = list.files(here("analysis_data","habsburg_railways")) %>% 
           str_extract("[0-9]{4}") %>% 
           as.numeric())

# Bind segments
segments.sf <- bind_rows(segments.ls)

# Find unique segment polygons
segments.unique.sf <- segments.sf %>% 
  group_by(groupname, name_uni, from, to) %>% 
  slice_head(n = 1)


# Create background
back.sf <- cshp() %>% 
  st_union() %>% 
  nngeo::st_remove_holes()

# Get AH polygon
ah.sf <- cshp() %>% 
  filter(country_name == "Austria-Hungary") %>% 
  st_union() %>% 
  nngeo::st_remove_holes()

vec_years <- habsburg.sf$year
list_out <- list()
for (i in c(1855, 1870, 1884, 1901)) {
  
  # Get RShapes
  rails.cum <- rails.sf %>% 
    filter(year <= i) %>% 
    st_union()  %>% 
    suppressMessages()
  rails.sub <- rails.cum %>% 
    st_intersection(ah.sf) %>% 
    suppressMessages()
  
  # Sample points
  rail.pts <- rails.sub %>% 
    st_sample(round(as.numeric(st_length(rails.sub)) / 1000, 0)) %>% 
    .[!st_is_empty(.)] %>% 
    suppressMessages()
  
  # Subset and clean habsburg
  rails.ah <- habsburg.sf %>% 
    filter(year == i) %>% 
    st_cast() %>% 
    .[!st_geometry_type(.) == "POINT"] %>% 
    st_union()
  
  # Create buffers
  buff.5 <- st_buffer(rails.ah, 0.06) %>% 
    suppressMessages() %>% 
    suppressWarnings()
  buff.10 <- st_buffer(rails.ah, 0.127) %>% 
    suppressMessages() %>% 
    suppressWarnings()
  
  # Compute length
  length.rs <- (as.numeric(st_length(rails.ah)) / 1000) %>% round(2) %>%
    as.numeric() %>%
    suppressMessages()
  length.af <- (st_length(rails.sub) %>% as.numeric() / 1000) %>% round(2) %>%
    as.numeric() %>%
    suppressMessages()
  
  # Find share of points within buffer
  share.5 <- st_intersects(rail.pts, buff.5) %>% map(length) %>% unlist %>% mean() %>% round(3) %>% 
    as.numeric() %>% 
    suppressMessages() %>% 
    suppressWarnings()
  share.10 <- st_intersects(rail.pts, buff.10) %>% map(length) %>% unlist %>% mean() %>% round(3) %>% 
    as.numeric() %>% 
    suppressMessages() %>% 
    suppressWarnings()
  
  # Compute distances
  rail.dists <- st_distance(rail.pts, rails.ah) %>%
    as.numeric() %>%
    suppressMessages()
  
  if (i %in% c(1855, 1870, 1884, 1901)) {
    
    # Make plots
    ggplot() +
      geom_sf(data = back.sf, fill = "gray75") +
      geom_sf(data = rails.cum, aes(col = "RShapes")) +
      geom_sf(data = ah.sf, fill = "gray90") +
      geom_sf(data = rails.sub, aes(col = "RShapes")) +
      geom_sf(data = rails.ah, aes(col = "Austro-Hungarian")) +
      scale_color_viridis_d(direction = -1, begin = 0.1, end = 0.65, option = "B") +
      labs(col = "", title = i,
           subtitle = paste0("Length RShapes (km): ", length.rs, 
                             "\nLength Austro-Hungarian (km): ", length.af)) +
      coord_sf(xlim = c(10, 30), ylim = c(42, 51)) +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            legend.position = "bottom",
            panel.background = element_rect(fill = "aliceblue"),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            title = element_text(hjust = 1))
    ggsave(here("figures", paste0("habsburg_comparison_", i,".pdf")),
           width = 5.5, height = 4)
    
    # Make plots
    ggplot() +
      geom_sf(data = back.sf, fill = "gray75") +
      # geom_sf(data = rails.cum, aes(col = "RShapes")) +
      geom_sf(data = ah.sf, fill = "gray90") +
      geom_sf(data = rail.pts, aes(col = "RShapes"), size = 0.001, shape = 2) +
      geom_sf(data = buff.10, aes(col = "Austro-Hungarian"), fill = "transparent") +
      scale_color_viridis_d(direction = -1, begin = 0.1, end = 0.65, option = "B") +
      labs(col = "", title = i,
           subtitle = paste0("Share within 5km: ", share.5, 
                             "\nShare within 10km: ", share.10)) +
      coord_sf(xlim = c(10, 30), ylim = c(42, 51)) +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            legend.position = "bottom",
            panel.background = element_rect(fill = "aliceblue"),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            title = element_text(hjust = 1))
    ggsave(here("figures", paste0("habsburg_overlap_", i,".pdf")),
           width = 5.5, height = 4)
    
  }
  
  # return average metrics
  list_out[[which(vec_years == i)]] <- tibble(year = i,
                                              dist = (rail.dists),
                                              share_10 = share.10,
                                              share_5 = share.5)
}

# get trends
dat_out <- list_out %>% 
  bind_rows() %>% 
  group_by(year) %>% 
  summarise(dist = mean(dist, na.rm = T),
            share_10 = mean(share_10, na.rm = T),
            share_5 = mean(share_5, na.rm = T))

dat_out %>% 
  ggplot() +
  geom_line(aes(year, share_10 , col = "Share within 10 km")) +
  geom_line(aes(year, share_5 , col = "Share within 5 km")) +
  geom_point(aes(year, share_10 , col = "Share within 10 km")) +
  geom_point(aes(year, share_5 , col = "Share within 5 km")) +
  # labs(col = "", title = i,
  #      subtitle = paste0("Share within 5km: ", share.5, 
  #                        "\nShare within 5km: ", share.10)) +
  labs(col = "", y = "Share of points within\n distance from rails", x = "Year") +
  scale_x_continuous(breaks = scales::pretty_breaks(5)) +
  scale_color_viridis_d(direction = -1, begin = 0.1, end = 0.65, option = "A") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.box.margin = margin(-5, 0, 0, 0),
        title = element_text(hjust = 0.5))
ggsave(here("figures", "figure_a3a.pdf"),
       width = 6, height = 3.5)

dat_out %>% 
  ggplot() +
  geom_line(aes(year, dist / 1000, col = "Average distance (km)")) +
  geom_point(aes(year, dist / 1000, col = "Average distance (km)")) +
  # labs(col = "", title = i,
  #      subtitle = paste0("Share within 5km: ", share.5, 
  #                        "\nShare within 5km: ", share.10)) +
  labs(col = "", y = "Average distance (km)", x = "Year") +
  scale_x_continuous(breaks = scales::pretty_breaks(5)) +
  scale_color_grey() +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        title = element_text(hjust = 0.5))
ggsave(here("figures", "figure_a3b.pdf"),
       width = 6, height = 3.2)

