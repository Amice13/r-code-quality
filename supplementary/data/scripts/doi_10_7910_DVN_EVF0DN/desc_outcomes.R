### descriptives: outcomes
library(here)
library(sf)
library(tidyverse)

sf_use_s2(FALSE)

### load segment data
rail.df <- readRDS(here("analysis_data","analysis_rails_df.rds"))
segments.ls <- readRDS(here("analysis_data","segments_sf_ls_tv.rds"))

# Bind segments
segments.sf <- bind_rows(segments.ls)

# Find unique segment polygons
segments.unique.sf <- segments.sf %>% 
  group_by(groupname, name_uni, from, to) %>% 
  slice_head(n = 1)

### define countries to exclude from analysis
drop_cntrs <- c("United Kingdom","Egypt","Cherkessy","Ireland")

## code country_year fe
rail.df$country_year <- factor(paste(rail.df$id_cap_rule_corr,rail.df$year,sep="_"))


##### subset to analysis sample
rail.df.sample <- rail.df[which(rail.df$rail_ttt_5years_binned %in% c(seq(-60,90,5),-1000,1000) &
                                  rail.df$year>1815  & rail.df$year<1946 &
                                  rail.df$capital_yn_dist_corr==0 &
                                  rail.df$area_sqkm_raw>2000 &
                                  !rail.df$name_cap_rule_corr %in% drop_cntrs),]

# Separatism separatism_cw_sec_ind_aut_yn
# Secession: secession_yn
# terr civil war onset: cw_terr_onset_combined
# indipendence or autonomy claim: onset_nc_ind_aut


# Plot outcome trends ----------------------------------------------------------

# Make plot dataframe
dat_plot <- bind_rows(
  rail.df.sample %>% 
    group_by(year) %>% 
    summarise(count = sum(separatism_cw_sec_ind_aut_yn, na.rm = T),
              outcome = "All combined"),
  rail.df.sample %>% 
    group_by(year) %>% 
    summarise(count = sum(secession_yn, na.rm = T),
              outcome = "Secession"),
  rail.df.sample %>% 
    group_by(year) %>% 
    summarise(count = sum(cw_terr_onset_combined, na.rm = T),
              outcome = "Conflict"),
  rail.df.sample %>% 
    group_by(year) %>% 
    summarise(count = sum(onset_nc_ind_aut, na.rm = T),
              outcome = "Claims")
)

# Make plot
dat_plot %>% 
  ggplot() +
  geom_line(aes(year, count)) +
  facet_wrap(~ outcome) +
  labs(y = "Number of yearly onsets", x = "Year",
       title = "Outcome onsets over time") +
  theme_minimal(base_size=9) +
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = scales::pretty_breaks(6))
ggsave(here("figures", "figure_a4.pdf"),
       width = 6, height = 3)


# Plot outcome maps ------------------------------------------------------------
dat_sum <- rail.df.sample %>% 
  group_by(groupname, seg_id, name_uni) %>% 
  summarise(separatism_cw_sec_ind_aut_yn = sum(separatism_cw_sec_ind_aut_yn, na.rm = T),
            secession_yn = sum(secession_yn, na.rm = T),
            onset_nc_ind_aut = sum(onset_nc_ind_aut, na.rm = T),
            cw_terr_onset_combined = sum(cw_terr_onset_combined, na.rm = T)) 

# Create background
back.sf <- segments.unique.sf %>% 
  st_union() %>% 
  nngeo::st_remove_holes()

# Find unique segment polygons
segments.merge.sf <- segments.unique.sf %>% 
  mutate(groupname = str_replace_all(groupname, "\\_", " ")) %>% 
  select(groupname, name_uni, from) %>% 
  inner_join(dat_sum)


ggplot() +
  geom_sf(data = back.sf, fill = "gray80") +
  geom_sf(data = segments.merge.sf %>% 
            group_by(groupname) %>% 
            arrange(desc(separatism_cw_sec_ind_aut_yn)), 
          aes(fill = separatism_cw_sec_ind_aut_yn), alpha = .3, col = "transparent") +
  scale_fill_viridis_c(option = "B", breaks = scales::pretty_breaks(n = 5),
                       na.value = "transparent", begin = 0.1) +
  labs(fill = "Onsets") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        # panel.background = element_rect(fill = "aliceblue"),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(here("figures", "figure_a5a.pdf"),
       width = 5.5, height = 5)

ggplot() +
  geom_sf(data = back.sf, fill = "gray80") +
  geom_sf(data = segments.merge.sf %>% 
            group_by(groupname) %>% 
            arrange(desc(secession_yn)), 
          aes(fill = secession_yn), alpha = .3, col = "transparent") +
  scale_fill_viridis_c(option = "B", breaks = scales::pretty_breaks(n = 1),
                       na.value = "transparent", begin = 0.1) +
  labs(fill = "Onsets") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(here("figures", "figure_a5b.pdf"),
       width = 5.5, height = 5)

ggplot() +
  geom_sf(data = back.sf, fill = "gray80") +
  geom_sf(data = segments.merge.sf %>% 
            group_by(groupname) %>% 
            arrange(desc(onset_nc_ind_aut)), 
          aes(fill = onset_nc_ind_aut), alpha = .3, col = "transparent") +
  scale_fill_viridis_c(option = "B", breaks = scales::pretty_breaks(n = 1),
                       na.value = "transparent", begin = 0.1) +
  labs(fill = "Onsets") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(here("figures", "figure_a5c.pdf"),
       width = 5.5, height = 5)

ggplot() +
  geom_sf(data = back.sf, fill = "gray80") +
  geom_sf(data = segments.merge.sf %>% 
            group_by(groupname) %>% 
            arrange(desc(cw_terr_onset_combined)), 
          aes(fill = cw_terr_onset_combined), alpha = .3, col = "transparent") +
  scale_fill_viridis_c(option = "B", breaks = scales::pretty_breaks(n = 5),
                       na.value = "transparent", begin = 0.1) +
  labs(fill = "Onsets") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(here("figures", "figure_a5d.pdf"),
       width = 5.5, height = 5)

# Clean up ----
rm(back.sf,  segments.merge.sf, segments.sf, segments.unique.sf, dat_sum,
   dat_plot)
