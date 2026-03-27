# Alexander F. Gazmararian
# agazmararian@gmail.com

library(gridExtra)
library(here)
library(patchwork)
library(rnaturalearthdata)
library(scales)
library(scico)
library(sf)
library(tidyverse)

load(here("Data", "inter", "19_wrp", "subregion_sf.rda"))
sf_data <- st_make_valid(sf_data)

zone_size <- readRDS(here("Data", "inter", "19_wrp", "zone_area.rds"))
zone_size$area <- zone_size$area / 1e6 # convert to square kilometers
sf_data <- left_join(sf_data, zone_size, by = c("GID_0", "REG_ID"))

regid <- readRDS(here("Data", "inter", "19_wrp", "admin_wrp_crosswalk.rds"))

names(sf_data) <- tolower(names(sf_data))

sf_out <- left_join(regid, sf_data, by = c("reg_id", "gid_0"))

g <- readRDS(here("Data", "inter", "19_wrp", "19_wrp_noweights.rds"))

g <- subset(g, select = c(wpid_random, country, salient, best_tanom_7d_z, best_tanom_2sd_7d_z, 
                          modis_burnanomp_mu_6m_w1_z, gdp_50, globalreg))


df <- left_join(g, sf_out, by = "wpid_random")

# Calculate median respondent administrative area size
median(df$area)

# Create aggregate outcome for purposes of plotting countries
df_agg <- df %>%
  group_by(reg_id, globalreg, gid_0, geom) %>%
  summarize(risk = mean(salient))

country_reg <- left_join(countries110, df_agg, by = c("adm0_a3_us" = "gid_0"))

region_value <- "Middle/Western Africa"

p1 <- df %>%
  filter(globalreg == region_value) %>%
  group_by(reg_id, geom) %>%
  summarize(risk = mean(salient)) %>%
  ggplot() +
  geom_sf(data = subset(country_reg, globalreg == region_value), 
          color = "black", fill = "grey", aes(geometry = geometry), size = .75) +
  geom_sf(color = "black", aes(fill = risk, geometry = geom)) +
  scale_fill_scico(
    palette = "lapaz",
    labels = c("0.0%", "2%", "4%", ">6%"),
    limits = c(0, 0.06552), # 1st thru 3rd quartile coz extreme outlier
    oob = squish,
    direction = -1,
    na.value = "grey",
  ) +
  labs(fill = "", title = "Climate risk perceptions", 
       caption = str_wrap("% naming climate change as top/major daily risk (Source: World Risk Poll)", 50)) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    plot.title = element_text(hjust = .5),
    panel.background = element_blank(),
    legend.position = "bottom", 
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black"),
    strip.text = element_text(color = "black"),
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(.2, "cm"),
    plot.caption = element_text(hjust = .5),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_blank()
    
    )
ggsave(p1, filename = here("Output", "figures", "fig_B24_subregion_perception.pdf"), 
       width = 6.5, height = 4, scale = 1.5)

p2 <- df %>%
  filter(globalreg == region_value) %>%
  group_by(reg_id, geom) %>%
  summarize(tanom_z = mean(best_tanom_7d_z)) %>%
  ggplot() +
  geom_sf(data = subset(country_reg, globalreg == region_value), 
          color = "black", fill = "grey", aes(geometry = geometry), size = .75) +
  geom_sf(color = "black", aes(fill = tanom_z, geometry = geom)) +
  scale_fill_scico(
    palette = "vik",
    na.value = "grey",
    midpoint = 0,
    limits = c(-.75, 1.75),
    breaks = c(-.75, 0, .75, 1.5),
    labels = function(x) sprintf("%.2f SD", x)
  ) +
  labs(fill = NULL) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    plot.title = element_text(hjust = .5),
    panel.background = element_blank(),
    
    legend.background = element_blank(),
    legend.position = "bottom", 
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black"),
    strip.text = element_text(color = "black"),
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(.2, "cm"),
    plot.caption = element_text(hjust = .5),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_blank()
    
  )
ggsave(p2, filename = here("Output", "figures", "fig_1_subregion_temp.pdf"), 
       width = 6.5, height = 4, scale = 1.5)

p3 <- df %>%
  filter(globalreg == region_value) %>%
  group_by(reg_id, geom) %>%
  summarize(gdp_50 = 100 - (100 * mean(gdp_50))) %>%
  ggplot() +
  geom_sf(data = subset(country_reg, globalreg == region_value), 
          color = "black", fill = "grey", aes(geometry = geometry), size = .75) +
  geom_sf(color = "black", aes(fill = gdp_50, geometry = geom)) +
  scale_fill_scico(
    palette = "cork",
    midpoint = 0,
    limits = c(-2, 3),
    labels = scales::percent_format(scale = 1)
  ) +
  labs(fill = "", title = "Vulnerability", 
       caption = str_wrap("Projected % global warming damage to 2050 GDP from spatial IAM (Source: Cruz and Rossi-Hansberg 2024)", 50)) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    plot.title = element_text(hjust = .5),
    panel.background = element_blank(),
    legend.position = "bottom", 
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black"),
    strip.text = element_text(color = "black"),
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(.2, "cm"),
    plot.caption = element_text(hjust = .5),
    
    panel.border = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_blank()
    
  )

ggsave(p3, filename = here("Output", "figures", "fig_B23_subregion_vulnerability.pdf"), 
       width = 6.5, height = 4, scale = 1.5)
