library(tidyverse)
library(gtsummary)
library(countrycode)
library(gt)
library(ragg)

# Descriptive Statistics table
df <- readRDS("data/societal_determinants_flood-induced_displacement.rds")
df$continent <- countrycode(df$gwcode, origin = "gwn", destination = "continent")
df$continent <- if_else(df$gwcode == 816, "Asia", df$continent)
df$continent <- if_else(df$gwcode == 345, "Europe", df$continent)
df$continent <- if_else(df$gwcode == 678, "Asia", df$continent)

df$continent <- if_else(df$continent == "Oceania", "Asia and Oceania", df$continent)
df$continent <- if_else(df$continent == "Asia", "Asia and Oceania", df$continent)


selvars <- unique(c("displaced_w","population_affected","duration","nevents_sum10","brd_6mb","nlightsmean","wdi_gdppc",
"excluded_share","regime_type","brd_6mb","casualties_brd_sum10","nlightsmean","wdi_gdppc","brd_6mb","casualties_brd_sum10","excluded_share",
"regime_type","nlightsmean","wdi_gdppc","excluded_share","regime_type","brd_6mb","casualties_brd_sum10","nlightsmean","wdi_gdppc",
"excluded_share","regime_type","brd_6mb","casualties_brd_sum10","nlightsmean","wdi_gdppc","excluded_share","regime_type"))

train <- df %>% select(all_of(c(selvars, "began", "continent"))) %>%
  filter(began < as.Date("2015-01-01"), displaced_w > 0)

test <- df %>% select(all_of(c(selvars, "began", "continent"))) %>%
  filter(began >= as.Date("2015-01-01"), displaced_w > 0)

train %>% dplyr::select(-began) %>%
  tbl_summary(by = continent,
              statistic = all_continuous() ~ "{median} ({min}, {max})") %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Continent**") %>%
  add_n() %>%
  as_gt() %>%
  gt::tab_source_note(gt::md("*Training data: 2000-2014*")) %>%
  gtsave("brm_results/tables/summary_table_train.html")

test %>% dplyr::select(-began) %>%
  tbl_summary(by = continent,
              statistic = all_continuous() ~ "{median} ({min}, {max})") %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Continent**") %>%
  add_n() %>%
  as_gt() %>%
  gt::tab_source_note(gt::md("*Test data: 2015-2018*")) %>%
  gtsave("brm_results/tables/summary_table_test.html")



dfo <- readRDS("data/dfo.rds")
dfo$tropical_storm <- grepl("tropical|hurricane|typhoon", dfo$dfo_main_cause, ignore.case = T)  %>% as.numeric()
df <- left_join(df, dfo %>% select(id, tropical_storm), by = c("event_id" = "id"))
df$tropical_storm <- factor(df$tropical_storm, labels = c("Other", "Tropical Storm"))

df$recorded_displaced <- if_else(df$displaced_w > 0, T, F)
df$recorded_dead <- if_else(df$dead_w > 0, T, F)
df$recorded_duration <- if_else(df$duration > 0, T, F)
df$recorded_population_affected <- if_else(df$population_affected > 0, T, F)


df %>% dplyr::select(recorded_displaced, recorded_dead, continent) %>%
  tbl_cross(row = recorded_displaced, col = recorded_dead, percent = "cell",
            label = list(recorded_displaced ~ ">0 displaced", recorded_dead ~ ">0 dead")) %>%
  modify_header(label ~ "**Variable**") %>%
  as_gt() %>%
  gtsave("brm_results/tables/displaced_crosstab_dead.html")

df %>% dplyr::select(recorded_displaced, recorded_duration, continent) %>%
  tbl_cross(row = recorded_displaced, col = recorded_duration, percent = "cell",
            label = list(recorded_displaced ~ ">0 displaced", recorded_duration ~ ">0 duration")) %>%
  modify_header(label ~ "**Variable**") %>%
  as_gt() %>%
  gtsave("brm_results/tables/displaced_crosstab_duration.html")

df %>% dplyr::select(recorded_displaced, recorded_population_affected, continent) %>%
  tbl_cross(row = recorded_displaced, col = recorded_population_affected, percent = "cell",
            label = list(recorded_displaced ~ ">0 displaced", recorded_population_affected ~ ">0 population affected"))  %>%
  modify_header(label ~ "**Variable**") %>%
  as_gt() %>%
  gtsave("brm_results/tables/displaced_crosstab_population_affected.html")

df %>% select(all_of(c(selvars, "began", "continent"))) %>%
  filter(began < as.Date("2015-01-01"), displaced_w == 0) %>% dplyr::select(-began) %>%
  tbl_summary(by = continent,
              statistic = all_continuous() ~ "{median} ({min}, {max})") %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Continent**") %>%
  add_n() %>%
  as_gt() %>%
  gt::tab_source_note(gt::md("*Training data: 2000-2014*")) %>%
  gtsave("brm_results/tables/summary_table_train_only_zero_displaced.html")

df %>% select(all_of(c(selvars, "began", "continent"))) %>%
  filter(began >= as.Date("2015-01-01"), displaced_w == 0) %>% dplyr::select(-began) %>%
  tbl_summary(by = continent,
              statistic = all_continuous() ~ "{median} ({min}, {max})") %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Continent**") %>%
  add_n() %>%
  as_gt() %>%
  gt::tab_source_note(gt::md("*Test data: 20015-2018*")) %>%
  gtsave("brm_results/tables/summary_table_test_only_zero_displaced.html")

df <- df %>% mutate(missing = displaced_w == 0)

ga <- ggplot(df, aes(x = dead_w, y = displaced_w, shape = missing, color = continent)) + geom_point(size = 1) +
  scale_y_continuous(trans = "log1p", breaks = c(0, 10, 100, 1000, 10000, 100000, 1000000, 10000000), labels = scales::scientific) + 
  scale_x_continuous(trans = "log1p", breaks = c(0, 10, 100, 1000, 10000, 100000), labels = scales::scientific) +
  scale_shape_manual("Displacement count is missing", values = c(19,0)) +
  scale_color_manual("Continent", values = c("#e66101","#fdb863","#b2abd2","#5e3c99"))  +
  xlab("Dead (log-scale)") +
  ylab("Displaced (log-scale)") + 
  theme_bw() +
  theme(legend.position = "none")

df <- df %>% mutate(affected_undercounted = population_affected < displaced_w)

gb <- ggplot(df, aes(x = population_affected, y = displaced_w, shape = missing, color = continent)) + geom_point(size = 1) +
  scale_y_continuous(trans = "log1p", breaks = c(0, 10, 100, 1000, 10000, 100000, 1000000, 10000000), labels = scales::scientific) + 
  scale_x_continuous(trans = "log1p", breaks = c(0, 10, 100, 1000, 10000, 100000, 1000000, 10000000), labels = scales::scientific) +
  scale_shape_manual("Displacement count is missing", values = c(19,0), guide = "none") +
  scale_color_manual("Continent", values = c("#e66101","#fdb863","#b2abd2","#5e3c99"))  +
  xlab("Directly exposed (log-scale)") +
  ylab("Displaced (log-scale)") + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  theme_bw() +
  theme(legend.position = "bottom")

# Without continents
# gb <- ggplot(df, aes(x = population_affected, y = displaced_w, shape = missing)) + geom_point(size = 2) +
#   scale_y_continuous(trans = "log1p", breaks = c(0, 10, 100, 1000, 10000, 100000, 1000000, 10000000), labels = scales::comma) + 
#   scale_x_continuous(trans = "log1p", breaks = c(0, 10, 100, 1000, 10000, 100000, 1000000, 10000000), labels = scales::comma) +
#   scale_shape_manual("Displacement count is missing", values = c(19,0)) +
#   xlab("Population affected (log-scale)") +
#   ylab("Displaced (log-scale)") + 
#   geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
#   theme_bw() +
#   theme(legend.position = "bottom")

library(patchwork)
ga + gb + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect") & theme(axis.title = element_text(size = 10),
                                                    axis.text = element_text(size = 6),
                                                    legend.text = element_text(size = 8),
                                                    legend.title = element_text(size = 10),
                                                    plot.tag = element_text(size = 14),
                                                    plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
                                                    legend.position = "bottom") & scale_shape_manual(values = c(19,0), guide = "none")

my_tiff <- function(...) ragg::agg_tiff(..., units = "in", res = 300)
ggsave("brm_results/figures/figure4_small_300dpi.tiff", device = my_tiff, width = 3.6, height = 2.3, scale = 2)



ggplot(df, aes(x = population_affected, y = displaced_w, shape = missing, color = tropical_storm)) + geom_point(size = 1.3) +
  scale_y_continuous(trans = "log1p", breaks = c(0, 10, 100, 1000, 10000, 100000, 1000000, 10000000), labels = scales::comma) + 
  scale_x_continuous(trans = "log1p", breaks = c(0, 10, 100, 1000, 10000, 100000, 1000000, 10000000), labels = scales::comma) +
  scale_shape_manual("Displacement count is missing", values = c(19,0), guide = "none") +
  scale_color_manual("Disaster type", values = c("gray60", "red"))  +
  xlab("Directly exposed (log-scale)") +
  ylab("Displaced (log-scale)") + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~continent) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14, angle = 45, hjust = 1),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        plot.tag = element_text(size = 28))
ggsave("brm_results/figures/popaffected_on_displaced_by_disaster_type_and_continent.png", device = ragg::agg_png(), scale = 1, dpi = 300, width = 14, height = 7)




