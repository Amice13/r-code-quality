library(arrow)
library(here)
library(scales)
library(statar)
library(tidyverse)
library(extrafont)

loadfonts(device = "win")

next_cycle <- read_csv(here::here("data/next_cycle_95_slope.csv"))

pl2county <- read_feather(here::here("data/combined_samp_slope.feather")) %>% 
  distinct(PL_NAME, PARCEL_COUNTY) %>% 
  assertr::verify(anyDuplicated(PL_NAME) == FALSE) %>% 
  rename(county = PARCEL_COUNTY)

next_cycle <- next_cycle %>% 
  statar::join(pl2county, 
               on = "PL_NAME", 
               kind = "full", 
               check = 1 ~ 1, 
               gen = "merge") %>% 
  assertr::verify(merge == 3) %>% 
  dplyr::select(-merge) %>% 
  mutate(PL_NAME = str_to_title(PL_NAME),
         county = str_to_title(county)) 

samp <- read_feather(here::here("data/combined_samp_slope.feather"))

naive_pred <- samp %>% 
  group_by(PL_NAME) %>% 
  summarise(naive_pred = 2*sum(PARCEL_ADU)) %>% 
  mutate(PL_NAME = str_to_title(PL_NAME))


next_cycle <- 
  statar::join(next_cycle, naive_pred, 
               on = "PL_NAME", kind = "full", 
               check = 1 ~ 1, gen = "merge") %>% 
  assertr::verify(merge == 3) %>% 
  dplyr::select(-merge)

figure_2 <- next_cycle %>% 
  dplyr::filter(county == "Los Angeles") %>% 
  rename("Rule of thumb" = naive_pred,
         "Regression" = predicted_ADUs) %>% 
  ggplot(aes(y = reorder(PL_NAME, Regression, mean)),
         show.legend = FALSE) + 
  geom_point(aes(x = Regression, color = "Regression", 
                 shape = "Regression", alpha = 0.9)) +
  geom_point(aes(x = `Rule of thumb`, color = "Rule of thumb", 
                 shape = "Rule of thumb", alpha = 0.9)) +
  scale_color_manual("Model Type", values = c(Regression = "#F8766D", 
                                              `Rule of thumb` = "#619CFF")) + 
  scale_shape_manual("Model Type", values = c(Regression = 16, 
                                              `Rule of thumb` = 17)) + 
  geom_errorbar(aes(xmin=simulated_ADUs_05, 
                    xmax=simulated_ADUs_95, 
                    color = "Regression", alpha = 0.9),
                show.legend = FALSE) + 
  scale_y_discrete(guide = guide_axis(n.dodge = 2), position = "right") + 
  scale_x_log10(labels = scales::comma, 
                breaks = scales::trans_breaks('log10', function(x) 10^x)) + 
  labs(
    x = "Predicted ADUs, eight-year planning period (log scale)",
    y = "Jurisdiction"
  ) +
  guides(alpha = "none") +
  theme(text = element_text(family = "Times New Roman", size = 10))

dir.create(here::here("writing/japa/figures/"))

figure_2

ggsave(
  "writing/japa/figures/figure_2.png",
  width = 6.5,
  height = 8,
  units = "in"
)
