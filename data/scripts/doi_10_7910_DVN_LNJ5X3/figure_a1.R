library(arrow)
library(extrafont)
library(here)
library(tidyverse)

PL_samp <- read_feather(here::here("data/combined_samp_slope.feather")) |> 
  distinct(PL_NAME, .keep_all = TRUE) |> 
  dplyr::select(starts_with("PL_"))
  
figa1 <- PL_samp |> 
  ggplot(aes(x = PL_pct_adu_unmerged_all)) + 
    geom_histogram() +
    labs(x = "% of unmerged ADU observations", y = "Count of jurisdictions") +
    theme_bw() +
    theme(text = element_text(family = "serif", size = 10))

figa1

ggsave(
  "writing/japa/figures/fig_a1.png",
  width = 6.5,
  units = "in"
)
