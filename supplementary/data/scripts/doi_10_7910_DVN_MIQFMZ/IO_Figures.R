library(tidyverse)
library(ggplot2)
library(magrittr)
library(ggrepel)

# Replication of Figure 1
setwd("C:/Users/owner/Dropbox/Korea/Replication")
data <- read_csv("OECDPollution.csv") %>% filter(TIME %in% 2010:2019)

highlight_codes <- c("KOR","TUR","POL","COL","MEX","ITA","OECD","USA")
highlight <- data %>% filter(LOCATION %in% highlight_codes)

label_data <- highlight %>%
  filter(TIME == max(TIME)) %>%
  mutate(
    label_text = case_when(
      LOCATION == "KOR" ~ "KOR : $ 31640",
      LOCATION == "TUR" ~ "TUR : $ 9103",
      LOCATION == "POL" ~ "POL : $ 15053",
      LOCATION == "COL" ~ "COL : $ 6404",
      LOCATION == "MEX" ~ "MEX : $ 10013",
      LOCATION == "ITA" ~ "ITA : $ 32114",
      LOCATION == "OECD" ~ "OECD : $ 46664",
      LOCATION == "USA" ~ "USA : $ 60698",
      TRUE ~ as.character(LOCATION)
    )
  )

pal <- c(
  KOR="black", TUR="#e31a1c", POL="#9c27b0", COL="#ff00a6",
  MEX="#1f78b4", ITA="#f39c12", OECD="#8b2314", USA="#2e7d32"
)

p <- ggplot() +
  # LIGHTER gray background lines (and still a bit thick)
  geom_line(
    data = highlight,
    aes(x = TIME, y = Value, group = LOCATION, color = LOCATION),
    linewidth = 2.3
  ) +
  geom_line(
    data = highlight,
    aes(x = TIME, y = Value, group = LOCATION, color = LOCATION),
    linewidth = 1.6
  ) +
  geom_label_repel(
    data = label_data,
    aes(x = TIME, y = Value, label = label_text, color = LOCATION),
    fill = "white", fontface = "bold", label.size = 0.4,
    nudge_x = 1.5, hjust = 0, direction = "y",
    force = 2, box.padding = 0.3, point.padding = 0.2,
    min.segment.length = 0, segment.color = NA,
    show.legend = FALSE, seed = 123
  ) +
  scale_color_manual(values = pal, guide = "none") +
  scale_x_continuous(breaks = 2010:2019, expand = expansion(add = c(0, 2))) +
  xlab("Year") + ylab("PM2.5 Pollution") +
  coord_cartesian(clip = "off") +
  theme_classic() +
  theme(
    axis.ticks = element_line(color = "black"),
    axis.line = element_line(color = "black"),
    plot.margin = margin(5.5, 30, 5.5, 5.5)
  )

# Save PNG in the current working directory
ggsave("C:/Users/owner/Dropbox/Korea/Replication/output/OECDpollution.png", plot = p, width = 8, height = 8, dpi = 400, bg = "white")

# Replication of GongJung Public Opinion Survey Results
## data source : http://gongjung.org/hold-1/?board_name=planningboard&search_field=fn_title&search_text=중국&vid=77

gongjung <- tibble(
  country = c("China", "South Korea", "I don't know"),
  rate = c(82.6, 12.2, 5.2)
)

p <- ggplot(gongjung) +
  geom_col(aes(x = fct_reorder(country, rate, .desc = TRUE), y = rate)) +
  xlab("") + ylab("Perception Rate (%)") +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

ggsave("C:/Users/owner/Dropbox/Korea/Replication/output/fine_dust_country.png", plot = p, width = 6, height = 4, dpi = 400, bg = "white")

# Replication of Sisain Public Opinion Survey Results
## data source : https://www.sisain.co.kr/news/articleView.html?idxno=44821
sisain <- tibble(
  reason = c(
    "Yellow Dust and FIne Dust Issues",
    "China's Response to COVID-19",
    "Illegal Operations and Economic Sector Issues",
    "Negative Expressions by Chinese Netizens",
    "China's Political and Social System",
    "Negative Chinese Media Coverage of South Korea",
    "Cross-Strait Relations (China-Taiwan Conflict)",
    "Negative South Korean Media Coverage of China",
    "US-China Trade War",
    "China-North Korea Relations",
    "Anti-Chinese Expressions by South Korean Netizens",
    "South Korea-China Economic Cooperation",
    "People-to-People Exchange between South Korea and China",
    "Chinese Culture (Movies, Dramas, etc.)"
  ),
  value = c(89.4, 86.9, 84.3, 80.0, 78.1, 74.7, 62.5, 62.3, 61.2, 59.6, 49.8, 30.7, 27.8, 23.6)
)

p <- ggplot(sisain, aes(x = value, y = fct_reorder(reason, value, .desc = FALSE))) +
  geom_col() +
  xlab("") + ylab("") +
  geom_text(aes(label = value), hjust = -0.15, size = 3.5, color = "black") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.10))) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    plot.margin      = margin(5.5, 24, 5.5, 5.5)  # extra right padding for labels
  ) +
  coord_cartesian(clip = "off")

ggsave("C:/Users/owner/Dropbox/Korea/Replication/output/negative_perception.png", plot = p, width = 9, height = 7, dpi = 400, bg = "white")