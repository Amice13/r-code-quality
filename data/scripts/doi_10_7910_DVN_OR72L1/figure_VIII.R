# ==========================================
# Description: Create Figure VIII
# ==========================================

### ========================================
### Set Up =================================
### ========================================
# Set working directory to the directory to which this file is saved
setwd(dirname(getActiveDocumentContext()$path))

# Load Packages
source("requirements.R")

# Import Graph Themes
source("graph_themes.R")



### ========================================
### Data Prep ==============================
### ========================================
load("../data/representation_data.Rdata")

# Collapse to book_level_data to collection
decade_level_data <- book_level_data %>%
  group_by(collection, decade) %>%
  summarise(group_pct = mean(pct_female_words, na.rm=T))

# Merge Decade Level Representation Data with Census Population Estimates
merged_data <- census_data %>% 
  subset(group == "Female" & decade >= 1920) %>%
  bind_rows(decade_level_data)

# Relevel Factors
merged_data$collection %<>% factor(levels=c("Mainstream","Diversity",
                                            "People of Color","African American", 
                                            "Ability", "Female", "LGBTQIA+", "Census"))

# Group Decade Variable 
book_level_data$decade_bin <- case_when(
  book_level_data$decade %in% seq(1920,1950,10) ~ "1920-1960",
  book_level_data$decade %in% seq(1960,1980,10) ~ "1960-1990",
  book_level_data$decade %in% seq(1990,2010,10) ~ "1990-2010"
)


### ========================================
### Graph Panel (a) ========================
### ========================================
plot <- merged_data %>%
  ggplot(aes(x=decade, y=group_pct, color=collection, shape=collection, linetype=collection)) + 
  geom_line() + 
  geom_point(aes(size=collection)) +
  miie$lines +
  ylab("Female Words as a\nPercent of All Gendered Words") +
  xlab("Decade") +
  scale_y_continuous(labels=percent_format(), limits=c(0,1))

# Color
plot + miie$colors
ggsave("../figures/figure_VIII_a.png", units="in", height=5.25, width=7)

# BW
plot + miie$bw
ggsave("../figures/grayscale_figure_VIII_a.png", units="in", height=5.25, width=7)





### ========================================
### Graph Panel (b) ========================
### ========================================
plot <- book_level_data %>%
  subset(collection %in% c("Mainstream","Diversity")) %>%
  ggplot(aes(x=pct_female_words, fill=collection)) + 
  geom_density(alpha=0.75) +
  xlab("Female Words as a\nPercent of All Gendered Words") +
  ylab("Density") +
  facet_grid(rows=vars(collection), cols=vars(decade_bin)) + 
  scale_x_continuous(labels=percent_format(), limits=c(0,1)) + 
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.position = "none")


# Color
plot + miie$colors
ggsave("../figures/figure_VIII_b.png", units="in", width=6.2, height=4.2)

# BW
plot + miie$bw
ggsave("../figures/grayscale_figure_VIII_b.png", units="in", width=6.2, height=4.2)





### ========================================
### Graph Panel (c) ========================
### ========================================
plot <- book_level_data %>%
  ggplot(aes(x=pct_female_words, fill=collection)) + 
  geom_density(alpha=0.75) +
  xlab("Female Words as a\nPercent of All Gendered Words") +
  ylab("Density") +
  facet_grid(rows=vars(collection), labeller=miie$labeler) + 
  scale_x_continuous(labels=percent_format(), limits=c(0,1)) + 
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.position = "none")

# Color
plot + miie$colors
ggsave("../figures/figure_VIII_c.png", units="in", height=9.4, width=3.1)

# BW
plot + miie$bw
ggsave("../figures/grayscale_figure_VIII_c.png", units="in", height=9.4, width=3.1)





