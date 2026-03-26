# ==========================================
# Description: Create Figure VII
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
collection_level_data <- book_level_data %>%
  group_by(collection) %>%
  summarise(Asian_Female = mean(pct_asian_female_faces, na.rm=T),
            Asian_Male = mean(pct_asian_male_faces, na.rm=T),
            Black_Female = mean(pct_black_female_faces, na.rm=T),
            Black_Male = mean(pct_black_male_faces, na.rm=T),
            `Latinx + Others_Female` = mean(pct_latinx_others_female_faces, na.rm=T),
            `Latinx + Others_Male` = mean(pct_latinx_others_male_faces, na.rm=T),
            White_Female = mean(pct_white_female_faces, na.rm=T),
            White_Male = mean(pct_white_male_faces, na.rm=T))

# Convert data to long format
collection_level_data %<>% gather("race_gender","pct",-c("collection")) 

# Split Race and Gender identity into two variables
collection_level_data %<>% separate(race_gender, c("race","gender"), "_")


### ========================================
### Graph ==================================
### ========================================
plot <- collection_level_data %>%
  ggplot(aes(x=gender, y=pct, fill=collection)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label=percent(pct, 0.1)),  color="black", fontface="bold", vjust=-0.35)  +
  facet_grid(cols=vars(race), rows=vars(collection), labeller=miie$labeler) + 
  theme(axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(), 
        axis.title=element_blank(),
        legend.position = "none") + 
  ylim(0,1)

plot + miie$colors
ggsave("../figures/figure_VII.png", units="in", height=8, width=6)

plot + miie$bw
ggsave("../figures/grayscale_figure_VII.png", units="in", height=8, width=6)


