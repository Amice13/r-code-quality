# ==========================================
# Description: Create Figure IX
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
  summarise(mean_pct_female_faces = mean(pct_female_faces, na.rm=T),
            mean_pct_female_words = mean(pct_female_words, na.rm=T),
            num_books = n_distinct(book_ID))

plot <- decade_level_data %>%
  ggplot(aes(y=mean_pct_female_faces, x=mean_pct_female_words, shape=collection, color=collection, size=num_books)) +
  geom_point(alpha = 0.7) +
  theme(legend.position = "right", 
        legend.title = element_text(color ="black", size=11),
        panel.grid.minor.y = element_blank()) + 
  labs(size="Number of Books", color= "", shape="") +
  ylab("Female Faces as a Percent of All Faces") + 
  xlab("Female Words as a Percent of All Gendered Words") +
  scale_size(name = "Number of Books", breaks = c(15,30,150), labels = c(15,30,150)) +
  geom_abline(slope=1, linetype = "dashed", color="gray50") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color="gray85") +
  geom_vline(xintercept = 0.5, linetype = "dashed", color="gray85") +
  scale_y_continuous(limits = c(0.2,0.8), labels = percent_format()) + 
  scale_x_continuous(limits = c(0.2,0.8), labels = percent_format()) + 
  guides(shape = guide_legend(override.aes = list(size = 3)))

plot + miie$colors + miie$shapes
ggsave("../figures/figure_IX.png", width=6, height=4, units="in")

plot + miie$bw + miie$shapes
ggsave("../figures/grayscale_figure_IX.png", width=6, height=4, units="in")




