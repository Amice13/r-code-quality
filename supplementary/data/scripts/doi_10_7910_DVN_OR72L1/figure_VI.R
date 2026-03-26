# ==========================================
# Description: Create Figure VI
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
load("../data/census_data.Rdata")

# Collapse to book_level_data to decade
decade_level_data <- book_level_data %>%
  subset(collection %in% c("Mainstream", "Diversity")) %>%
  group_by(collection, decade) %>%
  summarise(Asian = mean(pct_mentions_famous_asian, na.rm=T),
            Black = mean(pct_mentions_famous_black, na.rm=T),
            Indigenous = mean(pct_mentions_famous_indigeneous, na.rm=T),
            Latinx = mean(pct_mentions_famous_latinx, na.rm=T),
            `Multiracial\n+ Others` = mean(pct_mentions_famous_multiracial, na.rm=T),
            White = mean(pct_mentions_famous_white, na.rm=T))
  
# Convert data to long format
decade_level_data %<>% gather("group","group_pct",-c("collection","decade")) 

# Merge Decade Level Representation Data with Census Population Estimates
merged_data <- census_data %>% 
  subset(type == "Race" & group != "Latinx + Others" & decade >= 1920) %>%
  bind_rows(decade_level_data)

# Relevel Factors
merged_data$collection %<>% factor(levels=c("Mainstream","Diversity","Census"))


### ========================================
### Graph Data =============================
### ========================================
plot <- merged_data %>%
ggplot(aes(x=decade, y=group_pct, color=collection, shape=collection)) +
  geom_line() +
  geom_point(size=2) +
  facet_wrap(vars(group),ncol=2) + 
  scale_y_continuous(labels = scales::percent_format(), limits=c(0,1)) +
  xlab("Decade") +
  theme(axis.title.y = element_blank()) +
  scale_shape_manual(values=c(16,17,0))

# Color
plot + miie$colors
ggsave("../figures/figure_VI.png", height=8, width = 6.5, units="in")


# Grayscale
plot + miie$bw
ggsave("../figures/grayscale_figure_VI.png", height=8, width = 6.5, units="in")
