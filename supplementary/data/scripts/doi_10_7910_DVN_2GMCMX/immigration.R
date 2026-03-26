library(tidyverse)
library(cregg)
library(patchwork)

# Load data
conjoint_JP <- read_csv("immigration_JP.csv")
conjoint_UK <- read_csv("immigration_UK.csv")

conjoint_JP <- conjoint_JP %>% 
  select(-task, -x)

# Add Demand/Skill variables----
conjoint_JP <- conjoint_JP %>% 
  mutate(attr6 = ifelse(attr3 %in% c(1,2,3,4),1,0)) %>%  #High skilled = 1, low skilled = 0
  mutate(attr7 = ifelse(attr3 %in% c(1,2,5,6),1,0)) %>% #High demand = 1, low demand = 0
  mutate(attr8 = case_when(
    attr6 == 1 & attr7 == 1 ~ 4,
    attr6 == 1 & attr7 == 0 ~ 3,
    attr6 == 0 & attr7 == 1 ~ 2,
    attr6 == 0 & attr7 == 0 ~ 1))

conjoint_UK <- conjoint_UK %>% 
  mutate(attr6 = ifelse(attr3 %in% c(1,2,3,4),1,0)) %>%  #High skilled = 1, low skilled = 0
  mutate(attr7 = ifelse(attr3 %in% c(1,2,5,6),1,0)) %>%   #High demand = 1, low demand = 0
  mutate(attr8 = case_when(
    attr6 == 1 & attr7 == 1 ~ 4,
    attr6 == 1 & attr7 == 0 ~ 3,
    attr6 == 0 & attr7 == 1 ~ 2,
    attr6 == 0 & attr7 == 0 ~ 1))

# Add labels----
conjoint_JP <- conjoint_JP %>% 
  mutate(attr1 = factor(attr1, labels = c("24 years old", "44 years old", "64 years old"))) %>% 
  mutate(attr2 = factor(attr2, labels = c("Male", "Female"))) %>% 
  mutate(attr3 = factor(attr3, labels = c("Computer programmer", "Doctor", "Lawyer", "Office manager", "Fruit picker", "Home care worker", "Retail worker", "Call center"))) %>% 
  mutate(attr4 = factor(attr4, labels = c("Peru","China","Korea","Viet Num"))) %>% 
  mutate(attr5 = factor(attr5, labels = c("Long","Short"))) %>% 
  mutate(attr6 = factor(attr6, labels = c("Low skilled", "High skilled"))) %>% 
  mutate(attr7 = factor(attr7, labels = c("Low demand", "High demand"))) %>% 
  mutate(attr8 = factor(attr8, labels = c("Low Skilled + Low demand","Low Skilled + High demand", "High Skilled + Low demand", "High Skilled + High demand")))

attr(conjoint_JP$attr1, "label") <- "Age"
attr(conjoint_JP$attr2, "label") <- "Gender"
attr(conjoint_JP$attr3, "label") <- "Occupation"
attr(conjoint_JP$attr4, "label") <- "Country of origin"
attr(conjoint_JP$attr5, "label") <- "Length of stay"
attr(conjoint_JP$attr6, "label") <- "Skill"
attr(conjoint_JP$attr7, "label") <- "Demand"
attr(conjoint_JP$attr8, "label") <- "Skill * Demand"

conjoint_UK <- conjoint_UK %>% 
  mutate(attr1 = factor(attr1, labels = c("24 years old", "44 years old", "64 years old"))) %>% 
  mutate(attr2 = factor(attr2, labels = c("Male", "Female"))) %>% 
  mutate(attr3 = factor(attr3, labels = c("Computer programmer", "Doctor", "Lawyer", "Office manager", "Fruit picker", "Home care worker", "Retail worker", "Call center"))) %>% 
  mutate(attr4 = factor(attr4, labels = c("India","Australia","Poland","Nigeria"))) %>% 
  mutate(attr5 = factor(attr5, labels = c("Long","Short"))) %>% 
  mutate(attr6 = factor(attr6, labels = c("Low skilled", "High skilled"))) %>% 
  mutate(attr7 = factor(attr7, labels = c("Low demand", "High demand"))) %>% 
  mutate(attr8 = factor(attr8, labels = c("Low Skilled + Low demand","Low Skilled + High demand", "High Skilled + Low demand", "High Skilled + High demand")))

attr(conjoint_UK$attr1, "label") <- "Age"
attr(conjoint_UK$attr2, "label") <- "Gender"
attr(conjoint_UK$attr3, "label") <- "Occupation"
attr(conjoint_UK$attr4, "label") <- "Country of origin"
attr(conjoint_UK$attr5, "label") <- "Length of stay"
attr(conjoint_UK$attr6, "label") <- "Skill"
attr(conjoint_UK$attr7, "label") <- "Demand"
attr(conjoint_UK$attr8, "label") <- "Skill * Demand"

# Figure 2----
mm_skill_UK <- mm(conjoint_UK, dep ~ attr6 + attr1 + attr2 + attr4 + attr5, id = ~ID)
mm_skill_JP <- mm(conjoint_JP, dep ~ attr6 + attr1 + attr2 + attr4 + attr5, id = ~ResponseId)
fig2uk <- plot(mm_skill_UK, size = 3, legend_pos = "",
               vline = 0.5, xlim = c(0.35, 0.9), feature_headers = FALSE) + 
  labs(title = "United Kingdom") +
  scale_color_manual(values = rep("black", 9)) +
  facet_grid(feature~., scales = "free_y")

fig2jp <- plot(mm_skill_JP, size = 3, legend_pos = "",
               vline = 0.5, xlim = c(0.35, 0.9), feature_headers = FALSE) + 
  labs(title = "Japan") +
  scale_color_manual(values = rep("black", 9)) +
  facet_grid(feature~., scales = "free_y")


fig2 <- fig2uk + fig2jp
ggsave("Fig2.pdf", plot = fig1, dpi = 400)
fig2

# Figure 3----
mm_demand_UK <- mm(conjoint_UK, dep ~ attr1 + attr2 + attr7 + attr4 + attr5, id = ~ID)
mm_demand_JP <- mm(conjoint_JP, dep ~ attr1 + attr2 + attr7 + attr4 + attr5, id = ~ResponseId)
fig3uk <- plot(mm_demand_UK, size = 3, legend_pos = "",
               vline = 0.5, xlim = c(0.35, 0.9), feature_headers = FALSE) + 
  labs(title = "United Kingdom") +
  scale_color_manual(values = rep("black", 9)) +
  facet_grid(feature~., scales = "free_y")

fig3jp <- plot(mm_demand_JP, size = 3, legend_pos = "",
               vline = 0.5, xlim = c(0.35, 0.9), feature_headers = FALSE) + 
  labs(title = "Japan") +
  scale_color_manual(values = rep("black", 9)) +
  facet_grid(feature~., scales = "free_y")


fig3 <- fig3uk + fig3jp
ggsave("Fig3.pdf", plot = fig2, dpi = 400)

# Figure 4----
mm_int_UK <- mm(conjoint_UK, dep ~ attr8 + attr1 + attr2 + attr4 + attr5, id = ~ID)
mm_int_JP <- mm(conjoint_JP, dep ~ attr8 + attr1 + attr2 + attr4 + attr5, id = ~ResponseId)
fig4uk <- plot(mm_int_UK, size = 3, legend_pos = "",
               vline = 0.5, xlim = c(0.35, 0.9), feature_headers = FALSE) + 
  labs(title = "United Kingdom") +
  scale_color_manual(values = rep("black", 9)) +
  facet_grid(feature~., scales = "free_y")

fig4jp <- plot(mm_int_JP, size = 3, legend_pos = "",
               vline = 0.5, xlim = c(0.35, 0.9), feature_headers = FALSE) + 
  labs(title = "Japan") +
  scale_color_manual(values = rep("black", 9)) +
  facet_grid(feature~., scales = "free_y")

fig4 <- fig4uk + fig4jp
ggsave("Fig4.pdf", plot = fig3, dpi = 400)
