library(tidyverse)
library(cregg)
library(patchwork)
library(modelsummary)
library(gt)
library(estimatr)
library(emmeans)

# Setup----
## Load data----
conjoint_JP <- read_csv("immigration_JP.csv")
conjoint_UK <- read_csv("immigration_UK.csv")
trust_JP <- read_csv("trust_JP.csv")
trust_UK <- read_csv("trust_UK.csv")

## Add Demand/Skill variables----
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

## Add labels----
conjoint_JP <- conjoint_JP %>% 
  mutate(attr1 = factor(attr1, labels = c("24 years old", "44 years old", "64 years old"))) %>% 
  mutate(attr2 = factor(attr2, labels = c("Male", "Female"))) %>% 
  mutate(attr3 = factor(attr3, labels = c("Computer programmer", "Doctor", "Lawyer", "Office manager", "Fruit picker", "Home care worker", "Retail worker", "Call center"))) %>% 
  mutate(attr4 = factor(attr4, labels = c("Peru","China","Korea","Viet Num"))) %>% 
  mutate(attr5 = factor(attr5, labels = c("Long","Short"))) %>% 
  mutate(attr6 = factor(attr6, labels = c("Low skilled", "High skilled"))) %>% 
  mutate(attr7 = factor(attr7, labels = c("Low demand", "High demand"))) %>% 
  mutate(attr8 = factor(attr8, labels = c("L-S + L-D","L-S + H-D", "H-S + L-D", "H-S + H-D")))

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
  mutate(attr8 = factor(attr8, labels = c("L-S + L-D","L-S + H-D", "H-S + L-D", "H-S + H-D")))

attr(conjoint_UK$attr1, "label") <- "Age"
attr(conjoint_UK$attr2, "label") <- "Gender"
attr(conjoint_UK$attr3, "label") <- "Occupation"
attr(conjoint_UK$attr4, "label") <- "Country of origin"
attr(conjoint_UK$attr5, "label") <- "Length of stay"
attr(conjoint_UK$attr6, "label") <- "Skill"
attr(conjoint_UK$attr7, "label") <- "Demand"
attr(conjoint_UK$attr8, "label") <- "Skill * Demand"

## Add covariants----
conjoint_JP <- left_join(conjoint_JP, trust_JP, by = "ResponseId")
conjoint_JP <- conjoint_JP %>% 
  mutate(G_trust = ifelse(trust_JP_core_2 %in% c(1,2,3), 0, 1)) %>% 
  mutate(G_trust = factor(G_trust, labels = c("Low trust", "High trust"))) %>% 
  mutate(Ide = case_when(Ideology %in% c(0,1,2,3,4) ~ 1,
                         Ideology == 5 ~ 2,
                         Ideology %in% c(6,7,8,9,10) ~ 3)) %>% 
  mutate(Ide = factor(Ide, labels = c("Left", "Middle", "Right"))) %>% 
  mutate(gov_t = trust_JP_core_1_1 + trust_JP_core_1_2 + trust_JP_core_1_3 + trust_JP_core_1_4) %>% 
  mutate(gov_t2 = case_when(gov_t < 10 ~ 1,
                            gov_t > 9 & gov_t <19 ~ 2,
                            gov_t > 18 ~ 3)) %>% 
  mutate(gov_t2 = factor(gov_t2, labels = c("Low gov trust", "Middle gov trust", "High gov trust"))) %>% 
  mutate(gender = factor(gender, levels = c(1,2), labels = c("Male", "Female"))) %>% 
  mutate(univ = case_when(education < 4 ~ 0,
                          education > 3 & education < 7 ~ 1,
                          education == 7 ~ 0)) %>% 
  mutate(univ = factor(univ, labels = c("Without university degree", "University degree or above"))) %>% 
  mutate(old = ifelse(age < 46, 0, 1)) %>% 
  mutate(old = factor(old, labels = c("Young", "Old")))


conjoint_UK <- left_join(conjoint_UK, trust_UK, by = "ID")
conjoint_UK <- conjoint_UK %>% 
  mutate(G_trust = ifelse(trust_UK_core_2 %in% c(1,2,3), 0, 1)) %>% 
  mutate(G_trust = factor(G_trust, labels =c("Low trust", "High trust"))) %>% 
  mutate(Ide = case_when(Ideology %in% c(0,1,2,3,4) ~ 1,
                         Ideology == 5 ~ 2,
                         Ideology %in% c(6,7,8,9,10) ~ 3)) %>% 
  mutate(Ide = factor(Ide, labels = c("Left", "Middle", "Right"))) %>% 
  mutate(gov_t = trust_UK_core_1_1 + trust_UK_core_1_2 + trust_UK_core_1_3 + trust_UK_core_1_4) %>% 
  mutate(gov_t2 = case_when(gov_t < 10 ~ 1,
                            gov_t > 9 & gov_t <19 ~ 2,
                            gov_t > 18 ~ 3)) %>% 
  mutate(gov_t2 = factor(gov_t2, labels = c("Low gov trust", "Middle gov trust", "High gov trust"))) %>% 
  mutate(gender = factor(gender, labels = c("Male", "Female"))) %>% 
  mutate(univ = case_when(education < 15 ~ 0,
                          education > 14 & education <19 ~ 1,
                          education > 18 ~NA))%>% 
  mutate(univ = factor(univ, labels = c("Without university degree", "University degree or above")))%>% 
  mutate(old = ifelse(age < 46, 0, 1)) %>% 
  mutate(old = factor(old, labels = c("Young", "Old")))

# B Subgroup Analysis----
## B.1 Moderator Gender (Figure S1)----
mm_gen_UK <- cj(conjoint_UK, dep ~ attr8 + attr1 + attr2 + attr4 + attr5, id = ~ID, estimate = "mm", by = ~gender)
mm_gen_JP <- cj(conjoint_JP, dep ~ attr8 + attr1 + attr2 + attr4 + attr5, id = ~ResponseId, estimate = "mm", by = ~gender)
mod1uk <- plot(mm_gen_UK, size = 3, legend_pos = "bottom", legend_title = "", group = "gender",
               vline = 0.5, xlim = c(0.35, 0.9), feature_headers = FALSE) + 
  labs(title = "United Kingdom") +
  facet_grid(feature~., scales = "free_y")

mod1jp <- plot(mm_gen_JP, size = 3, legend_pos = "bottom", legend_title = "", group = "gender",
               vline = 0.5, xlim = c(0.35, 0.9), feature_headers = FALSE) + 
  labs(title = "Japan") +
  facet_grid(feature~., scales = "free_y")
mod1 <- mod1uk + mod1jp
mod1
ggsave("moderator1.png", plot = mod1, dpi = 400, width = 7, height = 6)
ggsave("moderator1.pdf", plot = mod1, dpi = 400, width = 7, height = 6)

## B.2 Moderator Age (Figure S2)----
mm_age_UK <- cj(conjoint_UK, dep ~ attr8 + attr1 + attr2 + attr4 + attr5, id = ~ID, estimate = "mm", by = ~old)
mm_age_JP <- cj(conjoint_JP, dep ~ attr8 + attr1 + attr2 + attr4 + attr5, id = ~ResponseId, estimate = "mm", by = ~old)
mod2uk <- plot(mm_age_UK, size = 3, legend_pos = "bottom", legend_title = "", group = "old",
               vline = 0.5, xlim = c(0.35, 0.9), feature_headers = FALSE) + 
  labs(title = "United Kingdom") +
  facet_grid(feature~., scales = "free_y")

mod2jp <- plot(mm_age_JP, size = 3, legend_pos = "bottom", legend_title = "", group = "old",
               vline = 0.5, xlim = c(0.35, 0.9), feature_headers = FALSE) + 
  labs(title = "Japan") +
  facet_grid(feature~., scales = "free_y")
mod2 <- mod2uk + mod2jp
mod2
ggsave("moderator2.png", plot = mod2, dpi = 400, width = 7, height = 6)
ggsave("moderator2.pdf", plot = mod2, dpi = 400, width = 7, height = 6)

## B.3 Moderator University attainment (Figure S3)----
mm_univ_UK <- cj(conjoint_UK, dep ~ attr8 + attr1 + attr2 + attr4 + attr5, id = ~ID, estimate = "mm", by = ~univ)
mm_univ_JP <- cj(conjoint_JP, dep ~ attr8 + attr1 + attr2 + attr4 + attr5, id = ~ResponseId, estimate = "mm", by = ~univ)
mod3uk <- plot(mm_univ_UK, size = 3, legend_pos = "bottom", legend_title = "", group = "univ",
               vline = 0.5, xlim = c(0.35, 0.9), feature_headers = FALSE) + 
  labs(title = "United Kingdom") +
  facet_grid(feature~., scales = "free_y")

mod3jp <- plot(mm_univ_JP, size = 3, legend_pos = "bottom", legend_title = "", group = "univ",
               vline = 0.5, xlim = c(0.35, 0.9), feature_headers = FALSE) + 
  labs(title = "Japan") +
  facet_grid(feature~., scales = "free_y")
mod3 <- mod3uk + mod3jp
mod3
ggsave("moderator3.png", plot = mod3, dpi = 400, width = 7, height = 6)
ggsave("moderator3.pdf", plot = mod3, dpi = 400, width = 7, height = 6)

## B.4 Moderator Ideology (Figure S4)----
mm_Ide_UK <- cj(conjoint_UK, dep ~ attr8 + attr1 + attr2 + attr4 + attr5, id = ~ID, estimate = "mm", by = ~Ide)
mm_Ide_JP <- cj(conjoint_JP, dep ~ attr8 + attr1 + attr2 + attr4 + attr5, id = ~ResponseId, estimate = "mm", by = ~Ide)
mod4uk <- plot(mm_Ide_UK, size = 3, legend_pos = "bottom", legend_title = "", group = "Ide",
               vline = 0.5, xlim = c(0.35, 0.9), feature_headers = FALSE) + 
  labs(title = "United Kingdom") +
  facet_grid(feature~., scales = "free_y")

mod4jp <- plot(mm_Ide_JP, size = 3, legend_pos = "bottom", legend_title = "", group = "Ide",
               vline = 0.5, xlim = c(0.35, 0.9), feature_headers = FALSE) + 
  labs(title = "Japan") +
  facet_grid(feature~., scales = "free_y")
mod4 <- mod4uk + mod4jp
mod4
ggsave("moderator4.png", plot = mod4, dpi = 400, width = 7, height = 6)
ggsave("moderator4.pdf", plot = mod4, dpi = 400, width = 7, height = 6)

## B.5 Moderator trust (Figure S5)----
mm_trust_UK <- cj(conjoint_UK, dep ~ attr8 + attr1 + attr2 + attr4 + attr5, id = ~ID, estimate = "mm", by = ~G_trust)
mm_trust_JP <- cj(conjoint_JP, dep ~ attr8 + attr1 + attr2 + attr4 + attr5, id = ~ResponseId, estimate = "mm", by = ~G_trust)
mod5uk <- plot(mm_trust_UK, size = 3, legend_pos = "bottom", legend_title = "", group = "G_trust",
               vline = 0.5, xlim = c(0.35, 0.9), feature_headers = FALSE) + 
  labs(title = "United Kingdom") +
  facet_grid(feature~., scales = "free_y")

mod5jp <- plot(mm_trust_JP, size = 3, legend_pos = "bottom", legend_title = "", group = "G_trust",
               vline = 0.5, xlim = c(0.35, 0.9), feature_headers = FALSE) + 
  labs(title = "Japan") +
  facet_grid(feature~., scales = "free_y")
mod5 <- mod5uk + mod5jp
mod5
ggsave("moderator5.png", plot = mod5, dpi = 400, width = 7, height = 6)
ggsave("moderator5.pdf", plot = mod5, dpi = 400, width = 7, height = 6)


# C Examining potential for ceiling effects (Figure S6)----
count_JP <- conjoint_JP %>% 
  select(ResponseId, dep) %>% 
  group_by(ResponseId) %>% 
  summarise(Count = sum(dep))

count_UK <- conjoint_UK %>% 
  select(ID, dep) %>% 
  group_by(ID) %>% 
  summarise(Count = sum(dep))

C1uk <- ggplot(count_UK) +
  geom_histogram(aes(x = Count)) +
  labs(x = "", y = "", title = "United Kingdom")

C1jp <- ggplot(count_JP) +
  geom_histogram(aes(x = Count)) +
  labs(x = "", y = "", title = "Japan")

C1 <- C1uk + C1jp
C1

# D How trial number affects support for visas----
## D.1 Regression table (Table S2)----
count_JP <- left_join(count_JP, trust_JP, by = "ResponseId")
count_UK <- left_join(count_UK, trust_UK, by = "ID")

count_JP <- count_JP %>% 
  mutate(G_trust = ifelse(trust_JP_core_2 %in% c(1,2,3), 0, 1)) %>% 
  mutate(G_trust = factor(G_trust, labels = c("Low trust", "High trust"))) %>% 
  mutate(Ide = case_when(Ideology %in% c(0,1,2,3,4) ~ 1,
                         Ideology == 5 ~ 2,
                         Ideology %in% c(6,7,8,9,10) ~ 3)) %>% 
  mutate(Ide = factor(Ide, labels = c("Left", "Middle", "Right"))) %>% 
  mutate(gov_t = trust_JP_core_1_1 + trust_JP_core_1_2 + trust_JP_core_1_3 + trust_JP_core_1_4) %>% 
  mutate(gov_t2 = case_when(gov_t < 10 ~ 1,
                            gov_t > 9 & gov_t <19 ~ 2,
                            gov_t > 18 ~ 3)) %>% 
  mutate(gov_t2 = factor(gov_t2, labels = c("Low gov trust", "Middle gov trust", "High gov trust"))) %>% 
  mutate(gender = factor(gender, levels = c(1,2), labels = c("Male", "Female"))) %>% 
  mutate(univ = case_when(education < 4 ~ 0,
                          education > 3 & education < 7 ~ 1,
                          education == 7 ~ 0)) %>% 
  mutate(univ = factor(univ, labels = c("Without university degree", "University degree or above"))) %>% 
  mutate(old = ifelse(age < 46, 0, 1)) %>% 
  mutate(old = factor(old, labels = c("Young", "Old")))


count_UK <- count_UK %>% 
  mutate(G_trust = ifelse(trust_UK_core_2 %in% c(1,2,3), 0, 1)) %>% 
  mutate(G_trust = factor(G_trust, labels =c("Low trust", "High trust"))) %>% 
  mutate(Ide = case_when(Ideology %in% c(0,1,2,3,4) ~ 1,
                         Ideology == 5 ~ 2,
                         Ideology %in% c(6,7,8,9,10) ~ 3)) %>% 
  mutate(Ide = factor(Ide, labels = c("Left", "Middle", "Right"))) %>% 
  mutate(gov_t = trust_UK_core_1_1 + trust_UK_core_1_2 + trust_UK_core_1_3 + trust_UK_core_1_4) %>% 
  mutate(gov_t2 = case_when(gov_t < 10 ~ 1,
                            gov_t > 9 & gov_t <19 ~ 2,
                            gov_t > 18 ~ 3)) %>% 
  mutate(gov_t2 = factor(gov_t2, labels = c("Low gov trust", "Middle gov trust", "High gov trust"))) %>% 
  mutate(gender = factor(gender, labels = c("Male", "Female"))) %>% 
  mutate(univ = case_when(education < 15 ~ 0,
                          education > 14 & education <19 ~ 1,
                          education > 18 ~NA))%>% 
  mutate(univ = factor(univ, labels = c("Without university degree", "University degree or above")))%>% 
  mutate(old = ifelse(age < 46, 0, 1)) %>% 
  mutate(old = factor(old, labels = c("Young", "Old")))

modelDuk <- lm(Count ~ gender + age + univ + Ideology + trust_UK_core_2, data = count_UK)
modelDjp <- lm(Count ~ gender + age + univ + Ideology + trust_JP_core_2, data = count_JP)

reg <- list()
reg[["UK"]] <- modelDuk
reg[["JP"]] <- modelDjp

var_nam = c("(Intercept)" = "Intercept", "genderFemale" = "Female", "age" = "Age",
            "univUniversity degree or above" = "University", "Ideology" = "Ideology",
            "trust_UK_core_2" ="Trust", "trust_JP_core_2" ="Trust")
reg_table <- msummary(reg, coef_map = var_nam, 
                      gof_omit = "R2 Adj.|AIC|BIC|RMSE",
                      stars = TRUE,
                      output = "gt")
reg_table
reg_table %>% 
  opt_table_font(font = "Times New Roman") %>% 
  gtsave("Additional2.pdf", expand = 10)



## D.2 Figure 2 with additional control (Figure S7)----
### 2-UK
model_skill_UK <- lm_robust(dep ~ attr6 + attr1 + attr2 + attr4 + attr5 + gender + age + univ + Ideology + trust_UK_core_2, 
                            clusters = ID, se_type = "stata",
                            data = conjoint_UK)

emm_list <- list(
  attr1 = emmeans(model_skill_UK, ~ attr1),
  attr2 = emmeans(model_skill_UK, ~ attr2),
  attr4 = emmeans(model_skill_UK, ~ attr4),
  attr5 = emmeans(model_skill_UK, ~ attr5),
  attr6 = emmeans(model_skill_UK, ~ attr6)
)

emm_df <- bind_rows(
  lapply(names(emm_list), function(name) {
    df <- as.data.frame(emm_list[[name]])
    df$level <- df[[name]]       
    df[[name]] <- NULL           
    df$feature <- name           
    df
  })
)
emm_df <- emm_df %>% 
  mutate(feature = case_when(feature == "attr1" ~ "Age",
                             feature == "attr2" ~ "Gender",
                             feature == "attr4" ~ "Country of origin",
                             feature == "attr5" ~ "Length of stay",
                             feature == "attr6" ~ "Skill")) %>% 
  mutate(feature = factor(feature, levels = c("Skill", "Age", "Gender", "Country of origin", "Length of stay"))) %>% 
  mutate(level = factor(level, levels = c("Low skilled","High skilled",
                                          "24 years old","44 years old","64 years old", 
                                          "Male","Female",
                                          "India", "Australia", "Poland", "Nigeria", 
                                          "Long", "Short")))

fig2uk <- ggplot(emm_df, aes(x = emmean, y = level)) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL), height = 0.25, alpha = 0.4) +
  geom_point(size = 3, color = "black") +
  facet_grid(feature ~ ., scales = "free_y") +
  labs(title = "United Kingdom", x = "Marginal Mean", y = "") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  xlim(c(0.35, 0.9))

fig2uk

## 2-JP
model_skill_JP <- lm_robust(dep ~ attr6 + attr1 + attr2 + attr4 + attr5 + gender + age + univ + Ideology + trust_JP_core_2, 
                            clusters = ResponseId, se_type = "stata",
                            data = conjoint_JP)
emm_list <- list(
  attr1 = emmeans(model_skill_JP, ~ attr1),
  attr2 = emmeans(model_skill_JP, ~ attr2),
  attr4 = emmeans(model_skill_JP, ~ attr4),
  attr5 = emmeans(model_skill_JP, ~ attr5),
  attr6 = emmeans(model_skill_JP, ~ attr6)
)

emm_df <- bind_rows(
  lapply(names(emm_list), function(name) {
    df <- as.data.frame(emm_list[[name]])
    df$level <- df[[name]]   
    df[[name]] <- NULL       
    df$feature <- name       
    df
  })
)
emm_df <- emm_df %>% 
  mutate(feature = case_when(feature == "attr1" ~ "Age",
                             feature == "attr2" ~ "Gender",
                             feature == "attr4" ~ "Country of origin",
                             feature == "attr5" ~ "Length of stay",
                             feature == "attr6" ~ "Skill")) %>% 
  mutate(feature = factor(feature, levels = c("Skill", "Age", "Gender", "Country of origin", "Length of stay"))) %>% 
  mutate(level = factor(level, levels = c("Low skilled","High skilled",
                                          "24 years old","44 years old","64 years old", 
                                          "Male","Female",
                                          "Peru", "China", "Korea", "Viet Num", 
                                          "Long", "Short")))

fig2jp <- ggplot(emm_df, aes(x = emmean, y = level)) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL), height = 0.25, alpha = 0.4) +
  geom_point(size = 3, color = "black") +
  facet_grid(feature ~ ., scales = "free_y") +
  labs(title = "Japan", x = "Marginal Mean", y = "") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  xlim(c(0.35, 0.9))

fig2jp
fig2 <- fig2uk + fig2jp
fig2
ggsave("Fig2_with_control.pdf", plot = fig2, dpi = 400)



## D.3 Figure 3 with additional control (Figure S8)----
### 3-UK
model_skill_UK3 <- lm_robust(dep ~ attr7 + attr1 + attr2 + attr4 + attr5 + gender + age + univ + Ideology + trust_UK_core_2, 
                             clusters = ID, se_type = "stata",
                             data = conjoint_UK)

emm_list <- list(
  attr1 = emmeans(model_skill_UK3, ~ attr1),
  attr2 = emmeans(model_skill_UK3, ~ attr2),
  attr4 = emmeans(model_skill_UK3, ~ attr4),
  attr5 = emmeans(model_skill_UK3, ~ attr5),
  attr7 = emmeans(model_skill_UK3, ~ attr7)
)

emm_df <- bind_rows(
  lapply(names(emm_list), function(name) {
    df <- as.data.frame(emm_list[[name]])
    df$level <- df[[name]]   
    df[[name]] <- NULL       
    df$feature <- name       
    df
  })
)
emm_df <- emm_df %>% 
  mutate(feature = case_when(feature == "attr1" ~ "Age",
                             feature == "attr2" ~ "Gender",
                             feature == "attr4" ~ "Country of origin",
                             feature == "attr5" ~ "Length of stay",
                             feature == "attr7" ~ "Demand")) %>% 
  mutate(feature = factor(feature, levels = c("Demand", "Age", "Gender", "Country of origin", "Length of stay"))) %>% 
  mutate(level = factor(level, levels = c("Low demand","High demand",
                                          "24 years old","44 years old","64 years old", 
                                          "Male","Female",
                                          "India", "Australia", "Poland", "Nigeria", 
                                          "Long", "Short")))

fig3uk <- ggplot(emm_df, aes(x = emmean, y = level)) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL), height = 0.25, alpha = 0.4) +
  geom_point(size = 3, color = "black") +
  facet_grid(feature ~ ., scales = "free_y") +
  labs(title = "United Kingdom", x = "Marginal Mean", y = "") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  xlim(c(0.35, 0.9))

fig3uk

### 3-JP
model_skill_JP3 <- lm_robust(dep ~ attr7 + attr1 + attr2 + attr4 + attr5 + gender + age + univ + Ideology + trust_JP_core_2, 
                             clusters = ResponseId, se_type = "stata",
                             data = conjoint_JP)

emm_list <- list(
  attr1 = emmeans(model_skill_JP3, ~ attr1),
  attr2 = emmeans(model_skill_JP3, ~ attr2),
  attr4 = emmeans(model_skill_JP3, ~ attr4),
  attr5 = emmeans(model_skill_JP3, ~ attr5),
  attr7 = emmeans(model_skill_JP3, ~ attr7)
)

emm_df <- bind_rows(
  lapply(names(emm_list), function(name) {
    df <- as.data.frame(emm_list[[name]])
    df$level <- df[[name]]    
    df[[name]] <- NULL        
    df$feature <- name        
    df
  })
)
emm_df <- emm_df %>% 
  mutate(feature = case_when(feature == "attr1" ~ "Age",
                             feature == "attr2" ~ "Gender",
                             feature == "attr4" ~ "Country of origin",
                             feature == "attr5" ~ "Length of stay",
                             feature == "attr7" ~ "Demand")) %>% 
  mutate(feature = factor(feature, levels = c("Demand", "Age", "Gender", "Country of origin", "Length of stay"))) %>% 
  mutate(level = factor(level, levels = c("Low demand","High demand",
                                          "24 years old","44 years old","64 years old", 
                                          "Male","Female",
                                          "Peru", "China", "Korea", "Viet Num", 
                                          "Long", "Short")))

fig3jp <- ggplot(emm_df, aes(x = emmean, y = level)) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL), height = 0.25, alpha = 0.4) +
  geom_point(size = 3, color = "black") +
  facet_grid(feature ~ ., scales = "free_y") +
  labs(title = "Japan", x = "Marginal Mean", y = "") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  xlim(c(0.35, 0.9))

fig3jp

fig3 <- fig3uk + fig3jp
fig3
ggsave("Fig3_with_control.pdf", plot = fig3, dpi = 400)

## D.4 Figure 4 with additional control (Figure S9)----
### 4-UK
model_skill_UK4 <- lm_robust(dep ~ attr8 + attr1 + attr2 + attr4 + attr5 + gender + age + univ + Ideology + trust_UK_core_2, 
                             clusters = ID, se_type = "stata",
                             data = conjoint_UK)

emm_list <- list(
  attr1 = emmeans(model_skill_UK4, ~ attr1),
  attr2 = emmeans(model_skill_UK4, ~ attr2),
  attr4 = emmeans(model_skill_UK4, ~ attr4),
  attr5 = emmeans(model_skill_UK4, ~ attr5),
  attr8 = emmeans(model_skill_UK4, ~ attr8)
)

emm_df <- bind_rows(
  lapply(names(emm_list), function(name) {
    df <- as.data.frame(emm_list[[name]])
    df$level <- df[[name]]        # 属性の列を "level" にコピー
    df[[name]] <- NULL            # 元の列名を削除
    df$feature <- name            # 属性名を記録
    df
  })
)
emm_df <- emm_df %>% 
  mutate(feature = case_when(feature == "attr1" ~ "Age",
                             feature == "attr2" ~ "Gender",
                             feature == "attr4" ~ "Country of origin",
                             feature == "attr5" ~ "Length of stay",
                             feature == "attr8" ~ "Skill * Demand")) %>% 
  mutate(feature = factor(feature, levels = c("Skill * Demand", "Age", "Gender", "Country of origin", "Length of stay"))) %>% 
  mutate(level = factor(level, levels = c("L-S + L-D","L-S + H-D", "H-S + L-D", "H-S + H-D",
                                          "24 years old","44 years old","64 years old", 
                                          "Male","Female",
                                          "India", "Australia", "Poland", "Nigeria", 
                                          "Long", "Short")))

fig4uk <- ggplot(emm_df, aes(x = emmean, y = level)) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL), height = 0.25, alpha = 0.4) +
  geom_point(size = 3, color = "black") +
  facet_grid(feature ~ ., scales = "free_y") +
  labs(title = "United Kingdom", x = "Marginal Mean", y = "") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  xlim(c(0.35, 0.9))

fig4uk

### JP-4
model_skill_JP4 <- lm_robust(dep ~ attr8 + attr1 + attr2 + attr4 + attr5 + gender + age + univ + Ideology + trust_JP_core_2, 
                             clusters = ResponseId, se_type = "stata",
                             data = conjoint_JP)

emm_list <- list(
  attr1 = emmeans(model_skill_JP4, ~ attr1),
  attr2 = emmeans(model_skill_JP4, ~ attr2),
  attr4 = emmeans(model_skill_JP4, ~ attr4),
  attr5 = emmeans(model_skill_JP4, ~ attr5),
  attr8 = emmeans(model_skill_JP4, ~ attr8)
)

emm_df <- bind_rows(
  lapply(names(emm_list), function(name) {
    df <- as.data.frame(emm_list[[name]])
    df$level <- df[[name]]        # 属性の列を "level" にコピー
    df[[name]] <- NULL            # 元の列名を削除
    df$feature <- name            # 属性名を記録
    df
  })
)
emm_df <- emm_df %>% 
  mutate(feature = case_when(feature == "attr1" ~ "Age",
                             feature == "attr2" ~ "Gender",
                             feature == "attr4" ~ "Country of origin",
                             feature == "attr5" ~ "Length of stay",
                             feature == "attr8" ~ "Skill * Demand")) %>% 
  mutate(feature = factor(feature, levels = c("Skill * Demand", "Age", "Gender", "Country of origin", "Length of stay"))) %>% 
  mutate(level = factor(level, levels = c("L-S + L-D","L-S + H-D", "H-S + L-D", "H-S + H-D",
                                          "24 years old","44 years old","64 years old", 
                                          "Male","Female",
                                          "Peru", "China", "Korea", "Viet Num", 
                                          "Long", "Short")))


fig4jp <- ggplot(emm_df, aes(x = emmean, y = level)) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL), height = 0.25, alpha = 0.4) +
  geom_point(size = 3, color = "black") +
  facet_grid(feature ~ ., scales = "free_y") +
  labs(title = "Japan", x = "Marginal Mean", y = "") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  xlim(c(0.35, 0.9))

fig4jp

fig4 <- fig4uk + fig4jp
fig4
ggsave("Fig4_with_control.pdf", plot = fig4, dpi = 400)





## D.5 Marginal means first-half vs second-half
### UK (Figure S10)----
conjoint_UK <- conjoint_UK %>% 
  mutate(qn = str_extract(Question, pattern = "[0-9]"))
conjoint_UK$qn <- as.numeric(conjoint_UK$qn)
conjoint_UK <- conjoint_UK %>% 
  mutate(latter = ifelse(qn > 4, 1, 0))

mm_skill_UK_6_f <- conjoint_UK %>% 
  filter(latter == 0) %>% 
  mm(., dep ~ attr6 + attr1 + attr2 + attr4 + attr5, id = ~ID)
mm_skill_UK_6_l <- conjoint_UK %>% 
  filter(latter == 1) %>% 
  mm(., dep ~ attr6 + attr1 + attr2 + attr4 + attr5, id = ~ID)
df_UK_6_f <- mm_skill_UK_6_f %>% as.data.frame() %>% mutate(group = "First Half") %>% 
  mutate(model = "Skill")
df_UK_6_l <- mm_skill_UK_6_l %>% as.data.frame() %>% mutate(group = "Latter Half") %>% 
  mutate(model = "Skill")

mm_skill_UK_7_f <- conjoint_UK %>% 
  filter(latter == 0) %>% 
  mm(., dep ~ attr7 + attr1 + attr2 + attr4 + attr5, id = ~ID)
mm_skill_UK_7_l <- conjoint_UK %>% 
  filter(latter == 1) %>% 
  mm(., dep ~ attr7 + attr1 + attr2 + attr4 + attr5, id = ~ID)
df_UK_7_f <- mm_skill_UK_7_f %>% as.data.frame() %>% mutate(group = "First Half") %>% 
  mutate(model = "Demand")
df_UK_7_l <- mm_skill_UK_7_l %>% as.data.frame() %>% mutate(group = "Latter Half") %>% 
  mutate(model = "Demand")

mm_skill_UK_8_f <- conjoint_UK %>% 
  filter(latter == 0) %>% 
  mm(., dep ~ attr8 + attr1 + attr2 + attr4 + attr5, id = ~ID)
mm_skill_UK_8_l <- conjoint_UK %>% 
  filter(latter == 1) %>% 
  mm(., dep ~ attr8 + attr1 + attr2 + attr4 + attr5, id = ~ID)
df_UK_8_f <- mm_skill_UK_8_f %>% as.data.frame() %>% mutate(group = "First Half") %>% 
  mutate(model = "Skill * Demand")
df_UK_8_l <- mm_skill_UK_8_l %>% as.data.frame() %>% mutate(group = "Latter Half") %>% 
  mutate(model = "Skill * Demand")

df_UK_all <- bind_rows(df_UK_6_f, df_UK_6_l, df_UK_7_f, df_UK_7_l, df_UK_8_f, df_UK_8_l)

main_UK <- df_UK_all %>% 
  mutate(feature = factor(feature, levels = c("Skill","Demand","Skill * Demand",
                                              "Age", "Gender","Country of origin","Length of stay"))) %>% 
  filter(feature %in% c("Skill","Demand","Skill * Demand")) %>% 
  ggplot(aes(x = estimate, y = level, xmin = lower, xmax = upper, color = group)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbarh(position = position_dodge(width = 0.5), height = 0.2) +
  facet_wrap(.~feature, nrow = 3, scales = "free_y") +
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  labs(x = "Marginal Means", y = "Levels", color = "Subset") +
  theme_bw(base_size = 14) 
main_UK


origin <- df_UK_all %>% 
  mutate(feature = factor(feature, levels = c("Skill","Demand","Skill * Demand",
                                              "Age", "Gender","Country of origin","Length of stay"))) %>% 
  filter(feature == "Country of origin") %>% 
  ggplot(aes(x = estimate, y = level, xmin = lower, xmax = upper, color = group)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbarh(position = position_dodge(width = 0.5), height = 0.2) +
  facet_wrap(.~model, nrow = 3, scales = "free_y") +
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  labs(x = "Marginal Means", y = "Levels", color = "Subset") +
  theme_bw(base_size = 14) 
origin
ggsave("Additional5_3_UK.pdf", plot = origin, dpi = 400)


### JP (Figure S11)----
conjoint_JP <- conjoint_JP %>% 
  mutate(qn = str_extract(Question, pattern = "[0-9]")) 
conjoint_JP$qn <- as.numeric(conjoint_JP$qn)
conjoint_JP <- conjoint_JP %>% 
  mutate(latter = ifelse(qn > 2, 1, 0))

mm_skill_JP_6_f <- conjoint_JP %>% 
  filter(latter == 0) %>% 
  mm(., dep ~ attr6 + attr1 + attr2 + attr4 + attr5, id = ~ResponseId)
mm_skill_JP_6_l <- conjoint_JP %>% 
  filter(latter == 1) %>% 
  mm(., dep ~ attr6 + attr1 + attr2 + attr4 + attr5, id = ~ResponseId)
df_JP_6_f <- mm_skill_JP_6_f %>% as.data.frame() %>% mutate(group = "First Half") %>% 
  mutate(model = "Skill")
df_JP_6_l <- mm_skill_JP_6_l %>% as.data.frame() %>% mutate(group = "Latter Half") %>% 
  mutate(model = "Skill")

mm_skill_JP_7_f <- conjoint_JP %>% 
  filter(latter == 0) %>% 
  mm(., dep ~ attr7 + attr1 + attr2 + attr4 + attr5, id = ~ResponseId)
mm_skill_JP_7_l <- conjoint_JP %>% 
  filter(latter == 1) %>% 
  mm(., dep ~ attr7 + attr1 + attr2 + attr4 + attr5, id = ~ResponseId)
df_JP_7_f <- mm_skill_JP_7_f %>% as.data.frame() %>% mutate(group = "First Half") %>% 
  mutate(model = "Demand")
df_JP_7_l <- mm_skill_JP_7_l %>% as.data.frame() %>% mutate(group = "Latter Half") %>% 
  mutate(model = "Demand")

mm_skill_JP_8_f <- conjoint_JP %>% 
  filter(latter == 0) %>% 
  mm(., dep ~ attr8 + attr1 + attr2 + attr4 + attr5, id = ~ResponseId)
mm_skill_JP_8_l <- conjoint_JP %>% 
  filter(latter == 1) %>% 
  mm(., dep ~ attr8 + attr1 + attr2 + attr4 + attr5, id = ~ResponseId)
df_JP_8_f <- mm_skill_JP_8_f %>% as.data.frame() %>% mutate(group = "First Half") %>% 
  mutate(model = "Skill * Demand")
df_JP_8_l <- mm_skill_JP_8_l %>% as.data.frame() %>% mutate(group = "Latter Half") %>% 
  mutate(model = "Skill * Demand")

df_JP_all <- bind_rows(df_JP_6_f, df_JP_6_l, df_JP_7_f, df_JP_7_l, df_JP_8_f, df_JP_8_l)
main_JP <- df_JP_all %>% 
  mutate(feature = factor(feature, levels = c("Skill","Demand","Skill * Demand",
                                              "Age", "Gender","Country of origin","Length of stay"))) %>% 
  filter(feature %in% c("Skill","Demand","Skill * Demand")) %>% 
  ggplot(aes(x = estimate, y = level, xmin = lower, xmax = upper, color = group)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbarh(position = position_dodge(width = 0.5), height = 0.2) +
  facet_wrap(.~feature, nrow = 3, scales = "free_y") +
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  labs(x = "Marginal Means", y = "Levels", color = "Subset") +
  theme_bw(base_size = 14) 
main_JP

# E Results for High demand attribute only (Figure S11)----
mm_skill_UK <- conjoint_UK %>% 
  filter(attr7 == "High demand") %>% 
  mm(., dep ~ attr6 + attr1 + attr2 + attr4 + attr5, id = ~ID)
mm_skill_JP <- conjoint_JP %>% 
  filter(attr7 == "High demand") %>% 
  mm(., dep ~ attr6 + attr1 + attr2 + attr4 + attr5, id = ~ResponseId)

Euk <- plot(mm_skill_UK, size = 3, legend_pos = "",
               vline = 0.5, xlim = c(0.35, 0.9), feature_headers = FALSE) + 
  labs(title = "United Kingdom") +
  scale_color_manual(values = rep("black", 9)) +
  facet_grid(feature~., scales = "free_y")

Ejp <- plot(mm_skill_JP, size = 3, legend_pos = "",
               vline = 0.5, xlim = c(0.35, 0.9), feature_headers = FALSE) + 
  labs(title = "Japan") +
  scale_color_manual(values = rep("black", 9)) +
  facet_grid(feature~., scales = "free_y")


E <- Euk + Ejp + plot_annotation(title = "Exclude low-demand")
E
