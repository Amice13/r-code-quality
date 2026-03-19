library(tidyverse)
library(qualtRics)
library(emmeans)
library(estimatr)
library(cregg)

# Setup
df <- read_csv("ref_data.csv")

df <- df %>% 
  mutate(majority = factor(majority)) %>% 
  mutate(turnout = factor(turnout)) %>% 
  mutate(theme = factor(theme, levels = c("City hall", "Waste dispose","Merger", "SDF base"))) %>% 
  mutate(propose = factor(propose, levels = c("Local Assembly","Mayor","Citizen Initiative"))) %>% 
  mutate(rejection = factor(rejection))

# Fig2
f2 <- dep_bi ~ majority * turnout + propose + theme 
plot(mm(df, f2, id = ~ResponseId), vline = 0.5)
mm <- mm(df, f1, id = ~ResponseId)
plot(mm)


# Fig3
model_emm3 <- lm_robust(dep_bi ~ theme + turnout * majority + propose, 
                        clusters = ResponseId, se_type = "stata",
                        data = df)
emm3 <- emmeans(model_emm3, "turnout", by = "majority")
plot(emm3) +
  theme_bw(base_size = 18) +
  geom_vline(xintercept = 0.5, size = 2, color = "red") +
  labs(x = "", y = "")

# Fig4
f4 <- dep_bi ~ majority * turnout + propose
mm_by <- df %>% 
  filter(theme == "Waste dispose") %>% 
  cj(., f4, id = ~ResponseId, estimate = "mm", by = ~rejection)
plot(mm_by, group = "rejection", vline = 0.5, legend_title = "")


# Fig5
model_emm5 <- df %>% 
  filter(theme == "Waste dispose") %>% 
  filter(rejection == "Disagree") %>% 
  lm_robust(dep_bi ~ turnout * majority + propose, 
            clusters = ResponseId, se_type = "stata",
            data = .)
emm5 <- emmeans(model_emm5, "turnout", by = "majority")
plot(emm5) +
  theme_bw(base_size = 18) +
  geom_vline(xintercept = 0.5, size = 2, color = "red") +
  labs(x = "", y = "")


