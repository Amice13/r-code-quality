library(tidyverse)
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

# Fig1
mm1 <- mm(df, dep_bi ~ majority + turnout + propose + theme, id = ~ResponseId)


fig1 <- plot(mm1, size = 3, legend_pos = "", feature_headers = FALSE,
             vline = 0.5, xlim = c(0.5, 0.75)) + 
  geom_vline(xintercept = 0.5, size = 1, color = "black") +
  labs(title = "Figure 1 Marginal means of the levels of each attribute") +
  scale_color_manual(values = rep("black", 9)) +
  facet_grid(feature~., scales = "free_y") +
  theme_bw(base_size = 14) +
  theme(legend.position = "")

fig1
ggsave("Fig1.png", plot = fig1, dpi = 400, width = 8, height = 6)



# Fig2
model2 <- lm_robust(dep_bi ~ theme + turnout * majority + propose,
                    clusters = ResponseId, se_type = "stata", data = df)

emm2 <- emmeans(model2, ~ turnout | majority)

emm_df2 <- as.data.frame(emm2)

fig2 <- ggplot(emm_df2, aes(x = emmean, y = turnout)) +
  geom_vline(xintercept = 0.5, size = 1, color = "black") +
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL),
                 height = 0) +
  geom_point(color = "black", size = 3) +
  facet_grid(rows = vars(majority), labeller = label_both) +
  theme_bw(base_size = 14) +
  labs(x = "", y = "", title = "Figure 2 Marginal means of the levels of turnout on the size of majority") 

fig2
ggsave("Fig2.png", plot = fig2, dpi = 400, width = 8, height = 6)


# Fig3----
f3 <- dep_bi ~ majority + turnout + propose
mm3 <- df %>% 
  filter(theme == "Waste dispose") %>% 
  cj(., f3, id = ~ResponseId, estimate = "mm", by = ~rejection)


fig3 <- mm3 %>% 
  ggplot(aes(shape = BY)) +
  geom_hline(yintercept = 0.5, color = "gray") +
  geom_pointrange(aes(x = level, y = estimate, ymin = lower, ymax = upper),
                  size = 1.5, linewidth = 1) +
  coord_flip() +
  facet_grid(feature~., scales = "free_y") +
  theme_bw(base_size = 14) +
  labs(x = "", y = "", title = "Figure 3 Marginal means for two groups", shape = "") +
  theme(legend.position = "bottom")

fig3
ggsave("Fig3.png", plot = fig3, dpi = 400, width = 8, height = 6)

