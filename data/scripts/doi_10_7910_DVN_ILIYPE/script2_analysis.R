#---- SCRIPT 2/2 ----
#---- ANALYSIS ----
#install.packages("pacman")
remove(list=ls())

library(pacman)
set.seed(234)



p_load(here, rio, ggpubr, dplyr, jtools, estimatr, stargazer, ggdist, tidytext,
       modelsummary, forcats, Hmisc, scales, stringr, cregg, haven, viridis)

df <- readRDS("data/analysisdata.rds")



#### ---- Main analyses -----

###### Main analysis: MMs of success

mm1 <- cj(Q2 ~ occupation + housing + p_occupation + region
          + education + gender + age + spouse_occupation, 
          data = df,
          estimate = "mm",
          feature_labels = list("occupation" = "Occupation", 
                                "housing" = "Housing",
                                "p_occupation" = "Parental \n Occupation",
                                "region" = "Region",
                                "education" = "Education", 
                                "gender" = "Gender",
                                "age" = "Age",
                                "spouse_occupation" = "Spousal \n Occupation"))

amce1 <- cj(Q2 ~ occupation + housing + p_occupation + region
            + education + gender + age + spouse_occupation, 
            data = df,
            feature_labels = list("occupation" = "Occupation", 
                                  "housing" = "Housing",
                                  "p_occupation" = "Parental \n Occupation",
                                  "region" = "Region",
                                  "education" = "Education", 
                                  "gender" = "Gender",
                                  "age" = "Age",
                                  "spouse_occupation" = "Spousal \n Occupation"))

esttab_amce <- select(amce1,
                      "Feature" = feature,
                      "Level" = level,
                      "Est." = estimate,
                      "SE" = std.error,
                      "LowerCI" = lower,
                      "UpperCI" = upper)

export_amce <- bind_cols(esttab_amce) %>%
  arrange(as.numeric(Feature), -as.numeric(Level)) %>%
  select(Level, Est., SE, LowerCI, UpperCI)

xtable::xtable(export_amce, 
               caption = "AMCE", 
               digits = 3) -> esttab_amce

xtable::print.xtable(esttab_amce, 
                     include.rownames = F,
                     type = "latex",
                     file = "outputs_appendix/Table9.tex")

esttab <- select(mm1,
                 "Feature" = feature,
                 "Level" = level,
                 "Est." = estimate,
                 "SE" = std.error,
                 "LowerCI" = lower,
                 "UpperCI" = upper)
export_mm <- bind_cols(esttab) %>%
  arrange(as.numeric(Feature), -as.numeric(Level)) %>%
  select(Level, Est., SE, LowerCI, UpperCI)

xtable::xtable(export_mm, 
               caption = "Marginal means", 
               digits = 3) -> esttab
xtable::print.xtable(esttab, 
                     include.rownames = F,
                     type = "latex",
                     file = "outputs_appendix/Table8.tex")

mm1$lower95 <- mm1$estimate - (1.96 * mm1$std.error)
mm1$higher95 <- mm1$estimate + (1.96 * mm1$std.error)
mm1$lower99 <- mm1$estimate - (2.58 * mm1$std.error)
mm1$higher99 <- mm1$estimate + (2.58 * mm1$std.error)
mm1$level <- gsub("^Occ: ", "", mm1$level)
mm1$level <- gsub("^BG: ", "", mm1$level)
mm1$level <- gsub("^SP: ", "", mm1$level)

amce1$lower95 <- amce1$estimate - (1.96 * amce1$std.error)
amce1$higher95 <- amce1$estimate + (1.96 * amce1$std.error)
amce1$lower99 <- amce1$estimate - (2.58 * amce1$std.error)
amce1$higher99 <- amce1$estimate + (2.58 * amce1$std.error)
amce1$level <- gsub("^Occ: ", "", amce1$level)
amce1$level <- gsub("^BG: ", "", amce1$level)
amce1$level <- gsub("^SP: ", "", amce1$level)


mm1 %>% 
  filter(feature %nin% c("Gender", "Age")) %>%
  ggplot(aes(reorder_within(level, estimate, feature), color = feature)) +
  geom_vline(xintercept = 4.794656, color = "grey10") +
  geom_segment(aes(x = lower95, xend = higher95, y = reorder_within(level, estimate, feature), yend = reorder_within(level, estimate, feature)), size = 3, alpha = .8) + 
  geom_segment(aes(x = lower99, xend = higher99, y = reorder_within(level, estimate, feature), yend = reorder_within(level, estimate, feature)), size = 2, alpha = .6) +
  facet_grid(feature ~ ., scales = 'free', space = 'free') +  
  geom_point(aes(x = estimate, y = reorder_within(level, estimate, feature)), fill = "white", color = "grey67", size = 2.5, pch = 21) +
  labs(y = NULL, x = 'Marginal means (0-10)', caption = "Confidence intervals at 95% and 99%") +
  theme_classic() +
  theme(
    strip.placement = 'outside',
    panel.background = element_rect(color = 'black'),
    legend.position = "none",
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    axis.text.y = element_text(face = "bold", size = 10)) +
  scale_fill_viridis(discrete = TRUE, option = "plasma", begin = 0.15, end = 0.8, direction = 1) +
  scale_color_viridis(discrete = TRUE, option = "plasma", begin = 0.15, end = 0.8, direction = 1) +
  scale_y_reordered() +
  geom_text(
    aes(label = sprintf("%0.1f", round(estimate, digits = 1)), y = reorder_within(level, estimate, feature), x = 3.65),
    position = position_dodge(width = 0.9), size = 5, fontface = "bold")

ggsave("Figure2.png", 
       path = here("outputs_main"),
       dpi = 500, height = 25, width = 21, units = "cm")



###### Predicted probabilities from most and least privileged

new_data <- data.frame(occupation = "Occ: Supermarket employee", ## Working-class
                       housing = "Rents local authority",
                       p_occupation = "BG: Supermarket employee",
                       region = "Scotland",
                       education = "Left at 16",
                       gender = "Male",
                       age = "79",
                       spouse_occupation = "SP: Supermarket employee")

new_data2 <- data.frame(occupation = "Occ: Barrister", ## Privileged
                        housing = "Owns outright",
                        p_occupation = "BG: Barrister",
                        region = "S. East",
                        education = "Oxford",
                        gender = "Male",
                        age = "57",
                        spouse_occupation = "SP: Barrister")

new_data3 <- data.frame(occupation = "Occ: Barrister", ## Upward mobility
                        housing = "Owns outright",
                        p_occupation = "BG: Supermarket employee",
                        region = "Scotland",
                        education = "Oxford",
                        gender = "Male",
                        age = "57",
                        spouse_occupation = "SP: Barrister")

new_data4 <- data.frame(occupation = "Occ: Supermarket employee", ## Downward mobility
                        housing = "Rents private",
                        p_occupation = "BG: Barrister",
                        region = "London",
                        education = "Left at 16",
                        gender = "Male",
                        age = "57",
                        spouse_occupation = "SP: Supermarket employee")


predmod <- lm(Q2 ~ occupation + housing + p_occupation + region
              + education + gender + age + spouse_occupation, data = df)

confidence_level <- 0.95

predicted_value_wc <- predict(predmod, newdata = new_data, interval = "confidence", level = 0.95)
predicted_value_uc <- predict(predmod, newdata = new_data2, interval = "confidence", level = 0.95)
predicted_value_um <- predict(predmod, newdata = new_data3, interval = "confidence", level = 0.95)
predicted_value_dm <- predict(predmod, newdata = new_data4, interval = "confidence", level = 0.95)
lower_bound1 <- predicted_value_wc[, "lwr"]
upper_bound1 <- predicted_value_wc[, "upr"]

lower_bound2 <- predicted_value_uc[, "lwr"]
upper_bound2 <- predicted_value_uc[, "upr"]

lower_bound3 <- predicted_value_um[, "lwr"]
upper_bound3 <- predicted_value_um[, "upr"]

lower_bound4 <- predicted_value_dm[, "lwr"]
upper_bound4 <- predicted_value_dm[, "upr"]

wcmargins <- data.frame(value = predicted_value_wc[, "fit"],
                        lower = predicted_value_wc[, "lwr"],
                        upper = predicted_value_wc[, "upr"],
                        class = "Least privileged")

ucmargins <- data.frame(value = predicted_value_uc[, "fit"],
                        lower = predicted_value_uc[, "lwr"],
                        upper = predicted_value_uc[, "upr"],
                        class = "Most privileged")

upmobilitymargins <- data.frame(value = predicted_value_um[, "fit"],
                                lower = predicted_value_um[, "lwr"],
                                upper = predicted_value_um[, "upr"],
                                class = "Upward mobility")

downmobilitymargins <- data.frame(value = predicted_value_dm[, "fit"],
                                  lower = predicted_value_dm[, "lwr"],
                                  upper = predicted_value_dm[, "upr"],
                                  class = "Downward mobility")

plot_df <- rbind(wcmargins, ucmargins, upmobilitymargins, downmobilitymargins)

plot_df$class <- factor(plot_df$class, levels=c("Most privileged", "Least privileged", 
                                                "Upward mobility", "Downward mobility"))



plot_df %>% 
  mutate(class = fct_reorder(class, value)) %>%
  ggplot(aes(x = class, y = value, group = 1, color = class)) +
  geom_point(size = 2) +
  theme_ggdist() +
  geom_linerange(aes(ymin = lower, ymax = upper), size = 2, alpha = 0.5) +
  labs(y = "Perceived chances of success\n(0 = Very unlikely, 10 = Very likely)",
       x = "",
       title = "",
       caption = "Confidence intervals at 95%") +
  ylim(0, 10) +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 7),
    plot.caption = element_text(size = 5)) +
  geom_hline(yintercept = 4.698098, alpha = 0.3, linetype = "dashed") +
  annotate("text", x = 2.5, y = 4.9, label = "Average predicted chance", alpha = 0.5, size = 2, angle = 90) +
  scale_fill_viridis(discrete = TRUE, option = "plasma", begin = 0.15, end = 0.8, direction = 1) +  
  scale_color_viridis(discrete = TRUE, option = "plasma", begin = 0.15, end = 0.8, direction = 1) +
  geom_text(
    aes(label = sprintf("%0.1f", value), x = class),
    position = position_dodge(width = 0.9), hjust = 2, size = 2, fontface = "bold") +
  coord_flip()

ggsave("Figure3.png", 
       path = here("outputs_main"),
       dpi = 500, height = 6, width = 12, units = "cm")




####SUBGROUP ANALYSIS###
# Class
df <- df %>% mutate(
  class = case_when(class == 1 ~ "DE",
                    class == 2 ~ "C",
                    class == 3 ~ "AB")
)

df$class <- as.factor(df$class)

mm_class4 <- cj(Q2 ~ occupation + housing + p_occupation + region
                + education + gender + age + spouse_occupation, 
                data = df,
                estimate = "mm",
                by = ~class,
                feature_labels = list("occupation" = "Occupation", 
                                      "housing" = "Housing",
                                      "p_occupation" = "Parental \n Occupation",
                                      "region" = "Region",
                                      "education" = "Education", 
                                      "gender" = "Gender",
                                      "age" = "Age",
                                      "spouse_occupation" = "Spousal \n Occupation"))


mm_class4 %>% 
  ggplot(aes(level)) +
  geom_hline(yintercept = 4.794656, color="grey10") +
  geom_crossbar(aes(y=estimate, ymin= lower, ymax = upper, fill = class),
                width=0.7, position = position_dodge(width = 0.9), alpha = 0.5) +
  facet_grid(feature~., scales='free', space='free') +
  coord_flip() +
  labs(x=NULL, y='Marginal means') +
  theme_classic() +
  theme(
    strip.placement = 'outside',
    #panel.spacing = unit(0,'pt'),
    panel.background = element_rect(color='black'),
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.background= element_blank(),
    strip.text = element_text(size = 12),
    axis.text.y = element_text(face="bold", size = 10)) +
  scale_fill_viridis(discrete = TRUE, option = "plasma", 
                     begin = 0.15, 
                     end = 0.8, direction = 1) +  
  scale_color_viridis(discrete = TRUE, option = "plasma", 
                      begin = 0.15,
                      end = 0.8, direction = 1)

ggsave("Figure9.png", 
       path = here("outputs_appendix"),
       dpi=500, height=31, width=21, units="cm")


# Education

df <- df %>% mutate(resp_educ = case_when(resp_educ == 1 ~ "None to GCSE",
                                                resp_educ == 2 ~ "A Level or Technical",
                                                resp_educ == 3 ~ "Degree or above"))
df$resp_educ <- as.factor(df$resp_educ)

mm_class5 <- cj(Q2 ~ occupation + housing + p_occupation + region
                + education + gender + age + spouse_occupation, 
                data = df,
                estimate = "mm",
                by = ~resp_educ,
                feature_labels = list("occupation" = "Occupation", 
                                      "housing" = "Housing",
                                      "p_occupation" = "Parental \n Occupation",
                                      "region" = "Region",
                                      "education" = "Education", 
                                      "gender" = "Gender",
                                      "age" = "Age",
                                      "spouse_occupation" = "Spousal \n Occupation"))

mm_class5 %>% 
  ggplot(aes(level)) +
  geom_hline(yintercept = 4.794656, color="grey10") +
  geom_crossbar(aes(y=estimate, ymin= lower, ymax = upper, fill = resp_educ),
                width=0.7, position = position_dodge(width = 0.9), alpha = 0.5) +
  facet_grid(feature~., scales='free', space='free') +
  coord_flip() +
  labs(x=NULL, y='Marginal means') +
  theme_classic() +
  theme(
    strip.placement = 'outside',
    #panel.spacing = unit(0,'pt'),
    panel.background = element_rect(color='black'),
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.background= element_blank(),
    strip.text = element_text(size = 12),
    axis.text.y = element_text(face="bold", size = 10)) +
  scale_fill_viridis(discrete = TRUE, option = "plasma", 
                     begin = 0.15, 
                     end = 0.8, direction = 1) +  
  scale_color_viridis(discrete = TRUE, option = "plasma", 
                      begin = 0.15,
                      end = 0.8, direction = 1)

ggsave("Figure10.png", 
       path = here("outputs_appendix"),
       dpi=500, height=31, width=21, units="cm")


# Income

df <- df %>% mutate(income = case_when(income == 1 ~ "Lowest tertile",
                                             income == 2 ~ "Middle tertile",
                                             income == 3 ~ "Upper tertile"))
df$income <- as.factor(df$income)

mm_class6 <- cj(Q2 ~ occupation + housing + p_occupation + region
                + education + gender + age + spouse_occupation, 
                data = df,
                estimate = "mm",
                by = ~income,
                feature_labels = list("occupation" = "Occupation", 
                                      "housing" = "Housing",
                                      "p_occupation" = "Parental \n Occupation",
                                      "region" = "Region",
                                      "education" = "Education", 
                                      "gender" = "Gender",
                                      "age" = "Age",
                                      "spouse_occupation" = "Spousal \n Occupation"))

mm_class6 %>% 
  ggplot(aes(level)) +
  geom_hline(yintercept = 4.794656, color="grey10") +
  geom_crossbar(aes(y=estimate, ymin= lower, ymax = upper, fill = income),
                width=0.7, position = position_dodge(width = 0.9), alpha = 0.5) +
  facet_grid(feature~., scales='free', space='free') +
  coord_flip() +
  labs(x=NULL, y='Marginal means') +
  theme_classic() +
  theme(
    strip.placement = 'outside',
    #panel.spacing = unit(0,'pt'),
    panel.background = element_rect(color='black'),
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.background= element_blank(),
    strip.text = element_text(size = 12),
    axis.text.y = element_text(face="bold", size = 10)) +
  scale_fill_viridis(discrete = TRUE, option = "plasma", 
                     begin = 0.15, 
                     end = 0.8, direction = 1) +  
  scale_color_viridis(discrete = TRUE, option = "plasma", 
                      begin = 0.15,
                      end = 0.8, direction = 1)
ggsave("Figure11.png", 
       path = here("outputs_appendix"),
       dpi=500, height=31, width=21, units="cm")


# Party
df <- df %>% mutate(vote = case_when(vote == 1 ~ "Conservative",
                                           vote == 2 ~ "Labour",
                                           vote == 3 ~ "Liberal Democrat",
                                           vote == 4 ~ "Other"))
df$vote <- as.factor(df$vote)
party3 <- subset(df, vote != "Other")

mm_class7 <- cj(Q2 ~ occupation + housing + p_occupation + region
                + education + gender + age + spouse_occupation, 
                data = df,
                estimate = "mm",
                by = ~vote,
                feature_labels = list("occupation" = "Occupation", 
                                      "housing" = "Housing",
                                      "p_occupation" = "Parental \n Occupation",
                                      "region" = "Region",
                                      "education" = "Education", 
                                      "gender" = "Gender",
                                      "age" = "Age",
                                      "spouse_occupation" = "Spousal \n Occupation"))

mm_class7 %>% 
  ggplot(aes(level)) +
  geom_hline(yintercept = 4.794656, color="grey10") +
  geom_crossbar(aes(y=estimate, ymin= lower, ymax = upper, fill = vote, color=vote),
                width=0.7, position = position_dodge(width = 0.9), alpha = 0.5) +
  facet_grid(feature~., scales='free', space='free') +
  coord_flip() +
  labs(x=NULL, y='Marginal means') +
  theme_classic() +
  theme(
    strip.placement = 'outside',
    #panel.spacing = unit(0,'pt'),
    panel.background = element_rect(color='black'),
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.background= element_blank(),
    strip.text = element_text(size = 12),
    axis.text.y = element_text(face="bold", size = 10)) +
  scale_fill_viridis(discrete = TRUE, option = "plasma", 
                     begin = 0.15, 
                     end = 0.8, direction = 1) +  
  scale_color_viridis(discrete = TRUE, option = "plasma", 
                      begin = 0.15,
                      end = 0.8, direction = 1)

ggsave("Figure12.png", 
       path = here("outputs_appendix"),
       dpi=500, height=31, width=21, units="cm")



###MISC FIGURES ON TRAITS###

p_load(here, rio, ggpubr, dplyr, jtools,modelsummary, Hmisc, viridis, tidyr, corrplot, GGally)



df_traits1 <- import(here("data/misc", "traits1.dta"))
df_traits1 <- df_traits1 %>%
  mutate(across(c(var368, var369, var370, var371, var372), ~ case_when(
    . == 2 ~ 1,
    . == 3 ~ 2,
    . == 4 ~ 3,
    . == 5 ~ 4,
    . == 6 ~ 5,
    . == 7 ~ 6,
    . == 8 ~ 7,
    . == 9 ~ 8,
    . == 11164 ~ 1,  # "Does not describe success"
    . == 11173 ~ 10, # "Fully describes success"
    . %in% c(11174) ~ NA_real_,  # Convert special values to NA
    TRUE ~ .  # Keep other values unchanged
  ))) %>% 
  rename(`Make to top` = var368,
         `Elected` = var369,
         `Get what they want` = var370, 
         `Allowed to run` = var371,
         `Sound successful` = var372)

corvars <- df_traits1 %>% select(`Make to top`, `Elected`, `Get what they want`, `Allowed to run`, `Sound successful`)
corrs <- cor(corvars, use = "complete.obs")
corvars %>% GGally::ggpairs(lower = "blank",
                            upper = list(continuous = wrap("cor", size = 5))) +
  theme(strip.text = element_text(size = 9))

ggsave(filename = "outputs_appendix/Figure8.png", 
       height=9, width=9,
       dpi = 600)


df_traits2 <- import(here("data/misc", "traits2.dta"))

corrs <- cor(df_traits2, use = "complete.obs")
plot1 <- corrplot(corrs, method = 'number', cl.pos = 'n')
df_traits2 %>% GGally::ggpairs(lower = "blank",
                               upper = list(continuous = wrap("cor", size = 5))) +
  theme(strip.text = element_text(size = 5))

ggsave(filename = "outputs_appendix/Figure7.png", 
       height=9, width=9,
       dpi = 600)

