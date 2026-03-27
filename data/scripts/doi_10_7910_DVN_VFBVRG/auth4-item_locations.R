################################################################################
# Created By: Pietryka
# Creation Date: 2017-07-24
# Purpose: Creates figure G1C in online supporting info
# R Version: R version 3.3.3 (2017-03-06)
# Data Input: Data/Derived/2012/groups_and_scales.csv
# NOTES: MUST FIRST RUN 'Analysis\Authoritarianism\auth1-run_models.R'
#
# Questions: mpietryka@fsu.edu
################################################################################




# 1. CLEAN DATA -----------------------------
final_df <- raw_df %>%
  arrange(item, mod)  %>%
  # IDENTIFY GROUPS COMBINED IN ITEM
  mutate(split_groups = stringr::str_extract(item, "[0-9]+") )    %>%
  mutate(split_groups = recode(split_groups, .missing = "01"))    %>%
  mutate(split_groups = gsub("(.{1})", "\\1 ", split_groups))   %>%
  separate(split_groups , paste0("value", 1:2), sep = " " )  %>%
  mutate_at(vars(starts_with("value")), as.numeric)  %>%
  # REMOVE SPLIT IDs FROM ITEM NAME
  mutate(word1 = stringr::word(item, 1, sep = "_"))  %>%
  mutate(word2 = stringr::word(item, 2, sep = "_"))  %>%
  mutate(item = paste(word1, word2, sep = "_"))   %>%
  # MERGE WITH GROUP VARIABLE LABELS
  mutate(group_var = "votechoice")  %>%
  mutate(scale_name = "auth_vars")  %>%
  left_join(labdata_group, by = c("group_var" = "group_abrv"))  %>%
  left_join(labdata_scale, by = c("scale_name" = "scale_abrv"))  %>%
  left_join(item_ids, by = c("scale_name" = "scale_abrv", "item"))  %>%
  # WIDE TO LONG
  gather(order, votechoice, value1:value2)  %>%
  filter(!is.na(votechoice))  %>%
  left_join(group_labs_df, by = c("group_lab", "votechoice" = "anova_id"))  %>%
  # ITEM ANCHORS
  mutate(anchors = recode(item_id,
                          `1` = "1. Independence vs. Respect for elders",
                          `2` = "2. Curiosity vs. Good manners",
                          `3` = "3. Self-reliance vs. Obedience",
                          `4` = "4. Being considerate vs. Well behaved"
  ))  %>%
  # FACET LABELS
  mutate(obama_lab = recode(votechoice,
                            `0` = "Obama Voters",
                            .default = ""))  %>%
  mutate(romney_lab = recode(votechoice,
                             `1` = "Romney Voters",
                             .default = ""))  %>%
  mutate_at(vars(obama_lab, romney_lab),
            funs(ifelse(mod == 1 & item_id == 1, ., "")))  %>%
  # ITERATION LABELS
  mutate(mod_lab = recode(mod,
                          `1` = "1: baseline",
                          `2` = "2: one item corrected",
                          `3` = "3: two items corrected",
                          `4` = "4: DIF free")
  )










# 2. MAKE PLOT ----------------



item_plot <- ggplot(final_df,
                   aes(
                     x = mod_lab,
                     y = xsi.item,
                     color = factor(value_lab),
                     group = factor(value_lab)
                   )) +
  # FACETS
  facet_wrap(~ anchors, scales = "fixed", nrow = 1) +
  # DISPLAY CORRECTED ESTIMATES
  geom_line(size = 2, alpha = 0.5) +
  geom_point(size = 6, color = "white") +
  geom_point(size = 6, alpha = 0.5) +
  # ANNOTATE PLOT
  geom_text(aes(label = obama_lab),
            x = 2.9,
            y = -1.1,
            size = rel(4),
            angle = 5,
            color = dem_col,
            fontface = "bold") +
  geom_text(aes(label = romney_lab),
            x = 2.8,
            y = -2.2,
            size = rel(4),
            angle = -10,
            color = rep_col,
            fontface = "bold") +
  # FORMAT
  labs(title = "Obama voters need to be more authoritarian than Romney voters to endorse items 1-3",
       subtitle = "\"Please tell me which one you think is more important for a child to have\"",
       x = "Iteration",
       y = "Item Location") +
  scale_colour_manual(values = c("blue", "red"), name = "Vote choice in 2012") +
  theme_minimal(base_size = 9) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.subtitle =  element_text(color = axis_col)
        )

item_plot

# 3. DISPLAY RELEVANT DATA ----------------

final_df  %>%
  arrange(mod, anchors, value_lab)  %>%
  select(iteration = mod, item = anchors, `vote choice` = value_lab, xsi.item)  %>%
  knitr::kable(digits = 2)



