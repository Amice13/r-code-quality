
###############
### FIGURES ###
###############

### Figure 2 ###

countries <- unique(dataset2$country)

dataset_long <- dataset2 %>%
  select(Gender, country, social, material, purposive) %>%
  pivot_longer(cols = c(social, material, purposive), names_to = "incentive", values_to = "score")

means <- dataset_long %>%
  group_by(country, incentive, Gender) %>%
  summarise(mean_score = mean(score, na.rm = TRUE),
            se = sd(score, na.rm = TRUE) / sqrt(n()),
            n = n(), .groups = "drop") %>%
  pivot_wider(names_from = Gender, values_from = c(mean_score, se, n), names_prefix = "G") %>%
  mutate(estimate = mean_score_G1 - mean_score_G0,
         se = sqrt(se_G1^2 + se_G0^2),
         conf.low = estimate - 1.96 * se,
         conf.high = estimate + 1.96 * se)

means$incentive <- factor(means$incentive,
                          levels = c("material", "purposive", "social"),
                          labels = c("Material", "Purposive", "Social"))

means$country <- factor(means$country,
                        levels = rev(c("Australia", "Austria", "Germany", "Italy", "Spain")))

means <- means %>%
  mutate(y_pos = as.numeric(country) + case_when(
    incentive == "Material" ~ 0.15,
    incentive == "Purposive" ~ 0,
    incentive == "Social" ~ -0.15
  ))

ggplot(means, aes(x = estimate, y = y_pos, shape = incentive, linetype = incentive)) +
  geom_point(size = 2.5) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_continuous(
    breaks = 1:5,
    labels = levels(means$country),
    name = ""
  ) +
  labs(title = "",
       x = "Difference in means (W − M)",
       shape = "Incentive type",
       linetype = "Incentive type") +
  scale_shape_manual(values = c("Material" = 17,
                                "Purposive" = 16,
                                "Social" = 15)) +
  scale_linetype_manual(values = c("Material" = "dotted",
                                   "Purposive" = "dashed",
                                   "Social" = "solid")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 14))


### Figure 3 ###

coefficientsnames_plot <- c("ideologyCenter-right" = "Ideology:\nCenter-right",
                            "interest" = "Political interest",
                            "Education" = "Tertiary education",
                            "age" = "Age",
                            "Familybackground" = "Party-affiliated\nfamily ties",
                            "Gender" = "Gender: Woman")

b <- list(geom_vline(xintercept = 0, color = 'black'))

plot1 <- modelplot(model5p,
                   size = 0.3,
                   coef_map = coefficientsnames_plot,
                   background = b,
                   conf_level = 0.95,
                   draw = FALSE)

plot1$term <- as.character(plot1$term)
plot1$term[plot1$term == ""] <- "Gender: Woman"
plot1$term <- factor(plot1$term, levels = c("ideologyCenter-right" = "Ideology:\nCenter-right",
                                            "interest" = "Political interest",
                                            "Education" = "Tertiary education",
                                            "age" = "Age",
                                            "Familybackground" = "Party-affiliated\nfamily ties",
                                            "Gender" = "Gender: Woman"))

p1 <- ggplot(plot1, aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Add vertical line at zero
  geom_point(color = "black") +  # Adjust point color and size
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), color = "black", height = 0.15) +
  labs(title = "Purposive incentives",
       x = "",
       y = "") +
  scale_x_continuous(breaks = c(-0.05, 0, 0.05, 0.1), limits = c(-0.07, 0.11)) +
  theme_light() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 15))

# ======================== #

plot2 <- modelplot(model5s,
                   size = 0.3,
                   coef_map = coefficientsnames_plot,
                   background = b,
                   conf_level = 0.95,
                   draw = FALSE)

plot2$term <- as.character(plot2$term)
plot2$term[plot2$term == ""] <- "Gender: Woman"
plot2$term <- factor(plot2$term, levels = c("ideologyCenter-right" = "Ideology:\nCenter-right",
                                            "interest" = "Political interest",
                                            "Education" = "Tertiary education",
                                            "age" = "Age",
                                            "Familybackground" = "Party-affiliated\nfamily ties",
                                            "Gender" = "Gender: Woman"))

p2 <- ggplot(plot2, aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Add vertical line at zero
  geom_point(color = "black") +  # Adjust point color and size
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), color = "black", height = 0.15) +
  labs(title = "Social incentives",
       x = "",
       y = "") +
  scale_x_continuous(breaks = c(-0.05, 0, 0.05, 0.1), limits = c(-0.07, 0.11)) +
  theme_light() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 15))

# ======================== #

plot3 <- modelplot(model5m,
                   size = 0.3,
                   coef_map = coefficientsnames_plot,
                   background = b,
                   conf_level = 0.95,
                   draw = FALSE)

plot3$term <- as.character(plot3$term)
plot3$term[plot3$term == ""] <- "Gender: Woman"
plot3$term <- factor(plot3$term, levels = c("ideologyCenter-right" = "Ideology:\nCenter-right",
                                            "interest" = "Political interest",
                                            "Education" = "Tertiary education",
                                            "age" = "Age",
                                            "Familybackground" = "Party-affiliated\nfamily ties",
                                            "Gender" = "Gender: Woman"))

p3 <- ggplot(plot3, aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Add vertical line at zero
  geom_point(color = "black") +  # Adjust point color and size
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), color = "black", height = 0.15) +
  labs(title = "Material incentives",
       x = "",
       y = "") +
  scale_x_continuous(breaks = c(-0.05, 0, 0.05, 0.1), limits = c(-0.07, 0.11)) +
  theme_light() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 15))

grid.arrange(p1, p2, p3, ncol = 3, nrow = 1)


### Figure 4 ###

# Figure 4a: family background by gender and country

CrossTable(australia$Familybackground, australia$Gender, prop.c=TRUE,
           prop.t=FALSE, prop.r=FALSE, prop.chisq = FALSE,
           total.r=FALSE, total.c=TRUE, chisq=TRUE)

CrossTable(austria$Familybackground, austria$Gender, prop.c=TRUE,
           prop.t=FALSE, prop.r=FALSE, prop.chisq = FALSE,
           total.r=FALSE, total.c=TRUE, chisq=TRUE)

CrossTable(germany$Familybackground, germany$Gender, prop.c=TRUE,
           prop.t=FALSE, prop.r=FALSE, prop.chisq = FALSE,
           total.r=FALSE, total.c=TRUE, chisq=TRUE)

CrossTable(italy$Familybackground, italy$Gender, prop.c=TRUE,
           prop.t=FALSE, prop.r=FALSE, prop.chisq = FALSE,
           total.r=FALSE, total.c=TRUE, chisq=TRUE)

CrossTable(spain$Familybackground, spain$Gender, prop.c=TRUE,
           prop.t=FALSE, prop.r=FALSE, prop.chisq = FALSE,
           total.r=FALSE, total.c=TRUE, chisq=TRUE)

figure4a <- matrix(c(31.5, 40.8,
                     46.5, 51.1,
                     34.1, 45.2,
                     40.4, 57.4,
                     31.2, 52.2), ncol = 2, byrow = TRUE)
colnames(figure4a) <- c("Men", "Women")
rownames(figure4a) <- c("Australia",
                        "Austria",
                        "Germany",
                        "Italy",
                        "Spain")

figure4a <- as.table(figure4a)

figure4a <- as.data.frame(figure4a)

figure4a$Var1 <- factor(figure4a$Var1, levels = c("Spain",
                                                  "Italy",
                                                  "Germany",
                                                  "Austria",
                                                  "Australia"))

pct1 <- scales::percent_format(scale = 1)
pct1(100)

p1 <- ggplot(figure4a, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position=position_dodge2(reverse=TRUE), stat="identity", width = 0.6) +
  ggtitle("By country") +
  coord_flip() +
  labs(x = "", y = "", fill = "") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
  scale_fill_grey(start=0.8, end=0.2) +
  theme_light() +
  theme(plot.title = element_text(hjust=0.5))

# Figure 4b: family background by gender and youth wing

AYL <- dataset2 %>% 
  filter(youthwing == "AYL")

YL <- dataset2 %>% 
  filter(youthwing == "YL")

SJOE <- dataset2 %>% 
  filter(youthwing == "SJOE")

JVP <- dataset2 %>% 
  filter(youthwing == "JVP")

Jusos <- dataset2 %>% 
  filter(youthwing == "Jusos")

JU <- dataset2 %>% 
  filter(youthwing == "JU")

GD <- dataset2 %>% 
  filter(youthwing == "GD")

FIG <- dataset2 %>% 
  filter(youthwing == "FIG")

JSE <- dataset2 %>% 
  filter(youthwing == "JSE")

NNGG <- dataset2 %>% 
  filter(youthwing == "NNGG")

CrossTable(AYL$Familybackground, AYL$Gender, prop.c=TRUE,
           prop.t=FALSE, prop.r=FALSE, prop.chisq = FALSE,
           total.r=FALSE, total.c=TRUE, chisq=TRUE)

CrossTable(YL$Familybackground, YL$Gender, prop.c=TRUE,
           prop.t=FALSE, prop.r=FALSE, prop.chisq = FALSE,
           total.r=FALSE, total.c=TRUE, chisq=TRUE)

CrossTable(SJOE$Familybackground, SJOE$Gender, prop.c=TRUE,
           prop.t=FALSE, prop.r=FALSE, prop.chisq = FALSE,
           total.r=FALSE, total.c=TRUE, chisq=TRUE)

CrossTable(JVP$Familybackground, JVP$Gender, prop.c=TRUE,
           prop.t=FALSE, prop.r=FALSE, prop.chisq = FALSE,
           total.r=FALSE, total.c=TRUE, chisq=TRUE)

CrossTable(Jusos$Familybackground, Jusos$Gender, prop.c=TRUE,
           prop.t=FALSE, prop.r=FALSE, prop.chisq = FALSE,
           total.r=FALSE, total.c=TRUE, chisq=TRUE)

CrossTable(JU$Familybackground, JU$Gender, prop.c=TRUE,
           prop.t=FALSE, prop.r=FALSE, prop.chisq = FALSE,
           total.r=FALSE, total.c=TRUE, chisq=TRUE)

CrossTable(GD$Familybackground, GD$Gender, prop.c=TRUE,
           prop.t=FALSE, prop.r=FALSE, prop.chisq = FALSE,
           total.r=FALSE, total.c=TRUE, chisq=TRUE)

CrossTable(FIG$Familybackground, FIG$Gender, prop.c=TRUE,
           prop.t=FALSE, prop.r=FALSE, prop.chisq = FALSE,
           total.r=FALSE, total.c=TRUE, chisq=TRUE)

CrossTable(JSE$Familybackground, JSE$Gender, prop.c=TRUE,
           prop.t=FALSE, prop.r=FALSE, prop.chisq = FALSE,
           total.r=FALSE, total.c=TRUE, chisq=TRUE)

CrossTable(NNGG$Familybackground, NNGG$Gender, prop.c=TRUE,
           prop.t=FALSE, prop.r=FALSE, prop.chisq = FALSE,
           total.r=FALSE, total.c=TRUE, chisq=TRUE)

figureF1a <- matrix(c(32.2, 40.7, # 8.5
                      30.7, 41.1, # 10.4
                      43.5, 50.9, # 7.4
                      49.3, 51.6, # 2.3
                      34.4, 45.0, # 10.6
                      33.6, 46.2, # 12.6
                      44.4, 62.4, # 18 second largest
                      33.8, 45.7, # 11.9
                      31.2, 54.9, # 23.7 largest
                      31.2, 47.8), # 16.6
                    ncol = 2, byrow = TRUE)
colnames(figureF1a) <- c("Men", "Women")
rownames(figureF1a) <- c("AYL", "YL", "SJOE", "JVP", "Jusos", "JU", "GD", "FIG", "JSE", "NNGG")

figureF1a <- as.table(figureF1a)

figureF1a <- as.data.frame(figureF1a)

figureF1a$Var1 <- factor(figureF1a$Var1, levels = c("NNGG", "JSE", 
                                                    "FIG", "GD", 
                                                    "JU", "Jusos",
                                                    "JVP", "SJOE",
                                                    "YL", "AYL"))

pct1 <- scales::percent_format(scale = 1)
pct1(100)

p3 <- ggplot(figureF1a, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position=position_dodge2(reverse=TRUE), stat="identity", width = 0.6) +
  ggtitle("By youth wing") +
  coord_flip() +
  labs(x = "", y = "", fill = "") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
  scale_fill_grey(start=0.8, end=0.2) +
  theme_light() +
  theme(plot.title = element_text(hjust=0.5))

grid_arrange_shared_legend(p1, p3, nrow = 1, position = "bottom")


### Figure 5: Interactions ###

# Purposive

purposive_preds <- plot_predictions(
  model5pint,
  type = "response",
  condition = c("Gender", "Familybackground"),
  vcov = ~youthwing,
  draw = FALSE
)

purposive_preds <- purposive_preds %>%
  mutate(
    Gender = factor(Gender, levels = c(0, 1), labels = c("Men", "Women")),
    Familybackground = factor(Familybackground, levels = c(0, 1), labels = c("No", "Yes"))
  )

purposive_preds_plot <- ggplot(purposive_preds, aes(x = Familybackground, y = estimate, group = Gender, shape = Gender)) +
  geom_line(aes(linetype = Gender), 
            position = position_dodge(width = 0.5), 
            color = "black") +
  geom_point(position = position_dodge(width = 0.5), size = 3, color = "black") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = position_dodge(width = 0.5),
                width = 0.1, color = "black") +
  labs(title = "Purposive incentives",
       x = "Family ties",
       y = "Predicted probabilities",
       shape = "Gender",
       linetype = "Gender") +
  scale_linetype_manual(values = c("Men" = "dotted", "Women" = "dashed")) +
  scale_y_continuous(breaks=c(0.6, 0.7, 0.8), limits = c(0.6, 0.8)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5),
    axis.title = element_text(size = 12)
  )

# Material

material_preds <- plot_predictions(
  model5mint,
  type = "response",
  condition = c("Gender", "Familybackground"),
  vcov = ~youthwing,
  draw = FALSE
)

material_preds <- material_preds %>%
  mutate(
    Gender = factor(Gender, levels = c(0, 1), labels = c("Men", "Women")),
    Familybackground = factor(Familybackground, levels = c(0, 1), labels = c("No", "Yes"))
  )

material_preds_plot <- ggplot(material_preds, aes(x = Familybackground, y = estimate, group = Gender, shape = Gender)) +
  geom_line(aes(linetype = Gender), 
            position = position_dodge(width = 0.5), 
            color = "black") +
  geom_point(position = position_dodge(width = 0.5), size = 3, color = "black") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = position_dodge(width = 0.5),
                width = 0.1, color = "black") +
  labs(title = "Material incentives",
       x = "Family ties",
       y = "",
       shape = "Gender",
       linetype = "Gender") +
  scale_linetype_manual(values = c("Men" = "dotted", "Women" = "dashed")) +
  scale_y_continuous(breaks=c(0.3, 0.4, 0.5), limits = c(0.3, 0.5)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5),
    axis.title = element_text(size = 12)
  )

grid_arrange_shared_legend(purposive_preds_plot, material_preds_plot)


### Appendix figures ###

# Replication of figure 2 by party ideology

dataset_long_ideo <- dataset2 %>%
  select(Gender, ideology, social, material, purposive) %>%
  pivot_longer(cols = c(social, material, purposive),
               names_to = "incentive",
               values_to = "score")

means_ideo <- dataset_long_ideo %>%
  group_by(ideology, incentive, Gender) %>%
  summarise(mean_score = mean(score, na.rm = TRUE),
            se = sd(score, na.rm = TRUE) / sqrt(n()),
            n = n(),
            .groups = "drop") %>%
  pivot_wider(names_from = Gender,
              values_from = c(mean_score, se, n),
              names_prefix = "G") %>%
  mutate(estimate = mean_score_G1 - mean_score_G0,
         se = sqrt(se_G1^2 + se_G0^2),
         conf.low = estimate - 1.96 * se,
         conf.high = estimate + 1.96 * se)

means_ideo$incentive <- factor(means_ideo$incentive,
                               levels = c("material", "purposive", "social"),
                               labels = c("Material", "Purposive", "Social"))

means_ideo$ideology <- factor(means_ideo$ideology,
                              levels = c("Center-right", "Center-left"))

means_ideo <- means_ideo %>%
  mutate(y_pos = as.numeric(ideology) + dplyr::case_when(
    incentive == "Material"  ~ 0.15,
    incentive == "Purposive" ~ 0,
    incentive == "Social"    ~ -0.15
  ))

ggplot(means_ideo,
       aes(x = estimate,
           y = y_pos,
           shape = incentive,
           linetype = incentive)) +
  geom_point(size = 2.5) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_continuous(
    breaks = 1:2,
    labels = levels(means_ideo$ideology),
    name = ""
  ) +
  labs(title = "",
       x = "Difference in means (W − M)",
       shape = "Incentive type",
       linetype = "Incentive type") +
  scale_shape_manual(values = c("Material" = 17,
                                "Purposive" = 16,
                                "Social" = 15)) +
  scale_linetype_manual(values = c("Material" = "dotted",
                                   "Purposive" = "dashed",
                                   "Social" = "solid")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 14))


# Replication of figure 4 by party ideology

CrossTable(centerleft$Familybackground, centerleft$Gender, prop.c=TRUE,
           prop.t=FALSE, prop.r=FALSE, prop.chisq = FALSE,
           total.r=FALSE, total.c=TRUE, chisq=TRUE)

CrossTable(centerright$Familybackground, centerright$Gender, prop.c=TRUE,
           prop.t=FALSE, prop.r=FALSE, prop.chisq = FALSE,
           total.r=FALSE, total.c=TRUE, chisq=TRUE)

figure4rep <- matrix(c(35.4, 49.3, # 13.9
                      34.5, 46.6), # 12.1
                    ncol = 2, byrow = TRUE)
colnames(figure4rep) <- c("Men", "Women")
rownames(figure4rep) <- c("Center-left", "Center-right")

figure4rep <- as.table(figure4rep)

figure4rep <- as.data.frame(figure4rep)

figure4rep$Var1 <- factor(figure4rep$Var1, levels = c("Center-right", "Center-left"))

pct1 <- scales::percent_format(scale = 1)
pct1(100)

ggplot(figure4rep, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position=position_dodge2(reverse=TRUE), stat="identity", width = 0.4) +
  ggtitle("") +
  coord_flip() +
  labs(x = "", y = "", fill = "") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
  scale_fill_grey(start=0.8, end=0.2) +
  theme_light() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))








