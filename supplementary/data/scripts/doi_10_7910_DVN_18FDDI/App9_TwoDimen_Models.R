####################################################################
########## Laterzo 2023 ############################################
######### Progressive/Punitive #####################################
####### Results from Two-Dimen Ideological Measure -- Appendix 9 ###
####################################################################

############################### SET UP #########################################

rm(list=ls()) #clear global environ


# uncomment below and set working directory using setwd() to directory which contains
# all data files
#setwd()


#packages
library(tidyverse)
library(naniar)
library(cregg)
library(scales)


#read in data and filter
data <- readRDS("laterzo_cps_2023c.RDS")
levels(data$crime)[levels(data$crime)=="Tough on Crime"] <- "Punitive"

############################### CONJOINT SET UP ################################


#safety variable
#4 insecure, 1 secure --> 1 = unsafe, 0 = safe
data$safety_di <- factor(ifelse(data$safety_neighb == 4, "1", 
                                ifelse(data$safety_neighb == 3, "1", 
                                       ifelse(data$safety_neighb == 2, "0", 
                                              ifelse(data$safety_neighb == 1, "0", NA)))))

##create data frames for prog vs. conserv on social and econ issues based on
## median value
prog_social <- data %>% filter(social >= 0) #same as not including zero
prog_econ <- data %>% filter(economy >= 0) #same as not including zero

cons_social <- data %>% filter(social < 0) #same as not including zero
cons_econ <- data %>% filter(economy < 0) #same as not including zero

#number of respondents 
sociocult_cons_n <- cons_social %>%
  select(id) %>%
  unique()
nrow(sociocult_cons_n) #1132

sociocult_prog_n <- prog_social %>%
  select(id) %>%
  unique()
nrow(sociocult_prog_n) #1267

econ_cons_n <- cons_econ %>%
  select(id) %>%
  unique()
nrow(econ_cons_n) #1116

econ_prog_n <- prog_econ %>%
  select(id) %>%
  unique()
nrow(econ_prog_n) #1283


########################### BASE MODELS ########################################
#variable for prog/conserv based on median index value
data$econ_di <- factor(ifelse(data$economy >= 0, "Prog", "Conserv"))
data$social_di <- factor(ifelse(data$social >= 0, "Prog", "Conserv"))


##### BASELINE MODEL: Divided by ideology ------------------
econ_model <- cj(data,
                 chosen ~ crime,
                 id = ~id,
                 estimate = "mm",
                 by = ~econ_di,
                 alpha = .05,
                 h0 = 0.5)

social_model <- cj(data,
                   chosen ~ crime,
                   id = ~id,
                   estimate = "mm",
                   alpha = .05,
                   by = ~social_di,
                   h0 = 0.5)

# combine
econ_model$sample <- "Economic Dimension"
social_model$sample <- "Sociocultural Dimension"
econ_model <- econ_model %>% select(BY, feature, level, estimate, lower, upper, sample)
social_model <- social_model %>% select(BY, feature, level, estimate, lower, upper, sample)
combo <- rbind(econ_model, social_model)

# Appendix Figure A9.4 PART 1: Candidate Attribute Preferences Across Divided by Ideology

#jpeg("app_figA9-4_part1.jpeg", res=600, width=5000, height=2000)
ggplot(combo, aes(x = level, y = estimate, color = BY)) +
  geom_hline(yintercept = 0.5, colour = gray(1/2), lty = 2) +
  geom_point(position=position_dodge(0.1)) +
  geom_errorbar(aes(x = level, 
                    ymin = lower,
                    ymax = upper,
                    color = BY),
                width = .2,
                alpha = 0.8,
                position=position_dodge(0.1),
                lwd = 0.7) +
  coord_flip() +
  facet_wrap(~sample) +
  scale_y_continuous(labels = label_number(accuracy = 0.10),
                     breaks = c(0.45, 0.5, 0.55)) +
  scale_colour_brewer(palette = "Set1",
                      name="Ideology",
                      labels = c("Conservative", 'Progressive')) +
  labs(x = "Crime Policy",
       y = "Marginal Mean") +
  theme_bw() +
  theme(text = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "bottom",
        legend.box.background = element_rect(colour = "black"))
#dev.off()



### Differences in MM

econ_model_diff <- cj(data,
                      chosen ~ crime,
                      id = ~id,
                      estimate = "mm_diff",
                      by = ~econ_di,
                      alpha = .05,
                      h0 = 0.5)

social_model_diff <- cj(data,
                        chosen ~ crime,
                        id = ~id,
                        estimate = "mm_diff",
                        alpha = .05,
                        by = ~social_di,
                        h0 = 0.5)

# combine
econ_model_diff$sample <- "Economic Dimension"
social_model_diff$sample <- "Sociocultural Dimension"
econ_model_diff <- econ_model_diff %>% select(BY, feature, level, estimate, lower, upper, sample)
social_model_diff <- social_model_diff %>% select(BY, feature, level, estimate, lower, upper, sample)
combo_diff <- rbind(econ_model_diff, social_model_diff)


# Figure A9.4 PART 2: Candidate Attribute Preferences Across Divided by Ideology
#jpeg("app_figA9-4_part2.jpeg", res=600, width=5000, height=1500)
ggplot(combo_diff, aes(x = level, y = estimate)) +
  geom_hline(yintercept = 0.0, colour = gray(1/2), lty = 2) + 
  geom_point(position=position_dodge(0.1)) +
  geom_errorbar(aes(x = level, 
                    ymin = lower,
                    ymax = upper),
                width = .1,
                alpha = 0.8,
                position=position_dodge(0.1),
                lwd = 0.7) +
  facet_wrap(~sample) +
  coord_flip() +
  scale_y_continuous(labels = label_number(accuracy = 0.01),
                     breaks = c(-0.03, 0.00, 0.03)) +
  labs(x = "Crime Policy",
       y = "Difference in Marginal Mean") +
  theme_bw() +
  theme(text = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))
#dev.off()



########################### VICTIMIZATION MODELS ###############################

vic_simp <- cj(data, chosen ~ crime,
               id = ~id, estimate = "mm",
               by = ~vic, alpha = .05)

vic_progecon_simp <- cj(prog_econ, chosen ~ crime,
                        id = ~id, estimate = "mm",
                        by = ~vic, alpha = .05)

vic_progsocial_simp <- cj(prog_social, chosen ~ crime,
                          id = ~id, estimate = "mm",
                          by = ~vic, alpha = .05)

vic_consecon_simp <- cj(cons_econ, chosen ~ crime,
                        id = ~id, estimate = "mm",
                        by = ~vic, alpha = .05)

vic_conssocial_simp <- cj(cons_social, chosen ~ crime,
                          id = ~id, estimate = "mm",
                          by = ~vic, alpha = .05)

### combining
vic_simp$sample <- "Pooled"
vic_progecon_simp$sample <- "Econ. Prog."
vic_progsocial_simp$sample <- "Sociocult. Prog."
vic_consecon_simp$sample <- "Econ. Conserv."
vic_conssocial_simp$sample <- "Sociocult. Conserv."
vic_total <- rbind(vic_simp, vic_progecon_simp, vic_progsocial_simp,
                   vic_consecon_simp, vic_conssocial_simp)
vic_total$sample <- factor(vic_total$sample, 
                           levels = c("Pooled", "Econ. Prog.",
                                      "Sociocult. Prog.",
                                      "Econ. Conserv.",
                                      "Sociocult. Conserv."),
                           ordered = T)

# Appendix Figure A9.5 PART 1: Victimization and Candidate Attribute Preferences and Differences in Marginal Means
#jpeg("app_figA9-5_part1.jpeg", res=600, width=6000, height=2000)
ggplot(vic_total, aes(x = level, y = estimate, color = BY)) +
  geom_hline(yintercept = 0.5, colour = gray(1/2), lty = 2) + 
  geom_point(position=position_dodge(0.2)) +
  geom_errorbar(aes(x = level, 
                    ymin = lower,
                    ymax = upper,
                    color = BY),
                width = .2,
                alpha = 0.8,
                position=position_dodge(0.2),
                lwd = 0.7) + 
  facet_grid(~sample) +
  coord_flip() +
  scale_y_continuous(labels = label_number(accuracy = 0.001),
                     breaks = c(0.475, 0.5, 0.525)) +
  scale_colour_brewer(palette = "Set1",
                      name="Victimization",
                      labels =c("Non-Victim", "Victim")) +
  labs(x = "Crime Policy",
       y = "Marginal Mean") +
  theme_bw() +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.box.background = element_rect(colour = "black"))
#dev.off()

#differences
vic_diff <- cj(data, chosen ~ crime,
               id = ~id, estimate = "mm_diff",
               by = ~vic, alpha = 0.05)

vic_progecon_diff <- cj(prog_econ, chosen ~ crime,
                        id = ~id, estimate = "mm_diff",
                        by = ~vic, alpha = .05)

vic_progsocial_diff <- cj(prog_social, chosen ~ crime,
                          id = ~id, estimate = "mm_diff",
                          by = ~vic, alpha = .05)

vic_consecon_diff <- cj(cons_econ, chosen ~ crime,
                        id = ~id, estimate = "mm_diff",
                        by = ~vic, alpha = .05)

vic_conssocial_diff <- cj(cons_social, chosen ~ crime,
                          id = ~id, estimate = "mm_diff",
                          by = ~vic, alpha = .05)



### combining diff
vic_diff$sample <- "Pooled"
vic_progecon_diff$sample <- "Econ. Prog."
vic_progsocial_diff$sample <- "Sociocult. Prog."
vic_consecon_diff$sample <- "Econ. Conserv."
vic_conssocial_diff$sample <- "Sociocult. Conserv."
vic_diff_total <- rbind(vic_diff, vic_progecon_diff, vic_progsocial_diff,
                        vic_consecon_diff, vic_conssocial_diff)
vic_diff_total$sample <- factor(vic_diff_total$sample, 
                                levels = c("Pooled", "Econ. Prog.",
                                           "Sociocult. Prog.",
                                           "Econ. Conserv.",
                                           "Sociocult. Conserv."),
                                ordered = T)


# Appendix Figure A9.5 PART 2: Victimization and Candidate Attribute Preferences and Differences in Marginal Means
#jpeg("app_figA9-5_part2.jpeg", res=600, width=6000, height=1500)
ggplot(vic_diff_total, aes(x = level, y = estimate)) +
  geom_hline(yintercept = 0.0, colour = gray(1/2), lty = 2) + 
  geom_point(position=position_dodge(0.1)) +
  geom_errorbar(aes(x = level, 
                    ymin = lower,
                    ymax = upper),
                width = .1,
                alpha = 0.8,
                position=position_dodge(0.1),
                lwd = 0.7) +
  facet_grid(~sample) +
  coord_flip() +
  scale_y_continuous(labels = label_number(accuracy = 0.01),
                     breaks = c(-0.03, 0.00, 0.03)) +
  labs(x = "Crime Policy",
       y = "Difference in Marginal Mean") +
  theme_bw() +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 14))
#dev.off()



######################### MODELS: COMMUNITY SAFETY ##########################

#models
safety_simp <- cj(data,
                  chosen ~ crime,
                  id = ~id,
                  estimate = "mm",
                  by = ~safety_di,
                  alpha = 0.05)

safety_progecon_simp <- cj(prog_econ,
                           chosen ~ crime,
                           id = ~id,
                           estimate = "mm",
                           by = ~safety_di,
                           alpha = 0.05)


safety_progsocial_simp <- cj(prog_social,
                             chosen ~ crime,
                             id = ~id,
                             estimate = "mm",
                             by = ~safety_di,
                             alpha = 0.05)

safety_consecon_simp <- cj(cons_econ,
                           chosen ~ crime,
                           id = ~id,
                           estimate = "mm",
                           by = ~safety_di,
                           alpha = 0.05)


safety_conssocial_simp <- cj(cons_social,
                             chosen ~ crime,
                             id = ~id,
                             estimate = "mm",
                             by = ~safety_di,
                             alpha = 0.05)

### combining
safety_simp$sample <- "Pooled"
safety_progecon_simp$sample <- "Econ. Prog."
safety_progsocial_simp$sample <- "Sociocult. Prog."
safety_consecon_simp$sample <- "Econ. Conserv."
safety_conssocial_simp$sample <- "Sociocult. Conserv."

safety_total <- rbind(safety_simp, safety_progecon_simp, safety_progsocial_simp,
                      safety_consecon_simp, safety_conssocial_simp)
safety_total$sample <- factor(safety_total$sample, 
                              levels = c("Pooled", "Econ. Prog.",
                                         "Sociocult. Prog.",
                                         "Econ. Conserv.",
                                         "Sociocult. Conserv."),
                              ordered = T)

# Appendix Figure A9.6 PART 1: Preferences for Candidate Attributes Disaggregated by Community Safety and Differences in Marginal Means
#jpeg("app_figA9-6_part1.jpeg", res=600, width=6000, height=2000)
ggplot(safety_total, aes(x = level, y = estimate, color = BY)) +
  geom_hline(yintercept = 0.5, colour = gray(1/2), lty = 2) + 
  geom_point(position=position_dodge(0.1)) +
  geom_errorbar(aes(x = level, 
                    ymin = lower,
                    ymax = upper,
                    color = BY),
                width = .2,
                alpha = 0.8,
                position=position_dodge(0.1),
                lwd = 0.7) + 
  facet_grid(~sample) +
  coord_flip() +
  scale_y_continuous(labels = label_number(accuracy = 0.01),
                     breaks = c(0.45, 0.5, 0.55)) +
  scale_colour_brewer(palette = "Set1",
                      name="Neighb. Safety",
                      labels =c("Secure", "Insecure")) +
  labs(x = "Crime Policy",
       y = "Marginal Mean") +
  theme_bw() +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.box.background = element_rect(colour = "black"))
dev.off()

#differences
safety_diff <- cj(data,
                  chosen ~ crime,
                  id = ~id,
                  estimate = "mm_diff",
                  by = ~safety_di,
                  alpha = 0.05)

safety_progecon_diff <- cj(prog_econ,
                           chosen ~ crime,
                           id = ~id,
                           estimate = "mm_diff",
                           by = ~safety_di,
                           alpha = 0.05)

safety_progsocial_diff <- cj(prog_social,
                             chosen ~ crime,
                             id = ~id,
                             estimate = "mm_diff",
                             by = ~safety_di,
                             alpha = 0.05)

safety_consecon_diff <- cj(cons_econ,
                           chosen ~ crime,
                           id = ~id,
                           estimate = "mm_diff",
                           by = ~safety_di,
                           alpha = 0.05)

safety_conssocial_diff <- cj(cons_social,
                             chosen ~ crime,
                             id = ~id,
                             estimate = "mm_diff",
                             by = ~safety_di,
                             alpha = 0.05)


### combining diff
safety_diff$sample <- "Pooled"
safety_progecon_diff$sample <- "Econ. Prog."
safety_progsocial_diff$sample <- "Sociocult. Prog."
safety_consecon_diff$sample <- "Econ. Conserv."
safety_conssocial_diff$sample <- "Sociocult. Conserv."
safety_diff_total <- rbind(safety_diff, safety_progecon_diff,
                           safety_progsocial_diff, safety_consecon_diff,
                           safety_conssocial_diff)
safety_diff_total$sample <- factor(safety_diff_total$sample, 
                                   levels = c("Pooled",
                                              "Econ. Prog.",
                                              "Sociocult. Prog.",
                                              "Econ. Conserv.",
                                              "Sociocult. Conserv."),
                                   ordered = T)


# Appendix -Figure A9.6 PART 2: Preferences for Candidate Attributes Disaggregated by Community Safety and Differences in Marginal Means 
#jpeg("app_figA9-6_part2.jpeg", res=600, width=6000, height=1500)
ggplot(safety_diff_total, aes(x = level, y = estimate)) +
  geom_hline(yintercept = 0.0, colour = gray(1/2), lty = 2) + 
  geom_point(position=position_dodge(0.1)) +
  geom_errorbar(aes(x = level, 
                    ymin = lower,
                    ymax = upper),
                width = .1,
                alpha = 0.8,
                position=position_dodge(0.1),
                lwd = 0.7) +
  facet_grid(~sample) +
  coord_flip() +
  scale_y_continuous(labels = label_number(accuracy = 0.01),
                     breaks = c(-0.04, 0.00, 0.04)) +
  labs(x = "Crime Policy",
       y = "Difference in Marginal Mean") +
  theme_bw() +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 14))
#dev.off()



######################### MODELS: PERCEPTIONS OF SOCIAL ASSISTANCE #############

assist_simp <- cj(data,
                  chosen ~ crime,
                  id = ~id,
                  estimate = "mm",
                  by = ~assist_effect,
                  alpha = .05)

assist_progecon_simp <- cj(prog_econ,
                           chosen ~ crime,
                           id = ~id,
                           estimate = "mm",
                           by = ~assist_effect,
                           alpha = .05)

assist_progsocial_simp <- cj(prog_social,
                             chosen ~ crime,
                             id = ~id,
                             estimate = "mm",
                             by = ~assist_effect,
                             alpha = .05)

assist_consecon_simp <- cj(cons_econ,
                           chosen ~ crime,
                           id = ~id,
                           estimate = "mm",
                           by = ~assist_effect,
                           alpha = .05)

assist_conssocial_simp <- cj(cons_social,
                             chosen ~ crime,
                             id = ~id,
                             estimate = "mm",
                             by = ~assist_effect,
                             alpha = .05)

### combining
assist_simp$sample <- "Pooled"
assist_progecon_simp$sample <- "Econ. Prog."
assist_progsocial_simp$sample <- "Sociocult. Prog."
assist_consecon_simp$sample <- "Econ. Conserv."
assist_conssocial_simp$sample <- "Sociocult. Conserv."
assist_total <- rbind(assist_simp, assist_progecon_simp, assist_progsocial_simp,
                      assist_consecon_simp, assist_conssocial_simp)
assist_total$sample <- factor(assist_total$sample, 
                              levels = c("Pooled", "Econ. Prog.",
                                         "Sociocult. Prog.",
                                         "Econ. Conserv.",
                                         "Sociocult. Conserv."),
                              ordered = T)

# Appendix Figure A9.7 PART 1: Perceived Effectiveness of Social Assistance and Candidate Attribute Preferences and Differences in Marginal Means
#jpeg("app_figA9-7_part1.jpeg", res=600, width=6000, height=2000)
ggplot(assist_total, aes(x = level, y = estimate, color = BY)) +
  geom_hline(yintercept = 0.5, colour = gray(1/2), lty = 2) + 
  geom_point(position=position_dodge(0.1)) +
  geom_errorbar(aes(x = level, 
                    ymin = lower,
                    ymax = upper,
                    color = BY),
                width = .2,
                alpha = 0.8,
                position=position_dodge(0.1),
                lwd = 0.7) + 
  facet_grid(~sample) +
  coord_flip() +
  scale_y_continuous(labels = label_number(accuracy = 0.01),
                     breaks = c(0.45, 0.5, 0.55)) +
  scale_colour_brewer(palette = "Set1",
                      name="Soc. Assistance Effective?",
                      labels =c("No", "Yes")) +
  labs(x = "Crime Policy",
       y = "Marginal Mean") +
  theme_bw() +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.box.background = element_rect(colour = "black"))
#dev.off()


#differences
assist_diff <- cj(data,
                  chosen ~ crime,
                  id = ~id,
                  estimate = "mm_diff",
                  by = ~assist_effect,
                  alpha = .05)

assist_progecon_diff <- cj(prog_econ,
                           chosen ~ crime,
                           id = ~id,
                           estimate = "mm_diff",
                           by = ~assist_effect,
                           alpha = .05)

assist_progsocial_diff <- cj(prog_social,
                             chosen ~ crime,
                             id = ~id,
                             estimate = "mm_diff",
                             by = ~assist_effect,
                             alpha = .05)

assist_consecon_diff <- cj(cons_econ,
                           chosen ~ crime,
                           id = ~id,
                           estimate = "mm_diff",
                           by = ~assist_effect,
                           alpha = 0.05)

assist_conssocial_diff <- cj(cons_social,
                             chosen ~ crime,
                             id = ~id,
                             estimate = "mm_diff",
                             by = ~assist_effect,
                             alpha = 0.05)




### combining diff
assist_diff$sample <- "Pooled"
assist_progecon_diff$sample <- "Econ. Prog."
assist_progsocial_diff$sample <- "Sociocult. Prog."
assist_consecon_diff$sample <- "Econ. Conserv."
assist_conssocial_diff$sample <- "Sociocult. Conserv."
assist_diff_total <- rbind(assist_diff, assist_progecon_diff,
                           assist_progsocial_diff, assist_consecon_diff,
                           assist_conssocial_diff)
assist_diff_total$sample <- factor(assist_diff_total$sample, 
                                   levels = c("Pooled", "Econ. Prog.",
                                              "Sociocult. Prog.",
                                              "Econ. Conserv.", "Sociocult. Conserv."),
                                   ordered = T)


# Appendix Figure A9.7 PART 2: Perceived Effectiveness of Social Assistance and Candidate Attribute Preferences and Differences in Marginal Means
#jpeg("app_figA9-7_part2.jpeg", res=600, width=6000, height=1500)
ggplot(assist_diff_total, aes(x = level, y = estimate)) +
  geom_hline(yintercept = 0.0, colour = gray(1/2), lty = 2) + 
  geom_point(position=position_dodge(0.1)) +
  geom_errorbar(aes(x = level, 
                    ymin = lower,
                    ymax = upper),
                width = .1,
                alpha = 0.8,
                position=position_dodge(0.1),
                lwd = 0.7) +
  facet_grid(~sample) +
  coord_flip() +
  scale_y_continuous(labels = label_number(accuracy = 0.01),
                     breaks = c(-0.06, 0.00, 0.06)) +
  labs(x = "Crime Policy",
       y = "Difference in Marginal Mean") +
  theme_bw() +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 14))
#dev.off()

######################### CRIME DRIVEN BY GANGS ###############################

gang_simp <- cj(data, chosen ~ crime,
                id = ~id, estimate = "mm",
                by = ~crime_gang, alpha = .05)


gang_progecon_simp <- cj(prog_econ, chosen ~ crime,
                         id = ~id, estimate = "mm",
                         by = ~crime_gang, alpha = .05)

gang_progsocial_simp <- cj(prog_social, chosen ~ crime,
                           id = ~id, estimate = "mm",
                           by = ~crime_gang, alpha = .05)

gang_consecon_simp <- cj(cons_econ, chosen ~ crime,
                         id = ~id, estimate = "mm",
                         by = ~crime_gang, alpha = .05)

gang_conssocial_simp <- cj(cons_social, chosen ~ crime,
                           id = ~id, estimate = "mm",
                           by = ~crime_gang, alpha = .05)


### combining
gang_simp$sample <- "Pooled"
gang_progecon_simp$sample <- "Econ. Prog."
gang_progsocial_simp$sample <- "Sociocult. Prog."
gang_consecon_simp$sample <- "Econ. Conserv."
gang_conssocial_simp$sample <- "Sociocult. Conserv."
gang_total <- rbind(gang_simp, gang_progecon_simp, gang_progsocial_simp,
                    gang_consecon_simp, gang_conssocial_simp)
gang_total$sample <- factor(gang_total$sample, 
                            levels = c("Pooled", "Econ. Prog.",
                                       "Sociocult. Prog.",
                                       "Econ. Conserv.",
                                       "Sociocult. Conserv."),
                            ordered = T)

# Appendix - Figure A9.8 PART 1: Perceived Source of Crime (Gangs) and Candidate Attribute Preferences and Differences in Marginal Means
#jpeg("app_figA9-8_part1.jpeg", res=600, width=6000, height=2000)
ggplot(gang_total, aes(x = level, y = estimate, color = BY)) +
  geom_hline(yintercept = 0.5, colour = gray(1/2), lty = 2) + 
  geom_point(position=position_dodge(0.1)) +
  geom_errorbar(aes(x = level, 
                    ymin = lower,
                    ymax = upper,
                    color = BY),
                width = .2,
                alpha = 0.8,
                position=position_dodge(0.1),
                lwd = 0.7) + 
  facet_grid(~sample) +
  coord_flip() +
  scale_y_continuous(labels = label_number(accuracy = 0.01),
                     breaks = c(0.45, 0.5, 0.55)) +
  scale_colour_brewer(palette = "Set1",
                      name="Crime by Gangs?",
                      labels =c("No", "Yes")) +
  labs(x = "Crime Policy",
       y = "Marginal Mean") +
  theme_bw() +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.box.background = element_rect(colour = "black"))
#dev.off()

#differences
gang_diff <- cj(data, chosen ~ crime,
                id = ~id, estimate = "mm_diff",
                by = ~crime_gang, alpha = 0.05)

gang_progecon_diff <- cj(prog_econ, chosen ~ crime,
                         id = ~id, estimate = "mm_diff",
                         by = ~crime_gang, alpha = 0.05)

gang_progsocial_diff <- cj(prog_social, chosen ~ crime,
                           id = ~id, estimate = "mm_diff",
                           by = ~crime_gang, alpha = 0.05)

gang_consecon_diff <- cj(cons_econ, chosen ~ crime,
                         id = ~id, estimate = "mm_diff",
                         by = ~crime_gang, alpha = 0.05)

gang_conssocial_diff <- cj(cons_social, chosen ~ crime,
                           id = ~id, estimate = "mm_diff",
                           by = ~crime_gang, alpha = 0.05)


### combining diff
gang_diff$sample <- "Pooled"
gang_progecon_diff$sample <- "Econ. Prog."
gang_progsocial_diff$sample <- "Sociocult. Prog."
gang_consecon_diff$sample <- "Econ. Conserv."
gang_conssocial_diff$sample <- "Sociocult. Conserv."
gang_diff_total <- rbind(gang_diff, gang_progecon_diff, gang_progsocial_diff,
                         gang_consecon_diff, gang_conssocial_diff)
gang_diff_total$sample <- factor(gang_diff_total$sample, 
                                 levels = c("Pooled", "Econ. Prog.",
                                            "Sociocult. Prog.",
                                            "Econ. Conserv.",
                                            "Sociocult. Conserv."),
                                 ordered = T)

# Appendix - Figure A9.8 PART 2: Perceived Source of Crime (Gangs) and Candidate Attribute Preferences and Differences in Marginal Means
#jpeg("app_figA9-8_part2.jpeg", res=600, width=6000, height=1500)
ggplot(gang_diff_total, aes(x = level, y = estimate)) +
  geom_hline(yintercept = 0.0, colour = gray(1/2), lty = 2) + 
  geom_point(position=position_dodge(0.1)) +
  geom_errorbar(aes(x = level, 
                    ymin = lower,
                    ymax = upper),
                width = .1,
                alpha = 0.8,
                position=position_dodge(0.1),
                lwd = 0.7) +
  facet_grid(~sample) +
  coord_flip() +
  scale_y_continuous(labels = label_number(accuracy = 0.01),
                     breaks = c(-0.04, 0.00, 0.04)) +
  labs(x = "Crime Policy",
       y = "Difference in Marginal Mean") +
  theme_bw() +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 14))
#dev.off()
