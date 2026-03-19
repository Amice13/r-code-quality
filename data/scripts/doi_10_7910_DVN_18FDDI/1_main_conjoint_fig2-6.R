################################################################################
#### Title: Progressive Ideology and Support for Punitive Crime Policy
#### Author: Isabel Laterzo
#### Year: 2023
#### Journal: Comparative Political Studies
#### Portion of Analysis: Main Analysis - Conjoint
################################################################################



################################ SET UP #######################################

rm(list=ls()) #clear global environ

# uncomment below and set working directory using setwd() to directory which contains
# all data files
#setwd()

#packages
library(tidyverse)
library(naniar)
library(cregg)
library(ggplot2)
library(scales)


#read in data and filter
data <- readRDS("laterzo_cps_2023c.RDS")

#update levels naming
levels(data$crime)[levels(data$crime)=="Tough on Crime"] <- "Punitive"

############################### CONJOINT SET UP ################################

#community safety dichotomous variable
#4 insecure, 1 secure --> 1 = unsafe, 0 = safe
data$safety_di <- factor(ifelse(data$safety_neighb == 4, "1", 
                                ifelse(data$safety_neighb == 3, "1", 
                                       ifelse(data$safety_neighb == 2, "0", 
                                              ifelse(data$safety_neighb == 1, "0", NA)))))

##create data frames for prog vs. conserv  based on centering value
prog_ideo <- data %>% filter(ideo >= 0) #same as not including zero
cons_ideo <- data %>% filter(ideo < 0) #same as not including zero

#total observations
total_n <- data %>%
  select(id) %>%
  unique()
nrow(total_n) #2301


#number of respondents in ideological subgroups
prog_n <- prog_ideo %>%
  select(id) %>%
  unique()
nrow(prog_n) #1205

conserv_n <- cons_ideo %>%
  select(id) %>%
  unique()
nrow(conserv_n) #1096


########################### BASE MODELS ########################################

#variable for prog/conserv based on median index value
data$ideo_di <- factor(ifelse(data$ideo >= 0, "Prog", "Conserv"))


##### BASELINE MODEL: Divided by ideology ------------------
base_model <- cj(data,
                 chosen ~ crime,
                 id = ~id,
                 estimate = "mm",
                 by = ~ideo_di,
                 alpha = .05,
                 h0 = 0.5)


# FIGURE 2 - Part 1 - uncomment to save figures to working directory
#jpeg("figure2_part1.jpeg", res=600, width=2500, height=1500)
ggplot(base_model, aes(x = level, y = estimate, linetype = BY)) +
  geom_hline(yintercept = 0.5, colour = gray(0.7), lty = 1) + 
  geom_point(position=position_dodge(0.2)) +
  geom_errorbar(aes(x = level, 
                    ymin = lower,
                    ymax = upper,
                    linetype = BY),
                width = .2,
                alpha = 0.8,
                position=position_dodge(0.2),
                lwd = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = label_number(accuracy = 0.10),
                     breaks = c(0.45, 0.5, 0.55)) +
  scale_linetype_discrete(name="Ideology",
                          labels = c("Conservatives", 'Progressives')) +
  labs(x = "Crime Policy",
       y = "Marginal Mean") +
  theme_bw() +
  theme(text = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.box.background = element_rect(colour = "black"))
#dev.off()


### Differences in MM

base_model_diff <- cj(data,
                      chosen ~ crime,
                      id = ~id,
                      estimate = "mm_diff",
                      by = ~ideo_di,
                      alpha = .05,
                      h0 = 0.5)


#### FIGURE 2 - PART 2
#jpeg("figure2_part2.jpeg", res=600, width=2500, height=1250)
ggplot(base_model_diff, aes(x = level, y = estimate)) +
  geom_hline(yintercept = 0.0, colour = gray(0.7), lty = 1) + 
  geom_point(position=position_dodge(0.3)) +
  geom_errorbar(aes(x = level, 
                    ymin = lower,
                    ymax = upper),
                width = .1,
                alpha = 0.8,
                position=position_dodge(0.3),
                lwd = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = label_number(accuracy = 0.01),
                     breaks = c(-0.03, 0.00, 0.03)) +
  labs(x = "Crime Policy",
       y = "Difference in Marginal Mean") +
  theme_bw() +
  theme(text = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))
#dev.off()




########################### VICTIMIZATION MODELS ###############################

#pooled
vic_simp <- cj(data, chosen ~ crime,
               id = ~id, estimate = "mm",
               by = ~vic, alpha = .05)

#progressives
vic_prog_simp <- cj(prog_ideo, chosen ~ crime,
                        id = ~id, estimate = "mm",
                        by = ~vic, alpha = .05)

#conserviatves
vic_cons_simp <- cj(cons_ideo, chosen ~ crime,
                        id = ~id, estimate = "mm",
                        by = ~vic, alpha = .05)

### combining
vic_simp$sample <- "Pooled"
vic_prog_simp$sample <- "Progressives"
vic_cons_simp$sample <- "Conservatives"
vic_total <- rbind(vic_simp, vic_prog_simp, vic_cons_simp)
vic_total$sample <- factor(vic_total$sample, 
                           levels = c("Pooled", "Progressives",
                                      "Conservatives"),
                           ordered = T)

##### FIGURE 3 - PART 1
#jpeg("figure3_part1.jpeg", res=600, width=6000, height=2000)
ggplot(vic_total, aes(x = level, y = estimate, linetype = BY)) +
  geom_hline(yintercept = 0.5, colour = gray(0.7), lty = 1) + 
  geom_point(position=position_dodge(0.2)) +
  geom_errorbar(aes(x = level, 
                    ymin = lower,
                    ymax = upper,
                    linetype = BY),
                width = .3,
                alpha = 0.8,
                position=position_dodge(0.2),
                lwd = 0.7) + 
  facet_grid(~sample) +
  coord_flip() +
  scale_y_continuous(labels = label_number(accuracy = 0.10),
                     breaks = c(0.45, 0.5, 0.55)) +
  scale_linetype_discrete(
                      name="Victimization",
                      labels =c("Non-Victim", "Victim")) +
  labs(x = "Crime Policy",
       y = "Marginal Mean") +
  theme_bw() +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.box.background = element_rect(colour = "black"))
#dev.off()

### Differences in MM

# pooled
vic_diff <- cj(data, chosen ~ crime,
               id = ~id, estimate = "mm_diff",
               by = ~vic, alpha = 0.05)

# progressives
vic_prog_diff <- cj(prog_ideo, chosen ~ crime,
                        id = ~id, estimate = "mm_diff",
                        by = ~vic, alpha = .05)

# consrevatives
vic_cons_diff <- cj(cons_ideo, chosen ~ crime,
                        id = ~id, estimate = "mm_diff",
                        by = ~vic, alpha = .05)



### combining diff
vic_diff$sample <- "Pooled"
vic_prog_diff$sample <- "Progressives"
vic_cons_diff$sample <- "Conservatives"
vic_diff_total <- rbind(vic_diff, vic_prog_diff, vic_cons_diff)
vic_diff_total$sample <- factor(vic_diff_total$sample, 
                                levels = c("Pooled", "Progressives",
                                           "Conservatives"),
                                ordered = T)


#### FIGURE 3 - PART 2
#jpeg("figure3_part2.jpeg", res=600, width=6000, height=1500)
ggplot(vic_diff_total, aes(x = level, y = estimate)) +
  geom_hline(yintercept = 0.0, colour = gray(0.7), lty = 1) + 
  geom_point(position=position_dodge(0.3)) +
  geom_errorbar(aes(x = level, 
                    ymin = lower,
                    ymax = upper),
                width = .1,
                alpha = 0.8,
                position=position_dodge(0.3),
                lwd = 0.7) +
  facet_grid(~sample) +
  coord_flip() +
  scale_y_continuous(labels = label_number(accuracy = 0.01),
                     breaks = c(-0.04, 0.00, 0.04)) +
  labs(x = "Crime Policy",
       y = "Difference in Marginal Mean") +
  theme_bw() +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))
#dev.off()




######################## MODELS: COMMUNITY SAFETY ##########################

# pooled
safety_simp <- cj(data,
                  chosen ~ crime,
                  id = ~id,
                  estimate = "mm",
                  by = ~safety_di,
                  alpha = 0.05)

# progressives
safety_prog_simp <- cj(prog_ideo,
                           chosen ~ crime,
                           id = ~id,
                           estimate = "mm",
                           by = ~safety_di,
                           alpha = 0.05)

# conservatives
safety_cons_simp <- cj(cons_ideo,
                           chosen ~ crime,
                           id = ~id,
                           estimate = "mm",
                           by = ~safety_di,
                           alpha = 0.05)

### combining
safety_simp$sample <- "Pooled"
safety_prog_simp$sample <- "Progressives"
safety_cons_simp$sample <- "Conservatives"

safety_total <- rbind(safety_simp, safety_prog_simp, safety_cons_simp)
safety_total$sample <- factor(safety_total$sample, 
                              levels = c("Pooled", "Progressives",
                                         "Conservatives"),
                              ordered = T)

####### FIGURE 4 - PART 1
#jpeg("figure4_part1.jpeg", res=600, width=6000, height=2000)
ggplot(safety_total, aes(x = level, y = estimate, linetype = BY)) +
  geom_hline(yintercept = 0.5, colour = gray(0.7), lty = 1) + 
  geom_point(position=position_dodge(0.3)) +
  geom_errorbar(aes(x = level, 
                    ymin = lower,
                    ymax = upper,
                    linetype = BY),
                width = .3,
                alpha = 0.8,
                position=position_dodge(0.3),
                lwd = 0.7) + 
  facet_grid(~sample) +
  coord_flip() +
  scale_y_continuous(labels = label_number(accuracy = 0.10),
                     breaks = c(0.45, 0.5, 0.55)) +
  scale_linetype_discrete(
                      name="Neighb. Safety",
                      labels =c("Secure", "Insecure")) +
  labs(x = "Crime Policy",
       y = "Marginal Mean") +
  theme_bw() +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.box.background = element_rect(colour = "black"))
#dev.off()


#differences in MM

# pooled
safety_diff <- cj(data,
                  chosen ~ crime,
                  id = ~id,
                  estimate = "mm_diff",
                  by = ~safety_di,
                  alpha = 0.05)

# progressives
safety_prog_diff <- cj(prog_ideo,
                           chosen ~ crime,
                           id = ~id,
                           estimate = "mm_diff",
                           by = ~safety_di,
                           alpha = 0.05)

# conservatives
safety_cons_diff <- cj(cons_ideo,
                           chosen ~ crime,
                           id = ~id,
                           estimate = "mm_diff",
                           by = ~safety_di,
                           alpha = 0.05)


### combining diff
safety_diff$sample <- "Pooled"
safety_prog_diff$sample <- "Progressives"
safety_cons_diff$sample <- "Conservatives"
safety_diff_total <- rbind(safety_diff, safety_prog_diff, safety_cons_diff)
safety_diff_total$sample <- factor(safety_diff_total$sample, 
                                   levels = c("Pooled",
                                              "Progressives",
                                              "Conservatives"),
                                   ordered = T)


#### FIGURE 4 - PART 2
#jpeg("figure4_part2.jpeg", res=600, width=6000, height=1500)
ggplot(safety_diff_total, aes(x = level, y = estimate)) +
  geom_hline(yintercept = 0.0, colour = gray(0.7), lty = 1) + 
  geom_point(position=position_dodge(0.3)) +
  geom_errorbar(aes(x = level, 
                    ymin = lower,
                    ymax = upper),
                width = .1,
                alpha = 0.8,
                position=position_dodge(0.3),
                lwd = 0.7) +
  facet_grid(~sample) +
  coord_flip() +
  scale_y_continuous(labels = label_number(accuracy = 0.01),
                     breaks = c(-0.04, 0.00, 0.04)) +
  labs(x = "Crime Policy",
       y = "Difference in Marginal Mean") +
  theme_bw() +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))
#dev.off()

######################### MODELS: PERCEPTIONS OF SOCIAL ASSISTANCE #############

# pooled
assist_simp <- cj(data,
                  chosen ~ crime,
                  id = ~id,
                  estimate = "mm",
                  by = ~assist_effect,
                  alpha = .05)

# progressives 
assist_prog_simp <- cj(prog_ideo,
                           chosen ~ crime,
                           id = ~id,
                           estimate = "mm",
                           by = ~assist_effect,
                           alpha = .05)

# conservatives 
assist_cons_simp <- cj(cons_ideo,
                           chosen ~ crime,
                           id = ~id,
                           estimate = "mm",
                           by = ~assist_effect,
                           alpha = .05)


### combining
assist_simp$sample <- "Pooled"
assist_prog_simp$sample <- "Progressives"
assist_cons_simp$sample <- "Conservatives"
assist_total <- rbind(assist_simp, assist_prog_simp, assist_cons_simp)
assist_total$sample <- factor(assist_total$sample, 
                              levels = c("Pooled", "Progressives",
                                         "Conservatives"),
                              ordered = T)
## FIGURE 5 - PART 1
jpeg("figure5_part1.jpeg", res=600, width=6000, height=2000)
ggplot(assist_total, aes(x = level, y = estimate, linetype = BY)) +
  geom_hline(yintercept = 0.5, colour = gray(0.7), lty = 1) + 
  geom_point(position=position_dodge(0.3)) +
  geom_errorbar(aes(x = level, 
                    ymin = lower,
                    ymax = upper,
                    linetype = BY),
                width = .3,
                alpha = 0.8,
                position=position_dodge(0.3),
                lwd = 0.7) + 
  facet_grid(~sample) +
  coord_flip() +
  scale_y_continuous(labels = label_number(accuracy = 0.10),
                     breaks = c(0.45, 0.5, 0.55)) +
  scale_linetype_discrete(
                      name="Soc. Assistance Effective?",
                      labels =c("No", "Yes")) +
  labs(x = "Crime Policy",
       y = "Marginal Mean") +
  theme_bw() +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.box.background = element_rect(colour = "black"))
 dev.off()


#differences in MM

# pooled
assist_diff <- cj(data,
                  chosen ~ crime,
                  id = ~id,
                  estimate = "mm_diff",
                  by = ~assist_effect,
                  alpha = .05)

# progressives
assist_prog_diff <- cj(prog_ideo,
                           chosen ~ crime,
                           id = ~id,
                           estimate = "mm_diff",
                           by = ~assist_effect,
                           alpha = .05)

# conservatives
assist_cons_diff <- cj(cons_ideo,
                           chosen ~ crime,
                           id = ~id,
                           estimate = "mm_diff",
                           by = ~assist_effect,
                           alpha = 0.05)



### combining diff
assist_diff$sample <- "Pooled"
assist_prog_diff$sample <- "Progressives"
assist_cons_diff$sample <- "Conservatives"
assist_diff_total <- rbind(assist_diff, assist_prog_diff, assist_cons_diff)
assist_diff_total$sample <- factor(assist_diff_total$sample, 
                                   levels = c("Pooled", "Progressives",
                                              "Conservatives"),
                                   ordered = T)


#### FIGURE 5 - PART 2
#jpeg("figure5_part2.jpeg", res=600, width=6000, height=1500)
ggplot(assist_diff_total, aes(x = level, y = estimate)) +
  geom_hline(yintercept = 0.0, colour = gray(0.7), lty = 1) + 
  geom_point(position=position_dodge(0.3)) +
  geom_errorbar(aes(x = level, 
                    ymin = lower,
                    ymax = upper),
                width = .1,
                alpha = 0.8,
                position=position_dodge(0.3),
                lwd = 0.7) +
  facet_grid(~sample) +
  coord_flip() +
  scale_y_continuous(labels = label_number(accuracy = 0.01),
                     breaks = c(-0.06, 0.00, 0.06)) +
  labs(x = "Crime Policy",
       y = "Difference in Marginal Mean") +
  theme_bw() +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))
#dev.off()

######################### CRIME DRIVEN BY GANGS ###############################

# pooled
gang_simp <- cj(data,
                chosen ~ crime,
                id = ~id,
                estimate = "mm",
                by = ~crime_gang,
                alpha = .05)

# progressives
gang_prog_simp <- cj(prog_ideo, 
                     chosen ~ crime,
                     id = ~id,
                     estimate = "mm",
                     by = ~crime_gang,
                     alpha = .05)

# conservatives
gang_cons_simp <- cj(cons_ideo, 
                     chosen ~ crime,
                     id = ~id,
                     estimate = "mm",
                     by = ~crime_gang,
                     alpha = .05)



### combining
gang_simp$sample <- "Pooled"
gang_prog_simp$sample <- "Progressives"
gang_cons_simp$sample <- "Conservatives"
gang_total <- rbind(gang_simp, gang_prog_simp,
                    gang_cons_simp)
gang_total$sample <- factor(gang_total$sample, 
                            levels = c("Pooled",
                                       "Progressives",
                                       "Conservatives"),
                            ordered = T)

## FIGURE 6 - PART 1
#jpeg("figure6_part1.jpeg", res=600, width=6000, height=2000)
ggplot(gang_total, aes(x = level, y = estimate, linetype = BY)) +
  geom_hline(yintercept = 0.5, colour = gray(0.7), lty = 1) + 
  geom_point(position=position_dodge(0.3)) +
  geom_errorbar(aes(x = level, 
                    ymin = lower,
                    ymax = upper,
                    linetype = BY),
                width = .3,
                alpha = 0.8,
                position=position_dodge(0.3),
                lwd = 0.7) + 
  facet_grid(~sample) +
  coord_flip() +
  scale_y_continuous(labels = label_number(accuracy = 0.10),
                     breaks = c(0.45, 0.5, 0.55)) +
  scale_linetype_discrete(
                      name="Crime by Gangs?",
                      labels =c("No", "Yes")) +
  labs(x = "Crime Policy",
       y = "Marginal Mean") +
  theme_bw() +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.box.background = element_rect(colour = "black"))
#dev.off()

#differences in MM

# pooled
gang_diff <- cj(data,
                chosen ~ crime,
                id = ~id,
                estimate = "mm_diff",
                by = ~crime_gang,
                alpha = 0.05)

# progressives
gang_prog_diff <- cj(prog_ideo,
                     chosen ~ crime,
                     id = ~id,
                     estimate = "mm_diff",
                     by = ~crime_gang,
                     alpha = 0.05)

# conservatives
gang_cons_diff <- cj(cons_ideo,
                           chosen ~ crime,
                           id = ~id,
                           estimate = "mm_diff",
                           by = ~crime_gang,
                           alpha = 0.05)


### combining diff 
gang_diff$sample <- "Pooled"
gang_prog_diff$sample <- "Progressives"
gang_cons_diff$sample <- "Conservatives"
gang_diff_total <- rbind(gang_diff,
                         gang_prog_diff,
                         gang_cons_diff)
gang_diff_total$sample <- factor(gang_diff_total$sample, 
                                 levels = c("Pooled",
                                            "Progressives",
                                            "Conservatives"),
                                 ordered = T)

## FIGURE 6 - PART 2
#jpeg("figure6_part2.jpeg", res=600, width=6000, height=1500)
ggplot(gang_diff_total, aes(x = level, y = estimate)) +
  geom_hline(yintercept = 0.0, colour = gray(0.7), lty = 1) + 
  geom_point(position=position_dodge(0.3)) +
  geom_errorbar(aes(x = level, 
                    ymin = lower,
                    ymax = upper),
                width = .1,
                alpha = 0.8,
                position=position_dodge(0.3),
                lwd = 0.7) +
  facet_grid(~sample) +
  coord_flip() +
  scale_y_continuous(labels = label_number(accuracy = 0.01),
                     breaks = c(-0.04, 0.00, 0.04)) +
  labs(x = "Crime Policy",
       y = "Difference in Marginal Mean") +
  theme_bw() +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))
#dev.off()

