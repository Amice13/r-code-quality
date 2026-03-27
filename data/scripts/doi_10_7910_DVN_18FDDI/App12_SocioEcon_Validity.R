################################################################################
#### Title: Progressive Ideology and Support for Punitive Crime Policy
#### Author: Isabel Laterzo
#### Year: 2023
#### Journal: Comparative Political Studies
#### Portion of Analysis: Appendix 12 - SocioEcon Ideology Validity
################################################################################


######################################## SET UP ################################

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

levels(data$crime)[levels(data$crime)=="Tough on Crime"] <- "Punitive"

#socio economic categories for each respondent from survey 
socio_econ_cats <- readRDS("socioecon_list_IDs.RDS")

#combine

data_final <- merge(data, socio_econ_cats, by = "id")

data <- data_final

############################### CONJOINT SET UP ################################

#safety variable
#4 insecure, 1 secure --> 1 = unsafe, 0 = safe
data$safety_di <- factor(ifelse(data$safety_neighb == 4, "1", 
                                ifelse(data$safety_neighb == 3, "1", 
                                       ifelse(data$safety_neighb == 2, "0", 
                                              ifelse(data$safety_neighb == 1, "0", NA)))))


#variable for prog/conserv based on median index value
data$ideo_di <- factor(ifelse(data$ideo >= 0, "Prog", "Conserv"))


# Create socio econ subsamples

high_mid <- data %>% filter(nse_group != "1. Low") # those in high and mid cats

low_mid <- data %>% filter(nse_group != "3. High") # those in low and mid cats


##create data frames for prog vs. conserv on social and econ issues based on
## median value

#high mid
prog_ideo_hm <- high_mid %>% filter(ideo >= 0) #same as not including zero
cons_ideo_hm <- high_mid %>% filter(ideo < 0) #same as not including zero

#low mid
prog_ideo_lm <- low_mid %>% filter(ideo >= 0) #same as not including zero
cons_ideo_lm <- low_mid %>% filter(ideo < 0) #same as not including zero




########################### BASE MODELS ########################################


##### BASELINE MODEL: Divided by ideology ------------------
base_model_hm <- cj(high_mid,
                 chosen ~ crime,
                 id = ~id,
                 estimate = "mm",
                 by = ~ideo_di,
                 alpha = .05,
                 h0 = 0.5)

base_model_lm <- cj(low_mid,
                    chosen ~ crime,
                    id = ~id,
                    estimate = "mm",
                    by = ~ideo_di,
                    alpha = .05,
                    h0 = 0.5)



# uncomment jpeg() and dev.off() to save images in working directory

# Figure A12.1
#jpeg("app_figA12-1.jpeg", res=600, width=2500, height=1500)
ggplot(base_model_hm, aes(x = level, y = estimate, color = BY)) +
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
  coord_flip() +
  scale_y_continuous(labels = label_number(accuracy = 0.10),
                     breaks = c(0.45, 0.5, 0.55)) +
  scale_colour_brewer(palette = "Set1",
                      name="Ideology",
                      labels = c("Conservative", 'Progressive')) +
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

# Figure A12.2
#jpeg("app_figA12-2.jpeg", res=600, width=2500, height=1500)
ggplot(base_model_lm, aes(x = level, y = estimate, color = BY)) +
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
  coord_flip() +
  scale_y_continuous(labels = label_number(accuracy = 0.10),
                     breaks = c(0.45, 0.5, 0.55)) +
  scale_colour_brewer(palette = "Set1",
                      name="Ideology",
                      labels = c("Conservative", 'Progressive')) +
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




######################## MODELS: VICTIMIZATION ##########################

### HIGH MID ###

#models
vic_hm <- cj(high_mid,
                chosen ~ crime,
                id = ~id,
                estimate = "mm",
                by = ~vic,
                alpha = 0.05)

vic_prog_hm <- cj(prog_ideo_hm,
                     chosen ~ crime,
                     id = ~id,
                     estimate = "mm",
                     by = ~vic,
                     alpha = 0.05)


vic_cons_hm <- cj(cons_ideo_hm,
                     chosen ~ crime,
                     id = ~id,
                     estimate = "mm",
                     by = ~vic,
                     alpha = 0.05)

### combining
vic_hm$sample <- "Pooled"
vic_prog_hm$sample <- "Progressives"
vic_cons_hm$sample <- "Conservatives"

vic_total_hm <- rbind(vic_hm, vic_prog_hm, vic_cons_hm)
vic_total_hm$sample <- factor(vic_total_hm$sample, 
                                 levels = c("Pooled", "Progressives",
                                            "Conservatives"),
                                 ordered = T)

# Figure A12.3
#jpeg("app_figA12-3.jpeg", res=600, width=6000, height=2000)
ggplot(vic_total_hm, aes(x = level, y = estimate, color = BY)) +
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
  scale_y_continuous(labels = label_number(accuracy = 0.10),
                     breaks = c(0.45, 0.5, 0.55)) +
  scale_colour_brewer(palette = "Set1",
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


### LOW MID ###

#models
vic_lm <- cj(low_mid,
             chosen ~ crime,
             id = ~id,
             estimate = "mm",
             by = ~vic,
             alpha = 0.05)

vic_prog_lm <- cj(prog_ideo_lm,
                  chosen ~ crime,
                  id = ~id,
                  estimate = "mm",
                  by = ~vic,
                  alpha = 0.05)


vic_cons_lm <- cj(cons_ideo_lm,
                  chosen ~ crime,
                  id = ~id,
                  estimate = "mm",
                  by = ~vic,
                  alpha = 0.05)

### combining
vic_lm$sample <- "Pooled"
vic_prog_lm$sample <- "Progressives"
vic_cons_lm$sample <- "Conservatives"

vic_total_lm <- rbind(vic_lm, vic_prog_lm, vic_cons_lm)
vic_total_lm$sample <- factor(vic_total_lm$sample, 
                              levels = c("Pooled", "Progressives",
                                         "Conservatives"),
                              ordered = T)

# Figure A12.4
#jpeg("app_figA12-4.jpeg", res=600, width=6000, height=2000)
ggplot(vic_total_lm, aes(x = level, y = estimate, color = BY)) +
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
  scale_y_continuous(labels = label_number(accuracy = 0.10),
                     breaks = c(0.45, 0.5, 0.55)) +
  scale_colour_brewer(palette = "Set1",
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



######################## MODELS: COMMUNITY SAFETY ##########################

### HIGH MID ###

#models
safety_hm <- cj(high_mid,
                  chosen ~ crime,
                  id = ~id,
                  estimate = "mm",
                  by = ~safety_di,
                  alpha = 0.05)

safety_prog_hm <- cj(prog_ideo_hm,
                       chosen ~ crime,
                       id = ~id,
                       estimate = "mm",
                       by = ~safety_di,
                       alpha = 0.05)


safety_cons_hm <- cj(cons_ideo_hm,
                       chosen ~ crime,
                       id = ~id,
                       estimate = "mm",
                       by = ~safety_di,
                       alpha = 0.05)

### combining
safety_hm$sample <- "Pooled"
safety_prog_hm$sample <- "Progressives"
safety_cons_hm$sample <- "Conservatives"

safety_total_hm <- rbind(safety_hm, safety_prog_hm, safety_cons_hm)
safety_total_hm$sample <- factor(safety_total_hm$sample, 
                              levels = c("Pooled", "Progressives",
                                         "Conservatives"),
                              ordered = T)

# Figure A12.5
#jpeg("app_figA12-5.jpeg", res=600, width=6000, height=2000)
ggplot(safety_total_hm, aes(x = level, y = estimate, color = BY)) +
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
  scale_y_continuous(labels = label_number(accuracy = 0.10),
                     breaks = c(0.45, 0.5, 0.55)) +
  scale_colour_brewer(palette = "Set1",
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


### LOW MID ###

#models
safety_lm <- cj(low_mid,
                chosen ~ crime,
                id = ~id,
                estimate = "mm",
                by = ~safety_di,
                alpha = 0.05)

safety_prog_lm <- cj(prog_ideo_lm,
                     chosen ~ crime,
                     id = ~id,
                     estimate = "mm",
                     by = ~safety_di,
                     alpha = 0.05)


safety_cons_lm <- cj(cons_ideo_lm,
                     chosen ~ crime,
                     id = ~id,
                     estimate = "mm",
                     by = ~safety_di,
                     alpha = 0.05)

### combining
safety_lm$sample <- "Pooled"
safety_prog_lm$sample <- "Progressives"
safety_cons_lm$sample <- "Conservatives"

safety_total_lm <- rbind(safety_lm, safety_prog_lm, safety_cons_lm)
safety_total_lm$sample <- factor(safety_total_lm$sample, 
                                 levels = c("Pooled", "Progressives",
                                            "Conservatives"),
                                 ordered = T)

# Figure A12.6
#jpeg("app_figA12-6.jpeg", res=600, width=6000, height=2000)
ggplot(safety_total_lm, aes(x = level, y = estimate, color = BY)) +
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
  scale_y_continuous(labels = label_number(accuracy = 0.10),
                     breaks = c(0.45, 0.5, 0.55)) +
  scale_colour_brewer(palette = "Set1",
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


######################## MODELS: CRIME - GANGS ##########################

### HIGH MID ###

#models
gang_hm <- cj(high_mid,
                chosen ~ crime,
                id = ~id,
                estimate = "mm",
                by = ~crime_gang,
                alpha = 0.05)

gang_prog_hm <- cj(prog_ideo_hm,
                     chosen ~ crime,
                     id = ~id,
                     estimate = "mm",
                     by = ~crime_gang,
                     alpha = 0.05)


gang_cons_hm <- cj(cons_ideo_hm,
                     chosen ~ crime,
                     id = ~id,
                     estimate = "mm",
                     by = ~crime_gang,
                     alpha = 0.05)

### combining
gang_hm$sample <- "Pooled"
gang_prog_hm$sample <- "Progressives"
gang_cons_hm$sample <- "Conservatives"

gang_total_hm <- rbind(gang_hm, gang_prog_hm, gang_cons_hm)
gang_total_hm$sample <- factor(gang_total_hm$sample, 
                                 levels = c("Pooled", "Progressives",
                                            "Conservatives"),
                                 ordered = T)

# Figure A12.7
#jpeg("app_figA12-7.jpeg", res=600, width=6000, height=2000)
ggplot(gang_total_hm, aes(x = level, y = estimate, color = BY)) +
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
  scale_y_continuous(labels = label_number(accuracy = 0.10),
                     breaks = c(0.45, 0.5, 0.55)) +
  scale_colour_brewer(palette = "Set1",
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


### LOW MID ###

#models
gang_lm <- cj(low_mid,
              chosen ~ crime,
              id = ~id,
              estimate = "mm",
              by = ~crime_gang,
              alpha = 0.05)

gang_prog_lm <- cj(prog_ideo_lm,
                   chosen ~ crime,
                   id = ~id,
                   estimate = "mm",
                   by = ~crime_gang,
                   alpha = 0.05)


gang_cons_lm <- cj(cons_ideo_lm,
                   chosen ~ crime,
                   id = ~id,
                   estimate = "mm",
                   by = ~crime_gang,
                   alpha = 0.05)

### combining
gang_lm$sample <- "Pooled"
gang_prog_lm$sample <- "Progressives"
gang_cons_lm$sample <- "Conservatives"

gang_total_lm <- rbind(gang_lm, gang_prog_lm, gang_cons_lm)
gang_total_lm$sample <- factor(gang_total_lm$sample, 
                               levels = c("Pooled", "Progressives",
                                          "Conservatives"),
                               ordered = T)

# Figure A12.8
#jpeg("app_figA12-8.jpeg", res=600, width=6000, height=2000)
ggplot(gang_total_lm, aes(x = level, y = estimate, color = BY)) +
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
  scale_y_continuous(labels = label_number(accuracy = 0.10),
                     breaks = c(0.45, 0.5, 0.55)) +
  scale_colour_brewer(palette = "Set1",
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




