################################################################################
#### Title: Progressive Ideology and Support for Punitive Crime Policy
#### Author: Isabel Laterzo
#### Year: 2023
#### Journal: Comparative Political Studies
#### Portion of Analysis: Appendix 16 - Victimization Analysis (Figure A16.1)
################################################################################


################################## SET UP ######################################

rm(list=ls()) #clear global environ


# uncomment below and set working directory using setwd() to directory which contains
# all data files
#setwd()


#packages
library(tidyverse)
library(cregg)


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

#make close vic a factor

data$close_vic <- as.factor(data$close_vic)

##create data frames for prog vs. conserv  based on centering value
prog_ideo <- data %>% filter(ideo >= 0) #same as not including zero
cons_ideo <- data %>% filter(ideo < 0) #same as not including zero


#number of victims/close family victims
close_vic <- data %>%
  select(id, close_vic, vic) %>%
  unique()
table(close_vic$close_vic)
table(close_vic$vic)


########################### MODELS: CLOSE VICTIMIZATION ###############################

#pooled
vicclose_simp <- cj(data, chosen ~ crime,
               id = ~id, estimate = "mm",
               by = ~close_vic, alpha = .05)

#progressives
vicclose_prog_simp <- cj(prog_ideo, chosen ~ crime,
                    id = ~id, estimate = "mm",
                    by = ~close_vic, alpha = .05)

#conserviatves
vicclose_cons_simp <- cj(cons_ideo, chosen ~ crime,
                    id = ~id, estimate = "mm",
                    by = ~close_vic, alpha = .05)

### combining
vicclose_simp$sample <- "Pooled"
vicclose_prog_simp$sample <- "Progressive"
vicclose_cons_simp$sample <- "Conservative"
vicclose_total <- rbind(vicclose_simp, vicclose_prog_simp, vicclose_cons_simp)
vicclose_total$sample <- factor(vicclose_total$sample, 
                           levels = c("Pooled", "Progressive",
                                      "Conservative"),
                           ordered = T)

##### FIGURE A16.1
#jpeg("figA16_part1.jpeg", res=600, width=6000, height=2000)
ggplot(vicclose_total, aes(x = level, y = estimate, color = BY)) +
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
  scale_y_continuous(labels = label_number(accuracy = 0.10),
                     breaks = c(0.45, 0.5, 0.55)) +
  scale_colour_brewer(palette = "Set1",
                      name="Victimization: Self and/or Family",
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
vicclose_diff <- cj(data, chosen ~ crime,
               id = ~id, estimate = "mm_diff",
               by = ~close_vic, alpha = 0.05)

# progressives
vicclose_prog_diff <- cj(prog_ideo, chosen ~ crime,
                    id = ~id, estimate = "mm_diff",
                    by = ~close_vic, alpha = .05)

# consrevatives
vicclose_cons_diff <- cj(cons_ideo, chosen ~ crime,
                    id = ~id, estimate = "mm_diff",
                    by = ~close_vic, alpha = .05)



### combining diff
vicclose_diff$sample <- "Pooled"
vicclose_prog_diff$sample <- "Progressives"
vicclose_cons_diff$sample <- "Conservatives"
vicclose_diff_total <- rbind(vicclose_diff, vicclose_prog_diff, vicclose_cons_diff)
vicclose_diff_total$sample <- factor(vicclose_diff_total$sample, 
                                levels = c("Pooled", "Progressives",
                                           "Conservatives"),
                                ordered = T)


#### FIGURE A16.2
#jpeg("figA16_part2.jpeg", res=600, width=6000, height=1500)
ggplot(vicclose_diff_total, aes(x = level, y = estimate)) +
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
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))
#dev.off()


# For lapop analysis of victimization, see 2b_lapop_analysis