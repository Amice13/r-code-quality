################################################################################
#### Title: Progressive Ideology and Support for Punitive Crime Policy
#### Author: Isabel Laterzo
#### Year: 2023
#### Journal: Comparative Political Studies
#### Portion of Analysis: Appendix 9 Part 2- Ideological Index Validity
################################################################################

########################## SET UP ###############################################


rm(list=ls()) #clear global environ


# uncomment below and set working directory using setwd() to directory which contains
# all data files
#setwd()

#load in necessary packages
library(tidyverse)
library(stargazer)

#read in data
data <- readRDS("laterzo_cps_2023c.RDS") %>%
  select(id, ideo, social, economy, pol_04, pol_05) %>%
  unique() %>%
  select(-id)

#make ideology variables into numerics
data$pol_04 <- as.numeric(data$pol_04)
data$pol_05 <- as.numeric(data$pol_05)


#create left-right measure 
# use pol_04 measure, but if respondents answered "none" give them
# the response from pol_05 
# note that 878 individuals responded "none" (6) to pol_04
# and 751 responded "none" (7) to pol_05 and 184 "don't know" (8) to pol_05
data$lr <- ifelse(data$pol_04 == 6, data$pol_05,
                  data$pol_04)

# code remaining "none" and "don't knows" as NAs
data$lr <- ifelse(data$lr == 7 | data$lr == 8, NA, data$lr)

#flip scales so left is low and right is high
data$lr <- ifelse(data$lr == 5, 1,
                  ifelse(data$lr == 4, 2,
                         ifelse(data$lr == 3, 3,
                                ifelse(data$lr == 2, 4,
                                       ifelse(data$lr == 1, 5, data$lr)))))


# Panels for Figure A9.3: Relationship between Ideological Measures and Left-Right Identification

# First Panel - Combined Ideology dimension
#jpeg("figA9_3_part1.jpeg", res=600, width=4000, height=4000)
ggplot(data, aes(x = lr, y = ideo)) +
  geom_jitter(alpha = 0.2) +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Left-Right Identification",
       y = "Combined Ideology CFA Dimension") +
  scale_x_continuous(breaks = 1:5,
                     labels = c("Left", "Center-Left",
                                "Center", "Center-Right",
                                "Right"))
#dev.off()



# Second Panel - Economic CFA Dimension
#jpeg("figA9_3_part2.jpeg", res=600, width=4000, height=4000)
ggplot(data, aes(x = lr, y = economy)) +
  geom_jitter(alpha = 0.2) +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Left-Right Identification",
       y = "Economic CFA Dimension") +
  scale_x_continuous(breaks = 1:5,
                     labels = c("Left", "Center-Left",
                                "Center", "Center-Right",
                                "Right"))
#dev.off()

# Third Panel - Sociocultural CFA Dimension
#jpeg("figA9_3_part3.jpeg", res=600, width=4000, height=4000)
ggplot(data, aes(x = lr, y = social)) +
  geom_jitter(alpha = 0.2) +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Left-Right Identification",
       y = "Sociocultural CFA Dimension") +
  scale_x_continuous(breaks = 1:5,
                     labels = c("Left", "Center-Left",
                                "Center", "Center-Right",
                                "Right"))
#dev.off()

# linear models for Table A9.5 
# examine relationship between ideology measures and left-right self placement

combined <- lm(ideo ~ lr, data = data)
sociocultural <- lm(social ~ lr, data = data)
economic <- lm(economy ~ lr, data = data)

# Table A9.5: Linear Regression Results for Left-Right Self Identification and CFA Created  Ideology Measures

stargazer(combined, sociocultural, economic,
          covariate.labels = c("Left-Right Dimension",
                               "Constant"),
          dep.var.labels = c("Combined", "Sociocultural", "Economic"),
          model.numbers = FALSE,
          no.space = TRUE,
          font.size = "tiny")
