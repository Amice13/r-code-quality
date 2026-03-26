
rm(list=ls())

library(ggplot2)
library(tidyverse)
library(extrafont)
library(sf)
library(cshapes)
library(ggpubr)

setwd("")

###################
## Load the data
###################

data <- read.csv("data/final_data.csv")

###################
## Prep the data
###################

data <- subset(data, data$year <= 2015)

length(unique(data$obsid))
length(unique(data$ccode))

idx <- c(1, diff(data$year))
i2 <- c(1, which(idx != 1), nrow(data)+1)
data$grp <- rep(1:length(diff(i2)), diff(i2))

theme_set(theme_classic())
theme_update(legend.position = "none", text = element_text(family = "Times New Roman", face = "bold", size = 12))

###################
## Figure 1
###################

cshp <- cshp(date = NA, useGW = TRUE, dependencies = FALSE)

cshp$syear <- substr(cshp$start, 0, 4)
cshp$eyear <- substr(cshp$end, 0, 4)

cshp$year <- mapply(seq, cshp$syear, cshp$eyear, SIMPLIFY = FALSE)

cshp <- cshp %>% unnest(year)

cshp$unique <- paste(cshp$gwcode, cshp$year, sep = "-")

cshp <- cshp[!duplicated(cshp$unique),]

cshp <- select(cshp, c("gwcode", "year", "caplong", "caplat", "geometry"))
cshp <- dplyr::rename(cshp, ccode = gwcode)

cshp1960 <- subset(cshp, cshp$year == 1960)
cshp2015 <- subset(cshp, cshp$year == 2015)

temp <- select(data, c("ccode", "year", "startdate_byyr", "lnave_newsnum"))
temp$group <- paste(temp$ccode, temp$year, sep = "-")
temp <- temp %>% group_by(group) %>% mutate(max_startdate_byyr = max(startdate_byyr))
temp <- subset(temp, startdate_byyr == max_startdate_byyr)

temp1960 <- subset(temp, temp$year == 1960)
temp2015 <- subset(temp, temp$year == 2015)

map1960 <- cshp1960 %>%
  left_join(temp1960, by = c("ccode")) %>%
  st_transform(4326)

map2015 <- cshp2015 %>%
  left_join(temp2015, by = c("ccode")) %>%
  st_transform(4326)

A <- ggplot(data = map1960) +
  geom_sf(aes(fill = lnave_newsnum)) + 
  scale_fill_gradient(low = "white", high = "purple4", limits = c(0, 8)) + ggtitle("1960") + 
  labs(fill = "Ln(Ave. # Newsp.  Mentions)") + 
  theme(legend.position = "bottom", 
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank())

B<- ggplot(data = map2015) +
  geom_sf(aes(fill = lnave_newsnum)) + 
  scale_fill_gradient(low = "white", high = "purple4", limits = c(0, 8)) + ggtitle("2015") + 
  labs(fill = "Ln(Ave. # Newsp.  Mentions)") + 
  theme(legend.position = "bottom", 
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank())

Figure <- ggarrange(A, B,
                    common.legend = TRUE,
                    legend = c("bottom"),
                    ncol = 1)

Figure

ggsave("results/Figure_worldwide.png", device = "png", width = 6, height = 6, dpi = 800)

###################
## Figure A1
###################

ctryname <- c("Saudi Arabia", "Venezuela")
DATA <- data[(data$country_name %in% ctryname),]
DATA <- select(DATA, c("country_name", "grp", "year", "ave_newsnum", "ave_humanrights_newsnum", "hrs_violations", "ideal_diff", "syear"))

A <- ggplot(DATA[DATA$year >= 2000,], aes(x = year, color = country_name)) + 
  geom_line(aes(y = ave_newsnum, group = grp)) + xlab("Year") + ylab("Ave. # Newsp. Mentions") + 
  ylim(0, 500) + xlim(2000, 2015) +
  theme(legend.position = "bottom") + labs(color = "Country") + scale_color_manual(values = c("Saudi Arabia" = "black", "Venezuela" = "red"))

B <- ggplot(DATA[DATA$year >= 2000,], aes(x = year, color = country_name)) + 
  geom_line(aes(y = ave_humanrights_newsnum, group = grp)) + xlab("Year") + ylab("Ave. # Newsp. Human Rights Mentions") +
  ylim(0, 50) + xlim(2000, 2015) +
  theme(legend.position = "bottom") + labs(color = "Country") + scale_color_manual(values = c("Saudi Arabia" = "black", "Venezuela" = "red"))

combined_plot <- ggarrange(A, B, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")
print(combined_plot)

ggsave("results/Figure_examples.png", device = "png", width = 10, height = 5)
