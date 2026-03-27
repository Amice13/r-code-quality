#################################################
# Project:    Emergency Debate contribution
# Task:       Plot emergency emphasis in speeches
# Author:     Christian Rauh (16.09.2020)
#################################################

# R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"

# Packages ####
library(tidyverse) # 1.3.0
library(ggridges) # 0.5.2
library(Hmisc) # 4.4-1
library(countrycode) # 1.2.0


# Data ####
speech <- read_rds("./Corpora/EmergencyCorpData.rds")

speech$year <- as.numeric(str_extract(speech$date, "[0-9]{4}"))
speech$month <- str_extract(speech$date, "[0-9]{4}-[0-9]{1,2}")

speech$country2 <- countrycode(speech$country, origin = "iso2c", destination = "country.name")
table(speech$country)
table(speech$country2)
speech$country2[speech$country == "UK"] <- "United Kingdom"

table(speech$institution[speech$year>=1989])


# The Draghi speech ###
# Whatever it takes ...
draghi <- speech %>% filter(str_detect(speaker, "Draghi")) %>% 
  filter(str_detect(text, "whatever it takes"))

ggplot(speech[str_detect(speech$institution, "Commission|Central"), ], aes(x=ne.scale, group = institution))+
  geom_density(aes(colour = institution))+
  geom_vline(xintercept = draghi$ne.scale[1])


# Comm / ECB emergency emphasis ####

dat <- speech %>% 
  filter(str_detect(institution, "European Central Bank|EU Commission")) %>% 
  filter(year >= 1989)

ggplot(data = dat, aes(x=year, y=ne.scale))+
  geom_hline(yintercept = 0.1521369, size = .5, colour = "grey60") + # Hoc reference value, see EmergencyBaseline_HOC.R on Theia
  annotate("text", x = 1991.1, y = 0.145, label = "Reference value from 100.000\nHoC speeches (1985-2019)", color = "grey60", size = 2.1, hjust = 0, vjust = 1) +
  stat_summary(geom = "line", fun.data = "mean_cl_boot", aes(linetype = institution), position = position_dodge(width = 0.5), size = .4)+
  stat_summary(geom = "pointrange", fun.data = "mean_cl_boot", aes(shape = institution), position = position_dodge(width = 0.5), size = .4)+
  scale_x_continuous(breaks = seq(1989, 2020, 1), expand = c(0,1))+
  labs(y = "Normality-Emergency\nemphasis scale\n",
       x = "",
       shape = "Speaker:",
       linetype = "Speaker:")+
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(family = "serif"),
        axis.text.x = element_text(angle = 90, vjust = .5, colour = "black"), 
        panel.grid.minor.x = element_blank())

ggsave("./Plots/EmergencyScale_CommECB_OverTime.png", width = 16, height = 12, unit = "cm")




# Emergency emphasis during Eurocrisis ####

dat <- speech %>% 
  filter(year >= 2007 & year <= 2015) #EUspeech coverage
dat$inst2 <- dat$institution
dat$inst2[dat$inst2 == "Nat. leader"] <- dat$country2[dat$inst2 == "Nat. leader"]
# test <- dat[dat$institution == "Nat. leader",]
table(dat$inst2)
dat <- dat %>% filter(inst2 != "Poland") # Too few speeches
dat$type <- ifelse(dat$institution == "Nat. leader", "National\nhead of state or government", "Supranat.\nexecutive")

# Order
cmeans <- dat %>% 
  group_by(inst2) %>% 
  summarise(mne = mean(ne.scale, na.rm = T)) %>% 
  arrange(mne)
dat$inst2 <- factor(dat$inst2, levels = cmeans$inst2)
dat$type <- factor(dat$type, levels = c("Supranat.\nexecutive", "National\nhead of state or government"))


ggplot(dat, aes(x=inst2, y = ne.scale))+
  stat_summary(geom = "pointrange", fun.data = "mean_cl_boot", size = .4)+
  facet_grid(rows=vars(type), scales = "free_y", space = "free")+
  labs(x = "Speaker",
       y = "Normality-Emergency\nemphasis scale")+
  coord_flip()+
  theme_bw()+
  theme(text = element_text(family = "serif"),
        axis.text = element_text(color = "black"),
        axis.text.y = element_text(face = "bold"))
  
ggsave("./Plots/EmergencyEmphasisEurocrisis.png", width = 16, height = 10, units = "cm")

