
# Survey figures
rm(list = ls(all = TRUE))
library(scales)
library(maptools)
install.packages("GADMTools")
library(GADMTools)
library(dplyr)
library(classInt)
library(sp)
library(raster)
library(plyr)
library(ggplot2)
install.packages("ggpubr")
library(ggpubr)
library(ggmap)
install.packages("rgdal")
library(rgdal) 
library(broom)
install.packages("rvest")
library(rvest)
library(base)
install.packages("rmapshaper")
library(rmapshaper)
library(rgeos)
library(RColorBrewer)

#pal.6.cat <- brewer.pal(6, "RdYlBu")
#pal.6.cat[1] <- "gray60" 
pal.3.cat <- c("#cccccc", "#D73027", "#4575B4")
pal.4.cat <- c("#cccccc", "#D73027", "#FC8D59", "#FEE090", "#91BFDB", "#4575B4")
pal.5.cat <- c("#D73027", "#FC8D59", "#FEE090", "#91BFDB", "#4575B4")
pal.6.cat <- c("#cccccc", "#D73027", "#FC8D59", "#FEE090", "#91BFDB", "#4575B4")


opinion <- apsr.data %>% 
  subset(country != "") %>% 
  subset(self_sufficient != "") 

opinion.1 <- opinion %>% 
  group_by(country, contact_pers)  %>% 
  summarise(`Troops` = modal(troops_1), `Government` = modal(american_gov), `People` = modal(american_people)) %>% 
  melt(., id.vars = c("country","contact_pers"), measure.vars = c("Troops","Government","People")) %>% 
  mutate(contact_pers = factor(contact_pers, levels = c("Don't know/Decline to answer", "No", "Yes")),
         value = factor(value,levels = c("Don't know/Decline to answer", "Very unfavorable", "Somewhat unfavorable", "Neutral", "Somewhat favorable", "Very favorable")),
         Category = "Contact") %>%
  plyr::rename(c("contact_pers" = "response"))
  
opinion.2 <- opinion %>% 
  group_by(country, contact_nonpers)  %>% 
  summarise(`Troops` = modal(troops_1), `Government` = modal(american_gov), `People` = modal(american_people)) %>% 
  melt(., id.vars = c("country","contact_nonpers"), measure.vars = c("Troops","Government","People")) %>% 
  mutate(contact_nonpers = factor(contact_nonpers, levels = c("Don't know/Decline to answer", "No", "Yes")),
         value = factor(value,levels = c("Don't know/Decline to answer", "Very unfavorable", "Somewhat unfavorable", "Neutral", "Somewhat favorable", "Very favorable")),
         Category = "Network Contact") %>%
  plyr::rename(c("contact_nonpers" = "response"))

opinion.3 <- opinion %>% 
  group_by(country, benefit_pers)  %>% 
  summarise(`Troops` = modal(troops_1), `Government` = modal(american_gov), `People` = modal(american_people)) %>% 
    melt(., id.vars = c("country","benefit_pers"), measure.vars = c("Troops","Government","People")) %>% 
    mutate(benefit_pers = factor(benefit_pers, levels = c("Don't know/Decline to answer", "No", "Yes")),
           value = factor(value,levels = c("Don't know/Decline to answer", "Very unfavorable", "Somewhat unfavorable", "Neutral", "Somewhat favorable", "Very favorable")),
           Category = "Benefit") %>% 
  plyr::rename(c("benefit_pers" = "response"))

opinion.4 <- opinion %>% 
  group_by(country, benefit_nonpers)  %>% 
  summarise(`Troops` = modal(troops_1), `Government` = modal(american_gov), `People` = modal(american_people)) %>% 
  melt(., id.vars = c("country","benefit_nonpers"), measure.vars = c("Troops","Government","People")) %>% 
  mutate(benefit_nonpers = factor(benefit_nonpers, levels = c("Don't know/Decline to answer", "No", "Yes")),
         value = factor(value,levels = c("Don't know/Decline to answer", "Very unfavorable", "Somewhat unfavorable", "Neutral", "Somewhat favorable", "Very favorable")),
         Category = "Network Benefit") %>% 
  plyr::rename(c("benefit_nonpers" = "response"))
  
opinion.comb <- rbind(opinion.1, opinion.2, opinion.3, opinion.4)  %>% 
  mutate(`Category` = factor(`Category`, levels = c("Contact","Network Contact","Benefit","Network Benefit"))) %>% 
  subset(response != "Don't know/Decline to answer")

ggplot(data = opinion.comb, aes(x = response, y = country, fill = value)) +
  geom_tile(color = "black", size = 0.1) +
  facet_grid(variable ~ `Category` ) +
  scale_fill_manual(values = pal.6.cat, drop = FALSE) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 8),
        strip.text.y = element_text(size = 8)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  labs(fill = "Response",
       x = "Contact/Benefit?",
       y = "",
       title = "")

ggsave(here("Figures", "apsr-figure-contact-combined.pdf"), height = 6, width = 8, units = "in")



# Independent variable breakdown by country
contact.country <- apsr.data %>% 
  filter(country != "") %>% 
  dplyr::select(country, contact_pers, contact_nonpers, benefit_pers, benefit_nonpers) %>% 
  pivot_longer(cols = c(2:5)) %>% 
  filter(!is.na(value)) %>% 
  mutate(value = factor(value, levels = c("Don't know/Decline to answer", "No",  "Yes")),
         name2 = factor(name, levels = c("contact_pers", "contact_nonpers", "benefit_pers", "benefit_nonpers"), 
                        labels = c("Personal Contact", "Network Contact", "Personal Benefit", "Network Benefit")))


ggplot(contact.country, aes(x = country, y = ((..count..)/sum(..count..)), group = value, fill = value)) +
  geom_bar(stat = "count", position = "fill", color = "black", size = 0.1) +
  facet_wrap(. ~ name2, nrow = 1) +
  coord_flip() +
  theme_bw() +
  scale_fill_manual(values = pal.3.cat, drop = FALSE) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Percent",
       y = "",
       fill = "Response",
       title = "Country-Level Distribution of Primary Independent Variable Responses")

ggsave(here("Figures", "apsr-figure-descriptive-iv-distribution.pdf"), width = 11, height = 5, units = "in")


