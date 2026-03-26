
# clean
rm(list = ls())  

library(tidyverse)
library(lubridate)
library(ggplot2)
library(gridExtra)

# call data wp3_long
wp3_long <- read.csv("Data/wp3_long.csv")
wp3_long$date <- as.Date(wp3_long$start_time)

# arrange by dates
timeline <- wp3_long %>%
  filter (date < "2022-05-23") %>% 
  select (person_id, date, political_url, political_url_russia_ukraine, iso2) %>%
  mutate (political_url_counts = ifelse(political_url == TRUE, 1,0),
          political_url_ukraine_counts = ifelse ( political_url_russia_ukraine == TRUE, 1,0),
          country = iso2) %>%
  group_by (person_id, date, country) %>%
  summarize (political_visits = sum (political_url_counts, na.rm = T),
             ukraine_visits = sum (political_url_ukraine_counts, na.rm = T),
             news_visits = n())


# Plot this 
plot <- timeline %>%
  filter (news_visits < 1000) %>%
  ggplot(aes(x = date, y = news_visits, group = person_id)) + geom_line () + 
  stat_summary( # add average line
    aes(group = 1),
    fun = mean,
    geom = "line",
    size = 1.5,
    color = "red"
  ) + 
  xlab("") + ylab("Number of visits") + ggtitle("Daily news visits") + theme_classic() +
  facet_grid(~ country)


plot2 <- timeline %>%
  #filter (n.political_url < 20) %>%
  ggplot(aes(x = date, y = political_visits, group = person_id)) + geom_line () + 
  stat_summary( # add average line
    aes(group = 1),
    fun = mean,
    geom = "line",
    size = 1.5,
    color = "red"
  ) + 
  xlab("") + ylab("Number of political visits") + ggtitle("Daily political visits") + theme_classic() +
  facet_grid(~ country)



grid.arrange(plot, plot2)


# ================================ PIVOT LONGER THE TRAJECTORIES ======================================

timeline_long <- timeline %>%
  filter (news_visits < 1000) %>%
  pivot_longer(!c(person_id, date,country), names_to = "visit_type", values_to = "counts")

all_plot <- timeline_long %>%
  ggplot (aes (x = date, y = counts, colour = visit_type)) + geom_line () + 
  xlab("") + ylab("") + theme_classic () + 
  facet_grid(~ country)

p <- all_plot +  theme (legend.position = "bottom", axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15),
                        legend.text=element_text(size=15), legend.title = element_blank())  




# =================================== PLOT VISITS AGGREGATED BY DAY ====================================
  
# Plot but now only counts by days not individual trajectories 
timeline2 <- timeline %>%
  filter (news_visits < 1000) %>%
  group_by (date, country) %>%
  summarize (political_visits = sum(political_visits, na.rm = T),
             news_visits = sum(news_visits, na.rm = T),
             ukraine_visits = sum(ukraine_visits, na.rm =T))

# Plot this 
p <- timeline2 %>%
  ggplot(aes(x = date, y = news_visits)) + geom_line () + 
  #stat_summary( # add average line
  #  aes(group = 1),
  #  fun = mean,
  #  geom = "line",
  #  size = 1.5,
  #  color = "black"
  #) + 
  xlab("") + ylab("Number of visits") + ggtitle("Total news visits by day") + theme_classic() +
  facet_grid(~ country)


p2 <- timeline2 %>%
  ggplot(aes(x = date, y = political_visits)) + geom_line () + 
  #stat_summary( # add average line
  #  aes(group = 1),
  #  fun = mean,
  #  geom = "line",
  #  size = 1.5,
  #  color = "black"
  #) + 
  xlab("") + ylab("Number of political visits") + ggtitle("Total political visits by day") + theme_classic() +
  facet_grid(~ country)

grid.arrange(p, p2)



# --------------------
DE <- timeline2 %>%
  filter (country == "DE") %>%
  group_by (date) %>%
  summarise(news_visits = sum(news_visits, na.rm =T),
            political_visits = sum (political_visits, na.rm = T),
            ukraine_visits = sum (ukraine_visits, na.rm = T))
  

# ======================= FINAL PLOT: PIVOT LONGER TO SEE ALL LINES TOGETHER =====================================

timeline2_long <- timeline2 %>%
  pivot_longer(!c(date,country), names_to = "visit_type", values_to = "counts")

all_plot2 <- timeline2_long %>%
  ggplot (aes (x = date, y = counts, colour = visit_type)) + geom_line () + 
  xlab("") + ylab("") + theme_classic () + 
  facet_grid(~ country)

p2 <- all_plot2 +  theme (legend.position = "bottom", axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15),
                        legend.text=element_text(size=15), legend.title = element_blank())  

# High resolution
png("timeline.png", width = 3000, height = 2000, res = 300)  # High resolution
timeline2_long %>%
  ggplot (aes (x = date, y = counts, colour = visit_type)) + geom_line () + 
  xlab("") + ylab("") + theme_classic () + 
  facet_grid(~ country) +  
  theme (legend.position = "bottom", axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15),
                    legend.text=element_text(size=15), legend.title = element_blank())  
dev.off ()


#=================================== QUICK VIEW OF REL BETWEEN VISITS AND POLKNOW FOR  ==============================================

# Number of news visits and panelists by country

news_visits <-  wp3_long %>%
  group_by (iso2) %>%
  summarize (N = n(), panelists = length(unique(person_id)))



