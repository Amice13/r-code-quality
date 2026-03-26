## Data and Materials Sharing
#==========================================================================
# 04-figure1.R
# Purpose: replicate figure 1
# Author: Blind for review
#==========================================================================
# install.packages("ggplot2")
# install.packages("scales")
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(scales)
options(scipen = 999)

# load all urls by topics
all_topics <- readRDS('03_all_topics_URL_n10.rds')

# create a unique list of urls by selecting necessary variables
all_temp_uniqueURL <- all_topics %>% select(urls_rep, SIMPSON2, starts_with('url_fixed'), share_n, topic) %>% distinct()

## Figure 1A

(temp_biden_putin_url_sep <- ggplot(all_temp_uniqueURL %>% filter(topic=="Biden-Putin"), aes(x=SIMPSON2)) +
    geom_histogram( aes(y=(after_stat(count))/sum(after_stat(count))*100), bins = 50 ) +
    labs(
      title = "Biden-Putin Summit",
      x = "Simpson's Index",
      y = "Percent") +
    theme_bw() +
    scale_fill_manual(values=c( "grey")) +
    
    theme(legend.position = 'none') +
    labs(fill='URL Leanings') 
)

(temp_covid19_url_sep <- ggplot(all_temp_uniqueURL %>% filter(topic=="COVID-19"), aes(x=SIMPSON2)) +
    geom_histogram( aes(y=(after_stat(count))/sum(after_stat(count))*100), bins = 50 ) +
    labs(
      title = "COVID-19",
      x = "Simpson's Index",
      y = "Percent") +
    theme_bw() +
    theme(legend.position = 'none') +
    scale_fill_manual(values=c( "grey")) +
    labs(fill='URL Leanings') 
)

(temp_britney_url_sep <- ggplot(all_temp_uniqueURL %>% filter(topic=="FreeBritney"), aes(x=SIMPSON2)) +
    geom_histogram( aes(y=(after_stat(count))/sum(after_stat(count))*100), bins = 50 ) +
    labs(
      title = "FreeBritney",
      x = "Simpson's Index",
      y = "Percent") +
    theme_bw() +
    theme(legend.position = 'none') +
    scale_fill_manual(values=c(  "grey")) +
    labs(fill='URL Leanings') 
)

(temp_allstargame_url_sep <- ggplot(all_temp_uniqueURL %>% filter(topic=="MLB"), aes(x=SIMPSON2)) +
    geom_histogram( aes(y=(after_stat(count))/sum(after_stat(count))*100), bins = 50 ) +
    labs(
      title = "MLB All-Star Game",
      x = "Simpson's Index",
      y = "Percent") +
    theme_bw() +
    theme(legend.position = 'none') +
    scale_fill_manual(values=c( "grey")) +
    labs(fill='URL Leanings') 
)

## Figure 1B

# Biden-Putin
(temp_biden_putin_merge <- all_topics %>% filter(topic == "Biden-Putin") %>% count(theta_class, url_fixed) %>%
    mutate(pct = prop.table(n)*100) %>%
    ggplot() + aes(theta_class, pct, fill=url_fixed) +
    geom_bar(position="dodge", stat="identity") +
    labs(
      title = "Biden-Putin Summit",
      x = "Users",
      y = "Percent") + 
    theme_bw() +
    scale_fill_manual(values=c( "red", "blue", "grey")) +
    labs(fill='URL Leanings') +
    theme(legend.position = 'bottom', text=element_text(size=10))
  
)

# COVID-19
(temp_covid19_merge <- all_topics %>% filter(topic == "COVID-19")  %>% count(theta_class, url_fixed) %>%
    mutate(pct = prop.table(n)*100) %>%
    ggplot() + aes(theta_class, pct, fill=url_fixed) +
    geom_bar(position="dodge", stat="identity") +
    labs(
      title = "COVID-19",
      x = "Users",
      y = "Percent") + 
    theme_bw() +
    scale_fill_manual(values=c( "red", "blue", "grey")) +
    labs(fill='URL Leanings') +
    theme(legend.position = 'bottom', text=element_text(size=10))
)

# FreeBritney
temp_merge <- all_topics %>% filter(topic == "FreeBritney") %>% count(theta_class, url_fixed) %>%
  mutate(pct = prop.table(n)*100) %>%
  bind_rows(data.frame(theta_class=c('conservative'),
                       url_fixed=as.character(c('liberal')),
                       n=0,
                       pct=0,stringsAsFactors = F)) 

(temp_britney_merge <- temp_merge %>%
    ggplot() + aes(theta_class, pct, fill=url_fixed) +
    geom_bar(position="dodge", stat="identity") +
    labs(
      title = "FreeBritney",
      x = "Users",
      y = "Percent") + 
    theme_bw() +
    scale_fill_manual(values=c( "red", "blue", "grey")) +
    labs(fill='URL Leanings') +
    theme(legend.position = 'bottom', text=element_text(size=10))
)

# MLB
temp_merge <-  all_topics %>% filter(topic == "MLB") %>% count(theta_class, url_fixed) %>%
  mutate(pct = prop.table(n)*100) %>%
  bind_rows(data.frame(theta_class=c('conservative'),
                       url_fixed=as.character(c('liberal')),
                       n=0,
                       pct=0,stringsAsFactors = F)) 

(temp_allstargame_merge <- temp_merge %>%
    ggplot() + aes(theta_class, pct, fill=url_fixed) +
    geom_bar(position="dodge", stat="identity") +
    labs(
      title = "MLB All-Star Game",
      x = "Users",
      y = "Percent") + 
    theme_bw() +
    scale_fill_manual(values=c( "red", "blue", "grey")) +
    labs(fill='URL Leanings') +
    theme(legend.position = 'bottom', text=element_text(size=10))
  
)


(figure1_url_sep_nolabel <- ggarrange(temp_biden_putin_url_sep, 
                                      temp_covid19_url_sep, 
                                      temp_britney_url_sep, 
                                      temp_allstargame_url_sep,
                                      ncol = 2, nrow = 2, common.legend =TRUE, legend="bottom",
                                      font.label = list(size = 10)
                                      ) +
    theme(plot.margin = margin(0,0,1,0, "cm"))) 

(figure3_merge_nolabel <- ggarrange(temp_biden_putin_merge, temp_covid19_merge, temp_britney_merge, temp_allstargame_merge,
                                    ncol = 2, nrow = 2, common.legend = TRUE, legend="bottom",
                                    font.label = list(size = 10)))


(figure1_url_shares_merge <- ggarrange(figure1_url_sep_nolabel,
                                       figure3_merge_nolabel,
                                       
                                       labels = c("A","B"),
                                       ncol = 2, nrow = 1, common.legend = FALSE, legend="bottom",
                                       font.label = list(size = 15)))

png("HSSC_figure1.png", units="in", width=12, height=5, res=300)
figure1_url_shares_merge
dev.off()

