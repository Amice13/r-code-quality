#### purpose: reproducing figure a1 #### 

#### installing key packages #### 

list.of.packages = 
  c('readstata13', 'haven', 'tidyverse', 'dplyr', 'estimatr', 'texreg',
  'gridExtra', 'ggthemes', 'wCorr', 'questionr', 'xtable', 'sf', 
  'TAM', 'purrr', 'kable', 'kableExtra', 'wCorr', 'psych',
  'psychTools')
new.packages =  list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#### libraries #### 

suppressPackageStartupMessages(
  
  {
    
    library(readstata13)    
    library(haven)
    library(tidyverse)
    library(dplyr)
    library(estimatr)
    library(texreg)
    library(gridExtra)
    library(ggthemes)
    library(wCorr)
    library(questionr)
    library(xtable)
    library(sf)
    library(TAM)
    library(purrr)
    library(kable)
    library(kableExtra)
    library(wCorr)
    library(psych)
    library(psychTools)
    
  }
  
)

#### mediacloud #### 

mcloud = read_csv("mediacloud_data/antiblack-and-latino-or-stories-over-time-20220223030828.csv")

salp1 = mcloud %>%
  ggplot() + 
  geom_point(aes(x = date, y = count),
             alpha = .1,
             size = .07) + 
  geom_smooth(aes(x = date, y = count),
              col = "black",
              size = .4) + 
  labs(x = "Date", y = "Article Count",
       title = "A. Mediacloud") + 
  theme_tufte()


#### google scholar #### 

# Search term: 
# ("anti-blackness"  AND "latinos") OR ("anti-black"  AND "latinos") 
# data collected february 22, 2022

gscholar = data.frame(
  year = c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012,
           2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021),
  hits = c(1, 3, 4, 3, 4, 5, 4, 5, 5, 6, 11, 6, 11, 9, 23, 51, 59, 89, 127, 146, 225, 421)
)

salp2 = gscholar %>% 
  ggplot() + 
  geom_point(aes(x = year, y = hits)) + 
  geom_line(aes(x = year, y = hits)) + 
  labs(x = "Year", y = 'Google Scholar Hits\n',
       title = "B. Google Scholar") + 
  theme_tufte() 

#### what afrolatinos want you to know #### 

salp3 = data.frame(
  date = as.Date(c("2017-11-12",
                   "2018-05-04",
                   "2019-05-19",
                   "2020-11-02",
                   "2021-06-30",
                   "2022-04-23")),
  views = c(0, 
            429,
            601,
            722,
            798,
            863)
) %>% 
  ggplot() + 
  geom_point(aes(x = date, y = views)) + 
  geom_line(aes(x = date, y = views)) +
  labs(x = "Date", y = "Cumulative Views (in Thousands)", title = "C. YouTube (Pero Like)") + 
  theme_tufte()

#### the relationship between the black and latin x community, the grapevine #### 

salp4 = data.frame(
  date = as.Date(c("2019-02-21",
                   "2019-06-07",
                   "2020-07-15",
                   "2020-09-22",
                   "2020-11-03",
                   "2022-04-23")),
  views = c(0, 219, 336, 379, 383, 461)
) %>% 
  ggplot() + 
  geom_point(aes(x = date, y = views)) + 
  geom_line(aes(x = date, y = views)) +
  labs(x = "Date", y = "Cumulative Views (in Thousands)", title = "D. YouTube (The Grapevine)") + 
  theme_tufte()

salp_plot = arrangeGrob(salp1, salp2, salp3, salp4, ncol = 2)

ggsave(plot = salp_plot, filename = "absal.png", width = 8, height = 5)
