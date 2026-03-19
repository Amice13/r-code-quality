#### purpose: producing figure b2 #### 

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

#### cmps '16 --- loading datasets #### 

load(file = "cmps_lat.RData")
load(file = "cmps_wht.RData")
load(file = "cmps_blk.RData")

#### cmps '20 --- loading datasets #### 

load(file = "cmps20w_clean.RData")
load(file = "cmps20b_clean.RData")
load(file = "cmps20l_clean.RData")
# mean(cmps20w$blm_ft)
# mean(cmps20b$blm_ft)
# mean(cmps20l$blm_ft)

# quick fix for missingness 

cmps20l$cath = ifelse(is.na(cmps20l$cath), 0, cmps20l$cath)

# another quick fix on dtp

cmps20l$dtps2 = (cmps20l$dtp + cmps20l$dtp2 + cmps20l$dtp3) / 3
cmps20w$dtps2 = (cmps20w$dtp + cmps20w$dtp2 + cmps20w$dtp3) / 3
cmps20b$dtps2 = (cmps20b$dtp + cmps20b$dtp2 + cmps20b$dtp3) / 3


#### context plot --- undocumented social ties #### 

undoc_soc_tie = data.frame(
  
  est = c(weighted.mean(cmps16$know_undoc, cmps16$weight),
          weighted.mean(cmps16$know_undoc[cmps16$fobo == 1], cmps16$weight[cmps16$fobo == 1]),
          weighted.mean(cmps16$know_undoc[cmps16$second_gen == 1], cmps16$weight[cmps16$second_gen == 1]),
          weighted.mean(cmps16$know_undoc[cmps16$third_gen == 1], cmps16$weight[cmps16$third_gen == 1]),
          weighted.mean(cmps16$know_undoc[cmps16$cit == 1], cmps16$weight[cmps16$cit == 1]),
          weighted.mean(cmps16$know_undoc[cmps16$english == 0], cmps16$weight[cmps16$english == 0]),
          weighted.mean(cmps16$know_undoc[cmps16$english == 1], cmps16$weight[cmps16$english == 1])),
  
  cat = factor(c("Full", "1st Gen.", "2nd Gen.", "3rd Gen.+", "Citizen", "Spanish", "English"),
               levels = c("Full", "1st Gen.", "2nd Gen.", "3rd Gen.+", "Citizen", "Spanish", "English"))
  
) 

udp1 = undoc_soc_tie %>% 
  ggplot() + 
  geom_bar(aes(x = cat, y = est), stat = "identity") + 
  labs(x = "Latinx Sample",
       y = "Undocumented Social Ties",
       title = "A. Undocumented Social Ties") + 
  geom_text(label = round(undoc_soc_tie$est, 2),
            y = undoc_soc_tie$est + .03,
            x = seq(from = 1, to = 7),
            size = 2.25,
            family = "serif") + 
  ylim(0, .7) + 
  theme_tufte(base_size = 9)

#### context plot --- undocumented pop #### 

df_undocpop = data.frame(
  year = c(1990, 1995, 2000, 2004, 2005, 2007, 2010, 2016, 2017),
  pop = c(3.5, 5.7, 8.6, 10.7, 11.1, 12.2, 11.4, 10.7, 10.5)
)

df_undocpop2 = data.frame(
  year = c(1995, 2000, 2005, 2006, 2010, 2015, 2016, 2017),
  long = c(33, 35, 38, 41, 50, 64, 66, 66)
)

df_undocpop3 = data.frame(
  year = c(1995, 2000, 2005, 2006, 2010, 2015, 2016, 2017),
  medyearsres = c(7.1, 7.2, 8, 8.6, 10.6, 13.9, 14.8, 15.1)
)

udp2 = df_undocpop %>% 
  ggplot() + 
  geom_point(aes(x = year, y = pop),
             size = 2) + 
  geom_line(aes(x = year, y = pop),
            size = .4) + 
  geom_text(label = "3.5m\n(1990)",
            x = 1995, y = 4,
            size = 2.25,
            family = "serif") + 
  geom_text(label = "12.2m\n(2007)",
            x = 2009, y = 10,
            size = 2.25,
            family = "serif") + 
  geom_text(label = "10.5m\n(2017)",
            x = 2016, y = 8,
            size = 2.25,
            family = "serif") + 
  labs(x = "Year",
       y = "Undocumented Population\n(in millions)",
       title = "B. Undocumented Population") + 
  theme_tufte(base_size = 9)

udp3 = df_undocpop2 %>% 
  ggplot() + 
  geom_point(aes(x = year, y = long),
             size = 2) + 
  geom_line(aes(x = year, y = long),
            size = .4) + 
  geom_text(label = "33%\n(1995)",
            x = 1998, y = 45,
            size = 2.25,
            family = "serif") + 
  geom_text(label = "66%\n(2017)",
            x = 2011, y = 62,
            size = 2.25,
            family = "serif") + 
  labs(x = "Year",
       y = "% Undocumented in U.S. > 10 Years",
       title = "C. > 10 years") + 
  theme_tufte(base_size = 9)

udp4 = df_undocpop3 %>% 
  ggplot() + 
  geom_point(aes(x = year, y = medyearsres),
             size = 2) + 
  geom_line(aes(x = year, y = medyearsres),
            size = .4) + 
  geom_text(label = "7.1\n(1995)",
            x = 1998, y = 9,
            size = 2.25,
            family = "serif") + 
  geom_text(label = "15.1\n(2017)",
            x = 2011, y = 13,
            size = 2.25,
            family = "serif") + 
  labs(x = "Year",
       y = "Median Years in U.S.",
       title = "D. Median Years in U.S.") + 
  theme_tufte(base_size = 9)

udp_grob = arrangeGrob(udp1, udp2, udp3, udp4, ncol = 2)

ggsave(plot = udp_grob, filename = "udplot.png", width = 8, height = 5)


