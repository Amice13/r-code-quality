#### purpose: reproducing figure c3 #### 

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

#### motivation plot --- immigration enforcement #### 

# dhs yearbook of immigration statistics 2018, table 39. 

df_dhsyb = data.frame(
  
  year = c(2018, 2017, 2016, 2015, 2014, 2013, 2012, 2011, 2010, 2009, 2008, 2007, 2006, 
           2005, 2004, 2003, 2002, 2001, 2000, 1999, 1998, 1997, 1996, 1995, 1994,
           1993, 1992, 1991, 1990, 1989, 1988, 1987, 1986, 1985, 1984, 1983, 1982,
           1981, 1980, 1979, 1978, 1977, 1976, 1975, 1974, 1973, 1972, 1971, 1970,
           1969, 1968, 1967, 1966, 1965, 1964, 1963, 1962, 1961, 1960, 1959, 1958,
           1957, 1956, 1955, 1954, 1953, 1952, 1951, 1950, 1949, 1948, 1947, 1946,
           1945, 1944, 1943, 1942, 1941, 1940, 1939, 1938, 1937, 1936, 1935, 1934,
           1933, 1932, 1931, 1930, 1929, 1928, 1927, 1926, 1925, 1924, 1923, 1922,
           1921, 1920, 1919, 1918, 1917, 1916, 1915, 1914, 1913, 1912, 1911, 1910,
           1909, 1908, 1907, 1906, 1905, 1904, 1903, 1902, 1901, 1900, 1899, 1898,
           1897, 1896, 1895, 1894, 1893, 1892),
  
  removals = c(337287, 288093, 332227, 325668, 405239, 432281, 415636, 
               390442, 382461, 379739, 359795, 319382, 280974, 246431, 
               240665, 211098, 165168, 189026, 188467, 183114, 174813, 
               114432, 69680, 50924, 45674, 42542,43671,33189,30039,
               34427,25829,24336,24592,23105,18696,19211, 15216,17379,
               18013,26825,29277,31263,38471,24432,19413,17346,16883,
               18294,17469,11030,9590,	9728,	9680,	10572,9167,	7763,	8025,	8181,
               7240,	8468,	7875,	5989,	9006,	17695,30264,23482,23125,17328,10199,
               23874,25276,23434,17317,13611,8821,	5702,	5542,	7336,	12254,14700,
               17341,16905,16195,13877,14263,25392,26490,27886,24864,31035,30464,
               31417,31454,34885,36693,24280,18076,18296,14557,11694,8866,	17881,
               21648,26675,37651,23399,18513,25137,26965,12535,12971,14059,13108,
               12724,8773,	9316,	5439,	3879,	4602,	4052,	3229,	1880,	3037,	2596,
               1806,	1630,	2801)
  
)

df_dhsyb = df_dhsyb %>% 
  mutate(post_iirira = ifelse(year >= 1997, 1, 0),
         removals = removals / 1000)


plot_enforce1 = df_dhsyb %>% 
  filter(year > 1980) %>% 
  ggplot() + 
  geom_point(aes(x = year, y = removals),
             size = .5) + 
  geom_line(aes(x = year, y = removals),
            size = .3) + 
  labs(x = 'Year', y = "Removals (Thousands)", title = "Removals (1980-2018)") + 
  geom_vline(xintercept = 1997, linetype = 2, size = .3) +
  geom_vline(xintercept = 2008, linetype = 2, size = .3) +
  annotate("text", x = 2001.5, y = 325,
           label = "IIRIRA\n(1997)",
           family = "serif", size = 2.75) + 
  annotate("text", x = 2013, y = 250,
           label = "Secure\nCommunities\n(2008)",
           family = "serif", size = 2.25) + 
  annotate("text", x = 2014, y = 100, 
           label = paste0("Post-IIRIRA\nMean:\n ",
                          round(mean(df_dhsyb$removals[df_dhsyb$post_iirira == 1]), 0),
                          'k\n~1400%\nIncrease'),
           family = "serif", size = 2.5) + 
  annotate("text", x = 1988, y = 250, 
           label = paste0("Pre-IIRIRA Mean:\n ",
                          round(mean(df_dhsyb$removals[df_dhsyb$post_iirira == 0]), 0), "k"),
           family = "serif", size = 2.75) + 
  ggthemes::theme_tufte(base_size = 9) 

ggsave(plot = plot_enforce1, filename = "enforce.png", width = 4, height = 3)
