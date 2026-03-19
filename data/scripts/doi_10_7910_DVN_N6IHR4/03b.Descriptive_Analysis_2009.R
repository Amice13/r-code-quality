# load packages and data --------------------------------------------------
library(haven)
library(tidyverse)
library(dplyr)
library(psych)
library(stargazer)
library(purrr)
library(ggplot2)
library(reshape2)

root <- "~PISA/"
path <- paste0(root,"prepared_data/")
output <- paste0(root,"Output")
setwd(path)


# Load Country Dataframe for 2009 -----------------------------------------

countries <- c("DEU","GBR", "CAN", "USA", "SGP", "JPN", "KOR")
## make exception handler

for(country in countries){
  #country <- "DEU"
  print(paste("currently at:",country))
  

  # Load Country Dataframe per year-----------------------------------------
  path_year <- paste0(path,"2009","/")
  setwd(path_year)
  df <- read.csv(paste0(country,"with_indices_2009.csv")) 
  df <- df %>% select(-starts_with("X"))
  
  ##rename variables
  df <- df %>% rename(isei = bfmj)
  
  ##generate mean isei-values
  df <- df %>%
    group_by(schoolid) %>%
    mutate(mean_isei = mean(isei, na.rm=T)) %>%
    ungroup()
  
  ## Select variables 
  df <- df %>%
    select(starts_with("PV") |
             contains("ESCS") | 
             contains("ISEI") |
             mig_2nd , mean_mig , langn,
             disclima , mean_disclima,
             autonomy , leadership , accountability)
  

    
  des_stats <- describe(df)
  des_stats <- round(des_stats,3)
  
  print(des_stats)
  
  setwd(output)
  write.csv(des_stats, paste0(country,"_2009_descriptives.csv"))
  
  ##generate heatmap
  cor_df <- melt(cor(df, use="pairwise.complete.obs"))
  
  setwd(output)
  h <- ggplot(data=cor_df, aes(x = Var1, y = Var2, fill= value)) +
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, 
                                     size = 8, hjust = 1))+
    coord_fixed()
  
  ggsave(filename=paste0(country,"_2009_heatmap.png"),
         plot = h, dpi = 300, width=12, height =12, units="cm")
  }
  


