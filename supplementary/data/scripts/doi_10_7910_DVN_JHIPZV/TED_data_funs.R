match_old_nuts <- function(nuts3, nuts3_2006, nuts3_2010){
  nuts3_2006 <- na.omit(nuts3_2006)
  nuts3_2010 <- na.omit(nuts3_2010)
  res <- nuts3 %>% 
    #adding code from 2010
    left_join(nuts3_2006,  by = c('buyer_nuts'='Code 2006')) %>% 
    mutate( `Code 2010` = `Code 2010` %>% str_replace('(part)','') %>% str_replace(' \\(.*\\)','') %>% str_trim()) %>% 
    #adding code from 2013
    left_join(nuts3_2010,  by = c('buyer_nuts'='Code 2010')) %>% 
    mutate( `Code 2013` = `Code 2013` %>% str_replace('(part)','') %>% str_replace(' \\(.*\\)','') %>% str_trim()) %>% 
    rename(Code_2010 = `Code 2010`) %>% 
    #adding code from 2013 to joined code from 2010
    left_join(nuts3_2010,  by = c('Code_2010' = 'Code 2010')) %>%
    mutate( `Code 2013.x` = `Code 2013.x` %>% str_replace('(part)','') %>% str_replace(' \\(.*\\)','') %>% str_trim()) %>%
    # matching population by 2013 code
    # select(c('buyer_nuts', 'Code 2013.y', 'pop_nuts')) %>% distinct() %>% 
    left_join(pop, by = c('Code 2013.y'='GEO')) %>%
    mutate(pop_nuts = ifelse(is.na(pop_nuts) & !is.na(Value), Value, pop_nuts)) %>%
    select(-Value) %>% 
    mutate(pop_nuts = ifelse(pop_nuts == ':', NA, pop_nuts)) %>% 
    # #matching population by 2010 code
    left_join(pop, by = c('Code_2010'='GEO')) %>%
    mutate(pop_nuts = ifelse(is.na(pop_nuts) & !is.na(Value), Value, pop_nuts)) %>%
    select(-Value) %>%
    # #matching population by 2013 code
    left_join(pop, by = c('Code 2013.x'='GEO')) %>%
    mutate(pop_nuts = ifelse(is.na(pop_nuts) & !is.na(Value), Value, pop_nuts)) %>%
    select(-Value)
  return(res)
}

match_old_nuts_area <- function(nuts3, nuts3_2006, nuts3_2010){
  nuts3_2006 <- na.omit(nuts3_2006)
  nuts3_2010 <- na.omit(nuts3_2010)
  res <- nuts3 %>% 
    #adding code from 2010
    left_join(nuts3_2006,  by = c('buyer_nuts'='Code 2006')) %>% 
    mutate( `Code 2010` = `Code 2010` %>% str_replace('(part)','') %>% str_replace(' \\(.*\\)','') %>% str_trim()) %>% 
    #adding code from 2013
    left_join(nuts3_2010,  by = c('buyer_nuts'='Code 2010')) %>% 
    mutate( `Code 2013` = `Code 2013` %>% str_replace('(part)','') %>% str_replace(' \\(.*\\)','') %>% str_trim()) %>% 
    rename(Code_2010 = `Code 2010`) %>% 
    #adding code from 2013 to joined code from 2010
    left_join(nuts3_2010,  by = c('Code_2010' = 'Code 2010')) %>%
    mutate( `Code 2013.x` = `Code 2013.x` %>% str_replace('(part)','') %>% str_replace(' \\(.*\\)','') %>% str_trim()) %>%
    # matching area by 2013 code
    # select(c('buyer_nuts', 'Code 2013.y', 'kmq')) %>% distinct() %>% 
    left_join(area, by = c('Code 2013.y'='GEO')) %>%
    mutate(kmq = ifelse(is.na(kmq) & !is.na(Value), Value, kmq)) %>%
    select(-Value) %>% 
    mutate(kmq = ifelse(kmq == ':', NA, kmq)) %>% 
    # #matching areaulation by 2010 code
    left_join(area, by = c('Code_2010'='GEO')) %>%
    mutate(kmq = ifelse(is.na(kmq) & !is.na(Value), Value, kmq)) %>%
    select(-Value) %>%
    # #matching areaulation by 2013 code
    left_join(area, by = c('Code 2013.x'='GEO')) %>%
    mutate(kmq = ifelse(is.na(kmq) & !is.na(Value), Value, kmq)) %>%
    select(-Value)
  return(res)
}

match_old_mountains <- function(nuts3, nuts3_2006, nuts3_2010){
  nuts3_2006 <- na.omit(nuts3_2006)
  nuts3_2010 <- na.omit(nuts3_2010)
  res <- nuts3 %>% 
    #adding code from 2010
    left_join(nuts3_2006,  by = c('buyer_nuts'='Code 2006')) %>% 
    mutate( `Code 2010` = `Code 2010` %>% str_replace('(part)','') %>% str_replace(' \\(.*\\)','') %>% str_trim()) %>% 
    #adding code from 2013
    left_join(nuts3_2010,  by = c('buyer_nuts'='Code 2010')) %>% 
    mutate( `Code 2013` = `Code 2013` %>% str_replace('(part)','') %>% str_replace(' \\(.*\\)','') %>% str_trim()) %>% 
    rename(Code_2010 = `Code 2010`) %>% 
    #adding code from 2013 to joined code from 2010
    left_join(nuts3_2010,  by = c('Code_2010' = 'Code 2010')) %>%
    mutate( `Code 2013.x` = `Code 2013.x` %>% str_replace('(part)','') %>% str_replace(' \\(.*\\)','') %>% str_trim()) %>%
    # matching regions_m by 2013 code
    # select(c('buyer_nuts', 'Code 2013.y', 'kmq')) %>% distinct() %>% 
    left_join(regions_m, by = c('Code 2013.y'='NUTS3_ID_2010')) %>%
    mutate(mountain.regions.x = ifelse(is.na(mountain.regions.x) & !is.na(mountain.regions.y), mountain.regions.y, mountain.regions.x)) %>%
    select(-mountain.regions.y) %>% 
    rename(mountain_region = mountain.regions.x) %>% 
    left_join(regions_m, by = c('Code_2010'='NUTS3_ID_2010')) %>%
    mutate(mountain_region = ifelse(is.na(mountain_region) & !is.na(mountain.regions), mountain.regions, mountain_region)) %>%
    select(-mountain.regions) %>%
    # #matching regions_mulation by 2013 code
    left_join(regions_m, by = c('Code 2013.x'='NUTS3_ID_2010')) %>%
    mutate(mountain_region = ifelse(is.na(mountain_region) & !is.na(mountain.regions), mountain.regions, mountain_region)) %>%
    select(-mountain.regions)
  return(res)
}

match_old_coast <- function(nuts3, nuts3_2006, nuts3_2010){
  nuts3_2006 <- na.omit(nuts3_2006)
  nuts3_2010 <- na.omit(nuts3_2010)
  res <- nuts3 %>% 
    #adding code from 2010
    left_join(nuts3_2006,  by = c('buyer_nuts'='Code 2006')) %>% 
    mutate( `Code 2010` = `Code 2010` %>% str_replace('(part)','') %>% str_replace(' \\(.*\\)','') %>% str_trim()) %>% 
    #adding code from 2013
    left_join(nuts3_2010,  by = c('buyer_nuts'='Code 2010')) %>% 
    mutate( `Code 2013` = `Code 2013` %>% str_replace('(part)','') %>% str_replace(' \\(.*\\)','') %>% str_trim()) %>% 
    rename(Code_2010 = `Code 2010`) %>% 
    #adding code from 2013 to joined code from 2010
    left_join(nuts3_2010,  by = c('Code_2010' = 'Code 2010')) %>%
    mutate( `Code 2013.x` = `Code 2013.x` %>% str_replace('(part)','') %>% str_replace(' \\(.*\\)','') %>% str_trim()) %>%
    # matching coast_type by 2013 code
    left_join(coast_type, by = c('Code 2013.y'='NUTS')) %>%
    mutate(coast_region = ifelse(is.na(coast_region.x) & !is.na(coast_region.y), coast_region.y, coast_region.x)) %>%
    select(-c(coast_region.x, coast_region.y)) %>% 
    left_join(coast_type, by = c('Code_2010'='NUTS')) %>%
    mutate(coast_region = ifelse(is.na(coast_region.x) & !is.na(coast_region.y), coast_region.y, coast_region.x)) %>%
    select(-c(coast_region.x, coast_region.y)) %>%
    # #matching coast_typeulation by 2013 code
    left_join(coast_type, by = c('Code 2013.x'='NUTS')) %>%
    mutate(coast_region = ifelse(is.na(coast_region.x) & !is.na(coast_region.y), coast_region.y, coast_region.x)) %>%
    select(-c(coast_region.x, coast_region.y))
  return(res)
}
