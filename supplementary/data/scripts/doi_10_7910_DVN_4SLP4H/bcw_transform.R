rm(list=ls())
setwd('~/Dropbox/bcw_v1p1') #Set to folder with bcw_coder_level
#Packages
library(tidyverse)
library(countrycode)
library(imputeTS)

################################################################################
#Functions
################################################################################
na_interpolation_fornback <- function(x, option = "linear") { #Forward and backward
  total_not_missing <- sum(!is.na(x)) #Total non-missing values
  if(total_not_missing == 0){x} #If all NAs, then return all NAs again
  else if(total_not_missing==1){rep(max(x,na.rm=T),length(x))} #if only one observation, fill everything in with that value
  else {imputeTS::na_interpolation(x, option = option)} #Otherwise, the standard
}

na_interpolation_forward <- function(x, option = "linear") { #Only forward
  total_not_missing <- sum(!is.na(x)) #Total non-missing values
  if(total_not_missing == 0){x} #If all NAs, then return all NAs again 
  else {
    first <- min(which(!is.na(x)))
    if(total_not_missing==1){
      return(replace(x, (1:length(x)>first), max(x,na.rm=T)))}
      #if only one observation, fill everything in with that value after first obs
    else { #Otherwise, the standard, but NA prior to first obs
      return(replace(imputeTS::na_interpolation(x, option = "linear"), (1:length(x)<first), NA))} 
  }}

na_locf_fornback <- function(x){ 
  total_not_missing <- sum(!is.na(x))
  if(total_not_missing == 0){x}
  else{imputeTS::na_locf(x)}
}

na_locf_forward <- function(x){ 
  total_not_missing <- sum(!is.na(x))
  if(total_not_missing == 0){x}
  else{
    first <- min(which(!is.na(x)))
    return(replace(imputeTS::na_locf(x), (1:length(x)<first), NA))} 
}

################################################################################
#Step 1 - Load data and add descriptive variables
################################################################################

#Load Coder-level data
data <- read.csv('bcw_coder_level_v1p1.csv')

#Make number of coders var
num_coders <- data %>% 
  select(borderid,coder) %>%
  unique() %>%
  group_by(borderid) %>%
  summarise(num_coders=n()) %>% 
  ungroup()

#Make number of coders identifying imagery var
prop_imagery <- data %>%
  select(borderid,coder,year,imagery_earth_1,imagery_earth_2) %>%
  mutate(ie1 = as.numeric(imagery_earth_1>0), ie2= as.numeric(imagery_earth_2>0)) %>%
  group_by(borderid,year) %>%
  summarise(imagery_prop_1 = mean(ie1,na.rm=TRUE), imagery_prop_2 = mean(ie2,na.rm=TRUE)) %>% 
  ungroup()

coder_img <- merge(prop_imagery,num_coders,by="borderid",all.x=T,all.y=T)
data$border_coder_id <- paste(data$borderid,data$coder,sep="-") #coder-crossing ID
border_coder_list <- unique(data$border_coder_id) #list of coder-crossings 
cons_info <- unique(dplyr::select(data, borderid, construction_observed))


#######################################################
#Step 2 - Interpolation
#######################################################
data <- data %>% 
  arrange(border_coder_id, year) %>% 
  group_by(border_coder_id) %>%
  #Road Status
  mutate(road_status_1_locf_back = na_locf_fornback(road_status_1)) %>% 
  mutate(road_status_1_locf = na_locf_forward(road_status_1)) %>% 
  mutate(road_status_1_lin_back = na_interpolation_fornback(road_status_1)) %>% 
  mutate(road_status_1_lin = na_interpolation_forward(road_status_1)) %>% 
  mutate(road_status_2_locf_back = na_locf_fornback(road_status_2)) %>% 
  mutate(road_status_2_locf = na_locf_forward(road_status_2)) %>% 
  mutate(road_status_2_lin_back = na_interpolation_fornback(road_status_2)) %>% 
  mutate(road_status_2_lin = na_interpolation_forward(road_status_2)) %>% 
  #Gate 
  mutate(gate_1_locf_back = na_locf_fornback(gate_1)) %>% 
  mutate(gate_1_locf = na_locf_forward(gate_1)) %>% 
  mutate(gate_1_lin_back = na_interpolation_fornback(gate_1)) %>% 
  mutate(gate_1_lin = na_interpolation_forward(gate_1)) %>% 
  mutate(gate_2_locf_back = na_locf_fornback(gate_2)) %>% 
  mutate(gate_2_locf = na_locf_forward(gate_2)) %>% 
  mutate(gate_2_lin_back = na_interpolation_fornback(gate_2)) %>% 
  mutate(gate_2_lin = na_interpolation_forward(gate_2)) %>% 
  #Split
  mutate(split_1_locf_back = na_locf_fornback(split_1)) %>% 
  mutate(split_1_locf = na_locf_forward(split_1)) %>% 
  mutate(split_1_lin_back = na_interpolation_fornback(split_1)) %>% 
  mutate(split_1_lin = na_interpolation_forward(split_1)) %>% 
  mutate(split_2_locf_back = na_locf_fornback(split_2)) %>% 
  mutate(split_2_locf = na_locf_forward(split_2)) %>% 
  mutate(split_2_lin_back = na_interpolation_fornback(split_2)) %>% 
  mutate(split_2_lin = na_interpolation_forward(split_2)) %>% 
  #Ofbld
  mutate(ofbld_1_locf_back = na_locf_fornback(ofbld_1)) %>% 
  mutate(ofbld_1_locf = na_locf_forward(ofbld_1)) %>% 
  mutate(ofbld_1_lin_back = na_interpolation_fornback(ofbld_1)) %>% 
  mutate(ofbld_1_lin = na_interpolation_forward(ofbld_1)) %>% 
  mutate(ofbld_2_locf_back = na_locf_fornback(ofbld_2)) %>% 
  mutate(ofbld_2_locf = na_locf_forward(ofbld_2)) %>% 
  mutate(ofbld_2_lin_back = na_interpolation_fornback(ofbld_2)) %>% 
  mutate(ofbld_2_lin = na_interpolation_forward(ofbld_2)) %>% 
  #Numbld
  mutate(numbld_1_locf_back = na_locf_fornback(numbld_1)) %>% 
  mutate(numbld_1_locf = na_locf_forward(numbld_1)) %>% 
  mutate(numbld_1_lin_back = na_interpolation_fornback(numbld_1)) %>% 
  mutate(numbld_1_lin = na_interpolation_forward(numbld_1)) %>% 
  mutate(numbld_2_locf_back = na_locf_fornback(numbld_2)) %>% 
  mutate(numbld_2_locf = na_locf_forward(numbld_2)) %>% 
  mutate(numbld_2_lin_back = na_interpolation_fornback(numbld_2)) %>% 
  mutate(numbld_2_lin = na_interpolation_forward(numbld_2)) %>% 
  #Multi
  mutate(multilane_1_locf_back = na_locf_fornback(multilane_1)) %>% 
  mutate(multilane_1_locf = na_locf_forward(multilane_1)) %>% 
  mutate(multilane_1_lin_back = na_interpolation_fornback(multilane_1)) %>% 
  mutate(multilane_1_lin = na_interpolation_forward(multilane_1)) %>% 
  mutate(multilane_2_locf_back = na_locf_fornback(multilane_2)) %>% 
  mutate(multilane_2_locf = na_locf_forward(multilane_2)) %>% 
  mutate(multilane_2_lin_back = na_interpolation_fornback(multilane_2)) %>% 
  mutate(multilane_2_lin = na_interpolation_forward(multilane_2)) %>% 
  ungroup()
#Uncomment if you'd like a spreadhseet of the interpolated, coder-level data
#write.csv(data, file="bcw_interpolate_v1p1.csv",row.names=F, na="")


#######################################################
#Step 3 - Aggregation
#######################################################
#Split by coder type and assign weights
data_cat<-subset(data,coder_type=="codeathon")
data_ra<-subset(data,coder_type=="RA")
data_ra <- data_ra %>% 
  ungroup() %>% 
  dplyr::select(borderid,data_source,coder_type,year,lat,long,country1,country2,cow1,cow2,
                ends_with("_1"),ends_with("_2"),-contains("_con"),
                contains("_locf"),contains("_lin"),
                country1,country2) 
data_ra$weight<-2
data_cat <- data_cat %>% 
  ungroup() %>% 
  dplyr::select(borderid,data_source,coder_type,year,lat,long,country1,country2,cow1,cow2,
                ends_with("_1"),ends_with("_2"),-contains("_con"),
                contains("_locf"),contains("_lin"),
                country1,country2)
data_cat$weight<-1

#Remove CAT split-lane classifications
data_cat[,c('split_1_locf_back', 'split_1_locf', 'split_1_lin_back', 'split_1_lin',
            'split_2_locf_back', 'split_2_locf', 'split_2_lin_back', 'split_2_lin')] <- NA
#recombine
data2<- bind_rows(data_ra,data_cat)

#Take averages
data <- data2 %>% 
  group_by(borderid,data_source,year,country1,country2, cow1, cow2,lat,long) %>%
  summarize_at(vars(road_status_1_locf_back, road_status_1_locf, road_status_1_lin_back, road_status_1_lin,
                    road_status_2_locf_back, road_status_2_locf, road_status_2_lin_back, road_status_2_lin,
                    split_1_locf_back, split_1_locf, split_1_lin_back, split_1_lin,
                    split_2_locf_back, split_2_locf, split_2_lin_back, split_2_lin,
                    gate_1_locf_back, gate_1_locf, gate_1_lin_back, gate_1_lin,
                    gate_2_locf_back, gate_2_locf, gate_2_lin_back, gate_2_lin,
                    ofbld_1_locf_back, ofbld_1_locf, ofbld_1_lin_back, ofbld_1_lin,
                    ofbld_2_locf_back, ofbld_2_locf, ofbld_2_lin_back, ofbld_2_lin,
                    numbld_1_locf_back, numbld_1_locf, numbld_1_lin_back, numbld_1_lin,
                    numbld_2_locf_back, numbld_2_locf, numbld_2_lin_back, numbld_2_lin,
                    multilane_1_locf_back, multilane_1_locf, multilane_1_lin_back, multilane_1_lin,
                    multilane_2_locf_back, multilane_2_locf, multilane_2_lin_back, multilane_2_lin), ~ weighted.mean(., w = weight,na.rm=T))

#######################################################
#Step 4 - Make directed b.c. dyad version 
#######################################################
country1_df <- data
country2_df <- country1_df %>%
  rename("country2" = country1,"country1" = country2,
         "cow2"=cow1,"cow1"=cow2)
country1_df <- select(country1_df,borderid,data_source,country1,cow1,country2,cow2,year,contains("_1"),lat,long) 
country2_df <- select(country2_df,borderid,data_source,country1,cow1,country2,cow2,year,contains("_2"),lat,long)
colnames(country1_df) <- str_remove_all(colnames(country1_df),"_1")
colnames(country2_df) <- str_remove_all(colnames(country2_df),"_2")
data_dd <- rbind(country1_df,country2_df)

#######################################################
#Step 5 - Clean-up, add structures indexes
#######################################################
#add back coder/imagery variables
data <- merge(data, coder_img, by=c('borderid','year'),all.x=T,all.y=F)
data_dd <- merge(data_dd, coder_img, by=c('borderid','year'),all.x=T,all.y=F)

#Create structures index
data <- data  %>%
  mutate(structure_tot_1_lin = (gate_1_lin/2) + split_1_lin + (numbld_1_lin/4),
         structure_tot_2_lin = (gate_2_lin/2) + split_2_lin + (numbld_2_lin/4), 
         structure_tot_1_lin_back = (gate_1_lin_back/2) + split_1_lin_back + (numbld_1_lin_back/4),
         structure_tot_2_lin_back = (gate_2_lin_back/2) + split_2_lin_back + (numbld_2_lin_back/4), 
         structure_tot_1_locf = (gate_1_locf/2) + split_1_locf + (numbld_1_locf/4),
         structure_tot_2_locf = (gate_2_locf/2) + split_2_locf + (numbld_2_locf/4), 
         structure_tot_1_locf_back = (gate_1_locf_back/2) + split_1_locf_back + (numbld_1_locf_back/4),
         structure_tot_2_locf_back = (gate_2_locf_back/2) + split_2_locf_back + (numbld_2_locf_back/4))
data_dd <- data_dd  %>%
  mutate(structure_tot = (gate_locf/2) + split_locf + (numbld_locf/4),
         structure_tot_back = (gate_locf_back/2) + split_locf_back + (numbld_locf_back/4))

#Integrate construction info
data <- merge(data, cons_info, by="borderid", all.x=T, all.y=F)
data_dd <- merge(data_dd, cons_info, by="borderid", all.x=T, all.y=F)

#Reduce temporal domain
data <- subset(data, year>1992)
data_dd <- subset(data_dd, year>1992)

#Select and order columns
data_dd <- dplyr::select(data_dd, borderid,lat, long, year, country1, cow1, 
                         country2, cow2, structure_tot, structure_tot_back)

data <- relocate(data, borderid, country1, cow1, country2, cow2, lat, long, year,
                 imagery_prop_1, imagery_prop_2)
data <- relocate(data, -num_coders, -data_source, -construction_observed)

data <- arrange(data, borderid, year)
data_dd <- arrange(data_dd, borderid, cow1, year)

write.csv(data, file="bcw_full_v1p1.csv",row.names=F, na="")
write.csv(data_dd, file="bcw_v1p1.csv",row.names=F,na="")