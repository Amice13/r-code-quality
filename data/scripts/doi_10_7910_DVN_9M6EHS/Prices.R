#### PRICES ####
#calculating the crop prices per kg
crop_price<-data.frame(matrix(NA, nrow=nrow(dat_all), ncol=length(colnames(crop_sold_kg))))
colnames(crop_price)<- colnames(crop_sold_kg)
crop_price_means<-data.frame(matrix(NA, nrow=1, ncol=length(colnames(crop_sold_kg))))
colnames(crop_price_means)<- colnames(crop_sold_kg)
for(i in 1:length(colnames(crop_price)))
{
  for (j in 1:nrow(crop_sold_kg))
  {
    if (!is.na(crop_sold_kg[j,i])&& crop_sold_kg[j,i]>0) 
    {
      crop_price[j,i]<- crop_sold_income[j,i]/crop_sold_kg[j,i]
    }
  }
  crop_price_means[,i]<-median(crop_price[,i], na.rm=TRUE)
  # median_temp<-as.numeric(median(crop_price[,i], na.rm=TRUE))
  # price_quantile<- quantile(crop_price[,i],na.rm=TRUE)
  # price_IQR<- as.numeric(price_quantile[4])-as.numeric(price_quantile[2])
  # price_for_median<- crop_price[crop_price[,i]<median_temp-price_IQR & crop_price[,i]<median_temp+price_IQR,i]
  # crop_price_means[,i]<- median(price_for_median, na.rm=TRUE)
  # 
  # 
}






livestock_price<-data.frame(matrix(NA, nrow=nrow(dat_all), ncol=length(colnames(livestock_sold_number))))
colnames(livestock_price)<- colnames(livestock_sold_number)
livestock_price_means<-data.frame(matrix(NA, nrow=1, ncol=length(colnames(livestock_sold_number))))
colnames(livestock_price_means)<- colnames(livestock_sold_number)

for(i in 1:length(colnames(livestock_price)))
{
  for (j in 1:nrow(livestock_price))
  {
    if(!is.na(as.numeric(livestock_sold_number[j,i])&&as.numeric(livestock_sold_number[j,i])>0))
    {
      livestock_price[j,i]<- as.numeric(livestock_sold_income[j,i])/as.numeric(livestock_sold_number[j,i])
    }
  }
  livestock_price_means[,i]<-median(livestock_price[,i], na.rm=TRUE)
}

meat_price<-data.frame(matrix(NA, nrow=nrow(dat_all), ncol=length(colnames(meat_killed_for_number))))
colnames(meat_price)<- colnames(meat_killed_for_number)
meat_price_means<-data.frame(matrix(NA, nrow=1, ncol=length(colnames(meat_killed_for_number))))
colnames(meat_price_means)<- colnames(meat_killed_for_number)

for(i in 1:length(colnames(meat_price)))
{
  for (j in 1:nrow(meat_price))
  {
    if (!is.na(meat_sold_kg[j,i]) && meat_sold_kg[j,i]>0)
    {
      meat_price[j,i]<- meat_sold_income[j,i]/meat_sold_kg[j,i]
    }
  }
  meat_price_means[,i]<-median(meat_price[,i], na.rm=TRUE)
}

eggs_price<-data.frame(matrix(NA, nrow=nrow(dat_all), ncol=length(colnames(eggs_collected_per_day))))
colnames(eggs_price)<- colnames(eggs_collected_per_day)
eggs_price_means<-data.frame(matrix(NA, nrow=1, ncol=length(colnames(eggs_collected_per_day))))
colnames(eggs_price_means)<- colnames(eggs_collected_per_day)

for(i in 1:length(colnames(eggs_price)))
{
  for (j in 1:nrow(eggs_price))
  {
    if (!is.na(eggs_sold_per_day[j,i]) && eggs_sold_per_day[j,i]>0)
    {
      eggs_price[j,i]<- eggs_sold_per_day_income[j,i]/(eggs_sold_per_day[j,i])
    }
  }
  eggs_price_means[,i]<-median(eggs_price[,i], na.rm=TRUE)
}


milk_price_per_litre<-data.frame(matrix(NA, nrow=nrow(dat_all), ncol=length(colnames(average_milk_collected_per_day))))
colnames(milk_price_per_litre)<- colnames(average_milk_collected_per_day)
milk_price_means<-data.frame(matrix(NA, nrow=1, ncol=length(colnames(average_milk_collected_per_day))))
colnames(milk_price_means)<- colnames(average_milk_collected_per_day)

for(i in 1:length(colnames(milk_price_per_litre)))
{
  for (j in 1:nrow(milk_price_per_litre))
  {
    if( !is.na(average_milk_collected_per_day[j,i]) && average_milk_collected_per_day[j,i]>0)
    {
      milk_price_per_litre[j,i]<- milk_income_per_day[j,i]/(average_milk_collected_per_day[j,i])
    }
  }
  milk_price_means[,i]<-median(milk_price_per_litre[,i], na.rm=TRUE)
}

honey_price_per_kg<-data.frame(matrix(NA, nrow=nrow(dat_all), ncol=length(colnames(bees_honey_production_per_year))))
colnames(honey_price_per_kg)<- colnames(bees_honey_production_per_year)
honey_price_means<-data.frame(matrix(NA, nrow=1, ncol=length(colnames(bees_honey_production_per_year))))
colnames(honey_price_means)<- colnames(bees_honey_production_per_year)

for(i in 1:length(colnames(honey_price_per_kg)))
{
  for (j in 1:nrow(honey_price_per_kg))
  {
    if(!is.na(as.numeric(bees_honey_production_per_year[j,i])) && as.numeric(bees_honey_production_per_year[j,i])>0)
    {
      honey_price_per_kg[j,i]<- as.numeric(bees_honey_sold_income_per_year[j,i])/as.numeric(bees_honey_production_per_year[j,i])
    }
  }
  honey_price_means[,i]<-median(honey_price_per_kg[,i], na.rm=TRUE)
}

cheese_price_per_kg<- data.frame(matrix(NA, nrow=nrow(cheese_amount_per_day), ncol=1))
colnames(cheese_price_per_kg)<- colnames(cheese_amount_per_day)
cheese_price_means<-data.frame(matrix(NA, nrow=1, ncol=1))
colnames(cheese_price_means)<- colnames(cheese_amount_per_day)

for (j in nrow(cheese_amount_per_day[,1]))
{
  if(!is.na(cheese_amount_per_day[j,1])&&cheese_amount_per_day[j,1]>0)
  {
    cheese_price_per_kg[j,1]<- as.numeric(cheese_income_per_day[j,1])/as.numeric(cheese_amount_per_day[j,1])
  }
}
cheese_price_means<-median(cheese_price_per_kg[,1], na.rm=TRUE)



butter_price_per_kg<- data.frame(matrix(NA, nrow=nrow(dat_all), ncol=1))
colnames(butter_price_per_kg)<- colnames(butter_amount_per_day)
butter_price_means<-data.frame(matrix(NA, nrow=1, ncol=1))
colnames(butter_price_means)<- colnames(butter_amount_per_day)

for (j in nrow(butter_amount_per_day[,1]))
{
  if(!is.na(butter_amount_per_day[j,1])&&butter_amount_per_day[j,1]>0)
  {
    butter_price_per_kg[j,1]<- butter_income_per_day[j,1]/butter_amount_per_day[j,1]
  }
}
butter_price_means<-median(butter_price_per_kg[,1], na.rm=TRUE)

# crop_price_export<- t(crop_price_means)
# crop_price_export<- data.frame("Product"=row.names(crop_price_export), "Price"=as.numeric(crop_price_export[,1]))
# crop_price_export$Product <- paste0(crop_price_export$Product, "_", "1_kg")
# prices<- crop_price_export
# 
# livestock_price_export<- t(livestock_price_means)
# livestock_price_export<- data.frame("Product"=row.names(livestock_price_export), "Price"=as.numeric(livestock_price_export[,1]))
# crop_price_export$Product <- paste0(crop_price_export$Product, "_", "1_whole_animal")
# prices<- rbind(prices, livestock_price_export)
# 
# meat_price_export<- t(meat_price_means)
# meat_price_export<- data.frame("Product"=row.names(meat_price_export), "Price"=as.numeric(meat_price_export[,1]))
# meat_price_export$Product <- paste0(meat_price_export$Product, "_", "meat_1_kg")
# prices<- rbind(prices, meat_price_export)
# 
# milk_price_export<- t(milk_price_means)
# milk_price_export<- data.frame("Product"=row.names(milk_price_export), "Price"=as.numeric(milk_price_export[,1]))
# milk_price_export$Product <- paste0(milk_price_export$Product, "_", "milk_1_litre")
# prices<- rbind(prices, milk_price_export)
# 
# eggs_price_export<- t(eggs_price_means)
# eggs_price_export<- data.frame("Product"=row.names(eggs_price_export), "Price"=as.numeric(eggs_price_export[,1]))
# eggs_price_export$Product <- paste0(eggs_price_export$Product, "_", "1_egg")
# prices<- rbind(prices, eggs_price_export)
# 
# honey_price_export<- t(honey_price_means)
# honey_price_export<- data.frame("Product"=row.names(honey_price_export), "Price"=as.numeric(honey_price_export[,1]))
# honey_price_export$Product <- paste0(honey_price_export$Product, "_", "honey_1_litre")
# prices<- rbind(prices, honey_price_export)
# 
# 
# cheese_price_export<- data.frame("Product"="cheese_1_kg", "Price"=cheese_price_means)
# prices<- rbind(prices, cheese_price_export)
# butter_price_export<- data.frame("Product"="butter_1_kg", "Price"=butter_price_means)
# prices<- rbind(prices, butter_price_export)
dir.create(paste0("../../Indicator_Calculations"))
dir.create(paste0("../../Indicator_Calculations/Prices"))
#dir.create(paste0("../../Indicator_Calculations/Prices/",project_ID))


prev_crop_price<-data.frame(read_csv('../../Indicator_Calculations/Prices/crop_prices_per_kg.csv'),check.names = F)
prev_egg_price<-data.frame(read_csv('../../Indicator_Calculations/Prices/egg_price_per_egg.csv'),check.names = F)
prev_honey_price<-data.frame(read_csv('../../Indicator_Calculations/Prices/honey_price_per_litre.csv'),check.names = F)
prev_meat_price<-data.frame(read_csv('../../Indicator_Calculations/Prices/meat_prices_per_kg.csv'),check.names = F)
prev_milk_price<-data.frame(read_csv('../../Indicator_Calculations/Prices/milk_prices_per_l.csv'),check.names = F)
prev_lvstk_price<-data.frame(read_csv('../../Indicator_Calculations/Prices/whole_livestock_prices.csv'),check.names = F)


colnames(prev_crop_price)<- gsub('.', ' ', colnames(prev_crop_price), fixed=T)
colnames(prev_lvstk_price)<- gsub('.', ' ', colnames(prev_lvstk_price), fixed=T)
colnames(prev_meat_price)<- gsub('.', ' ', colnames(prev_meat_price), fixed=T)
colnames(prev_milk_price)<- gsub('.', ' ', colnames(prev_milk_price), fixed=T)
colnames(prev_egg_price)<- gsub('.', ' ', colnames(prev_egg_price), fixed=T)
colnames(prev_honey_price)<- gsub('.', ' ', colnames(prev_honey_price), fixed=T)

ID_USED<-project_ID

if (ID_USED%in%prev_crop_price$SURVEY_ID)
{
  matching_column_orders<-colnames(prev_crop_price[which(prev_crop_price$SURVEY_ID==ID_USED), which(colnames(prev_crop_price)%in%colnames(crop_price_means))])
  prev_crop_price[which(prev_crop_price$SURVEY_ID==ID_USED), which(colnames(prev_crop_price)%in%colnames(crop_price_means))]<-crop_price_means[1, c(matching_column_orders)]
}else{new_data<-data.frame('ID_PROJ'=unique(PROJECT),'ID_COUNTRY'=unique(COUNTRY_CODE),'YEAR'=as.numeric(unique(YEAR)),'ITERATION'=unique(ITERATION),'SURVEY_ID'=ID_USED,crop_price_means)
  colnames(new_data)<- gsub('.', ' ', colnames(new_data), fixed=T)
  prev_crop_price<-rbind.fill(prev_crop_price,new_data)}

if (ID_USED%in%prev_egg_price$SURVEY_ID)
{
  matching_column_orders<-colnames(  prev_egg_price[which(prev_egg_price$SURVEY_ID==ID_USED), which(colnames(prev_egg_price)%in%colnames(eggs_price_means))])
  prev_egg_price[which(prev_egg_price$SURVEY_ID==ID_USED), which(colnames(prev_egg_price)%in%colnames(eggs_price_means))]<-eggs_price_means[1, c(matching_column_orders)]
}else{new_data<-data.frame('ID_PROJ'=unique(PROJECT),'ID_COUNTRY'=unique(COUNTRY_CODE),'YEAR'=as.numeric(unique(YEAR)),'ITERATION'=unique(ITERATION),'SURVEY_ID'=ID_USED,eggs_price_means)
colnames(new_data)<- gsub('.', ' ', colnames(new_data), fixed=T)
prev_egg_price<-rbind.fill(prev_egg_price,new_data)}

if (ID_USED%in%prev_honey_price$SURVEY_ID)
{
  matching_column_orders<-colnames(  prev_honey_price[which(prev_honey_price$SURVEY_ID==ID_USED), which(colnames(prev_honey_price)%in%colnames(honey_price_means))])
  prev_honey_price[which(prev_honey_price$SURVEY_ID==ID_USED), which(colnames(prev_honey_price)%in%colnames(honey_price_means))]<-honey_price_means[1, c(matching_column_orders)]
}else{new_data<-data.frame('ID_PROJ'=unique(PROJECT),'ID_COUNTRY'=unique(COUNTRY_CODE),'YEAR'=as.numeric(unique(YEAR)),'ITERATION'=unique(ITERATION),'SURVEY_ID'=ID_USED,honey_price_means)
colnames(new_data)<- gsub('.', ' ', colnames(new_data), fixed=T)
prev_honey_price<-rbind.fill(prev_honey_price,new_data)}

if (ID_USED%in%prev_meat_price$SURVEY_ID)
{
  matching_column_orders<-colnames(  prev_meat_price[which(prev_meat_price$SURVEY_ID==ID_USED), which(colnames(prev_meat_price)%in%colnames(meat_price_means))])
  prev_meat_price[which(prev_meat_price$SURVEY_ID==ID_USED), which(colnames(prev_meat_price)%in%colnames(meat_price_means))]<-meat_price_means[1, c(matching_column_orders)]
}else{new_data<-data.frame('ID_PROJ'=unique(PROJECT),'ID_COUNTRY'=unique(COUNTRY_CODE),'YEAR'=as.numeric(unique(YEAR)),'ITERATION'=unique(ITERATION),'SURVEY_ID'=ID_USED,meat_price_means)
colnames(new_data)<- gsub('.', ' ', colnames(new_data), fixed=T)
prev_meat_price<-rbind.fill(prev_meat_price,new_data)}

if (ID_USED%in%prev_milk_price$SURVEY_ID)
{
  matching_column_orders<-colnames(  prev_milk_price[which(prev_milk_price$SURVEY_ID==ID_USED), which(colnames(prev_milk_price)%in%colnames(milk_price_means))])
  prev_milk_price[which(prev_milk_price$SURVEY_ID==ID_USED), which(colnames(prev_milk_price)%in%colnames(milk_price_means))]<-milk_price_means[1, c(matching_column_orders)]
}else{new_data<-data.frame('ID_PROJ'=unique(PROJECT),'ID_COUNTRY'=unique(COUNTRY_CODE),'YEAR'=as.numeric(unique(YEAR)),'ITERATION'=unique(ITERATION),'SURVEY_ID'=ID_USED,milk_price_means)
colnames(new_data)<- gsub('.', ' ', colnames(new_data), fixed=T)
prev_milk_price<-rbind.fill(prev_milk_price,new_data)}

if (ID_USED%in%prev_lvstk_price$SURVEY_ID)
{
  matching_column_orders<-colnames(  prev_lvstk_price[which(prev_lvstk_price$SURVEY_ID==ID_USED), which(colnames(prev_lvstk_price)%in%colnames(livestock_price_means))])
  prev_lvstk_price[which(prev_lvstk_price$SURVEY_ID==ID_USED), which(colnames(prev_lvstk_price)%in%colnames(livestock_price_means))]<-livestock_price_means[1, c(matching_column_orders)]
}else{new_data<-data.frame('ID_PROJ'=unique(PROJECT),'ID_COUNTRY'=unique(COUNTRY_CODE),'YEAR'=as.numeric(unique(YEAR)),'ITERATION'=unique(ITERATION),'SURVEY_ID'=ID_USED,livestock_price_means)
colnames(new_data)<- gsub('.', ' ', colnames(new_data), fixed=T)
prev_lvstk_price<-rbind.fill(prev_lvstk_price,new_data)}



colnames(prev_crop_price)<- gsub('.', ' ', colnames(prev_crop_price), fixed=T)
colnames(prev_lvstk_price)<- gsub('.', ' ', colnames(prev_lvstk_price), fixed=T)
colnames(prev_meat_price)<- gsub('.', ' ', colnames(prev_meat_price), fixed=T)
colnames(prev_milk_price)<- gsub('.', ' ', colnames(prev_milk_price), fixed=T)
colnames(prev_egg_price)<- gsub('.', ' ', colnames(prev_egg_price), fixed=T)
colnames(prev_honey_price)<- gsub('.', ' ', colnames(prev_honey_price), fixed=T)


write_csv(prev_crop_price,paste0("../../Indicator_Calculations/Prices/crop_prices_per_kg.csv"))#,row.names = F, fileEncoding = 'UTF-8')
write_csv(prev_lvstk_price,paste0("../../Indicator_Calculations/Prices/whole_livestock_prices.csv"))#,row.names = F, fileEncoding = 'UTF-8')
write_csv(prev_meat_price,paste0("../../Indicator_Calculations/Prices/meat_prices_per_kg.csv"))#,row.names = F, fileEncoding = 'UTF-8')
write_csv(prev_milk_price,paste0("../../Indicator_Calculations/Prices/milk_prices_per_l.csv"))##,row.names = F, fileEncoding = 'UTF-8')
write_csv(prev_egg_price,paste0("../../Indicator_Calculations/Prices/egg_price_per_egg.csv"))#,row.names = F, fileEncoding = 'UTF-8')
write_csv(prev_honey_price,paste0("../../Indicator_Calculations/Prices/honey_price_per_litre.csv"))#,row.names = F, fileEncoding = 'UTF-8')

# if (sum(cheese_price_per_kg[,1],na.rm = T)>0)
# {
#   write_csv(data.frame('ID_PROJ'=unique(PROJECT),'ID_COUNTRY'=unique(COUNTRY_CODE),'YEAR'=unique(YEAR),'ITERATION'=unique(ITERATION),'SURVEY_ID'=ID_USED,cheese_price_means),paste0("../../Indicator_Calculations/Prices/",project_ID,"/cheese_price_per_kg.csv"))#,row.names = F, fileEncoding = 'UTF-8')
# }else{write_csv(data.frame('ID_PROJ'=unique(PROJECT),'ID_COUNTRY'=unique(COUNTRY_CODE),'YEAR'=unique(YEAR),'ITERATION'=unique(ITERATION),'SURVEY_ID'=ID_USED,'cheese'=NA),paste0("../../Indicator_Calculations/Prices/",project_ID,"/cheese_price_per_kg.csv"))}
# 
# if (sum(butter_price_per_kg[,1],na.rm = T)>0)
# {
#   write_csv('ID_PROJ'=unique(PROJECT),'ID_COUNTRY'=unique(COUNTRY_CODE),'YEAR'=unique(YEAR),'ITERATION'=unique(ITERATION),'SURVEY_ID'=ID_USED,butter_price_means,paste0("../../Indicator_Calculations/Prices/",project_ID,"/butter_price_per_kg.csv"))#,row.names = F, fileEncoding = 'UTF-8')
# }else{write_csv(data.frame('ID_PROJ'=unique(PROJECT),'ID_COUNTRY'=unique(COUNTRY_CODE),'YEAR'=unique(YEAR),'ITERATION'=unique(ITERATION),'SURVEY_ID'=ID_USED,'butter'=NA),paste0("../../Indicator_Calculations/Prices/",project_ID,"/butter_price_per_kg.csv"))}


##### NTFP PRICES ####

# shea_fruit_df$Shea_fruit_price<-as.numeric(shea_fruit_df$shea_fruit_sold_income_weekly)/shea_fruit_df$amount_sold_kg
# shea_fruit_df$Mean_Price<- mean(shea_fruit_df$Shea_fruit_price, na.rm = T)
# 
# shea_nut_df$Shea_nut_price<-as.numeric(shea_nut_df$Sold_Income_per_Week)/shea_nut_df$amount_sold_kg
# shea_nut_df$Mean_Price<- mean(shea_nut_df$Shea_nut_price, na.rm=T)
# 
# shea_butter_df$Shea_butter_price<- as.numeric(as.numeric(shea_butter_df$Sold_Income_per_Week)/shea_butter_df$amount_sold_kg)
# shea_butter_df$Mean_Price<- mean(shea_butter_df$Shea_butter_price, na.rm=T)
# 
# baobab_fruit_df$baobab_fruits_price<-as.numeric(as.numeric(baobab_fruit_df$Sold_Income_per_Week)/baobab_fruit_df$amount_sold_kg)
# baobab_fruit_df$Mean_Price<- mean(baobab_fruit_df$baobab_fruits_price, na.rm=T)
# 
# baobab_leaves_df$baobab_leaves_price<-as.numeric(as.numeric(baobab_leaves_df$Sold_Income_per_Week)/baobab_leaves_df$amount_sold_kg)
# baobab_leaves_df$Mean_Price<- mean(baobab_leaves_df$baobab_leaves_price, na.rm=T)
# 
# moringa_leaves_df$moringa_leaves_price<-as.numeric(as.numeric(moringa_leaves_df$Sold_Income_per_Week)/moringa_leaves_df$amount_sold_kg)
# moringa_leaves_df$Mean_Price<- mean(moringa_leaves_df$moringa_leaves_price, na.rm=T)
# 
# balanites_fruit_df$balanites_fruit_price<- as.numeric(as.numeric(balanites_fruit_df$Sold_Income_per_Week)/balanites_fruit_df$amount_sold_kg)
# balanites_fruit_df$Mean_Price<- mean(balanites_fruit_df$balanites_fruit_price, na.rm=T)
# 
# balanites_nut_df$balanites_nut_price<-  as.numeric(as.numeric(balanites_nut_df$Sold_Income_per_Week)/balanites_nut_df$amount_sold_kg)
# balanites_nut_df$Mean_Price<- mean(balanites_nut_df$balanites_nut_price, na.rm=T)
# 
# balanites_leaves_df$balanites_leaves_price<- as.numeric(as.numeric(balanites_leaves_df$Sold_Income_per_Week)/balanites_leaves_df$amount_sold_kg)
# balanites_leaves_df$Mean_Price<- mean(balanites_leaves_df$balanites_leaves_price, na.rm=T)
# 
# honey_df$honey_price<- as.numeric(as.numeric(honey_df$Sold_Income_per_Week)/honey_df$amount_sold_kg)
# honey_df$Mean_Price<- mean(honey_df$honey_price, na.rm=T)
# 
# locust_bean_df$locust_bean_price<-  as.numeric(as.numeric(locust_bean_df$Sold_Income_per_Week)/locust_bean_df$amount_sold_kg)
# locust_bean_df$locust_bean_price[is.infinite(locust_bean_df$locust_bean_price)]<-NA
# locust_bean_df$Mean_Price<- mean(locust_bean_df$locust_bean_price, na.rm=T)
# 
# gum_arabic_df$gumarabic_price<-as.numeric(as.numeric(gum_arabic_df$Sold_Income_per_Week)/gum_arabic_df$amount_sold_kg)
# gum_arabic_df$Mean_Price<- mean(gum_arabic_df$gumarabic_price, na.rm=T)
# 
# tamarind_df$tamarind_price<-as.numeric(as.numeric(tamarind_df$Sold_Income_per_Week)/tamarind_df$amount_sold_kg)
# tamarind_df$Mean_Price<- mean(tamarind_df$tamarind_price, na.rm=T)
# 
# sabasenegal_df$sabasenegal_price<-as.numeric(as.numeric(sabasenegal_df$Sold_Income_per_Week)/sabasenegal_df$amount_sold_kg)
# sabasenegal_df$Mean_Price<- mean(sabasenegal_df$sabasenegal_price, na.rm=T)
# 
# jujube_df$jujube_price<-as.numeric(as.numeric(jujube_df$Sold_Income_per_Week)/jujube_df$amount_sold_kg)
# jujube_df$Mean_Price<- mean(jujube_df$jujube_price, na.rm=T)
# 
# tallow_df$tallow_price<-as.numeric(as.numeric(tallow_df$Sold_Income_per_Week)/tallow_df$amount_sold_kg)
# tallow_df$Mean_Price<- mean(tallow_df$tallow_price, na.rm=T)
# 
# cashew_df$cashew_price<- as.numeric(as.numeric(cashew_df$Sold_Income_per_Week)/cashew_df$amount_sold_kg)
# cashew_df$Mean_Price<- mean(cashew_df$cashew_price, na.rm=T)
# 
# wild_grape_df$wild_grape_price<- as.numeric(as.numeric(wild_grape_df$Sold_Income_per_Week)/wild_grape_df$amount_sold_kg)
# wild_grape_df$Mean_Price<- mean(wild_grape_df$wild_grape_price, na.rm=T)
# 
# NTFP_mean_prices<- data.frame("shea_fruit"=unique(shea_fruit_df$Mean_Price),
#                               "shea_nut"=unique(shea_nut_df$Mean_Price),
#                               "shea_butter"=unique(shea_butter_df$Mean_Price),
#                               "baobab_fruit"=unique(baobab_fruit_df$Mean_Price),
#                               "baobab_leaves"=unique(baobab_leaves_df$Mean_Price),
#                               "moringa_leaves"=unique(moringa_leaves_df$Mean_Price),
#                               "balanites_fruit"=unique(balanites_fruit_df$Mean_Price),
#                               "balanites_nut"=unique(balanites_nut_df$Mean_Price),
#                               "balanites_leaves"=unique(balanites_leaves_df$Mean_Price),
#                               "honey"=unique(honey_df$Mean_Price),
#                               "locust_bean"=unique(locust_bean_df$Mean_Price),
#                               "gum_arabic"=unique(gum_arabic_df$Mean_Price),
#                               "tamarind"=unique(tamarind_df$Mean_Price),
#                               "sabasenegal"=unique(sabasenegal_df$Mean_Price),
#                               "jujube"=unique(jujube_df$Mean_Price),
#                               "tallow"=unique(tallow_df$Mean_Price),
#                               "cashew"=unique(cashew_df$Mean_Price),
#                               "wild_grape"=unique(wild_grape_df$Mean_Price))

#write.csv(NTFP_mean_prices, "Prices/Niger_2016/mean_NTFP_prices_per_kg.csv")





