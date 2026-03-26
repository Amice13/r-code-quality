
# Value Calculations

# A script to calculate values for crops consumed, crop yield, livestock held, livestock products consumed etc...

#### valuefarmproduce, valuecropproduce, valuecropconsumed, livestockprodsales, valuelivestockproduction, valuelivestockprodconsumed ####
#SEARCH_value_crop_produce_USD_PPP_pHH_Yr: Follow the variable created below for the remainder of the calculation
value_crop_produce_each_crop<-data.frame(matrix(NA, nrow=nrow(dat_all), ncol=length(colnames(crop_yield_kg))))
colnames(value_crop_produce_each_crop)<- colnames(crop_yield_kg)
#SEARCH_value_crop_consumed_USD_PPP_pHH_Yr: Follow the variable created below for the remainder of the calculation
value_crop_consumed_each_crop<-data.frame(matrix(NA, nrow=nrow(dat_all), ncol=length(colnames(crop_yield_kg))))
colnames(value_crop_consumed_each_crop)<- colnames(crop_yield_kg)
for (i in 1:length(colnames(value_crop_produce_each_crop)))
{
  
  value_crop_produce_each_crop[,i]<- crop_yield_kg[,i]*crop_price_means[1,i]
  value_crop_consumed_each_crop[,i]<- crop_consumed_kg[,i]*crop_price_means[1,i]
  
}
#value_crop_produce<- data.frame("value_crop_produce"= rowSums(value_crop_produce_each_crop, na.rm = TRUE))
value_crop_consumed<-data.frame("value_crop_consumed"= rowSums(value_crop_consumed_each_crop, na.rm = TRUE))
value_crop_produce<- data.frame("value_crop_produce"= rowSums(data.frame(value_crop_consumed_each_crop, crop_sold_income),na.rm = TRUE))


#SEARCH_value_livestock_production_USD_PPP_pHH_Yr
#SEARCH_value_livestock_prod_consumed_USD_PPP_pHH_Yr

value_livestock_each_animal<-data.frame(matrix(NA, nrow=nrow(dat_all), ncol=length(colnames(livestock_number))))
colnames(value_livestock_each_animal)<- colnames(livestock_number)


value_meat_each_animal<-data.frame(matrix(NA, nrow=nrow(dat_all), ncol=length(colnames(meat_killed_for_number))))
colnames(value_meat_each_animal)<- colnames(meat_killed_for_number)
value_meat_each_animal_consumed<-data.frame(matrix(NA, nrow=nrow(dat_all), ncol=length(colnames(meat_killed_for_number))))
colnames(value_meat_each_animal_consumed)<- colnames(meat_killed_for_number)

value_milk_each_animal<-data.frame(matrix(NA, nrow=nrow(dat_all), ncol=length(colnames(average_milk_collected_per_day))))
colnames(value_milk_each_animal)<- colnames(average_milk_collected_per_day)
value_milk_each_animal_consumed<-data.frame(matrix(NA, nrow=nrow(dat_all), ncol=length(colnames(average_milk_collected_per_day))))
colnames(value_milk_each_animal_consumed)<- colnames(average_milk_collected_per_day)

value_eggs_each_animal<-data.frame(matrix(NA, nrow=nrow(dat_all), ncol=length(colnames(eggs_collected_per_day))))
colnames(value_eggs_each_animal)<- colnames(eggs_collected_per_day)
value_eggs_each_animal_consumed<-data.frame(matrix(NA, nrow=nrow(dat_all), ncol=length(colnames(eggs_collected_per_day))))
colnames(value_eggs_each_animal_consumed)<- colnames(eggs_collected_per_day)

value_honey_each_animal<-data.frame(matrix(NA, nrow=nrow(dat_all), ncol=length(colnames(bees_honey_production_per_year))))
colnames(value_honey_each_animal)<- colnames(bees_honey_production_per_year)
value_honey_each_animal_consumed<-data.frame(matrix(NA, nrow=nrow(dat_all), ncol=length(colnames(bees_honey_production_per_year))))
colnames(value_honey_each_animal_consumed)<- colnames(bees_honey_production_per_year)

for (i in colnames(livestock_number))
{
  value_livestock_each_animal[,i]<- as.numeric(as.character(livestock_number[,i]))*livestock_price_means[1,i]
}
for (i in colnames(meat_killed_for_number))
{
  
  value_meat_each_animal[,i]<- meat_amount_kg[,i]*meat_price_means[1,i]
  value_meat_each_animal_consumed[,i]<- meat_consumed_kg[,i]*meat_price_means[1,i]
  
  value_milk_each_animal[,i]<- average_milk_collected_per_day[,i]*365*milk_price_means[1,i]
  value_milk_each_animal_consumed[,i]<- average_milk_consumed_per_day[,i]*365*milk_price_means[1,i]
  
  value_eggs_each_animal[,i]<- eggs_collected_per_day[,i]*365*eggs_price_means[1,i]
  value_eggs_each_animal_consumed[,i]<- eggs_consumed_per_day[,i]*365*eggs_price_means[1,i]
  
  value_honey_each_animal[,i]<- bees_honey_production_per_year[,i]*honey_price_means[1,i]
  value_honey_each_animal_consumed[,i]<-bees_honey_consumed_per_year[,i]*honey_price_means[1,i]
}
value_livestock<- data.frame("value_livestock"= rowSums(value_livestock_each_animal, na.rm = TRUE))
# value_meat<- data.frame("value_meat"= rowSums(value_meat_each_animal, na.rm = TRUE))
# value_milk<- data.frame("value_milk"= rowSums(value_milk_each_animal, na.rm = TRUE))
# value_eggs<- data.frame("value_eggs"= rowSums(value_eggs_each_animal, na.rm = TRUE))
# value_honey<- data.frame("value_honey"= rowSums(value_honey_each_animal, na.rm = TRUE))

value_meat_consumed<- data.frame("value_meat_consumed"= rowSums(value_meat_each_animal_consumed, na.rm = TRUE))
value_milk_consumed<- data.frame("value_milk_consumed"= rowSums(value_milk_each_animal_consumed, na.rm = TRUE))
value_eggs_consumed<- data.frame("value_eggs_consumed"= rowSums(value_eggs_each_animal_consumed, na.rm = TRUE))
value_honey_consumed<- data.frame("value_honey_consumed"= rowSums(value_honey_each_animal_consumed, na.rm = TRUE))


value_meat<- data.frame("value_meat"= rowSums(data.frame(value_meat_consumed, meat_income), na.rm = TRUE))
value_milk<- data.frame("value_milk"= rowSums(data.frame(value_milk_consumed, milk_income), na.rm = TRUE))
value_eggs<- data.frame("value_eggs"= rowSums(data.frame(value_eggs_consumed, eggs_income), na.rm = TRUE))
value_honey<- data.frame("value_honey"= rowSums(data.frame(value_honey_consumed, bees_honey_sold_income_per_year), na.rm = TRUE))

value_farm_produce<-data.frame(value_crop_produce, 
                               # value_livestock,    ##### REMOVE_VALUE_LIVESTOCK
                               value_meat,
                               value_milk,
                               value_eggs,
                               value_honey,
                               livestock_income)

value_farm_produce_consumed<-data.frame(value_crop_consumed, 
                                        value_meat_consumed,
                                        value_milk_consumed,
                                        value_eggs_consumed,
                                        value_honey_consumed)

bees_honey_sold_income_per_year<-apply(bees_honey_sold_income_per_year,2,as.numeric)
prod_sales<- data.frame("honey"=rowSums(bees_honey_sold_income_per_year, na.rm=TRUE),
                        "meat"=rowSums(meat_income, na.rm=TRUE),
                        "eggs"=rowSums(eggs_income, na.rm=TRUE),
                        "milk"=rowSums(milk_income, na.rm=TRUE),
                        "whole_livestock"=livestock_income[,1] )

#SEARCH_value_farm_produce_USD_PPP_pHH_Yr
valuefarmproduce<- data.frame("valuefarmproduce"= rowSums(value_farm_produce, na.rm = TRUE))
#SEARCH_crop_sales_USD_PPP_pHH_Yr
cropsales<-data.frame("cropsales"= rowSums(crop_sold_income, na.rm = TRUE))
#SEARCH_livestock_prodsales_USD_PPP_pHH_Yr
livestockprodsales<- data.frame("livestockprodsales"= rowSums(prod_sales, na.rm = TRUE))
#livestocksales<-data.frame("livestocksales"= rowSums(livestock_sold_income, na.rm = TRUE))
#SEARCH_value_crop_produce_USD_PPP_pHH_Yr
valuecropproduce<- data.frame("valuecropproduce"= rowSums(value_crop_produce, na.rm = TRUE))
#SEARCH_value_livestock_production_USD_PPP_pHH_Yr
valuelivestockproduce<- data.frame("valuelivestockproduce"= valuefarmproduce-valuecropproduce)
#SEARCH_value_crop_consumed_USD_PPP_pHH_Yr
value_crop_consumed<-data.frame("value_crop_consumed"= rowSums(value_crop_consumed, na.rm = TRUE))
#SEARCH_value_livestock_prod_consumed_USD_PPP_pHH_Yr
valuelivestockprodconsumed<-data.frame("valuelivestockprodconsumed"= rowSums(value_farm_produce_consumed, na.rm = TRUE)-value_crop_consumed[,1])



