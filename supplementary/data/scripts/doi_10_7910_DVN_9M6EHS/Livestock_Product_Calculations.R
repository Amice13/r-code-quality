



#' A script calculating the amount produced, sold and the income from sales for livestock products. 
#' The specific units relating to each livestock products are located within this script file

#' These calculations largely follow the same process except for Butter and cheese. 
#' Additionally with each of the livestock products there are some specific adjustments that have to be made
#' in the calculation

#------------------
#### Meat Calculations ####

# Creating empty data frames for information about meat #

length_temp<- length(grep("livestock_name_", colnames(dat_all)))
meat_killed_for_number<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(meat_killed_for_number)<- unique_livestock
#SEARCH_Meat_Amount: Follow the variable created below for the remainder of the calculation
meat_amount_kg<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(meat_amount_kg)<- unique_livestock
#SEARCH_Meat_Income: Follow the variable created below for the remainder of the calculation
meat_sold_income<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(meat_sold_income)<- unique_livestock
#SEARCH_Meat_Sold: Follow the variable created below for the remainder of the calculation
meat_sold_kg<- data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(meat_sold_kg)<- unique_livestock
#SEARCH_Milk_Consumed: Follow the variable created below for the remainder of the calculation
meat_consumed_kg<- data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(meat_consumed_kg)<- unique_livestock
meat_sold_fraction_animal<- data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(meat_sold_fraction_animal)<- unique_livestock
meat_consumed_fraction_animal<- data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(meat_consumed_fraction_animal)<- unique_livestock

Male_Youth_meat_sold<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(Male_Youth_meat_sold)<- unique_livestock
Male_Youth_meat_consumed<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(Male_Youth_meat_consumed)<- unique_livestock
Male_meat_sold<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(Male_meat_sold)<- unique_livestock
Male_meat_consumed<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(Male_meat_consumed)<- unique_livestock
Female_Youth_meat_sold<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(Female_Youth_meat_sold)<- unique_livestock
Female_Youth_meat_consumed<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(Female_Youth_meat_consumed)<- unique_livestock
Female_meat_sold<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(Female_meat_sold)<- unique_livestock
Female_meat_consumed<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(Female_meat_consumed)<- unique_livestock

# for these calculations the weight of each animal killed is used to determine the amount of meat.
for (i in 1:length_temp)
{
  
  killed_for_meat_number_temp<- dat_all[,colnames(dat_all)==paste0("killed_for_meat_",i)]
  consumed_prop_column<- dat_all[,colnames(dat_all)==paste0("meat_consumed_amount_",i)]
  consumed_prop_column<- tolower(gsub("NA", NA,consumed_prop_column))
  sold_prop_column<- dat_all[,colnames(dat_all)==paste0("meat_sell_amount_",i)]
  sold_prop_column<- tolower(gsub("NA", NA,sold_prop_column))
  sold_income_column<-  dat_all[,colnames(dat_all)==paste0("meat_sold_income_",i)]
  who_control_sold<-  dat_all[,colnames(dat_all)==paste0("livestock_meat_who_sells_",i)]
  who_control_sold<- tolower(gsub("NA", NA,who_control_sold))
  who_control_consume<-  dat_all[,colnames(dat_all)==paste0("livestock_meat_who_sells_",i)]
  who_control_consume<- tolower(gsub("NA", NA,who_control_consume))
  
  use_column<- dat_all[,colnames(dat_all)==paste0("meat_use_",i)]
  use_column<-tolower(gsub("NA",NA,use_column))
  use_column<-gsub("c(","",use_column, fixed=T)
  use_column<-gsub("[[:punct:]]","",use_column)
  
  for(j in 1:nrow(dat_all))
  {
    if(consumed_prop_column[j] %in% colnames(prop_switch)==TRUE)
    {consumed_prop_column[j]<- prop_switch[1,colnames(prop_switch)==consumed_prop_column[j]]}
    
    if(sold_prop_column[j] %in% colnames(prop_switch)==TRUE)
    {sold_prop_column[j]<- prop_switch[1,colnames(prop_switch)==sold_prop_column[j]]}
    
    use_temp<- as.character(use_column[j])
    use_temp<- unlist(strsplit(use_temp, " "))
    
    if(length(use_temp)==1)
    {
      if(!is.na(use_temp))
      {
        if (use_temp=="use")
        {
          consumed_prop_column[j]<-1
        }
        
        if (use_temp=="sell")
        {
          sold_prop_column[j]<-1
        }
        
      }
      
    }
    
    
    
    gender_sold_temp<-unlist(strsplit(as.character(who_control_sold[j]), " "))
    # for (gend in 1:length(gender_sold_temp))
    # {
    #   if(gender_sold_temp[gend] %in% colnames(Male_female_control_switch)==TRUE)
    #   {gender_sold_temp[gend]<- as.character(Male_female_control_switch[1,colnames(Male_female_control_switch)==gender_sold_temp[gend]])}
    # }
    
    
    gender_consume_temp<-unlist(strsplit(as.character(who_control_consume[j]), " "))
    # for (gend in 1:length(gender_consume_temp))
    # {
    #   if(gender_consume_temp[gend] %in% colnames(Male_female_control_switch)==TRUE)
    #   {gender_consume_temp[gend]<- as.character(Male_female_control_switch[1,colnames(Male_female_control_switch)==gender_consume_temp[gend]])}
    # }
    
    # if(who_control_sold[j] %in% colnames(Male_female_control_switch)==TRUE)
    # {who_control_sold[j]<- as.character(Male_female_control_switch[1,colnames(Male_female_control_switch)==who_control_sold[j]])}
    # 
    # if(who_control_consume[j] %in% colnames(Male_female_control_switch)==TRUE)
    # {who_control_consume[j]<- as.character(Male_female_control_switch[1,colnames(Male_female_control_switch)==who_control_consume[j]])}
    # 
    
    if (dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)] %in% unique_livestock==TRUE)
    {
      meat_killed_for_number[j,colnames(meat_killed_for_number)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-as.numeric(killed_for_meat_number_temp[j])
      meat_sold_income[j,colnames(meat_sold_income)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-as.numeric(sold_income_column[j])
      meat_sold_fraction_animal[j,colnames(meat_sold_fraction_animal)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-as.numeric(killed_for_meat_number_temp[j])*as.numeric(sold_prop_column[j])
      meat_consumed_fraction_animal[j,colnames(meat_consumed_fraction_animal)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-as.numeric(killed_for_meat_number_temp[j])*as.numeric(consumed_prop_column[j])
      
      if(dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]%in% weight_killed_animals_set[,1]==TRUE)
      {
        meat_amount_kg[j,colnames(meat_amount_kg)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<- as.numeric(killed_for_meat_number_temp[j])*as.numeric(weight_killed_animals_set[weight_killed_animals_set[,1]==as.character(dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]),2])
        meat_sold_kg[j,colnames(meat_sold_kg)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-as.numeric(killed_for_meat_number_temp[j])*as.numeric(sold_prop_column[j])*as.numeric(weight_killed_animals_set[weight_killed_animals_set[,1]==as.character(dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]),2])
        meat_consumed_kg[j,colnames(meat_consumed_kg)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-as.numeric(killed_for_meat_number_temp[j])*as.numeric(consumed_prop_column[j])*as.numeric(weight_killed_animals_set[weight_killed_animals_set[,1]==as.character(dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]),2])
        
        #  gender_sold_temp<-unlist(strsplit(as.character(who_control_sold[j]), " "))
        #  gender_consume_temp<- unlist(strsplit(as.character(who_control_consume[j]), " "))
        sold<- meat_sold_kg[j,colnames(meat_sold_kg)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]
        consumed<- meat_consumed_kg[j,colnames(meat_consumed_kg)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]
        if("female_youth" %in% gender_consume_temp==TRUE)
        {Female_Youth_meat_consumed[j,colnames(Female_Youth_meat_consumed)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-consumed/length(gender_consume_temp)}
        if("female_adult" %in% gender_consume_temp==TRUE | "female_head" %in% gender_consume_temp==TRUE)
        {Female_meat_consumed[j,colnames(Female_meat_consumed)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-consumed/length(gender_consume_temp)}
        if("male_youth" %in% gender_consume_temp==TRUE)
        {Male_Youth_meat_consumed[j,colnames(Male_Youth_meat_consumed)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-consumed/length(gender_consume_temp)}
        if("male_adult" %in% gender_consume_temp==TRUE | "male_head" %in% gender_consume_temp==TRUE)
        {Male_meat_consumed[j,colnames(Male_meat_consumed)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-consumed/length(gender_consume_temp)}
        if("female_youth" %in% gender_sold_temp==TRUE)
        {Female_Youth_meat_sold[j,colnames(Female_Youth_meat_sold)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-sold/length(gender_sold_temp)}
        if("female_adult" %in%gender_sold_temp==TRUE | "female_adult" %in%gender_sold_temp==TRUE)
        {Female_meat_sold[j,colnames(Female_meat_sold)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-sold/length(gender_sold_temp)}
        if("male_youth" %in% gender_sold_temp==TRUE)
        {Male_Youth_meat_sold[j,colnames(Male_Youth_meat_sold)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-sold/length(gender_sold_temp)}
        if("male_adult" %in%gender_sold_temp==TRUE | "male_adult" %in%gender_sold_temp==TRUE)
        {Male_meat_sold[j,colnames(Male_meat_sold)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-sold/length(gender_sold_temp)}
      }
    }
    
  }
}

meat_income<-data.frame(apply(meat_sold_income, 2, function (x) as.numeric(x)))
meat_income<- data.frame("meat_income"=rowSums(meat_income, na.rm = T))

female_meat_income<-data.frame(apply(Female_meat_sold, 2, function (x) as.numeric(x)))
female_meat_income<- data.frame("female_meat_income"=rowSums(female_meat_income, na.rm = T))
female_youth_meat_income<-data.frame(apply(Female_Youth_meat_sold, 2, function (x) as.numeric(x)))
female_youth_meat_income<- data.frame("female_Youth_meat_income"=rowSums(female_youth_meat_income, na.rm = T))
male_meat_income<-data.frame(apply(Male_meat_sold, 2, function (x) as.numeric(x)))
male_meat_income<- data.frame("male_meat_income"=rowSums(male_meat_income, na.rm = T))
male_youth_meat_income<-data.frame(apply(Male_Youth_meat_sold, 2, function (x) as.numeric(x)))
male_youth_meat_income<- data.frame("male_meat_income"=rowSums(male_youth_meat_income, na.rm = T))


#------------------
#### Milk Calculations ####

#' Empty data frames created for good and bad season of milk
#' Some of these calculations have to deal with the amount of milk collected per animal
#SEARCH_Milk_Amount: The milk collected per day in the good season is combined and averaged with the milk collected in the bad season in order to provide an estimate of the milk collected all year round. Follow the variables for both the good season and the bad season to understand how this value is calculated. Good and bad season will later be combined for a year-long average.
milk_litres_collected_per_day_good_season<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(milk_litres_collected_per_day_good_season)<- unique_livestock
#SEARCH_Milk_Amount: The milk collected per day in the good season is combined and averaged with the milk collected in the bad season in order to provide an estimate of the milk collected all year round. Follow the variables for both the good season and the bad season to understand how this value is calculated. Good and bad season will later be combined for a year-long average.
milk_litres_collected_per_day_bad_season<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(milk_litres_collected_per_day_bad_season)<- unique_livestock
#SEARCH_Milk_Consumed: The milk consumed per day in the good season is combined and averaged with the milk consumed in the bad season in order to provide an estimate of the milk collected all year round. Follow the variables for both the good season and the bad season to understand how this value is calculated. Good and bad season will later be combined for a year-long average.
milk_litres_consumed_per_day_good_season<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(milk_litres_consumed_per_day_good_season)<- unique_livestock
#SEARCH_Milk_Consumed: The milk consumed per day in the good season is combined and averaged with the milk consumed in the bad season in order to provide an estimate of the milk collected all year round. Follow the variables for both the good season and the bad season to understand how this value is calculated. Good and bad season will later be combined for a year-long average.
milk_litres_consumed_per_day_bad_season<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(milk_litres_consumed_per_day_bad_season)<- unique_livestock
#SEARCH_Milk_Sold:The milk sold per day in the good season is combined and averaged with the milk sold in the bad season in order to provide an estimate of the milk collected all year round. Follow the variables for both the good season and the bad season to understand how this value is calculated. Good and bad season will later be combined for a year-long average.
milk_litres_sold_per_day_good_season<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(milk_litres_sold_per_day_good_season)<- unique_livestock
#SEARCH_Milk_Sold:The milk sold per day in the good season is combined and averaged with the milk sold in the bad season in order to provide an estimate of the milk collected all year round. Follow the variables for both the good season and the bad season to understand how this value is calculated. Good and bad season will later be combined for a year-long average.
milk_litres_sold_per_day_bad_season<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(milk_litres_sold_per_day_bad_season)<- unique_livestock
#SEARCH_Milk_Income
milk_income_per_day<- data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(milk_income_per_day)<- unique_livestock

Male_Youth_milk_sold<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(Male_Youth_milk_sold)<- unique_livestock
Male_Youth_milk_consumed<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(Male_Youth_milk_consumed)<- unique_livestock
Male_milk_sold<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(Male_milk_sold)<- unique_livestock
Male_milk_consumed<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(Male_milk_consumed)<- unique_livestock
Female_Youth_milk_sold<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(Female_Youth_milk_sold)<- unique_livestock
Female_Youth_milk_consumed<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(Female_Youth_milk_consumed)<- unique_livestock
Female_milk_sold<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(Female_milk_sold)<- unique_livestock
Female_milk_consumed<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(Female_milk_consumed)<- unique_livestock


for (i in 1:length_temp)
{
  
  
  units_column<-dat_all[,colnames(dat_all)==paste0("milk_units_",i)]
  units_other_column<-dat_all[,colnames(dat_all)==paste0("milk_amount_units_other_",i)]
  consumed_prop_column<- dat_all[,colnames(dat_all)==paste0("milk_consumed_amount_",i)]
  consumed_prop_column<- tolower(gsub("NA", NA,consumed_prop_column))
  sold_prop_column<- dat_all[,colnames(dat_all)==paste0("milk_sell_amount_",i)]
  sold_prop_column<- tolower(gsub("NA", NA,sold_prop_column))
  sold_units_column<-dat_all[,colnames(dat_all)==paste0("milk_sold_price_timeunits_",i)]
  who_control_sold<-dat_all[,colnames(dat_all)==paste0("milk_who_sells_",i)]
  who_control_sold<- tolower(gsub("NA", NA,who_control_sold))
  who_control_consume<-dat_all[,colnames(dat_all)==paste0("milk_who_control_eating_",i)]
  who_control_consume<- tolower(gsub("NA", NA,who_control_consume))
  
  use_column<- dat_all[,colnames(dat_all)==paste0("milk_use_",i)]
  use_column<-tolower(gsub("NA",NA,use_column))
  use_column<-gsub("c(","",use_column, fixed=T)
  use_column<-gsub("[[:punct:]]","",use_column)
  
  for(j in 1:nrow(dat_all))
  {
    
    if(consumed_prop_column[j] %in% colnames(prop_switch)==TRUE)
    {consumed_prop_column[j]<- prop_switch[1,colnames(prop_switch)==consumed_prop_column[j]]}
    
    if(sold_prop_column[j] %in% colnames(prop_switch)==TRUE)
    {sold_prop_column[j]<- prop_switch[1,colnames(prop_switch)==sold_prop_column[j]]}
    
    use_temp<- as.character(use_column[j])
    use_temp<- unlist(strsplit(use_temp, " "))
    
    if(length(use_temp)==1)
    {
      if(!is.na(use_temp))
      {
        if (use_temp=="use")
        {
          consumed_prop_column[j]<-1
        }
        
        if (use_temp=="sell")
        {
          sold_prop_column[j]<-1
        }
        
      
      }
      
    }
    
    
    if(sold_units_column[j] %in% colnames(milk_sold_price_units)==TRUE)
    {sold_units_column[j]<- milk_sold_price_units[1,colnames(milk_sold_price_units)==sold_units_column[j]]}
    
    gender_sold_temp<-unlist(strsplit(as.character(who_control_sold[j]), " "))
    # for (gend in 1:length(gender_sold_temp))
    # {
    #   if(gender_sold_temp[gend] %in% colnames(Male_female_control_switch)==TRUE)
    #   {gender_sold_temp[gend]<- as.character(Male_female_control_switch[1,colnames(Male_female_control_switch)==gender_sold_temp[gend]])}
    # }
    
    
    gender_consume_temp<-unlist(strsplit(as.character(who_control_consume[j]), " "))
    # for (gend in 1:length(gender_consume_temp))
    # {
    #   if(gender_consume_temp[gend] %in% colnames(Male_female_control_switch)==TRUE)
    #   {gender_consume_temp[gend]<- as.character(Male_female_control_switch[1,colnames(Male_female_control_switch)==gender_consume_temp[gend]])}
    # }
    
    # if(who_control_sold[j] %in% colnames(Male_female_control_switch)==TRUE)
    # {who_control_sold[j]<- as.character(Male_female_control_switch[1,colnames(Male_female_control_switch)==who_control_sold[j]])}
    # 
    # if(who_control_consume[j] %in% colnames(Male_female_control_switch)==TRUE)
    # {who_control_consume[j]<- as.character(Male_female_control_switch[1,colnames(Male_female_control_switch)==who_control_consume[j]])}
    # 
   
      if(units_column[j] %in% colnames(milk_units_column)==TRUE)
      {units_column[j]<- milk_units_column[1,colnames(milk_units_column)==units_column[j]]}
      
    
    if(length(grep("animal",units_column[j]))>0)
    {
      units_column[j]<- switch(as.character(units_column[j]),
                               "l/animal/day"=as.numeric(as.character(dat_all[j,colnames(dat_all)==paste0("milk_number_animals_milked_",i)])),
                               "l_animal_day"=as.numeric(as.character(dat_all[j,colnames(dat_all)==paste0("milk_number_animals_milked_",i)])),
                               "0.3l/animal/day"=0.3*as.numeric(as.character(dat_all[j,colnames(dat_all)==paste0("milk_number_animals_milked_",i)])),
                               "cups/animal/day"=0.5*as.numeric(as.character(dat_all[j,colnames(dat_all)==paste0("milk_number_animals_milked_",i)])),
                               "per animal per week"=(1/7)*as.numeric(as.character(dat_all[j,colnames(dat_all)==paste0("milk_number_animals_milked_",i)])),
                               "litre par animal par an"=(1/365)*as.numeric(as.character(dat_all[j,colnames(dat_all)==paste0("milk_number_animals_milked_",i)])))
      
      # "l/animal/day"=as.numeric(as.character(livestock_number[j, colnames(livestock_number)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]])),
      # "l_animal_day"=as.numeric(as.character(livestock_number[j, colnames(livestock_number)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]])),
      # "0.3l/animal/day"=0.3*as.numeric(as.character(livestock_number[j, colnames(livestock_number)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]])),
      # "cups/animal/day"=0.5*as.numeric(as.character(livestock_number[j, colnames(livestock_number)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]])),
      # "per animal per week"=(1/7)*as.numeric(as.character(livestock_number[j, colnames(livestock_number)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]])),
      # "litre par animal par an"=(1/365)*as.numeric(as.character(livestock_number[j, colnames(livestock_number)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]))
      # 
      
    }
    
    if(length(grep("animal",units_other_column[j]))==0)
    {
      if(units_other_column[j] %in% colnames(milk_units_column)==TRUE)
      {units_other_column[j]<- milk_units_column[1,colnames(milk_units_column)==units_other_column[j]]}
      
    }
    if(length(grep("animal",units_other_column[j]))>0)
    {
      units_other_column[j]<-  switch(as.character(units_other_column[j]),
                                      "l/animal/day"=as.numeric(as.character(dat_all[j,colnames(dat_all)==paste0("milk_number_animals_milked_",i)])),
                                      "l_animal_day"=as.numeric(as.character(dat_all[j,colnames(dat_all)==paste0("milk_number_animals_milked_",i)])),
                                      "0.3l/animal/day"=0.3*as.numeric(as.character(dat_all[j,colnames(dat_all)==paste0("milk_number_animals_milked_",i)])),
                                      "cups/animal/day"=0.5*as.numeric(as.character(dat_all[j,colnames(dat_all)==paste0("milk_number_animals_milked_",i)])),
                                      "per animal per week"=(1/7)*as.numeric(as.character(dat_all[j,colnames(dat_all)==paste0("milk_number_animals_milked_",i)])),
                                      "litre par animal par an"=(1/365)*as.numeric(as.character(dat_all[j,colnames(dat_all)==paste0("milk_number_animals_milked_",i)])))
      
      # "l/animal/day"=as.numeric(as.character(livestock_number[j, colnames(livestock_number)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]])),
      # "l_animal_day"=as.numeric(as.character(livestock_number[j, colnames(livestock_number)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]])),
      # "0.3l/animal/day"=0.3*as.numeric(as.character(livestock_number[j, colnames(livestock_number)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]])),
      # "cups/animal/day"=0.5*as.numeric(as.character(livestock_number[j, colnames(livestock_number)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]])),
      # "per animal per week"=(1/7)*as.numeric(as.character(livestock_number[j, colnames(livestock_number)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]])),
      # "litre par animal par an"=(1/365)*as.numeric(as.character(livestock_number[j, colnames(livestock_number)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]))
      # 
      
      
      
      
    }
    
    
    
    milk_amount_good_season_temp<-   as.numeric(dat_all[j,colnames(dat_all)==paste0("milk_amount_good_season_",i)]) 
    milk_amount_bad_season_temp<-   as.numeric(dat_all[j,colnames(dat_all)==paste0("milk_amount_bad_season_",i)]) 
    milk_sold_income_temp<-  as.numeric(dat_all[j,colnames(dat_all)==paste0("milk_sold_income_",i)]) 
    milk_amount_unit_temp<- as.numeric(units_column[j])
    milk_amount_unit_other_temp<- as.numeric(units_other_column[j])
    sold_price_units_temp<- as.numeric(sold_units_column[j])
    
    if (!is.na(milk_amount_unit_temp) && milk_amount_unit_temp!="other")
    {
      milk_litres_collected_per_day_good_season[j,colnames(milk_litres_collected_per_day_good_season)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<- milk_amount_unit_temp*as.numeric(milk_amount_good_season_temp)
      milk_litres_collected_per_day_bad_season[j,colnames(milk_litres_collected_per_day_bad_season)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<- milk_amount_unit_temp*as.numeric(milk_amount_bad_season_temp)
      
    }
    if (!is.na(milk_amount_unit_other_temp))
    {
      milk_litres_collected_per_day_good_season[j,colnames(milk_litres_collected_per_day_good_season)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<- milk_amount_unit_other_temp*as.numeric(milk_amount_good_season_temp)
      milk_litres_collected_per_day_bad_season[j,colnames(milk_litres_collected_per_day_bad_season)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<- milk_amount_unit_other_temp*as.numeric(milk_amount_bad_season_temp)
    }
    
    if (!is.na(sold_price_units_temp))
    {
      milk_income_per_day[j,colnames(milk_income_per_day)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<- milk_sold_income_temp*sold_price_units_temp
    }
    
    if (dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)] %in% unique_livestock==TRUE)
    {
      milk_litres_collected_per_day_good_season[j,colnames(milk_litres_collected_per_day_good_season)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-milk_litres_collected_per_day_good_season[j,colnames(milk_litres_collected_per_day_good_season)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]
      milk_litres_collected_per_day_bad_season[j,colnames(milk_litres_collected_per_day_bad_season)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-milk_litres_collected_per_day_bad_season[j,colnames(milk_litres_collected_per_day_bad_season)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]
      milk_litres_consumed_per_day_good_season[j,colnames(milk_litres_consumed_per_day_good_season)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-as.numeric(consumed_prop_column[j])*milk_litres_collected_per_day_good_season[j,colnames(milk_litres_collected_per_day_good_season)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]
      milk_litres_consumed_per_day_bad_season[j,colnames(milk_litres_consumed_per_day_bad_season)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-as.numeric(consumed_prop_column[j])*milk_litres_collected_per_day_bad_season[j,colnames(milk_litres_collected_per_day_bad_season)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]
      milk_litres_sold_per_day_good_season[j,colnames(milk_litres_sold_per_day_good_season)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-as.numeric(sold_prop_column[j])*milk_litres_collected_per_day_good_season[j,colnames(milk_litres_collected_per_day_good_season)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]*sold_price_units_temp
      milk_litres_sold_per_day_bad_season[j,colnames(milk_litres_sold_per_day_bad_season)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-as.numeric(sold_prop_column[j])*milk_litres_collected_per_day_bad_season[j,colnames(milk_litres_collected_per_day_bad_season)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]*sold_price_units_temp
      milk_income_per_day[j,colnames(milk_litres_sold_per_day_bad_season)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<- milk_income_per_day[j,colnames(milk_income_per_day)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]*as.numeric(sold_price_units_temp)
      
      
        gender_sold_temp<-unlist(strsplit(as.character(who_control_sold[j]), " "))
        gender_consume_temp<- unlist(strsplit(as.character(who_control_consume[j]), " "))
      if("female_youth" %in% gender_consume_temp==TRUE)
      {Female_Youth_milk_consumed[j,colnames(Female_Youth_milk_consumed)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-1/length(gender_consume_temp)}
      if("female_adult" %in% gender_consume_temp==TRUE | "female_adult" %in% gender_consume_temp==TRUE)
      {Female_milk_consumed[j,colnames(Female_milk_consumed)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-1/length(gender_consume_temp)}
      if("male_youth" %in% gender_consume_temp==TRUE)
      {Male_Youth_milk_consumed[j,colnames(Male_Youth_milk_consumed)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-1/length(gender_consume_temp)}
      if("male_adult" %in% gender_consume_temp==TRUE | "male_adult" %in% gender_consume_temp==TRUE)
      {Male_milk_consumed[j,colnames(Male_milk_consumed)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-1/length(gender_consume_temp)}
      if("female_youth" %in% gender_sold_temp==TRUE)
      {Female_Youth_milk_sold[j,colnames(Female_Youth_milk_sold)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-1/length(gender_sold_temp)}
      if("female_adult" %in%gender_sold_temp==TRUE | "female_adult" %in%gender_sold_temp==TRUE)
      {Female_milk_sold[j,colnames(Female_milk_sold)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-1/length(gender_sold_temp)}
      if("male_youth" %in% gender_sold_temp==TRUE)
      {Male_Youth_milk_sold[j,colnames(Male_Youth_milk_sold)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-1/length(gender_sold_temp)}
      if("male_adult" %in%gender_sold_temp==TRUE | "male_adult" %in%gender_sold_temp==TRUE)
      {Male_milk_sold[j,colnames(Male_milk_sold)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-1/length(gender_sold_temp)}
      
      
    }
    
  }
}

# Using milk collected in good and bad seasons to calculate average collection throughout the year

#SEARCH_Milk_Amount: Good and bad season variables combined here to produce the year long average.
average_milk_collected_per_day<-data.frame(matrix(NA, nrow=nrow(dat_all), ncol=length(colnames(milk_litres_collected_per_day_bad_season))))
colnames(average_milk_collected_per_day)<- colnames(milk_litres_collected_per_day_bad_season)
#SEARCH_Milk_Consumed
average_milk_consumed_per_day<-data.frame(matrix(NA, nrow=nrow(dat_all), ncol=length(colnames(milk_litres_collected_per_day_bad_season))))
colnames(average_milk_consumed_per_day)<- colnames(milk_litres_collected_per_day_bad_season)
#SEARCH_Milk_Sold
average_milk_sold_per_day<-data.frame(matrix(NA, nrow=nrow(dat_all), ncol=length(colnames(milk_litres_collected_per_day_bad_season))))
colnames(average_milk_sold_per_day)<- colnames(milk_litres_collected_per_day_bad_season)

for(i in 1:length(colnames(average_milk_consumed_per_day)))
{
  average_milk_collected_per_day[,i]<- 0.5*(milk_litres_collected_per_day_bad_season[,i]+milk_litres_collected_per_day_good_season[,i])
  average_milk_consumed_per_day[,i]<- 0.5*(milk_litres_consumed_per_day_bad_season[,i]+milk_litres_consumed_per_day_good_season[,i])
  average_milk_sold_per_day[,i]<- 0.5*(milk_litres_sold_per_day_bad_season[,i]+milk_litres_sold_per_day_good_season[,i])
  
  for (j in 1:nrow(average_milk_consumed_per_day))
  {
    Female_Youth_milk_consumed[j,i]<-Female_Youth_milk_consumed[j,i]*average_milk_consumed_per_day[j,i]
    Female_milk_consumed[j,i]<- Female_milk_consumed[j,i]*average_milk_consumed_per_day[j,i]
    Male_Youth_milk_consumed[j,i]<-Male_Youth_milk_consumed[j,i]*average_milk_consumed_per_day[j,i]
    Male_milk_consumed[j,i]<-Male_milk_consumed[j,i]*average_milk_consumed_per_day[j,i]
    
    Female_Youth_milk_sold[j,i]<-Female_Youth_milk_sold[j,i]*milk_income_per_day[j,i]
    Female_milk_sold[j,i]<-Female_milk_sold[j,i]*milk_income_per_day[j,i]
    Male_Youth_milk_sold[j,i]<-Male_Youth_milk_sold[j,i]*milk_income_per_day[j,i]
    Male_milk_sold[j,i]<- Male_milk_sold[j,i]*milk_income_per_day[j,i]
    
  }
} 



milk_income<-rowSums(milk_income_per_day, na.rm=TRUE)
milk_income<-365*milk_income
milk_income<-data.frame("milk_income"=milk_income)

female_milk_income<-rowSums(Female_milk_sold, na.rm=TRUE)
female_milk_income<-365*female_milk_income
female_milk_income<-data.frame("female_milk_income"=female_milk_income)
female_youth_milk_income<-rowSums(Female_Youth_milk_sold, na.rm=TRUE)
female_youth_milk_income<-365*female_youth_milk_income
female_youth_milk_income<-data.frame("female_Youth_milk_income"=female_youth_milk_income)
male_milk_income<-rowSums(Male_milk_sold, na.rm=TRUE)
male_milk_income<-365*male_milk_income
male_milk_income<-data.frame("male_milk_income"=male_milk_income)
male_youth_milk_income<-rowSums(Male_Youth_milk_sold, na.rm=TRUE)
male_youth_milk_income<-365*male_youth_milk_income
male_youth_milk_income<-data.frame("male_youth_milk_income"=male_youth_milk_income)

# female_milk_income<-data.frame(apply(Female_milk_sold, 2, function (x) as.numeric(x)))
# female_milk_income<- data.frame("female_milk_income"=rowSums(female_milk_income, na.rm = T))
# female_youth_milk_income<-data.frame(apply(Female_Youth_milk_sold, 2, function (x) as.numeric(x)))
# female_youth_milk_income<- data.frame("female_Youth_milk_income"=rowSums(female_youth_milk_income, na.rm = T))
# male_milk_income<-data.frame(apply(Male_milk_sold, 2, function (x) as.numeric(x)))
# male_milk_income<- data.frame("male_milk_income"=rowSums(male_milk_income, na.rm = T))
# male_youth_milk_income<-data.frame(apply(Male_Youth_milk_sold, 2, function (x) as.numeric(x)))
# male_youth_milk_income<- data.frame("male_milk_income"=rowSums(male_youth_milk_income, na.rm = T))
# 
#------------------
#### Eggs Calculations ####
#SEARCH_Eggs_Collected: follw the variable created below to see the full calculation
eggs_collected_per_day<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(eggs_collected_per_day)<- unique_livestock
#SEARCH_Eggs_Consumed: follw the variable created below to see the full calculation
eggs_consumed_per_day<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(eggs_consumed_per_day)<- unique_livestock
#SEARCH_Eggs_Sold: follw the variable created below to see the full calculation
eggs_sold_per_day<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(eggs_sold_per_day)<- unique_livestock
#SEARCH_Eggs_Income: follw the variable created below to see the full calculation
eggs_sold_per_day_income<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(eggs_sold_per_day_income)<- unique_livestock

Male_Youth_eggs_sold<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(Male_Youth_eggs_sold)<- unique_livestock
Male_Youth_eggs_consumed<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(Male_Youth_eggs_consumed)<- unique_livestock
Male_eggs_sold<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(Male_eggs_sold)<- unique_livestock
Male_eggs_consumed<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(Male_eggs_consumed)<- unique_livestock
Female_Youth_eggs_sold<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(Female_Youth_eggs_sold)<- unique_livestock
Female_Youth_eggs_consumed<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(Female_Youth_eggs_consumed)<- unique_livestock
Female_eggs_sold<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(Female_eggs_sold)<- unique_livestock
Female_eggs_consumed<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(Female_eggs_consumed)<- unique_livestock


eggs_collected_per_day_temp<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(eggs_collected_per_day_temp)<- unique_livestock
eggs_sold_per_day_income_temp<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(eggs_sold_per_day_income_temp)<- unique_livestock
for (i in 1:length_temp)
{
  units_column<-as.character(dat_all[,colnames(dat_all)==paste0("eggs_units_",i)])
  units_other_column<-as.character(dat_all[,colnames(dat_all)==paste0("eggs_amount_units_other_",i)])
  consumed_prop_column<- dat_all[,colnames(dat_all)==paste0("eggs_consumed_amount_",i)]
  consumed_prop_column<- tolower(gsub("NA", NA,consumed_prop_column))
  sold_prop_column<- dat_all[,colnames(dat_all)==paste0("eggs_sell_amount_",i)]
  sold_prop_column<- tolower(gsub("NA", NA,sold_prop_column))
  sold_units_column<-dat_all[,colnames(dat_all)==paste0("eggs_sold_price_timeunits_",i)]
  sold_units_other_column<-dat_all[,colnames(dat_all)==paste0("eggs_sold_price_timeunits_other_",i)]
  who_control_sold<-dat_all[,colnames(dat_all)==paste0("eggs_who_sells_",i)]
  who_control_sold<- tolower(gsub("NA", NA,who_control_sold))
  who_control_consume<-dat_all[,colnames(dat_all)==paste0("eggs_who_control_eating_",i)]
  who_control_consume<- tolower(gsub("NA", NA,who_control_consume))
  
  use_column<- dat_all[,colnames(dat_all)==paste0("eggs_use_",i)]
  use_column<-tolower(gsub("NA",NA,use_column))
  use_column<-gsub("c(","",use_column, fixed=T)
  use_column<-gsub("[[:punct:]]","",use_column)
  
  
  
  for(j in 1:nrow(dat_all))
  {
    
    if(consumed_prop_column[j] %in% colnames(prop_switch)==TRUE)
    {consumed_prop_column[j]<- prop_switch[1,colnames(prop_switch)==consumed_prop_column[j]]}
    
    if(sold_prop_column[j] %in% colnames(prop_switch)==TRUE)
    {sold_prop_column[j]<- prop_switch[1,colnames(prop_switch)==sold_prop_column[j]]}
    
    use_temp<- as.character(use_column[j])
    use_temp<- unlist(strsplit(use_temp, " "))
    
    if(length(use_temp)==1)
    {
      if(!is.na(use_temp))
      {
        if (use_temp=="use")
        {
          consumed_prop_column[j]<-1
        }
        
        if (use_temp=="sell")
        {
          sold_prop_column[j]<-1
        }
        
       
      }
      
    }
    
    
    gender_sold_temp<-unlist(strsplit(as.character(who_control_sold[j]), " "))
    # for (gend in 1:length(gender_sold_temp))
    # {
    #   if(gender_sold_temp[gend] %in% colnames(Male_female_control_switch)==TRUE)
    #   {gender_sold_temp[gend]<- as.character(Male_female_control_switch[1,colnames(Male_female_control_switch)==gender_sold_temp[gend]])}
    # }
    
    
    gender_consume_temp<-unlist(strsplit(as.character(who_control_consume[j]), " "))
    # for (gend in 1:length(gender_consume_temp))
    # {
    #   if(gender_consume_temp[gend] %in% colnames(Male_female_control_switch)==TRUE)
    #   {gender_consume_temp[gend]<- as.character(Male_female_control_switch[1,colnames(Male_female_control_switch)==gender_consume_temp[gend]])}
    # }
    
    # if(who_control_sold[j] %in% colnames(Male_female_control_switch)==TRUE)
    # {who_control_sold[j]<- as.character(Male_female_control_switch[1,colnames(Male_female_control_switch)==who_control_sold[j]])}
    # 
    # if(who_control_consume[j] %in% colnames(Male_female_control_switch)==TRUE)
    # {who_control_consume[j]<- as.character(Male_female_control_switch[1,colnames(Male_female_control_switch)==who_control_consume[j]])}
    # 
    if(sold_units_column[j] %in% colnames(eggs_sold_price_unit)==TRUE)
    {sold_units_column[j]<- eggs_sold_price_unit[1,colnames(eggs_sold_price_unit)==sold_units_column[j]]}
    
    if(sold_units_other_column[j] %in% colnames(eggs_sold_price_unit)==TRUE)
    {sold_units_other_column[j]<- eggs_sold_price_unit[1,colnames(eggs_sold_price_unit)==sold_units_other_column[j]]}
    
    
    ##### ####  
    
    if(length(grep("animal",units_column[j]))==0)
    {
      if(units_column[j] %in% colnames(eggs_units_column)==TRUE)
      {units_column[j]<- eggs_units_column[1,colnames(eggs_units_column)==units_column[j]]}
      
    }
    if(length(grep("animal",units_column[j]))>0 && length(livestock_number[j, colnames(livestock_number)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]])>0)
    {
      units_column[j]<-  switch(as.character(units_column[j]),
                                "pieces/animal/day"=as.numeric(as.character(livestock_number[j, colnames(livestock_number)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]])),
                                "per animal per month"=as.numeric(as.character(livestock_number[j, colnames(livestock_number)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]))/28)
      
    }
    
    if(length(grep("animal",units_other_column[j]))==0 )
    {
      if(units_other_column[j] %in% colnames(eggs_units_column)==TRUE)
      {units_other_column[j]<- eggs_units_column[1,colnames(eggs_units_column)==units_other_column[j]]}
      
    }
    
    if(length(grep("animal",units_other_column[j]))>0 && length(livestock_number[j, colnames(livestock_number)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]])>0)
    {
      units_other_column[j]<-  switch(as.character(units_other_column[j]),
                                      "pieces/animal/day"=as.numeric(as.character(livestock_number[j, colnames(livestock_number)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]])),
                                      "per animal per month"=as.numeric(as.character(livestock_number[j, colnames(livestock_number)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]))/28)
      
    }
    
    
    
    eggs_amount_temp<-as.numeric(as.character(dat_all[j,colnames(dat_all)==paste0("eggs_amount_good_",i)])) 
    eggs_sold_income_temp<-as.numeric(as.character(dat_all[j,colnames(dat_all)==paste0("eggs_sold_income_",i)]))
    eggs_amount_unit_temp<- as.numeric(units_column[j])
    eggs_amount_unit_other_temp<- as.numeric(units_other_column[j])
    
    
    eggs_sold_price_unit_temp<-as.numeric(sold_units_column[j])
    eggs_sold_price_unit_temp_other<-as.numeric(sold_units_other_column[j])
    
    if (!is.na(eggs_amount_unit_temp)==TRUE && eggs_amount_unit_temp!="other")
    {
      eggs_collected_per_day_temp[j,i]<- as.numeric(eggs_amount_unit_temp)*as.numeric(eggs_amount_temp)
    }
    if (!is.na(eggs_amount_unit_other_temp)==TRUE)
    {
      eggs_collected_per_day_temp[j,i]<- as.numeric(eggs_amount_unit_other_temp)*as.numeric(eggs_amount_temp)
    }
    
    if (!is.na(eggs_sold_price_unit_temp)==TRUE && eggs_sold_price_unit_temp!="other")
    {
      eggs_sold_per_day_income_temp[j,i]<- eggs_sold_price_unit_temp*as.numeric(eggs_sold_income_temp)
    }
    if (!is.na(eggs_sold_price_unit_temp_other)==TRUE)
    {
      eggs_sold_per_day_income_temp[j,i]<- eggs_sold_price_unit_temp_other*as.numeric(eggs_sold_income_temp)
    }
    
    
    
    if (dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)] %in% unique_livestock==TRUE)
    {
      eggs_collected_per_day[j,colnames(eggs_collected_per_day)==as.character(dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)])]<-eggs_collected_per_day_temp[j,i]
      eggs_consumed_per_day[j,colnames(eggs_consumed_per_day)==as.character(dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)])]<-as.numeric(consumed_prop_column[j])*eggs_collected_per_day_temp[j,i]
      eggs_sold_per_day[j,colnames(eggs_sold_per_day)==as.character(dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)])]<-as.numeric(sold_prop_column[j])*eggs_collected_per_day_temp[j,i]
      eggs_sold_per_day_income[j,colnames(eggs_sold_per_day_income)==as.character(dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)])]<- eggs_sold_per_day_income_temp[j,i]
      
      #  gender_sold_temp<-unlist(strsplit(as.character(who_control_sold[j]), " "))
      # gender_consume_temp<- unlist(strsplit(as.character(who_control_consume[j]), " "))
      sold<- eggs_sold_per_day_income[j,colnames(eggs_sold_per_day_income)==as.character(dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)])]
      eggs_consumed_per_day[j,colnames(eggs_consumed_per_day)==as.character(dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)])]<-as.numeric(consumed_prop_column[j])*eggs_collected_per_day_temp[j,i]
      if("female_youth" %in% gender_consume_temp==TRUE)
      {Female_Youth_eggs_consumed[j,colnames(Female_Youth_eggs_consumed)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-consumed/length(gender_consume_temp)}
      if("female_adult" %in% gender_consume_temp==TRUE | "female_head" %in% gender_consume_temp==TRUE)
      {Female_eggs_consumed[j,colnames(Female_eggs_consumed)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-consumed/length(gender_consume_temp)}
      if("male_youth" %in% gender_consume_temp==TRUE)
      {Male_Youth_eggs_consumed[j,colnames(Male_Youth_eggs_consumed)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-consumed/length(gender_consume_temp)}
      if("male_adult" %in% gender_consume_temp==TRUE | "male_head" %in% gender_consume_temp==TRUE)
      {Male_eggs_consumed[j,colnames(Male_eggs_consumed)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-consumed/length(gender_consume_temp)}
      if("female_youth" %in% gender_sold_temp==TRUE)
      {Female_Youth_eggs_sold[j,colnames(Female_Youth_eggs_sold)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-sold/length(gender_sold_temp)}
      if("female_adult" %in%gender_sold_temp==TRUE | "female_head" %in%gender_sold_temp==TRUE)
      {Female_eggs_sold[j,colnames(Female_eggs_sold)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-sold/length(gender_sold_temp)}
      if("male_youth" %in% gender_sold_temp==TRUE)
      {Male_Youth_eggs_sold[j,colnames(Male_Youth_eggs_sold)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-sold/length(gender_sold_temp)}
      if("male_adult" %in%gender_sold_temp==TRUE | "male_head" %in%gender_sold_temp==TRUE)
      {Male_eggs_sold[j,colnames(Male_eggs_sold)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-sold/length(gender_sold_temp)}
      
    }
  }
}
eggs_income<-data.frame(apply(eggs_sold_per_day_income, 2, function (x) (as.numeric(x))*365))
eggs_income<- data.frame("eggs_income"=rowSums(eggs_income, na.rm = T))

female_eggs_income<-data.frame(apply(Female_eggs_sold, 2, function (x) as.numeric(x)))
female_eggs_income<- data.frame("female_eggs_income"=rowSums(female_eggs_income, na.rm = T))
female_youth_eggs_income<-data.frame(apply(Female_Youth_eggs_sold, 2, function (x) as.numeric(x)))
female_youth_eggs_income<- data.frame("female_Youth_eggs_income"=rowSums(female_youth_eggs_income, na.rm = T))
male_eggs_income<-data.frame(apply(Male_eggs_sold, 2, function (x) as.numeric(x)))
male_eggs_income<- data.frame("male_eggs_income"=rowSums(male_eggs_income, na.rm = T))
male_youth_eggs_income<-data.frame(apply(Male_Youth_eggs_sold, 2, function (x) as.numeric(x)))
male_youth_eggs_income<- data.frame("male_eggs_income"=rowSums(male_youth_eggs_income, na.rm = T))

#------------------
#### Bees Calculations ####
bees_honey_production_per_year<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(bees_honey_production_per_year)<- unique_livestock
bees_honey_consumed_per_year<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(bees_honey_consumed_per_year)<- unique_livestock
bees_honey_sold_per_year<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(bees_honey_sold_per_year)<- unique_livestock
bees_honey_sold_income_per_year<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(bees_honey_sold_income_per_year)<- unique_livestock

bees_honey_production_per_year_temp<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(bees_honey_production_per_year_temp)<- unique_livestock
bees_honey_sold_income_per_year_temp<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(bees_honey_sold_income_per_year_temp)<- unique_livestock

Male_Youth_bees_honey_sold<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(Male_Youth_bees_honey_sold)<- unique_livestock
Male_Youth_bees_honey_consumed<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(Male_Youth_bees_honey_consumed)<- unique_livestock
Male_bees_honey_sold<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(Male_bees_honey_sold)<- unique_livestock
Male_bees_honey_consumed<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(Male_bees_honey_consumed)<- unique_livestock
Female_Youth_bees_honey_sold<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(Female_Youth_bees_honey_sold)<- unique_livestock
Female_Youth_bees_honey_consumed<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(Female_Youth_bees_honey_consumed)<- unique_livestock
Female_bees_honey_sold<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(Female_bees_honey_sold)<- unique_livestock
Female_bees_honey_consumed<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(Female_bees_honey_consumed)<- unique_livestock


for (i in 1:length_temp)
{
  units_column<-as.character(dat_all[,colnames(dat_all)==paste0("bees_honey_production_units_",i)])
  units_other_column<-as.character(dat_all[,colnames(dat_all)==paste0("bees_honey_production_units_other_",i)])
  consumed_prop_column<- dat_all[,colnames(dat_all)==paste0("bees_honey_consumed_amount_",i)]
  consumed_prop_column<- tolower(gsub("NA", NA,consumed_prop_column))

  sold_prop_column<- dat_all[,colnames(dat_all)==paste0("bees_honey_sell_amount_",i)]
  sold_prop_column<- tolower(gsub("NA", NA,sold_prop_column))

  who_control_consume<- dat_all[,colnames(dat_all)==paste0("bees_who_control_eating_",i)]
  who_control_consume<- tolower(gsub("NA", NA,who_control_consume))

  who_control_sold<- dat_all[,colnames(dat_all)==paste0("bees_who_sells_",i)]
  who_control_sold<- tolower(gsub("NA", NA,who_control_sold))

  use_column<- dat_all[,colnames(dat_all)==paste0("bees_honey_use_",i)]
  use_column<-tolower(gsub("NA",NA,use_column))
  use_column<-gsub("c(","",use_column, fixed=T)
  use_column<-gsub("[[:punct:]]","",use_column)
  


  for(j in 1:nrow(dat_all))
  {
    if(consumed_prop_column[j] %in% colnames(prop_switch)==TRUE)
    {consumed_prop_column[j]<- prop_switch[1,colnames(prop_switch)==consumed_prop_column[j]]}

    if(sold_prop_column[j] %in% colnames(prop_switch)==TRUE)
    {sold_prop_column[j]<- prop_switch[1,colnames(prop_switch)==sold_prop_column[j]]}
    
    
    use_temp<- as.character(use_column[j])
    use_temp<- unlist(strsplit(use_temp, " "))
    
    if(length(use_temp)==1)
    {
      if(!is.na(use_temp))
      {
        if (use_temp=="use")
        {
          consumed_prop_column[j]<-1
        }
        
        if (use_temp=="sell")
        {
          sold_prop_column[j]<-1
        }
        
        
      }
      
    }

    gender_sold_temp<-unlist(strsplit(as.character(who_control_sold[j]), " "))
    # for (gend in 1:length(gender_sold_temp))
    # {
    #   if(gender_sold_temp[gend] %in% colnames(Male_female_control_switch)==TRUE)
    #   {gender_sold_temp[gend]<- as.character(Male_female_control_switch[1,colnames(Male_female_control_switch)==gender_sold_temp[gend]])}
    # }


    gender_consume_temp<-unlist(strsplit(as.character(who_control_consume[j]), " "))
    # for (gend in 1:length(gender_consume_temp))
    # {
    #   if(gender_consume_temp[gend] %in% colnames(Male_female_control_switch)==TRUE)
    #   {gender_consume_temp[gend]<- as.character(Male_female_control_switch[1,colnames(Male_female_control_switch)==gender_consume_temp[gend]])}
    # }


#     # if(who_control_sold[j] %in% colnames(Male_female_control_switch)==TRUE)
#     # {who_control_sold[j]<-as.character(Male_female_control_switch[1,colnames(Male_female_control_switch)==who_control_sold[j]])}
#     #
#     # if(who_control_consume[j] %in% colnames(Male_female_control_switch)==TRUE)
#     # {who_control_consume[j]<- as.character(Male_female_control_switch[1,colnames(Male_female_control_switch)==who_control_consume[j]])}
#     #
    if(units_column[j] %in% colnames(bees_honey_units_column)==TRUE)
    {units_column[j]<- bees_honey_units_column[1,colnames(bees_honey_units_column)==units_column[j]]}

    if(units_other_column[j] %in% colnames(bees_honey_units_column)==TRUE)
    {units_other_column[j]<- bees_honey_units_column[1,colnames(bees_honey_units_column)==units_other_column[j]]}




    honey_amount_temp<-as.numeric(dat_all[j,colnames(dat_all)==paste0("bees_honey_production_",i)])
    honey_sold_income_temp<-as.numeric(dat_all[j,colnames(dat_all)==paste0("bees_honey_sold_income_",i)])
    honey_amount_unit_temp<- as.numeric(units_column[j])
    honey_amount_unit_other_temp<- as.numeric(units_other_column[j])



    if (!is.na(honey_amount_unit_temp)==TRUE && honey_amount_unit_temp!="other")
    {
      bees_honey_production_per_year_temp[j,i]<- as.numeric(honey_amount_unit_temp)*as.numeric(honey_amount_temp)
    }
    if (!is.na(honey_amount_unit_other_temp)==TRUE)
    {
      bees_honey_production_per_year_temp[j,i]<- as.numeric(honey_amount_unit_other_temp)*as.numeric(honey_amount_temp)
    }

    bees_honey_sold_income_per_year_temp[j,i]<- honey_sold_income_temp

    if (!is.na(dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]))
    {
    if (dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)] =="bees")
    {
      bees_honey_production_per_year[j,colnames(bees_honey_production_per_year)=="bees"]<-bees_honey_production_per_year_temp[j,i]
      bees_honey_consumed_per_year[j,colnames(bees_honey_consumed_per_year)=="bees"]<-as.numeric(consumed_prop_column[j])*bees_honey_production_per_year_temp[j,i]
      bees_honey_sold_per_year[j,colnames(bees_honey_sold_per_year)=="bees"]<-as.numeric(sold_prop_column[j])*bees_honey_production_per_year_temp[j,i]
      bees_honey_sold_income_per_year[j,colnames(bees_honey_sold_income_per_year)=="bees"]<- bees_honey_sold_income_per_year_temp[j,i]

     # gender_sold_temp<-unlist(strsplit(as.character(who_control_sold[j]), " "))
    #  gender_consume_temp<- unlist(strsplit(as.character(who_control_consume[j]), " "))
      sold<-  bees_honey_sold_income_per_year[j,colnames(bees_honey_sold_income_per_year)=="bees"]
      consumed<- bees_honey_consumed_per_year[j,colnames(bees_honey_consumed_per_year)=="bees"]<-as.numeric(consumed_prop_column[j])*bees_honey_production_per_year_temp[j,i]


      if("female_youth" %in% gender_consume_temp==TRUE)
      {Female_Youth_bees_honey_consumed[j,colnames(Female_Youth_bees_honey_consumed)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-consumed/length(gender_consume_temp)}
      if("female_adult" %in% gender_consume_temp==TRUE | "female_head" %in% gender_consume_temp==TRUE)
      {Female_bees_honey_consumed[j,colnames(Female_bees_honey_consumed)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-consumed/length(gender_consume_temp)}
      if("male_youth" %in% gender_consume_temp==TRUE)
      {Male_Youth_bees_honey_consumed[j,colnames(Male_Youth_bees_honey_consumed)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-consumed/length(gender_consume_temp)}
      if("male_adult" %in% gender_consume_temp==TRUE | "male_head" %in% gender_consume_temp==TRUE)
      {Male_bees_honey_consumed[j,colnames(Male_bees_honey_consumed)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-consumed/length(gender_consume_temp)}
      if("female_youth" %in% gender_sold_temp==TRUE)
      {Female_Youth_bees_honey_sold[j,colnames(Female_Youth_bees_honey_sold)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-as.numeric(sold)/length(gender_sold_temp)}
      if("female_adult" %in%gender_sold_temp==TRUE | "female_head" %in%gender_sold_temp==TRUE)
      {Female_bees_honey_sold[j,colnames(Female_bees_honey_sold)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-as.numeric(sold)/length(gender_sold_temp)}
      if("male_youth" %in% gender_sold_temp==TRUE)
      {Male_Youth_bees_honey_sold[j,colnames(Male_Youth_bees_honey_sold)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-as.numeric(sold)/length(gender_sold_temp)}
      if("male_adult" %in%gender_sold_temp==TRUE | "male_head" %in%gender_sold_temp==TRUE)
      {Male_bees_honey_sold[j,colnames(Male_bees_honey_sold)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-as.numeric(sold)/length(gender_sold_temp)}

    }
    }


  }
}


bees_income<-data.frame(apply(bees_honey_sold_income_per_year, 2, function (x) as.numeric(x)))
bees_income<- data.frame("bees_income"=rowSums(bees_income, na.rm = T))

female_bees_honey_income<-data.frame(apply(Female_bees_honey_sold, 2, function (x) as.numeric(x)))
female_bees_honey_income<- data.frame("female_bees_honey_income"=rowSums(female_bees_honey_income, na.rm = T))
female_youth_bees_honey_income<-data.frame(apply(Female_Youth_bees_honey_sold, 2, function (x) as.numeric(x)))
female_youth_bees_honey_income<- data.frame("female_Youth_bees_honey_income"=rowSums(female_youth_bees_honey_income, na.rm = T))
male_bees_honey_income<-data.frame(apply(Male_bees_honey_sold, 2, function (x) as.numeric(x)))
male_bees_honey_income<- data.frame("male_bees_honey_income"=rowSums(male_bees_honey_income, na.rm = T))
male_youth_bees_honey_income<-data.frame(apply(Male_Youth_bees_honey_sold, 2, function (x) as.numeric(x)))
male_youth_bees_honey_income<- data.frame("male_bees_honey_income"=rowSums(male_youth_bees_honey_income, na.rm = T))

#------------------
#### Cheese Calculations ####
cheese_amount_per_day<- data.frame(matrix(NA,ncol=1, nrow = length(dat_all$cheese_amount)))
cheese_consumed_per_day<- data.frame(matrix(NA,ncol=1, nrow = length(dat_all$cheese_amount)))
cheese_sold_per_day<- data.frame(matrix(NA,ncol=1, nrow = length(dat_all$cheese_amount)))
cheese_income_per_day<- data.frame(matrix(NA,ncol=1, nrow = length(dat_all$cheese_amount)))




units_column<-as.character(dat_all[,colnames(dat_all)=="cheese_units"])
units_other_column<-as.character(dat_all[,colnames(dat_all)=="cheese_amount_time_units_other"])
time_units_column<- as.character(dat_all[,colnames(dat_all)=="cheese_time_units"])
time_units_column_other<-as.character(dat_all[,colnames(dat_all)=="cheese_amount_time_units_other"])
price_time_units_column<-as.character(dat_all[,colnames(dat_all)=="cheese_sold_price_timeunits"])
consumed_prop_column<- dat_all[,colnames(dat_all)=="cheese_consumed_amount"]
sold_prop_column<- dat_all[,colnames(dat_all)=="cheese_sell_amount"]

use_column<- dat_all[,colnames(dat_all)==paste0("cheese_use")]
use_column<-tolower(gsub("NA",NA,use_column))
use_column<-gsub("c(","",use_column, fixed=T)
use_column<-gsub("[[:punct:]]","",use_column)

for (i in 1:length(cheese_amount_per_day[,1]))
{
  if (length(consumed_prop_column)>0)
  {
    if(consumed_prop_column[i] %in% colnames(prop_switch)==TRUE)
    {consumed_prop_column[i]<- prop_switch[1,colnames(prop_switch)==consumed_prop_column[i]]}
  }
  if (length(sold_prop_column)>0)
  {
    if(sold_prop_column[i] %in% colnames(prop_switch)==TRUE)
    {sold_prop_column[i]<- prop_switch[1,colnames(prop_switch)==sold_prop_column[i]]}
  }
  use_temp<- as.character(use_column[i])
  use_temp<- unlist(strsplit(use_temp, " "))
  
  if(length(use_temp)==1)
  {
    if(!is.na(use_temp))
    {
      if (use_temp=="use")
      {
        consumed_prop_column[i]<-1
      }
      
      if (use_temp=="sell")
      {
        sold_prop_column[i]<-1
      }
    
    }
    
  }
  

  if(units_column[i] %in% colnames(cheese_units)==TRUE)
  {units_column[i]<- cheese_units[1,colnames(cheese_units)==units_column[i]]}

  if(units_other_column[i] %in% colnames(cheese_units)==TRUE)
  {units_other_column[i]<- cheese_units[1,colnames(cheese_units)==units_other_column[i]]}

  if(time_units_column[i] %in% colnames(cheese_time_units)==TRUE)
  {time_units_column[i]<- cheese_time_units[1,colnames(cheese_time_units)==time_units_column[i]]}

  if(time_units_column_other[i] %in% colnames(cheese_time_units)==TRUE)
  {time_units_column_other[i]<- cheese_time_units[1,colnames(cheese_time_units)==time_units_column_other[i]]}

  if(price_time_units_column[i] %in% colnames(cheese_price_time_units)==TRUE)
  {price_time_units_column[i]<- cheese_price_time_units[1,colnames(cheese_price_time_units)==price_time_units_column[i]]}






  if (!is.na(as.numeric(units_column[i])) && units_column[i]!= "other")
  {
    if (!is.na(as.numeric(time_units_column[i])) && time_units_column[i]!= "other")
    {
      cheese_amount_per_day[i,1]<- as.numeric(dat_all[i, colnames(dat_all)=="cheese_amount"])*as.numeric(units_column[i])*as.numeric(time_units_column[i])
    }
    if (time_units_column[i]== "other" && !is.na(as.numeric(time_units_column_other[i])))
    {
      cheese_amount_per_day[i,1]<- as.numeric(dat_all[i, colnames(dat_all)=="cheese_amount"])*as.numeric(units_column[i])*as.numeric(time_units_column_other[i])
    }
  }
  if (!is.na(as.numeric(units_other_column[i])))
  {
    if (!is.na(as.numeric(time_units_column[i])) && time_units_column[i]!= "other")
    {
      cheese_amount_per_day[i,1]<- as.numeric(dat_all[i, colnames(dat_all)=="cheese_amount"])*as.numeric(units_other_column[i])*as.numeric(time_units_column[i])
    }
    if (time_units_column[i]== "other" && !is.na(as.numeric(time_units_column_other[i])))
    {
      cheese_amount_per_day[i,1]<- as.numeric(dat_all[i, colnames(dat_all)=="cheese_amount"])*as.numeric(units_other_column[i])*as.numeric(time_units_column_other[i])
    }
  }
  
  if (length(consumed_prop_column)>0)
  {
  cheese_consumed_per_day[i,1]<- cheese_amount_per_day[i,1]*as.numeric(consumed_prop_column[i])
  }
  if (length(sold_prop_column)>0)
  {
  cheese_sold_per_day[i,1]<- cheese_amount_per_day[i,1]*as.numeric(sold_prop_column[i])
  }
  if (!is.na(as.numeric(price_time_units_column[i])) && price_time_units_column[i]!= "other")
  {
    cheese_income_per_day[i,1]<- as.numeric(price_time_units_column[i])*as.numeric(dat_all[i, colnames(dat_all)=="cheese_sold_income"])
  }
}
cheese_income<-data.frame(apply(cheese_income_per_day, 2, function (x) as.numeric(x)*365))
colnames(cheese_income)<- c("cheese_income")

# #------------------
# #### Butter Calculations ####
butter_amount_per_day<- data.frame(matrix(NA,ncol=1, nrow = length(dat_all$butter_amount)))
butter_consumed_per_day<- data.frame(matrix(NA,ncol=1, nrow = length(dat_all$butter_amount)))
butter_sold_per_day<- data.frame(matrix(NA,ncol=1, nrow = length(dat_all$butter_amount)))
butter_income_per_day<- data.frame(matrix(NA,ncol=1, nrow = length(dat_all$butter_amount)))

units_column<-as.character(dat_all[,colnames(dat_all)=="butter_units"])
units_other_column<-as.character(dat_all[,colnames(dat_all)=="butter_amount_time_units_other"])
time_units_column<- as.character(dat_all[,colnames(dat_all)=="butter_time_units"])
time_units_column_other<-as.character(dat_all[,colnames(dat_all)=="butter_amount_time_units_other"])
price_time_units_column<-as.character(dat_all[,colnames(dat_all)=="butter_sold_price_timeunits"])
consumed_prop_column<- dat_all[,colnames(dat_all)=="butter_consumed_amount"]
sold_prop_column<- dat_all[,colnames(dat_all)=="butter_sell_amount"]
use_column<- dat_all[,colnames(dat_all)==paste0("butter_use")]
use_column<-tolower(gsub("NA",NA,use_column))
use_column<-gsub("c(","",use_column, fixed=T)
use_column<-gsub("[[:punct:]]","",use_column)




for (i in 1:length(butter_amount_per_day[,1]))
{
  if (length(consumed_prop_column)>0)
  {
  if(consumed_prop_column[i] %in% colnames(prop_switch)==TRUE)
  {consumed_prop_column[i]<- prop_switch[1,colnames(prop_switch)==consumed_prop_column[i]]}
  }
  if (length(sold_prop_column)>0)
  {
  if(sold_prop_column[i] %in% colnames(prop_switch)==TRUE)
  {sold_prop_column[i]<- prop_switch[1,colnames(prop_switch)==sold_prop_column[i]]}
  }

  use_temp<- as.character(use_column[i])
  use_temp<- unlist(strsplit(use_temp, " "))
  
  if(length(use_temp)==1)
  {
    if(!is.na(use_temp))
    {
      if (use_temp=="use")
      {
        consumed_prop_column[i]<-1
      }
      
      if (use_temp=="sell")
      {
        sold_prop_column[i]<-1
      }
      
    }
    
  }
  
  
  if(units_column[i] %in% colnames(butter_units_column)==TRUE)
  {units_column[i]<- butter_units_column[1,colnames(butter_units_column)==units_column[i]]}

  if(units_other_column[i] %in% colnames(butter_units_column)==TRUE)
  {units_other_column[i]<- butter_units_column[1,colnames(butter_units_column)==units_other_column[i]]}

  if(time_units_column[i] %in% colnames(butter_time_units)==TRUE)
  {time_units_column[i]<- butter_time_units[1,colnames(butter_time_units)==time_units_column[i]]}

  if(time_units_column_other[i] %in% colnames(butter_time_units)==TRUE)
  {time_units_column_other[i]<- butter_time_units[1,colnames(butter_time_units)==time_units_column_other[i]]}

  if(price_time_units_column[i] %in% colnames(butter_price_time_units)==TRUE)
  {price_time_units_column[i]<- butter_price_time_units[1,colnames(butter_price_time_units)==price_time_units_column[i]]}


  if (!is.na(as.numeric(units_column[i])) && units_column[i]!= "other")
  {
    if (!is.na(as.numeric(time_units_column[i])) && time_units_column[i]!= "other")
    {
      butter_amount_per_day[i,1]<- as.numeric(dat_all[i, colnames(dat_all)=="butter_amount"])*as.numeric(units_column[i])*as.numeric(time_units_column[i])
    }
    if (time_units_column[i]== "other" && !is.na(as.numeric(time_units_column_other[i])))
    {
      butter_amount_per_day[i,1]<- as.numeric(dat_all[i, colnames(dat_all)=="butter_amount"])*as.numeric(units_column[i])*as.numeric(time_units_column_other[i])
    }
  }
  if (!is.na(as.numeric(units_other_column[i])))
  {
    if (!is.na(as.numeric(time_units_column[i])) && time_units_column[i]!= "other")
    {
      butter_amount_per_day[i,1]<- as.numeric(dat_all[i, colnames(dat_all)=="butter_amount"])*as.numeric(units_other_column[i])*as.numeric(time_units_column[i])
    }
    if (time_units_column[i]== "other" && !is.na(as.numeric(time_units_column_other[i])))
    {
      butter_amount_per_day[i,1]<- as.numeric(dat_all[i, colnames(dat_all)=="butter_amount"])*as.numeric(units_other_column[i])*as.numeric(time_units_column_other[i])
    }
  }
  if (length(consumed_prop_column)>0)
  {
  butter_consumed_per_day[i,1]<- butter_amount_per_day[i,1]*as.numeric(consumed_prop_column[i])
  }
  if (length(sold_prop_column)>0)
  {
  butter_sold_per_day[i,1]<- butter_amount_per_day[i,1]*as.numeric(sold_prop_column[i])
  }
  if (!is.na(as.numeric(price_time_units_column[i])) && price_time_units_column[i]!= "other")
  {
    butter_income_per_day[i,1]<- as.numeric(price_time_units_column[i])*as.numeric(dat_all[i, colnames(dat_all)=="butter_sold_income"])
  }
}
butter_income<-data.frame(apply(butter_income_per_day, 2, function (x) as.numeric(x)*365))
colnames(butter_income)<- c("butter_income")
# #------------------