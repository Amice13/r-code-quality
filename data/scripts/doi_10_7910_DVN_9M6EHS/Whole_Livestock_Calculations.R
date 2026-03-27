# Whole Livestock Calculation


#' #' A script calculating the numbers and income relating to whole livestock
#' 
#'   #### Investigating the different livestock farmed ####
#'   SEARCH_LivestockHoldings

LivestockHoldings<- 
  data.frame(coeff_cattle*as.numeric(dat_all$livestock_heads_cattle),
             coeff_sheep*as.numeric(dat_all$livestock_heads_sheep),
             coeff_goats*as.numeric(dat_all$livestock_heads_goats),
             coeff_pigs*as.numeric(dat_all$livestock_heads_pigs),
             coeff_chicken_poultry*as.numeric(dat_all$livestock_heads_chicken),
             coeff_other_poultry*as.numeric(dat_all$livestock_heads_otherpoultry),
             coeff_rabbits*as.numeric(dat_all$livestock_heads_rabbits),
             coeff_donkeys_horses*as.numeric(dat_all$livestock_heads_donkeys),
             coeff_fish*as.numeric(dat_all$livestock_heads_fish),
             coeff_bees*as.numeric(dat_all$livestock_heads_beehives))

LivestockHoldings<- rowSums(LivestockHoldings, na.rm=TRUE)







unique_livestock<-c()
for (livestock in 1:length(grep("livestock_name_", colnames(dat_all))))
{
  for (j in 1:nrow(dat_all))
  {
    if (!is.na(dat_all[j,colnames(dat_all)==paste0("livestock_name_",livestock)]))
    {
    if (dat_all[j,colnames(dat_all)==paste0("livestock_name_",livestock)]=="other1")
    {
      dat_all[j,colnames(dat_all)==paste0("livestock_name_",livestock)]<- dat_all$livestock_other1[j] 
    }
    if (dat_all[j,colnames(dat_all)==paste0("livestock_name_",livestock)]=="other2")
    {
      dat_all[j,colnames(dat_all)==paste0("livestock_name_",livestock)]<- dat_all$livestock_other2[j] 
    }
    if (dat_all[j,colnames(dat_all)==paste0("livestock_name_",livestock)]=="other3")
    {
      dat_all[j,colnames(dat_all)==paste0("livestock_name_",livestock)]<- dat_all$livestock_other3[j] 
    }
    }
  }
  temp<- dat_all[,colnames(dat_all)==paste0("livestock_name_",livestock)]
  unique_livestock<-c(unique_livestock, unique(as.character(temp)))
}    
unique_livestock<- unique(unique_livestock)
unique_livestock<- unique_livestock[!is.na(unique_livestock)]

if( "other" %in% unique_livestock==TRUE)
{
  unique_livestock<- unique_livestock[-grep("other", unique_livestock)]
}
if( "NA" %in% unique_livestock==TRUE)
{
  unique_livestock<- unique_livestock[-grep("NA", unique_livestock)]
}
unique_livestock<- unique_livestock[unique_livestock!=""]

unique_livestock_temp<- gsub("donkeys or mules", "donkeys", unique_livestock)
livestock_number<-data.frame(matrix(NA,ncol=length(unique_livestock_temp), nrow = nrow(dat_all)))
colnames(livestock_number)<- unique_livestock_temp
livestock_heads<- data.frame(dat_all[,grep("livestock_heads_", colnames(dat_all))])
colnames(livestock_heads)<- gsub("livestock_heads_", "",colnames(livestock_heads))
for (livestock in unique_livestock_temp)
{
  if(livestock %in% unique_livestock_temp==TRUE)
  {
    livestock_number[,colnames(livestock_number)==livestock]<- livestock_heads[,colnames(livestock_heads)==livestock]
  }
}
colnames(livestock_number)<- gsub("donkeys","donkeys or mules",colnames(livestock_number))
#------------------
#### Whole livestock sales ####

# Creating empty data frames for livestock information

livestock_sold_income<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(livestock_sold_income)<- unique_livestock
livestock_sold_number<- data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(livestock_sold_number)<- unique_livestock

Male_Youth_livestock_sold<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(Male_Youth_livestock_sold)<- unique_livestock
Male_livestock_sold<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(Male_livestock_sold)<- unique_livestock
Female_Youth_livestock_sold<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(Female_Youth_livestock_sold)<- unique_livestock
Female_livestock_sold<-data.frame(matrix(NA,ncol=length(unique_livestock), nrow = nrow(dat_all)))
colnames(Female_livestock_sold)<- unique_livestock



length_temp<- length(grep("livestock_name_", colnames(dat_all)))
for (i in 1:length_temp)
{
  livestock_sold_number_temp<- dat_all[,colnames(dat_all)==paste0("livestock_sold_",i)]
  livestock_sold_income_temp<- dat_all[,colnames(dat_all)==paste0("livestock_sale_income_",i)]
  who_control_sold<- tolower(dat_all[,colnames(dat_all)==paste0("livestock_who_sells_",i)])
  who_control_sold<- tolower(gsub("NA", NA,who_control_sold))
  
  
  
  
  
  for(j in 1:nrow(dat_all))
  {
    gender_sold_temp<-strsplit(as.character(who_control_sold[j]), " ")
   
        if(gender_sold_temp %in% colnames(Male_female_control_switch)==TRUE)
        {gender_sold_temp<- as.character(Male_female_control_switch[1,colnames(Male_female_control_switch)==gender_sold_temp])}
        
    
    
    
    if (dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)] %in% unique_livestock==TRUE)
    {
      livestock_sold_number[j,colnames(livestock_sold_number)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-as.numeric(livestock_sold_number_temp[j])
      livestock_sold_income[j,colnames(livestock_sold_income)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-as.numeric(livestock_sold_income_temp[j])
      
      # gender_sold_temp<-unlist(strsplit(as.character(who_control_sold[j]), " "))
      sold<- livestock_sold_income[j,colnames(livestock_sold_income)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-as.numeric(livestock_sold_income_temp[j])
      if("female_youth" %in% gender_sold_temp==TRUE)
      {Female_Youth_livestock_sold[j,colnames(Female_Youth_livestock_sold)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-as.numeric(sold)/length(gender_sold_temp)}
      if("female_adult" %in%gender_sold_temp==TRUE | "female_head" %in%gender_sold_temp==TRUE)
      {Female_livestock_sold[j,colnames(Female_livestock_sold)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-as.numeric(sold)/length(gender_sold_temp)}
      if("male_youth" %in% gender_sold_temp==TRUE)
      {Male_Youth_livestock_sold[j,colnames(Male_Youth_livestock_sold)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-as.numeric(sold)/length(gender_sold_temp)}
      if("male_adult" %in%gender_sold_temp==TRUE | "male_head" %in%gender_sold_temp==TRUE)
      {Male_livestock_sold[j,colnames(Male_livestock_sold)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<-as.numeric(sold)/length(gender_sold_temp)}
    }
  }
}

livestock_income<-data.frame(apply(livestock_sold_income, 2, function (x) as.numeric(x)))
livestock_income<- data.frame("livestock_income"=rowSums(livestock_income, na.rm = T))


female_livestock_income<-data.frame(apply(Female_livestock_sold, 2, function (x) as.numeric(x)))
female_livestock_income<- data.frame("female_livestock_income"=rowSums(female_livestock_income, na.rm = T))
female_youth_livestock_income<-data.frame(apply(Female_Youth_livestock_sold, 2, function (x) as.numeric(x)))
female_youth_livestock_income<- data.frame("female_Youth_livestock_income"=rowSums(female_youth_livestock_income, na.rm = T))
male_livestock_income<-data.frame(apply(Male_livestock_sold, 2, function (x) as.numeric(x)))
male_livestock_income<- data.frame("male_livestock_income"=rowSums(male_livestock_income, na.rm = T))
male_youth_livestock_income<-data.frame(apply(Male_Youth_livestock_sold, 2, function (x) as.numeric(x)))
male_youth_livestock_income<- data.frame("male_livestock_income"=rowSums(male_youth_livestock_income, na.rm = T))

