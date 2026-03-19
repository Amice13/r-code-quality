# # Total Incomes
# 
# 
# #### NTFP Incomes ####
# 
# NTFP_Incomes<- data.frame("shea_fruit"=shea_fruit_df$shea_fruit_sold_income_weekly,
#                           "shea_nut"=shea_nut_df$Sold_Income_per_Week,
#                           "shea_butter"=shea_butter_df$Sold_Income_per_Week,
#                           "baobab_fruit"=baobab_fruit_df$Sold_Income_per_Week,
#                           "baobab_leaves"=baobab_leaves_df$Sold_Income_per_Week,
#                           "moringa_leaves"=moringa_leaves_df$Sold_Income_per_Week,
#                           "balanites_fruit"=balanites_fruit_df$Sold_Income_per_Week,
#                           "balanites_leaves"=balanites_leaves_df$Sold_Income_per_Week,
#                           "honey"=honey_df$Sold_Income_per_Week,
#                           "locust_bean"=locust_bean_df$Sold_Income_per_Week,
#                           "gum_arabic"=locust_bean_df$Sold_Income_per_Week,
#                           "tamarind"=tamarind_df$Sold_Income_per_Week,
#                           "sabasenegal"=sabasenegal_df$Sold_Income_per_Week,
#                           "jujube"=jujube_df$Sold_Income_per_Week,
#                           "tallow"=tallow_df$Sold_Income_per_Week,
#                           "cashew"=cashew_df$Sold_Income_per_Week, 
#                           "wild_grape"=wild_grape_df$Sold_Income_per_Week
# )
# 
# NTFP_Incomes<-data.frame(lapply(NTFP_Incomes, function (x) as.numeric(x)))
# NTFP_Income_total<- (1/7)*as.numeric(rowSums(NTFP_Incomes, na.rm = T))
# A script to add all of the incomes together

#### Putting all of the income calculations together ####
#butter_amount_per_day<- data.frame(matrix(NA,ncol=1, nrow = length(dat_all$butter_amount)))
#butter_consumed_per_day<- data.frame(matrix(NA,ncol=1, nrow = length(dat_all$butter_amount)))
#butter_sold_per_day<- data.frame(matrix(NA,ncol=1, nrow = length(dat_all$butter_amount)))
#butter_income_per_day<- data.frame(matrix(NA,ncol=1, nrow = length(dat_all$butter_amount)))

#SEARCH_Whole_Livestock_Sale_Income: Follow the variable created below for the remainder of the calculation
livestock_income<- data.frame("livestock_income"=livestock_income) #variable created in "Whole_Livestock_Calculations.R"
#SEARCH_Milk_Income: Follow the variable created below for the remainder of the calculation
milk_income<- data.frame("milk_income"=milk_income) #variable created in "Livestock_Product_Calculations.R"
#SEARCH_Eggs_Income: Follow the variable created below for the remainder of the calculation
eggs_income<- data.frame("eggs_income"=eggs_income) #variable created in "Livestock_Product_Calculations.R"
#SEARCH_Meat_Income : Follow the variable created below for the remainder of the calculation
meat_income<- data.frame("meat_income"=meat_income) #variable created in "Livestock_Product_Calculations.R"
cheese_income<- data.frame("cheese_income"=cheese_income) #variable created in "Livestock_Product_Calculations.R"
butter_income<- data.frame("butter_income"=butter_income) #variable created in "Livestock_Product_Calculations.R"
#SEARCH_Honey_Income: Follow the variable created below for the remainder of the calculation
bees_income<- data.frame("bees_income"=bees_income) #variable created in "Livestock_Product_Calculations.R"

#SEARCH_total_income_USD_PPP_pHH_Yr
incomes_df<- data.frame("crop_income"= crops_income[,1], #variable created in "Crop_Calculations.R"
                        # "crop_products"= as.numeric.factor(dat_all[,colnames(dat_all)=="crop_product_sold_income"]),
                        "livestock_income"= livestock_income[,1],
                        "eggs"= eggs_income[,1], #variable created in "Whole_Livestock_Calculations.R"
                        "milk"= milk_income[,1], #variable created in "Livestock_Product_Calculations.R"
                        "meat"= meat_income[,1], #variable created in "Livestock_Product_Calculations.R"
                       # "cheese"= cheese_income[,1], #variable created in "Livestock_Product_Calculations.R"
                       # "butter"= butter_income[,1], #variable created in "Livestock_Product_Calculations.R"
                        #     "wool"= as.numeric(dat_all[,colnames(dat_all)=="wool_sold_income"]))
                        "honey"= as.numeric(bees_income[,1])) #variable created in "Livestock_Product_Calculations.R"




#### NTFP gender income dissagregation ####
# Male_Youth_NTFP<- data.frame("shea_fruit"=shea_fruit_df$Sold_Male_Youth,
#                              "shea_nut"=shea_nut_df$Sold_Male_Youth,
#                              "shea_butter"=shea_butter_df$Sold_Male_Youth,
#                              "baobab_leaves"=baobab_leaves_df$Sold_Male_Youth,
#                              "moringa_leaves"=moringa_leaves_df$Sold_Male_Youth,
#                              "balanites_leaves"=balanites_leaves_df$Sold_Male_Youth,
#                              "honey"=honey_df$Sold_Male_Youth,
#                              "locust_bean"=locust_bean_df$Sold_Male_Youth,
#                              "gumarabic"=gum_arabic_df$Sold_Male_Youth,
#                              "sabasenegal"=sabasenegal_df$Sold_Male_Youth,
#                              "jujube"= jujube_df$Sold_Male_Youth,
#                              "tallow"=tallow_df$Sold_Male_Youth,
#                              "cashew"=cashew_df$Sold_Male_Youth,
#                              "wild_grape"=wild_grape_df$Sold_Male_Youth
# )
# Male_Youth_NTFP_total<-data.frame(lapply(Male_Youth_NTFP, function (x) as.numeric(x)))
# Male_Youth_NTFP_total<- rowSums(Male_Youth_NTFP_total/7, na.rm = T)
# 
# 
# 
# Male_Adult_NTFP<- data.frame("shea_fruit"=shea_fruit_df$Sold_Male_Adult,
#                              "shea_nut"=shea_nut_df$Sold_Male_Adult,
#                              "shea_butter"=shea_butter_df$Sold_Male_Adult,
#                              "baobab_leaves"=baobab_leaves_df$Sold_Male_Adult,
#                              "moringa_leaves"=moringa_leaves_df$Sold_Male_Adult,
#                              "balanites_leaves"=balanites_leaves_df$Sold_Male_Adult,
#                              "honey"=honey_df$Sold_Male_Adult,
#                              "locust_bean"=locust_bean_df$Sold_Male_Adult,
#                              "gumarabic"=gum_arabic_df$Sold_Male_Adult,
#                              "sabasenegal"=sabasenegal_df$Sold_Male_Adult,
#                              "jujube"= jujube_df$Sold_Male_Adult,
#                              "tallow"=tallow_df$Sold_Male_Adult,
#                              "cashew"=cashew_df$Sold_Male_Adult,
#                              "wild_grape"=wild_grape_df$Sold_Male_Adult
# )
# Male_Adult_NTFP_total<-data.frame(lapply(Male_Adult_NTFP, function (x) as.numeric(x)))
# Male_Adult_NTFP_total<- rowSums(Male_Adult_NTFP_total/7, na.rm = T)
# 
# 
# Female_Youth_NTFP<- data.frame("shea_fruit"=shea_fruit_df$Sold_Female_Youth,
#                                "shea_nut"=shea_nut_df$Sold_Female_Youth,
#                                "shea_butter"=shea_butter_df$Sold_Female_Youth,
#                                "baobab_leaves"=baobab_leaves_df$Sold_Female_Youth,
#                                "moringa_leaves"=moringa_leaves_df$Sold_Female_Youth,
#                                "balanites_leaves"=balanites_leaves_df$Sold_Female_Youth,
#                                "honey"=honey_df$Sold_Female_Youth,
#                                "locust_bean"=locust_bean_df$Sold_Female_Youth,
#                                "gumarabic"=gum_arabic_df$Sold_Female_Youth,
#                                "sabasenegal"=sabasenegal_df$Sold_Female_Youth,
#                                "jujube"= jujube_df$Sold_Female_Youth,
#                                "tallow"=tallow_df$Sold_Female_Youth,
#                                "cashew"=cashew_df$Sold_Female_Youth,
#                                "wild_grape"=wild_grape_df$Sold_Female_Youth
# )
# 
# Female_Youth_NTFP_total<-data.frame(lapply(Female_Youth_NTFP, function (x) as.numeric(x)))
# Female_Youth_NTFP_total<- rowSums(Female_Youth_NTFP_total/7, na.rm = T)
# 
# Female_Adult_NTFP<- data.frame("shea_fruit"=shea_fruit_df$Sold_Female_Adult,
#                                "shea_nut"=shea_nut_df$Sold_Female_Adult,
#                                "shea_butter"=shea_butter_df$Sold_Female_Adult,
#                                "baobab_leaves"=baobab_leaves_df$Sold_Female_Adult,
#                                "moringa_leaves"=moringa_leaves_df$Sold_Female_Adult,
#                                "balanites_leaves"=balanites_leaves_df$Sold_Female_Adult,
#                                "honey"=honey_df$Sold_Female_Adult,
#                                "locust_bean"=locust_bean_df$Sold_Female_Adult,
#                                "gumarabic"=gum_arabic_df$Sold_Female_Adult,
#                                "sabasenegal"=sabasenegal_df$Sold_Female_Adult,
#                                "jujube"= jujube_df$Sold_Female_Adult,
#                                "tallow"=tallow_df$Sold_Female_Adult,
#                                "cashew"=cashew_df$Sold_Female_Adult,
#                                "wild_grape"=wild_grape_df$Sold_Female_Adult
# )
# Female_Adult_NTFP_total<-data.frame(lapply(Female_Adult_NTFP, function (x) as.numeric(x)))
# Female_Adult_NTFP_total<- rowSums(Female_Adult_NTFP_total/7, na.rm = T)

#-------------------------------------------------------                  




incomes_male_youth_df<- data.frame("crop_income"= male_youth_crops_income,
                                   "livestock_income"= male_youth_livestock_income,
                                   "eggs"= male_youth_eggs_income,
                                   "milk"= male_youth_milk_income,
                                   "meat"= male_youth_meat_income,
                                   "honey"= male_youth_bees_honey_income)
# incomes_male_youth_NTFP_df<- data.frame("crop_income"= male_youth_crops_income,
#                                         "livestock_income"= male_youth_livestock_income,
#                                         "eggs"= male_youth_eggs_income,
#                                         "milk"= male_youth_milk_income,
#                                         "meat"= male_youth_meat_income,
#                                         "NTFP"=Male_Youth_NTFP_total)


incomes_female_youth_df<- data.frame("crop_income"= female_youth_crops_income,
                                     "livestock_income"= female_youth_livestock_income,
                                     "eggs"= female_youth_eggs_income,
                                     "milk"= female_youth_milk_income,
                                     "meat"= female_youth_meat_income,
                                     "honey"= female_youth_bees_honey_income)
# incomes_female_youth_NTFP_df<- data.frame("crop_income"= female_youth_crops_income,
#                                           "livestock_income"= female_youth_livestock_income,
#                                           "eggs"= female_youth_eggs_income,
#                                           "milk"= female_youth_milk_income,
#                                           "meat"= female_youth_meat_income,
#                                           "NTFP"=Female_Youth_NTFP_total,
#   "honey"= female_youth_bees_honey_income)

incomes_female_df<- data.frame("crop_income"= female_crops_income,
                               "livestock_income"= female_livestock_income,
                               "eggs"= female_eggs_income,
                               "milk"= female_milk_income,
                               "meat"= female_meat_income,
                               "honey"= female_bees_honey_income)
# incomes_female_NTFP_df<- data.frame("crop_income"= female_crops_income,
#                                     "livestock_income"= female_livestock_income,
#                                     "eggs"= female_eggs_income,
#                                     "milk"= female_milk_income,
#                                     "meat"= female_meat_income, 
#                                     "NTFP"=Female_Adult_NTFP_total)

incomes_male_df<- data.frame("crop_income"= male_crops_income,
                             "livestock_income"= male_livestock_income,
                             "eggs"= male_eggs_income,
                             "milk"= male_milk_income,
                             "meat"= male_meat_income,
                             "honey"= male_bees_honey_income)
# incomes_male_NTFP_df<- data.frame("crop_income"= male_crops_income,
#                                   "livestock_income"= male_livestock_income,
#                                   "eggs"= male_eggs_income,
#                                   "milk"= male_milk_income,
#                                   "meat"= male_meat_income, 
#                                   "NTFP"=Male_Adult_NTFP_total)



temp<-data.frame(incomes_df$crop_income,
                 #  incomes_df$crop_products,
                 incomes_df$livestock_income,
                 #  incomes_df$wildfoods,
                 incomes_df$eggs,
                 incomes_df$milk,
                 incomes_df$meat,
                 incomes_df$honey
                 #incomes_df$butter
                 )
temp<- apply(temp, 2, as.numeric)
####On_farm_income
on_farm_income<-rowSums(temp, na.rm = T)

female_youth_income<-rowSums(incomes_female_youth_df, na.rm = T)
#female_youth_income_NTFP<-rowSums(incomes_female_youth_NTFP_df, na.rm = T)
female_income<-rowSums(incomes_female_df, na.rm = T)
#female_income_NTFP<-rowSums(incomes_female_NTFP_df, na.rm = T)
male_youth_income<-rowSums(incomes_male_youth_df, na.rm = T)
#male_youth_income_NTFP<-rowSums(incomes_male_youth_NTFP_df, na.rm = T)
male_income<-rowSums(incomes_male_df, na.rm = T)
#male_income_NTFP<-rowSums(incomes_male_NTFP_df, na.rm = T)

####Off_farm_income 
#SEARCH_offfarm_income_USD_PPP_pHH_Yr
off_farm_prop<- data.frame(unlist(sapply(as.character(dat_all$offfarm_income_proportion),
                                         switch,
                                         "all"=coeff_All,
                                         "most"=coeff_most,
                                         "half"=coeff_Half,
                                         "underhalf"=coeff_Underhalf,
                                         "little"=coeff_Little,
                                         "none"=coeff_None,
                                         "na"=NA,
                                         "NA"=NA,
                                         "UnderHalf"=coeff_Underhalf,
                                         "Most"=coeff_most,
                                         "All"=coeff_All,
                                         "Half"=coeff_Half,
                                         "Little"=coeff_Little)))
off_farm_prop[dat_all$offfarm_incomes_any=="N",]<-0 

# firewood_proportion<- data.frame(unlist(sapply(as.character(dat_all$FP_materials_sold_income_prop),
#                                                switch,
#                                                "all"=coeff_All,
#                                                "most"=coeff_most,
#                                                "half"=coeff_Half,
#                                                "underhalf"=coeff_Underhalf,
#                                                "little"=coeff_Little,
#                                                "none"=coeff_None,
#                                                "None"=coeff_None,
#                                                "NA"=NA,
#                                                "UnderHalf"=coeff_Underhalf,
#                                                "Most"=coeff_most,
#                                                "All"=coeff_All,
#                                                "Half"=coeff_Half,
#                                                "Little"=coeff_Little)))

#### normal_income_calculations #####

#Calculating off-farm income based on the proportion stated and from on-farm income
#SEARCH_offfarm_income_USD_PPP_pHH_Yr
off_farm_income<- off_farm_prop*on_farm_income/(1-off_farm_prop)

names_temp<- colnames(incomes_df)
incomes_df<- data.frame(incomes_df, "on_farm_income"=on_farm_income,"off_farm_income"=off_farm_income)
colnames(incomes_df)<- c(names_temp, "on_farm_income","off_farm_income")

#SEARCH_offfarm_income_USD_PPP_pHH_Yr
off_farm_income_for_sum<- data.frame(incomes_df$off_farm_income)
off_farm_income_for_sum[is.na(off_farm_income_for_sum)==TRUE,1]<- 0

#SEARCH_total_income_USD_PPP_pHH_Yr
total_income<- 
  incomes_df$on_farm_income+
  off_farm_income_for_sum

#SEARCH_farm_income_USD_PPP_pHH_Yr
farm_income<- data.frame(incomes_df$on_farm_income)
colnames(farm_income)<- c("farm_income")
off_farm_income<- data.frame("off_farm_income"= off_farm_income_for_sum)
colnames(off_farm_income)<- c("off_farm_income")

#------------------------------------------


