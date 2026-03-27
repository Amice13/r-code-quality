### Obesity calculation Spain###

set.seed(1234)

# function preserving sum

round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y/up 
  
}

## obesity prevalence
setwd('your_path/COVID Analysis/Data/Epi_Data/')

data_folder<-'your_path/COVID Analysis/Data/Epi_Data/'

obesity_file<-c(paste(data_folder,'data_repository/obprev_covid_clean_June242022_Normal prevalence.csv',sep=''),
                paste(data_folder,'data_repository//obprev_covid_clean_June242022_Scenario 2_ half prevalence.csv',sep=''),
                paste(data_folder,'data_repository//obprev_covid_clean_June242022_Scenario 1_ flat prevalence.csv',sep='')
)

type_obesity_prev<-c('normal','half','flat')

saving_folders<-c('Normal_overweight_prev',
                  'Half_overweight_prev',
                  'Flat_overweight_prev') 

for (i in 1:3){
  

## IHME data 

IHME_data_all_countries<-read.csv( paste(data_folder,'data_repository/IHME_data//COVID19_country_data.csv', sep=''))
  
IHME_data_all_countries_20_22<-read.csv( paste(data_folder,'data_repository/IHME_data/new_data_IHME/COVID19_country_data.csv', sep=''))
  
IHME_data_country<-IHME_data_all_countries[IHME_data_all_countries$location_name=='Spain',]

IHME_data_country_20_22<-IHME_data_all_countries_20_22[IHME_data_all_countries_20_22$location_name=='Spain',]

IHME_data_country$date

other_year_results<-list()

#New data

BMI_all_country<-read.csv(obesity_file[i])

BMI_all_country<-BMI_all_country[BMI_all_country$Country=='Spain',]


names(BMI_all_country)<-c("country", "year","BMI_25.30","BMI_30.35","BMI_above_35", "Total_prev", 
                          "BMI_25.30","BMI_30.35","BMI_above_35", "Total_prev")  

BMI_attribute<-data.frame(country='Spain', iso3='SPA', sex=c(rep('All', 19),rep('Men', 19), rep('Women', 19)),
                          year=2017:2035)

BMI_value<-rbind((BMI_all_country[,3:6]+BMI_all_country[,7:10])/2,BMI_all_country[,3:6],BMI_all_country[,7:10])

BMI_ratio<-cbind(BMI_attribute,BMI_value)


# 2020

odd_ratio<-read.csv(paste(data_folder,'data_repository/Odd_ratio_obesity.csv', sep=''))


pop_saudi<-   46771996


list_2020<-list()

obs_1<-BMI_ratio[4,5]

obs_2<-BMI_ratio[4,6]

obs_3<-	BMI_ratio[4,7]

no_obs<- 1- obs_1 - obs_2 - obs_3

cases_reported<-sum(IHME_data_country$confirmed_infections [grep('2020',IHME_data_country$date)], na.rm=T)

deaths<- sum(IHME_data_country$deaths_reported_mean [grep('2020',IHME_data_country$date)], na.rm=T)

tot_infected<- cases_reported/0.30

hospital_2020<-IHME_data_country$admis_mean[grep('2020',IHME_data_country$date)[-c(1:3)]]

cases_2020<- IHME_data_country$confirmed_infections[grep('2020',IHME_data_country$date)][1:length(hospital_2020)]

hospital<- cases_reported*median(hospital_2020/cases_2020,  na.rm=T)

dig_no_obs<-1

dig_obs_1<-odd_ratio[1,2]

dig_obs_2<-odd_ratio[2,2]

dig_obs_3<-odd_ratio[3,2]


cases_no_obs<-tot_infected * no_obs

cases_obs_1<-tot_infected * obs_1

cases_obs_2<-tot_infected * obs_2

cases_obs_3<-tot_infected * obs_3 


cases_reported

#  dig cases

adj_dig_prob<-c(
  
  dig_no_obs * no_obs ,
  
  dig_obs_1 * obs_1 ,
  
  dig_obs_2 * obs_2 ,
  
  dig_obs_3 * obs_3
  
)

dig_cases_per_obs<-adj_dig_prob/sum(adj_dig_prob)*cases_reported


pop_per_group<-pop_saudi * c(no_obs, obs_1, obs_2,obs_3)

cases_odd<-c(dig_no_obs, dig_obs_1, dig_obs_2, dig_obs_3)

case_rate<- cases_reported/ pop_saudi

odds<-case_rate/(1-case_rate)



odds_for_all_group<- odds*cases_odd

final_prob<-1-(1/(1+odds_for_all_group))

dig_cases_per_obs<-round((final_prob*pop_per_group)/sum(final_prob*pop_per_group)*cases_reported)



list_2020[[length(list_2020)+1]]<-dig_cases_per_obs

# hospitalization 

hospital


hosp_no_obs<-1

hosp_obs_1<-odd_ratio[4,2]

hosp_obs_2<-odd_ratio[5,2]

hosp_obs_3<-odd_ratio[6,2]

hosp_odd<-c(hosp_no_obs, hosp_obs_1, hosp_obs_2, hosp_obs_3)


hospital_rate<-median(hospital_2020/cases_2020,  na.rm=T)#0.1

odds<-hospital_rate/(1-hospital_rate)

odds_for_all_group<- odds*hosp_odd

final_prob<-1-(1/(1+odds_for_all_group))

hosp_cases_per_obs<-round((final_prob*dig_cases_per_obs)/sum(final_prob*dig_cases_per_obs)*hospital)




list_2020[[length(list_2020)+1]]<-hosp_cases_per_obs


ICU_2020<- IHME_data_country$newICU_mean[grep('2020',IHME_data_country$date)][1:length(hospital_2020)]

ICU_fraction_hospital<-median(ICU_2020/hospital_2020,na.rm=T)

ICU<-hospital*ICU_fraction_hospital

ICU_no_obs<-1

ICU_obs_1<-odd_ratio[10,2]

ICU_obs_2<-odd_ratio[11,2]

ICU_obs_3<-odd_ratio[12,2]

ICU_odd<-c(ICU_no_obs, ICU_obs_1, ICU_obs_2, ICU_obs_3)

ICU_rate<-ICU/cases_reported

odds<-ICU_rate/(1-ICU_rate)


odds_for_all_group<- odds*ICU_odd

final_prob<-1-(1/(1+odds_for_all_group))

ICU_cases_per_obs<-round((final_prob*dig_cases_per_obs)/sum(final_prob*dig_cases_per_obs)*ICU)



list_2020[[length(list_2020)+1]]<-ICU_cases_per_obs


# Deaths 

deaths



death_no_obs<-1 

death_obs_1<-odd_ratio[13,2] 

death_obs_2<-odd_ratio[14,2] 

death_obs_3<-odd_ratio[15,2]


death_odd<-c(death_no_obs, death_obs_1, death_obs_2, death_obs_3)

death_rate<-deaths/cases_reported

odds<-death_rate/(1-death_rate)



odds_for_all_group<- odds*death_odd

final_prob<-1-(1/(1+odds_for_all_group))



death_cases_per_obs<-round((final_prob*dig_cases_per_obs)/sum(final_prob*dig_cases_per_obs)*deaths)





list_2020[[length(list_2020)+1]]<-death_cases_per_obs


res_2020_db<-do.call(rbind,list_2020)

BMI_ratio_year<-BMI_ratio[BMI_ratio$year==2020,][-1,]

men_ratio<-(BMI_ratio_year[1,5:7]*0.51/(BMI_ratio_year[1,5:7]*0.51+BMI_ratio_year[2,5:7]*0.49))

men_calc<-t(t(res_2020_db)*c(0.51,as.numeric(men_ratio)))

wom_calc<-res_2020_db-men_calc

men_calc<-as.data.frame( men_calc)

wom_calc<-as.data.frame(wom_calc)

men_calc$sex<-'Men'

wom_calc$sex<-'Women'

men_calc$value<-c('reported cases','hospitalized','ICU','deaths')

wom_calc$value<-c('reported cases','hospitalized','ICU','deaths')

both_sex_calc<-rbind(men_calc,wom_calc)

both_sex_calc$year<-2020

both_sex_calc$Prev_COVID<-round((cases_reported/0.3)/pop_saudi,5)

other_year_results[[length(other_year_results)+1]]<-both_sex_calc


fraction_people_infected<-t(t(res_2020_db)/c(cases_no_obs, cases_obs_1, cases_obs_2, cases_obs_3))


### 2021 ###

BMI_ratio_year<-BMI_ratio[BMI_ratio$year==2021,] [-1,]

obs_1<-mean(BMI_ratio_year[,5])

obs_2<-mean(BMI_ratio_year[,6])

obs_3<-	mean(BMI_ratio_year[,7])

no_obs<- 1- obs_1 - obs_2 - obs_3

## cases

cases_2021<- sum(IHME_data_country$confirmed_infections[grep('2021',IHME_data_country$date)][1:190], na.rm=T)



cases_reported<- cases_2021

tot_infected<- cases_2021/30*100



tot_cases_obs<- c(cases_no_obs<-tot_infected * no_obs,
                  
                  cases_obs_1<-tot_infected * obs_1,
                  
                  cases_obs_2<-tot_infected * obs_2,
                  
                  cases_obs_3<-tot_infected * obs_3 )


res_calc<-(t(t(fraction_people_infected)*tot_cases_obs))

# Vac effect

third_dose<-0

second_dose<-pop_saudi*0.84-third_dose

first_dose<-(pop_saudi*0.87-pop_saudi*0.84)


tot_dose<-first_dose+ second_dose + third_dose

mean_effect<- (first_dose*0.75+second_dose*0.92+ third_dose*0)/tot_dose

vac_effect<-mean_effect*(tot_dose/pop_saudi)

###

res_calc[2:4,]<-res_calc[2:4,]*(1-vac_effect/2)





men_ratio<-BMI_ratio_year[1,5:7]*0.58/(BMI_ratio_year[1,5:7]*0.58+BMI_ratio_year[2,5:7]*0.42)

men_calc<-t(t(res_calc)*c(0.58,as.numeric(men_ratio)))

wom_calc<-res_calc-men_calc

men_calc<-as.data.frame( men_calc)

wom_calc<-as.data.frame(wom_calc)

men_calc$sex<-'Men'

wom_calc$sex<-'Women'

men_calc$value<-c('reported cases','hospitalized','ICU','deaths')

wom_calc$value<-c('reported cases','hospitalized','ICU','deaths')

both_sex_calc<-rbind(men_calc,wom_calc)

both_sex_calc$year<-2021

both_sex_calc$Prev_COVID<-(cases_2021/0.30)/pop_saudi

other_year_results[[length(other_year_results)+1]]<-both_sex_calc



# Other years ###

years<-rep(2022:2035)

covid_prev<-rep(c(0.005,0.01,0.05,0.10,0.15))

for (i_year in years){
  

  
  pop_saudi<- pop_saudi + pop_saudi*0.08 
  
  # year prev
  for (i_prev in covid_prev){
    
    
    BMI_ratio_year<-BMI_ratio[BMI_ratio$year==i_year,][-1,]
    
    obs_1<-mean(BMI_ratio_year[,5])
    
    obs_2<-mean(BMI_ratio_year[,6])
    
    obs_3<-	mean(BMI_ratio_year[,7])
    
    no_obs<- 1- obs_1 - obs_2 - obs_3
    
    cases_reported<- pop_saudi*i_prev*0.30
    
    tot_infected<- pop_saudi*i_prev
    
 
    
    tot_cases_obs<- c(cases_no_obs<-tot_infected * no_obs,
                      
                      cases_obs_1<-tot_infected * obs_1,
                      
                      cases_obs_2<-tot_infected * obs_2,
                      
                      cases_obs_3<-tot_infected * obs_3 )
    
    
    res_calc<-(t(t(fraction_people_infected)* tot_cases_obs))
    
    res_calc[2:4,]<-res_calc[2:4,]*vac_effect
        
    
   
    
    
    
    men_ratio<-BMI_ratio_year[1,5:7]*0.51/(BMI_ratio_year[1,5:7]*0.51+BMI_ratio_year[2,5:7]*0.49)
    
    men_calc<-t(t(res_calc)*c(0.51,as.numeric(men_ratio)))
    
    wom_calc<-res_calc-men_calc
    
    men_calc<-as.data.frame( men_calc)
    
    wom_calc<-as.data.frame(wom_calc)
    
    men_calc$sex<-'Men'
    
    wom_calc$sex<-'Women'
    
    men_calc$value<-c('reported cases','hospitalized','ICU','deaths')
    
    wom_calc$value<-c('reported cases','hospitalized','ICU','deaths')
    
    both_sex_calc<-rbind(men_calc,wom_calc)
    
    both_sex_calc$year<-i_year
    
    both_sex_calc$Prev_COVID<-i_prev
    
    other_year_results[[length(other_year_results)+1]]<-both_sex_calc
    
  }
  
}


other_years_db<-as.data.frame(do.call(rbind,other_year_results))

names(other_years_db)[1:4]<-c('BMI_low_25','BMI_25.30', 'BMI_30.35', 'BMI_above_35')

other_years_db$country<-'Spain'

other_years_db$iso3<-'SPA'

other_years_db<-other_years_db[,c('country', 'iso3', 
                                  'year', 'Prev_COVID', 
                                  'value', 'sex', 'BMI_low_25',
                                  'BMI_25.30',  'BMI_30.35', 'BMI_above_35')]

other_years_db[,7:10]<-round(other_years_db[,7:10])


write.csv(other_years_db,paste(saving_folders[i],'/SPA_Estimates_2020_2025_IHME_',
                               type_obesity_prev[i],'.csv',sep=''))


other_years_db_deaths<-other_years_db[other_years_db$value=='deaths',]



deaths_age_ratio<-data.frame(age=c('0-9','10-19','20-29','30-39','40-49','50-59', '60-69','70-79','>-80') )


deaths_age_ratio$BMI_low_25 <-c(0.001, 0.001, 0.002, 0.004, 0.013, 0.028, 0.088, 0.279, 0.586)
deaths_age_ratio$BMI_25.30 <-c(0.001, 0.001, 0.002, 0.004, 0.013, 0.028, 0.088, 0.279, 0.586)
deaths_age_ratio$BMI_30.35 <-c(0.001, 0.001, 0.002, 0.004, 0.013, 0.028, 0.088, 0.279, 0.586)

new_ratio<- sum(c(0.001, 0.001, 0.002, 0.004, 0.013, 0.028, 0.088, 0.279, 0.586))/sum(c(0, 0, 0.002, 0.004, 0.013, 0.028, 0.088, 0.279, 0.586))

deaths_age_ratio$BMI_above_35 <-c(0, 0, 0.002, 0.004, 0.013, 0.028, 0.088, 0.279, 0.586)*new_ratio



deaths_age_list<-list()

for (i_row in 1:dim(other_years_db_deaths)[1]){
  
  deaths_row<-other_years_db_deaths[i_row,]
  
  estimated_deaths<-as.numeric(deaths_row[7:10])
  
  deaths_age<-round(t(t(deaths_age_ratio[,-1])*estimated_deaths))
  
  death_age_db<-data.frame(country=unlist(deaths_row[1]), iso3=unlist(deaths_row[2]), year=unlist(deaths_row[3])
                           , Prev_COVID= unlist(deaths_row[4]),  value=unlist(deaths_row[5]), sex=unlist(deaths_row[6]),
                           age_group=deaths_age_ratio[,1] )
  
  death_age_db<-cbind( death_age_db, deaths_age)
  
  deaths_age_list[[length(deaths_age_list)+1]]<-death_age_db
  
}



deaths_age_full<-do.call(rbind,deaths_age_list)



### deaths



### calculate excess

excess_list<-list()

for(i_line in 1:dim(other_years_db)[1] ){
  
  
  year<-other_years_db$year[i_line]
  
  sex<-  other_years_db$sex[i_line]
  
  BMI_year_sex<-BMI_ratio[ BMI_ratio$year==year & BMI_ratio$sex==sex,5:8] 
  
  obs_0<-1-BMI_year_sex[1,4]
  
  pop_per_group<-as.numeric(c(obs_0,as.numeric(BMI_year_sex[1:3]))*pop_saudi)
  
  
  fraction_obs_0<-other_years_db[i_line,7]/pop_per_group[1]
  
  expected_cases<-fraction_obs_0*pop_per_group[2:4]
  
  excess_values<-other_years_db[i_line,8:10]-expected_cases
  
  excess_values[excess_values<0]<-0
  
  excess_values<-sum(excess_values)
  
  excess_list[[length(excess_list)+1]]<- round_preserve_sum(excess_values)
}


db_excess_data<-cbind(other_years_db[,1:6],do.call(rbind,excess_list))

names(db_excess_data)[7]<-'Excesses'

write.csv(db_excess_data, paste(saving_folders[i],'/SPA_Excess_cases_hosp_deaths_2020_2025_IHME_',
                                type_obesity_prev[i],'.csv',sep=''))

# Excess deaths 


excess_list_deaths<-list()

for(i_line in 1:dim(deaths_age_full)[1] ){
  
  
  
  year<-deaths_age_full$year[i_line]
  
  sex<-  as.character(deaths_age_full$sex[i_line])
  
  BMI_year_sex<-BMI_ratio[ BMI_ratio$year==year & BMI_ratio$sex==sex,5:8] 
  
  obs_0<-1-BMI_year_sex[1,4]
  
  fraction_obs_0<-deaths_age_full[i_line,8]/pop_per_group[1]
  
  expected_cases<-fraction_obs_0*pop_per_group[2:4]
  
  excess_values<-deaths_age_full[i_line,9:11]-expected_cases
  
  excess_values[excess_values<0]<-0
  
  excess_values<-sum(excess_values)
  
  
  excess_list_deaths[[length(excess_list_deaths)+1]]<- round_preserve_sum(excess_values)
}


db_excess_deaths_data<-cbind(deaths_age_full[,1:7],do.call(rbind,excess_list_deaths))

names(db_excess_deaths_data)[8]<-'Excess_deaths'




##### Extra: deaths by age/year ####


deaths_age_ratio<-data.frame(age=c('0-9','10-19','20-29','30-39','40-49','50-59', '60-69','70-79','>-80') )

age<-c(seq(5,75,10),80,85,90,95,100)

age_above_85<-c(1287,1038,470,11,13)



age_above_85_fraction<-age_above_85/sum(age_above_85)

deaths_fraction<- c(0.001, 0.001, 0.002, 0.004, 0.013, 0.028, 0.088, 0.279, age_above_85_fraction*0.586)

age_death_fraction_db<-data.frame(age,deaths_fraction)

library(mgcv)

model_age<-lm( deaths_fraction~I(age^4)+I(age^3)+I(age^2)+age ,data=age_death_fraction_db)

model_age<-gam(deaths_fraction~s(age,k=10) ,data=age_death_fraction_db)

predict(model_age, newdata = data.frame(age=seq(0,100,1)))

plot(seq(0,100,1),predict(model_age, newdata = data.frame(age=seq(0,100,1))), ylim=c(0,.35))

points(age_death_fraction_db$age, age_death_fraction_db$deaths_fraction, col=2)

prediction_death_fraction<-predict(model_age, newdata = data.frame(age=seq(0,100,1)))

prediction_death_fraction<-prediction_death_fraction/sum(prediction_death_fraction)

deaths_age_ratio<-data.frame(age=seq(0,100,1))

deaths_age_ratio$BMI_low_25 <-prediction_death_fraction 
deaths_age_ratio$BMI_25.30 <-prediction_death_fraction
deaths_age_ratio$BMI_30.35 <-prediction_death_fraction

prediction_death_fraction_high_obesity<-prediction_death_fraction

prediction_death_fraction_high_obesity[1:21]<-0

new_ratio<- sum(prediction_death_fraction)/sum(prediction_death_fraction_high_obesity)

deaths_age_ratio$BMI_above_35 <-prediction_death_fraction_high_obesity*new_ratio



deaths_age_list<-list()

for (i_row in 1:dim(other_years_db_deaths)[1]){
  
  deaths_row<-other_years_db_deaths[i_row,]
  
  estimated_deaths<-as.numeric(deaths_row[7:10])
  
  deaths_age<-round(t(t(deaths_age_ratio[,-1])*estimated_deaths))
  
  deaths_age[ deaths_age<0]<-0
  
  death_age_db<-data.frame(country=unlist(deaths_row[1]), iso3=unlist(deaths_row[2]), year=unlist(deaths_row[3])
                           , Prev_COVID= unlist(deaths_row[4]),  value=unlist(deaths_row[5]), sex=unlist(deaths_row[6]),
                           age_group=deaths_age_ratio[,1] )
  
  death_age_db<-cbind( death_age_db, deaths_age)
  
  deaths_age_list[[length(deaths_age_list)+1]]<-death_age_db
  
}



deaths_age_full_per_year_0_95<-do.call(rbind,deaths_age_list)



#### excess deaths per year ####

# Excess deaths 

db_excess_deaths_all<-db_excess_data[db_excess_data$value=='deaths',]


excess_list_deaths<-list()

for(i_line in 1:dim(db_excess_deaths_all)[1] ){
  
  
  
  year<-db_excess_deaths_all$year[i_line]
  
  sex<-  as.character(db_excess_deaths_all$sex[i_line])
  
  
  prediction_death_fraction[prediction_death_fraction<0]<-min(prediction_death_fraction[prediction_death_fraction>0])
  
  prediction_death_fraction[1:10]<-min(prediction_death_fraction)
  
  excess_values<-sample(c(1:length(prediction_death_fraction)),db_excess_deaths_all$Excesses[i_line], 
                        prob=prediction_death_fraction, replace=T)
  
  
  
  deaths_count<-table(factor(excess_values, levels=1:length(prediction_death_fraction_high_obesity)))
  
  death_age_db<-data.frame(country=db_excess_deaths_all$country[i_line], iso3=db_excess_deaths_all$iso3[i_line], 
                           year=db_excess_deaths_all$year[i_line]
                           , Prev_COVID= db_excess_deaths_all$Prev_COVID[i_line],  
                           value=db_excess_deaths_all$value[i_line], sex=db_excess_deaths_all$sex[i_line],
                           age_group=0:100, Excess_deaths=as.numeric(deaths_count))
  
  
  

  
  
  excess_list_deaths[[length(excess_list_deaths)+1]]<- death_age_db
}


db_excess_deaths_data_per_year_0_95<-do.call(rbind,excess_list_deaths)

write.csv(db_excess_deaths_data_per_year_0_95, paste(saving_folders[i],'/SPA_Excess_deaths_age_group_per_year_0_95_2020_2025_IHME_',
                                                     type_obesity_prev[i],'.csv',sep=''))


}






