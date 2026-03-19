

IHME_data<-read.csv('~/Desktop/RTI_data/Obesity_COVID19/IHME_data/reference_hospitalization_all_locs.csv')


country<-c('South Africa', 'Australia', 'Saudi Arabia', 'India','Thailand','Mexico','Spain','Brazil')


match(country,IHME_data$location_name)


IHME_data_obesity_country<-droplevels(IHME_data[which(IHME_data$location_name %in% country),
                                                
                                                c('location_name','date','confirmed_infections','est_infections_mean', 'admis_mean', 
                                                  'newICU_mean','deaths_reported_mean' )])



#plot(IHME_data_obesity_country$admis_mean[12:512]/IHME_data_obesity_country$confirmed_infections[1:501])


names(IHME_data)

boxplot(IHME_data_obesity_country$admis_mean[12:512]/IHME_data_obesity_country$confirmed_infections[1:501], outline=F)

write.csv(IHME_data_obesity_country, '~/Desktop/RTI_data/Obesity_COVID19/IHME_data/COVID19_country_data.csv',row.names = F)

