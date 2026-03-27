dat_all$offfarm_who_control_revenue_1

who_control_off_farm<-dat_all[,grep('offfarm_who_control', colnames(dat_all))]


female_offfarm_control<- data.frame(matrix(NA, nrow=nrow(dat_all), ncol=length(grep('offfarm_who_control', colnames(dat_all)))))
male_offfarm_control<- data.frame(matrix(NA, nrow=nrow(dat_all), ncol=length(grep('offfarm_who_control', colnames(dat_all)))))
female_youth_offfarm_control<- data.frame(matrix(NA, nrow=nrow(dat_all), ncol=length(grep('offfarm_who_control', colnames(dat_all)))))
male_youth_offfarm_control<- data.frame(matrix(NA, nrow=nrow(dat_all), ncol=length(grep('offfarm_who_control', colnames(dat_all)))))

for (i in 1:length(grep('offfarm_who_control', colnames(dat_all))))
  
{
  who_control_sold<- dat_all[,colnames(dat_all)==paste0("offfarm_who_control_revenue_",i)]
  who_control_sold <-tolower(gsub("NA",NA,who_control_sold))
  
  
  for(j in 1:nrow(dat_all))
  {
    #Switching the variables with the appropriate units and proportion coefficients ###
    
    
    gender_sold_temp<-unlist(strsplit(as.character(who_control_sold[j]), " "))
    
    
    #Changing the gender labels to all be consistent
   
    
    
    
    if("female_youth" %in% gender_sold_temp==TRUE)
    {female_youth_offfarm_control[j,i]<-1/length(gender_sold_temp)}
    if("female_adult" %in%gender_sold_temp==TRUE | "female_head" %in%gender_sold_temp==TRUE)
    {female_offfarm_control[j,i]<-1/length(gender_sold_temp)}
    if("male_youth" %in% gender_sold_temp==TRUE)
    {male_youth_offfarm_control[j,i]<-1/length(gender_sold_temp)}
    if("male_adult" %in%gender_sold_temp==TRUE | "male_head" %in%gender_sold_temp==TRUE)
    {male_offfarm_control[j,i]<-1/length(gender_sold_temp)}
    
  }
  
  
  
}

female_youth_offfarm_control<- rowSums(female_youth_offfarm_control, na.rm = T)
female_offfarm_control<- rowSums(female_offfarm_control, na.rm = T)
male_youth_offfarm_control<- rowSums(male_youth_offfarm_control, na.rm = T)
male_offfarm_control<- rowSums(male_offfarm_control, na.rm = T)


  female_youth_offfarm_control<- female_youth_offfarm_control/(female_youth_offfarm_control+female_offfarm_control+male_youth_offfarm_control+male_offfarm_control)
  female_offfarm_control<- female_offfarm_control/(female_youth_offfarm_control+female_offfarm_control+male_youth_offfarm_control+male_offfarm_control)
  male_youth_offfarm_control<- male_youth_offfarm_control/(female_youth_offfarm_control+female_offfarm_control+male_youth_offfarm_control+male_offfarm_control)
  male_offfarm_control<- male_offfarm_control/(female_youth_offfarm_control+female_offfarm_control+male_youth_offfarm_control+male_offfarm_control)
  



