
startwith_plot<-grep("^plot", colnames(dat_all))
end_with_num<-grep("[1-9$]", colnames(dat_all))
plot_number<- dat_all[startwith_plot[startwith_plot%in%end_with_num]]


loop_number<-as.numeric(unique(gsub('.*_', '', colnames(plot_number))))

parcelle_area_hectares<- data.frame(matrix(NA, ncol=length(loop_number), nrow=nrow(dat_all)))
parcelle_use<- data.frame(matrix(NA, ncol=length(loop_number), nrow=nrow(dat_all)))
parcelle_steepness<- data.frame(matrix(NA, ncol=length(loop_number), nrow=nrow(dat_all)))

for (i in loop_number)
{
  
  parcelle_use_temp<- as.character(plot_number[,  colnames(plot_number)==paste0("plot_use_",i)])
  parcelle_use_other_temp<- gsub(", ",",",as.character(plot_number[,  colnames(plot_number)==paste0("plot_use_other_",i)]))
  parcelle_use_other_temp<-gsub(" ", "_", parcelle_use_other_temp)
  parcelle_use_other_temp<-gsub(",", " ", parcelle_use_other_temp)
  
  parcelle_use_temp<-paste0(parcelle_use_temp, " ",parcelle_use_other_temp )
  parcelle_use_temp<-gsub("NA", "", parcelle_use_temp)
  parcelle_use_temp<-trimws(parcelle_use_temp, which = c("both", "left", "right"))
  
  parcelle_use[,i]<- parcelle_use_temp

  parcelle_steepness[,i]<-as.character(plot_number[,  colnames(plot_number)==paste0("plot_slope_",i)])
    
  
  length_temp<-as.numeric(as.character(plot_number[,  colnames(plot_number)==paste0("plot_length_",i)]))
  width_temp<-as.numeric(as.character(plot_number[,colnames(plot_number)==paste0("plot_width_",i)]))
  parcelle_area_hectares[,i]<-length_temp*width_temp*0.0001
  

  
   
    
  
}


colnames(parcelle_area_hectares)<- paste0("Parcelle_Area_",c(1:15))
colnames(parcelle_steepness)<- paste0("Parcelle_Steepness_",c(1:15))
colnames(parcelle_use)<- paste0("Parcelle_Use_",c(1:15))




dat_all$unitland_owned<-rep("hectares", length(dat_all$unitland))
dat_all$landowned<-rowSums(parcelle_area_hectares, na.rm = T)




