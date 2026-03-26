
#######################################################################
###        Examine variation in strength across time                ###



datafile<-read.csv("Gridded_dataset_April_2020.csv",header=T)

datafile$X<-datafile$Long
datafile$Y<-datafile$Lat
datafile$SteppeMax<-datafile$maxSteppe
datafile$SteppeMin<-datafile$minSteppe





#######################################################################


#create empty data frames to store results
sep_time_series_ALL<-data.frame()      #as separate predictors
joint_time_series_FE_ALL=data.frame()  #as predictors in same model 
joint_time_series_ALL=data.frame()     #as predictors in same model but no distance from First Empire



#analyses run on 10 sub-samples

for ( l in 1:10){
  print ("rep")
  print (l)
  
  NUM=700  #number of cells in sub-samples
  
  output<-matrix(ncol=22, nrow=NUM)
  
  datafile3=datafile[sample(nrow(datafile), NUM), ]  #create random sample
  datafile3$match<-c(1:NUM)#create column to match
  

  #create extra columns for storing new imperial density time series variables
  #each new column is the sum of imperial density for 10 time slices
  for (i in 6:27){
    output[1:NUM,(i-5)]=rowSums (datafile3[i:(i+9)], na.rm = FALSE, dims = 1)
  }
  MAdata<-as.data.frame(output)
  datafile3<-merge(datafile3,MAdata,by.x="match", by.y="row.names")
  
  write.table(datafile3, file=paste("GLS_datafile_time_series_",toString(l),".txt",sep=""), sep="\t", row.names = FALSE)
  
  #create lists to store time series output
  
  ##as single predictors
  
  steppe_list=c()
  CAL_list=c()
  AgriTime_list=c()
  FirstEmp_list=c()
  
  
  ##as predictors in same model
  steppe_list2=c()
  CAL_list2=c()
  AgriTime_list2=c()
  FirstEmp_list2=c()
  interaction_list2=c()
  Elev_list2=c()
  
  
  ##as predictors in same model with no "distance from First Empire" included in the models
  
  steppe_list3=c()
  CAL_list3=c()
  AgriTime_list3=c()
  interaction_list3=c()
  Elev_list3=c()
  
  rep_list=c() #this is used for the output to keep track of which time period is being analysed 
  
  #step through the 22 time slices (1500BCE - 600BCE, 1400BCE - 500BCE etc.)
  for ( i in 1:22 ){
    print ("time slice")
    print (i)
    
    #reduce file to only cells with agri at that time
    datafile100<-datafile3
    datafile100$agriBest<-datafile100$agriBest*-1
    datafile100<-datafile100[which(datafile100$agriBest>(2600-(i*100))),]
    
    allYield<-scale(datafile100$allYield)
    elevStd<-scale((log10(datafile100$elevStd+1))*-1)
    maxSteppe_distance<-scale(datafile100$maxSteppe_distance*-1)
    FirstEmp_distance<-scale(datafile100$FirstEmp_distance*-1)
    X<-datafile100$X
    Y<-datafile100$Y
    
    emp_number<-i+53   #find right variable for imperial density
    emp<-scale(datafile100[[emp_number]])
    agri<-scale(datafile100$agriBest)
    print(length(agri))

    ################################
    # run models as separate predictors, and store the coeffcients (parameter estimates)
    
    St=gls(emp~maxSteppe_distance, method="ML")
    St<-update(St,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
    steppe_list=append(steppe_list,St$coefficients[[2]])
    
    Ca=gls(emp~allYield, method="ML")
    Ca<-update(Ca,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
    CAL_list=append(CAL_list,Ca$coefficients[[2]])
    
    Ag=gls(emp~agri, method="ML")
    Ag<-update(Ag,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
    AgriTime_list=append(AgriTime_list,Ag$coefficients[[2]])
    
    Fe=gls(emp~FirstEmp_distance, method="ML")
    Fe<-update(Fe,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
    FirstEmp_list=append(FirstEmp_list,Fe$coefficients[[2]])
    
    ################################
    #all predictors in same model with FE 
    FULL=gls(emp~maxSteppe_distance*agri+allYield+elevStd+FirstEmp_distance, method="ML")
    FULL<-update(FULL,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
    
    #store coeffcients (parameter estimates)
    steppe_list2=append(steppe_list2,FULL$coefficients[[2]])
    CAL_list2=append(CAL_list2,FULL$coefficients[[4]])
    AgriTime_list2=append(AgriTime_list2,FULL$coefficients[[3]])
    FirstEmp_list2=append(FirstEmp_list2,FULL$coefficients[[6]])
    interaction_list2=append(interaction_list2,FULL$coefficients[[7]])
    Elev_list2=append(Elev_list2,FULL$coefficients[[5]])
    
    #################################
    #all predcitors in same model without FE
    FULL2=gls(emp~maxSteppe_distance*agri+allYield+elevStd, method="ML")
    FULL2<-update(FULL2,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
    
    steppe_list3=append(steppe_list3,FULL2$coefficients[[2]])
    CAL_list3=append(CAL_list3,FULL2$coefficients[[4]])
    AgriTime_list3=append(AgriTime_list3,FULL2$coefficients[[3]])  
    interaction_list3=append(interaction_list3,FULL2$coefficients[[6]])
    Elev_list3=append(Elev_list3,FULL2$coefficients[[5]])
    
    
    rep_list<-append(rep_list,l)  
  }
  
  
  #write the output to file
  sep_time_series<-data.frame(steppe_list, CAL_list, AgriTime_list, FirstEmp_list,1:22,rep_list)
  sep_time_series_ALL <- rbind(sep_time_series_ALL, sep_time_series)
  
  joint_time_series_FE<-data.frame(steppe_list2, CAL_list2, AgriTime_list2, FirstEmp_list2,Elev_list2,interaction_list2,1:22,rep_list)
  joint_time_series_FE_ALL <- rbind(joint_time_series_FE_ALL, joint_time_series_FE)
  
  joint_time_series<-data.frame(steppe_list3, CAL_list3, AgriTime_list3,Elev_list3,interaction_list3,1:22,rep_list)
  joint_time_series_ALL <- rbind(joint_time_series_ALL, joint_time_series)
  
  write.table(sep_time_series_ALL, file=paste("sep_time_series_ALL_OLS.txt",sep=""), sep="\t", row.names = FALSE)
  write.table(joint_time_series_FE_ALL, file=paste("joint_time_series_FE_ALL_GLS.txt",sep=""), sep="\t", row.names = FALSE)
  write.table(joint_time_series_ALL, file=paste("joint_time_series_ALL_GLS_with_interaction.txt",sep=""), sep="\t", row.names = FALSE)
  
  
  
}



