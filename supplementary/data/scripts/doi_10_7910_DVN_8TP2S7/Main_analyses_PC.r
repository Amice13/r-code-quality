

#####################################################################
###                      MAIN ANALYSES                            ###

#read in datafile
datafile<-read.csv("Gridded_dataset_April_2020.csv",header=T)

#create variable names that are called in analyses below
datafile$X<-datafile$Long
datafile$Y<-datafile$Lat
datafile$SteppeMax<-datafile$maxSteppe
datafile$SteppeMin<-datafile$minSteppe





#####################################################################
###                     CORRELATION MATRIX                        ###

#create variables for use in correlation analyses
Total_emp_dens<-datafile$Total_emp_dens
allYield<-datafile$allYield
agriBest<-datafile$agriBest
elevStd<-log10(datafile$elevStd+1) #distribution logged to reduce skewness
maxSteppe_distance<-datafile$maxSteppe_distance
FirstEmp_distance<-datafile$FirstEmp_distance
agriMin<-datafile$agriMin
agriMax<-datafile$agriMax
minSteppe_distance<-datafile$minSteppe_distance
maxSteppe_distance<-datafile$maxSteppe_distance


#run correlation analyses
pearson_correlations<-cor(data.frame(Total_emp_dens,agriBest,agriMin,agriMax,allYield,maxSteppe_distance,minSteppe_distance,FirstEmp_distance,elevStd), method="pearson")
spearman_correlations<-cor(data.frame(Total_emp_dens,agriBest,agriMin,agriMax,allYield,maxSteppe_distance,minSteppe_distance,FirstEmp_distance,elevStd), method="spearman")


write.table(pearson_correlations, file="pearson_correlations.txt", sep="\t", row.names = FALSE)
write.table(spearman_correlations, file="spearman_correlations.txt", sep="\t", row.names = FALSE)



####RUN SPATIAL ANALYSES ON 20 SUB-SAMPLES


#begin loop through different sub-samples
for ( j in 1:20){     
  
  print(j)
  NUM=1000  #choose number of cells to sample
  datafile4=datafile[sample(nrow(datafile), NUM), ]  #create random sample
  
  #create new scaled variables to enable standardized parameter estimates
  #some variables are reversed to make parameter estimates positive and aid intepretation
  Total_emp_dens<-scale(datafile4$Total_emp_dens)
  allYield<-scale(datafile4$allYield)
  agriBest<-scale(datafile4$agriBest*-1) 
  agriMin<-scale(datafile4$agriMin*-1)
  agriMax<-scale(datafile4$agriMax*-1)
  elevStd<-scale((log10(datafile4$elevStd+1))*-1)
  minSteppe_distance<-scale(datafile4$minSteppe_distance*-1)
  maxSteppe_distance<-scale(datafile4$maxSteppe_distance*-1)
  FirstEmp_distance<-scale(datafile4$FirstEmp_distance*-1)
  X<-datafile4$X
  Y<-datafile4$Y
  
  #different models to be run with different predictors - we start with an OLS version of each that then gets updated to a spatially explicit GLS below
  OLS1=gls(Total_emp_dens~agriBest, method = "ML")
  OLS2=gls(Total_emp_dens~allYield, method = "ML")
  OLS3=gls(Total_emp_dens~elevStd, method = "ML")
  OLS4=gls(Total_emp_dens~maxSteppe_distance, method = "ML")
  OLS5=gls(Total_emp_dens~FirstEmp_distance, method = "ML")
  OLS6=gls(Total_emp_dens~agriBest*maxSteppe_distance, method = "ML")
  OLS7=gls(Total_emp_dens~agriBest*maxSteppe_distance+allYield+elevStd+FirstEmp_distance, method = "ML")
  OLS8=gls(Total_emp_dens~agriBest*maxSteppe_distance+FirstEmp_distance, method = "ML")
  OLS9=gls(Total_emp_dens~agriBest+maxSteppe_distance, method = "ML")
  OLS10=gls(Total_emp_dens~agriBest+maxSteppe_distance+FirstEmp_distance, method = "ML")
  OLS11=gls(Total_emp_dens~agriBest+allYield+elevStd+maxSteppe_distance+FirstEmp_distance, method = "ML")
  OLS12=gls(Total_emp_dens~agriBest+FirstEmp_distance, method = "ML")
  OLS13=gls(Total_emp_dens~maxSteppe_distance+FirstEmp_distance, method = "ML")
  OLS14=gls(Total_emp_dens~1, method = "ML")
  OLS15=gls(Total_emp_dens~agriBest*maxSteppe_distance+allYield+elevStd, method = "ML")
  OLS16=gls(Total_emp_dens~agriBest+allYield+elevStd+maxSteppe_distance, method = "ML")
  
  GLS1=update(OLS1,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS1","_",toString(j),".txt",sep=""))
  print(summary(GLS1))
  sink()   
  print("model completed")
  
  
  GLS2=update(OLS2,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS2","_",toString(j),".txt",sep=""))
  print(summary(GLS2))
  sink()   
  print("model completed")
  
  
  GLS3=update(OLS3,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS3","_",toString(j),".txt",sep=""))
  print(summary(GLS3))
  sink()   
  print("model completed")
  
  
  GLS4=update(OLS4,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS4","_",toString(j),".txt",sep=""))
  print(summary(GLS4))
  sink()   
  print("model completed")
  
  GLS5=update(OLS5,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS5","_",toString(j),".txt",sep=""))
  print(summary(GLS5))
  sink()   
  print("model completed")
  
  GLS6=update(OLS6,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS6","_",toString(j),".txt",sep=""))
  print(summary(GLS6))
  sink()   
  print("model completed")
  
  
  GLS7=update(OLS7,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS7","_",toString(j),".txt",sep=""))
  print(summary(GLS7))
  sink()   
  print("model completed")
  
  
  GLS8=update(OLS8,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS8","_",toString(j),".txt",sep=""))
  print(summary(GLS8))
  sink()   
  print("model completed")
  
  
  GLS9=update(OLS9,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS9","_",toString(j),".txt",sep=""))
  print(summary(GLS9))
  sink()   
  print("model completed")
  
  
  GLS10=update(OLS10,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS10","_",toString(j),".txt",sep=""))
  print(summary(GLS10))
  sink()   
  print("model completed")
  
  
  GLS11=update(OLS11,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS11","_",toString(j),".txt",sep=""))
  print(summary(GLS11))
  sink()   
  print("model completed")
  
  
  GLS12=update(OLS12,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS12","_",toString(j),".txt",sep=""))
  print(summary(GLS12))
  sink()   
  print("model completed")
  
  
  GLS13=update(OLS13,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS13","_",toString(j),".txt",sep=""))
  print(summary(GLS13))
  sink()   
  print("model completed")
  
  
  GLS14=update(OLS14,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS14","_",toString(j),".txt",sep=""))
  print(summary(GLS14))
  sink()   
  print("model completed")
  
  
  GLS15=update(OLS15,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS15","_",toString(j),".txt",sep=""))
  print(summary(GLS15))
  sink()   
  print("model completed")
  
  
  GLS16=update(OLS16,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS16","_",toString(j),".txt",sep=""))
  print(summary(GLS16))
  sink()   
  print("model completed")
  
  
  
  
  model_list<-c("GLS1", "GLS2", "GLS3", "GLS4", "GLS5", "GLS6", "GLS7", "GLS8", "GLS9", "GLS10", "GLS11", "GLS12", "GLS13", "GLS14", "GLS15", "GLS16")

  #calculate AIC scores using information about log likelihood and no. of parameters  
  logLik_list<-c(GLS1$logLik,  GLS2$logLik,  GLS3$logLik,  GLS4$logLik,  GLS5$logLik,  GLS6$logLik,  GLS7$logLik,  GLS8$logLik,  GLS9$logLik,  GLS10$logLik,  GLS11$logLik,  GLS12$logLik,  GLS13$logLik,  GLS14$logLik, GLS15$logLik, GLS16$logLik)
  df_list<-c(GLS1$dims$p,  GLS2$dims$p,  GLS3$dims$p,  GLS4$dims$p,  GLS5$dims$p,  GLS6$dims$p,  GLS7$dims$p,  GLS8$dims$p,  GLS9$dims$p,  GLS10$dims$p,  GLS11$dims$p,  GLS12$dims$p,  GLS13$dims$p,  GLS14$dims$p,  GLS15$dims$p,  GLS16$dims$p)
  AIC_list<-(2*df_list)-(2*logLik_list)
  coeff_list<-c(GLS1$coefficients,  GLS2$coefficients,  GLS3$coefficients,  GLS4$coefficients,  GLS5$coefficients,  GLS6$coefficients,  GLS7$coefficients,  GLS8$coefficients,  GLS9$coefficients,  GLS10$coefficients,  GLS11$coefficients,  GLS12$coefficients,  GLS13$coefficients,  GLS14$coefficients,  GLS15$coefficients,  GLS16$coefficients)
  coeff_names_list<-c(names(GLS1$coefficients), names(GLS2$coefficients), names(GLS3$coefficients), names(GLS4$coefficients), names(GLS5$coefficients), names(GLS6$coefficients), names(GLS7$coefficients), names(GLS8$coefficients), names(GLS9$coefficients), names(GLS10$coefficients), names(GLS11$coefficients), names(GLS12$coefficients), names(GLS13$coefficients), names(GLS14$coefficients), names(GLS15$coefficients), names(GLS16$coefficients))
  
  out_LH_p<-data.frame(logLik_list,df_list,AIC_list)
  out_coeff<-data.frame(coeff_names_list, coeff_list)

  #write output to file  
  write.table(out_LH_p, file=paste("GLS_logLik_p_",toString(j),".txt",sep=""), sep="\t", row.names = FALSE)
  write.table(out_coeff, file=paste("GLS_coeff_",toString(j),".txt",sep=""), sep="\t", row.names = FALSE)
  
  write.table(datafile4, file=paste("GLS_datafile_",toString(j),".txt",sep=""), sep="\t", row.names = FALSE)
  
  
  
}#end loop for repetitions of sub-samples


