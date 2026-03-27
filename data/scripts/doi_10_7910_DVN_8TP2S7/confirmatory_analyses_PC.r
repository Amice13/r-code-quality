

#######################################################################
###                 Confirmatory analyses                           ###


datafile<-read.csv("Gridded_dataset_April_2020.csv",header=T)

datafile$X<-datafile$Long
datafile$Y<-datafile$Lat
datafile$SteppeMax<-datafile$maxSteppe
datafile$SteppeMin<-datafile$minSteppe



############################################################
#assess uncertainty using different estimations of variables by examining whether the same qualitative pattern is produced
#analyses follow same process as in main analyses

#cycle through analyses on 5 sub-samples of the data
for ( k in 1:5){
  print(k)
  NUM=500 #no. of cells in sub-sample
  datafile5=datafile[sample(nrow(datafile), NUM), ]  #create random sample
  
  Total_emp_dens<-scale(datafile5$Total_emp_dens)
  allYield<-scale(datafile5$allYield)
  agriBest<-scale(datafile5$agriBest*-1)
  agriMin<-scale(datafile5$agriMin*-1)
  agriMax<-scale(datafile5$agriMax*-1)
  elevStd<-scale((log10(datafile5$elevStd+1))*-1)
  minSteppe_distance<-scale(datafile5$minSteppe_distance*-1)
  maxSteppe_distance<-scale(datafile5$maxSteppe_distance*-1)
  FirstEmp_distance<-scale(datafile5$FirstEmp_distance*-1)
  X<-datafile5$X
  Y<-datafile5$Y
  
  
  OLS_conf1=gls(Total_emp_dens~agriBest+minSteppe_distance, method = "ML")
  OLS_conf2=gls(Total_emp_dens~agriBest*minSteppe_distance, method = "ML")
  OLS_conf3=gls(Total_emp_dens~agriBest+minSteppe_distance+FirstEmp_distance, method = "ML")
  OLS_conf4=gls(Total_emp_dens~agriBest*minSteppe_distance+FirstEmp_distance, method = "ML")
  OLS_conf5=gls(Total_emp_dens~agriMin+maxSteppe_distance, method = "ML")
  OLS_conf6=gls(Total_emp_dens~agriMin*maxSteppe_distance, method = "ML")
  OLS_conf7=gls(Total_emp_dens~agriMin+maxSteppe_distance+FirstEmp_distance, method = "ML")
  OLS_conf8=gls(Total_emp_dens~agriMin*maxSteppe_distance+FirstEmp_distance, method = "ML")
  OLS_conf9=gls(Total_emp_dens~agriMin+minSteppe_distance, method = "ML")
  OLS_conf10=gls(Total_emp_dens~agriMin*minSteppe_distance, method = "ML")
  OLS_conf11=gls(Total_emp_dens~agriMin+minSteppe_distance+FirstEmp_distance, method = "ML")
  OLS_conf12=gls(Total_emp_dens~agriMin*minSteppe_distance+FirstEmp_distance, method = "ML")
  OLS_conf13=gls(Total_emp_dens~agriMax+maxSteppe_distance, method = "ML")
  OLS_conf14=gls(Total_emp_dens~agriMax*maxSteppe_distance, method = "ML")
  OLS_conf15=gls(Total_emp_dens~agriMax+maxSteppe_distance+FirstEmp_distance, method = "ML")
  OLS_conf16=gls(Total_emp_dens~agriMax*maxSteppe_distance+FirstEmp_distance, method = "ML")
  OLS_conf17=gls(Total_emp_dens~agriMax+minSteppe_distance, method = "ML")
  OLS_conf18=gls(Total_emp_dens~agriMax*minSteppe_distance, method = "ML")
  OLS_conf19=gls(Total_emp_dens~agriMax+minSteppe_distance+FirstEmp_distance, method = "ML")
  OLS_conf20=gls(Total_emp_dens~agriMax*minSteppe_distance+FirstEmp_distance, method = "ML")
  
  GLS_conf1=update(OLS_conf1,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS_conf1_",toString(k),".txt", sep=""))
  print(summary(GLS_conf1))
  sink()
  
  
  GLS_conf2=update(OLS_conf2,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS_conf2_",toString(k),".txt", sep=""))
  print(summary(GLS_conf2))
  sink()
  
  GLS_conf3=update(OLS_conf3,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS_conf3_",toString(k),".txt", sep=""))
  print(summary(GLS_conf3))
  sink()
  
  GLS_conf4=update(OLS_conf4,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS_conf4_",toString(k),".txt", sep=""))
  print(summary(GLS_conf4))
  sink()
  
  GLS_conf5=update(OLS_conf5,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS_conf5_",toString(k),".txt", sep=""))
  print(summary(GLS_conf5))
  sink()
  
  GLS_conf6=update(OLS_conf6,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS_conf6_",toString(k),".txt", sep=""))
  print(summary(GLS_conf6))
  sink()
  
  GLS_conf7=update(OLS_conf7,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS_conf7_",toString(k),".txt", sep=""))
  print(summary(GLS_conf7))
  sink()
  
  GLS_conf8=update(OLS_conf8,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS_conf8_",toString(k),".txt", sep=""))
  print(summary(GLS_conf8))
  sink()
  
  GLS_conf9=update(OLS_conf9,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS_conf9_",toString(k),".txt", sep=""))
  print(summary(GLS_conf9))
  sink()
  
  GLS_conf10=update(OLS_conf10,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS_conf10_",toString(k),".txt", sep=""))
  print(summary(GLS_conf10))
  sink()
  
  GLS_conf11=update(OLS_conf11,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS_conf11_",toString(k),".txt", sep=""))
  print(summary(GLS_conf11))
  sink()
  
  GLS_conf12=update(OLS_conf12,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS_conf12_",toString(k),".txt", sep=""))
  print(summary(GLS_conf12))
  sink()
  
  GLS_conf13=update(OLS_conf13,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS_conf13_",toString(k),".txt", sep=""))
  print(summary(GLS_conf13))
  sink()
  
  GLS_conf14=update(OLS_conf14,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS_conf14_",toString(k),".txt", sep=""))
  print(summary(GLS_conf14))
  sink()
  
  GLS_conf15=update(OLS_conf15,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS_conf15_",toString(k),".txt", sep=""))
  print(summary(GLS_conf15))
  sink()
  
  GLS_conf16=update(OLS_conf16,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS_conf16_",toString(k),".txt", sep=""))
  print(summary(GLS_conf16))
  sink()
  
  GLS_conf17=update(OLS_conf17,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS_conf17_",toString(k),".txt", sep=""))
  print(summary(GLS_conf17))
  sink()
  
  GLS_conf18=update(OLS_conf18,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS_conf18_",toString(k),".txt", sep=""))
  print(summary(GLS_conf18))
  sink()
  
  GLS_conf19=update(OLS_conf19,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS_conf19_",toString(k),".txt", sep=""))
  print(summary(GLS_conf19))
  sink()
  
  GLS_conf20=update(OLS_conf20,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS_conf20_",toString(k),".txt", sep=""))
  print(summary(GLS_conf20))
  sink()
  
  
  logLik_list_conf<-c(GLS_conf1$logLik,  GLS_conf2$logLik,  GLS_conf3$logLik,  GLS_conf4$logLik,  GLS_conf5$logLik,  GLS_conf6$logLik,  GLS_conf7$logLik,  GLS_conf8$logLik,  GLS_conf9$logLik,  GLS_conf10$logLik,  GLS_conf11$logLik,  GLS_conf12$logLik,  GLS_conf13$logLik,  GLS_conf14$logLik, GLS_conf15$logLik, GLS_conf16$logLik,  GLS_conf17$logLik,  GLS_conf18$logLik, GLS_conf19$logLik, GLS_conf20$logLik)
  df_list_conf<-c(GLS_conf1$dims$p,  GLS_conf2$dims$p,  GLS_conf3$dims$p,  GLS_conf4$dims$p,  GLS_conf5$dims$p,  GLS_conf6$dims$p,  GLS_conf7$dims$p,  GLS_conf8$dims$p,  GLS_conf9$dims$p,  GLS_conf10$dims$p,  GLS_conf11$dims$p,  GLS_conf12$dims$p,  GLS_conf13$dims$p,  GLS_conf14$dims$p,  GLS_conf15$dims$p,  GLS_conf16$dims$p,  GLS_conf17$dims$p,  GLS_conf18$dims$p,  GLS_conf19$dims$p,  GLS_conf20$dims$p)
  AIC_list_conf<-(2*df_list_conf)-(2*logLik_list_conf)
  coeff_list_conf<-c(GLS_conf1$coefficients,  GLS_conf2$coefficients,  GLS_conf3$coefficients,  GLS_conf4$coefficients,  GLS_conf5$coefficients,  GLS_conf6$coefficients,  GLS_conf7$coefficients,  GLS_conf8$coefficients,  GLS_conf9$coefficients,  GLS_conf10$coefficients,  GLS_conf11$coefficients,  GLS_conf12$coefficients,  GLS_conf13$coefficients,  GLS_conf14$coefficients,  GLS_conf15$coefficients,  GLS_conf16$coefficients,  GLS_conf17$coefficients,  GLS_conf18$coefficients,  GLS_conf19$coefficients,  GLS_conf20$coefficients)
  coeff_names_list_conf<-c(names(GLS_conf1$coefficients), names(GLS_conf2$coefficients), names(GLS_conf3$coefficients), names(GLS_conf4$coefficients), names(GLS_conf5$coefficients), names(GLS_conf6$coefficients), names(GLS_conf7$coefficients), names(GLS_conf8$coefficients), names(GLS_conf9$coefficients), names(GLS_conf10$coefficients), names(GLS_conf11$coefficients), names(GLS_conf12$coefficients), names(GLS_conf13$coefficients), names(GLS_conf14$coefficients), names(GLS_conf15$coefficients), names(GLS_conf16$coefficients), names(GLS_conf17$coefficients), names(GLS_conf18$coefficients), names(GLS_conf19$coefficients), names(GLS_conf20$coefficients))
  
  
  
  out_LH_p_conf<-data.frame(logLik_list_conf,df_list_conf,AIC_list_conf)
  out_coeff_conf<-data.frame(coeff_names_list_conf,coeff_list_conf)
  
  write.table(out_LH_p_conf, file=paste("GLS_logLik_p_conf_",toString(k),".txt",sep=""), sep="\t", row.names = FALSE)
  write.table(out_coeff_conf, file=paste("GLS_coeff_conf_",toString(k),".txt",sep=""), sep="\t", row.names = FALSE)
  
  write.table(datafile5, file=paste("GLS_datafile_conf_",toString(k),".txt",sep=""), sep="\t", row.names = FALSE)
  
  
}   #end loop for repetitions of models



####################################################################################
####################################################################################


#examine whether threshold for including empire in final sample affects main results

#process is similar to above and main analyses

for ( k in 1:10){
  print(k)
  NUM=1000
  datafile9=datafile[sample(nrow(datafile), NUM), ]  #create random sample
  
  Total_emp_dens_80<-scale(datafile9$Total_emp_dens_80)
  Total_emp_dens_120<-scale(datafile9$Total_emp_dens_120)
  allYield<-scale(datafile9$allYield)
  agriBest<-scale(datafile9$agriBest*-1)
  elevStd<-scale((log10(datafile9$elevStd+1))*-1)
  maxSteppe_distance<-scale(datafile9$maxSteppe_distance*-1)
  FirstEmp_distance_80<-scale(datafile9$FirstEmp_distance_80*-1)
  FirstEmp_distance_120<-scale(datafile9$FirstEmp_distance_120*-1)
  X<-datafile9$X
  Y<-datafile9$Y
  
  
  OLS_conf1_emp80=gls(Total_emp_dens_80~agriBest*maxSteppe_distance+allYield+elevStd, method = "ML")
  OLS_conf2_emp120=gls(Total_emp_dens_120~agriBest*maxSteppe_distance+allYield+elevStd, method = "ML")
  OLS_conf3_emp80=gls(Total_emp_dens_80~agriBest*maxSteppe_distance+allYield+elevStd+FirstEmp_distance_80, method = "ML")
  OLS_conf4_emp120=gls(Total_emp_dens_120~agriBest*maxSteppe_distance+allYield+elevStd+FirstEmp_distance_120, method = "ML")

  GLS_conf1_emp80=update(OLS_conf1_emp80,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS_conf1_emp80_",toString(k),".txt", sep=""))
  print(summary(GLS_conf1_emp80))
  sink()
  
  GLS_conf2_emp120=update(OLS_conf2_emp120,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS_conf2_emp120_",toString(k),".txt", sep=""))
  print(summary(GLS_conf2_emp120))
  sink()
  
  GLS_conf3_emp80=update(OLS_conf3_emp80,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS_conf3_emp80_",toString(k),".txt", sep=""))
  print(summary(GLS_conf3_emp80))
  sink()
  
  GLS_conf4_emp120=update(OLS_conf4_emp120,corr=corRatio(c(500, 0.05),form=~X+Y, nugget=T, fixed=F, metric="rdist.earth"), method="ML")
  sink(paste("GLS_conf4_emp120_",toString(k),".txt", sep=""))
  print(summary(GLS_conf4_emp120))
  sink()
  

  logLik_list_conf_emp<-c(GLS_conf1_emp80$logLik,  GLS_conf2_emp120$logLik,  GLS_conf3_emp80$logLik,  GLS_conf4_emp120$logLik)
  df_list_conf_emp<-c(GLS_conf1_emp80$dims$p,  GLS_conf2_emp120$dims$p,  GLS_conf3_emp80$dims$p,  GLS_conf4_emp120$dims$p)
  AIC_list_conf_emp<-(2*df_list_conf_emp)-(2*logLik_list_conf_emp)
  coeff_list_conf_emp<-c(GLS_conf1_emp80$coefficients,  GLS_conf2_emp120$coefficients,  GLS_conf3_emp80$coefficients,  GLS_conf4_emp120$coefficients)
  coeff_names_list_conf_emp<-c(names(GLS_conf1_emp80$coefficients), names(GLS_conf2_emp120$coefficients), names(GLS_conf3_emp80$coefficients), names(GLS_conf4_emp120$coefficients))
  
  
  
  out_LH_p_conf_emp<-data.frame(logLik_list_conf_emp,df_list_conf_emp,AIC_list_conf_emp)
  out_coeff_conf_emp<-data.frame(coeff_names_list_conf_emp,coeff_list_conf_emp)
  
  write.table(out_LH_p_conf_emp, file=paste("GLS_logLik_p_conf_emp_",toString(k),".txt",sep=""), sep="\t", row.names = FALSE)
  write.table(out_coeff_conf_emp, file=paste("GLS_coeff_conf_emp_",toString(k),".txt",sep=""), sep="\t", row.names = FALSE)
  
  write.table(datafile9, file=paste("GLS_datafile_conf_emp_",toString(k),".txt",sep=""), sep="\t", row.names = FALSE)
  
  
}   #end loop for analyses of sub-samples





#################################




