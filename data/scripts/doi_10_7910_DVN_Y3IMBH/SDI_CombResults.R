## ======================================================================
## This file loads the results of the meta-analyses (which were generated in 2-analysisFramework.R)
## and computes summaries of them (such as mean error, MSE, coverage, etc.)
## ======================================================================
# load the results files which were generated in 2-analysisFramework.R,
# combine them into one large data frame
analysisFiles <- mixedsort(list.files("SDI/analysisResults", pattern=".*\\.RData", full.names=TRUE))
print(paste0("Collecting results from ", length(analysisFiles), " analysis files."))
#######################
##  Simulation Set up Cohen's d
######################################
simN <- SimulationLength
simulationType <- "Cohend"
effectSize_List<-c(0, 50);
sigH_List<-c(0, 6.25, 12.5, 25, 50);
PubBias_List<-c(0, 0.5, 0.75);
MetaStudyN_List<- c(5,10,20,40,80);
param1 <- as.data.frame(expand.grid(effectSize=effectSize_List, sigH=sigH_List, PubBias=PubBias_List, m=MetaStudyN_List, SimType=simulationType))

simulationType <- "LogOdds"
effectSize_List<-c(0.00, 0.03, 0.06);
sigH_List<-c(0.006);
PubBias_List<-c(0.0, 0.5);
MetaStudyN_List<- c(5,10,20,40,80);
param2 <- as.data.frame(expand.grid(effectSize=effectSize_List, sigH=sigH_List, PubBias=PubBias_List, m=MetaStudyN_List, SimType=simulationType))

paramONE <- rbind(param1,param2)


simulationType <- "Cohend"
effectSize_List<-c(0, 50);
sigH_List<-c(0, 6.25, 12.5, 25, 50);
PubBias_List<-c(0, 0.5, 0.75);
MetaStudyN_List<- c(100,200,400,800);
param3 <- as.data.frame(expand.grid(effectSize=effectSize_List, sigH=sigH_List, PubBias=PubBias_List, m=MetaStudyN_List, SimType=simulationType))

simulationType <- "LogOdds"
effectSize_List<-c(0.00, 0.03, 0.06);
sigH_List<-c(0.006);
PubBias_List<-c(0.0, 0.5);
MetaStudyN_List<- c(100,200,400,800);
param4 <- as.data.frame(expand.grid(effectSize=effectSize_List, sigH=sigH_List, PubBias=PubBias_List, m=MetaStudyN_List, SimType=simulationType))
paramTWO <- rbind(param3,param4)
param <- rbind(paramONE,paramTWO)
param$Condition <- c(1:nrow(param))
######################################


# loop through all files
res_list <- list()
table.final <- c()
i<-1
for (f in analysisFiles) {
  res_list <- list()
  print(f)
  load(f)	# the simulation data frame always is called "res"
  res_list <-Est.Results	
  resfinal <- bind_rows(res_list)
  res.final <- sqldf('select resfinal.*, param.m as m from resfinal, param where resfinal.Condition=param.Condition')
  tab <- res.final %>% group_by(Environ, Condition, TrueEffect, sigH, PubBias, m) %>% summarise(n.MA=length(unique(RepID)))
  res.final <- res.final %>% droplevels()
  res.wide <- dcast(res.final, RepID + Condition + TrueEffect + sigH + PubBias + m + method ~ term + variable, value.var="value")
 
  tmpDT <- subset(res.wide, res.wide$Condition==i)
  
  tmpDT2 <- subset(tmpDT , tmpDT$method=="reMA")
  i2tmp <- sort(tmpDT2$I2_estimate[!is.na(tmpDT2$I2_estimate)])
  bnd <- round(length(i2tmp)*c(.025,0.975),0)+c(1,-1)
  i2tmp <- i2tmp[bnd[1]:bnd[2]]
  I2 <- mean(i2tmp/100)
  tmpDT2 <- subset(tmpDT , tmpDT$method=="EK")
  ssize <- mean(tmpDT2$b0_DataSize, na.rm=T)
  ss <- tmpDT2$b0_DataSize
  simCase <- cbind(subset(param, param$Condition==i), DTsize=ssize, I2=I2) 
  
  for(j in c("reMA" , "TF" , "PETPEESE" , "pcurve" , "puniform" , "3PSM" , "4PSM" , "WAAP-WLS" , "AK1" , "AK2" , "EK")){
    tmpDT2 <- subset(tmpDT , tmpDT$method==j)
    tmpDT2 <- tmpDT2[(!is.na(tmpDT2$b0_estimate)),]
    tmpDT2 <- tmpDT2[order(tmpDT2$b0_estimate),]
    bnd <- round(nrow(tmpDT2)*c(.025,0.975),0)+c(1,-1)
    if(nrow(tmpDT2)>2){
      tmpDT2 <- tmpDT2[c(bnd[1]:bnd[2]),]
    }
    bias <- abs(mean(tmpDT2$b0_estimate-tmpDT2$TrueEffect, na.rm=T))
    mse <- mean((tmpDT2$b0_estimate-tmpDT2$TrueEffect)^2, na.rm=T)
    
    
    if(j=="AK2"){
      COVDT <- cbind.data.frame(lower=(tmpDT2$b0_estimate- qt(0.975, df=ss[1]-1)*tmpDT2$b0_std.error),
                                upper=(tmpDT2$b0_estimate+ qt(0.975, df=ss[1]-1)*tmpDT2$b0_std.error),
                                true=tmpDT2$TrueEffect)
      COVDT <- NaRV.omit(COVDT)
      cov <-mean((COVDT$lower<COVDT$true)*(COVDT$true<COVDT$upper), na.rm=T)
    }else{
      cov <-mean((tmpDT2$b0_conf.low<tmpDT2$TrueEffect)*(tmpDT2$TrueEffect<tmpDT2$b0_conf.high), na.rm=T)
    }
    combine <- cbind.data.frame(bias, mse, cov)
    colnames(combine)<-paste0(c("bias_","mse_","cov_"), j)
    simCase <- cbind(simCase , combine)
  }
  table.final <- rbind.data.frame(table.final, simCase)
  rm(res_list)
  i<-i+1
}

View(table.final)

Est <- c("reMA" , "TF" , "PETPEESE" , "pcurve" , "puniform" , "3PSM" , "4PSM" , "WAAP-WLS" , "AK1" , "AK2" , "EK")
ColSelect <- c(paste0(c("bias_"), Est), paste0(c("mse_"), Est), paste0(c("cov_"), Est))

EstResultsI <- table.final[, c("PubBias","effectSize","sigH","DTsize","I2")]
EstResultsI <- cbind.data.frame(Env="SDI", Type=NA, BiasI=NA, EstResultsI)
colnames(EstResultsI) <- c("Study","Type","BiasI","BiasII","TrueValue","tau","Size","I2")
EstResultsII <- table.final[, c(paste0(c("bias_"), Est), paste0(c("mse_"), Est), paste0(c("cov_"), Est))]
EstResults <- cbind.data.frame(EstResultsI, EstResultsII)
write.csv(EstResults, "SDI/dataFiles/table.final.csv", row.names=FALSE)



