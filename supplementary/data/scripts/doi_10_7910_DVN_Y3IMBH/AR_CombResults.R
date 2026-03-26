analysisFiles <- list.files("AR/analysisResults", pattern=".*\\.RData", full.names=TRUE)
print(paste0("Collecting results from ", length(analysisFiles), " analysis files."))

# loop through all files
res_list <- list()
for (f in analysisFiles) {
	print(f)
	load(f)	# the simulation data frame always is called "res"
	#res$id <- paste0(f, "_", res$id)
	res_list[[f]] <- Est.Results
}
res.final <- bind_rows(res_list)
str(res.final)

# final data set in long format:
save(res.final, file="AR/dataFiles/res.final.RData")
#load(file="dataFiles/res.final.RData")


# Show conditions
tab <- res.final %>% group_by(Environment, Condition, al, bias) %>% summarise(n.MA=length(unique(replication)))
all(tab$n.MA == SimulationLength)

res.final <- res.final %>% droplevels()

# reshape long format to wide format
res.wide <- dcast(res.final, replication + Condition + al + bias + method ~ term + variable, value.var="value")
head(res.wide, 1)
# save(res.wide, file="AR/dataFiles/res.wide.RData")


#######################
##  Simulation Set up
#######################
Sim_Environ<-c('RE','PRE','FE') # 'PRE'
effectSize_List=c(0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0);
simN <- SimulationLength

param <- as.data.frame(matrix(NA, nrow=(length(Sim_Environ)*length(effectSize_List)), ncol=3))
colnames(param) <-c("Environ", "Effect","Condition")
param$Environ <- sort(rep(Sim_Environ, length(effectSize_List)))
param$Effect <- rep(effectSize_List, length(Sim_Environ))
param$Condition <- c(1:nrow(param))
######################################


table.final <- c()
for(bs in c("Pos",  "Sig" )){
for(i in unique(res.wide$Condition)){
 tmpDT <- subset(res.wide, res.wide$Condition==i & res.wide$bias==bs)
 tmpDT2 <- subset(tmpDT , tmpDT$method=="reMA")
 
 i2tmp <- sort(tmpDT2$I2_estimate[!is.na(tmpDT2$I2_estimate)])
 bnd <- round(length(i2tmp)*c(.025,0.975),0)+c(1,-1)
 i2tmp <- i2tmp[bnd[1]:bnd[2]]
 I2 <- mean(i2tmp/100, na.rm=T)
 tmpDT2 <- subset(tmpDT , tmpDT$method=="EK")
 ssize <- mean(tmpDT2$b0_DataSize, na.rm=T)
 ss <- tmpDT2$b0_DataSize
 simCase <- cbind(subset(param, param$Condition==i), Bias=bs, DTsize=ssize, I2=I2) 
 
 for(j in c("reMA" , "TF" , "PETPEESE" , "pcurve" , "puniform" , "3PSM" , "4PSM" , "WAAP-WLS" , "AK1" , "AK2" , "EK")){
  tmpDT2 <- subset(tmpDT , tmpDT$method==j)
  tmpDT2 <- tmpDT2[(!is.na(tmpDT2$b0_estimate)),]
  tmpDT2 <- tmpDT2[order(tmpDT2$b0_estimate),]
  bnd <- round(nrow(tmpDT2)*c(.025,0.975),0)+c(1,-1)
  if(nrow(tmpDT2)>2){
    tmpDT2 <- tmpDT2[c(bnd[1]:bnd[2]),]
  }
  bias <- abs(mean(tmpDT2$b0_estimate-tmpDT2$al, na.rm=T))
  mse <- mean((tmpDT2$b0_estimate-tmpDT2$al)^2, na.rm=T)
  
  if(j=="AK2"){
    COVDT <- cbind.data.frame(lower=(tmpDT2$b0_estimate- qt(0.975, df=ssize-1)*tmpDT2$b0_std.error),
                            upper=(tmpDT2$b0_estimate+ qt(0.975, df=ssize-1)*tmpDT2$b0_std.error),
                            true=tmpDT2$al)
    COVDT <- NaRV.omit(COVDT)
    cov <-mean((COVDT$lower<COVDT$true)*(COVDT$true<COVDT$upper), na.rm=T)
  }else{
    cov <-mean((tmpDT2$b0_conf.low<tmpDT2$al)*(tmpDT2$al<tmpDT2$b0_conf.high), na.rm=T)
  }
  combine <- cbind.data.frame(bias, mse, cov)
  colnames(combine)<-paste0(c("bias_","mse_","cov_"), j)
  simCase <- cbind(simCase , combine)
}
table.final<- rbind(table.final, simCase)
}
}
write.csv(table.final, "AR/dataFiles/table.final.csv")


Est <- c("reMA" , "TF" , "PETPEESE" , "pcurve" , "puniform" , "3PSM" , "4PSM" , "WAAP-WLS" , "AK1" , "AK2" , "EK")
ColSelect <- c(paste0(c("bias_"), Est), paste0(c("mse_"), Est), paste0(c("cov_"), Est))
EstResultsI <- table.final[, c("Bias","Effect")]
EstResultsI <- cbind.data.frame(Env="AR", Type=table.final[,"Environ"], BiasI=NA, EstResultsI, sigH=NA, table.final[, c("DTsize","I2")])
colnames(EstResultsI) <- c("Study","Type","BiasI","BiasII","TrueValue","tau","Size","I2")
EstResultsII <- table.final[, c(paste0(c("bias_"), Est), paste0(c("mse_"), Est), paste0(c("cov_"), Est))]
EstResults <- cbind.data.frame(EstResultsI, EstResultsII)
write.csv(EstResults, "AR/dataFiles/table.final.csv", row.names=FALSE)








