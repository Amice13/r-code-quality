analysisFiles <- mixedsort(list.files("Carter/analysisResults", pattern=".*\\.RData", full.names=TRUE))
print(paste0("Collecting results from ", length(analysisFiles), " analysis files."))


# ---------------------------------------------------------------------
#  experimental factors
k_set <- c(10, 30, 60, 100)							# number of studies in each MA
delta_set <- c(0, .2, .5, .8)						# true mean of effect sizes
qrpEnv_Set <- c("none", "med", "high")	# QRP environment
censor_set <- c("none", "med", "high")	# publication bias
tau_set <- c(0, .2, .4)									# heterogeneity; assumed to follow a normal distribution

# params stores all possible combinations of experimental factors
paramsONE <- expand.grid(k=k_set, delta=delta_set, qrpEnv=qrpEnv_Set, censor=censor_set, tau=tau_set)


k_set <- c(200, 400, 800)							  # number of studies in each MA
delta_set <- c(0, .2, .5, .8)						# true mean of effect sizes
qrpEnv_Set <- c("none", "med", "high")	# QRP environment
censor_set <- c("none", "med", "high")	# publication bias
tau_set <- c(0, .2, .4)									# heterogeneity; assumed to follow a normal distribution

# params stores all possible combinations of experimental factors
paramsTWO <- expand.grid(k=k_set, delta=delta_set, qrpEnv=qrpEnv_Set, censor=censor_set, tau=tau_set)
params <- rbind(paramsONE, paramsTWO)
params$Condition <- c(1:nrow(params))
rownames(params) <- NULL
print(paste0(nrow(params), " fully crossed experimental conditions have been generated."))

param <- params




# loop through all files
res_list <- list()
table.final <- c()
i<-1
for (f in analysisFiles) {
  res_list <- list()
  print(f)
  load(f)	# the simulation data frame always is called "res"
  #res$id <- paste0(f, "_", res$id)
  tt <- subset(Est.Results, Est.Results$method=='reMA' & Est.Results$term=='b0' & Est.Results$variable=='estimate')
  indx <- as.numeric(c(as.numeric(rownames(tt)), nrow(Est.Results)))
  idnum <- c()
  for(j in 1:(length(indx)-1)){
    idnum <- c(idnum, rep(j, (indx[j+1]-indx[j])))
  }
  Est.Results$RepId <- c(idnum, j)
  res_list <-Est.Results	
  res.final <- bind_rows(res_list)
  tab <- res.final %>% group_by(k, delta, qrpEnv, censor, tau)  %>% summarise(n.MA=length(unique(RepId)))
  res.final <- res.final %>% droplevels()
  res.wide <- dcast(res.final, RepId + Condition + k + delta + qrpEnv + censor + tau + method ~ term + variable, value.var="value")
  
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
    bias <- abs(mean(tmpDT2$b0_estimate-tmpDT2$delta, na.rm=T))
    mse <- mean((tmpDT2$b0_estimate-tmpDT2$delta)^2, na.rm=T)
    
    if(j=="AK2"){
      COVDT <- cbind.data.frame(lower=(tmpDT2$b0_estimate- qt(0.975, df=ss[1]-1)*tmpDT2$b0_std.error),
                                upper=(tmpDT2$b0_estimate+ qt(0.975, df=ss[1]-1)*tmpDT2$b0_std.error),
                                true=tmpDT2$delta)
      COVDT <- NaRV.omit(COVDT)
      cov <-mean((COVDT$lower<COVDT$true)*(COVDT$true<COVDT$upper), na.rm=T)
    }else{
      cov <-mean((tmpDT2$b0_conf.low<tmpDT2$delta)*(tmpDT2$delta<tmpDT2$b0_conf.high), na.rm=T)
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
EstResultsI <- table.final[, c("qrpEnv","censor","delta","tau","DTsize","I2")]
EstResultsI <- cbind.data.frame(Env="Carteretal", Type="Cohensd", EstResultsI)
colnames(EstResultsI) <- c("Study","Type","BiasI","BiasII","TrueValue","tau","Size","I2")
EstResultsII <- table.final[, c(paste0(c("bias_"), Est), paste0(c("mse_"), Est), paste0(c("cov_"), Est))]
EstResults <- cbind.data.frame(EstResultsI, EstResultsII)
write.csv(EstResults, "Carter/dataFiles/table.final.csv", row.names=FALSE)


