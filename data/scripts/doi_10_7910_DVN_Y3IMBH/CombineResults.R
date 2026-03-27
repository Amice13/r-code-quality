## ======================================================================
## Collecting and Combining Individual Simulation Resutls
## ======================================================================
library('gtools')
PATH <<- "//file/UsersS$/sho88/Home/Desktop/Simulation2020/"

setwd(PATH)
ResultsCombined <- rbind.data.frame()
studylist <- c("Carter","SDI","AR","BR")
for(f in paste0(studylist,"/dataFiles/table.final.csv")){
  dt <- as.data.frame(read.csv(f))
  dt$BiasI<- as.character(dt$BiasI)
  dt$BiasII<- as.character(dt$BiasII)
  ResultsCombined <- rbind(ResultsCombined, dt)
  colnames(as.data.frame(read.csv(f)))
}
write.csv(ResultsCombined,"MakingTables/CombinedResults.csv", row.names=FALSE)




#################
# Table 2
#################
# as.matrix(colnames(ResultsCombined))
BIAS <- as.matrix(colMeans(ResultsCombined[, c(9:19)], na.rm=T))
BIAS <- t(t(BIAS[order(BIAS),]))
MSE <- as.matrix(colMeans(ResultsCombined[, c(20:30)], na.rm=T))
MSE <- t(t(MSE[order(MSE),]))
COV <- as.matrix(colMeans(ResultsCombined[, c(31:41)], na.rm=T))
COV <- t(t(COV[order(abs(COV-0.95)),]))
Table2 <- cbind.data.frame(BiasRanking=as.matrix(rownames(BIAS)), BIAS=as.numeric(BIAS),
                            MSERanking=as.matrix(rownames(MSE)), MSE=as.numeric(MSE),
                            COVRanking=as.matrix(rownames(COV)), COV=as.numeric(COV))
write.csv(Table2,"MakingTables/Table02.csv", row.names=FALSE)

#################
# Table 3a,b
#################
ResultsCombined$GroupSize <- NA
ResultsCombined$GroupI2 <- NA
ResultsCombined$Group <- NA
ResultsCombined[ResultsCombined$Size<=10, "GroupSize"] <- "S010"
ResultsCombined[(10<ResultsCombined$Size & ResultsCombined$Size<=30), "GroupSize"] <- "S030"
ResultsCombined[(30<ResultsCombined$Size & ResultsCombined$Size<=60), "GroupSize"] <- "S060"
ResultsCombined[(60<ResultsCombined$Size & ResultsCombined$Size<=100), "GroupSize"] <- "S100"
ResultsCombined[(100<ResultsCombined$Size & ResultsCombined$Size<=500), "GroupSize"] <- "S500"
ResultsCombined[(500<ResultsCombined$Size), "GroupSize"] <- "S500P"

for(j in unique(ResultsCombined$GroupSize)){
  ResultsCombined[((ResultsCombined$GroupSize==j) & (ResultsCombined$I2<=0.25) ), "GroupI2"] <- "I1"
  ResultsCombined[((ResultsCombined$GroupSize==j) & (0.25<ResultsCombined$I2 & ResultsCombined$I2<=0.75) ), "GroupI2"] <- "I2"
  ResultsCombined[((ResultsCombined$GroupSize==j) & (0.75<ResultsCombined$I2) ), "GroupI2"] <- "I3"
}
ResultsCombined$Group <- paste(ResultsCombined$GroupSize,ResultsCombined$GroupI2, sep = "")


Groups <- mixedsort(unique(c(ResultsCombined$Group)))
Table3a <- as.data.frame(matrix(NA, nrow=6, ncol=3))
colnames(Table3a) <- c("LowI2","MedI2","HighI2")
rownames(Table3a) <- c("S10","S30","S60","S100","S500","S500P")
Table3b<-Table3a
col<-row<-1
for(i in 1:18){
  tmpdt <- subset(ResultsCombined, ResultsCombined$Group==Groups[i])
  if(nrow(tmpdt)>0){
    Table3a[row,col] <- nrow(tmpdt)
    Table3b[row,col] <- mean(tmpdt$I2, na.rm=T)
  }
  col<-col+1
  if(i%%3==0){
    row<-row+1;
    col<-1;
  }
}
write.csv(Table3a,"MakingTables/Table03a.csv", row.names=FALSE)
write.csv(Table3b,"MakingTables/Table03b.csv", row.names=FALSE)


#################
# Table 4,5,6
#################
Groups <- mixedsort(unique(c(ResultsCombined$Group)))
BiasAve <- cbind.data.frame(c())
MSEAve <- cbind.data.frame(c())
COVAve <- cbind.data.frame(c())
for(i in 1:18){
  tmpdt_BS <- subset(ResultsCombined, ResultsCombined$Group==Groups[i])[, c(9:19)]
  BiasAve_tmp <- as.matrix(abs(colMeans(tmpdt_BS, na.rm=T)))
  BiasAve_tmp <- cbind.data.frame(name=as.matrix(rownames(BiasAve_tmp)), value=BiasAve_tmp)
  BiasAve_tmp <- BiasAve_tmp[order(BiasAve_tmp[,2]),]
  rownames(BiasAve_tmp) <- c()
  colnames(BiasAve_tmp) <- c()
  
  tmpdt_MS <- subset(ResultsCombined, ResultsCombined$Group==Groups[i])[, c(20:30)]
  MSAve_tmp <- as.matrix(abs(colMeans(tmpdt_MS, na.rm=T)))
  MSAve_tmp <- cbind.data.frame(name=as.matrix(rownames(MSAve_tmp)), value=MSAve_tmp)
  MSAve_tmp <- MSAve_tmp[order(MSAve_tmp[,2]),]
  rownames(MSAve_tmp) <- c()
  colnames(MSAve_tmp) <- c()
  
  tmpdt_CV <- subset(ResultsCombined, ResultsCombined$Group==Groups[i])[, c(31:41)]
  CVAve_tmp <- as.matrix(abs(colMeans(tmpdt_CV, na.rm=T)))
  CVAve_tmp <- cbind.data.frame(name=as.matrix(rownames(CVAve_tmp)), value=CVAve_tmp)
  CVAve_tmp <- CVAve_tmp[order( abs(0.95-CVAve_tmp[,2]) ),]
  rownames(CVAve_tmp) <- c()
  colnames(CVAve_tmp) <- c()
  
  if(i==1){
    BiasAve <- BiasAve_tmp
    MSEAve <- MSAve_tmp
    COVAve <- CVAve_tmp
  }else{
    BiasAve <- cbind.data.frame(BiasAve, BiasAve_tmp)
    MSEAve <- cbind.data.frame(MSEAve, MSAve_tmp)
    COVAve <- cbind.data.frame(COVAve, CVAve_tmp)
  }
}
names<-c()
for(i in Groups){
  names <- c(names, paste0(c("Estimators_","Value_"),i))
}
colnames(BiasAve) <-names
colnames(MSEAve) <-names
colnames(COVAve) <-names
write.csv(BiasAve,"MakingTables/Table04.csv", row.names=FALSE)
write.csv(MSEAve,"MakingTables/Table05.csv", row.names=FALSE)
write.csv(COVAve,"MakingTables/Table06.csv", row.names=FALSE)


#################
# Table 7
#################
Groups <- mixedsort(unique(c(ResultsCombined$Group)))
CarterSTUDY <- subset(ResultsCombined, ResultsCombined$Study=="Carteretal")
SDISTUDY <- subset(ResultsCombined, ResultsCombined$Study=="SDI")
BRSTUDY <- subset(ResultsCombined, ResultsCombined$Study=="BR")
ARSTUDY <- subset(ResultsCombined, ResultsCombined$Study=="AR")
BiasForEachStudy <- c()
MSEForEachStudy <- c()
COVForEachStudy <- c()
for(i in 1:length(Groups)){
  g <- Groups[i]
  tmpCarterSTUDY <- subset(CarterSTUDY, CarterSTUDY$Group==g)
  tmpCarterSTUDY_BS <- as.matrix(abs(colMeans(tmpCarterSTUDY[, c(9:19)], na.rm=T)))
  tmpCarterSTUDY_BS <- cbind.data.frame(name=as.matrix(rownames(tmpCarterSTUDY_BS)), value=tmpCarterSTUDY_BS)
  tmpCarterSTUDY_BS <- tmpCarterSTUDY_BS[order(tmpCarterSTUDY_BS[,2]),][1,]
  tmpCarterSTUDY_MS <- as.matrix(abs(colMeans(tmpCarterSTUDY[, c(20:30)], na.rm=T)))
  tmpCarterSTUDY_MS <- cbind.data.frame(name=as.matrix(rownames(tmpCarterSTUDY_MS)), value=tmpCarterSTUDY_MS)
  tmpCarterSTUDY_MS <- tmpCarterSTUDY_MS[order(tmpCarterSTUDY_MS[,2]),][1,]                           
  tmpCarterSTUDY_CV <- as.matrix(abs(colMeans(tmpCarterSTUDY[, c(31:41)], na.rm=T)))
  tmpCarterSTUDY_CV <- cbind.data.frame(name=as.matrix(rownames(tmpCarterSTUDY_CV)), value=tmpCarterSTUDY_CV)
  tmpCarterSTUDY_CV <- tmpCarterSTUDY_CV[order(abs(0.95-tmpCarterSTUDY_CV[,2])),][1,]     
  
  tmpSDISTUDY <- subset(SDISTUDY, SDISTUDY$Group==g)
  tmpSDISTUDY_BS <- as.matrix(abs(colMeans(tmpSDISTUDY[, c(9:19)], na.rm=T)))
  tmpSDISTUDY_BS <- cbind.data.frame(name=as.matrix(rownames(tmpSDISTUDY_BS)), value=tmpSDISTUDY_BS)
  tmpSDISTUDY_BS <- tmpSDISTUDY_BS[order(tmpSDISTUDY_BS[,2]),][1,]
  tmpSDISTUDY_MS <- as.matrix(abs(colMeans(tmpSDISTUDY[, c(20:30)], na.rm=T)))
  tmpSDISTUDY_MS <- cbind.data.frame(name=as.matrix(rownames(tmpSDISTUDY_MS)), value=tmpSDISTUDY_MS)
  tmpSDISTUDY_MS <- tmpSDISTUDY_MS[order(tmpSDISTUDY_MS[,2]),][1,]                           
  tmpSDISTUDY_CV <- as.matrix(abs(colMeans(tmpSDISTUDY[, c(31:41)], na.rm=T)))
  tmpSDISTUDY_CV <- cbind.data.frame(name=as.matrix(rownames(tmpSDISTUDY_CV)), value=tmpSDISTUDY_CV)
  tmpSDISTUDY_CV <- tmpSDISTUDY_CV[order(abs(0.95-tmpSDISTUDY_CV[,2])),][1,]    
  
  tmpBRSTUDY <- subset(BRSTUDY, BRSTUDY$Group==g)
  tmpBRSTUDY_BS <- as.matrix(abs(colMeans(tmpBRSTUDY[, c(9:19)], na.rm=T)))
  tmpBRSTUDY_BS <- cbind.data.frame(name=as.matrix(rownames(tmpBRSTUDY_BS)), value=tmpBRSTUDY_BS)
  tmpBRSTUDY_BS <- tmpBRSTUDY_BS[order(tmpBRSTUDY_BS[,2]),][1,]
  tmpBRSTUDY_MS <- as.matrix(abs(colMeans(tmpBRSTUDY[, c(20:30)], na.rm=T)))
  tmpBRSTUDY_MS <- cbind.data.frame(name=as.matrix(rownames(tmpBRSTUDY_MS)), value=tmpBRSTUDY_MS)
  tmpBRSTUDY_MS <- tmpBRSTUDY_MS[order(tmpBRSTUDY_MS[,2]),][1,]                           
  tmpBRSTUDY_CV <- as.matrix(abs(colMeans(tmpBRSTUDY[, c(31:41)], na.rm=T)))
  tmpBRSTUDY_CV <- cbind.data.frame(name=as.matrix(rownames(tmpBRSTUDY_CV)), value=tmpBRSTUDY_CV)
  tmpBRSTUDY_CV <- tmpBRSTUDY_CV[order(abs(0.95-tmpBRSTUDY_CV[,2])),][1,]   
  
  tmpARSTUDY <- subset(ARSTUDY, ARSTUDY$Group==g)
  tmpARSTUDY_BS <- as.matrix(abs(colMeans(tmpARSTUDY[, c(9:19)], na.rm=T)))
  tmpARSTUDY_BS <- cbind.data.frame(name=as.matrix(rownames(tmpARSTUDY_BS)), value=tmpARSTUDY_BS)
  tmpARSTUDY_BS <- tmpARSTUDY_BS[order(tmpARSTUDY_BS[,2]),][1,]
  tmpARSTUDY_MS <- as.matrix(abs(colMeans(tmpARSTUDY[, c(20:30)], na.rm=T)))
  tmpARSTUDY_MS <- cbind.data.frame(name=as.matrix(rownames(tmpARSTUDY_MS)), value=tmpARSTUDY_MS)
  tmpARSTUDY_MS <- tmpARSTUDY_MS[order(tmpARSTUDY_MS[,2]),][1,]                           
  tmpARSTUDY_CV <- as.matrix(abs(colMeans(tmpARSTUDY[, c(31:41)], na.rm=T)))
  tmpARSTUDY_CV <- cbind.data.frame(name=as.matrix(rownames(tmpARSTUDY_CV)), value=tmpARSTUDY_CV)
  tmpARSTUDY_CV <- tmpARSTUDY_CV[order(abs(0.95-tmpARSTUDY_CV[,2])),][1,]    

  subBias <- rbind.data.frame(tmpCarterSTUDY_BS, tmpSDISTUDY_BS, tmpBRSTUDY_BS, tmpARSTUDY_BS)
  rownames(subBias) <- c("Carteretal","SDI","BR","AR")
  colnames(subBias) <- paste0(c("Estimator_","Value_"),g)
  if(i==1){
    BiasForEachStudy <- subBias
  }else{
    BiasForEachStudy <- cbind.data.frame(BiasForEachStudy, subBias)
  }
  
  subMSE <- rbind.data.frame(tmpCarterSTUDY_MS, tmpSDISTUDY_MS, tmpBRSTUDY_MS, tmpARSTUDY_MS)
  rownames(subMSE) <- c("Carteretal","SDI","BR","AR")
  colnames(subMSE) <- paste0(c("Estimator_","Value_"),g)
  if(i==1){
    MSEForEachStudy <- subMSE
  }else{
    MSEForEachStudy <- cbind.data.frame(MSEForEachStudy, subMSE)
  }
  
  subCOV <- rbind.data.frame(tmpCarterSTUDY_CV, tmpSDISTUDY_CV, tmpBRSTUDY_CV, tmpARSTUDY_CV)
  rownames(subCOV) <- c("Carteretal","SDI","BR","AR")
  colnames(subCOV) <- paste0(c("Estimator_","Value_"),g)
  if(i==1){
    COVForEachStudy <- subCOV
  }else{
    COVForEachStudy <- cbind.data.frame(COVForEachStudy, subCOV)
  }
}
write.csv(BiasForEachStudy,"MakingTables/Table07aBias.csv", row.names=TRUE)
write.csv(MSEForEachStudy,"MakingTables/Table07bMSE.csv", row.names=TRUE)
write.csv(COVForEachStudy,"MakingTables/Table07cCov.csv", row.names=TRUE)



#################
# Table 8
#################
ARSTUDY <- subset(ResultsCombined, ResultsCombined$Study=="AR")
RESIG <- subset(ARSTUDY, (ARSTUDY$Type=="RE" & ARSTUDY$BiasII=="Sig"))[, c(2,4,5,7,8,9:19)]
PRESIG <- subset(ARSTUDY, (ARSTUDY$Type=="PRE" & ARSTUDY$BiasII=="Sig")) [, c(2,4,5,7,8,9:19)]
REPOS <- subset(ARSTUDY, (ARSTUDY$Type=="RE" & ARSTUDY$BiasII=="Pos"))[, c(2,4,5,7,8,9:19)]
PREPOS <- subset(ARSTUDY, (ARSTUDY$Type=="PRE" & ARSTUDY$BiasII=="Pos"))[, c(2,4,5,7,8,9:19)]
TB08<-rbind.data.frame(RESIG, PRESIG, REPOS, PREPOS)
TB08$Size<-TB08$Size/10
write.csv(TB08, "MakingTables/Table08.csv", row.names=FALSE)


#################
# Table 9
#################
ARSTUDY <- subset(ResultsCombined, ResultsCombined$Study=="AR")
RESIG <- subset(ARSTUDY, (ARSTUDY$Type=="RE" & ARSTUDY$BiasII=="Sig"))[, c(2,4,5,7,8,20:30)]
PRESIG <- subset(ARSTUDY, (ARSTUDY$Type=="PRE" & ARSTUDY$BiasII=="Sig")) [, c(2,4,5,7,8,20:30)]
REPOS <- subset(ARSTUDY, (ARSTUDY$Type=="RE" & ARSTUDY$BiasII=="Pos"))[, c(2,4,5,7,8,20:30)]
PREPOS <- subset(ARSTUDY, (ARSTUDY$Type=="PRE" & ARSTUDY$BiasII=="Pos"))[, c(2,4,5,7,8,20:30)]
TB09<-rbind.data.frame(RESIG, PRESIG, REPOS, PREPOS)
TB09$Size<-TB09$Size/10
write.csv(TB09, "MakingTables/Table09.csv", row.names=FALSE)



#################
# Table 10
#################
ARSTUDY <- subset(ResultsCombined, ResultsCombined$Study=="AR")
RESIG <- subset(ARSTUDY, (ARSTUDY$Type=="RE" & ARSTUDY$BiasII=="Sig"))[, c(2,4,5,7,8,31:41)]
PRESIG <- subset(ARSTUDY, (ARSTUDY$Type=="PRE" & ARSTUDY$BiasII=="Sig")) [, c(2,4,5,7,8,31:41)]
REPOS <- subset(ARSTUDY, (ARSTUDY$Type=="RE" & ARSTUDY$BiasII=="Pos"))[, c(2,4,5,7,8,31:41)]
PREPOS <- subset(ARSTUDY, (ARSTUDY$Type=="PRE" & ARSTUDY$BiasII=="Pos"))[, c(2,4,5,7,8,31:41)]
TB10<-rbind.data.frame(RESIG, PRESIG, REPOS, PREPOS)
TB10$Size<-TB10$Size/10
write.csv(TB10, "MakingTables/Table10.csv", row.names=FALSE)


