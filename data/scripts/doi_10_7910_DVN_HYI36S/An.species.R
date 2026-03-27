
library(biomod2)
library(terra)
library(doParallel)
library(tidyterra)
library(base)

#读取发生记录
setwd("G:/anwen/csv/") ##读文件夹
DataSpecies <- read.csv("1.csv") ###数据点文件
head(DataSpecies)
myRespName <- 'Aggambiae'
myResp.PA <- as.numeric(DataSpecies[, myRespName])
myRespXY <- DataSpecies[, c("decimalLon","decimalLat")]


#读取环境因子
nameExpl <- gtools::mixedsort(c(
  list.files("G:/anwen/5currentpredictors",pattern = "*.tif$",full.names = TRUE)
))
nameExpl
myExpl <- rast(nameExpl[c(1,3,5,7,14,18,19,20,21,22,23,24,25,26,27,28,29,30)])   ###选择需要的变量
##当变量中有重复或名称不准确时用以下代码修改变量名
#names(myExpl)
#names(myExpl)[14]<-"pn9"
#names(myExpl)[15]<-"pp12"
#names(myExpl)
terra::crs(myExpl, proj = TRUE)

#设置路径，格式化数据
setwd("G:/anwen/biomod2/1/")
myBiomodData <- BIOMOD_FormatingData(resp.var = myResp.PA,
                                     expl.var = myExpl,
                                     resp.xy = myRespXY,
                                     resp.name = myRespName,
                                     PA.nb.rep = 3,
                                     PA.nb.absences = 6023,
                                     PA.strategy = 'random')

PApoints <- cbind(1:length(myBiomodData@data.species),myBiomodData@data.species,myBiomodData@coord)
colnames(PApoints)[1:2] <- c("plot ID", "observed values")
PApoints[is.na(PApoints)] <- 0
write.csv(PApoints,"PApoints.csv")

#模型默认设置
bm_DefaultModelingOptions()
myBiomodOptions <- BIOMOD_ModelingOptions()

#模型验证方式，k-fold 和分层交叉验证,加入其他验证方式参考"?bm_CrossValidation"
# k-fold selection
k.rn = 3
k.kn = 3
cv.k <- bm_CrossValidation(bm.format = myBiomodData,
                           strategy = "kfold",
                           nb.rep = k.rn,
                           k = k.kn)

# stratified selection (geographic)
s.kn = 3
cv.s <- bm_CrossValidation(bm.format = myBiomodData,
                           strategy = "strat",
                           k = s.kn,
                           balance = "presences",
                           strat = "x")
head(cv.k)
head(cv.s)
cv.sNames <- c()
for (i in 1:length(myBiomodData@PA.table[1,])) {
  for (j in (k.rn*k.kn+1):(k.rn*k.kn + s.kn)) {
    name <- paste0("_PA",i,"_RUN",j)
    cv.sNames <- c(cv.sNames,name)
  }
}
colnames(cv.s) <- cv.sNames
myBiomodCV <- cbind(cv.k,cv.s)
myBiomodCV <- myBiomodCV[,order(colnames(myBiomodCV))]
gc()

#单模型设置，如果只有一个MAXENT模型，不要在路径下放置maxent.jar，有两个就放置
myBiomodModelOut <- BIOMOD_Modeling(bm.format = myBiomodData,
                                    bm.options = myBiomodOptions,
                                    modeling.id = 'AllModels',
                                    models = c("GLM", "GBM", "CTA", "ANN", "FDA", "MARS", "RF", "MAXENT",
                                               "MAXNET", "XGBOOST"),
                                    CV.perc = 0.7,
                                    CV.user.table = myBiomodCV,
                                    var.import = 3,
                                    metric.eval = c("KAPPA",'TSS','ROC'),
                                    do.full.models = FALSE,
                                    seed.val = 123,
                                    nb.cpu = 8)
myBiomodModelOut
gc()



#输出模型评价指标、因子重要值、响应曲线数值(以平均值计算的(mean)，可选择其他值)
#输出模型评价指标、因子重要值、响应曲线数值(以平均值计算的(mean)，可选择其他值)
write.csv(get_evaluations(myBiomodModelOut)[,c(1,4,5,9,10)], file = "singleMod_myBiomodModelEval.csv")

write.csv(aggregate(get_evaluations(myBiomodModelOut)[,9],list(get_evaluations(myBiomodModelOut)[,4], 
                                                               get_evaluations(myBiomodModelOut)[,5]),mean),
          file = "singleMod_myBiomodModelEval_Mean.csv")

write.csv(get_variables_importance(myBiomodModelOut)[,c(1,4,5,7)], file = "singleMod_varimportan.csv")

responseCurve <- bm_PlotResponseCurves(bm.out = myBiomodModelOut, 
                                       models.chosen = get_built_models(myBiomodModelOut),
                                       fixed.var = 'mean')

write.csv(responseCurve[["tab"]][,c(2,3,5)], file = "singleMod_Response_curve.csv")
###制作响应曲线的数据文件

#画出评价指标、重要值等
#bm_PlotEvalMean(bm.out = myBiomodModelOut, metric.eval = c("TSS","ROC"))
#bm_PlotEvalBoxplot(bm.out = myBiomodModelOut, group.by = c('algo', 'algo'))
#bm_PlotEvalBoxplot(bm.out = myBiomodModelOut, group.by = c('algo', 'run'))
#bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut, group.by = c('expl.var', 'algo', 'algo'))
#bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut, group.by = c('expl.var', 'algo', 'PA'))
#bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut, group.by = c('algo', 'expl.var', 'PA'))


#集成模型设置,这里只进行加权平均预测，
#如需平均值、中位数、变异系数等，将以下FALSE设置为TRUE
myBiomodEM <- BIOMOD_EnsembleModeling(bm.mod = myBiomodModelOut,
                                      models.chosen = 'all',
                                      em.by = 'all',
                                      em.algo = c('EMwmean'),
                                      metric.select = c("TSS", "ROC"),
                                      metric.select.thresh = c(0.7,0.8),
                                      metric.eval = c("KAPPA",'TSS', 'ROC'),
                                      var.import = 3,
                                      EMci.alpha = 0.05,
                                      EMwmean.decay = 'proportional',
                                      nb.cpu = 8,
                                      seed.val = 123)
myBiomodEM
gc()

#输出集成模型评价指标、因子重要值、响应曲线数据(以平均值计算的(mean)，可选择其他值)
write.csv(get_evaluations(myBiomodEM)[,c(1,6,7,11)], file = "ensembleMod_myBiomodModelEval.csv")

write.csv(aggregate(get_evaluations(myBiomodEM)[,11], list(get_evaluations(myBiomodEM)[,7]),mean), 
          file = "ensembleMod_EI_Mean.csv")

write.csv(get_variables_importance(myBiomodEM)[,c(1,7,9)], file = "ensembleMod_vI.csv")

varimportan_Mean <- aggregate(get_variables_importance(myBiomodEM)[,9], 
                              list(get_variables_importance(myBiomodEM)[,7]),mean)

write.csv(varimportan_Mean[order(varimportan_Mean[,2],decreasing = TRUE),], file = "ensembleMod_vI_Mean.csv")
rm(varimportan_Mean)

responseCurveEM <- bm_PlotResponseCurves(bm.out = myBiomodEM, 
                                         models.chosen = get_built_models(myBiomodEM),
                                         fixed.var = 'mean')

write.csv(responseCurveEM[["tab"]][,c(2,3,5)], file = "ensembleMod_Response_curve.csv")

#画出集成评价指标、重要值等
#bm_PlotEvalMean(bm.out = myBiomodEM, group.by = 'algo')
#bm_PlotEvalBoxplot(bm.out = myBiomodEM, group.by = c('merged.by.PA', 'algo'))
#bm_PlotVarImpBoxplot(bm.out = myBiomodEM, group.by = c('expl.var', 'algo', 'merged.by.run'))
#bm_PlotVarImpBoxplot(bm.out = myBiomodEM, group.by = c('expl.var', 'algo', 'full.name'))
#bm_PlotVarImpBoxplot(bm.out = myBiomodEM, group.by = c('algo', 'expl.var', 'merged.by.PA'))


#用Boyce指数和MPA评估模型
myBiomodPO <- BIOMOD_PresenceOnly(bm.mod = myBiomodModelOut,
                                  bm.em = myBiomodEM)


# #将单个模型映射在环境上，以显示分布概率，current
# myBiomodProj <- BIOMOD_Projection(bm.mod = myBiomodModelOut,
#                                   proj.name = 'Current',
#                                   new.env = myExpl,
#                                   models.chosen = 'all',
#                                   metric.binary = 'all',
#                                   metric.filter = 'all',
#                                   build.clamping.mask = TRUE,
#                                   nb.cpu = 8,
#                                   seed.val = 123)

# myBiomodProj
# plot(myBiomodProj)


#将集成模型映射在环境上，以显示分布概率，current
myBiomodEMProj <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM,
                                             proj.name = 'CurrentEM',
                                             new.env = myExpl,
                                             models.chosen = 'all',
                                             metric.binary = 'all',
                                             metric.filter = 'all',
                                             nb.cpu = 8,
                                             seed.val = 123)
gc()

#输出tif
allTif <- rast(list.files(paste0(myBiomodEMProj@sp.name,"/","proj_",myBiomodEMProj@proj.name),
                          pattern = "*ensemble.tif$",full.names = TRUE))
#读取wmean的结果平均TSS、ROC、KAPPA，如平均全部结果直接到“写出”这一部分
wmean <- which(grepl("EMwmean",names(allTif)))
wmeanTif <- allTif[[wmean]]
#写出，写出全部的平均结果将wmeanTif修改为allTif
writeRaster(mean(wmeanTif)/1000,"G:/anwen/biomod2/1/1currentwmeanResult.tif", 
            overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES"), datatype='FLT4S')


#读取其他(future)环境变量
nameExplFuture <- gtools::mixedsort(c(
  list.files("G:/anwen/5futurepredictors/f126",pattern = "*.tif$",full.names = TRUE)
))
nameExplFuture
myExplFuture <- rast(nameExplFuture[c(1,8,11,13,14,18,20,21,23,24,25,26,27,28,29,30,31)])  ###选择与current相应的变量
terra::crs(myExplFuture, proj = TRUE)

# #将单个模型映射在环境上，以显示分布概率，other(future)
# myBiomodProj <- BIOMOD_Projection(bm.mod = myBiomodModelOut,
#                                   proj.name = 'Future',
#                                   new.env = myExplFuture,
#                                   models.chosen = 'all',
#                                   metric.binary = 'all',
#                                   metric.filter = 'all',
#                                   build.clamping.mask = TRUE,
#                                   nb.cpu = 8,
#                                   seed.val = 123)

# myBiomodProj
# plot(myBiomodProj)


#将集成模型映射在环境上，以显示分布概率，other(future)
myBiomodEMProjFuture <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM,
                                                   proj.name = 'FutureEM',
                                                   new.env = myExplFuture,
                                                   models.chosen = 'all',
                                                   metric.binary = 'all',
                                                   metric.filter = 'all',
                                                   nb.cpu = 8,
                                                   seed.val = 123)
gc()

#写出tif
allTif <- rast(list.files(paste0(myBiomodEMProjFuture@sp.name,"/","proj_",myBiomodEMProjFuture@proj.name),
                          pattern = "*ensemble.tif$",full.names = TRUE))
#读取wmean的结果平均TSS、ROC、KAPPA，如平均全部结果直接到“写出”这一部分
wmean <- which(grepl("EMwmean",names(allTif)))
wmeanTif <- allTif[[wmean]]
#写出，写出全部的平均结果将wmeanTif修改为allTif
writeRaster(mean(wmeanTif)/1000,"G:/anwen/biomod2/1/1wmeanResultf26.tif", 
            overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES"), datatype='FLT4S')

