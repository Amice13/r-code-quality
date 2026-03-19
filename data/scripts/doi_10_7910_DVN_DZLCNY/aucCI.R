#Aim: compute AUROC, AUPRC and other predictive performance metrics 

list.of.packages <- c("readr","precrec","palettetown", "ggthemes", "ggplot2","ggpubr","knitr",
                      "kableExtra","mltools","MLmetrics")#,"s2dverification"
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos = "http://cran.us.r-project.org")
lapply(list.of.packages, library, character.only = TRUE)

#evaluation file paths
evalpath <- paste0(getwd(),"/results/evaluation/")
modelnames <- c("AR1","AR1X","AR2","GAM","RF","XGB")
#check prevalence of terrorism (percentage of positive week-cells) for each region
regionnames <- c("A", "B","C","K","L","J", "I","D","H","E","G","F","M")
#A NAm, B CAm, C SAm, K EAs, L SEAs, J SAs, I CAs, D: EU, H Rus, E: MENA, G SubAf, F WAf, M: Oce
#names for plots with correct order A-M
regionAM <- c("A", "B","C","D","E","F","G","H","I","J","K","L","M")
nbAM <- c(1,2,3,8,10,12,11,9,7,6,4,5,13)

#load prevalence data
load(paste0(evalpath,'prevalencetraining.Rdata'))

#plot themes
sizeline <- 1 # size of line 
mytheme <-    
  ggplot2::theme(plot.margin = unit(c(0.1, 0.05, 0, 0.05), "cm"),
                 plot.title = element_blank(),
                 axis.title.x = element_blank(),
                 axis.title.y = element_blank(),
                 legend.title = element_blank(),
                 legend.position = "none",
                 line = element_line(size=sizeline),
                 aspect.ratio=1)

mythemelegend <-    
  ggplot2::theme(plot.margin = unit(c(0.1, 0.05, 0, 0.05), "cm"),
                 plot.title = element_blank(),
                 legend.title = element_blank(),
                 legend.position = "none",
                 line = element_line(size=sizeline),
                 aspect.ratio=1)
#ROC and PR-curve plots and AUC
#loop over all regions
rocl <- prcl <- aucl <- list()
aucals <- list() 

for (mm in 1:length(regionnames)) {
  #make sure that the file names are properly saved (with _ before region number)
  f2 <- list.files(evalpath, full.names = TRUE, pattern = paste0('_',mm,'\\.csv$'))
  f2 <- f2[!grepl('extraction', f2)]
  predreg <- lapply(f2, readr::read_csv)
  # #rename for plot
  predreg[[1]]$method <- modelnames[1]
  predreg[[2]]$method <- modelnames[2]
  predreg[[3]]$method <- modelnames[3]
  predreg[[4]]$method <- modelnames[4]
  predreg[[5]]$method <- modelnames[5]
  predreg[[6]]$method <- modelnames[6]
  predreg <- do.call(rbind, predreg)
  predreg$modnames <-predreg$method 
  
  for (tt in 1:5){#5 testing samples (1-5)
    
  preds<- predreg[predreg$test_iteration==tt,]

  #join scores and labels for each method
  score <- label <- list()
  for (i in 1:length(modelnames))
  {
    pred <- preds[preds$modnames == modelnames[i],]
    score[[i]] <- pred$prediction
    label[[i]] <- pred$terror
    #change one value in Australia to create plot (all cells were only negatives)
    if(!anyNA(label[[i]]) & all(label[[i]] == label[[i]][1])){
      label[[i]][1] <- 1
    }
  }
  myscores <- precrec::join_scores(score)
  mylabels <- precrec::join_labels(label)
  mmdat <- precrec::mmdata(myscores,mylabels,modnames = modelnames,dsids = c(1, 2,3,4,5,6))
  mscurves <- precrec::evalmod(mmdat,cb_alpha = 0.05)
  # Calculate CI of AUCs with normal distribution
  myauc <- precrec::auc_ci(mscurves)
  myauc$error <- myauc$n <- NULL
  myauc$region <- as.factor(regionnames[mm])
  names(myauc) <- c("Model", "Metric", "mean", "lower 95% CI","upper 95% CI", "Region")
  myauc$Model <- as.factor(myauc$Model)
  myauc <- myauc[c("Model", "Metric", "mean", "Region")]#CI not needed (all are very tight)
  roc <- myauc[myauc$Metric=="ROC",] 
  roc <-roc[c("Model", "mean", "Region")]
  names(roc) <- c("Model", "AUROC", "Region")
  prc <- myauc[myauc$Metric=="PRC",]
  prc <-prc[c("Model", "mean", "Region")]
  names(prc) <- c("Model", "AUPRC", "Region")
  myauc <- merge(roc,prc,by=c("Region","Model"))
  aucl[[tt]] <- myauc[order(myauc$Region, myauc$Model), ]
}
  aucall <- do.call(rbind,aucl)
  aucall$AUPRC <- aucall$AUPRC *100
  aucall$AUROC <- aucall$AUROC *100
  aucall$iter <- tt
  names(aucall) <- c("Region","Model", "AUROC (%)", "AUPRC (%)",'iter')
  aucals[[mm]] <- aucall
}
aucall <- do.call(rbind,aucals)

library(dplyr)
aucall$iter <- NULL
aucall <-  aucall %>%
  dplyr::group_by(Region, Model) %>%
  dplyr::summarise(dplyr::across(
    .cols = where(is.numeric), 
    .fns = list(Mean = mean, SD = sd), na.rm = TRUE, 
    .names = "{col}_{fn}"
  ))
aucall <- data.frame(aucall)
############################################################

#confusion matrix for each model and region
#generate a confusion matrix for each model/region
modnames <- c("ar1","ar1_extra","ar2", "gam","rf","xgb")
myexp <- c('myprevalt','evalpath', 'modelnames','regionnames',
           'regionAM', 'nbAM')


# #run the parallel loop with foreach #############################
lspack <- c("dplyr", "ggplot2","mgcv", "caret","purrr","plotROC","doSNOW", "parallel", "ranger", "xgboost", "ggpubr",
            "readr","precrec","palettetown", "ggthemes", "knitr","kableExtra")#"pkgbuild","devtools", needs dplyr 1.0.0
new.packages <- lspack[!(lspack %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos = "http://cran.us.r-project.org")
lapply(lspack, library, character.only = TRUE)
#define cores
cores <- min(6,nbcores)                                                
cl <- makeCluster(cores[1])                                   
doSNOW::registerDoSNOW(cl)                                             
#total of 6 models, choose below when to start and end the loop
#############################################################
foreach(i=1:6, .packages=list.of.packages, .export = myexp)  %dopar% {
mymodel <- modelnames[[i]]
for (mm in 1:length(regionnames)){
  for (tt in 1:5){
eval_dfall <- read.csv(paste0("results/evaluation/eval-df-",modnames[[i]], "_",mm,".csv"))
eval_df <- eval_dfall[eval_dfall$test_iteration == tt,] 

if (mymodel == "AR1" | mymodel == "AR1X" | mymodel == "AR2" ) {
  eval_df$prediction <- eval_df$prediction + rnorm(nrow(eval_df), 0, 0.000001)
}
if (mymodel == "GAM") {
  eval_df$prediction <- boot::inv.logit(eval_df$prediction)
}

if (mymodel == "RF") { #rescale to mitigate bad calibration of RF model
    eval_df$prediction <- scales::rescale(eval_df$prediction, to = c(0.000001,0.1))
  }
#############################################################

myregion <- regionnames[[mm]]
#define threshold for confusion matrices and plot
falsenegrate <- myprevalt[myprevalt$region==regionnames[[mm]],]
falsenegrate <- falsenegrate$thres
#number of true positive cases
pred <-eval_df[complete.cases(eval_df),]
#small issues in Australia where all cells might be only negatives
if(!anyNA(pred$terror) & all(pred$terror == pred$terror[1])){
  pred$terror[1]  <- 1
}
truepos <- pred[pred$terror>0,]
########################IMPORTANT OPTION THRESHOLD FALSE NEGATIVES###################
prevregion <- myprevalt[myprevalt$region==regionnames[[mm]],]
fnthres <- prevregion$thres
myprev <- prevregion$prevalence/100
########################IMPORTANT OPTION THRESHOLD FALSE NEGATIVES###################

#get threshold value of prediction based on percentile value
#compute thresholds
thresholds <- seq(min(pred$prediction,na.rm=TRUE), max(pred$prediction,na.rm=TRUE), length.out = 10000)
false_negative <- rep(NA, length(thresholds))
truth <- pred$terror == 1
fn <- function(aa, bb){
  sum(!aa & bb) / sum(bb)
}
calc_fn <- function(t, eval_df, truth){
  pred      <- eval_df$prediction > t
  fn(pred, truth)
}
#Run in parallel if Mac, Linux, Unix, BSD Machine
if(Sys.info()["sysname"] != 'Windows'){#required to run on linux
  false_negative <- mclapply(thresholds, calc_fn, eval_df = pred, truth = truth, mc.cores = nbcores)
} else {
  false_negative <- lapply(thresholds, calc_fn, eval_df = pred, truth = truth)
}
false_negative <- simplify2array(false_negative)
#check plot
#plot(false_negative ~ thresholds, type = 'l')

#small issues in Australia where all cells might be only negatives
if(!anyNA(false_negative) & all(false_negative == false_negative[1])){
  false_negative[1]  <- 0
}
thres <- max(thresholds[false_negative < fnthres])

###########evaluation based on raw predictive values############################
# #Brier score 
BS <- sum((pred$terror - pred$prediction)^2,na.rm=TRUE)/length(pred$terror)
BSref <- sum((pred$terror - myprev)^2,na.rm=TRUE)/length(pred$terror)
BSS <- 1 - BS/BSref
###########evaluation based on categorical predictions (threshold)##############
pred$pred <- ifelse(pred$prediction>thres, 1, 0)
#Matthew's correlation
MCC <- mltools::mcc(pred$pred, pred$terror)
#F1 score
pred$terror <- as.factor(pred$terror)
F1 <- MLmetrics::F1_Score(pred$pred,pred$terror, positive = "1")
################################################################################

#confusion matrices
pred$pred <- as.factor(pred$pred)
cm <- caret::confusionMatrix(pred$pred, pred$terror,prevalence = myprev,positive="1")
cm <- data.frame(cm$table)
totevts <- sum(cm$Freq,na.rm=TRUE)
totneg <- cm[cm$Reference == "0",]
totneg <- sum(totneg$Freq,na.rm=TRUE)
totpos <- cm[cm$Reference == "1",]
totpos <- sum(totpos$Freq,na.rm=TRUE)
totevts <- c(totneg, totneg,totpos,totpos)
cm$prop <- round(cm$Freq / totevts *100,1)
cm$fn <- falsenegrate*100
cm$prev <- myprev*100
cm$Region <- myregion
cm$Model <- mymodel
cm$BSS <- BSS *100
cm$F1 <- F1 *100
cm$MCC <- MCC *100
names(cm)<- c("Pred.", "Obs.","Freq.", "Prop.obs (%)","Target false neg.(%)", "terror preval.(%)", "Region", "Model","BSS (%)","F1 (%)", "MCC (%)")
Type<- c("TN","FP", "FN", "TP")
cm  <- cbind(Type,cm)
cm$iter <- tt
save(cm,file=paste0('results/confusion_matrices_CI/cm_',mymodel,'_', mm,'iter',tt, '.Rdata'))
  }}
}
stopCluster(cl)
##############end cluster

#load cms, aggregate it as a table, and save it as latex format
cmpath <- paste0(getwd(),"/results/confusion_matrices_CI/")
cmf <- list.files(cmpath, full.names = TRUE, pattern = paste0("*.Rdata$"))
cml <- list()
for (i in 1:length(cmf)){
load(cmf[[i]])
  cml[[i]]<- cm
}
cmall<- do.call(rbind, cml)
cmall <- cmall[order(cmall$Region, cmall$Model,cmall$iter), ]
#confusion matrix
cm <- cmall[c("Region","Model","Type", "Pred.", "Obs.","Freq.", "Prop.obs (%)","Target false neg.(%)", "terror preval.(%)","iter")]
#predictive performance (scores)
sc <- cmall[c("Region", "Model","terror preval.(%)","BSS (%)","F1 (%)", "MCC (%)", "iter")]
sc <- unique(sc) 

#merge score with AUC summary (AUROC, AUPRC)
names(sc) <- c("Region", "Model","AUPRC baseline (%)","BSS (%)","F1 (%)", "MCC (%)", 'iter')

#compute mean and sd for all scores
library(dplyr)
sc$iter <- NULL
sc<-sc %>%
  dplyr::group_by(Region, Model) %>%
  dplyr::summarise(dplyr::across(
    .cols = where(is.numeric), 
    .fns = list(Mean = mean, SD = sd), na.rm = TRUE, 
    .names = "{col}_{fn}"
  ))
sc <- data.frame(sc)
#merge aucall with sc
myscore <- merge(sc,aucall,by=c("Region","Model"))
names(myscore) <- c("Region", "Model","AUPRCbasm","AUPRCbassd",
                    "BSSm","BSSsd","F1m","F1sd","MCCm","MCCsd","AUROCm","AUROCsd",
                    "AUPRCm","AUPRCsd")
myscore <- myscore[c("Region", "Model","AUROCm","AUROCsd","AUPRCm","AUPRCsd","AUPRCbasm",
                     "F1m","F1sd","MCCm","MCCsd","BSSm","BSSsd")]

#save score metrics and confusion matrices as latex file
scdigits <- c(0,0,1,1,2,2,3,2,2,2,2,2,2)

knitr::kable(myscore,format = "html",digits = scdigits,booktabs = T,row.names = FALSE) %>% kable_classic(full_width = F)
auclatex <-knitr::kable(myscore,format = "latex",digits = scdigits,row.names = FALSE,booktabs = T,
                        
                        linesep = c(rep("",5), "\\addlinespace")) 
kableExtra::save_kable(auclatex ,paste0(evalpath,"auctableCI.tex"),  keep_tex = TRUE)
#END
