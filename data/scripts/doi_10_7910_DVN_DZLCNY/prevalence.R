# compute and save prevalence of terrorism in each region (useful for plot and confusion matrix computation)
# first we compute prevalence on the entire dataset (training and testing)
preval <-list()
evalpath <- paste0(getwd(),"/results/evaluation/")
for (i in 1:13)
{
  preval[[i]] <- read.csv(paste0('results/featextract/terror_feature_extraction',i,'.csv')) %>%
  dplyr::filter(!is.na(country))
  positives <- nrow(preval[[i]][preval[[i]]$terror==1,])
  mytotal <- nrow(preval[[i]])
  prevalence <- positives/mytotal*100 #prevalence in percent
  preval[[i]] <- data.frame(positives=positives,total=mytotal, prevalence=prevalence)
  preval[[i]]$region <- regionnames[[i]]
  names(preval[[i]]) <- c("positives","total", "prevalence", 'region')
}
mypreval <- do.call(rbind,preval)

# define threshold values adapted to each region
prev1 <- mypreval[mypreval$prevalence < 0.03,]
prev1$thres <- 0.75
prev2 <- mypreval[(mypreval$prevalence >= 0.03 & mypreval$prevalence < 0.2),]
prev2$thres <- 0.5
prev3 <- mypreval[(mypreval$prevalence >= 0.2),]
prev3$thres <- 0.2
mypreval <- rbind(prev1,prev2,prev3)

mypreval <- mypreval[order(mypreval$region), ]
save(mypreval, file=paste0(evalpath, 'prevalence.Rdata'))
#save as latex object
myprevlatex <-knitr::kable(mypreval,format = "latex",row.names = FALSE,digits = c(0,0,3,0,2))
kableExtra::save_kable(myprevlatex, paste0(evalpath,"prevalence.tex"),  keep_tex = TRUE)
allpositives <- sum(mypreval$positives)
allobs <- sum(mypreval$total)
allpositives/allobs*100

#second we compute prevalence from the training sample only
prevalt <-list()
for (i in 1:13)
{
  test <- readRDS(paste0('results/xgb/xgb-until-2016_',i,'.Rds')) 
  test <-test$trainingData
  positives <- nrow(test[test$.outcome=="yes",])
  mytotal <- nrow(test)
  prevalence <- positives/mytotal*100 #prevalence in percent
  prevalence <- data.frame(positives=positives,total=mytotal, prevalence=prevalence)
  prevalence$region <- regionnames[[i]]
  names(prevalence) <- c("positives","total", "prevalence", 'region')
  prevalt[[i]] <- prevalence
  }
myprevalt <- do.call(rbind,prevalt)
##define threshold values adapted to each region
prev1 <- myprevalt[myprevalt$prevalence < 0.03,]
prev1$thres <- 0.75
prev2 <- myprevalt[(myprevalt$prevalence >= 0.03 & myprevalt$prevalence < 0.2),]
prev2$thres <- 0.5
prev3 <- myprevalt[(myprevalt$prevalence >= 0.2),]
prev3$thres <- 0.2
myprevalt <- rbind(prev1,prev2,prev3)
myprevalt <- myprevalt[order(myprevalt$region), ]
save(myprevalt, file=paste0(evalpath, 'prevalencetraining.Rdata'))
#save as latex object
myprevtlatex <-knitr::kable(myprevalt,format = "latex",row.names = FALSE,digits = c(0,0,3,0,2))
kableExtra::save_kable(myprevtlatex ,paste0(evalpath,"prevalencetraining.tex"),  keep_tex = TRUE)
allpositives <- sum(myprevalt$positives)
allobs <- sum(myprevalt$total)
allpositives/allobs*100
#END
