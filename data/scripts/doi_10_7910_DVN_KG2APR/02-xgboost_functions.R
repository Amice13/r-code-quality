
#====================================================================
# Functions for logistic and multinomial classifiers
# Authors:     Code originally written by Pablo Barbera (Last update: 2017/08/16)
# Maintainers: updated by: Anita Gohdes 
#====================================================================


############## TEXT ANALYSIS FUNCTIONS ##################

create_test_dfm <- function(dfm, corpus){

  # create new DFM based on corpus
  new.dfm <- dfm(corpus, ngrams=1:2, remove_punct=TRUE, 
    remove_url=TRUE, verbose=TRUE)
  # keep only features in original DFM
  new.dfm <- new.dfm[,featnames(new.dfm) %in% featnames(dfm)]
  # join together to add features in original DFM but NOT here
  new.dfm <- rbind(dfm, new.dfm)
  new.dfm <- new.dfm[,order(featnames(new.dfm))]
  # keep only new DFM
  new.dfm <- new.dfm[ (ndoc(dfm)+1) : ndoc(new.dfm)]

  return(new.dfm)
}


############## PERFORMANCE METRICS ##################

accuracy <- function(ypred, y){
    return(
      sum(ypred==y)/length(y)
      )
}

precision <- function(ypred, y){
    tab <- table(ypred, y)
    return(
        sum((ypred==1)&(y==1)) /
        (sum((ypred==1)&(y==0)) + sum((ypred==1)&(y==1)))
        )
}

recall <- function(ypred, y){
    tab <- table(ypred, y)
    return(
        sum((ypred==1)&(y==1)) /
        (sum((ypred==0)&(y==1)) + sum((ypred==1)&(y==1)))
        )
}



############## CLASSIFIERS ##################
# global parameters
tryEta <- c(0.5,1,2)
tryDepths <- c(1,2,3)
nround <- 1000
print_every_n <- 200L
nround_long <- 2000
print_every_n_long <- 500L

multinomial_classifier <- function(X, y, labels, n.cats=length(unique(labels)), seed=123){

    # choosing training and test sets
    set.seed(seed)
    training <- sample(1:nrow(d), floor(.80 * nrow(d)))
    test <- (1:nrow(d))[1:nrow(d) %in% training == FALSE]
    
    # placeholders for now
    bestEta=NA
    bestDepth=NA
    bestAcc=0

    for(eta in tryEta){
      for(dp in tryDepths){ 
        bst <- xgb.cv(data = X[training,], 
                label =  y[training], 
                max.depth = dp,
              eta = eta, 
              nthread = 4,
              nround = nround,
              nfold=5,
              print_every_n = print_every_n,
              objective = "multi:softmax",
              num_class=n.cats)
        # cross-validated accuracy
        acc <- 1-mean(tail(bst$evaluation_log$test_merror_mean))
            message("Results for eta=",eta," and depth=", dp, " : ",
                    acc," accuracy.")
            if(acc>bestAcc){
                    bestEta=eta
                    bestAcc=acc
                    bestDepth=dp
            }
        }
    }
    
    message("Best model has eta=",bestEta," and depth=", bestDepth, " : ",
        bestAcc," accuracy.")
    
    # running best model
    rf <- xgboost(data = X[training,], 
        label = y[training], 
            max.depth = bestDepth,
        eta = bestEta, 
        nthread = 4,
        nround = nround_long,
        print_every_n=print_every_n_long,
        objective = "multi:softprob",
        num_class=n.cats)
    
    
    # out-of-sample accuracy
    preds <- predict(rf, X[test,], reshape = TRUE)
    preds <- apply(preds, 1, which.max)-1
    message("Accuracy on test set=", round(accuracy(preds, y[test]),3))
    # save out-of-sample accurary
    acc.save <- round(accuracy(preds, y[test]),3)
   
    # by categories
   ## save prediction and recall
   perf.save <- list()
   for (i in 0:(n.cats-1)){
    	message("Category: ", i, "--", labels[i+1], '--', round(mean(preds==i)*100,1), "%")
    	message("A=", round(accuracy(preds==i, y[test]==i), 3))
    	message("P(1)=", round(precision(preds==i, y[test]==i), 3))
    	message("R(1)=", round(recall(preds==i, y[test]==i), 3))
        perf.save[[labels[i+1]]] <- data.frame(precision = round(precision(preds==i, y[test]==i), 3),
                                               recall = round(recall(preds==i, y[test]==i), 3))
    }
    
    # feature importance
    ngrams <- dimnames(X)[[2]]
    importance <- xgb.importance(ngrams, model = rf)
    importance <- importance[order(importance$Gain, decreasing=TRUE),]
    head(importance, n=20)
    
    # adding sign
    sums <- list()
    for (v in 0:(n.cats-1)){
    	sums[[v+1]] <- colSums(X[y==v,])
    }
    sums <- do.call(cbind, sums)
    sign <- apply(sums, 1, which.max)
    
    df <- data.frame(
    	Feature = ngrams, 
    	sign = sign,
    	stringsAsFactors=F)
    importance <- merge(importance, df, by="Feature")

   ## best predictors
   ## save as list
   feat.imp <- list()
   for (v in 0:(n.cats-1)){
      message("value==", v, '(', labels[v+1], ')')
      importance <- importance[order(importance$Gain, decreasing=TRUE),]
                                        #print(head(importance[importance$sign==v,], n=50))
      message(paste(unique(head(importance$Feature[importance$sign==v+1], n=25)), collapse=", "))
      feat.imp[[labels[v+1]]] <- paste(unique(head(importance$Feature[importance$sign==v+1], n=25)), collapse=", ") 
   }

    return(list(rf=rf, acc.save=acc.save, perf.save=perf.save, feat.imp=feat.imp))

}





