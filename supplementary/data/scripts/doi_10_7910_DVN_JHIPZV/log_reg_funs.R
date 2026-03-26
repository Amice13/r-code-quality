# Define model accuracy
glm.accuracy <- function(test, class, glm.fit) {
  probs = predict(glm.fit, type = 'response', newdata = test)
  preds <- as.numeric(probs > 0.5)
  cm=table(test[[class]],preds)
  accuracy <-(cm[1,1]+cm[2,2])/(nrow(test))
  cat('Accuracy=', accuracy, "/n")
  #Computing AUC 
  ROCR_pred = prediction(probs, test$mafia_dummy)
  auc = round(as.numeric(performance(ROCR_pred, "auc")@y.values),2) 
  cat('AUC=', auc, "/n")
  res <- c(accuracy, auc)
  return(res)
} 

#Importance plot
plotVarImp <- function(fit){
  options(scipen=999)
  varImp(fit) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column(var = "Variables") %>% 
    top_n(20) %>% 
    ggplot(aes(x=reorder(Variables,Overall), y=Overall)) +
    geom_bar(stat="Identity") +
    coord_flip() +
    theme_minimal() +
    labs(x = '', y = 'Overall Importance')
}
