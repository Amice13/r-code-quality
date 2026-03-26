library(tidyverse)           # CRAN v2.0.0
library(arrow)               # CRAN v14.0.0.2
library(quanteda)            # CRAN v3.3.1
library(quanteda.textmodels) # CRAN v0.9.6
library(caret)               # CRAN v6.0-94
library(mltest)              # CRAN v1.0.1
library(xtable)              # CRAN v1.8-4

# run SVM Model
train = read_parquet("./data_dontshare/train.parquet")

test = read_parquet("./data_dontshare/test.parquet")

dfm_train = corpus(train) %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_remove(stopwords("en")) %>% 
  dfm()

tmod_svm_eval_excluded = textmodel_svm(dfm_train, y = dfm_train$housing_coded)

dfm_test = corpus(test) %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_remove(stopwords("en")) %>% 
  dfm()

svm_excluded_pred = predict(tmod_svm_eval_excluded, newdata = dfm_test)

test$svm_eval_excluded = svm_excluded_pred

svm = test


# compare classifiers

distilbert = read_parquet("./data_dontshare/test_distilbert.parquet")
test = read_parquet("./data_dontshare/test.parquet")

distilbert$distilbert_pred = ifelse(distilbert$Housing > distilbert$`Non-housing`, 1,0)
distilbert$housing_coded = test$housing_coded
distilbert = distilbert %>% select(text, distilbert_pred, housing_coded)


# get classifier performance
performance_bert <- ml_test(predicted = factor(distilbert$distilbert_pred),
                            true = factor(distilbert$housing_coded),
                            output.as.table = TRUE) |> 
    mutate(classifier = "DistilBert")
performance_bert$class <- rownames(performance_bert)

performance_bert = performance_bert %>% select(classifier, class, balanced.accuracy, F1, precision,recall,specificity)

performance_svm <- ml_test(predicted = factor(svm$svm_eval_excluded),
                            true = factor(svm$housing_coded),
                            output.as.table = TRUE) |> 
    mutate(classifier = "SVM")
performance_svm$class <- rownames(performance_svm)

performance_svm = performance_svm %>% select(classifier, class, balanced.accuracy, F1, precision,recall,specificity)


# combine and export table
performance_housing = bind_rows(performance_bert,performance_svm)
performance_housing$class = ifelse(performance_housing$class == 1, "Housing", "Non-Housing")

names(performance_housing)

performance_housing <- performance_housing |> 
    dplyr::rename(Classifier = classifier,
                  Class = class,
                  `Balanced Accuracy` = balanced.accuracy,
                  `F1 Score` = F1,
                  Precision = precision,
                  Recall = recall,
                  Specificity = specificity)


print(xtable(performance_housing,
             caption.placement = "top",
             digits = 3,
             label = "tab:classifiers",
             caption = "Performance metrics for DistilBert and SVM"),
      include.rownames = FALSE,
      type = "latex",
      caption.placement = "top",
      size = "footnotesize",
      file = "tab_a03.tex")

print(xtable(performance_housing,
             caption.placement = "top",
             digits = 3,
             label = "tab:classifiers",
             caption = "Performance metrics for DistilBert and SVM"),
      include.rownames = FALSE,
      type = "html",
      caption.placement = "top",
      size = "footnotesize",
      file = "tab_a03.html")

