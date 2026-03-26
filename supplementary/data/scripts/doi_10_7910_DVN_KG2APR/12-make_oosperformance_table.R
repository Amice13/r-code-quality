library(xtable)

### Make Appendix tables:
## Table A9. Out-of-sample performance of classifier
## Table A10. N-grams with highest feature importance for each category
## Table A11. Post-classification: Five randomly selected record details for each classified category (xgboost model)

## XGBOOST performance metrics
load("xgboost-performance.Rdata")

## collate metrics
prec.recall <- round(do.call(rbind, xgboost.performace), digits=2)

## create table
tab <- data.frame(Accuracy=round(c(xgboost.accuracy, NA, NA), digits=2),
                  Category=c("Untargeted", "Targeted", "Other"),
                  Precision=prec.recall$precision,
                  Recall=prec.recall$recall)


## OOS performance of classifiers
## Table A9. Out-of-sample performance of classifiers

print(xtable(tab, align="l|l|ccc"),
      include.rownames=FALSE, hline.after=c(0,1),
      file="TableA9_classification-oos.tex", floating=FALSE)

## XGBOOST Ngrams with highest feature importance
## Table A10. N-grams with highest feature importance for each category

feat.imp <- data.frame(Category=c("Untargeted", "Targeted", "Other"),
                       Ngram = c(xgboost.feat.importance$untargeted, xgboost.feat.importance$targeted, xgboost.feat.importance$other))

print(xtable(feat.imp, align="llp{12cm}"),
      include.rownames=FALSE, include.colnames = FALSE, hline.after=c(1,2), file="TableA10_ngram-feat-importance.tex", floating=FALSE)


## Table A11. Post-classification: Five randomly selected record details for each classified category (xgboost model)
load("classified_data-xgboost.RData")

## sample some classified text
untarg <- data[data$xgboost.label=="is_untargeted", c("all.text")]
untarg <- tolower(untarg)

set.seed(100)
untarg.post <- paste(seq(1,5), gsub("\\s+"," ",untarg[sample(length(untarg), 5)]), sep=": ")

targ <- data[data$xgboost.label=="is_targeted", c("all.text")]
set.seed(1000)
targ.post <- paste(seq(1,5),gsub("\\s+"," ",targ[sample(length(targ), 5)]), sep=": ")


other <- data[data$xgboost.label=="is_other", c("all.text")]
set.seed(10000)
other.post <- paste(seq(1,5), gsub("\\s+"," ",other[sample(length(other), 5)]), sep=": ")

post.val <- data.frame(Category=c("Untargeted", NA, NA, NA, NA,
                                  "Targeted", NA, NA, NA, NA,
                                  "Other", NA, NA, NA, NA),
                       Text = tolower(c(untarg.post, targ.post, other.post)))


print(xtable(post.val, align="llp{12cm}"),
      include.rownames=FALSE, include.colnames = FALSE, hline.after=c(5,5,10,10), file="TableA11_post-validation-rec-details.tex", floating=FALSE)



