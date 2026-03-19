library(quanteda)
library(xgboost)

#====================================================================
# Running xgboost classifier 
#====================================================================
source("02-xgboost_functions.R") ## note that the code in functions.R was written by Pablo Barbera 
set.seed(123)

# loading training dataset
d <- read.csv(file="trainingset.csv", stringsAsFactors=FALSE)
str(d)
d <- d[, c("code", "text")]

# clean text and create DFM
d$text <- gsub('NA', '', d$text)
d$text <- gsub('\\|', ' ', d$text)
corp <- corpus(d$text)
dfm <- dfm(corp, remove = stopwords("english"), remove_punct = TRUE)
dfm <- dfm_trim(dfm, min_docfreq = 2, verbose=TRUE)
dfm <- dfm[,order(featnames(dfm))]
topfeatures(dfm, n=50)

# converting matrix object
X <- as(dfm, "dgCMatrix")

# outcome variables
d$content_level <- NA
d$content_level[d$code==1] <- 0
d$content_level[d$code==2] <- 1
d$content_level[d$code==3] <- 2
table(d$content_level)

#######################################################################
##### CLASSIFIER ######
#######################################################################

content_cl.all <- multinomial_classifier(X=X, y=d$content_level,
	labels=c("untargeted", "targeted", "other"),
	n.cats=3)

content_cl <- content_cl.all[['rf']]

xgboost.accuracy <- content_cl.all[['acc.save']]
xgboost.performace <- content_cl.all[['perf.save']]
xgboost.feat.importance <- content_cl.all[['feat.imp']]
save(xgboost.accuracy, xgboost.performace, xgboost.feat.importance, file="xgboost-performance.Rdata")


#######################################################################
##### Untrained data classification ######
#######################################################################

load("merged-records-details.RData")
data <- mr
data.corp <- corpus(data, text_field="all.text")
data.dfm <- create_test_dfm(dfm, data.corp)
Xmr <- as(data.dfm, "dgCMatrix")


## predict on virgin data
preds <- predict(content_cl, Xmr, reshape=TRUE)
data$is_untargeted <- round(preds[,1],3)
data$is_targeted <- round(preds[,2],3)
data$is_other <- round(preds[,3],3)

data$xgboost.label <- apply(data[, grep("is", colnames(data))], 1, function(x) 
   names(which.max(x)))
data$xgboost.code <-  NA
data$xgboost.code[data$xgboost.label=="is_untargeted"] <- 1 
data$xgboost.code[data$xgboost.label=="is_targeted"] <- 2
data$xgboost.code[data$xgboost.label=="is_other"] <- 3

save(data, file="classified_data-xgboost.RData")
