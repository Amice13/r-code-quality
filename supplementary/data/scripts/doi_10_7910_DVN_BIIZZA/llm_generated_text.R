##############################################################################
#Replication script for: Claassen, J., Höhne, J.K., Bach, R., & Haensch, A.-C.
#(in press). Identifying bots through LLM-generated text in open narrative
#responses: A proof-of-concept study. Social Science Computer Review.
##############################################################################

#load libraries
library(summarytools)

#load data
data <- readRDS("llm_generated_text.RDS")


#######################################################
#Table 1. Sample characteristics of the Facebook survey
#######################################################

#sample size
freq(data$group_prompt)

#age
descr(data$f_age[which(data$group == "facebook")], 
      stats = c("mean", "N.Valid"))

#gender
freq(data$f_gender[which(data$group == "facebook")])

#formal education
freq(data$f_education[which(data$group == "facebook")])


################################
#Table 2. Descriptive statistics
################################

#item-nonresponse
ctable(data$onq1_nresp, data$group_prompt, prop = "c", useNA = "no") #onq1
chisq.test(data$onq1_nresp, data$group_prompt)

ctable(data$onq2_nresp, data$group_prompt, prop = "c", useNA = "no") #onq2
chisq.test(data$onq2_nresp, data$group_prompt)

ctable(data$onq3_nresp, data$group_prompt, prop = "c", useNA = "no") #onq3
chisq.test(data$onq3_nresp, data$group_prompt)


#unique responses
ctable(data$onq1_unique, data$group_prompt, prop = "c", useNA = "no") #onq1
chisq.test(data$onq1_unique, data$group_prompt)

ctable(data$onq2_unique, data$group_prompt, prop = "c", useNA = "no") #onq2
chisq.test(data$onq2_unique, data$group_prompt)

ctable(data$onq3_unique, data$group_prompt, prop = "c", useNA = "no") #onq3
chisq.test(data$onq3_unique, data$group_prompt)


#response length (onq1)
aggregate(data$onq1_length, list(data$group_prompt), FUN=mean, na.rm = T) #means

summary(aov(onq1_length ~ group_prompt, data = data)) #one-way ANOVA

pairwise.t.test(data$onq1_length, data$group_prompt, p.adjust.method = "bonf") #pairwise t-tests with bonferroni correction procedure


#response length (onq2)
aggregate(data$onq2_length, list(data$group_prompt), FUN=mean, na.rm = T) #means

summary(aov(onq2_length ~ group_prompt, data = data)) #one-way ANOVA

pairwise.t.test(data$onq2_length, data$group_prompt, p.adjust.method = "bonf") #pairwise t-tests with bonferroni correction procedure


#response length (onq3)
aggregate(data$onq3_length, list(data$group_prompt), FUN=mean, na.rm = T) #means

summary(aov(onq3_length ~ group_prompt, data = data)) #one-way ANOVA

pairwise.t.test(data$onq3_length, data$group_prompt, p.adjust.method = "bonf") #pairwise t-tests with bonferroni correction procedure


################################
#Table 3. Prediction performance
################################

#function to calculate precision, recall, and F1 score
eval_metrics <- function(true_labels, predicted_labels, data, display = "all") {
  
  #reduce data to "testset"
  testset <- subset(data[, c(true_labels, predicted_labels)], !is.na(predicted_labels))
  n <- nrow(testset)
  
  #create confusion matrix
  true <- testset[, true_labels]
  pred <- testset[, predicted_labels]
  matrix <- table(true, pred)
  
  #determine TP, FP, and FN
  TP <- matrix[2,2]
  FP <- matrix[1,2]
  FN <- matrix[2,1]
  
  #calculate precision, recall, and F1 score
  precision = TP/(TP+FP) #https://scikit-learn.org/stable/modules/generated/sklearn.metrics.precision_score.html#sklearn.metrics.precision_score
  recall = TP/(TP+FN) #https://scikit-learn.org/stable/modules/generated/sklearn.metrics.recall_score.html
  F1 = 2*TP/(2*TP+FP+FN) #https://scikit-learn.org/stable/modules/generated/sklearn.metrics.f1_score.html
  
  #return evaluation metrics
  
  if (display == "all") {
    output <- paste("n = ", n, ", ",
                    "precision = ", round(precision, 2), ", ",
                    "recall = ", round(recall, 2), ", ",
                    "F1 score = ", round(F1, 2), sep = ""
    )
  } else if (display == "recall") {
    output <- paste("n = ", n, ", ",
                    "recall = ", round(recall, 2), sep = ""
    )
  }
  output
}

#onq1 
eval_metrics("robotic_language_num", "onq1_pred_onq1m", data[which(!is.na(data$onq1_pred_onq1m)),]) #onq1 model (in-corpus prediction)

eval_metrics("robotic_language_num", "onq1_pred_onq2m", data[which(!is.na(data$onq1_pred_onq2m)),]) #onq2 model (cross-corpus prediction)

eval_metrics("robotic_language_num", "onq1_pred_onq3m", data[which(!is.na(data$onq1_pred_onq3m)),]) #onq3 model (cross-corpus prediction)

#onq2
eval_metrics("robotic_language_num", "onq2_pred_onq1m", data[which(!is.na(data$onq2_pred_onq1m)),]) #onq1 model (cross-corpus prediction)

eval_metrics("robotic_language_num", "onq2_pred_onq2m", data[which(!is.na(data$onq2_pred_onq2m)),]) #onq2 model (in-corpus prediction)

eval_metrics("robotic_language_num", "onq2_pred_onq3m", data[which(!is.na(data$onq2_pred_onq3m)),]) #onq3 model (cross-corpus prediction)

#onq3
eval_metrics("robotic_language_num", "onq3_pred_onq1m", data[which(!is.na(data$onq3_pred_onq1m)),]) #onq1 model (cross-corpus prediction)

eval_metrics("robotic_language_num", "onq3_pred_onq2m", data[which(!is.na(data$onq3_pred_onq2m)),]) #onq2 model (cross-corpus prediction)

eval_metrics("robotic_language_num", "onq3_pred_onq3m", data[which(!is.na(data$onq3_pred_onq3m)),]) #onq3 model (in-corpus prediction)

#rate of false positive predictions among Facebook responses (in-corpus predictions)
prop.table(table(data$onq1_pred_onq1m[which(data$group == "facebook")])) #ONQ1
prop.table(table(data$onq2_pred_onq2m[which(data$group == "facebook")])) #ONQ2
prop.table(table(data$onq3_pred_onq3m[which(data$group == "facebook")])) #ONQ3

############################################################
#Table 4. Top five contributing tokens by ONQ and prediction
############################################################

#Due to data protection, we cannot release Facebook respondents' open answers
#verbatim. Therefore, this analysis cannot be replicated with the public dataset.


############################################################
#Table C1. Response distribution of the CQ on child adoption
############################################################

#cross-distribution of group and cq variables
ctable(data$cq, data$group, prop = "c", useNA = "no")


##############################################################################
#Table D1. Recall of in-corpus predictions by LLM-driven bot and prompt design
##############################################################################

#onq1
eval_metrics("robotic_language_num", "onq1_pred_onq1m", 
             data[which(!is.na(data$onq1_pred_onq1m)
                        & (data$group_prompt == "LLM & baseline"
                           | data$group_prompt == "facebook")),], display = "recall")

eval_metrics("robotic_language_num", "onq1_pred_onq1m", 
             data[which(!is.na(data$onq1_pred_onq1m)
                        & (data$group_prompt == "LLM & misspellings"
                           | data$group_prompt == "facebook")),], display = "recall")

eval_metrics("robotic_language_num", "onq1_pred_onq1m", 
             data[which(!is.na(data$onq1_pred_onq1m)
                        & (data$group_prompt == "LLM+ & baseline"
                           | data$group_prompt == "facebook")),], display = "recall")

eval_metrics("robotic_language_num", "onq1_pred_onq1m", 
             data[which(!is.na(data$onq1_pred_onq1m)
                        & (data$group_prompt == "LLM+ & misspellings"
                           | data$group_prompt == "facebook")),], display = "recall")


#onq2
eval_metrics("robotic_language_num", "onq2_pred_onq2m", 
             data[which(!is.na(data$onq2_pred_onq2m)
                        & (data$group_prompt == "LLM & baseline"
                           | data$group_prompt == "facebook")),], display = "recall")

eval_metrics("robotic_language_num", "onq2_pred_onq2m", 
             data[which(!is.na(data$onq2_pred_onq2m)
                        & (data$group_prompt == "LLM & misspellings"
                           | data$group_prompt == "facebook")),], display = "recall")

eval_metrics("robotic_language_num", "onq2_pred_onq2m", 
             data[which(!is.na(data$onq2_pred_onq2m)
                        & (data$group_prompt == "LLM+ & baseline"
                           | data$group_prompt == "facebook")),], display = "recall")

eval_metrics("robotic_language_num", "onq2_pred_onq2m", 
             data[which(!is.na(data$onq2_pred_onq2m)
                        & (data$group_prompt == "LLM+ & misspellings"
                           | data$group_prompt == "facebook")),], display = "recall")


#onq3
eval_metrics("robotic_language_num", "onq3_pred_onq3m", 
             data[which(!is.na(data$onq3_pred_onq3m)
                        & (data$group_prompt == "LLM & baseline"
                           | data$group_prompt == "facebook")),], display = "recall")

eval_metrics("robotic_language_num", "onq3_pred_onq3m", 
             data[which(!is.na(data$onq3_pred_onq3m)
                        & (data$group_prompt == "LLM & misspellings"
                           | data$group_prompt == "facebook")),], display = "recall")

eval_metrics("robotic_language_num", "onq3_pred_onq3m", 
             data[which(!is.na(data$onq3_pred_onq3m)
                        & (data$group_prompt == "LLM+ & baseline"
                           | data$group_prompt == "facebook")),], display = "recall")

eval_metrics("robotic_language_num", "onq3_pred_onq3m", 
             data[which(!is.na(data$onq3_pred_onq3m)
                        & (data$group_prompt == "LLM+ & misspellings"
                           | data$group_prompt == "facebook")),], display = "recall")
