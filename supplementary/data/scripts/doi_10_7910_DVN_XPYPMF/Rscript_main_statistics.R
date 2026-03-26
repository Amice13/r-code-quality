# packages

library(lme4)
library(dplyr)
library(effsize)
library(lsr)


## Whole lm model (2 ways ANOVA)
# dataset : data_stats_exams2
interaction.aov = lm(score_perc_exam ~ learning_condition * question_type, data = data_stats_exams2)
anova(interaction.aov)
etaSquared(interaction.aov)


## One way ANOVA trained questions
# dataset : data_stats_exams
trained.aov = lm(score_perc_Trained_exam ~ learning_condition, data = data_stats_exams) 
anova(trained.aov)
etaSquared(trained.aov)


## one way ANOVA untrained questions (jointly and separately)
# dataset : data_stats_exams
untrained.aov = lm(score_perc_Untrained_exam ~ learning_condition, data = data_stats_exams) 
anova(untrained.aov)
etaSquared(untrained.aov)

new.aov = lm(score_perc_New_exam ~ learning_condition, data = data_stats_exams) 
anova(new.aov)
etaSquared(new.aov)


general.aov = lm(score_perc_General_exam ~ learning_condition, data = data_stats_exams) 
anova(general.aov)
etaSquared(general.aov)


## t-test comparisons

# pre processing data.frame All questions t-tests
posthoc_total <- data.frame(data_stats_exams$user_regrank, data_stats_exams$learning_condition, data_stats_exams$score_perc_Total_exam)
data_RQ = posthoc_total %>%
  filter(data_stats_exams.learning_condition == "Reading-quiz")
data_QR = posthoc_total %>%
  filter(data_stats_exams.learning_condition == "Quiz-reading")
data_RR = posthoc_total %>%
  filter(data_stats_exams.learning_condition == "Reading-reading")

testing_total <- rbind(data_RQ, data_RR)
pretesting_total <- rbind(data_QR, data_RR)
position_total <- rbind(data_RQ, data_QR)


# pre processing data.frame Untrained questions (total) t-tests
posthoc_untrained <- data.frame(data_stats_exams$user_regrank, data_stats_exams$learning_condition, data_stats_exams$score_perc_Untrained_exam)
data_RQ1 = posthoc_untrained %>%
  filter(data_stats_exams.learning_condition == "Reading-quiz")
data_QR1 = posthoc_untrained %>%
  filter(data_stats_exams.learning_condition == "Quiz-reading")
data_RR1 = posthoc_untrained %>%
  filter(data_stats_exams.learning_condition == "Reading-reading")

testing_untrained <- rbind(data_RQ1, data_RR1)
pretesting_untrained <- rbind(data_QR1, data_RR1)
position_untrained <- rbind(data_RQ1, data_QR1)


# pre processing data.frame Trained questions t-tests
posthoc_trained <- data.frame(data_stats_exams$user_regrank, data_stats_exams$learning_condition, data_stats_exams$score_perc_Trained_exam)
data_RQ2 = posthoc_trained %>%
  filter(data_stats_exams.learning_condition == "Reading-quiz")
data_QR2 = posthoc_trained %>%
  filter(data_stats_exams.learning_condition == "Quiz-reading")
data_RR2 = posthoc_trained %>%
  filter(data_stats_exams.learning_condition == "Reading-reading")

testing_trained <- rbind(data_RQ2, data_RR2)
pretesting_trained <- rbind(data_QR2, data_RR2)
position_trained <- rbind(data_RQ2, data_QR2)


# pre processing data.frame New questions t-tests
posthoc_new <- data.frame(data_stats_exams$user_regrank, data_stats_exams$learning_condition, data_stats_exams$score_perc_New_exam)
data_CQ3 = posthoc_new %>%
  filter(data_stats_exams.learning_condition == "Reading-quiz")
data_QC3 = posthoc_new %>%
  filter(data_stats_exams.learning_condition == "Quiz-reading")
data_CC3 = posthoc_new %>%
  filter(data_stats_exams.learning_condition == "Reading-reading")

testing_New <- rbind(data_CQ3, data_CC3)
pretesting_New <- rbind(data_QC3, data_CC3)
position_New <- rbind(data_CQ3, data_QC3)

# pre processing data.frame Generalisation questions t-tests
posthoc_gen <- data.frame(data_stats_exams$user_regrank, data_stats_exams$learning_condition, data_stats_exams$score_perc_General_exam)
data_CQ4 = posthoc_gen %>%
  filter(data_stats_exams.learning_condition == "Reading-quiz")
data_QC4 = posthoc_gen %>%
  filter(data_stats_exams.learning_condition == "Quiz-reading")
data_CC4 = posthoc_gen %>%
  filter(data_stats_exams.learning_condition == "Reading-reading")

testing_Gen <- rbind(data_CQ4, data_CC4)
pretesting_Gen <- rbind(data_QC4, data_CC4)
position_Gen <- rbind(data_CQ4, data_QC4)


# Prediction according to previous studies on the post-testing effect
postesting_effect_total <- t.test(data_stats_exams.score_perc_Total_exam ~ data_stats_exams.learning_condition, data=testing_total, var.equal=TRUE)
postesting_effect_Untrained <- t.test(data_stats_exams.score_perc_Untrained_exam ~ data_stats_exams.learning_condition, data=testing_untrained, var.equal=TRUE)
postesting_effect_Trained <- t.test(data_stats_exams.score_perc_Trained_exam ~ data_stats_exams.learning_condition, data=testing_trained, var.equal=TRUE)
postesting_effect_New <- t.test(data_stats_exams.score_perc_New_exam ~ data_stats_exams.learning_condition, data=testing_New, var.equal=TRUE)
postesting_effect_Gen <- t.test(data_stats_exams.score_perc_General_exam ~ data_stats_exams.learning_condition, data=testing_Gen, var.equal=TRUE)

# Prediction according to previous study on pre-testing effect  
pretesting_effect_total <- t.test(data_stats_exams.score_perc_Total_exam ~ data_stats_exams.learning_condition, data=pretesting_total, var.equal=TRUE)
pretesting_effect_Untrained <- t.test(data_stats_exams.score_perc_Untrained_exam ~ data_stats_exams.learning_condition, data=pretesting_untrained, var.equal=TRUE)
pretesting_effect_Trained <- t.test(data_stats_exams.score_perc_Trained_exam ~ data_stats_exams.learning_condition, data= pretesting_trained, var.equal=TRUE)
pretesting_effect_New <- t.test(data_stats_exams.score_perc_New_exam ~ data_stats_exams.learning_condition, data=pretesting_New, var.equal=TRUE)
pretesting_effect_Gen <- t.test(data_stats_exams.score_perc_General_exam ~ data_stats_exams.learning_condition, data=pretesting_Gen, var.equal=TRUE)

# Our main question: Quiz-reading >? Reading-quiz 
position_effect_total <- t.test(data_stats_exams.score_perc_Total_exam ~ data_stats_exams.learning_condition, data=position_total, var.equal=TRUE)
position_effect_Untrained <- t.test(data_stats_exams.score_perc_Untrained_exam ~ data_stats_exams.learning_condition, data=position_untrained, var.equal=TRUE)
position_effect_Trained <- t.test(data_stats_exams.score_perc_Trained_exam ~ data_stats_exams.learning_condition, data=position_trained, var.equal=TRUE)
position_effect_New <- t.test(data_stats_exams.score_perc_New_exam ~ data_stats_exams.learning_condition, data=position_New, var.equal=TRUE)
position_effect_Gen <- t.test(data_stats_exams.score_perc_General_exam ~ data_stats_exams.learning_condition, data=position_Gen, var.equal=TRUE)

## All effect sizes

#post testing effect with All questions score (total)
cohen.d(testing_total$data_stats_exams.score_perc_Total_exam, testing_total$data_stats_exams.learning_condition, pooled=TRUE, paired=FALSE, na.rm=TRUE, hedges.correction = FALSE, conf.level = 0.95, noncentral = FALSE)
#post testing effect with Untrained questions score
cohen.d(testing_untrained$data_stats_exams.score_perc_Untrained_exam, testing_untrained$data_stats_exams.learning_condition, pooled=TRUE, paired=FALSE, na.rm=TRUE, hedges.correction = FALSE, conf.level = 0.95, noncentral = FALSE)
#post testing effect with Trained questions score
cohen.d(testing_trained$data_stats_exams.score_perc_Trained_exam, testing_trained$data_stats_exams.learning_condition, pooled=TRUE, paired=FALSE, na.rm=TRUE, hedges.correction = FALSE, conf.level = 0.95, noncentral = FALSE)
#post testing effect with New questions score
cohen.d(testing_New$data_stats_exams.score_perc_New_exam, testing_New$data_stats_exams.learning_condition, pooled=TRUE, paired=FALSE, na.rm=TRUE, hedges.correction = FALSE, conf.level = 0.95, noncentral = FALSE)
#post testing effect with Generalisation questions score
cohen.d(testing_Gen$data_stats_exams.score_perc_General_exam, testing_Gen$data_stats_exams.learning_condition, pooled=TRUE, paired=FALSE, na.rm=TRUE, hedges.correction = FALSE, conf.level = 0.95, noncentral = FALSE)

#pre-testing effect with All questions score (total)
cohen.d(pretesting_total$data_stats_exams.score_perc_Total_exam, pretesting_total$data_stats_exams.learning_condition, pooled=TRUE, paired=FALSE, na.rm=TRUE, hedges.correction = FALSE, conf.level = 0.95, noncentral = FALSE)
#pre-testing effect with Untrained questions score
cohen.d(pretesting_untrained$data_stats_exams.score_perc_Untrained_exam, pretesting_untrained$data_stats_exams.learning_condition, pooled=TRUE, paired=FALSE, na.rm=TRUE, hedges.correction = FALSE, conf.level = 0.95, noncentral = FALSE)
#pre-testing effect with Trained questions score
cohen.d(pretesting_trained$data_stats_exams.score_perc_Trained_exam, pretesting_trained$data_stats_exams.learning_condition, pooled=TRUE, paired=FALSE, na.rm=TRUE, hedges.correction = FALSE, conf.level = 0.95, noncentral = FALSE)
#pre-testing effect with New questions score
cohen.d(pretesting_New$data_stats_exams.score_perc_New_exam, pretesting_New$data_stats_exams.learning_condition, pooled=TRUE, paired=FALSE, na.rm=TRUE, hedges.correction = FALSE, conf.level = 0.95, noncentral = FALSE)
#pre-testing effect with Generalisation questions score
cohen.d(pretesting_Gen$data_stats_exams.score_perc_General_exam, pretesting_Gen$data_stats_exams.learning_condition, pooled=TRUE, paired=FALSE, na.rm=TRUE, hedges.correction = FALSE, conf.level = 0.95, noncentral = FALSE)

#order effect with All questions score (total)
cohen.d(position_total$data_stats_exams.score_perc_Total_exam, position_total$data_stats_exams.learning_condition, pooled=TRUE, paired=FALSE, na.rm=TRUE, hedges.correction = FALSE, conf.level = 0.95, noncentral = FALSE)
#order effect with Untrained questions score
cohen.d(position_untrained$data_stats_exams.score_perc_Untrained_exam, position_untrained$data_stats_exams.learning_condition, pooled=TRUE, paired=FALSE, na.rm=TRUE, hedges.correction = FALSE, conf.level = 0.95, noncentral = FALSE)
#order effect with Trained questions score
cohen.d(position_trained$data_stats_exams.score_perc_Trained_exam, position_trained$data_stats_exams.learning_condition, pooled=TRUE, paired=FALSE, na.rm=TRUE, hedges.correction = FALSE, conf.level = 0.95, noncentral = FALSE)
#order effect with New questions score
cohen.d(position_New$data_stats_exams.score_perc_New_exam, position_New$data_stats_exams.learning_condition, pooled=TRUE, paired=FALSE, na.rm=TRUE, hedges.correction = FALSE, conf.level = 0.95, noncentral = FALSE)
#order effect with Generalisation questions score
cohen.d(position_Gen$data_stats_exams.score_perc_General_exam, position_Gen$data_stats_exams.learning_condition, pooled=TRUE, paired=FALSE, na.rm=TRUE, hedges.correction = FALSE, conf.level = 0.95, noncentral = FALSE)


