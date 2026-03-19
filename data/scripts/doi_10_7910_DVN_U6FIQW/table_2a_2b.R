##TikTok Identity Study - Table 2a and 2b
##R version 4.2.2
##Windows operating system

#import libraries
library("tidyr") #version 1.2.1
library("ggplot2") #version 3.4.0
library("dplyr") #version 1.0.10
library("ggpubr") #version 0.5.0
library('readxl') #version 1.4.1
library("rstatix") #version 0.7.1
library("lme4") #version 1.1.31
library(glmmTMB) #version 1.1.7
library(performance) #version 0.10.3
library(MuMIn) #version 1.47.5

###1. data retrieval.

d <- read_xlsx("transcribing_tiktok_openai_with_final_dataset_0427_anonymized.xlsx")

dnew <- d %>%
  mutate(humanface_detected = ifelse(angry_deep_face == "NA" & disgust_deep_face == "NA" & fear_deep_face == "NA" & happy_deep_face == "NA" & sad_deep_face == "NA" & surprise_deep_face == "NA" & neutral_deep_face == "NA", 0, 1))

dnew$humanface_detected <- as.factor(dnew$humanface_detected)

dnew$angry_deep_face[dnew$angry_deep_face == "NA"] <- 0
dnew$angry_deep_face <- as.numeric(dnew$angry_deep_face)

dnew$fear_deep_face[dnew$fear_deep_face == "NA"] <- 0
dnew$fear_deep_face <- as.numeric(dnew$fear_deep_face)

dnew$happy_deep_face[dnew$happy_deep_face == "NA"] <- 0
dnew$happy_deep_face <- as.numeric(dnew$happy_deep_face)

#####number of shares - main effects
summary(m1 <- glmmTMB(video_shareCount ~ humanface_detected + angry_deep_face + fear_deep_face + happy_deep_face + anger_desc + fear_desc + joy_desc + anger_openai_trans + fear_openai_trans + joy_openai_trans + conservative_desc_count + liberal_desc_count + relational_desc_count + vaccine_injured_identity_desc_count + conspiracy_desc_count + conservative_openai_trans_count + liberal_openai_trans_count + relational_openai_trans_count + vaccine_injured_identity_openai_trans_count + conspiracy_openai_trans_count + partisan_desc_count + partisan_openai_trans_count + (1|channel_id), family = "nbinom2"(link="log"), control = glmmTMBControl(parallel = 5), REML = FALSE, data = dnew))
r.squaredGLMM(m1)

#####number of comments - main effects
summary(m2 <- glmmTMB(video_commentCount ~ humanface_detected + angry_deep_face + fear_deep_face + happy_deep_face + anger_desc + fear_desc + joy_desc + anger_openai_trans + fear_openai_trans + joy_openai_trans + conservative_desc_count + liberal_desc_count + relational_desc_count + vaccine_injured_identity_desc_count + conspiracy_desc_count + conservative_openai_trans_count + liberal_openai_trans_count + relational_openai_trans_count + vaccine_injured_identity_openai_trans_count + conspiracy_openai_trans_count + partisan_desc_count + partisan_openai_trans_count + (1|channel_id), family = "nbinom2"(link="log"), control = glmmTMBControl(parallel = 5), REML = FALSE, data = dnew))
r.squaredGLMM(m2)

#####number of likes - main effects
summary(m3 <- glmmTMB(video_diggCount ~ humanface_detected + angry_deep_face + fear_deep_face + happy_deep_face + anger_desc + fear_desc + joy_desc + anger_openai_trans + fear_openai_trans + joy_openai_trans + conservative_desc_count + liberal_desc_count + relational_desc_count + vaccine_injured_identity_desc_count + conspiracy_desc_count + conservative_openai_trans_count + liberal_openai_trans_count + relational_openai_trans_count + vaccine_injured_identity_openai_trans_count + conspiracy_openai_trans_count + partisan_desc_count + partisan_openai_trans_count + (1|channel_id), family = "nbinom2"(link="log"), control = glmmTMBControl(parallel = 5), REML = FALSE, data = dnew))
r.squaredGLMM(m3)


#TABLE 2b: Emotion, identity, and engagement with TikTok videos: Results from negative binomial regression - interaction effects

##number of shares -- interaction effects
summary(m1it <- glmmTMB(video_shareCount ~ humanface_detected + angry_deep_face + fear_deep_face + happy_deep_face + anger_desc + fear_desc + joy_desc + anger_openai_trans + fear_openai_trans + joy_openai_trans + conservative_desc_count + liberal_desc_count + relational_desc_count + vaccine_injured_identity_desc_count + conspiracy_desc_count + conservative_openai_trans_count + liberal_openai_trans_count + relational_openai_trans_count + vaccine_injured_identity_openai_trans_count + conspiracy_openai_trans_count + partisan_desc_count + partisan_openai_trans_count + partisan_openai_trans_count:anger_openai_trans + partisan_desc_count:anger_desc + partisan_openai_trans_count:angry_deep_face + (1|channel_id), family = "nbinom2"(link="log"), control = glmmTMBControl(parallel = 5), REML = FALSE, data = dnew))
r.squaredGLMM(m1it)

##number of comments - interaction effects
summary(m2it <- glmmTMB(video_commentCount ~ humanface_detected + angry_deep_face + fear_deep_face + happy_deep_face + anger_desc + fear_desc + joy_desc + anger_openai_trans + fear_openai_trans + joy_openai_trans + conservative_desc_count + liberal_desc_count + relational_desc_count + vaccine_injured_identity_desc_count + conspiracy_desc_count + conservative_openai_trans_count + liberal_openai_trans_count + relational_openai_trans_count + vaccine_injured_identity_openai_trans_count + conspiracy_openai_trans_count + partisan_desc_count + partisan_openai_trans_count + partisan_openai_trans_count:anger_openai_trans + partisan_desc_count:anger_desc + partisan_openai_trans_count:angry_deep_face + (1|channel_id), family = "nbinom2"(link="log"), control = glmmTMBControl(parallel = 5), REML = FALSE, data = dnew))
r.squaredGLMM(m2it)

##number of likes - interaction effects
summary(m3it <- glmmTMB(video_diggCount ~ humanface_detected + angry_deep_face + fear_deep_face + happy_deep_face + anger_desc + fear_desc + joy_desc + anger_openai_trans + fear_openai_trans + joy_openai_trans + conservative_desc_count + liberal_desc_count + relational_desc_count + vaccine_injured_identity_desc_count + conspiracy_desc_count + conservative_openai_trans_count + liberal_openai_trans_count + relational_openai_trans_count + vaccine_injured_identity_openai_trans_count + conspiracy_openai_trans_count + partisan_desc_count + partisan_openai_trans_count + partisan_openai_trans_count:anger_openai_trans + partisan_desc_count:anger_desc + partisan_openai_trans_count:angry_deep_face + (1|channel_id), family = "nbinom2"(link="log"), control = glmmTMBControl(parallel = 5), REML = FALSE, data = dnew))
r.squaredGLMM(m3it)




