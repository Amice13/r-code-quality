# Libraries
install.packages(c("lme4","plm","lmtest","rms","texreg"))
library(lme4)
library(plm)
library(lmtest)
library(rms)
library(texreg)

######################
##### Table OA4.1
#######################

load("Study1b.Rdata")

t <- study1b[,c("neg_share_all_other_only", "loyal_vote","prev_vote_choice_cdd","ideo_scaled_inverted_0to1","age","age_sq","male","votet1","income_scaled","educ_scaled")]
t <- t[complete.cases(t),]
control_inter_0to1_rob <- lrm(loyal_vote ~ neg_share_all_other_only*ideo_scaled_inverted_0to1  + age+ age_sq + male  +income_scaled+educ_scaled +votet1, data=t, x=T, y=T)
control_inter_0to1_rob  <- robcov(control_inter_0to1_rob, t$prev_vote_choice_cdd, method="huber")

texreg(list(control_inter_0to1_rob),stars=c(0.01,0.05,0.1))

######################
##### Table OA4.2
#######################

control_lmer_inter_0to1_country <- glmer(loyal_vote ~ neg_share_all_other_only*ideo_scaled_inverted_0to1 + total_counts+ age+age_sq+male +income_scaled+educ_scaled+ (1|country_name), data=study1b , family="binomial")

control_lmer_inter_0to1_election <- glmer(loyal_vote ~ neg_share_all_other_only*ideo_scaled_inverted_0to1 + total_counts+ age+age_sq+male +income_scaled+educ_scaled+ (1|country_election), data=study1b , family="binomial")

texreg(list(control_lmer_inter_0to1_country, control_lmer_inter_0to1_election), stars=c(0.01,0.05,0.1))

######################
##### Table OA4.3
#######################

load("Study1b_including_abstentions.Rdata")

t <- study1b_incl_abstentions[,c("neg_share_all_other_only", "loyal_vote","prev_vote_choice_cdd","ideo_scaled_inverted_0to1","age","age_sq","male","total_counts","income_scaled","educ_scaled")]
t <- t[complete.cases(t),]
control_inter_0to1 <- lrm(loyal_vote ~ neg_share_all_other_only*ideo_scaled_inverted_0to1  + age+ age_sq + male  +income_scaled+educ_scaled +total_counts, data=t, x=T, y=T)
control_inter_0to1 <- robcov(control_inter_0to1, t$prev_vote_choice_cdd, method="huber")

texreg(list(control_inter_0to1),stars=c(0.01,0.05,0.1))

######################
##### Table OA4.4
#######################

load("Study1b.Rdata")

t <- study1b[,c("neg_share_all_other_only", "loyal_vote","prev_vote_choice_cdd","ideo_scaled_inverted_0to1","age","age_sq","male","total_counts","income_scaled","educ_scaled","country_name")]
t <- t[complete.cases(t),]
country_fe <- lrm(loyal_vote ~ neg_share_all_other_only*ideo_scaled_inverted_0to1 + age+ age_sq + male  +income_scaled+educ_scaled +total_counts + country_name, data=t, x=T, y=T)
country_fe <- robcov(country_fe, t$prev_vote_choice_cdd, method="huber")

t <- study1b[,c("neg_share_all_other_only", "loyal_vote","prev_vote_choice_cdd","ideo_scaled_inverted_0to1","age","age_sq","male","total_counts","income_scaled","educ_scaled","country_election")]
t <- t[complete.cases(t),]
election_fe <- lrm(loyal_vote ~ neg_share_all_other_only*ideo_scaled_inverted_0to1 + age+ age_sq + male  +income_scaled+educ_scaled +total_counts + country_election, data=t, x=T, y=T)
election_fe <- robcov(election_fe, t$prev_vote_choice_cdd, method="huber")

texreg(list(country_fe,election_fe), stars=c(0.01,0.05,0.1))

#######################
### Table OA4.5
#######################

# Create party variable by getting rid of the year part
study1b$party <- paste(substr(study1b$prev_vote_choice_cdd,1,3),substr(study1b$prev_vote_choice_cdd,10,nchar(study1b$prev_vote_choice_cdd)),sep="_")

t <- study1b[,c("neg_share_all_other_only", "loyal_vote","ideo_scaled_inverted_0to1","age","age_sq","male","total_counts","income_scaled","educ_scaled","party")]
t <- t[complete.cases(t),]
mod_w_party_cluster <- lrm(loyal_vote ~ neg_share_all_other_only*ideo_scaled_inverted_0to1 + age+ age_sq + male  +income_scaled+educ_scaled +total_counts, data=t, x=T, y=T)
mod_w_party_cluster <- robcov(mod_w_party_cluster, t$party, method="huber")

texreg(list(mod_w_party_cluster),stars=c(0.01,0.05,0.1))

#######################
### Table OA4.6
#######################

# Run glm model
t <- study1b[,c("neg_share_all_other_only", "loyal_vote","prev_vote_choice_cdd","ideo_scaled_inverted_0to1","age","age_sq","male","total_counts","income_scaled","educ_scaled")]
t <- t[which(complete.cases(t)),]
rownames(t) <- NULL
control_inter_0to1_glm <- glm(loyal_vote ~ neg_share_all_other_only*ideo_scaled_inverted_0to1  + age+ age_sq + male  +income_scaled+educ_scaled +total_counts, data=t, family="binomial")
coeftest(control_inter_0to1_glm, function(x) vcovHC(x, method="white2",cluster="prev_vote_choice_cdd"))

# Identify influential observations
cooks <- cooks.distance(control_inter_0to1_glm)
cutoff <- 4/(nrow(t)-9-1)

# Run model without influential observations
t <- t[which(cooks<=cutoff),]
control_inter_0to1_reduced <- lrm(loyal_vote ~ neg_share_all_other_only*ideo_scaled_inverted_0to1  + age+ age_sq + male  +income_scaled+educ_scaled +total_counts, data=t, x=T, y=T)
control_inter_0to1_reduced <- robcov(control_inter_0to1_reduced, t$prev_vote_choice_cdd, method="huber")

texreg(control_inter_0to1_reduced,stars=c(0.01,0.05,0.1))

#######################
### Table OA4.7
#######################

t <- study1b[,c("neg_share_all_other_only", "loyal_vote","prev_vote_choice_cdd","ideo_scaled_inverted_0to1","age","age_sq","male","total_counts","income_scaled","educ_scaled","perceived_distance")]
t <- t[complete.cases(t),]
distance_mod <- lrm(loyal_vote ~ neg_share_all_other_only*ideo_scaled_inverted_0to1 + age+ age_sq + male  +income_scaled+educ_scaled +total_counts + perceived_distance, data=t, x=T, y=T) # Does not run
distance_mod <- robcov(distance_mod, t$prev_vote_choice_cdd, method="huber")

t <- study1b[,c("neg_share_all_other_only", "loyal_vote","prev_vote_choice_cdd","ideo_scaled_inverted_0to1","age","age_sq","male","total_counts","income_scaled","educ_scaled","abs_rile_change")]
t <- t[complete.cases(t),]
pos_change_mod <- lrm(loyal_vote ~ neg_share_all_other_only*ideo_scaled_inverted_0to1 + age+ age_sq + male  +income_scaled+educ_scaled +total_counts + abs_rile_change, data=t, x=T, y=T)
pos_change_mod <- robcov(pos_change_mod, t$prev_vote_choice_cdd, method="huber")

t <- study1b[,c("neg_share_all_other_only", "loyal_vote","prev_vote_choice_cdd","ideo_scaled_inverted_0to1","age","age_sq","male","total_counts","income_scaled","educ_scaled","perceived_distance","abs_rile_change")]
t <- t[complete.cases(t),]
distance_pos_change_mod <- lrm(loyal_vote ~ neg_share_all_other_only*ideo_scaled_inverted_0to1 + age+ age_sq + male  +income_scaled+educ_scaled +total_counts + perceived_distance + abs_rile_change, data=t, x=T, y=T)
distance_pos_change_mod <- robcov(distance_pos_change_mod, t$prev_vote_choice_cdd, method="huber")

t <- study1b[,c("neg_share_all_other_only", "loyal_vote","prev_vote_choice_cdd","ideo_scaled_inverted_0to1","age","age_sq","male","total_counts","income_scaled","educ_scaled","perceived_distance","abs_rile_change","gdp","enp_votes")]
t <- t[complete.cases(t),]
all_controls_mod <- lrm(loyal_vote ~ neg_share_all_other_only*ideo_scaled_inverted_0to1 + age+ age_sq + male  +income_scaled+educ_scaled +total_counts + perceived_distance + abs_rile_change+gdp + enp_votes, data=t, x=T, y=T)
all_controls_mod <- robcov(all_controls_mod, t$prev_vote_choice_cdd, method="huber")

t <- study1b[,c("neg_share_all_other_only", "loyal_vote","prev_vote_choice_cdd","ideo_scaled_inverted_0to1","age","age_sq","male","total_counts","income_scaled","educ_scaled","perceived_distance","abs_rile_change","gdp","left_num")]
t <- t[complete.cases(t),]
all_controls_mod2 <- lrm(loyal_vote ~ neg_share_all_other_only*ideo_scaled_inverted_0to1 + age+ age_sq + male  +income_scaled+educ_scaled +total_counts + perceived_distance + abs_rile_change+gdp + left_num, data=t, x=T, y=T)
all_controls_mod2 <- robcov(all_controls_mod2, t$prev_vote_choice_cdd, method="huber")

texreg(list(distance_mod,pos_change_mod,distance_pos_change_mod,all_controls_mod,all_controls_mod2),stars=c(0.01,0.05,0.1))

#######################
### Table OA4.8
#######################

t <- study1b[,c("neg_share_all_other_only", "loyal_vote","prev_vote_choice_cdd","ideo_scaled_inverted_0to1","age","age_sq","male","total_counts","income_scaled","educ_scaled","lastgovt")]
t <- t[complete.cases(t),]
control_inter_0to1_lastgovt_controlled <- lrm(loyal_vote ~ neg_share_all_other_only*ideo_scaled_inverted_0to1  + age+ age_sq + male  +income_scaled+educ_scaled +total_counts + lastgovt, data=t, x=T, y=T)
control_inter_0to1_lastgovt_controlled <- robcov(control_inter_0to1_lastgovt_controlled, t$prev_vote_choice_cdd, method="huber")

t <- study1b[,c("neg_share_all_other_only", "loyal_vote","prev_vote_choice_cdd","ideo_scaled_inverted_0to1","age","age_sq","male","total_counts","income_scaled","educ_scaled","pm_party")]
t <- t[complete.cases(t),]
t$pm_party <- as.numeric(t$pm_party)
control_inter_0to1_pm_party_controlled <- lrm(loyal_vote ~ neg_share_all_other_only*ideo_scaled_inverted_0to1  + age+ age_sq + male  +income_scaled+educ_scaled +total_counts + pm_party, data=t, x=T, y=T)
control_inter_0to1_pm_party_controlled <- robcov(control_inter_0to1_pm_party_controlled, t$prev_vote_choice_cdd, method="huber")

texreg(list(control_inter_0to1_lastgovt_controlled,control_inter_0to1_pm_party_controlled),stars=c(0.01,0.05,0.1))

t <- subset(study1b, country_election!="NLD_2010")
t <- t[,c("neg_share_all_other_only", "loyal_vote","prev_vote_choice_cdd","ideo_scaled_inverted_0to1","age","age_sq","male","total_counts","income_scaled","educ_scaled","lastgovt")]
t <- t[complete.cases(t),]
control_inter_0to1_lastgovt_controlled <- lrm(loyal_vote ~ neg_share_all_other_only*ideo_scaled_inverted_0to1  + age+ age_sq + male  +income_scaled+educ_scaled +total_counts + lastgovt, data=t, x=T, y=T)
control_inter_0to1_lastgovt_controlled <- robcov(control_inter_0to1_lastgovt_controlled, t$prev_vote_choice_cdd, method="huber")
control_inter_0to1_lastgovt_controlled

#######################
### Table OA4.9
#######################

# Using lastgovt
t <- study1b[,c("neg_share_all_other_only", "loyal_vote","prev_vote_choice_cdd","ideo_scaled_inverted_0to1","age","age_sq","male","total_counts","income_scaled","educ_scaled","lastgovt")]
t <- subset(t, lastgovt==1)
t <- t[complete.cases(t),]
control_inter_0to1_lastgovt_only <- lrm(loyal_vote ~ neg_share_all_other_only*ideo_scaled_inverted_0to1  + age+ age_sq + male  +income_scaled+educ_scaled +total_counts, data=t, x=T, y=T)
control_inter_0to1_lastgovt_only <- robcov(control_inter_0to1_lastgovt_only, t$prev_vote_choice_cdd, method="huber")

# Using pm_party
t <- study1b[,c("neg_share_all_other_only", "loyal_vote","prev_vote_choice_cdd","ideo_scaled_inverted_0to1","age","age_sq","male","total_counts","income_scaled","educ_scaled","pm_party")]
t <- subset(t, pm_party==1)
t <- t[complete.cases(t),]
t$pm_party <- as.numeric(t$pm_party)
control_inter_0to1_pm_party_only <- lrm(loyal_vote ~ neg_share_all_other_only*ideo_scaled_inverted_0to1  + age+ age_sq + male  +income_scaled+educ_scaled +total_counts, data=t, x=T, y=T, tol=1e-8)
control_inter_0to1_pm_party_only <- robcov(control_inter_0to1_pm_party_only, t$prev_vote_choice_cdd, method="huber")

# Using lastgovt
t <- study1b[,c("neg_share_all_other_only", "loyal_vote","prev_vote_choice_cdd","ideo_scaled_inverted_0to1","age","age_sq","male","total_counts","income_scaled","educ_scaled","lastgovt")]
t <- subset(t, lastgovt==0)
t <- t[complete.cases(t),]
control_inter_0to1_nonlastgovt_only <- lrm(loyal_vote ~ neg_share_all_other_only*ideo_scaled_inverted_0to1  + age+ age_sq + male  +income_scaled+educ_scaled +total_counts, data=t, x=T, y=T)
control_inter_0to1_nonlastgovt_only <- robcov(control_inter_0to1_nonlastgovt_only, t$prev_vote_choice_cdd, method="huber")

# Using pm_party
t <- study1b[,c("neg_share_all_other_only", "loyal_vote","prev_vote_choice_cdd","ideo_scaled_inverted_0to1","age","age_sq","male","total_counts","income_scaled","educ_scaled","pm_party")]
t <- subset(t, pm_party==0)
t <- t[complete.cases(t),]
t$pm_party <- as.numeric(t$pm_party)
control_inter_0to1_non_pm_party_only <- lrm(loyal_vote ~ neg_share_all_other_only*ideo_scaled_inverted_0to1  + age+ age_sq + male  +income_scaled+educ_scaled +total_counts, data=t, x=T, y=T)
control_inter_0to1_non_pm_party_only <- robcov(control_inter_0to1_non_pm_party_only, t$prev_vote_choice_cdd, method="huber")

texreg(list(control_inter_0to1_lastgovt_only, control_inter_0to1_pm_party_only, control_inter_0to1_nonlastgovt_only,control_inter_0to1_non_pm_party_only), stars=c(0.01,0.05,0.1))
