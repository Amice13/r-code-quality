
##Who falsifies - confidence
##File for calculation of subgroup falsification of confidence item
dim(high.income)

####High income v low income
high.fit.list <- ictreg(y_confidence ~ YearBirth + Education + Gender + PartyMember + GovProcure, 
                        data = high.income, treat = "treat_confidence", J = 4, method = "lm")

high.fit.sens <- glm(ConfidenceDirect ~ YearBirth + Education + Gender + PartyMember + GovProcure,
                     data = high.income, family = binomial("logit"))

avg.pred.social.desirability.high <- predict.ictreg(high.fit.list,
                                             direct.glm = high.fit.sens, se.fit = TRUE)


low.fit.list <- ictreg(y_confidence ~ YearBirth + Education + Gender + PartyMember + GovProcure, 
                       data = low.income, treat = "treat_confidence", J = 4, method = "lm")

low.fit.sens <- glm(ConfidenceDirect ~ YearBirth + Education + Gender + PartyMember + GovProcure,
                    data = low.income, family = binomial("logit"))

avg.pred.social.desirability.low <- predict(low.fit.list,
                                            direct.glm = low.fit.sens, se.fit = TRUE)


#This extracts the difference between the direct and indirect questioning with CIs based on the predict function calculations
high.confidence.falsify <- c(avg.pred.social.desirability.high[[1]][3,1],avg.pred.social.desirability.high[[1]][3,2],avg.pred.social.desirability.high[[1]][3,3])
low.confidence.falsify <- c(avg.pred.social.desirability.low[[1]][3,1],avg.pred.social.desirability.low[[1]][3,2],avg.pred.social.desirability.low[[1]][3,3])





####Young v Old
young.fit.list <- ictreg(y_confidence ~ Income + Education + Gender + Hukou + PartyMember + GovProcure, 
                         data = youngest, treat = "treat_confidence", J = 4, method = "lm")

young.fit.sens <- glm(ConfidenceDirect ~ Income + Education + Gender + Hukou + PartyMember + GovProcure,
                      data = youngest, family = binomial("logit"))

avg.pred.social.desirability.young <- predict(young.fit.list,
                                              direct.glm = young.fit.sens, se.fit = TRUE)



old.fit.list <- ictreg(y_confidence ~ Income + Education + Gender + Hukou + PartyMember + GovProcure, 
                       data = oldest, treat = "treat_confidence", J = 4, method = "lm")

old.fit.sens <- glm(ConfidenceDirect ~ Income + Education + Gender + Hukou + PartyMember + GovProcure,
                    data = oldest, family = binomial("logit"))

avg.pred.social.desirability.old <- predict(old.fit.list,
                                            direct.glm = old.fit.sens, se.fit = TRUE)


#This extracts the difference between the direct and indirect questioning with CIs based on the predict function calculations
young.confidence.falsify <- c(avg.pred.social.desirability.young[[1]][3,1],avg.pred.social.desirability.young[[1]][3,2],avg.pred.social.desirability.young[[1]][3,3])
old.confidence.falsify <- c(avg.pred.social.desirability.old[[1]][3,1],avg.pred.social.desirability.old[[1]][3,2],avg.pred.social.desirability.old[[1]][3,3])




####Uni v J College
uni.fit.list <- ictreg(y_confidence ~ YearBirth + Income + Gender + Hukou + PartyMember + GovProcure, 
                       data = uni, treat = "treat_confidence", J = 4, method = "lm")

uni.fit.sens <- glm(ConfidenceDirect ~ YearBirth + Income + Gender + Hukou + PartyMember + GovProcure,
                    data = uni, family = binomial("logit"))

avg.pred.social.desirability.uni <- predict(uni.fit.list,
                                            direct.glm = uni.fit.sens, se.fit = TRUE)



jc.fit.list <- ictreg(y_confidence ~ YearBirth + Income + Gender + Hukou + PartyMember + GovProcure, 
                      data = junior.college, treat = "treat_confidence", J = 4, method = "lm")

jc.fit.sens <- glm(ConfidenceDirect ~ YearBirth + Income + Gender + Hukou + PartyMember + GovProcure,
                   data = junior.college, family = binomial("logit"))

avg.pred.social.desirability.jc <- predict(jc.fit.list,
                                           direct.glm = jc.fit.sens, se.fit = TRUE)


#This extracts the difference between the direct and indirect questioning with CIs based on the predict function calculations
uni.confidence.falsify <- c(avg.pred.social.desirability.uni[[1]][3,1],avg.pred.social.desirability.uni[[1]][3,2],avg.pred.social.desirability.uni[[1]][3,3])
jc.confidence.falsify <- c(avg.pred.social.desirability.jc[[1]][3,1],avg.pred.social.desirability.jc[[1]][3,2],avg.pred.social.desirability.jc[[1]][3,3])
uni.confidence.falsify
jc.confidence.falsify

####Men v Women
men.fit.list <- ictreg(y_confidence ~ YearBirth + Income + Education + Hukou + PartyMember + GovProcure, 
                       data = men, treat = "treat_confidence", J = 4, method = "lm")

men.fit.sens <- glm(ConfidenceDirect ~ YearBirth + Income + Education + Hukou + PartyMember + GovProcure,
                    data = men, family = binomial("logit"))

avg.pred.social.desirability.men <- predict(men.fit.list,
                                            direct.glm = men.fit.sens, se.fit = TRUE)



women.fit.list <- ictreg(y_confidence ~ YearBirth + Education + Income + Hukou + PartyMember + GovProcure, 
                         data = women, treat = "treat_confidence", J = 4, method = "lm")

women.fit.sens <- glm(ConfidenceDirect ~ YearBirth + Income + Education + Hukou + PartyMember + GovProcure,
                      data = women, family = binomial("logit"))

avg.pred.social.desirability.women <- predict(women.fit.list,
                                              direct.glm = women.fit.sens, se.fit = TRUE)


#This extracts the difference between the direct and indirect questioning with CIs based on the predict function calculations
men.confidence.falsify <- c(avg.pred.social.desirability.men[[1]][3,1],avg.pred.social.desirability.men[[1]][3,2],avg.pred.social.desirability.men[[1]][3,3])
women.confidence.falsify <- c(avg.pred.social.desirability.women[[1]][3,1],avg.pred.social.desirability.women[[1]][3,2],avg.pred.social.desirability.women[[1]][3,3])




####Party v NonParty
party.fit.list <- ictreg(y_confidence ~ YearBirth + Income + Education + Hukou + Gender + GovProcure, 
                         data = party.member, treat = "treat_confidence", J = 4, method = "lm")

party.fit.sens <- glm(ConfidenceDirect ~ YearBirth + Income + Education + Hukou + Gender + GovProcure,
                      data = party.member, family = binomial("logit"))

avg.pred.social.desirability.party <- predict(party.fit.list,
                                              direct.glm = party.fit.sens, se.fit = TRUE)



nonparty.fit.list <- ictreg(y_confidence ~ YearBirth + Education + Income + Hukou + Gender + GovProcure, 
                            data = not.party.member, treat = "treat_confidence", J = 4, method = "lm")

nonparty.fit.sens <- glm(ConfidenceDirect ~ YearBirth + Income + Education + Hukou + Gender + GovProcure,
                         data = not.party.member, family = binomial("logit"))

avg.pred.social.desirability.nonparty <- predict(nonparty.fit.list,
                                                 direct.glm = nonparty.fit.sens, se.fit = TRUE)


#This extracts the difference between the direct and indirect questioning with CIs based on the predict function calculations
party.confidence.falsify <- c(avg.pred.social.desirability.party[[1]][3,1],avg.pred.social.desirability.party[[1]][3,2],avg.pred.social.desirability.party[[1]][3,3])
nonparty.confidence.falsify <- c(avg.pred.social.desirability.nonparty[[1]][3,1],avg.pred.social.desirability.nonparty[[1]][3,2],avg.pred.social.desirability.nonparty[[1]][3,3])


####Rural v Urban
rural.fit.list <- ictreg(y_confidence ~ YearBirth + Income + Education + Gender + PartyMember + GovProcure, 
                         data = rural, treat = "treat_confidence", J = 4, method = "lm")

rural.fit.sens <- glm(ConfidenceDirect ~ YearBirth + Income + Education + Gender + PartyMember + GovProcure,
                      data = rural, family = binomial("logit"))

avg.pred.social.desirability.rural <- predict(rural.fit.list,
                                              direct.glm = rural.fit.sens, se.fit = TRUE)



urban.fit.list <- ictreg(y_confidence ~ YearBirth + Education + Income + Gender + PartyMember + GovProcure, 
                         data = urban, treat = "treat_confidence", J = 4, method = "lm")

urban.fit.sens <- glm(ConfidenceDirect ~ YearBirth + Income + Education + Gender + PartyMember + GovProcure,
                      data = urban, family = binomial("logit"))

avg.pred.social.desirability.urban <- predict(urban.fit.list,
                                              direct.glm = urban.fit.sens, se.fit = TRUE)


#This extracts the difference between the direct and indirect questioning with CIs based on the predict function calculations
rural.confidence.falsify <- c(avg.pred.social.desirability.rural[[1]][3,1],avg.pred.social.desirability.rural[[1]][3,2],avg.pred.social.desirability.rural[[1]][3,3])
urban.confidence.falsify <- c(avg.pred.social.desirability.urban[[1]][3,1],avg.pred.social.desirability.urban[[1]][3,2],avg.pred.social.desirability.urban[[1]][3,3])



####Gov commission v not gov
gov.fit.list <- ictreg(y_confidence ~ YearBirth + Income + Education + Gender + PartyMember + Hukou, 
                       data = gov.commission, treat = "treat_confidence", J = 4, method = "lm")

gov.fit.sens <- glm(ConfidenceDirect ~ YearBirth + Income + Education + Gender + PartyMember + Hukou,
                    data = gov.commission, family = binomial("logit"))

avg.pred.social.desirability.gov <- predict(gov.fit.list,
                                            direct.glm = gov.fit.sens, se.fit = TRUE)



nongov.fit.list <- ictreg(y_confidence ~ YearBirth + Education + Income + Gender + PartyMember + Hukou, 
                          data = not.gov.commission, treat = "treat_confidence", J = 4, method = "lm")

nongov.fit.sens <- glm(ConfidenceDirect ~ YearBirth + Income + Education + Gender + PartyMember + Hukou,
                       data = not.gov.commission, family = binomial("logit"))

avg.pred.social.desirability.nongov <- predict(nongov.fit.list,
                                               direct.glm = nongov.fit.sens, se.fit = TRUE)


#This extracts the difference between the direct and indirect questioning with CIs based on the predict function calculations
gov.confidence.falsify <- c(avg.pred.social.desirability.gov[[1]][3,1],avg.pred.social.desirability.gov[[1]][3,2],avg.pred.social.desirability.gov[[1]][3,3])
nongov.confidence.falsify <- c(avg.pred.social.desirability.nongov[[1]][3,1],avg.pred.social.desirability.nongov[[1]][3,2],avg.pred.social.desirability.nongov[[1]][3,3])







###Forest Plot figure 

label <- c("High Income", "Low Income", "Young", "Old", "Uni or above", "JColl or below", 
           "Men", "Women", "Party Member", "Non Member", "Rural", "Urban", "Gov Procure", "Nongov Procure")

mean <- c(high.confidence.falsify[1], low.confidence.falsify[1], young.confidence.falsify[1], old.confidence.falsify[1], 
          uni.confidence.falsify[1], jc.confidence.falsify[1], men.confidence.falsify[1], women.confidence.falsify[1],
          party.confidence.falsify[1], nonparty.confidence.falsify[1], rural.confidence.falsify[1], urban.confidence.falsify[1], 
          gov.confidence.falsify[1], nongov.confidence.falsify[1])

lower <-  c(high.confidence.falsify[2], low.confidence.falsify[2], young.confidence.falsify[2], old.confidence.falsify[2], 
            uni.confidence.falsify[2], jc.confidence.falsify[2], men.confidence.falsify[2], women.confidence.falsify[2],
            party.confidence.falsify[2], nonparty.confidence.falsify[2], rural.confidence.falsify[2], urban.confidence.falsify[2], 
            gov.confidence.falsify[2], nongov.confidence.falsify[2])

upper <- c(high.confidence.falsify[3], low.confidence.falsify[3], young.confidence.falsify[3], old.confidence.falsify[3], 
           uni.confidence.falsify[3], jc.confidence.falsify[3], men.confidence.falsify[3], women.confidence.falsify[3],
           party.confidence.falsify[3], nonparty.confidence.falsify[3], rural.confidence.falsify[3], urban.confidence.falsify[3], 
           gov.confidence.falsify[3], nongov.confidence.falsify[3])

df <- data.frame(label, mean, lower, upper) #create a dataframe with above vectors
df$label <- factor(df$label, levels=(df$label)) #label dataframe




