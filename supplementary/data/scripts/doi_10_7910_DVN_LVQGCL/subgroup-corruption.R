
##Who falsifies - corruption
##File for calculation of subgroup falsification of corruption item


####High income v low income
high.fit.list <- ictreg(y_corruption ~ YearBirth + Education + Gender + Hukou + PartyMember + GovProcure, 
                        data = high.income, treat = "treat_corruption", J = 4, method = "lm")

high.fit.sens <- glm(CorruptionDirect ~ YearBirth + Education + Gender + Hukou + PartyMember + GovProcure,
                     data = high.income, family = binomial("logit"))

avg.pred.social.desirability.high <- predict(high.fit.list,
                                             direct.glm = high.fit.sens, se.fit = TRUE)

predict(high.fit.sens, se.fit = TRUE)


low.fit.list <- ictreg(y_corruption ~ YearBirth + Education + Gender + Hukou + PartyMember + GovProcure, 
                       data = low.income, treat = "treat_corruption", J = 4, method = "lm")

low.fit.sens <- glm(CorruptionDirect ~ YearBirth + Education + Gender + Hukou + PartyMember + GovProcure,
                    data = low.income, family = binomial("logit"))

avg.pred.social.desirability.low <- predict(low.fit.list,
                                            direct.glm = low.fit.sens, se.fit = TRUE)


#This extracts the difference between the direct and indirect questioning with CIs based on the predict function calculations
high.corruption.falsify <- c(avg.pred.social.desirability.high[[1]][3,1],avg.pred.social.desirability.high[[1]][3,2],avg.pred.social.desirability.high[[1]][3,3])
low.corruption.falsify <- c(avg.pred.social.desirability.low[[1]][3,1],avg.pred.social.desirability.low[[1]][3,2],avg.pred.social.desirability.low[[1]][3,3])





####Young v Old
young.fit.list <- ictreg(y_corruption ~ Income + Education + Gender + Hukou + PartyMember + GovProcure, 
                         data = youngest, treat = "treat_corruption", J = 4, method = "lm")

young.fit.sens <- glm(CorruptionDirect ~ Income + Education + Gender + Hukou + PartyMember + GovProcure,
                      data = youngest, family = binomial("logit"))

avg.pred.social.desirability.young <- predict(young.fit.list,
                                              direct.glm = young.fit.sens, se.fit = TRUE)



old.fit.list <- ictreg(y_corruption ~ Income + Education + Gender + Hukou + PartyMember + GovProcure, 
                       data = oldest, treat = "treat_corruption", J = 4, method = "lm")

old.fit.sens <- glm(CorruptionDirect ~ Income + Education + Gender + Hukou + PartyMember + GovProcure,
                    data = oldest, family = binomial("logit"))

avg.pred.social.desirability.old <- predict(old.fit.list,
                                            direct.glm = old.fit.sens, se.fit = TRUE)


#This extracts the difference between the direct and indirect questioning with CIs based on the predict function calculations
young.corruption.falsify <- c(avg.pred.social.desirability.young[[1]][3,1],avg.pred.social.desirability.young[[1]][3,2],avg.pred.social.desirability.young[[1]][3,3])
old.corruption.falsify <- c(avg.pred.social.desirability.old[[1]][3,1],avg.pred.social.desirability.old[[1]][3,2],avg.pred.social.desirability.old[[1]][3,3])




####Uni v J College
uni.fit.list <- ictreg(y_corruption ~ YearBirth + Income + Gender + Hukou + PartyMember + GovProcure, 
                       data = uni, treat = "treat_corruption", J = 4, method = "lm")

uni.fit.sens <- glm(CorruptionDirect ~ YearBirth + Income + Gender + Hukou + PartyMember + GovProcure,
                    data = uni, family = binomial("logit"))

avg.pred.social.desirability.uni <- predict(uni.fit.list,
                                            direct.glm = uni.fit.sens, se.fit = TRUE)



jc.fit.list <- ictreg(y_corruption ~ YearBirth + Income + Gender + Hukou + PartyMember + GovProcure, 
                      data = junior.college, treat = "treat_corruption", J = 4, method = "lm")

jc.fit.sens <- glm(CorruptionDirect ~ YearBirth + Income + Gender + Hukou + PartyMember + GovProcure,
                   data = junior.college, family = binomial("logit"))

avg.pred.social.desirability.jc <- predict(jc.fit.list,
                                           direct.glm = jc.fit.sens, se.fit = TRUE)


#This extracts the difference between the direct and indirect questioning with CIs based on the predict function calculations
uni.corruption.falsify <- c(avg.pred.social.desirability.uni[[1]][3,1],avg.pred.social.desirability.uni[[1]][3,2],avg.pred.social.desirability.uni[[1]][3,3])
jc.corruption.falsify <- c(avg.pred.social.desirability.jc[[1]][3,1],avg.pred.social.desirability.jc[[1]][3,2],avg.pred.social.desirability.jc[[1]][3,3])
uni.corruption.falsify
jc.corruption.falsify

####Men v Women
men.fit.list <- ictreg(y_corruption ~ YearBirth + Income + Education + Hukou + PartyMember + GovProcure, 
                       data = men, treat = "treat_corruption", J = 4, method = "lm")

men.fit.sens <- glm(CorruptionDirect ~ YearBirth + Income + Education + Hukou + PartyMember + GovProcure,
                    data = men, family = binomial("logit"))

avg.pred.social.desirability.men <- predict(men.fit.list,
                                            direct.glm = men.fit.sens, se.fit = TRUE)



women.fit.list <- ictreg(y_corruption ~ YearBirth + Education + Income + Hukou + PartyMember + GovProcure, 
                         data = women, treat = "treat_corruption", J = 4, method = "lm")

women.fit.sens <- glm(CorruptionDirect ~ YearBirth + Income + Education + Hukou + PartyMember + GovProcure,
                      data = women, family = binomial("logit"))

avg.pred.social.desirability.women <- predict(women.fit.list,
                                              direct.glm = women.fit.sens, se.fit = TRUE)

#This extracts the difference between the direct and indirect questioning with CIs based on the predict function calculations
men.corruption.falsify <- c(avg.pred.social.desirability.men[[1]][3,1],avg.pred.social.desirability.men[[1]][3,2],avg.pred.social.desirability.men[[1]][3,3])
women.corruption.falsify <- c(avg.pred.social.desirability.women[[1]][3,1],avg.pred.social.desirability.women[[1]][3,2],avg.pred.social.desirability.women[[1]][3,3])




####Party v NonParty
party.fit.list <- ictreg(y_corruption ~ YearBirth + Income + Education + Hukou + Gender + GovProcure, 
                         data = party.member, treat = "treat_corruption", J = 4, method = "lm")

party.fit.sens <- glm(CorruptionDirect ~ YearBirth + Income + Education + Hukou + Gender + GovProcure,
                      data = party.member, family = binomial("logit"))

avg.pred.social.desirability.party <- predict(party.fit.list,
                                              direct.glm = party.fit.sens, se.fit = TRUE)



nonparty.fit.list <- ictreg(y_corruption ~ YearBirth + Education + Income + Hukou + Gender + GovProcure, 
                            data = not.party.member, treat = "treat_corruption", J = 4, method = "lm")

nonparty.fit.sens <- glm(CorruptionDirect ~ YearBirth + Income + Education + Hukou + Gender + GovProcure,
                         data = not.party.member, family = binomial("logit"))

avg.pred.social.desirability.nonparty <- predict(nonparty.fit.list,
                                                 direct.glm = nonparty.fit.sens, se.fit = TRUE)

#This extracts the difference between the direct and indirect questioning with CIs based on the predict function calculations
party.corruption.falsify <- c(avg.pred.social.desirability.party[[1]][3,1],avg.pred.social.desirability.party[[1]][3,2],avg.pred.social.desirability.party[[1]][3,3])
nonparty.corruption.falsify <- c(avg.pred.social.desirability.nonparty[[1]][3,1],avg.pred.social.desirability.nonparty[[1]][3,2],avg.pred.social.desirability.nonparty[[1]][3,3])


####Rural v Urban
rural.fit.list <- ictreg(y_corruption ~ YearBirth + Income + Education + Gender + PartyMember + GovProcure, 
                         data = rural, treat = "treat_corruption", J = 4, method = "lm")

rural.fit.sens <- glm(CorruptionDirect ~ YearBirth + Income + Education + Gender + PartyMember + GovProcure,
                      data = rural, family = binomial("logit"))

avg.pred.social.desirability.rural <- predict(rural.fit.list,
                                              direct.glm = rural.fit.sens, se.fit = TRUE)



urban.fit.list <- ictreg(y_corruption ~ YearBirth + Education + Income + Gender + PartyMember + GovProcure, 
                         data = urban, treat = "treat_corruption", J = 4, method = "lm")

urban.fit.sens <- glm(CorruptionDirect ~ YearBirth + Income + Education + Gender + PartyMember + GovProcure,
                      data = urban, family = binomial("logit"))

avg.pred.social.desirability.urban <- predict(urban.fit.list,
                                              direct.glm = urban.fit.sens, se.fit = TRUE)

#This extracts the difference between the direct and indirect questioning with CIs based on the predict function calculations
rural.corruption.falsify <- c(avg.pred.social.desirability.rural[[1]][3,1],avg.pred.social.desirability.rural[[1]][3,2],avg.pred.social.desirability.rural[[1]][3,3])
urban.corruption.falsify <- c(avg.pred.social.desirability.urban[[1]][3,1],avg.pred.social.desirability.urban[[1]][3,2],avg.pred.social.desirability.urban[[1]][3,3])



####Gov commission v not gov
gov.fit.list <- ictreg(y_corruption ~ YearBirth + Income + Education + Gender + PartyMember + Hukou, 
                       data = gov.commission, treat = "treat_corruption", J = 4, method = "lm")

gov.fit.sens <- glm(CorruptionDirect ~ YearBirth + Income + Education + Gender + PartyMember + Hukou,
                    data = gov.commission, family = binomial("logit"))

avg.pred.social.desirability.gov <- predict(gov.fit.list,
                                            direct.glm = gov.fit.sens, se.fit = TRUE)



nongov.fit.list <- ictreg(y_corruption ~ YearBirth + Education + Income + Gender + PartyMember + Hukou, 
                          data = not.gov.commission, treat = "treat_corruption", J = 4, method = "lm")

nongov.fit.sens <- glm(CorruptionDirect ~ YearBirth + Income + Education + Gender + PartyMember + Hukou,
                       data = not.gov.commission, family = binomial("logit"))

avg.pred.social.desirability.nongov <- predict(nongov.fit.list,
                                               direct.glm = nongov.fit.sens, se.fit = TRUE)

#This extracts the difference between the direct and indirect questioning with CIs based on the predict function calculations
gov.corruption.falsify <- c(avg.pred.social.desirability.gov[[1]][3,1],avg.pred.social.desirability.gov[[1]][3,2],avg.pred.social.desirability.gov[[1]][3,3])
nongov.corruption.falsify <- c(avg.pred.social.desirability.nongov[[1]][3,1],avg.pred.social.desirability.nongov[[1]][3,2],avg.pred.social.desirability.nongov[[1]][3,3])







###Forest Plot figure 

label <- c("High Income", "Low Income", "Young", "Old", "Uni or above", "JColl or below", 
           "Men", "Women", "Party Member", "Non Member", "Rural", "Urban", "Gov Procure", "Nongov Procure")

mean <- c(high.corruption.falsify[1], low.corruption.falsify[1], young.corruption.falsify[1], old.corruption.falsify[1], 
          uni.corruption.falsify[1], jc.corruption.falsify[1], men.corruption.falsify[1], women.corruption.falsify[1],
          party.corruption.falsify[1], nonparty.corruption.falsify[1], rural.corruption.falsify[1], urban.corruption.falsify[1], 
          gov.corruption.falsify[1], nongov.corruption.falsify[1])

lower <-  c(high.corruption.falsify[2], low.corruption.falsify[2], young.corruption.falsify[2], old.corruption.falsify[2], 
            uni.corruption.falsify[2], jc.corruption.falsify[2], men.corruption.falsify[2], women.corruption.falsify[2],
            party.corruption.falsify[2], nonparty.corruption.falsify[2], rural.corruption.falsify[2], urban.corruption.falsify[2], 
            gov.corruption.falsify[2], nongov.corruption.falsify[2])

upper <- c(high.corruption.falsify[3], low.corruption.falsify[3], young.corruption.falsify[3], old.corruption.falsify[3], 
           uni.corruption.falsify[3], jc.corruption.falsify[3], men.corruption.falsify[3], women.corruption.falsify[3],
           party.corruption.falsify[3], nonparty.corruption.falsify[3], rural.corruption.falsify[3], urban.corruption.falsify[3], 
           gov.corruption.falsify[3], nongov.corruption.falsify[3])

df <- data.frame(label, mean, lower, upper) #create a dataframe with above vectors
df$label <- factor(df$label, levels=(df$label)) #label dataframe






