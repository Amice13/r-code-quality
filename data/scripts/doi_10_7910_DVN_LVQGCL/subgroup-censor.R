
##Who falsifies - censor
##File for calculation of subgroup falsification of censorship item

####High income v low income
high.fit.list <- ictreg(y_censor ~ YearBirth + Education + Gender + Hukou + PartyMember + GovProcure, 
                        data = high.income, treat = "treat_censor", J = 4, method = "lm")

high.fit.sens <- glm(CensorDirect ~ YearBirth + Education + Gender + Hukou + PartyMember + GovProcure,
                     data = high.income, family = binomial("logit"))

avg.pred.social.desirability.high <- predict(high.fit.list,
                                             direct.glm = high.fit.sens, se.fit = TRUE)



low.fit.list <- ictreg(y_censor ~ YearBirth + Education + Gender + Hukou + PartyMember + GovProcure, 
                       data = low.income, treat = "treat_censor", J = 4, method = "lm")

low.fit.sens <- glm(CensorDirect ~ YearBirth + Education + Gender + Hukou + PartyMember + GovProcure,
                    data = low.income, family = binomial("logit"))

avg.pred.social.desirability.low <- predict(low.fit.list,
                                            direct.glm = low.fit.sens, se.fit = TRUE)


#This extracts the difference between the direct and indirect questioning with CIs based on the predict function calculations
high.censor.falsify <- c(avg.pred.social.desirability.high[[1]][3,1],avg.pred.social.desirability.high[[1]][3,2],avg.pred.social.desirability.high[[1]][3,3])
low.censor.falsify <- c(avg.pred.social.desirability.low[[1]][3,1],avg.pred.social.desirability.low[[1]][3,2],avg.pred.social.desirability.low[[1]][3,3])





####Young v Old
young.fit.list <- ictreg(y_censor ~ Income + Education + Gender + Hukou + PartyMember + GovProcure, 
                         data = youngest, treat = "treat_censor", J = 4, method = "lm")

young.fit.sens <- glm(CensorDirect ~ Income + Education + Gender + Hukou + PartyMember + GovProcure,
                      data = youngest, family = binomial("logit"))

avg.pred.social.desirability.young <- predict(young.fit.list,
                                              direct.glm = young.fit.sens, se.fit = TRUE)



old.fit.list <- ictreg(y_censor ~ Income + Education + Gender + Hukou + PartyMember + GovProcure, 
                       data = oldest, treat = "treat_censor", J = 4, method = "lm")

old.fit.sens <- glm(CensorDirect ~ Income + Education + Gender + Hukou + PartyMember + GovProcure,
                    data = oldest, family = binomial("logit"))

avg.pred.social.desirability.old <- predict(old.fit.list,
                                            direct.glm = old.fit.sens, se.fit = TRUE)


#This extracts the difference between the direct and indirect questioning with CIs based on the predict function calculations
young.censor.falsify <- c(avg.pred.social.desirability.young[[1]][3,1],avg.pred.social.desirability.young[[1]][3,2],avg.pred.social.desirability.young[[1]][3,3])
old.censor.falsify <- c(avg.pred.social.desirability.old[[1]][3,1],avg.pred.social.desirability.old[[1]][3,2],avg.pred.social.desirability.old[[1]][3,3])




####Uni v J College
uni.fit.list <- ictreg(y_censor ~ YearBirth + Income + Gender + Hukou + PartyMember + GovProcure, 
                       data = uni, treat = "treat_censor", J = 4, method = "lm")

uni.fit.sens <- glm(CensorDirect ~ YearBirth + Income + Gender + Hukou + PartyMember + GovProcure,
                    data = uni, family = binomial("logit"))

avg.pred.social.desirability.uni <- predict(uni.fit.list,
                                            direct.glm = uni.fit.sens, se.fit = TRUE)



jc.fit.list <- ictreg(y_censor ~ YearBirth + Income + Gender + Hukou + PartyMember + GovProcure, 
                      data = junior.college, treat = "treat_censor", J = 4, method = "lm")

jc.fit.sens <- glm(CensorDirect ~ YearBirth + Income + Gender + Hukou + PartyMember + GovProcure,
                   data = junior.college, family = binomial("logit"))

avg.pred.social.desirability.jc <- predict(jc.fit.list,
                                           direct.glm = jc.fit.sens, se.fit = TRUE)


#This extracts the difference between the direct and indirect questioning with CIs based on the predict function calculations
uni.censor.falsify <- c(avg.pred.social.desirability.uni[[1]][3,1],avg.pred.social.desirability.uni[[1]][3,2],avg.pred.social.desirability.uni[[1]][3,3])
jc.censor.falsify <- c(avg.pred.social.desirability.jc[[1]][3,1],avg.pred.social.desirability.jc[[1]][3,2],avg.pred.social.desirability.jc[[1]][3,3])


####Men v Women
men.fit.list <- ictreg(y_censor ~ YearBirth + Income + Education + Hukou + PartyMember + GovProcure, 
                       data = men, treat = "treat_censor", J = 4, method = "lm")

men.fit.sens <- glm(CensorDirect ~ YearBirth + Income + Education + Hukou + PartyMember + GovProcure,
                    data = men, family = binomial("logit"))

avg.pred.social.desirability.men <- predict(men.fit.list,
                                            direct.glm = men.fit.sens, se.fit = TRUE)



women.fit.list <- ictreg(y_censor ~ YearBirth + Education + Income + Hukou + PartyMember + GovProcure, 
                         data = women, treat = "treat_censor", J = 4, method = "lm")

women.fit.sens <- glm(CensorDirect ~ YearBirth + Income + Education + Hukou + PartyMember + GovProcure,
                      data = women, family = binomial("logit"))

avg.pred.social.desirability.women <- predict(women.fit.list,
                                              direct.glm = women.fit.sens, se.fit = TRUE)

#This extracts the difference between the direct and indirect questioning with CIs based on the predict function calculations
men.censor.falsify <- c(avg.pred.social.desirability.men[[1]][3,1],avg.pred.social.desirability.men[[1]][3,2],avg.pred.social.desirability.men[[1]][3,3])
women.censor.falsify <- c(avg.pred.social.desirability.women[[1]][3,1],avg.pred.social.desirability.women[[1]][3,2],avg.pred.social.desirability.women[[1]][3,3])




####Party v NonParty
party.fit.list <- ictreg(y_censor ~ YearBirth + Income + Education + Hukou + Gender + GovProcure, 
                         data = party.member, treat = "treat_censor", J = 4, method = "lm")

party.fit.sens <- glm(CensorDirect ~ YearBirth + Income + Education + Hukou + Gender + GovProcure,
                      data = party.member, family = binomial("logit"))

avg.pred.social.desirability.party <- predict(party.fit.list,
                                              direct.glm = party.fit.sens, se.fit = TRUE)



nonparty.fit.list <- ictreg(y_censor ~ YearBirth + Education + Income + Hukou + Gender + GovProcure, 
                            data = not.party.member, treat = "treat_censor", J = 4, method = "lm")

nonparty.fit.sens <- glm(CensorDirect ~ YearBirth + Income + Education + Hukou + Gender + GovProcure,
                         data = not.party.member, family = binomial("logit"))

avg.pred.social.desirability.nonparty <- predict(nonparty.fit.list,
                                                 direct.glm = nonparty.fit.sens, se.fit = TRUE)


    ####Bivariate
    party.fit.list.biv <- ictreg(y_censor ~ 1, 
                             data = party.member, treat = "treat_censor", J = 4, method = "lm")
    
    party.fit.sens.biv <- glm(CensorDirect ~ 1,
                          data = party.member, family = binomial("logit"))
    
    avg.pred.social.desirability.party.biv <- predict(party.fit.list.biv,
                                                  direct.glm = party.fit.sens.biv, se.fit = TRUE)
    
    
    
    nonparty.fit.list.biv <- ictreg(y_censor ~ 1, 
                                data = not.party.member, treat = "treat_censor", J = 4, method = "lm")
    
    nonparty.fit.sens.biv <- glm(CensorDirect ~ 1,
                             data = not.party.member, family = binomial("logit"))
    
    avg.pred.social.desirability.nonparty.biv <- predict(nonparty.fit.list.biv,
                                                     direct.glm = nonparty.fit.sens.biv, se.fit = TRUE)
    

#This extracts the difference between the direct and indirect questioning with CIs based on the predict function calculations
party.censor.falsify <- c(avg.pred.social.desirability.party[[1]][3,1],avg.pred.social.desirability.party[[1]][3,2],avg.pred.social.desirability.party[[1]][3,3])
nonparty.censor.falsify <- c(avg.pred.social.desirability.nonparty[[1]][3,1],avg.pred.social.desirability.nonparty[[1]][3,2],avg.pred.social.desirability.nonparty[[1]][3,3])


####Rural v Urban
rural.fit.list <- ictreg(y_censor ~ YearBirth + Income + Education + Gender + PartyMember + GovProcure, 
                         data = rural, treat = "treat_censor", J = 4, method = "lm")

rural.fit.sens <- glm(CensorDirect ~ YearBirth + Income + Education + Gender + PartyMember + GovProcure,
                      data = rural, family = binomial("logit"))

avg.pred.social.desirability.rural <- predict(rural.fit.list,
                                              direct.glm = rural.fit.sens, se.fit = TRUE)



urban.fit.list <- ictreg(y_censor ~ YearBirth + Education + Income + Gender + PartyMember + GovProcure, 
                         data = urban, treat = "treat_censor", J = 4, method = "lm")

urban.fit.sens <- glm(CensorDirect ~ YearBirth + Income + Education + Gender + PartyMember + GovProcure,
                      data = urban, family = binomial("logit"))

avg.pred.social.desirability.urban <- predict(urban.fit.list,
                                              direct.glm = urban.fit.sens, se.fit = TRUE)


#This extracts the difference between the direct and indirect questioning with CIs based on the predict function calculations
rural.censor.falsify <- c(avg.pred.social.desirability.rural[[1]][3,1],avg.pred.social.desirability.rural[[1]][3,2],avg.pred.social.desirability.rural[[1]][3,3])
urban.censor.falsify <- c(avg.pred.social.desirability.urban[[1]][3,1],avg.pred.social.desirability.urban[[1]][3,2],avg.pred.social.desirability.urban[[1]][3,3])



####Gov commission v not gov
gov.fit.list <- ictreg(y_censor ~ YearBirth + Income + Education + Gender + PartyMember + Hukou, 
                       data = gov.commission, treat = "treat_censor", J = 4, method = "lm")

gov.fit.sens <- glm(CensorDirect ~ YearBirth + Income + Education + Gender + PartyMember + Hukou,
                    data = gov.commission, family = binomial("logit"))

avg.pred.social.desirability.gov <- predict(gov.fit.list,
                                            direct.glm = gov.fit.sens, se.fit = TRUE)



nongov.fit.list <- ictreg(y_censor ~ YearBirth + Education + Income + Gender + PartyMember + Hukou, 
                          data = not.gov.commission, treat = "treat_censor", J = 4, method = "lm")

nongov.fit.sens <- glm(CensorDirect ~ YearBirth + Income + Education + Gender + PartyMember + Hukou,
                       data = not.gov.commission, family = binomial("logit"))

avg.pred.social.desirability.nongov <- predict(nongov.fit.list,
                                               direct.glm = nongov.fit.sens, se.fit = TRUE)


#This extracts the difference between the direct and indirect questioning with CIs based on the predict function calculations
gov.censor.falsify <- c(avg.pred.social.desirability.gov[[1]][3,1],avg.pred.social.desirability.gov[[1]][3,2],avg.pred.social.desirability.gov[[1]][3,3])
nongov.censor.falsify <- c(avg.pred.social.desirability.nongov[[1]][3,1],avg.pred.social.desirability.nongov[[1]][3,2],avg.pred.social.desirability.nongov[[1]][3,3])







# pref dataframe for forest plot

label <- c("High Income", "Low Income", "Young", "Old", "Uni or above", "JColl or below", 
           "Men", "Women", "Party Member", "Non Member", "Rural", "Urban", "Gov Procure", "Nongov Procure")

mean <- c(high.censor.falsify[1], low.censor.falsify[1], young.censor.falsify[1], old.censor.falsify[1], 
          uni.censor.falsify[1], jc.censor.falsify[1], men.censor.falsify[1], women.censor.falsify[1],
          party.censor.falsify[1], nonparty.censor.falsify[1], rural.censor.falsify[1], urban.censor.falsify[1], 
          gov.censor.falsify[1], nongov.censor.falsify[1])

lower <-  c(high.censor.falsify[2], low.censor.falsify[2], young.censor.falsify[2], old.censor.falsify[2], 
            uni.censor.falsify[2], jc.censor.falsify[2], men.censor.falsify[2], women.censor.falsify[2],
            party.censor.falsify[2], nonparty.censor.falsify[2], rural.censor.falsify[2], urban.censor.falsify[2], 
            gov.censor.falsify[2], nongov.censor.falsify[2])

upper <- c(high.censor.falsify[3], low.censor.falsify[3], young.censor.falsify[3], old.censor.falsify[3], 
           uni.censor.falsify[3], jc.censor.falsify[3], men.censor.falsify[3], women.censor.falsify[3],
           party.censor.falsify[3], nonparty.censor.falsify[3], rural.censor.falsify[3], urban.censor.falsify[3], 
           gov.censor.falsify[3], nongov.censor.falsify[3])

df <- data.frame(label, mean, lower, upper) #create a dataframe with above vectors
df$label <- factor(df$label, levels=(df$label)) #label dataframe





