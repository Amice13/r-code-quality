# file for creating the main falsification estimates 
# called from "main-analysis.R"


####Confidence
fit.list.confidence <- ictreg(y_confidence ~ YearBirth + Education + Gender + Hukou + PartyMember + GovProcure + AutPersonality,
                        data = df, treat = "treat_confidence", J = 4, method = "lm")

fit.sens.confidence <- glm(ConfidenceDirect ~ YearBirth + Education + Gender + Hukou + PartyMember + GovProcure + AutPersonality,
                     data = df, family = binomial("logit"))

avg.pred.social.desirability.confidence <- predict(fit.list.confidence,
                                             direct.glm = fit.sens.confidence, se.fit = TRUE)



####system
fit.list.system <- ictreg(y_system ~ YearBirth + Education + Gender + Hukou + PartyMember + GovProcure + AutPersonality,
                              data = df, treat = "treat_system", J = 4, method = "lm")

fit.sens.system <- glm(SystemDirect ~ YearBirth + Education + Gender + Hukou + PartyMember + GovProcure + AutPersonality,
                           data = df, family = binomial("logit"))

avg.pred.social.desirability.system <- predict(fit.list.system,
                                                   direct.glm = fit.sens.system, se.fit = TRUE)


####corruption
fit.list.corruption <- ictreg(y_corruption ~ YearBirth + Education + Gender + Hukou + PartyMember + GovProcure + AutPersonality,
                              data = df, treat = "treat_corruption", J = 4, method = "lm")

fit.sens.corruption <- glm(CorruptionDirect ~ YearBirth + Education + Gender + Hukou + PartyMember + GovProcure + AutPersonality,
                           data = df, family = binomial("logit"))

avg.pred.social.desirability.corruption <- predict(fit.list.corruption,
                                                   direct.glm = fit.sens.corruption, se.fit = TRUE)


####censor
fit.list.censor <- ictreg(y_censor ~ YearBirth + Education + Gender + Hukou + PartyMember + GovProcure + AutPersonality,
                              data = df, treat = "treat_censor", J = 4, method = "lm")

fit.sens.censor <- glm(CensorDirect ~ YearBirth + Education + Gender + Hukou + PartyMember + GovProcure + AutPersonality,
                           data = df, family = binomial("logit"))

avg.pred.social.desirability.censor <- predict(fit.list.censor,
                                                   direct.glm = fit.sens.censor, se.fit = TRUE)
