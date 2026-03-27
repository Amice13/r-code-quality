

##Instructions: There are three data files. Please upload all three.


###Note In the data variable Q59_1 == organizational performance evaluations
###Note In the data variable Q42_1 == preferences to fire/reappoint



install.packages("retrodesign")
install.packages("medmod")
install.packages("jmv")
install.packages("rjags")
install.packages("gamlj")
install.packages("BEST")
install.packages("TOSTER")
install.packages("mediation")
install.packages("reghelper")
attach(Eval_Attrib_Combined_Data_Low_Performance_FINAL)

data<- Eval_Attrib_Combined_Data_Low_Performance_FINAL

condition <- factor(condition, labels = c("1", "2", "3", "4"))


###############Test of hypothesis 1 for negative performance information study ############################


jmv::ANOVA(
  formula = Q59_1 ~ hi_info + black + hi_info:black,
  data = data,
  norm = TRUE,
  emMeans = ~ black,
  emmPlots = FALSE,
  emmTables = TRUE)


###############Test of hypothesis 2a ############################


jmv::ANOVA(
  formula = Q59_1 ~ condition,
  data = data,
  norm = TRUE,
  contrasts = list(
    list(
      var="condition",
      type="repeated")),
  postHocCorr = NULL,
  emMeans = ~ condition,
  emmTables = TRUE)


####kruskal wallis test for significant effect of increased negative performance information on performance evaluations of the Black public manager


data.kruskal <- data[ which(data$black=='1'), ]

jmv::anovaNP(
  formula = Q59_1 ~ condition,
  data = data.kruskal)


##########post hoc power analysis for significant effect of increased negative performance information on performance evaluations of the Black public manager

retro_design(4.61, 1.93, alpha = 0.05)

###############Test of hypothesis 3a #############################


medmod::mod(
  data = data,
  dep = 'Q42_1',
  mod = 'black',
  pred = 'Q59_1',
  ci = TRUE,
  simpleSlopeEst = TRUE)


###############Indirect effect for negative performance information study ############################


set.seed(73)
med.fit.negative <- lm(Q59_1 ~ hi_info*black, data = data)
out.fit.negative <- lm(Q42_1 ~ Q59_1 + hi_info*black, data = data)

med.negative.black <- mediate(med.fit.negative, out.fit.negative, treat = "hi_info", mediator = "Q59_1", covariates = list(black=1), robustSE = TRUE, sims = 1000)

med.negative.white <- mediate(med.fit.negative, out.fit.negative, treat = "hi_info", mediator = "Q59_1", covariates = list(black=0), robustSE = TRUE, sims = 1000)

summary(med.negative.black)

summary(med.negative.white)


med.init <- mediate(med.fit.negative, out.fit.negative, treat = "hi_info", mediator = "Q59_1",
                    sims = 2)
test.modmed(med.init, covariates.1 = list(black = 0),
            covariates.2 = list(black = 1), sims = 100)

sens.negative.black <- medsens(med.negative.black, rho.by = 0.1, effect.type = "indirect",
                    sims = 100)

summary(sens.negative.black)

sens.negative.white <- medsens(med.negative.white, rho.by = 0.1, effect.type = "indirect",
                               sims = 100)

summary(sens.negative.white)


##################################Positive Performance Information Study#################################################


attach(Eval_and_Attrib_High_Performance_Final)

data1<- Eval_and_Attrib_High_Performance_Final


###############Test of hypothesis 1 for positive performance information study ############################


jmv::ANOVA(
  formula = Q59_1 ~ Black + Hi_info + Black:Hi_info,
  data = data1,
  norm = TRUE,
  emMeans = ~ Black,
  emmPlots = FALSE,
  emmTables = TRUE)


###############Test of hypothesis 2b ############################


jmv::ANOVA(
  formula = Q59_1 ~ Condition,
  data = data1,
  norm = TRUE,
  contrasts = list(
    list(
      var="Condition",
      type="repeated")),
  postHocCorr = NULL,
  emMeans = ~ Condition,
  emmTables = TRUE)


###############Test of hypothesis 3b ############################


medmod::mod(
  data = data1,
  dep = 'Q42_1',
  mod = 'Black',
  pred = 'Q59_1',
  ci = TRUE,
  simpleSlopeEst = TRUE)


###############Indirect effect for positive performance information study ############################


set.seed(73)
med.fit.positive <- lm(Q59_1 ~ Hi_info*Black, data = data1)
out.fit.positive <- lm(Q42_1 ~ Q59_1 + Hi_info*Black, data = data1)

med.positive.black <- mediate(med.fit.positive, out.fit.positive, treat = "Hi_info", mediator = "Q59_1", covariates = list(Black=1), robustSE = TRUE, sims = 1000)

med.positive.white <- mediate(med.fit.positive, out.fit.positive, treat = "Hi_info", mediator = "Q59_1", covariates = list(Black=0), robustSE = TRUE, sims = 1000)

summary(med.positive.black)

summary(med.positive.white)


med.init.positive <- mediate(med.fit.positive, out.fit.positive, treat = "Hi_info", mediator = "Q59_1",
                    sims = 2)
test.modmed(med.init.positive, covariates.1 = list(Black = 0),
            covariates.2 = list(Black = 1), sims = 100)

sens.positive.black <- medsens(med.positive.black, rho.by = 0.1, effect.type = "indirect",
                               sims = 100)

summary(sens.positive.black)

sens.positive.white <- medsens(med.positive.white, rho.by = 0.1, effect.type = "indirect",
                               sims = 100)

summary(sens.positive.white)
