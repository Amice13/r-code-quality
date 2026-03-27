

###Note In the data variable Q59_1 == organizational performance evaluations
###Note In the data variable Q42_1 == preferences to fire/reappoint


attach(Eval_Attrib_Combined_Data_Low_Performance_FINAL)

data<- Eval_Attrib_Combined_Data_Low_Performance_FINAL


########Analysis for Section 3 ########


data_pass<-data[ which(data$pass_check=='1'), ]


###############Test of hypothesis 1 for negative performance information study ############################


jmv::ANOVA(
  formula = Q59_1 ~ hi_info + black + hi_info:black,
  data = data_pass,
  norm = TRUE,
  emMeans = ~ black,
  emmPlots = FALSE,
  emmTables = TRUE)


###############Test of hypothesis 2a ############################


jmv::ANOVA(
  formula = Q59_1 ~ condition,
  data = data_pass,
  norm = TRUE,
  contrasts = list(
    list(
      var="condition",
      type="repeated")),
  postHocCorr = NULL,
  emMeans = ~ condition,
  emmTables = TRUE)


####kruskal wallis test for significant effect of increased negative performance information on performance evaluations of the Black public manager


data.kruskal_pass <- data_pass[ which(data_pass$black=='1'), ]

jmv::anovaNP(
  formula = Q59_1 ~ condition,
  data = data.kruskal_pass)


##########post hoc power analysis for significant effect of increased negative performance information on performance evaluations of the Black public manager

retro_design(5.41, 2.60, alpha = 0.05)

###############Test of hypothesis 3a #############################


medmod::mod(
  data = data_pass,
  dep = 'Q42_1',
  mod = 'black',
  pred = 'Q59_1',
  ci = TRUE,
  simpleSlopeEst = TRUE)


###############Indirect effect for negative performance information study ############################


set.seed(70)
med.fit.negative <- lm(Q59_1 ~ hi_info*black, data = data_pass)
out.fit.negative <- lm(Q42_1 ~ Q59_1 + hi_info*black, data = data_pass)

med.negative.black <- mediate(med.fit.negative, out.fit.negative, treat = "hi_info", mediator = "Q59_1", covariates = list(black=1), robustSE = TRUE, sims = 1000)

med.negative.white <- mediate(med.fit.negative, out.fit.negative, treat = "hi_info", mediator = "Q59_1", covariates = list(black=0), robustSE = TRUE, sims = 1000)

summary(med.negative.black)

summary(med.negative.white)


##################################Positive Performance Information Study#################################################


attach(Eval_and_Attrib_High_Performance_Final)

data1<- Eval_and_Attrib_High_Performance_Final

data1_pass<- data1[ which(data1$pass_check!='1'), ]

###############Test of hypothesis 1 for positive performance information study ############################


jmv::ANOVA(
  formula = Q59_1 ~ Black + Hi_info + Black:Hi_info,
  data = data1_pass,
  norm = TRUE,
  emMeans = ~ Black,
  emmPlots = FALSE,
  emmTables = TRUE)


###############Test of hypothesis 2b ############################


jmv::ANOVA(
  formula = Q59_1 ~ Condition,
  data = data1_pass,
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
  data = data1_pass,
  dep = 'Q42_1',
  mod = 'Black',
  pred = 'Q59_1',
  ci = TRUE,
  simpleSlopeEst = TRUE)


###############Indirect effect for positive performance information study ############################


set.seed(73)
med.fit.positive <- lm(Q59_1 ~ Hi_info*Black, data = data1_pass)
out.fit.positive <- lm(Q42_1 ~ Q59_1 + Hi_info*Black, data = data1_pass)

med.positive.black <- mediate(med.fit.positive, out.fit.positive, treat = "Hi_info", mediator = "Q59_1", covariates = list(Black=1), robustSE = TRUE, sims = 1000)

med.positive.white <- mediate(med.fit.positive, out.fit.positive, treat = "Hi_info", mediator = "Q59_1", covariates = list(Black=0), robustSE = TRUE, sims = 1000)

summary(med.positive.black)

summary(med.positive.white)




########
########
########
########Analysis for Section 4 ########


attach(Eval_Attrib_Combined_Data_Low_Performance_FINAL)

data<- Eval_Attrib_Combined_Data_Low_Performance_FINAL

condition <- factor(condition, labels = c("1", "2", "3", "4"))


###############Test of hypothesis 1 for negative performance information study ############################


jmv::ANOVA(
  formula = Q59_1 ~ hi_info + black + hi_info:black + Q3 + Q4 + Q5 + Q6 +Q7,
  data = data,
  norm = TRUE,
  emMeans = ~ black,
  emmPlots = FALSE,
  emmTables = TRUE)


###############Test of hypothesis 2a ############################


jmv::ANOVA(
  formula = Q59_1 ~ condition + Q3 + Q4 + Q5 + Q6 +Q7,
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
  formula = Q59_1 ~ condition + Q3 + Q4 + Q5 + Q6 +Q7,
  data = data.kruskal)


##########post hoc power analysis for significant effect of increased negative performance information on performance evaluations of the Black public manager

retro_design(4.51, 1.93, alpha = 0.05)

###############Test of hypothesis 3a #############################


model <- lm(Q42_1 ~ Q59_1 * black+ Q3 + Q4 + Q5 + Q6 +Q7, data=data)
summary(model)
simple_slopes(model)
simple_slopes(model,
              levels=list(black=c(0,1, 'sstest')))


###############Indirect effect for negative performance information study ############################


set.seed(73)
med.fit.negative <- lm(Q59_1 ~ hi_info*black+ Q3 + Q4 + Q5 + Q6 +Q7, data = data)
out.fit.negative <- lm(Q42_1 ~ Q59_1 + hi_info*black+ Q3 + Q4 + Q5 + Q6 +Q7, data = data)

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


attach(Eval_and_Attrib_High_Performance_Final1)

data1<- Eval_and_Attrib_High_Performance_Final1


###############Test of hypothesis 1 for positive performance information study ############################


jmv::ANOVA(
  formula = Q59_1 ~ Black + Hi_info + Black:Hi_info + Q3 + Q4 + Q5 + Q6 +Q7,
  data = data1,
  norm = TRUE,
  emMeans = ~ Black,
  emmPlots = FALSE,
  emmTables = TRUE)


###############Test of hypothesis 2b ############################

jmv::ANOVA(
  formula = Q59_1 ~ Condition + Q3 + Q4 + Q5 + Q6 +Q7,
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


model1<- lm(Q42_1 ~ Q59_1 * Black+ Q3 + Q4 + Q5 + Q6 +Q7, data1)
summary(model1)
simple_slopes(model1)
simple_slopes(model1,
              levels=list(Black=c(0,1, 'sstest')))


###############Indirect effect for positive performance information study ############################


set.seed(73)
med.fit.negative <- lm(Q59_1 ~ Hi_info*Black+ Q3 + Q4 + Q5 + Q6 +Q7, data = data1)
out.fit.negative <- lm(Q42_1 ~ Q59_1 + Hi_info*Black+ Q3 + Q4 + Q5 + Q6 +Q7, data = data1)

med.negative.black <- mediate(med.fit.negative, out.fit.negative, treat = "Hi_info", mediator = "Q59_1", covariates = list(Black=1), robustSE = TRUE, sims = 1000)

med.negative.white <- mediate(med.fit.negative, out.fit.negative, treat = "Hi_info", mediator = "Q59_1", covariates = list(Black=0), robustSE = TRUE, sims = 1000)

summary(med.negative.black)

summary(med.negative.white)



attach(Eval_Attrib_Combined_Data_Low_Performance_FINAL)

data<- Eval_Attrib_Combined_Data_Low_Performance_FINAL

########
########
########
########Analysis for Section 5 ########


attach(Eval_Attrib_Combined_Data_Low_Performance_FINAL)

data<- Eval_Attrib_Combined_Data_Low_Performance_FINAL

####H1

#Figure 1a

yb <- filter(data, black == 1) %>% select(Q59_1)
yw <- filter(data, black== 0) %>% select(Q59_1) 

BESTout_h1a <- BESTmcmc(yb$Q59_1,yw$Q59_1, parallel=F)

par(mfrow=c(2, 2))
plot(BESTout_h1a, ROPE = c(-.5, .5))

plot(BESTout_h1a, ROPE = c(-0.1, 0.1), which = "effect")

#Figure 1b


attach(Eval_and_Attrib_High_Performance_Final1)

data1<- Eval_and_Attrib_High_Performance_Final1

yb <- filter(data1, Black == 1) %>% select(Q59_1)
yw <- filter(data1, Black== 0) %>% select(Q59_1) 

BESTout_h1b <- BESTmcmc(yb$Q59_1,yw$Q59_1, parallel=F)

par(mfrow=c(2, 2))
plot(BESTout_h1b, ROPE = c(-.5, .5))

plot(BESTout_h1b, ROPE = c(-0.1, 0.1), which = "effect")

####H2

#Figure 2a

y1 <- filter(data, condition == 1) %>% select(Q59_1)
y2 <- filter(data, condition== 2) %>% select(Q59_1) 

BESTout_2a <- BESTmcmc(y1$Q59_1,y2$Q59_1, parallel=F)

par(mfrow=c(2, 2))
plot(BESTout_2a, ROPE = c(-0.5, 0.5))
plot(BESTout_2a, ROPE = c(-0.1, 0.1), which = "effect")


#Figure 2b


y1 <- filter(data, condition == 3) %>% select(Q59_1)
y2 <- filter(data, condition== 4) %>% select(Q59_1) 

BEST_2b <- BESTmcmc(y1$Q59_1,y2$Q59_1, parallel=F)

par(mfrow=c(2, 2))
plot(BEST_2b, ROPE = c(-0.5, 0.5))
plot(BEST_2b, ROPE = c(-0.1, 0.1), which = "effect")


#Figure 3a


y1 <- filter(data1, Condition == 1) %>% select(Q59_1)
y2 <- filter(data1, Condition== 2) %>% select(Q59_1) 

BESTout_3a <- BESTmcmc(y1$Q59_1,y2$Q59_1, parallel=F)

par(mfrow=c(2, 2))
plot(BESTout_3a, ROPE = c(-0.5, 0.5))
plot(BESTout_3a, ROPE = c(-0.1, 0.1), which = "effect")


#Figure 3b


y1 <- filter(data1, Condition == 3) %>% select(Q59_1)
y2 <- filter(data1, Condition== 4) %>% select(Q59_1) 

BEST_3b <- BESTmcmc(y1$Q59_1,y2$Q59_1, parallel=F)

par(mfrow=c(2, 2))
plot(BEST_3b, ROPE = c(-0.5, 0.5))
plot(BEST_3b, ROPE = c(-0.1, 0.1), which = "effect")


########
########
########
######## Analysis for Section 6 ########


#### H1

#Figure 4a


par(mfrow=c(1,1))
m0<-25.195
m1<-25.082
sd0<-17.879
sd1<-17.943
n0<-344
n1<-352

TOSTtwo(m0,m1,sd0,sd1,n0,n1,
        low_eqbound = -.2,
        high_eqbound = .2,
        alpha = 0.05,
        var.equal = F, 
        plot = T)


#Figure 4b


par(mfrow=c(1,1))
m0<-86.938
m1<-87.728
sd0<-12.894
sd1<-12.330
n0<-354
n1<-342

TOSTtwo(m0,m1,sd0,sd1,n0,n1,
        low_eqbound = -.2,
        high_eqbound = .2,
        alpha = 0.05,
        var.equal = F, 
        plot = T)


#Figure 5a


m0<-27.373
m1<-22.766
sd0<-19.019
sd1<-16.518
n0<-177
n1<-175

TOSTtwo(m0,m1,sd0,sd1,n0,n1,
        low_eqbound = -.2,
        high_eqbound = .2,
        alpha = 0.05,
        var.equal = F, 
        plot = T)


#Figure 5b


m0<-25.898
m1<-24.449
sd0<-18.630
sd1<-17.071
n0<-177
n1<-167

TOSTtwo(m0,m1,sd0,sd1,n0,n1,
        low_eqbound = -.2,
        high_eqbound = .2,
        alpha = 0.05,
        var.equal = F, 
        plot = T)


#Figure 6a


m0<-88.211
m1<-87.246
sd0<-10.702
sd1<-13.783
n0<-171
n1<-171

TOSTtwo(m0,m1,sd0,sd1,n0,n1,
        low_eqbound = -.2,
        high_eqbound = .2,
        alpha = 0.05,
        var.equal = F, 
        plot = T)


#Figure 6b


m0<-86.869
m1<-87.006
sd0<-11.321
sd1<-14.298
n0<-175
n1<-179

TOSTtwo(m0,m1,sd0,sd1,n0,n1,
        low_eqbound = -.2,
        high_eqbound = .2,
        alpha = 0.05,
        var.equal = F, 
        plot = T)


########
########
########
######## Analysis for Section 7 ########

data.robust <- data[ which(data$black=='1'), ]

data.robust1 <- data[ which(data$black=='0'), ]

### Responsibility Attribution

#Black Public Manager


set.seed(73)
med.fit.negative.RA <- lm(Attribution ~ hi_info, data = data.robust)
out.fit.negative.RA <- lm(Q59_1 ~ Attribution + hi_info, data = data.robust)

med.RA <- mediate(med.fit.negative.RA, out.fit.negative.RA, treat = "hi_info", mediator = "Attribution", robustSE = TRUE, sims = 1000)

summary(med.RA)

sens.RA <- medsens(med.RA, rho.by = 0.1, effect.type = "direct",
                               sims = 100)

summary(sens.RA)


#White Public Manager


set.seed(73)
med.fit.negative.RA <- lm(Attribution ~ hi_info, data = data.robust1)
out.fit.negative.RA <- lm(Q59_1 ~ Attribution + hi_info, data = data.robust1)

med.RA <- mediate(med.fit.negative.RA, out.fit.negative.RA, treat = "hi_info", mediator = "Attribution", robustSE = TRUE, sims = 1000)

summary(med.RA)

sens.RA <- medsens(med.RA, rho.by = 0.1, effect.type = "direct",
                   sims = 100)

summary(sens.RA)


### Romance of Leadership

#Black Public Manager

set.seed(73)
med.fit.negative.ROL <- lm(ROL_Sum ~ hi_info, data = data.robust)
out.fit.negative.ROL <- lm(Q59_1 ~ ROL_Sum + hi_info, data = data.robust)


med.ROL <- mediate(med.fit.negative.ROL, out.fit.negative.ROL, treat = "hi_info", mediator = "ROL_Sum", robustSE = TRUE, sims = 1000)

summary(med.ROL)

sens.ROL <- medsens(med.ROL, rho.by = 0.1, effect.type = "direct",
                   sims = 100)

summary(sens.ROL)


#White Public Manager


set.seed(73)
med.fit.negative.ROL <- lm(ROL_Sum ~ hi_info, data = data.robust1)
out.fit.negative.ROL <- lm(Q59_1 ~ ROL_Sum + hi_info, data = data.robust1)


med.ROL <- mediate(med.fit.negative.ROL, out.fit.negative.ROL, treat = "hi_info", mediator = "ROL_Sum", robustSE = TRUE, sims = 1000)

summary(med.ROL)

sens.ROL <- medsens(med.ROL, rho.by = 0.1, effect.type = "direct",
                    sims = 100)

summary(sens.ROL)

### Individual Performance Evaluations

#Black Public Manager


set.seed(74)
med.fit.negative.in <- lm(Per2_1 ~ hi_info, data = data.robust)
out.fit.negative.in <- lm(Q59_1 ~ Per2_1 + hi_info, data = data.robust)

med.in <- mediate(med.fit.negative.in, out.fit.negative.in, treat = "hi_info", mediator = "Per2_1", robustSE = TRUE, sims = 1000)

summary(med.in)

sens.in <- medsens(med.in, rho.by = 0.1, effect.type = "direct",
                    sims = 100)

summary(sens.in)


#White Public Manager


set.seed(74)
med.fit.negative.in <- lm(Per2_1 ~ hi_info, data = data.robust1)
out.fit.negative.in <- lm(Q59_1 ~ Per2_1 + hi_info, data = data.robust1)

med.in <- mediate(med.fit.negative.in, out.fit.negative.in, treat = "hi_info", mediator = "Per2_1", robustSE = TRUE, sims = 1000)

summary(med.in)

sens.in <- medsens(med.in, rho.by = 0.1, effect.type = "direct",
                   sims = 100)

summary(sens.in)



### Service Performance Evaluations

#Black Public Manager

set.seed(73)
med.fit.negative.S <- lm(Perf1_1 ~ hi_info, data = data.robust)
out.fit.negative.S <- lm(Q59_1 ~ Perf1_1 + hi_info, data = data.robust)

med.S <- mediate(med.fit.negative.S, out.fit.negative.S, treat = "hi_info", mediator = "Perf1_1", robustSE = TRUE, sims = 1000)

summary(med.S)

sens.S <- medsens(med.ROL, rho.by = 0.1, effect.type = "direct",
                    sims = 100)

summary(sens.S)


#White Public Manager


set.seed(3)
med.fit.negative.S <- lm(Perf1_1 ~ hi_info, data = data.robust1)
out.fit.negative.S <- lm(Q59_1 ~ Perf1_1 + hi_info, data = data.robust1)

med.S <- mediate(med.fit.negative.S, out.fit.negative.S, treat = "hi_info", mediator = "Perf1_1", robustSE = TRUE, sims = 1000)

summary(med.S)

sens.S <- medsens(med.ROL, rho.by = 0.1, effect.type = "direct",
                  sims = 100)

summary(sens.S)


### Perceived Competence


#Black Public Manager

set.seed(73)
med.fit.negative.C <- lm(Competence ~ hi_info, data = data.robust)
out.fit.negative.C <- lm(Q59_1 ~ Competence + hi_info, data = data.robust)

med.C <- mediate(med.fit.negative.C, out.fit.negative.C, treat = "hi_info", mediator = "Competence", robustSE = TRUE, sims = 1000)

summary(med.C)

sens.C <- medsens(med.C, rho.by = 0.1, effect.type = "direct",
                    sims = 100)

summary(sens.C)


#White Public Manager



set.seed(73)
med.fit.negative.C <- lm(Competence ~ hi_info, data = data.robust1)
out.fit.negative.C <- lm(Q59_1 ~ Competence + hi_info, data = data.robust1)

med.C <- mediate(med.fit.negative.C, out.fit.negative.C, treat = "hi_info", mediator = "Competence", robustSE = TRUE, sims = 1000)

summary(med.C)

sens.C <- medsens(med.C, rho.by = 0.1, effect.type = "direct",
                  sims = 100)

summary(sens.C)

