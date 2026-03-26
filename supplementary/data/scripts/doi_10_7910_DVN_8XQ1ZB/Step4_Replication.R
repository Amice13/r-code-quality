## ---------------------------
##
## Script name: Reproduction and improvement code for Do Women Officers Police Differently? Evidence from Traffic Stops
##
## Purpose of script: Reproduction
##
## Date of last modification: 2023-12-11
##
## Authors: Dianyi Yang
## ---------------------------
##
## Notes: Our work is based on Shoub, Stauffer and Song's (2021) paper. We use the data/regression files from the authors,
## which are stored in the "Data" folder or can be produced by running Steps 1-3, which areprovided by the original authors.
## ---------------------------

###
### 1. Setting up the space. 
###

# Automatically install other packages:
need <- c("ggplot2", "rstudioapi","dplyr", "texreg","readr","modelsummary", "mgcv", "car", "fixest", "kableExtra", 'alpaca', 'margins',
          "pscl","arm","stats","estimatr",'plm',"arsenal",'lme4','modelr','foreign', "tidyverse", "compiler",'fwildclusterboot') # list packages needed
have <- need %in% rownames(installed.packages()) # checks packages you have
if(any(!have)) install.packages(need[!have]) # install missing packages
invisible(lapply(need, library, character.only=T))

# Setting the working directory:
setwd(gsub("/Code","",dirname(rstudioapi::getSourceEditorContext()$path))) #Automatically sets the working directory

rm(list = ls())

############################Section 2##########################
# Table 1:  Summary of Stops and Searches by Agency
  # Loading in the Data
load("Data/NorthCarolina.RData")
load("Data/FloridaLarge.RData")
load("Data/FloridaSmall.RData")
cmpd.employee = read_csv("Data/CMPD_Employee_Demographics.csv")
  #summarise the data for Table 1
tab1 = data.frame("Department"=c("Charlotte PD (NC)",
                                 "Male Officers","Female Officers",
                                 "Florida Highway Patrol",       #typo: "Highwar" to "Highway"
                                 "Male Officers","Female Officers"),
                  "Type"=c("Municipal","","","Statewide","",""),
                  "Years"=c("2016-17, 2019-20","","",           #edited by replication authors
                            "2010-15","",""),
                  "Stops"=c(dim(nc)[1],table(nc$of_gender),
                            dim(fl[!is.na(fl$search_occur),])[1],
                            table(fl$of_gender[!is.na(fl$search_occur)])),
                  "Searches"=c(table(nc$search)[2],table(nc$of_gender,nc$search)[,2],
                               table(fl$search_occur)[2],
                               table(fl$of_gender,fl$search_occur)[,2]),
                  "Search Rate"=c(table(nc$search)[2]/dim(nc)[1],
                                  table(nc$of_gender,nc$search)[,2]/table(nc$of_gender),
                                  table(fl$search_occur)[2]/dim(fl[!is.na(fl$search_occur),])[1],
                                  table(fl$of_gender,fl$search_occur)[,2]/
                                    table(fl$of_gender[!is.na(fl$search_occur)]))) #prepare dataset for table 1: summary of stops and searches by Agency
tab1 = rbind(tab1,
             c("Total","","",
               sum(tab1[c(1,4),4]),sum(tab1[c(1,4),5]),
               sum(tab1[c(1,4),5])/sum(tab1[c(1,4),4]))) #produce table 1: summary of stops and searches by Agency
tab1$Search.Rate <- round(as.numeric(tab1$Search.Rate), digits = 3)  #round search rates to 3 decimal places (added by replication authors)
tab1 #show table 1

tab1$Stops <- as.numeric(tab1$Stops)
tab1$Searches <- as.numeric(tab1$Searches)

tab1[1,1] <- cell_spec(tab1[1,1], bold=T, format='latex')
tab1[4,1] <- cell_spec(tab1[4,1], bold=T, format='latex')

kbl(tab1, booktabs=T, escape=F, align = 'lccrrc', col.names = gsub('[.]',' ',colnames(tab1)),
    format='latex', format.args = list(big.mark = ',')) %>%
  row_spec(0, align = "c", bold=T) %>%
  row_spec(7, bold=T) #Table 1b, need to manually left-align 'Department' and adjust linespace
 #latex table
rm(tab1)

############################Section 3#######################
#Table 2:Regressions Explaining Searches Following a Traffic Stop
#Run alternative regressions with robust and clustered standard errors
nc.search_robust = feols(search~factor(race_gender)+subject_age+
                 investigatory+
                 factor(of_race)+
                 factor(of_gender)+Officer_Years_of_Service+
                 factor(month)+factor(year)+
                 factor(CMPD_Division), vcov='HC1',
               data=nc) #TWFE regression of search rate on gender and controls.
save(nc.search_robust,file="Data/NCSearch_robust.RData")

nc.search_cluster = feols(search~factor(race_gender)+subject_age+
                           investigatory+
                           factor(of_race)+
                           factor(of_gender)+Officer_Years_of_Service+
                           factor(month)+factor(year)+factor(CMPD_Division), cluster = ~CMPD_Division,
                         data=nc) #TWFE regression of search rate on gender and controls.
save(nc.search_cluster,file="Data/NCSearch_cluster.RData")


load("Data/NCSearch_OLS.RData") #original OLS with IID se

fl.search_robust = feols(search_occur~factor(race_gender)+
                 subject_age+out_of_state+
                 investigatory+
                 factor(of_gender)+factor(of_race)+
                 officer_years_of_service+officer_age+
                 factor(hour_of_day)+factor(month)+factor(year)+factor(county_name),
               data=fl.sm,  vcov='HC1',
               subset=fl.sm$county_include==1&fl.sm$officer_exclude==0) #TWFE regression of search rate on gender and controls.
save(fl.search_robust,file="Data/FLSearch_robust.RData")

fl.search_cluster = feols(search_occur~factor(race_gender)+
                           subject_age+out_of_state+
                           investigatory+
                           factor(of_gender)+factor(of_race)+
                           officer_years_of_service+officer_age+
                           factor(hour_of_day)+factor(month)+factor(year)+factor(county_name),cluster=~county_name,
                         data=fl.sm,
                         subset=fl.sm$county_include==1&fl.sm$officer_exclude==0) #TWFE regression of search rate on gender and controls.
save(fl.search_cluster,file="Data/FLSearch_cluster.RData")
load("Data/FLSearch_OLS.RData")  #Original OLS with IID se

#load all models (if regressions are not run/loaded)
load('Data/NCSearch_robust.RData')
load("Data/NCSearch_cluster.RData")
load("Data/NCSearch_OLS.RData")
load("Data/FLSearch_robust.RData")
load("Data/FLSearch_cluster.RData")
load("Data/FLSearch_OLS.RData")

#Test for heterogeneity (non-constant variance)
test.nc <- ncvTest(nc.search)
test.fl <- ncvTest(fl.search)
ncv.result <- data.frame('Department' = c("Charlotte PD (NC)", "Florida Highway Patrol (FHP)"),
                         'Chi-squared Statistic' = c(test.nc$ChiSquare,test.fl$ChiSquare),
                         'Degrees of Freedom'         =  c(test.nc$Df,test.fl$Df),
                         'p.value'          =  c(test.nc$p,test.fl$p))
ncv.result$p.value <- ifelse(ncv.result$p.value<0.001, '<0.001', ncv.result$p.value)
kbl(ncv.result, booktabs=T, escape=F, align = 'lccc',col.names = gsub('[.]',' ',colnames(ncv.result)),
    format='latex', format.args = list(big.mark = ',')) %>%
  row_spec(0, bold=T) #Table 2a

#prep for result table
models <- list('IID' = nc.search,
               'Robust' = nc.search_robust,
               'Clustered' = nc.search_cluster,
               'IID' = fl.search,
               'Robust' = fl.search_robust,
               'Clustered' = fl.search_cluster)

coef_map = list("factor(of_gender)1"="Female Officer",
                "(Intercept)"="(Intercept)")
f <- function(x) format(round(x, 3), big.mark=",")
gof_map = list(list("raw" = "r.squared", "clean" = "$R^2$", "fmt" = 3),
               list("raw" = "adj.r.squared", "clean" = "Adjusted $R^2$", "fmt" = 3),
               list("raw" = "nobs", "clean" = "$N$", "fmt" = f))

modelsummary(models, stars=T, coef_map=coef_map, gof_map=gof_map, output='latex') %>%
  add_header_above(c(" " = 1, "CPD" = 3, "FHP" = 3), bold=TRUE) #Table 2b

#wild cluster bootstrap
seed=12345 #random seed for wild cluster bootstrap
set.seed(12345)
dqrng::dqset.seed(seed)
load("Data/NorthCarolina.RData") #in case the nc dataset is not loaded
nc.search_wild <-  boottest(nc.search, param="factor(of_gender)1", B=9999, clustid="CMPD_Division", type="webb")
save(nc.search_wild,file="Data/NCSearch_wild.RData")

modelsummary(list('CPD - WCB' = nc.search_wild), estimate = "{estimate} ({p.value}){stars}", statistic = "[{conf.low}, {conf.high}]",
             notes=c("+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001",
                     "P-values are in parentheses; 95% confidence intervals are in square brackets.",
                     "CIs are calculated through Wild Cluster Bootstrapping by country with Webb weights."),
             stars = c('+' = .1, '*' = .05, '**' = .01, '***' = .001), gof_map = gof_map,
             coef_rename = c("1*factor(of_gender)1 = 0"="Female Officer"),
             output="latex") %>%
  row_spec(0, bold=T) #Table 2c

#Logistic regressions for search rate
rm(list=ls())
load("Data/FLSearch_Logit.RData") #logistic regression from original authors
load("Data/FloridaSmall.RData")
fl.search.feglm = alpaca::feglm(search_occur~factor(race_gender)+
                                  subject_age+out_of_state+
                                  investigatory+
                                  factor(of_gender)+factor(of_race)+
                                  officer_years_of_service+officer_age+
                                  factor(hour_of_day)+factor(month)|county_name+year,
                                data=subset(fl.sm, county_include==1&officer_exclude==0),family=binomial())
fl.search.nobias <- biasCorr(fl.search.feglm)    #bias correction for fixed-effects logit regressions
save(fl.search.nobias,file="Data/FLSearch_Logit_nobias.RData") #save the logit regression

load("Data/NCSearch_Logit.RData") #logistic regression from original authors
load("Data/NorthCarolina.RData") #load cleaned NC data
nc.search.nobias = biasCorr(alpaca::feglm(search~factor(race_gender)+subject_age+
                  investigatory+
                  factor(of_race)+
                  factor(of_gender)+Officer_Years_of_Service+
                  factor(month)|CMPD_Division+year,
                family=binomial(),
                data=nc)) #do the same bias-corrected search rate regression with Logit for nc data.
save(nc.search.nobias,file="Data/NCSearch_Logit_nobias.RData") #save the logit regression

glance_custom.feglm <- function(x, ...) {
  data.frame(
    "dev" = x[['deviance']])}

glance_custom.glm <- function(x, ...) {
  data.frame(
    "dev" = x[['deviance']])}

nc.search.ape <- getAPEs(nc.search.nobias)
nc.search.ape$gof <- get_gof(nc.search.nobias)
fl.search.ape <- getAPEs(fl.search.nobias)
fl.search.ape$gof <- get_gof(fl.search.nobias)

models <- list('Logistic' = nc.search,
               'Bias Corrected' = nc.search.nobias,
               'APE' = nc.search.ape,
               'Logistic' = fl.search,
               'Bias Corrected' = fl.search.nobias,
               'APE' = fl.search.ape)

coef_map = list("factor(of_gender)1"="Female Officer")



glance.APEs <- function(x, ...) {
  x$gof
}

tidy.APEs <- function(x, ...) {
  s <- summary(x, ...)
  ret <- data.frame(
    term      = row.names(s$cm),
    estimate  = s$cm[, 1],
    std.error = s$cm[, 2],
    p.value   = s$cm[, 4])
  ret
}

f <- function(x) format(round(x, 3), big.mark=",")
gof_map = list(list("raw" = "dev", "clean" = "Deviance", "fmt" = 3),
               list("raw" = "nobs", "clean" = "$N$", "fmt" = f))

modelsummary(models, stars=T, coef_map=coef_map, gof_map=gof_map, output='latex') %>%
  add_header_above(c(" " = 1, "CPD" = 3, "FHP" = 3), bold=T) #Table 2d

#Table 4: Regressions Explaining Probability and Frequency of Finding Contraband
rm(list = ls())
load("Data/FlContra_Logit.RData") #Column 3 of Table C1
fl.contra.logit <- fl.contra
load("Data/FlContra_OLS.RData") #Column 1 of original Table 4
load("Data/FloridaSmall.RData")

#Table 4a (Column 1 in original Table 4): clustered and logit regressions
#Regressions Explaining Probability of Finding Contraband Given a Search
fl.contra.cluster = feols(contra~factor(race_gender)+
                 subject_age+out_of_state+
                 investigatory+
                 factor(of_gender)+factor(of_race)+
                 officer_years_of_service+officer_age+
                 factor(hour_of_day)+factor(month)+factor(year)+
                 factor(county_name), cluster = ~county_name,
               data=fl.sm,  
               subset=fl.sm$county_include==1&
                 fl.sm$search_occur==1&
                 fl.sm$officer_exclude==0) 

fl.contra.nobias <- biasCorr(alpaca::feglm(contra~factor(race_gender)+
                       subject_age+out_of_state+
                       investigatory+
                       factor(of_gender)+factor(of_race)+
                       officer_years_of_service+officer_age+
                       factor(hour_of_day)+factor(month)|county_name+year,
                     data=subset(fl.sm, county_include==1 & search_occur==1 &officer_exclude==0),
                     family=binomial()))
#prep for Table 4a
glance_custom.feglm <- function(x, ...) {
  data.frame(
    "dev" = x[['deviance']])}
glance_custom.glm <- function(x, ...) {
  data.frame(
    "dev" = x[['deviance']])}

fl.contra.APE <- getAPEs(fl.contra.nobias)
fl.contra.APE$gof <- get_gof(fl.contra.nobias)

glance.APEs <- function(x, ...) {
  x$gof
}

tidy.APEs <- function(x, ...) {
  s <- summary(x, ...)
  ret <- data.frame(
    term      = row.names(s$cm),
    estimate  = s$cm[, 1],
    std.error = s$cm[, 2],
    p.value   = s$cm[, 4])
  ret
}

coef_map = list("factor(of_gender)1"="Female Officer",
                "(Intercept)"="(Intercept)")

models <- list('IID'       = fl.contra,
               'Clustered' = fl.contra.cluster,
               'APE'       = fl.contra.APE,
               'Coefficient'= fl.contra.nobias,
               'Coefficient'= fl.contra.logit)

f <- function(x) format(round(x, 3), big.mark=",")
gof_map = list(list("raw" = "r.squared", "clean" = "$R^2$", "fmt" = 3),
               list("raw" = "adj.r.squared", "clean" = "Adjusted $R^2$", "fmt" = 3),
               list("raw" = "dev", "clean" = "Deviance", "fmt" = 3),
               list("raw" = "nobs", "clean" = "$N$", "fmt" = f))

modelsummary(models, stars=T, coef_map=coef_map, gof_map=gof_map, output='latex') %>%
  add_header_above(c(" " = 1, "OLS" = 2, "Logistic (Bias Corrected)" = 2, "Logistic (Original)")) #Table 4a

#Table 4b and 4c: Regressions Explaining Frequency of Finding Contraband per 10 Searches and per 100 stops
rm(list = ls())
load("Data/FlSearchRate_OLS.RData")
load("Data/FlStopRate_OLS.RData")
load("Data/FL_Aggregated.RData")

contra.search.rate.robust = feols(contra.search.rate ~ factor(of_gender) + factor(of_exper) + 
                              factor(of_age) +factor(of_race) +
                              factor(race_gender) + factor(driver_age)+ 
                              investigatory + out_of_state +
                              factor(year)+factor(tod),
                            data=fl.ag.officers, vcov='HC1',
                            subset=fl.ag.officers$search_occur>0) #regress hit rate per 10 searches on gender, as well as other controls.
save(contra.search.rate.robust, file='Data/FLSearchRate_robust.RData')

contra.stop.rate.robust = feols(contra.stop.rate ~ factor(of_gender) + factor(of_exper) + 
                            factor(of_age) + factor(of_race) +
                            factor(race_gender) + factor(driver_age)+ 
                            investigatory + out_of_state +
                            factor(year)+factor(tod), vcov='HC1',
                          data=fl.ag.officers) #regress Hit Rate per100 Stops on gender, as well as other controls.
save(contra.stop.rate.robust, file='Data/FLStopRate_robust.RData')
glance_custom.glm <- function(x, ...) {
  data.frame(
    "dev" = x[['deviance']])}

contra.search.rate.logit = glm(cbind(contra, search_occur-contra) ~ factor(of_gender) + factor(of_exper) + 
                                  factor(of_age) +factor(of_race) +
                                  factor(race_gender) + factor(driver_age)+ 
                                  investigatory + out_of_state +
                                  factor(year)+factor(tod), family=binomial(),
                                data=subset(fl.ag.officers, search_occur>0))

contra.search.rate.ape = margins(contra.search.rate.logit, variables='of_gender')
attributes(contra.search.rate.ape)$gof <- get_gof(contra.search.rate.logit)
contra.search.rate.ape$dydx_of_gender1 <- contra.search.rate.ape$dydx_of_gender1*10
contra.search.rate.ape$Var_dydx_of_gender1 <- contra.search.rate.ape$Var_dydx_of_gender1*100

contra.stop.rate.logit = glm(cbind(contra, stops-contra) ~ factor(of_gender) + factor(of_exper) + 
                                  factor(of_age) + factor(of_race) +
                                  factor(race_gender) + factor(driver_age)+ 
                                  investigatory + out_of_state +
                                  factor(year)+factor(tod), family=binomial(),
                                data=fl.ag.officers)
contra.stop.rate.ape = margins(contra.stop.rate.logit, variables='of_gender')
attributes(contra.stop.rate.ape)$gof <- get_gof(contra.stop.rate.logit)
contra.stop.rate.ape$dydx_of_gender1 <- contra.stop.rate.ape$dydx_of_gender1*100
contra.stop.rate.ape$Var_dydx_of_gender1 <- contra.stop.rate.ape$Var_dydx_of_gender1*10000

save(contra.search.rate.logit, contra.search.rate.ape, contra.stop.rate.logit, contra.stop.rate.ape, file = 'Data/FLSearchSearchStopRate_Logit.Rdata')

glance_custom.margins <- function(x, ...) {
  attributes(x)$gof
}

models1 <- list('IID' = contra.search.rate.reg,
               'Robust' = contra.search.rate.robust,
               'APE' = contra.search.rate.ape,
               'Coefficient' = contra.search.rate.logit) #models for Table 4b

models2 <- list('IID' = contra.stop.rate.reg,
               'Robust' = contra.stop.rate.robust,
               'APE' = contra.stop.rate.ape,
               'Coefficient' = contra.stop.rate.logit) #models for Table 4c

coef_map = list("factor(of_gender)1"="Female Officer",
                "of_gender1"        ="Female Officer",
                "(Intercept)"="(Intercept)")

f <- function(x) format(round(x, 3), big.mark=",")
gof_map = list(list("raw" = "r.squared", "clean" = "$R^2$", "fmt" = 3),
               list("raw" = "adj.r.squared", "clean" = "Adjusted $R^2$", "fmt" = 3),
               list("raw" = "dev", "clean" = "Deviance", "fmt" = 3),
               list("raw" = "nobs", "clean" = "$N$", "fmt" = f))

modelsummary(models1, stars=T, coef_map=coef_map, gof_map=gof_map, output='latex') %>%
  add_header_above(c(" " = 1, "OLS" = 2, "Logistic" = 2)) %>%
  add_header_above(c(" " = 1, "Hit Rate per 10 Searches" = 4), bold=T) #Table 4b

modelsummary(models2, stars=T, coef_map=coef_map, gof_map=gof_map, output='latex') %>%
  add_header_above(c(" " = 1, "OLS" = 2, "Logistic" = 2)) %>%
  add_header_above(c(" " = 1, "Hit Rate per 100 Stops"=4), bold=T) #Table 4c

#################Section 4###########################
#Figure 1a: Frequency by Police Division in the Charlotte Police Department Dataset
rm(list=ls())
load("Data/NorthCarolina.RData")
ggplot(nc, aes(x=fct_infreq(CMPD_Division), after_stat(count)))+theme_bw()+
  geom_bar()+theme(axis.text.x = element_text(angle = 45, hjust=1))+
  labs(x="Charlotte Police Deparment Division", y='Frequency') #Figure 1a
ggsave("Figures/Fig1a_FrequencyByDivision.pdf", width=6, height=4.5)

#Figure 1b: Estimated Police Division Fixed Effects in the Charlotte Police Department 
#OLS Regression Explaining Searches Following a Traffic Stop
load('Data/NCSearch_OLS.RData')
divisions <- data.frame('Division' = attributes(coef(nc.search))$names,
                        'Effect'   = coef(nc.search)) %>% 
              filter(grepl('CMPD_Division', Division))
divisions$Division <- gsub('factor(CMPD_Division)','',divisions$Division, fixed=T)
divisions <- divisions %>% arrange(-Effect)

coef_map <- setNames(as.list(divisions$Division),attributes(divisions)$row.names)
add_rows = data.frame(
  term = 'Central Division',
  model = '(1)',
  estimate = 0
)
attr(add_rows, 'position')=10
modelplot(nc.search, coef_map=coef_map, add_rows=add_rows)+
  labs(x='Division Fixed Effect Estimates and 95% Confidence Intervals', y='Charlotte Police Deparment Division')
ggsave("Figures/Fig1b_DivisionEffects.pdf", width=7, height=4.5)

#Figure 2: Expected Probability of Being Searched by a Male or Female Officer, by Agency and Estimator
rm(list=ls())
#nc
load("Data/NCSearch_Logit.RData") #logistic regression from original authors

nc.logit.mode = predict(nc.search,
                     newdata = data.frame("of_gender"=c(0,1),
                                          "race_gender"=0,
                                          "subject_age"=36,
                                          "investigatory"=1,
                                          "Officer_Years_of_Service"=10.25,
                                          "of_race"=0,"month"="01",
                                          "year"=2019,"CMPD_Division"="South Division"),
                     type="response",se.fit=T) #"predict" the expected probability for a "typical" stop, where the officer and driver are both white, the driver is aged 36,
#the driver is driving a local car, it is an investigatory search, the officer's experience is 10.25 years, the officer is aged 39,
#the stop took place in Jan 2019, in "South Division"
source('Code/AuxillaryFunctions.R')

load("Data/NorthCarolina.RData") #load cleaned NC data
nc <- drop_na(nc, any_of(c("race_gender","subject_age","investigatory", "of_race",
                           "of_gender","Officer_Years_of_Service", "month","year",
                           "CMPD_Division"))) #remove missing value so we don't get NAs in results
nc.logit.bootstrap <- bootstrap_prediction(nc, regression = nc.search, num_simulations = 1000, seed = 98) #do monte carlo predictions for NC
load("Data/NCSearch_OLS.RData") #OLS regression from original authors
nc.ols.mode = predict(nc.search,
                        newdata = data.frame("of_gender"=c(0,1),
                                             "race_gender"=0,
                                             "subject_age"=36,
                                             "investigatory"=1,
                                             "Officer_Years_of_Service"=10.25,
                                             "of_race"=0,"month"="01",
                                             "year"=2019,"CMPD_Division"="South Division"),
                        type="response",se.fit=T) #"predict" the expected probability for a "typical" stop, where the officer and driver are both white, the driver is aged 36,
#the driver is driving a local car, it is an investigatory search, the officer's experience is 10.25 years, the officer is aged 39,
#the stop took place in Jan 2019, in "South Division"
nc.ols.bootstrap <- bootstrap_prediction(nc, regression = nc.search, num_simulations = 1000, seed = 98) #do monte carlo predictions for NC

rm(nc.search, nc)

#fl
load("Data/FLSearch_Logit.RData") #logistic regression from original authors
fl.logit.mode = predict(fl.search,
                     newdata = data.frame("of_gender"=c(0,1),"race_gender"=0,
                                          "subject_age"=35,"out_of_state"=0,
                                          "investigatory"=1,
                                          "officer_years_of_service"=6,
                                          "of_race"=0,"officer_age"=39,
                                          "hour_of_day"=15,
                                          "month"="05","year"=2013,
                                          "county_name"="Orange County"),
                     type="response",se.fit=T) #"predict" the expected probability for a "typical" stop, where the officer and driver are both white, the driver is aged 35,
#the driver is driving a local car, it is an investigatory search, the officer's experience is 6 years, the officer is aged 39,
#the stop took place at 15:00, in May 2013, in "Orange County"

load("Data/FloridaSmall.RData") #load cleaned FL data
fl.sm <- subset(fl.sm, fl.sm$county_include==1&fl.sm$officer_exclude==0) #exclude to get the same data as the regression



fl.logit.bootstrap <- bootstrap_prediction(fl.sm, regression = fl.search, num_simulations = 1000, seed = 98) #do predictions for fl data

load("Data/FLSearch_OLS.RData")
fl.ols.mode = predict(fl.search,
                        newdata = data.frame("of_gender"=c(0,1),"race_gender"=0,
                                             "subject_age"=35,"out_of_state"=0,
                                             "investigatory"=1,
                                             "officer_years_of_service"=6,
                                             "of_race"=0,"officer_age"=39,
                                             "hour_of_day"=15,
                                             "month"="05","year"=2013,
                                             "county_name"="Orange County"),
                        type="response",se.fit=T) #"predict" the expected probability for a "typical" stop, where the officer and driver are both white, the driver is aged 35,
#the driver is driving a local car, it is an investigatory search, the officer's experience is 6 years, the officer is aged 39,
#the stop took place at 15:00, in May 2013, in "Orange County"

fl.ols.bootstrap <- bootstrap_prediction(fl.sm, regression = fl.search, num_simulations = 1000, seed = 98) #do predictions for fl data

save(fl.logit.bootstrap, fl.logit.mode, fl.ols.bootstrap, fl.ols.mode,
     nc.logit.bootstrap, nc.logit.mode, nc.ols.bootstrap, nc.ols.mode,
     file="Data/search.predict.RData") #save all results

rm(fl.sm,fl.search) #remove the huge dataset to save RAM


#load('Data/search.predict.RData') #in case the predictions are not run
pred.df = data.frame("Department" = c(rep("Charlotte Police Department", 8), rep("Florida Highway Patrol", 8)),
                     "Gender" = rep(c('Male','Female'),8),
                     'Type'   = c("OLS Mode", "OLS Mode",
                                  'Logit Mode', 'Logit Mode',
                                  'OLS Bootstrap', 'OLS Bootstrap',
                                  'Logit Bootstrap', 'Logit Bootstrap'),
                     "Predict" = c(nc.ols.mode$fit,
                                   nc.logit.mode$fit,
                                   nc.ols.bootstrap$predicted_probs$male,
                                   nc.ols.bootstrap$predicted_probs$female,
                                   nc.logit.bootstrap$predicted_probs$male,
                                   nc.logit.bootstrap$predicted_probs$female,
                                   fl.ols.mode$fit,
                                   fl.logit.mode$fit,
                                   fl.ols.bootstrap$predicted_probs$male,
                                   fl.ols.bootstrap$predicted_probs$female,
                                   fl.logit.bootstrap$predicted_probs$male,
                                   fl.logit.bootstrap$predicted_probs$female),
                     "Lower"=c(nc.ols.mode$fit-1.96*nc.ols.mode$se.fit,
                               nc.logit.mode$fit-1.96*nc.logit.mode$se.fit,
                               nc.ols.bootstrap$lower_ci$male,
                               nc.ols.bootstrap$lower_ci$female,
                               nc.logit.bootstrap$lower_ci$male,
                               nc.logit.bootstrap$lower_ci$female,
                               fl.ols.mode$fit-1.96*fl.ols.mode$se.fit,
                               fl.logit.mode$fit-1.96*fl.logit.mode$se.fit,
                               fl.ols.bootstrap$lower_ci$male,
                               fl.ols.bootstrap$lower_ci$female,
                               fl.logit.bootstrap$lower_ci$male,
                               fl.logit.bootstrap$lower_ci$female),
                     "Upper"=c(nc.ols.mode$fit+1.96*nc.ols.mode$se.fit,
                               nc.logit.mode$fit+1.96*nc.logit.mode$se.fit,
                               nc.ols.bootstrap$upper_ci$male,
                               nc.ols.bootstrap$upper_ci$female,
                               nc.logit.bootstrap$upper_ci$male,
                               nc.logit.bootstrap$upper_ci$female,
                               fl.ols.mode$fit+1.96*fl.ols.mode$se.fit,
                               fl.logit.mode$fit+1.96*fl.logit.mode$se.fit,
                               fl.ols.bootstrap$upper_ci$male,
                               fl.ols.bootstrap$upper_ci$female,
                               fl.logit.bootstrap$upper_ci$male,
                               fl.logit.bootstrap$upper_ci$female)) #prepare a dataframe for plotting
pred.df$Type <- factor(pred.df$Type, levels=c("OLS Mode",'Logit Mode','OLS Bootstrap','Logit Bootstrap'))

ggplot(data = pred.df, aes(x=Gender,y=Predict, color=Type, linetype=Type, shape=Type)) + 
  geom_point(size=2,position = position_dodge(.5)) +  
  geom_errorbar(aes(ymin = Lower, ymax = Upper),
                width=.2,size = 0.75,                   
                position=position_dodge(.5)) + 
  ylab("Expected Probbility of a Search") +
  xlab("Officer Sex") +
  theme_bw(base_size=15) +facet_wrap(~Department)+
  scale_colour_manual(name  ="Estimator",
                      breaks=c("OLS Mode", "Logit Mode", "OLS Bootstrap", "Logit Bootstrap"),
                      values  = c('#F8766D','#00BFC4','#F8766D','#00BFC4'))+
  scale_linetype_manual(name  ="Estimator",
                        breaks=c("OLS Mode", "Logit Mode", "OLS Bootstrap", "Logit Bootstrap"),
                        values  = c('dashed','dashed','solid','solid'))+
  scale_shape_manual(name  ="Estimator",
                     breaks=c("OLS Mode", "Logit Mode", "OLS Bootstrap", "Logit Bootstrap"),
                     values  = c(17,17,20,20)) #figure 2
ggsave("Figures/Fig2_PredProb_correction.pdf", width=10, height=6)

pred.df$Predict[7]/pred.df$Predict[8]#(1.51): logit bootstrapped male-to-female ratio of search probability for nc
pred.df$Predict[15]/pred.df$Predict[16]#(4.08): logit bootstrapped male-to-female ratio of search probability for fl
pred.df$Predict[5]/pred.df$Predict[6]#(1.84): OLS bootstrapped male-to-female ratio of search probability for nc
pred.df$Predict[13]/pred.df$Predict[14]#(4.35): OLS bootstrapped male-to-female ratio of search probability for fl

####################Section 5#############################
#Figure 3: Expected Hit rate per 100 Stops by a Male or Female Officer, by Estimator
rm(list=ls())
source('Code/AuxillaryFunctions.R')
load("Data/FL_Aggregated.RData")
load('Data/FlStopRate_OLS.RData')
load('Data/FLSearchSearchStopRate_Logit.Rdata')
fl.stop.logit.bootstrap <- bootstrap_prediction(fl.ag.officers, regression = contra.stop.rate.logit, num_simulations = 1000, seed = 98) #do predictions for fl data
fl.stop.ols.bootstrap <- bootstrap_prediction(fl.ag.officers, regression = contra.stop.rate.reg, num_simulations = 1000, seed = 98) #do predictions for fl data
save(fl.stop.logit.bootstrap, fl.stop.ols.bootstrap, file='Data/fl.stop.bootstrap.Rdata')

#load('Data/fl.stop.bootstrap.Rdata')
pred.df = data.frame("Department" = rep("Florida Highway Patrol",4),
                     "Gender" = c("Male","Female","Male","Female"),
                     'Type'   = c('OLS Bootstrap', 'OLS Bootstrap',
                                  'Logit Bootstrap', 'Logit Bootstrap'),
                     "Predict" = c(fl.stop.ols.bootstrap$predicted_probs$male,
                                   fl.stop.ols.bootstrap$predicted_probs$female,
                                   fl.stop.logit.bootstrap$predicted_probs$male*100,
                                   fl.stop.logit.bootstrap$predicted_probs$female*100),
                     "Lower"=c(fl.stop.ols.bootstrap$lower_ci$male,
                               fl.stop.ols.bootstrap$lower_ci$female,
                               fl.stop.logit.bootstrap$lower_ci$male*100,
                               fl.stop.logit.bootstrap$lower_ci$female*100),
                     "Upper"=c(fl.stop.ols.bootstrap$upper_ci$male,
                               fl.stop.ols.bootstrap$upper_ci$female,
                               fl.stop.logit.bootstrap$upper_ci$male*100,
                               fl.stop.logit.bootstrap$upper_ci$female*100)) #prepare a dataframe for plotting
pred.df$Type <- factor(pred.df$Type, levels=c('OLS Bootstrap','Logit Bootstrap'))
ggplot(data = pred.df, aes(x=Gender,y=Predict, color=Type)) + 
  geom_point(size=2,position = position_dodge(.5)) +  
  geom_errorbar(aes(ymin = Lower, ymax = Upper),
                width=.2,size = 0.75,                   
                position=position_dodge(.5)) + 
  ylab("Expected Hit Rate per 100 stops") +
  xlab("Officer Sex") +
  theme_bw(base_size=15) +facet_wrap(~Department)  #do a plot
ggsave("Figures/Fig3_PredHit_100stops.pdf", width=7, height=6)

pred.df$Predict[1]/pred.df$Predict[2]#(2.29)OLS:male-to-female ratio of expected hit rate per 100 stops
pred.df$Predict[3]/pred.df$Predict[4]#(2.95)Logit:male-to-female ratio of expected hit rate per 100 stops

#Table C2: Hierarchical Linear Models Including Officer Random Effects for Hit Rate per 100 Stops (FHP data)

rm(list=ls())
source('Code/AuxillaryFunctions.R')
load("Data/FL_Aggregated.RData")
load("Data/FlStopRate_OLS_FE.RData") #original random effects model

fixed <- feols(contra.stop.rate ~ factor(of_gender) + factor(of_exper) + 
                 factor(of_age) + factor(of_race) +
                 factor(race_gender) + factor(driver_age)+ 
                 investigatory + out_of_state +
                 factor(year)+factor(tod)|officer_id, vcov='IID',
               data=fl.ag.officers) #purely for generating the names of time-variant variables

  #Correlated Random Effects
dummies <- model.matrix(~ factor(of_age) + factor(race_gender)+factor(driver_age)+factor(year)+factor(tod) -1, fl.ag.officers)
#time_variants <- names(coef(fixed))
#save(time_variants, file="Data/time_invariant.RData")
load("Data/time_invariant.RData")
dummies <- subset(dummies, select=colnames(dummies) %in% time_variants)

data_dummies <- cbind(fl.ag.officers, dummies)

means <- data_dummies %>%
  group_by(officer_id) %>%
  summarise_at(vars(c('factor(of_age)2':'factor(tod)8', 'of_exper', 'investigatory', 'out_of_state')), mean, na.rm=T)

data_means <- data_dummies %>% dplyr::select(-c('factor(of_age)2':'factor(tod)8'))

data_means <- full_join(data_means, means, by='officer_id', suffix=c('','_mean'))

data_means <- rename_with(
  data_means,
  ~ paste0(.x, "_mean",recycle0 = TRUE),
  starts_with("factor(")
)

data_dummies <- data_dummies %>%
  rename_with(~gsub(".", "_", .x, fixed = TRUE)) %>%
  rename_with(~gsub("(", "_", .x, fixed = TRUE)) %>%
  rename_with(~gsub(")", "_", .x, fixed = TRUE))

save(data_dummies, data_means, file='Data/fl.ag.officers.means.Rdata')
load('Data/fl.ag.officers.means.Rdata')
contra.stop.rate.correlated = lmer(contra.stop.rate ~ factor(of_gender) + factor(of_exper) + 
                              factor(of_age) + factor(of_race) +
                              factor(race_gender) + factor(driver_age)+ 
                              investigatory + out_of_state +
                              factor(year)+factor(tod)+
                              `factor(of_age)2_mean`+`factor(of_age)3_mean`+`factor(race_gender)1_mean`+`factor(race_gender)2_mean`+
                                `factor(race_gender)3_mean`+ `factor(race_gender)4_mean`+
                              `factor(race_gender)5_mean`+`factor(driver_age)2_mean`+`factor(driver_age)3_mean`+
                              `factor(year)2011_mean`+`factor(year)2012_mean`+`factor(year)2013_mean`+
                              `factor(year)2014_mean`+`factor(year)2015_mean`+
                              `factor(tod)2_mean`+`factor(tod)3_mean`+`factor(tod)4_mean`+`factor(tod)5_mean`+`factor(tod)6_mean`+
                              `factor(tod)7_mean`+`factor(tod)8_mean`+`of_exper_mean`+`investigatory_mean`+`out_of_state_mean`+
                                (1|officer_id),
                            data=data_means) 

contra.stop.rate.ht <- plm(contra_stop_rate ~  factor(of_gender)+of_exper+ factor_of_age_2+factor_of_age_3+factor(of_race)+
                             factor_race_gender_1+factor_race_gender_2+factor_race_gender_3+factor_race_gender_4+factor_race_gender_5 + factor_driver_age_2+
                             factor_driver_age_3+investigatory + out_of_state +factor_year_2011+factor_year_2012+factor_year_2013+
                             factor_year_2014+factor_year_2015+factor_tod_2+factor_tod_3+factor_tod_4+factor_tod_5+
                             factor_tod_6+factor_tod_7+factor_tod_8|factor_of_age_2+factor_of_age_3+
                             factor_race_gender_1+factor_race_gender_2+factor_year_2011+factor_year_2012+factor_year_2013+
                             factor_year_2014+factor_tod_2+factor_tod_3+factor_tod_4+factor_tod_6+
                             +factor_tod_7+of_exper+out_of_state|factor(of_gender)+
                             of_exper+factor(of_race)+factor_race_gender_3+factor_race_gender_4+factor_race_gender_5+
                             factor_driver_age_2+factor_driver_age_3+factor_year_2015+factor_tod_5+factor_tod_8+investigatory,
               data=data_dummies, index='officer_id', effect='individual',
               random.method = "ht", model = "random", inst.method = "baltagi")
save(contra.stop.rate.correlated, contra.stop.rate.ht, file='Data/FlStopRate_corrlated_ht.RData')

coef_map = list("factor(of_gender)1"="Female Officer",
                "factor(of_race)1"="Black Officer",
                "factor(of_age)2"='Officer Age: 30-64',
                "factor_of_age_2"='Officer Age: 30-64',
                "factor(of_age)3"='Officer Age: 65+',
                "factor_of_age_3"='Officer Age: 65+',
                "factor(of_exper)1"="Experienced Officer",
                "of_exper"="Experienced Officer",
                "factor(race_gender)1"="White Male",
                "factor_race_gender_1"="White Male",
                "factor(race_gender)2"="Black Male",
                "factor_race_gender_2"="Black Male",
                "factor(race_gender)3"="Black Female",
                "factor_race_gender_3"="Black Female",
                "factor(race_gender)4"="Latino Male",
                "factor_race_gender_4"="Latino Male",
                "factor(race_gender)5"="Latino Female",
                "factor_race_gender_5"="Latino Female",
                "factor(driver_age)2"="Driver Age: 30-64",
                "factor_driver_age_2"="Driver Age: 30-64",
                "factor(driver_age)3"="Driver Age: 65+",
                "factor_driver_age_3"="Driver Age: 65+",
                "investigatory"="Investigatory Stop Purpose",
                "out_of_state"="Out of State",
                "(Intercept)"="(Intercept)")
f <- function(x) format(round(x, 3), big.mark=",")
gof_map = list(list("raw" = "nobs", "clean" = "$N$", "fmt" = f))

modelsummary(list('Original' = contra.stop.rate.reg,
                  'Correlated' = contra.stop.rate.correlated,
                  'Hausman-Taylor' = contra.stop.rate.ht),
             stars=T, coef_map = coef_map, gof_map=gof_map,
             output = 'latex') #Table C2


