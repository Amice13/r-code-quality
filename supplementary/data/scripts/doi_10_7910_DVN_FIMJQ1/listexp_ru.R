#### Additional analyses for "Solid support or secret dissent? A list 
#### experiment on preference falsification during the Russian war against Ukraine"
#### Authors: Philip Chapkovski and Max Schaub

# Clear environment
rm(list = ls())
gc()


# Packages
library(pacman)
p_load('arm')
library(tidyverse)
library(haven)
library(list)
library(misreport)

# Set working directory
setwd("") # set your directory


# Load data 
data <- read_dta("toloka_2022_04_13.dta")


# Encoding
data$nlist <- data$playermain # outcome
data$treat <- ifelse(data$playertreatment=="treatment",1,0) 
data$direct <- data$playerdirect
data$female <- data$playergender
data$male <- ifelse(data$playergender==1,0,1)
data$age <- data$playerage
data$fortyplus <- ifelse(data$age>=40, 1, 0) 
data$edu <- data$playereducation
data<-data%>%mutate(tv=ifelse(playermedia_tv=='1', 1,0))


#### Testing for design effects (Table A2) ####
designtest <- ict.test(data$nlist, data$treat, J = 3, alpha = 0.05, n.draws = 250000,
         gms = TRUE, pi.table = TRUE)
summary(designtest)



#### Modeling preference falsification (Table A4 and Figure 3a) ####

# enforce monotonicity assumption; exclude 23 observations that violate the assumption
data_m <- subset(data, !(data$nlist == 4 & data$direct == 0))


A <- data_m %>%
  dplyr::select(c(nlist, female , fortyplus ,edu,tv,playeremployment, treat, direct))%>%
  mutate(edu=if_else(edu>2,1,0))
model.1 <- listExperiment(nlist ~ 1 + female + fortyplus + edu+tv+playeremployment,
                          data = A, J = 3, 
                          treatment = "treat",
                          direct = "direct",
                          sensitive.response = 0,
                          control.constraint = "none",
                          misreport.treatment = F)

summary(model.1)

n_sims <- 200

# Simulate model parameters
coefs <- c(model.1$par.control, model.1$par.sens, model.1$par.misreport)
par_sim <- mvtnorm::rmvnorm(n_sims, coefs, model.1$vcov.mle)

# Get the matrix of parameters for the misreport submodel
# Note that the parameter estimates in par_sim are in the following order: control, sensitive, misreport
par_sim_misreport <- par_sim[, (length(coefs)-length(model.1$par.misreport)+1):length(coefs)]
data_m%>%skim
params <-  c("female", "fortyplus", "edu" , "tv", "playeremployment")

A%>%dplyr::select(matches('tv'))%>%names



diffsimulator <- function(varname, l1, l2, label) {
  pp_diff <- rep(NA, n_sims)
  
  # TV
  for (i in 1:n_sims) {
    data_l1 <-
      A %>% dplyr::select(-matches(varname)) %>% mutate("{varname}" := l1)
    pp_1 <-
      predict(model.1,
              newdata = data_l1,
              par.misreport = par_sim_misreport[i,])$u.hat
    
    data_l2 <-
      A %>% dplyr::select(-matches(varname)) %>% mutate("{varname}" := l2)
    pp_2 <-
      predict(model.1,
              newdata = data_l2,
              par.misreport = par_sim_misreport[i,])$u.hat
    
    pp_diff[i] <- mean(pp_2- pp_1)
  }
  
  
  tibble(est = mean(pp_diff),
         sd = sd(pp_diff),
         varname = varname,
         label=label)
}

diffparams <- data.frame(
  param= c("female", "fortyplus",  "tv", "playeremployment", "playeremployment", 'edu'),
  l1<-c(0,0,0,0,0,0),
  l2<-c(1,1,1,1,2,1),
  label=c('Female', '40 years and over','TV major info source','State employed','Not formally employed','Education')
)
diffsimulator('tv',0,1,'aaa')
ddd<-apply(diffparams, 1,function(x) {diffsimulator(x[1],as.numeric(x[2]),as.numeric(x[3]), x[4])})
forplot<-bind_rows(ddd)

coefplot(forplot$est, forplot$sd, varnames=forplot$label,mar=c(.7,.1,2.7,.1), main='', h.axis=T, xlim=c(-0.2,0.5))

sink(type = "message")
sink()
file.show("all.Rout")