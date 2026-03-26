####################
# Replication of Models in Main Text
#######################


  rm(list=ls())
  load("in/data_anes.Rdata")

# Interaction Effect Model
######################

  # Distance fa  
  m1  <- glm(voteObama ~ DISSfa  + resid + pid + age  + race + edu + home + income  , data=dta,  family=binomial(link = "probit")) 
  m11 <- glm(voteObama ~ DISSfa*resid  + pid   + age + race + edu + home + income, data=dta,  family=binomial(link = "probit")) 
  summary(m11)

  # Distance fa  
  m2  <- glm(voteObama ~ DISSfamean  + resid + pid + age  + race + edu + home + income, data=dta,  family=binomial(link = "probit")) 
  m21 <- glm(voteObama ~ DISSfamean*resid  + pid + age  + race + edu + home + income , data=dta,  family=binomial(link = "probit")) 
  summary(m21)

  # Distance SP
  m3  <- glm(voteObama ~ DISS + resid +pid + age  + race + edu + home + income, data=dta,  family=binomial(link = "probit")) 
  m31 <- glm(voteObama ~ DISS*resid + pid   + age + race + edu + home  + income, data=dta,  family=binomial(link = "probit")) 
  summary(m31)

  # Distance SP  mean
  m4  <- glm(voteObama ~ DISSmean + resid + pid + age  + race + edu + home + income, data=dta,  family=binomial(link = "probit"))
  m41 <- glm(voteObama ~ DISSmean*resid + pid   + age + race + edu + home + income, data=dta,  family=binomial(link = "probit")) 
  summary(m41)
  

# Save
##############

  save(m1,m11,m2,m21,m3,m31,m4,m41,
       dta,file="results_anes.Rdata")


