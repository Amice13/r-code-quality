
#from https://rpubs.com/bbolker/glmmchapter


#source("C:/Users/lisanjie2/Documents/1_R/Fx_FUNCTIONS/easyPredCI.R")
#
### Bolker::easyPredCI function ###
easyPredCI <- function(model,
                       newdata,
                       alpha=0.05) 
{
  ## baseline prediction, on the linear predictor (logit) scale:
  pred0 <- predict(model,re.form=NA,newdata=newdata)
  
  ## fixed-effects model matrix for new data
  X <- model.matrix(formula(model,
                            fixed.only=TRUE)[-2],
                    newdata)
  
  ## fixed-effects coefficients (betas; regression coefs); ingnore ranef
  beta <- fixef(model) 
  
  ## variance-covariance matrix of beta
  V <- vcov(model)     
  
  ## std errors of predictions
  pred.se <- sqrt(diag(X %*% V %*% t(X))) #this is a standard regression result that I don't understand
  
  
  ## inverse-link (logistic) function: could also use plogis()
  linkinv <- model@resp$family$linkinv
  
  ## construct 95% Normal CIs on the link scale and
  ##  transform back to the response (probability) scale:
  crit <- -qnorm(alpha/2)
  
  
  # data on original link scale (log odds for logistic, log response ratio for poisson)
  linkscale <- data.frame(lw=pred0-crit*pred.se,
                             up=pred0+crit*pred.se)
  
  names(linkscale) <- paste("log", names(linkscale),sep = ".")
  
  
  # data on exponentiated scale (odds for logistic, response ratio for poisson)
  expscale <- data.frame(lw=exp(pred0-crit*pred.se),
                     up=exp(pred0+crit*pred.se))
  
  names(expscale) <- paste("exp", names(expscale),sep = ".")
  
  
  #data on real scale (inverse link scale; probablity for logit, count for poisson)
  realscale <- data.frame(linkinv(cbind(lw=pred0-crit*pred.se,
                             up=pred0+crit*pred.se)))
  
  names(realscale) <- paste("real",names(realscale),sep = ".")
  
  cbind(linkscale,expscale,realscale)
}

print("Bolker easy pred CI for GLMM easyPredCI()")



# prediction call
# cpred1 <- predict(cmod_lme4_L,
#                   re.form=NA,
#                   newdata=pframe,
#                   type="response")











# for lmer
### Bolker::easyPredCI function ###
easyPredCI.lmm <- function(model,
                       newdata,
                       alpha=0.05) 
{
  ## baseline prediction, on the linear predictor (logit) scale:
  pred0 <- predict(model,re.form=NA,newdata=newdata)
  
  ## fixed-effects model matrix for new data
  X <- model.matrix(formula(model,
                            fixed.only=TRUE)[-2],
                    newdata)
  
  ## fixed-effects coefficients (betas; regression coefs); ingnore ranef
  beta <- fixef(model) 
  
  ## variance-covariance matrix of beta
  V <- vcov(model)     
  
  ## std errors of predictions
  pred.se <- sqrt(diag(X %*% V %*% t(X))) #this is a standard regression result that I don't understand
  
  
        #   ## inverse-link (logistic) function: could also use plogis()
        #   linkinv <- model@resp$family$linkinv
        #   
  
  ## construct 95% Normal CIs on the link scale and
  ##  transform back to the response (probability) scale:
  crit <- -qnorm(alpha/2)
  cbind(predict = pred0,
        lw=pred0-crit*pred.se,
                up=pred0+crit*pred.se)
}






### Bolker::easyPredCI function for ADMB ###
easyPredCI.admb <- function(model,
                            newdata = model$frame,
                            alpha=0.05,
                            form = form,
                            link = "log") {
  
  ## baseline prediction, on the linear predictor (logit) scale:
  pred0 <- predict(model,re.form=NA,newdata=newdata)
  
  ## fixed-effects model matrix for new data
  X <- model.matrix(form,
                    newdata)
  
  ## fixed-effects coefficients (betas; regression coefs); ingnore ranef
  beta <- fixef(model) 
  
  ## variance-covariance matrix of beta
  V <- vcov(model)     
  
  ## std errors of predictions
  pred.se <- sqrt(diag(X %*% V %*% t(X))) #this is a standard regression result that I don't understand
  
  
  #   ## set link scale
  if(link == "log")   linkinv <- function(x) {exp(x)}
  if(link == "logit") linkinv <- function(x) {1/(1 + exp(-x))}
  
  ## construct 95% Normal CIs on the link scale and
  ##  transform back to the response (probability) scale:
  crit <- -qnorm(alpha/2)
  
  
  # data on original link scale (log odds for logistic, log response ratio for poisson)
  linkscale <- data.frame(lw=pred0-crit*pred.se,
                          up=pred0+crit*pred.se)
  
  #add "log" to the link scale
  #   logitstic models are estimated on the log odds scale
  #   poisson models are estiamted on the log rate ratio scale
  names(linkscale) <- paste("log", names(linkscale),sep = ".")
  
  
  # data on exponentiated scale (odds for logistic, response ratio for poisson)
  expscale <- data.frame(lw=exp(pred0-crit*pred.se),
                         up=exp(pred0+crit*pred.se))
  
  names(expscale) <- paste("exp", names(expscale),sep = ".")
  
  
  #data on real scale (inverse link scale; probablity for logit, count for poisson)
  realscale <- data.frame(linkinv(cbind(lw=pred0-crit*pred.se,
                                        up=pred0+crit*pred.se)))
  
  names(realscale) <- paste("real",names(realscale),sep = ".")
  
  cbind(linkscale,expscale,realscale)
}








print("Bolker easy pred CI for linear MM easyPredCI.lmm()    Be sure to declare model type as lmer glmer or admb  all lower case  for admb must linclude fixed effects formula w/o response")








# cpred1.CI <- easyPredCI(cmod_lme4_L,pframe)

# 
# 
# # this is a breakdown of the call to formula() that is within the function he defines
# # note that there is a particular formula call for lme4 formula.merMod
# 
# # the formula for the model
# formula(model)
# 
# # drop the ranef; formula.merMod must know how to pars the ranefs
# formula(model, fixed.only=TRUE)
# 
# #drop the response variable; not sure why is happens to be in slot #2
# formula(model, fixed.only=TRUE)[-2]
# 
# #examine formula object
# #has odd structure
# str(formula(model, fixed.only=TRUE))
# formula(model, fixed.only=TRUE)[1]  #`~`()
# formula(model, fixed.only=TRUE)[2]  #predation()
# formula(model, fixed.only=TRUE)[3]  #ttt()
