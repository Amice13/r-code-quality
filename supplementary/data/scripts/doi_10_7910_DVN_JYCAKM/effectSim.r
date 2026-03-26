#' effectSim - simulate the effect of change along a continous variable
#' 
#' Simulates substantive effects from glm and parametric duration models. 
#' @author Bjørn Høyland
#' @description \code{effectSim} takes a model and a matrix of fixed variable-values. It returns a matrix of predicted effects (oftentimes probabilties) with number of rows equal to sims. Higher number of sims increases the precision in the effects.
#' @param model model to be simulated from
#' @param beta to be used in the simulation, defaults to coef(beta)
#' @param covvar variance-covariance matrix used to calculate uncertainty, defaults to vcov(model)
#' @param x matrix of variable values
#' @param sims number of simulations, defaults to 1000
#' @details \code{effectSim} rely on \code{\link{mvrnorm}} to simulate values from the multivariate normal distribution given beta and covvar. These values are then multiplied by the x matrix and put into the relevant link function. 
#' @export
#' @examples
#' library(car);data(Chile)
#' Chile$voteBinary <- recode(Chile$vote,"c('A','U')=NA")
#' mod <- glm(voteBinary ~ statusquo + age,data=Chile,family=binomial(logit))
#' x <-cbind(1,0,18:75)
#' prob.out <- effectSim(model=mod,x=x,sims=1000)
#' effectSimPlot(prob.out,low=18,high=75,x=x,i=3,covariate=Chile$age, 
#' main="Voting Pinochet",xlab="Age",ylab="Probability",bty="n",sub="Logit")
effectSim<- function(model, beta=coef(model),covvar=vcov(model),x,sims=1000){
  beta <- beta
  covvar <- covvar
  beta.sim <- mvrnorm(sims, beta,covvar)
  xb <- x %*% t(beta.sim)
  if (is.null(model$family$link)==FALSE){
  if (model$family$link=="logit") {
    p <- 1/(1 + exp(-xb))
  } else if (model$family$link=="probit") {
    p <- pnorm(xb)
  }else if (model$family$link=="cloglog") {
    p <- 1 - exp(-exp(xb)) 
  }else if (model$family$link=="cauchit") {
    p <- pi^(-1)*atan(xb) + .5 
  } else if (model$family$link=="log") {
    p <- exp(xb)
  }else if (model$dist=="logistic") {
    p <- 1/(1 + exp(-xb))
  } else if (model$dist=="weibull") {
    p <- exp(-xb)
  }else {
    break
    }
  }else if (class(model)[1]=="clogit") {
    p <- exp(xb)
  }else if (class(model)[1]=="coxph") {
    p <- exp(xb)
  }else if (class(model)=="lm") {
    p <- xb 
   }else{ 
     break
  }
  class(p) <- "effectSim"
  return(p)
}