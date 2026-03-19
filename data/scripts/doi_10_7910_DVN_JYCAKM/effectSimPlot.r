#' effectSimPlot - plots the simulated effect of change along a continous variable
#' 
#' plots the simulated effects from \code{effectSim}
#' @param p matrix of simulated effects from \code{effectSim}
#' @param high maximum value on the investigated variable
#' @param low mimimum value on the investigated variable
#' @param x matrix of variables at fixed values
#' @param i indexes the covariate that varies
#' @param covariate full name of varying covariate (data$covariate)
#' @param rugplot if TRUE (default) a rugplot will be added. It may be useful to set this to FALSE if there are very many observations.
#' @param ... additional arguments passed to \code{\link{plot}} 
#' @author Bjørn Høyland
#' @export
#' @seealso \code{\link{sim}} in the arm library
#' @examples
#' library(car);data(Chile)
#' Chile$voteBinary <- recode(Chile$vote,"c('A','U')=NA")
#' mod <- glm(voteBinary ~ statusquo + age,data=Chile,family=binomial(logit))
#' x <-cbind(1,0,18:75)
#' prob.out <- effectSim(model=mod,x=x,sims=1000)
#' effectSimPlot(prob.out,low=18,high=75,x=x,i=3,covariate=Chile$age, 
#' main="Voting Pinochet",xlab="Age",ylab="Probability",bty="n",sub="Logit")
effectSimPlot <- function(p,x,low,high,i,covariate,rugplot=TRUE,color="skyblue",...){
  if (class(p)!="effectSim"){
    cat("not an effectSim object")
  }else{
predicted.prob <- t(apply(p,1,quantile, probs = c(.05,.5,.95)))
cord.y <- c(predicted.prob[,1],rev(predicted.prob[,3]))
cord.x <- c(seq(low,high,length=nrow(p)),seq(high,low,length=nrow(p)))
plot(0,0, type="n",xlim=c(low,high),...)
polygon(cord.x,cord.y,col=color,border=NA)
lines(x[,i],predicted.prob[,2],lty=1)
if (rugplot==TRUE){
rug(jitter(covariate,.5))
}
}}