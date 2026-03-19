# extension for firth regression  (logistf package) for use by texreg

extract.logistf <- function(model, include.nobs=TRUE, include.loglik = FALSE, ...) {
  s<-summary(model)
  coefnames <- names(coef(model))
  coefs <- coef(model)
  se <- sqrt(diag(vcov(model)))
  p <- s$prob

  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()

 if (include.nobs == TRUE) {
    n <- s$n
    gof <- c(gof, n)
    gof.names <- c(gof.names, "Num.\ obs.")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.loglik == TRUE) {
    ll <- s$loglik[2]
    gof <- c(gof, ll)
    gof.names <- c(gof.names, "Log Likelihood")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  
  tr <- createTexreg(
    coef.names = coefnames, 
    coef = coefs,
    se = se,
    pvalues = p, 
    gof.names = gof.names, 
    gof = gof, 
    gof.decimal = gof.decimal
  )
  return(tr)
}

setMethod("extract", signature = className("logistf", "logistf"), 
    definition = extract.logistf)
