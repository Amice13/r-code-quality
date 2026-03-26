summary.pgmm_R <- function (object, robust = FALSE, time.dummies = FALSE, ...) 
{
  model <- plm:::describe(object, "model")
  effect <- plm:::describe(object, "effect")
  transformation <- plm:::describe(object, "transformation")
  if (robust) {
    vv <- vcovHC(object)
  }
  else {
    vv <- vcov(object)
  }
  if (model == "onestep") 
    K <- length(object$coefficients)
  else K <- length(object$coefficients[[2]])
  Kt <- length(object$args$namest)
  if (!time.dummies && effect == "twoways") 
    rowsel <- -c((K - Kt + 1):K)
  else rowsel <- 1:K
  std.err <- sqrt(diag(vv))
  b <- coef(object)
  z <- b/std.err
  p <- 2 * pnorm(abs(z), lower.tail = FALSE) 
  CoefTable <- cbind(b, std.err, z, p)
  colnames(CoefTable) <- c("Estimate", "Std. Error", "z-value", 
                           "Pr(>|z|)")
  CoefTable <- CoefTable[rowsel, , drop = FALSE]
  sargan <- sargan(object)

  if (length(object$residuals[[1]] ) > 2) object$m2 <- plm:::mtest(object, 2, vv) 
  wald.coef <- plm:::pwaldtest(object, "coef", vv)
  if (plm:::describe(object, "effect") == "twoways") 
    wald.td <- plm:::pwaldtest(object, "time", vv)
  return(list("Coef Table"=round(CoefTable,3), "Sargan Test"=sargan))
}



extract.pgmm_R <- 
  function (model, include.nobs = TRUE, include.sargan = TRUE, 
            include.wald = TRUE, ...) 
  {
    s <- model
    if (plm:::describe(s, "model") == "onestep") {
      coefficient.names <- names(s$coefficients)
      coefficients <- s$coefficients
    }
    else {
      coefficient.names <- names(s$coefficients[[2]])
      coefficients <- s$coefficients[[2]]
    }
    vv <- vcovHC(s)
    standard.errors <- sqrt(diag(vv))
    z <- coefficients / standard.errors
    significance <- qnorm(0.975) * pnorm(abs(z), lower.tail = FALSE) 
    gof <- numeric()
    gof.names <- character()
    gof.decimal <- logical()
    if (include.nobs == TRUE) {
      n <- attr(s, "pdim")$nT$n
      T <- attr(s, "pdim")$nT$T
      N <- attr(s, "pdim")$nT$N
      ntot <- sum(unlist(s$residuals) != 0)
      gof <- c(gof, n, T, N, ntot)
      gof.names <- c(gof.names, "n", "T", "Num. obs.", 
                     "Num. obs. used")
      gof.decimal <- c(gof.decimal, FALSE, FALSE, FALSE, FALSE)
    }
    if (include.sargan == TRUE) {
      ss <- sargan(s)
      sarg.stat <- ss$statistic
      sarg.par <- ss$parameter
      sarg.pval <- ss$p.value
      gof <- c(gof, sarg.stat, sarg.par, sarg.pval)
      gof.names <- c(gof.names, "Sargan Test: chisq", 
                     "Sargan Test: df", "Sargan Test: p-value")
      gof.decimal <- c(gof.decimal, TRUE, TRUE, TRUE)
    }
    if (include.wald ==FALSE) {
      ww <- plm:::pwaldtest(s, "coef", vv)
      wald.coef <- ww$statistic
      wald.pval <- ww$p.value
      wald.par <- ww$parameter
      gof <- c(gof, wald.coef, wald.par, wald.pval)
      gof.names <- c(gof.names, "Wald Test Coefficients: chisq", 
                     "Wald Test Coefficients: df", "Wald Test Coefficients: p-value")
      gof.decimal <- c(gof.decimal, TRUE, FALSE, TRUE)
      if (!is.null(s$wald.td)) {
        td.coef <- s$wald.td$statistic[1]
        td.pval <- s$wald.td$p.value[1]
        td.par <- s$wald.td$parameter
        gof <- c(gof, td.coef, td.par, td.pval)
        gof.names <- c(gof.names, "Wald Test Time Dummies: chisq", 
                       "Wald Test Time Dummies: df", "Wald Test Time Dummies: p-value")
        gof.decimal <- c(gof.decimal, TRUE, FALSE, TRUE)
      }
    }
    tr <- createTexreg(coef.names = coefficient.names, coef = coefficients, 
                       se = standard.errors, pvalues = significance, gof.names = gof.names, 
                       gof = gof, gof.decimal = gof.decimal)
    return(tr)
  }






