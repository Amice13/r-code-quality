extract.pgmm_mi <- 
  function (model, include.nobs = TRUE, include.sargan = TRUE, 
            include.wald = TRUE, ...)  {
    s <- MIcombine(MIextract(model, fun = coef), MIextract(model, fun = vcovHC))
    
    coefficient.names <- names(s[[1]])
    coefficients <- s[[1]]
  
    standard.errors <- sqrt(diag(s[[2]]))
    z <- coefficients / standard.errors
    significance <- qnorm(0.975) * pnorm(abs(z), lower.tail = FALSE) 
    gof <- numeric()
    gof.names <- character()
    gof.decimal <- logical()
    if (include.nobs == TRUE) {
      n <- attr(model[[1]], "pdim")$nT$n
      T <- attr(model[[1]], "pdim")$nT$T
      N <- attr(model[[1]], "pdim")$nT$N
      ntot <- sum(unlist(model[[1]]$residuals) != 0)
      gof <- c(gof, n, T, N, ntot)
      gof.names <- c(gof.names, "n", "T", "Num. obs.", 
                     "Num. obs. used")
      gof.decimal <- c(gof.decimal, FALSE, FALSE, FALSE, FALSE)
    }
    if (include.sargan == TRUE) {
      ss <- lapply(model, sargan)
      sarg.stat <- mean(sapply(ss, function(x) x$statistic))
      sarg.par <-  mean(sapply(ss, function(x) x$parameter))
      sarg.pval <- mean(sapply(ss, function(x) x$p.value))
      gof <- c(gof, sarg.stat, sarg.par, sarg.pval)
      gof.names <- c(gof.names, "Sargan Test: chisq", 
                     "Sargan Test: df", "Sargan Test: p-value")
      gof.decimal <- c(gof.decimal, TRUE, TRUE, TRUE)
    }
    if (include.wald == FALSE) {
      ww <- lapply(model, pwaldtest, param="coef")
      wald.coef <- mean(sapply(ww, function(x) x$statistic))
      wald.pval <- mean(sapply(ww, function(x) x$p.value))
      wald.par <- mean(sapply(ww, function(x) x$parameter))
      gof <- c(gof, wald.coef, wald.par, wald.pval)
      gof.names <- c(gof.names, "Wald Test Coefficients: chisq", 
                     "Wald Test Coefficients: df", "Wald Test Coefficients: p-value")
      gof.decimal <- c(gof.decimal, TRUE, FALSE, TRUE)
      }
    
    tr <- createTexreg(coef.names = coefficient.names, coef = coefficients, 
                       se = standard.errors, pvalues = significance, 
                       gof.names = gof.names, gof = gof, gof.decimal = gof.decimal)
    return(tr)
  }