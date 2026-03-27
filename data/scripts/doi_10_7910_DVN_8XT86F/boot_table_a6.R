boot_table_a6 <- function(x, db, niter) {
    res <- list()
    for (i in 1:niter) {
        d <- econet:::sample_data(x$second_step, hypothesis = "par", group = db$id_2)
        specification <- d[[1]]
        dd <- d[[2]]
        sv <- econet:::replace_start_value(x$second_step, coef(x$second_step))
        dd$w <- db$weights
        tmp <- minpack.lm::nlsLM(formula = specification, start = sv, data = dd,
                     trace = T, weights =  w, control = nls.lm.control(ftol = sqrt(.Machine$double.eps)/1000,
                                                                         ptol = sqrt(.Machine$double.eps)/1000, gtol = 0,
                                                                         diag = list(), epsfcn = 0, factor = 100, maxfev = integer(),
                                                                         maxiter = 300, nprint = 0))
        res[[i]] <- coef(tmp)
    }
    
    res <- Reduce("cbind", res)
    res <- apply(res, 1, sd)
    res <- data.frame(coefficient = coefficients(x$second_step),
                      boot.Std.Error = res)
    res[, "boot.t.value"] <- res[, "coefficient"]/res[, "boot.Std.Error"]
    res[, "boot.p.value"] <- 2 * pt(-abs(res[, "boot.t.value"]),
                                    df = nobs(x$second_step) - nrow(res))
    return(res)
}

