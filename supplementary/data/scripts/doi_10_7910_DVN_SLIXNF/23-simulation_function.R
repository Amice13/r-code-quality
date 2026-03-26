##
## Simulation Study: double DID function
##

double_did_panel_long <- function(dids, id, time_length, treatment_var = "D") {

    Xlist <- ylist <- xvars <- est <- list()
    for (j in 1:length(dids)) {
      Xlist[[j]] <- model.matrix(dids[[j]])
      xvars[[j]] <- colnames(Xlist[[j]])
      ylist[[j]] <- pmodel.response(dids[[j]])
      est[[j]]   <- dids[[j]]$coef[xvars[[j]] == treatment_var]
    }

    XX <- as.matrix(Matrix::bdiag(Xlist))
    YY <- do.call("c", ylist)
    SUR <- lm(YY ~ XX - 1)

    ## col_id
    col_id <- list(); cum_len <- 0
    for (j in 1:length(dids)) {
      col_id[[j]] <- which(xvars[[j]] == treatment_var) + cum_len
      cum_len <- cum_len + length(xvars[[j]])
    }


    ## compute variance
    resid_sur <- SUR$residuals
    n  <- length(unique(id))
    TT <- 2; time_length
    mt <- list()
    for (i in 1:n) {
      use_id <- which(id == i)
      xtmp <- crossprod(XX[use_id, ], resid_sur[use_id])
      mt[[i]] <- tcrossprod(xtmp)
    }

    ## sand-part
    df2 <- n / (n - 1)
    meat  <- Reduce('+', mt)  * df2
    bread <- t(XX) %*% XX
    V <- solve(bread, meat) %*% solve(bread)

    ## weighted average
    est_vec <- unlist(est)
    W       <- solve(V)
    ddid_pt <- sum(solve(V, est_vec)) / sum(W)
    ddid_pt_var <- 1 / sum(W)

    x1 <- length(xvars[[1]])
    e1 <- rep(-1, x1)
    Vc <- V[e1, e1]; Wc <- solve(Vc)
    ddid_ptt <- sum(solve(Vc, est_vec[e1])) / sum(Wc)
    ddid_ptt_var <- 1 / sum(Wc)

    out <- list("PT" = list("ATT" = ddid_pt, "Var" = ddid_pt_var),
                "PTT" = list("ATT" = ddid_ptt, "Var" = ddid_ptt_var))
    return(out)
}