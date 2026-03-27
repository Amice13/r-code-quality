update.gamma <- function(Z, alpha, beta, prior, data) {
    ## Z - N x J matrix
    ## alpha - J X 1 vector
    ## beta - J x D matrix
    ## prior list of sigma.inv.gamma, sigma.inv.ab, mu.gamma, mu.ab
    ## data:
    ## * N - number of actors
    ## * J - number of questions
    ## * D - number of dimensions
    ## * Y - 1/-1/NA observations in N x J matrix

    prior.mean <- prior$sigma.inv.gamma %*% prior$mu.gamma

    new.gamma <- array(NA, dim = c(data$N, data$D))
    for (i in 1:data$N) {
        use.cols <- data$cols.obs[[i]]
        
        # post.prec <- solve(prior$sigma.inv.gamma + crossprod(beta[use.cols,]))

        beta_sub <- matrix(beta[use.cols,],
          nrow = length(use.cols))

        post.prec <- solve(prior$sigma.inv.gamma +
          crossprod(beta_sub))

        post.loc <- prior.mean + t(beta_sub) %*% (Z[i,use.cols] - alpha[use.cols])

        new.gamma[i,] <- post.prec %*% post.loc
    }

    return(new.gamma)
}

update.ab <- function(Z, gamma, prior, data) {
    ## Z - N x J matrix
    ## gamma - N x D matrix
    ## prior list of sigma.inv.gamma, sigma.inv.ab, mu.gamma, mu.ab
    ## data:
    ## * N - number of actors
    ## * J - number of questions
    ## * D - number of dimensions
    ## * Y - 1/-1/NA observations in N x J matrix

    gamma.design <- cbind(rep(1.0, data$N), gamma) # add intercept
    prior.mean <- prior$sigma.inv.ab %*% prior$mu.ab

    new.ab <- array(NA, dim = c(data$J, data$D + 1))
    for (j in 1:data$J) {
        use.rows <- data$rows.obs[[j]]
        post.prec <- solve(prior$sigma.inv.ab + crossprod(gamma.design[use.rows,]))
        post.loc <- prior.mean + t(gamma.design[use.rows,]) %*% Z[use.rows,j]
        new.ab[j,] <- post.prec %*% post.loc
    }

    return(new.ab)
}

update.Z <- function(alpha, beta, gamma, prior, data) {
    ## alpha - J X 1 vector
    ## beta - J x D matrix
    ## gamma - N x D matrix
    ## prior list of sigma.inv.gamma, sigma.inv.ab, mu.gamma, mu.ab
    ## data:
    ## * N - number of actors
    ## * J - number of questions
    ## * D - number of dimensions
    ## * Y - 1/-1/NA observations in N x J matrix

    M <- sweep(gamma %*% t(beta), 2, -alpha)
    M[M > 15] <- 15 # keep quantiles in range
    M[M < -15] <- -15 #
    add.to.M <- array(0, dim = c(data$N, data$J))

    for (i in 1:data$N) {
        for (j in 1:data$J) {
            if (is.na(data$Y[i,j])) {
            } else if (data$Y[i,j] == 1) {
                add.to.M[i,j] <- exp(dnorm(M[i,j], log = TRUE) - pnorm(M[i,j], log.p = TRUE))
            } else if (data$Y[i,j] == -1) {
                add.to.M[i,j] <- - exp(dnorm(M[i,j], log = TRUE) - pnorm(-M[i,j], log.p = TRUE))
            } else {
                message(paste0("ERROR: cell ", i, ",", j, " has non-(1,-1,NA) value ", data$Y[i,j]))
            }
        }
    }

    return(M + add.to.M)
}
