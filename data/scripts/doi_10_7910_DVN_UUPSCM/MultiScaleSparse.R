# Code from Marble, William, and Matthew Tyler. 2022. “The Structure of Political Choices: Distinguishing Between Constraint and Multidimensionality.” Political Analysis 30 (3): 328–45. 
source(here("functions", "MultiScaleSparseFunctions.R"))

MultiScaleSparse <- function(prior, data, init, max.iter = 250, tol = 1e-4, verbose = TRUE) {
    ## prior list of sigma.inv.gamma, sigma.inv.ab, mu.gamma, mu.ab
    ## data:
    ## * N - number of actors
    ## * J - number of questions
    ## * D - number of dimensions
    ## * Y - 1/-1/NA observations in N x J matrix
    ## init:
    ## * alpha - J x 1 matrix
    ## * beta - J x D matrix
    ## * gamma - N x D matrix
    ## tol - convergence criteria is cor(old, new) > 1 - tol

    if (verbose) {
        message(">>>WARNING:<<<")
        message("The parameters of this model are NOT locally identified. Importantly, estimated beta and gamma will not be necessarily be the same when this algorithm is re-used. For more information, see Rivers, Douglas. 2003. Identification of Multidimensional Spatial Voting Models.")
    }


    data$rows.obs <- list()
    data$cols.obs <- list()

    for (i in 1:data$N) {
        data$cols.obs[[i]] <- which(!is.na(data$Y[i,]))
    }
    for (j in 1:data$J) {
        data$rows.obs[[j]] <- which(!is.na(data$Y[,j]))
    }

    alpha <- init$alpha
    beta <- init$beta
    gamma <- init$gamma
    old.params <- c(alpha, beta, gamma)

    for (iter in 1:max.iter) {
        Z <- update.Z(alpha, beta, gamma, prior, data)
        gamma <- update.gamma(Z, alpha, beta, prior, data)
        ab <- update.ab(Z, gamma, prior, data)
        alpha <- c(ab[,1])
        beta <- as.matrix(ab[,-1])

        new.params <- c(alpha, beta, gamma)
        param.cor <- cor(new.params, old.params)

        if (param.cor > 1 - tol) {
            message(paste("Converged after", iter, "iterations!"))
            break
        } else if (iter == max.iter) {
            message(paste("Maximum number of iterations reached at", param.cor, "correlation"))
        }

        old.params <- new.params
        if (verbose) message(paste("Iteration:", iter))
    }


    return(list(iter = iter,
                alpha = alpha,
                beta = beta,
                gamma = gamma))
}
