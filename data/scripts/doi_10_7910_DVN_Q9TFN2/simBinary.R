## SIMULATION TOOLS

# Dependencies
library("Matrix") # for triu (upper triangle of matrix)

# MAIN --v

# Simulate from a binary response model using a link function to pin down
# the distribution of the unobservable and Gaussian regressors having a Toeplitz correlation.
sim_data <- function(n, p, rho, a0, theta, link) {
    switch(link,
        "logit" = {
            eps <- matrix(rlogis(n), n, 1)
        },
        "probit" = {
            eps <- matrix(rnorm(n), n, 1)
        },
        "cauchit" = {
            eps <- matrix(rcauchy(n), n, 1)
        },
        {
            fam <- binomial(link = link)
            qd_func <- fam$linkfun # link (i.e. quantile) fctn
            eps <- matrix(qd_func(runif(n)), n, 1)
        }
    )
    sigma <- cor_mat(p, rho)
    x <- matrix(rnorm(n * p), n, p) %*% chol(sigma)
    y <- matrix(a0 + x %*% theta >= eps, n, 1)
    return(list(x = x, y = as.numeric(y)))
}

# Calculate mu associated with a binary response model via simulation
sim_mu <- function(nsim = 100000, p, rho, a0, theta, link, intercept = TRUE) {
    data <- sim_data(nsim, p, rho, a0, theta, link)
    x <- data$x # regressors
    fam <- binomial(link = link) # family
    cd_func <- fam$linkinv # inverse link (CDF)
    pd_func <- fam$mu.eta # inv. link deriv. (PDF)
    lin <- as.numeric(a0 + x %*% theta) # linear form
    cdf_lin <- cd_func(lin) # CDF of linear form
    cdf1mincdf_lin <- cdf_lin * (1 - cdf_lin) # CDF * (1 - CDF) of linear form
    pdf_lin <- pd_func(lin) # PDF of linear form
    omega <- (pdf_lin^2) / cdf1mincdf_lin # E[m11(xtheta, y)|x]
    d <- x[, 1] # first regressor ("treatment")
    w <- as.matrix(x[, -1]) # remaining regressor ("controls")
    if (intercept) {
        res <- lm(d ~ w, weights = omega) # weighted regression w/ intercept
        intr <- res$coefficients[1]
        mu <- res$coefficients[-1]
    } else {
        res <- lm(d ~ w - 1, weights = omega) # weighted regression w/o intercept
        intr <- NA
        mu <- res$coefficients
    }
    return(list(intr = intr, mu = mu))
}

# HELPERS (nothing to do with binary response) --v
# Regressor correlation matrix, Sigma (Toeplitz structure)
cor_mat <- function(p, rho) {
    sigma_temp <- matrix(0, p, p)
    for (j in 1:(p - 1)) {
        for (k in (j + 1):p) {
            sigma_temp[j, k] <- rho^(abs(j - k))
        }
    }
    tri_up <- triu(sigma_temp, 1) # strict upper triangle (from Matrix)
    sigma <- diag(p) + tri_up + t(tri_up)
    return(sigma)
}