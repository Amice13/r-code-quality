## SIMULATION TOOLS

# Dependencies
library("Matrix") # for triu (upper triangle of matrix)

# MAIN --v

# Simulate from a linear model using independent Gaussian unobservable
# and Gaussian regressors having a Toeplitz correlation
sim_data <- function(n, p, rho, a0, theta) {
    sigma <- cor_mat(p, rho)
    x <- matrix(rnorm(n * p), n, p) %*% chol(sigma)
    eps <- matrix(rnorm(n), n, 1)
    y <- a0 + x %*% theta + eps
    return(list(x = x, y = as.numeric(y)))
}

# HELPERS (nothing to do with linear model) --v
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