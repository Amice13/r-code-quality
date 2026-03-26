##########################################
# Adjusted KDE bandwidth helper functions
# Taylor Grimm
# September 21st, 2023
##########################################

# formula for the silverman bandwidth, replacing the regular sample size n with n_eff
silverman_bw <- function(x, n_eff = NULL) {
  if(is.null(n_eff)) n_eff <- length(x)
  0.9*(n_eff^(-1/5))*min(sd(x), (quantile(x, .75) - quantile(x, .25))/1.34)
}

# formula for the scott bandwidth, replacing the regular sample size n with n_eff
scott_bw <- function(x, n_eff = NULL) {
  if(is.null(n_eff)) n_eff <- length(x)
  1.06*(n_eff^(-1/5))*min(sd(x), (quantile(x, .75) - quantile(x, .25))/1.34)
}

# compute the effective sample size for an AR(1) process using the estimated
# phi value and the length of the series (n)
effective_size <- function(n, phi) {
  x <- 1:n
  y <- 1:n
  xy <- expand.grid(x=x,y=y)
  COV <- matrix(phi^abs(xy[, 1] - xy[, 2]) / (1 - phi^2), nrow = n, ncol = n)
  in.brackets <- 1 + (sum(COV) - sum(diag(COV))) / n
  floor(n/(in.brackets))
}

# compute the effective sample size for an ARMA(1, 1) process using the estimated
# phi and theta values and the length of the series (n)
effective_size_arma <- function(n, phi, theta) {
  x <- 1:n
  y <- 1:n
  xy <- expand.grid(x=x,y=y)
  COV <- matrix((((1 + theta*phi)*(phi + theta)) / (1 - phi^2))*phi^abs(xy[, 1] - xy[, 2]), nrow = n, ncol = n)
  in.brackets <- 1 + (sum(COV) - sum(diag(COV))) / n
  floor(n/(in.brackets))
}