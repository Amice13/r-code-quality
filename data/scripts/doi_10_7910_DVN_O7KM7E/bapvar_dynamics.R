## XXX: The data, outcomes, covariates setup is a first step in generalizing
##      this codebase, but more work would have to be done (see how the
##      parameter names are hardcoded later in the function)

#' bapvar_dynamics
#'
#' Compute Impulse Response Function and Forecast Error Variance Decomposition
#' from Posterior BaP-VAR Draws
#'
#' @param samples A numeric matrix or mcmc object containing posterior draws
#' @param data A dataframe with the endogenous & exogenous variable observations
#' @param outcomes A character vector of column names indicating which columns
#'     in \code{data} contain the endogenous variable observations
#' @param covariates A character vector of column names indicating which
#'     columns in \code{data} contain the exogenous variable observations
#' @param p An integer vector of length one giving the number of lags
#' @param h An integer vector of length one giving the number of periods to
#'   compute impulse responses for; note in the first period this is the
#'   initial shock, then impulse responses for h-1 additional steps are computed
#' @param shock A character vector of length one giving the type of shock;
#'   should be one of "sd" (the default -- a one standard deviation shock)
#'   or "unit" (a one unit shock)
#' @param scale_by_mean A logical vector of length 1; if TRUE (the default),
#'   the shocks are scaled by the equation means
#' @param ordering An integer vector of length m giving the order to draw
#'   the equations' DFEV plots; if NULL (the default), the ordering is 1:m
#' @param verbose A logical vector of length one; if TRUE, a progress update
#'   is given every \code{check_nth} iteration (default is FALSE)
#' @param check_nth An integer vector of length one that determines how many
#'   iterations pass between user interrupt checks (and if verbose, how often
#'   progress updates are given)
#'
#' @return A list of length two, with class "bapvar_dynamics", containing:
#'     \describe{
#'         \item{irf}{
#'             An array of dimensions \code{c(nrow(samples), ncol(y)^2, h)}
#'             giving the IRF at each of the h steps at each of the posterior
#'             draws with class "bapvarIRF".
#'         }
#'         \item{fevd}{
#'             An array of dimensions \code{c(nrow(samples), ncol(y)^2, h)}
#'             giving the FEVD at each of the h steps at each of the posterior
#'             draws with class "bapvarFEVD".
#'         }
#'     }
#' @export
bapvar_dynamics <- function(samples, data,
                            outcomes = c("SCcites", "ACcites", "DCcites"),
                            covariates = c("age", "distance", "overruled"),
                            p, h, shock = "sd",
                            scale_by_mean = TRUE, ordering = NULL,
                            verbose = FALSE, check_nth = 1000) {
    ## Setup matrix of endogenous variables
    y <- as.matrix(data[ , outcomes])
    ## Setup matrix of exogenous variables
    x <- as.matrix(cbind(1, data[ , covariates]))
    ## Ensure variation in all exogenous variables
    if ( ncol(x) > 1 ) {
        bad_columns <- numeric()
        for ( j in 2:ncol(x) ) {
            if ( length(unique(x[ , j])) == 1 ) {
                bad_columns <- c(bad_columns, j)
            }
        }
        if ( length(bad_columns) > 0 ) {
            x <- x[ , -j]
        }
    }
    ## Permute as appropriate
    m <- ncol(y)
    K <- ncol(x)
    if ( is.null(ordering) ) {
        ordering <- 1:m
    }
    y <- y[ , ordering]
    vnames <- colnames(samples)
    courts <- c("SC", "AC", "DC")
    lag_names   <- paste0(courts, rep(1:p, each = m))
    A_names     <- c(outer(lag_names, courts, paste, sep = "_"))
    A_order     <- rep(1:(m*p), m) + rep(ordering-1, each = m*p) * m*p
    beta_names  <- outer(covariates, courts, paste, sep = "_")
    beta_names  <- c(rbind(paste0("Intercepts[", 1:m, "]"), beta_names))
    beta_order  <- rep(1:K, m) + rep(ordering-1, each = K) * K
    C_names     <- vnames[grepl("C\\[", vnames)]
    Sigma_names <- vnames[grepl("Sigma\\[", vnames)]
    mat_order   <- rep(1:m, m) + rep(ordering-1, each = m) * m
    mat_order   <- c(matrix(1:m^2, ncol = m)[ordering, ordering])
    column_ordering <- c(
        A_names[A_order],       ## lag coefficient draws
        C_names[mat_order],     ## contemp. cor. draws (unneeded)
        Sigma_names[mat_order], ## b variance draws
        beta_names[beta_order]  ## covariate coefficient draws
    )
    samples <- samples[ , column_ordering]
    ## Compute dynamics
    if ( shock == "sd" ) {
        f <- .dynamics
    } else if ( shock == "unit" ) {
        f <- .unit_dynamics
    } else {
        stop("Specify shock as either \"unit\" or \"sd\"")
    }
    result <- f(samples, y, x, p, h, verbose, check_nth, scale_by_mean)
    ## Return classed result
    class(result) <- "bapvar_dynamics"
    class(result[[1]]) <- "bapvarIRF"
    class(result[[2]]) <- "bapvarFEVD"
    attr(result, "ordering") <- ordering
    attr(result[[1]], "ordering") <- ordering
    attr(result[[2]], "ordering") <- ordering
    return(result)
}
