#' Run the BaP-VAR model on citation data
#'
#' @param data A dataframe that should be a subset of the replication data,
#'     giving only the data for one precedent (sorted by citing year)
#' @param lags An integer vector of length one giving the number of lags to use
#'     (the default is 1)
#' @param nchains An integer vector of length one giving the number of MCMC
#'     chains to generate (the default is 2)
#' @param seed An integer vector of length one giving the seed to use for
#'     reproducibility of results (the default is 8086)
#' @param adapt_iters An integer vector of length one giving the number of
#'     iterations to use in the adaptation phase (the default is 1000000,
#'     and a large number such as one million is encouraged)
#' @param sample_iters An integer vector of length one giving the number of
#'     iterations to use for sampling (the default is 250000,
#'     and a large number such as that is encouraged)
#'
#' @return A list of length two, with elements: "samples", containing an
#'     \code{\link[coda]{mcmc.list}} with the posterior samples;
#'     and "dic", containing a \code{\link[rjags:dic.samples]{dic}} object
#'     with the DIC samples
#'
#' @export
bapvar <- function(data, lags = 1, nchains = 2, seed = 8086,
                   adapt_iters = 1000000, sample_iters = 250000) {
    ## Subset the data to the variables we need
    vars <- c("SCcites", "ACcites", "DCcites", "age", "distance", "overruled")
    data <- data[ , vars]
    ## And record the number of observations and equations
    N <- nrow(data)
    m <- 3
    ## We need to figure out if this case was ever overruled
    overruled <- any(data$overruled > 0)
    ## And choose the right bug file accordingly
    if ( !overruled ) {
        data <- data[ , -ncol(data)]
        bug_file_suffix <- "-no-overrule.bug"
    } else {
        bug_file_suffix <- ".bug"
    }
    ## Then we generate the initial values
    inits <- gen_inits(data, lags, overruled)
    ## And prepare the data for the jags models
    data <- as.list(data)
    data["N"] <- N
    data["m"] <- m
    ## Set the seed
    set.seed(seed)
    ## Setup model and run adaptation/burn-in
    modfile <- paste0("lag", lags, bug_file_suffix)
    modfile <- system.file("jags", modfile, package = "bapvar")
    model <- jags.model(
        file = modfile,
        data = data,
        n.chains = nchains,
        inits = inits,
        n.adapt = adapt_iters
    )
    ## Monitor / draw posterior after burnin
    courts <- c("SC", "AC", "DC")
    exog <- c("age", "distance", "if"(overruled, "overruled", NULL))
    exog <- paste(rep(exog, each = length(courts)), courts, sep = "_")
    endog <- paste0(courts, rep(1:lags, each = length(courts)^2))
    endog <- paste(endog, rep(courts, each = length(courts)), sep = "_")
    monitor_list <- c("Intercepts", endog, exog, "Sigma", "C")
    samples <- coda.samples(model, monitor_list, sample_iters)
    # Compute the DIC
    dic <- dic.samples(model, n.iter = sample_iters, type = "pD")
    ## Return the results
    return(list(samples = samples, dic = dic))
}
