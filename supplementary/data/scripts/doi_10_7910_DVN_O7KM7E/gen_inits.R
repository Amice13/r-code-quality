## This is simply a helper function to generate starting values for the chains
gen_inits <- function(data, p, overrule = FALSE, m = 3) {
    y <- cbind(data$SCcites, data$ACcites, data$DCcites)
    seeds <- c(314, 2718)
    result <- lapply(1:2, function(x) {
        P <- try(solve(0.8 * var(y)), silent = TRUE)
        if ( inherits(P, "try-error") ) {
            stop("Analysis aborted as the DVs' covariance matrix was singular.")
        }
        means <- log(apply(y, 2, mean))
        this_init <- list(P = P, Intercepts = rnorm(m, mean = means, sd = 4))
        this_init[paste0("SC", 1, "_SC")] = 1
        this_init[paste0("SC", 1, "_AC")] = 1
        this_init[paste0("SC", 1, "_DC")] = 1
        this_init[paste0("AC", 1, "_SC")] = 0
        this_init[paste0("AC", 1, "_AC")] = 0
        this_init[paste0("AC", 1, "_DC")] = 0
        this_init[paste0("DC", 1, "_SC")] = 0
        this_init[paste0("DC", 1, "_AC")] = 0
        this_init[paste0("DC", 1, "_DC")] = 0
        if ( p > 1 ) {
            for ( i in 1:p ) {
                this_init[paste0("SC", i, "_SC")] = 0
                this_init[paste0("SC", i, "_AC")] = 0
                this_init[paste0("SC", i, "_DC")] = 0
                this_init[paste0("AC", i, "_SC")] = 0
                this_init[paste0("AC", i, "_AC")] = 0
                this_init[paste0("AC", i, "_DC")] = 0
                this_init[paste0("DC", i, "_SC")] = 0
                this_init[paste0("DC", i, "_AC")] = 0
                this_init[paste0("DC", i, "_DC")] = 0
            }
        }
        this_init["age_SC"] = 0
        this_init["age_AC"] = 0
        this_init["age_DC"] = 0
        this_init["distance_SC"] = 0
        this_init["distance_AC"] = 0
        this_init["distance_DC"] = 0
        if ( overrule ) {
            this_init["overruled_SC"] = 0
            this_init["overruled_AC"] = 0
            this_init["overruled_DC"] = 0
        }
        this_init[".RNG.seed"] = seeds[x]
        this_init[".RNG.name"] = "base::Wichmann-Hill"
        return(this_init)
    })
    return(result)
}
