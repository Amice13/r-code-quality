
#' Calculate neighborhood demographics
#'
#' @param nbhds a list of vectors, either character vectors of FIPS codes, or
#'   integer vectors of indices in `block_d`.
#' @param block_d the block data frame
#'
#' @returns A tibble with a row for each neighborhood in `nbhds`, containing
#'   summary demographic information on each neighborhood.
nbhd_demg = function(nbhds, block_d, comp_nbhd=NULL) {
    stopifnot(is.list(nbhds))
    do.call(bind_rows, lapply(nbhds, function(nbhd) {
        if (is.character(nbhd)) {
            nbhd_d = ungroup(block_d[match(nbhd, block_d$fips), ])
        } else {
            nbhd_d = ungroup(block_d[nbhd, ])
            nbhd = nbhd_d$fips
        }

        dists = sf::st_distance(nbhd_d$centroid[1], nbhd_d$centroid)
        out = dplyr::summarize(nbhd_d,
                        city = city[1],
                        pct_dem = weighted.mean(pct_dem, registrants),
                        blocks = n(),
                        sd_white = sd(pop_white / pop, na.rm=TRUE),
                        sd_dem = sd(democrats / registrants, na.rm=TRUE),
                        sd_coll = sd(pct_college, na.rm=TRUE),
                        sd_homeown = sd(pct_homeown, na.rm=TRUE),
                        herf_race = weighted.mean((pop_white^2 + pop_black^2 + pop_hisp^2 +
                                                       (pop - pop_white - pop_black - pop_hisp)^2) /
                                                      pop^2, pop, na.rm=TRUE),
                        herf_party = weighted.mean((democrats^2 + republicans^2) / registrants^2,
                                                   registrants, na.rm=TRUE),
                        herf_educ = weighted.mean(pct_college^2 + (1-pct_college)^2, pop, na.rm=T),
                        herf_own = weighted.mean(pct_homeown^2 + (1-pct_homeown)^2, pop, na.rm=T),
                        across(c(pct_college, pct_homeown), ~ weighted.mean(., pop)),
                        across(c(pop:pop_hisp, registrants:republicans, area), sum)) %>%
            mutate(radius = as.numeric(max(dists)) + sqrt(area / blocks) / pi,
                   reock = pmin(area / (pi * radius^2), 1.0))

        if (!is.null(comp_nbhd)) {
            out$pct_in = length(intersect(nbhd, comp_nbhd)) / length(nbhd)
        }

        out
    }))
}

