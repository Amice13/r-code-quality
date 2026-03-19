#' Create a model data frame for all respondents across all cities
#'
#' @param resps the clean response table ([get_clean_responses()])
#' @param block_d a tibble of all census blocks
#' @param gr a list of cities, with each entry containing two elements: the
#'   adjacency list, and the vector of FIPS codes that correspond to the
#'   adjacency list entries
#' @param min_progress minimum survey progress required to keep an observation
#' @param min_nbhd minimum neighborhood size (in census blocks) to keep
#'
#' @returns A tibble that can be used inside a modeling function
make_model_frame = function(resps, block_d, gr, min_progress=0, min_nbhd=2) {
    cities = names(gr)
    stopifnot(all.equal(sort(unique(block_d$city)), sort(cities)))

    lapply(cities, function(city_nm) {
        cli::cli_ul("Making model frame for {city_nm}")
        city_resps = filter(resps, city==city_nm, progress >= min_progress,
                            lengths(neighborhood) >= min_nbhd)
        city_d = filter(block_d, city==city_nm)
        calc_city_frames(city_resps, city_d, gr[[city_nm]])
    }) %>%
        do.call(bind_rows, .) %>%
        group_by(city, id)
}

#' Create a model data frame for all respondents in a city
#'
#' @param resps the clean response table ([get_clean_responses()]) for the city
#' @param city_d a tibble of census blocks for the city of the respondent
#' @param city_gr a list with two elements: the adjacency list, and the vector
#'   of FIPS codes that correspond to the adjacency list entries
#'
#' @returns A tibble that can be used inside a modeling function
calc_city_frames = function(resps, city_d, city_gr) {
    match_tbl = fastmatch::fmatch.hash(resps$neighborhood[[1]], city_gr$blocks)
    # check for same ordering
    # if (!all(unique(diff(fmatch(city_d$fips, match_tbl))) == 1))
        # cli::cli_abort("Different ordering between `city_d` FIPS and `city_gr$blocks`")

    nbhds = lapply(resps$neighborhood, function(x) fastmatch::fmatch(x, match_tbl))
    contig = sapply(nbhds, function(x) all(x %in% unlist(city_gr$graph[x]))) # quick contig check
    if (any(!contig)) {
        cli::cli_inform("{sum(!contig)} noncontiguous neigbhorhood{?s}.")
    }
    resps = resps[contig, ]
    nbhds = nbhds[contig]

    resps = select(resps, -why_map, -home_address, -neighborhood, -city)
    city_d$centroid = s2::as_s2_geography(city_d$centroid)

    cli::cli_process_start("Processing individual respondents")
    out = do.call(bind_rows, lapply(seq_len(nrow(resps)), function(i) {
        calc_indiv_frame(resps[i,], nbhds[[i]], city_d, city_gr)
    }))
    cli::cli_process_done()

    out
}

#' Add variables which are cumulative per neighborhood
#'
#' @param model_d the model data frame
#' @param sum_vars variables which should be provided to `cumsum()`
#' @param mean_vars variables which should be provided to `cummean()`
#' @param max_vars variables which should be provided to `cummax()`
#' @param prod_vars variables which should be provided to `cumprod()`
#'
#' @return The modified data frame.
add_cuml_vars = function(model_d, sum_vars=NULL, mean_vars=NULL,
                         max_vars=NULL, prod_vars=NULL) {
    model_d %>%
        group_by(id) %>%
        mutate(across({{ sum_vars }}, ~ lag(cumsum(.)), .names="cuml_{.col}"),
               across({{ mean_vars }}, ~ lag(cummean(.)), .names="avg_{.col}"),
               across({{ max_vars }}, ~ lag(cummax(.)), .names="max_{.col}"),
               across({{ prod_vars }}, ~ lag(cumprod(.)), .names="cumlpr_{.col}")) %>%
        group_by(city, id)
}
