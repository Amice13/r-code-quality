#' Download an adjacency graph from the survey hosting site
#'
#' @param city the city key
#' @param template the URL template to use
#'
#' @returns A list with two elements: `graph` with the adjacency list, and
#'   `blocks` with a matching character vector of block FIPS codes.
download_graph = function(city, template=paste0("https://cdn.jsdelivr.net/gh/",
                                                "CoryMcCartan/neighborhood-survey/",
                                                "docs/assets/{city}_graph.json")) {
    cli::cli_process_start("Downloading {city} graph")
    raw = read_json(str_glue(template), simplifyVector=TRUE)
    cli::cli_process_done()

    cli::cli_process_start("Processing adjacency graph")
    blocks = names(raw)
    match_tbl = fastmatch::fmatch.hash(raw[[1]], blocks)
    gr = list(graph = set_names(map(raw, fastmatch::fmatch, match_tbl), NULL),
              blocks = recover_fips(blocks, city))
    cli::cli_process_done()

    gr
}

#' Load city adjacency graphs
#'
#' @param path the path to the RDS file to cache/save
#' @param cities a vector of city keys to get
#' @param ... additional arguments to [download_graph]
#'
#' @returns A named list of cities, with each entry a list of two elements:
#'   `graph` with the adjacency list, and `blocks` with a matching character
#'   vector of block FIPS codes.
get_graphs = function(path = here("data/city_graphs.rds"),
                      cities = c("new-york", "miami", "phoenix"), ...) {
    if (file.exists(path)) {
        gr = read_rds(path)
        already = names(gr)
        new_cities = setdiff(cities, already)
        names(new_cities) = new_cities
        if (length(new_cities) > 0) {
            cli::cli_alert_info("{already} already downloaded.")
            cli::cli_text("Downloading graphs for {new_cities}")

            new_gr = map(new_cities, download_graph, ...)
            gr = c(gr, new_gr)
            write_rds(gr, path, compress="xz")
        }

        gr[cities]
    } else {
        names(cities) = cities

        cli::cli_text("Downloading graphs for {cities}")
        gr = map(cities, download_graph, ...)
        write_rds(gr, path, compress="xz")
        gr
    }
}
