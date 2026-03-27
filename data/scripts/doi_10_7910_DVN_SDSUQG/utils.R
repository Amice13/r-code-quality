
#' Prepend State FIPS codes to truncated codes
#'
#' Helper function to recover original FIPS codes, which have been truncated
#' to save space for the online survey.
#'
#' @param x vector of truncated codes
#' @param city matching vector of city groups: one of `new-york`, `miami`, `phoenix`
#'
#' @return The recovered codes
recover_fips = function(x, city) {
    case_when(
        city == "new-york" ~ if_else(str_sub(x, 1, 2) %in% c("03", "17", "31"),
                                     str_c("340", x),
                                     if_else(str_sub(x, 1, 2) == "19",
                                             str_c("361", x), str_c("360", x))),
        city == "miami" ~ str_c("120", x),
        city == "phoenix" ~ str_c("040", x)
    )
}


cloglog = function(x) 1 - exp(-exp(x))
midpt = uniroot(\(x) cloglog(x) - 0.5, c(-1, 0))$root
cloglog = posterior::rfun(cloglog)

make_coef_d <- function(coefs, probs = c(0.05, 0.25, 0.5, 0.75, 0.95)) {
    map_dfr(coefs, function(coef) {
        names(coef) = c(miami="Miami", `new-york`="NYC", phoenix="Phoenix")[names(coef)]
        map_dfr(coef, function(beta) {
            diff = 0.5 - cloglog(midpt + beta)
            t(posterior::quantile2(diff, probs=probs)) %>%
                as_tibble()
        }, .id="city")
    }, .id="coef") %>%
        mutate(coef = fct_rev(fct_inorder(coef)))
}



proc_coef_names = function(x) {
    y = x %>%
        str_remove_all("TRUE") %>%
        str_remove_all("Yes") %>%
        str_replace_all(":", " * ") %>%
        str_replace("partyind", "party = IND") %>%
        str_replace("partyrep", "party = REP") %>%
        str_replace("educ_grpno_coll", "educ = No College") %>%
        str_replace("educ", "education") %>%
        str_replace("pct_", "Fraction same ") %>%
        str_replace("is_", "") %>%
        str_replace("_bg", " block group") %>%
        str_replace("_tract", " tract") %>%
        str_replace("dist_", "distance to") %>%
        str_replace("_rr", " road region") %>%
        str_replace("children_home", "children") %>%
        str_replace("pop", "population") %>%
        str_replace("med_inc", "income") %>%
        str_replace(" own", " ownership") %>%
        str_replace_all("(log|sqrt)\\((\\w+).*\\)", "\\2") %>%
        str_to_sentence()
    names(y) = x
    y
}

# Coefficient tables
make_coef_table = function(m) {
    map_dfr(m, summary, .id="city") %>%
        select(coefficient=variable, city, mean, `std. dev.`=sd, q5, median, q95) %>%
        rename_with(str_to_title) %>%
        mutate(Coefficient = fct_inorder(proc_coef_names(Coefficient)),
               City = lbl_city(City)) %>%
        arrange(Coefficient, City)
}
fmt_coef_table = function(tbl, cap) {
    tbl %>%
        knitr::kable(format="latex", digits=2, longtable=TRUE, booktabs=TRUE,
                     linesep="", caption=cap) %>%
        kableExtra::kable_styling(font_size=10, latex_options=c("striped"),
                                  stripe_index=which(rep(rep(c(T, F), 100), each=3)))
}
