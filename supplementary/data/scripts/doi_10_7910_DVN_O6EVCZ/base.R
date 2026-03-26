#####################################################################################################
###### DEFINE FUNCTIONS
#####################################################################################################

### function to make calculating area a bit less annoying
get_area <- function(x, value = "km^2") {
        units::drop_units(units::set_units(sf::st_area(x), value, mode = "standard"))
}

### compute significance stars for a given p-value
p_stars <- function(p, star3 = 0.001, star2 = 0.01, star1 = 0.05, dot = NULL) {
        stringr::str_pad(dplyr::case_when(p < star3 ~ "***",
                                          p < star2 ~ "**",
                                          p < star1 ~ "*",
                                          p < ifelse(is.null(dot), -Inf, dot) ~ ".",
                                          TRUE ~ ""),
                         3, "right")
}

### compute Moran's I, Geary's C, and Lagrange multiplier spatial tests for OLS residuals
resid_spatial_tests <- function(mod_obj, lW, n_sim = 999, n_dig = 3) {
        if (
                sum(is.null(names(mod_obj))) > 0 | 
                (!("list" %in% class(mod_obj))) | 
                (sum(sapply(mod_obj, class) == "lm") != length(mod_obj))
        ) {
                if ("lm" %in% class(mod_obj)) {
                        mod_obj <- list("model" = mod_obj)
                } else{
                        stop("`mod_obj` must be an `lm` object or list of named `lm` objects") 
                }
        }
        workhorse <- function(mod, mod_name, lW, n_sim = n_sim) {
                mi <- spdep::moran.mc(mod$residuals, listw = lW, nsim = n_sim)
                g <- spdep::geary.mc(mod$residuals, listw = lW, nsim = n_sim)
                lg <- spdep::lm.LMtests(model = mod, listw = lW, zero.policy = TRUE, test = "all")
                out <- dplyr::tibble(stat = c(paste0(round(mi$statistic, n_dig), 
                                                     p_stars(mi$p.value)),
                                              paste0(round(g$statistic, n_dig), 
                                                     p_stars(g$p.value)),
                                              paste0(round(lg$LMerr$statistic, n_dig), 
                                                     p_stars(lg$LMerr$p.value)),
                                              paste0(round(lg$LMlag$statistic, n_dig), 
                                                     p_stars(lg$LMlag$p.value)),
                                              paste0(round(lg$RLMerr$statistic, n_dig), 
                                                     p_stars(lg$RLMerr$p.value)),
                                              paste0(round(lg$RLMlag$statistic, n_dig), 
                                                     p_stars(lg$RLMlag$p.value)),
                                              paste0(round(lg$SARMA$statistic, n_dig), 
                                                     p_stars(lg$SARMA$p.value))))
                names(out)[names(out) == "stat"] <- mod_name
                return(out)
        }
        left <- dplyr::tibble(test = c("Moran's I (permutation)", "Geary's C (permutation)",
                                       "LM error (simple)", "LM lag (simple)", 
                                       "LM error (robust)", "LM lag (robust)", "LM portmanteau"))
        right <- purrr::map2_dfc(mod_obj, names(mod_obj), 
                                 ~ workhorse(.x, .y, lW = lW, n_sim = n_sim))
        out <- dplyr::bind_cols(left, right)
        message("Note: *** p<0.001, ** p<0.01, * p<0.05.")
        return(as.matrix(out))
}

## split the combined bootstrap spatial weights into one matrix per iteration
split_W_boot <- function(n, nr, m) {
        r1 <- nr * (n - 1) + n # column names
        r2 <- r1 + 1 # first row of data
        r3 <- r1 + nr # last row of data
        cn <- as.character(m[r1, -(1:2)])
        rn <- as.character(m[r2:r3, 2])
        if (!identical(rn, cn)) {stop("Row/column names do not match.")}
        m2 <- m[r2:r3, -(1:2)]
        dimnames(m2) <- list(rn, cn)
        m2 <- apply(m2, 1:2, as.numeric)
        m2
}

### run SEMs
run_sem <- function(lhs, rhs, ...){
        f <- as.formula(paste(lhs, paste(rhs, collapse = " + "), sep = " ~ "))
        m <- eval(bquote(spatialreg::errorsarlm(.(f), ...)))
        return(m)
}

### run models on bootstrapped data
boot_sem <- function(boot_W, id_df, data_df, sim_df) {
        boot_df <- left_join(id_df, data_df, by = "geo")
        boot_mods <- map(scratch$y, ~ run_sem(., scratch$x, weights = boot_df$n_15o_est,
                                              data = boot_df, listw = mat2listw(boot_W), 
                                              Durbin = FALSE, zero.policy = TRUE)) %>%
                set_names(c("mar", "apr", "dif"))
        boot_df %>%
                dplyr::select(geo, city, n_15o_est) %>%
                left_join(sim_df, by = "city") %>%
                mutate(yhat_mar = suppressWarnings(predict(boot_mods$mar, ., pred.type = "trend")),
                       yhat_apr = suppressWarnings(predict(boot_mods$apr, ., pred.type = "trend")),
                       yhat_dif = suppressWarnings(predict(boot_mods$dif, ., pred.type = "trend")),
                       across(contains("yhat"), as.numeric)) %>%
                group_by(re_group, re_level, ses_within) %>%
                summarize(across(contains("yhat"), ~ weighted.mean(., n_15o_est)),
                          .groups = "keep") %>%
                ungroup()
}

## function to make comparison coefficient plots
cmpr_plot <- function(df, leg_stack) {
        ggplot(aes(y = yn), data = df) +
                geom_vline(xintercept = 0, linetype = "dotted") +
                geom_rect(aes(xmax = ci_hi, xmin = ci_lo, ymin = yn - 0.2, ymax = yn + 0.2,
                              fill = spec), alpha = 0.375) +
                geom_segment(aes(x = ci_lo, xend = ci_lo, y = yn - 0.45, yend = yn + 0.45, 
                                 color = spec),
                             size = 1) +
                geom_segment(aes(x = ci_hi, xend = ci_hi, y = yn - 0.45, yend = yn + 0.45, 
                                 color = spec),
                             size = 1) +
                geom_point(aes(x = beta, color = spec)) +
                facet_grid(cols = vars(y), scales = "free_x") +
                theme_stata(scheme = "s1color", base_size = 10) +
                theme(legend.position = "bottom", legend.direction = leg_stack,
                      axis.text.y = element_text(angle = 0),
                      strip.background = element_rect(fill = "transparent")) +
                scale_x_continuous(labels = scales::percent) +
                scale_y_continuous(breaks = -1:-9, 
                                   # labels = c("Percent age 65+",
                                   #            paste("Percent employed in health\n care or",
                                   #                  "social assistance"),
                                   #            "Percent under poverty line",
                                   #            paste0("Percent with Medicaid or\n other means-",
                                   #                   "tested\n public health insurance"),
                                   #            "Percent without health\n insurance",
                                   #            "Percent without internet\n access",
                                   #            "Percent Black",
                                   #            "Percent Hispanic",
                                   #            "Percent Asian")) +
                                   labels = c("% 65+",
                                              "% health care workers",
                                              "% under poverty line",
                                              "% w/Medicaid, etc.",
                                              "% w/o health insurance",
                                              "% w/o internet access",
                                              "% Black",
                                              "% Hispanic",
                                              "% Asian")) +
                scale_color_manual(values = c("#6699CC", "#CC6677", "#DDCC77")) +
                scale_fill_manual(values = c("#6699CC", "#CC6677", "#DDCC77")) +
                labs(y = NULL, 
                     x = paste("Estimated coefficient & confidence interval",
                               "(95% level; cluster-robust)"), 
                     fill = NULL, color = NULL)
}

#####################################################################################################
###### SET UP PATHS, ETC.
#####################################################################################################

### scratch pad 
scratch <- list(
        packages = c("tigris", "units", "sf", "spdep", "spatialreg",
                     "clubSandwich", "lmtest", "furrr", "ggthemes", "tidyverse"),
        cities = c("New York", "Chicago", "Houston", "Phoenix", 
                   "Philadelphia", "San Antonio", "San Diego", "Dallas"),
        airports = c("jfk", "ord", "hou", "phx", "phl", "sat", "san", "dal"),
        data = "analysis_data.csv",
        units = "units.geojson",
        W = "W.txt",
        W_boot = "W_boot.zip",
        modzcta = paste0("https://raw.githubusercontent.com/nychealth/coronavirus-data/",
                         "master/Geography-resources/ZCTA-to-MODZCTA.csv"),
        lmrk = c("C3026", "C3077", paste0("K21", 80:90), "K2362", 
                 paste0("K24", 24:60), "K2561", "K2564", "K2582", "K2586"),
        y = c("pr_15o_vac_pf_est_mar", "pr_15o_vac_pf_est_apr", "pr_15o_vac_pf_est_dif"),
        x = c("city", "pr_65o_est", "pr_hsa_est", 
              "pr_pov_est", "pr_hig_mtp_est", "pr_hig_ui_est", "pr_ni_est",
              "pr_b_est", "pr_l_est", "pr_a_est")
)

### install any missing packages that will be needed
scratch$uninstalled <- setdiff(scratch$packages, installed.packages())
if (length(scratch$uninstalled) > 0) {
        install.packages(scratch$uninstalled, 
                         dependencies = TRUE, repos = "https://cran.microsoft.com/")
}

### table of geographic identifiers
scratch$geoids <- tibble::tibble(
        city = factor(scratch$cities, levels = scratch$cities),
        city_abb = factor(scratch$airports, levels = scratch$airports),
        state = c("36", "17", "48", "04", "42", "48", "06", "48"),
        place = c("51000", "14000", "35000", "55000", "60000", "65000", "66000", "19000")
) 
scratch$states <- sort(unique(scratch$geoids$state))

### read data set 
df <- readr::read_csv(scratch$data,
                      col_types = readr::cols(city = readr::col_factor(levels = scratch$cities),
                                              state = readr::col_factor(),
                                              geo = readr::col_character()))

### read spatial weights matrix (W)
# see Equation e4.4 in supplement
W <- as.matrix(read.table(scratch$W, header = TRUE, sep = ","))