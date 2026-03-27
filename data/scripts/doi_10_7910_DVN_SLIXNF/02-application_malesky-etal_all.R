##
##

## load packages
require(tidyverse)
require(DIDdesign)
require(Formula)
require(patchwork)
require(furrr)
require(future)

## parallel
plan(multicore, workers = parallel::detectCores() - 1)

## load data
data(malesky2014)
malesky2014 <- drop_na(malesky2014, lnarea, lnpopden, city, rm2c7b)

## outcomes
index1 <- c("goodroadv", "transport", "pro3", "tapwater", "roadv")
index2 <- c("rm2c7d", "rm2c7e", "rm2c7g", "animal_s", "agrvisit", "plant_s", "agrext", "irrigation")
index3 <- c("rm2c7c", "pro5")
index4 <- c("pro4", "rm2c7b", "useschool", "kgarten", "v_prischool")
index5 <- c("broadcast", "post", "vpost")
index6 <- c("rm2c7a", "rm2c7f", "market", "nonfarm", "vmarket1", "vmarket2", "vmarket3")
outcome_list <- c(index1, index2, index3, index4, index5, index6)


fm <- map(outcome_list,
  ~ as.Formula(paste(.x, "~ treatment + post_treat | lnarea + lnpopden + city + as.factor(reg8)")))

## -------------------------------------
## assess the assumption
## -------------------------------------
cat("Assessing the parallel trends assumption ... \n")
set.seed(1234)
check_out <- future_map(fm, ~did_check(
  formula  = .x,
  data     = malesky2014,
  id_time  = "year",
  is_panel = FALSE,
  option   = list(n_boot = 2000, parallel = TRUE, id_cluster = "id_district", lag = 1)),
  .options = furrr_options(seed = TRUE)
)


names(check_out) <- outcome_list
check_summary <- map_dfr(check_out, ~.x$estimate, .id = "var") %>%
  mutate(statistic = estimate / std.error,
         p_value = 2 * pnorm(abs(statistic), lower.tail = FALSE))

## save output
saveRDS(check_summary, file = "../results/application/03-check_all_summary.rds")


## save plot
map2(check_out, outcome_list, ~ggsave(filename = paste0("../results/figures/assessment/", .y, ".pdf"), plot(.x)))


## --------------------------------------------
## Estimate ATT
## --------------------------------------------
cat("Estimating ATT ... \n")
set.seed(1234)
est_out <- future_map(fm, ~did(
  formula  = .x,
  data     = malesky2014,
  id_time  = "year",
  is_panel = FALSE,
  option   = list(n_boot = 2000, parallel = TRUE, id_cluster = "id_district", lead = 0)),
  .options = furrr_options(seed = TRUE)
)


names(est_out) <- outcome_list
est_summary <- map_dfr(est_out, ~as_tibble(summary(.x)), .id = "var")

## --------------------------------------------
## Save output
## --------------------------------------------

## save output
saveRDS(est_summary, "../results/application/04-est_all_output.rds")

## save plot
plot_all <- function(obj) {
  tmp <- as_tibble(summary(obj)) %>%
    mutate(CI95_LB = estimate - qnorm(0.95) * std.error,
           CI95_UB = estimate + qnorm(0.95) * std.error)
  ggplot(tmp, aes(x = estimator, y = estimate)) +
    geom_point() +
    geom_pointrange(aes(ymin = CI95_LB, ymax = CI95_UB)) +
    geom_hline(yintercept = 0, color = 'red', linetype = 'dashed')
}

map2(est_out, outcome_list, ~ggsave(filename = paste0("../results/figures/estimate/", .y, ".pdf"), plot_all(.x)))


## joint
map(1:length(est_out), function(i) {
  p1 <- plot(check_out[[i]])
  p2 <- plot_all(est_out[[i]])
  ggsave(filename = paste0("../results/figures/combined/", outcome_list[i], ".pdf"), p1 + p2,
        height = 4, width = 10)
})
