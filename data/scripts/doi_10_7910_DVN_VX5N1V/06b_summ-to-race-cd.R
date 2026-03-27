library(tidyverse)
library(tictoc)
library(data.table)
library(vroom)
library(fs)
library(glue)


iter_grp_vars <- c("cd", "race", "iter")

# about 5 min, 2021-07-18
for (yr in c(2016, 2020)) {
  dir_create(glue("data/output/fit-strata/cd-level/{yr}"))
  dir_create(glue("data/output/fit-strata/state-level/{yr}"))

  tic()
  for (s in state.abb) {

    chunk_s <- glue("data/output/fit-chunk-tidy/{yr}/chunk-{s}.csv.gz")
    # if (!file_exists(chunk_s)) next

    out_s <- vroom(chunk_s, col_types = "cciidd", delim = ",")
    strat_est <- data.table(out_s)[,
                                   list(p_mrp_ggfix = sum(pred_ggfix)/sum(N),
                                        p_mrp_nofix = sum(pred_nofix)/sum(N),
                                        N = sum(N)),
                                   by = iter_grp_vars]
    st_est <- data.table(out_s)[,
                                list(p_mrp_ggfix = sum(pred_ggfix)/sum(N),
                                     p_mrp_nofix = sum(pred_nofix)/sum(N),
                                     N = sum(N)),
                                by = c("race", "iter")]
    rm(out_s)

    fwrite(strat_est, glue("data/output/fit-strata/cd-level/{yr}/strata-{s}.csv.gz"))
    fwrite(st_est, glue("data/output/fit-strata/state-level/{yr}/strata-{s}.csv.gz"))
  }
  toc()
}
