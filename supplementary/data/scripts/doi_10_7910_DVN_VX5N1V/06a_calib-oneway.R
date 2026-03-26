library(tidyverse)
library(tictoc)
library(ccesMRPrun)
library(vroom)
library(fs)
library(glue)

gc()

# pivot longer

# tidy, then select to only columns we need ----
## also re-adjust all cells to add up to voteshare in this case
tic() # 20 min, with calibration
for (yr in c(2016, 2020)) {
  dir_create(glue("data/output/fit-chunk-tidy/{yr}"))

  for (f in dir_ls(glue("data/output/fit-chunk/{yr}"), type = "file")) {
    load(f)
    cat("Starting ", f, "\n")
    n_draws <- nrow(p_draws_chunk)
    out_f <- pivot_celldraws_longer(
      mod_draws = p_draws_chunk[seq(1, n_draws - 1, by = 2), ], # thin by 2
      data_strat = p_cells_chunk,
      yhat_name = "pred_n_yes")

    # from inside ccesMRPrun::poststrat_draws
    correct_add <- out_f |>
      group_by(cd, iter) |>
      summarize(
        delta = calib_oneway(tgt = unique(pct_trump),
                             ests = pred_n_yes / n_response,
                             n = n_response),
        .groups = "drop"
      )
    out_f <- out_f |>
      left_join(correct_add, by = c("cd", "iter")) |>
      mutate(
        pred_nofix = pred_n_yes,
        pred_n_yes = n_response * ccesMRPrun:::invlogit(delta + logit_ghitza(pred_n_yes / n_response))) |>
      group_by(cd, iter)

    out_f <- select(out_f, cd, race, iter, N = n_response, pred_ggfix = pred_n_yes, pred_nofix) |>
      mutate(race = as.character(race))

    rm(p_draws_chunk, p_cells_chunk)
    vroom_write(
      out_f,
      delim = ",",
      path("data/output/fit-chunk-tidy",
           glue("{yr}"),
           str_replace(path_file(f), '\\.rda', '.csv.gz'))
    )
  }
  toc()
}





