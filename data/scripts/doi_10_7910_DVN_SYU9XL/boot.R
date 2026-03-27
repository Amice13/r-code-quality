set.seed(34281505)

rm(b17e19, 
   b17e19_long, 
   bivariate_scatter, 
   bivariate_scatter_data, 
   density_pre_invalid_share_national, 
   density_pre_invalid_share_ep,
   density_pre_invalid_share,
   density_pre_turnout_national, 
   density_pre_turnout_ep, 
   density_pre_turnout,
   miesbach19)


# define the number of boots
boot_times <- 5000

## bootstraps

tic()
bootstraps <-
  b17e19_data_models %>%
  # sample 5000 times over each subset of the data
  mutate(boots = map2(
    .x = data,
    .y = modeltype,
    .f = ~ bootstraps(.x,
                      strata = state,
                      times = boot_times,
                      apparent = T
    )
  )) %>%
  # treatment variable
  mutate(boots = map2(
    boots,
    election_type, 
    ~ mutate(
      .x,
      model = map2(
        .x = .x$splits, 
        .y = .y, 
        .f = ~ fit_model(
          data = analysis(.x), 
          election_type = .y
        )), 
      coefs_model = map(model, tidy),
      stats_model = map(model, glance), 
      vcov_model = map(model, vcov))
  )) %>%
  # interaction model
  mutate(boots = map2(
    boots,
    election_type, 
    ~ mutate(
      .x,
      model_interact = map2(
        .x = .x$splits, 
        .y = .y, 
        .f = ~ fit_model_interact(
          data = analysis(.x), 
          election_type = .y
        )),
      coefs_model_interact = map(model_interact, tidy),
      stats_model_interact = map(model_interact, glance), 
      vcov_model_interact = map2(
        .x = model_interact, 
        .y = .y, 
        .f = ~ vcov(.x))
    )
  )) 



# calculate the CIs
bootstraps <- bootstraps %>%
  # treatment variable
  mutate(
    boot_percentiles =
      map(
        boots,
        ~ summarise(
          .x,
          int_pctl(
            .data = .x,
            statistics = coefs_model
          )
        )
      )
  ) %>% 
  mutate(
    boot_percentiles =
      map(
        boot_percentiles,
        ~ rename(.x,
                 conf.low = .lower,
                 conf.high = .upper
        )
      )
  ) %>% 
  # interaction model
  mutate(
    boot_percentiles_interact =
      map(
        boots,
        ~ summarise(
          .x,
          int_pctl(
            .data = .x,
            statistics = coefs_model_interact
          )
        )
      )
  ) %>%
  mutate(
    boot_percentiles_interact =
      map(
        boot_percentiles_interact,
        ~ rename(.x,
                 conf.low = .lower,
                 conf.high = .upper
        )
      )
  ) 


# treatment CIs
booted_cis <- bootstraps %>%
  select(-data, -boots) %>%
  unnest(boot_percentiles) %>%
  select(modeltype, term, conf.low, conf.high) %>%
  nest(term, conf.low, conf.high) %>%
  # create named variables "conf.low", "conf.high"
  mutate(
    conf.low = map(
      data,
      ~ deframe(.x %>%
                  select(term, conf.low))
    ),
    conf.high = map(
      data,
      ~ deframe(.x %>%
                  select(term, conf.high))
    ),
    ci.int = map(
      data,
      ~ tibble(
        paste0(.x %>% select(term) %>% pull()),
        paste0(
          "[",
          sprintf("%.2f", .x %>% pull(conf.low)),
          "; ",
          sprintf("%.2f", .x %>% pull(conf.high)),
          "]"
        )
      ) %>% deframe()
    )
  )

## save the estimated bootstrap cis
write_csv(booted_cis %>%
            select(modeltype, data) %>%
            unnest(cols = c(data)),
          path = "data/booted_cis.csv"
)



# Interaction CI
booted_cis_interact <- bootstraps %>%
  select(-data, -boots) %>%
  unnest(boot_percentiles_interact) %>%
  select(modeltype, term, conf.low, conf.high) %>%
  nest(term, conf.low, conf.high) %>%
  arrange(modeltype) %>%
  # create named variables "conf.low", "conf.high"
  mutate(
    conf.low = map(
      data,
      ~ deframe(.x %>%
                  select(term, conf.low))
    ),
    conf.high = map(
      data,
      ~ deframe(.x %>%
                  select(term, conf.high))
    ),
    ci.int = map(
      data,
      ~ tibble(
        paste0(.x %>% select(term) %>% pull()),
        paste0(
          "[",
          sprintf("%.2f", .x %>% pull(conf.low)),
          "; ",
          sprintf("%.2f", .x %>% pull(conf.high)),
          "]"
        )
      ) %>% deframe()
    )
  )


## save the estimated bootstrap cis
write_csv(booted_cis_interact %>%
            select(modeltype, data) %>%
            unnest(cols = c(data)),
          path = "data/booted_cis_interact.csv"
)



toc()