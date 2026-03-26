#
#
# About: R Script with helper funcitons for fitting and evaluating WBIT Models
#
#

view_table <-
  function(data, rows = 10) {
    data %>%
      reactable::reactable(.,
        fullWidth = TRUE,
        resizable = TRUE,
        filterable = TRUE,
        highlight = TRUE,
        defaultPageSize = rows,
        wrap = FALSE,
        showSortIcon = TRUE,
        striped = TRUE,
        compact = TRUE
      )
  }

keep_best_workflows <-
  function(result_list, workflow_id, top_performers) {
    result_type <-
      top_performers %>%
      filter(wflow_id == {{ workflow_id }}) %>%
      pull(.config)

    best_results <-
      if (str_detect(string = result_type, pattern = "Preprocessor")) {
        result_list[[{{ workflow_id }}]]
      } else {
        best_iteration <-
          top_performers %>%
          filter(wflow_id == {{ workflow_id }}) %>%
          mutate(
            iter = str_replace(string = .config, pattern = "Iter", replacement = ""),
            iter = as.numeric(iter)
          ) %>%
          pull(iter)

        best_results <-
          result_list[[{{ workflow_id }}]] %>%
          filter(.iter == best_iteration)

        best_results <-
          tibble::new_tibble(best_results, class = "tune_results")
      }

    best_results
  }

get_best_model_metrics <-
  function(models) {
    best_model <-
      models %>%
      collect_metrics() %>%
      filter(.metric == "accuracy") %>%
      mutate(
        model_type = case_when(
          str_detect(wflow_id, "log_reg") ~ "Logistic Reg",
          str_detect(wflow_id, "mars") ~ "MARS",
          str_detect(wflow_id, "xgb") ~ "XGBoost",
          str_detect(wflow_id, "rules") ~ "C5.0 Rules"
        )
      ) %>%
      filter(mean == max(mean))

    models %>%
      collect_metrics() %>%
      inner_join(., best_model %>%
        select(wflow_id, model_type), "wflow_id") %>%
      select(-std_err) %>%
      pivot_wider(names_from = ".metric", values_from = "mean") %>%
      mutate(across(where(is.numeric), ~ round(.x, 4)))
  }

get_model_full_comparison_table <-
  function(models) {
    model_compare <-
      models %>%
      collect_metrics() %>%
      mutate(
        preproc = case_when(
          str_detect(wflow_id, "impute") ~ "Dummy Variable Creation & Mean Imputation",
          str_detect(wflow_id, "normalize") ~ "Dummy Variable Creation, Mean Imputation & Normalization",
          str_detect(wflow_id, "plain") ~ "Dummy Variable Creation"
        ),
        result_components_used = ifelse(str_detect(wflow_id, "no_diff_components"), "No Diff Components Only", "Diff and No Diff Components")
      ) %>%
      mutate(
        model_type = case_when(
          str_detect(wflow_id, "log_reg") ~ "Logistic Reg",
          str_detect(wflow_id, "mars") ~ "MARS",
          str_detect(wflow_id, "xgb") ~ "XGBoost",
          str_detect(wflow_id, "rules") ~ "C5.0 Rules"
        )
      ) %>%
      select(-c(wflow_id, model, std_err, .iter, .config, .estimator, n)) %>%
      select(model_type, result_components_used, preproc, everything()) %>%
      pivot_wider(names_from = .metric, values_from = mean) %>%
      arrange(model_type, result_components_used, preproc) %>%
      rename(
        Model = model_type,
        `Pre-Processing Steps` = preproc,
        `Result Components Used` = result_components_used
      )

    reactable::reactable(model_compare,
      fullWidth = TRUE,
      resizable = TRUE,
      filterable = TRUE,
      highlight = TRUE,
      defaultPageSize = 50,
      wrap = FALSE,
      showSortIcon = TRUE,
      striped = TRUE,
      compact = TRUE,
      defaultColDef = colDef(
        cell = data_bars(model_compare,
          background = "lightgrey",
          text_position = "inside-end",
          force_outside = c(0, 0.4)
        )
      )
    )
  }

get_roc_curves <-
  function(models, type) {
    roc_vals <-
      models %>%
      collect_metrics() %>%
      filter(.metric == "roc_auc")

    roc_data <-
      if (type == "cbc-with-diff") {
        roc_data <- models %>%
          unnest(result) %>%
          unnest(.predictions) %>%
          left_join(., roc_vals %>%
            select(wflow_id, mean)) %>%
          mutate(
            preproc = case_when(
              str_detect(wflow_id, "impute") ~ "mean impute",
              str_detect(wflow_id, "normalize") ~ "mean impute & normalize",
              str_detect(wflow_id, "plain") ~ "dummy variable creation"
            ),
            result_components_used = ifelse(str_detect(wflow_id, "no_diff_components"), "no diff components only", "diff and no diff components"),
            model_type = case_when(
              str_detect(wflow_id, "log_reg") ~ "Logistic Reg",
              str_detect(wflow_id, "mars") ~ "MARS",
              str_detect(wflow_id, "xgb") ~ "XGBoost",
              str_detect(wflow_id, "rules") ~ "C5.0 Rules"
            ),
            label = paste(model_type, result_components_used, preproc, sep = "\n")
          ) %>%
          group_by(mean, label) %>%
          roc_curve(truth = wbit_error, .pred_1)
      } else {
        models %>%
          unnest(result) %>%
          unnest(.predictions) %>%
          left_join(., roc_vals %>%
            select(wflow_id, mean)) %>%
          mutate(
            preproc = case_when(
              str_detect(wflow_id, "impute") ~ "mean impute",
              str_detect(wflow_id, "normalize") ~ "mean impute & normalize",
              str_detect(wflow_id, "plain") ~ "dummy variable creation"
            ),
            result_components_used = "no diff components only",
            model_type = case_when(
              str_detect(wflow_id, "log_reg") ~ "Logistic Reg",
              str_detect(wflow_id, "mars") ~ "MARS",
              str_detect(wflow_id, "xgb") ~ "XGBoost",
              str_detect(wflow_id, "rules") ~ "C5.0 Rules"
            ),
            label = paste(model_type, result_components_used, preproc, sep = "\n")
          ) %>%
          group_by(mean, label) %>%
          roc_curve(truth = wbit_error, .pred_1)
      }

    roc_val_distinct <-
      roc_data %>%
      select(mean, label) %>%
      distinct()

    roc_data %>%
      ggplot() +
      geom_line(aes(x = 1 - specificity, y = sensitivity), linewidth = .75) +
      geom_abline(slope = 1, intercept = 0, linewidth = 0.4, linetype = "dashed") +
      theme(
        text = element_text(size = 20),
        legend.position = "right"
      ) +
      geom_label(data = roc_val_distinct, aes(label = paste("AUC: ", round(mean, 4)), x = .75, y = .25)) +
      scale_x_continuous(labels = seq(0, 1, .5), breaks = seq(0, 1, .5)) +
      labs(title = "Candidate Model ROC-AUC Curves", color = "model") +
      theme(text = element_text(size = 15), legend.position = "none") +
      facet_wrap(vars(label))
  }
