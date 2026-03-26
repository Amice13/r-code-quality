
check_create_dir <- function(dir_path) {
  print(dir_path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path)
  }
}


parse_make_args <- function(user_arg_names = c(), inter_active = interactive(), task_dir = NULL) {

  if (!inter_active) {
    # Read command line arguments automagically ----

    parser <- ArgumentParser()
    map(.x = user_arg_names, .f = ~ parser$add_argument(str_c('--', .x)))
    cl_args <- parser$parse_args()

  } else {

    # Parse command line arguments from makefile ----

    # Get task directory for current script and set working directory to correct path
    if (is.null(task_dir)) {
      task_dir <- gsub("src(.*)?", "", rstudioapi::getSourceEditorContext()$path, "/")
    }
    if (task_dir != '') {
      setwd(task_dir)
    }
    print(glue('Working directory: {getwd()}'))

    # Read makefiles
    makefile <- readLines('makefile')
    if (file.exists('../config.mk')) {
      makefile_config <- readLines('../config.mk')
    } else if (file.exists('../../config.mk')) {
      makefile_config <- readLines('../../config.mk')
    } else {
      makefile_config <- readLines('../../../config.mk')
    }
    makefile <- c(makefile, makefile_config)

    # Assert that correct version of R is being used
    config_r_version <- str_extract_all(str_subset(makefile_config, "R_script"), pattern = "[:digit:]") %>%
      unlist() %>%
      paste0(collapse = ".")
    current_r_version <- paste(sessionInfo()$R.version$major, sessionInfo()$R.version$minor, sep = '.')
    if(current_r_version != config_r_version) { stop("R version 3.6.3 required")}

    # Find lines where important directories are defined
    assignment_regex = '( )?=( )?|( )?:=( )?'
    out_dir <- strsplit(makefile_config[grepl("OUT_DIR", makefile_config)], assignment_regex)[[1]][[2]]
    build_data_pipeline_dir <- strsplit(makefile_config[grepl("BUILD_DATA_PIPELINE", makefile_config)], assignment_regex)[[1]][[2]]
    benfit_cost_pipeline_dir <- strsplit(makefile_config[grepl("BENEFIT_COST_PIPELINE", makefile_config)], assignment_regex)[[1]][[2]]

    # Find lines where args are defined
    arg_names = unique(c(user_arg_names, c("OUT_DIR")))

    # Find lines where args are defined
    arg_lines <- makefile[grepl(paste(paste0('^', arg_names), collapse = '|'), makefile)]

    # Separate arg name from arg definition
    arg_list <- list(sapply(strsplit(arg_lines, assignment_regex), '[[',1), sapply(strsplit(arg_lines, '= '), '[[',2))

    # Replace directory placeholders with correct path
    # Note: since working directory has been set above, replace within project and
    #     task file paths with relative file paths
    cl_args <- gsub('\\$\\(PROJECT_DIR\\)\\/', glue(here(), "/"), arg_list[[2]])
    cl_args <- gsub('\\$\\(OUT_DIR\\)\\/', out_dir, cl_args)
    cl_args <- gsub('\\$\\(TASK_DIR\\)\\/', '', cl_args)
    cl_args <- gsub('\\$\\(BUILD_DATA_PIPELINE\\)\\/', build_data_pipeline_dir, cl_args)
    cl_args <- gsub('\\$\\(BENEFIT_COST_PIPELINE\\)\\/', benfit_cost_pipeline_dir, cl_args)
    cl_args <- lapply(lapply(cl_args, strsplit, split = ' '), `[[`, 1)
    names(cl_args) = arg_list[[1]]

    # If there are function paths defined in the makefile replace with relative file paths
    if(!is.null(cl_args$task_fns)){
      cl_args$task_fns <- paste0("R/", list.files("R/"))
    }

    # Check for and possibly create output dir ----
    check_create_dir(cl_args$OUT_DIR)

  }

  message("Command Line Arguments:")
  print(cl_args[user_arg_names])

  return(cl_args)
}


get_stars <- function(x, stars = TRUE) {

  if (stars == FALSE) return("")

  stars <- case_when(
  x < .01 ~ "***",
  x < .05 ~ "**",
  x < .1 ~ "*",
  TRUE ~ "")

  return(stars)
}


get_timestamp <- function() {
  timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
  return(timestamp)
}


knitr_setup <- function() {

  knitr::opts_chunk$set(include = T)
  knitr::opts_chunk$set(echo = F)
  knitr::opts_chunk$set(warning = F)
  knitr::opts_chunk$set(message = F)

  knitr::opts_chunk$set(out.height = "\\textheight",  out.width = "\\textwidth")

  options(knitr.table.format = "latex",
          knitr.kable.NA = '',
          scipen = 999)

}


get_numeric_window <- function(number_word) {

  number = as.numeric(gsub('post_', '', number_word))

  return(number)
}


get_outcomes <- function(data, outcome_set_list, outcome_suffix_list, outcome_set_lookup, check_for_existance = T) {

  outcomes = c()
  for (outcome_set in outcome_set_list) {
    outcomes = c(outcomes, outcome_set_lookup[[outcome_set]])
  }

  all_outcomes = c()
  if (!is.null(outcome_suffix_list)) {
    for (outcome_suffix in outcome_suffix_list) {
      all_outcomes = c(all_outcomes, paste(outcomes, outcome_suffix, sep = '_'))
    }
  } else {
    all_outcomes = outcomes
  }

  if (check_for_existance) {
    missing_outcomes = all_outcomes[!all_outcomes %in% gsub('_post_20', '', names(data))]
    if (length(missing_outcomes) > 0) {
      if (length(outcome_suffix_list) == 0) {
        stop(glue("The following outcomes are not in the dataset: {paste0(missing_outcomes, collapse=', ')}"))
      } else {
        print(glue("The following outcomes are not in the dataset and will be skipped: {paste0(missing_outcomes, collapse=', ')}"))
        all_outcomes = all_outcomes[!all_outcomes %in% missing_outcomes]
      }
    }
  }

  return(all_outcomes)
}


standardize_component_with_control_mean_and_sd <- function(dframe, component, tvar = treatment){

  # Get mean and sd of component in control group
  control_stats <- dframe %>%
    filter(!!sym(tvar) == 0) %>%
    summarise(mean = mean(!!sym(component), na.rm = TRUE),
              sd = sd(!!sym(component), na.rm = TRUE))
  # standardize component with than mean and sd
  dframe %>%
    mutate(!!str_c(component, "_s") := (!!sym(component) - control_stats$mean)/control_stats$sd)

}


create_index_for_window <- function(window, dframe, components = c(), tvar = 'treatment') {

  components <- str_c(components, str_c("_", window))
  i_col <- create_index(dframe, components = components, tvar = tvar)

  return(i_col)
}


create_index <- function(dframe, components = c(), tvar = 'treatment') {

  components_s <- str_c(components, '_s')

  for (component in components) {
    dframe <- standardize_component_with_control_mean_and_sd(dframe, component, tvar)
  }

  i_col = rowMeans(select(dframe, all_of(components_s)))

  return(i_col)
}
