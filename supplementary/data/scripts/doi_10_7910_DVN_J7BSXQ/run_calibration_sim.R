suppressMessages(source("simulate.R"))

set.seed(21181011)




## main runs simulation
if(!interactive()) {

  pscore <- NULL
  y_model <- NULL
  n <- NULL
  max_order <- NULL
  n_sims <- NULL
  data_path <- NULL
  n_cores <- NULL

  args <- commandArgs(TRUE)
  if(length(args) != 0) {
      for(i in 1:length(args)) {
          eval(parse(text = args[i]))
      }
  }  
  
  pscore <- if(is.null(pscore)) "pscore_rf" else pscore
  y_model <- if(is.null(y_model)) "out_rf" else y_model
  max_order <- if(is.null(max_order)) 4 else max_order
  n_sims <- if(is.null(n_sims)) 1000 else n_sims
  data_path <- if(is.null(data_path)) "sim_data.csv" else data_path
  n_cores <- if(is.null(n_cores)) 1 else n_cores


  true_data <- read_csv(data_path)

  X <- true_data %>%
    select(-contains("pscore"), -contains("out"), -responded, -rvote)
  
  # n_cores <- Sys.getenv("SLURM_CPUS_PER_TASK")
  print(c(pscore, y_model, n_sims))
  invisible(
    res <- eval_general(n_sims, n_cores,
                        X = X, y_model = true_data[[y_model]],
                        pscore = true_data[[pscore]], max_order = max_order,
                        n_dat = n)
  )
  res$n <- n
  res$pscore <- pscore
  res$y_model <- y_model
  res$n_sims <- n_sims
  res$data_path <- data_path

    ## save file
    timestamp <- format(Sys.time(), "%d-%b-%Y-%H-%M-%S")

    sim_name <- paste(pscore, y_model, n,
                      sep = "_")

    path <- "results/"

    res_name <- paste(path,
                      paste(sim_name,
                            timestamp, sep = "_"),
                      ".csv", sep = "")

    write_csv(res, res_name)

}

