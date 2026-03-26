require(remotes)

repo <- "http://cran.us.r-project.org"

remotes::install_version(
  "tidyverse",
  version = "2.0.0",
  repos = repo
)
remotes::install_version(
  "tidymodels",
  version = "1.2.0",
  repos = repo
)
remotes::install_version(
  "workflowsets",
  version = "1.1.0",
  repos = repo
)
remotes::install_version(
  "xgboost",
  version = "1.7.7.1",
  repos = repo
)
remotes::install_version(
  "rules",
  version = "1.0.2",
  repos = repo
)
remotes::install_version(
  "parallel",
  version = "4.4.0",
  repos = repo
)
remotes::install_version(
  "glue",
  version = "1.7.0",
  repos = repo
)
remotes::install_version(
  "finetune",
  version = "1.2.0",
  repos = repo
)
remotes::install_version(
  "reactable",
  version = "0.4.4",
  repos = repo
)
remotes::install_version(
  "reactablefmtr",
  version = "2.0.0",
  repos = repo
)
