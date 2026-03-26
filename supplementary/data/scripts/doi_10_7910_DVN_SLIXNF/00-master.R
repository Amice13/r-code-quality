##
## Main script
## 

## create save dir
system("mkdir ../results/application")
system("mkdir ../results/simulation")
system("mkdir ../results/figures")
system("mkdir ../results/figures/assessment")
system("mkdir ../results/figures/estimate")
system("mkdir ../results/figures/combined")
system("mkdir ../results/figures/interpretation")
system("mkdir ../results/tables")


## application
cat("\n
  ----------------------------------------\n 
  > Running main analysis\n
  ----------------------------------------\n 
")

## main analysis
source("01-application_malesky-etal_main.R")
source("03-application_malesky-etal_main-figures.R")


cat("\n
  ----------------------------------------\n 
  > Running additional analysis\n
  ----------------------------------------\n 
")

## analyze all outcomes
source("02-application_malesky-etal_all.R")
source("04-application_malesky-etal_all-figures.R")
rmarkdown::render(
  input = "05-application_malesky-etal_output.Rmd",
  output_file = "../results/figures/interpretation/figure4_malesky-etal_output.pdf"
)

cat("\n
  ----------------------------------------\n 
  > Running staggered adoption analysis\n
  ----------------------------------------\n 
")

## staggered adoption: compare estimators

source("11-application_paglayan_main.R")
source("12-application_paglayan_augsynth.R")
source("13-application_paglayan_gsynth.R")


cat("\n
  ----------------------------------------\n 
  > Running additional analysis for staggered adoption\n
  ----------------------------------------\n 
")

## create figures
source("14-application_paglayan_check-PT.R")
source("15-application_paglayan_figures.R")


cat("\n
  ----------------------------------------\n 
  > Running simulation analysis\n
  ----------------------------------------\n 
")


## setup files
source("21-simulation_setup.R")
source("22-simulation_dgp.R")
source("23-simulation_function.R")

## run simulation
source("24-simulation_run.R")

## summarize results and generate figures
source("25-simulation_efficiency-auto-figure.R")
source("26-simulation_efficiency-figure.R")


cat("\n
  ----------------------------------------\n 
  > Generating figures for illustration\n
  ----------------------------------------\n 
")

source("31-illustration-figures.R")

cat("\n
  ----------------------------------------\n 
  > Done!\n
  ----------------------------------------\n 
")
