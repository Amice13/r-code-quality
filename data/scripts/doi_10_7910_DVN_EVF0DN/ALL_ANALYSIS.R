################################################################################
# RUNS ALL ANALYSIS
#
#
# Source from command line: 
# nohup Rscript scripts/analysis/ALL_ANALYSIS.R  >ALL_RAILPREP.out>&1 & 
################################################################################

if (!"here" %in% installed.packages()[, 1]) {
  
  install.packages("here")
  library(here)

  } else {
  
  library(here)
  
}

# Check package installation
source(here("scripts", "analysis", "setup_analysis.R"))


# MAIN RESULTS

# Runs main models with combined outcome
source(here("scripts", "analysis", "did_separatism.R"))
# Produces table 1, figure 3

# Runs IV models
source(here("scripts", "analysis", "separatism_iv.R"))
# Produces tables 2 and A3

# Runs interaction models for heterogeneity
source(here("scripts", "analysis", "rails_ia.R"))
# Produces tables A12 and A13, figures 4a-4f


# MECHANISMS

# Runs models to test causal mechanisms
source(here("scripts", "analysis", "mech_separatism.R"))
# Produces tables 3 and A14-A20


# ROBUSTNESS

# Runs models with alternative fect estimator
source(here("scripts", "analysis", "did_estimators.R"))
# Produces Produces table A2

# Runs models dropping segments after first onset
source(here("scripts", "analysis", "did_separatism_npc.R"))
# Produces table A4

# Runs models dropping never-treated segments
source(here("scripts", "analysis", "did_separatism_nnt.R"))
# Produces table A5

# Runs main models on sample covering period with treatment variation
source(here("scripts", "analysis", "did_1815_1922.R"))
# Produces Produces table A6, figure A7

# Runs mechanism models on sample covering period with treatment variation
source(here("scripts", "analysis", "mech_1815_1922.R"))
# Produces Produces table A7

# Runs models with successful secession only
source(here("scripts", "analysis", "did_secession.R"))
# Produces table A8, figure A8

# Runs models with secessionist conflict onset as outcome only
source(here("scripts", "analysis", "did_cw_terr.R"))
# Produces table A9, figure A9

# Runs models with nationalist claims as outcome only
source(here("scripts", "analysis", "did_ind_aut.R"))
# Produces table A10, figure A10

# Runs models with irredentist claims in the outcome
source(here("scripts", "analysis", "did_separatism_irr.R"))
# Produces table A11, figure A11


# DESCRIPTIVES

# Make trend plots and spatial maps of outcomes
source(here("scripts", "analysis", "desc_outcomes.R"))
# Produces figures A4, A5a-A5d

# Make trend plots and spatial maps of outcomes
source(here("scripts", "analysis", "validate_rails.R"))
# Produces figures A2 and A3a-A3b

# Make spatial plot of railway network
source(here("scripts", "analysis", "desc_rails.R"))
# Produces figure 2

# Make table of descriptive statistics
source(here("scripts", "analysis", "desc_table.R"))
# Produces table A1 