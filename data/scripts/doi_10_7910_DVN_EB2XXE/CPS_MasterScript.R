# Note: all quantitative analyses of our original protest data as well as the ESS rounds 1-5 used R Version 2024.04.0
# Set the working directory for all scripts
setwd("~/Dropbox/CPS/CPS_Materials")

# List of script names to source
script_files <- c("Step1_CPS_ProtestDataCleaning.R", "CPS_MainFigures_Appendices1-2.R", "CPS_Figures5-A4-A8.R", 
                  "CPS_Figure1.R", "CPS_Figure6.R", "CPS_AppendixExcludingLettersStatements.R")

# Source each script to reproduce full analysis. Note that "CPS_Figure1.R", "CPS_Figure6.R" will require users to first download ESS rounds 1-5 
# from the ESS website at https://ess.sikt.no/en/data-builder/

for (script in script_files) {
  source(script)
}