#---- Primary replication script ----

# Daniel Devine, Stuart J. Turnbull-Dugarte, and Matt Ryan (2025)
# "Success Denied: Social class and Perceptions of Political Success"
# Legislative Studies Quarterly
# README: This single script will reproduce all analysis and output from the main text and supplementary appendix file,
# and produces two .html files of the log output from each.

remove(list=ls())

library(knitr)
library(sessioninfo)  

rmarkdown::render("script1_datacleaning.R")
rmarkdown::render("script2_analysis.R")


writeLines(c(
  "---- Session Info: Daniel Devine, Stuart J. Turnbull-Dugarte, and Matt Ryan (2025) ----",
  capture.output(sessioninfo::session_info())
), "session_log.txt")


###The following figures and tables are not reproduced as they are descriptive items not rendered in R.
# Figures 1, 5, 6 
# Tables 1, 4, 5
