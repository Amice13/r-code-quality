#### End. 
### do not run. 

### For citing used R Packages
library(grateful)

grateful::cite_packages(output = "citekeys", # what will be displayed as output
                        out.dir = ".", # save report to working directory
                        omit = NULL, # to be grateful for grateful
                        cite.tidyverse = TRUE,
                        dependencies = FALSE,
                        include.RStudio = TRUE,
                        bib.file ="software-citations")

#### Session Info for Reproducibility: ####  
writeLines(
  capture.output(sessionInfo()),
  ( "session-info.txt")
)

