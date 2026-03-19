## Eszter Hargittai & Aaron Shaw
## 2019
## 01-import.R

## Description of this file:
## Imports dataset, R packages/libraries, and creates several
## tables/variables for use later in the analysis and modeling.

setwd("~/research/web_use_US_survey/mturk_norc_comparison/analysis/")

## May need to set the working directory first:
d <- readRDS("Hargittai-Shaw-AMT-NORC-2019.rds")

### Provide dependent variable object mapping
dv.map <- data.frame("sd.varname"= c("std_accesssum",
                                     "std_webweekhrs",
                                     "std_skillsmean",
                                     "std_snssumcompare",
                                     "std_do_sum",
                                     "std_trust_scale",
                                     "std_altru_scale",
                                     "std_pts_give"),
                     "var.name" = c("access", "webhrs", "skills",
                                     "snssum", "dosum",
                                     "trust","altruism","points"),
                     "var.name.pretty" =
                         factor(c("Internet use autonomy",
                                  "Internet use frequency",
                                  "Internet use skills",
                                  "Social media site use",
                                  "Online participation",
                                  "General trust",
                                  "Cooperative behavior",
                                  "Generosity")))
