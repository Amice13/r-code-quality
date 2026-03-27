# ====
# Table B4 Replication:
# FDI project count by country
# input: 
# - fdi_count_by_country.csv
# - extensive_margin_paneldata_Export
# R version 4.4.2 (2024-10-31)
# ====

# clean slate
rm(list = ls())
date()

library(tidyverse)
library(kableExtra)

# set main directory
REPLICATION_DATA_DIR <- "data/"
REPLICATION_FIG_DIR <-  "output/"

##########################################################
## 0. load data  ------
##########################################################
# Number of projects (all/manufacturing) between 2003-17 for each country
fdi_all_country_tab <- read_csv(file = paste0(REPLICATION_DATA_DIR,
                                              "fdi_count_by_country.csv"))

fdi_all_country_tab %>% glimpse()

# only keep countries used in Table 1
extensive_dat <- read_csv(file = paste0(REPLICATION_DATA_DIR,
                                        "extensive_margin_paneldata_Export.csv"))
country_used <- unique(extensive_dat$iso3c)

fdi_all_country_tab <- fdi_all_country_tab %>%
  filter(destin_ctry_iso3c %in% country_used)

################################################################################
## 1. To table format ------
################################################################################

fdi_all_country_tab <- fdi_all_country_tab %>%
  dplyr::select(destin_country, all, manufacturing) %>%
  mutate(all = as.integer(all),
         manufacturing = as.integer(manufacturing)) %>%
  dplyr::rename("Host Country" = destin_country,
                "Total" = all,
                "Manufacturing" = manufacturing) %>%
  arrange(desc(Total))

# split into two columns for the paper
halfrow <- as.integer((nrow(fdi_all_country_tab) + 1) / 2)
fullrow <- nrow(fdi_all_country_tab)

# latex table 
tab1 <- kable(fdi_all_country_tab[1:halfrow, ], position = "t",
              booktabs = TRUE, format = "latex", linesep = "")

tab2 <- kable(fdi_all_country_tab[(halfrow + 1):fullrow, ],
              booktabs = TRUE, format = "latex", linesep = "")

tab1 <- tab1 %>%
  gsub("\\\\begin\\{tabular\\}", "\\\\begin\\{tabular\\}[t]", .)

tab2 <- tab2 %>%
  gsub("\\\\begin\\{tabular\\}", "\\\\begin\\{tabular\\}[t]", .) %>%
  gsub("\\\\bottomrule", "&&\\\\\\\\ \\\\bottomrule", .) %>%  # add one empty row at the bottom
  gsub("Côte d’Ivoire", "C\\\\^ote d'Ivoire", .)

writeLines(tab1,
           paste0(REPLICATION_FIG_DIR, "TB4_fdi_count_percountry_1.tex"))

writeLines(tab2,
           paste0(REPLICATION_FIG_DIR, "TB4_fdi_count_percountry_2.tex"))

# html table
tab1.html <- kable(fdi_all_country_tab[1:halfrow, ], 
              format = "html")

tab2.html <- kable(fdi_all_country_tab[(halfrow + 1):fullrow, ],
              format = "html")

writeLines(tab1.html,
           paste0(REPLICATION_FIG_DIR, "TB4_fdi_count_percountry_1.html"))

writeLines(tab2.html,
           paste0(REPLICATION_FIG_DIR, "TB4_fdi_count_percountry_2.html"))
