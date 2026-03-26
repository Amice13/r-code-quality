# ====
# Table C1 Replication:
# Extensive margin table for Vietnam
# input:
# - intensive_margin_paneldata.RData
# - vnm_extmarginlist_export.RData
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

################################################################################
## 0. load data  ------
################################################################################
# fdi data
load(file = paste0(REPLICATION_DATA_DIR, "intensive_margin_paneldata.RData"))
data.panel.pm  %>% glimpse()

# product list (Vietnamese exports above certain trade volume (100 USD) and  corresponding to NAICS 31-33)
load(file = paste0(REPLICATION_DATA_DIR, "vnm_extmarginlist_export.RData"))
extmargin %>% glimpse()

# ################################################################################
# ## 1. Check Vietnamese data ------
# ################################################################################

# to list format
extmargin$extlist <- lapply(extmargin$product.list,
function(x) str_split(x, ",") %>% 
                unique(.) %>% unlist(.))

# take the lagged list
extmargin$extlist_lag <- c(NA, extmargin$extlist[1:(nrow(extmargin) - 1)])


# for each year, extract the newly added products
extmargin$diflist <- mapply(function(x, y) setdiff(x, y), extmargin$extlist, extmargin$extlist_lag) # those in x but not in y

# length of extlist and diflist
extmargin <- extmargin %>%
        mutate(extlen = lengths(extlist),
               diflen = lengths(diflist)) # matches with the extensive margin data

extmargin %>% glimpse()


# newly-added product level --------------------------------
newprod_long <- extmargin %>% 
        filter(Year >= 2003) %>%
        unnest(cols = c(diflist)) 

newprod_long %>% glimpse()

newprod_long$newprod <- 1


# add the FDI information
newprod_long <- newprod_long %>%
    left_join(data.panel.pm %>% dplyr::select(fdi_count_dm_exp.once, HS_6d, year), by = c("diflist" = "HS_6d", "Year" = "year")) 

newprod.tab <- newprod_long %>% 
        mutate(fdi_count_dm_exp.once = replace_na(fdi_count_dm_exp.once, 0)) %>% 
        group_by(Year, fdi_count_dm_exp.once) %>% 
        summarise(n = n()) %>%
        ungroup() %>% 
        tidyr::pivot_wider(names_from = fdi_count_dm_exp.once, values_from = n)

newprod.tab

# add back the extensive margin data
newprod.tab <- newprod.tab %>% left_join(extmargin %>% dplyr::select(Year, extlen, diflen) %>% distinct(), by = c("Year"))


# add ratio
newprod.tab <- newprod.tab %>% mutate(fdi_ratio = (`1`/diflen)*100)


newprod.tab <- newprod.tab[c("Year",  "extlen", "diflen", "1", "0", "fdi_ratio")]

newprod.tab <- newprod.tab %>% dplyr::rename(`FDI` = `1`, 
                                             `No FDI` = `0`,
                                             `FDI-related (\\%)` = fdi_ratio,
                                             `Extensive margin` = extlen,
                                             `New products ($t$ - $(t-1)$)` = diflen)


names(newprod.tab)

mean(newprod.tab$FDI)
mean(newprod.tab$FDI/newprod.tab$`New products ($t$ - $(t-1)$)`)

summary(newprod.tab$`FDI-related (\\%)`)
sd(newprod.tab$`FDI-related (\\%)`)

# print to latex
kable(newprod.tab, format = "latex", booktabs = T, digits = 2, align = "c", linesep = "", escape = F) %>%
  save_kable(file = paste0(REPLICATION_FIG_DIR, "TC1_extmargin_vnm_newprod_tab.tex"))

# print to html
newprod.tab.html <- newprod.tab %>% dplyr::rename(
    `FDI-related (%)` = `FDI-related (\\%)`, 
    `New products (t - (t-1))` = `New products ($t$ - $(t-1)$)`)

kable(newprod.tab.html, format = "html", digits = 2, align = "c") %>%
  save_kable(file = paste0(REPLICATION_FIG_DIR, "TC1_extmargin_vnm_newprod_tab.html"))
