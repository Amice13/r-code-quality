#################################################################################
### load needed packages & define functions
#################################################################################

library(tidyverse)
library(readxl)

## get LA City/County longitudinal group population from raw IPUMS files,
## decennial census data, and corresponding sheet in glossaries workbook
get_la_dec <- function(.data) {
     v1 <- c("YEAR", "COUNTY", "PLACE")
     v2 <- .data$var
     v <- c(v1, v2)
     unlist(map(unique(.data$file), 
                ~ paste0("data/raw/", ., c("_county", "_place"), ".csv"))) %>%
          map_dfr(~ read_csv(., col_select = any_of(v), 
                             col_types = cols(.default = col_character()))) %>%
          pivot_longer(cols = -v1, names_to = "var", values_to = "est") %>%
          filter(str_detect(COUNTY, "Los Angeles") | 
                      str_detect(PLACE, "Los Angeles")) %>%
          mutate(geography = if_else(is.na(COUNTY), "LA City", "LA County")) %>%
          right_join(.data, by = c("YEAR" = "year_data", "var")) %>%
          group_by(year, geography, lab) %>%
          summarize(est = sum(as.integer(est), na.rm = TRUE)) %>%
          mutate(pr_est = est / max(est)) %>%
          ungroup()
}

#################################################################################
### read in data
#################################################################################

## decennial census group variables
groups_dec <- read_excel("data/glossaries.xlsx", sheet = "Groups (Decennial)", 
                         col_types = "text") 

## ACS group variables
groups_acs <- read_excel("data/glossaries.xlsx", sheet = "Groups (Full)",
                         col_types = c("text", "skip", "text", "text")) %>%
     distinct()

## CBSA directory
cbsa_dir <- read_excel("data/glossaries.xlsx", sheet = "CBSAs", 
                       col_types = c("text", "skip", "text", "text")) %>%
     mutate(across(.fns = as.factor))

## CBSA demographics
cbsa <- read_csv("data/clean/groups_cbsa.csv.gz", col_types = "fffi") %>%
     left_join(cbsa_dir)

#################################################################################
### identify immigrant gateways
#################################################################################

gateways <- cbsa %>%
     filter(abb %in% c("pop", "fb") & cbsa_type == "metro") %>%
     pivot_wider(names_from = "abb", values_from = "est",
                 id_cols = c("year", "cbsa_name")) %>%
     group_by(year) %>%
            # share foreign-born
     mutate(pr_fb_est = fb / pop,
            # share foreign-born in standard-deviation (SD) units
            z_fb_est = as.numeric(scale(pr_fb_est)),
            # immigrant gateways have foreign-born shares >= 1 SD
            imm_gate = z_fb_est >= 1) %>%
     ungroup()

#################################################################################
### group population shares over time
#################################################################################

pop_over_time <- get_la_dec(groups_dec) %>%
     # format for easy reading
     mutate(est = str_pad(prettyNum(est, ",", scientific = FALSE), 10, "left"),
            pr_est = str_pad(
                 paste0("(", 
                        str_trim(format(round(pr_est * 100, 1), nsmall = 1)), 
                        "%)"),
                 8, "left")) %>%
     unite("value", c("est", "pr_est"), sep = " ") %>%
     mutate(lab = factor(lab, levels = c("Total", "Black", "Latino", "Asian",
                                         "Monoracial White", "Immigrant"))) %>%
     pivot_wider(id_cols = c("year", "geography"), names_from = "lab") %>%
     arrange(geography, year) %>%
     select(Year = year, Geography = geography, Total, Black, Latino,
            Asian, `Monoracial White`, Immigrant)

View(pop_over_time)

#################################################################################
### densities
#################################################################################

# rank metropolitan group populations by year
rank_pop <- cbsa %>%
     left_join(groups_acs) %>%
     filter(cbsa_type == "metro" & !is.na(est)) %>% 
     group_by(year, lab) %>%
     mutate(rank_est = percent_rank(est)) %>%
     ungroup() 

# immigrant groups for which LA Metro has the largest populations in 2015-19
la_largest <- rank_pop %>%
     filter(year == "2015-19" & cbsa_name == "Los Angeles CA" &
                 rank_est == 1 & str_detect(lab, "Immigrant,")) %>%
     .$lab %>%
     str_remove("Immigrant, ")

print(la_largest)