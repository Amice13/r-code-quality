library(arrow)
library(assertr)
library(here)
library(readxl)
library(sf)
library(statar)
library(tidyverse)


# Import census 2012-2016 BLOCK GROUP population and boundary to obtain BG that
# represents tract centroids

se_bg <- read_csv(here::here("data-raw-temp/se/bg2012_5yr/R13149433_SL150.csv"),
                  col_select = c("Geo_FIPS", "ACS16_5yr_B01003001"),
                  col_types = paste(c("c", "n"), collapse = "")) %>% 
  rename(GEOID = Geo_FIPS) %>% 
  rename(pop_acs16_bg = ACS16_5yr_B01003001) %>% 
  mutate(tract = substr(GEOID, 1, 11))


ca_bg <- st_read(here::here("data-raw-temp/tl/tl_2016_06_bg/tl_2016_06_bg.shp")) %>%
  st_transform(4326)

# Use 2016 BG boundary to avoid issues related to the
# erroneous deletion of tract 1370 in 2010 Census.
# This step is necessary because land area is missing for block groups
# in tract 1370 even in the 2016 5-yr ACS file. 

# Merge
ca_bg <- merge(se_bg, ca_bg, 
               by = "GEOID", 
               all = TRUE) %>% 
  mutate(pop_per_sqm_bg = pop_acs16_bg / ALAND)

# Population per square meter in each block group
ca_bg %>% 
  dplyr::filter(tract=="06037137000") %>% 
  assertr::verify(!is.na(pop_per_sqm_bg))

tract_bg <- ca_bg %>%
  group_by(tract) %>%
  arrange(desc(pop_per_sqm_bg), .by_group = TRUE) %>%
  dplyr::filter(row_number() == 1) %>%
  dplyr::select(tract, GEOID) %>%
  dplyr::rename(bg1 = GEOID) %>% 
  ungroup() %>% 
  dplyr::filter(str_sub(tract, 1, 5) %in% 
                  c("06037", "06059", "06065", "06071", "06111",
                    "06001", "06013", "06041", "06055", "06075", 
                    "06081", "06085", "06095", "06097"))
  
rm(ca_bg, se_bg)

# Derive "job accessibility"
# Job accessibility is defined as the distance-weighted sum of jobs within 50 
# miles of each census tract centroid.
# First, merge with NBER Block Group Distance Database (2010, 50-mile radius) on
# bg1 and keep only the matched records. Next, merge with total job counts in 
# Workplace Census Blocks (from the LEHD-RAC 2019 file) on bg2.
# The Workplace Area Characteristics (WAC) file provides information on the jobs 
# located in each block by counting workers who work (rather than live) in it. 
# Here, bg1 is treated as the home BG, and bg2 is workplace within a 50 miles.

tract_bg %>%
  dplyr::filter(tract=="06037137000") %>% 
  assertr::verify(bg1=="060371370001")

tract_bg["bg1"][tract_bg["bg1"] == "060371370001"] <- "060379304011"
# Confirmed that "060371370001" is the densest BG in tract 13700.00; however,
# it won't be matched with the NBER 2010 file due to the erroneous deletion 
# of tract 1370 in 2010 Census. 
# Source: https://www2.census.gov/geo/pdfs/reference/Geography_Notes.pdf
# Recode "060371370001" to BG 9304.011 because that's where the centroid of 
# "060371370001" should fall (by overlaying two shapefiles)

f <- function(x, pos) subset(x, str_sub(tract1, 1, 5) %in% 
                               c("06037", "06059", "06065", "06071", "06111",
                                 "06001", "06013", "06041", "06055", "06075", 
                                 "06081", "06085", "06095", "06097"))

nber_50mi <- read_delim_chunked(
  here::here("data-raw-temp/nber/sf12010blkgrpdistance50miles.csv"),
  DataFrameCallback$new(f),
  delim = ",",
  escape_double = FALSE,
  trim_ws = TRUE,
  col_types = cols_only(
    tract1 = col_character(), 
    blkgrp1 = col_character(),
    mi_to_blkgrp = col_double(), 
    tract2 = col_character(),
    blkgrp2 = col_character()
  ))

nber_50mi <- nber_50mi %>% 
  mutate(state = substr(tract1, 1, 2)) %>% 
  dplyr::filter(state=="06") %>% 
  mutate(bg1 = paste0(tract1, blkgrp1)) %>% 
  mutate(bg2 = paste0(tract2, blkgrp2)) %>% 
  dplyr::select(bg1, bg2, mi_to_blkgrp)

tract_bg_50mi <- statar::join(tract_bg, nber_50mi,
                              kind="full",
                              on="bg1",
                              check=1~m,
                              gen = "merge")

tract_bg_50mi %>% statar::tab(merge)

tract_bg_50mi %>% 
  dplyr::filter(merge==1) %>% 
  assertr::verify(bg1=="061119800001")
# One unmatched from master: "061119800001" is on 
# San Nicolas Island, Ventura County - no jobs within the 50-mile radius,
# which means "061119800001" does not exist as "bg2" either.

tract_bg_50mi <- tract_bg_50mi %>% 
  dplyr::filter(merge==1 | merge==3) %>% 
  dplyr::select(-merge) 

## Merge with LEHD-WAC 2016 file (BLOCK Level data)
wac_2016 <- read_csv(here::here("data-raw-temp/lodes/ca_wac_S000_JT00_2016.csv"),
                     col_types = cols_only(w_geocode="c", C000="i")) %>%
  mutate(bg2 = substr(w_geocode, 1, 12)) %>% 
  group_by(bg2) %>%
  summarise(tot_jobs = sum(C000)) %>% 
  assertr::verify(anyDuplicated(bg2) == FALSE) 

tract_jobs_50mi <- statar::join(tract_bg_50mi, wac_2016,
                                kind = "full",
                                on = "bg2",
                                check = m~1,
                                gen = "merge")

tract_jobs_50mi %>% statar::tab(merge)

# 181 block groups missing jobs counts
tract_jobs_50mi  %>% 
  dplyr::filter(merge==1) %>% 
  assertr::verify(n_distinct(bg2)==181)

jobs_access_50mi <- tract_jobs_50mi %>% 
  group_by(bg1) %>% 
  mutate(jobs_access = sum(tot_jobs/mi_to_blkgrp, na.rm = TRUE))  %>% 
  arrange(bg2, .by_group = TRUE) %>% 
  dplyr::filter(row_number() == 1) %>% 
  ungroup() %>% 
  dplyr::select(tract, jobs_access)

jobs_access_50mi %>% 
  assertr::verify(anyDuplicated(tract) == FALSE) 

dir.create("data")

jobs_access_50mi %>% 
  arrow::write_feather("data/jobs-access.feather")

rm(list = ls())