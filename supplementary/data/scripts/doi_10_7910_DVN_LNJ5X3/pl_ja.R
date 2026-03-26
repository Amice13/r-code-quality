library(assertr)
library(here)
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


# Derive "job accessibility"
# Job accessibility is defined as the distance-weighted sum of jobs within 50 
# miles of each census tract centroid.
# First, merge with NBER Block Group Distance Database (2010, 50-mile radius). Next, merge with total job counts in 
# Workplace Census Blocks (from the LEHD-RAC 2019 file) on bg2.
# The Workplace Area Characteristics (WAC) file provides information on the jobs 
# located in each block by counting workers who work (rather than live) in it. 
# Here, bg1 is treated as the home BG, and bg2 is workplace within a 50 mile radius.

nber_50mi <- read_delim_chunked(
  here::here("data-raw-temp/nber/sf12010blkgrpdistance50miles.csv"),
  DataFrameCallback$new(function(x, pos) subset(x, str_sub(tract1, 1, 2) == "06")),
  delim = ",",
  escape_double = FALSE,
  trim_ws = TRUE,
  col_types = cols_only(
    tract1 = col_character(), 
    blkgrp1 = col_character(),
    mi_to_blkgrp = col_double(), 
    tract2 = col_character(),
    blkgrp2 = col_character()
  )) %>% 
  mutate(blkgrp1 = paste0(tract1, blkgrp1),
         blkgrp2 = paste0(tract2, blkgrp2)) %>% 
  dplyr::select(-starts_with("tract"))

wac_2016 <- read_csv(here::here("data-raw-temp/lodes/ca_wac_S000_JT00_2016.csv"),
                     col_types = cols_only(w_geocode="c", C000="i")) %>% 
  mutate(blkgrp2 = str_sub(w_geocode, 1, 12)) %>% 
  group_by(blkgrp2) %>% 
  summarise(tot_jobs = sum(C000)) %>% 
  ungroup() %>% 
  assertr::verify(anyDuplicated(blkgrp2) == FALSE) 

ca_bg_50mi <- statar::join(wac_2016, nber_50mi,
                           kind="full",
                           on="blkgrp2",
                           check=1~m)

rm(nber_50mi)

ja <- ca_bg_50mi %>% 
  group_by(blkgrp1) %>% 
  mutate(jobs_access = sum(tot_jobs/mi_to_blkgrp, na.rm = TRUE))  %>% 
  arrange(blkgrp2, .by_group = TRUE) %>% 
  dplyr::filter(row_number() == 1) %>% 
  ungroup() %>% 
  dplyr::select(blkgrp1, jobs_access)

bg2pl_col_names <- read_csv(here::here(
  "data-raw-temp/geocorr/geocorr2018_bg_to_pl_2010_popw.csv")) %>% 
  names()

bg2pl <- read_csv(here::here("data-raw-temp/geocorr/geocorr2018_bg_to_pl_2010_popw.csv"),
                  skip = 2,
                  col_names = bg2pl_col_names,
                  col_types = "ccccccccid") %>% 
  mutate(blkgrp1 = paste0(county, str_sub(tract, 1, 4), str_sub(tract, 6, 7), bg)) %>% 
  dplyr::filter(afact > 0.5) %>% 
  dplyr::select(-c(county, tract, bg, state, stab, afact)) 

pl2010_age <- read_csv(here::here("data-raw-temp/se/pl2010_census/R13276010_SL160.csv"),
                       col_names = TRUE,
                       col_types = paste0(paste0(rep("c", times = 11), collapse = ""), 
                                          paste0(rep("i", times = 48), collapse = ""))) %>% 
  rowwise() %>% 
  mutate(pl2010_18to64 = sum(SF1_P0120007, SF1_P0120008, SF1_P0120009, SF1_P0120010, SF1_P0120011,
                             SF1_P0120012, SF1_P0120013, SF1_P0120014, SF1_P0120015, SF1_P0120016,
                             SF1_P0120017, SF1_P0120018, SF1_P0120019, SF1_P0120031, SF1_P0120032,
                             SF1_P0120033, SF1_P0120034, SF1_P0120035, SF1_P0120036, SF1_P0120037,
                             SF1_P0120038, SF1_P0120039, SF1_P0120040, SF1_P0120041, SF1_P0120042,
                             SF1_P0120043, na.rm = TRUE)) %>% 
  dplyr::select(Geo_NAME, Geo_PLACE, pl2010_18to64) %>% 
  dplyr::filter(str_sub(Geo_NAME, -4) != " CDP") %>% 
  rename(placefp = Geo_PLACE)

bg2010_age <- read_csv(here::here("data-raw-temp/se/bg2010_census/R13276019_SL150.csv"),
                       col_names = TRUE,
                       col_types = paste0(paste0(rep("c", times = 11), collapse = ""), 
                                          paste0(rep("i", times = 48), collapse = ""))) %>% 
  rowwise() %>% 
  mutate(bg2010_18to64 = sum(SF1_P0120007, SF1_P0120008, SF1_P0120009, SF1_P0120010, SF1_P0120011,
                             SF1_P0120012, SF1_P0120013, SF1_P0120014, SF1_P0120015, SF1_P0120016,
                             SF1_P0120017, SF1_P0120018, SF1_P0120019, SF1_P0120031, SF1_P0120032,
                             SF1_P0120033, SF1_P0120034, SF1_P0120035, SF1_P0120036, SF1_P0120037,
                             SF1_P0120038, SF1_P0120039, SF1_P0120040, SF1_P0120041, SF1_P0120042,
                             SF1_P0120043, na.rm = TRUE)) %>% 
  dplyr::select(Geo_FIPS, bg2010_18to64) %>% 
  rename(blkgrp1 = Geo_FIPS)


bg2pl <- left_join(bg2pl, bg2010_age, by = "blkgrp1")

bg2pl <- statar::join(bg2pl, pl2010_age, on = "placefp", kind = "full", gen = "merge")
bg2pl %>% statar::tab(merge)

# Note that 11 cities (with working age populations ranging from 123 to 1240 don't have any block groups where more than 50% lives in the city.
bg2pl %>% 
  assertr::verify(nrow(
    bg2pl %>% dplyr::filter(merge == 2)) == 11)

bg2pl %>% 
  dplyr::filter(merge == 2) %>% 
  assertr::verify(pl2010_18to64 <= 1240)

bg2pl %>% 
  dplyr::filter(merge == 3) %>% 
  dplyr::select(-merge) %>% 
  mutate(afact = bg2010_18to64 / pl2010_18to64) %>% 
  dplyr::select(-ends_with("_18to64")) %>% 
  left_join(ja, by = "blkgrp1") %>% 
  mutate(ja_bg = jobs_access * afact) %>% 
  group_by(placefp, cntyname, placenm, Geo_NAME) %>% 
  mutate(pl_ja = sum(ja_bg, na.rm = TRUE)) %>% 
  distinct(placefp, cntyname, placenm, Geo_NAME, .keep_all = TRUE) %>% 
  dplyr::select(-c(pop10, blkgrp1, afact, jobs_access, ja_bg)) %>% 
  ungroup() %>% 
  mutate(Jurisdiction = str_replace(Geo_NAME, " city", "")) %>% 
  mutate(Jurisdiction = str_replace(Jurisdiction, " town", "")) %>% 
  mutate(Jurisdiction = str_to_upper(Jurisdiction)) %>%
  write_csv(here::here("data/pl_ja.csv"))