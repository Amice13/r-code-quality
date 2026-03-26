library(arrow)
library(tidyverse)

bay <- arrow::read_feather("data/bay_samp.feather") %>% 
  rename(PARCEL_COUNTY = PARCEL_BAYCOUNTY) %>% 
  dplyr::select(-c(OBI_JURIS, BS_APN2, BS_COUNTY, BS_CITY)) %>% 
  mutate(PARCEL_COG = "ABAG",
         PARCEL_ZONINGDESC = case_when(
           PARCEL_ZONING == 0 ~ "Non-Residential",
           PARCEL_ZONING == 1 ~ "Single Family Residential",
           PARCEL_ZONING == 2 ~ "Multi-Family Residential",
           PARCEL_ZONING == 3 ~ "Non-Developable"
         )) %>% 
  mutate(PARCEL_ZONINGCODE = as.character(PARCEL_ZONING)) %>% 
  dplyr::select(-PARCEL_ZONING)

scag <- arrow::read_feather("data/SCAG_samp.feather") %>% 
  rename(PARCEL_COUNTY = PARCEL_SCAGCOUNTY,
         PL_NAME = PL_SCAGNAME,
         PARCEL_ID = PARCEL_SCAGUID16) %>% 
  mutate(PARCEL_COG = "SCAG")
# 
# compare <- statar::join(names(bay) %>% as_tibble(), 
#                         names(scag) %>% as_tibble(),
#                         on = "value",
#                         kind = "full", 
#                         check = 1 ~ 1, 
#                         gen = "merge")
# 
# compare %>% filter(merge != 3)

combined <- bind_rows(bay, scag) %>% 
  mutate(combined = TRUE)

rm(bay, scag)



# Add slope data ----------------------------------------------------------

slope <- arrow::read_feather("data/slope.feather") %>% 
  mutate(slope = TRUE) %>% 
  rename(PARCEL_PROP_STEEPSLOPE = prop_steep_slope)

combined <- full_join(combined, slope, 
                      by = "PARCEL_ID") %>% 
  mutate(
    indicator = case_when(
      combined & slope ~ "both",
      combined ~ "combined only",
      slope ~ "slope only"
    ),
    .keep = "unused"
  ) %>% 
  assertr::verify(indicator != "combined only") %>% 
  dplyr::filter(indicator == "both") %>% 
  dplyr::select(-indicator) %>% 
  mutate(combined = TRUE) 

rm(slope)

# Add land cover data -----------------------------------------------------

land_cover <- read_csv(paste0("data-raw-temp/nhgis/us_tract_2015_nlcd_timevariesbyfile/",
                              "US_tract_2015_nlcd_2011.csv"), 
                       col_types = paste0("ic", strrep("d", 34))) %>% 
  dplyr::select(GISJOIN, starts_with("PROP")) %>% 
  replace_na(list(
    PROP11 = 0, PROP12 = 0, PROP21 = 0, PROP22 = 0, PROP23 = 0, PROP24 = 0, PROP31 = 0, 
    PROP41 = 0, PROP42 = 0, PROP43 = 0, PROP52 = 0, PROP71 = 0, 
    PROP81 = 0, PROP82 = 0, PROP90 = 0, PROP95 = 0)) %>% 
  mutate(TRT_2015 = paste0(str_sub(GISJOIN, 2, 3), str_sub(GISJOIN, 5, 7), str_sub(GISJOIN, 9, 14)),
         PROP_VACANT = PROP31 + PROP41 + PROP42 + PROP43 + PROP52 + PROP71 + PROP81 + 
           PROP82 + PROP90 + PROP95) %>% 
  dplyr::select(TRT_2015, PROP_VACANT) %>% 
  mutate(land_cover = TRUE)

# 
# land_cover %>% 
#   filter(str_sub(TRT_2015, 1, 2) == "06") %>% 
#   arrange(desc(PROP_VACANT)) %>% 
#   View()
# 
# land_cover %>% 
#   filter(str_sub(TRT_2015, 1, 2) == "06") %>% 
#   ggplot(aes(x = PROP_VACANT)) + geom_histogram()

combined <- full_join(combined, land_cover, 
                  by = c("TRT_2012" = "TRT_2015")) %>% 
  mutate(
    indicator = case_when(
      combined & land_cover ~ "both",
      combined ~ "combined",
      land_cover ~ "land_cover"
    ),
    .keep = "unused"
  ) %>% 
  assertr::verify(indicator != "combined") %>% 
  dplyr::filter(indicator == "both") %>% 
  dplyr::select(-indicator) %>% 
  mutate(combined = TRUE) %>% 
  rename(TRT_PROP_VACANT = PROP_VACANT)

combined %>% arrow::write_feather("data/combined_samp_slope.feather")