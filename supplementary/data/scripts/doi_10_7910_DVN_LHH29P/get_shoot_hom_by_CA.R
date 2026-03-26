
# Package setup
if (!require("pacman")) install.packages("pacman", repos = "http://cran.us.r-project.org"); library(pacman);
pacman::p_load(yaml, arrow, readr, tidyr, argparse, here, stringr, dplyr, lubridate, sf, purrr, glue);

# Working directory and command line argument setup
current_script = "generate_tables_and_figures/gen_shoot_homs_per_capita/prepare_data_for_plot/src/get_shoot_hom_by_CA.R"
here::i_am(current_script)
source(here("R", "project_functions.R"))
task_dir = here(dirname(dirname(file.path(current_script))))
cl_args <- parse_make_args(c(
  "CLASSIFIED_VICTIMS",
  "SHOOTING_VICS",
  "HOMICIDE_VICS",
  "GEOCODES",
  "CONFIG_FILE",
  "OUT_DIR"
), inter_active = TRUE, task_dir = task_dir)


config <- yaml::read_yaml(cl_args$CONFIG_FILE)

# Get victimizations ----
# Filter to relevant victimizations
victims_sample <- read_feather(cl_args$CLASSIFIED_VICTIMS) %>%
  select(cluster, rd_no, earliest_event_date, all_of(config$victimization_classification)) %>%
  filter(year(earliest_event_date) %in% config$years) %>%
  filter(!!sym(config$victimization_classification) == 1) %>%
  mutate(month = rollback(earliest_event_date, roll_to_first = T))

# Get event addresses ----
shooting_vics <- read_feather(cl_args$SHOOTING_VICS) %>%
  rename(street_no = stnum,
         street_dir = stdir,
         street_nme = street,
         state = state_cd) %>%
  select(cluster, rd_no, street_no, street_dir, street_nme, city, state) %>%
  mutate(address_source = "shooting_vics")

homicide_vics <- read_feather(cl_args$HOMICIDE_VICS) %>%
  rename(street_dir = street_direction_cd,
         state = state_cd) %>%
  select(cluster, rd_no, street_no, street_dir, street_nme, city, state) %>%
  mutate(address_source = "homicide_vics")

all_addresses <- bind_rows(shooting_vics,
                           homicide_vics) %>%
  mutate(clean_address = toupper(paste(street_no, street_dir, street_nme))) %>%
  select(cluster, rd_no, clean_address, address_source)

# Add event addresses
victims_sample_w_address <- victims_sample %>%
  left_join(all_addresses, by = c("cluster", "rd_no"))

# Get geocodes ----
geocodes <- read_csv(cl_args$GEOCODES)

# Check if we need any geocodes for our shooting/homicide victims data data
victims_without_latlng <- victims_sample_w_address %>%
  anti_join(geocodes) %>%
  filter(!is.na(clean_address))

if (nrow(victims_without_latlng) > 0 & config$require_all_geocoded) {
  stop(paste0(nrow(victims_without_latlng), " victimizations are missing lat longs. "))
}

# Add geocodes to the victims data ----
victims_sample_w_geocodes <- victims_sample_w_address %>%
  inner_join(geocodes)

# Load/clean community area shapefile from the data portal
all_community_areas <- read_csv(config$community_area_shapefile) %>%
  rename(geometry = the_geom) %>%
  st_as_sf(wkt = "geometry",
           crs = 4326) %>%
  st_transform(crs = 3435)

# Spatial join shooting and homicides with the CA file above,
  # adding CA info to each shooting/homicide row (assuming the shooting took place within Chicago)
shoot_homicide_cas <- victims_sample_w_geocodes %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = F) %>%
  st_transform(crs = 3435) %>%
  st_join(all_community_areas, left = T, join = st_within)

# Collapse to the month level ----
shootings_by_month_ca <- shoot_homicide_cas %>%
  st_drop_geometry() %>%
  rename(ca = area_numbe, ca_name = community) %>%
  count(ca_name, ca, month) %>%
  arrange(ca)

# Save ----
if (!dir.exists(cl_args$OUT_DIR)) { dir.create(cl_args$OUT_DIR) }
shootings_by_month_ca %>%
  select(ca, ca_name, month, n) %>%
  write_csv(paste0(cl_args$OUT_DIR, "/shooting_and_homicide_vics_by_month_and_ca.csv"))
