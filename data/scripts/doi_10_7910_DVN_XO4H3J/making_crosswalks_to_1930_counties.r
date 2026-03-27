dir.create(file.path("Data/_Intermediates"))
dir.create(file.path("Data/_Intermediates/census_place_project"))
dir.create(file.path("Data/_Intermediates/us_census_compact"))
dir.create(file.path("Data/_Intermediates/parish_gis_crosswalks"))
dir.create(file.path("Data/_Intermediates/parish_constituency_crosswalks"))
dir.create(file.path("Data/_Intermediates/uk_census_constituency_aggregates"))
dir.create(file.path("Data/_Clean"))

if (!require("pacman")) install.packages("pacman")
pacman::p_load(sf, data.table)

sf_1930 <- read_sf(
    paste0("Data/Shapefiles/nhgis0036_shapefile_tl2008_us_county_1930/",
        "US_county_1930_conflated.shp"))

sf_1930 <- st_simplify(sf_1930, dTolerance = 100, preserveTopology = TRUE)

sf_1930 <- st_buffer(sf_1930, 0)


sf_1930_y <- copy(sf_1930)
sf_1930_y$area <- as.numeric(st_area(sf_1930_y))
df_1930_y <- st_drop_geometry(sf_1930_y)

df_1930_y <- data.table(df_1930_y)[, .(NHGISST, NHGISCTY, NHGISNAM, ICPSRST,
    ICPSRCTY, ICPSRFIP, state_1930 = NHGISST,
    county_1930 = NHGISCTY, year = 1930, area, county_area = area, weight = 1)]

sf_1930 <- dplyr::select(sf_1930, state_1930 = NHGISST, county_1930 = NHGISCTY)

sf_1930$state_1930 <- as.numeric(sf_1930$state_1930)
sf_1930$county_1930 <- as.numeric(sf_1930$county_1930)


file_locs <- c(
    "nhgis0009_shapefile_tl2008_us_county_1880/US_county_1880_conflated.shp",
    "nhgis0003_shapefile_tl2008_us_county_1890/US_county_1890_conflated.shp",
    "nhgis0009_shapefile_tl2008_us_county_1900/US_county_1900_conflated.shp",
    "nhgis0009_shapefile_tl2008_us_county_1910/US_county_1910_conflated.shp",
    "nhgis0036_shapefile_tl2008_us_county_1920/US_county_1920_conflated.shp",
    "nhgis0050_shapefile_tl2008_us_county_1940/US_county_1940_conflated.shp",
    "nhgis0032_shapefile_tl2008_us_county_1950/US_county_1950_conflated.shp",
    "nhgis0062_shapefile_tl2008_us_county_1960/US_county_1960_conflated.shp",
    "nhgis0062_shapefile_tl2008_us_county_1970/US_county_1970_conflated.shp",
    "nhgis0062_shapefile_tl2008_us_county_1980/US_county_1980_conflated.shp",
    "nhgis0062_shapefile_tl2008_us_county_1990/US_county_1990_conflated.shp",
    "nhgis0062_shapefile_tl2008_us_county_2000/US_county_2000_conflated.shp")
file_locs <- paste0("Data/Shapefiles/", file_locs)
years <- seq(1880, 2000, by = 10)
years <- years[years != 1930]

sf_use_s2(FALSE)


create_county_crosswalk <- function(file_loc, year_no) {
    message(year_no)
    sf_year <- read_sf(file_loc)
    sf_year <- st_simplify(sf_year, dTolerance = 100, preserveTopology = TRUE)
    sf_year <- st_buffer(sf_year, 0)

    sf_intersect <- st_intersection(sf_1930, sf_year)
    sf_intersect$area <- as.numeric(st_area(sf_intersect))
    sf_intersect <- st_drop_geometry(sf_intersect)

    sf_intersect <- data.table(sf_intersect)[, .(NHGISST, NHGISCTY, NHGISNAM,
        ICPSRST, ICPSRCTY, ICPSRFIP, state_1930, county_1930,
        year = year_no, area)]

    sf_intersect[, county_area := sum(area), by = .(NHGISST, NHGISCTY)]
    sf_intersect[, weight := area / county_area]
    sf_intersect <- sf_intersect[weight > 0.01]
    sf_intersect[, county_area := sum(area), by = .(NHGISST, NHGISCTY)]
    sf_intersect[, weight := area / county_area]
}
df_cw <- lapply(1:length(file_locs), function(x){create_county_crosswalk(file_locs[x],
    years[x])}) %>% rbindlist()

df_cw <- rbind(df_cw, df_1930_y)

fwrite(df_cw, "Data/_Intermediates/crosswalks_to_1930_counties.csv")

