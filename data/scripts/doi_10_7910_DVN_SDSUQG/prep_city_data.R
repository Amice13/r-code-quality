library(sf)

#' Load city census block data
#'
#' @param path the path to the RDS file to cache/save
#' @param cities a vector of city keys to get
#' @param block_path the path to the CSV file of partisanship block counts
#' @param simplify `TRUE` to simplify geometry. Requires
#'   [rmapshaper][rmapshaper::rmapshaper].
#'
#' @returns A tibble, with each row a census block, and containing demographic,
#' partisan, and other information on each block.
get_block_data = function(path = here("data/block_data.rds"),
                          cities = c("new-york", "miami", "phoenix"),
                          block_path = here("data-raw/pid-states-block-partisan-counts.csv"),
                          geom_path = here("data/block_geometry.rds"),
                          school_path = here("data-raw/Public_Schools-shp/PublicSchools.shp"),
                          church_path = here("data-raw/All_Places_Of_Worship-shp/All_Places_Of_Worship.shp"),
                          simplify = TRUE) {
    if (file.exists(path)) {
        read_rds(path) %>%
            filter(city %in% cities)
    } else {
        cli::cli_process_start("Reading block partisanship information")
        part_d = read_csv(block_path, col_types="cddd") %>%
            rename(GEOID = vb.tsmart_census_id) %>%
            mutate(GEOID = if_else(str_length(GEOID) == 15, GEOID, str_c("0", GEOID)),
                   pct_dem = (democrats+0.5)/(registrants+1))
        cli::cli_process_done()

        cli::cli_process_start("Reading church and school shapefiles.")
        school_d = read_sf(school_path) %>%
            filter(STATE %in% c("NY", "NJ", "FL", "AZ")) %>%
            mutate(type="school") %>%
            select(state=STATE, type, geometry) %>%
            st_transform(4269)
        church_d = read_sf(church_path) %>%
            filter(STATE %in% c("NY", "NJ", "FL", "AZ")) %>%
            mutate(type="church") %>%
            select(state=STATE, type, geometry) %>%
            st_transform(4269)
        poi_d = bind_rows(school_d, church_d)
        cli::cli_process_done()

        cli::cli_process_start("Downloading rail data.")
        rail_d = tigris::rails() %>%
            st_cast("MULTILINESTRING") %>%
            st_geometry() %>%
            suppressMessages() %>%
            suppressWarnings()
        cli::cli_process_done()

        names(cities) = cities
        d = map_dfr(cities, get_city_data, part_d, poi_d, rail_d)
        if (FALSE && simplify) {
            if (requireNamespace("rmapshaper", quietly=T))
                d$geometry = rmapshaper::ms_simplify(d$geometry, keep_shapes=TRUE)
            else
                stop("Must install `rmapshaper` package or set `simplify=FALSE`.")
        }

        cli::cli_process_start("Calculating distance to nearest features.")
        school_pts = s2::as_s2_geography(st_geometry(school_d))
        church_pts = s2::as_s2_geography(st_geometry(church_d))
        centroids = s2::as_s2_geography(d$centroid)
        d$dist_school = s2_distance(centroids, school_pts[
            s2_closest_feature(centroids, school_pts)])
        d$dist_church = s2_distance(centroids, church_pts[
            s2_closest_feature(centroids, church_pts)])
        cli::cli_process_done()

        select(d, city, fips, geometry) %>%
            write_rds(geom_path, compress="xz")
        sf::st_drop_geometry(d) %>%
            write_rds(path, compress="xz")

        sf::st_drop_geometry(d)
    }
}

#' Load city census block geography
#'
#' @param path the path to the RDS file to cache/save
#'
#' @returns An [sf::sf] object, with each row a census block
get_block_geom = function(path = here("data/block_geometry.rds")) {
    if (file.exists(path)) {
        read_rds(path)
    } else {
        stop("Use `get_block_data` first.")
    }
}

#' Load census block data for one city
#'
#' @param city a city key
#' @param part_d a data frame containing the partisan information
#'
#' @returns A tibble, with each row a census block, and containing demographic,
#' partisan, and other information on each block.
get_city_data = function(city, part_d, poi_d, rail_d) {
    COUNTIES_NY = c("New York", "Bronx", "Kings", "Queens", "Richmond",
                    "Westchester", "Rockland", "Putnam", "Nassau")
    COUNTIES_NJ = c("Hudson", "Bergen", "Passaic") # FIPS 17, 03, 31
    COUNTIES_FL = c("Miami-Dade", "Broward", "Palm Beach")
    COUNTIES_AZ = c("Maricopa", "Pinal")

    vars = c(pop="P009001", pop_white="P009005", pop_black="P009006",
             pop_hisp="P009002")

    cli::cli_process_start("Downloading block-level Census variables for {city}")
    suppressWarnings(suppressMessages(
    if (city == "new-york") {
        d_ny = get_decennial("block", variables=vars, state="NY",
                             county=COUNTIES_NY, output="wide", geometry=TRUE)
        d_nj = get_decennial("block", variables=vars, state="NJ",
                             county=COUNTIES_NJ, output="wide", geometry=TRUE)
        d = bind_rows(d_ny, d_nj)

        feat_d = bind_rows(get_features("NY"),
                           get_features("NJ"),
                           select(filter(poi_d, state %in% c("NY", "NJ")), -state)) %>%
            st_crop(st_bbox(d))
    } else if (city == "miami") {
        d = get_decennial("block", variables=vars, state="FL",
                          county=COUNTIES_FL, output="wide", geometry=TRUE)
        feat_d = bind_rows(get_features("FL"),
                           select(filter(poi_d, state=="FL"), -state)) %>%
            st_crop(st_bbox(d))
    } else if (city == "phoenix") {
        d = get_decennial("block", variables=vars, state="AZ",
                          county=COUNTIES_AZ, output="wide", geometry=TRUE)
        feat_d = bind_rows(get_features("AZ"),
                           select(filter(poi_d, state=="AZ"), -state)) %>%
            st_crop(st_bbox(d))
    } else {
        stop("City `", city, "` not supported.")
    }))
    cli::cli_process_done()

    acs_vars = c(med_inc = "B19013_001E",
                 educ_tot = "B15003_001E",
                 educ_1 = "B15003_021E",
                 educ_2 = "B15003_022E",
                 educ_3 = "B15003_023E",
                 educ_4 = "B15003_024E",
                 educ_5 = "B15003_025E",
                 hh = "B25003_001E",
                 homeowners = "B25003_002E")
    cli::cli_process_start("Downloading block-group-level Census variables for {city}")
    suppressWarnings(suppressMessages(
    if (city == "new-york") {
        d_ny = get_acs("block group", variables=acs_vars, state="NY",
                       county=COUNTIES_NY, output="wide")
        d_nj = get_acs("block group", variables=acs_vars, state="NJ",
                       county=COUNTIES_NJ, output="wide")
        bg_d = bind_rows(d_ny, d_nj)
    } else if (city == "miami") {
        bg_d = get_acs("block group", variables=acs_vars, state="FL",
                    county=COUNTIES_FL, output="wide")
    } else if (city == "phoenix") {
        bg_d = get_acs("block group", variables=acs_vars, state="AZ",
                    county=COUNTIES_AZ, output="wide")
    }))
    cli::cli_process_done()

    bg_d = bg_d %>%
        transmute(block_group=GEOID,
                  tract = str_sub(block_group, 1, 11),
                  county = str_sub(block_group, 1, 5),
                  med_inc = med_inc,
                  pct_college = (educ_1+educ_2+educ_3+educ_4+educ_5)/educ_tot,
                  pct_homeown = homeowners/hh) %>%
        group_by(tract) %>%
        mutate(med_inc = coalesce(med_inc, mean(med_inc, na.rm=T)),
               pct_college = coalesce(pct_college, mean(pct_college, na.rm=T)),
               pct_homeown = coalesce(pct_homeown, mean(pct_homeown, na.rm=T))) %>%
        group_by(county) %>%
        mutate(med_inc = coalesce(med_inc, mean(med_inc, na.rm=T)),
               pct_college = coalesce(pct_college, mean(pct_college, na.rm=T)),
               pct_homeown = coalesce(pct_homeown, mean(pct_homeown, na.rm=T))) %>%
        ungroup() %>%
        select(-tract, -county)

    cli::cli_process_start("Downloading road data and forming regions")
    suppressWarnings(suppressMessages({
        if (city == "new-york") {
            roads_d = bind_rows(tigris::primary_secondary_roads("NY"),
                                tigris::primary_secondary_roads("NJ"))
        } else if (city == "miami") {
            roads_d = tigris::primary_secondary_roads("FL")
        } else if (city == "phoenix") {
            roads_d = tigris::primary_secondary_roads("AZ")
        }
        roads_d = roads_d %>%
            st_crop(st_bbox(d)) %>%
            st_cast("MULTILINESTRING") %>%
            st_geometry()

        rr_d = c(roads_d, rail_d, st_cast(st_as_sfc(st_bbox(d)), "MULTILINESTRING"))
        road_regions = st_union(rr_d) %>%
            st_polygonize() %>%
            st_cast() %>%
            st_as_sf() %>%
            mutate(rr_id = as.factor(seq_len(n())))
    }))
    cli::cli_process_done()

    cli::cli_process_start("Joining road and rail regions")
    suppressWarnings(suppressMessages({
        d2 = st_join(st_centroid(d), road_regions)
        d$rr_id = d2$rr_id
        rm(d2)
    }))
    cli::cli_process_done()


    cli::cli_process_start("Joining local features")
    suppressWarnings(suppressMessages({
        d = st_join(d, feat_d, largest=TRUE)
    }))
    cli::cli_process_done()

    cli::cli_process_start("Calculating centroids")
    centroids = sf::st_as_sfc(s2_centroid(d)) # raw s2 objects don't serialize
    cli::cli_process_done()
    cli::cli_process_start("Calculating areas")
    areas = s2_area(d)
    cli::cli_process_done()

    left_join(d, part_d, by="GEOID") %>%
        rename(fips=GEOID) %>%
        mutate(city=city, block_group=str_sub(fips, 1, 12), .before=fips) %>%
        left_join(bg_d, by="block_group") %>%
        mutate(registrants = coalesce(registrants, 0),
               democrats = coalesce(democrats, 0),
               republicans = coalesce(republicans, 0),
               centroid = centroids,
               area = areas) %>%
        select(-NAME, -block_group)
}

# get tigris features
get_features = function(state) {
    feat_pt = tigris::landmarks(state, type="point", year=2019) %>%
        mutate(type = case_when(
            MTFCC %in% c("H2030", "H2040", "H2041", "H2051", "H2053",
                         "H3010", "H3013", "H3020") ~ "water",
            MTFCC %in% c("K2180", "K2181", "K2182", "K2183", "K2184", "K2185",
                         "K2186", "K2187", "K2188", "K2189", "K2190",
                         "K2561", "K2582") ~ "park",
            TRUE ~ NA_character_)) %>%
        filter(!is.na(type)) %>%
        select(type, geometry)
    feat_ar = tigris::landmarks(state, type="area", year=2019) %>%
        mutate(type = case_when(
            MTFCC %in% c("H2030", "H2040", "H2041", "H2051", "H2053",
                         "H3010", "H3013", "H3020") ~ "water",
            MTFCC %in% c("K2180", "K2181", "K2182", "K2183", "K2184", "K2185",
                         "K2186", "K2187", "K2188", "K2189", "K2190",
                         "K2561", "K2582") ~ "park",
            TRUE ~ NA_character_)) %>%
        filter(!is.na(type)) %>%
        select(type, geometry)
    bind_rows(feat_ar, feat_pt)
}
