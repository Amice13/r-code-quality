# This script allocates UK census data to parliamentary constituencies

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, sf, dplyr, readxl, stringr, stringdist)


sf_parish <- st_read("Data/Shapefiles/EW Parishes 1911/EW1911_Parishes.shp")
head(sf_parish)
sf_county <- st_read("Data/Shapefiles/EW Administrative Counties 1911/EW1911_admcounties.shp")
head(sf_county)
sf_county <- dplyr::select(sf_county, county_name = G_NAME)

sf_parish <- st_join(st_buffer(sf_parish, 0), st_buffer(sf_county, 0), largest = TRUE)
head(sf_parish)


df_parish <- copy(sf_parish)
df_parish$geometry <- NULL
df_parish <- data.table(df_parish)
head(df_parish)




# Census data
df_census <- fread("Data/Census UK/1911/ipumsi_00018.csv")
head(df_census)
df_census <- df_census[, .(n = .N), by = .(countyuk = COUNTYUK, parishuk = PARISHUK_1911)]
head(df_census)

df_parish_names <- read_excel("Data/Census UK/UK_census_geographies.xlsx",
    sheet = "parishuk_1911")
df_parish_names <- data.table(df_parish_names)[,
    .(parishuk = PARISHUK_1911, parishuk_name)]


df_countyuk <- read_excel("Data/Census UK/UK_census_geographies.xlsx",
    sheet = "countyuk")
df_countyuk <- data.table(df_countyuk)[, .(countyuk = COUNTYUK, countyuk_name)]


df_gbhgis_countyuk <- read_excel("Data/Census UK/UK_census_geographies.xlsx",
    sheet = "countyuk_gbhgis")

df_gbhgis_countyuk <- data.table(df_gbhgis_countyuk)[, .(county_name = gbhgis_county, countyuk = COUNTYUK)]

df_parish <- merge(df_parish, df_gbhgis_countyuk, by = "county_name", all.x = TRUE)

df_census <- merge(df_census, df_parish_names, by = "parishuk", all.x = TRUE)

df_census[, parish_name_clean := str_replace(parishuk_name, " \\s*\\([^\\)]+\\)", "")]

df_census[, parish_name_clean := str_trim(tolower(parish_name_clean))]

df_parish[, parish_name_clean := tolower(G_NAME)]


# IPUMS data for 1911 combines Lancashire and Yorkshire
# and northeast counties
df_census[, county_combined := countyuk]
df_census[countyuk %in% c(35:39), county_combined := 35]
df_census[countyuk %in% c(40:43), county_combined := 40]

df_parish[, county_combined := countyuk]
df_parish[countyuk %in% c(35:39), county_combined := 35]
df_parish[countyuk %in% c(40:43), county_combined := 40]

# code to merge census parishes to GBHGIS parish shapefile
merge_seq <- function(df_census_sub, df_parish_sub) {


    match_1 <- amatch(df_census_sub$parish_name_clean, df_parish_sub$parish_name_clean,
        method = "jw", p = 0.1)

    df_census_sub[, G_UNIT_match1 := df_parish_sub$G_UNIT[match_1]]
    df_census_sub[, parish_name_match1 := df_parish_sub$parish_name_clean[match_1]]
    df_census_sub[, countyuk_match1 := df_parish_sub$countyuk[match_1]]
    df_census_unmatched <- df_census_sub[is.na(parish_name_match1)]
    df_census_sub <- df_census_sub[!is.na(parish_name_match1)]


    df_parish_unmatched <- df_parish_sub[!(G_UNIT %in% df_census_sub$G_UNIT_match1)]

    if(nrow(df_parish_unmatched) >= 1 & nrow(df_census_unmatched[nchar(parish_name_clean) > 5]) >= 1) {
        df_census_unmatched_l <- df_census_unmatched[nchar(parish_name_clean) > 5]
        for (x in 1:nrow(df_census_unmatched_l)) {
            x_in_y <- grepl(df_census_unmatched_l$parish_name_clean[x],
                df_parish_unmatched$parish_name_clean)

            if(sum(x_in_y, na.rm = TRUE) > 0) {
                df_census_unmatched_l$parish_name_match1[x] <- df_parish_unmatched$parish_name_clean[x_in_y][1]
                df_census_unmatched_l$G_UNIT_match1[x] <- df_parish_unmatched$G_UNIT[x_in_y][1]
                df_census_unmatched_l$countyuk_match1[x] <- df_parish_unmatched$countyuk[x_in_y][1]
            }
        }

        df_census_sub <- rbindlist(list(df_census_sub, df_census_unmatched_l[!is.na(G_UNIT_match1)]))
        df_census_unmatched <- df_census_unmatched[!(parishuk %in% df_census_sub$parishuk)]
        df_parish_unmatched <- df_parish_unmatched[!(G_UNIT %in% df_census_sub$G_UNIT_match1)]

    }

    if(nrow(df_parish_unmatched[nchar(parish_name_clean) >= 5]) >= 1 &
        nrow(df_census_unmatched) >= 1) {
        df_parish_unmatched_l <- df_parish_unmatched[nchar(parish_name_clean) > 5]
        for (x in 1:nrow(df_parish_unmatched_l)) {
            y_in_x <- grepl(df_parish_unmatched_l$parish_name_clean[x], df_census_unmatched$parish_name_clean)

            if(sum(y_in_x, na.rm = TRUE) > 0) {
                df_census_unmatched$parish_name_match1[y_in_x] <- df_parish_unmatched_l$parish_name_clean[x]
                df_census_unmatched$G_UNIT_match1[y_in_x] <- df_parish_unmatched_l$G_UNIT[x]
                df_census_unmatched$countyuk_match1[y_in_x] <- df_parish_unmatched_l$countyuk[x]
            }
        }
        df_census_sub <- rbindlist(list(df_census_sub, df_census_unmatched[!is.na(G_UNIT_match1)]))
        df_census_unmatched <- df_census_unmatched[is.na(G_UNIT_match1)]
        df_parish_unmatched <- df_parish_unmatched[!(G_UNIT %in% df_census_sub$G_UNIT_match1)]

    }



    reorder_string <- function(string) {
        string <- str_remove_all(string, "[[:punct:]]")
        splitted <- str_split(string, " ")[[1]]
        splitted <- sort(splitted)
        splitted <- paste(splitted, collapse = "")
    }
    if(nrow(df_parish_unmatched) >= 1 & nrow(df_census_unmatched) >= 1) {
        unmatched_census_parish_s <- sapply(df_census_unmatched$parish_name_clean, reorder_string)
        unmatched_gis_parish_s <- sapply(df_parish_unmatched$parish_name_clean, reorder_string)

        match_2 <- amatch(unmatched_census_parish_s, unmatched_gis_parish_s,
            method = "jw", p = 0.1)

        df_census_unmatched[, G_UNIT_match1 := df_parish_unmatched$G_UNIT[match_2]]
        df_census_unmatched[, parish_name_match1 := df_parish_unmatched$parish_name_clean[match_2]]
        df_census_unmatched[, countyuk_match1 := df_parish_unmatched$countyuk[match_2]]

        df_census_sub <- rbindlist(list(df_census_sub, df_census_unmatched[!is.na(G_UNIT_match1)]))
        df_census_unmatched <- df_census_unmatched[is.na(G_UNIT_match1)]
        df_parish_unmatched <- df_parish_unmatched[!(G_UNIT %in% df_census_sub$G_UNIT_match1)]
    }
    return(list(df_census_sub, df_census_unmatched, df_parish_unmatched))
}
subset_county_merge <- function(countyuk_no) {
    df_census_sub_c <- df_census[county_combined == countyuk_no]
    df_parish_sub_c <- df_parish[county_combined == countyuk_no]
    return(merge_seq(df_census_sub_c, df_parish_sub_c))
}


match_list <- lapply(unique(df_census[countyuk <= 56, county_combined]), subset_county_merge)

df_matches <- lapply(match_list, function(x){x[[1]]}) %>% rbindlist()

df_unmatched <- lapply(match_list, function(x){x[[2]]}) %>% rbindlist()
df_parish_unmatched <- lapply(match_list, function(x){x[[3]]}) %>% rbindlist()


bracket_counties <- str_extract_all(df_unmatched$parishuk_name,  "(?<=\\().+?(?=\\))")
bracket_counties1 <- lapply(bracket_counties, function(x){x[1]})
bracket_counties1 <- unlist(bracket_counties1)
bracket_counties2 <- lapply(bracket_counties, function(x){x[2]})
bracket_counties2 <- unlist(bracket_counties2)
str(bracket_counties1)
df_unmatched[, bracket_county_1 := bracket_counties1]
df_unmatched[, bracket_county_2 := bracket_counties2]



df_bracket_county <- read_excel("Data/Census UK/UK_census_geographies.xlsx",
    sheet = "parishuk_bracket_counties")


df_bracket_county <- data.table(df_bracket_county)[, .(bracket_county_1, countyuk_bracket = countyuk)]
df_unmatched[!(bracket_county_1 %in% df_bracket_county$bracket_county_1) & !is.na(bracket_county_1)]
df_unmatched <- merge(df_unmatched, df_bracket_county, by = "bracket_county_1",
    all.x = TRUE)
df_unmatched[bracket_county_2 == "Glamorgan", countyuk_bracket := 45]

subset_bracket_county_merge <- function(countyuk_no) {
    df_census_sub_c <- df_unmatched[countyuk_bracket == countyuk_no]
    df_parish_sub_c <- df_parish[countyuk == countyuk_no]
    return(merge_seq(df_census_sub_c, df_parish_sub_c))
}
bracket_matches <- lapply(unique(df_unmatched$countyuk_bracket), subset_bracket_county_merge)
df_bracket_matches <- lapply(bracket_matches, function(x){x[[1]]}) %>% rbindlist()
df_bracket_unmatched <- lapply(bracket_matches, function(x){x[[2]]}) %>% rbindlist()



df_unmatched <- df_unmatched[!(parishuk %in% df_bracket_matches$parishuk)]
sum(df_unmatched$n)

df_matches <- rbindlist(list(df_matches, df_bracket_matches), use.names = TRUE, fill = TRUE)


df_unmatched

df_parish_unmatched <- df_parish[!(G_UNIT %in% df_matches$G_UNIT_match1)]
merge_out <- merge_seq(df_unmatched, df_parish_unmatched)
df_matches <- rbindlist(list(df_matches, merge_out[[1]]), use.names = TRUE, fill = TRUE)
df_matches

df_matches_out <- df_matches[, .(parishuk_1911 = parishuk, parish_name = parishuk_name, countyuk, parish_g_unit = G_UNIT_match1,
    parish_name_gbhgis = parish_name_match1, countyuk_gbhgis = countyuk_match1, pop_1911 = n)]
fwrite(df_matches_out, "Data/_Intermediates/parish_gis_crosswalks/parish_gis_cw_1911.csv")



# post WWI constituencies


merge_parish_post_wwi_constituencies <- function(year_no, parish_shapefile) {


    sf_parish <- st_read(parish_shapefile)
    sf_constituency_e <- st_read(
        "Data/Shapefiles/E Constituencies 1918 1945/E1918_1945_Constituencies.shp")
    sf_constituency_w <- st_read(
        "Data/Shapefiles/W Constituencies 1918 1945/W1918_1945_Constituencies.shp")
    sf_constituency <- rbind(sf_constituency_e, sf_constituency_w)

    sf_parish <- dplyr::select(sf_parish, parish_g_unit = G_UNIT, parish_name_g_unit = G_NAME)
    head(sf_parish)
    sf_intersect <- st_intersection(st_buffer(sf_parish, 0),
        st_buffer(sf_constituency, 0))

    sf_intersect$area <- as.numeric(st_area(sf_intersect) / 1e6)
    df_join <- sf_intersect
    df_join$geometry <- NULL
    df_join <- data.table(df_join)[, .(parish_g_unit, parish_name_g_unit, g_unit = G_UNIT,
        g_name_gbhgis = G_NAME, area)]

    df_join[, parish_area := sum(area), by = .(parish_g_unit)]
    df_join[, parish_area_share := area / parish_area]
    df_join <- df_join[parish_area_share > 0.01]
    df_join[, parish_area := sum(area), by = .(parish_g_unit)]
    df_join[, parish_area_share := area / parish_area]

    df_const_pop <- fread("Data/Constituency Populations/const_pops_1911.csv")

    df_join <- merge(df_join, df_const_pop, by = "g_unit", all.x = TRUE)

    df_join[, constituency_area := sum(area), by = .(g_unit)]
    df_join[, density := pop_1911 / constituency_area]

    df_join[, density_pop := density * area]
    df_join[, density_pop_parish := sum(density_pop), by = .(parish_g_unit)]
    df_join[, density_pop_share := density_pop / density_pop_parish]
    df_join <- df_join[, .(parish_g_unit, parish_name_g_unit, g_unit, g_name, constituency_id,
        parish_area, share_of_parish = density_pop_share, constituency_area)]

    save_loc <- paste0("Data/_Intermediates/parish_constituency_crosswalks/gis_parish_",
        year_no, "_constituency_1918_1945.csv")
    fwrite(df_join, save_loc)
}
merge_parish_post_wwi_constituencies(1911, "Data/Shapefiles/EW Parishes 1911/EW1911_Parishes.shp")

merge_parish_pre_wwi_constituencies <- function(year_no, parish_shapefile) {


    sf_parish <- st_read(parish_shapefile)
    sf_constituency_e <- st_read(
        "Data/Shapefiles/E Constituencies 1885 1918/E1885_1918_Constituencies.shp")
    sf_constituency_w <- st_read(
        "Data/Shapefiles/W Constituencies 1885 1918/W1885_1918_Constituencies.shp")
    sf_constituency <- rbind(sf_constituency_e, sf_constituency_w)

    sf_parish <- dplyr::select(sf_parish, parish_g_unit = G_UNIT, parish_name_g_unit = G_NAME)
    head(sf_parish)
    sf_intersect <- st_intersection(st_buffer(sf_parish, 0),
        st_buffer(sf_constituency, 0))

    sf_intersect$area <- as.numeric(st_area(sf_intersect) / 1e6)
    df_join <- sf_intersect
    df_join$geometry <- NULL
    head(df_join)
    df_join <- data.table(df_join)[, .(parish_g_unit, parish_name_g_unit, g_unit,
        g_name_gbhgis = G_NAME, area)]

    df_join[, parish_area := sum(area), by = .(parish_g_unit)]
    df_join[, parish_area_share := area / parish_area]
    df_join <- df_join[parish_area_share > 0.01]
    df_join[, parish_area := sum(area), by = .(parish_g_unit)]
    df_join[, parish_area_share := area / parish_area]


    df_const_pop <- fread("Data/Constituency Populations/const_pops_prewar.csv")
    head(df_const_pop)
    pop_year <- paste0("Population ", year_no)
    if (year_no > 1911) {
        pop_year <- "Population 1911"
    }
    if (year_no < 1891) {
        pop_year <- "Population 1891"
    }
    df_const_pop <- df_const_pop[, c("constituency_id", pop_year), with = FALSE]
    colnames(df_const_pop) <- c("constituency_id", "pop")
    df_const_codes <- fread("Data/Constituency Populations/const_codes_prewar.csv")
    head(df_const_codes)
    df_const_codes <- merge(df_const_codes[,
        .(g_unit, G_NAME, constituency_id, county_name)],
        df_const_pop, by = "constituency_id", all.x = TRUE)

    df_join <- merge(df_join, df_const_codes, by = "g_unit", all.x = TRUE)

    df_join[, constituency_area := sum(area), by = .(g_unit)]
    df_join[, density := pop / constituency_area]

    df_join[, density_pop := density * area]
    df_join[, density_pop_parish := sum(density_pop), by = .(parish_g_unit)]
    df_join[, density_pop_share := density_pop / density_pop_parish]
    df_join <- df_join[, .(parish_g_unit, parish_name_g_unit, g_unit, g_name = G_NAME,
        constituency_id,
        parish_area, share_of_parish = density_pop_share, constituency_area)]

    save_loc <- paste0("Data/_Intermediates/parish_constituency_crosswalks/gis_parish_",
        year_no, "_constituency_1885_1918.csv")
    fwrite(df_join, save_loc)
}
merge_parish_pre_wwi_constituencies(1911, "Data/Shapefiles/EW Parishes 1911/EW1911_Parishes.shp")

# now process census

process_census <- function(year_no, census_file) {
    df_census <- fread(census_file)

    colnames(df_census) <- tolower(colnames(df_census))
    dim(df_census)
    parish_var <- paste0("parishuk_", year_no)
    colnames(df_census)[colnames(df_census) == parish_var] <- "parishuk"
    df_census <- df_census[, .(n = .N), by = .(parishuk, occicem)]

    df_census[occicem == 999, occicem := NA]
    agric_jobs <- 173:193
    nat_gov_jobs <- 1:10
    nat_gov_nonpostal_jobs <- 5:10
    

    df_pst <- read_excel("Data/PST/I-CeMOccode_1881EW_51EA_17EA_PST_SIC Final.xlsx")
    df_pst <- data.table(df_pst)[, .(occicem = OCCODE, pst_sector = PSTsector,
        pst_group = PSTgroup, cat17 = `17cat`)]

    df_census <- merge(df_census, df_pst, by = "occicem", all.x = TRUE)

    df_class <- read_excel("Data/Class categories/occicem_class.xlsx") %>%
        data.table()

    df_census <- merge(df_census, df_class, by = "occicem", all.x = TRUE)

    df_parishuk_gbhgis_parish <- fread(
        paste0("Data/_Intermediates/parish_gis_crosswalks/parish_gis_cw_",
            year_no, ".csv"))
    head(df_parishuk_gbhgis_parish)
    colnames(df_parishuk_gbhgis_parish)[
        colnames(df_parishuk_gbhgis_parish) == parish_var] <- "parishuk"
    df_census <- merge(df_census, df_parishuk_gbhgis_parish[,
        .(parishuk, parish_g_unit, countyuk_gbhgis)],
        by = "parishuk")



    df_census <- df_census[!is.na(pst_group), .(n = sum(n),
        agric = weighted.mean(pst_group == "Agriculture", n, na.rm = TRUE),
        primary = weighted.mean(pst_sector == "PRIMARY", n, na.rm = TRUE),
        mining = weighted.mean(pst_group == "Mining and quarrying", n, na.rm = TRUE),
        mf = weighted.mean(cat17 == "Mf", n, na.rm = TRUE),
        mining17 = weighted.mean(cat17 == "Mining & quarrying", n, na.rm = TRUE),
        secondary = weighted.mean(pst_sector == "SECONDARY", n, na.rm = TRUE),
        construction = weighted.mean(pst_group == "Building and construction", n, na.rm = TRUE),
        textiles = weighted.mean(pst_group == "Textiles", n, na.rm = TRUE),
        metals = weighted.mean(
            pst_group %in% c("Non-ferrous metal manufacture and products",
                "Iron and steel manufacture and products"), n * !is.na(pst_group), na.rm = TRUE),
        machinery = weighted.mean(pst_group == "Machines and tools, making and operation",
            n, na.rm = TRUE),
        shipbuilding = weighted.mean(pst_group == "Boat and ship building", n, na.rm = TRUE),
        nat_gov = weighted.mean(pst_group == "National government service", n, na.rm = TRUE),
        local_gov = weighted.mean(pst_group == "Local government service", n, na.rm = TRUE),
        n_class = sum(n[!is.na(rg_class_1913)]),
        working_class = weighted.mean(rg_class_1913 > 2, n * !is.na(rg_class_1913), na.rm = TRUE),
        unskilled = weighted.mean(rg_class_1913 == 5, n, na.rm = TRUE)),
        by = .(parish_g_unit, countyuk_gbhgis)]


    df_cw_post <- fread(paste0("Data/_Intermediates/parish_constituency_crosswalks/",
        "gis_parish_", year_no, "_constituency_1918_1945.csv"))

    df_census_post <- merge(df_census, df_cw_post, by = "parish_g_unit", all.x = TRUE)

    df_census_post[, weight := share_of_parish * n]

    df_census_post_out <- df_census_post[,
        .(
        n_census = sum(weight),
        agric = weighted.mean(agric, weight, na.rm = TRUE),
        primary = weighted.mean(primary, weight, na.rm = TRUE),
        mining = weighted.mean(mining, weight, na.rm = TRUE),
        mining17 = weighted.mean(mining17, weight, na.rm = TRUE),
        mf = weighted.mean(mf, weight, na.rm = TRUE),
        secondary = weighted.mean(secondary, weight, na.rm = TRUE),
        construction = weighted.mean(construction, weight, na.rm = TRUE),
        textiles = weighted.mean(textiles, weight, na.rm = TRUE),
        metals = weighted.mean(metals, weight, na.rm = TRUE),
        machinery = weighted.mean(machinery, weight, na.rm = TRUE),
        shipbuilding = weighted.mean(shipbuilding, weight, na.rm = TRUE),
        nat_gov = weighted.mean(nat_gov, weight, na.rm = TRUE),
        local_gov = weighted.mean(local_gov, weight, na.rm = TRUE),
        working_class = weighted.mean(working_class,
            share_of_parish * n_class, na.rm = TRUE),
        unskilled = weighted.mean(unskilled,
            share_of_parish * n_class, na.rm = TRUE)
        ),
        by = .(g_unit, g_name, constituency_id, constituency_area)]
    fwrite(df_census_post_out,
        paste0("Data/_Intermediates/uk_census_constituency_aggregates/census_", year_no, "_by_1918_constituencies.csv"))

    df_cw_pre <- fread(paste0("Data/_Intermediates/parish_constituency_crosswalks/",
        "gis_parish_", year_no, "_constituency_1885_1918.csv"))

    df_census_pre <- merge(df_census, df_cw_pre, by = "parish_g_unit", all.x = TRUE)

    df_census_pre[, weight := share_of_parish * n]

    df_census_pre_out <- df_census_pre[,
        .(
        n_census = sum(weight),
        agric = weighted.mean(agric, weight, na.rm = TRUE),
        primary = weighted.mean(primary, weight, na.rm = TRUE),
        mining = weighted.mean(mining, weight, na.rm = TRUE),
        mining17 = weighted.mean(mining17, weight, na.rm = TRUE),
        mf = weighted.mean(mf, weight, na.rm = TRUE),
        secondary = weighted.mean(secondary, weight, na.rm = TRUE),
        construction = weighted.mean(construction, weight, na.rm = TRUE),
        textiles = weighted.mean(textiles, weight, na.rm = TRUE),
        metals = weighted.mean(metals, weight, na.rm = TRUE),
        machinery = weighted.mean(machinery, weight, na.rm = TRUE),
        shipbuilding = weighted.mean(shipbuilding, weight, na.rm = TRUE),
        nat_gov = weighted.mean(nat_gov, weight, na.rm = TRUE),
        local_gov = weighted.mean(local_gov, weight, na.rm = TRUE),
        working_class = weighted.mean(working_class,
            share_of_parish * n_class, na.rm = TRUE),
        unskilled = weighted.mean(unskilled,
            share_of_parish * n_class, na.rm = TRUE)
        ),
        by = .(g_unit, g_name, constituency_id, constituency_area)]
    fwrite(df_census_pre_out,
        paste0("Data/_Intermediates/uk_census_constituency_aggregates/census_", year_no, "_by_1885_constituencies.csv"))
}


process_census(year_no = 1911,
    census_file = "Data/Census UK/1911/ipumsi_00018.csv")
