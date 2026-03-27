if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, magrittr, stringr, readxl, haven, sf, asciiSetupReader)


# crosswalks 
df_cw <- fread("Data/_Intermediates/crosswalks_to_1930_counties.csv")
df_statecodes <- fread("Data/state_codes.csv")
df_cw <- merge(df_cw, df_statecodes[, .(ICPSRST, statefip)], by = "ICPSRST", all.x = TRUE)


df_cw[year >= 1980, ICPSRFIP := statefip * 1000 + ICPSRCTY / 10]
max(df_cw$year)

df_cw_2000 <- df_cw[year == 2000]
df_cw_2010 <- copy(df_cw_2000)
df_cw_2010[, year := 2010]
df_cw_2020 <- copy(df_cw_2000)
df_cw_2020[, year := 2020]

df_cw <- rbindlist(list(df_cw, df_cw_2010, df_cw_2020))




# New Deal spending data
df_nd <- read_excel("Data/New Deal Spending Fishback Kantor/101199-V1/New-Deal-Spending/nddist04.xls")
head(df_nd)
print(colnames(df_nd))
colnames(df_nd) <- tolower(colnames(df_nd))
df_nd <- data.table(df_nd)
colnames(df_nd) <- paste0("fk_", colnames(df_nd))
df_nd[, `:=`(ICPSRST = fk_state, ICPSRCTY = fk_ndcode)]
head(df_nd)

df_nd[, c("fk_stname", "fk_state", "fk_name", "fk_ndcode") := NULL]
head(df_nd)
df_nd <- merge(df_nd, df_cw[year == 1930, .(state_1930, county_1930,
        ICPSRST, ICPSRCTY, weight)][weight > 0.1],
        by = c("ICPSRST", "ICPSRCTY"))
df_nd[, c("weight", "ICPSRST", "ICPSRCTY") := NULL]



df_1930 <- fread("Data/NHGIS/nhgis0065_ds54_1930_county.csv")[, .(NHGISST = STATEA,
    NHGISCTY = COUNTYA, pop_employed_1930 = BEH001, pop_unemployed_1930 = BEH002,
    pop_layed_off_1930 = BEH003)]
df_ind30 <- fread("Data/NHGIS/nhgis0037_ds54_1930_county.csv")[,
    .(NHGISST = STATEA, NHGISCTY = COUNTYA, pop_1930 = BDP001,
    pop_10_plus_1930 = BDU001, pop_illiterate_1930 = BDV001,
    farm_output_crops_1930 = BEV001, farms_1930 = BEX001)]

df_city30 <- fread("Data/NHGIS/nhgis0054_ds54_1930_county.csv")[,
    .(NHGISST = STATEA, NHGISCTY = COUNTYA, pop_city_1930 = BD6001)]


df_infant_deaths_1932 <- fread("Data/NHGIS/nhgis0068_ds224_1932_county.csv")[, .(NHGISST = STATEA,
    NHGISCTY = COUNTYA, births_1932 = AF1U001, infant_deaths_1932 = AF1Z001)]



df_1930 <- merge(df_1930, df_ind30, by = c("NHGISST", "NHGISCTY"),
    all.x = TRUE, all.y = TRUE)
df_1930 <- merge(df_1930, df_city30, by = c("NHGISST", "NHGISCTY"),
    all.x = TRUE, all.y = TRUE)
df_1930 <- merge(df_1930, df_infant_deaths_1932, by = c("NHGISST", "NHGISCTY"),
    all.x = TRUE, all.y = TRUE)

df_1930 <- melt(df_1930, id.vars = c("NHGISST", "NHGISCTY"))

df_1930 <- merge(df_1930, df_cw[year == 1930], by = c("NHGISST", "NHGISCTY"),
    all.x = TRUE, allow.cartesian = TRUE)
df_1930 <- df_1930[, .(value = sum(value * weight)),
    by = .(state_1930, county_1930, variable)]
df_1930 <- dcast(df_1930, state_1930 + county_1930 ~ variable, value.var = "value")




df_spending <- read_excel(paste0("Data/New Deal Spending Fishback Kantor/",
    "101199-V1/Retail-Sales/retailsales_data.xls"))
head(df_spending)
df_spending <- data.table(df_spending)[, .(ICPSRST = STATE, ICPSRCTY = NDMTCODE,
    sales_pc_1935 = RRTSAP35, sales_pc_1933 = RRTSAP33,
    sales_pc_1929 = RRTSAP29)]




df_spending <- merge(df_spending, df_cw[year == 1930,
    .(state_1930, county_1930, ICPSRST, ICPSRCTY, weight)],
    by = c("ICPSRST", "ICPSRCTY"))



df_spending[, c("ICPSRST", "ICPSRCTY", "ndcode", "weight", "n_temp") := NULL]

read_bank_data <- function(year_no) {
    file_loc <- paste0("Data/NHGIS/nhgis0066_ds49_", year_no, "_county.csv")
    df_temp <- fread(file_loc)[, .(NHGISST = STATEA, NHGISCTY = COUNTYA,
        deposits = BCJ001)]
    df_temp[, year := year_no]
    return(df_temp)
}
df_banks <- lapply(c(1932, 1935, 1936), read_bank_data) %>%
    rbindlist()
head(df_banks)
df_banks[, deposits := deposits / 1000]

df_banks <- melt(df_banks, id.vars = c("NHGISST", "NHGISCTY", "year"))
head(df_banks)
df_banks[, varname := paste0(variable, "_", year)]
df_banks[, year := NULL]
df_banks <- merge(df_banks, df_cw[year == 1930], by = c("NHGISST", "NHGISCTY"))
df_banks <- df_banks[, .(value = sum(value * weight, na.rm = TRUE)),
    by = .(state_1930, county_1930, varname)]
df_banks <- dcast(df_banks, state_1930 + county_1930 ~ varname, value.var = "value")




sf_map <- read_sf(
    "Data/Shapefiles/nhgis0036_shapefile_tl2008_us_county_1930/US_county_1930_conflated.shp")
df_map <- copy(sf_map)
df_map$geometry <- NULL
df_map <- data.table(df_map)[, .(state_1930 = as.numeric(NHGISST), county_1930 = as.numeric(NHGISCTY))]
county_area <- st_area(sf_map)
head(county_area)
county_area <- as.numeric(county_area) / 1e6
df_map[, area_sq_km := county_area]

# potential for unionization based on industry mix
df_census_u <- fread("Data/_Intermediates/us_census_compact/census_1930_compact.csv")
df_census_u <- df_census_u[, .(n = sum(n)), by = .(stateicp, countyicp, ind1950)]

df_census_u <- merge(df_census_u, df_cw[year == 1930,
    .(stateicp = ICPSRST, countyicp = ICPSRCTY, state_1930, county_1930, weight)])
df_census_u <- df_census_u[, .(n = sum(n * weight)),
    by = .(state_1930, county_1930, ind1950)]
df_union_categories <- read_excel("Data/Unionization/Troy_unionization_industry.xlsx",
    sheet = "industry_categories")
df_union_categories <- data.table(df_union_categories)[,
    .(ind1950 = as.numeric(ind1950), industry)][!is.na(ind1950)]
df_census_u <- df_census_u[between(ind1950, 1, 994)]
df_census_u <- merge(df_census_u, df_union_categories, all.x = TRUE)
df_census_u <- df_census_u[, .(n = sum(n)), by = .(state_1930, county_1930, industry)]
df_census_u[, n_county_total := sum(n), by = .(state_1930, county_1930)]
df_census_u[, n_county_unionization := sum(n[!is.na(industry)]), by = .(state_1930, county_1930)]
df_union_rates <- read_excel("Data/Unionization/Troy_unionization_industry.xlsx",
    sheet = "union_figures")
df_union_rates <- data.table(df_union_rates)[!is.na(industry), .(industry, percent_1939, percent_1953)]
df_union_rates
df_census_u <- merge(df_census_u, df_union_rates, by = "industry",
    all.x = TRUE)
unique(df_census_u[is.na(percent_1939), industry])

df_unionization <- df_census_u[, .(union_potential_1939 = sum(percent_1939 * n / n_county_total,
            na.rm = TRUE)
        ),
        by = .(state_1930, county_1930)]

# employment in healthcare etc
df_census_emp <- fread("Data/_Intermediates/us_census_compact/census_1930_compact.csv")
df_census_emp <- df_census_emp[, .(n = sum(n)),
    by = .(stateicp, countyicp, ind1950, occ1950, age16, school, lit)]
print(colnames(df_census_emp))

df_census_emp <- merge(df_census_emp, df_cw[year == 1930,
    .(stateicp = ICPSRST, countyicp = ICPSRCTY, state_1930, county_1930, weight)])
df_census_emp <- df_census_emp[, .(n = sum(n * weight)),
    by = .(state_1930, county_1930, ind1950, occ1950, age16, school, lit)]

df_census_emp <- df_census_emp[, .(
    n_firemen_1930 = sum(n[occ1950 == 762]),
    n_school_children_1930 = sum(n[school == 2 & age16 == 0]),
    n_children_1930 = sum(n[age16 == 0]),
    n_police_1930 = sum(n[occ1950 == 773])),
    by = .(state_1930, county_1930)]

# 1930 covariates
df_1930_cov <- fread("Data/_Intermediates/us_census_compact/census_1930_compact.csv")
colnames(df_1930_cov)
df_1930_cov <- df_1930_cov[, .(n = sum(n)),
    by = .(stateicp, countyicp, ind1950, occ1950, race, bpl, urban, farm)]


df_1930_cov <- merge(df_1930_cov, df_cw[year == 1930,
    .(stateicp = ICPSRST, countyicp = ICPSRCTY, state_1930, county_1930, weight)])
df_1930_cov <- df_1930_cov[, .(n = sum(n * weight)),
    by = .(state_1930, county_1930, ind1950, occ1950, race, bpl, urban, farm)]


df_1930_cov[ind1950 %in% c(0, 995, 997, 998, 999), ind1950 := NA]
df_1930_cov[, ind_1_digit := substr(ind1950, 1, 1)]

df_1930_cov[bpl %in% c(950, 999), bpl := NA]

df_1930_cov[, foreign_born := 0]

df_1930_cov[bpl > 120, foreign_born := 1]



df_1930_cov[, rural := 1 * (urban == 1)]
df_1930_cov[urban == 0, rural := NA]


df_1930_cov <- df_1930_cov[, .(
    mf_1930 = weighted.mean(ind_1_digit %in% c(3, 4), n * !is.na(ind_1_digit), na.rm = T),
    rural_1930 = weighted.mean(rural, n * !is.na(rural), na.rm = TRUE),
    agric_1930 = weighted.mean(ind1950 == 105, n, na.rm = T),
    foreign_born_1930 = weighted.mean(foreign_born, n * !is.na(foreign_born), na.rm = T),
    white_1930 = weighted.mean(race == 1, n, na.rm = T),
    n_census_1930 = sum(n)),
    by = .(state_1930, county_1930)]


library(stringr)
# IWW data
df_locals <- read_excel("Data/IWW/IWW data.xlsx", sheet = "locals") %>%
        data.table()
head(df_locals)
df_locals[, active := str_extract(Local , "active 19[0-9][0-9]-19[0-9][0-9]")]
df_locals[is.na(active), active := str_extract(Local , "active 19[0-9][0-9]")]
head(df_locals)
years <- str_extract_all(df_locals$active, "19[0-9][0-9]")
head(years)
df_locals[, year1 := unlist(lapply(years, function(x){x[1]}))]
df_locals[, year2 := unlist(lapply(years, function(x){x[2]}))]
head(df_locals[is.na(year1)])
df_locals[Local == "Local No. 117 - Shoe Workers' Industrial Union (Shoes) Established in early 1916.",
    year1 := 1916]
df_locals[, place := paste0(str_squish(`City/town`), ", ",
    str_squish(`State/Province`))]
df_locals[, ]

df_strikes <- read_excel("Data/IWW/IWW data.xlsx", sheet = "strikes") %>%
    data.table()
head(df_strikes)
df_strikes[, place := paste0(str_squish(City), ", ", str_squish(State))]

df_locals[is.na(`City/town`)]

places <- unique(c(df_locals[!is.na(`City/town`) & !is.na(`State/Province`),
    place],
    df_strikes[!is.na(City) & !is.na(State), place]))




df_places <- data.table(place = places)

# this is the code used to geocode the IWW data. Note this requires an API key
geocoding <- function() {
    
    library(ggmap)
    key <- "INSERT API KEY HERE"
    register_google(key = key)
    getOption("ggmap")


    geocoded <- geocode(df_places[, place], output = "more")
    df_places <- cbind(df_places, geocoded)
    fwrite(df_places, "Data/IWW/IWW_places_geocoded.csv")
}

df_places <- fread("Data/IWW/IWW_places_geocoded.csv")



library(sf)
sf_places <- st_as_sf(subset(df_places, !is.na(lon)),
    coords = c("lon", "lat"), crs = 4326)
sf_places <- st_transform(sf_places, crs = 5070)

sf_counties <- st_read(paste0("Data/Shapefiles/",
    "nhgis0036_shapefile_tl2008_us_county_1930/",
    "US_county_1930_conflated.shp"))
sf_counties <- st_transform(sf_counties, crs = 5070)

joins <- st_join(sf_places, sf_counties, join = st_within)
head(joins)
colnames(sf_counties) %>% print()
df_joins <- data.table(joins)[, .(place, geocoding_address = address,
    NHGISNAM, NHGISST, NHGISCTY, ICPSRST, ICPSRCTY)]



df_joins <- merge(df_joins, df_locals[, .(n_locals = .N
    ), by = .(place)],
    by = "place", all.x = TRUE)
df_joins <- merge(df_joins, df_strikes[, .(n_strikes = .N
    ), by = .(place)],
    by = "place", all.x = TRUE)

df_joins <- df_joins[, .(n_locals = sum(n_locals, na.rm = TRUE),
    n_strikes = sum(n_strikes, na.rm = TRUE)
    ),
    by = .(state_1930 = as.numeric(NHGISST), county_1930 = as.numeric(NHGISCTY))]
df_iww_counties <- copy(sf_counties)
df_iww_counties$geometry <- NULL
df_iww_counties <- data.table(df_iww_counties)[, .(state_1930 = as.numeric(NHGISST),
    county_1930 = as.numeric(NHGISCTY))]
df_iww_counties <- merge(df_iww_counties, df_joins, by = c("state_1930", "county_1930"),
    all.x = TRUE)

df_iww_counties[is.na(n_locals), n_locals := 0]
df_iww_counties[is.na(n_strikes), n_strikes := 0]


df_ed <- read_dta(
    paste0("Data/Welfare Spending/",
        "Financial Statistics of State and Local Governments/pf1932.dta"))
df_ed <- data.table(df_ed)
unique(df_ed[, state_name]) %>% print()
df_ed <- df_ed[, .(ICPSRST = stateicp, ICPSRCTY = countyicp, total, total_op, general_op, police_op,
    health_op, highways_op, char_hosp_corr_op, education_op, libraries_op,
    recreation_op, devt_cons_op, misc_op, public_service, interest, outlays)]
colnames(df_ed)[3:ncol(df_ed)] <- paste0("sp_", colnames(df_ed)[3:ncol(df_ed)])


df_ed <- merge(df_ed, df_cw[year == 1930, .(ICPSRST, ICPSRCTY, state_1930, county_1930)],
    by = c("ICPSRST", "ICPSRCTY"))
df_ed[, c("ICPSRST", "ICPSRCTY") := NULL]




df_ed_va <- read_excel(paste0("Data/Welfare Spending/",
    "Financial Statistics of State and Local Governments/virginia_data_1932.xlsx"))
df_ed_va <- data.table(df_ed_va)
df_ed_va
df_ed_va[, c("NHGISNAM", "total_check", "total_op_check") := NULL]
colnames(df_ed_va)[3:ncol(df_ed_va)] <- paste0("sp_", colnames(df_ed_va)[3:ncol(df_ed_va)])


df_ed_wv <- read_excel(paste0("Data/Welfare Spending/",
    "Financial Statistics of State and Local Governments/west_virginia_data_1932.xlsx"))
df_ed_wv <- data.table(df_ed_wv)

df_ed_wv[, c("county_name", "total_check", "total_op_check") := NULL]
colnames(df_ed_wv)[3:ncol(df_ed_wv)] <- paste0("sp_", colnames(df_ed_wv)[3:ncol(df_ed_wv)])
df_ed <- rbindlist(list(df_ed, df_ed_wv, df_ed_va), use.names = TRUE, fill = TRUE)



df_places30 <- fread("Data/_Intermediates/census_place_project/places_populations_1930.csv")
sf_map30 <- st_read("Data/Shapefiles/nhgis0036_shapefile_tl2008_us_county_1930/US_county_1930_conflated.shp")
sf_places30 <- st_as_sf(df_places30[!is.na(lon)], coords = c("lon", "lat"))
st_crs(sf_places30) <- "WGS84"
sf_places30 <- st_transform(sf_places30, st_crs(sf_map))
sf_joined30 <- st_join(sf_places30, sf_map30)
dim(sf_joined30)
dim(df_places30[!is.na(lon)])
head(sf_joined30)
unjoined_places <- sf_joined30$cpp_placeid[is.na(sf_joined30$NHGISST)]
sf_joined30 <- sf_joined30[!is.na(sf_joined30$NHGISST), ]
sf_places_to_join30 <- sf_places30[sf_places30$cpp_placeid %in% unjoined_places, ]

sf_joined_302 <- st_join(sf_places_to_join30, sf_map30, join = st_nearest_feature)

sf_joined30 <- rbind(sf_joined30, sf_joined_302)
sf_joined30$geometry <- NULL
df_joins30 <- data.table(sf_joined30)[, .(cpp_placeid, state_1930 = as.numeric(NHGISST),
    county_1930 = as.numeric(NHGISCTY))]

head(df_places30)
df_places30 <- merge(df_places30, df_joins30, by = "cpp_placeid", all.x = TRUE)

df_places30[, n_county := sum(n), by = .(state_1930, county_1930)]
df_places30[, s_county := n / n_county]
alpha <- 0.078
df_place_size_1930 <- df_places30[!is.na(lat), .(
    agglomeration_1930 = sum(n ^ alpha * s_county)),
    by = .(state_1930, county_1930)]



df_combined <- merge(df_nd, df_1930,
    by = c("state_1930", "county_1930"),
    all.x = TRUE, all.y = TRUE)


df_combined <- merge(df_combined, df_map,
    by = c("state_1930", "county_1930"),
    all.x = TRUE, all.y = TRUE)
df_combined <- merge(df_combined, df_spending,
    by = c("state_1930", "county_1930"),
    all.x = TRUE, all.y = TRUE)
df_combined <- merge(df_combined, df_banks,
    by = c("state_1930", "county_1930"),
    all.x = TRUE, all.y = TRUE)
df_combined <- merge(df_combined, df_unionization,
    by = c("state_1930", "county_1930"),
    all.x = TRUE, all.y = TRUE)
df_combined <- merge(df_combined, df_iww_counties,
    by = c("state_1930", "county_1930"),
    all.x = TRUE, all.y = TRUE)
df_combined <- merge(df_combined, df_ed,
    by = c("state_1930", "county_1930"),
    all.x = TRUE, all.y = TRUE)
df_combined <- merge(df_combined, df_census_emp,
    by = c("state_1930", "county_1930"),
    all.x = TRUE, all.y = TRUE)
df_combined <- merge(df_combined, df_1930_cov,
    by = c("state_1930", "county_1930"),
    all.x = TRUE, all.y = TRUE)
df_combined <- merge(df_combined, df_place_size_1930,
    by = c("state_1930", "county_1930"),
    all.x = TRUE, all.y = TRUE)


southern <- df_statecodes[between(ICPSRST, 40, 59), NHGISST]
df_combined <- df_combined[!is.na(state_1930)]


df_combined[, ue_1930 := (pop_unemployed_1930 + pop_layed_off_1930) /
    (pop_employed_1930 + pop_unemployed_1930 + pop_layed_off_1930)]


df_combined[, farms_pc_1930 := farms_1930 / pop_1930]
df_combined[, ag_output_pc_1930 := ( (farm_output_crops_1930) / pop_1930)]

df_combined[, d_sales := (sales_pc_1935) - (sales_pc_1933)]
df_combined[, deposits_pc_1932 := deposits_1932 / pop_1930]
df_combined[, d_deposits_pc := (deposits_1936 - deposits_1932) / pop_1930]
df_combined[, illiteracy_rate_1930 := pop_illiterate_1930 / pop_10_plus_1930]


df_combined[, overhead_efficiency := (sp_police_op + sp_health_op + sp_char_hosp_corr_op +
    sp_education_op + sp_libraries_op + sp_recreation_op + sp_devt_cons_op ) / (sp_police_op + sp_health_op + sp_char_hosp_corr_op +
        sp_education_op + sp_libraries_op + sp_recreation_op + sp_devt_cons_op + sp_general_op + sp_misc_op)]

df_combined[is.na(overhead_efficiency), overhead_efficiency := (sp_police_op + sp_health_op + sp_char_hosp_corr_op +
    sp_education_op + sp_libraries_op + sp_recreation_op ) / (sp_police_op + sp_health_op + sp_char_hosp_corr_op +
        sp_education_op + sp_libraries_op + sp_recreation_op + sp_general_op + sp_misc_op)]
df_combined[, urban_1930 := 1 - rural_1930]


df_combined[, `:=`(
    ln_children_in_school = log( (1 + n_school_children_1930) / n_children_1930),
    ln_infant_mortality = log( (1 + infant_deaths_1932) / (1 + births_1932))
    
    )]



df_combined[, total_spend := sp_general_op + sp_police_op + sp_highways_op +
    sp_health_op + sp_char_hosp_corr_op + sp_education_op + sp_libraries_op
    + sp_recreation_op + sp_devt_cons_op + sp_misc_op]
df_combined[is.na(total_spend), total_spend := sp_general_op + sp_police_op + sp_highways_op +
    sp_health_op + sp_char_hosp_corr_op + sp_education_op + sp_libraries_op
    + sp_recreation_op +  sp_misc_op]


df_combined[, highway_spending_share := sp_highways_op / total_spend]

df_combined[, police_fire_percap := (n_police_1930 + n_firemen_1930) / n_census_1930]
df_combined[, police_fire_spend_percap := sp_police_op / pop_1930]
df_combined[, spend_percap := total_spend / pop_1930]

df_combined[, strikes_percap := n_strikes / pop_1930]
df_combined[, locals_percap := n_locals / pop_1930]
df_combined[, city_1930 := pop_city_1930 / pop_1930]
df_combined[, log_density := log(pop_1930 / area_sq_km)]

df_combined[, spend_nonhighway := total_spend - sp_highways_op]


df_combined[, general_gov_spend_percap := sp_general_op / pop_1930]
df_combined[, non_general_gov_spend_percap := (total_spend - sp_general_op) / pop_1930]



df_combined[, change_log_deposits_35_36 := log(deposits_1936 / deposits_1935)]
df_combined[, change_log_deposits_32_36 := log(deposits_1936 / deposits_1932)]
df_combined[, change_log_sales_33_35 := log(sales_pc_1935 / sales_pc_1933)]



df_combined[, `:=`(log_nd_fera_pc = log(fk_fera / pop_1930),
    log_nd_cwa_pc = log(fk_cwa / pop_1930),
    log_nd_wpa_pc = log(fk_wpa / pop_1930),
    log_nd_sspa_pc = log(fk_pubass / pop_1930),
    log_nd_pwa_pc = log((fk_pwaf + fk_pwanf1 + fk_pwanf2) / pop_1930),
    log_nd_pra_pc = log(fk_pra / pop_1930),
    log_nd_pba_pc = log(fk_pba / pop_1930),
    log_nd_aaa_pc = log(fk_aaa / pop_1930),
    log_nd_fca_pc = log(fk_fca / pop_1930),
    log_nd_fsa_pc = log((fk_fsarr2 + fk_fsalo) / pop_1930),
    log_nd_rural_elec_pc = log(fk_rea / pop_1930),
    log_nd_rfc_pc = log(fk_rfc / pop_1930),
    log_nd_holc_pc = log(fk_holc / pop_1930),
    log_nd_fha_pc = log(fk_ins / pop_1930),
    log_nd_usha_pc = log((fk_ushalc + fk_ushah) / pop_1930),
    log_nd_total_pc = log((fk_ndexp + fk_loan) / pop_1930)
    )]

colnames(df_combined)

to_drop <- c("fk_pcturb3"   , "fk_pop30", "fk_hten73b"   , "fk_pctblk30"  , "fk_pctfrmln",
    "fk_pctill30", "fk_pctfail"   , "fk_avesz"     , "fk_pctunem"   , "fk_invpop",                
    "fk_rfc"       , "fk_rea"       , "fk_holc"      , "fk_pba",                      
    "fk_wpa"       , "fk_fera"      , "fk_cwa"       , "fk_std9632",                  
    "fk_mean9628"  , "fk_ag311"     , "fk_app311"    , "fk_bkcr311" ,                 
    "fk_xxd311"    , "fk_flcn311"   , "fk_irrc311"   , "fk_lab311"  ,                 
    "fk_pbld311"   , "fk_pbln311"   , "fk_rvhb311"   , "fk_road311" ,               
    "fk_wymn311"   , "fk_pctvt32"   , "fk_rtsapc29"  , "fk_aaa",               
    "fk_relief"    , "fk_loan"      , "fk_ins"       , "fk_taxrtp29" ,            
    "fk_rrtsap29"  , "fk_rldf3329"  , "fk_areapop"   , "fk_roosmmn2",            
    "fk_pubass"    , "fk_pwaf"      , "fk_pwanf2"    , "fk_pwanf1",            
    "fk_pra"       , "fk_fca"       , "fk_fsarr2"    , "fk_fsalo",             
    "fk_ushalc"    , "fk_ushah"     , "fk_ndexp"     , "fk_pubwor",
    "pop_employed_1930", "pop_unemployed_1930", "pop_layed_off_1930", "pop_urban_1930",
    "pop_white_1930",  "pop_foreign_born_1930", "pop_other_races_1930", "pop_illiterate_1930", 
    "firms_1930", "employees_1930", "wages_1930", "value_materials_1930", "value_energy_1930",
    "value_products_1930", "value_added_1930" , "farm_output_crops_1930", "farms_1930",
    "land_farm_1930", "farm_value_1930", "farm_land_value_1930", "farm_building_value_1930",
    "farm_implement_value_1930", "farm_dwelling_value_1930", "pop_city_1930",
    "births_1932", "infant_deaths_1932", "area_sq_km", "sales_pc_1939", "sales_pc_1935",
    "sales_pc_1933", "banks_1932", "banks_1935", "banks_1936", 
    "deposits_1932", "deposits_1935", "deposits_1936",
    "n_locals", "n_strikes", 
    "sp_total" ,"sp_total_op" ,
    "sp_general_op"       ,"sp_police_op"       , "sp_health_op"       , "sp_highways_op",
    "sp_char_hosp_corr_op","sp_education_op"    , "sp_libraries_op"    , "sp_recreation_op",
    "sp_devt_cons_op"     ,"sp_misc_op"         , "sp_public_service"  , "sp_interest" ,
    "sp_outlays", "sp_per_capita", "n_police_1930", "n_firemen_1930", "n_school_children_1930",
    "n_children_1930", "pop_10_plus_1930"
      )
df_combined[, (to_drop) := NULL]



fwrite(df_combined, "Data/_Clean/us_county_cross_section.csv")

# Michigan emergency relief data
# load in michigan data
df_michigan <- read_excel("Data/Welfare spending/Michigan cost of emergency relief/michigan_cost_digitized.xlsx")
head(df_michigan)

df_michigan <- data.table(df_michigan)
df_michigan[, county_1930 := as.numeric(county_1930)]
df_michigan <- df_michigan[, .(state_1930, county_1930, admin_cost_ratio = total_admin_cost / total_obligations)]
fwrite(df_michigan, "Data/_Clean/Michigan_emergency_relief_data.csv")




# Southern voting data



read_southern_primary <- function(index) {
    pad_index <- str_pad(index, 4, side = "left", pad = "0")
    base_path <- paste0("Data/Electoral data US/ICPSR_00071/DS",
        pad_index, "/00071-", pad_index)
    data_path <- paste0(base_path, "-Data.txt")
    setup_path <- paste0(base_path, "-Setup-edited.sas")
    if(!file.exists(setup_path)) {
        setup_path <- paste0(base_path, "-Setup.sas")
    }
    


    print(index)
    df <- read_ascii_setup(
        data = data_path,
        setup_file = setup_path,
        use_value_labels = FALSE, use_clean_names = TRUE) %>% data.table()
    print(colnames(df)[1:3])
    head(df)
    unique(df$ICPR_STATE_CODE)
    print(colnames(df))
    df <- melt(df,
        id.vars = c("ICPR_STATE_CODE", "COUNTY_OR_STATE_NAME", "DATA_TYPE")
    )
    return(df)

}


# Georgia
df_GA <- read_southern_primary(5)
head(df_GA)
df_GA[, .N, variable]
df_GA[, year := as.numeric(substr(as.character(variable), 2, 3)) + 1900]
df_GA[year > 1990, year := year - 100]
df_GA[, year_floor := floor(year / 10) * 10]
unique(df_GA$ICPR_STATE_CODE)


df_GA[, ICPR_STATE_CODE := 44]
df_GA

df_GA[, dem_g_primary := str_detect(variable, "GOV_P_D")]
df_GA[, dem_g_runoff := str_detect(variable, "GOV_P_RO_D")]

df_GA[COUNTY_OR_STATE_NAME == "DE KALB", COUNTY_OR_STATE_NAME := "DEKALB"]
df_GA[COUNTY_OR_STATE_NAME == "BARTOW/CASS", COUNTY_OR_STATE_NAME := "BARTOW"]


df_GA <- merge(df_GA, df_cw[, .(year_floor = year, ICPR_STATE_CODE = ICPSRST,
    COUNTY_OR_STATE_NAME = toupper(NHGISNAM), state_1930, county_1930, weight)],
    by = c("year_floor", "ICPR_STATE_CODE", "COUNTY_OR_STATE_NAME"),
    all.x = TRUE, allow.cartesian = TRUE)
unique(df_GA[is.na(weight) & year == 1932, COUNTY_OR_STATE_NAME])
#df_cw[ICPSRST == 44, .N, NHGISNAM] %>% print(., nrow = 200)
df_GA <- df_GA[, .(value = sum(value * weight)), by = .(state_1930, county_1930, year, variable, dem_g_primary, dem_g_runoff)]
df_GA <- df_GA[dem_g_primary == TRUE | dem_g_runoff == TRUE]
df_GA[, event := paste(year, dem_g_primary, dem_g_runoff)]
df_GA[, total_vote := sum(value, na.rm = TRUE), by = .(event, state_1930, county_1930)]
df_GA[, vote_share := value / total_vote]
df_GA
unique(df_GA$variable)
talmadge_candidates <- c("X32_GOV_P_D_TALMADGE", "X34_GOV_P_D_TALMADGE",
    "X36_GOV_P_D_REDWINE", "X38_GOV_P_D_HOWELL", "X40_GOV_P_D_TALMADGE",
    "X42_GOV_P_D_TALMADGE", "X46_GOV_P_D_TALMADGE")

df_GA[, talmadge := variable %in% talmadge_candidates]
df_GA <- df_GA[talmadge == TRUE][between(year, 1932, 1945), .(state_1930, county_1930, year, dem_g_primary, dem_g_runoff, event, talmadge_vote_share = vote_share)]
fwrite(df_GA, "Data/_Clean/voting_GA.csv")

# in 1936, Rivers successfully ran as the pro-New Deal candidates against Talmadge's candidate Redwine
# in 1938, less clear who the anti-New Deal candidate was. Main opponent to Rivers was Howell, who had worked for Talmadge

df_NC <- read_southern_primary(8)
df_NC[, .N, variable]
df_NC[, year := as.numeric(substr(as.character(variable), 2, 3)) + 1900]
df_NC[year > 1990, year := year - 100]
df_NC[, year_floor := floor(year / 10) * 10]
unique(df_NC$ICPR_STATE_CODE)


df_NC[, ICPR_STATE_CODE := 47]
df_NC

df_NC[, dem_g_primary := str_detect(variable, "GOV_P_D")]
df_NC[, dem_g_runoff := str_detect(variable, "GOV_P_RO_D")]


df_NC <- merge(df_NC, df_cw[, .(year_floor = year, ICPR_STATE_CODE = ICPSRST,
    COUNTY_OR_STATE_NAME = toupper(NHGISNAM), state_1930, county_1930, weight)],
    by = c("year_floor", "ICPR_STATE_CODE", "COUNTY_OR_STATE_NAME"),
    all.x = TRUE, allow.cartesian = TRUE)
unique(df_NC[is.na(weight) & between(year, 1932, 1940), COUNTY_OR_STATE_NAME])
df_NC <- df_NC[, .(value = sum(value * weight)), by = .(state_1930, county_1930, year, variable, dem_g_primary, dem_g_runoff)]
df_NC <- df_NC[dem_g_primary == TRUE | dem_g_runoff == TRUE]
df_NC[, event := paste(year, dem_g_primary, dem_g_runoff)]
df_NC[, total_vote := sum(value, na.rm = TRUE), by = .(event, state_1930, county_1930)]
df_NC[, vote_share := value / total_vote]
df_NC
unique(df_NC$variable)
shelby_candidates <- c("X32_GOV_P_D_EHRINGHAUS", "X32_GOV_P_RO_D_ERINGHAUS", "X36_GOV_P_D_HOEY",
    "X36_GOV_P_RO_D_HOEY", "X40_GOV_P_D_BROUGHTON", "X44_GOV_P_D_CHERRY")

df_NC[, shelby := variable %in% shelby_candidates]
df_NC <- df_NC[shelby == TRUE][between(year, 1932, 1945), .(state_1930, county_1930, year,
    dem_g_primary, dem_g_runoff, event, shelby_vote_share = vote_share)]
fwrite(df_NC, "Data/_Clean/voting_NC.csv")



# TN

df_TN <- read_southern_primary(11)
df_TN[, .N, variable]
df_TN[, year := as.numeric(substr(as.character(variable), 2, 3)) + 1900]
df_TN[year > 1990, year := year - 100]
df_TN[, year_floor := floor(year / 10) * 10]
unique(df_TN$ICPR_STATE_CODE)


df_TN[, dem_g_primary := str_detect(variable, "GOV_P_D")]
df_TN[, dem_g_runoff := str_detect(variable, "GOV_P_RO_D")]


df_TN <- merge(df_TN, df_cw[, .(year_floor = year, ICPR_STATE_CODE = ICPSRST,
    COUNTY_OR_STATE_NAME = toupper(NHGISNAM), state_1930, county_1930, weight)],
    by = c("year_floor", "ICPR_STATE_CODE", "COUNTY_OR_STATE_NAME"),
    all.x = TRUE, allow.cartesian = TRUE)
unique(df_TN[is.na(weight) & between(year, 1932, 1940), COUNTY_OR_STATE_NAME])
df_TN <- df_TN[, .(value = sum(value * weight)), by = .(state_1930, county_1930, year, variable, dem_g_primary, dem_g_runoff)]
df_TN <- df_TN[dem_g_primary == TRUE | dem_g_runoff == TRUE]
df_TN[, event := paste(year, dem_g_primary, dem_g_runoff)]
df_TN[, total_vote := sum(value, na.rm = TRUE), by = .(event, state_1930, county_1930)]
df_TN[, vote_share := value / total_vote]
df_TN
unique(df_TN$variable)
crump_candidates <- c("X26_GOV_P_D_MCALISTER", "X28_GOV_P_D_MCALISTER",
    "X32_GOV_P_D_MCALISTER", "X34_GOV_P_D_MCALISTER", "X36_GOV_P_D_BROWNING",
    "X38_GOV_P_D_COOPER", "X40_GOV_P_D_COOPER", "X42_GOV_P_D_COOPER", "X44_GOV_P_D_MCCORD")
# 1930: Crump did not take a side, https://en.wikipedia.org/wiki/Henry_Hollis_Horton
# 1936: Crump endorsed Browning (Key p 63)
# 1938: Crump backed Cooper https://en.wikipedia.org/wiki/Gordon_Browning
df_TN[, crump := variable %in% crump_candidates]

df_TN <- df_TN[crump == TRUE][between(year, 1932, 1945), .(state_1930, county_1930, year,
    dem_g_primary, dem_g_runoff, event, crump_vote_share = vote_share)]

fwrite(df_TN, "Data/_Clean/voting_TN.csv")



# Key considered 4 states to have organized factional politics: VA, NC, GA, TN.
# In VA, NC, GA, machine was anti-ND. In TN pro-ND.

# VA
df_VA <- read_southern_primary(1)
df_VA[, year := as.numeric(substr(as.character(variable), 2, 3)) + 1900]
df_VA[year > 1990, year := year - 100]
df_VA[, year_floor := floor(year / 10) * 10]
unique(df_VA$ICPR_STATE_CODE)

df_VA[, ICPR_STATE_CODE := 40]
df_VA[COUNTY_OR_STATE_NAME == "ARLINGTON/ALEXAND", COUNTY_OR_STATE_NAME := "ARLINGTON"]
df_VA[COUNTY_OR_STATE_NAME == "MATHEWS", COUNTY_OR_STATE_NAME := "MATTHEWS"]
df_VA[COUNTY_OR_STATE_NAME %in% c("BRISTOL", "BUENA VISTA", "CHARLOTTESVILLE",
    "CLIFTON FORGE", "DANVILLE", "FREDERICKSBURG", "HAMPTON", "HARRISONBURG",
    "HOPEWELL", "LYNCHBURG", "MARTINSVILLE", "NEWPORT NEWS", "PETERSBURG",
    "PORTSMOUTH", "RADFORD", "SOUTH NORFOLK", "STAUNTON", "SUFFOLK",
    "WILLIAMSBURG", "WINCHESTER", "FALLS CHURCH", "WAYNESBORO"),
    COUNTY_OR_STATE_NAME := paste0(COUNTY_OR_STATE_NAME, " CITY")]



df_VA[, dem_g_primary := str_detect(variable, "GOV_P_D")]
df_VA[, dem_g_runoff := str_detect(variable, "GOV_P_RO_D")]

unique(df_cw[ICPSRST == 40 & year == 1930, .(NHGISNAM)]) %>% print(., nrow = 200)

dim(df_VA[variable == "X33_GOV_P_D_PEERY"])

unique(df_VA$variable)

df_VA <- merge(df_VA, df_cw[, .(year_floor = year, ICPR_STATE_CODE = ICPSRST,
    COUNTY_OR_STATE_NAME = toupper(NHGISNAM), state_1930, county_1930, weight)],
    by = c("year_floor", "ICPR_STATE_CODE", "COUNTY_OR_STATE_NAME"),
    all.x = TRUE, allow.cartesian = TRUE)


df_VA <- df_VA[, .(value = sum(value * weight)), by = .(state_1930, county_1930, year, variable, dem_g_primary, dem_g_runoff)]



df_VA <- df_VA[dem_g_primary == TRUE | dem_g_runoff == TRUE]
df_VA[, event := paste(year, dem_g_primary, dem_g_runoff)]
df_VA[, total_vote := sum(value, na.rm = TRUE), by = .(event, state_1930, county_1930)]
df_VA[, vote_share := value / total_vote]
df_VA





byrd_candidates <- c("X25_GOV_P_D_BYRD", "X29_GOV_P_D_POLLARD", "X33_GOV_P_D_PEERY",
    "X37_GOV_P_D_PRICE", "X41_GOV_P_D_DARDEN", "X45_GOV_P_D_TUCK")   

df_VA[, byrd := variable %in% byrd_candidates]
df_VA <- df_VA[byrd == TRUE][between(year, 1932, 1945), .(state_1930, county_1930, year,
    dem_g_primary, dem_g_runoff, event, byrd_vote_share = vote_share)]

fwrite(df_VA, "Data/_Clean/voting_VA.csv")






#%% voting data
# voting data
df_vote_p <- read_dta("Data/Electoral data US/ICPSR_08611/DS0001/08611-0001-Data.dta")

df_vote_p <- data.table(df_vote_p)[, .(ICPSRST = V1, vote_county_name = V2,
    ICPSRCTY = V3,
    dem_pres_1880 = V207, dem_pres_1884 = V229, dem_pres_1888 = V251,
    dem_pres_1892 = V273, dem_pres_1896 = V295, dem_pres_1900 = V317,
    dem_pres_1904 = V336, dem_pres_1908 = V357, dem_pres_1912 = V378,
    dem_pres_1916 = V402, dem_pres_1920 = V428, dem_pres_1924 = V452,
    dem_pres_1928 = V477, dem_pres_1932 = V498, dem_pres_1936 = V521,
    dem_pres_1940 = V546, dem_pres_1944 = V567, dem_pres_1948 = V586,
    dem_pres_1952 = V604, dem_pres_1956 = V619, dem_pres_1960 = V634,
    dem_pres_1964 = V649, dem_pres_1968 = V664, dem_pres_1972 = V680,
    rep_pres_1880 = V208, rep_pres_1884 = V230, rep_pres_1888 = V252,
    rep_pres_1892 = V274, rep_pres_1896 = V296, rep_pres_1900 = V318,
    rep_pres_1904 = V337, rep_pres_1908 = V358, rep_pres_1912 = V379,
    rep_pres_1916 = V403, rep_pres_1920 = V429, rep_pres_1924 = V453,
    rep_pres_1928 = V478, rep_pres_1932 = V499, rep_pres_1936 = V522,
    rep_pres_1940 = V547, rep_pres_1944 = V568, rep_pres_1948 = V587,
    rep_pres_1952 = V605, rep_pres_1956 = V620, rep_pres_1960 = V635,
    rep_pres_1964 = V650, rep_pres_1968 = V665, rep_pres_1972 = V681,
    votes_1880 = V211, votes_1882 = V227, votes_1884 = V233,
    votes_1886 = V249, votes_1888 = V254,
    votes_1890 = V271, votes_1892 = V277, votes_1894 = V293,
    votes_1896 = V299, votes_1898 = V315, votes_1900 = V320,
    votes_1902 = V334, votes_1904 = V341, votes_1906 = V355,
    votes_1908 = V362, votes_1910 = V376, votes_1912 = V383,
    votes_1914 = V400, votes_1916 = V407, votes_1918 = V426,
    votes_1920 = V432, votes_1922 = V450, votes_1924 = V456,
    votes_1926 = V475, votes_1928 = V481, votes_1930 = V496,
    votes_1932 = V502, votes_1934 = V519, votes_1936 = V525,
    votes_1940 = V550, votes_1942 = V565,
    votes_1944 = V571, votes_1946 = V584, votes_1948 = V591,
    votes_1950 = V602, votes_1952 = V607, votes_1954 = V617,
    votes_1956 = V622, votes_1958 = V632, votes_1960 = V637,
    votes_1964 = V652, votes_1968 = V668, votes_1972 = V683
    )]

df_vote_p[, .(n = .N), by = .(ICPSRST, vote_county_name, ICPSRCTY)][n > 1]
colnames(df_vote_p)
counts = sapply(colnames(df_vote_p), function(x){sum(colnames(df_vote_p) == x)})
colnames(df_vote_p)[counts > 1]


drop_nas <- function(vector) {
    vector[vector > 100] <- NA
    return(vector)
}

my_cols <- colnames(df_vote_p)[4:ncol(df_vote_p)]
my_cols
vote_cols <- my_cols[str_detect(my_cols, "votes_")]
my_cols <- my_cols[!str_detect(my_cols, "votes_")]
df_vote_p[, (my_cols) := lapply(.SD, drop_nas), .SDcols = my_cols]

drop_nas_votes <- function(vector) {
    vector[vector == 9999 | vector == 9999999] <- NA
    return(vector)
}
df_vote_p[, (vote_cols) := lapply(.SD, drop_nas_votes), .SDcols = vote_cols]


df_vote_p <- melt(df_vote_p, id.vars = c("ICPSRST", "ICPSRCTY", "vote_county_name"))
head(df_vote_p)

df_vote_p[, variable := as.character(variable)]
df_vote_p[, year := str_extract(variable, "1[8|9][0-9]{2}") %>% as.numeric()]
df_vote_p[, variable := substr(variable, 1, nchar(variable) - 5)]



df_vote_p[, votes := value[variable == "votes"], by = .(ICPSRST, ICPSRCTY, vote_county_name, year)]

year_no <- 1872

df_vote_p[, election_year := year]
df_vote_p[, year := floor(election_year / 10) * 10]



df_vote_p <- merge(df_vote_p, df_cw, by = c("ICPSRST", "ICPSRCTY", "year"),
    allow.cartesian = TRUE)
df_cw
df_vote_p <- df_vote_p[!is.na(votes)]
df_vote_p[, vote_weight := votes * weight]


df_vote_p <- df_vote_p[, .(
    value = weighted.mean(value, vote_weight, na.rm = T),
    votes = sum(votes * weight, na.rm = T)),
    by = .(state_1930, county_1930, variable, election_year, year)]

df_vote_p <- dcast(df_vote_p[variable != "votes"],
    state_1930 + county_1930 + year + election_year + votes ~ variable, value.var = "value")
df_vote_p[, year := round(election_year / 10) * 10]

# Dave Leip election data

read_leip_pres <- function(year_no) {
    file_string <- paste0("Data/Electoral data US/Dave Leip/Pres_Election_Data_",
        year_no, ".xlsx")
    if (!file.exists(file_string)) {
        file_string <- paste0("Data/Electoral data US/Dave Leip/Pres_Election_Data_",
            year_no, ".xls")
    }
    df_leip_county <- read_excel(file_string, sheet = "County")
    colnames(df_leip_county)
    df_leip_county <- data.table(df_leip_county)
    colnames(df_leip_county)[colnames(df_leip_county) == "State Code"] <- "ST"
    colnames(df_leip_county)[colnames(df_leip_county) == "County Code"] <- "CTY"

    colnames(df_leip_county)[1] <- "county_name"
    df_leip_county <- melt(df_leip_county,
        id.vars = c("FIPS", "LSAD_TRANS", "ST", "CTY", "county_name", "Total Vote"))

    df_leip_county[, varmax := max(as.numeric(value), na.rm = TRUE), by = .(variable)]
    df_leip_county <- df_leip_county[varmax > 1 & !is.na(value)]

    df_leip_county[, var_name := str_remove_all(variable, "\\.\\.\\.[0-9]+")]

    df_leip_county[, var_number := str_extract(variable, "[0-9]+")]
    df_leip_county[, n_name := length(unique(variable)), by = .(var_name)]
    unique(df_leip_county[n_name > 1, .(var_name)])
    names_to_rank <- unique(df_leip_county[n_name > 1, .(var_name, var_number)])
    names_to_rank
    if(nrow(names_to_rank) > 0) {
        names_to_rank[, name_rank := frank(as.numeric(var_number)), by = .(var_name)]
        df_leip_county <- merge(df_leip_county, names_to_rank, by = c("var_name", "var_number"),
            all.x = TRUE)
        df_leip_county[n_name > 1, var_name := paste0(var_name, "_", name_rank)]
    }


    if (year_no == 2000) {
        df_leip_candidates <- fread("Data/Electoral data US/Dave Leip/candidate_short_names_2000.csv",
            header = TRUE)
    } else {
        df_leip_candidates <- read_excel(file_string, sheet = "Candidates")
    }


    df_leip_candidates

    df_leip_candidates <- data.table(df_leip_candidates)[, .(party = Party, short_name = `Short Name`)]

    df_leip_candidates[, row := 1:nrow(df_leip_candidates)]
    df_leip_candidates[, n_name := .N, by = .(short_name)]
    df_leip_candidates[, name_rank := frank(row), by = .(short_name)]
    df_leip_candidates[n_name > 1, short_name := paste0(short_name, "_", name_rank)]
    df_leip_candidates <- df_leip_candidates[, .(short_name, party)]

    df_leip_county <- merge(df_leip_county, df_leip_candidates, by.x = "var_name",
        by.y = "short_name", all.x = TRUE)
    unique(df_leip_county[, .(var_name, party)])
    df_leip_county[var_name == "Independent" & is.na(party), party := "Independent"]
    df_leip_county[var_name %in% c("Write-in", "Write-ins") , party := "Write in"]
    df_leip_county[var_name %in% c("Unpledged") , party := "Unpledged"]
    df_leip_county[, n_party := length(unique(var_name)), by = .(party)]
    df_leip_county[n_party > 1 & !is.na(party), party := paste0(party, "_", var_name)]


    df_leip_county[, party := str_replace_all(party, " ", "_")]
    df_leip_county[, party := tolower(party)]
    df_leip_county[, party := str_remove_all(party, "[^_[:^punct:]]")]
    df_leip_county <- df_leip_county[!is.na(party),
        .(fips = FIPS, statefips = ST, countyfips = CTY, county_name,
        total_vote = `Total Vote`, area_type = LSAD_TRANS, party,
        votes = as.numeric(value))]
    df_leip_county <- dcast(df_leip_county[!is.na(fips)], fips + statefips + countyfips +
        county_name + total_vote ~ party, value.var = "votes", fill = 0)
    df_leip_county[, year := year_no]
    return(df_leip_county)
}
election_years <- seq(1976, 2020, by = 4)
election_years
df_leip <- lapply(election_years, read_leip_pres) %>%
    rbindlist(., use.names = TRUE, fill = TRUE)
head(df_leip)
df_leip <- melt(df_leip, id.vars = c("fips", "statefips", "countyfips", "county_name", "year"))
df_leip[, fips := as.numeric(fips)]
dim(df_leip)
gc()


df_leip[, year_floor := floor(year / 10) * 10]
df_leip <- merge(df_leip, df_cw, by.x = c("fips", "year_floor"), by.y = c("ICPSRFIP", "year"), all.x = TRUE,
    allow.cartesian = TRUE)
head(df_leip)
df_leip <- df_leip[!is.na(state_1930), .(value = sum(value * weight)),
    by = .(state_1930, county_1930, year, variable)]
df_leip <- dcast(df_leip, state_1930 + county_1930 + year ~ variable, value.var = "value")

head(df_leip)

df_voting_combined <- df_vote_p[!is.na(dem_pres) | !is.na(rep_pres), .(state_1930, county_1930, year = election_year,
    dem_pres, rep_pres, total_vote = votes)]
head(df_voting_combined)

df_voting_combined_leip <- df_leip[year > 1972, .(state_1930, county_1930, year,
    dem_pres = democratic / total_vote, rep_pres = republican / total_vote,
    total_vote)]

df_voting_combined <- rbindlist(list(df_voting_combined, df_voting_combined_leip))
head(df_voting_combined)

df_vote_p
df_vote_p <- df_vote_p[, .(state_1930, county_1930, election_year, dem2p = dem_pres / (dem_pres + rep_pres))]

fwrite(df_voting_combined, "Data/_Clean/us_voting_pres_long_timeseries.csv")
fwrite(df_vote_p, "Data/_Clean/us_voting_pres.csv")


#%% 1930 place data



df_place_cat <- read_excel(paste0("Data/Welfare spending/",
    "Financial Statistics of State and Local Governments/",
    "cpp_places_1930_to_cities.xlsx"), sheet = "places_merged")
df_place_cat <- data.table(df_place_cat)[!is.na(city_id)]
df_city_cat <- read_excel(paste0("Data/Welfare spending/",
    "Financial Statistics of State and Local Governments/",
    "cpp_places_1930_to_cities.xlsx"), sheet = "cities_merged")
df_place_cat[, n_city := .N, by = .(potential_match, postal_code, city_id)]


df_check <- merge(df_place_cat, df_city_cat, by = "city_id")
df_check[, dist := abs(pop - n)]
df_check[, min_dist := min(dist), by = .(potential_match, postal_code, city_id)]
df_check <- df_check[dist == min_dist]
df_place_cat <- df_place_cat[cpp_placeid %in% df_check$cpp_placeid]


df_cities <- data.table(read_excel(paste0("Data/Welfare spending/",
    "Financial Statistics of State and Local Governments/",
    "cities_data.xlsx")))
df_cities <- df_cities[!str_detect(city, "30,000|8,000")]
df_cities <- df_cities[!is.na(general_op)]

df_cities_clean <- df_cities[, .(state, city, pop,
    overhead_efficiency = (total_op - general_op - highways_op - misc_op) / (total_op - highways_op),
    highway_spending_share = highways_op / total_op
    )]
fwrite(df_cities_clean, "Data/_clean/cities_efficiency.csv")


df_cities <- merge(df_cities, data.table(df_city_cat)[, .(state, city, city_id)], by = c("state", "city"))

df_cities <- df_cities[, .(pop = sum(pop), total = sum(as.numeric(total)), total_op = sum(total_op),
    general_op = sum(general_op), police_op = sum(police_op),
    highways_op = sum(highways_op), misc_op = sum(misc_op)),
    by = .(state, city_id)]


df_census_place <- fread("Data/Merged CPP Census data/census_1930_by_cpp_place.csv")
df_census_place_n <- df_census_place[, .(n = sum(n),
    n_police = sum(n[occ1950 == 773]),
    n_firemen = sum(n[occ1950] == 763)
    ), by = .(cpp_placeid)]
head(df_place_cat)
df_places <- merge(df_place_cat[, .(cpp_placeid, city_id)], df_census_place_n, by = "cpp_placeid")
df_places <- df_places[, .(n_police = sum(n_police), n_firemen = sum(n_firemen),
    n_place = sum(n)), by = .(city_id)]

df_cities <- merge(df_cities, df_places, by = "city_id")
# drop Indiana, because Census Place Project localities are much smaller and
# have big discrepancies in population with reported city population
df_cities <- df_cities[state != "IN"]


df_cities <- df_cities[, .(city_id, state, pop, police_fire_percap = (n_police + n_firemen) / n_place,
    police_fire_spend_percap = (police_op / pop)
    )]

fwrite(df_cities, "Data/_Clean/cities_employment.csv")

# Data for charts

df_union_ts <- read_excel("Data/Unionization/HSUS union membership/Ba4783-4791.xls")
df_union_ts
colnames(df_union_ts)
df_union_ts <- data.table(df_union_ts)[,
    .(year = as.numeric(`...1`),
    bls_union_members = as.numeric(`Table Ba4783-4791.  Union membership: 1880-1999`),
    bls_canada_members = as.numeric(`...4`),
    troy_sheflin = as.numeric(`...5`))]
df_union_ts[!is.na(bls_canada_members), union_ex_canada := bls_union_members - bls_canada_members]
df_union_ts[is.na(bls_canada_members), union_ex_canada := bls_union_members]
df_union_ts <- df_union_ts[between(year, 1900, 1960), .(year, bls_union_members = bls_union_members / 1000)]
fwrite(df_union_ts, "Data/_Clean/union_membership_timeseries.csv")






df_state <- fread("Data/NHGIS/nhgis0069_ds78_1940_state.csv")
df_state
df_state <- df_state[, .(state = STATE, STATEICP, pop = BV7001, pop_urban = BW1001, pop_city = BXD001)]
df_ue <- read_excel("Data/Welfare spending/Social Security Board Reports/social_security_board.xlsx")
head(df_ue)
df_ue <- merge(df_ue, df_state, by = "state", all.x = TRUE, all.y = TRUE)
df_ue <- data.table(df_ue)
df_ue[is.na(pop)]
df_statecodes <- fread("Data/state_codes.csv")
df_ue <- merge(df_ue, df_statecodes[, .(STATEICP = ICPSRST, postal_code)])
df_ue <- df_ue[, .(postal_code, log_share_urban = log(pop_urban / pop), log_admin_ratio = log(administrative_expenditures / (unemployment_compensation_benefits_paid_thousands * 1000)))]

fwrite(df_ue, "Data/_Clean/social_security_admin_costs_state.csv")


df_wallis <- read_excel("Data/Welfare spending/Wallis timeseries/wallis_level_of_government_data.xlsx",
    sheet = "transposed")
df_wallis <- data.table(df_wallis)[, .(year = as.numeric(year),
    federal_ex_military_percentage = as.numeric(federal_ex_military) / as.numeric(total) * 100
    )][year <= 1960]
fwrite(df_wallis, "Data/_Clean/federal_spending_timeseries.csv")



df_spend_total <- read_excel("Data/Welfare spending/HSUS Intergov share/TableEa61-124/Ea61-124.xls",
    skip = 9, col_names = FALSE)
df_spend_total <- data.table(df_spend_total)[, .(year = as.numeric(`...1`),
    total_total = as.numeric(`...2`) / 1000,
    educ_total = as.numeric(`...8`) / 1000,
    highways_total = as.numeric(`...28`) / 1000,
    health_total = as.numeric(`...17`) / 1000,
    welfare_total = as.numeric(`...13`) / 1000)]

df_spend_fed_intergov <- read_excel("Data/Welfare spending/HSUS Intergov share/TableEa220-246/Ea220-246.xls",
    skip = 7, col_names = FALSE)
df_spend_fed_intergov <- data.table(df_spend_fed_intergov)[, .(year = as.numeric(`...1`),
    educ_fed_intergov = as.numeric(`...4`),
    welfare_fed_intergov = as.numeric(`...6`))]

df_spend <- merge(df_spend_total, df_spend_fed_intergov, by = "year")




df_spend_long <- df_spend[, .(year, Education = educ_fed_intergov / educ_total * 100,
    `Public Welfare` = welfare_fed_intergov / welfare_total * 100)]
fwrite(df_spend_long, "Data/_Clean/intergov_transfers_timeseries.csv")



df_place_cpp <- fread("Data/Merged CPP Census data/census_1940_by_cpp_place.csv")
print(colnames(df_place_cpp))
df_place_cpp <- df_place_cpp[, .(n = sum(n)),
    by = .(stateicp, countyicp, cpp_placeid, farm, urban, ssenroll, empstatd,
        ind1950, occ1950, race, bpl, sex, educ)]
df_place_lookup <- fread("Data/_Intermediates/census_place_project/places_populations_1940.csv")
head(df_place_lookup)
df_place_cpp <- merge(df_place_cpp, df_place_lookup[, .(cpp_placeid, potential_match, n_place_tot = n)],
    by = "cpp_placeid")
head(df_place_cpp)
df_place_cpp[, n_place := sum(n), by = .(cpp_placeid)]
df_place_cpp[empstatd == 0, empstatd := NA]
df_place_cpp[, emergency_work := (empstatd == 11)]
df_place_cpp[empstatd >= 30, emergency_work := NA]
df_place_cpp[empstatd <= 10, emergency_work := NA]


df_place_cpp[, farm_hh := (farm == 2)]
df_place_cpp[farm == 0, farm_hh := NA]

df_place_cpp[, rural := 1 * (urban == 1)]
df_place_cpp[urban == 0, rural := NA]

df_place_cpp[ssenroll == 0, ssenroll := NA]
df_place_cpp[, social_security := ssenroll - 1]


confederacy_states <- c(510, 10, 50, 120, 130, 220, 280, 370, 450, 480, 210,
    400, 470)
confederacy_states_icp <- df_statecodes[NHGISST %in% confederacy_states, ICPSRST]
confederacy_states_icp
df_place_ew <- df_place_cpp[!(stateicp %in% confederacy_states_icp) & potential_match != ""#& placenhg != "9999999999"
    ,
    .(emergency_work = weighted.mean(emergency_work * 100, n, na.rm = TRUE),
    n_emergency_work = sum(n[!is.na(emergency_work)])),
    by = .(stateicp, countyicp, cpp_placeid, n_place, potential_match)]
df_place_ss <- df_place_cpp[!(stateicp %in% confederacy_states_icp) &
    !(ind1950 %in% c(105, 826:849)) & potential_match != "",
    .(social_security = weighted.mean(social_security * 100, n, na.rm = TRUE),
    n_social_security = sum(n[!is.na(social_security)])),
    by = .(stateicp, countyicp, cpp_placeid, n_place, potential_match)]

df_place_both <- merge(df_place_ew[, .(stateicp, countyicp, cpp_placeid, n_place, potential_match, emergency_work)],
    df_place_ss[, .(stateicp, countyicp, cpp_placeid, n_place, social_security)],
    by = c("stateicp", "countyicp", "cpp_placeid", "n_place"), all.x = TRUE, all.y = TRUE)

fwrite(df_place_both, "Data/_Clean/emergency_work_social_security_1940_census.csv")

#%% consumption data

read_data <- function(file_loc) {
    df_temp <- fread(file_loc, sep = "\t")

    df_temp[V1148 <= 1, auto_spend := V1147 * (1 - V1148)]
    df_temp[V1148 > 1, auto_spend := V1147 - V1148]
    df_temp <- df_temp[, .(famid = V4, famsize = V9, townid = V10, state = V12, urban_rural = V14, race = V101,
        total_income = V285,
        housing_spend = V359 + V360 + V361 + V362 + V381 + V382 + V383 + V384 +
            V389 + V390 + V385 + V386 + V387, 
        household_op_spend = V639,
        med_spend = V686, rec_spend = V788, tobacco_spend = V798,
        reading_spend = V812, ed_spend = V837 + V838 + V839,
        oc_spend = V850, gift_spend = V853, food_spend = V1077,
        auto_spend, clothing_spend = V1254, travel_spend = V1262,
        personal_spend = V1290, equipment_spend = V1298 + V1301 + V1304 + V1307 +
            V1310 + V1313 + V1315 + V1318 + V1321 + V1324,
        other_spend = V1336 + V1337 + V1339 + V1340,
        water_supply_living = V320, water_supply_indoors = V321,
        running_water_hot = V323, running_water_cold = V324,
        toilets_living = V327, toilets_indoors = V328,
        #n_toilets_flush = V330, n_toilets_other = V331,
        heating_central = V334, heating_stove = V336, heating_stove_kitchen = V338,
        heating_fireplace = V339, lighting_electricity = V341, lighting_gas = V343,
        lighting_kerosene = V344, cooking_gas = V346, cooking_electricity = V347,
        cooking_wood = V348, cooking_kerosene = V349,
        home_rent = V355, home_rent_total = V359, home_rent_months = V353,
        movies_winter = V735, movies_spring = V740, movies_summer = V745,
        movies_fall = V750, movies_spend = V739 + V744 + V749 + V754,
        plays_spend = V757, games_spend = V758,
        dance_spend = V759, hunting_spend = V761, fishing_spend = V762, 
        camping_spend = V763, trapping_spend = V764, hiking_spend = V765,
        riding_spend = V766, baseball_spend = V767, tennis_spend = V768,
        golf_spend = V769, cycling_spend = V770, skates_spend = V771,
        billiards_spend = V772, boats_spend = V773, cards_spend = V774,
        other_games_spend = V775, radio_spend = V777, radio_extra_spend = V778,
        instrument_spend = V779, music_spend = V781, camera_spend = V782,
        toy_spend = V783, pet_spend = V784, entertaining_spend = V785,
        dues_spend = V786, other_rec_spend = V787, total_rec_spend = V789,
        movie_price_winter = V736, movie_price_spring = V741,
        movie_price_summer = V746, movie_price_fall = V751,
        daily_newspaper_price = V799, weekly_newspaper_price = V802,
        wife_haircut_price = V1265, husband_haircut_price = V1269,
        toilet_soap_price = V1281,
        value_of_farm_products_used = V284, value_farm_fuel_used = V282,
        value_farm_other_used = V283,
        value_home_produced_food = V286 + V289 + V291,
        own_piano = V1291, own_phonograph = V1293, own_radio = V1295, 
        own_refrigerator = V1297, own_other_refrigerator = V1300,
        own_ice_box = V1303, own_pressure_cooker = V1306, own_power_wash = V1309,
        own_other_washer = V1312, own_ironing_machine = V1314, own_vacuum_cleaner = V1317,
        own_sewing_machine = V1320, own_automobile = 1 * (V1094 > 0),
        dwelling_type = V302, n_rooms = V303, n_occupants = V304,
        home_rented_months = V353 + V354, home_owned_months = V367 + V368
        )]
    df_temp[, rented_share := home_rent_months / (home_rented_months + home_owned_months)]
    df_temp[urban_rural == 1, urban := 1]
    df_temp[urban_rural %in% c(2, 3), urban := 0]
    df_temp[urban_rural == 1, urban2 := 1]
    df_temp[urban_rural == 2, urban2 := 0]
    
}
file_locs <- c("Data/Consumption/ICPSR_08908/DS0003/08908-0003-Data.tsv",
    "Data/Consumption/ICPSR_08908/DS0004/08908-0004-Data.tsv"
    )
df <- lapply(file_locs, read_data) %>% rbindlist()
head(df)


df[, total_spend := housing_spend +
    household_op_spend + med_spend + rec_spend
    + tobacco_spend + reading_spend + ed_spend + oc_spend + gift_spend + food_spend
    + auto_spend + clothing_spend + travel_spend + personal_spend + equipment_spend +
    other_spend]

df_classif <- data.table(read_excel("Data/Consumption/classifying_1935_towns.xlsx"))
df_classif <- df_classif[, .(townid, name, group)]

df <- merge(df, df_classif, by = "townid", all.x = TRUE)
df[is.na(group), .N, by = .(townid, urban)]

df <- df[, .(famid, non_food_share = 1 - food_spend / total_spend, group, total_spend, famsize, state, race, townname = name)]
df[,.N, race]
df[race == 1, race_l := "white"]
df[race == 0, race_l := NA]
df[race == 2, race_l := "black"]
df[, race := NULL]
df[, race := race_l]
df[, race_l := NULL]
df <- df[!is.na(group)]

fwrite(df, "Data/_Clean/consumption_data.csv")


df_wpa <- read_excel("Data/Consumption/WPA cost of living/table_2.xlsx")



df_wpa <- data.table(df_wpa)
df_wpa[, b_south := "Non-South"]
df_wpa[south == 1, b_south := "South"]

df_wpa <- df_wpa[, .(name_clean, b_south, pop_1930, total, food, housing)]
fwrite(df_wpa, "Data/_Clean/WPA_cost_of_living.csv")


df_ny <- read_excel("Data/Consumption/1928 new york state/cost_of_living_table_clean.xlsx")
df_ny <- data.table(df_ny)
df_ny <- df_ny[, .(town, pop, total_calc, food, rent)]
df_ny
fwrite(df_ny, "Data/_Clean/NY_cost_of_living.csv")

#%% CCES data

df_16 <- read_dta("Data/CCES/2016/CCES16_Common_OUTPUT_Feb2018_VV.dta")
head(df_16)
df_16 <- data.table(df_16)
df_16[, .N, by = .(CC16_427_a)]
unique(df_16$CC16_427_a)

df_16[, .N, by = .(CC16_427_b)]

df_16[CC16_427_a <= 5, school_satisfaction := 5 - CC16_427_a]
df_16[CC16_427_b <= 5, police_satisfaction := 5 - CC16_427_b]
df_16[CC16_427_c <= 5, roads_satisfaction := 5 - CC16_427_c]
df_16[CC16_427_d <= 5, zoning_satisfaction := 5 - CC16_427_d]
df_16[CC16_427_e <= 5, mayor_satisfaction := 5 - CC16_427_e]
df_16[CC16_427_f <= 5, council_satisfaction := 5 - CC16_427_f]
df_16 <- data.table(df_16)[, .(case_id = V101, school_satisfaction, police_satisfaction,
    roads_satisfaction, zoning_satisfaction, mayor_satisfaction, council_satisfaction,
    commonweight,
    gender, educ, race, pid3, union,
    zip = as.numeric(lookupzip))]


df_census <- fread("Data/NHGIS/nhgis0070_ds172_2010_zcta.csv")
head(df_census)
df_census <- df_census[, .(zip = as.numeric(ZCTA5A), share_urban = H7W002 / H7V001)]
df_16 <- merge(df_16, df_census, by = "zip", all.x = TRUE)

fwrite(df_16, "Data/_Clean/cces_data.csv")

#%% ANES data

df_anes <- fread("Data/ANES/Cumulative Dataset/anes_timeseries_cdf_csv_20211118.csv")
head(df_anes)

df_anes_s <- df_anes[, .(year = VCF0004, id = VCF0006, weight = VCF0009z, gov_waste = VCF0606, gov_responsive = VCF0649,
    gov_trust = VCF0656, vote_pres = VCF0704a, lib_con = VCF0803,
    gov_spending = VCF0839, welfare_spending = VCF0894)]
df_anes_s

df_anes_s[vote_pres == 1, vote_dem := 1]
df_anes_s[vote_pres == 2, vote_dem := 0]
df_anes_s[gov_spending %in% c(0, 9), gov_spending := NA]

df_anes_s[lib_con %in% c(0, 9), lib_con := NA]
df_anes_s[lib_con <= 3, lib_con_b := 0]
df_anes_s[lib_con >= 5, lib_con_b := 1]

df_anes_s[gov_waste %in% c(1, 2, 3), gov_wasteful := 3 - gov_waste]


df_anes_s[gov_responsive == 999, gov_responsive := NA]
df_anes_s[gov_trust == 999, gov_trust := NA]

df_anes_s[welfare_spending == 1, increase_welfare := 1]
df_anes_s[welfare_spending %in% c(2), increase_welfare := 0]
df_anes_s[welfare_spending == 3, increase_welfare := -1]

df_anes_s[, .N, by = .(year, welfare_spending)]

df_anes_s[, gov_spending_01 := (gov_spending - 1)/ 6]
df_anes_s[, increase_welfare_01 := (increase_welfare + 1)/ 2]
df_anes_s[, lib_ideology_01 := (7 - lib_con)/ 6]

df_anes_s <- df_anes_s[, .(year, id, weight, vote_dem, gov_spending_01, increase_welfare_01, lib_ideology_01)]
fwrite(df_anes_s, "Data/_Clean/ANES_data.csv")

#%% Country-Level data

df_wpid <- fread("Data/WPID/macro data/gmp-macro-final-party.csv")
head(df_wpid)
df_wpid <- df_wpid[party %in% c("left", "right")]
head(df_wpid)

df_wpid <- df_wpid[variable == "Rural-urban location"]
unique(df_wpid$isoname)

df_wpid[, year := floor(year/ 10) * 10]


df_wpid <- df_wpid[, .(mean = weighted.mean(mean, n, na.rm = TRUE),
    n = sum(n)),
    by = .(iso, isoname, party, sub, year)]
head(df_wpid)
df_wpid <- dcast(df_wpid, iso + isoname + year ~ party + sub, value.var = c("mean", "n"))
df_wpid
df_wpid[, rural_urban := mean_right_Rural - mean_left_Rural - (mean_right_Urban - mean_left_Urban)]
df_wpid[, rural_urban_r := mean_right_Rural - mean_right_Urban]
df_wpid[, rural_urban_l := mean_left_Rural - mean_left_Urban]
df_wpid[order(rural_urban)]
setorder(df_wpid, rural_urban)

df_wdi <- fread("Data/World Bank Country Data/1e625a1e-5598-495b-bfbf-e40e9af4acc3_Data.csv")
df_wdi
df_wdi <- df_wdi[, .(name = `Country Name`, code = `Country Code`, series = `Series Name`,
    `1980` = as.numeric(`1980 [YR1980]`),
    `1990` = as.numeric(`1990 [YR1990]`),
    `2000` = as.numeric(`2000 [YR2000]`),
    `2010` = as.numeric(`2010 [YR2010]`),
    `2020` = as.numeric(`2020 [YR2020]`))]
df_wdi <- melt(df_wdi, id.vars = c("name", "code", "series"))

unique(df_wdi$series)
df_wdi[series == "GDP per capita (constant 2015 US$)", series_s := "gdppc"]
df_wdi[series == "Tax revenue (% of GDP)", series_s := "tax_share"]
df_wdi[series == "General government final consumption expenditure (% of GDP)",
    series_s := "gov_share"]
df_wdi[series == "Government Effectiveness: Estimate",
    series_s := "gov_effect"]
df_wdi[series == "Urban population (% of total population)",
    series_s := "urban_share"]
df_wdi <- dcast(df_wdi[!is.na(series_s)], name + code + variable ~ series_s, value.var = "value")
df_wdi[, isoname := name]



df_wdi[, name] %>% unique()
df_wdi[name == "Czechia", isoname := "Czech Republic"]
df_wdi[name == "Korea, Rep.", isoname := "South Korea"]
df_wdi[name == "Turkiye", isoname := "Turkey"]
df_wdi[, year := as.numeric(as.character(variable))]

df_wpid <- merge(df_wpid, df_wdi, by = c("isoname", "year"), all.x = TRUE)


df_dalp <- fread("Data/DALP datasets_20131108/countrylevel_20130907.csv")
head(df_wpid)
df_dalp[, isoname := country]


df_dalp[, country] %>% unique()
df_dalp[country == "Czech Rep.", isoname := "Czech Republic"]
df_dalp[country == "S. Africa", isoname := "South Africa"]
df_dalp[country == "ROK", isoname := "South Korea"]
df_dalp[country == "USA", isoname := "United States"]

df_wpid <- merge(df_wpid, df_dalp, by = "isoname", all.x = TRUE)

df_imf <- read_excel("Data/IMF/imf-dm-export-20230914.xls")
df_imf <- melt(data.table(df_imf), id.vars = c("Government revenue, percent of GDP (% of GDP)"))
df_imf <- df_imf[, .(countryname = `Government revenue, percent of GDP (% of GDP)`,
    year = as.numeric(as.character(variable)),
    gov_rev_pc = as.numeric(value))]
df_imf <- df_imf[!is.na(countryname) & !is.na(gov_rev_pc)]
df_imf[, isoname := countryname]
df_imf[]

df_imf[countryname == "Korea, Republic of", isoname := "South Korea"]
df_imf[countryname == "Türkiye, Republic of", isoname := "Turkey"]

df_wpid <- merge(df_wpid, df_imf, by = c("isoname", "year"), all.x = TRUE)

df_wpid <- df_wpid[year == 2010, .(isoname, gdppc, clientelism = b6, gov_rev_pc, rural_urban)]
df_wpid

fwrite(df_wpid, "Data/_Clean/cross_country_data.csv")


#%% UK data

# election data
es_elections <- fread("Data/Eggers Spirling/elections.csv")


es_returns <- fread("Data/Eggers Spirling/election_returns.csv")
es_elections <- merge(es_elections, es_returns, by = "election_id")
es_elections[, year := as.numeric(substr(date, 1, 4))]
es_elections[, month := as.numeric(substr(date, 6, 7))]
es_elections[year == 1910 & month == 12, year := 1911]

es_elections[, rank := rank(-votes), by = .(election_id)]
es_elections[seats_up == 4, top_candidates := 1 * (rank <= 8)]
es_elections[seats_up == 3, top_candidates := 1 * (rank <= 6)]
es_elections[seats_up == 2, top_candidates := 1 * (rank <= 4)]
es_elections[seats_up == 1, top_candidates := 1 * (rank <= 2)]
unique(es_elections[, seats_up])
numbers <- c(7, 12, 4)
rank(-numbers)
head(es_elections)
es_elections <- es_elections[between(year, 1885, 1945)]
winners_list <- es_elections[winner == 1,
    .(last_election = election_id, member_id, party)]
winners_list
election_list <- es_elections[, .(constituency.id, date, election_id)] %>%
    unique()
setorder(election_list, date)
election_list[, last_election := shift(election_id, 1), by = .(constituency.id)]
election_list <- merge(election_list, winners_list, by = "last_election")
election_list <- election_list[,
    .(election_id, incumbent_member_id = member_id, incumbent_party = party)]
incumbents <- merge(es_elections, election_list, by = "election_id")
incumbents[, `:=`(incumbent_mp = as.numeric(member_id == incumbent_member_id),
    incumbent_party = as.numeric(party == incumbent_party))]
incumbents <- incumbents[, .(incumbent_mp = max(incumbent_mp, na.rm = T),
    incumbent_party = max(incumbent_party, na.rm = T)),
    by = .(election_id, name_on_return)]
incumbents[incumbent_mp == -Inf, incumbent_mp := 0]
es_elections <- merge(es_elections, incumbents,
    by = c("election_id", "name_on_return"), all.x = T)
es_elections <- es_elections[between(year, 1885, 1945)]



conservative_parties <- c("C", "LU", "Ind C", "Ind C (C)", "C*", "C (Nat P)",
    "LU (Ind L)", "C (Ind C)", "LU*", "C (L)", "LU (C)", "C* (Nat P)",
    "Ind LU", "LU (L)", 'LU (Nat P) (C)', 'C (Nat P) (C)',
    'LU/Crf', "C (Ind C) (C)", "Conservative", "Unionist",
    "U",  "Ind. U(R)", "Co C (C)", "Co C (Ind)", "Co C", "Ind. C", "Ind. LU", "CoC",
    "Co Ind (Co C)")
n_parties <- es_elections[between(year, 1885, 1935), .(n = .N), by = .(party)]


leftist_parties <- c('L/Lab', 'ILP', 'Lab', 'Ind Lab', 'SDF', 'SDP', 'Lab (L/Lab)',
    'Ind L/Lab', 'Ind Lab (L/Lab)', 'Ind Lab (ILP)', 'Ind Lab (Lab)', 'Ind. Lab',
    "SWRC", "SPLP", "SLP", "SPP", "SSF & SUTCLP", "SLRL", "BSP", "Comm.",
    "Ind Lab (Lab) (Ind Lab)", "Lab/Co-op", "Lab (N Lab)", "N Lab", "N Lab",
    "Lab U", "CWLP", "BLP", "NI Lab (lab) (Ind Lab)", "Lab (Ind Lab) (Lab)",
    "Co Lab", "Co Lab", "N Lab (Nat)", "SCPGB", "SPGB", "Lab (NP)", "CP (Lab)",
    "Co-op", "Lab/Co-op", "Co NDP", "NDP")
liberal_parties <- c("L", "L (Ind L) (Lab)", "L (C)", "NL (IND) (NL)", "NL (Ind L)",
    "NL(C)", "NL (Ind L) (L)", "Liberal", "L (Ind) (C)", "NL (Nat Ind)",
    "L (Ind L) (NL)", "NL (L)", "Co L", "L (NL)", "NL", "L (CW)", "L (LU)", "Ind L",
    "L (Co L)", "Ind L", "Co L (L)", "Co L (BSP) (Com)", "Ind L (Lab)", "Co L (Ind)",
    "L(LU)", "Ind L/Crf (LU)", "Ind L (LU)", "Ind L/Crf", "L/Crf", "Ind. L",
    "Ind L (L)", "L (Ind L)", "L (Co L) (L)",  "co L"    )
labour_parties <- c('L/Lab', 'Lab', 'Lab (L/Lab)', "L/Lab (Lab)", "Lab/Co-op",
    "Lab (N Lab)", "N Lab", "NI Lab", "Lab U", "LRC", "NI Lab (lab) (Ind Lab)",
    "Lab (Ind Lab) (Lab)", "Lab (Ind)", "Co Lab", "N Lab (Nat)", "LAB",
    "Lab (Ind Lab)", "Lab (Ind) (Lab)", "Lab (Ind) (C)", "Co-op (Lab/Co-op)",
    "N Lab (Nat Ind)", "Lab (NP)", "Co-op", "ILP (Lab)", "Lab (Ind) (Lab)",
    "Ind Lab (ILP) (Lab)", "ILP", "Lab (Ind Lab)")
es_elections[, `:=`(
    left = 1 * (party %in% leftist_parties),
    labour = 1 * (party %in% labour_parties),
    liberal = 1 * (party %in% liberal_parties),
    ind = 1 * (party == "Ind"),
    conservative = 1 * (party %in% conservative_parties))]

es_elections[is.na(unopposed), unopposed := 0]
es_elections[, space_pos := str_locate(name_on_return, " ")[, 1]]
es_elections[, title := substr(name_on_return, 1, space_pos - 1)]
unique(es_elections$title)
titles <- c("Baron", "Sir", "Hon.", "Lord", "Viscount", "Rt.", "Sir.",
    "Viscout", "Count", "Marquess", "Rt", "Earl", "Marquis", "Baroness",
    "Viscountess", "Duchess", "Dame", "Master", "Hon")
es_elections[, titled := as.numeric(title %in% titles)]
setorder(es_elections[, .(n = .N), by = .(title)], -n) %>% print(nrow = 1210)
military_titles <- c("Lieut-Colonel", "Major", "Colonel", "Captain",
    "Brigadier-General", "Capt.", "Major-General", "Commander", "Col.",
    "Lieut-General", "Lt", "Rear-Admiral", "Vice-Admiral", "Lieut-Commander",
    "Admiral", "Brigadier", "Commodore", "Maj", "General", "Col", "Col.Thomas",
    "Squadron", "Flight")
es_elections[, military := as.numeric(title %in% military_titles)]
es_elections_ag <- es_elections[,
    .(total_votes = sum(votes, na.rm = T),
    cons_votes = sum(votes * conservative, na.rm = T),
    unopposed = as.numeric( sum(unopposed, na.rm = T) > 0)),
    by = .(election_id, electors, constituency_id = constituency.id, year,
        seats_up, by_election)]

es_elections_ag[sapply(es_elections_ag, function(x) !is.finite(x))] <- NA

es_elections_ag[, `:=`(cons_share = cons_votes / total_votes
    )]

es_elections_ag
df_elec <- es_elections_ag

df_census_pre <- fread("Data/_Intermediates/uk_census_constituency_aggregates/census_1911_by_1885_constituencies.csv")


df_census_pre
df_census_post <- fread("Data/_Intermediates/uk_census_constituency_aggregates/census_1911_by_1918_constituencies.csv")



df_const_pop <- fread("Data/Constituency Populations/const_pops_1911.csv")
head(df_const_pop)


head(df_elec)
df_elec_pre <- df_elec[year < 1918]
df_elec_post <- df_elec[year >= 1918]
df_elec_pre[constituency_id %in% df_census_pre$constituency_id, constituency_id] %>% unique() %>% length()



df_pre <- merge(df_elec_pre, df_census_pre, by = "constituency_id", all.x = TRUE)
df_post <- merge(df_elec_post, df_census_post, by = "constituency_id", all.x = TRUE)

df_electors <- fread("Data/Constituency Populations/const_pops_1911_with_franchise.csv")
df_electors <- df_electors[!is.na(constituency_id), .(constituency_id, parliamentary_electors_men_1918, local_gov_electors_men_1918)]
df_post <- merge(df_post, df_electors, by = "constituency_id", all.x = TRUE)

df <- rbindlist(list(df_pre, df_post), fill = TRUE)
df

df[, cons_share_1918 := (cons_share[year == 1918 & by_election == FALSE]), by = .(constituency_id)]
df[, cons_share_1900 := (cons_share[year == 1900 & by_election == FALSE]), by = .(constituency_id)]

df[, ]


df[, new_male_electorate_pc := (parliamentary_electors_men_1918 - local_gov_electors_men_1918) / parliamentary_electors_men_1918 * 100]

df <- df[year < 1945][by_election == FALSE & unopposed == 0]

df <- df[, .(constituency_id, year, election_id, cons_pc = cons_share * 100,
    diff_cons_pc_1918 = (cons_share - cons_share_1918) * 100,
    diff_cons_pc_1900 = (cons_share - cons_share_1900) * 100,
    constituency_area, agric, working_class, mf_mining = mf + mining17,
    new_male_electorate_pc
    )]
fwrite(df, "Data/_Clean/analysis_data_uk.csv")


