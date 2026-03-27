if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, magrittr)


# Census place project, calculate population by place
# 1930 places
read_census_place <- function(index) {
    message(index)
    df_place_sub <- fread("Data/Census Place Project/1930/histid_place_crosswalk_1930.csv",
        nrows = 10000000, skip = (index - 1) * 10000000)
    colnames(df_place_sub) <- tolower(colnames(
        fread("Data/Census Place Project/1930/histid_place_crosswalk_1930.csv", nrows = 0)))
    df_place_sub <- df_place_sub[, .(n = .N
        ), by = .(cpp_placeid, potential_match, lat, lon)]
    return(df_place_sub)
}



df_census_place <- lapply(1:13, read_census_place) %>%
    rbindlist()
dim(df_census_place)
head(df_census_place)
df_census_place <- df_census_place[, .(n = sum(n)),
    by = .(cpp_placeid, potential_match, lat, lon)]
dim(df_census_place)
head(df_census_place)

fwrite(df_census_place, "Data/_Intermediates/census_place_project/places_populations_1930.csv")




# 1940 places
read_census_place <- function(index) {
    message(index)
    df_place_sub <- fread("Data/Census Place Project/1940/histid_place_crosswalk_1940.csv",
        nrows = 10000000, skip = (index - 1) * 10000000)
    colnames(df_place_sub) <- tolower(colnames(
        fread("Data/Census Place Project/1940/histid_place_crosswalk_1940.csv", nrows = 0)))
    df_place_sub <- df_place_sub[, .(n = .N
        ), by = .(cpp_placeid, potential_match, lat, lon)]
    return(df_place_sub)
}
# 11 groups of 1000000


df_census_place <- lapply(1:14, read_census_place) %>%
    rbindlist()
dim(df_census_place)
head(df_census_place)
df_census_place <- df_census_place[, .(n = sum(n)),
    by = .(cpp_placeid, potential_match, lat, lon)]
dim(df_census_place)
head(df_census_place)
fwrite(df_census_place, "Data/_Intermediates/census_place_project/places_populations_1940.csv")



# 1930 microdata
process_1930 <- function(index) {
    message(index)
    df_census_sub <- fread("Data/Census US/1930/usa_00035.csv",
        nrows = 10000000, skip = (index - 1) * 10000000)
    colnames(df_census_sub) <- tolower(colnames(
        fread("Data/Census US/1930/usa_00035.csv", nrows = 0)))
    df_census_sub[, age16 := 1 * (age >= 16)]
    df_census_sub[valueh %in% c(0, 9999998, 9999999), valueh := NA]
    df_census_sub[rent30 %in% c(0, 9998, 9999), rent30 := NA]
    df_census_sub <- df_census_sub[, .(n = .N,
        valueh = mean(valueh, na.rm = TRUE),
        n_valueh = sum(!is.na(valueh)),
        rent = mean(rent30, na.rm = TRUE),
        n_rent = sum(!is.na(rent30))
    ), by = .(stateicp, countyicp, farm, sex, race, bpl,
        lit, ind1950, occ1950, ownershp, citizen, speakeng,
        age16, school, urban, radio30, vet1930)]
    return(df_census_sub)
}


indices <- 1:13


df_census <- lapply(indices, process_1930) %>%
    rbindlist()
df_census <- df_census[, .(n = sum(n),
    valueh = weighted.mean(valueh, n_valueh, na.rm = TRUE),
    n_valueh = sum(n_valueh, na.rm = TRUE),
    rent = weighted.mean(rent, n_rent, na.rm = TRUE),
    n_rent = sum(n_rent, na.rm = TRUE)),
    by = .(stateicp, countyicp, sex, race, bpl, lit, occ1950, ind1950, farm,
        ownershp, citizen, speakeng, age16, school, urban, radio30, vet1930)]
fwrite(df_census, "Data/_Intermediates/us_census_compact/census_1930_compact.csv")
