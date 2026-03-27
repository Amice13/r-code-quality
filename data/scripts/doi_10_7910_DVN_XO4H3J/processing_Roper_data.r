#%% loading packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, stringr)


YynN_index <- function(q_vec) {
    out_vec <- rep(NA, length(q_vec))
    if (!all(q_vec %in% c("Yes", "No", "YES!", "NO!", "", "Yes (Emphatic)", "No (Emphatic)"))) {
        message("other responses")
    }
    out_vec[q_vec %in% c("YES!", "Yes (Emphatic)")] <- 1
    out_vec[q_vec == "Yes"] <- 0.75
    out_vec[q_vec == "No"] <- 0.25
    out_vec[q_vec %in% c("NO!", "No (Emphatic)")] <- 0
    return(out_vec)
}

yn_index <- function(vec) {
    out_vec <- rep(NA, length(vec))
    if (!all(vec %in% c("Yes", "No", ""))) {
        message("other responses")
    }
    out_vec[vec == "Yes"] <- 1
    out_vec[vec == "No"] <- 0
    return(out_vec)
}

lib_con_index <- function(vec) {
    if (!all(vec %in% c("liberal", "Liberal", "conservative", "Conservative", "",
        "Conservatives", "Liberals"))) {
        message("other responses")
    }
    out_vec <- rep(NA, length(vec))
    out_vec[vec %in% c("liberal", "Liberal", "Liberals")] <- 1
    out_vec[vec %in% c("conservative", "Conservative", "Conservatives")] <- 0
    return(out_vec)
}

more_lib_index <- function(vec) {
    if (!all(vec %in% c("More liberal", "More Liberal", "More conservative",
        "More conservative", "About the same", "Same", ""))) {
        message("other responses")
    }
    out_vec <- rep(NA, length(vec))
    out_vec[vec %in% c("More liberal", "More Liberal")] <- 1
    out_vec[vec %in% c("More conservative", "More Conservative")] <- 0
    out_vec[vec %in% c("About the same", "Same")] <- 0.5
    return(out_vec)
}

too_little_index <- function(vec) {
    if (!all(vec %in% c("Too little", "Too much", "About right amount", "", "About right"))) {
        message("other responses")
    }
    out_vec <- rep(NA, length(vec))
    out_vec[vec %in% c("Too little")] <- 1
    out_vec[vec %in% c("Too much")] <- 0
    out_vec[vec %in% c("About right amount", "About right")] <- 0.5
    return(out_vec)
}

spend_more_index <- function(vec) {
    if (!all(vec %in% c("Increased", "Decreased", "Remain about the same", "Increase",
        "Decrease", "About the same", "", "Remain Same"))) {
        message("other responses")
    }
    out_vec <- rep(NA, length(vec))
    out_vec[vec %in% c("Increased", "Increase")] <- 1
    out_vec[vec %in% c("Decreased", "Decrease")] <- 0
    out_vec[vec %in% c("Remain about the same", "About the same", "Remain Same")] <- 0.5
    return(out_vec)
}

#%% questions about relief

read_number_string <- function(number) {
    folders <- list.files("Data/Roper Poll")
    folders
    if (number < 100) {
        number_string <- paste0("00", number)
    } else {
        number_string <- paste0("0", number)
    }
    number_string
    folder_val <- folders[str_detect(folders, number_string)]
    file_val <- substr(folder_val, 8, nchar(folder_val))
    file_val
    file_loc <- paste0("Data/Roper Poll/", folder_val, "/USAIPO", file_val, ".csv")
    df <- fread(file_loc)
    df[, survey_wave := file_val]
    df[, rownumber := 1:nrow(df)]
    print(colnames(df))
    return(df)
}



df_56 <- read_number_string(56)



df_56[, roosevelt_more_lib := more_lib_index(Q3)]

# do you favor the compulsory old-age insurance plan?
df_56[, favor_old_age_insurance := yn_index(Q9)]



df_65 <- read_number_string(65)
df_65[, favor_old_age_insurance := yn_index(Q5)]



df_68 <- read_number_string(68)
df_68[, second_aaa := yn_index(Q7B)]

df_69 <- read_number_string(69)
df_69[, second_aaa := yn_index(Q6)]
df_69[, lib_con_party := lib_con_index(Q9)]


df_77 <- read_number_string(77)
df_77[, spending_too_little := too_little_index(Q3)]
df_77[, tax_3000 := as.numeric(Q5A)]
df_77[, tax_5000 := as.numeric(Q5B)]
df_77[, tax_10000 := as.numeric(Q5C)]

df_77[, tax_combined := 1/3 * (tax_3000 / 3000 + tax_5000 / 5000 + tax_10000 / 10000)]



df_88 <- read_number_string(88)



df_88[, .N, Q11]
df_88[Q11 == "Conservative", nd_con := 0]
df_88[Q11 == "New Dealer", nd_con := 1]



df_94 <- read_number_string(94)
df_94[, lib_con_party := lib_con_index(Q4B)]



df_101 <- read_number_string(101)

df_101[, increase_ue_relief := spend_more_index(Q5B)]


df_103 <- read_number_string(103)
# question asks if Roosevelt is too liberal, recode to would you like Roosevelt
# to be more liberal
df_103[Q10B == "Too liberal", roosevelt_more_lib := 0]
df_103[Q10B == "Too conservative", roosevelt_more_lib := 1]
df_103[Q10B == "About right", roosevelt_more_lib := 0.5]


df_104 <- read_number_string(104)
# are Roosevelt's policies too liberal, asked of Democrats only
df_104[Q8B == "Too liberal", roosevelt_more_lib := 0]
df_104[Q8B == "Too conservative", roosevelt_more_lib := 1]
df_104[Q8B == "About right", roosevelt_more_lib := 0.5]

df_107 <- read_number_string(107)
df_107[, approve_ss_tax := yn_index(Q8)]


df_109 <- read_number_string(109)


# If Roosevelt is not a candidate, would you prefer a Conservative or a New Dealer?
df_109[, .(n = .N), by = .(Q11A)]
df_109[Q11A == "Conservative", nd_con := 0]
df_109[Q11A == "New Dealer", nd_con := 1]




df_118 <- read_number_string(118)

df_118[, lib_con_party := lib_con_index(Q2B)]

df_120 <- read_number_string(120)


# do you think government spending should be increased to help get business
# out of its present slump?
df_120[, increase_spending := YynN_index(Q2BA)]


df_123 <- read_number_string(123)
df_123[, increase_spending := YynN_index(Q11)]

df_125 <- read_number_string(125)
# During the next two years would you like to see the Roosevelt administration be
# more liberal or more conservative Q4A

df_125[, roosevelt_more_lib := more_lib_index(Q4B)]
df_125[is.na(roosevelt_more_lib), roosevelt_more_lib := more_lib_index(Q4A)]

df_126 <- read_number_string(126)

# If FDR doesn't run, should Dems nominate conservative or New Dealer
df_126[Q11A == "Conservative", nd_con := 0]
df_126[Q11A == "New Dealer", nd_con := 1]

df_127 <- read_number_string(127)


# do you approve of the present social security laws
df_127[, approve_ss := YynN_index(Q4)]

df_127[, lib_con_party := lib_con_index(Q10)]



df_132 <- read_number_string(132)
df_132[, lib_con_party := lib_con_index(Q8C)]



df_138 <- read_number_string(138)
df_138[Q7A == "Conservative", nd_con := 0]
df_138[Q7A == "New Dealer", nd_con := 1]
df_138



ls() %>% print()

data_frames <- list(df_101, df_103, df_104, df_107, df_109,
    df_118, df_120, df_123, df_125, df_126, df_127, 
    df_132, 
    df_56, 
    df_65,
    df_68, df_69, 
    df_77,
    df_88, df_94
    )
df <- rbindlist(data_frames, use.names = TRUE, fill = TRUE)

cols_to_keep <- colnames(df)[!str_detect(colnames(df), "Q")]
df <- df[, ..cols_to_keep]
unique(df$size)

df[, .N, by = .(size, urban)]
df[urban == "Urban", rural := 0]
df[urban == "Non-Urban", rural := 1]

df[urban == "Urban", urban_b := 1]
df[urban == "Non-Urban", urban_b := 0]

df[, weight := WtPubComp]
df[is.na(weight), weight := WtPubFeas]


df[, t_3000 := tax_3000 / 3000]
df[, t_5000 := tax_5000 / 5000]
df[, t_10000 := tax_10000 / 10000]



df[, t_ave := (t_3000 + t_5000 + t_10000) / 3]

df[, tax_combined := tax_combined * 100]
df
cols_to_keep <- c( "survey_wave", "rownumber", "urban_b", "region", "black", "OCCUPATION1",
    "female", "weight", "increase_spending", "spending_too_little",
    "tax_combined",
    "nd_con", "roosevelt_more_lib",
    "lib_con_party", 
    "increase_ue_relief",
    "second_aaa",
    "favor_old_age_insurance",
    "approve_ss", "approve_ss_tax")
df <- df[south == "Non-South", ..cols_to_keep]


fwrite(df, "Data/_Clean/roper_surveys_policy.csv")


# now read in all for regressions of vote-choice on urban status
read_number_string <- function(number) {
    folders <- list.files("Data/Roper Poll")
    folders
    if (number < 100) {
        number_string <- paste0("00", number)
    } else {
        number_string <- paste0("0", number)
    }
    number_string
    folder_val <- folders[str_detect(folders, number_string)]
    file_val <- substr(folder_val, 8, nchar(folder_val))
    file_val
    file_loc <- paste0("Data/Roper Poll/", folder_val, "/USAIPO", file_val, ".csv")
    df <- fread(file_loc)
    df[, survey_wave := file_val]
    df[, rownumber := 1:nrow(df)]
    print(colnames(df))
    return(df)
}


numbers <- c( 56, 57, 59:69, 71:142)
df <- rbindlist(lapply(numbers, read_number_string), fill = TRUE)
print(colnames(df))

df <- df[, .(rownumber, urban, south, OCCUPATION1, state, female, black, size, VOTE_PRO, VOTE_RETRO, WtPubComp, WtPubFeas, WtVotComp, WtVotFeas, year,city, survey_wave)]



df[OCCUPATION1 %in% c("Skilled workers", "Unskilled workers", "Unskilled white collar"), occ_recode := "Labor"]
df[OCCUPATION1 %in% c("Other and None", "Other & None", "Other and none"), occ_recode := "Other"]
df[OCCUPATION1 %in% c("Unemployed", "wpa"), occ_recode := "Unemployed"]
df[OCCUPATION1 %in% c("Professional", "Skilled white collar", "Business", "Businessmen"), occ_recode := "Professional"]


df[VOTE_RETRO %in% c("fdr", "Landon"), vote_fdr := VOTE_RETRO == "fdr"]



df[female %in% c("Male", "Female"), female_b := female == "Female"]
df[black %in% c("Black", "White"), black_b := black == "Black"]

df[, weight := WtPubComp]
df[is.na(WtPubComp), weight := WtPubFeas]

df[size %in% c("100,000-500,000", "2,500-25,000", "25,000-100,000", "Over 500,000") &
    is.na(urban), urban := "Urban"]
df[size %in% c("Small town", "Farm") & is.na(urban), urban := "Non-Urban"]

df[urban %in% c("Urban", "Non-Urban"), urban_b := urban == "Urban"]


df[size %in% c("Small town", "Farm", "2,500-25,000", "Urban - 2,500-25,000", "Small Town", "2500-25,000"),
    city_25 := FALSE]

df[size %in% c("Over 500,000", "100,000-500,000", "25,000-100,000", "Urban - 25,000-100,000", "Urban - over 500,000", "Urban - 100,000-500,000"),
    city_25 := TRUE]

df[size %in% c("Small town", "Farm", "2,500-25,000", "Urban - 2,500-25,000",
    "Small Town", "2500-25,000",  "Urban - 25,000-100,000","25,000-100,000"),
    city_100 := FALSE]

df[size %in% c("Over 500,000", "100,000-500,000",  "Urban - over 500,000", "Urban - 100,000-500,000"),
    city_100 := TRUE]

df[size %in% c("Small town", "Farm", "2,500-25,000", "Urban - 2,500-25,000",
    "Small Town", "2500-25,000",  "Urban - 25,000-100,000","25,000-100,000",
    "100,000-500,000", "Urban - 100,000-500,000"),
    city_500 := FALSE]

df[size %in% c("Over 500,000",   "Urban - over 500,000"),
    city_500 := TRUE]

df[south %in% c("South", "Non-South"), south_b := south == "South"]




df <- df[, .(survey_wave, rownumber, state, vote_fdr, urban_b, city_25, occ_recode, female_b, black, south_b, weight)]

fwrite(df, "Data/_Clean/roper_surveys_vote_choice.csv", na = "NA")







df_state <- fread("Data/NHGIS/nhgis0069_ds78_1940_state.csv")
df_state
df_state <- df_state[, .(state = STATE, STATEICP, pop = BV7001, pop_urban = BW1001, pop_city = BXD001)]
df_state

df_agg <- df[!is.na(urban_b) & !is.na(weight), .(sample_urban = weighted.mean(urban_b, weight), weights = sum(weight)), by = .(state)]
df_agg <- merge(df_agg, df_state, by = "state")
df_agg[, census_urban := pop_urban / pop]




confederacy_states <- c("North Carolina", "South Carolina", "Arkansas", "Louisiana",
    "Georgia", "Texas", "Oklahoma", "Tennessee", "Kentucky", "Florida", "Alabama",
        "Mississippi", "Virginia")
df_agg[state %in% confederacy_states, southern := "South"]
df_agg[!(state %in% confederacy_states), southern := "Non-South"]
df_agg <- df_agg[, .(state, southern, weights, sample_urban, census_urban)]
fwrite(df_agg, "Data/_Clean/roper_state_urban_totals.csv")



