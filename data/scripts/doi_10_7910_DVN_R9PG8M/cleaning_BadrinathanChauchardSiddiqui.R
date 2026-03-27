# Code for cleaning and stacking dataset for Misinformation and Support for Vigilantism
# Authors: Sumitra Badrinathan & Josiah Gottfried

library(stringi)

#### LOAD DATA ####

setwd("~/Dropbox/Misinformation in India & Pakistan/APSR/Acceptance/Dataverse/")

india <- read.csv("data_india_original.csv")[-1]
pak <- read.csv("data_pakistan_original.csv")[-1]


#### FUNCTIONS ####

find.replace <- function(data, pattern, replacement, variables = 1:length(data), approx = FALSE){
  # THIS FUNCTION REPLACES ALL PATTERNS WITH THEIR INDICATED REPLACEMENTS FOR (SPECIFIED
  # COLUMNS WITHIN) A DATA FRAME
  
  # INPUT
  #   data: A DATA FRAME
  #   pattern: A VECTOR OF THE PATTERNS TO BE REPLACED
  #   replacement: A VECTOR OF THE REPLACEMENTS, IN THE SAME ORDER AS pattern
  #   variables: THE CHARACTER COLUMNS ON WHICH TO PERFORM THE OPERATION; BY DEFAULT,
  #     ALL COLUMNS IN THE DATA FRAME ARE INCLUDED
  #   approx: IF TRUE, INDICATES THAT ANY COLUMNS WHOSE NAMES CONTAIN ANY OF THE 
  #     VARIABLE NAMES SPECIFIED IN THE variables ARGUMENT SHOULD BE INCLUDED (E.G.,
  #     var1 AND var2 WILL BE INCLUDED IF variables = "var")
  
  # OUTPUT
  #   data: A DATA FRAME IDENTICAL TO THE INPUT DATA, EXCEPT WITH ALL PATTERNS REPLACED
  
  # Include variables with similar names if approx is true
  if(approx == TRUE){
    var_pattern <- paste(variables, collapse = "|")
    variables <- grepl(var_pattern, colnames(data))
  }
  
  # Check for transitivity (i.e., A replaced with B, B replaced with C, causes A 
  # to be replaced with C)
  transitive <- FALSE
  if(any(pattern %in% replacement))
    for(i in 1:length(pattern))
      if(pattern[i] %in% replacement[1:i]){
        transitive <- TRUE
        break
      }
  
  # If transitivity exists, intermediate with a unique replacement
  if(transitive){
    final_replacement <- replacement
    replacement <- as.character(runif(length(replacement)))
  }
  
  # Replace patterns with replacements
  data[variables] <- data.frame(apply(data[variables], 2,
                                      stri_replace_all_regex, 
                                      pattern = paste("^", pattern, "$", sep = ""), 
                                      replacement = replacement,
                                      vectorize_all = FALSE))
  
  # If transitivity exists, replace intermediary replacement with final replacement
  if(transitive){
    
    data[variables] <- data.frame(apply(data[variables], 2,
                                        stri_replace_all_regex, 
                                        pattern = replacement, 
                                        replacement = final_replacement,
                                        vectorize_all = FALSE))
    
  }
  
  # Return the final dataset
  return(data)
  
}


#### RECODE FOR CONSISTENCY ####

# Convert numeric data to character for easier find and replace
india_numeric_cols <- colnames(india)[sapply(india, is.numeric)]
pak_numeric_cols <- colnames(pak)[sapply(pak, is.numeric)]
india[india_numeric_cols] <- lapply(india[india_numeric_cols], as.character)
pak[pak_numeric_cols] <- lapply(pak[pak_numeric_cols], as.character)

# Use 98 to represent don't know and 99 to represent refused
pak2 <- find.replace(pak, pattern = c("888", "999"), replacement = c("98", "99"), 
                     variables = names(pak)[-which(names(pak) == "respondent_id")]) # all columns except respondent_id

# Recode yes/no values for consistency
india2 <- find.replace(india, pattern = 2, replacement = 0, variables = "personal_cellphone")
pak3 <- find.replace(pak2, pattern = 2, replacement = 0, 
                     variables = c("cellphone", "internet_access", "general_state_capacity_", 
                                   "tlp_familiar"),
                     approx = T)

# Recode values for education for consistency
india3 <- find.replace(india2, pattern = 6:9, replacement = c(5, 6, 6, 7), variables = "education")
pak4 <- find.replace(pak3, pattern = 1:8, replacement = c(0, 7, 1:6), variables = "education_level")

# Recode Pakistan locality values for consistency with India
pak5 <- find.replace(pak4, pattern = 1:5, replacement = 5:1, variables = "locality")

# Recode income source values for consistency
india4 <- find.replace(india3, pattern = 4:8, replacement = 3:7, variables = "income_source")
pak6 <- find.replace(pak5, variables = "income_source",
                     pattern = c(14, 5:7, 1:3, 8:11, 15, 17, 12, 16, 13, 777), 
                     replacement = c(1, rep(2, 3), rep(3, 9), 4:7))

# Recode Pakistan asset values for consistency
pak7 <- find.replace(pak6, pattern = 1:31, replacement = 1,
                     variables = c("fans", "air_conditioner", "fridge", "indoor_toilets", 
                                   "motorcycle", "water_pump"))

# Recode income for consistency (change from absolute income to relative income)
india5 <- find.replace(india4, pattern = 1:8, replacement = c(1, 2, rep(3, 6)), variables = "income")
pak8 <- find.replace(pak7, pattern = 1:6, replacement = c(rep(1, 4), 2, 3), variables = "income")

# Recode Pakistan "other" values
pak9 <- find.replace(pak8, pattern = 777, replacement = 8, variables = "mother_tongue")
pak10 <- find.replace(pak9, pattern = 777, replacement = 6, variables = "sect")

# Recode perception of govt fairness for consistency
india6 <- find.replace(india5, pattern = 1:2, replacement = 1, variables = "perception_of_state_fairness")
pak11 <- find.replace(pak10, pattern = 1:4, replacement = c(0, rep(1, 3)), variables = "perception_of_govt_fairness")

# Recode vignette order information and Pakistan vignette type names
vig_order_india <- data.frame(first = c(1,1,2,2,3,3), second = c(2,3,1,3,1,2), third = c(3,2,3,1,2,1))
india6[c("first_vignette", "second_vignette", "third_vignette")] <- apply(vig_order_india, 2, function(i) as.character(i[as.integer(india6$scaled_draw_outer)]))
india7 <- find.replace(india6, pattern = 1:3, replacement = c("love_jihad", "corona", "cow_transport"),
                       variables = c("first_vignette", "second_vignette", "third_vignette"))

pak11[c("first_vignette", "second_vignette", "third_vignette")] <- stri_split_fixed(pak11$order_vig, "+", 3, simplify = T)
pak12 <- find.replace(pak11, pattern = 1:3, replacement = c("blasphemy", "quran", "religious_poster"),
                      variables = c("vignette_type", "first_vignette", "second_vignette", "third_vignette"))

#### OTHER PRE-STACK PROCESSING ####

# Final recoded tables
india_recoded <- india7
pak_recoded <- pak12

# Convert originally numeric columns back to numeric
india_recoded[india_numeric_cols] <- lapply(india_recoded[india_numeric_cols], as.numeric)
pak_numeric_cols <- pak_numeric_cols[pak_numeric_cols != "vignette_type"] # vignette type is no longer numeric
pak_recoded[pak_numeric_cols] <- lapply(pak_recoded[pak_numeric_cols], as.numeric)

# Create vignette order column
india_recoded$vignette_order <- dplyr::case_when(india_recoded$vignette_type == india_recoded$first_vignette ~ 1,
                                                 india_recoded$vignette_type == india_recoded$second_vignette ~ 2,
                                                 india_recoded$vignette_type == india_recoded$third_vignette ~ 3)
pak_recoded$vignette_order <- dplyr::case_when(pak_recoded$vignette_type == pak_recoded$first_vignette ~ 1,
                                               pak_recoded$vignette_type == pak_recoded$second_vignette ~ 2,
                                               pak_recoded$vignette_type == pak_recoded$third_vignette ~ 3)

# Create country column
india_recoded$country <- "india"
pak_recoded$country <- "pakistan"

# Standardize enumerator IDs
india_recoded$fi_id <- stri_replace_all_regex(stri_trans_toupper(india_recoded$fi_id), 
                                              pattern = c("O", ",|-|^\\d.*|T$"), 
                                              replacement = c("0", ""), 
                                              vectorize_all = FALSE)
pak_recoded$enumerator <- paste0("X", pak_recoded$enumerator)

# Standardize respondent IDs
india_recoded$respondent_id <- paste("IN", stri_pad(india_recoded$respondent_id, width = 4, pad = "0"), 
                                     sep = "")
pak_recoded$respondent_id <- paste("PK", stri_pad(pak_recoded$respondent_id, width = 4, pad = "0"),
                                   sep = "")

# Combine India asset motorcycle and bicycle columns for consistency with Pakistan data
india_recoded$asset_cycle <- ifelse(india_recoded$asset_5 == 1 | india_recoded$asset_6 == 1, 0, 1)


#### COMBINE THE TWO DATA FRAMES ####

# Rename variables so that they match
new_names <- read.csv("rename_final.csv", fileEncoding = "UTF-8-BOM")
match_india <- match(colnames(india_recoded), new_names$India)
colnames(india_recoded) <- new_names$Both[match_india]
match_pak <- match(colnames(pak_recoded), new_names$Pakistan)
colnames(pak_recoded) <- new_names$Both[match_pak]

# Remove unnecessary rows
india_recoded <- india_recoded[colnames(india_recoded) != ""]
pak_recoded <- pak_recoded[colnames(pak_recoded) != ""]

# Bind rows
stacked_all <- dplyr::bind_rows(india_recoded, pak_recoded)
order_all <- na.omit(match(new_names$Both, colnames(stacked_all)))
stacked_all <- stacked_all[order_all]


#### POST-STACK PROCESSING ####

# Replace 0's with unclear meanings with NAs
stacked_all$misinfo_belief[stacked_all$misinfo_belief == 0] <- NA
stacked_all$support_vigil_self[stacked_all$support_vigil_self == 0] <- NA
stacked_all$support_vigil_neighbors[stacked_all$support_vigil_neighbors == 0] <- NA
stacked_all$punish_mob[stacked_all$punish_mob == 0] <- NA
stacked_all$whatsapp_usage[stacked_all$whatsapp_usage == 8] <- NA # whatsapp_usage = 8 is also unclear
stacked_all$democratic_scale[stacked_all$democratic_scale == 0] <- NA


#### EXPORT ####

write.csv(stacked_all, "data_final.csv")


#### SIMPLIFIED ####
# the simplified dataset converts don't know / refused values to NAs and defines scales so that higher numbers represent stronger agreement with the question

simplify <- read.csv("simplify_data.csv")

# Remove don't know / refused
dkr.remove <- function(x){
  x[x %in% 98:99] <- NA
  return(x)
}

stacked_all[simplify$DKR] <- lapply(stacked_all[simplify$DKR], dkr.remove)

# Flip scales
stacked_all[simplify$Flip] <- lapply(stacked_all[simplify$Flip], function(x) max(x, na.rm = T) + 1 - x)

# Export
write.csv(stacked_all, "data_final_simplified.csv")
