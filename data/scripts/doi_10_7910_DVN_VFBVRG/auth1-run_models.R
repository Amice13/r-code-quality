################################################################################
# Created By: Pietryka
# Creation Date:  2017-03-09
# Purpose: RUN RASCH MODELS FOR AUTHORITARIANISM
# R Version: R version 3.3.3 (2017-03-06)
# Data Input: Data/Derived/2012/groups_and_scales.csv
# NOTES: 'groups_and_scales.csv' created in DataClean\anes-4-define_scales.R
#
# Questions: mpietryka@fsu.edu
################################################################################


# 1. PREAMBLE  ===================================


#  LOAD PACKAGES
library(tidyverse) # MANY CONVENIENCE FUNCTIONS
library(TAM)       # RASCH MODELS
library(survey)    # WEIGHTED REGRESSION MODELS


# LOAD DATA
load("Data/Derived/2012/groups_and_scales.RData")

data_raw_12 <- a12_scales  %>%
  rownames_to_column("id")


# LOAD OBJECTS THAT SET FIGURE PREFERENCES
source("Analysis/report-results/Figures/figure_preferences.R")

# LOAD OBJECTS THAT IDENTIFY SCALE ITEMS
source("DataClean/scale_items.R")


# EXTRACT AUTHORITARIANISM - VOTE DATA
auth_df <- data_raw_12  %>%
  select(
    caseid,
    weight_full,
    psu_full,
    strata_full,
    votechoice,
    one_of(auth_vars)
         )







# 2. FIRST RUN  ===================================

# ITEM DATA
items_1 <- select(auth_df, one_of(auth_vars))

# GROUPING VARIABLE, WEIGHTS, AND ID
all_data <- auth_df  %>%
  select(caseid, votechoice, weight_full)


# INITIAL VALUES
run <- 1L
anovas <- data_frame()

# _2A. FIT BASELINE MODEL -----------

mod_1 <- tam.mml(
  resp = items_1,
  irtmodel =  "1PL",
  pweights = all_data$weight_full,
  control = list(progress = FALSE)
)

# EXTRACT PERSON ABILITY PARAMETER
ability_1 <- data_frame(ability = tam.wle(mod_1)$theta)

# EXTRACT ITEM RESIDUALS
item_names <- names(items_1)
item_resids <- IRT.residuals(mod_1)$stand_residuals  %>%
  tbl_df()  %>%
  rownames_to_column("row_id")  %>%
  gather(item, residual, one_of(item_names))

# PREP DATA FOR ANOVA
anova_wide_1 <- bind_cols(all_data, items_1)
anova_data_1 <- anova_wide_1  %>%
  rownames_to_column("row_id")  %>%
  # RESHAPE FROM WIDE TO LONG DATA FOR ANOVA LOOP
  gather(item, response, one_of(item_names))  %>%
  # MERGE WITH RESIDUALS
  left_join(item_resids, by = c("row_id", "item"))

# RUN ANOVAS
run_anova <- function(anova_data){
  anova_data$item %>%
    unique()  %>%
    sapply(function(x) {
      anova_data_x <- anova_data  %>%
        filter(item == x)
      a1 <- aov(residual ~ factor(votechoice),
                data = anova_data_x,
                weights = weight_full)
      a1  %>%
        broom::tidy(.id = "item")  %>%
        mutate(item = x)  %>%
        mutate(bh_pval = p.adjust(p.value, method = "BH")) %>%
        mutate(run = run)  %>%
        select(item, run, everything())
    }, simplify = FALSE, USE.NAMES = TRUE)  %>%
    bind_rows()
}

anova_df_1 <- run_anova(anova_data_1)


# IDENTIFY WORST ITEM
results <- anova_df_1  %>% filter(term != "Residuals")
max_stat <- max(results$statistic, na.rm = TRUE)
max_item <- results$item[results$statistic == max_stat]
max_item_p <- results$bh_pval[results$statistic == max_stat]




# _2B. PSEUDO CODED ITEM FOR EACH GROUP THAT MUST BE SPLIT -----------
split_items <- function(values, item_data, item_to_split){
  values  %>%
    lapply(function(x) {
      item_vector <- item_data  %>%
        select(matches(item_to_split))  %>%
        unlist(use.names = FALSE)
      stub_name <- gsub('[[:digit:]]+', '', item_to_split)  %>%
        strsplit(split = '__')  %>%
        unlist()
      col_name <- paste(stub_name, x, sep = "_")
      pseudo_col <- all_data  %>%
        mutate(item_raw = item_vector)  %>%
        mutate(pseudo = ifelse(votechoice == x, item_raw, NA_integer_))  %>%
        select(pseudo)  %>%
        set_names(col_name)
      return(pseudo_col)
    })  %>%
    bind_cols()
}

split_items_1 <- all_data$votechoice  %>%
  na.omit()  %>%
  unique()  %>%
  split_items(items_1, max_item)


# 3. SECOND RUN ========================================

# UPDATE COUNT
run <- run + 1


# UPDATE DATA WITH NEW SPLITS
items_2 <- items_1  %>%
  select(-matches(max_item))  %>%
  bind_cols(split_items_1)

# _3A. FIT NEW MODEL -----------

mod_2 <- tam.mml(
  resp = items_2,
  irtmodel =  "1PL",
  pweights = all_data$weight_full,
  control = list(progress = FALSE)
)

# EXTRACT PERSON ABILITY PARAMETER
ability_2 <- data_frame(ability = tam.wle(mod_2)$theta)

# EXTRACT ITEM RESIDUALS
item_names <- names(items_2)
item_resids <- IRT.residuals(mod_2)$stand_residuals  %>%
  tbl_df()  %>%
  rownames_to_column("row_id")  %>%
  gather(item, residual, one_of(item_names))

# PREP DATA FOR ANOVA
anova_data_2 <- bind_cols(all_data, items_2)  %>%
  rownames_to_column("row_id")  %>%
  # RESHAPE FROM WIDE TO LONG DATA FOR ANOVA LOOP
  gather(item, response, one_of(item_names))  %>%
  # MERGE WITH RESIDUALS
  left_join(item_resids, by = c("row_id", "item"))  %>%
  # FOCUS ONLY ON ORIGINAL ITEMS
  filter(item %in% auth_vars)


# RUN ANOVAS
anova_df_2 <- run_anova(anova_data_2)


# IDENTIFY WORST ITEM
results <- anova_df_2  %>% filter(term != "Residuals")
max_stat <- max(results$statistic, na.rm = TRUE)
max_item <- results$item[results$statistic == max_stat]
max_item_p <- results$bh_pval[results$statistic == max_stat]


# _3B. PSEUDO CODED ITEM FOR EACH GROUP THAT MUST BE SPLIT -----------
split_items_2 <- all_data$votechoice  %>%
  na.omit()  %>%
  unique()  %>%
  split_items(items_2, max_item)



# 4. THIRD RUN =============================================

# UPDATE COUNT
run <- run + 1


# UPDATE DATA WITH NEW SPLITS
items_3 <- items_2  %>%
  select(-matches(max_item))  %>%
  bind_cols(split_items_2)

# _4A. FIT NEW MODEL -----------

mod_3 <- tam.mml(
  resp = items_3,
  irtmodel =  "1PL",
  pweights = all_data$weight_full,
  control = list(progress = FALSE)
)

# EXTRACT PERSON ABILITY PARAMETER
ability_3 <- data_frame(ability = tam.wle(mod_3)$theta)

# EXTRACT ITEM RESIDUALS
item_names <- names(items_3)
item_resids <- IRT.residuals(mod_3)$stand_residuals  %>%
  tbl_df()  %>%
  rownames_to_column("row_id")  %>%
  gather(item, residual, one_of(item_names))

# PREP DATA FOR ANOVA
anova_data_3 <- bind_cols(all_data, items_3)  %>%
  rownames_to_column("row_id")  %>%
  # RESHAPE FROM WIDE TO LONG DATA FOR ANOVA LOOP
  gather(item, response, one_of(item_names))  %>%
  # MERGE WITH RESIDUALS
  left_join(item_resids, by = c("row_id", "item"))  %>%
  # FOCUS ONLY ON ORIGINAL ITEMS
  filter(item %in% auth_vars)


# RUN ANOVAS
anova_df_3 <- run_anova(anova_data_3)


# IDENTIFY WORST ITEM
results <- anova_df_3  %>% filter(term != "Residuals")
max_stat <- max(results$statistic, na.rm = TRUE)
max_item <- results$item[results$statistic == max_stat]
max_item_p <- results$bh_pval[results$statistic == max_stat]


# _4B. PSEUDO CODED ITEM FOR EACH GROUP THAT MUST BE SPLIT -----------
split_items_3 <- all_data$votechoice  %>%
  na.omit()  %>%
  unique()  %>%
  split_items(items_3, max_item)




# 5. FOURTH RUN ===========================================

# UPDATE COUNT
run <- run + 1


# UPDATE DATA WITH NEW SPLITS
items_4 <- items_3  %>%
  select(-matches(max_item))  %>%
  bind_cols(split_items_3)

# _5A. FIT NEW MODEL -----------

mod_4 <- tam.mml(
  resp = items_4,
  irtmodel =  "1PL",
  pweights = all_data$weight_full,
  control = list(progress = FALSE)
)

# EXTRACT PERSON ABILITY PARAMETER
ability_4 <- data_frame(ability = tam.wle(mod_4)$theta)

# EXTRACT ITEM RESIDUALS
item_names <- names(items_4)
item_resids <- IRT.residuals(mod_4)$stand_residuals  %>%
  tbl_df()  %>%
  rownames_to_column("row_id")  %>%
  gather(item, residual, one_of(item_names))

# PREP DATA FOR ANOVA
anova_data_4 <- bind_cols(all_data, items_4)  %>%
  rownames_to_column("row_id")  %>%
  # RESHAPE FROM WIDE TO LONG DATA FOR ANOVA LOOP
  gather(item, response, one_of(item_names))  %>%
  # MERGE WITH RESIDUALS
  left_join(item_resids, by = c("row_id", "item"))  %>%
  # FOCUS ONLY ON ORIGINAL ITEMS
  filter(item %in% auth_vars)


# RUN ANOVAS
anova_df_4 <- run_anova(anova_data_4)


# IDENTIFY WORST ITEM
results <- anova_df_4  %>% filter(term != "Residuals")
max_stat <- max(results$statistic, na.rm = TRUE)
max_item <- results$item[results$statistic == max_stat]
max_item_p <- results$bh_pval[results$statistic == max_stat]

# REMAINING ITEM SHOWS NO DIF







# 6. EXTRACT PERSON ESTIMATES ==============================

ability_df <- auth_df  %>%
  # DUMMY: ARE ALL auth_vars VALID IN ROW?
  mutate(all_valid = complete.cases(select(., one_of(auth_vars))))  %>%
  # OFF-THE-SHELF SUM
  mutate(sum_all = rowSums(select(., one_of(auth_vars)), na.rm = TRUE))  %>%
  # OFF-THE-SHELF MEAN
  mutate(mean_all = rowMeans(select(., one_of(auth_vars)), na.rm = TRUE))  %>%
  # ABILITY ESTIMATES
  mutate(ability_1 = tam.wle(mod_1)$theta)  %>%
  mutate(ability_2 = tam.wle(mod_2)$theta)  %>%
  mutate(ability_3 = tam.wle(mod_3)$theta)  %>%
  mutate(ability_4 = tam.wle(mod_4)$theta)  %>%
  # STANDARDIZE
  mutate_at(vars(mean_all, starts_with("ability")), scale)

# CORRELATION BETWEEN ESTIMATES
ability_df  %>%
  select(mean_all, starts_with("ability"))  %>%
  cor(use = "pairwise.complete.obs")

# CREATE WEIGHTED DATA
survey_df <- ability_df %>%
  svydesign(
    #NOTE: PSU VARIABLE IS INCOMPLETE IN 2016 DATA
    ids = ~ psu_full,
    strata = ~ strata_full ,
    data = . ,
    weights = ~ weight_full ,
    nest = TRUE
  )





# 7. EXTRACT ITEM DIFFICULTIES ========================

# FUNCTION TO EXTRACT ITEM DATA
extract_items <- function(mod, id){
  mod[["item"]]  %>%
    tbl_df()  %>%
    mutate(mod = id)  %>%
    mutate(item = as.character(item))  %>%
    select(mod, item, N, M, xsi.item)
}


# EXTRACT ITEM DATA
mod_list <- list(m1 = mod_1, m2 = mod_2, m3 = mod_3, m4 = mod_4)
raw_df <- map2_df(mod_list, seq_along(mod_list), extract_items)


