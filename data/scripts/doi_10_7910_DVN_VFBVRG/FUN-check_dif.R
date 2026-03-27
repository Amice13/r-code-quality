#=============================================
#   FUNCTION TO ITERATE THROUGH DIF TESTS, SPLITS
#=============================================
check_dif <- function(
  baseline_model, # MODEL FIT WITHOUT PSEUDO VARIABLES
  df,   # DATA WITH GROUPING VARIABLE AND WEIGHTS
  n_ints = 4, # NUM. OF CLASS INTERVALS
  p_threshold = 0.05 # THRESHOLD TO DIAGNOSE DIF
         ){

  # ------- EXTRACT RELEVANT DATA  ---------------- #


  # GROUPING VARIABLE, WEIGHTS, AND ID
  all_data <- df  %>%
    select(id, group_var, weight_full)

  min_group_value <- min(all_data$group_var, na.rm = TRUE)
  if (min_group_value < 0){
  all_data   <- all_data    %>%
    mutate(group_var = group_var + abs(min_group_value))
  }


  # TYPE OF RASCH MODEL
  mod_type <- baseline_model$irtmodel

  # NAMES OF ITEMS IN SCALE
  all_names <- IRT.data(baseline_model)  %>% colnames()

  # SET INITIAL PARAMETERS
  anovas <- data_frame()
  bad_items <- c()
  run <- 1
  split_items <-  data_frame()


  # ============================================================================
  # REPEATEDLY SPLIT DATA, TEST FOR DIF UNTIL NO REMAINING DIF OR
  #   OUT OF ITEMS TO TEST
  # ============================================================================
  repeat {

    # PRINT MESSAGE ON SCREEN
    items_paste <- paste(bad_items, collapse = ', ')
    message <- paste("\nRun Number", run,
                     "\nSplitting item(s):", items_paste,
                     "\n")
    cat(message, sep = "")


    # FIT NEW RASCH MODEL (AFTER FIRST ANOVA) ----------------------------------
    if( run == 1){
      items <- IRT.data(baseline_model)  %>% tbl_df()
      anova_wide <- bind_cols(all_data, items)
      mod_rasch <- baseline_model
    } else {
    # FIT MODEL
    mod_rasch <- tam.mml(
        resp = items_new,
        irtmodel = mod_type,
        pweights = df$weight_full,
        control = list(progress = FALSE)
      )
    items <- IRT.data(mod_rasch)  %>% tbl_df()
    anova_wide <- bind_cols(all_data, items)
    }

    # EXTRACT ITEM RESIDUALS
    item_names <- names(items)
    item_resids <- IRT.residuals(mod_rasch)$stand_residuals  %>%
      tbl_df()  %>%
      rownames_to_column("row_id")  %>%
      gather(item, residual, one_of(item_names))

    # PREP DATA FOR ANOVA
    anova_data <- anova_wide  %>%
      rownames_to_column("row_id")  %>%
      # RESHAPE FROM WIDE TO LONG DATA FOR ANOVA LOOP
      gather(item, response, one_of(item_names))  %>%
      # MERGE WITH RESIDUALS
      left_join(item_resids, by = c("row_id", "item"))

    # ITEMS WITH MORE THAN ONE GROUP VALUE
    items_to_test <- anova_data  %>%
      filter(is.na(response) == FALSE)  %>%
      group_by(item)  %>%
      summarise(n_vals = length(unique(group_var)))  %>%
      filter(n_vals > 1)  %>%
      select(item)  %>%
      unlist(use.names = FALSE)


    # ESTIMATE ANOVAS & POSTHOC TESTS
anova_list <-  items_to_test  %>%
  sapply(function(x) {
    anova_data_x <- anova_data  %>%
      filter(item == x)
    a1 <- aov(residual ~ factor(group_var),
              data = anova_data_x,
              weights = weight_full)
    posthoc <- TukeyHSD(a1)
    list(anova = a1, posthoc = posthoc)
  }, simplify = FALSE, USE.NAMES = TRUE)

# EXTRACT & FORMAT RESULTS
anova_df <- map(anova_list, `[[`, "anova")   %>%
  map_df(broom::tidy, .id = "item")  %>%
  mutate(bh_pval = p.adjust(p.value, method = "BH"))  %>%
  mutate(run = run)

posthoc_df <- map(anova_list, `[[`, "posthoc")  %>% map(1)


# IDENTIFY WORST ITEM
results <- anova_df  %>% filter(term != "Residuals")
max_stat <- max(results$statistic, na.rm = TRUE)
max_item <- results$item[results$statistic == max_stat]
max_item_p <- results$bh_pval[results$statistic == max_stat]


posthoc_df_max <- posthoc_df[[max_item]]
posthoc_df_names <- rownames(posthoc_df_max)  %>% strsplit("-")
posthoc_a <- map_chr(posthoc_df_names, 1)
posthoc_b <- map_chr(posthoc_df_names, 2)
posthoc_pvals  <- posthoc_df_max[, "p adj"]
min_posthoc_p <- min(posthoc_pvals, na.rm = TRUE)


# ALWAYS SEPARATE GROUPS WITH MINIMUM P-VALUE IF max_item_p IS statistically significant
if(max_item_p < p_threshold) {
  posthoc_pvals[posthoc_pvals == min(posthoc_pvals, na.rm = TRUE)] <- 0
}
posthoc_groupvals <- c(posthoc_a, posthoc_b)  %>% unique()
posthoc_groupvals <- posthoc_groupvals[order(posthoc_groupvals)]
n_vals <-  length(posthoc_groupvals)

# VALUES OF GROUP VARIABLE WITHOUT DIF ON WORST ITEM
mat <- matrix(1, nrow = n_vals, ncol = n_vals)
rownames(mat) <- colnames(mat) <- posthoc_groupvals
for(i in seq_along(posthoc_df_names)) {
  p_i <- posthoc_pvals[i]
  mat[posthoc_a[i], posthoc_b[i]] <-
    mat[posthoc_b[i], posthoc_a[i]] <- p_i
}
stopifnot(isSymmetric(mat))

groups <- rep(NA, n_vals)
mat_new <- mat
diag(mat_new) <- 0

# GROUP VALUES OF group_var BASED ON TUKEY POSTHOC TESTS
while (anyNA(groups)) {
  best_pair <- which(mat_new == max(mat_new),
                     arr.ind = TRUE,
                     useNames = FALSE)[1, ]
  group_a <- groups[best_pair[1]]
  group_b <- groups[best_pair[2]]
  others <- which(groups == group_b)
  mat_others <- mat[best_pair[1], others]
  min_others <- ifelse(length(mat_others) >= 1, min(mat_others), 1)
  all_na <- is.na(groups)  %>% all()
  max_group <- ifelse(all_na, 0, max(groups, na.rm = TRUE))
  if (is.na(group_b)) {
    group_b = max_group + 1
  }
  if (is.na(group_a)) {
    if (mat_new[best_pair[1], best_pair[2]] > 0.05 &
        min_others > 0.05) {
      group_a = group_b
    }
    if (mat_new[best_pair[1], best_pair[2]] < 0.05) {
      group_a = max_group + 1
    }
  }
  mat_new[best_pair[1], best_pair[2]] <- -1
  groups[best_pair[1]] <- group_a
}



# PSEUDO CODED ITEM FOR EACH GROUP THAT MUST BE SPLIT -----------
  split_items <- unique(groups)  %>%
    lapply(function(x) {
      item_vector <- items  %>%
        select(matches(max_item))  %>%
        unlist(use.names = FALSE)
      the_vals <- posthoc_groupvals[which(groups == x)]  %>% as.numeric()
      the_vals <- the_vals[order(the_vals)]
      valname <- paste0(the_vals, collapse = "")
      stub_name <- gsub('[[:digit:]]+', '', max_item)  %>%
        strsplit(split = '__')  %>%
        unlist()
      col_name <- paste(stub_name, valname, sep = "_")
      pseudo_col <- all_data  %>%
          mutate(item_raw = item_vector)  %>%
          mutate(pseudo = ifelse(group_var %in% the_vals,
                                 item_raw,
                                 NA_integer_))  %>%
          select(pseudo)  %>%
          set_names(col_name)
      return(pseudo_col)
    })  %>%
      bind_cols()

    # UPDATE DATA WITH NEW SPLITS
    items_new <- items  %>%
      select(-matches(max_item))  %>%
      bind_cols(split_items)

    # IF WORST ITEM IS < THRESHOLD, ADD TO SPLIT LIST
    if (max_item_p < p_threshold & min_posthoc_p < p_threshold) {
      bad_items <- c(bad_items, max_item)
    }

    #  ITEMS YET TO FIND DIF
    good_items <- all_names[!all_names %in% bad_items]

    # EXTRACT PERSON ABILITY PARAMETER
    ability <- data_frame(ability = tam.wle(mod_rasch)$theta)

    # UPDATE ANOVA DATA
    anovas <- bind_rows(anovas, anova_df)

    # END IF P-VALUE OF MAX STAT IS > THRESHOLD
    if (max_item_p >= p_threshold |
        min_posthoc_p >= p_threshold)
      break

    # END IF NO ITEMS REMAIN UNSPLIT
    if (length(good_items) == 0)
      break

    # UPDATE COUNT
    run <- run + 1

  }
  results_list <- list(anovas = anovas, # SUMMARY OF ALL ANOVAS
                       good_items = good_items, # ITEMS WITHOUT DIF ON LAST RUN
                       ability = ability, # ABILITY FROM LAST RASCH MODEL
                       mod_final = mod_rasch # FINAL RASCH MODEL FIT
                       )


  if (length(good_items) == 0) {
    cat("NO ITEMS WITHOUT DIF")
  }

  if (length(good_items) > 0) {
    gooditems_paste <- paste(good_items, collapse = ', ')
    end_message <- paste("ITEMS WITHOUT DIF:", gooditems_paste)
    cat(end_message, sep = "")
  }

  return(results_list)
}



