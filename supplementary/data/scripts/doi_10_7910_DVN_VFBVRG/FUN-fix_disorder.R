# THIS FUNCTION TAKES A 'tam.mml' MODEL FOR POLYTOMOUS DATA,
# CHECKS FOR THRESHOLD DISORDER, AND RECODES ITEMS TO ELIMINATE THE DISORDER
fix_disorder <- function(mod, # 'tam.mml' MODEL OBJECT
                         df  # DATA WITH 'weights_full' SURVEY WEIGHTS
){

  # SET PARAMETERS FOR LATER
  neutral_val <- IRT.data(mod)[, 1]  %>% na.omit()  %>% unique()  %>% median()
  any_disordered <- TRUE
  mod_new <- NULL
  no_improvement <- FALSE
  improved_first <- FALSE
  run <- 1
  compare_stats <- data_frame()
  return_scheme <- data_frame()
  return_items  <- data_frame()
  return_model  <- NULL

  # ------- REPEAT LOOP UNTIL NO DISORDER REMAINS   ---------------- #

  repeat {


    # RUN ON FIRST PASS ONLY
    if(run == 1){
      mod_old <- mod
    }

    # DATA IDENTIFYING PROBLEM THRESHOLDS
    deltas <- mod_old$xsi  %>%
      tbl_df()  %>%
      rownames_to_column(var = "item_cat")  %>%
      arrange(item_cat) %>%
      separate(item_cat, into = c("item", "step"), sep = "_Cat")  %>%
      # IDENTIFY VALUES ADJACENT TO THRESHOLD
      mutate(lower_val = as.numeric(step) - 1)  %>%
      mutate(upper_val = as.numeric(step) )  %>%
      group_by(item)  %>%
      # IDENTIFY NEUTRAL VALUE
      mutate(neutral_lower = floor(median(upper_val)))  %>%
      mutate(neutral_upper = ceiling (median(upper_val)))  %>%
      # DIFFERENCE IN XSI B/W CAT & ADJACENT CATS
      mutate(dif_lag = xsi - lag(xsi, 1))  %>%
      mutate(dif_lead = xsi - lead(xsi, 1))  %>%
      # STD. ERRORS OF DIFFERENCES
      mutate(se_lag = sqrt(se.xsi^2 + lag(se.xsi, 1)^2))  %>%
      mutate(se_lead = sqrt(se.xsi^2 + lead(se.xsi, 1)^2))  %>%
      mutate(z_lag = abs(dif_lag/se_lag))  %>%
      mutate(z_lead = dif_lead/se_lead)  %>%
      # FLAG DISORDERED THRESHOLDS (THOSE NOT MONOTONICALLY INCREASING)
      mutate(is_disordered = (xsi != cummax(xsi) & z_lag > 1.96))

    # ARE ANY THRESHOLDS DISORDERED (W/ STAT. SIG. DIFFERENCE)
    any_disordered <- any(deltas$is_disordered)

    # END IF NO DISORDER
    if (any_disordered == FALSE & run == 1){
      disorder_remains <- FALSE
      recoded = FALSE
      end_message <- "\nNo disorder in original scheme"
      cat(end_message)
      break
    }
    if (any_disordered == FALSE & run > 1 & improved_first == TRUE){
      disorder_remains <- FALSE
      recoded = TRUE
      return_items  <- new_items
      return_model  <- mod_new
      end_message <- "\nRecoded and no disorder remains after correction"
      cat(end_message)
      break
    }
    if (any_disordered == FALSE & run > 1 & improved_first == FALSE){
      recoded = FALSE
      no_improvement <- TRUE
      disorder_remains <- TRUE
      return_scheme <- data_frame()
      return_items  <- data_frame()
      return_model  <- NULL
      end_message <- "\nDisorder exists which recoding eliminates, but recoding yields insufficient improvement over original"
      cat(end_message)
      break
    }


    # DATA MAPPING OLD VALUES TO NEW ONES
    disordered_data <- deltas  %>%
      filter(is_disordered)  %>%
      filter(dif_lag == min(dif_lag, na.rm = TRUE))  %>%
      group_by(item, step)  %>%
      nest()  %>%
      mutate(old_lower = map_dbl(data, "lower_val"))  %>%
      mutate(old_upper = map_dbl(data, "upper_val"))  %>%
      select(-data)  %>%
      mutate(old_upper = ifelse(old_lower == neutral_val,
                                old_upper + 1, old_upper))  %>%
      mutate(old_lower = ifelse(old_lower == neutral_val,
                                old_lower + 1, old_lower))  %>%
      mutate(old_lower = ifelse(old_upper == neutral_val,
                                old_lower - 1, old_lower))  %>%
      mutate(old_upper = ifelse(old_upper == neutral_val,
                                old_upper - 1, old_upper))   %>%
      mutate(new_val = old_lower)  %>%
      mutate(run = run)  %>%
      select(run, item, everything())

    # CREATE RECODED ITEMS WITH NEW SCHEME
    new_items <- IRT.data(mod_old)  %>%
      tbl_df()  %>%
      rownames_to_column( var = "id")  %>%
      mutate(id = as.integer(id))  %>%
      gather(item, response, -id)  %>%
      left_join(disordered_data, by = "item")  %>%
      mutate(response = if_else(response == old_lower | response == old_upper,
                                as.integer(new_val),
                                as.integer(response),
                                missing = as.integer(response)))  %>%
      select(id, item, response)  %>%
      spread(key = item, value = response)  %>%
      select(-id)

    # RESCALE RESPONSE SO ALL VALUES ARE ADJACENT TO ANOTHER VALUE
    new_items <-  apply(new_items, 2, function(x){
      recode_data <- data_frame(
        old_vals = na.omit(x)  %>% unique(),
        new_vals = rank(old_vals, ties.method = "min") - 1
      )  %>% arrange(old_vals)
      x_new <- x
      for (i in seq_along(recode_data$old_vals)){
        x_new <- ifelse(x_new == recode_data$old_vals[i],
                        recode_data$new_vals[i],
                        x_new)
      }
      x_new
    })  %>% tbl_df()

    # RUN MODEL ON NEW ITEMS
    mod_new <- tam.mml(
      resp = new_items,
      irtmodel = "PCM",
      pweights = df$weight_full,
      control = list(progress = FALSE))



    # ------- COMPARE MODELS   ---------------- #

    first_mod_stats <- get_relstats(mod)
    old_mod_stats <- get_relstats(mod_old)
    new_mod_stats <- get_relstats(mod_new)

    compare_stats <- data_frame(stat = names(first_mod_stats),
                                first_mod = first_mod_stats,
                                old_mod = old_mod_stats,
                                new_mod = new_mod_stats)  %>%
      filter(!(stat %in% c("person_rel", "item_rel")))  %>%
      mutate(lower_better = stat %in% c("person_insd",
                                        "person_outsd",
                                        "item_insd",
                                        "item_outsd"))  %>%
      mutate(new_beats_first = ifelse(lower_better == TRUE,
                                      new_mod < first_mod,
                                      new_mod > first_mod))  %>%
      mutate(new_beats_old = ifelse(lower_better == TRUE,
                                    new_mod < old_mod,
                                    new_mod > old_mod))

    # IMPROVE ON PERSON SEPARTION
    person_sep_improve_first <- compare_stats$new_beats_first[which(compare_stats$stat == "person_sep")]
    person_sep_improve_old <- compare_stats$new_beats_old[which(compare_stats$stat == "person_sep")]

    # IMPROVE ON ITEM INFIT SD
    item_insd_improve_first <- compare_stats$new_beats_first[which(compare_stats$stat == "item_insd")]
    item_insd_improve_old  <- compare_stats$new_beats_old[which(compare_stats$stat == "item_insd")]

    # IMPROVE ON ITEM OUTFIT SD
    item_outsd_improve_first <- compare_stats$new_beats_first[which(compare_stats$stat == "item_outsd")]
    item_outsd_improve_old  <- compare_stats$new_beats_old[which(compare_stats$stat == "item_outsd")]

    # IMPROVE ON PERSON SEPARTION & (ITEM INFIT SD | ITEM OUTFIT SD)
    improved_first <- person_sep_improve_first == TRUE &
      (item_insd_improve_first == TRUE | item_outsd_improve_first == TRUE)
    improved_old <- person_sep_improve_old == TRUE &
      (item_insd_improve_old == TRUE | item_outsd_improve_old == TRUE)

    # RETURN CODING SCHEME
    return_scheme <- return_scheme  %>% bind_rows(disordered_data)



    # CHECK TO SEE IF LOOP SHOULD STOP
    if (improved_old == TRUE) {
      recoded = TRUE
      return_items  <- new_items
      return_model  <- mod_new
    }
    if (run == 1) {
      mod_old <- mod_new
    }

    if (run > 1 & improved_old == TRUE) {
      recoded = TRUE
      mod_old <- mod_new
      return_items  <- new_items
      return_model  <- mod_new
    }

    if (run > 1 & improved_first == FALSE) {
      recoded = FALSE
      no_improvement <- TRUE
      disorder_remains <- TRUE
      end_message <- "\nDisorder exists, but recoding yields no improvement over original"
      return_scheme <- data_frame()
      return_items  <- data_frame()
      return_model  <- NULL
    }

    if (run > 1 & improved_first == TRUE & improved_old == FALSE) {
      recoded = TRUE
      no_improvement <- TRUE
      disorder_remains <- TRUE
      end_message <- "\nRecoded, but disorder remains with no further improvement"
    }


    if (no_improvement == TRUE){
      cat(end_message)
      break
    }

    # UPDATE BEFORE RUNNING AGAIN
    run <- run + 1


  }
  # RETURN LIST WITH RECODE SCHEME, RECODED ITEMS, FINAL MODEL
  return(
    list(
      scheme = return_scheme,
      items = return_items,
      model = return_model,
      stats = compare_stats,
      message = end_message,
      recoded = recoded,
      disorder_remains = disorder_remains
    )
  )
}
