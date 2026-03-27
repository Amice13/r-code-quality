# FUNCTION TO RUN ANOVAS FOR ALL ITEMS IN SCALE, EXTRACT RESULTS
run_anovas <- function(data,       # CLEANED DATA
                       all_names,  # NAMES OF ITEMS IN SCALE
                       model,      # RASCH MODEL
                       n_ints = 4, # NUM. OF VALUES IN CLASS INTERVAL
                       twoway = FALSE, # ONE OR TWO WAY ANOVA
                       # NUM OF COMPARISONS TO CORRECT FOR IN P-VALUE
                       bh_n = IRT.residuals(model)$stand_residuals  %>%
                         colnames()  %>%
                         length()
                       ) {

  # EXTRACT PERSON PARAMETERS
  person_rasch <- tam.wle(model)

  # EXTRACT ITEM RESIDUALS
  item_names <- IRT.residuals(model)$stand_residuals  %>% colnames()
  item_resids <- IRT.residuals(model)$stand_residuals  %>%
    tbl_df()  %>%
    mutate(id = c(data_clean$id) ) %>%
    gather(item, res, one_of(item_names))

  # PREP DATA FOR ANOVA
  mod_data <- data  %>%
    # COMPUTE CLASS INTERVALS
    mutate(class_int = ntile(raw_sum, n_ints))  %>%
    # ABILITY
    mutate(theta = person_rasch$theta)  %>%
    # RESHAPE FROM WIDE TO LONG DATA FOR ANOVA LOOP
    gather(item, response, one_of(item_names))  %>%
    # MERGE WITH RESIDUALS
    left_join(item_resids, by = c("id", "item"))



  #=============================================
  #   ANOVAS
  #=============================================
  # ITEMS WITHOUT PSEUDO CODES
  unsplit_names <- item_names[item_names %in% all_names]

  # RUN ONE-WAY ANOVA
  if(twoway == FALSE){
      anova_df <-  unsplit_names  %>%
    lapply(function(x){
      anova_data <- mod_data  %>%   filter(item == x)
        ols <- lm(res ~ factor(group_var),
           data = anova_data)
        anova(ols)  %>%
        broom::tidy()  %>%
        mutate(item = x)  %>%
        select(item, everything())
    })   %>%
    bind_rows()  %>%
    mutate(bh_pval = p.adjust(p.value, method = "BH", n = bh_n))
  }
  # RUN TWO-WAY ANOVA
  if(twoway == TRUE){
      anova_df <-  unsplit_names  %>%
    lapply(function(x){
      anova_data <- mod_data  %>%   filter(item == x)
      ols <- lm(res ~
                  factor(group_var) +
                  factor(class_int) +
                  factor(group_var):factor(class_int),
                data = anova_data)
        anova(ols)  %>%
        broom::tidy()  %>%
        mutate(item = x)  %>%
        select(item, everything())
    })   %>%
    bind_rows()  %>%
    mutate(bh_pval = p.adjust(p.value, method = "BH", n = bh_n))
  }

    return(anova_df)
}
