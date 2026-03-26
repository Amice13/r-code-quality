# FUNCTION TO CONFIRM THAT SCALE ITEMS HAVE POSITIVE CORRELATIONS


check_scale_items <- function(df1, df2, vars){
  a <- select(df1, one_of(vars))  %>%
    cor(use = "pairwise.complete.obs")

  b <- select(df2, one_of(vars))  %>%
    cor(use = "pairwise.complete.obs")

  cat(
    " ==============================\n",
    "1. Within year, between item correlations\n",
    "==============================\n\n"
    )

  cat("2012 -----------------------------------\n\n")


  if(all(a > 0)){
    cat("All 2012 correlations are positive: \n")
  } else{
    cat("One or more 2012 correlations are negative: \n")
  }
  print(a %>% round(2))
  cat("\n\n")


  cat("2016 -----------------------------------\n\n")

  if(all(b > 0)){
    cat("All 2016 correlations are positive: \n")
  } else{
    cat("One or more 2016 correlations are negative: \n")
  }
  print(b %>% round(2))

  cat(
    "\n\n",
    "==============================\n",
    "2. Between year, within item correlation\n",
    "==============================\n\n"
  )


  print((cor(c(a), c(b))))

}

