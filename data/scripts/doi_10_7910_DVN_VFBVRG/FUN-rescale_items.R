###################################################
# FUNCTION THAT TAKES ITEMS AND:
#  1. REPLACES MISSING VALUES WITH NA,
#  2. MAKES ZERO THE MINIMUM VALUE,
#  3. RESCALES SO LARGER VALUES INDICATE GREATER ABILITY
###################################################

rescale_items <- function(data,
                 all_vars = c(), # ALL VARIABLES IN SCALE
                 reverse_vars = c() # VARIABLES THAT SHOULD BE REVERSED
  ){
    # 1. set invalid values as missing
    data <- data  %>% mutate_each(
        funs(ifelse(. %in% -9:-1, NA, .)),
        one_of(all_vars))

    # 2. rescale so minimum is 0
    data <- data  %>% mutate_each(
        funs(ifelse(is.na(.), NA, . - 1)),
        one_of(all_vars))

    # 3. rescale so larger values indicate greater ability
    if(is.null(reverse_vars) == FALSE){
    data <- data  %>% mutate_each(
        funs(ifelse(is.na(.), NA, abs(. - max(., na.rm = TRUE)))),
        one_of(reverse_vars))
    }

    # ENSURE MINIMUM IS ZERO
    lapply(all_vars, function(x){
          stopifnot(data  %>% select(one_of(x))  %>% min(na.rm = TRUE) == 0)
      })


    # RETURN DATA
    return(data)
  }
