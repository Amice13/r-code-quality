# ---------------------------------------------------------------------------------------
# THE COMPARATIVE LEGISLATORS DATABASE
# Sascha Göbel and Simon Munzert
# Script for functions
# April 2020
# ---------------------------------------------------------------------------------------


# get_age ===============================================================================

get_age <- function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}

# turnoverRate ==========================================================================

turnoverRate <- function(parliaments) {
  for (j in 1:length(parliaments)) {
    # extract legislatoR data
    parliament_pol <- legislatoR::get_political(parliaments[j])
    parliament_cor <- legislatoR::get_core(parliaments[j])
    sessions <- unique(parliament_pol$session)
    turnover <- data.frame(legislature = parliaments[j],
                           session = sessions,
                           year = NA,
                           alternation = NA,
                           renewal = NA)
    for (i in 1:length(sessions)) {
      if (i == max(sessions)) {
        break
      }
      prev <- filter(parliament_pol, session == i)
      curr <- filter(parliament_pol, session == i+1)
      turnover$year[i+1] <- lubridate::year(unique(curr$session_start))
      if (i == 1) {
        trace_ids <- unique(prev$pageid)
      } else {
        trace_ids <- unique(c(trace_ids, prev$pageid))
      }
      reelected_ids <- unique(curr$pageid[which(curr$pageid %in% prev$pageid)]) # reelected
      firsttime_ids <- unique(curr$pageid[which(!(curr$pageid %in% trace_ids))])
      turnover$alternation[i+1] <- 1-(length(reelected_ids)/(length(unique(curr$pageid))))
      turnover$renewal[i+1] <- length(firsttime_ids)/(length(unique(curr$pageid)))
    }
    turnover <- turnover[-1,]
    if (j == 1) {
      output <- turnover
    } else {
      output <- rbind(output, turnover)
    }
  }
  return(output)
}