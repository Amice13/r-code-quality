#### From DAMISC ####
# https://github.com/davidaarmstrong/damisc/blob/20e8f9665e00fa2a84231ddaccc4b1376ef9f36b/R/DAMisc_functions.R#L109
# DAMisc was removed from CRAN in 2023

btscs <-
  function (data, event, tvar, csunit, pad.ts = FALSE)
  {
    data$orig_order <- 1:nrow(data)
    data <- data[order(data[[csunit]], data[[tvar]]), ]
    spells <- function(x) {
      tmp <- rep(0, length(x))
      runcount <- 0
      for (j in 2:length(x)) {
        if (x[j] == 0 & x[(j - 1)] == 0) {
          tmp[j] <- runcount <- runcount + 1
        }
        if (x[j] != 0 & x[(j - 1)] == 0) {
          tmp[j] <- runcount + 1
          runcount <- 0
        }
        if (x[j] == 0 & x[(j - 1)] != 0) {
          tmp[j] <- runcount <- 0
        }
      }
      tmp
    }
    sp <- split(data, data[[csunit]])
    if (pad.ts) {
      sp <- lapply(sp, function(x) x[match(seq(min(x[[tvar]],
                                                   na.rm = TRUE), max(x[[tvar]], na.rm = TRUE)), x[[tvar]]),
      ])
      for (i in 1:length(sp)) {
        if (any(is.na(sp[[i]][[event]]))) {
          sp[[i]][[event]][which(is.na(sp[[i]][[event]]))] <- 1
        }
        if (any(is.na(sp[[i]][[tvar]]))) {
          sp[[i]][[tvar]] <- seq(min(sp[[i]][[tvar]], na.rm = TRUE),
                                 max(sp[[i]][[tvar]], na.rm = TRUE))
        }
        if (any(is.na(sp[[i]][[csunit]]))) {
          sp[[i]][[csunit]][which(is.na(sp[[i]][[csunit]]))] <- mean(sp[[i]][[csunit]],
                                                                     na.rm = TRUE)
        }
      }
    }
    sp <- lapply(1:length(sp), function(x) {
      cbind(sp[[x]], data.frame(spell = spells(sp[[x]][[event]])), stringsAsFactors=TRUE)
    })
    data <- do.call(rbind, sp)
    if (!pad.ts) {
      if (any(is.na(data$orig_order))) {
        data <- data[-which(is.na(data$orig_order)), ]
      }
      data <- data[data$orig_order, ]
    }
    else {
      data <- data[order(data[[csunit]], data[[tvar]]), ]
    }
    invisible(data)
  }
