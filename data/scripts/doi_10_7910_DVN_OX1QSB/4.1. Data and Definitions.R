# Load data

if (frequency == 12) {
  freq.component <- "month"
  
  forecast.data <-
    readRDS(stringr::str_c(getwd(), "/monthdata.Rds"))
  
  if(predictors == "welch.goyal.monthly"){
    
    predictor.set <-
      subset(forecast.data,
             select = c(dp, dy, ep, de, svar, `b/m`, ntis, tbl, lty, ltr, tms, dfy,
                        dfr, cpi))
    
    # Perfect collinearity implies that de and tms need to be eliminated for KS forecasts
    predictor.set.ks <-
      subset(forecast.data,
             select = c(dp, dy, ep, svar, `b/m`, ntis, tbl, lty, ltr, dfy,
                        dfr, cpi))
    
    # CT restrictions: coefficient signs (in order of presentation in RSZ)
    
    pos <- c(1, 2, 3, 4, 5, 6, 10, 11, 12, 13)
    neg <- c(7, 8, 9, 14)
    
    # ## CT restrictions for KS forecasts ---------
    lower <- c(0, 0, 0, 0, 0,-Inf,-Inf,-Inf, 0, 0, 0,-Inf)
    upper <- c(Inf, Inf, Inf, Inf, Inf, 0, 0, 0, Inf, Inf, Inf, 0)
    
    ## CT restrictions for E-NET, Ridge, Lasso forecasts ----
    lower.enet <-
      c(0, 0, 0, 0, 0, 0,-Inf,-Inf,-Inf, 0, 0, 0, 0,-Inf)
    upper.enet <-
      c(Inf, Inf, Inf, Inf, Inf, Inf, 0, 0, 0, Inf, Inf, Inf, Inf, 0)
    
  }

}

if(predictors == "li.tsiakas"){
  
  predictor.set <-
    subset(forecast.data,
           select = c(dy, ep, svar, `b/m`, ntis, tbl, ltr, tms, dfy,
                      dfr, cpi))
  predictor.set.ks <- predictor.set
  
  # CT restrictions: coefficient signs (in order of presentation in RSZ)
  
  pos <- c(1, 2, 3, 4, 7, 8, 9, 10)
  neg <- c(5, 6, 11)
  
  # ## CT restrictions for KS forecasts ---------
  lower <- c(0, 0, 0, 0,-Inf,-Inf, 0, 0, 0, 0,-Inf)
  upper <- c(Inf, Inf, Inf, Inf, 0, 0, Inf, Inf, Inf, Inf, 0)
  
  ## CT restrictions for E-NET, Ridge, Lasso forecasts ----
  lower.enet <- lower
  upper.enet <- upper
  
}

if(predictors == "rapach.zhou"){
  
  predictor.set <-
    subset(
      forecast.data,
      select = c(
        dp,
        ep,
        vol,
        bill,
        bond,
        tms,
        credit,
        ipg,
        ppig,
        ma_1_12,
        ma_3_12,
        mom_6
      )
    )
  
  predictor.set.ks <- predictor.set
  
  # CT restrictions: coefficient signs (in order of presentation in RSZ)
  
  pos <- c(1, 2, 3, 6, 7, 10, 11, 12)
  neg <- c(4, 5, 8, 9)
  
  # ## CT restrictions for KS forecasts ---------
  lower <- c(0, 0, 0,-Inf,-Inf, 0, 0,-Inf,-Inf, 0, 0, 0)
  upper <- c(Inf, Inf, Inf, 0, 0, Inf, Inf, 0, 0, Inf, Inf, Inf)
  ## CT restrictions for E-NET, Ridge, Lasso forecasts ----
  lower.enet <- lower
  upper.enet <- upper  
  
  }

if (frequency == 4) {
  freq.component <- "quarter"
  
  # Load data
  forecast.data <-
    readRDS(stringr::str_c(getwd(), "/quarterdata.Rds"))
  
  if(predictors == "welch.goyal.quarterly"){
    
  }
  predictor.set <-
    subset(
      forecast.data,
      select = c(
        dp,
        dy,
        ep,
        de,
        svar,
        `b/m`,
        ntis,
        tbl,
        lty,
        ltr,
        tms,
        dfy,
        dfr,
        cpi,
        ik
      )
    )
  
  # Perfect collinearity implies that de and tms need to be eliminated for KS forecasts
  predictor.set.ks <-
    subset(forecast.data,
           select = c(dp, dy, ep, svar, `b/m`, ntis, tbl, lty, ltr, dfy,
                      dfr, cpi, ik))
    # CT restrictions: coefficient signs (in order of presentation in RSZ)
  pos <- c(1, 2, 3, 4, 5, 6, 10, 11, 12, 13)
  neg <- c(7, 8, 9, 14, 15)
  
  # ## CT restrictions for KS forecasts ---------
  lower <- c(0, 0, 0, 0, 0, -Inf, -Inf, -Inf, 0, 0, 0, -Inf, -Inf)
  upper <- c(Inf, Inf, Inf, Inf, Inf, 0, 0, 0, Inf, Inf, Inf, 0, 0)
  
  ## CT restrictions for E-NET, Ridge, Lasso forecasts ----
  lower.enet <- c(0, 0, 0, 0, 0, 0, -Inf, -Inf, -Inf, 0, 0, 0, 0, -Inf, -Inf)
  upper.enet <- c(Inf, Inf, Inf, Inf, Inf, Inf, 0, 0, 0, Inf, Inf, Inf, Inf, 0, 0)
}


if(frequency == 12){
  start.oos <- which(forecast.data$yyyym == first.oos)
  end.oos <- which(forecast.data$yyyym == last.oos)
}

if(frequency == 4){
  start.oos <- which(forecast.data$yyyyq == first.oos)
  end.oos <- which(forecast.data$yyyyq == last.oos)
}

variable.count <- ncol(predictor.set)
variable.count.ks <- ncol(predictor.set.ks)
training.years <- start.year.oos - start.year.is

# Definitions for output ----------------------------------

if (predictors == "welch.goyal.monthly") {
  filename.component <-
    stringr::str_c("welchgoyal.", freq.component, start.year.is)
}
if (predictors == "li.tsiakas") {
  filename.component <-
    stringr::str_c("li.tsiakas.", freq.component, start.year.is)
}
if (predictors == "rapach.zhou") {
  filename.component <-
    stringr::str_c("rapachzhou.", freq.component, start.year.is)
}

if (predictors == "welch.goyal.quarterly") {
  filename.component <-
    stringr::str_c("welchgoyal.", freq.component, start.year.is)
}