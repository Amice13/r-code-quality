##########################################################################
#####################PREPARATION OF QUARTERLY DATA########################
##########################################################################

# load quarterly data

quarter <-
  read_excel(stringr::str_c(getwd(),"/PredictorData2022.xlsx"), sheet = "Quarterly", na = "NaN")

# convert numeric yyyyq variable to quarterly date variable

quarter$yearqtr <- quarter$yyyyq
quarter$yearqtr <- paste(quarter$yearqtr, "q", sep = "")
quarter$yearqtr <- gsub("1q", "-01", quarter$yearqtr)
quarter$yearqtr <- gsub("2q", "-04", quarter$yearqtr)
quarter$yearqtr <- gsub("3q", "-07", quarter$yearqtr)
quarter$yearqtr <- gsub("4q", "-10", quarter$yearqtr)
quarter$yearqtr <- paste(quarter$yearqtr, "-01", sep = "")
quarter$yearqtr <- as.Date(quarter$yearqtr, "%Y-%m-%d")

# convert to zoo series (using yearqtr as index)

quarter$yearqtr <- as.yearqtr(quarter$yearqtr)
quarterdata <- zoo(quarter, order.by = quarter$yearqtr)

# variables are stored in character --> convert to numeric

storage.mode(quarterdata) <- "numeric"

# stock returns include dividends

quarterdata$returns <- log(quarterdata$CRSP_SPvw + 1)

# log risk-free rate

quarterdata$logrf <- log(1 + quarterdata$Rfree)

# equity premium (log and simple)

quarterdata$premium <-
  #quarterdata$returns - stats::lag(quarterdata$logrf,0)
  quarterdata$returns - quarterdata$logrf
quarterdata$simple_premium <-
  #quarterdata$CRSP_SPvw - stats::lag(quarterdata$Rfree,0)
  quarterdata$CRSP_SPvw - quarterdata$Rfree

# dividend/price ratio

quarterdata$dp <-
  na.omit(log(quarterdata$D12) - log(quarterdata$Index))

# dividend yield

quarterdata$dy <-
  na.omit(log(quarterdata$D12) - stats::lag(log(quarterdata$Index),-1))

# earnings/price ratio

quarterdata$ep <-
  na.omit(log(quarterdata$E12) - log(quarterdata$Index))

# dividend payout ratio

quarterdata$de <-
  na.omit(log(quarterdata$D12) - log(quarterdata$E12))

# term spread

quarterdata$tms <- na.omit(quarterdata$lty - quarterdata$tbl)

# default yield spread

quarterdata$dfy <- na.omit(quarterdata$BAA - quarterdata$AAA)

# default return spread

quarterdata$dfr <- na.omit(quarterdata$corpr - quarterdata$ltr)

# inflation is lagged by one period

quarterdata$cpi <- stats::lag(quarterdata$infl, -1)

# remove rows before 1926
start.date <- which(quarterdata$yyyyq==start.data)
end.date <- which(quarterdata$yyyyq==last.oos)
rsz.data <- window(quarterdata, index(quarterdata$Index))[start.date:end.date]

# remove unnecessary columns

quarterdata <-
  subset(
    rsz.data,
    select = -c(
      Index,
      D12,
      E12,
      BAA,
      AAA,
      infl,
      corpr,
      csp,
      CRSP_SPvw,
      CRSP_SPvwx,
      D3,
      E3,
      yearqtr,
      returns,
      logrf
    )
  )

# export to RDS file
saveRDS(quarterdata, file = stringr::str_c(getwd(), "/quarterdata.Rds"))
  