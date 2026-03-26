##########################################################################
#######################PREPARATION OF MONTHLY DATA########################
##########################################################################

monthly <-
  read_excel(
    stringr::str_c(getwd(), "/PredictorData2022.xlsx"),
    sheet = "Monthly",
    na = "NaN"
  )

# convert numeric yyyymm variable to monthly date

monthly$yearmon <- monthly$yyyymm
monthly$yearmon <-
  as.Date(paste0(as.character(monthly$yearmon), '01'), format = '%Y%m%d')

# convert to zoo series (using yearmon as index)

monthly$yearmon <- as.yearmon(monthly$yearmon)
monthdata <- zoo(monthly, order.by = monthly$yearmon)

# variables are stored in character --> convert to numeric

storage.mode(monthdata) <- "numeric"

# stock returns include dividends

monthdata$returns <- log(monthdata$CRSP_SPvw + 1)

# log risk-free rate

monthdata$logrf <- log(1 + monthdata$Rfree)

# equity premium (log and simple)

monthdata$premium <-
  monthdata$returns - stats::lag(monthdata$logrf, 0)
monthdata$simple_premium <-
  monthdata$CRSP_SPvw - stats::lag(monthdata$Rfree, 0)


# dividend/price ratio

monthdata$dp <- na.omit(log(monthdata$D12) - log(monthdata$Index))

# dividend yield

monthdata$dy <-
  na.omit(log(monthdata$D12) - stats::lag(log(monthdata$Index), -1))

# earnings/price ratio

monthdata$ep <- na.omit(log(monthdata$E12) - log(monthdata$Index))

# dividend payout ratio

monthdata$de <- na.omit(log(monthdata$D12) - log(monthdata$E12))

# term spread

monthdata$tms <- (na.omit(monthdata$lty - monthdata$tbl))

# default yield spread

monthdata$dfy <- (na.omit(monthdata$BAA - monthdata$AAA))

# default return spread

monthdata$dfr <- (na.omit(monthdata$corpr - monthdata$ltr))

# inflation is lagged one period

monthdata$cpi <- stats::lag(monthdata$infl, -1)

## Additional variables from Rapach & Zhou -----------------------


# calculation of Mele volatility

sigma <- 1 / 12 * rollsum(abs(monthdata$simple_premium), 12, align = "right")
monthdata$vol <- sqrt(pi / 2) * sqrt(12) * sigma

# relative Treasury bill yield and bond yield

monthdata$bill <-
  monthdata$tbl - rollmean(monthdata$tbl, 12, align = "right")
monthdata$bond <-
  monthdata$lty - rollmean(monthdata$lty, 12, align = "right")

# Credit spread

monthdata$credit <- 
  monthdata$AAA-monthdata$lty

# 12-month moving average of the S&P 500

monthdata$ma_index_12 <-
  rollmean(monthdata$Index, 12, align = "right")

# 3-month moving average of index

monthdata$ma_index_3 <-
  rollmean(monthdata$Index, 3, align = "right")

# MA(1,12)

monthdata$ma_1_12 <-
  ifelse(monthdata$Index >= monthdata$ma_index_12, 1, 0)

# MA(3,12)

monthdata$ma_3_12 <-
  ifelse(monthdata$ma_index_3 >= monthdata$ma_index_12, 1, 0)

# MOM(6)

monthdata$mom_6 <-
  ifelse(monthdata$Index >= stats::lag(monthdata$Index,-6), 1, 0)

#########################################################################

# # Industrial production growth
indpro <-
  read_excel(stringr::str_c(getwd(), "/INDPRO.xls"),
             na = "NaN")

header <- which(indpro[, 1] == "observation_date")
colnames(indpro) <- indpro[header, ]
indpro <- indpro[(header + 1):nrow(indpro), ]
indpro$observation_date <-
  as.Date(as.numeric(indpro$observation_date), origin = "1899-12-30")
indpro$yearmon <- as.yearmon(indpro$observation_date)
indpro$ip <- ((as.numeric(indpro$INDPRO)))
indpro <- zoo(indpro, order.by = indpro$yearmon)
indpro$ip.nolag <- 0
for (n in 1:length(indpro$ip.nolag)) {
  if (n == 1) {
    indpro[n, "ip.nolag"] <- NA
  } else {
    indpro[n, "ip.nolag"] <-
      (as.numeric(indpro[n, "ip"]) / as.numeric(indpro[n - 1, "ip"]))
  }
}


indpro$ipg <- stats::lag(indpro$ip.nolag, -1)


# # Producer price index inflation rate
# # Industrial production growth

ppiaco <-
  read_excel(stringr::str_c(getwd(), "/PPIACO.xls"),
             na = "NaN")
header <- which(ppiaco[, 1] == "observation_date")
colnames(ppiaco) <- ppiaco[header, ]
ppiaco <- ppiaco[(header + 1):nrow(ppiaco), ]
ppiaco$observation_date <-
  as.Date(as.numeric(ppiaco$observation_date), origin = "1899-12-30")
ppiaco$yearmon <- as.yearmon(ppiaco$observation_date)
ppiaco$ppi <- ((as.numeric(ppiaco$PPIACO)))
ppiaco <- zoo(ppiaco, order.by = ppiaco$yearmon)
ppiaco$ppi.nolag <- 0
for (n in 1:length(ppiaco$ppi.nolag)) {
  if (n == 1) {
    ppiaco[n, "ppi.nolag"] <- NA
  } else {
    ppiaco[n, "ppi.nolag"] <-
      (as.numeric(ppiaco[n, "ppi"]) / as.numeric(ppiaco[n - 1, "ppi"]))
  }
}
ppiaco$ppig <- stats::lag(ppiaco$ppi.nolag, -1)

# remove rows before January 1926

start.date <- which(monthdata$yyyym == start.data)
end.date <- which(monthdata$yyyym == last.oos)
rsz.data <-
  window(monthdata, index(monthdata$Index))[start.date:end.date]

# remove unnecessary columns

rsz.indpro <- merge(rsz.data, indpro, all = FALSE)
 monthdata <- merge(rsz.indpro, ppiaco, all = FALSE)
storage.mode(monthdata)<-"numeric"
# export to RDS file

saveRDS(monthdata, file = stringr::str_c(getwd(), "/monthdata.Rds"))
