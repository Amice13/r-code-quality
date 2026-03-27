# NEW TRADE DATA 2010:2014

# Notes on the Comtrade data: 
#   
# •	All dyadic trade data are reported in current US dollar values, calculated using an average annual exchange rate - calculated by weighting the monthly exchange rate with the monthly volume of trade.
# 
# •	GDP, PPP (constant 2011 international $): converted to international dollars using purchasing power parity rates in constant 2011 international dollars.
# 
# •	GDP (current US$) : at purchaser's prices,  in current U.S. dollars. Dollar figures for GDP are converted from domestic currencies using single year official exchange rates. For a few countries where the official exchange rate does not reflect the rate effectively applied to actual foreign exchange transactions, an alternative conversion factor is used.
# 
# •	For the following countries, the WB calculated GDP estimates based on regression: 
# 
# o	Afghanistan
# o	Eritrea
# o	Tanzania
# o	Timor-Leste
# o	Turkmenistan
# o	Uzbekistan
# 
# References:
# 
# •	United Nations (2014). United Nations Commodity Trade Statistics Database [data set]. Available from http://comtrade.un.org/data/. Accessed 26 October 2016
# 
# •	Country GDP extracted from WITS: http://wits.worldbank.org/datadownload.aspx?lang=en
# 
# •	World Bank, International Comparison Program database.
# 
# •	World Bank national accounts data, and OECD National Accounts data files.

## Load and transform to dyadic 

require(readxl)
trade_df <-
  read_excel("trade_data.xlsx",
             col_types = c(rep("numeric", 4),
                           rep("text", 4),
                           rep("numeric", 5)))

trade_df_dyadic_part <- trade_df[,c('Ccode_imp1','Ccode_imp2','2014','2013','2012','2011','2010', 'Indicator Type')]
trade_df_dyadic_part <- trade_df_dyadic_part[trade_df_dyadic_part$`Indicator Type` %in% c("Export", "Import"),]

require(reshape2)
trade_df_dyadic_part <- melt(trade_df_dyadic_part, 
                             id.vars = c('Ccode_imp1','Ccode_imp2','Indicator Type'),
                                         variable.name = "year")

trade_df_dyadic_part_import <- trade_df_dyadic_part[trade_df_dyadic_part$`Indicator Type` == "Import" &
                                                      !is.na(trade_df_dyadic_part$value),]
trade_df_dyadic_part_export <- trade_df_dyadic_part[trade_df_dyadic_part$`Indicator Type` == "Export" &
                                                      !is.na(trade_df_dyadic_part$value),]
trade_df_dyadic_part_import$`Indicator Type` <- NULL
trade_df_dyadic_part_export$`Indicator Type` <- NULL
names(trade_df_dyadic_part_import)[4] <- "import_dyadic"
names(trade_df_dyadic_part_export)[4] <- "export_dyadic"

trade_df_nondyadic_part <- trade_df[,c('Ccode_imp1','2014','2013','2012','2011','2010', 'Indicator Type')]
trade_df_nondyadic_part <- trade_df_nondyadic_part[trade_df_nondyadic_part$`Indicator Type` %in% c("exports world", "GDP_adjusted", "GDP_Current", "imports world"),]

trade_df_nondyadic_part <- melt(trade_df_nondyadic_part, 
                             id.vars = c('Ccode_imp1','Indicator Type'),
                             variable.name = "year")

trade_df_nondyadic_part <- dcast(trade_df_nondyadic_part, Ccode_imp1 + year ~ `Indicator Type`, value.var = 'value')
names(trade_df_nondyadic_part) <- c("ccode", "year", "export_tot", "gdp_adjusted", "gdp_current", "import_tot")

## Merge
trade_df_dyadic_part <- merge(trade_df_dyadic_part_import, trade_df_dyadic_part_export, 
                              by = c("Ccode_imp1", "Ccode_imp2", "year"),
                              all = TRUE)

trade_df <- merge(trade_df_dyadic_part, trade_df_nondyadic_part, 
                  by.x = c("Ccode_imp1","year"), 
                  by.y = c("ccode", "year"), all.x = TRUE)

trade_df <- merge(trade_df, trade_df_nondyadic_part, 
                  by.x = c("Ccode_imp2","year"), 
                  by.y = c("ccode", "year"), all.x = TRUE,
                  suffixes = c("_1", "_2"))
names(trade_df)[1] <- 'ccode1'
names(trade_df)[3] <- 'ccode2'

# South Sudan is out
trade_df <- subset(trade_df, !is.na(ccode1))
trade_df <- subset(trade_df, !is.na(ccode2))

trade_df_ordered <- data.frame()

for(i in 1:nrow(trade_df)) {
  if(trade_df$ccode1[i] < trade_df$ccode2[i]) {
    trade_df_ordered <- 
      rbind(trade_df_ordered, 
            trade_df[i,])
  } else {
    trade_df_ordered <- 
      rbind(trade_df_ordered, 
            data.frame(ccode1 = trade_df$ccode2[i],
                       year = trade_df$year[i],
                       ccode2 = trade_df$ccode1[i], 
                       import_dyadic = trade_df$export_dyadic[i],
                       export_dyadic = trade_df$import_dyadic[i],
                       export_tot_1 = trade_df$export_tot_2[i],
                       gdp_adjusted_1 = trade_df$gdp_adjusted_2[i],
                       gdp_current_1 = trade_df$gdp_current_2[i],
                       import_tot_1 = trade_df$import_tot_2[i],
                       export_tot_2 = trade_df$export_tot_1[i],
                       gdp_adjusted_2 = trade_df$gdp_adjusted_1[i],
                       gdp_current_2 = trade_df$gdp_current_1[i], 
                       import_tot_2 = trade_df$import_tot_1[i]))
  }
}


trade_df <- trade_df_ordered
rm(trade_df_ordered)

# Load MID and ICB data
load("mid_and_icb_dyad_yr.RData")

# Load dependent variables data
require(haven)
dv_df <- 
  read_stata("dependet_variables_dyadic_form.dta")
dv_df <- data.frame(dv_df)

require(zoo)
require(dplyr)

movAvg <- function(x, k) {
  require(zoo)
  if (length(x)<k) {
    return(x) 
  } else {
    return(rollapply(x, width = k, mean, na.rm = T, fill = NA))
  }
}

movAvgRightAligned <- function(x, k) {
  require(zoo)
  if (is.na(x)[1]) {
    x[1] <- x[2]
  }
  if (length(x)<k) {
    return(x) 
  } else {
    return(rollapply(x, width = k, mean, na.rm = T, fill = NA, align = 'right'))
  }
}

dv_df$ccode1 <- dv_df$ccode1_n
dv_df$ccode2 <- dv_df$ccode2_n

## Integrate with trade dataset
dv_df <- merge(dv_df, 
               trade_df[,c("ccode1","ccode2","year","export_tot_1","export_tot_2",
                           "import_dyadic", "export_dyadic")], 
               by = c("ccode1","ccode2","year"),
               all.x = T)

replaceNA <- function(old, new) {
  if (is.na(old) & !is.na(new)) {
    return(new/1000000)
  } else {
    return(old)
  }
}
dv_df$exports_1 <- mapply(replaceNA, dv_df$exports_1, dv_df$export_tot_1)
dv_df$exports_2 <- mapply(replaceNA, dv_df$exports_2, dv_df$export_tot_2)

dv_df$flow1 <- mapply(replaceNA, dv_df$flow1, dv_df$export_dyadic)
dv_df$flow2 <- mapply(replaceNA, dv_df$flow2, dv_df$import_dyadic)

# Deflact $ values
dv_df$exports_1 <- dv_df$exports_1 * dv_df$deflat2005bg_rate
dv_df$exports_2 <- dv_df$exports_2 * dv_df$deflat2005bg_rate

dv_df$flow1 <- dv_df$flow1 * dv_df$deflat2005bg_rate
dv_df$flow2 <- dv_df$flow2 * dv_df$deflat2005bg_rate

## CAPRAT & ABS_CAP

codeCaprat <- function(cap_1, cap_2) {
  if (is.na(cap_1) | is.na(cap_2)) return(NA)
  if (cap_2 >= cap_1) return(cap_1 / cap_2)
  if (cap_1 > cap_2) return(cap_2 / cap_1)
}
dv_df$caprat <- mapply(codeCaprat, dv_df$cap_1, dv_df$cap_2)

dv_df$abs_cap <- abs(dv_df$cap_1 - dv_df$cap_2)


## fd_humdefbur & fd_milex

dv_df <- 
  dv_df %>%
  dplyr::group_by(ccode1, ccode2) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(fd_humdefbur1 = c(NA, diff(milper_1  / tpop_1)),
                fd_humdefbur2 = c(NA, diff(milper_2  / tpop_2)),
                fd_milex1 = c(NA, diff(milex_1)),
                fd_milex2 = c(NA, diff(milex_2)))

dv_df <-
  dv_df %>%
  dplyr::group_by(ccode1, ccode2) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(fd_humdefbur1 = na.approx(fd_humdefbur1, na.rm = F),
                fd_humdefbur2 = na.approx(fd_humdefbur2, na.rm = F),
                fd_milex1 = na.approx(fd_milex1, na.rm = F),
                fd_milex2 = na.approx(fd_milex2, na.rm = F), 
                IntraUCDP_1 = na.approx(IntraUCDP_1, na.rm = F),
                IntraUCDP_2 = na.approx(IntraUCDP_2, na.rm = F))

# count_missing <- 
#   dv_df %>%
#   dplyr::group_by(year) %>%
#   dplyr::summarize(fd_humdefbur1 = sum(!is.finite(fd_humdefbur1)),
#                    fd_humdefburw = sum(!is.finite(fd_humdefbur2)),
#                    fd_milex1 = sum(!is.finite(fd_milex1)),
#                    fd_milex2 = sum(!is.finite(fd_milex2)))
# apply(count_missing, 2, sum)

dv_df$interac_humdefbur <- dv_df$fd_humdefbur1 * dv_df$fd_humdefbur2
dv_df$interac_fd_milex <- dv_df$fd_milex1 * dv_df$fd_milex2
dv_df$interac_IntraUCDP <- dv_df$IntraUCDP_1 * dv_df$IntraUCDP_2

dv_df <-
  dv_df %>%
  dplyr::group_by(ccode1, ccode2) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(polity21 = na.approx(polity21, na.rm = F),
                polity22 = na.approx(polity22, na.rm = F),
                flow1 = na.approx(flow1, na.rm = F),
                flow2 = na.approx(flow2, na.rm = F),
                exports_1 = na.approx(exports_1, na.rm = F),
                exports_2 = na.approx(exports_2, na.rm = F))

dv_df$interac_polity2 <- (dv_df$polity21+10) * (dv_df$polity22+10)
dv_df$abs_polity2 <- abs(dv_df$polity21 - dv_df$polity22)
dv_df$interac_flow = dv_df$flow1 * dv_df$flow2
dv_df$interac_export = dv_df$exports_1 * dv_df$exports_2

dv_df$jntdemord <-NA
dv_df$jntdemord[dv_df$polity21==10 & dv_df$polity22==10] <- 13
dv_df$jntdemord[(dv_df$polity21==10 & dv_df$polity22<10 & dv_df$polity22>6) | (dv_df$polity22==10 & dv_df$polity21<10 & dv_df$polity21>6)] <- 12
dv_df$jntdemord[dv_df$polity22<10 & dv_df$polity22>6 & dv_df$polity21<10 & dv_df$polity21>6] <- 11
dv_df$jntdemord[(dv_df$polity21<=10 & dv_df$polity21>6 & dv_df$polity22<7 & dv_df$polity22>-1) | (dv_df$polity22<=10 & dv_df$polity22>6 & dv_df$polity21<7 & dv_df$polity21>-1)] <- 10
dv_df$jntdemord[dv_df$polity22<7 & dv_df$polity22>-1 & dv_df$polity21<7 & dv_df$polity21>-1] <- 9
dv_df$jntdemord[(dv_df$polity21<=10 & dv_df$polity21>6 & dv_df$polity22<1 & dv_df$polity22>-7) | (dv_df$polity22<=10 & dv_df$polity22>6 & dv_df$polity21<1 & dv_df$polity21>-7)] <- 8
dv_df$jntdemord[(dv_df$polity21<7 & dv_df$polity21>-1 & dv_df$polity22<1 & dv_df$polity22>-7) | (dv_df$polity22<7 & dv_df$polity22>-1 & dv_df$polity21<1 & dv_df$polity21>-7)] <- 7
dv_df$jntdemord[(dv_df$polity21<=10 & dv_df$polity21>6 & dv_df$polity22< -6 & dv_df$polity22>= -10) | (dv_df$polity22<=10 & dv_df$polity22>6 & dv_df$polity21< -6 & dv_df$polity21>= -10)] <- 6
dv_df$jntdemord[(dv_df$polity21<7 & dv_df$polity21> -1 & dv_df$polity22< -6 & dv_df$polity22>= -10) | (dv_df$polity22<7 & dv_df$polity22> -1 & dv_df$polity21< -6 & dv_df$polity21>= -10)] <- 5
dv_df$jntdemord[dv_df$polity22<1 & dv_df$polity22> -7 & dv_df$polity21<1 & dv_df$polity21> -7] <- 4
dv_df$jntdemord[(dv_df$polity21<1 & dv_df$polity21> -7 & dv_df$polity22< -6 & dv_df$polity22>= -10) | (dv_df$polity22<1 & dv_df$polity22> -7 & dv_df$polity21< -6 & dv_df$polity21>= -10)] <- 3
dv_df$jntdemord[dv_df$polity22< -6 & dv_df$polity22> -10 & dv_df$polity21< -6 & dv_df$polity21> -10] <- 2
dv_df$jntdemord[(dv_df$polity21== -10 & dv_df$polity22< -6 & dv_df$polity22> -10) | (dv_df$polity22== -10 & dv_df$polity21< -6 & dv_df$polity21> -10)] <- 1
dv_df$jntdemord[dv_df$polity21== -10 & dv_df$polity22== -10] <- 0 


dv_df$poldisab <- NA
dv_df$poldisab <- abs(dv_df$polity21-dv_df$polity22)

dv_df$jntautoc <- FALSE
dv_df$jntautoc[dv_df$polity21 <= -7 & dv_df$polity22 <= -7] <- TRUE
dv_df$jntautoc[is.na(dv_df$polity21) | is.na(dv_df$polity22)] <- NA

dv_df <- 
  dv_df %>%
  dplyr::group_by(ccode1, ccode2) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(dyaddur = year - min(year))

## Moving averages

require(dplyr)
dv_df <- 
  dv_df %>%
  dplyr::group_by(ccode1, ccode2) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(caprat_5ma = as.numeric(movAvg(caprat, 5)),
                abs_cap_5ma = as.numeric(movAvg(abs_cap, 5)),
                interac_humdefbur_5ma = as.numeric(movAvg(interac_humdefbur, 5)),
                interac_fd_milex_5ma = as.numeric(movAvg(interac_fd_milex, 5)),
                allies_w_5ma = as.numeric(movAvg(allies_w, 5)),
                ally_dum_w_5ma = as.numeric(movAvg(ally_dum_w, 5)),
                dyaddur_5ma = as.numeric(movAvg(dyaddur, 5)),
                interac_flow_5ma = as.numeric(movAvg(interac_flow, 5)),
                interac_export_5ma = as.numeric(movAvg(interac_export, 5)),
                interac_IntraUCDP_5ma = as.numeric(movAvg(interac_IntraUCDP, 5)),
                fd_humdefbur1_5ma = as.numeric(movAvg(fd_humdefbur1, 5)),
                fd_humdefbur2_5ma = as.numeric(movAvg(fd_humdefbur2, 5)),
                fd_milex1_5ma = as.numeric(movAvg(fd_milex1, 5)),
                fd_milex2_5ma = as.numeric(movAvg(fd_milex2, 5)),
                IntraUCDP_1_5ma = as.numeric(movAvg(IntraUCDP_1, 5)),
                IntraUCDP_2_5ma = as.numeric(movAvg(IntraUCDP_2, 5)),
                flow1_5ma = as.numeric(movAvg(flow1, 5)),
                flow2_5ma = as.numeric(movAvg(flow2, 5)),
                exports_1_5ma = as.numeric(movAvg(exports_1, 5)),
                exports_2_5ma = as.numeric(movAvg(exports_2, 5)),
                
                caprat_3ma = as.numeric(movAvg(caprat, 3)),
                abs_cap_3ma = as.numeric(movAvg(abs_cap, 3)),
                interac_humdefbur_3ma = as.numeric(movAvg(interac_humdefbur, 3)),
                interac_fd_milex_3ma = as.numeric(movAvg(interac_fd_milex, 3)),
                allies_w_3ma = as.numeric(movAvg(allies_w, 3)),
                ally_dum_w_3ma = as.numeric(movAvg(ally_dum_w, 3)),
                dyaddur_3ma = as.numeric(movAvg(dyaddur, 3)),
                interac_flow_3ma = as.numeric(movAvg(interac_flow, 3)),
                interac_export_3ma = as.numeric(movAvg(interac_export, 3)),
                interac_IntraUCDP_3ma = as.numeric(movAvg(interac_IntraUCDP, 3)),
                fd_humdefbur1_3ma = as.numeric(movAvg(fd_humdefbur1, 3)),
                fd_humdefbur2_3ma = as.numeric(movAvg(fd_humdefbur2, 3)),
                fd_milex1_3ma = as.numeric(movAvg(fd_milex1, 3)),
                fd_milex2_3ma = as.numeric(movAvg(fd_milex2, 3)),
                IntraUCDP_1_3ma = as.numeric(movAvg(IntraUCDP_1, 3)),
                IntraUCDP_2_3ma = as.numeric(movAvg(IntraUCDP_2, 3)),
                flow1_3ma = as.numeric(movAvg(flow1, 3)),
                flow2_3ma = as.numeric(movAvg(flow2, 3)),
                exports_1_3ma = as.numeric(movAvg(exports_1, 3)),
                exports_2_3ma = as.numeric(movAvg(exports_2, 3)),
                
                caprat_5mar = as.numeric(movAvgRightAligned(caprat, 5)),
                abs_cap_5mar = as.numeric(movAvgRightAligned(abs_cap, 5)),
                interac_humdefbur_5mar = as.numeric(movAvgRightAligned(interac_humdefbur, 5)),
                interac_fd_milex_5mar = as.numeric(movAvgRightAligned(interac_fd_milex, 5)),
                allies_w_5mar = as.numeric(movAvgRightAligned(allies_w, 5)),
                ally_dum_w_5mar = as.numeric(movAvgRightAligned(ally_dum_w, 5)),
                dyaddur_5mar = as.numeric(movAvgRightAligned(dyaddur, 5)),
                interac_flow_5mar = as.numeric(movAvgRightAligned(interac_flow, 5)),
                interac_export_5mar = as.numeric(movAvgRightAligned(interac_export, 5)),
                interac_IntraUCDP_5mar = as.numeric(movAvgRightAligned(interac_IntraUCDP, 5)),
                fd_humdefbur1_5mar = as.numeric(movAvgRightAligned(fd_humdefbur1, 5)),
                fd_humdefbur2_5mar = as.numeric(movAvgRightAligned(fd_humdefbur2, 5)),
                fd_milex1_5mar = as.numeric(movAvgRightAligned(fd_milex1, 5)),
                fd_milex2_5mar = as.numeric(movAvgRightAligned(fd_milex2, 5)),
                IntraUCDP_1_5mar = as.numeric(movAvgRightAligned(IntraUCDP_1, 5)),
                IntraUCDP_2_5mar = as.numeric(movAvgRightAligned(IntraUCDP_2, 5)),
                flow1_5mar = as.numeric(movAvgRightAligned(flow1, 5)),
                flow2_5mar = as.numeric(movAvgRightAligned(flow2, 5)),
                exports_1_5mar = as.numeric(movAvgRightAligned(exports_1, 5)),
                exports_2_5mar = as.numeric(movAvgRightAligned(exports_2, 5)),
                
                caprat_3mar = as.numeric(movAvgRightAligned(caprat, 3)),
                abs_cap_3mar = as.numeric(movAvgRightAligned(abs_cap, 3)),
                interac_humdefbur_3mar = as.numeric(movAvgRightAligned(interac_humdefbur, 3)),
                interac_fd_milex_3mar = as.numeric(movAvgRightAligned(interac_fd_milex, 3)),
                allies_w_3mar = as.numeric(movAvgRightAligned(allies_w, 3)),
                ally_dum_w_3mar = as.numeric(movAvgRightAligned(ally_dum_w, 3)),
                dyaddur_3mar = as.numeric(movAvgRightAligned(dyaddur, 3)),
                interac_flow_3mar = as.numeric(movAvgRightAligned(interac_flow, 3)),
                interac_export_3mar = as.numeric(movAvgRightAligned(interac_export, 3)),
                interac_IntraUCDP_3mar = as.numeric(movAvgRightAligned(interac_IntraUCDP, 3)),
                fd_humdefbur1_3mar = as.numeric(movAvgRightAligned(fd_humdefbur1, 3)),
                fd_humdefbur2_3mar = as.numeric(movAvgRightAligned(fd_humdefbur2, 3)),
                fd_milex1_3mar = as.numeric(movAvgRightAligned(fd_milex1, 3)),
                fd_milex2_3mar = as.numeric(movAvgRightAligned(fd_milex2, 3)),
                IntraUCDP_1_3mar = as.numeric(movAvgRightAligned(IntraUCDP_1, 3)),
                IntraUCDP_2_3mar = as.numeric(movAvgRightAligned(IntraUCDP_2, 3)),
                flow1_3mar = as.numeric(movAvgRightAligned(flow1, 3)),
                flow2_3mar = as.numeric(movAvgRightAligned(flow2, 3)),
                exports_1_3mar = as.numeric(movAvgRightAligned(exports_1, 3)),
                exports_2_3mar = as.numeric(movAvgRightAligned(exports_2, 3)),
                
                caprat_4mar = as.numeric(movAvgRightAligned(caprat, 4)),
                abs_cap_4mar = as.numeric(movAvgRightAligned(abs_cap, 4)),
                interac_humdefbur_4mar = as.numeric(movAvgRightAligned(interac_humdefbur, 4)),
                interac_fd_milex_4mar = as.numeric(movAvgRightAligned(interac_fd_milex, 4)),
                allies_w_4mar = as.numeric(movAvgRightAligned(allies_w, 4)),
                ally_dum_w_4mar = as.numeric(movAvgRightAligned(ally_dum_w, 4)),
                dyaddur_4mar = as.numeric(movAvgRightAligned(dyaddur, 4)),
                interac_flow_4mar = as.numeric(movAvgRightAligned(interac_flow, 4)),
                interac_export_4mar = as.numeric(movAvgRightAligned(interac_export, 4)),
                interac_IntraUCDP_4mar = as.numeric(movAvgRightAligned(interac_IntraUCDP, 4)),
                fd_humdefbur1_4mar = as.numeric(movAvgRightAligned(fd_humdefbur1, 4)),
                fd_humdefbur2_4mar = as.numeric(movAvgRightAligned(fd_humdefbur2, 4)),
                fd_milex1_4mar = as.numeric(movAvgRightAligned(fd_milex1, 4)),
                fd_milex2_4mar = as.numeric(movAvgRightAligned(fd_milex2, 4)),
                IntraUCDP_1_4mar = as.numeric(movAvgRightAligned(IntraUCDP_1, 4)),
                IntraUCDP_2_4mar = as.numeric(movAvgRightAligned(IntraUCDP_2, 4)),
                flow1_4mar = as.numeric(movAvgRightAligned(flow1, 4)),
                flow2_4mar = as.numeric(movAvgRightAligned(flow2, 4)),
                exports_1_4mar = as.numeric(movAvgRightAligned(exports_1, 4)),
                exports_2_4mar = as.numeric(movAvgRightAligned(exports_2, 4)),
                
                caprat_2mar = as.numeric(movAvgRightAligned(caprat, 2)),
                abs_cap_2mar = as.numeric(movAvgRightAligned(abs_cap, 2)),
                interac_humdefbur_2mar = as.numeric(movAvgRightAligned(interac_humdefbur, 2)),
                interac_fd_milex_2mar = as.numeric(movAvgRightAligned(interac_fd_milex, 2)),
                allies_w_2mar = as.numeric(movAvgRightAligned(allies_w, 2)),
                ally_dum_w_2mar = as.numeric(movAvgRightAligned(ally_dum_w, 2)),
                dyaddur_2mar = as.numeric(movAvgRightAligned(dyaddur, 2)),
                interac_flow_2mar = as.numeric(movAvgRightAligned(interac_flow, 2)),
                interac_export_2mar = as.numeric(movAvgRightAligned(interac_export, 2)),
                interac_IntraUCDP_2mar = as.numeric(movAvgRightAligned(interac_IntraUCDP, 2)),
                fd_humdefbur1_2mar = as.numeric(movAvgRightAligned(fd_humdefbur1, 2)),
                fd_humdefbur2_2mar = as.numeric(movAvgRightAligned(fd_humdefbur2, 2)),
                fd_milex1_2mar = as.numeric(movAvgRightAligned(fd_milex1, 2)),
                fd_milex2_2mar = as.numeric(movAvgRightAligned(fd_milex2, 2)),
                IntraUCDP_1_2mar = as.numeric(movAvgRightAligned(IntraUCDP_1, 2)),
                IntraUCDP_2_2mar = as.numeric(movAvgRightAligned(IntraUCDP_2, 2)),
                flow1_2mar = as.numeric(movAvgRightAligned(flow1, 2)),
                flow2_2mar = as.numeric(movAvgRightAligned(flow2, 2)),
                exports_1_2mar = as.numeric(movAvgRightAligned(exports_1, 2)),
                exports_2_2mar = as.numeric(movAvgRightAligned(exports_2, 2))
                )


findDistLastConflict <- function(years, conflict_lev) {
  this_df <- data.frame(years, conflict_lev)
  this_df$dist_last_conflict <- NA
  for (i in 1:length(years)) {
    this_df$dist_last_conflict[i] <- min(years[i] - years[years < years[i] & conflict_lev > 0])
  }
  return(this_df$dist_last_conflict)
}
                          
mid_and_icb_dyad_yr <-
  mid_and_icb_dyad_yr %>%
  dplyr::group_by(ccode1, ccode2) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(midpcyrs = findDistLastConflict(year, HostLevelFatality))

## Distance to last conflict is arbitrarily set to 100 if no conflict occurs in the dataset
mid_and_icb_dyad_yr$midpcyrs[is.infinite(mid_and_icb_dyad_yr$midpcyrs)] <- 100

mid_and_icb_dyad_yr <-
  mid_and_icb_dyad_yr %>%
  dplyr::group_by(ccode1, ccode2) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(midpcyrs_5ma = as.numeric(movAvg(midpcyrs, 5)),
                midpcyrs2_5ma = as.numeric(movAvg(midpcyrs^(1/2), 5)),
                midpcyrs3_5ma = as.numeric(movAvg(midpcyrs^(1/3), 5)),
                midpcyrs_3ma = as.numeric(movAvg(midpcyrs, 3)),
                midpcyrs2_3ma = as.numeric(movAvg(midpcyrs^(1/2), 3)),
                midpcyrs3_3ma = as.numeric(movAvg(midpcyrs^(1/3), 3)),
                
                midpcyrs_5mar = as.numeric(movAvgRightAligned(midpcyrs, 5)),
                midpcyrs2_5mar = as.numeric(movAvgRightAligned(midpcyrs^(1/2), 5)),
                midpcyrs3_5mar = as.numeric(movAvgRightAligned(midpcyrs^(1/3), 5)),
                midpcyrs_3mar = as.numeric(movAvgRightAligned(midpcyrs, 3)),
                midpcyrs2_3mar = as.numeric(movAvgRightAligned(midpcyrs^(1/2), 3)),
                midpcyrs3_3mar = as.numeric(movAvgRightAligned(midpcyrs^(1/3), 3)),
                
                midpcyrs_4mar = as.numeric(movAvgRightAligned(midpcyrs, 4)),
                midpcyrs2_4mar = as.numeric(movAvgRightAligned(midpcyrs^(1/2), 4)),
                midpcyrs3_4mar = as.numeric(movAvgRightAligned(midpcyrs^(1/3), 4)),
                midpcyrs_2mar = as.numeric(movAvgRightAligned(midpcyrs, 2)),
                midpcyrs2_2mar = as.numeric(movAvgRightAligned(midpcyrs^(1/2), 2)),
                midpcyrs3_2mar = as.numeric(movAvgRightAligned(midpcyrs^(1/3), 2))
                )


# Merge
require(data.table)
dv_df <- data.table(dv_df)
mid_and_icb_dyad_yr <- data.table(mid_and_icb_dyad_yr)

setkeyv(dv_df, c('ccode1','ccode2','year'))
setkeyv(mid_and_icb_dyad_yr, c('ccode1','ccode2','year'))

names(dv_df)[names(dv_df) == "abbrev1"] <- "abbrev1.y"
names(dv_df)[names(dv_df) == "abbrev2"] <- "abbrev2.y"

mid_and_icb_dyad_yr_plus_dv <- merge(mid_and_icb_dyad_yr, dv_df[,sevvio:=NULL])

mid_and_icb_dyad_yr_plus_dv <- 
  mid_and_icb_dyad_yr_plus_dv %>%
  dplyr::group_by(BGregion, year) %>%
  dplyr::mutate(MID_level_region = mean(HostLevelFatality, na.rm = T),
                MID_level_region.5point = mean(HostLevelFatality.5point, na.rm = T)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(ccode1, ccode2) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(MID_level_region_5ma = movAvg(MID_level_region, 5),
                MID_level_region_3ma = movAvg(MID_level_region, 3),
                MID_level_region_5mar = movAvgRightAligned(MID_level_region, 5),
                MID_level_region_4mar = movAvgRightAligned(MID_level_region, 4),
                MID_level_region_3mar = movAvgRightAligned(MID_level_region, 3),
                MID_level_region_2mar = movAvgRightAligned(MID_level_region, 2),
                
                MID_level_region_5ma.5point = movAvg(MID_level_region.5point, 5),
                MID_level_region_3ma.5point = movAvg(MID_level_region.5point, 3),
                MID_level_region_5mar.5point = movAvgRightAligned(MID_level_region.5point, 5),
                MID_level_region_4mar.5point = movAvgRightAligned(MID_level_region.5point, 4),
                MID_level_region_3mar.5point = movAvgRightAligned(MID_level_region.5point, 3),
                MID_level_region_2mar.5point = movAvgRightAligned(MID_level_region.5point, 2)
  )

# Add variable for nuclear weapon state in dyad (based on Rauchhaus 2009)

mid_and_icb_dyad_yr_plus_dv$nuc_state1 <- 0
mid_and_icb_dyad_yr_plus_dv$nuc_state2 <- 0

nuclear_states <- 
  list("USA" = 1945:2013, "USR|RUS" = 1949:2013, "UKG" = 1951:2013, 
       "FRN" = 1960:2013, "CHN" = 1964:2013, "ISR" = 1967:2013, 
       "SAF" = 1982:1990, "IND" = 1968:2013, "PAK" = 1990:2013,
       "PRK" = 2006:2013)

for (i in 1:length(nuclear_states)) {
  rows1 <- 
    with(mid_and_icb_dyad_yr_plus_dv,
         abbrev1 %in% names(nuclear_states)[i] &
           year %in% nuclear_states[[i]]
    )
  rows2 <-
    with(mid_and_icb_dyad_yr_plus_dv,
         abbrev2 %in% names(nuclear_states)[i] &
           year %in% nuclear_states[[i]]
    )
  mid_and_icb_dyad_yr_plus_dv$nuc_state1[rows1] <- 1 
  mid_and_icb_dyad_yr_plus_dv$nuc_state2[rows2] <- 1 
}

mid_and_icb_dyad_yr_plus_dv$nuc_states <- with(mid_and_icb_dyad_yr_plus_dv, nuc_state1 + nuc_state2)


# Save
vars_to_save <- 
  c(colnames(mid_and_icb_dyad_yr), 
    
    "exports_1", "exports_2", "flow1", "flow2", "IntraUCDP_1", "IntraUCDP_2", 
    "fd_humdefbur1", "fd_humdefbur2", "fd_milex1", "fd_milex2", 
    "caprat", "abs_cap", "ally_dum_w", "allies_w", "interac_humdefbur", 
    "interac_fd_milex", "dyaddur", "interac_flow", "interac_export",
    "midpcyrs", "MID_level_region", "interac_IntraUCDP",
    
    "exports_1_5ma", "exports_2_5ma", "flow1_5ma", "flow2_5ma", "IntraUCDP_1_5ma", "IntraUCDP_2_5ma", 
    "fd_humdefbur1_5ma", "fd_humdefbur2_5ma", "fd_milex1_5ma", "fd_milex2_5ma", 
    "caprat_5ma", "abs_cap_5ma", "ally_dum_w_5ma", "allies_w_5ma", "interac_humdefbur_5ma", 
    "interac_fd_milex_5ma", "dyaddur_5ma", "interac_flow_5ma", "interac_export_5ma",
    "midpcyrs_5ma", "midpcyrs2_5ma", "midpcyrs3_5ma", "MID_level_region_5ma", "interac_IntraUCDP_5ma",
    
    "exports_1_3ma", "exports_2_3ma", "flow1_3ma", "flow2_3ma", "IntraUCDP_1_3ma", "IntraUCDP_2_3ma", 
    "fd_humdefbur1_3ma", "fd_humdefbur2_3ma", "fd_milex1_3ma", "fd_milex2_3ma", 
    "caprat_3ma", "abs_cap_3ma", "ally_dum_w_3ma", "allies_w_3ma", "interac_humdefbur_3ma", 
    "interac_fd_milex_3ma", "dyaddur_3ma", "interac_flow_3ma", "interac_export_3ma",
    "midpcyrs_3ma", "midpcyrs2_3ma", "midpcyrs3_3ma", "MID_level_region_3ma", "interac_IntraUCDP_3ma",
    
    "exports_1_5mar", "exports_2_5mar", "flow1_5mar", "flow2_5mar", "IntraUCDP_1_5mar", "IntraUCDP_2_5mar", 
    "fd_humdefbur1_5mar", "fd_humdefbur2_5mar", "fd_milex1_5mar", "fd_milex2_5mar", 
    "caprat_5mar", "abs_cap_5mar", "ally_dum_w_5mar", "allies_w_5mar", "interac_humdefbur_5mar", 
    "interac_fd_milex_5mar", "dyaddur_5mar", "interac_flow_5mar", "interac_export_5mar",
    "midpcyrs_5mar", "midpcyrs2_5mar", "midpcyrs3_5mar", "MID_level_region_5mar", "MID_level_region_5mar.5point", 
    "interac_IntraUCDP_5mar",
    
    "exports_1_4mar", "exports_2_4mar", "flow1_4mar", "flow2_4mar", "IntraUCDP_1_4mar", "IntraUCDP_2_4mar", 
    "fd_humdefbur1_4mar", "fd_humdefbur2_4mar", "fd_milex1_4mar", "fd_milex2_4mar", 
    "caprat_4mar", "abs_cap_4mar", "ally_dum_w_4mar", "allies_w_4mar", "interac_humdefbur_4mar", 
    "interac_fd_milex_4mar", "dyaddur_4mar", "interac_flow_4mar", "interac_export_4mar",
    "midpcyrs_4mar", "midpcyrs2_4mar", "midpcyrs3_4mar", "MID_level_region_4mar", "MID_level_region_4mar.5point",
    "interac_IntraUCDP_4mar",
    
    "exports_1_3mar", "exports_2_3mar", "flow1_3mar", "flow2_3mar", "IntraUCDP_1_3mar", "IntraUCDP_2_3mar", 
    "fd_humdefbur1_3mar", "fd_humdefbur2_3mar", "fd_milex1_3mar", "fd_milex2_3mar", 
    "caprat_3mar", "abs_cap_3mar", "ally_dum_w_3mar", "allies_w_3mar", "interac_humdefbur_3mar", 
    "interac_fd_milex_3mar", "dyaddur_3mar", "interac_flow_3mar", "interac_export_3mar",
    "midpcyrs_3mar", "midpcyrs2_3mar", "midpcyrs3_3mar", "MID_level_region_3mar", "MID_level_region_3mar.5point",
    "interac_IntraUCDP_3mar",
    
    "exports_1_2mar", "exports_2_2mar", "flow1_2mar", "flow2_2mar", "IntraUCDP_1_2mar", "IntraUCDP_2_2mar", 
    "fd_humdefbur1_2mar", "fd_humdefbur2_2mar", "fd_milex1_2mar", "fd_milex2_2mar", 
    "caprat_2mar", "abs_cap_2mar", "ally_dum_w_2mar", "allies_w_2mar", "interac_humdefbur_2mar", 
    "interac_fd_milex_2mar", "dyaddur_2mar", "interac_flow_2mar", "interac_export_2mar",
    "midpcyrs_2mar", "midpcyrs2_2mar", "midpcyrs3_2mar", "MID_level_region_2mar", "MID_level_region_2mar.5point",
    "interac_IntraUCDP_2mar",
    
    "contig", "contig24", "lndistance", 
    "jntdemord", "poldisab", "jntautoc", "nuc_states")

mid_and_icb_dyad_yr_plus_dv <- mid_and_icb_dyad_yr_plus_dv[,vars_to_save]

save(mid_and_icb_dyad_yr_plus_dv, file = "mid_and_icb_dyad_yr_plus_dv.RData")
